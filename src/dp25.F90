      SUBROUTINE DPREAL(IRD2,ICOL1,ICOL2,MINCO2,MAXCO2,X,N,IFLGSV,        &
                        IXC,NXC,                                          &
                        ICASRE,IFUNC2,N2,MAXN2,                           &
                        IMACRO,IMACNU,IMACCS,                             &
                        IANSLC,IWIDTH,IREACS,ISTOR1,ISTOR2,IEND,NUMLRD,   &
                        IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,     &
                        ICOMCH,ICOMSW,LINETY,IGRPAU,                      &
                        ICOLL,ICOLU,ITYPE,NCOLS,NCALL,                    &
                        IREADL,IDATDL,ITIMDL,IRDIPA,PREAMV,               &
                        MAXRDV,MAXCHV,IFIETY,                             &
                        IDECPT,IDATMV,IDATNN,                             &
                        IREACD,IREACM,IREADS,IREAPM,IREAMC,ITABNC,IREALT, &
                        XTAG,IOUNI5,                                      &
                        IREAAS,IREAPC,                                    &
                        IB,                                               &
                        IOTERM,IANSLO,MAXLI2,MAXCI2,ILOOST,ILOOLI,        &
                        IREPCH,IMALEV,IREANQ,                             &
                        IERRFI,IBUGS2,ISUBRO,IERROR)
!
!     PURPOSE--THIS SUBROUTINE PERFORMS A FORMAT-FREE READ
!              OF ONE LINE OF DATA FROM INPUT UNIT = IRD2.
!              ONLY THE CARD COLUMNS BETWEEN ICOL1 AND ICOL2
!              (INCLUSIVELY) ARE SCANNED FOR THE READ.
!              THIS SUBROUTINE GIVES THE DATA ANALYST THE ABILITY
!              TO GET DATA INTO THE MACHINE FROM A VARIETY OF INPUT
!              SOURCES (CARD, TAPE, DISC, ETC.) WITHOUT HAVING
!              TO WORRY ABOUT AND SPECIFY FORMATS.  THE DATA CARD
!              IMAGES MAY BE MADE WITHOUT REGARD TO ANY PARTICULAR
!              FORMAT AND MAY BE ENTERED INTO THE MACHINE
!              WITHOUT DEFINING ANY FORMATS.
!     INPUT  ARGUMENTS--IRD2   = THE INTEGER VALUE SPECIFYING
!                                THE INPUT UNIT FROM WHICH
!                                THE CARD IMAGES WILL COME.
!                     --ICOL1  = THE INTEGER CARD COLUMN NUMBER
!                                WHICH DEFINES THE LOWER BOUND
!                                (INCLUSIVELY) OF THE INTERVAL
!                                ON EACH CARD IMAGE TO BE SCANNED
!                                FOR THE READ.
!                     --ICOL2  = THE INTEGER CARD COLUMN NUMBER
!                                WHICH DEFINES THE UPPER BOUND
!                                (INCLUSIVELY) OF THE INTERVAL
!                                ON EACH CARD IMAGE TO BE SCANNED
!                                FOR THE READ.
!     OUTPUT ARGUMENTS--X      = THE SINGLE PRECISION VECTOR
!                                INTO WHICH THE READ DATA VALUES
!                                WILL BE SEQUENTIALLY PLACED.
!                     --N      = THE INTEGER VALUE
!                                WHICH WILL EQUAL THE NUMBER OF DATA
!                                VALUES WHICH WERE READ.
!     OUTPUT--THE SINGLE PRECISION VECTOR X WHICH
!             WILL CONTAIN THE READ DATA VALUES, AND
!             THE INTEGER VALUE N WHICH WILL EQUAL THE NUMBER OF
!             DATA VALUES READ INTO X.  ALSO, 7 LINES OF SUMMARY
!             INFORMATION WILL BE GENERATED--
!             REGARDING WHAT WAS IN FACT READ INTO THE MACHINE--
!             1) THE VALUES OF ICOL1 AND ICOL2;
!             2) THE (ENTIRE) FIRST DATA CARD READ;
!             3) THE (ENTIRE) LAST DATA CARD READ;
!             4) THE TOTAL NUMBER OF DATA CARDS READ;
!             5) THE TOTAL NUMBER OF DATA VALUES READ.
!     PRINTING--YES.
!     RESTRICTIONS--ICOL1 AND ICOL2 MUST BE BETWEEN 1 AND 132,
!                   INCLUSIVELY.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     COMMENT--ADJACENT DATA VALUES ON THE SAME CARD MUST BE
!              SEPARATED BY AT LEAST 1 BLANK OR 1 ALPHABETIC
!              CHARACTER, OR BY  ANY COMBINATION OF BLANKS AND
!              ALPHABETIC CHARACTERS.  IN THIS CONTEXT, AN
!              ALPHABETIC CHARACTER IS ANY CHARACTER
!              OTHER THAN 0, 1, 2, ..., 9, +, -, OR ..
!              IN EFFECT, THEREFORE, ALL ALPHABETIC INFORMATION
!              IN THE INTERVAL DEFINED BY ICOL1 AND ICOL2
!              (INCLUSIVELY) IS IGNORED FOR READING PURPOSES.
!              ALL INFORMATION (BOTH NUMERIC AND ALPHABETIC)
!              OUTSIDE THE DEFINED INTERVAL IS ALSO IGNORED
!              FOR READING PURPOSES.
!     COMMENT--THE DATA VALUES ON THE CARDS ARE FREE-FORMAT.
!              THEY MAY BE EITHER INTEGER OR FLOATING POINT
!              (THAT IS, WITHOUT OR WITH THE DECIMAL POINTS).
!              EXPONENTIAL FLOATING POINT FORMAT (E FORMAT)
!              IS NOT PERMITTED.  ALL DATA, WHETHER WITHOUT OR WITH
!              THE DECIMAL POINT ON THE CARDS, WILL BE READ INTO
!              THE MACHINE INTO THE X VECTOR AND WILL RESIDE THERE
!              AS FLOATING POINT NUMBERS.
!     COMMENT--ANY PARTICULAR DATA VALUE MUST START AND END
!              ON THE SAME DATA CARD; DATA VALUES MAY NOT
!              START ON ONE CARD AND FINISH ON THE NEXT.
!              VARIOUS ILLEGAL COMBINATIONS (SUCH AS
!              MULTIPLE DECIMAL POINTS, MULTIPLE PLUSSES OR
!              MINUSES, INCOMPLETE VALUES CONSISTING ONLY
!              OF A DECIMAL POINT, OR ONLY OF A SIGN AND A DECIMAL
!              POINT, ETC. ARE NOT ACCEPTED AND THE
!              DATA ANALYST WILL BE INFORMED OF THE EXISTENCE OF
!              SUCH BY AN ERROR DIAGNOSTIC.
!              IN THE EVENT OF SUCH AN ILLEGAL COMBINATION,
!              THAT 'NUMBER' AND ALL REMAINING NUMBERS ON THAT CARD WILL
!              WILL BE IGNORED (NOT READ INTO THE MACHINE)
!              AND THE NEXT DATA CARD WILL THEN BE READ.
!     COMMENT--THIS SUBROUTINE WILL CONTINUOUSLY AND
!              SEQUENTIALLY READ CARDS UNTIL A CARD WITH
!              THE WORD         END       (SOMEWHERE BETWEEN
!              COLUMNS ICOL1 AND ICOL2 (INCLUSIVELY) IS ENCOUNTERED.
!              TO TERMINATE A DATA SET, THE ANALYST SHOULD
!              APPEND SUCH A CARD WHICH HAS THE WORD
!              END        SOMEWHERE IN THE INTERVAL
!              DEFINED BY ICOL1 AND ICOL2.  FOR EXAMPLE, IF
!              ICOL1 = 1 AND ICOL2 = 20, THEN A SEPARATE CARD WITH
!                 END   IN COLUMNS 1, 2, AND 3, OR IN COLUMNS 10, 11,
!              AND 12, ETC.  WOULD TERMINATE THE READ.
!              IT IS IMPORTANT TO APPEND SUCH A CARD--
!              FAILURE TO DO SO WILL RESULT IN AN INCOMPLETE
!              DATA SET OR (ON SOME COMPUTERS) AN
!              UNPREDICTABLE RUN TERMINATION.
!     REFERENCES--NONE.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     VERSION NUMBER--86/1
!     ORIGINAL VERSION--DECEMBER  1972.
!     UPDATED         --AUGUST    1974.
!     UPDATED         --NOVEMBER  1975.
!     UPDATED         --OCTOBER   1976.
!     UPDATED         --JANUARY   1977.
!     UPDATED         --MARCH     1977.
!     UPDATED         --SEPTEMBER 1981.
!     UPDATED         --OCTOBER   1981.
!     UPDATED         --DECEMBER  1981.
!     UPDATED         --MARCH     1982.
!     UPDATED         --MAY       1982.
!     UPDATED         --DECEMBER  1985.
!     UPDATED         --MARCH     1986.
!     UPDATED         --NOVEMBER  1989. READ STRINGS FROM CERTAIN COLUMNS
!     UPDATED         --MAY       1990.  1) IGNORE BLANK LINES
!                                        2) CHECK FOR "D" EXPONENTIAL NOTATION
!                                        3) CHECK FOR COMMENT CHARACTER COL 1
!     UPDATED         --JULY      1990. ICOMFL RENAMED AS ICOMSW
!     UPDATED         --FEBRUARY  1994. WRITE STATEMENTS: 132->80
!     UPDATED         --SEPTEMBER 1995. REPORT BLANK LINE VIA LINETY
!     UPDATED         --JANUARY   1998. CHECK LINE FOR NON-PRINTING
!                                       CHARACTERS (CONVERT TO SPACE)
!     UPDATED         --DECEMBER  1999. ROW LABEL CASE (ROWI)
!     UPDATED         --FEBRUARY  2003. INCREASE MAXIMUM RECORD LENGTH
!                                       THAT CAN BE READ
!     UPDATED         --JANUARY   2004. RECODE FOR BETTER CLARITY
!     UPDATED         --JANUARY   2004. SUPPORT FOR CHARACTER DATA
!     UPDATED         --APRIL     2005. HANDLE BLANK FIELDS FOR
!                                       VECTOR COLUMN LIMITS CASE
!     UPDATED         --APRIL     2005. SUPPORT "," AS DECIMAL POINT
!                                       (FOR INTERNATIONAL)
!     UPDATED         --SEPTEMBER 2006. IF USING COLUMN LIMITS TO READ
!                                       CHARACTER DATA, MAKE SURE FIRST
!                                       OCCURRENCE EXTRACTS FULL STRING
!                                       (I.E., NEED TO ACCOUNT FOR BLANKS)
!     UPDATED         --APRIL     2009. CHECK FOR NaN (NOT A NUMBER)
!                                       IN NUMERIC FIELDS
!     UPDATED         --APRIL     2009. CHECK FOR A "MISSING VALUE"
!                                       CHARACTER STRING IN NUMERIC
!                                       FIELDS (DEFAULT IS "MV", BUT
!                                       IS USER-SETTABLE)
!     UPDATED         --JANUARY   2010. TREAT ASCII 127 AS A
!                                       NON-PRINTING CHARACTER
!     UPDATED         --APRIL     2010. INITIALIZE VECTOR WITH
!                                       DATA VALUES (X) TO MISSING
!                                       VALUE.  THIS WAY, IF A "SHORT"
!                                       LINE IS ENCOUNTERED, MISSING
!                                       VALUES AT END WILL BE SET TO
!                                       MISSING VALUE RATHER THAN ZERO.
!     UPDATED         --APRIL     2010. IF HAVE "MV,", THEN MOVE
!                                       POINTER TO "," POSITION TO
!                                       AVOID EXTRA MISSING VALUE
!     UPDATED         --SEPTEMBER 2012. ADD MISSING VALUE IF RECORD
!                                       ENDS WITH DELIMITER
!     UPDATED         --SEPTEMBER 2012. ALLOW USER TO SPECIFY CERTAIN
!                                       COLUMNS AS CHARACTER
!     UPDATED         --JANUARY   2015. IF DOING A TERMINAL READ WHEN A
!                                       LOOP IS ACTIVE, THEN NEED TO
!                                       EXTRACT LINE FROM SAVED LOOP
!                                       LINES RATHER THAN STANDARD INPUT
!     UPDATED         --JUNE      2016. ADD IDATDL AND ITIMDL TO SUPPORT
!                                       DELIMITERS FOR DATE/TIME FIELDS
!     UPDATED         --JUNE      2016. ADD IRDIPA TO READ IP ADDRESSES
!     UPDATED         --JUNE      2018. CORRECTED ISSUES:
!                                        1) 3Dxxxx SHOULD BE RECOGNIZED
!                                           AS CHARACTER FIELD RATHER
!                                           THAN EXPONENTIAL NUMBER
!                                        2) HANDLE "-3.27+105" CORRECTLY
!                                           (ISSUE OCCURS WHEN THERE WAS
!                                           A SIGN BOTH AT THE BEGINNING
!                                           AND AT THE START OF THE
!                                           EXPONENT)
!     UPDATED         --JUNE      2018. IGRPAU = CATEGORICAL
!     UPDATED         --JULY      2018. SOME UPDATES TO SUPPORT READING
!                                       CHARACTER DATA BASED ON THE
!                                       DELIMITER
!     UPDATED         --AUGUST    2018. ISSUE WHEN SPACE IS DELIMITER
!     UPDATED         --APRIL     2019. BUG WITH VECTOR COLUMN LIMITS
!     UPDATED         --APRIL     2019. SET READ ASTERISK IGNORE
!     UPDATED         --JUNE      2019. MAXIMUM NUMBER OF CHARACTER
!                                       VARIABLES RAISED TO 50
!     UPDATED         --JUNE      2019. BETTER CHECKING FOR LIMITS
!                                       WHEN READING CHARACTER DATA
!     UPDATED         --SEPTEMBER 2019. TO ALLOW READING CHARACTER
!                                       VARIABLES FROM THE TERMINAL,
!                                       USE "IFLGSV" TO PARSE A SAVED
!                                       LINE (TERMINAL READ WILL USE A
!                                       SAVED LINE FOR THE FIRST LINE)
!     UPDATED         --OCTOBER   2019. ADD "IREAPR" TO CHECK FOR
!                                       PERCENT SIGN IN NUMERIC
!                                       VARIABLES
!     UPDATED         --APRIL     2023. CHECK IF "EOF" IS EMBEDDED IN
!                                       LARGER STRING
!     UPDATED         --FEBRUARY  2025. CALL LIST TO DPREAL
!                                       1. ADD IREALT
!                                       2. ADD XTAG, IOUNI5
!                                          (OPEN dpst5f.dat IF NEEDED)
!                                       CHECK FOR LEADING "<" OR ">" AND
!                                       WRITE TAG VARIABLES TO dpst5f.dat
!                                       INDICATING WHETHER DATA FIELD IS
!                                       EQUAL TO VALUE (0), LESS THAN THE
!                                       VALUE (1) OR GREATER THAN THE
!                                       VALUE (2).
!     UPDATED         --JANUARY   2026. CALL LIST TO DPREAL
!                                       1. ADD IREANQ - OPTION TO REMOVE
!                                          QUOTES FROM LINE
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASRE
      CHARACTER*4 IFUNC2
!
      CHARACTER*4 IOTERM
      CHARACTER*4 ILOOST
      CHARACTER*1 IREPCH
      CHARACTER*4 IREANQ
      CHARACTER*4 IANSLO(MAXLI2,MAXCI2)
!
      CHARACTER*4 IMACRO
      CHARACTER*12 IMACCS
      CHARACTER*4 IANSLC
      CHARACTER*12 IREACS
      CHARACTER*4 ISTOR1
      CHARACTER*4 ISTOR2
      CHARACTER*4 IEND
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
!
      CHARACTER*4 ISUBN0
      CHARACTER*4 IERRFI
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      CHARACTER*4 IOFILE
!
      CHARACTER*4 IDATMV
      CHARACTER*4 IDATNN
      CHARACTER*4 IREACD
      CHARACTER*4 IREACM
      CHARACTER*4 IREADS
      CHARACTER*4 IREAPC
      CHARACTER*4 IREAPM
      CHARACTER*4 IREALT
      CHARACTER*4 IREAAS
      CHARACTER*8 IREAMC
      CHARACTER*4 IB
      CHARACTER*4 ICHAR3
!CCCC CHARACTER*4 IC
      CHARACTER*4 ICHEXP
!
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
      CHARACTER*20 IFORMT
!
!---------------------------------------------------------------------
!
      CHARACTER*24 IXC(*)
      DIMENSION X(*)
      DIMENSION XTAG(*)
!
      INTEGER ICOLL(*)
      INTEGER ICOLU(*)
      INTEGER ITYPE(*)
      INTEGER IFIETY(*)
!
      DIMENSION IFUNC2(*)
!
      DIMENSION ISTOR1(*)
      DIMENSION ISTOR2(*)
      DIMENSION IANSLC(*)
      DIMENSION IB(*)
      DIMENSION ICHAR3(41)
      DIMENSION ICHEXP(41)
!CCCC DIMENSION IC(10)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOFO.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCONP.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
!CCCC DATA IC(1),IC(2),IC(3),IC(4),IC(5),IC(6),IC(7),IC(8),IC(9),IC(10)
!CCCC1/'0','1','2','3','4','5','6','7','8','9'/
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPRE'
      ISUBN2='AL  '
      ISUBN0='REAL'
      ICOL2S=ICOL2
      IDATD2=IDATDL
      ITIMD2=ITIMDL
      IREAD3=IREADL
      TAGZZ=0.0
      IF(IREADL.EQ.',' .AND. IREACM.EQ.'ON')IREADL='    '
!
!     THE FOLLOWING NULL-CORRECTION WAS MADE IN APRIL OF 1987 (ELGIN PERRY
!     AND DICK ATLEE FROM THE UNIV. OF MARYLAND; UNIVAC COMPILER MESSAGE)
!
      IOFILE='-999'
!
      IEND='NO'
      IERROR='NO'
      DO 2 I=1,MAXRDV
        X(I)=PREAMV
    2 CONTINUE
      DO 4 I=1,100
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
      DO 6 I=4,1,-1
        IF(IDATMV(I:I).NE.' ')THEN
          NCDAMV=I
          GO TO 7
        ENDIF
    6 CONTINUE
    7 CONTINUE
      NCDNAN=0
      DO 8 I=4,1,-1
        IF(IDATNN(I:I).NE.' ')THEN
          NCDNAN=I
          GO TO 9
        ENDIF
    8 CONTINUE
    9 CONTINUE
!
      IFLAGD=0
      IFLAGQ=0
      IFLGQ2=0
!
!               **************************************
!               **  CHECK FOR VECTOR COLUMN LIMITS  **
!               **************************************
!
      IF(NCALL.EQ.0)THEN
        DO 11 I=1,MAXRDV
          ITYPE(I)=-1
   11   CONTINUE
        NCOLS=0
        DO 20 I=1,50
          IF(ICOLL(I).GT.0 .AND. ICOLU(I).GT.0)THEN
            NCOLS=NCOLS+1
          ELSE
            GO TO 29
          ENDIF
   20   CONTINUE
   29   CONTINUE
      ENDIF
      NREAD=0
!
!               *************************
!               **  READ A LINE IMAGE  **
!               *************************
!
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'REAL')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPREAL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IRD2,ICOL1,ICOL2,IWIDTH,IREALT,IBUGS2
   52   FORMAT('IRD2,ICOL1,ICOL2,IWIDTH,IREALT,IBUGS2 = ',4I8,2X,2(A4,2X))
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
   63   FORMAT('IREAMC,IREADS,IREAPM,IREACD,IREACM,IREAAS,IREAPC = ',   &
               A8,6(2X,A4))
        CALL DPWRST('XXX','BUG ')
        DO 65 I=1,5
          WRITE(ICOUT,67)I,ICOLL(I),ICOLU(I)
   67     FORMAT('I,ICOLL(I),ICOLU(I) = ',3I7)
          CALL DPWRST('XXX','BUG ')
   65   CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      ISTEPN='1'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(MINCO2.LE.ICOL1.AND.ICOL1.LE.MAXCO2.AND.   &
         MINCO2.LE.ICOL2.AND.ICOL2.LE.MAXCO2)GO TO  89
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
        GO TO 9000
   89 CONTINUE
!
      NUMCRD=0
!
!               *********************************************
!               **  STEP 2--                               **
!               **  IF THE READ IS DONE FROM A FILE,       **
!               **  THEN CARRY OUT THE FILE READ OPERATION.**
!               *********************************************
!
      ISTEPN='2'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,91)IOFILE,IRD,IRD2,IOUNIT,IMACCS
   91   FORMAT('IOFILE,IRD,IRD2,IOUNIT,IMACCS = ',A4,3I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(IOFILE.EQ.'NO')THEN
!
        IF(IFLGSV.EQ.0)THEN
          IF(IRD2.EQ.IRD)THEN
            READ(IRD2,93,END=8000)(IB(IZ),IZ=1,ICOL2)
   93       FORMAT(255A1)
          ELSEIF(IMACCS.EQ.'OPEN')THEN
            NUMCHA=MAXCO2
            CALL DPREFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
                        IB,NUMCHA,                                      &
                        ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
          ENDIF
        ELSE
          NUMCHA=1
          DO 2993 KK=MAXCO2,1,-1
            IF(IB(KK).NE.' ')THEN
              NUMCHA=KK
              GO TO 2995
            ENDIF
 2993     CONTINUE
 2995     CONTINUE
          IFLGSV=0
        ENDIF
!
      ELSE
        IF(IOTERM.EQ.'LOOP')THEN
          ILOOLI=ILOOLI+1
          ILAST=1
          IFLAG=0
          DO 92 IZ=MAXCI2,1,-1
            IB(IZ)=IANSLO(ILOOLI,IZ)
            IF(IB(IZ).NE.' ' .AND. IFLAG.EQ.0)THEN
              ILAST=IZ
              IFLAG=1
            ENDIF
   92     CONTINUE
          NUMCHA=ILAST
!
!         CALL DPREP3 TO SEARCH FOR REPLACEMENT CHARACTER.
!         (LINES EXTRACTED FROM SAVED LOOP COMMANDS WILL NOT
!         HAVE THE REPLACEMENT CHARACTER CHECK PERFORMED YET).
!
          CALL DPREP3(IB,NUMCHA,IREPCH,IMALEV,   &
                      IBUGS2,IERROR)
          IF(NUMCHA.LT.MAXCI2)THEN
            DO 9993 IZ=NUMCHA+1,MAXCI2
              IB(IZ)=' '
 9993       CONTINUE
          ENDIF
        ELSE
          NUMCHA=MAXCO2
          IF(IFLGSV.EQ.0)THEN
            CALL DPREFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
                        IB,NUMCHA,                                      &
                        ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
          ELSE
            NUMCHA=1
            DO 2998 KK=ICOL2,1,-1
              IF(IB(KK).NE.' ')THEN
                NUMCHA=KK
                GO TO 2999
              ENDIF
 2998       CONTINUE
 2999       CONTINUE
            IFLGSV=0
          ENDIF
        ENDIF
      ENDIF
!
!CCCC CHECK FOR COMMENT LINE IN DATA FILE
!
      IF(ICOMSW.EQ.'ON  '.AND.IB(1).EQ.ICOMCH(1:1))THEN
         LINETY='BLAN'
         GO TO 9000
      ENDIF
!
!     IF REQUESTED, STRIP OUT QUOTE CHARACTERS FROM LINE.
!     FOR EXAMPLE, POWERSHELL SCRIPT TO READ EXCEL FILES
!     ENCLOSES ALL FIELDS, EVEN NUMERIC FIELDS, IN QUOTES.
!
      IF(IREANQ.EQ.'ON')THEN
        ICNT=0
        DO II=1,NUMCHA
           IF(IB(II)(1:1).NE.'"')THEN
             ICNT=ICNT+1
             IB(ICNT)=IB(II)
           ENDIF
        ENDDO
        NUMCHA=ICNT
      ENDIF
!
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'REAL')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,96)
   96   FORMAT('***** FROM THE MIDDLE OF DPREAL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,98)NUMCHA
   98   FORMAT('NUMCHA = ',I8)
        CALL DPWRST('XXX','BUG ')
        NMAX=NUMCHA
        IF(NMAX.GT.255)NMAX=255
        WRITE(ICOUT,97)(IB(J)(1:1),J=1,NMAX)
   97   FORMAT('IB(.) = ',255A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!CCCC REMOVE NON-PRINTING CHARACTERS.  CHECK FOR BLANK LINE
!CCCC AS WELL
!CCCC
!CCCC NOTE 1/2010: TREAT "127" AS PRINTING CHARACTER AS WELL
!CCCC              (THIS IS A "DELETE").
!CCCC
!CCCC              ALSO, CONVERT SPACE, "TAB" OR "READ DELIMITER" TO
!CCCC              SPACE.  OTHERWISE, SIMPLY REMOVE.
!CCCC
!CCCC              2019/02: WHEN READING STRINGS, YOU MAY WANT TO LEAVE
!CCCC                       TAB CHARACTERS INTACT.
!CCCC
!CCCC NOTE 9/2012: DELIMITER CHARACTER SHOULD REMAIN IN LINE
!CCCC              SINCE CONSECUTIVE DELIMITER CHARACTERS IMPLY
!CCCC              A MISSING VALUE SHOULD BE INSERTED.
!CCCC
!CCCC              IF VECTOR COLUMN LIMITS GIVEN, DO NOT CONVERT
!CCCC              MULTIPLE SPACES TO A SINGLE SPACE.  HOWEVER,
!CCCC              IF VECTOR COLUMN LIMITS NOT GIVEN, THEN CHECK
!CCCC              LOWER AND UPPER COLUMN LIMITS.  IF LOWER LIMIT
!CCCC              IS NOT 1, THEN DO NOT COMPRESS MULTIPLE SPACES.
!CCCC              HOWEVER, IF LOWER LIMIT IS 1, THEN ONLY PROCESS
!CCCC              UP TO THE UPPER COLUMN LIMIT AND COMPRESS MULTIPLE
!CCCC              SPACES TO A SINGLE SPACE.
!CCCC
!CCCC              NOTE THAT ANYTHING ENCLOSED IN DOUBLE QUOTES (34)
!CCCC              SINGLE QUOTES (39) ARE TREATED AS JUST A CHARACTER.
!CCCC
!CCCC              ADJUST UPPER COLUMN LIMIT (I.E., SUBTRACT BY 1
!CCCC              WHEN SKIP SPACE)
!CCCC
!
      IVECLM=0
      IF(ICOLL(1).GT.0)IVECLM=1
!
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
!
!CCCC CHECK FOR VECTOR COLUMN LIMITS, DO NOT STRIP ANY CHARACTERS
!CCCC IF FOUND.
!
      IF(IVECLM.EQ.1)THEN
        ICNT=ICOL2
        DO 105 J=1,ICOL2
          ISTOR1(J)=ISTOR2(J)
          IF(IB(J).NE.' ')IBLANK=1
  105   CONTINUE
        GO TO 109
      ENDIF
!
!CCCC DO103J=1,MAXCO2
      DO 103 J=1,ICOL2
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
!
!         2016/06: FOR DATE OR TIME DELIMITER, JUST CONVERT TO SPACE.
!
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
!
!     2016/06: IF SET READ IP ADDRESSES ON ENTERED, THEN LOOK FOR
!              FIELDS LIKE THE FOLLOWING:
!
!                 129.6.48.55
!
!              IF THIS FOUND, THEN CONVERT PERIODS TO SPACES SO THAT
!              THIS WILL BE READ AS 4 NUMERIC FIELDS.
!
!              DO NOT DO THIS IF THE FIELD IS EMBEDDED IN QUOTES.
!
      IF(IRDIPA.EQ.'ON')THEN
        IFLAGQ=0
        NDEC=0
        NPOS1=0
        NPOS2=0
        NPOS3=0
!
        DO 3103 J=1,ICNT
!
!         CHECK FOR QUOTE
!
          IF(IB(J).EQ.'"')THEN
            IF(IFLAGQ.EQ.0)THEN
              IFLAGQ=1
            ELSE
              IFLAGQ=0
            ENDIF
          ENDIF
          IF(IFLAGQ.EQ.1)GO TO 3103
!
!         CHECK FOR DECIMALS
!
!         LOOK FOR "." EMBEDDED WITHIN A STRING OF NUMBERS
!
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
!
      IF(ICNT.LT.MAXCO2)THEN
        DO 104 J=ICNT+1,MAXCO2
          IB(J)=' '
          ISTOR2(J)=IB(J)
  104   CONTINUE
      ENDIF
!
      IF(IBLANK.EQ.0)THEN
        LINETY='BLAN'
        GO TO 9000
      ENDIF
!
      N2=0
      DO 106 J=ICOL2,ICOL1,-1
        IF(IB(J)(1:1).NE.' ')THEN
          NLAST=J
          GO TO 107
        ENDIF
  106 CONTINUE
      NLAST=ICOL1
  107 CONTINUE
      DO 108 J=ICOL1,NLAST
        N2=N2+1
        IFUNC2(N2)=IB(J)
  108 CONTINUE
      ICOL22=N2+ICOL1-1
!
      IF(ICASRE.EQ.'FUNC')GO TO 8000
!
      IF(IB(1).EQ.'E'.AND.IB(2).EQ.'O'.AND.IB(3).EQ.'F')GO TO 8000
      IF(ICOL22.LT.ICOL1)THEN
        LINETY='BLAN'
        GO TO 9000
      ENDIF
!
!               *******************************************************
!               **  STEP 4--                                         **
!               **  SCAN FOR THE PHRASE     END DATA                 **
!               **  OR THE PHRASE           END OF DATA              **
!               **  OR THE PHRASE           END OF READ              **
!               **  OR THE PHRASE           EOF                      **
!               **  SCAN FOR THE PHRASE BETWEEN COLUMNS 1 TO ICOL2.  **
!               **  EXCEPTION--IF ICOL2 IS LESS THAT 11              **
!               **  (11 = THE NUMBER OF LETTERS IN THE PHRASE        **
!               **  END OF DATA  )                                   **
!               **  THEN EXPAND THE SCAN TO COVER THE COLUMNS 1 TO 11.*
!               *******************************************************
!
      ISTEPN='4'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!CCCC 2023/04: CHECK IF "EOF" IS EMBEDDED IN A LONGER STRING.  THIS
!CCCC          CASE SHOULD NOT BE INTERPRETED AS AND END OF FILE.
!
      JMAX=ICOL22
      IF(JMAX.GE.3)THEN
        DO 130 J=1,JMAX-2
          IF((IB(J).EQ.'E'.OR.IB(J).EQ.'e').AND.   &
             (IB(J+1).EQ.'O'.OR.IB(J+1).EQ.'o').AND.   &
             (IB(J+2).EQ.'F'.OR.IB(J+2).EQ.'f'))THEN
            IF(J.GT.1 .AND. IB(J-1).NE.' ')GO TO 130
            IF(J+3.LE.JMAX .AND. IB(J+3).NE.' ')GO TO 130
            GO TO 8000
          ENDIF
  130   CONTINUE
      ENDIF
!
      JMAX=ICOL22
      IF(JMAX.GE.11)THEN
        DO 132 J=1,JMAX-10
          IF((IB(J).EQ.'E'.OR.IB(J).EQ.'e').AND.   &
             (IB(J+1).EQ.'N'.OR.IB(J+1).EQ.'n').AND.   &
             (IB(J+2).EQ.'D'.OR.IB(J+2).EQ.'d').AND.   &
             (IB(J+3).EQ.' ').AND.   &
             (IB(J+4).EQ.'O'.OR.IB(J+4).EQ.'o').AND.   &
             (IB(J+5).EQ.'F'.OR.IB(J+5).EQ.'f').AND.   &
             (IB(J+6).EQ.' ').AND.   &
             (IB(J+7).EQ.'D'.OR.IB(J+7).EQ.'d').AND.   &
             (IB(J+8).EQ.'A'.OR.IB(J+8).EQ.'a').AND.   &
             (IB(J+9).EQ.'T'.OR.IB(J+9).EQ.'t').AND.   &
             (IB(J+10).EQ.'A'.OR.IB(J+10).EQ.'a'))GO TO 8000
  132   CONTINUE
      ENDIF
!
      JMAX=ICOL22
      IF(JMAX.GE.11)THEN
        DO 133 J=1,JMAX-10
          IF((IB(J).EQ.'E'.OR.IB(J).EQ.'e').AND.   &
             (IB(J+1).EQ.'N'.OR.IB(J+1).EQ.'n').AND.   &
             (IB(J+2).EQ.'D'.OR.IB(J+2).EQ.'d').AND.   &
             (IB(J+3).EQ.' ').AND.   &
             (IB(J+4).EQ.'O'.OR.IB(J+4).EQ.'o').AND.   &
             (IB(J+5).EQ.'F'.OR.IB(J+5).EQ.'f').AND.   &
             (IB(J+6).EQ.' ').AND.   &
             (IB(J+7).EQ.'R'.OR.IB(J+7).EQ.'r').AND.   &
             (IB(J+8).EQ.'E'.OR.IB(J+8).EQ.'e').AND.   &
             (IB(J+9).EQ.'A'.OR.IB(J+9).EQ.'a').AND.   &
             (IB(J+10).EQ.'D'.OR.IB(J+10).EQ.'d'))GO TO 8000
  133   CONTINUE
      ENDIF
!
      JMAX=ICOL22
      IF(JMAX.GE.8)THEN
        DO 134 J=1,JMAX-7
          IF((IB(J).EQ.'E'.OR.IB(J).EQ.'e').AND.   &
             (IB(J+1).EQ.'N'.OR.IB(J+1).EQ.'n').AND.   &
             (IB(J+2).EQ.'D'.OR.IB(J+2).EQ.'d').AND.   &
             (IB(J+3).EQ.' ').AND.   &
             (IB(J+4).EQ.'D'.OR.IB(J+4).EQ.'d').AND.   &
             (IB(J+5).EQ.'A'.OR.IB(J+5).EQ.'a').AND.   &
             (IB(J+6).EQ.'T'.OR.IB(J+6).EQ.'t').AND.   &
             (IB(J+7).EQ.'A'.OR.IB(J+7).EQ.'a'))GO TO 8000
  134   CONTINUE
      ENDIF
!
      JMAX=ICOL22
      IF(JMAX.GE.8)THEN
        DO 135 J=1,JMAX-7
          IF((IB(J).EQ.'E'.OR.IB(J).EQ.'e').AND.   &
             (IB(J+1).EQ.'N'.OR.IB(J+1).EQ.'n').AND.   &
             (IB(J+2).EQ.'D'.OR.IB(J+2).EQ.'d').AND.   &
             (IB(J+3).EQ.' ').AND.   &
             (IB(J+4).EQ.'R'.OR.IB(J+4).EQ.'r').AND.   &
             (IB(J+5).EQ.'E'.OR.IB(J+5).EQ.'e').AND.   &
             (IB(J+6).EQ.'A'.OR.IB(J+6).EQ.'a').AND.   &
             (IB(J+7).EQ.'D'.OR.IB(J+7).EQ.'d'))GO TO 8000
  135   CONTINUE
      ENDIF
!
!               *************************************
!               **  STEP 4.2--                     **
!               **  INCREMENT THE NUMBER OF CARDS  **
!               *************************************
!
      ISTEPN='4.2'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMCRD=NUMCRD+1
      IF(ICASRE.EQ.'ROWI')GO TO 9000
      IFLAGD=1
!
      IF(NCOLS.GT.0)ICOL1=ICOLL(1)
      I=ICOL1
!
!  START OF A NEW VARIABLE.  BASIC ALGORITHM IS:
!
!  1) IF FIRST CHARACTER IS A NUMBER OR A "+" OR "-" OR A ".", ASSUME
!     WE HAVE A NUMBER.
!
!     A) A NUMBER MAY CONTAIN A ".", "+", "-", "E", OR "D".
!
!        2005/04: DECIMAL POINT IS NOW USER SETTABLE.
!
!        2018/06: TWO ISSUES WITH EXPONENTIAL FORMAT
!
!                 1) NUMBERS LIKE  3.27+301
!
!                    IF THE EXPONENENT IS 3 DIGITS LONG, THE FORTRAN
!                    COMPILER MAY DROP THE "E" OR "D".  SO IF ENCOUNTER
!                    "+" OR "-" IN THE MIDDLE OF THE FIELD, CHECK FOR
!                    THIS CASE.
!
!                 2) THE STRING   3DResNet   FAILED.  THIS IS DUE TO THE
!                    FACT THAT "3D" IS INTERPRETED AS EXPONENTIAL
!                    NOTATION.  SO WHEN ENCOUNTER A "D" OR "E", CHECK
!                    WHAT FOLLOWS TO MAKE SURE THAT IT IS IN FACT NOT
!                    A CHARACTER FIELD.
!
!     B) IF A SINGLE OR DOUBLE QUOTE IS ENCOUNTERED, END NUMBER
!        AND START A STRING.
!
!     C) IF A SPACE, ",", ":", ";", "/", "\", "[", "(", ")", "]",
!        TREAT AS A DELIMITER.  THAT IS, END THE NUMERIC VARIABLE.
!
!     D) MAKE EXPLICIT CHECK FOR "NAN" AND SYMBOLIC MISSING VALUE
!        (SET BY IDATMV).
!
!     E) ANY OTHER CHARACTER IS ASSUMED TO BE PART OF A CHARACTER
!        VARIABLE.  DEPENDING ON IGRPAU:
!
!        ERROR      - TREAT NON-NUMERIC CHARACTER AS AN ERROR
!        IGNORE     - SKIP THE CURRENT CHARACTER AND ALL SUBSEQUENT
!                     CHARACTERS UNTIL THE NEXT NUMERIC VARIABLE IS
!                     FOUND.
!        CHARACTER  - SEARCH UNTIL ONE OF THE DELIMITERS IS FOUND
!                     AND SAVE STRING IN IXC.
!        CATEGORICAL - SAME AS "CHARACTER" IN THIS ROUTINE
!
!  2) IF FIRST CHARACTER IS A DELIMITER, SIMPLY GO TO NEXT CHARACTER.
!
!     CHECK FOR SPECIAL CASE WHERE DELIMITER IS LAST CHARACTER
!     OF THE RECORD.  IN THIS CASE, NEED TO ADD A MISSING VALUE
!     TO THE LIST.
!
!  3) IF THE FIRST CHARACTER IS A NON-NUMERIC CHARACTER AND NOT A
!     DELIMITER, THEN ASSUME A CHARACTER VARIABLE.  INTERPERT BASED
!     ON VALUE OF IGRPAU AS DESCRIBED ABOVE.
!
!     IF THE FIRST CHARACTER IS A SINGLE OR DOUBLE QUOTE, ASSUME
!     ANY CHARACTERS UNTIL MATCHING QUOTE FOUND IS PART OF THE
!     CHARACTER VARIABLE.
!
!  APRIL 2005.  FOR VECTOR COLUMN LIMITS, CHECK IMMEDIATELY FOR
!               A BLANK FIELD.  IF BLANK FIELD ENCOUNTERED, THEN
!               SET TO MISSING VALUE AND CONTINUE TO NEXT FIELD.
!
!  APRIL 2017.
!
!               1) CHECK FOR EMPTY FIRST FIELD (E.G., ",23,12").  THAT
!                  IS, NEED TO INSERT AN INITIAL MISSING VALUE FOR THIS
!                  CASE.
!
!               2) ALLOW MISSING DATA FOR CHARACTER FIELDS.  USER CAN
!                  SET PREFERRED STRING (ZZZZNULL BY DEFAULT).
!
!               3) CHECK FOR FIELDS LIKE
!
!                      $236,219.11
!
!                  MONETARY DATA MAY ON OCCASSION BE ENTERED THIS WAY.
!                  USER CAN ENTER COMMANDS
!
!                      SET READ DOLLAR SIGN IGNORE <ON/OFF>
!                      SET READ COMMA <ON/OFF>
!
!                  THE FIRST COMMAND SPECIFIES WHETHER A DOLLAR SIGN IN
!                  A NUMERIC FIELD WILL BE IGNORED OR NOT.  THE SECOND
!                  COMMAND SPECIFIES THAT "," IN A NUMERIC FIELD WILL BE
!                  IGNORED.  NOTE THAT THIS MEANS THAT YOU CANNOT SET
!                  THE "," AS A DECIMAL POINT (CONVENTION IN SOME
!                  EUROPEAN COUNTRIES) AND FURTHER THAT YOU CANNOT TREAT
!                  THE COMMA AS A DELIMITER (DATAPLOT HAS NO WAY TO
!                  DISTINGUISH WHETHER YOU INTEND A COMMAND TO MEAN A
!                  DELIMITER OR TO SIGNIFY MONETARY DATA).
!
  149 CONTINUE
      NREAD=NREAD+1
!
!     CHECK IF FIELD IS CHARACTER OR NUMERIC
!
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
!
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
        WRITE(ICOUT,30149)NREAD,NCALL,ITYP,NCOLS,I
30149   FORMAT('AT 149: NREAD,NCALL,ITYP,NCOLS,I = ',5I6)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(NCOLS.GT.0)THEN
!
!       CASE WHERE COLUMN LIMITS HAVE BEEN SPECIFIED.  NOTE THAT
!       THIS CASE HAS NOT GONE THROUGH CLEANING PROCESS, SO CHECK
!       FOR LEADING/TRAILING SPACES.
!
        IF(NREAD.GT.NCOLS)GO TO 9000
        I=ICOLL(NREAD)
        ICOL22=ICOLU(NREAD)
        DO 22120 II=I,ICOL22
          IF(IB(II).NE.' ')THEN
            I=II
            GO TO 22129
          ENDIF
22120   CONTINUE
22129   CONTINUE
!
        DO 22130 II=ICOL22,I,-1
          IF(IB(II).NE.' ')THEN
            ICOL22=II
            GO TO 22139
          ENDIF
22130   CONTINUE
22139   CONTINUE
!
!       CHECK FOR BLANK FIELD.  ALSO CHECK IF FIRST CHARACTER IS THE
!       FIELD DELIMITER.  IN EITHER CASE, SET TO MISSING VALUE.  BUT
!       NEED TO CHECK WHETHER WE ARE SETTING THE MISSING VALUE FOR A
!       NUMERIC VARIABLE OR A CHARACTER VARIABLE.
!
        IF(IB(I).NE.IREADL)THEN
          DO 22140 II=I,ICOL22
!CCCC       IF(IB(II).NE.' ')GO TO 22149
            IF(IB(II).NE.IREADL)GO TO 22149
22140     CONTINUE
        ENDIF
!
        IF(ITYP.EQ.1)THEN
          IF(NXC.LT.MAXCHV)THEN
            NXC=NXC+1
            IXC(NXC)=' '
            IXC(NXC)(1:8)=IREAMC(1:8)
          ENDIF
        ELSE
          N=N+1
          X(N)=PREAMV
          IF(IREALT.EQ.'ON' .OR. IREAPM.EQ.'ON')XTAG(N)=0.0
        ENDIF
        GO TO 149
!
22149   CONTINUE
      ENDIF
!
!     DETERMINE FIRST AND LAST NON-BLANK CHARACTERS IN CURRENT COLUMN
!
!     IF "SET CHARACTER FIELD COMMA DELIMITER ON" COMMAND WAS ENTERED,
!     SEARCH FOR A ",".
!
!     2020/06/22: FOR COMMA DELIMITED FIELDS, CHECK FOR LEADING QUOTE (").
!                 IF FOUND, THEN IGNORE ALL COMMAS UNTIL MATCHING QUOTE
!                 FOUND.
!
      IF(I.GT.ICOL22)GO TO 9000
!
      IFRST=I
      ILAST=ICOL22
      ILAST2=ICOL22
      IFLAGZ=0
!
      IF(IREACD.EQ.'ON')THEN
        IFLAGQ=0
        IFLGQ2=0
        DO 11150 L=I,ICOL22
          IF(IB(L).NE.' ')THEN
            IFRST=L
            GO TO 11151
          ENDIF
11150   CONTINUE
        IFRST=ICOL22
11151   CONTINUE
!
        IF(IB(IFRST).EQ.'"')THEN
!
!         IF FIRST CHARACTER IS DOUBLE QUOTE, THEN SEARCH FOR ENDING
!         MATCHING QUOTE.  FOR NOW, DO NOT SUPPORT QUOTES (E.G., \" OR
!         """) EMBEDDED WITHIN QUOTED STRING.  ONCE ENDING QUOTE FOUND,
!         SEARCH FOR THE NEXT COMMA TO INDICATE START OF NEXT FIELD.
!
          IFLAGQ=1
          IFLGQ2=1
          DO 81153 L=IFRST+1,ICOL22
            IF(IFLAGQ.EQ.1)THEN
              IF(IB(L).EQ.'"')THEN
                ILAST2=L
                IFLAGQ=0
              ENDIF
            ELSE
              IF(IB(L).EQ.',')THEN
                ILAST=L
                GO TO 81156
              ENDIF
            ENDIF
81153     CONTINUE
          ILAST=ICOL22
81156     CONTINUE
        ELSE
          DO 11153 L=IFRST,ICOL22
            IF(IB(L).EQ.',')THEN
              ILAST=L
              GO TO 11154
            ENDIF
11153     CONTINUE
          ILAST=ICOL22
11154     CONTINUE
          ILAST2=ILAST
        ENDIF
!
!       IF USER HAS SPECIFIED A READ DELIMITER OTHER THAN A BLANK SPACE,
!       THEN SEARCH FOR THE NEXT DELIMITER.
!
      ELSEIF(IREADL.NE.' ')THEN
        DO 12150 L=I,ICOL22
          IF(IB(L).NE.' ')THEN
            IFRST=L
            GO TO 12151
          ENDIF
12150   CONTINUE
        IFRST=ICOL22
12151   CONTINUE
        DO 12153 L=IFRST,ICOL22
          IF(IB(L).EQ.IREADL)THEN
            ILAST=L
            ILAST2=L-1
            GO TO 12154
          ENDIF
12153   CONTINUE
        ILAST=ICOL22
        ILAST2=ILAST
12154   CONTINUE
      ELSE
!
!       SPACE DELIMITED CASE
!
        DO 1153 L=I,ICOL22
          IF(IFLAGZ.EQ.0.AND.IB(L).NE.' ')THEN
             IFRST=L
             IFLAGZ=1
             GO TO 1153
          ELSEIF(IFLAGZ.EQ.1 .AND.   &
                (IB(L).EQ.' ' .OR. IB(L).EQ.IREADL .OR.   &
                 IB(L).EQ.IDATDL .OR. IB(L).EQ.ITIMDL))THEN
            ILAST=L
            ILAST2=L-1
            GO TO 1154
          ENDIF
 1153   CONTINUE
 1154   CONTINUE
!CCCC   DO1157L=ICOL22,I,-1
!CCCC     IF(IB(L).NE.' ')THEN
!CCCC       ILAST=L
!CCCC       ILAST2=L
!CCCC       GO TO 1159
!CCCC     ENDIF
!1157   CONTINUE
!CCCC   ILAST=IFRST
!CCCC   ILAST2=ILAST
!1159   CONTINUE
      ENDIF
!
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
        WRITE(ICOUT,1148)NREAD,NCALL,I,ICOL22
 1148   FORMAT('NREAD,NCALL,I,ICOL22 = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1149)ITYPE(NREAD),IFRST,ILAST,ILAST2
 1149   FORMAT('ITYPE(NREAD),IFRST,ILAST,ILAST2 = ',4I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
  150 CONTINUE
!
      DO 151 J=1,41
        ICHAR3(J)=' '
  151 CONTINUE
!
      NC=0
      NDP=0
      NSIGN=0
      NCE=0
      NUMDEX=0
      ISUMEX=0
      LOCPT=0
      AFACT=1.0
      NSTR=0
!
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
!
!               **************************************************
!               **  VECTOR COLUMN LIMITS CASE                   **
!               **************************************************
!
!     NOTE 2012/09: CHECK IF USER HAS SPECIFIED FIELD AS
!                   CHARACTER TYPE.
!
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
        WRITE(ICOUT,2149)NCALL,NCOLS,ITYPE(NREAD),IFIETY(NREAD),ITYP
 2149   FORMAT('NCALL,NCOLS,ITYPE(NREAD),IFIETY(NREAD),ITYP = ',5I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!
!  HANDLE CHARACTER FIELD IMMEDIATELY
!
!  SEPTEMBER 2006: FOR FIRST OCCURENCE (NCALL=0), NEED TO
!                  MAKE SURE WE GET FULL FIELD IF CHARACTER
!                  DATA.
!
!     VECTOR COLUMN LIMITS CASE
!
      IF(NCOLS.GT.0 .AND. ITYP.EQ.1)THEN
        IF(IGRPAU.EQ.'IGNO')GO TO 149
        IF(IFRST.LT.ILAST)GO TO 154
        IF(IFRST.EQ.ILAST.AND.IB(IFRST).NE.' ')GO TO 154
        IF(NXC.LT.MAXCHV)THEN
          NXC=NXC+1
          IXC(NXC)(1:8)=IREAMC(1:8)
        ENDIF
        GO TO 149
  154   CONTINUE
        IF(NXC.LT.MAXCHV)THEN
          NXC=NXC+1
          IXC(NXC)=' '
          J=0
          DO 158 II=IFRST,ILAST
            IF(J.LE.24)THEN
              J=J+1
              IXC(NXC)(J:J)=IB(II)(1:1)
            ENDIF
  158     CONTINUE
        ENDIF
        GO TO 149
!
!     NO VECTOR COLUMN LIMITS CASE.  FOR THIS CASE,
!     SEARCH FOR FIRST AND LAST NON-BLANK CHARACTERS.
!     ALSO NEED TO CHECK FOR LEADING QUOTE MARK.
!
!     NOTE: SOME FILES DELINEATE CHARACTER FIELDS BY
!           COMMAS AND DO NOT INCLUDE CHARACTER DATA
!           IN QUOTES EVEN IF IT CONTAINS SPACES.  IF
!           THE COMMAND
!
!              SET CHARACTER FIELD COMMA DELIMITER ON
!
!           WAS ENTERED, THEN DELINEATE FIELDS BY COMMAS
!           IRRESPECTIVE OF SPACES AND QUOTES.
!
      ELSEIF(NCOLS.EQ.0 .AND. ITYP.EQ.1)THEN
!
!       STEP 1: FIND FIRST NON-BLANK CHARACTER
!
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
          WRITE(ICOUT,21161)I,IFRST,ILAST
21161     FORMAT('AT NCOLS = 0, ITYPE = 1, I,IFRST,ILAST = ',3I6)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        IF(IB(IFRST).EQ.'"' .AND. IB(ILAST2).EQ.'"')IFLGQ2=1
        DO 161 II=IFRST,ILAST
          IF(IB(II).NE.' ')THEN
            IFRST=II
            GO TO 162
          ENDIF
  161   CONTINUE
        GO TO 9000
!
!       STEP 2: FIND END OF CURRENT FIELD.
!
!               A) LOOK FOR MATCHING QUOTE
!               B) FOR  "SET CHARACTER FIELD COMMA DELIMITER ON CASE,
!                  SEARCH FOR NEXT ",".
!               C) OTHERWISE SEARCH FOR SPACE OR DELIMITER.
!
  162   CONTINUE
        IF(IFLGQ2.EQ.1)THEN
          ISTRT=IFRST+1
          IFLAGQ=1
        ELSE
          ISTRT=IFRST
          IFLAGQ=0
        ENDIF
        DO 164 II=ISTRT,ILAST
          IF(IFLAGQ.EQ.1)THEN
            IF(IB(II).EQ.'"')THEN
              ILAST2=II
              IFLAGQ=0
              GO TO 164
            ENDIF
          ELSEIF(IREACD.EQ.'ON' .AND. IB(II).EQ.',')THEN
            ILAST=II-1
            GO TO 165
          ELSEIF(IB(II).EQ.IREADL .OR.   &
                 IB(II).EQ.IDATDL .OR. IB(II).EQ.ITIMDL)THEN
              ILAST=II-1
              GO TO 165
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
            DO 168 II=ISTRT,ISTOP
              IF(J.LE.24)THEN
                J=J+1
                IXC(NXC)(J:J)=IB(II)(1:1)
              ENDIF
  168       CONTINUE
            IF(J.LE.1 .AND. IXC(NXC)(1:1).EQ.' ')   &
               IXC(NXC)(1:8)=IREAMC(1:8)
          ENDIF
!
          IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
            WRITE(ICOUT,169)NXC,IXC(NXC)(1:J),IFRST,ILAST
  169       FORMAT('AT 168: NXC,IXC(NXC),IFRST,ILAST=',I8,A24,2X,2I5)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
        ENDIF
        IF(IREACD.EQ.'ON')THEN
          I=ILAST+2
        ELSE
          IF(IB(ILAST+1).EQ.IREADL .OR. IB(ILAST+1).EQ.ITIMDL .OR.   &
             IB(ILAST+1).EQ.IDATDL)THEN
            I=ILAST+2
          ELSE
            I=ILAST+1
          ENDIF
        ENDIF
        GO TO 149
      ELSEIF(NCOLS.GE.0 .AND. NCALL.EQ.0)THEN
!
!       CHECK FOR CHARACTER FIELD.  CHECK FOR FIRST NON-BLANK CHARACTER
!       AND IF IT IS NOT A NUMBER, A DECIMAL POINT, OR A +/- SIGN, THEN
!       ASSUME CHARACTER.  NOTE THAT THIS IS A QUICK CHECK, MAY MISS
!       SOME CASES.  SO IF YOU HAVE A CHARACTER STRING THAT STARTS WITH
!       A NUMBER, DECIMAL POINT, OR +/- SIGN AND ALSO CONTAINS EMBEDDED
!       SPACES, YOU MAY HAVE A TRUNCATED STRING FOR FIRST LINE.
!
!       AMBIGUOUS CASE: IF FIRST CHARACTER IS ",", THEN DO WE HAVE A
!       NUMERIC OR A CHARACTER FIELD?  IF TYPE IS NOT ALREADY
!       DETERMINED, ASSUME NUMERIC FIELD.
!
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
          WRITE(ICOUT,21162)I,IFRST,ILAST
21162     FORMAT('AT NCOLS >= 0, NCOL = 0, I,IFRST,ILAST = ',3I6)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        IF(IFRST.EQ.ILAST .AND. IB(IFRST).EQ.' ')GO TO 7199
!
!       NOW CHECK FIRST CHARACTER (BUT FIRST CHECK FOR NAN AND
!       MISSING VALUE SINCE THESE WILL BE INTERPRETED AS NUMERIC
!       FIELDS)
!
        IF(ILAST-IFRST+1.EQ.3)THEN
          IF((IB(IFRST).EQ.'N' .OR. IB(IFRST).EQ.'n') .AND.   &
             (IB(IFRST+1).EQ.'A' .OR. IB(IFRST+1).EQ.'a') .AND.   &
             (IB(IFRST+2).EQ.'N' .OR. IB(IFRST+2).EQ.'n') .AND.   &
             IB(IFRST+3).EQ.' ')THEN
            GO TO 7199
          ENDIF
        ENDIF
!
        IF(NCDNAN.GT.0 .AND. ILAST-IFRST+1.EQ.NCDNAN .AND.   &
           IDATNN.NE.'NAN')THEN
          DO 77167 LL=1,NCDNAN
            IF(IB(IFRST+LL-1).NE.IDATNN(LL:LL))GO TO 77169
77167     CONTINUE
          GO TO 7199
77169     CONTINUE
        ENDIF
!
        IF(NCDAMV.GT.0 .AND. ILAST-IFRST+1.EQ.NCDAMV)THEN
          DO 77157 LL=1,NCDAMV
            IF(IB(IFRST+LL-1).NE.IDATMV(LL:LL))GO TO 77159
77157     CONTINUE
          GO TO 7199
77159     CONTINUE
        ENDIF
!
!       IF "SET DOLLAR SIGN IGNORE ON" COMMAND WAS ENTERED,
!       CHECK FOR (AND IGNORE) LEADING "$".  NOTE THAT THIS
!       ONLY APPLIES TO THE FIRST CHARACTER IN THE FIELD.
!
!       ALSO, IF "SET TRAILING PLUS MINUS IGNORE ON" COMMAND
!       WAS ENTERED, IGNORE A "+" OR "-" CHARACTER IN THE LAST
!       COLUMN OF THE FIELD (SOME FILES USE THIS SYNTAX TO
!       INDICATE A VALUE GREATER THAN OR EQUAL TO (+) OR LESS
!       THAN OR EQUAL TO (-).  DATAPLOT WILL SIMPLY IGNORE THIS
!       TRAILING +/- SIGN.
!
!       IF "SET ASTERISK IGNORE ON" COMMAND WAS ENTERED, CHECK
!       FOR EITHER LEADING OR TRAILING ASTERISK.  UP TO 3 TRAILING
!       ASTERISKS ARE CHECKED FOR.
!
!       2025/02: FOR LEADING "<" OR ">" OR TRAILINE "+" OR "-",
!                WRITE TAG VARIABLES TO dpst5f.dat WHERE
!
!                   0 => EQUAL TO     THE GIVEN DATA VALUE
!                   1 => LESS    THAN THE GIVEN DATA VALUE
!                   2 => GREATER THAN THE GIVEN DATA VALUE
!
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
          WRITE(ICOUT,21662)IFRST,ILAST,IB(IFRST),IB(ILAST)
21662     FORMAT('21622: IFRST,ILAST,IB(IFRST),IB(ILAST) = ',2I5,2(2X,A4))
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        TAGZZ=0.0
        IF(IREADS.EQ.'ON' .AND. IB(IFRST).EQ.'$')IFRST=IFRST+1
        IF(IREAAS.EQ.'ON' .AND. IB(IFRST).EQ.'*')IFRST=IFRST+1
        IF((IREALT.EQ.'ON' .OR. IREALT.EQ.'MV') .AND. IB(IFRST).EQ.'<')THEN
          TAGZZ=1.0
          IFRST=IFRST+1
          IF(IB(IFRST).EQ.' ')IFRST=IFRST+1
        ENDIF
        IF((IREALT.EQ.'ON' .OR. IREALT.EQ.'MV') .AND. IB(IFRST).EQ.'>')THEN
          TAGZZ=2.0
          IFRST=IFRST+1
          IF(IB(IFRST).EQ.' ')IFRST=IFRST+1
        ENDIF
        IF(IREAPM.EQ.'ON' .AND. IB(ILAST).EQ.'+')THEN
          TAGZZ=2.0
          IB(ILAST)=' '
        ENDIF
        IF(IREAPM.EQ.'ON' .AND. IB(ILAST).EQ.'-')THEN
          TAGZZ=1.0
          IB(ILAST)=' '
        ENDIF
!
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
          WRITE(ICOUT,21664)TAGZZ
21664     FORMAT('21664: TAGZZ = ',F5.1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
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
        ELSEIF(IREAAS.EQ.'ON' .AND. IB(ILAST).EQ.' ' .AND.   &
               IB(ILAST-1).EQ.'*')THEN
          IB(ILAST-1)=' '
          IF(IB(ILAST-2).EQ.'*')THEN
            IB(ILAST-2)=' '
            IF(IB(ILAST-3).EQ.'*')THEN
              IB(ILAST-3)=' '
            ENDIF
          ENDIF
        ENDIF
        IF(IB(IFRST).EQ.'.')GO TO 7199
        IF(IB(IFRST).EQ.'+')GO TO 7199
        IF(IB(IFRST).EQ.'-')GO TO 7199
        IF(IB(IFRST).EQ.'0')GO TO 7199
        IF(IB(IFRST).EQ.'1')GO TO 7199
        IF(IB(IFRST).EQ.'2')GO TO 7199
        IF(IB(IFRST).EQ.'3')GO TO 7199
        IF(IB(IFRST).EQ.'4')GO TO 7199
        IF(IB(IFRST).EQ.'5')GO TO 7199
        IF(IB(IFRST).EQ.'6')GO TO 7199
        IF(IB(IFRST).EQ.'7')GO TO 7199
        IF(IB(IFRST).EQ.'8')GO TO 7199
        IF(IB(IFRST).EQ.'9')GO TO 7199
        IF(IB(IFRST).EQ.',')GO TO 7199
!
        IF(NXC.LT.MAXCHV)THEN
          NXC=NXC+1
          IXC(NXC)=' '
          J=0
!
          IF(IB(IFRST).EQ.'"' .AND. IB(ILAST2).EQ.'"')IFLGQ2=1
          IF(IFLGQ2.EQ.1)THEN
            ISTRT=IFRST+1
            ISTOP=ILAST2-1
          ELSE
            ISTRT=IFRST
            ISTOP=ILAST
          ENDIF
          DO 7158 II=ISTRT,ISTOP
            IF(II.EQ.ILAST .AND. IB(II).EQ.IREADL)GO TO 7158
            IF(J.LE.24)THEN
              J=J+1
              IXC(NXC)(J:J)=IB(II)(1:1)
            ENDIF
 7158     CONTINUE
          IF(J.LE.1 .AND. IXC(NXC)(1:1).EQ.' ')IXC(NXC)(1:8)=IREAMC(1:8)
        ENDIF
        ITYPE(NREAD)=1
!
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
          WRITE(ICOUT,7156)NXC,IXC(NXC)(1:J),IFRST,ILAST,J
 7156     FORMAT('AT 7156: NXC,IXC(NXC),IFRST,ILAST,J=',I8,A24,3I5)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        I=ILAST+1
!CCCC   IF(IREADL.NE.' ' .AND. IB(ILAST).EQ.IREADL)THEN
!CCCC     I=ILAST+2
!CCCC   ELSE
!CCCC     I=ILAST+1
!CCCC   ENDIF
        GO TO 149
      ENDIF
!
 7199 CONTINUE
      IF(NCALL.EQ.0)ITYPE(NREAD)=1
!
!               **************************************************
!               **  STEP 6--                                    **
!               **  EXAMINE THE I-TH CHARACTER IN THIS STRING,  **
!               **************************************************
!
!
!     NOTE 4/2009: CHECK FOR "NaN" OR MISSING VALUE FIRST
!     NOTE 4/2010: ALLOW USER SPECIFIED "NAN" VALUE
!
      IF(ILAST2-IFRST+1.EQ.3)THEN
        IF((IB(IFRST).EQ.'N' .OR. IB(IFRST).EQ.'n') .AND.   &
             (IB(IFRST+1).EQ.'A' .OR. IB(IFRST+1).EQ.'a') .AND.   &
             (IB(IFRST+2).EQ.'N' .OR. IB(IFRST+2).EQ.'n'))THEN
          IF(NCALL.EQ.0)THEN
            ITYPE(NREAD)=0
            IF(NREAD.GE.1 .AND. NREAD.LE.250 .AND.   &
               IFIETY(NREAD).EQ.1)ITYPE(NREAD)=1
          ENDIF
          N=N+1
          X(N)=PREAMV
          IF(IREALT.EQ.'ON' .OR. IREAPM.EQ.'ON')XTAG(N)=TAGZZ
          I=I+3
          IF(I.LE.ICOL22 .OR. NCOLS.GT.0)GO TO 149
          GO TO 9000
        ENDIF
      ENDIF
!
!     FOR NAN VALUE, WE DON'T WANT IT TO BE CASE SENSITIVE.
!     SO CONVERT TO UPPER CASE BEFORE COMPARE.
!
      IF(NCDNAN.GT.0 .AND. ILAST2-IFRST+1.EQ.NCDNAN .AND.   &
         IDATNN.NE.'NAN')THEN
        DO 77281 LL=1,NCDNAN
          CALL DPCOAN(IB(IFRST+LL-1),IASC1)
          CALL DPCOAN(IDATNN(LL:LL),IASC2)
          IF(IASC1.GE.97 .AND. IASC1.LE.122)IASC1=IASC1-32
          IF(IASC2.GE.97 .AND. IASC2.LE.122)IASC2=IASC2-32
          IF(IASC1.NE.IASC2)GO TO 77289
77281   CONTINUE
        IF(NCALL.EQ.0)THEN
          ITYPE(NREAD)=0
          IF(NREAD.GE.1 .AND. NREAD.LE.250 .AND.   &
             IFIETY(NREAD).EQ.1)ITYPE(NREAD)=1
        ENDIF
        N=N+1
        X(N)=PREAMV
        IF(IREALT.EQ.'ON' .OR. IREAPM.EQ.'ON')XTAG(N)=TAGZZ
        I=I+NCDNAN
        IF(IB(I+1).EQ.IREADL .OR. IB(I+1).EQ.IDATDL .OR.   &
           IB(I+1).EQ.ITIMDL)I=I+1
        IF(I.LE.ICOL22 .OR. NCOLS.GT.0)GO TO 149
        GO TO 9000
77289   CONTINUE
      ENDIF
!
!     FOR MISSING VALUE, WE DON'T WANT IT TO BE CASE SENSITIVE.
!     SO CONVERT TO UPPER CASE BEFORE COMPARE.
!
      IF(NCDAMV.GT.0 .AND. ILAST2-IFRST+1.EQ.NCDAMV)THEN
        DO 77181 LL=1,NCDAMV
          CALL DPCOAN(IB(IFRST+LL-1),IASC1)
          CALL DPCOAN(IDATMV(LL:LL),IASC2)
          IF(IASC1.GE.97 .AND. IASC1.LE.122)IASC1=IASC1-32
          IF(IASC2.GE.97 .AND. IASC2.LE.122)IASC2=IASC2-32
          IF(IASC1.NE.IASC2)GO TO 77189
77181   CONTINUE
        IF(NCALL.EQ.0)THEN
          ITYPE(NREAD)=0
          IF(NREAD.GE.1 .AND. NREAD.LE.250 .AND.   &
             IFIETY(NREAD).EQ.1)ITYPE(NREAD)=1
        ENDIF
        N=N+1
        X(N)=PREAMV
        IF(IREALT.EQ.'ON' .OR. IREAPM.EQ.'ON')XTAG(N)=TAGZZ
        I=I+NCDAMV
        IF(IB(I+1).EQ.IREADL .OR. IB(I+1).EQ.IDATDL .OR.   &
           IB(I+1).EQ.ITIMDL)I=I+1
        IF(I.LE.ICOL22 .OR. NCOLS.GT.0)GO TO 149
        GO TO 9000
77189   CONTINUE
      ENDIF
!
  160 CONTINUE
!
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
        ISTEPN='6'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,1161)I,IB(I),N,NXC
 1161   FORMAT('I,IB(I),N,NXC = ',I8,2X,A4,2X,2I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *****************************
!               **  STEP 6.1--             **
!               **  TREAT THE 0 TO 9 CASE  **
!               *****************************
!
!     CHECK IF LAST CHARACTER IS THE READ DELIMITER
!
!     2019/10: IGNORE "%" IF LAST CHARACTER IN FIELD
!
      IBASCI=ICHAR(IB(I)(1:1)) - IZERO
      IF(IBASCI.GE.0 .AND. IBASCI.LE.9)THEN
        IF(NC.EQ.0 .AND. NSIGN.EQ.0)NSIGN=NSIGN+1
        NC=NC+1
        ICHAR3(NC)=IB(I)
        I=I+1
        IF(I.LE.ILAST)GO TO 160
        GO TO 1050
      ELSEIF(IB(I).EQ.',' .AND. IREACM.EQ.'ON')THEN
        I=I+1
        GO TO 160
      ELSEIF(IB(I).EQ.'%')THEN
        IF(IREAPC.EQ.'ON')THEN
          IF(I.EQ.ILAST)GO TO 1050
          IF(IB(I+1).EQ.IREADL .AND. I.EQ.ILAST-1)GO TO 1050
        ENDIF
      ELSEIF(IB(I).EQ.'+' .OR. IB(I).EQ.'-')THEN
!
!               **************************
!               **  STEP 6.3--          **
!               **  TREAT THE +/- CASE  **
!               **************************
!
!       2017/04: IGNORE "+/-" IF LAST CHARACTER IN FIELD
!
!       2018/06: ISSUE WITH "-3.27+309".  NOTE THAT "3.27+309" DOES
!                WORK AS EXPECTED.  THE ISSUE IS WHEN THERE ARE 2
!                "+/-" SIGNS IN THE FIELD.
!
        ISTEPN='6.3'
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')   &
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!       IGNORE TRAILING +/- OR TRAILING % IF REQUESTED
!
!       NOTE THAT ILAST MAY BE THE DELIMITER, SO MAY NEED TO
!       CHECK CHARACTER BEFORE DELIMITER
!
        TAGZZ=0.0
        IF(IREAPM.EQ.'ON')THEN
          IF(I.EQ.ILAST)THEN
            IF(IB(I).EQ.'+')TAGZZ=2.0
            IF(IB(I).EQ.'-')TAGZZ=1.0
            GO TO 1050
          ENDIF
          IF(IB(I+1).EQ.IREADL .AND. I.EQ.ILAST-1)GO TO 1050
        ENDIF
!
!       FIRST CHECK IF THIS +/- IS FIRST CHARACTER OF FIELD
!
        IF(I.EQ.IFRST)THEN
          NSIGN=NSIGN+1
          AFACT=1.0
          IF(IB(I).EQ.'-')AFACT=-1.0
          I=I+1
          IF(I.LE.ICOL22)GO TO 160
          N=N+1
          X(N)=AFACT
          IF((IREALT.EQ.'ON'.OR.IREALT.EQ.'MV') .OR. IREAPM.EQ.'ON')   &
              XTAG(N)=TAGZZ
          IF(IREALT.EQ.'MV' .AND. TAGZZ.GE.0.5)X(N)=PREAMV
          GO TO 149
        ENDIF
!
!       NOW CASE WHERE +/- OCCURS IN MIDDLE OF FIELD.  NOTE THAT
!       ONLY ONE +/- SIGN SHOULD OCCUR (NOT INCLUDING A LEADING
!       +/- SIGN).
!
!
        IF(NC.NE.0)THEN
          NSIGN=NSIGN+1
          IF(NSIGN.GT.2)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2001)
 2001       FORMAT('***** INPUT    DATA ERROR--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2022)
 2022       FORMAT('      AN UNEXPECTED  PLUS OR MINUS HAS OCCURRED ',   &
                  'IN THE MIDDLE OF A DATA VALUE.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2021)
 2021       FORMAT('      THIS ILLEGAL DATA VALUE AND ALL SUBSEQUENT ',   &
                   'DATA VALUES ON THIS LINE HAVE BEEN DELETED')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 8100
          ENDIF
!
          IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
            WRITE(ICOUT,3001)I,IB(I)
 3001       FORMAT('AT START OF EXPONENT EVALUATION--I,IB(I) = ',   &
                   I6,2X,A1)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
          NCE=1
          NUMDEX=NCE-1
          ICHEXP(1)=IB(I)
          I=I+1
          IF(I.LE.ICOL22)THEN
 3160       CONTINUE
!
            IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
              WRITE(ICOUT,3002)I,IB(I),IREADL
 3002         FORMAT('IN MIDST OF EXPONENT EXTRACTION--I,IB(I) = ',   &
                     I6,2X,A1)
              CALL DPWRST('XXX','BUG ')
            ENDIF
!
!           ONLY NUMERIC VALUES ALLOWED AT THIS POINT (HOWEVER,
!           CHECK FOR DELIMITER)
!
            IF(IB(I).EQ.' ' .OR. IB(I).EQ.IREADL)THEN
              GO TO 1050
            ENDIF
!
            IBASCI=ICHAR(IB(I)(1:1)) - IZERO
            IF(IBASCI.GE.0 .AND. IBASCI.LE.9)THEN
              NCE=NCE+1
              NUMDEX=NCE-1
              ICHEXP(NCE)=IB(I)
              I=I+1
              IF(I.LE.ICOL22)GO TO 3160
              GO TO 1050
            ELSE
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2001)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2051)
 2051         FORMAT('      A NON-NUMERIC CHARACTER HAS OCCURRED IN ',   &
                     'THE EXPONENT ON THE CURRENT LINE.')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2052)IB(I)
 2052         FORMAT('THE ILLEGAL CHARACTER WAS ',A1)
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GO TO 8100
            ENDIF
            GO TO 1050
          ENDIF
          GO TO 1050
        ENDIF
!
!               **********************************
!               **  STEP 6.3B-                 **
!               **  TREAT THE <  AND >   CASES **
!               *********************************
!
      ELSEIF(I.EQ.IFRST .AND.                              &
             (IREALT.EQ.'ON' .OR. IREALT.EQ.'MV') .AND.    &
             (IB(I).EQ.'<' .OR. IB(I).EQ.'>'))THEN
!
        ISTEPN='6.3A'
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')   &
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        TAGZZ=0.0
        IF(IB(I).EQ.'<')TAGZZ=1.0
        IF(IB(I).EQ.'>')TAGZZ=2.0
        I=I+1
        GO TO 1050
!
!               **************************
!               **  STEP 6.3B-          **
!               **  TREAT THE *   CASE  **
!               **************************
!
!       2019/04: LEADING OR TRAILING ASTERISKS CAN OPTIONALLY BE IGNORED
!
      ELSEIF(IB(I).EQ.'*')THEN
        ISTEPN='6.3B'
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')   &
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!       IGNORE LEADING/TRAILING IF REQUESTED
!
!       NOTE THAT ILAST MAY BE THE DELIMITER, SO MAY NEED TO
!       CHECK CHARACTER BEFORE DELIMITER.  NOTE THAT FROM 1 TO
!       3 ASTERISKS MAY BE IGNORED.
!
        IF(IREAAS.EQ.'ON')THEN
          IF(IB(I+1).EQ.'*' .AND. IB(I+2).EQ.'*')THEN
!
!           THREE ASTERISKS
!
            IF(I+2.EQ.ILAST)GO TO 1050
            IF(IB(I+3).EQ.IREADL .AND. I.EQ.ILAST-3)GO TO 1050
          ELSEIF(IB(I+1).EQ.'*')THEN
!
!           TWO ASTERISKS
!
            IF(I+1.EQ.ILAST)GO TO 1050
            IF(IB(I+2).EQ.IREADL .AND. I.EQ.ILAST-2)GO TO 1050
          ELSE
!
!           SINGLE ASTERISK
!
            IF(I.EQ.ILAST)GO TO 1050
            IF(IB(I+1).EQ.IREADL .AND. I.EQ.ILAST-1)GO TO 1050
          ENDIF
        ENDIF
!
!       CHECK IF * IS FIRST CHARACTER OF FIELD
!
        IF(I.EQ.IFRST)THEN
          I=I+1
          IF(I.LE.ICOL22)GO TO 160
          N=N+1
          X(N)=0.0
          IF((IREALT.EQ.'ON'.OR.IREALT.EQ.'MV') .OR. IREAPM.EQ.'ON')   &
              XTAG(N)=TAGZZ
          IF(IREALT.EQ.'MV' .AND. TAGZZ.GE.0.5)X(N)=PREAMV
          GO TO 149
        ENDIF
!
!       NOW CASE WHERE * OCCURS IN MIDDLE OF FIELD.  THIS IS AN
!       ERROR.
!
        IF(NC.NE.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2001)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2032)
 2032     FORMAT('      AN UNEXPECTED  ATERISK ("*") HAS OCCURRED ',   &
                 'IN THE MIDDLE OF A DATA VALUE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2021)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 8100
        ENDIF
!
      ELSEIF(IB(I).EQ.IDECPT)THEN
!
!               *****************************************
!               **  STEP 7--                           **
!               **  TREAT THE DECIMAL POINT CASE--     **
!               *****************************************
!
        ISTEPN='7'
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')   &
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        IF(NC.EQ.0 .AND. NSIGN.EQ.0)THEN
          AFACT=1.0
          NSIGN=NSIGN+1
        ENDIF
        NC=NC+1
        ICHAR3(NC)=IB(I)
        NDP=NDP+1
        I=I+1
        LOCPT=NC
        IF(I.LE.ICOL22)GO TO 160
        IF(NC.GE.2)THEN
          IF(NDP.EQ.1)THEN
            GO TO 1050
          ELSE
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2005)
 2005       FORMAT('***** INPUT     DATA ERROR--',   &
                   'SOME DATA VALUE ON THE CARD BELOW ',   &
                   'HAS MULTIPLE DECIMAL POINTS *****')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2021)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 8100
          ENDIF
        ELSE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          IF(NC.EQ.0)THEN
            WRITE(ICOUT,2007)
 2007       FORMAT('***** INPUT     DATA ERROR--',   &
                   'THE LAST DATA VALUE ON THE CARD BELOW',   &
                   'CONSISTS ONLY OF A DECIMAL POINT')
            CALL DPWRST('XXX','BUG ')
          ELSEIF(NC.EQ.1)THEN
            WRITE(ICOUT,2006)IDECPT,IDECPT
 2006       FORMAT('***** INPUT     DATA ERROR--',   &
                   'THE LAST DATA VALUE ON THE CARD BELOW ',   &
                   'CONSISTS OF ONLY A    +',A1,'  OR   -',A1)
            CALL DPWRST('XXX','BUG ')
          ENDIF
          WRITE(ICOUT,2021)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 8100
        ENDIF
      ELSEIF((IB(I).EQ.'E'.OR.IB(I).EQ.'e'.OR.   &
             IB(I).EQ.'D'.OR.IB(I).EQ.'d') .AND. NC.GT.0)THEN
!
!               ***************************************************
!               **  STEP 10--                                    **
!               **  FORM THE VECTOR ICHEXP(.) WHICH              **
!               **  CONTAINS CHARACTERS OF THE EXPONENT (IF ANY).**
!               ***************************************************
!
!       2018/06: CHECK TO ENSURE THAT COLUMNNS TO RIGHT OF "D" OR "E"
!                CONTAIN NUMERIC VALUES.
!
        ISTEPN='10'
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')   &
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        NCE=0
        I=I+1
        IF(I.GT.ICOL22)GO TO 1050
!
!       CHECK IF FIRST CHARACTER IS "+/-" SIGN.  IF NO SIGN GIVEN,
!       ASSUME EXPONENT IS POSITIVE.
!
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
!
          IF(I.GT.ICOL22)GO TO 1050
!
 4160     CONTINUE
!
          IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
            WRITE(ICOUT,3002)I,IB(I)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,33002)NCE,NUMDEX,ICHEXP(NCE)
33002       FORMAT('NCE,NUMDEX,ICHEXP(NCE) = ',2I5,2X,A4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
!         AFTER  "+/-", CHARACTERS SHOULD BE NUMERIC
!
          IBASCI=ICHAR(IB(I)(1:1)) - IZERO
          IF(IBASCI.GE.0 .AND. IBASCI.LE.9)THEN
            NCE=NCE+1
            NUMDEX=NCE-1
            ICHEXP(NCE)=IB(I)
            I=I+1
            IF(I.LE.ICOL22)GO TO 4160
            GO TO 1050
!CCCC     ELSEIF(IB(I).EQ.'+'.OR.IB(I).EQ.'-'.OR.IB(I).EQ.IDECPT)THEN
          ELSEIF(IB(I).EQ.' ' .OR. IB(I).EQ.IREADL)THEN
            GO TO 1050
          ELSE
!
!           2018/06: NON-NUMERIC CHARACTER FOUND.  IF THIS IS THE
!                    FIRST ROW, DEFINE THIS FIELD AS CHARACTER,
!                    OTHERWISE REPORT AN ERROR.
!
            IF(NCALL.GT.0)THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2051)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2052)IB(I)
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GO TO 8100
            ELSE
              IF(NXC.LT.MAXCHV)THEN
                NXC=NXC+1
                IXC(NXC)=' '
                J=0
                DO 7258 II=IFRST,ILAST
                  IF(IREACD.EQ.'ON' .AND. IB(II).EQ.',')THEN
                    I=II
                    GO TO 7259
!CCCC             ELSEIF(IB(II).EQ.' ' .OR. IB(II).EQ.IREADL)THEN
                  ELSEIF(IB(II).EQ.IREADL)THEN
                    I=II
                    GO TO 7259
                  ENDIF
                  IF(J.LE.24)THEN
                    J=J+1
                    IXC(NXC)(J:J)=IB(II)(1:1)
                  ENDIF
 7258           CONTINUE
 7259           CONTINUE
                ITYPE(NREAD)=1
              ENDIF
              GO TO 149
            ENDIF
            GO TO 1050
          ENDIF
          GO TO 1050
!
        GO TO 1050
      ELSE
!
!CCCCC  2013/04: BUG FIX FOR ICHAR3
!CCCCC  IF(NC.EQ.1.AND.ICHAR3(I).EQ.IDECPT.AND.IGRPAU.EQ.'ERRO')THEN
        IF(NC.EQ.1)THEN
          IF(ICHAR3(NC).EQ.IDECPT.AND.IGRPAU.EQ.'ERRO')THEN
!
!               ****************************************************
!               **  STEP 8.3--                                    **
!               **  TREAT THE SPECIAL CASE WHEN THE SECOND OR MORE**
!               **  CHARACTER  IS ALPHABETIC, BLANK, ETC.         **
!               ****************************************************
!
            ISTEPN='8.3'
            IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')   &
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2009)IDECPT,IDECPT
 2009       FORMAT('***** INPUT    DATA ERROR--',   &
                   'SOME DATA VALUE ON THE CARD BELOW ',   &
                 'CONSISTS OF ONLY A   +',A1,' OR -',A1)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2021)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 8100
          ENDIF
        ENDIF
        IF(NC.GE.1)THEN
          IF(IB(I).EQ.' ' .OR. IB(I).EQ.IREAD2)GO TO 1050
        ENDIF
        IQFLAG=0
        ISFLAG=0
 6160   CONTINUE
!
        IF(IB(I).EQ.' ')THEN
          IDELIM=1
        ELSEIF(IB(I).EQ.IREAD2)THEN
          IDELIM=1
          IF(NC.EQ.0)THEN
            IF(NCALL.EQ.0)THEN
              N=N+1
              X(N)=PREAMV
              ITYPE(NREAD)=0
              IF((IREALT.EQ.'ON'.OR.IREALT.EQ.'MV') .OR. IREAPM.EQ.'ON')   &
                  XTAG(N)=TAGZZ
            ELSE
              IF(ITYP.EQ.0)THEN
                N=N+1
                X(N)=PREAMV
                IF((IREALT.EQ.'ON'.OR.IREALT.EQ.'MV') .OR. IREAPM.EQ.'ON')   &
                    XTAG(N)=TAGZZ
              ELSE
                IF(NXC.LT.MAXCHV)THEN
                  NXC=NXC+1
                  IXC(NXC)=' '
                ENDIF
              ENDIF
            ENDIF
            I=I+1
            IF(I.LE.ICOL22)GO TO 149
            GO TO 1040
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
!
        IF(NC.EQ.0 .AND. ISFLAG.EQ.0)THEN
!
!            *******************************************************
!            **  STEP 8.1--                                       **
!            **  TREAT THE SPECIAL CASE WHEN THE LEADING CHARACTER**
!            **  IS ALPHABETIC, BLANK, ETC.                       **
!            *******************************************************
!
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
            IF(IREALT.EQ.'ON' .OR. IREAPM.EQ.'ON')XTAG(N)=TAGZZ
            IF(IREALT.EQ.'MV' .AND. TAGZZ.GE.1.0)X(N)=PREAMV
            GO TO 149
          ENDIF
!
!  FOR ALL CASES, TREAT DELIMITER
!
          IF(IGRPAU.EQ.'ERRO')THEN
            I=I+1
            IF(I.LE.ICOL22)GO TO 160
            GO TO 9000
          ELSEIF(IGRPAU.EQ.'IGNO' .OR. IGRPAU.EQ.'CHAR' .OR.   &
                 IGRPAU.EQ.'CATE')THEN
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
              IF(I.LE.ICOL22)GO TO 6160
              GO TO 1040
            ELSEIF(IDELIM.EQ.1)THEN
              I=I+1
              IF(I.LE.ICOL22)GO TO 150
              IF(NCOLS.GT.0)THEN
                N=N+1
                X(N)=PREAMV
                IF(NCALL.EQ.0)ITYPE(NREAD)=0
                IF((IREALT.EQ.'ON'.OR.IREALT.EQ.'MV') .OR. IREAPM.EQ.'ON')   &
                    XTAG(N)=TAGZZ
                GO TO 149
              ELSE
                GO TO 9000
              ENDIF
            ELSEIF(IDELIM.EQ.2)THEN
              ISFLAG=1
              IF(NXC.LT.MAXCHV)THEN
                NXC=NXC+1
                IXC(NXC)=' '
              ENDIF
              IQFLAG=1
              IF(I.LE.ICOL22)GO TO 6160
              GO TO 1040
            ELSEIF(IDELIM.EQ.3)THEN
              ISFLAG=1
              IF(NXC.LT.MAXCHV)THEN
                NXC=NXC+1
                IXC(NXC)=' '
              ENDIF
              IQFLAG=2
              IF(I.LE.ICOL22)GO TO 6160
              GO TO 1040
            ENDIF
          ENDIF
        ELSEIF(NC.GE.1 .OR. ISFLAG.EQ.1)THEN
          IF(IGRPAU.EQ.'ERRO')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,6009)
 6009       FORMAT('***** INPUT    DATA ERROR--',   &
                   'SOME DATA VALUE ON THE CARD BELOW ',   &
                   'CONSISTS OF A NON-NUMERIC CHARACTER.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2021)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 8100
          ELSEIF(IGRPAU.EQ.'IGNO' .OR. IGRPAU.EQ.'CHAR' .OR.   &
                 IGRPAU.EQ.'CATE')THEN
            IF(IQFLAG.EQ.1)THEN
              IF(IDELIM.EQ.2)THEN
                IQFLAG=0
                ISFLAG=0
                I=I+1
                IF(I.LE.ICOL22)GO TO 149
                GO TO 1040
              ELSE
                IF((IGRPAU.EQ.'CHAR'.OR.IGRPAU.EQ.'CATE') .AND.   &
                   NXC.LE.MAXCHV)THEN
                  IF(NSTR.LE.24)THEN
                    NSTR=NSTR+1
                    IXC(NXC)(NSTR:NSTR)=IB(I)(1:1)
                  ENDIF
                ENDIF
                I=I+1
                IF(I.LE.ICOL22)GO TO 6160
                GO TO 1040
              ENDIF
            ELSEIF(IQFLAG.EQ.2)THEN
              IF(IDELIM.EQ.3)THEN
                IQFLAG=0
                I=I+1
                IF(I.LE.ICOL22)GO TO 149
                GO TO 1040
              ELSE
                IF((IGRPAU.EQ.'CHAR'.OR.IGRPAU.EQ.'CATE') .AND.   &
                   NXC.LE.MAXCHV)THEN
                  IF(NSTR.LE.24)THEN
                    NSTR=NSTR+1
                    IXC(NXC)(NSTR:NSTR)=IB(I)(1:1)
                  ENDIF
                ENDIF
                I=I+1
                IF(I.LE.ICOL22)GO TO 6160
                GO TO 1040
              ENDIF
            ENDIF
            IF(IDELIM.EQ.0)THEN
              IF((IGRPAU.EQ.'CHAR'.OR.IGRPAU.EQ.'CATE') .AND.   &
                 NXC.LE.MAXCHV)THEN
                IF(NSTR.LE.24)THEN
                  NSTR=NSTR+1
                  IXC(NXC)(NSTR:NSTR)=IB(I)(1:1)
                ENDIF
              ENDIF
              NC=NC+1
              I=I+1
              IF(I.LE.ICOL22)GO TO 6160
              GO TO 1040
            ELSEIF(IDELIM.EQ.1)THEN
              IF(ISFLAG.EQ.1)ISFLAG=0
              I=I+1
              IF(I.LE.ICOL22)GO TO 149
              GO TO 1040
            ELSEIF(IDELIM.EQ.2)THEN
              NC=0
              IQFLAG=1
              ISFLAG=1
              IF(I.LE.ICOL22)GO TO 6160
              GO TO 1040
            ELSEIF(IDELIM.EQ.3)THEN
              NC=0
              IQFLAG=2
              ISFLAG=1
              IF(I.LE.ICOL22)GO TO 6160
              GO TO 1040
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      GO TO 1050
!
!               **************************************
!               **  FOR VECTOR COLUMN LIMITS, GO TO **
!               **  NEXT FIELD.                     **
!               **************************************
!
 1040 CONTINUE
      IF(NCOLS.GT.0)GO TO 150
      GO TO 9000
!
!               **************************************
!               **  STEP 12--                       **
!               **  OPERATE ON THE ICHAR3(.) VECTOR **
!               **************************************
!
 1050 CONTINUE
!
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
!
!               ************************************************
!               **  STEP 12.1--                               **
!               **  LOCATE THE POSITION OF THE DECIMAL POINT  **
!               ************************************************
!
      IF(NCALL.EQ.0)ITYPE(NREAD)=0
      IF(NC.EQ.0)THEN
        IF(NSIGN.EQ.1)THEN
          N=N+1
          X(N)=AFACT
          IF((IREALT.EQ.'ON'.OR.IREALT.EQ.'MV') .OR. IREAPM.EQ.'ON')   &
              XTAG(N)=TAGZZ
          IF(IREALT.EQ.'MV' .AND. TAGZZ.GE.0.5)X(N)=PREAMV
        ELSEIF(NCOLS.GT.0)THEN
          N=N+1
          X(N)=PREAMV
          IF((IREALT.EQ.'ON'.OR.IREALT.EQ.'MV') .OR. IREAPM.EQ.'ON')   &
              XTAG(N)=TAGZZ
        ENDIF
        GO TO 149
      ENDIF
!
      IF(NDP.EQ.0)THEN
        NC=NC+1
        ICHAR3(NC)=IDECPT
        LOCPT=NC
      ENDIF
!
!               ********************************
!               **  STEP 12.2--               **
!               **  COMPUTE THE INTEGER PART  **
!               ********************************
!
!
      ISTEPN='12.2'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      SUMINT=0.0
      NUMINT=LOCPT-1
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
        WRITE(ICOUT,1201)NC,LOCPT,NUMINT
 1201   FORMAT('NC,LOCPT,NUMINT = ',3I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IF(NUMINT.GT.0)THEN
        IPOWER=-1
        DO 1200 J=LOCPT-1,1,-1
          IBASCI=ICHAR(ICHAR3(J)(1:1)) - IZERO
          IF(IBASCI.GE.0 .AND. IBASCI.LE.9)THEN
            IPOWER=IPOWER+1
            SUMINT=SUMINT+REAL(IBASCI)*(10.0**IPOWER)
            GO TO 1200
          ENDIF
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2024)
 2024     FORMAT('***** INTERNAL ERROR IN DPREAL--A ',   &
                 'NON-NUMERIC CHARACTER WAS ENCOUNTERED IN ',   &
                 'CONVERTING THE INTEGER PART')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2025)(ICHAR3(L),L=1,41)
 2025     FORMAT('     OF THE FOLLOWING DATA VALUE--',41A1)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 8100
!
 1200   CONTINUE
      ENDIF
!
!               ***********************************
!               **  STEP 12.2--                  **
!               **  COMPUTE THE FRACTIONAL PART  **
!               ***********************************
!
      SUMDEC=0.0
      NUMDEC=NC-LOCPT
!
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
        ISTEPN='12.2'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,1601)NC,LOCPT,NUMDEC
 1601   FORMAT('NC,LOCPT,NUMDEC = ',3I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IF(NUMDEC.NE.0)THEN
        IPOWER=0
        DO 1500 J=LOCPT+1,NC
          IBASCI=ICHAR(ICHAR3(J)(1:1)) - IZERO
          IF(IBASCI.GE.0 .AND. IBASCI.LE.9)THEN
            IPOWER=IPOWER+1
            SUMDEC=SUMDEC+REAL(IBASCI)/(10.0**IPOWER)
            GO TO 1500
          ENDIF
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2026)
 2026     FORMAT('***** INTERNAL ERROR IN DPREAL--A NON-NUMERIC ',   &
                 'CHARACTER WAS ENCOUNTERED IN CONVERTING ',   &
                 'THE DECIMAL PART')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2025)(ICHAR3(L),L=1,41)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 8100
 1500   CONTINUE
      ENDIF
!
!               ***************************************************
!               **  STEP 12.3--                                  **
!               **  IF EXPONENTIAL FORMAT, COMPUTE THE           **
!               **  EXPONENTIAL PART.                            **
!               ***************************************************
!
      ISTEPN='12.3'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,1851)NCE,NUMDEX,ICHEXP(1),ICHEXP(2),ICHEXP(3)
 1851   FORMAT('NCE,NUMDEX,ICHEXP(1),ICHEXP(2),ICHEXP(3) = ',2I8,   &
               2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      ISUMEX=0
      IF(NUMDEX.NE.0)THEN
        ISTART=2
        ISTOP=NUMDEX+1
        IPOWER=-1
        DO 1860 J=ISTART,ISTOP
          JREV=ISTOP-J+2
          IBASCI=ICHAR(ICHEXP(JREV)(1:1)) - IZERO
          IF(IBASCI.GE.0 .AND. IBASCI.LE.9)THEN
            IPOWER=IPOWER+1
            ISUMEX=ISUMEX+IBASCI*(INT(10.0**IPOWER + 0.01))
              GO TO 1860
            ENDIF
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2027)
 2027     FORMAT('***** INTERNAL ERROR IN DPREAL--A NON-NUMERIC',   &
                 'CHARACTER WAS ENCOUNTERED IN CONVERTING ',   &
                 'THE EXPONENTIAL PART')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2028)(ICHEXP(L),L=1,41)
 2028     FORMAT('      THE EXPONENT WAS AS FOLLOWS--',41A1)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 8100
 1860   CONTINUE
        IF(ICHEXP(1).EQ.'-')ISUMEX=-ISUMEX
      ENDIF
!
!               ****************************************************
!               **  STEP 12.4--                                   **
!               **  FINAL STEPS:                                  **
!               **  1) COMBINE THE INTEGER, DECIMAL, AND          **
!               **     EXPONENTIAL PARTS                          **
!               **  2) DETERMINE THE SIGN FOR THE ENTIRE NUMBER   **
!               **  3) PLACE THE COMPUTED NUMBER                  **
!               **     IN THE PROPER ELEMENT OF X(.)  **
!               ****************************************************
!
      SUM=AFACT*(SUMINT+SUMDEC)*(10.0**ISUMEX)
      N=N+1
      X(N)=SUM
      IF((IREALT.EQ.'ON'.OR.IREALT.EQ.'MV') .OR. IREAPM.EQ.'ON')   &
          XTAG(N)=TAGZZ
      IF(IREALT.EQ.'MV' .AND. TAGZZ.GE.0.5)X(N)=PREAMV
!
      ISTEPN='12.4'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,3003)SUMINT,SUMDEC,SUM,ISUMEX,NUMDEX
 3003   FORMAT('SUMINT,SUMDEC,SUM,ISUMEX,NUMDEX = ',3E15.7,2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3004)N,X(N),XTAG(N)
 3004   FORMAT('N,X(N),XTAG(N) = ',I8,2G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *********************************************
!               **  STEP 15--                              **
!               **  INCREMENT THE COLUMN AND DETERMINE IF  **
!               **  THE READ OF THE LINE IS FINISHED       **
!               *********************************************
!
      ISTEPN='15'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3011)I,ICOL22,N,NXC,X(N)
 3011   FORMAT('I,ICOL22,N,NXC,X(N) = ',4I8,G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      I=I+1
      IF(I.LE.ICOL22 .OR. NCOLS.GT.0)GO TO 149
      GO TO 9000
!               **********************************
!               **  STEP 18--                   **
!               **  TREAT THE END OF FILE CASE  **
!               **********************************
!
 8000 CONTINUE
!
      ISTEPN='18'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IEND='YES'
      GO TO 9000
!               **********************************
!               **  STEP 19                     **
!               **  ERROR READING FILE          **
!               **********************************
!
 8100 CONTINUE
!
      ISTEPN='19'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REAL')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      NUMLR2=NUMLRD+NUMCRD
      WRITE(ICOUT,8122)NUMLR2
 8122 FORMAT('      THIS CARD IMAGE WAS THE ',I8,' TH DATA CARD ',   &
             'IMAGE THAT WAS READ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8123)(IB(J),J=1,80)
 8123 FORMAT('      THE CARD IMAGE IS AS FOLLOWS--',80A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
!
!     IF LAST NON-BLANK CHARACTER IS A DELIMITER, THEN NEED TO ADD A
!     MISSING VALUE FIELD.  ONLY DO THIS IF DELIMITER IS NOT THE SPACE
!     CHARACTER (I.E., TRAILING SPACES ARE NOT INDICATIVE OF A MISSING
!     VALUE).  WE ARE TRYING TO CAPTURE CASE WHERE THE LINE ENDS WITH
!     A NON-BLANK DELIMITER, SUCH AS A COMMA, WHICH INDICATES THERE
!     SHOULD BE A "MISSING" FIELD.
!
      IF(IFLAGD.EQ.1 .AND. IB(NLASTZ).EQ.IREADL .AND.   &
         IREADL.NE.' ')THEN
        N=N+1
        X(N)=PREAMV
        IF(IREALT.EQ.'ON' .OR. IREAPM.EQ.'ON')XTAG(N)=TAGZZ
      ENDIF
      ICOL2=ICOL2S
!
      IF(IREAPM.EQ.'ON' .OR. IREALT.EQ.'ON')THEN
        IFORMT='(    F5.0)'
        WRITE(IFORMT(2:5),'(I4)')N
        IF(N.GT.0)WRITE(IOUNI5,IFORMT)(XTAG(I),I=1,N)
      ENDIF
      IDATDL=IDATD2
      ITIMDL=ITIMD2
      IREADL=IREAD3
!
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
          DO 9119 I=1,NXC
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
!
      RETURN
      END SUBROUTINE DPREAL
      SUBROUTINE DPREBA(ADERBA,MAXREG,AREGBA,IREBIN,IREBPL,   &
                        IBUGP2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE REGION BASES.
!              THESE ARE LOCATED IN THE VECTOR AREGBA(.).
!     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
!                     --IARGT  (A  CHARACTER VECTOR)
!                     --ARG
!                     --NUMARG
!                     --ADERBA
!                     --MAXREG
!                     --IBUGP2 ('ON' OR 'OFF' )
!     OUTPUT ARGUMENTS--AREGBA (A FLOATING POINT VECTOR)
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
!           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
!           MODIFIED, OR OTHERWISE USED IN A CONTEXT
!           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--DECEMBER  1983.
!     UPDATED         --OCTOBER   1993.  ADD REGION BASE AUTOMATIC Y
!     UPDATED         --OCTOBER   1993.  ADD REGION BASE INTERPOLATE
!                                               <ON/OFF>
!     UPDATED         --MARCH     1994.  ADD REGION BASE POLYGON
!                                               <ON/OFF>
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!CCCC OCTOBER 1993.  COMMENT OUT FOLLOWING 2 LINES
!CCCC CHARACTER*4 IHARG
!CCCC CHARACTER*4 IARGT
!
      CHARACTER*4 IBUGP2
      CHARACTER*4 IFOUND
!CCCC ADD FOLLOWING LINE NOVEMBER 1994.
      CHARACTER*4 IBUGQ
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHOLD1
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!CCCC OCTOBER 1993.  ADD FOLLOWING SECTION.
      CHARACTER*4 IREBIN
      CHARACTER*4 IHLEFT
      CHARACTER*4 IHLEF2
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 ICASEQ
      CHARACTER*4 IWRITE
!CCCC MARCH 1994.  ADD FOLLOWING LINE.
      CHARACTER*4 IREBPL
!
!CCCC OCTOBER 1993.  COMMENT OUT FOLLOWING 3 LINES
!CCCC DIMENSION IHARG(*)
!CCCC DIMENSION IARGT(*)
!CCCC DIMENSION ARG(*)
      DIMENSION AREGBA(*)
!
!CCCC OCTOBER 1993.  ADD FOLLOWING COMMON BLOCKS
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPRE'
      ISUBN2='BA  '
!
      NUMREG=0
      IHOLD1='-999'
      HOLD1=-999.0
      HOLD2=-999.0
!
      IF(IBUGP2.EQ.'OFF')GO TO 90
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
      DO 65 I=1,NUMARG
      WRITE(ICOUT,66)IHARG(I),IARGT(I),ARG(I)
   66 FORMAT('IHARG(I),IARGT(I),ARG(I) = ',A4,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,70)AREGBA(1)
   70 FORMAT('AREGBA(1) = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      DO 75 I=1,10
      WRITE(ICOUT,76)I,AREGBA(I)
   76 FORMAT('I,AREGBA(I) = ',I8,2X,E15.7)
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
!CCCC OCTOBER 1993.  ADD REGION BASE AUTOMATIC <VAR>
      IF(NUMARG.GE.3.AND.IHARG(2).EQ.'AUTO')GO TO 3000
      IF(NUMARG.GE.4.AND.IHARG(3).EQ.'AUTO')GO TO 3000
!CCCC OCTOBER 1993.  ADD REGION BASE INTERPOLATE <ON/OFF>
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'INTE')GO TO 4000
!CCCC MARCH 1994.  ADD REGION BASE POLYGON <ON/OFF>
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'POLY')GO TO 5000
!
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
      IF(IHARG(2).EQ.'ALL')HOLD1=ADERBA
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
      NUMREG=1
      AREGBA(1)=ADERBA
      GO TO 1270
!
 1220 CONTINUE
      NUMREG=NUMARG-1
      IF(NUMREG.GT.MAXREG)NUMREG=MAXREG
      DO 1225 I=1,NUMREG
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
      GO TO 1270
!
 1270 CONTINUE
      IF(IFEEDB.EQ.'OFF')GO TO 1279
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO 1278 I=1,NUMREG
      WRITE(ICOUT,1276)I,AREGBA(I)
 1276 FORMAT('THE BASE OF REGION ',I6,   &
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
      NUMREG=MAXREG
      HOLD2=HOLD1
      IF(IHOLD1.EQ.'ON')HOLD2=ADERBA
      IF(IHOLD1.EQ.'OFF')HOLD2=ADERBA
      IF(IHOLD1.EQ.'AUTO')HOLD2=ADERBA
      IF(IHOLD1.EQ.'DEFA')HOLD2=ADERBA
      DO 1315 I=1,NUMREG
      AREGBA(I)=HOLD2
 1315 CONTINUE
      GO TO 1370
!
 1370 CONTINUE
      IF(IFEEDB.EQ.'OFF')GO TO 1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1316)AREGBA(I)
 1316 FORMAT('THE BASE OF ALL REGIONS',   &
      ' HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1319 CONTINUE
      IFOUND='YES'
      GO TO 9000
!
!               ******************************************************
!               **  STEP 30--                                       **
!               **  TREAT THE REGION BASEAUTOMATIC <VARIABLE>   CASE**
!               ******************************************************
!
 3000 CONTINUE
!
!               ********************************************
!               **  STEP 31--                             **
!               **  CHECK THE VALIDITY OF ARGUMENT 2 (OR 3)**
!               **  (THIS WILL BE THE RESPONSE VARIABLE)  **
!               ********************************************
!
      ISTEPN='31'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IHLEFT=IHARG(3)
      IHLEF2=IHARG2(3)
      IF(IHARG(3).EQ.'DIST'.AND.IHARG2(3).EQ.'INCT')IHLEFT=IHARG(4)
      IF(IHARG(3).EQ.'DIST'.AND.IHARG2(3).EQ.'INCT')IHLEF2=IHARG2(4)
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
 3351 FORMAT('***** ERROR IN DPREBA--')
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
!               **  IF HAVE THE FORM--                 **
!               **  REGION BASE AUTOMATIC DISTINCT X   **
!               **  EXTRACT THE DISTINCT VALUES        **
!               **  FROM THE TARGET VARIABLE Y(.)   .  **
!               **  STORE THEM IN X(.)   .             **
!               **  IF HAVE THE FORM--                 **
!               **  CHARACTERS AUTOMATIC X             **
!               **  DO NOTHING                         **
!               *****************************************
!
      IF(IHARG(3).EQ.'DIST'.AND.IHARG2(3).EQ.'INCT')GO TO 3420
!
      DO 3411 I=1,NY
        X(I)=Y(I)
 3411 CONTINUE
      NX=NY
      GO TO 3490
!
 3420 CONTINUE
      IWRITE='OFF'
      CALL DISTIN(Y,NY,IWRITE,X,NX,IBUGP2,IERROR)
      GO TO 3490
!
 3490 CONTINUE
!
!               ******************************************
!               **  STEP 36--                           **
!               **  COPY VALUES IN X(.) TO ABARBA       **
!               **        MAX NUMBER OF BARS    = 100   **
!               ******************************************
!
      IMAX=NX
      IF(IMAX.GT.MAXREG)IMAX=MAXREG
      DO 3650 I=1,IMAX
      AREGBA(I)=X(I)
 3650 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 3679
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO 3675 I=1,IMAX
      WRITE(ICOUT,3676)I,AREGBA(I)
 3676 FORMAT('REGION BASE ',I6,' HAS JUST BEEN SET TO ',   &
      E15.7)
      CALL DPWRST('XXX','BUG ')
 3675 CONTINUE
 3679 CONTINUE
      IFOUND='YES'
      GO TO 9000
!
!               ******************************************
!               **  STEP 40--                           **
!               **  REGION BASE INTERPOLATE <ON/OFF>    **
!               ******************************************
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
!
      IF(IFEEDB.EQ.'OFF')GO TO 4099
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4010)IREBIN
 4010 FORMAT('REGION BASE INTERPOLATE HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 4099 CONTINUE
      IFOUND='YES'
      GO TO 9000
!
!               ******************************************
!               **  STEP 50--                           **
!               **  REGION BASE POLYGON     <ON/OFF>    **
!               ******************************************
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
!
      IF(IFEEDB.EQ.'OFF')GO TO 5099
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5010)IREBPL
 5010 FORMAT('REGION BASE POLYGON HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 5099 CONTINUE
      IFOUND='YES'
      GO TO 9000
!
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGP2.EQ.'OFF')GO TO 9090
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
      DO 9025 I=1,NUMARG
      WRITE(ICOUT,9026)IHARG(I),IARGT(I),ARG(I)
 9026 FORMAT('IHARG(I),IARGT(I),ARG(I) = ',A4,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      WRITE(ICOUT,9030)AREGBA(1)
 9030 FORMAT('AREGBA(1) = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      DO 9035 I=1,10
      WRITE(ICOUT,9036)I,AREGBA(I)
 9036 FORMAT('I,AREGBA(I) = ',I8,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPREBA
      SUBROUTINE DPRECF(ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,   &
                        IA,PARAM,IPARN,IPARN2,   &
                        IANGLU,   &
                        IBUGA3,IBUGCO,IBUGEV,ISUBRO,IERROR)
!
!     PURPOSE--TREAT THE LET CASE FOR A RECURSIVE FUNCTION.
!     EXAMPLE--LET X(1) = 1
!              LET Y = RECURSIVE FUNCTION 2*X  FOR X = 2 100
!
!              THIS IS EQUIVALENT TO
!
!              LET X(1) = 1
!              LOOP FOR K = 2 1 100
!                  LET KM1 = K-1
!                  LET ATEMP = X(KM1)
!                  LET AVAL = 2*ATEMP
!              END OF LOOP
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
!     VERSION NUMBER--2011/3
!     ORIGINAL VERSION--MARCH     2011.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
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
!
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
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      DIMENSION ITYPEH(*)
      DIMENSION IW21HO(*)
      DIMENSION IW22HO(*)
      DIMENSION W2HOLD(*)
!
      DIMENSION IA(*)
      DIMENSION PARAM(*)
      DIMENSION IPARN(*)
      DIMENSION IPARN2(*)
!
      DIMENSION IDUMV(100)
      DIMENSION IDUMV2(100)
      DIMENSION ROOTS2(100)
!
      DIMENSION ILAB(10)
      DIMENSION IOLD(10)
      DIMENSION IOLD2(10)
      DIMENSION INEW(10)
      DIMENSION INEW2(10)
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
      ISUBN1='DPRE'
      ISUBN2='CF  '
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
      ILOCMX=0
      NUMLIM=0
      ILOC3=0
      NCHANF=0
!
!               ********************************************
!               **  TREAT THE RECURSIVE FUNCTION SUBCASE  **
!               **  OF THE LET COMMAND                    **
!               ********************************************
!
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
!
!               **********************************
!               **  STEP 1--                    **
!               **  INITIALIZE SOME VARIABLES.  **
!               **********************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RECF')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NEWNAM='NO'
!
      MAXN2=MAXCHF
      MAXN3=MAXCHF
!
!               *********************************************************
!               **  STEP 2--                                            *
!               **  EXAMINE THE LEFT-HAND SIDE--                        *
!               **  IS THE VARIABLE NAME TO LEFT OF = SIGN              *
!               **  ALREADY IN THE NAME LIST?                           *
!               **  NOTE THAT     ILISTL    IS THE LINE IN THE TABLE    *
!               **  OF THE NAME ON THE LEFT.                            *
!               *********************************************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RECF')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IHLEFT=IHARG(1)
      IHLEF2=IHARG2(1)
      DO 2000 I=1,NUMNAM
        I2=I
        IF(IHLEFT.EQ.IHNAME(I).AND.IHLEF2.EQ.IHNAM2(I))THEN
          ILISTL=I2
          GO TO 2900
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
        GO TO 9000
      ENDIF
!
 2900 CONTINUE
!               ***************************************************************
!               **  STEP 3.1--                                               **
!               **  EXTRACT THE RIGHT-SIDE FUNCTIONAL
!               **  EXPRESSION FROM THE INPUT COMMAND LINE                   **
!               **  (STARTING WITH THE FIRST NON-BLANK LOCATION AFTER THE    **
!               **  EQUAL SIGN AND ENDING WITH THE END OF THE LINE           **
!               **  OR WITH THE LAST NON-BLANK CHARACTER BEFORE     WRT  .   **
!               **  PLACE THE FUNCTION IN IFUNC2(.)  .                       **
!               ***************************************************************
!
      ISTEPN='3.1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RECF')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IWD1=IHARG(3)
      IWD12=IHARG2(3)
      IWD2='WRT '
      IWD22='    '
      CALL DPEXST(IANS,IWIDTH,IWD1,IWD12,IWD2,IWD22,MAXN2,   &
                  IFUNC2,N2,IBUGA3,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      IF(IFOUND.EQ.'YES')GO TO 3500
!
      IWD1=IHARG(3)
      IWD12=IHARG2(3)
      IWD2='FOR '
      IWD22='    '
      CALL DPEXST(IANS,IWIDTH,IWD1,IWD12,IWD2,IWD22,MAXN2,   &
                  IFUNC2,N2,IBUGA3,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      IF(IFOUND.EQ.'YES')GO TO 3500
!
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
 3104 FORMAT('      LET ... = RECURSIVE FUNCTION  ... WRT  ... ',   &
      'FOR ...  =  ...   ...')
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
      GO TO 9000
!
 3500 CONTINUE
!
!               *****************************************************
!               **  STEP 3.2--                                     **
!               **  DETERMINE IF THE RIGHT-HAND SIDE IS            **
!               **  IN FUNCTION FORM OR IS IN EQUATION FORM.       **
!               **  IF IN EQUATION FORM, CONVERT TO FUNCTION FORM  **
!               **  BY REPLACING THE EQUAL SIGN BY A MINUS SIGN    **
!               **  AND ENCLOSING THE REST OF THE EXPRESSION IN    **
!               **  PARENTHESES.                                   **
!               **  PLACE THE OUTPUT FUNCTION BACK IN IFUNC2(.)    **
!               *****************************************************
!
      ISTEPN='3.2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RECF')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 3600 I=1,N2
        I2=I
        IF(IFUNC2(I).EQ.'=')GO TO 3610
 3600 CONTINUE
      GO TO 3900
!
 3610 CONTINUE
      ILOCE2=I2
!
      IMIN=ILOCE2+1
      IF(IMIN.GT.N2)GO TO 3690
      DO 3650 I=IMIN,N2
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
!
 3900 CONTINUE
!
!
!               ***********************************************************
!               **  STEP 4--                                             **
!               **  DETERMINE IF THE EXPRESSION HAS ANY FUNCTION NAMES   **
!               **  INBEDDED.  IF SO, REPLACE THE FUNCTION NAMES         **
!               **  BY EACH FUNCTION'S DEFINITION.  DO SO REPEATEDLY     **
!               **  UNTIL ALL FUNCTION REFERENCES HAVE BEEN ANNIHILATED  **
!               **  AND THE EXPRESSION IS LEFT ONLY WITH                 **
!               **  CONSTANTS, PARAMETERS, AND VARIABLES--NO FUNCTIONS.  **
!               **  PLACE THE RESULTING FUNCTIONAL EXPRESSION INTO IFUNC3(.) **
!               ***********************************************************
!
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RECF')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPEXFU(IFUNC2,N2,IHNAME,IHNAM2,IUSE,IVSTAR,IVSTOP,   &
                  NUMNAM,IANS,IWIDTH,IFUNC,NUMCHF,MAXCHF,IFUNC3,   &
                  N3,MAXN3,   &
                  IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
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
!
        WRITE(ICOUT,5081)IDUMV(1),IDUMV2(1)
 5081   FORMAT('RECURSIVE VARIABLE         = ',A4,A4)
        CALL DPWRST('XXX','BUG ')
!
      ENDIF
!
!               *************************************
!               **  STEP 5--                       **
!               **  EXTRACT QUALIFIER INFORMATION. **
!               *************************************
!
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RECF')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!               **************************************************
!               **  STEP 5.1--                                  **
!               **  DETERMINE THE DUMMY VARIABLE FOR THE ROOT.  **
!               **************************************************
!
      ISTEPN='5.1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RECF')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IKEY='WRT '
      IKEY2='    '
      ISHIFT=1
      ILOCA=1
      ILOCB=NUMARG
      INCLUN='NO'
      CALL DPEXQU(IKEY,IKEY2,ISHIFT,ILOCA,ILOCB,   &
                  IHARG,IHARG2,NUMARG,   &
                  INCLUN,IANS,IWIDTH,IHNAME,IHNAM2,IVALUE,VALUE,   &
                  IUSE,IN,NUMNAM,   &
                  IFOUN1,IFOUN2,ILOC1,ILOC2,IHOUT,IHOUT2,ILOUT,   &
                  IVOUT,VOUT,IUOUT,   &
                  INOUT,IBUGA3,IERROR)
      IF(IFOUN1.EQ.'NO'.OR.IFOUN2.EQ.'NO')GO TO 5119
        IDUMV(1)=IHOUT
        IDUMV2(1)=IHOUT2
        NUMDV=1
        GO TO 5190
 5119 CONTINUE
!
      IKEY='FOR '
      IKEY2='    '
      ISHIFT=1
      ILOCA=1
      ILOCB=NUMARG
      INCLUN='NO'
      CALL DPEXQU(IKEY,IKEY2,ISHIFT,ILOCA,ILOCB,   &
                  IHARG,IHARG2,NUMARG,   &
                  INCLUN,IANS,IWIDTH,IHNAME,IHNAM2,IVALUE,VALUE,   &
                  IUSE,IN,NUMNAM,   &
                  IFOUN1,IFOUN2,ILOC1,ILOC2,IHOUT,IHOUT2,ILOUT,   &
                  IVOUT,VOUT,IUOUT,   &
                  INOUT,IBUGA3,IERROR)
      IF(IFOUN1.EQ.'NO'.OR.IFOUN2.EQ.'NO')GO TO 5129
        IDUMV(1)=IHOUT
        IDUMV2(1)=IHOUT2
        NUMDV=1
        GO TO 5190
 5129 CONTINUE
!
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
      GO TO 9000
 5190 CONTINUE
!
!               **************************************************
!               **  STEP 5.2--                                  **
!               **  DETERMINE THE LIMITS FOR   THE RECURSION.   **
!               **************************************************
!
      ISTEPN='5.2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RECF')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMLIM=0
!
      IKEY='FOR '
      IKEY2='    '
      ISHIFT=3
      ILOCA=1
      ILOCB=NUMARG
      INCLUN='NO'
      CALL DPEXQU(IKEY,IKEY2,ISHIFT,ILOCA,ILOCB,   &
                  IHARG,IHARG2,NUMARG,   &
                  INCLUN,IANS,IWIDTH,IHNAME,IHNAM2,IVALUE,   &
                  VALUE,IUSE,IN,NUMNAM,   &
                  IFOUN1,IFOUN2,ILOC1,ILOC2,IHOUT,IHOUT2,ILOUT,   &
                  IVOUT,VOUT,IUOUT,   &
                  INOUT,IBUGA3,IERROR)
      IF(IFOUN1.EQ.'NO'.OR.IFOUN2.EQ.'NO')GO TO 5219
        XMIN=VOUT
        NUMLIM=NUMLIM+1
 5219 CONTINUE
!
      IKEY='FOR '
      IKEY2='    '
      ISHIFT=4
      ILOCA=1
      ILOCB=NUMARG
      INCLUN='NO'
      CALL DPEXQU(IKEY,IKEY2,ISHIFT,ILOCA,ILOCB,   &
                  IHARG,IHARG2,NUMARG,   &
                  INCLUN,IANS,IWIDTH,IHNAME,IHNAM2,IVALUE,   &
                  VALUE,IUSE,IN,NUMNAM,   &
                  IFOUN1,IFOUN2,ILOC1,ILOC2,IHOUT,IHOUT2,ILOUT,   &
                  IVOUT,VOUT,IUOUT,   &
                  INOUT,IBUGA3,IERROR)
      IF(IFOUN1.EQ.'NO'.OR.IFOUN2.EQ.'NO')GO TO 5239
      IF(IHOUT.EQ.'TO  '.AND.IHOUT2.EQ.'    ')GO TO 5229
        XMAX=VOUT
        ILOCMX=ILOC2
        NUMLIM=NUMLIM+1
 5229 CONTINUE
!
      IF(NUMLIM.EQ.2)GO TO 5239
      IKEY='FOR '
      IKEY2='    '
      ISHIFT=5
      ILOCA=1
      ILOCB=NUMARG
      INCLUN='NO'
      CALL DPEXQU(IKEY,IKEY2,ISHIFT,ILOCA,ILOCB,   &
                  IHARG,IHARG2,NUMARG,   &
                  INCLUN,IANS,IWIDTH,IHNAME,IHNAM2,IVALUE,VALUE,   &
                  IUSE,IN,NUMNAM,   &
                  IFOUN1,IFOUN2,ILOC1,ILOC2,IHOUT,IHOUT2,ILOUT,   &
                  IVOUT,VOUT,IUOUT,   &
                  INOUT,IBUGA3,IERROR)
      IF(IFOUN1.EQ.'NO'.OR.IFOUN2.EQ.'NO')GO TO 5239
        XMAX=VOUT
        ILOCMX=ILOC2
        NUMLIM=NUMLIM+1
 5239 CONTINUE
!
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
        GO TO 9000
      ENDIF
!
!               **********************************************
!               **  STEP 6.3--                              **
!               **  SCAN THE QUALIFIERS FOR VARIABLE,       **
!               **  PARAMETER, FUNCTION, AND VALUE CHANGES  **
!               **  IN THE FUNCTION.                        **
!               **********************************************
!
      ISTEPN='6.3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RECF')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NCHANG=0
      DO 6300 IFORI=1,10
!
        IKEY='FOR '
        IKEY2='    '
        ISHIFT=1
        ILOCA=ILOC3
        IF(IFORI.EQ.1)ILOCA=ILOCMX
        ILOCB=NUMARG
        INCLUN='NO'
        CALL DPEXQU(IKEY,IKEY2,ISHIFT,ILOCA,ILOCB,   &
                    IHARG,IHARG2,NUMARG,   &
                    INCLUN,IANS,IWIDTH,IHNAME,IHNAM2,IVALUE,VALUE,   &
                    IUSE,IN,NUMNAM,   &
                    IFOUN1,IFOUN2,ILOC1,ILOC2,IHOUT,IHOUT2,ILOUT,   &
                    IVOUT,VOUT,IUOUT,   &
                    INOUT,IBUGA3,IERROR)
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
          GO TO 9000
        ENDIF
        IF(IFOUN1.EQ.'NO'.OR.IFOUN2.EQ.'NO')GO TO 6350
!
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
          GO TO 9000
        ENDIF
        NCHANG=NCHANG+1
        IOLD(NCHANG)=IHARG(ILOC2)
        IOLD2(NCHANG)=IHARG2(ILOC2)
        INEW(NCHANG)=IHARG(ILOC3)
        INEW2(NCHANG)=IHARG2(ILOC3)
!
 6300 CONTINUE
 6350 CONTINUE
      GO TO 6390
!
!
 6390 CONTINUE
!
!               **********************************************
!               **  STEP 6.4--                              **
!               **  CARRY OUT THE VARIABLE,                 **
!               **  PARAMETER, AND FUNCTION CHANGES         **
!               **  AND THEN PRINT OUT A BRIEF MESSAGE      **
!               **  INDICATING THAT THE CHANGES             **
!               **  HAVE BEEN MADE.                         **
!               **********************************************
!
      ISTEPN='6.4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RECF')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IPRINT.EQ.'ON' .AND. IFEEDB.EQ.'ON' .AND. NCHANF.GE.1)THEN
!
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
!
        CALL COMPIC(IFUNC3,N3,IOLD,IOLD2,INEW,INEW2,NCHANG,IFUNC3,N3,   &
                    IBUGA3,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
        ILAB(1)='POST'
        ILAB(2)='-CHA'
        ILAB(3)='NGE '
        ILAB(4)='FUNC'
        ILAB(5)='TION'
        ILAB(6)='  = '
        NUMWDL=6
        CALL DPPRIF(ILAB,NUMWDL,IFUNC3,N3,IBUGA3)
!
      ENDIF
!
!               **********************************************************
!               **  STEP 6.7--                                          **
!               **  MAKE A NON-CALCULATING PASS AT THE FUNCTION         **
!               **  SO AS TO EXTRACT ALL PARAMETER AND VARIABLE NAMES.  **
!               **********************************************************
!
      ISTEPN='6.8'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RECF')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IPASS=1
      CALL COMPIM(IFUNC3,N3,IPASS,PARAM,IPARN,IPARN2,NUMPV,   &
                  IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,AJUNK,   &
                  IBUGCO,IBUGEV,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               ***********************************************
!               **  STEP 7--                                 **
!               **  CHECK THAT ALL PARAMETERS                **
!               **  IN THE FUNCTION ARE ALREADY PRESENT      **
!               **  IN THE AVAILABLE NAME LIST IHNAME(.).    **
!               **  ALSO CHECK THAT THE VARIABLE NAME        **
!               **  THAT FOLLOWS FOR (THAT IS, THE DUMMY     **
!               **  VARIABLE IS IN THE FUNCTION.             **
!               **  NOTE--ALL PARAMETERS AND VARIABLES       **
!               **  THAT ARE NOT FOUND IN IHNAME(.)          **
!               **  WILL BE AUTOMATICALLY SET TO 0.0         **
!               **  (BUT ONLY TEMPORARILY);                  **
!               **  THIS CONVENTION ALLOWS AN AUTOMATIC      **
!               **  SOLUTION TO THE PROBLEM OF SOLVING       **
!               **  FOR ROOTS OF EQUATIONS                   **
!               **  (AS OPPOSED TO FUNCTIONS)                **
!               **  SINCE 'Y' WILL TYPICALLY BE SET TO ZERO  **
!               **  AS ONE WOULD WANT FOR SOLVING            **
!               **  FOR A ROOT (= A FUNCTION ZERO).          **
!               ***********************************************
!
      ISTEPN='7'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RECF')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IP=0
      IV=0
      IF(NUMPV.LE.0)GO TO 7650
      DO 7600 J=1,NUMPV
        IHPARN=IPARN(J)
        IHPAR2=IPARN2(J)
        IF(IHPARN.EQ.IDUMV(1).AND.IHPAR2.EQ.IDUMV2(1))THEN
          IV=IV+1
          LOCDUM=J
          GO TO 7600
        ENDIF
        IHWUSE='P'
        MESSAG='YES'
        CALL CHECKN(IHPARN,IHPAR2,IHWUSE,   &
                    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                    ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
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
!
!               ******************************
!               **  STEP 8--                **
!               **  DETERMINE THE ROOTS  .  **
!               ******************************
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RECF')THEN
        ISTEPN='8'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7711)
 7711   FORMAT('***** FROM DPRECF, IMMEDIATELY BEFORE CALLING ',   &
               'ROOTS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7712)N3,NUMPV
 7712   FORMAT('N3,NUMPV = ',I8,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7713)NUMDV,XMIN,XMAX
 7713   FORMAT('NUMDV,XMIN,XMAX = ',I8,2E15.7)
        CALL DPWRST('XXX','BUG ')
        DO 7714 I=1,NUMDV
          WRITE(ICOUT,7715)I,IDUMV(I),IDUMV2(I)
 7715     FORMAT('I,IDUMV(I),IDUMV2(I) = ',I8,2X,A4,A4)
          CALL DPWRST('XXX','BUG ')
 7714   CONTINUE
      ENDIF
!
!CCCC CALL DPREC2(IFUNC3,N3,PARAM,IPARN,IPARN2,NUMPV,
!CCCC1            IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,
!CCCC1            IDUMV,IDUMV2,NUMDV,XMIN,XMAX,ROOTS2,NROOTS,
!CCCC1            IBUGA3,IBUGCO,IBUGEV,IERROR)
!
!               *****************************************
!               **  STEP 9--                           **
!               **  ENTER THE ROOTS INTO THE DATAPLOT  **
!               **  ARRAY V(.).                        **
!               **  ENTER THE FOUND NUMBER OF ROOTS    **
!               **  INTO THE DATAPLOT PARAMETER        **
!               **  NROOTS   .                         **
!               *****************************************
!
      ISTEPN='9'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RECF')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IHL=IHLEFT
      IHL2=IHLEF2
      ICASUP='V'
      CALL DPINVP(IHL,IHL2,ICASUP,ROOTS2,NROOTS,AROOTS,NROOTS,   &
      ISUBN1,ISUBN2,IBUGA3,IERROR)
!
!               ****************
!               **  STEP 90-- **
!               **  EXIT      **
!               ****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RECF')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DPRECF--')
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,NUMNAM
          WRITE(ICOUT,9016)I,IHNAME(I),IHNAM2(I),IUSE(I),   &
                           IVSTAR(I),IVSTOP(I)
 9016     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVSTAR(I),IVSTOP(I)=',   &
                 I8,2X,A4,A4,2X,A4,I8,I8)
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
!
      RETURN
      END SUBROUTINE DPRECF
      SUBROUTINE DPRECH(IHARG,NUMARG,   &
      IBASLC,   &
      IREPCH,   &
      IBUGS2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE REPLACEMENT CHARACTOR WHICH MAY
!              BE USED TO REPLACE A PARAMETER NAME
!              OR A STRING NAME BY ITS CONTENTS.
!              WHEN A COMMAND LINE IS READ,
!              IT IS SEARCHED FOR THE REPLACEMENT CHARACTER;
!              IF IT IS FOUND, THE PARAMETER OR STRING
!              NAME IMMEDIATELY FOLLOWING THE REPLACEMENT CHARACTER
!              IS REPLACEWD/SUBSTITUTED IN LITERALLY
!              AND IMMEDIATELY.
!              THE REPLACEMENT CHARACTER CAPABILITY ALLOWS THE ANALYST
!              TO FILL IN CURRENT VALUES OF PARAMETERS
!              AS LABELS AND LEGENDS ON PLOTS,
!              IT ALSO ALLOWS FILE NAMES TO BE SYMBOLICALLY
!              BUILT INSIDE A LOOP, ETC.
!              THE SPECIFIED REPLACEMENT CHARACTOR WILL BE PLACED
!              IN THE CHARACTER VARIABLE IREPCH.
!     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
!                     --NUMARG (AN INTEGER VARIABLE)
!                     --IBASLC (A CHARACTER VARIABLE--BACKSLASH)
!                     --IBUGS2 (A  CHARACTER VARIABLE)
!     OUTPUT ARGUMENTS--IREPCH (A CHARACTER VARIABLE)
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
!           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
!           MODIFIED, OR OTHERWISE USED IN A CONTEXT
!           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--89/7
!     ORIGINAL VERSION--JUNE     1989.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*1 IBASLC
      CHARACTER*1 IREPCH
      CHARACTER*4 IBUGS2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHARG4
      CHARACTER*1 IHOLD
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
   51 FORMAT('***** AT THE BEGINNING OF DPRECH--')
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
      IHOLD=IBASLC
      GO TO 1180
!
 1160 CONTINUE
      IHARG4=IHARG(NUMARG)
      IHOLD=IHARG4(1:1)
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
      IREPCH=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)IREPCH
 1181 FORMAT('THE REPLACEMENT CHARACTOR HAS JUST BEEN SET TO ',   &
      A1)
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
      WRITE(ICOUT,9013)IHARG4,IHOLD
 9013 FORMAT('IHARG4,IHOLD = ',A4,2X,A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)IREPCH
 9014 FORMAT('IREPCH = ',A1)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPRECH
      SUBROUTINE DPRECI(ISEED,IBUGA2,IBUGA3,IBUGCO,IBUGEV,IBUGQ,   &
                        ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--CARRY OUT MARK VANGEL'S RECIPE FIT
!              FOR LINEAR MODELS.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--88/2
!     ORIGINAL VERSION--AUGUST   1997.
!     UPDATED         --JULY     2019. TWEAK SCRATCH SPACE
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IBUGA2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGCO
      CHARACTER*4 IBUGEV
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 ICASRE
      CHARACTER*4 ICASDG
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 ICASEQ
      CHARACTER*4 IKEY
      CHARACTER*4 IHRIGH
      CHARACTER*4 IHRIG2
      CHARACTER*4 IFLAG
!
      CHARACTER*4 IREPU
      CHARACTER*4 IRESU
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 IHLEFT
      CHARACTER*4 IHLEF2
!
      CHARACTER*4 NEWNAM
      CHARACTER*4 IWRITE
!
      CHARACTER*4 IVARN1
      CHARACTER*4 IVARN2
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      CHARACTER*20 IMODEL
!
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DMEAN
!
      LOGICAL SATT
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZI.INC'
      INCLUDE 'DPCOZD.INC'
!
      DIMENSION ILIS(100)
      DIMENSION ICOLR(100)
!
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
!
      DIMENSION IP(MAXOBV)
      DIMENSION IQ(MAXOBV)
!
      DIMENSION PRED2(MAXOBV)
      DIMENSION RES2(MAXOBV)
!
      DOUBLE PRECISION XMAT(5*MAXOBV)
      DOUBLE PRECISION SCRTCH(10*MAXOBV)
!
      DOUBLE PRECISION XTX(100)
      DOUBLE PRECISION XTXI(100)
      DOUBLE PRECISION S1(100)
      DOUBLE PRECISION S2(100)
      DOUBLE PRECISION V1(100)
      DOUBLE PRECISION COEF(100)
!
      DIMENSION IVARN1(100)
      DIMENSION IVARN2(100)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOHO.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
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
!
      EQUIVALENCE (IGARBG(IIGAR1),IQ(1))
      EQUIVALENCE (IGARBG(IIGAR2),IP(1))
!
      EQUIVALENCE (GARBAG(IGARB1),XMAT(1))
      EQUIVALENCE (GARBAG(IGAR11),SCRTCH(1))
!
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
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPRE'
      ISUBN2='CI  '
      IERROR='NO'
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
      MAXPAR=20
      MAXV2=MAXPAR
      MINN2=2
      MAXN2=MAXCHF
      MAXN3=MAXCHF
      MAXN4=MAXCHF
      MAXLVL=INT(SQRT(REAL(IGARB0)))
!CCCC MAXPT1=20*MAXOBV
!CCCC MAXPT2=10*MAXOBV
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
!
      CPUEPS=R1MACH(3)
!
!               *****************************
!               **  TREAT THE RECIPE CASE  **
!               *****************************
!
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
        DO 57 I=1,NUMNAM
          WRITE(ICOUT,58)I,IHNAME(I),IHNAM2(I),IUSE(I),IN(I),IVALUE(I),   &
                         VALUE(I)
   58     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IN(I),IVALUE(I)',   &
                 'VALUE(I) = ',I8,2X,2A4,2X,A4,2I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   57   CONTINUE
        WRITE(ICOUT,61)IRECSA,RECIDG,RECIPC,RECICO
   61   FORMAT('IRECSA,RECIDG,RECIPC,RECICO=',A4,1X,3(E15.7))
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               **********************************
!               **  STEP 1--                    **
!               **  EXTRACT THE COMMAND         **
!               **    RECIPE FIT                **
!               **    RECIPE ANOVA              **
!               **    RECIPE Y <UNIVARIATE CASE **
!               **********************************
!
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'RECI')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
!
      IF(IBUGA2.EQ.'ON')THEN
        WRITE(ICOUT,66)ICASRE
   66   FORMAT('ICASRE=',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(ICASRE.EQ.'    ')GO TO 9000
!
!               *******************************************************
!               **  STEP 2--                                         **
!               **  CHECK FOR THE PROPER NUMBER OF INPUT ARGUMENTS.  **
!               *******************************************************
!
      ISTEPN='2'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'RECI')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      MINNA=0
      MAXNA=100
      CALL CHECKA(NUMARG,MINNA,MAXNA,IANS,IWIDTH,ISUBN1,ISUBN2,   &
      IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               ******************************************************
!               **  STEP 3--                                        **
!               **  IN PARTICULAR, CHECK THAT THE NUMBER OF ARGUMENTS*
!               **  IS AT LEAST 1,                                  **
!               *******************************************************
!
      ISTEPN='3'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'RECI')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
        GO TO 9000
      ENDIF
!
      DO 2100 J=1,NUMARG
        J1=J
        IF(IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ')GO TO 2110
        IF(IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  ')GO TO 2110
        IF(IHARG(J).EQ.'FOR '.AND.IHARG2(J).EQ.'    ')GO TO 2110
 2100 CONTINUE
      ILOCQ=NUMARG+1
      GO TO 2120
 2110 CONTINUE
      ILOCQ=J1
      GO TO 2120
 2120 CONTINUE
!
!               *******************************************************
!               **  STEP 4--                                         **
!               **  FOR RECIPE FIT AND RECIPE ANOVA,                 **
!               **  THE SECOND WORD AFTER  RECIPE SHOULD BE THE      **
!               **  RESPONSE VARIABLE (= THE DEPENDENT VARIABLE).    **
!               **  FOR RECIPE <Y>, RESPONSE VARIABLE IS FIRST WORD. **
!               **  EXTRACT THE RESPONSE VARIABLE AND DETERMINE      **
!               **  IF IT IS ALREADY IN THE NAME LIST AND IS, IN FACT,*
!               **  A VARIABLE (AS OPPOSED TO A PARAMETER).          **
!               *******************************************************
!
      ISTEPN='4'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'RECI')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ILOCY=2
      IF(ICASRE.EQ.'UREC')ILOCY=1
      IHLEFT=IHARG(ILOCY)
      IHLEF2=IHARG2(ILOCY)
      DO 2350 I=1,NUMNAM
      I2=I
      IF(IHLEFT.EQ.IHNAME(I2).AND.IHLEF2.EQ.IHNAM2(I2).AND.   &
      IUSE(I2).EQ.'V')GO TO 2379
 2350 CONTINUE
      WRITE(ICOUT,2001)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2362)
 2362 FORMAT('      THE NAME FOLLOWING THE WORD RECIPE FIT ',   &
      '(OR RECIPE ANOVA),')
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
      GO TO 9000
 2379 CONTINUE
      ILOCV=I2
      ICOLL=IVALUE(ILOCV)
      NLEFT=IN(ILOCV)
!
!               *******************************************************
!               **  STEP 5--                                         **
!               **  FOR ALL VARIATIONS OF THE RECIPE COMMAND,        **
!               **  CHECK THAT THE INPUT NUMBER OF OBSERVATIONS (NLEFT)
!               **  FOR THE RESPONSE VARIABLE IS 2 OR LARGER.        **
!               *******************************************************
!
      ISTEPN='5'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'RECI')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
        GO TO 9000
      ENDIF
!
!               **************************************************
!               **  STEP 12--                                   **
!               **  EXTRACT THE INDEPENDENT VARIABLES           **
!               **  FOR RECIPE FIT:                             **
!               **      Y X <BATCH> <XPRED>                     **
!               **  FOR RECIPE ANOVA:                           **
!               **      Y X1 ... XK <BATCH>                     **
!               **  FOR RECIPE :                                **
!               **      Y <BATCH>                               **
!               **  IF THE   TO   FEATURE IS USED IN THE        **
!               **  ARGUMENT LIST, TRANSLATE THE   TO   TO      **
!               **  EXPLICIT VARIABLE NAMES             INTO    **
!               **************************************************
!
      ISTEPN='12'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'RECI')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICASRE.EQ.'FREC'.AND.IFITFC.LE.1)THEN
        MAXREC=3
        JMIN=ILOCY+1
        JMAX=ILOCQ-1
        CALL EXTVAR(IHARG,IHARG2,NUMARG,JMIN,JMAX,MAXREC,   &
        IHNAME,IHNAM2,IUSE,NUMNAM,   &
        IVARN1,IVARN2,NUMVAR,IBUGA2,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
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
          GO TO 9000
        ENDIF
  412   FORMAT('      BETWEEN 1 AND 4 VARIABLE NAMES CAN BE SPECIFIED ',   &
               'FOR THIS COMMAND')
  413   FORMAT('      ',I8,' VARIABLES WERE GIVEN.')
      ELSEIF(ICASRE.EQ.'FREC'.AND.IFITFC.GT.1)THEN
        MAXREC=2*IFITFC+1
        JMIN=ILOCY+1
        JMAX=ILOCQ-1
        CALL EXTVAR(IHARG,IHARG2,NUMARG,JMIN,JMAX,MAXREC,   &
        IHNAME,IHNAM2,IUSE,NUMNAM,   &
        IVARN1,IVARN2,NUMVAR,IBUGA2,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
        IF(NUMVAR.LT.IFITFC)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2001)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1422)NUMVAR,IFITFC
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
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
          GO TO 9000
        ENDIF
 1422   FORMAT('      THE NUMBER OF VARIABLES ENTERED ',I5,' IS LESS ',   &
               'THAN THE NUMBER OF FIT FACTORS ',I5)
 1412   FORMAT('      AN IMPROPER NUMBER OF VARIABLE NAMES HAS BEEN ',   &
               'SPECIFIED FOR THIS COMMAND.')
      ELSEIF(ICASRE.EQ.'UREC')THEN
        MAXREC=1
        JMIN=ILOCY+1
        JMAX=ILOCQ-1
        CALL EXTVAR(IHARG,IHARG2,NUMARG,JMIN,JMAX,MAXREC,   &
        IHNAME,IHNAM2,IUSE,NUMNAM,   &
        IVARN1,IVARN2,NUMVAR,IBUGA2,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
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
          GO TO 9000
        ENDIF
  422   FORMAT('      BETWEEN 0 AND 1 VARIABLE NAMES CAN BE SPECIFIED ',   &
               'FOR THIS COMMAND')
  423   FORMAT('      ',I8,' VARIABLES WERE GIVEN.')
      ELSEIF(ICASRE.EQ.'AREC')THEN
        NUMFAC=INT(RECIFA + 0.1)
!CCCC   IF(NUMFAC.GT.MAXPAR)THEN
!CCCC     WRITE(ICOUT,999)
!CCCC     CALL DPWRST('XXX','BUG ')
!CCCC     WRITE(ICOUT,511)
!CCCC     CALL DPWRST('XXX','BUG ')
!CCCC     WRITE(ICOUT,512)NUMFAC,MAXPAR
!CCCC     CALL DPWRST('XXX','BUG ')
!CCCC     IERROR='YES'
!CCCC     GO TO 9000
!CCCC   ENDIF
!C511   FORMAT('***** ERROR IN DPRECI (RECIPE ANOVA)--')
!C512   FORMAT('      THE REQUESTED NUMBER OF FACTORS ',I8,
!CCCC1        ' IS GREATER THAN THE ALLOWED MAXIMUM OF ',I8)
        MAXREC=NUMFAC+1
        JMIN=ILOCY+1
        JMAX=ILOCQ-1
        CALL EXTVAR(IHARG,IHARG2,NUMARG,JMIN,JMAX,MAXREC,   &
        IHNAME,IHNAM2,IUSE,NUMNAM,   &
        IVARN1,IVARN2,NUMVAR,IBUGA2,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
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
          GO TO 9000
        ENDIF
  612   FORMAT('      ',I8,' FACTORS WERE SPECIFIED, BUT ONLY ',I8,   &
               ' VARIABLES WERE GIVEN ON THE COMMAND LINE.')
      ENDIF
!
      IF(IBUGA2.EQ.'ON')THEN
        WRITE(ICOUT,71)NUMVAR,NUMFAC
   71   FORMAT('NUMVAR,NUMFAC=',2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ***************************************
!               **  STEP 13--                        **
!               **  CHECK THE VALIDITY OF EACH       **
!               **  OF THE VARIABLES.                **
!               **  THE DESIGN MATRIX (X) AND BATCH  **
!               **  IDENTIFIER VARIABLE MUST HAVE THE**
!               **  SAME NUMER OF OBSERVATIONS AS THE**
!               **  Y VARIABLE.  THE XPRED VARIABLE  **
!               **  MUST HAVE AT LEAST 2 OBSERVATIONS**
!               ***************************************
!
      ISTEPN='13'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'RECI')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICASRE.EQ.'UREC'.AND.NUMVAR.EQ.0)GO TO 1399
      NPRED=-1
      IFITVA=IFITFC
      IF(ILOCB.GT.0)IFITVA=IFITVA+1
      DO 1300 I=1,NUMVAR
!
      IHRIGH=IVARN1(I)
      IHRIG2=IVARN2(I)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IHRIGH,IHRIG2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      NRIGHT=IN(ILOCV)
      ILIS(I)=ILOCV
      ICOLR(I)=IVALUE(ILOCV)
!
      IF(ICASRE.EQ.'AREC'.OR.(ICASRE.EQ.'FREC'.AND.IFITFC.LE.1).OR.   &
         ILOCXP.LT.0)THEN
        IF(ILOCXP.GT.0 .AND. I.EQ.NUMVAR)NPRED=NRIGHT
        IF(NRIGHT.EQ.NLEFT)GO TO 1390
        IF(ILOCXP.GT.0 .AND. I.EQ.NUMVAR .AND. NRIGHT.GT.2)GO TO 1390
        GO TO 1309
      ENDIF
!
      IF(I.GT.IFITVA)THEN
        IF(NPRED.LT.0)THEN
          NPRED=NRIGHT
          GO TO 1390
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
            GO TO 9000
11313       FORMAT('      THE VARIABLES FOR THE PREDICTED VARIABLES ',   &
                   'DO NOT ALL CONTAIN THE SAME')
11315       FORMAT('      NUMBER OF ELEMENTS FOR THE MULTI-LINEAR FIT ',   &
                   'CASE.')
          ELSE
            GO TO 1390
          ENDIF
        ENDIF
      ELSE
        IF(NRIGHT.EQ.NLEFT)GO TO 1390
      ENDIF
!
 1309 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2001)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1312)
 1312 FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE INDEPENDENT ',   &
             'VARIABLES')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1322)
 1322 FORMAT('      MUST BE THE SAME AS THE DEPENDENT VARIABLE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1323)
 1323 FORMAT('      IN ADDITION, THE VARIABLE CONTAINING THE X ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1324)
 1324 FORMAT('      VALUES FOR THE TOLERANCE LIMITS MUST HAVE AT ',   &
             'LEAST 2 ELEMENTS.')
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
      GO TO 9000
 1390 CONTINUE
!
 1300 CONTINUE
 1399 CONTINUE
!
!               **********************************************
!               **  STEP 6.3--                              **
!               **  FOR ALL VARIATIONS OF THE RECIPE COMMAND,*
!               **  CHECK TO SEE THE TYPE CASE--            **
!               **    1) UNQUALIFIED (THAT IS, FULL);       **
!               **    2) SUBSET/EXCEPT; OR                  **
!               **    3) FOR.                               **
!               **********************************************
!
      ISTEPN='6.3'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'RECI')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICASEQ='FULL'
      ILOCQ=NUMARG+1
      IF(NUMARG.LT.1)GO TO 490
      DO 400 J=1,NUMARG
      J1=J
      IF(IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ')GO TO 410
      IF(IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  ')GO TO 410
      IF(IHARG(J).EQ.'FOR '.AND.IHARG2(J).EQ.'    ')GO TO 420
  400 CONTINUE
      GO TO 490
  410 CONTINUE
      ICASEQ='SUBS'
      IKEY='SUBS'
      IF(IHARG(J1).EQ.'EXCE')IKEY='EXCE'
      ILOCQ=J1
      GO TO 490
  420 CONTINUE
      ICASEQ='FOR'
      ILOCQ=J1
      GO TO 490
  490 CONTINUE
      IF(IBUGA2.EQ.'OFF'.AND.ISUBRO.NE.'RECI')GO TO 495
      WRITE(ICOUT,491)NUMARG,ILOCQ
  491 FORMAT('NUMARG,ILOCQ = ',2I8)
      CALL DPWRST('XXX','BUG ')
  495 CONTINUE
!
!               *******************************************************
!               **  STEP 12--                                        **
!               **  BRANCH TO THE APPROPRIATE SUBCASE; THEN          **
!               **  COPY OVER THE RESPONSE VECTOR TO BE USED IN THE  **
!               **  MODEL INTO THE VECTOR Y2; AND                    **
!               **  COPY OVER THE VECTORS THAT WERE USED IN THE MODEL**
!               **  INTO THE FULL DESIGN MATRIX                      **
!               *******************************************************
!
      ISTEPN='12'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'RECI')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'RECI')WRITE(ICOUT,601)NLEFT,NUMVAR
  601 FORMAT('NLEFT,NUMVAR = ',2I8)
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'RECI')CALL DPWRST('XXX','BUG ')
!
      IF(ICASEQ.EQ.'FULL')GO TO 610
      IF(ICASEQ.EQ.'SUBS')GO TO 620
      IF(ICASEQ.EQ.'FOR')GO TO 630
!
  610 CONTINUE
      DO 615 I=1,NLEFT
      ISUB(I)=1
  615 CONTINUE
      NQ=NLEFT
      GO TO 650
!
  620 CONTINUE
      NIOLD=NLEFT
      CALL DPSUBS(NIOLD,ILOCS,NS,IBUGQ,IERROR)
      NQ=NIOLD
      GO TO 650
!
  630 CONTINUE
      NIOLD=NLEFT
      CALL DPFOR(NIOLD,NFOR,IROW1,IROWN,   &
      NLOCAL,ILOCS,NS,IBUGQ,IERROR)
      NQ=NFOR
      GO TO 650
!
  650 CONTINUE
      NTOT=NQ
      K=ICOLL
      J=0
      DO 4500 I=1,NLEFT
      IF(ISUB(I).EQ.0)GO TO 4500
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
!
      IF(IBUGA2.EQ.'ON')THEN
        DO 4503 I=1,NTOT
        WRITE(ICOUT,4504)I,Y2(I)
 4504   FORMAT('I,Y2(I)=',I8,2X,G15.7)
        CALL DPWRST('XXX','BUG ')
 4503   CONTINUE
      ENDIF
!
!     ********************************************************
!     ** DEFINE A VECTOR OF ALL 1'S (FOR THE CONSTANT TERM) **
!     ** IN THE DESIGN MATRIX.                              **
!     ********************************************************
!
      J=0
      DO 380 I=1,NLEFT
      IF(ISUB(I).EQ.0)GO TO 380
      J=J+1
      XMAT(J)=1.0D0
  380 CONTINUE
!
!     ********************************************************
!     ** DETERMINE IF THERE IS A BATCH VARIABLE.  IF NOT,   **
!     ** CREATE ONE EQUAL TO ALL 1'S.  IF YES, DETERMINE    **
!     ** HOW MANY UNIQUE VALUES.                            **
!     ********************************************************
!
      IF(ILOCB.LE.0)THEN
        J=0
        DO 4610 I=1,NLEFT
          IF(ISUB(I).EQ.0)GO TO 4610
          J=J+1
          IQ(J)=1
 4610   CONTINUE
        NBCH=1
        GO TO 4699
      ENDIF
!
      IF(ICASRE.EQ.'FREC'.AND.IFITFC.LE.1)THEN
        K=ICOLR(NUMVAR)
        IF(ILOCXP.GT.0)K=ICOLR(NUMVAR-1)
      ELSEIF(ICASRE.EQ.'FREC'.AND.IFITFC.GT.1)THEN
        K=ICOLR(ILOCB)
      ELSE
        K=ICOLR(NUMVAR)
      ENDIF
!
      J=0
      DO 4600 I=1,NLEFT
      IF(ISUB(I).EQ.0)GO TO 4600
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
!
      CALL SORT(RES2,NQ,PRED2)
      IWRITE='NO'
      CALL DISTIN(PRED2,NQ,IWRITE,PRED2,NBCH,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      DO 4650 I=1,NQ
        IQ(I)=0
        DO 4660 J=1,NBCH
          IF(RES2(I).EQ.PRED2(J))THEN
            IQ(I)=J
            GO TO 4650
          ENDIF
 4660   CONTINUE
 4650 CONTINUE
!
 4699 CONTINUE
!
      IF(IBUGA2.EQ.'ON')THEN
        DO 4603 I=1,NTOT
        WRITE(ICOUT,4604)I,IQ(I)
 4604   FORMAT('I,IQ(I)=',I8,2X,I8)
        CALL DPWRST('XXX','BUG ')
 4603   CONTINUE
      ENDIF
!
!     ********************************************************
!     ** DETERMINE IF THERE IS A PREDICTED VARIABLE (FIT    **
!     ** CASE ONLY).  IF SO, EXTRACT AND PUT IN XPTS.       **
!     ********************************************************
!
      IF(ICASRE.EQ.'UREC')THEN
        XPTS(1)=1.D0
        NPRED=1
        NPAR=1
        GO TO 4799
      ELSEIF(ILOCXP.LT.0.OR.ICASRE.EQ.'AREC')THEN
        DO 4701 I=1,MAXOBV/2
          XPTS(I)=0.D0
 4701   CONTINUE
        NPRED=0
        GO TO 4799
      ENDIF
!
      IF(ICASRE.EQ.'FREC'.AND.IFITFC.LE.1)THEN
        K=ICOLR(NUMVAR)
        DO 4703 I=1,NPRED
          XPTS(I)=1.D0
 4703   CONTINUE
        J=NPRED
        DO 4700 I=1,NPRED
        IF(ISUB(I).EQ.0)GO TO 4700
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
!
      ELSEIF(ICASRE.EQ.'FREC'.AND.IFITFC.GT.1)THEN
        DO 5903 I=1,NPRED
          XPTS(I)=1.D0
 5903   CONTINUE
        NLOOP=IFITFC
        ISTRT=IFITFC+1
        IF(ILOCB.GT.0)ISTRT=ISTRT+1
        ISTOP=ISTRT+IFITFC-1
        DO 5376 IVAR=ISTRT,ISTOP
          K=ICOLR(IVAR)
          J=(IVAR-ISTRT+1)*NPRED
          DO 5371 I=1,NPRED
            IF(ISUB(I).EQ.0)GO TO 5371
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
!
 4799 CONTINUE
!
      IF(IBUGA2.EQ.'ON')THEN
        DO 4713 I=1,2*NPRED
        WRITE(ICOUT,4714)I,XPTS(I)
 4714   FORMAT('I,XPTS(I)=',I8,2X,D15.7)
        CALL DPWRST('XXX','BUG ')
 4713   CONTINUE
      ENDIF
!
!     ********************************************************
!     ** COPY OVER THE FULL DESIGN MATRIX.                  **
!     ********************************************************
!
      IF(ICASRE.EQ.'FREC'.AND.IFITFC.LE.1)THEN
        NPAR=1
        IF(ICASDG.EQ.'0')GO TO 379
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
        DO 376 IVAR=1,NLOOP
          J=IVAR*NTOT
          DO 371 I=1,NLEFT
            IF(ISUB(I).EQ.0)GO TO 371
            J=J+1
!
            IF(J.GT.MAXPT2)THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2001)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,377)
  377         FORMAT('      STORAGE SPACE FOR DESIGN SPACE ',   &
                     'EXCEEDED.')
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GO TO 9000
            ENDIF
!
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
!
      ELSEIF(ICASRE.EQ.'FREC'.AND.IFITFC.GT.1)THEN
        NPAR=1
        NLOOP=IFITFC
        DO 1376 IVAR=1,NLOOP
          K=ICOLR(IVAR)
          J=IVAR*NTOT
          DO 1371 I=1,NLEFT
            IF(ISUB(I).EQ.0)GO TO 1371
            J=J+1
!
            IF(J.GT.MAXPT2)THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2001)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,377)
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GO TO 9000
            ENDIF
!
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
!CCCC   DO372I=1,NLEFT
!CCCC     IF(ISUB(I).EQ.0)GO TO 372
!CCCC     J=J+1
!CCCC     XMAT(J)=1.D0
!372    CONTINUE
      ELSEIF(ICASRE.EQ.'AREC')THEN
        NLOOP=NUMVAR
        IF(ILOCB.GT.0)NLOOP=NUMVAR-1
        DO 389 IVAR=1,NLOOP
          K=ICOLR(IVAR)
          J=IVAR*NTOT
          DO 381 I=1,NLEFT
            IF(ISUB(I).EQ.0)GO TO 381
            J=J+1
!
            IF(J.GT.MAXPT2)THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2001)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,377)
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GO TO 9000
            ENDIF
!
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
!
      IF(IBUGA2.EQ.'ON')THEN
        DO 4803 I=1,NTOT*NPAR
        WRITE(ICOUT,4804)I,XMAT(I)
 4804   FORMAT('I,XMAT(I)=',I8,2X,D15.7)
        CALL DPWRST('XXX','BUG ')
 4803   CONTINUE
      ENDIF
!
!               ******************************************************
!               **  STEP 14--                                       **
!               **  CARRY OUT THE ACTUAL FIT                        **
!               **  VIA CALLING                                     **
!               **  REGINI AND REGDAT                               **
!               ******************************************************
!
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
        GO TO 9000
      ENDIF
 6071 FORMAT('      THE AMOUNT OF SCRATCH STORAGE REQUIRED EQUALS')
 6072 FORMAT('     NUMBER OF POINTS*(NUMBER OF PARAMETERS + NUMBER OF',   &
             ' BATCHES) = ',I8)
 6073 FORMAT('     EXCEEDS THE MAXIMIM ALLOWABLE OF ',I8)
!
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'RECI')THEN
        ISTEPN='14'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6081)
 6081   FORMAT('***** FROM DPRECI, AS ABOUT TO CALL REGINI--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      SATT=.FALSE.
      IF(IRECSA.EQ.'YES'.OR.IRECSA.EQ.'TRUE'.OR.IRECSA.EQ.'ON')   &
         SATT=.TRUE.
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
 6531 FORMAT('THE REQUESTED NUMBER OF SIMULATION REPLICATIONS ',I8,   &
      ' IS GREATER THAN THE ALLOWED MAXIMUM OF ',I8)
 6532 FORMAT('THE MAXIMUM ALLOWED NUMBER OF REPLICATIONS WILL BE ',   &
      'USED.')
      CALL REGINI(NLVL,NPAR,NTOT,NBCH,NPRED,XDESGN,XPTS,IP,IQ,   &
                  DBLE(RECIPC),DBLE(RECICO),XMAT,XTX,XTXI,XN,SCRTCH,   &
                  S1,V1,S2,V2,TLM0,TLM1,ETA0,ETA1,   &
                  SATT,IN2,WK2,WK3,   &
                  CRT,ISEED,MAXREP,MAXLVL,   &
                  ICASRE,ISUBRO,IBUGA2,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'RECI')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6181)
 6181   FORMAT('***** FROM DPRECI, AS ABOUT TO CALL REGDAT--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IFLAG='RECI'
      CALL REGDAT(NPAR,NTOT,NBCH,NPRED,XPTS,Y2,COEF,   &
                  SCRTCH,S1,V1,TLM0,TLM1,ETA0,ETA1,   &
                  XMAT,XM,T,XDESGN,NLVL,   &
                  ICASRE,IFLAG,ISUBRO,IBUGA2,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      DSUM1=0.D0
      IF(ICASRE.EQ.'AREC'.OR.(ICASRE.EQ.'FREC'.AND.ILOCXP.LE.0))THEN
        DO 1029 I=1,NLVL
          TLM0(I)=T(I)
 1029   CONTINUE
      ENDIF
      DO 1030 I=1,NTOT
        INDX=IP(I)
        PRED2(I)=SNGL(XM(INDX))
        RES2(I)=SNGL(Y2(I))-PRED2(I)
        IF(ICASRE.EQ.'AREC'.OR.(ICASRE.EQ.'FREC'.AND.ILOCXP.LE.0))THEN
          T(I)=TLM0(INDX)
        ENDIF
!
        IF(IBUGA2.EQ.'ON')THEN
          WRITE(ICOUT,11030)I,INDX,PRED2(I),RES2(I)
11030     FORMAT('I,INDX,PRED2(I),RES2(I)=',2I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        DSUM1=DSUM1+DBLE(RES2(I))
 1030 CONTINUE
      DMEAN=DSUM1/DBLE(NTOT)
      DSUM1=0.D0
      DO 1031 I=1,NTOT
        DSUM1=DSUM1+(DBLE(RES2(I))-DMEAN)**2
 1031 CONTINUE
      RESDF=REAL(NTOT-NPAR)
      IF(ICASRE.EQ.'AREC')RESDF=REAL(NTOT-(NLVL-NUMFAC)-1)
      IF(ICASRE.EQ.'UREC')RESDF=REAL(NTOT-1)
      RESSD=SNGL(DSQRT(DSUM1)/DBLE(RESDF))
!
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
!
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
 1139   FORMAT('SIMULATED CRITICAL VALUES (SIMPVT) USED WITH ',   &
               I8,' REPLICATIONS')
        CALL DPWRST('XXX','WRIT')
      ENDIF
      WRITE(ICOUT,998)
  998 FORMAT(' ')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,998)
      CALL DPWRST('XXX','WRIT')
!
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
!
!               ***************************************
!               **  STEP 15--                        **
!               **  UPDATE INTERNAL DATAPLOT TABLES  **
!               ***************************************
!
      ISTEPN='15'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'RECI')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICOLPR=MAXCP1
      ICOLRE=MAXCP2
      IREPU='OFF'
      IRESU='ON'
      CALL UPDAPR(ICOLPR,ICOLRE,PRED2,RES2,PRED,RES,ISUB,NLEFT,   &
      IREPU,REPSD,REPDF,IRESU,RESSD,RESDF,ALFCDF,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,ILOCN,IBUGA3,IERROR)
!
!               ***************************************
!               **  STEP 16--                        **
!               **  STORE THE TOLERANCE VALUES       **
!               ***************************************
      IH=IRECTN(1:4)
      IH2=IRECTN(5:8)
!
      NEWNAM='NO'
      DO 7650 I=1,NUMNAM
      I2=I
      IF(IH.EQ.IHNAME(I).AND.IH2.EQ.IHNAM2(I).AND.   &
      IUSE(I).EQ.'V')THEN
        ICOLL1=IVALUE(I2)
        GO TO 7680
      ENDIF
      IF(IH.EQ.IHNAME(I).AND.IH2.EQ.IHNAM2(I).AND.   &
      IUSE(I).NE.'V')THEN
        WRITE(ICOUT,7646)
 7646   FORMAT('***** ERROR IN DPRECI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7647)IRECTN
 7647   FORMAT('      THE REQUESTED NAME FOR THE TOLERANCE ',   &
               'VARIABLE, ',A8,', WAS FOUND IN THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7648)
 7648   FORMAT('      CURRENT NAME LIST, BUT NOT AS A VARIABLE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7649)
 7649   FORMAT('      THEREFORE THE TOLERANCE VARIABLE WAS NOT ',   &
               'UPDATED.')
        CALL DPWRST('XXX','BUG ')
        GO TO 7699
      ENDIF
 7650 CONTINUE
      NEWNAM='YES'
!
!  NEW VARIABLE, CHECK TO ENSURE MAXIMUM NAMES AND MAXIMUM
!  COLUMNS NOT EXCEEDED.
!
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
        GO TO 7699
      ENDIF
!
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
        GO TO 7699
      ENDIF
!
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
!
      IF(IBUGA2.EQ.'ON')THEN
        WRITE(ICOUT,7681)NEWNAM,ICOLL1,NUMCOL,NUMNAM,NPRED,NTEMP
        CALL DPWRST('XXX','BUG ')
 7681   FORMAT('NEWNAM,ICOLL1,NUMCOL,NUMNAM,NPRED,NTEMP =',   &
               A4,1X,5I8)
      ENDIF
!
      K=ICOLL1
      DO 7682 I=1,NTEMP
        IJ=MAXN*(K-1)+I
        IF(K.LE.MAXCOL)V(IJ)=T(I)
        IF(K.EQ.MAXCP1)PRED(I)=T(I)
        IF(K.EQ.MAXCP1)RES(I)=T(I)
        IF(K.EQ.MAXCP1)YPLOT(I)=T(I)
        IF(K.EQ.MAXCP1)XPLOT(I)=T(I)
        IF(K.EQ.MAXCP1)X2PLOT(I)=T(I)
        IF(K.EQ.MAXCP1)TAGPLO(I)=T(I)
 7682 CONTINUE
!
 7699 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
        DO 9017 I=1,NUMNAM
          WRITE(ICOUT,9018)I,IHNAME(I),IHNAM2(I),IUSE(I),IN(I),   &
                           IVALUE(I),VALUE(I)
 9018     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IN(I),IVALUE(I)',   &
                 'VALUE(I) = ',I8,2X,2A4,2X,A4,2I8,F15.7)
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
!
      RETURN
      END SUBROUTINE DPRECI
      SUBROUTINE DPRECO(IHARG,IARGT,ARG,NUMARG,DEFRCO,   &
      RECICO,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE RECIPE CONFIDENCE
!              IN THE FLOATING POINT VARIABLE RECICO.
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --IARGT  (A  HOLLERITH VECTOR)
!                     --ARG    (A  FLOATING POINT VECTOR)
!                     --NUMARG (AN INTEGER VARIABLE)
!                     --DEFRCO (A  FLOATING POINT VARIABLE)
!     OUTPUT ARGUMENTS--RECICO  (A  FLOATING POINT VARIABLE)
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
!           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
!           MODIFIED, OR OTHERWISE USED IN A CONTEXT
!           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--97/8
!     ORIGINAL VERSION--AUGUST   1997.
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
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
!
      HOLD=CPUMIN
!
      IF(NUMARG.EQ.0)GO TO 1199
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'=')GO TO 1140
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'CONF')GO TO 1110
      GO TO 1199
!
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'CONT')GO TO 1150
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1150
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      IF(IARGT(NUMARG).EQ.'NUMB')GO TO 1160
      GO TO 1120
!
 1120 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,1121)
 1121 FORMAT('***** ERROR IN DPRECO--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR RECIPE CONFIDENCE ',   &
      'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1130)
 1130 FORMAT('      AN EXAMPLE OF THIS COMMAND IS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1131)
 1131 FORMAT('      RECIPE CONFIDENCE .90 ')
      CALL DPWRST('XXX','BUG ')
      GO TO 1199
!
 1140 CONTINUE
      IF(NUMARG.EQ.2)HOLD=DEFRCO
      IF(NUMARG.GT.2.AND.IARGT(NUMARG).EQ.'NUMB')HOLD=ARG(NUMARG)
      GO TO 1180
!
 1150 CONTINUE
      HOLD=DEFRCO
      GO TO 1180
!
 1160 CONTINUE
      HOLD=ARG(NUMARG)
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
      IF(HOLD.GE.1.0 .AND. HOLD.LT.100.0)HOLD=HOLD/100.
      IF(HOLD.LE.0.0 .OR. HOLD.GE.1.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1182)
 1182 FORMAT('**** THE RECIPE CONFIDENCE MUST BE SET TO BETWEEN 0 AND',   &
      ' 1 EXCLUSIVE (TYPICAL VALUES BETWEEN .9 AND .99)')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1183)HOLD
 1183 FORMAT('     THE VALUE ENTERED WAS ',E15.7)
        CALL DPWRST('XXX','BUG ')
        GO TO 1199
      ENDIF
      RECICO=HOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)RECICO
 1181 FORMAT('THE RECIPE CONFIDENCE HAS JUST BEEN SET TO ',   &
      E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 1199
!
 1199 CONTINUE
      RETURN
      END SUBROUTINE DPRECO
      SUBROUTINE DPRECR(IHARG,IARGT,IARG,NUMARG,IDEFCR,   &
                        IRECCR,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE NUMBER OF RECIPE CORRELATION POINTS
!              TO USE FOR THE SIMCOV COMMAND
!              IN THE INTEGER POINT VARIABLE IRECCR.
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --IARGT  (A  HOLLERITH VECTOR)
!                     --IARG   (A  INTEGER POINT VECTOR)
!                     --NUMARG (AN INTEGER VARIABLE)
!                     --IDEFCR (A  FLOATING POINT VARIABLE)
!     OUTPUT ARGUMENTS--IRECCR  (A  FLOATING POINT VARIABLE)
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
!           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
!           MODIFIED, OR OTHERWISE USED IN A CONTEXT
!           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--97/8
!     ORIGINAL VERSION--AUGUST   1997.
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
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
!
      IHOLD=0
!
      IF(NUMARG.EQ.0)GO TO 1199
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'=')GO TO 1140
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'CORR')GO TO 1110
      GO TO 1199
!
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'CORR')GO TO 1150
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1150
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      IF(IARGT(NUMARG).EQ.'NUMB')GO TO 1160
      GO TO 1120
!
 1120 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,1121)
 1121 FORMAT('***** ERROR IN DPRECR--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR RECIPE CORRELATION ',   &
      'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1130)
 1130 FORMAT('      AN EXAMPLE OF THIS COMMAND IS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1131)
 1131 FORMAT('      RECIPE CORRELATION 11 ')
      CALL DPWRST('XXX','BUG ')
      GO TO 1199
!
 1140 CONTINUE
      IF(NUMARG.EQ.2)IHOLD=IDEFCR
      IF(NUMARG.GT.2.AND.IARGT(NUMARG).EQ.'NUMB')IHOLD=IARG(NUMARG)
      GO TO 1180
!
 1150 CONTINUE
      IHOLD=IDEFCR
      GO TO 1180
!
 1160 CONTINUE
      IHOLD=IARG(NUMARG)
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
      IF(IHOLD.LE.3)IHOLD=3
      IF(IHOLD.GE.100)IHOLD=100
      IRECCR=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)IRECCR
 1181 FORMAT('THE NUMBER OF CORRELATION POINTS FOR THE SIMCOV ',   &
      'COMMAND HAS JUST BEEN SET TO ',I8)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 1199
!
 1199 CONTINUE
      RETURN
      END SUBROUTINE DPRECR
      SUBROUTINE DPREDG(IHARG,IARGT,ARG,NUMARG,DEFRDG,   &
      RECIDG,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE RECIPE FIT DEGREE
!              IN THE FLOATING POINT VARIABLE RECIDG.
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --IARGT  (A  HOLLERITH VECTOR)
!                     --ARG    (A  FLOATING POINT VECTOR)
!                     --NUMARG (AN INTEGER VARIABLE)
!                     --DEFRDG (A  FLOATING POINT VARIABLE)
!     OUTPUT ARGUMENTS--RECIDG  (A  FLOATING POINT VARIABLE)
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
!           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
!           MODIFIED, OR OTHERWISE USED IN A CONTEXT
!           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--97/8
!     ORIGINAL VERSION--AUGUST   1997.
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
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
!
      IF(NUMARG.EQ.0)GO TO 1199
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'FIT '.AND.IHARG(2).EQ.'DEGR')   &
      GO TO 1110
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'=')GO TO 1110
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'=')GO TO 1110
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'DEGR')GO TO 1110
      GO TO 1199
!
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'DEGR')GO TO 1150
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1150
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      IF(IARGT(NUMARG).EQ.'NUMB')GO TO 1160
      GO TO 1120
!
 1120 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,1121)
 1121 FORMAT('***** ERROR IN DPREDG--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR RECIPE FIT DEGREE ',   &
      'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1130)
 1130 FORMAT('      AN EXAMPLE OF THIS COMMAND IS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1131)
 1131 FORMAT('      RECIPE FIT DEGREE 2 ')
      CALL DPWRST('XXX','BUG ')
      GO TO 1199
!
 1150 CONTINUE
      HOLD=DEFRDG
      GO TO 1180
!
 1160 CONTINUE
      HOLD=ARG(NUMARG)
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
      IF(HOLD.LT.0.0 .OR. HOLD.GT.10.5)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1182)
 1182 FORMAT('**** THE RECIPE DEGREE MUST BE SET TO BETWEEN 0 AND 10',   &
      ' (WITH TYPICAL VALUES BEING 1 OR 2)')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1183)HOLD
 1183 FORMAT('     THE VALUE ENTERED WAS ',E15.7)
        CALL DPWRST('XXX','BUG ')
        GO TO 1199
      ENDIF
      RECIDG=HOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)RECIDG
 1181 FORMAT('THE RECIPE FIT DEGREE HAS JUST BEEN SET TO ',   &
      E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 1199
!
 1199 CONTINUE
      RETURN
      END SUBROUTINE DPREDG
      SUBROUTINE DPREFA(IHARG,IARGT,ARG,NUMARG,DEFRFA,   &
      RECIFA,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE RECIPE ANOVA FACTORS
!              IN THE FLOATING POINT VARIABLE RECIFA.
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --IARGT  (A  HOLLERITH VECTOR)
!                     --ARG    (A  FLOATING POINT VECTOR)
!                     --NUMARG (AN INTEGER VARIABLE)
!                     --DEFRFA (A  FLOATING POINT VARIABLE)
!     OUTPUT ARGUMENTS--RECIFA  (A  FLOATING POINT VARIABLE)
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
!           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
!           MODIFIED, OR OTHERWISE USED IN A CONTEXT
!           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--97/8
!     ORIGINAL VERSION--AUGUST   1997.
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
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
!
      IF(NUMARG.EQ.0)GO TO 1199
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'ANOV'.AND.IHARG(2).EQ.'FACT')   &
      GO TO 1110
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'=')GO TO 1110
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'=')GO TO 1110
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'FACT')GO TO 1110
      GO TO 1199
!
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'FACT')GO TO 1150
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1150
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      IF(IARGT(NUMARG).EQ.'NUMB')GO TO 1160
      GO TO 1120
!
 1120 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,1121)
 1121 FORMAT('***** ERROR IN DPREFA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR RECIPE ANOVA FACTORS ',   &
      'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1130)
 1130 FORMAT('      AN EXAMPLE OF THIS COMMAND IS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1131)
 1131 FORMAT('      RECIPE ANOVA FACTORS 2 ')
      CALL DPWRST('XXX','BUG ')
      GO TO 1199
!
 1150 CONTINUE
      HOLD=DEFRFA
      GO TO 1180
!
 1160 CONTINUE
      HOLD=ARG(NUMARG)
      GO TO 1180
!
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
        GO TO 1199
      ENDIF
      RECIFA=HOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)RECIFA
 1181 FORMAT('THE NUMBER OF FACTORS FOR RECIPE ANOVA HAS JUST BEEN ',   &
      'SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 1199
!
 1199 CONTINUE
      RETURN
      END SUBROUTINE DPREFA
      SUBROUTINE DPREFF(IHARG,IARGT,ARG,NUMARG,DEFRFF,   &
      RECIFF,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE RECIPE FIT FACTORS
!              IN THE FLOATING POINT VARIABLE RECIFF.
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --IARGT  (A  HOLLERITH VECTOR)
!                     --ARG    (A  FLOATING POINT VECTOR)
!                     --NUMARG (AN INTEGER VARIABLE)
!                     --DEFRFF (A  FLOATING POINT VARIABLE)
!     OUTPUT ARGUMENTS--RECIFF  (A  FLOATING POINT VARIABLE)
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
!           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
!           MODIFIED, OR OTHERWISE USED IN A CONTEXT
!           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--98/4
!     ORIGINAL VERSION--APRIL    1998.
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
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
!
      IF(NUMARG.EQ.0)GO TO 1199
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'FIT '.AND.IHARG(2).EQ.'FACT')   &
      GO TO 1110
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'=')GO TO 1110
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'=')GO TO 1110
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'FACT')GO TO 1110
      GO TO 1199
!
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'FACT')GO TO 1150
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1150
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      IF(IARGT(NUMARG).EQ.'NUMB')GO TO 1160
      GO TO 1120
!
 1120 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,1121)
 1121 FORMAT('***** ERROR IN DPREFF--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR RECIPE FIT FACTORS ',   &
      'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1130)
 1130 FORMAT('      AN EXAMPLE OF THIS COMMAND IS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1131)
 1131 FORMAT('      RECIPE FIT FACTORS 2 ')
      CALL DPWRST('XXX','BUG ')
      GO TO 1199
!
 1150 CONTINUE
      HOLD=DEFRFF
      GO TO 1180
!
 1160 CONTINUE
      HOLD=ARG(NUMARG)
      GO TO 1180
!
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
        GO TO 1199
      ENDIF
      RECIFF=HOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)RECIFF
 1181 FORMAT('THE NUMBER OF FACTORS FOR RECIPE FIT HAS JUST BEEN ',   &
      'SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 1199
!
 1199 CONTINUE
      RETURN
      END SUBROUTINE DPREFF
      SUBROUTINE DPREGR(IANSLC,IWIDTH,IHARG,IARGT,IARG,NUMARG,   &
                        IBUGS2,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--REPEAT A PREVIOUSLY CREATED PLOT AND PUT ON A DISTINCT
!              GRAPHICS WINDOW.
!
!                  REPEAT PLOT <FILE NAME>:
!                      REDRAWS THE PIXMAP FROM A SPECIFIED FILE
!
!                  REPEAT PLOT <+N>:
!                      REDRAWS THE Nth PIXMAP FROM THE CURRENT LIST
!
!                  REPEAT PLOT <-N>:
!                      REDRAWS THE Nth PIXMAP AGO FROM THE CURRENT LIST
!                      (E.G., IF THERE ARE CURRENTLY 8 PIXMAPS,
!                      REPEAT PLOT -2 PLOTS THE SIXTH PIXMAP
!
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGU
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--97/4
!     ORIGINAL VERSION--APRIL     1997.
!     UPDATED         --AUGUST    1997. MOVE SOME CODE TO A LOWER LEVEL
!                                       TO SUPPORT NON-X11 DEVICES
!                                       (SPECIFICALLY PC FOR NOW)
!     UPDATED         --AUGUST    2016. ARGUMENT LIST TO DPINFI
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      INCLUDE 'DPCOPA.INC'
!
      CHARACTER*4 IANSLC(*)
      CHARACTER*4 IHARG(*)
      CHARACTER*4 IARGT(*)
!
      CHARACTER*4 IBUGS2
      CHARACTER*4 IEXIST
      CHARACTER*4 IOPEN
      CHARACTER*8 IACC
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
      CHARACTER*4 IFOUND
!
      CHARACTER*4 IC4
      CHARACTER*4 ICODE
      CHARACTER*128 CTEMP
!  DIMENSION FOLLOWING 2 LINES TO MAXSTR
      CHARACTER (LEN=MAXSTR) :: ISTRIN
      CHARACTER (LEN=MAXSTR) :: ISTRI2
!
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
      DIMENSION IARG(*)
!CCCC DIMENSION IADE(128)
!CCCC DIMENSION IADE2(138)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPM.INC'
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPRE'
      ISUBN2='GR  '
      IFOUND='NO'
      IERROR='NO'
!
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
          DO 56 I=1,NUMARG
            WRITE(ICOUT,57)I,IHARG(I)
   57       FORMAT('I,IHARG(I) = ',I8,2X,A4)
            CALL DPWRST('XXX','BUG ')
   56     CONTINUE
        ENDIF
      ENDIF
!
      IFOUND='YES'
!
!               ******************************************************
!               **  STEP 11--                                       **
!               **  DETERMINE IF HAVE AN EXPLICIT FILE REFERENCE    **
!               **  WHERE THE PIXMAP FILE IS STORED                 **
!               ******************************************************
!
      ISTEPN='11'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REGR')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IFILWD=(-999)
!
      DO 1100 I=1,MAXSTR
        IC4=IANSLC(I)
        ISTRIN(I:I)=IC4(1:1)
 1100 CONTINUE
!
      IWORD=1
      ISTART=1
      ISTOP=MAXSTR-1
      CALL DPEXWO(ISTRIN,ISTART,ISTOP,IWORD,   &
      ICOL1,ICOL2,ISTRI2,NCSTR2,   &
      IBUGS2,ISUBRO,IERROR)
!
      IF(NUMARG.LE.0)GO TO 1129
      IWORD=2
      ISTART=1
      ISTOP=MAXSTR-1
      CALL DPEXWO(ISTRIN,ISTART,ISTOP,IWORD,   &
      ICOL1,ICOL2,ISTRI2,NCSTR2,   &
      IBUGS2,ISUBRO,IERROR)
      IF(NCSTR2.LE.0)GO TO 1129
      DO 1121 I=1,NCSTR2
      IF(ISTRI2(I:I).EQ.'.')GO TO 1122
 1121 CONTINUE
      GO TO 1129
 1122 CONTINUE
      IFILWD=2
      GO TO 1150
 1129 CONTINUE
!
      IF(NUMARG.LE.1)GO TO 1139
      IWORD=3
      ISTART=1
      ISTOP=MAXSTR-1
      CALL DPEXWO(ISTRIN,ISTART,ISTOP,IWORD,   &
      ICOL1,ICOL2,ISTRI2,NCSTR2,   &
      IBUGS2,ISUBRO,IERROR)
      IF(NCSTR2.LE.0)GO TO 1139
      DO 1131 I=1,NCSTR2
      IF(ISTRI2(I:I).EQ.'.')GO TO 1132
 1131 CONTINUE
      GO TO 1139
 1132 CONTINUE
      IFILWD=3
      GO TO 1150
 1139 CONTINUE
!
 1150 CONTINUE
      ISTAM1=0
      IF(IFILWD.EQ.2.OR.IFILWD.EQ.3)ISTAM1=1
!
!               ******************************************************
!               **  STEP 11.B--IF NO FILE, CHECK FOR NUMBER         **
!               ******************************************************
!
      ISTEPN='11'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REGR')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
              GO TO 9000
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
              GO TO 9000
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
            GO TO 9000
          ENDIF
        ENDIF
        NCSTR2=0
        DO 1187 I=128,1,-1
            NCSTR2=I
            IF(ISTRI2(I:I).NE.' ')GO TO 1189
 1187   CONTINUE
 1189   CONTINUE
!
!  FOR PIXMAP SPECIFIED BY FILE NAME, CHECK CURRENT LIST.  IF NOT
!  FOUND, ADD TO LIST.
!
      ELSE
        ICURPM=0
        IF(NUMPXM.GE.1)THEN
          DO 1191 I=1,NUMPXM
            IF(ISTRI2(1:128).EQ.IPXMFN(I)(1:128))THEN
              ICURPM=I
              GO TO 1199
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
 1161 FORMAT('***** ERROR IN DPREGR: THE SPECIFIED PIXMAP NUMBER (',I5,   &
      ') IS GREATER THAN ')
 1163 FORMAT('      THE NUMBER OF CURRENTLY DEFINED PIXMAPS (',I5,')')
 1171 FORMAT('***** ERROR IN DPREGR: YOU ASKED FOR (',I5,') PLOTS ',   &
      'AGO AND')
 1173 FORMAT('      THERE ARE ONLY (',I5,') PIXMAPS CURRENTLY SAVED.')
 1181 FORMAT('***** ERROR IN DPREGR: NO PIXMAP FILE NAME OR NUMBER ',   &
      'WAS SPECIFIED.')
 1183 FORMAT('      HOWEVER, THERE ARE CURRENTLY NO PIXMAPS SAVED.')
!
!               *******************************
!               **  STEP 12--                **
!               **  CALL XRESTG              **
!               *******************************
!
      ISTEPN='12'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REGR')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISUBN0='REGR'
      CALL DPINFI(ISTRI2,IEXIST,IOPEN,IACC,ISUBN0,IBUGS2,ISUBRO,IERROR)
      IF(IEXIST.NE.'YES'.OR.IERROR.EQ.'YES')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1203)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1204)ISTRI2(1:MIN(NCSTR2,80))
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
 1203 FORMAT('***** ERROR IN DPREGR--UNABLE TO OPEN THE REQUESTED ',   &
             'PLOT.')
 1204 FORMAT('      THE REQUESTED FILE IS: ',A80)
!
      IF(NCSTR2.GT.127)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1209)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
 1209 FORMAT('***** ERROR IN DPREGR--FILE NAME EXCEEDS 127 ',   &
      'CHARACTERS.')
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
!
      DO 1220 I=1,NCSTR2
        NCTEMP=NCTEMP+1
        CTEMP(NCTEMP:NCTEMP)=ISTRI2(I:I)
 1220 CONTINUE
!
!  AUGUST 1997.  GENERALIZE THIS ROUTINE FOR NON-X11 DEVICES.
!  CALL A LOWER LEVEL ROUTINE, MOVE FOLLOWING CODE TO THAT ROUTINE.
!
      ICODE='REST'
      CALL GRSAGR(ICODE,ISTRI2,NCSTR2,CTEMP,NCTEMP)
!CCCC DO1215I=1,NCTEMP
!CCCC   CALL DPCOAN(CTEMP(I:I),IADE2(I))
!1215 CONTINUE
!CCCC DO1220I=1,NCSTR2
!CCCC   CALL DPCOAN(ISTRI2(I:I),IADE(I))
!CCCC   CALL DPCOAN(ISTRI2(I:I),IADE2(I+NCTEMP))
!1220 CONTINUE
!CCCC IADE(NCSTR2+1)=0
!CCCC IADE2(NCSTR2+NCTEMP+1)=0
!
!CCCC IERR=0
!CCCC CALL XRESTG(IADE,IADE2,IERR)
!CCCC IF(IERR.EQ.1)THEN
!CCCC   WRITE(ICOUT,999)
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,1251)
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   IERROR='YES'
!CCCC   GO TO 9000
!1251 FORMAT('***** ERROR IN DPREGR--READING BIT MAP UNSUCCESSFUL.')
!CCCC ELSEIF(IERR.EQ.2)THEN
!CCCC   WRITE(ICOUT,999)
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,1261)
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   IERROR='YES'
!CCCC   GO TO 9000
!1261 FORMAT('***** ERROR IN DPREGR--NO CURRENT PIXMAP TO SAVE.')
!CCCC ELSEIF(IERR.EQ.3)THEN
!CCCC   WRITE(ICOUT,999)
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,1271)
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   IERROR='YES'
!CCCC   GO TO 9000
!1271 FORMAT('***** ERROR IN DPREGR--X11 HAS NOT BEEN OPENED.')
!CCCC ELSEIF(IERR.EQ.4)THEN
!CCCC   WRITE(ICOUT,999)
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,1281)
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   IERROR='YES'
!CCCC   GO TO 9000
!1281 FORMAT('***** ERROR IN DPREGR--X11 NOT INSTALLED ON THIS ',
!CCCC1'IMPLEMENTATION.')
!CCCC ELSEIF(IERR.EQ.5)THEN
!CCCC   WRITE(ICOUT,999)
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,1286)
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   IERROR='YES'
!CCCC   GO TO 9000
!1286 FORMAT('***** ERROR IN DPREGR--UNABLE TO OPEN NEW X11 WINDOW ')
!CCCC ELSE
!CCCC   WRITE(ICOUT,999)
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,1291)
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,1292)ISTRI2(1:NCSTR2)
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   IERROR='YES'
!CCCC   GO TO 9000
!1291 FORMAT('***** CURRENT PIXMAP SUCCESSFULLY COPIED FROM FILE ')
!1292 FORMAT('      ',A128)
!CCCC ENDIF
!
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'REGR')THEN
        WRITE(ICOUT,1293)ISTRI2(1:NCSTR2)
 1293   FORMAT('ISTRI2 = ',A128)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1294)NCSTR2
 1294   FORMAT('NCSTR2 = ',I4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPREGR
      SUBROUTINE DPREMA(IHARG,NUMARG,   &
                        IMACSC,IDEFMS,   &
                        IBUGS2,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE MACRO SUBSTITUTION CHARACTOR WHICH MAY
!              BE USED TO REPLACE A COMMAND LINE ARGUMENT TO
!              A MACRO.  FOR EXAMPLE
!
!                  CALL SAMPLE.DP BERGER1.DAT Y X
!
!              IN SAMPLE.DP, $1 WILL BE REPLACED BY BERGER1.DAT,
!              $2 WILL BE REPLACED BY Y, AND $3 WILL BE REPLACED
!              BY X.  THIS ROUTINE LETS YOU SPECIFY A CHARACTER
!              OTHER THAN  "$" TO SIGNIFY A COMMAND LINE ARGUMENT.
!              THE SPECIFIED MACRO SUBSTITUTION CHARACTOR WILL BE
!              PLACED IN THE CHARACTER VARIABLE IMACSC.
!     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
!                     --NUMARG (AN INTEGER VARIABLE)
!                     --IBASLC (A CHARACTER VARIABLE--BACKSLASH)
!                     --IBUGS2 (A  CHARACTER VARIABLE)
!     OUTPUT ARGUMENTS--IMACSC (A CHARACTER VARIABLE)
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
!     VERSION NUMBER--2005/9
!     ORIGINAL VERSION--SEPTEMBER  2005.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*1 IMACSC
      CHARACTER*1 IDEFMS
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHARG4
      CHARACTER*1 IHOLD
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
        DO 55 I=1,NUMARG
          WRITE(ICOUT,56)I,IHARG(I)
   56     FORMAT('I,IHARG(I) = ',I8,2X,A4)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
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
      IHOLD=IDEFMS
      GO TO 1180
!
 1160 CONTINUE
      IHARG4=IHARG(NUMARG)
      IHOLD=IHARG4(1:1)
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
      IMACSC=IHOLD
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1181)IMACSC
 1181   FORMAT('THE MACRO SUBSTITUTION CHARACTOR HAS BEEN SET TO ',   &
               A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
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
!
      RETURN
      END SUBROUTINE DPREMA
      SUBROUTINE DPREMO(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
      IANGLU,DEMOFR,DEMODF,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
!
!     PURPOSE--GENERATE A COMPLEX REMODULATION PLOT
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
!           MODIFIED, OR OTHERWISE USED IN A CONTEXT
!           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--86/6
!     ORIGINAL VERSION--MARCH     1986.
!     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE COMMON
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
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 ICASEQ
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
      ISUBN1='DPRE'
      ISUBN2='MO  '
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
!
!               ***********************************************
!               **  TREAT THE COMPLEX REMODULATION CASE      **
!               ***********************************************
!
      IF(IBUGG2.EQ.'OFF')GO TO 90
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
!
!               ***************************
!               **  STEP 1--             **
!               **  EXTRACT THE COMMAND  **
!               ***************************
!
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.GE.1.AND.   &
      ICOM.EQ.'REMO'.AND.IHARG(1).EQ.'PLOT')GO TO 110
!
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'COMP'.AND.IHARG(1).EQ.'REMO'.AND.IHARG(2).EQ.'PLOT')   &
      GO TO 120
!
      IFOUND='NO'
      GO TO 9000
!
  110 CONTINUE
      ICASPL='CR'
      ILASTC=1
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      GO TO 180
!
  120 CONTINUE
      ICASPL='CR'
      ILASTC=2
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      GO TO 180
!
  180 CONTINUE
      IFOUND='YES'
      GO TO 190
!
  190 CONTINUE
!
!               ***********************************************************
!               **  STEP 1--                                             **
!               **  CHECK FOR THE PROPER NUMBER OF INPUT ARGUMENTS.      **
!               ***********************************************************
!
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      MINNA=1
      MAXNA=100
      CALL CHECKA(NUMARG,MINNA,MAXNA,IANS,IWIDTH,ISUBN1,ISUBN2,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               ********************************************
!               **  STEP 2--                              **
!               **  CHECK THE VALIDITY OF ARGUMENT 1      **
!               **  (THIS WILL BE THE RESPONSE VARIABLE)  **
!               ********************************************
!
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
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
      IF(IBUGG2.EQ.'ON')WRITE(ICOUT,211)IHLEFT,ICOLL,NLEFT
  211 FORMAT('IHLEFT,ICOLL,NLEFT = ',A4,I8,I8)
      IF(IBUGG2.EQ.'ON')CALL DPWRST('XXX','BUG ')
!
!               ***************************************************************
!               **  STEP 3--                                                 **
!               **  CHECK THAT THE INPUT NUMBER OF OBSERVATIONS (NLEFT)      **
!               **  FOR THE RESPONSE VARIABLE IS POSITIVE.                   **
!               ***************************************************************
!
      ISTEPN='3'
      IF(IBUGG2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NLEFT.GE.MINN2)GO TO 390
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
      GO TO 9000
  390 CONTINUE
!
!               *****************************************
!               **  STEP 4--                           **
!               **  CHECK TO SEE THE TYPE SUBCASE      **
!               **  (BASED ON THE QUALIFIER)--         **
!               **    1) UNQUALIFIED (THAT IS, FULL);  **
!               **    2) SUBSET/EXCEPT; OR             **
!               **    3) FOR.                          **
!               *****************************************
!
      ISTEPN='4'
      IF(IBUGG2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICASEQ='FULL'
      ILOCQ=NUMARG+1
      IF(NUMARG.LT.1)GO TO 480
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
!
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
      GO TO 9000
!
  490 CONTINUE
      IF(IBUGG2.EQ.'OFF')GO TO 495
      WRITE(ICOUT,491)NUMARG,ILOCQ,ICASEQ
  491 FORMAT('NUMARG,ILOCQ,ICASEQ = ',I8,I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
  495 CONTINUE
!
!               *********************************************
!               **  STEP 5--                               **
!               **  CHECK FOR PROPER NUMBER OF VARIABLES.  **
!               **  FOR A COMPLEX REMODULATION PLOT,       **
!               **  THE PROPER NUMBER OF VARIABLES IS      **
!               **  EXACTLY 1.                             **
!               *********************************************
!
      ISTEPN='5'
      IF(IBUGG2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMV2=ILOCQ-1
      IF(NUMV2.EQ.1)GO TO 590
!
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
      GO TO 9000
!
  590 CONTINUE
!
!               *************************************************
!               **  STEP 6--                                   **
!               **  BRANCH TO THE APPROPRIATE SUBCASE;         **
!               **  (BASED ON THE QUALIFIER)                   **
!               **  THEN FORM THE RESPONSE VARIABLE            **
!               **  AND THE SECOND VARIABLE (IF EXISTENT)      **
!               *************************************************
!
      ISTEPN='6'
      IF(IBUGG2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICASEQ.EQ.'FULL')GO TO 610
      IF(ICASEQ.EQ.'SUBS')GO TO 620
      IF(ICASEQ.EQ.'FOR')GO TO 630
!
  610 CONTINUE
      DO 615 I=1,NLEFT
      ISUB(I)=1
  615 CONTINUE
      NQ=NLEFT
      GO TO 650
!
  620 CONTINUE
      NIOLD=NLEFT
      CALL DPSUBS(NIOLD,ILOCS,NS,IBUGQ,IERROR)
      NQ=NIOLD
      GO TO 650
!
  630 CONTINUE
      NIOLD=NLEFT
      CALL DPFOR(NIOLD,NFOR,IROW1,IROWN,   &
      NLOCAL,ILOCS,NS,IBUGQ,IERROR)
      NQ=NFOR
      GO TO 650
!
  650 CONTINUE
      J=0
      IMAX=NLEFT
      IF(NQ.LT.NLEFT)IMAX=NQ
      DO 660 I=1,IMAX
      IF(ISUB(I).EQ.0)GO TO 660
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
!
!               ***********************************************************
!               **  STEP 7--                                             **
!               **  DETERMINE IF THE ANALYST                             **
!               **  HAS SPECIFIED    THE DEMODULATION FREQUENCY          **
!               **  FOR THE COMPLEX DEMODULATION ANALYSIS.               **
!               **  THE FREQUENCY SETTING IS DEFINED BY PRE-USE          **
!               **  OF THE DEMODULATION FREQUENCY     COMMAND.           **
!               **  IF FOUND, USE THE SPECIFIED VALUE.                   **
!               **  IF NOT FOUND, GENERATE AN ERROR MESSAGE.             **
!               ***********************************************************
!
      ISTEPN='7'
      IF(IBUGG2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DEMOF2=DEMOFR
      IF(IANGLU.EQ.'DEGR')DEMOF2=DEMOF2*PI/180.0
      IF(IANGLU.EQ.'GRAD')DEMOF2=DEMOF2*PI/200.0
      IF(0.0.LT.DEMOF2.AND.DEMOF2.LT.0.5)GO TO 790
!
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
  750 FORMAT('       THE DEMODULATION FREQUENCY = ',E15.7,2X,   &
      'RADIANS')
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
      GO TO 9000
!
  790 CONTINUE
!
!               ****************************************************************
!               **  STEP 8--                                                   *
!               **  COMPUTE THE APPROPRIATE COMPLEX REMODULATION PLOT.         *
!               **  FORM THE VERTICAL AND HORIZONTAL AXIS                      *
!               **  VALUES Y(.) AND X(.) FOR THE PLOT.                         *
!               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).              *
!               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).              *
!               ****************************************************************
!
      ISTEPN='8'
      IF(IBUGG2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPREM2(Y1,NLEFT,ICASPL,DEMOF2,DEMODF,   &
      Y,X,D,NPLOTP,NPLOTV,IBUGG3,IERROR)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGG2.EQ.'OFF')GO TO 9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPREMO--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IFOUND,IERROR
 9012 FORMAT('IFOUND,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9013 FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',   &
      I8,I8,I8,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)DEMOFR,IANGLU,DEMOF2
 9014 FORMAT('DEMOFR,IANGLU,DEMOF2 = ',E15.7,2X,A4,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
      IF(NPLOTP.LE.0)GO TO 9090
      DO 9015 I=1,NPLOTP
      WRITE(ICOUT,9016)I,Y(I),X(I),D(I)
 9016 FORMAT('I,Y(I),X(I),D(I) = ',I8,3F12.5)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPREMO
      SUBROUTINE DPREM2(Y,N,ICASPL,F,DEMODF,   &
                        Y2,X2,D2,N2,NPLOTV,IBUGG3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE PERFORMS A COMPLEX REMODULATION
!              ON THE DATA IN THE INPUT VECTOR X
!              AT THE INPUT DEMODULATION FREQUENCY = F.
!              THE COMPLEX REMODULATION CONSISTS OF
!              DEMODULATING AT THE SPECIFIED FREQUENCY
!              AND THEN REMODULATING TO FORM A PLOT
!              IN THE ORIGINAL UNITS OF THE DATA WHICH
!              SHOWS THE CONTRIBUTION AT THAT FREQUENCY
!              TO THE ORIGINAL SERIES.  IT IS USEFUL
!              FOR FORMING A BAND-PASS FILTERED SERIES
!              AND (AFTER SUBTRACTION) A REJECTION-PASS
!              FILTERED SERIES.
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
!     PRINTING--YES.
!     RESTRICTIONS--THE MAXIMUM ALLOWABLE VALUE OF N
!                   FOR THIS SUBROUTINE IS 5000.
!                 --THE SAMPLE SIZE N MUST BE GREATER
!                   THAN OR EQUAL TO 3.
!                 --THE INPUT FREQUENCY F MUST BE
!                   GREATER THAN OR EQUAL TO 2/(N-2).
!                 --THE INPUT FREQUENCY F MUST BE
!                   SMALLER THAN 0.5.
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
!                 ESPECIALLY PAGES 174 AND 175.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
!           MODIFIED, OR OTHERWISE USED IN A CONTEXT
!           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--86/6
!     ORIGINAL VERSION--NOVEMBER  1972.
!     UPDATED         --JANUARY   1989.  PARAMETER STATEMENT MISPLACED (ALAN)
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASPL
      CHARACTER*4 IBUGG3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
!
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
      DATA PI/3.141592653/
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPRE'
      ISUBN2='2  '
      IERROR='NO'
!
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
      '(A VECTOR) TO THE DPREM2  SUBROUTINE HAS ALL ELEMENTS = ',   &
      G15.7)
   17 FORMAT('***** ERROR--THE SECOND ARGUMENT TO THE ',   &
      'DPREM2  SUBROUTINE IS OUTSIDE THE ALLOWABLE (',I6,',',I6,') ',   &
      'INTERVAL')
   27 FORMAT('***** ERROR--THE THIRD ARGUMENT TO THE ',   &
      'DPREM2  SUBROUTINE IS OUTSIDE THE ALLOWABLE (',I6,'0.5) ',   &
      'INTERVAL')
   28 FORMAT('                   THE ABOVE LOWER LIMIT (',F11.8,   &
      ') = 2/(N-2) WHERE N = THE INPUT SAMPLE SIZE = ',I8)
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
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
!               ***********************************************************
!               **  STEP 2--                                             **
!               **  FORM THE FIRST MOVING AVERAGE FOR THE COSINE SERIES  **
!               ***********************************************************
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
  500 CONTINUE
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
  900 CONTINUE
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
 1100 CONTINUE
      SUM=SUM+D2(I)/2.0+D2(IENDP1)/2.0
      X2(I)=SUM/ALEN2
 1000 CONTINUE
!
!               *****************************************
!               **  STEP 7--                           **
!               **  FORM THE REMODULATED SERIES        **
!               *****************************************
!
      IHALF=(LENMA1+LENMA2)/2
      ISTART=IHALF+1
      ISTOP=N-IHALF
!
!CCCC DO1450I=1,IMAX2
      DO 1450 I=1,N
      IF(I.LT.ISTART)GO TO 1410
      IF(I.GT.ISTOP)GO TO 1410
      GO TO 1420
!
 1410 CONTINUE
      Y2(I)=Y(I)
      X2(I)=I
      D2(I)=1.0
      GO TO 1450
!
 1420 CONTINUE
      AI=I
      TERM1=2.0*Y2(I)*SIN(2.0*PI*F*AI)
      TERM2=2.0*X2(I)*COS(2.0*PI*F*AI)
      Y2(I)=TERM1+TERM2
      X2(I)=I
      D2(I)=1.0
      GO TO 1450
!
 1450 CONTINUE
!CCCC N2=IMAX2
      N2=N
      NPLOTV=2
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGG3.EQ.'OFF')GO TO 9090
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
!
      RETURN
      END SUBROUTINE DPREM2
      SUBROUTINE DPREPC(IHARG,IARGT,ARG,NUMARG,DEFRPC,   &
      RECIPC,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE RECIPE PROBABILITY CONTENT
!              IN THE FLOATING POINT VARIABLE RECIPC.
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --IARGT  (A  HOLLERITH VECTOR)
!                     --ARG    (A  FLOATING POINT VECTOR)
!                     --NUMARG (AN INTEGER VARIABLE)
!                     --DEFRPC (A  FLOATING POINT VARIABLE)
!     OUTPUT ARGUMENTS--RECIPC  (A  FLOATING POINT VARIABLE)
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
!           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
!           MODIFIED, OR OTHERWISE USED IN A CONTEXT
!           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--97/8
!     ORIGINAL VERSION--AUGUST   1997.
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
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
!
      IF(NUMARG.EQ.0)GO TO 1199
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'PROB'.AND.IHARG(2).EQ.'CONT')   &
      GO TO 1110
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'=')GO TO 1110
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'=')GO TO 1110
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'CONT')GO TO 1110
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PROB')GO TO 1110
      GO TO 1199
!
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'CONT')GO TO 1150
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1150
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      IF(IARGT(NUMARG).EQ.'NUMB')GO TO 1160
      GO TO 1120
!
 1120 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,1121)
 1121 FORMAT('***** ERROR IN DPREPC--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR RECIPE PROBABILITY CONTENT ',   &
      'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1130)
 1130 FORMAT('      AN EXAMPLE OF THIS COMMAND IS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1131)
 1131 FORMAT('      RECIPE PROBABILITY CONTENT .90 ')
      CALL DPWRST('XXX','BUG ')
      GO TO 1199
!
 1150 CONTINUE
      HOLD=DEFRPC
      GO TO 1180
!
 1160 CONTINUE
      HOLD=ARG(NUMARG)
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
      IF(HOLD.GE.1.0 .AND. HOLD.LT.100.0)HOLD=HOLD/100.
      IF(HOLD.LE.0.0 .OR. HOLD.GE.1.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1182)
 1182 FORMAT('**** THE RECIPE PROBABILITY CONTENT MUST BE SET BETWEEN',   &
      ' 0 AND 1 EXCLUSIVE (TYPICALLY BETWEEN .9 AND .99)')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1183)HOLD
 1183 FORMAT('     THE VALUE ENTERED WAS ',E15.7)
        CALL DPWRST('XXX','BUG ')
        GO TO 1199
      ENDIF
      RECIPC=HOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)RECIPC
 1181 FORMAT('THE RECIPE PROBABILITY CONTENT HAS JUST BEEN SET TO ',   &
      E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 1199
!
 1199 CONTINUE
      RETURN
      END SUBROUTINE DPREPC
      SUBROUTINE DPREPL(ITEXHO,NUMTEC,   &
                        IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,   &
                        IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,   &
                        IBUGD2,IERROR)
!
!     PURPOSE--TRANSLATE A STRING AS DICTATED BY THE   VALU()   OPERATOR.
!              THAT IS, REPLACE ALL OCCURRANCES OF  XXXVALU()YYY
!              (WHERE XXX IS AN ARBITRARY STRING, AND
!              YYY IS A DATAPLOT PARAMETER NAME OR A FUNCTION NAME)
!              BY THE NUMERIC VALUE OF THE PARAMETER YYY, OR
!              THE FUNCTIONAL STRING IN THE FUNCTION YYY.
!              (E.G., IF PARAMETER K HAS THE VALUE 7,
!              THEN   YVALU()K   BECOMES   Y7
!              OR     IF THE FUNCTION K HAS THE CONTENTS XYZ,
!              THEN   YVALU()K   BECOMES   YABC  ).
!     NOTE--THIS SUBROUTINE CHANGES THE CONTENTS OF THE INPUT VECTOR ITEXHO()
!           AND THE INPUT VARIABLE NUMTEC.
!     NOTE--THIS SUBROUTINE IS SIMILAR TO (BUT NOT IDENTICAL TO) DPREP2.
!           SUBROUTINE DPREPL TRANSLATES THE VALU() OPERATOR.
!           SUBROUTINE DPREPL TRANSLATES THE \      OPERATOR.
!     NOTE--ALTHOUGH IREPCH IS AN INPUT ARGUMENT TO THIS SUBROUTINE,
!           IT IS NEVER USED HEREIN.
!           IT IS ALLOWED TO REMAIN AS AN INPUT ARGUMENT
!           ONLY TO MAINTAIN CONSISTENCY WITH SUBROUTINE DPREP2
!           (WHICH DOES USE IT).
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
!     ORIGINAL VERSION--MARCH      1983.
!     UPDATED         --DECEMBER  1986. STOP WITH "
!     UPDATED         --DECEMBER  1988. STOP WITH )
!     UPDATED         --DECEMBER  1988. STOP WITH & AND COLLAPSE
!     UPDATED         --MAY       1992. ADD 8 DELIMITERS FOR ^
!     UPDATED         --JULY      1992. ADD . AND ^ AS DELIMITERS
!     UPDATED         --DECEMBER  1993. ALLOW LOWER CASE: valu()
!     UPDATED         --DECEMBER  1993. ALLOW LOWER CASE PAR. NAME
!     UPDATED         --JULY      1995. COMMENT OUT 2 LINES
!     UPDATED         --AUGUST    2002. ADD "?" AS DELIMITER
!     UPDATED         --JUNE      2003. TREAT ANYTHING THAT IS NOT A
!                                       NUMBER OR LETTER OR UNDERSCORE
!                                       AS DELIMITER
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ITEXHO
      CHARACTER*4 IBUGD2
      CHARACTER*4 IERROR
!
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
!
      CHARACTER*4 IHNAME
      CHARACTER*4 IHNAM2
      CHARACTER*4 IUSE
!
      CHARACTER*4 IFUNC
!
      CHARACTER*1 IREPCH
!
      CHARACTER*4 IAMPER
!
!---------------------------------------------------------------------
!
      DIMENSION ITEXHO(*)
!CCCC DIMENSION ISTR(20) JAN 1987--PROBLEMS WITH \ AND LONG TITLES
      DIMENSION ISTR(200)
!CCCC DIMENSION IH(20)   JAN 1987--PROBLEMS WITH \ AND LONG TITLES
      DIMENSION IH(200)
!
      DIMENSION IHNAME(*)
      DIMENSION IHNAM2(*)
      DIMENSION IUSE(*)
      DIMENSION IVALUE(*)
      DIMENSION VALUE(*)
!
      DIMENSION IVSTAR(*)
      DIMENSION IVSTOP(*)
      DIMENSION IFUNC(*)
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
      NUMCHN=0
!
      ILOC1=0
      ILOC2=0
      ILOC3=0
      I2=0
      IHNAP1='-999'
      IHNAP2='-999'
      IUS='-999'
!
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
!
!               *****************************************************
!               **  STEP 10--                                      **
!               **  LOOP THROUGH (AT MOST) 100 PASSES.  EACH PASS  **
!               **  SEARCHES FOR THE NEXT OCCURRANCE OF VALU().  A **
!               **  GIVEN PASS WIPES OUT VALU()XX AND REPLACES IT  **
!               **  WITH THE NUMERIC VALUE OF PARAMETER XX.  NOTE  **
!               **  THAT EACH PASS CHANGES THE CONTENTS OF INOUT   **
!               **  VARIABLE ITEXHO() AND INPUT VALUE NUMTEC.      **
!               *****************************************************
!
      IFOUNG='NO'
      DO 1000 IPASS=1,100
!
!               ****************************************************
!               **  STEP 11--                                     **
!               **  FOR THIS PASS,                                **
!               **  SEARCH THE STRING FOR THE NEXT OCCURRANCE OF  **
!               **  THE SUBSTRING VALU()                          **
!               **  PROCEED RIGHT TO LEFT (DEC. 1986).            **
!               **  IF FOUND, THEN PROCEED FURTHER.               **
!               **  IF NOT FOUND, THEN EXIT.                      **
!               ****************************************************
!
        IFOUNV='NO'
        IF(NUMTEC.LE.0)GO TO 9000
!
        NUMTM5=NUMTEC-5
        IF(NUMTM5.GT.0)THEN
          DO 1110 IDUMMY=1,NUMTM5
            I=NUMTM5-IDUMMY+1
!
            IP1=I+1
            IP2=I+2
            IP3=I+3
            IP4=I+4
            IP5=I+5
!
            IF(ITEXHO(I).NE.'V'.AND.ITEXHO(I).NE.'v')GO TO 1110
            IF(ITEXHO(IP1).NE.'A'.AND.ITEXHO(IP1).NE.'a')GO TO 1110
            IF(ITEXHO(IP2).NE.'L'.AND.ITEXHO(IP2).NE.'l')GO TO 1110
            IF(ITEXHO(IP3).NE.'U'.AND.ITEXHO(IP3).NE.'u')GO TO 1110
            IF(ITEXHO(IP4).NE.'(')GO TO 1110
            IF(ITEXHO(IP5).NE.')')GO TO 1110
!
            IFOUNV='YES'
            IFOUNG='YES'
            ILOC1=I
            ILOC2=IP5
            GO TO 1190
!
 1110     CONTINUE
        ENDIF
        GO TO 9000
!
 1190   CONTINUE
!
!               ****************************************************
!               **  STEP 12--                                     **
!               **  EXTRACT THE PARAMETER OR FUNCTION NAME.  THIS **
!               **  WILL BE THE STRING IMMEDIATELY FOLLOWING ()   **
!               **  UNTIL A BLANK IS FOUND                        **
!               **  OR UNTIL A    "   IS FOUND (DEC. 1986)        **
!               **  OR UNTIL A    )   IS FOUND (DEC. 1988)        **
!               **  OR UNTIL A    &   IS FOUND (DEC. 1988)        **
!               ****************************************************
!
        DO 1210 I=1,8
          ISTR(I)=' '
 1210   CONTINUE
!
        IAMPER='NO'
!
        IMIN=ILOC2+1
        IMAX=IMIN+7
        IF(IMAX.GT.NUMTEC)IMAX=NUMTEC
        J=0
!
!  JUNE 2003.  BASICALLY, A DATAPLOT NAME CONSISTS OF NUMBERS OR
!              ALPABETIC CHARACTERS OR UNDERSCORE.  ANYTHING ELSE
!              SHOULD TERMINATE THE NAME.
!
        DO 1250 I=IMIN,IMAX
          I2=I
          ITEMP=ICHAR(ITEXHO(I)(1:1))
          IF(ITEMP.LT.48 .OR. ITEMP.GT.122 .OR.   &
             (ITEMP.GT.57 .AND. ITEMP.LT.65) .OR.   &
             (ITEMP.GT.90 .AND. ITEMP.LT.97 .AND. ITEMP.NE.95)   &
             )THEN
             ILOC3=I2-1
             IF(ITEMP.EQ.38)IAMPER='YES'
             GO TO 1290
          ENDIF
          J=J+1
          ISTR(J)=ITEXHO(I)
 1250   CONTINUE
        ILOC3=I2
!
 1290   CONTINUE
        NUMCHN=J
        IF(IBUGD2.EQ.'ON')THEN
          WRITE(ICOUT,1291)ILOC1,ILOC2,ILOC3,IMIN,IMAX,NUMCHN
 1291     FORMAT('ILOC1,ILOC2,ILOC3,IMIN,IMAX,NUMCHN = ',6I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
!               ****************************************************
!               **  STEP 13--                                     **
!               **  PACK THE PARAMETER/FUNCTION NAME STRING INTO  **
!               **  2 4-BYTE WORDS.                               **
!               ****************************************************
!
        IWORD1='    '
        IWORD2='    '
        NUMASC=4
        IMAX=2*NUMASC
        IF(NUMCHN.LE.0)GO TO 1390
        IF(NUMCHN.LT.IMAX)IMAX=NUMCHN
!
        IF(IBUGD2.EQ.'ON')THEN
          WRITE(ICOUT,1301)IMAX
 1301     FORMAT('IMAX = ',I6)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        DO 1300 I=1,IMAX
          IF(ISTR(I).EQ.' ')GO TO 1390
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
!
        DO 1395 I=1,4
          IH1=IHNAP3(I:I)
          INH1=ICHAR(IH1)
          IF(97.LE.INH1.AND.INH1.LE.122)IH1=CHAR(INH1-32)
          IHNAP3(I:I)=IH1
          IH1=IHNAP4(I:I)
          INH1=ICHAR(IH1)
          IF(97.LE.INH1.AND.INH1.LE.122)IH1=CHAR(INH1-32)
          IHNAP4(I:I)=IH1
 1395   CONTINUE
!
!               ****************************************
!               **  STEP 15--                         **
!               **  DETERMINE IF THE NAME IS IN THE   **
!               **  INTERNAL DATAPLOT NAME LIST,      **
!               **  AND AS A PARAMETER OR FUNCTION.   **
!               ****************************************
!
        IF(NUMNAM.GE.1)THEN
          DO 1500 I=1,NUMNAM
            I2=I
            IF(IHNAP1.EQ.IHNAME(I).AND.IHNAP2.EQ.IHNAM2(I))GO TO 1550
            IF(IHNAP3.EQ.IHNAME(I).AND.IHNAP4.EQ.IHNAM2(I))GO TO 1550
 1500     CONTINUE
!
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
          GO TO 9000
!
 1550     CONTINUE
          IVAL=IVALUE(I2)
          VAL=VALUE(I2)
          IUS=IUSE(I2)
          IL1=IVSTAR(I2)
          IL2=IVSTOP(I2)
!
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
            GO TO 9000
          ENDIF
!
        ENDIF
!
!               ************************************************
!               **  STEP 16--                                 **
!               **  FOR THE CASE WHEN HAVE A PARAMETER NAME,  **
!               **  DETERMINE THE LITERAL STRING ASSOCIATED   **
!               **  WITH THE PARAMETER VALUE.                 **
!               ************************************************
!
        IF(IUS.EQ.'P')THEN
          CALL DPCONH(IVAL,VAL,IH,NH,IBUGD2,IERROR)
        ELSEIF(IUS.EQ.'F')THEN
          CALL DPCOFH(IL1,IL2,IFUNC,NUMCHF,IH,NH,IBUGD2,IERROR)
        ENDIF
!
!               ****************************************************
!               **  STEP 21--                                     **
!               **  COLLAPSE THE SUBSTRING VALU() FOLLOWED BY     **
!               **  THE PARAMETER/FUNCTION NAME TO A NULL STRING. **
!               ****************************************************
!
        ILOC1M=ILOC1-1
        ILOC3P=ILOC3+1
        J=ILOC1M
        IF(NUMTEC.GE.ILOC3P)THEN
          DO 2100 I=ILOC3P,NUMTEC
            J=J+1
            ITEXHO(J)=ITEXHO(I)
 2100     CONTINUE
        ENDIF
        NUMTE2=J
!
!               ****************************************************
!               **  STEP 22--                                     **
!               **  INSERT THE LITERAL VALUE                      **
!               **  AT THE PROPER PLACE IN THE COLLAPSED STRING.  **
!               ****************************************************
!
        J=ILOC1M
        IF(NUMTE2.GE.ILOC1)THEN
          DO 2200 I=ILOC1,NUMTE2
            IREV=NUMTE2-I+ILOC1
            IREVNH=IREV+NH
            ITEXHO(IREVNH)=ITEXHO(IREV)
 2200     CONTINUE
        ENDIF
!
        IF(NH.GE.1)THEN
          DO 2300 I=1,NH
            J=ILOC1M+I
            ITEXHO(J)=IH(I)
 2300     CONTINUE
        ENDIF
        NUMTE3=NUMTE2+NH
        NUMTEC=NUMTE3
!
!               ****************************************************
!               **  STEP 24--                                     **
!               **  IF THE TERMINATOR WAS &,                      **
!               **  THEN COLLAPSE & TO A NULL STRING.             **
!               **  (THUS & SERVES AS A USEFUL CONCATONATION      **
!               **  CHARACTER.                                    **
!               **  (DECEMBER 1988)                               **
!               ****************************************************
!
        IF(IAMPER.EQ.'YES')THEN
          ILOC4=ILOC1+NH
          ILOC4M=ILOC4-1
          ILOC4P=ILOC4+1
          J=ILOC4M
          IF(NUMTEC.GE.ILOC4P)THEN
            DO 2420 I=ILOC4P,NUMTEC
              J=J+1
              ITEXHO(J)=ITEXHO(I)
 2420     CONTINUE
            NUMTEC=J
          ENDIF
        ENDIF
!
 1000 CONTINUE
!
!               ****************
!               **  STEP 90-- **
!               **  EXIT      **
!               ****************
!
 9000 CONTINUE
!
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
!
      RETURN
      END SUBROUTINE DPREPL
      SUBROUTINE DPREP2(ITEXHO,NUMTEC,   &
                        IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,   &
                        IVARLB,IROWLB,MAXNXT,   &
                        IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,IMALEV,   &
                        IBUGD2,ISUBRO,IERROR)
!
!     PURPOSE--TRANSLATE A STRING AS DICTATED BY THE   ^   OPERATOR.
!              THAT IS, REPLACE ALL OCCURRANCES OF  XXX^YYY
!              (WHERE XXX IS AN ARBITRARY STRING, AND
!              YYY IS A DATAPLOT PARAMETER NAME OR A FUNCTION NAME)
!              BY THE NUMERIC VALUE OF THE PARAMETER YYY, OR
!              THE FUNCTIONAL STRING IN THE FUNCTION YYY.
!              (E.G., IF PARAMETER K HAS THE VALUE 7,
!              THEN   Y^K   BECOMES   Y7
!              OR     IF THE FUNCTION K HAS THE CONTENTS XYZ,
!              THEN   Y^K   BECOMES   YABC  ).
!     NOTE--THIS SUBROUTINE CHANGES THE CONTENTS OF THE INPUT VECTOR ITEXHO()
!           AND THE INPUT VARIABLE NUMTEC.
!     NOTE--THIS SUBROUTINE IS SIMILAR TO (BUT NOT IDENTICAL TO) DPREPL.
!           SUBROUTINE DPREP2 TRANSLATES THE ^      OPERATOR.
!           SUBROUTINE DPREPL TRANSLATES THE VALU() OPERATOR.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--87/1
!     ORIGINAL VERSION--DECEMBER  1986.
!     UPDATED         --DECEMBER  1986. STOP WITH "
!     UPDATED         --JUNE      1987.
!     UPDATED         --DECEMBER  1988. STOP WITH )
!     UPDATED         --DECEMBER  1988. STOP WITH & AND COLLAPSE
!     UPDATED         --OCTOBER   1991. SIMPLIFY A SECTION (ALAN)
!     UPDATED         --APRIL     1992. FIX ^ LOWER CASE CONVERSION
!     UPDATED         --APRIL     1992. FIX DEBUG STATMENT
!     UPDATED         --MAY       1992. ADD 8 DELIMITERS FOR ^
!     UPDATED         --JULY      1992. ADD . AND ^ AS DELIMITERS
!     UPDATED         --AUGUST    1992. NON-EXISTENT K: ^K ==> BLANK
!     UPDATED         --OCTOBER   1993. TOP WITH (
!     UPDATED         --JANUARY   2000. REPLACE VARIABLE NAME WITH
!                                       VARIABLE LABEL.
!     UPDATED         --AUGUST    2002. ADD "?" AS DELIMITER
!     UPDATED         --JUNE      2003. TREAT ANYTHING THAT IS NOT A
!                                       NUMBER OR LETTER AS DELIMITER
!     UPDATED         --FEBRUARY  2005. CASE OF "&"
!     UPDATED         --SEPTEMBER 2005. SUPPORT ARGUMENTS TO MACROS
!                                       ($1, $2, ETC.)
!     UPDATED         --SEPTEMBER 2007. SUPPORT ^ROWLABEL^K SYNTAX
!                                       (I.E., EXTRACT A ROW LABEL)
!     UPDATED         --OCTOBER   2016. SUPPORT NAMED ARGUMENTS
!                                       ($Y, $TITLE, ETC.)
!     UPDATED         --OCTOBER   2016. INCREASE NUMBER OF ORDERED
!                                       ARGUMENTS FROM 10 TO 20
!     UPDATED         --MAY       2018. $99 TO RETRIEVE THE FULL CALL
!                                       COMMAND CONTAINING THE ARGUMENTS
!     UPDATED         --JANUARY   2019. OPTION TO SUPPRESS COMMAND
!                                       (^) SUBSTITUTION
!     UPDATED         --JANUARY   2019. OPTION TO SPECIFY FORTRAN
!                                       FORMAT FOR SUBSTITUTION
!     UPDATED         --DECEMBER  2020. BUG FIX FOR NAMED COMMAND
!                                       LINE ARGUMENTS
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ITEXHO
      CHARACTER*4 IBUGD2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      INCLUDE 'DPCOPA.INC'
!
      CHARACTER*4 ISTR
      CHARACTER*4 IH
      CHARACTER*4 IFOUNV
      CHARACTER*4 IFOUNG
      CHARACTER*4 IWORD1
      CHARACTER*4 IWORD2
      CHARACTER*4 IHNAP1
      CHARACTER*4 IHNAP2
      CHARACTER*4 IUS
!
      CHARACTER*4 IROWFL
      CHARACTER*4 IHNAME
      CHARACTER*4 IHNAM2
      CHARACTER*4 IUSE
!
      CHARACTER*40 IVARLB(*)
      CHARACTER*40 ILABT
!CCCC CHARACTER*255 ITTEMP
      CHARACTER (LEN=MAXSTR) :: ITTEMP
      CHARACTER*4  IFUNC
      CHARACTER*24 IROWLB(*)
!
      CHARACTER*1 IREPCH
      CHARACTER*4 IJUNK1
      CHARACTER*4 IJUNK2
      CHARACTER*1 IC1
      CHARACTER*1 IC2
      CHARACTER*2 IAJUNK
      CHARACTER*4 IAMPER
      CHARACTER*10 IFORMT
      CHARACTER*20 IVALXX
!
!-------------------------------------------------------------------
!
      DIMENSION ITEXHO(*)
      DIMENSION ISTR(200)
      DIMENSION IH(200)
!
      DIMENSION IHNAME(*)
      DIMENSION IHNAM2(*)
      DIMENSION IUSE(*)
      DIMENSION IVALUE(*)
      DIMENSION VALUE(*)
!
      DIMENSION IVSTAR(*)
      DIMENSION IVSTOP(*)
      DIMENSION IFUNC(*)
!
!-----COMMON VARIABLES (GENERAL)------------------------------------
!
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT---------------------------------------------------
!
      IERROR='NO'
      NUMCHN=0
!
      ILOC1=0
      ILOC2=0
      ILOC3=0
      I2=0
      IHNAP1='-999'
      IHNAP2='-999'
      IUS='-999'
      ILAST=0
      II=0
!
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
   55   FORMAT('NUMTEC,IMALEV,NMACAG,NMACLA,ITABNC,MAXNXT = ',   &
               5I5,I10)
        CALL DPWRST('XXX','BUG ')
        IF(NUMCHF.GT.0)THEN
          DO 58 I=1,NUMCHF
            WRITE(ICOUT,59)I,IFUNC(I)
   59       FORMAT('I,IFUNC(I) = ',I8,A1)
            CALL DPWRST('XXX','BUG ')
   58     CONTINUE
        ENDIF
        IF(NMACLA.GT.0)THEN
          DO 61 I=1,NMACLA
            WRITE(ICOUT,63)I,IMACLL(I),IMACNC(I),IMACLA(I)
   63       FORMAT('I,IMACLL(I),IMACNC(I),IMACLA(I) = ',3I8,2X,A80)
            CALL DPWRST('XXX','BUG ')
   61     CONTINUE
        ENDIF
      ENDIF
!
!               ****************************************************
!               **  STEP 10--                                     **
!               **  LOOP THROUGH (AT MOST) 100 PASSES.  EACH PASS **
!               **  SEARCHES FOR THE NEXT OCCURRANCE OF ^.  A     **
!               **  GIVEN PASS WIPES OUT ^XX AND REPLACES IT WITH **
!               **  THE NUMERIC VALUE OF PARAMETER XX OR THE      **
!               **  STRING CONTENTS VALUE OF FUNCTION XX.  NOTE   **
!               **  THAT EACH PASS CHANGES THE CONTENTS OF INPUT  **
!               **  VARIABLE ITEXHO() AND INPUT VALUE NUMTEC.     **
!               ****************************************************
!
!       2019/01: OPTION TO SUPPRESS THIS STEP
!
      IFOUNG='NO'
      IF(ISUBSK.EQ.'OFF')GO TO 1009
      DO 1000 IPASS=1,100
!
!               ****************************************************
!               **  STEP 11--                                     **
!               **  FOR THIS PASS,                                **
!               **  SEARCH THE STRING FOR THE NEXT OCCURRANCE OF  **
!               **  THE SUBSTRING IN IREPCH (USUALLY ^ )          **
!               **  PROCEED RIGHT TO LEFT (DEC. 1986).            **
!               **  IF FOUND, THEN PROCEED FURTHER.               **
!               **  IF NOT FOUND, THEN EXIT.                      **
!               ****************************************************
!
        IFOUNV='NO'
!
        IF(NUMTEC.GT.0)THEN
          DO 1120 IDUMMY=1,NUMTEC
            I=NUMTEC-IDUMMY+1
            IF(ITEXHO(I).EQ.IREPCH)THEN
              IFOUNV='YES'
              IFOUNG='YES'
              ILOC1=I
              ILOC2=I
              GO TO 1190
            ENDIF
 1120     CONTINUE
          GO TO 2500
        ELSE
          GO TO 9000
        ENDIF
!
 1190   CONTINUE
!
!               *****************************************************
!               **  STEP 12--                                      **
!               **  EXTRACT THE PARAMETER OR FUNCTION NAME.        **
!               **  THIS WILL BE THE STRING IMMEDIATELY FOLLOWING  **
!               **  ^ UNTIL A BLANK IS FOUND                       **
!               **  OR UNTIL A    "   IS FOUND (DEC. 1986)         **
!               **  OR UNTIL A    )   IS FOUND (DEC. 1988)         **
!               **  OR UNTIL A    &   IS FOUND (DEC. 1988)         **
!               *****************************************************
!
!       SEPTEMBER 2007.  CHECK FOR ^ROWLABEL^K TYPE SYNTAX.  THIS
!                        WILL SUBSTITUTE THE K-TH ROW LABEL.
!
        DO 1210 I=1,8
          ISTR(I)=' '
 1210   CONTINUE
!
        IAMPER='NO'
!
        IMIN=ILOC2+1
!CCCC   IMAX=IMIN+7
        IMAX=IMIN+8
        IF(IMAX.GT.NUMTEC)IMAX=NUMTEC
        J=0
!
!  JUNE 2003.  BASICALLY, A DATAPLOT NAME CONSISTS OF NUMBERS OR
!              ALPABETIC CHARACTERS OR UDERSCORES.  ANYTHING ELSE
!              SHOULD TERMINATE THE NAME.
!
        DO 1250 I=IMIN,IMAX
          I2=I
          ITEMP=ICHAR(ITEXHO(I)(1:1))
!
          IF(IBUGD2.EQ.'ON' .OR. ISUBRO.EQ.'REP2')THEN
            WRITE(ICOUT,1251)I,IPASS,ITEMP,ITEXHO(I)
 1251       FORMAT('AT 1250: I,IPASS,ITEMP,ITEXHO(I) = ',3I8,2X,A4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
          IF(ITEMP.LT.48 .OR. ITEMP.GT.122 .OR.   &
             (ITEMP.GT.57 .AND. ITEMP.LT.65) .OR.   &
             (ITEMP.GT.90 .AND. ITEMP.LT.97 .AND. ITEMP.NE.95)   &
             )THEN
             ILOC3=I2-1
             IF(ITEMP.EQ.38)IAMPER='YES'
             GO TO 1290
          ENDIF
          J=J+1
          ISTR(J)=ITEXHO(I)
 1250   CONTINUE
        ILOC3=I2
!
 1290   CONTINUE
        NUMCHN=J
!
        IF(IBUGD2.EQ.'ON' .OR. ISUBRO.EQ.'REP2')THEN
          WRITE(ICOUT,1291)ILOC1,ILOC2,ILOC3,IAMP
 1291     FORMAT('ILOC1,ILOC2,ILOC3,IAMP = ',3I8,2X,A4)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1292)IMIN,IMAX,NUMCHN
 1292     FORMAT('IMIN,IMAX,NUMCHN = ',3I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
!               ****************************************************
!               **  STEP 13--                                     **
!               **  PACK THE PARAMETER/FUNCTION NAME STRING INTO  **
!               **  2 4-BYTE WORDS.                               **
!               ****************************************************
!
        IWORD1='    '
        IWORD2='    '
        DO 1310 I=1,4
          IF(I.LE.NUMCHN)IWORD1(I:I)=ISTR(I)(1:1)
          IF(I+4.LE.NUMCHN)IWORD2(I:I)=ISTR(I+4)(1:1)
1310    CONTINUE
!
        IF(IBUGD2.EQ.'ON' .OR. ISUBRO.EQ.'REP2')THEN
          WRITE(ICOUT,1302)IWORD1,IWORD2
1302      FORMAT('IWORD1,IWORD2=',2A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        IHNAP1=IWORD1
        IHNAP2=IWORD2
!
!               ****************************************************
!               **  STEP 14--                                     **
!               **  CONVERT THE 2 4-BYTE WORDS INTO UPPER CASE.   **
!               **  (JUNE 1987)                                   **
!               ****************************************************
!
        IJUNK1=IHNAP1
        IJUNK2=IHNAP2
        CALL DPUPP4(IJUNK1,IJUNK1,IBUGD2,IERROR)
        CALL DPUPP4(IJUNK2,IJUNK2,IBUGD2,IERROR)
        IHNAP1=IJUNK1
        IHNAP2=IJUNK2
!
!               ****************************************
!               **  STEP 15--                         **
!               **  DETERMINE IF THE NAME IS IN THE   **
!               **  INTERNAL DATAPLOT NAME LIST,      **
!               **  AND AS A PARAMETER OR FUNCTION.   **
!               ****************************************
!
        IF(NUMNAM.GT.0)THEN
          DO 1500 I=1,NUMNAM
            I2=I
            IF(IHNAP1.EQ.IHNAME(I).AND.IHNAP2.EQ.IHNAM2(I))GO TO 1550
 1500     CONTINUE
!
          NH=1
          IH(1)='    '
          GO TO 2100
!
 1550     CONTINUE
          IVAL=IVALUE(I2)
          VAL=VALUE(I2)
          IUS=IUSE(I2)
          IL1=IVSTAR(I2)
          IL2=IVSTOP(I2)
!
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
!
          ILABT=' '
          IF(IVAL.GT.0.AND.IUS.EQ.'V')ILABT(1:40)=IVARLB(IVAL)(1:40)
!
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
            GO TO 9000
          ENDIF
!
        ENDIF
!
!               ************************************************
!               **  STEP 16--                                 **
!               **  FOR THE CASE WHEN HAVE A PARAMETER NAME,  **
!               **  DETERMINE THE LITERAL STRING ASSOCIATED   **
!               **  WITH THE PARAMETER VALUE.                 **
!               ************************************************
!
!CCCCC  SEPTEMBER 2007: CHECK FOR ROWLABEL:
!
        IF(IUS.EQ.'P')THEN
          IROWFL='OFF'
          IF(ILOC1.GE.9 .AND. IVAL.GT.0 .AND. IVAL.LE.MAXNXT)THEN
            IF((ITEXHO(ILOC1-8)(1:1).EQ.'R' .OR.   &
                ITEXHO(ILOC1-8)(1:1).EQ.'r') .AND.   &
               (ITEXHO(ILOC1-7)(1:1).EQ.'O' .OR.   &
                ITEXHO(ILOC1-7)(1:1).EQ.'o') .AND.   &
               (ITEXHO(ILOC1-6)(1:1).EQ.'W' .OR.   &
                ITEXHO(ILOC1-6)(1:1).EQ.'w') .AND.   &
               (ITEXHO(ILOC1-5)(1:1).EQ.'L' .OR.   &
                ITEXHO(ILOC1-5)(1:1).EQ.'l') .AND.   &
               (ITEXHO(ILOC1-4)(1:1).EQ.'A' .OR.   &
                ITEXHO(ILOC1-4)(1:1).EQ.'a') .AND.   &
               (ITEXHO(ILOC1-3)(1:1).EQ.'B' .OR.   &
                ITEXHO(ILOC1-3)(1:1).EQ.'b') .AND.   &
               (ITEXHO(ILOC1-2)(1:1).EQ.'E' .OR.   &
                ITEXHO(ILOC1-2)(1:1).EQ.'e') .AND.   &
               (ITEXHO(ILOC1-1)(1:1).EQ.'L' .OR.   &
                ITEXHO(ILOC1-1)(1:1).EQ.'l'))THEN
              IROWFL='YES'
              ILOC1=ILOC1-9
            ENDIF
          ENDIF
          IF(IROWFL.EQ.'ON')THEN
            ILABT=' '
            ILABT(1:24)=IROWLB(IVAL)(1:24)
            NH=24
            DO 1710 I=24,1,-1
              IF(ILABT(I:I).NE.' ')THEN
                NH=I
                GO TO 1719
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
              DO 1730 I=1,NH
                IH(I)=' '
                IH(I)(1:1)=ILABT(I:I)
 1730         CONTINUE
            ENDIF
          ELSE
!
!           2019/01: USE FORTRAN FORMAT STATEMENT FOR THE
!                    PARAMETER SUBSTITUTION
!
            IF(ISUBFM.NE.'NULL')THEN
              IFORMT=' '
              IFORMT(1:8)=ISUBFM(1:8)
              IF(IFORMT(1:1).NE.'(')THEN
                IFORMT(2:9)=IFORMT(1:8)
                IFORMT(1:1)='('
              ENDIF
              DO 1722 II=10,2,-1
                IF(IFORMT(II:II).NE.' ')THEN
                  ILAST=II
                  GO TO 1723
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
              DO 1731 II=1,20
                IF(IVALXX(II:II).NE.' ')THEN
                  IFRST=II
                  GO TO 1732
                ENDIF
 1731         CONTINUE
 1732         CONTINUE
              ILAST=20
              DO 1733 II=20,1,-1
                IF(IVALXX(II:II).NE.' ')THEN
                  ILAST=II
                  GO TO 1734
                ENDIF
 1733         CONTINUE
 1734         CONTINUE
              IF(ILAST.LT.IFRST)GO TO 1736
              NCH=ILAST-IFRST+1
              IVALXX(1:NCH)=IVALXX(IFRST:ILAST)
              IF(NCH.LT.20)IVALXX(NCH+1:20)=' '
              NH=0
              DO 1735 II=1,NCH
                NH=NH+1
                IH(NH)=' '
                IH(NH)(1:1)=IVALXX(II:II)
 1735         CONTINUE
              GO TO 1739
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
          DO 1610 I=52,1,-1
            IF(ILABT(I:I).NE.' ')THEN
              NH=I
              GO TO 1619
            ENDIF
 1610     CONTINUE
          NH=0
 1619     CONTINUE
          IF(NH.EQ.0)THEN
            DO 1620 I=1,4
              IH(I)=' '
              IH(I+4)=' '
              IH(I)(1:1)=IHNAP1(I:I)
              IH(I+4)(1:1)=IHNAP2(I:I)
 1620       CONTINUE
            NH=8
            DO 1625 I=8,1,-1
              IF(IH(I).NE.' ')THEN
                NH=I
                GO TO 1629
              ENDIF
 1625       CONTINUE
 1629       CONTINUE
          ELSE
            DO 1630 I=1,NH
              IH(I)=' '
              IH(I)(1:1)=ILABT(I:I)
 1630       CONTINUE
          ENDIF
        ENDIF
!
!               ****************************************************
!               **  STEP 21--                                     **
!               **  COLLAPSE THE SUBSTRING ^ FOLLOWED BY          **
!               **  THE PARAMETER/FUNCTION NAME TO A NULL STRING. **
!               ****************************************************
!
 2100   CONTINUE
        ILOC1M=ILOC1-1
        ILOC3P=ILOC3+1
        J=ILOC1M
!
        IF(IBUGD2.EQ.'ON' .OR. ISUBRO.EQ.'REP2')THEN
          WRITE(ICOUT,2102)ILOC1M,ILOC3P,NUMTEC
2102      FORMAT('AT 2100: ILOC1M,ILOC3P,NUMTEC=',3I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        IF(NUMTEC.GE.ILOC3P)THEN
          DO 2110 I=ILOC3P,NUMTEC
            J=J+1
            ITEXHO(J)=ITEXHO(I)
!
            IF(IBUGD2.EQ.'ON' .OR. ISUBRO.EQ.'REP2')THEN
               WRITE(ICOUT,2112)I,J,ITEXHO(J),ITEXHO(I)
2112           FORMAT('AT 2110: I,J,ITEXHO(I),ITEXHO(J)=',2I8,2(2X,A4))
               CALL DPWRST('XXX','BUG ')
            ENDIF
!
 2110     CONTINUE
        ENDIF
        NUMTE2=J
!
!               ****************************************************
!               **  STEP 22--                                     **
!               **  INSERT THE LITERAL VALUE                      **
!               **  AT THE PROPER PLACE IN THE COLLAPSED STRING.  **
!               ****************************************************
!
        J=ILOC1M
        IF(NUMTE2.GE.ILOC1)THEN
          DO 2200 I=ILOC1,NUMTE2
            IREV=NUMTE2-I+ILOC1
            IREVNH=IREV+NH
            ITEXHO(IREVNH)=ITEXHO(IREV)
!
            IF(IBUGD2.EQ.'ON' .OR. ISUBRO.EQ.'REP2')THEN
               WRITE(ICOUT,2202)I,IREV,IREVNH,   &
                                ITEXHO(IREV),ITEXHO(IREVNH)
 2202          FORMAT('AT 2200: I,IREV,IREVNH,ITEXHO(IREV),',   &
                      'ITEXHO(J)=',3I8,2(2X,A4))
               CALL DPWRST('XXX','BUG ')
            ENDIF
!
 2200     CONTINUE
        ENDIF
!
        IF(NH.GE.1)THEN
          DO 2300 I=1,NH
            J=ILOC1M+I
            ITEXHO(J)=IH(I)
!
            IF(IBUGD2.EQ.'ON' .OR. ISUBRO.EQ.'REP2')THEN
               WRITE(ICOUT,2302)I,J,IH(I),ITEXHO(J)
 2302          FORMAT('AT 2302: I,J,IH(I),ITEXHO(J),',2I8,2(2X,A4))
               CALL DPWRST('XXX','BUG ')
            ENDIF
!
 2300     CONTINUE
        ENDIF
        NUMTE3=NUMTE2+NH
        NUMTEC=NUMTE3
!
!               ****************************************************
!               **  STEP 24--                                     **
!               **  IF THE TERMINATOR WAS &,                      **
!               **  THEN COLLAPSE & TO A NULL STRING.             **
!               **  (THUS & SERVES AS A USEFUL CONCATONATION      **
!               **  CHARACTER.                                    **
!               **  (DECEMBER 1988)                               **
!               ****************************************************
!
        IF(IAMPER.EQ.'YES')THEN
          ILOC4=ILOC1+NH
          ILOC4M=ILOC4-1
          ILOC4P=ILOC4+1
          J=ILOC4M
!
          IF(IBUGD2.EQ.'ON' .OR. ISUBRO.EQ.'REP2')THEN
            WRITE(ICOUT,2422)ILOC1,NH,ILOC4,ILOC4M,ILOC4P,J
 2422       FORMAT('AT 2422: ILOC1,NH,ILOC4,ILOC4M,ILOC4P,J = ',6I6)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
          IF(NUMTEC.GE.ILOC4P)THEN
            DO 2420 I=ILOC4P,NUMTEC
              J=J+1
              ITEXHO(J)=ITEXHO(I)
!
              IF(IBUGD2.EQ.'ON' .OR. ISUBRO.EQ.'REP2')THEN
                WRITE(ICOUT,2427)I,J,ITEXHO(I),ITEXHO(J)
 2427           FORMAT('AT 2420: I,J,ITEXHO(I),ITEXHO(J) = ',   &
                       2I6,2(2X,A4))
                CALL DPWRST('XXX','BUG ')
              ENDIF
!
 2420       CONTINUE
            NUMTEC=J
          ENDIF
        ENDIF
!
 1000 CONTINUE
 1009 CONTINUE
!
!
!               ****************************************************
!               **  STEP 25--                                     **
!               **  NOW CHECK FOR ANY MACRO SUBSTITUTION          **
!               **  CHARACTERS.  THESE ARE IDENTIFIED BY A        **
!               **  $1, $2, ..., $10.  NOTE THAT $0 IS USED TO    **
!               **  DENOTE THE NUMBER OF MACRO ARGUMENTS.         **
!               **  (SEPTEMBER  2005)                             **
!               ****************************************************
!
!     2018/05: $99 TO RETRIEVE FULL CALL COMMAND
!     2018/05: DO SUBSTITUTION FOR $0 AND $00 EVEN IF
!              IMALEV = 0
!
 2500 CONTINUE
!
      IF(IBUGD2.EQ.'ON' .OR. ISUBRO.EQ.'REP2')THEN
        WRITE(ICOUT,22700)NUMTEC,IMALEV
22700   FORMAT('AT 2500: NUMTEC,IMALEV = ',2I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!CCCC IF(NUMTEC.GT.0 .AND. IMALEV.GE.1)THEN
      IF(NUMTEC.GT.0)THEN
!CCCC   MAXNCH=80
        MAXNCH=MAXSTR
        ITTEMP=' '
        NCH=0
        DO 2510 I=1,NUMTEC-1
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
              GO TO 2519
            ELSEIF(ITEXHO(IP1).EQ.'0')THEN
              ITTEMP=' '
              WRITE(ITTEMP(1:2),'(I2)')NMACAG
              NCH=2
              ILOC1=I
              ILOC2=IP1+1
              GO TO 2519
            ELSEIF(IMALEV.LT.1)THEN
              GO TO 9000
            ELSEIF(ITEXHO(IP1).EQ.'9' .AND. ITEXHO(IP2).EQ.'9')THEN
              ITTEMP=' '
              NCH=0
              IF(NMACCL.GT.0)THEN
                ITTEMP(1:NMACCL)=IMACCL(1:NMACCL)
                NCH=NMACCL
              ENDIF
              ILOC1=I
              ILOC2=IP2+1
              GO TO 2519
            ELSEIF((ITEXHO(IP1).EQ.'1' .OR. ITEXHO(IP1).EQ.'2' .OR.   &
                    ITEXHO(IP1).EQ.'3' .OR. ITEXHO(IP1).EQ.'4' .OR.   &
                    ITEXHO(IP1).EQ.'4' .OR. ITEXHO(IP1).EQ.'5' .OR.   &
                    ITEXHO(IP1).EQ.'6' .OR. ITEXHO(IP1).EQ.'7' .OR.   &
                    ITEXHO(IP1).EQ.'8' .OR. ITEXHO(IP1).EQ.'9') .AND.   &
                   (ITEXHO(IP2).EQ.'1' .OR. ITEXHO(IP2).EQ.'2' .OR.   &
                    ITEXHO(IP2).EQ.'3' .OR. ITEXHO(IP2).EQ.'4' .OR.   &
                    ITEXHO(IP2).EQ.'4' .OR. ITEXHO(IP2).EQ.'5' .OR.   &
                    ITEXHO(IP2).EQ.'6' .OR. ITEXHO(IP2).EQ.'7' .OR.   &
                    ITEXHO(IP2).EQ.'8' .OR. ITEXHO(IP2).EQ.'9')   &
                  )THEN
              IAJUNK(1:1)=ITEXHO(IP1)(1:1)
              IAJUNK(2:2)=ITEXHO(IP2)(1:1)
              READ(ITEXHO(IP1)(1:2),'(I2)')IITEMP
              IF(IITEMP.LE.0 .OR. IITEMP.GT.50)THEN
                WRITE(ICOUT,1561)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,3690)
 3690           FORMAT('      SPECIFIED POSITIONAL COMMAND LINE ',   &
                       'ARGUMENT IS LESS THAN 1 OR GREATER THAN 50.')
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,3693)IITEMP
 3693           FORMAT('      THE SPECIFIED ARGUMENT IS ',I8)
                CALL DPWRST('XXX','BUG ')
                IERROR='YES'
                GO TO 9000
              ENDIF
!CCCC         MAXNCH=80
              MAXNCH=MAXFNC
              NCH=0
              ITTEMP=' '
              DO 3630 II=MAXNCH,1,-1
                IF(IMACAR(IITEMP)(II:II).NE.' ')THEN
                  NCH=II
                  ITTEMP(1:NCH)=IMACAR(IITEMP)(1:NCH)
                  GO TO 3639
                ENDIF
 3630         CONTINUE
 3639         CONTINUE
!
              IF(IBUGD2.EQ.'ON' .OR. ISUBRO.EQ.'REP2')THEN
                WRITE(ICOUT,33693)NCH,ITTEMP
33693           FORMAT('AT 3639: NCH: ',I5,' ITTEMP: ',A)
                CALL DPWRST('XXX','BUG ')
              ENDIF
!
            ELSEIF(ITEXHO(IP1).EQ.'1' .OR. ITEXHO(IP1).EQ.'2' .OR.   &
                   ITEXHO(IP1).EQ.'3' .OR. ITEXHO(IP1).EQ.'4' .OR.   &
                   ITEXHO(IP1).EQ.'4' .OR. ITEXHO(IP1).EQ.'5' .OR.   &
                   ITEXHO(IP1).EQ.'6' .OR. ITEXHO(IP1).EQ.'7' .OR.   &
                   ITEXHO(IP1).EQ.'8' .OR. ITEXHO(IP1).EQ.'9')THEN
              READ(ITEXHO(IP1)(1:1),'(I1)')IITEMP
!CCCC         MAXNCH=80
              MAXNCH=MAXFNC
              NCH=0
              ITTEMP=' '
              DO 2630 II=MAXNCH,1,-1
                IF(IMACAR(IITEMP)(II:II).NE.' ')THEN
                  NCH=II
                  ITTEMP(1:NCH)=IMACAR(IITEMP)(1:NCH)
                  GO TO 2639
                ENDIF
 2630         CONTINUE
 2639         CONTINUE
!
              IF(IBUGD2.EQ.'ON' .OR. ISUBRO.EQ.'REP2')THEN
                WRITE(ICOUT,23693)NCH,ITTEMP
23693           FORMAT('AT 2639: NCH: ',I5,' ITTEMP: ',A)
                CALL DPWRST('XXX','BUG ')
              ENDIF
!
              ILOC1=I
              ILOC2=IP1+1
              GO TO 2519
            ELSE
!
!             CHECK FOR NAMED ARGUMENTS
!
              IF(IBUGD2.EQ.'ON' .OR. ISUBRO.EQ.'REP2')THEN
                WRITE(ICOUT,22701)NMACLA,IP1,NUMTEC,I,ITEXHO(I)
22701           FORMAT('AT CHECK FOR NAMED ARGUMENTS: ',   &
                       'NMACLA,IP1,NUMTEC,I,ITEXHO(I) = ',4I5,2X,A4)
                CALL DPWRST('XXX','BUG ')
              ENDIF
!
              IF(NMACLA.GE.1)THEN
!
!               2018/07: NEED TO CHECK THE NUMBER OF CHARACTERS IN
!                        ITEXHO TO BE INCLUDED IN THE MATCH.  THE
!                        LABEL NAME SHOULD ONLY CONTAIN NUMBERS,
!                        LOWER OR UPPER CASE ALPHABETIC, OR UNDERSCORE
!                        CHARACTER.  ANY OTHER CHARACTER TERMINATES
!                        CURRENT NAME.
!
                NMAXCH=0
!CCCC           DO2640JJ=IP1+1,NUMTEC
                DO 2640 JJ=IP1,NUMTEC
                  NMAXCH=NMAXCH+1
!CCCC             2020/12: BUG FIX IN FOLLOWING LINE
!CCCC             IC1=ITEXHO(IP1+KK-1)(1:1)
                  IINDX=JJ
                  IC1=ITEXHO(IINDX)(1:1)
                  IF(IC1.EQ.'_')THEN
                    IVAL1=38
                  ELSE
                    CALL DPCOAN(IC1,IVAL1)
                  ENDIF
!
                  IF(IBUGD2.EQ.'ON' .OR. ISUBRO.EQ.'REP2')THEN
                    WRITE(ICOUT,22801)JJ,IINDX,IC1,IVAL1,ITEXHO(IINDX)
22801               FORMAT('2640 LOOP: JJ,IINDX,IC1,IVAL1,',   &
                           'ITEXHO(IINDX) = ',2I5,2X,A1,2X,I5,2X,A4)
                    CALL DPWRST('XXX','BUG ')
                  ENDIF
!
                  IF(IVAL1.GE.48 .AND. IVAL1.LE.57)GO TO 2640
                  IF(IVAL1.GE.97 .AND. IVAL1.LE.122)GO TO 2640
                  IF(IVAL1.GE.65 .AND. IVAL1.LE.90)GO TO 2640
                  NMAXCH=NMAXCH-1
                  GO TO 2641
 2640           CONTINUE
 2641           CONTINUE
!
                IF(IBUGD2.EQ.'ON' .OR. ISUBRO.EQ.'REP2')THEN
                  WRITE(ICOUT,22803)NMAXCH
22803             FORMAT('AT 2641: NMAXCH = ',I5)
                  CALL DPWRST('XXX','BUG ')
                  DO 22805 LL=IP1+1,NUMTEC
                    WRITE(ICOUT,22808)LL,ITEXHO(LL)(1:1)
22808               FORMAT('LL,ITEXHO(LL) = ',I5,2X,A1)
                    CALL DPWRST('XXX','BUG ')
22805             CONTINUE
                ENDIF
!
                DO 2642 JJ=1,NMACLA
                  IFLAG=1
                  ILAST=MAX(IMACNC(JJ),NMAXCH)
!
                  IF(IBUGD2.EQ.'ON' .OR. ISUBRO.EQ.'REP2')THEN
                    WRITE(ICOUT,22703)JJ,IMACNC(JJ),NMAXCH,ILAST
22703               FORMAT('JJ,IMACNC(JJ),NMAXCH,ILAST = ',4I5)
                    CALL DPWRST('XXX','BUG ')
                  ENDIF
!
                  DO 2643 KK=1,ILAST
!
!                   MAKE NAME SEARCH CASE INSENSITIVE
!
                    IC1=ITEXHO(IP1+KK-1)(1:1)
                    CALL DPCOAN(IC1,IVAL1)
                    IF(IVAL1.GE.97 .AND. IVAL1.LE.122)IVAL1=IVAL1-32
                    CALL DPCONA(IVAL1,IC1)
                    IC2=IMACLA(JJ)(KK:KK)
                    CALL DPCOAN(IC2,IVAL2)
                    IF(IVAL2.GE.97 .AND. IVAL2.LE.122)IVAL2=IVAL2-32
                    CALL DPCONA(IVAL2,IC2)
!
                    IF(IBUGD2.EQ.'ON' .OR. ISUBRO.EQ.'REP2')THEN
                      WRITE(ICOUT,22731)JJ,KK,IC1,IC2
22731                 FORMAT('AT 2642: JJ,KK,IC1,IC2 = ',2I5,2(A1,1X))
                      CALL DPWRST('XXX','BUG ')
                    ENDIF
!
                    IF(IC1.NE.IC2)THEN
                      IFLAG=0
                      GO TO 2642
                    ENDIF
 2643             CONTINUE
                  IF(IFLAG.EQ.1)THEN
                    IROWXX=IMACLL(JJ)
                    NCHTMP=IMACNC(JJ)
!
                    IF(IBUGD2.EQ.'ON' .OR. ISUBRO.EQ.'REP2')THEN
                      WRITE(ICOUT,22711)JJ,IROWXX,NCHTMP
22711                 FORMAT('AT 2643: JJ,IROWXX,NCHTMP=',3I8)
                      CALL DPWRST('XXX','BUG ')
                    ENDIF
!
                    GO TO 2645
                  ENDIF
 2642           CONTINUE
!
!               NO MATCH FOUND
!
                GO TO 2510
!
!               MATCH FOUND
!
 2645           CONTINUE
!CCCC           MAXNCH=80
                MAXNCH=MAXFNC
                ITTEMP=' '
                DO 2650 II=MAXNCH,1,-1
                  IF(IMACAR(IROWXX)(II:II).NE.' ')THEN
                    NCH=II
                    ITTEMP(1:NCH)=IMACAR(IROWXX)(1:NCH)
                    GO TO 2659
                  ENDIF
 2650           CONTINUE
 2659           CONTINUE
!
                IF(IBUGD2.EQ.'ON' .OR. ISUBRO.EQ.'REP2')THEN
                  WRITE(ICOUT,22721)IMACAR(IROWXX)
22721             FORMAT('AT 2659: IMACAR(IROWXX) = ',A)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,22722)I,NCH,NCHTMP,ITTEMP(1:MIN(100,NCH))
22722             FORMAT('I,NCH,NCHTMP,ITTEMP = ',3I5,A100)
                  CALL DPWRST('XXX','BUG ')
                ENDIF
!
                ILOC1=I
                ILOC2=IP1+NCHTMP
                GO TO 2519
!
              ENDIF
            ENDIF
          ENDIF
          GO TO 2510
!
 2519     CONTINUE
!
          IF(IBUGD2.EQ.'ON' .OR. ISUBRO.EQ.'REP2')THEN
            WRITE(ICOUT,2711)ILOC1,ILOC2,NUMTEC,IP1,NCH,NCHTMP
 2711       FORMAT('AT 2519: ILOC1,ILOC2,NUMTEC,IP1,NCH,NCHTMP = ',6I8)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2713)ITEXHO(ILOC1),ITEXHO(ILOC2)
 2713       FORMAT('ITEXHO(ILOC1),ITEXHO(ILOC2) = ',A4,2X,A4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
          NUMTE2=0
          IF(ILOC1.GE.1)THEN
            DO 2720 KK=1,ILOC1-1
              NUMTE2=NUMTE2+1
              ISTR(NUMTE2)=ITEXHO(KK)
 2720       CONTINUE
          ENDIF
!
          IF(NCH.GT.0)THEN
            DO 2730 KK=1,NCH
              NUMTE2=NUMTE2+1
              ISTR(NUMTE2)=ITTEMP(KK:KK)
 2730       CONTINUE
          ENDIF
!
          IF(NUMTEC.GE.ILOC2)THEN
            DO 2740 KK=ILOC2,NUMTEC
              NUMTE2=NUMTE2+1
              ISTR(NUMTE2)=ITEXHO(KK)
 2740       CONTINUE
          ENDIF
          NUMTEC=NUMTE2
          IF(NUMTEC.GE.1)THEN
            DO 2750 KK=1,NUMTEC
              ITEXHO(KK)=ISTR(KK)
 2750       CONTINUE
          ENDIF
!
 2510   CONTINUE
      ENDIF
!
!               ****************
!               **  STEP 90-- **
!               **  EXIT      **
!               ****************
!
 9000 CONTINUE
!
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
          DO 9028 I=1,NUMCHF
            WRITE(ICOUT,9029)I,IFUNC(I)
 9029       FORMAT('I,IFUNC(I) = ',I8,A1)
            CALL DPWRST('XXX','BUG ')
 9028     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPREP2
      SUBROUTINE DPREP3(ITEXHO,NUMTEC,IREPCH,IMALEV,   &
                        IBUGD2,IERROR)
!
!     PURPOSE--TRANSLATE A STRING AS DICTATED BY THE   ^   OPERATOR.
!              THAT IS, REPLACE ALL OCCURRANCES OF  XXX^YYY
!              (WHERE XXX IS AN ARBITRARY STRING, AND
!              YYY IS A DATAPLOT PARAMETER NAME OR A FUNCTION NAME)
!              BY THE NUMERIC VALUE OF THE PARAMETER YYY, OR
!              THE FUNCTIONAL STRING IN THE FUNCTION YYY.
!              (E.G., IF PARAMETER K HAS THE VALUE 7,
!              THEN   Y^K   BECOMES   Y7
!              OR     IF THE FUNCTION K HAS THE CONTENTS XYZ,
!              THEN   Y^K   BECOMES   YABC  ).
!     NOTE--THIS SUBROUTINE CHANGES THE CONTENTS OF THE INPUT VECTOR ITEXHO()
!           AND THE INPUT VARIABLE NUMTEC.
!     NOTE--THIS SUBROUTINE IS A COPY OF DPREP2, BUT WITH A NUMBER
!           OF THE ARGUMENTS TAKEN FROM THE INCLUDE FILES RATHER
!           THAN BEING EXPLICITLY PASSED.  THIS ROUTINE HAS BEEN
!           CREATED TO ALLOW IT TO BE CALLED FROM THE DPREAL
!           ROUTINE.   USE WHEN READING FROM STORED LOOP COMMANDS
!           RATHER THAN THE TERMINAL OR FILE.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2015/1
!     ORIGINAL VERSION--JANUARY   2015.
!     UPDATED         --OCTOBER   2016. SUPPORT NAMED ARGUMENTS
!                                       ($Y, $TITLE, ETC.)
!     UPDATED         --OCTOBER   2016. INCREASE NUMBER OF ORDERED
!                                       ARGUMENTS FROM 10 TO 20
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ITEXHO
      CHARACTER*4 IBUGD2
      CHARACTER*1 IREPCH
      CHARACTER*4 IERROR
!
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
!
      CHARACTER*40 ILABT
      CHARACTER*40 ITTEMP
!
!-------------------------------------------------------------------
!
      DIMENSION ITEXHO(*)
      DIMENSION ISTR(200)
      DIMENSION IH(200)
!
!-----COMMON VARIABLES (GENERAL)------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT---------------------------------------------------
!
      IERROR='NO'
!
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
!
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
!
!               ****************************************************
!               **  STEP 10--                                     **
!               **  LOOP THROUGH (AT MOST) 100 PASSES.  EACH PASS **
!               **  SEARCHES FOR THE NEXT OCCURRANCE OF ^.  A     **
!               **  GIVEN PASS WIPES OUT ^XX AND REPLACES IT WITH **
!               **  THE NUMERIC VALUE OF PARAMETER XX OR THE      **
!               **  STRING CONTENTS VALUE OF FUNCTION XX.  NOTE   **
!               **  THAT EACH PASS CHANGES THE CONTENTS OF INPUT  **
!               **  VARIABLE ITEXHO() AND INPUT VALUE NUMTEC.     **
!               ****************************************************
!
      IFOUNG='NO'
      DO 1000 IPASS=1,100
!
!               ****************************************************
!               **  STEP 11--                                     **
!               **  FOR THIS PASS,                                **
!               **  SEARCH THE STRING FOR THE NEXT OCCURRANCE OF  **
!               **  THE SUBSTRING IN IREPCH (USUALLY ^ )          **
!               **  PROCEED RIGHT TO LEFT (DEC. 1986).            **
!               **  IF FOUND, THEN PROCEED FURTHER.               **
!               **  IF NOT FOUND, THEN EXIT.                      **
!               ****************************************************
!
        IFOUNV='NO'
!
        IF(NUMTEC.GT.0)THEN
          DO 1120 IDUMMY=1,NUMTEC
            I=NUMTEC-IDUMMY+1
            IF(ITEXHO(I).EQ.IREPCH)THEN
              IFOUNV='YES'
              IFOUNG='YES'
              ILOC1=I
              ILOC2=I
              GO TO 1190
            ENDIF
 1120     CONTINUE
          GO TO 2500
        ELSE
          GO TO 9000
        ENDIF
!
 1190   CONTINUE
!
!               *****************************************************
!               **  STEP 12--                                      **
!               **  EXTRACT THE PARAMETER OR FUNCTION NAME.        **
!               **  THIS WILL BE THE STRING IMMEDIATELY FOLLOWING  **
!               **  ^ UNTIL A BLANK IS FOUND                       **
!               **  OR UNTIL A    "   IS FOUND (DEC. 1986)         **
!               **  OR UNTIL A    )   IS FOUND (DEC. 1988)         **
!               **  OR UNTIL A    &   IS FOUND (DEC. 1988)         **
!               *****************************************************
!
!       SEPTEMBER 2007.  CHECK FOR ^ROWLABEL^K TYPE SYNTAX.  THIS
!                        WILL SUBSTITUTE THE K-TH ROW LABEL.
!
        DO 1210 I=1,8
          ISTR(I)=' '
 1210   CONTINUE
!
        IAMPER='NO'
!
        IMIN=ILOC2+1
        IMAX=IMIN+7
        IF(IMAX.GT.NUMTEC)IMAX=NUMTEC
        J=0
!
!  JUNE 2003.  BASICALLY, A DATAPLOT NAME CONSISTS OF NUMBERS OR
!              ALPABETIC CHARACTERS OR UDERSCORES.  ANYTHING ELSE
!              SHOULD TERMINATE THE NAME.
!
        DO 1250 I=IMIN,IMAX
          I2=I
          ITEMP=ICHAR(ITEXHO(I)(1:1))
!
          IF(IBUGD2.EQ.'ON')THEN
            WRITE(ICOUT,1251)I,IPASS,ITEXHO(I)
 1251       FORMAT('I,IPASS,ITEXHO(I) = ',2I8,1X,A4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
          IF(ITEMP.LT.48 .OR. ITEMP.GT.122 .OR.   &
             (ITEMP.GT.57 .AND. ITEMP.LT.65) .OR.   &
             (ITEMP.GT.90 .AND. ITEMP.LT.97 .AND. ITEMP.NE.95)   &
             )THEN
             ILOC3=I2-1
             IF(ITEMP.EQ.38)IAMPER='YES'
             GO TO 1290
          ENDIF
          J=J+1
          ISTR(J)=ITEXHO(I)
 1250   CONTINUE
        ILOC3=I2
!
 1290   CONTINUE
        NUMCHN=J
!
        IF(IBUGD2.EQ.'ON')THEN
          WRITE(ICOUT,1291)ILOC1,ILOC2,ILOC3
 1291     FORMAT('ILOC1,ILOC2,ILOC3 = ',3I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1292)IMIN,IMAX,NUMCHN
 1292     FORMAT('IMIN,IMAX,NUMCHN = ',3I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
!               ****************************************************
!               **  STEP 13--                                     **
!               **  PACK THE PARAMETER/FUNCTION NAME STRING INTO  **
!               **  2 4-BYTE WORDS.                               **
!               ****************************************************
!
        IWORD1='    '
        IWORD2='    '
        DO 1310 I=1,4
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
!
!               ****************************************************
!               **  STEP 14--                                     **
!               **  CONVERT THE 2 4-BYTE WORDS INTO UPPER CASE.   **
!               **  (JUNE 1987)                                   **
!               ****************************************************
!
        IJUNK1=IHNAP1
        IJUNK2=IHNAP2
        CALL DPUPP4(IJUNK1,IJUNK1,IBUGD2,IERROR)
        CALL DPUPP4(IJUNK2,IJUNK2,IBUGD2,IERROR)
        IHNAP1=IJUNK1
        IHNAP2=IJUNK2
!
!               ****************************************
!               **  STEP 15--                         **
!               **  DETERMINE IF THE NAME IS IN THE   **
!               **  INTERNAL DATAPLOT NAME LIST,      **
!               **  AND AS A PARAMETER OR FUNCTION.   **
!               ****************************************
!
        IF(NUMNAM.GT.0)THEN
          DO 1500 I=1,NUMNAM
            I2=I
            IF(IHNAP1.EQ.IHNAME(I).AND.IHNAP2.EQ.IHNAM2(I))GO TO 1550
 1500     CONTINUE
!
          NH=1
          IH(1)='    '
          GO TO 2100
!
 1550     CONTINUE
          IVAL=IVALUE(I2)
          VAL=VALUE(I2)
          IUS=IUSE(I2)
          IL1=IVSTAR(I2)
          IL2=IVSTOP(I2)
!
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
!
          ILABT=' '
          IF(IVAL.GT.0.AND.IUS.EQ.'V')ILABT(1:40)=IVARLB(IVAL)(1:40)
!
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
            GO TO 9000
          ENDIF
!
        ENDIF
!
!               ************************************************
!               **  STEP 16--                                 **
!               **  FOR THE CASE WHEN HAVE A PARAMETER NAME,  **
!               **  DETERMINE THE LITERAL STRING ASSOCIATED   **
!               **  WITH THE PARAMETER VALUE.                 **
!               ************************************************
!
!CCCCC  SEPTEMBER 2007: CHECK FOR ROWLABEL:
!
        IF(IUS.EQ.'P')THEN
          IROWFL='OFF'
          IF(ILOC1.GE.9 .AND. IVAL.GT.0 .AND. IVAL.LE.MAXOBV)THEN
            IF((ITEXHO(ILOC1-8)(1:1).EQ.'R' .OR.   &
                ITEXHO(ILOC1-8)(1:1).EQ.'r') .AND.   &
               (ITEXHO(ILOC1-7)(1:1).EQ.'O' .OR.   &
                ITEXHO(ILOC1-7)(1:1).EQ.'o') .AND.   &
               (ITEXHO(ILOC1-6)(1:1).EQ.'W' .OR.   &
                ITEXHO(ILOC1-6)(1:1).EQ.'w') .AND.   &
               (ITEXHO(ILOC1-5)(1:1).EQ.'L' .OR.   &
                ITEXHO(ILOC1-5)(1:1).EQ.'l') .AND.   &
               (ITEXHO(ILOC1-4)(1:1).EQ.'A' .OR.   &
                ITEXHO(ILOC1-4)(1:1).EQ.'a') .AND.   &
               (ITEXHO(ILOC1-3)(1:1).EQ.'B' .OR.   &
                ITEXHO(ILOC1-3)(1:1).EQ.'b') .AND.   &
               (ITEXHO(ILOC1-2)(1:1).EQ.'E' .OR.   &
                ITEXHO(ILOC1-2)(1:1).EQ.'e') .AND.   &
               (ITEXHO(ILOC1-1)(1:1).EQ.'L' .OR.   &
                ITEXHO(ILOC1-1)(1:1).EQ.'l'))THEN
              IROWFL='YES'
              ILOC1=ILOC1-9
            ENDIF
          ENDIF
          IF(IROWFL.EQ.'ON')THEN
            ILABT=' '
            ILABT(1:24)=IROWLB(IVAL)(1:24)
            NH=24
            DO 1710 I=24,1,-1
              IF(ILABT(I:I).NE.' ')THEN
                NH=I
                GO TO 1719
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
              DO 1730 I=1,NH
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
          DO 1610 I=52,1,-1
            IF(ILABT(I:I).NE.' ')THEN
              NH=I
              GO TO 1619
            ENDIF
 1610     CONTINUE
          NH=0
 1619     CONTINUE
          IF(NH.EQ.0)THEN
            DO 1620 I=1,4
              IH(I)=' '
              IH(I+4)=' '
              IH(I)(1:1)=IHNAP1(I:I)
              IH(I+4)(1:1)=IHNAP2(I:I)
 1620       CONTINUE
            NH=8
            DO 1625 I=8,1,-1
              IF(IH(I).NE.' ')THEN
                NH=I
                GO TO 1629
              ENDIF
 1625       CONTINUE
 1629       CONTINUE
          ELSE
            DO 1630 I=1,NH
              IH(I)=' '
              IH(I)(1:1)=ILABT(I:I)
 1630       CONTINUE
          ENDIF
        ENDIF
!
!               ****************************************************
!               **  STEP 21--                                     **
!               **  COLLAPSE THE SUBSTRING ^ FOLLOWED BY          **
!               **  THE PARAMETER/FUNCTION NAME TO A NULL STRING. **
!               ****************************************************
!
 2100   CONTINUE
        ILOC1M=ILOC1-1
        ILOC3P=ILOC3+1
        J=ILOC1M
        IF(NUMTEC.GE.ILOC3P)THEN
          DO 2110 I=ILOC3P,NUMTEC
            J=J+1
            ITEXHO(J)=ITEXHO(I)
 2110     CONTINUE
        ENDIF
        NUMTE2=J
!
!               ****************************************************
!               **  STEP 22--                                     **
!               **  INSERT THE LITERAL VALUE                      **
!               **  AT THE PROPER PLACE IN THE COLLAPSED STRING.  **
!               ****************************************************
!
        J=ILOC1M
        IF(NUMTE2.GE.ILOC1)THEN
          DO 2200 I=ILOC1,NUMTE2
            IREV=NUMTE2-I+ILOC1
            IREVNH=IREV+NH
            ITEXHO(IREVNH)=ITEXHO(IREV)
 2200     CONTINUE
        ENDIF
!
        IF(NH.GE.1)THEN
          DO 2300 I=1,NH
            J=ILOC1M+I
            ITEXHO(J)=IH(I)
 2300     CONTINUE
        ENDIF
        NUMTE3=NUMTE2+NH
        NUMTEC=NUMTE3
!
!               ****************************************************
!               **  STEP 24--                                     **
!               **  IF THE TERMINATOR WAS &,                      **
!               **  THEN COLLAPSE & TO A NULL STRING.             **
!               **  (THUS & SERVES AS A USEFUL CONCATONATION      **
!               **  CHARACTER.                                    **
!               **  (DECEMBER 1988)                               **
!               ****************************************************
!
        IF(IAMPER.EQ.'YES')THEN
          ILOC4=ILOC1+NH
          ILOC4M=ILOC4-1
          ILOC4P=ILOC4+1
          J=ILOC4M
          IF(NUMTEC.GE.ILOC4P)THEN
            DO 2420 I=ILOC4P,NUMTEC
              J=J+1
              ITEXHO(J)=ITEXHO(I)
 2420       CONTINUE
            NUMTEC=J
          ENDIF
        ENDIF
!
 1000 CONTINUE
!
!
!               ****************************************************
!               **  STEP 25--                                     **
!               **  NOW CHECK FOR ANY MACRO SUBSTITUTION          **
!               **  CHARACTERS.  THESE ARE IDENTIFIED BY A        **
!               **  $1, $2, ..., $10.  NOTE THAT $0 IS USED TO    **
!               **  DENOTE THE NUMBER OF MACRO ARGUMENTS.         **
!               **  (SEPTEMBER  2005)                             **
!               ****************************************************
!
 2500 CONTINUE
      IF(NUMTEC.GT.0 .AND. IMALEV.GE.1)THEN
        MAXNCH=80
        DO 2510 I=1,NUMTEC-1
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
              GO TO 2519
            ELSEIF(ITEXHO(IP1).EQ.'0')THEN
              ITTEMP=' '
              WRITE(ITTEMP(1:2),'(I2)')NMACAG
              NCH=2
              ILOC1=I
              ILOC2=IP2+1
              GO TO 2519
            ELSEIF(ITEXHO(IP1).EQ.'1' .AND. ITEXHO(IP2).EQ.'0')THEN
              DO 2610 II=MAXNCH,1,-1
                IF(IMACAR(10)(II:II).NE.' ')THEN
                  NCH=II
                  DO 2613 JJ=1,NCH
                    ITTEMP(JJ:JJ)=IMACAR(10)(JJ:JJ)
 2613             CONTINUE
                  GO TO 2619
                ENDIF
 2610         CONTINUE
 2619         CONTINUE
              ILOC1=I
              ILOC2=IP2+1
              GO TO 2519
            ELSEIF(ITEXHO(IP1).EQ.'1' .AND. ITEXHO(IP2).EQ.'1')THEN
              DO 3610 II=MAXNCH,1,-1
                IF(IMACAR(11)(II:II).NE.' ')THEN
                  NCH=II
                  DO 3613 JJ=1,NCH
                    ITTEMP(JJ:JJ)=IMACAR(11)(JJ:JJ)
 3613             CONTINUE
                  GO TO 3619
                ENDIF
 3610         CONTINUE
 3619         CONTINUE
              ILOC1=I
              ILOC2=IP2+1
              GO TO 2519
            ELSEIF(ITEXHO(IP1).EQ.'1' .AND. ITEXHO(IP2).EQ.'2')THEN
              DO 3620 II=MAXNCH,1,-1
                IF(IMACAR(12)(II:II).NE.' ')THEN
                  NCH=II
                  DO 3623 JJ=1,NCH
                    ITTEMP(JJ:JJ)=IMACAR(12)(JJ:JJ)
 3623             CONTINUE
                  GO TO 3629
                ENDIF
 3620         CONTINUE
 3629         CONTINUE
              ILOC1=I
              ILOC2=IP2+1
              GO TO 2519
            ELSEIF(ITEXHO(IP1).EQ.'1' .AND. ITEXHO(IP2).EQ.'3')THEN
              DO 3630 II=MAXNCH,1,-1
                IF(IMACAR(13)(II:II).NE.' ')THEN
                  NCH=II
                  DO 3633 JJ=1,NCH
                    ITTEMP(JJ:JJ)=IMACAR(13)(JJ:JJ)
 3633             CONTINUE
                  GO TO 3639
                ENDIF
 3630         CONTINUE
 3639         CONTINUE
              ILOC1=I
              ILOC2=IP2+1
              GO TO 2519
            ELSEIF(ITEXHO(IP1).EQ.'1' .AND. ITEXHO(IP2).EQ.'4')THEN
              DO 3640 II=MAXNCH,1,-1
                IF(IMACAR(14)(II:II).NE.' ')THEN
                  NCH=II
                  DO 3643 JJ=1,NCH
                    ITTEMP(JJ:JJ)=IMACAR(14)(JJ:JJ)
 3643             CONTINUE
                  GO TO 3649
                ENDIF
 3640         CONTINUE
 3649         CONTINUE
              ILOC1=I
              ILOC2=IP2+1
              GO TO 2519
            ELSEIF(ITEXHO(IP1).EQ.'1' .AND. ITEXHO(IP2).EQ.'5')THEN
              DO 3650 II=MAXNCH,1,-1
                IF(IMACAR(15)(II:II).NE.' ')THEN
                  NCH=II
                  DO 3653 JJ=1,NCH
                    ITTEMP(JJ:JJ)=IMACAR(15)(JJ:JJ)
 3653             CONTINUE
                  GO TO 3659
                ENDIF
 3650         CONTINUE
 3659         CONTINUE
              ILOC1=I
              ILOC2=IP2+1
              GO TO 2519
            ELSEIF(ITEXHO(IP1).EQ.'1' .AND. ITEXHO(IP2).EQ.'6')THEN
              DO 3660 II=MAXNCH,1,-1
                IF(IMACAR(16)(II:II).NE.' ')THEN
                  NCH=II
                  DO 3663 JJ=1,NCH
                    ITTEMP(JJ:JJ)=IMACAR(16)(JJ:JJ)
 3663             CONTINUE
                  GO TO 3669
                ENDIF
 3660         CONTINUE
 3669         CONTINUE
              ILOC1=I
              ILOC2=IP2+1
              GO TO 2519
            ELSEIF(ITEXHO(IP1).EQ.'1' .AND. ITEXHO(IP2).EQ.'7')THEN
              DO 3670 II=MAXNCH,1,-1
                IF(IMACAR(17)(II:II).NE.' ')THEN
                  NCH=II
                  DO 3673 JJ=1,NCH
                    ITTEMP(JJ:JJ)=IMACAR(17)(JJ:JJ)
 3673             CONTINUE
                  GO TO 3679
                ENDIF
 3670         CONTINUE
 3679         CONTINUE
              ILOC1=I
              ILOC2=IP2+1
              GO TO 2519
            ELSEIF(ITEXHO(IP1).EQ.'1' .AND. ITEXHO(IP2).EQ.'8')THEN
              DO 3680 II=MAXNCH,1,-1
                IF(IMACAR(18)(II:II).NE.' ')THEN
                  NCH=II
                  DO 3683 JJ=1,NCH
                    ITTEMP(JJ:JJ)=IMACAR(18)(JJ:JJ)
 3683             CONTINUE
                  GO TO 3689
                ENDIF
 3680         CONTINUE
 3689         CONTINUE
              ILOC1=I
              ILOC2=IP2+1
              GO TO 2519
            ELSEIF(ITEXHO(IP1).EQ.'1' .AND. ITEXHO(IP2).EQ.'9')THEN
              DO 3690 II=MAXNCH,1,-1
                IF(IMACAR(19)(II:II).NE.' ')THEN
                  NCH=II
                  DO 3693 JJ=1,NCH
                    ITTEMP(JJ:JJ)=IMACAR(19)(JJ:JJ)
 3693             CONTINUE
                  GO TO 3699
                ENDIF
 3690         CONTINUE
 3699         CONTINUE
              ILOC1=I
              ILOC2=IP2+1
              GO TO 2519
            ELSEIF(ITEXHO(IP1).EQ.'2' .AND. ITEXHO(IP2).EQ.'0')THEN
              DO 3710 II=MAXNCH,1,-1
                IF(IMACAR(20)(II:II).NE.' ')THEN
                  NCH=II
                  DO 3713 JJ=1,NCH
                    ITTEMP(JJ:JJ)=IMACAR(20)(JJ:JJ)
 3713             CONTINUE
                  GO TO 3719
                ENDIF
 3710         CONTINUE
 3719         CONTINUE
              ILOC1=I
              ILOC2=IP2+1
              GO TO 2519
            ELSEIF(ITEXHO(IP1).EQ.'0')THEN
              IF(NMACAG.LE.9)THEN
                WRITE(ITTEMP(1:1),'(I1)')NMACAG
                NCH=1
              ELSEIF(NMACAG.LE.99)THEN
                WRITE(ITTEMP(1:2),'(I2)')NMACAG
                NCH=2
              ELSE
                GO TO 2510
              ENDIF
              ILOC1=I
              ILOC2=IP1+1
              GO TO 2519
            ELSEIF(ITEXHO(IP1).EQ.'1' .OR. ITEXHO(IP1).EQ.'2' .OR.   &
                   ITEXHO(IP1).EQ.'3' .OR. ITEXHO(IP1).EQ.'4' .OR.   &
                   ITEXHO(IP1).EQ.'4' .OR. ITEXHO(IP1).EQ.'5' .OR.   &
                   ITEXHO(IP1).EQ.'6' .OR. ITEXHO(IP1).EQ.'7' .OR.   &
                   ITEXHO(IP1).EQ.'8' .OR. ITEXHO(IP1).EQ.'9')THEN
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
              DO 2630 II=MAXNCH,1,-1
                IF(IMACAR(IITEMP)(II:II).NE.' ')THEN
                  NCH=II
                  DO 2633 JJ=1,NCH
                    ITTEMP(JJ:JJ)=IMACAR(IITEMP)(JJ:JJ)
 2633             CONTINUE
                  GO TO 2639
                ENDIF
 2630         CONTINUE
 2639         CONTINUE
              ILOC1=I
              ILOC2=IP1+1
              GO TO 2519
            ELSE
!
!             CHECK FOR NAMED ARGUMENTS
!
              IF(NMACLA.GE.1)THEN
!
!               2018/07: NEED TO CHECK THE NUMBER OF CHARACTERS IN
!                        ITEXHO TO BE INCLUDED IN THE MATCH.  THE
!                        LABEL NAME SHOULD ONLY CONTAIN NUMBERS,
!                        LOWER OR UPPER CASE ALPHABETIC, OR UNDERSCORE
!                        CHARACTER.  ANY OTHER CHARACTER TERMINATES
!                        CURRENT NAME.
!
                NMAXCH=0
                DO 2640 JJ=IP1+1,NUMTEC
                  NMAXCH=NMAXCH+1
                  IC1=ITEXHO(IP1+KK-1)(1:1)
                  IF(IC1.EQ.'_')GO TO 2640
                  CALL DPCOAN(IC1,IVAL1)
                  IF(IVAL1.GE.48 .AND. IVAL1.LE.57)GO TO 2640
                  IF(IVAL1.GE.97 .AND. IVAL1.LE.122)GO TO 2640
                  IF(IVAL1.GE.65 .AND. IVAL1.LE.90)GO TO 2640
                  NMAXCH=NMAXCH-1
                  GO TO 2641
 2640           CONTINUE
 2641           CONTINUE
!
                IF(IBUGD2.EQ.'ON')THEN
                  WRITE(ICOUT,22703)JJ,IMACNC(JJ)
22703             FORMAT('JJ,IMACNC(JJ) = ',3I5)
                  CALL DPWRST('XXX','BUG ')
                ENDIF
!
                DO 2642 JJ=1,NMACLA
                  IFLAG=1
                  ILAST=MAX(IMACNC(JJ),NMAXCH)
                  DO 2643 KK=1,ILAST
!
!                   MAKE NAME SEARCH CASE INSENSITIVE
!
                    IC1=ITEXHO(IP1+KK-1)(1:1)
                    CALL DPCOAN(IC1,IVAL1)
                    IF(IVAL1.GE.97 .AND. IVAL1.LE.122)IVAL1=IVAL1-32
                    CALL DPCONA(IVAL1,IC1)
                    IC2=IMACLA(JJ)(KK:KK)
                    CALL DPCOAN(IC2,IVAL2)
                    IF(IVAL2.GE.97 .AND. IVAL2.LE.122)IVAL2=IVAL2-32
                    CALL DPCONA(IVAL2,IC2)
!
                    IF(IBUGD2.EQ.'ON')THEN
                      WRITE(ICOUT,22731)JJ,KK,IC1,IC2
22731                 FORMAT('AT 2642: JJ,KK,IC1,IC2 = ',2I5,2(A1,1X))
                      CALL DPWRST('XXX','BUG ')
                    ENDIF
!
                    IF(IC1.NE.IC2)THEN
                      IFLAG=0
                      GO TO 2642
                    ENDIF
 2643             CONTINUE
                  IF(IFLAG.EQ.1)THEN
                    IROWXX=IMACLL(JJ)
                    NCHTMP=IMACNC(JJ)
!
                    IF(IBUGD2.EQ.'ON')THEN
                      WRITE(ICOUT,22711)JJ,IROWXX,NCHTMP
22711                 FORMAT('AT 2643: JJ,IROWXX,NCHTMP=',3I8)
                      CALL DPWRST('XXX','BUG ')
                    ENDIF
!
                    GO TO 2645
                  ENDIF
 2642           CONTINUE
!
!               NO MATCH FOUND
!
                GO TO 2510
!
!               MATCH FOUND
!
 2645           CONTINUE
                MAXNCH=80
                DO 2650 II=MAXNCH,1,-1
                  IF(IMACAR(IROWXX)(II:II).NE.' ')THEN
                    NCH=II
                    DO 2653 JJ=1,NCH
                      ITTEMP(JJ:JJ)=IMACAR(IROWXX)(JJ:JJ)
 2653               CONTINUE
                    GO TO 2659
                  ENDIF
 2650           CONTINUE
 2659           CONTINUE
                ILOC1=I
                ILOC2=IP1+NCHTMP
                GO TO 2519
!
              ENDIF
            ENDIF
          ENDIF
          GO TO 2510
!
 2519     CONTINUE
          NUMTE2=0
          DO 2720 KK=1,ILOC1-1
            NUMTE2=NUMTE2+1
            ISTR(NUMTE2)=ITEXHO(KK)
 2720     CONTINUE
          DO 2730 KK=1,NCH
            NUMTE2=NUMTE2+1
            ISTR(NUMTE2)=ITTEMP(KK:KK)
 2730     CONTINUE
          IF(NUMTEC.GE.ILOC2)THEN
            DO 2740 KK=ILOC2,NUMTEC
              NUMTE2=NUMTE2+1
              ISTR(NUMTE2)=ITEXHO(KK)
 2740       CONTINUE
          ENDIF
          NUMTEC=NUMTE2
          DO 2750 KK=1,NUMTEC
            ITEXHO(KK)=ISTR(KK)
 2750     CONTINUE
!
 2510   CONTINUE
      ENDIF
!
!               ****************
!               **  STEP 90-- **
!               **  EXIT      **
!               ****************
!
 9000 CONTINUE
!
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
          DO 9028 I=1,NUMCHF
            WRITE(ICOUT,9029)I,IFUNC(I)
 9029       FORMAT('I,IFUNC(I) = ',I8,A1)
            CALL DPWRST('XXX','BUG ')
 9028     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPREP3
      SUBROUTINE DPREPS(Y,X1,MAXNXT,N,NUMVAR,DUM1,DUM2,   &
                        IREP,REPSS,REPMS,REPSD,REPDF,NUMSET,   &
                        IBUGA3,IERROR)
!
!     PURPOSE--DETERMINE IF REPLICATION EXISTS AND
!              (IF EXISTENT) COMPUTE THE REPLIATION STANDARD DEVIATION
!              AND REPLICATION DEGREES OF FREEDOM.
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
!     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MARCH     1981.
!     UPDATED         --JULY      1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --MARCH     1992. FIX FORMAT
!     UPDATED         --JULY      2019. CONVERT X1 ... X5 TO SINGLE
!                                       MATRIX ARGUMENT
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IREP
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION X1(MAXNXT,NUMVAR)
      DIMENSION DUM1(*)
      DIMENSION DUM2(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPRE'
      ISUBN2='PS  '
      IERROR='NO'
!
      IREP='NO'
      REPSS=0.0
      REPMS=0.0
      REPSD=0.0
      REPDF=0.0
      NUMSET=0
!
!               ********************************************************
!               **  CHECK FOR REPLICATION AND IF EXISTENT COMPUTE     **
!               **  A (MODEL-FREE) REPLICATION STANDARD DEVIATION.    **
!               ********************************************************
!
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
!
!               ********************************************************
!               **  STEP 1--                                          **
!               **  DETERMINE THE NUMBER OF DISTINCT SUBSETS          **
!               **  FOR VARIABLE 1;                                   **
!               **  IF ALL VALUES ARE DISTINCT, THEN THIS             **
!               **  IMPLIES WE HAVE THE NO REPLICATION CASE           **
!               **  WITHOUT FURTHER CHECKING OF THE OTHER VARIABLES.  **
!               ********************************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMSET=0
      DO 4200 I=1,N
        IF(NUMSET.EQ.0)GO TO 4350
        DO 4300 J=1,NUMSET
          IF(X1(I,1).EQ.DUM1(J))GO TO 4200
 4300   CONTINUE
 4350   NUMSET=NUMSET+1
        DUM1(NUMSET)=X1(I,1)
 4200 CONTINUE
      IF(NUMSET.EQ.0)THEN
        WRITE(ICOUT,4205)
 4205   FORMAT('ERROR IN DPREPS   SUBROUTINE--NUMSET = 0')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(NUMSET.EQ.N)THEN
        GO TO 9000
      ENDIF
!
!               ********************************************************
!               **  STEP 2--                                          **
!               **  FOR THE CASE WHEN HAVE SOME REPLICATION FOR X1,   **
!               **  AND WHEN THE NUMBER OF VARIABLES IS 1, COPY OUT   **
!               **  THE Y'S FOR EACH X1 SUBSET INTO THE DUM2 VECTOR   **
!               **  AND ANALYZE IT THEREIN.                           **
!               ********************************************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMVAR.LE.1)THEN
        IREP='YES'
        IREPDF=0
        REPSS=0.0
!
        DO 4600 ISET=1,NUMSET
          NI=0
          DO 4700 I=1,N
            IF(X1(I,1).EQ.DUM1(ISET))NI=NI+1
            IF(X1(I,1).EQ.DUM1(ISET))DUM2(NI)=Y(I)
 4700     CONTINUE
          ANI=NI
          SUM=0.0
          DO 5100 I=1,NI
            SUM=SUM+DUM2(I)
 5100     CONTINUE
          YMEAN=SUM/ANI
          SUM=0.0
          DO 5200 I=1,NI
            SUM=SUM+(DUM2(I)-YMEAN)**2
 5200     CONTINUE
          IREPDF=IREPDF+NI-1
          REPSS=REPSS+SUM
 4600   CONTINUE
        GO TO 4800
      ENDIF
!
!               ********************************************************
!               **  STEP 3--                                          **
!               **  FOR THE CASE WHEN HAVE SOME REPLICATION FOR X1    **
!               **  AND THE NUMBER OF VARIABLES IS 2 OR MORE,         **
!               **  CARRY OUT A DETAILED EXAMINATION FOR REPLICATION  **
!               **  AND ANALYZE APPROPRIATELY.                        **
!               ********************************************************
!
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IREP='YES'
      IREPDF=0
      REPSS=0.0
!
      DO 4405 I=1,N
        DUM1(I)=-1.0
 4405 CONTINUE
!
      NUMSET=0
      DO 4410 I=1,N
        IF(DUM1(I).GT.0.0)GO TO 4410
        NI=0
        DO 4420 J=I,N
          DO 4425 K=1,NUMVAR
            IF(X1(J,K).NE.X1(I,K))GO TO 4420
 4425     CONTINUE
          NI=NI+1
          DUM1(J)=1.0
          DUM2(NI)=Y(J)
 4420   CONTINUE
        NUMSET=NUMSET+1
        IF(NI.LE.1)GO TO 4410
        ANI=NI
        SUM=0.0
        DO 4450 L=1,NI
          SUM=SUM+DUM2(L)
 4450   CONTINUE
        YMEAN=SUM/ANI
        SUM=0.0
        DO 4460 L=1,NI
          SUM=SUM+(DUM2(L)-YMEAN)**2
 4460   CONTINUE
        IREPDF=IREPDF+NI-1
        REPSS=REPSS+SUM
 4410 CONTINUE
!
 4800 CONTINUE
      IF(IREPDF.LE.0)THEN
        IREP='NO'
        GO TO 9000
      ENDIF
      REPDF=IREPDF
      REPMS=REPSS/REPDF
      REPSD=0.0
      IF(REPMS.GT.0.0)REPSD=SQRT(REPMS)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPREPS
      SUBROUTINE DPRES2(X1,Y1,X2,Y2,PX,PY,   &
                        IFIG,ILINPA,ILINCO,ILINC2,MAXLN,PLINTH,   &
                        AREGBA,IREBLI,IREBCO,IREBC2,MAXRG,PREBTH,   &
                        IREFSW,IREFCO,IREFC2,   &
                        IREPTY,IREPLI,IREPCO,IREPC2,PREPTH,PREPSP,   &
                        PTEXHE,PTEXWI,PTEXVG,PTEXHG)
!
!     PURPOSE--DRAW A RESISITOR WITH ONE END AT (X1,Y1)
!              AND THE OTHER END AT (X2,Y2).
!     NOTE--THE HEIGHT OF EACH RIPPLE IS PTEXHE.
!           THE WIDTH  OF EACH RIPPLE IS PTEXWI.
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
!     UPDATED         --JULY      2019. CREATE SCRATCH STORAGE IN DPRESI
!                                       RATHER THAN DPRES2
!     UPDATED         --OCTOBER   2020. SUPPORT FOR RGB COLOR
!
!-----NON-COMMON VARIABLES-------------------------------------
!
      CHARACTER*4 IFIG
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
      CHARACTER*4 IPATT
!CCCC CHARACTER*4 ICOLF
!CCCC CHARACTER*4 ICOLP
      CHARACTER*4 ICOL
      CHARACTER*4 IFLAG
!
      DIMENSION PX(*)
      DIMENSION PY(*)
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
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
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
        WRITE(ICOUT,63)IREBLI(1),IREBCO(1),IREBC2(1,1),PREBTH(1)
   63   FORMAT('IREBLI(1),IREBCO(1),IREBC2(1,1),PREBTH(1) = ',   &
               2(A4,2X),I5,G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,64)IREFSW(1),IREFCO(1),IREFC2(1,1),IREPC2(1,1)
   64   FORMAT('IREFSW(1),IREFCO(1),IREFC2(1,1),IREPC2(1,1) = ',   &
               2(A4,2X),2I5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,65)IREPTY(1),IREPLI(1),IREPCO(1),PREPTH(1),PREPSP(1)
   65   FORMAT('IREPTY(1),IREPLI(1),IREPCO(1),PREPTH(1),PREPSP(1) = ',   &
               3(A4,2X),2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,69)PTEXHE,PTEXWI,PTEXVG,PTEXHG
   69   FORMAT('PTEXHE,PTEXWI,PTEXVG,PTEXHG = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,79)IBUGG4,ISUBG4,IERRG4
   79   FORMAT('IBUGG4,ISUBG4,IERRG4 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!               *********************************
!               **  STEP 1--                   **
!               **  DETERMINE THE COORDINATES  **
!               **  FOR THE RESISTOR           **
!               *********************************
!
      AJY2=0
!
      DELX=X2-X1
      DELY=Y2-Y1
      ALEN=0.0
      TERM=(X2-X1)**2+(Y2-Y1)**2
      IF(TERM.GT.0.0)ALEN=SQRT(TERM)
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
      Y=0
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
!
      X=XMIN
      Y=0
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
!
      L=0
!CCCC DO1450JX=AJXMIN,AJXMAX,AJXDEL
      AJX=AJXMIN-AJXDEL
 1440 CONTINUE
      AJX=AJX+AJXDEL
      IF(AJX.GT.AJXMAX)GO TO 1460
!
      L=L+1
      L01=L-2*(L/2)
!
      AJX1=AJX
      AJX2=AJX+AJXDEL/2.0
      AJX3=AJX+AJXDEL
      AJY1=0.0
      IF(L01.EQ.0)AJY2=AJYDEL/2.0
      IF(L01.EQ.1)AJY2=-AJYDEL/2.0
      AJY3=0
!
      X=AJX1
      Y=AJY1
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
!
      X=AJX2
      Y=AJY2
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
!
      X=AJX3
      Y=AJY3
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
!
      GO TO 1440
!
 1460 CONTINUE
      X=ALEN
      Y=0
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
!
      NP=K
!
!               ***********************
!               **  STEP 2--         **
!               **  FILL THE FIGURE  **
!               **  (IF CALLED FOR)  **
!               ***********************
!
!CCCC IF(IREFSW(1).EQ.'OFF')GO TO 2190
!CCCC IPATT=IREPTY(1)
!CCCC PTHICK=PREPTH(1)
!CCCC PXGAP=PREPSP(1)
!CCCC PYGAP=PREPSP(1)
!CCCC ICOLF=IREFCO(1)
!CCCC ICOLP=IREPCO(1)
!CCCC CALL DPFIRE(PX,PY,NP,
!CCCC1IFIG,IPATT,PTHICK,PXGAP,PYGAP,ICOLF,ICOLP)
!2190 CONTINUE
!
!               *********************************
!               **  STEP 3--                   **
!               **  DRAW OUT THE FIGURE        **
!               *********************************
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
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'RES2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPRES2--')
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,NP
          WRITE(ICOUT,9016)I,PX(I),PY(I)
 9016     FORMAT('I,PX(I),PY(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
        WRITE(ICOUT,9039)IERRG4
 9039   FORMAT('IERRG4 = ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPRES2
      SUBROUTINE DPRES1(IHARG,IARGT,IARG,NUMARG,IDEFSR,   &
      IRECSR,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE RECIPE SIMCOV REPLICATES
!              IN THE FLOATING POINT VARIABLE IRECSR.
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --IARGT  (A  HOLLERITH VECTOR)
!                     --IARG   (A  INTEGER POINT VECTOR)
!                     --NUMARG (AN INTEGER VARIABLE)
!                     --IDEFSR (A  FLOATING POINT VARIABLE)
!     OUTPUT ARGUMENTS--IRECSR  (A  FLOATING POINT VARIABLE)
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
!           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
!           MODIFIED, OR OTHERWISE USED IN A CONTEXT
!           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--97/8
!     ORIGINAL VERSION--AUGUST   1997.
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
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
!
      IF(NUMARG.EQ.0)GO TO 1199
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'SIMC'.AND.IHARG(2).EQ.'REPL')   &
      GO TO 1110
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'=')GO TO 1110
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'=')GO TO 1110
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'REPL')GO TO 1110
      GO TO 1199
!
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'REPL')GO TO 1150
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1150
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      IF(IARGT(NUMARG).EQ.'NUMB')GO TO 1160
      GO TO 1120
!
 1120 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,1121)
 1121 FORMAT('***** ERROR IN DPRES1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR RECIPE SIMCOV REPLICATES ',   &
      'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1130)
 1130 FORMAT('      AN EXAMPLE OF THIS COMMAND IS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1131)
 1131 FORMAT('      RECIPE SIMCOV REPLICATES 100000 ')
      CALL DPWRST('XXX','BUG ')
      GO TO 1199
!
 1150 CONTINUE
      IHOLD=IDEFSR
      GO TO 1180
!
 1160 CONTINUE
      IHOLD=IARG(NUMARG)
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
      IF(IHOLD.LE.1000)IHOLD=1000
      IRECSR=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)IRECSR
 1181 FORMAT('THE NUMBER OF REPLICATIONS FOR THE SIMCOV ',   &
      'COMMAND HAS JUST BEEN SET TO ',I8)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 1199
!
 1199 CONTINUE
      RETURN
      END SUBROUTINE DPRES1
      SUBROUTINE DPRESZ(IHARG,IARGT,IARG,NUMARG,IDEFSR,   &
      IRECSR,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE RECIPE SIMPVT REPLICATES
!              IN THE FLOATING POINT VARIABLE IRECSR.
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --IARGT  (A  HOLLERITH VECTOR)
!                     --IARG   (A  INTEGER POINT VECTOR)
!                     --NUMARG (AN INTEGER VARIABLE)
!                     --IDEFSR (A  FLOATING POINT VARIABLE)
!     OUTPUT ARGUMENTS--IRECSR  (A  FLOATING POINT VARIABLE)
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
!           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
!           MODIFIED, OR OTHERWISE USED IN A CONTEXT
!           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--97/8
!     ORIGINAL VERSION--AUGUST   1997.
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
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
!
      IF(NUMARG.EQ.0)GO TO 1199
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'SIMP'.AND.IHARG(2).EQ.'REPL')   &
      GO TO 1110
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'=')GO TO 1110
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'=')GO TO 1110
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'SIMC')GO TO 1110
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'REPL')GO TO 1110
      GO TO 1199
!
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'REPL')GO TO 1150
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1150
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      IF(IARGT(NUMARG).EQ.'NUMB')GO TO 1160
      GO TO 1120
!
 1120 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,1121)
 1121 FORMAT('***** ERROR IN DPRESZ--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR RECIPE SIMPVT REPLICATES ',   &
      'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1130)
 1130 FORMAT('      AN EXAMPLE OF THIS COMMAND IS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1131)
 1131 FORMAT('      RECIPE SIMPVT REPLICATES 100000 ')
      CALL DPWRST('XXX','BUG ')
      GO TO 1199
!
 1150 CONTINUE
      IHOLD=IDEFSR
      GO TO 1180
!
 1160 CONTINUE
      IHOLD=IARG(NUMARG)
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
      IF(IHOLD.LE.1000)IHOLD=1000
      IRECSR=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)IRECSR
 1181 FORMAT('THE NUMBER OF REPLICATIONS FOR THE SIMPVT ',   &
      'COMMAND HAS JUST BEEN SET TO ',I8)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 1199
!
 1199 CONTINUE
      RETURN
      END SUBROUTINE DPRESZ
      SUBROUTINE DPRESA(IHARG,NUMARG,IDEFSA,IRECSA,   &
      IBUGS2,IFOUND,IERROR)
!
!     PURPOSE--SPECIFY WHETHER THE RECIPE COMMAND USES
!              SATTERTHWAITE APPROXIMATION TO OBTAIN CRITICAL
!              VALUES OR NOT.
!
!     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
!                     --NUMARG (AN INTEGER VARIABLE)
!                     --IDEFSA (A  CHARACTER VARIABLE)
!                     --IBUGS2 (A  CHARACTER VARIABLE)
!     OUTPUT ARGUMENTS--IRECSA (A CHARACTER VARIABLE)
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
!           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
!           MODIFIED, OR OTHERWISE USED IN A CONTEXT
!           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--97/8
!     ORIGINAL VERSION--AUGUST   1997.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IDEFSA
      CHARACTER*4 IRECSA
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
   51 FORMAT('***** AT THE BEGINNING OF DPRESA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IDEFSA
   53 FORMAT('IDEFSA = ',A4)
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
        GO TO 9000
      ENDIF
!
      IFOUND='YES'
      IRECSA=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)IRECSA
 1181 FORMAT(   &
      'THE RECIPE SATTERTHWAITE APPROXIMATION HAS JUST BEEN SET TO ',   &
      A4)
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
 1189 CONTINUE
!
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
!
      RETURN
      END SUBROUTINE DPRESA
      SUBROUTINE DPRESE(IFOUND,IERROR)
!
!     PURPOSE--RESET ALL INTERNAL DATAPLOT SETTINGS
!              (INCLUDING DATA) AS IF ONE HAD SIGNED OFF
!              DATAPLOT AND LOGGED BACK ON.
!     INPUT  ARGUMENTS--NONE
!     OUTPUT ARGUMENTS--IFOUND ('YES' OR 'NO')
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
!     ORIGINAL VERSION--NOVEMBER  1980.
!     UPDATED         --MAY       1982.
!     UPDATED         --DECEMBER  1988. (REWRITE) RESET DATA, IO, PC, SU
!     UPDATED         --JANUARY   1992. RESET GRAPHICS=RESET PLOT
!     UPDATED         --JANUARY   1992. RESET I/O=RESET IO
!     UPDATED         --AUGUST    1992. SET PRED & RES TO 10000 OBS
!     UPDATED                           (NOT WORKING)
!     UPDATED         --SEPTEMBER 1993. FIX MAJOR SUBTLE BUG IN FIT
!                                       CAUSED BE REDEFINITION HEREIN
!                                       OF MAXCOL (AND THUS ICOLPR IN
!                                       DPFIT).  NEVER CHANGE
!                                       MAX...<ANYTHING>.
!                                       COMMENT OUT ALL SUCH CHANGES.
!     UPDATED         --SEPTEMBER 1993. RESET CLSB
!     UPDATED         --SEPTEMBER 1993. RESET LIMITS
!     UPDATED         --DECEMBER  2015. FOR WINDOWS, DO NOT RESET THE
!                                       QUICKWIN DEVICE
!     UPDATED         --SEPTEMBER 2016. ALLOW THE USER TO DEFINE
!                                       CERTAIN NAMES THAT WILL NOT
!                                       BE RESET
!     UPDATED         --SEPTEMBER 2016. RESET COMMAND LINE ARGUMENTS
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 ITEMEC
      CHARACTER*4 ITEMFE
      CHARACTER*4 ITEMPR
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 IVALC
!
!CCCC CHARACTER*4 IDEFGC
!
!-----COMMON----------------------------------------------------------
!
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
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPRE'
      ISUBN2='SE  '
!
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
          DO 55 I=1,NUMARG
            WRITE(ICOUT,56)I,IHARG(I)
   56       FORMAT('I,IHARG(I) = ',I8,2X,A4)
            CALL DPWRST('XXX','BUG ')
   55     CONTINUE
        ENDIF
      ENDIF
!
!               **************************************************
!               **  TREAT THE RESET CASE                        **
!               **************************************************
!
      IFOUND='YES'
!
!               **************************************************
!               **  BRANCH TO THE APPROPRIATE CASE              **
!               **************************************************
!
      ISTEPN='10'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.0)GO TO 5100
!
!     2016/09: CHECK FOR "RESET COMMAND LINE ARGUMENTS"
!
      IF(IHARG(1).EQ.'COMM' .AND. IHARG(2).EQ.'LINE' .AND.   &
         IHARG(3).EQ.'ARGU')THEN
        NMACAG=0
        NMACLA=0
        DO 101 II=1,50
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
        GO TO 9000
      ENDIF
!
!     2016/09: CHECK FOR "RESET NO RESET NAME <ON/OFF> FIRST
!
      IF(IHARG(1).EQ.'NO  ' .AND. IHARG(2).EQ.'RESE')THEN
        IF(NUMARG.LE.2)GO TO 9000
        IH=IHARG(3)
        IH2=IHARG2(3)
        IVALC='ON'
        IF(NUMARG.GE.4)THEN
          IF(IHARG(4).EQ.'ON' .OR. IHARG(4).EQ.'YES' .OR.   &
             IHARG(4).EQ.'TRUE')THEN
            IVALC='ON'
          ELSE
            IVALC='OFF'
          ENDIF
        ENDIF
!
        DO 110 II=1,MAXRES
          IF(IH.EQ.IRESN1(II) .AND. IH2.EQ.IRESN2(II))THEN
            IF(IVALC.EQ.'ON')THEN
!
!             NAME ALREADY IN LIST, DO NOTHING
!
              GO TO 9000
            ELSE
!
!             REMOVE NAME FROM LIST
!
              IRESN1(II)='    '
              IRESN2(II)='    '
              IF(II.LT.NUMNRE)THEN
                DO 120 JJ=II+1,NUMNRE
                  IRESN1(JJ-1)=IRESN1(JJ)
                  IRESN2(JJ-1)=IRESN2(JJ)
  120           CONTINUE
                NUMNRE=NUMNRE-1
                IF(IFEEDB.EQ.'ON')THEN
                  WRITE(ICOUT,999)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,123)IH,IH2
  123             FORMAT(A4,A4,' REMOVED FROM THE LIST OF NAMES THAT ',   &
                         'WILL NOT BE RESET')
                 CALL DPWRST('XXX','BUG ')
                ENDIF
              ENDIF
            ENDIF
            GO TO 9000
          ENDIF
!
  110   CONTINUE
!
!       ADD NAME TO LIST
!
        IF(NUMNRE.LT.MAXRES)THEN
          NUMNRE=NUMNRE+1
          IRESN1(NUMNRE)=IH
          IRESN2(NUMNRE)=IH2
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,126)IH,IH2
  126       FORMAT(A4,A4,' ADDED TO LIST OF NAMES THAT WILL NOT BE ',   &
                   'RESET')
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSE
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,128)MAXRES
  128       FORMAT('THE MAXIMUM NUMBER OF NAMES THAT WILL NOT BE ',   &
                   'RESET (',I4,') REACHED.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,129)IH,IH2
  129       FORMAT(A4,A4,' WILL NOT BE ADDED TO THE NO RESET LIST.')
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ENDIF
        GO TO 9000
      ENDIF
!
      DO 1000 I=1,NUMARG
!
      IF(IHARG(I).EQ.'DATA' .OR. IHARG(I).EQ.'DA' .OR.   &
         IHARG(I).EQ.'D')GO TO 1100
      IF(IHARG(I).EQ.'VARI' .OR. IHARG(I).EQ.'VA' .OR.   &
         IHARG(I).EQ.'V')GO TO 1200
      IF(IHARG(I).EQ.'PARA' .OR. IHARG(I).EQ.'PA' .OR.   &
         IHARG(I).EQ.'P')GO TO 1300
      IF(IHARG(I).EQ.'FUNC' .OR. IHARG(I).EQ.'FU' .OR.   &
         IHARG(I).EQ.'F')GO TO 1400
      IF(IHARG(I).EQ.'INPU' .OR. IHARG(I).EQ.'IO')GO TO 2100
      IF(IHARG(I).EQ.'PLOT' .OR. IHARG(I).EQ.'GRAP' .OR.   &
         IHARG(I).EQ.'PC')GO TO 2200
      IF(IHARG(I).EQ.'CLSB' .OR. IHARG(I).EQ.'LCSB')GO TO 2300
      IF(IHARG(I).EQ.'LIMI')GO TO 2400
      IF(IHARG(I).EQ.'SUPP')GO TO 3100
      IF(IHARG(I).EQ.'SU')GO TO 3100
      IF(IHARG(I).EQ.'ALL')GO TO 5100
      GO TO 1000
!
!               **************************************************
!               **  STEP 11--                                   **
!               **  RESET DATA (VARIABLES, PARAM, FUNC)         **
!               **************************************************
!
 1100 CONTINUE
      ISTEPN='11'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!CCCC THE FOLLOWING LINE WAS COMMENTED OUT   SEPTEMBER 1993
!CCCC MAXNK=MAXOBW
      NK=0
      IDEMXN=MAXOBV
!CCCC THE FOLLOWING LINE WAS COMMENTED OUT   SEPTEMBER 1993
!CCCC MAXN=IDEMXN
      N=0
      IDEMXC=MAXOBW/MAXOBV
!CCCC THE FOLLOWING LINE WAS COMMENTED OUT         SEPTEMBER 1993
!CCCC (CAUSED BIG SUBTLE PROBLEMS ELSEWHERE (E.G., FIT)) SEPT. 1993
!CCCC MAXCOL=IDEMXC
      NUMCOL=0
!CCCC THE FOLLOWING LINE WAS COMMENTED OUT   SEPTEMBER 1993
!CCCC MAXCHF=1000
      NUMCHF=0
!CCCC THE FOLLOWING LINE WAS COMMENTED OUT   SEPTEMBER 1993
!CCCC MAXFUN=100
      NUMFUN=0
!CCCC THE FOLLOWING LINE WAS COMMENTED OUT   SEPTEMBER 1993
!CCCC MAXCHM=200
      NUMCHM=0
!CCCC THE FOLLOWING LINE WAS COMMENTED OUT   SEPTEMBER 1993
!CCCC MAXCON=100
      NUMCON=0
!
      DO 1110 J=1,NUMNAM
!CCCC THE FOLLOWING LINE WAS ADDED AUGUST 1992
!CCCC IF(IHNAME(J).EQ.'PRED'.AND.IHNAM2(J).EQ.'    ')IN(J)=1
      IF(IHNAME(J).EQ.'PRED'.AND.IHNAM2(J).EQ.'    ')GO TO 1110
!CCCC THE FOLLOWING LINE WAS ADDED AUGUST 1992
!CCCC IF(IHNAME(J).EQ.'RES '.AND.IHNAM2(J).EQ.'    ')IN(J)=1
      IF(IHNAME(J).EQ.'RES '.AND.IHNAM2(J).EQ.'    ')GO TO 1110
      IF(IHNAME(J).EQ.'YPLO'.AND.IHNAM2(J).EQ.'T   ')GO TO 1110
      IF(IHNAME(J).EQ.'XPLO'.AND.IHNAM2(J).EQ.'T   ')GO TO 1110
      IF(IHNAME(J).EQ.'X2PL'.AND.IHNAM2(J).EQ.'OT  ')GO TO 1110
      IF(IHNAME(J).EQ.'TAGP'.AND.IHNAM2(J).EQ.'LOT ')GO TO 1110
      IF(IHNAME(J).EQ.'INFI'.AND.IHNAM2(J).EQ.'NITY')GO TO 1110
      IF(IHNAME(J).EQ.'PI  '.AND.IHNAM2(J).EQ.'    ')GO TO 1110
      IF(NUMNRE.GE.1)THEN
        DO 1111 K=1,NUMNRE
          IF(IHNAME(J).EQ.IRESN1(K) .AND. IHNAM2(J).EQ.IRESN2(K))   &
            GO TO 1110
 1111   CONTINUE
      ENDIF
      IF(IUSE(J).EQ.'V')IN(J)=(-1)
      IF(IUSE(J).EQ.'P')IN(J)=(-1)
      IF(IUSE(J).EQ.'F')IN(J)=(-1)
      IF(IUSE(J).EQ.'M')IN(J)=(-1)
 1110 CONTINUE
      CALL DPUPNT(IHNAME,IHNAM2,IUSE,IVALUE,VALUE,IN,   &
      IVSTAR,IVSTOP,MAXNAM,NUMNAM,V,MAXN,MAXCOL,NUMCOL,   &
      IBUGS2,IERROR)
!
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
      GO TO 1000
!
!               **************************************************
!               **  STEP 12--                                   **
!               **  RESET VARIABLES                             **
!               **************************************************
!
 1200 CONTINUE
      ISTEPN='12'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!CCCC THE FOLLOWING LINE WAS COMMENTED OUT   SEPTEMBER 1993
!CCCC MAXNK=MAXOBW
      NK=0
      IDEMXN=MAXOBV
!CCCC THE FOLLOWING LINE WAS COMMENTED OUT   SEPTEMBER 1993
!CCCC MAXN=IDEMXN
      N=0
      IDEMXC=MAXOBW/MAXOBV
!CCCC THE FOLLOWING LINE WAS COMMENTED OUT   SEPTEMBER 1993
!CCCC (CAUSED BIG SUBTLE PROBLEMS ELSEWHERE (E.G., FIT)) SEPT. 1993
!CCCC MAXCOL=IDEMXC
      NUMCOL=0
!
      DO 1210 J=1,NUMNAM
!CCCC THE FOLLOWING LINE WAS ADDED AUGUST 1992
!CCCC IF(IHNAME(J).EQ.'PRED'.AND.IHNAM2(J).EQ.'    ')IN(J)=1
      IF(IHNAME(J).EQ.'PRED'.AND.IHNAM2(J).EQ.'    ')GO TO 1210
!CCCC THE FOLLOWING LINE WAS ADDED AUGUST 1992
!CCCC IF(IHNAME(J).EQ.'RES '.AND.IHNAM2(J).EQ.'    ')IN(J)=1
      IF(IHNAME(J).EQ.'RES '.AND.IHNAM2(J).EQ.'    ')GO TO 1210
      IF(IHNAME(J).EQ.'YPLO'.AND.IHNAM2(J).EQ.'T   ')GO TO 1210
      IF(IHNAME(J).EQ.'XPLO'.AND.IHNAM2(J).EQ.'T   ')GO TO 1210
      IF(IHNAME(J).EQ.'X2PL'.AND.IHNAM2(J).EQ.'OT  ')GO TO 1210
      IF(IHNAME(J).EQ.'TAGP'.AND.IHNAM2(J).EQ.'LOT ')GO TO 1210
      IF(NUMNRE.GE.1)THEN
        DO 1211 K=1,NUMNRE
          IF(IHNAME(J).EQ.IRESN1(K) .AND. IHNAM2(J).EQ.IRESN2(K))   &
            GO TO 1210
 1211   CONTINUE
      ENDIF
      IF(IUSE(J).EQ.'V')IN(J)=(-1)
 1210 CONTINUE
      CALL DPUPNT(IHNAME,IHNAM2,IUSE,IVALUE,VALUE,IN,   &
      IVSTAR,IVSTOP,MAXNAM,NUMNAM,V,MAXN,MAXCOL,NUMCOL,   &
      IBUGS2,IERROR)
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1281)
 1281   FORMAT('ALL USER VARIABLES HAVE JUST BEEN DELETED.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GO TO 1000
!
!               **************************************************
!               **  STEP 13--                                   **
!               **  RESET PARAMETERS                            **
!               **************************************************
!
 1300 CONTINUE
      ISTEPN='13'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 1310 J=1,NUMNAM
      IF(IHNAME(J).EQ.'INFI'.AND.IHNAM2(J).EQ.'NITY')GO TO 1310
      IF(IHNAME(J).EQ.'PI  '.AND.IHNAM2(J).EQ.'    ')GO TO 1310
      IF(NUMNRE.GE.1)THEN
        DO 1311 K=1,NUMNRE
          IF(IHNAME(J).EQ.IRESN1(K) .AND. IHNAM2(J).EQ.IRESN2(K))   &
            GO TO 1310
 1311   CONTINUE
      ENDIF
      IF(IUSE(J).EQ.'P')IN(J)=(-1)
 1310 CONTINUE
      CALL DPUPNT(IHNAME,IHNAM2,IUSE,IVALUE,VALUE,IN,   &
      IVSTAR,IVSTOP,MAXNAM,NUMNAM,V,MAXN,MAXCOL,NUMCOL,   &
      IBUGS2,IERROR)
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1381)
 1381   FORMAT('ALL USER PARAMETERS HAVE JUST BEEN DELETED.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GO TO 1000
!
!               **************************************************
!               **  STEP 14--                                   **
!               **  RESET FUNCTIONS (STRINGS)                   **
!               **************************************************
!
 1400 CONTINUE
      ISTEPN='14'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!CCCC THE FOLLOWING LINE WAS COMMENTED OUT   SEPTEMBER 1993
!CCCC MAXCHF=1000
      NUMCHF=0
!CCCC THE FOLLOWING LINE WAS COMMENTED OUT   SEPTEMBER 1993
!CCCC MAXFUN=100
      NUMFUN=0
!CCCC THE FOLLOWING LINE WAS COMMENTED OUT   SEPTEMBER 1993
!CCCC MAXCHM=200
      NUMCHM=0
!
      DO 1410 J=1,NUMNAM
        IF(NUMNRE.GE.1)THEN
          DO 1411 K=1,NUMNRE
            IF(IHNAME(J).EQ.IRESN1(K) .AND. IHNAM2(J).EQ.IRESN2(K))   &
              GO TO 1410
 1411     CONTINUE
        ENDIF
        IF(IUSE(J).EQ.'F')IN(J)=(-1)
 1410 CONTINUE
      CALL DPUPNT(IHNAME,IHNAM2,IUSE,IVALUE,VALUE,IN,   &
      IVSTAR,IVSTOP,MAXNAM,NUMNAM,V,MAXN,MAXCOL,NUMCOL,   &
      IBUGS2,IERROR)
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1481)
 1481   FORMAT('ALL USER FUNCTIONS (= STRINGS) HAVE JUST BEEN ',   &
               'DELETED.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GO TO 1000
!
!               **************************************************
!               **  STEP 15--                                   **
!               **  RESET ALL MATRICES                          **
!               **************************************************
!
      ISTEPN='15'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 1510 J=1,NUMNAM
        IF(NUMNRE.GE.1)THEN
          DO 1511 K=1,NUMNRE
            IF(IHNAME(J).EQ.IRESN1(K) .AND. IHNAM2(J).EQ.IRESN2(K))   &
              GO TO 1510
 1511     CONTINUE
        ENDIF
        IF(IUSE(J).EQ.'M')IN(J)=(-1)
 1510 CONTINUE
      CALL DPUPNT(IHNAME,IHNAM2,IUSE,IVALUE,VALUE,IN,   &
      IVSTAR,IVSTOP,MAXNAM,NUMNAM,V,MAXN,MAXCOL,NUMCOL,   &
      IBUGS2,IERROR)
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1581)
 1581   FORMAT('ALL USER MATRICES HAVE JUST BEEN DELETED.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GO TO 1000
!
!               **************************************************
!               **  STEP 21--                                   **
!               **  RESET INPUT/OUTPUT                          **
!               **************************************************
!
 2100 CONTINUE
      ISTEPN='21'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
      GO TO 1000
!
!               **************************************************
!               **  STEP 22--                                   **
!               **  RESET PLOT CONTROL                          **
!               **************************************************
!
 2200 CONTINUE
      ISTEPN='22'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL INITPC(IBUGS2)
!
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
      GO TO 1000
!
!               **************************************************
!               **  STEP 23--                                   **
!               **  RESET CLSB (CHARACTERS, LINES,              **
!               **  SPIKES, AND BARS)                           **
!               **************************************************
!
 2300 CONTINUE
      ISTEPN='23'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!CCCC IDEFGC='BLAC'
!
      DO 2310 J=1,MAXCH2
      ICHAPA(J)='    '
!CCCC ICHAFO(J)='TEKT'
      ICHACA(J)='UPPE'
      ICHAJU(J)='CECE'
      ICHADI(J)='VERT'
      ICHAFI(J)='OFF'
!CCCC ICHACO(J)='BLAC'
      PCHAHE(J)=2.0
      PCHAWI(J)=1.0
      PCHAVG(J)=0.75
      PCHAHG(J)=0.25
!CCCC PCHATH(J)=0.1
      ACHAAN(J)=0.0
      PCHAHO(J)=0.0
      PCHAVO(J)=0.0
 2310 CONTINUE
!
      DO 2320 J=1,MAXLN
      ILINPA(J)='SOLI'
!CCCC ILINCO(J)='BLAC'
!CCCC PLINTH(J)=0.1
      PLINLE(J)=1.0
      PLINL2(J)=1.0
      PLINL3(J)=1.0
      PLINGA(J)=1.0
      PLING2(J)=1.0
      PLING3(J)=1.0
 2320 CONTINUE
!
      DO 2330 J=1,MAXSP
      ISPISW(J)='OFF'
      ISPILI(J)='SOLI'
!CCCC ISPICO(J)='BLAC'
      ISPIDI(J)='V'
!CCCC PSPITH(J)=0.1
      ASPIBA(J)=0.0
 2330 CONTINUE
!
      DO 2340 J=1,MAXBA
      IBARSW(J)='OFF'
      IBABLI(J)='SOLI'
!CCCC IBABCO(J)='BLAC'
      IBAFSW(J)='OFF'
!CCCC IBAFCO(J)=IDEFGC
      IBAPTY(J)='SOLI'
      IBAPLI(J)='SOLI'
!CCCC IBAPCO(J)=IDEFGC
      IBARTY(J)='2'
      IBARDI(J)='V'
      ABARBA(J)=0.0
      ABARWI(J)=CPUMIN
!CCCC PBABTH(J)=0.1
!CCCC PBAPTH(J)=0.1
      PBAPSP(J)=1.0
 2340 CONTINUE
!
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
      GO TO 1000
!
!               **************************************************
!               **  STEP 24--                                   **
!               **  RESET LIMITS (ON PLOTS)                     ZZ
!               **************************************************
!
 2400 CONTINUE
      ISTEPN='24'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IX1MIN='FLOA'
      IX1MAX='FLOA'
      IY1MIN='FLOA'
      IY1MAX='FLOA'
      IZ1MIN='FLOA'
      IZ1MAX='FLOA'
!
      IX2MIN='FLOA'
      IX2MAX='FLOA'
      IY2MIN='FLOA'
      IY2MAX='FLOA'
      IZ2MIN='FLOA'
      IZ2MAX='FLOA'
!
      PDXMIN=CPUMIN
      PDXMAX=CPUMAX
      PDYMIN=CPUMIN
      PDYMAX=CPUMAX
      PDZMIN=CPUMIN
      PDZMAX=CPUMAX
!
      PGXMIN=CPUMIN
      PGXMAX=CPUMAX
      PGYMIN=CPUMIN
      PGYMAX=CPUMAX
      PGZMIN=CPUMIN
      PGZMAX=CPUMAX
!
      GX1MIN=CPUMIN
      GX1MAX=CPUMAX
      GY1MIN=CPUMIN
      GY1MAX=CPUMAX
      GZ1MIN=CPUMIN
      GZ1MAX=CPUMAX
!
      GX2MIN=CPUMIN
      GX2MAX=CPUMAX
      GY2MIN=CPUMIN
      GY2MAX=CPUMAX
      GZ2MIN=CPUMIN
      GZ2MAX=CPUMAX
!
      DX1MIN=CPUMIN
      DX1MAX=CPUMAX
      DY1MIN=CPUMIN
      DY1MAX=CPUMAX
      DZ1MIN=CPUMIN
      DZ1MAX=CPUMAX
!
      DX2MIN=CPUMIN
      DX2MAX=CPUMAX
      DY2MIN=CPUMIN
      DY2MAX=CPUMAX
      DZ2MIN=CPUMIN
      DZ2MAX=CPUMAX
!
      FX1MIN=CPUMIN
      FX1MAX=CPUMAX
      FY1MIN=CPUMIN
      FY1MAX=CPUMAX
      FZ1MIN=CPUMIN
      FZ1MAX=CPUMAX
!
      FX2MIN=CPUMIN
      FX2MAX=CPUMAX
      FY2MIN=CPUMIN
      FY2MAX=CPUMAX
      FZ2MIN=CPUMIN
      FZ2MAX=CPUMAX
!
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
      GO TO 1000
!
!               **************************************************
!               **  STEP 31--                                   **
!               **  RESET SUPPORT                               **
!               **************************************************
!
 3100 CONTINUE
      ISTEPN='31'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ITEMEC=IECHO
      ITEMFE=IFEEDB
      ITEMPR=IPRINT
      CALL INITSU(IBUGS2)
!
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
      GO TO 1000
!
 1000 CONTINUE
      GO TO 9000
!
!               **************************************************
!               **  TREAT THE    RESET ALL    CASE              **
!               **  (WILL BE DONE BACK IN THE MAIN ROUNTINE)    **
!               **************************************************
!
 5100 CONTINUE
      ISTEPN='51'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
      GO TO 9000
!
!               ************
!               **  EXIT  **
!               ************
!
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
          DO 9015 I=1,NUMARG
            WRITE(ICOUT,9016)I,IHARG(I)
 9016       FORMAT('I,IHARG(I) = ',I8,2X,A4)
            CALL DPWRST('XXX','BUG ')
 9015     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPRESE
      SUBROUTINE DPRESI(IHARG,IARGT,ARG,NUMARG,   &
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
!     PURPOSE--DRAW ONE OR MORE RESISTORS (DEPENDING ON HOW MANY NUMBERS
!              ARE PROVIDED).  THE COORDINATES ARE IN STANDARDIZED UNITS
!              OF 0 TO 100.
!     NOTE--THE INPUT COORDINATES DEFINE THE 2 ENDS OF THE RESISTOR.
!     NOTE-THE USUAL INPUT NUMBER OF COORDINATES IS 2 AND THEREFORE
!          THE USUAL INPUT NUMBER OF NUMBERS IS 2*2 = 4.
!     NOTE--IF 2 NUMBERS ARE PROVIDED, THEN THE DRAWN RESISTOR WILL GO
!           FROM THE LAST CURSOR POSITION TO THE (X,Y) POINT (EITHER
!           ABSOLUTE OR RELATIVE) AS DEFINED BY THE 2 NUMBERS.
!     NOTE--IF 4 NUMBERS ARE PROVIDED, THEN THE DRAWN RESISTOR WILL GO
!           FROM THE ABSOLUTE (X,Y) POSITION AS DEFINED BY THE FIRST 2
!           NUMBERS TO THE (X,Y) POINT (EITHER ABSOLUTE OR RELATIVE)
!           AS DEFINED BY THE THIRD AND FOURTH NUMBERS.
!     NOTE--IF 6 NUMBERS ARE PROVIDED, THEN THE DRAWN RESISTOR WILL GO
!           FROM THE (X,Y) POSITION AS RESULTING FROM THE THIRD AND
!           FOURTH NUMBERS TO THE (X,Y) POINT (EITHER ABSOLUTE OR
!           RELATIVE) AS DEFINED BY THE FIFTH AND SIXTH NUMBERS.
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
!     UPDATED         --JULY      2019. CREATE SCRATCH STORAGE IN DPRESI
!                                       RATHER THAN DPRES2
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
      CHARACTER*4 IDFONT
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
      DIMENSION IDFONT(*)
      DIMENSION IDNVPP(*)
      DIMENSION IDNHPP(*)
      DIMENSION IDUNIT(*)
      DIMENSION IDNVOF(*)
      DIMENSION IDNHOF(*)
      DIMENSION IBACC2(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
      DIMENSION PX(1000)
      DIMENSION PY(1000)
      EQUIVALENCE (GARBAG(IGARB1),PX(1))
      EQUIVALENCE (GARBAG(IGARB2),PY(1))
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      IERRG4=IERROR
!
      ILOCFN=0
      NUMNUM=0
!
      X1=0.0
      Y1=0.0
      X2=0.0
      Y2=0.0
!
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
        WRITE(ICOUT,88)IBUGG4,IBUGD2,ISUBG4,IERRG4,IFOUND,IERROR
   88   FORMAT('IBUGG4,IBUGD2,ISUBG4,IERRG4,IFOUND,IERROR = ',   &
               5(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IFIG='RESI'
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
           IARGT(1).EQ.'NUMB'.AND.IARGT(2).EQ.'NUMB')THEN
          ITYPEO='ABSO'
          ILOCFN=1
        ELSEIF(NUMARG.GE.3.AND.IHARG(1).EQ.'ABSO'.AND.   &
               IARGT(2).EQ.'NUMB'.AND.IARGT(3).EQ.'NUMB')THEN
          ITYPEO='ABSO'
          ILOCFN=2
        ELSEIF(NUMARG.GE.3.AND.IHARG(1).EQ.'RELA'.AND.   &
               IARGT(2).EQ.'NUMB'.AND.IARGT(3).EQ.'NUMB')THEN
          ITYPEO='RELA'
          ILOCFN=2
        ELSE
          GO TO 1130
        ENDIF
!
        IF(ILOCFN.GT.NUMARG)GO TO 1130
        DO 1120 I=ILOCFN,NUMARG
          IF(IARGT(I).NE.'NUMB')GO TO 1130
 1120   CONTINUE
        IFOUND='YES'
!
!               ****************************
!               **  STEP 3--              **
!               **  DRAW OUT THE LINE(S)  **
!               ****************************
!
        NUMNUM=NUMARG-ILOCFN+1
        IF(NUMNUM.LT.NUMPT2)THEN
          J=ILOCFN-1
          X1=PXSTAR
          Y1=PYSTAR
        ELSE
          J=ILOCFN
          IF(J.GT.NUMARG)GO TO 1190
          X1=ARG(J)
          IF(UNITSW.EQ.'DATA')CALL DPCODS('X',X1,X1,   &
             IBUGD2,ISUBRO,IERROR)
          J=J+1
          IF(J.GT.NUMARG)GO TO 1190
          Y1=ARG(J)
          IF(UNITSW.EQ.'DATA')CALL DPCODS('Y',Y1,Y1,   &
             IBUGD2,ISUBRO,IERROR)
        ENDIF
!
 1160   CONTINUE
        J=J+1
        IF(J.GT.NUMARG)GO TO 1190
        X2=ARG(J)
        IF(UNITSW.EQ.'DATA')CALL DPCODS('X',X2,X2,IBUGD2,ISUBRO,IERROR)
        IF(ITYPEO.EQ.'RELA')X2=X1+X2
        J=J+1
        IF(J.GT.NUMARG)GO TO 1190
        Y2=ARG(J)
        IF(UNITSW.EQ.'DATA')CALL DPCODS('Y',Y2,Y2,IBUGD2,ISUBRO,IERROR)
        IF(ITYPEO.EQ.'RELA')Y2=Y1+Y2
!
        CALL DPRES2(X1,Y1,X2,Y2,PX,PY,   &
                    IFIG,ILINPA,ILINCO,ILINC2,MAXLNZ,PLINTH,   &
                    AREGBA,IREBLI,IREBCO,IREBC2,MAXRGZ,PREBTH,   &
                    IREFSW,IREFCO,IREFC2,   &
                    IREPTY,IREPLI,IREPCO,IREPC2,PREPTH,PREPSP,   &
                    PTEXHE,PTEXWI,PTEXVG,PTEXHG)
!
        X1=X2
        Y1=Y2
!
        GO TO 1160
 1190   CONTINUE
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
      GO TO 9000
!
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
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPRESI
      SUBROUTINE DPREST(IFOUND,IERROR)
!
!     PURPOSE--RESTORE (= READ IN TO MEMORY) ALL INTERNAL DATAPLOT
!              SETTINGS.  THE MASS STORAGE FILE IS DESIGNATED BY THE
!              ANALYST.  THIS IS USEFUL WHEN A RUN MUST BE INTERRUPTED
!              (E.G., LUNCH) (SEE THE SAVE COMMAND) AND IT IS DESIRED
!              TO PICK UP THE RUN LATER AT THE POINT OF INTERRUPTION
!              (SEE THE RESTORE COMMAND).
!     NOTE--THE SAVE MEMORY COMMAND (AND ITS COMPLEMENT, THE RESTORE
!           MEMORY COMMAND) BOTH USE UNFORMATTED FORTRAN I/O STATEMENTS
!           (FOR SPEED AND EFFICIENCY).
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
!     ORIGINAL VERSION--NOVEMBER  1980.
!     UPDATED         --JANUARY   1981.
!     UPDATED         --JUNE      1981.
!     UPDATED         --NOVEMBER  1981.
!     UPDATED         --JANUARY   1982.
!     UPDATED         --MARCH     1982.
!     UPDATED         --MAY       1982.
!     UPDATED         --DECEMBER  1985.
!     UPDATED         --JUNE      1986.
!     UPDATED         --NOVEMBER  1987. DIMENSION FOR I1DATA--1100 TO 100
!     UPDATED         --DECEMBER  1987. DIMENSION FOR V--10000 TO MAXOBW
!     UPDATED         --JANUARY   1989. SOFT-CODE ALL (ALAN)
!     UPDATED         --OCTOBER   1991. SUN HAS LIMIT ON # OF WORDS
!                                       FOR UNFORMATTED I/O (2,046)
!     UPDATED         --APRIL     1992. INCLUDE DPCO3D.INC (ALAN)
!     UPDATED         --APRIL     1992. PPEDHE TO APEDSZ (ALAN)
!     UPDATED         --NOVEMBER  2020. UPDATE TO REFLECT CURRENT
!                                       CONTENTS OF COMMON BLOCKS
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
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
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*30 ISECT
!
!CCCC CHARACTER*80 ICANS
      CHARACTER (LEN=MAXSTR) :: ICANS
!
!-----COMMON----------------------------------------------------------
!
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
!CCCC FOLLOWING LINE WAS ADDED  APRIL 1992 (ALAN)
      INCLUDE 'DPCO3D.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPRE'
      ISUBN2='ST  '
      ISUBRO='-999'
      IFOUND='YES'
      IERROR='NO'
!
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'REST')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPREST--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGS2,ISUBRO,IERROR,IWIDTH
   53   FORMAT('IBUGS2,ISUBRO,IERROR,IWIDTH = ',3(A4,2X),I6)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,55)(IANSLC(I),I=1,MIN(80,IWIDTH))
   55     FORMAT('(IANSLC(I),I=1,MIN(80,IWIDTH)) = ',80A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        WRITE(ICOUT,62)ISAVNA(1:80)
   62   FORMAT('ISAVNA(1:80) = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,63)ISAVST,ISAVFO,ISAVAC,ISAVCS,ISAVNU
   63   FORMAT('ISAVST,ISAVFO,ISAVAC,ISAVCS,ISAVNU = ',4(A12,2X),I6)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               **************************
!               **  STEP 11--           **
!               **  COPY OVER VARIABLES **
!               **************************
!
      ISTEPN='11'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REST')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IOUNIT=ISAVNU
      IFILE=ISAVNA
      ISTAT=ISAVST
      IFORM=ISAVFO
      IACCES=ISAVAC
      IPROT=ISAVPR
      ICURST=ISAVCS
!
      ISUBN0='REST'
      IERRFI='NO'
!
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'REST')THEN
        WRITE(ICOUT,1194)IFILE(1:80)
 1194   FORMAT('IFILE(1:80) = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1195)ISTAT,IFORM,IACCES,IPROT,ICURST
 1195   FORMAT('ISTAT,IFORM,IACCES,IPROT,ICURST = ',   &
               4(A12,2X),A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1196)ISUBN0,IERRFI,IOUNIT
 1196   FORMAT('ISUBN0,IERRFI,IOUNIT = ',2(A4,2X),I6)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *******************************************
!               **  STEP 12--                            **
!               **  CHECK TO SEE IF SAVE FILE MAY EXIST  **
!               *******************************************
!
      ISTEPN='12'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REST')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ISTAT.EQ.'NONE')THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1211)
 1211   FORMAT('***** ERROR IN DPREST--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1212)
 1212   FORMAT('      THE DESIRED SAVE CANNOT BE GIVEN BECAUSE THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1214)
 1214   FORMAT('      REQUIRED SYSTEM MASS STORAGE FILE WHICH STORES ',   &
               'SUCH SAVE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1216)
 1216   FORMAT('      IS NOT AVAILABLE AT THIS INSTALLATION.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1217)ISTAT,ISAVST
 1217   FORMAT('ISTAT,ISAVST = ',A12,2X,A12)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
!               ****************************
!               **  STEP 13--             **
!               **  EXTRACT THE FILE NAME **
!               **  (THE THIRD WORD)      **
!               ****************************
!
      ISTEPN='13'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REST')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 1310 I=1,MAXFNC
        IFILE(I:I)=' '
 1310 CONTINUE
!
      DO 1320 I=1,MAXSTR
        ICANS(I:I)=IANSLC(I)
 1320 CONTINUE
!
!     2020/11: IF FEWER THAN 2 ARGUMENTS, NO FILE NAME GIVEN, SO
!              USE DEFAULT NAME (dpsavf.tex).
!
      ISTART=1
      ISTOP=IWIDTH
      IF(NUMARG.LE.1)THEN
!CCCC    CALL DPW280(ICANS,ISTART,ISTOP,ICOL3,IBUGS2,ISUBRO,IERROR)
         DO 1325 II=1,MAXFNC
           IFILE(II:II)=ISAVNA(II:II)
 1325    CONTINUE
      ELSEIF(NUMARG.GE.2)THEN
        CALL DPW380(ICANS,ISTART,ISTOP,ICOL3,IBUGS2,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
        J=0
        IF(ICOL3.LE.IWIDTH)THEN
          DO 1330 I=ICOL3,IWIDTH
            J=J+1
            IFILE(J:J)=ICANS(I:I)
 1330     CONTINUE
        ENDIF
      ENDIF
!
      NMAX=MAXFNC
      CALL DPDB80(IFILE,JMAX,NMAX,IBUGS2,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      NCFILE=JMAX
!
      IF(NCFILE.LT.1)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1342)
 1342   FORMAT('      A FILE NAME IS REQUIRED IN THE RESTORE MEMORY ',   &
               'COMMAND.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1344)
 1344   FORMAT('      (FOR EXAMPLE,    RESTORE MEMORY DPRUN.DAT)')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1345)
 1345   FORMAT('      BUT NONE WAS GIVEN HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1346)
 1346   FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,1347)(IANSLC(I),I=1,MIN(80,IWIDTH))
 1347     FORMAT('      ',80A1)
          CALL DPWRST('XXX','BUG ')
        ELSE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GO TO 9000
      ENDIF
!
!               *********************
!               **  STEP 31--      **
!               **  OPEN THE FILE  **
!               *********************
!
      ISTEPN='31'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REST')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IREWIN='ON'
      CALL DPOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
                  IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
      IF(IERRFI.EQ.'YES')GO TO 9000
!
!               ****************************************************************
!               **  STEP 41--
!               **  READ  IN  FROM THE SAVE FILE;
!               ****************************************************************
!
      ISTEPN='41'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REST')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     -----BEGIN READING IN-----------------------
!
!     -----READ IN COMMON FOR STANDARD I/O-----
!
      ISECT='STANDARD I/O'
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IRD,IPR,CPUMIN,CPUMAX,   &
                                               NUMBPC,NUMCPW,NUMBPW
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IFEEDB,IPRINT
!
!     -----READ IN COMMON FOR MACHINE CONSTANTS-----
!
      ISECT='MACHINE CONSTANTS'
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(I1MACH(I),I=1,16)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(R1MACH(I),I=1,5)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(D1MACH(I),I=1,5)
!
!     -----READ IN COMMON FOR BUGS-----
!
      ISECT='BUGS'
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(I1BUG(I),I=1,10)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IH1BUG(I),I=1,100)
!
!     -----READ IN COMMON FOR HOUSEKEEPING-----
!
      ISECT='HOUSEKEEPING'
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)   &
          (I1HOUS(I),I=1,5*MAXNME+MAXSTR+50)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IH1HOU(I),   &
           I=1,120+3*MAXSTR+5*MAXARG+3*MAXNME)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)   &
          (R1HOUS(I),I=1,MAXSTR+MAXNME)
!
!     -----READ IN COMMON FOR DATA-----
!
      ISECT='DATA'
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(I1DATA(I),I=1,100)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ISUB(I),I=1,MAXOBV)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IPARNC(I),I=1,MAXFN2)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IPANC2(I),I=1,MAXFN2)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IPAROP(I),I=1,MAXFN2)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(MODEL(I),I=1,MAXF3)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IFUNC(I),I=1,MAXF1)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IFUNC2(I),I=1,MAXF1)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IFUNC3(I),I=1,MAXF1)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PARLIM(I),I=1,100)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PARLLM(I),I=1,100)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PARULM(I),I=1,100)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PRED(I),I=1,MAXOBV)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(RES(I),I=1,MAXOBV)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(Y(I),I=1,MAXPOP)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(X(I),I=1,MAXPOP)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(X3D(I),I=1,MAXPOP)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(D(I),I=1,MAXPOP)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(DSIZE(I),I=1,MAXPOP)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(DSYMB(I),I=1,MAXPOP)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(DCOLOR(I),I=1,MAXPOP)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(DFILL(I),I=1,MAXPOP)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(YPLOT(I),I=1,MAXPOP)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(XPLOT(I),I=1,MAXPOP)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(X2PLOT(I),I=1,MAXPOP)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(TAGPLO(I),I=1,MAXPOP)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(V(I),I=1,MAXOBW)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)   &
          ((AMATR1(I,J),I=1,100),J=1,100)
!
!     -----READ IN COMMON FOR SUPPORT-----
!
      ISECT='SUPPORT'
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(I1SUPP(I),I=1,162)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IFORWI(I),I=1,200)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IFORWR(I),I=1,200)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IMACLL(I),I=1,50)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IMACNC(I),I=1,50)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IH1SUP(I),I=1,250)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)   &
            ICONCH,IDEFCC,IMACSC,IDEFMS,   &
            ICOMCH,IDEFCZ,ICOMSW,   &
            IVCFMT,IDEFVF,IVCARR,IDEFVA,IVCOPN,IDEFVO,   &
            IFRATY,IDEFFT,IPCMTY,IDEFPT,ICLIFL,ICLILO,   &
            IMACAR,IMACLA,IMACCL
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IRESN1(I),I=1,MAXRES),   &
            (IRESN2(I),I=1,MAXRES),NUMNRE
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(R1SUPP(I),I=1,60)
!
!     -----READ IN COMMON FOR SUBFILE I/O (UNIVAC ONLY)-----
!
      ISECT='SUBFILE I/O'
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IBUF(I),I=1,504)
!
!     -----READ IN COMMON FOR DIAGRAMMATIC GRAPHICS-----
!
      ISECT='DIAGRAMMATIC GRAPHICS'
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IH1DIA(I),I=1,40)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(R1DIAG(I),I=1,40)
!
!     -----READ IN COMMON FOR COLOR-----
!
      ISECT='COLOR'
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICOLOR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IPLOTF
!
!     -----READ IN COMMON FOR BUGS AND ERROR-----
!
      ISECT='BUGS AND ERROR'
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IBUGG4
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISUBG4
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IERRG4
!
!     -----READ IN COMMON FOR HOST-----
!
      ISECT='HOST'
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHOST1
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHOST2
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHMOD1
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHMOD2
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IOPSY1
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IOPSY2
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICOMPI
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISITE
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IWBFLG
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IBROWS
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDPURL
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IURL
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHBURL
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)NCHURL
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IGUIIO
!
!     -----READ IN COMMON FOR TRANSLATOR-----
!
      ISECT='TRANSLATOR'
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ITRANS
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)NCTRA1
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)NCTRA2
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)NUMTRA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICTRA1
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICTRA2
!
!     -----READ IN COMMON FOR NON-PRINTING CHARACTERS-----
!
      ISECT='NON-PRINTING CHARACTERS'
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)INULC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISOHC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISTXC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IETXC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IEOTC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IENQC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IACKC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IBELC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IBSC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHTC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ILFC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IVTC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IFFC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICRC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISOC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISIC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDLEC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDC1C
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDC2C
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDC3C
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDC4C
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)INAKC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISYNC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IETBC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICANC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IEMC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISUBC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IESCC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IFSC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IGSC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IRSC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IUSC
!
!     -----READ IN COMMON FOR GRAPHICS-----
!
      ISECT='GRAPHICS'
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IMANUF
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IMODEL
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IMODE2
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IMODE3
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IGCODE
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IGUNIT
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IGCONT
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)NUMHPP
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)NUMVPP
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ANUMHP
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ANUMVP
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IGCOLO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IGBAUD
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)AGERDE
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)AGCODE
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISOFT
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISOFT2
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISOFT3
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IPPDE1
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IPPDE2
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICPREP
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICPOST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)NCPREP
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)NCPOST
!
!     -----READ IN COMMON FOR FILE OPERATIONS-----
!
      ISECT='FILE OPERATIONS'
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(I1FILO(I),I=1,10)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IH1FIL(I),I=1,11000)
!
!     -----READ IN COMMON FOR FILE OPERATIONS, PART 2-----
!
      ISECT='FILE OPERATIONS PART 2'
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IMESNU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IMESNA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IMESST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IMESFO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IMESAC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IMESPR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IMESCS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)INEWNU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)INEWNA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)INEWST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)INEWFO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)INEWAC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)INEWPR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)INEWCS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IMAINU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IMAINA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IMAIST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IMAIFO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IMAIAC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IMAIPR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IMAICS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHELNU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHELNA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHELST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHELFO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHELAC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHELPR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHELCS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IBUGNU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IBUGNA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IBUGST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IBUGFO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IBUGAC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IBUGPR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IBUGCS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IQUENU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IQUENA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IQUEST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IQUEFO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IQUEAC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IQUEPR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IQUECS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ILOGNU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ILOGNA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ILOGST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ILOGFO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ILOGAC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ILOGPR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ILOGCS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISYSNU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISYSNA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISYSST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISYSFO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISYSAC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISYSPR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISYSCS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDIRNU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDIRNA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDIRST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDIRFO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDIRAC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDIRPR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDIRCS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDICNU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDICNA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDICST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDICFO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDICAC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDICPR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDICCS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IREANU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IREANA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IREAST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IREAFO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IREAAC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IREAPR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IREACS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IWRINU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IWRINA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IWRIST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IWRIFO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IWRIAC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IWRIPR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IWRICS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISAVNU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISAVNA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISAVST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISAVFO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISAVAC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISAVPR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISAVCS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ILISNU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ILISNA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ILISST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ILISFO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ILISAC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ILISPR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ILISCS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICRENU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICRENA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICREST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICREFO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICREAC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICREPR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICRECS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICREN2
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICREN2
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICRES2
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICREF2
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICREA2
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICREP2
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICREC2
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICAPNU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICAPNA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICAPST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICAPFO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICAPAC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICAPPR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICAPCS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICPNU2
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICPNA2
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICPST2
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICPFO2
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICPAC2
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICPPR2
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICPCS2
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IREAN1
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IRENA1
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IREAS1
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IREAF1
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IREAA1
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IREAP1
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IREAC1
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IREAN2
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IRENA2
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IREAS2
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IREAF2
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IREAA2
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IREAP2
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IREAC2
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IREAN3
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IRENA3
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IREAS3
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IREAF3
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IREAA3
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IREAP3
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IREAC3
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IWRIN1
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IWRNA1
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IWRIS1
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IWRIF1
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IWRIA1
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IWRIP1
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IWRIC1
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IWRIN2
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IWRNA2
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IWRIS2
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IWRIF2
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IWRIA2
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IWRIP2
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IWRIC2
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IWRIN3
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IWRNA3
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IWRIS3
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IWRIF3
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IWRIA3
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IWRIP3
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IWRIC3
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISCRNU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISCRNA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISCRST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISCRFO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISCRAC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISCRPR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISCRCS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDATNU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDATNA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDATST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDATFO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDATAC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDATPR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDATCS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IPL1NU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IPL1NA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IPL1ST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IPL1FO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IPL1AC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IPL1PR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IPL1CS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IPL2NU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IPL2NA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IPL2ST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IPL2FO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IPL2AC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IPL2PR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IPL2CS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IPRONU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IPRONA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IPROST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IPROFO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IPROAC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IPROPR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IPROCS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICONNU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICONNA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICONST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICONFO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICONAC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICONPR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ICONCS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISACNU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISACNA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISACST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISACFO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISACAC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISACPR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISACCS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IEX1NU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IEX1NA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IEX1ST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IEX1FO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IEX1AC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IEX1PR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IEX1CS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IEX2NU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IEX2NA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IEX2ST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IEX2FO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IEX2AC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IEX2PR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IEX2CS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IEX3NU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IEX3NA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IEX3ST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IEX3FO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IEX3AC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IEX3PR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IEX3CS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IEX4NU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IEX4NA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IEX4ST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IEX4FO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IEX4AC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IEX4PR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IEX4CS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IEX5NU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IEX5NA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IEX5ST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IEX5FO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IEX5AC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IEX5PR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IEX5CS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE1NU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE1NA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE1ST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE1FO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE1AC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE1PR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE1CS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE2NU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE2NA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE2ST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE2FO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE2AC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE2PR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE2CS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE3NU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE3NA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE3ST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE3FO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE3AC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE3PR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE3CS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE4NU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE4NA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE4ST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE4FO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE4AC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE4PR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE4CS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE5NU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE5NA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE5ST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE5FO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE5AC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE5PR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE5CS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE6NU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE6NA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE6ST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE6FO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE6AC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE6PR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE6CS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE7NU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE7NA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE7ST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE7FO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE7AC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE7PR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE7CS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE8NU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE8NA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE8ST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE8FO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE8AC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE8PR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE8CS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE9NU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE9NA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE9ST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE9FO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE9AC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE9PR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHE9CS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHRMNU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHRMNA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHRMST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHRMFO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHRMAC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHRMPR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHRMCS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHHBNU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHHBNA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHHBST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHHBFO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHHBAC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHHBPR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHHBCS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IST1NU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IST1NA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IST1ST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IST1FO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IST1AC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IST1PR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IST1CS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IST2NU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IST2NA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IST2ST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IST2FO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IST2AC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IST2PR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IST2CS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IST3NU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IST3NA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IST3ST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IST3FO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IST3AC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IST3PR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IST3CS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IST4NU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IST4NA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IST4ST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IST4FO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IST4AC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IST4PR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IST4CS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IST5NU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IST5NA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IST5ST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IST5FO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IST5AC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IST5PR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IST5CS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IFCHAR
!
!     -----READ IN COMMON FOR PLOT CONTROL-----
!
      ISECT='PLOT CONTROL'
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IDMANU(I),I=1,MAXDV)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IDMODE(I),I=1,MAXDV)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IDMOD2(I),I=1,MAXDV)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IDMOD3(I),I=1,MAXDV)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IDPOWE(I),I=1,MAXDV)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IDCONT(I),I=1,MAXDV)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IDCOLO(I),I=1,MAXDV)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IDSCRE(I),I=1,MAXDV)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IDSCRO(I),I=1,MAXDV)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IDPAER(I),I=1,MAXDV)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IDSEGM(I),I=1,MAXDV)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IDSOFT(I),I=1,MAXDV)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IDSOF2(I),I=1,MAXDV)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IDSOF3(I),I=1,MAXDV)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IDFONT(I),I=1,MAXDV)
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IDCODE(I),I=1,MAXDV)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IDUNIT(I),I=1,MAXDV)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IDNHPP(I),I=1,MAXDV)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IDNVPP(I),I=1,MAXDV)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IDBAUD(I),I=1,MAXDV)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IDNVOF(I),I=1,MAXDV)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IDNHOF(I),I=1,MAXDV)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)NUMDEV,MAXDEV
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IERASW,IBELSW,ISORSW,   &
                                               ICOPSW
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IPENSW
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IBACCO,IMARCO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDEFXC,IDEFBK,IDEFMC,   &
                                               IDEPEC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISEQSW
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IFENSW
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)INEGSW
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IVISSW,IPEDSW,IPEDCO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDEFMA,IDEFMO,IDEFM2,   &
                                               IDEFM3
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDEFPO,IDEFCN,IDEFDC,   &
                                               IDEFTU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IHORSW,IMPSW2,IEMBJH,   &
                                               IEMBJV
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)NUMRIN,NUMCOP
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)NUMSEQ
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDEFVP,IDEFHP,IDEFUN
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)BAWIDT,BARSPA,DEFBAS
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)DEFSZ,DEFTL,DEFTOF
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PDSCAL(I),I=1,MAXDV)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)AMPSCH,AMPSCW
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IGRASW
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PGRAXO,PGRAYO,PGRAXC,   &
                                               PGRAYC,PGRAXN,PGRAYN
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PMARXC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PGRAXF,PGRAYF
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PCROXC,PCROYC
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDIASW
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PDIAXC,PDIAYC,   &
                                               PDIAX2,PDIAY2
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PDIAHE,PDIAWI,   &
                                               PDIAVG,PDIAHG
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PWXMIN,PWXMAX,   &
                                               PWYMIN,PWYMAX
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)WWXMIN,WWXMAX,   &
                                               WWYMIN,WWYMAX
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IX1MIN,IX1MAX,   &
                                               IY1MIN,IY1MAX
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IX2MIN,IX2MAX,   &
                                               IY2MIN,IY2MAX
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PXMIN,PXMAX,PYMIN,PYMAX
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PDXMIN,PDXMAX,   &
                                               PDYMIN,PDYMAX
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PGXMIN,PGXMAX,   &
                                               PGYMIN,PGYMAX
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)GX1MIN,GX1MAX,   &
                                               GY1MIN,GY1MAX
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)GX2MIN,GX2MAX,   &
                                               GY2MIN,GY2MAX
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)DX1MIN,DX1MAX,   &
                                               DY1MIN,DY1MAX
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)DX2MIN,DX2MAX,   &
                                               DY2MIN,DY2MAX
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)FX1MIN,FX1MAX,   &
                                               FY1MIN,FY1MAX
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)FX2MIN,FX2MAX,   &
                                               FY2MIN,FY2MAX
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)FZ1MIN,FZ1MAX,   &
                                               FZ2MIN,FZ2MAX
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)FX1MNZ,FX1MXZ,   &
                                               FY1MNZ,FY1MXZ
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)FX2MNZ,FX2MXZ,   &
                                               FY2MNZ,FY2MXZ
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)FZ1MNZ,FZ1MXZ,   &
                                               FZ2MNZ,FZ2MXZ
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IX1FSW,IX2FSW,   &
                                               IY1FSW,IY2FSW
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IX1FPA,IX2FPA,   &
                                               IY1FPA,IY2FPA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IX1FCO,IX2FCO,   &
                                               IY1FCO,IY2FCO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IZ1FSW,IZ2FSW,   &
                                               IZ1FPA,IZ2FPA,   &
                                               IZ1FCO,IZ2FCO
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PFRATH
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IX1TSW,IX2TSW,   &
                                               IY1TSW,IY2TSW
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IX1JSW,IX2JSW,   &
                                               IY1JSW,IY2JSW
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IX1NSW,IX2NSW,   &
                                               IY1NSW,IY2NSW
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IX1TSC,IX2TSC,   &
                                               IY1TSC,IY2TSC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IX1TJU,IX2TJU,   &
                                               IY1TJU,IY2TJU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IX1TCO,IX2TCO,   &
                                               IY1TCO,IY2TCO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IZITSW,IZ2TSW,   &
                                               IZ1JSW,IZ2JSW,   &
                                               IZ1NSW,IZ2NSW,   &
                                               IZ1TSC,IZ2TSC,   &
                                               IZ1TJU,IZ2TJU,   &
                                               IZ1TCO,IZ2TCO
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)NMJX1T,NMJX2T,   &
                                               NMJY1T,NMJY2T
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)NMNX1T,NMNX2T,   &
                                               NMNY1T,NMNY2T
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)NX1COO,NX2COO,   &
                                               NY1COO,NY2COO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)NX1CMN,NX2CMN,   &
                                               NY1CMN,NY2CMN
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)MAXTIC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ITICUN,ITICX1,ITICX2,   &
                                               ITICY1,ITICY2
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PX1COO(I),I=1,MAXTC)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PX2COO(I),I=1,MAXTC)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PY1COO(I),I=1,MAXTC)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PY2COO(I),I=1,MAXTC)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(X1COOR(I),I=1,MAXTC)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(X2COOR(I),I=1,MAXTC)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(Y1COOR(I),I=1,MAXTC)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(Y2COOR(I),I=1,MAXTC)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PX1CMN(I),I=1,MAXTC)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PX2CMN(I),I=1,MAXTC)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PY1CMN(I),I=1,MAXTC)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PY2CMN(I),I=1,MAXTC)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(X1COMN(I),I=1,MAXTC)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(X2COMN(I),I=1,MAXTC)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(Y1COMN(I),I=1,MAXTC)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(Y2COMN(I),I=1,MAXTC)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PX1TLE,PX2TLE,   &
                                               PY1TLE,PY2TLE
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PTICTH,PMNTFA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PZ1COO(I),I=1,MAXTC)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PZ2COO(I),I=1,MAXTC)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(Z1COOR(I),I=1,MAXTC)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(Z2COOR(I),I=1,MAXTC)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PZ1CMN(I),I=1,MAXTC)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PZ2CMN(I),I=1,MAXTC)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(Z1COMN(I),I=1,MAXTC)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(Z2COMN(I),I=1,MAXTC)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PX1TLE,PX2TLE,   &
                                               PY1TLE,PY2TLE,   &
                                               PZ1TLE,PZ2TLE,   &
                                               PTICTH,PMNTFA,   &
                                               PX1TOL,PX2TOL,   &
                                               PY1TOB,PY2TOB,   &
                                               PX1TOR,PX2TOR,   &
                                               PY1TOT,PY2TOT
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IX1ZSW,IX2ZSW,   &
                                               IY1ZSW,IY2ZSW
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IX1ZFO,IX2ZFO,   &
                                               IY1ZFO,IY2ZFO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IX1ZCA,IX2ZCA,   &
                                               IY1ZCA,IY2ZCA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IX1ZJU,IX2ZJU,   &
                                               IY1ZJU,IY2ZJU
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IX1ZDI,IX2ZDI,   &
                                               IY1ZDI,IY2ZDI
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IX1ZFI,IX2ZFI,   &
                                               IY1ZFI,IY2ZFI
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IX1ZCO,IX2ZCO,   &
                                               IY1ZCO,IY2ZCO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IZ1ZSW,IZ2ZSW,   &
                                               IZ1ZCN,IZ2ZCN,   &
                                               IZ1ZFM,IZ2ZFM,   &
                                               IZ1ZFO,IZ2ZFO,   &
                                               IZ1ZCA,IZ2ZCA,   &
                                               IZ1ZJU,IZ2ZJU,   &
                                               IZ1ZDI,IZ2ZDI,   &
                                               IZ1ZFI,IZ2ZFI,   &
                                               IZ1ZCO,IZ2ZCO
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IX1ZDP,IX2ZDP,   &
                                               IY1ZDP,IY2ZDP,IDEFDP,   &
                                               IZ1ZDP,IZ2ZDP
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PX1ZDS,PX2ZDS,   &
                                               PY1ZDS,PY2ZDS
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)AX1ZAN,AX2ZAN,   &
                                               AY1ZAN,AY2ZAN
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PX1ZHE,PX1ZWI,   &
                                               PX1ZVG,PX1ZHG
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PX2ZHE,PX2ZWI,   &
                                               PX2ZVG,PX2ZHG
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PY1ZHE,PY1ZWI,   &
                                               PY1ZVG,PY1ZHG
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PY2ZHE,PY2ZWI,   &
                                               PY2ZVG,PY2ZHG
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PTIZTH
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PZ1ZDS,PZ2ZDS,   &
                                               AZ1ZAN,AZ2AN
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PX1ZHE,PX1ZWI,   &
                                               PX1ZVG,PX1ZHG,   &
                                               PX2ZHE,PX2ZWI,   &
                                               PX2ZVG,PX2ZHG,   &
                                               PY1ZHE,PY1ZWI,   &
                                               PY1ZVG,PY1ZHG,   &
                                               PY2ZHE,PY2ZWI,   &
                                               PY2ZVG,PY2ZHG,   &
                                               PZ1ZHE,PZ1ZWI,   &
                                               PZ1ZVG,PZ1ZHG,   &
                                               PZ2ZHE,PZ2ZWI,   &
                                               PZ2ZVG,PZ2ZHG,   &
                                               PTIZTH,   &
                                               PX1ZGA,PX2ZGA,   &
                                               PY1ZGA,PY2ZGA
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IVGRSW,IHGRSW
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IVGRPA,IHGRPA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IVGRCO,IHGRCO
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PVGRTH,PHGRTH
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ITITTE(I),I=1,130)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ITITFO,ITITCA,   &
                                               ITITFI,ITITCO
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)NCTITL
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PTITHE,PTITWI,PTITVG,   &
                                               PTITHG,PTITTH,PTITDS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IX1LTE(I),I=1,MAXCH)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IX1LFO,IX1LCA,IX1LFI,   &
                                               IX1LCO,IX1LJU,IX1LDI
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IX2LTE(I),I=1,MAXCH)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IX2LFO,IX2LCA,IX2LFI,   &
                                               IX2LCO,IX2LJU,IX2LDI
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IX3LTE(I),I=1,MAXCH)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IX3LFO,IX3LCA,IX3LFI,   &
                                               IX3LCO,IX3LJU,IX3LDI
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IY1LTE(I),I=1,MAXCH)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IY1LFO,IY1LCA,IY1LFI,   &
                                               IY1LCO,IY1LJU,IY1LDI
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IY2LTE(I),I=1,MAXCH)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IY2LFO,IY2LCA,IY2LFI,   &
                                               IY2LCO,IY2LJU,IY2LDI
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IZ1LTE(I),I=1,MAXCH)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IZ1LFO,IZ1LCA,IZ1LFI,   &
                                               IZ1LCO,IZ1LJU,IZ1LDI
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IZ2LTE(I),I=1,MAXCH)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IZ2LFO,IZ2LCA,IZ2LFI,   &
                                               IZ2LCO,IZ2LJU,IZ2LDI
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)NCX1LA,NCX2LA,NCX3LA,   &
                                               NCY1LA,NCY2LA,   &
                                               NCZ1LA,NCZ2LA
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PX1LHE,PX1LWI,PX1LVG,   &
                                               PX1LHG,PX1LTH,PX1LDS,   &
                                               PX1LOF,PX1LAN
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PX2LHE,PX2LWI,PX2LVG,   &
                                               PX2LHG,PX2LTH,PX2LDS,   &
                                               PX2LOF,PX2LAN
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PX3LHE,PX3LWI,PX3LVG,   &
                                               PX3LHG,PX3LTH,PX3LDS,   &
                                               PX3LOF,PX3LAN
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PY1LHE,PY1LWI,PY1LVG,   &
                                               PY1LHG,PY1LTH,PY1LDS,   &
                                               PY1LOF,PY1LAN
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PY2LHE,PY2LWI,PY2LVG,   &
                                               PY2LHG,PY2LTH,PY2LDS,   &
                                               PY2LOF,PY2LAN
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PZ1LHE,PZ1LWI,PZ1LVG,   &
                                               PZ1LHG,PZ1LTH,PZ1LDS,   &
                                               PZ1LOF,PZ1LAN
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PZ2LHE,PZ2LWI,PZ2LVG,   &
                                               PZ2LHG,PZ2LTH,PZ2LDS,   &
                                               PZ2LOF,PZ2LAN
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PX1LXC,PX1LYC,   &
                                               PX2LXC,PX2LYC,   &
                                               PX3LXC,PX3LYC,   &
                                               PY1LXC,PY1LYC,   &
                                               PY2LXC,PY2LYC,   &
                                               PDEFDS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ILEGTE(I),I=1,MAXLG2)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ILEGFO(I),I=1,MAXLG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ILEGCA(I),I=1,MAXLG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ILEGJU(I),I=1,MAXLG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ILEGDI(I),I=1,MAXLG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ILEGFI(I),I=1,MAXLG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ILEGCO(I),I=1,MAXLG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ILEGNA(I),I=1,MAXLG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ILEGUN(I),I=1,MAXLG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDEFUZ
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ILEGST(I),I=1,MAXLG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ILEGSP(I),I=1,MAXLG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)NCLEG,MXCLEG
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)NUMLEG,MAXLEG
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PLEGXC(I),I=1,MAXLG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PLEGYC(I),I=1,MAXLG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PLEGHE(I),I=1,MAXLG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PLEGWI(I),I=1,MAXLG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PLEGVG(I),I=1,MAXLG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PLEGHG(I),I=1,MAXLG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PLEGTH(I),I=1,MAXLG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ALEGAN(I),I=1,MAXLG)
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IBOBFI(I),I=1,MAXBX)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IBOBCO(I),I=1,MAXBX)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IBOBPA(I),I=1,MAXBX)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IBOPPA(I),I=1,MAXBX)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IBOPCO(I),I=1,MAXBX)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IBOFPA(I),I=1,MAXBX)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IBOFCO(I),I=1,MAXBX)
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)NUMBOX,MAXBOX,   &
                                               PDEFSH,PDEFSW
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)   &
           ((PBOXXC(I,J),I=1,MAXBX),J=1,2)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)   &
           ((PBOXYC(I,J),I=1,MAXBX),J=1,2)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PBOPTH(I),I=1,MAXBX)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PBOPGA(I),I=1,MAXBX)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PBOFTH(I),I=1,MAXBX)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PBOSHE(I),I=1,MAXBX)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PBOSWI(I),I=1,MAXBX)
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IARRPA(I),I=1,MAXAR)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IARRCO(I),I=1,MAXAR)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IARHFI(I),I=1,MAXAR)
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)NUMARR,MAXARR
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)   &
           ((PARRXC(I,J),I=1,MAXAR),J=1,2)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)   &
           ((PARRYC(I,J),I=1,MAXAR),J=1,2)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PARRTH(I),I=1,MAXAR)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PARHLE(I),I=1,MAXAR)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PARHWI(I),I=1,MAXAR)
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ISEGPA(I),I=1,MAXSG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ISEGCO(I),I=1,MAXSG)
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)NUMSEG,MAXSEG
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)   &
          ((PSEGXC(I,J),I=1,MAXSG),J=1,2)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)   &
          ((PSEGYC(I,J),I=1,MAXSG),J=1,2)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PSEGTH(I),I=1,MAXSG)
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ILINPA(I),I=1,MAXLN)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ILINCO(I),I=1,MAXLN)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ILINPO(I),I=1,MAXLN)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ILINTY(I),I=1,MAXLN)
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)MAXLIN
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PLINTH(I),I=1,MAXLN)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PLINLE(I),I=1,MAXLN)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PLINL2(I),I=1,MAXLN)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PLINL3(I),I=1,MAXLN)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PLINGA(I),I=1,MAXLN)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PLING2(I),I=1,MAXLN)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PLING3(I),I=1,MAXLN)
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ICHAPA(I),I=1,MAXCH2)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ICHAFO(I),I=1,MAXCH2)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ICHACA(I),I=1,MAXCH2)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ICHAJU(I),I=1,MAXCH2)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ICHADI(I),I=1,MAXCH2)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ICHAFI(I),I=1,MAXCH2)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ICHACO(I),I=1,MAXCH2)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ICHAPO(I),I=1,MAXCH2)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ICHATY(I),I=1,MAXCH2)
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)MAXCHA,ICHMAP
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PCHAHE(I),I=1,MAXCH2)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PCHAWI(I),I=1,MAXCH2)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PCHAVG(I),I=1,MAXCH2)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PCHAHG(I),I=1,MAXCH2)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PCHATH(I),I=1,MAXCH2)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ACHAAN(I),I=1,MAXCH2)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PCHAHO(I),I=1,MAXCH2)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PCHAVO(I),I=1,MAXCH2)
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ITEXTE(I),I=1,MAXCH)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ITEXPA,ITEXFO,ITEXCA,   &
                                               ITEXJU,ITEXDI,ITEXAU,   &
                                               ITEXFI,ITEXCO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDEFPA,IDEFFO,IDEFCA,   &
                                               IDEFJU,IDEFDI,IDEFAU,   &
                                               IDEFFI,IDEFCO
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ITEXCR,ITEXLF
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDEFCR,IDEFLF
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ITEXSY,ITEXSP
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDEFSY,IDEFSP
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)NCTEXT,MXCTEX
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PTEXHE,PTEXWI,   &
                                               PTEXVG,PTEXHG
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PTEXTH,PTEXLE,ATEXAN
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PDEFHE,PDEFWI,   &
                                               PDEFVG,PDEFHG
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PDEFTH,PDEFLE,ADEFAN
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PTEXMR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PDEFMR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PXSTAR,PYSTAR
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PXEND,PYEND
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IFILSW(I),I=1,MAXFL)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IFILPA(I),I=1,MAXFL)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IFILCO(I),I=1,MAXFL)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDEFFS
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDEFFP
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDEFFC
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)MAXFIL
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PFILSP(I),I=1,MAXFL)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PFILTH(I),I=1,MAXFL)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(AFILBA(I),I=1,MAXFL)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PDEFFG
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PDEFFT
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ADEFFB
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IPATSW(I),I=1,MAXPT)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IPATPA(I),I=1,MAXPT)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IPATLI(I),I=1,MAXPT)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IPATCO(I),I=1,MAXPT)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDEFPS
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDEFPP
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDEFPL
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDEFPC
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)MAXPAT
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PPATHE(I),I=1,MAXPT)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PPATWI(I),I=1,MAXPT)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PPATSP(I),I=1,MAXPT)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PPATTH(I),I=1,MAXPT)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PDEFPH
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PDEFPW
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PDEFPG
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PDEFPT
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ISPISW(I),I=1,MAXSP)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ISPILI(I),I=1,MAXSP)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ISPICO(I),I=1,MAXSP)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ISPIDI(I),I=1,MAXSP)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDEFSS
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDEFSL
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDEFSC
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)MAXSPI
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PSPITH(I),I=1,MAXSP)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ASPIBA(I),I=1,MAXSP)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PDEFST
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ADEFSB
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IBARSW(I),I=1,MAXBA)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IBABLI(I),I=1,MAXBA)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IBABCO(I),I=1,MAXBA)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IBAFSW(I),I=1,MAXBA)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IBAFCO(I),I=1,MAXBA)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IBAPTY(I),I=1,MAXBA)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IBAPLI(I),I=1,MAXBA)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IBAPCO(I),I=1,MAXBA)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IBARTY(I),I=1,MAXBA)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IBARDI(I),I=1,MAXBA)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IBARBA(I),I=1,MAXBA)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDEBSW
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDEBBL
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDEBBC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDEBFS
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDEBFC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDEBPT
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDEBPL
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDEBPC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDEBTY
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDEBDI
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)MAXBAR
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ABARBA(I),I=1,MAXBA)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ABARWI(I),I=1,MAXBA)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PBABTH(I),I=1,MAXBA)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PBAPTH(I),I=1,MAXBA)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PBAPSP(I),I=1,MAXBA)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ADEBBA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ADEBWI
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PDEBBT
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PDEBPT
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PDEBPS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IREGSW(I),I=1,MAXRG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IREBLI(I),I=1,MAXRG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IREBCO(I),I=1,MAXRG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IREFSW(I),I=1,MAXRG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IREFCO(I),I=1,MAXRG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IREPTY(I),I=1,MAXRG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IREPLI(I),I=1,MAXRG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IREPCO(I),I=1,MAXRG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDERSW
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDERBL
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDERBC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDERFS
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDERFC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDERPT
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDERPL
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDERPC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IREBIN,IREBPL
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)MAXREG
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(AREGBA(I),I=1,MAXRG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(AREGWI(I),I=1,MAXRG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PREBTH(I),I=1,MAXRG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PREPTH(I),I=1,MAXRG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PREPSP(I),I=1,MAXRG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ADERBA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ADERWI
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PDERBT
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PDERPT
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PDERPS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IMARSW(I),I=1,MAXMR)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IMABLI(I),I=1,MAXMR)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IMABCO(I),I=1,MAXMR)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IMAFSW(I),I=1,MAXMR)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IMAFCO(I),I=1,MAXMR)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IMAPTY(I),I=1,MAXMR)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IMAPLI(I),I=1,MAXMR)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IMAPCO(I),I=1,MAXMR)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDEMSW
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDEMBL
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDEMBC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDEMFS
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDEMFC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDEMPT
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDEMPL
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDEMPC
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)MAXMAR
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(AMARBA(I),I=1,MAXMR)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(AMARWI(I),I=1,MAXMR)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PMABTH(I),I=1,MAXMR)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PMAPTH(I),I=1,MAXMR)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PMAPSP(I),I=1,MAXMR)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ADEMBA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ADEMWI
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PDEMBT
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PDEMPT
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PDEMPS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ITEXSW(I),I=1,MAXTX)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ITEBLI(I),I=1,MAXTX)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ITEBCO(I),I=1,MAXTX)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ITEFSW(I),I=1,MAXTX)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ITEFCO(I),I=1,MAXTX)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ITEPTY(I),I=1,MAXTX)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ITEPLI(I),I=1,MAXTX)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ITEPCO(I),I=1,MAXTX)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDETSW
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDETBL
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDETBC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDETFS
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDETFC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDETPT
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDETPL
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDETPC
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)IDETLF
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)MAXTEX
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ATEXBA(I),I=1,MAXTX)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ATEXWI(I),I=1,MAXTX)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PTEBTH(I),I=1,MAXTX)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PTEPTH(I),I=1,MAXTX)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(PTEPSP(I),I=1,MAXTX)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ADETBA
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ADETWI
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PDETBT
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PDETPT
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)PDETPS
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ASUBXL(I),I=1,MAXSUB)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ASUBXU(I),I=1,MAXSUB)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ASUBYL(I),I=1,MAXSUB)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ASUBYU(I),I=1,MAXSUB)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ISUBTY(I),I=1,MAXSUB)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ISUBSW(I),I=1,MAXSUB)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)ISUBNU,IDEFSB
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IX1TC2(I),I=1,3)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IX2TC2(I),I=1,3)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IY1TC2(I),I=1,3)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IY2TC2(I),I=1,3)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IZ1TC2(I),I=1,3)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IZ2TC2(I),I=1,3)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IX1FC2(I),I=1,3)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IX2FC2(I),I=1,3)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IY1FC2(I),I=1,3)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IY2FC2(I),I=1,3)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IZ1FC2(I),I=1,3)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IZ2FC2(I),I=1,3)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IX1ZC2(I),I=1,3)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IX2ZC2(I),I=1,3)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IY1ZC2(I),I=1,3)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IY2ZC2(I),I=1,3)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IZ1ZC2(I),I=1,3)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IZ2ZC2(I),I=1,3)
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IVGRC2(I),I=1,3)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IHGRC2(I),I=1,3)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ITITC2(I),I=1,3)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IMARC2(I),I=1,3)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IBACC2(I),I=1,3)
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IX1LC2(I),I=1,3)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IX2LC2(I),I=1,3)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IX3LC2(I),I=1,3)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IY1LC2(I),I=1,3)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IY2LC2(I),I=1,3)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IZ1LC2(I),I=1,3)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IZ2LC2(I),I=1,3)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ITEXC2(I),I=1,3)
!
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ILEGC2(I,1),I=1,MAXLG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ILEGC2(I,2),I=1,MAXLG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ILEGC2(I,3),I=1,MAXLG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IBOBC2(I,1),I=1,MAXBX)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IBOBC2(I,2),I=1,MAXBX)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IBOBC2(I,3),I=1,MAXBX)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IBOFC2(I,1),I=1,MAXBX)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IBOFC2(I,2),I=1,MAXBX)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IBOFC2(I,3),I=1,MAXBX)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IARRC2(I,1),I=1,MAXAR)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IARRC2(I,2),I=1,MAXAR)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IARRC2(I,3),I=1,MAXAR)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ISEGC2(I,1),I=1,MAXSG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ISEGC2(I,2),I=1,MAXSG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ISEGC2(I,3),I=1,MAXSG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ILINC2(I,1),I=1,MAXLN)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ILINC2(I,2),I=1,MAXLN)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ILINC2(I,3),I=1,MAXLN)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ICHAC2(I,1),I=1,MAXLN)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ICHAC2(I,2),I=1,MAXLN)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ICHAC2(I,3),I=1,MAXLN)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IFILC2(I,1),I=1,MAXFL)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IFILC2(I,2),I=1,MAXFL)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IFILC2(I,3),I=1,MAXFL)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IPATC2(I,1),I=1,MAXPT)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IPATC2(I,2),I=1,MAXPT)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IPATC2(I,3),I=1,MAXPT)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ISPIC2(I,1),I=1,MAXSP)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ISPIC2(I,2),I=1,MAXSP)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ISPIC2(I,3),I=1,MAXSP)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IBABC2(I,1),I=1,MAXBA)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IBABC2(I,2),I=1,MAXBA)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IBABC2(I,3),I=1,MAXBA)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IBAFC2(I,1),I=1,MAXBA)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IBAFC2(I,2),I=1,MAXBA)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IBAFC2(I,3),I=1,MAXBA)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IBAPC2(I,1),I=1,MAXBA)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IBAPC2(I,2),I=1,MAXBA)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IBAPC2(I,3),I=1,MAXBA)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IREBC2(I,1),I=1,MAXRG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IREBC2(I,2),I=1,MAXRG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IREBC2(I,3),I=1,MAXRG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IREFC2(I,1),I=1,MAXRG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IREFC2(I,2),I=1,MAXRG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IREFC2(I,3),I=1,MAXRG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IREPC2(I,1),I=1,MAXRG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IREPC2(I,2),I=1,MAXRG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IREPC2(I,3),I=1,MAXRG)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IMABC2(I,1),I=1,MAXMR)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IMABC2(I,2),I=1,MAXMR)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IMABC2(I,3),I=1,MAXMR)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IMAFC2(I,1),I=1,MAXMR)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IMAFC2(I,2),I=1,MAXMR)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IMAFC2(I,3),I=1,MAXMR)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IMAPC2(I,1),I=1,MAXMR)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IMAPC2(I,2),I=1,MAXMR)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(IMAPC2(I,3),I=1,MAXMR)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ITEBC2(I,1),I=1,MAXTX)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ITEBC2(I,2),I=1,MAXTX)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ITEBC2(I,3),I=1,MAXTX)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ITEFC2(I,1),I=1,MAXTX)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ITEFC2(I,2),I=1,MAXTX)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ITEFC2(I,3),I=1,MAXTX)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ITEPC2(I,1),I=1,MAXTX)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ITEPC2(I,2),I=1,MAXTX)
      READ(IOUNIT,IOSTAT=IOS,ERR=8000,END=8100)(ITEPC2(I,3),I=1,MAXTX)
!
!     -----END READING IN-----------------------
!
!               ***************************
!               **  STEP 42--            **
!               **  WRITE OUT A MESSAGE  **
!               ***************************
!
      ISTEPN='42'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REST')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IERROR.EQ.'NO' .AND. IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4211)
 4211   FORMAT('THE RESTORING OF ALL INTERNAL DATAPLOT VARIABLES,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4212)
 4212   FORMAT('    PARAMETERS, ETC. HAS JUST BEEN COMPLETED')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GO TO 8999
!
 8000 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1211)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8011)ISECT
 8011 FORMAT('      READ FAILED IN SECTION ',A30)
      CALL DPWRST('XXX','BUG ')
      GO TO 8999
!
 8100 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1211)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8111)ISECT
 8111 FORMAT('      END OF FILE ON READ IN SECTION ',A30)
      CALL DPWRST('XXX','BUG ')
      GO TO 8999
!
 8999 CONTINUE
!               ***********************
!               **  STEP 51--        **
!               **  CLOSE THE FILE.  **
!               ***********************
!
      ISTEPN='51'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REST')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IENDFI='ON'
      IREWIN='ON'
      CALL DPCLFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
      IENDFI,IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
!
!               ****************
!               **  STEP 90-- **
!               **  EXIT.     **
!               ****************
!
 9000 CONTINUE
      IF(IBUGS2.EQ.'OFF'.AND.ISUBRO.NE.'REST')GO TO 9090
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
!
      RETURN
      END SUBROUTINE DPREST
      SUBROUTINE DPREVA(IFOUND,IERROR)
!
!     PURPOSE--RESTORE A LIST OF VARIABLES GENERATED BY THE
!              "SAVE VARIABLES" COMMAND.  NOTE THAT ONLY VARIABLES
!              AND PARAMETERS ARE SAVED/RESTORED (I.E., MATRICES AND
!              STRINGS/FUNCTIONS ARE NOT SUPPORTED).
!
!              THE PRIMARY USE OF THIS COMMAND IS FOR GENERAL
!              PURPOSE MACROS THAT MAY POTENTIALLY USE PREVIOUSLY
!              DEFINED VARIABLE/PARAMETER NAMES.  THE RESTORE ALLOWS
!              THESE VARIABLES TO BE RESET TO VALUES PRIOR TO THE
!              MACRO BEING CALLED.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2020/12
!     ORIGINAL VERSION--DECEMBER  2020.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      INCLUDE 'DPCOPA.INC'
!
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
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
      CHARACTER*4 IHLEFT
      CHARACTER*4 IHLEF2
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IEXIST
      CHARACTER*4 IOPEN
      CHARACTER*4 IOFILE
      CHARACTER*8 ITYPE
!
      CHARACTER (LEN=MAXSTR) :: ICANS
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOFO.INC'
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCODB.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOST.INC'
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPRE'
      ISUBN2='VA  '
      ISUBRO='-999'
      IFOUND='YES'
      IERROR='NO'
!CCCC ISUBRO='REVA'
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'REVA')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPREVA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGS2,ISUBRO,IERROR,IWIDTH
   53   FORMAT('IBUGS2,ISUBRO,IERROR,IWIDTH = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,55)(IANSLC(I),I=1,MIN(80,IWIDTH))
   55     FORMAT('(IANSLC(I),I=1,MIN(80,IWIDTH)) = ',80A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        WRITE(ICOUT,62)ISAVNA(1:80)
   62   FORMAT('ISAVNA(1:80) = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,63)ISAVST,ISAVFO,ISAVAC,ISAVCS,ISAVNU
   63   FORMAT('ISAVST,ISAVFO,ISAVAC,ISAVCS,ISAVNU = ',4(A12,2X),I6)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               **************************
!               **  STEP 11--           **
!               **  COPY OVER VARIABLES **
!               **************************
!
      ISTEPN='11'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REVA')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IOUNIT=ISAVNU
      IFILE=ISAVNA
      ISTAT=ISAVST
!CCCC IFORM=ISAVFO
      IFORM='FORMATTED'
      IACCES=ISAVAC
      IPROT=ISAVPR
      ICURST=ISAVCS
!
      ISUBN0='REVA'
      IERRFI='NO'
!
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'REVA')THEN
        WRITE(ICOUT,1194)IFILE(1:80)
 1194   FORMAT('IFILE(1:80) = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1195)ISTAT,IFORM,IACCES,IPROT,ICURST
 1195   FORMAT('ISTAT,IFORM,IACCES,IPROT,ICURST = ',   &
               4(A12,2X),A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1196)ISUBN0,IERRFI,IOUNIT
 1196   FORMAT('ISUBN0,IERRFI,IOUNIT = ',2(A4,2X),I6)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *******************************************
!               **  STEP 12--                            **
!               **  CHECK TO SEE IF SAVE FILE MAY EXIST  **
!               *******************************************
!
      ISTEPN='12'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REVA')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ISTAT.EQ.'NONE')THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1211)
 1211   FORMAT('***** ERROR IN DPREVA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1212)
 1212   FORMAT('      THE DESIRED SAVE CANNOT BE GIVEN BECAUSE THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1214)
 1214   FORMAT('      REQUIRED SYSTEM MASS STORAGE FILE WHICH STORES ',   &
               'SUCH SAVE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1216)
 1216   FORMAT('      IS NOT AVAILABLE AT THIS INSTALLATION.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1217)ISTAT,ISAVST
 1217   FORMAT('ISTAT,ISAVST = ',A12,2X,A12)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
!               ****************************
!               **  STEP 13--             **
!               **  EXTRACT THE FILE NAME **
!               **  (THE THIRD WORD)      **
!               ****************************
!
      ISTEPN='13'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REVA')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 1310 I=1,MAXFNC
        IFILE(I:I)=' '
 1310 CONTINUE
!
      DO 1320 I=1,MAXSTR
        ICANS(I:I)=IANSLC(I)
 1320 CONTINUE
!
!     IF FEWER THAN 2 ARGUMENTS, NO FILE NAME GIVEN, SO
!     USE DEFAULT NAME (dpsavf.tex).
!
      IFRST=2
      ISTART=1
      ISTOP=IWIDTH
      IF(NUMARG.LE.1)THEN
        DO 1325 II=1,MAXFNC
          IFILE(II:II)=ISAVNA(II:II)
 1325   CONTINUE
        NUMFIL=0
      ELSEIF(NUMARG.GE.2)THEN
        IWORD=3
        IOFILE='NO'
        CALL DPFILE(IANSLC,IWIDTH,IWORD,IOFILE,IBUGS2,ISUBRO,IERROR)
!
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REVA')THEN
          WRITE(ICOUT,1329)IOFILE
 1329     FORMAT('IOFILE = ',A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        IF(IOFILE.EQ.'NO')THEN
          IFILE=ISAVNA
          IFRST=2
        ELSE
          ISTART=1
          ISTOP=IWIDTH
          IWORD=3
          CALL DPEXWO(ICANS,ISTART,ISTOP,IWORD,   &
                      ICOL1,ICOL2,IFILE,NCFILE,   &
                      IBUGS2,ISUBRO,IERROR)
!
          IF(NCFILE.LT.1)THEN
            IFILE=ISAVNA
            IFRST=2
          ENDIF
          IFRST=3
        ENDIF
!
        IFWORD=0
        IFQUOT=0
        ILAST=0
        IF(IFILE(1:1).EQ.'"')THEN
          IFQUOT=1
          DO 1351 I=MAXFNC,1,-1
            IF(IFILE(I:I).NE.' ')THEN
              ILAST=I
              GO TO 1354
            ENDIF
 1351     CONTINUE
 1354     CONTINUE
          ICOUNT=0
          ISPAC=0
          DO 1356 I=1,ILAST
            IF((IFILE(I:I).EQ.' '.OR.IFILE(I:I).EQ.'-') .AND.   &
               ISPAC.EQ.0)THEN
              ISPAC=1
              ICOUNT=ICOUNT+1
            ELSEIF((IFILE(I:I).NE.' '.AND.IFILE(I:I).NE.'-') .AND.   &
              ISPAC.EQ.1)THEN
              ISPAC=0
            ENDIF
 1356     CONTINUE
          IFWORD=ICOUNT
        ENDIF
      ENDIF
!
      NCFILE=0
      DO 1341 II=MAXFNC,1,-1
        IF(IFILE(II:II).NE.' ')THEN
          NCFILE=II
          GO TO 1349
        ENDIF
 1341 CONTINUE
 1349 CONTINUE
!
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REVA')THEN
        WRITE(ICOUT,1366)IFRST,NCFILE,IFILE(1:MIN(256,NCFILE))
 1366   FORMAT('IFRST,NCFILE,IFILE = ',2I6,2X,A256)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *********************
!               **  STEP 14--      **
!               **  OPEN THE FILE  **
!               *********************
!
      ISTEPN='14'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REVA')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     FIRST CHECK IF FILE EXISTS
!
      CALL DPINFI(IFILE,IEXIST,IOPEN,IACCES,ISUBN0,IBUGS2,   &
                  ISUBRO,IERROR)
!
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REVA')THEN
        WRITE(ICOUT,1368)IEXIST
 1368   FORMAT('IEXIST = ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(IEXIST.EQ.'NO')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1372)
 1372   FORMAT('      THE SAVE VARIABLE FILE DOES NOT EXIST.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,203)IFILE(1:NCFILE)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      ISTEPN='14B'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REVA')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IREWIN='ON'
      CALL DPOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
                  IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
      IF(IERRFI.EQ.'YES')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1382)
 1382   FORMAT('      ERROR IN OPENING THE SAVE VARIABLE FILE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,203)IFILE(1:NCFILE)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               ****************************************
!               **  STEP 2--                          **
!               **  PARSE THE SAVE FILE AND RESTORE   **
!               **  SAVED VARIABLES AND PARAMETERS.   **
!               ****************************************
!
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'REVA')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMLIN=0
!
 2000 CONTINUE
      NUMLIN=NUMLIN+1
      READ(IOUNIT,'(2A4,1X,A8)',ERR=8200,END=8100)IHLEFT,IHLEF2,ITYPE
!
      NUMLIN=NUMLIN+1
      IF(ITYPE(1:4).EQ.'VARI')THEN
        READ(IOUNIT,*,ERR=8200,END=8100)NLEFT
      ELSEIF(ITYPE(1:4).EQ.'PARA')THEN
        READ(IOUNIT,*,ERR=8200,END=8100)AVAL
      ELSE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,201)
  201   FORMAT('      SAVE FILE DOES NOT SEEM TO BE A VALID ',   &
               'SAVE FILE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,203)IFILE(1:NCFILE)
  203   FORMAT('      FILE NAME = ',A)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!     DOES NAME EXIST IN CURRENT NAME TABLE
!
      ISTEPN='2A'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'REVA')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 220 JJ=1,NUMNAM
        I2=JJ
!
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REVA')THEN
          WRITE(ICOUT,227)I2,IHNAME(I2),IHNAM2(I2),IUSE(I2)
  227     FORMAT('I2,IHNAME(I2),IHNAM2(I2),IUSE(I2) = ',   &
                 I6,2X,2A4,2X,A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        IF(IHLEFT.EQ.IHNAME(I2).AND.IHLEF2.EQ.IHNAM2(I2).AND.   &
           IUSE(I2).EQ.'V'.AND.ITYPE(1:4).EQ.'VARI')THEN
!
!         CURRENT NAME IS VARIABLE, RESTORED NAME IS A VARIABLE
!
          ILOCV=I2
          IN(ILOCV)=NLEFT
          ICOLL=IVALUE(ILOCV)
          DO 230 KK=1,NLEFT
            NUMLIN=NUMLIN+1
            READ(IOUNIT,*,ERR=8200,END=8100)AVAL
            IJ=MAXN*(ICOLL-1)+KK
            IF(ICOLL.LE.MAXCOL)V(IJ)=AVAL
            IF(ICOLL.EQ.MAXCP1)PRED(I)=AVAL
            IF(ICOLL.EQ.MAXCP2)RES(I)=AVAL
            IF(ICOLL.EQ.MAXCP3)YPLOT(I)=AVAL
            IF(ICOLL.EQ.MAXCP4)XPLOT(I)=AVAL
            IF(ICOLL.EQ.MAXCP5)X2PLOT(I)=AVAL
            IF(ICOLL.EQ.MAXCP6)TAGPLO(I)=AVAL
  230     CONTINUE
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,232)IHLEFT,IHLEF2,NLEFT
  232       FORMAT(2A4,' RESTORED AS A VARIABLE WITH ',I8,   &
                   ' ELEMENTS.')
            CALL DPWRST('XXX','BUG ')
          ENDIF
          GO TO 2000
        ELSEIF(IHLEFT.EQ.IHNAME(I2).AND.IHLEF2.EQ.IHNAM2(I2).AND.   &
               IUSE(I2).EQ.'P'.AND.ITYPE(1:4).EQ.'PARA')THEN
!
!         CURRENT NAME IS PARAMETER, RESTORED NAME IS A VARIABLE
!
          ILOCV=I2
          VALUE(ILOCV)=AVAL
          IVALUE(ILOCV)=INT(AVAL)
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,234)IHLEFT,IHLEF2
  234       FORMAT(2A4,' RESTORED AS A PARAMETER.')
            CALL DPWRST('XXX','BUG ')
          ENDIF
          GO TO 2000
        ELSEIF(IHLEFT.EQ.IHNAME(I2).AND.IHLEF2.EQ.IHNAM2(I2).AND.   &
               IUSE(I2).EQ.'V'.AND.ITYPE(1:4).EQ.'PARA')THEN
!
!         CURRENT NAME IS VARIABLE, RESTORED NAME IS A PARAMETER
!
          ILOCV=I2
          VALUE(ILOCV)=AVAL
          IVALUE(ILOCV)=INT(AVAL)
          IUSE(ILOCV)='P'
          IN(ILOCV)=1
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,235)IHLEFT,IHLEF2
  235       FORMAT(2A4,' RESTORED AS A PARAMETER.')
            CALL DPWRST('XXX','BUG ')
          ENDIF
          GO TO 2000
        ELSEIF(IHLEFT.EQ.IHNAME(I2).AND.IHLEF2.EQ.IHNAM2(I2).AND.   &
           IUSE(I2).EQ.'P'.AND.ITYPE(1:4).EQ.'VARI')THEN
!
!         CURRENT NAME IS PARAMETER, RESTORED NAME IS A VARIABLE
!
          ILOCV=I2
          ICOLL=NUMCOL+1
          IF(ICOLL.GT.MAXCOL)THEN
            IF(IFEEDB.EQ.'ON')THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,237)IHLEFT,IHLEF2
  237         FORMAT('UNABLE TO RESTORE ',2A4,' AS A VARIABLE ',   &
                     '(MAXIMUM NUMBER OF COLUMNS EXCEEDED.')
              CALL DPWRST('XXX','BUG ')
            ENDIF
          ENDIF
          NUMCOL=NUMCOL+1
          IN(ILOCV)=NLEFT
          IVALUE(ILOCV)=ICOLL
          VALUE(ILOCV)=REAL(ICOLL)
          DO 240 KK=1,NLEFT
            NUMLIN=NUMLIN+1
            READ(IOUNIT,*,ERR=8200,END=8100)AVAL
            IJ=MAXN*(ICOLL-1)+KK
            IF(ICOLL.LE.MAXCOL)V(IJ)=AVAL
            IF(ICOLL.EQ.MAXCP1)PRED(I)=AVAL
            IF(ICOLL.EQ.MAXCP2)RES(I)=AVAL
            IF(ICOLL.EQ.MAXCP3)YPLOT(I)=AVAL
            IF(ICOLL.EQ.MAXCP4)XPLOT(I)=AVAL
            IF(ICOLL.EQ.MAXCP5)X2PLOT(I)=AVAL
            IF(ICOLL.EQ.MAXCP6)TAGPLO(I)=AVAL
  240     CONTINUE
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,232)IHLEFT,IHLEF2,NLEFT
            CALL DPWRST('XXX','BUG ')
          ENDIF
          GO TO 2000
        ELSEIF(IHLEFT.EQ.IHNAME(I2).AND.IHLEF2.EQ.IHNAM2(I2))THEN
          IF(IUSE(I2).NE.'P'.AND.IUSE(I2).NE.'V')THEN
            IF(IFEEDB.EQ.'ON')THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,252)IHLEFT,IHLEF2
  252         FORMAT('UNABLE TO RESTORE ',2A4,' (CURRENT NAME IS ',   &
                     'NEITHER A VARIABLE OR A PARAMETER.')
              CALL DPWRST('XXX','BUG ')
            ENDIF
          ENDIF
          GO TO 2000
        ENDIF
  220 CONTINUE
!
!     RESTORED NAME NOT MATCHED IN CURRENT NAME TABLE.
!
      IF(NUMNAM+1.GT.MAXNME)THEN
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,262)IHLEFT,IHLEF2
  262     FORMAT('UNABLE TO RESTORE ',2A4,' (MAXIMUM NUNBER OF NAMES ',   &
                 'EXCEEDED.')
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GO TO 2000
      ENDIF
      NUMNAM=NUMNAM+1
!
      IF(ITYPE(1:4).EQ.'VARI')THEN
        ILOCV=I2+1
        ICOLL=NUMCOL+1
        IF(ICOLL.GT.MAXCOL)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,237)IHLEFT,IHLEF2
          CALL DPWRST('XXX','BUG ')
        ENDIF
        NUMCOL=NUMCOL+1
        IHNAME(NUMNAM)=IHLEFT
        IHNAM2(NUMNAM)=IHLEF2
        IUSE(ILOCV)='V'
        IN(ILOCV)=NLEFT
        IVALUE(ILOCV)=NUMCOL
        VALUE(ILOCV)=REAL(NUMCOL)
        ICOLL=NUMCOL
        DO 270 KK=1,NLEFT
          NUMLIN=NUMLIN+1
          READ(IOUNIT,*,ERR=8200,END=8100)AVAL
          IJ=MAXN*(ICOLL-1)+KK
          IF(ICOLL.LE.MAXCOL)V(IJ)=AVAL
          IF(ICOLL.EQ.MAXCP1)PRED(I)=AVAL
          IF(ICOLL.EQ.MAXCP2)RES(I)=AVAL
          IF(ICOLL.EQ.MAXCP3)YPLOT(I)=AVAL
          IF(ICOLL.EQ.MAXCP4)XPLOT(I)=AVAL
          IF(ICOLL.EQ.MAXCP5)X2PLOT(I)=AVAL
          IF(ICOLL.EQ.MAXCP6)TAGPLO(I)=AVAL
  270   CONTINUE
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,232)IHLEFT,IHLEF2,NLEFT
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GO TO 2000
      ELSEIF(ITYPE(1:4).EQ.'PARA')THEN
        ILOCV=I2+1
        VALUE(ILOCV)=AVAL
        IVALUE(ILOCV)=INT(AVAL)
        IUSE(ILOCV)='P'
        IHNAME(NUMNAM)=IHLEFT
        IHNAM2(NUMNAM)=IHLEF2
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,234)IHLEFT,IHLEF2
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GO TO 2000
      ENDIF
      GO TO 2000
!
!     END OF FILE REACHED
!
 8100 CONTINUE
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8110)NUMLIN
 8110   FORMAT(I12,' LINES PROCESSED FROM SAVE FILE.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GO TO 8900
!
!     ERROR READING FILE
!
 8200 CONTINUE
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8210)NUMLIN
 8210   FORMAT('ERROR READING SAVE FILE AT LINE ',I12)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GO TO 8900
!
!               ***********************
!               **  STEP 51--        **
!               **  CLOSE THE FILE.  **
!               ***********************
!
 8900 CONTINUE
!
      ISTEPN='51'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'REVA')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IENDFI='ON'
      IREWIN='ON'
      CALL DPCLFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
                  IENDFI,IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
!
!               ****************
!               **  STEP 90-- **
!               **  EXIT.     **
!               ****************
!
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'REVA')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPREVA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9032)IERROR,IERRFI,NUMLIN
 9032   FORMAT('IERROR,IERRFI,NUMLIN = ',A4,2X,A12,2X,I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPREVA
      SUBROUTINE DPRETA(IBUGS2,IBUGQ,IFOUND,IERROR)
!
!     PURPOSE--TREAT THE RETAIN CASE--
!              RETAIN SPECIFIED ELEMENTS OF A VARIABLE
!              AND PACK THESE RETAINED ELEMENTS
!              INTO THE FIRST AVAILABLE LOCATIONS;
!              REDEFINE THE LENGTH OF THE PACKED VARIABLE.
!     INPUT --NECESSARILY A VARIABLE.
!     OUTPUT--NECESSARILY A VARIABLE.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
!           MODIFIED, OR OTHERWISE USED IN A CONTEXT
!           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--MARCH     1978.
!     UPDATED         --MAY       1978.
!     UPDATED         --JUNE      1978.
!     UPDATED         --JULY      1978.
!     UPDATED         --NOVEMBER  1978.
!     UPDATED         --NOVEMBER  1980.
!     UPDATED         --AUGUST    1981.
!     UPDATED         --OCTOBER   1981.
!     UPDATED         --DECEMBER  1981.
!     UPDATED         --MARCH     1982.
!     UPDATED         --MAY       1982.
!     UPDATED         --JANUARY   2000. SUPPORT FOR VARIABLE LABELS
!     UPDATED         --JULY      2019. TWEAK SCRATCH SPACE
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IBUGS2
      CHARACTER*4 IBUGQ
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
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
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
!
      DIMENSION ILISTV(100)
      DIMENSION TEMP(MAXOBV)
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(IGARB1),TEMP(1))
!
      DIMENSION IVN(100)
      DIMENSION IVN2(100)
      DIMENSION IRN(100)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPRE'
      ISUBN2='TA  '
!
      IPASS=0
      NUMDEL=0
      ISAVE=0
      IROD1O=0
      IRODNO=0
      IROW1O=0
      IROWNO=0
      ILQP1=0
!
!
      TEMPD=0.0
      VALD1O=0.0
      VALDNO=0.0
      VAL1O=0.0
      VALNO=0.0
!
!               *************************************************
!               **  TREAT THE RETAIN CASE                      **
!               **  RETAIN SPECIFIC ELEMENTS OF A VECTOR       **
!               **  AND PACK THOSE     ELEMENTS                **
!               **  INTO THE FIRST AVAILABLE LOCATIONS.        **
!               *************************************************
!
      IFOUND='YES'
      IERROR='NO'
!
      MAXDEL=100
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
      IF(IBUGS2.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPRETA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGS2,IERROR
   52   FORMAT('IBUGS2,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)MAXNAM,NUMNAM,MAXN,MAXCOL,NUMCOL
   54   FORMAT('MAXNAM,NUMNAM,MAXN,MAXCOL,NUMCOL = ',5I8)
        CALL DPWRST('XXX','BUG ')
        DO 60 I=1,NUMNAM
          WRITE(ICOUT,61)I,IHNAME(I),IHNAM2(I),IUSE(I),   &
                         IVALUE(I),VALUE(I)
   61     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVALUE(I),VALUE(I) = ',   &
                 I8,2X,2A4,2X,A4,I8,G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,62)I,IHNAME(I),IHNAM2(I),IN(I),IVSTAR(I),IVSTOP(I)
   62     FORMAT('I,IHNAME(I),IHNAM2(I),IN(I),IVSTAR(I),IVSTOP(I)  = ',   &
                 I8,2X,2A4,6X,3I8)
          CALL DPWRST('XXX','BUG ')
   60   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        DO 70 J=1,NUMCOL
          IJ=MAXN*(J-1)+1
          WRITE(ICOUT,71)J,MAXN,IJ,V(IJ)
   71     FORMAT('J,MAXN,IJ,V(IJ) = ',3I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   70   CONTINUE
      ENDIF
!               *******************************************************
!               **  STEP 1--                                         **
!               **  CHECK FOR THE PROPER NUMBER OF INPUT ARGUMENTS.  **
!               *******************************************************
!
      ISTEPN='1'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LT.1)THEN
        IERROR='YES'
        GO TO 8900
      ENDIF
      IFOUND='YES'
!
!               ***********************************************************
!               **  STEP 2--                                             **
!               **  DETERMINE THE SUBCASE BASED ON THE QUALIFIER.        **
!               **  SCAN TO CHECK IF 'SUBSET' OR 'FOR' IS PRESENT.       **
!               **  IF NOT PRESENT, THEN HAVE CASE 1--                   **
!               **  EXAMPLE--RETAIN X(4) Y(1) Z(46)                      **
!               **  IF PRESENT, THEN HAVE CASE 2--                       **
!               **  EXAMPLE--RETAIN X Y Z FOR I = 1 1 10                 **
!               **  DETERMINE THE LOCATION IN THE ARGUMENT LIST          **
!               **  OF 'SUBSET' OR 'FOR'.                                **
!               **  BRANCH TO THE APPROPRIATE SUBCASE                    **
!               **  FULL VERSUS SUBSET/FOR.                              **
!               ***********************************************************
!
      ISTEPN='2'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ILOCQ=1
      ICASEQ='UNKN'
      IF(NUMARG.LE.0)GO TO 290
      DO 210 J=1,NUMARG
      J2=J
      IF(IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ')GO TO 220
      IF(IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  ')GO TO 220
      IF(IHARG(J).EQ.'FOR '.AND.IHARG2(J).EQ.'    ')GO TO 230
  210 CONTINUE
      ICASEQ='FULL'
      ILOCQ=NUMARG+1
      GO TO 300
!
  220 CONTINUE
      ICASEQ='SUBS'
      ILOCQ=J2
      GO TO 7000
!
  230 CONTINUE
      ICASEQ='FOR'
      ILOCQ=J2
      GO TO 7000
!
  290 CONTINUE
!
!               ***********************************************************
!               **  STEP 3--                                             **
!               **  FOR THE FULL CASE,                                   **
!               **  EXTRACT EACH VARIABLE NAME AND EACH ARGUMENT VALUE.  **
!               ***********************************************************
!
      ISTEPN='3'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IPASS=0
  300 CONTINUE
      IPASS=IPASS+1
!
      IF(IPASS.LT.1 .OR. IPASS.GT.MAXDEL)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,301)
  301   FORMAT('***** ERROR IN RETAIN (DPRETA)--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,302)
  302   FORMAT('      THE RETAIN COMMAND REQUIRES THAT THE NUMBER OF ',   &
               'VARIABLES WITH ELEMENTS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,304)IPASS
  304   FORMAT('      TO BE RETAINED BE BETWEEN 1 AND ',I8,   &
               ' (INCLUSIVE);')
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
        GO TO 8900
      ENDIF
!
      IF(IPASS.GE.2)ISAVE=IENDRP
!
!               ****************************************************************
!               **  STEP 3.1--
!               **  IF THIS IS THE FIRST PASS ON THIS LINE (AND ONLY FOR PASS 1)
!               **  SEARCH FOR RETAIN (OTHERWISE SKIP THIS STEP)
!               **  SEARCH BETWEEN COLUMN 1 AND THE END OF THE LINE (INCLUSIVE).
!               ****************************************************************
!
      ISTEPN='3.1'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IPASS.LT.2)THEN
        ISTAR1=1
        ISTOP1=IWIDTH
        ISTRIN='RETA'
        ISTRI2='IN  '
        INEX='II'
        CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGS2,   &
                    IFOUCO,IBEGCO,IENDCO,   &
                    ITYPCO,IHOLCO,IHLCO2,INT1CO,FLOACO,IERRO1)
        IF(IFOUCO.EQ.'NO')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,301)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,312)
  312     FORMAT('      THE WORD      RETAIN      NOT FOUND')
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
          GO TO 8900
        ENDIF
      ENDIF
!
!               ****************************************************************
!               **  STEP 3.2--
!               **  SEARCH FOR LEFT PARENTHESIS;
!               **  IF THIS IS THE FIRST PASS FOR THIS LINE,
!               **  SEARCH BETWEEN    RETAIN     AND      END OF LINE
!               **  (IF NO LEFT PARENTHESIS FOUND AT ALL, JUMP TO 7000).
!               **  IF THIS IS THE SECOND (OR HIGHER) PASS FOR THIS LINE,
!               **  SEARCH BETWEEN    PREVIOUS RIGHT PARENTHESIS AND
!               **  END OF LINE.
!               ****************************************************************
!
      ISTEPN='3.2'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IPASS.LE.1)ISTAR1=IENDCO+1
      IF(IPASS.GE.2)ISTAR1=ISAVE+1
      ISTOP1=IWIDTH
      ISTRIN='('
      INEX='II'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGS2,   &
                  IFOULP,IBEGLP,IENDLP,   &
                  ITYPLP,IHOLLP,IHLLP2,INT1LP,FLOALP,IERRO1)
      IF(IFOULP.EQ.'YES')GO TO 338
      IF(IFOULP.EQ.'NO'.AND.IPASS.GE.2)GO TO 399
      GO TO 7000
  338 CONTINUE
!
!               ****************************************************************
!               **  STEP 3.3--
!               **  SEARCH FOR RIGHT PARENTHESIS;
!               **  SEARCH BETWEEN    LEFT PARENTHESIS     AND    END OF LINE.
!               ****************************************************************
!
      ISTEPN='3.3'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTAR1=IENDLP+1
      ISTOP1=IWIDTH
      ISTRIN=')'
      INEX='II'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGS2,   &
                  IFOURP,IBEGRP,IENDRP,   &
                  ITYPRP,IHOLRP,IHLRP2,INT1RP,FLOARP,IERRO1)
      IF(IFOURP.EQ.'NO')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,301)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,342)
  342   FORMAT('      WHEN THE RETAIN COMMAND IS USED WITHOUT A SUBSET')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,344)
  344   FORMAT('      QUALIFICATION, OR WITHOUT A FOR QUALIFICATION,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,345)
  345   FORMAT('      THEN ONLY INDIVIDUAL ELEMENTS OF A VARIABLE MAY')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,347)
  347   FORMAT('      BE RETAINED SUCH INDIVIDUAL ELEMENTS ARE')
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
        GO TO 8900
      ENDIF
!
!               ****************************************************************
!               **  STEP 3.4--
!               **  SEARCH FOR ROW NUMBER;
!               **  SEARCH BETWEEN    LEFT PARENTHESIS     AND     RIGHT PARENTH
!               ****************************************************************
!
      ISTEPN='3.4'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTAR1=IENDLP
      ISTOP1=IENDRP
      ISTRIN='(;)'
      INEX='EE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGS2,   &
                  IFOURN,IBEGRN,IENDRN,   &
                  ITYPRN,IHOLRN,IHLRN2,INT1RN,FLOARN,IERRO1)
      IF(IFOURN.EQ.'NO')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,301)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,362)
  362   FORMAT('      WHEN THE RETAIN COMMAND IS USED WITHOUT A SUBSET')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,364)
  364   FORMAT('      QUALIFICATION, OR WITHOUT A FOR  QUALIFICATION,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,365)
  365   FORMAT('      THEN ONLY INDIVIDUAL ELEMENTS OF A VARIABLE MAY')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,367)
  367   FORMAT('      BE RETAINED.  SUCH INDIVIDUAL ELEMENTS ARE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,368)
  368   FORMAT('      SPECIFIED BY A VARIABLE NAME FOLLOWED BY A')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,369)
  369   FORMAT('      PAIR OF PARENTHSES WITH A ROW NUMBER WITHIN;')
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
        GO TO 8900
      ENDIF
!
!               ****************************************************************
!               **  STEP 3.5--
!               **  SEARCH FOR VARIABLE NAME;
!               **  IF THIS IS THE FIRST PASS FOR THIS LINE,
!               **  SEARCH BETWEEN    RETAIN     AND      LEFT PARENTHESIS;
!               **  IF THIS IS THE SECOND (OR HIGHER) PASS FOR THIS LINE,
!               **  SEARCH BETWEEN    PREVIOUS RIGHT PARENTHESIS AND
!               **  THE NEXT LEFT PARENTHESIS.
!               ****************************************************************
!
      ISTEPN='3.5'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IPASS.LE.1)ISTAR1=IENDCO+1
      IF(IPASS.GE.2)ISTAR1=ISAVE+1
      ISTOP1=IENDLP
      ISTRIN='!;('
      INEX='IE'
      CALL DPTYP3(IANS,IWIDTH,ISTAR1,ISTOP1,ISTRIN,ISTRI2,INEX,IBUGS2,   &
                  IFOUVN,IBEGVN,IENDVN,   &
                  ITYPVN,IHOLVN,IHLVN2,INT1VN,FLOAVN,IERRO1)
      IF(IFOUVN.EQ.'NO')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,301)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,382)
  382   FORMAT('      WHEN THE RETAIN COMMAND IS USED WITHOUT A SUBSET')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,384)
  384   FORMAT('      QUALIFICATION, OR WITHOUT A FOR  QUALIFICATION,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,385)
  385   FORMAT('      THEN ONLY INDIVIDUAL ELEMENTS OF A VARIABLE MAY')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,387)
  387   FORMAT('      BE RETAINED.  SUCH INDIVIDUAL ELEMENTS ARE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,388)
  388   FORMAT('      SPECIFIED BY A VARIABLE NAME FOLLOWED BY A')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,389)
  389   FORMAT('      PAIR OF PARENTHSES WITH A ROW NUMBER WITHIN;')
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
        GO TO 8900
      ENDIF
!
      IVN(IPASS)=IHOLVN
      IVN2(IPASS)=IHLVN2
      IRN(IPASS)=INT1RN
!
      GO TO 300
!
  399 CONTINUE
      NUMDEL=IPASS-1
!
!               *********************************************************
!               **  STEP 4--                                           **
!               **  FOR THE FULL CASE,                                 **
!               **  CHECK TO MAKE SURE ALL VARIABLES WITH RETENTIONS   **
!               **  ARE, IN FACT, IN THE INTERNAL LIST,                **
!               **  AND ARE, IN FACT, VARIABLES (AS OPPOSED TO         **
!               **  PARAMETERS).                                       **
!               *********************************************************
!
      ISTEPN='4'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 420 J=1,NUMDEL
        J2=J
        IHVARJ=IVN(J)
        IHVRJ2=IVN2(J)
        DO 430 I=1,NUMNAM
          I2=I
          IF(IHVARJ.EQ.IHNAME(I).AND.IHVRJ2.EQ.IHNAM2(I).AND.   &
             IUSE(I).EQ.'V')GO TO 440
          IF(IHVARJ.EQ.IHNAME(I).AND.IHVRJ2.EQ.IHNAM2(I).AND.   &
             IUSE(I).EQ.'P')GO TO 450
  430   CONTINUE
!
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,301)
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
        WRITE(ICOUT,306)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,307)(IANS(I),I=1,MIN(100,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GO TO 8900
!
  440   CONTINUE
        ILISTV(J2)=I2
        GO TO 420
!
  450   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,301)
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
        WRITE(ICOUT,306)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,307)(IANS(I),I=1,MIN(100,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GO TO 8900
!
  420 CONTINUE
!
!               *****************************************
!               **  STEP 5--                           **
!               **  TREAT THE FULL CASE.               **
!               **  CARRY OUT THE RETAINING,            **
!               **  AND THE SUBSEQUENT PACKING,        **
!               **  DO THE LIST UPDATING, AND          **
!               **  PRODUCE SOME INFORMATIVE PRINTING. **
!               *****************************************
!
      ISTEPN='5'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     2021/01: ONLY PRINT THE BLANK LINE IF THE FEEDBACK SWITCH IS ON
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      DO 500 J=1,NUMDEL
      IHVARJ=IVN(J)
      IHVRJ2=IVN2(J)
      IROWD=IRN(J)
      ILIST2=ILISTV(J)
      NIVARJ=IN(ILIST2)
      ICOLVJ=IVALUE(ILIST2)
      IMAX=NIVARJ
      IF(1.LE.IROWD.AND.IROWD.LE.IMAX)GO TO 539
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,301)
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
      GO TO 8900
!
  539 CONTINUE
!
      NS2=0
      ND2=0
      DO 550 I=1,IMAX
      IF(I.NE.IROWD)GO TO 560
      GO TO 570
!
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
      GO TO 550
!
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
      GO TO 550
!
  550 CONTINUE
      NIOLD=NIVARJ
      NINEW=NS2
      IROW1N=1
      IROWNN=NS2
!
      IF(NS2.GE.1)GO TO 580
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,301)
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
      WRITE(ICOUT,306)
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,307)(IANS(I),I=1,MIN(100,IWIDTH))
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GO TO 8900
!
  580 CONTINUE
      DO 600 I=1,NS2
      IJ=MAXN*(ICOLVJ-1)+I
      IF(ICOLVJ.LE.MAXCOL)V(IJ)=TEMP(I)
      IF(ICOLVJ.EQ.MAXCP1)PRED(I)=TEMP(I)
      IF(ICOLVJ.EQ.MAXCP2)RES(I)=TEMP(I)
      IF(ICOLVJ.EQ.MAXCP3)YPLOT(I)=TEMP(I)
      IF(ICOLVJ.EQ.MAXCP4)XPLOT(I)=TEMP(I)
      IF(ICOLVJ.EQ.MAXCP5)X2PLOT(I)=TEMP(I)
      IF(ICOLVJ.EQ.MAXCP6)TAGPLO(I)=TEMP(I)
  600 CONTINUE
!
      DO 602 J4=1,NUMNAM
      IF(IUSE(J4).EQ.'V'.AND.IVALUE(J4).EQ.ICOLVJ)GO TO 605
      GO TO 602
  605 CONTINUE
      IUSE(J4)='V'
      IVALUE(J4)=ICOLVJ
      VALUE(J4)=ICOLVJ
      IN(J4)=NINEW
      IVSTAR(J4)=MAXN*(ICOLVJ-1)+1
      IVSTOP(J4)=MAXN*(ICOLVJ-1)+NINEW
  602 CONTINUE
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,611)IHVARJ,IHVRJ2,NIOLD
  611   FORMAT('VARIABLE ',2A4,'--OLD NUMBER OF ELEMENTS = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,612)NINEW
  612   FORMAT('                   NEW NUMBER OF ELEMENTS = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,613)VALD1O
  613   FORMAT('                   FIRST VALUE DELETED    = ',E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,614)IROD1O
  614   FORMAT('                         (DELETED FROM ROW ',I8,')')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,615)VALDNO
  615   FORMAT('                   LAST  VALUE DELETED    = ',E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,616)IRODNO
  616   FORMAT('                         (DELETED FROM ROW ',I8,')')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,617)VAL1O
  617   FORMAT('                   FIRST VALUE RETAINED   = ',E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,618)IROW1O,IROW1N
  618   FORMAT('                         (MOVED FROM ROW ',I8,   &
               ' TO ROW ',I8,')')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,619)VALNO
  619   FORMAT('                   LAST  VALUE RETAINED   = ',E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,620)IROWNO,IROWNN
  620   FORMAT('                         (MOVED FROM ROW ',I8,   &
               ' TO ROW ',I8,')')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
  500 CONTINUE
!
      GO TO 8900
!
!               ***************************************************************
!               **  STEP 7--                                                 **
!               **  FOR THE SUBSET AND FOR CASES                            **
!               **  (AND WHEN RETAINING ENTIRE VARIABLES),
!               **  CHECK TO MAKE SURE ALL VARIABLES WITH RETENTIONS          **
!               **  ARE, IN FACT, IN THE INTERNAL LIST,                      **
!               **  AND ARE, IN FACT, VARIABLES (AS OPPOSED TO PARAMETERS).  **
!               ***************************************************************
!
 7000 CONTINUE
!
      ISTEPN='7'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMDEL=ILOCQ-1
      IF(1.LE.NUMDEL.AND.NUMDEL.LE.MAXDEL)GO TO 7100
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,301)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7102)
 7102 FORMAT('      THE RETAIN COMMAND REQUIRES THAT ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7103)
 7103 FORMAT('      THE NUMBER OF VARIABLES WITH ELEMENTS ',   &
      'TO BE RETAINED')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7104)MAXDEL
 7104 FORMAT('      BE BETWEEN 1 AND ',I8,' (INCLUSIVE);')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7105)NUMDEL
 7105 FORMAT('      THE SPECIFIED NUMBER WAS ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,306)
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,307)(IANS(I),I=1,MIN(100,IWIDTH))
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GO TO 8900
!
 7100 CONTINUE
      DO 7200 J=1,NUMDEL
      J2=J
      IHVARJ=IHARG(J)
      IHVRJ2=IHARG2(J)
      DO 7300 I=1,NUMNAM
      I2=I
      IF(IHVARJ.EQ.IHNAME(I).AND.IHVRJ2.EQ.IHNAM2(I).AND.   &
      IUSE(I).EQ.'V')GO TO 7400
      IF(IHVARJ.EQ.IHNAME(I).AND.IHVRJ2.EQ.IHNAM2(I).AND.   &
      IUSE(I).EQ.'P')GO TO 7500
 7300 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,301)
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
      WRITE(ICOUT,306)
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,307)(IANS(I),I=1,MIN(100,IWIDTH))
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GO TO 8900
!
 7400 CONTINUE
      ILISTV(J2)=I2
      GO TO 7200
!
 7500 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,301)
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
      WRITE(ICOUT,306)
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,307)(IANS(I),I=1,MIN(100,IWIDTH))
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GO TO 8900
!
 7200 CONTINUE
!
!               *****************************************
!               **  STEP 8--                           **
!               **  TREAT THE SUBSET AND FOR CASES     **
!               **  AND CERTAIN FULL CASES.            **
!               **  CARRY OUT THE RETAINING,            **
!               **  AND THE SUBSEQUENT PACKING,        **
!               **  DO THE LIST UPDATING, AND          **
!               **  PRODUCE SOME INFORMATIVE PRINTING. **
!               *****************************************
!
      ISTEPN='8'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICASEQ.EQ.'FULL')GO TO 8100
      ILQP1=ILOCQ+1
      IF(ILQP1.LE.NUMARG)GO TO 8100
      IF(ICASEQ.EQ.'FOR')GO TO 8030
      GO TO 8010
!
 8010 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,301)
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
 8016 FORMAT('      THE FIRST ARGUMENT SPECIFIES THE SUBSET ',   &
      'VARIABLE;')
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
      WRITE(ICOUT,306)
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,307)(IANS(I),I=1,MIN(100,IWIDTH))
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GO TO 8900
!
 8030 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,301)
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
 8038 FORMAT('      3) ONE LIMIT (LOWER OR UPPER) ',   &
      'FOR THE DUMMY VARIABLE;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8039)
 8039 FORMAT('      4) THE INCREMENT FOR THE DUMMY VARIABLE;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9040)
 9040 FORMAT('      5) THE OTHER LIMIT (UPPER OR LOWER) ',   &
      'FOR THE DUMMY VARIABLE.')
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
      GO TO 8900
!
 8100 CONTINUE
      IF(ICASEQ.EQ.'FULL')GO TO 8130
      IF(ICASEQ.EQ.'FOR')GO TO 8120
      IHSET=IHARG(ILQP1)
      IHSET2=IHARG2(ILQP1)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IHSET,IHSET2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      GO TO 8110
!
 8110 CONTINUE
      NISET=IN(ILOC)
      CALL DPSUBS(NISET,ILOCS,NS,IBUGQ,IERROR)
      NQ=NISET
      GO TO 8200
!
 8120 CONTINUE
      NIOLD=MAXN
      CALL DPFOR(NIOLD,NINEW,IROW1,IROWN,   &
      NLOCAL,ILOCS,NS,IBUGQ,IERROR)
      NQ=NINEW
      GO TO 8200
!
 8130 CONTINUE
      DO 8135 I=1,MAXN
      ISUB(I)=1
 8135 CONTINUE
      NQ=MAXN
      GO TO 8200
!
 8200 CONTINUE
!
!     2021/01: ONLY PRINT THE BLANK LINE IF THE FEEDBACK SWITCH IS ON
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      DO 8300 J=1,NUMDEL
      IHVARJ=IHARG(J)
      IHVRJ2=IHARG2(J)
      ILIST2=ILISTV(J)
      NIVARJ=IN(ILIST2)
      ICOLVJ=IVALUE(ILIST2)
      NS2=0
      ND2=0
      IMAX=NQ
      IF(NIVARJ.LT.NQ)IMAX=NIVARJ
      DO 8400 I=1,IMAX
      IF(ISUB(I).EQ.1)GO TO 8450
!
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
      GO TO 8400
!
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
      GO TO 8400
!
 8400 CONTINUE
      NIOLD=NIVARJ
      NINEW=NS2
      IROW1N=1
      IROWNN=NS2
!
      IF(ND2.GE.1)GO TO 8550
!CCCC WRITE(ICOUT,999)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,8501)
!8501 FORMAT('***** NOTE--')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,8502)
!8502 FORMAT('      NO DELETING/RETAINING WAS CARRIED OUT;')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,8503)
!8503 FORMAT('      POSSIBLE CAUSES--')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,8504)
!8504 FORMAT('      1) A NULL    SUBSET    SPECIFICATION;')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,8505)
!8505 FORMAT('      2) A NULL    FOR       SPECIFICATION;')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,8506)
!8506 FORMAT('      3) THE ELEMENTS TO BE RETAINED')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,8507)
!8507 FORMAT('         DID NOT EXIST. ')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,8510)
!8510 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC IF(IWIDTH.GE.1)WRITE(ICOUT,8511)(IANS(I),I=1,IWIDTH)
!8511 FORMAT('      ',100A1)
!CCCC IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 8900
!
 8550 CONTINUE
      DO 8500 I=1,NS2
      IJ=MAXN*(ICOLVJ-1)+I
      IF(ICOLVJ.LE.MAXCOL)V(IJ)=TEMP(I)
      IF(ICOLVJ.EQ.MAXCP1)PRED(I)=TEMP(I)
      IF(ICOLVJ.EQ.MAXCP2)RES(I)=TEMP(I)
      IF(ICOLVJ.EQ.MAXCP3)YPLOT(I)=TEMP(I)
      IF(ICOLVJ.EQ.MAXCP4)XPLOT(I)=TEMP(I)
      IF(ICOLVJ.EQ.MAXCP5)X2PLOT(I)=TEMP(I)
      IF(ICOLVJ.EQ.MAXCP6)TAGPLO(I)=TEMP(I)
 8500 CONTINUE
!
      NS2P1=NS2+1
      IF(NS2P1.GT.IMAX)GO TO 8569
      DO 8560 I=NS2P1,IMAX
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
!
      DO 8600 J4=1,NUMNAM
      IF(IUSE(J4).EQ.'V'.AND.IVALUE(J4).EQ.ICOLVJ)GO TO 8605
      GO TO 8600
 8605 CONTINUE
      IUSE(J4)='V'
      IVALUE(J4)=ICOLVJ
      VALUE(J4)=ICOLVJ
      IN(J4)=NINEW
      IVSTAR(J4)=MAXN*(ICOLVJ-1)+1
      IVSTOP(J4)=MAXN*(ICOLVJ-1)+NINEW
 8600 CONTINUE
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8611)IHVARJ,IHVRJ2,NIOLD
 8611   FORMAT('VARIABLE ',2A4,'--OLD NUMBER OF ELEMENTS = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8612)NINEW
 8612   FORMAT('                   NEW NUMBER OF ELEMENTS = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8613)VALD1O
 8613   FORMAT('                   FIRST VALUE DELETED    = ',E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8614)IROD1O
 8614   FORMAT('                         (DELETED FROM ROW ',I8,')')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8615)VALDNO
 8615   FORMAT('                   LAST  VALUE DELETED    = ',E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8616)IRODNO
 8616   FORMAT('                         (DELETED FROM ROW ',I8,')')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8617)VAL1O
 8617   FORMAT('                   FIRST VALUE RETAINED   = ',E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8618)IROW1O,IROW1N
 8618   FORMAT('                         (MOVED FROM ROW ',I8,   &
               ' TO ROW ',I8,'  )')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8619)VALNO
 8619   FORMAT('                   LAST  VALUE RETAINED   = ',E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8620)IROWNO,IROWNN
 8620   FORMAT('                         (MOVED FROM ROW ',I8,   &
               ' TO ROW ',I8,')')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
 8300 CONTINUE
!
      GO TO 8900
!
!               **********************************
!               **  STEP 9--                    **
!               **  UPDATE INTERNAL DATA ARRAY  **
!               **  (IF NECESSARY)              **
!               **********************************
!
 8900 CONTINUE
!
      ISTEPN='9'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!CCCC OCTOBER 1993.  ADD IVALU2 TO ARGUMENT LIST.
!CCCC CALL DPUPDV(IHNAME,IHNAM2,IUSE,IVALUE,VALUE,IN,
      CALL DPUPDV(IHNAME,IHNAM2,IUSE,IVALUE,IVALU2,VALUE,IN,   &
      IVARLB,   &
      IVSTAR,IVSTOP,MAXNAM,NUMNAM,V,MAXN,MAXCOL,NUMCOL,   &
      IBUGS2,IERROR)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPRETA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR,NUMNAM,NUMCOL
 9012   FORMAT('IERROR,NUMNAM,NUMCOL = ',A4,2X,2I8)
        CALL DPWRST('XXX','BUG ')
        DO 9020 I=1,NUMNAM
          WRITE(ICOUT,9021)I,IHNAME(I),IHNAM2(I),IUSE(I),   &
                           IVALUE(I),VALUE(I)
 9021     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVALUE(I),VALUE(I) = ',   &
                 I8,2X,2A4,2X,A4,I8,E15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,9022)I,IHNAME(I),IHNAM2(I),IN(I),   &
                           IVSTAR(I),IVSTOP(I)
 9022     FORMAT('I,IHNAME(I),IHNAM2(I),IN(I),IVSTAR(I),IVSTOP(I)  = ',   &
                 I8,2X,2A4,6X,I8,I8,I8)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        DO 9030 J=1,NUMCOL
          IJ=MAXN*(J-1)+1
          WRITE(ICOUT,9031)J,MAXN,IJ,V(IJ)
 9031     FORMAT('J,MAXN,IJ,V(IJ) = ',I8,I8,I8,E15.7)
          CALL DPWRST('XXX','BUG ')
 9030   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPRETA
      SUBROUTINE DPRETN(IHARG,NUMARG,IDEFTN,IRECTN,   &
      IBUGS2,IFOUND,IERROR)
!
!     PURPOSE--SPECIFY THE NAME OF THE VARIABLE TO PUT
!              THE TOLERANCE LIMIT VALUES CALCULATED FROM
!              A RECIPE FIT/ANOVA COMMAND
!
!     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
!                     --NUMARG (AN INTEGER VARIABLE)
!                     --IDEFTN (A  CHARACTER VARIABLE)
!                     --IBUGS2 (A  CHARACTER VARIABLE)
!     OUTPUT ARGUMENTS--IRECTN (A CHARACTER VARIABLE)
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
!           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
!           MODIFIED, OR OTHERWISE USED IN A CONTEXT
!           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--97/8
!     ORIGINAL VERSION--AUGUST   1997.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*8 IDEFTN
      CHARACTER*8 IRECTN
      CHARACTER*4 IBUGS2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*8 IHOLD
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
   51 FORMAT('***** AT THE BEGINNING OF DPRETN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IDEFTN
   53 FORMAT('IDEFTN = ',A4)
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
!
      IFOUND='YES'
      IRECTN=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)IRECTN
 1181 FORMAT(   &
      'THE TOLERANCE LIMITS FROM SUBSEQUENT RECIPE FIT/ANOVA COMMANDS',   &
      ' WILL BE SAVED IN THE VARIABLE ',A8)
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
 1189 CONTINUE
!
 9000 CONTINUE
      IF(IBUGS2.EQ.'OFF')GO TO 9090
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
!
      RETURN
      END SUBROUTINE DPRETN
      SUBROUTINE DPRF(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,MAXNPP,   &
                      CLLIMI,CLWIDT,                                &
                      ICONT,NUMHPP,IMANUF,                          &
                      XMATN,YMATN,XMITN,YMITN,                      &
                      ISQUAR,IVGMSW,IHGMSW,                         &
                      IMPSW,IMPNR,IMPNC,IMPCO,IMPCO9,               &
                      PMXMIN,PMXMAX,PMYMIN,PMYMAX,                  &
                      IX3AUT,ITIAUT,ICAPSW,                         &
                      IBUGG2,IBUGG3,IBUGQ,                          &
                      IBUGUG,IBUGU2,IBUGU3,IBUGU4,ISUBRO,           &
                      IFOUND,IERROR)
!
!     PURPOSE--GENERATE A R-F SPREAD PLOT WHICH CONSISTS OF
!              THE FOLLOWING 2 SIDE-BY-SIDE PLOTS
!                 1) A PLOT OF FITTED VALUES (MINUS MEAN)
!                 2) A PLOT OF THE RESIDUALS
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--99/9
!     ORIGINAL VERSION--SEPTEMBER 1999
!     UPDATED         --FEBRUARY  2011 CALL LIST TO DPPERC
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES---------------
!
      INCLUDE 'DPCOPA.INC'
!
      CHARACTER*4 ICASPL
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICONT
      CHARACTER*4 IPOWE
      CHARACTER*4 IAND1
      CHARACTER*4 IAND2
      CHARACTER*4 IBUGG2
      CHARACTER*4 IBUGG3
!
      CHARACTER*4 IBUGUG
      CHARACTER*4 IBUGU2
      CHARACTER*4 IBUGU3
      CHARACTER*4 IBUGU4
!
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
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
      CHARACTER*4 ITICX1_SV
      CHARACTER*4 ITICX2_SV
      CHARACTER*4 ITICY1_SV
      CHARACTER*4 ITICY2_SV
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
!
      CHARACTER*4 ITITSV(MAXCH)
!
!-----------------------------------------------------------------
!
      DIMENSION CLLIMI(*)
      DIMENSION CLWIDT(*)
!
      DIMENSION IANSPP(20)
      DIMENSION IANSRP(20)
!
!-----COMMON------------------------------------------------------
!
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOPC.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
!
      DIMENSION PREDSV(MAXOBV)
      EQUIVALENCE (GARBAG(IGARB1),PREDSV(1))
!
!-----COMMON VARIABLES (GENERAL)----------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS---------------------------------------------
!
      DATA (IANSPP(I),I=1,18)   &
      /'Q   ','U   ','A   ','N   ','T   ','I   ','L   ','E   ',   &
       '    ',   &
       'P   ','L   ','O   ','T   ',   &
       '    ',   &
       'P   ','R   ','E   ','D   '/
      DATA (IANSRP(I),I=1,17)   &
      /'Q   ','U   ','A   ','N   ','T   ','I   ','L   ','E   ',   &
       '    ',   &
       'P   ','L   ','O   ','T   ',   &
       '    ',   &
       'R   ','E   ','S   '/
!
!-----START POINT-------------------------------------------------
!
      IFOUND='YES'
      IERROR='NO'
!
      ISUBN1='DPRF'
      ISUBN2='    '
!
      ICASPL='RFPL'
      NDONE=0
      NCPP=18
      NCRP=17
!
!               ******************************************
!               **  TREAT THE RF-PLOT ... ANALYSIS CASE **
!               ******************************************
!
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
          DO 61 I=1,NUMARG
            WRITE(ICOUT,62)I,IHARG(I),IARGT(I)
   62       FORMAT('I,IHARG(I),IARGT(I) = ',I8,2(2X,A4))
            CALL DPWRST('XXX','BUG ')
   61     CONTINUE
        ENDIF
      ENDIF
!
!               **************************************************
!               **   STEP 20--                                  **
!               **   SAVE INITIAL SETTINGS                      **
!               **************************************************
!
      ISTEPN='20'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'4PLO')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      PWXMN2=PWXMIN
      PWXMX2=PWXMAX
      PWYMN2=PWYMIN
      PWYMX2=PWYMAX
      IERAS2=IERASW
      ICOPS2=ICOPSW
      ICHAP2=ICHAPA(1)
      ILINP2=ILINPA(1)
      IFEED9=IFEEDB
      DO 110 I=1,MAXCH
        ITITSV(I)=ITITTE(I)
  110 CONTINUE
      NCTITS=NCTITL
      PTITDZ=PTITDS
      IPPTSV=IPPTBI
      ITUNSV=ITICUN
      ITICX1_SV=ITICX1
      ITICX2_SV=ITICX2
      ITICY1_SV=ITICY1
      ITICY2_SV=ITICY2
      PX1TS1=PX1TOL
      PX1TS2=PX1TOR
!
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
!
      PXMNSV=PXMIN
      PXMXSV=PXMAX
!
      IH11='PRED'
      IH12='    '
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IH11,IH12,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
      ICOL=IVALUE(ILOCV)
      N1=IN(ILOCV)
!
      DO 510 I=1,N1
        PREDSV(I)=PRED(I)
  510 CONTINUE
      IWRITE='OFF'
      CALL MEAN(PRED,N1,IWRITE,PMEAN,IBUGG3,IERROR)
      DO 520 I=1,N1
        PRED(I)=PRED(I)-PMEAN
  520 CONTINUE
!
!               ***************************************************
!               **   STEP 21--                                   **
!               **   GENERATE THE PREDICTED VALUES QUANTILE PLOT **
!               ***************************************************
!
      ISTEPN='21'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DPRF')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      PWXMIN=0.0
      PWXMAX=50.0
      PWYMIN=0.0
      PWYMAX=100.0
      PXMIN=15.0
      PXMAX=100.0
      ICOPSW='OFF'
      IPPTBI='UNBI'
      DO 2105 I=1,MAXCH
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
      ITICX1='SCRE'
      ITICX2='SCRE'
      ITICY1='SCRE'
      ITICY2='SCRE'
      NCTITL=13
      PX1TOL=5.0
      PX1TOR=5.0
      IY1TSW='ON'
      IY2TSW='OFF'
      IY1ZSW='ON'
      IY2ZSW='OFF'
      PTITDS=3.0
      NCY1SA=NCY1LA
!
      ICOM='PERC'
      IHARG(1)='POIN'
      IHARG2(1)='T   '
      IHARG(2)='PLOT'
      IHARG2(2)='    '
      IHARG(3)='PRED'
      IHARG2(3)='    '
      NUMARG=3
      CALL DPPERC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                  CLLIMI,CLWIDT,   &
                  IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 2800
!
      J=0
      DO 2111 I=1,NCPP
      J=J+1
      IF(IX3AUT.EQ.'ON')IX3LTE(J)=IANSPP(I)
      IF(ITIAUT.EQ.'ON')ITITTE(J)=IANSPP(I)
 2111 CONTINUE
      IF(IX3AUT.EQ.'ON')NCTITL=J
      IF(ITIAUT.EQ.'ON')NCTITL=J
      GO TO 2500
!
!               ***************************************************
!               **   STEP 22--                                   **
!               **   GENERATE THE RESIDUAL VALUES QUANTILE PLOT  **
!               ***************************************************
!
 2200 CONTINUE
      ISTEPN='22'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DPRF')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      PWXMIN=50.0
      PWXMAX=100.0
      PWYMIN=0.0
      PWYMAX=100.0
      PXMIN=0.0
      PXMAX=85.0
      ICOPSW='OFF'
      IERASW='OFF'
      DO 2210 I=1,MAXCH
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
!
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
!
      ICOM='PERC'
      IHARG(1)='POIN'
      IHARG2(1)='T   '
      IHARG(2)='PLOT'
      IHARG2(2)='    '
      IHARG(3)='RES '
      IHARG2(3)='    '
      NUMARG=3
      CALL DPPERC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                  CLLIMI,CLWIDT,   &
                  IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 2800
!
      J=0
      DO 2211 I=1,NCRP
      J=J+1
      IF(IX3AUT.EQ.'ON')IX3LTE(J)=IANSRP(I)
      IF(ITIAUT.EQ.'ON')ITITTE(J)=IANSRP(I)
 2211 CONTINUE
      GO TO 2500
!
!               **************************************************
!               **   STEP 25--                                  **
!               **   PLOT THE CURRENT PLOT (OUT OF THE 4)       **
!               **************************************************
 2500 CONTINUE
!
      ICONT=IDCONT(1)
      IPOWE=IDPOWE(1)
      NUMHPP=IDNHPP(1)
      IF(IBUGG3.EQ.'ON')WRITE(ICOUT,2507)IMANUF,NUMDEV,IDMANU(1)
 2507 FORMAT('IMANUF,NUMDEV,IDMANU(1) = ',A4,I8,2X,A4)
      IF(IBUGG3.EQ.'ON')CALL DPWRST('XXX','BUG ')
      IMPARG=2
      CALL DPGRAP(Y,X,X3D,D,N,NPLOTP,ICASPL,ICONT,IPOWE,NUMHPP,   &
      XMATN,YMATN,XMITN,YMITN,   &
      ISQUAR,   &
      IVGMSW,IHGMSW,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,   &
      IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,   &
      YPLOT,XPLOT,X2PLOT,TAGPLO,   &
      IMPSW,IMPNR,IMPNC,IMPCO,IMPCO9,   &
      IMPARG,   &
      PMXMIN,PMXMAX,PMYMIN,PMYMAX,   &
      MAXCOL,   &
      DSIZE,DSYMB,DCOLOR,DFILL,   &
      ICAPSW,   &
      IBUGUG,IBUGU2,IBUGU3,IBUGU4,ISUBRO,   &
      IERROR)
      IF(IERROR.EQ.'NO')IAND1=IAND2
      IF(IERROR.EQ.'YES')GO TO 2800
      NDONE=NDONE+1
      IF(NDONE.LE.1)GO TO 2200
      GO TO 2800
!
!               **************************************************
!               **   STEP 28--                                  **
!               **   REINSTATE INITIAL SETTINGS                 **
!               **************************************************
!
 2800 CONTINUE
      ISTEPN='28'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'4PLO')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
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
      DO 2809 I=1,MAXCH
        ITITTE(I)=ITITSV(I)
 2809 CONTINUE
      NCTITL=NCTITS
      PTITDS=PTITDZ
      NCY1LA=NCY1SA
      IPPTBI=IPPTSV
      ITICUN=ITUNSV
      ITICX1=ITICX1_SV
      ITICX2=ITICX2_SV
      ITICY1=ITICY1_SV
      ITICY2=ITICY2_SV
      PX1TOL=PX1TS1
      PX1TOR=PX1TS2
!
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
      DO 2820 I=1,N1
        PRED(I)=PREDSV(I)
 2820 CONTINUE
      IF(IERROR.EQ.'YES')GO TO 9000
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGG2.EQ.'OFF')GO TO 9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPRF--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IFOUND,IERROR
 9012 FORMAT('IFOUND,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9013 FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',   &
      I8,I8,I8,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)NUMARG
 9014 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NUMARG.LE.0)GO TO 9029
      DO 9021 I=1,NUMARG
      WRITE(ICOUT,9022)I,IHARG(I),IARGT(I)
 9022 FORMAT('I,IHARG(I),IARGT(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9021 CONTINUE
 9029 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPRF
      SUBROUTINE DPRFCO(IHARG,IARG,NUMARG,IDERFC,MAXREG,IREFCO,   &
                        ICASCL,IREFC2,IRGBMX,                     &
                        IBUGP2,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE REGION FILL COLORS = THE COLORS
!              OF THE (BACKGROUND) FILL WITHIN THE REGIONS.
!              THESE ARE LOCATED IN THE VECTOR IREFCO(.).
!     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
!                     --NUMARG
!                     --IDERFC
!                     --MAXREG
!                     --IBUGP2 ('ON' OR 'OFF' )
!     OUTPUT ARGUMENTS--IREFCO (A CHARACTER VECTOR)
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
!     UPDATED         --OCTOBER   2020. SUPPORT FOR "RGB" COLORS
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IDERFC
      CHARACTER*4 ICASCL
      CHARACTER*4 IHARG(*)
      CHARACTER*4 IREFCO(*)
!
      INTEGER IARG(*)
      INTEGER IREFC2(MAXREG,3)
!
      CHARACTER*4 IBUGP2
      CHARACTER*4 ISUBRO
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
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPRF'
      ISUBN2='CO  '
!
      NUMREG=0
      IHOLD1='-999'
!
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'RFCO')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPRFCO--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGP2,ICASCL,IDERFC,IFOUND,IERROR
   52   FORMAT('IBUGP2,ICASCL,IDERFC,IFOUND,IERROR = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)MAXREG,NUMREG,NUMARG,IRGBMX
   53   FORMAT('MAXREG,NUMREG,NUMARG,IRGBMX = ',4I8)
        CALL DPWRST('XXX','BUG ')
        DO 65 I=1,NUMARG
          WRITE(ICOUT,66)IHARG(I)
   66     FORMAT('IHARG(I) = ',A4)
          CALL DPWRST('XXX','BUG ')
   65   CONTINUE
        DO 75 I=1,10
          WRITE(ICOUT,76)I,IREFCO(I)
   76     FORMAT('I,IREFCO(I) = ',I8,2X,A4)
          CALL DPWRST('XXX','BUG ')
   75   CONTINUE
      ENDIF
!
!               **************************************
!               **  STEP 1--                        **
!               **  BRANCH TO THE APPROPRIATE CASE  **
!               **************************************
!
      ISTEPN='1'
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'RFCO')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.1)GO TO 9000
      IF(ICASCL.EQ.'RGB ')GO TO 2000
!
!     THIS IS THE "STANDARD" CASE
!
      IF(NUMARG.EQ.3 .AND. IHARG(3).EQ.'ALL')THEN
        IHOLD1='    '
        GO TO 1300
      ELSEIF(NUMARG.EQ.4)THEN
        IF(IHARG(3).EQ.'ALL')THEN
          IHOLD1=IHARG(4)
          GO TO 1300
        ELSEIF(IHARG(4).EQ.'ALL')THEN
          IHOLD1=IHARG(3)
          GO TO 1300
        ENDIF
      ENDIF
!
!               *************************************************
!               **  STEP 2--                                   **
!               **  TREAT THE SINGLE      SPECIFICATION  CASE  **
!               *************************************************
!
      ISTEPN='2'
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'RFCO')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.2)THEN
        NUMREG=1
        IREFCO(1)=IDERFC
      ELSE
        NUMREG=NUMARG-2
        IF(NUMREG.GT.MAXREG)NUMREG=MAXREG
        DO 1225 I=1,NUMREG
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
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        DO 1278 I=1,NUMREG
          WRITE(ICOUT,1276)I,IREFCO(I)
 1276     FORMAT('THE FILL COLOR OF REGION ',I6,   &
                 ' HAS JUST BEEN SET TO ',A4)
          CALL DPWRST('XXX','BUG ')
 1278   CONTINUE
      ENDIF
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
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'RFCO')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMREG=MAXREG
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2=IDERFC
      IF(IHOLD1.EQ.'OFF')IHOLD2=IDERFC
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDERFC
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDERFC
      DO 1315 I=1,NUMREG
        IREFCO(I)=IHOLD2
 1315 CONTINUE
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        I=1
        WRITE(ICOUT,1316)IREFCO(I)
 1316   FORMAT('THE FILL COLOR OF ALL REGIONS',   &
               ' HAS JUST BEEN SET TO ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IFOUND='YES'
      GO TO 9000
!
!     RGB COLORS CASE: 3 COLORS SHOULD BE GIVEN
!
!                      REGION FILL COLOR
!                      REGION FILL COLOR IRED IBLUE IGREEN
!                      REGION FILL COLOR IRED IBLUE IGREEN ALL
!                      REGION FILL COLOR ALL IRED IBLUE IGREEN
!                      REGION FILL COLOR IRED1 IBLUE1 IGREEN1 IRED2 ...
!
!                      THE "RGB" KEYWORD HAS ALREADY BEEN STRIPPED
!                      OUT.  NOTE THAT THE DEFAULT COLOR IS -999
!                      (I.E., NO RGB COLOR VALUES SPECIFIED).
!
 2000 CONTINUE
!
      JHOLD1=-999
      JHOLD2=-999
      JHOLD3=-999
      NUMREG=MAXREG
!
      IF(NUMARG.EQ.3 .AND. IHARG(3).EQ.'ALL')THEN
        GO TO 2300
      ELSEIF(NUMARG.EQ.6)THEN
        IF(IHARG(3).EQ.'ALL')THEN
          JHOLD1=IARG(4)
          JHOLD2=IARG(5)
          JHOLD3=IARG(6)
          IF(JHOLD1.LT.0 .OR. JHOLD1.GT.IRGBMX)JHOLD1=-1
          IF(JHOLD2.LT.0 .OR. JHOLD2.GT.IRGBMX)JHOLD1=-1
          IF(JHOLD3.LT.0 .OR. JHOLD3.GT.IRGBMX)JHOLD1=-1
          GO TO 2300
        ELSEIF(IHARG(6).EQ.'ALL')THEN
          JHOLD1=IARG(3)
          JHOLD2=IARG(4)
          JHOLD3=IARG(5)
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
      ISTEPN='22'
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'RFCO')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.2)THEN
        NUMREG=1
        IREFC2(1,1)=-1
        IREFC2(1,2)=-1
        IREFC2(1,3)=-1
      ELSE
        NTEMP=NUMARG-2
        NUMREG=NTEMP/3
        IF(NUMREG.LT.1)THEN
          IREFC2(1,1)=-1
          IREFC2(1,2)=-1
          IREFC2(1,3)=-1
        ELSEIF(NUMREG.GT.MAXREG)THEN
          NUMREG=MAXREG
        ENDIF
        DO 2225 I=1,NUMREG
          J1=(I-1)*3+3
          J2=J1+1
          J3=J1+2
          JHOLD1=IARG(J1)
          JHOLD2=IARG(J2)
          JHOLD3=IARG(J3)
          IF(JHOLD1.LT.0 .OR. JHOLD1.GT.IRGBMX)JHOLD1=-1
          IF(JHOLD2.LT.0 .OR. JHOLD2.GT.IRGBMX)JHOLD2=-1
          IF(JHOLD3.LT.0 .OR. JHOLD3.GT.IRGBMX)JHOLD3=-1
          IREFC2(I,1)=JHOLD1
          IREFC2(I,2)=JHOLD2
          IREFC2(I,3)=JHOLD3
 2225   CONTINUE
      ENDIF
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        DO 2278 I=1,NUMREG
          WRITE(ICOUT,2276)I,IREFC2(I,1),IREFC2(I,2),IREFC2(I,3)
 2276     FORMAT('THE RGB FILL COLORS OF REGION ',I6,   &
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
      ISTEPN='23'
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'RFCO')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 2315 I=1,NUMREG
        IREFC2(I,1)=JHOLD1
        IREFC2(I,2)=JHOLD2
        IREFC2(I,3)=JHOLD3
 2315 CONTINUE
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        I=1
        WRITE(ICOUT,2316)IREFC2(I,1),IREFC2(I,2),IREFC2(I,3)
 2316   FORMAT('THE RGB FILL COLORS OF ALL REGIONS HAVE JUST ',   &
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
!
      RETURN
      END SUBROUTINE DPRFCO
      SUBROUTINE DPRFSW(IHARG,NUMARG,IDERFS,MAXREG,IREFSW,   &
      IBUGP2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE REGION FILL SWITCHES = THE ON/OFF SWITCHES
!              OF THE (BACKGROUND) FILL WITHIN THE REGIONS.
!              THESE ARE LOCATED IN THE VECTOR IREFSW(.).
!     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
!                     --NUMARG
!                     --IDERFS
!                     --MAXREG
!                     --IBUGP2 ('ON' OR 'OFF' )
!     OUTPUT ARGUMENTS--IREFSW (A CHARACTER VECTOR)
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
!           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
!           MODIFIED, OR OTHERWISE USED IN A CONTEXT
!           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--DECEMBER  1983.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IDERFS
      CHARACTER*4 IREFSW
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
      DIMENSION IREFSW(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPRF'
      ISUBN2='SW  '
!
      NUMREG=0
      IHOLD1='-999'
      IHOLD2='-999'
!
      IF(IBUGP2.EQ.'OFF')GO TO 90
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
      DO 65 I=1,NUMARG
      WRITE(ICOUT,66)IHARG(I)
   66 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,70)IREFSW(1)
   70 FORMAT('IREFSW(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO 75 I=1,10
      WRITE(ICOUT,76)I,IREFSW(I)
   76 FORMAT('I,IREFSW(I) = ',I8,2X,A4)
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
      NUMREG=1
      IREFSW(1)='ON'
      GO TO 1270
!
 1220 CONTINUE
      NUMREG=NUMARG-2
      IF(NUMREG.GT.MAXREG)NUMREG=MAXREG
      DO 1225 I=1,NUMREG
      J=I+2
      IHOLD1=IHARG(J)
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2='ON'
      IF(IHOLD1.EQ.'OFF')IHOLD2='OFF'
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDERFS
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDERFS
      IREFSW(I)=IHOLD2
 1225 CONTINUE
      GO TO 1270
!
 1270 CONTINUE
      IF(IFEEDB.EQ.'OFF')GO TO 1279
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO 1278 I=1,NUMREG
      WRITE(ICOUT,1276)I,IREFSW(I)
 1276 FORMAT('THE FILL SWITCH FOR REGION ',I6,   &
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
      NUMREG=MAXREG
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2='ON'
      IF(IHOLD1.EQ.'OFF')IHOLD2='OFF'
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDERFS
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDERFS
      DO 1315 I=1,NUMREG
      IREFSW(I)=IHOLD2
 1315 CONTINUE
      GO TO 1370
!
 1370 CONTINUE
      IF(IFEEDB.EQ.'OFF')GO TO 1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1316)IREFSW(I)
 1316 FORMAT('THE FILL SWITCH FOR ALL REGIONS',   &
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
      DO 9025 I=1,NUMARG
      WRITE(ICOUT,9026)IHARG(I)
 9026 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      WRITE(ICOUT,9030)IREFSW(1)
 9030 FORMAT('IREFSW(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO 9035 I=1,10
      WRITE(ICOUT,9036)I,IREFSW(I)
 9036 FORMAT('I,IREFSW(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPRFSW
      SUBROUTINE DPRING(IHARG,IARGT,IARG,NUMARG,   &
                        NUMDEV,IDMANU,IDMODE,IDMOD2,IDMOD3,   &
                        IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,   &
                        IDFONT,   &
                        IBUGD2,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--RING THE BELL IMMEDIATELY
!              FOR A SPECIFIED NUMBER OF RINGS.
!     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
!                     --IARGT  (A  CHARACTER VECTOR)
!                     --IARG   (AN INTEGER VECTOR)
!                     --NUMARG
!     OUTPUT ARGUMENTS--IFOUND ('YES' OR 'NO')
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
!     ORIGINAL VERSION--NOVEMBER  1980.
!     UPDATED         --APRIL     1982.
!     UPDATED         --MAY       1982.
!     UPDATED         --MARCH     1997. SUPPORT FOR DEVICE FONT (ALAN)
!
!-----NON-COMMON VARIABLES----------------------------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IARGT
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
      CHARACTER*4 IFOUND
      CHARACTER*4 IBUGD2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
      DIMENSION IARGT(*)
      DIMENSION IARG(*)
!
      DIMENSION IDMANU(*)
      DIMENSION IDMODE(*)
      DIMENSION IDMOD2(*)
      DIMENSION IDMOD3(*)
!
      DIMENSION IDPOWE(*)
      DIMENSION IDCONT(*)
      DIMENSION IDCOLO(*)
      DIMENSION IDFONT(*)
      DIMENSION IDNVPP(*)
      DIMENSION IDNHPP(*)
      DIMENSION IDUNIT(*)
!
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPCO'
      ISUBN2='SC  '
      IFOUND='NO'
      IERROR='NO'
!
      NUMRIN=1
!
      IBUGG4=IBUGD2
      ISUBG4=ISUBRO
      IERRG4=IERROR
!
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
        DO 61 I=1,NUMARG
          WRITE(ICOUT,62)I,IHARG(I),IARGT(I),IARG(I)
   62     FORMAT('I,IHARG(I),IARGT(I),IARG(I) = ',   &
                 I8,2(2X,A4),2X,I8)
          CALL DPWRST('XXX','BUG ')
   61   CONTINUE
        DO 71 I=1,NUMDEV
          WRITE(ICOUT,72)I,IDMANU(I),IDMODE(I),IDMOD2(I),IDMOD3(I)
   72     FORMAT('I,IDMANU(I),IDMODE(I),IDMOD2(I),IDMOD3(I) = ',   &
                 I8,4(2X,A4))
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,73)I,IDPOWE(I),IDCONT(I),IDCOLO(I)
   73     FORMAT('I,IDPOWE(I),IDCONT(I),IDCOLO(I) = ',   &
                 I8,3(2X,A4))
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
!
!               ********************************************************
!               **  STEP 1--                                          **
!               **  EXTRACT NEEDED INFORMATION FROM THE COMMAND LINE  **
!               ********************************************************
!
      IF(NUMARG.GE.1.AND.IARGT(NUMARG).EQ.'NUMB')GO TO 1120
      GO TO 1110
!
 1110 CONTINUE
      NUMRIN=1
      GO TO 1150
!
 1120 CONTINUE
      NUMRIN=IARG(NUMARG)
      GO TO 1150
!
 1150 CONTINUE
      IFOUND='YES'
!
!               ********************************
!               **  STEP 2--                  **
!               **  STEP THROUGH EACH DEVICE  **
!               ********************************
!
      IF(NUMDEV.LE.0)GO TO 9000
      DO 8000 IDEVIC=1,NUMDEV
!
        IF(IDPOWE(IDEVIC).EQ.'OFF')GO TO 8000
        IF(IDMANU(IDEVIC).EQ.'OFF')GO TO 8000
        IF(IDMANU(IDEVIC).EQ.'DISC')GO TO 8000
        IF(IDMANU(IDEVIC).EQ.'NULL')GO TO 8000
        IF(IDMANU(IDEVIC).EQ.'NONE')GO TO 8000
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
        IGUNIT=IDUNIT(IDEVIC)
!
!               ****************************************
!               **  STEP 2.1--                        **
!               **  TREAT THE RING BELL CASE          **
!               ****************************************
!
        ISTEPN='2.1'
        IF(IBUGD2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        IF(NUMRIN.GE.1)THEN
          DO 1200 I=1,NUMRIN
!
            IF(IBUGD2.EQ.'ON')THEN
              WRITE(ICOUT,1205)
 1205         FORMAT('***** THE BELL SHOULD SOUND NOW *****')
              CALL DPWRST('XXX','BUG ')
            ENDIF
            CALL GRRIBE
 1200     CONTINUE
        ENDIF
!
 8000 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPRING
      SUBROUTINE DPRIPL(NPLOTV,NPLOTP,N2,ICASPL,IAND1,IAND2,         &
                        IKDENP,PKDEWI,ISEED,                         &
                        CLLIMI,CLWIDT,                               &
                        ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
!
!     PURPOSE--GENERATE A RIDGELINE PLOT
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2025/06
!     ORIGINAL VERSION--JUNE      2025.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASPL
      CHARACTER*4 IAND1
      CHARACTER*4 IAND2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGG2
      CHARACTER*4 IBUGG3
      CHARACTER*4 IBUGQ
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 ICASE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IMULT
!
      DIMENSION CLLIMI(*)
      DIMENSION CLWIDT(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZD.INC'
      INCLUDE 'DPCOZZ.INC'
!
      DOUBLE PRECISION Y1(MAXOBV)
      DOUBLE PRECISION SMOOTH(MAXOBV)
      DOUBLE PRECISION FT(MAXOBV)
      DOUBLE PRECISION ZY(MAXOBV)
!
      DIMENSION X1(MAXOBV)
      DIMENSION XDIST(MAXOBV)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TEMP3(MAXOBV)
      DIMENSION TEMP4(MAXOBV)
      DIMENSION WORK1(MAXOBV)
      DIMENSION WORK2(MAXOBV)
      DIMENSION WORK3(MAXOBV)
      DIMENSION WORK4(MAXOBV)
      DIMENSION WORK5(MAXOBV)
      DIMENSION WORK6(MAXOBV)
      DIMENSION WORK7(MAXOBV)
      DIMENSION WORK8(MAXOBV)
      DIMENSION WORK9(MAXOBV)
      DIMENSION WORK10(MAXOBV)
      DIMENSION WORK11(4,MAXOBV)
      DIMENSION WORK12(MAXOBV,3)
      DIMENSION Y2T(MAXOBV)
      DIMENSION X2T(MAXOBV)
      DIMENSION D2T(MAXOBV)
!
      EQUIVALENCE (DGARBG(IDGAR1),Y1(1))
      EQUIVALENCE (DGARBG(IDGAR2),SMOOTH(1))
      EQUIVALENCE (DGARBG(IDGAR3),FT(1))
      EQUIVALENCE (DGARBG(IDGAR4),ZY(1))
!
      EQUIVALENCE (GARBAG(IGARB1),X1(1))
      EQUIVALENCE (GARBAG(IGARB2),XDIST(1))
      EQUIVALENCE (GARBAG(IGARB3),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB4),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB5),TEMP3(1))
      EQUIVALENCE (GARBAG(IGARB6),TEMP4(1))
      EQUIVALENCE (GARBAG(IGARB7),WORK1(1))
      EQUIVALENCE (GARBAG(IGARB8),WORK2(1))
      EQUIVALENCE (GARBAG(IGARB9),WORK3(1))
      EQUIVALENCE (GARBAG(IGAR10),WORK4(1))
      EQUIVALENCE (GARBAG(JGAR11),WORK5(1))
      EQUIVALENCE (GARBAG(JGAR12),WORK6(1))
      EQUIVALENCE (GARBAG(JGAR13),WORK7(1))
      EQUIVALENCE (GARBAG(JGAR14),WORK8(1))
      EQUIVALENCE (GARBAG(JGAR15),WORK9(1))
      EQUIVALENCE (GARBAG(JGAR16),WORK10(1))
      EQUIVALENCE (GARBAG(JGAR17),X2T(1))
      EQUIVALENCE (GARBAG(JGAR18),Y2T(1))
      EQUIVALENCE (GARBAG(JGAR19),D2T(1))
      EQUIVALENCE (GARBAG(IGAR11),WORK11(1,1))
      EQUIVALENCE (GARBAG(IGAR15),WORK12(1,1))
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
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPRI'
      ISUBN2='PL  '
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
      MAXV2=1
!
!               ***************************************************
!               **  TREAT THE RIDGELINE PLOT                     **
!               ***************************************************
!
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'RIPL')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPRIPL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASPL,IAND1,IAND2
   52   FORMAT('ICASPL,IAND1,IAND2 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGG2,IBUGG3,IBUGQ
   53   FORMAT('IBUGG2,IBUGG3,IBUGQ = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ******************************************************
!               **  STEP 1--                                        **
!               **  EXTRACT THE COMMAND                             **
!               **  LOOK FOR ONE OF THE FOLLOWING COMMANDS:         **
!               **    1) RIDGELINE PLOT Y X                         **
!               **    2) MULTIPLE RIDGELINE PLOT Y1 ... YK          **
!               ******************************************************
!
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'RIPL')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IMULT='OFF'
      IF(ICOM.EQ.'RIDG' .AND. IHARG(1).EQ.'PLOT')THEN
        ILASTC=1
        IFOUND='YES'
      ELSEIF(ICOM.EQ.'MULT' .AND. IHARG(1).EQ.'RIDG' .AND.      &
        IHARG(2).EQ.'PLOT')THEN
        IMULT='ON'
        ILASTC=2
        IFOUND='YES'
      ELSE
        GO TO 9000
      ENDIF
!
      ICASPL='RIPL'
      IF(IRIPTY.EQ.'HIST' .OR. IRIPTY.EQ.'CHIS')ICASPL='RIPH'
      IF(ILASTC.GE.1)THEN
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
        ILASTC=0
      ENDIF
!
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'RIPL')THEN
        WRITE(ICOUT,112)ICASPL,IMULT
  112   FORMAT('ICASPL,IMULT = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ****************************************
!               **  STEP 2--                          **
!               **  EXTRACT THE VARIABLE LIST         **
!               ****************************************
!
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'RIPL')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      INAME='RIDGELINE PLOT'
      MINNA=1
      MAXNA=100
      MINN2=10
      IFLAGE=1
      IF(IMULT.EQ.'ON')IFLAGE=0
      IFLAGM=0
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=1
      MAXNVA=30
!
      CALL DPPARS(IHARG,IHARG2,IARGT,ARG,NUMARG,IANS,IWIDTH,   &
                  IHNAME,IHNAM2,IUSE,NUMNAM,IN,IVALUE,VALUE,   &
                  JMIN,JMAX,                                   &
                  MINN2,MINNA,MAXNA,MAXSPN,IFLAGE,INAME,       &
                  IVARN1,IVARN2,IVARTY,PVAR,                   &
                  ILIS,NRIGHT,ICOLR,ISUB,NQ,ILOCQ,NUMVAR,      &
                  MINNVA,MAXNVA,                               &
                  IFLAGM,IFLAGP,                               &
                  IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'RIPL')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,281)
  281   FORMAT('***** AFTER CALL DPPARS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,282)NQ,NUMVAR
  282   FORMAT('NQ,NUMVAR = ',2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMVAR.GT.0)THEN
          DO I=1,NUMVAR
            WRITE(ICOUT,287)I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),      &
                            ICOLR(I)
  287       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),ICOLR(I) = ', &
                   I8,2(2X,A4),3I8)
            CALL DPWRST('XXX','BUG ')
          ENDDO
        ENDIF
      ENDIF
!
      NRESP=2
      NREPL=0
      IF(NUMVAR.GT.2)IMULT='ON'
      IF(IMULT.EQ.'ON')NRESP=NUMVAR
      CLWID=CLWIDT(1)
      XSTART=CLLIMI(1)
      XSTOP=CLLIMI(2)
!
!               ********************************************
!               **  STEP 6--                              **
!               **  GENERATE THE RIDGELINE      PLOT FOR  **
!               **  THE VARIOUS CASES.                    **
!               ********************************************
!
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'RIPL')THEN
        ISTEPN='6'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,601)NRESP,NREPL
  601   FORMAT('NRESP,NREPL = ',2I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *************************************************
!               **  STEP 7A--                                  **
!               **  CASE 1: SINGLE RESPONSE VARIABLE WITH NO   **
!               **          REPLICATION (RESPONSE VARIABLE CAN **
!               **          BE A MATRIX).                      **
!               *************************************************
!
      MAXNXT=MAXOBV
      IF(IMULT.EQ.'OFF')THEN
        ISTEPN='7A'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'RIPL')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        ICOL=1
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,           &
                    INAME,IVARN1,IVARN2,IVARTY,                  &
                    ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,            &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,                 &
                    MAXCP4,MAXCP5,MAXCP6,                        &
                    V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,        &
                    TEMP1,X1,TEMP2,N2,NLOCA2,NLOCA3,ICASE,       &
                    IBUGG3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
        Y1(1:N2)=DBLE(TEMP1(1:N2))
!
!               *****************************************************
!               **  STEP 7B--                                      **
!               **  FORM THE VERTICAL AND HORIZONTAL AXIS          **
!               **  VALUES Y(.) AND X(.) FOR THE PLOT.             **
!               **  RESET THE VECTOR D(.) TO ALL ONES.             **
!               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).  **
!               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).  **
!               *****************************************************
!
        NCURVE=1
        NPLOTP=0
        CALL DPRIP2(Y1,X1,N2,ZY,XDIST,MAXNXT,                    &
                    FT,SMOOTH,TEMP1,TEMP2,TEMP3,TEMP4,           &
                    WORK1,WORK2,WORK3,WORK4,WORK5,WORK6,WORK7,   &
                    WORK8,WORK9,WORK10,WORK11,WORK12,            &
                    Y2T,X2T,D2T,                                 &
                    PRIPHE,IRIPTY,                               &
                    IKDENP,PKDEWI,IKDERN,ISEED,                  &
                    CLWID,XSTART,XSTOP,                          &
                    IHSTCW,IHSTEB,IHSTOU,IASHWT,IHSTMC,IHSTOP,   &
                    Y,X,D,X3D,DFILL,NPLOTP,NPLOTV,               &
                    IBUGG3,ISUBRO,IERROR)
!
!               ******************************************
!               **  STEP 8A--                           **
!               **  CASE 2: MULTIPLE RESPONSE VARIABLES **
!               ******************************************
!
      ELSEIF(IMULT.EQ.'ON')THEN
        ISTEPN='8A'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'RIPL')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!       LOOP THROUGH EACH OF THE RESPONSE VARIABLES
!
        N2=0
        NCURVE=1
        DO IRESP=1,NRESP
!
          IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'RIPL')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,811)IRESP,NCURVE
  811       FORMAT('IRESP,NCURVE = ',2I5)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
          ICOL=IRESP
          NUMVA2=1
          CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,              &
                      INAME,IVARN1,IVARN2,IVARTY,                     &
                      ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,               &
                      MAXCOL,MAXCP1,MAXCP2,MAXCP3,                    &
                      MAXCP4,MAXCP5,MAXCP6,                           &
                      V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,           &
                      TEMP1,TEMP1,TEMP1,NLOCAL,NLOCA2,NLOCA3,ICASE,   &
                      IBUGG3,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
          DO I=1,NLOCAL
            N2=N2+1
            Y1(N2)=DBLE(TEMP1(I))
            X1(N2)=REAL(IRESP)
          ENDDO
        ENDDO
!
!               *****************************************************
!               **  STEP 8B--                                      **
!               **  FORM THE VERTICAL AND HORIZONTAL AXIS          **
!               **  VALUES Y(.) AND X(.) FOR THE PLOT.             **
!               *****************************************************
!
        NPLOTP=0
        CALL DPRIP2(Y1,X1,N2,ZY,XDIST,MAXNXT,                    &
                    FT,SMOOTH,TEMP1,TEMP2,TEMP3,TEMP4,           &
                    WORK1,WORK2,WORK3,WORK4,WORK5,WORK6,WORK7,   &
                    WORK8,WORK9,WORK10,WORK11,WORK12,            &
                    Y2T,X2T,D2T,                                 &
                    PRIPHE,IRIPTY,                               &
                    IKDENP,PKDEWI,IKDERN,ISEED,                  &
                    CLWID,XSTART,XSTOP,                          &
                    IHSTCW,IHSTEB,IHSTOU,IASHWT,IHSTMC,IHSTOP,   &
                    Y,X,D,X3D,DFILL,NPLOTP,NPLOTV,               &
                    IBUGG3,ISUBRO,IERROR)
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'RIPL')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPRIPL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTV,NPLOTP,N2,ICASPL,IAND1,IAND2
 9013   FORMAT('NPLOTV,NPLOTP,N2,ICASPL,IAND1,IAND2 = ',3I8,3(2X,A4))
        CALL DPWRST('XXX','BUG ')
        IF(NPLOTP.GT.0)THEN
          DO I=1,NPLOTP
            WRITE(ICOUT,9016)I,Y(I),X(I),D(I)
 9016       FORMAT('I,Y(I),X(I),D(I) = ',I8,3F12.5)
           CALL DPWRST('XXX','BUG ')
          ENDDO
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPRIPL
      SUBROUTINE DPRIP2(Y,X,N,YTEMP,XDIST,MAXNXT,                    &
                        FT,SMOOTH,TEMP1,TEMP2,TEMP3,TEMP4,           &
                        WORK1,WORK2,WORK3,WORK4,WORK5,WORK6,         &
                        WORK7,WORK8,WORK9,WORK10,WORK11,WORK12,      &
                        Y2T,X2T,D2T,                                 &
                        PRIPHE,IRIPTY,                               &
                        IKDENP,PKDEWI,IKDERN,ISEED,                  &
                        CLWID,XSTART,XSTOP,                          &
                        IHSTCW,IHSTEB,IHSTOU,IASHWT,IHSTMC,IHSTOP,   &
                        Y2,X2,D2,X3D2,DFILL,NPLOTP,NPLOTV,           &
                        IBUGG3,ISUBRO,IERROR)
!
!     PURPOSE--GENERATE A RIDGELINE PLOT
!     REFERENCE--Wilke, C. O.(2019). Fundamentals of data visualization:
!                a primer on making informative and compelling figures.
!                O'Reilly Media.(See pages, 39, 43 88-91, 196).
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2025/07
!     ORIGINAL VERSION--JUNE      2025.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IRIPTY
      CHARACTER*4 IHSTCW
      CHARACTER*4 IHSTEB
      CHARACTER*4 IHSTOU
      CHARACTER*4 IHSTOP
      CHARACTER*4 IASHWT
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ICASPL
      CHARACTER*4 IRELATT
      CHARACTER*4 IKDEPFT
      CHARACTER*4 IHIGHT
      CHARACTER*4 IDATSWT
      CHARACTER*4 IRHSTGT
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IWRITE
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
      DIMENSION XDIST(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
      DIMENSION TEMP4(*)
      DIMENSION WORK1(*)
      DIMENSION WORK2(*)
      DIMENSION WORK3(*)
      DIMENSION WORK4(*)
      DIMENSION WORK5(*)
      DIMENSION WORK6(*)
      DIMENSION WORK7(*)
      DIMENSION WORK8(*)
      DIMENSION WORK9(*)
      DIMENSION WORK10(*)
      DIMENSION WORK11(4,MAXNXT)
      DIMENSION WORK12(MAXNXT,3)
      DIMENSION Y2T(*)
      DIMENSION X2T(*)
      DIMENSION D2T(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
      DIMENSION X3D2(*)
      DIMENSION DFILL(*)
!
      DOUBLE PRECISION Y(*)
      DOUBLE PRECISION YTEMP(*)
      DOUBLE PRECISION FT(*)
      DOUBLE PRECISION SMOOTH(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPRI'
      ISUBN2='P2  '
      IERROR='NO'
      IWRITE='OFF'
!
      AFACT=1.0
      MINN2=10
!
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'RIP2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPRIP2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGG3,ISUBRO,IRIPTY,PRIPHE,N
   52   FORMAT('IBUGG3,ISUBRO,IRIPTY,PRIPHE,N = ',3(A4,2X),F10.2,I8)
        CALL DPWRST('XXX','BUG ')
        DO I=1,N
          WRITE(ICOUT,62)I,Y(I),X(I)
   62     FORMAT('I,Y(I),X(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDDO
      ENDIF
!
!               ********************************************
!               **  STEP 11--                             **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      IF(N.LT.10)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR IN RIDGELINE PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)
  102   FORMAT('      THE NUMBER OF OBSERVATIONS MUST BE AT LEAST 10.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,104)N
  104   FORMAT('      THE NUMBER OF OBSERVATIONS = ',I6)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               ****************************************************
!               **  STEP 2--LOOP THROUGH THE GROUPS               **
!               ****************************************************
!
      ISTEPN='21'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'RIP2')   &
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DISTIN(X,N,IWRITE,XDIST,NUMSET,IBUGG3,IERROR)
      CALL SORT(XDIST,NUMSET,XDIST)
!
      NCURVE=0
      N2=0
      N2SAVE=0
      ABASE=0.0
      DO ISET=1,NUMSET
         HOLD=XDIST(ISET)
         ICNT=0
         DO IROW=1,N
            IF(X(IROW).EQ.HOLD)THEN
              ICNT=ICNT+1
              YTEMP(ICNT)=Y(IROW)
            ENDIF
         ENDDO
!
         IF(IRIPTY.EQ.'HIST' .OR. IRIPTY.EQ.'CHIS')THEN
           IRELATT='ON'
           ICASPL='HIST'
           IF(IRIPTY.EQ.'CHIS')ICASPL='CUMH'
           IHIGHT='OFF'
           IDATSWT='RAW'
           IRHSTGT='AREA'
           NPLOTP=0
           M=0
           CALL DPHIS2(YTEMP,TEMP1,TEMP2,ICNT,                               &
                       ICASPL,IRELATT,IHIGHT,IDATSWT,CLWID,XSTART,XSTOP,     &
                       TEMP3,TEMP4,WORK1,WORK2,MAXNXT,                       &
                       IRHSTGT,IHSTCW,IHSTEB,IHSTOU,IASHWT,M,IHSTMC,IHSTOP,  &
                       WORK3,WORK4,WORK5,WORK6,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
           DO II=1,NPLOTP
              N2=N2+1
              Y2(N2)   =WORK3(II) + ABASE
              X2(N2)   =WORK4(II)
              X3D2(N2) =CPUMIN
              D2(N2)   =REAL(ISET)
              DFILL(N2)=ABASE
           ENDDO
           ABASE=ABASE+PRIPHE
         ELSEIF(IRIPTY.EQ.'KDEN' .OR. IRIPTY.EQ.'CKDE')THEN
           IKDEPFT='PDF'
           IF(IRIPTY.EQ.'CKDE')IKDEPFT='CDF'
           NCURVE=NCURVE+1
           MINN2=10
           NCURVET=1
           N2TEMP=0
           CALL DPKDE2(YTEMP,FT,SMOOTH,TEMP1,TEMP2,TEMP3,TEMP4,NCURVET,  &
                       ICNT,ICASPL,IKDENP,PKDEWI,                        &
                       IKDERN,IKDEPFT,ISEED,MINN2,                       &
                       WORK1,WORK2,WORK3,WORK4,WORK5,WORK6,WORK7,        &
                       WORK8,WORK9,WORK10,WORK11,WORK12,MAXNXT,          &
                       Y2T,X2T,D2T,N2TEMP,NPLOTV,IBUGG3,ISUBRO,IERROR)
           IF(IERROR.EQ.'YES')GO TO 9000
           IF(ABASE.GT.0.0)THEN
             DO II=1,N2TEMP
                N2=N2+1
                Y2(N2)=Y2T(II)+ABASE
                X2(N2)=X2T(II)
                D2(N2)=REAL(NCURVE)
                X3D2(N2)=ABASE
             ENDDO
           ELSE
             DO II=1,N2TEMP
                N2=N2+1
                Y2(N2)=Y2T(II)
                X2(N2)=X2T(II)
                D2(N2)=REAL(NCURVE)
                X3D2(N2)=ABASE
             ENDDO
           ENDIF
           ABASE=ABASE+PRIPHE
           N2SAVE=N2
         ENDIF
!
      ENDDO
!
      NPLOTV=3
      NPLOTP=N2
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'RIP2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPRIP2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR,NPLOTP
 9012   FORMAT('IERROR,NPLOTP = ',A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
        DO I=1,NPLOTP
          WRITE(ICOUT,9016)I,Y2(I),X2(I),D2(I),X3D2(I),DFILL(I)
 9016     FORMAT('I,Y2(I),X2(I),D2(I),DFILL(I) = ',I8,2G15.7,F7.1,2G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDDO
      ENDIF
!
      RETURN
      END SUBROUTINE DPRIP2
      SUBROUTINE DPRK(ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,   &
                      PARAM,IPARN,IPARN2,IANGLU,   &
                      TEMPX,TEMPY,TEMPYD,   &
                      IBUGA3,IBUGCO,IBUGEV,IBUGQ,ISUBRO,IERROR)
!
!     PURPOSE--TREAT THE LET CASE FOR
!              FINDING THE RUNGE-KUTTA SOLITION
!              OF A DIFFERENTIAL EQUATION
!              (FOR A FULL OR PARTIAL DATA SET)
!     EXAMPLE--LET Y = RUNGE-KUTTA EXP(X-Y) X
!            --LET Y = RUNGE-KUTTA F X
!     NOTE--THIS SUBROUTINE OPERATES ON A FUNCTION AND A VECTOR
!           AND PRODUCES A VECTOR.
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
!     ORIGINAL VERSION--SEPTEMBER 1987.
!     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE COMMON
!     UPDATED         --APRIL     1992. MANY SMALL CHANGES
!     UPDATED         --SEPTEMBER 1993. ADD INPUT ARGUMENT ISUBRO
!     UPDATED         --SEPTEMBER 1993. ADD ISUBRO TO TRACE STATEMENTS
!     UPDATED         --SEPTEMBER 2015. SUPPORT FOR FUNCTION BLOCKS
!     UPDATED         --JULY      2019. CREATE SCRATCH SPACE IN CALLING
!                                       ROUTINE (DPLET)
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASL7
      CHARACTER*4 IANGLU
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGQ
      CHARACTER*4 IBUGCO
      CHARACTER*4 IBUGEV
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
      CHARACTER*4 IFOUND
!
      CHARACTER*4 NEWNA1
      CHARACTER*4 NEWNA2
      CHARACTER*4 NEWCOL
      CHARACTER*4 ICASEQ
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 IWRITE
!
      CHARACTER*4 IHRI11
      CHARACTER*4 IHRI12
      CHARACTER*4 ILEF11
      CHARACTER*4 ILEF12
      CHARACTER*4 ILEF21
      CHARACTER*4 ILEF22
      CHARACTER*4 IHSET
      CHARACTER*4 IHSET2
!
      CHARACTER*4 ITYPEH
      CHARACTER*4 IW21HO
      CHARACTER*4 IW22HO
      CHARACTER*4 IPARN
      CHARACTER*4 IPARN2
!
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
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IH1
      CHARACTER*4 IH2
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
!
      DIMENSION ITYPEH(*)
      DIMENSION IW21HO(*)
      DIMENSION IW22HO(*)
      DIMENSION W2HOLD(*)
!
      DIMENSION PARAM(*)
      DIMENSION IPARN(*)
      DIMENSION IPARN2(*)
!
      DIMENSION IDUMV(100)
      DIMENSION IDUMV2(100)
!
      DIMENSION ILAB(10)
!CCCC THE FOLLOWING 4 LINES WERE COMMENTED OUT   APRIL 1992
!CCCC DIMENSION IOLD(10)
!CCCC DIMENSION IOLD2(10)
!CCCC DIMENSION INEW(10)
!CCCC DIMENSION INEW2(10)
!
      DIMENSION TEMPX(*)
      DIMENSION TEMPY(*)
      DIMENSION TEMPYD(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOFB.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!               *******************************************
!               **  TREAT THE RUNGE-KUTTA    SUBCASE     **
!               **  OF THE LET COMMAND                   **
!               *******************************************
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DPRK')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPRK--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,IBUGCO,IBUGEV,IBUGCO,ISUBRO,ICASL7
   52   FORMAT('IBUGA3,IBUGCO,IBUGEV,IBUGQ,ISUBRO,ICASL7 = ',   &
               5(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               **********************************
!               **  STEP 1--                    **
!               **  INITIALIZE SOME VARIABLES.  **
!               **********************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
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
!
!               *******************************************************
!               **  STEP 2--                                         **
!               **  CHECK FOR THE PROPER NUMBER OF INPUT ARGUMENTS.  **
!               *******************************************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      MINNA=4
      MAXNA=100
      CALL CHECKA(NUMARG,MINNA,MAXNA,IANS,IWIDTH,ISUBN1,ISUBN2,   &
      IERROR)
      IF(IERROR.EQ.'YES')GO TO 19000
!
!
!               ****************************************************************
!               **  STEP 2A--                                                  *
!               **  EXAMINE THE LEFT-HAND SIDE--                               *
!               **  IS THE FIRST  VARIABLE NAME TO LEFT OF = SIGN              *
!               **  ALREADY IN THE NAME LIST?    AS A VARIABLE?                *
!               **  NOTE THAT     ILEF11     IS THE NAME OF THE VARIABLE
!               **  ON THE LEFT.                                               *
!               **  NOTE THAT     ILISL1    IS THE LINE IN THE TABLE           *
!               **  OF THE NAME ON THE LEFT.                                   *
!               **  NOTE THAT     ICOLL1    IS THE DATA COLUMN (1 TO 12)
!               **  FOR THE NAME OF THE LEFT.                                  *
!               ****************************************************************
!
      ISTEPN='2A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ILEF11=IHARG(1)
      ILEF12=IHARG2(1)
      DO 210 I=1,NUMNAM
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
  242         FORMAT('      THE NUMBER OF DATA COLUMNS HAS JUST ',   &
                     'EXCEEDED')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,243)MAXCOL
  243         FORMAT('      THE MAX ALLOWABLE ',I8,'  .  SUGGESTED ',   &
                     'ACTION--')
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
              GO TO 19000
            ENDIF
            GO TO 290
          ELSEIF(IUSE(I).EQ.'V')THEN
            NUMVAL=1
            ILISL1=I2
            ICOLL1=IVALUE(ILISL1)
            NIOLD=IN(ILISL1)
            GO TO 290
          ENDIF
        ENDIF
  210 CONTINUE
!
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
      GO TO 19000
!
!
  290 CONTINUE
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')THEN
        WRITE(ICOUT,291)
  291   FORMAT('AT THE END OF STEP 2A--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,292)ILEF11,ILEF12,NEWNA1,NUMNAM,ILISL1,NUMCOL,   &
                        ICOLL1,NIOLD
  292   FORMAT('ILEF11,ILEF12,NEWNA1,NUMNAM,ILISL1,NUMCOL,ICOLL1,',   &
               'NIOLD = ',A4,A4,2X,A4,2X,5I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ****************************************************************
!               **  STEP 2B--                                                  *
!               **  EXAMINE THE LEFT-HAND SIDE--                               *
!               **  IS THE SECOND VARIABLE NAME TO LEFT OF = SIGN              *
!               **  ALREADY IN THE NAME LIST?    AS A VARIABLE?                *
!               **  NOTE THAT     ILEF21     IS THE NAME OF THE VARIABLE
!               **  ON THE LEFT.                                               *
!               **  NOTE THAT     ILISL2    IS THE LINE IN THE TABLE           *
!               **  OF THE NAME ON THE LEFT.                                   *
!               **  NOTE THAT     ICOLL2    IS THE DATA COLUMN (1 TO 12)
!               **  FOR THE NAME OF THE LEFT.                                  *
!               ****************************************************************
!
      ISTEPN='2B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     2015/09: CHECK TO SEE IF THE FIRST ARGUMENT ON RHS IS A FUNCTION
!              BLOCK NAME.
!
      IF(IHARG(4).EQ.IFBNA1(1:4) .AND. IHARG2(4).EQ.IFBNA1(5:8))THEN
        IFLGFB=1
      ELSEIF(IHARG(4).EQ.IFBNA2(1:4) .AND. IHARG2(4).EQ.IFBNA2(5:8))THEN
        IFLGFB=2
      ELSEIF(IHARG(4).EQ.IFBNA3(1:4) .AND. IHARG2(4).EQ.IFBNA3(5:8))THEN
        IFLGFB=3
      ELSE
        IFLGFB=0
      ENDIF
!
      IF(IHARG(2).NE.'=')THEN
        ILEF21=IHARG(2)
        ILEF22=IHARG2(2)
        DO 310 I=1,NUMNAM
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
                GO TO 19000
              ENDIF
              GO TO 390
            ELSEIF(IUSE(I).EQ.'V')THEN
              NUMVAL=2
              ILISL2=I2
              ICOLL2=IVALUE(ILISL2)
              GO TO 390
            ENDIF
          ENDIF
  310   CONTINUE
!
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
        GO TO 19000
!
  390   CONTINUE
!
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DPRK')THEN
          WRITE(ICOUT,391)
  391     FORMAT('AT THE END OF STEP 2--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,392)ILEF21,ILEF22,NEWNA2,NUMNAM,ILISL2,NUMCOL,   &
                          ICOLL2,NIOLD
  392     FORMAT('ILEF21,ILEF22,NEWNA1,NUMNAM,ILISL2,NUMCOL,ICOLL2,',   &
                 'NIOLD = ',A4,A4,2X,A4,2X,5I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
      ENDIF
!
!               ***********************************************************
!               **  STEP 4--                                             **
!               **  FIND    THE VARIABLE ON THE RIGHT-HAND SIDE--        **
!               **  (THIS WILL BE THE VARIABLE OF DIFFERENTIATION AND    **
!               **  HORIZONTAL AXIS VARIABLE.  HAS THIS VARIABLE ON THE  **
!               **  RIGHT ALREADY BEEN DEFINED?  NOTE THAT    ILISR1     **
!               **  IS THE LINE IN THE TABLE OF THIS VARIABLE ON THE     **
!               **  RIGHT.  NOTE THAT     ICOLR1    IS THE DATA COLUMN   **
!               **  OF THIS VARIABLE ON THE RIGHT.                       **
!               ***********************************************************
!
!               ********************************************
!               **  STEP 4.1--                            **
!               **  DETERMINE THE LOCATION                **
!               **  OF THE VARIABLE ON THE RIGHT          **
!               ********************************************
!
      ISTEPN='4.1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMVAR=1
!
      DO 1005 I=4,NUMARG
        I2=I
        IH1=IHARG(I)
        IH2=IHARG2(I)
        IF(IH1.EQ.'SUBS'.AND.IH2.EQ.'ET  ')GO TO 1007
        IF(IH1.EQ.'EXCE'.AND.IH2.EQ.'PT  ')GO TO 1007
        IF(IH1.EQ.'FOR '.AND.IH2.EQ.'    ')GO TO 1007
 1005 CONTINUE
      ILOCQ=NUMARG+1
      GO TO 1009
 1007 CONTINUE
      ILOCQ=I2
      GO TO 1009
 1009 CONTINUE
      ILOCR1=ILOCQ-1
      ILOCR2=ILOCR1+1
      ILOCR3=ILOCR1+2
      ILOCR4=ILOCR1+3
!CCCC THE FOLLOWING 2 LINES WERE ADDED   APRIL 1992
      ILOCR5=ILOCR1+4
      ILOCR6=ILOCR1+5
!
!               ***************************************
!               **  STEP 5.1--                       **
!               **  EXAMINE THIS VARIABLE            **
!               **  ON THE RIGHT.                    **
!               ***************************************
!
!
      ISTEPN='5.1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IHRI11=IHARG(ILOCR1)
      IHRI12=IHARG2(ILOCR1)
      DO 1120 I=1,NUMNAM
        I2=I
        IF(IHRI11.EQ.IHNAME(I).AND.IHRI12.EQ.IHNAM2(I))THEN
          IF(IUSE(I).EQ.'V')THEN
            ILISR1=I2
            ICOLR1=IVALUE(ILISR1)
            NIRIG1=IN(ILISR1)
            GO TO 1190
          ELSEIF(IUSE(I).EQ.'P')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,211)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1142)
 1142       FORMAT('      THE SPECIFIED DIFFERENTIATION VARIABLE ON ',   &
                   'THE RIGHT')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1145)
 1145       FORMAT('      OF THE = SIGN WAS FOUND IN THE INTERNAL ',   &
                   'NAME LIST,')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1147)
 1147       FORMAT('      BUT AS A PARAMETER AND NOT AS A VARIABLE ',   &
                   'AS IT SHOULD BE HERE.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1128)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1129)(IANS(II),II=1,MIN(100,IWIDTH))
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 19000
          ENDIF
        ENDIF
 1120 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,211)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      THE SPECIFIED DIFFERENTIATION VARIABLE ON THE ',   &
             'RIGHT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1125)
 1125 FORMAT('      OF THE = SIGN WAS NOT FOUND IN THE INTERNAL NAME ',   &
             'LIST')
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
      GO TO 19000
!
 1190 CONTINUE
!
!
!               *******************************
!               **  STEP 7--                 **
!               **  DETERMINE THE SUBCASE    **
!               **  AND BRANCH ACCORDINGLY.  **
!               *******************************
!
!
      ISTEPN='7'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ILOCR1.EQ.NUMARG)GO TO 8000
      IF(ILOCR1.LT.NUMARG.AND.IHARG(ILOCR2).EQ.'SUBS'.AND.   &
      IHARG2(ILOCR2).EQ.'ET  ')GO TO 9000
      IF(ILOCR1.LT.NUMARG.AND.IHARG(ILOCR2).EQ.'EXCE'.AND.   &
      IHARG2(ILOCR2).EQ.'PT  ')GO TO 9000
      IF(ILOCR1.LT.NUMARG.AND.IHARG(ILOCR2).EQ.'FOR '.AND.   &
      IHARG2(ILOCR2).EQ.'    ')GO TO 10000
      GO TO 7080
!
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
      GO TO 19000
!
!               ************************************************
!               **  STEP 8--                                  **
!               **  TREAT THE FULL VARIABLE CASE.             **
!               **  THEN JUMP TO STEP NUMBER 10 BELOW         **
!               **  FOR THE LIST UPDATING AND                 **
!               **  FOR SOME INFORMATIVE PRINTING.            **
!               ************************************************
!
!
 8000 CONTINUE
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')THEN
        ISTEPN='8'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,8011)NINEW,NIRIG1
 8011   FORMAT('NINEW,NIRIG1 = ',2I8)
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      ENDIF
!
      ICASEQ='FULL'
      NIOLD=NIRIG1
      IF(NUMVAR.GE.2.AND.NIRIG1.GT.NIOLD)NIOLD=NIRIG1
!     MORE HERE FOR NUMVAR = 3 ????? APRIL 1987
      NINEW=NIOLD
      DO 8100 I=1,NINEW
        ISUB(I)=1
 8100 CONTINUE
      GO TO 11000
!
!               ***********************************************
!               **  STEP 9--                                 **
!               **  TREAT THE PARTIAL VARIABLE SUBSET CASE.  **
!               **  JUMP TO STEP NUMBER 11 BELOW             **
!               **  FOR THE ACTUAL MATHEMATICAL OPERATION,   **
!               **  FOR THE LIST UPDATING, AND               **
!               **  FOR SOME INFORMATIVE PRINTING.           **
!               ***********************************************
!
 9000 CONTINUE
      ISTEPN='9'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICASEQ='SUBS'
      IF(NUMVAR.EQ.1)ILOCSV=ILOCR3
      IF(NUMVAR.EQ.2)ILOCSV=ILOCR4
      IF(NUMVAR.EQ.3)ILOCSV=ILOCR5
      IF(NUMVAR.EQ.4)ILOCSV=ILOCR6
      IHSET=IHARG(ILOCSV)
      IHSET2=IHARG2(ILOCSV)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IHSET,IHSET2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
      IF(IERROR.EQ.'YES')GO TO 19000
      NIOLD=IN(ILOC)
      CALL DPSUBS(NIOLD,ILOCS,NS,IBUGQ,IERROR)
!CCCC NINEW=NS
      NINEW=NIOLD
      GO TO 11000
!
!               **********************************************
!               **  STEP 10--                               **
!               **  TREAT THE PARTIAL VARIABLE FOR CASE.    **
!               **  JUMP TO STEP NUMBER 11 BELOW            **
!               **  FOR THE ACTUAL MATHEMATICAL OPERATION,  **
!               **  FOR THE LIST UPDATING, AND              **
!               **  FOR SOME INFORMATIVE PRINTING.          **
!               **********************************************
!
10000 CONTINUE
      ISTEPN='10'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICASEQ='FOR'
      CALL DPFOR(NIOLD,NINEW,IROW1,IROWN,   &
      NLOCAL,ILOCS,NS,IBUGQ,IERROR)
      NIFOR=NINEW
      GO TO 11000
!
!               *******************************************
!               **  STEP 11.1--                          **
!               **  FILL TEMPORARY VARIBLES.             **
!               **  EXTRACT AND EXAMINE THE FUNCTION.    **
!               **  CARRY OUT THE                        **
!               **  RUNGE-KUTTA CALCULATIONS,            **
!               **  THE LIST UPDATING, AND               **
!               **  GENERATE THE INFORMATIVE PRINTING    **
!               **  FOR STEP NUMBERS 7, 8, AND 9 ABOVE.  **
!               *******************************************
!
11000 CONTINUE
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NITEMX=NINEW
      NS1=0
      NS2=0
      NS3=0
      NS4=0
                                                                                                                                  
!CCCC IMAX=NINEW
!CCCC IF(ICASEQ.EQ.'FOR'.AND.IMAX.GT.NIFOR)IMAX=NIFOR
!CCCC DO11100I=1,IMAX
!
      DO 11100 I=1,NINEW
        IJ=MAXN*(ICOLR1-1)+I
!
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')THEN
          WRITE(ICOUT,11101)I,NS1,NINEW,ISUB(I),IJ,V(IJ)
11101     FORMAT('I,NS1,NINEW,ISUB(I),IJ,V(IJ) = ',5I8,F12.5)
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        ENDIF
!
        IF(ISUB(I).EQ.0)GO TO 11100
!
        IF(I.GT.NIRIG1)GO TO 11190
        NS1=NS1+1
        IJ=MAXN*(ICOLR1-1)+I
        IF(ICOLR1.LE.MAXCOL)TEMPX(NS1)=V(IJ)
        IF(ICOLR1.EQ.MAXCP1)TEMPX(NS1)=PRED(I)
        IF(ICOLR1.EQ.MAXCP2)TEMPX(NS1)=RES(I)
        IF(ICOLR1.EQ.MAXCP3)TEMPX(NS1)=YPLOT(I)
        IF(ICOLR1.EQ.MAXCP4)TEMPX(NS1)=XPLOT(I)
        IF(ICOLR1.EQ.MAXCP5)TEMPX(NS1)=X2PLOT(I)
        IF(ICOLR1.EQ.MAXCP6)TEMPX(NS1)=TAGPLO(I)
!
        IJ=MAXN*(ICOLL1-1)+I
        IF(ICOLL1.LE.MAXCOL)TEMPY(NS1)=V(IJ)
        IF(ICOLL1.EQ.MAXCP1)TEMPY(NS1)=PRED(I)
        IF(ICOLL1.EQ.MAXCP2)TEMPY(NS1)=RES(I)
        IF(ICOLL1.EQ.MAXCP3)TEMPY(NS1)=YPLOT(I)
        IF(ICOLL1.EQ.MAXCP4)TEMPY(NS1)=XPLOT(I)
        IF(ICOLL1.EQ.MAXCP5)TEMPY(NS1)=X2PLOT(I)
        IF(ICOLL1.EQ.MAXCP6)TEMPY(NS1)=TAGPLO(I)
!
        IF(NUMVAL.LE.1)GO TO 11100
        IJ=MAXN*(ICOLL2-1)+I
        IF(ICOLL2.LE.MAXCOL)TEMPYD(NS1)=V(IJ)
        IF(ICOLL2.EQ.MAXCP1)TEMPYD(NS1)=PRED(I)
        IF(ICOLL2.EQ.MAXCP2)TEMPYD(NS1)=RES(I)
        IF(ICOLL2.EQ.MAXCP3)TEMPYD(NS1)=YPLOT(I)
        IF(ICOLL2.EQ.MAXCP4)TEMPYD(NS1)=XPLOT(I)
        IF(ICOLL2.EQ.MAXCP5)TEMPYD(NS1)=X2PLOT(I)
        IF(ICOLL2.EQ.MAXCP6)TEMPYD(NS1)=TAGPLO(I)
!
11100 CONTINUE
11190 CONTINUE
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')THEN
        WRITE(ICOUT,11191)ICOLL1,ICOLL2,ICOLR1,NS1,NUMVAL
11191   FORMAT('ICOLL1,ICOLL2,ICOLR1,NS1,NUMVAL = ',5I8)
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,11192)NINEW,ICASL7,ICASEQ
11192   FORMAT('NINEW,ICASL7,ICASEQ = ',I8,2X,A4,2X,A4)
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      ENDIF
!
      IWRITE='ON'
      IF(IPRINT.EQ.'OFF')IWRITE='OFF'
      IF(IFEEDB.EQ.'OFF')IWRITE='OFF'
!
!               ***************************************************************
!               **  STEP 11.2--                                              **
!               **  EXTRACT THE RIGHT-SIDE FUNCTIONAL
!               **  EXPRESSION FROM THE INPUT COMMAND LINE                   **
!               **  (STARTING WITH THE FIRST NON-BLANK LOCATION AFTER THE    **
!               **  WORD    KUTTA     AND ENDING WITH THE NEXT TO THE LAST   **
!               **  WORD.                                                    **
!               **  PLACE THE FUNCTION IN IFUNC2(.)  .                       **
!               ***************************************************************
!
      ISTEPN='11.2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.GT.5)THEN
!
        IWD1=IHARG(4)
        IWD12=IHARG2(4)
        IF(NUMVAL.GE.2)IWD1=IHARG(5)
        IF(NUMVAL.GE.2)IWD12=IHARG2(5)
        IWD2=IHARG(ILOCR1)
        IWD22=IHARG2(ILOCR1)
        CALL DPEXST(IANS,IWIDTH,IWD1,IWD12,IWD2,IWD22,MAXN2,   &
                    IFUNC2,N2,IBUGA3,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GO TO 19000
        IF(IFOUND.EQ.'YES')GO TO 11290
      ENDIF
!
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
      GO TO 19000
!
11290 CONTINUE
!
!               ***********************************************************
!               **  STEP 11.3--                                          **
!               **  DETERMINE IF THE EXPRESSION HAS ANY FUNCTION NAMES   **
!               **  INBEDDED.  IF SO, REPLACE THE FUNCTION NAMES         **
!               **  BY EACH FUNCTION'S DEFINITION.  DO SO REPEATEDLY     **
!               **  UNTIL ALL FUNCTION REFERENCES HAVE BEEN ANNIHILATED  **
!               **  AND THE EXPRESSION IS LEFT ONLY WITH                 **
!               **  CONSTANTS, PARAMETERS, AND VARIABLES--NO FUNCTIONS.  **
!               **  PLACE THE RESULTING FUNCTIONAL EXPRESSION INTO IFUNC3(.) **
!               ***********************************************************
!
      ISTEPN='11.3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IFLGFB.LE.0)THEN
        CALL DPEXFU(IFUNC2,N2,IHNAME,IHNAM2,IUSE,IVSTAR,IVSTOP,   &
                    NUMNAM,IANS,IWIDTH,IFUNC,NUMCHF,MAXCHF,IFUNC3,   &
                    N3,MAXN3,   &
                    IBUGA3,IERROR)
        IF(IERROR.EQ.'YES')GO TO 19000
!
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
!
          WRITE(ICOUT,11311)IDUMV(1),IDUMV2(1)
11311     FORMAT('DIFFERENTIATION  = ',A4,A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
      ENDIF
!
!               ************************************************************
!               **  STEP 11.4--                                           **
!               **  DETERMINE THE DUMMY VARIABLE IN THE DIFFERENTIATION.  **
!               ************************************************************
!
      ISTEPN='11.4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
!
!               **********************************************************
!               **  STEP 11.5--                                         **
!               **  MAKE A NON-CALCULATING PASS AT THE FUNCTION         **
!               **  SO AS TO EXTRACT ALL PARAMETER AND VARIABLE NAMES.  **
!               **********************************************************
!
      ISTEPN='11.5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IFLGFB.LE.0)THEN
        IPASS=1
        CALL COMPIM(IFUNC3,N3,IPASS,PARAM,IPARN,IPARN2,NUMPV,   &
                    IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,AJUNK,   &
                    IBUGCO,IBUGEV,IERROR)
        IF(IERROR.EQ.'YES')GO TO 19000
      ELSE
        GO TO 11901
      ENDIF
!
!               ********************************************
!               **  STEP 11.6--                           **
!               **  TREAT THE SPECIAL CASE WHERE          **
!               **  THE HORIZONTAL AXIS AND/OR THE        **
!               **  VERTICAL AXIS VARIABLE DOES NOT       **
!               **  EXPLICITLY APPEAR IN THE FUNCTON;     **
!               **  IN SUCH CASE, AUGMENT THE PARAMETER   **
!               **  LIST WITH THE 1 (OR 2) VARIABLES.     **
!               ********************************************
!
      ISTEPN='11.6'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMPV.GE.1)THEN
        DO 11610 I=1,NUMPV
          I2=I
          IF(IHRI11.EQ.IPARN(I).AND.IHRI12.EQ.IPARN2(I))GO TO 11619
11610   CONTINUE
      ENDIF
      NUMPV=NUMPV+1
      IPARN(NUMPV)=IHRI11
      IPARN2(NUMPV)=IHRI12
11619 CONTINUE
!
      IF(NUMPV.GT.0)THEN
        DO 11620 I=1,NUMPV
          I2=I
          IF(ILEF11.EQ.IPARN(I).AND.ILEF12.EQ.IPARN2(I))GO TO 11629
11620   CONTINUE
      ENDIF
      NUMPV=NUMPV+1
      IPARN(NUMPV)=ILEF11
      IPARN2(NUMPV)=ILEF12
11629 CONTINUE
!
      IF(NUMVAL.LE.1)GO TO 11639
      IF(NUMPV.GE.1)THEN
        DO 11630 I=1,NUMPV
          I2=I
          IF(ILEF21.EQ.IPARN(I).AND.ILEF22.EQ.IPARN2(I))GO TO 11639
11630   CONTINUE
      ENDIF
      NUMPV=NUMPV+1
      IPARN(NUMPV)=ILEF21
      IPARN2(NUMPV)=ILEF22
11639 CONTINUE
!
!               ***********************************************
!               **  STEP 11.7--                              **
!               **  CHECK THAT ALL PARAMETERS                **
!               **  IN THE FUNCTION ARE ALREADY PRESENT      **
!               **  IN THE AVAILABLE NAME LIST IHNAME(.).    **
!               ***********************************************
!
      ISTEPN='11.7'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IP=0
      IV=0
      IF(NUMPV.GE.1)THEN
        DO 11700 J=1,NUMPV
          IHPARN=IPARN(J)
          IHPAR2=IPARN2(J)
          IF(IHPARN.EQ.IDUMV(3).AND.IHPAR2.EQ.IDUMV2(3))GO TO 11730
          IF(IHPARN.EQ.IDUMV(2).AND.IHPAR2.EQ.IDUMV2(2))GO TO 11730
          IF(IHPARN.EQ.IDUMV(1).AND.IHPAR2.EQ.IDUMV2(1))GO TO 11730
          IHWUSE='P'
          MESSAG='YES'
          CALL CHECKN(IHPARN,IHPAR2,IHWUSE,   &
                      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
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
            GO TO 19000
          ENDIF
!
          IP=IP+1
          PARAM(J)=VALUE(ILOCP)
          GO TO 11700
!
11730     CONTINUE
          IV=IV+1
          LOCDUM=J
11700   CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 11.8--                           **
!               **  DETERMINE WHERE IN THE PARAM(.) LIST  **
!               **  THE HORIZ. AXIS VARIABLE LAY,         **
!               **  THE VERT. AXIS VARIABLE LAY,          **
!               **  AND (IF AN ORDER 2 EQUATION) WHERE    **
!               **  THE DERIVATIVE AXIS VARIABLE LAY.     **
!               ********************************************
!
      ISTEPN='11.8'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ILOCHV=(-999)
      IF(NUMPV.GE.1)THEN
        DO 11810 I=1,NUMPV
          I2=I
          IF(IHRI11.EQ.IPARN(I).AND.IHRI12.EQ.IPARN2(I))GO TO 11815
11810   CONTINUE
      ENDIF
!
      WRITE(ICOUT,211)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,11813)IHRI11,IHRI12
11813 FORMAT('       ',A4,A4,' NOT FOUND IN COMPIM PARAMETER LIST')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 19000
!
11815 CONTINUE
      ILOCHV=I2
!
      ILOCVV=(-999)
      IF(NUMPV.GE.1)THEN
        DO 11820 I=1,NUMPV
          I2=I
          IF(ILEF11.EQ.IPARN(I).AND.ILEF12.EQ.IPARN2(I))GO TO 11825
11820   CONTINUE
      ENDIF
!
      WRITE(ICOUT,211)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,11823)ILEF11,ILEF12
11823 FORMAT('       ',A4,A4,' NOT FOUND IN COMPIM PARAMETER LIST')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 19000
!
11825 CONTINUE
      ILOCVV=I2
!
      ILOCDV=(-999)
      IF(NUMVAL.LE.1)GO TO 11839
      IF(NUMPV.GE.1)THEN
        DO 11830 I=1,NUMPV
          I2=I
          IF(ILEF21.EQ.IPARN(I).AND.ILEF22.EQ.IPARN2(I))THEN
            ILOCDV=I2
            GO TO 11839
          ENDIF
11830   CONTINUE
      ENDIF
!
      WRITE(ICOUT,211)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,11833)ILEF21,ILEF22
11833 FORMAT('       ',A4,A4,' NOT FOUND IN COMPIM PARAMETER LIST')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 19000
!
11839 CONTINUE
!
!               ******************************************
!               **  STEP 11.9--                         **
!               **  COMPUTE THE RUNGE-KUTTA SOLUTION    **
!               ******************************************
!
11901 CONTINUE
!
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
        DO 11914 I=1,NUMDV
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
!
      IWRITE='ON'
      IF(IPRINT.EQ.'OFF')IWRITE='OFF'
      IF(IFEEDB.EQ.'OFF')IWRITE='OFF'
!
      CALL DPRK2(TEMPX,TEMPY,TEMPYD,NS1,ILOCHV,ILOCVV,ILOCDV,NUMVAL,   &
                 IFUNC3,N3,PARAM,IPARN,IPARN2,NUMPV,   &
                 IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,   &
                 IDUMV,IDUMV2,NUMDV,   &
                 IBUGA3,IBUGCO,IBUGEV,ISUBRO,IERROR)
!
      IFOUND='YES'
!
!               *******************************************
!               **  STEP 12--                            **
!               **  COPY THE OUTPUT VARIABLE             **
!               **  TO THE PROPER WORKSHEET COLUMN       **
!               *******************************************
!
      IF(ICASEQ.EQ.'FULL')GO TO 12100
      IF(ICASEQ.EQ.'SUBS')GO TO 12300
      IF(ICASEQ.EQ.'FOR')GO TO 12500
!
!               *******************************************
!               **  STEP 12.1--                          **
!               **  TREAT THE FULL CASE.                 **
!               *******************************************
!
12100 CONTINUE
      ISTEPN='12.1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      NSX=0
      IF(NITEMX.LE.0)IROW1=0
      IF(NITEMX.LE.0)IROWN=0
      IF(NITEMX.LE.0)GO TO 12190
      DO 12110 I=1,NITEMX
      NSX=I
!
      IJ=MAXN*(ICOLL1-1)+I
      IF(ICOLL1.LE.MAXCOL)V(IJ)=TEMPY(NSX)
      IF(ICOLL1.EQ.MAXCP1)PRED(I)=TEMPY(NSX)
      IF(ICOLL1.EQ.MAXCP2)RES(I)=TEMPY(NSX)
      IF(ICOLL1.EQ.MAXCP3)YPLOT(I)=TEMPY(NSX)
      IF(ICOLL1.EQ.MAXCP4)XPLOT(I)=TEMPY(NSX)
      IF(ICOLL1.EQ.MAXCP5)X2PLOT(I)=TEMPY(NSX)
      IF(ICOLL1.EQ.MAXCP6)TAGPLO(I)=TEMPY(NSX)
!
      IF(NUMVAL.LE.1)GO TO 12110
      IJ=MAXN*(ICOLL2-1)+I
      IF(ICOLL2.LE.MAXCOL)V(IJ)=TEMPYD(NSX)
      IF(ICOLL2.EQ.MAXCP1)PRED(I)=TEMPYD(NSX)
      IF(ICOLL2.EQ.MAXCP2)RES(I)=TEMPYD(NSX)
      IF(ICOLL2.EQ.MAXCP3)YPLOT(I)=TEMPYD(NSX)
      IF(ICOLL2.EQ.MAXCP4)XPLOT(I)=TEMPYD(NSX)
      IF(ICOLL2.EQ.MAXCP5)X2PLOT(I)=TEMPYD(NSX)
      IF(ICOLL2.EQ.MAXCP6)TAGPLO(I)=TEMPYD(NSX)
!
12110 CONTINUE
12190 CONTINUE
!
      IF(NITEMX.GE.1)IROW1=1
      IF(NITEMX.GE.1)IROWN=NITEMX
      IN(ILISL1)=NITEMX
!CCCC IN(ICOLL1)=NITEMX
      IF(NUMVAL.EQ.2)IN(ILISL2)=NITEMX
!
      DO 12210 J4=1,NUMNAM
      IF(IUSE(J4).EQ.'V'.AND.IVALUE(J4).EQ.ICOLL1)GO TO 12215
      GO TO 12210
12215 CONTINUE
      IUSE(J4)='V'
      IVALUE(J4)=ICOLL1
      VALUE(J4)=ICOLL1
      IN(J4)=NITEMX
12210 CONTINUE
!
      IF(NUMVAL.LE.1)GO TO 12229
      DO 12220 J4=1,NUMNAM
      IF(IUSE(J4).EQ.'V'.AND.IVALUE(J4).EQ.ICOLL2)GO TO 12225
      GO TO 12220
12225 CONTINUE
      IUSE(J4)='V'
      IVALUE(J4)=ICOLL2
      VALUE(J4)=ICOLL2
      IN(J4)=NITEMX
12220 CONTINUE
12229 CONTINUE
!
      GO TO 13000
!
!               *******************************************
!               **  STEP 12.2--                          **
!               **  TREAT THE SUBSET CASE.               **
!               *******************************************
!
12300 CONTINUE
      ISTEPN='12.2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NSX=0
      IF(NITEMX.LE.0)IROW1=0
      IF(NITEMX.LE.0)IROWN=0
      IF(NITEMX.LE.0)GO TO 12390
      DO 12310 I=1,NITEMX
      IF(ISUB(I).EQ.0)GO TO 12310
      NSX=NSX+1
!
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
!
      IF(NUMVAL.LE.1)GO TO 12310
      IJ=MAXN*(ICOLL2-1)+I
      IF(ICOLL2.LE.MAXCOL)V(IJ)=TEMPYD(NSX)
      IF(ICOLL2.EQ.MAXCP1)PRED(I)=TEMPYD(NSX)
      IF(ICOLL2.EQ.MAXCP2)RES(I)=TEMPYD(NSX)
      IF(ICOLL2.EQ.MAXCP3)YPLOT(I)=TEMPYD(NSX)
      IF(ICOLL2.EQ.MAXCP4)XPLOT(I)=TEMPYD(NSX)
      IF(ICOLL2.EQ.MAXCP5)X2PLOT(I)=TEMPYD(NSX)
      IF(ICOLL2.EQ.MAXCP6)TAGPLO(I)=TEMPYD(NSX)
!
12310 CONTINUE
12390 CONTINUE
!
      IN(ILISL1)=NITEMX
!CCCC IN(ICOLL1)=NITEMX
      IF(NUMVAL.EQ.2)IN(ILISL2)=NITEMX
!
      DO 12410 J4=1,NUMNAM
      IF(IUSE(J4).EQ.'V'.AND.IVALUE(J4).EQ.ICOLL1)GO TO 12415
      GO TO 12410
12415 CONTINUE
      IUSE(J4)='V'
      IVALUE(J4)=ICOLL1
      VALUE(J4)=ICOLL1
      IN(J4)=NITEMX
12410 CONTINUE
!
      IF(NUMVAL.LE.1)GO TO 12429
      DO 12420 J4=1,NUMNAM
      IF(IUSE(J4).EQ.'V'.AND.IVALUE(J4).EQ.ICOLL2)GO TO 12425
      GO TO 12420
12425 CONTINUE
      IUSE(J4)='V'
      IVALUE(J4)=ICOLL2
      VALUE(J4)=ICOLL2
      IN(J4)=NITEMX
12420 CONTINUE
12429 CONTINUE
!
      GO TO 13000
!
!               *******************************************
!               **  STEP 12.3--                          **
!               **  TREAT THE FOR CASE.                  **
!               *******************************************
!
12500 CONTINUE
      ISTEPN='12.3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      NSX=0
      IF(NITEMX.LE.0)IROW1=0
      IF(NITEMX.LE.0)IROWN=0
      IF(NITEMX.LE.0)GO TO 12590
      DO 12510 I=1,NITEMX
      IF(I.GT.NIFOR)GO TO 12550
      IF(ISUB(I).EQ.0)GO TO 12510
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
!
      IF(NUMVAL.LE.1)GO TO 12510
      IJ=MAXN*(ICOLL2-1)+I
      IF(ICOLL2.LE.MAXCOL)V(IJ)=TEMPYD(NSX)
      IF(ICOLL2.EQ.MAXCP1)PRED(I)=TEMPYD(NSX)
      IF(ICOLL2.EQ.MAXCP2)RES(I)=TEMPYD(NSX)
      IF(ICOLL2.EQ.MAXCP3)YPLOT(I)=TEMPYD(NSX)
      IF(ICOLL2.EQ.MAXCP4)XPLOT(I)=TEMPYD(NSX)
      IF(ICOLL2.EQ.MAXCP5)X2PLOT(I)=TEMPYD(NSX)
      IF(ICOLL2.EQ.MAXCP6)TAGPLO(I)=TEMPYD(NSX)
!
12510 CONTINUE
12590 CONTINUE
12550 CONTINUE
!
      IN(ILISL1)=NITEMX
!CCCC IN(ICOLL1)=NITEMX
      IF(NUMVAL.EQ.2)IN(ILISL2)=NITEMX
!
      DO 12610 J4=1,NUMNAM
      IF(IUSE(J4).EQ.'V'.AND.IVALUE(J4).EQ.ICOLL1)GO TO 12615
      GO TO 12610
12615 CONTINUE
      IUSE(J4)='V'
      IVALUE(J4)=ICOLL1
      VALUE(J4)=ICOLL1
      IN(J4)=NITEMX
12610 CONTINUE
!
      IF(NUMVAL.LE.1)GO TO 12629
      DO 12620 J4=1,NUMNAM
      IF(IUSE(J4).EQ.'V'.AND.IVALUE(J4).EQ.ICOLL2)GO TO 12625
      GO TO 12620
12625 CONTINUE
      IUSE(J4)='V'
      IVALUE(J4)=ICOLL2
      VALUE(J4)=ICOLL2
      IN(J4)=NITEMX
12620 CONTINUE
12629 CONTINUE
!
      GO TO 13000
!
!               *******************************************
!               **  STEP 13--                            **
!               **  CARRY OUT THE LIST UPDATING AND      **
!               **  GENERATE THE INFORMATIVE PRINTING    **
!               **  FOR STEP NUMBERS 7, 8, AND 9 ABOVE.  **
!               *******************************************
!
13000 CONTINUE
      ISTEPN='12'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DPRK')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IHNAME(ILISL1)=ILEF11
      IHNAM2(ILISL1)=ILEF12
      IUSE(ILISL1)='V'
      IVALUE(ILISL1)=ICOLL1
      VALUE(ILISL1)=ICOLL1
!CCCC IUSE(ICOLL1)='V'
!CCCC IVALUE(ICOLL1)=ICOLL1
!CCCC VALUE(ICOLL1)=ICOLL1
      IF(NEWNA1.EQ.'YES')NUMNAM=NUMNAM+1
      IF(NEWNA1.EQ.'YES')NUMCOL=NUMCOL+1
!
      IF(NUMVAL.LE.1)GO TO 13009
      IHNAME(ILISL2)=ILEF21
      IHNAM2(ILISL2)=ILEF22
      IUSE(ILISL2)='V'
      IVALUE(ILISL2)=ICOLL2
      VALUE(ILISL2)=ICOLL2
!CCCC IUSE(ICOLL2)='V'
!CCCC IVALUE(ICOLL2)=ICOLL2
!CCCC VALUE(ICOLL2)=ICOLL2
      IF(NEWNA2.EQ.'YES')NUMNAM=NUMNAM+1
      IF(NEWNA2.EQ.'YES')NUMCOL=NUMCOL+1
13009 CONTINUE
!
      IF(IPRINT.EQ.'OFF')GO TO 13090
      IF(IFEEDB.EQ.'OFF')GO TO 13090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
!
      WRITE(ICOUT,13011)ILEF11,ILEF12,NSX
13011 FORMAT('THE NUMBER OF VALUES GENERATED FOR ',   &
      'THE VARIABLE ',A4,A4,' = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
!
      IJ=MAXN*(ICOLL1-1)+IROW1
      IF(ICOLL1.LE.MAXCOL)WRITE(ICOUT,13021)ILEF11,ILEF12,V(IJ),IROW1
      IF(ICOLL1.LE.MAXCOL)CALL DPWRST('XXX','BUG ')
!CCCC THE FOLLOWING 4 LINES WERE FIXED   APRIL 1992
!CCCC IF(ICOLL1.EQ.MAXCP1)WRITE(ICOUT,13021)ILEF11,ILEF12,PRED(IROW1),IROW1
!CCCC IF(ICOLL1.EQ.MAXCP1)CALL DPWRST('XXX','BUG ')
!CCCC IF(ICOLL1.EQ.MAXCP2)WRITE(ICOUT,13021)ILEF11,ILEF12,RES(IROW1),IROW1
!CCCC IF(ICOLL1.EQ.MAXCP2)CALL DPWRST('XXX','BUG ')
!CCCC IF(ICOLL1.EQ.MAXCP3)WRITE(ICOUT,13021)ILEF11,ILEF12,YPLOT(IROW1),IROW1
!CCCC IF(ICOLL1.EQ.MAXCP3)CALL DPWRST('XXX','BUG ')
!CCCC IF(ICOLL1.EQ.MAXCP4)WRITE(ICOUT,13021)ILEF11,ILEF12,XPLOT(IROW1),IROW1
!CCCC IF(ICOLL1.EQ.MAXCP4)CALL DPWRST('XXX','BUG ')
      IF(ICOLL1.EQ.MAXCP1)WRITE(ICOUT,13021)ILEF11,ILEF12,PRED(IROW1),   &
      IROW1
13021 FORMAT('THE FIRST           COMPUTED VALUE OF ',A4,A4,   &
      ' = ',E15.7,'   (ROW ',I6,')')
      IF(ICOLL1.EQ.MAXCP1)CALL DPWRST('XXX','BUG ')
      IF(ICOLL1.EQ.MAXCP2)WRITE(ICOUT,13021)ILEF11,ILEF12,RES(IROW1),   &
      IROW1
      IF(ICOLL1.EQ.MAXCP2)CALL DPWRST('XXX','BUG ')
      IF(ICOLL1.EQ.MAXCP3)WRITE(ICOUT,13021)ILEF11,ILEF12,YPLOT(IROW1),   &
      IROW1
      IF(ICOLL1.EQ.MAXCP3)CALL DPWRST('XXX','BUG ')
      IF(ICOLL1.EQ.MAXCP4)WRITE(ICOUT,13021)ILEF11,ILEF12,XPLOT(IROW1),   &
      IROW1
      IF(ICOLL1.EQ.MAXCP4)CALL DPWRST('XXX','BUG ')
      IF(ICOLL1.EQ.MAXCP5)WRITE(ICOUT,13021)ILEF11,ILEF12,X2PLOT(IROW1),   &
      IROW1
      IF(ICOLL1.EQ.MAXCP5)CALL DPWRST('XXX','BUG ')
      IF(ICOLL1.EQ.MAXCP6)WRITE(ICOUT,13021)ILEF11,ILEF12,TAGPLO(IROW1),   &
      IROW1
      IF(ICOLL1.EQ.MAXCP6)CALL DPWRST('XXX','BUG ')
!
      IJ=MAXN*(ICOLL1-1)+IROWN
      IF(ICOLL1.LE.MAXCOL.AND.   &
      NSX.NE.1)WRITE(ICOUT,13031)NSX,ILEF11,ILEF12,V(IJ),IROWN
13031 FORMAT('THE LAST (',I5,'-TH) COMPUTED VALUE OF ',A4,A4,   &
      ' = ',E15.7,'   (ROW ',I6,')')
      IF(ICOLL1.LE.MAXCOL.AND.   &
      NSX.NE.1)CALL DPWRST('XXX','BUG ')
      IF(ICOLL1.EQ.MAXCP1.AND.   &
      NSX.NE.1)WRITE(ICOUT,13031)NSX,ILEF11,ILEF12,PRED(IROWN),IROWN
      IF(ICOLL1.EQ.MAXCP1.AND.   &
      NSX.NE.1)CALL DPWRST('XXX','BUG ')
      IF(ICOLL1.EQ.MAXCP2.AND.   &
      NSX.NE.1)WRITE(ICOUT,13031)NSX,ILEF11,ILEF12,RES(IROWN),IROWN
      IF(ICOLL1.EQ.MAXCP2.AND.   &
      NSX.NE.1)CALL DPWRST('XXX','BUG ')
      IF(ICOLL1.EQ.MAXCP3.AND.   &
      NSX.NE.1)WRITE(ICOUT,13031)NSX,ILEF11,ILEF12,YPLOT(IROWN),IROWN
      IF(ICOLL1.EQ.MAXCP3.AND.   &
      NSX.NE.1)CALL DPWRST('XXX','BUG ')
      IF(ICOLL1.EQ.MAXCP4.AND.   &
      NSX.NE.1)WRITE(ICOUT,13031)NSX,ILEF11,ILEF12,XPLOT(IROWN),IROWN
      IF(ICOLL1.EQ.MAXCP4.AND.   &
      NSX.NE.1)CALL DPWRST('XXX','BUG ')
      IF(ICOLL1.EQ.MAXCP5.AND.   &
      NSX.NE.1)WRITE(ICOUT,13031)NSX,ILEF11,ILEF12,X2PLOT(IROWN),IROWN
      IF(ICOLL1.EQ.MAXCP5.AND.   &
      NSX.NE.1)CALL DPWRST('XXX','BUG ')
      IF(ICOLL1.EQ.MAXCP6.AND.   &
      NSX.NE.1)WRITE(ICOUT,13031)NSX,ILEF11,ILEF12,TAGPLO(IROWN),IROWN
      IF(ICOLL1.EQ.MAXCP6.AND.   &
      NSX.NE.1)CALL DPWRST('XXX','BUG ')
      IF(NSX.NE.1)GO TO 13039
      WRITE(ICOUT,13032)
13032 FORMAT('SINCE THE GENERATED SAMPLE SIZE WAS ONLY 1,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,13033)
13033 FORMAT('THE ABOVE VALUE WAS THE SOLE VALUE COMPUTED.')
      CALL DPWRST('XXX','BUG ')
13039 CONTINUE
!
      IF(NUMVAL.LE.1)GO TO 13079
      WRITE(ICOUT,13051)ILEF21,ILEF22,NSX
13051 FORMAT('THE NUMBER OF VALUES GENERATED FOR ',   &
      'THE VARIABLE ',A4,A4,' = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
!
      IJ=MAXN*(ICOLL2-1)+IROW1
      IF(ICOLL2.LE.MAXCOL)THEN
         WRITE(ICOUT,13061)ILEF21,ILEF22,V(IJ),IROW1
13061    FORMAT('THE FIRST           COMPUTED VALUE OF ',   &
         A4,A4,' = ',E15.7,'   (ROW ',I6,')')
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
!
      IJ=MAXN*(ICOLL2-1)+IROWN
      IF(NSX.NE.1)THEN
         IF(ICOLL2.LE.MAXCOL)THEN
            WRITE(ICOUT,13071)NSX,ILEF21,ILEF22,V(IJ),IROWN
13071       FORMAT('THE LAST (',I5,'-TH) COMPUTED VALUE OF ',   &
            A4,A4,' = ',E15.7,'   (ROW ',I6,')')
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
      IF(NSX.NE.1)GO TO 13079
      WRITE(ICOUT,13072)
13072 FORMAT('SINCE THE GENERATED SAMPLE SIZE WAS ONLY 1,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,13073)
13073 FORMAT('THE ABOVE VALUE WAS THE SOLE VALUE COMPUTED.')
      CALL DPWRST('XXX','BUG ')
13079 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
13090 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPRK
      SUBROUTINE DPRK2(X,Y,YD,N,ILOCHV,ILOCVV,ILOCDV,IORDER,   &
                       MODEL,NUMCHA,PARAM,IPARN,IPARN2,NUMPV,   &
                       IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,   &
                       IVARN,IVARN2,NUMVAR,   &
                       IBUGA3,IBUGCO,IBUGEV,ISUBRO,IERROR)
!
!*****COPIED OVER ON TUESDAY NIGHT OCT 13, 1987 AFTER DISAPPEARANCE OF DPRK.FOR
!     AND STRANGE PRIVILEDGE MESSAGES ABOUT DPRK2.FOR
                                                                                                                                  
!     PURPOSE--COMPUTE THE RUNGE-KUTA SOLUTIONS
!              OF A FIRST- OR SECOND-ORDER DIFFERENTIAL EQUATION
!              OVER THE RANGE OF VALUES OF THE VARIABLE X.
!     NOTE--FOR FIRST-ORDER EQUATIONS,
!           X(1) AND Y(1) ARE THE INITIAL VALUES
!           FOR THE DIFFERENTIAL EQUATION--
!           THEY MUST PRE-EXIST.
!         --FOR SECOND-ORDER EQUATIONS,
!           X(1), Y(1), AND YD(1) ARE THE INITIAL VALUES
!           FOR THE DIFFERENTIAL EQUATION--
!           THEY MUST PRE-EXIST.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--87/10
!     ORIGINAL VERSION--SEPTEMBER  1987.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
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
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
      DIMENSION Y(*)
      DIMENSION YD(*)
!
      DIMENSION MODEL(*)
      DIMENSION PARAM(*)
      DIMENSION IPARN(*)
      DIMENSION IPARN2(*)
      DIMENSION IVARN(*)
      DIMENSION IVARN2(*)
!
      DIMENSION ITYPEH(*)
      DIMENSION IW21HO(*)
      DIMENSION IW22HO(*)
      DIMENSION W2HOLD(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPRK'
      ISUBN2='2   '
!
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
        DO 57 I=1,N
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
        DO 65 I=1,NUMPV
          WRITE(ICOUT,66)I,PARAM(I),IPARN(I),IPARN2(I)
   66     FORMAT('I,PARAM(I),IPARN(I),IPARN2(I) = ',I8,G15.7,A4,A4)
          CALL DPWRST('XXX','BUG ')
   65   CONTINUE
        DO 70 I=1,NUMVAR
          WRITE(ICOUT,71)I,IVARN(I),IVARN2(I)
   71     FORMAT('I, IVARN(I) = ',I8,2X,A4,A4)
          CALL DPWRST('XXX','BUG ')
   70   CONTINUE
      ENDIF
!
!               **************************************************
!               **  STEP 1--                                    **
!               **  BRANCH TO THE PROPER CASE                   **
!               **************************************************
!
      IF(IORDER.EQ.1)GO TO 1100
      GO TO 2100
!
!               **************************************************
!               **  STEP 11--                                   **
!               **  FOR THIS FIRST ORDER CASE,                  **
!               **  WRITE OUT  PRELIMINARY SUMMARY INFORMATION  **
!               **************************************************
!
 1100 CONTINUE
      IF(IPRINT.EQ.'OFF')GO TO 1109
      IF(IFEEDB.EQ.'OFF')GO TO 1109
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
!
!               ***********************************************
!               **  STEP 12--                                **
!               **  FOR THIS FIRST ORDER CASE,               **
!               **  STEP THROUGH THE   VALUES OF THE         **
!               **  HORIZONTAL AXIS VARIABLE                 **
!               **  (THE VARIABLE OF DIFFERENTIATION)        **
!               **  AND COMPUTE THE RUNGE-KUTTA SOLUTIONS    **
!               ***********************************************
!
      DO 1200 I=2,N
!
      IM1=I-1
      X0=X(IM1)
      Y0=Y(IM1)
      H=X(I)-X(IM1)
!
!     STEP 11.1--
!
      XARG=X0
      YARG=Y0
!CCCC CALL FUNC(XARG,YARG,FOUT)
!CCCC AK1=H*FOUT
      PARAM(ILOCHV)=XARG
      PARAM(ILOCVV)=YARG
      CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM,IPARN,IPARN2,NUMPV,   &
      IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,FOUT,   &
      IBUGCO,IBUGEV,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      AK1=H*FOUT
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')   &
      WRITE(ICOUT,1211)PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AK1
 1211 FORMAT('PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AK1 = ',4E15.7)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')   &
      CALL DPWRST('XXX','BUG ')
!
!     STEP 11.2--
!
      XARG=X0+H/2.0
      YARG=Y0+AK1/2.0
!CCCC CALL FUNC(XARG,YARG,FOUT)
!CCCC AK2=H*FOUT
      PARAM(ILOCHV)=XARG
      PARAM(ILOCVV)=YARG
      CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM,IPARN,IPARN2,NUMPV,   &
      IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,FOUT,   &
      IBUGCO,IBUGEV,IERROR)
      AK2=H*FOUT
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')   &
      WRITE(ICOUT,1212)PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AK2
 1212 FORMAT('PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AK2 = ',4E15.7)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')   &
      CALL DPWRST('XXX','BUG ')
!
!     STEP 11.3--
!
      XARG=X0+H/2.0
      YARG=Y0+AK2/2.0
!CCCC CALL FUNC(XARG,YARG,FOUT)
!CCCC AK3=H*FOUT
      PARAM(ILOCHV)=XARG
      PARAM(ILOCVV)=YARG
      CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM,IPARN,IPARN2,NUMPV,   &
      IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,FOUT,   &
      IBUGCO,IBUGEV,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      AK3=H*FOUT
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')   &
      WRITE(ICOUT,1213)PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AK3
 1213 FORMAT('PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AK3 = ',4E15.7)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')   &
      CALL DPWRST('XXX','BUG ')
!
!     STEP 11.4--
!
      XARG=X0+H
      YARG=Y0+AK3
!CCCC CALL FUNC(XARG,YARG,FOUT)
!CCCC AK4=H*FOUT
      PARAM(ILOCHV)=XARG
      PARAM(ILOCVV)=YARG
      CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM,IPARN,IPARN2,NUMPV,   &
      IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,FOUT,   &
      IBUGCO,IBUGEV,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      AK4=H*FOUT
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')   &
      WRITE(ICOUT,1214)PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AK4
 1214 FORMAT('PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AK4 = ',4E15.7)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')   &
      CALL DPWRST('XXX','BUG ')
!
      YNEXT=Y0+(AK1/6.0)+(AK2/3.0)+(AK3/3.0)+(AK4/6.0)
!
      Y(I)=YNEXT
!
 1200 CONTINUE
!
      GO TO 9000
!
!               **************************************************
!               **  STEP 21--                                   **
!               **  FOR THIS SECOND ORDER CASE,                 **
!               **  WRITE OUT  PRELIMINARY SUMMARY INFORMATION  **
!               **************************************************
!
 2100 CONTINUE
      IF(IPRINT.EQ.'OFF')GO TO 2109
      IF(IFEEDB.EQ.'OFF')GO TO 2109
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
!
!               ***********************************************
!               **  STEP 22--                                **
!               **  FOR THIS SECOND ORDER CASE,              **
!               **  STEP THROUGH THE   VALUES OF THE         **
!               **  HORIZONTAL AXIS VARIABLE                 **
!               **  (THE VARIABLE OF DIFFERENTIATION)        **
!               **  AND COMPUTE THE RUNGE-KUTTA SOLUTIONS    **
!               ***********************************************
!
      DO 2200 I=2,N
!
      IM1=I-1
      X0=X(IM1)
      Y0=Y(IM1)
      YD0=YD(IM1)
      H=X(I)-X(IM1)
!
!     STEP 22.1--
!
      XARG=X0
      YARG=Y0
      YDARG=YD0
!CCCC FOUT=YDARG
!CCCC AK1=H*FOUT
      FOUT=YDARG
      AK1=H*FOUT
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')   &
      WRITE(ICOUT,2211)PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AK1
 2211 FORMAT('PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AK1 = ',4E15.7)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')   &
      CALL DPWRST('XXX','BUG ')
!
!     STEP 22.2--
!
      XARG=X0
      YARG=Y0
      YDARG=YD0
!CCCC CALL FUNC(XARG,YARG,YDARG,FOUT)
!CCCC AK1=H*FOUT
      PARAM(ILOCHV)=XARG
      PARAM(ILOCVV)=YARG
      PARAM(ILOCDV)=YDARG
      CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM,IPARN,IPARN2,NUMPV,   &
      IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,FOUT,   &
      IBUGCO,IBUGEV,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      AL1=H*FOUT
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')   &
      WRITE(ICOUT,2212)PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AL1
 2212 FORMAT('PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AL1 = ',4E15.7)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')   &
      CALL DPWRST('XXX','BUG ')
!
!     STEP 22.3--
!
      XARG=X0+H/2.0
      YARG=Y0+AK1/2.0
      YDARG=YD0+AL1/2.0
!CCCC FOUT=YDARG
!CCCC AK2=H*FOUT
      FOUT=YDARG
      AK2=H*FOUT
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')   &
      WRITE(ICOUT,2213)PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AK2
 2213 FORMAT('PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AK2 = ',4E15.7)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')   &
      CALL DPWRST('XXX','BUG ')
!
!     STEP 22.4--
!
      XARG=X0+H/2.0
      YARG=Y0+AK1/2.0
      YDARG=YD0+AL1/2.0
!CCCC CALL FUNC(XARG,YARG,YDARG,FOUT)
!CCCC AK2=H*FOUT
      PARAM(ILOCHV)=XARG
      PARAM(ILOCVV)=YARG
      PARAM(ILOCDV)=YDARG
      CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM,IPARN,IPARN2,NUMPV,   &
      IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,FOUT,   &
      IBUGCO,IBUGEV,IERROR)
      AL2=H*FOUT
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')   &
      WRITE(ICOUT,2214)PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AL2
 2214 FORMAT('PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AL2 = ',4E15.7)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')   &
      CALL DPWRST('XXX','BUG ')
!
!     STEP 22.5--
!
      XARG=X0+H/2.0
      YARG=Y0+AK2/2.0
      YDARG=YD0+AL2/2.0
!CCCC FOUT=YDARG
!CCCC AK3=H*FOUT
      FOUT=YDARG
      AK3=H*FOUT
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')   &
      WRITE(ICOUT,2215)PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AK3
 2215 FORMAT('PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AK3 = ',4E15.7)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')   &
      CALL DPWRST('XXX','BUG ')
!
!     STEP 22.6--
!
      XARG=X0+H/2.0
      YARG=Y0+AK2/2.0
      YDARG=YD0+AL2/2.0
!CCCC CALL FUNC(XARG,YARG,YDARG,FOUT)
!CCCC AK3=H*FOUT
      PARAM(ILOCHV)=XARG
      PARAM(ILOCVV)=YARG
      PARAM(ILOCDV)=YDARG
      CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM,IPARN,IPARN2,NUMPV,   &
      IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,FOUT,   &
      IBUGCO,IBUGEV,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      AL3=H*FOUT
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')   &
      WRITE(ICOUT,2216)PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AL3
 2216 FORMAT('PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AL3 = ',4E15.7)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')   &
      CALL DPWRST('XXX','BUG ')
!
!     STEP 22.7--
!
      XARG=X0+H
      YARG=Y0+AK3
      YDARG=YD0+AL3
!CCCC FOUT=YDARG
!CCCC AK4=H*FOUT
      FOUT=YDARG
      AK4=H*FOUT
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')   &
      WRITE(ICOUT,2217)PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AK4
 2217 FORMAT('PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AK4 = ',4E15.7)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')   &
      CALL DPWRST('XXX','BUG ')
!
!     STEP 22.8--
!
      XARG=X0+H
      YARG=Y0+AK3
      YDARG=YD0+AL3
!CCCC CALL FUNC(XARG,YARG,YDARG,FOUT)
!CCCC AK4=H*FOUT
      PARAM(ILOCHV)=XARG
      PARAM(ILOCVV)=YARG
      PARAM(ILOCDV)=YDARG
      CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM,IPARN,IPARN2,NUMPV,   &
      IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,FOUT,   &
      IBUGCO,IBUGEV,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      AL4=H*FOUT
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')   &
      WRITE(ICOUT,2218)PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AL4
 2218 FORMAT('PARAM(ILOCHV),PARAM(ILOCVV),FOUT,AL4 = ',4E15.7)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')   &
      CALL DPWRST('XXX','BUG ')
!
      YNEXT=Y0+(AK1/6.0)+(AK2/3.0)+(AK3/3.0)+(AK4/6.0)
      YDNEXT=YD0+(AL1/6.0)+(AL2/3.0)+(AL3/3.0)+(AL4/6.0)
!
      Y(I)=YNEXT
      YD(I)=YDNEXT
!
 2200 CONTINUE
!
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PRK2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPRK2--')
        CALL DPWRST('XXX','BUG ')
        IF(N.GE.1)THEN
          DO 9020 I=1,N
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
!
      RETURN
      END SUBROUTINE DPRK2

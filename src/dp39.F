      SUBROUTINE GRTRIN(IX,NCHTO2,ICSTR,NCSTR)
CCCCC SUBROUTINE GRTRIN(IX,NCHTOT,ICSTR,NCSTR)
C
C     PURPOSE--TRANSLATE THE INTEGER IX INTO ITS CHARACTER EQUIVALENT.
C              NCHTOT = DESIRED NUMBER OF DIGITS IN INTEGER FORMAT (SO
C              THAT THE OUTPUT WILL CORRESPOND TO   I(NCHTOT)   FORMAT).
C              THE OUTPUT WILL BE RIGHT-JUSTIFIED AS ONE WOULD EXPECT
C              FROM AN I FORTRAN INTEGER FORMAT.
C     NOTE--THE RESULTING TRANSLATED VALUES WILL BE PLACED IN SPECIFIC
C           ELEMENTS OF THE CHARACTER VARIABLE ICSTR.  THE VALUE OF THE
C           VARIABLE    NCSTR   REPRESENTS THE NUMBER OF CHARACTERS IN
C           ICSTR THAT HAVE ALREADY BEEN FILLED.  THE RESULTING TRANSLATED
C           VALUES WILL GO (RIGHT-JUSTIFIED) INTO THE NEXT    NCHTOT
C           CHARACTERS OF ICSTR AND THE VALUE OF    NCSTR    WILL BE
C           UPDATED ACCORDINGLY, THAT IS, NEW NCSTR = OLD NCSTR + NCHTOT
C     DANGER--NCSTR IS BOTH AN INPUT ARGUMENT
C             AND AN OUTPUT ARGUMENT OF THIS SUBROUTINE.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--83.6
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MAY       1983.
C     UPDATED         --MAY 1988
C                       IF NCHTOT IS NEGATIVE, MAKE LEADING ZEROS
C                       EXPLICIT ZEROS RATHER THAN SPACES.  THIS IS
C                       REQUIRED BY THE QUIC DRIVER IN PARTICULAR.
C     UPDATED         --JANUARY  1989. SUN (BY BILL ANDERSON)
C     UPDATED         --JANUARY  1989. POSTSCRIPT (BY ALAN HECKERT)
C     UPDATED         --JANUARY  1989. CGM (BY ALAN HECKERT)
C     UPDATED         --JANUARY  1989. QMS QUIC (BY ALAN HECKERT)
C     UPDATED         --JANUARY  1989. CALCOMP (BY ALAN HECKERT)
C     UPDATED         --JANUARY  1989. ZETA (BY ALAN HECKERT)
C     UPDATED         --JULY     1996. LAHEY DRIVER (ALAN HECKERT)
C     UPDATED         --OCTOBER  1996. QUICKWIN DRIVER (ALAN)
C     UPDATED         --OCTOBER  1996. OPENGL DRIVER (ALAN)
C
C-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
C
      CHARACTER*(*) ICSTR
C
      DIMENSION IDIGIT(20)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C  MAY,1988: CHECK FOR NEGATIVE NCHTOT
      NCHTOT=ABS(NCHTO2)
      IASC0=48
      IREV=(-999)
      IDIG=(-999)
C
      IERRG4='NO'
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRIN')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF GRTRIN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IX,NCHTOT,NCSTR
   52   FORMAT('IX,NCHTOT,NCSTR = ',3I8)
        CALL DPWRST('XXX','BUG ')
        IF(NCSTR.GT.0)THEN
          DO65I=1,NCSTR
            WRITE(ICOUT,66)I,ICSTR(I:I)
   66       FORMAT('I,ICSTR(I:I) = ',I8,2X,A1)
            CALL DPWRST('XXX','BUG ')
   65     CONTINUE
        ENDIF
        WRITE(ICOUT,69)IBUGG4,ISUBG4,IERRG4
   69   FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***********************************
C               **  STEP 1--                     **
C               **  SAVE THE OLD VALUE OF NCSTR  **
C               ***********************************
C
      NCSOLD=NCSTR
C
C
C               ********************************************
C               **  STEP 2--                              **
C               **  FILL THE NEXT   NCHTOT   ELEMENTS     **
C               **  IN ICSTR() WITH BLANKS.               **
C               **  UPDATE NCSTR BY NCSTR + NCHTOT.       **
C               **  MAY,1988: - FILL WITH EXPLICIT ZEROS  **
C               **  IF NCHTOT SENT AS NEGATIVE            **
C               ********************************************
C
C
      IF(NCHTO2.LT.0)THEN
        DO1295I=1,NCHTOT
          NCSTR=NCSTR+1
          ICSTR(NCSTR:NCSTR)='0'
 1295   CONTINUE
      ELSE
        DO1200I=1,NCHTOT
          NCSTR=NCSTR+1
          ICSTR(NCSTR:NCSTR)=' '
 1200   CONTINUE
      ENDIF
C
C               ********************************
C               **  STEP 3--                  **
C               **  STRIP OUT THE DIGITS      **
C               **  (IN RIGHT TO LEFT ORDER)  **
C               ********************************
C
      ITEN=10
C
      IABSIX=IABS(IX)
      IRESUL=IABSIX
C
      NUMDIG=0
      DO1300I=1,20
        IDIVID=IRESUL
        IRESUL=IDIVID/ITEN
        IREM=IDIVID-IRESUL*ITEN
        NUMDIG=NUMDIG+1
        IDIGIT(NUMDIG)=IREM
        IF(IRESUL.EQ.0)GOTO1390
 1300 CONTINUE
 1390 CONTINUE
C
C               *****************************************************
C               **   STEP 4--                                      **
C               **   CHECK TO SEE THAT TOTAL NUMBER OF CHARACTERS  **
C               **   (= NUMBER OF DIGITS IF IX >= 0, AND           **
C               **   = NUMBER OF DIGITS + 1 IF IX < 0)             **
C               **   IS LESS THAN OR EQUAL TO NCHTOT.              **
C               **   IF YES, THEN GO TO NEXT STEP.                 **
C               **   IF NO, THEN FILL THE NCHTOT ELEMENTS          **
C               **  WITH ASTERISKS                                 **
C               *****************************************************
C
      NUMTOT=NUMDIG
      IF(IX.LT.0)NUMTOT=NUMTOT+1
C
      IF(NUMTOT.GT.NCHTOT)THEN
        DO1400I=1,NCHTOT
          J=I+NCSOLD
          ICSTR(J:J)='*'
 1400   CONTINUE
        GOTO9000
      ENDIF
C
C               ****************************************
C               **  STEP 5--                          **
C               **  IF HAVE A VALID NUMBER OF DIGITS, **
C               **  TRANSLATE EACH DIGIT TO ITS       **
C               **  CHARACTER EQUIVALENT.             **
C               **  IN DOING SO, PLACE THE ASCII      **
C               **  CHARACTER EQUIVALENT              **
C               **  IN THE USUAL RIGHT-TO-LEFT ORDER  **
C               **  IN ICSTR(.).  IF IX < 0, INSERT   **
C               **  A MINUS BEFORE THE NUMBER.        **
C               **  HAVE THE CHARACTER STRING         **
C               **  RIGHT JUSTIFIED IN THE            **
C               **  NCHTOT ELEMENTS.                  **
C               ****************************************
C
      J=NCSOLD+(NCHTOT-NUMTOT)
      IF(IX.LT.0)J=J+1
      IF(IX.LT.0)ICSTR(J:J)='-'
C
      DO1500I=1,NUMDIG
        IREV=NUMDIG-I+1
        IDIG=IDIGIT(IREV)
        J=J+1
        IASCDI=IDIG+IASC0
        CALL DPCONA(IASCDI,ICSTR(J:J))
 1500 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRIN')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF GRTRIN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)IDIVID,IRESUL,IREM,NUMDIG,IDIGIT(NUMDIG)
 9021   FORMAT('IDIVID,IRESUL,IREM,NUMDIG,IDIGIT(NUMDIG) = ',5I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9022)IREV,NCSTR,IDIG,ICSTR(NCSTR:NCSTR)
 9022   FORMAT('IREV,NCSTR,IDIG,ICSTR(NCSTR:NCSTR) = ',3I8,2X,A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9031)IASC0,IX,NUMDIG,NUMTOT,NCSTR,NCSOLD
 9031   FORMAT('IASC0,IX,NUMDIG,NUMTOT,NCSTR,NCSOLD = ',6I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMDIG.GT.0)THEN
          DO9035I=1,NUMDIG
            WRITE(ICOUT,9036)I,IDIGIT(I)
 9036       FORMAT('I,IDIGIT(I) = ',2I8)
            CALL DPWRST('XXX','BUG ')
 9035     CONTINUE
        ENDIF
        IF(NCSTR.GT.0)THEN
          DO9045I=1,NCSTR
            WRITE(ICOUT,9046)I,ICSTR(I:I)
 9046       FORMAT('I,ICSTR(I:I) = ',I8,2X,A1)
            CALL DPWRST('XXX','BUG ')
 9045     CONTINUE
        ENDIF
        WRITE(ICOUT,9049)IBUGG4,ISUBG4,IERRG4
 9049   FORMAT('IBUGG4,ISUBG4,IERRG4 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE GRTRI1(IX,NCHTO2,ICSTR,NCSTR,IJUST)
C
C     PURPOSE--TRANSLATE THE INTEGER IX INTO ITS CHARACTER EQUIVALENT.
C              NCHTOT = DESIRED NUMBER OF DIGITS IN INTEGER FORMAT (SO
C              THAT THE OUTPUT WILL CORRESPOND TO   I(NCHTOT)   FORMAT).
C              THE OUTPUT WILL BE RIGHT-JUSTIFIED AS ONE WOULD EXPECT
C              FROM AN I FORTRAN INTEGER FORMAT.
C
C              THIS IS A MODIFIED VERSION OF GRTRIN THAT ALLOWS LEFT OR
C              CENTER JUSTIFICATION.
C     NOTE--THE RESULTING TRANSLATED VALUES WILL BE PLACED IN SPECIFIC
C           ELEMENTS OF THE CHARACTER VARIABLE ICSTR.  THE VALUE OF THE
C           VARIABLE    NCSTR   REPRESENTS THE NUMBER OF CHARACTERS IN
C           ICSTR THAT HAVE ALREADY BEEN FILLED.  THE RESULTING TRANSLATED
C           VALUES WILL GO (RIGHT-JUSTIFIED) INTO THE NEXT    NCHTOT
C           CHARACTERS OF ICSTR AND THE VALUE OF    NCSTR    WILL BE
C           UPDATED ACCORDINGLY, THAT IS, NEW NCSTR = OLD NCSTR + NCHTOT
C     DANGER--NCSTR IS BOTH AN INPUT ARGUMENT
C             AND AN OUTPUT ARGUMENT OF THIS SUBROUTINE.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2015.4
C     ORIGINAL VERSION--APRIL     2015.
C
C-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
C
      CHARACTER*4   IJUST
      CHARACTER*(*) ICSTR
C
      DIMENSION IDIGIT(20)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      NCHTOT=ABS(NCHTO2)
      IASC0=48
      IREV=(-999)
      IDIG=(-999)
C
      IERRG4='NO'
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRI1')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF GRTRI1--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IX,NCHTOT,NCSTR
   52   FORMAT('IX,NCHTOT,NCSTR = ',3I8)
        CALL DPWRST('XXX','BUG ')
        IF(NCSTR.GT.0)THEN
          DO65I=1,NCSTR
            WRITE(ICOUT,66)I,ICSTR(I:I)
   66       FORMAT('I,ICSTR(I:I) = ',I8,2X,A1)
            CALL DPWRST('XXX','BUG ')
   65     CONTINUE
        ENDIF
        WRITE(ICOUT,69)IBUGG4,ISUBG4,IERRG4
   69   FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***********************************
C               **  STEP 1--                     **
C               **  SAVE THE OLD VALUE OF NCSTR  **
C               ***********************************
C
      NCSOLD=NCSTR
C
C
C               ********************************************
C               **  STEP 2--                              **
C               **  FILL THE NEXT   NCHTOT   ELEMENTS     **
C               **  IN ICSTR() WITH BLANKS.               **
C               **  UPDATE NCSTR BY NCSTR + NCHTOT.       **
C               **  MAY,1988: - FILL WITH EXPLICIT ZEROS  **
C               **  IF NCHTOT SENT AS NEGATIVE            **
C               ********************************************
C
C
      IF(NCHTO2.LT.0)THEN
        DO1295I=1,NCHTOT
          NCSTR=NCSTR+1
          ICSTR(NCSTR:NCSTR)='0'
 1295   CONTINUE
      ELSE
        DO1200I=1,NCHTOT
          NCSTR=NCSTR+1
          ICSTR(NCSTR:NCSTR)=' '
 1200   CONTINUE
      ENDIF
C
C               ********************************
C               **  STEP 3--                  **
C               **  STRIP OUT THE DIGITS      **
C               **  (IN RIGHT TO LEFT ORDER)  **
C               ********************************
C
      ITEN=10
C
      IABSIX=IABS(IX)
      IRESUL=IABSIX
C
      NUMDIG=0
      DO1300I=1,20
        IDIVID=IRESUL
        IRESUL=IDIVID/ITEN
        IREM=IDIVID-IRESUL*ITEN
        NUMDIG=NUMDIG+1
        IDIGIT(NUMDIG)=IREM
        IF(IRESUL.EQ.0)GOTO1390
 1300 CONTINUE
 1390 CONTINUE
C
C               *****************************************************
C               **   STEP 4--                                      **
C               **   CHECK TO SEE THAT TOTAL NUMBER OF CHARACTERS  **
C               **   (= NUMBER OF DIGITS IF IX >= 0, AND           **
C               **   = NUMBER OF DIGITS + 1 IF IX < 0)             **
C               **   IS LESS THAN OR EQUAL TO NCHTOT.              **
C               **   IF YES, THEN GO TO NEXT STEP.                 **
C               **   IF NO, THEN FILL THE NCHTOT ELEMENTS          **
C               **  WITH ASTERISKS                                 **
C               *****************************************************
C
      NUMTOT=NUMDIG
      IF(IX.LT.0)NUMTOT=NUMTOT+1
C
      IF(NUMTOT.GT.NCHTOT)THEN
        DO1400I=1,NCHTOT
          J=I+NCSOLD
          ICSTR(J:J)='*'
 1400   CONTINUE
        GOTO9000
      ENDIF
C
C               ****************************************
C               **  STEP 5--                          **
C               **  IF HAVE A VALID NUMBER OF DIGITS, **
C               **  TRANSLATE EACH DIGIT TO ITS       **
C               **  CHARACTER EQUIVALENT.             **
C               **  IN DOING SO, PLACE THE ASCII      **
C               **  CHARACTER EQUIVALENT              **
C               **  IN THE USUAL RIGHT-TO-LEFT ORDER  **
C               **  IN ICSTR(.).  IF IX < 0, INSERT   **
C               **  A MINUS BEFORE THE NUMBER.        **
C               **  HAVE THE CHARACTER STRING         **
C               **  RIGHT JUSTIFIED IN THE            **
C               **  NCHTOT ELEMENTS.                  **
C               ****************************************
C
      IF(IJUST.EQ.'l')THEN
        J=NCSOLD+1
      ELSEIF(IJUST.EQ.'c')THEN
        NTEMP=(NCHTOT-NUMTOT)/2
        NREM=MOD(NCHTOT-NUMTOT,2)
        IF(NREM.EQ.1)NTEMP=NTEMP+1
        IF(NTEMP.GE.1)THEN
          J=NCSOLD + NTEMP
        ELSE
          J=NCSOLD+1
        ENDIF
      ELSE
        J=NCSOLD+(NCHTOT-NUMTOT)
      ENDIF
C
      IF(IX.LT.0)J=J+1
      IF(IX.LT.0)ICSTR(J:J)='-'
C
      DO1500I=1,NUMDIG
        IREV=NUMDIG-I+1
        IDIG=IDIGIT(IREV)
        J=J+1
        IASCDI=IDIG+IASC0
        CALL DPCONA(IASCDI,ICSTR(J:J))
 1500 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRI1')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF GRTRI1--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)IDIVID,IRESUL,IREM,NUMDIG,IDIGIT(NUMDIG)
 9021   FORMAT('IDIVID,IRESUL,IREM,NUMDIG,IDIGIT(NUMDIG) = ',5I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9022)IREV,NCSTR,IDIG,ICSTR(NCSTR:NCSTR)
 9022   FORMAT('IREV,NCSTR,IDIG,ICSTR(NCSTR:NCSTR) = ',3I8,2X,A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9031)IASC0,IX,NUMDIG,NUMTOT,NCSTR,NCSOLD
 9031   FORMAT('IASC0,IX,NUMDIG,NUMTOT,NCSTR,NCSOLD = ',6I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMDIG.GT.0)THEN
          DO9035I=1,NUMDIG
            WRITE(ICOUT,9036)I,IDIGIT(I)
 9036       FORMAT('I,IDIGIT(I) = ',2I8)
            CALL DPWRST('XXX','BUG ')
 9035     CONTINUE
        ENDIF
        IF(NCSTR.GT.0)THEN
          DO9045I=1,NCSTR
            WRITE(ICOUT,9046)I,ICSTR(I:I)
 9046       FORMAT('I,ICSTR(I:I) = ',I8,2X,A1)
            CALL DPWRST('XXX','BUG ')
 9045     CONTINUE
        ENDIF
        WRITE(ICOUT,9049)IBUGG4,ISUBG4,IERRG4
 9049   FORMAT('IBUGG4,ISUBG4,IERRG4 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE GRTRPG(IPATT,ISTRIN,NCSTRI)
C
C     PURPOSE--TRANSLATE A 1-WORD (4-CHARACTER) REPRESENTATION
C              FOR A MARKER INTO A MULTI-WORD REPRESENTATION
C              THAT SUBROUTINE DPSCR7 CAN UNDERSTAND.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--83.6
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MAY       1983.
C
C-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
C
      CHARACTER*(*) IPATT
      CHARACTER*4 ISTRIN
C
      CHARACTER*1 IC1
C
      DIMENSION ISTRIN(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IERRG4='NO'
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRPG')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF GRTRPG--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGG4,IPATT
   52   FORMAT('IBUGG4,IPATT = ',A4,2X,A24)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      NCSTRI=0
      DO1100I=1,16
        ISTRIN(I)='    '
 1100 CONTINUE
C
      DO1200I=1,24
        IREV=16-I+1
        IC1=IPATT(IREV:IREV)
        IF(IC1.NE.' ')GOTO1250
 1200 CONTINUE
 1250 CONTINUE
      NCSTRI=IREV
      IF(NCSTRI.LE.0)NCSTRI=1
C
      DO1300I=1,NCSTRI
        ISTRIN(I)=IPATT(I:I)
 1300 CONTINUE
C
      IF(NCSTRI.GT.1)THEN
        NCSTRI=NCSTRI+1
        ISTRIN(NCSTRI)='('
        NCSTRI=NCSTRI+1
        ISTRIN(NCSTRI)=')'
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRPG')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF GRTRPG--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NCSTRI
 9013   FORMAT('NCSTRI = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,NCSTRI
          WRITE(ICOUT,9016)I,ISTRIN(I)
 9016     FORMAT('I,ISTRIN(I) = ',I8,2X,A4)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE GRTRRE(X,NCHTOT,NCHDEC,ICSTR,NCSTR)
C
C     PURPOSE--TRANSLATE THE REAL (= FLOATING POINT) NUMBER X
C              INTO ITS CHARACTER EQUIVALENT.
C              NCHTOT = DESIRED NUMBER OF CHARACTERS FOR TOTAL NUMBER.
C              NCTDEC = DESIRED NUMBER OF CHARACTERS FOR DECIMAL PART OF NUMBER.
C              THE OUTPUT WILL CORRESPOND TO A FORTRAN F FORMAT.
C              AS IN    F NCHTOT.NCHDEC     FORMAT (E.G., F10.5).
C              THE OUTPUT WILL BE RIGHT-JUSTIFIED AS ONE
C              WOULD EXPECT FROM A F FORTRAN FLOATING POINT FORMAT.
C     NOTE--THE RESULTING TRANSLATED VALUES
C           WILL BE PLACED IN SPECIFIC ELEMENTS
C           OF THE A130 CHARACTER VARIABLE ICSTR.
C           THE VALUE OF THE VARIABLE    NCSTR
C           REPRESENTS THE NUMBER OF CHARACTERS IN ICSTR
C           THAT HAVE ALREADY BEEN FILLED.
C           THE RESULTING TRANSLATED VALUES WILL GO
C           (RIGHT-JUSTIFIED) INTO THE NEXT    NCHTOT
C           CHARACTERS OF ICSTR.
C           AND THE VALUE OF    NCSTR    WILL BE
C           UPDATED ACCORDINGLY, THAT IS,
C           NEW NCSTR = OLD NCSTR + NCHTOT
C     DANGER--NCSTR IS BOTH AN INPUT ARGUMENT
C             AND AN OUTPUT ARGUMENT OF THIS SUBROUTINE.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--83.6
C     ORIGINAL VERSION --MAY       1983.
C     UPDATED          --MAY       2014. ROUND NUMBER FOR DECIMAL PART
C                                        FIRST
C
C-----NON-COMMON VARIABLES (GRAPHICS)---------------------------------
C
      CHARACTER*(*) ICSTR
C
      DIMENSION IINTDI(20)
      DIMENSION IDECDI(20)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IASC0=48
      IREV=(-999)
      IDIG=(-999)
      IERRG4='NO'
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRRE')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF GRTRRE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)NCHTOT,NCSTR,X
   52   FORMAT('NCHTOT,NCSTR,X = ',2I8,G15.7)
        CALL DPWRST('XXX','BUG ')
        IF(NCSTR.GT.0)THEN
          DO65I=1,NCSTR
            WRITE(ICOUT,66)I,ICSTR(I:I)
   66       FORMAT('I,ICSTR(I:I) = ',I8,2X,A1)
            CALL DPWRST('XXX','BUG ')
   65     CONTINUE
        ENDIF
        WRITE(ICOUT,69)IBUGG4,ISUBG4,IERRG4
   69   FORMAT('IBUGG4,ISUBG4,IERRG4 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***********************************
C               **  STEP 1--                     **
C               **  SAVE THE OLD VALUE OF NCSTR  **
C               ***********************************
C
      NCSOLD=NCSTR
C
C     2014/05: ROUND NUMBER FIRST
C
      EPS=5./(10**(NCHDEC+1))
      IF(X.GE.0.0)THEN
        XRND=X+EPS
      ELSE
        XRND=X-EPS
      ENDIF
CCCCC XRND=RND(X,NCHDEC)
C
C
C               ********************************************
C               **  STEP 2--                              **
C               **  FILL THE NEXT   NCHTOT   ELEMENTS     **
C               **  IN ICSTR() WITH BLANKS.               **
C               **  UPDATE NCSTR BY NCSTR + NCHTOT.       **
C               ********************************************
C
      DO1200I=1,NCHTOT
        NCSTR=NCSTR+1
        ICSTR(NCSTR:NCSTR)=' '
 1200 CONTINUE
C
C               ********************************
C               **  STEP 3--                  **
C               **  STRIP OUT THE DIGITS      **
C               **  OF THE INTEGER PART       **
C               **  (IN RIGHT TO LEFT ORDER)  **
C               ********************************
C
      ITEN=10
C
      IX=INT(XRND)
      IABSIX=IABS(IX)
C
      IRESUL=IABSIX
      NUMIND=0
      DO1300I=1,20
        IDIVID=IRESUL
        IRESUL=IDIVID/ITEN
        IREM=IDIVID-IRESUL*ITEN
        NUMIND=NUMIND+1
        IINTDI(NUMIND)=IREM
        IF(IRESUL.EQ.0)GOTO1390
 1300 CONTINUE
 1390 CONTINUE
C
C               ********************************
C               **  STEP 4--                  **
C               **  STRIP OUT NCHDEC DIGITS   **
C               **  OF THE DECIMAL PART       **
C               **  (IN RIGHT TO LEFT ORDER)  **
C               ********************************
C
      ITEN=10
C
      AIABIX=REAL(IABSIX)
      ABSX=ABS(XRND)
      DEC=AIABIX-ABSX
      IF(DEC.LT.0.0)DEC=0.0
      DEC=ABSX-AIABIX
      DEC2=DEC*(10.0**NCHDEC)
C
      IY=INT(DEC2)
      IABSIY=IABS(IY)
C
      IRESUL=IABSIY
      NUMDED=0
      IF(NCHDEC.LE.0)GOTO1490
      DO1400I=1,NCHDEC
        IDIVID=IRESUL
        IRESUL=IDIVID/ITEN
        IREM=IDIVID-IRESUL*ITEN
        NUMDED=NUMDED+1
        IDECDI(NUMDED)=IREM
 1400 CONTINUE
 1490 CONTINUE
C
C               *****************************************************
C               **   STEP 5--                                      **
C               **   CHECK TO SEE THAT TOTAL NUMBER OF CHARACTERS  **
C               **   (= NUMBER OF DIGITS + 1 IF IX >= 0, AND       **
C               **   = NUMBER OF DIGITS + 2 IF IX < 0)             **
C               **   IS LESS THAN OR EQUAL TO NCHTOT.              **
C               **   IF YES, THEN GO TO NEXT STEP.                 **
C               **   IF NO, THEN FILL THE NCHTOT ELEMENTS          **
C               **   WITH ASTERISKS                                **
C               *****************************************************
C
      NUMTOT=NUMIND+1+NUMDED
      IF(X.LT.0.0)NUMTOT=NUMTOT+1
C
      IF(NUMTOT.LE.NCHTOT)GOTO1590
      DO1500I=1,NCHTOT
      J=I+NCSOLD
      ICSTR(J:J)='*'
 1500 CONTINUE
      GOTO9000
 1590 CONTINUE
C
C               ****************************************
C               **  STEP 6--                          **
C               **  IF HAVE A VALID NUMBER OF DIGITS, **
C               **  TRANSLATE EACH DIGIT TO ITS       **
C               **  CHARACTER EQUIVALENT.             **
C               **  IN DOING SO, PLACE THE ASCII      **
C               **  CHARACTER EQUIVALENT              **
C               **  IN THE USUAL RIGHT-TO-LEFT ORDER  **
C               **  IN ICSTR(.).  IF IX < 0, INSERT   **
C               **  A MINUS BEFORE THE NUMBER.        **
C               **  HAVE THE CHARACTER STRING         **
C               **  RIGHT JUSTIFIED IN THE            **
C               **  NCHTOT ELEMENTS.                  **
C               ****************************************
C
      J=NCSOLD+(NCHTOT-NUMTOT)
      IF(X.LT.0.0)J=J+1
      IF(X.LT.0.0)ICSTR(J:J)='-'
C
      DO1610I=1,NUMIND
        IREV=NUMIND-I+1
        IDIG=IINTDI(IREV)
        J=J+1
        IASCDI=IDIG+IASC0
CCCCC   ICSTR(J:J)=CHAR(IASCDI)
        CALL DPCONA(IASCDI,ICSTR(J:J))
 1610 CONTINUE
C
      J=J+1
      ICSTR(J:J)='.'
C
      DO1620I=1,NUMDED
        IREV=NUMDED-I+1
        IDIG=IDECDI(IREV)
        J=J+1
        IASCDI=IDIG+IASC0
CCCCC   ICSTR(J:J)=CHAR(IASCDI)
        CALL DPCONA(IASCDI,ICSTR(J:J))
 1620 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRRE')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF GRTRRE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NUMIND,NUMDED,NUMTOT
 9014   FORMAT('NUMIND,NUMDED,NUMTOT = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)IDIVID,IRESUL,IREM,NUMIND,IINTDI(NUMIND)
 9021   FORMAT('IDIVID,IRESUL,IREM,NUMIND,IINTDI(NUMIND) = ',5I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9022)IREV,NCSTR,IDIG,ICSTR(NCSTR:NCSTR)
 9022   FORMAT('IREV,NCSTR,IDIG,ICSTR(NCSTR:NCSTR) = ',3I8,2X,A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9023)IASC0,IX,NUMIND,NUMTOT
 9023   FORMAT('IASC0,IX,NUMIND,NUMTOT = ',4I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMIND.GT.0)THEN
          DO9035I=1,NUMIND
            WRITE(ICOUT,9036)I,IINTDI(I)
 9036       FORMAT('I,IINTDI(I) = ',2I8)
            CALL DPWRST('XXX','BUG ')
 9035     CONTINUE
        ENDIF
        WRITE(ICOUT,9041)NUMDED,NUMTOT,NCSTR,NCSOLD
 9041   FORMAT('NUMDED,NUMTOT,NCSTR,NCSOLD = ',4I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMDED.GT.0)THEN
          DO9045I=1,NUMDED
            WRITE(ICOUT,9046)I,IDECDI(I)
 9046       FORMAT('I,IDECDI(I) = ',2I8)
            CALL DPWRST('XXX','BUG ')
 9045     CONTINUE
        ENDIF
        IF(NCSTR.GT.0)THEN
          DO9055I=1,NCSTR
            WRITE(ICOUT,9056)I,ICSTR(I:I)
 9056       FORMAT('I,ICSTR(I:I) = ',I8,2X,A1)
            CALL DPWRST('XXX','BUG ')
 9055     CONTINUE
        ENDIF
        WRITE(ICOUT,9059)IBUGG4,ISUBG4,IERRG4
 9059   FORMAT('IBUGG4,ISUBG4,IERRG4 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE GRTRR1(X,NCHTOT,NCHDEC,ICSTR,NCSTR,IJUST)
C
C     PURPOSE--TRANSLATE THE REAL (= FLOATING POINT) NUMBER X
C              INTO ITS CHARACTER EQUIVALENT.
C              NCHTOT = DESIRED NUMBER OF CHARACTERS FOR TOTAL NUMBER.
C              NCHDEC = DESIRED NUMBER OF CHARACTERS FOR DECIMAL PART OF NUMBER.
C              THE OUTPUT WILL CORRESPOND TO A FORTRAN F FORMAT.
C              AS IN    F NCHTOT.NCHDEC     FORMAT (E.G., F10.5).
C
C              THIS IS A MODIFIED VERSION OF GRTRRE.  THE GRTRRE ROUTINE
C              WILL RIGHT JUSTIFY THE NUMBER.  THIS ROUTINE WILL ALLOW
C              THE CALLING ROUTINE TO SPECIFY EITHER LEFT, CENTER, OR
C              RIGHT JUSTIFICATION.
C     NOTE--THE RESULTING TRANSLATED VALUES WILL BE PLACED IN SPECIFIC
C           ELEMENTS OF THE CHARACTER VARIABLE ICSTR.  THE VALUE OF THE
C           VARIABLE    NCSTR   REPRESENTS THE NUMBER OF CHARACTERS IN
C           ICSTR THAT HAVE ALREADY BEEN FILLED.  THE RESULTING
C           TRANSLATED VALUES WILL GO INTO THE NEXT    NCHTOT
C           CHARACTERS OF ICSTR.  AND THE VALUE OF    NCSTR    WILL BE
C           UPDATED ACCORDINGLY, THAT IS, NEW NCSTR = OLD NCSTR + NCHTOT
C     DANGER--NCSTR IS BOTH AN INPUT ARGUMENT
C             AND AN OUTPUT ARGUMENT OF THIS SUBROUTINE.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2015.4
C     ORIGINAL VERSION --APRIL     2015. MADE DISTINCT FROM GRTRRE
C
C-----NON-COMMON VARIABLES (GRAPHICS)---------------------------------
C
      CHARACTER*4   IJUST
      CHARACTER*(*) ICSTR
C
      DIMENSION IINTDI(20)
      DIMENSION IDECDI(20)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IASC0=48
      IREV=(-999)
      IDIG=(-999)
      IERRG4='NO'
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRR1')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF GRTR1--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)NCHTOT,NCSTR,X
   52   FORMAT('NCHTOT,NCSTR,X = ',2I8,G15.7)
        CALL DPWRST('XXX','BUG ')
        IF(NCSTR.GT.0)THEN
          DO65I=1,NCSTR
            WRITE(ICOUT,66)I,ICSTR(I:I)
   66       FORMAT('I,ICSTR(I:I) = ',I8,2X,A1)
            CALL DPWRST('XXX','BUG ')
   65     CONTINUE
        ENDIF
        WRITE(ICOUT,69)IBUGG4,ISUBG4,IERRG4
   69   FORMAT('IBUGG4,ISUBG4,IERRG4 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***********************************
C               **  STEP 1--                     **
C               **  SAVE THE OLD VALUE OF NCSTR  **
C               ***********************************
C
      NCSOLD=NCSTR
C
C     ROUND NUMBER FIRST
C
      XSAVE=X
      X=ABS(X)
      EPS=5./(10**(NCHDEC+1))
      XRND=X+EPS
C
C
C               ********************************************
C               **  STEP 2--                              **
C               **  FILL THE NEXT   NCHTOT   ELEMENTS     **
C               **  IN ICSTR() WITH BLANKS.               **
C               **  UPDATE NCSTR BY NCSTR + NCHTOT.       **
C               ********************************************
C
      DO1200I=1,NCHTOT
        NCSTR=NCSTR+1
        ICSTR(NCSTR:NCSTR)=' '
 1200 CONTINUE
C
C               ********************************
C               **  STEP 3--                  **
C               **  STRIP OUT THE DIGITS      **
C               **  OF THE INTEGER PART       **
C               **  (IN RIGHT TO LEFT ORDER)  **
C               ********************************
C
      ITEN=10
C
      IX=INT(XRND)
      IABSIX=IABS(IX)
C
      IRESUL=IABSIX
      NUMIND=0
      DO1300I=1,20
        IDIVID=IRESUL
        IRESUL=IDIVID/ITEN
        IREM=IDIVID-IRESUL*ITEN
        NUMIND=NUMIND+1
        IINTDI(NUMIND)=IREM
        IF(IRESUL.EQ.0)GOTO1390
 1300 CONTINUE
 1390 CONTINUE
C
C               ********************************
C               **  STEP 4--                  **
C               **  STRIP OUT NCHDEC DIGITS   **
C               **  OF THE DECIMAL PART       **
C               **  (IN RIGHT TO LEFT ORDER)  **
C               ********************************
C
      ITEN=10
C
      AIABIX=REAL(IABSIX)
      ABSX=ABS(XRND)
      DEC=AIABIX-ABSX
      IF(DEC.LT.0.0)DEC=0.0
      DEC=ABSX-AIABIX
      DEC2=DEC*(10.0**NCHDEC)
C
      IY=INT(DEC2)
      IABSIY=IABS(IY)
C
      IRESUL=IABSIY
      NUMDED=0
      IF(NCHDEC.LE.0)GOTO1490
      DO1400I=1,NCHDEC
        IDIVID=IRESUL
        IRESUL=IDIVID/ITEN
        IREM=IDIVID-IRESUL*ITEN
        NUMDED=NUMDED+1
        IDECDI(NUMDED)=IREM
 1400 CONTINUE
 1490 CONTINUE
C
C               *****************************************************
C               **   STEP 5--                                      **
C               **   CHECK TO SEE THAT TOTAL NUMBER OF CHARACTERS  **
C               **   (= NUMBER OF DIGITS + 1 IF IX >= 0, AND       **
C               **   = NUMBER OF DIGITS + 2 IF IX < 0)             **
C               **   IS LESS THAN OR EQUAL TO NCHTOT.              **
C               **   IF YES, THEN GO TO NEXT STEP.                 **
C               **   IF NO, THEN FILL THE NCHTOT ELEMENTS          **
C               **   WITH ASTERISKS                                **
C               *****************************************************
C
      NUMTOT=NUMIND+1+NUMDED
      IF(XSAVE.LT.0.0)NUMTOT=NUMTOT+1
C
      IF(NUMTOT.GT.NCHTOT)THEN
        DO1500I=1,NCHTOT
          J=I+NCSOLD
          ICSTR(J:J)='*'
 1500   CONTINUE
        GOTO9000
      ENDIF
C
C               ****************************************
C               **  STEP 6--                          **
C               **  IF HAVE A VALID NUMBER OF DIGITS, **
C               **  TRANSLATE EACH DIGIT TO ITS       **
C               **  CHARACTER EQUIVALENT.             **
C               **  IN DOING SO, PLACE THE ASCII      **
C               **  CHARACTER EQUIVALENT              **
C               **  IN THE USUAL RIGHT-TO-LEFT ORDER  **
C               **  IN ICSTR(.).  IF IX < 0, INSERT   **
C               **  A MINUS BEFORE THE NUMBER.        **
C               **  HAVE THE CHARACTER STRING         **
C               **  RIGHT JUSTIFIED IN THE            **
C               **  NCHTOT ELEMENTS.                  **
C               ****************************************
C
C
C     RIGHT JUSTIFIED
C
      IF(IJUST.EQ.'l')THEN
        J=NCSOLD+1
      ELSEIF(IJUST.EQ.'c')THEN
        J=NCSOLD + ((NCHTOT-NUMTOT)/2)
      ELSE
        J=NCSOLD+(NCHTOT-NUMTOT)
      ENDIF
C
      IF(XSAVE.LT.0.0)J=J+1
      IF(XSAVE.LT.0.0)ICSTR(J:J)='-'
C
      DO1610I=1,NUMIND
        IREV=NUMIND-I+1
        IDIG=IINTDI(IREV)
        J=J+1
        IASCDI=IDIG+IASC0
        CALL DPCONA(IASCDI,ICSTR(J:J))
 1610 CONTINUE
C
      J=J+1
      ICSTR(J:J)='.'
C
      DO1620I=1,NUMDED
        IREV=NUMDED-I+1
        IDIG=IDECDI(IREV)
        J=J+1
        IASCDI=IDIG+IASC0
        CALL DPCONA(IASCDI,ICSTR(J:J))
 1620 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRR1')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF GRTR1--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NUMIND,NUMDED,NUMTOT
 9014   FORMAT('NUMIND,NUMDED,NUMTOT = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)IDIVID,IRESUL,IREM,NUMIND,IINTDI(NUMIND)
 9021   FORMAT('IDIVID,IRESUL,IREM,NUMIND,IINTDI(NUMIND) = ',5I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9022)IREV,NCSTR,IDIG,ICSTR(NCSTR:NCSTR)
 9022   FORMAT('IREV,NCSTR,IDIG,ICSTR(NCSTR:NCSTR) = ',3I8,2X,A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9023)IASC0,IX,NUMIND,NUMTOT
 9023   FORMAT('IASC0,IX,NUMIND,NUMTOT = ',4I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMIND.GT.0)THEN
          DO9035I=1,NUMIND
            WRITE(ICOUT,9036)I,IINTDI(I)
 9036       FORMAT('I,IINTDI(I) = ',2I8)
            CALL DPWRST('XXX','BUG ')
 9035     CONTINUE
        ENDIF
        WRITE(ICOUT,9041)NUMDED,NUMTOT,NCSTR,NCSOLD
 9041   FORMAT('NUMDED,NUMTOT,NCSTR,NCSOLD = ',4I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMDED.GT.0)THEN
          DO9045I=1,NUMDED
            WRITE(ICOUT,9046)I,IDECDI(I)
 9046       FORMAT('I,IDECDI(I) = ',2I8)
            CALL DPWRST('XXX','BUG ')
 9045     CONTINUE
        ENDIF
        IF(NCSTR.GT.0)THEN
          DO9055I=1,NCSTR
            WRITE(ICOUT,9056)I,ICSTR(I:I)
 9056       FORMAT('I,ICSTR(I:I) = ',I8,2X,A1)
            CALL DPWRST('XXX','BUG ')
 9055     CONTINUE
        ENDIF
        WRITE(ICOUT,9059)IBUGG4,ISUBG4,IERRG4
 9059   FORMAT('IBUGG4,ISUBG4,IERRG4 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE GRTRRP(IPATTT,IHORPA,IVERPA,IDUPPA,IDDOPA)
C  MARCH, 1988: FOR EACH PAATTERN TYPE, SET IPATT TO A STANDARD
C               NAME.
C
C     PURPOSE--CONVERT A 1 WORD REPRESENTATION FOR THE PATTERN
C              (FOR A REGION)
C              INTO 4 SINGLE-WORD REPRESENTATIONS
C              BASED ON HORIZONTAL, VERTICAL,
C              DIAGONAL UP, AND DIAGONAL DOWN
C              COMPONENTS OF THE PATTERN.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--83.6
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MAY       1983.
C
C-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
C
      CHARACTER*4 IPATTT
      CHARACTER*4 IHORPA
      CHARACTER*4 IVERPA
      CHARACTER*4 IDUPPA
      CHARACTER*4 IDDOPA
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'TRRP')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF GRTRRP--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IPATTT
   52 FORMAT('IPATTT = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGG4,ISUBG4,IERRG4
   59 FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
      IHORPA='OFF'
      IVERPA='OFF'
      IDUPPA='OFF'
      IDDOPA='OFF'
C
      IF(IPATTT.EQ.'    ')GOTO110
      IF(IPATTT.EQ.'BLAN')GOTO110
      IF(IPATTT.EQ.'NONE')GOTO110
      IF(IPATTT.EQ.'EMPT')GOTO110
C
      IF(IPATTT.EQ.'HORI')GOTO210
      IF(IPATTT.EQ.'H   ')GOTO210
      IF(IPATTT.EQ.'VERT')GOTO220
      IF(IPATTT.EQ.'V   ')GOTO220
      IF(IPATTT.EQ.'SOLI')GOTO220
      IF(IPATTT.EQ.'DU  ')GOTO230
      IF(IPATTT.EQ.'D1  ')GOTO230
      IF(IPATTT.EQ.'DD  ')GOTO240
      IF(IPATTT.EQ.'D2  ')GOTO240
C
      IF(IPATTT.EQ.'HV  ')GOTO310
      IF(IPATTT.EQ.'VH  ')GOTO310
      IF(IPATTT.EQ.'DUDD')GOTO320
      IF(IPATTT.EQ.'D1D2')GOTO320
      IF(IPATTT.EQ.'DDDU')GOTO320
      IF(IPATTT.EQ.'D2D1')GOTO320
C
      IF(IPATTT.EQ.'HDU ')GOTO410
      IF(IPATTT.EQ.'HD1 ')GOTO410
CCCCC OCTOBER 1992.  ADD FOLLOWING 2 LINES
      IF(IPATTT.EQ.'HOD1')GOTO410
      IF(IPATTT.EQ.'D1HO')GOTO410
      IF(IPATTT.EQ.'HDD ')GOTO420
      IF(IPATTT.EQ.'HD2 ')GOTO420
CCCCC OCTOBER 1992.  ADD FOLLOWING 4 LINES
      IF(IPATTT.EQ.'HOD2')GOTO420
      IF(IPATTT.EQ.'D2HO')GOTO420
      IF(IPATTT.EQ.'DDHO')GOTO420
      IF(IPATTT.EQ.'HODD')GOTO420
CCCCC OCTOBER 1992.  ADD FOLLOWING 6 LINES
      IF(IPATTT.EQ.'HO12')GOTO425
      IF(IPATTT.EQ.'12HO')GOTO425
      IF(IPATTT.EQ.'H12 ')GOTO425
      IF(IPATTT.EQ.'12H ')GOTO425
      IF(IPATTT.EQ.'HD12')GOTO425
      IF(IPATTT.EQ.'D12H')GOTO425
C
      IF(IPATTT.EQ.'VDU ')GOTO430
      IF(IPATTT.EQ.'VD1 ')GOTO430
CCCCC OCTOBER 1992.  ADD FOLLOWING 2 LINES
      IF(IPATTT.EQ.'VED1')GOTO430
      IF(IPATTT.EQ.'D1VE')GOTO430
      IF(IPATTT.EQ.'VDD ')GOTO440
CCCCC OCTOBER 1992.  ADD FOLLOWING 2 LINES
      IF(IPATTT.EQ.'VED2')GOTO440
      IF(IPATTT.EQ.'D2VE')GOTO440
CCCCC OCTOBER 1992.  ADD FOLLOWING 4 LINES
      IF(IPATTT.EQ.'VE12')GOTO445
      IF(IPATTT.EQ.'12VE')GOTO445
      IF(IPATTT.EQ.'V12 ')GOTO445
      IF(IPATTT.EQ.'12V ')GOTO445
      IF(IPATTT.EQ.'VD12')GOTO445
      IF(IPATTT.EQ.'D12V')GOTO445
C
      IF(IPATTT.EQ.'HVDU')GOTO510
      IF(IPATTT.EQ.'HVD1')GOTO510
      IF(IPATTT.EQ.'VHDU')GOTO510
      IF(IPATTT.EQ.'VHD1')GOTO510
      IF(IPATTT.EQ.'HVDD')GOTO520
      IF(IPATTT.EQ.'HVD2')GOTO520
      IF(IPATTT.EQ.'VHDD')GOTO520
      IF(IPATTT.EQ.'VHD2')GOTO520
CCCCC OCTOBER 1992.  ADD FOLLOWING LINES
      IF(IPATTT.EQ.'HV12')GOTO610
      IF(IPATTT.EQ.'12HV')GOTO610
C
      IF(IPATTT.EQ.'ALL')GOTO610
C
      GOTO9000
C
  110 CONTINUE
      IHORPA='OFF'
      IVERPA='OFF'
      IDUPPA='OFF'
      IDDOPA='OFF'
      IPATTT='EMPT'
      GOTO9000
C
  210 CONTINUE
      IHORPA='ON'
      IVERPA='OFF'
      IDUPPA='OFF'
      IDDOPA='OFF'
      IPATTT='HORI'
      GOTO9000
C
  220 CONTINUE
      IHORPA='OFF'
      IVERPA='ON'
      IDUPPA='OFF'
      IDDOPA='OFF'
      IF(IPATTT.EQ.'V')IPATTT='VERT'
      GOTO9000
C
  230 CONTINUE
      IHORPA='OFF'
      IVERPA='OFF'
      IDUPPA='ON'
      IDDOPA='OFF'
      IPATTT='D1'
      GOTO9000
C
  240 CONTINUE
      IHORPA='OFF'
      IVERPA='OFF'
      IDUPPA='OFF'
      IDDOPA='ON'
      IPATTT='D2'
      GOTO9000
C
  310 CONTINUE
      IHORPA='ON'
      IVERPA='ON'
      IDUPPA='OFF'
      IDDOPA='OFF'
      IPATTT='HV'
      GOTO9000
C
  320 CONTINUE
      IHORPA='OFF'
      IVERPA='OFF'
      IDUPPA='ON'
      IDDOPA='ON'
      IPATTT='D1D2'
      GOTO9000
C
  410 CONTINUE
      IHORPA='ON'
      IVERPA='OFF'
      IDUPPA='ON'
      IDDOPA='OFF'
      IPATTT='HD1'
      GOTO9000
C
  420 CONTINUE
      IHORPA='ON'
      IVERPA='OFF'
      IDUPPA='OFF'
      IDDOPA='ON'
      IPATTT='HD2'
      GOTO9000
C
CCCCC OCTOBER 1992.  ADD FOLLOWING BLOCK OF CODE
  425 CONTINUE
      IHORPA='ON'
      IVERPA='OFF'
      IDUPPA='ON'
      IDDOPA='ON'
      IPATTT='HD12'
      GOTO9000
C
  430 CONTINUE
      IHORPA='OFF'
      IVERPA='ON'
      IDUPPA='ON'
      IDDOPA='OFF'
      IPATTT='VD1'
      GOTO9000
C
  440 CONTINUE
      IHORPA='OFF'
      IVERPA='ON'
      IDUPPA='OFF'
      IDDOPA='ON'
      IPATTT='VD2'
      GOTO9000
C
CCCCC OCTOBER 1992.  ADD FOLLOWING BLOCK OF CODE
  445 CONTINUE
      IHORPA='OFF'
      IVERPA='ON'
      IDUPPA='ON'
      IDDOPA='ON'
      IPATTT='VD12'
      GOTO9000
C
  510 CONTINUE
      IHORPA='ON'
      IVERPA='ON'
      IDUPPA='ON'
      IDDOPA='OFF'
      IPATTT='HVD1'
      GOTO9000
C
  520 CONTINUE
      IHORPA='ON'
      IVERPA='ON'
      IDUPPA='OFF'
      IDDOPA='ON'
      IPATTT='HVD2'
      GOTO9000
C
  610 CONTINUE
      IHORPA='ON'
      IVERPA='ON'
      IDUPPA='ON'
      IDDOPA='ON'
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'TRRP')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF GRTRRP--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IPATTT
 9012 FORMAT('IPATTT = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IHORPA,IVERPA,IDUPPA,IDDOPA
 9013 FORMAT('IHORPA,IVERPA,IDUPPA,IDDOPA = ',A4,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9019)IBUGG4,ISUBG4,IERRG4
 9019 FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE GRTRSA(PX1,PY1,AX1,AY1,ISUBN0)
C
C     THIS ROUTINE IS A MODIFIED VERSION OF GRTRSD.  IT IS USED
C     ONLY BY THE META FILE DEVICES (I.E., "GENERAL", "GENERAL CODED"
C     AND FOR FUTURE PURPOSES "CGM").  GRTRSA CONVERTS FROM DATAPLOT
C     UNITS TO DEVICE INTEGER UNITS, BUT IT ALSO APPLIES "WINDOW"
C     TRANSFORMATIONS NEEDED BY THE "MULTI-PLOT" AND "WINDOW
C     COORDINATE" COMMANDS.  THE METAFILE DOES NOT SUPPORT ANY
C     PARTICULAR NUMBER OF PICTURE POINTS.  IT DOES HOWEVER NEED
C     TO APPLY THE "WINDOW" TRANSFORMATIONS.
C
C     PURPOSE--TRANSLATE THE STANDARDIZED (0.0 TO 100.0) COORDINATES (PX1,PY1)
C              INTO (INTEGER PICTURE POINT) DEVICE COORDINATES (AX1,AY1)
C     ISUBN0 = NAME OF SUBROUTINE WHICH CALLED GRWRST.
C              (AND THEREBY HAVE WALKBACK INFORMATION).
C     NOTE--THE ONLY VARIABLES IN THE    PLOT CONTROL COMMON
C           THAT ARE USED HEREIN ARE THE ONES IN /RWIND/
C
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--89.2
C     ORIGINAL VERSION--JANUARY   1989.
C     UPDATED         --APRIL     1992.  FIX SOME DEBUG STATEMENTS
C
C-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
C
      CHARACTER*4 ISUBN0
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOPC.INC'
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IERRG4='NO'
C
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'TRSA')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF GRTRSA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ISUBN0
   52 FORMAT('ISUBN0 (NAME OF THE CALLING SUBROUTINE) = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IMANUF,IMODEL
   53 FORMAT('IMANUF,IMODEL = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)NUMHPP,NUMVPP
   54 FORMAT('NUMHPP,NUMVPP = ',I8,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,55)ANUMHP,ANUMVP
   55 FORMAT('ANUMHP,ANUMVP = ',E15.7,E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,56)PX1,PY1
   56 FORMAT('PX1,PY1 = ',E15.7,E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,61)PWXMIN,PWXMAX,PWYMIN,PWYMAX
   61 FORMAT('PWXMIN,PWXMAX,PWYMIN,PWYMAX = ',4E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,69)IBUGG4
   69 FORMAT('IBUGG4 = ',A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               *************************************
C               **  STEP 1--                       **
C               **  CARRY OUT THE TRANSFORMATION.  **
C               *************************************
C
      AX1=PWXMIN+(PX1/100.0)*(PWXMAX-PWXMIN)
      IF(AX1.LE.0.0)AX1=0.0
      IF(AX1.GE.100.)AX1=100.
C
      AY1=PWYMIN+(PY1/100.0)*(PWYMAX-PWYMIN)
      IF(AY1.LE.0.0)AY1=0.0
      IF(AY1.GE.100.)AY1=100.
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
      IF(IBUGG4.EQ.'ON' .OR. ISUBG4.EQ.'TRSA')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF GRTRSA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IMANUF,IMODEL
 9012   FORMAT('IMANUF,IMODEL = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)ANUMHP,ANUMVP,NUMHPP,NUMVPP
 9014   FORMAT('ANUMHP,ANUMVP,NUMHPP,NUMVPP = ',2G15.7,2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)PX1,PY1,AX1,AY1
 9015   FORMAT('PX1,PY1,AX1,AY1   = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)PWXMIN,PWXMAX,PWYMIN,PWYMAX
 9021   FORMAT('PWXMIN,PWXMAX,PWYMIN,PWYMAX = ',4E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9029)IBUGG4,ISUBG4,IERRG4
 9029   FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
 
      SUBROUTINE GRTRSD(PX1,PY1,IX1,IY1,ISUBN0)
C
C     PURPOSE--TRANSLATE THE STANDARDIZED (0.0 TO 100.0) COORDINATES (PX1,PY1)
C              INTO (INTEGER PICTURE POINT) DEVICE COORDINATES (IX1,IY1)
C     ISUBN0 = NAME OF SUBROUTINE WHICH CALLED GRWRST.
C              (AND THEREBY HAVE WALKBACK INFORMATION).
C     NOTE--THE ONLY VARIABLES IN THE    PLOT CONTROL COMMON
C           THAT ARE USED HEREIN ARE THE ONES IN /RWIND/
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     UPDATED   --MARCH     1990. PATCH FOR X11, USE OFFSET VARIABLES
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--83.6
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MAY       1983.
C     UPDATED--SEPTEMBER 1986.
C     UPDATED--MAY 1990.    FOR X11, USE OFFSET VARIABLES
C     UPDATED--JUNE   1990. FIXED BUG WHEN USING OFFSETS
C     UPDATED--OCTOBER 1996.  SUPPORT MICROSOFT QWIN DRIVER
C     UPDATED--OCTOBER 1996.  SUPPORT MICROSOFT QWIN DRIVER
C     UPDATED--MARCH   2002.  SUPPORT SVG DRIVER
C     UPDATED--OCTOBER 2017.  SUPPORT CAIRO DRIVER
C
C-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
C
      CHARACTER*4 ISUBN0
C
C-----COMMON----------------------------------------------------------
C
CCCCC THE FOLLOWING LINE WAS INSERTED MARCH 1989
      INCLUDE 'DPCOPA.INC'
C
      INCLUDE 'DPCOPC.INC'
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCODV.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IERRG4='NO'
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRSD')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF GRTRSD--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ISUBB0,IBUGG4,ISUBN0,IMANUF,IMODEL
   53   FORMAT('ISUBN0,IBUGG4,ISUBN0,IMANUF,IMODEL = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)NUMHPP,NUMVPP,ANUMHP,ANUMVP
   54   FORMAT('NUMHPP,NUMVPP,ANUMHP,ANUMVP = ',2I8,2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,56)PX1,PY1
   56   FORMAT('PX1,PY1 = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,61)PWXMIN,PWXMAX,PWYMIN,PWYMAX
   61   FORMAT('PWXMIN,PWXMAX,PWYMIN,PWYMAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *************************************
C               **  STEP 1--                       **
C               **  CARRY OUT THE TRANSFORMATION.  **
C               *************************************
C
C
C     FOR CAIRO DEVICE WITH EITHER POSTSCRIPT OR PDF MODEL, RETURN
C     REAL VALUES FOR THE COORDINATES.  POSTSCRIPT AND PDF WILL USE
C     THE UNROUNDED VALUES WHILE THE OTHER DEVICES WILL USE THE
C     INTEGER COORDINATES.  COMPUTE BOTH AND LET THE CALLING
C     ROUTINE DECIDE WHICH TO USE.
C
      IF(IMANUF.EQ.'CAIR')THEN
        PWX1=PWXMIN+(PX1/100.0)*(PWXMAX-PWXMIN)
        X1=(PWX1/100.0)*ANUMHP
        PX1=X1
        IX1=INT(X1+0.5)
        IX1=IX1+IOFFSH
        IF(IX1.LE.0)IX1=0
        ITEMP=NUMHPP + IOFFSH
        IF(IX1.GE.ITEMP)IX1=ITEMP-1
        PX1=REAL(IX1)
C
        PWY1=PWYMIN+(PY1/100.0)*(PWYMAX-PWYMIN)
        Y1=(PWY1/100.0)*ANUMVP
        PY1=Y1
        PY1=REAL(NUMVPP) - 1.0 - PY1
        IF(PY1.LE.0.0)PY1=0.0
        ATEMP=REAL(NUMVPP + IOFFSV)
        IF(PY1.GE.ATEMP)PY1=ATEMP-1.0
        IY1=INT(Y1+0.5)
        IY1=IY1+IOFFSV
        IY1=NUMVPP - 1 - IY1
        IF(IY1.LE.0)IY1=0
        ITEMP=NUMVPP + IOFFSV
        IF(IY1.GE.ITEMP)IY1=ITEMP-1
        PY1=REAL(IY1)
        GOTO9000
      ENDIF
C
CCCCC X1=(PX1/100.0)*ANUMHP
      PWX1=PWXMIN+(PX1/100.0)*(PWXMAX-PWXMIN)
      X1=(PWX1/100.0)*ANUMHP
      IX1=INT(X1+0.5)
CCCCC FOLLOWING LINE ADDED MARCH, 1990
      IX1=IX1+IOFFSH
      IF(IX1.LE.0)IX1=0
CCCCC JUNE, 1990.  FOLLOWING LINE MODIFIED.  NEED TO ACCOUNT FOR OFFSET
CCCCC WHEN TEST FOR MAXIMUM POINT.
CCCCC IF(IX1.GE.NUMHPP)IX1=NUMHPP-1
      ITEMP=NUMHPP+IOFFSH
      IF(IX1.GE.ITEMP)IX1=ITEMP-1
C
CCCCC Y1=(PY1/100.0)*ANUMVP
CCCCC NEED TO MODIFY FOLLOWING LINE FOR QWIN DRIVER SINCE IT MEASURES
CCCCC FROM TOP TO BOTTOM.  OCTOBER 1996.
      IF(IMANUF.EQ.'QWIN')THEN
        PWYMNT=100.0-PWYMIN
        PWYMXT=100.0-PWYMAX
        PWY1=PWYMNT+(PY1/100.0)*(PWYMXT-PWYMNT)
      ELSE
        PWY1=PWYMIN+(PY1/100.0)*(PWYMAX-PWYMIN)
      ENDIF
      Y1=(PWY1/100.0)*ANUMVP
      IY1=INT(Y1+0.5)
CCCCC FOLLOWING LINE ADDED MARCH, 1990
      IY1=IY1+IOFFSV
CCCCC IF(IMANUF.EQ.'REGI')IY1=NUMVPP-1-IY1
C  ABOVE LINE MODIFIED FOR X11 DRIVER MARCH, 1990.
C  ABOVE LINE MODIFIED FOR QWIN DRIVER OCTOBER 1996
CCCCC SAME MODIFICATION FOR PNG, JPG (GD) DEVICE  FEBRUARY 2001
CCCCC SAME MODIFICATION FOR SVG DEVICE  MARCH 2002
CCCCC IF(IMANUF.EQ.'REGI'.OR.IMANUF.EQ.'X11 '.OR.IMANUF.EQ.'QWIN')
CCCCC IF(IMANUF.EQ.'REGI'.OR.IMANUF.EQ.'X11 ')
      IF(IMANUF.EQ.'REGI'.OR.IMANUF.EQ.'X11 '.OR.IMANUF.EQ.'GD'.OR.
     &   IMANUF.EQ.'SVG'.OR.IMANUF.EQ.'CAIR')
     &IY1=NUMVPP-1-IY1
      IF(IY1.LE.0)IY1=0
CCCCC JUNE, 1990 FOR OFFSET.
CCCCC IF(IY1.GE.NUMVPP)IY1=NUMVPP-1
      ITEMP=NUMVPP+IOFFSV
      IF(IY1.GE.ITEMP)IY1=ITEMP-1
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRSD')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF GRTRSD--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)PWX1,PWY1,X1,Y1
 9016   FORMAT('PWX1,PWY1,X1,Y1 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)PWXMIN,PWXMAX,PWYMIN,PWYMAX
 9021   FORMAT('PWXMIN,PWXMAX,PWYMIN,PWYMAX = ',4E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9029)IERRG4,IX1,IY1
 9029   FORMAT('IERRG4,IX1,IY1 = ',A4,2X,2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE GRTRST(ICSTR,NCSTR,
     1ICTRA1,NCTRA1,ICTRA2,NCTRA2,NUMTRA,
     1IBUGG4,ISUBG4,IERRG4)
C
C     PURPOSE--APPLY VARIOUS USER-DEFINED
C              TRANSLATIONS
C              TO EACH OUTPUT GRAPHICS LINE (CHARACTER*130)
C              SO AS TO CREATE A NEW OUTPUT GRAPHICS LINE.
C     EXAMPLE--CHANGE ESC FF TO ESC ESC FF
C              SO AS TO CIRCUMVENT SOME NETWORKS
C              "EATING UP" ESCAPES.
C     CAUTION--THE INPUT ARGUMENTS ICSTR AND NCSTR
C              ARE CHANGED WITHIN THIS SUBROUTINE
C              AND THUS ARE ALSO OUTPUT ARGUMENTS.
C     CAUTION--ICSTR IS CHARACTER*130--NOT CHARACTER*132.
C              THE SAME IS TRUE FOR ICSTR2.
C     NOTE--EVERY OCCURRANCE (NOT JUST THE FIRST
C           OCCURRANCE) WILL BE TRANSLATED ON THE
C           INPUT LINE.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--86.6
C     ORIGINAL VERSION--MARCH 1986.
C
C-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
C
      CHARACTER*130 ICSTR
      CHARACTER*30 ICTRA1
      CHARACTER*30 ICTRA2
C
      CHARACTER*4 IBUGG4
      CHARACTER*4 ISUBG4
      CHARACTER*4 IERRG4
C
      CHARACTER*30 ICS1
      CHARACTER*30 ICS2
      CHARACTER*130 ICSTR2
C
      CHARACTER*4 IFOUST
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
      DIMENSION ICTRA1(*)
      DIMENSION NCTRA1(*)
      DIMENSION ICTRA2(*)
      DIMENSION NCTRA2(*)
C
C-----COMMON----------------------------------------------------------
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='GRTR'
      ISUBN2='ST  '
      IERRG4='NO'
      IFOUST='NO'
C
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'TRST')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF GRTRST--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGG4,ISUBG4,IERRG4
   52 FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,61)NCSTR
   61 FORMAT('NCSTR = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,62)(ICSTR(I:I),I=1,100)
   62 FORMAT('ICSTR(I:I) = ',100A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,71)NUMTRA
   71 FORMAT('NUMTRA = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NUMTRA.LE.0)GOTO79
      DO72ITRA=1,NUMTRA
      WRITE(ICOUT,73)ITRA,NCTRA1(ITRA),ICTRA1(ITRA)
   73 FORMAT('ITRA,NCTRA1(ITRA),ICTRA1(ITRA) = ',I8,I8,2X,A30)
      CALL DPWRST('XXX','BUG ')
   72 CONTINUE
      DO75ITRA=1,NUMTRA
      WRITE(ICOUT,76)ITRA,NCTRA2(ITRA),ICTRA2(ITRA)
   76 FORMAT('ITRA,NCTRA2(ITRA),ICTRA2(ITRA) = ',I8,I8,2X,A30)
      CALL DPWRST('XXX','BUG ')
   75 CONTINUE
   79 CONTINUE
   90 CONTINUE
C
C               ***********************************
C               **  STEP 11--                    **
C               **  LOOP THROUGH EACH ELEMENT    **
C               **  IN THE TRANSLATION TABLE.    **
C               ***********************************
C
      IF(NUMTRA.LE.0)GOTO1190
      DO1100ITRA=1,NUMTRA
C
C               ***********************************
C               **  STEP 12--                    **
C               **  FOR THIS GIVEN ELEMENT       **
C               **  IN THE TRANSLATION TABLE,    **
C               **  EXTRACT THE "OLD" SUBSTRING  **
C               ***********************************
C
      NCS1=NCTRA1(ITRA)
      ICS1=ICTRA1(ITRA)
C
C               ***********************************
C               **  STEP 13--                    **
C               **  FOR THIS GIVEN ELEMENT       **
C               **  IN THE TRANSLATION TABLE,    **
C               **  EXTRACT THE "NEW" SUBSTRING  **
C               ***********************************
C
      NCS2=NCTRA2(ITRA)
      ICS2=ICTRA2(ITRA)
C
C               **************************************
C               **  STEP 14--                       **
C               **  APPLY THE DESIRED CHANGE        **
C               **  TO EVERY OCCUURANCE OF THE      **
C               **  OLD STRING IN THE TARGET LINE;  **
C               **  THUS CREATE A NEW LINE.         **
C               **************************************
C
      CALL GRTRS2(ICS1,NCS1,ICS2,NCS2,
     1ICSTR,NCSTR,ICSTR2,NCSTR2,IFOUST,
     1IBUGG4,ISUBG4,IERRG4)
C
C               ******************************************
C               **  STEP 15--                           **
C               **  COPY THE NEWLY-CREATED LINE         **
C               **  BACK ONTO THE ORIGINAL LINE.        **
C               ******************************************
C
      IF(IFOUST.EQ.'NO')GOTO1590
      NCSTR=NCSTR2
      ICSTR=ICSTR2
 1590 CONTINUE
C
C               ***********************************
C               **  STEP 16--                    **
C               **  FINISH THE LOOP              **
C               **  SO AS TO MOVE ONTO THE       **
C               **  NEXT ELEMENT                 **
C               **  OF THE TRANSLATION TABLE,    **
C               ***********************************
 
 1100 CONTINUE
 1190 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
      IF(IBUGG4.EQ.'ON' .OR. ISUBG4.EQ.'TRST')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF GRTRST--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERRG4,NCSTR,NCSTR2,NUMTRA
 9012   FORMAT('IERRG4,NCSTR,NCSTR2,NUMTRA = ',A4,2X,3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9022)(ICSTR(I:I),I=1,100)
 9022   FORMAT('ICSTR(I:I) = ',100A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9042)(ICSTR2(I:I),I=1,100)
 9042   FORMAT('ICSTR2(I:I) = ',100A1)
        CALL DPWRST('XXX','BUG ')
        IF(NUMTRA.GE.1)THEN
          DO9032ITRA=1,NUMTRA
            WRITE(ICOUT,9033)ITRA,NCTRA1(ITRA),ICTRA1(ITRA)
 9033       FORMAT('ITRA,NCTRA1(ITRA),ICTRA1(ITRA) = ',2I8,2X,A30)
            CALL DPWRST('XXX','BUG ')
 9032     CONTINUE
          DO9035ITRA=1,NUMTRA
            WRITE(ICOUT,9036)ITRA,NCTRA2(ITRA),ICTRA2(ITRA)
 9036       FORMAT('ITRA,NCTRA2(ITRA),ICTRA2(ITRA) = ',2I8,2X,A30)
            CALL DPWRST('XXX','BUG ')
 9035     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE GRTRS2(ICS1,NCS1,ICS2,NCS2,
     1ICSTR,NUMCST,ICSTR2,NUMCS2,IFOUST,
     1IBUGG4,ISUBG4,IERRG4)
C
C     PURPOSE--APPLY A SINGLE USER-DEFINED
C              TRANSLATION
C              TO EACH OUTPUT GRAPHICS LINE
C              SO AS TO CREATE A NEW OUTPUT GRAPHICS LINE.
C              IN PARTICULAR, SCAN ICSTR(.) (CHARACTER*130)
C              FOR EVERY OCCURRANCE
C              OF THE STRING IN ICS1(.).
C              IF FOUND, THEN FORM THE NEW LINE ICSTR2(.)
C              WHICH IS THE SAME AS ICSTR(.) EXCEPT
C              EVERY ICS1(.) HAS BEEN CHANGED TO ICS2(.).
C              IF NOT FOUND, THEN ICSTR2(.) = ICSTR(.)
C     EXAMPLE--CHANGE ESC FF TO ESC ESC FF
C              SO AS TO CIRCUMVENT SOME NETWORKS
C              "EATING UP" ESCAPES.
C     CAUTION--ICSTR IS CHARACTER*130--NOT CHARACTER*132.
C              THE SAME IS TRUE FOR ICSTR2.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--86.6
C     ORIGINAL VERSION--MARCH 1986.
C
C-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
C
      CHARACTER*30 ICS1
      CHARACTER*30 ICS2
      CHARACTER*130 ICSTR
      CHARACTER*130 ICSTR2
C
      CHARACTER*4 IFOUST
C
      CHARACTER*4 IBUGG4
      CHARACTER*4 ISUBG4
      CHARACTER*4 IERRG4
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
CCCCC CHARACTER*4 ISTEPN
C
C-----COMMON----------------------------------------------------------
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='GRTR'
      ISUBN2='S2  '
      IERRG4='NO'
      IFOUST='NO'
C
      ICLIM1=1
      ICLIM2=130
C
      JPIM1=(-999)
C
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'TRS2')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF GRTRS2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGG4,ISUBG4,IERRG4
   52 FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,61)NUMCST
   61 FORMAT('NUMCST = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,62)(ICSTR(I:I),I=1,100)
   62 FORMAT('ICSTR(I:I) = ',100A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,71)NCS1,ICS1
   71 FORMAT('NCS1,ICS1 = ',I8,2X,A30)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,72)NCS2,ICS2
   72 FORMAT('NCS2,ICS2 = ',I8,2X,A30)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,81)NUMCS2
   81 FORMAT('NUMCS2 = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,82)(ICSTR2(I:I),I=1,100)
   82 FORMAT('ICSTR2(I:I) = ',100A1)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               *****************************
C               **  STEP 11--              **
C               **  COPY THE OLD LINE      **
C               **  TO THE NEW LINE.       **
C               *****************************
C
      NUMCS2=NUMCST
      ICSTR2=ICSTR
C
C               *******************************
C               **  STEP 21--                **
C               **  TREAT THE CASE WHEN      **
C               **  STRING 1 IS NULL         **
C               **  (THAT IS, NCS1 <=  0)  **
C               *******************************
C
      IF(NCS1.LE.0)GOTO2100
      GOTO2900
C
 2100 CONTINUE
CCCCC WRITE(ICOUT,774)J,K,NUMCST,JPIM1,NCS1,L,IMIN,IMAX
CC774 FORMAT('FROM 2100--J,K,NUMCST,JPIM1,NCS1,L,IMIN,IMAX = ',8I8)
CCCCC CALL DPWRST('XXX','BUG ')
      IFOUST='YES'
C
C     STEP 21.1--COPY STRING 2 TO THE BEGINNING OF LINE 1
C
      K=0
C
      IMIN=ICLIM1
      IMAX=ICLIM1+NCS2-1
      IF(IMIN.GT.IMAX)GOTO2129
      L=0
      DO2120I=IMIN,IMAX
      K=K+1
      L=L+1
      ICSTR2(K:K)=ICS2(L:L)
CCCCC WRITE(ICOUT,777)I,K,ICSTR2(K:K),IMIN,IMAX
CCCCC CALL DPWRST('XXX','BUG ')
 2120 CONTINUE
 2129 CONTINUE
C
C     STEP 21.2--PUSH (COPY) THE OLD LINE TO THE RIGHT
C
      IMIN=ICLIM1+NCS2
      IMAX=ICLIM2
      IF(IMIN.GT.IMAX)GOTO2139
      DO2130I=IMIN,IMAX
      I2=I-NCS2
      IF(I2.GT.NUMCST)GOTO2139
      K=K+1
      ICSTR2(K:K)=ICSTR(I2:I2)
CCCCC WRITE(ICOUT,777)I,K,ICSTR2(K:K),IMIN,IMAX
CCCCC CALL DPWRST('XXX','BUG ')
 2130 CONTINUE
 2139 CONTINUE
C
      NUMCS2=K
      GOTO9000
C
 2900 CONTINUE
C
C               **********************************
C               **  STEP 31--                   **
C               **  TREAT THE CASE WHEN         **
C               **  THE OLD STRING IS NON-NULL  **
C               **  (THAT IS, NCS1 >= 1)      **
C               **********************************
C
      J=0
      K=0
 3100 CONTINUE
CCCCC WRITE(ICOUT,776)
CC776 FORMAT('-------------------------------')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,780)J,K,NUMCST,JPIM1,NCS1
CC780 FORMAT('AFTER 3100--J,K,NUMCST,JPIM1,NCS1 = ',5I8)
CCCCC CALL DPWRST('XXX','BUG ')
      J=J+1
      IF(J.GT.NUMCST)GOTO3900
      IF(J.GT.ICLIM2)GOTO3900
C
      IF(J.LT.ICLIM1)GOTO3210
      DO3200I=1,NCS1
      JPIM1=J+(I-1)
CCCCC IF(ICS1(I).EQ.IMASK)GOTO3200
      IF(ICSTR(JPIM1:JPIM1).EQ.ICS1(I:I))GOTO3200
      GOTO3210
 3200 CONTINUE
CCCCC WRITE(ICOUT,781)J,K,NUMCST,JPIM1,NCS1
CC781 FORMAT('AFTER 3200 C--J,K,NUMCST,JPIM1,NCS1 = ',5I8)
CCCCC CALL DPWRST('XXX','BUG ')
      GOTO3390
C
 3210 CONTINUE
CCCCC WRITE(ICOUT,779)J,K,NUMCST,JPIM1,NCS1
CC779 FORMAT('AFTER 3210--J,K,NUMCST,JPIM1,NCS1 = ',5I8)
CCCCC CALL DPWRST('XXX','BUG ')
      K=K+1
      ICSTR2(K:K)=ICSTR(J:J)
      GOTO3100
C
 3390 CONTINUE
CCCCC WRITE(ICOUT,782)J,K,NUMCST,JPIM1,NCS1
CC782 FORMAT('AFTER 3390 C--J,K,NUMCST,JPIM1,NCS1 = ',5I8)
CCCCC CALL DPWRST('XXX','BUG ')
C
      IFOUST='YES'
      IF(NCS2.LE.0)GOTO3350
      DO3300I=1,NCS2
      K=K+1
      ICSTR2(K:K)=ICS2(I:I)
 3300 CONTINUE
 3350 CONTINUE
CCCCC WRITE(ICOUT,783)J,K,NUMCST,JPIM1,NCS1
CC783 FORMAT('AFTER 3350 C--J,K,NUMCST,JPIM1,NCS1 = ',5I8)
CCCCC CALL DPWRST('XXX','BUG ')
      J=JPIM1
      GOTO3100
C
 3400 CONTINUE
CCCCC WRITE(ICOUT,784)J,K,NUMCST,JPIM1,NCS1
CC784 FORMAT('AFTER 3400 C--J,K,NUMCST,JPIM1,NCS1 = ',5I8)
CCCCC CALL DPWRST('XXX','BUG ')
      J=J+1
      IF(J.GT.NUMCST)GOTO3900
      K=K+1
      ICSTR2(K:K)=ICSTR(J:J)
      GOTO3400
C
 3900 CONTINUE
      NUMCS2=K
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'TRS2')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF GRTRS2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGG4,ISUBG4,IERRG4
 9012 FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9021)NUMCST
 9021 FORMAT('NUMCST = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9022)(ICSTR(I:I),I=1,100)
 9022 FORMAT('ICSTR(I:I) = ',100A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9031)NCS1,ICS1
 9031 FORMAT('NCS1,ICS1 = ',I8,2X,A30)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9032)NCS2,ICS2
 9032 FORMAT('NCS2,ICS2 = ',I8,2X,A30)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9041)NUMCS2
 9041 FORMAT('NUMCS2 = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9042)(ICSTR2(I:I),I=1,100)
 9042 FORMAT('ICSTR2(I:I) = ',100A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9051)ICLIM1,ICLIM2
 9051 FORMAT('ICLIM1,ICLIM2 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9052)IFOUST
 9052 FORMAT('IFOUST = ',A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE GRTRTK(ICSTR,NCSTR)
C
C     PURPOSE--TRANSLATE 1 LINE OF TEKTRONIX 4014 ASCII DIRECTIVES
C              INTO A SERIES OF DUMMY FORTRAN CALLS BY WHICH
C              OTHER NON-TEKTRONIX GRAPHICS DEVICES MAY BE DRIVEN
C              (E.G., CALCOMP, HP, CHROMATICS, ETC.)
C     INPUT --ICSTR  = THE CHARACTER*130 STRING
C                      CONTAINING THE CONTROL STRING.
C             NCSTR  = THE NUMBER OF ELEMENTS IN ICSTR(.:.)
C                      TO BE PROCESSED.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--83.6
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--SEPTEMBER 1984.
C
C-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
C
      CHARACTER*130 ICSTR
C
      CHARACTER*1 ICTEXT
      CHARACTER*1 IC1
      CHARACTER*1 IC2
      CHARACTER*4 ILINTY
      CHARACTER*5 ICBYTE
      CHARACTER*4 IOP
C
      DIMENSION ICTEXT(130)
C
      DIMENSION X(100)
      DIMENSION Y(100)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCONP.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IERRG4='NO'
C
      ISIZE=(-999)
      ICBYTE='     '
      JP4=(-999)
C
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'TRTK')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF GRTRTK--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)NCSTR
   54 FORMAT('NCSTR = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NCSTR.LE.0)GOTO57
      DO55I=1,NCSTR
CCCCC IASCNE=ICHAR(ICSTR(I:I))
      CALL DPCOAN(ICSTR(I:I),IASCNE)
      WRITE(ICOUT,56)I,ICSTR(I:I),IASCNE
   56 FORMAT('I,ICSTR(I:I),IASCNE = ',I8,2X,A1,I8)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   57 CONTINUE
      WRITE(ICOUT,69)IBUGG4,ISUBG4,IERRG4
   69 FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
      I=0
C
 1000 CONTINUE
      I=I+1
      IF(I.GT.NCSTR)GOTO9000
      IC1=ICSTR(I:I)
      IP1=I+1
      IC2=ICSTR(2:2)
C
      IF(IC1.EQ.IESCC.AND.IC2.EQ.IFFC)GOTO1100
C
      IF(IC1.EQ.ISYNC)GOTO1200
C
      IF(IC1.EQ.IESCC.AND.IC2.EQ.IBELC)GOTO1300
C
      IF(IC1.EQ.IESCC.AND.IC2.EQ.'`')GOTO1400
      IF(IC1.EQ.IESCC.AND.IC2.EQ.'a')GOTO1400
      IF(IC1.EQ.IESCC.AND.IC2.EQ.'b')GOTO1400
      IF(IC1.EQ.IESCC.AND.IC2.EQ.'c')GOTO1400
C
      IF(IC1.EQ.IESCC.AND.IC2.EQ.';')GOTO1500
      IF(IC1.EQ.IESCC.AND.IC2.EQ.':')GOTO1500
      IF(IC1.EQ.IESCC.AND.IC2.EQ.'9')GOTO1500
      IF(IC1.EQ.IESCC.AND.IC2.EQ.'8')GOTO1500
C
      IF(IC1.EQ.IGSC)GOTO1600
C
      IF(IC1.EQ.IUSC)GOTO1700
C
      GOTO9000
C
C
C               ***********************************
C               **  STEP 1--                     **
C               **  TREAT THE ERASE SCREEN CASE  **
C               ***********************************
C
 1100 CONTINUE
CCCCC CALL ERASESCREEN
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRTK')WRITE(ICOUT,1101)
 1101 FORMAT('ERASE SCREEN')
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRTK')CALL DPWRST('XXX','BUG ')
      I=I+1
      GOTO1000
C
C               *************************************
C               **  STEP 2--                       **
C               **  TREAT THE SEND NULL LINE CASE  **
C               *************************************
C
 1200 CONTINUE
      I1=I
      I2=I
      DO1210J=I,NCSTR
      IF(ICSTR(J:J).EQ.ISYNC)I2=J
      IF(ICSTR(J:J).EQ.ISYNC)GOTO1210
      GOTO1211
 1210 CONTINUE
 1211 CONTINUE
      NUMNUL=I2-I1+1
CCCCC CALL SENDNULLLINE(CONSISTING OF NUMNUL CHARACTERS)
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRTK')WRITE(ICOUT,1212)NUMNUL
 1212 FORMAT('SEND A NULL LINE CONSISTING OF ',I8,'CHARACTERS')
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRTK')CALL DPWRST('XXX','BUG ')
      I=I2
      GOTO1000
C
C               *****************
C               **  STEP 3--   **
C               **  RING BELL  **
C               *****************
C
 1300 CONTINUE
CCCCC CALL RINGBELL
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRTK')WRITE(ICOUT,1301)
 1301 FORMAT('RING BELL')
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRTK')CALL DPWRST('XXX','BUG ')
      I=I+1
      GOTO1000
C
C               ************************************
C               **  STEP 4--                      **
C               **  TREAT THE SET LINE TYPE CASE  **
C               ************************************
C
C
 1400 CONTINUE
CCCCC IF(IC2.EQ.'`')CALL SETLINE(SOLID)
CCCCC IF(IC2.EQ.'a')CALL SETLINE(DOTTED)
CCCCC IF(IC2.EQ.'b')CALL SETLINE(DASHED)
CCCCC IF(IC2.EQ.'c')CALL SETLINE(DOT-DASHED)
      ILINTY='SOLI'
      IF(IC2.EQ.'`')ILINTY='SOLI'
      IF(IC2.EQ.'a')ILINTY='DOTT'
      IF(IC2.EQ.'b')ILINTY='DASH'
      IF(IC2.EQ.'c')ILINTY='DODA'
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRTK')WRITE(ICOUT,1411)ILINTY
 1411 FORMAT('SET LINE TYPE TO ',A4)
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRTK')CALL DPWRST('XXX','BUG ')
      I=I+1
      GOTO1000
C
C               *****************************************
C               **  STEP 5--                           **
C               **  TREAT THE SET CHARACTER SIZE CASE  **
C               *****************************************
C
 1500 CONTINUE
CCCCC IF(IC2.EQ.';')CALL SETCHARSIZE(SMALLEST)
CCCCC IF(IC2.EQ.':')CALL SETCHARSIZE(NEXTTOSMALLEST)
CCCCC IF(IC2.EQ.'9')CALL SETCHARSIZE(NEXTTOLARGEEST)
CCCCC IF(IC2.EQ.'8')CALL SETCHARSIZE(LARGEST)
      ICHASZ=1
      IF(IC2.EQ.';')ISIZE=1
      IF(IC2.EQ.':')ISIZE=2
      IF(IC2.EQ.'9')ISIZE=3
      IF(IC2.EQ.'8')ISIZE=4
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRTK')WRITE(ICOUT,1511)ISIZE
 1511 FORMAT('SET CHARACTER SIZE TO ',I8)
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRTK')CALL DPWRST('XXX','BUG ')
      I=I+1
      GOTO1000
C
C               **********************************************
C               **  STEP 6--                                **
C               **  TREAT THE GRAPHICS MODE MOVE/DRAW CASE  **
C               **********************************************
C
 1600 CONTINUE
      IOP='MOVE'
      IP1=I+1
      NPLOTP=0
      DO1610J=IP1,NCSTR,5
      JP4=J+4
      ICBYTE(1:5)=ICSTR(J:JP4)
CCCCC IB1=ICHAR(ICBYTE(1:1))
      CALL DPCOAN(ICBYTE(1:1),IB1)
CCCCC IB2=ICHAR(ICBYTE(2:2))
      CALL DPCOAN(ICBYTE(2:2),IB2)
CCCCC IB3=ICHAR(ICBYTE(3:3))
      CALL DPCOAN(ICBYTE(3:3),IB3)
CCCCC IB4=ICHAR(ICBYTE(4:4))
      CALL DPCOAN(ICBYTE(4:4),IB4)
CCCCC IB5=ICHAR(ICBYTE(5:5))
      CALL DPCOAN(ICBYTE(5:5),IB5)
C     A TEKTRONIX 4014 (ENHANCED GRAPHICS)
C     HAS A VISIBLE SCREEN CONSISTING OF
C     4096 HORIZONTAL PICTURE POINTS, AND
C     3124 VERTICAL PICTURE POINTS,
C     (WITH (0,0) AT THE BOTTOM LEFT), THEREFORE,
C     IXPP BELOW WILL RANGE FROM 0 TO 4095, AND
C     IYPP BELOW WILL RANGE FROM 0 TO 3123.
C     IXP BELOW WILL RANGE FROM 0 TO 1.
C     IYP BELOW WILL RANGE FROM 0 TO 1.
C
      IXPP=(IB4-32)*32+(IB5-64)
      IYPP=(IB1-32)*32+(IB3-96)
      IXPP=4*IXPP
      IYPP=4*IYPP
      XPP=IXPP
      YPP=IYPP
      XP=XPP/4095.
      YP=YPP/3123.
C
C     THE FOLLOWING SETTINGS ARE FOR A TYPICAL
C     ALPHANUMERIC TERMINAL SCREEN
C     (80 COLUMNS WIDE BY 24 ROWS DEEP
C     WITH (1,1) AT BOTTOM LEFT).
C
      XLEFT=1.00
      XRIGHT=80.00
      YBOT=1.00
      YTOP=24.00
C
      X2=(XLEFT-0.5)+((XRIGHT+0.5)-(XLEFT-0.5))*XP
      Y2=(YBOT-0.5)+((YTOP+0.5)-(YBOT-0.5))*YP
      IX2=INT(X2+0.5)
      IY2=INT(Y2+0.5)
C
CCCCC IF(IOP.EQ.'MOVE')CALL PENUP
CCCCC IF(IOP.EQ.'MOVE')CALL MOVETO(IX2,IY2)
CCCCC IF(IOP.EQ.'DRAW')CALL PENDOWN
CCCCC IF(IOP.EQ.'DRAW')CALL DRAWTO(IX2,IY2)
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRTK'.AND.
     1   IOP.EQ.'MOVE')THEN
        WRITE(ICOUT,1611)IX2,IY2
 1611   FORMAT('PEN UP   AND MOVE TO ',I8,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1612)IX2,IY2
 1612   FORMAT('PEN DOWN AND DRAW TO ',I8,I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IOP='DRAW'
      NPLOTP=NPLOTP+1
      X(NPLOTP)=IX2
      Y(NPLOTP)=IY2
 1610 CONTINUE
CCCCC IF(NPLOTP.GE.2)CALL PLOT(X,Y,NPLOTP)
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRTK')THEN
        WRITE(ICOUT,1621)NPLOTP
 1621   FORMAT('PLOT',I8,' POINTS FROM 2 VECTORS X (HOR) AND Y (VER)')
        CALL DPWRST('XXX','BUG ')
C
        DO1625J=1,NPLOTP
          WRITE(ICOUT,1626)J,X(J),Y(J)
 1626     FORMAT('      J,X(J),Y(J) = ',I8,2F15.7)
          CALL DPWRST('XXX','BUG ')
 1625   CONTINUE
      ENDIF
C
      I=JP4
      GOTO1000
C
C               **********************************************************
C               **  STEP 7--                                            **
C               **  TREAT THE ALPHANUMERIC MODE WRITE TEXT STRING CASE  **
C               **********************************************************
C
 1700 CONTINUE
      IP1=I+1
      NCTEXT=0
      DO1710J=IP1,NCSTR
        IF(ICSTR(J:J).EQ.IGSC)GOTO1790
        ICTEXT(NCTEXT)=ICSTR(J:J)
        NCTEXT=NCTEXT+1
 1710 CONTINUE
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRTK')THEN
        WRITE(ICOUT,1711)NCTEXT
 1711   FORMAT('WRITE THE ',I8,' TEXT STRING--',80A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
CCCCC CALL WRITETEXT(ICTEXT,NCTEXT)
 1790 CONTINUE
      I=I+NCTEXT
      GOTO1000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG4.EQ.'ON' .OR. ISUBG4.EQ.'TRTK')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF GRTRTK--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NCSTR,IGUNIT,IMANUF,IERRG4
 9013   FORMAT('NCSTR,IGUNIT,IMANUF,IERRG4 = ',2I8,2(2X,A4))
        CALL DPWRST('XXX','BUG ')
        IF(NCSTR.GE.1)THEN
          DO9015I=1,NCSTR
CCCCC       IASCNE=ICHAR(ICSTR(I:I))
            CALL DPCOAN(ICSTR(I:I),IASCNE)
            WRITE(ICOUT,9016)I,ICSTR(I:I),IASCNE
 9016       FORMAT('I,ICSTR(I:I),IASCNE = ',I8,2X,A1,I8)
            CALL DPWRST('XXX','BUG ')
 9015     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE GRWRTE(PX1,PY1,ICTEXT,NCTEXT,
     1                  IPATTT,IFONT,ICASE,IJUST,IDIR,ANGLE,IFILLT,
     1                  ICOL,ICOLR,ICOLG,ICOLB,IRGBFL,
     1                  JPATTT,JFONT,JCASE,JJUST,JDIR,ANGLE2,
     1                  JFILLT,JCOL,
     1                  PHEIGH,PWIDTH,PVEGAP,PHOGAP,PTHICK,
     1                  JSIZE,
     1                  JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1                  PHEIG2,PWIDT2,PVEGA2,PHOGA2,
     1                  JTHICK,PTHIC2,
     1                  PXLEC,PXLECG,PYLEC,PYLECG,
     1                  ISYMBL,ISPAC,
     1                  IMPSW2,AMPSCH,AMPSCW,
     1                  PX99,PY99)
C
C     PURPOSE--FOR A SPECIFIC GRAPHICS DEVICE,
C              GO TO THE POINT (PX1,PY1) AND WRITE
C              OUT THE TEXT STRING CONTAINED IN THE
C              CHARACTER VECTOR ICTEXT(.),
C              WHICH CONSISTS OF    NCTEXT    CHARACTERS.
C     NOTE--PX1 AND PY1 ARE IN STANDARDIZED COORDINATES
C           THAT IS, EACH IS 0.0 TO 100.0.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--83.6
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MAY       1983.
C     UPDATED--JULY        1996. FORCE LAHEY DEVICE TO SOFTWARE CHAR.
C     UPDATED--SEPTEMBER   1999. ARGUMENT LIST TO GRWRTG
C     UPDATED--OCTOBER     2020. SUPPORT FOR RGB COLOR
C
C-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
C
      CHARACTER*4 ICTEXT
      CHARACTER*4 IPATTT
      CHARACTER*4 IFONT
      CHARACTER*4 ICASE
      CHARACTER*4 IJUST
      CHARACTER*4 IDIR
      CHARACTER*4 IFILLT
      CHARACTER*4 ICOL
      CHARACTER*24 ISYMBL
      CHARACTER*4 ISPAC
      CHARACTER*4 IMPSW2
C
      CHARACTER*4 IFONT2
C
      DIMENSION ICTEXT(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
C
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'WRTE')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF GRWRTE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)PX1,PY1,PX99,PY99,NCTEXT
   53   FORMAT('PX1,PY1,PX99,PY99,NCTEXT = ',4G15.7,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,56)(ICTEXT(I),I=1,MIN(NCTEXT,25))
   56   FORMAT('(ICTEXT(I),I=1,NCTEXT) = ',25A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,57)JSIZE,JPATTT,JFONT,JCASE,JJUST,JDIR
   57   FORMAT('JSIZE,JPATTT,JFONT,JCASE,JJUST,JDIR = ',6I6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,59)IPATTT,IFONT,ICASE,IJUST,IDIR
   59   FORMAT('IPATTT,IFONT,ICASE,IJUST,IDIR= ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,64)ANGLE,ANGLE2
   64   FORMAT('ANGLE,ANGLE2= ',2E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,65)IFILLT,JFILLT,ICOL,JCOL
   65   FORMAT('IFILLT,JFILLT,ICOL,JCOL= ',2(A4,I8))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,67)PHEIGH,JHEIG2,PHEIG2
   67   FORMAT('PHEIGH,JHEIG2,PHEIG2= ',G15.7,I8,G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,68)PWIDTH,JWIDT2,PWIDT2
   68   FORMAT('PWIDTH,JWIDT2,PWIDT2= ',G15.7,I8,G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,69)PVEGAP,JVEGA2,PVEGA2
   69   FORMAT('PVEGAP,JVEGA2,PVEGA2= ',G15.7,I8,G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,70)PHOGAP,JHOGA2,PHOGA2
   70   FORMAT('PHOGAP,JHOGA2,PHOGA2= ',G15.7,I8,F15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)PTHICK,JTHICK,PTHIC2
   71   FORMAT('PTHICK,JTHICK,PTHIC2= ',G15.7,I8,G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,73)PXLEC,PXLECG,PYLEC,PYLECG
   73   FORMAT('PXLEC,PXLECG,PYLEC,PYLECG = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,75)ISYMBL,ISPAC,IGFONT
   75   FORMAT('ISYMBL,ISPAC,IGFONT = ',A24,2(2X,A4))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,77)ICOLR,ICOLG,ICOLB,IRGBFL
   77   FORMAT('ICOLR,ICOLG,ICOLB,IRGBFL = ',4I5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,79)IBUGG4,ISUBG4,IERRG4
   79   FORMAT('IBUGG4,ISUBG4,IERRG4 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ********************************************
C               **  STEP 1--                              **
C               **  BRANCH ACCORDING TO THE DIRECTION     **
C               **  AND THE FONT                          **
C               ********************************************
C
CCCCC FOLLOWING SECTION MODIFIED JULY 1996.
      IF(IGFONT.EQ.'OFF')THEN
C
        IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'WRTE')THEN
          WRITE(ICOUT,401)IGFONT,IFONT,IDIR
  401     FORMAT('AT 401: IGFONT,IFONT,IDIR = ',2(A4,2X),A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(IFONT.NE.'TEKT')GOTO1300
        IF(IDIR.EQ.'HORI')GOTO1100
        IF(IDIR.EQ.'VERT')GOTO1200
        GOTO1300
      ELSE
C
        IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'WRTE')THEN
          WRITE(ICOUT,501)IGFONT,IDIR
  501     FORMAT('AT 501: IGFONT,IDIR = ',A4,2X,A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(IGFONT.NE.'TEKT')GOTO1300
        IF(IDIR.EQ.'HORI')GOTO1100
        IF(IDIR.EQ.'VERT')GOTO1200
        GOTO1300
      ENDIF
C
C               **************************************
C               **  STEP 11--                       **
C               **  TREAT THE HORIZONTAL DIRECTION  **
C               **************************************
C
 1100 CONTINUE
      CALL GRWRTH(PX1,PY1,ICTEXT,NCTEXT,
     1            IPATTT,IFONT,ICASE,IJUST,IDIR,ANGLE,IFILLT,
     1            ICOL,ICOLR,ICOLG,ICOLB,IRGBFL,
     1            JPATTT,JFONT,JCASE,JJUST,JDIR,ANGLE2,JFILLT,JCOL,
     1            PHEIGH,PWIDTH,PVEGAP,PHOGAP,PTHICK,
     1            JSIZE,
     1            JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1            PHEIG2,PWIDT2,PVEGA2,PHOGA2,
     1            JTHICK,PTHIC2,
     1            PXLEC,PXLECG,PYLEC,PYLECG,
     1            ISYMBL,ISPAC,
     1            PX99,PY99)
      GOTO9000
C
C               ************************************
C               **  STEP 12--                     **
C               **  TREAT THE VERTICAL DIRECTION  **
C               ************************************
C
 1200 CONTINUE
      CALL GRWRTV(PX1,PY1,ICTEXT,NCTEXT,
     1            IPATTT,IFONT,ICASE,IJUST,IDIR,ANGLE,IFILLT,
     1            ICOL,ICOLR,ICOLG,ICOLB,IRGBFL,
     1            JPATTT,JFONT,JCASE,JJUST,JDIR,ANGLE2,JFILLT,JCOL,
     1            PHEIGH,PWIDTH,PVEGAP,PHOGAP,PTHICK,
     1            JSIZE,
     1            JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1            PHEIG2,PWIDT2,PVEGA2,PHOGA2,
     1            JTHICK,PTHIC2,
     1            PXLEC,PXLECG,PYLEC,PYLECG,
     1            ISYMBL,ISPAC,
     1            PX99,PY99)
      GOTO9000
C
C               ************************************
C               **  STEP 13--                     **
C               **  TREAT THE GENERAL DIRECTION   **
C               ************************************
C
 1300 CONTINUE
      IFONT2=IFONT
      IF(IFONT.EQ.'TEKT')IFONT2='SIMP'
      CALL GRWRTG(PX1,PY1,ICTEXT,NCTEXT,
     1            IPATTT,IFONT2,ICASE,IJUST,IDIR,ANGLE,IFILLT,
     1            ICOL,ICOLR,ICOLG,ICOLB,IRGBFL,
     1            JPATTT,JFONT,JCASE,JJUST,JDIR,ANGLE2,JFILLT,JCOL,
     1            PHEIGH,PWIDTH,PVEGAP,PHOGAP,PTHICK,
     1            JSIZE,
     1            JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1            PHEIG2,PWIDT2,PVEGA2,PHOGA2,
     1            JTHICK,PTHIC2,
     1            PXLEC,PXLECG,PYLEC,PYLECG,
     1            ISYMBL,ISPAC,
     1            IMPSW2,AMPSCH,AMPSCW,
     1            PX99,PY99)
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'WRTE')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF GRWRTE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)PX1,PY1,PX99,PY99
 9013   FORMAT('PX1,PY1,PX99,PY99 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE GRWRTG(PX1,PY1,ICTEXT,NCTEXT,
     1                  IPATT,IFONT,ICASE,IJUST,IDIR,ANGLE,IFILL,
     1                  ICOL,ICOLR,ICOLG,ICOLB,IRGBFL,
     1                  JPATT,JFONT,JCASE,JJUST,JDIR,ANGLE2,JFILL,JCOL,
     1                  PHEIGH,PWIDTH,PVEGAP,PHOGAP,PTHICK,
     1                  JSIZE,
     1                  JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1                  PHEIG2,PWIDT2,PVEGA2,PHOGA2,
     1                  JTHICK,PTHIC2,
     1                  PXLEC,PXLECG,PYLEC,PYLECG,
     1                  ISYMBL,ISPAC,
     1                  IMPSW2,AMPSCH,AMPSCW,
     1                  PX99,PY99)
C
C     PURPOSE--FOR A SPECIFIC GRAPHICS DEVICE, AND FOR A GENERAL
C              (SOFTWARE-GENERATED HERSHEY) FONT, GO TO THE POINT
C              (PX1,PY1) AND WRITE OUT THE TEXT STRING (IN A GENERAL
C              DIRECTION) CONTAINED IN THE CHARACTER VECTOR ICTEXT(.),
C              WHICH CONSISTS OF NCTEXT CHARACTERS.
C     NOTE--PX1 AND PY1 ARE IN STANDARDIZED COORDINATES
C           THAT IS, EACH IS 0.0 TO 100.0.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--83.6
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MAY       1983.
C     UPDATED         --SEPTEMBER    1997. SUPPORT MULTIPLOT SCALE FACTOR
C     UPDATED         --OCTOBER      2020. SUPPORT FOR RGB COLOR
C
C-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
C
      CHARACTER*4 ICTEXT
C
      CHARACTER*4 IPATT
      CHARACTER*4 IFONT
      CHARACTER*4 ICASE
      CHARACTER*4 IJUST
      CHARACTER*4 IDIR
      CHARACTER*4 IFILL
      CHARACTER*4 ICOL
      CHARACTER*24 ISYMBL
      CHARACTER*4 ISPAC
      CHARACTER*4 IMPSW2
      CHARACTER*4 IFOUND
      CHARACTER*4 IBUGD2
      CHARACTER*4 IERROR
      CHARACTER*4 ISUBN0
C
      CHARACTER*4 ISTRIN
      CHARACTER*130 ICSTR
C
      DIMENSION ICTEXT(*)
      DIMENSION ISTRIN(130)
C
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
CCCCC ADD FOLLOWING LINE NOVEMBER 1994.
      ISUBN0='WRTG'
      IF(IBUGG4.EQ.'ON' .OR. ISUBG4.EQ.'WRTG')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF GRWRTG--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)PX1,PY1,PX99,PY99
   53   FORMAT('PX1,PY1,PX99,PY99 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)NCTEXT,JSIZE,JFONT,JCASE,JJUST
   54   FORMAT('NCTEXT,JSIZE,JFONT,JCASE,JJUST = ',5I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)JDIR,JFILL,JCOL,ICOLR,ICOLG,ICOLB,IRGBFL
   55   FORMAT('JDIR,JFILL,ICOLR,ICOLG,ICOLB,IRGBFL = ',6I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,56)(ICTEXT(I),I=1,NCTEXT)
   56   FORMAT('(ICTEXT(I),I=1,NCTEXT) = ',25A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,58)IPATT,IFONT,ICASE,IJUST
   58   FORMAT('IPATT,IFONT,ICASE,IJUST = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,59)IDIR,IFILL,ICOL
   59   FORMAT('IDIR,IFILL,IFILL = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,64)ANGLE,ANGLE2
   64   FORMAT('ANGLE,ANGLE2= ',2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,67)PHEIGH,JHEIG2,PHEIG2
   67   FORMAT('PHEIGH,JHEIG2,PHEIG2= ',G15.7,I8,G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,68)PWIDTH,JWIDT2,PWIDT2
   68   FORMAT('PWIDTH,JWIDT2,PWIDT2= ',G15.7,I8,G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,69)PVEGAP,JVEGA2,PVEGA2
   69   FORMAT('PVEGAP,JVEGA2,PVEGA2= ',G15.7,I8,G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,70)PHOGAP,JHOGA2,PHOGA2
   70   FORMAT('PHOGAP,JHOGA2,PHOGA2= ',G15.7,I8,G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)PTHICK,JTHICK,PTHIC2
   71   FORMAT('PTHICK,JTHICK,PTHIC2= ',G15.7,I8,G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,73)PXLEC,PXLECG,PYLEC,PYLECG
   73   FORMAT('PXLEC,PXLECG,PYLEC,PYLECG = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,75)ISYMBL,ISPAC
   75   FORMAT('ISYMBL,ISPAC = ',A24,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,79)IBUGG4,ISUBG4,IERRG4
   79   FORMAT('IBUGG4,ISUBG4,IERRG4 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ********************************************
C               **  STEP 1--                              **
C               **  BRANCH ACCORDING TO THE MANUFACTURER  **
C               **  AND THE MODEL                         **
C               ********************************************
C
      IF(IMANUF.EQ.'GENE')GOTO1030
      GOTO1100
C
C
 1030 CONTINUE
      IF(IFNTSW.EQ.'OFF')GOTO1100
      IF(IMODEL.EQ.'CODE')GOTO3200
      GOTO3100
C
C               *************************************************
C               **  STEP 11--                                  **
C               **  TREAT THE GENERAL MODEL                    **
C               **  (TEKTRONIX, HP, DEVICE-INDEPENDENT, ETC.)  **
C               *************************************************
C
 1100 CONTINUE
      NUMCHA=NCTEXT
      DO1110I=1,NUMCHA
        ISTRIN(I)=ICTEXT(I)
 1110 CONTINUE
      X0=PX1
      Y0=PY1
C
      HEIGHT=PHEIGH+PVEGAP
      WIDTH=PWIDTH+PHOGAP
C
      IBUGD2=IBUGG4
      HMAX=100.0
      VMAX=100.0
      AMAX=360.0
C
      CALL DPSCR7(ISTRIN,NUMCHA,X0,Y0,
     1            IFONT,ICASE,IJUST,ANGLE,HMAX,VMAX,AMAX,WIDTH,HEIGHT,
     1            PHEIGH,PWIDTH,PVEGAP,PHOGAP,
     1            PHEIG2,PWIDT2,PVEGA2,PHOGA2,
     1            ANUMHP,ANUMVP,
     1            IPATT,PTHICK,ICOL,ICOLR,ICOLG,ICOLB,IRGBFL,
     1            JPATT,JTHICK,PTHIC2,JCOL,
     1            ISYMBL,ISPAC,
     1            IFILL,
     1            IMPSW2,AMPSCH,AMPSCW,
     1            PX99,PY99,IFOUND,IBUGD2,IERROR)
C
      GOTO9000
C
C               *****************************************************
C               **  FOR GENERAL DEVICE (AND "SET GENERAL FONT ON") **
C               **  LET POST PROCESSOR WRITE THE STRING IN  ONE OF **
C               **  ITS OWN FONTS. NOTE THAT THE DATAPLOT FONTS    **
C               **  WILL BE MAPPED TO THE POST PROCESSORS FONTS.   **
C               **  NOTE THAT IN THIS CASE, IT IS ASSUMMED THAT    **
C               **  THE POST PROCESSOR WILL ALSO SET THE "ANGLE"   **
C               **  AND THE JUSTIFICATION.                         **
C               *****************************************************
 3100 CONTINUE
      PX1P=PX1
      PY1P=PY1
      ICSTR(1:8)='MOVE TO '
      NCSTR=8
      NCHTOT=10
      NCHDEC=5
      CALL GRTRSA(PX1P,PY1P,AX,AY,ISUBN0)
      PX1P=AX
      PY1P=AY
      CALL GRTRRE(PX1P,NCHTOT,NCHDEC,ICSTR,NCSTR)
      ICSTR(19:20)='  '
      NCSTR=20
      CALL GRTRRE(PY1P,NCHTOT,NCHDEC,ICSTR,NCSTR)
      CALL GRWRST(ICSTR,NCSTR,ISUBN0)
      IF(NCTEXT.LE.0)GOTO3190
      ICSTR(1:11)='WRITE TEXT '
      NCSTR=11
      K=0
      DO3112J=1,NCTEXT
      K=J+NCSTR
      ICSTR(K:K)=ICTEXT(J)
 3112 CONTINUE
      NCSTR=K
      CALL GRWRST(ICSTR,NCSTR,ISUBN0)
 3190 CONTINUE
      GOTO9000
C
C               ***************************************************************
C               **  STEP 32--                                                **
C               **  TREAT THE CODED GENERAL (DEVICE-INDEPENDENT) CASE        **
C               ***************************************************************
C
 3200 CONTINUE
      PX1P=PX1
      PY1P=PY1
      ICSTR(1:5)='MOTO '
      NCSTR=5
      NCHTOT=10
      NCHDEC=5
      CALL GRTRRE(PX1P,NCHTOT,NCHDEC,ICSTR,NCSTR)
      ICSTR(16:17)='  '
      NCSTR=17
      CALL GRTRSA(PX1P,PY1P,AX,AY,ISUBN0)
      PX1P=AX
      PY1P=AY
      CALL GRTRRE(PY1P,NCHTOT,NCHDEC,ICSTR,NCSTR)
      CALL GRWRST(ICSTR,NCSTR,ISUBN0)
      IF(NCTEXT.LE.0)GOTO3290
      ICSTR(1:5)='WRTE '
      NCSTR=5
      K=0
      DO3212J=1,NCTEXT
      K=J+NCSTR
      ICSTR(K:K)=ICTEXT(J)
 3212 CONTINUE
      NCSTR=K
      CALL GRWRST(ICSTR,NCSTR,ISUBN0)
 3290 CONTINUE
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG4.EQ.'ON' .OR. ISUBG4.EQ.'WRTG')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF GRWRTG--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9039)IERRG4
 9039   FORMAT('IERRG4 = ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION gsumln(a,b)
C-----------------------------------------------------------------------
C          EVALUATION OF THE FUNCTION LN(GAMMA(A + B))
C          FOR 1 .LE. A .LE. 2  AND  1 .LE. B .LE. 2
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      DOUBLE PRECISION a,b
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION x
C     ..
C     .. External Functions ..
      DOUBLE PRECISION alnrel,gamln1
      EXTERNAL alnrel,gamln1
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC dble,dlog
C     ..
C     .. Executable Statements ..
      x = dble(a) + dble(b) - 2.D0
      IF (x.GT.0.25D0) GO TO 10
      gsumln = gamln1(1.0D0+x)
      RETURN

   10 IF (x.GT.1.25D0) GO TO 20
      gsumln = gamln1(x) + alnrel(x)
      RETURN

   20 gsumln = gamln1(x-1.0D0) + dlog(x* (1.0D0+x))
      RETURN

      END
      SUBROUTINE GTLCDF(X,ALPHA,BETA,A,B,CDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
C              FUNCTION VALUE FOR THE
C              GENERALIZED TOPP AND LEONE DISTRIBUTION.
C              THE CUMULATIVE DISTRIBUTION FUNCTION IS:
C
C                  F(X;ALPHA,BETA,A,B) = 1 -
C                     {ALPHA*((X-A)/(B-A)) - ALPHA-1)*
C                     ((X-A)/(B-A))**2}**BETA
C                                    A <= X <= B, BETA > 0,
C                                    0 < ALPHA <= 2
C
C              WITH ALPHA AND BETA DENOTING THE SHAPE PARAMETERS.
C
C     INPUT  ARGUMENTS--X      = THE DOUBLE PRECISION VALUE AT
C                                WHICH THE CUMULATIVE DISTRIBUTION
C                                FUNCTION IS TO BE EVALUATED.
C                     --ALPHA  = THE DOUBLE PRECISION FIRST SHAPE
C                                PARAMETER
C                     --BETA   = THE DOUBLE PRECISION SECOND SHAPE
C                                PARAMETER
C     OUTPUT ARGUMENTS--CDF    = THE DOUBLE PRECISION CUMULATIVE
C                                DISTRIBUTION FUNCTION VALUE.
C     OUTPUT--THE DOUBLE PRECISION CUMULATIVE DISTRIBUTION
C             FUNCTION VALUE CDF.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS. 
C     RESTRICTIONS--X SHOULD BE BETWEEN 0 AND 1, INCLUSIVELY.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--DLOG, DEXP.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
C                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
C                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
C                 PUBLISHING COMPANY, CHAPTER 7.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE:  301-975-2855
C     ORIGINAL VERSION--FEBRUARY  2007. 
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION X
      DOUBLE PRECISION ALPHA
      DOUBLE PRECISION BETA
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION CDF
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DX
C
      INCLUDE 'DPCOP2.INC'
C
C---------------------------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(B.LT.A)THEN
        TERM1=B
        B=A
        A=TERM1
      ENDIF
C
      CDF=0.0D0
      IF(X.LT.A .OR. X.GT.B)THEN
        WRITE(ICOUT,2)
    2   FORMAT('***** ERROR--THE FIRST ARGUMENT TO GTLCDF IS ',
     1         'OUTSIDE THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3)A,B
    3   FORMAT('      (',G15.7,',',G15.7,') INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(ALPHA.LE.0.0D0 .OR. ALPHA.GT.2.0)THEN
        WRITE(ICOUT,12)
   12   FORMAT('***** ERROR--THE SECOND ARGUMENT TO GTLCDF IS ',
     1         'OUTSIDE THE [0,2) INTERVAL')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(BETA.LE.0.0D0)THEN
        WRITE(ICOUT,14)
   14   FORMAT('***** ERROR--THE THIRD ARGUMENT TO GTLCDF IS ',
     1         'IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(B.EQ.A)THEN
        WRITE(ICOUT,16)
   16   FORMAT('***** ERROR--THE LOWER AND UPPER LIMITS FOR GTLCDF ',
     1         'ARE EQUAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)A
   48   FORMAT('***** THE VALUE OF THE LIMIT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C-----START POINT-----------------------------------------------------
C
      DX=(X-A)/(B-A)
      IF(X.LE.A)THEN
        CDF=0.0D0
      ELSEIF(X.GE.B)THEN
        CDF=1.0D0
      ELSE
        DTERM1=ALPHA*DX - (ALPHA-1.0D0)*DX**2
        DTERM2=BETA*DLOG(DTERM1)
        CDF=DEXP(DTERM2)
      ENDIF
C
 9000 CONTINUE
      RETURN
      END 
      SUBROUTINE GTLPDF(X,ALPHA,BETA,A,B,PDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
C              FUNCTION VALUE FOR THE
C              GENERALIZED TOPP AND LEONE DISTRIBUTION.
C              THE PROBABILITY DENSITY FUNCTION IS:
C
C                  f(X;ALPHA,BETA,A,B) = BETA*{ALPHA*((X-A)/B-A) -
C                     (ALPHA - 1)*((X-A)/(B-A))**2}**{BETA-1)*
C                     (ALPHA - 2*(ALPHA - 1)*((X-A)/(B-A)))
C                                    A <= X <= B, BETA > 0,
C                                    0 < ALPHA <= 2
C
C              WITH ALPHA AND BETA DENOTING THE SHAPE PARAMETERS.
C
C     INPUT  ARGUMENTS--X      = THE DOUBLE PRECISION VALUE AT
C                                WHICH THE PROBABILITY DENSITY
C                                FUNCTION IS TO BE EVALUATED.
C                     --ALPHA  = THE DOUBLE PRECISION FIRST SHAPE
C                                PARAMETER
C                     --BETA   = THE DOUBLE PRECISION SECOND SHAPE
C                                PARAMETER
C     OUTPUT ARGUMENTS--PDF    = THE DOUBLE PRECISION CUMULATIVE
C                                DISTRIBUTION FUNCTION VALUE.
C     OUTPUT--THE DOUBLE PRECISION PROBABILITY DENSITY
C             FUNCTION VALUE PDF.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS. 
C     RESTRICTIONS--X SHOULD BE BETWEEN 0 AND 1, INCLUSIVELY.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--DLOG, DEXP.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
C                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
C                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
C                 PUBLISHING COMPANY, CHAPTER 7.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE:  301-975-2855
C     ORIGINAL VERSION--FEBRUARY  2007. 
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION X
      DOUBLE PRECISION ALPHA
      DOUBLE PRECISION BETA
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION PDF
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION DX
C
      INCLUDE 'DPCOP2.INC'
C
C---------------------------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      PDF=0.0D0
      IF(B.LT.A)THEN
        TERM1=B
        B=A
        A=TERM1
      ENDIF
C
      IF(X.LT.A .OR. X.GT.B)THEN
        WRITE(ICOUT,2)
    2   FORMAT('***** ERROR--THE FIRST ARGUMENT TO GTLPDF IS ',
     1         'OUTSIDE THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3)A,B
    3   FORMAT('      (',G15.7,',',G15.7,') INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(ALPHA.LE.0.0D0 .OR. ALPHA.GT.2.0)THEN
        WRITE(ICOUT,12)
   12   FORMAT('***** ERROR--THE SECOND ARGUMENT TO GTLPDF IS ',
     1         'OUTSIDE THE [0,2) INTERVAL')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(BETA.LE.0.0D0)THEN
        WRITE(ICOUT,14)
   14   FORMAT('***** ERROR--THE THIRD ARGUMENT TO GTLPDF IS ',
     1         'IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(B.EQ.A)THEN
        WRITE(ICOUT,16)
   16   FORMAT('***** ERROR--THE LOWER AND UPPER LIMITS FOR GTLPDF ',
     1         'ARE EQUAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)A
   48   FORMAT('***** THE VALUE OF THE LIMIT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C-----START POINT-----------------------------------------------------
C
      DEPS=1.0D-012
      DX=(X-A)/(B-A)
      IF(X.LE.A)THEN
        IF(BETA.GE.1.0D0 - DEPS)THEN
          PDF=BETA*(2.0D0 - ALPHA)
        ELSE
          PDF=0.0D0
        ENDIF
      ELSEIF(X.GE.B)THEN
        PDF=BETA*(2.0D0 - ALPHA)
      ELSE
        DTERM1=DLOG(BETA)
        DTERM2=(BETA-1.0D0)*DLOG(ALPHA*DX - (ALPHA-1.0D0)*DX**2)
        DTERM3=DLOG(ALPHA - 2.0D0*(ALPHA - 1.0D0)*DX)
        PDF=DEXP(DTERM1 + DTERM2 + DTERM3)
      ENDIF
C
 9000 CONTINUE
      RETURN
      END 
      SUBROUTINE GTLPPF(P,ALPHA,BETA,PPF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
C              FUNCTION VALUE FOR THE GENERALIZED TOPP AND LEONE
C              DISTRIBUTION.
C
C              THE PERCENT POINT FUNCTION IS:
C
C              G(P;ALPHA,BETA,A,B) = P**(1/BETA)     FOR ALPHA = 1
C                  {-ALPHA + SQRT(ALPHA**2 0 4*(1-ALPHA)*
C                  (1-P**(1/BETA))}/{2*(1-ALPHA)}
C                  FOR 0 < ALPHA < 1 AND 1 < ALPHA <= 2
C
C              WITH ALPHA AND BETA DENOTING THE SHAPE PARAMETERS.
C
C     INPUT  ARGUMENTS--X      = THE DOUBLE PRECISION VALUE AT
C                                WHICH THE PERCENT POINT
C                                FUNCTION IS TO BE EVALUATED.
C                     --ALPHA  = THE DOUBLE PRECISION FIRST SHAPE
C                                PARAMETER
C                     --BETA   = THE DOUBLE PRECISION SECOND SHAPE
C                                PARAMETER
C     OUTPUT ARGUMENTS--PPF    = THE DOUBLE PRECISION PERCENT POINT
C                                FUNCTION VALUE.
C     OUTPUT--THE DOUBLE PRECISION PERCENT POINT FUNCTION VALUE PDF.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS. 
C     RESTRICTIONS--X SHOULD BE BETWEEN 0 AND 1, INCLUSIVELY.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--GTLCDF
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
C                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
C                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
C                 PUBLISHING COMPANY, CHAPTER 7.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2007/2
C     ORIGINAL VERSION--FEBRUARY  2007.
C     UPDATED         --SEPTEMBER 2007. REPLACE BISECTION FORMULA
C                                       WITH EXPLICIT FUNCTION
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES----------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C-----COMMON--------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
CCCCC DATA EPS /1.0D-8/
CCCCC DATA SIG /1.0D-8/
CCCCC DATA ZERO /0.0D0/
CCCCC DATA MAXIT /500/
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      PPF=0.0D0
      IF(P.LT.0.0D0 .OR. P.GT.1.0D0)THEN
        WRITE(ICOUT,2)
    2   FORMAT('***** ERROR--THE FIRST ARGUMENT TO GTLPPF IS ',
     1         'OUTSIDE THE (0,1) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(ALPHA.LE.0.0D0 .OR. ALPHA.GT.2.0)THEN
        WRITE(ICOUT,12)
   12   FORMAT('***** ERROR--THE SECOND ARGUMENT TO GTLPPF IS ',
     1         'OUTSIDE THE [0,2) INTERVAL')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(BETA.LE.0.0D0)THEN
        WRITE(ICOUT,14)
   14   FORMAT('***** ERROR--THE THIRD ARGUMENT TO GTLPPF IS ',
     1         'IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
      IF(ALPHA.EQ.1.0)THEN
        PPF=P**(1.0D0/BETA)
      ELSE
        TERM1=-ALPHA
        TERM2=ALPHA**2 - 4.0D0*(1.0D0 - ALPHA)*(-P**(1.0D0/BETA))
        IF(TERM2.LT.0.0D0)THEN
          WRITE(ICOUT,101)
  101     FORMAT('***** ERROR--UNABLE TO COMPUTE GTLPPF.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,103)P
  103     FORMAT('      FIRST INPUT ARGUMENT = ',G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,105)ALPHA
  105     FORMAT('      SECOND INPUT ARGUMENT = ',G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,107)BETA
  107     FORMAT('      THIRD INPUT ARGUMENT = ',G15.7)
          CALL DPWRST('XXX','BUG ')
          PPF=0.0D0
          GOTO9000
        ENDIF
        TERM3=2.0D0*(1.0D0 - ALPHA)
        PPF=(TERM1 + SQRT(TERM2))/TERM3
        IF(PPF.LT.0.0D0 .OR. PPF.GT.1.0D0)THEN
          PPF=(TERM1 - SQRT(TERM2))/TERM3
          IF(PPF.LT.0.0D0 .OR. PPF.GT.1.0D0)THEN
            WRITE(ICOUT,101)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,103)P
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,105)ALPHA
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,107)BETA
            CALL DPWRST('XXX','BUG ')
            PPF=0.0D0
            GOTO9000
          ENDIF
        ENDIF
      ENDIF
C
CCCCC A = 0.0D0
CCCCC B = 1.0D0
C
CCCCC IERR=0
CCCCC IC = 0
CCCCC XL = 0.0D0
CCCCC XR = 1.0D0
CCCCC FXL = -P
CCCCC FXR = 1.0D0 - P
C
C  BISECTION METHOD
C
CC105 CONTINUE
CCCCC X = (XL+XR)*0.5D0
CCCCC CALL GTLCDF(X,ALPHA,BETA,A,B,DCDF)
CCCCC P1=DCDF
CCCCC PPF=X
CCCCC FCS = P1 - P
C
CCCCC IF(FCS*FXL.GT.ZERO)THEN
CCCCC   XL = X
CCCCC   FXL = FCS
CCCCC ELSE
CCCCC   XR = X
CCCCC   FXR = FCS 
CCCCC ENDIF
C
CCCCC XRML = XR - XL
CCCCC IF(XRML.LE.SIG .AND. DABS(FCS).LE.EPS)GOTO9000
CCCCC IC = IC + 1
CCCCC IF(IC.LE.MAXIT)GOTO105
CCCCC WRITE(ICOUT,130)
CC130 FORMAT('***** ERROR--GTLPPF ROUTINE DID NOT CONVERGE.')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC GOTO9000
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE GTLRAN(N,ALPHA,BETA,ISEED,X)
C
C     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
C              FROM THE GENERALIZED TOPP AND LEONE
C              DISTRIBUTION WITH SHAPE PARAMETERS ALPHA AND BETA.
C
C              THE PROBABILITY DENSITY FUNCTION IS:
C
C                  f(X;ALPHA,BETA,A,B) = BETA*{ALPHA*((X-A)/B-A) -
C                     (ALPHA - 1)*((X-A)/(B-A))**2}**{BETA-1)*
C                     (ALPHA - 2*(ALPHA - 1)*((X-A)/(B-A)))
C                                    A <= X <= B, BETA > 0,
C                                    0 < ALPHA <= 2
C
C              WITH ALPHA AND BETA DENOTING THE SHAPE PARAMETERS.
C
C     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
C                                OF RANDOM NUMBERS TO BE
C                                GENERATED.
C                     --ALPHA  = THE DOUBLE PRECISION FIRST SHAPE
C                                PARAMETER
C                     --BETA   = THE DOUBLE PRECISION VALUE OF THE
C                                SHAPE PARAMETER BETA.
C                                BETA SHOULD BE IN THE RANGE (0,1).
C     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
C                                (OF DIMENSION AT LEAST N)
C                                INTO WHICH THE GENERATED
C                                RANDOM SAMPLE WILL BE PLACED.
C     OUTPUT--A RANDOM SAMPLE OF SIZE N
C             FROM THE GENERALIZED TOPP AND LEONE
C             DISTRIBUTION WITH SHAPE PARAMETERS ALPHA AND BETA.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN, RGTPPF.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
C                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
C                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
C                 PUBLISHING COMPANY, CHAPTER 7.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHMOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2007.2
C     ORIGINAL VERSION--FEBRUARY  2007.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION ALPHA
      DOUBLE PRECISION BETA
      DOUBLE PRECISION DTEMP
      DIMENSION X(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(N.LT.1)THEN
        WRITE(ICOUT,5)
    5   FORMAT('***** ERROR--THE REQUESTED NUMBER OF ',
     1         'GENERALIZED TOPP AND LEONE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6)
    6   FORMAT('      RANDOM NUMBERS IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
   47   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(BETA.LE.0.0D0)THEN
        WRITE(ICOUT,201)
  201   FORMAT('***** ERROR--THE BETA SHAPE PARAMETER IS ',
     1         'NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,203)BETA
  203   FORMAT('      THE VALUE OF BETA IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(ALPHA.LE.0.0D0 .OR. ALPHA.GT.2.0D0)THEN
        WRITE(ICOUT,301)
  301   FORMAT('***** ERROR--THE ALPHA SHAPE PARAMETER IS ',
     1         'OUTSIDE THE [0,2) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,303)ALPHA
  303   FORMAT('      THE VALUE OF ALPHA IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
C     GENERATE N UNIFORM (0,1) RANDOM NUMBERS;
C
      CALL UNIRAN(N,ISEED,X)
C
C     GENERATE N GENERALIZED TOPP AND LEONE DISTRIBUTION
C     RANDOM NUMBERS USING THE PERCENT POINT FUNCTION TRANSFORMATION
C     METHOD.
C
      DO300I=1,N
        ZTEMP=X(I)
        CALL GTLPPF(DBLE(ZTEMP),ALPHA,BETA,DTEMP)
        X(I)=REAL(DTEMP)
  300 CONTINUE
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE GTRCDF(X,A,B,C,D,ANU1,ANU3,ALPHA,CDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
C              FUNCTION VALUE FOR THE GENERALIZED TRAPEZOID
C              DISTRIBUTION.
C              THIS DISTRIBUTION HAS THE FOLLOWING CDF FUNCTION:
C              F(X,A,B,C,D,N1,N2,ALPHA)
C              = 0                                    X < A
C              = [2*ALPHA*(B-A)*NU3/
C              (2*ALPHA*(B-A)*NU3+(ALPHA+1)*(C-B)*NU1*NU3+2*(D-C)*NU1)]
C              *((X-A)/(B-A))**NU1
C                                                     A <= X <  B
C              = [2*ALPHA*(B-A)*NU3 + 2*(X-B)*NU1*NU3*
C              {1 + (ALPHA-1)*(2*C-B-X)/(2*(C-B)}]/
C              [(2*ALPHA*(B-A)*NU3+(ALPHA+1)*(C-B)*NU1*NU3+2*(D-C)*NU1)]
C              *{(ALPHA-1)*(C-X)/(C-B)+1}
C                                                     B <= X <  C
C              = 1 -
C              [2*(D-C)*NU1]/
C              (2*ALPHA*(B-A)*NU3+(ALPHA+1)*(C-B)*NU1*NU3+2*(D-C)*NU1)]
C              *((D-X)/(D-C))**NU3
C                                                     C <= X <  D
C              = 1                                    X >= D
C              WHERE
C                  A <= B <= C <= D, NU1, NU3, ALPHA > 0
C              THIS DISTRIBUTION MODELS A "GROWTH PHASE",
C              A "STABLE PHASE", AND A "DECAY PHASE".
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE CUMULATIVE DISTRIBUTION
C                                FUNCTION IS TO BE EVALUATED.
C                     --A      = THE SINGLE PRECISION SHAPE PARAMETER
C                       B      = THE SINGLE PRECISION SHAPE PARAMETER
C                       C      = THE SINGLE PRECISION SHAPE PARAMETER
C                       D      = THE SINGLE PRECISION SHAPE PARAMETER
C                       ANU1   = THE SINGLE PRECISION SHAPE PARAMETER
C                       ANU3   = THE SINGLE PRECISION SHAPE PARAMETER
C                       ALPHA  = THE SINGLE PRECISION SHAPE PARAMETER
C     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE
C                                DISTRIBUTION FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION
C             FUNCTION VALUE CDF.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS. 
C     RESTRICTIONS--X SHOULD BE BETWEEN A AND D, INCLUSIVELY.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--J. RENE VAN DORP AND SAMIEL KOTZ, "GENERALIZED
C                 TRAPEZOIDAL DISTRIBUTIONS", METRIKA, VOL. 58,
C                 ISSUE 1, JULY 2003.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE:  301-975-2855
C     ORIGINAL VERSION--JUNE      2003. 
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION DX
      DOUBLE PRECISION DA
      DOUBLE PRECISION DB
      DOUBLE PRECISION DC
      DOUBLE PRECISION DD
      DOUBLE PRECISION DALPHA
      DOUBLE PRECISION DNU1
      DOUBLE PRECISION DNU3
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION DTERM4
C
      INCLUDE 'DPCOP2.INC'
C
C---------------------------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(A.GT.B .OR. B.GT.C .OR. C.GT.D)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,13)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,14)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,16)A,B,C,D
        CALL DPWRST('XXX','BUG ')
        CDF=0.0
        GOTO9000
      ENDIF
      IF(ALPHA.LE.0.0)THEN
        WRITE(ICOUT,22)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,23)ALPHA
        CALL DPWRST('XXX','BUG ')
        CDF=0.0
        GOTO9000
      ENDIF
      IF(ANU1.LE.0.0)THEN
        WRITE(ICOUT,32)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,33)ANU1
        CALL DPWRST('XXX','BUG ')
        CDF=0.0
        GOTO9000
      ENDIF
      IF(ANU3.LE.0.0)THEN
        WRITE(ICOUT,42)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,43)ANU3
        CALL DPWRST('XXX','BUG ')
        CDF=0.0
        GOTO9000
      ENDIF
   12 FORMAT(
     1'***** FATAL ERROR--FOR THE GENERALIZED TRAPEZOID DISTRIBUTION,')
   13 FORMAT(
     1'      THE FOUR SHAPE PARAMETERS (A, B, C, D) MUST SATISFY')
   14 FORMAT(
     1'         A <= B <= C <= D')
   16 FORMAT(
     1'      A, B, C, D = ',4E15.7)
   22 FORMAT(
     1'***** FATAL ERROR--FOR THE GENERALIZED TRAPEZOID DISTRIBUTION,')
   23 FORMAT(
     1'      THE ALPHA SHAPE PARAMETER MUST BE > 0.  ALPHA = ',E15.7)
   32 FORMAT(
     1'***** FATAL ERROR--FOR THE GENERALIZED TRAPEZOID DISTRIBUTION,')
   33 FORMAT(
     1'      THE NU1 SHAPE PARAMETER MUST BE > 0.  NU1 = ',E15.7)
   42 FORMAT(
     1'***** FATAL ERROR--FOR THE GENERALIZED TRAPEZOID DISTRIBUTION,')
   43 FORMAT(
     1'      THE NU3 SHAPE PARAMETER MUST BE > 0.  NU3 = ',E15.7)
C
C-----START POINT-----------------------------------------------------
C
      DCDF=0.0D0
      IF(X.LT.A)THEN
        CDF=0.0
        GOTO9000
      ELSEIF(X.GE.D)THEN
        CDF=1.0
        GOTO9000
      ENDIF
C
      DX=DBLE(X)
      DA=DBLE(A)
      DB=DBLE(B)
      DC=DBLE(C)
      DD=DBLE(D)
      DNU1=DBLE(ANU1)
      DNU3=DBLE(ANU3)
      DALPHA=DBLE(ALPHA)
      DTERM2=2.0D0*DALPHA*(DB-DA)*DNU3 +
     1       (DALPHA+1.0D0)*(DC-DB)*DNU1*DNU3 +
     1       2.0D0*(DD-DC)*DNU1
C
      IF(A.LE.X .AND. X.LT.B)THEN
        DTERM1=2.0D0*DALPHA*(DB-DA)*DNU3
        DTERM3=((DX-DA)/(DB-DA))**DNU1
        DCDF=(DTERM1/DTERM2)*DTERM3
      ELSEIF(B.LE.X .AND. X.LT.C)THEN
        DTERM1=2.0D0*DALPHA*(DB-DA)*DNU3
        DTERM3=2.0D0*(DX-DB)*DNU1*DNU3
        DTERM4=1.0D0+(DALPHA-1.0D0)*(2.0D0*DC-DB-DX)/(2.0D0*(DC-DB))
        DCDF=(DTERM1 + DTERM3*DTERM4)/DTERM2
      ELSEIF(C.LE.X .AND. X.LT.D)THEN
        DTERM1=2.0D0*(DD-DC)*DNU1
        DTERM3=((DD-DX)/(DD-DC))**DNU3
        DCDF=1.0D0 - (DTERM1/DTERM2)*DTERM3
      ENDIF
      CDF=REAL(DCDF)
C
 9000 CONTINUE
      RETURN
      END 
      SUBROUTINE GTRPDF(X,A,B,C,D,ANU1,ANU3,ALPHA,PDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
C              FUNCTION VALUE FOR THE GENERALIZED TRAPEZOID
C              DISTRIBUTION.
C              THIS DISTRIBUTION HAS THE FOLLOWING PDF FUNCTION:
C              f(X,A,B,C,D,N1,N2,ALPHA)
C              = [2*ALPHA*NU1*NU3/
C              (2*ALPHA*(B-A)*NU3+(ALPHA+1)*(C-B)*NU1*NU3+2*(D-C)*NU1)]
C              *((X-A)/(B-A))**(NU1-1)
C                                                     A <= X <  B
C              = [2*NU1*NU3/
C              (2*ALPHA*(B-A)*NU3+(ALPHA+1)*(C-B)*NU1*NU3+2*(D-C)*NU1)]
C              *{(ALPHA-1)*(C-X)/(C-B)+1}
C                                                     B <= X <  C
C              = [2*NU1*NU3/
C              (2*ALPHA*(B-A)*NU3+(ALPHA+1)*(C-B)*NU1*NU3+2*(D-C)*NU1)]
C              *((D-X)/(D-C))**(NU3-1)
C                               = U*((D-X)/(D-C))     C <= X <  D
C                               = 0                   X < A, X >= D
C              WHERE
C                  A <= B <= C <= D, NU1, NU3, ALPHA > 0
C              THIS DISTRIBUTION MODELS A "GROWTH PHASE",
C              A "STABLE PHASE", AND A "DECAY PHASE".
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE PROBABILITY DENSITY
C                                FUNCTION IS TO BE EVALUATED.
C                     --A      = THE SINGLE PRECISION SHAPE PARAMETER
C                       B      = THE SINGLE PRECISION SHAPE PARAMETER
C                       C      = THE SINGLE PRECISION SHAPE PARAMETER
C                       D      = THE SINGLE PRECISION SHAPE PARAMETER
C                       ANU1   = THE SINGLE PRECISION SHAPE PARAMETER
C                       ANU3   = THE SINGLE PRECISION SHAPE PARAMETER
C                       ALPHA  = THE SINGLE PRECISION SHAPE PARAMETER
C     OUTPUT ARGUMENTS--PDF    = THE SINGLE PRECISION PROBABILITY
C                                DENSITY FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
C             FUNCTION VALUE PDF.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS. 
C     RESTRICTIONS--X SHOULD BE BETWEEN A AND D, INCLUSIVELY.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--J. RENE VAN DORP AND SAMIEL KOTZ, "GENERALIZED
C                 TRAPEZOIDAL DISTRIBUTIONS", METRIKA, VOL. 58,
C                 ISSUE 1, JULY 2003.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE:  301-975-2855
C     ORIGINAL VERSION--JUNE      2003. 
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DPDF
      DOUBLE PRECISION DX
      DOUBLE PRECISION DA
      DOUBLE PRECISION DB
      DOUBLE PRECISION DC
      DOUBLE PRECISION DD
      DOUBLE PRECISION DALPHA
      DOUBLE PRECISION DNU1
      DOUBLE PRECISION DNU3
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DTERM3
C
      INCLUDE 'DPCOP2.INC'
C
C---------------------------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      PDF=0.0
      DPDF=0.0D0
      IF(A.GT.B .OR. B.GT.C .OR. C.GT.D)THEN
        WRITE(ICOUT,12)
   12   FORMAT('***** ERROR--FOR THE GENERALZIED TRAPEZOID ',
     1         'DISTRIBUTION,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,13)
   13   FORMAT('      THE FOUR SHAPE PARAMETERS (A, B, C, D) MUST ',
     1         'SATISFY')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,14)
   14   FORMAT('         A <= B <= C <= D.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,16)A,B,C,D
   16   FORMAT('      A, B, C, D = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(ALPHA.LE.0.0)THEN
        WRITE(ICOUT,22)
   22   FORMAT('***** ERROR--FOR THE GENERALIZED TRAPEZOID ',
     1         'DISTRIBUTION,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,23)ALPHA
   23   FORMAT('      THE ALPHA SHAPE PARAMETER MUST BE > 0.  ',
     1         'ALPHA = ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(ANU1.LE.0.0)THEN
        WRITE(ICOUT,32)
   32   FORMAT('***** ERROR--FOR THE GENERALIZED TRAPEZOID ',
     1         'DISTRIBUTION,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,33)ANU1
   33   FORMAT('      THE NU1 SHAPE PARAMETER MUST BE > 0.  NU1 = ',
     1         G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(ANU3.LE.0.0)THEN
        WRITE(ICOUT,42)
   42   FORMAT('***** ERROR--FOR THE GENERALIZED TRAPEZOID ',
     1         'DISTRIBUTION,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,43)ANU3
   43   FORMAT('      THE NU3 SHAPE PARAMETER MUST BE > 0.  NU3 = ',
     1         G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
C-----START POINT-----------------------------------------------------
C
      IF(X.LT.A .OR. X.GE.D)THEN
        PDF=0.0
        GOTO9000
      ENDIF
C
      DX=DBLE(X)
      DA=DBLE(A)
      DB=DBLE(B)
      DC=DBLE(C)
      DD=DBLE(D)
      DNU1=DBLE(ANU1)
      DNU3=DBLE(ANU3)
      DALPHA=DBLE(ALPHA)
      DTERM1=2.0D0*DNU1*DNU3
      DTERM2=2.0D0*DALPHA*(DB-DA)*DNU3 +
     1       (DALPHA+1.0D0)*(DC-DB)*DNU1*DNU3 +
     1       2.0D0*(DD-DC)*DNU1
      DTERM3=DTERM1/DTERM2
C
      IF(A.LE.X .AND. X.LT.B)THEN
        IF(A.EQ.X .AND. ANU1.LE.1.0)THEN
          WRITE(ICOUT,132)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,133)ANU1
          CALL DPWRST('XXX','BUG ')
          PDF=0.0
          GOTO9000
        ENDIF
  132 FORMAT(
     1'***** FATAL ERROR--FOR THE GENERALIZED TRAPEZOID DISTRIBUTION,')
  133 FORMAT(
     1'      WHEN X = A AND NU1 <= 1, THE PDF IS UNDEFINED.')
        DPDF=DALPHA*DTERM3*((DX-DA)/(DB-DA))**(DNU1-1.0D0)
      ELSEIF(B.LE.X .AND. X.LT.C)THEN
        DPDF=DTERM3*((DALPHA-1.0D0)*(DC-DX)/(DC-DB) + 1.0D0)
      ELSEIF(C.LE.X .AND. X.LT.D)THEN
        IF(D.EQ.X .AND. ANU3.LE.1.0)THEN
          WRITE(ICOUT,232)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,233)ANU1
          CALL DPWRST('XXX','BUG ')
          PDF=0.0
          GOTO9000
        ENDIF
  232 FORMAT(
     1'***** FATAL ERROR--FOR THE GENERALIZED TRAPEZOID DISTRIBUTION,')
  233 FORMAT(
     1'      WHEN X = D AND NU3 <= 1, THE PDF IS UNDEFINED.')
        DPDF=DTERM3*((DD-DX)/(DD-DC))**(DNU3-1.0D0)
      ENDIF
      PDF=REAL(DPDF)
C
 9000 CONTINUE
      RETURN
      END 
      SUBROUTINE GTRPPF(P,A,B,C,D,ANU1,ANU3,ALPHA,PPF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
C              FUNCTION VALUE FOR THE GENERALZIED TRAPEZOID
C              DISTRIBUTION.
C              THIS DISTRIBUTION HAS THE FOLLOWING CDF FUNCTION:
C              F(X,A,B,C,D,N1,N2,ALPHA)
C              = 0                                    X < A
C              = [2*ALPHA*(B-A)*NU3/
C              (2*ALPHA*(B-A)*NU3+(ALPHA+1)*(C-B)*NU1*NU3+2*(D-C)*NU1)]
C              *((X-A)/(B-A))**NU1
C                                                     A <= X <  B
C              = [2*ALPHA*(B-A)*NU3 + 2*(X-B)*NU1*NU3*
C              {1 + (ALPHA-1)*(2*C-B-X)/(2*(C-B)}]/
C              [(2*ALPHA*(B-A)*NU3+(ALPHA+1)*(C-B)*NU1*NU3+2*(D-C)*NU1)]
C              *{(ALPHA-1)*(C-X)/(C-B)+1}
C                                                     B <= X <  C
C              = 1 -
C              [2*(D-C)*NU1]/
C              (2*ALPHA*(B-A)*NU3+(ALPHA+1)*(C-B)*NU1*NU3+2*(D-C)*NU1)]
C              *((D-X)/(D-C))**NU3
C                                                     C <= X <  D
C              = 1                                    X >= D
C              WHERE
C                  A <= B <= C <= D, NU1, NU3, ALPHA > 0
C              THE ALGORITHM FOR THE PPF IS TO COMPUTE THE CDF AT
C              X = A, X = B, X = C, AND X = D TO FIND THE APPROPRIATE
C              INTERVAL FOR P.  THEN INVERT THE APPROPRIATE EQUATION
C              ABOVE TO FIND THE PPF VALUE.  FOR THE INTERVAL FOR
C              B < X < C, USE A BISECTION METHOD (ALGEBRA FOR
C              INVERSION GETS A BIT MESSY).
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE PROBABILITY DENSITY
C                                FUNCTION IS TO BE EVALUATED.
C                     --A      = THE SINGLE PRECISION SHAPE PARAMETER
C                       B      = THE SINGLE PRECISION SHAPE PARAMETER
C                       C      = THE SINGLE PRECISION SHAPE PARAMETER
C                       D      = THE SINGLE PRECISION SHAPE PARAMETER
C                       ANU1   = THE SINGLE PRECISION SHAPE PARAMETER
C                       ANU3   = THE SINGLE PRECISION SHAPE PARAMETER
C                       ALPHA  = THE SINGLE PRECISION SHAPE PARAMETER
C     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT POINT
C                                FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PERCENT POINT FUNCTION VALUE PPF.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS. 
C     RESTRICTIONS--P SHOULD BE BETWEEN 0 AND 1, INCLUSIVELY.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--J. RENE VAN DORP AND SAMIEL KOTZ, "GENERALIZED
C                 TRAPEZOIDAL DISTRIBUTIONS", METRIKA, VOL. 58,
C                 ISSUE 1, JULY 2003.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE:  301-975-2855
C     ORIGINAL VERSION--JUNE      2003. 
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DPPF
      DOUBLE PRECISION DP
      DOUBLE PRECISION DA
      DOUBLE PRECISION DB
      DOUBLE PRECISION DC
      DOUBLE PRECISION DD
      DOUBLE PRECISION DALPHA
      DOUBLE PRECISION DNU1
      DOUBLE PRECISION DNU3
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION DTERM4
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION DXL
      DOUBLE PRECISION DXR
      DOUBLE PRECISION DFXL
      DOUBLE PRECISION DFXR
      DOUBLE PRECISION DP1
      DOUBLE PRECISION DFCS
      DOUBLE PRECISION DXRML
      DOUBLE PRECISION DSIG
      DOUBLE PRECISION DEPS
C
      INCLUDE 'DPCOP2.INC'
C
      DATA DEPS /0.0000001/
      DATA DSIG /1.0D-7/
CCCCC DATA DZERO /0./
      DATA MAXIT /2000/
C
C---------------------------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(P.LT.0.0 .OR. P.GT.1.0)THEN
        WRITE(ICOUT,22)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,23)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,26)P
        CALL DPWRST('XXX','BUG ')
        PPF=0.0
        GOTO9000
      ENDIF
C
      IF(A.GE.B .OR. B.GE.C .OR. C.GE.D)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,13)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,14)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,16)A,B,C,D
        CALL DPWRST('XXX','BUG ')
        PPF=0.0
        GOTO9000
      ENDIF
      IF(ALPHA.LE.0.0)THEN
        WRITE(ICOUT,22)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,23)ALPHA
        CALL DPWRST('XXX','BUG ')
        PPF=0.0
        GOTO9000
      ENDIF
      IF(ANU1.LE.0.0)THEN
        WRITE(ICOUT,32)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,33)ANU1
        CALL DPWRST('XXX','BUG ')
        PPF=0.0
        GOTO9000
      ENDIF
      IF(ANU3.LE.0.0)THEN
        WRITE(ICOUT,42)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,43)ANU3
        CALL DPWRST('XXX','BUG ')
        PPF=0.0
        GOTO9000
      ENDIF
C
   12 FORMAT(
     1'***** FATAL ERROR--FOR THE GENERALZIED TRAPEZOID DISTRIBUTION,')
   13 FORMAT(
     1'      THE FOUR SHAPE PARAMETERS (A, B, C, D) MUST SATISFY')
   14 FORMAT(
     1'         A < B < C < D')
   16 FORMAT(
     1'      A, B, C, D = ',4E15.7)
   22 FORMAT(
     1'***** FATAL ERROR--FOR THE GENERALZIED TRAPEZOID PERCENT,')
   23 FORMAT(
     1'      POINT FUNCTION THE VALUE OF THE INPUT ARGUMENT IS ',
     1'OUTSIDE THE ALLOWABLE (0,1) INTERVAL.')
   26 FORMAT(
     1'      VALUE OF INPUT ARGUMENT = ',E15.7)
   32 FORMAT(
     1'***** FATAL ERROR--FOR THE GENERALIZED TRAPEZOID DISTRIBUTION,')
   33 FORMAT(
     1'      THE NU1 SHAPE PARAMETER MUST BE > 0.  NU1 = ',E15.7)
   42 FORMAT(
     1'***** FATAL ERROR--FOR THE GENERALIZED TRAPEZOID DISTRIBUTION,')
   43 FORMAT(
     1'      THE NU3 SHAPE PARAMETER MUST BE > 0.  NU3 = ',E15.7)
C
C-----START POINT-----------------------------------------------------
C
      DPPF=0.0D0
      P1=0.0
      CALL GTRCDF(B,A,B,C,D,ANU1,ANU3,ALPHA,P2)
      CALL GTRCDF(C,A,B,C,D,ANU1,ANU3,ALPHA,P3)
      P4=1.0
C
      IF(P.EQ.0.0)THEN
        PPF=A
        GOTO9000
      ELSEIF(P.EQ.1.0)THEN
        PPF=D
        GOTO9000
      ELSEIF(P.EQ.P2)THEN
        PPF=B
        GOTO9000
      ELSEIF(P.EQ.P3)THEN
        PPF=C
        GOTO9000
      ENDIF
C
      DP=DBLE(P)
      DALPHA=DBLE(ALPHA)
      DNU1=DBLE(ANU1)
      DNU3=DBLE(ANU3)
      DA=DBLE(A)
      DB=DBLE(B)
      DC=DBLE(C)
      DD=DBLE(D)
      DTERM2=2.0D0*DALPHA*(DB-DA)*DNU3 +
     1       (DALPHA+1.0D0)*(DC-DB)*DNU1*DNU3 +
     1       2.0D0*(DD-DC)*DNU1
C
      IF(P.GE.P1 .AND. P.LE.P2)THEN
        DTERM1=2.0D0*DALPHA*(DB-DA)*DNU3
        DPPF=(DB-DA)*((DTERM2/DTERM1)*DP)**(1.0D0/DNU1) + DA
      ELSEIF(P.GE.P2 .AND. P.LE.P3)THEN
        DXL=DB
        DXR=DC
C
C       BISECTION METHOD
C
        IC = 0
        DFXL = -DP
        DFXR = 1.0D0 - DP
  105   CONTINUE
          DX = (DXL+DXR)*0.5D0
C
C         GTRCDF FOR B < X < C CASE
C
          DTERM1=2.0D0*DALPHA*(DB-DA)*DNU3
          DTERM3=2.0D0*(DX-DB)*DNU1*DNU3
          DTERM4=1.0D0+(DALPHA-1.0D0)*(2.0D0*DC-DB-DX)/(2.0D0*(DC-DB))
          DCDF=(DTERM1 + DTERM3*DTERM4)/DTERM2
C
          DP1=DCDF
          DPPF=DX
          PPF=REAL(DPPF)
          DFCS = DP1 - DP
          IF(DFCS*DFXL.GT.0.0D0)GOTO110
          DXR = DX
          DFXR = DFCS 
          GOTO115
  110     CONTINUE
          DXL = DX
          DFXL = DFCS
  115     CONTINUE
          DXRML = DXR - DXL
          IF(DXRML.LE.DSIG .AND. DABS(DFCS).LE.DEPS)GOTO9000
          IC = IC + 1
          IF(IC.LE.MAXIT)GOTO105
          WRITE(ICOUT,130)
          CALL DPWRST('XXX','BUG ')
  130     FORMAT(
     1    '***** FATAL ERROR--GENERALIZED TRAPEZOID PPF ROUTINE DID ',
     1    'NOT CONVERGE.')
        GOTO9000
C
      ELSEIF(P.GE.P3 .AND. P.LE.P4)THEN
        DTERM1=2.0D0*(DD-DC)*DNU1
        DPPF=DD - (DD-DC)*((1.0D0-DP)*(DTERM2/DTERM1))**(1.0D0/DNU3)
      ENDIF
      PPF=REAL(DPPF)
C
 9000 CONTINUE
      RETURN
      END 
      SUBROUTINE GTRRAN(N,A,B,C,D,ANU1,ANU3,ALPHA,ISEED,X)
C
C     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
C              FROM THE GENERALIZED TRAPEZOID DISTRIBUTION
C              THIS DISTRIBUTION HAS THE FOLLOWING PDF FUNCTION:
C              f(X,A,B,C,D,N1,N2,ALPHA)
C              = [2*ALPHA*NU1*NU3/
C              (2*ALPHA*(B-A)*NU3+(ALPHA+1)*(C-B)*NU1*NU3+2*(D-C)*NU1)]
C              *((X-A)/(B-A))**(NU1-1)
C                                                     A <= X <  B
C              = [2*NU1*NU3/
C              (2*ALPHA*(B-A)*NU3+(ALPHA+1)*(C-B)*NU1*NU3+2*(D-C)*NU1)]
C              *{(ALPHA-1)*(C-X)/(C-B)+1}
C                                                     B <= X <  C
C              = [2*NU1*NU3/
C              (2*ALPHA*(B-A)*NU3+(ALPHA+1)*(C-B)*NU1*NU3+2*(D-C)*NU1)]
C              *((D-X)/(D-C))**(NU3-1)
C                               = U*((D-X)/(D-C))     C <= X <  D
C                               = 0                   X < A, X >= D
C              WHERE
C                  A <= B <= C <= D, NU1, NU3, ALPHA > 0
C              THIS DISTRIBUTION MODELS THE SIMPLEST CASE OF 
C              A "GROWTH PHASE", A "STABLE PHASE", AND A "DECAY PHASE".
C     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
C                                OF RANDOM NUMBERS TO BE
C                                GENERATED.
C     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
C                                (OF DIMENSION AT LEAST N)
C                                INTO WHICH THE GENERATED
C                                RANDOM SAMPLE WILL BE PLACED.
C                     --A      = THE SINGLE PRECISION SHAPE PARAMETER
C                       B      = THE SINGLE PRECISION SHAPE PARAMETER
C                       C      = THE SINGLE PRECISION SHAPE PARAMETER
C                       D      = THE SINGLE PRECISION SHAPE PARAMETER
C                       ANU1   = THE SINGLE PRECISION SHAPE PARAMETER
C                       ANU3   = THE SINGLE PRECISION SHAPE PARAMETER
C                       ALPHA  = THE SINGLE PRECISION SHAPE PARAMETER
C     OUTPUT--A RANDOM SAMPLE OF SIZE N
C             FROM THE GENERALIZED TRAPEZOID DISTRIBUTION
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN, GTRPPF
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--J. RENE VAN DORP AND SAMIEL KOTZ, "GENERALIZED
C                 GENERALIZED TRAPEZOIDAL DISTRIBUTIONS", METRIKA, VOL. 58,
C                 ISSUE 1, JULY 2003.
C               --TOCHER, THE ART OF SIMULATION,
C                 1963, PAGES 14-15.
C               --HAMMERSLEY AND HANDSCOMB, MONTE CARLO METHODS,
C                 1964, PAGE 36.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2003.6
C     ORIGINAL VERSION--JUNE      2003.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(N.LT.1)THEN
        WRITE(ICOUT, 5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
    5 FORMAT('***** FATAL ERROR--THE REQUESTED NUMBER OF GENERALIZED',
     1' TRAPEZOID RADOM NUMBERS IS NON-POSITIVE *****')
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8,' *****')
      IF(A.GE.B .OR. B.GE.C .OR. C.GE.D)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,13)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,14)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,16)A,B,C,D
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GOTO9000
      ENDIF
   12 FORMAT(
     1'***** FATAL ERROR--FOR THE GENERALIZED TRAPEZOID DISTRIBUTION,')
   13 FORMAT(
     1'      THE FOUR SHAPE PARAMETERS (A, B, C, D) MUST SATISFY')
   14 FORMAT(
     1'         A < B < C < D')
   16 FORMAT(
     1'      A, B, C, D = ',4E15.7)
      IF(ALPHA.LE.0.0)THEN
        WRITE(ICOUT,22)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,23)ALPHA
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GOTO9000
      ENDIF
      IF(ANU1.LE.0.0)THEN
        WRITE(ICOUT,32)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,33)ANU1
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GOTO9000
      ENDIF
      IF(ANU3.LE.0.0)THEN
        WRITE(ICOUT,42)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,43)ANU3
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GOTO9000
      ENDIF
   22 FORMAT(
     1'***** FATAL ERROR--FOR THE GENERALIZED TRAPEZOID DISTRIBUTION,')
   23 FORMAT(
     1'      THE ALPHA SHAPE PARAMETER MUST BE > 0.  ALPHA = ',E15.7)
   32 FORMAT(
     1'***** FATAL ERROR--FOR THE GEnERALIZED TRAPEZOID DISTRIBUTION,')
   33 FORMAT(
     1'      THE NU1 SHAPE PARAMETER MUST BE > 0.  NU1 = ',E15.7)
   42 FORMAT(
     1'***** FATAL ERROR--FOR THE GENERALIZED TRAPEZOID DISTRIBUTION,')
   43 FORMAT(
     1'      THE NU3 SHAPE PARAMETER MUST BE > 0.  NU3 = ',E15.7)
C
C
C     GENERATE N UNIFORM (0,1) RANDOM NUMBERS;
C
      CALL UNIRAN(N,ISEED,X)
C
C     GENERATE N GENERALIZED TRAPEZOID RANDOM NUMBERS
C     USING THE PERCENT POINT FUNCTION TRANSFORMATION METHOD.
C
      DO100I=1,N
      P=X(I)
      CALL GTRPPF(P,A,B,C,D,ANU1,ANU3,ALPHA,PPF)
      X(I)=PPF
  100 CONTINUE
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE GVECT(X,Y,N,
     1                 XTEMP,YTEMP,TATEMP,NTEMP,NTRACE,
     1                 IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--XX
C
C     WRITTEN BY--DAVID W. BEHRINGER NOAA/AOML (MIAMI).
C                 AS PART OF NOAA'S CONCX V.3   MARCH 1988.
C     ORIGINAL VERSION (IN DATAPLOT)--AUGUST    1988.
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      DIMENSION X(*)
      DIMENSION Y(*)
C
      DIMENSION XTEMP(*)
      DIMENSION YTEMP(*)
      DIMENSION TATEMP(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IERROR='NO'
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'VECT')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1011)N
 1011   FORMAT('FROM GVECT--N = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO1015I=1,N
          WRITE(ICOUT,1016)I,X(I),Y(I)
 1016     FORMAT('            I,X(I),Y(I) = ',I8,2F10.5)
          CALL DPWRST('XXX','BUG ')
 1015   CONTINUE
      ENDIF
C
      NTRACE=NTRACE+1
      DO1100I=1,N
      NTEMP=NTEMP+1
      XTEMP(NTEMP)=X(I)
      YTEMP(NTEMP)=Y(I)
      TATEMP(NTEMP)=NTRACE
 1100 CONTINUE
C
      RETURN
      END
      SUBROUTINE GWACDF(X,ALPHA,BETA,K,CDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
C              FUNCTION VALUE FOR THE DISCRETE BETA-NEGATIVE
C              BINOMIAL (OR GENERALIZED WARING) DISTRIBUTION WITH
C              SHAPE PARAMETERS K, ALPHA, AND BETA.
C              THIS DISTRIBUTION IS DEFINED FOR ALL INTEGER X >= 0.
C              THE PROBABILITY MASS FUNCTION IS:
C              P(X;ALPHA,BETA,K)=G(BETA+ALPHA)*G(K+BETA)*
C                  G(X+K)*G(X+ALPHA)/
C                  [G(K)*G(BETA)*G(ALPHA)*G(X+1)*G(X+K+BETA+ALPHA)]
C                  X = 0, 1, 2, ...
C              WHERE G IS THE GAMMA FUNCTION.  NOTE THAT THERE ARE
C              A NUMBER OF DIFFERENT PARAMETERIZATIONS OF THIS
C              DISTRIBUTION IN THE LITERATURE.  WE USE THIS
C              PARAMETERIZATION BECAUSE IT MAKES THE RELATIONSHIPS
C              WITH THE NEGATIVE BINOMIAL, BETA-BINOMIAL, AND
C              BETA-GEOMETRIC CLEAR AND IT ALSO PROVIDES A
C              COMPUTATIONALLY CONVENIENT FORM.
C
C              WE COMPUTE THE CDF USING A RECURENCE RELATION
C              DERIVED BY HESSELAGER:
C
C              P(X) = P(X-1)*[X+(K-1)]*[X+(ALPHA-1)]/
C                     [X*(X+(ALPHA+BETA+K-1))]
C
C     INPUT  ARGUMENTS--X      = THE DOUBLE PRECISION VALUE AT
C                                WHICH THE PROBABILITY DENSITY
C                                FUNCTION IS TO BE EVALUATED.
C                                X SHOULD BE NON-NEGATIVE.
C                     --ALPHA  = THE FIRST SHAPE PARAMETER
C                     --BETA   = THE SECOND SHAPE PARAMETER
C                     --K      = THE THIRD SHAPE PARAMETER
C     OUTPUT ARGUMENTS--CDF    = THE DOUBLE PRECISION DENSITY
C                                FUNCTION VALUE.
C     OUTPUT--THE DOUBLE PRECISION PROBABILITY DENSITY
C             FUNCTION VALUE
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--X SHOULD BE A NON-NEGATIVE INTEGER
C                 --K, ALPHA, AND BETA > 0
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--OLE HESSELAGER (1994).  "A RECURSIVE PROCEDURE
C                 FOR CALCULATIONS OF SOME COMPOUND DISTRIBUTIONS",
C                 ASTIN BULLETIN, VOL. 24, NO. 1, PP. 19-32.
C               --IRWIN (1975), "THE GENERALIZED WARING DISTRIBUTION
C                 PART 1", JOURNAL OF THE ROYAL STATISTICAL SOCIETY,
C                 SERIES A, 138, PP. 18-31.
C               --IRWIN (1975), "THE GENERALIZED WARING DISTRIBUTION
C                 PART 1", JOURNAL OF THE ROYAL STATISTICAL SOCIETY,
C                 SERIES A, 138, PP. 204-227.
C               --IRWIN (1975), "THE GENERALIZED WARING DISTRIBUTION
C                 PART 1", JOURNAL OF THE ROYAL STATISTICAL SOCIETY,
C                 SERIES A, 138, PP. 374-378.
C               --JOHNSON, KOTZ, AND KEMP, "DISCRETE UNIVARIATE
C                 DISTRIBUTIONS", SECOND EDITION, 1992, WILEY,
C                 PP. 242-244.
C               --LUC DEVROYE, "RANDOM VARIATE GENERATION FOR THE
C                 DIGAMMA AND TRIGAMMA DISTRIBUTIONS", JOURNAL OF
C                 STATISTICAL COMPUTATION AND SIMULATION", VOL. 43,
C                 1992, PP. 197-216.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2006/5
C     ORIGINAL VERSION--MAY       2006.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-----------------
C
C-------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION K
C
C----COMMON---------------------------------------------------------
C
      REAL R1MACH
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT---------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      CDF=0.0D0
      IX=INT(X+0.5D0)
      IF(K.LE.0.0D0)THEN
        WRITE(ICOUT,5)
    5   FORMAT('***** ERROR--THE FOURTH ARGUMENT TO GWACDF ',
     1         'IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)K
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ELSEIF(ALPHA.LE.0.0D0)THEN
        WRITE(ICOUT,15)
   15   FORMAT('***** ERROR--THE SECOND ARGUMENT TO GWACDF ',
     1         'IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)ALPHA
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ELSEIF(BETA.LE.0.0D0)THEN
        WRITE(ICOUT,25)
   25   FORMAT('***** ERROR--THE THIRD ARGUMENT TO GWACDF ',
     1         'IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ELSEIF(IX.LT.0)THEN
        WRITE(ICOUT,4)
    4   FORMAT('***** ERROR--THE FIRST ARGUMENT TO GWACDF ',
     1         'IS LESS THAN 0.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ELSEIF(X.GT.DBLE(I1MACH(9)))THEN
        WRITE(ICOUT,55)
   55   FORMAT('***** ERROR--THE FIRST ARGUMENT TO GWACDF ',
     1         'IS GREATER THAN.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,56)
   56   FORMAT('      THE LARGEST MACHINE INTEGER.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
C
      CDF=0.0D0
      DX=0.0D0
      CALL GWAPDF(DX,ALPHA,BETA,K,PDFSV)
C
      CDF=PDFSV
      IF(IX.GT.0)THEN
        DO100I=1,IX
          DX=DBLE(I)
          DTERM1=(DX + (K-1.0D0))*(DX + (ALPHA-1.0D0))
          DTERM2=DX*(DX + (ALPHA+BETA+K-1.0D0))
          PDF=(DTERM1/DTERM2)*PDFSV
          CDF=CDF + PDF
          PDFSV=PDF
  100   CONTINUE
      ENDIF
C
 9999 CONTINUE
      RETURN
      END
      SUBROUTINE GWAPDF(X,ALPHA,BETA,K,PDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
C              FUNCTION VALUE FOR THE DISCRETE BETA-NEGATIVE
C              BINOMIAL (OR GENERALIZED WARING) DISTRIBUTION WITH
C              SHAPE PARAMETERS K, ALPHA, AND BETA.
C              THIS DISTRIBUTION IS DEFINED FOR ALL INTEGER X >= 0.
C              THE PROBABILITY MASS FUNCTION IS:
C              P(X;ALPHA,BETA,K)=G(BETA+ALPHA)*G(K+BETA)*
C                  G(X+K)*G(X+ALPHA)/
C                  [G(K)*G(BETA)*G(ALPHA)*G(X+1)*G(X+K+BETA+ALPHA)]
C                  X = 0, 1, 2, ...
C              WHERE G IS THE GAMMA FUNCTION.  NOTE THAT THERE ARE
C              A NUMBER OF DIFFERENT PARAMETERIZATIONS OF THIS
C              DISTRIBUTION IN THE LITERATURE.  WE USE THIS
C              PARAMETERIZATION BECAUSE IT MAKES THE RELATIONSHIPS
C              WITH THE NEGATIVE BINOMIAL, BETA-BINOMIAL, AND
C              BETA-GEOMETRIC CLEAR AND IT ALSO PROVIDES A
C              COMPUTATIONALLY CONVENIENT FORM.
C     INPUT  ARGUMENTS--X      = THE DOUBLE PRECISION VALUE AT
C                                WHICH THE PROBABILITY DENSITY
C                                FUNCTION IS TO BE EVALUATED.
C                                X SHOULD BE NON-NEGATIVE.
C                     --ALPHA  = THE FIRST SHAPE PARAMETER
C                     --BETA   = THE SECOND SHAPE PARAMETER
C                     --K      = THE THIRD SHAPE PARAMETER
C     OUTPUT ARGUMENTS--PDF    = THE DOUBLE PRECISION DENSITY
C                                FUNCTION VALUE.
C     OUTPUT--THE DOUBLE PRECISION PROBABILITY DENSITY
C             FUNCTION VALUE
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--X SHOULD BE A NON-NEGATIVE INTEGER
C                 --C, ALPHA, AND BETA > 0
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--OLE HESSELAGER (1994).  "A RECURSIVE PROCEDURE
C                 FOR CALCULATIONS OF SOME COMPOUND DISTRIBUTIONS",
C                 ASTIN BULLETIN, VOL. 24, NO. 1, PP. 19-32.
C               --IRWIN (1975), "THE GENERALIZED WARING DISTRIBUTION
C                 PART 1", JOURNAL OF THE ROYAL STATISTICAL SOCIETY,
C                 SERIES A, 138, PP. 18-31.
C               --IRWIN (1975), "THE GENERALIZED WARING DISTRIBUTION
C                 PART 1", JOURNAL OF THE ROYAL STATISTICAL SOCIETY,
C                 SERIES A, 138, PP. 204-227.
C               --IRWIN (1975), "THE GENERALIZED WARING DISTRIBUTION
C                 PART 1", JOURNAL OF THE ROYAL STATISTICAL SOCIETY,
C                 SERIES A, 138, PP. 374-378.
C               --JOHNSON, KOTZ, AND KEMP, "DISCRETE UNIVARIATE
C                 DISTRIBUTIONS", SECOND EDITION, 1992, WILEY,
C                 PP. 242-244.
C               --LUC DEVROYE, "RANDOM VARIATE GENERATION FOR THE
C                 DIGAMMA AND TRIGAMMA DISTRIBUTIONS", JOURNAL OF
C                 STATISTICAL COMPUTATION AND SIMULATION", VOL. 43,
C                 1992, PP. 197-216.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2006/5
C     ORIGINAL VERSION--MAY       2006.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-----------------
C
C-------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION K
C
C-----COMMON--------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT---------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      PDF=0.0D0
      IX=INT(X+0.5D0)
      IF(K.LE.0.0D0)THEN
        WRITE(ICOUT,5)
    5   FORMAT('***** ERROR--THE FOURTH ARGUMENT TO GWAPDF ',
     1         'IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)K
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ELSEIF(ALPHA.LE.0.0D0)THEN
        WRITE(ICOUT,15)
   15   FORMAT('***** ERROR--THE SECOND ARGUMENT TO GWAPDF ',
     1         'IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)ALPHA
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ELSEIF(BETA.LE.0.0D0)THEN
        WRITE(ICOUT,25)
   25   FORMAT('***** ERROR--THE THIRD ARGUMENT TO GWAPDF ',
     1         'IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ELSEIF(IX.LT.0)THEN
        WRITE(ICOUT,4)
    4   FORMAT('***** ERROR--THE FIRST ARGUMENT TO GWAPDF ',
     1         'IS LESS THAN 0.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ELSEIF(X.GT.DBLE(I1MACH(9)))THEN
        WRITE(ICOUT,55)
   55   FORMAT('***** ERROR--THE FIRST ARGUMENT TO GWAPDF ',
     1         'IS GREATER THAN.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,56)
   56   FORMAT('      THE LARGEST MACHINE INTEGER.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
C
      DX=DBLE(IX)
      DA=ALPHA
      DB=BETA
      DC=K
C
      DTERM1=DLNGAM(DX+DC)
      DTERM2=DLNGAM(DA+DB)
      DTERM3=DLNGAM(DX+DA)
      DTERM4=DLNGAM(DC+DB)
      DNUM=DTERM1+DTERM2+DTERM3+DTERM4
C
      DTERM5=DLNGAM(DA)
      DTERM6=DLNGAM(DB)
      DTERM7=DLNGAM(DC)
      DTERM8=DLNGAM(DX+1.0D0)
      DTERM9=DLNGAM(DX+DA+DB+DC)
      DENOM=DTERM5+DTERM6+DTERM7+DTERM8+DTERM9
C
      DTERM9=DNUM-DENOM
      PDF=DEXP(DNUM-DENOM)
C
 9999 CONTINUE
      RETURN
      END
      SUBROUTINE GWAPPF(P,ALPHA,BETA,K,PPF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
C              FUNCTION VALUE FOR THE DISCRETE BETA-NEGATIVE
C              BINOMIAL (OR GENERALIZED WARING) DISTRIBUTION WITH
C              SHAPE PARAMETERS K, ALPHA, AND BETA.
C              THIS DISTRIBUTION IS DEFINED FOR 0 <= P < 1.
C              THE PROBABILITY MASS FUNCTION IS:
C              P(X;ALPHA,BETA,K)=G(BETA+ALPHA)*G(K+BETA)*
C                  G(X+K)*G(X+ALPHA)/
C                  [G(K)*G(BETA)*G(ALPHA)*G(X+1)*G(X+K+BETA+ALPHA)]
C                  X = 0, 1, 2, ...
C              WHERE G IS THE GAMMA FUNCTION.  NOTE THAT THERE ARE
C              A NUMBER OF DIFFERENT PARAMETERIZATIONS OF THIS
C              DISTRIBUTION IN THE LITERATURE.  WE USE THIS
C              PARAMETERIZATION BECAUSE IT MAKES THE RELATIONSHIPS
C              WITH THE NEGATIVE BINOMIAL, BETA-BINOMIAL, AND
C              BETA-GEOMETRIC CLEAR AND IT ALSO PROVIDES A
C              COMPUTATIONALLY CONVENIENT FORM.
C
C              WE COMPUTE THE CDF USING A RECURENCE RELATION
C              DERIVED BY HESSELAGER:
C
C              P(X) = P(X-1)*[X+(K-1)]*[X+(ALPHA-1)]/
C                     [X*(X+(ALPHA+BETA+K-1))]
C
C              WE COMPUTE THE PERCENT POINT FUNCTION BY COMPUTING
C              THE CDF FUNCTION UNTIL THE SPECIFIED VALUE OF P
C              IS REACHED.
C
C     INPUT  ARGUMENTS--P      = THE DOUBLE PRECISION VALUE AT
C                                WHICH THE PERCENT POINT
C                                FUNCTION IS TO BE EVALUATED.
C                                0 <= P < 1.
C                     --ALPHA  = THE FIRST SHAPE PARAMETER
C                     --BETA   = THE SECOND SHAPE PARAMETER
C                     --K      = THE THIRD SHAPE PARAMETER
C     OUTPUT ARGUMENTS--PPF    = THE DOUBLE PRECISION PERCENT POINT
C                                FUNCTION VALUE.
C     OUTPUT--THE DOUBLE PRECISION PERCENT POINT FUNCTION VALUE
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--0 <= P < 1.
C                 --K, ALPHA, AND BETA > 0
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--OLE HESSELAGER (1994).  "A RECURSIVE PROCEDURE
C                 FOR CALCULATIONS OF SOME COMPOUND DISTRIBUTIONS",
C                 ASTIN BULLETIN, VOL. 24, NO. 1, PP. 19-32.
C               --IRWIN (1975), "THE GENERALIZED WARING DISTRIBUTION
C                 PART 1", JOURNAL OF THE ROYAL STATISTICAL SOCIETY,
C                 SERIES A, 138, PP. 18-31.
C               --IRWIN (1975), "THE GENERALIZED WARING DISTRIBUTION
C                 PART 1", JOURNAL OF THE ROYAL STATISTICAL SOCIETY,
C                 SERIES A, 138, PP. 204-227.
C               --IRWIN (1975), "THE GENERALIZED WARING DISTRIBUTION
C                 PART 1", JOURNAL OF THE ROYAL STATISTICAL SOCIETY,
C                 SERIES A, 138, PP. 374-378.
C               --JOHNSON, KOTZ, AND KEMP, "DISCRETE UNIVARIATE
C                 DISTRIBUTIONS", SECOND EDITION, 1992, WILEY,
C                 PP. 242-244.
C               --LUC DEVROYE, "RANDOM VARIATE GENERATION FOR THE
C                 DIGAMMA AND TRIGAMMA DISTRIBUTIONS", JOURNAL OF
C                 STATISTICAL COMPUTATION AND SIMULATION", VOL. 43,
C                 1992, PP. 197-216.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2006/5
C     ORIGINAL VERSION--MAY       2006.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-----------------
C
C-------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION K
      DOUBLE PRECISION DEPS
      DOUBLE PRECISION DP
C
C-----COMMON--------------------------------------------------------
C
      REAL R1MACH
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT---------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      PPF=0.0D0
      IF(K.LE.0.0D0)THEN
        WRITE(ICOUT,5)
    5   FORMAT('***** ERROR--THE FOURTH ARGUMENT TO GWACDF ',
     1         'IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)K
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ELSEIF(ALPHA.LE.0.0D0)THEN
        WRITE(ICOUT,15)
   15   FORMAT('***** ERROR--THE SECOND ARGUMENT TO GWACDF ',
     1         'IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)ALPHA
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ELSEIF(BETA.LE.0.0D0)THEN
        WRITE(ICOUT,25)
   25   FORMAT('***** ERROR--THE THIRD ARGUMENT TO GWACDF ',
     1         'IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ELSEIF(P.LT.0.0.OR.P.GE.1.0)THEN
        WRITE(ICOUT,1)
    1   FORMAT('***** ERROR--THE FIRST ARGUMENT TO GWAPPF ',
     1         'IS OUTSIDE THE ALLOWABLE (0,1] INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C     COMPUTE PDF FOR X = 0
C
      EPS=1.0E-6
      DEPS=1.0D-6
      DX=0.0D0
      CALL GWAPDF(DX,ALPHA,BETA,K,DSUM)
      IF(DSUM.GE.P-EPS)THEN
        PPF=0.0D0
        GOTO9999
      ENDIF
      DPDFSV=DSUM
      I=0
C
      DP=DBLE(P)
  100 CONTINUE
        I=I+1
        IF(DBLE(I).GE.DBLE(I1MACH(9)))THEN
          WRITE(ICOUT,65)
   65     FORMAT('***** ERROR--THE COMPUTED PERCENT POINT VALUE ',
     1           'EXCEEDS THE LARGEST MACHINE INTEGER.')
          CALL DPWRST('XXX','BUG ')
          PPF=0.0D0
          GOTO9999
        ENDIF
        DX=DBLE(I)
        DTERM1=(DX + (K-1.0D0))*(DX + (ALPHA-1.0D0))
        DTERM2=DX*(DX + (ALPHA+BETA+K-1.0D0))
        DPDF=(DTERM1/DTERM2)*DPDFSV
        DPDFSV=DPDF
        DSUM=DSUM + DPDF
        IF(DSUM.GE.DP-DEPS)THEN
          PPF=DBLE(I)
          GOTO9999
        ENDIF
      GOTO100
C
 9999 CONTINUE
      RETURN
      END
      SUBROUTINE GWARAN(N,ALPHA,BETA,K,ISEED,X)
C
C     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
C              FROM THE BETA NEGATIVE BINOMIAL (GENERALIZED WARING)
C              DISTRIBUTION WITH SINGLE PRECISION SHAPE PARAMETERS
C              K, ALPHA, AND BETA.
C              THIS DISTRIBUTION IS DEFINED FOR ALL INTEGER X >= 0.
C              THE PROBABILITY MASS FUNCTION IS:
C              P(X;ALPHA,BETA,K)=G(BETA+ALPHA)*G(K+BETA)*
C                  G(X+K)*G(X+ALPHA)/
C                  [G(K)*G(BETA)*G(ALPHA)*G(X+1)*G(X+K+BETA+ALPHA)]
C                  X = 0, 1, 2, ...
C              WHERE G IS THE GAMMA FUNCTION.  NOTE THAT THERE ARE
C              A NUMBER OF DIFFERENT PARAMETERIZATIONS OF THIS
C              DISTRIBUTION IN THE LITERATURE.  WE USE THIS
C              PARAMETERIZATION BECAUSE IT MAKES THE RELATIONSHIPS
C              WITH THE NEGATIVE BINOMIAL, BETA-BINOMIAL, AND
C              BETA-GEOMETRIC CLEAR AND IT ALSO PROVIDES A
C              COMPUTATIONALLY CONVENIENT FORM.
C     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
C                                OF RANDOM NUMBERS TO BE
C                                GENERATED.
C                     --ALPHA  = THE FIRST SHAPE PARAMETER
C                     --BETA   = THE SECOND SHAPE PARAMETER
C                     --K      = THE THIRD SHAPE PARAMETER
C     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
C                                (OF DIMENSION AT LEAST N)
C                                INTO WHICH THE GENERATED
C                                RANDOM SAMPLE WILL BE PLACED.
C     OUTPUT--A RANDOM SAMPLE OF SIZE N FROM THE BETA NEGATIVE
C             BINOMIAL (GENERALIZED WARING) DISTRIBUTION
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C                 --K, ALPHA, BETA SHOULD BE > 0
C     OTHER DATAPAC   SUBROUTINES NEEDED--GAMRAN, POIRAN.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--LUC DEVROYE, "RANDOM VARIATE GENERATION FOR THE
C                 DIGAMMA AND TRIGAMMA DISTRIBUTIONS", JOURNAL OF
C                 STATISITCAL COMPUTATION AND SIMULATION", VOL. 43,
C                 PP. 197-216, 1992.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2004/4
C     ORIGINAL VERSION--APRIL     2004.
C     UPDATED         --MAY       2006.  UPDATE TO USE BETA-
C                                        NEGATIVE BINOMIAL
C                                        PARAMETERIZATION (RANDOM
C                                        NUMBER GENERATION ALGORITHM
C                                        NOT MODIFIED)
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-----------------
C
C-------------------------------------------------------------------
C
      REAL K
C
      DIMENSION X(*)
      DIMENSION U(2)
C
C-----COMMON--------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT---------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
C     CONVERT FROM BETA-NEGATIVE BINOMIAL PARAMETERIZATION TO
C     DEVROYE PARAMETERIZATION.
C
      A=K
      B=ALPHA
      C=BETA
C
      IF(N.LT.1)THEN
        WRITE(ICOUT,5)
    5   FORMAT('***** ERROR--NUMBER OF GENERALIZED WARING RANDOM ',
     1         'NUMBERS REQUESTED IS LESS THAN 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
   47   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ELSEIF(A.LE.0.0)THEN
        WRITE(ICOUT,15)
   15   FORMAT('***** ERROR--THE K SHAPE PARAMETER FOR THE ',
     1         'GENERALIZED WARING DISTRIBUTION IS <= 0.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)A
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ELSEIF(B.LE.0.0)THEN
        WRITE(ICOUT,16)
   16   FORMAT('***** ERROR--THE ALPHA SHAPE PARAMETER FOR THE ',
     1         'GENERALIZED WARING DISTRIBUTION IS <= 0.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)B
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ELSEIF(C.LE.0.0)THEN
        WRITE(ICOUT,17)
   17   FORMAT('***** ERROR--THE BETA SHAPE PARAMETER FOR THE ',
     1         'GENERALIZED WARING DISTRIBUTION IS <= 0.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)C
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
C
C     GENERATE N GENERALIZED WARING  RANDOM NUMBERS
C
C     ALGORITHM:
C       1. GENERATE THREE GAMMA RANDOM NUMBERS WITH SHAPE PARAMETERS
C          A, B, AND C (SCALE PARAMETER = 1).
C       2. GAMMA(A)*GAMMA(B)/GAMMA(C) = LAMBDA
C       3. GENERATE A POISSON RANDOM NUMBER WITH SHAPE PARAMETER
C          LAMBDA
C
      NTEMP=1
      DO100I=1,N
        CALL GAMRAN(NTEMP,A,ISEED,U)
        U1=U(1)
        CALL GAMRAN(NTEMP,B,ISEED,U)
        U2=U(1)
        CALL GAMRAN(NTEMP,C,ISEED,U)
        U3=U(1)
        ALAMB=U1*U2/U3
        CALL POIRAN(NTEMP,ALAMB,ISEED,U)
        X(I)=U(1)
  100 CONTINUE
C
 9999 CONTINUE
      RETURN
      END

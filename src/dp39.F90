      SUBROUTINE GRTRIN(IX,NCHTO2,ICSTR,NCSTR)
!CCCC SUBROUTINE GRTRIN(IX,NCHTOT,ICSTR,NCSTR)
!
!     PURPOSE--TRANSLATE THE INTEGER IX INTO ITS CHARACTER EQUIVALENT.
!              NCHTOT = DESIRED NUMBER OF DIGITS IN INTEGER FORMAT (SO
!              THAT THE OUTPUT WILL CORRESPOND TO   I(NCHTOT)   FORMAT).
!              THE OUTPUT WILL BE RIGHT-JUSTIFIED AS ONE WOULD EXPECT
!              FROM AN I FORTRAN INTEGER FORMAT.
!     NOTE--THE RESULTING TRANSLATED VALUES WILL BE PLACED IN SPECIFIC
!           ELEMENTS OF THE CHARACTER VARIABLE ICSTR.  THE VALUE OF THE
!           VARIABLE    NCSTR   REPRESENTS THE NUMBER OF CHARACTERS IN
!           ICSTR THAT HAVE ALREADY BEEN FILLED.  THE RESULTING TRANSLATED
!           VALUES WILL GO (RIGHT-JUSTIFIED) INTO THE NEXT    NCHTOT
!           CHARACTERS OF ICSTR AND THE VALUE OF    NCSTR    WILL BE
!           UPDATED ACCORDINGLY, THAT IS, NEW NCSTR = OLD NCSTR + NCHTOT
!     DANGER--NCSTR IS BOTH AN INPUT ARGUMENT
!             AND AN OUTPUT ARGUMENT OF THIS SUBROUTINE.
!
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--83.6
!     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MAY       1983.
!     UPDATED         --MAY 1988
!                       IF NCHTOT IS NEGATIVE, MAKE LEADING ZEROS
!                       EXPLICIT ZEROS RATHER THAN SPACES.  THIS IS
!                       REQUIRED BY THE QUIC DRIVER IN PARTICULAR.
!     UPDATED         --JANUARY  1989. SUN (BY BILL ANDERSON)
!     UPDATED         --JANUARY  1989. POSTSCRIPT (BY ALAN HECKERT)
!     UPDATED         --JANUARY  1989. CGM (BY ALAN HECKERT)
!     UPDATED         --JANUARY  1989. QMS QUIC (BY ALAN HECKERT)
!     UPDATED         --JANUARY  1989. CALCOMP (BY ALAN HECKERT)
!     UPDATED         --JANUARY  1989. ZETA (BY ALAN HECKERT)
!     UPDATED         --JULY     1996. LAHEY DRIVER (ALAN HECKERT)
!     UPDATED         --OCTOBER  1996. QUICKWIN DRIVER (ALAN)
!     UPDATED         --OCTOBER  1996. OPENGL DRIVER (ALAN)
!
!-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
!
      CHARACTER*(*) ICSTR
!
      DIMENSION IDIGIT(20)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!  MAY,1988: CHECK FOR NEGATIVE NCHTOT
      NCHTOT=ABS(NCHTO2)
      IASC0=48
      IREV=(-999)
      IDIG=(-999)
!
      IERRG4='NO'
!
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
          DO 65 I=1,NCSTR
            WRITE(ICOUT,66)I,ICSTR(I:I)
   66       FORMAT('I,ICSTR(I:I) = ',I8,2X,A1)
            CALL DPWRST('XXX','BUG ')
   65     CONTINUE
        ENDIF
        WRITE(ICOUT,69)IBUGG4,ISUBG4,IERRG4
   69   FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ***********************************
!               **  STEP 1--                     **
!               **  SAVE THE OLD VALUE OF NCSTR  **
!               ***********************************
!
      NCSOLD=NCSTR
!
!
!               ********************************************
!               **  STEP 2--                              **
!               **  FILL THE NEXT   NCHTOT   ELEMENTS     **
!               **  IN ICSTR() WITH BLANKS.               **
!               **  UPDATE NCSTR BY NCSTR + NCHTOT.       **
!               **  MAY,1988: - FILL WITH EXPLICIT ZEROS  **
!               **  IF NCHTOT SENT AS NEGATIVE            **
!               ********************************************
!
!
      IF(NCHTO2.LT.0)THEN
        DO 1295 I=1,NCHTOT
          NCSTR=NCSTR+1
          ICSTR(NCSTR:NCSTR)='0'
 1295   CONTINUE
      ELSE
        DO 1200 I=1,NCHTOT
          NCSTR=NCSTR+1
          ICSTR(NCSTR:NCSTR)=' '
 1200   CONTINUE
      ENDIF
!
!               ********************************
!               **  STEP 3--                  **
!               **  STRIP OUT THE DIGITS      **
!               **  (IN RIGHT TO LEFT ORDER)  **
!               ********************************
!
      ITEN=10
!
      IABSIX=IABS(IX)
      IRESUL=IABSIX
!
      NUMDIG=0
      DO 1300 I=1,20
        IDIVID=IRESUL
        IRESUL=IDIVID/ITEN
        IREM=IDIVID-IRESUL*ITEN
        NUMDIG=NUMDIG+1
        IDIGIT(NUMDIG)=IREM
        IF(IRESUL.EQ.0)GO TO 1390
 1300 CONTINUE
 1390 CONTINUE
!
!               *****************************************************
!               **   STEP 4--                                      **
!               **   CHECK TO SEE THAT TOTAL NUMBER OF CHARACTERS  **
!               **   (= NUMBER OF DIGITS IF IX >= 0, AND           **
!               **   = NUMBER OF DIGITS + 1 IF IX < 0)             **
!               **   IS LESS THAN OR EQUAL TO NCHTOT.              **
!               **   IF YES, THEN GO TO NEXT STEP.                 **
!               **   IF NO, THEN FILL THE NCHTOT ELEMENTS          **
!               **  WITH ASTERISKS                                 **
!               *****************************************************
!
      NUMTOT=NUMDIG
      IF(IX.LT.0)NUMTOT=NUMTOT+1
!
      IF(NUMTOT.GT.NCHTOT)THEN
        DO 1400 I=1,NCHTOT
          J=I+NCSOLD
          ICSTR(J:J)='*'
 1400   CONTINUE
        GO TO 9000
      ENDIF
!
!               ****************************************
!               **  STEP 5--                          **
!               **  IF HAVE A VALID NUMBER OF DIGITS, **
!               **  TRANSLATE EACH DIGIT TO ITS       **
!               **  CHARACTER EQUIVALENT.             **
!               **  IN DOING SO, PLACE THE ASCII      **
!               **  CHARACTER EQUIVALENT              **
!               **  IN THE USUAL RIGHT-TO-LEFT ORDER  **
!               **  IN ICSTR(.).  IF IX < 0, INSERT   **
!               **  A MINUS BEFORE THE NUMBER.        **
!               **  HAVE THE CHARACTER STRING         **
!               **  RIGHT JUSTIFIED IN THE            **
!               **  NCHTOT ELEMENTS.                  **
!               ****************************************
!
      J=NCSOLD+(NCHTOT-NUMTOT)
      IF(IX.LT.0)J=J+1
      IF(IX.LT.0)ICSTR(J:J)='-'
!
      DO 1500 I=1,NUMDIG
        IREV=NUMDIG-I+1
        IDIG=IDIGIT(IREV)
        J=J+1
        IASCDI=IDIG+IASC0
        CALL DPCONA(IASCDI,ICSTR(J:J))
 1500 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
          DO 9035 I=1,NUMDIG
            WRITE(ICOUT,9036)I,IDIGIT(I)
 9036       FORMAT('I,IDIGIT(I) = ',2I8)
            CALL DPWRST('XXX','BUG ')
 9035     CONTINUE
        ENDIF
        IF(NCSTR.GT.0)THEN
          DO 9045 I=1,NCSTR
            WRITE(ICOUT,9046)I,ICSTR(I:I)
 9046       FORMAT('I,ICSTR(I:I) = ',I8,2X,A1)
            CALL DPWRST('XXX','BUG ')
 9045     CONTINUE
        ENDIF
        WRITE(ICOUT,9049)IBUGG4,ISUBG4,IERRG4
 9049   FORMAT('IBUGG4,ISUBG4,IERRG4 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE GRTRIN
      SUBROUTINE GRTRI1(IX,NCHTO2,ICSTR,NCSTR,IJUST)
!
!     PURPOSE--TRANSLATE THE INTEGER IX INTO ITS CHARACTER EQUIVALENT.
!              NCHTOT = DESIRED NUMBER OF DIGITS IN INTEGER FORMAT (SO
!              THAT THE OUTPUT WILL CORRESPOND TO   I(NCHTOT)   FORMAT).
!              THE OUTPUT WILL BE RIGHT-JUSTIFIED AS ONE WOULD EXPECT
!              FROM AN I FORTRAN INTEGER FORMAT.
!
!              THIS IS A MODIFIED VERSION OF GRTRIN THAT ALLOWS LEFT OR
!              CENTER JUSTIFICATION.
!     NOTE--THE RESULTING TRANSLATED VALUES WILL BE PLACED IN SPECIFIC
!           ELEMENTS OF THE CHARACTER VARIABLE ICSTR.  THE VALUE OF THE
!           VARIABLE    NCSTR   REPRESENTS THE NUMBER OF CHARACTERS IN
!           ICSTR THAT HAVE ALREADY BEEN FILLED.  THE RESULTING TRANSLATED
!           VALUES WILL GO (RIGHT-JUSTIFIED) INTO THE NEXT    NCHTOT
!           CHARACTERS OF ICSTR AND THE VALUE OF    NCSTR    WILL BE
!           UPDATED ACCORDINGLY, THAT IS, NEW NCSTR = OLD NCSTR + NCHTOT
!     DANGER--NCSTR IS BOTH AN INPUT ARGUMENT
!             AND AN OUTPUT ARGUMENT OF THIS SUBROUTINE.
!
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2015.4
!     ORIGINAL VERSION--APRIL     2015.
!
!-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
!
      CHARACTER*4   IJUST
      CHARACTER*(*) ICSTR
!
      DIMENSION IDIGIT(20)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      NCHTOT=ABS(NCHTO2)
      IASC0=48
      IREV=(-999)
      IDIG=(-999)
!
      IERRG4='NO'
!
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
          DO 65 I=1,NCSTR
            WRITE(ICOUT,66)I,ICSTR(I:I)
   66       FORMAT('I,ICSTR(I:I) = ',I8,2X,A1)
            CALL DPWRST('XXX','BUG ')
   65     CONTINUE
        ENDIF
        WRITE(ICOUT,69)IBUGG4,ISUBG4,IERRG4
   69   FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ***********************************
!               **  STEP 1--                     **
!               **  SAVE THE OLD VALUE OF NCSTR  **
!               ***********************************
!
      NCSOLD=NCSTR
!
!
!               ********************************************
!               **  STEP 2--                              **
!               **  FILL THE NEXT   NCHTOT   ELEMENTS     **
!               **  IN ICSTR() WITH BLANKS.               **
!               **  UPDATE NCSTR BY NCSTR + NCHTOT.       **
!               **  MAY,1988: - FILL WITH EXPLICIT ZEROS  **
!               **  IF NCHTOT SENT AS NEGATIVE            **
!               ********************************************
!
!
      IF(NCHTO2.LT.0)THEN
        DO 1295 I=1,NCHTOT
          NCSTR=NCSTR+1
          ICSTR(NCSTR:NCSTR)='0'
 1295   CONTINUE
      ELSE
        DO 1200 I=1,NCHTOT
          NCSTR=NCSTR+1
          ICSTR(NCSTR:NCSTR)=' '
 1200   CONTINUE
      ENDIF
!
!               ********************************
!               **  STEP 3--                  **
!               **  STRIP OUT THE DIGITS      **
!               **  (IN RIGHT TO LEFT ORDER)  **
!               ********************************
!
      ITEN=10
!
      IABSIX=IABS(IX)
      IRESUL=IABSIX
!
      NUMDIG=0
      DO 1300 I=1,20
        IDIVID=IRESUL
        IRESUL=IDIVID/ITEN
        IREM=IDIVID-IRESUL*ITEN
        NUMDIG=NUMDIG+1
        IDIGIT(NUMDIG)=IREM
        IF(IRESUL.EQ.0)GO TO 1390
 1300 CONTINUE
 1390 CONTINUE
!
!               *****************************************************
!               **   STEP 4--                                      **
!               **   CHECK TO SEE THAT TOTAL NUMBER OF CHARACTERS  **
!               **   (= NUMBER OF DIGITS IF IX >= 0, AND           **
!               **   = NUMBER OF DIGITS + 1 IF IX < 0)             **
!               **   IS LESS THAN OR EQUAL TO NCHTOT.              **
!               **   IF YES, THEN GO TO NEXT STEP.                 **
!               **   IF NO, THEN FILL THE NCHTOT ELEMENTS          **
!               **  WITH ASTERISKS                                 **
!               *****************************************************
!
      NUMTOT=NUMDIG
      IF(IX.LT.0)NUMTOT=NUMTOT+1
!
      IF(NUMTOT.GT.NCHTOT)THEN
        DO 1400 I=1,NCHTOT
          J=I+NCSOLD
          ICSTR(J:J)='*'
 1400   CONTINUE
        GO TO 9000
      ENDIF
!
!               ****************************************
!               **  STEP 5--                          **
!               **  IF HAVE A VALID NUMBER OF DIGITS, **
!               **  TRANSLATE EACH DIGIT TO ITS       **
!               **  CHARACTER EQUIVALENT.             **
!               **  IN DOING SO, PLACE THE ASCII      **
!               **  CHARACTER EQUIVALENT              **
!               **  IN THE USUAL RIGHT-TO-LEFT ORDER  **
!               **  IN ICSTR(.).  IF IX < 0, INSERT   **
!               **  A MINUS BEFORE THE NUMBER.        **
!               **  HAVE THE CHARACTER STRING         **
!               **  RIGHT JUSTIFIED IN THE            **
!               **  NCHTOT ELEMENTS.                  **
!               ****************************************
!
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
!
      IF(IX.LT.0)J=J+1
      IF(IX.LT.0)ICSTR(J:J)='-'
!
      DO 1500 I=1,NUMDIG
        IREV=NUMDIG-I+1
        IDIG=IDIGIT(IREV)
        J=J+1
        IASCDI=IDIG+IASC0
        CALL DPCONA(IASCDI,ICSTR(J:J))
 1500 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
          DO 9035 I=1,NUMDIG
            WRITE(ICOUT,9036)I,IDIGIT(I)
 9036       FORMAT('I,IDIGIT(I) = ',2I8)
            CALL DPWRST('XXX','BUG ')
 9035     CONTINUE
        ENDIF
        IF(NCSTR.GT.0)THEN
          DO 9045 I=1,NCSTR
            WRITE(ICOUT,9046)I,ICSTR(I:I)
 9046       FORMAT('I,ICSTR(I:I) = ',I8,2X,A1)
            CALL DPWRST('XXX','BUG ')
 9045     CONTINUE
        ENDIF
        WRITE(ICOUT,9049)IBUGG4,ISUBG4,IERRG4
 9049   FORMAT('IBUGG4,ISUBG4,IERRG4 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE GRTRI1
      SUBROUTINE GRTRPG(IPATT,ISTRIN,NCSTRI)
!
!     PURPOSE--TRANSLATE A 1-WORD (4-CHARACTER) REPRESENTATION
!              FOR A MARKER INTO A MULTI-WORD REPRESENTATION
!              THAT SUBROUTINE DPSCR7 CAN UNDERSTAND.
!
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--83.6
!     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MAY       1983.
!
!-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
!
      CHARACTER*(*) IPATT
      CHARACTER*4 ISTRIN
!
      CHARACTER*1 IC1
!
      DIMENSION ISTRIN(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERRG4='NO'
!
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
!
      NCSTRI=0
      DO 1100 I=1,16
        ISTRIN(I)='    '
 1100 CONTINUE
!
      DO 1200 I=1,24
        IREV=16-I+1
        IC1=IPATT(IREV:IREV)
        IF(IC1.NE.' ')GO TO 1250
 1200 CONTINUE
 1250 CONTINUE
      NCSTRI=IREV
      IF(NCSTRI.LE.0)NCSTRI=1
!
      DO 1300 I=1,NCSTRI
        ISTRIN(I)=IPATT(I:I)
 1300 CONTINUE
!
      IF(NCSTRI.GT.1)THEN
        NCSTRI=NCSTRI+1
        ISTRIN(NCSTRI)='('
        NCSTRI=NCSTRI+1
        ISTRIN(NCSTRI)=')'
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRPG')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF GRTRPG--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NCSTRI
 9013   FORMAT('NCSTRI = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,NCSTRI
          WRITE(ICOUT,9016)I,ISTRIN(I)
 9016     FORMAT('I,ISTRIN(I) = ',I8,2X,A4)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE GRTRPG
      SUBROUTINE GRTRRE(X,NCHTOT,NCHDEC,ICSTR,NCSTR)
!
!     PURPOSE--TRANSLATE THE REAL (= FLOATING POINT) NUMBER X
!              INTO ITS CHARACTER EQUIVALENT.
!              NCHTOT = DESIRED NUMBER OF CHARACTERS FOR TOTAL NUMBER.
!              NCTDEC = DESIRED NUMBER OF CHARACTERS FOR DECIMAL PART OF NUMBER.
!              THE OUTPUT WILL CORRESPOND TO A FORTRAN F FORMAT.
!              AS IN    F NCHTOT.NCHDEC     FORMAT (E.G., F10.5).
!              THE OUTPUT WILL BE RIGHT-JUSTIFIED AS ONE
!              WOULD EXPECT FROM A F FORTRAN FLOATING POINT FORMAT.
!     NOTE--THE RESULTING TRANSLATED VALUES
!           WILL BE PLACED IN SPECIFIC ELEMENTS
!           OF THE A130 CHARACTER VARIABLE ICSTR.
!           THE VALUE OF THE VARIABLE    NCSTR
!           REPRESENTS THE NUMBER OF CHARACTERS IN ICSTR
!           THAT HAVE ALREADY BEEN FILLED.
!           THE RESULTING TRANSLATED VALUES WILL GO
!           (RIGHT-JUSTIFIED) INTO THE NEXT    NCHTOT
!           CHARACTERS OF ICSTR.
!           AND THE VALUE OF    NCSTR    WILL BE
!           UPDATED ACCORDINGLY, THAT IS,
!           NEW NCSTR = OLD NCSTR + NCHTOT
!     DANGER--NCSTR IS BOTH AN INPUT ARGUMENT
!             AND AN OUTPUT ARGUMENT OF THIS SUBROUTINE.
!
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--83.6
!     ORIGINAL VERSION --MAY       1983.
!     UPDATED          --MAY       2014. ROUND NUMBER FOR DECIMAL PART
!                                        FIRST
!
!-----NON-COMMON VARIABLES (GRAPHICS)---------------------------------
!
      CHARACTER*(*) ICSTR
!
      DIMENSION IINTDI(20)
      DIMENSION IDECDI(20)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IASC0=48
      IREV=(-999)
      IDIG=(-999)
      IERRG4='NO'
!
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
          DO 65 I=1,NCSTR
            WRITE(ICOUT,66)I,ICSTR(I:I)
   66       FORMAT('I,ICSTR(I:I) = ',I8,2X,A1)
            CALL DPWRST('XXX','BUG ')
   65     CONTINUE
        ENDIF
        WRITE(ICOUT,69)IBUGG4,ISUBG4,IERRG4
   69   FORMAT('IBUGG4,ISUBG4,IERRG4 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ***********************************
!               **  STEP 1--                     **
!               **  SAVE THE OLD VALUE OF NCSTR  **
!               ***********************************
!
      NCSOLD=NCSTR
!
!     2014/05: ROUND NUMBER FIRST
!
      EPS=5./(10**(NCHDEC+1))
      IF(X.GE.0.0)THEN
        XRND=X+EPS
      ELSE
        XRND=X-EPS
      ENDIF
!CCCC XRND=RND(X,NCHDEC)
!
!
!               ********************************************
!               **  STEP 2--                              **
!               **  FILL THE NEXT   NCHTOT   ELEMENTS     **
!               **  IN ICSTR() WITH BLANKS.               **
!               **  UPDATE NCSTR BY NCSTR + NCHTOT.       **
!               ********************************************
!
      DO 1200 I=1,NCHTOT
        NCSTR=NCSTR+1
        ICSTR(NCSTR:NCSTR)=' '
 1200 CONTINUE
!
!               ********************************
!               **  STEP 3--                  **
!               **  STRIP OUT THE DIGITS      **
!               **  OF THE INTEGER PART       **
!               **  (IN RIGHT TO LEFT ORDER)  **
!               ********************************
!
      ITEN=10
!
      IX=INT(XRND)
      IABSIX=IABS(IX)
!
      IRESUL=IABSIX
      NUMIND=0
      DO 1300 I=1,20
        IDIVID=IRESUL
        IRESUL=IDIVID/ITEN
        IREM=IDIVID-IRESUL*ITEN
        NUMIND=NUMIND+1
        IINTDI(NUMIND)=IREM
        IF(IRESUL.EQ.0)GO TO 1390
 1300 CONTINUE
 1390 CONTINUE
!
!               ********************************
!               **  STEP 4--                  **
!               **  STRIP OUT NCHDEC DIGITS   **
!               **  OF THE DECIMAL PART       **
!               **  (IN RIGHT TO LEFT ORDER)  **
!               ********************************
!
      ITEN=10
!
      AIABIX=REAL(IABSIX)
      ABSX=ABS(XRND)
      DEC=AIABIX-ABSX
      IF(DEC.LT.0.0)DEC=0.0
      DEC=ABSX-AIABIX
      DEC2=DEC*(10.0**NCHDEC)
!
      IY=INT(DEC2)
      IABSIY=IABS(IY)
!
      IRESUL=IABSIY
      NUMDED=0
      IF(NCHDEC.LE.0)GO TO 1490
      DO 1400 I=1,NCHDEC
        IDIVID=IRESUL
        IRESUL=IDIVID/ITEN
        IREM=IDIVID-IRESUL*ITEN
        NUMDED=NUMDED+1
        IDECDI(NUMDED)=IREM
 1400 CONTINUE
 1490 CONTINUE
!
!               *****************************************************
!               **   STEP 5--                                      **
!               **   CHECK TO SEE THAT TOTAL NUMBER OF CHARACTERS  **
!               **   (= NUMBER OF DIGITS + 1 IF IX >= 0, AND       **
!               **   = NUMBER OF DIGITS + 2 IF IX < 0)             **
!               **   IS LESS THAN OR EQUAL TO NCHTOT.              **
!               **   IF YES, THEN GO TO NEXT STEP.                 **
!               **   IF NO, THEN FILL THE NCHTOT ELEMENTS          **
!               **   WITH ASTERISKS                                **
!               *****************************************************
!
      NUMTOT=NUMIND+1+NUMDED
      IF(X.LT.0.0)NUMTOT=NUMTOT+1
!
      IF(NUMTOT.LE.NCHTOT)GO TO 1590
      DO 1500 I=1,NCHTOT
      J=I+NCSOLD
      ICSTR(J:J)='*'
 1500 CONTINUE
      GO TO 9000
 1590 CONTINUE
!
!               ****************************************
!               **  STEP 6--                          **
!               **  IF HAVE A VALID NUMBER OF DIGITS, **
!               **  TRANSLATE EACH DIGIT TO ITS       **
!               **  CHARACTER EQUIVALENT.             **
!               **  IN DOING SO, PLACE THE ASCII      **
!               **  CHARACTER EQUIVALENT              **
!               **  IN THE USUAL RIGHT-TO-LEFT ORDER  **
!               **  IN ICSTR(.).  IF IX < 0, INSERT   **
!               **  A MINUS BEFORE THE NUMBER.        **
!               **  HAVE THE CHARACTER STRING         **
!               **  RIGHT JUSTIFIED IN THE            **
!               **  NCHTOT ELEMENTS.                  **
!               ****************************************
!
      J=NCSOLD+(NCHTOT-NUMTOT)
      IF(X.LT.0.0)J=J+1
      IF(X.LT.0.0)ICSTR(J:J)='-'
!
      DO 1610 I=1,NUMIND
        IREV=NUMIND-I+1
        IDIG=IINTDI(IREV)
        J=J+1
        IASCDI=IDIG+IASC0
!CCCC   ICSTR(J:J)=CHAR(IASCDI)
        CALL DPCONA(IASCDI,ICSTR(J:J))
 1610 CONTINUE
!
      J=J+1
      ICSTR(J:J)='.'
!
      DO 1620 I=1,NUMDED
        IREV=NUMDED-I+1
        IDIG=IDECDI(IREV)
        J=J+1
        IASCDI=IDIG+IASC0
!CCCC   ICSTR(J:J)=CHAR(IASCDI)
        CALL DPCONA(IASCDI,ICSTR(J:J))
 1620 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
          DO 9035 I=1,NUMIND
            WRITE(ICOUT,9036)I,IINTDI(I)
 9036       FORMAT('I,IINTDI(I) = ',2I8)
            CALL DPWRST('XXX','BUG ')
 9035     CONTINUE
        ENDIF
        WRITE(ICOUT,9041)NUMDED,NUMTOT,NCSTR,NCSOLD
 9041   FORMAT('NUMDED,NUMTOT,NCSTR,NCSOLD = ',4I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMDED.GT.0)THEN
          DO 9045 I=1,NUMDED
            WRITE(ICOUT,9046)I,IDECDI(I)
 9046       FORMAT('I,IDECDI(I) = ',2I8)
            CALL DPWRST('XXX','BUG ')
 9045     CONTINUE
        ENDIF
        IF(NCSTR.GT.0)THEN
          DO 9055 I=1,NCSTR
            WRITE(ICOUT,9056)I,ICSTR(I:I)
 9056       FORMAT('I,ICSTR(I:I) = ',I8,2X,A1)
            CALL DPWRST('XXX','BUG ')
 9055     CONTINUE
        ENDIF
        WRITE(ICOUT,9059)IBUGG4,ISUBG4,IERRG4
 9059   FORMAT('IBUGG4,ISUBG4,IERRG4 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE GRTRRE
      SUBROUTINE GRTRR1(X,NCHTOT,NCHDEC,ICSTR,NCSTR,IJUST)
!
!     PURPOSE--TRANSLATE THE REAL (= FLOATING POINT) NUMBER X
!              INTO ITS CHARACTER EQUIVALENT.
!              NCHTOT = DESIRED NUMBER OF CHARACTERS FOR TOTAL NUMBER.
!              NCHDEC = DESIRED NUMBER OF CHARACTERS FOR DECIMAL PART OF NUMBER.
!              THE OUTPUT WILL CORRESPOND TO A FORTRAN F FORMAT.
!              AS IN    F NCHTOT.NCHDEC     FORMAT (E.G., F10.5).
!
!              THIS IS A MODIFIED VERSION OF GRTRRE.  THE GRTRRE ROUTINE
!              WILL RIGHT JUSTIFY THE NUMBER.  THIS ROUTINE WILL ALLOW
!              THE CALLING ROUTINE TO SPECIFY EITHER LEFT, CENTER, OR
!              RIGHT JUSTIFICATION.
!     NOTE--THE RESULTING TRANSLATED VALUES WILL BE PLACED IN SPECIFIC
!           ELEMENTS OF THE CHARACTER VARIABLE ICSTR.  THE VALUE OF THE
!           VARIABLE    NCSTR   REPRESENTS THE NUMBER OF CHARACTERS IN
!           ICSTR THAT HAVE ALREADY BEEN FILLED.  THE RESULTING
!           TRANSLATED VALUES WILL GO INTO THE NEXT    NCHTOT
!           CHARACTERS OF ICSTR.  AND THE VALUE OF    NCSTR    WILL BE
!           UPDATED ACCORDINGLY, THAT IS, NEW NCSTR = OLD NCSTR + NCHTOT
!     DANGER--NCSTR IS BOTH AN INPUT ARGUMENT
!             AND AN OUTPUT ARGUMENT OF THIS SUBROUTINE.
!
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2015.4
!     ORIGINAL VERSION --APRIL     2015. MADE DISTINCT FROM GRTRRE
!
!-----NON-COMMON VARIABLES (GRAPHICS)---------------------------------
!
      CHARACTER*4   IJUST
      CHARACTER*(*) ICSTR
!
      DIMENSION IINTDI(20)
      DIMENSION IDECDI(20)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IASC0=48
      IREV=(-999)
      IDIG=(-999)
      IERRG4='NO'
!
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRR1')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF GRTR1--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)NCHTOT,NCSTR,NCHDEC,X
   52   FORMAT('NCHTOT,NCSTR,NCHDEC,X = ',3I8,G15.7)
        CALL DPWRST('XXX','BUG ')
        IF(NCSTR.GT.0)THEN
          DO I=1,NCSTR
            WRITE(ICOUT,66)I,ICSTR(I:I)
   66       FORMAT('I,ICSTR(I:I) = ',I8,2X,A1)
            CALL DPWRST('XXX','BUG ')
          ENDDO
        ENDIF
        WRITE(ICOUT,69)IBUGG4,ISUBG4,IERRG4
   69   FORMAT('IBUGG4,ISUBG4,IERRG4 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ***********************************
!               **  STEP 1--                     **
!               **  SAVE THE OLD VALUE OF NCSTR  **
!               ***********************************
!
      NCSOLD=NCSTR
!
!     ROUND NUMBER FIRST
!
      XSAVE=X
      X=ABS(X)
      EPS=5./(10**(NCHDEC+1))
      XRND=X+EPS
!
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRR1')THEN
        WRITE(ICOUT,101)EPS,XRND
  101   FORMAT('EPS,XRND = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
      ENDIF
!
!               ********************************************
!               **  STEP 2--                              **
!               **  FILL THE NEXT   NCHTOT   ELEMENTS     **
!               **  IN ICSTR() WITH BLANKS.               **
!               **  UPDATE NCSTR BY NCSTR + NCHTOT.       **
!               ********************************************
!
      DO 1200 I=1,NCHTOT
        NCSTR=NCSTR+1
        ICSTR(NCSTR:NCSTR)=' '
 1200 CONTINUE
!
!               ********************************
!               **  STEP 3--                  **
!               **  STRIP OUT THE DIGITS      **
!               **  OF THE INTEGER PART       **
!               **  (IN RIGHT TO LEFT ORDER)  **
!               ********************************
!
      ITEN=10
!
      IX=INT(XRND)
      IABSIX=IABS(IX)
!
      IRESUL=IABSIX
      NUMIND=0
      DO I=1,20
        IDIVID=IRESUL
        IRESUL=IDIVID/ITEN
        IREM=IDIVID-IRESUL*ITEN
        NUMIND=NUMIND+1
        IINTDI(NUMIND)=IREM
!
        IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRR1')THEN
          WRITE(ICOUT,1391)I,IABSIX,IRESUL,IREM,NUMIND,IINTDI(NUMIND)
 1391     FORMAT('I,IABSIX,IRESUL,IREM,NUMIND,IINTDI(NUMIND) = ',7I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        IF(IRESUL.EQ.0)GO TO 1390
      ENDDO
 1390 CONTINUE
!
!               ********************************
!               **  STEP 4--                  **
!               **  STRIP OUT NCHDEC DIGITS   **
!               **  OF THE DECIMAL PART       **
!               **  (IN RIGHT TO LEFT ORDER)  **
!               ********************************
!
      ITEN=10
!
      AIABIX=REAL(IABSIX)
      ABSX=ABS(XRND)
      DEC=AIABIX-ABSX
      IF(DEC.LT.0.0)DEC=0.0
      DEC=ABSX-AIABIX
      DEC2=DEC*(10.0**NCHDEC)
!
      IY=INT(DEC2)
      IABSIY=IABS(IY)
!
      IRESUL=IABSIY
      NUMDED=0
!
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRR1')THEN
        WRITE(ICOUT,1491)DEC,DEC2,IY,IRESUL
 1491   FORMAT('DEC,DEC2,IY,IRESUL = ',2G15.7,2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(NCHDEC.GT.0)THEN
        DO I=1,NCHDEC
          IDIVID=IRESUL
          IRESUL=IDIVID/ITEN
          IREM=IDIVID-IRESUL*ITEN
          NUMDED=NUMDED+1
          IDECDI(NUMDED)=IREM
!
          IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRR1')THEN
            WRITE(ICOUT,1493)I,IDIVID,IRESUL,IREM,NUMDED,IDECDI(NUMDED)
 1493       FORMAT('I,IDIVID,IRESUL,IREM,NUMDED,IDECDI(NUMDED) = ',6I8)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
        ENDDO
      ENDIF
!
!               *****************************************************
!               **   STEP 5--                                      **
!               **   CHECK TO SEE THAT TOTAL NUMBER OF CHARACTERS  **
!               **   (= NUMBER OF DIGITS + 1 IF IX >= 0, AND       **
!               **   = NUMBER OF DIGITS + 2 IF IX < 0)             **
!               **   IS LESS THAN OR EQUAL TO NCHTOT.              **
!               **   IF YES, THEN GO TO NEXT STEP.                 **
!               **   IF NO, THEN FILL THE NCHTOT ELEMENTS          **
!               **   WITH ASTERISKS                                **
!               *****************************************************
!
      NUMTOT=NUMIND+1+NUMDED
      IF(XSAVE.LT.0.0)NUMTOT=NUMTOT+1
!
      IF(NUMTOT.GT.NCHTOT)THEN
        DO 1500 I=1,NCHTOT
          J=I+NCSOLD
          ICSTR(J:J)='*'
 1500   CONTINUE
        GO TO 9000
      ENDIF
!
!               ****************************************
!               **  STEP 6--                          **
!               **  IF HAVE A VALID NUMBER OF DIGITS, **
!               **  TRANSLATE EACH DIGIT TO ITS       **
!               **  CHARACTER EQUIVALENT.             **
!               **  IN DOING SO, PLACE THE ASCII      **
!               **  CHARACTER EQUIVALENT              **
!               **  IN THE USUAL RIGHT-TO-LEFT ORDER  **
!               **  IN ICSTR(.).  IF IX < 0, INSERT   **
!               **  A MINUS BEFORE THE NUMBER.        **
!               **  HAVE THE CHARACTER STRING         **
!               **  RIGHT JUSTIFIED IN THE            **
!               **  NCHTOT ELEMENTS.                  **
!               ****************************************
!
!
!     RIGHT JUSTIFIED
!
      IF(IJUST.EQ.'l')THEN
        J=NCSOLD+1
      ELSEIF(IJUST.EQ.'c')THEN
        J=NCSOLD + ((NCHTOT-NUMTOT)/2)
      ELSE
        J=NCSOLD+(NCHTOT-NUMTOT)
      ENDIF
!
      IF(XSAVE.LT.0.0)J=J+1
      IF(XSAVE.LT.0.0)ICSTR(J:J)='-'
!
      DO 1610 I=1,NUMIND
        IREV=NUMIND-I+1
        IDIG=IINTDI(IREV)
        J=J+1
        IASCDI=IDIG+IASC0
        CALL DPCONA(IASCDI,ICSTR(J:J))
 1610 CONTINUE
!
      J=J+1
      ICSTR(J:J)='.'
!
      DO 1620 I=1,NUMDED
        IREV=NUMDED-I+1
        IDIG=IDECDI(IREV)
        J=J+1
        IASCDI=IDIG+IASC0
        CALL DPCONA(IASCDI,ICSTR(J:J))
 1620 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
          DO 9035 I=1,NUMIND
            WRITE(ICOUT,9036)I,IINTDI(I)
 9036       FORMAT('I,IINTDI(I) = ',2I8)
            CALL DPWRST('XXX','BUG ')
 9035     CONTINUE
        ENDIF
        WRITE(ICOUT,9041)NUMDED,NUMTOT,NCSTR,NCSOLD
 9041   FORMAT('NUMDED,NUMTOT,NCSTR,NCSOLD = ',4I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMDED.GT.0)THEN
          DO 9045 I=1,NUMDED
            WRITE(ICOUT,9046)I,IDECDI(I)
 9046       FORMAT('I,IDECDI(I) = ',2I8)
            CALL DPWRST('XXX','BUG ')
 9045     CONTINUE
        ENDIF
        IF(NCSTR.GT.0)THEN
          DO 9055 I=1,NCSTR
            WRITE(ICOUT,9056)I,ICSTR(I:I)
 9056       FORMAT('I,ICSTR(I:I) = ',I8,2X,A1)
            CALL DPWRST('XXX','BUG ')
 9055     CONTINUE
        ENDIF
        WRITE(ICOUT,9059)IBUGG4,ISUBG4,IERRG4
 9059   FORMAT('IBUGG4,ISUBG4,IERRG4 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE GRTRR1
      SUBROUTINE GRTRRP(IPATTT,IHORPA,IVERPA,IDUPPA,IDDOPA)
!  MARCH, 1988: FOR EACH PAATTERN TYPE, SET IPATT TO A STANDARD
!               NAME.
!
!     PURPOSE--CONVERT A 1 WORD REPRESENTATION FOR THE PATTERN
!              (FOR A REGION)
!              INTO 4 SINGLE-WORD REPRESENTATIONS
!              BASED ON HORIZONTAL, VERTICAL,
!              DIAGONAL UP, AND DIAGONAL DOWN
!              COMPONENTS OF THE PATTERN.
!
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--83.6
!     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MAY       1983.
!
!-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
!
      CHARACTER*4 IPATTT
      CHARACTER*4 IHORPA
      CHARACTER*4 IVERPA
      CHARACTER*4 IDUPPA
      CHARACTER*4 IDDOPA
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'TRRP')GO TO 90
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
!
      IHORPA='OFF'
      IVERPA='OFF'
      IDUPPA='OFF'
      IDDOPA='OFF'
!
      IF(IPATTT.EQ.'    ')GO TO 110
      IF(IPATTT.EQ.'BLAN')GO TO 110
      IF(IPATTT.EQ.'NONE')GO TO 110
      IF(IPATTT.EQ.'EMPT')GO TO 110
!
      IF(IPATTT.EQ.'HORI')GO TO 210
      IF(IPATTT.EQ.'H   ')GO TO 210
      IF(IPATTT.EQ.'VERT')GO TO 220
      IF(IPATTT.EQ.'V   ')GO TO 220
      IF(IPATTT.EQ.'SOLI')GO TO 220
      IF(IPATTT.EQ.'DU  ')GO TO 230
      IF(IPATTT.EQ.'D1  ')GO TO 230
      IF(IPATTT.EQ.'DD  ')GO TO 240
      IF(IPATTT.EQ.'D2  ')GO TO 240
!
      IF(IPATTT.EQ.'HV  ')GO TO 310
      IF(IPATTT.EQ.'VH  ')GO TO 310
      IF(IPATTT.EQ.'DUDD')GO TO 320
      IF(IPATTT.EQ.'D1D2')GO TO 320
      IF(IPATTT.EQ.'DDDU')GO TO 320
      IF(IPATTT.EQ.'D2D1')GO TO 320
!
      IF(IPATTT.EQ.'HDU ')GO TO 410
      IF(IPATTT.EQ.'HD1 ')GO TO 410
!CCCC OCTOBER 1992.  ADD FOLLOWING 2 LINES
      IF(IPATTT.EQ.'HOD1')GO TO 410
      IF(IPATTT.EQ.'D1HO')GO TO 410
      IF(IPATTT.EQ.'HDD ')GO TO 420
      IF(IPATTT.EQ.'HD2 ')GO TO 420
!CCCC OCTOBER 1992.  ADD FOLLOWING 4 LINES
      IF(IPATTT.EQ.'HOD2')GO TO 420
      IF(IPATTT.EQ.'D2HO')GO TO 420
      IF(IPATTT.EQ.'DDHO')GO TO 420
      IF(IPATTT.EQ.'HODD')GO TO 420
!CCCC OCTOBER 1992.  ADD FOLLOWING 6 LINES
      IF(IPATTT.EQ.'HO12')GO TO 425
      IF(IPATTT.EQ.'12HO')GO TO 425
      IF(IPATTT.EQ.'H12 ')GO TO 425
      IF(IPATTT.EQ.'12H ')GO TO 425
      IF(IPATTT.EQ.'HD12')GO TO 425
      IF(IPATTT.EQ.'D12H')GO TO 425
!
      IF(IPATTT.EQ.'VDU ')GO TO 430
      IF(IPATTT.EQ.'VD1 ')GO TO 430
!CCCC OCTOBER 1992.  ADD FOLLOWING 2 LINES
      IF(IPATTT.EQ.'VED1')GO TO 430
      IF(IPATTT.EQ.'D1VE')GO TO 430
      IF(IPATTT.EQ.'VDD ')GO TO 440
!CCCC OCTOBER 1992.  ADD FOLLOWING 2 LINES
      IF(IPATTT.EQ.'VED2')GO TO 440
      IF(IPATTT.EQ.'D2VE')GO TO 440
!CCCC OCTOBER 1992.  ADD FOLLOWING 4 LINES
      IF(IPATTT.EQ.'VE12')GO TO 445
      IF(IPATTT.EQ.'12VE')GO TO 445
      IF(IPATTT.EQ.'V12 ')GO TO 445
      IF(IPATTT.EQ.'12V ')GO TO 445
      IF(IPATTT.EQ.'VD12')GO TO 445
      IF(IPATTT.EQ.'D12V')GO TO 445
!
      IF(IPATTT.EQ.'HVDU')GO TO 510
      IF(IPATTT.EQ.'HVD1')GO TO 510
      IF(IPATTT.EQ.'VHDU')GO TO 510
      IF(IPATTT.EQ.'VHD1')GO TO 510
      IF(IPATTT.EQ.'HVDD')GO TO 520
      IF(IPATTT.EQ.'HVD2')GO TO 520
      IF(IPATTT.EQ.'VHDD')GO TO 520
      IF(IPATTT.EQ.'VHD2')GO TO 520
!CCCC OCTOBER 1992.  ADD FOLLOWING LINES
      IF(IPATTT.EQ.'HV12')GO TO 610
      IF(IPATTT.EQ.'12HV')GO TO 610
!
      IF(IPATTT.EQ.'ALL')GO TO 610
!
      GO TO 9000
!
  110 CONTINUE
      IHORPA='OFF'
      IVERPA='OFF'
      IDUPPA='OFF'
      IDDOPA='OFF'
      IPATTT='EMPT'
      GO TO 9000
!
  210 CONTINUE
      IHORPA='ON'
      IVERPA='OFF'
      IDUPPA='OFF'
      IDDOPA='OFF'
      IPATTT='HORI'
      GO TO 9000
!
  220 CONTINUE
      IHORPA='OFF'
      IVERPA='ON'
      IDUPPA='OFF'
      IDDOPA='OFF'
      IF(IPATTT.EQ.'V')IPATTT='VERT'
      GO TO 9000
!
  230 CONTINUE
      IHORPA='OFF'
      IVERPA='OFF'
      IDUPPA='ON'
      IDDOPA='OFF'
      IPATTT='D1'
      GO TO 9000
!
  240 CONTINUE
      IHORPA='OFF'
      IVERPA='OFF'
      IDUPPA='OFF'
      IDDOPA='ON'
      IPATTT='D2'
      GO TO 9000
!
  310 CONTINUE
      IHORPA='ON'
      IVERPA='ON'
      IDUPPA='OFF'
      IDDOPA='OFF'
      IPATTT='HV'
      GO TO 9000
!
  320 CONTINUE
      IHORPA='OFF'
      IVERPA='OFF'
      IDUPPA='ON'
      IDDOPA='ON'
      IPATTT='D1D2'
      GO TO 9000
!
  410 CONTINUE
      IHORPA='ON'
      IVERPA='OFF'
      IDUPPA='ON'
      IDDOPA='OFF'
      IPATTT='HD1'
      GO TO 9000
!
  420 CONTINUE
      IHORPA='ON'
      IVERPA='OFF'
      IDUPPA='OFF'
      IDDOPA='ON'
      IPATTT='HD2'
      GO TO 9000
!
!CCCC OCTOBER 1992.  ADD FOLLOWING BLOCK OF CODE
  425 CONTINUE
      IHORPA='ON'
      IVERPA='OFF'
      IDUPPA='ON'
      IDDOPA='ON'
      IPATTT='HD12'
      GO TO 9000
!
  430 CONTINUE
      IHORPA='OFF'
      IVERPA='ON'
      IDUPPA='ON'
      IDDOPA='OFF'
      IPATTT='VD1'
      GO TO 9000
!
  440 CONTINUE
      IHORPA='OFF'
      IVERPA='ON'
      IDUPPA='OFF'
      IDDOPA='ON'
      IPATTT='VD2'
      GO TO 9000
!
!CCCC OCTOBER 1992.  ADD FOLLOWING BLOCK OF CODE
  445 CONTINUE
      IHORPA='OFF'
      IVERPA='ON'
      IDUPPA='ON'
      IDDOPA='ON'
      IPATTT='VD12'
      GO TO 9000
!
  510 CONTINUE
      IHORPA='ON'
      IVERPA='ON'
      IDUPPA='ON'
      IDDOPA='OFF'
      IPATTT='HVD1'
      GO TO 9000
!
  520 CONTINUE
      IHORPA='ON'
      IVERPA='ON'
      IDUPPA='OFF'
      IDDOPA='ON'
      IPATTT='HVD2'
      GO TO 9000
!
  610 CONTINUE
      IHORPA='ON'
      IVERPA='ON'
      IDUPPA='ON'
      IDDOPA='ON'
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'TRRP')GO TO 9090
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
!
      RETURN
      END SUBROUTINE GRTRRP
      SUBROUTINE GRTRSA(PX1,PY1,AX1,AY1,ISUBN0)
!
!     THIS ROUTINE IS A MODIFIED VERSION OF GRTRSD.  IT IS USED
!     ONLY BY THE META FILE DEVICES (I.E., "GENERAL", "GENERAL CODED"
!     AND FOR FUTURE PURPOSES "CGM").  GRTRSA CONVERTS FROM DATAPLOT
!     UNITS TO DEVICE INTEGER UNITS, BUT IT ALSO APPLIES "WINDOW"
!     TRANSFORMATIONS NEEDED BY THE "MULTI-PLOT" AND "WINDOW
!     COORDINATE" COMMANDS.  THE METAFILE DOES NOT SUPPORT ANY
!     PARTICULAR NUMBER OF PICTURE POINTS.  IT DOES HOWEVER NEED
!     TO APPLY THE "WINDOW" TRANSFORMATIONS.
!
!     PURPOSE--TRANSLATE THE STANDARDIZED (0.0 TO 100.0) COORDINATES (PX1,PY1)
!              INTO (INTEGER PICTURE POINT) DEVICE COORDINATES (AX1,AY1)
!     ISUBN0 = NAME OF SUBROUTINE WHICH CALLED GRWRST.
!              (AND THEREBY HAVE WALKBACK INFORMATION).
!     NOTE--THE ONLY VARIABLES IN THE    PLOT CONTROL COMMON
!           THAT ARE USED HEREIN ARE THE ONES IN /RWIND/
!
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--89.2
!     ORIGINAL VERSION--JANUARY   1989.
!     UPDATED         --APRIL     1992.  FIX SOME DEBUG STATEMENTS
!
!-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
!
      CHARACTER*4 ISUBN0
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOPC.INC'
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERRG4='NO'
!
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'TRSA')GO TO 90
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
!
!               *************************************
!               **  STEP 1--                       **
!               **  CARRY OUT THE TRANSFORMATION.  **
!               *************************************
!
      AX1=PWXMIN+(PX1/100.0)*(PWXMAX-PWXMIN)
      IF(AX1.LE.0.0)AX1=0.0
      IF(AX1.GE.100.)AX1=100.
!
      AY1=PWYMIN+(PY1/100.0)*(PWYMAX-PWYMIN)
      IF(AY1.LE.0.0)AY1=0.0
      IF(AY1.GE.100.)AY1=100.
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE GRTRSA
                                                                                                                                  
      SUBROUTINE GRTRSD(PX1,PY1,IX1,IY1,ISUBN0)
!
!     PURPOSE--TRANSLATE THE STANDARDIZED (0.0 TO 100.0) COORDINATES (PX1,PY1)
!              INTO (INTEGER PICTURE POINT) DEVICE COORDINATES (IX1,IY1)
!     ISUBN0 = NAME OF SUBROUTINE WHICH CALLED GRWRST.
!              (AND THEREBY HAVE WALKBACK INFORMATION).
!     NOTE--THE ONLY VARIABLES IN THE    PLOT CONTROL COMMON
!           THAT ARE USED HEREIN ARE THE ONES IN /RWIND/
!
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     UPDATED   --MARCH     1990. PATCH FOR X11, USE OFFSET VARIABLES
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--83.6
!     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MAY       1983.
!     UPDATED--SEPTEMBER 1986.
!     UPDATED--MAY 1990.    FOR X11, USE OFFSET VARIABLES
!     UPDATED--JUNE   1990. FIXED BUG WHEN USING OFFSETS
!     UPDATED--OCTOBER 1996.  SUPPORT MICROSOFT QWIN DRIVER
!     UPDATED--OCTOBER 1996.  SUPPORT MICROSOFT QWIN DRIVER
!     UPDATED--MARCH   2002.  SUPPORT SVG DRIVER
!     UPDATED--OCTOBER 2017.  SUPPORT CAIRO DRIVER
!
!-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
!
      CHARACTER*4 ISUBN0
!
!-----COMMON----------------------------------------------------------
!
!CCCC THE FOLLOWING LINE WAS INSERTED MARCH 1989
      INCLUDE 'DPCOPA.INC'
!
      INCLUDE 'DPCOPC.INC'
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCODV.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERRG4='NO'
!
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
!
!               *************************************
!               **  STEP 1--                       **
!               **  CARRY OUT THE TRANSFORMATION.  **
!               *************************************
!
!
!     FOR CAIRO DEVICE WITH EITHER POSTSCRIPT OR PDF MODEL, RETURN
!     REAL VALUES FOR THE COORDINATES.  POSTSCRIPT AND PDF WILL USE
!     THE UNROUNDED VALUES WHILE THE OTHER DEVICES WILL USE THE
!     INTEGER COORDINATES.  COMPUTE BOTH AND LET THE CALLING
!     ROUTINE DECIDE WHICH TO USE.
!
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
!
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
        GO TO 9000
      ENDIF
!
!CCCC X1=(PX1/100.0)*ANUMHP
      PWX1=PWXMIN+(PX1/100.0)*(PWXMAX-PWXMIN)
      X1=(PWX1/100.0)*ANUMHP
      IX1=INT(X1+0.5)
!CCCC FOLLOWING LINE ADDED MARCH, 1990
      IX1=IX1+IOFFSH
      IF(IX1.LE.0)IX1=0
!CCCC JUNE, 1990.  FOLLOWING LINE MODIFIED.  NEED TO ACCOUNT FOR OFFSET
!CCCC WHEN TEST FOR MAXIMUM POINT.
!CCCC IF(IX1.GE.NUMHPP)IX1=NUMHPP-1
      ITEMP=NUMHPP+IOFFSH
      IF(IX1.GE.ITEMP)IX1=ITEMP-1
!
!CCCC Y1=(PY1/100.0)*ANUMVP
!CCCC NEED TO MODIFY FOLLOWING LINE FOR QWIN DRIVER SINCE IT MEASURES
!CCCC FROM TOP TO BOTTOM.  OCTOBER 1996.
      IF(IMANUF.EQ.'QWIN')THEN
        PWYMNT=100.0-PWYMIN
        PWYMXT=100.0-PWYMAX
        PWY1=PWYMNT+(PY1/100.0)*(PWYMXT-PWYMNT)
      ELSE
        PWY1=PWYMIN+(PY1/100.0)*(PWYMAX-PWYMIN)
      ENDIF
      Y1=(PWY1/100.0)*ANUMVP
      IY1=INT(Y1+0.5)
!CCCC FOLLOWING LINE ADDED MARCH, 1990
      IY1=IY1+IOFFSV
!CCCC IF(IMANUF.EQ.'REGI')IY1=NUMVPP-1-IY1
!  ABOVE LINE MODIFIED FOR X11 DRIVER MARCH, 1990.
!  ABOVE LINE MODIFIED FOR QWIN DRIVER OCTOBER 1996
!CCCC SAME MODIFICATION FOR PNG, JPG (GD) DEVICE  FEBRUARY 2001
!CCCC SAME MODIFICATION FOR SVG DEVICE  MARCH 2002
!CCCC IF(IMANUF.EQ.'REGI'.OR.IMANUF.EQ.'X11 '.OR.IMANUF.EQ.'QWIN')
!CCCC IF(IMANUF.EQ.'REGI'.OR.IMANUF.EQ.'X11 ')
      IF(IMANUF.EQ.'REGI'.OR.IMANUF.EQ.'X11 '.OR.IMANUF.EQ.'GD'.OR.   &
         IMANUF.EQ.'SVG'.OR.IMANUF.EQ.'CAIR')   &
      IY1=NUMVPP-1-IY1
      IF(IY1.LE.0)IY1=0
!CCCC JUNE, 1990 FOR OFFSET.
!CCCC IF(IY1.GE.NUMVPP)IY1=NUMVPP-1
      ITEMP=NUMVPP+IOFFSV
      IF(IY1.GE.ITEMP)IY1=ITEMP-1
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE GRTRSD
      SUBROUTINE GRTRST(ICSTR,NCSTR,   &
      ICTRA1,NCTRA1,ICTRA2,NCTRA2,NUMTRA,   &
      IBUGG4,ISUBG4,IERRG4)
!
!     PURPOSE--APPLY VARIOUS USER-DEFINED
!              TRANSLATIONS
!              TO EACH OUTPUT GRAPHICS LINE (CHARACTER*130)
!              SO AS TO CREATE A NEW OUTPUT GRAPHICS LINE.
!     EXAMPLE--CHANGE ESC FF TO ESC ESC FF
!              SO AS TO CIRCUMVENT SOME NETWORKS
!              "EATING UP" ESCAPES.
!     CAUTION--THE INPUT ARGUMENTS ICSTR AND NCSTR
!              ARE CHANGED WITHIN THIS SUBROUTINE
!              AND THUS ARE ALSO OUTPUT ARGUMENTS.
!     CAUTION--ICSTR IS CHARACTER*130--NOT CHARACTER*132.
!              THE SAME IS TRUE FOR ICSTR2.
!     NOTE--EVERY OCCURRANCE (NOT JUST THE FIRST
!           OCCURRANCE) WILL BE TRANSLATED ON THE
!           INPUT LINE.
!     WRITTEN BY--JAMES J. FILLIBEN
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--86.6
!     ORIGINAL VERSION--MARCH 1986.
!
!-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
!
      CHARACTER*130 ICSTR
      CHARACTER*30 ICTRA1
      CHARACTER*30 ICTRA2
!
      CHARACTER*4 IBUGG4
      CHARACTER*4 ISUBG4
      CHARACTER*4 IERRG4
!
      CHARACTER*30 ICS1
      CHARACTER*30 ICS2
      CHARACTER*130 ICSTR2
!
      CHARACTER*4 IFOUST
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!CCCC CHARACTER*4 ISTEPN
!
      DIMENSION ICTRA1(*)
      DIMENSION NCTRA1(*)
      DIMENSION ICTRA2(*)
      DIMENSION NCTRA2(*)
!
!-----COMMON----------------------------------------------------------
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='GRTR'
      ISUBN2='ST  '
      IERRG4='NO'
      IFOUST='NO'
!
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'TRST')GO TO 90
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
      IF(NUMTRA.LE.0)GO TO 79
      DO 72 ITRA=1,NUMTRA
      WRITE(ICOUT,73)ITRA,NCTRA1(ITRA),ICTRA1(ITRA)
   73 FORMAT('ITRA,NCTRA1(ITRA),ICTRA1(ITRA) = ',I8,I8,2X,A30)
      CALL DPWRST('XXX','BUG ')
   72 CONTINUE
      DO 75 ITRA=1,NUMTRA
      WRITE(ICOUT,76)ITRA,NCTRA2(ITRA),ICTRA2(ITRA)
   76 FORMAT('ITRA,NCTRA2(ITRA),ICTRA2(ITRA) = ',I8,I8,2X,A30)
      CALL DPWRST('XXX','BUG ')
   75 CONTINUE
   79 CONTINUE
   90 CONTINUE
!
!               ***********************************
!               **  STEP 11--                    **
!               **  LOOP THROUGH EACH ELEMENT    **
!               **  IN THE TRANSLATION TABLE.    **
!               ***********************************
!
      IF(NUMTRA.LE.0)GO TO 1190
      DO 1100 ITRA=1,NUMTRA
!
!               ***********************************
!               **  STEP 12--                    **
!               **  FOR THIS GIVEN ELEMENT       **
!               **  IN THE TRANSLATION TABLE,    **
!               **  EXTRACT THE "OLD" SUBSTRING  **
!               ***********************************
!
      NCS1=NCTRA1(ITRA)
      ICS1=ICTRA1(ITRA)
!
!               ***********************************
!               **  STEP 13--                    **
!               **  FOR THIS GIVEN ELEMENT       **
!               **  IN THE TRANSLATION TABLE,    **
!               **  EXTRACT THE "NEW" SUBSTRING  **
!               ***********************************
!
      NCS2=NCTRA2(ITRA)
      ICS2=ICTRA2(ITRA)
!
!               **************************************
!               **  STEP 14--                       **
!               **  APPLY THE DESIRED CHANGE        **
!               **  TO EVERY OCCUURANCE OF THE      **
!               **  OLD STRING IN THE TARGET LINE;  **
!               **  THUS CREATE A NEW LINE.         **
!               **************************************
!
      CALL GRTRS2(ICS1,NCS1,ICS2,NCS2,   &
      ICSTR,NCSTR,ICSTR2,NCSTR2,IFOUST,   &
      IBUGG4,ISUBG4,IERRG4)
!
!               ******************************************
!               **  STEP 15--                           **
!               **  COPY THE NEWLY-CREATED LINE         **
!               **  BACK ONTO THE ORIGINAL LINE.        **
!               ******************************************
!
      IF(IFOUST.EQ.'NO')GO TO 1590
      NCSTR=NCSTR2
      ICSTR=ICSTR2
 1590 CONTINUE
!
!               ***********************************
!               **  STEP 16--                    **
!               **  FINISH THE LOOP              **
!               **  SO AS TO MOVE ONTO THE       **
!               **  NEXT ELEMENT                 **
!               **  OF THE TRANSLATION TABLE,    **
!               ***********************************
                                                                                                                                  
 1100 CONTINUE
 1190 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
          DO 9032 ITRA=1,NUMTRA
            WRITE(ICOUT,9033)ITRA,NCTRA1(ITRA),ICTRA1(ITRA)
 9033       FORMAT('ITRA,NCTRA1(ITRA),ICTRA1(ITRA) = ',2I8,2X,A30)
            CALL DPWRST('XXX','BUG ')
 9032     CONTINUE
          DO 9035 ITRA=1,NUMTRA
            WRITE(ICOUT,9036)ITRA,NCTRA2(ITRA),ICTRA2(ITRA)
 9036       FORMAT('ITRA,NCTRA2(ITRA),ICTRA2(ITRA) = ',2I8,2X,A30)
            CALL DPWRST('XXX','BUG ')
 9035     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE GRTRST
      SUBROUTINE GRTRS2(ICS1,NCS1,ICS2,NCS2,   &
      ICSTR,NUMCST,ICSTR2,NUMCS2,IFOUST,   &
      IBUGG4,ISUBG4,IERRG4)
!
!     PURPOSE--APPLY A SINGLE USER-DEFINED
!              TRANSLATION
!              TO EACH OUTPUT GRAPHICS LINE
!              SO AS TO CREATE A NEW OUTPUT GRAPHICS LINE.
!              IN PARTICULAR, SCAN ICSTR(.) (CHARACTER*130)
!              FOR EVERY OCCURRANCE
!              OF THE STRING IN ICS1(.).
!              IF FOUND, THEN FORM THE NEW LINE ICSTR2(.)
!              WHICH IS THE SAME AS ICSTR(.) EXCEPT
!              EVERY ICS1(.) HAS BEEN CHANGED TO ICS2(.).
!              IF NOT FOUND, THEN ICSTR2(.) = ICSTR(.)
!     EXAMPLE--CHANGE ESC FF TO ESC ESC FF
!              SO AS TO CIRCUMVENT SOME NETWORKS
!              "EATING UP" ESCAPES.
!     CAUTION--ICSTR IS CHARACTER*130--NOT CHARACTER*132.
!              THE SAME IS TRUE FOR ICSTR2.
!     WRITTEN BY--JAMES J. FILLIBEN
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--86.6
!     ORIGINAL VERSION--MARCH 1986.
!
!-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
!
      CHARACTER*30 ICS1
      CHARACTER*30 ICS2
      CHARACTER*130 ICSTR
      CHARACTER*130 ICSTR2
!
      CHARACTER*4 IFOUST
!
      CHARACTER*4 IBUGG4
      CHARACTER*4 ISUBG4
      CHARACTER*4 IERRG4
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!CCCC CHARACTER*4 ISTEPN
!
!-----COMMON----------------------------------------------------------
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='GRTR'
      ISUBN2='S2  '
      IERRG4='NO'
      IFOUST='NO'
!
      ICLIM1=1
      ICLIM2=130
!
      JPIM1=(-999)
!
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'TRS2')GO TO 90
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
!
!               *****************************
!               **  STEP 11--              **
!               **  COPY THE OLD LINE      **
!               **  TO THE NEW LINE.       **
!               *****************************
!
      NUMCS2=NUMCST
      ICSTR2=ICSTR
!
!               *******************************
!               **  STEP 21--                **
!               **  TREAT THE CASE WHEN      **
!               **  STRING 1 IS NULL         **
!               **  (THAT IS, NCS1 <=  0)  **
!               *******************************
!
      IF(NCS1.LE.0)GO TO 2100
      GO TO 2900
!
 2100 CONTINUE
!CCCC WRITE(ICOUT,774)J,K,NUMCST,JPIM1,NCS1,L,IMIN,IMAX
!C774 FORMAT('FROM 2100--J,K,NUMCST,JPIM1,NCS1,L,IMIN,IMAX = ',8I8)
!CCCC CALL DPWRST('XXX','BUG ')
      IFOUST='YES'
!
!     STEP 21.1--COPY STRING 2 TO THE BEGINNING OF LINE 1
!
      K=0
!
      IMIN=ICLIM1
      IMAX=ICLIM1+NCS2-1
      IF(IMIN.GT.IMAX)GO TO 2129
      L=0
      DO 2120 I=IMIN,IMAX
      K=K+1
      L=L+1
      ICSTR2(K:K)=ICS2(L:L)
!CCCC WRITE(ICOUT,777)I,K,ICSTR2(K:K),IMIN,IMAX
!CCCC CALL DPWRST('XXX','BUG ')
 2120 CONTINUE
 2129 CONTINUE
!
!     STEP 21.2--PUSH (COPY) THE OLD LINE TO THE RIGHT
!
      IMIN=ICLIM1+NCS2
      IMAX=ICLIM2
      IF(IMIN.GT.IMAX)GO TO 2139
      DO 2130 I=IMIN,IMAX
      I2=I-NCS2
      IF(I2.GT.NUMCST)GO TO 2139
      K=K+1
      ICSTR2(K:K)=ICSTR(I2:I2)
!CCCC WRITE(ICOUT,777)I,K,ICSTR2(K:K),IMIN,IMAX
!CCCC CALL DPWRST('XXX','BUG ')
 2130 CONTINUE
 2139 CONTINUE
!
      NUMCS2=K
      GO TO 9000
!
 2900 CONTINUE
!
!               **********************************
!               **  STEP 31--                   **
!               **  TREAT THE CASE WHEN         **
!               **  THE OLD STRING IS NON-NULL  **
!               **  (THAT IS, NCS1 >= 1)      **
!               **********************************
!
      J=0
      K=0
 3100 CONTINUE
!CCCC WRITE(ICOUT,776)
!C776 FORMAT('-------------------------------')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,780)J,K,NUMCST,JPIM1,NCS1
!C780 FORMAT('AFTER 3100--J,K,NUMCST,JPIM1,NCS1 = ',5I8)
!CCCC CALL DPWRST('XXX','BUG ')
      J=J+1
      IF(J.GT.NUMCST)GO TO 3900
      IF(J.GT.ICLIM2)GO TO 3900
!
      IF(J.LT.ICLIM1)GO TO 3210
      DO 3200 I=1,NCS1
      JPIM1=J+(I-1)
!CCCC IF(ICS1(I).EQ.IMASK)GO TO 3200
      IF(ICSTR(JPIM1:JPIM1).EQ.ICS1(I:I))GO TO 3200
      GO TO 3210
 3200 CONTINUE
!CCCC WRITE(ICOUT,781)J,K,NUMCST,JPIM1,NCS1
!C781 FORMAT('AFTER 3200 C--J,K,NUMCST,JPIM1,NCS1 = ',5I8)
!CCCC CALL DPWRST('XXX','BUG ')
      GO TO 3390
!
 3210 CONTINUE
!CCCC WRITE(ICOUT,779)J,K,NUMCST,JPIM1,NCS1
!C779 FORMAT('AFTER 3210--J,K,NUMCST,JPIM1,NCS1 = ',5I8)
!CCCC CALL DPWRST('XXX','BUG ')
      K=K+1
      ICSTR2(K:K)=ICSTR(J:J)
      GO TO 3100
!
 3390 CONTINUE
!CCCC WRITE(ICOUT,782)J,K,NUMCST,JPIM1,NCS1
!C782 FORMAT('AFTER 3390 C--J,K,NUMCST,JPIM1,NCS1 = ',5I8)
!CCCC CALL DPWRST('XXX','BUG ')
!
      IFOUST='YES'
      IF(NCS2.LE.0)GO TO 3350
      DO 3300 I=1,NCS2
      K=K+1
      ICSTR2(K:K)=ICS2(I:I)
 3300 CONTINUE
 3350 CONTINUE
!CCCC WRITE(ICOUT,783)J,K,NUMCST,JPIM1,NCS1
!C783 FORMAT('AFTER 3350 C--J,K,NUMCST,JPIM1,NCS1 = ',5I8)
!CCCC CALL DPWRST('XXX','BUG ')
      J=JPIM1
      GO TO 3100
!
 3400 CONTINUE
!CCCC WRITE(ICOUT,784)J,K,NUMCST,JPIM1,NCS1
!C784 FORMAT('AFTER 3400 C--J,K,NUMCST,JPIM1,NCS1 = ',5I8)
!CCCC CALL DPWRST('XXX','BUG ')
      J=J+1
      IF(J.GT.NUMCST)GO TO 3900
      K=K+1
      ICSTR2(K:K)=ICSTR(J:J)
      GO TO 3400
!
 3900 CONTINUE
      NUMCS2=K
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'TRS2')GO TO 9090
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
!
      RETURN
      END SUBROUTINE GRTRS2
      SUBROUTINE GRTRTK(ICSTR,NCSTR)
!
!     PURPOSE--TRANSLATE 1 LINE OF TEKTRONIX 4014 ASCII DIRECTIVES
!              INTO A SERIES OF DUMMY FORTRAN CALLS BY WHICH
!              OTHER NON-TEKTRONIX GRAPHICS DEVICES MAY BE DRIVEN
!              (E.G., CALCOMP, HP, CHROMATICS, ETC.)
!     INPUT --ICSTR  = THE CHARACTER*130 STRING
!                      CONTAINING THE CONTROL STRING.
!             NCSTR  = THE NUMBER OF ELEMENTS IN ICSTR(.:.)
!                      TO BE PROCESSED.
!
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--83.6
!     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--SEPTEMBER 1984.
!
!-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
!
      CHARACTER*130 ICSTR
!
      CHARACTER*1 ICTEXT
      CHARACTER*1 IC1
      CHARACTER*1 IC2
      CHARACTER*4 ILINTY
      CHARACTER*5 ICBYTE
      CHARACTER*4 IOP
!
      DIMENSION ICTEXT(130)
!
      DIMENSION X(100)
      DIMENSION Y(100)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCONP.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERRG4='NO'
!
      ISIZE=(-999)
      ICBYTE='     '
      JP4=(-999)
!
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'TRTK')GO TO 90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF GRTRTK--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)NCSTR
   54 FORMAT('NCSTR = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NCSTR.LE.0)GO TO 57
      DO 55 I=1,NCSTR
!CCCC IASCNE=ICHAR(ICSTR(I:I))
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
!
      I=0
!
 1000 CONTINUE
      I=I+1
      IF(I.GT.NCSTR)GO TO 9000
      IC1=ICSTR(I:I)
      IP1=I+1
      IC2=ICSTR(2:2)
!
      IF(IC1.EQ.IESCC.AND.IC2.EQ.IFFC)GO TO 1100
!
      IF(IC1.EQ.ISYNC)GO TO 1200
!
      IF(IC1.EQ.IESCC.AND.IC2.EQ.IBELC)GO TO 1300
!
      IF(IC1.EQ.IESCC.AND.IC2.EQ.'`')GO TO 1400
      IF(IC1.EQ.IESCC.AND.IC2.EQ.'a')GO TO 1400
      IF(IC1.EQ.IESCC.AND.IC2.EQ.'b')GO TO 1400
      IF(IC1.EQ.IESCC.AND.IC2.EQ.'c')GO TO 1400
!
      IF(IC1.EQ.IESCC.AND.IC2.EQ.';')GO TO 1500
      IF(IC1.EQ.IESCC.AND.IC2.EQ.':')GO TO 1500
      IF(IC1.EQ.IESCC.AND.IC2.EQ.'9')GO TO 1500
      IF(IC1.EQ.IESCC.AND.IC2.EQ.'8')GO TO 1500
!
      IF(IC1.EQ.IGSC)GO TO 1600
!
      IF(IC1.EQ.IUSC)GO TO 1700
!
      GO TO 9000
!
!
!               ***********************************
!               **  STEP 1--                     **
!               **  TREAT THE ERASE SCREEN CASE  **
!               ***********************************
!
 1100 CONTINUE
!CCCC CALL ERASESCREEN
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRTK')WRITE(ICOUT,1101)
 1101 FORMAT('ERASE SCREEN')
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRTK')CALL DPWRST('XXX','BUG ')
      I=I+1
      GO TO 1000
!
!               *************************************
!               **  STEP 2--                       **
!               **  TREAT THE SEND NULL LINE CASE  **
!               *************************************
!
 1200 CONTINUE
      I1=I
      I2=I
      DO 1210 J=I,NCSTR
      IF(ICSTR(J:J).EQ.ISYNC)I2=J
      IF(ICSTR(J:J).EQ.ISYNC)GO TO 1210
      GO TO 1211
 1210 CONTINUE
 1211 CONTINUE
      NUMNUL=I2-I1+1
!CCCC CALL SENDNULLLINE(CONSISTING OF NUMNUL CHARACTERS)
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRTK')WRITE(ICOUT,1212)NUMNUL
 1212 FORMAT('SEND A NULL LINE CONSISTING OF ',I8,'CHARACTERS')
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRTK')CALL DPWRST('XXX','BUG ')
      I=I2
      GO TO 1000
!
!               *****************
!               **  STEP 3--   **
!               **  RING BELL  **
!               *****************
!
 1300 CONTINUE
!CCCC CALL RINGBELL
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRTK')WRITE(ICOUT,1301)
 1301 FORMAT('RING BELL')
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRTK')CALL DPWRST('XXX','BUG ')
      I=I+1
      GO TO 1000
!
!               ************************************
!               **  STEP 4--                      **
!               **  TREAT THE SET LINE TYPE CASE  **
!               ************************************
!
!
 1400 CONTINUE
!CCCC IF(IC2.EQ.'`')CALL SETLINE(SOLID)
!CCCC IF(IC2.EQ.'a')CALL SETLINE(DOTTED)
!CCCC IF(IC2.EQ.'b')CALL SETLINE(DASHED)
!CCCC IF(IC2.EQ.'c')CALL SETLINE(DOT-DASHED)
      ILINTY='SOLI'
      IF(IC2.EQ.'`')ILINTY='SOLI'
      IF(IC2.EQ.'a')ILINTY='DOTT'
      IF(IC2.EQ.'b')ILINTY='DASH'
      IF(IC2.EQ.'c')ILINTY='DODA'
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRTK')WRITE(ICOUT,1411)ILINTY
 1411 FORMAT('SET LINE TYPE TO ',A4)
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRTK')CALL DPWRST('XXX','BUG ')
      I=I+1
      GO TO 1000
!
!               *****************************************
!               **  STEP 5--                           **
!               **  TREAT THE SET CHARACTER SIZE CASE  **
!               *****************************************
!
 1500 CONTINUE
!CCCC IF(IC2.EQ.';')CALL SETCHARSIZE(SMALLEST)
!CCCC IF(IC2.EQ.':')CALL SETCHARSIZE(NEXTTOSMALLEST)
!CCCC IF(IC2.EQ.'9')CALL SETCHARSIZE(NEXTTOLARGEEST)
!CCCC IF(IC2.EQ.'8')CALL SETCHARSIZE(LARGEST)
      ICHASZ=1
      IF(IC2.EQ.';')ISIZE=1
      IF(IC2.EQ.':')ISIZE=2
      IF(IC2.EQ.'9')ISIZE=3
      IF(IC2.EQ.'8')ISIZE=4
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRTK')WRITE(ICOUT,1511)ISIZE
 1511 FORMAT('SET CHARACTER SIZE TO ',I8)
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRTK')CALL DPWRST('XXX','BUG ')
      I=I+1
      GO TO 1000
!
!               **********************************************
!               **  STEP 6--                                **
!               **  TREAT THE GRAPHICS MODE MOVE/DRAW CASE  **
!               **********************************************
!
 1600 CONTINUE
      IOP='MOVE'
      IP1=I+1
      NPLOTP=0
      DO 1610 J=IP1,NCSTR,5
      JP4=J+4
      ICBYTE(1:5)=ICSTR(J:JP4)
!CCCC IB1=ICHAR(ICBYTE(1:1))
      CALL DPCOAN(ICBYTE(1:1),IB1)
!CCCC IB2=ICHAR(ICBYTE(2:2))
      CALL DPCOAN(ICBYTE(2:2),IB2)
!CCCC IB3=ICHAR(ICBYTE(3:3))
      CALL DPCOAN(ICBYTE(3:3),IB3)
!CCCC IB4=ICHAR(ICBYTE(4:4))
      CALL DPCOAN(ICBYTE(4:4),IB4)
!CCCC IB5=ICHAR(ICBYTE(5:5))
      CALL DPCOAN(ICBYTE(5:5),IB5)
!     A TEKTRONIX 4014 (ENHANCED GRAPHICS)
!     HAS A VISIBLE SCREEN CONSISTING OF
!     4096 HORIZONTAL PICTURE POINTS, AND
!     3124 VERTICAL PICTURE POINTS,
!     (WITH (0,0) AT THE BOTTOM LEFT), THEREFORE,
!     IXPP BELOW WILL RANGE FROM 0 TO 4095, AND
!     IYPP BELOW WILL RANGE FROM 0 TO 3123.
!     IXP BELOW WILL RANGE FROM 0 TO 1.
!     IYP BELOW WILL RANGE FROM 0 TO 1.
!
      IXPP=(IB4-32)*32+(IB5-64)
      IYPP=(IB1-32)*32+(IB3-96)
      IXPP=4*IXPP
      IYPP=4*IYPP
      XPP=IXPP
      YPP=IYPP
      XP=XPP/4095.
      YP=YPP/3123.
!
!     THE FOLLOWING SETTINGS ARE FOR A TYPICAL
!     ALPHANUMERIC TERMINAL SCREEN
!     (80 COLUMNS WIDE BY 24 ROWS DEEP
!     WITH (1,1) AT BOTTOM LEFT).
!
      XLEFT=1.00
      XRIGHT=80.00
      YBOT=1.00
      YTOP=24.00
!
      X2=(XLEFT-0.5)+((XRIGHT+0.5)-(XLEFT-0.5))*XP
      Y2=(YBOT-0.5)+((YTOP+0.5)-(YBOT-0.5))*YP
      IX2=INT(X2+0.5)
      IY2=INT(Y2+0.5)
!
!CCCC IF(IOP.EQ.'MOVE')CALL PENUP
!CCCC IF(IOP.EQ.'MOVE')CALL MOVETO(IX2,IY2)
!CCCC IF(IOP.EQ.'DRAW')CALL PENDOWN
!CCCC IF(IOP.EQ.'DRAW')CALL DRAWTO(IX2,IY2)
!
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRTK'.AND.   &
         IOP.EQ.'MOVE')THEN
        WRITE(ICOUT,1611)IX2,IY2
 1611   FORMAT('PEN UP   AND MOVE TO ',I8,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1612)IX2,IY2
 1612   FORMAT('PEN DOWN AND DRAW TO ',I8,I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IOP='DRAW'
      NPLOTP=NPLOTP+1
      X(NPLOTP)=IX2
      Y(NPLOTP)=IY2
 1610 CONTINUE
!CCCC IF(NPLOTP.GE.2)CALL PLOT(X,Y,NPLOTP)
!
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRTK')THEN
        WRITE(ICOUT,1621)NPLOTP
 1621   FORMAT('PLOT',I8,' POINTS FROM 2 VECTORS X (HOR) AND Y (VER)')
        CALL DPWRST('XXX','BUG ')
!
        DO 1625 J=1,NPLOTP
          WRITE(ICOUT,1626)J,X(J),Y(J)
 1626     FORMAT('      J,X(J),Y(J) = ',I8,2F15.7)
          CALL DPWRST('XXX','BUG ')
 1625   CONTINUE
      ENDIF
!
      I=JP4
      GO TO 1000
!
!               **********************************************************
!               **  STEP 7--                                            **
!               **  TREAT THE ALPHANUMERIC MODE WRITE TEXT STRING CASE  **
!               **********************************************************
!
 1700 CONTINUE
      IP1=I+1
      NCTEXT=0
      DO 1710 J=IP1,NCSTR
        IF(ICSTR(J:J).EQ.IGSC)GO TO 1790
        ICTEXT(NCTEXT)=ICSTR(J:J)
        NCTEXT=NCTEXT+1
 1710 CONTINUE
!
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'TRTK')THEN
        WRITE(ICOUT,1711)NCTEXT
 1711   FORMAT('WRITE THE ',I8,' TEXT STRING--',80A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!CCCC CALL WRITETEXT(ICTEXT,NCTEXT)
 1790 CONTINUE
      I=I+NCTEXT
      GO TO 1000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
          DO 9015 I=1,NCSTR
!CCCC       IASCNE=ICHAR(ICSTR(I:I))
            CALL DPCOAN(ICSTR(I:I),IASCNE)
            WRITE(ICOUT,9016)I,ICSTR(I:I),IASCNE
 9016       FORMAT('I,ICSTR(I:I),IASCNE = ',I8,2X,A1,I8)
            CALL DPWRST('XXX','BUG ')
 9015     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE GRTRTK
      SUBROUTINE GRWRTE(PX1,PY1,ICTEXT,NCTEXT,                        &
                        IPATTT,IFONT,ICASE,IJUST,IDIR,ANGLE,IFILLT,   &
                        ICOL,ICOLR,ICOLG,ICOLB,IRGBFL,                &
                        JPATTT,JFONT,JCASE,JJUST,JDIR,ANGLE2,         &
                        JFILLT,JCOL,                                  &
                        PHEIGH,PWIDTH,PVEGAP,PHOGAP,PTHICK,           &
                        JSIZE,                                        &
                        JHEIG2,JWIDT2,JVEGA2,JHOGA2,                  &
                        PHEIG2,PWIDT2,PVEGA2,PHOGA2,                  &
                        JTHICK,PTHIC2,                                &
                        PXLEC,PXLECG,PYLEC,PYLECG,                    &
                        ISYMBL,ISPAC,                                 &
                        IMPSW2,AMPSCH,AMPSCW,                         &
                        PX99,PY99)
!
!     PURPOSE--FOR A SPECIFIC GRAPHICS DEVICE,
!              GO TO THE POINT (PX1,PY1) AND WRITE
!              OUT THE TEXT STRING CONTAINED IN THE
!              CHARACTER VECTOR ICTEXT(.),
!              WHICH CONSISTS OF    NCTEXT    CHARACTERS.
!     NOTE--PX1 AND PY1 ARE IN STANDARDIZED COORDINATES
!           THAT IS, EACH IS 0.0 TO 100.0.
!
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--83.6
!     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MAY       1983.
!     UPDATED--JULY        1996. FORCE LAHEY DEVICE TO SOFTWARE CHAR.
!     UPDATED--SEPTEMBER   1999. ARGUMENT LIST TO GRWRTG
!     UPDATED--OCTOBER     2020. SUPPORT FOR RGB COLOR
!
!-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
!
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
!
      CHARACTER*4 IFONT2
!
      DIMENSION ICTEXT(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
!
!-----START POINT-----------------------------------------------------
!
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
!
!               ********************************************
!               **  STEP 1--                              **
!               **  BRANCH ACCORDING TO THE DIRECTION     **
!               **  AND THE FONT                          **
!               ********************************************
!
!CCCC FOLLOWING SECTION MODIFIED JULY 1996.
      IF(IGFONT.EQ.'OFF')THEN
!
        IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'WRTE')THEN
          WRITE(ICOUT,401)IGFONT,IFONT,IDIR
  401     FORMAT('AT 401: IGFONT,IFONT,IDIR = ',2(A4,2X),A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        IF(IFONT.NE.'TEKT')GO TO 1300
        IF(IDIR.EQ.'HORI')GO TO 1100
        IF(IDIR.EQ.'VERT')GO TO 1200
        GO TO 1300
      ELSE
!
        IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'WRTE')THEN
          WRITE(ICOUT,501)IGFONT,IDIR
  501     FORMAT('AT 501: IGFONT,IDIR = ',A4,2X,A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        IF(IGFONT.NE.'TEKT')GO TO 1300
        IF(IDIR.EQ.'HORI')GO TO 1100
        IF(IDIR.EQ.'VERT')GO TO 1200
        GO TO 1300
      ENDIF
!
!               **************************************
!               **  STEP 11--                       **
!               **  TREAT THE HORIZONTAL DIRECTION  **
!               **************************************
!
 1100 CONTINUE
      CALL GRWRTH(PX1,PY1,ICTEXT,NCTEXT,   &
                  IPATTT,IFONT,ICASE,IJUST,IDIR,ANGLE,IFILLT,   &
                  ICOL,ICOLR,ICOLG,ICOLB,IRGBFL,   &
                  JPATTT,JFONT,JCASE,JJUST,JDIR,ANGLE2,JFILLT,JCOL,   &
                  PHEIGH,PWIDTH,PVEGAP,PHOGAP,PTHICK,   &
                  JSIZE,   &
                  JHEIG2,JWIDT2,JVEGA2,JHOGA2,   &
                  PHEIG2,PWIDT2,PVEGA2,PHOGA2,   &
                  JTHICK,PTHIC2,   &
                  PXLEC,PXLECG,PYLEC,PYLECG,   &
                  ISYMBL,ISPAC,   &
                  PX99,PY99)
      GO TO 9000
!
!               ************************************
!               **  STEP 12--                     **
!               **  TREAT THE VERTICAL DIRECTION  **
!               ************************************
!
 1200 CONTINUE
      CALL GRWRTV(PX1,PY1,ICTEXT,NCTEXT,   &
                  IPATTT,IFONT,ICASE,IJUST,IDIR,ANGLE,IFILLT,   &
                  ICOL,ICOLR,ICOLG,ICOLB,IRGBFL,   &
                  JPATTT,JFONT,JCASE,JJUST,JDIR,ANGLE2,JFILLT,JCOL,   &
                  PHEIGH,PWIDTH,PVEGAP,PHOGAP,PTHICK,   &
                  JSIZE,   &
                  JHEIG2,JWIDT2,JVEGA2,JHOGA2,   &
                  PHEIG2,PWIDT2,PVEGA2,PHOGA2,   &
                  JTHICK,PTHIC2,   &
                  PXLEC,PXLECG,PYLEC,PYLECG,   &
                  ISYMBL,ISPAC,   &
                  PX99,PY99)
      GO TO 9000
!
!               ************************************
!               **  STEP 13--                     **
!               **  TREAT THE GENERAL DIRECTION   **
!               ************************************
!
 1300 CONTINUE
      IFONT2=IFONT
      IF(IFONT.EQ.'TEKT')IFONT2='SIMP'
      CALL GRWRTG(PX1,PY1,ICTEXT,NCTEXT,   &
                  IPATTT,IFONT2,ICASE,IJUST,IDIR,ANGLE,IFILLT,   &
                  ICOL,ICOLR,ICOLG,ICOLB,IRGBFL,   &
                  JPATTT,JFONT,JCASE,JJUST,JDIR,ANGLE2,JFILLT,JCOL,   &
                  PHEIGH,PWIDTH,PVEGAP,PHOGAP,PTHICK,   &
                  JSIZE,   &
                  JHEIG2,JWIDT2,JVEGA2,JHOGA2,   &
                  PHEIG2,PWIDT2,PVEGA2,PHOGA2,   &
                  JTHICK,PTHIC2,   &
                  PXLEC,PXLECG,PYLEC,PYLECG,   &
                  ISYMBL,ISPAC,   &
                  IMPSW2,AMPSCH,AMPSCW,   &
                  PX99,PY99)
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE GRWRTE
      SUBROUTINE GRWRTG(PX1,PY1,ICTEXT,NCTEXT,   &
                        IPATT,IFONT,ICASE,IJUST,IDIR,ANGLE,IFILL,   &
                        ICOL,ICOLR,ICOLG,ICOLB,IRGBFL,   &
                        JPATT,JFONT,JCASE,JJUST,JDIR,ANGLE2,JFILL,JCOL,   &
                        PHEIGH,PWIDTH,PVEGAP,PHOGAP,PTHICK,   &
                        JSIZE,   &
                        JHEIG2,JWIDT2,JVEGA2,JHOGA2,   &
                        PHEIG2,PWIDT2,PVEGA2,PHOGA2,   &
                        JTHICK,PTHIC2,   &
                        PXLEC,PXLECG,PYLEC,PYLECG,   &
                        ISYMBL,ISPAC,   &
                        IMPSW2,AMPSCH,AMPSCW,   &
                        PX99,PY99)
!
!     PURPOSE--FOR A SPECIFIC GRAPHICS DEVICE, AND FOR A GENERAL
!              (SOFTWARE-GENERATED HERSHEY) FONT, GO TO THE POINT
!              (PX1,PY1) AND WRITE OUT THE TEXT STRING (IN A GENERAL
!              DIRECTION) CONTAINED IN THE CHARACTER VECTOR ICTEXT(.),
!              WHICH CONSISTS OF NCTEXT CHARACTERS.
!     NOTE--PX1 AND PY1 ARE IN STANDARDIZED COORDINATES
!           THAT IS, EACH IS 0.0 TO 100.0.
!
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--83.6
!     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MAY       1983.
!     UPDATED         --SEPTEMBER    1997. SUPPORT MULTIPLOT SCALE FACTOR
!     UPDATED         --OCTOBER      2020. SUPPORT FOR RGB COLOR
!
!-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
!
      CHARACTER*4 ICTEXT
!
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
!
      CHARACTER*4 ISTRIN
      CHARACTER*130 ICSTR
!
      DIMENSION ICTEXT(*)
      DIMENSION ISTRIN(130)
!
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!CCCC ADD FOLLOWING LINE NOVEMBER 1994.
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
!
!               ********************************************
!               **  STEP 1--                              **
!               **  BRANCH ACCORDING TO THE MANUFACTURER  **
!               **  AND THE MODEL                         **
!               ********************************************
!
      IF(IMANUF.EQ.'GENE')GO TO 1030
      GO TO 1100
!
!
 1030 CONTINUE
      IF(IFNTSW.EQ.'OFF')GO TO 1100
      IF(IMODEL.EQ.'CODE')GO TO 3200
      GO TO 3100
!
!               *************************************************
!               **  STEP 11--                                  **
!               **  TREAT THE GENERAL MODEL                    **
!               **  (TEKTRONIX, HP, DEVICE-INDEPENDENT, ETC.)  **
!               *************************************************
!
 1100 CONTINUE
      NUMCHA=NCTEXT
      DO 1110 I=1,NUMCHA
        ISTRIN(I)=ICTEXT(I)
 1110 CONTINUE
      X0=PX1
      Y0=PY1
!
      HEIGHT=PHEIGH+PVEGAP
      WIDTH=PWIDTH+PHOGAP
!
      IBUGD2=IBUGG4
      HMAX=100.0
      VMAX=100.0
      AMAX=360.0
!
      CALL DPSCR7(ISTRIN,NUMCHA,X0,Y0,   &
                  IFONT,ICASE,IJUST,ANGLE,HMAX,VMAX,AMAX,WIDTH,HEIGHT,   &
                  PHEIGH,PWIDTH,PVEGAP,PHOGAP,   &
                  PHEIG2,PWIDT2,PVEGA2,PHOGA2,   &
                  ANUMHP,ANUMVP,   &
                  IPATT,PTHICK,ICOL,ICOLR,ICOLG,ICOLB,IRGBFL,   &
                  JPATT,JTHICK,PTHIC2,JCOL,   &
                  ISYMBL,ISPAC,   &
                  IFILL,   &
                  IMPSW2,AMPSCH,AMPSCW,   &
                  PX99,PY99,IFOUND,IBUGD2,IERROR)
!
      GO TO 9000
!
!               *****************************************************
!               **  FOR GENERAL DEVICE (AND "SET GENERAL FONT ON") **
!               **  LET POST PROCESSOR WRITE THE STRING IN  ONE OF **
!               **  ITS OWN FONTS. NOTE THAT THE DATAPLOT FONTS    **
!               **  WILL BE MAPPED TO THE POST PROCESSORS FONTS.   **
!               **  NOTE THAT IN THIS CASE, IT IS ASSUMMED THAT    **
!               **  THE POST PROCESSOR WILL ALSO SET THE "ANGLE"   **
!               **  AND THE JUSTIFICATION.                         **
!               *****************************************************
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
      IF(NCTEXT.LE.0)GO TO 3190
      ICSTR(1:11)='WRITE TEXT '
      NCSTR=11
      K=0
      DO 3112 J=1,NCTEXT
      K=J+NCSTR
      ICSTR(K:K)=ICTEXT(J)
 3112 CONTINUE
      NCSTR=K
      CALL GRWRST(ICSTR,NCSTR,ISUBN0)
 3190 CONTINUE
      GO TO 9000
!
!               ***************************************************************
!               **  STEP 32--                                                **
!               **  TREAT THE CODED GENERAL (DEVICE-INDEPENDENT) CASE        **
!               ***************************************************************
!
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
      IF(NCTEXT.LE.0)GO TO 3290
      ICSTR(1:5)='WRTE '
      NCSTR=5
      K=0
      DO 3212 J=1,NCTEXT
      K=J+NCSTR
      ICSTR(K:K)=ICTEXT(J)
 3212 CONTINUE
      NCSTR=K
      CALL GRWRST(ICSTR,NCSTR,ISUBN0)
 3290 CONTINUE
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE GRWRTG
      DOUBLE PRECISION FUNCTION gsumln(a,b)
!-----------------------------------------------------------------------
!          EVALUATION OF THE FUNCTION LN(GAMMA(A + B))
!          FOR 1 .LE. A .LE. 2  AND  1 .LE. B .LE. 2
!-----------------------------------------------------------------------
!     .. Scalar Arguments ..
      DOUBLE PRECISION a,b
!     ..
!     .. Local Scalars ..
      DOUBLE PRECISION x
!     ..
!     .. External Functions ..
      DOUBLE PRECISION alnrel,gamln1
      EXTERNAL alnrel,gamln1
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC dble,dlog
!     ..
!     .. Executable Statements ..
      x = dble(a) + dble(b) - 2.D0
      IF (x.GT.0.25D0) GO TO 10
      gsumln = gamln1(1.0D0+x)
      RETURN
                                                                                                                                  
   10 IF (x.GT.1.25D0) GO TO 20
      gsumln = gamln1(x) + alnrel(x)
      RETURN
                                                                                                                                  
   20 gsumln = gamln1(x-1.0D0) + dlog(x* (1.0D0+x))
      RETURN
                                                                                                                                  
      END FUNCTION gsumln
      SUBROUTINE GTLCDF(X,ALPHA,BETA,A,B,CDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
!              FUNCTION VALUE FOR THE
!              GENERALIZED TOPP AND LEONE DISTRIBUTION.
!              THE CUMULATIVE DISTRIBUTION FUNCTION IS:
!
!                  F(X;ALPHA,BETA,A,B) = 1 -
!                     {ALPHA*((X-A)/(B-A)) - ALPHA-1)*
!                     ((X-A)/(B-A))**2}**BETA
!                                    A <= X <= B, BETA > 0,
!                                    0 < ALPHA <= 2
!
!              WITH ALPHA AND BETA DENOTING THE SHAPE PARAMETERS.
!
!     INPUT  ARGUMENTS--X      = THE DOUBLE PRECISION VALUE AT
!                                WHICH THE CUMULATIVE DISTRIBUTION
!                                FUNCTION IS TO BE EVALUATED.
!                     --ALPHA  = THE DOUBLE PRECISION FIRST SHAPE
!                                PARAMETER
!                     --BETA   = THE DOUBLE PRECISION SECOND SHAPE
!                                PARAMETER
!     OUTPUT ARGUMENTS--CDF    = THE DOUBLE PRECISION CUMULATIVE
!                                DISTRIBUTION FUNCTION VALUE.
!     OUTPUT--THE DOUBLE PRECISION CUMULATIVE DISTRIBUTION
!             FUNCTION VALUE CDF.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE BETWEEN 0 AND 1, INCLUSIVELY.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--DLOG, DEXP.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
!                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
!                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
!                 PUBLISHING COMPANY, CHAPTER 7.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE:  301-975-2855
!     ORIGINAL VERSION--FEBRUARY  2007.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION X
      DOUBLE PRECISION ALPHA
      DOUBLE PRECISION BETA
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      DOUBLE PRECISION CDF
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DX
!
      INCLUDE 'DPCOP2.INC'
!
!---------------------------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(B.LT.A)THEN
        TERM1=B
        B=A
        A=TERM1
      ENDIF
!
      CDF=0.0D0
      IF(X.LT.A .OR. X.GT.B)THEN
        WRITE(ICOUT,2)
    2   FORMAT('***** ERROR--THE FIRST ARGUMENT TO GTLCDF IS ',   &
               'OUTSIDE THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3)A,B
    3   FORMAT('      (',G15.7,',',G15.7,') INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(ALPHA.LE.0.0D0 .OR. ALPHA.GT.2.0)THEN
        WRITE(ICOUT,12)
   12   FORMAT('***** ERROR--THE SECOND ARGUMENT TO GTLCDF IS ',   &
               'OUTSIDE THE [0,2) INTERVAL')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(BETA.LE.0.0D0)THEN
        WRITE(ICOUT,14)
   14   FORMAT('***** ERROR--THE THIRD ARGUMENT TO GTLCDF IS ',   &
               'IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(B.EQ.A)THEN
        WRITE(ICOUT,16)
   16   FORMAT('***** ERROR--THE LOWER AND UPPER LIMITS FOR GTLCDF ',   &
               'ARE EQUAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)A
   48   FORMAT('***** THE VALUE OF THE LIMIT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!-----START POINT-----------------------------------------------------
!
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
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE GTLCDF
      SUBROUTINE GTLPDF(X,ALPHA,BETA,A,B,PDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
!              FUNCTION VALUE FOR THE
!              GENERALIZED TOPP AND LEONE DISTRIBUTION.
!              THE PROBABILITY DENSITY FUNCTION IS:
!
!                  f(X;ALPHA,BETA,A,B) = BETA*{ALPHA*((X-A)/B-A) -
!                     (ALPHA - 1)*((X-A)/(B-A))**2}**{BETA-1)*
!                     (ALPHA - 2*(ALPHA - 1)*((X-A)/(B-A)))
!                                    A <= X <= B, BETA > 0,
!                                    0 < ALPHA <= 2
!
!              WITH ALPHA AND BETA DENOTING THE SHAPE PARAMETERS.
!
!     INPUT  ARGUMENTS--X      = THE DOUBLE PRECISION VALUE AT
!                                WHICH THE PROBABILITY DENSITY
!                                FUNCTION IS TO BE EVALUATED.
!                     --ALPHA  = THE DOUBLE PRECISION FIRST SHAPE
!                                PARAMETER
!                     --BETA   = THE DOUBLE PRECISION SECOND SHAPE
!                                PARAMETER
!     OUTPUT ARGUMENTS--PDF    = THE DOUBLE PRECISION CUMULATIVE
!                                DISTRIBUTION FUNCTION VALUE.
!     OUTPUT--THE DOUBLE PRECISION PROBABILITY DENSITY
!             FUNCTION VALUE PDF.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE BETWEEN 0 AND 1, INCLUSIVELY.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--DLOG, DEXP.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
!                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
!                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
!                 PUBLISHING COMPANY, CHAPTER 7.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE:  301-975-2855
!     ORIGINAL VERSION--FEBRUARY  2007.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
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
!
      INCLUDE 'DPCOP2.INC'
!
!---------------------------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      PDF=0.0D0
      IF(B.LT.A)THEN
        TERM1=B
        B=A
        A=TERM1
      ENDIF
!
      IF(X.LT.A .OR. X.GT.B)THEN
        WRITE(ICOUT,2)
    2   FORMAT('***** ERROR--THE FIRST ARGUMENT TO GTLPDF IS ',   &
               'OUTSIDE THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3)A,B
    3   FORMAT('      (',G15.7,',',G15.7,') INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(ALPHA.LE.0.0D0 .OR. ALPHA.GT.2.0)THEN
        WRITE(ICOUT,12)
   12   FORMAT('***** ERROR--THE SECOND ARGUMENT TO GTLPDF IS ',   &
               'OUTSIDE THE [0,2) INTERVAL')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(BETA.LE.0.0D0)THEN
        WRITE(ICOUT,14)
   14   FORMAT('***** ERROR--THE THIRD ARGUMENT TO GTLPDF IS ',   &
               'IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(B.EQ.A)THEN
        WRITE(ICOUT,16)
   16   FORMAT('***** ERROR--THE LOWER AND UPPER LIMITS FOR GTLPDF ',   &
               'ARE EQUAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)A
   48   FORMAT('***** THE VALUE OF THE LIMIT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!-----START POINT-----------------------------------------------------
!
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
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE GTLPDF
      SUBROUTINE GTLPPF(P,ALPHA,BETA,PPF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
!              FUNCTION VALUE FOR THE GENERALIZED TOPP AND LEONE
!              DISTRIBUTION.
!
!              THE PERCENT POINT FUNCTION IS:
!
!              G(P;ALPHA,BETA,A,B) = P**(1/BETA)     FOR ALPHA = 1
!                  {-ALPHA + SQRT(ALPHA**2 0 4*(1-ALPHA)*
!                  (1-P**(1/BETA))}/{2*(1-ALPHA)}
!                  FOR 0 < ALPHA < 1 AND 1 < ALPHA <= 2
!
!              WITH ALPHA AND BETA DENOTING THE SHAPE PARAMETERS.
!
!     INPUT  ARGUMENTS--X      = THE DOUBLE PRECISION VALUE AT
!                                WHICH THE PERCENT POINT
!                                FUNCTION IS TO BE EVALUATED.
!                     --ALPHA  = THE DOUBLE PRECISION FIRST SHAPE
!                                PARAMETER
!                     --BETA   = THE DOUBLE PRECISION SECOND SHAPE
!                                PARAMETER
!     OUTPUT ARGUMENTS--PPF    = THE DOUBLE PRECISION PERCENT POINT
!                                FUNCTION VALUE.
!     OUTPUT--THE DOUBLE PRECISION PERCENT POINT FUNCTION VALUE PDF.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE BETWEEN 0 AND 1, INCLUSIVELY.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--GTLCDF
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
!                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
!                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
!                 PUBLISHING COMPANY, CHAPTER 7.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2007/2
!     ORIGINAL VERSION--FEBRUARY  2007.
!     UPDATED         --SEPTEMBER 2007. REPLACE BISECTION FORMULA
!                                       WITH EXPLICIT FUNCTION
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES----------------
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
!-----COMMON--------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!CCCC DATA EPS /1.0D-8/
!CCCC DATA SIG /1.0D-8/
!CCCC DATA ZERO /0.0D0/
!CCCC DATA MAXIT /500/
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      PPF=0.0D0
      IF(P.LT.0.0D0 .OR. P.GT.1.0D0)THEN
        WRITE(ICOUT,2)
    2   FORMAT('***** ERROR--THE FIRST ARGUMENT TO GTLPPF IS ',   &
               'OUTSIDE THE (0,1) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(ALPHA.LE.0.0D0 .OR. ALPHA.GT.2.0)THEN
        WRITE(ICOUT,12)
   12   FORMAT('***** ERROR--THE SECOND ARGUMENT TO GTLPPF IS ',   &
               'OUTSIDE THE [0,2) INTERVAL')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(BETA.LE.0.0D0)THEN
        WRITE(ICOUT,14)
   14   FORMAT('***** ERROR--THE THIRD ARGUMENT TO GTLPPF IS ',   &
               'IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
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
          GO TO 9000
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
            GO TO 9000
          ENDIF
        ENDIF
      ENDIF
!
!CCCC A = 0.0D0
!CCCC B = 1.0D0
!
!CCCC IERR=0
!CCCC IC = 0
!CCCC XL = 0.0D0
!CCCC XR = 1.0D0
!CCCC FXL = -P
!CCCC FXR = 1.0D0 - P
!
!  BISECTION METHOD
!
!C105 CONTINUE
!CCCC X = (XL+XR)*0.5D0
!CCCC CALL GTLCDF(X,ALPHA,BETA,A,B,DCDF)
!CCCC P1=DCDF
!CCCC PPF=X
!CCCC FCS = P1 - P
!
!CCCC IF(FCS*FXL.GT.ZERO)THEN
!CCCC   XL = X
!CCCC   FXL = FCS
!CCCC ELSE
!CCCC   XR = X
!CCCC   FXR = FCS
!CCCC ENDIF
!
!CCCC XRML = XR - XL
!CCCC IF(XRML.LE.SIG .AND. DABS(FCS).LE.EPS)GO TO 9000
!CCCC IC = IC + 1
!CCCC IF(IC.LE.MAXIT)GO TO 105
!CCCC WRITE(ICOUT,130)
!C130 FORMAT('***** ERROR--GTLPPF ROUTINE DID NOT CONVERGE.')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC GO TO 9000
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE GTLPPF
      SUBROUTINE GTLRAN(N,ALPHA,BETA,ISEED,X)
!
!     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
!              FROM THE GENERALIZED TOPP AND LEONE
!              DISTRIBUTION WITH SHAPE PARAMETERS ALPHA AND BETA.
!
!              THE PROBABILITY DENSITY FUNCTION IS:
!
!                  f(X;ALPHA,BETA,A,B) = BETA*{ALPHA*((X-A)/B-A) -
!                     (ALPHA - 1)*((X-A)/(B-A))**2}**{BETA-1)*
!                     (ALPHA - 2*(ALPHA - 1)*((X-A)/(B-A)))
!                                    A <= X <= B, BETA > 0,
!                                    0 < ALPHA <= 2
!
!              WITH ALPHA AND BETA DENOTING THE SHAPE PARAMETERS.
!
!     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
!                                OF RANDOM NUMBERS TO BE
!                                GENERATED.
!                     --ALPHA  = THE DOUBLE PRECISION FIRST SHAPE
!                                PARAMETER
!                     --BETA   = THE DOUBLE PRECISION VALUE OF THE
!                                SHAPE PARAMETER BETA.
!                                BETA SHOULD BE IN THE RANGE (0,1).
!     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
!                                (OF DIMENSION AT LEAST N)
!                                INTO WHICH THE GENERATED
!                                RANDOM SAMPLE WILL BE PLACED.
!     OUTPUT--A RANDOM SAMPLE OF SIZE N
!             FROM THE GENERALIZED TOPP AND LEONE
!             DISTRIBUTION WITH SHAPE PARAMETERS ALPHA AND BETA.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN, RGTPPF.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--KOTZ AND VAN DORP (2004), "BEYOND BETA: OTHER
!                 CONTINUOUS FAMILIES OF DISTRIBUTIONS WITH BOUNDED
!                 SUPPORT AND APPLICATIONS", WORLD SCIENTFIC
!                 PUBLISHING COMPANY, CHAPTER 7.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHMOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2007.2
!     ORIGINAL VERSION--FEBRUARY  2007.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION ALPHA
      DOUBLE PRECISION BETA
      DOUBLE PRECISION DTEMP
      DIMENSION X(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(N.LT.1)THEN
        WRITE(ICOUT,5)
    5   FORMAT('***** ERROR--THE REQUESTED NUMBER OF ',   &
               'GENERALIZED TOPP AND LEONE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6)
    6   FORMAT('      RANDOM NUMBERS IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
   47   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(BETA.LE.0.0D0)THEN
        WRITE(ICOUT,201)
  201   FORMAT('***** ERROR--THE BETA SHAPE PARAMETER IS ',   &
               'NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,203)BETA
  203   FORMAT('      THE VALUE OF BETA IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(ALPHA.LE.0.0D0 .OR. ALPHA.GT.2.0D0)THEN
        WRITE(ICOUT,301)
  301   FORMAT('***** ERROR--THE ALPHA SHAPE PARAMETER IS ',   &
               'OUTSIDE THE [0,2) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,303)ALPHA
  303   FORMAT('      THE VALUE OF ALPHA IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
!     GENERATE N UNIFORM (0,1) RANDOM NUMBERS;
!
      CALL UNIRAN(N,ISEED,X)
!
!     GENERATE N GENERALIZED TOPP AND LEONE DISTRIBUTION
!     RANDOM NUMBERS USING THE PERCENT POINT FUNCTION TRANSFORMATION
!     METHOD.
!
      DO 300 I=1,N
        ZTEMP=X(I)
        CALL GTLPPF(DBLE(ZTEMP),ALPHA,BETA,DTEMP)
        X(I)=REAL(DTEMP)
  300 CONTINUE
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE GTLRAN
      SUBROUTINE GTRCDF(X,A,B,C,D,ANU1,ANU3,ALPHA,CDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
!              FUNCTION VALUE FOR THE GENERALIZED TRAPEZOID
!              DISTRIBUTION.
!              THIS DISTRIBUTION HAS THE FOLLOWING CDF FUNCTION:
!              F(X,A,B,C,D,N1,N2,ALPHA)
!              = 0                                    X < A
!              = [2*ALPHA*(B-A)*NU3/
!              (2*ALPHA*(B-A)*NU3+(ALPHA+1)*(C-B)*NU1*NU3+2*(D-C)*NU1)]
!              *((X-A)/(B-A))**NU1
!                                                     A <= X <  B
!              = [2*ALPHA*(B-A)*NU3 + 2*(X-B)*NU1*NU3*
!              {1 + (ALPHA-1)*(2*C-B-X)/(2*(C-B)}]/
!              [(2*ALPHA*(B-A)*NU3+(ALPHA+1)*(C-B)*NU1*NU3+2*(D-C)*NU1)]
!              *{(ALPHA-1)*(C-X)/(C-B)+1}
!                                                     B <= X <  C
!              = 1 -
!              [2*(D-C)*NU1]/
!              (2*ALPHA*(B-A)*NU3+(ALPHA+1)*(C-B)*NU1*NU3+2*(D-C)*NU1)]
!              *((D-X)/(D-C))**NU3
!                                                     C <= X <  D
!              = 1                                    X >= D
!              WHERE
!                  A <= B <= C <= D, NU1, NU3, ALPHA > 0
!              THIS DISTRIBUTION MODELS A "GROWTH PHASE",
!              A "STABLE PHASE", AND A "DECAY PHASE".
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE CUMULATIVE DISTRIBUTION
!                                FUNCTION IS TO BE EVALUATED.
!                     --A      = THE SINGLE PRECISION SHAPE PARAMETER
!                       B      = THE SINGLE PRECISION SHAPE PARAMETER
!                       C      = THE SINGLE PRECISION SHAPE PARAMETER
!                       D      = THE SINGLE PRECISION SHAPE PARAMETER
!                       ANU1   = THE SINGLE PRECISION SHAPE PARAMETER
!                       ANU3   = THE SINGLE PRECISION SHAPE PARAMETER
!                       ALPHA  = THE SINGLE PRECISION SHAPE PARAMETER
!     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE
!                                DISTRIBUTION FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION
!             FUNCTION VALUE CDF.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE BETWEEN A AND D, INCLUSIVELY.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--J. RENE VAN DORP AND SAMIEL KOTZ, "GENERALIZED
!                 TRAPEZOIDAL DISTRIBUTIONS", METRIKA, VOL. 58,
!                 ISSUE 1, JULY 2003.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING LABORATORY (205.03)
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899
!                 PHONE:  301-975-2855
!     ORIGINAL VERSION--JUNE      2003.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
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
!
      INCLUDE 'DPCOP2.INC'
!
!---------------------------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
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
        GO TO 9000
      ENDIF
      IF(ALPHA.LE.0.0)THEN
        WRITE(ICOUT,22)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,23)ALPHA
        CALL DPWRST('XXX','BUG ')
        CDF=0.0
        GO TO 9000
      ENDIF
      IF(ANU1.LE.0.0)THEN
        WRITE(ICOUT,32)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,33)ANU1
        CALL DPWRST('XXX','BUG ')
        CDF=0.0
        GO TO 9000
      ENDIF
      IF(ANU3.LE.0.0)THEN
        WRITE(ICOUT,42)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,43)ANU3
        CALL DPWRST('XXX','BUG ')
        CDF=0.0
        GO TO 9000
      ENDIF
   12 FORMAT(   &
      '***** FATAL ERROR--FOR THE GENERALIZED TRAPEZOID DISTRIBUTION,')
   13 FORMAT(   &
      '      THE FOUR SHAPE PARAMETERS (A, B, C, D) MUST SATISFY')
   14 FORMAT(   &
      '         A <= B <= C <= D')
   16 FORMAT(   &
      '      A, B, C, D = ',4E15.7)
   22 FORMAT(   &
      '***** FATAL ERROR--FOR THE GENERALIZED TRAPEZOID DISTRIBUTION,')
   23 FORMAT(   &
      '      THE ALPHA SHAPE PARAMETER MUST BE > 0.  ALPHA = ',E15.7)
   32 FORMAT(   &
      '***** FATAL ERROR--FOR THE GENERALIZED TRAPEZOID DISTRIBUTION,')
   33 FORMAT(   &
      '      THE NU1 SHAPE PARAMETER MUST BE > 0.  NU1 = ',E15.7)
   42 FORMAT(   &
      '***** FATAL ERROR--FOR THE GENERALIZED TRAPEZOID DISTRIBUTION,')
   43 FORMAT(   &
      '      THE NU3 SHAPE PARAMETER MUST BE > 0.  NU3 = ',E15.7)
!
!-----START POINT-----------------------------------------------------
!
      DCDF=0.0D0
      IF(X.LT.A)THEN
        CDF=0.0
        GO TO 9000
      ELSEIF(X.GE.D)THEN
        CDF=1.0
        GO TO 9000
      ENDIF
!
      DX=DBLE(X)
      DA=DBLE(A)
      DB=DBLE(B)
      DC=DBLE(C)
      DD=DBLE(D)
      DNU1=DBLE(ANU1)
      DNU3=DBLE(ANU3)
      DALPHA=DBLE(ALPHA)
      DTERM2=2.0D0*DALPHA*(DB-DA)*DNU3 +   &
             (DALPHA+1.0D0)*(DC-DB)*DNU1*DNU3 +   &
             2.0D0*(DD-DC)*DNU1
!
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
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE GTRCDF
      SUBROUTINE GTRPDF(X,A,B,C,D,ANU1,ANU3,ALPHA,PDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
!              FUNCTION VALUE FOR THE GENERALIZED TRAPEZOID
!              DISTRIBUTION.
!              THIS DISTRIBUTION HAS THE FOLLOWING PDF FUNCTION:
!              f(X,A,B,C,D,N1,N2,ALPHA)
!              = [2*ALPHA*NU1*NU3/
!              (2*ALPHA*(B-A)*NU3+(ALPHA+1)*(C-B)*NU1*NU3+2*(D-C)*NU1)]
!              *((X-A)/(B-A))**(NU1-1)
!                                                     A <= X <  B
!              = [2*NU1*NU3/
!              (2*ALPHA*(B-A)*NU3+(ALPHA+1)*(C-B)*NU1*NU3+2*(D-C)*NU1)]
!              *{(ALPHA-1)*(C-X)/(C-B)+1}
!                                                     B <= X <  C
!              = [2*NU1*NU3/
!              (2*ALPHA*(B-A)*NU3+(ALPHA+1)*(C-B)*NU1*NU3+2*(D-C)*NU1)]
!              *((D-X)/(D-C))**(NU3-1)
!                               = U*((D-X)/(D-C))     C <= X <  D
!                               = 0                   X < A, X >= D
!              WHERE
!                  A <= B <= C <= D, NU1, NU3, ALPHA > 0
!              THIS DISTRIBUTION MODELS A "GROWTH PHASE",
!              A "STABLE PHASE", AND A "DECAY PHASE".
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE PROBABILITY DENSITY
!                                FUNCTION IS TO BE EVALUATED.
!                     --A      = THE SINGLE PRECISION SHAPE PARAMETER
!                       B      = THE SINGLE PRECISION SHAPE PARAMETER
!                       C      = THE SINGLE PRECISION SHAPE PARAMETER
!                       D      = THE SINGLE PRECISION SHAPE PARAMETER
!                       ANU1   = THE SINGLE PRECISION SHAPE PARAMETER
!                       ANU3   = THE SINGLE PRECISION SHAPE PARAMETER
!                       ALPHA  = THE SINGLE PRECISION SHAPE PARAMETER
!     OUTPUT ARGUMENTS--PDF    = THE SINGLE PRECISION PROBABILITY
!                                DENSITY FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
!             FUNCTION VALUE PDF.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE BETWEEN A AND D, INCLUSIVELY.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--J. RENE VAN DORP AND SAMIEL KOTZ, "GENERALIZED
!                 TRAPEZOIDAL DISTRIBUTIONS", METRIKA, VOL. 58,
!                 ISSUE 1, JULY 2003.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING LABORATORY (205.03)
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899
!                 PHONE:  301-975-2855
!     ORIGINAL VERSION--JUNE      2003.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
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
!
      INCLUDE 'DPCOP2.INC'
!
!---------------------------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      PDF=0.0
      DPDF=0.0D0
      IF(A.GT.B .OR. B.GT.C .OR. C.GT.D)THEN
        WRITE(ICOUT,12)
   12   FORMAT('***** ERROR--FOR THE GENERALZIED TRAPEZOID ',   &
               'DISTRIBUTION,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,13)
   13   FORMAT('      THE FOUR SHAPE PARAMETERS (A, B, C, D) MUST ',   &
               'SATISFY')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,14)
   14   FORMAT('         A <= B <= C <= D.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,16)A,B,C,D
   16   FORMAT('      A, B, C, D = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(ALPHA.LE.0.0)THEN
        WRITE(ICOUT,22)
   22   FORMAT('***** ERROR--FOR THE GENERALIZED TRAPEZOID ',   &
               'DISTRIBUTION,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,23)ALPHA
   23   FORMAT('      THE ALPHA SHAPE PARAMETER MUST BE > 0.  ',   &
               'ALPHA = ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(ANU1.LE.0.0)THEN
        WRITE(ICOUT,32)
   32   FORMAT('***** ERROR--FOR THE GENERALIZED TRAPEZOID ',   &
               'DISTRIBUTION,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,33)ANU1
   33   FORMAT('      THE NU1 SHAPE PARAMETER MUST BE > 0.  NU1 = ',   &
               G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(ANU3.LE.0.0)THEN
        WRITE(ICOUT,42)
   42   FORMAT('***** ERROR--FOR THE GENERALIZED TRAPEZOID ',   &
               'DISTRIBUTION,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,43)ANU3
   43   FORMAT('      THE NU3 SHAPE PARAMETER MUST BE > 0.  NU3 = ',   &
               G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
!-----START POINT-----------------------------------------------------
!
      IF(X.LT.A .OR. X.GE.D)THEN
        PDF=0.0
        GO TO 9000
      ENDIF
!
      DX=DBLE(X)
      DA=DBLE(A)
      DB=DBLE(B)
      DC=DBLE(C)
      DD=DBLE(D)
      DNU1=DBLE(ANU1)
      DNU3=DBLE(ANU3)
      DALPHA=DBLE(ALPHA)
      DTERM1=2.0D0*DNU1*DNU3
      DTERM2=2.0D0*DALPHA*(DB-DA)*DNU3 +   &
             (DALPHA+1.0D0)*(DC-DB)*DNU1*DNU3 +   &
             2.0D0*(DD-DC)*DNU1
      DTERM3=DTERM1/DTERM2
!
      IF(A.LE.X .AND. X.LT.B)THEN
        IF(A.EQ.X .AND. ANU1.LE.1.0)THEN
          WRITE(ICOUT,132)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,133)ANU1
          CALL DPWRST('XXX','BUG ')
          PDF=0.0
          GO TO 9000
        ENDIF
  132 FORMAT(   &
      '***** FATAL ERROR--FOR THE GENERALIZED TRAPEZOID DISTRIBUTION,')
  133 FORMAT(   &
      '      WHEN X = A AND NU1 <= 1, THE PDF IS UNDEFINED.')
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
          GO TO 9000
        ENDIF
  232 FORMAT(   &
      '***** FATAL ERROR--FOR THE GENERALIZED TRAPEZOID DISTRIBUTION,')
  233 FORMAT(   &
      '      WHEN X = D AND NU3 <= 1, THE PDF IS UNDEFINED.')
        DPDF=DTERM3*((DD-DX)/(DD-DC))**(DNU3-1.0D0)
      ENDIF
      PDF=REAL(DPDF)
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE GTRPDF
      SUBROUTINE GTRPPF(P,A,B,C,D,ANU1,ANU3,ALPHA,PPF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
!              FUNCTION VALUE FOR THE GENERALZIED TRAPEZOID
!              DISTRIBUTION.
!              THIS DISTRIBUTION HAS THE FOLLOWING CDF FUNCTION:
!              F(X,A,B,C,D,N1,N2,ALPHA)
!              = 0                                    X < A
!              = [2*ALPHA*(B-A)*NU3/
!              (2*ALPHA*(B-A)*NU3+(ALPHA+1)*(C-B)*NU1*NU3+2*(D-C)*NU1)]
!              *((X-A)/(B-A))**NU1
!                                                     A <= X <  B
!              = [2*ALPHA*(B-A)*NU3 + 2*(X-B)*NU1*NU3*
!              {1 + (ALPHA-1)*(2*C-B-X)/(2*(C-B)}]/
!              [(2*ALPHA*(B-A)*NU3+(ALPHA+1)*(C-B)*NU1*NU3+2*(D-C)*NU1)]
!              *{(ALPHA-1)*(C-X)/(C-B)+1}
!                                                     B <= X <  C
!              = 1 -
!              [2*(D-C)*NU1]/
!              (2*ALPHA*(B-A)*NU3+(ALPHA+1)*(C-B)*NU1*NU3+2*(D-C)*NU1)]
!              *((D-X)/(D-C))**NU3
!                                                     C <= X <  D
!              = 1                                    X >= D
!              WHERE
!                  A <= B <= C <= D, NU1, NU3, ALPHA > 0
!              THE ALGORITHM FOR THE PPF IS TO COMPUTE THE CDF AT
!              X = A, X = B, X = C, AND X = D TO FIND THE APPROPRIATE
!              INTERVAL FOR P.  THEN INVERT THE APPROPRIATE EQUATION
!              ABOVE TO FIND THE PPF VALUE.  FOR THE INTERVAL FOR
!              B < X < C, USE A BISECTION METHOD (ALGEBRA FOR
!              INVERSION GETS A BIT MESSY).
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE PROBABILITY DENSITY
!                                FUNCTION IS TO BE EVALUATED.
!                     --A      = THE SINGLE PRECISION SHAPE PARAMETER
!                       B      = THE SINGLE PRECISION SHAPE PARAMETER
!                       C      = THE SINGLE PRECISION SHAPE PARAMETER
!                       D      = THE SINGLE PRECISION SHAPE PARAMETER
!                       ANU1   = THE SINGLE PRECISION SHAPE PARAMETER
!                       ANU3   = THE SINGLE PRECISION SHAPE PARAMETER
!                       ALPHA  = THE SINGLE PRECISION SHAPE PARAMETER
!     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT POINT
!                                FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PERCENT POINT FUNCTION VALUE PPF.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--P SHOULD BE BETWEEN 0 AND 1, INCLUSIVELY.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--J. RENE VAN DORP AND SAMIEL KOTZ, "GENERALIZED
!                 TRAPEZOIDAL DISTRIBUTIONS", METRIKA, VOL. 58,
!                 ISSUE 1, JULY 2003.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING LABORATORY (205.03)
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899
!                 PHONE:  301-975-2855
!     ORIGINAL VERSION--JUNE      2003.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
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
!
      INCLUDE 'DPCOP2.INC'
!
      DATA DEPS /0.0000001/
      DATA DSIG /1.0D-7/
!CCCC DATA DZERO /0./
      DATA MAXIT /2000/
!
!---------------------------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(P.LT.0.0 .OR. P.GT.1.0)THEN
        WRITE(ICOUT,22)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,23)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,26)P
        CALL DPWRST('XXX','BUG ')
        PPF=0.0
        GO TO 9000
      ENDIF
!
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
        GO TO 9000
      ENDIF
      IF(ALPHA.LE.0.0)THEN
        WRITE(ICOUT,22)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,23)ALPHA
        CALL DPWRST('XXX','BUG ')
        PPF=0.0
        GO TO 9000
      ENDIF
      IF(ANU1.LE.0.0)THEN
        WRITE(ICOUT,32)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,33)ANU1
        CALL DPWRST('XXX','BUG ')
        PPF=0.0
        GO TO 9000
      ENDIF
      IF(ANU3.LE.0.0)THEN
        WRITE(ICOUT,42)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,43)ANU3
        CALL DPWRST('XXX','BUG ')
        PPF=0.0
        GO TO 9000
      ENDIF
!
   12 FORMAT(   &
      '***** FATAL ERROR--FOR THE GENERALZIED TRAPEZOID DISTRIBUTION,')
   13 FORMAT(   &
      '      THE FOUR SHAPE PARAMETERS (A, B, C, D) MUST SATISFY')
   14 FORMAT(   &
      '         A < B < C < D')
   16 FORMAT(   &
      '      A, B, C, D = ',4E15.7)
   22 FORMAT(   &
      '***** FATAL ERROR--FOR THE GENERALZIED TRAPEZOID PERCENT,')
   23 FORMAT(   &
      '      POINT FUNCTION THE VALUE OF THE INPUT ARGUMENT IS ',   &
      'OUTSIDE THE ALLOWABLE (0,1) INTERVAL.')
   26 FORMAT(   &
      '      VALUE OF INPUT ARGUMENT = ',E15.7)
   32 FORMAT(   &
      '***** FATAL ERROR--FOR THE GENERALIZED TRAPEZOID DISTRIBUTION,')
   33 FORMAT(   &
      '      THE NU1 SHAPE PARAMETER MUST BE > 0.  NU1 = ',E15.7)
   42 FORMAT(   &
      '***** FATAL ERROR--FOR THE GENERALIZED TRAPEZOID DISTRIBUTION,')
   43 FORMAT(   &
      '      THE NU3 SHAPE PARAMETER MUST BE > 0.  NU3 = ',E15.7)
!
!-----START POINT-----------------------------------------------------
!
      DPPF=0.0D0
      P1=0.0
      CALL GTRCDF(B,A,B,C,D,ANU1,ANU3,ALPHA,P2)
      CALL GTRCDF(C,A,B,C,D,ANU1,ANU3,ALPHA,P3)
      P4=1.0
!
      IF(P.EQ.0.0)THEN
        PPF=A
        GO TO 9000
      ELSEIF(P.EQ.1.0)THEN
        PPF=D
        GO TO 9000
      ELSEIF(P.EQ.P2)THEN
        PPF=B
        GO TO 9000
      ELSEIF(P.EQ.P3)THEN
        PPF=C
        GO TO 9000
      ENDIF
!
      DP=DBLE(P)
      DALPHA=DBLE(ALPHA)
      DNU1=DBLE(ANU1)
      DNU3=DBLE(ANU3)
      DA=DBLE(A)
      DB=DBLE(B)
      DC=DBLE(C)
      DD=DBLE(D)
      DTERM2=2.0D0*DALPHA*(DB-DA)*DNU3 +   &
             (DALPHA+1.0D0)*(DC-DB)*DNU1*DNU3 +   &
             2.0D0*(DD-DC)*DNU1
!
      IF(P.GE.P1 .AND. P.LE.P2)THEN
        DTERM1=2.0D0*DALPHA*(DB-DA)*DNU3
        DPPF=(DB-DA)*((DTERM2/DTERM1)*DP)**(1.0D0/DNU1) + DA
      ELSEIF(P.GE.P2 .AND. P.LE.P3)THEN
        DXL=DB
        DXR=DC
!
!       BISECTION METHOD
!
        IC = 0
        DFXL = -DP
        DFXR = 1.0D0 - DP
  105   CONTINUE
          DX = (DXL+DXR)*0.5D0
!
!         GTRCDF FOR B < X < C CASE
!
          DTERM1=2.0D0*DALPHA*(DB-DA)*DNU3
          DTERM3=2.0D0*(DX-DB)*DNU1*DNU3
          DTERM4=1.0D0+(DALPHA-1.0D0)*(2.0D0*DC-DB-DX)/(2.0D0*(DC-DB))
          DCDF=(DTERM1 + DTERM3*DTERM4)/DTERM2
!
          DP1=DCDF
          DPPF=DX
          PPF=REAL(DPPF)
          DFCS = DP1 - DP
          IF(DFCS*DFXL.GT.0.0D0)GO TO 110
          DXR = DX
          DFXR = DFCS
          GO TO 115
  110     CONTINUE
          DXL = DX
          DFXL = DFCS
  115     CONTINUE
          DXRML = DXR - DXL
          IF(DXRML.LE.DSIG .AND. DABS(DFCS).LE.DEPS)GO TO 9000
          IC = IC + 1
          IF(IC.LE.MAXIT)GO TO 105
          WRITE(ICOUT,130)
          CALL DPWRST('XXX','BUG ')
  130     FORMAT(   &
          '***** FATAL ERROR--GENERALIZED TRAPEZOID PPF ROUTINE DID ',   &
          'NOT CONVERGE.')
        GO TO 9000
!
      ELSEIF(P.GE.P3 .AND. P.LE.P4)THEN
        DTERM1=2.0D0*(DD-DC)*DNU1
        DPPF=DD - (DD-DC)*((1.0D0-DP)*(DTERM2/DTERM1))**(1.0D0/DNU3)
      ENDIF
      PPF=REAL(DPPF)
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE GTRPPF
      SUBROUTINE GTRRAN(N,A,B,C,D,ANU1,ANU3,ALPHA,ISEED,X)
!
!     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
!              FROM THE GENERALIZED TRAPEZOID DISTRIBUTION
!              THIS DISTRIBUTION HAS THE FOLLOWING PDF FUNCTION:
!              f(X,A,B,C,D,N1,N2,ALPHA)
!              = [2*ALPHA*NU1*NU3/
!              (2*ALPHA*(B-A)*NU3+(ALPHA+1)*(C-B)*NU1*NU3+2*(D-C)*NU1)]
!              *((X-A)/(B-A))**(NU1-1)
!                                                     A <= X <  B
!              = [2*NU1*NU3/
!              (2*ALPHA*(B-A)*NU3+(ALPHA+1)*(C-B)*NU1*NU3+2*(D-C)*NU1)]
!              *{(ALPHA-1)*(C-X)/(C-B)+1}
!                                                     B <= X <  C
!              = [2*NU1*NU3/
!              (2*ALPHA*(B-A)*NU3+(ALPHA+1)*(C-B)*NU1*NU3+2*(D-C)*NU1)]
!              *((D-X)/(D-C))**(NU3-1)
!                               = U*((D-X)/(D-C))     C <= X <  D
!                               = 0                   X < A, X >= D
!              WHERE
!                  A <= B <= C <= D, NU1, NU3, ALPHA > 0
!              THIS DISTRIBUTION MODELS THE SIMPLEST CASE OF
!              A "GROWTH PHASE", A "STABLE PHASE", AND A "DECAY PHASE".
!     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
!                                OF RANDOM NUMBERS TO BE
!                                GENERATED.
!     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
!                                (OF DIMENSION AT LEAST N)
!                                INTO WHICH THE GENERATED
!                                RANDOM SAMPLE WILL BE PLACED.
!                     --A      = THE SINGLE PRECISION SHAPE PARAMETER
!                       B      = THE SINGLE PRECISION SHAPE PARAMETER
!                       C      = THE SINGLE PRECISION SHAPE PARAMETER
!                       D      = THE SINGLE PRECISION SHAPE PARAMETER
!                       ANU1   = THE SINGLE PRECISION SHAPE PARAMETER
!                       ANU3   = THE SINGLE PRECISION SHAPE PARAMETER
!                       ALPHA  = THE SINGLE PRECISION SHAPE PARAMETER
!     OUTPUT--A RANDOM SAMPLE OF SIZE N
!             FROM THE GENERALIZED TRAPEZOID DISTRIBUTION
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN, GTRPPF
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--J. RENE VAN DORP AND SAMIEL KOTZ, "GENERALIZED
!                 GENERALIZED TRAPEZOIDAL DISTRIBUTIONS", METRIKA, VOL. 58,
!                 ISSUE 1, JULY 2003.
!               --TOCHER, THE ART OF SIMULATION,
!                 1963, PAGES 14-15.
!               --HAMMERSLEY AND HANDSCOMB, MONTE CARLO METHODS,
!                 1964, PAGE 36.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2003.6
!     ORIGINAL VERSION--JUNE      2003.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(N.LT.1)THEN
        WRITE(ICOUT, 5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
    5 FORMAT('***** FATAL ERROR--THE REQUESTED NUMBER OF GENERALIZED',   &
      ' TRAPEZOID RADOM NUMBERS IS NON-POSITIVE *****')
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
        GO TO 9000
      ENDIF
   12 FORMAT(   &
      '***** FATAL ERROR--FOR THE GENERALIZED TRAPEZOID DISTRIBUTION,')
   13 FORMAT(   &
      '      THE FOUR SHAPE PARAMETERS (A, B, C, D) MUST SATISFY')
   14 FORMAT(   &
      '         A < B < C < D')
   16 FORMAT(   &
      '      A, B, C, D = ',4E15.7)
      IF(ALPHA.LE.0.0)THEN
        WRITE(ICOUT,22)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,23)ALPHA
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GO TO 9000
      ENDIF
      IF(ANU1.LE.0.0)THEN
        WRITE(ICOUT,32)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,33)ANU1
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GO TO 9000
      ENDIF
      IF(ANU3.LE.0.0)THEN
        WRITE(ICOUT,42)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,43)ANU3
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GO TO 9000
      ENDIF
   22 FORMAT(   &
      '***** FATAL ERROR--FOR THE GENERALIZED TRAPEZOID DISTRIBUTION,')
   23 FORMAT(   &
      '      THE ALPHA SHAPE PARAMETER MUST BE > 0.  ALPHA = ',E15.7)
   32 FORMAT(   &
      '***** FATAL ERROR--FOR THE GEnERALIZED TRAPEZOID DISTRIBUTION,')
   33 FORMAT(   &
      '      THE NU1 SHAPE PARAMETER MUST BE > 0.  NU1 = ',E15.7)
   42 FORMAT(   &
      '***** FATAL ERROR--FOR THE GENERALIZED TRAPEZOID DISTRIBUTION,')
   43 FORMAT(   &
      '      THE NU3 SHAPE PARAMETER MUST BE > 0.  NU3 = ',E15.7)
!
!
!     GENERATE N UNIFORM (0,1) RANDOM NUMBERS;
!
      CALL UNIRAN(N,ISEED,X)
!
!     GENERATE N GENERALIZED TRAPEZOID RANDOM NUMBERS
!     USING THE PERCENT POINT FUNCTION TRANSFORMATION METHOD.
!
      DO 100 I=1,N
      P=X(I)
      CALL GTRPPF(P,A,B,C,D,ANU1,ANU3,ALPHA,PPF)
      X(I)=PPF
  100 CONTINUE
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE GTRRAN
      SUBROUTINE GVECT(X,Y,N,   &
                       XTEMP,YTEMP,TATEMP,NTEMP,NTRACE,   &
                       IBUGG3,ISUBRO,IERROR)
!
!     PURPOSE--XX
!
!     WRITTEN BY--DAVID W. BEHRINGER NOAA/AOML (MIAMI).
!                 AS PART OF NOAA'S CONCX V.3   MARCH 1988.
!     ORIGINAL VERSION (IN DATAPLOT)--AUGUST    1988.
!
!---------------------------------------------------------------------
!
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      DIMENSION X(*)
      DIMENSION Y(*)
!
      DIMENSION XTEMP(*)
      DIMENSION YTEMP(*)
      DIMENSION TATEMP(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
!
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'VECT')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1011)N
 1011   FORMAT('FROM GVECT--N = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO 1015 I=1,N
          WRITE(ICOUT,1016)I,X(I),Y(I)
 1016     FORMAT('            I,X(I),Y(I) = ',I8,2F10.5)
          CALL DPWRST('XXX','BUG ')
 1015   CONTINUE
      ENDIF
!
      NTRACE=NTRACE+1
      DO 1100 I=1,N
      NTEMP=NTEMP+1
      XTEMP(NTEMP)=X(I)
      YTEMP(NTEMP)=Y(I)
      TATEMP(NTEMP)=NTRACE
 1100 CONTINUE
!
      RETURN
      END SUBROUTINE GVECT
      SUBROUTINE GWACDF(X,ALPHA,BETA,K,CDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
!              FUNCTION VALUE FOR THE DISCRETE BETA-NEGATIVE
!              BINOMIAL (OR GENERALIZED WARING) DISTRIBUTION WITH
!              SHAPE PARAMETERS K, ALPHA, AND BETA.
!              THIS DISTRIBUTION IS DEFINED FOR ALL INTEGER X >= 0.
!              THE PROBABILITY MASS FUNCTION IS:
!              P(X;ALPHA,BETA,K)=G(BETA+ALPHA)*G(K+BETA)*
!                  G(X+K)*G(X+ALPHA)/
!                  [G(K)*G(BETA)*G(ALPHA)*G(X+1)*G(X+K+BETA+ALPHA)]
!                  X = 0, 1, 2, ...
!              WHERE G IS THE GAMMA FUNCTION.  NOTE THAT THERE ARE
!              A NUMBER OF DIFFERENT PARAMETERIZATIONS OF THIS
!              DISTRIBUTION IN THE LITERATURE.  WE USE THIS
!              PARAMETERIZATION BECAUSE IT MAKES THE RELATIONSHIPS
!              WITH THE NEGATIVE BINOMIAL, BETA-BINOMIAL, AND
!              BETA-GEOMETRIC CLEAR AND IT ALSO PROVIDES A
!              COMPUTATIONALLY CONVENIENT FORM.
!
!              WE COMPUTE THE CDF USING A RECURENCE RELATION
!              DERIVED BY HESSELAGER:
!
!              P(X) = P(X-1)*[X+(K-1)]*[X+(ALPHA-1)]/
!                     [X*(X+(ALPHA+BETA+K-1))]
!
!     INPUT  ARGUMENTS--X      = THE DOUBLE PRECISION VALUE AT
!                                WHICH THE PROBABILITY DENSITY
!                                FUNCTION IS TO BE EVALUATED.
!                                X SHOULD BE NON-NEGATIVE.
!                     --ALPHA  = THE FIRST SHAPE PARAMETER
!                     --BETA   = THE SECOND SHAPE PARAMETER
!                     --K      = THE THIRD SHAPE PARAMETER
!     OUTPUT ARGUMENTS--CDF    = THE DOUBLE PRECISION DENSITY
!                                FUNCTION VALUE.
!     OUTPUT--THE DOUBLE PRECISION PROBABILITY DENSITY
!             FUNCTION VALUE
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE A NON-NEGATIVE INTEGER
!                 --K, ALPHA, AND BETA > 0
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--OLE HESSELAGER (1994).  "A RECURSIVE PROCEDURE
!                 FOR CALCULATIONS OF SOME COMPOUND DISTRIBUTIONS",
!                 ASTIN BULLETIN, VOL. 24, NO. 1, PP. 19-32.
!               --IRWIN (1975), "THE GENERALIZED WARING DISTRIBUTION
!                 PART 1", JOURNAL OF THE ROYAL STATISTICAL SOCIETY,
!                 SERIES A, 138, PP. 18-31.
!               --IRWIN (1975), "THE GENERALIZED WARING DISTRIBUTION
!                 PART 1", JOURNAL OF THE ROYAL STATISTICAL SOCIETY,
!                 SERIES A, 138, PP. 204-227.
!               --IRWIN (1975), "THE GENERALIZED WARING DISTRIBUTION
!                 PART 1", JOURNAL OF THE ROYAL STATISTICAL SOCIETY,
!                 SERIES A, 138, PP. 374-378.
!               --JOHNSON, KOTZ, AND KEMP, "DISCRETE UNIVARIATE
!                 DISTRIBUTIONS", SECOND EDITION, 1992, WILEY,
!                 PP. 242-244.
!               --LUC DEVROYE, "RANDOM VARIATE GENERATION FOR THE
!                 DIGAMMA AND TRIGAMMA DISTRIBUTIONS", JOURNAL OF
!                 STATISTICAL COMPUTATION AND SIMULATION", VOL. 43,
!                 1992, PP. 197-216.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2006/5
!     ORIGINAL VERSION--MAY       2006.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-----------------
!
!-------------------------------------------------------------------
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION K
!
!----COMMON---------------------------------------------------------
!
      REAL R1MACH
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT---------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      CDF=0.0D0
      IX=INT(X+0.5D0)
      IF(K.LE.0.0D0)THEN
        WRITE(ICOUT,5)
    5   FORMAT('***** ERROR--THE FOURTH ARGUMENT TO GWACDF ',   &
               'IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)K
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(ALPHA.LE.0.0D0)THEN
        WRITE(ICOUT,15)
   15   FORMAT('***** ERROR--THE SECOND ARGUMENT TO GWACDF ',   &
               'IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)ALPHA
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(BETA.LE.0.0D0)THEN
        WRITE(ICOUT,25)
   25   FORMAT('***** ERROR--THE THIRD ARGUMENT TO GWACDF ',   &
               'IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(IX.LT.0)THEN
        WRITE(ICOUT,4)
    4   FORMAT('***** ERROR--THE FIRST ARGUMENT TO GWACDF ',   &
               'IS LESS THAN 0.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(X.GT.DBLE(I1MACH(9)))THEN
        WRITE(ICOUT,55)
   55   FORMAT('***** ERROR--THE FIRST ARGUMENT TO GWACDF ',   &
               'IS GREATER THAN.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,56)
   56   FORMAT('      THE LARGEST MACHINE INTEGER.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
!
      CDF=0.0D0
      DX=0.0D0
      CALL GWAPDF(DX,ALPHA,BETA,K,PDFSV)
!
      CDF=PDFSV
      IF(IX.GT.0)THEN
        DO 100 I=1,IX
          DX=DBLE(I)
          DTERM1=(DX + (K-1.0D0))*(DX + (ALPHA-1.0D0))
          DTERM2=DX*(DX + (ALPHA+BETA+K-1.0D0))
          PDF=(DTERM1/DTERM2)*PDFSV
          CDF=CDF + PDF
          PDFSV=PDF
  100   CONTINUE
      ENDIF
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE GWACDF
      SUBROUTINE GWAPDF(X,ALPHA,BETA,K,PDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
!              FUNCTION VALUE FOR THE DISCRETE BETA-NEGATIVE
!              BINOMIAL (OR GENERALIZED WARING) DISTRIBUTION WITH
!              SHAPE PARAMETERS K, ALPHA, AND BETA.
!              THIS DISTRIBUTION IS DEFINED FOR ALL INTEGER X >= 0.
!              THE PROBABILITY MASS FUNCTION IS:
!              P(X;ALPHA,BETA,K)=G(BETA+ALPHA)*G(K+BETA)*
!                  G(X+K)*G(X+ALPHA)/
!                  [G(K)*G(BETA)*G(ALPHA)*G(X+1)*G(X+K+BETA+ALPHA)]
!                  X = 0, 1, 2, ...
!              WHERE G IS THE GAMMA FUNCTION.  NOTE THAT THERE ARE
!              A NUMBER OF DIFFERENT PARAMETERIZATIONS OF THIS
!              DISTRIBUTION IN THE LITERATURE.  WE USE THIS
!              PARAMETERIZATION BECAUSE IT MAKES THE RELATIONSHIPS
!              WITH THE NEGATIVE BINOMIAL, BETA-BINOMIAL, AND
!              BETA-GEOMETRIC CLEAR AND IT ALSO PROVIDES A
!              COMPUTATIONALLY CONVENIENT FORM.
!     INPUT  ARGUMENTS--X      = THE DOUBLE PRECISION VALUE AT
!                                WHICH THE PROBABILITY DENSITY
!                                FUNCTION IS TO BE EVALUATED.
!                                X SHOULD BE NON-NEGATIVE.
!                     --ALPHA  = THE FIRST SHAPE PARAMETER
!                     --BETA   = THE SECOND SHAPE PARAMETER
!                     --K      = THE THIRD SHAPE PARAMETER
!     OUTPUT ARGUMENTS--PDF    = THE DOUBLE PRECISION DENSITY
!                                FUNCTION VALUE.
!     OUTPUT--THE DOUBLE PRECISION PROBABILITY DENSITY
!             FUNCTION VALUE
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE A NON-NEGATIVE INTEGER
!                 --C, ALPHA, AND BETA > 0
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--OLE HESSELAGER (1994).  "A RECURSIVE PROCEDURE
!                 FOR CALCULATIONS OF SOME COMPOUND DISTRIBUTIONS",
!                 ASTIN BULLETIN, VOL. 24, NO. 1, PP. 19-32.
!               --IRWIN (1975), "THE GENERALIZED WARING DISTRIBUTION
!                 PART 1", JOURNAL OF THE ROYAL STATISTICAL SOCIETY,
!                 SERIES A, 138, PP. 18-31.
!               --IRWIN (1975), "THE GENERALIZED WARING DISTRIBUTION
!                 PART 1", JOURNAL OF THE ROYAL STATISTICAL SOCIETY,
!                 SERIES A, 138, PP. 204-227.
!               --IRWIN (1975), "THE GENERALIZED WARING DISTRIBUTION
!                 PART 1", JOURNAL OF THE ROYAL STATISTICAL SOCIETY,
!                 SERIES A, 138, PP. 374-378.
!               --JOHNSON, KOTZ, AND KEMP, "DISCRETE UNIVARIATE
!                 DISTRIBUTIONS", SECOND EDITION, 1992, WILEY,
!                 PP. 242-244.
!               --LUC DEVROYE, "RANDOM VARIATE GENERATION FOR THE
!                 DIGAMMA AND TRIGAMMA DISTRIBUTIONS", JOURNAL OF
!                 STATISTICAL COMPUTATION AND SIMULATION", VOL. 43,
!                 1992, PP. 197-216.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2006/5
!     ORIGINAL VERSION--MAY       2006.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-----------------
!
!-------------------------------------------------------------------
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION K
!
!-----COMMON--------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT---------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      PDF=0.0D0
      IX=INT(X+0.5D0)
      IF(K.LE.0.0D0)THEN
        WRITE(ICOUT,5)
    5   FORMAT('***** ERROR--THE FOURTH ARGUMENT TO GWAPDF ',   &
               'IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)K
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(ALPHA.LE.0.0D0)THEN
        WRITE(ICOUT,15)
   15   FORMAT('***** ERROR--THE SECOND ARGUMENT TO GWAPDF ',   &
               'IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)ALPHA
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(BETA.LE.0.0D0)THEN
        WRITE(ICOUT,25)
   25   FORMAT('***** ERROR--THE THIRD ARGUMENT TO GWAPDF ',   &
               'IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(IX.LT.0)THEN
        WRITE(ICOUT,4)
    4   FORMAT('***** ERROR--THE FIRST ARGUMENT TO GWAPDF ',   &
               'IS LESS THAN 0.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(X.GT.DBLE(I1MACH(9)))THEN
        WRITE(ICOUT,55)
   55   FORMAT('***** ERROR--THE FIRST ARGUMENT TO GWAPDF ',   &
               'IS GREATER THAN.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,56)
   56   FORMAT('      THE LARGEST MACHINE INTEGER.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
!
      DX=DBLE(IX)
      DA=ALPHA
      DB=BETA
      DC=K
!
      DTERM1=DLNGAM(DX+DC)
      DTERM2=DLNGAM(DA+DB)
      DTERM3=DLNGAM(DX+DA)
      DTERM4=DLNGAM(DC+DB)
      DNUM=DTERM1+DTERM2+DTERM3+DTERM4
!
      DTERM5=DLNGAM(DA)
      DTERM6=DLNGAM(DB)
      DTERM7=DLNGAM(DC)
      DTERM8=DLNGAM(DX+1.0D0)
      DTERM9=DLNGAM(DX+DA+DB+DC)
      DENOM=DTERM5+DTERM6+DTERM7+DTERM8+DTERM9
!
      DTERM9=DNUM-DENOM
      PDF=DEXP(DNUM-DENOM)
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE GWAPDF
      SUBROUTINE GWAPPF(P,ALPHA,BETA,K,PPF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
!              FUNCTION VALUE FOR THE DISCRETE BETA-NEGATIVE
!              BINOMIAL (OR GENERALIZED WARING) DISTRIBUTION WITH
!              SHAPE PARAMETERS K, ALPHA, AND BETA.
!              THIS DISTRIBUTION IS DEFINED FOR 0 <= P < 1.
!              THE PROBABILITY MASS FUNCTION IS:
!              P(X;ALPHA,BETA,K)=G(BETA+ALPHA)*G(K+BETA)*
!                  G(X+K)*G(X+ALPHA)/
!                  [G(K)*G(BETA)*G(ALPHA)*G(X+1)*G(X+K+BETA+ALPHA)]
!                  X = 0, 1, 2, ...
!              WHERE G IS THE GAMMA FUNCTION.  NOTE THAT THERE ARE
!              A NUMBER OF DIFFERENT PARAMETERIZATIONS OF THIS
!              DISTRIBUTION IN THE LITERATURE.  WE USE THIS
!              PARAMETERIZATION BECAUSE IT MAKES THE RELATIONSHIPS
!              WITH THE NEGATIVE BINOMIAL, BETA-BINOMIAL, AND
!              BETA-GEOMETRIC CLEAR AND IT ALSO PROVIDES A
!              COMPUTATIONALLY CONVENIENT FORM.
!
!              WE COMPUTE THE CDF USING A RECURENCE RELATION
!              DERIVED BY HESSELAGER:
!
!              P(X) = P(X-1)*[X+(K-1)]*[X+(ALPHA-1)]/
!                     [X*(X+(ALPHA+BETA+K-1))]
!
!              WE COMPUTE THE PERCENT POINT FUNCTION BY COMPUTING
!              THE CDF FUNCTION UNTIL THE SPECIFIED VALUE OF P
!              IS REACHED.
!
!     INPUT  ARGUMENTS--P      = THE DOUBLE PRECISION VALUE AT
!                                WHICH THE PERCENT POINT
!                                FUNCTION IS TO BE EVALUATED.
!                                0 <= P < 1.
!                     --ALPHA  = THE FIRST SHAPE PARAMETER
!                     --BETA   = THE SECOND SHAPE PARAMETER
!                     --K      = THE THIRD SHAPE PARAMETER
!     OUTPUT ARGUMENTS--PPF    = THE DOUBLE PRECISION PERCENT POINT
!                                FUNCTION VALUE.
!     OUTPUT--THE DOUBLE PRECISION PERCENT POINT FUNCTION VALUE
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--0 <= P < 1.
!                 --K, ALPHA, AND BETA > 0
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--OLE HESSELAGER (1994).  "A RECURSIVE PROCEDURE
!                 FOR CALCULATIONS OF SOME COMPOUND DISTRIBUTIONS",
!                 ASTIN BULLETIN, VOL. 24, NO. 1, PP. 19-32.
!               --IRWIN (1975), "THE GENERALIZED WARING DISTRIBUTION
!                 PART 1", JOURNAL OF THE ROYAL STATISTICAL SOCIETY,
!                 SERIES A, 138, PP. 18-31.
!               --IRWIN (1975), "THE GENERALIZED WARING DISTRIBUTION
!                 PART 1", JOURNAL OF THE ROYAL STATISTICAL SOCIETY,
!                 SERIES A, 138, PP. 204-227.
!               --IRWIN (1975), "THE GENERALIZED WARING DISTRIBUTION
!                 PART 1", JOURNAL OF THE ROYAL STATISTICAL SOCIETY,
!                 SERIES A, 138, PP. 374-378.
!               --JOHNSON, KOTZ, AND KEMP, "DISCRETE UNIVARIATE
!                 DISTRIBUTIONS", SECOND EDITION, 1992, WILEY,
!                 PP. 242-244.
!               --LUC DEVROYE, "RANDOM VARIATE GENERATION FOR THE
!                 DIGAMMA AND TRIGAMMA DISTRIBUTIONS", JOURNAL OF
!                 STATISTICAL COMPUTATION AND SIMULATION", VOL. 43,
!                 1992, PP. 197-216.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2006/5
!     ORIGINAL VERSION--MAY       2006.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-----------------
!
!-------------------------------------------------------------------
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION K
      DOUBLE PRECISION DEPS
      DOUBLE PRECISION DP
!
!-----COMMON--------------------------------------------------------
!
      REAL R1MACH
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT---------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      PPF=0.0D0
      IF(K.LE.0.0D0)THEN
        WRITE(ICOUT,5)
    5   FORMAT('***** ERROR--THE FOURTH ARGUMENT TO GWACDF ',   &
               'IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)K
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(ALPHA.LE.0.0D0)THEN
        WRITE(ICOUT,15)
   15   FORMAT('***** ERROR--THE SECOND ARGUMENT TO GWACDF ',   &
               'IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)ALPHA
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(BETA.LE.0.0D0)THEN
        WRITE(ICOUT,25)
   25   FORMAT('***** ERROR--THE THIRD ARGUMENT TO GWACDF ',   &
               'IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(P.LT.0.0.OR.P.GE.1.0)THEN
        WRITE(ICOUT,1)
    1   FORMAT('***** ERROR--THE FIRST ARGUMENT TO GWAPPF ',   &
               'IS OUTSIDE THE ALLOWABLE (0,1] INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!     COMPUTE PDF FOR X = 0
!
      EPS=1.0E-6
      DEPS=1.0D-6
      DX=0.0D0
      CALL GWAPDF(DX,ALPHA,BETA,K,DSUM)
      IF(DSUM.GE.P-EPS)THEN
        PPF=0.0D0
        GO TO 9999
      ENDIF
      DPDFSV=DSUM
      I=0
!
      DP=DBLE(P)
  100 CONTINUE
        I=I+1
        IF(DBLE(I).GE.DBLE(I1MACH(9)))THEN
          WRITE(ICOUT,65)
   65     FORMAT('***** ERROR--THE COMPUTED PERCENT POINT VALUE ',   &
                 'EXCEEDS THE LARGEST MACHINE INTEGER.')
          CALL DPWRST('XXX','BUG ')
          PPF=0.0D0
          GO TO 9999
        ENDIF
        DX=DBLE(I)
        DTERM1=(DX + (K-1.0D0))*(DX + (ALPHA-1.0D0))
        DTERM2=DX*(DX + (ALPHA+BETA+K-1.0D0))
        DPDF=(DTERM1/DTERM2)*DPDFSV
        DPDFSV=DPDF
        DSUM=DSUM + DPDF
        IF(DSUM.GE.DP-DEPS)THEN
          PPF=DBLE(I)
          GO TO 9999
        ENDIF
      GO TO 100
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE GWAPPF
      SUBROUTINE GWARAN(N,ALPHA,BETA,K,ISEED,X)
!
!     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
!              FROM THE BETA NEGATIVE BINOMIAL (GENERALIZED WARING)
!              DISTRIBUTION WITH SINGLE PRECISION SHAPE PARAMETERS
!              K, ALPHA, AND BETA.
!              THIS DISTRIBUTION IS DEFINED FOR ALL INTEGER X >= 0.
!              THE PROBABILITY MASS FUNCTION IS:
!              P(X;ALPHA,BETA,K)=G(BETA+ALPHA)*G(K+BETA)*
!                  G(X+K)*G(X+ALPHA)/
!                  [G(K)*G(BETA)*G(ALPHA)*G(X+1)*G(X+K+BETA+ALPHA)]
!                  X = 0, 1, 2, ...
!              WHERE G IS THE GAMMA FUNCTION.  NOTE THAT THERE ARE
!              A NUMBER OF DIFFERENT PARAMETERIZATIONS OF THIS
!              DISTRIBUTION IN THE LITERATURE.  WE USE THIS
!              PARAMETERIZATION BECAUSE IT MAKES THE RELATIONSHIPS
!              WITH THE NEGATIVE BINOMIAL, BETA-BINOMIAL, AND
!              BETA-GEOMETRIC CLEAR AND IT ALSO PROVIDES A
!              COMPUTATIONALLY CONVENIENT FORM.
!     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
!                                OF RANDOM NUMBERS TO BE
!                                GENERATED.
!                     --ALPHA  = THE FIRST SHAPE PARAMETER
!                     --BETA   = THE SECOND SHAPE PARAMETER
!                     --K      = THE THIRD SHAPE PARAMETER
!     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
!                                (OF DIMENSION AT LEAST N)
!                                INTO WHICH THE GENERATED
!                                RANDOM SAMPLE WILL BE PLACED.
!     OUTPUT--A RANDOM SAMPLE OF SIZE N FROM THE BETA NEGATIVE
!             BINOMIAL (GENERALIZED WARING) DISTRIBUTION
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!                 --K, ALPHA, BETA SHOULD BE > 0
!     OTHER DATAPAC   SUBROUTINES NEEDED--GAMRAN, POIRAN.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--LUC DEVROYE, "RANDOM VARIATE GENERATION FOR THE
!                 DIGAMMA AND TRIGAMMA DISTRIBUTIONS", JOURNAL OF
!                 STATISITCAL COMPUTATION AND SIMULATION", VOL. 43,
!                 PP. 197-216, 1992.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2004/4
!     ORIGINAL VERSION--APRIL     2004.
!     UPDATED         --MAY       2006.  UPDATE TO USE BETA-
!                                        NEGATIVE BINOMIAL
!                                        PARAMETERIZATION (RANDOM
!                                        NUMBER GENERATION ALGORITHM
!                                        NOT MODIFIED)
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-----------------
!
!-------------------------------------------------------------------
!
      REAL K
!
      DIMENSION X(*)
      DIMENSION U(2)
!
!-----COMMON--------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT---------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
!     CONVERT FROM BETA-NEGATIVE BINOMIAL PARAMETERIZATION TO
!     DEVROYE PARAMETERIZATION.
!
      A=K
      B=ALPHA
      C=BETA
!
      IF(N.LT.1)THEN
        WRITE(ICOUT,5)
    5   FORMAT('***** ERROR--NUMBER OF GENERALIZED WARING RANDOM ',   &
               'NUMBERS REQUESTED IS LESS THAN 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
   47   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(A.LE.0.0)THEN
        WRITE(ICOUT,15)
   15   FORMAT('***** ERROR--THE K SHAPE PARAMETER FOR THE ',   &
               'GENERALIZED WARING DISTRIBUTION IS <= 0.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)A
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(B.LE.0.0)THEN
        WRITE(ICOUT,16)
   16   FORMAT('***** ERROR--THE ALPHA SHAPE PARAMETER FOR THE ',   &
               'GENERALIZED WARING DISTRIBUTION IS <= 0.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)B
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(C.LE.0.0)THEN
        WRITE(ICOUT,17)
   17   FORMAT('***** ERROR--THE BETA SHAPE PARAMETER FOR THE ',   &
               'GENERALIZED WARING DISTRIBUTION IS <= 0.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)C
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
!
!     GENERATE N GENERALIZED WARING  RANDOM NUMBERS
!
!     ALGORITHM:
!       1. GENERATE THREE GAMMA RANDOM NUMBERS WITH SHAPE PARAMETERS
!          A, B, AND C (SCALE PARAMETER = 1).
!       2. GAMMA(A)*GAMMA(B)/GAMMA(C) = LAMBDA
!       3. GENERATE A POISSON RANDOM NUMBER WITH SHAPE PARAMETER
!          LAMBDA
!
      NTEMP=1
      DO 100 I=1,N
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
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE GWARAN

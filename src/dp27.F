       SUBROUTINE DPRTL(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1                  IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN TRIPLEX LOWER CASE.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--87/4
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MARCH     1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --MARCH     1987.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICHAR2
      CHARACTER*4 IOP
      CHARACTER*4 IBUGD2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION IOP(*)
      DIMENSION X(*)
      DIMENSION Y(*)
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
      NUMCO=1
      ISTART=1
      ISTOP=1
      NC=1
C
C               ******************************************
C               **  TREAT THE ROMAN SIMPLEX UPPER CASE  **
C               **  HERSHEY CHARACTER SET CASE          **
C               ******************************************
C
C
      IF(IBUGD2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPRTL--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHAR2
   52 FORMAT('ICHAR2 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **************************************************
C               **  STEP 1--                                    **
C               **  SEARCH FOR THE INPUT CHARACTER(S).          **
C               **  MAP THE CHARACTER(S) INTO A NUMERIC VALUE.  **
C               **************************************************
C
      CALL DPCHAL(ICHAR2,ICHARN,IBUGD2,IFOUND)
      IF(IFOUND.EQ.'NO')GOTO9000
C
      IF(ICHARN.LE.6)GOTO1010
      GOTO1019
 1010 CONTINUE
      CALL DRTL1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1019 CONTINUE
C
      IF(7.LE.ICHARN.AND.ICHARN.LE.12)GOTO1020
      GOTO1029
 1020 CONTINUE
      CALL DRTL2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1029 CONTINUE
C
      IF(13.LE.ICHARN.AND.ICHARN.LE.18)GOTO1030
      GOTO1039
 1030 CONTINUE
      CALL DRTL3(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1039 CONTINUE
C
      IF(ICHARN.GE.19)GOTO1040
      GOTO1049
 1040 CONTINUE
      CALL DRTL4(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1049 CONTINUE
C
      IFOUND='NO'
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
 9011 FORMAT('***** AT THE END       OF DPRTL--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ICHAR2,ICHARN
 9013 FORMAT('ICHAR2,ICHARN = ',A4,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)ISTART,ISTOP,NC,NUMCO
 9014 FORMAT('ISTART,ISTOP,NC,NUMCO = ',4I8)
      CALL DPWRST('XXX','BUG ')
      IF(NUMCO.GE.1.AND.NUMCO.LE.1000)GOTO9019
      DO9015I=1,NUMCO
      WRITE(ICOUT,9016)I,IOP(I),X(I),Y(I)
 9016 FORMAT('I,IOP(I),X(I),Y(I) = ',I8,2X,A4,2F10.2)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9019 CONTINUE
      WRITE(ICOUT,9021)IXMINS,IXMAXS,IXDELS
 9021 FORMAT('IXMINS,IXMAXS,IXDELS = ',3I8)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPRTN(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN TRIPLEX NUMERIC.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--87/4
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MARCH     1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --MARCH     1987.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICHAR2
      CHARACTER*4 IOP
      CHARACTER*4 IBUGD2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION IOP(*)
      DIMENSION X(*)
      DIMENSION Y(*)
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
      NUMCO=1
      ISTART=1
      ISTOP=1
      NC=1
C
C               ******************************************
C               **  TREAT THE ROMAN SIMPLEX UPPER CASE  **
C               **  HERSHEY CHARACTER SET CASE          **
C               ******************************************
C
C
      IF(IBUGD2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPRTN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHAR2
   52 FORMAT('ICHAR2 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **************************************************
C               **  STEP 1--                                    **
C               **  SEARCH FOR THE INPUT CHARACTER(S).          **
C               **  MAP THE CHARACTER(S) INTO A NUMERIC VALUE.  **
C               **************************************************
C
      CALL DPCHNU(ICHAR2,ICHARN,IBUGD2,IFOUND)
      IF(IFOUND.EQ.'NO')GOTO9000
C
      IF(ICHARN.LE.6)GOTO1010
      GOTO1019
 1010 CONTINUE
      CALL DRTN1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1019 CONTINUE
C
      IF(ICHARN.GE.7)GOTO1020
      GOTO1029
 1020 CONTINUE
      CALL DRTN2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1029 CONTINUE
C
      IFOUND='NO'
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
 9011 FORMAT('***** AT THE END       OF DPRTN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ICHAR2,ICHARN
 9013 FORMAT('ICHAR2,ICHARN = ',A4,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)ISTART,ISTOP,NC,NUMCO
 9014 FORMAT('ISTART,ISTOP,NC,NUMCO = ',4I8)
      CALL DPWRST('XXX','BUG ')
      IF(NUMCO.GE.1.AND.NUMCO.LE.1000)GOTO9019
      DO9015I=1,NUMCO
      WRITE(ICOUT,9016)I,IOP(I),X(I),Y(I)
 9016 FORMAT('I,IOP(I),X(I),Y(I) = ',I8,2X,A4,2F10.2)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9019 CONTINUE
      WRITE(ICOUT,9021)IXMINS,IXMAXS,IXDELS
 9021 FORMAT('IXMINS,IXMAXS,IXDELS = ',3I8)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPRTS(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN TRIPLEX SYMBOLS.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--87/4
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MARCH     1981.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MARCH     1987.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICHAR2
      CHARACTER*4 IOP
      CHARACTER*4 IBUGD2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION IOP(*)
      DIMENSION X(*)
      DIMENSION Y(*)
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
      NUMCO=1
      ISTART=1
      ISTOP=1
      NC=1
C
C               ******************************************
C               **  TREAT THE ROMAN SIMPLEX UPPER CASE  **
C               **  HERSHEY CHARACTER SET CASE          **
C               ******************************************
C
C
      IF(IBUGD2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPRTS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHAR2
   52 FORMAT('ICHAR2 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **************************************************
C               **  STEP 1--                                    **
C               **  SEARCH FOR THE INPUT CHARACTER(S).          **
C               **  MAP THE CHARACTER(S) INTO A NUMERIC VALUE.  **
C               **************************************************
C
      CALL DPCHSY(ICHAR2,ICHARN,IBUGD2,IFOUND)
      IF(IFOUND.EQ.'NO')GOTO9000
C
      IF(ICHARN.LE.8)GOTO1010
      GOTO1019
 1010 CONTINUE
      CALL DRTS1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1019 CONTINUE
C
      IF(ICHARN.GE.9)GOTO1020
      GOTO1029
 1020 CONTINUE
      CALL DRTS2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1029 CONTINUE
C
      IFOUND='NO'
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
 9011 FORMAT('***** AT THE END       OF DPRTS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ICHAR2,ICHARN
 9013 FORMAT('ICHAR2,ICHARN = ',A4,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)ISTART,ISTOP,NC,NUMCO
 9014 FORMAT('ISTART,ISTOP,NC,NUMCO = ',4I8)
      CALL DPWRST('XXX','BUG ')
      IF(NUMCO.GE.1.AND.NUMCO.LE.1000)GOTO9019
      IF(NUMCO.GE.1.AND.NUMCO.LE.1000)GOTO9019
      DO9015I=1,NUMCO
      WRITE(ICOUT,9016)I,IOP(I),X(I),Y(I)
 9016 FORMAT('I,IOP(I),X(I),Y(I) = ',I8,2X,A4,2F10.2)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9019 CONTINUE
      WRITE(ICOUT,9021)IXMINS,IXMAXS,IXDELS
 9021 FORMAT('IXMINS,IXMAXS,IXDELS = ',3I8)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPRTU(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN TRIPLEX UPPER CASE.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--87/4
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MARCH     1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --MARCH     1987.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICHAR2
      CHARACTER*4 IOP
      CHARACTER*4 IBUGD2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION IOP(*)
      DIMENSION X(*)
      DIMENSION Y(*)
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
      NUMCO=1
      ISTART=1
      ISTOP=1
      NC=1
C
C               ******************************************
C               **  TREAT THE ROMAN SIMPLEX UPPER CASE  **
C               **  HERSHEY CHARACTER SET CASE          **
C               ******************************************
C
C
      IF(IBUGD2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPRTU--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHAR2
   52 FORMAT('ICHAR2 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **************************************************
C               **  STEP 1--                                    **
C               **  SEARCH FOR THE INPUT CHARACTER(S).          **
C               **  MAP THE CHARACTER(S) INTO A NUMERIC VALUE.  **
C               **************************************************
C
      CALL DPCHAL(ICHAR2,ICHARN,IBUGD2,IFOUND)
      IF(IFOUND.EQ.'NO')GOTO9000
C
      IF(ICHARN.LE.6)GOTO1010
      GOTO1019
 1010 CONTINUE
      CALL DRTU1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1019 CONTINUE
C
      IF(7.LE.ICHARN.AND.ICHARN.LE.13)GOTO1020
      GOTO1029
 1020 CONTINUE
      CALL DRTU2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1029 CONTINUE
C
      IF(14.LE.ICHARN.AND.ICHARN.LE.19)GOTO1030
      GOTO1039
 1030 CONTINUE
      CALL DRTU3(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1039 CONTINUE
C
      IF(ICHARN.GE.20)GOTO1040
      GOTO1049
 1040 CONTINUE
      CALL DRTU4(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1049 CONTINUE
C
      IFOUND='NO'
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
 9011 FORMAT('***** AT THE END       OF DPRTU--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ICHAR2,ICHARN
 9013 FORMAT('ICHAR2,ICHARN = ',A4,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)ISTART,ISTOP,NC,NUMCO
 9014 FORMAT('ISTART,ISTOP,NC,NUMCO = ',4I8)
      CALL DPWRST('XXX','BUG ')
      IF(NUMCO.GE.1.AND.NUMCO.LE.1000)GOTO9019
      DO9015I=1,NUMCO
      WRITE(ICOUT,9016)I,IOP(I),X(I),Y(I)
 9016 FORMAT('I,IOP(I),X(I),Y(I) = ',I8,2X,A4,2F10.2)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9019 CONTINUE
      WRITE(ICOUT,9021)IXMINS,IXMAXS,IXDELS
 9021 FORMAT('IXMINS,IXMAXS,IXDELS = ',3I8)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPRUH1(P1,N1,P2,N2,P3,N3,ALPHA,ICASAN,IWRITE,
     1                  PVALUE,ALOWLM,AUPPLM,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--FOR THREE BINOMIAL PROPORTIONS (P1, N1, P2, N2, P3, N3)
C              AND ALPHA, COMPUTE THE HYPOTHESIS TEST FOR:
C
C                 Ho: P1 = P2*P3
C
C              AGAINST
C
C                 Ha: P1 <> P1*P2
C                 Ha: P1 <  P1*P2
C                 Ha: P1  > P1*P2
C
C              RETURN THE APPROPRIATE P-VALUE.
C     REFERENCE--PRIVATE COMMUNICATION WITH ANDREW RUHKIN OF THE
C                NIST STATISTICAL ENGINEERING DIVISION.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2008/9
C     ORIGINAL VERSION--SEPTEMBER 2008.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASAN
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      REAL P1
      REAL P2
      REAL P3
      REAL ALPHA
      REAL PVALUE
      REAL ALOWLM
      REAL AUPPLM
      INTEGER N1
      INTEGER N2
      INTEGER N3
C
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION DTERM4
      DOUBLE PRECISION DP1
      DOUBLE PRECISION DP2
      DOUBLE PRECISION DP3
      DOUBLE PRECISION DN1
      DOUBLE PRECISION DN2
      DOUBLE PRECISION DN3
      DOUBLE PRECISION DPVAL
      DOUBLE PRECISION DPPF
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPRU'
      ISUBN2='H1  '
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RUH1')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPRUH1--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ICASAN,IWRITE
   52   FORMAT('IBUGA3,ICASAN,IWRITE = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)P1,N1,P2,N2,P3,N3,ALPHA
   53   FORMAT('P1,N1,P2,N2,P3,N3,ALPHA = ',3(G15.7,I8),G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ********************************
C               **  STEP 1--                  **
C               **  CHECK FOR INPUT ERRORS    **
C               ********************************
C
      PVALUE=0.0
      ALOWLM=0.0
      AUPPLM=1.0
C
      IF(N1.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,111)
  111   FORMAT('****** ERROR IN RUHKIN 1 TEST-- ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('         THE NUMBER OF OBSERVATIONS FOR THE FIRST ',
     1         'RESPONSE VARIABLE IS LESS THAN 2.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,114)N1
  114   FORMAT('SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(N2.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,123)
  123   FORMAT('         THE NUMBER OF OBSERVATIONS FOR THE ',
     1         'SECOND RESPONSE VARIABLE IS LESS THAN 2.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,114)N2
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(N3.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,133)
  133   FORMAT('         THE NUMBER OF OBSERVATIONS FOR THE ',
     1         'THIRD RESPONSE VARIABLE IS LESS THAN 2.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,114)N3
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(P1.LT.0.0 .OR. P1.GT.1.0)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,162)
  162   FORMAT('      THE BINOMIAL PROBABILITY OF SUCCESS PARAMETER ',
     1         'FOR THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,164)
  164   FORMAT('      FIRST RESPONSE VARIABLE IS OUTSIDE THE ',
     1         '(0,1) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,167)P1
  167   FORMAT('      THE PROBABILITY OF SUCCESS PARAMETER = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
      IF(P2.LT.0.0 .OR. P2.GT.1.0)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,162)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,174)
  174   FORMAT('      SECOND RESPONSE VARIABLE IS OUTSIDE THE ',
     1         '(0,1) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,167)P2
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
      IF(P3.LT.0.0 .OR. P3.GT.1.0)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,162)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,184)
  184   FORMAT('      THIRD RESPONSE VARIABLE IS OUTSIDE THE ',
     1         '(0,1) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,167)P3
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
      ALPHSV=ALPHA
      IF(ALPHA.GT.1.0 .AND. ALPHA.LE.100.0)ALPHA=ALPHA/100.0
      IF(ALPHA.LE.0.0 .OR. ALPHA.GE.1.0)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,192)
  192   FORMAT('      THE VALUE OF ALPHA IS OUTSIDE THE (0,1) ',
     1         'INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,197)ALPHA
  197   FORMAT('      THE VALUE OF ALPHA = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
CCCCC FOR THESE FORMULAS, WE WANT ALPHA AS 0.05 RATHER THAN
CCCCC 0.95.
C
CCCCC IF(ALPHA.LT.0.5)ALPHA=1.0 - ALPHA
      IF(ALPHA.GT.0.5)ALPHA=1.0 - ALPHA
C
C               ********************************************
C               **  STEP 2--                              **
C               **  COMPUTE THE DIFFERENCE OF PROPORTIONS **
C               **  CONFIDENCE INTERVAL.                  **
C               ********************************************
C
      STATVA=P1 - P2*P3
      IF(P1.GE.1.0 .AND. P2.GE.1.0 .AND. P3.GE.1.0)THEN
        PVALUE=1.0
        ALOWLM=STATVA
        AUPPLM=STATVA
        GOTO9000
      ELSEIF(P1.LE.0.0 .AND. P2.LE.0.0 .AND. P3.LE.0.0)THEN
        PVALUE=1.0
        ALOWLM=STATVA
        AUPPLM=STATVA
        GOTO9000
      ENDIF
C
      DN1=DBLE(N1)
      DN2=DBLE(N2)
      DN3=DBLE(N3)
      DP1=DBLE(P1)
      DP2=DBLE(P2)
      DP3=DBLE(P3)
C
      DTERM1=DP1*(1.0D0 - DP1)/DN1
      DTERM2=(DP3**2)*DP2*(1.0D0 - DP2)/DN2
      DTERM3=(DP2**2)*DP3*(1.0D0 - DP3)/DN3
      IF(ICASAN.EQ.'R1LT')THEN
        DTERM4=(DP2*DP3 - DP1)/DSQRT(DTERM1 + DTERM2 + DTERM3)
        CALL NODCDF(DTERM4,DPVAL)
        DPVAL=1.0D0 - DPVAL
      ELSEIF(ICASAN.EQ.'R1UT')THEN
        DTERM4=(DP1 - DP2*DP3)/DSQRT(DTERM1 + DTERM2 + DTERM3)
        CALL NODCDF(DTERM4,DPVAL)
        DPVAL=1.0D0 - DPVAL
      ELSE
        DTERM4=DABS(DP1 - DP2*DP3)/DSQRT(DTERM1 + DTERM2 + DTERM3)
        CALL NODCDF(DTERM4,DPVAL)
        DPVAL=2.0D0*(1.0D0 - DPVAL)
      ENDIF
      PVALUE=REAL(DPVAL)
C
      DTERM4=DSQRT(DTERM1 + DTERM2 + DTERM3)
      ALP2=ALPHA/2.0
      IF(ALP2.LE.0.5)ALP2=1.0 - ALP2
      CALL NODPPF(DBLE(ALP2),DPPF)
      A1=STATVA - REAL(DPPF*DTERM4)
      A2=STATVA + REAL(DPPF*DTERM4)
      ALOWLM=MIN(A1,A2)
      AUPPLM=MAX(A1,A2)
      IF(ALOWLM.LT.-1.0)ALOWLM=-1.0
      IF(AUPPLM.GT.1.0)AUPPLM=1.0
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RUH1')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPRUH1--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR
 9012   FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)STATVA,ALP2,DPPF
 9013   FORMAT('STATVA,ALP2,DPPF = ',3(G15.7,2X))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)A1,A2,ALOWLM,AUPPLM
 9014   FORMAT('A1,A2,ALOWLM,AUPPLM = ',4(G15.7,2X))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9018)DTERM1,DTERM2,DTERM3,DTERM4
 9018   FORMAT('DTERM1,DTERM2,DTERM3,DTERM4 = ',4(G15.7,2X))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9019)DPVAL,PVALUE
 9019   FORMAT('DPVAL,PVALUE = ',2(G15.7,2X))
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPRUH2(P1,N1,P2,N2,ALPHA,ICASAN,IWRITE,
     1                  PVALUE,ALOWLM,AUPPLM,
     1                  IBUGA3,IERROR)
C
C     PURPOSE--FOR TWO BINOMIAL PROPORTIONS (P1, N1, P2, N2)
C              AND ALPHA, COMPUTE THE HYPOTHESIS TEST FOR:
C
C                 Ho: P1 = 0.5*P2
C
C              AGAINST
C
C                 Ha: P1 <> 0.5*P2
C                 Ha: P1 <  0.5*P2
C                 Ha: P1  > 0.5*P2
C
C              RETURN THE APPROPRIATE P-VALUE AND A CONFIDENCE
C              INTERVAL.
C     REFERENCE--PRIVATE COMMUNICATION WITH ANDREW RUHKIN OF THE
C                NIST STATISTICAL ENGINEERING DIVISION.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2008/9
C     ORIGINAL VERSION--SEPTEMBER 2008.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASAN
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      REAL P1
      REAL P2
      REAL ALPHA
      REAL PVALUE
      INTEGER N1
      INTEGER N2
C
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION DTERM4
      DOUBLE PRECISION DP1
      DOUBLE PRECISION DP2
      DOUBLE PRECISION DN1
      DOUBLE PRECISION DN2
      DOUBLE PRECISION DPVAL
      DOUBLE PRECISION DPPF
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPRU'
      ISUBN2='H2  '
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPRUH2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ICASAN,IWRITE
   52   FORMAT('IBUGA3,ICASAN,IWRITE = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)P1,N1,P2,N2,ALPHA
   53   FORMAT('P1,N1,P2,N2,ALPHA = ',2(G15.7,I8),G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ********************************
C               **  STEP 1--                  **
C               **  CHECK FOR INPUT ERRORS    **
C               ********************************
C
      PVALUE=0.0
      ALOWLM=0.0
      AUPPLM=1.0
C
      IF(N1.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,111)
  111   FORMAT('****** ERROR IN RUHKIN 2 TEST-- ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('         THE NUMBER OF OBSERVATIONS FOR THE FIRST ',
     1         'RESPONSE VARIABLE IS LESS THAN 2.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,114)N1
  114   FORMAT('SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(N2.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,123)
  123   FORMAT('         THE NUMBER OF OBSERVATIONS FOR THE ',
     1         'SECOND RESPONSE VARIABLE IS LESS THAN 2.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,114)N2
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(P1.LT.0.0 .OR. P1.GT.1.0)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,162)
  162   FORMAT('      THE BINOMIAL PROBABILITY OF SUCCESS PARAMETER ',
     1         'FOR THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,164)
  164   FORMAT('      FIRST RESPONSE VARIABLE IS OUTSIDE THE ',
     1         '(0,1) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,167)P1
  167   FORMAT('      THE PROBABILITY OF SUCCESS PARAMETER = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
      IF(P2.LT.0.0 .OR. P2.GT.1.0)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,162)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,174)
  174   FORMAT('      SECOND RESPONSE VARIABLE IS OUTSIDE THE ',
     1         '(0,1) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,167)P2
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
      ALPHSV=ALPHA
      IF(ALPHA.GT.1.0 .AND. ALPHA.LE.100.0)ALPHA=ALPHA/100.0
      IF(ALPHA.LE.0.0 .OR. ALPHA.GE.1.0)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,192)
  192   FORMAT('      THE VALUE OF ALPHA IS OUTSIDE THE (0,1) ',
     1         'INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,197)ALPHA
  197   FORMAT('      THE VALUE OF ALPHA = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
CCCCC FOR THESE FORMULAS, WE WANT ALPHA AS 0.05 RATHER THAN
CCCCC 0.95.
C
CCCCC IF(ALPHA.LT.0.5)ALPHA=1.0 - ALPHA
      IF(ALPHA.GT.0.5)ALPHA=1.0 - ALPHA
C
C               ********************************************
C               **  STEP 2--                              **
C               **  COMPUTE THE DIFFERENCE OF PROPORTIONS **
C               **  CONFIDENCE INTERVAL.                  **
C               ********************************************
C
      STATVA=P1 - 0.5*P2
      IF(P1.GE.1.0 .AND. P2.GE.1.0)THEN
        PVALUE=1.0
        ALOWLM=STATVA
        AUPPLM=STATVA
        GOTO9000
      ELSEIF(P1.LE.0.0 .AND. P2.LE.0.0)THEN
        PVALUE=1.0
        ALOWLM=STATVA
        AUPPLM=STATVA
        GOTO9000
      ENDIF
C
      DN1=DBLE(N1)
      DN2=DBLE(N2)
      DP1=DBLE(P1)
      DP2=DBLE(P2)
C
      DTERM1=DP1*(1.0D0 - DP1)/DN1
      DTERM2=DP2*(1.0D0 - DP2)/(4.0D0*DN2)
      IF(ICASAN.EQ.'R2LT')THEN
        DTERM3=0.5D0*DP2 - DP1
        DTERM4=DTERM3/DSQRT(DTERM1 + DTERM2)
        CALL NODCDF(DTERM4,DPVAL)
        DPVAL=1.0D0 - DPVAL
      ELSEIF(ICASAN.EQ.'R2UT')THEN
        DTERM3=DP1 - 0.5D0*DP2
        DTERM4=DTERM3/DSQRT(DTERM1 + DTERM2)
        CALL NODCDF(DTERM4,DPVAL)
        DPVAL=1.0D0 - DPVAL
      ELSE
        DTERM3=DABS(DP1 - 0.5D0*DP2)
        DTERM4=DTERM3/DSQRT(DTERM1 + DTERM2)
        CALL NODCDF(DTERM4,DPVAL)
        DPVAL=2.0D0*(1.0D0 - DPVAL)
      ENDIF
      PVALUE=REAL(DPVAL)
C
      DTERM4=DSQRT(DTERM1 + DTERM2)
      ALP2=ALPHA/2.0
      IF(ALP2.LE.0.5)ALP2=1.0 - ALP2
      CALL NODPPF(DBLE(ALP2),DPPF)
      A1=STATVA - REAL(DPPF*DTERM4)
      A2=STATVA + REAL(DPPF*DTERM4)
      ALOWLM=MIN(A1,A2)
      AUPPLM=MAX(A1,A2)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPRUH2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR
 9012   FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)DTERM1,DTERM2,DTERM3,DTERM4
 9013   FORMAT('DTERM1,DTERM2,DTERM3,DTERM4 = ',4(G15.7,2X))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)DPVAL,PVALUE
 9014   FORMAT('DPVAL,PVALUE = ',2(G15.7,2X))
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPRUH3(P1,N1,P2,N2,P3,N3,P4,N4,ALPHA,ICASAN,IWRITE,
     1                  PVALUE,ALOWLM,AUPPLM,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--FOR THREE BINOMIAL PROPORTIONS (P1, N1, P2, N2, P3, N3)
C              AND ALPHA, COMPUTE THE HYPOTHESIS TEST FOR:
C
C                 Ho: P1*P2 = P3*P4
C
C              AGAINST
C
C                 Ha: P1*P2 <> P3*P4
C                 Ha: P1*P2 <  P3*P4
C                 Ha: P1*P2  > P3*P4
C
C              RETURN THE APPROPRIATE P-VALUE.
C     REFERENCE--PRIVATE COMMUNICATION WITH ANDREW RUHKIN AND
C                BILL STRAWDERMAN OF THE NIST STATISTICAL ENGINEERING
C                DIVISION.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2010/6
C     ORIGINAL VERSION--JUNE      2010.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASAN
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      REAL P1
      REAL P2
      REAL P3
      REAL P4
      REAL ALPHA
      REAL PVALUE
      REAL ALOWLM
      REAL AUPPLM
      INTEGER N1
      INTEGER N2
      INTEGER N3
      INTEGER N4
C
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION DTERM4
      DOUBLE PRECISION DTERM5
      DOUBLE PRECISION DDELTA
      DOUBLE PRECISION DP1
      DOUBLE PRECISION DP2
      DOUBLE PRECISION DP3
      DOUBLE PRECISION DP4
      DOUBLE PRECISION DN1
      DOUBLE PRECISION DN2
      DOUBLE PRECISION DN3
      DOUBLE PRECISION DN4
      DOUBLE PRECISION DPVAL
      DOUBLE PRECISION DPPF
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPRU'
      ISUBN2='H3  '
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RUH3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPRUH3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ICASAN,IWRITE
   52   FORMAT('IBUGA3,ICASAN,IWRITE = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)P1,N1,P2,N2,P3,N3,P4,N4,ALPHA
   53   FORMAT('P1,N1,P2,N2,P3,N3,P4,N4,ALPHA = ',4(G15.7,I8),G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ********************************
C               **  STEP 1--                  **
C               **  CHECK FOR INPUT ERRORS    **
C               ********************************
C
      PVALUE=0.0
      ALOWLM=0.0
      AUPPLM=1.0
C
      IF(N1.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,111)
  111   FORMAT('****** ERROR IN RUHKIN 3 TEST-- ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('         THE NUMBER OF OBSERVATIONS FOR THE FIRST ',
     1         'RESPONSE VARIABLE IS LESS THAN 2.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,114)N1
  114   FORMAT('SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(N2.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,123)
  123   FORMAT('         THE NUMBER OF OBSERVATIONS FOR THE ',
     1         'SECOND RESPONSE VARIABLE IS LESS THAN 2.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,114)N2
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(N3.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,133)
  133   FORMAT('         THE NUMBER OF OBSERVATIONS FOR THE ',
     1         'THIRD RESPONSE VARIABLE IS LESS THAN 2.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,114)N3
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(N4.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,143)
  143   FORMAT('         THE NUMBER OF OBSERVATIONS FOR THE ',
     1         'FOURTH RESPONSE VARIABLE IS LESS THAN 2.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,114)N4
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(P1.LT.0.0 .OR. P1.GT.1.0)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,162)
  162   FORMAT('      THE BINOMIAL PROBABILITY OF SUCCESS PARAMETER ',
     1         'FOR THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,164)
  164   FORMAT('      FIRST RESPONSE VARIABLE IS OUTSIDE THE ',
     1         '(0,1) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,167)P1
  167   FORMAT('      THE PROBABILITY OF SUCCESS PARAMETER = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
      IF(P2.LT.0.0 .OR. P2.GT.1.0)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,162)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,174)
  174   FORMAT('      SECOND RESPONSE VARIABLE IS OUTSIDE THE ',
     1         '(0,1) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,167)P2
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
      IF(P3.LT.0.0 .OR. P3.GT.1.0)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,162)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,184)
  184   FORMAT('      THIRD RESPONSE VARIABLE IS OUTSIDE THE ',
     1         '(0,1) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,167)P3
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
      IF(P4.LT.0.0 .OR. P4.GT.1.0)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,162)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,194)
  194   FORMAT('      FOURTH RESPONSE VARIABLE IS OUTSIDE THE ',
     1         '(0,1) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,167)P4
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
      ALPHSV=ALPHA
      IF(ALPHA.GT.1.0 .AND. ALPHA.LE.100.0)ALPHA=ALPHA/100.0
      IF(ALPHA.LE.0.0 .OR. ALPHA.GE.1.0)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,192)
  192   FORMAT('      THE VALUE OF ALPHA IS OUTSIDE THE (0,1) ',
     1         'INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,197)ALPHA
  197   FORMAT('      THE VALUE OF ALPHA = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
CCCCC FOR THESE FORMULAS, WE WANT ALPHA AS 0.05 RATHER THAN
CCCCC 0.95.
C
CCCCC IF(ALPHA.LT.0.5)ALPHA=1.0 - ALPHA
      IF(ALPHA.GT.0.5)ALPHA=1.0 - ALPHA
C
C               ********************************************
C               **  STEP 2--                              **
C               **  COMPUTE THE DIFFERENCE OF PROPORTIONS **
C               **  CONFIDENCE INTERVAL.                  **
C               ********************************************
C
C     DEFINE CORRECTION TERM:
C
C        P(i) = (X(i) + 0.5)/(N(i) + 1)
C
      X1=P1*REAL(N1)
      IX1=INT(X1+0.01)
      X1=REAL(IX1)
      P1=(X1+0.5)/(REAL(N1)+1.0)
      X2=P2*REAL(N2)
      IX2=INT(X2+0.01)
      X2=REAL(IX2)
      P2=(X2+0.5)/(REAL(N2)+1.0)
      X3=P3*REAL(N3)
      IX3=INT(X3+0.01)
      X3=REAL(IX3)
      P3=(X3+0.5)/(REAL(N3)+1.0)
      X4=P4*REAL(N4)
      IX4=INT(X4+0.01)
      X4=REAL(IX4)
      P4=(X4+0.5)/(REAL(N4)+1.0)
C
      STATVA=P1*P2 - P3*P4
C
      IF(P1.GE.1.0 .AND. P2.GE.1.0 .AND. P3.GE.1.0 .AND. P4.GE.1.0)THEN
        PVALUE=1.0
        ALOWLM=STATVA
        AUPPLM=STATVA
        GOTO9000
      ELSEIF(P1.LE.0.0 .AND. P2.LE.0.0 .AND. P3.LE.0.0 .AND.
     1       P4.LE.0.0)THEN
        PVALUE=1.0
        ALOWLM=STATVA
        AUPPLM=STATVA
        GOTO9000
      ENDIF
C
      DN1=DBLE(N1)
      DN2=DBLE(N2)
      DN3=DBLE(N3)
      DN4=DBLE(N4)
      DP1=DBLE(P1)
      DP2=DBLE(P2)
      DP3=DBLE(P3)
      DP4=DBLE(P4)
C
      DTERM1=(DP2**2)*DP1*(1.0D0 - DP1)/DN1
      DTERM2=(DP1**2)*DP2*(1.0D0 - DP2)/DN2
      DTERM3=(DP4**2)*DP3*(1.0D0 - DP3)/DN3
      DTERM4=(DP3**2)*DP4*(1.0D0 - DP4)/DN4
      DDELTA=DSQRT(DTERM1 + DTERM2 + DTERM3 + DTERM4)
C
      IF(ICASAN.EQ.'R3LT')THEN
        DTERM5=(DP3*DP4 - DP1*DP2)/DDELTA
        CALL NODCDF(DTERM5,DPVAL)
        DPVAL=1.0D0 - DPVAL
      ELSEIF(ICASAN.EQ.'R3UT')THEN
        DTERM5=(DP1*DP2 - DP3*DP4)/DDELTA
        CALL NODCDF(DTERM5,DPVAL)
        DPVAL=1.0D0 - DPVAL
      ELSE
        DTERM5=DABS(DP3*DP4 - DP1*DP2)/DDELTA
        CALL NODCDF(DTERM5,DPVAL)
        DPVAL=2.0D0*(1.0D0 - DPVAL)
      ENDIF
      PVALUE=REAL(DPVAL)
C
      ALP2=ALPHA/2.0
      IF(ALP2.LE.0.5)ALP2=1.0 - ALP2
      CALL NODPPF(DBLE(ALP2),DPPF)
      A1=STATVA - REAL(DPPF*DDELTA)
      A2=STATVA + REAL(DPPF*DDELTA)
      ALOWLM=MIN(A1,A2)
      AUPPLM=MAX(A1,A2)
      IF(ALOWLM.LT.-1.0)ALOWLM=-1.0
      IF(AUPPLM.GT.1.0)AUPPLM=1.0
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RUH3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPRUH3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR
 9012   FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)STATVA,ALP2,DPPF
 9013   FORMAT('STATVA,ALP2,DPPF = ',3(G15.7,2X))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)A1,A2,ALOWLM,AUPPLM
 9014   FORMAT('A1,A2,ALOWLM,AUPPLM = ',4(G15.7,2X))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9018)DTERM1,DTERM2,DTERM3,DTERM4,DTERM5
 9018   FORMAT('DTERM1,DTERM2,DTERM3,DTERM4,DTERM5 = ',5(G15.7,2X))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9019)DPVAL,PVALUE
 9019   FORMAT('DPVAL,PVALUE = ',2(G15.7,2X))
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPRUN(XTEMP1,MAXNXT,ICASAN,ICAPSW,IFORSW,
     1                  IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--CARRY OUT A RUNS ANALYSIS TO TEST FOR RANDOMNESS.
C     EXAMPLE--RUNS Y
C              RUNS Y1 TO Y5
C     REFERENCES--LEVENE AND WOLFOWITZ, ANNALS OF MATHEMATICAL
C                 STATISTICS, 1944, PAGES 58-69;
C                 ESPECIALLY PAGES 60, 63, AND 64.
C               --BRADLEY, DISTRIBUTION-FREE STATISTICAL TESTS,
C                 1968, CHAPTER 12, PAGES 271-282.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--JULY      1984.
C     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE COMMON
C     UPDATED         --MAY       2011. USE DPPARS ROUTINE
C     UPATED          --MAY       2011. REWRITTEN TO HANDLE MULTIPLE
C                                       RESPONSE VARIABLES, GROUP-ID
C                                       VARIABLES, OR A LAB-ID VARIABLE
C     UPATED          --JULY      2019. TWEAK SCRATCH SPACE
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
C
      INCLUDE 'DPCOZZ.INC'
C
      EQUIVALENCE (GARBAG(IGARB1),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB2),XIDTEM(1))
      EQUIVALENCE (GARBAG(IGARB3),XIDTE2(1))
      EQUIVALENCE (GARBAG(IGARB4),XIDTE3(1))
      EQUIVALENCE (GARBAG(IGARB5),XIDTE4(1))
      EQUIVALENCE (GARBAG(IGARB6),XIDTE5(1))
      EQUIVALENCE (GARBAG(IGARB7),XIDTE6(1))
      EQUIVALENCE (GARBAG(IGARB8),W(1))
      EQUIVALENCE (GARBAG(IGARB9),TEMP2(1))
      EQUIVALENCE (GARBAG(IGAR10),XDESGN(1,1))
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
      ICASAN='RUNS'
      IREPL='OFF'
      IMULT='OFF'
      ISUBN1='DPRU'
      ISUBN2='N   '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
C               ***********************************************
C               **  TREAT THE RUNS             TEST  CASE    **
C               ***********************************************
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'PRUN')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPRUN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGA2,IBUGA3,IBUGQ,ISUBRO,MAXNXT
   53   FORMAT('IBUGA2,IBUGA3,IBUGQ,ISUBRO,MAXNXT = ',4(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************************************************
C               **  STEP 1--                                       **
C               **  EXTRACT THE COMMAND                            **
C               **  LOOK FOR ONE OF THE FOLLOWING COMMANDS:        **
C               **    1) RUNS              Y                       **
C               **    2) MULTIPLE RUNS     Y1 ... YK               **
C               **    3) REPLICATED RUNS   Y X1 ... XK             **
C               *****************************************************
C
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PRUN')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ILASTZ=9999
C
C     LOOK FOR:
C
C          RUNS 
C          RUNS TEST
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
        ELSEIF(ICTMP1.EQ.'RUNS' .AND. ICTMP2.EQ.'TEST')THEN
          IFOUND='YES'
          ILASTZ=I+1
        ELSEIF(ICTMP1.EQ.'RUNS')THEN
          IFOUND='YES'
          ILASTZ=I
        ELSEIF(ICTMP1.EQ.'REPL')THEN
          IREPL='ON'
          ILASTZ=MAX(ILASTZ,I)
        ELSEIF(ICTMP1.EQ.'MULT')THEN
          IMULT='ON'
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PRUN')THEN
        WRITE(ICOUT,91)ICASAN,IMULT,IREPL,ISHIFT
   91   FORMAT('DPRUN: ICASAN,IMULT,IREPL,ISHIFT = ',3(A4,2X),I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IMULT.EQ.'ON')THEN
        IF(IREPL.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
  101     FORMAT('***** ERROR IN RUNS TEST--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,103)
  103     FORMAT('      YOU CANNOT SPECIFY BOTH "MULTIPLE" AND ',
     1           '"REPLICATION"')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,104)
  104     FORMAT('      FOR THE RUNS COMMAND.')
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PRUN')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='RUNS'
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PRUN')THEN
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PRUN')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO501I=1,MAXNXT
        W(I)=1.0
  501 CONTINUE
      NRESP=0
      NREPL=0
      IF(IMULT.EQ.'ON')THEN
        NRESP=NUMVAR
      ELSEIF(IREPL.EQ.'ON')THEN
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PRUN')THEN
        WRITE(ICOUT,521)NRESP,NREPL
  521   FORMAT('NRESP,NREPL = ',2I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ******************************************************
C               **  STEP 6--                                        **
C               **  GENERATE THE RUNS             TEST FOR THE      **
C               **  VARIOUS CASES                                   **
C               ******************************************************
C
      ISTEPN='6'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PRUN')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               ******************************************
C               **  STEP 8A--                           **
C               **  CASE 1: NO REPLICATION VARIABLES    **
C               ******************************************
C
      IF(NREPL.LT.1)THEN
        ISTEPN='8A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PRUN')
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
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PRUN')THEN
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
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'PRUN')THEN
            ISTEPN='8B'
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,822)
  822       FORMAT('***** FROM THE MIDDLE  OF DPRUN--')
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
          CALL DPRUN2(Y,W,NS1,XTEMP1,MAXNXT,
     1                ICAPSW,ICAPTY,IFORSW,ICASAN,
     1                PID,IVARID,IVARI2,NREPL,
     1                ISUBRO,IBUGA3,IERROR)
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
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PRUN')
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
C       **  CALL DPRUN2 TO PERFORM RUNS             TEST.  **
C       *****************************************************
C
C
        IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'PRUN')THEN
          ISTEPN='9C'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,941)
  941     FORMAT('***** FROM THE MIDDLE  OF DPRUN--')
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
              CALL DPRUN2(TEMP1,W,NTEMP,XTEMP1,MAXNXT,
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
              CALL DPRUN2(TEMP1,W,NTEMP,XTEMP1,MAXNXT,
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
            IF(NTEMP.GT.0)THEN
              CALL DPRUN2(TEMP1,W,NTEMP,XTEMP1,MAXNXT,
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
            IF(NTEMP.GT.0)THEN
              CALL DPRUN2(TEMP1,W,NTEMP,XTEMP1,MAXNXT,
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
            IF(NTEMP.GT.0)THEN
              CALL DPRUN2(TEMP1,W,NTEMP,XTEMP1,MAXNXT,
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
            IF(NTEMP.GT.0)THEN
              CALL DPRUN2(TEMP1,W,NTEMP,XTEMP1,MAXNXT,
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
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'PRUN')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPRUN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR,ICASAN
 9012   FORMAT('IFOUND,IERROR,ICASAN = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPRUN2(Y,W,N,XTEMP1,MAXNXT,
     1                  ICAPSW,ICAPTY,IFORSW,ICASAN,
     1                  PID,IVARID,IVARI2,NREPL,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS ROUTINE CARRIES OUT A RUNS ANALYSIS
C              FOR THE DATA IN THE INPUT VECTOR Y.
C     NOTE--ASSUMPTION--DATA COLLECTED SEQUENTIALLY IN TIME.
C     INPUT  ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR
C                                OF EQUALLY-SPACED OBSERVATIONS
C                                TO BE SMOOTHED.
C                       N      = THE INTEGER NUMBER OF
C                                OBSERVATIONS IN THE VECTOR Y.
C     NOTE--THE ANALYSIS CONSISTS OF FIRST DETERMINING
C           THE OBSERVED NUMBER OF RUNS FROM THE DATA,
C           AND THEN COMPUTING
C           THE EXPECTED NUMBER OF RUNS,
C           THE STANDARD DEVIATION OF THE NUMBER OF RUNS,
C           AND THE RESULTING STANDARDIZED STATISTIC
C           FOR THE NUMBER OF RUNS FOR RUNS OF VARIOUS
C           LENGTHS.
C           THIS IS DONE FOR RUNS UP, RUNS DOWN, AND
C           RUNS UP AND DOWN.
C           THIS RUNS ANSLYSIS IS A USEFUL DISTRIBUTION-FREE
C           TEST OF THE RANDOMNESS OF A DATA SET.
C     OUTPUT--4 PAGES OF AUTOMATIC PRINTOUT
C             CONSISTING OF THE OBSERVED NUMBER,
C             EXPECTED NUMBER, STANDARD DEVIATION
C             AND RESULTING STANDARDIZED STATISTIC
C             FOR RUNS OF VARIOUS LENGTHS.
C             AND THE CUMULATIVE FREQUENCY.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI 77 FORTRAN.
C     REFERENCES--LEVENE AND WOLFOWITZ, ANNALS OF MATHEMATICAL
C                 STATISTICS, 1944, PAGES 58-69;
C                 ESPECIALLY PAGES 60, 63, AND 64.
C     REFERENCES--BRADLEY, DISTRIBUTION-FREE STATISTICAL TESTS,
C                 1968, CHAPTER 12, PAGES 271-282.
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
C     ORIGINAL VERSION--JULY      1984.
C     UPDATED         --MAY       2011. USE DPDTA1 AND DPDTA2 TO PRINT
C                                       TABLES
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
      CHARACTER*4 ISUBRO
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
      DIMENSION W(*)
      DIMENSION XTEMP1(*)
      DIMENSION PID(*)
C
      DIMENSION NRUL(16), NRDL(16), NRTL(16), NRULG(16), NRDLG(16)
      DIMENSION NRTLG(16)
      DIMENSION ENRUL(16),ENRTL(16),ENRULG(16),ENRTLG(16)
      DIMENSION SNRUL(16),SNRTL(16),SNRULG(16),SNRTLG(16)
      DIMENSION ZNRUL(16),ZNRDL(16),ZNRTL(16),ZNRULG(16),ZNRDLG(16)
      DIMENSION ZNRTLG(16)
      DIMENSION C1(15),C2(15),C3(15),C4(15)
      DIMENSION ANRUL(16),ANRDL(16),ANRTL(16)
      DIMENSION ANRULG(16),ANRDLG(16),ANRTLG(16)
C
      PARAMETER (MAXROW=20)
      CHARACTER*60 ITITLE
      CHARACTER*60 ITITLZ
      CHARACTER*60 ITITL9
      CHARACTER*40 ITEXT(MAXROW)
      CHARACTER*4  ALIGN(MAXROW)
      CHARACTER*4  VALIGN(MAXROW)
      REAL         AVALUE(MAXROW)
      INTEGER      NCTEXT(MAXROW)
      INTEGER      IDIGIT(MAXROW)
      INTEGER      NTOT(MAXROW)
      LOGICAL      IFRST
      LOGICAL      ILAST
C
      PARAMETER(NUMCLI=6)
      PARAMETER(MAXLIN=1)
      CHARACTER*60 ITITL2(MAXLIN,NUMCLI)
      INTEGER      NCTIT2(MAXLIN,NUMCLI)
      INTEGER      IWHTML(NUMCLI)
      INTEGER      IWRTF(NUMCLI)
      REAL         AMAT(MAXROW,NUMCLI)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C------DATA STATEMENTS------------------------------------------------
C
      DATA C1(1),C1(2),C1(3),C1(4),C1(5),C1(6),C1(7),C1(8),C1(9),C1(10),
     1C1(11),C1(12),C1(13),C1(14),C1(15)
     1/ .4236111111E+00,  .1126675485E+00,  .4191688713E-01,
     1  .1076912487E-01,  .2003959238E-02,  .3023235799E-03,
     1  .3911555473E-04,  .4459038843E-05,  .4551105210E-06,
     1  .4207466837E-07,  .3555930927E-08,  .2768273257E-09,
     1  .1997821524E-10,  .1343876568E-11,  .8465610177E-13/
      DATA C2(1),C2(2),C2(3),C2(4),C2(5),C2(6),C2(7),C2(8),C2(9),C2(10),
     1C2(11),C2(12),C2(13),C2(14),C2(15)
     1/-.4819444444E+00, -.1628284832E+00, -.9690696649E-01,
     1 -.3778106786E-01, -.9289228716E-02, -.1724429252E-02,
     1 -.2638557888E-03, -.3466965096E-04, -.4004129153E-05,
     1 -.4130382587E-06, -.3851876069E-07, -.3279103786E-08,
     1 -.2568491117E-09, -.1863433868E-10, -.1259220466E-11/
      DATA C3(1),C3(2),C3(3),C3(4),C3(5),C3(6),C3(7),C3(8),C3(9),C3(10),
     1C3(11),C3(12),C3(13),C3(14),C3(15)
     1/ .1777777778E+00,  .7916666667E-01,  .4738977072E-01,
     1  .1274801587E-01,  .2338606059E-02,  .3461358734E-03,
     1  .4407121770E-04,  .4960020603E-05,  .5010387575E-06,
     1  .4592883352E-07,  .3854170274E-08,  .2982393839E-09,
     1  .2141205844E-10,  .1433843200E-11,  .8996663214E-13/
      DATA C4(1),C4(2),C4(3),C4(4),C4(5),C4(6),C4(7),C4(8),C4(9),C4(10),
     1C4(11),C4(12),C4(13),C4(14),C4(15)
     1/-.3222222222E+00, -.5972222222E-01, -.1130268959E+00,
     1 -.4696428571E-01, -.1123273065E-01, -.2025170849E-02,
     1 -.3029410411E-03, -.3912824548E-04, -.4459234519E-05,
     1 -.4551128785E-06, -.4207469124E-07, -.3555931110E-08,
     1 -.2768273269E-09, -.1997821525E-10, -.1343876568E-11/
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPRU'
      ISUBN2='N2  '
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RUN2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPRUN2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASAN,IBUGA3,ISUBRO,N,MAXNXT
   52   FORMAT('ICASAN,IBUGA3,ISUBRO,N,MAXNXT = ',3(A4,2X),2I8)
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RUN2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN RUNS ANALYSIS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      THE NUMBER OF OBSERVATIONS IS LESS THAN TWO.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)N
  112   FORMAT('SAMPLE SIZE = ',I8)
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
C               ********************************************
C               **  STEP 11--                             **
C               **  FORM THE SEQUENTIAL DIFFERENCE TABLE  **
C               ********************************************
C
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RUN2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      AN=N
      NM1=N-1
      DO100I=1,NM1
        IP1=I+1
        XTEMP1(I)=Y(IP1)-Y(I)
  100 CONTINUE
C
C               ***********************************************
C               **  STEP 12--                                **
C               **  ZERO-OUT THE 6 'NUMBER OF RUNS' VECTORS  **
C               ***********************************************
C
      ISTEPN='12'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RUN2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO200I=1,16
        NRUL(I)=0
        NRDL(I)=0
        NRTL(I)=0
        NRULG(I)=0
        NRDLG(I)=0
        NRTLG(I)=0
  200 CONTINUE
C
C               *********************************************************
C               **  STEP 13--                                          **
C               **  DETERMINE THE NUMBER OF RUNS UP OF LENGTH EXACTLY I**
C               **  AND THE NUMBER OF RUNS DOWN OF LENGTH EXACTLY I    **
C               **  DETERMINE THE LENGTH OF THE LONGEST RUN UP         **
C               **  AND THE LENGTH OF THE LONGEST RUN DOWN             **
C               *********************************************************
C
      ISTEPN='13'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RUN2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      LENUP=0
      LENDN=0
      MAXLNU=0
      MAXLND=0
      DO300I=1,NM1
        IF(XTEMP1(I).EQ.0.0)THEN
          IF(LENUP.GE.1)LENUP=LENUP+1
          IF(LENDN.GE.1)LENDN=LENDN+1
          IF(LENUP.EQ.0.AND.LENDN.EQ.0)LENUP=LENUP+1
        ELSEIF(XTEMP1(I).GT.0.0)THEN
          IF(LENDN.GE.1.AND.LENDN.LE.15)NRDL(LENDN)=NRDL(LENDN)+1
          IF(LENDN.GE.16)NRDL(16)=NRDL(16)+1
          LENDN=0
          LENUP=LENUP+1
        ELSEIF(XTEMP1(I).LT.0.0)THEN
          IF(LENUP.GE.1.AND.LENUP.LE.15)NRUL(LENUP)=NRUL(LENUP)+1
          IF(LENUP.GE.16)NRUL(16)=NRUL(16)+1
          LENUP=0
          LENDN=LENDN+1
        ENDIF
        IF(I.EQ.NM1.AND.LENDN.GE.1)THEN
          IF(LENDN.LE.15)NRDL(LENDN)=NRDL(LENDN)+1
          IF(LENDN.GE.16)NRDL(16)=NRDL(16)+1
        ENDIF
        IF(I.EQ.NM1.AND.LENUP.GE.1)THEN
          IF(LENUP.LE.15)NRUL(LENUP)=NRUL(LENUP)+1
          IF(LENUP.GE.16)NRUL(16)=NRUL(16)+1
        ENDIF
        IF(LENUP.GT.MAXLNU)MAXLNU=LENUP
        IF(LENDN.GT.MAXLND)MAXLND=LENDN
  300 CONTINUE
C
C               **************************************************************
C               **  STEP 14--                                               **
C               **  DETERMINE THE NUMBER OF RUNS TOTAL OF LENGTH EXACTLY I  **
C               **  AND THE LENGTH OF THE LONGEST RUN UP OR DOWN            **
C               **************************************************************
C
      ISTEPN='14'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RUN2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO400I=1,16
        NRTL(I)=NRUL(I)+NRDL(I)
  400 CONTINUE
      MAXLNT=MAXLNU
      IF(MAXLND.GT.MAXLNU)MAXLNT=MAXLND
C
C               ***********************************************************
C               **  STEP 15--                                            **
C               **  DETERMINE THE NUMBER OF RUNS UP OF LENGTH I OR MORE  **
C               **  AND THE NUMBER OF RUNS DOWN OF LENGTH I OR MORE      **
C               **  AND THE NUMBER OF RUNS TOTAL OF LENGTH I OR MORE     **
C               ***********************************************************
C
      ISTEPN='15'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RUN2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NRULG(16)=NRUL(16)
      NRDLG(16)=NRDL(16)
      NRTLG(16)=NRTL(16)
      DO500I=1,15
        J=16-I
        JP1=J+1
        NRULG(J)=NRULG(JP1)+NRUL(J)
        NRDLG(J)=NRDLG(JP1)+NRDL(J)
        NRTLG(J)=NRTLG(JP1)+NRTL(J)
  500 CONTINUE
C
C               ****************************************************************
C               **  STEP 16--
C               **  DETERMINE THE NUMBER OF POSITIVE, ZERO, AND NEGATIVE ENTRIES
C               **  IN THE DIFFERENCE TABLE.  IF RANDOM, THE NUMBER OF POSITIVE
C               **  APPROXIMATELY EQUAL TO THE NUMBER OF NEGATIVE
C               ****************************************************************
C
      ISTEPN='16'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RUN2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NNEG=0
      NZER=0
      NPOS=0
      DO800I=1,NM1
        IF(XTEMP1(I).LT.0.0)NNEG=NNEG+1
        IF(XTEMP1(I).EQ.0.0)NZER=NZER+1
        IF(XTEMP1(I).GT.0.0)NPOS=NPOS+1
  800 CONTINUE
C
C               ****************************************************************
C               **  STEP 17--
C               **  COMPUTE THE EXPECTED NUMBER OF RUNS UP OF LENGTH EXACTLY I =
C               **  THE EXPECTED NUMBER OF RUNS DOWN OF LENGTH EXACTLY I =
C               **  ONE HALF THE EXPECTED NUMBER OF RUNS TOTAL OF LENGTH EXACTLY
C               ****************************************************************
C
      ISTEPN='17'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RUN2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DEN=6.0
      DO2000I=1,15
        AI=I
        ENRUL(I)=AN*(AI*AI+3.0*AI+1.0)-(AI*AI*AI+3.0*AI*AI-AI-4.0)
        DEN=DEN*(AI+3.0)
        ENRUL(I)=ENRUL(I)/DEN
        ENRTL(I)=2.0*ENRUL(I)
 2000 CONTINUE
C
C               ****************************************************************
C               **  STEP 18-
C               **  COMPUTE THE EXPECTED NUMBER OF RUNS UP OF LENGTH I OR MORE =
C               **  THE EXPECTED NUMBER OF RUNS DOWN OF LENGTH I OR MORE =
C               **  ONE HALF THE EXPECTED NUMBER OF RUNS TOTAL OF LENGTH I OR MO
C               ****************************************************************
C
      ISTEPN='18'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RUN2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DEN=2.0
      DO2100I=1,15
        AI=I
        ENRULG(I)=AN*(AI+1.0)-(AI*AI+AI-1.0)
        DEN=DEN*(AI+2.0)
        ENRULG(I)=ENRULG(I)/DEN
        ENRTLG(I)=2.0*ENRULG(I)
 2100 CONTINUE
C
C               ****************************************************************
C               **  STEP 19--
C               **  COMPUTE THE STANDARD DEV. OF THE NUMBER OF RUNS UP OF LENGTH
C               **  THE STANDARD DEV. OF THE NUMBER OF RUNS DOWN OF LENGTH EXACT
C               **  SQRT(0.5)* THE STAND. DEV. OF THE NUMBER OF RUNS TOTAL OF LE
C               ****************************************************************
C
      ISTEPN='19'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RUN2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO2500I=1,15
        ARG=C1(I)*AN+C2(I)
        SNRTL(I)=0.0
        IF(ARG.GT.0.0)SNRTL(I)=SQRT(ARG)
        SNRUL(I)=SQRT(0.5)*SNRTL(I)
 2500 CONTINUE
C
C               ****************************************************************
C               **  STEP 20--
C               **  COMPUTE THE STAND. DEV. OF THE NUMBER OF RUNS UP OF LENGTH I
C               **  THE STAND. DEV. OF THE NUMBER OF RUNS DOWN OF LENGTH I OR MO
C               **  SQRT(0.5)* THE STAND. DEV. OF THE NUMBER OF RUNS TOTAL OF LE
C               ****************************************************************
C
      ISTEPN='20'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RUN2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO2600I=1,15
        ARG=C3(I)*AN+C4(I)
        SNRTLG(I)=0.0
        IF(ARG.GT.0.0)SNRTLG(I)=SQRT(ARG)
        SNRULG(I)=SQRT(0.5)*SNRTLG(I)
 2600 CONTINUE
C
C               *************************
C               **  STEP 21--          **
C               **  FORM Z STATISTICS  **
C               *************************
C
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RUN2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO3100I=1,15
C
        STAT=NRUL(I)
        ZNRUL(I)=(-99999.99)
        IF(SNRUL(I).GT.0.0)ZNRUL(I)=(STAT-ENRUL(I))/SNRUL(I)
C
        STAT=NRDL(I)
        ZNRDL(I)=(-99999.99)
        IF(SNRUL(I).GT.0.0)ZNRDL(I)=(STAT-ENRUL(I))/SNRUL(I)
C
        STAT=NRTL(I)
        ZNRTL(I)=(-99999.99)
        IF(SNRTL(I).GT.0.0)ZNRTL(I)=(STAT-ENRTL(I))/SNRTL(I)
C
        STAT=NRULG(I)
        ZNRULG(I)=(-99999.99)
        IF(SNRULG(I).GT.0.0)ZNRULG(I)=(STAT-ENRULG(I))/SNRULG(I)
C
        STAT=NRDLG(I)
        ZNRDLG(I)=(-99999.99)
        IF(SNRULG(I).GT.0.0)ZNRDLG(I)=(STAT-ENRULG(I))/SNRULG(I)
C
        STAT=NRTLG(I)
        ZNRTLG(I)=(-99999.99)
        IF(SNRTLG(I).GT.0.0)ZNRTLG(I)=(STAT-ENRTLG(I))/SNRTLG(I)
C
 3100 CONTINUE
C
      DO3200I=1,15
        ANRUL(I)=NRUL(I)
        ANRDL(I)=NRDL(I)
        ANRTL(I)=NRTL(I)
        ANRULG(I)=NRULG(I)
        ANRDLG(I)=NRDLG(I)
        ANRTLG(I)=NRTLG(I)
 3200 CONTINUE
C
C               ****************************
C               **  STEP 22--             **
C               **  WRITE EVERYTHING OUT  **
C               ****************************
C
      ISTEPN='22'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RUN2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     PRINT SUMMARY STATISTICS TABLE
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
      ITITLE='Runs Analysis'
      NCTITL=13
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
      ITEXT(ICNT)='Length of the Longest Run Up:'
      NCTEXT(ICNT)=29
      AVALUE(ICNT)=REAL(MAXLNU)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Length of the Longest Run Down:'
      NCTEXT(ICNT)=31
      AVALUE(ICNT)=REAL(MAXLND)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Length of the Longest Run Up or Down:'
      NCTEXT(ICNT)=37
      AVALUE(ICNT)=REAL(MAXLNT)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Positive Differences:'
      NCTEXT(ICNT)=31
      AVALUE(ICNT)=REAL(NPOS)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Negative Differences:'
      NCTEXT(ICNT)=31
      AVALUE(ICNT)=REAL(NNEG)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Zero Differences:'
      NCTEXT(ICNT)=25
      AVALUE(ICNT)=REAL(NZER)
      IDIGIT(ICNT)=0
C
      NUMROW=ICNT
      DO2410I=1,NUMROW
        NTOT(I)=15
 2410 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1            AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ITITL9='Runs Up'
      NCTIT9=7
      ITITLE='Statistic: Number of Runs Up of Length Exactly I'
      NCTITL=48
C
      NUMLIN=1
      NUMCOL=5
      DO4101J=1,NUMCLI
        DO4103I=1,MAXLIN
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
 4103   CONTINUE
 4101 CONTINUE
C
      ITITL2(1,1)='I'
      NCTIT2(1,1)=1
      ITITL2(1,2)='Statistic'
      NCTIT2(1,2)=9
      ITITL2(1,3)='EXP(Stat)'
      NCTIT2(1,3)=9
      ITITL2(1,4)='SD(Stat)'
      NCTIT2(1,4)=8
      ITITL2(1,5)='Z-Score'
      NCTIT2(1,5)=7
C
      NMAX=0
      DO4106I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IF(I.EQ.1)NTOT(I)=3
        IF(I.EQ.5)NTOT(I)=10
        NMAX=NMAX+NTOT(I)
        IDIGIT(I)=NUMDIG
        IF(I.EQ.1)IDIGIT(I)=0
        IF(I.EQ.2)IDIGIT(I)=2
        IF(I.EQ.5)IDIGIT(I)=2
 4106 CONTINUE
C
      IMAX=15
      IF(IMAX.GT.N)IMAX=N
      IMAX2=10
C
      DO4110I=1,IMAX2
        NCTEXT(I)=0
        AMAT(I,1)=REAL(I)
        AMAT(I,2)=ANRUL(I)
        AMAT(I,3)=ENRUL(I)
        AMAT(I,4)=SNRUL(I)
        AMAT(I,5)=ZNRUL(I)
 4110 CONTINUE
      IWHTML(1)=75
      IWHTML(2)=150
      IWHTML(3)=150
      IWHTML(4)=150
      IWHTML(5)=150
      IWHTML(6)=150
      IWRTF(1)=800
      IWRTF(2)=IWRTF(1)+1800
      IWRTF(3)=IWRTF(2)+1800
      IWRTF(4)=IWRTF(3)+1800
      IWRTF(5)=IWRTF(4)+1800
      IFRST=.TRUE.
      ILAST=.TRUE.
C
      CALL DPDTA2(ITITL9,NCTIT9,
     1            ITITLE,NCTITL,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            ITEXT,NCTEXT,AMAT,MAXROW,IMAX2,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ITITL9=' '
      NCTIT9=0
      ITITLE='Statistic: Number of Runs Up of Length I or More'
      NCTITL=48
C
      DO4130I=1,IMAX2
        NCTEXT(I)=0
        AMAT(I,1)=REAL(I)
        AMAT(I,2)=ANRULG(I)
        AMAT(I,3)=ENRULG(I)
        AMAT(I,4)=SNRULG(I)
        AMAT(I,5)=ZNRULG(I)
 4130 CONTINUE
      IFRST=.TRUE.
      ILAST=.TRUE.
C
      CALL DPDTA2(ITITL9,NCTIT9,
     1            ITITLE,NCTITL,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            ITEXT,NCTEXT,AMAT,MAXROW,IMAX2,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ITITL9='Runs Down'
      NCTIT9=9
      ITITLE='Statistic: Number of Runs Down of Length Exactly I'
      NCTITL=50
C
      DO4210I=1,IMAX2
        NCTEXT(I)=0
        AMAT(I,1)=REAL(I)
        AMAT(I,2)=ANRDL(I)
        AMAT(I,3)=ENRUL(I)
        AMAT(I,4)=SNRUL(I)
        AMAT(I,5)=ZNRDL(I)
 4210 CONTINUE
      IFRST=.TRUE.
      ILAST=.TRUE.
C
      CALL DPDTA2(ITITL9,NCTIT9,
     1            ITITLE,NCTITL,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            ITEXT,NCTEXT,AMAT,MAXROW,IMAX2,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ITITL9=' '
      NCTIT9=0
      ITITLE='Statistic: Number of Runs Down of Length I or More'
      NCTITL=50
C
      DO4230I=1,IMAX2
        NCTEXT(I)=0
        AMAT(I,1)=REAL(I)
        AMAT(I,2)=ANRDLG(I)
        AMAT(I,3)=ENRULG(I)
        AMAT(I,4)=SNRULG(I)
        AMAT(I,5)=ZNRDLG(I)
 4230 CONTINUE
      IFRST=.TRUE.
      ILAST=.TRUE.
C
      CALL DPDTA2(ITITL9,NCTIT9,
     1            ITITLE,NCTITL,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            ITEXT,NCTEXT,AMAT,MAXROW,IMAX2,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ITITL9='Runs Total = Runs Up + Runs Down'
      NCTIT9=32
      ITITLE='Statistic: Number of Runs Total of Length Exactly I'
      NCTITL=51
C
      DO4310I=1,IMAX2
        NCTEXT(I)=0
        AMAT(I,1)=REAL(I)
        AMAT(I,2)=ANRTL(I)
        AMAT(I,3)=ENRTL(I)
        AMAT(I,4)=SNRTL(I)
        AMAT(I,5)=ZNRTL(I)
 4310 CONTINUE
      IFRST=.TRUE.
      ILAST=.TRUE.
C
      CALL DPDTA2(ITITL9,NCTIT9,
     1            ITITLE,NCTITL,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            ITEXT,NCTEXT,AMAT,MAXROW,IMAX2,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ITITL9=' '
      NCTIT9=0
      ITITLE='Statistic: Number of Runs Total of Length I or More'
      NCTITL=51
C
      DO4330I=1,IMAX2
        NCTEXT(I)=0
        AMAT(I,1)=REAL(I)
        AMAT(I,2)=ANRTLG(I)
        AMAT(I,3)=ENRTLG(I)
        AMAT(I,4)=SNRTLG(I)
        AMAT(I,5)=ZNRTLG(I)
 4330 CONTINUE
      IFRST=.TRUE.
      ILAST=.TRUE.
C
      CALL DPDTA2(ITITL9,NCTIT9,
     1            ITITLE,NCTITL,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            ITEXT,NCTEXT,AMAT,MAXROW,IMAX2,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            ISUBRO,IBUGA3,IERROR)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RUN2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPRUN2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPRUNS(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                  IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--FORM A RUN-SEQUENCE PLOT.
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
C     ORIGINAL VERSION--DECEMBER  1977.
C     UPDATED         --JANUARY   1978.
C     UPDATED         --FEBRUARY  1978.
C     UPDATED         --MAY       1978.
C     UPDATED         --JULY      1978.
C     UPDATED         --JANUARY   1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --MAY       2011. USE DPPARS
C     UPDATED         --MAY       2011. SUPPORT HIGHLIGHT, MULTIPLE
C                                       AND REPLICATION OPTIONS
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
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      CHARACTER*4 IREPL
      CHARACTER*4 IMULT
      CHARACTER*4 IHIGH
      CHARACTER*4 IWRITE
C
      CHARACTER*4 ICTMP1
      CHARACTER*4 ICTMP2
      CHARACTER*4 ICTMP3
      CHARACTER*4 ICTMP4
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
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
C
      DIMENSION XHIGH(MAXOBV)
      DIMENSION ZY(MAXOBV)
      DIMENSION ZX(MAXOBV)
      DIMENSION X1(MAXOBV)
      DIMENSION X2(MAXOBV)
      DIMENSION X3(MAXOBV)
      DIMENSION X4(MAXOBV)
      DIMENSION X5(MAXOBV)
      DIMENSION X6(MAXOBV)
      DIMENSION XTEMP1(MAXOBV)
      DIMENSION XTEMP2(MAXOBV)
      DIMENSION XTEMP3(MAXOBV)
      DIMENSION XTEMP4(MAXOBV)
      DIMENSION XTEMP5(MAXOBV)
      DIMENSION XTEMP6(MAXOBV)
C
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(IGARB1),XHIGH(1))
      EQUIVALENCE (GARBAG(IGARB2),ZY(1))
      EQUIVALENCE (GARBAG(IGARB3),ZX(1))
      EQUIVALENCE (GARBAG(IGARB4),X1(1))
      EQUIVALENCE (GARBAG(IGARB5),X2(1))
      EQUIVALENCE (GARBAG(IGARB6),X3(1))
      EQUIVALENCE (GARBAG(IGARB7),X4(1))
      EQUIVALENCE (GARBAG(IGARB8),X5(1))
      EQUIVALENCE (GARBAG(IGARB9),X6(1))
      EQUIVALENCE (GARBAG(IGAR10),XTEMP1(1))
      EQUIVALENCE (GARBAG(JGAR11),XTEMP2(1))
      EQUIVALENCE (GARBAG(JGAR12),XTEMP3(1))
      EQUIVALENCE (GARBAG(JGAR13),XTEMP4(1))
      EQUIVALENCE (GARBAG(JGAR14),XTEMP5(1))
      EQUIVALENCE (GARBAG(JGAR15),XTEMP6(1))
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
      IREPL='OFF'
      IHIGH='OFF'
      IMULT='OFF'
      ISUBN1='DPRU'
      ISUBN2='NS  '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
C               ****************************************
C               **  TREAT THE RUN-SEQUENCE PLOT CASE  **
C               ****************************************
C
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'RUNS')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPRUNS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASPL,IAND1,IAND2
   52   FORMAT('ICASPL,IAND1,IAND2 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGG2,IBUGG3,IBUGQ,ISUBRO
   53   FORMAT('IBUGG2,IBUGG3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      ICASPL='RUNS'
      ILASTZ=9999
      DO100I=0,NUMARG-1
        IF(I.EQ.0)THEN
          ICTMP1=ICOM
        ELSE
          ICTMP1=IHARG(I)
        ENDIF
        ICTMP2=IHARG(I+1)
        ICTMP3=IHARG(I+2)
        ICTMP4=IHARG(I+3)
C
        IF(ICTMP1.EQ.'RUN' .AND. ICTMP2.EQ.'SEQU')THEN
          IFOUND='YES'
          ILASTZ=I+1
        ELSEIF(ICTMP1.EQ.'PLOT')THEN
          ILASTZ=I
        ELSEIF(ICTMP1.EQ.'REPL')THEN
          IREPL='ON'
          ILASTZ=MAX(ILASTZ,I)
        ELSEIF(ICTMP1.EQ.'MULT')THEN
          IMULT='ON'
          ILASTZ=MAX(ILASTZ,I)
        ELSEIF(ICTMP1.EQ.'HIGH' .OR. ICTMP1.EQ.'SUBS')THEN
          IHIGH='ON'
          ILASTZ=MAX(ILASTZ,I)
        ENDIF
  100 CONTINUE
C
      IF(IFOUND.EQ.'NO')GOTO9000
C
      ISHIFT=ILASTZ
      CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1            IBUGG2,IERROR)
C
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'RUNS')THEN
        WRITE(ICOUT,92)IMULT,IREPL,IHIGH,ILASTZ
   92   FORMAT('IMULT,IREPL,IHIGH,ILASTZ = ',3(A4,2X),I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IMULT.EQ.'ON')THEN
        IF(IREPL.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
  101     FORMAT('***** ERROR IN RUN SEQUENCE PLOT--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,102)
  102     FORMAT('      YOU CANNOT SPECIFY BOTH "MULTIPLE" AND ',
     1           '"REPLICATION" FOR THE PROBABILITY PLOT.')
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
     1           '"HIGHTLIGHTED" FOR THE PROBABILITY PLOT.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ENDIF
C
C               *********************************
C               **  STEP 2--                   **
C               **  EXTRACT THE VARIABLE LIST  **
C               *********************************
C
      ISTEPN='4'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'RUNS')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='RUN SEQUENCE PLOT'
      MINNA=1
      MAXNA=100
      MINN2=1
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
      MINNVA=1
      MAXNVA=1
      IF(IHIGH.EQ.'ON')THEN
        MINNVA=2
        MAXNVA=2
      ELSEIF(IREPL.EQ.'ON')THEN
        MINNVA=2
        MAXNVA=7
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
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'RUNS')THEN
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
      IF(IERROR.EQ.'YES')GOTO9000
C
C               ***********************************************
C               **  STEP 3--                                 **
C               **  DETERMINE:                               **
C               **  1) NUMBER OF REPLICATION VARIABLES (0-1) **
C               **  2) NUMBER OF RESPONSE    VARIABLES (>= 1)**
C               **  3) NUMBER OF HIGHLIGHT   VARIABLES (0-1) **
C               ***********************************************
C
      ISTEPN='5'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'RUNS')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NRESP=0
      NREPL=0
      NHIGH=0
      IF(IMULT.EQ.'ON')THEN
        NRESP=NUMVAR
      ELSEIF(IHIGH.EQ.'ON')THEN
        NRESP=1
        NHIGH=1
      ELSEIF(IREPL.EQ.'ON')THEN
        NRESP=1
        NREPL=NUMVAR-NRESP
        IF(NREPL.LT.1)IREPL='OFF'
        IF(NREPL.GT.6)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,511)
  511     FORMAT('      FOR THE REPLICATION CASE, AT MOST SIX ',
     1           'REPLICATION VARIABLE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,512)
  512     FORMAT('      ALLOWED;  SUCH WAS NOT THE CASE HERE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,513)NREPL
  513     FORMAT('      THE NUMBER OF REPLICATION VARIABLES = ',I5)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ENDIF
C
C       CASE 1: NO HIGHLIGHTING AND NO REPLICATION
C
      IF(IREPL.EQ.'OFF' .AND. IHIGH.EQ.'OFF')THEN
        NPLOTP=0
        ICNT=0
        DO510K=1,NUMVAR
          ICOL=K
          NUMVA2=1
          CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1                INAME,IVARN1,IVARN2,IVARTY,
     1                ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1                MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1                MAXCP4,MAXCP5,MAXCP6,
     1                V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1                ZY,ZY,ZY,NS,NS,NS,ICASE,
     1                IBUGG3,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ICNT=ICNT+1
          IF(NS.GE.1)THEN
            DO520I=1,NS
              NPLOTP=NPLOTP+1
              Y(NPLOTP)=ZY(I)
              X(NPLOTP)=REAL(NPLOTP)
              D(NPLOTP)=REAL(ICNT)
  520       CONTINUE
          ENDIF
  510   CONTINUE
C
C       CASE 2: HIGHLIGHTING
C
      ELSEIF(IHIGH.EQ.'ON')THEN
        NPLOTP=0
        ICOL=1
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              ZY,XHIGH,ZY,NS,NS,NS,ICASE,
     1              IBUGG3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
        IF(NS.GE.1)THEN
          DO620I=1,NS
            NPLOTP=NPLOTP+1
            Y(NPLOTP)=ZY(I)
            X(NPLOTP)=REAL(NPLOTP)
            D(NPLOTP)=XHIGH(I)
  620     CONTINUE
        ENDIF
      ELSEIF(IREPL.EQ.'ON')THEN
        ICOL=1
        CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              ZY,X1,X2,X3,X4,X5,X6,NLOCAL,
     1              IBUGG3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        IF(NLOCAL.LE.0)GOTO9000
C
C       IF THERE ARE TWO OR MORE REPLICATION VARIABLES, COMBINE
C       THEM TO CREATE A SINGLE REPLICATION VARIABLE.
C
        ICCTOF=0
        ICCTG1=-99
        ICCTG2=-99
        ICCTG3=-99
        ICCTG4=-99
        ICCTG5=-99
        IWRITE='OFF'
C
        IF(NUMVAR.EQ.3)THEN
          CALL CODCT2(X1,X2,NLOCAL,ICCTOF,ICCTG1,IWRITE,
     1                ZX,XTEMP1,XTEMP2,
     1                IBUGG3,ISUBRO,IERROR)
          DO7011I=1,NLOCAL
            X1(I)=ZX(I)
 7011     CONTINUE
          NUMVAR=2
        ELSEIF(NUMVAR.EQ.4)THEN
          CALL CODCT3(X1,X2,X3,NLOCAL,ICCTOF,ICCTG1,ICCTG2,IWRITE,
     1                ZX,XTEMP1,XTEMP2,XTEMP3,
     1                IBUGG3,ISUBRO,IERROR)
          DO7012I=1,NLOCAL
            X1(I)=ZX(I)
 7012     CONTINUE
          NUMVAR=2
        ELSEIF(NUMVAR.EQ.5)THEN
          CALL CODCT4(X1,X2,X3,X4,NLOCAL,
     1                ICCTOF,ICCTG1,ICCTG2,ICCTG3,IWRITE,
     1                ZX,XTEMP1,XTEMP2,XTEMP3,XTEMP4,
     1                IBUGG3,ISUBRO,IERROR)
          DO7013I=1,NLOCAL
            X1(I)=ZX(I)
 7013     CONTINUE
          NUMVAR=2
        ELSEIF(NUMVAR.EQ.6)THEN
          CALL CODCT5(X1,X2,X3,X4,X5,NLOCAL,
     1                ICCTOF,ICCTG1,ICCTG2,ICCTG3,ICCTG4,IWRITE,
     1                ZX,XTEMP1,XTEMP2,XTEMP3,XTEMP4,XTEMP5,
     1                IBUGG3,ISUBRO,IERROR)
          DO7014I=1,NLOCAL
            X1(I)=ZX(I)
 7014     CONTINUE
          NUMVAR=2
        ELSEIF(NUMVAR.EQ.7)THEN
          CALL CODCT6(X1,X2,X3,X4,X5,X6,NLOCAL,
     1                ICCTOF,ICCTG1,ICCTG2,ICCTG3,ICCTG4,ICCTG5,IWRITE,
     1                ZX,XTEMP1,XTEMP2,XTEMP3,XTEMP4,XTEMP5,XTEMP6,
     1                IBUGG3,ISUBRO,IERROR)
          DO7015I=1,NLOCAL
            X1(I)=ZX(I)
 7015     CONTINUE
          NUMVAR=2
        ENDIF
C
        NPLOTP=0
        DO7020I=1,NLOCAL
            NPLOTP=NPLOTP+1
            Y(NPLOTP)=ZY(I)
            X(NPLOTP)=REAL(NPLOTP)
            D(NPLOTP)=X1(I)
 7020   CONTINUE
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      NPLOTV=1
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'RUNS')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPRUNS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTV,NPLOTP,NLOCAL
 9013   FORMAT('NPLOTV,NPLOTP,NLOCAL = ',3I8)
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
      SUBROUTINE DPRUPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                  IANGLU,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
C
C     PURPOSE--GENERATE A RUNS PLOT.
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
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--SEPTEMBER 1981.
C     UPDATED         --MAY       1982.
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
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPRU'
      ISUBN2='PL  '
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGG2.EQ.'ON')THEN
        WRITE(ICOUT,51)
   51   FORMAT('***** ERROR IN DPRUPL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)NPLOTV,NPLOTP,NS
   53   FORMAT('NPLOTV,NPLOTP,NS = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)ICASPL,IAND1,IAND2,IANGLU
   55   FORMAT('ICASPL,IAND1,IAND2,IANGLU = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,57)IBUGG2,IBUGG3,IBUGQ
   57   FORMAT('IBUGG2,IBUGG3,IBUGQ = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,101)
  101 FORMAT('***** ERROR IN DPRUPL--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,102)
  102 FORMAT('      RUNS PLOT CAPABILITY')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,103)
  103 FORMAT('      NOT YET AVAILABLE')
      CALL DPWRST('XXX','BUG ')
C
      RETURN
      END
      SUBROUTINE DPRWLA(IA,IPARN,IPARN2,IWRITE,IINDX,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--CONVERT A STRING TO A ROW LABEL.  EXAMPLE:
C
C                 LET ROWLABEL = STRING TO ROW LABEL IROW S
C
C              WHERE IROW IS THE ROW NUMBER IN THE ROW LABEL AND
C              S IS A PREVIOUSLY DEFINED STRING.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THE MAXIMUM ROW NUMBER IS MAXOBV.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--NONE.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2012/6
C     ORIGINAL VERSION--JUNE      2012.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 MESSAG
      CHARACTER*4 IA
      CHARACTER*4 IPARN
      CHARACTER*4 IPARN2
C
      DIMENSION IA(*)
      DIMENSION IPARN(*)
      DIMENSION IPARN2(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOHK.INC'
C
      CHARACTER*4 IHTEMP(200)
      CHARACTER*130 ISTRIN
      CHARACTER*130 ISTRI2
C
      PARAMETER(MAXIND=100)
C
      CHARACTER*4 ISTRN1(MAXIND)
      CHARACTER*4 ISTRN2(MAXIND)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPRW'
      ISUBN2='LA  '
      IERROR='NO'
      IOPFLG=0
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RWLA')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPRWLA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,IA(1),IPARN(1),IPARN2(1)
   52   FORMAT('IBUGA3,ISUBRO,IA(1),IPARN1,IPARN2 = ',3(A4,2X),2A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *************************************************
C               **  STEP 1--                                   **
C               **  DETERMINE ROW INDEX.                       **
C               *************************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RWLA')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NTEMP=IINDX+1
      IF(IARGT(NTEMP).EQ.'NUMB')THEN
        IROW=INT(ARG(NTEMP)+0.5)
        IF(IROW.LT.1 .OR. IROW.GT.MAXOBV)THEN
          WRITE(ICOUT,1001)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1013)MAXOBV
 1013     FORMAT('      THE ROW INDEX IS LESS THAN ONE OR GREATER ',
     1           'THAN ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1015)NTEMP
 1015     FORMAT('      THE VALUE OF THE ROW INDEX  = ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ELSE
        WRITE(ICOUT,1001)
 1001   FORMAT('***** ERROR IN STRING TO ROW LABEL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1003)NTEMP
 1003   FORMAT('      ARGUMENT ',I5,' (THE ROW INDEX) IS NOT NUMBER.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1005)IHARG(NTEMP),IHARG2(NTEMP)
 1005   FORMAT('      THE VALUE OF THE ARGUMENT  = ',A4,A4)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               *************************************************
C               **  STEP 2--                                   **
C               **  DETERMINE IF NEXT ARGUMENT IS A PREVIOUSLY **
C               **  DEFINED STRING.  IF NOT, TREAT AS A        **
C               **  LITERAL STRING.                            **
C               *************************************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RWLA')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      JMIN=IINDX+2
      JMAX=NUMARG
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RWLA')THEN
        WRITE(ICOUT,4001)JMIN,JMAX,MAXIND
 4001   FORMAT('JMIN,JMAX,MAXIND = ',3I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(JMAX.LT.JMIN)GOTO8000
      IWRITE='OFF'
      IERROR='NO'
C
      CALL EXTSTR(IHARG,IHARG2,NUMARG,JMIN,JMAX,MAXIND,
     1            IHNAME,IHNAM2,IUSE,NUMNAM,
     1            ISTRN1,ISTRN2,NUMSTR,
     1            IWRITE,IBUGA3,ISUBRO,IERROR)
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RWLA')THEN
        WRITE(ICOUT,4003)NUMSTR
 4003   FORMAT('NUMSTR = ',I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IERROR.EQ.'NO')THEN
C
C  CASE WHERE WE ARE EXTRACTING STRINGS
C
        IONE=1
        NUMSTR=MIN(NUMSTR,IONE)
        N=NUMSTR
        IROWLB(IROW)=' '
C
        DO4010I2=1,NUMSTR
          DO4015I=1,NUMNAM
            II=I
            IF(ISTRN1(I2).EQ.IHNAME(I) .AND. ISTRN2(I2).EQ.IHNAM2(I))
     1        GOTO4019
 4015     CONTINUE
C
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1001)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,4023)ISTRN1(I2),ISTRN2(I2)
 4023     FORMAT('       STRING ',A4,A4,' NOT MATCHED IN NAME ',
     1           'TABLE.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO8000
C
 4019     CONTINUE
          IVAL=IVALUE(II)
          VAL=VALUE(II)
          IL1=IVSTAR(II)
          IL2=IVSTOP(II)
C
          IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RWLA')THEN
            WRITE(ICOUT,4011)IL1,IL2
 4011       FORMAT('II,IL1,IL2 = ',3I8)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          CALL DPCOFH(IL1,IL2,IFUNC,NUMCHF,IHTEMP,NH,IBUGA3,IERROR)
          ILAST=MIN(24,NH)
C
          IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RWLA')THEN
            WRITE(ICOUT,4013)NH,ILAST
 4013       FORMAT('NH,ILAST = ',2I8)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          IF(ILAST.GT.0)THEN
            IROWLB(IROW)=' '
            DO4020J=1,ILAST
              IROWLB(IROW)(J:J)=IHTEMP(J)(1:1)
 4020       CONTINUE
C
            IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RWLA')THEN
              WRITE(ICOUT,4014)IROW,IROWLB(IROW)
 4014         FORMAT('IROW,IROWLB(IROW) = ',I8,A24)
              CALL DPWRST('XXX','BUG ')
            ENDIF
C
          ENDIF
 4010   CONTINUE
      ELSE
C
C  CASE WHERE WE ARE EXTRACTING LITERALS
C
        ICNT=0
        IFRST=5
        MESSAG='OFF'
        IROWLB(IROW)=' '
        DO4108I=1,130
          ISTRIN(I:I)=IANSLC(I)(1:1)
 4108   CONTINUE
C
 4100   CONTINUE
        IFRST=IFRST+1
        ICNT=ICNT+1
        ISTART=1
        ISTOP=130
        IERROR='NO'
        ICOL1=1
        ICOL2=130
        NCOLMX=130
        CALL DPEXS1(ISTRIN,NCOLMX,ISTART,ISTOP,IFRST,MESSAG,
     1              ICOL1,ICOL2,ISTRI2,NCSTR2,
     1              IBUGA3,ISUBRO,IERROR)
        IF(NCSTR2.GT.0 .AND. IERROR.NE.'YES')THEN
          ILAST=MIN(24,NCSTR2)
          IROWLB(IROW)=' '
          DO4120J=1,ILAST
            IROWLB(IROW)(J:J)=ISTRI2(J:J)
 4120     CONTINUE
          GOTO4100
        ENDIF
        N=ICNT-1
      ENDIF
C
C               ******************************
C               **  STEP 3--                **
C               **  WRITE OUT A FEW LINES   **
C               **  OF SUMMARY INFORMATION  **
C               **  ABOUT THE CODING.       **
C               ******************************
C
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2821)IROW,IROWLB(IROW)
 2821   FORMAT('ROW LABEL ',I8,' SET TO: ',A24)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GOTO8000
C
 8000 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RWLA')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DPRWLA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR
 9012   FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPRWL2(IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--DEFINE A SPECIFIC ROW LABEL.  FOR EXAMPLE
C
C                 LET ROWLABEL 3 = CIRC
C
C              WILL DEFINE ROW LABEL 3 AS "CIRC".  THIS COMMAND
C              HAS OCCASSIONAL USE WHEN THE ROW LABELS ARE USED
C              BY THE CHARACTER COMMAND TO DEFINE PLOT POINTS.
C
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THE MAXIMUM ROW NUMBER IS MAXOBV.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--NONE.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2012/8
C     ORIGINAL VERSION--AUGUST    2012.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPRW'
      ISUBN2='L2  '
      IERROR='NO'
C
      DO10I=1,MAXOBV
        ISUB(I)=1
   10 CONTINUE
      IEQUAL=0
      ILAST=0
      IFRST=0
      NLEN=0
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RWL2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPRWL2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO
   52   FORMAT('IBUGA3,ISUBRO = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *************************************************
C               **  STEP 1--                                   **
C               **  DETERMINE INDEX VALUE                      **
C               *************************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RWL2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IINDX=0
      IF(IARGT(2).EQ.'NUMB')THEN
        IINDX=INT(ARG(2)+0.5)
        IF(IINDX.LT.1 .OR. IINDX.GT.MAXOBV)THEN
          WRITE(ICOUT,1001)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1013)MAXOBV
 1013     FORMAT('      THE ROW INDEX IS LESS THAN ONE OR GREATER ',
     1           'THAN ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1015)IINDX
 1015     FORMAT('      THE VALUE OF THE ROW INDEX = ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ELSE
        WRITE(ICOUT,1001)
 1001   FORMAT('***** ERROR IN ROW LABEL INDEX--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1003)
 1003   FORMAT('      ARGUMENT 3 (THE ROW INDEX) IS NOT A NUMBER.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1005)IHARG(3),IHARG2(3)
 1005   FORMAT('      THE VALUE OF THE ARGUMENT  = ',2A4)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               *************************************************
C               **  STEP 2--                                   **
C               **  NOW EXTRACT THE LABEL FROM IANSLC          **
C               *************************************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RWL2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     CHECK FOR SPECIAL CASE: NO ARGUMENTS AFTER "="
C
      IF(NUMARG.EQ.3)THEN
        IROWLB(IINDX)(I:I)=' '
        GOTO4000
      ENDIF
C
C     FIRST FIND THE LOCATION OF THE "="
C
      DO2010I=1,IWIDTH
        IF(IANSLC(I)(1:1).EQ.'=')THEN
          IEQUAL=I
          GOTO2019
        ENDIF
 2010 CONTINUE
 2019 CONTINUE
C
C     NOW FIND THE LAST NON-BLANK CHARACTER IN IANSLC
C
      DO2110I=IWIDTH,IEQUAL+1,-1
        IF(IANSLC(I)(1:1).NE.' ')THEN
          ILAST=I
          GOTO2119
        ENDIF
 2110 CONTINUE
 2119 CONTINUE
C
C               *************************************************
C               **  STEP 3--                                   **
C               **  NOW DEFINE THE ROW LABEL                   **
C               *************************************************
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RWL2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)

      IFRST=IEQUAL+1
      NLEN=ILAST-IFRST+1
      IF(NLEN.GT.24)NLEN=24
      IF(NLEN.LT.1)NLEN=1
      IROWLB(IINDX)=' '
      DO3010I=1,NLEN
        ICNT=IEQUAL+I
        IROWLB(IINDX)(I:I)=IANSLC(ICNT)(1:1)
 3010 CONTINUE
C
C               ******************************
C               **  STEP 4--                **
C               **  WRITE OUT A FEW LINES   **
C               **  OF SUMMARY INFORMATION  **
C               **  ABOUT THE CODING.       **
C               ******************************
C
 4000 CONTINUE
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4010)IINDX,IROWLB(IINDX)(1:24)
 4010   FORMAT('ROW LABEL ',I8,' HAS BEEN SET TO ',A24)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RWL2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DPRWL2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR,IINDX
 9012   FORMAT('IBUGA3,IERROR,IINDX = ',A4,2X,A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
        IF(IINDX.GE.1 .AND. IINDX.LE.MAXOBV)THEN
          WRITE(ICOUT,9014)IROWLB(IINDX)(1:24)
 9014     FORMAT('IROWLB(IINDX) = ',A24)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPRWSH(IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--SHIFT ROW LABELS LEFT (DOWN) OR RIGHT (UP)
C              A SPECIFIED NUMBER OF ROWS.  FOR EXAMPLE,
C
C                 LET ROWLABEL = SHIFT LEFT 3
C
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THE MAXIMUM ROW NUMBER IS MAXOBV.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--NONE.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2012/8
C     ORIGINAL VERSION--AUGUST    2012.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 IDIR
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPRW'
      ISUBN2='SH  '
      IERROR='NO'
      IDIR='LEFT'
      IF(IHARG(4).EQ.'RIGH')IDIR='RIGH'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RWSH')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPRWSH--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,IDIR
   52   FORMAT('IBUGA3,ISUBRO,IDIR = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *************************************************
C               **  STEP 1--                                   **
C               **  DETERMINE SHIFT VALUE                      **
C               *************************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RWSH')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NSHIFT=0
      IF(IARGT(5).EQ.'NUMB')THEN
        NSHIFT=INT(ARG(5)+0.5)
        NSHIFT=ABS(NSHIFT)
        IF(NSHIFT.LT.1 .OR. NSHIFT.GT.MAXOBV)THEN
          WRITE(ICOUT,1001)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1013)MAXOBV
 1013     FORMAT('      THE SHIFT VALUE IS LESS THAN ONE OR GREATER ',
     1           'THAN ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1015)NSHIFT
 1015     FORMAT('      THE VALUE OF THE SHIFT = ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ELSE
        WRITE(ICOUT,1001)
 1001   FORMAT('***** ERROR IN ROW LABEL SHIFT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1003)
 1003   FORMAT('      ARGUMENT 5 (THE SHIFT VALUE) IS NOT A NUMBER.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1005)IHARG(5),IHARG2(5)
 1005   FORMAT('      THE VALUE OF THE ARGUMENT  = ',2A4)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               *************************************************
C               **  STEP 2--                                   **
C               **  NOW SHIFT THE ROW LABELS.                  **
C               *************************************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RWSH')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IDIR.EQ.'LEFT')THEN
         ISTRT=NSHIFT+1
         ISTOP=MAXOBV
         DO2010I=ISTRT,ISTOP
           IROWLB(I-NSHIFT)=IROWLB(I)
 2010    CONTINUE
         ITEMP=MAXOBV-NSHIFT+1
         DO2020I=ITEMP,MAXOBV
           IROWLB(I)='BLAN'
 2020    CONTINUE
      ELSE
         ISTRT=1
         ISTOP=MAXOBV-NSHIFT
         DO2110I=ISTOP,ISTRT,-1
           IROWLB(I+NSHIFT)=IROWLB(I)
 2110    CONTINUE
         DO2120I=1,NSHIFT
           IROWLB(I)='BLAN'
 2120    CONTINUE
      ENDIF
C
C               ******************************
C               **  STEP 3--                **
C               **  WRITE OUT A FEW LINES   **
C               **  OF SUMMARY INFORMATION  **
C               **  ABOUT THE CODING.       **
C               ******************************
C
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IF(IDIR.EQ.'LEFT')THEN
          WRITE(ICOUT,2811)NSHIFT
 2811     FORMAT('THE ROW LABELS HAVE BEEN SHIFTED ',I8,' ROWS LEFT.')
          CALL DPWRST('XXX','BUG ')
        ELSE
          WRITE(ICOUT,2821)NSHIFT
 2821     FORMAT('THE ROW LABELS HAVE BEEN SHIFTED ',I8,' ROWS RIGHT.')
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RWSH')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DPRWSH--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR,NSHIFT
 9012   FORMAT('IBUGA3,IERROR,NSHIFT = ',A4,2X,A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPSACO(IANSLC,IWIDTH,IHARG,IARGT,IARG,NUMARG,
     1                  IANSSV,IREPMX,IPOINT,ISACNC,
     1                  IBUGS2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--SAVE (FOR FUTURE USE BY THE REEXECUTE COMMAND)
C              SELECTED COMMANDS IN THE (RECENT) COMMAND LIST.
C              THE RECENT COMMAND LIST CONSISTS OF THE
C              LAST IREPMX (= 50) COMMANDS.
C              LAST MAXLIS (==> 200) COMMANDS.  APRIL 1993
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
C     ORIGINAL VERSION--APRIL     1986.
C     UPDATED         --APRIL     1993. SOFT-CODE DIMEN. FOR IANSSV()
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
CCCCC THE FOLLOWING LINE WAS ADDED APRIL 1993
      INCLUDE 'DPCOPA.INC'
C
      CHARACTER*4 IANSLC(*)
      CHARACTER*4 IHARG(*)
      CHARACTER*4 IARGT(*)
      CHARACTER*1 IANSSV(MAXLIS,MAXCIS)
CCCCC CHARACTER*80 ISACNC
      CHARACTER (LEN=*) :: ISACNC
C
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
      CHARACTER*4 IFOUND
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
      CHARACTER*1 IC1
      CHARACTER*4 IC4
CCCCC CHARACTER*80 ISTRIN
CCCCC CHARACTER*80 ISTRI2
      CHARACTER (LEN=MAXSTR) :: ISTRIN
      CHARACTER (LEN=MAXSTR) :: ISTRI2
C
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
      DIMENSION IARG(*)
CCCCC THE FOLLOWING 2 LINES WERE CHANGED APRIL 1993
CCCCC DIMENSION IANSSV(50,80)
CCCCC DIMENSION ITAB(50)
      DIMENSION ITAB(MAXLIS)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPSA'
      ISUBN2='CO  '
      IFOUND='NO'
      IERROR='NO'
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SACO')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('AT THE BEGINNING OF DPSACO--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGS2,ISUBRO,IFOUND,IERROR,IWIDTH,NUMARG
   52   FORMAT('IBUGS2,ISUBRO,IFOUND,IERROR,IWIDTH,NUMARG = ',
     1         4(A4,2X),2I8)
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
CCCCC   THE FOLLOWING 2 LINES WERE CHANGED APRIL 1993
CCCCC   WRITE(ICOUT,61)IREPMX,IPOINT
CCC61   FORMAT('IREPMX,IPOINT = ',I8,2X,I8)
CCCCC   CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,61)MAXLIS,IPOINT,ISACNU
   61   FORMAT('MAXLIS,IPOINT,ISACNU = ',3I8)
        CALL DPWRST('XXX','BUG ')
CCCCC   THE FOLLOWING LINE WAS CHANGED APRIL 1993
CCCCC   DO62J=1,IREPMX
        DO62J=1,MAXLIS
          WRITE(ICOUT,63)J,(IANSSV(J,I),I=1,80)
   63     FORMAT('J,(IANSSV(J,I),I=1,80) = ',I8,2X,80A1)
          CALL DPWRST('XXX','BUG ')
   62   CONTINUE
        WRITE(ICOUT,72)ISACNA
   72   FORMAT('ISACNA = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,73)ISACST,ISACFO,ISACAC,ISACFO,ISACCS
   73   FORMAT('ISACST,ISACFO,ISACAC,ISACFO,ISACCS = ',4(A12,2X),A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,81)ISACNC
   81   FORMAT('ISACNC = ',A80)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IFOUND='YES'
C
C               ******************************************************
C               **  STEP 11--                                       **
C               **  DETERMINE IF HAVE AN EXPLICIT FILE REFERENCE    **
C               **  WHERE THE COMMANDS WILL BE SAVED, OR WILL THEY  **
C               **  BE SAVED IN THE DEFAULT FILE (DPSACF.TEX)?      **
C               ******************************************************
C
      ISTEPN='11'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SACO')
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
      ISTOP=MAXSTR
      CALL DPEXWO(ISTRIN,ISTART,ISTOP,IWORD,
     1            ICOL1,ICOL2,ISTRI2,NCSTR2,
     1            IBUGS2,ISUBRO,IERROR)
C
      IF(NUMARG.GT.0)THEN
        IWORD=2
        ISTART=1
        ISTOP=MAXSTR
        CALL DPEXWO(ISTRIN,ISTART,ISTOP,IWORD,
     1              ICOL1,ICOL2,ISTRI2,NCSTR2,
     1              IBUGS2,ISUBRO,IERROR)
        IF(NCSTR2.GT.0)THEN
          DO1121I=1,NCSTR2
            IF(ISTRI2(I:I).EQ.'.')THEN
              IFILWD=2
              GOTO1190
            ENDIF
 1121     CONTINUE
        ENDIF
      ENDIF
C
      IF(NUMARG.GT.1)THEN
        IWORD=3
        ISTART=1
        ISTOP=MAXSTR
        CALL DPEXWO(ISTRIN,ISTART,ISTOP,IWORD,
     1              ICOL1,ICOL2,ISTRI2,NCSTR2,
     1              IBUGS2,ISUBRO,IERROR)
        IF(NCSTR2.GT.0)THEN
          DO1131I=1,NCSTR2
            IF(ISTRI2(I:I).EQ.'.')THEN
              IFILWD=3
              GOTO1190
            ENDIF
 1131    CONTINUE
        ENDIF
      ENDIF
C
 1190 CONTINUE
      ISTAM1=0
      IF(IFILWD.EQ.2.OR.IFILWD.EQ.3)ISTAM1=1
C
C               *******************************
C               **  STEP 12--                **
C               **  COPY OVER FILE VARIABLES **
C               *******************************
C
      ISTEPN='12'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SACO')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ISACNC=ISACNA
      IF(IFILWD.EQ.2.OR.IFILWD.EQ.3)ISACNC=ISTRI2
C
      IOUNIT=ISACNU
      IFILE=ISACNA
      IF(IFILWD.EQ.2.OR.IFILWD.EQ.3)IFILE=ISTRI2(1:256)
      ISTAT=ISACST
      IFORM=ISACFO
      IACCES=ISACAC
      IPROT=ISACPR
      ICURST=ISACCS
C
      ISUBN0='SACO'
      IERRFI='NO'
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SACO')THEN
        WRITE(ICOUT,1294)IFILE
 1294   FORMAT('IFILE = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1295)ISTAT,IFORM,IACCES,IPROT,ICURST
 1295   FORMAT('ISTAT,IFORM,IACCES,IPROT,ICURST = ',4(A12,2X),A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1296)ISUBN0,IERRFI,IOUNIT
 1296   FORMAT('ISUBN0,IERRFI,IOUNIT = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***********************************************************
C               **  STEP 13--                                            **
C               **  CHECK TO SEE IF THE SAVE-CONCLUSIONS FILE MAY EXIST  **
C               ***********************************************************
C
      ISTEPN='13'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SACO')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ISTAT.EQ.'NONE')THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1311)
 1311   FORMAT('***** IMPLEMENTATION ERROR IN DPSACO--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1312)
 1312   FORMAT('      THE DESIRED SAVING OF COMMANDS CANNOT BE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1314)
 1314   FORMAT('      OUT BECAUSE THE INTERNAL VARIABLE    ISACST ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1315)
 1315   FORMAT('      WHICH ALLOWS SUCH COMMAND-SAVINGING')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1316)
 1316   FORMAT('      HAS BEEN SET TO    NONE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1317)ISTAT,ISACST
 1317   FORMAT('ISTAT,ISACST = ',A12,2X,A12)
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
C               *********************************************************
C               **  STEP 21--                                          **
C               **  FROM THE RECALL-LIST OF THE PREVIOUS 30 COMMANDS,  **
C               **  STRIP OUT THE DESIRED COMMAND LINE NUMBERS         **
C               **  THE LIST THAT THE ANALYST HAS SPECIFIED            **
C               **  SHOULD BE IN THE ORDER THAT THE ANALYST            **
C               **  WANTS THE COMMANDS EXECUTED                        **
C               **  (USUALLY--BUT NOT NECESSARILY--IT IS FROM LARGEST  **
C               **  (MOST DISTANT) TO SMALLEST (MOST RECENT))          **
C               *********************************************************
C
CCCCC THE FOLLOWING LINE WAS CHANGED APRIL 1993
CCCCC MAXTAB=IREPMX
      MAXTAB=MAXLIS
      MININT=1
CCCCC THE FOLLOWING LINE WAS CHANGED APRIL 1993
CCCCC MAXINT=IREPMX
      MAXINT=MAXLIS
      ISTART=ISTAM1+1
      ISTOP=NUMARG
      IF(ISTART.GT.ISTOP)THEN
        I=1
        ITAB(I)=1
        NTAB=I
      ELSE
        CALL DPEXIN(IHARG,IARGT,IARG,NUMARG,ISTART,ISTOP,
     1              MININT,MAXINT,
     1              ITAB,NTAB,MAXTAB,
     1              IBUGS2,ISUBRO,IERROR)
      ENDIF
C
C               **************************
C               **  STEP 31--           **
C               **  OPEN  THE FILE      **
C               **************************
C
      ISTEPN='31'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SACO')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IREWIN='ON'
      CALL DPOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
      IF(IERRFI.EQ.'YES')GOTO9000
C
C               ******************************************************
C               **  STEP 41--                                       **
C               **  PRINT OUT THE SPECIFIED COMMANDS                **
C               **  (BOTH TO SCREEN AND TO FILE)                    **
C               **  IN ORDER OF EXECUTION                           **
C               ******************************************************
C
      ISTEPN='41'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SACO')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4101)
 4101   FORMAT('THE SAVED COMMAND LINES--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      NMAX=80
      DO4110I=1,NTAB
        I2=ITAB(I)
        I3=IPOINT-I2
CCCCC   THE FOLLOWING LINE WAS CHANGED APRIL 1993
CCCCC   IF(I3.LE.0)I3=I3+IREPMX
        IF(I3.LE.0)I3=I3+MAXLIS
        DO4120J=1,MAXSTR
          IC1=IANSSV(I3,J)
          ISTRIN(J:J)=IC1
 4120   CONTINUE
        WRITE(IOUNIT,4125)(ISTRIN(J:J),J=1,80)
 4125   FORMAT(80A1)
C
        IF(IFEEDB.EQ.'ON')THEN
          CALL DPDB80(ISTRIN,J2MAX,NMAX,IBUGS2,ISUBRO,IERROR)
          WRITE(ICOUT,4126)I2,(ISTRIN(J:J),J=1,MIN(80,J2MAX))
 4126     FORMAT(4X,I2,'--',80A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
 4110 CONTINUE
C
C               **************************
C               **  STEP 51--           **
C               **  CLOSE THE FILE      **
C               **************************
C
      ISTEPN='51'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SACO')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IENDFI='OFF'
      IREWIN='OFF'
      CALL DPCLFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1IENDFI,IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SACO')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('AT THE END       OF DPSACO--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGS2,ISUBRO,IFOUND,IERROR
 9012   FORMAT('IBUGS2,ISUBRO,IFOUND,IERROR = ',A4,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)MAXLIS,IPOINT,IOUNIT
 9021   FORMAT('MAXLIS,IPOINT,IOUNIT = ',3I8)
        CALL DPWRST('XXX','BUG ')
        DO9022J=1,IREPMX
          WRITE(ICOUT,9023)J,(IANSSV(J,I),I=1,80)
 9023     FORMAT('J,(IANSSV(J,I),I=1,80) = ',I8,2X,80A1)
          CALL DPWRST('XXX','BUG ')
 9022   CONTINUE
        WRITE(ICOUT,9042)IFILE(1:80)
 9042   FORMAT('IFILE  = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9043)ISTAT,IFORM,IACCES,IPROT,ICURST
 9043   FORMAT('ISTAT,IFORM,IACCES,IPROT,ICURST  = ',4(A12,2X),A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9048)IENDFI,IREWIN,ISUBN0,IERRFI
 9048   FORMAT('IENDFI,IREWIN,ISUBN0,IERRFI = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9061)IFILWD,ISTAM1,ISTART,ISTOP
 9061   FORMAT('IFILWD,ISTAM1,ISTART,ISTOP = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9063)MININT,MAXINT,NTAB,MAXTAB
 9063   FORMAT('MININT,MAXINT,NTAB,MAXTAB = ',4I8)
        CALL DPWRST('XXX','BUG ')
        IF(NTAB.GT.0)THEN
          DO9072I=1,NTAB
            WRITE(ICOUT,9073)I,ITAB(I)
 9073       FORMAT('I,ITAB(I) = ',2I8)
            CALL DPWRST('XXX','BUG ')
 9072     CONTINUE
        ENDIF
        WRITE(ICOUT,9081)ISACNC(1:80)
 9081   FORMAT('ISACNC = ',A80)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPSAPC(IBUGS2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--GUI SAVE PLOT CONTROL (= LIST OUT PLOT CONTROL
C              SETTINGS TO SCREEN SO TCL/TK CAN READ THEM.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--97/11
C     ORIGINAL VERSION--NOVEMBER  1997.
C     UPDATED         --JULY      2009. MODIFY SOME FORMATS FOR
C                                       GUI
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
      CHARACTER*4 ITEMP
      CHARACTER*4 ITITFL
      CHARACTER*4 ILABFL
      CHARACTER*4 ILEGFL
      CHARACTER*4 ILINFL
      CHARACTER*4 ICHAFL
      CHARACTER*4 ISPIFL
      CHARACTER*4 IBARFL
      CHARACTER*4 IBACFL
      CHARACTER*4 ILIMFL
C
      CHARACTER*4 ITMP1
      CHARACTER*4 ITMP2
      CHARACTER*4 ITMP3
      CHARACTER*4 ITMP4
C
      CHARACTER*24 ITEMPH(10)
C
      REAL TEMP(100)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOPC.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPSA'
      ISUBN2='PC  '
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGS2.EQ.'OFF'.AND.ISUBRO.NE.'SAPC')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPSAPC--')
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
   90 CONTINUE
C
      ISTART=1
      ISTOP=100
      ITITFL='OFF'
      ILABFL='OFF'
      ILEGFL='OFF'
      ILINFL='OFF'
      ICHAFL='OFF'
      ISPIFL='OFF'
      IBARFL='OFF'
      IBACFL='OFF'
      ILIMFL='OFF'
C
      IJUNK1=NUMARG
      IJUNK2=NUMARG-1
      IJUNK3=NUMARG-2
      IF(NUMARG.GE.2.AND.IARGT(IJUNK1).EQ.'NUMB'.AND.
     1  IARGT(IJUNK2).EQ.'NUMB')THEN
         ISTART=IARG(IJUNK2)
         ISTOP=IARG(IJUNK1)
         IF(ISTART.LT.1)ISTART=1
         IF(ISTOP.GT.100)ISTOP=100
         IF(ISTART.GT.ISTOP)THEN
           IJUNK4=ISTOP
           ISTOP=ISTART
           ISTART=IJUNK4
         ENDIF
      ELSEIF(NUMARG.GE.2.AND.IARGT(IJUNK1).EQ.'NUMB'.AND.
     1  IARGT(IJUNK2).NE.'NUMB')THEN
        ISTART=1
        ISTOP=IARG(IJUNK1)
        IF(ISTOP.GT.100)ISTOP=100
        IJUNK3=IJUNK2
      ELSE
        IJUNK3=IJUNK1
      ENDIF
C
      IF(IJUNK3.GE.1)THEN
        ITEMP=IHARG(IJUNK3)
        IF(ITEMP.EQ.'TITL')ITITFL='ON'
        IF(ITEMP.EQ.'LABE')ILABFL='ON'
        IF(ITEMP.EQ.'LEGE')ILEGFL='ON'
        IF(ITEMP.EQ.'LINE')ILINFL='ON'
        IF(ITEMP.EQ.'CHAR')ICHAFL='ON'
        IF(ITEMP.EQ.'SPIK')ISPIFL='ON'
        IF(ITEMP.EQ.'BAR ')IBARFL='ON'
        IF(ITEMP.EQ.'BACK')IBACFL='ON'
        IF(ITEMP.EQ.'LIMI')ILIMFL='ON'
        IF(ITEMP.EQ.'ALL ')THEN
          ITITFL='ON'
          ILABFL='ON'
          ILEGFL='ON'
          ILINFL='ON'
          ICHAFL='ON'
          ISPIFL='ON'
          IBARFL='ON'
          IBACFL='ON'
          ILIMFL='ON'
        ENDIF
      ENDIF
C
      LINC=5
C
C               ******************************************************
C               **  STEP 41--
C               **  WRITE OUT TO THE SAVE FILE;
C               ******************************************************
C
      ISTEPN='41'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SAPC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     -----WRITE OUT COMMON FOR PLOT CONTROL-----
C
      IF(IBACFL.EQ.'OFF')GOTO199
      WRITE(ICOUT,101)IBACCO
  101 FORMAT('BACKGROUND COLOR = ',A4)
      CALL DPWRST('XXX','BUG ')
  199 CONTINUE
C
      IF(ITITFL.EQ.'OFF')GOTO299
      WRITE(ICOUT,201)
  201 FORMAT('TITLE ATTRIBUTES')
      CALL DPWRST('XXX','BUG ')
C
      WRITE(ICOUT,202)(ITITTE(I)(1:1),I=1,MIN(NCTITL,130))
  202 FORMAT('   TITLE = ',130A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,203)ITITFO
  203 FORMAT('   TITLE FONT = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,204)ITITCA
  204 FORMAT('   TITLE CASE = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,205)ITITFI
  205 FORMAT('   TITLE FILL = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,206)ITITCO
  206 FORMAT('   TITLE COLOR = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,207)PTITHE
  207 FORMAT('   TITLE SIZE = ',E12.5)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,208)PTITTH
  208 FORMAT('   TITLE THICKNESS = ',E12.5)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,209)PTITDS
  209 FORMAT('   TITLE DISPLACEMENT = ',E12.5)
      CALL DPWRST('XXX','BUG ')
  299 CONTINUE
C
      IF(ILABFL.EQ.'OFF')GOTO399
      WRITE(ICOUT,301)
  301 FORMAT('AXIS LABEL ATTRIBUTES')
      CALL DPWRST('XXX','BUG ')
C
      WRITE(ICOUT,311)(IX1LTE(I)(1:1),I=1,NCX1LA)
  311 FORMAT('    X1LABEL = ',130A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,312)(IX2LTE(I)(1:1),I=1,NCX2LA)
  312 FORMAT('    X2LABEL = ',130A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,313)(IX3LTE(I)(1:1),I=1,NCX3LA)
  313 FORMAT('    X3LABEL = ',130A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,314)(IY1LTE(I)(1:1),I=1,NCY1LA)
  314 FORMAT('    Y1LABEL = ',130A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,315)(IY2LTE(I)(1:1),I=1,NCY2LA)
  315 FORMAT('    Y2LABEL = ',130A1)
      CALL DPWRST('XXX','BUG ')
C
      WRITE(ICOUT,321)IX1LFO
  321 FORMAT('   X1LABEL FONT = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,322)IX1LCA
  322 FORMAT('   X1LABEL CASE = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,323)IX1LFI
  323 FORMAT('   X1LABEL FILL = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,324)IX1LCO
  324 FORMAT('   X1LABEL COLOR = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,325)PX1LDS
  325 FORMAT('   X1LABEL DISPLACEMENT = ',E12.5)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,326)PX1LHE
  326 FORMAT('   X1LABEL SIZE = ',E12.5)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,327)PX1LTH
  327 FORMAT('   X1LABEL THICKNESS = ',E12.5)
      CALL DPWRST('XXX','BUG ')
C
      WRITE(ICOUT,331)IX2LFO
  331 FORMAT('   X2LABEL FONT = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,332)IX2LCA
  332 FORMAT('   X2LABEL CASE = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,333)IX2LFI
  333 FORMAT('   X2LABEL FILL = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,334)IX2LCO
  334 FORMAT('   X2LABEL COLOR = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,335)PX2LDS
  335 FORMAT('   X2LABEL DISPLACEMENT = ',E12.5)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,336)PX2LHE
  336 FORMAT('   X2LABEL SIZE = ',E12.5)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,337)PX2LTH
  337 FORMAT('   X2LABEL THICKNESS = ',E12.5)
      CALL DPWRST('XXX','BUG ')
C
      WRITE(ICOUT,341)IX3LFO
  341 FORMAT('   X3LABEL FONT = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,342)IX3LCA
  342 FORMAT('   X3LABEL CASE = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,343)IX3LFI
  343 FORMAT('   X3LABEL FILL = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,344)IX3LCO
  344 FORMAT('   X3LABEL COLOR = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,345)PX3LDS
  345 FORMAT('   X3LABEL DISPLACEMENT = ',E12.5)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,346)PX3LHE
  346 FORMAT('   X3LABEL SIZE = ',E12.5)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,347)PX3LTH
  347 FORMAT('   X3LABEL THICKNESS = ',E12.5)
      CALL DPWRST('XXX','BUG ')
C
      WRITE(ICOUT,351)IY1LFO
  351 FORMAT('   Y1LABEL FONT = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,352)IY1LCA
  352 FORMAT('   Y1LABEL CASE = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,353)IY1LFI
  353 FORMAT('   Y1LABEL FILL = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,354)IY1LCO
  354 FORMAT('   Y1LABEL COLOR = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,355)PY1LDS
  355 FORMAT('   Y1LABEL DISPLACEMENT = ',E12.5)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,356)PY1LHE
  356 FORMAT('   Y1LABEL SIZE = ',E12.5)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,357)PY1LTH
  357 FORMAT('   Y1LABEL THICKNESS = ',E12.5)
      CALL DPWRST('XXX','BUG ')
C
      WRITE(ICOUT,361)IY2LFO
  361 FORMAT('   Y2LABEL FONT = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,362)IY2LCA
  362 FORMAT('   Y2LABEL CASE = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,363)IY2LFI
  363 FORMAT('   Y2LABEL FILL = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,364)IY2LCO
  364 FORMAT('   Y2LABEL COLOR = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,365)PY2LDS
  365 FORMAT('   Y2LABEL DISPLACEMENT = ',E12.5)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,366)PY2LHE
  366 FORMAT('   Y2LABEL SIZE = ',E12.5)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,367)PY2LTH
  367 FORMAT('   Y2LABEL THICKNESS = ',E12.5)
      CALL DPWRST('XXX','BUG ')
  399 CONTINUE
C
      IF(ILEGFL.EQ.'OFF')GOTO499
      WRITE(ICOUT,401)
  401 FORMAT('LEGEND ATTRIBUTES')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,402)NUMLEG
  402 FORMAT('    NUMBER OF CURRENTLY DEFINED LEGENDS = ',I10)
      CALL DPWRST('XXX','BUG ')
C
      DO491LL=1,20 
      LSTRT=(LL-1)*LINC+1
      LSTOP=LL*LINC
      IF(LSTRT.GT.NUMLEG)GOTO498
      IF(LSTOP.GT.NUMLEG)LSTOP=NUMLEG
C
      DO490L=LSTRT,LSTOP
      ISTRT=ILEGST(L)
      ISTP=ILEGSP(L)
      IF(ISTP-ISTRT+1.GT.80)ISTP=ISTRT+79
      WRITE(ICOUT,411)L,L,(ILEGTE(J)(1:1),J=ISTRT,ISTP)
  411 FORMAT('    LEGEND ',2I5,' = ',80A1)
      CALL DPWRST('XXX','BUG ')
  490 CONTINUE
  491 CONTINUE
  498 CONTINUE
C
      DO492LL=1,20
      LSTRT=(LL-1)*LINC+1
      LSTOP=LL*LINC
      IF(LSTRT.GT.ISTOP)GOTO492
      IF(LSTRT.LT.ISTART)LSTRT=ISTART
      IF(LSTOP.GT.ISTOP)LSTOP=ISTOP
C
      WRITE(ICOUT,412)LSTRT,LSTOP,(ILEGFO(I),I=LSTRT,LSTOP)
  412 FORMAT('    LEGEND FONT ',I5,1X,I5,' = ',10(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,413)LSTRT,LSTOP,(ILEGCA(I),I=LSTRT,LSTOP)
  413 FORMAT('    LEGEND CASE ',I5,1X,I5,' = ',10(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,414)LSTRT,LSTOP,(ILEGJU(I),I=LSTRT,LSTOP)
  414 FORMAT('    LEGEND JUSTIFICATION ',I5,1X,I5,' = ',10(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,415)LSTRT,LSTOP,(ILEGDI(I),I=LSTRT,LSTOP)
  415 FORMAT('    LEGEND DIRECTION ',I5,1X,I5,' = ',10(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,416)LSTRT,LSTOP,(ILEGFI(I),I=LSTRT,LSTOP)
  416 FORMAT('    LEGEND FILL ',I5,1X,I5,' = ',10(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,417)LSTRT,LSTOP,(ILEGCO(I),I=LSTRT,LSTOP)
  417 FORMAT('    LEGEND COLOR ',I5,1X,I5,' = ',10(A4,1X))
      CALL DPWRST('XXX','BUG ')
C
      DO1418I=LSTRT,LSTOP
      WRITE(ICOUT,418)I,I,PLEGXC(I),PLEGYC(I)
  418 FORMAT('    LEGEND COORDINATES ',I5,1X,I5,' = ',2(E12.5,1X))
      CALL DPWRST('XXX','BUG ')
 1418 CONTINUE
C
      WRITE(ICOUT,419)LSTRT,LSTOP,(PLEGHE(I),I=LSTRT,LSTOP)
  419 FORMAT('    LEGEND SIZE  ',I5,1X,I5,' = ',10(E12.5,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,420)LSTRT,LSTOP,(PLEGWI(I),I=LSTRT,LSTOP)
  420 FORMAT('    LEGEND WIDTH ',I5,1X,I5,' = ',10(E12.5,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,421)LSTRT,LSTOP,(PLEGTH(I),I=LSTRT,LSTOP)
  421 FORMAT('    LEGEND THICKNESS ',I5,1X,I5,' = ',10(E12.5,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,422)LSTRT,LSTOP,(ALEGAN(I),I=LSTRT,LSTOP)
  422 FORMAT('    LEGEND ANGLE ',I5,1X,I5,' = ',10(E12.5,1X))
      CALL DPWRST('XXX','BUG ')
  492 CONTINUE
C
  499 CONTINUE
C
      DO1990LL=1,20
      LSTRT=(LL-1)*LINC+1
      LSTOP=LL*LINC
      IF(LSTRT.GT.ISTOP)GOTO1999
      IF(LSTRT.LT.ISTART)LSTRT=ISTART
      IF(LSTOP.GT.ISTOP)LSTOP=ISTOP
C
      IF(ILINFL.EQ.'OFF')GOTO599
CCCCC WRITE(ICOUT,501)
CC501 FORMAT('LINE ATTRIBUTES')
CCCCC CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,512)LSTRT,LSTOP,(ILINPA(I),I=LSTRT,LSTOP)
  512 FORMAT('    LINE ',I5,1X,I5,' = ',10(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,513)LSTRT,LSTOP,(ILINCO(I),I=LSTRT,LSTOP)
  513 FORMAT('    LINE COLOR ',I5,1X,I5,' = ',10(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,514)LSTRT,LSTOP,(PLINTH(I),I=LSTRT,LSTOP)
  514 FORMAT('    LINE THICKNESS ',I5,1X,I5,' = ',10(E12.5,1X))
      CALL DPWRST('XXX','BUG ')
  599 CONTINUE
C
      IF(ICHAFL.EQ.'OFF')GOTO699
CCCCC WRITE(ICOUT,601)
CC601 FORMAT('CHARACTER ATTRIBUTES')
CCCCC CALL DPWRST('XXX','BUG ')
      ICOUNT=0
      DO601I=LSTRT,LSTOP
         ICOUNT=ICOUNT+1
         ITEMPH(ICOUNT)='BLAN'
         IF(ICHAPA(I).NE.'    ')ITEMPH(ICOUNT)=ICHAPA(I)
  601 CONTINUE
C
      WRITE(ICOUT,611)LSTRT,LSTOP,(ITEMPH(I),I=1,ICOUNT)
  611 FORMAT('    CHARACTER ',I5,1X,I5,' = ',10(A16,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,612)LSTRT,LSTOP,(ICHAFO(I),I=LSTRT,LSTOP)
  612 FORMAT('   CHARACTER FONT ',I5,1X,I5,' = ',10(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,613)LSTRT,LSTOP,(ICHACO(I),I=LSTRT,LSTOP)
  613 FORMAT('   CHARACTER COLOR ',I5,1X,I5,' = ',10(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,614)LSTRT,LSTOP,(ICHACA(I),I=LSTRT,LSTOP)
  614 FORMAT('   CHARACTER CASE ',I5,1X,I5,' = ',10(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,615)LSTRT,LSTOP,(ICHAJU(I),I=LSTRT,LSTOP)
  615 FORMAT('   CHARACTER JUSTIFICATION ',I5,1X,I5,' = ',10(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,616)LSTRT,LSTOP,(ICHADI(I),I=LSTRT,LSTOP)
  616 FORMAT('   CHARACTER DIRECTION ',I5,1X,I5,' = ',10(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,617)LSTRT,LSTOP,(ICHAFI(I),I=LSTRT,LSTOP)
  617 FORMAT('   CHARACTER FILL ',I5,1X,I5,' = ',10(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,618)LSTRT,LSTOP,(PCHAHE(I),I=LSTRT,LSTOP)
  618 FORMAT('   CHARACTER SIZE ',I5,1X,I5,' = ',10(E12.5,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,619)LSTRT,LSTOP,(PCHAWI(I),I=LSTRT,LSTOP)
  619 FORMAT('   CHARACTER WIDTH ',I5,1X,I5,' = ',10(E12.5,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,620)LSTRT,LSTOP,(ACHAAN(I),I=LSTRT,LSTOP)
  620 FORMAT('   CHARACTER ANGLE ',I5,1X,I5,' = ',10(E12.5,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,621)LSTRT,LSTOP,(PCHATH(I),I=LSTRT,LSTOP)
  621 FORMAT('   CHARACTER THICKNESS ',I5,1X,I5,' = ',10(E12.5,1X))
      CALL DPWRST('XXX','BUG ')
  699 CONTINUE
C
      IF(ISPIFL.EQ.'OFF')GOTO799
CCCCC WRITE(ICOUT,701)
CC701 FORMAT('SPIKE ATTRIBUTES')
CCCCC CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,711)LSTRT,LSTOP,(ISPISW(I),I=LSTRT,LSTOP)
  711 FORMAT('   SPIKE ',I5,1X,I5,' = ',10(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,712)LSTRT,LSTOP,(ISPILI(I),I=LSTRT,LSTOP)
  712 FORMAT('   SPIKE LINE ',I5,1X,I5,' = ',10(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,713)LSTRT,LSTOP,(ISPICO(I),I=LSTRT,LSTOP)
  713 FORMAT('   SPIKE COLOR ',I5,1X,I5,' = ',10(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,714)LSTRT,LSTOP,(PSPITH(I),I=LSTRT,LSTOP)
  714 FORMAT('   SPIKE THICKNESS ',I5,1X,I5,' = ',10(E12.5,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,715)LSTRT,LSTOP,(ASPIBA(I),I=LSTRT,LSTOP)
  715 FORMAT('   SPIKE BASE ',I5,1X,I5,' = ',10(E12.5,1X))
      CALL DPWRST('XXX','BUG ')
  799 CONTINUE
C
      IF(IBARFL.EQ.'OFF')GOTO899
CCCCC WRITE(ICOUT,801)
CC801 FORMAT('BAR ATTRIBUTES')
CCCCC CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,811)LSTRT,LSTOP,(IBARSW(I),I=LSTRT,LSTOP)
  811 FORMAT('   BAR ',I5,1X,I5,' = ',10(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,812)LSTRT,LSTOP,(ABARBA(I),I=LSTRT,LSTOP)
  812 FORMAT('   BAR BASE ',I5,1X,I5,' ',' = ',10(E12.5,1X))
      CALL DPWRST('XXX','BUG ')
C
C     NOTE JULY 2009: FOR GUI, IF VALUE SET TO CPUMIN, THEN
C                     SET TO -99.
C
      DO8813I=1,100
        IF(ABARWI(I).LT.-99.0)THEN
          TEMP(I)=-99.0
        ELSE
          TEMP(I)=ABARWI(I)
        ENDIF
 8813 CONTINUE
CCCCC WRITE(ICOUT,813)LSTRT,LSTOP,(ABARWI(I),I=LSTRT,LSTOP)
      WRITE(ICOUT,813)LSTRT,LSTOP,(TEMP(I),I=LSTRT,LSTOP)
  813 FORMAT('   BAR WIDTH ',I5,1X,I5,' = ',10(E12.5,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,821)LSTRT,LSTOP,(IBABLI(I),I=LSTRT,LSTOP)
  821 FORMAT('   BAR BORDER LINE ',I5,1X,I5,' = ',10(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,822)LSTRT,LSTOP,(IBABCO(I),I=LSTRT,LSTOP)
  822 FORMAT('   BAR BORDER COLOR ',I5,1X,I5,' = ',10(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,823)LSTRT,LSTOP,(PBABTH(I),I=LSTRT,LSTOP)
  823 FORMAT('   BAR BORDER THICKNESS ',I5,1X,I5,' = ',10(E12.5,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,831)LSTRT,LSTOP,(IBAFSW(I),I=LSTRT,LSTOP)
  831 FORMAT('   BAR FILL ',I5,1X,I5,' = ',10(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,832)LSTRT,LSTOP,(IBAFCO(I),I=LSTRT,LSTOP)
  832 FORMAT('   BAR FILL COLOR ',I5,1X,I5,' = ',10(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,841)LSTRT,LSTOP,(IBAPTY(I),I=LSTRT,LSTOP)
  841 FORMAT('   BAR PATTERN ',I5,1X,I5,' = ',10(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,842)LSTRT,LSTOP,(IBAPLI(I),I=LSTRT,LSTOP)
  842 FORMAT('   BAR PATTERN LINE ',I5,1X,I5,' = ',10(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,843)LSTRT,LSTOP,(IBAPCO(I),I=LSTRT,LSTOP)
  843 FORMAT('   BAR PATTERN COLOR ',I5,1X,I5,' = ',10(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,844)LSTRT,LSTOP,(PBABTH(I),I=LSTRT,LSTOP)
  844 FORMAT('   BAR PATTERN THICKNESS ',I5,1X,I5,' = ',10(E12.5,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,845)LSTRT,LSTOP,(PBAPSP(I),I=LSTRT,LSTOP)
  845 FORMAT('   BAR PATTERN SPACING ',I5,1X,I5,' = ',10(E12.5,1X))
      CALL DPWRST('XXX','BUG ')
  899 CONTINUE
C
 1990 CONTINUE
 1999 CONTINUE
C
      IF(ILIMFL.EQ.'OFF')GOTO990
      WRITE(ICOUT,901)
  901 FORMAT('LIMIT AND TIC MARK ATTRIBUTES')
      CALL DPWRST('XXX','BUG ')
C
C     NOTE JULY 2009: FOR GUI, IF VALUE SET TO CPUMIN, THEN
C                     SET TO -99.
C
      ATEMP1=FX1MNZ
      ATEMP2=FX1MXZ
      IF(FX1MNZ.LT.-99)ATEMP1=-99.0
      IF(FX1MXZ.LT.-99)ATEMP1=-99.0
CCCCC WRITE(ICOUT,902)FX1MNZ,FX1MXZ
      WRITE(ICOUT,902)ATEMP1,ATEMP2
  902 FORMAT('    X1 LIMITS = ',E12.5,1X,E15.7)
      CALL DPWRST('XXX','BUG ')
      ATEMP1=FX2MNZ
      ATEMP2=FX2MXZ
      IF(FX2MNZ.LT.-99)ATEMP1=-99.0
      IF(FX2MXZ.LT.-99)ATEMP1=-99.0
CCCCC WRITE(ICOUT,904)FX2MNZ,FX2MXZ
      WRITE(ICOUT,904)ATEMP1,ATEMP2
  904 FORMAT('    X2 LIMITS = ',E12.5,1X,E15.7)
      CALL DPWRST('XXX','BUG ')
      ATEMP1=FY1MNZ
      ATEMP2=FY1MXZ
      IF(FY1MNZ.LT.-99)ATEMP1=-99.0
      IF(FY1MXZ.LT.-99)ATEMP1=-99.0
CCCCC WRITE(ICOUT,906)FY1MNZ,FY1MXZ
      WRITE(ICOUT,906)ATEMP1,ATEMP2
  906 FORMAT('    Y1 LIMITS = ',E12.5,1X,E15.7)
      CALL DPWRST('XXX','BUG ')
      ATEMP1=FY2MNZ
      ATEMP2=FY2MXZ
      IF(FY2MNZ.LT.-99)ATEMP1=-99.0
      IF(FY2MXZ.LT.-99)ATEMP1=-99.0
CCCCC WRITE(ICOUT,908)FY2MNZ,FY2MXZ
      WRITE(ICOUT,908)ATEMP1,ATEMP2
  908 FORMAT('    Y2 LIMITS = ',E12.5,1X,E15.7)
      CALL DPWRST('XXX','BUG ')
C
      WRITE(ICOUT,911)IX1FSW,IX2FSW,IY1FSW,IY2FSW
  911 FORMAT('    X1, X2, Y1, Y2 FRAME = ',4(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1911)IX1FPA,IX2FPA,IY1FPA,IY2FPA
 1911 FORMAT('    X1, X2, Y1, Y2 FRAME PATTERN = ',4(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1912)IX1FCO,IX2FCO,IY1FCO,IY2FCO
 1912 FORMAT('    X1, X2, Y1, Y2 FRAME COLOR = ',4(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1913)PFRATH
 1913 FORMAT('    FRAME THICKNESS = ',E12.5)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1915)PXMIN,PXMAX,PYMIN,PYMAX
 1915 FORMAT('    FRAME COORDINATES = ',4E12.5)
      CALL DPWRST('XXX','BUG ')
C
      WRITE(ICOUT,912)IVGRSW,IHGRSW
  912 FORMAT('    X, Y GRID = ',2(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,913)IVGRPA,IHGRPA
  913 FORMAT('    X, Y GRID PATTERN = ',2(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,914)IVGRCO,IHGRCO
  914 FORMAT('    X, Y GRID COLOR = ',2(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,915)PVGRTH,PHGRTH
  915 FORMAT('    X, Y GRID THICKNESS = ',2(E12.5,1X))
      CALL DPWRST('XXX','BUG ')
C
      WRITE(ICOUT,921)IX1TSW,IX2TSW,IY1TSW,IY2TSW
  921 FORMAT('    X1, X2, Y1, Y2 TIC = ',4(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,922)IX1TJU,IX2TJU,IY1TJU,IY2TJU
  922 FORMAT('    X1, X2, Y1, Y2 TIC POSITION = ',4(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,923)IX1TCO,IX2TCO,IY1TCO,IY2TCO
  923 FORMAT('    X1, X2, Y1, Y2 TIC COLOR = ',4(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,924)PX1TLE,PX2TLE,PY1TLE,PY2TLE
  924 FORMAT('    X1, X2, Y1, Y2 TIC SIZE = ',4(E12.5,1X))
      CALL DPWRST('XXX','BUG ')
      ITMP1='OFF'
      ITMP2='OFF'
      ITMP3='OFF'
      ITMP4='OFF'
      IF(IX1TSC.EQ.'LOG')ITMP1='ON'
      IF(IX2TSC.EQ.'LOG')ITMP2='ON'
      IF(IY1TSC.EQ.'LOG')ITMP3='ON'
      IF(IY2TSC.EQ.'LOG')ITMP4='ON'
      WRITE(ICOUT,925)ITMP1,ITMP2,ITMP3,ITMP4
  925 FORMAT('    X1, X2, Y1, Y2 LOG = ',4(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,931)PX1TOL,PX1TOR
  931 FORMAT('    X1 TIC OFFSET = ',2(E12.5,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,932)PX2TOL,PX2TOR
  932 FORMAT('    X2 TIC OFFSET = ',2(E12.5,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,933)PY1TOB,PY1TOT
  933 FORMAT('    Y1 TIC OFFSET = ',2(E12.5,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,934)PY2TOB,PY2TOT
  934 FORMAT('    Y2 TIC OFFSET = ',2(E12.5,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,935)ITICUN
  935 FORMAT('    TIC OFFSET UNITS = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,941)NMJX1T,NMJX2T,NMJY1T,NMJY2T
  941 FORMAT('    X1, X2, Y1, Y2 TIC NUMBER MAJOR = ',4(I5,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,942)NMNX1T,NMNX2T,NMNY1T,NMNY2T
  942 FORMAT('    X1, X2, Y1, Y2 TIC NUMBER MINOR = ',4(I5,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,951)IX1ZSW,IX2ZSW,IY1ZSW,IY2ZSW
  951 FORMAT('    X1, X2, Y1, Y2 TIC LABEL = ',4(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,952)IX1ZCO,IX2ZCO,IY1ZCO,IY2ZCO
  952 FORMAT('    X1, X2, Y1, Y2 TIC LABEL COLOR = ',4(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,953)IX1ZCA,IX2ZCA,IY1ZCA,IY2ZCA
  953 FORMAT('    X1, X2, Y1, Y2 TIC LABEL CASE = ',4(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,954)IX1ZFO,IX2ZFO,IY1ZFO,IY2ZFO
  954 FORMAT('    X1, X2, Y1, Y2 TIC LABEL FONT = ',4(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,955)IX1ZJU,IX2ZJU,IY1ZJU,IY2ZJU
  955 FORMAT('    X1, X2, Y1, Y2 TIC LABEL JUSTIFICATION = ',
     14(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,956)IX1ZDI,IX2ZDI,IY1ZDI,IY2ZDI
  956 FORMAT('    X1, X2, Y1, Y2 TIC LABEL DIRECTION = ',
     14(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,957)IX1ZFI,IX2ZFI,IY1ZFI,IY2ZFI
  957 FORMAT('    X1, X2, Y1, Y2 TIC LABEL FILL = ',4(A4,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,958)IX1ZDP,IX2ZDP,IY1ZDP,IY2ZDP
  958 FORMAT('    X1, X2, Y1, Y2 TIC LABEL DECIMALS = ',4(I5,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,959)PX1ZDS,PX2ZDS,PY1ZDS,PY2ZDS
  959 FORMAT('    X1, X2, Y1, Y2 TIC LABEL DISPLACEMENT = ',
     14(E12.5,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,960)AX1ZAN,AX2ZAN,AY1ZAN,AY2ZAN
  960 FORMAT('    X1, X2, Y1, Y2 TIC LABEL ANGLE = ',4(E12.5,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,961)PX1ZHE,PX2ZHE,PY1ZHE,PY2ZHE
  961 FORMAT('    X1, X2, Y1, Y2 TIC LABEL SIZE = ',4(E12.5,1X))
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,971)PTIZTH
  971 FORMAT('    TIC LABEL THICKNESS = ',4(E12.5,1X))
      CALL DPWRST('XXX','BUG ')
C
  990 CONTINUE
C
C     -----END WRITING OUT-----------------------
C
C               ***************************
C               **  STEP 42--            **
C               **  WRITE OUT A MESSAGE  **
C               ***************************
C
      ISTEPN='42'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SAPC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               ****************
C               **  STEP 90-- **
C               **  EXIT.     **
C               ****************
C
      RETURN
      END
      SUBROUTINE DPSAPL(IANSLC,IWIDTH,IHARG,NUMARG,
     1                  IBUGS2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--SAVE (FOR FUTURE USE BY THE REPEAT GRAPH COMMAND)
C              SELECTED PLOTS.  IT SUPPORTS THE FOLLOWING:
C
C                  SAVE PLOT <FILE NAME>:
C                      SAVES THE CURRENT PIXMAP TO THE SPECIFIED FILE
C                  SAVE PLOT AUTOMATIC <FILENAME>:
C                      AUTOMATICALLY SAVE ALL SUBSEQUENT FILES, USING
C                      <FILE NAME> AS THE BASE FILE NAME (APPEND A
C                      ".1", ".2", ETC.)
C
C     WRITTEN BY--JAMES J. FILLIBEN
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
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      INCLUDE 'DPCOPA.INC'
C
      CHARACTER*4 IANSLC(*)
      CHARACTER*4 IHARG(*)
CCCCC CHARACTER*1 IANSSV
CCCCC CHARACTER*80 ISACNC
C
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
      CHARACTER*4 IFOUND
C
      CHARACTER*4 IC4
      CHARACTER*4 ICODE
C  DIMENSION FOLLOWING 2 LINES TO MAXSTR
      CHARACTER (LEN=MAXSTR) :: ISTRIN
      CHARACTER (LEN=MAXSTR) :: ISTRI2
      CHARACTER*128 CTEMP
C
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISAVFL
C
CCCCC DIMENSION IADE(128)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPM.INC'
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPSA'
      ISUBN2='PL  '
      IFOUND='NO'
      IERROR='NO'
C
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'SAPL')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('AT THE BEGINNING OF DPSAPL--')
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
C               **  STEP 10--                                       **
C               **  DETERMINE IF HAVE SAVE PLOT AUTOMATIC CASE      **
C               ******************************************************
C
      ISTEPN='10'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SAPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ISAVFL='OFF'
      IF(NUMARG.GE.1)THEN
        DO1010I=1,NUMARG
          IF(IHARG(I).EQ.'AUTO' .OR. IHARG(I).EQ.'ON' .OR.
     1       IHARG(I).EQ.'YES' )THEN
            ISAVFL='ON'
            IPXMFL='ON'
            GOTO1019
          ENDIF
 1010   CONTINUE
 1019   CONTINUE
      ENDIF
C
C               ******************************************************
C               **  STEP 11--                                       **
C               **  DETERMINE IF HAVE AN EXPLICIT FILE REFERENCE    **
C               **  WHERE THE PIXMAPS  WILL BE SAVED, OR WILL THEY  **
C               **  BE SAVED IN THE DEFAULT FILE (PIXMAP.<n>?       **
C               ******************************************************
C
      ISTEPN='11'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SAPL')
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
      GOTO1190
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
      GOTO1190
 1139 CONTINUE
C
      IF(NUMARG.LE.2)GOTO1149
      IWORD=4
      ISTART=1
      ISTOP=MAXSTR-1
      CALL DPEXWO(ISTRIN,ISTART,ISTOP,IWORD,
     1ICOL1,ICOL2,ISTRI2,NCSTR2,
     1IBUGS2,ISUBRO,IERROR)
      IF(NCSTR2.LE.0)GOTO1149
      DO1141I=1,NCSTR2
      IF(ISTRI2(I:I).EQ.'.')GOTO1142
 1141 CONTINUE
      GOTO1149
 1142 CONTINUE
      IFILWD=4
      GOTO1190
 1149 CONTINUE
C
 1190 CONTINUE
C
      IF(ISAVFL.EQ.'ON')THEN
        IF(IFILWD.GE.1)THEN
          IPXMFB=' '
          IPXMFB(1:NCSTR2)=ISTRI2(1:NCSTR2)
          IPXMNC=NCSTR2
        ENDIF
        IF(IHARG(NUMARG).EQ.'OFF'.OR.IHARG(NUMARG).EQ.'DEFA'.OR.
     1     IHARG(NUMARG).EQ.'NO')THEN
          ISAVFL='OFF'
        ENDIF
        GOTO9000
      ENDIF
C
      NUMPXM=NUMPXM+1
      IF(NUMPXM.GT.MAXPM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1191)MAXPM
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
 1191 FORMAT('***** ERROR IN DPSAPL: MAXIMUM NUMBER OF PIXMAPS (',I5,
     1') EXCEEDED.')
C
      IF(IFILWD.LE.0)THEN
        ISTRI2=' '
        ISTRI2(1:7)='pixmap.'
        IF(NUMPXM.LE.9)THEN
          WRITE(ISTRI2(8:8),'(I1)')NUMPXM
          NCSTR2=8
        ELSEIF(NUMPXM.LE.99)THEN
          WRITE(ISTRI2(8:9),'(I2)')NUMPXM
          NCSTR2=9
        ELSEIF(NUMPXM.LE.999)THEN
          WRITE(ISTRI2(8:10),'(I3)')NUMPXM
          NCSTR2=10
        ENDIF
      ENDIF
      IPXMFN(NUMPXM)=' '
      IPXMFN(NUMPXM)(1:128)=ISTRI2(1:128)
      IF(IPXMCM(NUMPXM).EQ.' ')THEN
        IPXMCM(NUMPXM)(1:128)=IPXMFN(NUMPXM)(1:128) 
      ENDIF
C
C               *******************************
C               **  STEP 12--                **
C               **  CALL XSAVEG              **
C               *******************************
C
      ISTEPN='12'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SAPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NCSTR2.GT.127)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1209)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
 1209 FORMAT('***** ERROR IN DPSAPL--FILE NAME EXCEEDS 127 ',
     1'CHARACTERS.')
      ENDIF
c
C  AUGUST 1997.  TO MAKE CODE MORE GENERAL, CALL A LOW LEVEL
C  GRAPHICS ROUTINE.  MOVE THIS CODE TO THAT SUBROUTINE.
C
      ICODE='SAVE'
      CTEMP=' '
      NCTEMP=0
      CALL GRSAGR(ICODE,ISTRI2,NCSTR2,CTEMP,NCTEMP)
C
CCCCC DO1220I=1,NCSTR2
CCCCC   CALL DPCOAN(ISTRI2(I:I),IJUNK)
CCCCC   IADE(I)=IJUNK
C1220 CONTINUE
CCCCC IADE(NCSTR2+1)=0
C
CCCCC IERR=0
CCCCC CALL XSAVEG(IADE,IERR)
CCCCC IF(IERR.EQ.1)THEN
CCCCC   WRITE(ICOUT,999)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,1251)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   IERROR='YES'
CCCCC   GOTO9000
C1251 FORMAT('***** ERROR IN DPSAPL--WRITING BIT MAP UNSUCCESSFUL.')
CCCCC ELSEIF(IERR.EQ.2)THEN
CCCCC   WRITE(ICOUT,999)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,1261)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   IERROR='YES'
CCCCC   GOTO9000
C1261 FORMAT('***** ERROR IN DPSAPL--NO CURRENT PIXMAP TO SAVE.')
CCCCC ELSEIF(IERR.EQ.3)THEN
CCCCC   WRITE(ICOUT,999)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,1271)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   IERROR='YES'
CCCCC   GOTO9000
C1271 FORMAT('***** ERROR IN DPSAPL--X11 HAS NOT BEEN OPENED.')
CCCCC ELSEIF(IERR.EQ.4)THEN
CCCCC   WRITE(ICOUT,999)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,1281)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   IERROR='YES'
CCCCC   GOTO9000
C1281 FORMAT('***** ERROR IN DPSAPL--X11 NOT INSTALLED ON THIS ',
CCCCC1'IMPLEMENTATION.')
CCCCC ELSE
CCCCC   WRITE(ICOUT,999)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,1291)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,1292)ISTRI2(1:NCSTR2)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   IERROR='YES'
CCCCC   GOTO9000
C1291 FORMAT('***** CURRENT PIXMAP SUCCESSFULLY SAVED TO FILE ')
C1292 FORMAT('      ',A128)
CCCCC ENDIF
C
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'SAPL')THEN
        WRITE(ICOUT,1293)ISTRI2(1:MIN(128,NCSTR2))
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
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'SAPL')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('AT THE END       OF DPSAPL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGS2,ISUBRO,IFOUND,IERROR
 9012   FORMAT('IBUGS2,ISUBRO,IFOUND,IERROR = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IWIDTH,NUMARG
 9013   FORMAT('IWIDTH,NUMARG = ',2I8)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,9014)(IANSLC(I),I=1,IWIDTH)
 9014     FORMAT('(IANSLC(I),I=1,IWIDTH) = ',80A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IF(NUMARG.GE.1)THEN
          DO9016I=1,NUMARG
            WRITE(ICOUT,9017)I,IHARG(I)
 9017       FORMAT('I,IHARG(I) = ',I8,2X,A4)
           CALL DPWRST('XXX','BUG ')
 9016    CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPSAVE(IFOUND,IERROR)
C
C     PURPOSE--SAVE (= WRITE OUT TO FILE) ALL INTERNAL DATAPLOT
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
C     UPDATED         --NOVEMBER  1987.  (DIMENSION FOR I1DATA--1100 TO 100)
C     UPDATED         --DECEMBER  1987.  (DIMENSION FOR V--10000 TO MAXOBW)
C     UPDATED         --FEBRUARY  1989.  SOFT-CODE ALL (ALAN)
C     UPDATED         --OCTOBER   1991.  SUN HAS LIMIT ON # OF WORDS
C     UPDATED                            FOR UNFORMATTED I/O (2,046)
C     UPDATED         --APRIL     1992.  INCLUDE DPCO3D.INC (ALAN)
C     UPDATED         --APRIL     1992.  PPEDHE TO APEDSZ (ALAN)
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      INCLUDE 'DPCOPA.INC'
C
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
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
C  APRIL 1992.  ADD FOLLOWING INCLUDE FILE.
      INCLUDE 'DPCO3D.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPSA'
      ISUBN2='VE  '
      ISUBRO='-999'
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGS2.EQ.'OFF'.AND.ISUBRO.NE.'SAVE')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPSAVE--')
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
      WRITE(ICOUT,62)ISAVNA
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
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SAVE')
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
      ISUBN0='SAVE'
      IERRFI='NO'
C
      IF(IBUGS2.EQ.'OFF'.AND.ISUBRO.NE.'SAVE')GOTO1199
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
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SAVE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ISTAT.EQ.'NONE')GOTO1200
      GOTO1290
 1200 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1211)
 1211 FORMAT('***** ERROR IN DPSAVE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1212)
 1212 FORMAT('      THE DESIRED SAVE')
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
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SAVE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO1310I=1,80
      IFILE(I:I)=' '
 1310 CONTINUE
C
      DO1320I=1,80
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
 1341 FORMAT('***** ERROR IN DPSAVE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1342)
 1342 FORMAT('      A FILE NAME IS REQUIRED')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1343)
 1343 FORMAT('      IN THE SAVE COMMAND')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1344)
 1344 FORMAT('      (FOR EXAMPLE,    SAVE MEMORY DPRUN.DAT)')
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
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SAVE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IREWIN='ON'
      CALL DPOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
      IF(IERRFI.EQ.'YES')GOTO9000
C
C               ********************************************************
C               **  STEP 41-                                          **
C               **  WRITE OUT TO THE SAVE FILE;                       **
C               ********************************************************
C
      ISTEPN='41'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SAVE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     -----BEGIN WRITING OUT-----------------------
C
C     -----WRITE OUT COMMON FOR STANDARD I/O-----
C
      WRITE(IOUNIT)IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      WRITE(IOUNIT)IFEEDB,IPRINT
C
C     -----WRITE OUT COMMON FOR MACHINE CONSTANTS-----
C
      WRITE(IOUNIT)(I1MACH(I),I=1,16)
      WRITE(IOUNIT)(R1MACH(I),I=1,5)
      WRITE(IOUNIT)(D1MACH(I),I=1,5)
C
C     -----WRITE OUT COMMON FOR BUGS-----
C
      WRITE(IOUNIT)(I1BUG(I),I=1,10)
      WRITE(IOUNIT)(IH1BUG(I),I=1,100)
C
C     -----WRITE OUT COMMON FOR HOUSEKEEPING-----
C
C     WRITE(IOUNIT)(I1HOUS(I),I=1,1050)
      WRITE(IOUNIT)(I1HOUS(I),I=1,5*MAXSTR+50)
C     WRITE(IOUNIT)(IH1HOU(I),I=1,2320)
      WRITE(IOUNIT)(IH1HOU(I),I=1,11*MAXSTR+120)
C     WRITE(IOUNIT)(R1HOUS(I),I=1,400)
      WRITE(IOUNIT)(R1HOUS(I),I=1,2*MAXSTR)
C
C     -----WRITE OUT COMMON FOR DATA-----
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
CCCC  WRITE(IOUNIT)(I1DATA(I),I=1,1100)
CCCCC WRITE(IOUNIT)(I1DATA(I),I=1,MAXOBS+100)
      WRITE(IOUNIT)(I1DATA(I),I=1,100)
CCCCC WRITE(IOUNIT)(ISUB(I),I=1,MAXOBV)
      DO9112IK=1,NLOOP1
      JSTART=(IK-1)*MAXWRD+1
      IF(JSTART.GT.MAXOBV)GOTO9117
      JSTOP=IK*MAXWRD
      IF(JSTOP.GT.MAXOBV)JSTOP=MAXOBV
      WRITE(IOUNIT)(ISUB(I),I=JSTART,JSTOP)
 9112 CONTINUE
 9117 CONTINUE
CCCCC WRITE(IOUNIT)(IH1DAT(I),I=1,3500)
CCCCC WRITE(IOUNIT)(IH1DAT(I),I=1,3*MAXF1+3*MAXFN2+MAXF3)
      WRITE(IOUNIT)(IPARNC(I),I=1,MAXFN2)
      WRITE(IOUNIT)(IPANC2(I),I=1,MAXFN2)
      WRITE(IOUNIT)(IPAROP(I),I=1,MAXFN2)
      WRITE(IOUNIT)(MODEL(I),I=1,MAXF3)
      WRITE(IOUNIT)(IFUNC(I),I=1,MAXF1)
      WRITE(IOUNIT)(IFUNC2(I),I=1,MAXF1)
      WRITE(IOUNIT)(IFUNC3(I),I=1,MAXF1)
CCCCC WRITE(IOUNIT)(R1DATA(I),I=1,10200)
CCCCC WRITE(IOUNIT)(R1DATA(I),I=1,42200)
      WRITE(IOUNIT)(PARLIM(I),I=1,100)
CCCCC WRITE(IOUNIT)(PRED(I),I=1,MAXOBV)
      DO9122IK=1,NLOOP1
      JSTART=(IK-1)*MAXWRD+1
      IF(JSTART.GT.MAXOBV)GOTO9127
      JSTOP=IK*MAXWRD
      IF(JSTOP.GT.MAXOBV)JSTOP=MAXOBV
      WRITE(IOUNIT)(PRED(I),I=JSTART,JSTOP)
 9122 CONTINUE
 9127 CONTINUE
CCCCC WRITE(IOUNIT)(RES(I),I=1,MAXOBV)
      DO9132IK=1,NLOOP1
      JSTART=(IK-1)*MAXWRD+1
      IF(JSTART.GT.MAXOBV)GOTO9137
      JSTOP=IK*MAXWRD
      IF(JSTOP.GT.MAXOBV)JSTOP=MAXOBV
      WRITE(IOUNIT)(RES(I),I=JSTART,JSTOP)
 9132 CONTINUE
 9137 CONTINUE
CCCCC WRITE(IOUNIT)(Y(I),I=1,MAXPOP)
      DO9142IK=1,NLOOP2
      JSTART=(IK-1)*MAXWRD+1
      IF(JSTART.GT.MAXPOP)GOTO9147
      JSTOP=IK*MAXWRD
      IF(JSTOP.GT.MAXPOP)JSTOP=MAXPOP
      WRITE(IOUNIT)(Y(I),I=JSTART,JSTOP)
 9142 CONTINUE
 9147 CONTINUE
CCCCC WRITE(IOUNIT)(X(I),I=1,MAXPOP)
      DO9152IK=1,NLOOP2
      JSTART=(IK-1)*MAXWRD+1
      IF(JSTART.GT.MAXPOP)GOTO9157
      JSTOP=IK*MAXWRD
      IF(JSTOP.GT.MAXPOP)JSTOP=MAXPOP
      WRITE(IOUNIT)(X(I),I=JSTART,JSTOP)
 9152 CONTINUE
 9157 CONTINUE
CCCCC WRITE(IOUNIT)(X3D(I),I=1,MAXPOP)
      DO9162IK=1,NLOOP2
      JSTART=(IK-1)*MAXWRD+1
      IF(JSTART.GT.MAXPOP)GOTO9167
      JSTOP=IK*MAXWRD
      IF(JSTOP.GT.MAXPOP)JSTOP=MAXPOP
      WRITE(IOUNIT)(X3D(I),I=JSTART,JSTOP)
 9162 CONTINUE
 9167 CONTINUE
CCCCC WRITE(IOUNIT)(D(I),I=1,MAXPOP)
      DO9172IK=1,NLOOP2
      JSTART=(IK-1)*MAXWRD+1
      IF(JSTART.GT.MAXPOP)GOTO9177
      JSTOP=IK*MAXWRD
      IF(JSTOP.GT.MAXPOP)JSTOP=MAXPOP
      WRITE(IOUNIT)(D(I),I=JSTART,JSTOP)
 9172 CONTINUE
 9177 CONTINUE
CCCCC WRITE(IOUNIT)(YPLOT(I),I=1,MAXPOP)
      DO9182IK=1,NLOOP2
      JSTART=(IK-1)*MAXWRD+1
      IF(JSTART.GT.MAXPOP)GOTO9187
      JSTOP=IK*MAXWRD
      IF(JSTOP.GT.MAXPOP)JSTOP=MAXPOP
      WRITE(IOUNIT)(YPLOT(I),I=JSTART,JSTOP)
 9182 CONTINUE
 9187 CONTINUE
CCCCC WRITE(IOUNIT)(XPLOT(I),I=1,MAXPOP)
      DO9192IK=1,NLOOP2
      JSTART=(IK-1)*MAXWRD+1
      IF(JSTART.GT.MAXPOP)GOTO9197
      JSTOP=IK*MAXWRD
      IF(JSTOP.GT.MAXPOP)JSTOP=MAXPOP
      WRITE(IOUNIT)(XPLOT(I),I=JSTART,JSTOP)
 9192 CONTINUE
 9197 CONTINUE
CCCCC WRITE(IOUNIT)(X2PLOT(I),I=1,MAXPOP)
      DO9212IK=1,NLOOP2
      JSTART=(IK-1)*MAXWRD+1
      IF(JSTART.GT.MAXPOP)GOTO9217
      JSTOP=IK*MAXWRD
      IF(JSTOP.GT.MAXPOP)JSTOP=MAXPOP
      WRITE(IOUNIT)(X2PLOT(I),I=JSTART,JSTOP)
 9212 CONTINUE
 9217 CONTINUE
CCCCC WRITE(IOUNIT)(TAGPLO(I),I=1,MAXPOP)
      DO9222IK=1,NLOOP2
      JSTART=(IK-1)*MAXWRD+1
      IF(JSTART.GT.MAXPOP)GOTO9227
      JSTOP=IK*MAXWRD
      IF(JSTOP.GT.MAXPOP)JSTOP=MAXPOP
      WRITE(IOUNIT)(TAGPLO(I),I=JSTART,JSTOP)
 9222 CONTINUE
 9227 CONTINUE
CCCCC WRITE(IOUNIT)(V(I),I=1,MAXOBW)
      DO9232IK=1,NLOOP3
      JSTART=(IK-1)*MAXWRD+1
      IF(JSTART.GT.MAXOBW)GOTO9237
      JSTOP=IK*MAXWRD
      IF(JSTOP.GT.MAXOBW)JSTOP=MAXOBW
      WRITE(IOUNIT)(V(I),I=JSTART,JSTOP)
 9232 CONTINUE
 9237 CONTINUE
CCCCC WRITE(IOUNIT)((AMATR1(I,J),I=1,100),J=1,100)
      ITEMP=100*100
      IF(ITEMP.LE.MAXWRD)THEN
        WRITE(IOUNIT)((AMATR1(I,J),I=1,100),J=1,100)
      ELSE
        WRITE(IOUNIT)((AMATR1(I,J),I=1,100),J=1,10)
        WRITE(IOUNIT)((AMATR1(I,J),I=1,100),J=11,20)
        WRITE(IOUNIT)((AMATR1(I,J),I=1,100),J=21,30)
        WRITE(IOUNIT)((AMATR1(I,J),I=1,100),J=31,40)
        WRITE(IOUNIT)((AMATR1(I,J),I=1,100),J=41,50)
        WRITE(IOUNIT)((AMATR1(I,J),I=1,100),J=51,60)
        WRITE(IOUNIT)((AMATR1(I,J),I=1,100),J=61,70)
        WRITE(IOUNIT)((AMATR1(I,J),I=1,100),J=71,80)
        WRITE(IOUNIT)((AMATR1(I,J),I=1,100),J=81,90)
        WRITE(IOUNIT)((AMATR1(I,J),I=1,100),J=91,100)
      END IF
CCCCC WRITE(IOUNIT)(R1DATA(I),I=1,2*MAXOBS+8*MAXPLP+200)
CCCCC WRITE(IOUNIT)(V(I),I=1,10000)
CCCCC WRITE(IOUNIT)(V(I),I=1,MAXWS)
C
C     -----WRITE OUT COMMON FOR SUPPORT-----
C
      WRITE(IOUNIT)(I1SUPP(I),I=1,50)
      WRITE(IOUNIT)(IH1SUP(I),I=1,70)
      WRITE(IOUNIT)(R1SUPP(I),I=1,60)
C
C     -----WRITE OUT COMMON FOR SUBFILE I/O (UNIVAC ONLY)-----
C
      WRITE(IOUNIT)(IBUF(I),I=1,504)
C
C     -----WRITE OUT COMMON FOR DIAGRAMMATIC GRAPHICS-----
C
      WRITE(IOUNIT)(IH1DIA(I),I=1,40)
      WRITE(IOUNIT)(R1DIAG(I),I=1,40)
C
C     -----WRITE OUT COMMON FOR COLOR-----
C
      WRITE(IOUNIT)ICOLOR
      WRITE(IOUNIT)IPLOTF
C
C     -----WRITE OUT COMMON FOR BUGS AND ERROR-----
C
      WRITE(IOUNIT)IBUGG4
      WRITE(IOUNIT)ISUBG4
      WRITE(IOUNIT)IERRG4
C
C     -----WRITE OUT COMMON FOR HOST-----
C
      WRITE(IOUNIT)IHOST1
      WRITE(IOUNIT)IHOST2
      WRITE(IOUNIT)IHMOD1
      WRITE(IOUNIT)IHMOD2
      WRITE(IOUNIT)IOPSY1
      WRITE(IOUNIT)IOPSY2
      WRITE(IOUNIT)ICOMPI
      WRITE(IOUNIT)ISITE
C
C     -----WRITE OUT COMMON FOR TRANSLATOR-----
C
      WRITE(IOUNIT)ITRANS
      WRITE(IOUNIT)NCTRA1
      WRITE(IOUNIT)NCTRA2
      WRITE(IOUNIT)NUMTRA
      WRITE(IOUNIT)ICTRA1
      WRITE(IOUNIT)ICTRA2
C
C     -----WRITE OUT COMMON FOR NON-PRINTING CHARACTERS-----
C
      WRITE(IOUNIT)INULC
      WRITE(IOUNIT)ISOHC
      WRITE(IOUNIT)ISTXC
      WRITE(IOUNIT)IETXC
      WRITE(IOUNIT)IEOTC
      WRITE(IOUNIT)IENQC
      WRITE(IOUNIT)IACKC
      WRITE(IOUNIT)IBELC
      WRITE(IOUNIT)IBSC
      WRITE(IOUNIT)IHTC
      WRITE(IOUNIT)ILFC
      WRITE(IOUNIT)IVTC
      WRITE(IOUNIT)IFFC
      WRITE(IOUNIT)ICRC
      WRITE(IOUNIT)ISOC
      WRITE(IOUNIT)ISIC
      WRITE(IOUNIT)IDLEC
      WRITE(IOUNIT)IDC1C
      WRITE(IOUNIT)IDC2C
      WRITE(IOUNIT)IDC3C
      WRITE(IOUNIT)IDC4C
      WRITE(IOUNIT)INAKC
      WRITE(IOUNIT)ISYNC
      WRITE(IOUNIT)IETBC
      WRITE(IOUNIT)ICANC
      WRITE(IOUNIT)IEMC
      WRITE(IOUNIT)ISUBC
      WRITE(IOUNIT)IESCC
      WRITE(IOUNIT)IFSC
      WRITE(IOUNIT)IGSC
      WRITE(IOUNIT)IRSC
      WRITE(IOUNIT)IUSC
C
C     -----WRITE OUT COMMON FOR GRAPHICS-----
C
      WRITE(IOUNIT)IMANUF
      WRITE(IOUNIT)IMODEL
      WRITE(IOUNIT)IMODE2
      WRITE(IOUNIT)IMODE3
      WRITE(IOUNIT)IGCODE
      WRITE(IOUNIT)IGUNIT
      WRITE(IOUNIT)IGCONT
      WRITE(IOUNIT)NUMHPP
      WRITE(IOUNIT)NUMVPP
      WRITE(IOUNIT)ANUMHP
      WRITE(IOUNIT)ANUMVP
      WRITE(IOUNIT)IGCOLO
      WRITE(IOUNIT)IGBAUD
      WRITE(IOUNIT)AGERDE
      WRITE(IOUNIT)AGCODE
      WRITE(IOUNIT)ISOFT
      WRITE(IOUNIT)ISOFT2
      WRITE(IOUNIT)ISOFT3
C
C     -----WRITE OUT COMMON FOR FILE OPERATIONS-----
C
      WRITE(IOUNIT)(I1FILO(I),I=1,10)
      WRITE(IOUNIT)(IH1FIL(I),I=1,200)
C
C     -----WRITE OUT COMMON FOR FILE OPERATIONS, PART 2-----
C
      WRITE(IOUNIT)IMESNU
      WRITE(IOUNIT)IMESNA
      WRITE(IOUNIT)IMESST
      WRITE(IOUNIT)IMESFO
      WRITE(IOUNIT)IMESAC
      WRITE(IOUNIT)IMESPR
      WRITE(IOUNIT)IMESCS
C
      WRITE(IOUNIT)INEWNU
      WRITE(IOUNIT)INEWNA
      WRITE(IOUNIT)INEWST
      WRITE(IOUNIT)INEWFO
      WRITE(IOUNIT)INEWAC
      WRITE(IOUNIT)INEWPR
      WRITE(IOUNIT)INEWCS
C
      WRITE(IOUNIT)IMAINU
      WRITE(IOUNIT)IMAINA
      WRITE(IOUNIT)IMAIST
      WRITE(IOUNIT)IMAIFO
      WRITE(IOUNIT)IMAIAC
      WRITE(IOUNIT)IMAIPR
      WRITE(IOUNIT)IMAICS
C
      WRITE(IOUNIT)IHELNU
      WRITE(IOUNIT)IHELNA
      WRITE(IOUNIT)IHELST
      WRITE(IOUNIT)IHELFO
      WRITE(IOUNIT)IHELAC
      WRITE(IOUNIT)IHELPR
      WRITE(IOUNIT)IHELCS
C
      WRITE(IOUNIT)IBUGNU
      WRITE(IOUNIT)IBUGNA
      WRITE(IOUNIT)IBUGST
      WRITE(IOUNIT)IBUGFO
      WRITE(IOUNIT)IBUGAC
      WRITE(IOUNIT)IBUGPR
      WRITE(IOUNIT)IBUGCS
C
      WRITE(IOUNIT)IQUENU
      WRITE(IOUNIT)IQUENA
      WRITE(IOUNIT)IQUEST
      WRITE(IOUNIT)IQUEFO
      WRITE(IOUNIT)IQUEAC
      WRITE(IOUNIT)IQUEPR
      WRITE(IOUNIT)IQUECS
C
      WRITE(IOUNIT)ILOGNU
      WRITE(IOUNIT)ILOGNA
      WRITE(IOUNIT)ILOGST
      WRITE(IOUNIT)ILOGFO
      WRITE(IOUNIT)ILOGAC
      WRITE(IOUNIT)ILOGPR
      WRITE(IOUNIT)ILOGCS
C
      WRITE(IOUNIT)IREANU
      WRITE(IOUNIT)IREANA
      WRITE(IOUNIT)IREAST
      WRITE(IOUNIT)IREAFO
      WRITE(IOUNIT)IREAAC
      WRITE(IOUNIT)IREAPR
      WRITE(IOUNIT)IREACS
C
      WRITE(IOUNIT)IWRINU
      WRITE(IOUNIT)IWRINA
      WRITE(IOUNIT)IWRIST
      WRITE(IOUNIT)IWRIFO
      WRITE(IOUNIT)IWRIAC
      WRITE(IOUNIT)IWRIPR
      WRITE(IOUNIT)IWRICS
C
      WRITE(IOUNIT)ISAVNU
      WRITE(IOUNIT)ISAVNA
      WRITE(IOUNIT)ISAVST
      WRITE(IOUNIT)ISAVFO
      WRITE(IOUNIT)ISAVAC
      WRITE(IOUNIT)ISAVPR
      WRITE(IOUNIT)ISAVCS
C
      WRITE(IOUNIT)ILISNU
      WRITE(IOUNIT)ILISNA
      WRITE(IOUNIT)ILISST
      WRITE(IOUNIT)ILISFO
      WRITE(IOUNIT)ILISAC
      WRITE(IOUNIT)ILISPR
      WRITE(IOUNIT)ILISCS
C
      WRITE(IOUNIT)ICRENU
      WRITE(IOUNIT)ICRENA
      WRITE(IOUNIT)ICREST
      WRITE(IOUNIT)ICREFO
      WRITE(IOUNIT)ICREAC
      WRITE(IOUNIT)ICREPR
      WRITE(IOUNIT)ICRECS
C
      WRITE(IOUNIT)ISCRNU
      WRITE(IOUNIT)ISCRNA
      WRITE(IOUNIT)ISCRST
      WRITE(IOUNIT)ISCRFO
      WRITE(IOUNIT)ISCRAC
      WRITE(IOUNIT)ISCRPR
      WRITE(IOUNIT)ISCRCS
C
      WRITE(IOUNIT)IDATNU
      WRITE(IOUNIT)IDATNA
      WRITE(IOUNIT)IDATST
      WRITE(IOUNIT)IDATFO
      WRITE(IOUNIT)IDATAC
      WRITE(IOUNIT)IDATPR
      WRITE(IOUNIT)IDATCS
C
      WRITE(IOUNIT)IPL1NU
      WRITE(IOUNIT)IPL1NA
      WRITE(IOUNIT)IPL1ST
      WRITE(IOUNIT)IPL1FO
      WRITE(IOUNIT)IPL1AC
      WRITE(IOUNIT)IPL1PR
      WRITE(IOUNIT)IPL1CS
C
      WRITE(IOUNIT)IPL2NU
      WRITE(IOUNIT)IPL2NA
      WRITE(IOUNIT)IPL2ST
      WRITE(IOUNIT)IPL2FO
      WRITE(IOUNIT)IPL2AC
      WRITE(IOUNIT)IPL2PR
      WRITE(IOUNIT)IPL2CS
C
      WRITE(IOUNIT)IPRONU
      WRITE(IOUNIT)IPRONA
      WRITE(IOUNIT)IPROST
      WRITE(IOUNIT)IPROFO
      WRITE(IOUNIT)IPROAC
      WRITE(IOUNIT)IPROPR
      WRITE(IOUNIT)IPROCS
C
      WRITE(IOUNIT)ICONNU
      WRITE(IOUNIT)ICONNA
      WRITE(IOUNIT)ICONST
      WRITE(IOUNIT)ICONFO
      WRITE(IOUNIT)ICONAC
      WRITE(IOUNIT)ICONPR
      WRITE(IOUNIT)ICONCS
C
      WRITE(IOUNIT)ISACNU
      WRITE(IOUNIT)ISACNA
      WRITE(IOUNIT)ISACST
      WRITE(IOUNIT)ISACFO
      WRITE(IOUNIT)ISACAC
      WRITE(IOUNIT)ISACPR
      WRITE(IOUNIT)ISACCS
C
      WRITE(IOUNIT)IEX1NU
      WRITE(IOUNIT)IEX1NA
      WRITE(IOUNIT)IEX1ST
      WRITE(IOUNIT)IEX1FO
      WRITE(IOUNIT)IEX1AC
      WRITE(IOUNIT)IEX1PR
      WRITE(IOUNIT)IEX1CS
C
      WRITE(IOUNIT)IEX2NU
      WRITE(IOUNIT)IEX2NA
      WRITE(IOUNIT)IEX2ST
      WRITE(IOUNIT)IEX2FO
      WRITE(IOUNIT)IEX2AC
      WRITE(IOUNIT)IEX2PR
      WRITE(IOUNIT)IEX2CS
C
      WRITE(IOUNIT)IEX3NU
      WRITE(IOUNIT)IEX3NA
      WRITE(IOUNIT)IEX3ST
      WRITE(IOUNIT)IEX3FO
      WRITE(IOUNIT)IEX3AC
      WRITE(IOUNIT)IEX3PR
      WRITE(IOUNIT)IEX3CS
C
      WRITE(IOUNIT)IEX4NU
      WRITE(IOUNIT)IEX4NA
      WRITE(IOUNIT)IEX4ST
      WRITE(IOUNIT)IEX4FO
      WRITE(IOUNIT)IEX4AC
      WRITE(IOUNIT)IEX4PR
      WRITE(IOUNIT)IEX4CS
C
      WRITE(IOUNIT)IEX5NU
      WRITE(IOUNIT)IEX5NA
      WRITE(IOUNIT)IEX5ST
      WRITE(IOUNIT)IEX5FO
      WRITE(IOUNIT)IEX5AC
      WRITE(IOUNIT)IEX5PR
      WRITE(IOUNIT)IEX5CS
C
      WRITE(IOUNIT)IFCHAR
C
C     -----WRITE OUT COMMON FOR PLOT CONTROL-----
C
      WRITE(IOUNIT)(IDMANU(I),I=1,MAXDV)
      WRITE(IOUNIT)(IDMODE(I),I=1,MAXDV)
      WRITE(IOUNIT)(IDMOD2(I),I=1,MAXDV)
      WRITE(IOUNIT)(IDMOD3(I),I=1,MAXDV)
      WRITE(IOUNIT)(IDPOWE(I),I=1,MAXDV)
      WRITE(IOUNIT)(IDCONT(I),I=1,MAXDV)
      WRITE(IOUNIT)(IDCOLO(I),I=1,MAXDV)
      WRITE(IOUNIT)(IDSCRE(I),I=1,MAXDV)
      WRITE(IOUNIT)(IDSCRO(I),I=1,MAXDV)
      WRITE(IOUNIT)(IDPAER(I),I=1,MAXDV)
      WRITE(IOUNIT)(IDSEGM(I),I=1,MAXDV)
      WRITE(IOUNIT)(IDSOFT(I),I=1,MAXDV)
      WRITE(IOUNIT)(IDSOF2(I),I=1,MAXDV)
      WRITE(IOUNIT)(IDSOF3(I),I=1,MAXDV)
C
      WRITE(IOUNIT)(IDCODE(I),I=1,MAXDV)
      WRITE(IOUNIT)(IDUNIT(I),I=1,MAXDV)
      WRITE(IOUNIT)(IDNHPP(I),I=1,MAXDV)
      WRITE(IOUNIT)(IDNVPP(I),I=1,MAXDV)
      WRITE(IOUNIT)(IDBAUD(I),I=1,MAXDV)
      WRITE(IOUNIT)NUMDEV,MAXDEV
C
      WRITE(IOUNIT)IERASW,IBELSW,ISORSW,ICOPSW
      WRITE(IOUNIT)IPENSW
      WRITE(IOUNIT)IBACCO,IMARCO
      WRITE(IOUNIT)IDEFXC,IDEFBK,IDEFMC,IDEPEC
      WRITE(IOUNIT)ISEQSW
      WRITE(IOUNIT)IFENSW
      WRITE(IOUNIT)INEGSW
      WRITE(IOUNIT)IVISSW,IPEDSW,IPEDCO
      WRITE(IOUNIT)IDEFMA,IDEFMO,IDEFM2,IDEFM3
      WRITE(IOUNIT)IDEFPO,IDEFCN,IDEFDC
C
      WRITE(IOUNIT)NUMRIN,NUMCOP
      WRITE(IOUNIT)NUMSEQ
      WRITE(IOUNIT)IDEFVP,IDEFHP,IDEFUN
C
      WRITE(IOUNIT)BAWIDT,BARSPA,DEFBAS
      WRITE(IOUNIT)AORIXC,AORIYC,AORIZC
      WRITE(IOUNIT)AEYEXC,AEYEYC,AEYEZC
CCCCC THE FOLLOWING LINE WAS FIXED    APRIL 1992 (ALAN)
CCCCC WRITE(IOUNIT)PPEDHE
      WRITE(IOUNIT)APEDSZ
      WRITE(IOUNIT)DEFSZ,DEFTL
C
      WRITE(IOUNIT)IGRASW
C
      WRITE(IOUNIT)PGRAXO,PGRAYO,PGRAXC,PGRAYC,PGRAXN,PGRAYN
      WRITE(IOUNIT)PMARXC
      WRITE(IOUNIT)PGRAXF,PGRAYF
      WRITE(IOUNIT)PCROXC,PCROYC
C
      WRITE(IOUNIT)IDIASW
C
      WRITE(IOUNIT)PDIAXC,PDIAYC,PDIAX2,PDIAY2
      WRITE(IOUNIT)PDIAHE,PDIAWI,PDIAVG,PDIAHG
C
      WRITE(IOUNIT)PWXMIN,PWXMAX,PWYMIN,PWYMAX
      WRITE(IOUNIT)WWXMIN,WWXMAX,WWYMIN,WWYMAX
C
      WRITE(IOUNIT)IX1MIN,IX1MAX,IY1MIN,IY1MAX
      WRITE(IOUNIT)IX2MIN,IX2MAX,IY2MIN,IY2MAX
C
      WRITE(IOUNIT)PXMIN,PXMAX,PYMIN,PYMAX
      WRITE(IOUNIT)PDXMIN,PDXMAX,PDYMIN,PDYMAX
      WRITE(IOUNIT)PGXMIN,PGXMAX,PGYMIN,PGYMAX
      WRITE(IOUNIT)GX1MIN,GX1MAX,GY1MIN,GY1MAX
      WRITE(IOUNIT)GX2MIN,GX2MAX,GY2MIN,GY2MAX
      WRITE(IOUNIT)DX1MIN,DX1MAX,DY1MIN,DY1MAX
      WRITE(IOUNIT)DX2MIN,DX2MAX,DY2MIN,DY2MAX
      WRITE(IOUNIT)FX1MIN,FX1MAX,FY1MIN,FY1MAX
      WRITE(IOUNIT)FX2MIN,FX2MAX,FY2MIN,FY2MAX
C
      WRITE(IOUNIT)IX1FSW,IX2FSW,IY1FSW,IY2FSW
      WRITE(IOUNIT)IX1FPA,IX2FPA,IY1FPA,IY2FPA
      WRITE(IOUNIT)IX1FCO,IX2FCO,IY1FCO,IY2FCO
C
      WRITE(IOUNIT)PFRATH
C
      WRITE(IOUNIT)IX1TSW,IX2TSW,IY1TSW,IY2TSW
      WRITE(IOUNIT)IX1JSW,IX2JSW,IY1JSW,IY2JSW
      WRITE(IOUNIT)IX1NSW,IX2NSW,IY1NSW,IY2NSW
      WRITE(IOUNIT)IX1TSC,IX2TSC,IY1TSC,IY2TSC
      WRITE(IOUNIT)IX1TJU,IX2TJU,IY1TJU,IY2TJU
      WRITE(IOUNIT)IX1TCO,IX2TCO,IY1TCO,IY2TCO
C
      WRITE(IOUNIT)NMJX1T,NMJX2T,NMJY1T,NMJY2T
      WRITE(IOUNIT)NMNX1T,NMNX2T,NMNY1T,NMNY2T
      WRITE(IOUNIT)NX1COO,NX2COO,NY1COO,NY2COO
      WRITE(IOUNIT)NX1CMN,NX2CMN,NY1CMN,NY2CMN
      WRITE(IOUNIT)MAXTIC
C
      WRITE(IOUNIT)(PX1COO(I),I=1,MAXTC)
      WRITE(IOUNIT)(PX2COO(I),I=1,MAXTC)
      WRITE(IOUNIT)(PY1COO(I),I=1,MAXTC)
      WRITE(IOUNIT)(PY2COO(I),I=1,MAXTC)
      WRITE(IOUNIT)(X1COOR(I),I=1,MAXTC)
      WRITE(IOUNIT)(X2COOR(I),I=1,MAXTC)
      WRITE(IOUNIT)(Y1COOR(I),I=1,MAXTC)
      WRITE(IOUNIT)(Y2COOR(I),I=1,MAXTC)
      WRITE(IOUNIT)(PX1CMN(I),I=1,MAXTC)
      WRITE(IOUNIT)(PX2CMN(I),I=1,MAXTC)
      WRITE(IOUNIT)(PY1CMN(I),I=1,MAXTC)
      WRITE(IOUNIT)(PY2CMN(I),I=1,MAXTC)
      WRITE(IOUNIT)(X1COMN(I),I=1,MAXTC)
      WRITE(IOUNIT)(X2COMN(I),I=1,MAXTC)
      WRITE(IOUNIT)(Y1COMN(I),I=1,MAXTC)
      WRITE(IOUNIT)(Y2COMN(I),I=1,MAXTC)
      WRITE(IOUNIT)PX1TLE,PX2TLE,PY1TLE,PY2TLE
      WRITE(IOUNIT)PTICTH,PMNTFA
C
      WRITE(IOUNIT)IX1ZSW,IX2ZSW,IY1ZSW,IY2ZSW
      WRITE(IOUNIT)IX1ZFO,IX2ZFO,IY1ZFO,IY2ZFO
      WRITE(IOUNIT)IX1ZCA,IX2ZCA,IY1ZCA,IY2ZCA
      WRITE(IOUNIT)IX1ZJU,IX2ZJU,IY1ZJU,IY2ZJU
      WRITE(IOUNIT)IX1ZDI,IX2ZDI,IY1ZDI,IY2ZDI
      WRITE(IOUNIT)IX1ZFI,IX2ZFI,IY1ZFI,IY2ZFI
      WRITE(IOUNIT)IX1ZCO,IX2ZCO,IY1ZCO,IY2ZCO
C
      WRITE(IOUNIT)IX1ZDP,IX2ZDP,IY1ZDP,IY2ZDP
C
      WRITE(IOUNIT)PX1ZDS,PX2ZDS,PY1ZDS,PY2ZDS
      WRITE(IOUNIT)AX1ZAN,AX2ZAN,AY1ZAN,AY2ZAN
      WRITE(IOUNIT)PX1ZHE,PX1ZWI,PX1ZVG,PX1ZHG
      WRITE(IOUNIT)PX2ZHE,PX2ZWI,PX2ZVG,PX2ZHG
      WRITE(IOUNIT)PY1ZHE,PY1ZWI,PY1ZVG,PY1ZHG
      WRITE(IOUNIT)PY2ZHE,PY2ZWI,PY2ZVG,PY2ZHG
      WRITE(IOUNIT)PTIZTH
C
      WRITE(IOUNIT)IVGRSW,IHGRSW
      WRITE(IOUNIT)IVGRPA,IHGRPA
      WRITE(IOUNIT)IVGRCO,IHGRCO
C
      WRITE(IOUNIT)PVGRTH,PHGRTH
C
      WRITE(IOUNIT)(ITITTE(I),I=1,MAXCH)
      WRITE(IOUNIT)ITITFO,ITITCA,ITITFI,ITITCO
C
      WRITE(IOUNIT)NCTITL
C
      WRITE(IOUNIT)PTITHE,PTITWI,PTITVG,PTITHG,PTITTH,PTITDS
C
      WRITE(IOUNIT)(IX1LTE(I),I=1,MAXCH)
      WRITE(IOUNIT)IX1LFO,IX1LCA,IX1LFI,IX1LCO
      WRITE(IOUNIT)(IX2LTE(I),I=1,MAXCH)
      WRITE(IOUNIT)IX2LFO,IX2LCA,IX2LFI,IX2LCO
      WRITE(IOUNIT)(IX3LTE(I),I=1,MAXCH)
      WRITE(IOUNIT)IX3LFO,IX3LCA,IX3LFI,IX3LCO
      WRITE(IOUNIT)(IY1LTE(I),I=1,MAXCH)
      WRITE(IOUNIT)IY1LFO,IY1LCA,IY1LFI,IY1LCO
      WRITE(IOUNIT)(IY2LTE(I),I=1,MAXCH)
      WRITE(IOUNIT)IY2LFO,IY2LCA,IY2LFI,IY2LCO
C
      WRITE(IOUNIT)NCX1LA,NCX2LA,NCX3LA,NCY1LA,NCY2LA
C
      WRITE(IOUNIT)PX1LHE,PX1LWI,PX1LVG,PX1LHG,PX1LTH,PX1LDS
      WRITE(IOUNIT)PX2LHE,PX2LWI,PX2LVG,PX2LHG,PX2LTH,PX2LDS
      WRITE(IOUNIT)PX3LHE,PX3LWI,PX3LVG,PX3LHG,PX3LTH,PX3LDS
      WRITE(IOUNIT)PY1LHE,PY1LWI,PY1LVG,PY1LHG,PY1LTH,PY1LDS
      WRITE(IOUNIT)PY2LHE,PY2LWI,PY2LVG,PY2LHG,PY2LTH,PY2LDS
C
      WRITE(IOUNIT)(ILEGTE(I),I=1,MAXLG2)
      WRITE(IOUNIT)(ILEGFO(I),I=1,MAXLG)
      WRITE(IOUNIT)(ILEGCA(I),I=1,MAXLG)
      WRITE(IOUNIT)(ILEGJU(I),I=1,MAXLG)
      WRITE(IOUNIT)(ILEGDI(I),I=1,MAXLG)
      WRITE(IOUNIT)(ILEGFI(I),I=1,MAXLG)
      WRITE(IOUNIT)(ILEGCO(I),I=1,MAXLG)
      WRITE(IOUNIT)(ILEGNA(I),I=1,MAXLG)
C
      WRITE(IOUNIT)(ILEGST(I),I=1,MAXLG)
      WRITE(IOUNIT)(ILEGSP(I),I=1,MAXLG)
      WRITE(IOUNIT)NCLEG,MXCLEG
      WRITE(IOUNIT)NUMLEG,MAXLEG
C
      WRITE(IOUNIT)(PLEGXC(I),I=1,MAXLG)
      WRITE(IOUNIT)(PLEGYC(I),I=1,MAXLG)
      WRITE(IOUNIT)(PLEGHE(I),I=1,MAXLG)
      WRITE(IOUNIT)(PLEGWI(I),I=1,MAXLG)
      WRITE(IOUNIT)(PLEGVG(I),I=1,MAXLG)
      WRITE(IOUNIT)(PLEGHG(I),I=1,MAXLG)
      WRITE(IOUNIT)(PLEGTH(I),I=1,MAXLG)
      WRITE(IOUNIT)(ALEGAN(I),I=1,MAXLG)
C
      WRITE(IOUNIT)(IBOBFI(I),I=1,MAXBX)
      WRITE(IOUNIT)(IBOBCO(I),I=1,MAXBX)
      WRITE(IOUNIT)(IBOPPA(I),I=1,MAXBX)
      WRITE(IOUNIT)(IBOPCO(I),I=1,MAXBX)
      WRITE(IOUNIT)(IBOFPA(I),I=1,MAXBX)
      WRITE(IOUNIT)(IBOFCO(I),I=1,MAXBX)
C
      WRITE(IOUNIT)NUMBOX,MAXBOX
C
      WRITE(IOUNIT)((PBOXXC(I,J),I=1,MAXBX),J=1,2)
      WRITE(IOUNIT)((PBOXYC(I,J),I=1,MAXBX),J=1,2)
      WRITE(IOUNIT)(PBOPTH(I),I=1,MAXBX)
      WRITE(IOUNIT)(PBOPGA(I),I=1,MAXBX)
      WRITE(IOUNIT)(PBOFTH(I),I=1,MAXBX)
C
      WRITE(IOUNIT)(IARRPA(I),I=1,MAXAR)
      WRITE(IOUNIT)(IARRCO(I),I=1,MAXAR)
      WRITE(IOUNIT)(IARHFI(I),I=1,MAXAR)
C
      WRITE(IOUNIT)NUMARR,MAXARR
C
      WRITE(IOUNIT)((PARRXC(I,J),I=1,MAXAR),J=1,2)
      WRITE(IOUNIT)((PARRYC(I,J),I=1,MAXAR),J=1,2)
      WRITE(IOUNIT)(PARRTH(I),I=1,MAXAR)
      WRITE(IOUNIT)(PARHLE(I),I=1,MAXAR)
      WRITE(IOUNIT)(PARHWI(I),I=1,MAXAR)
C
      WRITE(IOUNIT)(ISEGPA(I),I=1,MAXSG)
      WRITE(IOUNIT)(ISEGCO(I),I=1,MAXSG)
C
      WRITE(IOUNIT)NUMSEG,MAXSEG
C
      WRITE(IOUNIT)((PSEGXC(I,J),I=1,MAXSG),J=1,2)
      WRITE(IOUNIT)((PSEGYC(I,J),I=1,MAXSG),J=1,2)
      WRITE(IOUNIT)(PSEGTH(I),I=1,MAXSG)
C
      WRITE(IOUNIT)(ILINPA(I),I=1,MAXLN)
      WRITE(IOUNIT)(ILINCO(I),I=1,MAXLN)
C
      WRITE(IOUNIT)MAXLIN
C
      WRITE(IOUNIT)(PLINTH(I),I=1,MAXLN)
      WRITE(IOUNIT)(PLINLE(I),I=1,MAXLN)
      WRITE(IOUNIT)(PLINL2(I),I=1,MAXLN)
      WRITE(IOUNIT)(PLINL3(I),I=1,MAXLN)
      WRITE(IOUNIT)(PLINGA(I),I=1,MAXLN)
      WRITE(IOUNIT)(PLING2(I),I=1,MAXLN)
      WRITE(IOUNIT)(PLING3(I),I=1,MAXLN)
C
      WRITE(IOUNIT)(ICHAPA(I),I=1,MAXCH2)
      WRITE(IOUNIT)(ICHAFO(I),I=1,MAXCH2)
      WRITE(IOUNIT)(ICHACA(I),I=1,MAXCH2)
      WRITE(IOUNIT)(ICHAJU(I),I=1,MAXCH2)
      WRITE(IOUNIT)(ICHADI(I),I=1,MAXCH2)
      WRITE(IOUNIT)(ICHAFI(I),I=1,MAXCH2)
      WRITE(IOUNIT)(ICHACO(I),I=1,MAXCH2)
C
      WRITE(IOUNIT)MAXCHA
C
      WRITE(IOUNIT)(PCHAHE(I),I=1,MAXCH2)
      WRITE(IOUNIT)(PCHAWI(I),I=1,MAXCH2)
      WRITE(IOUNIT)(PCHAVG(I),I=1,MAXCH2)
      WRITE(IOUNIT)(PCHAHG(I),I=1,MAXCH2)
      WRITE(IOUNIT)(PCHATH(I),I=1,MAXCH2)
      WRITE(IOUNIT)(ACHAAN(I),I=1,MAXCH2)
C
      WRITE(IOUNIT)(ITEXTE(I),I=1,MAXCH)
      WRITE(IOUNIT)ITEXPA,ITEXFO,ITEXCA,ITEXJU,ITEXDI,ITEXAU,ITEXFI,
     1ITEXCO
      WRITE(IOUNIT)IDEFPA,IDEFFO,IDEFCA,IDEFJU,IDEFDI,IDEFAU,IDEFFI,
     1IDEFCO
      WRITE(IOUNIT)ITEXCR,ITEXLF
      WRITE(IOUNIT)IDEFCR,IDEFLF
      WRITE(IOUNIT)ITEXSY,ITEXSP
      WRITE(IOUNIT)IDEFSY,IDEFSP
C
      WRITE(IOUNIT)NCTEXT,MXCTEX
C
      WRITE(IOUNIT)PTEXHE,PTEXWI,PTEXVG,PTEXHG
      WRITE(IOUNIT)PTEXTH,PTEXLE,ATEXAN
      WRITE(IOUNIT)PDEFHE,PDEFWI,PDEFVG,PDEFHG
      WRITE(IOUNIT)PDEFTH,PDEFLE,ADEFAN
      WRITE(IOUNIT)PTEXMR
      WRITE(IOUNIT)PDEFMR
      WRITE(IOUNIT)PXSTAR,PYSTAR
      WRITE(IOUNIT)PXEND,PYEND
C
      WRITE(IOUNIT)(IFILSW(I),I=1,MAXFL)
      WRITE(IOUNIT)(IFILPA(I),I=1,MAXFL)
      WRITE(IOUNIT)(IFILCO(I),I=1,MAXFL)
      WRITE(IOUNIT)IDEFFS
      WRITE(IOUNIT)IDEFFP
      WRITE(IOUNIT)IDEFFC
C
      WRITE(IOUNIT)MAXFIL
C
      WRITE(IOUNIT)(PFILSP(I),I=1,MAXFL)
      WRITE(IOUNIT)(PFILTH(I),I=1,MAXFL)
      WRITE(IOUNIT)(AFILBA(I),I=1,MAXFL)
      WRITE(IOUNIT)PDEFFG
      WRITE(IOUNIT)PDEFFT
      WRITE(IOUNIT)ADEFFB
C
      WRITE(IOUNIT)(IPATSW(I),I=1,MAXPT)
      WRITE(IOUNIT)(IPATPA(I),I=1,MAXPT)
      WRITE(IOUNIT)(IPATLI(I),I=1,MAXPT)
      WRITE(IOUNIT)(IPATCO(I),I=1,MAXPT)
      WRITE(IOUNIT)IDEFPS
      WRITE(IOUNIT)IDEFPP
      WRITE(IOUNIT)IDEFPL
      WRITE(IOUNIT)IDEFPC
C
      WRITE(IOUNIT)MAXPAT
C
      WRITE(IOUNIT)(PPATHE(I),I=1,MAXPT)
      WRITE(IOUNIT)(PPATWI(I),I=1,MAXPT)
      WRITE(IOUNIT)(PPATSP(I),I=1,MAXPT)
      WRITE(IOUNIT)(PPATTH(I),I=1,MAXPT)
      WRITE(IOUNIT)PDEFPH
      WRITE(IOUNIT)PDEFPW
      WRITE(IOUNIT)PDEFPG
      WRITE(IOUNIT)PDEFPT
C
      WRITE(IOUNIT)(ISPISW(I),I=1,MAXSP)
      WRITE(IOUNIT)(ISPILI(I),I=1,MAXSP)
      WRITE(IOUNIT)(ISPICO(I),I=1,MAXSP)
      WRITE(IOUNIT)IDEFSS
      WRITE(IOUNIT)IDEFSL
      WRITE(IOUNIT)IDEFSC
C
      WRITE(IOUNIT)MAXSPI
C
      WRITE(IOUNIT)(PSPITH(I),I=1,MAXSP)
      WRITE(IOUNIT)(ASPIBA(I),I=1,MAXSP)
      WRITE(IOUNIT)PDEFST
      WRITE(IOUNIT)ADEFSB
C
      WRITE(IOUNIT)(IBARSW(I),I=1,MAXBA)
      WRITE(IOUNIT)(IBABLI(I),I=1,MAXBA)
      WRITE(IOUNIT)(IBABCO(I),I=1,MAXBA)
      WRITE(IOUNIT)(IBAFSW(I),I=1,MAXBA)
      WRITE(IOUNIT)(IBAFCO(I),I=1,MAXBA)
      WRITE(IOUNIT)(IBAPTY(I),I=1,MAXBA)
      WRITE(IOUNIT)(IBAPLI(I),I=1,MAXBA)
      WRITE(IOUNIT)(IBAPCO(I),I=1,MAXBA)
      WRITE(IOUNIT)IDEBSW
      WRITE(IOUNIT)IDEBBL
      WRITE(IOUNIT)IDEBBC
      WRITE(IOUNIT)IDEBFS
      WRITE(IOUNIT)IDEBFC
      WRITE(IOUNIT)IDEBPT
      WRITE(IOUNIT)IDEBPL
      WRITE(IOUNIT)IDEBPC
C
      WRITE(IOUNIT)MAXBAR
C
      WRITE(IOUNIT)(ABARBA(I),I=1,MAXBA)
      WRITE(IOUNIT)(ABARWI(I),I=1,MAXBA)
      WRITE(IOUNIT)(PBABTH(I),I=1,MAXBA)
      WRITE(IOUNIT)(PBAPTH(I),I=1,MAXBA)
      WRITE(IOUNIT)(PBAPSP(I),I=1,MAXBA)
      WRITE(IOUNIT)ADEBBA
      WRITE(IOUNIT)ADEBWI
      WRITE(IOUNIT)PDEBBT
      WRITE(IOUNIT)PDEBPT
      WRITE(IOUNIT)PDEBPS
C
      WRITE(IOUNIT)(IREGSW(I),I=1,MAXRG)
      WRITE(IOUNIT)(IREBLI(I),I=1,MAXRG)
      WRITE(IOUNIT)(IREBCO(I),I=1,MAXRG)
      WRITE(IOUNIT)(IREFSW(I),I=1,MAXRG)
      WRITE(IOUNIT)(IREFCO(I),I=1,MAXRG)
      WRITE(IOUNIT)(IREPTY(I),I=1,MAXRG)
      WRITE(IOUNIT)(IREPLI(I),I=1,MAXRG)
      WRITE(IOUNIT)(IREPCO(I),I=1,MAXRG)
      WRITE(IOUNIT)IDERSW
      WRITE(IOUNIT)IDERBL
      WRITE(IOUNIT)IDERBC
      WRITE(IOUNIT)IDERFS
      WRITE(IOUNIT)IDERFC
      WRITE(IOUNIT)IDERPT
      WRITE(IOUNIT)IDERPL
      WRITE(IOUNIT)IDERPC
C
      WRITE(IOUNIT)MAXREG
C
      WRITE(IOUNIT)(AREGBA(I),I=1,MAXRG)
      WRITE(IOUNIT)(AREGWI(I),I=1,MAXRG)
      WRITE(IOUNIT)(PREBTH(I),I=1,MAXRG)
      WRITE(IOUNIT)(PREPTH(I),I=1,MAXRG)
      WRITE(IOUNIT)(PREPSP(I),I=1,MAXRG)
      WRITE(IOUNIT)ADERBA
      WRITE(IOUNIT)ADERWI
      WRITE(IOUNIT)PDERBT
      WRITE(IOUNIT)PDERPT
      WRITE(IOUNIT)PDERPS
C
      WRITE(IOUNIT)(IMARSW(I),I=1,MAXMR)
      WRITE(IOUNIT)(IMABLI(I),I=1,MAXMR)
      WRITE(IOUNIT)(IMABCO(I),I=1,MAXMR)
      WRITE(IOUNIT)(IMAFSW(I),I=1,MAXMR)
      WRITE(IOUNIT)(IMAFCO(I),I=1,MAXMR)
      WRITE(IOUNIT)(IMAPTY(I),I=1,MAXMR)
      WRITE(IOUNIT)(IMAPLI(I),I=1,MAXMR)
      WRITE(IOUNIT)(IMAPCO(I),I=1,MAXMR)
      WRITE(IOUNIT)IDEMSW
      WRITE(IOUNIT)IDEMBL
      WRITE(IOUNIT)IDEMBC
      WRITE(IOUNIT)IDEMFS
      WRITE(IOUNIT)IDEMFC
      WRITE(IOUNIT)IDEMPT
      WRITE(IOUNIT)IDEMPL
      WRITE(IOUNIT)IDEMPC
C
      WRITE(IOUNIT)MAXMAR
C
      WRITE(IOUNIT)(AMARBA(I),I=1,MAXMR)
      WRITE(IOUNIT)(AMARWI(I),I=1,MAXMR)
      WRITE(IOUNIT)(PMABTH(I),I=1,MAXMR)
      WRITE(IOUNIT)(PMAPTH(I),I=1,MAXMR)
      WRITE(IOUNIT)(PMAPSP(I),I=1,MAXMR)
      WRITE(IOUNIT)ADEMBA
      WRITE(IOUNIT)ADEMWI
      WRITE(IOUNIT)PDEMBT
      WRITE(IOUNIT)PDEMPT
      WRITE(IOUNIT)PDEMPS
C
      WRITE(IOUNIT)(ITEXSW(I),I=1,MAXTX)
      WRITE(IOUNIT)(ITEBLI(I),I=1,MAXTX)
      WRITE(IOUNIT)(ITEBCO(I),I=1,MAXTX)
      WRITE(IOUNIT)(ITEFSW(I),I=1,MAXTX)
      WRITE(IOUNIT)(ITEFCO(I),I=1,MAXTX)
      WRITE(IOUNIT)(ITEPTY(I),I=1,MAXTX)
      WRITE(IOUNIT)(ITEPLI(I),I=1,MAXTX)
      WRITE(IOUNIT)(ITEPCO(I),I=1,MAXTX)
      WRITE(IOUNIT)IDETSW
      WRITE(IOUNIT)IDETBL
      WRITE(IOUNIT)IDETBC
      WRITE(IOUNIT)IDETFS
      WRITE(IOUNIT)IDETFC
      WRITE(IOUNIT)IDETPT
      WRITE(IOUNIT)IDETPL
      WRITE(IOUNIT)IDETPC
C
      WRITE(IOUNIT)MAXTEX
C
      WRITE(IOUNIT)(ATEXBA(I),I=1,MAXTX)
      WRITE(IOUNIT)(ATEXWI(I),I=1,MAXTX)
      WRITE(IOUNIT)(PTEBTH(I),I=1,MAXTX)
      WRITE(IOUNIT)(PTEPTH(I),I=1,MAXTX)
      WRITE(IOUNIT)(PTEPSP(I),I=1,MAXTX)
      WRITE(IOUNIT)ADETBA
      WRITE(IOUNIT)ADETWI
      WRITE(IOUNIT)PDETBT
      WRITE(IOUNIT)PDETPT
      WRITE(IOUNIT)PDETPS
C
C     -----END WRITING OUT-----------------------
C
C               ***************************
C               **  STEP 42--            **
C               **  WRITE OUT A MESSAGE  **
C               ***************************
C
      ISTEPN='42'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SAVE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IERROR.EQ.'YES')GOTO4290
      IF(IFEEDB.EQ.'OFF')GOTO4290
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4211)
 4211 FORMAT('THE SAVING OF ALL INTERNAL DATAPLOT VARIABLES,')
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
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SAVE')
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
      IF(IBUGS2.EQ.'OFF'.AND.ISUBRO.NE.'SAVE')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPSAVE--')
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
      SUBROUTINE DPSBEX(ISBNAM,IANGLU,ISEED,IFTEXP,IFTORD,IFORSW,
     1                  IBUGA2,IBUGA3,IBUGCO,IBUGEV,IBUGQ,
     1                  ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--CARRY OUT A SUBSET OF THE LET COMMAND TO BE USED BY
C              THE "STATISTIC BLOCK".
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABOARATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2016/08
C     ORIGINAL VERSION--AUGUST    2016.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*8 ISBNAM
      CHARACTER*4 IANGLU
      CHARACTER*4 IFTEXP
      CHARACTER*4 IFTORD
      CHARACTER*4 IFORSW
      CHARACTER*4 IBUGA2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGCO
      CHARACTER*4 IBUGEV
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 ICASLE
      CHARACTER*4 ITYPEL
      CHARACTER*4 IFOUNZ
      CHARACTER*4 ITYPE
      CHARACTER*4 IHOL
      CHARACTER*4 IHOL2
      CHARACTER*4 IERRO1
      CHARACTER*4 ITYPEH
      CHARACTER*4 IW21HO
      CHARACTER*4 IW22HO
      CHARACTER*4 IA
      CHARACTER*4 IPARN
      CHARACTER*4 IPARN2
      CHARACTER*4 IFOUNR
      CHARACTER*4 IFOUN7
      CHARACTER*4 IFOUN8
      CHARACTER*4 ICASL7
      CHARACTER*4 ICASS7
      CHARACTER*4 ICASL8
      CHARACTER*4 ICASRA
      CHARACTER*4 ITYW1L
      CHARACTER*4 ICAT1L
      CHARACTER*4 INLI1L
      CHARACTER*4 ITYW2L
      CHARACTER*4 ITYW1R
      CHARACTER*4 ICAT1R
      CHARACTER*4 INLI1R
      CHARACTER*4 ITYW2R
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ICOMT
      CHARACTER*4 IMSUBC
      CHARACTER*4 ICASAR
      CHARACTER*1 IREPCH
C
C---------------------------------------------------------------------
C
      DIMENSION IFOUNZ(30)
      DIMENSION IBEGIN(30)
      DIMENSION IEND(30)
      DIMENSION ITYPE(30)
      DIMENSION IHOL(30)
      DIMENSION IHOL2(30)
      DIMENSION INT1(30)
      DIMENSION FLOAT1(30)
      DIMENSION IERRO1(30)
C
      DIMENSION ITYPEH(1000)
      DIMENSION IW21HO(1000)
      DIMENSION IW22HO(1000)
      DIMENSION W2HOLD(1000)
C
C     NOTE--THE DIMENSION OF IA SHOULD BE THE SAME AS
C           THE DIMENSION OF IB IN SUBROUTINE COMPIM
C           (THE DIMENSION OF IB IS 1000 (JULY 1986))
C
      DIMENSION IA(1000)
      DIMENSION PARAM(100)
      DIMENSION IPARN(100)
      DIMENSION IPARN2(100)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSB.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCODA.INC'
C
      INCLUDE 'DPCOZI.INC'
      INCLUDE 'DPCOZ3.INC'
      INCLUDE 'DPCOZD.INC'
C
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TEMP3(MAXOBV)
      DIMENSION TEMP4(MAXOBV)
      DIMENSION TEMP5(MAXOBV)
      DIMENSION TEMP6(MAXOBV)
      INTEGER ITEMP1(MAXOBV)
      INTEGER ITEMP2(MAXOBV)
      INTEGER ITEMP3(MAXOBV)
      INTEGER ITEMP4(MAXOBV)
      INTEGER ITEMP5(MAXOBV)
      INTEGER ITEMP6(MAXOBV)
C
      EQUIVALENCE (G3RBAG(KGARB1),TEMP1(1))
      EQUIVALENCE (G3RBAG(KGARB2),TEMP2(1))
      EQUIVALENCE (G3RBAG(KGARB3),TEMP3(1))
      EQUIVALENCE (G3RBAG(KGARB4),TEMP4(1))
      EQUIVALENCE (G3RBAG(KGARB5),TEMP5(1))
      EQUIVALENCE (G3RBAG(KGARB6),TEMP6(1))
C
      EQUIVALENCE (IGARBG(IIGR12),ITEMP1(1))
      EQUIVALENCE (IGARBG(IIGR13),ITEMP2(1))
      EQUIVALENCE (IGARBG(IIGR14),ITEMP3(1))
      EQUIVALENCE (IGARBG(IIGR15),ITEMP4(1))
      EQUIVALENCE (IGARBG(IIGR16),ITEMP5(1))
      EQUIVALENCE (IGARBG(IIGR17),ITEMP6(1))
C
      DOUBLE PRECISION DTEMP1(MAXOBV)
      DOUBLE PRECISION DTEMP2(MAXOBV)
      DOUBLE PRECISION DTEMP3(MAXOBV)
      EQUIVALENCE (DGARBG(IDGAR8),DTEMP1(1))
      EQUIVALENCE (DGARBG(IDGAR9),DTEMP2(1))
      EQUIVALENCE (DGARBG(IDGA10),DTEMP3(1))
C
      CHARACTER*4 IANSSV(MAXSTR)
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPSB'
      ISUBN2='EX  '
      IERROR='NO'
      ICASLE='UNKN'
      IMSUBC='UNKN'
      IREPCH='^'
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
      DO40I=1,1000
        ITYPEH(I)='    '
        IW21HO(I)='    '
        IW22HO(I)='    '
        W2HOLD(I)=0.0
   40 CONTINUE
C
C               *************************************
C               **  TREAT THE STATISTIC BLOCK CASE **
C               *************************************
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'SBEX')THEN
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPSBEX--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ISBNAM,IANGLU,IFTEXP,IFORSW,ISEED
   52   FORMAT('ISBNAM,IANGLU,IFTEXP,IFORSW,ISEED, = ',
     1         A8,2X,3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGA2,IBUGA3,IBUGCO,IBUGEV,IBUGQ
   53   FORMAT('IBUGA2,IBUGA3,IBUGCO,IBUGEV,IBUGQ = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)ISBNA1,ISBNA2,ISBNA3
   55   FORMAT('ISBNA1,ISBNA2,ISBNA3 = ',2(A8,2X),A8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,57)ISBCN1,ISBCN2,ISBCN3
   57   FORMAT('ISBCN1,ISBCN2,ISBCN3 = ',3I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ******************************************
C               **  STEP 1--                            **
C               **  CHECK IF STATISTIC BLOCK IS DEFINED **
C               ******************************************
C
      IFLAG=0
      IF(ISBNAM.EQ.ISBNA1)THEN
        IFLAG=1
        ISBCNT=ISBCN1
        ISBCN2=ISBCP1
      ELSEIF(ISBNAM.EQ.ISBNA2)THEN
        IFLAG=2
        ISBCNT=ISBCN2
        ISBCN2=ISBCP2
      ELSEIF(ISBNAM.EQ.ISBNA3)THEN
        IFLAG=3
        ISBCNT=ISBCN3
        ISBCN2=ISBCP3
      ELSE
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR IN STATISTIC BLOCK--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)ISBNAM
  102   FORMAT('      STATISTIC BLOCK ',A8,' HAS NOT BEEN DEFINED.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(ISBCNT.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)ISBNAM
  112   FORMAT('      FUNCTION BLOCK ',A8,' HAS NO ACTIVE COMMANDS.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C     SAVE CURRENT COMMAND LINE
C
      DO910II=1,MAXSTR
        IANSSV(II)=IANSLC(II)
  910 CONTINUE
C
C     LOOP THROUGH EACH LINE OF THE FUNCTION BLOCK
C
      DO1000KK=1,ISBCNT
C
C       STEP 1: PUT THE FUNCTION BLOCK LINE INTO IANSLC
C
        IF(IFLAG.EQ.1)THEN
          DO1010II=1,MAXSTR
            IANSLC(II)=' '
            IANSLC(II)(1:1)=ISBLI1(KK)(II:II)
 1010     CONTINUE
        ELSEIF(IFLAG.EQ.2)THEN
          DO1020II=1,MAXSTR
            IANSLC(II)=' '
            IANSLC(II)(1:1)=ISBLI2(KK)(II:II)
 1020     CONTINUE
        ELSEIF(IFLAG.EQ.3)THEN
          DO1030II=1,MAXSTR
            IANSLC(II)=' '
            IANSLC(II)(1:1)=ISBLI3(KK)(II:II)
 1030     CONTINUE
        ENDIF
C
        IWIDTH=1
        DO1040II=MAXSTR,1,-1
          IF(IANSLC(II)(1:1).NE.' ')THEN
            IWIDTH=II
            GOTO1049
          ENDIF
 1040   CONTINUE
 1049   CONTINUE
C
        IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'SBEX')THEN
          WRITE(ICOUT,1051)KK,IWIDTH
 1051     FORMAT('KK,IWIDTH = ',2I8)
          CALL DPWRST('XXX','BUG ')
          DO1053II=1,IWIDTH
            WRITE(ICOUT,1054)II,IANSLC(II)
 1054       FORMAT('II,IANSLC(II) = ',I5,2X,A4)
            CALL DPWRST('XXX','BUG ')
 1053     CONTINUE
        ENDIF
C
C       STEP 2: NOW PROCESS IANSLC TO BREAK IT INTO COMPONENT ARGUMENTS
C
        CALL DPNONP(IANSLC,IWIDTH,IANSLC,IBUGA2,ISUBRO,IERROR)
        CALL DPREP2(IANSLC,IWIDTH,
     1              IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,
     1              IVARLB,IROWLB,MAXOBV,
     1              IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,IMALEV,
     1              IBUGA2,ISUBRO,IERROR)
        CALL DPUPPE(IANSLC,IWIDTH,IANS,IBUGA2,IERROR)
        CALL DPTYPE(IANSLC,IWIDTH,IBUGA2,
     1              ICOM,ICOM2,ICOMT,ICOMI,ACOM,ICOMLC,ICOML2,
     1              IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,
     1              IHARG,IHARG2,IARGT,IARG,ARG,IHARLC,IHARL2,NUMARG,
     1              IHOST1,IHOST2)
C
        IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'SBEX')THEN
          WRITE(ICOUT,1061)NUMARG
 1061     FORMAT('NUMARG = ',I8)
          CALL DPWRST('XXX','BUG ')
          DO1063II=1,NUMARG
            WRITE(ICOUT,1064)II,IHARG(II),IHARG2(II)
 1064       FORMAT('II,IHARG(II),IHARG2(II) = ',I5,2(2X,A4))
            CALL DPWRST('XXX','BUG ')
 1063     CONTINUE
        ENDIF
C
C       STEP 3: NOW PROCESS THE LET COMMANDS
C
C               CHECK FOR AN "=" SIGN (THIS SHOULD NOT BE LAST
C               ARGUMENT IN LIST)
C
        DO1103I=1,NUMARG
          IF(IHARG(I).EQ.'=   ')THEN
            IF(I.LT.NUMARG)GOTO1119
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,101)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1112)
 1112       FORMAT('      IMPROPER FORM FOR THE    LET   COMMAND.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1123)
 1123       FORMAT('      NOTHING FOUND TO THE RIGHT OF THE EQUAL SIGN')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1114)
            CALL DPWRST('XXX','BUG ')
            IF(IWIDTH.GE.1)THEN
              WRITE(ICOUT,1115)(IANSLC(JJ),JJ=1,MIN(120,IWIDTH))
              CALL DPWRST('XXX','BUG ')
            ENDIF
            IERROR='YES'
            GOTO9000
          ENDIF
 1103   CONTINUE
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1112)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1113)
 1113   FORMAT('      NO EQUAL SIGN FOUND AFTER THE ',
     1         'VARIABLE/PARAMETER NAME.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1114)
 1114   FORMAT('      THE ENTERED COMMAND LINE IS AS FOLLOWS--')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,1115)(IANSLC(I),I=1,MIN(120,IWIDTH))
 1115     FORMAT('      ',120A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
C
 1119   CONTINUE
C
C               **************************************
C               **  STEP 2--                        **
C               **  TREAT THE VARIOUS LET SUBCASES  **
C               **************************************
C
C      CURRENTLY, THERE ARE 25 COMMANDS THAT UTILIZE SUPPORTED
C      STATISTICS.  THE STATISTIC BLOCKS CAN BE USED BY ANY OF
C      THESE 25 COMMANDS:
C
C      CURRENTLY, STATISTIC BLOCKS ARE RESTRICTED TO THE FOLLOWING
C      LET SUB-COMMANDS:
C
C         1. PATTERN/DATA
C         2. RANDOM NUMBERS
C         3. MATH LET SUB-COMMANDS (BUT NOT MATRIX COMMANDS)
C         4. STATISTICS LET SUB-COMMANDS
C         5. ARITHMETIC OPERATIONS
C         6. LET ... = EXECUTE ...
C
C      NOTE THAT FOLLOWING MATH LET SUB-COMMANDS ARE NOT HANDLED
C      IN DPMATC AND ARE NOT SUPPORTED IN STATISTIC BLOCKS:
C
C                 A. DERIVATIVE
C                 B. NUMERICAL DERIVATIVE
C                 C. INTEGRAL
C                 D. RUNGE-KUTTA
C                 E. OPTIMIZE
C                 F. ROOTS
C
C       NOTE THAT FOR STATISTIC LET SUB-COMMANDS, WE NEED TO BE
C       CONCERNED ABOUT RECURSION.
C
C               ********************************************
C               **  STEP 2.12--                           **
C               **  TREAT THE PATTERN GENERATION SUBCASE  **
C               ********************************************
C
      IF((IHARG(3).EQ.'PATT'.AND.IHARG2(3).EQ.'ERN ') .OR.
     1   (IHARG(3).EQ.'DATA'.AND.IHARG2(3).EQ.'    '))THEN
        IF(IHARG(1).EQ.'PLOT' .AND.
     1    (IHARG(2).EQ.'CHAR' .OR. IHARG(2).EQ.'LINE' .OR.
     1    IHARG(2).EQ.'SPIK' .OR. IHARG(2).EQ.'REGI' .OR.
     1    IHARG(2).EQ.'BAR'))GOTO1290
        ICASLE='PATT'
        CALL DPPAT(IBUGA3,IBUGQ,IFOUND,IERROR)
      ENDIF
C
 1290 CONTINUE
C
C               **************************************************
C               **  STEP 2.13--                                 **
C               **  TREAT THE RANDOM NUMBER GENERATION SUBCASE  **
C               **  (AND THE RANDOM PERMUTATION SUBCASE)        **
C               **  (AND THE BOOTSTRAP INDEX SUBCASE == THE     **
C               **  DISCRETE UNIFORM RANDOM NUMBER SUBCASE)     **
C               **************************************************
C
      CALL CKRAND(ICASRA,ILOCNU,NUMSHA,
     1            SHAPE1,SHAPE2,SHAPE3,SHAPE4,
     1            SHAPE5,SHAPE6,SHAPE7,
     1            IBUGA3,ISUBRO,IFOUNR,IERROR)
      IF(IFOUNR.EQ.'YES')THEN
        ICASLE='RAND'
        CALL DPRAND(ICASRA,ISEED,ILOCNU,NUMSHA,
     1              SHAPE1,SHAPE2,SHAPE3,SHAPE4,
     1              SHAPE5,SHAPE6,SHAPE7,
     1              IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
        GOTO9000
      ENDIF
C
C               **********************************************
C               **  STEP 2.20--                             **
C               **  TREAT THE MATH CALCULATIONS SUBCASE     **
C               **   (INPUT = A VECTOR; OUTPUT = A VECTOR)  **
C               **********************************************
C
C
        IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'SBEX')THEN
          WRITE(ICOUT,2001)
 2001     FORMAT('BEFORE CALL CKMATH')
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
C        MATH LET SUBCOMMANDS.
C
        CALL CKMATH(IBUGA3,ISUBRO,IFOUN7,ICASL7,ICASS7,ISTANR,
     1              IMSUBC,ILOCV)
        IF(IFOUN7.EQ.'YES'.AND.ICASL7.NE.'UNKN'.AND.
     1     ILOCV.GE.1)THEN
          ICASLE='MANI'
          IFOUND='NO'
          CALL DPMATC(ICASL7,ICASS7,ISTANR,ILOCV,IFTEXP,IFTORD,ISEED,
     1                IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
          IF(IFOUND.EQ.'YES')GOTO1000
C
C         DON'T SUPPORT MATRIX CALLS AS THESE MAY HAVE
C         POTENTIAL CONFLICTS WITH SCRATCH STORAGE.
C
CCCCC     CALL DPMAT2(ICASL7,ICASS7,ILOCV,
CCCCC1                ISEED,IMSUBC,
CCCCC1                IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
          GOTO1000
        ENDIF
C
C               **************************************************
C               **  STEP 2.41--                                 **
C               **  TREAT THE STATISTICAL CALCULATIONS SUBCASE  **
C               **  (INPUT = A VECTOR; OUTPUT = A PARAMETER)    **
C               **************************************************
C
        CALL DPTYP2(IANS,IWIDTH,IHNAME,IHNAM2,NUMNAM,MAXNAM,IBUGA3,
     1             IUSE,IVALUE,VALUE,IN,
     1             IFOUNZ,IBEGIN,IEND,
     1             ITYPE,IHOL,IHOL2,INT1,FLOAT1,IERRO1,
     1             NUMCL,NUMPL,NUMAOL,ITYW1L,ICAT1L,INLI1L,ITYW2L,
     1             NUMCR,NUMPR,NUMAOR,ITYW1R,ICAT1R,INLI1R,ITYW2R)
C
        IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'SBEX')THEN
          WRITE(ICOUT,3091)
 3091     FORMAT('BEFORE CALL CKARIT')
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        CALL CKARIT(IFOUNZ,IBEGIN,IANS,IWIDTH,ICASAR,IBUGA3,ISUBRO)
C
        IF(NUMARG.GE.3 .AND.
     1    (IHARG(3).EQ.'SN- ' .OR. IHARG(3).EQ.'SN+ '))ICASAR='NO'
        IF(NUMARG.GE.4 .AND. IHARG(3).EQ.'TAGU' .AND.
     1    (IHARG(4).EQ.'SN- ' .OR. IHARG(4).EQ.'SN+ '))ICASAR='NO'
        IF(NUMARG.GE.6 .AND. IHARG(3).EQ.'CHI ' .AND.
     1     IHARG(4).EQ.'SQUA'.AND. IHARG(5).EQ.'SD  ' .AND.
     1     IHARG(6).EQ.'TEST')ICASAR='NO'
        IF(NUMARG.GE.6 .AND. IHARG(3).EQ.'ONE '.AND.
     1     IHARG(4).EQ.'SAMP' .AND. IHARG(5).EQ.'T   ' .AND.
     1     IHARG(6).EQ.'TEST')ICASAR='NO'
        IF(NUMARG.GE.7 .AND. IHARG(3).EQ.'CHI ' .AND.
     1     IHARG(4).EQ.'SQUA' .AND. IHARG(5).EQ.'STAN' .AND.
     1     IHARG(6).EQ.'DEVI' .AND. IHARG(7).EQ.'TEST')ICASAR='NO'
        IF(NUMARG.GE.4 .AND. IHARG(3).EQ.'HODG' .AND.
     1     IHARG(4).EQ.'LEHM')ICASAR='NO'
        IF(NUMARG.GE.6 .AND. IHARG(5).EQ.'HODG' .AND.
     1     IHARG(6).EQ.'LEHM')ICASAR='NO'
C
        IF(ICASAR.EQ.'NO')THEN
C
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'SBEX')THEN
            WRITE(ICOUT,4001)
 4001       FORMAT('BEFORE CALL CKSTAT')
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          CALL CKSTAT(IBUGA3,IFOUN8,ICASL8,ILOCV,ISTANR)
          IF(IFOUN8.EQ.'YES'.AND.ICASL8.NE.'UNKN'.AND.
     1       ILOCV.GE.1)THEN
            ICASLE='STAT'
            CALL DPSTC2(ICASL8,ILOCV,ISTANR,
     1                  IFOUNZ,IBEGIN,IEND,ITYPE,IHOL,IHOL2,INT1,
     1                  FLOAT1,IERRO1,
     1                  TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6,MAXOBV,
     1                  ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1                  DTEMP1,DTEMP2,DTEMP3,
     1                  IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
            GOTO1000
          ENDIF
        ENDIF
C
C               **********************************************
C               **  STEP 2.19A--                            **
C               **  TREAT THE EXECUTE              SUBCASE  **
C               **********************************************
C
      IF(IHARG(2).EQ.'=   '.AND.IHARG(3).EQ.'EXEC')THEN
        ICASLE='EXEC'
        IFOUND='YES'
        ITYPEL='V'
C
C       EXTRACT CURRENT PARAMETER LIST FOR FUNCTION BLOCK
C
        ICNT=0
        DO2190K=1,ISBCN2
          IH='    '
          IH2='    '
          IF(IFLAG.EQ.1)THEN
            IH=ISBPL1(K)(1:4)
            IH2=ISBPL1(K)(5:8)
          ELSEIF(IFLAG.EQ.2)THEN
            IH=ISBPL2(K)(1:4)
            IH2=ISBPL2(K)(5:8)
          ELSEIF(IFLAG.EQ.3)THEN
            IH=ISBPL3(K)(1:4)
            IH2=ISBPL3(K)(5:8)
          ENDIF
C
          DO2195II=1,NUMNAM
            IF(IH.EQ.IHNAME(II) .AND. IH2.EQ.IHNAM2(II) .AND.
     1         IUSE(II).EQ.'P')THEN
              ICNT=ICNT+1
              TEMP1(ICNT)=VALUE(II)
              GOTO2190
            ENDIF
 2195     CONTINUE
 2190     CONTINUE
C
        CALL DPEXFI(TEMP1,ICNT,IBUGA3,ISUBRO,IFOUND,IERROR)
        GOTO1000
      ENDIF
C
C               *********************************************
C               **  STEP 2.50--                            **
C               **  TREAT THE FUNCTION EVALUATION SUBCASE  **
C               *********************************************
C
C       DON'T SUPPORT THIS AS IT IS MORE USEFUL TO CALL DPSBEX FROM
C       DPFUEV (I.E., ALLOW FUNCTION EVALUATION TO HANDLE FUNCTION
C       BLOCKS).
C
        IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'SBEX')THEN
          WRITE(ICOUT,5001)
 5001     FORMAT('BEFORE CALL DPFUEV')
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        ICASLE='FUNC'
        CALL DPFUEV(ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,
     1              IA,PARAM,IPARN,IPARN2,
     1              IFOUNZ,IBEGIN,IEND,ITYPE,IHOL,IHOL2,INT1,
     1              FLOAT1,IERRO1,
     1              NUMCL,NUMPL,NUMAOL,ITYW1L,ICAT1L,INLI1L,ITYW2L,
     1              NUMCR,NUMPR,NUMAOR,ITYW1R,ICAT1R,INLI1R,ITYW2R,
     1              IANGLU,
     1              IBUGA3,IBUGCO,IBUGEV,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IFOUND.EQ.'YES')GOTO1000
C
C       ADMISSABLE LET COMMAND NOT FOUND
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6001)
 6001   FORMAT('      COMMAND IS NOT SUPPORTED IN FUNCTION BLOCK')
        IERROR='YES'
        GOTO9000
C
 1000 CONTINUE
C
C
C               **************************************
C               **  STEP 3--                        **
C               **  RESET ORIGINAL COMMAND LINE     **
C               **************************************
C
      DO7010II=1,MAXSTR
        IANSLC(II)=IANSSV(II)
 7010 CONTINUE
C
      CALL DPNONP(IANSLC,IWIDTH,IANSLC,IBUGA2,ISUBRO,IERROR)
      CALL DPREP2(IANSLC,IWIDTH,
     1            IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,
     1            IVARLB,IROWLB,MAXOBV,
     1            IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,IMALEV,
     1            IBUGA2,ISUBRO,IERROR)
      CALL DPUPPE(IANSLC,IWIDTH,IANS,IBUGA2,IERROR)
      CALL DPTYPE(IANSLC,IWIDTH,IBUGA2,
     1            ICOM,ICOM2,ICOMT,ICOMI,ACOM,ICOMLC,ICOML2,
     1            IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,
     1            IHARG,IHARG2,IARGT,IARG,ARG,IHARLC,IHARL2,NUMARG,
     1            IHOST1,IHOST2)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'SBEX')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSBEX--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)ICASLE,IMSUBC
 9016   FORMAT('ICASLE,IMSUBC = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9017)IFOUND,IERROR
 9017   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPSBLI(ICOM,IHARG,IARGT,ARG,NUMARG,
     1ASUBXL,ASUBXU,ASUBYL,ASUBYU,
     1MAXSUB,
     1IFOUND,IERROR)
C
C     PURPOSE--DEFINE LIMITS FOR SUBREGIONS.
C                 SUBREGION XLIMITS 10 20
C                 SUBREGION YLIMITS 10 20
C                 SUBREGION 1 YLIMITS 10 20
C                 SUBREGION 2 YLIMITS 10 20
C     INPUT  ARGUMENTS--ICOM  (A  HOLLERITH VARIABLE)
C                     --IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --ARG    (A  FLOATING POINT VECTOR)
C                     --NUMARG
C     OUTPUT ARGUMENTS--
C                     --ASUBXL = COORDINATE FOR LOWER X LIMIT
C                     --ASUBXU = COORDINATE FOR UPPER X LIMIT
C                     --ASUBYL = COORDINATE FOR LOWER Y LIMIT
C                     --ASUBYU = COORDINATE FOR UPPER Y LIMIT
C                     --MAXSUB = MAXIMUM NUMBER OF SUBREGIONS
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
C     VERSION NUMBER--99/11
C     ORIGINAL VERSION--NOVEMBER  1999.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
      CHARACTER*4 IARGT
C
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
      DIMENSION IARGT(*)
      DIMENSION ARG(*)
C
      DIMENSION ASUBXL(*)
      DIMENSION ASUBXU(*)
      DIMENSION ASUBYL(*)
      DIMENSION ASUBYU(*)
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
      IF(ICOM.NE.'SUBR')THEN
        IFOUND='NO'
        GOTO9000
      ENDIF
C
      IF(NUMARG.LE.0)THEN
        GOTO9000
      ENDIF
      IF(IHARG(NUMARG).EQ.'?')GOTO8100
C
C               *****************************************************
C               **  CHECK IF THE FIRST ARGUMENT IS NUMERIC         **
C               **  (THIS SHOULD DEFINE WHICH SUBREGION IS BEING   **
C               **  SET)                                           **
C               *****************************************************
C
      IF(IARGT(1).EQ.'NUMB')THEN
        ISUBID=INT(ARG(1)+0.5)
        IF(ISUBID.LT.1 .OR. ISUBID.GT.MAXSUB)ISUBID=1
        IWORD=2
      ELSE
        IWORD=1
        ISUBID=1
      ENDIF
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  THE HORIZONTAL SUBREGION LIMITS ARE TO BE FIXED**
C               *****************************************************
C
      IF(IHARG(IWORD).EQ.'XLIM')GOTO1100
      GOTO1199
C
 1100 CONTINUE
      IF(NUMARG.LE.IWORD)GOTO1110
      IF(IHARG(IWORD+1).EQ.'DEFA')GOTO1110
      IF(IARGT(IWORD+1).EQ.'NUMB'.AND.IARGT(IWORD+2).EQ.'NUMB')GOTO1120
      GOTO1110
C
 1110 CONTINUE
      IFOUND='YES'
      ASUBXL(ISUBID)=CPUMIN
      ASUBXU(ISUBID)=CPUMAX
C
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1115)ISUBID
 1115   FORMAT('THE X LIMITS FOR SUB-REGION ',I8,' HAVE JUST BEEN SET')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1117)
 1117   FORMAT('TO THE FULL PLOT AREA.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GOTO9000
C
 1120 CONTINUE
      IFOUND='YES'
      ASUBXL(ISUBID)=ARG(IWORD+1)
      ASUBXU(ISUBID)=ARG(IWORD+2)
      IF(ASUBXL(ISUBID).GT.ASUBXU(ISUBID))THEN
        ATEMP=ASUBXL(ISUBID)
        ASUBXL(ISUBID)=ASUBXU(ISUBID)
        ASUBXU(ISUBID)=ATEMP
      ENDIF
C
      IF(IFEEDB.EQ.'OFF')GOTO1129
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1125)ISUBID
 1125 FORMAT('THE SUBREGION X LIMITS FOR SUBREGION ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1126)ASUBXL(ISUBID),ASUBXU(ISUBID)
 1126 FORMAT('HAVE JUST BEEN SET TO ',E15.7,E15.7)
      CALL DPWRST('XXX','BUG ')
 1129 CONTINUE
      GOTO9000
C
 1199 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  THE VERTICAL   SUBREGION LIMITS ARE TO BE FIXED**
C               *****************************************************
C
      IF(IHARG(IWORD).EQ.'YLIM')GOTO2100
      GOTO2199
C
 2100 CONTINUE
      IF(NUMARG.LE.IWORD)GOTO2110
      IF(IHARG(IWORD+1).EQ.'DEFA')GOTO2110
      IF(IARGT(IWORD+1).EQ.'NUMB'.AND.IARGT(IWORD+2).EQ.'NUMB')GOTO2120
      GOTO2110
C
 2110 CONTINUE
      IFOUND='YES'
      ASUBYL(ISUBID)=CPUMIN
      ASUBYU(ISUBID)=CPUMAX
C
      IF(IFEEDB.EQ.'OFF')GOTO2119
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2115)ISUBID
 2115 FORMAT('THE Y LIMITS FOR SUB-REGION ',I8,' HAVE JUST BEEN SET')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2117)
 2117 FORMAT('TO THE FULL PLOT AREA.')
      CALL DPWRST('XXX','BUG ')
 2119 CONTINUE
      GOTO9000
C
 2120 CONTINUE
      IFOUND='YES'
      ASUBYL(ISUBID)=ARG(IWORD+1)
      ASUBYU(ISUBID)=ARG(IWORD+2)
      IF(ASUBYL(ISUBID).GT.ASUBYU(ISUBID))THEN
        ATEMP=ASUBYL(ISUBID)
        ASUBYL(ISUBID)=ASUBYU(ISUBID)
        ASUBYU(ISUBID)=ATEMP
      ENDIF
C
      IF(IFEEDB.EQ.'OFF')GOTO2129
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2125)ISUBID
 2125 FORMAT('THE SUBREGION Y LIMITS FOR SUBREGION ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2126)ASUBYL(ISUBID),ASUBYU(ISUBID)
 2126 FORMAT('HAVE JUST BEEN SET TO ',E15.7,E15.7)
      CALL DPWRST('XXX','BUG ')
 2129 CONTINUE
      GOTO9000
C
 2199 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN BOTH THE HORIZONTAL AND    **
C               **  VERTICAL SUBREGION LIMITS ARE TO BE FIXED      **
C               *****************************************************
C
      IF(IHARG(IWORD).EQ.'LIMI')GOTO3100
      GOTO3199
C
 3100 CONTINUE
      IF(NUMARG.LE.IWORD)GOTO3110
      IF(IHARG(IWORD+1).EQ.'DEFA')GOTO3110
      IF(IARGT(IWORD+1).EQ.'NUMB'.AND.IARGT(IWORD+2).EQ.'NUMB')GOTO3120
      GOTO3110
C
 3110 CONTINUE
      IFOUND='YES'
      ASUBXL(ISUBID)=CPUMIN
      ASUBXU(ISUBID)=CPUMAX
      ASUBYL(ISUBID)=CPUMIN
      ASUBYU(ISUBID)=CPUMAX
C
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3115)ISUBID
 3115   FORMAT('THE LIMITS FOR SUB-REGION ',I8,' HAVE JUST BEEN SET')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3117)
 3117   FORMAT('TO THE FULL PLOT AREA.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GOTO9000
C
 3120 CONTINUE
      IFOUND='YES'
      ASUBXL(ISUBID)=ARG(IWORD+1)
      ASUBXU(ISUBID)=ARG(IWORD+2)
      ASUBYL(ISUBID)=ARG(IWORD+1)
      ASUBYU(ISUBID)=ARG(IWORD+2)
      IF(ASUBYL(ISUBID).GT.ASUBYU(ISUBID))THEN
        ATEMP=ASUBYL(ISUBID)
        ASUBYL(ISUBID)=ASUBYU(ISUBID)
        ASUBYU(ISUBID)=ATEMP
      ENDIF
      IF(ASUBXL(ISUBID).GT.ASUBXU(ISUBID))THEN
        ATEMP=ASUBXL(ISUBID)
        ASUBXL(ISUBID)=ASUBXU(ISUBID)
        ASUBXU(ISUBID)=ATEMP
      ENDIF
C
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3125)ISUBID
 3125   FORMAT('THE SUBREGION Y LIMITS FOR SUBREGION ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3126)ASUBYL(ISUBID),ASUBYU(ISUBID)
 3126   FORMAT('HAVE JUST BEEN SET TO ',2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3126)ASUBXL(ISUBID),ASUBXU(ISUBID)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GOTO9000
C
 3199 CONTINUE
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
      DO8105I=1,MAXSUB
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8111)I
 8111 FORMAT('THE CURRENT SUBREGION ',I5,' LIMITS ARE ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8112)ASUBXL(I),ASUBXU(I)
 8112 FORMAT('            --XLIMITS        = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8113)ASUBYL(I),ASUBYU(I)
 8113 FORMAT('            --YLIMITS        = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
 8105 CONTINUE
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
      SUBROUTINE DPSBSP(IFOUNO,IOP,XEND,YEND,HEIGHT,WIDTH,
     1                  PHEIGH,PWIDTH,PVEGAP,PHOGAP,
     1                  PHEIG2,PWIDT2,PVEGA2,PHOGA2,
     1                  ANGLE,AMAX,
     1                  IBUGD2,IERROR)
C
C     PURPOSE--ADJUST XEND, YEND, HEIGHT, AND WIDTH
C              WHEN ENTERING OR EXITING
C              SUBSCRIPT OR SUPERSCRIPT MODE.
C     NOTE--THE INPUT ARGUMENTS XEND, YEND, HEIGHT, AND WIDTH
C           MAY BE CHANGED BY THIS SUBROUTINE.
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
C     UPDATED         --MARCH     2001. ALLOW SCALE FACTORS FOR
C                                       SIZE OF SUPER/SUB/SCRIPTS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IFOUNO
      CHARACTER*4 IOP
      CHARACTER*4 IBUGD2
      CHARACTER*4 IERROR
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IERROR='NO'
      SUBFAC=0.15
      SUPFAC=0.50
C
      IF(IBUGD2.EQ.'ON' .OR. ISUBG4.EQ.'SBSP')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPSBSP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IFOUNO,IOP
   52   FORMAT('IFOUNO,IOP = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)XEND,YEND,HEIGHT,WIDTH
   53   FORMAT('XEND,YEND,HEIGHT,WIDTH = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)SUBFAC,SUPFAC,PSUPXS,PSUPYS
   55   FORMAT('SUBFAC,SUPFAC,PSUPXS,PSUPYS = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,56)PHEIGH,PWIDTH,PVEGAP,PHOGAP
   56   FORMAT('PHEIGH,PWIDTH,PVEGAP,PHOGAP = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,57)PHEIG2,PWIDT2,PVEGA2,PHOGA2
   57   FORMAT('PHEIG2,PWIDT2,PVEGA2,PHOGA2 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,58)ANGLE,AMAX
   58   FORMAT('ANGLE,AMAX = ',2E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,69)IBUGG4,ISUBG4,IERRG4
   69   FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      THETA=(ANGLE/AMAX)*2.0*3.1315926
C
      IF(IFOUNO.EQ.'NO')GOTO1190
C
      IF(IOP.EQ.'SUB')GOTO1110
      IF(IOP.EQ.'UNSB')GOTO1120
      IF(IOP.EQ.'SUP')GOTO1130
      IF(IOP.EQ.'UNSP')GOTO1140
      GOTO1190
C
 1110 CONTINUE
CCCCC YEND=YEND-SUBFAC*HEIGHT
      XEND=XEND+SUBFAC*HEIGHT*SIN(THETA)
      YEND=YEND-SUBFAC*HEIGHT*COS(THETA)
CCCCC HEIGHT=HEIGHT/2.0
CCCCC WIDTH=WIDTH/2.0
CCCCC PHEIGH=PHEIGH/2.0
CCCCC PWIDTH=PWIDTH/2.0
CCCCC PVEGAP=PVEGAP/2.0
CCCCC PHOGAP=PHOGAP/2.0
CCCCC PHEIG2=PHEIG2/2.0
CCCCC PWIDT2=PWIDT2/2.0
CCCCC PVEGA2=PVEGA2/2.0
CCCCC PHOGA2=PHOGA2/2.0
      HEIGHT=HEIGHT/2.0
      WIDTH=WIDTH*PSUPXS
      PHEIGH=PHEIGH*PSUPYS
      PWIDTH=PWIDTH*PSUPXS
      PVEGAP=PVEGAP*PSUPYS
      PHOGAP=PHOGAP*PSUPXS
      PHEIG2=PHEIG2*PSUPYS
      PWIDT2=PWIDT2*PSUPXS
      PVEGA2=PVEGA2*PSUPYS
      PHOGA2=PHOGA2*PSUPXS
      GOTO1190
C
 1120 CONTINUE
CCCCC HEIGHT=HEIGHT*2.0
CCCCC WIDTH=WIDTH*2.0
CCCCC PHEIGH=PHEIGH*2.0
CCCCC PWIDTH=PWIDTH*2.0
CCCCC PVEGAP=PVEGAP*2.0
CCCCC PHOGAP=PHOGAP*2.0
CCCCC PHEIG2=PHEIG2*2.0
CCCCC PWIDT2=PWIDT2*2.0
CCCCC PVEGA2=PVEGA2*2.0
CCCCC PHOGA2=PHOGA2*2.0
      HEIGHT=HEIGHT*(1.0/PSUPYS)
      WIDTH=WIDTH*(1.0/PSUPXS)
      PHEIGH=PHEIGH*(1.0/PSUPYS)
      PWIDTH=PWIDTH*(1.0/PSUPXS)
      PVEGAP=PVEGAP*(1.0/PSUPYS)
      PHOGAP=PHOGAP*(1.0/PSUPXS)
      PHEIG2=PHEIG2*(1.0/PSUPYS)
      PWIDT2=PWIDT2*(1.0/PSUPXS)
      PVEGA2=PVEGA2*(1.0/PSUPYS)
      PHOGA2=PHOGA2*(1.0/PSUPXS)
CCCCC YEND=YEND+SUBFAC*HEIGHT
      XEND=XEND-SUBFAC*HEIGHT*SIN(THETA)
      YEND=YEND+SUBFAC*HEIGHT*COS(THETA)
      GOTO1190
C
 1130 CONTINUE
CCCCC YEND=YEND+SUPFAC*HEIGHT
      XEND=XEND-SUPFAC*HEIGHT*SIN(THETA)
      YEND=YEND+SUPFAC*HEIGHT*COS(THETA)
CCCCC HEIGHT=HEIGHT/2.0
CCCCC WIDTH=WIDTH/2.0
CCCCC PHEIGH=PHEIGH/2.0
CCCCC PWIDTH=PWIDTH/2.0
CCCCC PVEGAP=PVEGAP/2.0
CCCCC PHOGAP=PHOGAP/2.0
CCCCC PHEIG2=PHEIG2/2.0
CCCCC PWIDT2=PWIDT2/2.0
CCCCC PVEGA2=PVEGA2/2.0
CCCCC PHOGA2=PHOGA2/2.0
      HEIGHT=HEIGHT*PSUPYS
      WIDTH=WIDTH*PSUPXS
      PHEIGH=PHEIGH*PSUPYS
      PWIDTH=PWIDTH*PSUPXS
      PVEGAP=PVEGAP*PSUPYS
      PHOGAP=PHOGAP*PSUPXS
      PHEIG2=PHEIG2*PSUPYS
      PWIDT2=PWIDT2*PSUPXS
      PVEGA2=PVEGA2*PSUPYS
      PHOGA2=PHOGA2*PSUPXS
      GOTO1190
C
 1140 CONTINUE
CCCCC HEIGHT=HEIGHT*2.0
CCCCC WIDTH=WIDTH*2.0
CCCCC PHEIGH=PHEIGH*2.0
CCCCC PWIDTH=PWIDTH*2.0
CCCCC PVEGAP=PVEGAP*2.0
CCCCC PHOGAP=PHOGAP*2.0
CCCCC PHEIG2=PHEIG2*2.0
CCCCC PWIDT2=PWIDT2*2.0
CCCCC PVEGA2=PVEGA2*2.0
CCCCC PHOGA2=PHOGA2*2.0
      HEIGHT=HEIGHT*(1.0/PSUPYS)
      WIDTH=WIDTH*(1.0/PSUPXS)
      PHEIGH=PHEIGH*(1.0/PSUPYS)
      PWIDTH=PWIDTH*(1.0/PSUPXS)
      PVEGAP=PVEGAP*(1.0/PSUPYS)
      PHOGAP=PHOGAP*(1.0/PSUPXS)
      PHEIG2=PHEIG2*(1.0/PSUPYS)
      PWIDT2=PWIDT2*(1.0/PSUPXS)
      PVEGA2=PVEGA2*(1.0/PSUPYS)
      PHOGA2=PHOGA2*(1.0/PSUPXS)
CCCCC YEND=YEND-SUPFAC*HEIGHT
      XEND=XEND+SUPFAC*HEIGHT*SIN(THETA)
      YEND=YEND-SUPFAC*HEIGHT*COS(THETA)
      GOTO1190
C
 1190 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
      IF(IBUGG4.EQ.'ON' .OR. ISUBG4.EQ.'SBSP')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSBSP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUNO,IOP,IERRG4
 9012   FORMAT('IFOUNO,IOP,IERRG4 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)XEND,YEND,HEIGHT,WIDTH
 9013   FORMAT('XEND,YEND,HEIGHT,WIDTH = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)SUBFAC,SUPFAC,ANGLE,AMAX,THETA
 9015   FORMAT('SUBFAC,SUPFAC,ANGLE,AMAX,THETA = ',5G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)PHEIGH,PWIDTH,PVEGAP,PHOGAP
 9016   FORMAT('PHEIGH,PWIDTH,PVEGAP,PHOGAP = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9017)PHEIG2,PWIDT2,PVEGA2,PHOGA2
 9017   FORMAT('PHEIG2,PWIDT2,PVEGA2,PHOGA2 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPSBSW(IHARG,NUMARG,IDEFSB,MAXSUB,ISUBSW,
     1                  IBUGP2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE SUB-REGION SWITCHES.
C              THESE ARE LOCATED IN THE VECTOR ISUBSW(.).
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --NUMARG
C                     --IDEFSB
C                     --MAXSUB
C                     --IBUGP2 ('ON' OR 'OFF' )
C     OUTPUT ARGUMENTS--ISUBSW (A CHARACTER VECTOR)
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
C     VERSION NUMBER--99/11
C     ORIGINAL VERSION--NOVEMBER  1999.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IDEFSB
      CHARACTER*4 ISUBSW
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
      DIMENSION ISUBSW(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPSB'
      ISUBN2='SW  '
C
      NUMSUB=0
      IHOLD1='-999'
      IHOLD2='-999'
C
      IF(IBUGP2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPSBSW--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGP2,IFOUND,IERROR
   52 FORMAT('IBUGP2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)MAXSUB,NUMSUB
   53 FORMAT('MAXSUB,NUMSUB = ',I8,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)IHOLD1,IHOLD2
   54 FORMAT('IHOLD1,IHOLD2 = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,55)IDEFSB
   55 FORMAT('IDEFSB = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,60)NUMARG
   60 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO65I=1,NUMARG
      WRITE(ICOUT,66)IHARG(I)
   66 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,70)ISUBSW(1)
   70 FORMAT('ISUBSW(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO75I=1,10
      WRITE(ICOUT,76)I,ISUBSW(I)
   76 FORMAT('I,ISUBSW(I) = ',I8,2X,A4)
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
      IF(NUMARG.LE.0)GOTO1100
      IF(NUMARG.EQ.1)GOTO1110
      IF(NUMARG.EQ.2)GOTO1120
      GOTO1130
C
 1100 CONTINUE
      GOTO1200
C
 1110 CONTINUE
      IF(IHARG(1).EQ.'ALL')IHOLD1='OFF'
      IF(IHARG(1).EQ.'ALL')GOTO1300
      GOTO1200
C
 1120 CONTINUE
      IF(IHARG(1).EQ.'ALL')IHOLD1=IHARG(2)
      IF(IHARG(1).EQ.'ALL')GOTO1300
      IF(IHARG(2).EQ.'ALL')IHOLD1=IHARG(1)
      IF(IHARG(2).EQ.'ALL')GOTO1300
      GOTO1200
C
 1130 CONTINUE
      GOTO1200
C
C               *************************************************
C               **  STEP 2--                                   **
C               **  TREAT THE INDIVIDUAL SPECIFICATIONS CASE  **
C               *************************************************
C
 1200 CONTINUE
      ISTEPN='2'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LE.0)GOTO1210
      GOTO1220
C
 1210 CONTINUE
      NUMSUB=1
      ISUBSW(1)='ON'
      GOTO1270
C
 1220 CONTINUE
      NUMSUB=NUMARG
      IF(NUMSUB.GT.MAXSUB)NUMSUB=MAXSUB
      DO1225I=1,NUMSUB
      J=I
      IHOLD1=IHARG(J)
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2='ON'
      IF(IHOLD1.EQ.'OFF')IHOLD2='OFF'
      ISUBSW(I)=IHOLD2
 1225 CONTINUE
      GOTO1270
C
 1270 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1279
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO1278I=1,NUMSUB
      WRITE(ICOUT,1276)I,ISUBSW(I)
 1276 FORMAT('SUBREGION ',I6,' HAS JUST BEEN SET TO ',
     1A4)
      CALL DPWRST('XXX','BUG ')
 1278 CONTINUE
 1279 CONTINUE
      IFOUND='YES'
      GOTO9000
C
C               **************************
C               **  STEP 2--            **
C               **  TREAT THE ALL CASE  **
C               **************************
C
 1300 CONTINUE
      ISTEPN='3'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMSUB=MAXSUB
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2='ON'
      IF(IHOLD1.EQ.'OFF')IHOLD2='OFF'
      DO1315I=1,NUMSUB
      ISUBSW(I)=IHOLD2
 1315 CONTINUE
      GOTO1370
C
 1370 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1316)ISUBSW(I)
 1316 FORMAT('ALL SPIKES HAVE JUST BEEN SET TO ',
     1A4)
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
 9011 FORMAT('***** AT THE END       OF DPSBSW--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGP2,IFOUND,IERROR
 9012 FORMAT('IBUGP2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)MAXSUB,NUMSUB
 9013 FORMAT('MAXSUB,NUMSUB = ',I8,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)IHOLD1,IHOLD2
 9014 FORMAT('IHOLD1,IHOLD2 = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)IDEFSB
 9015 FORMAT('IDEFSB = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9020)NUMARG
 9020 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO9025I=1,NUMARG
      WRITE(ICOUT,9026)IHARG(I)
 9026 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      WRITE(ICOUT,9030)ISUBSW(1)
 9030 FORMAT('ISUBSW(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO9035I=1,10
      WRITE(ICOUT,9036)I,ISUBSW(I)
 9036 FORMAT('I,ISUBSW(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPSCAN(ISTART,ISTRIN,NUMCHS,ILOCR2,
     1ICHAR2,IOP,IFONT,ICASE,IJUST,ISEQUE,ISUBSU,
     1HMAX,VMAX,AMAX,X0,Y0,ANGLE,WIDTH,HEIGHT,
     1IEND,IFOUNC,IFOUNO,IBUGD2,IERROR)
C
C     PURPOSE--SCAN THE STRING IN ISTRIN(.) STARTING WITH POSITION ISTART.
C                EXAMINE THE NEXT 6 CHARACTERS AT MOST.
C                COPY AND PACK THE NEXT 4 CHARACTERS INTO IWORD1.
C           IF () FOUND IN NEXT 6 CHARACTERS, THEN STRIP OFF ()
C            AND SAVE PREVIOUS INTO IWORD1 (PACKED).
C      |      IF() NOT FOUND, THEN OUTPUT A SINGLE CHARACTER IN IWORD1.
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
C     ORIGINAL VERSION--JANUARY   1981.
C     UPDATED         --OCTOBER   1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --APRIL     1987.
C     UPDATED         --AUGUST    1992.  ADDITIONAL SYMBOLS
C     UPDATED         --FEBRUARY  1995.  CONVERT IWORD1 TO UPPER CASE
C                                        (CASE ASIS COMPLICATION)
C     UPDATED         --NOVEMBER  1996.  COMPILE ERROR FOR LINIX G77
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ISTRIN
      CHARACTER*4 ICHAR2
      CHARACTER*4 IOP
      CHARACTER*4 IFONT
      CHARACTER*4 ICASE
      CHARACTER*4 IJUST
      CHARACTER*4 ISEQUE
      CHARACTER*4 ISUBSU
      CHARACTER*4 IFOUNC
      CHARACTER*4 IFOUNO
      CHARACTER*4 IBUGD2
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWORD1
      CHARACTER*4 IXXXXX
      CHARACTER*4 IFOULR
      CHARACTER*4 IOPERT
      CHARACTER*4 IGREET
      CHARACTER*4 IMATHT
C
CCCCC CHARACTER*4 ICHAR3
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION ISTRIN(*)
C
      DIMENSION IOPERT(50)
      DIMENSION IGREET(25)
      DIMENSION IMATHT(200)
C
      DIMENSION IOPERN(50)
      DIMENSION IGREEN(25)
      DIMENSION IMATHN(200)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----DATA STATEMENTS-------------------------------------------------
C
C               *************************
C               **  DEFINE OPERATIONS  **
C               *************************
C
      DATA IOPERT( 1)   /'SIMP'/
      DATA IOPERT( 2)   /'DUPL'/
      DATA IOPERT( 3)   /'TRIP'/
      DATA IOPERT( 4)   /'COMP'/
      DATA IOPERT( 5)   /'TRII'/
      DATA IOPERT( 6)   /'COMI'/
      DATA IOPERT( 7)   /'SIMS'/
      DATA IOPERT( 8)   /'COMS'/
C
      DATA IOPERT( 9)   /'UC  '/
      DATA IOPERT(10)   /'CAP '/
      DATA IOPERT(11)   /'CAPS'/
      DATA IOPERT(12)   /'LC  '/
C
      DATA IOPERT(13)   /'LJUS'/
      DATA IOPERT(14)   /'CJUS'/
      DATA IOPERT(15)   /'RJUS'/
C
      DATA IOPERT(16)   /'SEQ '/
      DATA IOPERT(17)   /'UNSQ'/
C
      DATA IOPERT(18)   /'SUB '/
      DATA IOPERT(19)   /'UNSB'/
      DATA IOPERT(20)   /'SUP '/
      DATA IOPERT(21)   /'UNSP'/
C
      DATA IOPERT(22)   /'HMAX'/
      DATA IOPERT(23)   /'VMAX'/
      DATA IOPERT(24)   /'ANGL'/
      DATA IOPERT(25)   /'HEIG'/
      DATA IOPERT(26)   /'WIDT'/
      DATA IOPERT(27)   /'ANGL'/
C
      DATA IOPERT(28)   /'MOVE'/
      DATA IOPERT(29)   /'DRAW'/
      DATA IOPERT(30)   /'RELM'/
      DATA IOPERT(31)   /'RELD'/
C
      DATA IOPERT(32)   /'BACK'/
      DATA IOPERT(33)   /'OVER'/
      DATA IOPERT(34)   /'UP  '/
      DATA IOPERT(35)   /'DOWN'/
      DATA IOPERT(36)   /'TAB '/
      DATA IOPERT(37)   /'RETU'/
C
C               *******************************
C               **  DEFINE GREEK CHARACTERS  **
C               *******************************
C
      DATA IGREET( 1)   /'ALPH'/
      DATA IGREET( 2)   /'BETA'/
      DATA IGREET( 3)   /'GAMM'/
      DATA IGREET( 4)   /'DELT'/
      DATA IGREET( 5)   /'EPSI'/
      DATA IGREET( 6)   /'ZETA'/
      DATA IGREET( 7)   /'ETA '/
      DATA IGREET( 8)   /'THET'/
      DATA IGREET( 9)   /'IOTA'/
      DATA IGREET(10)   /'KAPP'/
      DATA IGREET(11)   /'LAMB'/
      DATA IGREET(12)   /'MU  '/
      DATA IGREET(13)   /'NU  '/
      DATA IGREET(14)   /'XI  '/
      DATA IGREET(15)   /'OMIC'/
      DATA IGREET(16)   /'PI  '/
      DATA IGREET(17)   /'RHO '/
      DATA IGREET(18)   /'SIGM'/
      DATA IGREET(19)   /'TAU '/
      DATA IGREET(20)   /'UPSI'/
      DATA IGREET(21)   /'PHI '/
      DATA IGREET(22)   /'CHI '/
      DATA IGREET(23)   /'PSI '/
      DATA IGREET(24)   /'OMEG'/
C
C               ***************************
C               **  DEFINE MATH SYMBOLS  **
C               ***************************
C
      DATA IMATHT( 1)   /'HASP'/
      DATA IMATHT( 2)   /'SPAC'/
      DATA IMATHT( 3)   /'SP  '/
      DATA IMATHT( 4)   /'LAPO'/
      DATA IMATHT( 5)   /'RAPO'/
      DATA IMATHT( 6)   /'LBRA'/
      DATA IMATHT( 7)   /'RBRA'/
      DATA IMATHT( 8)   /'LCBR'/
      DATA IMATHT( 9)   /'RCBR'/
      DATA IMATHT(10)   /'LELB'/
      DATA IMATHT(11)   /'RELB'/
      DATA IMATHT(12)   /'+-  '/
      DATA IMATHT(13)   /'-+  '/
      DATA IMATHT(14)   /'TIME'/
      DATA IMATHT(15)   /'DOTP'/
      DATA IMATHT(16)   /'DIVI'/
      DATA IMATHT(17)   /'NOT='/
      DATA IMATHT(18)   /'EQUI'/
      DATA IMATHT(19)   /'LT  '/
      DATA IMATHT(20)   /'GT  '/
      DATA IMATHT(21)   /'LTEQ'/
      DATA IMATHT(22)   /'GTEQ'/
      DATA IMATHT(23)   /'VARI'/
      DATA IMATHT(24)   /'APPR'/
      DATA IMATHT(25)   /'TILD'/
      DATA IMATHT(26)   /'CARA'/
      DATA IMATHT(27)   /'RACC'/
      DATA IMATHT(28)   /'PRIM'/
      DATA IMATHT(29)   /'LACC'/
      DATA IMATHT(30)   /'BREV'/
      DATA IMATHT(31)   /'RQUO'/
      DATA IMATHT(32)   /'LQUO'/
      DATA IMATHT(33)   /'NASP'/
      DATA IMATHT(34)   /'IASP'/
      DATA IMATHT(35)   /'RADI'/
      DATA IMATHT(36)   /'LRAD'/
      DATA IMATHT(37)   /'BRAD'/
      DATA IMATHT(38)   /'SUBS'/
      DATA IMATHT(39)   /'SUPE'/
      DATA IMATHT(40)   /'UNIO'/
      DATA IMATHT(41)   /'INTR'/
      DATA IMATHT(42)   /'ELEM'/
      DATA IMATHT(43)   /'RARR'/
      DATA IMATHT(44)   /'LARR'/
      DATA IMATHT(45)   /'UARR'/
      DATA IMATHT(46)   /'DARR'/
      DATA IMATHT(47)   /'PART'/
      DATA IMATHT(48)   /'INTE'/
      DATA IMATHT(49)   /'CINT'/
      DATA IMATHT(50)   /'SUMM'/
      DATA IMATHT(51)   /'PROD'/
      DATA IMATHT(52)   /'INFI'/
      DATA IMATHT(53)   /'PARA'/
      DATA IMATHT(54)   /'DAGG'/
      DATA IMATHT(55)   /'DDAG'/
      DATA IMATHT(56)   /'THEX'/
      DATA IMATHT(57)   /'THFO'/
      DATA IMATHT(58)   /'VBAR'/
      DATA IMATHT(59)   /'DVBA'/
      DATA IMATHT(60)   /'LVBA'/
      DATA IMATHT(61)   /'HBAR'/
      DATA IMATHT(62)   /'LHBA'/
      DATA IMATHT(63)   /'HHBA'/
      DATA IMATHT(64)   /'BAR '/
      DATA IMATHT(65)   /'DEL '/
C
      DATA IMATHT(66)   /'ZZZZ'/
      DATA IMATHT(67)   /'ZZZZ'/
      DATA IMATHT(68)   /'ZZZZ'/
      DATA IMATHT(69)   /'ZZZZ'/
      DATA IMATHT(70)   /'ZZZZ'/
C
      DATA IMATHT(71)   /'.   '/
      DATA IMATHT(72)   /'POIN'/
      DATA IMATHT(73)   /'PO  '/
      DATA IMATHT(74)   /'PT  '/
      DATA IMATHT(75)   /'CIRC'/
      DATA IMATHT(76)   /'CI  '/
      DATA IMATHT(77)   /'SQUA'/
      DATA IMATHT(78)   /'SQ  '/
      DATA IMATHT(79)   /'TRIA'/
      DATA IMATHT(80)   /'TR  '/
      DATA IMATHT(81)   /'DIAM'/
      DATA IMATHT(82)   /'DI  '/
      DATA IMATHT(83)   /'STAR'/
      DATA IMATHT(84)   /'ST  '/
      DATA IMATHT(85)   /'*   '/
      DATA IMATHT(86)   /'ASTE'/
      DATA IMATHT(87)   /'AS  '/
      DATA IMATHT(88)   /'TRIR'/
      DATA IMATHT(89)   /'TRII'/
      DATA IMATHT(90)   /'BARU'/
      DATA IMATHT(91)   /'BU  '/
      DATA IMATHT(92)   /'BARV'/
      DATA IMATHT(93)   /'BV  '/
      DATA IMATHT(94)   /'BARH'/
      DATA IMATHT(95)   /'BH  '/
      DATA IMATHT(96)   /'ARRU'/
      DATA IMATHT(97)   /'AU  '/
      DATA IMATHT(98)   /'ARRD'/
      DATA IMATHT(99)   /'AD  '/
      DATA IMATHT(100)  /'ARRL'/
      DATA IMATHT(101)  /'AL  '/
      DATA IMATHT(102)  /'ARRR'/
      DATA IMATHT(103)  /'AR  '/
CCCCC NOVEMBER 1996.  FOLLOWING LINE CAUSES COMPILE ERROR ON LINUX
CCCCC G77 COMPILER. 
CLINX DATA IMATHT(104)  /'\   '/
      DATA IMATHT(105)  /'BASL'/
      DATA IMATHT(106)  /'BACK'/
      DATA IMATHT(107)  /'BS  '/
      DATA IMATHT(108)  /'_   '/
      DATA IMATHT(109)  /'UNDE'/
      DATA IMATHT(110)  /'CUBE'/
      DATA IMATHT(111)  /'PYRA'/
C  AUGUST 1992.  ADD REVT, RT (FOR REVERSE TRIANGLE, TO AGREE WITH
C  DOCUMENTATION), AND ARRO, ARRH, VECT FOR THE ARROW COMMAND
      DATA IMATHT(112)  /'REVT'/
      DATA IMATHT(113)  /'RT  '/
      DATA IMATHT(114)  /'ARRO'/
      DATA IMATHT(115)  /'ARRH'/
      DATA IMATHT(116)  /'VECT'/
      DATA IMATHT(117)  /'DEGR'/
C
C---------------------------------------------------------------------
C
C               ******************************************************
C               **  DEFINE THE NUMBER OF CHARACTERS FOR OPERATIONS  **
C               ******************************************************
C
      DATA IOPERN( 1)   /4/
      DATA IOPERN( 2)   /4/
      DATA IOPERN( 3)   /4/
      DATA IOPERN( 4)   /4/
      DATA IOPERN( 5)   /4/
      DATA IOPERN( 6)   /4/
      DATA IOPERN( 7)   /4/
      DATA IOPERN( 8)   /4/
C
      DATA IOPERN( 9)   /2/
      DATA IOPERN(10)   /3/
      DATA IOPERN(11)   /4/
      DATA IOPERN(12)   /2/
C
      DATA IOPERN(13)   /4/
      DATA IOPERN(14)   /4/
      DATA IOPERN(15)   /4/
C
      DATA IOPERN(16)   /3/
      DATA IOPERN(17)   /4/
C
      DATA IOPERN(18)   /3/
      DATA IOPERN(19)   /4/
      DATA IOPERN(20)   /3/
      DATA IOPERN(21)   /4/
C
      DATA IOPERN(22)   /4/
      DATA IOPERN(23)   /4/
      DATA IOPERN(24)   /4/
      DATA IOPERN(25)   /4/
      DATA IOPERN(26)   /4/
      DATA IOPERN(27)   /4/
C
      DATA IOPERN(28)   /4/
      DATA IOPERN(29)   /4/
      DATA IOPERN(30)   /4/
      DATA IOPERN(31)   /4/
C
      DATA IOPERN(32)   /4/
      DATA IOPERN(33)   /4/
      DATA IOPERN(34)   /2/
      DATA IOPERN(35)   /4/
      DATA IOPERN(36)   /3/
      DATA IOPERN(37)   /4/
C
C               ************************************************************
C               **  DEFINE THE NUMBER OF CHARACTERS FOR GREEK CHARACTERS  **
C               ************************************************************
C
      DATA IGREEN( 1)   /4/
      DATA IGREEN( 2)   /4/
      DATA IGREEN( 3)   /4/
      DATA IGREEN( 4)   /4/
      DATA IGREEN( 5)   /4/
      DATA IGREEN( 6)   /4/
      DATA IGREEN( 7)   /3/
      DATA IGREEN( 8)   /4/
      DATA IGREEN( 9)   /4/
      DATA IGREEN(10)   /4/
      DATA IGREEN(11)   /4/
      DATA IGREEN(12)   /2/
      DATA IGREEN(13)   /2/
      DATA IGREEN(14)   /2/
      DATA IGREEN(15)   /4/
      DATA IGREEN(16)   /2/
      DATA IGREEN(17)   /3/
      DATA IGREEN(18)   /4/
      DATA IGREEN(19)   /3/
      DATA IGREEN(20)   /4/
      DATA IGREEN(21)   /3/
      DATA IGREEN(22)   /3/
      DATA IGREEN(23)   /3/
      DATA IGREEN(24)   /4/
C
C               ********************************************************
C               **  DEFINE THE NUMBER OF CHARACTERS FOR MATH SYMBOLS  **
C               ********************************************************
C
      DATA IMATHN( 1)   /4/
      DATA IMATHN( 2)   /4/
      DATA IMATHN( 3)   /2/
      DATA IMATHN( 4)   /4/
      DATA IMATHN( 5)   /4/
      DATA IMATHN( 6)   /4/
      DATA IMATHN( 7)   /4/
      DATA IMATHN( 8)   /4/
      DATA IMATHN( 9)   /4/
      DATA IMATHN(10)   /4/
      DATA IMATHN(11)   /4/
      DATA IMATHN(12)   /2/
      DATA IMATHN(13)   /2/
      DATA IMATHN(14)   /4/
      DATA IMATHN(15)   /4/
      DATA IMATHN(16)   /4/
      DATA IMATHN(17)   /4/
      DATA IMATHN(18)   /4/
      DATA IMATHN(19)   /2/
      DATA IMATHN(20)   /2/
      DATA IMATHN(21)   /4/
      DATA IMATHN(22)   /4/
      DATA IMATHN(23)   /4/
      DATA IMATHN(24)   /4/
      DATA IMATHN(25)   /4/
      DATA IMATHN(26)   /4/
      DATA IMATHN(27)   /4/
      DATA IMATHN(28)   /4/
      DATA IMATHN(29)   /4/
      DATA IMATHN(30)   /4/
      DATA IMATHN(31)   /4/
      DATA IMATHN(32)   /4/
      DATA IMATHN(33)   /4/
      DATA IMATHN(34)   /4/
      DATA IMATHN(35)   /4/
      DATA IMATHN(36)   /4/
      DATA IMATHN(37)   /4/
      DATA IMATHN(38)   /4/
      DATA IMATHN(39)   /4/
      DATA IMATHN(40)   /4/
      DATA IMATHN(41)   /4/
      DATA IMATHN(42)   /4/
      DATA IMATHN(43)   /4/
      DATA IMATHN(44)   /4/
      DATA IMATHN(45)   /4/
      DATA IMATHN(46)   /4/
      DATA IMATHN(47)   /4/
      DATA IMATHN(48)   /4/
      DATA IMATHN(49)   /4/
      DATA IMATHN(50)   /4/
      DATA IMATHN(51)   /4/
      DATA IMATHN(52)   /4/
      DATA IMATHN(53)   /4/
      DATA IMATHN(54)   /4/
      DATA IMATHN(55)   /4/
      DATA IMATHN(56)   /4/
      DATA IMATHN(57)   /4/
      DATA IMATHN(58)   /4/
      DATA IMATHN(59)   /4/
      DATA IMATHN(60)   /4/
      DATA IMATHN(61)   /4/
      DATA IMATHN(62)   /4/
      DATA IMATHN(63)   /4/
      DATA IMATHN(64)   /3/
      DATA IMATHN(65)   /3/
C
      DATA IMATHN(66)   /4/
      DATA IMATHN(67)   /4/
      DATA IMATHN(68)   /4/
      DATA IMATHN(69)   /4/
      DATA IMATHN(70)   /4/
C
      DATA IMATHN(71)   /1/
      DATA IMATHN(72)   /4/
      DATA IMATHN(73)   /2/
      DATA IMATHN(74)   /2/
      DATA IMATHN(75)   /4/
      DATA IMATHN(76)   /2/
      DATA IMATHN(77)   /4/
      DATA IMATHN(78)   /2/
      DATA IMATHN(79)   /4/
      DATA IMATHN(80)   /2/
      DATA IMATHN(81)   /4/
      DATA IMATHN(82)   /2/
      DATA IMATHN(83)   /4/
      DATA IMATHN(84)   /2/
      DATA IMATHN(85)   /1/
      DATA IMATHN(86)   /4/
      DATA IMATHN(87)   /2/
      DATA IMATHN(88)   /4/
      DATA IMATHN(89)   /4/
      DATA IMATHN(90)   /4/
      DATA IMATHN(91)   /2/
      DATA IMATHN(92)   /4/
      DATA IMATHN(93)   /2/
      DATA IMATHN(94)   /4/
      DATA IMATHN(95)   /2/
      DATA IMATHN(96)   /4/
      DATA IMATHN(97)   /2/
      DATA IMATHN(98)   /4/
      DATA IMATHN(99)   /2/
      DATA IMATHN(100)   /4/
      DATA IMATHN(101)   /2/
      DATA IMATHN(102)   /4/
      DATA IMATHN(103)   /2/
      DATA IMATHN(104)   /1/
      DATA IMATHN(105)   /4/
      DATA IMATHN(106)   /4/
      DATA IMATHN(107)   /2/
      DATA IMATHN(108)   /1/
      DATA IMATHN(109)   /4/
      DATA IMATHN(110)   /4/
      DATA IMATHN(111)   /4/
C
C  AUGUST 1992.  ADDED FOLLOWING LINES FOR REVERSE TRIANGLE SYNONYMS
C  AND FOR ARROW.
C
      DATA IMATHN(112)   /4/
      DATA IMATHN(113)   /2/
      DATA IMATHN(114)   /4/
      DATA IMATHN(115)   /4/
      DATA IMATHN(116)   /4/
      DATA IMATHN(117)   /4/
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPSC'
      ISUBN2='AN  '
C
      IFOUNO='NO'
      IFOUNC='NO'
      IERROR='NO'
C
CLINX NOVEMBER 1996.  FOLLOWING TO ACCOMODATE LINUX G77 COMPILER.
      CALL DPCONA(92,IMATHT(104))
      J2=0
      NUMC=0
C
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'SCAN')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPSCAN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ISTART,ISTRIN(ISTART),NUMCHS,ILOCR2
   52 FORMAT('ISTART,ISTRIN(ISTART),NUMCHS,ILOCR2 = ',I8,2X,A4,2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)(ISTRIN(I),I=1,NUMCHS)
   53 FORMAT('(ISTRIN(I),I=1,NUMCHS) = ',100A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGG4,ISUBG4
   59 FORMAT('IBUGG4,ISUBG4 = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               *********************************************
C               **  STEP 2--                               **
C               **  PACK THE PRESENT CHARACTER             **
C               **  AND THE NEXT 3 CHARACTERS INTO         **
C               **  THE SINGLE COMPUTER WORD IWORD1.       **
C               **  IF A LEFT PARENTHESIS IS ENCOUNTERED,  **
C               **  STOP THE PACK                          **
C               **  (AND EXCLUDE THE LEFT PARENTHESIS      **
C               **  FROM THE PACK).                        **
C               *********************************************
C
      ISTEPN='2'
      IF(IBUGD2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IWORD1=' '
C
      ISTAR1=0
      ILEN1=NUMBPC
      ILEN2=NUMBPC
C
      DO1100K=1,4
      L=ISTART+K-1
      IF(L.GT.NUMCHS)GOTO1190
      IF(ISTRIN(L).EQ.'(')GOTO1190
      ISTAR2=NUMBPC*(K-1)
      CALL DPCHEX(ISTAR1,ILEN1,ISTRIN(L),ISTAR2,ILEN2,IWORD1)
 1100 CONTINUE
 1190 CONTINUE
CCCCC CONVERT IWORD1 TO UPPER CASE.             FEBRUARY 1995.
      DO1191I=1,4
        CALL DPCOAN(IWORD1(I:I),IVALT)
        IF(IVALT.GE.97.AND.IVALT.LE.122)IVALT=IVALT-32
        CALL DPCONA(IVALT,IWORD1(I:I))
 1191 CONTINUE
C
C               *************************************************************
C               **  STEP 1--CHECK TO SEE                                   **
C               **          IF BEYOND THE RIGHTMOST RIGHT PARENTHESIS      **
C               **          (WHICH IMPLIES THAT ALL SUBSEQUENT CHARACTERS  **
C               **          ARE ONLY 1 CHARACTER LONG).                    **
C               *************************************************************
C
      IF(ISTART.GT.ILOCR2)GOTO6000
C
C               ***************************
C               **  STEP 3.1--           **
C               **  CHECK FOR FONT TYPE  **
C               ***************************
C
      ISTEPN='3.1'
      IF(IBUGD2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      JMIN=1
      JMAX=8
      DO2110J=JMIN,JMAX
      J2=J
      IF(IWORD1.EQ.IOPERT(J))GOTO2150
 2110 CONTINUE
      GOTO2190
 2150 CONTINUE
      NUMC=IOPERN(J2)
      ILOCLP=ISTART+NUMC
      ILOCRP=ISTART+NUMC+1
      CALL DPCHLR(ISTRIN,NUMCHS,ILOCLP,ILOCRP,IFOULR,IBUGD2,IERROR)
      IF(IFOULR.EQ.'YES')GOTO2160
      GOTO2190
 2160 CONTINUE
      IFONT=IWORD1
      IEND=ILOCRP
      IOP=IFONT
      IFOUNO='YES'
      GOTO9000
 2190 CONTINUE
C
C               **********************************
C               **  STEP 3.2--                  **
C               **  CHECK FOR UPPER/LOWER CASE  **
C               **********************************
C
      ISTEPN='3.2'
      IF(IBUGD2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C
      JMIN=9
      JMAX=12
      DO2210J=JMIN,JMAX
      J2=J
      IF(IWORD1.EQ.IOPERT(J))GOTO2250
 2210 CONTINUE
      GOTO2290
 2250 CONTINUE
      NUMC=IOPERN(J2)
      ILOCLP=ISTART+NUMC
      ILOCRP=ISTART+NUMC+1
      CALL DPCHLR(ISTRIN,NUMCHS,ILOCLP,ILOCRP,IFOULR,IBUGD2,IERROR)
      IF(IFOULR.EQ.'YES')GOTO2260
      GOTO2290
 2260 CONTINUE
      ICASE=IWORD1
      IF(ICASE.EQ.'LC')ICASE='LOWE'
      IF(ICASE.EQ.'LCAS')ICASE='LOWE'
      IF(ICASE.EQ.'UC')ICASE='UPPE'
      IF(ICASE.EQ.'UCAS')ICASE='UPPE'
      IF(ICASE.EQ.'CAPS')ICASE='UPPE'
      IF(ICASE.EQ.'CAP')ICASE='UPPE'
      IEND=ILOCRP
      IOP=ICASE
      IFOUNO='YES'
      GOTO9000
 2290 CONTINUE
C
C               *************************************************
C               **  STEP 3.3--                                 **
C               **  CHECK FOR LEFT/CENTER/RIGHT JUSTIFICATION  **
C               *************************************************
C
      ISTEPN='3.3'
      IF(IBUGD2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      JMIN=13
      JMAX=15
      DO2310J=JMIN,JMAX
      J2=J
      IF(IWORD1.EQ.IOPERT(J))GOTO2350
 2310 CONTINUE
      GOTO2390
 2350 CONTINUE
      NUMC=IOPERN(J2)
      ILOCLP=ISTART+NUMC
      ILOCRP=ISTART+NUMC+1
      CALL DPCHLR(ISTRIN,NUMCHS,ILOCLP,ILOCRP,IFOULR,IBUGD2,IERROR)
      IF(IFOULR.EQ.'YES')GOTO2360
      GOTO2390
 2360 CONTINUE
      IJUST=IWORD1
      IEND=ILOCRP
      IOP=IJUST
      IFOUNO='YES'
      GOTO9000
 2390 CONTINUE
C
C               ******************************************
C               **  STEP 3.4--                          **
C               **  CHECK FOR SEQUENCE/UNSEQUENCE CASE  **
C               ******************************************
C
      ISTEPN='3.4'
      IF(IBUGD2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      JMIN=16
      JMAX=17
      DO2410J=JMIN,JMAX
      J2=J
      IF(IWORD1.EQ.IOPERT(J))GOTO2450
 2410 CONTINUE
      GOTO2490
 2450 CONTINUE
      NUMC=IOPERN(J2)
      ILOCLP=ISTART+NUMC
      ILOCRP=ISTART+NUMC+1
      CALL DPCHLR(ISTRIN,NUMCHS,ILOCLP,ILOCRP,IFOULR,IBUGD2,IERROR)
      IF(IFOULR.EQ.'YES')GOTO2460
      GOTO2490
 2460 CONTINUE
      ISEQUE=IWORD1
      IEND=ILOCRP
      IOP=ISEQUE
      IFOUNO='YES'
      GOTO9000
 2490 CONTINUE
C
C               ********************************************
C               **  STEP 3.5--                            **
C               **  CHECK FOR SUBSCRIPT/SUPERSCRIPT CASE  **
C               ********************************************
C
      ISTEPN='3.5'
      IF(IBUGD2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      JMIN=18
      JMAX=21
      DO2510J=JMIN,JMAX
      J2=J
      IF(IWORD1.EQ.IOPERT(J))GOTO2550
 2510 CONTINUE
      GOTO2590
 2550 CONTINUE
      NUMC=IOPERN(J2)
      ILOCLP=ISTART+NUMC
      ILOCRP=ISTART+NUMC+1
      CALL DPCHLR(ISTRIN,NUMCHS,ILOCLP,ILOCRP,IFOULR,IBUGD2,IERROR)
      IF(IFOULR.EQ.'YES')GOTO2560
      GOTO2590
 2560 CONTINUE
      ISUBSU=IWORD1
      IEND=ILOCRP
      IOP=ISUBSU
      IFOUNO='YES'
      GOTO9000
 2590 CONTINUE
C
C               ****************************************
C               **  STEP 3.6--                        **
C               **  CHECK FOR SCREEN MAX, ANGLE MAX,  **
C               **  HEIGHT, WIDTH, AND ANGLE.         **
C               ****************************************
C
      ISTEPN='3.6'
      IF(IBUGD2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      JMIN=22
      JMAX=27
      DO2610J=JMIN,JMAX
      J2=J
      IF(IWORD1.EQ.IOPERT(J))GOTO2650
 2610 CONTINUE
      GOTO2690
 2650 CONTINUE
      NUMC=IOPERN(J2)
      ILOCLP=ISTART+NUMC
      ILOCRP=ISTART+NUMC+1
      CALL DPCHLR(ISTRIN,NUMCHS,ILOCLP,ILOCRP,IFOULR,IBUGD2,IERROR)
      IF(IFOULR.EQ.'YES')GOTO2660
      GOTO2690
 2660 CONTINUE
      IXXXXX=IWORD1
      IEND=ILOCRP
      IOP=IXXXXX
      IFOUNO='YES'
      GOTO9000
 2690 CONTINUE
C
C               *********************************************
C               **  STEP 3.7--                             **
C               **  CHECK FOR MOVE, DRAW, ETC. OPERATIONS  **
C               *********************************************
C
      ISTEPN='3.7'
      IF(IBUGD2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      JMIN=28
      JMAX=37
      DO2710J=JMIN,JMAX
      J2=J
      IF(IWORD1.EQ.IOPERT(J))GOTO2750
 2710 CONTINUE
      GOTO2790
 2750 CONTINUE
      NUMC=IOPERN(J2)
      ILOCLP=ISTART+NUMC
      ILOCRP=ISTART+NUMC+1
      CALL DPCHLR(ISTRIN,NUMCHS,ILOCLP,ILOCRP,IFOULR,IBUGD2,IERROR)
      IF(IFOULR.EQ.'YES')GOTO2760
      GOTO2790
 2760 CONTINUE
      IXXXXX=IWORD1
      IEND=ILOCRP
      IOP=IXXXXX
      IFOUNO='YES'
      GOTO9000
 2790 CONTINUE
C
C               **********************************
C               **  STEP 3.8--                  **
C               **  CHECK FOR GREEK CHARACTERS  **
C               **********************************
C
      ISTEPN='3.8'
      IF(IBUGD2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      JMIN=1
      JMAX=24
      DO3110J=JMIN,JMAX
      J2=J
      IF(IWORD1.EQ.IGREET(J))GOTO3150
 3110 CONTINUE
      GOTO3190
 3150 CONTINUE
      NUMC=IGREEN(J2)
      ILOCLP=ISTART+NUMC
      ILOCRP=ISTART+NUMC+1
      CALL DPCHLR(ISTRIN,NUMCHS,ILOCLP,ILOCRP,IFOULR,IBUGD2,IERROR)
      IF(IFOULR.EQ.'YES')GOTO3160
      GOTO3190
 3160 CONTINUE
      ICHAR2=IWORD1
      IEND=ILOCRP
      IFOUNC='YES'
      GOTO9000
 3190 CONTINUE
C
C               ******************************
C               **  STEP 3.9--              **
C               **  CHECK FOR MATH SYMBOLS  **
C               ******************************
C
      ISTEPN='3.9'
      IF(IBUGD2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      JMIN=1
CCCCC JMAX=109
CCCCC JMAX=111
      JMAX=117
      DO4110J=JMIN,JMAX
      J2=J
      IF(IWORD1.EQ.IMATHT(J))GOTO4150
 4110 CONTINUE
      GOTO4190
 4150 CONTINUE
      NUMC=IMATHN(J2)
      ILOCLP=ISTART+NUMC
      ILOCRP=ISTART+NUMC+1
      CALL DPCHLR(ISTRIN,NUMCHS,ILOCLP,ILOCRP,IFOULR,IBUGD2,IERROR)
      IF(IFOULR.EQ.'YES')GOTO4160
      GOTO4190
 4160 CONTINUE
      ICHAR2=IWORD1
      IEND=ILOCRP
      IFOUNC='YES'
      GOTO9000
 4190 CONTINUE
C
C               *************************************************
C               **  STEP 4--                                   **
C               **  NO MATCH FOUND FOR ANY OF THE ABOVE;       **
C               **  THEREFORE OUTPUT ONLY THE LEAD CHARACTER.  **
C               *************************************************
C
C
 6000 CONTINUE
      ISTEPN='4'
      IF(IBUGD2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMCHS.LE.1)GOTO6005
      ILOCLP=ISTART
      ILOCRP=ISTART+1
      IF(ISTRIN(ILOCLP).EQ.'('.AND.ISTRIN(ILOCRP).EQ.')')GOTO6006
 6005 CONTINUE
      ICHAR2=ISTRIN(ISTART)
      IEND=ISTART
      IFOUNC='YES'
      GOTO9000
 6006 CONTINUE
      IEND=ILOCRP
      IFOUNO='YES'
      GOTO9000
C
C     PRE-1986--THE FOLLOWING COMMENTED-OUT CODE WAS FOR PUTTING OUT
C     UP TO 4 CHARACTERS AS A PLOT CHARACTER
C     AND THEREFORE COMMENTED OUT.
C
CCCCC DO6010I=1,4
CCCCC I2=I
CCCCC ICHAR3='    '
CCCCC ICHAR3(1:1)=IWORD1(I:I)
CCCCC IF(ICHAR3.EQ.'(')GOTO6020
CCCCC IF(ICHAR3.EQ.' ')GOTO6020
C6010 CONTINUE
CCCCC NUMC=I2
CCCCC GOTO6080
C6020 CONTINUE
CCCCC NUMC=I2-1
CCCCC GOTO6080
C6080 CONTINUE
CCCCC IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'SCAN')GOTO6089
CCCCC WRITE(ICOUT,6081)
C6081 FORMAT('***** FROM THE MIDDLE OF DPSCAN--')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,6082)IWORD1,ICHAR3
C6082 FORMAT('IWORD1,ICHAR3 = ',A4,2X,A4)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,6083)I2,NUMC
C6083 FORMAT('I2,NUMC = ',2I8)
CCCCC CALL DPWRST('XXX','BUG ')
C6089 CONTINUE
C6090 CONTINUE
CCCCC ILOCLP=ISTART+NUMC
CCCCC ILOCRP=ISTART+NUMC+1
CCCCC CALL DPCHLR(ISTRIN,NUMCHS,ILOCLP,ILOCRP,IFOULR,IBUGD2,IERROR)
CCCCC IF(IFOULR.EQ.'YES')GOTO6095
CCCCC GOTO6097
C6095 CONTINUE
CCCCC ICHAR2=IWORD1
CCCCC IEND=ILOCRP
CCCCC IFOUNC='YES'
CCCCC GOTO9000
C6097 CONTINUE
CCCCC ICHAR2=ISTRIN(ISTART)
CCCCC IEND=ISTART
CCCCC IFOUNC='YES'
CCCCC GOTO9000
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'SCAN')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPSCAN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IFOUNC,IFOUNO,IBUGD2,IERROR
 9012 FORMAT('IFOUNC,IFOUNO,IBUGD2,IERROR = ',
     1A4,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ICHAR2,IOP,ISTART,IEND
 9013 FORMAT('ICHAR2,IOP,ISTART,IEND = ',A4,2X,A4,I8,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)IFONT,ICASE,IJUST,ISEQUE,ISUBSU
 9014 FORMAT('IFONT,ICASE,IJUST,ISEQUE,ISUBSU = ',
     1A4,2X,A4,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)HMAX,VMAX,AMAX
 9015 FORMAT('HMAX,VMAX,AMAX = ',3E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9016)X0,Y0,ANGLE
 9016 FORMAT('X0,Y0,ANGLE = ',3E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9017)WIDTH,HEIGHT
 9017 FORMAT('WIDTH,HEIGHT = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9018)ISTAR2,IWORD1,NUMC,J2
 9018 FORMAT('ISTAR2,IWORD1,NUMC,J2 = ',I8,2X,A4,I8,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9019)IBUGG4,ISUBG4
 9019 FORMAT('IBUGG4,ISUBG4 = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPSCEB(NPTS,NLAB,
     1                  W,N,
     1                  AMEAN,ASD,S2BMPS,
     1                  XSE,XSES2,IDFH,SIGMAH,
     1                  SESUK1,SESUK2,
     1                  DLOWSE,DHIGSE,
     1                  IWRITE,
     1                  ICAPSW,ICAPTY,NUMDIG,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--IMPLEMENT SCHILLER-EBERHARDT APPROACH TO CONSENSUS MEANS
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
C     ORIGINAL VERSION--MARCH     2006. EXTRACTED FROM DPMAN2 ROUTINE
C     UPDATED         --OCTOBER   2006. CALL LIST TO TPPF
C     UPDATED         --FEBRUARY  2010. USE DPDTA1 TO PRINT
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES--------------
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
      REAL APPF
      REAL XSE
      REAL XSES2
      REAL S2BMPS
      REAL SIGMAH
      REAL SESUK1
      REAL SESUK2
C
C----------------------------------------------------------------
C
      REAL AMEAN(*)
      REAL ASD(*)
C
      INTEGER N(*)
C
      DOUBLE PRECISION W(*)
C
      INCLUDE 'DPCOST.INC'
C
      PARAMETER (MAXROW=20)
      CHARACTER*60 ITITLE
      CHARACTER*60 ITITLZ
      CHARACTER*60 ITITL9
      CHARACTER*60 ITEXT(MAXROW)
      REAL         AVALUE(MAXROW)
      INTEGER      NCTEXT(MAXROW)
      INTEGER      IDIGIT(MAXROW)
      INTEGER      NTOT(MAXROW)
      LOGICAL IFRST
      LOGICAL ILAST
C
C-----COMMON-----------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT------------------------------------------------
C
      IERROR='NO'
      ISUBN1='DPVR'
      ISUBN2='ML  '
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SCEB')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPSCEB--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IWRITE,NPTS,NLAB
   52   FORMAT('IWRITE,NPTS,NLAB = ',A4,2X,2I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,NPTS
          WRITE(ICOUT,56)I,AMEAN(I),ASD(I),N(I)
   56     FORMAT('I,AMEAN(I),ASD(I),N(I) = ',I8,2G15.7,I8)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
      DSUM1=0.0D0
      DO810I=1,NLAB
        DVAR=DBLE(ASD(I))**2
        W(I)=1.0D0/(DVAR+DBLE(S2BMPS))
        DSUM1=DSUM1 + W(I)
  810 CONTINUE
      DWTSUM=DSUM1
      DSUM1=0.0D0
      DSUM2=0.0D0
      DSUM3=0.0D0
      DO815I=1,NLAB
        NITEMP=ABS(N(I))
        IF(NITEMP.EQ.0)THEN
          IERROR='YES'
          GOTO9000
        ENDIF
        DVAR=DBLE(ASD(I))**2
        W(I)=W(I)/DWTSUM
        XI=DBLE(AMEAN(I))
        DSUM1=DSUM1 + W(I)*XI
        DSUM2=DSUM2 + W(I)*DVAR
        DSUM3=DSUM3 + (W(I)*DVAR)**2/DBLE(NITEMP-1.0D0)
  815 CONTINUE
      XSE=REAL(DSUM1)
      ADFH=REAL(IDFH)
      DTERM1=(DSUM2 + SIGMAH**2)**2
      DTERM2=(DSUM3 + SIGMAH**4/ADFH)
      ADF=REAL(DTERM1/DTERM2)
      IDF=INT(ADF+0.5)
C
      DSUM1=0.0D0
      DO820I=1,NLAB
        DVAR=DBLE(ASD(I))**2
        W(I)=1.0D0/DVAR
        DSUM1=DSUM1 + W(I)
  820 CONTINUE
      DWTSUM=DSUM1
      DSUM1=0.0D0
      DO825I=1,NLAB
        DTERM1=(W(I)/DWTSUM)**2
        DSUM1=DSUM1 + DTERM1*DBLE(ASD(I)**2)
  825 CONTINUE
      XSES2=REAL(DSUM1)
C
      DBIAS=0.0D0
      DO830I=1,NLAB
        XI=DBLE(AMEAN(I))
        DTERM1=DABS(XI-DBLE(XSE))
        IF(DTERM1.GT.DBIAS)DBIAS=DTERM1
  830 CONTINUE
C
      CALL TPPF(0.975,REAL(IDF),APPF)
      DSESU1=SQRT(DBLE(XSES2) + DBLE(SIGMAH)**2) + DBIAS
      DSESU2=2.0D0*SQRT(DBLE(XSES2) + DBLE(SIGMAH)**2) + DBIAS
      DSEU=DBLE(APPF)*SQRT(DBLE(XSES2) + DBLE(SIGMAH)**2) + DBIAS
      DLOWSE=DBLE(XSE) - DSEU
      DHIGSE=DBLE(XSE) + DSEU
      ABIAS=REAL(DBIAS)
      ISEDF=IDF
      SESUK1=REAL(DSESU1)
      SESUK2=REAL(DSESU2)
C
      ITITLE=' '
      NCTITL=0
      ITITLZ=' '
      NCTITZ=0
C
      ICNT=1
      ITEXT(ICNT)='12. Method:Schiller-Eberhardt'
      NCTEXT(ICNT)=29
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='    Estimate of Consensus Mean:'
      NCTEXT(ICNT)=31
      AVALUE(ICNT)=XSE
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='    Estimate of Variance of Mean:'
      NCTEXT(ICNT)=33
      AVALUE(ICNT)=XSES2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='    Bias Allowance:'
      NCTEXT(ICNT)=19
      AVALUE(ICNT)=ABIAS
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='    Sigmah (heterogeneity):'
      NCTEXT(ICNT)=27
      AVALUE(ICNT)=SIGMAH
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='    Degrees of Freedom for Sigmah:'
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=IDFH
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='    Standard Uncertainty (k = 1):'
      NCTEXT(ICNT)=33
      AVALUE(ICNT)=DSESU1
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='    Expanded Uncertainty (k = 2):'
      NCTEXT(ICNT)=33
      AVALUE(ICNT)=DSESU2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='    Expanded Uncertainty (k =           ):'
      WRITE(ITEXT(ICNT)(31:40),'(F10.7)')APPF
      NCTEXT(ICNT)=42
      AVALUE(ICNT)=DSEU
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='    Degrees of Freedom:'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=IDF
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='    t Percent Point Value (alpha = 0.05):'
      NCTEXT(ICNT)=41
      AVALUE(ICNT)=APPF
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='    Lower 95% Confidence Limit:'
      NCTEXT(ICNT)=31
      AVALUE(ICNT)=DLOWSE
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='    Upper 95% Confidence Limit:'
      NCTEXT(ICNT)=31
      AVALUE(ICNT)=DHIGSE
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='    Note: Schiller-Eberhardt Best Usage:'
      NCTEXT(ICNT)=40
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='          5 or Fewer Labs:'
      NCTEXT(ICNT)=26
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
      ITITLE=' '
      NCTITL=0
      ITITLZ=' '
      NCTITZ=0
      ITITL9=' '
      NCTIT9=0
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SCEB')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSCEB--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPTS,NLAB
 9013   FORMAT('NPTS,NLAB = ',2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)XSE,XSES2,DSEU
 9014   FORMAT('XSE,XSES2,DSEU = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)DLOWSE,DHIGSE
 9015   FORMAT('DLOWSE,DHIGSE = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPSCI2(X1,Y1,X2,Y2,PX,PY,
     1                  IFIG,ILINPA,ILINCO,ILINC2,MAXLN,PLINTH,
     1                  AREGBA,IREBLI,IREBCO,IREBC2,MAXRG,PREBTH,
     1                  IREFSW,IREFCO,IREFC2,
     1                  IREPTY,IREPLI,IREPCO,IREPC2,PREPTH,PREPSP,
     1                  PTEXHE,PTEXWI,PTEXVG,PTEXHG)
C
C     PURPOSE--DRAW A SEMI-CIRCLE
C              WITH ONE END OF THE DIAGONAL AT (X1,Y1)
C              AND THE OTHER END AT (X2,Y2).
C     NOTE--THE SEMI-CIRCLE WILL BE DRAWN CLOCKWISE.
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
C     UPDATED         --JANUARY   1989. MODIFY CALLS TO DPDRPL (ALAN)
C     UPDATED         --JANUARY   1989. MODIFY CALL  TO DPFIRE (ALAN)
C     UPDATED         --JULY      2019. CREATE SCRATCH STORAGE IN DPSCIR
C                                       RATHER THAN DPSCI2
C     UPDATED         --OCTOBER   2020. SUPPORT FOR RGB COLOR
C
C-----NON-COMMON VARIABLES-------------------------------------
C
      DIMENSION PX(*)
      DIMENSION PY(*)
C
      CHARACTER*4 IFIG
      CHARACTER*4 IPATT2
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
      DIMENSION ILINC2(MAXLN,3)
      DIMENSION PLINTH(*)
C
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
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'SCI2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPSCI2--')
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
C               **  FOR THE SEMI-CIRCLE        **
C               *********************************
C
      DELX=X2-X1
      DELY=Y2-Y1
      ALEN=0.0
      TERM=(X2-X1)**2+(Y2-Y1)**2
      IF(TERM.GT.0.0)ALEN=SQRT(TERM)
      RADIUS=ALEN/2.0
      IF(ABS(DELX).GE.0.00001)THETA=ATAN(DELY/DELX)
      IF(ABS(DELX).LT.0.00001.AND.DELY.GE.0.0)THETA=3.1415926/2.0
      IF(ABS(DELX).LT.0.00001.AND.DELY.LT.0.0)THETA=-3.1415926/2.0
C
      XCENT=(X1+X2)/2.0
      YCENT=(Y1+Y2)/2.0
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
      DO3010I=1,181,5
      IREV=181-I+1
      PHI2=IREV-1
      PHI2=PHI2*(2.0*3.1415926)/360.0
      X=RADIUS*COS(PHI2)+RADIUS
      Y=RADIUS*SIN(PHI2)
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
      ICOLFR=IREFC2(1,1)
      ICOLFG=IREFC2(1,2)
      ICOLFB=IREFC2(1,3)
      ICOLP=IREPCO(1)
      ICOLPR=IREPC2(1,1)
      ICOLPG=IREPC2(1,2)
      ICOLPB=IREPC2(1,3)
      ICOLBR=IREBC2(1,1)
      ICOLBG=IREBC2(1,2)
      ICOLBB=IREBC2(1,3)
      CALL DPFIRE(PX,PY,NP,
     1            IFIG,IPATT,PTHICK,PXGAP,PYGAP,
     1            ICOLF,ICOLFR,ICOLFG,ICOLFB,
     1            ICOLP,ICOLPR,ICOLPG,ICOLPB,
     1            IPATT2)
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
      ICOLR=ILINC2(1,1)
      ICOLG=ILINC2(1,2)
      ICOLB=ILINC2(1,3)
      IFLAG='ON'
      CALL DPDRPL(PX,PY,NP,
     1            IFIG,IPATT,PTHICK,
     1            ICOL,ICOLR,ICOLG,ICOLB,
     1            JPATT,JTHICK,PTHIC2,JCOL,IFLAG)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'SCI2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSCI2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NP,IERRG4
 9014   FORMAT('NP,IERRG4 = ',A4,2X,I8)
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
      SUBROUTINE DPSCIR(IHARG,IARGT,ARG,NUMARG,
     1                  PXSTAR,PYSTAR,PXEND,PYEND,
     1                  ILINPA,ILINCO,ILINC2,MAXLNZ,PLINTH,
     1                  AREGBA,IREBLI,IREBCO,IREBC2,MAXRGZ,PREBTH,
     1                  IREFSW,IREFCO,IREFC2,
     1                  IREPTY,IREPLI,IREPCO,IREPC2,PREPTH,PREPSP,
     1                  PTEXHE,PTEXWI,PTEXVG,PTEXHG,
     1                  IGRASW,IDIASW,
     1                  PGRAXF,PGRAYF,PDIAXC,PDIAYC,PDIAX2,PDIAY2,
     1                  PDIAHE,PDIAWI,PDIAVG,PDIAHG,
     1                  NUMDEV,IDMANU,IDMODE,IDMOD2,IDMOD3,
     1                  IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,
     1                  IDNVOF,IDNHOF,IDFONT,UNITSW,PDSCAL,
     1                  IBACCO,IBACC2,
     1                  IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DRAW ONE OR MORE SEMI-CIRCLES (DEPENDING ON HOW MANY
C              NUMBERS ARE PROVIDED).  THE COORDINATES ARE IN
C              STANDARDIZED UNITS OF 0 TO 100.
C     NOTE--THE SEMI-CIRCLE WILL BE DRAWN CLOCKWISE.
C     NOTE--THE INPUT COORDINATES DEFINE THE ENDS OF THE DIAMETER
C           OF THE SEMI-CIRCLE.
C     NOTE-THE USUAL INPUT NUMBER OF COORDINATES IS 2
C          AND THEREFORE THE USUAL INPUT NUMBER OF NUMBERS IS 2*2 = 4.
C     NOTE--IF 2 NUMBERS ARE PROVIDED, THEN THE DRAWN SEMI-CIRCLE WILL
C           GO FROM THE LAST CURSOR POSITION TO THE (X,Y) POINT (EITHER
C           ABSOLUTE OR RELATIVE) AS DEFINED BY THE 2 NUMBERS.
C     NOTE--IF 4 NUMBERS ARE PROVIDED, THEN THE DRAWN SEMI-CIRCLE WILL
C           GO FROM THE ABSOLUTE (X,Y) POSITION AS DEFINED BY THE FIRST
C           2 NUMBERS TO THE (X,Y) POINT (EITHER ABSOLUTE OR RELATIVE)
C           AS DEFINED BY THE THIRD AND FOURTH NUMBERS.
C     NOTE--IF 6 NUMBERS ARE PROVIDED, THEN THE DRAWN SEMI-CIRCLE WILL
C           GO FROM THE (X,Y) POSITION AS RESULTING FROM THE THIRD AND
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
C     UPDATED         --DECEMBER  2018. CHECK FOR NULL OR NONE DEVICE
C     UPDATED         --DECEMBER  2018. SUPPORT FOR "DEVICE ... SCALE"
C                                       COMMAND
C     UPDATED         --JULY      2019. MOVE CREATION OF SCRATCH
C                                       STORAGE FROM DPSCI2 TO DPSCIR
C     UPDATED         --OCTOBER   2020. SUPPORT FOR RGB COLOR
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
      DIMENSION ILINC2(MAXLNZ,3)
      DIMENSION PLINTH(*)
C
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
      DIMENSION IBACC2(*)
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
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'SCIR')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPSCIR--')
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
      IFIG='SCIR'
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
C
        CALL DPOPPL(IGRASW,IBELSW,NUMRIN,IERASW,IBACCO,IBACC2)
C
C               *****************************************
C               **  STEP 2--                           **
C               **  SEARCH FOR COMMAND SPECIFICATIONS  **
C               *****************************************
C
        IF(NUMARG.GE.3.AND.
     1     IARGT(2).EQ.'NUMB'.AND.IARGT(3).EQ.'NUMB')THEN
          ITYPEO='ABSO'
          ILOCFN=2
        ELSEIF(NUMARG.GE.4.AND.IHARG(2).EQ.'ABSO'.AND.
     1         IARGT(3).EQ.'NUMB'.AND.IARGT(4).EQ.'NUMB')THEN
          ITYPEO='ABSO'
          ILOCFN=3
        ELSEIF(NUMARG.GE.4.AND.IHARG(2).EQ.'RELA'.AND.
     1         IARGT(3).EQ.'NUMB'.AND.IARGT(4).EQ.'NUMB')THEN
          ITYPEO='RELA'
          ILOCFN=3
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
        CALL DPSCI2(X1,Y1,X2,Y2,PX,PY,
     1              IFIG,ILINPA,ILINCO,ILINC2,MAXLNZ,PLINTH,
     1              AREGBA,IREBLI,IREBCO,IREBC2,MAXRGZ,PREBTH,
     1              IREFSW,IREFCO,IREFC2,
     1              IREPTY,IREPLI,IREPCO,IREPC2,PREPTH,PREPSP,
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
 1131 FORMAT('***** ERROR IN SEMI-CIRCLE (DPSCIR)--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1132)
 1132 FORMAT('      ILLEGAL FORM FOR SEMI-CIRCLE COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1134)
 1134 FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE PROPER FORM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1135)
 1135 FORMAT('      SUPPOSE IT IS DESIRED TO DRAW A SEMI-CIRCLE ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1136)
 1136 FORMAT('      WITH ONE END OF A DIAGONAL AT 20 20 ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1137)
 1137 FORMAT('      AND THE OTHER END OF THE DIAGONAL AT 40 60,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1141)
 1141 FORMAT('      THEN ALLOWABLE FORMS ARE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1142)
 1142 FORMAT('      SEMI-CIRCLE 20 20 40 60')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1143)
 1143 FORMAT('      SEMI-CIRCLE ABSOLUTE 20 20 40 60')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1145)
 1145 FORMAT('      SEMI-CIRCLE RELATIVE 20 20 40 60')
      CALL DPWRST('XXX','BUG ')
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'SCIR')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSCIR--')
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
      SUBROUTINE DPSCR7(ISTRIN,NUMCHA,X0,Y0,
     1                  IFONT,ICASE,IJUST,ANGLE,HMAX,VMAX,AMAX,
     1                  WIDTH,HEIGHT,
     1                  PHEIGH,PWIDTH,PVEGAP,PHOGAP,
     1                  PHEIG2,PWIDT2,PVEGA2,PHOGA2,
     1                  ANUMHP,ANUMVP,
     1                  IPATT,PTHICK,ICOL,ICOLR,ICOLG,ICOLB,IRGBFL,
     1                  JPATT,JTHICK,PTHIC2,JCOL,
     1                  ISYMBL,ISPAC,IFILL,
     1                  IMPSW2,AMPSCH,AMPSCW,
     1                  XEND,YEND,IFOUND,IBUGD2,IERROR)
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
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--JANUARY   1981.
C     UPDATED         --OCTOBER   1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --OCTOBER   1993. HANDLE LOWER CASE CHARACTERS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ISTRIN
C
      CHARACTER*4 IPATT
      CHARACTER*4 IFONT
      CHARACTER*4 ICASE
      CHARACTER*4 IJUST
      CHARACTER*4 ICOL
C
      CHARACTER*24 ISYMBL
      CHARACTER*4 ISPAC
      CHARACTER*4 IFILL
C
      CHARACTER*4 IFOUND
      CHARACTER*4 IBUGD2
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISEQUE
      CHARACTER*4 ISUBSU
      CHARACTER*4 IDRAW
      CHARACTER*4 IFOUNO
      CHARACTER*4 IFONSV
      CHARACTER*4 ICASSV
      CHARACTER*4 ICHAR2
      CHARACTER*4 IOP
      CHARACTER*4 IFOUNC
CCCCC OCTOBER 1993.  ADD FOLLOWING LINE
      CHARACTER*4 ICASE2
C
C---------------------------------------------------------------------
C
      DIMENSION ISTRIN(*)
C
C-----COMMON----------------------------------------------------------
C
C
      CHARACTER*4 IMPSW2
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
CCCCC OCTOBER 1993.  ADD FOLLOWING LINE
      ICASE2='UPPE'
      ISEQUE='ON'
      ISUBSU='OFF'
C
C
      X02=50.0
      Y02=50.0
C
      IF(IBUGG4.EQ.'ON' .OR. ISUBG4.EQ.'SCR7')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPSCR7--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)X0,Y0,ANGLE,IFONT,ICASE,IJUST
   52   FORMAT('X0,Y0,ANGLE,IFONT,ICASE,IJUST = ',3G15.7,3(2X,A4))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)HMAX,VMAX,AMAX,WIDTH,HEIGHT
   53   FORMAT('HMAX,VMAX,AMAX,WIDTH,HEIGHT = ',5G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)ANUMHP,ANUMVP
   54   FORMAT('ANUMHP,ANUMVP = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)NUMCHA,XEND,YEND,IBUGD2,ISUBRO,ISUBG4
   55   FORMAT('NUMCHA,XEND,YEND,IBUGD2,ISUBG4 = ',I6,2G15.7,2(2X,A4))
        CALL DPWRST('XXX','BUG ')
        DO57I=1,NUMCHA
          WRITE(ICOUT,58)I,ISTRIN(I)
   58     FORMAT('I,ISTRIN(I) = ',I8,2X,A4)
          CALL DPWRST('XXX','BUG ')
   57   CONTINUE
        WRITE(ICOUT,60)ICOL,JCOL,PTHICK,JTHICK,PTHIC2
   60   FORMAT('ICOL,JCOL,PTHICK,JTHICK,PTHIC2 = ',A4,I8,G15.7,I8,G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,61)IFILL,IPATT,JPATT
   61   FORMAT('IFILL,IPATT,JPATT = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,62)PHEIGH,PWIDTH,PVEGAP,PHOGAP
   62   FORMAT('PHEIGH,PWIDTH,PVEGAP,PHOGAP = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,63)PHEIG2,PWIDT2,PVEGA2,PHOGA2
   63   FORMAT('PHEIG2,PWIDT2,PVEGA2,PHOGA2 = ',4E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,65)ISYMBL,ISPAC
   65   FORMAT('ISYMBL,ISPAC = ',A24,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,68)IFOUND,IERROR,IERRG4
   68   FORMAT('IFOUND,IERROR,IERRG4 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *************************
C               **  STEP XX--          **
C               **  SAVE INPUT VALUES  **
C               *************************
C
      IFONSV=IFONT
      ICASSV=ICASE
      HEIGSV=HEIGHT
      WIDTSV=WIDTH
C
      PHEISV=PHEIGH
      PWIDSV=PWIDTH
      PVEGSV=PVEGAP
      PHOGSV=PHOGAP
C
      PHE2SV=PHEIG2
      PWI2SV=PWIDT2
      PVG2SV=PVEGA2
      PHG2SV=PHOGA2
C
      IF(IMPSW2.EQ.'ON')THEN
        PHEIGH=PHEIGH*AMPSCH
        PVEGAP=PVEGAP*AMPSCH
        PWIDTH=PWIDTH*AMPSCW
        PHOGAP=PHOGAP*AMPSCW
        PHEIG2=PHEIG2*AMPSCH
        PVEGA2=PVEGA2*AMPSCH
        PWIDT2=PWIDT2*AMPSCW
        PHOGA2=PHOGA2*AMPSCW
        HEIGHT=HEIGHT*AMPSCH
        WIDTH=WIDTH*AMPSCW
      ENDIF
C
C               *********************************************
C               **  STEP XX--                              **
C               **  DETERMINE THE LOCATION                 **
C               **  OF THE RIGHT-MOST NON-BLANK CHARACTER  **
C               *********************************************
C
      DO300I=1,NUMCHA
      IREV=NUMCHA-I+1
      IF(ISTRIN(IREV).NE.' ')GOTO305
  300 CONTINUE
      NUMCHS=0
      GOTO309
  305 CONTINUE
      NUMCHS=IREV
  309 CONTINUE
C
C               *************************************
C               **  STEP XX--                      **
C               **  DETERMINE THE LOCATION         **
C               **  OF THE RIGHT-MOST PARENTHESIS  **
C               *************************************
C
      ILOCR2=0
      DO600I=1,NUMCHS
      IREV=NUMCHS-I+1
      IF(ISTRIN(IREV).EQ.')')GOTO610
  600 CONTINUE
      GOTO690
  610 CONTINUE
      ILOCR2=IREV
      GOTO690
  690 CONTINUE
C
C               ***********************************************
C               **  STEP XX--                                **
C               **  PROCEED SEQUENTIALLY THROUGH THE STRING  **
C               ***********************************************
C
      IF(IJUST.EQ.'LEFT')GOTO1100
      IF(IJUST.EQ.'LEBO')GOTO1100
      IF(IJUST.EQ.'LECE')GOTO1100
      IF(IJUST.EQ.'LETO')GOTO1100
C
      IF(IJUST.EQ.'CENT')GOTO1200
      IF(IJUST.EQ.'CEBO')GOTO1200
      IF(IJUST.EQ.'CECE')GOTO1200
      IF(IJUST.EQ.'CETO')GOTO1200
C
      IF(IJUST.EQ.'RIGH')GOTO1200
      IF(IJUST.EQ.'RIBO')GOTO1200
      IF(IJUST.EQ.'RICE')GOTO1200
      IF(IJUST.EQ.'RITO')GOTO1200
C
      GOTO1100
C
C               *****************************************
C               **  STEP 11--                          **
C               **  TREAT THE LEFT-JUSTIFICATION CASE  **
C               *****************************************
C
 1100 CONTINUE
C
      IEND=0
C
      XEND=X0
      YEND=Y0
      IF(IJUST.EQ.'LECE')YEND=Y0-PHEIGH/2.0
      IF(IJUST.EQ.'LETO')YEND=Y0-PHEIGH
C
 1110 CONTINUE
      ISTART=IEND+1
      IF(ISTART.GT.NUMCHS)GOTO1190
C
C               ************************************
C               **  STEP 12--                     **
C               **  DECODE THE NEXT CHARACTER     **
C               **  (OR THE NEXT FEW CHARACTERS)  **
C               ************************************
C
      CALL DPSCAN(ISTART,ISTRIN,NUMCHS,ILOCR2,
     1            ICHAR2,IOP,IFONT,ICASE,IJUST,ISEQUE,ISUBSU,
     1            HMAX,VMAX,AMAX,X0,Y0,ANGLE,WIDTH,HEIGHT,
     1            IEND,IFOUNC,IFOUNO,IBUGD2,IERROR)
C
      IF(IBUGG4.EQ.'ON' .OR. ISUBG4.EQ.'SCR7')THEN
        WRITE(ICOUT,1112)ICHAR2,IOP,ISTART,IEND,IFOUNC,IFOUNO
 1112   FORMAT('ICHAR2,IOP,ISTART,IEND,IFOUNC,IFOUNO = ',
     1         A4,2X,A4,I8,I8,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ******************************
C               **  STEP 13--               **
C               **  DRAW OUT THE CHARACTER  **
C               ******************************
C
      CALL DPSBSP(IFOUNO,IOP,XEND,YEND,HEIGHT,WIDTH,
     1            PHEIGH,PWIDTH,PVEGAP,PHOGAP,
     1            PHEIG2,PWIDT2,PVEGA2,PHOGA2,
     1            ANGLE,AMAX,
     1            IBUGD2,IERROR)
      IF(IFOUNO.EQ.'YES')GOTO1180
C
      XSTART=XEND
      YSTART=YEND
C
      IDRAW='ON'
CCCCC OCTOBER 1993.  HANDLE CASE IF ICHAR2 IS LOWER CASE.
      ICASE2=ICASE
      CALL DPCOAN(ICHAR2(1:1),IVAL)
      IF(IVAL.GE.97.AND.IVAL.LE.122)THEN
        IVAL=IVAL-32
        CALL DPCONA(IVAL,ICHAR2(1:1))
        IF(ICASE.EQ.'LOWE'.OR.ICASE.EQ.'ASIS')ICASE2='LOWE'
      ELSE
        IF(ICASE.EQ.'ASIS')ICASE2='UPPE'
      END IF
CCCCC END CHANGE
C
      CALL DPSCR8(ICHAR2,XSTART,YSTART,IDRAW,
     1            IFONT,ICASE2,ANGLE,HMAX,VMAX,AMAX,WIDTH,HEIGHT,
     1            PHEIGH,PWIDTH,PVEGAP,PHOGAP,
     1            PHEIG2,PWIDT2,PVEGA2,PHOGA2,
     1            ANUMHP,ANUMVP,
     1            IPATT,PTHICK,ICOL,ICOLR,ICOLG,ICOLB,IRGBFL,
     1            JPATT,JTHICK,PTHIC2,JCOL,
     1            XEND,YEND,
     1            ISPAC,IFILL,
     1            IFOUND,IBUGD2,IERROR)
C
 1180 CONTINUE
      GOTO1110
C
 1190 CONTINUE
      IF(IJUST.EQ.'LECE')YEND=YEND+PHEIGH/2.0
      IF(IJUST.EQ.'LETO')YEND=YEND+PHEIGH
      GOTO8000
C
C               *****************************************
C               **  STEP 21--                          **
C               **  TREAT THE CENTER-JUSTIFICATION     **
C               **  AND THE RIGHT-JUSTIFICATION CASES  **
C               *****************************************
C
 1200 CONTINUE
C
      XLEN=0.0
      YLEN=0.0
C
      IEND=0
C
      IDRAW='OFF'
C
      XEND99=X0
      YEND99=Y0
C
 1210 CONTINUE
      ISTART=IEND+1
      IF(ISTART.GT.NUMCHS)GOTO1250
C
C               ************************************
C               **  STEP 22--                     **
C               **  DECODE THE NEXT CHARACTER     **
C               **  (OR THE NEXT FEW CHARACTERS)  **
C               ************************************
C
      CALL DPSCAN(ISTART,ISTRIN,NUMCHS,ILOCR2,
     1            ICHAR2,IOP,IFONT,ICASE,IJUST,ISEQUE,ISUBSU,
     1            HMAX,VMAX,AMAX,X0,Y0,ANGLE,WIDTH,HEIGHT,
     1            IEND,IFOUNC,IFOUNO,IBUGD2,IERROR)
C
      IF(IBUGG4.EQ.'ON' .OR. ISUBG4.EQ.'SCR7')THEN
        WRITE(ICOUT,1212)ICHAR2,IOP,ISTART,IEND,IFOUNC,IFOUNO
 1212   FORMAT('ICHAR2,IOP,ISTART,IEND,IFOUNC,IFOUNO = ',
     1         A4,2X,A4,I8,I8,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *********************************************
C               **  STEP 23--                              **
C               **  DETERMINE THE LENGTH OF THE CHARACTER  **
C               *********************************************
C
      CALL DPSBSP(IFOUNO,IOP,XEND99,YEND99,HEIGHT,WIDTH,
     1            PHEIGH,PWIDTH,PVEGAP,PHOGAP,
     1            PHEIG2,PWIDT2,PVEGA2,PHOGA2,
     1            ANGLE,AMAX,
     1            IBUGD2,IERROR)
      IF(IFOUNO.EQ.'YES')GOTO1240
C
      XSTA99=XEND99
      YSTA99=YEND99
CCCCC OCTOBER 1993.  HANDLE CASE IF ICHAR2 IS LOWER CASE.
      ICASE2=ICASE
      CALL DPCOAN(ICHAR2(1:1),IVAL)
      IF(IVAL.GE.97.AND.IVAL.LE.122)THEN
        IVAL=IVAL-32
        CALL DPCONA(IVAL,ICHAR2(1:1))
        IF(ICASE.EQ.'LOWE'.OR.ICASE.EQ.'ASIS')ICASE2='LOWE'
      ELSE
        IF(ICASE.EQ.'ASIS')ICASE2='UPPE'
      END IF
CCCCC END CHANGE
C
      CALL DPSCR8(ICHAR2,XSTA99,YSTA99,IDRAW,
     1             IFONT,ICASE2,ANGLE,HMAX,VMAX,AMAX,WIDTH,HEIGHT,
     1             PHEIGH,PWIDTH,PVEGAP,PHOGAP,
     1             PHEIG2,PWIDT2,PVEGA2,PHOGA2,
     1             ANUMHP,ANUMVP,
     1             IPATT,PTHICK,ICOL,ICOLR,ICOLG,ICOLB,IRGBFL,
     1             JPATT,JTHICK,PTHIC2,JCOL,
     1             XEND99,YEND99,
     1             ISPAC,IFILL,
     1             IFOUND,IBUGD2,IERROR)
C
 1240 CONTINUE
      GOTO1210
C
 1250 CONTINUE
      XLEN=XEND99-X0
      YLEN=YEND99-Y0
C
C               ***************************************
C               **  STEP 24--                        **
C               **  RESTORE VALUES TO THOSE AT TIME  **
C               **  OF INPUT TO THIS SUBROUTINE      **
C               ***************************************
C
      IFONT=IFONSV
      ICASE=ICASSV
      HEIGHT=HEIGSV
      WIDTH=WIDTSV
C
      PHEIGH=PHEISV
      PWIDTH=PWIDSV
      PVEGAP=PVEGSV
      PHOGAP=PHOGSV
C
      PHEIG2=PHE2SV
      PWIDT2=PWI2SV
      PVEGA2=PVG2SV
      PHOGA2=PHG2SV
C
      IF(IMPSW2.EQ.'ON')THEN
        PHEIGH=PHEIGH*AMPSCH
        PVEGAP=PVEGAP*AMPSCH
        PWIDTH=PWIDTH*AMPSCW
        PHOGAP=PHOGAP*AMPSCW
        PHEIG2=PHEIG2*AMPSCH
        PVEGA2=PVEGA2*AMPSCH
        PWIDT2=PWIDT2*AMPSCW
        PHOGA2=PHOGA2*AMPSCW
        HEIGHT=HEIGHT*AMPSCH
        WIDTH=WIDTH*AMPSCW
      ENDIF
C               ************************************************
C               **  STEP 25--                                 **
C               **  COMPUTE STARTING POINT                    **
C               **  FOR THE CENTER- OR RIGHT-JUSTIFIED STRING **
C               ************************************************
C
CCCCC IF(IJUST.EQ.'CENT')X02=X0-(XLEN/2.0)
      IF(IJUST.EQ.'CENT')X02=X0-(XLEN/2.0)+(PHOGAP/2.0)
      IF(IJUST.EQ.'CENT')Y02=Y0-(YLEN/2.0)
C
CCCCC IF(IJUST.EQ.'CEBO')X02=X0-(XLEN/2.0)
      IF(IJUST.EQ.'CEBO')X02=X0-(XLEN/2.0)+(PHOGAP/2.0)
      IF(IJUST.EQ.'CEBO')Y02=Y0-(YLEN/2.0)
C
CCCCC IF(IJUST.EQ.'CECE')X02=X0-(XLEN/2.0)
      IF(IJUST.EQ.'CECE')X02=X0-(XLEN/2.0)+(PHOGAP/2.0)
      IF(IJUST.EQ.'CECE')Y02=Y0-(YLEN/2.0)-PHEIGH/2.0
C
CCCCC IF(IJUST.EQ.'CETO')X02=X0-(XLEN/2.0)
      IF(IJUST.EQ.'CETO')X02=X0-(XLEN/2.0)+(PHOGAP/2.0)
      IF(IJUST.EQ.'CETO')Y02=Y0-(YLEN/2.0)-PHEIGH
C
      IF(IJUST.EQ.'RIGH')X02=X0-XLEN
      IF(IJUST.EQ.'RIGH')Y02=Y0-YLEN
C
      IF(IJUST.EQ.'RIBO')X02=X0-XLEN
      IF(IJUST.EQ.'RIBO')Y02=Y0-YLEN
C
      IF(IJUST.EQ.'RICE')X02=X0-XLEN
      IF(IJUST.EQ.'RICE')Y02=Y0-YLEN-PHEIGH/2.0
C
      IF(IJUST.EQ.'RITO')X02=X0-XLEN
      IF(IJUST.EQ.'RITO')Y02=Y0-YLEN-PHEIGH
C
      IF(IBUGG4.EQ.'ON' .OR. ISUBG4.EQ.'SCR7')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1251)
 1251   FORMAT('***** FROM THE MIDDLE    OF DPSCR7--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1252)IJUST,XLEN,YLEN,PWIDT2,PHEIG2
 1252   FORMAT('IJUST,XLEN,YLEN,PWIDT2,PHEIG2 = ',A4,4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1253)X0,Y0,X02,Y02
 1253   FORMAT('X0,Y0,X02,Y02 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IEND=0
C
      XEND=X02
      YEND=Y02
C
      IDRAW='ON'
C
 1260 CONTINUE
      ISTART=IEND+1
      IF(ISTART.GT.NUMCHS)GOTO1290
C
C               ************************************
C               **  STEP 26--                     **
C               **  DECODE THE NEXT CHARACTER     **
C               **  (OR THE NEXT FEW CHARACTERS)  **
C               ************************************
C
      CALL DPSCAN(ISTART,ISTRIN,NUMCHS,ILOCR2,
     1            ICHAR2,IOP,IFONT,ICASE,IJUST,ISEQUE,ISUBSU,
     1            HMAX,VMAX,AMAX,X0,Y0,ANGLE,WIDTH,HEIGHT,
     1            IEND,IFOUNC,IFOUNO,IBUGD2,IERROR)
C
      IF(IBUGG4.EQ.'ON' .OR. ISUBG4.EQ.'SCR7')THEN
        WRITE(ICOUT,1262)ICHAR2,IOP,ISTART,IEND,IFOUNC,IFOUNO
 1262   FORMAT('ICHAR2,IOP,ISTART,IEND,IFOUNC,IFOUNO = ',
     1         2(A4,2X),2I8,2(2X,A4))
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ******************************
C               **  STEP 27--               **
C               **  DRAW OUT THE CHARACTER  **
C               ******************************
C
      CALL DPSBSP(IFOUNO,IOP,XEND,YEND,HEIGHT,WIDTH,
     1            PHEIGH,PWIDTH,PVEGAP,PHOGAP,
     1            PHEIG2,PWIDT2,PVEGA2,PHOGA2,
     1            ANGLE,AMAX,
     1            IBUGD2,IERROR)
      IF(IFOUNO.EQ.'YES')GOTO1280
C
      XSTART=XEND
      YSTART=YEND
CCCCC OCTOBER 1993.  HANDLE CASE IF ICHAR2 IS LOWER CASE.
      ICASE2=ICASE
      CALL DPCOAN(ICHAR2(1:1),IVAL)
      IF(IVAL.GE.97.AND.IVAL.LE.122)THEN
        IVAL=IVAL-32
        CALL DPCONA(IVAL,ICHAR2(1:1))
        IF(ICASE.EQ.'LOWE'.OR.ICASE.EQ.'ASIS')ICASE2='LOWE'
      ELSE
        IF(ICASE.EQ.'ASIS')ICASE2='UPPE'
      END IF
CCCCC END CHANGE
C
      CALL DPSCR8(ICHAR2,XSTART,YSTART,IDRAW,
     1            IFONT,ICASE2,ANGLE,HMAX,VMAX,AMAX,WIDTH,HEIGHT,
     1            PHEIGH,PWIDTH,PVEGAP,PHOGAP,
     1            PHEIG2,PWIDT2,PVEGA2,PHOGA2,
     1            ANUMHP,ANUMVP,
     1            IPATT,PTHICK,ICOL,ICOLR,ICOLG,ICOLB,IRGBFL,
     1            JPATT,JTHICK,PTHIC2,JCOL,
     1            XEND,YEND,
     1            ISPAC,IFILL,
     1            IFOUND,IBUGD2,IERROR)
C
 1280 CONTINUE
      GOTO1260
C
 1290 CONTINUE
      IF(IJUST.EQ.'CECE')YEND=YEND+PHEIGH/2.0
      IF(IJUST.EQ.'CETO')YEND=YEND+PHEIGH
      IF(IJUST.EQ.'RICE')YEND=YEND+PHEIGH/2.0
      IF(IJUST.EQ.'RITO')YEND=YEND+PHEIGH
      GOTO8000
C
C               ***************************************
C               **  STEP 28--                        **
C               **  RESTORE VALUES TO THOSE AT TIME  **
C               **  OF INPUT TO THIS SUBROUTINE      **
C               ***************************************
C
 8000 CONTINUE
      IFONT=IFONSV
      ICASE=ICASSV
      WIDTH=WIDTSV
      HEIGHT=HEIGSV
C
      PHEIGH=PHEISV
      PWIDTH=PWIDSV
      PVEGAP=PVEGSV
      PHOGAP=PHOGSV
C
      PHEIG2=PHE2SV
      PWIDT2=PWI2SV
      PVEGA2=PVG2SV
      PHOGA2=PHG2SV
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG4.EQ.'ON' .OR. ISUBG4.EQ.'SCR7')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSCR7--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)XEND,YEND
 9012   FORMAT('XEND,YEND = ',2E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9028)IFOUND,IERROR,IERRG4
 9028   FORMAT('IFOUND,IERROR,IERRG4 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPSCR8(ICHAR2,XSTART,YSTART,IDRAW,
     1                  IFONT,ICASE,ANGLE,HMAX,VMAX,AMAX,WIDTH,HEIGHT,
     1                  PHEIGH,PWIDTH,PVEGAP,PHOGAP,
     1                  PHEIG2,PWIDT2,PVEGA2,PHOGA2,
     1                  ANUMHP,ANUMVP,
     1                  IPATT,PTHICK,ICOL,ICOLR,ICOLG,ICOLB,IRGBFL,
     1                  JPATT,JTHICK,PTHIC2,JCOL,
     1                  XEND,YEND,
     1                  ISPAC,
     1                  IFILL,
     1                  IFOUND,IBUG,IERROR)
C
C     PURPOSE--SCRIBE OUT THE SINGLE CHARACTER
C              IN THE HOLLERITH VARIABLE ICHAR2.
C     NOTE--ICHAR2 SHOULD CONTAIN A SINGLE CHARACTER OR SHOULD CONTAIN AN
C           ABBREVIATED STRING (4 CHARACTERS AT MOST) INDICATING A DESIRED
C           MATH OPERATION, GREEK LETTER, ETC.  THE ABBREVIATED STRING HAS
C           HAD () REMOVED.  THE PRE-CHECKING AND FORMATION OF A VALID
C           ICHAR2 WAS DONE IN DPSCAN.
C     INPUT  ARGUMENTS--ICHAR2  = THE HOLLERITH VARIABLE
C                                CONTAINING THE CHARACTER OF INTEREST.
C                       XSTART = THE STARTING HORIZONTAL COORDINATE;
C                                THE HORIZONTAL COORDINATE OF THE
C                                BOTTOM LEFT POINT OF THE FIRST CHARACTER.
C                                XSTART MAY BE IN ANY UNITS, BUT IS USUALLY
C                                GIVEN IN % UNITS, INCHES, CENTIMETERS, OR
C                                TEKTRONIX PICTURE POINTS.
C                       YSTART = THE STARTING VERTICAL COORDINATE;
C                                THE VERTICAL COORDINATE OF THE
C                                BOTTOM LEFT POINT OF THE FIRST CHARACTER.
C                                YSTART MAY BE IN ANY UNITS, BUT IS USUALLY
C                                GIVEN IN % UNITS, INCHES, CENTIMETERS, OR
C                                TEKTRONIX PICTURE POINTS.
C                       HEIGHT = THE HEIGHT OF THE CHARACTERS (INCLUDING GAP);
C                                THE HEIGHT OF A CHARACTER
C                                MAY BE IN ANY UNITS, BUT IS USUALLY
C                                GIVEN IN % UNITS, INCHES, CENTIMETERS, OR
C                                TEKTRONIX PICTURE POINTS.
C
C                       WIDTH  = THE WIDTH OF THE CHARACTERS (INCLUDING GAP);
C                                THE WIDTH OF A CHARACTER
C                                MAY BE IN ANY UNITS, BUT IS USUALLY
C                                GIVEN IN % UNITS, INCHES, CENTIMETERS, OR
C                                TEKTRONIX PICTURE POINTS.
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
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--
C     UPDATED         --SEPTEMBER 1981.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --OCTOBER   1991. ADDED SOME ABBREVIATIONS FOR CHARACTER
C                                       FILL.  ALAN
C     UPDATED         --AUGUST    1992. ADD SOME CHAR FILL (ALAN)
C     UPDATED         --OCTOBER   2020. SUPPORT RGB COLOR
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICHAR2
      CHARACTER*4 IDRAW
      CHARACTER*4 IPATT
      CHARACTER*4 IFONT
      CHARACTER*4 ICASE
      CHARACTER*4 ICOL
      CHARACTER*4 ISPAC
      CHARACTER*4 IFILL
C
      CHARACTER*4 IFOUND
      CHARACTER*4 IBUG
      CHARACTER*4 IERROR
C
      CHARACTER*4 IOP
      CHARACTER*4 IFIG
      CHARACTER*4 IMATH
      CHARACTER*4 ICHAR3
      CHARACTER*4 IHORPA
      CHARACTER*4 IVERPA
      CHARACTER*4 IDUPPA
      CHARACTER*4 IDDOPA
C
      CHARACTER*4 ICOLF
      CHARACTER*4 ICOLP
      CHARACTER*4 IFLAG
C
      CHARACTER*4 IPATT2
C
C---------------------------------------------------------------------
C
      DIMENSION IOP(100)
      DIMENSION X(100)
      DIMENSION Y(100)
C
      DIMENSION PX(100)
      DIMENSION PY(100)
C
CCCCC DIMENSION PX3(100)
CCCCC DIMENSION PY3(100)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IPATT2='SOLI'
      IMATH='NO'
C
      XFACHP=1.0
      YFACHP=1.0
      XMINC=0.0
      XMAXC=0.0
      XMINC2=0.0
      XMAXC2=0.0
      YMINC2=0.0
      YMAXC2=0.0
      X2=0.0
      X3=0.0
      X4=0.0
      XEND2=(-999.0)
      YEND2=(-999.0)
      I2=(-999)
      PPENTH=(-999.0)
      NLOOP=(-999)
C
      IF(IBUGG4.EQ.'ON' .OR. ISUBG4.EQ.'SCR8')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('****** AT THE BEGINNING OF DPSCR8--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICHAR2,XSTART,YSTART,IDRAW,WIDTH,HEIGHT
   52   FORMAT('ICHAR2,XSTART,YSTART,IDRAW,WIDTH,HEIGHT = ',
     1         A4,2G15.7,2X,A4,2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IFONT,ICASE,ISPAC,IFILL,ANGLE
   53   FORMAT('IFONT,ICASE,ISPAC,IFILL,ANGLE = ',4(A4,2X),G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)HMAX,VMAX,AMAX
   54   FORMAT('HMAX,VMAX,AMAX = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)ANUMHP,ANUMVP
   55   FORMAT('ANUMHP,ANUMVP = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,58)XEND,YEND
   58   FORMAT('XEND,YEND = ',2E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,59)IPATT,JPATT
   59   FORMAT('IPATT,JPATT = ',A4,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,60)ICOL,JCOL,PTHICK,JTHICK,PTHIC2
   60   FORMAT('ICOL,JCOL,PTHICK,JTHICK,PTHIC2 = ',
     1         A4,I8,G15.7,I8,G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,62)PHEIGH,PWIDTH,PVEGAP,PHOGAP
   62   FORMAT('PHEIGH,PWIDTH,PVEGAP,PHOGAP = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,63)PHEIG2,PWIDT2,PVEGA2,PHOGA2
   63   FORMAT('PHEIG2,PWIDT2,PVEGA2,PHOGA2 = ',4E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,69)IFOUND,IBUGG4,ISUBG4,IERROR
   69   FORMAT('IFOUND,IBUGG4,ISUBG4,IERROR = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
CCCCC           ******************************************************
CCCCC           **  STEP 3.0--                                      **
CCCCC           **  COPY OVER VALUES FOR THE USUAL CASE (= 1 PASS)  **
CCCCC           ******************************************************
CCCCC
CCCCC ISTART = LOCATION (1 TO 4) OF FIRST CHARACTER IN ICHAR2
CCCCC   (IF FIRST CHARACTER IS BLANK, THEN ISTART STILL = 1)
CCCCC  ISTOP = LOCATION (1 TO 4) OF LAST NON-BLANK CHARACTER IN ICHAR2
CCCCC   OR OF LAST CHARACTER BEFORE (
CCCCC  (UNLESS ( IS IN LOCATION 1)
CCCCC  IPOINT = LOCATION (1 TO 4) OF CURRENTLOCATION OF ITNTEREST.
CCCCC ICHAR3 EITHER HAS ELEMENTS IPOINT TO ISTOP OF ICHAR2
CCCCC OR (IF NO MATCH WAS FOUND),
CCCCC ELEMENTS IPOINT OT IPOINT OF ICHAR2.
CCCCC ISTART AND ISTOP DO NOT CHANGE.
CCCCC IPOINT MAY CHANGE (INCREASE) IF NO MATCH
CCCCC
CCCCC ISTART=1
CCCCC ISTOP=4
CCCCC ICTEMP=ICHAR2(4:4)
CCCCC IF(ICTEMP.EQ.' ')ISTOP=3
CCCCC IF(ICTEMP.EQ.'(')ISTOP=3
CCCCC ICTEMP=ICHAR2(3:3)
CCCCC IF(ICTEMP.EQ.' ')ISTOP=2
CCCCC IF(ICTEMP.EQ.'(')ISTOP=2
CCCCC ICTEMP=ICHAR2(2:2)
CCCCC IF(ICTEMP.EQ.' ')ISTOP=1
CCCCC IF(ICTEMP.EQ.'(')ISTOP=1
CCCCC
CCCCC IPOINT=ISTART
C
      ICHAR3=ICHAR2
      XSTAR2=XSTART
      YSTAR2=YSTART
C
C               **********************************************
C               **  STEP 3.1--                              **
C               **  TREAT THE ROMAN ALPHABET, NUMERIC, AND  **
C               **  STANDARD SYMBOLS CASE                   **
C               **********************************************
C
      IF(IBUGG4.EQ.'ON' .OR. ISUBG4.EQ.'SCR8')THEN
        WRITE(ICOUT,1201)
 1201   FORMAT('***** FROM NEAR BEGINNING OF DPSCR8--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1202)ICHAR2,ICHAR3
 1202   FORMAT('ICHAR2,ICHAR3 = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IFOUND='NO'
      IF(IFONT.EQ.'SIMP')THEN
        IF(ICASE.EQ.'UPPE')THEN
          CALL DPRSU(ICHAR3,IOP,X,Y,NUMCO,IXMINC,IXMAXC,IXDELC,
     1               IBUG,IFOUND,IERROR)
        ELSEIF(ICASE.EQ.'LOWE')THEN
          CALL DPRSL(ICHAR3,IOP,X,Y,NUMCO,IXMINC,IXMAXC,IXDELC,
     1               IBUG,IFOUND,IERROR)
        ENDIF
        IF(IFOUND.EQ.'NO')THEN
          CALL DPRSN(ICHAR3,IOP,X,Y,NUMCO,IXMINC,IXMAXC,IXDELC,
     1               IBUG,IFOUND,IERROR)
        ENDIF
        IF(IFOUND.EQ.'NO')THEN
          CALL DPRSS(ICHAR3,IOP,X,Y,NUMCO,IXMINC,IXMAXC,IXDELC,
     1               IBUG,IFOUND,IERROR)
        ENDIF
      ELSEIF(IFONT.EQ.'DUPL')THEN
        IF(ICASE.EQ.'UPPE')THEN
          CALL DPRDU(ICHAR3,IOP,X,Y,NUMCO,IXMINC,IXMAXC,IXDELC,
     1               IBUG,IFOUND,IERROR)
        ELSEIF(ICASE.EQ.'LOWE')THEN
          CALL DPRDL(ICHAR3,IOP,X,Y,NUMCO,IXMINC,IXMAXC,IXDELC,
     1               IBUG,IFOUND,IERROR)
        ENDIF
        IF(IFOUND.EQ.'NO')THEN
          CALL DPRDN(ICHAR3,IOP,X,Y,NUMCO,IXMINC,IXMAXC,IXDELC,
     1               IBUG,IFOUND,IERROR)
        ENDIF
        IF(IFOUND.EQ.'NO')THEN
          CALL DPRDS(ICHAR3,IOP,X,Y,NUMCO,IXMINC,IXMAXC,IXDELC,
     1               IBUG,IFOUND,IERROR)
        ENDIF
      ELSEIF(IFONT.EQ.'TRIP')THEN
        IF(ICASE.EQ.'UPPE')THEN
          CALL DPRTU(ICHAR3,IOP,X,Y,NUMCO,IXMINC,IXMAXC,IXDELC,
     1               IBUG,IFOUND,IERROR)
        ELSEIF(ICASE.EQ.'LOWE')THEN
          CALL DPRTL(ICHAR3,IOP,X,Y,NUMCO,IXMINC,IXMAXC,IXDELC,
     1               IBUG,IFOUND,IERROR)
        ENDIF
        IF(IFOUND.EQ.'NO')THEN
          CALL DPRTN(ICHAR3,IOP,X,Y,NUMCO,IXMINC,IXMAXC,IXDELC,
     1               IBUG,IFOUND,IERROR)
        ENDIF
        IF(IFOUND.EQ.'NO')THEN
          CALL DPRTS(ICHAR3,IOP,X,Y,NUMCO,IXMINC,IXMAXC,IXDELC,
     1               IBUG,IFOUND,IERROR)
        ENDIF
      ELSEIF(IFONT.EQ.'TRII')THEN
        IF(ICASE.EQ.'UPPE')THEN
          CALL DPRTIU(ICHAR3,IOP,X,Y,NUMCO,IXMINC,IXMAXC,IXDELC,
     1                IBUG,IFOUND,IERROR)
        ELSEIF(ICASE.EQ.'LOWE')THEN
          CALL DPRTIL(ICHAR3,IOP,X,Y,NUMCO,IXMINC,IXMAXC,IXDELC,
     1                IBUG,IFOUND,IERROR)
        ENDIF
        IF(IFOUND.EQ.'NO')THEN
          CALL DPRTIN(ICHAR3,IOP,X,Y,NUMCO,IXMINC,IXMAXC,IXDELC,
     1                IBUG,IFOUND,IERROR)
        ENDIF
        IF(IFOUND.EQ.'NO')THEN
           CALL DPRTS(ICHAR3,IOP,X,Y,NUMCO,IXMINC,IXMAXC,IXDELC,
     1                IBUG,IFOUND,IERROR)
        ENDIF
      ELSEIF(IFONT.EQ.'COMI')THEN
        IF(ICASE.EQ.'UPPE')THEN
          CALL DPRCIU(ICHAR3,IOP,X,Y,NUMCO,IXMINC,IXMAXC,IXDELC,
     1                IBUG,IFOUND,IERROR)
        ELSEIF(ICASE.EQ.'LOWE')THEN
          CALL DPRCIL(ICHAR3,IOP,X,Y,NUMCO,IXMINC,IXMAXC,IXDELC,
     1                IBUG,IFOUND,IERROR)
        ENDIF
        IF(IFOUND.EQ.'NO')THEN
          CALL DPRCIN(ICHAR3,IOP,X,Y,NUMCO,IXMINC,IXMAXC,IXDELC,
     1                IBUG,IFOUND,IERROR)
        ENDIF
        IF(IFOUND.EQ.'NO')THEN
          CALL DPRCS(ICHAR3,IOP,X,Y,NUMCO,IXMINC,IXMAXC,IXDELC,
     1               IBUG,IFOUND,IERROR)
        ENDIF
      ELSEIF(IFONT.EQ.'SIMS')THEN
        IF(ICASE.EQ.'UPPE')THEN
          CALL DPRSSU(ICHAR3,IOP,X,Y,NUMCO,IXMINC,IXMAXC,IXDELC,
     1                IBUG,IFOUND,IERROR)
        ELSEIF(ICASE.EQ.'LOWE')THEN
          CALL DPRSSL(ICHAR3,IOP,X,Y,NUMCO,IXMINC,IXMAXC,IXDELC,
     1                IBUG,IFOUND,IERROR)
        ENDIF
        IF(IFOUND.EQ.'NO')THEN
          CALL DPRSN(ICHAR3,IOP,X,Y,NUMCO,IXMINC,IXMAXC,IXDELC,
     1               IBUG,IFOUND,IERROR)
        ENDIF
        IF(IFOUND.EQ.'NO')THEN
          CALL DPRSS(ICHAR3,IOP,X,Y,NUMCO,IXMINC,IXMAXC,IXDELC,
     1               IBUG,IFOUND,IERROR)
        ENDIF
      ELSEIF(IFONT.EQ.'COMS')THEN
        IF(ICASE.EQ.'UPPE')THEN
           CALL DPRCSU(ICHAR3,IOP,X,Y,NUMCO,IXMINC,IXMAXC,IXDELC,
     1                 IBUG,IFOUND,IERROR)
        ELSEIF(ICASE.EQ.'LOWE')THEN
          CALL DPRCSL(ICHAR3,IOP,X,Y,NUMCO,IXMINC,IXMAXC,IXDELC,
     1                IBUG,IFOUND,IERROR)
        ENDIF
        IF(IFOUND.EQ.'NO')THEN
          CALL DPRCSN(ICHAR3,IOP,X,Y,NUMCO,IXMINC,IXMAXC,IXDELC,
     1                IBUG,IFOUND,IERROR)
        ENDIF
        IF(IFOUND.EQ.'NO')THEN
          CALL DPRCS(ICHAR3,IOP,X,Y,NUMCO,IXMINC,IXMAXC,IXDELC,
     1               IBUG,IFOUND,IERROR)
        ENDIF
CCCCC ELSEIF(IFONT.EQ.'COMP')THEN
C
C       NOTE: IFONT='COMP' IS THE DEFAULT CASE IF FONT NOT MATCHED
C
      ELSE
        IF(ICASE.EQ.'UPPE')THEN
          CALL DPRCU(ICHAR3,IOP,X,Y,NUMCO,IXMINC,IXMAXC,IXDELC,
     1               IBUG,IFOUND,IERROR)
        ELSEIF(ICASE.EQ.'LOWE')THEN
          CALL DPRCL(ICHAR3,IOP,X,Y,NUMCO,IXMINC,IXMAXC,IXDELC,
     1               IBUG,IFOUND,IERROR)
        ENDIF
        IF(IFOUND.EQ.'NO')THEN
          CALL DPRCN(ICHAR3,IOP,X,Y,NUMCO,IXMINC,IXMAXC,IXDELC,
     1               IBUG,IFOUND,IERROR)
        ENDIF
        IF(IFOUND.EQ.'NO')THEN
          CALL DPRCS(ICHAR3,IOP,X,Y,NUMCO,IXMINC,IXMAXC,IXDELC,
     1               IBUG,IFOUND,IERROR)
        ENDIF
      ENDIF
C
      IF(IFOUND.EQ.'YES')GOTO1900
C
C               *************************************
C               **  STEP 3.2--                     **
C               **  TREAT THE GREEK ALPHABET CASE  **
C               *************************************
C
      IFOUND='NO'
      IF(IFONT.EQ.'SIMP')THEN
        IF(ICASE.EQ.'UPPE')THEN
          CALL DPGSU(ICHAR3,IOP,X,Y,NUMCO,IXMINC,IXMAXC,IXDELC,
     1               IBUG,IFOUND,IERROR)
        ELSEIF(ICASE.EQ.'LOWE')THEN
          CALL DPGSL(ICHAR3,IOP,X,Y,NUMCO,IXMINC,IXMAXC,IXDELC,
     1               IBUG,IFOUND,IERROR)
        ENDIF
      ELSE
        IF(ICASE.EQ.'UPPE')THEN
          CALL DPGCU(ICHAR3,IOP,X,Y,NUMCO,IXMINC,IXMAXC,IXDELC,
     1               IBUG,IFOUND,IERROR)
        ELSEIF(ICASE.EQ.'LOWE')THEN
          CALL DPGCL(ICHAR3,IOP,X,Y,NUMCO,IXMINC,IXMAXC,IXDELC,
     1               IBUG,IFOUND,IERROR)
        ENDIF
      ENDIF
C
      IF(IFOUND.EQ.'YES')GOTO1900
C
C               ***********************************
C               **  STEP 3.3--                   **
C               **  TREAT THE MATH SYMBOLS CASE  **
C               ***********************************
C
      IFOUND='NO'
      CALL DPMATH(ICHAR3,IOP,X,Y,NUMCO,IXMINC,IXMAXC,IXDELC,
     1            IBUG,IFOUND,IERROR)
      IF(IFOUND.EQ.'YES')THEN
        IMATH='YES'
        GOTO1900
      ELSE
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               ****************************************************
C               **  STEP XX--                                     **
C               **  BRANCH POINT FOR A SUCCESSFUL FIND            **
C               **  (IN THE VARIOUS FONTS) OF ICHAR2 FROM ABOVE.  **
C               ****************************************************
C
 1900 CONTINUE
C
C               ********************************************************
C               **  STEP XX--                                         **
C               **  DRAW OUT THE CHARACTER (IF IDRAW IS ON).          **
C               **  INVISIBLY DRAW OUT THE CHARACTER (TO DETERMINE    **
C               **  LENGTH) (IF I INDEX I IS THE POSITION IN THE      **
C               **  COORDINATE VECTOR INDEX J IS THE VERTEX NUMBER    **
C               **  WITHIN A SUB-TRACE.                               **
C               **  NOTE--(XMAXC2-XMINC2) (= 20) HERSHEY UNITS =      **
C               **  PWIDTH (= %) DA FOR BOTH FIXED SPACING AND        **
C               **  PROPORTIONAL SPACING.  THEREFORE TO TRANSLATE A   **
C               **  HERSHEY DIFFERENCE INTO A DATAPLOT (0 TO 100%     **
C               **  UNITS) DIFFERENCE, MULTIPLY THE HERSHEY           **
C               **  DIFFERENCE BY PWIDTH/(XMAXC2-XMINC2) = PWIDTH/20  **
C               ********************************************************
C
C     NOTE--THE VALUES -8 TO 8 ARE THE ACTUAL HERSHEY
C           WIDTH OF THE ROMAN SIMPLEX UPPER CASE A
C           AND -9 TO 12 ARE THE ACTUAL HERESHEY HEIGHT
C           OF THE ROMAN SIMPLEX UPPER CASE A.
C
      XMINC=IXMINC
      XMAXC=IXMAXC
C
CCCCC XMINC2=(-10.0)
CCCCC XMAXC2=10.0
      XMINC2=(-8.0)
      XMAXC2=8.0
      IF(IMATH.EQ.'YES')XMINC2=XMINC
      IF(IMATH.EQ.'YES')XMAXC2=XMAXC
      YMINC2=(-9.0)
      YMAXC2=12.0
CCCCC IF(IMATH.EQ.'YES')YMINC2=(-10.0)
CCCCC IF(IMATH.EQ.'YES')YMAXC2=10.0
      IF(IMATH.EQ.'YES')YMINC2=XMINC
      IF(IMATH.EQ.'YES')YMAXC2=XMAXC
C
      XFACHP=PWIDTH/(XMAXC2-XMINC2)
      YFACHP=PHEIGH/(YMAXC2-YMINC2)
C
      I=0
      J=0
 2500 CONTINUE
      I=I+1
      IF(I.GT.NUMCO)GOTO2580
      IF(IOP(I).EQ.'MOVE')GOTO2510
      GOTO2530
C
 2510 CONTINUE
      NPTEMP=J
      IFIG='LINE'
      IF(J.GE.1.AND.IDRAW.EQ.'ON')GOTO2520
      GOTO2529
 2520 CONTINUE
      IFLAG='ON'
      ICOLR=-1
      ICOLG=-1
      ICOLB=-1
      CALL DPDRPL(PX,PY,NPTEMP,
     1            IFIG,IPATT,PTHICK,
     1            ICOL,ICOLR,ICOLG,ICOLB,
     1            JPATT,JTHICK,PTHIC2,JCOL,IFLAG)
C
C               *********************************
C               **  FILL (CERTAIN) CHARACTERS  **
C               *********************************
C
      IF(IFILL.EQ.'ON')THEN
        NPTEM2=NPTEMP
C       OCTOBER 1991.  FOLLOWING CODE MODIFIED TO RECOGNIZE CHARACTER
C                      ABREVIATIONS.  SPECIFICALLY, ADDED TR, SQ, DI
C
        IF(ICHAR2.EQ.'PYRA')NPTEM2=4
        IF(ICHAR2.EQ.'CUBE')NPTEM2=5
        IF(ICHAR2.EQ.'TRIA' .OR. ICHAR2.EQ.'TR'   .OR.
     1     ICHAR2.EQ.'SQUA' .OR. ICHAR2.EQ.'SQ'   .OR.
     1     ICHAR2.EQ.'DIAM' .OR. ICHAR2.EQ.'DI'   .OR.
     1     ICHAR2.EQ.'HEXA' .OR.
     1     ICHAR2.EQ.'CIRC' .OR. ICHAR2.EQ.'CI'   .OR.
     1     ICHAR2.EQ.'CUBE' .OR. ICHAR2.EQ.'PYRA' .OR.
     1     ICHAR2.EQ.'REVT' .OR. ICHAR2.EQ.'TRIR' .OR.
     1     ICHAR2.EQ.'TRII' .OR. ICHAR2.EQ.'RT  ' .OR.
     1     ICHAR2.EQ.'ARRO' .OR. ICHAR2.EQ.'ARRH')THEN
C
          NP=NPTEM2
          IFLAG='LOOP'
          CALL DPDRPL(PX,PY,NPTEM2,
     1                IFIG,IPATT,PTHICK,
     1                ICOL,ICOLR,ICOLG,ICOLB,
     1                JPATT,JTHICK,PTHIC2,JCOL,IFLAG)
        ENDIF
      ENDIF
C
 2529 CONTINUE
      J=0
      GOTO2530
C
 2530 CONTINUE
      J=J+1
CCCCC X2=X(I)-XMINC2
CCCCC IF(ISPAC.EQ.'PROP')X2=X(I)-XMINC
      X2=X(I)-XMINC
      Y2=Y(I)-YMINC2
      X3=X2*XFACHP
      Y3=Y2*YFACHP
      X5=XSTAR2+X3
      Y5=YSTAR2+Y3
      CALL DPROTA(X5,Y5,XSTAR2,YSTAR2,ANGLE,AMAX,X6,Y6)
      PX(J)=X6
      PY(J)=Y6
      GOTO2500
C
 2580 CONTINUE
      NPTEMP=J
      IF(J.GE.1.AND.IDRAW.EQ.'ON')GOTO2590
      GOTO2599
 2590 CONTINUE
      IFLAG='ON'
      CALL DPDRPL(PX,PY,NPTEMP,
     1            IFIG,IPATT,PTHICK,
     1            ICOL,ICOLR,ICOLG,ICOLB,
     1            JPATT,JTHICK,PTHIC2,JCOL,IFLAG)
C
C               *********************************
C               **  FILL (CERTAIN) CHARACTERS  **
C               *********************************
C
C  OCTOBER 1991.  FOLLOWING CODE MODIFIED TO RECOGNIZE CHARACTER
C                 ABREVIATIONS.  SPECIFICALLY, ADDED TR, SQ, DI
C
      IF(IFILL.EQ.'ON')THEN
        NPTEM2=NPTEMP
        IF(ICHAR2.EQ.'CUBE')NPTEM2=5
        IF(ICHAR2.EQ.'PYRA')NPTEM2=4
        IF(ICHAR2.EQ.'TRIA' .OR. ICHAR2.EQ.'TR'   .OR.
     1     ICHAR2.EQ.'SQUA' .OR. ICHAR2.EQ.'SQ'   .OR.
     1     ICHAR2.EQ.'DIAM' .OR. ICHAR2.EQ.'DI'   .OR.
     1     ICHAR2.EQ.'HEXA' .OR.
     1     ICHAR2.EQ.'CIRC' .OR. ICHAR2.EQ.'CI'   .OR.
     1     ICHAR2.EQ.'CUBE' .OR. ICHAR2.EQ.'PYRA' .OR.
     1     ICHAR2.EQ.'REVT' .OR. ICHAR2.EQ.'TRIR' .OR.
     1     ICHAR2.EQ.'TRII' .OR. ICHAR2.EQ.'RT  ' .OR.
     1     ICHAR2.EQ.'ARRO' .OR. ICHAR2.EQ.'ARRH')THEN
          IHORPA='OFF'
          IVERPA='ON'
          IDUPPA='OFF'
          IDDOPA='OFF'
          PXSPA2=0.1
          PYSPA2=0.1
          ICOLF=ICOL
          JCOLF=JCOL
          ICOLP=ICOL
          JCOLP=JCOL
          CALL GRFIRE(PX,PY,NPTEM2,IFIG,
     1                IPATT,JPATT,IHORPA,IVERPA,IDUPPA,IDDOPA,
     1                PXSPA2,PYSPA2,PTHICK,JTHICK,PTHIC2,
     1                ICOLF,JCOLF,ICOLP,JCOLP,
     1                ICOLR,ICOLG,ICOLB,ICOLR,ICOLG,ICOLB,
     1                IRGBFL,IPATT2)
        ENDIF
      ENDIF
C
 2599 CONTINUE
C
C     X2 IS THE WIDTH OF THE CHARACTER (NO SPACING) IN HERSHEY UNITS
C     X3 IS THE WIDTH OF THE CHARACTER (NO SPACING) IN DATAPLOT UNITS
C     X4 IS THE WIDHT OF THE CHARACTER + SPACING IN DATAPLOT UNITS
C
      X2=XMAXC2-XMINC2
      IF(ISPAC.EQ.'PROP')X2=XMAXC-XMINC
      X3=X2*XFACHP
      X4=X3+PHOGAP
      X5=XSTAR2+X4
      Y5=YSTAR2
      CALL DPROTA(X5,Y5,XSTAR2,YSTAR2,ANGLE,AMAX,X6,Y6)
      XEND2=X6
      YEND2=Y6
C
C               ********************************************************
C               **  STEP 3.6--                                        **
C               **  ARE WE DECOMPOSING ICHAR2 CHARACTER BY CHARACTER? **
C               **  (USUALLY N)  IF NOT, THEN EXIT.                   **
C               **  IF SO, ARE WE DONE?                               **
C               ********************************************************
C
      XEND=XEND2
      YEND=YEND2
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
C
 9000 CONTINUE
C
      IF(IBUGG4.EQ.'ON' .OR. ISUBG4.EQ.'SCR8')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('****** AT THE END       OF DPSCR8--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)XSTART,YSTART,XEND,YEND
 9012   FORMAT('XSTART,YSTART,XEND,YEND = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9041)IMATH,XMINC,XMAXC
 9041   FORMAT('IMATH,XMINC,XMAXC = ',A4,2X,2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9042)XMINC2,XMAXC2,YMINC2,YMAXC2
 9042   FORMAT('XMINC2,XMAXC2,YMINC2,YMAXC2 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9043)XFACHP,YFACHP,Y5,Y6
 9043   FORMAT('XFACHP,YFACHP,Y5,Y6 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9044)X2,X3,X4,X5,X6
 9044   FORMAT('X2,X3,X4,X5,X6 = ',5G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9046)XSTART,XEND,YSTART,YEND
 9046   FORMAT('XSTART,XEND,YSTART,YEND = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9051)XSTAR2,XEND2,YSTAR2,YEND2
 9051   FORMAT('XSTAR2,XEND2,YSTAR2,YEND2 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9053)ICHAR2,ICHAR3
 9053   FORMAT('ICHAR2,ICHAR3 = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9056)PWIDTH,PPENTH,NLOOP
 9056   FORMAT('PWIDTH,PPENTH,NLOOP = ',2G15.7,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9059)IFOUND,IERROR
 9059   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPSDCL(IHARG,NUMARG,IDSDCO,ISDFCO,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE COLOR FOR THE 3-D SIDEFACE.
C              THE COLOR FOR THE SIDEFACE WILL BE PLACED
C              IN THE CHARACTER VARIABLE ISDFCO.
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --NUMARG
C                     --IDSDCO
C     OUTPUT ARGUMENTS--ISDFCO
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     NOTE--THIS SUBROUTINE ASSUMES A
C           COMPLICATED-TO-SIMPLE CHECKING ORDER
C           (IN MAIPC4) OF THE VARIOUS SIDEFACE COMMANDS.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--88/10
C     ORIGINAL VERSION--SEPTEMBER 1988.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IDSDCO
      CHARACTER*4 ISDFCO
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
      IF(NUMARG.EQ.0)GOTO1199
      IF(NUMARG.EQ.1)GOTO1150
C
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      GOTO1160
C
 1150 CONTINUE
      ISDFCO=IDSDCO
      GOTO1180
C
 1160 CONTINUE
      ISDFCO=IHARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)ISDFCO
 1181 FORMAT('THE (3-D) SIDEFACE COLOR ',
     1'HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPSDCI(XTEMP1,XTEMP2,MAXNXT,ICASAN,
     1                  ICAPSW,IFORSW,IMULT,IREPL,
     1                  ISUBRO,IBUGA2,IBUGA3,IBUGQ,IFOUND,IERROR)
C
C     PURPOSE--GENERATE A CONFIDENCE INTERVAL FOR THE STANDARD
C              DEVIATION FOR NORMALLY DISTRIBUTED DATA.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2013/4
C     ORIGINAL VERSION--APRIL     2013.
C     UPDATED         --DECEMBER  2017. ADD BONETT'S INTERVAL FOR
C                                       NON-NORMAL DATA
C     UPDATED         --AUGUST    2019. ADD CTL999, CTU999
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICAPSW
      CHARACTER*4 IFORSW
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGQ
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 ICASAN
      CHARACTER*4 ICASA2
      CHARACTER*4 ICASA3
      CHARACTER*4 ICASA4
      CHARACTER*4 ICASE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IFLAGU
C
      LOGICAL IFRST
      LOGICAL ILAST
C
      CHARACTER*4 IREPL
      CHARACTER*4 IMULT
      CHARACTER*4 ICTMP0
      CHARACTER*4 ICTMP1
      CHARACTER*4 ICTMP2
      CHARACTER*4 ICTMP3
      CHARACTER*4 ICTMP4
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
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TEMP3(MAXOBV)
C
      DIMENSION XDESGN(MAXOBV,6)
      DIMENSION XIDTEM(MAXOBV)
      DIMENSION XIDTE2(MAXOBV)
      DIMENSION XIDTE3(MAXOBV)
      DIMENSION XIDTE4(MAXOBV)
      DIMENSION XIDTE5(MAXOBV)
      DIMENSION XIDTE6(MAXOBV)
C
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(IGARB1),XIDTEM(1))
      EQUIVALENCE (GARBAG(IGARB2),XIDTE2(1))
      EQUIVALENCE (GARBAG(IGARB3),XIDTE3(1))
      EQUIVALENCE (GARBAG(IGARB4),XIDTE4(1))
      EQUIVALENCE (GARBAG(IGARB5),XIDTE5(1))
      EQUIVALENCE (GARBAG(IGARB6),XIDTE6(1))
      EQUIVALENCE (GARBAG(IGARB7),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB8),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB9),TEMP3(1))
      EQUIVALENCE (GARBAG(IGAR10),XDESGN(1,1))
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPSD'
      ISUBN2='CI  '
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
C               *******************************************
C               **  TREAT THE SD CONFIDENCE LIMITS CASE  **
C               *******************************************
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'SDCI')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPSDCI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA2,IBUGA3,IBUGQ,ISUBRO,MAXNXT
   52   FORMAT('IBUGA2,IBUGA3,IBUGQ,ISUBRO,MAXNXT = ',4(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *********************************
C               **  STEP 1--                   **
C               **  EXTRACT THE COMMAND        **
C               *********************************
C
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SDCI')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     THE FOLLOWING COMMANDS ARE ACCEPTED:
C
C         STANDARD DEVIATION CONFIDENCE LIMITS Y        (TWO SIDED)
C         LOWER STANDARD DEVIATION CONFIDENCE LIMITS Y  (ONE SIDED)
C         UPPER STANDARD DEVIATION CONFIDENCE LIMITS Y  (ONE SIDED)
C
C
C     IN ADDITION, CHECK FOR THE "MULTIPLE" AND "REPLICATION" OPTIONS.
C
      ILASTZ=9999
      IFOUND='NO'
      ICASAN='LIMI'
      ICASA2='UPPE'
      ICASA3='RAW'
      ICASA4='TWOS'
C
      DO100I=0,NUMARG-1
C
        ICTMP0='XXXX'
        IF(I.EQ.0)THEN
          ICTMP1=ICOM
          ICTMP2=IHARG(I+1)
          ICTMP3=IHARG(I+2)
          ICTMP4=IHARG(I+3)
        ELSE
          IF(I.GE.2)ICTMP0=IHARG(I-1)
          ICTMP1=IHARG(I)
          ICTMP2=IHARG(I+1)
          ICTMP3=IHARG(I+2)
          ICTMP4=IHARG(I+3)
        ENDIF
C
        IF(ICTMP1.EQ.'SD  ' .AND. ICTMP2.EQ.'CONF' .AND.
     1         ICTMP3.EQ.'LIMI')THEN
          IFOUND='YES'
          ILASTZ=I+2
          ICASAN='SDLI'
          GOTO109
        ELSEIF(ICTMP1.EQ.'SD  ' .AND. ICTMP2.EQ.'CONF' .AND.
     1         ICTMP3.EQ.'INTE')THEN
          IFOUND='YES'
          ILASTZ=I+2
          ICASAN='SDLI'
          GOTO109
        ELSEIF(ICTMP1.EQ.'STAN' .AND. ICTMP2.EQ.'DEVI' .AND.
     1         ICTMP3.EQ.'CONF' .AND. ICTMP4.EQ.'LIMI')THEN
          IFOUND='YES'
          ILASTZ=I+3
          ICASAN='SDLI'
          GOTO109
        ELSEIF(ICTMP1.EQ.'STAN' .AND. ICTMP2.EQ.'DEVI' .AND.
     1         ICTMP3.EQ.'CONF' .AND. ICTMP4.EQ.'INTE')THEN
          IFOUND='YES'
          ILASTZ=I+3
          ICASAN='SDLI'
          GOTO109
        ELSEIF(ICTMP1.EQ.'LOWE')THEN
          ICASA4='ONES'
          ICASA2='LOWE'
        ELSEIF(ICTMP1.EQ.'UPPE')THEN
          ICASA4='ONES'
          ICASA2='UPPE'
        ELSEIF(ICTMP1.EQ.'ONE ' .AND. ICTMP2.EQ.'SIDE')THEN
          ICASA4='ONES'
        ELSEIF(ICTMP1.EQ.'REPL')THEN
          IREPL='ON'
        ELSEIF(ICTMP1.EQ.'MULT')THEN
          IMULT='ON'
        ENDIF
  100 CONTINUE
  109 CONTINUE
C
      IF(IFOUND.EQ.'NO')GOTO9000
      ISHIFT=ILASTZ
      CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1            IBUGA2,IERROR)
C
      IF(IMULT.EQ.'ON')THEN
        IF(IREPL.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
  101     FORMAT('***** ERROR IN STANDARD DEVIATION CONFIDENCE ',
     1           'LIMITS--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,102)
  102     FORMAT('      YOU CANNOT SPECIFY BOTH "MULTIPLE" AND ',
     1           '"REPLICATION" FOR THIS COMMAND.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ENDIF
C
C               *********************************
C               **  STEP 2--                   **
C               **  EXTRACT THE VARIABLE LIST  **
C               *********************************
C
      ISTEPN='2'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SDCI')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='SD CONFIDENCE LIMITS'
      MAXNA=100
      MINNVA=1
      MAXNVA=100
      MINNA=1
      IFLAGE=1
      IFLAGM=1
      IF(IREPL.EQ.'ON')THEN
        MAXNVA=7
        IFLAGM=0
      ELSE
        MAXNVA=30
        IFLAGE=0
      ENDIF
      MINN2=4
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
      IF(NUMVAR.GT.1 .AND. IREPL.EQ.'OFF')IMULT='ON'
C
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SDCI')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,181)
  181   FORMAT('***** AFTER CALL DPPARS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,182)NQ,NUMVAR,IMULT,IREPL
  182   FORMAT('NQ,NUMVAR,IMULT,IREPL = ',2I8,2(2X,A4))
        CALL DPWRST('XXX','BUG ')
        IF(NUMVAR.GT.0)THEN
          DO185I=1,NUMVAR
            WRITE(ICOUT,187)I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),
     1                      ICOLR(I)
  187       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',
     1             'ICOLR(I) = ',I8,2X,A4,A4,2X,3I8)
            CALL DPWRST('XXX','BUG ')
  185     CONTINUE
        ENDIF
      ENDIF
C
C               ***********************************************
C               **  STEP 2--                                 **
C               **  DETERMINE:                               **
C               **  1) NUMBER OF REPLICATION VARIABLES (0-6) **
C               **  2) NUMBER OF RESPONSE    VARIABLES (>= 1)**
C               ***********************************************
C
      ISTEPN='2'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SDCI')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NRESP=0
      NREPL=0
C
      IF(IMULT.EQ.'ON')THEN
        NRESP=NUMVAR
      ELSEIF(IREPL.EQ.'ON')THEN
        NRESP=1
        NREPL=NUMVAR-NRESP
        IF(NREPL.LT.1 .OR. NREPL.GT.6)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,211)
  211     FORMAT('      FOR THE REPLICATION CASE, THE NUMBER OF ',
     1           'REPLICATION VARIABLES')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,212)
  212     FORMAT('      MUST BE BETWEEN ONE AND SIX.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,213)NREPL
  213     FORMAT('      THE NUMBER OF REPLICATION VARIABLES = ',I5)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ELSE
        NRESP=1
      ENDIF
C
      IH='NNEW'
      IH2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,
     1            IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1            ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
      IF(IERROR.EQ.'YES')THEN
        NNEW=1
      ELSE
        NNEW=INT(VALUE(ILOCV)+0.5)
        IF(NNEW.LT.1)NNEW=1
      ENDIF
C
      IH='N0  '
      IH2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,
     1            IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1            ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'NO')THEN
        N0=INT(VALUE(ILOCP)+0.5)
      ELSE
        N0=0
      ENDIF
C
      IH='KURT'
      IH2='OSIS'
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,
     1            IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1            ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'NO')THEN
        AKURT=VALUE(ILOCP)
      ELSE
        AKURT=CPUMIN
      ENDIF
C
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SDCI')THEN
        WRITE(ICOUT,221)NRESP,NREPL,NNEW,N0,AKURT
  221   FORMAT('NRESP,NREPL,NNEW,N0,AKURT = ',4I5,G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ******************************************************
C               **  STEP 3--                                        **
C               **  GENERATE THE PREDICTION LIMITS FOR THE VARIOUS  **
C               **  CASES                                           **
C               ******************************************************
C
      ISTEPN='3'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SDCI')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               *****************************************
C               **  STEP 3A--                          **
C               **  CASE 1: NO REPLICATION             **
C               *****************************************
C
      IF(NREPL.EQ.0)THEN
        ISTEPN='3A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SDCI')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C       LOOP THROUGH EACH OF THE RESPONSE VARIABLES
C
        NCURVE=0
        DO410IRESP=1,NRESP
          NCURVE=NCURVE+1
C
          IINDX=ICOLR(IRESP)
          PID(1)=CPUMIN
          IVARID(1)=IVARN1(IRESP)
          IVARI2(1)=IVARN2(IRESP)
C
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SDCI')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,411)IRESP,NCURVE
  411       FORMAT('IRESP,NCURVE = ',2I5)
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
     1                Y,XTEMP1,XTEMP2,NLOCAL,NLOCA2,NLOCA3,ICASE,
     1                IBUGA3,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
C         *****************************************************
C         **  STEP 4B--                                      **
C         *****************************************************
C
          ISTEPN='4B'
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SDCI')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'SDCI')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,422)
  422       FORMAT('***** FROM THE MIDDLE OF DPSDCI--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,423)ICASAN,NUMVAR,NLOCAL,IRESP
  423       FORMAT('ICASAN,NUMVAR,NLOCAL,IRESP = ',A4,3I8)
            CALL DPWRST('XXX','BUG ')
            IF(NLOCAL.GE.1)THEN
              DO425I=1,NLOCAL
                WRITE(ICOUT,426)I,Y(I)
  426           FORMAT('I,Y(I) = ',I8,F12.5)
                CALL DPWRST('XXX','BUG ')
  425         CONTINUE
            ENDIF
          ENDIF
C
          CALL DPSDC2(Y,NLOCAL,ICASAN,ICASA2,ICASA3,ICASA4,
     1                PID,IVARID,IVARI2,NREPL,
     1                CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1                CTL999,CTU999,
     1                ICAPSW,ICAPTY,IFORSW,IBONSD,IBONAD,
     1                TEMP3,MAXNXT,AKURT,N0,
     1                ISUBRO,IBUGA3,IERROR)
C
          IFLAGU='FILE'
          IFRST=.FALSE.
          ILAST=.FALSE.
          IF(IRESP.EQ.1)IFRST=.TRUE.
          IF(IRESP.EQ.NRESP)ILAST=.TRUE.
          CALL DPCNF3(CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1                CTL999,CTU999,
     1                IFLAGU,IFRST,ILAST,ICASAN,
     1                IBUGA2,IBUGA3,ISUBRO,IERROR)
C
  410   CONTINUE
C
C               ****************************************************
C               **  STEP 5A--                                     **
C               **  CASE 3: ONE OR MORE REPLICATION VARIABLES.    **
C               **          FOR THIS CASE, ALL VARIABLES MUST     **
C               **          HAVE THE SAME LENGTH.                 **
C               ****************************************************
C
      ELSEIF(IREPL.EQ.'ON')THEN
        ISTEPN='5A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SDCI')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        J=0
        IMAX=NRIGHT(1)
        IF(NQ.LT.NRIGHT(1))IMAX=NQ
        DO510I=1,IMAX
          IF(ISUB(I).EQ.0)GOTO510
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
            DO520IR=1,MIN(NREPL,6)
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
  520       CONTINUE
          ENDIF
C
  510   CONTINUE
        NLOCAL=J
C
        ISTEPN='5B'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SDCI')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        PID(1)=CPUMIN
        IVARID(1)=IVARN1(1)
        IVARI2(1)=IVARN2(1)
        IADD=1
        DO540II=1,NREPL
          IVARID(II+IADD)=IVARN1(II+IADD)
          IVARI2(II+IADD)=IVARN2(II+IADD)
  540   CONTINUE
C
C       *****************************************************
C       **  STEP 5C--                                      **
C       **                                                 **
C       **  FOR THIS CASE, WE NEED TO LOOP THROUGH THE     **
C       **  VARIOUS REPLICATIONS.                          **
C       *****************************************************
C
        ISTEPN='5C'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'SDCI')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'SDCI')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,541)
  541     FORMAT('***** FROM THE MIDDLE  OF DPSDCL--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,542)ICASAN,NUMVAR,NLOCAL,NLOCA2,NREPL
  542     FORMAT('ICASAN,NUMVAR,NLOCAL,NLOCA2,NREPL = ',A4,2X,4I8)
          CALL DPWRST('XXX','BUG ')
          IF(NLOCAL.GE.1)THEN
            DO545I=1,NLOCAL
              WRITE(ICOUT,546)I,Y(I),X(I),XDESGN(I,1),XDESGN(I,2)
  546         FORMAT('I,Y(I),X(I),XDESGN(I,1),XDESGN(I,2) = ',
     1               I8,4F12.5)
              CALL DPWRST('XXX','BUG ')
  545       CONTINUE
          ENDIF
        ENDIF
C
C       *****************************************************
C       **  STEP 5C--                                      **
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
C       **  STEP 5D--                                      **
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
                TEMP1(K)=Y(I)
                TEMP2(K)=X(I)
              ENDIF
 1130       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            IF(NTEMP.GT.0)THEN
              CALL DPSDC2(TEMP1,NTEMP,ICASAN,ICASA2,ICASA3,ICASA4,
     1                    PID,IVARID,IVARI2,NREPL,
     1                    CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1                    CTL999,CTU999,
     1                    ICAPSW,ICAPTY,IFORSW,IBONSD,IBONAD,
     1                    TEMP3,MAXOBV,AKURT,N0,
     1                    ISUBRO,IBUGA3,IERROR)
            ENDIF
C
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NUMSE1)ILAST=.TRUE.
            CALL DPCNF3(CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1                  CTL999,CTU999,
     1                  IFLAGU,IFRST,ILAST,ICASAN,
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
                TEMP1(K)=Y(I)
                TEMP2(K)=X(I)
              ENDIF
 1290       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              CALL DPSDC2(TEMP1,NTEMP,ICASAN,ICASA2,ICASA3,ICASA4,
     1                    PID,IVARID,IVARI2,NREPL,
     1                    CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1                    CTL999,CTU999,
     1                    ICAPSW,ICAPTY,IFORSW,IBONSD,IBONAD,
     1                    TEMP3,MAXOBV,AKURT,N0,
     1                    ISUBRO,IBUGA3,IERROR)
            ENDIF
            NPLOT2=NPLOTP
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            CALL DPCNF3(CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1                  CTL999,CTU999,
     1                  IFLAGU,IFRST,ILAST,ICASAN,
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
                TEMP1(K)=Y(I)
                TEMP2(K)=X(I)
              ENDIF
 1390       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            IF(NTEMP.GT.0)THEN
              CALL DPSDC2(TEMP1,NTEMP,ICASAN,ICASA2,ICASA3,ICASA4,
     1                    PID,IVARID,IVARI2,NREPL,
     1                    CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1                    CTL999,CTU999,
     1                    ICAPSW,ICAPTY,IFORSW,IBONSD,IBONAD,
     1                    TEMP3,MAXOBV,AKURT,N0,
     1                    ISUBRO,IBUGA3,IERROR)
            ENDIF
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            CALL DPCNF3(CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1                  CTL999,CTU999,
     1                  IFLAGU,IFRST,ILAST,ICASAN,
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
                TEMP1(K)=Y(I)
                TEMP2(K)=X(I)
              ENDIF
 1490       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            IF(NTEMP.GT.0)THEN
              CALL DPSDC2(TEMP1,NTEMP,ICASAN,ICASA2,ICASA3,ICASA4,
     1                    PID,IVARID,IVARI2,NREPL,
     1                    CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1                    CTL999,CTU999,
     1                    ICAPSW,ICAPTY,IFORSW,IBONSD,IBONAD,
     1                    TEMP3,MAXOBV,AKURT,N0,
     1                    ISUBRO,IBUGA3,IERROR)
            ENDIF
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            CALL DPCNF3(CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1                  CTL999,CTU999,
     1                  IFLAGU,IFRST,ILAST,ICASAN,
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
                TEMP1(K)=Y(I)
                TEMP2(K)=X(I)
              ENDIF
 1590       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            IF(NTEMP.GT.0)THEN
              CALL DPSDC2(TEMP1,NTEMP,ICASAN,ICASA2,ICASA3,ICASA4,
     1                    PID,IVARID,IVARI2,NREPL,
     1                    CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1                    CTL999,CTU999,
     1                    ICAPSW,ICAPTY,IFORSW,IBONSD,IBONAD,
     1                    TEMP3,MAXOBV,AKURT,N0,
     1                    ISUBRO,IBUGA3,IERROR)
            ENDIF
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            CALL DPCNF3(CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1                  CTL999,CTU999,
     1                  IFLAGU,IFRST,ILAST,ICASAN,
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
                TEMP1(K)=Y(I)
                TEMP2(K)=X(I)
              ENDIF
 1690       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            IF(NTEMP.GT.0)THEN
              CALL DPSDC2(TEMP1,NTEMP,ICASAN,ICASA2,ICASA3,ICASA4,
     1                    PID,IVARID,IVARI2,NREPL,
     1                    CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1                    CTL999,CTU999,
     1                    ICAPSW,ICAPTY,IFORSW,IBONSD,IBONAD,
     1                    TEMP3,MAXOBV,AKURT,N0,
     1                    ISUBRO,IBUGA3,IERROR)
            ENDIF
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            CALL DPCNF3(CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1                  CTL999,CTU999,
     1                  IFLAGU,IFRST,ILAST,ICASAN,
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
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'SDCI')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSDCL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)IFOUND,IERROR
 9016   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPSDC2(Y,N,ICASAN,ICASA2,ICASA3,ICASA4,
     1                  PID,IVARID,IVARI2,NREPL,
     1                  CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1                  CTL999,CTU999,
     1                  ICAPSW,ICAPTY,IFORSW,IBONSD,IBONAD,
     1                  TEMP1,MAXNXT,AKURT,N0,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--GENERATE A CONFIDENCE INTERVAL FOR THE STANDARD
C              DEVIATION FOR NORMALLY DISTRIBUTED DATA.
C
C     INPUT  ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR OF 
C                                ORIGINAL OBSERVATIONS.
C                       N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR Y.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     REFERENCES--HAHN AND MEEKER (1991), "STATISTICAL INTERVALS: A
C                 GUIDE FOR PRACTIONERS", WILEY, PP. 55-56.
C               --BONETT (2006), "APPROXIMATE CONFIDENCE INTERVAL FOR
C                 STANDARD DEVIATION OF NONNORMAL DISTRIBUTIONS",
C                 COMPUTATIONAL STATISTICS AND DATA ANALYSIS,
C                 VOL. 50, PP. 775 - 782.
C               --NIWITPONG AND KIRDWICHAI (2008), "ADJUSTED BONETT
C                 CONFIDENCE INTERVAL FOR STANDARD DEVIATION OF
C                 NON-NORMAL DISTRIBUTIONS", THAILAND STATISTICIAN,
C                 VOL. 6, NO. 1, PP. 1-6.
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2013/4
C     ORIGINAL VERSION--APRIL     2013.
C     UPDATED         --DECEMBER  2017. ADD BONETT'S INTERVAL FOR
C                                       NON-NORMAL DATA
C     UPDATED         --AUGUST    2019. ADD CTL999, CTU999
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASAN
      CHARACTER*4 ICASA2
      CHARACTER*4 ICASA3
      CHARACTER*4 ICASA4
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 IBONSD
      CHARACTER*4 IBONAD
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 IVARID(*)
      CHARACTER*4 IVARI2(*)
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION TEMP1(*)
      DIMENSION PID(*)
C
      PARAMETER (NUMALP=6)
      REAL ALPHA(NUMALP)
      REAL CONF(NUMALP)
C
      DIMENSION ALOWLM(NUMALP)
      DIMENSION AUPPLM(NUMALP)
C
      PARAMETER(NUMCLI=4)
      PARAMETER(MAXLIN=2)
      PARAMETER (MAXROW=20)
      CHARACTER*60 ITITLE
      CHARACTER*60 ITITLZ
      CHARACTER*40 ITITL9
      CHARACTER*60 ITEXT(MAXROW)
      CHARACTER*4  ALIGN(NUMCLI)
      CHARACTER*4  VALIGN(NUMCLI)
      CHARACTER*4  ITYPCO(NUMCLI)
      CHARACTER*20 ITITL2(MAXLIN,NUMCLI)
      CHARACTER*4  IVALUE(MAXROW,NUMCLI)
      REAL         AVALUE(MAXROW)
      REAL         AMAT(MAXROW,NUMCLI)
      INTEGER      NCVALU(MAXROW,NUMCLI)
      INTEGER      NCTIT2(MAXLIN,NUMCLI)
      INTEGER      NCTEXT(MAXROW)
      INTEGER      IDIGIT(MAXROW)
      INTEGER      NTOT(MAXROW)
      INTEGER      IWHTML(NUMCLI)
      INTEGER      IWRTF(NUMCLI)
      LOGICAL IFRST
      LOGICAL ILAST
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      DATA ALPHA /0.50, 0.80, 0.90, 0.95, 0.99, 0.999/
C
      ISUBN1='DPSD'
      ISUBN2='C2  '
      IERROR='NO'
      IWRITE='OFF'
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SDC2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPSDC2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,ICASAN,ICASA2,ICASA3,ICASA4
   52   FORMAT('IBUGA3,ISUBRO,ICASAN,ICASA2,ICASA3,ICASA4 = ',
     1         5(A4,2X),A4)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,54)N,IBONSD,IBONAD,AKURT,N0
   54   FORMAT('N,IBONSD,IBONAD,AKURT,N0 = ',I8,2(2X,A4),G15.7,I8)
        CALL DPWRST('XXX','WRIT')
        DO56I=1,N
          WRITE(ICOUT,57)I,Y(I)
   57     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SDC2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N.LE.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR IN STANDARD DEVIATION CONFIDENCE LIMITS--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,103)
  103   FORMAT('      THE NUMBER OF OBSERVATIONS IN THE RESPONSE ',
     1         'VARIABLE IS LESS THAN TWO.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,105)N
  105   FORMAT('SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      HOLD=Y(1)
      DO135I=2,N
        IF(Y(I).NE.HOLD)GOTO139
  135 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,101)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,131)HOLD
  131 FORMAT('      THE RESPONSE VARIABLE HAS ALL ELEMENTS = ',G15.7)
      CALL DPWRST('XXX','WRIT')
      GOTO9000
  139 CONTINUE
C
C               ***************************************
C               **  STEP 3--                         **
C               **  COMPUTE CONFIDENCE LIMITS        **
C               **  FOR VARIOUS PROBABILITY VALUES.  **
C               ***************************************
C
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SDC2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     ICASAN - LIMI   => CONFIDENCE LIMIT FOR THE SD
C     ICASA2:  LOWE   => LOWER LIMIT
C              UPPE   => UPPER LIMIT
C     ICASA3:  RAW    => RAW DATA
C              SUMM   => SUMMARY DATA
C     ICASA4:  ONES   => ONE-SIDED LIMIT
C              TWOS   => TWO-SIDED LIMIT
C
C     DO STANDARD INTERVAL ON PASS ONE, THEN IF REQUESTED
C     DO BONETT'S INTERVAL ON PASS TWO.
C
      AN=N
      ICASA3='RAW'
      IPASS=0
C
  400 CONTINUE
C
      IPASS=IPASS+1
      IF(IPASS.EQ.2)THEN
        IF(IBONSD.EQ.'OFF')GOTO9000
      ELSEIF(IPASS.GT.2)THEN
        GOTO9000
      ENDIF
C
      IF(IPASS.EQ.1)THEN
        CALL MEAN(Y,N,IWRITE,YMEAN,IBUGA3,IERROR)
        CALL DPSDC3(Y,N,ICASAN,ICASA2,ICASA3,ICASA4,
     1              YSD,
     1              ALPHA,NUMALP,ALOWLM,AUPPLM,
     1              ISUBRO,IBUGA3,IERROR)
      ELSE
        CALL MEAN(Y,N,IWRITE,YMEAN,IBUGA3,IERROR)
        CALL DPSDR3(Y,N,ICASA2,ICASA4,MAXNXT,
     1              TEMP1,AKURT,N0,IBONAD,
     1              YSD,
     1              ALPHA,NUMALP,ALOWLM,AUPPLM,
     1              ISUBRO,IBUGA3,IERROR)
      ENDIF
C
      CUTL90=ALOWLM(3)
      CUTU90=AUPPLM(3)
      CUTL95=ALOWLM(4)
      CUTU95=AUPPLM(4)
      CUTL99=ALOWLM(5)
      CUTU99=AUPPLM(5)
      CTL999=ALOWLM(6)
      CTU999=AUPPLM(6)
      NALP=NUMALP
      DO420I=1,NUMALP
        CONF(I)=100.0*ALPHA(I) + 0.0001
  420 CONTINUE
C
C               ****************************
C               **  STEP 5--              **
C               **  WRITE EVERYTHING OUT  **
C               ****************************
C
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SDC2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPRINT.EQ.'OFF')GOTO9000
C
      IF(ICASA4.EQ.'TWOS')THEN
        ITITLE='Two-Sided Confidence Limits for the SD'
        NCTITL=38
        IF(IPASS.EQ.2)THEN
          IF(IBONAD.EQ.'ON')THEN
            ITITLZ='Bonett Interval (Adjusted) for Non-Normality'
            NCTITZ=44
          ELSE
            ITITLZ='Bonett Interval for Non-Normality'
            NCTITZ=33
          ENDIF
        ELSE
          ITITLZ=' '
          NCTITZ=0
        ENDIF
      ELSEIF(ICASA4.EQ.'ONES')THEN
        IF(ICASA2.EQ.'LOWE')THEN
          ITITLE='One-Sided Lower Confidence Limits for the SD'
          NCTITL=44
          IF(IPASS.EQ.2)THEN
            IF(IBONAD.EQ.'ON')THEN
              ITITLZ='Bonett Interval (Adjusted) for Non-Normality'
              NCTITZ=44
            ELSE
              ITITLZ='Bonett Interval for Non-Normality'
              NCTITZ=33
            ENDIF
          ELSE
            ITITLZ=' '
            NCTITZ=0
          ENDIF
        ELSEIF(ICASA2.EQ.'UPPE')THEN
          ITITLE='One-Sided Upper Confidence Limits for the SD'
          NCTITL=44
          IF(IPASS.EQ.2)THEN
            IF(IBONAD.EQ.'ON')THEN
              ITITLZ='Bonett Interval (Adjusted) for Non-Normality'
              NCTITZ=44
            ELSE
              ITITLZ='Bonett Interval for Non-Normality'
              NCTITZ=33
            ENDIF
          ELSE
            ITITLZ=' '
            NCTITZ=0
          ENDIF
        ENDIF
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
        DO4101I=1,NREPL
          ICNT=ICNT+1
          ITEMP=I+NRESP
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
      ITEXT(ICNT)='Sample Mean:'
      NCTEXT(ICNT)=12
      AVALUE(ICNT)=YMEAN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Standard Deviation:'
      NCTEXT(ICNT)=26
      AVALUE(ICNT)=YSD
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
      ISTEPN='5A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SDC2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1            AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
      DO4215J=1,NUMCLI
        DO4218I=1,MAXLIN
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
 4218   CONTINUE
        DO4219I=1,MAXROW
          NCVALU(I,J)=0
          IVALUE(I,J)=' '
          AMAT(I,J)=0.0
 4219   CONTINUE
 4215   CONTINUE
C
      ITITL2(1,1)='Confidence'
      NCTIT2(1,1)=10
      ITITL2(2,1)='Value (%)'
      NCTIT2(2,1)=9
      ITITL2(1,2)='Standard'
      NCTIT2(1,2)=8
      ITITL2(2,2)='Deviation'
      NCTIT2(2,2)=9
      ICOL=2
C
      IF(ICASA4.EQ.'TWOS' .OR.
     1  (ICASA4.EQ.'ONES' .AND. ICASA2.EQ.'LOWE'))THEN
        ICOL=ICOL+1
        ITITL2(1,ICOL)='Lower'
        NCTIT2(1,ICOL)=5
        ITITL2(2,ICOL)='Limit'
        NCTIT2(2,ICOL)=5
      ENDIF
C
      IF(ICASA4.EQ.'TWOS' .OR.
     1  (ICASA4.EQ.'ONES' .AND. ICASA2.EQ.'UPPE'))THEN
        ICOL=ICOL+1
        ITITL2(1,ICOL)='Upper'
        NCTIT2(1,ICOL)=5
        ITITL2(2,ICOL)='Limit'
        NCTIT2(2,ICOL)=5
      ENDIF
C
      NUMLIN=2
      NUMCOL=ICOL
      NUMROW=NALP
      NMAX=0
      DO4221I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IDIGIT(I)=NUMDIG
        ITYPCO(I)='NUME'
        IWHTML(I)=150
        IF(I.EQ.1)THEN
          NTOT(I)=12
          IDIGIT(I)=1
          IWHTML(1)=75
          IWRTF(1)=2000
        ELSE
          IWRTF(I)=IWRTF(I-1)+2000
        ENDIF
        NMAX=NMAX+NTOT(I)
 4221 CONTINUE
C
      DO4223I=1,NUMROW
        AMAT(I,1)=CONF(I)
        AMAT(I,2)=YSD
        JCNT=2
        IF(ICASA4.EQ.'TWOS' .OR.
     1    (ICASA4.EQ.'ONES' .AND. ICASA2.EQ.'LOWE'))THEN
          JCNT=JCNT+1
          AMAT(I,JCNT)=ALOWLM(I)
        ENDIF
        IF(ICASA4.EQ.'TWOS' .OR.
     1    (ICASA4.EQ.'ONES' .AND. ICASA2.EQ.'UPPE'))THEN
          JCNT=JCNT+1
          AMAT(I,JCNT)=AUPPLM(I)
        ENDIF
 4223 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
      ITITL9=' '
      ITITLE=' '
      NCTIT9=0
      NCTITL=0
C
      CALL DPDTA4(ITITL9,NCTIT9,
     1            ITITLE,NCTITL,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            ISUBRO,IBUGA3,IERROR)
C
      GOTO400
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SDC2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSDC2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPSDC3(Y,N,ICASAN,ICASA2,ICASA3,ICASA4,
     1                  YSD,
     1                  ALPHA,NALPHA,ALOWLM,AUPPLM,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES CONFIDENCE LIMITS FOR THE
C              STANDARD DEVIATION ASSUMING A NORMAL DISTRIBUTION
C
C              THE FOLLOWING CASES ARE SUPPORTED:
C
C                 LET A = LOWER SD CONFIDENCE LIMIT Y
C                 LET A = UPPER SD CONFIDENCE LIMIT Y
C                 LET A = ONE SIDED LOWER SD CONFIDENCE INTERVAL Y
C                 LET A = ONE SIDED UPPER SD CONFIDENCE INTERVAL Y
C
C              THE DATA CONSISTS OF N OBSERVATIONS IN Y.
C
C              FOR ALL OF THE CASES ABOVE, THERE IS A "SUMMARY" CASE
C              WHERE WE SPECIFY THE MEAN, STANDARD DEVIATION, AND SAMPLE
C              SIZE FOR THE FIRST SAMPLE.  FOR EXAMPLE,
C
C                 LET A = SUMMARY LOWER SD CONFIDENCE INTERVAL YMEAN YSD N
C
C              A TWO-SIDED CONFIDENCE INTERVAL FOR THE STANDARD
C              DEVIATION IS:
C
C              [s(lower),s(upper)] = [s*SQRT((n-1)/CHSPPF(1-alpha/2;n-1)),
C                                     s*SQRT((n-1)/CHSPPF(1-alpha/2;n-1))]
C
C     INPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR OF
C                               (UNSORTED OR SORTED) OBSERVATIONS.
C                    --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                               IN THE VECTOR Y.
C                    --ALPHA  = THE SINGLE PRECISION VECTOR OF CONFIDENCE
C                               LEVELS
C                      NALPHA = THE INTEGER NUMBER OF ALPHA VALUES
C     OUTPUT ARGUMENTS-ALOWLM = THE SINGLE PRECISION VECTOR OF LOWER LIMIT
C                               VALUES
C                     -AUPPLM = THE SINGLE PRECISION VECTOR OF UPPER LIMIT
C                               VALUES
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--HAHN AND MEEKER (1991), "STATISTICAL INTERVALS: A
C                 GUIDE FOR PRACTIONERS", WILEY, PP. 55-56.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     ORIGINAL VERSION--APRIL     2013. 
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION ALOWLM(*)
      DIMENSION AUPPLM(*)
      DIMENSION ALPHA(*)
C
      CHARACTER*4 ICASAN
      CHARACTER*4 ICASA2
      CHARACTER*4 ICASA3
      CHARACTER*4 ICASA4
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='SDC3'
      ISUBN2='    '
      IWRITE='OFF'
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SDC3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPSDC3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,ICASAN,ICASA2,ICASA3,ICASA4
   52   FORMAT('IBUGA3,ISUBRO,ICASAN,ICASA2,ICASA3,ICASA4 = ',
     1         5(A4,2X),A4)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,53)N,NALPHA,YSD,ALPHA(1)
   53   FORMAT('N,NALPHA,YSD,ALPHA(1) = ',2I8,2G15.7)
        CALL DPWRST('XXX','WRIT')
        IF(ICASA3.EQ.'RAW')THEN
          DO56I=1,N
            WRITE(ICOUT,57)I,Y(I)
   57       FORMAT('I,Y(I) = ',I8,G15.7)
            CALL DPWRST('XXX','WRIT')
   56     CONTINUE
        ENDIF
        DO76I=1,NALPHA
          WRITE(ICOUT,77)I,ALPHA(I)
   77     FORMAT('I,ALPHA(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   76   CONTINUE
      ENDIF
C
C               ********************************************
C               **  STEP 11--                             **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SDC3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR: STANDARD DEVIATION CONFIDENCE LIMITS--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,102)
  102   FORMAT('      THE NUMBER OF ORIGINAL OBSERVATIONS  IS LESS ',
     1         'THAN TWO.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,103)N
  103   FORMAT('      SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               ********************************************
C               **  STEP 21--                             **
C               **  CARRY OUT CALCULATIONS FOR PREDICTION **
C               **  LIMITS.                               **
C               ********************************************
C
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.AND.ISUBRO.EQ.'SDC3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     ICASAN:  LIMI     => CONFIDENCE LIMIT FOR SD
C     ICASA2:  LOWE     => LOWER LIMIT
C              UPPE     => UPPER LIMIT
C     ICASA3:  RAW      => RAW DATA IN Y1
C              SUMM     => SUMMARY DATA IN YMEAN AND YSD
C     ICASA4:  ONES     => ONE-SIDED LIMIT
C              TWOS     => TWO-SIDED LIMIT
C
C     COMPUTE STANDARD DEVIATION
C
      DO210I=1,NALPHA
        ALOWLM(I)=CPUMIN
        AUPPLM(I)=CPUMIN
  210 CONTINUE
C
      IF(ICASA3.EQ.'RAW')THEN
        CALL SD(Y,N,IWRITE,YSD,IBUGA3,IERROR)
      ENDIF
C
      IF(YSD.LE.0.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,212)
  212   FORMAT('      THE STANDARD DEVIATION OF THE ORIGINAL ',
     1         'OBSERVATIONS IS NON-POSITIVE.')
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      NU1=N-1
      ANU=REAL(NU1)
C
C     2016/07: ISSUE WITH CHSPPF WHEN DEGREES OF FREEDOM IS LARGE.
C              FOR NOW, TRUNCATE DEGREES OF FREEDOM AT 150,000.
C
      NU1TMP=NU1
      IF(NU1TMP.GT.150000)THEN
        NU1TMP=150000
        ANU=REAL(NU1TMP)
      ENDIF
C
      IF(ICASA4.EQ.'ONES')THEN
        DO460I=1,NALPHA
          ALPHAT=ALPHA(I)
          IF(ALPHAT.GE.1.0 .AND. ALPHAT.LT.100.0)ALPHAT=ALPHAT/100.
          IF(ALPHAT.LE.0.0 .OR. ALPHAT.GE.1.0)GOTO8000
          IF(ALPHAT.GT.0.5)ALPHAT=1.0 - ALPHAT
          CALL CHSPPF(ALPHAT,NU1TMP,PPF)
          AUPPLM(I)=YSD*SQRT(ANU/PPF)
          ALPHAT=1.0 - ALPHAT
          CALL CHSPPF(ALPHAT,NU1TMP,PPF)
          ALOWLM(I)=YSD*SQRT(ANU/PPF)
  460   CONTINUE
      ELSEIF(ICASA4.EQ.'TWOS')THEN
        DO465I=1,NALPHA
          ALPHAT=ALPHA(I)
          IF(ALPHAT.GE.1.0 .AND. ALPHAT.LT.100.0)ALPHAT=ALPHAT/100.
          IF(ALPHAT.LE.0.0 .OR. ALPHAT.GE.1.0)GOTO8000
          IF(ALPHAT.GT.0.5)ALPHAT=1.0 - ALPHAT
          ALPHAT=ALPHAT/2.0
          CALL CHSPPF(ALPHAT,NU1TMP,PPF)
          AUPPLM(I)=YSD*SQRT(ANU/PPF)
          ALPHAT=1.0 - ALPHAT
          CALL CHSPPF(ALPHAT,NU1TMP,PPF)
          ALOWLM(I)=YSD*SQRT(ANU/PPF)
  465   CONTINUE
      ENDIF
C
      GOTO9000
C
 8000 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,101)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,8001)I
 8001 FORMAT('      ROW ',I8,' OF ALPHA VALUES IS OUT OF RANGE.')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,8003)ALPHA(I)
 8003 FORMAT('      THE VALUE OF ALPHA IS ',G15.7)
      CALL DPWRST('XXX','WRIT')
      IERROR='YES'
      GOTO9000
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SDC3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9051)
 9051   FORMAT('**** AT THE END OF DPSDC3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9052)YSD,PPF,ALPHA(NALPHA),ALPHAT,ANU,PPF
 9052   FORMAT('YSD,PPF,ALPHA(NALPHA),ALPHAT,ANU,PPF = ',6G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END 
      SUBROUTINE DPSDF(IHARG,NUMARG,ISDFSW,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE 3-D SIDEFACE SWITCH ISDFSW.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG
C     OUTPUT ARGUMENTS--ISDFSW   ('ON'  OR 'OFF')
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     NOTE--THIS SUBROUTINE ASSUMES A
C           COMPLICATED-TO-SIMPLE CHECKING ORDER
C           (IN MAIPC4) OF THE VARIOUS SIDEFACE COMMANDS.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--88/10
C     ORIGINAL VERSION--SEPTEMBER 1988.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 ISDFSW
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
      IF(NUMARG.EQ.0)GOTO1150
C
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1160
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1160
      GOTO1199
C
 1150 CONTINUE
      ISDFSW='ON'
      GOTO1180
C
 1160 CONTINUE
      ISDFSW='OFF'
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)ISDFSW
 1181 FORMAT('THE (3-D) SIDEFACE SWITCH ',
     1'HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPSDGC(IHARG,NUMARG,IDSDGC,ISDFGC,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE COLOR FOR THE 3-D SIDEFACE GRID.
C              THE COLOR FOR THE SIDEFACE GRID WILL BE PLACED
C              IN THE CHARACTER VARIABLE ISDFGC.
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --NUMARG
C                     --IDSDGC
C     OUTPUT ARGUMENTS--ISDFGC
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     NOTE--THIS SUBROUTINE ASSUMES A
C           COMPLICATED-TO-SIMPLE CHECKING ORDER
C           (IN MAIPC4) OF THE VARIOUS SIDEFACE COMMANDS.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 WASHINGPON, D. C. 20234
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--88/10
C     ORIGINAL VERSION--SEPTEMBER 1988.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IDSDGC
      CHARACTER*4 ISDFGC
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
      IF(NUMARG.LE.1)GOTO1199
      IF(NUMARG.EQ.2)GOTO1150
C
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      GOTO1160
C
 1150 CONTINUE
      ISDFGC=IDSDGC
      GOTO1180
C
 1160 CONTINUE
      ISDFGC=IHARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)ISDFGC
 1181 FORMAT('THE (3-D) SIDEFACE GRID COLOR ',
     1'HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPSDGP(IHARG,NUMARG,IDSDGP,ISDFGP,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE PATTERN FOR THE 3-D SIDEFACE GRID.
C              THE PATTERN FOR THE SIDEFACE GRID WILL BE PLACED
C              IN THE CHARACTER VARIABLE ISDFGP.
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --NUMARG
C                     --IDSDGP
C     OUTPUT ARGUMENTS--ISDFGP
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     NOTE--THIS SUBROUTINE ASSUMES A
C           COMPLICATED-TO-SIMPLE CHECKING ORDER
C           (IN MAIPC4) OF THE VARIOUS SIDEFACE COMMANDS.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 WASHINGPON, D. C. 20234
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--88/10
C     ORIGINAL VERSION--SEPTEMBER 1988.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IDSDGP
      CHARACTER*4 ISDFGP
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
      IF(NUMARG.LE.1)GOTO1199
      IF(NUMARG.EQ.2)GOTO1160
C
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1160
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1170
      GOTO1175
C
 1150 CONTINUE
      ISDFGP='SOLI'
      GOTO1180
C
 1160 CONTINUE
      ISDFGP='BLAN'
      GOTO1180
C
 1170 CONTINUE
      ISDFGP=IDSDGP
      GOTO1180
C
 1175 CONTINUE
      ISDFGP=IHARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)ISDFGP
 1181 FORMAT('THE (3-D) SIDEFACE GRID PATTERN ',
     1'HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPSDGR(IHARG,NUMARG,IDSDGR,ISDFGR,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE 3-D SIDEFACE GRID SWITCH ISDFGR.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG
C                     --IDSDGR
C     OUTPUT ARGUMENTS--ISDFGR   ('ON'  OR 'OFF')
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     NOTE--THIS SUBROUTINE ASSUMES A
C           COMPLICATED-TO-SIMPLE CHECKING ORDER
C           (IN MAIPC4) OF THE VARIOUS SIDEFACE COMMANDS.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--88/10
C     ORIGINAL VERSION--SEPTEMBER 1988.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IDSDGR
      CHARACTER*4 ISDFGR
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
      IF(NUMARG.EQ.0)GOTO1199
      IF(NUMARG.EQ.1)GOTO1150
C
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1160
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1170
      GOTO1199
C
 1150 CONTINUE
      ISDFGR='ON'
      GOTO1180
C
 1160 CONTINUE
      ISDFGR='OFF'
      GOTO1180
C
 1170 CONTINUE
      ISDFGR=IDSDGR
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)ISDFGR
 1181 FORMAT('THE (3-D) SIDEFACE GRID SWITCH ',
     1'HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPSDPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                  IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--GENERATE A SPATIAL DISTRIBUTION PLOT.
C
C              GIVEN A RECTANGULAR ARRAY OF POINTS, THERE
C              ARE 3 COMMON PATTERNS OF CLUSTERING:
C
C              1) UNIFORM -
C
C                 MODEL WITH A DISCRETE UNIFORM DISTRIBUTION
C
C              2) XX -
C
C                 MODEL WITH A POISSON DISTRIBUTION
C
C              3) XX -
C
C                 MODEL WITH A NEGATIVE BINOMIAL DISTRIBUTION
C
C              IT IS ASSUMED THAT EACH POINT IS EITHER ON OR
C              OFF (I.E., <0/1>).  IF THE RESPONSE DATA IS
C              A GREY-SCALE VALUE, POINTS ABOVE SOME
C              USER-SPECIFIED THRESHOLD VALUE ARE CONSIDERED
C              "ON" AND THOSE BELOW THE THRESHOLD ARE
C              CONSIDERED OFF.
C
C              THE POINT OF THIS PLOT IS TO SEE WHICH OF
C              THE THREE ABOVE DISTRIBUTIONS BEST FITS THE
C              DATA AT VARIOUS PARTITION SIZES.
C
C              THAT IS, WE PICK A PARTITION SIZE.  FOR EXAMPLE,
C              FOR A 512x512 ARRAY, WE MIGHT START WITH A
C              PARTITION CONSISTING OF 8x8 SQUARES.  WITHIN EACH
C              SQUARE, WE SUM THE NUMBER OF "1's".  WE THEN
C              MODEL THE DISTRIBUTION OF THESE SUMS.  SPECIFICALLY,
C
C                1) FOR THE DISCRETE UNIFORM, GENERATE A
C                   PROBABILITY PLOT.
C
C                2) FOR THE POISSON DISTRIBUTION, GENERATE A
C                   "POISSONESS" PLOT.
C
C                3) FOR THE NEGATIVE BINOMIAL, GENERATE A
C                   "NEGATIVE BINOMIALNESS" PLOT.
C
C              IN EACH CASE, THE LINEARITY OF THE PLOT IS AN
C              INDICATION OF GOODNESS OF FIT.  WE WILL USE THE
C              CORRELATION COEFFICIENT AS THE MEASURE OF GOODNESS OF
C              FIT.  THE SPATIAL DISTRIBUTION PLOT THEN CONSISTS OF:
C
C                 X-AXIS - SIZE OF PARTITION
C                 Y-AXIS - CORRELATION COEFFICIENT FOR EACH OF
C                          THE THREE DISTRIBUTIONS
C
C
C     EXAMPLES--SPATIAL DISTRIBUTION PLOT M
C               SPATIAL DISTRIBUTION PLOT M PART
C               SPATIAL DISTRIBUTION PLOT Y ROWID COLID
C               SPATIAL DISTRIBUTION PLOT Y ROWID COLID PART
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2008/4
C     ORIGINAL VERSION--APRIL     2008.
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
      CHARACTER*4 ICASE
      CHARACTER*4 IH11
      CHARACTER*4 IH12
      CHARACTER*4 IH21
      CHARACTER*4 IH22
      CHARACTER*4 IH31
      CHARACTER*4 IH32
      CHARACTER*4 IH41
      CHARACTER*4 IH42
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 IHP
      CHARACTER*4 IHP2
      CHARACTER*4 IUSE1
      CHARACTER*4 IUSE2
      CHARACTER*4 IUSE3
      CHARACTER*4 ICASEQ
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOCP.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
C
C---------------------------------------------------------------------
C
      DIMENSION YRESP(MAXOBV)
      DIMENSION PART(MAXOBV)
      DIMENSION ROWID(MAXOBV)
      DIMENSION COLID(MAXOBV)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TEMP3(MAXOBV)
      DIMENSION TEMP4(MAXOBV)
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(IGARB1),YRESP(1))
      EQUIVALENCE (GARBAG(IGARB2),PART(1))
      EQUIVALENCE (GARBAG(IGARB3),ROWID(1))
      EQUIVALENCE (GARBAG(IGARB4),COLID(1))
      EQUIVALENCE (GARBAG(IGARB5),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB6),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB7),TEMP3(1))
      EQUIVALENCE (GARBAG(IGARB8),TEMP4(1))
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPSD'
      ISUBN2='PL  '
      ICASE='VARI'
      ICASPL='SDPL'
      IFOUND='NO'
      IERROR='NO'
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
      MINN2=16
      ICOL4=0
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SDPL')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPSDPL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)NPLOTV,NPLOTP,NS
   52   FORMAT('NPLOTV,NPLOTP,NS = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,IAND1,IAND2
   53   FORMAT('ICASPL,IAND1,IAND2 = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,56)ICASPL,MAXN
   56   FORMAT('ICASPL,MAXN = ',A4,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,57)IFOUND,IERROR
   57   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,58)MAXNPP
   58   FORMAT('MAXNPP = ',I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               **************************************************
C               **  TREAT THE SPATIAL DISTRIBUTION PLOT CASE    **
C               **************************************************
C
      IFOUND='YES'
      ICASPL='SDPL'
C
C               *******************************************************
C               **  STEP 10--                                        **
C               **  CHECK FOR THE PROPER NUMBER OF INPUT ARGUMENTS.  **
C               *******************************************************
C
      ISTEPN='10'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SDPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      MINNA=1
      MAXNA=100
      CALL CHECKA(NUMARG,MINNA,MAXNA,IANS,IWIDTH,ISUBN1,ISUBN2,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
C               *****************************************
C               **  STEP 11--                          **
C               **  CHECK THE VALIDITY OF ARGUMENT 1   **
C               **  (THIS SHULD BE EITHER A VARIABLE   **
C               **  OR A MATRIX.                       **
C               **                                     **
C               **  IF A VARIABLE, THEN 3 OR 4         **
C               **  INPUT VARIABLES ARE EXPECTED.  IF  **
C               **  IF A MATRIX, THEN ONE MATRIX       **
C               **  EXPECTED AND ONE OPTIONAL VARIABLE.**
C               *****************************************
C
      ISTEPN='11'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SDPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IH11=IHARG(1)
      IH12=IHARG2(1)
      IHWUSE='V'
      MESSAG='YES'
C
      DO1100I=1,NUMNAM
        I2=I
        IF(IH11.EQ.IHNAME(I).AND.IH12.EQ.IHNAM2(I).AND.
     1     (IUSE(I).EQ.'P'.OR.IUSE(I).EQ.'F'))THEN
           GOTO1109
        ELSEIF(IH11.EQ.IHNAME(I).AND.IH12.EQ.IHNAM2(I).AND.
     1     IUSE(I).EQ.'V')THEN
           ICASE='VARI'
           GOTO3000
        ELSEIF(IH11.EQ.IHNAME(I).AND.IH12.EQ.IHNAM2(I).AND.
     1     IUSE(I).EQ.'M')THEN
           ICASE='MATR'
           ILISR=I2
           ICOL1=IVALUE(ILISR)
           ICOL2=IVALU2(ILISR)
           N1=IN(ILISR)
           NCOL=(ICOL2 - ICOL1) + 1
           GOTO5000
        ENDIF
 1100 CONTINUE
      GOTO1109
C
 1109 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1191)
 1191 FORMAT('***** ERROR IN SPATIAL DISTRIBUTION PLOT--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1192)
 1192 FORMAT('      THE FIRST ARGUMENT WAS EITHER NOT FOUND OR WAS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1193)
 1193 FORMAT('      FOUND AS A PARAMETER, SCALAR OR FUNCTION (AS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1194)
 1194 FORMAT('      OPPOSSED TO A VARIABLE OR A MATRIX).')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1196)
 1196 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,1197)(IANS(I),I=1,MIN(IWIDTH,80))
 1197   FORMAT(80A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GOTO9000
C
 3000 CONTINUE
C
C               ****************************************
C               **  STEP 30--                         **
C               **  CHECK THE VALIDITY OF ARGUMENT 1  **
C               **  (THIS SHOULD BE A VARIABLE.)      **
C               ****************************************
C
      ISTEPN='30'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SDPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IH11=IHARG(1)
      IH12=IHARG2(1)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IH11,IH12,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
C
      IF(IERROR.EQ.'YES')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3011)
 3011   FORMAT('***** ERROR IN SPATIAL DISTRIBUTION PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3012)
 3012   FORMAT('      FOR THE SPATIAL DISTRIBUTION PLOT, ALL ',
     1         'ARGUMENTS MUST')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3015)
 3015   FORMAT('      BE VARIABLES (AS OPPOSSED TO A PARAMETER OR A')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3016)
 3016   FORMAT('      FUNCTION).  ARGUMENT ONE WAS NOT A VARIABLE ',
     1         'HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3018)
 3018   FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,3019)(IANS(I),I=1,MIN(IWIDTH,80))
 3019     FORMAT(80A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IUSE1=IUSE(ILOCV)
      ICOL1=IVALUE(ILOCV)
      N1=IN(ILOCV)
C
      ICASE='VARI'
C
C               ******************************************************
C               **  STEP 31--                                       **
C               **  IF ARGUMENT ONE IS A VARIABLE, CHECK THAT THE   **
C               **  INPUT NUMBER OF OBSERVATIONS (N1) FOR ARGUMENT  **
C               **  ONE IS TWO OR MORE.                             **
C               ******************************************************
C
      ISTEPN='31'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SDPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N1.LT.MINN2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3011)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3022)
 3022   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS FOR THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3023)MINN2
 3023   FORMAT('      SPATIAL DISTRIBUTION PLOT MUST BE ',I8,
     1         ' OR LARGER.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3025)
 3025   FORMAT('      SUCH WAS NOT THE CASE HERE;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3027)IH11,IH12,N1
 3027   FORMAT('      VARIABLE ',A4,A4,' HAS ',I8,' OBSERVATIONS.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3018)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,3019)(IANS(I),I=1,MIN(IWIDTH,80))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               ****************************************
C               **  STEP 32--                         **
C               **  CHECK THE VALIDITY OF ARGUMENT 2  **
C               **  (THIS SHOULD BE A VARIABLE)       **
C               ****************************************
C
      ISTEPN='31B'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SDPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IH21=IHARG(2)
      IH22=IHARG2(2)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IH21,IH22,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
C
      IF(IERROR.EQ.'YES')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3011)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3112)
 3112   FORMAT('      FOR THE SPATIAL DISTRIBUTION PLOT, WHEN THE ',
     1         'FIRST ARGUMENT IS A VARIABLE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3115)
 3115   FORMAT('      THERE MUST BE AT LEAST THREE VARIABLES ',
     1         'ENTERED.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3116)
 3116   FORMAT('      ONLY ONE VARIABLE WAS GIVEN.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3118)
 3118   FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,3019)(IANS(I),I=1,MIN(IWIDTH,80))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IUSE2=IUSE(ILOCV)
      ICOL2=IVALUE(ILOCV)
      N2=IN(ILOCV)
      NVAR=2
C
      IF(N2.LT.N1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3011)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3122)
 3122   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE SECOND')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3123)
 3123   FORMAT('      VARIABLE IS NOT EQUAL TO THE NUMBER OF ',
     1         'OBSERVATIONS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3125)
 3125   FORMAT('      FOR THE FIRST VARIABLE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3127)IH11,IH12,N1
 3127   FORMAT('      VARIABLE ',A4,A4,' HAS ',I8,' OBSERVATIONS.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3027)IH21,IH22,N2
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3018)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,3019)(IANS(I),I=1,MIN(IWIDTH,80))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               ****************************************
C               **  STEP 32--                         **
C               **  CHECK THE VALIDITY OF ARGUMENT 3  **
C               **  (THIS SHOULD BE A VARIABLE)       **
C               ****************************************
C
      ISTEPN='32'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SDPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IH31=IHARG(3)
      IH32=IHARG2(3)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IH31,IH32,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
C
      IF(IERROR.EQ.'YES')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3011)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3212)
 3212   FORMAT('      FOR THE SPATIAL DISTRIBUTION PLOT, WHEN THE ',
     1         'FIRST ARGUMENT IS A VARIABLE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3115)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3216)
 3216   FORMAT('      ONLY TWO VARIABLE WERE GIVEN.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3118)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,3019)(IANS(I),I=1,MIN(IWIDTH,80))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
C
      IUSE3=IUSE(ILOCV)
      ICOL3=IVALUE(ILOCV)
      N3=IN(ILOCV)
      NVAR=3
C
C               ******************************************************
C               **  STEP 32B--                                      **
C               **  IF ARGUMENT THREE IS A VARIABLE, CHECK THAT THE **
C               **  INPUT NUMBER OF OBSERVATIONS (N3) FOR ARGUMENT  **
C               **  THREE IS EQUAL TO THE NUMBER OF OBSERVATIONS    **
C               **  FOR VARIABLE ONE.                               **
C               ******************************************************
C
      ISTEPN='32B'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SDPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N3.NE.N1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3011)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3222)
 3222   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE THIRD')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3123)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3125)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3027)IH11,IH12,N1
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3027)IH31,IH32,N3
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3018)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,3019)(IANS(I),I=1,MIN(IWIDTH,80))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               ****************************************
C               **  STEP 33--                         **
C               **  CHECK THE VALIDITY OF ARGUMENT 4  **
C               **  (THIS SHOULD BE A VARIABLE IF IT  **
C               **  EXISTS)                           **
C               ****************************************
C
      ISTEPN='33'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SDPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IH41=IHARG(4)
      IH42=IHARG2(4)
      IHWUSE='V'
      MESSAG='NO'
      CALL CHECKN(IH41,IH42,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
C
      IF(IERROR.EQ.'YES')THEN
         IERROR='NO'
         N4=0
         GOTO3999
      ELSE
        IUSE3=IUSE(ILOCV)
        ICOL4=IVALUE(ILOCV)
        N4=IN(ILOCV)
        NVAR=4
      ENDIF
C
      NPART=N4
C
C               ******************************************************
C               **  STEP 33B-                                       **
C               **  IF ARGUMENT FOUR  IS A VARIABLE, THIS DENOTES   **
C               **  THE "PARTITION" VALUES, SO THE NUMBER OF        **
C               **  OBSERVATIONS NEED NOT MATCH THE NUMBER OF       **
C               **  OBSERVATIONS FOR VARIABLE ONE.                  **
C               ******************************************************
C
C               *****************************************
C               **  STEP 40--                          **
C               **  CHECK TO SEE THE TYPE CASE--       **
C               **    1) UNQUALIFIED (THAT IS, FULL);  **
C               **    2) SUBSET/EXCEPT; OR             **
C               **    3) FOR.                          **
C               *****************************************
C
 3999 CONTINUE
C
      ISTEPN='40'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SDPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASEQ='FULL'
      ILOCQ=NUMARG+1
      IF(NUMARG.LT.1)GOTO4090
      DO4000J=1,NUMARG
      J1=J
      IF(IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ') GOTO4010
      IF(IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  ') GOTO4010
      IF(IHARG(J).EQ.'FOR '.AND.IHARG2(J).EQ.'    ') GOTO4020
 4000 CONTINUE
      GOTO4090
 4010 CONTINUE
      ICASEQ='SUBS'
      ILOCQ=J1
      GOTO4090
 4020 CONTINUE
      ICASEQ='FOR'
      ILOCQ=J1
      GOTO4090
 4090 CONTINUE
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SDPL')THEN
        WRITE(ICOUT,4091)NUMARG,ILOCQ
 4091   FORMAT('NUMARG,ILOCQ = ',2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***********************************************
C               **  STEP 41--                                **
C               **  TEMPORARILY FORM THE VARIABLE Y(.)       **
C               **  WHICH WILL HOLD THE DATA  FROM SAMPLE 1. **
C               **  FORM THIS VARIABLE BY                    **
C               **  BRANCHING TO THE APPROPRIATE SUBCASE     **
C               **  (FULL, SUBSET, OR FOR).                  **
C               ***********************************************
C
      ISTEPN='41'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SDPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASEQ.EQ.'FULL')GOTO4110
      IF(ICASEQ.EQ.'SUBS')GOTO4120
      IF(ICASEQ.EQ.'FOR')GOTO4130
C
 4110 CONTINUE
      DO4115I=1,N1
      ISUB(I)=1
 4115 CONTINUE
      NQ=N1
      GOTO4150
C
 4120 CONTINUE
      NIOLD=N1
      CALL DPSUBS(NIOLD,ILOCS,NS,IBUGQ,IERROR)
      NQ=NIOLD
      GOTO4150
C
 4130 CONTINUE
      NIOLD=N1
      CALL DPFOR(NIOLD,NFOR,IROW1,IROWN,
     1NLOCAL,ILOCS,NS,IBUGQ,IERROR)
      NQ=NFOR
      GOTO4150
C
 4150 CONTINUE
      IF(NQ.LT.MINN2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3011)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4152)
 4152   FORMAT('      AFTER THE APPROPRIATE SUBSET HAS BEEN ',
     1         'EXTRACTED,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4153)IH11,IH12
 4153   FORMAT('      THE NUMBER OF OBSERVATIONS REMAINING FROM ',
     1         'VARIABLE ',A4,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4154)
 4154   FORMAT('      (FOR WHICH THE SPATIAL DISTRIBUTION PLOT ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4155)MINN2
 4155   FORMAT('      IS TO BE CARRIED OUT) MUST BE AT LEAST ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4157)NQ
 4157   FORMAT('      SUCH WAS NOT THE CASE HERE.  (N = ',I8,')')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3018)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,3019)(IANS(I),I=1,MIN(IWIDTH,80))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
      J=0
      IMAX=N1
      IF(NQ.LT.N1)IMAX=NQ
      DO4170I=1,IMAX
        IF(ISUB(I).EQ.0)GOTO4170
        J=J+1
C
        IJ=MAXN*(ICOL1-1)+I
        IF(ICOL1.LE.MAXCOL)YRESP(J)=V(IJ)
        IF(ICOL1.EQ.MAXCP1)YRESP(J)=PRED(I)
        IF(ICOL1.EQ.MAXCP2)YRESP(J)=RES(I)
        IF(ICOL1.EQ.MAXCP3)YRESP(J)=YPLOT(I)
        IF(ICOL1.EQ.MAXCP4)YRESP(J)=XPLOT(I)
        IF(ICOL1.EQ.MAXCP5)YRESP(J)=X2PLOT(I)
        IF(ICOL1.EQ.MAXCP6)YRESP(J)=TAGPLO(I)
C
        IJ=MAXN*(ICOL2-1)+I
        IF(ICOL2.LE.MAXCOL)ROWID(J)=V(IJ)
        IF(ICOL2.EQ.MAXCP1)ROWID(J)=PRED(I)
        IF(ICOL2.EQ.MAXCP2)ROWID(J)=RES(I)
        IF(ICOL2.EQ.MAXCP3)ROWID(J)=YPLOT(I)
        IF(ICOL2.EQ.MAXCP4)ROWID(J)=XPLOT(I)
        IF(ICOL2.EQ.MAXCP5)ROWID(J)=X2PLOT(I)
        IF(ICOL2.EQ.MAXCP6)ROWID(J)=TAGPLO(I)
C
        IJ=MAXN*(ICOL3-1)+I
        IF(ICOL3.LE.MAXCOL)COLID(J)=V(IJ)
        IF(ICOL3.EQ.MAXCP1)COLID(J)=PRED(I)
        IF(ICOL3.EQ.MAXCP2)COLID(J)=RES(I)
        IF(ICOL3.EQ.MAXCP3)COLID(J)=YPLOT(I)
        IF(ICOL3.EQ.MAXCP4)COLID(J)=XPLOT(I)
        IF(ICOL3.EQ.MAXCP5)COLID(J)=X2PLOT(I)
        IF(ICOL3.EQ.MAXCP6)COLID(J)=TAGPLO(I)
C
 4170 CONTINUE
      NS=J
C
      IF(NPART.GT.0)THEN
        DO4180I=1,NPART
          IJ=MAXN*(ICOL4-1)+I
          IF(ICOL4.LE.MAXCOL)PART(I)=V(IJ)
          IF(ICOL4.EQ.MAXCP1)PART(I)=PRED(I)
          IF(ICOL4.EQ.MAXCP2)PART(I)=RES(I)
          IF(ICOL4.EQ.MAXCP3)PART(I)=YPLOT(I)
          IF(ICOL4.EQ.MAXCP4)PART(I)=XPLOT(I)
          IF(ICOL4.EQ.MAXCP5)PART(I)=X2PLOT(I)
          IF(ICOL4.EQ.MAXCP6)PART(I)=TAGPLO(I)
 4180   CONTINUE
      ENDIF
C
      GOTO6000
C
 5000 CONTINUE
C
C
C               ******************************************************
C               **  STEP 51--                                       **
C               **  IF ARGUMENT ONE IS A MATRIX, CHECK THAT THE     **
C               **  INPUT NUMBER OF OBSERVATIONS (N1) FOR ARGUMENT  **
C               **  ONE IS 16  OR MORE.                             **
C               ******************************************************
C
      ISTEPN='51'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SDPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N1.LT.MINN2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3011)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5022)
 5022   FORMAT('      THE INPUT NUMBER OF ROWS FOR THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5023)MINN2
 5023   FORMAT('      SPATIAL DISTRIBUTION PLOT MUST BE ',I8,
     1         ' OR LARGER.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5025)
 5025   FORMAT('      SUCH WAS NOT THE CASE HERE;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5027)IH11,IH12,N1
 5027   FORMAT('      MATRIX ',A4,A4,' HAS ',I8,' ROWS.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5018)
 5018   FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,5019)(IANS(I),I=1,MIN(IWIDTH,80))
 5019     FORMAT(80A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
C
C               ********************************************************
C               **  STEP 52--                                         **
C               **  CHECK IF ARGUMENT TWO IS A VARIABLE (IF IT EXISTS)**
C               ********************************************************
C
      ISTEPN='52'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SDPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IH21=IHARG(2)
      IH22=IHARG2(2)
      IHWUSE='V'
      MESSAG='NO'
      CALL CHECKN(IH21,IH22,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
C
      IF(IERROR.EQ.'YES')THEN
        NPART=0
      ELSE
        ILISR=ILOCV
        ICOL21=IVALUE(ILISR)
        NPART=IN(ILISR)
      ENDIF
C
C               *****************************************
C               **  STEP 56--                          **
C               **  CHECK TO SEE THE TYPE CASE--       **
C               **    1) UNQUALIFIED (THAT IS, FULL);  **
C               **    2) SUBSET/EXCEPT; OR             **
C               **    3) FOR.                          **
C               *****************************************
C
      ISTEPN='56'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SDPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASEQ='FULL'
      ILOCQ=NUMARG+1
      IF(NUMARG.LT.1)GOTO5609
      DO5600J=1,NUMARG
      J1=J
      IF(IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ') GOTO5601
      IF(IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  ') GOTO5601
      IF(IHARG(J).EQ.'FOR '.AND.IHARG2(J).EQ.'    ') GOTO5602
 5600 CONTINUE
      GOTO5609
 5601 CONTINUE
      ICASEQ='SUBS'
      ILOCQ=J1
      GOTO5609
 5602 CONTINUE
      ICASEQ='FOR'
      ILOCQ=J1
      GOTO5609
 5609 CONTINUE
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SDPL')THEN
        WRITE(ICOUT,5038)NUMARG,ILOCQ
 5038   FORMAT('NUMARG,ILOCQ = ',2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***********************************************
C               **  STEP 56B--                               **
C               **  TEMPORARILY FORM THE VARIABLE Y(.)       **
C               **  WHICH WILL HOLD THE DATA  FROM SAMPLE 1. **
C               **  FORM THIS VARIABLE BY                    **
C               **  BRANCHING TO THE APPROPRIATE SUBCASE     **
C               **  (FULL, SUBSET, OR FOR).                  **
C               ***********************************************
C
      ISTEPN='56B'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SDPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASEQ.EQ.'FULL')GOTO5610
      IF(ICASEQ.EQ.'SUBS')GOTO5620
      IF(ICASEQ.EQ.'FOR')GOTO5630
C
 5610 CONTINUE
      DO5615I=1,N1
      ISUB(I)=1
 5615 CONTINUE
      NQ=N1
      GOTO5650
C
 5620 CONTINUE
      NIOLD=N1
      CALL DPSUBS(NIOLD,ILOCS,NS,IBUGQ,IERROR)
      NQ=NIOLD
      GOTO5650
C
 5630 CONTINUE
      NIOLD=N1
      CALL DPFOR(NIOLD,NFOR,IROW1,IROWN,
     1NLOCAL,ILOCS,NS,IBUGQ,IERROR)
      NQ=NFOR
      GOTO5650
C
 5650 CONTINUE
      IF(NQ.LT.MINN2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5651)
 5651   FORMAT('***** ERROR IN THE SPATIAL DISTRIBUTION PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5652)
 5652   FORMAT('      AFTER THE APPROPRIATE SUBSET HAS BEEN ',
     1         'EXTRACTED,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5653)IH11,IH12
 5653   FORMAT('      THE NUMBER OF ROWS REMAINING FROM MATRIX ',
     1         A4,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5654)
 5654   FORMAT('      (FOR WHICH THE SPATIAL DISTRIBUTION PLOT IS ',
     1         'TO BE CARRIED')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5655)MINN2
 5655   FORMAT('      OUT) MUST BE AT LEAST ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5657)NQ
 5657   FORMAT('      SUCH WAS NOT THE CASE HERE.  (NROWS = ',I8,')')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3018)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,5019)(IANS(I),I=1,MIN(IWIDTH,80))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
      ICASE='MATR'
C
      NLOOP=NCOL
      IF(NLOOP.LT.1)NLOOP=1
      IMAX=N1
      IF(NQ.LT.N1)IMAX=NQ
C
      NCOL=0
      J=0
      DO5671JLOOP=1,NLOOP
        NCOL=NCOL+1
        NROW=0
        DO5670I=1,IMAX
          IF(ISUB(I).EQ.0)GOTO5670
          NROW=NROW+1
          J=J+1
          ICOLT=ICOL1+JLOOP-1
          IJ=MAXN*(ICOLT-1)+I
C
          IF(ICOLT.LE.MAXCOL)YRESP(J)=V(IJ)
          IF(ICOLT.EQ.MAXCP1)YRESP(J)=PRED(I)
          IF(ICOLT.EQ.MAXCP2)YRESP(J)=RES(I)
          IF(ICOLT.EQ.MAXCP3)YRESP(J)=YPLOT(I)
          IF(ICOLT.EQ.MAXCP4)YRESP(J)=XPLOT(I)
          IF(ICOLT.EQ.MAXCP5)YRESP(J)=X2PLOT(I)
          IF(ICOLT.EQ.MAXCP6)YRESP(J)=TAGPLO(I)
          ROWID(J)=REAL(NROW)
          COLID(J)=REAL(NCOL)
C
 5670   CONTINUE
 5671 CONTINUE
C
      NS=J
C
      IF(NPART.GT.0)THEN
        DO5680I=1,NPART
          IJ=MAXN*(ICOL2-1)+I
          IF(ICOL2.LE.MAXCOL)PART(I)=V(IJ)
          IF(ICOL2.EQ.MAXCP1)PART(I)=PRED(I)
          IF(ICOL2.EQ.MAXCP2)PART(I)=RES(I)
          IF(ICOL2.EQ.MAXCP3)PART(I)=YPLOT(I)
          IF(ICOL2.EQ.MAXCP4)PART(I)=XPLOT(I)
          IF(ICOL2.EQ.MAXCP5)PART(I)=X2PLOT(I)
          IF(ICOL2.EQ.MAXCP6)PART(I)=TAGPLO(I)
 5680   CONTINUE
      ENDIF
C
      GOTO6000
C
 6000 CONTINUE
C
C               ********************************************************
C               **  STEP 61--                                          *
C               **  FORM THE VERTICAL AND HORIZONTAL AXIS VARIABLES    *
C               **  (Y(.) AND X(.), RESPECTIVELY) FOR THE PLOT.        *
C               **  FORM THE CURVE DESIGNATION VARIABLE D(.)  .        *
C               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).      *
C               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).      *
C               ********************************************************
C
      ISTEPN='61'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SDPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IHP ='THRE'
      IHP2='SHOL'
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IHP,IHP2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'YES')THEN
        THRESH=CPUMIN
      ELSE
        THRESH=VALUE(ILOCP)
      ENDIF
C
      CALL DPSDP2(YRESP,ROWID,COLID,NS,PART,NPART,
     1THRESH,
     1TEMP1,TEMP2,TEMP3,TEMP4,MAXOBV,
     1Y,X,D,NPLOTP,NPLOTV,
     1IBUGG3,ISUBRO,IERROR)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'SDPL')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSDPL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9013   FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',
     1         I8,I8,I8,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)ICASPL,MAXN
 9014   FORMAT('ICASPL,MAXN = ',A4,I8)
        CALL DPWRST('XXX','BUG ')
        IF(NPLOTP.GE.1)THEN
          DO9020I=1,MIN(NPLOTP,200)
            WRITE(ICOUT,9021)I,Y(I),X(I),D(I)
 9021       FORMAT('I,Y(I),X(I),D(I) = ',I8,3F12.5)
            CALL DPWRST('XXX','BUG ')
 9020     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPSDP2(Y,ROWID,COLID,N,PART,NPART,
     1THRESH,
     1TEMP1,TEMP2,TEMP3,TEMP4,MAXOBV,
     1Y2,X2,D2,NPLOTP,NPLOTV,
     1IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--FORM A SPATIAL DISTRIBUTION PLOT.
C     EXAMPLE--SPATIAL DISTRIBUTION PLOT Y ROWID COLID PART
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2008/4
C     ORIGINAL VERSION--APRIL     2008.
C
C-----COMMON----------------------------------------------------------
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 IWRITE
      CHARACTER*4 IRELAT
      CHARACTER*4 IRHSTG
C
      DIMENSION Y(*)
      DIMENSION ROWID(*)
      DIMENSION COLID(*)
      DIMENSION PART(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
      DIMENSION TEMP4(*)
C
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DTERM3
      DOUBLE PRECISION DLNGAM
      DOUBLE PRECISION DBINLN
C
      EXTERNAL DLNGAM
      EXTERNAL DBINLN
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'SDP2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPSDP2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGG3,ISUBRO,N,NPART
   52   FORMAT('IBUGG3,ISUBRO,N,NPART = ',A4,2X,A4,2X,I8,2X,I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,MIN(N,100)
          WRITE(ICOUT,56)I,Y(I),ROWID(I),COLID(I)
   56     FORMAT('I,Y(I),ROWID(I),COLID(I) = ',I8,3G12.4)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
        IF(NPART.GT.0)THEN
          DO58I=1,MIN(NPART,100)
            WRITE(ICOUT,59)I,PART(I)
   59       FORMAT('I,PART(I) = ',I8,G12.4)
            CALL DPWRST('XXX','BUG ')
   58     CONTINUE
        ENDIF
      ENDIF
C
C               *******************************************************
C               **  STEP 1--                                         **
C               **  CHECK INPUT ARRAYS FOR ERRORS                    **
C               *******************************************************
C
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'SDP2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     STEP 1A: RESPONSE ARRAY SHOULD BE EITHER 0/1 OR
C              THERE SHOULD BE A USER-DEFINED THRESHOLD
C              TO CREATE 0/1 ARRAY.
C
      IWRITE='OFF'
      CALL DISTIN(Y,N,IWRITE,TEMP1,NDIST,IBUGG3,IERROR)
      IF(NDIST.EQ.2)THEN
        AVAL1=TEMP1(1)
        AVAL2=TEMP1(2)
        ALOW=MIN(AVAL1,AVAL2)
        AHIGH=MAX(AVAL1,AVAL2)
        DO110I=1,N
          IF(Y(I).EQ.ALOW)THEN
            Y(I)=0.0
          ELSE
            Y(I)=1.0
          ENDIF
  110   CONTINUE
      ELSE
        IF(THRESH.NE.CPUMIN)THEN
          DO210I=1,N
            IF(Y(I).LE.THRESH)THEN
              Y(I)=0.0
            ELSE
              Y(I)=1.0
            ENDIF
  210     CONTINUE
        ELSE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,260)
  260     FORMAT('***** ERROR IN SPATIAL DISTRIBUTION PLOT--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,261)
  261     FORMAT('      THERE ARE MORE THAN TWO DISTINCT VALUES FOUND')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,263)
  263     FORMAT('      IN THE RESPONSE VARIABLE, BUT NO THRESHOLD ',
     1           'WAS SPECIFIED.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ENDIF
C
C     STEP 1B: CHECK THAT THE NUMBER OR ROWS TIMES THE NUMBER OF
C              COLUMNS EQUALS THE NUMBER OF RESPONSE VALUES.  ALSO
C              CODE THE ROWID AND COLID TO 1, 2, ..., <NROW/NCOL>.
C
      CALL CODE(ROWID,N,IWRITE,TEMP1,TEMP2,MAXOBV,IBUGG3,IERROR)
      DO310I=1,N
        ROWID(I)=TEMP1(I)
  310 CONTINUE
      CALL DISTIN(ROWID,N,IWRITE,TEMP1,NROWS,IBUGG3,IERROR)
C
      CALL CODE(COLID,N,IWRITE,TEMP1,TEMP2,MAXOBV,IBUGG3,IERROR)
      DO320I=1,N
        COLID(I)=TEMP1(I)
  320 CONTINUE
      CALL DISTIN(COLID,N,IWRITE,TEMP1,NCOLS,IBUGG3,IERROR)
C
      IF(NROWS*NCOLS.NE.N)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,260)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,361)
  361   FORMAT('      THE NUMBER OF ROWS TIMES THE NUMBER OF ',
     1         'COLUMNS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,363)
  363   FORMAT('      WAS NOT EQUAL TO THE TOTAL NUMBER OF VALUES.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C     STEP 1C: DEFINE THE PARTITIONING.
C
C              FOR THIS PLOT TO MAKE SENSE, THERE MUST BE AT
C              LEAST TWO PARTITIONS, SO THE MINIMUM OF THE
C              NUMBER OF ROWS AND NUMBER OF COLUMNS MUST BE
C              AT LEAST 16.
C
      NMIN=MIN(NROWS,NCOLS)
      IF(NMIN.LT.16)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,260)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,401)
  401   FORMAT('      THE MINIMUM OF THE NUMBER OF ROWS AND THE ',
     1         'NUMBER OF COLUMNS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,403)
  403   FORMAT('      IS LESS THAN 16.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C     IF THE USER DID NOT SPECIFY A PARTITION, THEN
C     CREATE ONE.  THE MINIMUM PARTITION WILL BE 8x8
C     AND THE MAXIMUM PARTITION WILL BE N/8.
C
      IF(NPART.EQ.0)THEN
        NSTART=8
        NLAST=N/8
        DO510I=NSTART,NLAST
          PART(I)=REAL(I)
  510   CONTINUE
      ELSE
        CALL SORT(PART,NPART,PART)
        NLOW=4
        NHIGH=N/4
        ICNT=0
        DO520I=1,NPART
          NTEMP=INT(PART(I) + 0.01)
          IF(NTEMP.GE.NLOW .AND. NTEMP.LE.NHIGH)THEN
            ICNT=ICNT+1
            PART(ICNT)=REAL(NTEMP)
          ENDIF
  520   CONTINUE
        IF(ICNT.LT.2)THEN
          NSTART=8
          NLAST=N/8
          ICNT=0
          DO530I=NSTART,NLAST
            ICNT=ICNT+1
            PART(ICNT)=REAL(I)
  530     CONTINUE
          NPART=ICNT
        ELSE
          NPART=ICNT
        ENDIF
      ENDIF
C
C               *******************************************************
C               **  STEP 2--                                         **
C               **  NOW LOOP OVER THE PARTITIONS                     **
C               *******************************************************
C
      ISTEPN='2'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'SDP2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICNT=0
      ICNT2=0
      ICNT3=0
      ICNT4=0
C
      DO1000IPART=1,NPART
        APART=PART(IPART)
        ISIZE=INT(PART(INT(IPART + 0.01)))
        NHOR=NROWS/ISIZE
        NVERT=NCOLS/ISIZE
        ICNT2=0
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,1003)INT(APART+0.01)
 1003     FORMAT('PROCESSING PARTITION SIZE ',I8,' ...')
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        DO1010IROW=1,NHOR
          IROW1=(IROW-1)*ISIZE + 1
          IROW2=IROW*ISIZE
          DO1020ICOL=1,NVERT
            ICOL1=(ICOL-1)*ISIZE + 1
            ICOL2=ICOL*ISIZE
            SUM1=0.0
            ICNT=0
C
            DO1030I=1,N
              IROWC=INT(ROWID(I)+0.01)
              ICOLC=INT(COLID(I)+0.01)
              IF((IROWC.GE.IROW1 .AND. IROWC.LE.IROW2) .AND.
     1           (ICOLC.GE.ICOL1 .AND. ICOLC.LE.ICOL2))THEN
                ICNT=ICNT+1
                SUM1=SUM1 + Y(I)
              ENDIF
 1030       CONTINUE
            IF(ICNT.NE.ISIZE*ISIZE)THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,260)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1031)IROW,ICOL
 1031         FORMAT('      FOR PARTITION: ROW = ',I8,' COLUM = ',I8)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1033)ISIZE*ISIZE
 1033         FORMAT('      THE EXPECTED NUMBER OF VALUES = ',I8)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1035)ICNT
 1035         FORMAT('      THE NUMBER OF VALUES FOUND    = ',I8)
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GOTO9000
            ENDIF
            ICNT2=ICNT2+1
            TEMP1(ICNT2)=SUM1
C
 1020     CONTINUE
 1010   CONTINUE
C
C       NOW FIT DISCRETE UNIFORM, POISSON, AND NEGATIVE BINOMIAL
C       TO THE ARRAY OF SUMS.
C
C       DISCRETE UNIFORM PROBABILITY PLOT
C
        CALL MEAN(TEMP1,ICNT2,IWRITE,XMEAN,IBUGG3,IERROR)
        CALL SD(TEMP1,ICNT2,IWRITE,XSD,IBUGG3,IERROR)
        CALL SORT(TEMP1,ICNT2,TEMP1)
        XMIN=TEMP1(1)
        XMAX=TEMP1(ICNT2)
        NDUN=INT(XMAX+0.01)
        CALL UNIMED(ICNT2,TEMP2)
C
        DO2010I=1,ICNT2
          CALL DISPPF(TEMP2(I),NDUN,X2OUT)
          TEMP2(I)=X2OUT
 2010   CONTINUE
        CALL CORR(TEMP2,TEMP1,ICNT2,IWRITE,PPCC,IBUGG3,IERROR)
        ICNT4=ICNT4+1
        X2(ICNT4)=APART
        Y2(ICNT4)=PPCC
        D2(ICNT4)=1.0
C
C       BIN THE DATA AND REMOVE ZERO-FREQUENCY CLASSES
C
        IRELAT='OFF'
        IRHSTG='OFF'
        XMIN=TEMP1(1)
        XMAX=TEMP1(ICNT2)
        XSTART=XMIN-0.5
        XSTOP=XMAX+0.5
        CLWID=1.0
        CALL DPBINI(TEMP1,ICNT2,IRELAT,CLWID,XSTART,XSTOP,IRHSTG,
     1              TEMP4,TEMP3,N2,IBUGG3,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        NTOT=ICNT2
        ICNT3=0
        DO2101I=1,N2
          IF(INT(TEMP4(I)+0.01).GT.0)THEN
            ICNT3=ICNT3+1
            TEMP2(ICNT3)=TEMP4(I)
            TEMP1(ICNT3)=TEMP3(I)
          ENDIF
2101    CONTINUE
        N2=ICNT3
C
C       POISSON PLOT
C
        ICNT3=0
        DTERM1=DLOG(DBLE(NTOT))
        DO2200I=1,N2
          AK=TEMP1(I)
          IK=INT(AK+0.1)
          ANK=TEMP2(I)
          IF(ANK.GT.0.0)THEN
            ICNT3=ICNT3+1
            DTERM2=DLOG(DBLE(ANK))
            IF(IK.EQ.0 .OR. IK.EQ.1)THEN
              DTERM3=DLOG(1.0D0)
            ELSEIF(IK.EQ.2)THEN
              DTERM3=DLOG(2.0D0)
            ELSE
              DTERM3=DLNGAM(DBLE(AK+1.0))
            ENDIF
            TEMP4(ICNT3)=REAL(DTERM2 + DTERM3 - DTERM1)
            TEMP3(ICNT3)=AK
          ENDIF
C
 2200   CONTINUE
        NTEMP=ICNT3
        CALL CORR(TEMP4,TEMP3,NTEMP,IWRITE,PPCC,IBUGG3,IERROR)
        ICNT4=ICNT4+1
        X2(ICNT4)=APART
        Y2(ICNT4)=PPCC
        D2(ICNT4)=2.0
C
C       NEGATIVE BINOMIAL PLOT
C
        AKNB=XMEAN**2/(XSD**2 - XMEAN)
        ICNT3=0
        DTERM1=DLOG(DBLE(NTOT))
        DO2300I=1,N2
          AK=TEMP1(I)
          IK=INT(AK+0.1)
          ANK=TEMP2(I)
          INK=INT(ANK+0.1)
          IF(ANK.GT.0.0)THEN
            ICNT3=ICNT3+1
            DTERM2=DLOG(DBLE(ANK))
            ITEMP1=INT(AKNB+0.5)+IK-1
            ITEMP2=IK
            DTERM3=DBINLN(ITEMP1,ITEMP2)
            TEMP4(ICNT3)=REAL(DTERM2 - DTERM1 - DTERM3)
C
            TEMP3(ICNT3)=AK
          ENDIF
C
 2300   CONTINUE
        NTEMP=ICNT3
        CALL CORR(TEMP4,TEMP3,NTEMP,IWRITE,PPCC,IBUGG3,IERROR)
        ICNT4=ICNT4+1
        X2(ICNT4)=APART
        Y2(ICNT4)=PPCC
        D2(ICNT4)=3.0
C
 1000 CONTINUE
C
      NPLOTP=ICNT4
      NPLOTV=2
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'SDP2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSDP2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGG3,ISUBRO
 9012   FORMAT('IBUGG3,ISUBRO = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTP,NPLOTV
 9013   FORMAT('NPLOTP,NPLOTV = ',2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NPLOTP.GE.1)THEN
          DO9015I=1,MIN(200,NPLOTP)
            WRITE(ICOUT,9016)I,Y2(I),X2(I),D2(I)
 9016       FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,3F10.5)
            CALL DPWRST('XXX','BUG ')
 9015     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPSDR3(Y,N,ICASA2,ICASA4,MAXNXT,
     1                  TEMP1,AKURT,N0,IBONAD,
     1                  YSD,
     1                  ALPHA,NALPHA,ALOWLM,AUPPLM,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES CONFIDENCE LIMITS FOR THE
C              STANDARD DEVIATION ASSUMING A NON-NORMAL DISTRIBUTION
C
C              THE FOLLOWING CASES ARE SUPPORTED:
C
C                 LET A = LOWER ROBUST SD CONFIDENCE LIMIT Y
C                 LET A = UPPER ROBUST SD CONFIDENCE LIMIT Y
C                 LET A = ONE SIDED LOWER ROBUST SD CONFIDENCE INTERVAL Y
C                 LET A = ONE SIDED UPPER ROBUST SD CONFIDENCE INTERVAL Y
C
C              THE DATA CONSISTS OF N OBSERVATIONS IN Y.
C
C              THE METHOD HERE IS FROM THE BONETT PAPER.  THIS INTERVAL
C              PROVIDES A NEARLY EXACT INTERVAL FOR NORMALLY DISTRIBUTED
C              DATA, BUT ALSO PROVIDES GOOD PERFORMANCE FOR MODERATE
C              NON-NORMALITY.
C
C              THE INTERVAL FOR THE VARIANCE IS
C
C                 EXP{LOG(C*SIGMAHAT**2) +/- Z(ALPHA/2)*SE}
C
C              WHERE
C
C                 SIGMAHAT    = SAMPLE STANDARD DEVIATION
C                 Z           = NORMAL PERCENT POINT FUNCTION
C                 SE          = C*SQRT[{GAMMA4HAT - (N-3)/N}/(N-1)]
C                 C           = N/(N - Z(ALPHA/2))
C                 GAMMA4HAT   = AN ADJUSTED ESTIMATE OF KURTOSIS
C                               (SEE BELOW FOR DETAILS)
C
C              C IS A SMALL SAMPLE ADJUSTMENT FACTOR TO EQUALIZE TAIL
C              PROBABILITIES.
C
C              FOR THE STANDARD DEVIATION, TAKE THE SQUARE ROOT OF THE
C              ABOVE INTERVAL.
C
C              NIWITPONG AND KIRDWICHAI ADJUST THIS STATISTIC BY
C              USING THE MEDIAN RATHER THAN THE TRIMMED MEAN TO
C              COMPUTE THE ADJUSTED KURTOSIS AND THEY ALSO USE
C              t-INTERVALS RATHER THAN THE NORMAL INTERVALS.  THESE
C              ADJUSTMENTS RESULT IN A MORE CONSERVATIVE INTERVAL,
C              BUT ONES THAT ARE MORE LIKELY TO MEET THE NOMINAL
C              COVERAGE IN MORE EXTREME CASES OF NON-NORMALITY.
C
C     INPUT ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR OF
C                               (UNSORTED OR SORTED) OBSERVATIONS.
C                    --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                               IN THE VECTOR Y.
C                    --ALPHA  = THE SINGLE PRECISION VECTOR OF CONFIDENCE
C                               LEVELS
C                    --NALPHA = THE INTEGER NUMBER OF ALPHA VALUES
C                    --AKURT  = PRIOR ESTIMATE OF KURTOSIS
C     OUTPUT ARGUMENTS-ALOWLM = THE SINGLE PRECISION VECTOR OF LOWER LIMIT
C                               VALUES
C                     -AUPPLM = THE SINGLE PRECISION VECTOR OF UPPER LIMIT
C                               VALUES
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--BONETT (2006), "APPROXIMATE CONFIDENCE INTERVAL FOR
C                 STANDARD DEVIATION OF NONNORMAL DISTRIBUTIONS",
C                 COMPUTATIONAL STATISTICS AND DATA ANALYSIS,
C                 VOL. 50, PP. 775 - 782.
C               --NIWITPONG AND KIRDWICHAI (2008), "ADJUSTED BONETT
C                 CONFIDENCE INTERVAL FOR STANDARD DEVIATION OF
C                 NON-NORMAL DISTRIBUTIONS", THAILAND STATISTICIAN,
C                 VOL. 6, NO. 1, PP. 1-6.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     ORIGINAL VERSION--DECEMBER  2017.
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION TEMP1(*)
      DIMENSION ALOWLM(*)
      DIMENSION AUPPLM(*)
      DIMENSION ALPHA(*)
C
      CHARACTER*4 IBONAD
      CHARACTER*4 ICASA2
      CHARACTER*4 ICASA4
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
      DOUBLE PRECISION DTERM1
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='SDR3'
      ISUBN2='    '
      IWRITE='OFF'
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SDR3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPSDR3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,ICASA2,ICASA3,ICASA4
   52   FORMAT('IBUGA3,ISUBRO,ICASA2,ICASA3,ICASA4 = ',
     1         4(A4,2X),A4)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,53)N,NALPHA,ALPHA(1)
   53   FORMAT('N,NALPHA,ALPHA(1) = ',2I8,G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,54)AKURT,N0,IBONAD
   54   FORMAT('AKURT,N0,IBONAD = ',G15.7,I8,2X,A4)
        CALL DPWRST('XXX','WRIT')
        DO56I=1,N
          WRITE(ICOUT,57)I,Y(I)
   57     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
        DO76I=1,NALPHA
          WRITE(ICOUT,77)I,ALPHA(I)
   77     FORMAT('I,ALPHA(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   76   CONTINUE
      ENDIF
C
C               ********************************************
C               **  STEP 11--                             **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'SDR3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N.LT.5)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR: ROBUST STANDARD DEVIATION CONFIDENCE ',
     1         'LIMITS--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,102)
  102   FORMAT('      THE NUMBER OF ORIGINAL OBSERVATIONS  IS LESS ',
     1         'THAN FIVE.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,103)N
  103   FORMAT('      SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               ********************************************
C               **  STEP 21--                             **
C               **  CARRY OUT CALCULATIONS FOR SD         **
C               **  CONFIDENCE LIMITS.                    **
C               ********************************************
C
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.AND.ISUBRO.EQ.'SDR3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     ICASA2:  LOWE     => LOWER LIMIT
C              UPPE     => UPPER LIMIT
C     ICASA4:  ONES     => ONE-SIDED LIMIT
C              TWOS     => TWO-SIDED LIMIT
C
C     COMPUTE STANDARD DEVIATION
C
      DO210I=1,NALPHA
        ALOWLM(I)=CPUMIN
        AUPPLM(I)=CPUMIN
  210 CONTINUE
C
      CALL SORT(Y,N,Y)
      CALL MEAN(Y,N,IWRITE,YMEAN,IBUGA3,IERROR)
      CALL SD(Y,N,IWRITE,YSD,IBUGA3,IERROR)
      AN=REAL(N)
C
      IF(YSD.LE.0.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,212)
  212   FORMAT('      THE STANDARD DEVIATION OF THE ORIGINAL ',
     1         'OBSERVATIONS IS NON-POSITIVE.')
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C     COMPUTE ADJUSTED KURTOSIS
C
      IF(IBONAD.EQ.'ON' .OR. N.LE.6)THEN
        CALL MEDIAN(Y,N,IWRITE,TEMP1,MAXNXT,ADJMEA,IBUGA3,IERROR)
      ELSE
C
C       COMPUTE TRIMMED MEAN.  TRIM AT LEAST ONE FROM EACH END.
C
        APERC=1.0/(2.0*SQRT(AN-4.0))
        NTRIM=INT(AN*APERC + 0.5)
        IF(NTRIM.EQ.0)NTRIM=1
        NSTRT=NTRIM+1
        NSTOP=N-NTRIM
        NTEMP=N - 2*NTRIM
        DSUM1=0.0D0
        DO230I=NSTRT,NSTOP
          DSUM1=DSUM1 + DBLE(Y(I))
  230   CONTINUE
        DTERM1=DSUM1/DBLE(NTEMP)
        ADJMEA=REAL(DTERM1)
      ENDIF
C
      DSUM1=0.0D0
      DSUM2=0.0D0
      DO240I=1,N
        DSUM1=DSUM1 + (DBLE(Y(I) - ADJMEA)**4)
        DSUM2=DSUM2 + (DBLE(Y(I) - YMEAN)**2)
  240 CONTINUE
      DTERM1=DBLE(N)*DSUM1/(DSUM2**2)
      AKURTS=REAL(DTERM1)
C
C     POOL SAMPLE KURTOSIS WITH PRIOR MEASURE OF KURTOSIS
C     (IF GIVEN)
C
      IF(AKURT.NE.CPUMIN .AND. N0.GT.0)THEN
        AKURTS=(AN*AKURTS + REAL(N0)*AKURT)/REAL(N+N0)
      ENDIF
      TERM1=AKURTS - ((AN-3.0)/AN)
      SE=TERM1/(AN-1.0)
      SE=SQRT(SE)
C
      IF(ICASA4.EQ.'ONES')THEN
        DO460I=1,NALPHA
          ALPHAT=ALPHA(I)
          IF(ALPHAT.GE.1.0 .AND. ALPHAT.LT.100.0)ALPHAT=ALPHAT/100.
          IF(ALPHAT.LE.0.0 .OR. ALPHAT.GE.1.0)GOTO8000
          IF(ALPHAT.LT.0.5)ALPHAT=1.0 - ALPHAT
          IF(IBONAD.EQ.'OFF')THEN
            CALL NORPPF(ALPHAT,Z)
          ELSE
            IDF=N-1
            ANU=REAL(IDF)
            CALL TPPF(ALPHAT,ANU,Z)
          ENDIF
          C=AN/(AN+Z)
          TERM1=Z*C*SE
          TERM2=LOG(C*YSD**2)
          ALOWLM(I)=SQRT(EXP(TERM2 + TERM1))
          AUPPLM(I)=SQRT(EXP(TERM2 - TERM1))
  460   CONTINUE
      ELSEIF(ICASA4.EQ.'TWOS')THEN
        DO465I=1,NALPHA
          ALPHAT=ALPHA(I)
          IF(ALPHAT.GE.1.0 .AND. ALPHAT.LT.100.0)ALPHAT=ALPHAT/100.
          IF(ALPHAT.LE.0.0 .OR. ALPHAT.GE.1.0)GOTO8000
          IF(ALPHAT.GT.0.5)ALPHAT=1.0 - ALPHAT
          ALPHAT=ALPHAT/2.0
          IF(IBONAD.EQ.'OFF')THEN
            CALL NORPPF(ALPHAT,Z)
          ELSE
            IDF=N-1
            ANU=REAL(IDF)
            CALL TPPF(ALPHAT,ANU,Z)
          ENDIF
          C=AN/(AN+Z)
          TERM1=Z*C*SE
          TERM2=LOG(C*YSD**2)
          ALOWLM(I)=SQRT(EXP(TERM2 + TERM1))
          AUPPLM(I)=SQRT(EXP(TERM2 - TERM1))
C
          IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SDR3')THEN
            WRITE(ICOUT,471)I,Z,C,SE,TERM1,TERM2
  471       FORMAT('I,Z,C,SE,TERM1,TERM2 = ',I5,5G15.7)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,473)ALOWLM(I),AUPPLM(I)
  473       FORMAT('ALOWLM(I),AUPPLM(I) = ',2G15.7)
            CALL DPWRST('XXX','WRIT')
          ENDIF
C
  465   CONTINUE
      ENDIF
C
      GOTO9000
C
 8000 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,101)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,8001)I
 8001 FORMAT('      ROW ',I8,' OF ALPHA VALUES IS OUT OF RANGE.')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,8003)ALPHA(I)
 8003 FORMAT('      THE VALUE OF ALPHA IS ',G15.7)
      CALL DPWRST('XXX','WRIT')
      IERROR='YES'
      GOTO9000
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SDR3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9051)
 9051   FORMAT('**** AT THE END OF DPSDR3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9052)YMEAN,YSD,ADJMEA,AKURTS
 9052   FORMAT('YMEAN,YSD,ADJMEAN,AKURTS = ',4G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END 
      SUBROUTINE DPSEAR(IANS,IANSLC,IWIDTH,ICOM,IHARG,IHARG2,NUMARG,
     1                  ISEART,
CCCCC                   FEBRUARY 2003: ADD FOLLOWING LINE TO CALL LIST
     1                  IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                  IBUGS2,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--SEARCH A USER-DEFINED FILE FOR A USER-DEFINED STRING
C              AND PRINT ALL LINES WHERE THAT STRING OCCURS.
C     ALSO--IF CALLED FOR, SEARCH THE MASTER REFERENCE FILE (WHICH IS A
C           FILE CONTAINING LISTS OF FILE NAMES) FOR DATA FILE NAMES,
C           FOR REFERENCE FILE NAMES, AND FOR MACRO FILE NAMES.
C     ALSO--IF CALLED FOR, SEARCH THE DICTIONARY FILE (WHICH IS A FILE
C           CONTAINING THE LIST OF COMMANDS, FUNCTIONS, LET SUBCOMMANDS,
C           AND OTHER KEYWORDS.)
C     NOTE--THIS SUBROUTINE USES THE SAME FILE AS LIST.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--88/1
C     ORIGINAL VERSION--JANUARY   1988.
C     UPDATED         --AUGUST    1988. (CHANGE DPMASF TO DPDIRF)
C     UPDATED         --AUGUST    1988. (DICTIONARY FILE)
C     UPDATED         --JANUARY   1994. SEARCH1  (1LIN)
C     UPDATED         --FEBRUARY  2003. STORE LINE NUMBER OF FIRST MATCH
C                                       IN INTERNAL PARAMETER "LINENUMB".
C     UPDATED         --APRIL     2018. ADD "REFMAN.TEX" AND
C                                       "HANDBK.TEX" SEARCHES
C     UPDATED         --APRIL     2018. ADD VARIOUS SYNONYMS
C     UPDATED         --APRIL     2018. SUPPORT MORE THAN ONE
C                                       WORD FOR STRING TO MATCH
C     UPDATED         --SEPTEMBER 2019. ADD "GREP" OPTION TO USE
C                                       AN OPERATING SYSTEM BASED
C                                       SEARCH
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      INCLUDE 'DPCOPA.INC'
C
      CHARACTER*4 IANSLC(*)
      CHARACTER*4 IANS(*)
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG(*)
      CHARACTER*4 IHARG2(*)
C
      CHARACTER*4 IHNAME(*)
      CHARACTER*4 IHNAM2(*)
      CHARACTER*4 IUSE(*)
C
      CHARACTER*4 ISEART
      CHARACTER*4 IBUGS2
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
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
      CHARACTER*4 ICASEQ
      CHARACTER*4 IANSI
CCCCC CHARACTER*100 ICANS
CCCCC CHARACTER*100 ISTRIN
CCCCC CHARACTER*100 ISTRIU
CCCCC CHARACTER*100 ITAST
CCCCC CHARACTER*100 ITASTU
CCCCC CHARACTER*255 IAOUT
      CHARACTER (LEN=MAXSTR) :: IAOUT
      CHARACTER (LEN=MAXSTR) :: ICANS
      CHARACTER (LEN=MAXSTR) :: ISTRIN
      CHARACTER (LEN=MAXSTR) :: ISTRIU
      CHARACTER (LEN=MAXSTR) :: ITAST
      CHARACTER (LEN=MAXSTR) :: ITASTU
      CHARACTER*4 IHIT
      CHARACTER*4 IGO
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 ISSAV1
      CHARACTER*4 ISSAV2
C
      DIMENSION VALUE(*)
      DIMENSION IVALUE(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPSE'
      ISUBN2='AR  '
      IFOUND='YES'
      IERROR='NO'
      IHIT='NO'
      IGO='NO'
      IAOUT=' '
C
      ILISAV=1
      MINN2=1
      NCSTRI=(-999)
      MAXLEN=9999
      NQ=0
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SEAR')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPSEAR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IWIDTH,IBUGS2,ISUBRO,IERROR,ISEART,ICOM
   53   FORMAT('IWIDTH,IBUGS2,ISUBRO,IERROR,ISEART,ICOM = ',
     1         I5,5(2X,A4))
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,55)(IANSLC(I),I=1,MIN(100,IWIDTH))
   55     FORMAT('(IANSLC(I),I=1,IWIDTH) = ',100A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        WRITE(ICOUT,61)ILISNU,IDIRNU,IDICNU
   61   FORMAT('ILISNU,IDIRNU,IDICNU = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,62)ILISNA(1:80)
   62   FORMAT('ILISNA = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,63)ILISST,ILISFO,ILISAC,ILISFO,ILISCS
   63   FORMAT('ILISST,ILISFO,ILISAC,ILISFO,ILISCS = ',
     1         4(A12,2X),A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)IDIRNA(1:80)
   72   FORMAT('IDIRNA = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,73)IDIRST,IDIRFO,IDIRAC,IDIRFO,IDIRCS
   73   FORMAT('IDIRST,IDIRFO,IDIRAC,IDIRFO,IDIRCS = ',
     1         4(A12,2X),A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,82)IDICNA(1:80)
   82   FORMAT('IDICNA = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,83)IDICST,IDICFO,IDICAC,IDICFO,IDICCS
   83   FORMAT('IDICST,IDICFO,IDICAC,IDICFO,IDICCS = ',
     1         4(A12,2X),A12)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *************************************************
C               **  STEP 10--                                  **
C               **  PERFORM AN OPERATION SYSTEM BASED SEARCH   **
C               *************************************************
C
C     2019/09: THE "GREP" VARIANT WILL DO AN OPERATIONG SYSTEM
C              BASED SEARCH.
C
C                 1. FOR LINUX/UNIX (AND MACOS) SYSTEMS, USE THE
C                    GREP COMMAND.
C
C                 2. FOR WINDOWS SYSTEMS, USE THE FINDSTR COMMAND.
C
C              NOTE THAT WHILE THIS CAN EASILY BE DONE JUST USING
C              THE "SYSTEM" COMMAND, IMPLEMENTING THIS AS A DISTINCT
C              COMMAND ALLOWS GENERAL PURPOSE MACROS TO BE WRITTEN
C              THAT DON'T REQUIRE SPECIAL CODING BASED ON THE HOST
C              OPERATING SYSTEM.
C
      ISTEPN='10'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SEAR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ISEART.EQ.'GREP' .OR. ISEART.EQ.'FIND')THEN
C
C       STEP 1: FIND FIRST NON-BLANK CHARACTER AFTER "GREP "
C               OR "FINDSTR ".
C
        ISTRT=-1
        IF(ISEART.EQ.'GREP')THEN
          DO101II=1,MIN(250,IWIDTH-5)
            IF(IANS(II)(1:1)  .EQ.'G' .AND. IANS(II+1)(1:1).EQ.'R' .AND.
     1         IANS(II+2)(1:1).EQ.'E' .AND. IANS(II+3)(1:1).EQ.'P' .AND.
     1         IANS(II+4)(1:1).EQ.' ')THEN
              ISTRT=II+5
              GOTO109
            ENDIF
  101     CONTINUE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1211)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,103)
  103     FORMAT('      GREP NOT FOUND AT BEGINNING OF COMMAND LINE.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
  109     CONTINUE
        ELSEIF(ISEART.EQ.'FIND')THEN
          DO111II=1,MIN(247,IWIDTH-8)
            IF(IANS(II)(1:1)  .EQ.'F' .AND. IANS(II+1)(1:1).EQ.'I' .AND.
     1         IANS(II+2)(1:1).EQ.'N' .AND. IANS(II+3)(1:1).EQ.'D' .AND.
     1         IANS(II+4)(1:1).EQ.'S' .AND. IANS(II+5)(1:1).EQ.'T' .AND.
     1         IANS(II+6)(1:1).EQ.'R' .AND. IANS(II+7)(1:1).EQ.' ')THEN
              ISTRT=II+8
              GOTO119
            ENDIF
  111     CONTINUE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1211)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,113)
  113     FORMAT('      FINDSTR NOT FOUND AT BEGINNING OF COMMAND ',
     1           'LINE.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
  119     CONTINUE
C
          DO121II=ISTRT,IWIDTH
            IF(IANS(II)(1:1).NE.' ')THEN
              ISTRT=II
              GOTO129
            ENDIF
  121     CONTINUE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1211)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,123)
  123     FORMAT('      NO TEXT FOUND AFTER GREP (OR FINDSTR) ',
     1           'ON THE COMMAND LINE.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
  129     CONTINUE
        ENDIF
C
C       STEP 2: FIND LAST NON-BLANK CHARACTER
C
        ILAST=ISTRT
        DO131II=IWIDTH,ISTRT,-1
          IF(IANS(II)(1:1).NE.' ')THEN
            ILAST=II
            GOTO139
          ENDIF
  131   CONTINUE
  139   CONTINUE
C
C       STEP 3: CREATE STRING TO BE SENT TO SYSTEM COMMAND.
C               NOTE THAT DATAPLOT DOES NO ERROR CHECKING, IT
C               JUST PASSES WHAT THE USER ENTERED.
C
        IF(IHOST1.EQ.'IBM-')THEN
          IF(IGRPCA.EQ.'IGNO')THEN
            NCSTR=11
            IAOUT(1:NCSTR)='FINDSTR /I '
          ELSE
            NCSTR=8
            IAOUT(1:NCSTR)='FINDSTR '
          ENDIF
          IF(IGRPRE.EQ.'ON')THEN
            IAOUT(NCSTR+1:NCSTR+3)='/S '
            NCSTR=NCSTR+3
          ENDIF
          IF(IGRPLN.EQ.'ON')THEN
            IAOUT(NCSTR+1:NCSTR+3)='/N '
            NCSTR=NCSTR+3
          ENDIF
          IF(IGRPEM.EQ.'ON')THEN
            IAOUT(NCSTR+1:NCSTR+3)='/X '
            NCSTR=NCSTR+3
          ENDIF
          IF(IGRPNM.EQ.'ON')THEN
            IAOUT(NCSTR+1:NCSTR+3)='/V '
            NCSTR=NCSTR+3
          ENDIF
          IF(IGRPFN.EQ.'ON')THEN
            IAOUT(NCSTR+1:NCSTR+3)='/M '
            NCSTR=NCSTR+3
          ENDIF
        ELSE
          IF(IGRPCA.EQ.'IGNO')THEN
            NCSTR=8
            IAOUT(1:NCSTR)='grep -i '
          ELSE
            NCSTR=5
            IAOUT(1:NCSTR)='grep '
          ENDIF
          IF(IGRPRE.EQ.'ON')THEN
            IAOUT(NCSTR+1:NCSTR+3)='-r '
            NCSTR=NCSTR+3
          ENDIF
          IF(IGRPLN.EQ.'ON')THEN
            IAOUT(NCSTR+1:NCSTR+3)='-n '
            NCSTR=NCSTR+3
          ENDIF
          IF(IGRPEM.EQ.'ON')THEN
            IAOUT(NCSTR+1:NCSTR+3)='-x '
            NCSTR=NCSTR+3
          ENDIF
          IF(IGRPNM.EQ.'ON')THEN
            IAOUT(NCSTR+1:NCSTR+3)='-v '
            NCSTR=NCSTR+3
          ENDIF
          IF(IGRPFN.EQ.'ON')THEN
            IAOUT(NCSTR+1:NCSTR+3)='-l '
            NCSTR=NCSTR+3
          ENDIF
        ENDIF
C
        MAXNCH=MAXSTR
        DO141II=ISTRT,ILAST
          NCSTR=NCSTR+1
          IF(NCSTR.GT.MAXNCH)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1211)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1291)MAXNCH
 1291       FORMAT('      MAXIMUM NUMBER OF CHARACTERS (',I3,
     1             ') EXCEEDED.')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
          IAOUT(NCSTR:NCSTR)=IANSLC(II)(1:1)
  141   CONTINUE
C
        ISSAV1=ISYSPE
        ISSAV2=ISYSHI
        ISYSPE='ON'
        ISYSHI='OFF'
        CALL DPSYS2(IAOUT,NCSTR,ISUBRO,IERROR)
        ISYSPE=ISSAV1
        ISYSHI=ISSAV2
C
        GOTO9000
      ENDIF
C
C               **************************
C               **  STEP 11--           **
C               **  COPY OVER VARIABLES **
C               **************************
C
      ISTEPN='11'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SEAR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IOUNIT=ILISNU
      IFILE=ILISNA
      ISTAT=ILISST
      IFORM=ILISFO
      IACCES=ILISAC
      IPROT=ILISPR
      ICURST=ILISCS
C
      ISUBN0='SEAR'
      IERRFI='NO'
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SEAR')THEN
        WRITE(ICOUT,1193)IOUNIT,ISUBN0,IERRFI
 1193   FORMAT('IOUNIT,ISUBN0,IERRFI = ',I8,2(2X,A4))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1194)IFILE
 1194   FORMAT('IFILE = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1195)ISTAT,IFORM,IACCES,IPROT,ICURST
 1195   FORMAT('ISTAT,IFORM,IACCES,IPROT,ICURST = ',
     1         4(A12,2X),A12)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***********************************************
C               **  STEP 12--                                **
C               **  CHECK TO SEE IF THE LIST FILE MAY EXIST  **
C               ***********************************************
C
      ISTEPN='12'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SEAR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ISTAT.EQ.'NONE')THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1211)
 1211   FORMAT('***** ERROR IN SEARCH COMMAND--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1212)
 1212   FORMAT('      THE DESIRED SEARCHING CANNOT BE CARRIED OUT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1214)
 1214   FORMAT('      BECAUSE THE INTERNAL VARIABLE    ILISST   WHICH')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1215)
 1215   FORMAT('      ALLOWS SUCH SEARCHING HAS BEEN SET TO    NONE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1217)ISTAT,ILISST
 1217   FORMAT('ISTAT,ILISST = ',A12,2X,A12)
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
C               ********************************
C               **  STEP 13--                 **
C               **  EXTRACT THE FILE NAME.    **
C               **  DO THE GENERAL CASE OF    **
C               **  SEARCHING GENERAL FILES.  **
C               **  DO ALSO THE SPECIAL CASE  **
C               **  OF SEARCHING THE          **
C               **  MASTER DIRECTORY FILE.    **
C               **  DO ALSO THE SPECIAL CASE  **
C               **  OF SEARCHING THE          **
C               **  DICTIONARY      FILE.     **
C               ********************************
C
      ISTEPN='13'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SEAR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO1310I=1,MAXSTR
        IANSI=IANSLC(I)
        ICANS(I:I)=IANSI(1:1)
 1310 CONTINUE
C
      IFLAGZ=0
      ISTART=1
      ISTOP=IWIDTH
      IWORD=2
      NCFILE=0
      IF(ICOM.NE.'?   ' .AND. ICOM.NE.'??? ')THEN
        CALL DPEXWO(ICANS,ISTART,ISTOP,IWORD,
     1              ICOL1,ICOL2,IFILE,NCFILE,
     1              IBUGS2,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
      ELSEIF(ICOM.EQ.'?   ' .AND. NUMARG.EQ.0)THEN
        IFILE='dp_question_mark_examples.txt'
        NCFILE=29
        IFLAGZ=1
        ICASEQ='FULL'
        GOTO2190
      ENDIF
C
      IFLAGF=1
C
      IF((NCFILE.EQ.9.AND.IFILE.EQ.'DIRECTORY') .OR.
     1   (NCFILE.EQ.9.AND.IFILE.EQ.'directory') .OR.
     1   (NCFILE.EQ.3.AND.IFILE.EQ.'DIR') .OR.
     1   (NCFILE.EQ.3.AND.IFILE.EQ.'dir') .OR.
     1   (NCFILE.EQ.4.AND.IFILE.EQ.'DIRE') .OR.
     1   (NCFILE.EQ.4.AND.IFILE.EQ.'dire') .OR.
     1   (NCFILE.EQ.1.AND.IFILE.EQ.'M') .OR.
     1   (NCFILE.EQ.1.AND.IFILE.EQ.'m') .OR.
     1   (NCFILE.EQ.6.AND.IFILE.EQ.'MASTER') .OR.
     1   (NCFILE.EQ.6.AND.IFILE.EQ.'master'))THEN
        IFILE=IDIRNA
      ELSEIF((NCFILE.EQ.10.AND.IFILE.EQ.'DICTIONARY') .OR.
     1       (NCFILE.EQ.10.AND.IFILE.EQ.'dictionary') .OR.
     1       (NCFILE.EQ.3.AND.IFILE.EQ.'DIC') .OR.
     1       (NCFILE.EQ.3.AND.IFILE.EQ.'dic') .OR.
     1       (NCFILE.EQ.4.AND.IFILE.EQ.'DICT') .OR.
     1       (NCFILE.EQ.4.AND.IFILE.EQ.'dict'))THEN
        IFILE=IDICNA
      ELSEIF((NCFILE.EQ.8.AND.IFILE.EQ.'HANDBOOK') .OR.
     1       (NCFILE.EQ.8.AND.IFILE.EQ.'handbook') .OR.
     1       (NCFILE.EQ.2.AND.IFILE.EQ.'HB') .OR.
     1       (NCFILE.EQ.2.AND.IFILE.EQ.'hb') .OR.
     1       (NCFILE.EQ.6.AND.IFILE.EQ.'HANDBK') .OR.
     1       (NCFILE.EQ.6.AND.IFILE.EQ.'handbk') .OR.
     1       (ICOM.EQ.'??? '))THEN
        IFILE='handbk.tex'
        NCFILE=10
      ELSEIF((NCFILE.EQ.9.AND.IFILE.EQ.'REFERENCE') .OR.
     1       (NCFILE.EQ.9.AND.IFILE.EQ.'reference') .OR.
     1       (NCFILE.EQ.2.AND.IFILE.EQ.'RM') .OR.
     1       (NCFILE.EQ.2.AND.IFILE.EQ.'rm') .OR.
     1       (ICOM.EQ.'?   '))THEN
        IFILE='refman.tex'
        NCFILE=10
        MAXLEN=40
      ELSE
C
C       2018/04: CHECK IF FIRST ARGUMENT IS A FILE NAME (I.E.,
C                DOES IT CONTAIN A ".").
C
        DO1360II=1,NCFILE
          IF(IFILE(II:II).EQ.'.')GOTO1369
 1360   CONTINUE
        IFLAGF=0
        IFILE='refman.tex'
        NCFILE=10
 1369   CONTINUE
      ENDIF
C
      IF(NCFILE.LT.1)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1372)
 1372   FORMAT('      A USER FILE NAME IS REQUIRED IN THE SEARCH ',
     1         'COMMAND')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1374)
 1374   FORMAT('      (FOR EXAMPLE,    SEARCH PROG7.DP)')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1375)
 1375   FORMAT('      BUT NONE WAS GIVEN HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1376)
 1376   FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,1377)(IANSLC(I),I=1,MIN(80,IWIDTH))
 1377     FORMAT('      ',80A1)
          CALL DPWRST('XXX','BUG ')
        ELSE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GOTO9000
      ENDIF
C
C               ************************************************
C               **  STEP 14--                                 **
C               **  EXTRACT THE STRING TO BE SEARCHED FOR.    **
C               ************************************************
C
      ISTEPN='14'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SEAR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ISTART=1
      ISTOP=IWIDTH
      IWORD=3
      IF(ICOM.EQ.'?   ' .OR. ICOM.EQ.'??? ')THEN
        IWORD=2
      ELSE
        IWORD=3
        IF(IFLAGF.EQ.0)IWORD=IWORD-1
      ENDIF
C
C     2018/04: USE DPEXW3 INSTEAD OF DPEXWO SO THAT
C              THE STRING IS EXTRACTED IS TO THE END
C              OF THE COMMAND LINE INSTEAD OF JUST A
C              SINGLE WORD.  SO "SEARCH REFERENCE MEAN PLOT"
C              WILL MATCH "MEAN PLOT" INSTEAD OF JUST
C              "MEAN".
C
      CALL DPEXW3(ICANS,ISTART,ISTOP,IWORD,
     1            ICOLS1,ICOLS2,ITAST,NCTAST,
     1            IBUGS2,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      NMAX=MAXSTR
      CALL DPUP80(ITAST,ITASTU,NMAX,IBUGS2,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(NCTAST.LT.1)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1442)
 1442   FORMAT('      A TARGET STRING IS REQUIRED IN THE SEARCH ',
     1         'COMMAND')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1444)
 1444   FORMAT('      (FOR EXAMPLE,    SEARCH PHONE.TEX JONES)')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1445)
 1445   FORMAT('      BUT NONE WAS GIVEN HERE.')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,1377)(IANSLC(I),I=1,MIN(80,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ELSE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GOTO9000
      ENDIF
C
C               *****************************************
C               **  STEP 21--                          **
C               **  CHECK TO SEE THE TYPE CASE--       **
C               **    1) UNQUALIFIED (THAT IS, FULL);  **
C               **    2) SUBSET/EXCEPT; OR             **
C               **    3) FOR.                          **
C               *****************************************
C
      ISTEPN='21'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SEAR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASEQ='FULL'
      ILOCQ=NUMARG+1
      IF(NUMARG.LT.1)GOTO2190
      DO2100J=1,NUMARG
        J1=J
        IF(IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ')THEN
          ICASEQ='SUBS'
          ILOCQ=J1
          GOTO2190
        ELSEIF(IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  ')THEN
          ICASEQ='SUBS'
          ILOCQ=J1
          GOTO2190
        ELSEIF(IHARG(J).EQ.'FOR '.AND.IHARG2(J).EQ.'    ')THEN
          ICASEQ='FOR'
          ILOCQ=J1
          GOTO2190
        ENDIF
 2100 CONTINUE
 2190 CONTINUE
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SEAR')THEN
        WRITE(ICOUT,2191)NUMARG,ILOCQ,NCFILE
 2191   FORMAT('NUMARG,ILOCQ,NCFILE = ',3I8)
        CALL DPWRST('XXX','BUG ')
        IF(NCFILE.GT.0)THEN
          WRITE(ICOUT,2192)IFILE(1:NCFILE)
 2192     FORMAT('IFILE(1:NCFILE) = ',A80)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
C               *********************************************
C               **  STEP 22--                              **
C               **  BRANCH    TO THE APPROPRIATE SUBCASE   **
C               **  (FULL, SUBSET, OR FOR).                **
C               *********************************************
C
      ISTEPN='22'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SEAR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASEQ.EQ.'FULL')THEN
        DO2215I=1,MAXN
          ISUB(I)=1
 2215   CONTINUE
        NQ=MAXN
      ELSEIF(ICASEQ.EQ.'SUBS')THEN
        NIOLD=MAXN
        CALL DPSUBS(NIOLD,ILOCS,NS,IBUGQ,IERROR)
        NQ=NIOLD
      ELSEIF(ICASEQ.EQ.'FOR')THEN
        NIOLD=MAXN
        CALL DPFOR(NIOLD,NFOR,IROW1,IROWN,
     1             NLOCAL,ILOCS,NS,IBUGQ,IERROR)
        NQ=NFOR
        NMXFOR=IROWN
      ENDIF
C
      IF(NQ.LT.MINN2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2272)
 2272   FORMAT('      AFTER THE APPROPRIATE SUBSET HAS BEEN EXTRACTED,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2273)
 2273   FORMAT('      THE NUMBER OF SPECIFIED FILE LINES TO BE LISTED')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2276)MINN2
 2276   FORMAT('      MUST BE ',I8,' OR LARGER;  SUCH WAS NOT THE ',
     1         'CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,1377)(IANSLC(I),I=1,MIN(80,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ELSE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
      NS=NQ
C
C               **************************
C               **  STEP 51--           **
C               **  OPEN  THE FILE      **
C               **************************
C
      ISTEPN='31'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SEAR')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,3111)IFILE
 3111   FORMAT('IFILE = ',A80)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IREWIN='ON'
      CALL DPOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1            IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
      IF(IERRFI.EQ.'YES')GOTO9000
C
C               *************************************
C               **  STEP 41--                      **
C               **  READ A GENERAL FILE.           **
C               **  SEARCH FOR THE STRING.         **
C               **  IF FOUND, PRINT THE LINE OUT.  **
C               **  PRINT ALL LINES ON WHICH THE   **
C               **  STRING OCCURS.                 **
C               *************************************
C
      ISTEPN='41'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SEAR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IMAX=1000000
      IF(ICASEQ.EQ.'SUBS')IMAX=MAXN
      IF(ICASEQ.EQ.'FOR')IMAX=IROWN
C
      ILISAV=-1
C
      NMAX=MAXLEN
      IF(MAXLEN.GT.255)NMAX=255
CCCCC NMAX=100
      DO4110I=1,IMAX
C
        ILICUR=I
C
        ISTRIN=' '
        READ(IOUNIT,4111,END=4190)(ISTRIN(J:J),J=1,NMAX)
 4111   FORMAT(255A1)
C
        IF(IFLAGZ.EQ.1)THEN
          DO44111JJ=NMAX,1,-1
            IF(ISTRIN(JJ:JJ).NE.' ')THEN
              WRITE(ICOUT,4117)(ISTRIN(J:J),J=1,JJ)
              CALL DPWRST('XXX','BUG ')
              GOTO4110
            ENDIF
44111     CONTINUE
          WRITE(ICOUT,'(A1)')ISTRIN(1:1)
          CALL DPWRST('XXX','BUG ')
          GOTO4110
        ENDIF
C
        CALL DPDB80(ISTRIN,JMAX,NMAX,IBUGS2,ISUBRO,IERROR)
        NCSTRI=JMAX
C
        CALL DPUP80(ISTRIN,ISTRIU,NMAX,IBUGS2,IERROR)
C
        IF(NCSTRI.EQ.3.AND.ISTRIN(1:3).EQ.'EOF')GOTO4190
        IF(ICASEQ.EQ.'FULL' .OR.
     1    (ICASEQ.EQ.'SUBS' .OR.ICASEQ.EQ.'FOR'.AND.ISUB(I).EQ.1))THEN
           IHIT='NO'
           IF(ISEART.EQ.'1LIN')IGO='NO'
           IF(ISEART.EQ.'FIRS')IGO='NO'
           IF(ISEART.EQ.'BLAN'.AND.NCSTRI.LE.0)IGO='NO'
           IF(ISEART.EQ.'----'.AND.ISTRIN(1:4).EQ.'----')IGO='NO'
           IF(IGO.EQ.'YES')GOTO4129
C
           IF(NCSTRI.LE.0)GOTO4129
           DO4120I1=1,NCSTRI
             I2=I1+NCTAST-1
             IF(I2.GT.NCSTRI)GOTO4129
             IF(ISTRIN(I1:I2).EQ.ITAST(1:NCTAST))IHIT='YES'
             IF(ISTRIU(I1:I2).EQ.ITASTU(1:NCTAST))IHIT='YES'
             IF(IHIT.EQ.'YES')IGO='YES'
             IF(IHIT.EQ.'YES'.AND.ILISAV.LT.0)ILISAV=ILICUR
             IF(IHIT.EQ.'YES')GOTO4129
 4120      CONTINUE
 4129      CONTINUE
C
           IF(IHIT.EQ.'YES'.OR.IGO.EQ.'YES')THEN
             WRITE(ICOUT,4117)(ISTRIN(J:J),J=1,MIN(100,NCSTRI))
 4117        FORMAT(100A1)
             CALL DPWRST('XXX','BUG ')
           ENDIF
C
           IF(IHIT.EQ.'YES'.AND.ISEART.EQ.'FIRS')GOTO4190
        ENDIF
C
 4110 CONTINUE
C
 4190 CONTINUE
C
      IH='LINE'
      IH2='NUMB'
      VALUE0=REAL(ILISAV)
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1            IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1            IANSLC,IWIDTH,IBUGS2,IERROR)
C
C               **************************
C               **  STEP 51--           **
C               **  CLOSE THE FILE      **
C               **************************
C
      ISTEPN='51'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SEAR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IENDFI='OFF'
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
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'SEAR')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSEAR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR,IERRFI,IOUNIT
 9012   FORMAT('IERROR,IERRFI,IOUNIT = ',2(A4,2X),I5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9022)IFILE(1:80)
 9022   FORMAT('IFILE  = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9023)ISTAT,IFORM,IACCES,IPROT,ICURST
 9023   FORMAT('ISTAT,IFORM,IACCES,IPROT,ICURST  = ',4(A12,2X),A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9028)IENDFI,IREWIN
 9028   FORMAT('IENDFI,IREWIN = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9041)ICASEQ,NQ,NS,JMAX,NCSTRI
 9041   FORMAT('ICASEQ,NQ,NS,JMAX,NCSTRI = ',A4,4I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPSECL(IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,
     1                  IDEFCO,MAXSEG,ISEGCO,ISEGC2,IRGBMX,
     1                  IBUGP2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE COLOR FOR A SEGMENT.  THE COLOR FOR SEGMENT I
C              WILL BE PLACED IN THE I-TH ELEMENT OF THE CHARACTER
C              VECTOR ISEGCO(.).
C     INPUT  ARGUMENTS--IHARG  (A CHARACTER VECTOR)
C                     --IARGT  (A CHARACTER VECTOR)
C                     --IARG   (A CHARACTER VECTOR)
C                     --NUMARG
C                     --IDEFCO
C                     --MAXSEG
C     OUTPUT ARGUMENTS--ISEGCO (A CHARACTER VECTOR WHOSE I-TH ELEMENT
C                              CONTAINS THE COLOR FOR SEGMENT I.
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABOARATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--SEPTEMBER 1980.
C     UPDATED         --MAY       1982.
C     UPDATED         --OCTOBER   2020. SUPPORT FOR RGB COLOR
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IDEFCO
      CHARACTER*4 IBUGP2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IHARG(*)
      CHARACTER*4 IHARG2(*)
      CHARACTER*4 IARGT(*)
      CHARACTER*4 ISEGCO(*)
      DIMENSION IARG(*)
      DIMENSION ARG(*)
      DIMENSION ISEGC2(MAXSEG,3)
C
      CHARACTER*4 IHOLD
      CHARACTER*4 ICASCL
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
      ICASCL='STAN'
      ISUBN1='DPSE'
      ISUBN2='CL  '
      IHOLD='    '
      JHOLD1=-1
      JHOLD2=-1
      JHOLD3=-1
C
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'SECL')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPSECL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGP2,ISUBRO,IFOUND,IERROR
   52   FORMAT('IBUGP2,ISUBRO,IFOUND,IERROR = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IDEBCO,MAXSEG,NUMSEG,NUMARG,IRGBMX
   53   FORMAT('IDEBCO,MAXSEG,NUMSEG,NUMARG,IRGBMX = ',A4,2X,4I8)
        CALL DPWRST('XXX','BUG ')
        DO65I=1,NUMARG
          WRITE(ICOUT,66)I,IARG(I),IHARG(I)
   66     FORMAT('I,IARG(I),IHARG(I) = ',2I8,2X,A4)
          CALL DPWRST('XXX','BUG ')
   65   CONTINUE
        DO75I=1,10
          WRITE(ICOUT,76)I,ISEGCO(I),ISEGC2(I,1),ISEGC2(I,2),ISEGC2(I,3)
   76     FORMAT('I,ISEGCO(I),ISEGC2(I,1),ISEGC2(I,2),ISEGC2(I,3) = ',
     1           I8,2X,A4,2X,3I5)
          CALL DPWRST('XXX','BUG ')
   75   CONTINUE
      ENDIF
C
      ISTEPN='1'
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'SECL')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.EQ.0)GOTO9000
      IF(IHARG(1).EQ.'RGB ' .AND. IHARG(2).EQ.'COLO')THEN
        ICASCL='RGB '
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGP2,IERROR)
      ELSEIF(IHARG(2).EQ.'RGB ' .AND. IHARG(3).EQ.'COLO')THEN
        ICASCL='RGB '
        DO45II=2,NUMARG-1
          IHARG(II)=IHARG(II+1)
          IHARG2(II)=IHARG2(II+1)
          IARGT(II)=IARGT(II+1)
          IARG(II)=IARG(II+1)
          ARG(II)=ARG(II+1)
  45    CONTINUE
        IHARG(NUMARG)=IHARG(II+1)
        IHARG2(NUMARG)=IHARG2(II+1)
        IARGT(NUMARG)=IARGT(II+1)
        IARG(NUMARG)=IARG(II+1)
        ARG(NUMARG)=ARG(II+1)
        NUMARG=NUMARG-1
      ENDIF
C
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COLO')THEN
C
      ISTEPN='2'
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'SECL')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IF(NUMARG.LE.1        .OR. IHARG(2).EQ.'ON'   .OR.
     1     IHARG(2).EQ.'OFF'  .OR. IHARG(2).EQ.'AUTO' .OR.
     1     IHARG(2).EQ.'DEFA')THEN
          IF(ICASCL.EQ.'RGB ')THEN
            JHOLD1=-1
            JHOLD2=-1
            JHOLD3=-1
          ELSE
            IHOLD=IDEFCO
          ENDIF
        ELSE
          IF(ICASCL.EQ.'RGB ')THEN
            IF(NUMARG.GE.4)THEN
              JHOLD1=IARG(2)
              JHOLD2=IARG(3)
              JHOLD3=IARG(4)
              IF(JHOLD1.LT.0 .OR. JHOLD1.GT.IRGBMX)JHOLD1=-1
              IF(JHOLD2.LT.0 .OR. JHOLD2.GT.IRGBMX)JHOLD2=-1
              IF(JHOLD3.LT.0 .OR. JHOLD3.GT.IRGBMX)JHOLD3=-1
            ELSE
              JHOLD1=-1
              JHOLD2=-1
              JHOLD3=-1
            ENDIF
          ELSE
            IHOLD=IHARG(2)
          ENDIF
        ENDIF
C
        IFOUND='YES'
        IF(ICASCL.EQ.'RGB ')THEN
          DO1138I=1,MAXSEG
            ISEGC2(I,1)=JHOLD1
            ISEGC2(I,2)=JHOLD2
            ISEGC2(I,3)=JHOLD3
 1138     CONTINUE
C
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            I=1
            WRITE(ICOUT,1139)ISEGC2(I,1),ISEGC2(I,2),ISEGC2(I,3)
 1139       FORMAT('ALL SEGMENT RGB COLORS HAVE JUST BEEN SET TO ',3I5)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSE
          DO1135I=1,MAXSEG
            ISEGCO(I)=IHOLD
 1135     CONTINUE
C
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            I=1
            WRITE(ICOUT,1136)ISEGCO(I)
 1136       FORMAT('ALL SEGMENT COLORS HAVE JUST BEEN SET TO ',A4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ENDIF
        GOTO9000
C
      ELSEIF(NUMARG.GE.2.AND.IHARG(2).EQ.'COLO')THEN
C
      ISTEPN='3'
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'SECL')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IF(IARGT(1).NE.'NUMB')THEN
          IERROR='YES'
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1141)
 1141     FORMAT('***** ERROR IN SEGMENT COLOR (DPSECL)--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1142)
 1142     FORMAT('      IN THE SEGMENT ... COLOR COMMAND,')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1143)
 1143     FORMAT('      THE SEGMENT IS IDENTIFIED BY A NUMBER, AS IN--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1144)
 1144     FORMAT('      SEGMENT 3 COLOR GREEN')
          CALL DPWRST('XXX','BUG ')
          GOTO9000
        ENDIF
C
        I=IARG(1)
        IF(I.LT.1 .OR. I.GT.MAXSEG)THEN
          IERROR='YES'
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1141)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1152)
 1152     FORMAT('      IN THE SEGMENT ... COLOR COMMAND,THE NUMBER ',
     1           'OF SEGMENTS')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1154)MAXSEG
 1154     FORMAT('      MUST BE BETWEEN 1 AND ',I8,' (INCLUSIVELY);')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1155)
 1155     FORMAT('      SUCH WAS NOT THE CASE HERE--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1156)I
 1156     FORMAT('      A REFERENCE WAS MADE TO THE ',I8,'-TH SEGMENT.')
          CALL DPWRST('XXX','BUG ')
          GOTO9000
        ENDIF
C
        IF(NUMARG.LE.2        .OR. IHARG(3).EQ.'ON'   .OR.
     1     IHARG(3).EQ.'OFF'  .OR. IHARG(3).EQ.'AUTO' .OR.
     1     IHARG(3).EQ.'DEFA')THEN
          IF(ICASCL.EQ.'RGB ')THEN
            JHOLD1=-1
            JHOLD2=-1
            JHOLD3=-1
          ELSE
            IHOLD=IDEFCO
          ENDIF
        ELSE
          IF(ICASCL.EQ.'RGB ')THEN
            JHOLD1=-1
            JHOLD2=-1
            JHOLD3=-1
            IF(NUMARG.GE.5)THEN
              JHOLD1=IARG(3)
              JHOLD2=IARG(4)
              JHOLD3=IARG(5)
              IF(JHOLD1.LT.0 .OR. JHOLD1.GT.IRGBMX)JHOLD1=-1
              IF(JHOLD2.LT.0 .OR. JHOLD2.GT.IRGBMX)JHOLD2=-1
              IF(JHOLD3.LT.0 .OR. JHOLD3.GT.IRGBMX)JHOLD3=-1
            ENDIF
          ELSE
            IHOLD=IHARG(3)
          ENDIF
        ENDIF
C
        IFOUND='YES'
        IF(ICASCL.EQ.'RGB ')THEN
          ISEGC2(I,1)=JHOLD1
          ISEGC2(I,2)=JHOLD2
          ISEGC2(I,3)=JHOLD3
C
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1286)I,ISEGC2(I,1),ISEGC2(I,2),ISEGC2(I,3)
 1286       FORMAT('THE RGB COLOR FOR SEGMENT ',I8,
     1             ' HAS JUST BEEN SET TO ',3I5)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSE
          ISEGCO(I)=IHOLD
C
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1186)I,ISEGCO(I)
 1186       FORMAT('THE COLOR FOR SEGMENT ',I8,' HAS JUST BEEN SET TO ',
     1             A4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ENDIF
      ENDIF
C
 9000 CONTINUE
C
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'SECL')THEN
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSECL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR,NUMSEG
 9012   FORMAT('IFOUND,IERROR,NUMSEG = ',2(A4,2X),I5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IHOLD,JHOLD1,JHOLD2,JHOLD3
 9014   FORMAT('IHOLD,JHOLD1,JHOLD2,JHOLD3 = ',A4,2X,3I5)
        CALL DPWRST('XXX','BUG ')
        DO9075I=1,10
          WRITE(ICOUT,76)I,ISEGCO(I),ISEGC2(I,1),ISEGC2(I,2),ISEGC2(I,3)
          CALL DPWRST('XXX','BUG ')
 9075   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPSECO(IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,IANS,IWIDTH,
     1MAXSEG,PSEGXC,PSEGYC,NUMSEG,IBUGP2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE 2 PAIRS OF (X,Y) COORDINATES
C              FOR A LINE SEGMENT.
C              THE FIRST PAIR WILL BE FOR THE TAIL OF THE SEGMENT;
C              THE SECOND PAIR WILL BE FOR THE HEAD OF THE SEGMENT.
C              THE (X1,Y1), (X2,Y2) COORDINATES WILL BE PLACED IN THE
C              FIRST AND SECOND ELEMENTS (RESPECTIVELY) OF
C              THE 2 SEGAYS PSEGXC(.,.) AND PSEGYC(.,.)
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A HOLLERITH VECTOR)
C                     --IARG   (A HOLLERITH VECTOR)
C                     --ARG    (A HOLLERITH VECTOR)
C                     --NUMARG
C                     --MAXSEG
C     OUTPUT ARGUMENTS--PSEGXC (A FLOATING POINT VECTOR
C                              WHOSE (I,1)-TH ELEMENT CONTAINS THE
C                              X COORDINATE FOR THE TAIL OF SEGMENT I;
C                              WHOSE (I,2)-TH ELEMENT CONTAINS THE
C                              X COORDINATE FOR THE HEAD OF SEGMENT I;
C                     --PSEGYC (A FLOATING POINT VECTOR
C                              WHOSE (I,1)-TH ELEMENT CONTAINS THE
C                              Y COORDINATE FOR THE TAIL OF SEGMENT I;
C                              WHOSE (I,2)-TH ELEMENT CONTAINS THE
C                              Y COORDINATE FOR THE HEAD OF SEGMENT I;
C                     --NUMSEG = THE NUMBER OF SEGMENTS DEFINED SO FAR
C                              (ACTUALLY, THE HIGHEST REFERENCED SEGMENT SO FAR)
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
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IHARG2
      CHARACTER*4 IARGT
      CHARACTER*4 IHNAME
      CHARACTER*4 IHNAM2
      CHARACTER*4 IUSE
      CHARACTER*4 IANS
      CHARACTER*4 IBUGP2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 IHWORD
      CHARACTER*4 IHWOR2
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
      DIMENSION IHARG2(*)
      DIMENSION IARGT(*)
      DIMENSION IARG(*)
      DIMENSION ARG(*)
C
      DIMENSION IHNAME(*)
      DIMENSION IHNAM2(*)
      DIMENSION IUSE(*)
      DIMENSION IN(*)
      DIMENSION IVALUE(*)
      DIMENSION VALUE(*)
      DIMENSION IANS(*)
C
      DIMENSION PSEGXC(100,2)
      DIMENSION PSEGYC(100,2)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPAR'
      ISUBN2='CO  '
      IFOUND='NO'
      IERROR='NO'
C
      HOLD1=0.0
      HOLD2=0.0
      HOLD3=0.0
      HOLD4=0.0
C
      IF(NUMARG.EQ.0)GOTO9000
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COOR')GOTO1110
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'COOR')GOTO1140
      GOTO9000
C
 1110 CONTINUE
      IF(NUMARG.LE.1)GOTO1120
      IF(IHARG(2).EQ.'ON')GOTO1120
      IF(IHARG(2).EQ.'OFF')GOTO1120
      IF(IHARG(2).EQ.'AUTO')GOTO1120
      IF(IHARG(2).EQ.'DEFA')GOTO1120
      IF(NUMARG.GE.5)GOTO1125
C
      IERROR='YES'
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1111)
 1111 FORMAT('***** ERROR IN DPSECO--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1112)
 1112 FORMAT('      IN THE SEGMENT ... COORDINATES COMMAND,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1113)
 1113 FORMAT('      THE COORDINATES ARE SPECIFIED BY 4 NUMBERS, ',
     1'AS IN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1114)
 1114 FORMAT('      SEGMENT 3 COORDINATES 30 80 31 79')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
 1120 CONTINUE
      HOLD1=CPUMIN
      HOLD2=CPUMIN
      HOLD3=CPUMIN
      HOLD4=CPUMIN
      NUMSEG=0
      GOTO1130
C
 1125 CONTINUE
      DO1126J=2,5
      IF(IARGT(J).EQ.'NUMB')GOTO1127
      GOTO1128
 1127 CONTINUE
      IF(J.EQ.2)HOLD1=ARG(J)
      IF(J.EQ.3)HOLD2=ARG(J)
      IF(J.EQ.4)HOLD3=ARG(J)
      IF(J.EQ.5)HOLD4=ARG(J)
      GOTO1126
 1128 CONTINUE
      IHWORD=IHARG(J)
      IHWOR2=IHARG2(J)
      IHWUSE='P'
      MESSAG='YES'
      CALL CHECKN(IHWORD,IHWOR2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      IF(J.EQ.2)HOLD1=VALUE(ILOC)
      IF(J.EQ.3)HOLD2=VALUE(ILOC)
      IF(J.EQ.4)HOLD3=VALUE(ILOC)
      IF(J.EQ.5)HOLD4=VALUE(ILOC)
 1126 CONTINUE
      NUMSEG=MAXSEG
      GOTO1130
C
 1130 CONTINUE
      IFOUND='YES'
      DO1135I=1,MAXSEG
      PSEGXC(I,1)=HOLD1
      PSEGYC(I,1)=HOLD2
      PSEGXC(I,2)=HOLD3
      PSEGYC(I,2)=HOLD4
 1135 CONTINUE
C
      IF(IFEEDB.EQ.'OFF')GOTO1139
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1136)
 1136 FORMAT('ALL SEGMENT COORDINATES HAVE JUST BEEN SET TO--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1137)PSEGXC(I,1),PSEGYC(I,1)
 1137 FORMAT('    (X,Y) FOR TAIL OF SEGMENT = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1138)PSEGXC(I,2),PSEGYC(I,2)
 1138 FORMAT('    (X,Y) FOR HEAD OF SEGMENT = ',2E15.7)
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
 1141 FORMAT('***** ERROR IN DPSECO--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1142)
 1142 FORMAT('      IN THE SEGMENT ... COORDINATES COMMAND,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1143)
 1143 FORMAT('      THE SEGMENT IS IDENTIFIED BY A NUMBER, AS IN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1144)
 1144 FORMAT('      SEGMENT 3 COORDINATES 30 80 31 79')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
 1150 CONTINUE
      I=IARG(1)
      IF(1.LE.I.AND.I.LE.MAXSEG)GOTO1160
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1151)
 1151 FORMAT('***** ERROR IN DPSECO--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1152)
 1152 FORMAT('      IN THE SEGMENT ... COORDINATES COMMAND,')
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
 1156 FORMAT('      A REFERENCE WAS MADE TO THE ',I8,'-TH ',
     1'SEGMENT.')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
 1160 CONTINUE
      IF(NUMARG.LE.2)GOTO1170
      IF(IHARG(3).EQ.'ON')GOTO1170
      IF(IHARG(3).EQ.'OFF')GOTO1170
      IF(IHARG(3).EQ.'AUTO')GOTO1170
      IF(IHARG(3).EQ.'DEFA')GOTO1170
      IF(NUMARG.GE.6)GOTO1175
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1112)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1113)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1114)
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
 1170 CONTINUE
      HOLD1=CPUMIN
      HOLD2=CPUMIN
      HOLD3=CPUMIN
      HOLD4=CPUMIN
      IF(I.EQ.NUMSEG)NUMSEG=I-1
      GOTO1180
C
 1175 CONTINUE
      DO1176J=3,6
      IF(IARGT(J).EQ.'NUMB')GOTO1177
      GOTO1178
 1177 CONTINUE
      IF(J.EQ.3)HOLD1=ARG(J)
      IF(J.EQ.4)HOLD2=ARG(J)
      IF(J.EQ.5)HOLD3=ARG(J)
      IF(J.EQ.6)HOLD4=ARG(J)
      GOTO1176
 1178 CONTINUE
      IHWORD=IHARG(J)
      IHWOR2=IHARG2(J)
      IHWUSE='P'
      MESSAG='YES'
      CALL CHECKN(IHWORD,IHWOR2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      IF(J.EQ.3)HOLD1=VALUE(ILOC)
      IF(J.EQ.4)HOLD2=VALUE(ILOC)
      IF(J.EQ.5)HOLD3=VALUE(ILOC)
      IF(J.EQ.6)HOLD4=VALUE(ILOC)
 1176 CONTINUE
      IF(I.GT.NUMSEG)NUMSEG=I
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      PSEGXC(I,1)=HOLD1
      PSEGYC(I,1)=HOLD2
      PSEGXC(I,2)=HOLD3
      PSEGYC(I,2)=HOLD4
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1186)I
 1186 FORMAT('THE COORDINATES FOR SEGMENT ',I8,
     1' HAVE JUST BEEN SET TO--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1137)PSEGXC(I,1),PSEGYC(I,1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1138)PSEGXC(I,2),PSEGYC(I,2)
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
 9011 FORMAT('***** AT THE END       OF DPSECO--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IFOUND,IERROR
 9012 FORMAT('IFOUND,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPSEED(IHARG,IARGT,IARG,NUMARG,IDEFSE,
     1ISEED,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE SEED (AN INTEGER)
C              WHICH IS USED AS INPUT IN UNIFORM RANDOM NUMBER GENERATION AND
C              WHICH IN TURN SERVES AS THE BASIS FOR ALL RANDOM NUMBER GENERATIO
C              THE SPECIFIED SEED VALUE WILL BE PLACED
C              IN THE INTEGER VARIABLE ISEED.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --ARG    (A  FLOATING POINT VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C                     --IDEFSE (A  FLOATING POINT VARIABLE)
C     OUTPUT ARGUMENTS--ISEED  (AN INTEGER VARIABLE)
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
C     ORIGINAL VERSION--APRIL     1982.
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
      IF(NUMARG.LE.0)GOTO1150
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      IF(IARGT(NUMARG).EQ.'NUMB')GOTO1160
      GOTO1120
C
 1120 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1121)
 1121 FORMAT('***** ERROR IN DPSEED--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR SEED ',
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
 1126 FORMAT('      SEED VALUE FOR RANDOM NUMBER GENERATION')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1127)
 1127 FORMAT('      TO BE 735679238,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1129)
 1129 FORMAT('      THEN THE ALLOWABLE FORM IS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1130)
 1130 FORMAT('      SEED 735679238 ')
      CALL DPWRST('XXX','BUG ')
      GOTO1199
C
 1150 CONTINUE
      IHOLD=IDEFSE
      GOTO1180
C
 1160 CONTINUE
      IHOLD=IARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      ISEED=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)ISEED
 1181 FORMAT('THE RANDOM NUMBER SEED HAS JUST BEEN SET TO ',
     1I11)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END

      SUBROUTINE DPRCSN(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1                  IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN COMPLEX SCRIPT NUMERIC.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
C---------------------------------------------------------------------
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
   51 FORMAT('***** AT THE BEGINNING OF DPRCSN--')
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
      IF(ICHARN.LE.9)GOTO1010
      GOTO1019
 1010 CONTINUE
      CALL DRCSN1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1019 CONTINUE
C
      IF(ICHARN.GE.10)GOTO1020
      GOTO1029
 1020 CONTINUE
      CALL DRCSN2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
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
 9011 FORMAT('***** AT THE END       OF DPRCSN--')
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
      SUBROUTINE DPRCSU(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN COMPLEX SCRIPT UPPER CASE.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
C---------------------------------------------------------------------
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
   51 FORMAT('***** AT THE BEGINNING OF DPRCSU--')
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
      CALL DRCSU1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1019 CONTINUE
C
      IF(7.LE.ICHARN.AND.ICHARN.LE.13)GOTO1020
      GOTO1029
 1020 CONTINUE
      CALL DRCSU2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1029 CONTINUE
C
      IF(14.LE.ICHARN.AND.ICHARN.LE.20)GOTO1030
      GOTO1039
 1030 CONTINUE
      CALL DRCSU3(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1039 CONTINUE
C
      IF(ICHARN.GE.21)GOTO1040
      GOTO1049
 1040 CONTINUE
      CALL DRCSU4(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
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
 9011 FORMAT('***** AT THE END       OF DPRCSU--')
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
      SUBROUTINE DPRCU(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN COMPLEX UPPER CASE.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
C---------------------------------------------------------------------
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
   51 FORMAT('***** AT THE BEGINNING OF DPRCU--')
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
      IF(ICHARN.LE.14)GOTO1010
      GOTO1019
 1010 CONTINUE
      CALL DRCU1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1019 CONTINUE
C
      IF(ICHARN.GE.15)GOTO1020
      GOTO1029
 1020 CONTINUE
      CALL DRCU2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
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
 9011 FORMAT('***** AT THE END       OF DPRCU--')
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
      SUBROUTINE DPRDL(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN DUPLEX LOWER CASE.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
C---------------------------------------------------------------------
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
   51 FORMAT('***** AT THE BEGINNING OF DPRDL--')
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
      IF(ICHARN.LE.11)GOTO1010
      GOTO1019
 1010 CONTINUE
      CALL DRDL1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1019 CONTINUE
C
      IF(12.LE.ICHARN.AND.ICHARN.LE.24)GOTO1020
      GOTO1029
 1020 CONTINUE
      CALL DRDL2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1029 CONTINUE
C
      IF(ICHARN.GE.25)GOTO1030
      GOTO1039
 1030 CONTINUE
      CALL DRDL3(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1039 CONTINUE
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
 9011 FORMAT('***** AT THE END       OF DPRDL--')
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
      SUBROUTINE DPRDN(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN DUPLEX NUMERIC.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
C---------------------------------------------------------------------
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
   51 FORMAT('***** AT THE BEGINNING OF DPRDN--')
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
      IF(ICHARN.LE.8)GOTO1010
      GOTO1019
 1010 CONTINUE
      CALL DRDN1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1019 CONTINUE
C
      IF(ICHARN.GE.9)GOTO1020
      GOTO1029
 1020 CONTINUE
      CALL DRDN2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
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
 9011 FORMAT('***** AT THE END       OF DPRDN--')
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
      SUBROUTINE DPRDS(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN DUPLEX SYMBOLS.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
C---------------------------------------------------------------------
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
   51 FORMAT('***** AT THE BEGINNING OF DPRDS--')
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
      IF(ICHARN.LE.9)GOTO1010
      GOTO1019
 1010 CONTINUE
      CALL DRDS1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1019 CONTINUE
C
      IF(ICHARN.GE.10)GOTO1020
      GOTO1029
 1020 CONTINUE
      CALL DRDS2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
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
 9011 FORMAT('***** AT THE END       OF DPRDS--')
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
      SUBROUTINE DPRDU(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN DUPLEX UPPER CASE.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
C---------------------------------------------------------------------
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
   51 FORMAT('***** AT THE BEGINNING OF DPRDU--')
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
      IF(ICHARN.LE.14)GOTO1010
      GOTO1019
 1010 CONTINUE
      CALL DRDU1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1019 CONTINUE
C
      IF(ICHARN.GE.15)GOTO1020
      GOTO1029
 1020 CONTINUE
      CALL DRDU2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
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
 9011 FORMAT('***** AT THE END       OF DPRDU--')
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
      SUBROUTINE DPREAD(IFROW1,IFROW2,IFCOL1,IFCOL2,ISKIP,INTINF,
     1                  IMACRO,IMACNU,IMACCS,IMALEV,IOSW,ICREAF,NCREAF,
     1                  IREARW,ICOMCH,ICOMSW,
     1                  IUNFOF,IUNFNR,IUNFMC,NUMRCM,
     1                  IFCOLL,IFCOLU,
     1                  IANSLO,ILOOST,ILOOLI,IREPCH,
     1                  IBUGS2,IBUGQ,ISUBRO,IFOUND,IERROR)
CCCCC MAY 1990.  ADD ICOMCH, ICOMSW TO CALL LIST
CCCCC APRIL, 1995.  ADD IUNFOF, IYNFNR, IUNFMC TO CALL LIST
CCCCC MARCH, 1996.  ADD IMALEV TO CALL LIST
CCCCC FEBRUARY 2003.  ADD NUMRCM TO CALL LIST
CCCCC JANUARY 2015.  ADD "LOOP" ARGUMENTS
C
C     PURPOSE--READ IN THE VALUES OF A VARIABLE.  THE DATA IS LISTED
C              ACROSS A LINE IMAGE.  (E.G., X(1) Y(1) Z(1) ETC.)
C              THE DATA IS READ FORM A MASS STORAGE FILE
C              OR (IF NO FILE GIVEN) FROM THE DEFAULT INPUT UNIT
C              (WHICH WILL BE THE TERMINAL).
C     ASSUMPTION--THE INPUT  FILE ALREADY EXISTS; (THAT IS, DATAPLOT
C                 WILL AUTOMATICALLY OPEN THE FILE
C                 VIA (ON THE UNIVAC 1108), BY AN @ASG,AX ...)
C                 BUT WILL NOT AUTOMATICALLY CREATE THE FILE
C                 VIA (ON THE UNIVAC 1108), BY AN @ASG,UP ...))
C     ASSUMPTION--THE COMPUTER SYSTEM IS SUCH THAT EQUATING THE FILE NAME
C                 TO THE FORTRAN NUMERIC DESIGNATION OF 31 (OR HOWEVER
C                 THE VARIABLE  IREANU  IS DEFINED IN INITFO) IS
C                 PERMISSIBLE.
C     NOTE--INPUT FOR THE READ COMMAND MAY POTENTIALLY
C           COME FROM 2 DIFFERENT SOURCES--
C                1) THE TERMINAL ITSELF;
C                2) A FILE;
C           DIFFERENT SYSTEMS ALLOW DIFFERENT COMBINATIONS OF THE ABOVE.
C           ALL SYSTEMS WILL ALLOW INPUT FROM THER TERMINAL ITSELF;
C           MOST SYSTEMS WILL ALLOW INPUT FROM A FILE;
C     NOTE--ICOMCH = THE ALLOWABLE COMMENT CHARACTER
C           ICOMSW = THE COMMENT CHARACTER FLAG/SWITCH (ON/OFF)
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--DECEMBER  1977.
C     UPDATED         --JANUARY   1978.
C     UPDATED         --FEBRUARY  1978.
C     UPDATED         --MAY       1978.
C     UPDATED         --JULY      1978.
C     UPDATED         --NOVEMBER  1978.
C     UPDATED         --NOVEMBER  1980.
C     UPDATED         --JANUARY   1981.
C     UPDATED         --JUNE      1981.
C     UPDATED         --NOVEMBER  1981.
C     UPDATED         --JANUARY   1982.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --DECEMBER  1985.
C     UPDATED         --SEPTEMBER 1987. (READ MATRIX)
C     UPDATED         --FEBRUARY  1988. DEACT. COL. LIM. IF READ NON-FILE)
C     UPDATED         --JUNE      1988. (CORRECT DOUBLE ENTRY BY READ FUNCT
C     UPDATED         --DECEMBER  1988. CORRECT BOMB ON 2ND   READ PARAMETER
C     UPDATED         --MAY       1989. FIX IRIS PROBLEM--LOOP MAX & CPUMAX
C     UPDATED         --MAY       1990. CHECK FOR COMMENT CHARACTER (UNIX)
C     UPDATED         --MAY       1990. ERROR CHECK FOR FORMATTED READ
C     UPDATED         --JUNE      1990. FIX FORMATTED READ/1 LINE BUG
C     UPDATED         --JULY      1990. UPDATED WRITE/FORMAT STATEMENT
C     UPDATED         --JULY      1990. BUG/TRACE PRINT OF ICOMCH/FL
C     UPDATED         --JULY      1990. COMMENT CHECK BUG FIXED
C     UPDATED         --JULY      1990. RENAME ICOMFL TO ICOMSW
C     UPDATED         --JULY      1993. FIX MATRIX READ (ALAN)
C     UPDATED         --JULY      1993. FIX BOMB IF GOOD READ AFTER
C                                       READ NON-EXISTENT FILE
C     UPDATED         --MARCH     1994. FIX BUG WHERE DELETE AND 
C                                       RETAIN WIPED OUT PARAMETERS
C                                       CREATED VIA READ PARAMETER
C     UPDATED         --APRIL     1995. SUPPORT FOR UNFORMATTED READ
C     UPDATED         --SEPTEMBER 1995. ROW LIMITS & BLANK LINES PROBLEM
C     UPDATED         --MARCH     1996. FIX BUG WHERE TERMINAL READ
C                                       NESTED WITHIN A MACRO
C     UPDATED         --APRIL     1996. FOR READ STRING, IGNORE SET
C                                       READ FORMAT
C     UPDATED         --OCTOBER   1997. SUPPORT "SKIP AUTOMATIC",
C                                       READ UNTIL FIND "----"
C     UPDATED         --NOVEMBER  1998. READ MORE THAN 100 VARIABLES
C                                       (MAKE PARAMETER SETTABLE)
C     UPDATED         --DECEMBER  1999. READ ROWID
C     UPDATED         --MARCH     2001. FIX BUGS:
C                                       A) UPDATE LIMIT ON MAX COLUMNS
C                                       B) OFFSET FOR UNFORMATTED READ
C                                       C) MAX FOR ROW LIMITS
C     UPDATED         --JULY      2002. SUPPORT FOR QUOTES ON
C                                       FILE NAMES.
C     UPDATED         --FEBRUARY  2003. UP MAXIMUM NUMBER OF
C                                       CHARACTERS READ FROM ONE
C                                       RECORD OF DATA FILE (MAKE
C                                       SETTABLE TO PARAMETER)
C     UPDATED         --FEBRUARY  2003. AUTOMATICALLY DETERMINE
C                                       NUMBER OF VARIABLES IF NO
C                                       LIST GIVEN.
C     UPDATED         --JUNE      2003. HANDLE HYPHENS INSIDE OF QUOTED
C                                       FILE NAMES CORRECTLY.
C     UPDATED         --JULY      2003. BUG WHEN FILE NAME < 80
C                                       CHARACTERS, BUT COMMAND LINE
C                                       > 80 CHARACTERS 
C     UPDATED         --AUGUST    2003. QUOTES ON FILE NAMES
C                                       AUTOMATIC FOR READ
C     UPDATED         --JANUARY   2004. IF AUTOMATICALLY DETERMINE
C                                       VARIABLE LIST, CHECK FIRST
C                                       LINE FOR VARIABLE LIST
C     UPDATED         --JANUARY   2004. SOME RECODING FOR BETTER
C                                       CLARITY
C     UPDATED         --JANUARY   2004. HANDLE CHARACTER DATA
C     UPDATED         --OCTOBER   2004. WHEN READING VARIABLES, IF
C                                       NUMBER OF ITEMS IS GREATER
C                                       THAN NUMBER OF ITEMS READ,
C                                       PAD WITH "MISSING VALUE"
C                                       (BASED ON VALUE OF IREAPD)
C     UPDATED         --OCTOBER   2004. SET READ SUBSET
C                                       <PACK/DISPERSE>  <PACK/DISPERSE>
C     UPDATED         --DECEMBER  2004. IF GUI RUNNING (SET GUI), THEN
C                                       DO NOT ALLOW TERMINAL READ
C     UPDATED         --DECEMBER  2007. > 100 COLUMNS FOR MATRIX
C     UPDATED         --MARCH     2008. ADD: 
C                                       READ MATRIX TO VARIABLE FILE.DAT
C                                       Z ROWID COLID
C     UPDATED         --MARCH     2008. ADD:
C                                       READ STACKED VARIABLE FILE.DAT
C                                       Z GROUPID  <VARI-LIST>
C     UPDATED         --MARCH     2008. ADD: 
C                                       READ IMAGE TO VARIABLE FILE.DAT
C                                       Z ROWID COLID
C                                       READ IMAGE TO VARIABLE FILE.DAT
C                                       RED BLUE GREEN ROWID COLID
C     UPDATED         --APRIL     2009. ADD "IDATMV" TO DPREAL CALL
C     UPDATED         --APRIL     2009. WHEN READING IMAGES, CHECK
C                                       FOR DATAPLOT DIRECTORIES TO
C                                       MATCH FILE NAME
C     UPDATED         --JULY      2009. ALLOW "Y1 TO Y1" (USEFUL FOR
C                                       MACROS WHERE THE NUMBER OF
C                                       VARIABLES NOT KNOWN IN ADVANCE)
C     UPDATED         --JULY      2014. ADDITIONAL IMAGE TYPES FROM
C                                       GD LIBRARY (BMP, WBMP, WEBP,
C                                       TGA, TIF, XPM)
C     UPDATED         --OCTOBER   2014. SOME TWEAKS FOR CASE WHEN NO
C                                       VARIABLE NAMES GIVEN ON READ
C                                       COMMAND
C     UPDATED         --NOVEMBER  2014. READ FROM SYSTEM CLIPBOARD
C                                       (OS/COMPILER DEPENDENT)
C                                       SUPPORTED FOR READING A LIST OF
C                                       VARIABLES OR FOR READING A
C                                       STRING
C     UPDATED         --JANUARY   2015. IF HAVE READ FROM TERMINAL WHILE
C                                       IN LOOP, READ FROM SAVED LOOP
C                                       COMMANDS RATHER THAN STANDARD
C                                       INPUT (OR MACRO FILE)
C     UPDATED         --MARCH     2015. CALL LIST TO DPINFU
C     UPDATED         --JUNE      2016. CALL LIST TO DPREAL
C     UPDATED         --MARCH     2017. CHECK FOR "," WHEN READING
C                                       VARIABLE NAMES FROM FIRST LINE
C     UPDATED         --JUNE      2018. CORRECT HANDLING OF CHARACTER
C                                       DATA WITH " TO " SYNTAX
C     UPDATED         --JUNE      2018. IF ERROR ENCOUNTERED IN DPREAL,
C                                       STOP PROCESSING
C     UPDATED         --JUNE      2018. SET CONVERT CHARACTER
C                                           CATEGORICAL
C                                       (AUTOMATICALLY CONVERT
C                                       CHARACTER DATA TO NUMERIC
C                                       CATEGORICAL VARIABLE)
C     UPDATED         --SEPTEMBER 2018. ROW READ OPTION
C     UPDATED         --DECEMBER  2018. READ1/READ2/READ3 OPTIONS
C     UPDATED         --APRIL     2019. SET READ ASTERISK IGNORE
C     UPDATED         --JUNE      2019. RAISED MAXIMUM NUMBER OF
C                                       CHARACTER VARIABLES TO 50
C     UPDATED         --JUNE      2019. INITIALIZE IRWLC3 TO 0
C     UPDATED         --JULY      2019. TWEAK SCRATCH SPACE
C     UPDATED         --SEPTEMBER 2019. ALLOWS CHARACTER VARIABLES FROM
C                                       TERMINAL READ
C     UPDATED         --OCTOBER   2019. IF FILE EXTENSION IS ".csv" OR
C                                       ".CSV", AUTOMATICALLY SET READ
C                                       DELIMITER TO ","
C     UPDATED         --FEBRUARY  2020. READ EXCEL OPTION (THIS WILL USE
C                                       PYTHON (Pandas) TO READ THE
C                                       EXCEL FILE TO "dpst1f.dat"),
C                                       READ COMMAND WILL THEN READ
C                                       "dpst1f.dat".
C     UPDATED         --FEBRUARY  2020. FOR "READ CLIPBOARD", CHECK IF
C                                       "CLIPBOARD" ARGUMENT IS ACTUALLY
C                                       A FILE NAME
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IMACRO
      CHARACTER*12 IMACCS
      CHARACTER*4 ILOOST
      CHARACTER*1 IREPCH
C
      CHARACTER*80 ICREAF
C
      CHARACTER*4 IOSW
      CHARACTER*4 IREARW
      CHARACTER*4 IGRPA2
      CHARACTER*4 ICFLAG
      CHARACTER*4 IBUGS2
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 ICASEQ
      CHARACTER*4 ICASEA
      CHARACTER*4 IEND
      CHARACTER*4 IH1
      CHARACTER*4 IH2
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IFMFLG
      CHARACTER*4 ICRFLG
C
      CHARACTER*4 ICASRE
      CHARACTER*4 ICASR2
      CHARACTER*4 ICASR3
      CHARACTER*4 ICASR4
      CHARACTER*4 IOFILE
      CHARACTER*4 IOTERM
      CHARACTER*4 IREAD2
      CHARACTER*4 IFILQ2
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
      CHARACTER*9999 ISTR
      CHARACTER*20 IFRMT
      CHARACTER*20 IFRMT2
      CHARACTER*20 IFRMT3
      CHARACTER*4 IOP
      CHARACTER*4 IOPEN
      CHARACTER*8 IACC
C
CCCCC CHARACTER*80 IFILE1
      CHARACTER (LEN=MAXFNC) :: IFILE1
      CHARACTER*12 ISTAT1
      CHARACTER*12 IFORM1
      CHARACTER*12 IACCE1
      CHARACTER*12 IPROT1
      CHARACTER*12 ICURS1
      CHARACTER*4 IERRF1
      CHARACTER*4 IENDF1
      CHARACTER*4 IREWI1
C
CCCCC CHARACTER*80 IFILE2
      CHARACTER (LEN=MAXFNC) :: IFILE2
      CHARACTER*12 ISTAT2
      CHARACTER*12 IFORM2
      CHARACTER*12 IACCE2
      CHARACTER*12 IPROT2
      CHARACTER*12 ICURS2
      CHARACTER*4 IERRF2
      CHARACTER*4 IENDF2
      CHARACTER*4 IREWI2
C
CCCCC CHARACTER*80 IFILE3
      CHARACTER (LEN=MAXFNC) :: IFILE3
      CHARACTER*12 ISTAT3
      CHARACTER*12 IFORM3
      CHARACTER*12 IACCE3
      CHARACTER*12 IPROT3
      CHARACTER*12 ICURS3
      CHARACTER*4 IERRF3
      CHARACTER*4 IENDF3
      CHARACTER*4 IREWI3
C
CCCCC CHARACTER*80 IFILE4
      CHARACTER (LEN=MAXFNC) :: IFILE4
      CHARACTER*12 ISTAT4
      CHARACTER*12 IFORM4
      CHARACTER*12 IACCE4
      CHARACTER*12 IPROT4
      CHARACTER*12 ICURS4
      CHARACTER*4 IERRF4
      CHARACTER*4 IENDF4
      CHARACTER*4 IREWI4
C
CCCCC CHARACTER*80 IFILE5
      CHARACTER (LEN=MAXFNC) :: IFILE5
      CHARACTER*12 ISTAT5
      CHARACTER*12 IFORM5
      CHARACTER*12 IACCE5
      CHARACTER*12 IPROT5
      CHARACTER*12 ICURS5
      CHARACTER*4 IERRF5
      CHARACTER*4 IENDF5
      CHARACTER*4 IREWI5
C
      COMMON/FILTMP/IFILE1, ISTAT1, IFORM1, IACCE1, IPROT1, ICURS1,
     1              IERRF1, IENDF1, IREWI1,
     1              IFILE2, ISTAT2, IFORM2, IACCE2, IPROT2, ICURS2,
     1              IERRF2, IENDF2, IREWI2,
     1              IFILE3, ISTAT3, IFORM3, IACCE3, IPROT3, ICURS3,
     1              IERRF3, IENDF3, IREWI3,
     1              IFILE4, ISTAT4, IFORM4, IACCE4, IPROT4, ICURS4,
     1              IERRF4, IENDF4, IREWI4,
     1              IFILE5, ISTAT5, IFORM5, IACCE5, IPROT5, ICURS5,
     1              IERRF5, IENDF5, IREWI5
C
CCCCC CHARACTER*80 FTEMP
      CHARACTER (LEN=MAXFNC) :: FTEMP
C
      CHARACTER*4 ISTRZ2(8)
C
CCCCC CHARACTER*255 ICANS
      CHARACTER (LEN=MAXSTR) :: ICANS
      CHARACTER*4 IHLEFT
      CHARACTER*4 IHLEF2
      CHARACTER*4 NEWNAM
      CHARACTER*8 IVBASE
      CHARACTER*8 IVBASV
      CHARACTER*8 IVTEMP
      CHARACTER*4 IRTYPE
CCCCC CHARACTER*255 ISTRZZ
      CHARACTER (LEN=MAXSTR) :: ISTRZZ
C
      CHARACTER*4 ICASTO
      CHARACTER*4 IHMAT1
      CHARACTER*4 IHMAT2
      CHARACTER*80 IAJUNK
      CHARACTER*4 ICOMCH
      CHARACTER*4 ICOMSW
      CHARACTER*4 LINETY
      CHARACTER*4 IEXIST
      CHARACTER*4 IEXCEL
      CHARACTER*4 ITYPEZ
      CHARACTER*80 ISNAME
      CHARACTER*80 ISARGL
C
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZI.INC'
      INCLUDE 'DPCOZC.INC'
C
CCCCC NOVEMBER 1998.  DEFINE MAXRDV TO DEFINE MAXIMUM NUMBER OF
CCCCC VARIABLES.
C
CCCCC MARCH 2001.  UP LIMIT (MATRICES IN PARTICULAR CAN HAVE MORE)
CCCCC PARAMETER(MAXRDV=250)
CCCCC PARAMETER(MAXRDV=1000)
      PARAMETER(MAXRDV=2048)
      PARAMETER(MAXCHV=50)
C
      INTEGER IADE(200)
      INTEGER IFCOLL(*)
      INTEGER IFCOLU(*)
      INTEGER ITYPE(MAXRDV)
      INTEGER NIV(MAXRDV)
      INTEGER IEN(MAXRDV)
      INTEGER IECOL2(MAXRDV)
      INTEGER IFSTA2(MAXRDV)
      INTEGER IFSTO2(MAXRDV)
      INTEGER IXCATN(MAXCHV)
      INTEGER IECOLC(MAXCHV)
      INTEGER IENC(MAXCHV)
      DIMENSION X0CAT(MAXCHV)
      DIMENSION PVAL(MAXRDV)
C
CCCCC THE FOLLOWING LINES ADDED    FEBRUARY  2003.
C
      CHARACTER*4 IVRLST
      CHARACTER*4 IECASE(MAXRDV)
      CHARACTER*4 IVLIST(MAXRDV)
      CHARACTER*4 IVLIS2(MAXRDV)
      CHARACTER*4 IASAVE(MAXRDV)
      CHARACTER*4 ICLIST(MAXRDV)
      CHARACTER*4 ICLIS2(MAXRDV)
C
      CHARACTER*4 JVNAM1(MAXRDV)
      CHARACTER*4 JPNAM1(MAXRDV)
      CHARACTER*4 JMNAM1(MAXRDV)
      CHARACTER*4 JFNAM1(MAXRDV)
      CHARACTER*4 JUNAM1(MAXRDV)
      CHARACTER*4 JENAM1(MAXRDV)
C
      CHARACTER*4 JVNAM2(MAXRDV)
      CHARACTER*4 JPNAM2(MAXRDV)
      CHARACTER*4 JMNAM2(MAXRDV)
      CHARACTER*4 JFNAM2(MAXRDV)
      CHARACTER*4 JUNAM2(MAXRDV)
      CHARACTER*4 JENAM2(MAXRDV)
C
      CHARACTER*24 IXC(MAXCHV)
      CHARACTER*24 IXCAT(1000,MAXCHV)
      CHARACTER*4 ISTOR1(MAXRCL)
      CHARACTER*4 ISTOR2(MAXRCL)
      CHARACTER*4 ISTOR3(MAXRCL)
      CHARACTER*4 IB(MAXRCL)
C
      CHARACTER*4 IANSLO(MAXCIL,MAXLIL)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOFO.INC'
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOST.INC'
CCCCC MARCH 2001.  ADD FOLLOWING LINE
      INCLUDE 'DPCOMC.INC'
C
      DIMENSION XSCRT(3*MAXOBW)
      DIMENSION X0(MAXRDV)
C
      EQUIVALENCE (GARBAG(IGARB1),X0(1))
      EQUIVALENCE (GARBAG(IGARB2),X0CAT(1))
      EQUIVALENCE (GARBAG(IGARB3),XSCRT(1))
C
      EQUIVALENCE (IGARBG(IIGAR1),ITYPE(1))
      EQUIVALENCE (IGARBG(IIGAR1+1000),NIV(1))
      EQUIVALENCE (IGARBG(IIGAR1+3000),IEN(1))
      EQUIVALENCE (IGARBG(IIGAR1+5000),IECOL2(1))
      EQUIVALENCE (IGARBG(IIGAR1+7000),IFSTA2(1))
      EQUIVALENCE (IGARBG(IIGAR1+9000),IFSTO2(1))
      EQUIVALENCE (IGARBG(IIGAR1+11000),IADE(1))
      EQUIVALENCE (IGARBG(IIGAR1+13000),IECOLC(1))
      EQUIVALENCE (IGARBG(IIGAR1+15000),IENC(1))
C
      EQUIVALENCE (CGARBG(1),IECASE(1))
      EQUIVALENCE (CGARBG(20000),IVLIST(1))
      EQUIVALENCE (CGARBG(40000),IVLIS2(1))
      EQUIVALENCE (CGARBG(60000),IASAVE(1))
      EQUIVALENCE (CGARBG(80000),ICLIST(1))
      EQUIVALENCE (CGARBG(100000),ICLIS2(1))
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
      EQUIVALENCE (CGARBG(800000),IXCAT(1,1))
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPRE'
      ISUBN2='AD  '
      IFOUND='YES'
      IERROR='NO'
      ICASRE='-999'
      ICASR2='-999'
      IOFILE='-999'
      IOTERM='-999'
      IFILQ2=IFILQU
      IFILQU='ON'
      IREAD2=IREADL
      IEXCEL='OFF'
      IVBASV=' '
C
      ICASR3='0'
      IF(ICOM2.EQ.'1   ')ICASR3='1'
      IF(ICOM2.EQ.'2   ')ICASR3='2'
      IF(ICOM2.EQ.'3   ')ICASR3='3'
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
      IMNVAR=-1
      IMXVAR=-1
      IFLGSV=0
      ISKPSV=ISKIP
      NUMDSV=0
      INEXT=0
      ICOL=0
      J=0
      JM1=0
      ILINE=0
      ILAST=0
      IRWLC2=0
      NXCSAV=0
      ICNTCH=0
      IERR=0
      IXSIZE=0
      IYSIZE=0
      IRED=0
      IGREEN=0
      IBLUE=0
C
CCCCC FEBRUARY 2003: ADD FOLLOWING LINE.
CCCCC IF NO VARIABLE LIST GIVEN, THEN TWO CASES:
CCCCC    1) IF SKIP AUTOMATIC ON, THEN READ PREVIOUS LINE TO
CCCCC       DETERMINE VARIABLE LIST. 
CCCCC    2) IF SKIP AUTOMATIC OFF, THEN READ FIRST LINE TO
CCCCC       DETERMINE NUMBER OF VARIABLES.  NAME THEM X1, X2, ETC.
C
      IVRLST='YES'
      DO15I=1,MAXRDV
        IASAVE(I)='    '
        IVLIST(I)='    '
        IVLIS2(I)='    '
        ITYPE(I)=0
        JVNAM1(I)='    '
        JVNAM2(I)='    '
        JPNAM1(I)='    '
        JPNAM2(I)='    '
        JMNAM1(I)='    '
        JMNAM2(I)='    '
        JFNAM1(I)='    '
        JFNAM2(I)='    '
        JUNAM1(I)='    '
        JUNAM2(I)='    '
        JENAM1(I)='    '
        JENAM2(I)='    '
   15 CONTINUE
      DO13I=1,MAXCHV
        IXC(I)=' '
        ICLIST(I)=' '
        ICLIS2(I)=' '
        IECOLC(I)=0
        IENC(I)=0
        DO14J=1,1000
          IXCAT(J,I)=' '
   14   CONTINUE
        IXCATN(I)=0
        X0CAT(I)=0.0
   13 CONTINUE
      IGRPA2=IGRPAU
C
CCCCC THE FOLLOWING LINE WAS INSERTED MAY 1989
CCCCC MARCH 2001.  SET VALUE TO MAX INTEGER
CCCCC IBILLI=10**9
      IBILLI=I1MACH(9)
      I2=0
      NUMVRD=0
      NUMPRD=0
      NUMFRD=0
      MAXN2=MAXCHF
      AFROW2=IFROW2
      IMATC1=(-999)
      IMATNR=(-999)
      IMATNC=(-999)
      LINETY='-999'
      NCALL=0
      NCOLS=0
      NROWZ=0
      NCOLZ=0
      ITOTZ=0
      IMAGFL=-99
      IMAGTY=-99
      IMAGCO=1
      IMAGSH=0
      IRWLC3=0
C
C               ***************************
C               **  TREAT THE READ CASE  **
C               ***************************
C
CCCCC NOVEMBER 1998.  DEFINE MAXRDV TO DEFINE MAXIMUM NUMBER OF
CCCCC VARIABLES.
C
      MAXV2=MAXRDV
      MAXP2=MAXRDV
      MAXM2=MAXRDV
      MAXF2=MAXRDV
      MAXU2=MAXRDV
      MAXE2=MAXRDV
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPREAD--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IFROW1,AFROW2,IFCOL1,IFCOL2,NUMRCM
   52   FORMAT('IFROW1,AFROW2,IFCOL1,IFCOL2,NUMRCM = ',I8,2X,E15.7,3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)IRD,IRD2,ISKIP,IBUGS2,IBUGQ,IOSW
   54   FORMAT('IRD,IRD2,ISKIP,IBUGS2,IBUGQ,IOSW = ',3I8,2X,2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,56)IMACRO,IMACNU,IMACCS
   56   FORMAT('IMACRO,IMACNU,IMACCS = ',A4,I8,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,63)IBUGS2,ISUBRO,IERROR,ICASR3,IWIDTH
   63   FORMAT('IBUGS2,ISUBRO,IERROR,ICASR3,IWIDTH = ',4(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,65)(IANSLC(I),I=1,MIN(100,IWIDTH))
   65     FORMAT('(IANSLC(I),I=1,IWIDTH) = ',100A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        WRITE(ICOUT,72)IREANA(1:80)
   72   FORMAT('IREANA = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,73)IREANU,IREAST,IREAFO,IREAAC,IREAFO,IREACS
   73   FORMAT('IREANU,IREAST,IREAFO,IREAAC,IREAFO,IREACS = ',
     1         I8,5(1X,A12))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,82)NUMNAM,N2,MAXN2,NCREAF
   82   FORMAT('NUMNAM,N2,MAXN2,NCREAF = ',4I8)
        CALL DPWRST('XXX','BUG ')
        IF(NCREAF.GE.1)THEN
          WRITE(ICOUT,85)(ICREAF(I:I),I=1,NCREAF)
   85     FORMAT('(ICREAF(I:I),I=1,NCREAF) = ',80A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        WRITE(ICOUT,87)IREARW,ICOMCH,ICOMSW
   87   FORMAT('IREARW,ICOMCH,ICOMSW = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *******************************************************
C               **  STEP 1--                                         **
C               **  CHECK FOR THE PROPER NUMBER OF INPUT ARGUMENTS.  **
C               *******************************************************
C
      ISTEPN='1'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LT.1)THEN
        IERROR='YES'
        GOTO8800
      ENDIF
C
C               *****************************************
C               **  STEP 1B--                          **
C               **  DETERMINE THE TYPE OF READ CASE--  **
C               **     1) VARIABLE                     **
C               **     2) PARAMETER                    **
C               **     3) FUNCTION (= STRING)          **
C               **     4) MATRIX                       **
C               **     5) MATRIX TO VARIABLE           **
C               **     6) STACKED VARIABLE             **
C               **     7) IMAGE                        **
C               **     8) IMAGE TO VARIABLE            **
C               **     9) CLIPBOARD                    **
C               **    10) STRING CLIPBOARD             **
C               **    11) ROW READ                     **
C               **    12) EXCEL                        **
C               *****************************************
C
      ICASRE='VARI'
      IF(ICASR3.NE.'0')THEN
        IF(IHARG(1).EQ.'STRI')THEN
          ICASR4='STRI'
          ICASRE='LINE'
        ELSEIF(IHARG(1).EQ.'NUME')THEN
          ICASR4='NUME'
          ICASRE='LINE'
        ELSE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,211)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)ICASR3
  101     FORMAT('       FOR READ',A1,' CASE, THE FIRST ARGUMENT')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,103)
  103     FORMAT('       MUST BE EITHER   STRING   OR   ',
     1           'NUMERIC   .')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,105)IHARG(1)
  105     FORMAT('       THE FIRST ARGUMENT IS ',A4)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
        GOTO150
      ELSEIF(IHARG(1).EQ.'PARA'.AND.IHARG2(1).EQ.'METE')THEN
        ICASRE='PARA'
      ELSEIF(IHARG(1).EQ.'FUNC'.AND.IHARG2(1).EQ.'TION' .AND.
     1       IHARG(2).EQ.'CLIP' .AND. IHARG2(2).EQ.'BOAR')THEN
        ICASRE='CFUN'
      ELSEIF(IHARG(1).EQ.'STRI'.AND.IHARG2(1).EQ.'NG  ' .AND.
     1       IHARG(2).EQ.'CLIP' .AND. IHARG2(2).EQ.'BOAR')THEN
        ICASRE='CFUN'
      ELSEIF(IHARG(1).EQ.'FUNC'.AND.IHARG2(1).EQ.'TION')THEN
        ICASRE='FUNC'
      ELSEIF(IHARG(1).EQ.'STRI'.AND.IHARG2(1).EQ.'NG')THEN
        ICASRE='FUNC'
      ELSEIF(IHARG(1).EQ.'CLIP'.AND.IHARG2(1).EQ.'BOAR')THEN
C
C       CHECK IF ARGUMENT IS A FILE NAME STARTING WITH
C       "CLIPBOARD.
C
        IWORD=2
        IOFILE='NO'
        CALL DPFILE(IANSLC,IWIDTH,IWORD,IOFILE,IBUGS2,ISUBRO,IERROR)
        IF(IOFILE.EQ.'NO')ICASRE='CLIP'
      ELSEIF(IHARG(1).EQ.'ROW '.AND.IHARG2(1).EQ.'    ' .AND.
     1       IHARG(2).NE.'LABE')THEN
        ICASRE='ROWR'
      ELSEIF(IHARG(1).EQ.'MATR'.AND.IHARG2(1).EQ.'IX')THEN
        IF(IHARG(2).EQ.'TO  ' .AND. IHARG(3).EQ.'VARI')THEN
          ICASRE='MATZ'
        ELSE
          ICASRE='MATR'
        ENDIF
      ELSEIF(IHARG(1).EQ.'EXCE'.AND.IHARG2(1).EQ.'L   ')THEN
        IEXCEL='ON'
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGS2,IERROR)
      ENDIF
C
      IF(IHARG(1).EQ.'PNG' .AND. IHARG(2).EQ.'IMAG')THEN
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGS2,IERROR)
        IMAGTY=2
      ELSEIF((IHARG(1).EQ.'JPG' .OR. IHARG(1).EQ.'JPEG') .AND.
     1       IHARG(2).EQ.'IMAG')THEN
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGS2,IERROR)
        IMAGTY=1
      ELSEIF((IHARG(1).EQ.'GIF' .OR. IHARG(1).EQ.'GIFF') .AND.
     1       IHARG(2).EQ.'IMAG')THEN
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGS2,IERROR)
        IMAGTY=3
      ELSEIF(IHARG(1).EQ.'BMP' .AND. IHARG(2).EQ.'IMAG')THEN
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGS2,IERROR)
        IMAGTY=4
      ELSEIF(IHARG(1).EQ.'WBMP' .AND. IHARG(2).EQ.'IMAG')THEN
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGS2,IERROR)
        IMAGTY=5
      ELSEIF(IHARG(1).EQ.'WEBP' .AND. IHARG(2).EQ.'IMAG')THEN
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGS2,IERROR)
        IMAGTY=6
      ELSEIF(IHARG(1).EQ.'TGA' .AND. IHARG(2).EQ.'IMAG')THEN
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGS2,IERROR)
        IMAGTY=7
      ELSEIF((IHARG(1).EQ.'TIF' .OR. IHARG(1).EQ.'TIFF') .AND.
     1       IHARG(2).EQ.'IMAG')THEN
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGS2,IERROR)
        IMAGTY=8
      ELSEIF(IHARG(1).EQ.'XPM' .AND. IHARG(2).EQ.'IMAG')THEN
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGS2,IERROR)
        IMAGTY=9
      ENDIF
C
      IF(IHARG(1).EQ.'IMAG'.AND.IHARG2(1).EQ.'E')THEN
        IF(IHARG(2).EQ.'TO  ' .AND. IHARG(3).EQ.'VARI')THEN
          ICASRE='IMAZ'
        ELSE
          ICASRE='IMAG'
          IF(IHARG(2).EQ.'RED')THEN
            IMAGCO=1
            IMAGSH=1
          ELSEIF(IHARG(2).EQ.'GREE')THEN
            IMAGCO=2
            IMAGSH=1
          ELSEIF(IHARG(3).EQ.'BLUE')THEN
            IMAGCO=3
            IMAGSH=1
          ELSE
            IMAGCO=1
            IMAGSH=0
          ENDIF
        ENDIF
      ENDIF
C
      IF(IHARG(1).EQ.'ROW '.AND.IHARG2(1).EQ.' '.AND.
     1   IHARG(2).EQ.'LABE'.AND.IHARG2(2).EQ.'LS')ICASRE='ROWI'
      IF(IHARG(1).EQ.'ROW '.AND.IHARG2(1).EQ.' '.AND.
     1   IHARG(2).EQ.'LABE'.AND.IHARG2(2).EQ.'L ')ICASRE='ROWI'
C
      IF(IHARG(1).EQ.'STAC'.AND.IHARG2(1).EQ.'K   ')THEN
        IF(IHARG(2).EQ.'VARI'.AND.IHARG2(2).EQ.'ABLE')THEN
          ICASRE='STAC'
        ENDIF
      ENDIF
      IF(IHARG(1).EQ.'STAC'.AND.IHARG2(1).EQ.'KED ')THEN
        IF(IHARG(2).EQ.'VARI'.AND.IHARG2(2).EQ.'ABLE')THEN
          ICASRE='STAC'
        ENDIF
      ENDIF
C
  150 CONTINUE
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
        WRITE(ICOUT,155)ICASRE
  155   FORMAT('ICASRE = ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ******************************************************
C               **  STEP 2A--                                       **
C               **  DETERMINE THE TYPE OF READ CASE--               **
C               **       1) FROM TERMINAL;                          **
C               **       2) FROM FILE;                              **
C               **  NOTE--IOTERM  WILL = 'YES' ONLY IN EXPLICIT     **
C               **        TERMINAL CASE                             **
C               **        (THAT IS, ONLY WHEN INPUT IOSW            **
C               **                             = 'TERM')            **
C               **  NOTE--IOFILE  WILL = 'YES' ONLY IN FILE CASE.   **
C               **  NOTE--IMAGE READ ONLY SUPPORTED FOR FILE CASE.  **
C               ******************************************************
C
      ISTEPN='2A'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IWORD=2
      IF(ICASRE.EQ.'LINE')IWORD=3
      IF(ICASRE.EQ.'PARA')IWORD=3
      IF(ICASRE.EQ.'FUNC')IWORD=3
      IF(ICASRE.EQ.'MATR')IWORD=3
      IF(ICASRE.EQ.'ROWR')IWORD=3
      IF(ICASRE.EQ.'MATZ')IWORD=5
      IF(ICASRE.EQ.'IMAG')IWORD=3 + IMAGSH
      IF(ICASRE.EQ.'IMAZ')IWORD=5
      IF(ICASRE.EQ.'STAC')IWORD=4
      IF(IEXCEL.EQ.'ON')IWORD=3
      IF(ICASRE.EQ.'ROWI')THEN
        IWORD=4
        IF(NUMARG.LE.2)THEN
          IOFILE='NO'
          GOTO202
        ENDIF
      ELSEIF(ICASRE.EQ.'CLIP' .OR. ICASRE.EQ.'CFUN')THEN
        IOFILE='NO'
        GOTO202
      ENDIF
C
      CALL DPFILE(IANSLC,IWIDTH,IWORD,
     1            IOFILE,IBUGS2,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(IEXCEL.EQ.'ON' .AND. IOFILE.EQ.'NO')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,191)
  191   FORMAT('      THE READ EXCEL COMMAND REQUIRES A FILE NAME ',
     1         'TO BE SPECIFIED.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
  202 CONTINUE
C
      IOTERM='NO'
      IF(IOFILE.EQ.'NO'.AND.IOSW.EQ.'TERM')IOTERM='YES'
C
C     JANUARY 2015.  CHECK IF "LOOP" IS ACTIVE WHEN READING
C                    FROM TERMINAL.
C
      IF(IOFILE.EQ.'NO' .AND. ILOOST.EQ.'EXEC')THEN
        IF(IOSW.NE.'TERM')IOTERM='LOOP'
      ENDIF
C
CCCCC DECEMBER 2004.  IF GUI RUNNING, DO NOT ALLOW TERMINAL READ.
C
      IF(ICASRE.EQ.'LINE' .AND. IOFILE.EQ.'NO  ')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,213)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,205)
  205   FORMAT('      ARE NOT PERMITTED FOR THE READ1/READ2/READ3 ',
     1         'CASES.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ELSEIF(IOFILE.EQ.'NO' .AND. IGUIFL.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,211)
  211   FORMAT('***** ERROR FROM READ--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,213)
  213   FORMAT('      TERMINAL READS (I.E., READ WITH NO FILE NAME ',
     1         'SPECIFIED)')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,215)
  215   FORMAT('      ARE NOT PERMITTED WHEN RUNNING DATAPLOT FROM ',
     1         'THE GRAPHICAL USER INTERFACE)')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,217)
  217   FORMAT('      ALTERNATIVELY, YOU CAN DO ONE OF THE FOLLOWING:')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,219)
  219   FORMAT('      1) YOU CAN ENTER THE DATA DIRECTLY FROM THE ',
     1         'DATASHEET.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,221)
  221   FORMAT('      2) FROM THE COMMAND LINE WINDOW, YOU CAN USE ',
     1         'THE DATA COMMAND AS FOLLOWS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,223)
  223   FORMAT('         LET Y = DATA value1 value2 ...')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,225)
  225   FORMAT('      3) THE FIRST TWO METHODS ARE USEFUL FOR SMALL ',
     1         'AMOUNTS OF DATA.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,227)
  227   FORMAT('         FOR MORE THAN A FEW DATA POINTS, IT IS ',
     1         'RECOMMENDED THAT YOU')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,229)
  229   FORMAT('         CREATE THE DATA IN AN ASCII FILE AND THEN')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,231)
  231   FORMAT('         READ THE DATA FROM THAT FILE.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(IOFILE.EQ.'NO' .AND.
     1   (ICASRE.EQ.'IMAZ' .OR. ICASRE.EQ.'IMAG'))THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,241)
  241   FORMAT('      AN IMAGE READ REQUIRES THAT A FILE NAME BE ',
     1         'SPECIFIED.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,242)
  242   FORMAT('      NO FILE NAME WAS GIVEN ON THE READ COMMAND.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C
C               *************************************
C               **  STEP 2B--                      **
C               **  IF HAVE THE FILE INPUT CASE--  **
C               **  COPY OVER VARIABLES            **
C               *************************************
C
      ISTEPN='2B'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IOFILE.EQ.'YES')THEN
C
        IOUNIT=IREANU
        IFILE=IREANA
        ISTAT=IREAST
        IFORM=IREAFO
        IACCES=IREAAC
        IPROT=IREAPR
        ICURST=IREACS
C
        ISUBN0='READ'
        IERRFI='NO'
C
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
          WRITE(ICOUT,1183)IOUNIT,ISUBN0,IERRFI
 1183     FORMAT('IOUNIT,ISUBN0,IERRFI = ',I8,A4,2X,A4)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1184)IFILE(1:80)
 1184     FORMAT('IFILE = ',A80)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1185)ISTAT,IFORM,IACCES,IPROT,ICURST
 1185     FORMAT('ISTAT,IFORM,IACCES,IPROT,ICURST = ',4(A12,2X),A12)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
      ENDIF
C
C               ***********************************************
C               **  STEP 2C--                                **
C               **  IF HAVE THE FILE INPUT CASE--            **
C               **  CHECK TO SEE IF THE READ FILE MAY EXIST  **
C               ***********************************************
C
      ISTEPN='2C'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IOFILE.EQ.'YES')THEN
C
        IF(ISTAT.EQ.'NONE')THEN
          IERROR='YES'
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1211)
 1211     FORMAT('***** IMPLEMENTATION ERROR IN DPREAD--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1212)
 1212     FORMAT('      THE DESIRED READING CANNOT BE CARRIED OUT')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1214)
 1214     FORMAT('      BECAUSE THE INTERNAL VARIABLE   IREAST   WHICH')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1215)
 1215     FORMAT('      ALLOWS SUCH READING HAS BEEN SET TO    NONE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1217)ISTAT,IREAST
 1217     FORMAT('ISTAT,IREAST = ',A12,2X,A12)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1218)
 1218     FORMAT('      ALL READING MUST BE DONE DIRECTLY FROM ',
     1           'THE TERMINAL')
          CALL DPWRST('XXX','BUG ')
          GOTO9000
        ENDIF
      ENDIF
C
C               *************************************
C               **  STEP 2D--                      **
C               **  IF HAVE THE FILE INPUT CASE--  **
C               **  EXTRACT THE FILE NAME          **
C               *************************************
C
      ISTEPN='2D'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IOFILE.EQ.'YES')THEN
C
        DO1310I=1,MAXSTR
          ICANS(I:I)=IANSLC(I)(1:1)
 1310   CONTINUE
C
        ISTART=1
        ISTOP=IWIDTH
        IWORD=2
        IF(ICASRE.EQ.'PARA')IWORD=3
        IF(ICASRE.EQ.'FUNC')IWORD=3
        IF(ICASRE.EQ.'MATR')IWORD=3
        IF(ICASRE.EQ.'ROWR')IWORD=3
        IF(ICASRE.EQ.'MATZ')IWORD=5
        IF(ICASRE.EQ.'IMAG')IWORD=3 + IMAGSH
        IF(ICASRE.EQ.'IMAZ')IWORD=5
        IF(ICASRE.EQ.'ROWI')IWORD=4
        IF(ICASRE.EQ.'STAC')IWORD=4
        IF(IEXCEL.EQ.'ON')IWORD=3
        CALL DPEXWO(ICANS,ISTART,ISTOP,IWORD,
     1              ICOL1,ICOL2,IFILE,NCFILE,
     1              IBUGS2,ISUBRO,IERROR)
C
        IF(IEXCEL.EQ.'ON')THEN
C
          IOP='OPEN'
          IFLG11=0
          IFLG21=0
          IFLG31=0
          IFLAG4=0
          IFLAG5=1
          CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,
     1                IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1                IBUGS2,ISUBRO,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
          WRITE(IOUNI5,'(A256)')IFILE(1:256)
          WRITE(IOUNI5,'(A8)')IEXCSH
          IF(IEXCR1.LE.IEXCR2)THEN
            IF(IEXCR1.GE.1)THEN
              IVAL1=IEXCR1
              IVAL2=IEXCR2
            ELSE
              IVAL1=-1
              IVAL2=-1
            ENDIF
            WRITE(IOUNI5,'(I8)')IVAL1
            WRITE(IOUNI5,'(I8)')IVAL2
          ELSE
            IF(IEXCR2.GE.1)THEN
              IVAL1=IEXCR2
              IVAL2=IEXCR1
            ELSE
              IVAL1=-1
              IVAL2=-1
            ENDIF
            WRITE(IOUNI5,'(I8)')IVAL1
            WRITE(IOUNI5,'(I8)')IVAL2
          ENDIF
          IF(IEXCC1.LE.IEXCC2)THEN
            IF(IEXCC1.GE.1)THEN
              IVAL1=IEXCC1
              IVAL2=IEXCC2
            ELSE
              IVAL1=-1
              IVAL2=-1
            ENDIF
            WRITE(IOUNI5,'(I8)')IVAL1
            WRITE(IOUNI5,'(I8)')IVAL2
          ELSE
            IF(IEXCC2.GE.1)THEN
              IVAL1=IEXCC2
              IVAL2=IEXCC1
            ELSE
              IVAL1=-1
              IVAL2=-1
            ENDIF
            WRITE(IOUNI5,'(I8)')IVAL1
            WRITE(IOUNI5,'(I8)')IVAL2
          ENDIF
          IOP='CLOS'
          CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,
     1                IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1                IBUGS2,ISUBRO,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ITYPEZ='PYTH'
          ISNAME='read_excel.py'
          IWIDZZ=13
          ISARGL=' '
          NCARG=0
          CALL DPEXR2(ITYPEZ,ISNAME,IWIDZZ,ISARGL,NCARG,
     1                IBUGS2,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
          IFOUND='YES'
C
          IFILE=' '
          IFILE='dpst1f.dat'
          NCFILE=10
          IFOUND='YES'
          IREADL=','
          ISKIP=1
C
        ENDIF
C
        IF(NCFILE.LT.1)THEN
          IERROR='YES'
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,211)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1342)
 1342     FORMAT('      A USER FILE NAME IS REQUIRED IN THE READ')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1344)
 1344     FORMAT('      COMMAND (FOR EXAMPLE,    READ CALIB.DAT X Y Z)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1345)
 1345     FORMAT('      BUT NONE WAS GIVEN HERE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1346)
 1346     FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,1347)(IANSLC(I),I=1,MIN(100,IWIDTH))
 1347       FORMAT('      ',100A1)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            GOTO9000
          ENDIF
C
C       2019/10: CHECK FOR ".csv" OR ".CSV" EXTENSION.  IF FOUND, SET
C                READ DELIMITER TO ",".
        ELSEIF(NCFILE.GE.4)THEN
          IF(IFILE(NCFILE-3:NCFILE).EQ.'.csv' .OR.
     1       IFILE(NCFILE-3:NCFILE).EQ.'.CSV')THEN
            IREADL=','
          ENDIF
        ENDIF
C
      ENDIF
C
C               *************************************
C               **  STEP 2E--                      **
C               **  IF HAVE THE FILE INPUT CASE--  **
C               **  OPEN THE FILE                  **
C               *************************************
C
      ISTEPN='2E'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC NOTE: FOR THE "IMAGE" CASE, THE FILE OPEN IS HANDLED
CCCCC       BY THE "GD.C" CODE.
CCCCC
CCCCC       HOWEVER, WE DO NEED TO PERFORM A SEARCH OF THE
CCCCC       DATAPLOT DIRECTORIES AND LOOK FOR UPPER/LOWER
CCCCC       CASE ISSUES AS WE DO WITH OTHER FILES.  CALL
CCCCC       DPINF3 TO SEE IF FILE EXISTS BEFORE CALL GD
CCCCC       LIBRARY.
C
      IF(IOFILE.EQ.'YES' .AND. ICASRE.NE.'IMAG' .AND.
     1  ICASRE.NE.'IMAZ')THEN
C
        IREWIN='ON'
        ICRFLG='ROW'
        IF(NCREAF.GT.0.AND.IOFILE.EQ.'YES')THEN
          IF(ICREAF(1:5).EQ.'(UNFO'.AND.ICASRE.EQ.'VARI')THEN
            IFORM='UNFORMATTED'
            IFMFLG='ON'
            IF(ICREAF(13:16).EQ.'COLU')ICRFLG='COLU'
            IF(ICREAF(1:5).EQ.'(COLU')ICRFLG='COLU'
          ELSEIF(ICREAF(1:5).EQ.'(UNFO'.AND.ICASRE.EQ.'MATR')THEN
            IF(IUNFMC.GT.0)THEN
              IFORM='UNFORMATTED'
              IFMFLG='ON'
            ELSE
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,211)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1442)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1443)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,1444)
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GOTO9000
            ENDIF
          ELSE
            IFORM='FORMATTED'
            IFMFLG='OFF'
          ENDIF
        ELSE
          IFORM='FORMATTED'
          IFMFLG='OFF'
        ENDIF
 1442   FORMAT('      FOR UNFORMATTED READS OF MATRICES, THE ',
     1         ' FOLLOWING COMMAND IS REQUIRED:')
 1443   FORMAT('         SET UNFORMATTED COLUMNS <VALUE>')
 1444   FORMAT('      WHERE <VALUE> IS THE NUMBER OF COLUMNS IN THE ',
     1         'MATRTIX.')
C
        IF(IREACS(1:4).EQ.'CLOS')
     1    CALL DPOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1    IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
        IF(IERRFI.EQ.'YES')GOTO9090
        IF(IREACS(1:4).EQ.'CLOS')IREACS='OPEN'
C
      ELSEIF(IOFILE.EQ.'YES' .AND.
     1  (ICASRE.EQ.'IMAG' .OR. ICASRE.EQ.'IMAZ'))THEN
C
        CALL DPINF3(IFILE,FTEMP,IEXIST,
     1              ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
        IF(IEXIST.EQ.'NO')THEN
          WRITE(ICOUT,1501)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1561)
 1561     FORMAT('      UNABLE TO FIND THE IMAGE FILE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1563)IFILE(1:80)
 1563     FORMAT('      FILE NAME: ',A80)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9090
        ELSE
          IFILE=FTEMP
        ENDIF
C
        IF(IMAGTY.LT.1 .OR. IMAGTY.GT.9)THEN
          DO1560I=MAXSTR,1,-1
            IF(IFILE(I:I).NE.' ')THEN
              NLAST=I
              GOTO1569
            ENDIF
 1560     CONTINUE
          NLAST=0
 1569     CONTINUE
          IF(NLAST.LT.4)THEN
            WRITE(ICOUT,1501)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1571)
 1571       FORMAT('      UNABLE TO DETERMINE THE IMAGE TYPE.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
          IF(IFILE(NLAST-2:NLAST).EQ.'JPG')THEN
            IMAGTY=1
          ELSEIF(IFILE(NLAST-2:NLAST).EQ.'jpg')THEN
            IMAGTY=1
          ELSEIF(IFILE(NLAST-3:NLAST).EQ.'JPEG')THEN
            IMAGTY=1
          ELSEIF(IFILE(NLAST-3:NLAST).EQ.'jpeg')THEN
            IMAGTY=1
          ELSEIF(IFILE(NLAST-2:NLAST).EQ.'PNG')THEN
            IMAGTY=2
          ELSEIF(IFILE(NLAST-2:NLAST).EQ.'png')THEN
            IMAGTY=2
          ELSEIF(IFILE(NLAST-2:NLAST).EQ.'GIF')THEN
            IMAGTY=3
          ELSEIF(IFILE(NLAST-2:NLAST).EQ.'gif')THEN
            IMAGTY=3
          ELSEIF(IFILE(NLAST-3:NLAST).EQ.'GIFF')THEN
            IMAGTY=3
          ELSEIF(IFILE(NLAST-3:NLAST).EQ.'giff')THEN
            IMAGTY=3
          ELSEIF(IFILE(NLAST-2:NLAST).EQ.'BMP')THEN
            IMAGTY=4
          ELSEIF(IFILE(NLAST-2:NLAST).EQ.'bmp')THEN
            IMAGTY=4
          ELSEIF(IFILE(NLAST-3:NLAST).EQ.'WBMP')THEN
            IMAGTY=5
          ELSEIF(IFILE(NLAST-3:NLAST).EQ.'wbmp')THEN
            IMAGTY=5
          ELSEIF(IFILE(NLAST-3:NLAST).EQ.'WEBP')THEN
            IMAGTY=6
          ELSEIF(IFILE(NLAST-3:NLAST).EQ.'webp')THEN
            IMAGTY=6
          ELSEIF(IFILE(NLAST-2:NLAST).EQ.'TGA')THEN
            IMAGTY=7
          ELSEIF(IFILE(NLAST-2:NLAST).EQ.'tga')THEN
            IMAGTY=7
          ELSEIF(IFILE(NLAST-3:NLAST).EQ.'TIFF')THEN
            IMAGTY=8
          ELSEIF(IFILE(NLAST-3:NLAST).EQ.'tiff')THEN
            IMAGTY=8
          ELSEIF(IFILE(NLAST-2:NLAST).EQ.'TIF')THEN
            IMAGTY=8
          ELSEIF(IFILE(NLAST-2:NLAST).EQ.'tif')THEN
            IMAGTY=8
          ELSEIF(IFILE(NLAST-2:NLAST).EQ.'XPM')THEN
            IMAGTY=9
          ELSEIF(IFILE(NLAST-2:NLAST).EQ.'xpm')THEN
            IMAGTY=9
          ELSE
            WRITE(ICOUT,1501)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1571)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
        ENDIF
C
        NCFILE=MAXSTR
        DO1581I=NCFILE,1,-1
          IF(IFILE(I:I).NE.' ')THEN
            NCFILE=I
            GOTO1589
          ENDIF
 1581   CONTINUE
 1589   CONTINUE
        DO1590I=1,NCFILE
          CALL DPCOAN(IFILE(I:I),IJUNK)
          IADE(I)=IJUNK
 1590   CONTINUE
        IADE(NCFILE+1)=0
C
        IERR=0
#ifdef HAVE_GD
        CALL GDLOAD(IMAGTY,IXSIZE,IYSIZE,IADE,IERR)
#endif
        IF(IERR.EQ.1)THEN
          WRITE(ICOUT,1501)
 1501     FORMAT('***** ERROR IN READING IMAGE--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1503)
 1503     FORMAT('      UNABLE TO OPEN THE IMAGE FILE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9090
        ELSEIF(IERR.EQ.2)THEN
          WRITE(ICOUT,1501)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1513)
 1513     FORMAT('      UNABLE TO LOAD THE IMAGE FILE.  THE MOST')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1514)
 1514     FORMAT('      LIKELY CAUSE IS THAT THE FILE IS NOT OF THE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1515)
 1515     FORMAT('      EXPECTED TYPE.  THE EXPECTED TYPE IS:')
          CALL DPWRST('XXX','BUG ')
          IF(IMAGTY.EQ.1)THEN
            WRITE(ICOUT,1516)
 1516       FORMAT('          JPG')
            CALL DPWRST('XXX','BUG ')
          ELSEIF(IMAGTY.EQ.2)THEN
            WRITE(ICOUT,1517)
 1517       FORMAT('          PNG')
            CALL DPWRST('XXX','BUG ')
          ELSEIF(IMAGTY.EQ.3)THEN
            WRITE(ICOUT,1518)
 1518       FORMAT('          GIF')
            CALL DPWRST('XXX','BUG ')
          ENDIF
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9090
        ELSEIF(IERR.EQ.3)THEN
          WRITE(ICOUT,1501)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1523)
 1523     FORMAT('      THE IMAGE READ CAPABILITY IS NOT CURRENTLY')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1525)
 1525     FORMAT('      IMPLEMENTED FOR THIS INSTALLATION.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9090
        ENDIF
C
      ENDIF
C
C               ******************************************
C               **  STEP 2F--                           **
C               **  FOR THE 2 CASES--                   **
C               **      1) TERMINAL INPUT;              **
C               **      2) FILE INPUT;                  **
C               **  DEFINE THE INPUT READ UNIT NUMBER,  **
C               **  AND OTHER VARIABLES NEEDED          **
C               **  FOR UPCOMING READS.                 **
C               ******************************************
C
      ISTEPN='2F'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IRD2=IRD
      IF(IMACCS.EQ.'OPEN'.OR.IMALEV.GE.1)THEN
        IRD2=IMACNU
      ENDIF
      IF(IOFILE.EQ.'YES')IRD2=IREANU
      IF(IOTERM.EQ.'YES')IRD2=IRD
      IF(ICASRE.EQ.'CLIP')IRD2=IRD
C
      IOUNIT=IRD2
C
C               *****************************************
C               **  STEP 3--                           **
C               **  CHECK TO SEE THE TYPE CASE--       **
C               **    1) UNQUALIFIED (THAT IS, FULL);  **
C               **    2) SUBSET; OR                    **
C               **    3) FOR.                          **
C               *****************************************
C
      ISTEPN='3'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASEQ='FULL'
      ILOCQ=NUMARG+1
      IF(NUMARG.LT.1)GOTO390
      DO300J=1,NUMARG
        J1=J
        IF(IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ')THEN
          ICASEQ='SUBS'
          ILOCQ=J1
          GOTO390
        ELSEIF(IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  ')THEN
          ICASEQ='SUBS'
          ILOCQ=J1
          GOTO390
        ELSEIF(IHARG(J).EQ.'FOR '.AND.IHARG2(J).EQ.'    ')THEN
          ICASEQ='FOR'
          ILOCQ=J1
          GOTO390
        ENDIF
  300 CONTINUE
  390 CONTINUE
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
        WRITE(ICOUT,391)NUMARG,ILOCQ
  391   FORMAT('NUMARG,ILOCQ = ',2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ******************************************************
C               **  STEP 4--                                        **
C               **  DETERMINE THE TYPE AND NUMBER OF ITEMS          **
C               **  TO BE READ   .                                  **
C               **  NUMALL = TOTAL NUMBER OF READ  ITEMS            **
C               **           (AS DETERMINED BY INCLUDING ONLY ALL   **
C               **           BEFORE 'SUBSET' OR 'EXCEPT' OR 'FOR')  **
C               **  NUMV   = NUMBER OF VARIABLES TO BE READ    ;    **
C               **  NUMP   = NUMBER OF PARAMETERS TO BE READ    ;   **
C               **  NUMM   = NUMBER OF MODELS TO BE READ            **
C               **           (SHOULD = 0 OR 1)                      **
C               **  NUMF   = NUMBER OF FUNCTIONS TO BE READ         **
C               **  NUMU   = NUMBER OF UNKNOWNS TO BE READ    ;     **
C               **  NUME   = TOTAL NUMBER OF READ  ITEMS            **
C               **           (SHOULD = NUMALL);                     **
C               ******************************************************
C
      ISTEPN='4'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')
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
      IF(IOFILE.EQ.'NO'.AND.ICASRE.EQ.'PARA')JMIN=2
      IF(IOFILE.EQ.'NO'.AND.ICASRE.EQ.'FUNC')JMIN=2
      IF(IOFILE.EQ.'NO'.AND.ICASRE.EQ.'MATR')JMIN=2
      IF(IOFILE.EQ.'NO'.AND.ICASRE.EQ.'MATZ')JMIN=4
      IF(IOFILE.EQ.'NO'.AND.ICASRE.EQ.'IMAG')JMIN=2
      IF(IOFILE.EQ.'NO'.AND.ICASRE.EQ.'IMAZ')JMIN=4
      IF(IOFILE.EQ.'NO'.AND.ICASRE.EQ.'ROWI')JMIN=3
      IF(IOFILE.EQ.'NO'.AND.ICASRE.EQ.'STAC')JMIN=3
      IF(IOFILE.EQ.'YES')JMIN=2
      IF(IOFILE.EQ.'YES'.AND.ICASRE.EQ.'PARA')JMIN=3
      IF(IOFILE.EQ.'YES'.AND.ICASRE.EQ.'FUNC')JMIN=3
      IF(IOFILE.EQ.'YES'.AND.ICASRE.EQ.'MATR')JMIN=3
      IF(IOFILE.EQ.'YES'.AND.ICASRE.EQ.'MATZ')JMIN=5
      IF(IOFILE.EQ.'YES'.AND.ICASRE.EQ.'IMAG')JMIN=3
      IF(IOFILE.EQ.'YES'.AND.ICASRE.EQ.'IMAZ')JMIN=5
      IF(IOFILE.EQ.'YES'.AND.ICASRE.EQ.'ROWI')JMIN=4
      IF(IOFILE.EQ.'YES'.AND.ICASRE.EQ.'STAC')JMIN=4
      IF(ICASRE.EQ.'CLIP' .OR. ICASRE.EQ.'CFUN')THEN
        JMIN=2
        IF(ICASRE.EQ.'CFUN')JMIN=3
        IOFILE='NO'
      ENDIF
C
CCCCC JULY 2002: QUOTED FILE NAMES MAY CONTAIN SPACES.
CCCCC DETERMINE HOW MANY ARGUMENTS FILE NAME MAY CONTAIN.
CCCCC JUNE 2003: UPDATE TO INCLUDE HYPHENS AS WELL AS SPACES.
C
      IF(IOFILE.EQ.'YES' .AND. IFILE(1:1).EQ.'"')THEN
        DO421I=MAXSTR,1,-1
          IF(IFILE(I:I).NE.' ')THEN
            ILAST=I
            GOTO424
          ENDIF
  421   CONTINUE
  424   CONTINUE
        ICOUNT=0
        ISPAC=0
        DO426I=1,ILAST
          IF((IFILE(I:I).EQ.' '.OR.IFILE(I:I).EQ.'-') .AND.
     1      ISPAC.EQ.0)THEN
            ISPAC=1
            ICOUNT=ICOUNT+1
          ELSEIF((IFILE(I:I).NE.' '.AND.IFILE(I:I).NE.'-') .AND.
     1      ISPAC.EQ.1)THEN
            ISPAC=0
          ENDIF
  426   CONTINUE
        JMIN=JMIN+ICOUNT
      ENDIF
C
      JMAX=ILOCQ-1
      IF(ICASRE.EQ.'ROWI')JMAX=JMIN
      IF(ICASRE.EQ.'MATR')THEN
        JMAX=JMIN+MAXCOM-1
        IF(JMAX.GT.JMIN+MAXRDV-1)JMAX=JMIN+MAXRDV-1
        IHMAT1=IHARG(JMIN)
        IHMAT2=IHARG2(JMIN)
      ELSEIF(ICASRE.EQ.'MATZ')THEN
        JMAX=JMIN+2
      ELSEIF(ICASRE.EQ.'IMAZ')THEN
        JMAX=JMIN+4
      ELSEIF(ICASRE.EQ.'STAC')THEN
        JMAX=JMIN+1
      ELSEIF(ICASRE.EQ.'ROWR')THEN
        JMAX=JMIN+1
      ENDIF
C
      IVALMA=0
      NUMALL=0
      NUMALL=JMAX-JMIN+1
      IF(ICASRE.EQ.'CLIP'.AND.NUMALL.LE.0)IVRLST='NO'
      IF(ICASRE.EQ.'VARI'.AND.NUMALL.LE.0)IVRLST='NO'
C
      ISTEPN='4A'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,403)ICASRE,IVRLST,JMIN,JMAX,NUMALL
  403   FORMAT('ICASRE,IVRLST,JMIN,JMAX,NUMALL = ',2(A4,2X),3I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IVRLST.EQ.'NO' .AND. ICASRE.EQ.'VARI' .AND.
     1   IOTERM.NE.'LOOP')THEN
C
C     SKIP AUTOMATIC CASE:
C
C        1. IF IAVANM = FILE, THEN RETRIEVE VARIABLE LIST FROM LINE JUST
C           BEFORE THE "----".
C
C        2. IF IAVANM <> FILE, THEN USE AUTOMATIC VARIABLE NAMES (BASE
C           WILL BE DETERMINED BY IAVABN).
C
        IF(ISKIP.EQ.-1.AND.IOFILE.EQ.'YES'.AND.
     1     (ICASRE.NE.'IMAG' .AND. ICASRE.NE.'IMAZ'))THEN
C
          ISTEPN='4B'
          IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')
     1      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C         CASE 1: RETRIEVE VARIABLE LIST FROM THE FILE
C
          IF(IAVANM.EQ.'FILE')THEN
C
C           STEP 1: READ UNTIL "---" FOUND
C
            DO4578I=1,MAXOBV
              ILINE=I
              NUMCHA=-1
              CALL DPREFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1                    IA,NUMCHA,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
C
              IF(IERROR.EQ.'YES')GOTO8800
              IF(IA(1).EQ.'E'.AND.IA(2).EQ.'O'.AND.IA(3).EQ.'F'.AND.
     1          NUMCHA.EQ.3)THEN
                REWIND IOUNIT
                GOTO8800
              ELSEIF(IA(1).EQ.'-'.AND.IA(2).EQ.'-'.AND.IA(3).EQ.'-'.AND.
     1          IA(4).EQ.'-')THEN
                GOTO4581
              ELSE
                DO4511J=1,MAXRDV
                  IASAVE(J)=IA(J)
 4511           CONTINUE
              ENDIF
 4578       CONTINUE
 4581       CONTINUE
            ISKIP=ILINE
C
C           STEP 2: EXTRACT THE VARIABLE NAMES
C
            IF(ILINE.GT.1)THEN
              IFRST=0
              ILAST=0
              INEW=0
              IVAR=0
              NTEMP=255
              CALL DPUPPE(IASAVE,NTEMP,IASAVE,IBUGS2,IERROR)
              DO4583J=1,NTEMP
                IF(IASAVE(J)(1:1).EQ.' ' .OR. IASAVE(J)(1:1).EQ.',')THEN
                  IF(INEW.EQ.1)THEN
                    IVAR=IVAR+1
                    ILAST=J
                    NCHAR=ILAST-IFRST+1
                    DO4585K=1,MIN(4,NCHAR)
                      IVLIST(IVAR)(K:K)=IASAVE(IFRST+K-1)(1:1)
 4585               CONTINUE
                    IF(NCHAR.GE.5)THEN
                      DO4587K=5,MIN(8,NCHAR)
                        IVLIS2(IVAR)(K-4:K-4)=IASAVE(IFRST+K-1)(1:1)
 4587                 CONTINUE
                    ENDIF
                    INEW=0
                  ENDIF
                ELSE
                  ILAST=J
                  IF(INEW.EQ.0)THEN
                    INEW=1
                    IFRST=J
                  ENDIF
                ENDIF
 4583         CONTINUE
              REWIND IOUNIT
              JMIN=1
              JMAX=IVAR
            ENDIF
C
C         CASE 2: USE AUTOMATIC VARIABLE NAMES
C
          ELSEIF(IAVANM.EQ.'AUTO')THEN
C
            ISTEPN='4C'
            IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')
     1        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
            DO6578I=1,MAXOBV
              ILINE=I
              NUMCHA=-1
              CALL DPREFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1                    IA,NUMCHA,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
C
              IF(IERROR.EQ.'YES')GOTO8800
              IF(IA(1).EQ.'E'.AND.IA(2).EQ.'O'.AND.IA(3).EQ.'F'.AND.
     1          NUMCHA.EQ.3)THEN
                REWIND IOUNIT
                GOTO8800
              ELSEIF(IA(1).EQ.'-'.AND.IA(2).EQ.'-'.AND.IA(3).EQ.'-'.AND.
     1          IA(4).EQ.'-')THEN
                GOTO6581
              ELSE
                DO6511J=1,255
                  IASAVE(J)=IA(J)
 6511           CONTINUE
              ENDIF
 6578       CONTINUE
 6581       CONTINUE
            ISKIP=ILINE
            MINCO2=1
            MAXCO2=NUMRCM
            IFCOL3=IFCOL1
            IFCOL4=IFCOL2
            NUMLRD=0
C
            NCBASE=0
            DO54590II=8,1,-1
              IF(IAVABN(II:II).NE.' ')THEN
                NCBASE=II
                GOTO54599
              ENDIF
54590       CONTINUE
54599       CONTINUE
C
 5592       CONTINUE
            DO5597I=1,MAXRCL
              ISTOR1(I)='    '
              ISTOR2(I)='    '
              ISTOR3(I)='    '
              IB(I)='    '
 5597       CONTINUE
            CALL DPREAL(IRD2,IFCOL3,IFCOL4,MINCO2,MAXCO2,
     1             X0,NUMDPL,IFLGSV,
     1             IXC,NXC,
     1             ICASRE,IFUNC2,N2,MAXN2,
     1             IMACRO,IMACNU,IMACCS,
     1             IANSLC,IWIDTH,IREACS,ISTOR1,ISTOR2,IEND,NUMLRD,
     1             IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1             ICOMCH,ICOMSW,LINETY,IGRPA2,
     1             IFCOLL,IFCOLU,ITYPE,NCOLS,NCALL,
     1             IREADL,IDATDL,ITIMDL,IRDIPA,PREAMV,
     1             MAXRDV,MAXCHV,IFIETY,
     1             IDECPT,IDATMV,IDATNN,
     1             IREACD,IREACM,IREADS,IREAPM,IREAMC,ITABNC,
     1             IREAAS,IREAPC,
     1             IB,
     1             IOTERM,IANSLO,MAXLIL,MAXCIL,ILOOST,ILOOLI,
     1             IREPCH,IMALEV,
     1             IERRFI,IBUGS2,ISUBRO,IERROR)
            IF(IERROR.EQ.'YES')GOTO9000
            IF(LINETY.EQ.'BLAN')GOTO5592
            NUMLRD=0
            IF(NUMDPL.GT.0)THEN
C
              IF(IMNVAR.LT.0)IMNVAR=NUMDPL
              IF(IMXVAR.LT.0)IMXVAR=NUMDPL
              IF(NUMDPL.LT.IMNVAR)IMNVAR=NUMDPL
              IF(NUMDPL.GT.IMXVAR)IMXVAR=NUMDPL
              DO5593J=1,NUMDPL
                IF(NCBASE.LE.0)THEN
                  IVLIST(J)='COL '
                  IVLIS2(J)='    '
                  NCBASE=3
                ELSE
                  IVLIST(J)=IAVABN(1:4)
                  IVLIS2(J)=IAVABN(5:8)
                ENDIF
                NCSTAR=NCBASE+1
                NCSTOP=NCBASE+J
                IF(NCSTOP.GT.8)THEN
                  NDIFF=NCSTOP-8
                  NCSTAR=NCSTAR-NDIFF
                ENDIF
                IVTEMP(1:4)=IVLIST(J)
                IVTEMP(5:8)=IVLIS2(J)
                IF(J.LE.9)THEN
                  WRITE(IVTEMP(NCSTAR:NCSTAR),'(I1)')J
                ELSEIF(J.LE.99)THEN
                  WRITE(IVTEMP(NCSTAR:NCSTAR+1),'(I2)')J
                ELSEIF(J.LE.999)THEN
                  WRITE(IVTEMP(NCSTAR:NCSTAR+2),'(I3)')J
                ELSEIF(J.LE.9999)THEN
                  WRITE(IVTEMP(NCSTAR:NCSTAR+3),'(I4)')J
                ELSE
                  WRITE(IVTEMP(NCSTAR:NCSTAR+4),'(I5)')J
                ENDIF
                IVLIST(J)(1:4)=IVTEMP(1:4)
                IVLIS2(J)(1:4)=IVTEMP(5:8)
 5593         CONTINUE
            ENDIF
            REWIND IOUNIT
            NCALL=0
            NCOLS=0
            JMIN=1
            JMAX=NUMDPL
          ENDIF
C
C     SKIP AUTOMATIC OFF CASE
C
C        1) SET COLUMN LIMITS, ROW LIMITS
C        2) SKIP OVER HEADER LINES (IF NEEDED)
C        3) READ SINGLE LINE OF DATA
C        4) DETERMINE NUMBER OF COLUMNS OF DATA IN THAT LINE
C        5) SET VARIABLE NAMES TO X1, ..., Xk
C           (2014/10: THE IAVABN VARIABLE SPECIFIES THE DEFAULT FOR THE
C           VARIABLE NAMES, THE DEFUALT IS NOW COL1, COL2, ETC.)
C
C        IF IVARLA="ON", FIRST LINE READ SHOULD BE VARIABLE NAMES
C
C        6) REWIND THE FILE
C
        ELSEIF(IOFILE.EQ.'YES' .AND. ICASRE.NE.'IMAG' .AND.
     1         ICASRE.NE.'IMAZ')THEN
C
C         STEP 1: SKIP HEADER LINES
C
          IF(ISKIP.GE.0)THEN
            IFRMIN=IFROW1
            IFRMAX=IFROW1+ISKIP
            IF(IFRMAX.LT.IFRMIN)IFRMAX=IFRMIN
            MINCO2=1
            MAXCO2=NUMRCM
            IFCOL3=IFCOL1
            IFCOL4=IFCOL2
            IF(IFRMIN.LT.IFRMAX)THEN
              DO4591IFROW=IFRMIN,IFRMAX-1
                ILINE=IFROW
                NUMCHA=-1
                CALL DPREFI(
     1              IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1              IA,NUMCHA,
     1              ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
 
                IF(IA(1).NE.'-'.OR.IA(2).NE.'-'.OR.IA(3).NE.'-'.OR.
     1          IA(4).NE.'-')THEN
                  DO4513J=1,255
                    IASAVE(J)=IA(J)
 4513             CONTINUE
                ENDIF
 4591         CONTINUE
            ENDIF
C
C         STEP 2A: READ FIRST LINE OF DATA FILE TO DETERMINE NUMBER OF
C                  VARIABLES
C
            IF(IVARLA.EQ.'OFF' .AND. IAVANM.EQ.'AUTO')THEN
              NUMLRD=0
C
              NCBASE=0
              DO44590II=8,1,-1
                IF(IAVABN(II:II).NE.' ')THEN
                  NCBASE=II
                  GOTO44599
                ENDIF
44590         CONTINUE
44599         CONTINUE
C
 4592         CONTINUE
              DO4597I=1,MAXRCL
                ISTOR1(I)='    '
                ISTOR2(I)='    '
                ISTOR3(I)='    '
                IB(I)='    '
 4597         CONTINUE
              CALL DPREAL(IRD2,IFCOL3,IFCOL4,MINCO2,MAXCO2,
     1             X0,NUMDPL,IFLGSV,
     1             IXC,NXC,
     1             ICASRE,IFUNC2,N2,MAXN2,
     1             IMACRO,IMACNU,IMACCS,
     1             IANSLC,IWIDTH,IREACS,ISTOR1,ISTOR2,IEND,NUMLRD,
     1             IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1             ICOMCH,ICOMSW,LINETY,IGRPA2,
     1             IFCOLL,IFCOLU,ITYPE,NCOLS,NCALL,
     1             IREADL,IDATDL,ITIMDL,IRDIPA,PREAMV,
     1             MAXRDV,MAXCHV,IFIETY,
     1             IDECPT,IDATMV,IDATNN,
     1             IREACD,IREACM,IREADS,IREAPM,IREAMC,ITABNC,
     1             IREAAS,IREAPC,
     1             IB,
     1             IOTERM,IANSLO,MAXLIL,MAXCIL,ILOOST,ILOOLI,
     1             IREPCH,IMALEV,
     1             IERRFI,IBUGS2,ISUBRO,IERROR)
              IF(IERROR.EQ.'YES')GOTO9000
              IF(LINETY.EQ.'BLAN')GOTO4592
              NUMLRD=0
              IF(NUMDPL.GT.0)THEN
C
                IF(IMNVAR.LT.0)IMNVAR=NUMDPL
                IF(IMXVAR.LT.0)IMXVAR=NUMDPL
                IF(NUMDPL.LT.IMNVAR)IMNVAR=NUMDPL
                IF(NUMDPL.GT.IMXVAR)IMXVAR=NUMDPL
                DO4593J=1,NUMDPL
                  IF(NCBASE.LE.0)THEN
                    IVLIST(J)='COL '
                    IVLIS2(J)='    '
                    NCBASE=3
                  ELSE
                    IVLIST(J)=IAVABN(1:4)
                    IVLIS2(J)=IAVABN(5:8)
                  ENDIF
                  NCSTAR=NCBASE+1
                  NCSTOP=NCBASE+J
                  IF(NCSTOP.GT.8)THEN
                    NDIFF=NCSTOP-8
                    NCSTAR=NCSTAR-NDIFF
                  ENDIF
                  IVTEMP(1:4)=IVLIST(J)
                  IVTEMP(5:8)=IVLIS2(J)
                  IF(J.LE.9)THEN
                    WRITE(IVTEMP(NCSTAR:NCSTAR),'(I1)')J
                  ELSEIF(J.LE.99)THEN
                    WRITE(IVTEMP(NCSTAR:NCSTAR+1),'(I2)')J
                  ELSEIF(J.LE.999)THEN
                    WRITE(IVTEMP(NCSTAR:NCSTAR+2),'(I3)')J
                  ELSEIF(J.LE.9999)THEN
                    WRITE(IVTEMP(NCSTAR:NCSTAR+3),'(I4)')J
                  ELSE
                    WRITE(IVTEMP(NCSTAR:NCSTAR+4),'(I5)')J
                  ENDIF
                  IVLIST(J)(1:4)=IVTEMP(1:4)
                  IVLIS2(J)(1:4)=IVTEMP(5:8)
 4593           CONTINUE
              ENDIF
              REWIND IOUNIT 
              NCALL=0
              NCOLS=0
              JMIN=1
              JMAX=NUMDPL
C
C         STEP 2B: VARIABLE NAMES READ FROM LAST HEADER LINE (OR
C                  NEXT TO LAST LINE IF LAST LINE STARTS WITH
C                  "----").
C
            ELSEIF(IVARLA.EQ.'OFF' .AND. IAVANM.EQ.'FILE')THEN
              IF(ILINE.GT.1)THEN
                IFRST=0
                ILAST=0
                INEW=0
                IVAR=0
                NTEMP=255
                CALL DPUPPE(IASAVE,NTEMP,IASAVE,IBUGS2,IERROR)
                DO5583J=1,NTEMP
                  IF(IASAVE(J)(1:1).EQ.' '.OR.IASAVE(J)(1:1).EQ.',')THEN
                    IF(INEW.EQ.1)THEN
                      IVAR=IVAR+1
                      ILAST=J
                      NCHAR=ILAST-IFRST+1
                      DO5585K=1,MIN(4,NCHAR)
                        IVLIST(IVAR)(K:K)=IASAVE(IFRST+K-1)(1:1)
 5585                 CONTINUE
                      IF(NCHAR.GE.5)THEN
                        DO5587K=5,MIN(8,NCHAR)
                          IVLIS2(IVAR)(K-4:K-4)=IASAVE(IFRST+K-1)(1:1)
 5587                   CONTINUE
                      ENDIF
                      INEW=0
                    ENDIF
                  ELSE
                    ILAST=J
                    IF(INEW.EQ.0)THEN
                      INEW=1
                      IFRST=J
                    ENDIF
                  ENDIF
 5583           CONTINUE
                REWIND IOUNIT
                JMIN=1
                JMAX=IVAR
              ENDIF
C
C         STEP 3: CASE WHERE VARIABLE NAMES ON FIRST LINE
C
C                 2017/03: CHECK FOR "," AS SEPARATOR IN ADDITION
C                          TO SPACE CHARACTER.
C
            ELSEIF(IVARLA.EQ.'ON')THEN
              NUMCHA=-1
              CALL DPREFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1             IASAVE,NUMCHA,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
              IF(IERROR.EQ.'YES')GOTO8800
C
              IFRST=0
              ILAST=0
              INEW=0
              IVAR=0
              CALL DPUPPE(IASAVE,255,IASAVE,IBUGS2,IERROR)
              DO34583J=1,255
                IF(IASAVE(J)(1:1).EQ.' ' .OR. IASAVE(J)(1:1).EQ.',')THEN
                  IF(INEW.EQ.1)THEN
                    IVAR=IVAR+1
                    ILAST=J
                    NCHAR=ILAST-IFRST+1
                    DO34585K=1,MIN(4,NCHAR)
                      IVLIST(IVAR)(K:K)=IASAVE(IFRST+K-1)(1:1)
34585               CONTINUE
                    IF(NCHAR.GE.5)THEN
                      DO34587K=5,MIN(8,NCHAR)
                        IVLIS2(IVAR)(K-4:K-4)=IASAVE(IFRST+K-1)(1:1)
34587                 CONTINUE
                    ENDIF
                    INEW=0
                  ENDIF
                ELSE
                  ILAST=J
                  IF(INEW.EQ.0)THEN
                    INEW=1
                    IFRST=J
                  ENDIF
                ENDIF
34583         CONTINUE
C
              JMIN=1
              JMAX=IVAR
            ENDIF
C
          ENDIF
        ENDIF
      ENDIF
C
      IF(JMIN.GT.JMAX)GOTO4290
      IF(ICASRE.EQ.'ROWI')GOTO4290
C
C  JANUARY 2004.  THE DPREAL ROUTINE CAN NOW RETURN CHARACTER AS
C  WELL AS NUMERIC DATA.  FOR THE VARIABLE READ CASE, READ FIRST
C  LINE OF FILE TO EXTRACT THE TYPES FOR EACH OF THE VARIABLES.
C  FOLLOWING CONDITIONS NEED TO APPLY:
C
C  1. THIS IS THE VARIABLE (AS OPPOSSED TO STRING, PARAMETER, MATRIX)
C     READ CASE.
C
C  2. THE CONVERT CHARACTER CASE IS SET TO CHARACTER (AS OPPOSSED
C     TO IGNORE OR ERROR).  THIS IS DETERMINED BY VALUE OF IGRPAU.
C
C  3. THE READ IS FROM FILE RATHER THAN THE KEYBOARD.  FOR THE
C     KEYBOARD READ CASE, IGRPAU IS SET TO IGNORE.
C
C     2019/09: ALLOW READING OF CHARACTER DATA FROM TERMINAL.  PRIMARY
C              ISSUE IS THAT FIRST LINE IS READ TO DETERMINE THE
C              TYPE OF EACH FIELD.  FOR TERMINAL READ, CANNOT DO A
C              FILE REWIND, SO NEED TO SAVE THE FIRST LINE FOR
C              SUBSEQUENT USE.
C
C  4. IF A SET READ FORMAT HAS BEEN SET, NO CHARACTER DATA WILL
C     BE READ.
C
C  5. FOR NOW, READ CLIPBOARD WILL ONLY SUPPORT READING OF NUMERIC
C     VAIRABLES.
C
      ICFLAG='YES'
      IF(IGRPAU.NE.'CHAR' .AND. IGRPAU.NE.'CATE')ICFLAG='NO'
CCCCC IF(IOFILE.NE.'YES')ICFLAG='NO'
      IF(ICASRE.NE.'VARI')ICFLAG='NO'
      IF(NCREAF.GT.0)ICFLAG='NO'
      IF(ICASRE.EQ.'MATR')ICFLAG='NO'
      IF(ICASRE.EQ.'MATZ')ICFLAG='NO'
      IF(ICASRE.EQ.'IMAG')ICFLAG='NO'
      IF(ICASRE.EQ.'IMAZ')ICFLAG='NO'
      IF(ICASRE.EQ.'CLIP')ICFLAG='NO'
      IF(ICASRE.EQ.'ROWR')ICFLAG='NO'
      IFLGSV=0
C
      IF(ICFLAG.EQ.'YES')THEN
C
C        2018/07: CHECK IF ONE OF THE COLUMNS IS DESIGNATED TO
C                 BE A ROW LABEL.
C
        IRWLC2=-1
        IF(IRWLCO.GE.1)THEN
          IRWLC2=IRWLCO
        ENDIF
C
        MINCO2=1
        MAXCO2=NUMRCM
        IFCOL3=IFCOL1
        IFCOL4=IFCOL2
C
C       SKIP AUTOMATIC CASE: NEED TO READ UNTIL "----" FOUND
C
        IF(ISKIP.EQ.-1)THEN
          DO17382IFROW=1,MAXOBV
            NUMCHA=-1
            CALL DPREFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1                  IA,NUMCHA,
     1                  ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
            IF(IERROR.EQ.'YES')THEN
              IGRPA2='IGNO'
              ICFLAG='NO'
              GOTO17399
            ELSEIF(IA(1).EQ.'E'.AND.IA(2).EQ.'O'.AND.IA(3).EQ.'F'.AND.
     1        NUMCHA.EQ.3)THEN
              IGRPA2='IGNO'
              ICFLAG='NO'
              GOTO17399
            ELSEIF(IA(1).EQ.'-'.AND.IA(2).EQ.'-'.AND.IA(3).EQ.'-'.AND.
     1         IA(4).EQ.'-')THEN
                GOTO17391
              ENDIF
17382       CONTINUE
        ELSE
          ITEMP=IFROW1+ISKIP-1
          IF(ITEMP.GT.0)THEN
            DO17380IFROW=1,ITEMP
              NUMCHA=-1
              IF(IOTERM.EQ.'LOOP')THEN
                ILOOLI=ILOOLI+1
              ELSE
                CALL DPREFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,
     1                      IPROT,ICURST,IA,NUMCHA,
     1                      ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
                IF(IERROR.EQ.'YES')THEN
                  IGRPA2='IGNO'
                  ICFLAG='NO'
                  GOTO17399
                ENDIF
                IF(IA(1).EQ.'E'.AND.IA(2).EQ.'O'.AND.IA(3).EQ.'F'.AND.
     1            NUMCHA.EQ.3)THEN
                  IGRPA2='IGNO'
                  ICFLAG='NO'
                  GOTO17399
                ENDIF
              ENDIF
17380       CONTINUE
          ENDIF
        ENDIF
C
17391   CONTINUE
        NCALL=0
        NCOLS=0
        CALL DPREAL(IRD2,IFCOL3,IFCOL4,MINCO2,MAXCO2,X0,NUMDPL,IFLGSV,
     1              IXC,NXC,
     1              ICASRE,IFUNC2,N2,MAXN2,
     1              IMACRO,IMACNU,IMACCS,
     1              IANSLC,IWIDTH,IREACS,ISTOR1,ISTOR2,IEND,NUMLRD,
     1              IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1              ICOMCH,ICOMSW,LINETY,IGRPA2,
     1              IFCOLL,IFCOLU,ITYPE,NCOLS,NCALL,
     1              IREADL,IDATDL,ITIMDL,IRDIPA,PREAMV,
     1              MAXRDV,MAXCHV,IFIETY,
     1              IDECPT,IDATMV,IDATNN,
     1              IREACD,IREACM,IREADS,IREAPM,IREAMC,ITABNC,
     1              IREAAS,IREAPC,
     1              IB,
     1              IOTERM,IANSLO,MAXLIL,MAXCIL,ILOOST,ILOOLI,
     1              IREPCH,IMALEV,
     1              IERRFI,IBUGS2,ISUBRO,IERROR)
C
C       2019/04: CHECK FOR BLANK LINE BEFORE ERROR
C
        IF(LINETY.EQ.'BLAN')GOTO17391
        IF(IERROR.EQ.'YES')GOTO9000
C
        IF(IMNVAR.LT.0)IMNVAR=NUMDPL
        IF(IMXVAR.LT.0)IMXVAR=NUMDPL
        IF(NUMDPL.LT.IMNVAR)IMNVAR=NUMDPL
        IF(NUMDPL.GT.IMXVAR)IMXVAR=NUMDPL
C
        IF(NXC.LE.0)THEN
          ICFLAG='NO'
          IGRPA2='IGNO'
          GOTO17399
        ENDIF
C
17399   CONTINUE
        IF(IOFILE.EQ.'YES')THEN
          REWIND(IOUNIT)
          IFLGSV=0
        ELSE
          IFLGSV=1
        ENDIF
        NCALL=0
        NCOLS=0
      ENDIF
C
      ICNTNU=0
      ICNTCH=0
      ICOUNT=0
      IISKIP=0
C
      IF(ICASRE.EQ.'CLIP' .AND. IVRLST.EQ.'NO')GOTO4290
C
      DO4200J=JMIN,JMAX
C
        IF(IISKIP.EQ.1)THEN
          IISKIP=0
          GOTO4200
        ENDIF
C
        IF(ICFLAG.EQ.'NO' .OR. ICFLAG.EQ.'OFF')THEN
          ICOUNT=ICOUNT+1
        ENDIF
C
        IF(IVRLST.EQ.'NO')THEN
          IH1=IVLIST(J)
          IH2=IVLIS2(J)
        ELSE
          IH1=IHARG(J)
          IH2=IHARG2(J)
        ENDIF
C
C     **********
C     THE FOLLOWING 5 LINES OF CODE IS FOR      READ MATRIX.
C     IT ALLOWS COLUMN VECTOR NAMES TO BE FORMED
C     FROM THE BASE MATRIX NAME
C     BY THE APPENDING OF NUMBERS 1, 2, 3, ...
C     SEPTEMBER 1987
C     **********
C
        IF(ICASRE.EQ.'MATR')THEN
          IVALMA=IVALMA+1
          CALL DPAPN2(IHMAT1,IHMAT2,IVALMA,
     1                IH1,IH2,IBUGS2,ISUBRO,IERROR)
        ENDIF
C
C     ***************
C     THE FOLLOWING CODE ALLOWS THE    TO    KEYWORD
C     TO BE ACTIVATED, AS IN
C     READ FILE.EXT Y1 TO Y10
C     DECEMBER 1986
C     ***************
C
        ICASTO='OFF'
        IF(IH1.EQ.'TO  ')THEN
          ICASTO='ON'
          JM1=J-1
          JP1=J+1
          CALL DPEXTL(IHARG(JM1),IHARG2(JM1),IHARG(JP1),IHARG2(JP1),
     1                KNUMB,IVAL1,IVAL2,IBUGS2,ISUBRO,IERROR)
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
        ELSE
          IF(ICFLAG.EQ.'YES')THEN
            ICOUNT=ICOUNT+1
          ENDIF
          GOTO4219
        ENDIF
 4215   CONTINUE
        IVAL=IVAL+1
CCCCC   ICOUNT=ICOUNT+1
        IF(ICFLAG.EQ.'YES')THEN
          IF(IVAL.GT.IVAL2)GOTO4200
CCCCC     IF(IVAL.GE.IVAL2)GOTO4200
        ELSE
          IF(IVAL.GE.IVAL2)GOTO4200
        ENDIF
C
        CALL DPAPNU(IHARG(JM1),IHARG2(JM1),KNUMB,IVAL,
     1              IH1,IH2,IBUGS2,ISUBRO,IERROR)
 4219   CONTINUE
C
C  JANUARY 2004: CHECK WHETHER NAME SHOULD BE ADDED TO
C  REGULAR NAME LIST OR CHARACTER VARIABLE NAME LIST.
C
        IF(ICFLAG.EQ.'YES')THEN
C
          IF(ITYPE(ICOUNT).EQ.1)THEN
            ICNTCH=ICNTCH+1
            IF(ICNTCH.GT.MAXCHV)GOTO4200
            IFLGRL=0
            IF(IRWLC2.EQ.ICOUNT)THEN
              IRWLC3=ICNTCH
              IFLGRL=1
            ENDIF
            ICLIST(ICNTCH)=IH1
            ICLIS2(ICNTCH)=IH2
C
C           2018/07: CHECK IF THIS IS A PREVIOUSLY DEFINED NAME IF
C                    "CATEGORICAL" OPTION IS SET.  ONLY VARIABLE
C                    NAME IS ALLOWED.  OTHERWISE, REPORT AN ERROR.
C
            IF(IGRPAU.EQ.'CATE' .AND. IFLGRL.EQ.0)THEN
              DO42300II=1,NUMNAM
                I2=II
                IF(IH1.EQ.IHNAME(I2).AND.IH2.EQ.IHNAM2(I2))THEN
                  IF(IUSE(I2).NE.'V')THEN
                    WRITE(ICOUT,999)
                    CALL DPWRST('XXX','BUG ')
                    WRITE(ICOUT,211)
                    CALL DPWRST('XXX','BUG ')
                    WRITE(ICOUT,42320)
42320               FORMAT('      WHEN USING THE   SET CONVERT ',
     1                     'CHARACTER CATEGORICAL   COMMAND,')
                    CALL DPWRST('XXX','BUG ')
                    WRITE(ICOUT,42350)
42350               FORMAT('      THE REQUESTED NAME PREVIOUSLY ',
     1                     'EXISTS, BUT NOT AS A VARIABLE.')
                    CALL DPWRST('XXX','BUG ')
                    WRITE(ICOUT,4317)
                    CALL DPWRST('XXX','BUG ')
                    IERROR='YES'
                    GOTO8800
                  ELSE
                    IECOLC(ICNTCH)=IVALUE(I2)
                    GOTO42301
                  ENDIF
                ENDIF
42300         CONTINUE
              IECOLC(ICNTCH)=-1
42301         CONTINUE
            ENDIF
C
            IF(ICASTO.EQ.'ON')THEN
              IF(IVAL.GE.IVAL2)GOTO4200
              GOTO4215
            ELSE
              GOTO4200
            ENDIF
          ELSE
            ICNTNU=ICNTNU+1
          ENDIF
        ENDIF
C
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
C
              IF(ICASRE.EQ.'VARI' .OR. ICASRE.EQ.'CLIP')GOTO4370
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,211)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,4312)
 4312         FORMAT('      A NAME IN THE LIST OF VARIABLES TO BE ',
     1               'READ INCLUDED THE')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,4315)
 4315         FORMAT('      NAME OF A PREVIOUSLY-DEFINED PARAMETER OR ',
     1               'FUNCTION.')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,4316)IH1,IH2
 4316         FORMAT('      THE NAME OF THE PARAMETER OR FUNCTION WAS ',
     1               2A4,'   .')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,4317)
 4317         FORMAT('      NO READ WAS CARRIED OUT.')
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GOTO8800
C
            ELSEIF(IUSE(I).EQ.'P')THEN
              ICASEA='P'
              IP=IP+1
              IF(IP.GT.MAXP2)GOTO4370
              JPNAM1(IP)=IH1
              JPNAM2(IP)=IH2
              PVAL(IP)=VALUE(I2)
C
              IF(ICASRE.EQ.'PARA')GOTO4370
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,211)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,4322)
 4322         FORMAT('      A NAME IN THE LIST OF PARAMETERS TO BE ',
     1               'READ INCLUDED THE')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,4325)
 4325         FORMAT('      NAME OF A PREVIOUSLY-DEFINED VARIABLE OR ',
     1               'FUNCTION.')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,4326)IH1,IH2
 4326         FORMAT('      THE NAME OF THE VARIABLE OR FUNCTION WAS ',
     1               2A4,'   .')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,4317)
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GOTO8800
C
            ELSEIF(IUSE(I).EQ.'M')THEN
              ICASEA='M'
              IM=IM+1
              IF(IM.GT.MAXM2)GOTO4370
              JMNAM1(IM)=IH1
              JMNAM2(IM)=IH2
C
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,211)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,4332)
 4332         FORMAT('      A NAME IN THE LIST OF VARIABLES TO BE READ')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,4335)
 4335         FORMAT('      INCLUDED THE NAME OF A PREVIOUSLY-DEFINED ',
     1               'MODEL.')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,4336)IH1,IH2
 4336         FORMAT('      THE NAME OF THE MODEL WAS ',2A4,'  .')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,4317)
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GOTO8800
C
            ELSEIF(IUSE(I).EQ.'F')THEN
              ICASEA='F'
              IF=IF+1
              IF(IF.GT.MAXF2)GOTO4370
              JFNAM1(IF)=IH1
              JFNAM2(IF)=IH2
              IFSTA2(IF)=IVSTAR(I2)
              IFSTO2(IF)=IVSTOP(I2)
C
              IF(ICASRE.EQ.'FUNC' .OR. ICASRE.EQ.'CFUN')GOTO4370
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,211)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,4342)
 4342         FORMAT('      A NAME IN THE LIST OF FUNCTIONS (= ',
     1               'STRINGS)')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,4344)
 4344         FORMAT('      TO BE READ INCLUDED THE NAME OF A ')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,4345)
 4345         FORMAT('      PREVIOUSLY-DEFINED VARIABLE OR PARAMETER.')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,4346)IH1,IH2
 4346         FORMAT('      THE NAME OF THE VARIABLE OR PARAMETER WAS ',
     1               2A4,'   .')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,4317)
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GOTO8800
C
            ENDIF
          ENDIF
 4300   CONTINUE
C
        ICASEA='U'
        IU=IU+1
        IF(IU.GT.MAXU2)GOTO4370
        JUNAM1(IU)=IH1
        JUNAM2(IU)=IH2
        GOTO4370
C
 4370   CONTINUE
        IE=IE+1
        IF(IE.GT.MAXE2)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,211)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,4382)
 4382     FORMAT('      THE NUMBER OF NAMES IN THE READ COMMAND HAS')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,4384)MAXE2
 4384     FORMAT('      JUST EXCEEDED THE ALLOWABLE MAXIMUM (',I5,')')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO8800
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
C
        IF(ICASTO.EQ.'ON')GOTO4215
C
 4200 CONTINUE
 4290 CONTINUE
C
CCCCC FEBRUARY 2003: IF NO VARIABLES GIVEN, THEN WILL
CCCCC DETERMINE AUTOMATICALLY LATER ON.
C
      NUMV=IV
      NUMP=IP
      NUMM=IM
      NUMF=IF
      NUMU=IU
      NUME=IE
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
        WRITE(ICOUT,4411)NUMALL,NUMV,NUMP,NUMM,NUMF,NUMU,NUME
 4411   FORMAT('NUMALL,NUMV,NUMP,NUMM,NUMF,NUMU,NUME = ',7I6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4412)
 4412   FORMAT('I,JVNAM1(I),JVNAM2(I),JPNAM1(I),JPNAM2(I),
     1  JMNAM1(I),JMNAM2(I),JFNAM1(I),JFNAM2(I),JUNAM1(I),JUNAM2(I)')
        CALL DPWRST('XXX','BUG ')
        DO4420I=1,15
          WRITE(ICOUT,4421)I,JVNAM1(I),JVNAM2(I),JPNAM1(I),JPNAM2(I),
     1    JMNAM1(I),JMNAM2(I),JFNAM1(I),JFNAM2(I),JUNAM1(I),JUNAM2(I)
 4421     FORMAT(I8,5X,2A4,1X,2A4,1X,2A4,1X,2A4,1X,2A4)
          CALL DPWRST('XXX','BUG ')
 4420   CONTINUE
      ENDIF
C
C               ***************************************************
C               **  STEP 5--                                     **
C               **  CHECK FOR A VALID NUMBER                     **
C               **  (1 TO 100) OF VARIABLES TO BE READ           **
C               **  (NOTE--THIS DOES NOT INCLUDE PARAMETERS      **
C               **  OR MODELS IN THE ABOVE COUNT--               **
C               **  ONLY VARIABLES.)                             **
C               **  CHECK FOR A VALID NUMBER                     **
C               **  (0 TO 100) OF CONSTANTS TO BE READ   .       **
C               **  CHECK FOR A VALID NUMBER                     **
C               **  (0 TO 100) OF MODELS TO BE READ   .          **
C               **  CHECK FOR A VALID NUMBER                     **
C               **  (0 TO 100) OF FUNCTIONS TO BE READ   .       **
C               **  CHECK FOR A VALID NUMBER                     **
C               **  (1 TO 100) OF UNKNOWNS TO BE READ   .        **
C               ***************************************************
C
      IF(ICASRE.EQ.'CLIP' .AND. IVRLST.EQ.'NO')GOTO7001
C
      IF(NUMV.LT.0 .OR. NUMV.GT.MAXV2)THEN
C
        WRITE(ICOUT,211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,512)
  512   FORMAT('      FOR A READ, THE NUMBER OF VARIABLES (NOT ',
     1         'COUNTING')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,514)MAXV2
  514   FORMAT('      PARAMETERS OR MODELS) MUST BE AT MOST ',I8,'  .')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,515)
  515   FORMAT('      SUCH WAS NOT THE CASE HERE.  THE SPECIFIED')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,517)NUMV
  517   FORMAT('      NUMBER OF VARIABLES TO BE READ    WAS ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,518)MAXV2
  518   FORMAT('      NOTE--ONLY THE FIRST ',I8,' VARIABLES WILL BE ',
     1         'READ.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,520)
  520   FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,521)(IANSLC(I),I=1,MIN(80,IWIDTH))
  521     FORMAT(80A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
      IF(NUMP.LT.0 .OR. NUMP.GT.MAXP2)THEN
C
        WRITE(ICOUT,211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,532)
  532   FORMAT('      FOR A READ, THE NUMBER OF PARAMETERS ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,534)MAXP2
  534   FORMAT('      (CONSTANTS) MUST BE AT MOST ',I8,'  ;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,535)
  535   FORMAT('      SUCH WAS NOT THE CASE HERE.  THE SPECIFIED')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,537)NUMP
  537   FORMAT('      NUMBER OF PARAMETERS TO BE READ    WAS ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,538)MAXP2
  538   FORMAT('      NOTE--ONLY THE FIRST ',I8,' PARAMETERS WILL ',
     1         'BE READ.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,520)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,521)(IANSLC(I),I=1,MIN(80,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
      IF(NUMM.LT.0 .OR. NUMM.GT.MAXM2)THEN
C
        WRITE(ICOUT,211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,553)
  553   FORMAT('      FOR A READ, THE NUMBER OF MODELS MUST BE AT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,555)MAXM2
  555   FORMAT('      MOST ',I8,'  .  SUCH WAS NOT THE CASE HERE;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,556)NUMM
  556   FORMAT('      THE SPECIFIED NUMBER OF MODELS TO BE READ WAS ',
     1         I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,558)MAXM2
  558   FORMAT('      NOTE--ONLY THE FIRST ',I8,' MODELS WILL BE ',
     1         'READ.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,520)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,521)(IANSLC(I),I=1,MIN(80,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
      IF(NUMF.LT.0 .OR. NUMF.GT.MAXF2)THEN
C
        WRITE(ICOUT,211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,572)
  572   FORMAT('      FOR A READ, THE NUMBER OF FUNCTIONS MUST BE AT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,575)MAXF2
  575   FORMAT('      MOST ',I8,'  .  SUCH WAS NOT THE CASE HERE;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,576)NUMF
  576   FORMAT('      THE SPECIFIED NUMBER OF FUNCTIONS TO BE READ ',
     1         'WAS ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,578)MAXF2
  578   FORMAT('      NOTE--ONLY THE FIRST ',I8,' FUNCTIONS WILL BE ',
     1         'READ.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,520)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,521)(IANSLC(I),I=1,MIN(80,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
      IF(NUMU.LT.0 .OR. NUMU.GT.MAXU2)THEN
C
        WRITE(ICOUT,211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,612)
  612   FORMAT('      FOR A READ, THE NUMBER OF UNKNOWNS MUST BE AT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,614)MAXU2
  614   FORMAT('      MUST BE AT MOST ',I8,';  SUCH WAS NOT THE CASE ',
     1         'HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,617)NUMU
  617   FORMAT('      THE SPECIFIED NUMBER OF UNKNOWNS TO BE READ WAS ',
     1         I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,618)MAXU2
  618   FORMAT('      NOTE--ONLY THE FIRST ',I8,' UNKNOWNS WILL BE ',
     1         'READ.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,520)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,521)(IANSLC(I),I=1,MIN(80,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
      ENDIF
C
      IF(ICASRE.EQ.'MATZ' .AND. NUME.NE.3)THEN
C
        WRITE(ICOUT,211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,632)
  632   FORMAT('      FOR THE   READ MATRIX TO VARIABLES   CASE, THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,633)
  633   FORMAT('      NUMBER OF VARIABLES TO BE READ MUST BE EXACTLY')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,635)
  635   FORMAT('      THREE.  SUCH WAS NOT THE CASE HERE;  THE ',
     1         'SPECIFIED')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,517)NUMV
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,520)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,521)(IANSLC(I),I=1,MIN(80,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(ICASRE.EQ.'STAC' .AND. NUME.NE.2)THEN
C
        WRITE(ICOUT,211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,642)
  642   FORMAT('      FOR THE   READ STACK VARIABLES   CASE, THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,643)
  643   FORMAT('      NUMBER OF VARIABLES TO BE READ MUST BE EXACTLY')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,645)
  645   FORMAT('      TWO.  SUCH WAS NOT THE CASE HERE;  THE ',
     1         'SPECIFIED')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,517)NUMV
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,520)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,521)(IANSLC(I),I=1,MIN(80,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(ICASRE.EQ.'IMAZ')THEN
        IF(NUME.NE.3 .AND. NUME.NE.5)THEN
C
          WRITE(ICOUT,211)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,652)
  652     FORMAT('      FOR THE   READ IMAGE TO VARIABLES   CASE, ',
     1           'THE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,653)
  653     FORMAT('      NUMBER OF VARIABLES TO BE READ MUST BE ',
     1           'EITHER THREE OR')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,655)
  655     FORMAT('      FIVE.  SUCH WAS NOT THE CASE HERE;  THE ',
     1           'SPECIFIED')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,517)NUMV
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,520)
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,521)(IANSLC(I),I=1,MIN(80,IWIDTH))
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IERROR='YES'
          GOTO9000
        ENDIF
      ENDIF
C
      IF(ICASRE.EQ.'IMAG')THEN
        IF(NUME.NE.1 .AND. NUME.NE.3)THEN
C
          WRITE(ICOUT,211)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,662)
  662     FORMAT('      FOR THE   READ IMAGE   CASE, THE NUMBER OF')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,663)
  663     FORMAT('      VARIABLES TO BE READ MUST BE EITHER ONE OR')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,665)
  665     FORMAT('      THREE.  SUCH WAS NOT THE CASE HERE;  THE ',
     1           'SPECIFIED')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,517)NUMV
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,520)
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,521)(IANSLC(I),I=1,MIN(80,IWIDTH))
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IERROR='YES'
          GOTO9000
        ENDIF
      ENDIF
C
      IF(ICASRE.EQ.'ROWR')THEN
C
        IF(IOFILE.EQ.'YES')THEN
          IF(NUMARG.GE.3)THEN
            IVBASE(1:4)=IHARG(3)(1:4)
            IVBASE(5:8)=IHARG2(3)(1:4)
            NUME=1
          ELSE
            NUME=0
          ENDIF
        ELSE
          IF(NUMARG.GE.2)THEN
            IVBASE(1:4)=IHARG(2)(1:4)
            IVBASE(5:8)=IHARG2(2)(1:4)
            NUME=1
          ELSE
            NUME=0
          ENDIF
        ENDIF
C
        IF(NUME.LT.1)THEN
C
          WRITE(ICOUT,211)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,666)
  666     FORMAT('      FOR THE   ROW READ CASE, THE NUMBER OF')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,667)
  667     FORMAT('      VARIABLES TO BE READ MUST BE ONE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,668)
  668     FORMAT('      SUCH WAS NOT THE CASE HERE;  THE SPECIFIED')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,517)NUMV
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,520)
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,521)(IANSLC(I),I=1,MIN(80,IWIDTH))
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IERROR='YES'
          GOTO9000
        ENDIF
      ENDIF
C
      IF(ICASRE.NE.'ROWI' .AND. NUME.LT.1 .AND. ICNTCH.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4452)
 4452   FORMAT('      NO VARIABLE NAMES WERE PROVIDED IN THE READ ',
     1         'STATEMENT,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4453)
 4453   FORMAT('      HENCE NO READ WAS CARRIED OUT.  ILLUSTRATIVE ',
     1         'EXAMPLE TO')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4455)
 4455   FORMAT('      DEMONSTRATE THE PROPER FORM FOR THE READ ',
     1         'COMMAND--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4456)
 4456   FORMAT('      SUPPOSE THE ANALYST WISHES TO READ DATA FROM ',
     1         'FILE   CALIB.')
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
        GOTO8800
      ENDIF
C
C               *******************************************************
C               **  STEP 6--                                         **
C               **  THOSE NAMES WHICH ARE OF THE UNKNOWN CATEGORY    **
C               **  WILL BECOME  FUTURE VARIABLES/PARAMETERS/FUNCTIONS.*
C               **  ASSIGN THESE VARIABLES TO THE NEXT AVAILABLE     **
C               **  COLUMNS, AND UPDATE THE NAME TABLE ACCORDINGLY.  **
C               *******************************************************
C
      IF(NUME.GT.0 .AND. ICASRE.NE.'ROWR')THEN
        INAM=NUMNAM
        IF(ICASRE.EQ.'VARI')ICOL=NUMCOL
        IF(ICASRE.EQ.'STAC')ICOL=NUMCOL
        IF(ICASRE.EQ.'MATZ')ICOL=NUMCOL
        IF(ICASRE.EQ.'IMAZ')ICOL=NUMCOL
        IF(ICASRE.EQ.'CLIP')ICOL=NUMCOL
        DO700IE=1,NUME
          IF(ICASRE.EQ.'VARI'.AND.IECASE(IE).EQ.'OLD')GOTO700
          IF(ICASRE.EQ.'PARA'.AND.IECASE(IE).EQ.'OLD')GOTO700
          IF(ICASRE.EQ.'FUNC'.AND.IECASE(IE).EQ.'OLD')GOTO700
          IF(ICASRE.EQ.'CFUN'.AND.IECASE(IE).EQ.'OLD')GOTO700
          IF(ICASRE.EQ.'MATR'.AND.IECASE(IE).EQ.'OLD')GOTO700
          IF(ICASRE.EQ.'MATZ'.AND.IECASE(IE).EQ.'OLD')GOTO700
          IF(ICASRE.EQ.'IMAG'.AND.IECASE(IE).EQ.'OLD')GOTO700
          IF(ICASRE.EQ.'IMAZ'.AND.IECASE(IE).EQ.'OLD')GOTO700
          IF(ICASRE.EQ.'STAC'.AND.IECASE(IE).EQ.'OLD')GOTO700
          IF(ICASRE.EQ.'CLIP'.AND.IECASE(IE).EQ.'OLD')GOTO700
          IF(ICASRE.EQ.'VARI'.AND.IECOL2(IE).GE.1)GOTO700
          IF(ICASRE.EQ.'STAC'.AND.IECOL2(IE).GE.1)GOTO700
          IF(ICASRE.EQ.'MATR')GOTO700
          INAM=INAM+1
          IF(ICASRE.EQ.'VARI')ICOL=ICOL+1
          IF(ICASRE.EQ.'STAC')ICOL=ICOL+1
          IF(ICASRE.EQ.'MATZ')ICOL=ICOL+1
          IF(ICASRE.EQ.'IMAZ')ICOL=ICOL+1
          IF(ICASRE.EQ.'CLIP')ICOL=ICOL+1
C
          IF(INAM.GT.MAXNAM)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,211)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,712)
  712       FORMAT('      THE NUMBER OF NAMES (VARIABLES + PARAMETERS')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,714)
  714       FORMAT('      + FUNCTIONS HAS JUST EXCEEDED THE MAXIMUM ',
     1             'SIZE')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,715)MAXNAM
  715       FORMAT('      (',I5,') OF THE INTERNAL NAME TABLE.')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO8800
          ENDIF
C
          IF(ICASRE.NE.'PARA' .AND. ICASRE.NE.'FUNC' .AND.
     1       ICASRE.NE.'CFUN' .AND. ICASRE.NE.'ROWI' .AND.
     1       ICOL.GT.MAXCOL)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,211)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,722)
  722       FORMAT('      THE NUMBER OF COLUMNS IN THE INTERNAL ',
     1             'DATAPLOT DATA')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,724)MAXCOL
  724       FORMAT('      ARRAY HAS JUST EXCEEDED THE ALLOWABLE ',
     1             'MAXIMUM (',I5,')')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO8800
          ENDIF
C
          IHNAME(INAM)=JENAM1(IE)
          IHNAM2(INAM)=JENAM2(IE)
          IF(ICASRE.EQ.'PARA')IUSE(INAM)='P'
          IF(ICASRE.EQ.'FUNC')IUSE(INAM)='F'
          IF(ICASRE.EQ.'CFUN')IUSE(INAM)='F'
          IF(ICASRE.EQ.'VARI' .OR. ICASRE.EQ.'MATZ' .OR.
     1       ICASRE.EQ.'STAC' .OR. ICASRE.EQ.'IMAZ' .OR.
     1       ICASRE.EQ.'CLIP')THEN
            IUSE(INAM)='V'
            IVALUE(INAM)=ICOL
            IECOL2(IE)=ICOL
            IN(INAM)=0
          ENDIF
  700   CONTINUE
        NUMNAM=INAM
        IF(ICASRE.EQ.'VARI' .OR. ICASRE.EQ.'MATZ' .OR.
     1     ICASRE.EQ.'STAC' .OR. ICASRE.EQ.'IMAZ' .OR.
     1     ICASRE.EQ.'CLIP')NUMCOL=ICOL
C
C       2018/07: IF CONVERTING CHARACTER DATA TO CATEGORICAL DATA,
C                ADD NUMERIC VARIABLE NAMES TO NAME TABLE.
C
        IF(IGRPAU.EQ.'CATE' .AND. ICNTCH.GT.0 .AND.
     1     IRWLC3.NE.ICNTCH)THEN
          DO70000IE=1,ICNTCH
            IF(IECOLC(IE).GE.1)GOTO70000
            INAM=INAM+1
            ICOL=ICOL+1
C
            IF(INAM.GT.MAXNAM)THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,211)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,712)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,714)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,715)MAXNAM
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GOTO8800
            ENDIF
C
            IF(ICOL.GT.MAXCOL)THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,211)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,722)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,724)MAXCOL
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GOTO8800
            ENDIF
C
            IHNAME(INAM)=ICLIST(IE)
            IHNAM2(INAM)=ICLIS2(IE)
            IUSE(INAM)='V'
            IVALUE(INAM)=ICOL
            IECOLC(IE)=ICOL
            IN(INAM)=0
70000     CONTINUE
          NUMNAM=INAM
          NUMCOL=ICOL
        ENDIF
      ENDIF
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,791)NUMNAM,NUMCOL,NUMNAM,ICASRE
  791   FORMAT('NUMNAM,NUMCOL,NUMNAM,ICASRE = ',3I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C             ********************************************************
C             **  STEP 7--                                          **
C             **  FIRST, BRANCH TO THE APPROPRIATE SUBCASE          **
C             **  (DEPENDING ON WHETHER UNQUALIFIED, SUBSET OR FOR);**
C             **  THE DETERMINE THE LENGTH OF THE LONGEST           **
C             **  VARIABLE TO BE READ    IN ;                       **
C             **  THEN READ IN  THE VARIABLES                       **
C             **  THAT WERE SPECIFIED.                              **
C             ********************************************************
C
 7001 CONTINUE
C
      ISTEPN='7'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      MAXNRD=MAXN
      IF(IREASB.EQ.'P-P ')ICASEQ='FULL'
      IF(ICASEQ.EQ.'SUBS')THEN
        NIOLD=MAXNRD
        CALL DPSUBS(NIOLD,ILOCS,NS,IBUGQ,IERROR)
        NQ2=NIOLD
      ELSEIF(ICASEQ.EQ.'FOR')THEN
        NIOLD=MAXNRD
        CALL DPFOR(NIOLD,NFOR,IROW1,IROWN,
     1             NLOCAL,ILOCS,NS,IBUGQ,IERROR)
        NQ2=NFOR
      ELSE
        DO7315I=1,MAXNRD
          ISUB(I)=1
 7315   CONTINUE
        NQ2=MAXNRD
      ENDIF
C
C               *******************************************
C               **  STEP 8--                             **
C               **  IF A DATA ROW MINIMUM EXISTS AND SO  **
C               **  OUR ATTENTION IS FOCUSED ONLY ON     **
C               **  CERTAIN ROWS OF THE DATA FILE,       **
C               **  THEN GO DOWN TO THE FIRST SUCH ROW   **
C               **  IN THE FILE.                         **
C               *******************************************
C
      ISTEPN='8'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IFMFLG.EQ.'ON' .OR. IFROW1.LE.1 .OR. ICASRE.EQ.'IMAZ' .OR.
     1   ICASRE.EQ.'IMAG' .OR. ICASRE.EQ.'CLIP' .OR.
     1   ICASRE.EQ.'CFUN')GOTO7369
        IFRMIN=1
        IFRMAX=IFROW1-1
        IF(IFRMIN.GT.IFRMAX)GOTO7369
        MINCO2=1
        MAXCO2=NUMRCM
        IF(IRD2.EQ.IRD)MAXCO2=255
        IFCOL3=IFCOL1
        IFCOL4=IFCOL2
C       THE FOLLOWING 2 LINES WERE INSERTED FEBRUARY 1988
C       TO "TURN OFF" THE    COLUMN LIMITS    IF READING FROM A NON-FILE
C       (THAT IS, IF READING FROM THE TERMINAL OR WITHIN A MACRO).
        IF(IOFILE.EQ.'NO')THEN
          IFCOL3=MINCO2
          IFCOL4=MAXCO2
        ENDIF
        IF(IRD2.EQ.IRD.AND.IFCOL4.GT.MAXCO2)IFCOL4=MAXCO2
C
        DO7360IFROW=IFRMIN,IFRMAX
          IF(IOFILE.EQ.'NO')THEN
            READ(IRD2,7361,END=7363,ERR=7363)IJUNK
 7361       FORMAT(A1)
          ELSEIF(IOFILE.EQ.'YES')THEN
            NUMCHA=-1
            CALL DPREFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1                  IA,NUMCHA,
     1                  ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
            IF(IERROR.EQ.'YES')GOTO8800
            IF(IA(1).EQ.'E'.AND.IA(2).EQ.'O'.AND.IA(3).EQ.'F'.AND.
     1        NUMCHA.EQ.3)GOTO7363
          ENDIF
          GOTO7360
C
 7363     CONTINUE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,211)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7365)
 7365     FORMAT('      END OF FILE ENCOUNTERED WHILE SKIPPING OVER',
     1           'HEADER LINES.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7367)
 7367     FORMAT('      NOTE SKIP AND ROW LIMITS SETTINGS--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7368)ISKIP,IFROW1,AFROW2
 7368     FORMAT('      ISKIP,IFROW1,IFROW2 = ',2I8,2X,G15.7)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO8800
C
 7360   CONTINUE
 7369 CONTINUE
C
C               *******************************************
C               **  STEP 9--                             **
C               **  IN ADDITION, IF HEADER (= NON-DATA)  **
C               **  LINES EXIST WHICH ARE TO BE SKIPPED  **
C               **  OVER IN THE READ, DO SO HERE.        **
C               *******************************************
C
      ISTEPN='9'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IFMFLG.EQ.'ON' .OR. IOFILE.EQ.'NO' .OR. ICASRE.EQ.'IMAZ' .OR.
     1  ICASRE.EQ.'IMAG' .OR. ICASRE.EQ.'CLIP' .OR.
     1  ICASRE.EQ.'CFUN')GOTO7389
C
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IF(IFROW1.LE.1)THEN
          WRITE(ICOUT,7371)
 7371     FORMAT('THE NUMBER OF HEADER LINES')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(IFROW1.GE.2)THEN
          WRITE(ICOUT,7372)
 7372     FORMAT('THE NUMBER OF (ADDITIONAL) HEADER LINES')
          CALL DPWRST('XXX','BUG ')
        ENDIF
        WRITE(ICOUT,7373)ISKIP
 7373   FORMAT('    BEING SKIPPED = ',I6)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
CCCCC OCTOBER 1997.  SUPPORT "SKIP AUTOMATIC", DENOTED BY ISKIP = -1.
CCCCC READ UNTIL FIND "----".  IF "----" IS NOT FOUND, REWIND THE
CCCCC FILE, AND START READ FROM LINE 1.  ALSO, IF READING FROM
CCCCC THE TERMINAL, THEN THIS OPTION DOESN'T MAKE SENSE, SO
CCCCC ASSUME ISKIP = 0 IN THIS CASE.
C
      IF(ISKIP.EQ.-1.AND.IOFILE.EQ.'YES'.AND.
     1   ICASRE(1:3).NE.'IMA')THEN
        IFRMIN=1
        MINCO2=1
        MAXCO2=NUMRCM
        IF(IRD2.EQ.IRD)MAXCO2=255
        IFCOL3=IFCOL1
        IFCOL4=IFCOL2
        IF(IRD2.EQ.IRD.AND.IFCOL4.GT.MAXCO2)IFCOL4=MAXCO2
        DO7378I=1,50000
          NUMCHA=-1
          CALL DPREFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1                IA,NUMCHA,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
C
          IF(IERROR.EQ.'YES')GOTO8800
          IF(IA(1).EQ.'-'.AND.IA(2).EQ.'-'.AND.IA(3).EQ.'-'.AND.
     1      IA(4).EQ.'-')THEN
            GOTO7389
          ENDIF
          IF(NUMCHA.GE.5)THEN
            DO7379LL=1,NUMCHA-3
              IF(IA(LL).EQ.'-'.AND.IA(LL+1).EQ.'-'.AND.
     1          IA(LL+2).EQ.'-'.AND.IA(LL+3).EQ.'-')THEN
                GOTO7389
              ENDIF
 7379       CONTINUE
          ENDIF
          IF(IA(1).EQ.'E'.AND.IA(2).EQ.'O'.AND.IA(3).EQ.'F'.AND.
     1      NUMCHA.EQ.3)THEN
            REWIND IOUNIT
            GOTO7389
          ENDIF
 7378   CONTINUE
      ENDIF
C
      IF(ISKIP.LE.0)GOTO7389
      IFRMIN=IFROW1
      IFRMAX=IFROW1+ISKIP-1
      IF(IFRMIN.GT.IFRMAX)GOTO7389
      MINCO2=1
      MAXCO2=NUMRCM
      IF(IRD2.EQ.IRD)MAXCO2=255
      IFCOL3=IFCOL1
      IFCOL4=IFCOL2
C     THE FOLLOWING 2 LINES WERE INSERTED FEBRUARY 1988
C     TO "TURN OFF" THE    COLUMN LIMITS    IF READING FROM A NON-FILE
C     (THAT IS, IF READING FROM THE TERMINAL OR WITHIN A MACRO).
      IF(IOFILE.EQ.'NO')THEN
        IFCOL3=MINCO2
        IFCOL4=MAXCO2
      ENDIF
      IF(IRD2.EQ.IRD.AND.IFCOL4.GT.MAXCO2)IFCOL4=MAXCO2
      DO7380IFROW=IFRMIN,IFRMAX
        IF(IOFILE.EQ.'NO')THEN
          READ(IRD2,7382,END=7383)IJUNK
 7382     FORMAT(A1)
        ELSEIF(IOFILE.EQ.'YES')THEN
          NUMCHA=-1
          CALL DPREFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1                IA,NUMCHA,
     1                ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
          IF(IERROR.EQ.'YES')GOTO8800
          IF(IA(1).EQ.'E'.AND.IA(2).EQ.'O'.AND.IA(3).EQ.'F'.AND.
     1      NUMCHA.EQ.3)GOTO7383
        ENDIF
        GOTO7380
C
 7383   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7385)
 7385   FORMAT('      END OF FILE ENCOUNTERED WHILE SKIPPING OVER ',
     1         'HEADER')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7387)
 7387   FORMAT('      LINES.  NOTE SKIP AND ROW LIMITS SETTINGS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7388)ISKIP,IFROW1,AFROW2
 7388   FORMAT('      ISKIP,IFROW1,AFROW2 = ',2I8,2X,E15.7)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO8800
C
 7380 CONTINUE
 7389 CONTINUE
C
C               ************************
C               **  STEP 10--         **
C               **  READ IN THE DATA  **
C               ************************
C
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
        ISTEPN='10'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7210)NUME,IRD,IRD2,IFLGSV,IB(1),IB(2)
 7210   FORMAT('NUME,IRD,IRD2,IFLGSV,IB(1),IB(2),IB(2) = ',4I8,2(2X,A4))
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C     2019/09: DON'T INITIALIZE IB IF READING FIRST LINE WITH
C              POSSIBLY CHARACTER DATA FROM THE TERMINAL.
C
      IF(IFLGSV.EQ.0)THEN
        DO7260I=1,MAXRCL
          ISTOR1(I)=' '
          ISTOR2(I)=' '
          ISTOR3(I)=' '
          IB(I)=' '
 7260   CONTINUE
      ELSE
        DO7261I=1,MAXRCL
          ISTOR1(I)=' '
          ISTOR2(I)=' '
          ISTOR3(I)=' '
 7261   CONTINUE
      ENDIF
C
      IF(NUME.GT.0)THEN
        DO7300I=1,NUME
          IEN(I)=0
 7300   CONTINUE
      ENDIF
C
      MINCO2=1
      MAXCO2=NUMRCM
      IF(IRD2.EQ.IRD)MAXCO2=255
      IFCOL3=IFCOL1
      IFCOL4=IFCOL2
C     THE FOLLOWING 2 LINES WERE INSERTED FEBRUARY 1988
C     TO "TURN OFF" THE    COLUMN LIMITS    IF READING FROM A NON-FILE
C     (THAT IS, IF READING FROM THE TERMINAL OR WITHIN A MACRO).
      IF(IOFILE.EQ.'NO')THEN
        IFCOL3=MINCO2
        IFCOL4=MAXCO2
      ENDIF
      IF(IRD2.EQ.IRD.AND.IFCOL4.GT.MAXCO2)IFCOL4=MAXCO2
C
      I=0
      IIN=0
      NUMLRD=0
      IENDTY=1
CCCCC THE FOLLOWING LINE WAS ADDED    JUNE 1990
CCCCC TO FIX FORMATTED READ YIELDING ONLY 1 LINE   JUNE 1990
      IEND='NO'
      IF(ISKIP.GE.0)THEN
        IFRMIN=IFROW1+ISKIP
        IF(ICASEQ.EQ.'FOR')IFRMIN=IFROW1+ISKIP+IROW1-1
      ELSE
        IFRMIN=1
        IF(ICASEQ.EQ.'FOR')IFRMIN=IROW1
      ENDIF
C
CCCCC OCTOBER 2004: ACCOUNT FOR SUBSET/FOR CLAUSE LIMITS
C
      IFRMAX=IFROW2
      IF(ICASEQ.EQ.'FOR')THEN
        IFRMAX=MIN(IFROW2,IROWN)
      ENDIF
C
      IF(ICASRE.EQ.'PARA' .OR. ICASRE.EQ.'FUNC')IFRMAX=IFRMIN
      IF(IHOST1.EQ.'CDC'.AND.IFRMAX.GT.130000)IFRMAX=130000
      IF(IFRMAX.GE.IBILLI)IFRMAX=IBILLI
      IF(IFRMIN.GT.IFRMAX)GOTO7470
CCCCC APRIL 1995.  CHECK FOR UNFORMATTED READ CASE.
CCCCC INITIAL IMPLEMENTATION ONLY APPLIES TO VARIABLES (NOT
CCCCC STRINGS, FUNCTIONS, MATRICES).
CCCCC 1) THE FOLLOWING COMMAND:
CCCCC        SET UNFORMATTED COLUMNS <N>
CCCCC    SPECIFIES THE NUMBER OF COLUMNS WHEN READING A MATRIX
CCCCC 2) UNFORMATTED READ ASSUMES A "SQUARE MATRIX" OF NUMBERS 
CCCCC    CONTAINING ONLY REAL NUMBERS WAS WRITTEN (THAT IS, ASSUME
CCCCC    A SINGLE WRITE PERFORMED, NOT A MIXTURE OF DIFFERENT TYPES
CCCCC    ETC.).  THE FOLLOWING 2 COMMANDS PROVIDE A LIMITED AMOUNT
CCCCC    OF FLEXIBILITY:
CCCCC        SET UNFORMATTED OFFSET <VALUE>
CCCCC        SET UNFORMATTED RECORDS <VALUE>
CCCCC    THE FIRST COMMAND SPECIFIES THE NUMBER OF DATA VALUES TO
CCCCC    SKIP AT THE BEGINING OF THE FILE.  THE SECOND COMMAND 
CCCCC    SPECIFIES THE NUMBER OF DATA VALUES TO READ.
CCCCC 3) THERE ARE ESSENTIALLY 2 WAYS TO CREATE THE UNFORMATTED
CCCCC    FILE.  FOR EXAMPLE, ASSUME WRITING 10,000 ROWS OF VARIABLES
CCCCC    X AND Y.  THEN CAN WRITE AS:
CCCCC    A)    WRITE(IUNIT) X,Y
CCCCC    B)    WRITE(IUNIT) (X(I),Y(I),I=1,N)
CCCCC    THE DISTINCTION IS THAT (A) WRITES ALL OF X AND THEN ALL OF
CCCCC    Y WHEREAS (B) WRITES X(1), Y(1), X(2), Y(2), ..., X(N), Y(N).
CCCCC    INITIAL IMPLEMENTATION ASSUMES (B) SINCE THIS CORRESPONDS
CCCCC    TO DATAPLOT'S STORING BY COLUMN.  THE
CCCCC    "SET READ UNFORMATTED-COLUMNWISE" COMMAND SPECIFIES THAT
CCCCC    METHOD (A) WAS USED TO CREATE THE FILE.
CCCCC DATAPLOT WILL READ ENTIRE UNFORMATTED FILE INTO "XSCRT"
CCCCC ARRAY.  IT WILL CHECK HOW MANY DATA VALUES WERE READ.  IT THEN
CCCCC DIVIDES THIS BY NUMBER OF VARIABLES TO BE READ.  THE DO7400
CCCCC LOOP BELOW THEN EXTRACTS EACH ROW OF DATA FROM THIS XSCRT
CCCCC ARRAY.
C
      IF(IFMFLG.EQ.'ON'.AND.ICASRE.NE.'IMAZ'.AND.ICASRE.NE.'IMAG')THEN
C
        IF(IUNFOF.GT.2*MAXOBV)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,11212)IUNFOF,2*MAXOBW
11212     FORMAT('****** ERROR: OFFSET OF ',I8,' IS GREATER THAN ',
     1           'MAXIMUM ALLOWED OF ',I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        DO11002JJ=1,3*MAXOBW
          XSCRT(JJ)=CPUMIN
11002   CONTINUE
C
        IF(ICASRE.EQ.'MATR')NUME=IUNFMC
C
CCCCC   JULY 1996.  SGI DOESN'T READ IF XSCRT DIMENSIONED BIGGER
CCCCC   THAN NUMBER OF DATA POINTS IN FILE.  USER MAY NEED TO SPECIFY
CCCCC   THE COMMAND "SET UNFORMATTED RECORDS <N>".
C
        IF(IUNFNR.GT.0)THEN
          READ(IRD2,ERR=11080,END=11090,IOSTAT=JSTATS)
     1    (XSCRT(LL),LL=1,IUNFNR+IUNFOF)
        ELSE
          READ(IRD2,ERR=11080,END=11090,IOSTAT=JSTATS)XSCRT
        ENDIF
        GOTO11090
C
11080   CONTINUE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,11081)JSTATS
11081     FORMAT('****** ERROR TRYING TO READ AN UNFORMATTED FILE, ',
     1           'STATUS NUMBER = ',I8,'.')
          CALL DPWRST('XXX','BUG ')
        GOTO11090
C
11090   CONTINUE
        NSTOP=MAXOBW+IUNFOF
        IF(IUNFNR.GT.0)NSTOP=IUNFNR+IUNFOF
        DO11100JJ=NSTOP,1,-1
          NPTS=JJ
          IF(XSCRT(JJ).NE.CPUMIN)GOTO11109
11100   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,11111)
11111   FORMAT('****** ERROR: NO DATA FOUND IN THE UNFORMATTED FILE.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
11109   CONTINUE
        NPTS=NPTS-IUNFOF
        IFRMIN=1
        IFRMAX=NPTS/NUME
C
CCCCC OCTOBER 2014.  CHECK FOR READ FROM CLIPBOARD CASE.
CCCCC                DATAPLOT WILL READ ALL VALUES IN THE CLIPBORARD TO
CCCCC                THE "XSCRT" ARRAY.  IT WILL RETURN HOW MANY DATA
CCCCC                VALUES WERE READ.  IT THEN DIVIDES THIS BY NUMBER
CCCCC                OF VARIABLES TO BE READ.  THE DO7400 LOOP BELOW
CCCCC                THEN EXTRACTS EACH ROW OF DATA FROM THIS XSCRT
CCCCC                ARRAY.
C
      ELSE IF(ICASRE.EQ.'CLIP')THEN
C
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
          WRITE(ICOUT,11203)
11203     FORMAT('BEFORE CALL DPCLIP')
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IRTYPE='VARI'
        MAXVAL=3*MAXOBW
        NUMETT=NUME
        ISKIPT=ISKIP
        IF(ICLISK.EQ.'OFF')ISKIPT=0
        CALL DPCLIP(XSCRT,MAXVAL,NPTS,NUMETT,NUMVLN,PREAMV,ISKIPT,
     1              IGRPAU,
     1              IVLIST,IVLIS2,IAVANM,MAXRDV,
     1              IRTYPE,ISTRZZ,NCSTR,IEOF,
     1              IBUGS2,ISUBRO,IERROR)
C
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
          WRITE(ICOUT,11205)NPTS,NUMETT,IERROR
11205     FORMAT('AFTER CALL DPCLIP: NPTS,NUMETT,IERROR = ',2I10,2X,A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(NPTS.LE.0 .OR. IERROR.EQ.'YES')GOTO9000
C
        IF(NUME.EQ.0 .AND. NUMETT.GT.0)THEN
          NUME=NUMETT
          NUMVRD=NUMETT
          ICOL=NUMCOL
          INAM=NUMNAM
          IV=0
          IU=0
          IE=0
C
          NCBASE=0
          DO58590II=8,1,-1
            IF(IAVABN(II:II).NE.' ')THEN
              NCBASE=II
              GOTO58599
            ENDIF
58590     CONTINUE
58599     CONTINUE
C
          DO5893J=1,NUMETT
            IF(IAVANM.EQ.'FILE')THEN
              IVTEMP(1:4)=IVLIST(J)(1:4)
              IVTEMP(5:8)=IVLIS2(J)(1:4)
              DO5895JJ=1,8
                CALL DPCOAN(IVTEMP(JJ:JJ),IVALT)
                IF(IVALT.GE.97 .AND. IVALT.LE.122)THEN
                  IVALT=IVALT-32
                  CALL DPCONA(IVALT,IVTEMP(JJ:JJ))
                ENDIF
 5895         CONTINUE
            ELSE
              IF(NCBASE.LE.0)THEN
                 IVTEMP(1:4)='COL     '
                 NCBASE=3
              ELSE
                 IVTEMP(1:4)=IAVABN(1:4)
                 IVTEMP(5:8)=IAVABN(5:8)
              ENDIF
              NCSTAR=NCBASE+1
              NCSTOP=NCBASE+J
              IF(NCSTOP.GT.8)THEN
                NDIFF=NCSTOP-8
                NCSTAR=NCSTAR-NDIFF
              ENDIF
              IF(J.LE.9)THEN
                WRITE(IVTEMP(NCSTAR:NCSTAR),'(I1)')J
              ELSEIF(J.LE.99)THEN
                WRITE(IVTEMP(NCSTAR:NCSTAR+1),'(I2)')J
              ELSEIF(J.LE.999)THEN
                WRITE(IVTEMP(NCSTAR:NCSTAR+2),'(I3)')J
              ELSEIF(J.LE.9999)THEN
                WRITE(IVTEMP(NCSTAR:NCSTAR+3),'(I4)')J
              ELSE
                WRITE(IVTEMP(NCSTAR:NCSTAR+4),'(I5)')J
              ENDIF
            ENDIF
C
C           CHECK AGAINST VARIABLE LIST.  NOTE THAT READ CLIPBOARD IS
C           CURRENTLY RESRICTED TO READING VARIABLES (I.E., NO STRINGS,
C           PARAMETERS, OR MATRICES).
C
            ICASEA='    '
            DO5810I=1,NUMNAM
              I2=I
              IF(IVTEMP(1:4).EQ.IHNAME(I)(1:4).AND.
     1          IVTEMP(5:8).EQ.IHNAM2(I)(1:4))THEN
                IF(IUSE(I).EQ.'V')THEN
                  ICASEA='V'
                  IV=IV+1
                  IF(IV.GT.MAXV2)THEN
                    WRITE(ICOUT,999)
                    CALL DPWRST('XXX','BUG ')
                    WRITE(ICOUT,211)
                    CALL DPWRST('XXX','BUG ')
                    WRITE(ICOUT,5512)
 5512               FORMAT('      THE NUMBER OF VARIABLES DETECTED ',
     1                     'FROM THE READ CLIPBOARD COMMAND HAS')
                    CALL DPWRST('XXX','BUG ')
                    WRITE(ICOUT,5514)MAXV2
 5514               FORMAT('      EXCEEDED THE MAXIMUM OF ',I10)
                    CALL DPWRST('XXX','BUG ')
                    IERROR='YES'
                    GOTO8800
                  ENDIF
                  JVNAM1(IV)=IVTEMP(1:4)
                  JVNAM2(IV)=IVTEMP(5:8)
                  NIV(IV)=IN(I2)
                  GOTO5870
                ELSE
                  WRITE(ICOUT,999)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,211)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,4312)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,4315)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,4316)IVTEMP(1:4),IVTEMP(5:8)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,4317)
                  CALL DPWRST('XXX','BUG ')
                  IERROR='YES'
                  GOTO8800
                ENDIF
              ENDIF
 5810       CONTINUE
C
            ICASEA='U'
            IU=IU+1
            IF(IU.GT.MAXU2)GOTO5870
            JUNAM1(IU)=IVTEMP(1:4)
            JUNAM2(IU)=IVTEMP(5:8)
            GOTO5870
C
 5870       CONTINUE
            IE=IE+1
            IF(IE.GT.MAXE2)THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,211)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,4382)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,4384)MAXE2
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GOTO8800
            ENDIF
C
            JENAM1(IE)=IVTEMP(1:4)
            JENAM2(IE)=IVTEMP(5:8)
            IF(ICASEA.EQ.'V')THEN
              IECASE(IE)='OLD'
              IECOL2(IE)=IVALUE(I2)
            ELSE
              IECASE(IE)='NEW'
C
              INAM=INAM+1
              IF(INAM.GT.MAXNAM)THEN
                WRITE(ICOUT,999)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,211)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,712)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,714)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,715)MAXNAM
                CALL DPWRST('XXX','BUG ')
                IERROR='YES'
                GOTO8800
              ENDIF
C
              ICOL=ICOL+1
              IF(ICOL.GT.MAXCOL)THEN
                WRITE(ICOUT,999)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,211)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,722)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,724)MAXCOL
                CALL DPWRST('XXX','BUG ')
                IERROR='YES'
                GOTO8800
              ENDIF
C
              IF(IECASE(IE).EQ.'NEW')THEN
                IHNAME(INAM)=JENAM1(IE)
                IHNAM2(INAM)=JENAM2(IE)
                IUSE(INAM)='V'
                IVALUE(INAM)=ICOL
                IN(INAM)=0
                IECOL2(IE)=ICOL
              ENDIF
C
            ENDIF
C
 5893     CONTINUE
          NUMV=IV
          NUMU=IU
          NUME=IE
          NUMCOL=ICOL
          NUMNAM=INAM
C
        ENDIF
C
CCCCC OCTOBER 2014.  CHECK FOR READ STRING FROM CLIPBOARD CASE.
C
      ELSE IF(ICASRE.EQ.'CFUN')THEN
C
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
          WRITE(ICOUT,11203)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        ISTRZZ=' '
        IRTYPE='STRI'
        MAXVAL=3*MAXOBW
        NUMETT=0
        IEOF=0
C
C       LOOP THROUGH STRINGS
C
        DO11301II=1,NUME
          ISKIPT=ISKIP
          IF(ICLISK.EQ.'OFF')ISKIPT=0
          ISKIPT=ISKIPT+II-1
          CALL DPCLIP(XSCRT,MAXVAL,NPTS,NUMETT,NUMVLN,PREAMV,ISKIPT,
     1                IGRPAU,
     1                IVLIST,IVLIS2,IAVANM,MAXRDV,
     1                IRTYPE,ISTRZZ,NCSTR,IEOF,
     1                IBUGS2,ISUBRO,IERROR)
C
          IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
            WRITE(ICOUT,11305)NCSTR,ISTRZZ
11305       FORMAT('NCSTR,ISTRZZ = ',I5,255A1)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          IF(NCSTR.LE.0)THEN
            ISTRZZ='NULL'
            NCSTR=4
          ENDIF
C
          DO77801KK=1,NCSTR
            IFUNC2(KK)=' '
            IFUNC2(KK)(1:1)=ISTRZZ(KK:KK)
77801     CONTINUE
C
          CALL DPUPPE(IFUNC2,NCSTR,IFUNC3,IBUGS2,IERROR)
          ISTART=IFCOL1
          ISTOP=N2
          IH1=JENAM1(II)
          IH2=JENAM2(II)
          DO77820J=1,NUMNAM
            IF(IUSE(J).EQ.'F'.AND.
     1        IHNAME(J).EQ.IH1.AND.IHNAM2(J).EQ.IH2)THEN
              NEWNAM='NO'
              IF(IECASE(II).EQ.'NEW')NEWNAM='YES'
              ILISTL=J
C
              CALL DPINFU(IFUNC3,NCSTR,IHNAME,IHNAM2,IUSE,IN,
     1                    IVSTAR,IVSTOP,
     1                    NUMNAM,IANSLC,IWIDTH,IH1,IH2,ILISTL,
     1                    NEWNAM,MAXNME,
     1                    IFUNC,NUMCHF,MAXCHF,IBUGS2,IERROR)
C
              IF(NEWNAM.EQ.'YES'.AND.IERROR.EQ.'NO')NUMNAM=NUMNAM-1
C
            ENDIF
77820     CONTINUE
11301   CONTINUE
C
        GOTO7900
C
      ENDIF
C
CCCCC OCTOBER 2004: SUBSET/FOR/EXPECT CLAUSES ON READ HAVE SOME
CCCCC AMBIGUITY.  THAT IS, DOES THE SUBSET REFER TO THE LINES THAT
CCCCC ARE READ FROM THE FILE OR DOES THE SUBSET REFER TO HOW THE
CCCCC DATA ARE SAVED IN THE OUTPUT VECTORS.  WE ADDRESS THIS WITH
CCCCC THE COMMAND
CCCCC
CCCCC     SET READ SUBSET  <PACK/DISPERSE>   <PACK/DISPERSE>
CCCCC
CCCCC THE FIRST SETTING SPECIFIES HOW THE DATA FILE IS HANDLED
CCCCC (PACK MEANS SUBSET/FOR CLAUSE DOES NOT APPLY TO LINES IN
CCCCC FILE WHILE DISPERSE MEANS THAT IT DOES).  LIKEWISE, THE SECOND
CCCCC SETTING SPECIFIES HOW THE SUBSET/FOR CLAUSE APPLIES TO THE
CCCCC OUTPUT VARIABLES (PACK MEANS SUBSET IGNORED ON OUTPUT VECTOR,
CCCCC DISPERSE MEAMS THAT IT DOES).  THESE SETTINGS ARE CODED AS
CCCCC   "P-D", "P-P", "D-P", "D-D".  THE DEFAULT IS "P-D" (I.E.,
CCCCC THE SUBSET APPLIES TO THE OUTPUT VECTORS, BUT NOT THE INPUT
CCCCC FILE).  FOR EXAMPLE, THE COMMAND
CCCCC
CCCCC            READ X  FOR I = 1  2  10
CCCCC
CCCCC     X      P-D       P-P          D-P      D-D
CCCCC    ===========================================
CCCCC     1       1         1            1        1
CCCCC     2       0         2            3        0
CCCCC     3       2         3            5        3
CCCCC     4       0         4            7        0
CCCCC     5       3         5            9        5
CCCCC     6       0         -            -        0
CCCCC     7       4         -            -        7
CCCCC     8       0         -            -        0
CCCCC     9       5         -            -        9
CCCCC    10       0         -            -        0
C
C
      IF(ICASRE.EQ.'IMAZ' .OR. ICASRE.EQ.'IMAG')THEN
        IFRMIN=1
        IFRMAX=IYSIZE
      ENDIF
C
      IF(ICASRE.EQ.'CLIP')THEN
        IFRMIN=1
CCCCC   IFRMAX=NPTS/NUME
CCCCC   IREM=MOD(NPTS,NUME)
        IFRMAX=NPTS/NUMVLN
        IREM=MOD(NPTS,NUMVLN)
        IF(IREM.GT.0)IFRMAX=IFRMAX+1
      ENDIF
C
      NCALL=0
      I=0
      IMAXRW=IFRMAX-IFRMIN+1
      DO7400IFROW=IFRMIN,IFRMAX
C
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
          WRITE(ICOUT,7401)IFROW,IMNVAR,IMXVAR
 7401     FORMAT('AT 7400: IFROW,IMNVAR,IMXVAR = ',3I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(ICASRE.EQ.'IMAZ' .OR. ICASRE.EQ.'IMAG')THEN
          NUMLRD=NUMLRD+1
          IROWXX=IFROW
          DO74001JJ=1,IXSIZE
            ICOLXX=JJ
            IXTEMP=ICOLXX
CCCCC       IYTEMP=IROWXX
            IYTEMP=IYSIZE - IROWXX
#ifdef HAVE_GD
            CALL GDPIXE(IXTEMP,IYTEMP,IRED,IGREEN,IBLUE)
#endif
            X0(ICOLXX)=REAL(IRED)
            X0(IXSIZE + ICOLXX)=REAL(IGREEN)
            X0(2*IXSIZE + ICOLXX)=REAL(IBLUE)
74001     CONTINUE
          NUMDPL=3*IXSIZE
          GOTO7440
        ENDIF
C
        IIN=IIN+1
        IF(ISUB(IIN).NE.1 .AND. ICASRE.NE.'CLIP')THEN
          IF(IREASB(1:1).EQ.'D')THEN
            IF(IREASB(3:3).EQ.'D')THEN
              I=I+1
            ENDIF
            NUMCHA=-1
            CALL DPREFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1                  IA,NUMCHA,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
            IF(IA(1).EQ.'E'.AND.IA(2).EQ.'O'.AND.IA(3).EQ.'F'.AND.
     1         NUMCHA.EQ.3)THEN
               REWIND IOUNIT
               IENDTY=1
               GOTO7490
            ENDIF
            GOTO7400
          ENDIF
        ENDIF
C
        IF(ICASRE.NE.'CLIP' .AND. NCREAF.LE.0 .OR. ICASRE.EQ.'FUNC' .OR.
     1     ICASRE.EQ.'ROWI')THEN
          NXCSAV=NXC
          CALL DPREAL(IRD2,IFCOL3,IFCOL4,MINCO2,MAXCO2,X0,NUMDPL,IFLGSV,
     1                IXC,NXC,
     1                ICASRE,IFUNC2,N2,MAXN2,
     1                IMACRO,IMACNU,IMACCS,
     1                IANSLC,IWIDTH,IREACS,ISTOR1,ISTOR2,IEND,NUMLRD,
     1                IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1                ICOMCH,ICOMSW,LINETY,IGRPA2,
     1                IFCOLL,IFCOLU,ITYPE,NCOLS,NCALL,
     1                IREADL,IDATDL,ITIMDL,IRDIPA,PREAMV,
     1                MAXRDV,MAXCHV,IFIETY,
     1                IDECPT,IDATMV,IDATNN,
     1                IREACD,IREACM,IREADS,IREAPM,IREAMC,ITABNC,
     1                IREAAS,IREAPC,
     1                IB,
     1                IOTERM,IANSLO,MAXLIL,MAXCIL,ILOOST,ILOOLI,
     1                IREPCH,IMALEV,
     1                IERRFI,IBUGS2,ISUBRO,IERROR)
C
          IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
            WRITE(ICOUT,7402)LINETY,IEND,IERROR,NUMDPL
 7402       FORMAT('AFTER DPREAL: LINETY,IEND,IERROR,NUMDPL = ',
     1             3(A4,2X),I8)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          IF(LINETY.EQ.'BLAN')GOTO7400
          IF(IERROR.EQ.'YES')GOTO9000
          IF(ICASRE.NE.'ROWR' .AND. IEND.EQ.'NO')THEN
            IF(IMNVAR.EQ.-1)THEN
              IMNVAR=NUMDPL
            ELSE
              IF(NUMDPL.LT.IMNVAR)IMNVAR=NUMDPL
            ENDIF
            IF(NUMDPL.GT.IMXVAR)IMXVAR=NUMDPL
          ENDIF
C
          IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
            WRITE(ICOUT,7403)IMNVAR,IMXVAR
 7403       FORMAT('IMNVAR,IMXVAR = ',2I8)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          NUMLRD=NUMLRD+1
          NCALL=NCALL+1
C
C  HANDLE "ROW READ" SEPARATELY
C
          IF(ICASRE.EQ.'ROWR')THEN
C
            IF(IEND.EQ.'YES')THEN
              IVBASE=IVBASV
              NUMDPL=NUMDSV
              NUMLRD=NUMLRD-1
              GOTO8800
            ENDIF
C
            IF(IOFILE.EQ.'YES')THEN
              IVBASE(1:4)=IHARG(3)(1:4)
              IVBASE(5:8)=IHARG2(3)(1:4)
            ELSE
              IVBASE(1:4)=IHARG(2)(1:4)
              IVBASE(5:8)=IHARG2(2)(1:4)
            ENDIF
            IVLAST=8
            DO22111LL=8,1,-1
              IF(IVBASE(LL:LL).NE.' ')THEN
                IVLAST=LL
                GOTO22119
              ENDIF
22111       CONTINUE
            IVLAST=1
            IVBASE='X'
22119       CONTINUE
            IF(NUMDPL.GT.0)THEN
              IF(NUMLRD.LE.9)THEN
                IF(IVLAST.GT.7)IVLAST=7
                WRITE(IVBASE(IVLAST+1:IVLAST+1),'(I1)')NUMLRD
              ELSEIF(NUMLRD.LE.99)THEN
                IF(IVLAST.GT.6)IVLAST=6
                WRITE(IVBASE(IVLAST+1:IVLAST+2),'(I2)')NUMLRD
              ELSEIF(NUMLRD.LE.999)THEN
                IF(IVLAST.GT.5)IVLAST=5
                WRITE(IVBASE(IVLAST+1:IVLAST+3),'(I3)')NUMLRD
              ELSEIF(NUMLRD.LE.9999)THEN
                IF(IVLAST.GT.4)IVLAST=4
                WRITE(IVBASE(IVLAST+1:IVLAST+4),'(I4)')NUMLRD
              ELSEIF(NUMLRD.LE.99999)THEN
                IF(IVLAST.GT.3)IVLAST=3
                WRITE(IVBASE(IVLAST+1:IVLAST+5),'(I5)')NUMLRD
              ENDIF
            ENDIF
C
            IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
              WRITE(ICOUT,22101)NUMLRD,NUMDPL,IVLAST,IVBASE
22101         FORMAT('NUMLRD,NUMDPL,IVLAST,IVBASE = ',3I6,2X,A8)
              CALL DPWRST('XXX','BUG ')
            ENDIF
C
            DO55810I=1,NUMNAM
              I2=I
C
C             PRE-EXISTING VARIABLE NAME FOUND
C
              IF(IVBASE(1:4).EQ.IHNAME(I)(1:4).AND.
     1          IVBASE(5:8).EQ.IHNAM2(I)(1:4))THEN
                IF(IUSE(I).EQ.'V')THEN
                  ICASEA='V'
                  ICOL=IVALUE(I2)
                  GOTO55870
                ELSE
                  WRITE(ICOUT,999)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,211)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,4312)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,4315)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,4316)IVBASE(1:4),IVBASE(5:8)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,44317)
44317             FORMAT('     THIS ROW WILL BE SKIPPED.')
                  CALL DPWRST('XXX','BUG ')
                  IERROR='YES'
                  GOTO7400
                ENDIF
              ENDIF
55810       CONTINUE
C
            NUMNAM=NUMNAM+1
            I2=NUMNAM
            IF(NUMNAM.GT.MAXNME)THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,211)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,712)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,714)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,715)MAXNME
              CALL DPWRST('XXX','BUG ')
              NUMNAM=NUMNAM-1
              IVBASE=IVBASV
              NUMDPL=NUMDSV
              NUMLRD=NUMLRD-1
              IERROR='YES'
              GOTO8800
            ENDIF
C
            NUMCOL=NUMCOL+1
            ICOL=NUMCOL
            IF(ICOL.GT.MAXCOL)THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,211)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,722)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,724)MAXCOL
              CALL DPWRST('XXX','BUG ')
              NUMCOL=NUMCOL-1
              IERROR='YES'
              IVBASE=IVBASV
              NUMDPL=NUMDSV
              NUMLRD=NUMLRD-1
              GOTO8800
            ENDIF
C
            IHNAME(NUMNAM)=IVBASE(1:4)
            IHNAM2(NUMNAM)=IVBASE(5:8)
C
55870       CONTINUE
C
            IF(NUMDPL.GT.MAXN)THEN
              NUMDPL=MAXN
              WRITE(ICOUT,44318)NUMLRD,MAXN
44318         FORMAT('ROW READ: FOR LINE ',I6,' THE NUMBER ',
     1               'OF VALUES TRUNCATED AT ',I10)
              CALL DPWRST('XXX','BUG ')
            ENDIF
C
            DO55880II=1,NUMDPL
              IJ=MAXN*(ICOL-1)+II
              IF(ICOL.LE.MAXCOL)V(IJ)=X0(II)
              IF(ICOL.EQ.MAXCP1)PRED(I)=X0(II)
              IF(ICOL.EQ.MAXCP2)RES(I)=X0(II)
              IF(ICOL.EQ.MAXCP3)YPLOT(I)=X0(II)
              IF(ICOL.EQ.MAXCP4)XPLOT(I)=X0(II)
              IF(ICOL.EQ.MAXCP5)X2PLOT(I)=X0(II)
              IF(ICOL.EQ.MAXCP6)TAGPLO(I)=X0(II)
55880       CONTINUE
            IUSE(I2)='V'
            IVALUE(I2)=ICOL
            IN(I2)=NUMDPL
C
            IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
              WRITE(ICOUT,55802)NUMLRD,NUMDPL,IVLAST,IVBASE
55802         FORMAT('I2,ICOL,IUSE(I2),IVALUE(I2),IN(I2) = ',
     1               2I10,I6,2X,A8)
              CALL DPWRST('XXX','BUG ')
            ENDIF
C
            IF(IFEEDB.EQ.'ON' .AND. NUMLRD.EQ.1)THEN
CCCCC       IF(IFEEDB.EQ.'ON')THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,55805)NUMLRD,IVBASE,NUMDPL
55805         FORMAT('ROW READ: ROW ',I10,' READ AS ',A8,' WITH ',I10,
     1               ' OBSERVATIONS READ')
              CALL DPWRST('XXX','BUG ')
            ENDIF
C
            IVBASV=IVBASE
            NUMDSV=NUMDPL
            GOTO7400
          ENDIF
C
C  IF CHARACTER DATA ENCOUNTERED, WRITE IT TO FILE
C
C  2019/09: WRITE RESULTS TO "dpst2f.dat" INITIALLY.
C
          IF(NXC.GT.0 .AND.
     1      (IGRPAU.EQ.'CHAR' .OR. IGRPAU.EQ.'CATE'))THEN
            IF(NUMLRD.EQ.1)THEN
C
CCCCC         IOUNI2=IZCHNU
CCCCC         IFILE2=IZCHNA
CCCCC         ISTAT2=IZCHST
CCCCC         IFORM2=IZCHFO
CCCCC         IACCE2=IZCHAC
CCCCC         IPROT2=IZCHPR
CCCCC         ICURS2=IZCHCS
C
CCCCC         ISUBN0='READ'
CCCCC         IERRFI='NO'
CCCCC         CALL DPOPFI(IOUNI2,IFILE2,ISTAT2,IFORM2,IACCE2,IPROT2,
CCCCC1                    ICURS2,
CCCCC1                    IREWI2,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
CCCCC         IF(IERROR.EQ.'YES')GOTO9000
C
              IOP='OPEN'
              IFLG11=0
              IFLG21=1
              IFLG31=0
              IFLAG4=0
              IFLAG5=0
              CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,
     1                    IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1                    IBUGS2,ISUBRO,IERROR)
              IF(IERRFI.EQ.'YES')GOTO9000
C
              IZCHCS=ICURS2
C
              WRITE(IOUNI2,'(I8)')NXC
              DO27810ICNT=1,MIN(NXC,MAXCHV)
                WRITE(IOUNI2,'(A4,A4)')ICLIST(ICNT),ICLIS2(ICNT)
27810         CONTINUE
            ENDIF
            WRITE(IOUNI2,'(20(A24,1X))')(IXC(J)(1:24),J=1,NXC)
C
C           IF "SET CONVERT CHARACTER CATEGORICAL" GIVEN, THEN
C           CREATE A NUMERIC VARIABLE AS WELL.
C
            IF(IGRPAU.EQ.'CATE')THEN
              DO27820J=1,NXC
                NTEMP=IXCATN(J)
                IF(NTEMP.LT.1)THEN
                  IXCATN(J)=1
                  IXCAT(1,J)(1:24)=IXC(J)(1:24)
                  X0CAT(J)=1.0
                ELSE
                  DO27830II=1,NTEMP
                    IF(IXC(J)(1:24).EQ.IXCAT(II,J)(1:24))THEN
                      X0CAT(J)=REAL(II)
                      GOTO27820
                    ENDIF
27830             CONTINUE
                  NTEMP2=IXCATN(J)
                  IF(NTEMP2.GE.1000)THEN
                    X0CAT(J)=-1.0
                  ELSE
                    IXCATN(J)=IXCATN(J)+1
                    IXCAT(IXCATN(J),J)(1:24)=IXC(J)(1:24)
                    X0CAT(J)=REAL(IXCATN(J))
                  ENDIF
                ENDIF
27820         CONTINUE
            ENDIF
C
          ENDIF
        ELSEIF(IFMFLG.EQ.'ON')THEN
C
          NUMLRD=NUMLRD+1
          IF(IUNFNR.GT.0.AND.NUMLRD*NUME.GT.IUNFNR)GOTO7400
          NUMDPL=NUME
          IF(ICRFLG.EQ.'ROW')THEN
            IPTR1=(NUMLRD-1)*NUME+1+IUNFOF
            IPTR2=IPTR1+NUME-1
            ICOUNT=0
            DO17415JJ=IPTR1,IPTR2
              ICOUNT=ICOUNT+1
              X0(ICOUNT)=XSCRT(JJ)
17415       CONTINUE
          ELSE
            IPTR1=NUMLRD+IUNFOF
            IPTR2=IFRMAX
            DO17515JJ=1,NUME
              ICOUNT=IPTR1+(JJ-1)*IPTR2
              X0(JJ)=XSCRT(ICOUNT)
17515       CONTINUE
          ENDIF
        ELSEIF(ICASRE.EQ.'CLIP')THEN
C
          NUMLRD=NUMLRD+1
          NUMDPL=NUME
C
C         2020/02: SET POINTER BASED ON MANY VALUES READ FROM
C                  CLIPBOARD RATHER THAN THE NUMBER OF VARIABLES
C                  USER REQUESTED.
C
CCCCC     IPTR1=(NUMLRD-1)*NUME+1
          IPTR1=(NUMLRD-1)*NUMVLN+1
          IPTR2=IPTR1+NUME-1
          ICOUNT=0
          DO27415JJ=IPTR1,IPTR2
            ICOUNT=ICOUNT+1
            IF(ICOUNT.LE.NUMVLN)THEN
              X0(ICOUNT)=XSCRT(JJ)
            ELSE
              X0(ICOUNT)=PREAMV
            ENDIF
27415     CONTINUE
        ELSE
          NUMLRD=NUMLRD+1
          NUMDPL=NUME
          IF(ICOMSW.EQ.'ON')THEN
 7417       CONTINUE
            READ(IRD2,'(A80)',END=7480)IAJUNK
            IF(IAJUNK(1:1).EQ.ICOMCH(1:1))GOTO7417
            BACKSPACE(UNIT=IRD2,IOSTAT=IOS,ERR=7413)
            GOTO7415
 7413       CONTINUE
            WRITE(ICOUT,743)
 743        FORMAT('ERROR TRYING TO BACKSPACE FILE ON FORMATTED READ')
            CALL DPWRST('XXX','BUG ')
            GOTO7417
          ENDIF
          READ(IRD2,ICREAF,END=7480,ERR=7480)(X0(K),K=1,NUME)
          GOTO7415
        ENDIF
C
 7415   CONTINUE
        IF(IERROR.EQ.'YES')GOTO8800
        IF(IFROW.EQ.IFRMIN .AND.ICASRE.NE.'CLIP')THEN
          DO7425K=1,132
            ISTOR3(K)=ISTOR2(K)
 7425     CONTINUE
          GOTO7430
        ENDIF
        IF(IEND.EQ.'YES')GOTO7480
C
 7430   CONTINUE
        I=I+1
C
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7431)
 7431     FORMAT('***** FROM THE MIDDLE OF DPREAD--')
          CALL DPWRST('XXX','BUG ')
          AFRMAX=IFRMAX
          WRITE(ICOUT,7432)IFROW,IFRMIN,AFRMAX,IBUGS2,ISUBRO
 7432     FORMAT('IFROW,IFRMIN,AFRMAX,IBUGS2,ISUBRO = ',2I8,E15.7,
     1           2X,A4,2X,A4)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7433)I,ISUB(I),NUME,IBUGS2,ISUBRO
 7433     FORMAT('I,ISUB(I),NUME,IBUGS2,ISUBRO = ',3I8,2X,A4,2X,A4)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7434)MAXN,MAXCOL,MAXCP1,MAXCP2
 7434     FORMAT('MAXN,MAXCOL,MAXCP1,MAXCP2 = ',4I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7435)X0(1),X0(2),X0(3)
 7435     FORMAT('X0(1),X0(2),X0(3) = ',3E15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7436)IECOL2(1),IECOL2(2),IECOL2(3)
 7436     FORMAT('IECOL2(1),IECOL2(2),IECOL2(3) = ',3I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7437)IEN(1),IEN(2),IEN(3)
 7437     FORMAT('IEN(1),IEN(2),IEN(3) = ',3I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7438)ICASRE,NUMVRD,NUMPRD,NUMFRD
 7438     FORMAT('ICASRE,NUMVRD,NUMPRD,NUMFRD = ',A4,3I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
CCCCC   OCTOBER 2004: IS OUTPUT VECTOR PACKED OR DISPERSED?
C
        IF(I.GT.MAXN .OR. I.GT.IMAXRW)GOTO7480
        IJUNK=I
        IF(IREASB(3:3).EQ.'P' .AND. IREASB(1:1).EQ.'D')IJUNK=IIN
        IF(ISUB(IJUNK).EQ.1)THEN
          GOTO7440
        ELSE
          IF(IREASB(3:3).EQ.'D')THEN
            GOTO7430
          ELSE
            GOTO7400
          ENDIF
        ENDIF
C
 7440   CONTINUE
        IF(ICASRE.EQ.'PARA')THEN
          NUMPRD=NUME
          GOTO7400
        ELSEIF(ICASRE.EQ.'FUNC')THEN
           NUMFRD=NUME
           GOTO7400
        ELSEIF(ICASRE.EQ.'MATZ')THEN
C
C         IMPLEMENT THE "MATRIX TO VARIABLES" CASE.  THE
C         FIRST VARIABLE WILL CONTAIN THE MATRIX VALUES,
C         THE SECOND VARIABLE WILL CONTAIN THE ROW-ID, AND
C         THE THIRD VARIABLE WILL CONTAIN THE COLUMN-ID.
C
          NROWZ=NROWZ+1
          NCOLZ=0
          IE2=0
          IF(NUMDPL.LE.0)GOTO17448
          DO17445IE=1,NUMDPL
            IE2=IE
            Z0=X0(IE)
C
C           COLUMN 1: DATA VALUES
C
            NCOLZ=NCOLZ+1
            ITOTZ=ITOTZ+1
C
            IF(ITOTZ.GT.MAXN)THEN
              WRITE(ICOUT,17481)
17481         FORMAT('****** ERROR IN DPREAD--READ MATRIX TO ',
     1               'VARIABLES')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,17482)NROWZ
17482         FORMAT('       IN ROW ',I10,' OF THE DATA MATRIX,')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,17483)MAXN
17483         FORMAT('       THE MAXIMUM ROW SIZE ',I10,
     1               ' EXCEEDED.')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,17484)
17484         FORMAT('       NO ADDITIONAL DATA WILL BE READ.')
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GOTO7490
            ENDIF
C
            ICOLVJ=IECOL2(1)
            IJ=MAXN*(ICOLVJ-1)+ITOTZ
            IF(ICOLVJ.LE.MAXCOL)V(IJ)=Z0
            IF(ICOLVJ.EQ.MAXCP1)PRED(I)=Z0
            IF(ICOLVJ.EQ.MAXCP2)RES(I)=Z0
            IF(ICOLVJ.EQ.MAXCP3)YPLOT(I)=Z0
            IF(ICOLVJ.EQ.MAXCP4)XPLOT(I)=Z0
            IF(ICOLVJ.EQ.MAXCP5)X2PLOT(I)=Z0
            IF(ICOLVJ.EQ.MAXCP6)TAGPLO(I)=Z0
            IEN(1)=ITOTZ
C
C           COLUMN 2: ROW-ID
C
            Z0=REAL(NROWZ)
            ICOLVJ=IECOL2(2)
            IJ=MAXN*(ICOLVJ-1)+ITOTZ
            IF(ICOLVJ.LE.MAXCOL)V(IJ)=Z0
            IF(ICOLVJ.EQ.MAXCP1)PRED(I)=Z0
            IF(ICOLVJ.EQ.MAXCP2)RES(I)=Z0
            IF(ICOLVJ.EQ.MAXCP3)YPLOT(I)=Z0
            IF(ICOLVJ.EQ.MAXCP4)XPLOT(I)=Z0
            IF(ICOLVJ.EQ.MAXCP5)X2PLOT(I)=Z0
            IF(ICOLVJ.EQ.MAXCP6)TAGPLO(I)=Z0
            IEN(2)=ITOTZ
C
C           COLUMN 3: COLUMN-ID
C
            Z0=REAL(NCOLZ)
            ICOLVJ=IECOL2(3)
            IJ=MAXN*(ICOLVJ-1)+ITOTZ
            IF(ICOLVJ.LE.MAXCOL)V(IJ)=Z0
            IF(ICOLVJ.EQ.MAXCP1)PRED(I)=Z0
            IF(ICOLVJ.EQ.MAXCP2)RES(I)=Z0
            IF(ICOLVJ.EQ.MAXCP3)YPLOT(I)=Z0
            IF(ICOLVJ.EQ.MAXCP4)XPLOT(I)=Z0
            IF(ICOLVJ.EQ.MAXCP5)X2PLOT(I)=Z0
            IF(ICOLVJ.EQ.MAXCP6)TAGPLO(I)=Z0
            IEN(3)=ITOTZ
C
17445     CONTINUE
          NUMVRD=3
          GOTO7400
17448     CONTINUE
          GOTO7490
C
        ELSEIF(ICASRE.EQ.'IMAZ')THEN
C
C         IMPLEMENT THE "IMAGE TO VARIABLES" CASE.  THE
C         FIRST VARIABLE WILL CONTAIN THE RED COMPONENT,
C         THE SECOND VARIABLE WILL CONTAIN THE GREEN COMPONENT,
C         AND THE THIRD VARIABLE WILL CONTAIN THE BLUE COMPONENT.
C         NOTE THAT ONE ROW OF THE IMAGE IS READ, SO THERE WILL
C         BE 3*IXSIZE DATA POINTS (NOTE THAT ALL THE RED COMPONENT
C         VALUES ARE STORED, THEN ALL THE GREEN, THEN ALL THE BLUE).
C
C         IF THREE VARIABLES WERE GIVEN, COLUMN 1 IS THE RED
C         COMPONENT (I.E., GREY SCALE), COLUMNS 2 IS THE COLUMN-ID,
C         AND COLUMN 3 IS THE ROW-ID.  IF FIVE VARIABLES WERE GIVEN,
C         COLUMN 1 IS THE RED COMPONENT, COLUMN 2 IS THE GREEN
C         COMPONENT, COLUMN 3 IS THE BLUE COMPONENT, COLUMN 4 IS THE
C         COLUMN-ID, AND COLUMN 5 IS THE ROW-ID.
C
          NROWZ=NROWZ+1
          NCOLZ=0
          IE2=0
          IF(NUMDPL.LE.0)GOTO17548
          NLAST=NUMDPL/3
          DO17545IE=1,NLAST
            IE2=IE
            ZR=X0(IE)
            ZG=X0(IXSIZE + IE)
            ZB=X0(2*IXSIZE + IE)
C
C           COLUMN 1: RED COMPONENT
C
            NCOLZ=NCOLZ+1
            ITOTZ=ITOTZ+1
C
            IF(ITOTZ.GT.MAXN)THEN
              WRITE(ICOUT,17581)
17581         FORMAT('****** ERROR IN DPREAD--READ IMAGE TO ',
     1               'VARIABLES')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,17582)NROWZ
17582         FORMAT('       IN ROW ',I10,' OF THE DATA IMAGE,')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,17583)MAXN
17583         FORMAT('       THE MAXIMUM ROW SIZE ',I10,
     1               ' EXCEEDED.')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,17584)
17584         FORMAT('       NO ADDITIONAL DATA WILL BE READ.')
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GOTO7490
            ENDIF
C
C           COLUMN 1: RED COMPONENT
C
            ICOLVJ=IECOL2(1)
            IJ=MAXN*(ICOLVJ-1)+ITOTZ
            IF(ICOLVJ.LE.MAXCOL)V(IJ)=ZR
            IF(ICOLVJ.EQ.MAXCP1)PRED(I)=ZR
            IF(ICOLVJ.EQ.MAXCP2)RES(I)=ZR
            IF(ICOLVJ.EQ.MAXCP3)YPLOT(I)=ZR
            IF(ICOLVJ.EQ.MAXCP4)XPLOT(I)=ZR
            IF(ICOLVJ.EQ.MAXCP5)X2PLOT(I)=ZR
            IF(ICOLVJ.EQ.MAXCP6)TAGPLO(I)=ZR
            IEN(1)=ITOTZ
C
            IF(NUME.EQ.5)THEN
C
C             COLUMN 2: GREEN COMPONENT
C
              ICOLVJ=IECOL2(2)
              IJ=MAXN*(ICOLVJ-1)+ITOTZ
              IF(ICOLVJ.LE.MAXCOL)V(IJ)=ZG
              IF(ICOLVJ.EQ.MAXCP1)PRED(I)=ZG
              IF(ICOLVJ.EQ.MAXCP2)RES(I)=ZG
              IF(ICOLVJ.EQ.MAXCP3)YPLOT(I)=ZG
              IF(ICOLVJ.EQ.MAXCP4)XPLOT(I)=ZG
              IF(ICOLVJ.EQ.MAXCP5)X2PLOT(I)=ZG
              IF(ICOLVJ.EQ.MAXCP6)TAGPLO(I)=ZG
              IEN(2)=ITOTZ
C
C             COLUMN 3: BLUE COMPONENT
C
              ICOLVJ=IECOL2(3)
              IJ=MAXN*(ICOLVJ-1)+ITOTZ
              IF(ICOLVJ.LE.MAXCOL)V(IJ)=ZB
              IF(ICOLVJ.EQ.MAXCP1)PRED(I)=ZB
              IF(ICOLVJ.EQ.MAXCP2)RES(I)=ZB
              IF(ICOLVJ.EQ.MAXCP3)YPLOT(I)=ZB
              IF(ICOLVJ.EQ.MAXCP4)XPLOT(I)=ZB
              IF(ICOLVJ.EQ.MAXCP5)X2PLOT(I)=ZB
              IF(ICOLVJ.EQ.MAXCP6)TAGPLO(I)=ZB
              IEN(3)=ITOTZ
C
              INEXT=4
            ELSE
              INEXT=2
            ENDIF
C
C           COLUMN 2 OR 4: ROW-ID
C
            Z0=REAL(NROWZ)
            ICOLVJ=IECOL2(INEXT)
            IJ=MAXN*(ICOLVJ-1)+ITOTZ
            IF(ICOLVJ.LE.MAXCOL)V(IJ)=Z0
            IF(ICOLVJ.EQ.MAXCP1)PRED(I)=Z0
            IF(ICOLVJ.EQ.MAXCP2)RES(I)=Z0
            IF(ICOLVJ.EQ.MAXCP3)YPLOT(I)=Z0
            IF(ICOLVJ.EQ.MAXCP4)XPLOT(I)=Z0
            IF(ICOLVJ.EQ.MAXCP5)X2PLOT(I)=Z0
            IF(ICOLVJ.EQ.MAXCP6)TAGPLO(I)=Z0
            IEN(INEXT)=ITOTZ
            INEXT=INEXT+1
C
C           COLUMN 3 OR 5: COLUMN-ID
C
            Z0=REAL(NCOLZ)
            ICOLVJ=IECOL2(INEXT)
            IJ=MAXN*(ICOLVJ-1)+ITOTZ
            IF(ICOLVJ.LE.MAXCOL)V(IJ)=Z0
            IF(ICOLVJ.EQ.MAXCP1)PRED(I)=Z0
            IF(ICOLVJ.EQ.MAXCP2)RES(I)=Z0
            IF(ICOLVJ.EQ.MAXCP3)YPLOT(I)=Z0
            IF(ICOLVJ.EQ.MAXCP4)XPLOT(I)=Z0
            IF(ICOLVJ.EQ.MAXCP5)X2PLOT(I)=Z0
            IF(ICOLVJ.EQ.MAXCP6)TAGPLO(I)=Z0
            IEN(INEXT)=ITOTZ
C
17545     CONTINUE
          NUMVRD=INEXT
          GOTO7400
17548     CONTINUE
          GOTO7490
C
        ELSEIF(ICASRE.EQ.'STAC')THEN
C
C         IMPLEMENT THE "STACK VARIABLES" CASE.  THE
C         FIRST VARIABLE WILL CONTAIN THE RESPONSE VALUES
C         AND THE SECOND VARIABLE WILL CONTAIN A GROUP-ID
C         VARIABLE.
C
          NROWZ=NROWZ+1
          NCOLZ=0
          IE2=0
          IF(NUMDPL.LE.0)GOTO18448
          DO18445IE=1,NUMDPL
            IE2=IE
            Z0=X0(IE)
C
C           COLUMN 1: DATA VALUES
C
            NCOLZ=NCOLZ+1
            ITOTZ=ITOTZ+1
C
            IF(ITOTZ.GT.MAXN)THEN
              WRITE(ICOUT,18481)
18481         FORMAT('****** ERROR IN DPREAD--READ STACK ',
     1               'VARIABLES')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,18482)NROWZ
18482         FORMAT('       IN ROW ',I10,' OF THE DATA MATRIX,')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,18483)MAXN
18483         FORMAT('       THE MAXIMUM ROW SIZE ',I10,
     1               ' EXCEEDED.')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,18484)
18484         FORMAT('       NO ADDITIONAL DATA WILL BE READ.')
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GOTO7490
            ENDIF
C
            ICOLVJ=IECOL2(1)
            IJ=MAXN*(ICOLVJ-1)+ITOTZ
            IF(ICOLVJ.LE.MAXCOL)V(IJ)=Z0
            IF(ICOLVJ.EQ.MAXCP1)PRED(I)=Z0
            IF(ICOLVJ.EQ.MAXCP2)RES(I)=Z0
            IF(ICOLVJ.EQ.MAXCP3)YPLOT(I)=Z0
            IF(ICOLVJ.EQ.MAXCP4)XPLOT(I)=Z0
            IF(ICOLVJ.EQ.MAXCP5)X2PLOT(I)=Z0
            IF(ICOLVJ.EQ.MAXCP6)TAGPLO(I)=Z0
            IEN(1)=ITOTZ
C
C           COLUMN 2: GROUP-ID
C
            Z0=REAL(NCOLZ)
            ICOLVJ=IECOL2(2)
            IJ=MAXN*(ICOLVJ-1)+ITOTZ
            IF(ICOLVJ.LE.MAXCOL)V(IJ)=Z0
            IF(ICOLVJ.EQ.MAXCP1)PRED(I)=Z0
            IF(ICOLVJ.EQ.MAXCP2)RES(I)=Z0
            IF(ICOLVJ.EQ.MAXCP3)YPLOT(I)=Z0
            IF(ICOLVJ.EQ.MAXCP4)XPLOT(I)=Z0
            IF(ICOLVJ.EQ.MAXCP5)X2PLOT(I)=Z0
            IF(ICOLVJ.EQ.MAXCP6)TAGPLO(I)=Z0
            IEN(2)=ITOTZ
C
18445     CONTINUE
          NUMVRD=2
          GOTO7400
18448     CONTINUE
          GOTO7490
C
        ELSEIF(ICASRE.EQ.'MATR')THEN
C
C     -----BEGIN MATRIX COPY-----
C          IHMAT1 = FIRST  HALF OF MATRIX NAME
C          IHMAT2 = SECOND HALF OF MATRIX NAME
C          INAMMA = NAME INDEX FOR MATRIX
C          IMATC1 = FIRST COLUMN OF THE MATRIX
C          IMATNR = NUMBER OF ROWS    IN THE MATRIX
C          IMATNC = NUMBER OF COLUMNS IN THE NATRIX
C
          IF(NUMLRD.EQ.1)THEN
C
            INAM=NUMNAM
            ICOL=NUMCOL
C
            INAM=INAM+1
            ICOL=ICOL+1
C
            INAMMA=INAM
            IMATC1=ICOL
C
            IHNAME(INAMMA)=IHMAT1
            IHNAM2(INAMMA)=IHMAT2
            IUSE(INAMMA)='M'
            IVALUE(INAMMA)=ICOL
            IN(INAMMA)=0
            IVALU2(INAMMA)=ICOL+NUMDPL-1
            IMATNC=NUMDPL
            NUMNAM=INAM
            NUMCOL=ICOL
C
            ICOL=ICOL-1
            IF(NUMDPL.GT.0)THEN
              DO7452IE=1,NUMDPL
                INAM=INAM+1
                ICOL=ICOL+1
                IHNAME(INAM)=JENAM1(IE)
                IHNAM2(INAM)=JENAM2(IE)
                IUSE(INAM)='V'
                IVALUE(INAM)=ICOL
                IN(INAM)=0
                IECOL2(IE)=ICOL
                IF(IBUGS2.EQ.'ON')THEN
                  WRITE(ICOUT,7453)IE,IECOL2(IE),NUMDPL,INAM,NUMNAM
 7453             FORMAT('IE,IECOL2(IE),NUMDPL,INAM,NUMNAM = ',5I8)
                  CALL DPWRST('XXX','BUG ')
                ENDIF
 7452         CONTINUE
              NUMNAM=INAM
              NUMCOL=ICOL
            ENDIF
          ENDIF
C
          IE2=0
          IMATNR=0
          IF(NUMDPL.GT.0)THEN
            DO7455IE=1,NUMDPL
              IE2=IE
              Z0=X0(IE)
              IF(IBUGS2.EQ.'ON')THEN
                WRITE(ICOUT,7456)IE,IECOL2(IE),NUMDPL,INAM,NUMNAM
 7456           FORMAT('IE,IECOL2(IE),NUMDPL,INAM,NUMNAM = ',5I8)
                CALL DPWRST('XXX','BUG ')
              ENDIF
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
              IN(INAM)=I
              IN(INAMMA)=I
              IMATNR=I
 7455       CONTINUE
            NUMVRD=IE2
            GOTO7400
          ENDIF
          NUMVRD=IE2-1
          GOTO7400
C
C     -----END MATRIX COPY-----
C
        ELSEIF(ICASRE.EQ.'IMAG')THEN
C
C     IMAGE TO MATRIX (NOTE: CURRENTLY ONLY ONE COMPONENT
C     AT A TIME CAN BE READ, SO NEED TO DO SOMETHING LIKE
C
C          READ IMAGE RED   FILE.DAT RED
C          READ IMAGE GREEN FILE.DAT GREEN
C          READ IMAGE BLUE  FILE.DAT BLUE
C
C     IN ORDER TO READ ALL COMPONENTS INTO SEPARATE MATRICES.
C
C     -----BEGIN MATRIX COPY-----
C          IHMAT1 = FIRST  HALF OF MATRIX NAME
C          IHMAT2 = SECOND HALF OF MATRIX NAME
C          INAMMA = NAME INDEX FOR MATRIX
C          IMATC1 = FIRST COLUMN OF THE MATRIX
C          IMATNR = NUMBER OF ROWS    IN THE MATRIX
C          IMATNC = NUMBER OF COLUMNS IN THE NATRIX
C
          IF(NUMLRD.EQ.1)THEN
C
            NUMDPL=NUMDPL/3
C
            INAM=NUMNAM
            ICOL=NUMCOL
C
            INAM=INAM+1
            ICOL=ICOL+1
C
            INAMMA=INAM
            IMATC1=ICOL
C
            IHNAME(INAMMA)=IHMAT1
            IHNAM2(INAMMA)=IHMAT2
            IUSE(INAMMA)='M'
            IVALUE(INAMMA)=ICOL
            IN(INAMMA)=0
            IVALU2(INAMMA)=ICOL+NUMDPL-1
            IMATNC=NUMDPL
            NUMNAM=INAM
            NUMCOL=ICOL
C
            ICOL=ICOL-1
            IF(NUMDPL.GT.0)THEN
              DO7552IE=1,NUMDPL
                INAM=INAM+1
                ICOL=ICOL+1
                IHNAME(INAM)=JENAM1(IE)
                IHNAM2(INAM)=JENAM2(IE)
                IUSE(INAM)='V'
                IVALUE(INAM)=ICOL
                IN(INAM)=0
                IECOL2(IE)=ICOL
                IF(IBUGS2.EQ.'ON')THEN
                  WRITE(ICOUT,7553)IE,IECOL2(IE),NUMDPL,INAM,NUMNAM
 7553             FORMAT('IE,IECOL2(IE),NUMDPL,INAM,NUMNAM = ',5I8)
                  CALL DPWRST('XXX','BUG ')
                ENDIF
 7552         CONTINUE
              NUMNAM=INAM
              NUMCOL=ICOL
            ENDIF
          ENDIF
C
          IE2=0
          IMATNR=0
          IF(NUMDPL.GT.0)THEN
            DO7555IE=1,NUMDPL
              IE2=IE
              IF(IMAGCO.EQ.1)THEN
                Z0=X0(IE)
              ELSEIF(IMAGCO.EQ.2)THEN
                Z0=X0(IXSIZE + IE)
              ELSEIF(IMAGCO.EQ.3)THEN
                Z0=X0(2*IXSIZE + IE)
              ENDIF
              IF(IBUGS2.EQ.'ON')THEN
                WRITE(ICOUT,7556)IE,IECOL2(IE),NUMDPL,INAM,NUMNAM
 7556           FORMAT('IE,IECOL2(IE),NUMDPL,INAM,NUMNAM = ',5I8)
                CALL DPWRST('XXX','BUG ')
              ENDIF
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
              IN(INAM)=I
              IN(INAMMA)=I
              IMATNR=I
 7555       CONTINUE
            NUMVRD=IE2
            GOTO7400
          ENDIF
          NUMVRD=IE2-1
          GOTO7400
C
        ELSEIF(ICASRE.EQ.'ROWI')THEN
          IF(I.GT.MAXN)GOTO7480
          IROWLB(I)=' '
          IF(ISUB(I).NE.1)GOTO7400
          ILEN=24
          IF(N2.LT.ILEN)ILEN=N2
          DO7442KK=1,ILEN
            IROWLB(I)(KK:KK)=IFUNC2(KK)(1:1)
7442      CONTINUE
          GOTO7400
        ENDIF
C
C  OCTOBER 2004.  IF NUMBER OF REQUESTED ITEMS IS GREATER THAN
C                 NUMBER OF ITEMS ON THE LINE, PAD WITH MISSING
C                 VALUE (PREAMV).
C
C                 THE SET READ PAD MISSING COLUMNS COMMANDS
C                 DETERMINES WHETHER WE PAD OR USE THE PREVIOUS
C                 BEHAVIOR (I.E., IN SOME CASES, A MISSING COLUMN
C                 MAY INDICATE AN ERROR).
C
        IE2=0
        IF(NUME.LE.0)THEN
          NUMVRD=IE2-1
          GOTO7450
        ENDIF
        DO7445IE=1,NUME
C
          IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
            WRITE(ICOUT,7404)NUMDPL,IE,IREAPD
 7404       FORMAT('AT 7445: NUMDPL,IE,IREAPD = ',2I8,2X,A4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          IE2=IE
          IF(IREAPD.EQ.'OFF')THEN
            IF(IE2.GT.NUMDPL)THEN
              NUMVRD=NUMDPL
              WRITE(ICOUT,7446)IFROW,NUME,NUMDPL
 7446         FORMAT('****** WARNING AT LINE ',I10,': ',I5,
     1               ' VALUES READ, BUT ',I5,' VALUES WERE EXPECTED.')
              CALL DPWRST('XXX','BUG ')
CCCCC         GOTO7450
              Z0=PREAMV
            ELSE
              Z0=X0(IE)
            ENDIF
          ELSE
            IF(IE2.GT.NUMDPL)THEN
              Z0=PREAMV
            ELSE
              Z0=X0(IE)
            ENDIF
          ENDIF
          ICOLVJ=IECOL2(IE)
          IJ=MAXN*(ICOLVJ-1)+I
C
          IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
            WRITE(ICOUT,7406)ICOLVJ,IJ,Z0
 7406       FORMAT('AT 7445: ICOLVJ,IJ,Z0 = ',2I8,G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          IF(ICOLVJ.LE.MAXCOL)V(IJ)=Z0
          IF(ICOLVJ.EQ.MAXCP1)PRED(I)=Z0
          IF(ICOLVJ.EQ.MAXCP2)RES(I)=Z0
          IF(ICOLVJ.EQ.MAXCP3)YPLOT(I)=Z0
          IF(ICOLVJ.EQ.MAXCP4)XPLOT(I)=Z0
          IF(ICOLVJ.EQ.MAXCP5)X2PLOT(I)=Z0
          IF(ICOLVJ.EQ.MAXCP6)TAGPLO(I)=Z0
          IEN(IE)=I
 7445   CONTINUE
        NUMVRD=IE2
C
C     2018/07: IF REQUESED, SAVE CHARACTER DATA AS CATEGORICAL NUMERIC
C              DATA
C
 7450   CONTINUE
        IF(IGRPAU.EQ.'CATE' .AND. NXC.GT.0)THEN
          DO7558IE=1,NXC
            IF(IE.NE.IRWLC3)THEN
              Z0=X0CAT(IE)
              ICOLVJ=IECOLC(IE)
              IJ=MAXN*(ICOLVJ-1)+I
              IF(ICOLVJ.LE.MAXCOL)V(IJ)=Z0
              IF(ICOLVJ.EQ.MAXCP1)PRED(I)=Z0
              IF(ICOLVJ.EQ.MAXCP2)RES(I)=Z0
              IF(ICOLVJ.EQ.MAXCP3)YPLOT(I)=Z0
              IF(ICOLVJ.EQ.MAXCP4)XPLOT(I)=Z0
              IF(ICOLVJ.EQ.MAXCP5)X2PLOT(I)=Z0
              IF(ICOLVJ.EQ.MAXCP6)TAGPLO(I)=Z0
              IENC(IE)=I
            ELSE
              IROWLB(I)=IXC(IE)(1:24)
            ENDIF
 7558     CONTINUE
        ELSEIF(IGRPAU.EQ.'CHAR' .AND. IRWLC3.GT.0)THEN
          IROWLB(I)=IXC(IRWLC3)(1:24)
        ENDIF
C
 7400 CONTINUE
 7470 CONTINUE
      IENDTY=2
      GOTO7490
C
 7480 CONTINUE
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
        WRITE(ICOUT,7481)NUME
 7481   FORMAT('AT 7480: ERROR OR END OF FILE FOR FORMATTED READ, ',
     1         'NUME = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        DO7485K=1,NUME
        WRITE(ICOUT,7487)K,X0(K)
 7487   FORMAT('K, X0(K) = ',I8,2X,G15.7)
        CALL DPWRST('XXX','BUG ')
 7485   CONTINUE
      ENDIF 
C
      IENDTY=1
      NUMLRD=NUMLRD-1
      GOTO7490
C
 7490 CONTINUE
C
C               *****************************
C               **  STEP 11--              **
C               **  UPDATE THE NAME TABLE  **
C               *****************************
C
      ISTEPN='11'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASRE.EQ.'PARA')THEN
        ISTEPN='7700'
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        IF(NUMPRD.GT.0)THEN
          DO7710IE=1,NUMPRD
            IH1=JENAM1(IE)
            IH2=JENAM2(IE)
            IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
              WRITE(ICOUT,7711)IE,JENAM1(IE),JENAM2(IE),X0(IE)
 7711         FORMAT('IE,JENAM1(IE),JENAM2(IE),X0(IE) = ',
     1               I8,2X,2A4,E15.7)
              CALL DPWRST('XXX','BUG ')
            ENDIF
            DO7720J=1,NUMNAM
              IF(IUSE(J).EQ.'P'.AND.
     1          IHNAME(J).EQ.IH1.AND.IHNAM2(J).EQ.IH2)THEN
                IECOL2(IE)=J
                IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
                  WRITE(ICOUT,7731)IE,J,IECOL2(IE),X0(IE)
 7731             FORMAT('IE,J,IECOL2(IE),X0(IE) = ',3I8,E15.7)
                  CALL DPWRST('XXX','BUG ')
                ENDIF
                VALUE(J)=X0(IE)
                IVALUE(J)=INT(VALUE(J))
CCCCC           FOLLOWING LINE ADDED SO THAT DELETE AND RETAIN WILL NOT 
CCCCC           DELETE PARAMETER CREATED VIA READ PARAMETER.  MARCH 1994.
                IN(J)=1
              ENDIF
 7720       CONTINUE
 7710     CONTINUE
        ENDIF
        GOTO7900
      ELSEIF(ICASRE.EQ.'FUNC')THEN
C
        ISTEPN='7800'
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IF(NUMFRD.GT.0)THEN
          CALL DPUPPE(IFUNC2,N2,IFUNC3,IBUGS2,IERROR)
          ISTART=IFCOL1
          ISTOP=N2
          DO7810IE=1,NUMFRD
            IH1=JENAM1(IE)
            IH2=JENAM2(IE)
            IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
               WRITE(ICOUT,7811)IE,JENAM1(IE),JENAM2(IE),IECASE(IE)
 7811          FORMAT('IE,JENAM1(IE),JENAM2(IE),IECASE(IE) = ',
     1                I8,2X,2A4,2X,A4)
               CALL DPWRST('XXX','BUG ')
            ENDIF
            DO7820J=1,NUMNAM
              IF(IUSE(J).EQ.'F'.AND.
     1          IHNAME(J).EQ.IH1.AND.IHNAM2(J).EQ.IH2)THEN
                IECOL2(IE)=J
                IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
                  WRITE(ICOUT,7826)IE,J
 7826             FORMAT('IE,J = ',2I8)
                  CALL DPWRST('XXX','BUG ')
                ENDIF
                IHLEFT=IH1
                IHLEF2=IH2
                NEWNAM='NO'
                IF(IECASE(IE).EQ.'NEW')NEWNAM='YES'
                ILISTL=J
C
                IF(NUMFRD.EQ.1)THEN
                  IF(N2.LE.0)GOTO7832
                  ICOL1=1
                  ICOL2=N2
                  IF(ICOL2.GT.ICOL1+N2-1)ICOL2=ICOL1+N2-1
                  I2=0
                  DO7831I=ICOL1,ICOL2
                    I2=I2+1
                    IFUNC3(I2)=IFUNC2(I2)
 7831             CONTINUE
 7832             CONTINUE
                  N3=I2
C
                ELSE
                  IWORD=IE
                  IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
                    WRITE(ICOUT,7841)ICOL1,ICOL2,ISTART,ISTOP,N2,N3,
     1                               IE,IWORD
 7841               FORMAT('ICOL1,ICOL2,ISTART,ISTOP,N2,N3,IE,IWORD = ',
     1                     8I8)
                    CALL DPWRST('XXX','BUG ')
                  ENDIF
                  CALL DPEXW2(IFUNC2,N2,ISTART,ISTOP,IWORD,
     1                        ICOL1,ICOL2,IFUNC3,N3,
     1                        IBUGS2,ISUBRO,IERROR)
                  IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
                    WRITE(ICOUT,7841)ICOL1,ICOL2,ISTART,ISTOP,N2,N3,
     1                               IE,IWORD
                    CALL DPWRST('XXX','BUG ')
                  ENDIF
                ENDIF
C
                CALL DPINFU(IFUNC3,N3,IHNAME,IHNAM2,IUSE,IN,
     1                      IVSTAR,IVSTOP,
     1                      NUMNAM,IANSLC,IWIDTH,IHLEFT,IHLEF2,ILISTL,
     1                      NEWNAM,MAXNME,
     1                      IFUNC,NUMCHF,MAXCHF,IBUGS2,IERROR)
C
                IF(NEWNAM.EQ.'YES'.AND.IERROR.EQ.'NO')NUMNAM=NUMNAM-1
C
              ENDIF
 7820       CONTINUE
 7810     CONTINUE
        ENDIF
        GOTO7900
C
      ELSEIF(ICASRE.EQ.'ROWI')THEN
        GOTO7900
      ELSE
C
        ISTEPN='7600'
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,7601)ICASRE,NUMVRD,NUMNAM,NUME,NXC
 7601     FORMAT('ICASRE,NUMVRD,NUMNAM,NUME,NXC = ',A4,4I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
C       UPDATE NUMERIC VARIABLES
C
        IF(NUMVRD.GT.0)THEN
          DO7610IE=1,NUMVRD
            N=IEN(IE)
            ICOLVJ=IECOL2(IE)
            DO7620J=1,NUMNAM
              IF(IUSE(J).EQ.'V'.AND.IVALUE(J).EQ.ICOLVJ)THEN
                IUSE(J)='V'
                IVALUE(J)=ICOLVJ
                IF(N.GT.IN(J))IN(J)=N
                IVSTAR(J)=MAXN*(ICOLVJ-1)+1
                IVSTOP(J)=MAXN*(ICOLVJ-1)+N
              ENDIF
 7620       CONTINUE
 7610     CONTINUE
        ENDIF
C
        NUMVRP=NUMVRD+1
        IF(ICASRE.EQ.'MATR')GOTO7690
        IF(NUMVRP.GT.NUME)GOTO7690
        DO7650IE=NUMVRP,NUME
          IEREV=NUME-IE+NUMVRP
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
 7650   CONTINUE
 7690   CONTINUE
C
C       UPDATE CHARACTER VARIABLES CONVERTED TO CATEGORICAL VARIABLES
C
        IF(IGRPAU.EQ.'CATE' .AND. NXCSAV.GE.1)THEN
          DO77610IE=1,NXCSAV
            IF(IRWLC3.EQ.IE)GOTO77610
            N=IENC(IE)
            ICOLVJ=IECOLC(IE)
            DO77620J=1,NUMNAM
              IF(IUSE(J).EQ.'V'.AND.IVALUE(J).EQ.ICOLVJ)THEN
                IUSE(J)='V'
                IVALUE(J)=ICOLVJ
                IF(N.GT.IN(J))IN(J)=N
                IVSTAR(J)=MAXN*(ICOLVJ-1)+1
                IVSTOP(J)=MAXN*(ICOLVJ-1)+N
              ENDIF
77620       CONTINUE
77610     CONTINUE
        ENDIF
C
        GOTO7900
      ENDIF
C
 7900 CONTINUE
C
C               *************************************
C               **  STEP 12--                      **
C               **  WRITE OUT SUMMARY INFORMATION  **
C               **  ABOUT THE FILE THAT WAS READ   **
C               *************************************
C
      ISTEPN='12'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     2014/10: SAVE FOLLOWING AS INTERNAL PARAMETERS:
C
C              1) ISKIP   = NUMBER OF HEADER LINES SKIPPED
C              2) NUMLRD  = NUMBER OF LINES READ
C              3) NUMVRD  = NUMBER OF VARIABLES READ
C
C              WRITE INDIVIDUAL VARIABLE NAMES TO: ZZZV1 - ZZZVK
C
      IH1='ISKI'
      IH2='P   '
      VALUE0=REAL(ISKIP)
      CALL DPADDP(IH1,IH2,VALUE0,IHOST1,ISUBN0,
     1            IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1            IANS,IWIDTH,IBUGS2,IERROR)
C
      IH1='NUML'
      IH2='RD  '
      VALUE0=REAL(NUMLRD)
      CALL DPADDP(IH1,IH2,VALUE0,IHOST1,ISUBN0,
     1            IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1            IANS,IWIDTH,IBUGS2,IERROR)
C
      IH1='NUMV'
      IH2='RD  '
      VALUE0=REAL(NUMVRD)
      CALL DPADDP(IH1,IH2,VALUE0,IHOST1,ISUBN0,
     1            IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1            IANS,IWIDTH,IBUGS2,IERROR)
C
      IF(NUMVRD.GT.0)THEN
        DO12001II=1,NUMVRD
          IH1='ZZZV'
          IH2='    '
          IF(II.LE.9)THEN
            WRITE(IH2(1:1),'(I1)')II
          ELSEIF(II.LE.99)THEN
            WRITE(IH2(1:2),'(I2)')II
          ELSEIF(II.LE.999)THEN
            WRITE(IH2(1:3),'(I3)')II
          ELSEIF(II.LE.9999)THEN
            WRITE(IH2(1:4),'(I4)')II
          ELSE
            GOTO12001
          ENDIF
C
          DO12003JJ=1,8
            ISTRZ2(JJ)=' '
12003     CONTINUE
          ISTRZ2(1)(1:1)=JENAM1(II)(1:1)
          ISTRZ2(2)(1:1)=JENAM1(II)(2:2)
          ISTRZ2(3)(1:1)=JENAM1(II)(3:3)
          ISTRZ2(4)(1:1)=JENAM1(II)(4:4)
          ISTRZ2(5)(1:1)=JENAM2(II)(1:1)
          ISTRZ2(6)(1:1)=JENAM2(II)(2:2)
          ISTRZ2(7)(1:1)=JENAM2(II)(3:3)
          ISTRZ2(8)(1:1)=JENAM2(II)(4:4)
          NCHART=1
          DO12005JJ=8,1,-1
            IF(ISTRZ2(JJ)(1:1).NE.' ')THEN
              NCHART=JJ
              GOTO12009
            ENDIF
12005     CONTINUE
12009     CONTINUE
C
          NEWNAM='YES'
          DO12011JJ=1,NUMNAM
            IF(IH1.EQ.IHNAME(JJ) .AND. IH2.EQ.IHNAM2(JJ))THEN
              NEWNAM='OLD'
              ILISTL=JJ
              GOTO12019
            ENDIF
12011     CONTINUE
12019     CONTINUE
          IF(NEWNAM.EQ.'YES')ILISTL=NUMNAM+1
          CALL DPINFU(ISTRZ2,NCHART,IHNAME,IHNAM2,IUSE,IN,
     1                IVSTAR,IVSTOP,
     1                NUMNAM,IANS,IWIDTH,IH1,IH2,ILISTL,
     1                NEWNAM,MAXNME,
     1                IFUNC,NUMCHF,MAXCHF,IBUGS2,IERROR)
C
12001   CONTINUE
      ENDIF
C
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8000)
 8000   FORMAT('INPUT DATA FILE SUMMARY INFORMATION--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8001)IRD2
 8001   FORMAT('INPUT UNIT DEVICE NUMBER         = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8002)IFCOL3,IFCOL4
 8002   FORMAT('INPUT FILE COLUMN     LIMITS     = ',I8,4X,I8)
        CALL DPWRST('XXX','BUG ')
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
          WRITE(ICOUT,1111)AFROW2
 1111     FORMAT('AFROW2 = ',E15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IF(IFROW2.EQ.INTINF)THEN
          WRITE(ICOUT,8003)IFROW1
 8003     FORMAT('INPUT FILE ROW        LIMITS     = ',I8,4X,'INFINITY')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(IFROW2.NE.INTINF)THEN
          WRITE(ICOUT,8004)IFROW1,IFROW2
 8004     FORMAT('INPUT FILE ROW        LIMITS     = ',I8,4X,I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        WRITE(ICOUT,8005)ISKIP
 8005   FORMAT('NUMBER OF HEADER LINES SKIPPED   = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8006)NUMLRD
 8006   FORMAT('NUMBER OF DATA   LINES READ      = ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMVRD.GE.1)THEN
          WRITE(ICOUT,8007)NUMVRD
 8007     FORMAT('NUMBER OF VARIABLES    READ      = ',I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IF(NUMPRD.GE.1)THEN
          WRITE(ICOUT,8008)NUMPRD
 8008     FORMAT('NUMBER OF PARAMETERS   READ      = ',I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IF(NUMFRD.GE.1)THEN
          WRITE(ICOUT,8009)NUMFRD
 8009     FORMAT('NUMBER OF FUNCTIONS/STRINGS READ = ',I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IF(NCREAF.LE.0)THEN
C
          IFRST=IFCOL3
          IF(IFRST+240-1.GE.IFCOL4)THEN
            ILAST=IFCOL4
          ELSE
            ILAST=IFRST+240-1
          ENDIF
C
          IF(ICASRE.NE.'CLIP')THEN
            WRITE(ICOUT,8011)
 8011       FORMAT('THE SCANNED REGION OF THE FIRST DATA LINE READ ',
     1             '(TO A MAXIMUM OF 240 CHARACTERS) = ')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,8012)(ISTOR3(J),J=IFRST,MIN(240,ILAST))
 8012       FORMAT(240A1)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,8013)
 8013       FORMAT('THE SCANNED REGION OF THE LAST  DATA LINE READ ',
     1             '(TO A MAXIMUM OF 240 CHARACTERS) = ')
            CALL DPWRST('XXX','BUG ')
            IF(IENDTY.EQ.1)THEN
              WRITE(ICOUT,8014)(ISTOR1(J),J=IFRST,MIN(240,ILAST))
              CALL DPWRST('XXX','BUG ')
            ELSEIF(IENDTY.EQ.2)THEN
              WRITE(ICOUT,8014)(ISTOR2(J),J=IFRST,MIN(240,ILAST))
 8014         FORMAT(240A1)
              CALL DPWRST('XXX','BUG ')
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C
C               *********************************************
C               **  STEP 13--                              **
C               **  PRINT OUT SUMMARY INFORMATION          **
C               **  VARIABLES/PARAMETERS/FUNCTIONS         **
C               **  THAT WERE READ IN.                     **
C               *********************************************
C
      IF(ICASRE.EQ.'PARA')THEN
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8201)
 8201     FORMAT('PARAMETER     VALUE')
          CALL DPWRST('XXX','BUG ')
C
          DO8210IE=1,NUME
            IH1=JENAM1(IE)
            IH2=JENAM2(IE)
            DO8220I=1,NUMNAM
              I2=I
              IF(IH1.EQ.IHNAME(I).AND.IH2.EQ.IHNAM2(I))THEN
                WRITE(ICOUT,8226)IH1,IH2,VALUE(I2)
 8226           FORMAT(A4,A4,4X,E15.7)
                CALL DPWRST('XXX','BUG ')
              ENDIF
 8220       CONTINUE
 8210     CONTINUE
        ENDIF
C
      ELSEIF(ICASRE.EQ.'FUNC' .OR. ICASRE.EQ.'CFUN')THEN
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8301)
 8301     FORMAT('FUNCTION (= STRING)     CONTENT')
          CALL DPWRST('XXX','BUG ')
C
          DO8310IE=1,NUME
            IH1=JENAM1(IE)
            IH2=JENAM2(IE)
            DO8320I=1,NUMNAM
              I2=I
              IF(IH1.EQ.IHNAME(I).AND.IH2.EQ.IHNAM2(I))THEN
                JMIN=IVSTAR(I2)
                JMAX=IVSTOP(I2)
                WRITE(ICOUT,8326)IH1,IH2,(IFUNC(J),J=JMIN,JMAX)
 8326           FORMAT(A4,A4,10X,80A1)
                CALL DPWRST('XXX','BUG ')
              ENDIF
 8320       CONTINUE
 8310     CONTINUE
        ENDIF
C
      ELSEIF(ICASRE.EQ.'MATR')THEN
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8401)IHMAT1,IHMAT2,IMATNR
 8401     FORMAT('        MATRIX ',A4,A4,'--     ',I8,' ROWS')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8402)IMATNC
 8402     FORMAT('               ',4X,4X,'--     ',I8,' COLUMNS')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8404)
 8404     FORMAT('     VARIABLES        COLUMN    OBS/VARIABLE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8405)
 8405     FORMAT('(= COLUMN VECTORS)')
          CALL DPWRST('XXX','BUG ')
C
          DO8410IE=1,NUME
            IH1=JENAM1(IE)
            IH2=JENAM2(IE)
            DO8420I=1,NUMNAM
              I2=I
              IF(IH1.EQ.IHNAME(I).AND.IH2.EQ.IHNAM2(I))THEN
                WRITE(ICOUT,8426)IH1,IH2,IVALUE(I2),IN(I2)
 8426           FORMAT(8X,A4,A4,1X,I8,5X,I8)
                CALL DPWRST('XXX','BUG ')
              ENDIF
 8420       CONTINUE
 8410     CONTINUE
        ENDIF
      ELSEIF(ICASRE.EQ.'ROWI')THEN
        CONTINUE
      ELSE
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8101)
 8101     FORMAT('VARIABLE     COLUMN    OBS/VARIABLE')
          CALL DPWRST('XXX','BUG ')
C
          DO8110IE=1,NUME
            IH1=JENAM1(IE)
            IH2=JENAM2(IE)
            DO8120I=1,NUMNAM
              I2=I
              IF(IH1.EQ.IHNAME(I).AND.IH2.EQ.IHNAM2(I))THEN
                WRITE(ICOUT,8126)IH1,IH2,IVALUE(I2),IN(I2)
 8126           FORMAT(A4,A4,1X,I8,5X,I8)
                CALL DPWRST('XXX','BUG ')
              ENDIF
 8120       CONTINUE
 8110     CONTINUE
        ENDIF
        GOTO8800
      ENDIF
C
C               ***************************************
C               **  STEP 88--                        **
C               **  FOR THE FILE CASE,               **
C               **  CLOSE THE FILE.                  **
C               ***************************************
C
 8800 CONTINUE
      ISTEPN='88'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IOFILE.EQ.'YES'.AND.ICURST.EQ.'OPEN')GOTO8810
      GOTO8890
 8810 CONTINUE
      IENDFI='OFF'
      IREWIN='ON'
      IF(IREARW.EQ.'ON')THEN
         CALL DPCLFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1   IENDFI,IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
         IREACS='CLOSED'
      ENDIF
 8890 CONTINUE
C
      IF(IFEEDB.EQ.'ON' .AND. ICASRE.EQ.'ROWR')THEN
        WRITE(ICOUT,55805)NUMLRD,IVBASE,NUMDPL
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ******************************************
C               **  STEP 89--                           **
C               **  IF THE MACRO STATUS IS OPEN         **
C               **  THEN CHANGE IDEV FROM READ TO MACR  **
C               ******************************************
C
CCCCC IF(IMACST.EQ.'OPFI')IDEV='MACR'
CCCCC IF(IMACCS.EQ.'OPEN')IDEV='MACR'
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
C
      IREADL=IREAD2
      ISKIP=ISKPSV
C
C     2019/09: IF CHARACTER DATA WAS READ, DO THE FOLLOWING:
C
C              1. IF CHARACTER VARIABLES ARE IN "OVERWRITE" MODE, THEN
C
C                 A. CLOSE AND REOPEN "dpst2f.dat".
C
C                 B. OPEN THE CHARACTER DATA FILE ("dpzchf.dat" BY DEFAULT).
C
C                 C. LOOP TRHOUGH AND WRITE CONTENTS OF "dpst2f.dat" TO
C                    THE CHARACTER DATA FILE.  ADD THE NUMBER OF LINES
C                    FOR EACH VARIABLE.
C
C              2. IF CHARACTER VARIABLES ARE IN "APPEND" MODE, THEN
C
C                 A. CLOSE AND REOPEN "dpst2f.dat".
C
C                 B. OPEN THE CHARACTER DATA FILE.
C
C                 C. OPEN THE TEMPORARY FILE "dpst5f.dat".
C
C                 C. LOOP THROUGH AND APPEND CONTENTS OF "dpzchf.dat"
C                    AND "dpst2f.dat" AND WRITE TO "dpst5f.dat".
C
C                 D. COPY "dpst5f.dat" TO "dpzchf.dat".
C
      IF(IZCHCS.EQ.'OPEN')THEN
C
C       CHECK IF CHARACTER VARIABLE FILE EXISTS.  IF NOT, THEN USE
C       "OVERWRITE" METHOD.
C
        IFILE4=IZCHNA
        ISUBN0='READ'
        IERRFI='NO'
        CALL DPINFI(IFILE4,IEXIST,IOPEN,IACC,ISUBN0,IBUGS2,
     1              ISUBRO,IERROR)
C
        IF(ISTRVA.EQ.'OVER' .OR. IEXIST.EQ.'NO')THEN
C
C         STEP 1: CLOSE AND REOPEN "dpst2f.dat" FILE
C
          IOP='CLOS'
          IFLG11=0
          IFLG21=1
          IFLG31=0
          IFLAG4=0
          IFLAG5=0
          CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,
     1                IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1                IBUGS2,ISUBRO,IERROR)
          IOP='OPEN'
          CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,
     1                IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1                IBUGS2,ISUBRO,IERROR)
C
C         STEP 2: OPEN THE CHARACTER DATA FILE
C
          IOUNI4=IZCHNU
          IFILE4=IZCHNA
          ISTAT4=IZCHST
          IFORM4=IZCHFO
          IACCE4=IZCHAC
          IPROT4=IZCHPR
          ICURS4=IZCHCS
C
          ISUBN0='READ'
          IERRFI='NO'
          CALL DPOPFI(IOUNI4,IFILE4,ISTAT4,IFORM4,IACCE4,IPROT4,ICURS4,
     1                IREWI4,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
C
C         STEP 3: UPDATE THE CONTENTS OF THE CHARACTER DATA FILE
C
          READ(IOUNI2,*,ERR=9047,END=9047)IVAR
          WRITE(IOUNI4,'(I8)')IVAR
C
          DO9041KK=1,IVAR
            ISTR=' '
            READ(IOUNI2,'(A8)',ERR=9047,END=9047)ISTR(1:8)
            WRITE(IOUNI4,'(A8,I10)')ISTR(1:8),NUMLRD
 9041     CONTINUE
C
          IFRMT=' '
          IFRMT='(A    )'
          NTEMP=25*IVAR
          IF(NTEMP.GT.9999)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,211)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9042)
 9042       FORMAT('      MAXIMUM NUMBER OF CHARACTER VARIABLES ',
     1             'EXCEEDED.')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9049
          ENDIF
          WRITE(IFRMT(3:6),'(I4)')NTEMP
          DO9043KK=1,NUMLRD
            ISTR=' '
            READ(IOUNI2,IFRMT,ERR=9047,END=9047)ISTR(1:NTEMP)
            WRITE(IOUNI4,IFRMT)ISTR(1:NTEMP)
 9043     CONTINUE
          GOTO9049
C
 9047     CONTINUE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,211)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,9048)
 9048     FORMAT('      ERROR IN CREATING CHARACTER VARIABLE FILE.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
C
C         STEP 4: CLOSE THE CHARACTER DATA FILE AND "dpst2f.dat"
C
 9049     CONTINUE
          CALL DPCLFI(IOUNI4,IFILE4,ISTAT4,IFORM4,IACCE4,IPROT4,ICURS4,
     1                IENDFI,IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
          IZCHCS='CLOSED'
C
          IOP='CLOS'
          CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,
     1                IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1                IBUGS2,ISUBRO,IERROR)
C
        ELSE
C
C         STEP 1: CLOSE AND REOPEN "dpst2f.dat" FILE, ALSO OPEN
C                 "dpst5f.dat"
C
          IOP='CLOS'
          IFLG11=0
          IFLG21=1
          IFLG31=0
          IFLAG4=0
          IFLAG5=0
          CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,
     1                IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1                IBUGS2,ISUBRO,IERROR)
          IOP='OPEN'
          IFLAG5=1
          CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,
     1                IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1                IBUGS2,ISUBRO,IERROR)
C
C         STEP 2: OPEN THE CHARACTER DATA FILE
C
          IOUNI4=IZCHNU
          IFILE4=IZCHNA
          ISTAT4=IZCHST
          IFORM4=IZCHFO
          IACCE4=IZCHAC
          IPROT4=IZCHPR
          ICURS4=IZCHCS
C
          ISUBN0='READ'
          IERRFI='NO'
          CALL DPOPFI(IOUNI4,IFILE4,ISTAT4,IFORM4,IACCE4,IPROT4,ICURS4,
     1                IREWI4,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
C
C         STEP 3: UPDATE THE CONTENTS OF THE CHARACTER DATA FILE
C
          READ(IOUNI2,*,ERR=9057,END=9057)IVAR1
          READ(IOUNI4,*,ERR=9057,END=9057)IVAR2
          IVAR=IVAR1 + IVAR2
          WRITE(IOUNI5,'(I8)')IVAR
C
C         OLD DATA
C
          MXROW1=-1
          DO9051KK=1,IVAR2
            READ(IOUNI4,'(2A4,I10)',ERR=9057,END=9057)JVNAM1(KK),
     1           JVNAM2(KK),NIV(KK)
            IF(NIV(KK).GT.MXROW1)MXROW1=NIV(KK)
            WRITE(IOUNI5,'(2A4,I10)')JVNAM1(KK),JVNAM2(KK),NIV(KK)
 9051     CONTINUE
C
C         NEW DATA
C
          MXROW2=NUMLRD
          DO9061KK=1,IVAR1
            IROW=IVAR1+KK
            READ(IOUNI2,'(2A4)',ERR=9057,END=9057)JVNAM1(IROW),
     1                                            JVNAM2(IROW)
            NIV(IROW)=NUMLRD
            WRITE(IOUNI5,'(2A4,I10)')JVNAM1(IROW),JVNAM2(IROW),NIV(IROW)
 9061     CONTINUE
C
          NTEMP1=25*IVAR1
          NTEMP2=25*IVAR2
          NTEMP=NTEMP1+NTEMP2
          IF(NTEMP.GT.9999)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,211)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9052)
 9052       FORMAT('      MAXIMUM NUMBER OF CHARACTER VARIABLES ',
     1             'EXCEEDED.')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9059
          ENDIF
C
          IFRMT=' '
          IFRMT='(A    )'
          WRITE(IFRMT(3:6),'(I4)')NTEMP2
          IFRMT2=' '
          IFRMT2='(A    )'
          WRITE(IFRMT2(3:6),'(I4)')NTEMP1
          IFRMT3=' '
          IFRMT3='(A    )'
          WRITE(IFRMT3(3:6),'(I4)')NTEMP
C
          DO9053KK=1,MAX(MXROW1,MXROW2)
            ISTR=' '
            IF(KK.LE.MXROW1 .AND. KK.LE.MXROW2)THEN
              READ(IOUNI4,IFRMT,ERR=9057,END=9057)ISTR(1:NTEMP2)
              READ(IOUNI2,IFRMT2,ERR=9057,END=9057)
     1             ISTR(NTEMP2+1:NTEMP1+NTEMP2)
              WRITE(IOUNI5,IFRMT3)ISTR(1:NTEMP)
            ELSEIF(KK.GT.MXROW1 .AND.KK.LE.MXROW2)THEN
              READ(IOUNI2,IFRMT2,ERR=9057,END=9057)
     1             ISTR(NTEMP2+1:NTEMP1+NTEMP2)
              WRITE(IOUNI5,IFRMT3)ISTR(1:NTEMP)
            ELSEIF(KK.LE.MXROW1 .AND.KK.GT.MXROW2)THEN
              READ(IOUNI4,IFRMT,ERR=9057,END=9057)ISTR(1:NTEMP2)
              ISTR(NTEMP2+1:NTEMP1+NTEMP2)=' '
              WRITE(IOUNI5,IFRMT3)ISTR(1:NTEMP)
            ENDIF
 9053     CONTINUE
          GOTO9059
C
 9057     CONTINUE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,211)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,9058)
 9058     FORMAT('      ERROR IN CREATING CHARACTER VARIABLE FILE.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
C
C         STEP 4: CLOSE THE CHARACTER DATA FILE AND "dpst2f.dat"
C
 9059     CONTINUE
          CALL DPCLFI(IOUNI4,IFILE4,ISTAT4,IFORM4,IACCE4,IPROT4,ICURS4,
     1                IENDFI,IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
          IZCHCS='CLOSED'
C
          IOP='CLOS'
          CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,
     1                IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1                IBUGS2,ISUBRO,IERROR)
C
C         STEP 5: COPY "dpst5f.dat" TO CHARACTER VARIABLE FILE
C
          CALL COPYFI(IFILE5,IFILE4,IBUGS2,ISUBRO,IERROR)
C
        ENDIF
        GOTO9090
      ENDIF
C
      IF(ICASRE.EQ.'IMAZ' .OR. ICASRE.EQ.'IMAG')THEN
#ifdef HAVE_GD
        CALL GDUNLO()
#endif
      ENDIF
C
 9090 CONTINUE
C
      IFILQU=IFILQ2
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPREAD--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFROW1,IFCOL1,IFCOL2,AFROW2,ICASRE
 9012   FORMAT('IFROW1,IFCOL1,IFCOL2,AFROW2,ICASRE = ',
     1         3I8,2X,E15.7,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)IFOUND,IERROR
 9015   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)NUMVRD,NUMPRD,NUMFRD
 9016   FORMAT('NUMVRD,NUMPRD,NUMFRD = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9017)IMACRO,IMACNU,IMACCS
 9017   FORMAT('IMACRO,IMACNU,IMACCS = ',A4,I8,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9019)IOSW,IOFILE,IOTERM,IRD,IRD2,IOUNIT
 9019   FORMAT('IOSW,IOFILE,IOTERM,IRD,IRD2,IOUNIT = ',3(A4,2X),3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9022)IFILE(1:80)
 9022   FORMAT('IFILE  = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9023)ISTAT,IFORM,IACCES,IPROT,ICURST
 9023   FORMAT('ISTAT,IFORM,IACCES,IPROT,ICURST  =',5(1X,A12))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9028)IENDFI,IREWIN,ISUBN0,IERRFI,NUMNAM
 9028   FORMAT('IENDFI,IREWIN,ISUBN0,IERRFI,NUMNAM = ',3(A4,1X),A12,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9031)N2,MAXN2,N3,NCREAF
 9031   FORMAT('N2,MAXN2,N3,NCREAF = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9032)(IFUNC2(I),I=1,100)
 9032   FORMAT('(IFUNC2(I),I=1,100) = ',100A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9033)(IFUNC3(I),I=1,100)
 9033   FORMAT('(IFUNC3(I),I=1,100) = ',100A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9036)IHMAT1,IHMAT2,INAMMA,IMATC1,IMATNR,IMATNC
 9036   FORMAT('IHMAT1,IHMAT2,INAMMA,IMATC1,IMATNR,IMATNC = ',
     1         A4,2X,A4,2X,4I8)
        CALL DPWRST('XXX','BUG ')
        IF(NCREAF.GE.1)THEN
          WRITE(ICOUT,9038)(ICREAF(I:I),I=1,NCREAF)
 9038     FORMAT('(ICREAF(I:I),I=1,NCREAF) = ',80A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        WRITE(ICOUT,9039)IREARW,ICOMCH,ICOMSW
 9039   FORMAT('IREARW,ICOMCH,ICOMSW = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END

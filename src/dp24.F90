      SUBROUTINE DPRCSN(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
                        IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR ROMAN COMPLEX SCRIPT NUMERIC.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 CENTER FOR APPLIED MATHEMATICS
!                 NATIONAL BUREAU OF STANDARDS
!                 WASHINGTON, D. C. 20234
!                 PHONE--301-921-3651
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--87/4
!     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MARCH     1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --MARCH     1987.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICHAR2
      CHARACTER*4 IOP
      CHARACTER*4 IBUGD2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      DIMENSION IOP(*)
      DIMENSION X(*)
      DIMENSION Y(*)
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
      NUMCO=1
      ISTART=1
      ISTOP=1
      NC=1
!
!               ******************************************
!               **  TREAT THE ROMAN SIMPLEX UPPER CASE  **
!               **  HERSHEY CHARACTER SET CASE          **
!               ******************************************
!
!
      IF(IBUGD2.EQ.'OFF')GO TO 90
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
!
!               **************************************************
!               **  STEP 1--                                    **
!               **  SEARCH FOR THE INPUT CHARACTER(S).          **
!               **  MAP THE CHARACTER(S) INTO A NUMERIC VALUE.  **
!               **************************************************
!
      CALL DPCHNU(ICHAR2,ICHARN,IBUGD2,IFOUND)
      IF(IFOUND.EQ.'NO')GO TO 9000
!
      IF(ICHARN.LE.9)GO TO 1010
      GO TO 1019
 1010 CONTINUE
      CALL DRCSN1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
      GO TO 9000
 1019 CONTINUE
!
      IF(ICHARN.GE.10)GO TO 1020
      GO TO 1029
 1020 CONTINUE
      CALL DRCSN2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
      GO TO 9000
 1029 CONTINUE
!
      IFOUND='NO'
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
      IF(NUMCO.GE.1.AND.NUMCO.LE.1000)GO TO 9019
      DO 9015 I=1,NUMCO
      WRITE(ICOUT,9016)I,IOP(I),X(I),Y(I)
 9016 FORMAT('I,IOP(I),X(I),Y(I) = ',I8,2X,A4,2F10.2)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9019 CONTINUE
      WRITE(ICOUT,9021)IXMINS,IXMAXS,IXDELS
 9021 FORMAT('IXMINS,IXMAXS,IXDELS = ',3I8)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPRCSN
      SUBROUTINE DPRCSU(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR ROMAN COMPLEX SCRIPT UPPER CASE.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 CENTER FOR APPLIED MATHEMATICS
!                 NATIONAL BUREAU OF STANDARDS
!                 WASHINGTON, D. C. 20234
!                 PHONE--301-921-3651
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--87/4
!     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MARCH     1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --MARCH     1987.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICHAR2
      CHARACTER*4 IOP
      CHARACTER*4 IBUGD2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      DIMENSION IOP(*)
      DIMENSION X(*)
      DIMENSION Y(*)
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
      NUMCO=1
      ISTART=1
      ISTOP=1
      NC=1
!
!               ******************************************
!               **  TREAT THE ROMAN SIMPLEX UPPER CASE  **
!               **  HERSHEY CHARACTER SET CASE          **
!               ******************************************
!
!
      IF(IBUGD2.EQ.'OFF')GO TO 90
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
!
!               **************************************************
!               **  STEP 1--                                    **
!               **  SEARCH FOR THE INPUT CHARACTER(S).          **
!               **  MAP THE CHARACTER(S) INTO A NUMERIC VALUE.  **
!               **************************************************
!
      CALL DPCHAL(ICHAR2,ICHARN,IBUGD2,IFOUND)
      IF(IFOUND.EQ.'NO')GO TO 9000
!
      IF(ICHARN.LE.6)GO TO 1010
      GO TO 1019
 1010 CONTINUE
      CALL DRCSU1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
      GO TO 9000
 1019 CONTINUE
!
      IF(7.LE.ICHARN.AND.ICHARN.LE.13)GO TO 1020
      GO TO 1029
 1020 CONTINUE
      CALL DRCSU2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
      GO TO 9000
 1029 CONTINUE
!
      IF(14.LE.ICHARN.AND.ICHARN.LE.20)GO TO 1030
      GO TO 1039
 1030 CONTINUE
      CALL DRCSU3(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
      GO TO 9000
 1039 CONTINUE
!
      IF(ICHARN.GE.21)GO TO 1040
      GO TO 1049
 1040 CONTINUE
      CALL DRCSU4(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
      GO TO 9000
 1049 CONTINUE
!
      IFOUND='NO'
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
      IF(NUMCO.GE.1.AND.NUMCO.LE.1000)GO TO 9019
      DO 9015 I=1,NUMCO
      WRITE(ICOUT,9016)I,IOP(I),X(I),Y(I)
 9016 FORMAT('I,IOP(I),X(I),Y(I) = ',I8,2X,A4,2F10.2)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9019 CONTINUE
      WRITE(ICOUT,9021)IXMINS,IXMAXS,IXDELS
 9021 FORMAT('IXMINS,IXMAXS,IXDELS = ',3I8)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPRCSU
      SUBROUTINE DPRCU(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR ROMAN COMPLEX UPPER CASE.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 CENTER FOR APPLIED MATHEMATICS
!                 NATIONAL BUREAU OF STANDARDS
!                 WASHINGTON, D. C. 20234
!                 PHONE--301-921-3651
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--87/4
!     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MARCH     1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --MARCH     1987.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICHAR2
      CHARACTER*4 IOP
      CHARACTER*4 IBUGD2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      DIMENSION IOP(*)
      DIMENSION X(*)
      DIMENSION Y(*)
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
      NUMCO=1
      ISTART=1
      ISTOP=1
      NC=1
!
!               ******************************************
!               **  TREAT THE ROMAN SIMPLEX UPPER CASE  **
!               **  HERSHEY CHARACTER SET CASE          **
!               ******************************************
!
!
      IF(IBUGD2.EQ.'OFF')GO TO 90
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
!
!               **************************************************
!               **  STEP 1--                                    **
!               **  SEARCH FOR THE INPUT CHARACTER(S).          **
!               **  MAP THE CHARACTER(S) INTO A NUMERIC VALUE.  **
!               **************************************************
!
      CALL DPCHAL(ICHAR2,ICHARN,IBUGD2,IFOUND)
      IF(IFOUND.EQ.'NO')GO TO 9000
!
      IF(ICHARN.LE.14)GO TO 1010
      GO TO 1019
 1010 CONTINUE
      CALL DRCU1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
      GO TO 9000
 1019 CONTINUE
!
      IF(ICHARN.GE.15)GO TO 1020
      GO TO 1029
 1020 CONTINUE
      CALL DRCU2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
      GO TO 9000
 1029 CONTINUE
!
      IFOUND='NO'
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
      IF(NUMCO.GE.1.AND.NUMCO.LE.1000)GO TO 9019
      DO 9015 I=1,NUMCO
      WRITE(ICOUT,9016)I,IOP(I),X(I),Y(I)
 9016 FORMAT('I,IOP(I),X(I),Y(I) = ',I8,2X,A4,2F10.2)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9019 CONTINUE
      WRITE(ICOUT,9021)IXMINS,IXMAXS,IXDELS
 9021 FORMAT('IXMINS,IXMAXS,IXDELS = ',3I8)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPRCU
      SUBROUTINE DPRDL(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR ROMAN DUPLEX LOWER CASE.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 CENTER FOR APPLIED MATHEMATICS
!                 NATIONAL BUREAU OF STANDARDS
!                 WASHINGTON, D. C. 20234
!                 PHONE--301-921-3651
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--87/4
!     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MARCH     1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --MARCH     1987.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICHAR2
      CHARACTER*4 IOP
      CHARACTER*4 IBUGD2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      DIMENSION IOP(*)
      DIMENSION X(*)
      DIMENSION Y(*)
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
      NUMCO=1
      ISTART=1
      ISTOP=1
      NC=1
!
!               ******************************************
!               **  TREAT THE ROMAN SIMPLEX UPPER CASE  **
!               **  HERSHEY CHARACTER SET CASE          **
!               ******************************************
!
!
      IF(IBUGD2.EQ.'OFF')GO TO 90
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
!
!               **************************************************
!               **  STEP 1--                                    **
!               **  SEARCH FOR THE INPUT CHARACTER(S).          **
!               **  MAP THE CHARACTER(S) INTO A NUMERIC VALUE.  **
!               **************************************************
!
      CALL DPCHAL(ICHAR2,ICHARN,IBUGD2,IFOUND)
      IF(IFOUND.EQ.'NO')GO TO 9000
!
      IF(ICHARN.LE.11)GO TO 1010
      GO TO 1019
 1010 CONTINUE
      CALL DRDL1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
      GO TO 9000
 1019 CONTINUE
!
      IF(12.LE.ICHARN.AND.ICHARN.LE.24)GO TO 1020
      GO TO 1029
 1020 CONTINUE
      CALL DRDL2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
      GO TO 9000
 1029 CONTINUE
!
      IF(ICHARN.GE.25)GO TO 1030
      GO TO 1039
 1030 CONTINUE
      CALL DRDL3(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
      GO TO 9000
 1039 CONTINUE
!
      IFOUND='NO'
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
      IF(NUMCO.GE.1.AND.NUMCO.LE.1000)GO TO 9019
      DO 9015 I=1,NUMCO
      WRITE(ICOUT,9016)I,IOP(I),X(I),Y(I)
 9016 FORMAT('I,IOP(I),X(I),Y(I) = ',I8,2X,A4,2F10.2)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9019 CONTINUE
      WRITE(ICOUT,9021)IXMINS,IXMAXS,IXDELS
 9021 FORMAT('IXMINS,IXMAXS,IXDELS = ',3I8)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPRDL
      SUBROUTINE DPRDN(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR ROMAN DUPLEX NUMERIC.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 CENTER FOR APPLIED MATHEMATICS
!                 NATIONAL BUREAU OF STANDARDS
!                 WASHINGTON, D. C. 20234
!                 PHONE--301-921-3651
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--87/4
!     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MARCH     1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --MARCH     1987.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICHAR2
      CHARACTER*4 IOP
      CHARACTER*4 IBUGD2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      DIMENSION IOP(*)
      DIMENSION X(*)
      DIMENSION Y(*)
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
      NUMCO=1
      ISTART=1
      ISTOP=1
      NC=1
!
!               ******************************************
!               **  TREAT THE ROMAN SIMPLEX UPPER CASE  **
!               **  HERSHEY CHARACTER SET CASE          **
!               ******************************************
!
!
      IF(IBUGD2.EQ.'OFF')GO TO 90
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
!
!               **************************************************
!               **  STEP 1--                                    **
!               **  SEARCH FOR THE INPUT CHARACTER(S).          **
!               **  MAP THE CHARACTER(S) INTO A NUMERIC VALUE.  **
!               **************************************************
!
      CALL DPCHNU(ICHAR2,ICHARN,IBUGD2,IFOUND)
      IF(IFOUND.EQ.'NO')GO TO 9000
!
      IF(ICHARN.LE.8)GO TO 1010
      GO TO 1019
 1010 CONTINUE
      CALL DRDN1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
      GO TO 9000
 1019 CONTINUE
!
      IF(ICHARN.GE.9)GO TO 1020
      GO TO 1029
 1020 CONTINUE
      CALL DRDN2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
      GO TO 9000
 1029 CONTINUE
!
      IFOUND='NO'
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
      IF(NUMCO.GE.1.AND.NUMCO.LE.1000)GO TO 9019
      DO 9015 I=1,NUMCO
      WRITE(ICOUT,9016)I,IOP(I),X(I),Y(I)
 9016 FORMAT('I,IOP(I),X(I),Y(I) = ',I8,2X,A4,2F10.2)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9019 CONTINUE
      WRITE(ICOUT,9021)IXMINS,IXMAXS,IXDELS
 9021 FORMAT('IXMINS,IXMAXS,IXDELS = ',3I8)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPRDN
      SUBROUTINE DPRDS(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR ROMAN DUPLEX SYMBOLS.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 CENTER FOR APPLIED MATHEMATICS
!                 NATIONAL BUREAU OF STANDARDS
!                 WASHINGTON, D. C. 20234
!                 PHONE--301-921-3651
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--87/4
!     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MARCH     1981.
!     UPDATED         --MARCH     1982.
!     UPDATED         --MARCH     1987.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICHAR2
      CHARACTER*4 IOP
      CHARACTER*4 IBUGD2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      DIMENSION IOP(*)
      DIMENSION X(*)
      DIMENSION Y(*)
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
      NUMCO=1
      ISTART=1
      ISTOP=1
      NC=1
!
!               ******************************************
!               **  TREAT THE ROMAN SIMPLEX UPPER CASE  **
!               **  HERSHEY CHARACTER SET CASE          **
!               ******************************************
!
!
      IF(IBUGD2.EQ.'OFF')GO TO 90
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
!
!               **************************************************
!               **  STEP 1--                                    **
!               **  SEARCH FOR THE INPUT CHARACTER(S).          **
!               **  MAP THE CHARACTER(S) INTO A NUMERIC VALUE.  **
!               **************************************************
!
      CALL DPCHSY(ICHAR2,ICHARN,IBUGD2,IFOUND)
      IF(IFOUND.EQ.'NO')GO TO 9000
!
      IF(ICHARN.LE.9)GO TO 1010
      GO TO 1019
 1010 CONTINUE
      CALL DRDS1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
      GO TO 9000
 1019 CONTINUE
!
      IF(ICHARN.GE.10)GO TO 1020
      GO TO 1029
 1020 CONTINUE
      CALL DRDS2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
      GO TO 9000
 1029 CONTINUE
!
      IFOUND='NO'
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
      IF(NUMCO.GE.1.AND.NUMCO.LE.1000)GO TO 9019
      DO 9015 I=1,NUMCO
      WRITE(ICOUT,9016)I,IOP(I),X(I),Y(I)
 9016 FORMAT('I,IOP(I),X(I),Y(I) = ',I8,2X,A4,2F10.2)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9019 CONTINUE
      WRITE(ICOUT,9021)IXMINS,IXMAXS,IXDELS
 9021 FORMAT('IXMINS,IXMAXS,IXDELS = ',3I8)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPRDS
      SUBROUTINE DPRDU(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR ROMAN DUPLEX UPPER CASE.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 CENTER FOR APPLIED MATHEMATICS
!                 NATIONAL BUREAU OF STANDARDS
!                 WASHINGTON, D. C. 20234
!                 PHONE--301-921-3651
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--87/4
!     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MARCH     1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --MARCH     1987.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICHAR2
      CHARACTER*4 IOP
      CHARACTER*4 IBUGD2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      DIMENSION IOP(*)
      DIMENSION X(*)
      DIMENSION Y(*)
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
      NUMCO=1
      ISTART=1
      ISTOP=1
      NC=1
!
!               ******************************************
!               **  TREAT THE ROMAN SIMPLEX UPPER CASE  **
!               **  HERSHEY CHARACTER SET CASE          **
!               ******************************************
!
!
      IF(IBUGD2.EQ.'OFF')GO TO 90
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
!
!               **************************************************
!               **  STEP 1--                                    **
!               **  SEARCH FOR THE INPUT CHARACTER(S).          **
!               **  MAP THE CHARACTER(S) INTO A NUMERIC VALUE.  **
!               **************************************************
!
      CALL DPCHAL(ICHAR2,ICHARN,IBUGD2,IFOUND)
      IF(IFOUND.EQ.'NO')GO TO 9000
!
      IF(ICHARN.LE.14)GO TO 1010
      GO TO 1019
 1010 CONTINUE
      CALL DRDU1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
      GO TO 9000
 1019 CONTINUE
!
      IF(ICHARN.GE.15)GO TO 1020
      GO TO 1029
 1020 CONTINUE
      CALL DRDU2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
      GO TO 9000
 1029 CONTINUE
!
      IFOUND='NO'
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
      IF(NUMCO.GE.1.AND.NUMCO.LE.1000)GO TO 9019
      DO 9015 I=1,NUMCO
      WRITE(ICOUT,9016)I,IOP(I),X(I),Y(I)
 9016 FORMAT('I,IOP(I),X(I),Y(I) = ',I8,2X,A4,2F10.2)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9019 CONTINUE
      WRITE(ICOUT,9021)IXMINS,IXMAXS,IXDELS
 9021 FORMAT('IXMINS,IXMAXS,IXDELS = ',3I8)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPRDU
      SUBROUTINE DPREAD(IFROW1,IFROW2,IFCOL1,IFCOL2,ISKIP,INTINF,         &
                        IMACRO,IMACNU,IMACCS,IMALEV,IOSW,ICREAF,NCREAF,   &
                        IREARW,ICOMCH,ICOMSW,                             &
                        IUNFOF,IUNFNR,IUNFMC,NUMRCM,                      &
                        IFCOLL,IFCOLU,                                    &
                        IANSLO,ILOOST,ILOOLI,IREPCH,                      &
                        IBUGS2,IBUGQ,ISUBRO,IFOUND,IERROR)
!CCCC MAY 1990.  ADD ICOMCH, ICOMSW TO CALL LIST
!CCCC APRIL, 1995.  ADD IUNFOF, IYNFNR, IUNFMC TO CALL LIST
!CCCC MARCH, 1996.  ADD IMALEV TO CALL LIST
!CCCC FEBRUARY 2003.  ADD NUMRCM TO CALL LIST
!CCCC JANUARY 2015.  ADD "LOOP" ARGUMENTS
!
!     PURPOSE--READ IN THE VALUES OF A VARIABLE.  THE DATA IS LISTED
!              ACROSS A LINE IMAGE.  (E.G., X(1) Y(1) Z(1) ETC.)
!              THE DATA IS READ FORM A MASS STORAGE FILE
!              OR (IF NO FILE GIVEN) FROM THE DEFAULT INPUT UNIT
!              (WHICH WILL BE THE TERMINAL).
!     ASSUMPTION--THE INPUT  FILE ALREADY EXISTS; (THAT IS, DATAPLOT
!                 WILL AUTOMATICALLY OPEN THE FILE
!                 VIA (ON THE UNIVAC 1108), BY AN @ASG,AX ...)
!                 BUT WILL NOT AUTOMATICALLY CREATE THE FILE
!                 VIA (ON THE UNIVAC 1108), BY AN @ASG,UP ...))
!     ASSUMPTION--THE COMPUTER SYSTEM IS SUCH THAT EQUATING THE FILE NAME
!                 TO THE FORTRAN NUMERIC DESIGNATION OF 31 (OR HOWEVER
!                 THE VARIABLE  IREANU  IS DEFINED IN INITFO) IS
!                 PERMISSIBLE.
!     NOTE--INPUT FOR THE READ COMMAND MAY POTENTIALLY
!           COME FROM 2 DIFFERENT SOURCES--
!                1) THE TERMINAL ITSELF;
!                2) A FILE;
!           DIFFERENT SYSTEMS ALLOW DIFFERENT COMBINATIONS OF THE ABOVE.
!           ALL SYSTEMS WILL ALLOW INPUT FROM THE TERMINAL ITSELF;
!           MOST SYSTEMS WILL ALLOW INPUT FROM A FILE.
!     NOTE--ICOMCH = THE ALLOWABLE COMMENT CHARACTER
!           ICOMSW = THE COMMENT CHARACTER FLAG/SWITCH (ON/OFF)
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--DECEMBER  1977.
!     UPDATED         --JANUARY   1978.
!     UPDATED         --FEBRUARY  1978.
!     UPDATED         --MAY       1978.
!     UPDATED         --JULY      1978.
!     UPDATED         --NOVEMBER  1978.
!     UPDATED         --NOVEMBER  1980.
!     UPDATED         --JANUARY   1981.
!     UPDATED         --JUNE      1981.
!     UPDATED         --NOVEMBER  1981.
!     UPDATED         --JANUARY   1982.
!     UPDATED         --MARCH     1982.
!     UPDATED         --MAY       1982.
!     UPDATED         --DECEMBER  1985.
!     UPDATED         --SEPTEMBER 1987. (READ MATRIX)
!     UPDATED         --FEBRUARY  1988. DEACT. COL. LIM. IF READ NON-FILE)
!     UPDATED         --JUNE      1988. (CORRECT DOUBLE ENTRY BY READ FUNCT
!     UPDATED         --DECEMBER  1988. CORRECT BOMB ON 2ND   READ PARAMETER
!     UPDATED         --MAY       1989. FIX IRIS PROBLEM--LOOP MAX & CPUMAX
!     UPDATED         --MAY       1990. CHECK FOR COMMENT CHARACTER (UNIX)
!     UPDATED         --MAY       1990. ERROR CHECK FOR FORMATTED READ
!     UPDATED         --JUNE      1990. FIX FORMATTED READ/1 LINE BUG
!     UPDATED         --JULY      1990. UPDATED WRITE/FORMAT STATEMENT
!     UPDATED         --JULY      1990. BUG/TRACE PRINT OF ICOMCH/FL
!     UPDATED         --JULY      1990. COMMENT CHECK BUG FIXED
!     UPDATED         --JULY      1990. RENAME ICOMFL TO ICOMSW
!     UPDATED         --JULY      1993. FIX MATRIX READ (ALAN)
!     UPDATED         --JULY      1993. FIX BOMB IF GOOD READ AFTER
!                                       READ NON-EXISTENT FILE
!     UPDATED         --MARCH     1994. FIX BUG WHERE DELETE AND
!                                       RETAIN WIPED OUT PARAMETERS
!                                       CREATED VIA READ PARAMETER
!     UPDATED         --APRIL     1995. SUPPORT FOR UNFORMATTED READ
!     UPDATED         --SEPTEMBER 1995. ROW LIMITS & BLANK LINES PROBLEM
!     UPDATED         --MARCH     1996. FIX BUG WHERE TERMINAL READ
!                                       NESTED WITHIN A MACRO
!     UPDATED         --APRIL     1996. FOR READ STRING, IGNORE SET
!                                       READ FORMAT
!     UPDATED         --OCTOBER   1997. SUPPORT "SKIP AUTOMATIC",
!                                       READ UNTIL FIND "----"
!     UPDATED         --NOVEMBER  1998. READ MORE THAN 100 VARIABLES
!                                       (MAKE PARAMETER SETTABLE)
!     UPDATED         --DECEMBER  1999. READ ROWID
!     UPDATED         --MARCH     2001. FIX BUGS:
!                                       A) UPDATE LIMIT ON MAX COLUMNS
!                                       B) OFFSET FOR UNFORMATTED READ
!                                       C) MAX FOR ROW LIMITS
!     UPDATED         --JULY      2002. SUPPORT FOR QUOTES ON
!                                       FILE NAMES.
!     UPDATED         --FEBRUARY  2003. UP MAXIMUM NUMBER OF
!                                       CHARACTERS READ FROM ONE
!                                       RECORD OF DATA FILE (MAKE
!                                       SETTABLE TO PARAMETER)
!     UPDATED         --FEBRUARY  2003. AUTOMATICALLY DETERMINE
!                                       NUMBER OF VARIABLES IF NO
!                                       LIST GIVEN.
!     UPDATED         --JUNE      2003. HANDLE HYPHENS INSIDE OF QUOTED
!                                       FILE NAMES CORRECTLY.
!     UPDATED         --JULY      2003. BUG WHEN FILE NAME < 80
!                                       CHARACTERS, BUT COMMAND LINE
!                                       > 80 CHARACTERS
!     UPDATED         --AUGUST    2003. QUOTES ON FILE NAMES
!                                       AUTOMATIC FOR READ
!     UPDATED         --JANUARY   2004. IF AUTOMATICALLY DETERMINE
!                                       VARIABLE LIST, CHECK FIRST
!                                       LINE FOR VARIABLE LIST
!     UPDATED         --JANUARY   2004. SOME RECODING FOR BETTER
!                                       CLARITY
!     UPDATED         --JANUARY   2004. HANDLE CHARACTER DATA
!     UPDATED         --OCTOBER   2004. WHEN READING VARIABLES, IF
!                                       NUMBER OF ITEMS IS GREATER
!                                       THAN NUMBER OF ITEMS READ,
!                                       PAD WITH "MISSING VALUE"
!                                       (BASED ON VALUE OF IREAPD)
!     UPDATED         --OCTOBER   2004. SET READ SUBSET
!                                       <PACK/DISPERSE>  <PACK/DISPERSE>
!     UPDATED         --DECEMBER  2004. IF GUI RUNNING (SET GUI), THEN
!                                       DO NOT ALLOW TERMINAL READ
!     UPDATED         --DECEMBER  2007. > 100 COLUMNS FOR MATRIX
!     UPDATED         --MARCH     2008. ADD:
!                                       READ MATRIX TO VARIABLE FILE.DAT
!                                       Z ROWID COLID
!     UPDATED         --MARCH     2008. ADD:
!                                       READ STACKED VARIABLE FILE.DAT
!                                       Z GROUPID  <VARI-LIST>
!     UPDATED         --MARCH     2008. ADD:
!                                       READ IMAGE TO VARIABLE FILE.DAT
!                                       Z ROWID COLID
!                                       READ IMAGE TO VARIABLE FILE.DAT
!                                       RED BLUE GREEN ROWID COLID
!     UPDATED         --APRIL     2009. ADD "IDATMV" TO DPREAL CALL
!     UPDATED         --APRIL     2009. WHEN READING IMAGES, CHECK
!                                       FOR DATAPLOT DIRECTORIES TO
!                                       MATCH FILE NAME
!     UPDATED         --JULY      2009. ALLOW "Y1 TO Y1" (USEFUL FOR
!                                       MACROS WHERE THE NUMBER OF
!                                       VARIABLES NOT KNOWN IN ADVANCE)
!     UPDATED         --JULY      2014. ADDITIONAL IMAGE TYPES FROM
!                                       GD LIBRARY (BMP, WBMP, WEBP,
!                                       TGA, TIF, XPM)
!     UPDATED         --OCTOBER   2014. SOME TWEAKS FOR CASE WHEN NO
!                                       VARIABLE NAMES GIVEN ON READ
!                                       COMMAND
!     UPDATED         --NOVEMBER  2014. READ FROM SYSTEM CLIPBOARD
!                                       (OS/COMPILER DEPENDENT)
!                                       SUPPORTED FOR READING A LIST OF
!                                       VARIABLES OR FOR READING A
!                                       STRING
!     UPDATED         --JANUARY   2015. IF HAVE READ FROM TERMINAL WHILE
!                                       IN LOOP, READ FROM SAVED LOOP
!                                       COMMANDS RATHER THAN STANDARD
!                                       INPUT (OR MACRO FILE)
!     UPDATED         --MARCH     2015. CALL LIST TO DPINFU
!     UPDATED         --JUNE      2016. CALL LIST TO DPREAL
!     UPDATED         --MARCH     2017. CHECK FOR "," WHEN READING
!                                       VARIABLE NAMES FROM FIRST LINE
!     UPDATED         --JUNE      2018. CORRECT HANDLING OF CHARACTER
!                                       DATA WITH " TO " SYNTAX
!     UPDATED         --JUNE      2018. IF ERROR ENCOUNTERED IN DPREAL,
!                                       STOP PROCESSING
!     UPDATED         --JUNE      2018. SET CONVERT CHARACTER
!                                           CATEGORICAL
!                                       (AUTOMATICALLY CONVERT
!                                       CHARACTER DATA TO NUMERIC
!                                       CATEGORICAL VARIABLE)
!     UPDATED         --SEPTEMBER 2018. ROW READ OPTION
!     UPDATED         --DECEMBER  2018. READ1/READ2/READ3 OPTIONS
!     UPDATED         --APRIL     2019. SET READ ASTERISK IGNORE
!     UPDATED         --JUNE      2019. RAISED MAXIMUM NUMBER OF
!                                       CHARACTER VARIABLES TO 50
!     UPDATED         --JUNE      2019. INITIALIZE IRWLC3 TO 0
!     UPDATED         --JULY      2019. TWEAK SCRATCH SPACE
!     UPDATED         --SEPTEMBER 2019. ALLOWS CHARACTER VARIABLES FROM
!                                       TERMINAL READ
!     UPDATED         --OCTOBER   2019. IF FILE EXTENSION IS ".csv" OR
!                                       ".CSV", AUTOMATICALLY SET READ
!                                       DELIMITER TO ","
!     UPDATED         --FEBRUARY  2020. READ EXCEL OPTION (THIS WILL USE
!                                       PYTHON (Pandas) TO READ THE
!                                       EXCEL FILE TO "dpst1f.dat"),
!                                       READ COMMAND WILL THEN READ
!                                       "dpst1f.dat".
!     UPDATED         --FEBRUARY  2020. FOR "READ CLIPBOARD", CHECK IF
!                                       "CLIPBOARD" ARGUMENT IS ACTUALLY
!                                       A FILE NAME
!     UPDATED         --JUNE      2020. SUPPORT READ FROM CLIPBOARD
!                                       FOR LINUX SYSTEMS (THIS
!                                       WORKS DIFFERENTLY THAN ON
!                                       WINDOWS SYSTEMS)
!     UPDATED         --AUGUST    2020. SOME TWEAKS WHEN READING VARIABLE
!                                       NAMES FROM THE FILE
!     UPDATED         --AUGUST    2023. PBCOPY/PBPASTE FOR MACOS
!                                       CLIPBOARD
!     UPDATED         --AUGUST    2023. FIX BUG WHEN FILE NAME IS QUOTED
!     UPDATED         --JANUARY   2024. FIX BUG WHEN AUTOMATIC VARIABLE
!                                       NAMES USED WITH SKIP AUTOMATIC
!     UPDATED         --FEBRUARY  2025. CALL LIST TO DPREAL
!                                       1. ADD IREALT
!                                       2. ADD XTAG, IOUNI5
!                                          (OPEN dpst5f.dat IF NEEDED)
!     UPDATED         --SEPTEMBER 2025. USE ISO_C_BINDING FOR GD ROUTINES
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
       use, intrinsic   :: iso_c_binding
#ifdef HAVE_GD
       interface
           subroutine gdload(itype,ixpixels,iypixels,file_name,ierr) bind(C,name="gdload")
              use, intrinsic   :: iso_c_binding
              integer(c_int), intent(inout)  :: itype
              integer(c_int), intent(inout)  :: ixpixels
              integer(c_int), intent(inout)  :: iypixels
              integer(c_int), intent(inout)  :: file_name(*)
              integer(c_int), intent(inout)  :: ierr
           end subroutine gdload
       end interface
       interface
           subroutine gdunlo() bind(C,name="gdunlo")
              use, intrinsic   :: iso_c_binding
           end subroutine gdunlo
       end interface
       interface
           subroutine gdpixe(ix,iy,ired,igreen,iblue) bind(C,name="gdpixe")
              use, intrinsic   :: iso_c_binding
              integer(c_int), intent(inout)  :: ix
              integer(c_int), intent(inout)  :: iy
              integer(c_int), intent(inout)  :: ired
              integer(c_int), intent(inout)  :: igreen
              integer(c_int), intent(inout)  :: iblue
           end subroutine gdpixe
       end interface
#endif
      CHARACTER*4 IMACRO
      CHARACTER*12 IMACCS
      CHARACTER*4 ILOOST
      CHARACTER*1 IREPCH
!
      CHARACTER*80 ICREAF
!
      CHARACTER*4 IOSW
      CHARACTER*4 IREARW
      CHARACTER*4 IGRPA2
      CHARACTER*4 ICFLAG
      CHARACTER*4 IBUGS2
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
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
!
      CHARACTER*4 ICASRE
      CHARACTER*4 ICASR2
      CHARACTER*4 ICASR3
      CHARACTER*4 ICASR4
      CHARACTER*4 IOFILE
      CHARACTER*4 IOTERM
      CHARACTER*4 IREAD2
      CHARACTER*4 IFILQ2
      CHARACTER*4 IREANQSV
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
      CHARACTER*9999 ISTR
      CHARACTER*20 IFRMT
      CHARACTER*20 IFRMT2
      CHARACTER*20 IFRMT3
      CHARACTER*4 IOP
      CHARACTER*4 IOPEN
      CHARACTER*4 IFEEDSV
      CHARACTER*8 IACC
!
!CCCC CHARACTER*80 IFILE1
      CHARACTER (LEN=MAXFNC) :: IFILE1
      CHARACTER (LEN=MAXFNC) :: IFILE1SV
      CHARACTER*12 ISTAT1
      CHARACTER*12 IFORM1
      CHARACTER*12 IACCE1
      CHARACTER*12 IPROT1
      CHARACTER*12 ICURS1
      CHARACTER*4 IERRF1
      CHARACTER*4 IENDF1
      CHARACTER*4 IREWI1
!
!CCCC CHARACTER*80 IFILE2
      CHARACTER (LEN=MAXFNC) :: IFILE2
      CHARACTER*12 ISTAT2
      CHARACTER*12 IFORM2
      CHARACTER*12 IACCE2
      CHARACTER*12 IPROT2
      CHARACTER*12 ICURS2
      CHARACTER*4 IERRF2
      CHARACTER*4 IENDF2
      CHARACTER*4 IREWI2
!
!CCCC CHARACTER*80 IFILE3
      CHARACTER (LEN=MAXFNC) :: IFILE3
      CHARACTER*12 ISTAT3
      CHARACTER*12 IFORM3
      CHARACTER*12 IACCE3
      CHARACTER*12 IPROT3
      CHARACTER*12 ICURS3
      CHARACTER*4 IERRF3
      CHARACTER*4 IENDF3
      CHARACTER*4 IREWI3
!
!CCCC CHARACTER*80 IFILE4
      CHARACTER (LEN=MAXFNC) :: IFILE4
      CHARACTER*12 ISTAT4
      CHARACTER*12 IFORM4
      CHARACTER*12 IACCE4
      CHARACTER*12 IPROT4
      CHARACTER*12 ICURS4
      CHARACTER*4 IERRF4
      CHARACTER*4 IENDF4
      CHARACTER*4 IREWI4
!
!CCCC CHARACTER*80 IFILE5
      CHARACTER (LEN=MAXFNC) :: IFILE5
      CHARACTER*12 ISTAT5
      CHARACTER*12 IFORM5
      CHARACTER*12 IACCE5
      CHARACTER*12 IPROT5
      CHARACTER*12 ICURS5
      CHARACTER*4 IERRF5
      CHARACTER*4 IENDF5
      CHARACTER*4 IREWI5
!
      COMMON/FILTMP/IFILE1, ISTAT1, IFORM1, IACCE1, IPROT1, ICURS1,   &
                    IERRF1, IENDF1, IREWI1,                           &
                    IFILE2, ISTAT2, IFORM2, IACCE2, IPROT2, ICURS2,   &
                    IERRF2, IENDF2, IREWI2,                           &
                    IFILE3, ISTAT3, IFORM3, IACCE3, IPROT3, ICURS3,   &
                    IERRF3, IENDF3, IREWI3,                           &
                    IFILE4, ISTAT4, IFORM4, IACCE4, IPROT4, ICURS4,   &
                    IERRF4, IENDF4, IREWI4,                           &
                    IFILE5, ISTAT5, IFORM5, IACCE5, IPROT5, ICURS5,   &
                    IERRF5, IENDF5, IREWI5
!
!CCCC CHARACTER*80 FTEMP
      CHARACTER (LEN=MAXFNC) :: FTEMP
!
      CHARACTER*4 ISTRZ2(8)
!
!CCCC CHARACTER*255 ICANS
      CHARACTER (LEN=MAXSTR) :: ICANS
      CHARACTER*4 IHLEFT
      CHARACTER*4 IHLEF2
      CHARACTER*4 NEWNAM
      CHARACTER*8 IVBASE
      CHARACTER*8 IVBASV
      CHARACTER*8 IVTEMP
      CHARACTER*4 IRTYPE
!CCCC CHARACTER*255 ISTRZZ
      CHARACTER (LEN=MAXSTR) :: ISTRZZ
      CHARACTER*256 IAOUT
      CHARACTER*255 CURDIR
!
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
!
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZI.INC'
      INCLUDE 'DPCOZC.INC'
!
!CCCC NOVEMBER 1998.  DEFINE MAXRDV TO DEFINE MAXIMUM NUMBER OF
!CCCC VARIABLES.
!
!CCCC MARCH 2001.  UP LIMIT (MATRICES IN PARTICULAR CAN HAVE MORE)
!CCCC PARAMETER(MAXRDV=250)
!CCCC PARAMETER(MAXRDV=1000)
      PARAMETER(MAXRDV=2048)
      PARAMETER(MAXCHV=50)
!
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
!
!CCCC THE FOLLOWING LINES ADDED    FEBRUARY  2003.
!
      CHARACTER*4 IVRLST
      CHARACTER*4 IASAVE(MAXRCL)
!
      CHARACTER*4 IECASE(MAXRDV)
      CHARACTER*4 IVLIST(MAXRDV)
      CHARACTER*4 IVLIS2(MAXRDV)
      CHARACTER*4 ICLIST(MAXRDV)
      CHARACTER*4 ICLIS2(MAXRDV)
!
      CHARACTER*4 JVNAM1(MAXRDV)
      CHARACTER*4 JPNAM1(MAXRDV)
      CHARACTER*4 JMNAM1(MAXRDV)
      CHARACTER*4 JFNAM1(MAXRDV)
      CHARACTER*4 JUNAM1(MAXRDV)
      CHARACTER*4 JENAM1(MAXRDV)
!
      CHARACTER*4 JVNAM2(MAXRDV)
      CHARACTER*4 JPNAM2(MAXRDV)
      CHARACTER*4 JMNAM2(MAXRDV)
      CHARACTER*4 JFNAM2(MAXRDV)
      CHARACTER*4 JUNAM2(MAXRDV)
      CHARACTER*4 JENAM2(MAXRDV)
!
      CHARACTER*24 IXC(MAXCHV)
      CHARACTER*24 IXCAT(1000,MAXCHV)
      CHARACTER*4 ISTOR1(MAXRCL)
      CHARACTER*4 ISTOR2(MAXRCL)
      CHARACTER*4 ISTOR3(MAXRCL)
      CHARACTER*4 IB(MAXRCL)
!
      CHARACTER*4 IANSLO(MAXCIL,MAXLIL)
!
      CHARACTER*4 ISSAV1
      CHARACTER*4 ISSAV2
#ifdef HAVE_XCLIP
      CHARACTER (LEN=MAXSTR) :: ISTRIN
      CHARACTER*4 ISSAV3
      CHARACTER*4 ISSAV4
#elif defined(CYGWIN)
      CHARACTER (LEN=MAXSTR) :: ISTRIN
      CHARACTER*4 ISSAV3
      CHARACTER*4 ISSAV4
#elif defined(MACOSX)
      CHARACTER (LEN=MAXSTR) :: ISTRIN
      CHARACTER*4 ISSAV3
      CHARACTER*4 ISSAV4
#endif
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOFO.INC'
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOST.INC'
!CCCC MARCH 2001.  ADD FOLLOWING LINE
      INCLUDE 'DPCOMC.INC'
!
      DIMENSION XSCRT(3*MAXOBW)
      DIMENSION X0(MAXRDV)
      DIMENSION XTAG(MAXRDV)
!
      EQUIVALENCE (GARBAG(IGARB1),X0(1))
      EQUIVALENCE (GARBAG(IGARB1+MAXRDV),XTAG(1))
      EQUIVALENCE (GARBAG(IGARB2),X0CAT(1))
      EQUIVALENCE (GARBAG(IGARB3),XSCRT(1))
!
      EQUIVALENCE (IGARBG(IIGAR1),ITYPE(1))
      EQUIVALENCE (IGARBG(IIGAR1+1000),NIV(1))
      EQUIVALENCE (IGARBG(IIGAR1+3000),IEN(1))
      EQUIVALENCE (IGARBG(IIGAR1+5000),IECOL2(1))
      EQUIVALENCE (IGARBG(IIGAR1+7000),IFSTA2(1))
      EQUIVALENCE (IGARBG(IIGAR1+9000),IFSTO2(1))
      EQUIVALENCE (IGARBG(IIGAR1+11000),IADE(1))
      EQUIVALENCE (IGARBG(IIGAR1+13000),IECOLC(1))
      EQUIVALENCE (IGARBG(IIGAR1+15000),IENC(1))
!
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
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
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
      IREANQSV=IREANQ
!
      ICASR3='0'
      IF(ICOM2.EQ.'1   ')ICASR3='1'
      IF(ICOM2.EQ.'2   ')ICASR3='2'
      IF(ICOM2.EQ.'3   ')ICASR3='3'
!
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
!
!     FEBRUARY 2025: OPEN dpst5f.dat IF NEEDED
!
      IF(IREAPM.EQ.'ON' .OR. IREALT.EQ.'ON')THEN
        IOP='OPEN'
        IFLG11=0
        IFLG21=0
        IFLG31=0
        IFLAG4=0
        IFLAG5=1
        CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,   &
                    IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,       &
                    IBUGS2,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!CCCC FEBRUARY 2003: ADD FOLLOWING LINE.
!CCCC IF NO VARIABLE LIST GIVEN, THEN TWO CASES:
!CCCC    1) IF SKIP AUTOMATIC ON, THEN READ PREVIOUS LINE TO
!CCCC       DETERMINE VARIABLE LIST.
!CCCC    2) IF SKIP AUTOMATIC OFF, THEN READ FIRST LINE TO
!CCCC       DETERMINE NUMBER OF VARIABLES.  NAME THEM X1, X2, ETC.
!
      IVRLST='YES'
      DO 15 I=1,MAXRDV
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
      DO 13 I=1,MAXCHV
        IXC(I)=' '
        ICLIST(I)=' '
        ICLIS2(I)=' '
        IECOLC(I)=0
        IENC(I)=0
        DO 14 J=1,1000
          IXCAT(J,I)=' '
   14   CONTINUE
        IXCATN(I)=0
        X0CAT(I)=0.0
   13 CONTINUE
      IGRPA2=IGRPAU
!
!CCCC THE FOLLOWING LINE WAS INSERTED MAY 1989
!CCCC MARCH 2001.  SET VALUE TO MAX INTEGER
!CCCC IBILLI=10**9
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
!
!               ***************************
!               **  TREAT THE READ CASE  **
!               ***************************
!
!CCCC NOVEMBER 1998.  DEFINE MAXRDV TO DEFINE MAXIMUM NUMBER OF
!CCCC VARIABLES.
!
      MAXV2=MAXRDV
      MAXP2=MAXRDV
      MAXM2=MAXRDV
      MAXF2=MAXRDV
      MAXU2=MAXRDV
      MAXE2=MAXRDV
!
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1)
    1   FORMAT('***** AT THE BEGINNING OF DPREAD--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2)IFROW1,AFROW2,IFCOL1,IFCOL2,NUMRCM,MAXCOM
    2   FORMAT('IFROW1,AFROW2,IFCOL1,IFCOL2,NUMRCM,MAXCOM = ',   &
               I8,2X,E15.7,4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4)IRD,IRD2,ISKIP,IBUGS2,IBUGQ,IOSW
    4   FORMAT('IRD,IRD2,ISKIP,IBUGS2,IBUGQ,IOSW = ',3I8,2X,2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6)IMACRO,IMACNU,IMACCS
    6   FORMAT('IMACRO,IMACNU,IMACCS = ',A4,I8,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7)IBUGS2,ISUBRO,IERROR,ICASR3,IWIDTH
    7   FORMAT('IBUGS2,ISUBRO,IERROR,ICASR3,IWIDTH = ',4(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,8)(IANSLC(I),I=1,MIN(100,IWIDTH))
    8     FORMAT('(IANSLC(I),I=1,IWIDTH) = ',100A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        WRITE(ICOUT,22)IREANA(1:80)
   22   FORMAT('IREANA = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,23)IREANU,IREAST,IREAFO,IREAAC,IREAFO,IREACS
   23   FORMAT('IREANU,IREAST,IREAFO,IREAAC,IREAFO,IREACS = ',   &
               I8,5(1X,A12))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,32)NUMNAM,N2,MAXN2,NCREAF
   32   FORMAT('NUMNAM,N2,MAXN2,NCREAF = ',4I8)
        CALL DPWRST('XXX','BUG ')
        IF(NCREAF.GE.1)THEN
          WRITE(ICOUT,35)(ICREAF(I:I),I=1,NCREAF)
   35     FORMAT('(ICREAF(I:I),I=1,NCREAF) = ',80A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        WRITE(ICOUT,37)IREARW,ICOMCH,ICOMSW
   37   FORMAT('IREARW,ICOMCH,ICOMSW = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *******************************************************
!               **  STEP 1--                                         **
!               **  CHECK FOR THE PROPER NUMBER OF INPUT ARGUMENTS.  **
!               *******************************************************
!
      ISTEPN='1'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LT.1)THEN
        IERROR='YES'
        GO TO 8800
      ENDIF
!
!               *****************************************
!               **  STEP 1B--                          **
!               **  DETERMINE THE TYPE OF READ CASE--  **
!               **     1) VARIABLE                     **
!               **     2) PARAMETER                    **
!               **     3) FUNCTION (= STRING)          **
!               **     4) MATRIX                       **
!               **     5) MATRIX TO VARIABLE           **
!               **     6) STACKED VARIABLE             **
!               **     7) IMAGE                        **
!               **     8) IMAGE TO VARIABLE            **
!               **     9) CLIPBOARD                    **
!               **    10) STRING CLIPBOARD             **
!               **    11) ROW READ                     **
!               **    12) EXCEL                        **
!               *****************************************
!
#ifdef HAVE_XCLIP
   60 CONTINUE
#elif defined(CYGWIN)
   60 CONTINUE
#elif defined(MACOSX)
   60 CONTINUE
#endif
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
  103     FORMAT('       MUST BE EITHER   STRING   OR   ',   &
                 'NUMERIC   .')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,105)IHARG(1)
  105     FORMAT('       THE FIRST ARGUMENT IS ',A4)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
        GO TO 150
      ELSEIF(IHARG(1).EQ.'PARA'.AND.IHARG2(1).EQ.'METE')THEN
        ICASRE='PARA'
      ELSEIF((IHARG(1).EQ.'FUNC'.AND.IHARG2(1).EQ.'TION' .AND.   &
             IHARG(2).EQ.'CLIP' .AND. IHARG2(2).EQ.'BOAR') .OR.   &
             (IHARG(1).EQ.'STRI'.AND.IHARG2(1).EQ.'NG  ' .AND.   &
             IHARG(2).EQ.'CLIP' .AND. IHARG2(2).EQ.'BOAR'))THEN
        IF(IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU')THEN
#ifdef HAVE_XCLIP
!
!       TO READ STRING/FUNCTION, ISSUE THE COMMAND:
!
!          xclip -o -selection <clipboard/primary/secondary>  > dpst5f.dat
!
          ISTRIN=' '
          IF(IX11SE.EQ.'CLIP')THEN
            ISTRIN='xclip -o -selection clipboard'
            NCSTR=29
          ELSEIF(IX11SE.EQ.'PRIM')THEN
            ISTRIN='xclip -o -selection primary'
            NCSTR=27
          ELSE
            ISTRIN='xclip -o -selection secondary'
            NCSTR=29
          ENDIF
!
          NCSTR=NCSTR+1
          ISTRIN(NCSTR:NCSTR+12)=' > dpst5f.dat'
          NCSTR=NCSTR+12
!
          ISSAV1=ISYSPE
          ISSAV2=ISYSHI
          ISSAV3=ICLEWT
          ISSAV4=ILINSY
          ISYSPE='ON'
          ICLEWT='OFF'
          ILINSY='COMM'
          CALL DPSYS2(ISTRIN,NCSTR,ISUBRO,IERROR)
          ISYSPE=ISSAV1
          ISYSHI=ISSAV2
          ICLEWT=ISSAV3
          ILINSY=ISSAV4
!
          IHARG(2)='DPST'
          IHARG2(2)='5F.D'
          DO 65 II=1,IWIDTH-10
            IP1=II+1
            IP2=II+2
            IP3=II+3
            IP4=II+4
            IP5=II+5
            IP6=II+6
            IP7=II+7
            IP8=II+8
            IP9=II+9
            IP10=II+10
            IF(IANS(II)(1:1).EQ.'C'  .AND. IANS(IP1)(1:1).EQ.'L' .AND.   &
               IANS(IP2)(1:1).EQ.'I' .AND. IANS(IP3)(1:1).EQ.'P' .AND.   &
               IANS(IP4)(1:1).EQ.'B' .AND. IANS(IP5)(1:1).EQ.'O' .AND.   &
               IANS(IP6)(1:1).EQ.'A' .AND. IANS(IP7)(1:1).EQ.'R' .AND.   &
               IANS(IP8)(1:1).EQ.'D' .AND. IANS(IP9)(1:1).EQ.' ')THEN
              DO 61 JJ=IWIDTH,IP10,-1
                IANSLC(JJ+1)=IANSLC(JJ)
                IANS(JJ+1)=IANS(JJ)
   61         CONTINUE
              IWIDTH=IWIDTH+1
              IANSLC(II:II)='d'
              IANSLC(IP1:IP1)='p'
              IANSLC(IP2:IP2)='s'
              IANSLC(IP3:IP3)='t'
              IANSLC(IP4:IP4)='5'
              IANSLC(IP5:IP5)='f'
              IANSLC(IP6:IP6)='.'
              IANSLC(IP7:IP7)='d'
              IANSLC(IP8:IP8)='a'
              IANSLC(IP9:IP9)='t'
              IANSLC(IP10:IP10)=' '
              IANS(II:II)='D'
              IANS(IP1:IP1)='P'
              IANS(IP2:IP2)='S'
              IANS(IP3:IP3)='T'
              IANS(IP4:IP4)='5'
              IANS(IP5:IP5)='F'
              IANS(IP6:IP6)='.'
              IANS(IP7:IP7)='D'
              IANS(IP8:IP8)='A'
              IANS(IP9:IP9)='T'
              IANS(IP10:IP10)=' '
              GO TO 60
            ENDIF
   65     CONTINUE
!
#elif defined(MACOSX)
!
!       TO READ STRING/FUNCTION, ISSUE THE COMMAND:
!
!          pbpaste > dpst5f.dat
!
          ISTRIN=' '
          ISTRIN='pbpaste > dpst5f.dat'
          NCSTR=20
!
          ISSAV1=ISYSPE
          ISSAV2=ISYSHI
          ISSAV3=ICLEWT
          ISSAV4=ILINSY
          ISYSPE='ON'
          ICLEWT='OFF'
          ILINSY='COMM'
          CALL DPSYS2(ISTRIN,NCSTR,ISUBRO,IERROR)
          ISYSPE=ISSAV1
          ISYSHI=ISSAV2
          ICLEWT=ISSAV3
          ILINSY=ISSAV4
!
          IHARG(2)='DPST'
          IHARG2(2)='5F.D'
          DO 65 II=1,IWIDTH-10
            IP1=II+1
            IP2=II+2
            IP3=II+3
            IP4=II+4
            IP5=II+5
            IP6=II+6
            IP7=II+7
            IP8=II+8
            IP9=II+9
            IP10=II+10
            IF(IANS(II)(1:1).EQ.'C'  .AND. IANS(IP1)(1:1).EQ.'L' .AND.   &
               IANS(IP2)(1:1).EQ.'I' .AND. IANS(IP3)(1:1).EQ.'P' .AND.   &
               IANS(IP4)(1:1).EQ.'B' .AND. IANS(IP5)(1:1).EQ.'O' .AND.   &
               IANS(IP6)(1:1).EQ.'A' .AND. IANS(IP7)(1:1).EQ.'R' .AND.   &
               IANS(IP8)(1:1).EQ.'D' .AND. IANS(IP9)(1:1).EQ.' ')THEN
              DO 61 JJ=IWIDTH,IP10,-1
                IANSLC(JJ+1)=IANSLC(JJ)
                IANS(JJ+1)=IANS(JJ)
   61         CONTINUE
              IWIDTH=IWIDTH+1
              IANSLC(II:II)='d'
              IANSLC(IP1:IP1)='p'
              IANSLC(IP2:IP2)='s'
              IANSLC(IP3:IP3)='t'
              IANSLC(IP4:IP4)='5'
              IANSLC(IP5:IP5)='f'
              IANSLC(IP6:IP6)='.'
              IANSLC(IP7:IP7)='d'
              IANSLC(IP8:IP8)='a'
              IANSLC(IP9:IP9)='t'
              IANSLC(IP10:IP10)=' '
              IANS(II:II)='D'
              IANS(IP1:IP1)='P'
              IANS(IP2:IP2)='S'
              IANS(IP3:IP3)='T'
              IANS(IP4:IP4)='5'
              IANS(IP5:IP5)='F'
              IANS(IP6:IP6)='.'
              IANS(IP7:IP7)='D'
              IANS(IP8:IP8)='A'
              IANS(IP9:IP9)='T'
              IANS(IP10:IP10)=' '
              GO TO 60
            ENDIF
   65     CONTINUE
!
#elif defined(CYGWIN)
!
!       TO READ STRING/FUNCTION, ISSUE THE COMMAND:
!
!          cat /dev/clipboard > dpst5f.dat
!
          ISTRIN=' '
          ISTRIN='cat /dev/clipboard > dpst5f.dat'
          NCSTR=31
!
          ISSAV1=ISYSPE
          ISSAV2=ISYSHI
          ISSAV3=ICLEWT
          ISSAV4=ILINSY
          ISYSPE='ON'
          ICLEWT='OFF'
          ILINSY='COMM'
          CALL DPSYS2(ISTRIN,NCSTR,ISUBRO,IERROR)
          ISYSPE=ISSAV1
          ISYSHI=ISSAV2
          ICLEWT=ISSAV3
          ILINSY=ISSAV4
!
          IHARG(2)='DPST'
          IHARG2(2)='5F.D'
          DO 65 II=1,IWIDTH-10
            IP1=II+1
            IP2=II+2
            IP3=II+3
            IP4=II+4
            IP5=II+5
            IP6=II+6
            IP7=II+7
            IP8=II+8
            IP9=II+9
            IP10=II+10
            IF(IANS(II)(1:1).EQ.'C'  .AND. IANS(IP1)(1:1).EQ.'L' .AND.   &
               IANS(IP2)(1:1).EQ.'I' .AND. IANS(IP3)(1:1).EQ.'P' .AND.   &
               IANS(IP4)(1:1).EQ.'B' .AND. IANS(IP5)(1:1).EQ.'O' .AND.   &
               IANS(IP6)(1:1).EQ.'A' .AND. IANS(IP7)(1:1).EQ.'R' .AND.   &
               IANS(IP8)(1:1).EQ.'D' .AND. IANS(IP9)(1:1).EQ.' ')THEN
              DO 61 JJ=IWIDTH,IP10,-1
                IANSLC(JJ+1)=IANSLC(JJ)
                IANS(JJ+1)=IANS(JJ)
   61         CONTINUE
              IWIDTH=IWIDTH+1
              IANSLC(II:II)='d'
              IANSLC(IP1:IP1)='p'
              IANSLC(IP2:IP2)='s'
              IANSLC(IP3:IP3)='t'
              IANSLC(IP4:IP4)='5'
              IANSLC(IP5:IP5)='f'
              IANSLC(IP6:IP6)='.'
              IANSLC(IP7:IP7)='d'
              IANSLC(IP8:IP8)='a'
              IANSLC(IP9:IP9)='t'
              IANSLC(IP10:IP10)=' '
              IANS(II:II)='D'
              IANS(IP1:IP1)='P'
              IANS(IP2:IP2)='S'
              IANS(IP3:IP3)='T'
              IANS(IP4:IP4)='5'
              IANS(IP5:IP5)='F'
              IANS(IP6:IP6)='.'
              IANS(IP7:IP7)='D'
              IANS(IP8:IP8)='A'
              IANS(IP9:IP9)='T'
              IANS(IP10:IP10)=' '
              GO TO 60
            ENDIF
   65     CONTINUE
!
#else
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,71)
   71     FORMAT('***** ERROR IN READ COMMAND--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,73)
   73     FORMAT('      THE xclip COMMAND IS NOT AVAILABLE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,75)
   75     FORMAT('      THE READ FROM CLIPBOARD WILL NOT BE ',   &
                 'PERFORMED.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
#endif
        ELSE
          ICASRE='CFUN'
        ENDIF
      ELSEIF(IHARG(1).EQ.'FUNC'.AND.IHARG2(1).EQ.'TION')THEN
        ICASRE='FUNC'
      ELSEIF(IHARG(1).EQ.'STRI'.AND.IHARG2(1).EQ.'NG')THEN
        ICASRE='FUNC'
      ELSEIF(IHARG(1).EQ.'CLIP'.AND.IHARG2(1).EQ.'BOAR')THEN
!
!       CHECK IF ARGUMENT IS A FILE NAME STARTING WITH
!       "CLIPBOARD.
!
        IWORD=2
        IOFILE='NO'
        CALL DPFILE(IANSLC,IWIDTH,IWORD,IOFILE,IBUGS2,ISUBRO,IERROR)
        IF(IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU')THEN
#ifdef HAVE_XCLIP
!
!       ISSUE THE COMMAND
!
!          xclip -o -selection <clipboard/primary/secondary>  > dpst5f.dat
!
          ISTRIN=' '
          IF(IX11SE.EQ.'CLIP')THEN
            ISTRIN='xclip -o -selection clipboard'
            NCSTR=29
          ELSEIF(IX11SE.EQ.'PRIM')THEN
            ISTRIN='xclip -o -selection primary'
            NCSTR=27
          ELSE
            ISTRIN='xclip -o -selection secondary'
            NCSTR=29
          ENDIF
!
          NCSTR=NCSTR+1
          ISTRIN(NCSTR:NCSTR+12)=' < dpst5f.dat'
          NCSTR=NCSTR+12
!
          ISSAV1=ISYSPE
          ISSAV2=ISYSHI
          ISSAV3=ICLEWT
          ISSAV4=ILINSY
          ISYSPE='ON'
          ICLEWT='OFF'
          ILINSY='COMM'
          CALL DPSYS2(ISTRIN,NCSTR,ISUBRO,IERROR)
          ISYSPE=ISSAV1
          ISYSHI=ISSAV2
          ICLEWT=ISSAV3
          ILINSY=ISSAV4
!
          IHARG(1)='DPST'
          IHARG2(1)='5F.D'
          DO 67 II=1,IWIDTH-10
            IP1=II+1
            IP2=II+2
            IP3=II+3
            IP4=II+4
            IP5=II+5
            IP6=II+6
            IP7=II+7
            IP8=II+8
            IP9=II+9
            IP10=II+10
            IF(IANS(II)(1:1).EQ.'C'  .AND. IANS(IP1)(1:1).EQ.'L' .AND.   &
               IANS(IP2)(1:1).EQ.'I' .AND. IANS(IP3)(1:1).EQ.'P' .AND.   &
               IANS(IP4)(1:1).EQ.'B' .AND. IANS(IP5)(1:1).EQ.'O' .AND.   &
               IANS(IP6)(1:1).EQ.'A' .AND. IANS(IP7)(1:1).EQ.'R' .AND.   &
               IANS(IP8)(1:1).EQ.'D' .AND. IANS(IP9)(1:1).EQ.' ')THEN
              DO 68 JJ=IWIDTH,IP10,-1
                IANSLC(JJ+1)=IANSLC(JJ)
                IANS(JJ+1)=IANS(JJ)
   68         CONTINUE
              IWIDTH=IWIDTH+1
              IANSLC(II:II)='d'
              IANSLC(IP1:IP1)='p'
              IANSLC(IP2:IP2)='s'
              IANSLC(IP3:IP3)='t'
              IANSLC(IP4:IP4)='5'
              IANSLC(IP5:IP5)='f'
              IANSLC(IP6:IP6)='.'
              IANSLC(IP7:IP7)='d'
              IANSLC(IP8:IP8)='a'
              IANSLC(IP9:IP9)='t'
              IANSLC(IP10:IP10)=' '
              IANS(II:II)='D'
              IANS(IP1:IP1)='P'
              IANS(IP2:IP2)='S'
              IANS(IP3:IP3)='T'
              IANS(IP4:IP4)='5'
              IANS(IP5:IP5)='F'
              IANS(IP6:IP6)='.'
              IANS(IP7:IP7)='D'
              IANS(IP8:IP8)='A'
              IANS(IP9:IP9)='T'
              IANS(IP10:IP10)=' '
              GO TO 60
            ENDIF
   67     CONTINUE
!
#elif defined(CYGWIN)
!
!       FOR CYGWIN, ISSUE THE COMMAND:
!
!          cat /dev/clipboard  > dpst5f.dat
!
          ISTRIN=' '
          ISTRIN='cat /dev/clipboard > dpst5f.dat'
          NCSTR=21
!
          ISSAV1=ISYSPE
          ISSAV2=ISYSHI
          ISSAV3=ICLEWT
          ISSAV4=ILINSY
          ISYSPE='ON'
          ICLEWT='OFF'
          ILINSY='COMM'
          CALL DPSYS2(ISTRIN,NCSTR,ISUBRO,IERROR)
          ISYSPE=ISSAV1
          ISYSHI=ISSAV2
          ICLEWT=ISSAV3
          ILINSY=ISSAV4
!
          IHARG(1)='DPST'
          IHARG2(1)='5F.D'
          DO 67 II=1,IWIDTH-10
            IP1=II+1
            IP2=II+2
            IP3=II+3
            IP4=II+4
            IP5=II+5
            IP6=II+6
            IP7=II+7
            IP8=II+8
            IP9=II+9
            IP10=II+10
            IF(IANS(II)(1:1).EQ.'C'  .AND. IANS(IP1)(1:1).EQ.'L' .AND.   &
               IANS(IP2)(1:1).EQ.'I' .AND. IANS(IP3)(1:1).EQ.'P' .AND.   &
               IANS(IP4)(1:1).EQ.'B' .AND. IANS(IP5)(1:1).EQ.'O' .AND.   &
               IANS(IP6)(1:1).EQ.'A' .AND. IANS(IP7)(1:1).EQ.'R' .AND.   &
               IANS(IP8)(1:1).EQ.'D' .AND. IANS(IP9)(1:1).EQ.' ')THEN
              DO 68 JJ=IWIDTH,IP10,-1
                IANSLC(JJ+1)=IANSLC(JJ)
                IANS(JJ+1)=IANS(JJ)
   68         CONTINUE
              IWIDTH=IWIDTH+1
              IANSLC(II:II)='d'
              IANSLC(IP1:IP1)='p'
              IANSLC(IP2:IP2)='s'
              IANSLC(IP3:IP3)='t'
              IANSLC(IP4:IP4)='5'
              IANSLC(IP5:IP5)='f'
              IANSLC(IP6:IP6)='.'
              IANSLC(IP7:IP7)='d'
              IANSLC(IP8:IP8)='a'
              IANSLC(IP9:IP9)='t'
              IANSLC(IP10:IP10)=' '
              IANS(II:II)='D'
              IANS(IP1:IP1)='P'
              IANS(IP2:IP2)='S'
              IANS(IP3:IP3)='T'
              IANS(IP4:IP4)='5'
              IANS(IP5:IP5)='F'
              IANS(IP6:IP6)='.'
              IANS(IP7:IP7)='D'
              IANS(IP8:IP8)='A'
              IANS(IP9:IP9)='T'
              IANS(IP10:IP10)=' '
              GO TO 60
            ENDIF
   67     CONTINUE
!
#elif defined(MACOSX)
!
!       FOR MacOS, ISSUE THE COMMAND:
!
!          pbpaste > dpst5f.dat
!
          ISTRIN=' '
          ISTRIN='pbpaste > dpst5f.dat'
          NCSTR=20
!
          ISSAV1=ISYSPE
          ISSAV2=ISYSHI
          ISSAV3=ICLEWT
          ISSAV4=ILINSY
          ISYSPE='ON'
          ICLEWT='OFF'
          ILINSY='COMM'
          CALL DPSYS2(ISTRIN,NCSTR,ISUBRO,IERROR)
          ISYSPE=ISSAV1
          ISYSHI=ISSAV2
          ICLEWT=ISSAV3
          ILINSY=ISSAV4
!
          IHARG(1)='DPST'
          IHARG2(1)='5F.D'
          DO 67 II=1,IWIDTH-10
            IP1=II+1
            IP2=II+2
            IP3=II+3
            IP4=II+4
            IP5=II+5
            IP6=II+6
            IP7=II+7
            IP8=II+8
            IP9=II+9
            IP10=II+10
            IF(IANS(II)(1:1).EQ.'C'  .AND. IANS(IP1)(1:1).EQ.'L' .AND.   &
               IANS(IP2)(1:1).EQ.'I' .AND. IANS(IP3)(1:1).EQ.'P' .AND.   &
               IANS(IP4)(1:1).EQ.'B' .AND. IANS(IP5)(1:1).EQ.'O' .AND.   &
               IANS(IP6)(1:1).EQ.'A' .AND. IANS(IP7)(1:1).EQ.'R' .AND.   &
               IANS(IP8)(1:1).EQ.'D' .AND. IANS(IP9)(1:1).EQ.' ')THEN
              DO 68 JJ=IWIDTH,IP10,-1
                IANSLC(JJ+1)=IANSLC(JJ)
                IANS(JJ+1)=IANS(JJ)
   68         CONTINUE
              IWIDTH=IWIDTH+1
              IANSLC(II:II)='d'
              IANSLC(IP1:IP1)='p'
              IANSLC(IP2:IP2)='s'
              IANSLC(IP3:IP3)='t'
              IANSLC(IP4:IP4)='5'
              IANSLC(IP5:IP5)='f'
              IANSLC(IP6:IP6)='.'
              IANSLC(IP7:IP7)='d'
              IANSLC(IP8:IP8)='a'
              IANSLC(IP9:IP9)='t'
              IANSLC(IP10:IP10)=' '
              IANS(II:II)='D'
              IANS(IP1:IP1)='P'
              IANS(IP2:IP2)='S'
              IANS(IP3:IP3)='T'
              IANS(IP4:IP4)='5'
              IANS(IP5:IP5)='F'
              IANS(IP6:IP6)='.'
              IANS(IP7:IP7)='D'
              IANS(IP8:IP8)='A'
              IANS(IP9:IP9)='T'
              IANS(IP10:IP10)=' '
              GO TO 60
            ENDIF
   67     CONTINUE
!
#else
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,71)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,73)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,75)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
#endif
        ELSE
          IF(IOFILE.EQ.'NO')ICASRE='CLIP'
        ENDIF
      ELSEIF(IHARG(1).EQ.'ROW '.AND.IHARG2(1).EQ.'    ' .AND.   &
             IHARG(2).NE.'LABE')THEN
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
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGS2,IERROR)
      ENDIF
!
      IF(IHARG(1).EQ.'PNG' .AND. IHARG(2).EQ.'IMAG')THEN
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGS2,IERROR)
        IMAGTY=2
      ELSEIF((IHARG(1).EQ.'JPG' .OR. IHARG(1).EQ.'JPEG') .AND.   &
             IHARG(2).EQ.'IMAG')THEN
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGS2,IERROR)
        IMAGTY=1
      ELSEIF((IHARG(1).EQ.'GIF' .OR. IHARG(1).EQ.'GIFF') .AND.   &
             IHARG(2).EQ.'IMAG')THEN
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGS2,IERROR)
        IMAGTY=3
      ELSEIF(IHARG(1).EQ.'BMP' .AND. IHARG(2).EQ.'IMAG')THEN
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGS2,IERROR)
        IMAGTY=4
      ELSEIF(IHARG(1).EQ.'WBMP' .AND. IHARG(2).EQ.'IMAG')THEN
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGS2,IERROR)
        IMAGTY=5
      ELSEIF(IHARG(1).EQ.'WEBP' .AND. IHARG(2).EQ.'IMAG')THEN
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGS2,IERROR)
        IMAGTY=6
      ELSEIF(IHARG(1).EQ.'TGA' .AND. IHARG(2).EQ.'IMAG')THEN
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGS2,IERROR)
        IMAGTY=7
      ELSEIF((IHARG(1).EQ.'TIF' .OR. IHARG(1).EQ.'TIFF') .AND.   &
             IHARG(2).EQ.'IMAG')THEN
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGS2,IERROR)
        IMAGTY=8
      ELSEIF(IHARG(1).EQ.'XPM' .AND. IHARG(2).EQ.'IMAG')THEN
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGS2,IERROR)
        IMAGTY=9
      ENDIF
!
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
!
      IF(IHARG(1).EQ.'ROW '.AND.IHARG2(1).EQ.' '.AND.   &
         IHARG(2).EQ.'LABE'.AND.IHARG2(2).EQ.'LS')ICASRE='ROWI'
      IF(IHARG(1).EQ.'ROW '.AND.IHARG2(1).EQ.' '.AND.   &
         IHARG(2).EQ.'LABE'.AND.IHARG2(2).EQ.'L ')ICASRE='ROWI'
!
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
!
  150 CONTINUE
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
        WRITE(ICOUT,155)ICASRE
  155   FORMAT('ICASRE = ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ******************************************************
!               **  STEP 2A--                                       **
!               **  DETERMINE THE TYPE OF READ CASE--               **
!               **       1) FROM TERMINAL;                          **
!               **       2) FROM FILE;                              **
!               **  NOTE--IOTERM  WILL = 'YES' ONLY IN EXPLICIT     **
!               **        TERMINAL CASE                             **
!               **        (THAT IS, ONLY WHEN INPUT IOSW            **
!               **                             = 'TERM')            **
!               **  NOTE--IOFILE  WILL = 'YES' ONLY IN FILE CASE.   **
!               **  NOTE--IMAGE READ ONLY SUPPORTED FOR FILE CASE.  **
!               ******************************************************
!
      ISTEPN='2A'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
          GO TO 202
        ENDIF
      ELSEIF(ICASRE.EQ.'CLIP' .OR. ICASRE.EQ.'CFUN')THEN
        IOFILE='NO'
        GO TO 202
      ENDIF
!
      CALL DPFILE(IANSLC,IWIDTH,IWORD,   &
                  IOFILE,IBUGS2,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      IF(IEXCEL.EQ.'ON' .AND. IOFILE.EQ.'NO')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,191)
  191   FORMAT('      THE READ EXCEL COMMAND REQUIRES A FILE NAME ',   &
               'TO BE SPECIFIED.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
  202 CONTINUE
!
      IOTERM='NO'
      IF(IOFILE.EQ.'NO'.AND.IOSW.EQ.'TERM')IOTERM='YES'
!
!     JANUARY 2015.  CHECK IF "LOOP" IS ACTIVE WHEN READING
!                    FROM TERMINAL.
!
      IF(IOFILE.EQ.'NO' .AND. ILOOST.EQ.'EXEC')THEN
        IF(IOSW.NE.'TERM')IOTERM='LOOP'
      ENDIF
!
!CCCC DECEMBER 2004.  IF GUI RUNNING, DO NOT ALLOW TERMINAL READ.
!
      IF(ICASRE.EQ.'LINE' .AND. IOFILE.EQ.'NO  ')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,213)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,205)
  205   FORMAT('      ARE NOT PERMITTED FOR THE READ1/READ2/READ3 ',   &
               'CASES.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(IOFILE.EQ.'NO' .AND. IGUIFL.EQ.'ON')THEN
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
      IF(IOFILE.EQ.'NO' .AND.   &
         (ICASRE.EQ.'IMAZ' .OR. ICASRE.EQ.'IMAG'))THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,241)
  241   FORMAT('      AN IMAGE READ REQUIRES THAT A FILE NAME BE ',   &
               'SPECIFIED.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,242)
  242   FORMAT('      NO FILE NAME WAS GIVEN ON THE READ COMMAND.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!
!               *************************************
!               **  STEP 2B--                      **
!               **  IF HAVE THE FILE INPUT CASE--  **
!               **  COPY OVER VARIABLES            **
!               *************************************
!
      ISTEPN='2B'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IOFILE.EQ.'YES')THEN
!
        IOUNIT=IREANU
        IFILE=IREANA
        ISTAT=IREAST
        IFORM=IREAFO
        IACCES=IREAAC
        IPROT=IREAPR
        ICURST=IREACS
!
        ISUBN0='READ'
        IERRFI='NO'
!
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
          WRITE(ICOUT,1183)IOUNIT,ISUBN0,IERRFI
 1183     FORMAT('IOUNIT,ISUBN0,IERRFI = ',I8,A4,2X,A4)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1184)IFILE(1:255)
 1184     FORMAT('IFILE = ',A255)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1185)ISTAT,IFORM,IACCES,IPROT,ICURST
 1185     FORMAT('ISTAT,IFORM,IACCES,IPROT,ICURST = ',4(A12,2X),A12)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
      ENDIF
!
!               ***********************************************
!               **  STEP 2C--                                **
!               **  IF HAVE THE FILE INPUT CASE--            **
!               **  CHECK TO SEE IF THE READ FILE MAY EXIST  **
!               ***********************************************
!
      ISTEPN='2C'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IOFILE.EQ.'YES')THEN
!
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
 1218     FORMAT('      ALL READING MUST BE DONE DIRECTLY FROM ',   &
                 'THE TERMINAL')
          CALL DPWRST('XXX','BUG ')
          GO TO 9000
        ENDIF
      ENDIF
!
!               *************************************
!               **  STEP 2D--                      **
!               **  IF HAVE THE FILE INPUT CASE--  **
!               **  EXTRACT THE FILE NAME          **
!               *************************************
!
      ISTEPN='2D'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')   &
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
        CALL DPEXWO(ICANS,ISTART,ISTOP,IWORD,   &
                    ICOL1,ICOL2,IFILE,NCFILE,   &
                    IBUGS2,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
          WRITE(ICOUT,1312)NCFILE,IFILE
 1312     FORMAT('AFTER DPEXWO: NCFILE,IFILE = ',I5,2X,A256)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        IF(IEXCEL.EQ.'ON' .AND. IEXCME.EQ.'POWE')THEN
!
!         READ EXCEL FILE USING POWER SHELL
!
!         STEP 1: OPEN "read_excel.ps1"
!
!         Power Shell method currently only supported for Windows
!
          IF(IHOST1.NE.'IBM-')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,211)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1322)
 1322       FORMAT('      Power Shell Method currently only supported')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1324)
 1324       FORMAT('      for Windows platforms.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            GO TO 9000
          ENDIF
!
          IFILE1SV=IFILE1
          IOUNI1=IST1NU
          IFILE1=' '
          MAXNCHAR=255
          IFEEDSV=IFEEDB
          IFEEDB='OFF'
          CALL DPPWD(CURDIR,MAXNCHAR,IPOS,IBUGS2,ISUBRO,IFOUND,IERROR)
          IFEEDB=IFEEDSV
          IF(IERROR.EQ.'YES')THEN
            IFILE1=IFILE1SV
            GO TO 9000
          ENDIF
          IFILE1(1:IPOS)=CURDIR(1:IPOS)
          IPOS=IPOS+1
          IF(IHOST1.EQ.'IBM-')THEN
            IFILE1(IPOS:IPOS)='\'
          ELSE
            IFILE1(IPOS:IPOS)='/'
          ENDIF
          NCPATHZ=IPOS
          IFILE1(IPOS+1:IPOS+14)='read_excel.ps1'
          NCFILEZ=IPOS+14
          ISTAT1=IST1ST
          IFORM1=IST1FO
          IACCE1=IST1AC
          IPROT1=IST1PR
          ICURS1=IST1CS
          IERRF1='NO'
!
          IREWI1='ON'
          CALL DPOPFI(IOUNI1,IFILE1,ISTAT1,IFORM1,IACCE1,IPROT1,ICURS1,   &
                      IREWI1,ISUBN0,IERRF1,IBUGS2,ISUBRO,IERROR)
          IF(IERRF1.EQ.'YES')THEN
            IFILE1=IFILE1SV
            GO TO 9000
          ENDIF
!
!         STEP 2: CREATE SCRIPT
!
          IAOUT=' '
          IAOUT(1:18)='$excelFilePath = "'
          ISTRT=19
!
!         CHECK IF FILE NAME CONTAINS A FULL PATH
!
          IFLAGP=0
          IF(IHOST1.EQ.'IBM-')THEN
            IF(IFILE(2:3).EQ.':\')IFLAGP=1
          ELSE
            IF(IFILE(1:1).EQ.'/')IFLAGP=1
          ENDIF
          IF(IFLAGP.EQ.0)THEN
            ISTOP=18+NCPATHZ
            IAOUT(ISTRT:ISTOP)=IFILE1(1:NCPATHZ)
            ISTRT=ISTOP+1
            ISTOP=ISTRT+NCFILE-1
            IAOUT(ISTRT:ISTOP)=IFILE(1:NCFILE)
          ELSE
            ISTOP=ISTRT+NCFILE-1
            IAOUT(ISTRT:ISTOP)=IFILE(1:NCFILE)
          ENDIF
          ISTOP=ISTOP+1
          IAOUT(ISTOP:ISTOP)='"'
          WRITE(IOUNI1,'(A)')IAOUT(1:ISTOP)
!
          IAOUT=' '
          IAOUT(1:18)='$worksheetName = "'
          NCTEMP=0
          DO II=16,1,-1
             IF(IEXCSH(II:II).NE.' ')THEN
               NCTEMP=II
               EXIT
             ENDIF
          ENDDO
          IF(NCTEMP.EQ.0)THEN
            ISTRT=19
            ISTOP=24
            IAOUT(ISTRT:ISTOP)='Sheet1'
          ELSE
            ISTRT=19
            ISTOP=18+NCTEMP
            IAOUT(ISTRT:ISTOP)=IEXCSH(1:NCTEMP)
          ENDIF
          ISTOP=ISTOP+1
          IAOUT(ISTOP:ISTOP)='"'
          WRITE(IOUNI1,'(A)')IAOUT(1:ISTOP)
!
          IAOUT=' '
          IAOUT(1:16)='$csvFilePath = "'
          ISTRT=17
          ISTOP=16+NCPATHZ
          IAOUT(ISTRT:ISTOP)=IFILE1(1:NCPATHZ)
          ISTRT=ISTOP+1
          ISTOP=ISTRT+9
          IAOUT(ISTRT:ISTOP)='dpst1f.dat'
          ISTOP=ISTOP+1
          IAOUT(ISTOP:ISTOP)='"'
          WRITE(IOUNI1,'(A)')IAOUT(1:ISTOP)
!
!         EXTRACT SPECIFIC ROWS AND COLUMNS IF REQUESTED
!
          IF(IEXCR1.GE.1 .AND. IEXCR2.GE.IEXCR1)THEN
            IAOUT=' '
            IAOUT(1:12)='$startRow = '
            WRITE(IAOUT(13:18),'(I6)')IEXCR1
            ISTOP=18
            WRITE(IOUNI1,'(A)')IAOUT(1:ISTOP)
            IAOUT=' '
            IAOUT(1:10)='$endRow = '
            WRITE(IAOUT(11:16),'(I6)')IEXCR2
            ISTOP=16
            WRITE(IOUNI1,'(A)')IAOUT(1:ISTOP)
          ENDIF
          IF(IEXCC1.GE.1 .AND. IEXCC2.GE.IEXCC1)THEN
            IAOUT=' '
            IAOUT(1:15)='$startColumn = '
            WRITE(IAOUT(16:23),'(I6)')IEXCC1
            ISTOP=23
            WRITE(IOUNI1,'(A)')IAOUT(1:ISTOP)
            IAOUT=' '
            IAOUT(1:13)='$endColumn = '
            WRITE(IAOUT(14:19),'(I6)')IEXCC2
            ISTOP=19
            WRITE(IOUNI1,'(A)')IAOUT(1:ISTOP)
          ENDIF
!
          IAOUT=' '
          IAOUT(1:34) ='Import-Excel -Path $excelFilePath '
          IAOUT(35:64)='-worksheetName $worksheetName '
          ISTRT=65
!
          IF(IEXCR1.GE.1 .AND. IEXCR2.GE.IEXCR1)THEN
            ISTOP=ISTRT+19
            IAOUT(ISTRT:ISTOP) ='-StartRow $startRow '
            ISTRT=ISTOP+1
            ISTOP=ISTRT+15
            IAOUT(ISTRT:ISTOP) ='-EndRow $endRow '
            ISTRT=ISTOP+1
          ENDIF
          IF(IEXCC1.GE.1 .AND. IEXCC2.GE.IEXCC1)THEN
            ISTOP=ISTRT+25
            IAOUT(ISTRT:ISTOP) ='-StartColumn $startColumn '
            ISTRT=ISTOP+1
            ISTOP=ISTRT+21
            IAOUT(ISTRT:ISTOP) ='-EndColumn $endColumn '
            ISTRT=ISTOP+1
          ENDIF
!
          ISTOP=ISTRT+31
          IAOUT(ISTRT:ISTOP)='| Export-Csv -Path $csvFilePath '
          ISTRT=ISTOP+1
          ISTOP=ISTRT+32
          IAOUT(ISTRT:ISTOP)='-NoTypeInformation -Encoding UTF8'
!
          WRITE(IOUNI1,'(A)')IAOUT(1:ISTOP)
          IAOUT=' '
          ISTOP=4
          IAOUT(1:ISTOP)='exit'
          WRITE(IOUNI1,'(A)')IAOUT(1:ISTOP)
!
          IERRF1='NO'
          IENDF1='OFF'
          IREWI1='ON'
          CALL DPCLFI(IOUNI1,IFILE1,ISTAT1,IFORM1,IACCE1,IPROT1,ICURS1,   &
                      IENDF1,IREWI1,ISUBN0,IERRF1,IBUGS2,ISUBRO,IERROR)
          IF(IERRF1.EQ.'YES')THEN
            IFILE1=IFILE1SV
            GO TO 9000
          ENDIF
!
!         STEP 3: RUN SCRIPT
!
!             system  powershell.exe -NoProfile -ExecutionPolicy Bypass -File "script-name" > dpst2f.dat
!
          IAOUT=' '
          IAOUT(1:57)='powershell.exe -NoProfile -ExecutionPolicy Bypass -File "'
          ISTRT=58
          ISTOP=ISTRT+NCFILEZ-1
          IAOUT(ISTRT:ISTOP)=IFILE1(1:NCFILEZ)
          ISTRT=ISTOP+1
          ISTOP=ISTRT
          IAOUT(ISTRT:ISTOP)='"'
          ISSAV1=ISYSPE
          ISSAV2=ISYSHI
          ISYSPE='OFF'
          ISYSHI='OFF'
          CALL DPSYS2(IAOUT,ISTOP,ISUBRO,IERROR)
          ISYSPE=ISSAV1
          ISYSHI=ISSAV2
          IF(IERROR.EQ.'YES')THEN
            IFILE1=IFILE1SV
            GO TO 9000
          ENDIF
          IREANQ='ON'
          IFILE=' '
          IFILE='dpst1f.dat'
          NCFILE=10
          IFOUND='YES'
          IREADL=','
          ISKIP=0
!
        ELSEIF(IEXCEL.EQ.'ON' .AND. IEXCME.EQ.'PYTH')THEN
!
!         READ EXCEL FILE USING PYTHON
!
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
          WRITE(IOUNI5,'(A256)')IFILE(1:256)
          WRITE(IOUNI5,'(A16)')IEXCSH
!
          IVAL1=-1
          IVAL2=-1
          IF(IEXCR1.GE.1)IVAL1=IEXCR1
          IF(IEXCR2.GE.1)IVAL2=IEXCR2
          IF(IVAL1.GE.1 .AND. IVAL2.GE.1)THEN
            IF(IVAL1.GT.IVAL2)THEN
              IVAL3=IVAL1
              IVAL1=IVAL2
              IVAL2=IVAL3
            ENDIF
          ENDIF
          IF(IVAL1.GE.1)IVAL1=IVAL1-1
          WRITE(IOUNI5,'(I8)')IVAL1
          WRITE(IOUNI5,'(I8)')IVAL2
!
          IF(IEXCHE.EQ.'0')THEN
            WRITE(IOUNI5,'(A1)')'0'
          ELSEIF(IEXCHE.EQ.'None')THEN
            WRITE(IOUNI5,'(A2)')'-1'
          ELSE
            WRITE(IOUNI5,'(A4)')'-1'
          ENDIF
!
!         NOTE: PANDAS EXPECTS A SPECIFIC COLUMN ID, SO IF
!               IF COLUMN LIMITS ARE SPECIFIED, BOTH THE START
!               COLUMN AND THE STOP COLUMN MUST BE GIVEN.  IF
!               ONLY ONE IS GIVEN, THEN READ ALL COLUMNS.
!
          IVAL1=-1
          IVAL2=-1
          IF(IEXCC1.GE.1 .AND. IEXCC2.GE.1)THEN
            IVAL1=IEXCC1
            IVAL2=IEXCC2
            IF(IVAL1.GT.IVAL2)THEN
              IVAL3=IVAL1
              IVAL1=IVAL2
              IVAL2=IVAL3
            ENDIF
          ENDIF
          IF(IVAL1.GT.0)IVAL1=IVAL1-1
!         IF(IVAL2.GT.0)IVAL2=IVAL2-1
          WRITE(IOUNI5,'(I8)')IVAL1
          WRITE(IOUNI5,'(I8)')IVAL2
!
          IOP='CLOS'
          CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,   &
                      IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                      IBUGS2,ISUBRO,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ITYPEZ='PYTH'
          ISNAME='read_excel.py'
          IWIDZZ=13
          ISARGL=' '
          NCARG=0
          CALL DPEXR2(ITYPEZ,ISNAME,IWIDZZ,ISARGL,NCARG,   &
                      IBUGS2,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
          IFOUND='YES'
!
          IFILE=' '
          IFILE='dpst1f.dat'
          NCFILE=10
          IFOUND='YES'
          IREADL=','
          ISKIP=1
!
        ENDIF
!
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
            GO TO 9000
          ENDIF
!
!       2019/10: CHECK FOR ".csv" OR ".CSV" EXTENSION.  IF FOUND, SET
!                READ DELIMITER TO ",".
        ELSEIF(NCFILE.GE.4)THEN
          IF(IFILE(NCFILE-3:NCFILE).EQ.'.csv' .OR.   &
             IFILE(NCFILE-3:NCFILE).EQ.'.CSV')THEN
            IREADL=','
          ENDIF
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
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!CCCC NOTE: FOR THE "IMAGE" CASE, THE FILE OPEN IS HANDLED
!CCCC       BY THE "GD.C" CODE.
!CCCC
!CCCC       HOWEVER, WE DO NEED TO PERFORM A SEARCH OF THE
!CCCC       DATAPLOT DIRECTORIES AND LOOK FOR UPPER/LOWER
!CCCC       CASE ISSUES AS WE DO WITH OTHER FILES.  CALL
!CCCC       DPINF3 TO SEE IF FILE EXISTS BEFORE CALL GD
!CCCC       LIBRARY.
!
      IF(IOFILE.EQ.'YES' .AND. ICASRE.NE.'IMAG' .AND.   &
        ICASRE.NE.'IMAZ')THEN
!
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
              GO TO 9000
            ENDIF
          ELSE
            IFORM='FORMATTED'
            IFMFLG='OFF'
          ENDIF
        ELSE
          IFORM='FORMATTED'
          IFMFLG='OFF'
        ENDIF
 1442   FORMAT('      FOR UNFORMATTED READS OF MATRICES, THE ',   &
               ' FOLLOWING COMMAND IS REQUIRED:')
 1443   FORMAT('         SET UNFORMATTED COLUMNS <VALUE>')
 1444   FORMAT('      WHERE <VALUE> IS THE NUMBER OF COLUMNS IN THE ',   &
               'MATRTIX.')
!
        IF(IREACS(1:4).EQ.'CLOS')   &
          CALL DPOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
          IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
        IF(IERRFI.EQ.'YES')GO TO 9090
        IF(IREACS(1:4).EQ.'CLOS')IREACS='OPEN'
!
      ELSEIF(IOFILE.EQ.'YES' .AND.   &
        (ICASRE.EQ.'IMAG' .OR. ICASRE.EQ.'IMAZ'))THEN
!
        CALL DPINF3(IFILE,FTEMP,IEXIST,   &
                    ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
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
          GO TO 9090
        ELSE
          IFILE=FTEMP
        ENDIF
!
        IF(IMAGTY.LT.1 .OR. IMAGTY.GT.9)THEN
          DO 1560 I=MAXSTR,1,-1
            IF(IFILE(I:I).NE.' ')THEN
              NLAST=I
              GO TO 1569
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
            GO TO 9000
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
          ELSEIF(IFILE(NLAST-3:NLAST).EQ.'WBMP')THEN
            IMAGTY=3
          ELSEIF(IFILE(NLAST-3:NLAST).EQ.'wbmp')THEN
            IMAGTY=3
          ELSEIF(IFILE(NLAST-2:NLAST).EQ.'GIF')THEN
            IMAGTY=4
          ELSEIF(IFILE(NLAST-2:NLAST).EQ.'gif')THEN
            IMAGTY=4
          ELSEIF(IFILE(NLAST-3:NLAST).EQ.'GIFF')THEN
            IMAGTY=4
          ELSEIF(IFILE(NLAST-3:NLAST).EQ.'giff')THEN
            IMAGTY=4
          ELSEIF(IFILE(NLAST-3:NLAST).EQ.'TIFF')THEN
            IMAGTY=5
          ELSEIF(IFILE(NLAST-3:NLAST).EQ.'tiff')THEN
            IMAGTY=5
          ELSEIF(IFILE(NLAST-2:NLAST).EQ.'TIF')THEN
            IMAGTY=5
          ELSEIF(IFILE(NLAST-2:NLAST).EQ.'tif')THEN
            IMAGTY=5
          ELSEIF(IFILE(NLAST-2:NLAST).EQ.'BMP')THEN
            IMAGTY=6
          ELSEIF(IFILE(NLAST-2:NLAST).EQ.'bmp')THEN
            IMAGTY=6
          ELSEIF(IFILE(NLAST-3:NLAST).EQ.'WEBP')THEN
            IMAGTY=7
          ELSEIF(IFILE(NLAST-3:NLAST).EQ.'webp')THEN
            IMAGTY=7
          ELSEIF(IFILE(NLAST-2:NLAST).EQ.'TGA')THEN
            IMAGTY=8
          ELSEIF(IFILE(NLAST-2:NLAST).EQ.'tga')THEN
            IMAGTY=8
          ELSEIF(IFILE(NLAST-2:NLAST).EQ.'XPM')THEN
            IMAGTY=9
          ELSEIF(IFILE(NLAST-2:NLAST).EQ.'xpm')THEN
            IMAGTY=9
          ELSEIF(IFILE(NLAST-3:NLAST).EQ.'AVIF')THEN
            IMAGTY=10
          ELSEIF(IFILE(NLAST-3:NLAST).EQ.'avif')THEN
            IMAGTY=10
          ELSEIF(IFILE(NLAST-3:NLAST).EQ.'HEIF')THEN
            IMAGTY=11
          ELSEIF(IFILE(NLAST-3:NLAST).EQ.'heif')THEN
            IMAGTY=11
          ELSE
            WRITE(ICOUT,1501)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1571)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
        ENDIF
!
        NCFILE=MAXSTR
        DO 1581 I=NCFILE,1,-1
          IF(IFILE(I:I).NE.' ')THEN
            NCFILE=I
            GO TO 1589
          ENDIF
 1581   CONTINUE
 1589   CONTINUE
        DO 1590 I=1,NCFILE
          CALL DPCOAN(IFILE(I:I),IJUNK)
          IADE(I)=IJUNK
 1590   CONTINUE
        IADE(NCFILE+1)=0
!
        IXSIZE=0
        IYSIZE=0
        IERR=0
#ifdef HAVE_GD
        CALL gdload(IMAGTY,IXSIZE,IYSIZE,IADE,IERR)
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
          GO TO 9090
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
          GO TO 9090
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
          GO TO 9090
        ENDIF
!
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
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IRD2=IRD
      IF(IMACCS.EQ.'OPEN'.OR.IMALEV.GE.1)THEN
        IRD2=IMACNU
      ENDIF
      IF(IOFILE.EQ.'YES')IRD2=IREANU
      IF(IOTERM.EQ.'YES')IRD2=IRD
      IF(ICASRE.EQ.'CLIP')IRD2=IRD
!
      IOUNIT=IRD2
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
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICASEQ='FULL'
      ILOCQ=NUMARG+1
      IF(NUMARG.LT.1)GO TO 390
      DO 300 J=1,NUMARG
        J1=J
        IF(IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ')THEN
          ICASEQ='SUBS'
          ILOCQ=J1
          GO TO 390
        ELSEIF(IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  ')THEN
          ICASEQ='SUBS'
          ILOCQ=J1
          GO TO 390
        ELSEIF(IHARG(J).EQ.'FOR '.AND.IHARG2(J).EQ.'    ')THEN
          ICASEQ='FOR'
          ILOCQ=J1
          GO TO 390
        ENDIF
  300 CONTINUE
  390 CONTINUE
!
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
        WRITE(ICOUT,391)NUMARG,ILOCQ
  391   FORMAT('NUMARG,ILOCQ = ',2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ******************************************************
!               **  STEP 4--                                        **
!               **  DETERMINE THE TYPE AND NUMBER OF ITEMS          **
!               **  TO BE READ   .                                  **
!               **  NUMALL = TOTAL NUMBER OF READ  ITEMS            **
!               **           (AS DETERMINED BY INCLUDING ONLY ALL   **
!               **           BEFORE 'SUBSET' OR 'EXCEPT' OR 'FOR')  **
!               **  NUMV   = NUMBER OF VARIABLES TO BE READ    ;    **
!               **  NUMP   = NUMBER OF PARAMETERS TO BE READ    ;   **
!               **  NUMM   = NUMBER OF MODELS TO BE READ            **
!               **           (SHOULD = 0 OR 1)                      **
!               **  NUMF   = NUMBER OF FUNCTIONS TO BE READ         **
!               **  NUMU   = NUMBER OF UNKNOWNS TO BE READ    ;     **
!               **  NUME   = TOTAL NUMBER OF READ  ITEMS            **
!               **           (SHOULD = NUMALL);                     **
!               ******************************************************
!
      ISTEPN='4'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IV=0
      IP=0
      IM=0
      IF=0
      IU=0
      IE=0
!
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
!
!CCCC JULY 2002: QUOTED FILE NAMES MAY CONTAIN SPACES.
!CCCC DETERMINE HOW MANY ARGUMENTS FILE NAME MAY CONTAIN.
!CCCC JUNE 2003: UPDATE TO INCLUDE HYPHENS AS WELL AS SPACES.
!
      IF(IOFILE.EQ.'YES' .AND. IFILE(1:1).EQ.'"')THEN
!
!CCCC   2023/08: FIX BUG IN FOLLOWING LINE
!CCCC   DO421I=MAXSTR,1,-1
!
        DO 421 I=NCFILE,1,-1
          IF(IFILE(I:I).NE.' ')THEN
            ILAST=I
            GO TO 424
          ENDIF
  421   CONTINUE
  424   CONTINUE
        ICOUNT=0
        ISPAC=0
        DO 426 I=1,ILAST
          IF((IFILE(I:I).EQ.' '.OR.IFILE(I:I).EQ.'-') .AND.   &
            ISPAC.EQ.0)THEN
            ISPAC=1
            ICOUNT=ICOUNT+1
          ELSEIF((IFILE(I:I).NE.' '.AND.IFILE(I:I).NE.'-') .AND.   &
            ISPAC.EQ.1)THEN
            ISPAC=0
          ENDIF
  426   CONTINUE
        JMIN=JMIN+ICOUNT
!
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
          WRITE(ICOUT,427)ICOUNT,JMIN
  427     FORMAT('ICOUNT,JMIN = ',2I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
      ENDIF
!
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
!
      IVALMA=0
      NUMALL=0
      NUMALL=JMAX-JMIN+1
      IF(ICASRE.EQ.'CLIP'.AND.NUMALL.LE.0)IVRLST='NO'
      IF(ICASRE.EQ.'VARI'.AND.NUMALL.LE.0)IVRLST='NO'
!
      ISTEPN='4A'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,403)ICASRE,IVRLST,JMIN,JMAX,NUMALL,ISKIP
  403   FORMAT('ICASRE,IVRLST,JMIN,JMAX,NUMALL,ISKIP = ',2(A4,2X),4I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IVAR=0
      IF(IVRLST.EQ.'NO' .AND. ICASRE.EQ.'VARI' .AND.   &
         IOTERM.NE.'LOOP')THEN
!
!       SKIP AUTOMATIC CASE:
!
!        1. IF IAVANM = FILE, THEN RETRIEVE VARIABLE LIST FROM LINE JUST
!           BEFORE THE "----".
!
!        2. IF IAVANM <> FILE, THEN USE AUTOMATIC VARIABLE NAMES (BASE
!           WILL BE DETERMINED BY IAVABN).
!
        IF(ISKIP.EQ.-1.AND.IOFILE.EQ.'YES'.AND.   &
           (ICASRE.NE.'IMAG' .AND. ICASRE.NE.'IMAZ'))THEN
!
          ISTEPN='4B'
          IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')   &
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!         CASE 1: RETRIEVE VARIABLE LIST FROM THE FILE
!
          IF(IAVANM.EQ.'FILE')THEN
!
!           STEP 1: READ UNTIL "---" FOUND
!
            DO 4578 I=1,MAXOBV
              ILINE=I
              NUMCHA=-1
              CALL DPREFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
                          IA,NUMCHA,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
!
              IF(IERROR.EQ.'YES')GO TO 8800
              IF(IA(1).EQ.'E'.AND.IA(2).EQ.'O'.AND.IA(3).EQ.'F'.AND.   &
                NUMCHA.EQ.3)THEN
                REWIND IOUNIT
                GO TO 8800
              ELSEIF(IA(1).EQ.'-'.AND.IA(2).EQ.'-'.AND.IA(3).EQ.'-'.AND.   &
                IA(4).EQ.'-')THEN
                GO TO 4581
              ELSE
                DO 4511 J=1,MAXRDV
                  IASAVE(J)=IA(J)
 4511           CONTINUE
              ENDIF
 4578       CONTINUE
 4581       CONTINUE
            ISKIP=ILINE
!
!           STEP 2: EXTRACT THE VARIABLE NAMES
!
            IF(ILINE.GT.1)THEN
              IFRST=0
              ILAST=0
              INEW=0
              IVAR=0
              NLAST=1
              DO 44581 LL=NUMRCM,1,-1
                IF(IASAVE(LL).NE.' ')THEN
                  NLAST=LL
                  GO TO 44582
                ENDIF
44581         CONTINUE
44582         CONTINUE
              NTEMP=NLAST
              CALL DPUPPE(IASAVE,NTEMP,IASAVE,IBUGS2,IERROR)
              DO 4583 J=1,NTEMP
!CCCC           IF(IASAVE(J)(1:1).EQ.' ' .OR. IASAVE(J)(1:1).EQ.',')THEN
                IF(IASAVE(J)(1:1).EQ.IREADL(1:1) .OR. J.EQ.NLAST)THEN
                  IF(INEW.EQ.1)THEN
                    IVAR=IVAR+1
                    ILAST=J
                    NCHAR=ILAST-IFRST+1
                    ICNT=0
                    DO 4585 K=1,MIN(4,NCHAR)
                      CALL DPCOAN(IASAVE(IFRST+K-1)(1:1),IVAL)
                      IFLAG=0
                      IF(IVAL.GE.65 .AND. IVAL.LE.90)IFLAG=1
                      IF(IVAL.GE.48 .AND. IVAL.LE.57)IFLAG=1
                      IF(IASAVE(IFRST+K-1)(1:1).EQ.'_')IFLAG=1
                      IF(IFLAG.EQ.1)THEN
                        ICNT=ICNT+1
                        IF(ICNT.LE.4)THEN
                          IVLIST(IVAR)(ICNT:ICNT)=IASAVE(IFRST+K-1)(1:1)
                        ELSEIF(ICNT.GE.5 .AND. ICNT.LE.8)THEN
                          IVLIS2(IVAR)(ICNT-4:ICNT-4)=   &
                                 IASAVE(IFRST+K-1)(1:1)
                        ENDIF
                      ENDIF
 4585               CONTINUE
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
!
!         CASE 2: USE AUTOMATIC VARIABLE NAMES
!
          ELSEIF(IAVANM.EQ.'AUTO')THEN
!
            ISTEPN='4C'
            IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')   &
              CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
            DO 6578 I=1,MAXOBV
              ILINE=I
              NUMCHA=-1
              CALL DPREFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
                          IA,NUMCHA,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
!
              IF(IERROR.EQ.'YES')GO TO 8800
              IF(IA(1).EQ.'E'.AND.IA(2).EQ.'O'.AND.IA(3).EQ.'F'.AND.   &
                NUMCHA.EQ.3)THEN
                REWIND IOUNIT
                GO TO 8800
              ELSEIF(IA(1).EQ.'-'.AND.IA(2).EQ.'-'.AND.IA(3).EQ.'-'.AND.   &
                IA(4).EQ.'-')THEN
                GO TO 6581
              ELSE
                DO 6511 J=1,NUMRCM
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
!
            NCBASE=0
            DO 54590 II=8,1,-1
              IF(IAVABN(II:II).NE.' ')THEN
                NCBASE=II
                GO TO 54599
              ENDIF
54590       CONTINUE
54599       CONTINUE
!
 5592       CONTINUE
            DO 5597 I=1,MAXRCL
              ISTOR1(I)='    '
              ISTOR2(I)='    '
              ISTOR3(I)='    '
              IB(I)='    '
 5597       CONTINUE
            CALL DPREAL(IRD2,IFCOL3,IFCOL4,MINCO2,MAXCO2,            &
                   X0,NUMDPL,IFLGSV,                                 &
                   IXC,NXC,                                          &
                   ICASRE,IFUNC2,N2,MAXN2,                           &
                   IMACRO,IMACNU,IMACCS,                             &
                   IANSLC,IWIDTH,IREACS,ISTOR1,ISTOR2,IEND,NUMLRD,   &
                   IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,     &
                   ICOMCH,ICOMSW,LINETY,IGRPA2,                      &
                   IFCOLL,IFCOLU,ITYPE,NCOLS,NCALL,                  &
                   IREADL,IDATDL,ITIMDL,IRDIPA,PREAMV,               &
                   MAXRDV,MAXCHV,IFIETY,                             &
                   IDECPT,IDATMV,IDATNN,                             &
                   IREACD,IREACM,IREADS,IREAPM,IREAMC,ITABNC,IREALT, &
                   XTAG,IOUNI5,                                      &
                   IREAAS,IREAPC,                                    &
                   IB,                                               &
                   IOTERM,IANSLO,MAXLIL,MAXCIL,ILOOST,ILOOLI,        &
                   IREPCH,IMALEV,IREANQ,                             &
                   IERRFI,IBUGS2,ISUBRO,IERROR)
            IF(IERROR.EQ.'YES')GO TO 9000
            IF(LINETY.EQ.'BLAN')GO TO 5592
            NUMLRD=0
            IF(NUMDPL.GT.0)THEN
!
              IF(IMNVAR.LT.0)IMNVAR=NUMDPL
              IF(IMXVAR.LT.0)IMXVAR=NUMDPL
              IF(NUMDPL.LT.IMNVAR)IMNVAR=NUMDPL
              IF(NUMDPL.GT.IMXVAR)IMXVAR=NUMDPL
              DO 5593 J=1,NUMDPL
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
!
!     SKIP AUTOMATIC OFF CASE
!
!        1) SET COLUMN LIMITS, ROW LIMITS
!        2) SKIP OVER HEADER LINES (IF NEEDED)
!        3) READ SINGLE LINE OF DATA
!        4) DETERMINE NUMBER OF COLUMNS OF DATA IN THAT LINE
!        5) SET VARIABLE NAMES TO X1, ..., Xk
!           (2014/10: THE IAVABN VARIABLE SPECIFIES THE DEFAULT FOR THE
!           VARIABLE NAMES, THE DEFUALT IS NOW COL1, COL2, ETC.)
!
!           IF IVARLA="ON", FIRST LINE READ SHOULD BE VARIABLE NAMES
!
!           2020/08: FOR COMMA DELIMITED FILES, CHECK FOR COMMA
!                    WHEN SEPARATING THE VARIABLE NAMES
!
!        6) REWIND THE FILE
!
        ELSEIF(IOFILE.EQ.'YES' .AND. ICASRE.NE.'IMAG' .AND.   &
               ICASRE.NE.'IMAZ')THEN
!
!         STEP 1: SKIP HEADER LINES
!
          IF(ISKIP.GE.0)THEN
            IFRMIN=IFROW1
            IFRMAX=IFROW1+ISKIP
            IF(IFRMAX.LT.IFRMIN)IFRMAX=IFRMIN
            MINCO2=1
            MAXCO2=NUMRCM
            IFCOL3=IFCOL1
            IFCOL4=IFCOL2
            IF(IFRMIN.LT.IFRMAX)THEN
              DO 4591 IFROW=IFRMIN,IFRMAX-1
                ILINE=IFROW
                NUMCHA=-1
                CALL DPREFI(   &
                    IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
                    IA,NUMCHA,   &
                    ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
                                                                                                                                  
                IF(IA(1).NE.'-'.OR.IA(2).NE.'-'.OR.IA(3).NE.'-'.OR.   &
                IA(4).NE.'-')THEN
                  DO 4513 J=1,NUMRCM
                    IASAVE(J)=IA(J)
 4513             CONTINUE
                ENDIF
 4591         CONTINUE
            ENDIF
!
!         STEP 2A: READ FIRST LINE OF DATA FILE TO DETERMINE NUMBER OF
!                  VARIABLES
!
            IF(IVARLA.EQ.'OFF' .AND. IAVANM.EQ.'AUTO')THEN
              NUMLRD=0
!
              NCBASE=0
              DO 44590 II=8,1,-1
                IF(IAVABN(II:II).NE.' ')THEN
                  NCBASE=II
                  GO TO 44599
                ENDIF
44590         CONTINUE
44599         CONTINUE
!
 4592         CONTINUE
              DO 4597 I=1,MAXRCL
                ISTOR1(I)='    '
                ISTOR2(I)='    '
                ISTOR3(I)='    '
                IB(I)='    '
 4597         CONTINUE
              CALL DPREAL(IRD2,IFCOL3,IFCOL4,MINCO2,MAXCO2,            &
                   X0,NUMDPL,IFLGSV,                                   &
                   IXC,NXC,                                            &
                   ICASRE,IFUNC2,N2,MAXN2,                             &
                   IMACRO,IMACNU,IMACCS,                               &
                   IANSLC,IWIDTH,IREACS,ISTOR1,ISTOR2,IEND,NUMLRD,     &
                   IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,       &
                   ICOMCH,ICOMSW,LINETY,IGRPA2,                        &
                   IFCOLL,IFCOLU,ITYPE,NCOLS,NCALL,                    &
                   IREADL,IDATDL,ITIMDL,IRDIPA,PREAMV,                 &
                   MAXRDV,MAXCHV,IFIETY,                               &
                   IDECPT,IDATMV,IDATNN,                               &
                   IREACD,IREACM,IREADS,IREAPM,IREAMC,ITABNC,IREALT,   &
                   XTAG,IOUNI5,                                        &
                   IREAAS,IREAPC,                                      &
                   IB,                                                 &
                   IOTERM,IANSLO,MAXLIL,MAXCIL,ILOOST,ILOOLI,          &
                   IREPCH,IMALEV,IREANQ,                               &
                   IERRFI,IBUGS2,ISUBRO,IERROR)
              IF(IERROR.EQ.'YES')GO TO 9000
              IF(LINETY.EQ.'BLAN')GO TO 4592
              NUMLRD=0
              IF(NUMDPL.GT.0)THEN
!
                IF(IMNVAR.LT.0)IMNVAR=NUMDPL
                IF(IMXVAR.LT.0)IMXVAR=NUMDPL
                IF(NUMDPL.LT.IMNVAR)IMNVAR=NUMDPL
                IF(NUMDPL.GT.IMXVAR)IMXVAR=NUMDPL
                DO 4593 J=1,NUMDPL
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
!
!         STEP 2B: VARIABLE NAMES READ FROM LAST HEADER LINE (OR
!                  NEXT TO LAST LINE IF LAST LINE STARTS WITH
!                  "----").
!
            ELSEIF(IVARLA.EQ.'OFF' .AND. IAVANM.EQ.'FILE')THEN
              IF(ILINE.GT.1)THEN
                IFRST=0
                ILAST=0
                INEW=0
                IVAR=0
!
!               2020/08: SET LINE LENGTH TO MAXIMUM RECORD LENGTH RATHER
!                        THAN 255.  ALSO, ONLY INCLUDE UPPER/LOWER CASE
!                        CHARACTERS AND UNDERSCORE CHARACTER IN VARIABLE
!                        NAMES.
!CCCC           NTEMP=255
                NTEMP=NUMRCM
                NLAST=1
                DO 5581 LL=NUMRCM,1,-1
                  IF(IASAVE(LL).NE.' ')THEN
                    NLAST=LL
                    GO TO 5582
                  ENDIF
 5581           CONTINUE
 5582           CONTINUE
!
!               2024/01: SET NTEMP TO NLAST
!
                NTEMP=NLAST
                CALL DPUPPE(IASAVE,NTEMP,IASAVE,IBUGS2,IERROR)
                DO 5583 J=1,NTEMP
!CCCC             IF(IASAVE(J)(1:1).EQ.' '.OR.IASAVE(J)(1:1).EQ.',')THEN
                  IF(IASAVE(J)(1:1).EQ.IREADL(1:1) .OR. J.EQ.NLAST)THEN
                    IF(INEW.EQ.1)THEN
                      IVAR=IVAR+1
                      ILAST=J
                      NCHAR=ILAST-IFRST+1
                      ICNT=0
                      DO 5585 K=1,NCHAR
                        CALL DPCOAN(IASAVE(IFRST+K-1)(1:1),IVAL)
                        IFLAG=0
                        IF(IVAL.GE.65 .AND. IVAL.LE.90)IFLAG=1
                        IF(IVAL.GE.48 .AND. IVAL.LE.57)IFLAG=1
                        IF(IASAVE(IFRST+K-1)(1:1).EQ.'_')IFLAG=1
                        IF(IFLAG.EQ.1)THEN
                          ICNT=ICNT+1
                          IF(ICNT.LE.4)THEN
                            IVLIST(IVAR)(ICNT:ICNT)=   &
                                   IASAVE(IFRST+K-1)(1:1)
                          ELSEIF(ICNT.GE.5 .AND. ICNT.LE.8)THEN
                            IVLIS2(IVAR)(ICNT-4:ICNT-4)=   &
                                   IASAVE(IFRST+K-1)(1:1)
                          ENDIF
                        ENDIF
 5585                 CONTINUE
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
!
!         STEP 3: CASE WHERE VARIABLE NAMES ON FIRST LINE
!
!                 2017/03: CHECK FOR "," AS SEPARATOR IN ADDITION
!                          TO SPACE CHARACTER.
!
            ELSEIF(IVARLA.EQ.'ON')THEN
              NUMCHA=-1
              CALL DPREFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
                   IASAVE,NUMCHA,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
              IF(IERROR.EQ.'YES')GO TO 8800
!
              IFRST=0
              ILAST=0
              INEW=0
              IVAR=0
!
!               2020/08: SET LINE LENGTH TO MAXIMUM RECORD LENGTH RATHER
!                        THAN 255.  ALSO, ONLY INCLUDE ALPHANUMERIC
!                        CHARACTERS AND UNDERSCORE CHARACTER IN VARIABLE
!                        NAMES.
!
              NLAST=1
              DO 34581 LL=NUMRCM,1,-1
                IF(IASAVE(LL).NE.' ')THEN
                  NLAST=LL
                  GO TO 34582
                ENDIF
34581         CONTINUE
34582         CONTINUE
              NTEMP=NLAST
              CALL DPUPPE(IASAVE,NTEMP,IASAVE,IBUGS2,IERROR)
              DO 34583 J=1,NTEMP
!CCCC           IF(IASAVE(J)(1:1).EQ.' ' .OR. IASAVE(J)(1:1).EQ.',')THEN
                IF(IASAVE(J)(1:1).EQ.IREADL(1:1) .OR. J.EQ.NLAST)THEN
                  IF(INEW.EQ.1)THEN
                    IVAR=IVAR+1
                    ILAST=J
                    NCHAR=ILAST-IFRST+1
                    ICNT=0
                    DO 34585 K=1,MIN(4,NCHAR)
                      CALL DPCOAN(IASAVE(IFRST+K-1)(1:1),IVAL)
                      IFLAG=0
                      IF(IVAL.GE.65 .AND. IVAL.LE.90)IFLAG=1
                      IF(IVAL.GE.48 .AND. IVAL.LE.57)IFLAG=1
                      IF(IASAVE(IFRST+K-1)(1:1).EQ.'_')IFLAG=1
                      IF(IFLAG.EQ.1)THEN
                        ICNT=ICNT+1
                        IF(ICNT.LE.4)THEN
                          IVLIST(IVAR)(ICNT:ICNT)=IASAVE(IFRST+K-1)(1:1)
                        ELSEIF(ICNT.GE.5 .AND. ICNT.LE.8)THEN
                          IVLIS2(IVAR)(ICNT-4:ICNT-4)=   &
                                 IASAVE(IFRST+K-1)(1:1)
                        ENDIF
                      ENDIF
34585               CONTINUE
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
!
              JMIN=1
              JMAX=IVAR
            ENDIF
          ENDIF
        ENDIF
!
!       2020/08: CHECK FOR DUPLICATE/EMPTY NAMES
!
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
          ISTEPN='4B'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        ENDIF
!
        IF(IVAR.GE.2)THEN
          NBLANK=0
          DO 34610 JJ=IVAR,2,-1
            ILAST=JJ-1
            DO 34620 KK=1,ILAST
              IF(IVLIST(JJ).EQ.IVLIST(KK) .AND.   &
                 IVLIS2(JJ).EQ.IVLIS2(KK))THEN
                NLAST=0
                DO 34630 LL=4,1,-1
                  IF(IVLIS2(JJ)(LL:LL).NE.' ')THEN
                    NLAST=LL+4
                    GO TO 34639
                  ENDIF
34630           CONTINUE
                DO 34635 LL=4,1,-1
                  IF(IVLIST(JJ)(LL:LL).NE.' ')THEN
                    NLAST=LL
                    GO TO 34639
                  ENDIF
34635           CONTINUE
34639           CONTINUE
!
!               NOW CREATE MODIFIED NAME
!
!                   1. IF BLANK NAME, USE "Zxxxx"
!                   2. IF LESS THAN 8 CHARACTERS IN NAME, APPEND
!                      A "Z" TO NAME
!                   3. IF 8 CHARACTERS IN NAME, CHANGE LAST CHARACTER
!                      TO NAME
!
                IF(NLAST.EQ.0)THEN
                  IVLIST(JJ)='Z   '
                  NBLANK=NBLANK+1
                  IF(NBLANK.LE.9)THEN
                    WRITE(IVLIST(JJ)(2:2),'(I1)')NBLANK
                  ELSEIF(NBLANK.LE.99)THEN
                    WRITE(IVLIST(JJ)(2:3),'(I2)')NBLANK
                  ELSEIF(NBLANK.LE.999)THEN
                    WRITE(IVLIST(JJ)(2:4),'(I3)')NBLANK
                  ENDIF
                ELSEIF(NLAST.LT.4)THEN
                  IVLIST(JJ)(NLAST+1:NLAST+1)='Z'
                ELSEIF(NLAST.LT.8)THEN
                  IVLIS2(JJ)(NLAST-3:NLAST-3)='Z'
                ELSEIF(NLAST.EQ.8)THEN
                   IF(IVLIS2(JJ)(4:4).EQ.'Z')THEN
                     IVLIS2(JJ)(4:4)='X'
                   ELSE
                     IVLIS2(JJ)(4:4)='Z'
                   ENDIF
                ENDIF
!
              ENDIF
34620       CONTINUE
34610     CONTINUE
        ENDIF
!
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
          ISTEPN='4C'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        ENDIF
!
      ENDIF
!
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
        ISTEPN='4D'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      ENDIF
!
      IF(JMIN.GT.JMAX)GO TO 4290
      IF(ICASRE.EQ.'ROWI')GO TO 4290
!
!  JANUARY 2004.  THE DPREAL ROUTINE CAN NOW RETURN CHARACTER AS
!  WELL AS NUMERIC DATA.  FOR THE VARIABLE READ CASE, READ FIRST
!  LINE OF FILE TO EXTRACT THE TYPES FOR EACH OF THE VARIABLES.
!  FOLLOWING CONDITIONS NEED TO APPLY:
!
!  1. THIS IS THE VARIABLE (AS OPPOSSED TO STRING, PARAMETER, MATRIX)
!     READ CASE.
!
!  2. THE CONVERT CHARACTER CASE IS SET TO CHARACTER (AS OPPOSSED
!     TO IGNORE OR ERROR).  THIS IS DETERMINED BY VALUE OF IGRPAU.
!
!  3. THE READ IS FROM FILE RATHER THAN THE KEYBOARD.  FOR THE
!     KEYBOARD READ CASE, IGRPAU IS SET TO IGNORE.
!
!     2019/09: ALLOW READING OF CHARACTER DATA FROM TERMINAL.  PRIMARY
!              ISSUE IS THAT FIRST LINE IS READ TO DETERMINE THE
!              TYPE OF EACH FIELD.  FOR TERMINAL READ, CANNOT DO A
!              FILE REWIND, SO NEED TO SAVE THE FIRST LINE FOR
!              SUBSEQUENT USE.
!
!  4. IF A SET READ FORMAT HAS BEEN SET, NO CHARACTER DATA WILL
!     BE READ.
!
!  5. FOR NOW, READ CLIPBOARD WILL ONLY SUPPORT READING OF NUMERIC
!     VAIRABLES.
!
      ICFLAG='YES'
      IF(IGRPAU.NE.'CHAR' .AND. IGRPAU.NE.'CATE')ICFLAG='NO'
!CCCC IF(IOFILE.NE.'YES')ICFLAG='NO'
      IF(ICASRE.NE.'VARI')ICFLAG='NO'
      IF(NCREAF.GT.0)ICFLAG='NO'
      IF(ICASRE.EQ.'MATR')ICFLAG='NO'
      IF(ICASRE.EQ.'MATZ')ICFLAG='NO'
      IF(ICASRE.EQ.'IMAG')ICFLAG='NO'
      IF(ICASRE.EQ.'IMAZ')ICFLAG='NO'
      IF(ICASRE.EQ.'CLIP')ICFLAG='NO'
      IF(ICASRE.EQ.'ROWR')ICFLAG='NO'
      IFLGSV=0
!
      IF(ICFLAG.EQ.'YES')THEN
!
!        2018/07: CHECK IF ONE OF THE COLUMNS IS DESIGNATED TO
!                 BE A ROW LABEL.
!
        IRWLC2=-1
        IF(IRWLCO.GE.1)THEN
          IRWLC2=IRWLCO
        ENDIF
!
        MINCO2=1
        MAXCO2=NUMRCM
        IFCOL3=IFCOL1
        IFCOL4=IFCOL2
!
!       SKIP AUTOMATIC CASE: NEED TO READ UNTIL "----" FOUND
!
        IF(ISKIP.EQ.-1)THEN
          DO 17382 IFROW=1,MAXOBV
            NUMCHA=-1
            CALL DPREFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
                        IA,NUMCHA,   &
                        ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
            IF(IERROR.EQ.'YES')THEN
              IGRPA2='IGNO'
              ICFLAG='NO'
              GO TO 17399
            ELSEIF(IA(1).EQ.'E'.AND.IA(2).EQ.'O'.AND.IA(3).EQ.'F'.AND.   &
              NUMCHA.EQ.3)THEN
              IGRPA2='IGNO'
              ICFLAG='NO'
              GO TO 17399
            ELSEIF(IA(1).EQ.'-'.AND.IA(2).EQ.'-'.AND.IA(3).EQ.'-'.AND.   &
               IA(4).EQ.'-')THEN
                GO TO 17391
              ENDIF
17382       CONTINUE
        ELSE
          ITEMP=IFROW1+ISKIP-1
          IF(ITEMP.GT.0)THEN
            DO 17380 IFROW=1,ITEMP
              NUMCHA=-1
              IF(IOTERM.EQ.'LOOP')THEN
                ILOOLI=ILOOLI+1
              ELSE
                CALL DPREFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,   &
                            IPROT,ICURST,IA,NUMCHA,   &
                            ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
                IF(IERROR.EQ.'YES')THEN
                  IGRPA2='IGNO'
                  ICFLAG='NO'
                  GO TO 17399
                ENDIF
                IF(IA(1).EQ.'E'.AND.IA(2).EQ.'O'.AND.IA(3).EQ.'F'.AND.   &
                  NUMCHA.EQ.3)THEN
                  IGRPA2='IGNO'
                  ICFLAG='NO'
                  GO TO 17399
                ENDIF
              ENDIF
17380       CONTINUE
          ENDIF
        ENDIF
!
17391   CONTINUE
        NCALL=0
        NCOLS=0
        CALL DPREAL(IRD2,IFCOL3,IFCOL4,MINCO2,MAXCO2,X0,NUMDPL,IFLGSV, &
                    IXC,NXC,                                           &
                    ICASRE,IFUNC2,N2,MAXN2,                            &
                    IMACRO,IMACNU,IMACCS,                              &
                    IANSLC,IWIDTH,IREACS,ISTOR1,ISTOR2,IEND,NUMLRD,    &
                    IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,      &
                    ICOMCH,ICOMSW,LINETY,IGRPA2,                       &
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
!
!       2019/04: CHECK FOR BLANK LINE BEFORE ERROR
!
        IF(LINETY.EQ.'BLAN')GO TO 17391
        IF(IERROR.EQ.'YES')GO TO 9000
!
        IF(IMNVAR.LT.0)IMNVAR=NUMDPL
        IF(IMXVAR.LT.0)IMXVAR=NUMDPL
        IF(NUMDPL.LT.IMNVAR)IMNVAR=NUMDPL
        IF(NUMDPL.GT.IMXVAR)IMXVAR=NUMDPL
!
        IF(NXC.LE.0)THEN
          ICFLAG='NO'
          IGRPA2='IGNO'
          GO TO 17399
        ENDIF
!
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
!
      ICNTNU=0
      ICNTCH=0
      ICOUNT=0
      IISKIP=0
!
      IF(ICASRE.EQ.'CLIP' .AND. IVRLST.EQ.'NO')GO TO 4290
!
      DO 4200 J=JMIN,JMAX
!
        IF(IISKIP.EQ.1)THEN
          IISKIP=0
          GO TO 4200
        ENDIF
!
        IF(ICFLAG.EQ.'NO' .OR. ICFLAG.EQ.'OFF')THEN
          ICOUNT=ICOUNT+1
        ENDIF
!
        IF(IVRLST.EQ.'NO')THEN
          IH1=IVLIST(J)
          IH2=IVLIS2(J)
        ELSE
          IH1=IHARG(J)
          IH2=IHARG2(J)
        ENDIF
!
!     **********
!     THE FOLLOWING 5 LINES OF CODE IS FOR      READ MATRIX.
!     IT ALLOWS COLUMN VECTOR NAMES TO BE FORMED
!     FROM THE BASE MATRIX NAME
!     BY THE APPENDING OF NUMBERS 1, 2, 3, ...
!     SEPTEMBER 1987
!     **********
!
        IF(ICASRE.EQ.'MATR')THEN
          IVALMA=IVALMA+1
          CALL DPAPN2(IHMAT1,IHMAT2,IVALMA,MAXRDV,   &
                      IH1,IH2,IBUGS2,ISUBRO,IERROR)
        ENDIF
!
!     ***************
!     THE FOLLOWING CODE ALLOWS THE    TO    KEYWORD
!     TO BE ACTIVATED, AS IN
!     READ FILE.EXT Y1 TO Y10
!     DECEMBER 1986
!     ***************
!
        ICASTO='OFF'
        IF(IH1.EQ.'TO  ')THEN
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
        ELSE
          IF(ICFLAG.EQ.'YES')THEN
            ICOUNT=ICOUNT+1
          ENDIF
          GO TO 4219
        ENDIF
 4215   CONTINUE
        IVAL=IVAL+1
!CCCC   ICOUNT=ICOUNT+1
        IF(ICFLAG.EQ.'YES')THEN
          IF(IVAL.GT.IVAL2)GO TO 4200
!CCCC     IF(IVAL.GE.IVAL2)GO TO 4200
        ELSE
          IF(IVAL.GE.IVAL2)GO TO 4200
        ENDIF
!
        CALL DPAPNU(IHARG(JM1),IHARG2(JM1),KNUMB,IVAL,   &
                    IH1,IH2,IBUGS2,ISUBRO,IERROR)
 4219   CONTINUE
!
!  JANUARY 2004: CHECK WHETHER NAME SHOULD BE ADDED TO
!  REGULAR NAME LIST OR CHARACTER VARIABLE NAME LIST.
!
        IF(ICFLAG.EQ.'YES')THEN
!
          IF(ITYPE(ICOUNT).EQ.1)THEN
            ICNTCH=ICNTCH+1
            IF(ICNTCH.GT.MAXCHV)GO TO 4200
            IFLGRL=0
            IF(IRWLC2.EQ.ICOUNT)THEN
              IRWLC3=ICNTCH
              IFLGRL=1
            ENDIF
            ICLIST(ICNTCH)=IH1
            ICLIS2(ICNTCH)=IH2
!
!           2018/07: CHECK IF THIS IS A PREVIOUSLY DEFINED NAME IF
!                    "CATEGORICAL" OPTION IS SET.  ONLY VARIABLE
!                    NAME IS ALLOWED.  OTHERWISE, REPORT AN ERROR.
!
            IF(IGRPAU.EQ.'CATE' .AND. IFLGRL.EQ.0)THEN
              DO 42300 II=1,NUMNAM
                I2=II
                IF(IH1.EQ.IHNAME(I2).AND.IH2.EQ.IHNAM2(I2))THEN
                  IF(IUSE(I2).NE.'V')THEN
                    WRITE(ICOUT,999)
                    CALL DPWRST('XXX','BUG ')
                    WRITE(ICOUT,211)
                    CALL DPWRST('XXX','BUG ')
                    WRITE(ICOUT,42320)
42320               FORMAT('      WHEN USING THE   SET CONVERT ',   &
                           'CHARACTER CATEGORICAL   COMMAND,')
                    CALL DPWRST('XXX','BUG ')
                    WRITE(ICOUT,42350)
42350               FORMAT('      THE REQUESTED NAME PREVIOUSLY ',   &
                           'EXISTS, BUT NOT AS A VARIABLE.')
                    CALL DPWRST('XXX','BUG ')
                    WRITE(ICOUT,4317)
                    CALL DPWRST('XXX','BUG ')
                    IERROR='YES'
                    GO TO 8800
                  ELSE
                    IECOLC(ICNTCH)=IVALUE(I2)
                    GO TO 42301
                  ENDIF
                ENDIF
42300         CONTINUE
              IECOLC(ICNTCH)=-1
42301         CONTINUE
            ENDIF
!
            IF(ICASTO.EQ.'ON')THEN
              IF(IVAL.GE.IVAL2)GO TO 4200
              GO TO 4215
            ELSE
              GO TO 4200
            ENDIF
          ELSE
            ICNTNU=ICNTNU+1
          ENDIF
        ENDIF
!
        ICASEA='    '
        DO 4300 I=1,NUMNAM
          I2=I
          IF(IH1.EQ.IHNAME(I).AND.IH2.EQ.IHNAM2(I))THEN
            IF(IUSE(I).EQ.'V')THEN
              ICASEA='V'
              IV=IV+1
              IF(IV.GT.MAXV2)GO TO 4370
              JVNAM1(IV)=IH1
              JVNAM2(IV)=IH2
              NIV(IV)=IN(I2)
!
              IF(ICASRE.EQ.'VARI' .OR. ICASRE.EQ.'CLIP')GO TO 4370
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,211)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,4312)
 4312         FORMAT('      A NAME IN THE LIST OF VARIABLES TO BE ',   &
                     'READ INCLUDED THE')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,4315)
 4315         FORMAT('      NAME OF A PREVIOUSLY-DEFINED PARAMETER OR ',   &
                     'FUNCTION.')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,4316)IH1,IH2
 4316         FORMAT('      THE NAME OF THE PARAMETER OR FUNCTION WAS ',   &
                     2A4,'   .')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,4317)
 4317         FORMAT('      NO READ WAS CARRIED OUT.')
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GO TO 8800
!
            ELSEIF(IUSE(I).EQ.'P')THEN
              ICASEA='P'
              IP=IP+1
              IF(IP.GT.MAXP2)GO TO 4370
              JPNAM1(IP)=IH1
              JPNAM2(IP)=IH2
              PVAL(IP)=VALUE(I2)
!
              IF(ICASRE.EQ.'PARA')GO TO 4370
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,211)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,4322)
 4322         FORMAT('      A NAME IN THE LIST OF PARAMETERS TO BE ',   &
                     'READ INCLUDED THE')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,4325)
 4325         FORMAT('      NAME OF A PREVIOUSLY-DEFINED VARIABLE OR ',   &
                     'FUNCTION.')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,4326)IH1,IH2
 4326         FORMAT('      THE NAME OF THE VARIABLE OR FUNCTION WAS ',   &
                     2A4,'   .')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,4317)
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GO TO 8800
!
            ELSEIF(IUSE(I).EQ.'M')THEN
              ICASEA='M'
              IM=IM+1
              IF(IM.GT.MAXM2)GO TO 4370
              JMNAM1(IM)=IH1
              JMNAM2(IM)=IH2
!
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,211)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,4332)
 4332         FORMAT('      A NAME IN THE LIST OF VARIABLES TO BE READ')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,4335)
 4335         FORMAT('      INCLUDED THE NAME OF A PREVIOUSLY-DEFINED ',   &
                     'MODEL.')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,4336)IH1,IH2
 4336         FORMAT('      THE NAME OF THE MODEL WAS ',2A4,'  .')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,4317)
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GO TO 8800
!
            ELSEIF(IUSE(I).EQ.'F')THEN
              ICASEA='F'
              IF=IF+1
              IF(IF.GT.MAXF2)GO TO 4370
              JFNAM1(IF)=IH1
              JFNAM2(IF)=IH2
              IFSTA2(IF)=IVSTAR(I2)
              IFSTO2(IF)=IVSTOP(I2)
!
              IF(ICASRE.EQ.'FUNC' .OR. ICASRE.EQ.'CFUN')GO TO 4370
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,211)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,4342)
 4342         FORMAT('      A NAME IN THE LIST OF FUNCTIONS (= ',   &
                     'STRINGS)')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,4344)
 4344         FORMAT('      TO BE READ INCLUDED THE NAME OF A ')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,4345)
 4345         FORMAT('      PREVIOUSLY-DEFINED VARIABLE OR PARAMETER.')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,4346)IH1,IH2
 4346         FORMAT('      THE NAME OF THE VARIABLE OR PARAMETER WAS ',   &
                     2A4,'   .')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,4317)
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GO TO 8800
!
            ENDIF
          ENDIF
 4300   CONTINUE
!
        ICASEA='U'
        IU=IU+1
        IF(IU.GT.MAXU2)GO TO 4370
        JUNAM1(IU)=IH1
        JUNAM2(IU)=IH2
        GO TO 4370
!
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
          GO TO 8800
        ENDIF
!
        JENAM1(IE)=IH1
        JENAM2(IE)=IH2
        IECASE(IE)='NEW'
        IF(ICASEA.EQ.'V')IECASE(IE)='OLD'
        IECOL2(IE)=-1
        IF(ICASEA.EQ.'V')IECOL2(IE)=IVALUE(I2)
        IF(ICASEA.EQ.'P')IECASE(IE)='OLD'
        IF(ICASEA.EQ.'F')IECASE(IE)='OLD'
!
        IF(ICASTO.EQ.'ON')GO TO 4215
!
 4200 CONTINUE
 4290 CONTINUE
!
!CCCC FEBRUARY 2003: IF NO VARIABLES GIVEN, THEN WILL
!CCCC DETERMINE AUTOMATICALLY LATER ON.
!
      NUMV=IV
      NUMP=IP
      NUMM=IM
      NUMF=IF
      NUMU=IU
      NUME=IE
!
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
        WRITE(ICOUT,4411)NUMALL,NUMV,NUMP,NUMM,NUMF,NUMU,NUME
 4411   FORMAT('NUMALL,NUMV,NUMP,NUMM,NUMF,NUMU,NUME = ',7I6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4412)
 4412   FORMAT('I,JVNAM1(I),JVNAM2(I),JPNAM1(I),JPNAM2(I),',   &
               'JMNAM1(I),JMNAM2(I),JFNAM1(I),JFNAM2(I),',   &
               'JUNAM1(I),JUNAM2(I)')
        CALL DPWRST('XXX','BUG ')
        DO 4420 I=1,15
          WRITE(ICOUT,4421)I,JVNAM1(I),JVNAM2(I),JPNAM1(I),JPNAM2(I),   &
          JMNAM1(I),JMNAM2(I),JFNAM1(I),JFNAM2(I),JUNAM1(I),JUNAM2(I)
 4421     FORMAT(I8,4X,5(1X,2A4))
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
      IF(ICASRE.EQ.'CLIP' .AND. IVRLST.EQ.'NO')GO TO 7001
!
      IF(NUMV.LT.0 .OR. NUMV.GT.MAXV2)THEN
!
        WRITE(ICOUT,211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,512)
  512   FORMAT('      FOR A READ, THE NUMBER OF VARIABLES (NOT ',   &
               'COUNTING')
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
  518   FORMAT('      NOTE--ONLY THE FIRST ',I8,' VARIABLES WILL BE ',   &
               'READ.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,520)
  520   FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,521)(IANSLC(I),I=1,MIN(120,IWIDTH))
  521     FORMAT(120A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
!
      IF(NUMP.LT.0 .OR. NUMP.GT.MAXP2)THEN
!
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
  538   FORMAT('      NOTE--ONLY THE FIRST ',I8,' PARAMETERS WILL ',   &
               'BE READ.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,520)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,521)(IANSLC(I),I=1,MIN(80,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
!
      IF(NUMM.LT.0 .OR. NUMM.GT.MAXM2)THEN
!
        WRITE(ICOUT,211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,553)
  553   FORMAT('      FOR A READ, THE NUMBER OF MODELS MUST BE AT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,555)MAXM2
  555   FORMAT('      MOST ',I8,'  .  SUCH WAS NOT THE CASE HERE;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,556)NUMM
  556   FORMAT('      THE SPECIFIED NUMBER OF MODELS TO BE READ WAS ',   &
               I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,558)MAXM2
  558   FORMAT('      NOTE--ONLY THE FIRST ',I8,' MODELS WILL BE ',   &
               'READ.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,520)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,521)(IANSLC(I),I=1,MIN(80,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
!
      IF(NUMF.LT.0 .OR. NUMF.GT.MAXF2)THEN
!
        WRITE(ICOUT,211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,572)
  572   FORMAT('      FOR A READ, THE NUMBER OF FUNCTIONS MUST BE AT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,575)MAXF2
  575   FORMAT('      MOST ',I8,'  .  SUCH WAS NOT THE CASE HERE;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,576)NUMF
  576   FORMAT('      THE SPECIFIED NUMBER OF FUNCTIONS TO BE READ ',   &
               'WAS ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,578)MAXF2
  578   FORMAT('      NOTE--ONLY THE FIRST ',I8,' FUNCTIONS WILL BE ',   &
               'READ.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,520)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,521)(IANSLC(I),I=1,MIN(80,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
!
      IF(NUMU.LT.0 .OR. NUMU.GT.MAXU2)THEN
!
        WRITE(ICOUT,211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,612)
  612   FORMAT('      FOR A READ, THE NUMBER OF UNKNOWNS MUST BE AT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,614)MAXU2
  614   FORMAT('      MUST BE AT MOST ',I8,';  SUCH WAS NOT THE CASE ',   &
               'HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,617)NUMU
  617   FORMAT('      THE SPECIFIED NUMBER OF UNKNOWNS TO BE READ WAS ',   &
               I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,618)MAXU2
  618   FORMAT('      NOTE--ONLY THE FIRST ',I8,' UNKNOWNS WILL BE ',   &
               'READ.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,520)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,521)(IANSLC(I),I=1,MIN(80,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
      ENDIF
!
      IF(ICASRE.EQ.'MATZ' .AND. NUME.NE.3)THEN
!
        WRITE(ICOUT,211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,632)
  632   FORMAT('      FOR THE   READ MATRIX TO VARIABLES   CASE, THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,633)
  633   FORMAT('      NUMBER OF VARIABLES TO BE READ MUST BE EXACTLY')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,635)
  635   FORMAT('      THREE.  SUCH WAS NOT THE CASE HERE;  THE ',   &
               'SPECIFIED')
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
        GO TO 9000
      ENDIF
!
      IF(ICASRE.EQ.'STAC' .AND. NUME.NE.2)THEN
!
        WRITE(ICOUT,211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,642)
  642   FORMAT('      FOR THE   READ STACK VARIABLES   CASE, THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,643)
  643   FORMAT('      NUMBER OF VARIABLES TO BE READ MUST BE EXACTLY')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,645)
  645   FORMAT('      TWO.  SUCH WAS NOT THE CASE HERE;  THE ',   &
               'SPECIFIED')
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
        GO TO 9000
      ENDIF
!
      IF(ICASRE.EQ.'IMAZ')THEN
        IF(NUME.NE.3 .AND. NUME.NE.5)THEN
!
          WRITE(ICOUT,211)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,652)
  652     FORMAT('      FOR THE   READ IMAGE TO VARIABLES   CASE, ',   &
                 'THE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,653)
  653     FORMAT('      NUMBER OF VARIABLES TO BE READ MUST BE ',   &
                 'EITHER THREE OR')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,655)
  655     FORMAT('      FIVE.  SUCH WAS NOT THE CASE HERE;  THE ',   &
                 'SPECIFIED')
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
          GO TO 9000
        ENDIF
      ENDIF
!
      IF(ICASRE.EQ.'IMAG')THEN
        IF(NUME.NE.1 .AND. NUME.NE.3)THEN
!
          WRITE(ICOUT,211)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,662)
  662     FORMAT('      FOR THE   READ IMAGE   CASE, THE NUMBER OF')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,663)
  663     FORMAT('      VARIABLES TO BE READ MUST BE EITHER ONE OR')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,665)
  665     FORMAT('      THREE.  SUCH WAS NOT THE CASE HERE;  THE ',   &
                 'SPECIFIED')
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
          GO TO 9000
        ENDIF
      ENDIF
!
      IF(ICASRE.EQ.'ROWR')THEN
!
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
!
        IF(NUME.LT.1)THEN
!
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
          GO TO 9000
        ENDIF
      ENDIF
!
      IF(ICASRE.NE.'ROWI' .AND. NUME.LT.1 .AND. ICNTCH.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4452)
 4452   FORMAT('      NO VARIABLE NAMES WERE PROVIDED IN THE READ ',   &
               'STATEMENT,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4453)
 4453   FORMAT('      HENCE NO READ WAS CARRIED OUT.  ILLUSTRATIVE ',   &
               'EXAMPLE TO')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4455)
 4455   FORMAT('      DEMONSTRATE THE PROPER FORM FOR THE READ ',   &
               'COMMAND--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4456)
 4456   FORMAT('      SUPPOSE THE ANALYST WISHES TO READ DATA FROM ',   &
               'FILE   CALIB.')
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
!               **  WILL BECOME  FUTURE VARIABLES/PARAMETERS/FUNCTIONS.*
!               **  ASSIGN THESE VARIABLES TO THE NEXT AVAILABLE     **
!               **  COLUMNS, AND UPDATE THE NAME TABLE ACCORDINGLY.  **
!               *******************************************************
!
      IF(NUME.GT.0 .AND. ICASRE.NE.'ROWR')THEN
        INAM=NUMNAM
        IF(ICASRE.EQ.'VARI')ICOL=NUMCOL
        IF(ICASRE.EQ.'STAC')ICOL=NUMCOL
        IF(ICASRE.EQ.'MATZ')ICOL=NUMCOL
        IF(ICASRE.EQ.'IMAZ')ICOL=NUMCOL
        IF(ICASRE.EQ.'CLIP')ICOL=NUMCOL
        DO 700 IE=1,NUME
          IF(ICASRE.EQ.'VARI'.AND.IECASE(IE).EQ.'OLD')GO TO 700
          IF(ICASRE.EQ.'PARA'.AND.IECASE(IE).EQ.'OLD')GO TO 700
          IF(ICASRE.EQ.'FUNC'.AND.IECASE(IE).EQ.'OLD')GO TO 700
          IF(ICASRE.EQ.'CFUN'.AND.IECASE(IE).EQ.'OLD')GO TO 700
          IF(ICASRE.EQ.'MATR'.AND.IECASE(IE).EQ.'OLD')GO TO 700
          IF(ICASRE.EQ.'MATZ'.AND.IECASE(IE).EQ.'OLD')GO TO 700
          IF(ICASRE.EQ.'IMAG'.AND.IECASE(IE).EQ.'OLD')GO TO 700
          IF(ICASRE.EQ.'IMAZ'.AND.IECASE(IE).EQ.'OLD')GO TO 700
          IF(ICASRE.EQ.'STAC'.AND.IECASE(IE).EQ.'OLD')GO TO 700
          IF(ICASRE.EQ.'CLIP'.AND.IECASE(IE).EQ.'OLD')GO TO 700
          IF(ICASRE.EQ.'VARI'.AND.IECOL2(IE).GE.1)GO TO 700
          IF(ICASRE.EQ.'STAC'.AND.IECOL2(IE).GE.1)GO TO 700
          IF(ICASRE.EQ.'MATR')GO TO 700
          INAM=INAM+1
          IF(ICASRE.EQ.'VARI')ICOL=ICOL+1
          IF(ICASRE.EQ.'STAC')ICOL=ICOL+1
          IF(ICASRE.EQ.'MATZ')ICOL=ICOL+1
          IF(ICASRE.EQ.'IMAZ')ICOL=ICOL+1
          IF(ICASRE.EQ.'CLIP')ICOL=ICOL+1
!
          IF(INAM.GT.MAXNAM)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,211)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,712)
  712       FORMAT('      THE NUMBER OF NAMES (VARIABLES + PARAMETERS')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,714)
  714       FORMAT('      + FUNCTIONS HAS JUST EXCEEDED THE MAXIMUM ',   &
                   'SIZE')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,715)MAXNAM
  715       FORMAT('      (',I5,') OF THE INTERNAL NAME TABLE.')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 8800
          ENDIF
!
          IF(ICASRE.NE.'PARA' .AND. ICASRE.NE.'FUNC' .AND.   &
             ICASRE.NE.'CFUN' .AND. ICASRE.NE.'ROWI' .AND.   &
             ICOL.GT.MAXCOL)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,211)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,722)
  722       FORMAT('      THE NUMBER OF COLUMNS IN THE INTERNAL ',   &
                   'DATAPLOT DATA')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,724)MAXCOL
  724       FORMAT('      ARRAY HAS JUST EXCEEDED THE ALLOWABLE ',   &
                   'MAXIMUM (',I5,')')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 8800
          ENDIF
!
          IHNAME(INAM)=JENAM1(IE)
          IHNAM2(INAM)=JENAM2(IE)
          IF(ICASRE.EQ.'PARA')IUSE(INAM)='P'
          IF(ICASRE.EQ.'FUNC')IUSE(INAM)='F'
          IF(ICASRE.EQ.'CFUN')IUSE(INAM)='F'
          IF(ICASRE.EQ.'VARI' .OR. ICASRE.EQ.'MATZ' .OR.   &
             ICASRE.EQ.'STAC' .OR. ICASRE.EQ.'IMAZ' .OR.   &
             ICASRE.EQ.'CLIP')THEN
            IUSE(INAM)='V'
            IVALUE(INAM)=ICOL
            IECOL2(IE)=ICOL
            IN(INAM)=0
          ENDIF
  700   CONTINUE
        NUMNAM=INAM
        IF(ICASRE.EQ.'VARI' .OR. ICASRE.EQ.'MATZ' .OR.   &
           ICASRE.EQ.'STAC' .OR. ICASRE.EQ.'IMAZ' .OR.   &
           ICASRE.EQ.'CLIP')NUMCOL=ICOL
!
!       2018/07: IF CONVERTING CHARACTER DATA TO CATEGORICAL DATA,
!                ADD NUMERIC VARIABLE NAMES TO NAME TABLE.
!
        IF(IGRPAU.EQ.'CATE' .AND. ICNTCH.GT.0 .AND.   &
           IRWLC3.NE.ICNTCH)THEN
          DO 70000 IE=1,ICNTCH
            IF(IECOLC(IE).GE.1)GO TO 70000
            INAM=INAM+1
            ICOL=ICOL+1
!
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
              GO TO 8800
            ENDIF
!
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
              GO TO 8800
            ENDIF
!
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
!
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,791)NUMNAM,NUMCOL,NUMNAM,ICASRE
  791   FORMAT('NUMNAM,NUMCOL,NUMNAM,ICASRE = ',3I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!             ********************************************************
!             **  STEP 7--                                          **
!             **  FIRST, BRANCH TO THE APPROPRIATE SUBCASE          **
!             **  (DEPENDING ON WHETHER UNQUALIFIED, SUBSET OR FOR);**
!             **  THE DETERMINE THE LENGTH OF THE LONGEST           **
!             **  VARIABLE TO BE READ    IN ;                       **
!             **  THEN READ IN  THE VARIABLES                       **
!             **  THAT WERE SPECIFIED.                              **
!             ********************************************************
!
 7001 CONTINUE
!
      ISTEPN='7'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      MAXNRD=MAXN
      IF(IREASB.EQ.'P-P ')ICASEQ='FULL'
      IF(ICASEQ.EQ.'SUBS')THEN
        NIOLD=MAXNRD
        CALL DPSUBS(NIOLD,ILOCS,NS,IBUGQ,IERROR)
        NQ2=NIOLD
      ELSEIF(ICASEQ.EQ.'FOR')THEN
        NIOLD=MAXNRD
        CALL DPFOR(NIOLD,NFOR,IROW1,IROWN,   &
                   NLOCAL,ILOCS,NS,IBUGQ,IERROR)
        NQ2=NFOR
      ELSE
        DO 7315 I=1,MAXNRD
          ISUB(I)=1
 7315   CONTINUE
        NQ2=MAXNRD
      ENDIF
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
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IFMFLG.EQ.'ON' .OR. IFROW1.LE.1 .OR. ICASRE.EQ.'IMAZ' .OR.   &
         ICASRE.EQ.'IMAG' .OR. ICASRE.EQ.'CLIP' .OR.   &
         ICASRE.EQ.'CFUN')GO TO 7369
        IFRMIN=1
        IFRMAX=IFROW1-1
        IF(IFRMIN.GT.IFRMAX)GO TO 7369
        MINCO2=1
        MAXCO2=NUMRCM
        IF(IRD2.EQ.IRD)MAXCO2=255
        IFCOL3=IFCOL1
        IFCOL4=IFCOL2
!       THE FOLLOWING 2 LINES WERE INSERTED FEBRUARY 1988
!       TO "TURN OFF" THE    COLUMN LIMITS    IF READING FROM A NON-FILE
!       (THAT IS, IF READING FROM THE TERMINAL OR WITHIN A MACRO).
        IF(IOFILE.EQ.'NO')THEN
          IFCOL3=MINCO2
          IFCOL4=MAXCO2
        ENDIF
        IF(IRD2.EQ.IRD.AND.IFCOL4.GT.MAXCO2)IFCOL4=MAXCO2
!
        DO 7360 IFROW=IFRMIN,IFRMAX
          IF(IOFILE.EQ.'NO')THEN
            READ(IRD2,7361,END=7363,ERR=7363)IJUNK
 7361       FORMAT(A1)
          ELSEIF(IOFILE.EQ.'YES')THEN
            NUMCHA=-1
            CALL DPREFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
                        IA,NUMCHA,   &
                        ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
            IF(IERROR.EQ.'YES')GO TO 8800
            IF(IA(1).EQ.'E'.AND.IA(2).EQ.'O'.AND.IA(3).EQ.'F'.AND.   &
              NUMCHA.EQ.3)GO TO 7363
          ENDIF
          GO TO 7360
!
 7363     CONTINUE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,211)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7365)
 7365     FORMAT('      END OF FILE ENCOUNTERED WHILE SKIPPING OVER',   &
                 'HEADER LINES.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7367)
 7367     FORMAT('      NOTE SKIP AND ROW LIMITS SETTINGS--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7368)ISKIP,IFROW1,AFROW2
 7368     FORMAT('      ISKIP,IFROW1,IFROW2 = ',2I8,2X,G15.7)
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
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IFMFLG.EQ.'ON' .OR. IOFILE.EQ.'NO' .OR. ICASRE.EQ.'IMAZ' .OR.   &
        ICASRE.EQ.'IMAG' .OR. ICASRE.EQ.'CLIP' .OR.   &
        ICASRE.EQ.'CFUN')GO TO 7389
!
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
!
!CCCC OCTOBER 1997.  SUPPORT "SKIP AUTOMATIC", DENOTED BY ISKIP = -1.
!CCCC READ UNTIL FIND "----".  IF "----" IS NOT FOUND, REWIND THE
!CCCC FILE, AND START READ FROM LINE 1.  ALSO, IF READING FROM
!CCCC THE TERMINAL, THEN THIS OPTION DOESN'T MAKE SENSE, SO
!CCCC ASSUME ISKIP = 0 IN THIS CASE.
!
      IF(ISKIP.EQ.-1.AND.IOFILE.EQ.'YES'.AND.   &
         ICASRE(1:3).NE.'IMA')THEN
        IFRMIN=1
        MINCO2=1
        MAXCO2=NUMRCM
        IF(IRD2.EQ.IRD)MAXCO2=255
        IFCOL3=IFCOL1
        IFCOL4=IFCOL2
        IF(IRD2.EQ.IRD.AND.IFCOL4.GT.MAXCO2)IFCOL4=MAXCO2
        DO 7378 I=1,50000
          NUMCHA=-1
          CALL DPREFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
                      IA,NUMCHA,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
!
          IF(IERROR.EQ.'YES')GO TO 8800
          IF(IA(1).EQ.'-'.AND.IA(2).EQ.'-'.AND.IA(3).EQ.'-'.AND.   &
            IA(4).EQ.'-')THEN
            GO TO 7389
          ENDIF
          IF(NUMCHA.GE.5)THEN
            DO 7379 LL=1,NUMCHA-3
              IF(IA(LL).EQ.'-'.AND.IA(LL+1).EQ.'-'.AND.   &
                IA(LL+2).EQ.'-'.AND.IA(LL+3).EQ.'-')THEN
                GO TO 7389
              ENDIF
 7379       CONTINUE
          ENDIF
          IF(IA(1).EQ.'E'.AND.IA(2).EQ.'O'.AND.IA(3).EQ.'F'.AND.   &
            NUMCHA.EQ.3)THEN
            REWIND IOUNIT
            GO TO 7389
          ENDIF
 7378   CONTINUE
      ENDIF
!
      IF(ISKIP.LE.0)GO TO 7389
      IFRMIN=IFROW1
      IFRMAX=IFROW1+ISKIP-1
      IF(IFRMIN.GT.IFRMAX)GO TO 7389
      MINCO2=1
      MAXCO2=NUMRCM
      IF(IRD2.EQ.IRD)MAXCO2=255
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
        IF(IOFILE.EQ.'NO')THEN
          READ(IRD2,7382,END=7383)IJUNK
 7382     FORMAT(A1)
        ELSEIF(IOFILE.EQ.'YES')THEN
          NUMCHA=-1
          CALL DPREFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
                      IA,NUMCHA,   &
                      ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
          IF(IERROR.EQ.'YES')GO TO 8800
          IF(IA(1).EQ.'E'.AND.IA(2).EQ.'O'.AND.IA(3).EQ.'F'.AND.   &
            NUMCHA.EQ.3)GO TO 7383
        ENDIF
        GO TO 7380
!
 7383   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7385)
 7385   FORMAT('      END OF FILE ENCOUNTERED WHILE SKIPPING OVER ',   &
               'HEADER')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7387)
 7387   FORMAT('      LINES.  NOTE SKIP AND ROW LIMITS SETTINGS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7388)ISKIP,IFROW1,AFROW2
 7388   FORMAT('      ISKIP,IFROW1,AFROW2 = ',2I8,2X,E15.7)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 8800
!
 7380 CONTINUE
 7389 CONTINUE
!
!               ************************
!               **  STEP 10--         **
!               **  READ IN THE DATA  **
!               ************************
!
!
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
        ISTEPN='10'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7210)NUME,IRD,IRD2,IFLGSV,IB(1),IB(2)
 7210   FORMAT('NUME,IRD,IRD2,IFLGSV,IB(1),IB(2) = ',4I8,2(2X,A4))
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!     2019/09: DON'T INITIALIZE IB IF READING FIRST LINE WITH
!              POSSIBLY CHARACTER DATA FROM THE TERMINAL.
!
      IF(IFLGSV.EQ.0)THEN
        DO 7260 I=1,MAXRCL
          ISTOR1(I)=' '
          ISTOR2(I)=' '
          ISTOR3(I)=' '
          IB(I)=' '
 7260   CONTINUE
      ELSE
        DO 7261 I=1,MAXRCL
          ISTOR1(I)=' '
          ISTOR2(I)=' '
          ISTOR3(I)=' '
 7261   CONTINUE
      ENDIF
!
      IF(NUME.GT.0)THEN
        DO 7300 I=1,NUME
          IEN(I)=0
 7300   CONTINUE
      ENDIF
!
      MINCO2=1
      MAXCO2=NUMRCM
      IF(IRD2.EQ.IRD)MAXCO2=255
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
      IIN=0
      NUMLRD=0
      IENDTY=1
!CCCC THE FOLLOWING LINE WAS ADDED    JUNE 1990
!CCCC TO FIX FORMATTED READ YIELDING ONLY 1 LINE   JUNE 1990
      IEND='NO'
      IF(ISKIP.GE.0)THEN
        IFRMIN=IFROW1+ISKIP
        IF(ICASEQ.EQ.'FOR')IFRMIN=IFROW1+ISKIP+IROW1-1
      ELSE
        IFRMIN=1
        IF(ICASEQ.EQ.'FOR')IFRMIN=IROW1
      ENDIF
!
!CCCC OCTOBER 2004: ACCOUNT FOR SUBSET/FOR CLAUSE LIMITS
!
      IFRMAX=IFROW2
      IF(ICASEQ.EQ.'FOR')THEN
        IFRMAX=MIN(IFROW2,IROWN)
      ENDIF
!
      IF(ICASRE.EQ.'PARA' .OR. ICASRE.EQ.'FUNC')IFRMAX=IFRMIN
      IF(IHOST1.EQ.'CDC'.AND.IFRMAX.GT.130000)IFRMAX=130000
      IF(IFRMAX.GE.IBILLI)IFRMAX=IBILLI
      IF(IFRMIN.GT.IFRMAX)GO TO 7470
!CCCC APRIL 1995.  CHECK FOR UNFORMATTED READ CASE.
!CCCC INITIAL IMPLEMENTATION ONLY APPLIES TO VARIABLES (NOT
!CCCC STRINGS, FUNCTIONS, MATRICES).
!CCCC 1) THE FOLLOWING COMMAND:
!CCCC        SET UNFORMATTED COLUMNS <N>
!CCCC    SPECIFIES THE NUMBER OF COLUMNS WHEN READING A MATRIX
!CCCC 2) UNFORMATTED READ ASSUMES A "SQUARE MATRIX" OF NUMBERS
!CCCC    CONTAINING ONLY REAL NUMBERS WAS WRITTEN (THAT IS, ASSUME
!CCCC    A SINGLE WRITE PERFORMED, NOT A MIXTURE OF DIFFERENT TYPES
!CCCC    ETC.).  THE FOLLOWING 2 COMMANDS PROVIDE A LIMITED AMOUNT
!CCCC    OF FLEXIBILITY:
!CCCC        SET UNFORMATTED OFFSET <VALUE>
!CCCC        SET UNFORMATTED RECORDS <VALUE>
!CCCC    THE FIRST COMMAND SPECIFIES THE NUMBER OF DATA VALUES TO
!CCCC    SKIP AT THE BEGINING OF THE FILE.  THE SECOND COMMAND
!CCCC    SPECIFIES THE NUMBER OF DATA VALUES TO READ.
!CCCC 3) THERE ARE ESSENTIALLY 2 WAYS TO CREATE THE UNFORMATTED
!CCCC    FILE.  FOR EXAMPLE, ASSUME WRITING 10,000 ROWS OF VARIABLES
!CCCC    X AND Y.  THEN CAN WRITE AS:
!CCCC    A)    WRITE(IUNIT) X,Y
!CCCC    B)    WRITE(IUNIT) (X(I),Y(I),I=1,N)
!CCCC    THE DISTINCTION IS THAT (A) WRITES ALL OF X AND THEN ALL OF
!CCCC    Y WHEREAS (B) WRITES X(1), Y(1), X(2), Y(2), ..., X(N), Y(N).
!CCCC    INITIAL IMPLEMENTATION ASSUMES (B) SINCE THIS CORRESPONDS
!CCCC    TO DATAPLOT'S STORING BY COLUMN.  THE
!CCCC    "SET READ UNFORMATTED-COLUMNWISE" COMMAND SPECIFIES THAT
!CCCC    METHOD (A) WAS USED TO CREATE THE FILE.
!CCCC DATAPLOT WILL READ ENTIRE UNFORMATTED FILE INTO "XSCRT"
!CCCC ARRAY.  IT WILL CHECK HOW MANY DATA VALUES WERE READ.  IT THEN
!CCCC DIVIDES THIS BY NUMBER OF VARIABLES TO BE READ.  THE DO7400
!CCCC LOOP BELOW THEN EXTRACTS EACH ROW OF DATA FROM THIS XSCRT
!CCCC ARRAY.
!
      IF(IFMFLG.EQ.'ON'.AND.ICASRE.NE.'IMAZ'.AND.ICASRE.NE.'IMAG')THEN
!
        IF(IUNFOF.GT.2*MAXOBV)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,11212)IUNFOF,2*MAXOBW
11212     FORMAT('****** ERROR: OFFSET OF ',I8,' IS GREATER THAN ',   &
                 'MAXIMUM ALLOWED OF ',I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        DO 11002 JJ=1,3*MAXOBW
          XSCRT(JJ)=CPUMIN
11002   CONTINUE
!
        IF(ICASRE.EQ.'MATR')NUME=IUNFMC
!
!CCCC   JULY 1996.  SGI DOESN'T READ IF XSCRT DIMENSIONED BIGGER
!CCCC   THAN NUMBER OF DATA POINTS IN FILE.  USER MAY NEED TO SPECIFY
!CCCC   THE COMMAND "SET UNFORMATTED RECORDS <N>".
!
        IF(IUNFNR.GT.0)THEN
          READ(IRD2,ERR=11080,END=11090,IOSTAT=JSTATS)   &
          (XSCRT(LL),LL=1,IUNFNR+IUNFOF)
        ELSE
          READ(IRD2,ERR=11080,END=11090,IOSTAT=JSTATS)XSCRT
        ENDIF
        GO TO 11090
!
11080   CONTINUE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,11081)JSTATS
11081     FORMAT('****** ERROR TRYING TO READ AN UNFORMATTED FILE, ',   &
                 'STATUS NUMBER = ',I8,'.')
          CALL DPWRST('XXX','BUG ')
        GO TO 11090
!
11090   CONTINUE
        NSTOP=MAXOBW+IUNFOF
        IF(IUNFNR.GT.0)NSTOP=IUNFNR+IUNFOF
        DO 11100 JJ=NSTOP,1,-1
          NPTS=JJ
          IF(XSCRT(JJ).NE.CPUMIN)GO TO 11109
11100   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,11111)
11111   FORMAT('****** ERROR: NO DATA FOUND IN THE UNFORMATTED FILE.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
11109   CONTINUE
        NPTS=NPTS-IUNFOF
        IFRMIN=1
        IFRMAX=NPTS/NUME
!
!CCCC OCTOBER 2014.  CHECK FOR READ FROM CLIPBOARD CASE.
!CCCC                DATAPLOT WILL READ ALL VALUES IN THE CLIPBORARD TO
!CCCC                THE "XSCRT" ARRAY.  IT WILL RETURN HOW MANY DATA
!CCCC                VALUES WERE READ.  IT THEN DIVIDES THIS BY NUMBER
!CCCC                OF VARIABLES TO BE READ.  THE DO7400 LOOP BELOW
!CCCC                THEN EXTRACTS EACH ROW OF DATA FROM THIS XSCRT
!CCCC                ARRAY.
!
      ELSE IF(ICASRE.EQ.'CLIP')THEN
!
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
          WRITE(ICOUT,11203)
11203     FORMAT('BEFORE CALL DPCLIP')
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        IRTYPE='VARI'
        MAXVAL=3*MAXOBW
        NUMETT=NUME
        ISKIPT=ISKIP
        IF(ICLISK.EQ.'OFF')ISKIPT=0
        CALL DPCLIP(XSCRT,MAXVAL,NPTS,NUMETT,NUMVLN,PREAMV,ISKIPT,   &
                    IGRPAU,   &
                    IVLIST,IVLIS2,IAVANM,MAXRDV,   &
                    IRTYPE,ISTRZZ,NCSTR,IEOF,   &
                    IBUGS2,ISUBRO,IERROR)
!
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
          WRITE(ICOUT,11205)NPTS,NUMETT,IERROR
11205     FORMAT('AFTER CALL DPCLIP: NPTS,NUMETT,IERROR = ',2I10,2X,A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        IF(NPTS.LE.0 .OR. IERROR.EQ.'YES')GO TO 9000
!
        IF(NUME.EQ.0 .AND. NUMETT.GT.0)THEN
          NUME=NUMETT
          NUMVRD=NUMETT
          ICOL=NUMCOL
          INAM=NUMNAM
          IV=0
          IU=0
          IE=0
!
          NCBASE=0
          DO 58590 II=8,1,-1
            IF(IAVABN(II:II).NE.' ')THEN
              NCBASE=II
              GO TO 58599
            ENDIF
58590     CONTINUE
58599     CONTINUE
!
          DO 5893 J=1,NUMETT
            IF(IAVANM.EQ.'FILE')THEN
              IVTEMP(1:4)=IVLIST(J)(1:4)
              IVTEMP(5:8)=IVLIS2(J)(1:4)
              DO 5895 JJ=1,8
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
!
!           CHECK AGAINST VARIABLE LIST.  NOTE THAT READ CLIPBOARD IS
!           CURRENTLY RESRICTED TO READING VARIABLES (I.E., NO STRINGS,
!           PARAMETERS, OR MATRICES).
!
            ICASEA='    '
            DO 5810 I=1,NUMNAM
              I2=I
              IF(IVTEMP(1:4).EQ.IHNAME(I)(1:4).AND.   &
                IVTEMP(5:8).EQ.IHNAM2(I)(1:4))THEN
                IF(IUSE(I).EQ.'V')THEN
                  ICASEA='V'
                  IV=IV+1
                  IF(IV.GT.MAXV2)THEN
                    WRITE(ICOUT,999)
                    CALL DPWRST('XXX','BUG ')
                    WRITE(ICOUT,211)
                    CALL DPWRST('XXX','BUG ')
                    WRITE(ICOUT,5512)
 5512               FORMAT('      THE NUMBER OF VARIABLES DETECTED ',   &
                           'FROM THE READ CLIPBOARD COMMAND HAS')
                    CALL DPWRST('XXX','BUG ')
                    WRITE(ICOUT,5514)MAXV2
 5514               FORMAT('      EXCEEDED THE MAXIMUM OF ',I10)
                    CALL DPWRST('XXX','BUG ')
                    IERROR='YES'
                    GO TO 8800
                  ENDIF
                  JVNAM1(IV)=IVTEMP(1:4)
                  JVNAM2(IV)=IVTEMP(5:8)
                  NIV(IV)=IN(I2)
                  GO TO 5870
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
                  GO TO 8800
                ENDIF
              ENDIF
 5810       CONTINUE
!
            ICASEA='U'
            IU=IU+1
            IF(IU.GT.MAXU2)GO TO 5870
            JUNAM1(IU)=IVTEMP(1:4)
            JUNAM2(IU)=IVTEMP(5:8)
            GO TO 5870
!
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
              GO TO 8800
            ENDIF
!
            JENAM1(IE)=IVTEMP(1:4)
            JENAM2(IE)=IVTEMP(5:8)
            IF(ICASEA.EQ.'V')THEN
              IECASE(IE)='OLD'
              IECOL2(IE)=IVALUE(I2)
            ELSE
              IECASE(IE)='NEW'
!
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
                GO TO 8800
              ENDIF
!
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
                GO TO 8800
              ENDIF
!
              IF(IECASE(IE).EQ.'NEW')THEN
                IHNAME(INAM)=JENAM1(IE)
                IHNAM2(INAM)=JENAM2(IE)
                IUSE(INAM)='V'
                IVALUE(INAM)=ICOL
                IN(INAM)=0
                IECOL2(IE)=ICOL
              ENDIF
!
            ENDIF
!
 5893     CONTINUE
          NUMV=IV
          NUMU=IU
          NUME=IE
          NUMCOL=ICOL
          NUMNAM=INAM
!
        ENDIF
!
!CCCC OCTOBER 2014.  CHECK FOR READ STRING FROM CLIPBOARD CASE.
!
      ELSE IF(ICASRE.EQ.'CFUN')THEN
!
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
          WRITE(ICOUT,11203)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        ISTRZZ=' '
        IRTYPE='STRI'
        MAXVAL=3*MAXOBW
        NUMETT=0
        IEOF=0
!
!       LOOP THROUGH STRINGS
!
        DO 11301 II=1,NUME
          ISKIPT=ISKIP
          IF(ICLISK.EQ.'OFF')ISKIPT=0
          ISKIPT=ISKIPT+II-1
          CALL DPCLIP(XSCRT,MAXVAL,NPTS,NUMETT,NUMVLN,PREAMV,ISKIPT,   &
                      IGRPAU,   &
                      IVLIST,IVLIS2,IAVANM,MAXRDV,   &
                      IRTYPE,ISTRZZ,NCSTR,IEOF,   &
                      IBUGS2,ISUBRO,IERROR)
!
          IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
            WRITE(ICOUT,11305)NCSTR,ISTRZZ
11305       FORMAT('NCSTR,ISTRZZ = ',I5,255A1)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
          IF(NCSTR.LE.0)THEN
            ISTRZZ='NULL'
            NCSTR=4
          ENDIF
!
          DO 77801 KK=1,NCSTR
            IFUNC2(KK)=' '
            IFUNC2(KK)(1:1)=ISTRZZ(KK:KK)
77801     CONTINUE
!
          CALL DPUPPE(IFUNC2,NCSTR,IFUNC3,IBUGS2,IERROR)
          ISTART=IFCOL1
          ISTOP=N2
          IH1=JENAM1(II)
          IH2=JENAM2(II)
          DO 77820 J=1,NUMNAM
            IF(IUSE(J).EQ.'F'.AND.   &
              IHNAME(J).EQ.IH1.AND.IHNAM2(J).EQ.IH2)THEN
              NEWNAM='NO'
              IF(IECASE(II).EQ.'NEW')NEWNAM='YES'
              ILISTL=J
!
              CALL DPINFU(IFUNC3,NCSTR,IHNAME,IHNAM2,IUSE,IN,   &
                          IVSTAR,IVSTOP,   &
                          NUMNAM,IANSLC,IWIDTH,IH1,IH2,ILISTL,   &
                          NEWNAM,MAXNME,   &
                          IFUNC,NUMCHF,MAXCHF,IBUGS2,IERROR)
!
              IF(NEWNAM.EQ.'YES'.AND.IERROR.EQ.'NO')NUMNAM=NUMNAM-1
!
            ENDIF
77820     CONTINUE
11301   CONTINUE
!
        GO TO 7900
!
      ENDIF
!
!CCCC OCTOBER 2004: SUBSET/FOR/EXPECT CLAUSES ON READ HAVE SOME
!CCCC AMBIGUITY.  THAT IS, DOES THE SUBSET REFER TO THE LINES THAT
!CCCC ARE READ FROM THE FILE OR DOES THE SUBSET REFER TO HOW THE
!CCCC DATA ARE SAVED IN THE OUTPUT VECTORS.  WE ADDRESS THIS WITH
!CCCC THE COMMAND
!CCCC
!CCCC     SET READ SUBSET  <PACK/DISPERSE>   <PACK/DISPERSE>
!CCCC
!CCCC THE FIRST SETTING SPECIFIES HOW THE DATA FILE IS HANDLED
!CCCC (PACK MEANS SUBSET/FOR CLAUSE DOES NOT APPLY TO LINES IN
!CCCC FILE WHILE DISPERSE MEANS THAT IT DOES).  LIKEWISE, THE SECOND
!CCCC SETTING SPECIFIES HOW THE SUBSET/FOR CLAUSE APPLIES TO THE
!CCCC OUTPUT VARIABLES (PACK MEANS SUBSET IGNORED ON OUTPUT VECTOR,
!CCCC DISPERSE MEAMS THAT IT DOES).  THESE SETTINGS ARE CODED AS
!CCCC   "P-D", "P-P", "D-P", "D-D".  THE DEFAULT IS "P-D" (I.E.,
!CCCC THE SUBSET APPLIES TO THE OUTPUT VECTORS, BUT NOT THE INPUT
!CCCC FILE).  FOR EXAMPLE, THE COMMAND
!CCCC
!CCCC            READ X  FOR I = 1  2  10
!CCCC
!CCCC     X      P-D       P-P          D-P      D-D
!CCCC    ===========================================
!CCCC     1       1         1            1        1
!CCCC     2       0         2            3        0
!CCCC     3       2         3            5        3
!CCCC     4       0         4            7        0
!CCCC     5       3         5            9        5
!CCCC     6       0         -            -        0
!CCCC     7       4         -            -        7
!CCCC     8       0         -            -        0
!CCCC     9       5         -            -        9
!CCCC    10       0         -            -        0
!
!
      IF(ICASRE.EQ.'IMAZ' .OR. ICASRE.EQ.'IMAG')THEN
        IFRMIN=1
        IFRMAX=IYSIZE
      ENDIF
!
      IF(ICASRE.EQ.'CLIP')THEN
        IFRMIN=1
!CCCC   IFRMAX=NPTS/NUME
!CCCC   IREM=MOD(NPTS,NUME)
        IFRMAX=NPTS/NUMVLN
        IREM=MOD(NPTS,NUMVLN)
        IF(IREM.GT.0)IFRMAX=IFRMAX+1
      ENDIF
!
      NCALL=0
      I=0
      IMAXRW=IFRMAX-IFRMIN+1
      DO 7400 IFROW=IFRMIN,IFRMAX
!
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
          WRITE(ICOUT,7401)ICASRE,IFROW,IMNVAR,IMXVAR,NCREAF
 7401     FORMAT('AT 7400: ICASRE,IFROW,IMNVAR,IMXVAR,NCREAF = ',   &
                 A4,2X,4I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        IF(ICASRE.EQ.'IMAZ' .OR. ICASRE.EQ.'IMAG')THEN
          NUMLRD=NUMLRD+1
          IROWXX=IFROW
          IRED=1
          IGREEN=1
          IBLUE=1
          DO 74001 JJ=1,IXSIZE
            ICOLXX=JJ
            IXTEMP=ICOLXX
!CCCC       IYTEMP=IROWXX
            IYTEMP=IYSIZE - IROWXX
#ifdef HAVE_GD
            CALL GDPIXE(IXTEMP,IYTEMP,IRED,IGREEN,IBLUE)
#endif
            X0(ICOLXX)=REAL(IRED)
            X0(IXSIZE + ICOLXX)=REAL(IGREEN)
            X0(2*IXSIZE + ICOLXX)=REAL(IBLUE)
74001     CONTINUE
          NUMDPL=3*IXSIZE
          GO TO 7440
        ENDIF
!
        IIN=IIN+1
        IF(ISUB(IIN).NE.1 .AND. ICASRE.NE.'CLIP')THEN
          IF(IREASB(1:1).EQ.'D')THEN
            IF(IREASB(3:3).EQ.'D')THEN
              I=I+1
            ENDIF
            NUMCHA=-1
            CALL DPREFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
                        IA,NUMCHA,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
            IF(IA(1).EQ.'E'.AND.IA(2).EQ.'O'.AND.IA(3).EQ.'F'.AND.   &
               NUMCHA.EQ.3)THEN
               REWIND IOUNIT
               IENDTY=1
               GO TO 7490
            ENDIF
            GO TO 7400
          ENDIF
        ENDIF
!
        IF(ICASRE.NE.'CLIP' .AND. NCREAF.LE.0 .OR. ICASRE.EQ.'FUNC' .OR.   &
           ICASRE.EQ.'ROWI')THEN
          NXCSAV=NXC
          CALL DPREAL(IRD2,IFCOL3,IFCOL4,MINCO2,MAXCO2,X0,NUMDPL,IFLGSV, &
                      IXC,NXC,                                           &
                      ICASRE,IFUNC2,N2,MAXN2,                            &
                      IMACRO,IMACNU,IMACCS,                              &
                      IANSLC,IWIDTH,IREACS,ISTOR1,ISTOR2,IEND,NUMLRD,    &
                      IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,      &
                      ICOMCH,ICOMSW,LINETY,IGRPA2,                       &
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
!
          IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
            WRITE(ICOUT,7402)LINETY,IEND,IERROR,IFROW,IFRMIN,NUMDPL
 7402       FORMAT('AFTER DPREAL (7402): LINETY,IEND,IERROR,IFRMIN,',   &
                   'IFROW,NUMDPL = ',3(A4,2X),3I8)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
          IF(LINETY.EQ.'BLAN')GO TO 7400
          IF(IERROR.EQ.'YES')GO TO 9000
          IF(ICASRE.NE.'ROWR' .AND. IEND.EQ.'NO')THEN
            IF(IMNVAR.EQ.-1)THEN
              IMNVAR=NUMDPL
            ELSE
              IF(NUMDPL.LT.IMNVAR)IMNVAR=NUMDPL
            ENDIF
            IF(NUMDPL.GT.IMXVAR)IMXVAR=NUMDPL
          ENDIF
!
          IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
            WRITE(ICOUT,7403)IMNVAR,IMXVAR
 7403       FORMAT('IMNVAR,IMXVAR = ',2I8)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
          NUMLRD=NUMLRD+1
          NCALL=NCALL+1
!
!  HANDLE "ROW READ" SEPARATELY
!
          IF(ICASRE.EQ.'ROWR')THEN
!
            IF(IEND.EQ.'YES')THEN
              IVBASE=IVBASV
              NUMDPL=NUMDSV
              NUMLRD=NUMLRD-1
              GO TO 8800
            ENDIF
!
            IF(IOFILE.EQ.'YES')THEN
              IVBASE(1:4)=IHARG(3)(1:4)
              IVBASE(5:8)=IHARG2(3)(1:4)
            ELSE
              IVBASE(1:4)=IHARG(2)(1:4)
              IVBASE(5:8)=IHARG2(2)(1:4)
            ENDIF
            IVLAST=8
            DO 22111 LL=8,1,-1
              IF(IVBASE(LL:LL).NE.' ')THEN
                IVLAST=LL
                GO TO 22119
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
!
            IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
              WRITE(ICOUT,22101)NUMLRD,NUMDPL,IVLAST,IVBASE
22101         FORMAT('NUMLRD,NUMDPL,IVLAST,IVBASE = ',3I6,2X,A8)
              CALL DPWRST('XXX','BUG ')
            ENDIF
!
            DO 55810 I=1,NUMNAM
              I2=I
!
!             PRE-EXISTING VARIABLE NAME FOUND
!
              IF(IVBASE(1:4).EQ.IHNAME(I)(1:4).AND.   &
                IVBASE(5:8).EQ.IHNAM2(I)(1:4))THEN
                IF(IUSE(I).EQ.'V')THEN
                  ICASEA='V'
                  ICOL=IVALUE(I2)
                  GO TO 55870
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
                  GO TO 7400
                ENDIF
              ENDIF
55810       CONTINUE
!
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
              GO TO 8800
            ENDIF
!
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
              GO TO 8800
            ENDIF
!
            IHNAME(NUMNAM)=IVBASE(1:4)
            IHNAM2(NUMNAM)=IVBASE(5:8)
!
55870       CONTINUE
!
            IF(NUMDPL.GT.MAXN)THEN
              NUMDPL=MAXN
              WRITE(ICOUT,44318)NUMLRD,MAXN
44318         FORMAT('ROW READ: FOR LINE ',I6,' THE NUMBER ',   &
                     'OF VALUES TRUNCATED AT ',I10)
              CALL DPWRST('XXX','BUG ')
            ENDIF
!
            DO 55880 II=1,NUMDPL
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
!
            IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
              WRITE(ICOUT,55802)NUMLRD,NUMDPL,IVLAST,IVBASE
55802         FORMAT('I2,ICOL,IUSE(I2),IVALUE(I2),IN(I2) = ',   &
                     2I10,I6,2X,A8)
              CALL DPWRST('XXX','BUG ')
            ENDIF
!
            IF(IFEEDB.EQ.'ON' .AND. NUMLRD.EQ.1)THEN
!CCCC       IF(IFEEDB.EQ.'ON')THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,55805)NUMLRD,IVBASE,NUMDPL
55805         FORMAT('ROW READ: ROW ',I10,' READ AS ',A8,' WITH ',I10,   &
                     ' OBSERVATIONS READ')
              CALL DPWRST('XXX','BUG ')
            ENDIF
!
            IVBASV=IVBASE
            NUMDSV=NUMDPL
            GO TO 7400
          ENDIF
!
!  IF CHARACTER DATA ENCOUNTERED, WRITE IT TO FILE
!
!  2019/09: WRITE RESULTS TO "dpst2f.dat" INITIALLY.
!
          IF(NXC.GT.0 .AND.   &
            (IGRPAU.EQ.'CHAR' .OR. IGRPAU.EQ.'CATE'))THEN
            IF(NUMLRD.EQ.1)THEN
!
!CCCC         IOUNI2=IZCHNU
!CCCC         IFILE2=IZCHNA
!CCCC         ISTAT2=IZCHST
!CCCC         IFORM2=IZCHFO
!CCCC         IACCE2=IZCHAC
!CCCC         IPROT2=IZCHPR
!CCCC         ICURS2=IZCHCS
!
!CCCC         ISUBN0='READ'
!CCCC         IERRFI='NO'
!CCCC         CALL DPOPFI(IOUNI2,IFILE2,ISTAT2,IFORM2,IACCE2,IPROT2,
!CCCC1                    ICURS2,
!CCCC1                    IREWI2,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
!CCCC         IF(IERROR.EQ.'YES')GO TO 9000
!
              IOP='OPEN'
              IFLG11=0
              IFLG21=1
              IFLG31=0
              IFLAG4=0
              IFLAG5=0
              CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,   &
                          IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                          IBUGS2,ISUBRO,IERROR)
              IF(IERRFI.EQ.'YES')GO TO 9000
!
              IZCHCS=ICURS2
!
              WRITE(IOUNI2,'(I8)')NXC
              DO 27810 ICNT=1,MIN(NXC,MAXCHV)
                WRITE(IOUNI2,'(A4,A4)')ICLIST(ICNT),ICLIS2(ICNT)
27810         CONTINUE
            ENDIF
            WRITE(IOUNI2,'(20(A24,1X))')(IXC(J)(1:24),J=1,NXC)
!
!           IF "SET CONVERT CHARACTER CATEGORICAL" GIVEN, THEN
!           CREATE A NUMERIC VARIABLE AS WELL.
!
            IF(IGRPAU.EQ.'CATE')THEN
              DO 27820 J=1,NXC
                NTEMP=IXCATN(J)
                IF(NTEMP.LT.1)THEN
                  IXCATN(J)=1
                  IXCAT(1,J)(1:24)=IXC(J)(1:24)
                  X0CAT(J)=1.0
                ELSE
                  DO 27830 II=1,NTEMP
                    IF(IXC(J)(1:24).EQ.IXCAT(II,J)(1:24))THEN
                      X0CAT(J)=REAL(II)
                      GO TO 27820
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
!
          ENDIF
        ELSEIF(IFMFLG.EQ.'ON')THEN
!
          NUMLRD=NUMLRD+1
          IF(IUNFNR.GT.0.AND.NUMLRD*NUME.GT.IUNFNR)GO TO 7400
          NUMDPL=NUME
          IF(ICRFLG.EQ.'ROW')THEN
            IPTR1=(NUMLRD-1)*NUME+1+IUNFOF
            IPTR2=IPTR1+NUME-1
            ICOUNT=0
            DO 17415 JJ=IPTR1,IPTR2
              ICOUNT=ICOUNT+1
              X0(ICOUNT)=XSCRT(JJ)
17415       CONTINUE
          ELSE
            IPTR1=NUMLRD+IUNFOF
            IPTR2=IFRMAX
            DO 17515 JJ=1,NUME
              ICOUNT=IPTR1+(JJ-1)*IPTR2
              X0(JJ)=XSCRT(ICOUNT)
17515       CONTINUE
          ENDIF
        ELSEIF(ICASRE.EQ.'CLIP')THEN
!
          NUMLRD=NUMLRD+1
          NUMDPL=NUME
!
!         2020/02: SET POINTER BASED ON MANY VALUES READ FROM
!                  CLIPBOARD RATHER THAN THE NUMBER OF VARIABLES
!                  USER REQUESTED.
!
!CCCC     IPTR1=(NUMLRD-1)*NUME+1
          IPTR1=(NUMLRD-1)*NUMVLN+1
          IPTR2=IPTR1+NUME-1
          ICOUNT=0
          DO 27415 JJ=IPTR1,IPTR2
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
            IF(IAJUNK(1:1).EQ.ICOMCH(1:1))GO TO 7417
            BACKSPACE(UNIT=IRD2,IOSTAT=IOS,ERR=7413)
            GO TO 7415
 7413       CONTINUE
            WRITE(ICOUT,743)
 743        FORMAT('ERROR TRYING TO BACKSPACE FILE ON FORMATTED READ')
            CALL DPWRST('XXX','BUG ')
            GO TO 7417
          ENDIF
          READ(IRD2,ICREAF,END=7480,ERR=7480)(X0(K),K=1,NUME)
          GO TO 7415
        ENDIF
!
 7415   CONTINUE
        IF(IERROR.EQ.'YES')GO TO 8800
        IF(IFROW.EQ.IFRMIN .AND.ICASRE.NE.'CLIP')THEN
          DO 7425 K=1,132
            ISTOR3(K)=ISTOR2(K)
 7425     CONTINUE
          GO TO 7430
        ENDIF
        IF(IEND.EQ.'YES')GO TO 7480
!
 7430   CONTINUE
        I=I+1
!
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7431)
 7431     FORMAT('***** FROM THE MIDDLE OF DPREAD--')
          CALL DPWRST('XXX','BUG ')
          AFRMAX=IFRMAX
          WRITE(ICOUT,7432)IFROW,IFRMIN,AFRMAX,IBUGS2,ISUBRO
 7432     FORMAT('IFROW,IFRMIN,AFRMAX,IBUGS2,ISUBRO = ',2I8,E15.7,   &
                 2X,A4,2X,A4)
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
!
!CCCC   OCTOBER 2004: IS OUTPUT VECTOR PACKED OR DISPERSED?
!
        IF(I.GT.MAXN .OR. I.GT.IMAXRW)GO TO 7480
        IJUNK=I
        IF(IREASB(3:3).EQ.'P' .AND. IREASB(1:1).EQ.'D')IJUNK=IIN
        IF(ISUB(IJUNK).EQ.1)THEN
          GO TO 7440
        ELSE
          IF(IREASB(3:3).EQ.'D')THEN
            GO TO 7430
          ELSE
            GO TO 7400
          ENDIF
        ENDIF
!
 7440   CONTINUE
        IF(ICASRE.EQ.'PARA')THEN
          NUMPRD=NUME
          GO TO 7400
        ELSEIF(ICASRE.EQ.'FUNC')THEN
           NUMFRD=NUME
           GO TO 7400
        ELSEIF(ICASRE.EQ.'MATZ')THEN
!
!         IMPLEMENT THE "MATRIX TO VARIABLES" CASE.  THE
!         FIRST VARIABLE WILL CONTAIN THE MATRIX VALUES,
!         THE SECOND VARIABLE WILL CONTAIN THE ROW-ID, AND
!         THE THIRD VARIABLE WILL CONTAIN THE COLUMN-ID.
!
          NROWZ=NROWZ+1
          NCOLZ=0
          IE2=0
          IF(NUMDPL.LE.0)GO TO 17448
          DO 17445 IE=1,NUMDPL
            IE2=IE
            Z0=X0(IE)
!
!           COLUMN 1: DATA VALUES
!
            NCOLZ=NCOLZ+1
            ITOTZ=ITOTZ+1
!
            IF(ITOTZ.GT.MAXN)THEN
              WRITE(ICOUT,17481)
17481         FORMAT('****** ERROR IN DPREAD--READ MATRIX TO ',   &
                     'VARIABLES')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,17482)NROWZ
17482         FORMAT('       IN ROW ',I10,' OF THE DATA MATRIX,')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,17483)MAXN
17483         FORMAT('       THE MAXIMUM ROW SIZE ',I10,   &
                     ' EXCEEDED.')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,17484)
17484         FORMAT('       NO ADDITIONAL DATA WILL BE READ.')
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GO TO 7490
            ENDIF
!
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
!
!           COLUMN 2: ROW-ID
!
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
!
!           COLUMN 3: COLUMN-ID
!
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
!
17445     CONTINUE
          NUMVRD=3
          GO TO 7400
17448     CONTINUE
          GO TO 7490
!
        ELSEIF(ICASRE.EQ.'IMAZ')THEN
!
!         IMPLEMENT THE "IMAGE TO VARIABLES" CASE.  THE
!         FIRST VARIABLE WILL CONTAIN THE RED COMPONENT,
!         THE SECOND VARIABLE WILL CONTAIN THE GREEN COMPONENT,
!         AND THE THIRD VARIABLE WILL CONTAIN THE BLUE COMPONENT.
!         NOTE THAT ONE ROW OF THE IMAGE IS READ, SO THERE WILL
!         BE 3*IXSIZE DATA POINTS (NOTE THAT ALL THE RED COMPONENT
!         VALUES ARE STORED, THEN ALL THE GREEN, THEN ALL THE BLUE).
!
!         IF THREE VARIABLES WERE GIVEN, COLUMN 1 IS THE RED
!         COMPONENT (I.E., GREY SCALE), COLUMNS 2 IS THE COLUMN-ID,
!         AND COLUMN 3 IS THE ROW-ID.  IF FIVE VARIABLES WERE GIVEN,
!         COLUMN 1 IS THE RED COMPONENT, COLUMN 2 IS THE GREEN
!         COMPONENT, COLUMN 3 IS THE BLUE COMPONENT, COLUMN 4 IS THE
!         COLUMN-ID, AND COLUMN 5 IS THE ROW-ID.
!
          NROWZ=NROWZ+1
          NCOLZ=0
          IE2=0
          IF(NUMDPL.LE.0)GO TO 17548
          NLAST=NUMDPL/3
          DO 17545 IE=1,NLAST
            IE2=IE
            ZR=X0(IE)
            ZG=X0(IXSIZE + IE)
            ZB=X0(2*IXSIZE + IE)
!
!           COLUMN 1: RED COMPONENT
!
            NCOLZ=NCOLZ+1
            ITOTZ=ITOTZ+1
!
            IF(ITOTZ.GT.MAXN)THEN
              WRITE(ICOUT,17581)
17581         FORMAT('****** ERROR IN DPREAD--READ IMAGE TO ',   &
                     'VARIABLES')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,17582)NROWZ
17582         FORMAT('       IN ROW ',I10,' OF THE DATA IMAGE,')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,17583)MAXN
17583         FORMAT('       THE MAXIMUM ROW SIZE ',I10,   &
                     ' EXCEEDED.')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,17584)
17584         FORMAT('       NO ADDITIONAL DATA WILL BE READ.')
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GO TO 7490
            ENDIF
!
!           COLUMN 1: RED COMPONENT
!
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
!
            IF(NUME.EQ.5)THEN
!
!             COLUMN 2: GREEN COMPONENT
!
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
!
!             COLUMN 3: BLUE COMPONENT
!
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
!
              INEXT=4
            ELSE
              INEXT=2
            ENDIF
!
!           COLUMN 2 OR 4: ROW-ID
!
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
!
!           COLUMN 3 OR 5: COLUMN-ID
!
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
!
17545     CONTINUE
          NUMVRD=INEXT
          GO TO 7400
17548     CONTINUE
          GO TO 7490
!
        ELSEIF(ICASRE.EQ.'STAC')THEN
!
!         IMPLEMENT THE "STACK VARIABLES" CASE.  THE
!         FIRST VARIABLE WILL CONTAIN THE RESPONSE VALUES
!         AND THE SECOND VARIABLE WILL CONTAIN A GROUP-ID
!         VARIABLE.
!
          NROWZ=NROWZ+1
          NCOLZ=0
          IE2=0
          IF(NUMDPL.LE.0)GO TO 18448
          DO 18445 IE=1,NUMDPL
            IE2=IE
            Z0=X0(IE)
!
!           COLUMN 1: DATA VALUES
!
            NCOLZ=NCOLZ+1
            ITOTZ=ITOTZ+1
!
            IF(ITOTZ.GT.MAXN)THEN
              WRITE(ICOUT,18481)
18481         FORMAT('****** ERROR IN DPREAD--READ STACK ',   &
                     'VARIABLES')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,18482)NROWZ
18482         FORMAT('       IN ROW ',I10,' OF THE DATA MATRIX,')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,18483)MAXN
18483         FORMAT('       THE MAXIMUM ROW SIZE ',I10,   &
                     ' EXCEEDED.')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,18484)
18484         FORMAT('       NO ADDITIONAL DATA WILL BE READ.')
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GO TO 7490
            ENDIF
!
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
!
!           COLUMN 2: GROUP-ID
!
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
!
18445     CONTINUE
          NUMVRD=2
          GO TO 7400
18448     CONTINUE
          GO TO 7490
!
        ELSEIF(ICASRE.EQ.'MATR')THEN
!
!     -----BEGIN MATRIX COPY-----
!          IHMAT1 = FIRST  HALF OF MATRIX NAME
!          IHMAT2 = SECOND HALF OF MATRIX NAME
!          INAMMA = NAME INDEX FOR MATRIX
!          IMATC1 = FIRST COLUMN OF THE MATRIX
!          IMATNR = NUMBER OF ROWS    IN THE MATRIX
!          IMATNC = NUMBER OF COLUMNS IN THE NATRIX
!
          IF(NUMLRD.EQ.1)THEN
!
            INAM=NUMNAM
            ICOL=NUMCOL
!
            INAM=INAM+1
            ICOL=ICOL+1
!
            INAMMA=INAM
            IMATC1=ICOL
!
            IHNAME(INAMMA)=IHMAT1
            IHNAM2(INAMMA)=IHMAT2
            IUSE(INAMMA)='M'
            IVALUE(INAMMA)=ICOL
            IN(INAMMA)=0
            IVALU2(INAMMA)=ICOL+NUMDPL-1
            IMATNC=NUMDPL
            NUMNAM=INAM
            NUMCOL=ICOL
!
            ICOL=ICOL-1
            IF(NUMDPL.GT.0)THEN
              DO 7452 IE=1,NUMDPL
                INAM=INAM+1
                ICOL=ICOL+1
                IHNAME(INAM)=JENAM1(IE)
                IHNAM2(INAM)=JENAM2(IE)
                IUSE(INAM)='V'
                IVALUE(INAM)=ICOL
                IN(INAM)=0
                IECOL2(IE)=ICOL
!
                IF(IBUGS2.EQ.'ON')THEN
                  WRITE(ICOUT,7453)IE,IECOL2(IE),NUMDPL,INAM,NUMNAM
 7453             FORMAT('IE,IECOL2(IE),NUMDPL,INAM,NUMNAM = ',5I8)
                  CALL DPWRST('XXX','BUG ')
                ENDIF
!
 7452         CONTINUE
              NUMNAM=INAM
              NUMCOL=ICOL
            ENDIF
          ENDIF
!
          IE2=0
          IMATNR=0
          IF(NUMDPL.GT.0)THEN
            DO 7455 IE=1,NUMDPL
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
            GO TO 7400
          ENDIF
          NUMVRD=IE2-1
          GO TO 7400
!
!     -----END MATRIX COPY-----
!
        ELSEIF(ICASRE.EQ.'IMAG')THEN
!
!     IMAGE TO MATRIX (NOTE: CURRENTLY ONLY ONE COMPONENT
!     AT A TIME CAN BE READ, SO NEED TO DO SOMETHING LIKE
!
!          READ IMAGE RED   FILE.DAT RED
!          READ IMAGE GREEN FILE.DAT GREEN
!          READ IMAGE BLUE  FILE.DAT BLUE
!
!     IN ORDER TO READ ALL COMPONENTS INTO SEPARATE MATRICES.
!
!     -----BEGIN MATRIX COPY-----
!          IHMAT1 = FIRST  HALF OF MATRIX NAME
!          IHMAT2 = SECOND HALF OF MATRIX NAME
!          INAMMA = NAME INDEX FOR MATRIX
!          IMATC1 = FIRST COLUMN OF THE MATRIX
!          IMATNR = NUMBER OF ROWS    IN THE MATRIX
!          IMATNC = NUMBER OF COLUMNS IN THE NATRIX
!
          IF(NUMLRD.EQ.1)THEN
!
            NUMDPL=NUMDPL/3
!
            INAM=NUMNAM
            ICOL=NUMCOL
!
            INAM=INAM+1
            ICOL=ICOL+1
!
            INAMMA=INAM
            IMATC1=ICOL
!
            IHNAME(INAMMA)=IHMAT1
            IHNAM2(INAMMA)=IHMAT2
            IUSE(INAMMA)='M'
            IVALUE(INAMMA)=ICOL
            IN(INAMMA)=0
            IVALU2(INAMMA)=ICOL+NUMDPL-1
            IMATNC=NUMDPL
            NUMNAM=INAM
            NUMCOL=ICOL
!
            ICOL=ICOL-1
            IF(NUMDPL.GT.0)THEN
              DO 7552 IE=1,NUMDPL
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
!
          IE2=0
          IMATNR=0
          IF(NUMDPL.GT.0)THEN
            DO 7555 IE=1,NUMDPL
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
            GO TO 7400
          ENDIF
          NUMVRD=IE2-1
          GO TO 7400
!
        ELSEIF(ICASRE.EQ.'ROWI')THEN
          IF(I.GT.MAXN)GO TO 7480
          IROWLB(I)=' '
          IF(ISUB(I).NE.1)GO TO 7400
          ILEN=24
          IF(N2.LT.ILEN)ILEN=N2
          DO 7442 KK=1,ILEN
            IROWLB(I)(KK:KK)=IFUNC2(KK)(1:1)
7442      CONTINUE
          GO TO 7400
        ENDIF
!
!  OCTOBER 2004.  IF NUMBER OF REQUESTED ITEMS IS GREATER THAN
!                 NUMBER OF ITEMS ON THE LINE, PAD WITH MISSING
!                 VALUE (PREAMV).
!
!                 THE SET READ PAD MISSING COLUMNS COMMANDS
!                 DETERMINES WHETHER WE PAD OR USE THE PREVIOUS
!                 BEHAVIOR (I.E., IN SOME CASES, A MISSING COLUMN
!                 MAY INDICATE AN ERROR).
!
        IE2=0
        IF(NUME.LE.0)THEN
          NUMVRD=IE2-1
          GO TO 7450
        ENDIF
        DO 7445 IE=1,NUME
!
          IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
            WRITE(ICOUT,7404)NUMDPL,IE,IREAPD
 7404       FORMAT('AT 7445: NUMDPL,IE,IREAPD = ',2I8,2X,A4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
          IE2=IE
          IF(IREAPD.EQ.'OFF')THEN
            IF(IE2.GT.NUMDPL)THEN
              NUMVRD=NUMDPL
              WRITE(ICOUT,7446)IFROW,NUME,NUMDPL
 7446         FORMAT('****** WARNING AT LINE ',I10,': ',I5,   &
                     ' VALUES READ, BUT ',I5,' VALUES WERE EXPECTED.')
              CALL DPWRST('XXX','BUG ')
!CCCC         GO TO 7450
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
!
          IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
            WRITE(ICOUT,7406)ICOLVJ,IJ,Z0
 7406       FORMAT('AT 7445: ICOLVJ,IJ,Z0 = ',2I8,G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
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
!
!     2018/07: IF REQUESED, SAVE CHARACTER DATA AS CATEGORICAL NUMERIC
!              DATA
!
 7450   CONTINUE
        IF(IGRPAU.EQ.'CATE' .AND. NXC.GT.0)THEN
          DO 7558 IE=1,NXC
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
!
 7400 CONTINUE
 7470 CONTINUE
      IENDTY=2
      GO TO 7490
!
 7480 CONTINUE
!
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
        WRITE(ICOUT,7481)NUME
 7481   FORMAT('AT 7480: ERROR OR END OF FILE FOR FORMATTED READ, ',   &
               'NUME = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        DO 7485 K=1,NUME
        WRITE(ICOUT,7487)K,X0(K)
 7487   FORMAT('K, X0(K) = ',I8,2X,G15.7)
        CALL DPWRST('XXX','BUG ')
 7485   CONTINUE
      ENDIF
!
      IENDTY=1
      NUMLRD=NUMLRD-1
      GO TO 7490
!
 7490 CONTINUE
!
!               *****************************
!               **  STEP 11--              **
!               **  UPDATE THE NAME TABLE  **
!               *****************************
!
      ISTEPN='11'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICASRE.EQ.'PARA')THEN
        ISTEPN='7700'
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        IF(NUMPRD.GT.0)THEN
          DO 7710 IE=1,NUMPRD
            IH1=JENAM1(IE)
            IH2=JENAM2(IE)
            IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
              WRITE(ICOUT,7711)IE,JENAM1(IE),JENAM2(IE),X0(IE)
 7711         FORMAT('IE,JENAM1(IE),JENAM2(IE),X0(IE) = ',   &
                     I8,2X,2A4,E15.7)
              CALL DPWRST('XXX','BUG ')
            ENDIF
            DO 7720 J=1,NUMNAM
              IF(IUSE(J).EQ.'P'.AND.   &
                IHNAME(J).EQ.IH1.AND.IHNAM2(J).EQ.IH2)THEN
                IECOL2(IE)=J
                IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
                  WRITE(ICOUT,7731)IE,J,IECOL2(IE),X0(IE)
 7731             FORMAT('IE,J,IECOL2(IE),X0(IE) = ',3I8,E15.7)
                  CALL DPWRST('XXX','BUG ')
                ENDIF
                VALUE(J)=X0(IE)
                IVALUE(J)=INT(VALUE(J))
!CCCC           FOLLOWING LINE ADDED SO THAT DELETE AND RETAIN WILL NOT
!CCCC           DELETE PARAMETER CREATED VIA READ PARAMETER.  MARCH 1994.
                IN(J)=1
              ENDIF
 7720       CONTINUE
 7710     CONTINUE
        ENDIF
        GO TO 7900
      ELSEIF(ICASRE.EQ.'FUNC')THEN
!
        ISTEPN='7800'
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        IF(NUMFRD.GT.0)THEN
          CALL DPUPPE(IFUNC2,N2,IFUNC3,IBUGS2,IERROR)
          ISTART=IFCOL1
          ISTOP=N2
          DO 7810 IE=1,NUMFRD
            IH1=JENAM1(IE)
            IH2=JENAM2(IE)
            IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
               WRITE(ICOUT,7811)IE,JENAM1(IE),JENAM2(IE),IECASE(IE)
 7811          FORMAT('IE,JENAM1(IE),JENAM2(IE),IECASE(IE) = ',   &
                      I8,2X,2A4,2X,A4)
               CALL DPWRST('XXX','BUG ')
            ENDIF
            DO 7820 J=1,NUMNAM
              IF(IUSE(J).EQ.'F'.AND.   &
                IHNAME(J).EQ.IH1.AND.IHNAM2(J).EQ.IH2)THEN
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
!
                IF(NUMFRD.EQ.1)THEN
                  IF(N2.LE.0)GO TO 7832
                  ICOL1=1
                  ICOL2=N2
                  IF(ICOL2.GT.ICOL1+N2-1)ICOL2=ICOL1+N2-1
                  I2=0
                  DO 7831 I=ICOL1,ICOL2
                    I2=I2+1
                    IFUNC3(I2)=IFUNC2(I2)
 7831             CONTINUE
 7832             CONTINUE
                  N3=I2
!
                ELSE
                  IWORD=IE
                  IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
                    WRITE(ICOUT,7841)ICOL1,ICOL2,ISTART,ISTOP,N2,N3,   &
                                     IE,IWORD
 7841               FORMAT('ICOL1,ICOL2,ISTART,ISTOP,N2,N3,IE,IWORD = ',   &
                           8I8)
                    CALL DPWRST('XXX','BUG ')
                  ENDIF
                  CALL DPEXW2(IFUNC2,N2,ISTART,ISTOP,IWORD,   &
                              ICOL1,ICOL2,IFUNC3,N3,   &
                              IBUGS2,ISUBRO,IERROR)
                  IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
                    WRITE(ICOUT,7841)ICOL1,ICOL2,ISTART,ISTOP,N2,N3,   &
                                     IE,IWORD
                    CALL DPWRST('XXX','BUG ')
                  ENDIF
                ENDIF
!
                CALL DPINFU(IFUNC3,N3,IHNAME,IHNAM2,IUSE,IN,   &
                            IVSTAR,IVSTOP,   &
                            NUMNAM,IANSLC,IWIDTH,IHLEFT,IHLEF2,ILISTL,   &
                            NEWNAM,MAXNME,   &
                            IFUNC,NUMCHF,MAXCHF,IBUGS2,IERROR)
!
                IF(NEWNAM.EQ.'YES'.AND.IERROR.EQ.'NO')NUMNAM=NUMNAM-1
!
              ENDIF
 7820       CONTINUE
 7810     CONTINUE
        ENDIF
        GO TO 7900
!
      ELSEIF(ICASRE.EQ.'ROWI')THEN
        GO TO 7900
      ELSE
!
        ISTEPN='7600'
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,7601)ICASRE,NUMVRD,NUMNAM,NUME,NXC
 7601     FORMAT('ICASRE,NUMVRD,NUMNAM,NUME,NXC = ',A4,4I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
!       UPDATE NUMERIC VARIABLES
!
        IF(NUMVRD.GT.0)THEN
          DO 7610 IE=1,NUMVRD
            N=IEN(IE)
            ICOLVJ=IECOL2(IE)
            DO 7620 J=1,NUMNAM
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
!
        NUMVRP=NUMVRD+1
        IF(ICASRE.EQ.'MATR')GO TO 7690
        IF(NUMVRP.GT.NUME)GO TO 7690
        DO 7650 IE=NUMVRP,NUME
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
!
!       UPDATE CHARACTER VARIABLES CONVERTED TO CATEGORICAL VARIABLES
!
        IF(IGRPAU.EQ.'CATE' .AND. NXCSAV.GE.1)THEN
          DO 77610 IE=1,NXCSAV
            IF(IRWLC3.EQ.IE)GO TO 77610
            N=IENC(IE)
            ICOLVJ=IECOLC(IE)
            DO 77620 J=1,NUMNAM
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
!
        GO TO 7900
      ENDIF
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
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     2014/10: SAVE FOLLOWING AS INTERNAL PARAMETERS:
!
!              1) ISKIP   = NUMBER OF HEADER LINES SKIPPED
!              2) NUMLRD  = NUMBER OF LINES READ
!              3) NUMVRD  = NUMBER OF VARIABLES READ
!
!              WRITE INDIVIDUAL VARIABLE NAMES TO: ZZZV1 - ZZZVK
!
      IH1='ISKI'
      IH2='P   '
      VALUE0=REAL(ISKIP)
      CALL DPADDP(IH1,IH2,VALUE0,IHOST1,ISUBN0,   &
                  IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                  IANS,IWIDTH,IBUGS2,IERROR)
!
      IH1='NUML'
      IH2='RD  '
      VALUE0=REAL(NUMLRD)
      CALL DPADDP(IH1,IH2,VALUE0,IHOST1,ISUBN0,   &
                  IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                  IANS,IWIDTH,IBUGS2,IERROR)
!
      IH1='NUMV'
      IH2='RD  '
      VALUE0=REAL(NUMVRD)
      CALL DPADDP(IH1,IH2,VALUE0,IHOST1,ISUBN0,   &
                  IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                  IANS,IWIDTH,IBUGS2,IERROR)
!
      IF(NUMVRD.GT.0)THEN
        DO 12001 II=1,NUMVRD
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
            GO TO 12001
          ENDIF
!
          DO 12003 JJ=1,8
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
          DO 12005 JJ=8,1,-1
            IF(ISTRZ2(JJ)(1:1).NE.' ')THEN
              NCHART=JJ
              GO TO 12009
            ENDIF
12005     CONTINUE
12009     CONTINUE
!
          NEWNAM='YES'
          DO 12011 JJ=1,NUMNAM
            IF(IH1.EQ.IHNAME(JJ) .AND. IH2.EQ.IHNAM2(JJ))THEN
              NEWNAM='OLD'
              ILISTL=JJ
              GO TO 12019
            ENDIF
12011     CONTINUE
12019     CONTINUE
          IF(NEWNAM.EQ.'YES')ILISTL=NUMNAM+1
          CALL DPINFU(ISTRZ2,NCHART,IHNAME,IHNAM2,IUSE,IN,   &
                      IVSTAR,IVSTOP,   &
                      NUMNAM,IANS,IWIDTH,IH1,IH2,ILISTL,   &
                      NEWNAM,MAXNME,   &
                      IFUNC,NUMCHF,MAXCHF,IBUGS2,IERROR)
!
12001   CONTINUE
      ENDIF
!
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
!
          IFRST=IFCOL3
          IF(IFRST+240-1.GE.IFCOL4)THEN
            ILAST=IFCOL4
          ELSE
            ILAST=IFRST+240-1
          ENDIF
!
          IF(ICASRE.NE.'CLIP')THEN
            WRITE(ICOUT,8011)
 8011       FORMAT('THE SCANNED REGION OF THE FIRST DATA LINE READ ',   &
                   '(TO A MAXIMUM OF 240 CHARACTERS) = ')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,8012)(ISTOR3(J),J=IFRST,MIN(240,ILAST))
 8012       FORMAT(240A1)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,8013)
 8013       FORMAT('THE SCANNED REGION OF THE LAST  DATA LINE READ ',   &
                   '(TO A MAXIMUM OF 240 CHARACTERS) = ')
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
!
!               *********************************************
!               **  STEP 13--                              **
!               **  PRINT OUT SUMMARY INFORMATION          **
!               **  VARIABLES/PARAMETERS/FUNCTIONS         **
!               **  THAT WERE READ IN.                     **
!               *********************************************
!
      IF(ICASRE.EQ.'PARA')THEN
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8201)
 8201     FORMAT('PARAMETER     VALUE')
          CALL DPWRST('XXX','BUG ')
!
          DO 8210 IE=1,NUME
            IH1=JENAM1(IE)
            IH2=JENAM2(IE)
            DO 8220 I=1,NUMNAM
              I2=I
              IF(IH1.EQ.IHNAME(I).AND.IH2.EQ.IHNAM2(I))THEN
                WRITE(ICOUT,8226)IH1,IH2,VALUE(I2)
 8226           FORMAT(A4,A4,4X,E15.7)
                CALL DPWRST('XXX','BUG ')
              ENDIF
 8220       CONTINUE
 8210     CONTINUE
        ENDIF
!
      ELSEIF(ICASRE.EQ.'FUNC' .OR. ICASRE.EQ.'CFUN')THEN
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8301)
 8301     FORMAT('FUNCTION (= STRING)     CONTENT')
          CALL DPWRST('XXX','BUG ')
!
          DO 8310 IE=1,NUME
            IH1=JENAM1(IE)
            IH2=JENAM2(IE)
            DO 8320 I=1,NUMNAM
              I2=I
              IF(IH1.EQ.IHNAME(I).AND.IH2.EQ.IHNAM2(I))THEN
                JMIN=IVSTAR(I2)
                JMAX=IVSTOP(I2)
                NC=JMAX-JMIN+1
                IFRMT=' '
                IFRMT='(2A4,10X,    A1)'
                WRITE(IFRMT(10:13),'(I4)')NC
                WRITE(ICOUT,IFRMT)IH1,IH2,(IFUNC(J),J=JMIN,JMAX)
!8326           FORMAT(A4,A4,10X,80A1)
                CALL DPWRST('XXX','BUG ')
              ENDIF
 8320       CONTINUE
 8310     CONTINUE
        ENDIF
!
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
!
          DO 8410 IE=1,NUME
            IH1=JENAM1(IE)
            IH2=JENAM2(IE)
            DO 8420 I=1,NUMNAM
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
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8101)
 8101     FORMAT('VARIABLE     COLUMN    OBS/VARIABLE')
          CALL DPWRST('XXX','BUG ')
!
          DO 8110 IE=1,NUME
            IH1=JENAM1(IE)
            IH2=JENAM2(IE)
            DO 8120 I=1,NUMNAM
              I2=I
              IF(IH1.EQ.IHNAME(I).AND.IH2.EQ.IHNAM2(I))THEN
                WRITE(ICOUT,8126)IH1,IH2,IVALUE(I2),IN(I2)
 8126           FORMAT(A4,A4,1X,I8,5X,I8)
                CALL DPWRST('XXX','BUG ')
              ENDIF
 8120       CONTINUE
 8110     CONTINUE
        ENDIF
        GO TO 8800
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
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IOFILE.EQ.'YES'.AND.ICURST.EQ.'OPEN')GO TO 8810
      GO TO 8890
 8810 CONTINUE
      IENDFI='OFF'
      IREWIN='ON'
      IF(IREARW.EQ.'ON')THEN
         CALL DPCLFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
         IENDFI,IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
         IREACS='CLOSED'
      ENDIF
 8890 CONTINUE
!
      IF(IFEEDB.EQ.'ON' .AND. ICASRE.EQ.'ROWR')THEN
        WRITE(ICOUT,55805)NUMLRD,IVBASE,NUMDPL
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
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
      IREADL=IREAD2
      ISKIP=ISKPSV
      IREANQ=IREANQSV
!
!     2019/09: IF CHARACTER DATA WAS READ, DO THE FOLLOWING:
!
!              1. IF CHARACTER VARIABLES ARE IN "OVERWRITE" MODE, THEN
!
!                 A. CLOSE AND REOPEN "dpst2f.dat".
!
!                 B. OPEN THE CHARACTER DATA FILE ("dpzchf.dat" BY DEFAULT).
!
!                 C. LOOP TRHOUGH AND WRITE CONTENTS OF "dpst2f.dat" TO
!                    THE CHARACTER DATA FILE.  ADD THE NUMBER OF LINES
!                    FOR EACH VARIABLE.
!
!              2. IF CHARACTER VARIABLES ARE IN "APPEND" MODE, THEN
!
!                 A. CLOSE AND REOPEN "dpst2f.dat".
!
!                 B. OPEN THE CHARACTER DATA FILE.
!
!                 C. OPEN THE TEMPORARY FILE "dpst5f.dat".
!
!                 C. LOOP THROUGH AND APPEND CONTENTS OF "dpzchf.dat"
!                    AND "dpst2f.dat" AND WRITE TO "dpst5f.dat".
!
!                 D. COPY "dpst5f.dat" TO "dpzchf.dat".
!
      IF(IZCHCS.EQ.'OPEN')THEN
!
!       CHECK IF CHARACTER VARIABLE FILE EXISTS.  IF NOT, THEN USE
!       "OVERWRITE" METHOD.
!
        IFILE4=IZCHNA
        ISUBN0='READ'
        IERRFI='NO'
        CALL DPINFI(IFILE4,IEXIST,IOPEN,IACC,ISUBN0,IBUGS2,   &
                    ISUBRO,IERROR)
!
        IF(ISTRVA.EQ.'OVER' .OR. IEXIST.EQ.'NO')THEN
!
!         STEP 1: CLOSE AND REOPEN "dpst2f.dat" FILE
!
          IOP='CLOS'
          IFLG11=0
          IFLG21=1
          IFLG31=0
          IFLAG4=0
          IFLAG5=0
          CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,   &
                      IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                      IBUGS2,ISUBRO,IERROR)
          IOP='OPEN'
          CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,   &
                      IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                      IBUGS2,ISUBRO,IERROR)
!
!         STEP 2: OPEN THE CHARACTER DATA FILE
!
          IOUNI4=IZCHNU
          IFILE4=IZCHNA
          ISTAT4=IZCHST
          IFORM4=IZCHFO
          IACCE4=IZCHAC
          IPROT4=IZCHPR
          ICURS4=IZCHCS
!
          ISUBN0='READ'
          IERRFI='NO'
          CALL DPOPFI(IOUNI4,IFILE4,ISTAT4,IFORM4,IACCE4,IPROT4,ICURS4,   &
                      IREWI4,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
!
!         STEP 3: UPDATE THE CONTENTS OF THE CHARACTER DATA FILE
!
          READ(IOUNI2,*,ERR=9047,END=9047)IVAR
          WRITE(IOUNI4,'(I8)')IVAR
!
          DO 9041 KK=1,IVAR
            ISTR=' '
            READ(IOUNI2,'(A8)',ERR=9047,END=9047)ISTR(1:8)
            WRITE(IOUNI4,'(A8,I10)')ISTR(1:8),NUMLRD
 9041     CONTINUE
!
          IFRMT=' '
          IFRMT='(A    )'
          NTEMP=25*IVAR
          IF(NTEMP.GT.9999)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,211)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9042)
 9042       FORMAT('      MAXIMUM NUMBER OF CHARACTER VARIABLES ',   &
                   'EXCEEDED.')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9049
          ENDIF
          WRITE(IFRMT(3:6),'(I4)')NTEMP
          DO 9043 KK=1,NUMLRD
            ISTR=' '
            READ(IOUNI2,IFRMT,ERR=9047,END=9047)ISTR(1:NTEMP)
            WRITE(IOUNI4,IFRMT)ISTR(1:NTEMP)
 9043     CONTINUE
          GO TO 9049
!
 9047     CONTINUE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,211)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,9048)
 9048     FORMAT('      ERROR IN CREATING CHARACTER VARIABLE FILE.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
!
!         STEP 4: CLOSE THE CHARACTER DATA FILE AND "dpst2f.dat"
!
 9049     CONTINUE
          CALL DPCLFI(IOUNI4,IFILE4,ISTAT4,IFORM4,IACCE4,IPROT4,ICURS4,   &
                      IENDFI,IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
          IZCHCS='CLOSED'
!
          IOP='CLOS'
          CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,   &
                      IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                      IBUGS2,ISUBRO,IERROR)
!
        ELSE
!
!         STEP 1: CLOSE AND REOPEN "dpst2f.dat" FILE, ALSO OPEN
!                 "dpst5f.dat"
!
          IOP='CLOS'
          IFLG11=0
          IFLG21=1
          IFLG31=0
          IFLAG4=0
          IFLAG5=0
          CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,   &
                      IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                      IBUGS2,ISUBRO,IERROR)
          IOP='OPEN'
          IFLAG5=1
          CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,   &
                      IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                      IBUGS2,ISUBRO,IERROR)
!
!         STEP 2: OPEN THE CHARACTER DATA FILE
!
          IOUNI4=IZCHNU
          IFILE4=IZCHNA
          ISTAT4=IZCHST
          IFORM4=IZCHFO
          IACCE4=IZCHAC
          IPROT4=IZCHPR
          ICURS4=IZCHCS
!
          ISUBN0='READ'
          IERRFI='NO'
          CALL DPOPFI(IOUNI4,IFILE4,ISTAT4,IFORM4,IACCE4,IPROT4,ICURS4,   &
                      IREWI4,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
!
!         STEP 3: UPDATE THE CONTENTS OF THE CHARACTER DATA FILE
!
          READ(IOUNI2,*,ERR=9057,END=9057)IVAR1
          READ(IOUNI4,*,ERR=9057,END=9057)IVAR2
          IVAR=IVAR1 + IVAR2
          WRITE(IOUNI5,'(I8)')IVAR
!
!         OLD DATA
!
          MXROW1=-1
          DO 9051 KK=1,IVAR2
            READ(IOUNI4,'(2A4,I10)',ERR=9057,END=9057)JVNAM1(KK),   &
                 JVNAM2(KK),NIV(KK)
            IF(NIV(KK).GT.MXROW1)MXROW1=NIV(KK)
            WRITE(IOUNI5,'(2A4,I10)')JVNAM1(KK),JVNAM2(KK),NIV(KK)
 9051     CONTINUE
!
!         NEW DATA
!
          MXROW2=NUMLRD
          DO 9061 KK=1,IVAR1
            IROW=IVAR1+KK
            READ(IOUNI2,'(2A4)',ERR=9057,END=9057)JVNAM1(IROW),   &
                                                  JVNAM2(IROW)
            NIV(IROW)=NUMLRD
            WRITE(IOUNI5,'(2A4,I10)')JVNAM1(IROW),JVNAM2(IROW),NIV(IROW)
 9061     CONTINUE
!
          NTEMP1=25*IVAR1
          NTEMP2=25*IVAR2
          NTEMP=NTEMP1+NTEMP2
          IF(NTEMP.GT.9999)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,211)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9052)
 9052       FORMAT('      MAXIMUM NUMBER OF CHARACTER VARIABLES ',   &
                   'EXCEEDED.')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9059
          ENDIF
!
          IFRMT=' '
          IFRMT='(A    )'
          WRITE(IFRMT(3:6),'(I4)')NTEMP2
          IFRMT2=' '
          IFRMT2='(A    )'
          WRITE(IFRMT2(3:6),'(I4)')NTEMP1
          IFRMT3=' '
          IFRMT3='(A    )'
          WRITE(IFRMT3(3:6),'(I4)')NTEMP
!
          DO 9053 KK=1,MAX(MXROW1,MXROW2)
            ISTR=' '
            IF(KK.LE.MXROW1 .AND. KK.LE.MXROW2)THEN
              READ(IOUNI4,IFRMT,ERR=9057,END=9057)ISTR(1:NTEMP2)
              READ(IOUNI2,IFRMT2,ERR=9057,END=9057)   &
                   ISTR(NTEMP2+1:NTEMP1+NTEMP2)
              WRITE(IOUNI5,IFRMT3)ISTR(1:NTEMP)
            ELSEIF(KK.GT.MXROW1 .AND.KK.LE.MXROW2)THEN
              READ(IOUNI2,IFRMT2,ERR=9057,END=9057)   &
                   ISTR(NTEMP2+1:NTEMP1+NTEMP2)
              WRITE(IOUNI5,IFRMT3)ISTR(1:NTEMP)
            ELSEIF(KK.LE.MXROW1 .AND.KK.GT.MXROW2)THEN
              READ(IOUNI4,IFRMT,ERR=9057,END=9057)ISTR(1:NTEMP2)
              ISTR(NTEMP2+1:NTEMP1+NTEMP2)=' '
              WRITE(IOUNI5,IFRMT3)ISTR(1:NTEMP)
            ENDIF
 9053     CONTINUE
          GO TO 9059
!
 9057     CONTINUE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,211)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,9058)
 9058     FORMAT('      ERROR IN CREATING CHARACTER VARIABLE FILE.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
!
!         STEP 4: CLOSE THE CHARACTER DATA FILE AND "dpst2f.dat"
!
 9059     CONTINUE
          CALL DPCLFI(IOUNI4,IFILE4,ISTAT4,IFORM4,IACCE4,IPROT4,ICURS4,   &
                      IENDFI,IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
          IZCHCS='CLOSED'
!
          IOP='CLOS'
          CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,   &
                      IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                      IBUGS2,ISUBRO,IERROR)
!
!         STEP 5: COPY "dpst5f.dat" TO CHARACTER VARIABLE FILE
!
          CALL COPYFI(IFILE5,IFILE4,IBUGS2,ISUBRO,IERROR)
!
        ENDIF
        GO TO 9090
      ENDIF
!
      IF(ICASRE.EQ.'IMAZ' .OR. ICASRE.EQ.'IMAG')THEN
#ifdef HAVE_GD
        CALL gdunlo()
#endif
      ENDIF
!
 9090 CONTINUE
!
      IFILQU=IFILQ2
!
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'READ')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPREAD--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFROW1,IFCOL1,IFCOL2,AFROW2,ICASRE
 9012   FORMAT('IFROW1,IFCOL1,IFCOL2,AFROW2,ICASRE = ',   &
               3I8,2X,E15.7,2X,A4)
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
 9036   FORMAT('IHMAT1,IHMAT2,INAMMA,IMATC1,IMATNR,IMATNC = ',   &
               A4,2X,A4,2X,4I8)
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
!
      RETURN
      END SUBROUTINE DPREAD

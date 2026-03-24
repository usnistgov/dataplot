      SUBROUTINE DPDDS(XTEMP1,MAXNXT,   &
                       ICAPSW,IFORSW,   &
                       IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--CARRY OUT A DDS (DATA-DEPENDENT SYSTEM) ANALYSIS
!              (1-SAMPLE)
!     EXAMPLE--DDS Y 6 5 DELT
!              DDS Y 6 5  (== DDS Y 6 5 1)
!              DDS Y      (== DDS Y 6 5 1)
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--94/4
!     ORIGINAL VERSION--MARCH     1994.
!     UPDATED         --APRIL     1996. DDS CODE MODIFIED (ALAN):
!                                       A) SOME DIMENSIONS TO DPDDS, USE
!                                          EQUIVALENCE
!                                       B) I/O CONSISTENT WITH DATAPLOT
!                                       C) USE IERROR RATHER THAN STOP
!                                       D) INCLUDE FILE FOR DDS COMMON
!                                          BLOCKS AND PARAMETER STATEMENTS
!                                       THESE CHANGES PROPOGATE TO LOWER
!                                       LEVEL DDS ROUTINES
!     UPDATED         --MAY       2011. USE DPPARS AND DPPAR3
!     UPDATED         --JULY      2019. TWEAK SCRATCH SPACE
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
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IREPU
      CHARACTER*4 IRESU
      CHARACTER*4 ICASE
      CHARACTER*40 INAME
      PARAMETER (MAXSPN=10)
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
      INCLUDE 'DPCODD.INC'
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCODA.INC'
!
      DIMENSION XTEMP1(*)
!
      DIMENSION PRED2(MAXOBV)
      DIMENSION RES2(MAXOBV)
      DIMENSION XDDS(INT(REAL(MAXOBV)/3.),MXSER)
      DIMENSION YDDS(INT(REAL(MAXOBV)/3.),MXSER)
      DIMENSION Y2(100)
      DIMENSION AT(INT(REAL(MXNOB1)/3.),MXSER)
!
!CCCC EQUIVALENCE (X3D(1),PRED2(1))
!CCCC EQUIVALENCE (X3D(MAXOBV+1),RES2(1))
!CCCC EQUIVALENCE (D(1),XDDS(1,1))
!CCCC EQUIVALENCE (DSYMB(1),YDDS(1,1))
!CCCC EQUIVALENCE (DFILL(1),AT(1,1))
      EQUIVALENCE (PRED2(1),X3D(1))
      EQUIVALENCE (RES2(1),X3D(MAXOBV+1))
      EQUIVALENCE (XDDS(1,1),D(1))
      EQUIVALENCE (YDDS(1,1),DSYMB(1))
      EQUIVALENCE (AT(1,1),DFILL(1))
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPDD'
      ISUBN2='S   '
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
!               ********************************
!               **  TREAT THE DDS    CASE     **
!               ********************************
!
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'PDDS')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPDDS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGA2,IBUGA3,IBUGQ,ISUBRO,MAXNXT
   53   FORMAT('IBUGA2,IBUGA3,IBUGQ = ',4(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)ICAPSW,IFORSW
   55   FORMAT('ICAPSW,IFORSW = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *********************************
!               **  STEP 2--                   **
!               **  EXTRACT THE VARIABLE LIST  **
!               *********************************
!
      ISTEPN='2'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PDDS')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      INAME='DATA DEPENDENT SYSTEMS'
      MINNA=1
      MAXNA=100
      MINN2=2
      IFLAGE=0
      IFLAGM=1
      IFLAGP=39
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
                  IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PDDS')THEN
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
!               ****************************************
!               **  STEP 22--                         **
!               **  CHECK THE VALIDITY OF ARGUMENTS   **
!               **  2 - 4 (THESE SHOULD BE PARAMETERS **
!               **  OR NUMBERS).                      **
!               ****************************************
!
      ISTEPN='22'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PDDS')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IORDAR=2
      IORDMA=1
!
      IF(NUMVAR.GE.2)THEN
        IORDAR=INT(PVAR(2))
      ENDIF
!
      IF(NUMVAR.GE.3)THEN
        IORDMA=INT(PVAR(3))
      ENDIF
!
      DELTAT=1.0
      IF(NUMVAR.GE.4)THEN
        DELTAT=PVAR(4)
      ENDIF
!
      ICOL=1
      NUMVA2=1
      CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                  INAME,IVARN1,IVARN2,IVARTY,   &
                  ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                  MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                  MAXCP4,MAXCP5,MAXCP6,   &
                  V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                  Y,XTEMP1,XTEMP1,N1,NLOCA2,NLOCA3,ICASE,   &
                  IBUGA3,ISUBRO,IFOUND,IERROR)
       IF(IERROR.EQ.'YES')GO TO 9000
!
!               *********************************
!               **  STEP 52--                  **
!               **  FORM THE DDS ANALYSIS      **
!               *********************************
!
      ISTEPN='52'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PDDS')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5211)
 5211   FORMAT('***** FROM DPDDS, AS WE ARE ABOUT TO CALL DPDDS2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5212)N1,MAXN
 5212   FORMAT('N1,MAXN = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO 5215 I=1,N1
          WRITE(ICOUT,5216)I,Y(I)
 5216     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
 5215   CONTINUE
      ENDIF
!
!CCCC APRIL 1996.  ADD XDDS, YDDS, Y2 TO ARGUMENT LIST (DIMENSIONING DONE
!CCCC IN DPDDS RATHER THAN IN DPDDS2 AND DPDDS3 TO ALLOW USE OF EQUIVALENCE
!CCCC WITH DATAPLOT SCRATCH ARRAYS)
!
      MAXRW1=INT(REAL(MAXOBV)/3.)
      MAXRW2=INT(REAL(MXOBN1)/3.)
      CALL DPDDS2(Y,N1,IORDAR,IORDMA,DELTAT,NUMVAR,ILOCV,   &
                  XDDS,YDDS,AT,Y2,MAXNXT,MAXRW1,MAXW2,   &
                  PRED2,RES2,RESSD,RESDF,   &
                  IBUGA3,ISUBRO,IERROR)
!
!               ***************************************
!               **  STEP 15--                        **
!               **  UPDATE INTERNAL DATAPLOT TABLES  **
!               ***************************************
!
      ISTEPN='15'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PDDS')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICOLPR=MAXCP1
      ICOLRE=MAXCP2
      IREPU='OFF'
      IRESU='ON'
      NLEFT=N1
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
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'PDDS')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPDDS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)IFOUND,IERROR
 9016   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPDDS
      SUBROUTINE DPDDS2(Y1,N1,IORDAR,IORDMA,DELTAT,NUMVAR,ILOCV,   &
                        XDDS,YDDS,AT,Y2,MAXNXT,MAXRW1,MAXRW2,   &
                        PRED2,RES2,RESSD,RESDF,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS ROUTINE CARRIES OUT A DDS ANALYSIS
!              (1-SAMPLE OR 2-SAMPLE)
!     EXAMPLE--DDS Y 6 5 DELT
!              DDS Y 6 5  (== DDS Y 6 5 1)
!              DDS Y      (== DDS Y 6 5 1)
!     SAMPLE 1 IS IN INPUT VECTOR Y1
!              (WITH N1 OBSERVATIONS).
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 CENTER FOR APPLIED MATHEMATICS
!                 NATIONAL BUREAU OF STANDARDS
!                 WASHINGTON, D. C. 20234
!                 PHONE--301-921-3651
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
!           MODIFIED, OR OTHERWISE USED IN A CONTEXT
!           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--MAY       1984.
!     UPDATED         --APRIL     1987.  (LARRY KNAB CORRECTION--
!                                        BROWNLEE, P. 225)
!     UPDATED         --FEBRUARY  1994.  REFORMAT OUTPUT
!     UPDATED         --FEBRUARY  1994.  DPWRST: 'BUG ' => 'WRIT'
!     UPDATED         --APRIL     1996. DDS CODE MODIFIED (ALAN):
!                                       A) SOME DIMENSIONS TO DPDDS, USE
!                                          EQUIVALENCE
!                                       B) I/O CONSISTENT WITH DATAPLOT
!                                       C) USE IERROR RATHER THAN STOP
!                                       D) INCLUDE FILE FOR DDS COMMON
!                                          BLOCKS AND PARAMETER STATEMENTS
!                                       THESE CHANGES PROPOGATE TO LOWER
!                                       LEVEL DDS ROUTINES
!     UPDATED         --MAY       2024. FIX DIMENSIONING OF XDDS, YDDS,
!                                       AT TO MATCH DPDDS
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
!CCCC CHARACTER*4 IWRITE
!
!CCCC THE FOLLOWING 3 LINES WERE ADDED   FEBRUARY 1994
!CCCC CHARACTER*6 ICONC1
!CCCC CHARACTER*6 ICONC2
!CCCC CHARACTER*6 ICONC3
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      DIMENSION Y1(*)
!CCCC FUTURE--Y2 NEEDS TO BE MADE AN INPUT ARGUMENT
!CCCC APRIL 1996. MAKE Y2 AN INPUT ARGUMENT, ALSO ADD XDDS, YDDS
!CCCC DIMENSION Y2(100)
      DIMENSION Y2(*)
!CCCC APRIL 1996.  ADD FOLLOWING LINES
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODD.INC'
      DIMENSION AT(MAXRW2,MXSER)
!
      DIMENSION PRED2(*)
      DIMENSION RES2(*)
!
      DIMENSION XDDS(MAXRW1,MXSER)
      DIMENSION YDDS(MAXRW2,MXSER)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPCO'
      ISUBN2='F2  '
      IERROR='NO'
!
      N=(-99)
!
      IF(IBUGA3.EQ.'OFF')GO TO 90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,51)
   51 FORMAT('**** AT THE BEGINNING OF DPDDS2--')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,52)IBUGA3
   52 FORMAT('IBUGA3 = ',A4)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,53)DELTAT,NUMVAR,ILOCV
   53 FORMAT('DELTAT,NUMVAR,ILOCV = ',E15.7,I8,I8)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,55)N1
   55 FORMAT('N1 = ',I8)
      CALL DPWRST('XXX','WRIT')
      DO 56 I=1,N1
      WRITE(ICOUT,57)I,Y1(I)
   57 FORMAT('I,Y1(I) = ',I8,E15.7)
      CALL DPWRST('XXX','WRIT')
   56 CONTINUE
      WRITE(ICOUT,65)IORDMA
   65 FORMAT('IORDMA = ',I8)
      CALL DPWRST('XXX','WRIT')
      DO 66 I=1,IORDMA
      WRITE(ICOUT,67)I,Y2(I)
   67 FORMAT('I,Y2(I) = ',I8,E15.7)
      CALL DPWRST('XXX','WRIT')
   66 CONTINUE
   90 CONTINUE
!
!               ********************************************
!               **  STEP 11--                             **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(N1.GE.1)GO TO 1119
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1111)
 1111 FORMAT('***** ERROR IN DPDDS2--THE NUMBER OF OBSERVATIONS ',   &
      'FOR VARIABLE 1 IS NON-POSITIVE')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1112)N1
 1112 FORMAT('SAMPLE SIZE = ',I8)
      CALL DPWRST('XXX','WRIT')
      IERROR='YES'
      GO TO 9000
 1119 CONTINUE
!
      IF(N1.EQ.1)GO TO 1120
      GO TO 1129
 1120 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1121)
 1121 FORMAT('***** NOTE FROM DPDDS2--VARIABLE 1 ',   &
      'HAS ONLY 1 ELEMENT')
      CALL DPWRST('XXX','WRIT')
      GO TO 9000
 1129 CONTINUE
!
      HOLD=Y1(1)
      DO 1135 I=2,N1
      IF(Y1(I).NE.HOLD)GO TO 1139
 1135 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1131)HOLD
 1131 FORMAT('***** NOTE FROM DPDDS2--VARIABLE 1 ',   &
      'HAS ALL ELEMENTS = ',E15.7)
      CALL DPWRST('XXX','WRIT')
      GO TO 9000
 1139 CONTINUE
!
      IF(NUMVAR.LE.1)GO TO 1290
!
      IF(IORDMA.GE.1)GO TO 1219
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1211)
 1211 FORMAT('***** ERROR IN DPDDS2--THE NUMBER OF OBSERVATIONS ',   &
      'FOR VARIABLE 2 IS NON-POSITIVE')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1212)IORDMA
 1212 FORMAT('SAMPLE SIZE = ',I8)
      CALL DPWRST('XXX','WRIT')
      IERROR='YES'
      GO TO 9000
 1219 CONTINUE
!
      IF(IORDMA.EQ.1)GO TO 1220
      GO TO 1229
 1220 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1221)
 1221 FORMAT('***** NOTE FROM DPDDS2--VARIABLE 2 ',   &
      'HAS ONLY 1 ELEMENT')
      CALL DPWRST('XXX','WRIT')
      GO TO 9000
 1229 CONTINUE
!
      HOLD=Y2(1)
      DO 1235 I=2,IORDMA
      IF(Y2(I).NE.HOLD)GO TO 1239
 1235 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1231)HOLD
 1231 FORMAT('***** NOTE FROM DPDDS2--VARIABLE 2 ',   &
      'HAS ALL ELEMENTS = ',E15.7)
      CALL DPWRST('XXX','WRIT')
      GO TO 9000
 1239 CONTINUE
!
 1290 CONTINUE
!
!               ************************************
!               **   STEP 21--                    **
!               **   BRANCH DEPENDING ON WHETHER  **
!               **   1-SAMPLE DDS ANALYSIS OR           **
!               **   2-SAMPLE DDS ANALYSIS.             **
!               ************************************
!
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMVAR.EQ.1)GO TO 3100
      GO TO 4100
!
!               ***********************************
!               **  STEP 31--                    **
!               **  CARRY OUT CALCULATIONS       **
!               **  FOR A 1-SAMPLE DDS ANALYSIS  **
!               ***********************************
!
 3100 CONTINUE
!
      ISTEPN='31'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!CCCC APRIL 1996.  ADD XDDS, YDDS, AT TO ARGUMENT LIST.
      CALL DPDDS3(Y1,N1,IORDAR,IORDMA,DELTAT,NUMVAR,ILOCV,   &
                  XDDS,YDDS,AT,MAXNXT,MAXRW1,MAXRW2,   &
                  PRED2,RES2,RESSD,RESDF,   &
                  IBUGA3,ISUBRO,IERROR)
!
 4100 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'OFF')GO TO 9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPDDS2--')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,9012)N,IBUGA3,IERROR
 9012 FORMAT('N,IBUGA3,IERROR = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,9013)DELTAT,NUMVAR,ILOCV
 9013 FORMAT('DELTAT,NUMVAR,ILOCV = ',E15.7,I8,I8)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,9015)N1
 9015 FORMAT('N1 = ',I8)
      CALL DPWRST('XXX','WRIT')
      DO 9016 I=1,N1
      WRITE(ICOUT,9017)I,Y1(I)
 9017 FORMAT('I,Y1(I) = ',I8,E15.7)
      CALL DPWRST('XXX','WRIT')
 9016 CONTINUE
      WRITE(ICOUT,9025)IORDMA
 9025 FORMAT('IORDMA = ',I8)
      CALL DPWRST('XXX','WRIT')
      DO 9026 I=1,IORDMA
      WRITE(ICOUT,9027)I,Y2(I)
 9027 FORMAT('I,Y2(I) = ',I8,E15.7)
      CALL DPWRST('XXX','WRIT')
 9026 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPDDS2
!     ..........DPDDS3..........
!
      SUBROUTINE DPDDS3(Y1,N1,IORDAR,IORDMA,DELTAT,NUMVAR,ILOCV,   &
                        XDDS,YDDS,AT,MAXNXT,MAXRW1,MAXRW2,   &
                        PRED2,RES2,RESSD,RESDF,   &
                        IBUGA3,ISUBRO,IERROR)
!CCCC APRIL 1996.  ADD XDDS, YDDS, AT TO ARGUMENT LIST
!CCCC PROGRAM DPDDS3
!CCCC  UPDATED--AUGUST        1995.  CRAY DOESN'T LIKE "*" FORMAT
!CCCC                                FOR INTERNAL WRITE
!*****************************************************************
!                                                                *
!    THIS PROGRAM FITS UNIVARIATE (ARMA) AND EXTENDED (EARMA)    *
!  MODELS TO TIME SERIES DATA, WITH OPTIONAL DETERMINISTIC TREND *
!  ESTIMATION FOR UNIVARIATE MODELS. IT IS DESIGNED TO BE USED   *
!  WITH THE BOOK:                                                *
!        "TIME SERIES AND SYSTEM ANALYSIS WITH APPLICATIONS"     *
!                  BY S. M. PANDIT AND S. M. WU                  *
!                                                                *
!              THIS CODE WRITTEN BY WILLIAM WITTIG               *
!                              AT                                *
!               MICHIGAN TECHNOLOGICAL UNIVERSITY                *
!                             1982                               *
!                                                                *
!         SUBROUTINE LS ADAPTED FROM CODE WRITTEN AT             *
!              UNIVERSITY OF WISCONSIN-MADISON                   *
!                                                                *
!      THIS CODE IS INTENDED FOR INSTRUCTIONAL USE ONLY!         *
!                                                                *
!****NOTE***** THIS CODE HAS BEEN REVISED FROM APPENDIX III      *
!              VERSION, WRITTEN FOR UNIVAC 1100/80, TO RUN ON    *
!              IBM 4381.  EVERY CHANGED STATEMENT IS ACCOMPANIED *
!              BY THE ORIGINAL STATEMENT WITH 'CCCCC' AT THE     *
!              BEGINNING.                                        *
!                                                                *
!***WARNING*** THE FORTRAN 77 PRETEST DOLOOP FEATURE            *
!              IS USED EXTENSIVLY!                               *
!                                                                *
!  PARAMETER STORAGE IN THE PAR ARRAY IS AS FOLLOWS:             *
!  1. ESTIMATED SEASONALITY PARAMETERS, 2. MEANS, 3. PHI0 TERMS, *
!  4. PHI TERMS: SERIES 1, SERIES 2, ETC., 5. THETA TERMS        *
!                                                                *
!  ZERO LAG INVERSE FUNCTION COEFFICIENTS ARE STORED AT THE      *
!  END OF THE LAST COLUMN OF THE XTX ARRAY. (SUBROUTINE EAR)     *
!  JANUARY 1995.  ALAN HECKERT MADE THE FOLLOWING CHANGES FOR    *
!  NON-PC SYSTEMS.
!  1) CHANGED REAL*4 AND REAL*8 TO REAL AND DOUBLE PRECISION     *
!     (REAL*4 ETC. NOT ANSI STANDARD).  THERE ARE STILL 2        *
!     COMPLEX*16 STATEMENTS, WHICH MAY NEED TO BE MODIFIED ON    *
!     SOME SYSTEMS.                                              *
!  2) THE COMMON BLOCK DATA WAS RENAMED DDSDAT.                  *
!  3) THE "/" IN SOME FORMAT STATEMENTS WAS MODIFIED (BOMBS ON   *
!     UNIX WITH INTERNAL READ/WRITE WHICH DATAPLOT USES TO FUNNEL*
!     ALL OUTPUT THROUGH SINGLE ROUTINE.                         *
!     UPDATED         --APRIL     1996. DDS CODE MODIFIED (ALAN):
!                                       A) SOME DIMENSIONS TO DPDDS, USE
!                                          EQUIVALENCE
!                                       B) I/O CONSISTENT WITH DATAPLOT
!                                       C) USE IERROR RATHER THAN STOP
!                                       D) INCLUDE FILE FOR DDS COMMON
!                                          BLOCKS AND PARAMETER STATEMENTS
!                                       THESE CHANGES PROPOGATE TO LOWER
!                                       LEVEL DDS ROUTINES
!*****************************************************************
!
!     CALLING SEQUENCE:
!
!        DPDDS3 - DETPAR EAR GJR INVAL LS ATSCOR ANALYS
!
!        DETPAR - .
!        EAR - MODEL XX XA
!        GJR - .
!        INVAL - EAR GJR STBLIZ
!        LS - MODEL DIF STBLIZ GJR
!        ATSCOR - .
!        ANALYS - SPECTRUM GREEN FREQ EIGEN
!
!        STBLIZ - EIGEN
!        DIF - .
!        XA - .
!        MODEL - DETPAR
!        XX - .
!        FREQ - .
!        EIGEN - .
!        GREEN - .
!        SPECTRUM - .
!
!CCCC APRIL 1996.  FOLLOWING PUT IN INCLUDE FILE
!CCCC PARAMETER (MXSER=3,MXSR2=MXSER**2,MXNOB=1024,MXPAR=45,MXINV=50,
!CCCC>          MXNOB1=MXNOB+1,MXINV1=MXINV+1)
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODD.INC'
      DIMENSION XDDS(MAXRW1,MXSER)
      DIMENSION YDDS(MAXRW1,MXSER)
!
!CCCC DOUBLE PRECISION XTX(MXINV,MXINV1),XX
!CCCC COMMON /INDEX/ INDEX,MAXLAG,NOB,NSER,NPHI0,NPHI,NSEAS
!CCCC COMMON /LAG/ LAG(MXSER,MXSER),NAR(MXSER,MXSER),NMA(MXSER)
!CCCC COMMON /FLAG/ IFMEAN,IFAT,IFSEAS,IFGREEN,IFSPECTR
!CCCC COMMON /SEASON/ NPOLY,NEXP,NSIN,ISEAS(30),SEAS(30),DELTA
!CCCC APRIL 1996.  YDDS, XDDS DIMENSIONED IN
!CCCC COMMON/DDSDAT/ YDDS(MXNOB,MXSER),XDDS(MXNOB,MXSER)
!CCCC COMMON /BLOK1/ STEP,CONTOL,ITMAX,IFREF
!CCCC COMMON /JOSHI/ XX
      DOUBLE PRECISION XTX(MXINV,MXINV1)
!CCCC APRIL 1996.  USE DPOPFI TO OPEN FILES
!
!CCCC CHARACTER*80 IFILE1
      CHARACTER (LEN=MAXFNC) :: IFILE1
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
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      DIMENSION Y1(*)
      DIMENSION PRED2(*)
      DIMENSION RES2(*)
!
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
      CHARACTER*4 ISUBN0
!
      DIMENSION AT(MAXRW2,MXSER),PAR(MXPAR),AVG(MXSER),VAR(MXSER)   &
                ,OSSQ(MXSER),SCA(MXSER)
!CCCC CHARACTER*40 TITLE
      CHARACTER FORMT*50
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'DDS3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPDDS3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,MAXNXT,N1,IORDAR,IORDMA
   52   FORMAT('IBUGA3,ISUBRO,MAXNXT,N1,IORDAR,IORDMA = ',2(A4,2X),4I8)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,53)DELTAT,NUMVAR,ILOCV
   53   FORMAT('DELTAT,NUMVAR,ILOCV = ',G15.7,2I8)
        CALL DPWRST('XXX','WRIT')
        DO 56 I=1,N1
          WRITE(ICOUT,57)I,Y1(I)
   57     FORMAT('I,Y1(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
!
      NPHI0=0
      NPHI=0
      NSEASP=0
      NSEAS=0
      NEWFLAG=0
!
      DO 2000  JJ1 = 1,MXSER
         DO 2001  JJ2 = 1,MXSER
            LAG(JJ1,JJ2)=1
2001    CONTINUE
2000  CONTINUE
      IPAR=1
      IOUT=6
      IIN=5
      INDEX=1
      IFINV=0
      IFATS=0
      IFGREEN=0
      IFSPECTR=0
!
      ITEST=1
!
!     READ INPUT DATA
!
      IF(ITEST.EQ.1)THEN
!CCCC    WRITE(IOUT,1000)
!1000     FORMAT(' THE PROGRAM READS FROM FILE "5" AND WRITES TO '
!CCCC>   ,/,' FILE "6/12."  IF YOUR EXEC IS NOT SET UP THIS WAY,',/,
!CCCC>    ' ENTER A "1", OR ENTER A "0" TO CONTINUE.  NOTE THAT ',/,
!CCCC>    ' ALL RESULTS ARE ECHOED TO THE SCREEN.')
!CCCC    READ(IIN,*) ICONT
         ICONT=0
!CCCC APRIL 1996.  SET IERROR FLAG INSTEAD OF STOP
!CCCC    IF(ICONT.EQ.1) STOP 'RE-DO YOUR EXEC TO FIT THE PROGRAM.'
         IF(ICONT.EQ.1) THEN
           WRITE(ICOUT,1001)
 1001      FORMAT('*****ERROR IN DPDDS, BAD VALUE FOR ICONT FLAG')
           CALL DPWRST('XXX','WRIT')
           IERROR='YES'
           GO TO 9999
         ENDIF
!CCCC    READ(5,10) TITLE
!CC10    FORMAT(A)
!
!CCCC    READ(5,*) NOB,NSER,DELTA,NMOD,INCPH,INCTH
         NOB=N1
         NSER=1
         DELTA=DELTAT
         NMOD=1
         INCPH=1
         INCTH=1
!
!CCCC    READ(5,*) ((NAR(J,I),J=1,NSER),NMA(I),I=1,NSER)
         NAR(1,1)=IORDAR
         NMA(1)=IORDMA
!
!CCCC    READ(5,*) STEP,CONTOL,ITMAX,IFREF
         STEP=.001
         CONTOL=.00001
         ITMAX=20
         IFREF=0
!
!CCCC    READ(5,*) IFMEAN,IFAT,IFDIF,IFLAG,IFSEAS,IFSER,IFPRT
         IFMEAN=0
         IFAT=0
         IFDIF=0
         IFLAG=0
         IFSEAS=0
         IFSER=0
         IFPRT=1
!
!CCCC    READ(5,*) IFATS,IFGREEN,IFSPECTR
         IFATS=1
         IFGREEN=1
         IFSPECTR=1
!
      ENDIF
!
!CCCC    WRITE(ICOUT,*) TITLE
!CCCC WRITE(ICOUT,'(2I5,E15.8,3I4)') NOB,NSER,DELTA,NMOD,INCPH,INCTH
!CCCC    DO15I=1,NSER
!CC15        WRITE(ICOUT,*) (NAR(J,I),J=1,NSER),NMA(I)
!CCCC  WRITE(ICOUT,'(2E15.8,2I6)') STEP,CONTOL,ITMAX,IFREF
!CCCC WRITE(ICOUT,'(7I6)') IFMEAN,IFAT,IFDIF,IFLAG,IFSEAS,IFSER,IFPRT
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,30)
   30 FORMAT(1H ,'  DDS (Data Dependent System) Modeling')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,31)
   31 FORMAT(1H ,'  S. M. Pandit, S. M. Wu, & G. A. Joshi')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,32)
   32 FORMAT(1H ,'  906-487-2153              701-231-8671')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,33)
   33 FORMAT(1H ,'  Reference--Time Series & System Analysis')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,34)
   34 FORMAT(1H ,'             With Applications. Pandit/Wu (1983)')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      IF(NSER.GE.2)THEN
         WRITE(ICOUT,41)NSER
   41    FORMAT(1H ,'Number of Time Series             = ',I8)
         CALL DPWRST('XXX','WRIT')
      ENDIF
      IF(NSER.GE.2)CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,42)N1
   42 FORMAT(1H ,'Number of Observations per Series = ',I8)
      CALL DPWRST('XXX','WRIT')
!CCCC WRITE(ICOUT,43)IORDAR
!CC43 FORMAT(1H ,'Order of Autoregressive Terms    = ',I8)
!CCCC WRITE(ICOUT,44)IORDMA
!CC44 FORMAT(1H ,'Order of Moving Average Terms    = ',I8)
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
!
!     ECHO CHECK INPUT
!CCCC WRITE(ICOUT,20) NSER
!CCCC WRITE(3,20) NSER
!CC20 FORMAT (5X,'SERIES',I4)
!CC20 FORMAT (5X,'SERIES',I4,': ARMA(',I3,6X,I3,',',I3,' )',/)
!CCCC WRITE(ICOUT,*) TITLE
!CCCC WRITE(3,30) (TITLE(JJ1),JJ1=1,17)
!CC30 FORMAT (6X,'DATA DEPENDENT SYSTEMS MODELING ROUTINE - MTU 1982
!CCCC>',
!CCCC>//,6X,'THIS PROGRAM FITS UNIVARIATE ARMA AND EXTENDED ',/,6X
!CCCC>,'MODELS TO TIME SERIES DATA, WITH OPTIONAL DETERMINISTIC TREND
!CCCC>',/,6X,'ESTIMATION FOR UNIVARIATE MODELS.',
!CCCC>'  IT IS DESIGNED TO BE USED'
!CCCC>,/,6X,'WITH THE BOOK:' ,
!CCCC>/,6X,'   "TIME SERIES AND SYSTEM ANALYSIS WITH APPLICATIONS"'
!CCCC>,/,6X,'             BY S. M. PANDIT AND S. M. WU' ,
!CCCC>//,6X,'    THIS CODE IS INTENDED FOR INSTRUCTIONAL USE ONLY!'
!CCCC>,/,2X,70('*'),////,1X,17A4)
!CCCC WRITE(ICOUT,60) IFMEAN,IFAT,IFDIF,NMOD,IFPRT,IFLAG,IFSEAS,IFSER,
!CCCC>INCPH,INCTH
!CCCC WRITE(3,60) IFMEAN,IFAT,IFDIF,NMOD,IFPRT,IFLAG,IFSEAS,IFSER,
!CCCC>INCPH,INCTH
!CC60 FORMAT(/,15X,'PROGRAM CONTROL FLAGS',/,5X,'IFMEAN:',I3,4X,
!CCCC>'IFAT:',I3,'  IFDIF:',I3,'  NMOD:',I3,'  IFPRT:',I3,
!CCCC> /,6X,'IFLAG:',
!CCCC>I3,'  IFSEAS:',I3,'  IFSER:',I3,' INCPH:',I3,'  INCTH:',I3)
!CCCC WRITE(ICOUT,70) STEP,CONTOL,ITMAX,IFREF
!CCCC WRITE(3,70) STEP,CONTOL,ITMAX,IFREF
!CC70 FORMAT (5X,'STEP=',E10.4,'   CONVERGENCE TOLERANCE=',E10.4
!CCCC>,/,5X,I5,' ITERATIONS MAX    IFREF:',I4,//)
!CCCC WRITE(ICOUT,600) MXSER,MXNOB,MXPAR,MXINV
!C600  FORMAT(/,' --- PROGRAM SIZE LIMITS ---',
!CCCC>       /,'      MAX # SERIES=',I4,
!CCCC>       /,'      MAX # OBSER =',I4,
!CCCC>       /,'      MAX # PARAM =',I4,
!CCCC>       /,'      MAX # INVER =',I4,
!CCCC>       /)
!CCCC WRITE(ICOUT,601)
!C601   FORMAT(/,' (***LAST UPDATE***13 AUG 84 /19:02:14|)',
!CCCC>       /,' (1.SUMMARY ELT IN 11.                   )',
!CCCC>       /,' (2.IF NMOD=1, AT-S IN 12.               )',
!CCCC>       /,' (3.FIX: AT(MXNOB)->AT(MXNOB1) IN MODEL  )',
!CCCC>       /,/)
!CCCC WRITE(ICOUT,40)NSER,NOB,DELTA
!CCCC WRITE(3,40)NSER,NOB,DELTA
!CC40 FORMAT (1X,I6,' SERIES WITH',I5,' OBSERVATIONS, DELTA =',E10.4,//,
!CCCC>      '    INPUT STARTING MODEL ORDERS:',/)
!CCCC    DO50I=1,NSER
!CC50       WRITE(ICOUT,*) I,(NAR(J,I),J=1,NSER),NMA(I)
!     CHECK FOR ERRORS IN INPUT
!CCCC IF(NSER.GT.MXSER) STOP'TOO MANY DATA SETS, MXSER=3'
!CCCC IF(NOB.GT.MXNOB) STOP'TOO MANY OBSERVATIONS, MXNOB=1024'
!CCCC IF(IFSEAS.GT.0.AND.IFDIF.NE.0)
!CCCC>   STOP'NUMERICAL DERIVATIVES MUST BE USED WITH SEASONALITY'
!CCCC IF(IFSEAS.GT.0.AND.NSER.GT.1)
!CCCC>   STOP'ONLY ONE SERIES MAY BE USED WITH SEASONALITY'
!CCCC IF(IFSEAS.EQ.1.AND.NMOD.LT.0)
!CCCC>   STOP'NO STOCHASTIC INITIAL VALUES FOR IFSEAS = 1'
!CCCC IF((IFSEAS.EQ.1.OR.IFSEAS.EQ.3).AND.NMOD.GT.1)
!CCCC>   STOP'ONLY ONE MODEL AT A TIME FOR IFSEAS=1 OR 3'
!
!     DETERMINE LAGS FOR MULTI-VARIATE MODELS
!
!CCCC APRIL 1996.  IN FOLLOWING BLOCK, USE IRD AS THE INPUT UNIT
         IF(NSER.GT.1)THEN
            DO 100 I=1,NSER
              IF(IFLAG.EQ.0)THEN
                DO 80 J=I+1,NSER
                  LAG(J,I)=0
80              CONTINUE
              ELSE
                IF(I.EQ.1)THEN
!CCCC             READ(5,*) (LAG(J,I),J=2,NSER)
                  READ(IRD,*) (LAG(J,I),J=2,NSER)
                ELSEIF(I.EQ.NSER)THEN
!CCCC             READ(5,*) (LAG(J,I),J=1,I-1)
                  READ(IRD,*) (LAG(J,I),J=1,I-1)
                ELSE
!CCCC             READ(5,*) (LAG(J,I),J=1,I-1),(LAG(J,I),J=I+1,NSER)
                  READ(IRD,*) (LAG(J,I),J=1,I-1),   &
                              (LAG(J,I),J=I+1,NSER)
                 ENDIF
              ENDIF
!CCCC         WRITE(ICOUT,90) I,(LAG(J,I),J=1,NSER)
!CC90         FORMAT (5X,'SERIES',I4,':  LAGS=',10(I5,','),/)
!CCCC         CALL DPWRST('XXX','WRIT')
100         CONTINUE
         ENDIF
!
!     READ SEASONALITY INPUT IF SEASONALITY FLAG IS SET
!
!CCCC APRIL 1996.  IN FOLLOWING BLOCK, USE IRD AS THE INPUT UNIT, SET
!CCCC IERROR FLAG INSTEAD OF STOP
         IF(IFSEAS.GT.0)THEN
!CCCC       READ(5,*) NPOLY,NEXP,NSIN
            READ(IRD,*) NPOLY,NEXP,NSIN
            NSEASP=NPOLY+2*NEXP+4*NSIN+1
!CCCC       IF(NSEASP.GT.30)
!CCCC>         STOP'TOO MANY SEASONALITY PARAMETERS, MAX=30'
            IF(NSEASP.GT.30)THEN
              WRITE(ICOUT,1011)
 1011         FORMAT('*****ERROR IN DPDDS--TOO MANY SEASONAILITY ',   &
                     'PARAMETERS, MAXIMUM = 30.')
              CALL DPWRST('XXX','WRIT')
              IERROR='YES'
              GO TO 9999
            ENDIF
!CCCC       READ(5,*) (SEAS(I),ISEAS(I),I=1,NSEASP)
            READ(IRD,*) (SEAS(I),ISEAS(I),I=1,NSEASP)
            WRITE(ICOUT,111) NPOLY,NEXP
111         FORMAT(5X,'SEASONALITY:',I3,' POLYNOMIAL,',I3,   &
                   ' EXPONENTIAL')
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,112) NSIN
112         FORMAT(' AND',I3,' SINUSOIDAL TRENDS')
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,113)
 113        FORMAT (' ')
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,110) (I,SEAS(I),I=1,NSEASP)
!CCCC       WRITE(3,110) NPOLY,NEXP,NSIN,(I,SEAS(I),I=1,NSEASP)
110         FORMAT(5X,5('BETA(',I2,')=',E11.4,5X))
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,116)
 116        FORMAT (' ')
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,117)
 117        FORMAT(5X,'ESTIMATION FLAGS:')
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,115) (I,ISEAS(I),I=1,NSEASP)
!CCCC       WRITE(3,115) (I,ISEAS(I),I=1,NSEASP)
115         FORMAT(7X,5('IC(',I2,')=',I4,14X))
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,116)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,116)
            CALL DPWRST('XXX','WRIT')
!
!     MODIFY SEASONALITY PARAMETERS FOR SAMPLING INTERVAL
!
            N=NPOLY+1
               DO 120 I=1,NEXP
                  N=N+2
                  SEAS(N)=SEAS(N)*DELTA
  120          CONTINUE
               DO 130 I=1,NSIN
                  N=N+2
                  SEAS(N)=SEAS(N)*DELTA
                  N=N+2
                  SEAS(N)=SEAS(N)*DELTA*6.2831853
  130          CONTINUE
!
!     LOAD SEASONALITY PARAMETERS INTO PARAMETER ARRAY
!
               IF(IFSEAS.EQ.1.OR.IFSEAS.EQ.3)THEN
                     DO 140 I=1,NSEASP
                        IF(ISEAS(I).EQ.0)THEN
                           PAR(IPAR)=SEAS(I)
                           ISEAS(IPAR)=I
                           IPAR=IPAR+1
                        ENDIF
140                  CONTINUE
                  NSEAS=IPAR-1
               ENDIF
         ENDIF
!
!     READ IN DATA SERIES
!
!CCCC AUGUST, 1995.  MODIFIY FOLLOWING LINE FOR CRAY.
!CCCC CRAY DOESN'T ALLOW FREE FORMAT FOR INTERNAL WRITE.
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DDS3')THEN
!CCCC    WRITE(ICOUT,*)'JUST BEFORE THE DEFINITION OF YDDS'
         WRITE(ICOUT,9072)
 9072    FORMAT('JUST BEFORE THE DEFINITION OF YDDS')
         CALL DPWRST('XXX','WRIT')
!CCCC    WRITE(ICOUT,*)NOB,NSER
         WRITE(ICOUT,9073)NOB,NSER
 9073    FORMAT(I8,3X,I8)
         CALL DPWRST('XXX','WRIT')
      ENDIF
!
!CCCC READ(5,*) ((YDDS(I,J),I=1,NOB),J=1,NSER)
      DO 144 I=1,NOB
         YDDS(I,1)=Y1(I)
  144 CONTINUE
                                                                                                                                  
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DDS3')THEN
!CCCC AUGUST, 1995.  MODIFIY FOLLOWING LINE FOR CRAY.
!CCCC CRAY DOESN'T ALLOW FREE FORMAT FOR INTERNAL WRITE.
!CCCC    WRITE(ICOUT,*)'THE YDDS DEFINITION LOOP HAS JUST BEEN EXECUTED'
         WRITE(ICOUT,9075)
 9075    FORMAT('THE YDDS DEFINITION LOOP HAS JUST BEEN EXECUTED')
         CALL DPWRST('XXX','WRIT')
      ENDIF
!
!     DETREND DATA TO ESTIMATE STOCHASTIC PART OF COMBINED MODELS
!
!CCCC APRIL 1996.  ADD XDDS, YDDS TO CALL LIST, NOB PASSED THROUGH
!CCCC COMMON
!CCCC IF(IFSEAS.EQ.2)   CALL DETPAR(NOB)
      IF(IFSEAS.EQ.2)   CALL DETPAR(XDDS,YDDS,MAXRW1)
!
!     FIND AVERAGE AND VARIANCE OF EACH SERIES AND PRINT
!
!CCCC WRITE(ICOUT,145)
!C145 FORMAT (//,'   CHARACTERISTICS OF THE DATA',/)
!CCCC CALL DPWRST('XXX','WRIT')
!
         DO 190 I=1,NSER
            SUM=0.0
            VAR(I)=0.0
            DO 150 J=1,NOB
                  SUM=SUM+YDDS(J,I)
  150       CONTINUE
!CCCC       AVG(I)=SUM/NOB
            AVG(I)=SUM/REAL(NOB)
            DO 160 J=1,NOB
                  VAR(I)=VAR(I)+(YDDS(J,I)-AVG(I))**2
  160       CONTINUE
            VAR(I)=VAR(I)/REAL(NOB-1)
            SDI=SQRT(VAR(I))
!
            WRITE(ICOUT,170)
  170       FORMAT(1H ,'Series      Obs.  Average      Variance    SD')
            CALL DPWRST('XXX','WRIT')
!
            WRITE(ICOUT,171) I,N1,AVG(I),VAR(I),SDI
  171       FORMAT(1H ,I3,2X,I8,4X,3G12.5)
            CALL DPWRST('XXX','WRIT')
!
!     SCALE DATA FOR EARMA MODELS
!
            IF(NSER.GT.1)THEN
               SCA(I)=VAR(I)
               IF(IFMEAN.GT.0) SCA(I)=SCA(I)+AVG(I)**2
               SCA(I)=SQRT(SCA(I))
               DO 185 J=1,NOB
                   YDDS(J,I)=YDDS(J,I)/SCA(I)
  185          CONTINUE
               AVG(I)=AVG(I)/SCA(I)
           ENDIF
!
!     SUBTRACT AVERAGE FROM DATA IF MEAN IS NOT ESTIMATED
!
           IF(IFMEAN.LE.0.AND.IFSEAS.EQ.0)THEN
              IF(IFMEAN.LT.0) AVG(I)=XDDS(-IFMEAN,I)
              DO 180 J=1,NOB
                YDDS(J,I)=YDDS(J,I)-AVG(I)
                XDDS(J,I)=YDDS(J,I)
  180         CONTINUE
           ENDIF
190      CONTINUE
!
!     CHECK FOR INITIAL VALUE FLAG
!
         IF(NMOD.LT.0)THEN
            IFINV=1
            NMOD=-NMOD
         ENDIF
!CCCC APRIL 1996.  OPEN FILES VIA THE DPOPFI ROUTINE
!
!
         IF(IFATS .EQ. 1)THEN
!CCCC          OPEN(UNIT=2,FILE='residuals.out',STATUS='UNKNOWN')
               IOUNI1=2
               IFILE1='residual.out'
               ISTAT1='UNKNOWN'
               IFORM1='FORMATTED'
               IACCE1='SEQUENTIAL'
               IPROT1='WRITE'
               ICURS1='CLOSED'
               ISUBN0='DDS3'
               IERRF1='NO'
               IREWI1='ON'
               CALL DPOPFI(IOUNI1,IFILE1,ISTAT1,IFORM1,IACCE1,IPROT1,   &
                           ICURS1,   &
                           IREWI1,ISUBN0,IERRF1,IBUGA3,ISUBRO,IERROR)
               IF(IERRF1.EQ.'YES')THEN
                 WRITE(ICOUT,1091)
 1091            FORMAT('***** ERROR IN DDS--UNABLE TO OPEN ',   &
                        'residuals.out')
                 CALL DPWRST('XXX','WRIT')
                 IERROR='YES'
                 GO TO 9999
               ENDIF
         ENDIF
         IF(IFGREEN .EQ. 1)THEN
!CCCC          OPEN(UNIT=3,FILE='green.out',STATUS='UNKNOWN')
               IOUNI2=3
               IFILE2='green.out'
               ISTAT2='UNKNOWN'
               IFORM2='FORMATTED'
               IACCE2='SEQUENTIAL'
               IPROT2='WRITE'
               ICURS2='CLOSED'
               ISUBN0='DDS3'
               IERRF2='NO'
               IREWI2='ON'
               CALL DPOPFI(IOUNI2,IFILE2,ISTAT2,IFORM2,IACCE2,IPROT2,   &
                           ICURS2,   &
                           IREWI2,ISUBN0,IERRF2,IBUGA3,ISUBRO,IERROR)
               IF(IERRF2.EQ.'YES')THEN
                 WRITE(ICOUT,1092)
 1092            FORMAT('***** ERROR IN DDS--UNABLE TO OPEN green.out')
                 CALL DPWRST('XXX','WRIT')
                 IERROR='YES'
                 GO TO 9999
               ENDIF
         ENDIF
         IF(IFSPECTR .EQ. 1)THEN
!CCCC          OPEN(UNIT=4,FILE='spectrum.out',STATUS='UNKNOWN')
               IOUNI3=4
               IFILE3='spectrum.out'
               ISTAT3='UNKNOWN'
               IFORM3='FORMATTED'
               IACCE3='SEQUENTIAL'
               IPROT3='WRITE'
               ICURS3='CLOSED'
               ISUBN0='DDS3'
               IERRF3='NO'
               IREWI3='ON'
               CALL DPOPFI(IOUNI3,IFILE3,ISTAT3,IFORM3,IACCE3,IPROT3,   &
                           ICURS3,   &
                           IREWI3,ISUBN0,IERRF3,IBUGA3,ISUBRO,IERROR)
               IF(IERRF3.EQ.'YES')THEN
                 WRITE(ICOUT,1093)
 1093            FORMAT('***** ERROR IN DDS--UNABLE TO OPEN ',   &
                        'spectrum.out')
                 CALL DPWRST('XXX','WRIT')
                 IERROR='YES'
                 GO TO 9999
               ENDIF
         ENDIF
!
!     LOOP ON NUMBER OF MODELS
!
         DO 290  MOD=1,NMOD
!
!     SET INITIAL VALUES OF MEANS IF ESTIMATED
!
            IF(IFMEAN.GT.0.AND.IFSEAS.EQ.0)THEN
               IPAR=NSEAS+1
               DO 210 I=1,NSER
                 PAR(IPAR)=AVG(I)
                 IF(IFMEAN.GT.1) PAR(IPAR)=YDDS(IFMEAN-1,I)
                 IPAR=IPAR+1
!
!     SUBTRACT AVERAGE TO RESET DATA FOR INITIAL VALUES OF NEW MODEL
!
                 DO 215 J=1,NOB
                   XDDS(J,I)=YDDS(J,I)-AVG(I)
  215            CONTINUE
  210          CONTINUE
            ENDIF
!
!     SKIP STOCHASTIC ESTIMATION FOR DETERMINISTIC MODELS
!
         NPAR=NSEAS
         IF(IFSEAS.EQ.1)THEN
             NEWFLAG=1
             GO TO 280
         ENDIF
!
!     CHECK SERIES INDEX FLAG
!
            IF(IFSER.GT.0.AND.IFSER.LE.NSER)THEN
               INDEX=IFSER
               GO TO 280
            ENDIF
!
!     LOOP ON NUMBER OF DATA SERIES
!
             INDEX=1
!
  280 CONTINUE
            IF(INDEX .LE. NSER)THEN
!CCCC       DO280 INDEX=1,NSER
             IF(NEWFLAG .EQ. 1)THEN
                 NEWFLAG = 0
                 GO TO 250
             ENDIF
!
!     FIND NUMBER OF PHI0'S AND PHI'S, AND MAXLAG FOR THIS SERIES
!
               NPHI0=0
               NPHI=0
               MAXLAG=1
               DO 230 I=INDEX+1,NSER
                  IF(LAG(I,INDEX).EQ.0)NPHI0=NPHI0+1
230            CONTINUE
               DO 235 I=1,NSER
                 ML=LAG(I,INDEX)+NAR(I,INDEX)-1
                 IF(ML.GT.MAXLAG) MAXLAG=ML
                 NPHI=NPHI+NAR(I,INDEX)
  235          CONTINUE
               MAXLAG=MAX(MAXLAG,NMA(INDEX))
               NPAR=NPHI0+NPHI+NMA(INDEX)+IPAR-1
!
!     SET INITIAL VALUES OF INITIAL AT'S IF ESTIMATED
!
                     IF(IFAT.LT.0)THEN
                        DO 240 I=1,MAXLAG
                           PAR(NPAR+I)=0.1
  240                   CONTINUE
                        NPAR=NPAR+MAXLAG
                     ENDIF
!CCCC APRIL 1996.  SET ERROR FLAG INSTEAD OF STOP
!CCCC                IF(NPAR.GT.MXPAR) STOP'TOO MANY PARAMETERS, MXPAR=45'
                     IF(NPAR.GT.MXPAR) THEN
                       WRITE(ICOUT,9110)MXPAR
                       CALL DPWRST('XXX','WRIT')
                       IERROR='YES'
                       GO TO 9999
                     ENDIF
 9110                FORMAT('***** ERROR IN DDS--MAXIMUM OF ',I5,   &
                            ' PARAMETERS EXCEEDED.')
!CCCC               WRITE(ICOUT,'(/////)')
!CCCC               CALL DPWRST('XXX','WRIT')
!                   IF(IFPRT.GT.0)
!    >              WRITE(ICOUT,*) INDEX,(NAR(J,INDEX),J=1,NSER),NMA(INDEX)
!                   CALL DPWRST('XXX','WRIT')
!
!     PURE AR MODELS
!
                  IF(NMA(INDEX).EQ.0.AND.IFINV.NE.1)THEN
                     ISIZ=NPHI+NPHI0
                     ISIZ1=ISIZ+1
!CCCC APRIL 1996.  ADD XDDS, YDDS TO ARGUMENT LIST
!CCCC                CALL EAR(XTX,NAR(1,INDEX),LAG(1,INDEX),PAR,ISIZ)
                     CALL EAR(XTX,NAR(1,INDEX),LAG(1,INDEX),PAR,ISIZ,   &
                              XDDS,YDDS,MAXRW1)
                     CALL GJR(XTX,ISIZ,4,MXINV,MXINV1,IERROR)
                     IPAR=IPAR-1
!
!     LOAD PHI0'S INTO PAR
!
                     DO 245 I=1,NPHI0
                           PAR(I+IPAR)=XTX(NPHI+I,ISIZ1)
  245                CONTINUE
                     IPAR=IPAR+NPHI0
!
!     LOAD PHI'S INTO PAR
!
                      DO 246 I=1,NPHI
                           PAR(I+IPAR)=XTX(I,ISIZ1)
  246                 CONTINUE
                     IPAR=IPAR-NPHI0+1
                     GO TO 250
                  ENDIF
!
!     READ INITIAL VALUES IF IFINV = 1 (NMOD LT 0)
!
!CCCC APRIL 1996.  USE IRD
                  IF(IFINV.EQ.1)THEN
                     IF(IFMEAN.GT.0.AND.IFSEAS.EQ.0)   &
                     READ(IRD,*) (PAR(I),I=IPAR-NSER,IPAR-1)
                     READ(IRD,*) (PAR(I),I=IPAR,NPAR)
                  ELSE
!
!     FIND INITIAL VALUES BY INVERSE FUNCTION METHOD
!
!CCCC APRIL 1995.  ADD XDDS, YDDS TO ARGUMENT LIST
!CCCC                CALL INVAL(XTX,PAR,SCA,NPAR)
                     CALL INVAL(XTX,PAR,SCA,NPAR,XDDS,YDDS,MAXRW1,   &
                                IERROR)
                  ENDIF
!
!     SCALE INITIAL PARAMETERS FOR EARMA MODELS
!
                  IF(NSER.GT.1)THEN
                     N=NSEAS+1
                     IF(IFMEAN.GE.1) N=N+NSER
!
!     SCALE ZERO LAG TERMS
!
                        DO 247 I=INDEX+1,NSER
                           IF(LAG(I,INDEX).EQ.0)THEN
                              PAR(N)=PAR(N)*SCA(I)/SCA(INDEX)
                              N=N+1
                           ENDIF
247                     CONTINUE
!
!     SCALE REST OF PHI TERMS
!
                         DO 249 I=1,NSER
                           IF(I.NE.INDEX)THEN
                             NN=N
                             SCALE=SCA(I)/SCA(INDEX)
                             DO 248 J=1,NAR(I,INDEX)
                               PAR(NN)=PAR(NN)*SCALE
                               NN=NN+1
  248                        CONTINUE
                           ENDIF
                           N=N+NAR(I,INDEX)
  249                    CONTINUE
                    ENDIF
!
!     DO NON-LINEAR LEAST SQUARES SEARCH TO FIND BEST PARAMETERS
!
  250 CONTINUE
!CCCC APRIL 1996.  ADD XDDS, YDDS TO ARGUMENT LIST
               CALL LS(NPAR,PAR,AT(1,INDEX),FORMT,IFDIF,SCA,IFPRT,   &
               SEXT,NDF,IORDAR,IORDMA,XDDS,YDDS,MAXRW1,IERROR)
               IF(IERROR.EQ.'YES')GO TO 9999
!
!     CHECK FOR AUTO-REFINEMENT OF PARAMETERS
!
                  IF(IFREF.GT.0)THEN
                     IFREF=-IFREF
                     STEP=STEP*10.**FLOAT(IFREF)
                     CONTOL=CONTOL*10.**FLOAT(IFREF)
!CCCC                IF(IFPRT.GT.0) WRITE(3,255) STEP,CONTOL
                     IF(IFPRT.GT.0) THEN
                       WRITE(ICOUT,256)
256                    FORMAT(' ')
                       CALL DPWRST('XXX','WRIT')
                       WRITE(ICOUT,256)
                       CALL DPWRST('XXX','WRIT')
                       WRITE(ICOUT,255) STEP
255                    FORMAT(' NEW CONVERGENCE PARAMETERS:',5X,'STEP=',   &
                              E12.4,5X)
                       CALL DPWRST('XXX','WRIT')
                       WRITE(ICOUT,257) CONTOL
257                    FORMAT(1X,' CONVERGENCE TOLERANCE=',E12.4)
                       CALL DPWRST('XXX','WRIT')
                     ENDIF
!
!     REDO ESTIMATION AND RESTORE ESTIMATION PARAMETERS
!
!CCCC APRIL 1996.  ADD XDDS, YDDS TO ARGUMENT LIST
                     CALL LS(NPAR,PAR,AT(1,INDEX),FORMT,IFDIF,SCA,IFPRT,   &
                             SEXT,NDF,IORDAR,IORDMA,XDDS,YDDS,MAXRW1,   &
                             IERROR)
                     IF(IERROR.EQ.'YES')GO TO 9999
                     IFREF=-IFREF
                     STEP=STEP*10.**FLOAT(IFREF)
                     CONTOL=CONTOL*10.**FLOAT(IFREF)
                  ENDIF
!
!CCCC COPY PREDICTED VALUES AND RESIDUALS INTO PRED2 AND RES2
!CCCC FOR USE BY DATAPLOT
!
      DO 750 I=1,NOB
         RES2(I)=AT(I,1)
         PRED2(I)=Y1(I)-RES2(I)
  750 CONTINUE
      RESSD=SEXT
      RESDF=NDF
!
!     PRINT THE RESIDUALS FOR THE CURRENT MODEL
!
!CCCC APRIL 1996.  USE IOUNI1 INSTEAD OF HARD-CODED 2.
        IF(IFATS .EQ. 1)THEN
          WRITE(IOUNI1,771)
  771     FORMAT('This is Dataplot file  RESIDUAL.OUT  (DDS command)')
          WRITE(IOUNI1,772) NAR(1,INDEX), NMA(INDEX)
  772     FORMAT('Residuals for ARMA(',I2,',',I2,') model')
          WRITE(IOUNI1,773)
  773     FORMAT(1H )
          WRITE(IOUNI1,774)
  774     FORMAT('residuals')
          WRITE(IOUNI1,775)
  775     FORMAT('--------------------------------------------(line 5)')
          DO 776  I =1,NSER
!CCCC       WRITE(2,777) I
!C777       FORMAT('SERIES NUMBER', I3)
            DO 778 J=1,NOB
              WRITE(IOUNI1,779) AT(J,I)
779           FORMAT(F10.3)
778         CONTINUE
776       CONTINUE
        ENDIF
!
!     FIND AND PRINT F-TEST VALUE
!
         IF(NMOD.GT.1)THEN
               IF(MOD.GT.1)THEN
                  FTST=(OSSQ(INDEX)/AT(NOB+1,INDEX)-1.)*   &
                  FLOAT((NOB-NPAR)/(NSER*INCPH+INCTH))
                  WRITE(ICOUT,264)
 264              FORMAT (' ')
                  CALL DPWRST('XXX','WRIT')
                  WRITE(ICOUT,264)
                  CALL DPWRST('XXX','WRIT')
                  WRITE(ICOUT,265) FTST
!CCCC             WRITE(3,265) FTST
265               FORMAT(' F-TEST WITH LAST MODEL FOR THIS SERIES ='   &
                         ,F10.3)
                  CALL DPWRST('XXX','WRIT')
               ENDIF
            OSSQ(INDEX)=AT(NOB+1,INDEX)
         ENDIF
      NDEX = NSER
      IF(IFSER.GT.0) NDEX = IFSER
!CCCC IF(NMOD.EQ.1) WRITE(12,'(5E15.8)') (AT(I,NDEX),I=1,NOB)
!
!     FIND CORRELATIONS OF RESIDUALS
!
!CCCC APRIL 1996.  ADD MXNOB1, MXSER TO ARGUMENT LIST
!CCCC          CALL ATSCOR(AT,NOB,INDEX,IFSER)
               CALL ATSCOR(AT,MAXRW1,NOB,INDEX,IFSER,MXSER)
!
!     FIND ROOTS, FREQUENCIES, DAMPING, AND GREEN'S FUNCTION COEFFICIENTS
!
!CCCC APRIL 1996.  IN FOLLOWING BLOCK OF CODE, USE IOUNI2 RATHER THAN 3.
               IF(IFSEAS.NE.1.AND.IFPRT.GT.0)THEN
                 IF(IFGREEN .EQ. 1)THEN
                   WRITE(IOUNI2,781)
  781              FORMAT('This is Dataplot file GREEN.OUT ',   &
                          '(DDS command)')
                   WRITE(IOUNI2,782) NAR(1,INDEX), NMA(INDEX)
  782              FORMAT('Greens function for ARMA(',I2,',',I2,   &
                          ') model')
                   WRITE(IOUNI2,783)
  783              FORMAT(1H )
                   WRITE(IOUNI2,784)
  784              FORMAT('index   Green (total)   Green (comp. 1) ...')
                   WRITE(IOUNI2,785)
  785              FORMAT('------------------------------------------',   &
                          '(line 5)')
                 ENDIF
!CCCC APRIL 1996.  IN FOLLOWING BLOCK OF CODE, USE IOUNI3 RATHER THAN 4.
                 IF(IFSPECTR .EQ. 1)THEN
                   WRITE(IOUNI3,791)
  791              FORMAT('This is Dataplot file  SPECTRUM.OUT  ',   &
                          '(DDS command)')
                   WRITE(IOUNI3,792) NAR(1,INDEX), NMA(INDEX)
  792              FORMAT('Spectrum for ARMA(',I2,',',I2,') model')
                   WRITE(IOUNI3,793)
  793              FORMAT(1H )
                   WRITE(IOUNI3,794)
  794              FORMAT('index   Spectrum (total)   Spectrum ',   &
                          '(comp. 1) ','...')
                   WRITE(IOUNI3,795)
  795              FORMAT('------------------------------------------',   &
                          '(line 5)')
                 ENDIF
!CCCC APRIL 1996.  ADD I/O UNITS TO ARGUMENT LIST, DELTA PASSED
!CCCC THROUGH COMMON.
!CCCC            CALL ANALYS(PAR,DELTA,VAR)
                 CALL ANALYS(PAR,VAR,IOUNI2,IOUNI3)
               ENDIF
!
!     UPDATE MODEL ORDERS FOR NEXT MODEL
!
                  DO 270 I=1,NSER
                     NAR(I,INDEX)=NAR(I,INDEX)+INCPH
  270             CONTINUE
                  NMA(INDEX)=NMA(INDEX)+INCTH
!CCCC             WRITE(ICOUT,285)
!CCCC             CALL DPWRST('XXX','WRIT')
                  INDEX=INDEX+1
                  IF(IFSER .EQ. 0) GO TO 280
        ENDIF
!CCCC       WRITE(3,285)
!C285       FORMAT (///,1X,72('*'),//)
290      CONTINUE
!
!CCCC WRITE(IOUT,'(42A1)')  ' A SUMMARY OF MODEL(S) IS(ARE) IN FILE 6.'
!CCCC IF(NMOD.EQ.1) WRITE(IOUT,2004) NDEX
!2004  FORMAT(' ',I5,'-TH SERIES RESIDUALS ARE IN FILE 12.')
!
!CCCC WRITE OUT MESSAGES ABOUT THE OUTPUT FILES
!
      IF(IFATS .EQ. 1)THEN
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','WRIT')
         WRITE(ICOUT,770)
  770    FORMAT(1H ,'Residuals       written to file   RESIDUAL.OUT')
         CALL DPWRST('XXX','WRIT')
      ENDIF
      IF(IFGREEN .EQ. 1)THEN
         WRITE(ICOUT,780)
  780    FORMAT(1H ,'Greens function written to file      GREEN.OUT')
         CALL DPWRST('XXX','WRIT')
      ENDIF
      IF(IFSPECTR .EQ. 1)THEN
         WRITE(ICOUT,790)
  790    FORMAT(1H ,'Spectrum        written to file   SPECTRUM.OUT')
         CALL DPWRST('XXX','WRIT')
      ENDIF
!
!CCCC APRIL 1996.  USE DPCLFI ROUTINE
      IF(IFATS .EQ. 1)THEN
!CCCC   CLOSE(2)
!
        IENDF1='OFF'
        IREWI1='ON'
        CALL DPCLFI(IOUNI1,IFILE1,ISTAT1,IFORM1,IACCE1,IPROT1,ICURS1,   &
        IENDF1,IREWI1,ISUBN0,IERRF1,IBUGA3,ISUBRO,IERROR)
        IF(IERRF1.EQ.'YES')THEN
          WRITE(ICOUT,9211)
 9211     FORMAT('**** ERROR IN DDS TRYING TO CLOSE RESIDUAL.OUT FILE.')
          IERROR='YES'
          GO TO 9999
        ENDIF
!
      ENDIF
      IF(IFGREEN .EQ. 1)THEN
!CCCC   CLOSE(3)
        IENDF2='OFF'
        IREWI2='ON'
        CALL DPCLFI(IOUNI2,IFILE2,ISTAT2,IFORM2,IACCE2,IPROT2,ICURS2,   &
        IENDF2,IREWI2,ISUBN0,IERRF2,IBUGA3,ISUBRO,IERROR)
        IF(IERRF2.EQ.'YES')THEN
          WRITE(ICOUT,9221)
 9221     FORMAT('**** ERROR IN DDS TRYING TO CLOSE GREEN.OUT FILE.')
          IERROR='YES'
          GO TO 9999
        ENDIF
!
      ENDIF
      IF(IFSPECTR .EQ. 1)THEN
!CCCC   CLOSE(4)
        IENDF3='OFF'
        IREWI3='ON'
        CALL DPCLFI(IOUNI3,IFILE3,ISTAT3,IFORM3,IACCE3,IPROT3,ICURS3,   &
        IENDF3,IREWI3,ISUBN0,IERRF3,IBUGA3,ISUBRO,IERROR)
        IF(IERRF3.EQ.'YES')THEN
          WRITE(ICOUT,9231)
 9231     FORMAT('**** ERROR IN DDS TRYING TO CLOSE SPECTRUM.OUT FILE.')
          IERROR='YES'
          GO TO 9999
        ENDIF
      ENDIF
!
!CCCC STOP
!CCCC APRIL 1996.  ADD FOLLOWING LINE
 9999 CONTINUE
      RETURN
      END SUBROUTINE DPDDS3
      SUBROUTINE DETPAR(XDDS,YDDS,MAXRW1)
!     ..........DETPAR..........
!CCCC APRIL 1996.  ADD XDDS, YDDS TO THE CALL LIST, PASS NOB THROUGH
!CCCC COMMON
!CCCC SUBROUTINE DETPAR(NOB)
!
!     PURPOSE--SUBTRACTS THE DETERMINISTIC TRENDS FROM THE DATA
!
!CCCC COMMENT OUT FOLLOWING 3 LINES, ADD DDS INCLUDE FILE
!CCCC PARAMETER (MXSER=3,MXNOB=1024,MXPAR=45)
!CCCC COMMON/DDSDAT/ YDDS(MXNOB,MXSER),XDDS(MXNOB,MXSER)
!CCCC COMMON /SEASON/ NPOLY,NEXP,NSIN,ISEAS(30),SEAS(30),DELTA
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODD.INC'
      DIMENSION XDDS(MAXRW1,MXSER)
      DIMENSION YDDS(MAXRW1,MXSER)
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
       DO 40  IT=1,NOB
            N=2
            S=SEAS(1)
               DO 10 I=1,NPOLY
                  S=S+SEAS(N)*(DELTA*IT)**I
                  N=N+1
  10           CONTINUE
               DO 20 I=1,NEXP
                  S=S+SEAS(N)*EXP(SEAS(N+1)*IT)
                  N=N+2
  20           CONTINUE
               DO 30 I=1,NSIN
                  S=S+SEAS(N)*EXP(SEAS(N+1)*IT)*   &
                    (SEAS(N+2)*SIN(SEAS(N+3)*IT)+SQRT(1-SEAS(N+2)**2)*   &
                    COS(SEAS(N+3)*IT))
                 N=N+4
  30           CONTINUE
            XDDS(IT,1)=YDDS(IT,1)-S
  40  CONTINUE
      RETURN
      END SUBROUTINE DETPAR
                                                                                                                                  
!     ..........EAR..........
                                                                                                                                  
      SUBROUTINE EAR(XTX,NAR2,LAG2,PAR,ISIZ,XDDS,YDDS,MAXRW1)
!CCCC APRIL 1996.  ADD XDDS, YDDS ARGUMENTS
!CCCC RENAME LAG AND NAR TO LAG2 AND NAR2 TO AVOID NAME CONFLICT
!CCCC WITH DPCODD.INC COMMON BLOCK
!CCCC SUBROUTINE EAR(XTX,NAR,LAG,PAR,ISIZ)
!
!     PURPOSE--FORMS THE XTX AND XTY MATRICES FOR EAR MODELS
!
!CCCC APRIL 1996.  FOLLOWING DECLARATIONS IN DDS INCLUDE FILE
!CCCC PARAMETER (MXSER=3,MXNOB=1024,MXPAR=45,MXINV=50,MXINV1=MXINV+1)
      DOUBLE PRECISION XTX,SUM
!CCCC COMMON /INDEX/ INDEX,MAXLAG,NOB,NSER,NPHI0,NPHI,NSEAS
!CCCC COMMON /FLAG/ IFMEAN,IFAT,IFSEAS,IFGREEN,IFSPECTR
!CCCC COMMON/DDSDAT/ YDDS(MXNOB,MXSER),XDDS(MXNOB,MXSER)
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODD.INC'
      DIMENSION XDDS(MAXRW1,MXSER)
      DIMENSION YDDS(MAXRW1,MXSER)
      DIMENSION ATEMP(1)
!
!CCCC APRIL 1996.  GLOBALLY RENAME LAG AND NAR TO LAG2 AND NAR2
!CCCC DIMENSION XTX(MXINV,MXINV1),LAG(MXSER),LAGT(MXSER)
!CCCC DIMENSION PAR(MXPAR),NAR(MXSER)
      DIMENSION XTX(MXINV,MXINV1),LAG2(MXSER),LAGT(MXSER)
      DIMENSION PAR(MXPAR),NAR2(MXSER)
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!
!     SUBTRACT MEAN AND SEASONALITY FOR INITIAL VALUES
!
!CCCC APRIL 1996.  ADD XDDS, YDDS TO ARGUMENT LIST
!CCCC IF(IFSEAS.EQ.3) CALL MODEL(PAR,AT,0)
!CCCC IF(IFSEAS.EQ.3) CALL MODEL(PAR,AT,0,XDDS,YDDS)
      IF(IFSEAS.EQ.3) CALL MODEL(PAR,ATEMP,0,XDDS,YDDS,MAXRW1)
      AT=ATEMP(1)
!
!     SET TEMPORARY LAGS
!
      DO 10 I=1,NSER
!CCCC   LAGT(I)=LAG(I)
        LAGT(I)=LAG2(I)
        IF(LAGT(I).EQ.0) LAGT(I)=1
   10 CONTINUE
!
!     FORM INVARIANT PART OF XTX BY BLOCK COLUMNS
!
      IROW=1
      ICOL=1
      DO 30 I=1,NSER
!
!     FORM DIAGONAL BLOCK
!
!CCCC APRIL 1996.  ADD MXINV, MXINV1 TO ARGUMENT LIST, RENAME TO
!CCCC AVOID CONFLICT WITH XX COMMON BLOCK
!CCCC   CALL XX(XDDS(2-LAGT(I),I),XTX,NAR(I),IROW,ICOL,MAXLAG,NOB)
        CALL XXZ(XDDS(2-LAGT(I),I),XTX,NAR2(I),IROW,ICOL,MAXLAG,NOB,   &
                MXINV,MXINV1)
!
!     FORM REST OF COLUMN
!
         DO 20 J=I+1,NSER
!CCCC      IROW=IROW+NAR(J-1)
           IROW=IROW+NAR2(J-1)
!CCCC      CALL XA(XDDS(2-LAGT(I),I),XDDS(2-LAGT(J),J),XTX,NAR(I),
!CCCC APRIL 1996.  ADD ARRAY DIMENSION TO ARGUMENT LIST
!CCCC>             NAR(J),IROW,ICOL,MAXLAG,NOB)
           CALL XA(XDDS(2-LAGT(I),I),XDDS(2-LAGT(J),J),XTX,NAR2(I),   &
                   NAR2(J),IROW,ICOL,MAXLAG,NOB,MXINV,MXINV1)
  20     CONTINUE
!CCCC    ICOL=ICOL+NAR(I)
         ICOL=ICOL+NAR2(I)
         IROW=ICOL
  30  CONTINUE
!
!    FORM IMMEDIATE BLOCKS - LOOP DOWN THE ROWS
!
      DO 50 I=INDEX+1,NSER
!CCCC   IF(LAG(I).EQ.0)THEN
        IF(LAG2(I).EQ.0)THEN
          ICOL=1
!
!     LOOP ACROSS THE BLOCKS
!
          DO 40 J=1,NSER
!
!           LOOP ON ELEMENTS PER BLOCK
!
!CCCC       DO40K=1,NAR(J)
            DO 45 K=1,NAR2(J)
              SUM=0.0
              DO 35 L=MAXLAG+1,NOB
                SUM=SUM-XDDS(L-K-LAGT(J)+1,J)*XDDS(L,I)
  35          CONTINUE
              XTX(IROW,ICOL)=SUM
              ICOL=ICOL+1
  45        CONTINUE
  40     CONTINUE
!
!     FORM IMMEDIATE DIAGONAL BLOCK
!
          DO 70 J=INDEX+1,I
!CCCC       IF(LAG(J).EQ.0)THEN
            IF(LAG2(J).EQ.0)THEN
              SUM=0.0
              DO 60 K=MAXLAG+1,NOB
                SUM=SUM+XDDS(K,I)*XDDS(K,J)
  60          CONTINUE
              XTX(IROW,ICOL)=SUM
              ICOL=ICOL+1
            ENDIF
70        CONTINUE
          IROW=IROW+1
        ENDIF
50    CONTINUE
!
!     FORM XTY - SECTION CORRESPONDING TO INVARIANT PART
!
      IROW=1
      DO 90 I=1,NSER
!CCCC   DO90J=1,NAR(I)
        DO 95 J=1,NAR2(I)
          SUM=0.0
          DO 80 K=MAXLAG+1,NOB
            SUM=SUM+XDDS(K-J-LAGT(I)+1,I)*XDDS(K,INDEX)
  80      CONTINUE
          XTX(IROW,ISIZ+1)=SUM
          IROW=IROW+1
  95    CONTINUE
  90  CONTINUE
!
!     IMMEDIATE PART
!
      DO 110 I=INDEX+1,NSER
!CCCC   IF(LAG(I).EQ.0)THEN
        IF(LAG2(I).EQ.0)THEN
          SUM=0.0
          DO 100 J=MAXLAG+1,NOB
            SUM=SUM-XDDS(J,I)*XDDS(J,INDEX)
  100     CONTINUE
          XTX(IROW,ISIZ+1)=SUM
          IROW=IROW+1
       ENDIF
  110 CONTINUE
!
!    DEFINE UPPER TRIANGLE OF XTX
!
      DO 120 I=1,ISIZ-1
        DO 125 J=I+1,ISIZ
          XTX(I,J)=XTX(J,I)
  125   CONTINUE
  120 CONTINUE
      RETURN
      END SUBROUTINE EAR
                                                                                                                                  
!     ..........GJR..........
                                                                                                                                  
      SUBROUTINE GJR (A,N,IW,ISIZ,ISIZ1,IERROR)
!CCCC APRIL 1996.  ADD IERROR FLAG
!CCCC SUBROUTINE GJR (A,N,IW,ISIZ,ISIZ1)
!
!     PURPOSE--DOES A GAUSS-JORDAN REDUCTION
!
!CCCC APRIL 1996.  USE DPCODD.INC
!CCCC PARAMETER (MXINV=50)
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODD.INC'
      CHARACTER*4 IERROR
!
      DOUBLE PRECISION A
      DIMENSION A(ISIZ,ISIZ1),JC(MXINV)
!     IW=:, 1- FIND INVERSE, 4- SOLVE AX=B, 5- BOTH
!     JC IS THE PERMUTATION VECTOR
!     KI IS THE OPTION KEY FOR MATRIX INVERSION
!     L IS THE COLUMN CONTROL FOR AX=B
!     M IS THE COLUMN CONTOL FOR MATRIX INVERSION
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      K=0
      M=1
      L=N+(IW/4)
      KI=2-MOD(IW,2)
!CCCC GO TO (10,30), KI
      IF(KI.EQ.1)THEN
!     INITIALIZE JC FOR INVERSION
        DO 20 I=1,N
          JC(I)=I
  20    CONTINUE
      ENDIF
!     SEARCH FOR PIVOT ROW
      DO 120 I=1,N
!CCCC   GO TO (50,40), KI
        IF(KI.EQ.2) M=I
        IF(I.EQ.N) GO TO 95
        X=-1.
        DO 60 J=I,N
          IF(X.GT.ABS(A(J,I))) GO TO 60
          X=ABS(A(J,I))
          K=J
60      CONTINUE
        IF(K.EQ.I) GO TO 95
!CCCC   GO TO (70,80), KI
        IF(KI.EQ.1)THEN
            MU=JC(I)
            JC(I)=JC(K)
            JC(K)=MU
        ENDIF
!       INTERCHANGE ROW I AND ROW K
        DO 90 J=M,L
          X=A(I,J)
          A(I,J)=A(K,J)
          A(K,J)=X
  90    CONTINUE
!
!       TEST FOR SINGULARITY
!
  95    CONTINUE
        IF(ABS(A(I,I)).EQ.0.)THEN
!
!         MATRIX IS SINGULAR
!
!CCCC     APRIL 1996.  USE IERROR FLAG RATHER THAN STOP
!CCCC     PRINT*,'***ERROR*** IN GJR. MATRIX IS SINGULAR'
!CCCC     STOP'line 714'
          WRITE(ICOUT,9011)
 9011     FORMAT('***** ERROR--FROM DDS ROUTINE GJR, SINGULAR MATRIX ',   &
                 'ENCOUNTERED.')
          IERROR='YES'
          GO TO 9999
        ENDIF
        X=A(I,I)
        A(I,I)=1.
!
!       REDUCTION OF THE I-TH ROW
!
        DO 100 J=M,L
          A(I,J)=A(I,J)/X
100     CONTINUE
!
!       REDUCTION OF ALL REMAINING ROWS
!
        DO 125 K=1,N
          IF(K.EQ.I) GO TO 120
          X=A(K,I)
          A(K,I)=0.
          DO 110 J=M,L
            A(K,J)=A(K,J)-X*A(I,J)
110       CONTINUE
125     CONTINUE
120   CONTINUE
!
!     AX=B AND DET.(A) ARE NOW COMPUTED
!
!CCCC GO TO (130,180), KI
!
!     PERMUTATION OF THE COLUMNS FOR MATRIX INVERSION
!
      IF(KI.EQ.1)THEN
         DO 170 J=1,N
            IF(JC(J).EQ.J) GO TO 170
            JJ=J+1
            DO 140 I=JJ,N
              IF(JC(I).EQ.J) GO TO 150
140         CONTINUE
150         JC(I)=JC(J)
            DO 160 K=1,N
              X=A(K,I)
              A(K,I)=A(K,J)
              A(K,J)=X
160         CONTINUE
170      CONTINUE
      ENDIF
      JC(1)=N
!
!CCCC APRIL 1996.  ADD FOLLOWING LINE
 9999 CONTINUE
      RETURN
      END SUBROUTINE GJR 
                                                                                                                                  
!     ..........INVAL..........
                                                                                                                                  
      SUBROUTINE INVAL(XTX,PAR,SCA,NPAR,XDDS,YDDS,MAXRW1,IERROR)
!CCCC SUBROUTINE INVAL(XTX,PAR,SCA,NPAR)
!CCCC APRIL 1996.  ADD TO ARGUMENT LIST
!
!     PURPOSE--FINDS THE INITIAL VALUES OF THE STOCHASTIC PARAMETER
!              USING THE INVERSE FUNCTION METHOD
!
!CCCC APRIL 1996.  USE DDS INCLUDE FILE FOR FOLLOWING.
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODD.INC'
      DIMENSION XDDS(MAXRW1,MXSER)
      DIMENSION YDDS(MAXRW1,MXSER)
      CHARACTER*4 IERROR
!CCCC CHARACTER*4 IFLAG
!
!CCCC PARAMETER (MXSER=3,MXNOB=1024,MXPAR=45,MXINV=50,MXINV1=MXINV+1)
      DOUBLE PRECISION XTX(MXINV,MXINV1)
!CCCC COMMON /INDEX/ INDEX,MAXLAG,NOB,NSER,NPHI0,NPHI,NSEAS
!CCCC COMMON /LAG/ LAG(MXSER,MXSER),NAR(MXSER,MXSER),NMA(MXSER)
!CCCC COMMON /FLAG/ IFMEAN,IFAT,IFSEAS,IFGREEN,IFSPECTR
      DIMENSION PAR(MXPAR),N(MXSER),THETA(MXPAR),SCA(MXSER)
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
      IF(ISUBG4.EQ.'NVAL')THEN
        WRITE(ICOUT,52)NPAR
   52   FORMAT('NPAR = ',I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!     FIND MAXIMUM AUTOREGRESSIVE ORDER AND INVERSE FUNCTION ORDER
!
      NM=1
      DO 10 I=1,NSER
         IF(NAR(I,INDEX).GT.NM) NM=NAR(I,INDEX)
  10  CONTINUE
!
!     SET INDEXES
!
      M=NMA(INDEX)
      NP=0
      IF(NAR(INDEX,INDEX).EQ.0.OR.NM.EQ.1.AND.M.EQ.1) NP=1
      NM=MAX(NM,M)+M+NP
      ISIZ=NSER*NM+NPHI0
      NXTY=ISIZ+1
      OFFSET=NSEAS
      IF(IFMEAN.GT.0.AND.IFSEAS.EQ.0) OFFSET=OFFSET+NSER
      NN=INT(OFFSET)+NPHI0+NPHI
      MXLAG=MAXLAG
      MAXLAG=1
!
!     SET AR ORDERS AND FIND NEW MAXIMUM LAG
!
      DO 20 I=1,NSER
         N(I)=NM
         ML=NM+LAG(I,INDEX)-1
         IF(ML.GT.MAXLAG) MAXLAG=ML
  20  CONTINUE
!
!     FIT EAR(NM) MODEL TO GET INVERSE FUNCTION COEFFICIENTS
!
!CCCC APRIL 1996.  ARGUMENT LIST FOR FOLLOWING ROUTINES
!CCCC CALL EAR(XTX,N,LAG(1,INDEX),PAR,ISIZ)
!CCCC CALL GJR(XTX,ISIZ,4,MXINV,MXINV1)
      CALL EAR(XTX,N,LAG(1,INDEX),PAR,ISIZ,XDDS,YDDS,MAXRW1)
      CALL GJR(XTX,ISIZ,4,MXINV,MXINV1,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9999
!
!     UNSCALE INVERSE FUNCTION COEFFICIENTS
!
      K=1
      DO 22 I=1,NSER
        IF(I.NE.INDEX)THEN
          SCALE=SCA(INDEX)/SCA(I)
          KK=K
          DO 21 J=1,N(I)
            XTX(KK,NXTY)=XTX(KK,NXTY)*SCALE
            KK=KK+1
  21      CONTINUE
        ENDIF
        K=K+N(I)
  22  CONTINUE
!
!     UNSCALE ZERO LAG TERMS
!
         DO 23 I=INDEX+1,NSER
               IF(LAG(I,INDEX).EQ.0)THEN
                  XTX(K,NXTY)=XTX(K,NXTY)*SCA(INDEX)/SCA(I)
                  K=K+1
               ENDIF
23       CONTINUE
!
!CCCC WRITE(ICOUT,25) (XTX(I,NXTY),I=1,ISIZ)
!CC25 FORMAT (//,' THE INVERSE FUNCTION COEFFICIENTS ARE:',
!CCCC>/,9(/,8E14.6))
!CCCC CALL DPWRST('XXX','WRIT')
!
!     FIND INITIAL THETA'S
!
!CCCC APRIL 1996.  RENAME MP1 TO MP1Z TO AVOID COMMON BLOCK CONFLICT
!CCCC MP1=M+1
      MP1Z=M+1
      DO 40 J=1,M
!CCCC   DO40K=1,MP1
        DO 45 K=1,MP1Z
          SUM=0.0
          DO 30 I=0,(NSER-1)*NM,NM
            SUM=SUM+XTX(I+J+K,NXTY)
  30      CONTINUE
          XTX(J,K)=SUM
  45    CONTINUE
  40  CONTINUE
!CCCC APRIL 1996.  ADD IERROR TO ARGUMENT LIST
!CCCC CALL GJR(XTX,M,4,MXINV,MXINV1)
      CALL GJR(XTX,M,4,MXINV,MXINV1,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9999
!
!     CHECK FOR INVERTIBILITY AND FIX IF UNSTABLE
!
         DO 50 I=1,M
!CCCC       II=MP1-I
            II=MP1Z-I
!CCCC       THETA(I)=XTX(II,MP1)
            THETA(I)=XTX(II,MP1Z)
  50     CONTINUE
!CCCC    PRINT*,'THETAS BEFORE STBLIZ',(THETA(I),I=1,NMA(INDEX))  @@DIA
         CALL STBLIZ(THETA,M,IERROR)
!CCCC    PRINT*,'THETAS AFTER STBLIZ',(THETA(I),I=1,NMA(INDEX))  @@DIA
!
!     LOAD THETAS INTO PAR
!
         DO 60 I=1,M
            PAR(NN+I)=THETA(I)
  60     CONTINUE
!
!     FIND INITIAL PHI'S
!
         JPAR=INT(OFFSET)+NPHI0+1
         L=INT(OFFSET)+1
         DO 80 I=1,NSER
            IPAR=(I-1)*NM+1
!
!     FIND PHI0'S FOR SERIES I
!
            PHI0=0.0
            IF(I.EQ.INDEX) PHI0=1.0
            IF(LAG(I,INDEX).EQ.0)THEN
               IZHU = NSER*NM+L-INT(OFFSET)
               PAR(L)=XTX(IZHU,NXTY)
               PHI0=PAR(L)
               L=L+1
            ENDIF
!
!     FIND REST OF PHI'S FOR SERIES I
!
            DO 85 J=1,NAR(I,INDEX)
               SUM=0.0
               IF(J.LE.M) SUM=THETA(J)*PHI0
               DO 70 K=1,MIN(J-1,M)
                   SUM=SUM-THETA(K)*XTX(IPAR-K,NXTY)
  70           CONTINUE
               PAR(JPAR)=XTX(IPAR,NXTY)+SUM
               JPAR=JPAR+1
               IPAR=IPAR+1
  85        CONTINUE
  80     CONTINUE
!
!     PRINT INITIAL PARAMETER VALUES
!
!CCCC WRITE(ICOUT,90) (PAR(I),I=1,NPAR)
!CC90 FORMAT (//,' INITIAL PARAMETERS =',9E12.5)
!CCCC CALL DPWRST('XXX','WRIT')
      MAXLAG=MXLAG
!CCCC APRIL 1996.  ADD FOLLOWING LINE
 9999 CONTINUE
      RETURN
      END SUBROUTINE INVAL
                                                                                                                                  
!     ..........LS..........
                                                                                                                                  
      SUBROUTINE LS (NPAR,PAR,F,FORMT,IFDIF,SCA,IFPRT,SEXT,NDF,   &
                     IORDAR,IORDMA,XDDS,YDDS,MAXRW1,IERROR)
!
!     PURPOSE--DOES A MARQUART-LEVENBERG TYPE NON-LINEAR SEARCH
!
!CCCC APRIL 1996.  USE DDS INCLUDE FILE FOR SOME OF THE FOLLOWING
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODD.INC'
      INCLUDE 'DPCOZZ.INC'
!
      DIMENSION XDDS(MAXRW1,MXSER)
      DIMENSION YDDS(MAXRW1,MXSER)
      DIMENSION F(*)
!
      CHARACTER*4 IERROR
!CCCC PARAMETER (MXSER=3,MXNOB=1024,MXNOB1=MXNOB+1,MXPAR=45,
!CCCC>MXPAR1=MXPAR+1)
      LOGICAL LG,FLAG
      CHARACTER FORMT*50
      DOUBLE PRECISION PIVOT,CMULT,DENOM,SSRED,TRED
!CCCC DOUBLE PRECISION A(MXPAR1,MXPAR1),PARB(MXPAR),XX,FLR
      DOUBLE PRECISION A(MXPAR1,MXPAR1),PARB(MXPAR),FLR
!CCCC COMMON /INDEX/ INDEX,MAXLAG,NOB,NSER,NPHI0,NPHI,NSEAS
!CCCC COMMON /LAG/ LAG(MXSER,MXSER),NAR(MXSER,MXSER),NMA(MXSER)
!CCCC COMMON /FLAG/ IFMEAN,IFAT,IFSEAS,IFGREEN,IFSPECTR
!CCCC COMMON /SEASON/ NPOLY,NEXP,NSIN,ISEAS(30),SEAS(30),DELTA
!CCCC COMMON /BLOK1/ STEP,CONTOL,ITMAX,IFREF
!CCCC COMMON /JOSHI/ XX
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      DIMENSION PAR(MXPAR),CHMAX(MXPAR),Z(MXNOB,MXPAR1)
      DIMENSION SS(3),FL(3),FD(4),SD(4),LSTP(MXPAR1),SPDA(MXPAR1),   &
                SCA(MXSER)
!CCCC APRIL 1996.  ADD FOLLOWING EQUIVALENCE
!
      EQUIVALENCE (GARBAG(IGARB1),Z(1,1))
!
      IF(ISUBG4.EQ.'LS  ')THEN
        WRITE(ICOUT,52)FORMT
   52   FORMAT('FORMT ',A50)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      FINF=1.E19
!
!CCCC IF(IFPRT.GT.0) WRITE(ICOUT,10) NPAR
!CCCC IF(IFPRT.GT.0) WRITE(3,10) NPAR
!CC10 FORMAT (//,'   VERSION 4R1 OF LS, 09-03-82, WITH',I4,
!CCCC>' PARAMETERS',/)
!CCCC IF(IFPRT.GT.0) CALL DPWRST('XXX','WRIT')
!
      DEL=-.001
      BNDLW=-FINF
      BNDUP=FINF
      DO 20 I=1,NPAR
         CHMAX(I)=.2*ABS(PAR(I))
20    CONTINUE
!CCCC APRIL 1996.  SET ERROR FLAG INSTEAD OF STOP.
      DO 60 I=1,NPAR
!CCCC    IF(PAR(I).EQ.0.0) STOP 'A PARAMETER IS EQUAL TO ZERO'
         IF(PAR(I).EQ.0.0) THEN
           WRITE(ICOUT,12)
  12       FORMAT('******ERROR IN DDS ROUTINE LS--A PARAMETER IS ',   &
                  'EQUAL TO ZERO.')
           CALL DPWRST('XXX','WRIT')
           IERROR='YES'
           GO TO 9999
         ENDIF
!CCCC    IF(PAR(I).LT.BNDLW.OR.PAR(I).GT.BNDUP)
         IF(PAR(I).LT.BNDLW.OR.PAR(I).GT.BNDUP)THEN
           WRITE(ICOUT,22)
  22       FORMAT('******ERROR IN DDS ROUTINE LS--A PARAMETER IS ',   &
                  'OUTSIDE ITS BOUNDS.')
           CALL DPWRST('XXX','WRIT')
           IERROR='YES'
           GO TO 9999
         ENDIF
60    CONTINUE
!
!     MAIN ITERATION LOOP
!
      ITNO=1
      NPAR1=NPAR+1
      NFUNC=0
   70 CONTINUE
!
!CC70 IF(IFPRT.GT.0) WRITE(ICOUT,80) ITNO,NFUNC
!CCCC IF(IFPRT.GT.0) WRITE(3,80) ITNO,NFUNC
!CC80 FORMAT (/,' START ITERATION NO.',I3,' NO. OF CALLS TO MODEL',I4)
!CCCC IF(IFPRT.GT.0) CALL DPWRST('XXX','WRIT')
!
      FLAG=.TRUE.
!
!     FIND AT'S OF ORIGINAL PARAMETERS
!
   85 CONTINUE
!CCCC APRIL 1996.  ADD XDDS, YDDS ARGUMENTS
!CCCC CALL MODEL (PAR,F,1)
      CALL MODEL (PAR,F,1,XDDS,YDDS,MAXRW1)
      NFUNC=NFUNC+1
!
!     STORE AT'S IN LAST COLUMN OF Z
!
      DO 90  IOB=1,NOB
            Z(IOB,NPAR1)=-F(IOB)
   90 CONTINUE
      ITNO=ITNO+1
!
!     FIND DERIVATIVES WITH RESPECT TO EACH PARAMETER
!
      IF(IFDIF.NE.0)THEN
!
!     ANALYTICAL
!
!CCCC APRIL 1996.  ADD XDDS, YDDS ARRAYS, PASS IFMEAN THROUGH COMMON
!CCCC    CALL DIF(PAR,Z,F,IS,IFMEAN)
         CALL DIF(PAR,Z,F,IS,XDDS,YDDS,MAXRW1)
      ELSE
!
!     NUMERICAL
!
         DO 130  IPAR=1,NPAR
           IF(ABS(PAR(IPAR)).LE.1.E-25)THEN
             WRITE(ICOUT,101)
 101         FORMAT (' ')
             CALL DPWRST('XXX','WRIT')
             WRITE(ICOUT,100) IPAR
!CCCC        WRITE(3,100) IPAR
100          FORMAT (' THE VALUE OF PAR(',I3,') IS TOO SMALL FOR'   &
             , 'DETERMINING THE DERIVATIVE')
             CALL DPWRST('XXX','WRIT')
!CCCC APRIL 1996.  SET ERROR FLAG RATHER THAN STOP
!CCCC        STOP'line 915'
             IERROR='YES'
             GO TO 9999
           ENDIF
           PARD=PAR(IPAR)
           DPAR=ABS(PAR(IPAR)*DEL)
           S1=BNDUP-PARD-DPAR
           S2=PARD-DPAR-BNDLW
           IF(S1.LT.0..AND.S2.GT.S1)THEN
              PAR(IPAR)=AMAX1(PARD-DPAR,BNDLW)
              DENOM=PARD-PAR(I)
!CCCC APRIL 1996.  ADD XDDS, YDDS
!CCCC         CALL MODEL (PAR,F,1)
              CALL MODEL (PAR,F,1,XDDS,YDDS,MAXRW1)
              NFUNC=NFUNC+1
              DO 110  IOB=1,NOB
                 Z(IOB,IPAR)=-(Z(IOB,NPAR1)+F(IOB))/DENOM
  110         CONTINUE
            ELSE
              PAR(IPAR)=AMIN1(PARD+DPAR,BNDUP)
              DENOM=PAR(IPAR)-PARD
!CCCC APRIL 1996.  ADD XDDS, YDDS
!CCCC         CALL MODEL (PAR,F,1)
              CALL MODEL (PAR,F,1,XDDS,YDDS,MAXRW1)
              NFUNC=NFUNC+1
              DO 120  IOB=1,NOB
                        Z(IOB,IPAR)=(F(IOB)+Z(IOB,NPAR1))/DENOM
  120         CONTINUE
            ENDIF
            PAR(IPAR)=PARD
130      CONTINUE
         IS=1
      ENDIF
!
!     FORM XTX AND XTY MATRIX
!
      DO 150  IPAR=1,NPAR1
         DO 155  JPAR=1,IPAR
             XX=0.0
             DO 140  IOB=IS,NOB
                 XX=XX+1.0D0*Z(IOB,IPAR)*Z(IOB,JPAR)
  140        CONTINUE
             A(IPAR,JPAR)=XX
             A(JPAR,IPAR)=XX
  155    CONTINUE
  150 CONTINUE
!CCCC IF(ITNO.EQ.2.AND.IFPRT.GT.0) WRITE(ICOUT,160) A(NPAR1,NPAR1)
!CCCC IF(ITNO.EQ.2.AND.IFPRT.GT.0) WRITE(3,160) A(NPAR1,NPAR1)
!C160 FORMAT (//,' INITIAL SUM OF SQUARES =',D12.4,/)
!CCCC CALL DPWRST('XXX','WRIT')
!     FIND GAUSS-NEWTON CORRECTIONS (XX'XX)**-1(XX'Y)
      NES=0
      NTRANS=0
      SSB=A(NPAR1,NPAR1)
      DO 170 I=1,NPAR
        LSTP(I)=0
        PARB(I)=PAR(I)
        SPDA(I)=STEP*A(I,I)
  170 CONTINUE
!
  180 CONTINUE
      SSRED=0
      NPIV=0
      DO 190 I=1,NPAR
         IF(LSTP(I).NE.0.OR.A(I,I).LE.SPDA(I).OR.ABS(CHMAX(I))   &
             .LT.1./FINF) GO TO 190
         TRED=A(I,NPAR1)**2/A(I,I)
         IF(TRED.GE.SSRED)THEN
           SSRED=TRED
           NPIV=I
         ENDIF
190   CONTINUE
      IF(NPIV.EQ.0) GO TO 225
      NTRANS=NTRANS+1
      NES=NES+1
      LSTP(NPIV)=NPIV
!
!     DO GAUSS JORDAN MODIFICATION FOR THIS ROW AND COLUMN
!
      PIVOT=A(NPIV,NPIV)
      A(NPIV,NPIV)=1.D0
      DO 200 J=1,NPAR1
         A(NPIV,J)=A(NPIV,J)/PIVOT
  200 CONTINUE
      DO 220 I=1,NPAR1
         IF(I.NE.NPIV)THEN
           CMULT=A(I,NPIV)
           DO 210 J=1,NPAR1
              IF(J.NE.NPIV)A(I,J)=A(I,J)-CMULT*A(NPIV,J)
  210      CONTINUE
           A(I,NPIV)=-A(I,NPIV)/PIVOT
         ENDIF
220   CONTINUE
      GO TO 180
!
  225 CONTINUE
!CCCC APRIL 1996.  SET ERROR FLAG INSTEAD OF STOP
!CCCC IF(NTRANS.LE.0) STOP'NO PARAMETER CHANGES POSSIBLE. INSPECT BOUND
      IF(NTRANS.LE.0) THEN
        WRITE(ICOUT,229)
        CALL DPWRST('XXX','WRIT')
  229   FORMAT('******ERROR IN DDS ROUTINE LS--NO PARAMETER CHANGES ',   &
               'POSSIBLE. INSPECT BOUNDS & CHMAX ARRAY')
        IERROR='YES'
        GO TO 9999
      ENDIF
      SSE1=A(NPAR1,NPAR1)
      DO 230 I=1,NPAR
         IF(LSTP(I).EQ.0)A(I,NPAR1)=0.D0
230   CONTINUE
!
!     FIND MAXIMUM ALLOWABLE LAMBDA
!
      ILAM=0
      FLAM=1
      ILMAX=0
      FLMAX=FINF
      QMAX=FINF
      DO 240 I=1,NPAR
         ABSA=DABS(A(I,NPAR1))
         IF(ABSA.GE.1./FINF)THEN
           QLAM=ABS(CHMAX(I))
           IF(CHMAX(I).LE.0.0)QLAM=QLAM*DABS(PARB(I))
             IF(FLAM*ABSA.GT.QLAM)THEN
               ILAM=I
               FLAM=QLAM/ABSA
             ENDIF
             IF(A(I,NPAR1).GT.0.0)QMAX=BNDUP-PARB(I)
             IF(A(I,NPAR1).LT.0.0)QMAX=PARB(I)-BNDLW
             IF(QMAX.LT.FLMAX*ABSA)THEN
               ILMAX=I
               FLMAX=QMAX/ABSA
             ENDIF
         ENDIF
240   CONTINUE
!
!CCCC IF(ILAM.NE.0.AND.IFPRT.GT.0) THEN
!CCCC    WRITE(ICOUT,250)ILAM,FLAM
!CCCC    WRITE(3,250)ILAM,FLAM
!C250 FORMAT (' PARAMETER',I4,' LIMITS THE CORRECTIONS TO ',E12.4,
!CCCC>/,' TIMES THE GAUSS-NEWTON VALUES.')
!CCCC     CALL DPWRST('XXX','WRIT')
!CCCC ENDIF
!
!CCCC IF(FLMAX.LT.1..AND.ILMAX.NE.ILAM.AND.IFPRT.GT.0)THEN
!CCCC    WRITE(ICOUT,250) ILMAX,FLMAX
!CCCC    WRITE(3,250) ILMAX,FLMAX
!CCCC    CALL DPWRST('XXX','WRIT')
!CCCC ENDIF
!
!CCCC APRIL 1996.  SET ERROR FLAG INSTEAD OF STOP
!CCCC IF(FLAM.LT.1./FINF) STOP'NO PARAMETER CHANGES POSSIBLE. INSPECT B
!CCCC>OUNDS & CHMAX ARRAY'
      IF(FLAM.LT.1./FINF) THEN
        WRITE(ICOUT,259)
        CALL DPWRST('XXX','WRIT')
  259   FORMAT('******ERROR IN DDS ROUTINE LS--NO PARAMETER CHANGES ',   &
               'POSSIBLE. INSPECT BOUNDS & CHMAX ARRAY')
        IERROR='YES'
        GO TO 9999
      ENDIF
      SBEST=SSB
      FBEST=0
      FLR=2*FINF
      SSP=SSE1
      SS(1)=SSB
      FL(1)=0
      SS(2)=1.01*FINF
      FL(2)=1.01*FLMAX
      SS(3)=1.02*FINF
      FL(3)=1.02*FLMAX
      FLT=FLAM
      KEY=0
      LG=.TRUE.
!
!     ADJUST LAMBDA UNTIL BEST SSQ FOUND FOR THIS ITERATION
!
      DO 390  IGRID=1,ITMAX
  260    CONTINUE
         DO 270 I=1,NPAR
            PAR(I)=PARB(I)+FLT*A(I,NPAR1)
            IF(PAR(I).GT.BNDUP)PAR(I)=BNDUP
            IF(PAR(I).LT.BNDLW)PAR(I)=BNDLW
  270    CONTINUE
!
!     FIND SSQ OF AT'S FOR NEW PARAMETERS
!
!CCCC APRIL 1996.  ADD XDDS, YDDS
!CCCC    CALL MODEL (PAR,F,1)
         CALL MODEL (PAR,F,1,XDDS,YDDS,MAXRW1)
         NFUNC=NFUNC+1
         SST=0.
         DO 290  IOB=1,NOB
            DF=ABS(F(IOB))
!
!     CHECK FOR UNSTABLE MODEL
!
            IF(DF.GT.1.E15)THEN
              WRITE(ICOUT,281)
 281          FORMAT (' ')
              CALL DPWRST('XXX','WRIT')
              WRITE(ICOUT,280) IOB,F(IOB)
!CCCC         WRITE(3,280) IOB,F(IOB)
!280          FORMAT (1X,/,' AT(',I3,') =',E10.3,' IS TOO LARGE')
 280          FORMAT (' AT(',I3,') =',E10.3,' IS TOO LARGE')
              CALL DPWRST('XXX','WRIT')
              IF(FLAG)THEN
!CCCC APRIL 1996.  USE DATAPLOT I/O
!CCCC           PRINT*,'UNSTABLE MODEL- FIX AND RESTART ITERATION'
                WRITE(ICOUT,283)
  283           FORMAT('*****FROM DDS ROUTINE LS--UNSTABLE MODEL, FIX',   &
                       ' AND RESTART ITERATION')
                CALL DPWRST('XXX','WRIT')
                FLAG=.FALSE.
                N=NSEAS+NPHI0+NPHI+1
                IF(IFMEAN.GT.0.AND.IFSEAS.EQ.0) N=N+NSER
!CCCC           CALL STBLIZ(PAR(N),NMA(INDEX))
                CALL STBLIZ(PAR(N),NMA(INDEX),IERROR)
                IF(IERROR.EQ.'YES')GO TO 9999
                GO TO 85
              ELSE
!CCCC           STOP'UNSTABLE MODEL - SECOND TRY'
                WRITE(ICOUT,287)
  287           FORMAT('*****FROM DDS ROUTINE LS--UNSTABLE MODEL, ',   &
                       'SECOND TRY')
                CALL DPWRST('XXX','WRIT')
              ENDIF
            ENDIF
            SST=SST+DF**2
  290    CONTINUE
         SSR=SST
         LG=LG.AND.SST.GT.SSB
         IF(KEY .EQ. 1)THEN
!CCCC      IF(IFPRT.GT.0) WRITE(ICOUT,310) IGRID,FLR,SSR
!CCCC      IF(IFPRT.GT.0)CALL DPWRST('XXX','WRIT')
           GO TO 410
         ENDIF
!
!     CHECK FOR CONVERGENCE
!
         IF((ABS(FLT-1.).LE.CONTOL.OR.ABS(FLT-FLMAX).LE.CONTOL).AND.   &
            ABS(SST-SSE1).LE.ABS(SSE1)*CONTOL.AND..NOT.LG)THEN
            FLR=FLT
!C300       CONTINUE
!C300       IF(IFPRT.GT.0) WRITE(ICOUT,310) IGRID,FLR,SSR
!CCCC       IF(IFPRT.GT.0) WRITE(3,310) IGRID,FLR,SSR
!C310       FORMAT(' SEARCH CONVERGED AFTER',I3,' CYCLES, WITH LAMBDA ='
!CCCC>      ,D13.5,/,' AND SSQ =',E13.5,/)
!CCCC       CALL DPWRST('XXX','WRIT')
            GO TO 410
         ENDIF
         INS=0
         K=0
         DO 320 I=1,3
            IF(FL(I).GT.FLT.AND.INS.EQ.0)INS=I
            IF(INS.GT.0)K=1
            IK=I+K
            FD(IK)=FL(I)
            SD(IK)=SS(I)
  320    CONTINUE
         IF(INS.EQ.0)INS=4
         FD(INS)=FLT
         SD(INS)=SST
         K=0
         IF((SD(2).GT.SD(3).OR.INS.EQ.4).AND.IGRID.GT.2)K=1
         IF(SD(1).LE.SD(2))K=0
         DO 330 I=1,3
            IK=I+K
            FL(I)=FD(IK)
            SS(I)=SD(IK)
  330    CONTINUE
!
!     STORE BEST VALUES OF SSQ AND LAMBDA
!
         IF(SST.LT.SBEST)THEN
            SBEST=SST
            FBEST=FLT
         ENDIF
!
!     FIND NEW LAMBDA
!
         IF(FL(3).LE.FLMAX) GO TO 340
         IF(SS(1).LE.SS(2)) GO TO 350
         FLT=0.1*FL(1)+0.9*FL(2)
         GO TO 390
!
  340    CONTINUE
         DENOM=(FL(3)-FL(1))*(SS(2)-SS(1))+(FL(1)-FL(2))*(SS(3)-SS(1))
         IF(DENOM.LE.-1./FINF.AND.FL(3).LT.FINF) GO TO 370
         SSP=FINF
         IF(SS(1).GT.SS(2)) GO TO 360
!
  350    CONTINUE
         FLT=0.9*FL(1)+0.1*FL(2)
         GO TO 390
!
  360    CONTINUE
         FLT=FLMAX
         IF(FL(3).GE.0.98*FLMAX)FLT=0.1*FL(2)+0.9*FL(3)
         IF(FL(3).LT.0.49*FLMAX)FLT=2.*FL(3)
         GO TO 390
!
  370 CONTINUE
         FOLD=FLR
         FLR=((FL(3)**2-FL(1)**2)*(SS(2)-SS(1))+(FL(1)**2-FL(2)**2)*   &
         (SS(3)-SS(1)))/2./DENOM
         IF(FLR.GE.FLMAX)FLR=FLMAX
         IF(FLR.LE.FL(1))FLR=FL(1)
         SSR=SS(1)+(SS(2)-SS(1))*(FLR-FL(1))*(FLR-FL(3))/(FL(2)-FL(1))   &
         /(FL(2)-FL(3))+(SS(3)-SS(1))*(FLR-FL(1))*(FLR-FL(2))/(FL(3)-   &
         FL(1))/(FL(3)-FL(2))
         IF(ABS(SSR-SSP).GT.ABS(CONTOL*SSP).AND.DABS(FOLD-FLR).GT.   &
         DABS(CONTOL*FLR)) GO TO 380
         IF(SSR.LT.0..OR.FLR.LE.FL(1).OR.FLR.GT.FL(3).OR.LG) GO TO 380
         FLT=FLR
         KEY=1
         GO TO 260
!
  380    CONTINUE
         SSP=SSR
         FLT=0.9*FL(1)+0.1*FL(2)
         IF(FLR.GT.FLT)FLT=FLR
         FT=0.1*FL(1)+0.9*FL(2)
         IF(FLR.GT.FT)FLT=FT
         FT=0.9*FL(2)+0.1*FL(3)
         IF(FLR.LT.FT)FLT=FT
         IF(FLR.GE.FT)FLT=FLR
         FT=0.1*FL(2)+0.9*FL(3)
         IF(FLR.GT.FT)FLT=FT
         IF(FLR.GT.FL(3)) GO TO 360
  390 CONTINUE
!
      IGRID=ITMAX+1
      IF(IFPRT.GT.0)THEN
         WRITE(ICOUT,401)ITMAX
  401    FORMAT (' SEARCH TOOK FULL',I4,' CYCLES. BEST TRIAL POINT')
         CALL DPWRST('XXX','WRIT')
         WRITE(ICOUT,402)FBEST,SBEST
  402    FORMAT('  LAMBDA = ',E13.5,' SSQ = ',E13.5)
         CALL DPWRST('XXX','WRIT')
      ENDIF
      FLR=FBEST
      SSR=SBEST
!
!     MODIFY PARAMETERS FOR BEST CASE
!
  410 CONTINUE
      DO 420 I=1,NPAR
          PAR(I)=PARB(I)+A(I,NPAR1)*FLR
          IF(PAR(I).GT.BNDUP)PAR(I)=BNDUP
          IF(PAR(I).LT.BNDLW)PAR(I)=BNDLW
420   CONTINUE
      IF(SSB.LT.SSR.AND.IFPRT.GT.0) WRITE(ICOUT,430) SSR,SSB
!CCCC IF(SSB.LT.SSR.AND.IFPRT.GT.0) WRITE(3,430) SSR,SSB
430   FORMAT (1H ,' CURRENT SSQ',E15.8,' EXCEEDS SSQ',E15.8,   &
      ' OF PREVIOUS ITERATION')
      IF(SSB.LT.SSR.AND.IFPRT.GT.0) CALL DPWRST('XXX','WRIT')
      IF(ITNO.LE.ITMAX.AND.ABS((SSR-SSB)/CONTOL).GT.SSB.AND.IGRID.GT.1)   &
       GO TO 70
!
      IF(ABS((SSR-SSB)/CONTOL).GT.SSB.AND.IGRID.GT.1)THEN
         WRITE(ICOUT,441)
 441     FORMAT (' ')
         CALL DPWRST('XXX','WRIT')
         WRITE(ICOUT,441)
         CALL DPWRST('XXX','WRIT')
         WRITE(ICOUT,440)
 440     FORMAT (' ********CONVERGENCE CRITERION IS NOT SATISFIED',   &
         '********')
         WRITE(ICOUT,442)
 442     FORMAT(   &
          '******** MAXIMUM NUMBER OF ITERATIONS WAS REACHED ********')
         CALL DPWRST('XXX','WRIT')
!CCCC    WRITE(3,440)
!440      FORMAT (//' ********CONVERGENCE CRITERION IS NOT SATISFIED',
!CCCC1   '********'
!CCCC2,/,'******** MAXIMUM NUMBER OF ITERATIONS WAS REACHED ********')
      ENDIF
!
      IF(IFREF.GT.0) RETURN
!
!     FIND CONFIDENCE BOUNDS
!
      NDF=NOB-NES
      SEXT=0
      IF(NDF.GT.0)SEXT=SQRT(SSR/FLOAT(NDF))
!
!     REFORM XTX MATRIX AND INVERT IT
!
      DO 460 I=1,NPAR
         DO 465 J=1,I
            XX=0.0
            DO 450 K=IS,NOB
               XX=XX+Z(K,I)*Z(K,J)
  450       CONTINUE
            A(I,J)=XX
            A(J,I)=XX
  465    CONTINUE
  460 CONTINUE
!CCCC APRIL 1996.  ADD IERROR TO ARGUMENT LIST.  USE DATAPLOT I/O
!CCCC CALL GJR (A,NPAR,1,MXPAR1,MXPAR1)
      CALL GJR (A,NPAR,1,MXPAR1,MXPAR1,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9999
      DO 470 I=1,NPAR
            IF(A(I,I).LT.0.0)THEN
               A(I,I)=-A(I,I)
!CCCC          IF(IFPRT.GT.0) PRINT*,'***WARNING***',I,
!CCCC>         ' TH DIAGONAL OF COVARIANCE MATRIX IS NEGATIVE'
               IF(IFPRT.GT.0) THEN
                 WRITE(ICOUT,469)I
 469             FORMAT('*****WARNING FROM DDS ROUTINE LS--',I5,   &
                       'TH DIAGONAL OF COVARIANCE MATRIX IS NEGATIVE.')
               ENDIF
            ENDIF
            A(I,NPAR1)=DSQRT(A(I,I))
            PARB(I)=A(I,NPAR1)*SEXT*1.96
470   CONTINUE
!
!     FIND FINAL AT'S AND PRINT FINAL SSQ
!
!CCCC APRIL 1996.  ADD XDDS AND YDDS
!CCCC CALL MODEL (PAR,F,1)
      CALL MODEL (PAR,F,1,XDDS,YDDS,MAXRW1)
      NFUNC=NFUNC+1
      XX=0.0
      DO 630 I=1,NOB
         XX=XX+F(I)*F(I)
  630 CONTINUE
      F(NOB+1)=SNGL(XX)
!
!     UNSCALE RESIDUAL SSQ FOR EARMA MODELS
!
      IF(NSER.GT.1) XX=XX*SCA(INDEX)**2
!
!CCCC WRITE(ICOUT,640) XX
!CCCC WRITE(3,640) XX
!C640 FORMAT(//,72('*'),///,1X,
!CCCC>  'FINAL RESIDUAL SUM OF SQUARES = ',D12.6,/)
!CCCC CALL DPWRST('XXX','WRIT')
!
!CCCC WRITE(ICOUT,'(4H SSQ,D15.8)') XX
!CCCC CALL DPWRST('XXX','WRIT')
!
!     UNSCALE DETERMINISTIC PARAMETERS
!
      IF(IFSEAS.EQ.1.OR.IFSEAS.EQ.3)THEN
         N=1
         DO 471 I=1,NEXP
            N=N+2
            PAR(N)=PAR(N)/DELTA
            PARB(N)=PARB(N)/DELTA
  471    CONTINUE
         DO 472 I=1,NSIN
            N=N+2
            PAR(N)=PAR(N)/DELTA
            PARB(N)=PARB(N)/DELTA
            N=N+2
            PAR(N)=PAR(N)/DELTA/6.2831853
            PARB(N)=PARB(N)/DELTA/6.2831853
  472    CONTINUE
      ENDIF
!
!     UNSCALE EARMA PARAMETERS AND CONFIDENCE BOUNDS
!
      IF(NSER.GT.1)THEN
         N=NSEAS+1
            IF(IFMEAN.GE.1)THEN
               DO 476 I=1,NSER
                  PAR(N)=PAR(N)*SCA(I)
                  PARB(N)=PARB(N)*SCA(I)
                  N=N+1
  476          CONTINUE
            ENDIF
!
!     UNSCALE ZERO LAG TERMS
!
            DO 473 I=INDEX+1,NSER
               IF(LAG(I,INDEX).EQ.0)THEN
                  PAR(N)=PAR(N)*SCA(INDEX)/SCA(I)
                  PARB(N)=PARB(N)*SCA(INDEX)/SCA(I)
                  N=N+1
               ENDIF
473         CONTINUE
!
!     SCALE REST OF PHI TERMS
!
              DO 475 I=1,NSER
                 IF(I.NE.INDEX)THEN
                    NN=N
                    SCALE=SCA(INDEX)/SCA(I)
                    DO 474 J=1,NAR(I,INDEX)
                       PAR(NN)=PAR(NN)*SCALE
                       PARB(NN)=PARB(NN)*SCALE
                       NN=NN+1
  474               CONTINUE
                 ENDIF
                 N=N+NAR(I,INDEX)
  475         CONTINUE
           ENDIF
!
!     PRINT PARAMETERS AND CONFIDENCE BOUNDS
!
!CCCC WRITE(ICOUT,*) INDEX,(NAR(J,INDEX),J=1,NSER),NMA(INDEX)
!CCCC CALL DPWRST('XXX','WRIT')
!
!CCCC WRITE(ICOUT,480)
!CCCC WRITE(3,480)
!C480 FORMAT (////,' BEST PARAMETER VALUES AND 95% CONFIDENCE LIMITS ',
!CCCC>'ESTIMATED',/,
!CCCC>' BY LINEARIZATION FOR THE INDIVIDUAL PARAMETERS ARE',
!CCCC>' AS FOLLOWS:')
!CCCC CALL DPWRST('XXX','WRIT')
!
      NP=1
!
!     PRINT SEASONALITY PARAMETERS
!
      IF(IFSEAS.EQ.1.OR.IFSEAS.EQ.3)THEN
         J1=(NSEAS+3)/4
         DO 490  J2=1,J1
            I1=(J2-1)*4+1
            I2=MIN0(NSEAS,J2*4)
!
!CCCC       WRITE(ICOUT,485) (ISEAS(J),J=I1,I2)
!CCCC       WRITE(3,485) (ISEAS(J),J=I1,I2)
!C485       FORMAT (/,1X,'I  = ',3X,I3,3(5X,I3))
!CCCC       CALL DPWRST('XXX','WRIT')
!
            WRITE(ICOUT,495) (PAR(J),PARB(J),J=I1,I2)
  495       FORMAT (11X,'BETA(I) = ',4(E12.5,' +/-',E12.5,3X,/))
            CALL DPWRST('XXX','WRIT')
  490    CONTINUE
!
         NP=NSEAS+1
      ENDIF
!
!     PRINT MEANS
!
      IF(IFMEAN.GT.0.AND.IFSEAS.EQ.0)THEN
!CCCC    WRITE(ICOUT,500) (I,PAR(I),PARB(I),I=NP,NP+NSER-1)
!CCCC    WRITE(3,500) (I,PAR(I),PARB(I),I=NP,NP+NSER-1)
!C500    FORMAT (/,4X,'SERIES',I3,' MEAN = ',E11.5,' +/-',E10.5)
!CCCC    CALL DPWRST('XXX','WRIT')
!
!CCCC    WRITE(ICOUT,'(4H MNS,2E15.8)') (PAR(I),PARB(I),I=NP,NP+NSER-1)
!CCCC    CALL DPWRST('XXX','WRIT')
!
         NP=NP+NSER
      ENDIF
!
!     PRINT PHI'S
!
      WRITE(ICOUT,999)
  999 FORMAT(1H )
      CALL DPWRST('XXX','WRIT')
!
!CCCC AUGUST, 1995.  MODIFIY FOLLOWING LINE FOR CRAY.
!CCCC CRAY DOESN'T ALLOW FREE FORMAT FOR INTERNAL WRITE.
!CCCC WRITE(ICOUT,*)'             ARMA Modeling'
      WRITE(ICOUT,9081)
 9081 FORMAT('             ARMA Modeling')
      CALL DPWRST('XXX','WRIT')
!
!CCCC THE FOLLOWING LINE MUST BE FIXED--REPLACE IORDAR AND IORDMA
!
      WRITE(ICOUT,505)IORDAR,IORDMA
  505 FORMAT(1X,5X,'AR Order = ',I4,'     MA Order = ',I4)
      CALL DPWRST('XXX','WRIT')
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
!
      WRITE(ICOUT,506)
  506 FORMAT(1X,'                Estimate       95% Conf. Halfwidth')
      CALL DPWRST('XXX','WRIT')
!
      IF(IFSEAS.NE.1)THEN
         L=NP
         NP=NP+NPHI0-1
         DO 550 I=1,NSER
            IF(LAG(I,INDEX).EQ.0)THEN
               WRITE(ICOUT,510)INDEX,I,PAR(L)
  510          FORMAT(1H ,I4,I4,G12.5)
               CALL DPWRST('XXX','WRIT')
               LAGT=1
               L=L+1
            ELSE
               LAGT=LAG(I,INDEX)
            ENDIF
            J1=(NAR(I,INDEX)+3)/4
            DO 530  J2=1,J1
               I1=(J2-1)*4+1
               I2=MIN0(NAR(I,INDEX),J2*4)
               L1 = I1 + NP
               L2 = I2 + NP
               DO 546 J=L1,L2
                  WRITE(ICOUT,547) J,PAR(J),PARB(J)
  547             FORMAT(1X,'AR(',I3,')',2X,F15.6,' +-',F15.6)
                  CALL DPWRST('XXX','WRIT')
  546          CONTINUE
  530       CONTINUE
            NP=NP+NAR(I,INDEX)
  550    CONTINUE
!
!     PRINT THETAS
!
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','WRIT')
         J1=(NMA(INDEX)+3)/4
         DO 560  J2=1,J1
            I1=(J2-1)*4+1
            I2=MIN0(NMA(INDEX),J2*4)
            L1 = I1 + NP
            L2 = I2 + NP
            DO 576 J=L1,L2
               JMNP=J-NP
               WRITE(ICOUT,577) JMNP,PAR(J),PARB(J)
  577          FORMAT(1X,'MA(',I3,')',2X,F15.6,' +-',F15.6)
               CALL DPWRST('XXX','WRIT')
  576       CONTINUE
  560    CONTINUE
         NP=NP+NMA(INDEX)
      ENDIF
!
      IF(NSER.GT.1) SEXT=SEXT*SCA(INDEX)
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
!
      WRITE(ICOUT,580)SEXT
  580 FORMAT(1X,5X,'Residual Standard Deviation = ',G15.7)
      CALL DPWRST('XXX','WRIT')
!
!     FIND AND PRINT CORRELATION MATRIX
!
      DO 590 I=1,NPAR
         DO 595 J=1,I
            A(I,J)=A(I,J)/A(I,NPAR1)/A(J,NPAR1)
  595    CONTINUE
  590 CONTINUE
!
!CCCC IF(IFPRT.GT.0) WRITE(ICOUT,600)
!CCCC IF(IFPRT.GT.0) WRITE(3,600)
!C600 FORMAT (/,' NORMALIZED CORRELATION MATRIX OF PARAMETER',
!CCCC> ' ESTIMATES',/)
!CCCC CALL DPWRST('XXX','WRIT')
!
      J1=(NPAR+9)/10
      DO 610  J2=1,J1
         I1=(J2-1)*10+1
         I2=MIN0(NPAR,J2*10)
         DO 615 I=I1,NPAR
            II=MIN0(I,I2)
  615    CONTINUE
  610 CONTINUE
!CCCC IF(IFPRT.GT.0) WRITE(ICOUT,620) (A(I,J),J=I1,II)
!C620 FORMAT (6X,10(F10.7,2X))
!CCCC CALL DPWRST('XXX','WRIT')
!
!CCC APRIL 1996.  ADD FOLLOWING LINE
 9999 CONTINUE
      RETURN
      END SUBROUTINE LS 
                                                                                                                                  
!     ..........ATSCOR..........
                                                                                                                                  
      SUBROUTINE ATSCOR(AT,MAXRW2,NOB,INDEX,IFSER,MXSER)
!CCCC SUBROUTINE ATSCOR(AT,MAXRW2,NOB,INDEX,IFSER,MXNOB1,MXSER)
!CCCC APRIL 1996.  PASS DIMENSION SPECIFIERS AS PART OF ARGUMENT LIST
!CCCC SUBROUTINE ATSCOR(AT,NOB,INDEX,IFSER)
!
!     PURPOSE--CALCULATES CORRELATIONS OF THE RESIDUALS FOR 30 LAGS
!
!CCCC APRIL 1996.  COMMENT OUT FOLLOWING LINE.
!CCCC PARAMETER (MXSER=3,MXSR2=MXSER**2,MXNOB=1024,MXNOB1=MXNOB+1)
      DOUBLE PRECISION SUM,SUM1,SUM2
!CCCC DIMENSION AT(MXNOB1,MXSER)
      DIMENSION AT(MAXRW2,MXSER)
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      A=2./NOB
!
!     FIND AND PRINT THE AVERAGE OF THE RESIDUALS
!
      AVG=0.0
      DO 10 I=1,NOB
         AVG=AVG+AT(I,INDEX)
  10  CONTINUE
      AVG=AVG/NOB
!
!CCCC WRITE(ICOUT,20) INDEX,AVG
!CCCC WRITE(3,20) INDEX,AVG
!CC20 FORMAT (////,6X,
!CCCC> 'THE AVERAGE OF THE RESIDUALS FOR SERIES',I5,
!CCCC>'  =',E15.6)
!CCCC CALL DPWRST('XXX','WRIT')
!
!     MEAN SUBTRACT THE AT'S FOR THIS SERIES
!
      DO 25 I=1,NOB
         AT(I,INDEX)=AT(I,INDEX)-AVG
   25 CONTINUE
!
!     CALCULATE THE CORRELATIONS
!
      DO 110 I=1,INDEX-1
         IF(IFSER.EQ.0)THEN
!
!     FIND CROSS CORRELATIONS BETWEEN THE SERIES
!
            SSQ=0.0
            Q=SQRT(AT(NOB+1,I)*AT(NOB+1,INDEX))
!
!CCCC       WRITE(ICOUT,30) I,INDEX
!CCCC       WRITE(3,30) I,INDEX
!CC30       FORMAT (//,7X,'CORRELATIONS OF SERIES'
!CCCC>      ,I3,' RESIDUALS ',
!CCCC>      'WITH SERIES',I3,' RESIDUALS',//,1X,'LAG'
!CCCC>      ,1X,'CORRELATION',3X,'UNIFIED',2X,'LAG'
!CCCC>      ,1X,'CORRELATION',3X,'UNIFIED',/,
!CCCC>      2(1X,3('-'),1X,11('-'),3X,7('-'),1X))
!CCCC       CALL DPWRST('XXX','WRIT')
!
             DO 55 K=0,30
                SUM1=0.0
                SUM2=0.0
                DO 40 J=K+1,NOB
                   SUM1=SUM1+AT(J,I)*AT(J-K,INDEX)
                   SUM2=SUM2+AT(J-K,I)*AT(J,INDEX)
  40            CONTINUE
                R1=SUM1/Q
                R2=SUM2/Q
                SE=SQRT(FLOAT(NOB-K))
                S1=R1*SE
                S2=R2*SE
                SSQ=SSQ+R1**2+R2**2
!
!CCCC            WRITE(ICOUT,60) K,R1,S1,-K,R2,S2
!CCCC            WRITE(3,60) K,R1,S1,-K,R2,S2
!CC60            FORMAT(1X,I3,1X,F11.6,1X,F11.6,2X,I3,1X,F11.6,1X
!CCCC>                  ,F11.6)
!CCCC            CALL DPWRST('XXX','WRIT')
!
   55        CONTINUE
!CCCC        WRITE(ICOUT,65) SSQ
!CCCC        WRITE(3,65) SSQ
!CC65        FORMAT(//,3X,'THE SUM OF SQUARES OF THE CROSS',
!CCCC>       ' CORRELATIONS =',E12.4)
!CCCC        CALL DPWRST('XXX','WRIT')
!
         ENDIF
!
110   CONTINUE
!
!     FIND THE AUTOCORRELATIONS OF THE INDEX SERIES
!
      SSQ=0.0
      VAR=1./NOB
      Q=AT(NOB+1,INDEX)
!
!CCCC WRITE(ICOUT,70) INDEX
!CC70 FORMAT(//,10X,'AUTO CORRELATIONS OF SERIES',I3,' RESIDUALS'
!CCCC>, //,10X,'LAG',7X,'CORRELATION',13X,'UNIFIED',/)
!CCCC CALL DPWRST('XXX','WRIT')
!
      UAM = 0.
      LAGUAM=0
      LFUAG2=0
      NUAG2 =0
      DO 90 K=1,30
         SUM=0.0
         DO 80 J=1,NOB-K
                     SUM=SUM+AT(J,INDEX)*AT(J+K,INDEX)
  80     CONTINUE
         R=SUM/Q
         S=R/SQRT(VAR)
         AS = ABS(S)
         IF(AS.GT.2.)THEN
            IF(LFUAG2.EQ.0)THEN
               LFUAG2 = K
               NUAG2 = 1
            ELSE
               NUAG2 = NUAG2 + 1
            ENDIF
         ENDIF
         IF(AS.GT.ABS(UAM))THEN
            UAM = S
            LAGUAM = K
         ENDIF
         SSQ=SSQ+R**2
         VAR=VAR+A*R**2
!
!CCCC    WRITE(ICOUT,100) K,R,S
!CCCC    WRITE(3,100) K,R,S
!C100    FORMAT (10X,I3,5X,F12.7,10X,F12.7)
!CCCC    CALL DPWRST('XXX','WRIT')
!
   90 CONTINUE
!
!CCCC WRITE(ICOUT,105) SSQ
!CCCC WRITE(3,105) SSQ
!C105 FORMAT(//,9X,'THE SUM OF SQUARES OF THE AUTO',
!CCCC>' CORRELATIONS =',E12.4)
!CCCC CALL DPWRST('XXX','WRIT')
!
!CCCC WRITE(ICOUT,106) UAM,LAGUAM,NUAG2,LFUAG2
!CCCC WRITE(3,106) UAM,LAGUAM,NUAG2,LFUAG2
!C106 FORMAT(/,9X,
!CCCC>' THE MAXIMUM UNIFIED AUTOCORRELATION IS',E15.8,/,9X,
!CCCC>'   OCCURING AT LAG . . . . . . . . . . . ',I3,/,9X,
!CCCC>' THE NO. OF UNI. AUTCOR. WITH :UA: > 2 IS',I3,/,9X,
!CCCC>'   THE FIRST OCCURS AT LAG . . . . . . . ',I3)
!CCCC CALL DPWRST('XXX','WRIT')
!
!CCCC WRITE(ICOUT,'(E15.4,3I5)') UAM,LAGUAM,LFUAG2,NUAG2
!CCCC CALL DPWRST('XXX','WRIT')
!
      RETURN
      END SUBROUTINE ATSCOR
                                                                                                                                  
!     ..........ANALYS..........
                                                                                                                                  
      SUBROUTINE ANALYS(PAR,VAR,IOUNI2,IOUNI3)
!CCCC APRIL 1996.  ADD I/O UNITS TO ARGUMENT LIST, PASS DELTA THROUGH
!CCCC COMMON
!CCCC SUBROUTINE ANALYS(PAR,DELTA,VAR)
!
!     PURPOSE--FINDS THE ROOTS,FREQUENCIES,AND DAMPING, AND THE
!
!     GREEN'S FUNCTION AND IMPULSE RESPONSE COEFFICIENTS OF THE MODEL
!CCCC APRIL 1996.  USE DPCODD.INC FOR FOLLOWING
!CCCC PARAMETER (MXSER=3,MXNOB=1024,MXPAR=45)
!CCCC COMMON /INDEX/ INDEX,MAXLAG,NOB,NSER,NPHI0,NPHI,NSEAS
!CCCC COMMON /LAG/ LAG(MXSER,MXSER),NAR(MXSER,MXSER),NMA(MXSER)
!CCCC COMMON /FLAG/ IFMEAN,IFAT,IFSEAS,IFGREEN,IFSPECTR
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODD.INC'
      COMPLEX ROOT(MXPAR)
      DIMENSION PAR(MXPAR),VAR(MXSER)
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     FIND ROOTS OF AUTOREGRESSIVE SIDE
!
      IF(NAR(INDEX,INDEX).EQ.0) RETURN
      N=NPHI0+NSEAS+1
      IF(IFMEAN.GE.1.AND.IFSEAS.EQ.0) N=N+NSER
      DO 10 I=1,INDEX-1
         N=N+NAR(I,INDEX)
  10  CONTINUE
      NR=NAR(INDEX,INDEX)
!CCCC THE FOLLOWING LINE WAS QUESTIONABLY CHANGED--JJF NOVEMBER 1994
!CCCC CALL EIGEN(PAR(N),NR,ROOT)
      CALL EIGEN(PAR,NR,ROOT)
!
!CCCC WRITE(ICOUT,30)
!CC30 FORMAT (////,37X,20('*'),'MODEL CHARACTERISTICS',20('*'),///)
!CCCC CALL DPWRST('XXX','WRIT')
!
!     FIND EXPLICIT GREEN'S FUNCTION AND IMPULSE RESPONSE
!
      N=NSEAS+1
      IF(IFMEAN.GE.1.AND.IFSEAS.EQ.0) N=N+NSER
      N1=N
      N=N+NPHI0
      M=N+NPHI
      DO 63 I=1,NSER
!
!     SET ZERO LAG TERMS
!
         IF(LAG(I,INDEX).EQ.0)THEN
            PHI0=PAR(N1)
            N1=N1+1
            N2=N
            NN=NAR(I,INDEX)
         ELSE
            PHI0=PAR(N)
            N2=N+1
            NN=NAR(I,INDEX)-1
         ENDIF
!
!     FIND FUNCTION FOR SERIES I
!
         IF(I.NE.INDEX.AND.NR.GT.NN)THEN
            WRITE(ICOUT,61) INDEX,I
61          FORMAT(44X,'IMPULSE RESPONSE FUNCTION OF SERIES',   &
            I3,' TO SERIES',I3)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,62)
62          FORMAT(' ')
            CALL DPWRST('XXX','WRIT')
!
            CALL GREENZ(PHI0,PAR(N2),ROOT,NR,NN,LAG(I,INDEX),'IMP(j) =',   &
                        VAR(I),IOUNI2,IOUNI3)
         ELSEIF(I.EQ.INDEX.AND.NR.GT.NMA(INDEX))THEN
!
!CCCC       WRITE(ICOUT,62) INDEX
!CC62       FORMAT (44X,'GREEN''S FUNCTION OF SERIES',I3,
!CCCC>      ' WITH ITS RESIDUALS',/)
!CCCC       CALL DPWRST('XXX','WRIT')
!
            CALL GREENZ(1.0,PAR(M),ROOT,NR,NMA(INDEX),0,'  G(j) =',   &
                       VAR(I),IOUNI2,IOUNI3)
!
         ELSEIF(I.EQ.INDEX.AND.NR.LE.NMA(INDEX))THEN
!CCCC       WRITE(ICOUT,409) INDEX
!C409       FORMAT(44X,'EXPLICIT FORM OF SERIES',I3,' GREENS',
!CCCC>      ' FUNCTION AND',/,50X,'COMPONENTS OF SPECTRUM',
!CCCC>      ' DONOT EXIST',/)
!CCCC       CALL DPWRST('XXX','WRIT')
!
!CCCC APRIL 1996.  ADD TO ARGUMENT LIST, MAKE NAME 6 CHARACTERS
!CCCC PASS DELTA THROUGH COMMON
!CCCC       CALL SPECTRUM(1.0,PAR(M),PAR(N),NR,NMA(INDEX),0,
!CCCC>                    DELTA)
            CALL SPCTRM(1.0,PAR(M),PAR(N),NR,NMA(INDEX),0,IOUNI3)
!
         ENDIF
         N=N+NAR(I,INDEX)
  63  CONTINUE
!
!     FIND FREQENCIES AND DAMPING OF AUTOREGRESSIVE OPERATOR
!
!CCCC WRITE(ICOUT,64)
!CC64 FORMAT (///,45X,'CHARACTERISTICS OF THE AUTOREGRESSIVE OPERATOR')
!CCCC CALL DPWRST('XXX','WRIT')
!
      CALL FREQ(NR,ROOT,DELTA)
!
!     FIND ROOTS OF MOVING AVERAGE SIDE
!
      NR=NMA(INDEX)
      IF(NR.GT.0)THEN
         N=NSEAS+NPHI0+NPHI+1
         IF(IFMEAN.GE.1.AND.IFSEAS.EQ.0) N=N+NSER
!CCCC THE FOLLOWING LINE WAS QUESTIONABLY CHANGED--JJF NOVEMBER 1994
!CCCC    CALL EIGEN(PAR(N),NR,ROOT)
         CALL EIGEN(PAR,NR,ROOT)
!
!     FIND FREQENCIES AND DAMPING OF MOVING AVERAGE OPERATOR
!
!CCCC    WRITE(ICOUT,80)
!CC80    FORMAT(/////,45X,'CHARACTERISTICS OF THE MOVING',
!CCCC>   ' AVERAGE OPERATOR')
!CCCC    CALL DPWRST('XXX','WRIT')
!
         CALL FREQ(NR,ROOT,DELTA)
      ENDIF
      RETURN
      END SUBROUTINE ANALYS
                                                                                                                                  
!     ..........STBLIZ..........
                                                                                                                                  
      SUBROUTINE STBLIZ(THETA,N,IERROR)
!
!     PURPOSE--CHECKS FOR STABILITY OF THETA AND FIXES IF NECESSARY
!
      PARAMETER (MXPAR=34,MXPAR1=MXPAR+1)
      COMPLEX ROOT(MXPAR),UNSRT,RPRIME
!CCCC COMPLEX*16 CTHETA(MXPAR),F,FPRIME
      COMPLEX    CTHETA(MXPAR),F,FPRIME
      DIMENSION THETA(MXPAR),NSTABL(MXPAR)
      CHARACTER*4 IERROR
!
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     COMPUTE DISCRETE ROOTS
!
      IERROR='OFF'
      CALL EIGEN(THETA,N,ROOT(1))
!     PRINT*,'INITIAL NUS',(ROOT(I),I=1,N)  @@DIA
!
!     CHECK FOR UNSTABLE MODEL
!
      ISTABL=0
      EPS=0.0001
      DO 20 I=1,N
         IF(CABS(ROOT(I)).GT.1.0+EPS)THEN
           ISTABL=ISTABL+1
            NSTABL(ISTABL)=I
         ENDIF
20    CONTINUE
!     PRINT*,'ISTABL=',ISTABL,'NSTABL=',(NSTABL(I),I=1,ISTABL) @@DIA
!
!     STABILIZE ANY UNSTABLE ROOTS - IF NECESSARY
!
      IF(ISTABL.GT.0)THEN
         DO 30 I=1,N
            CTHETA(I)=CMPLX(THETA(I),0.)
  30     CONTINUE
         DO 50 I=1,ISTABL
            UNSRT=ROOT(NSTABL(I))
            RPRIME=1./UNSRT
!           PRINT*,'UNSRT=',UNSRT,' RPRIME=',RPRIME  @@DIA
!
!     PARTIAL REVERSE ROUTINE - UPDATE PARAMETERS FOR ONE ALTERED ROOT
!
            F=CTHETA(1)-UNSRT
            CTHETA(1)=F+RPRIME
            DO 40 J=2,N
               FPRIME=F*RPRIME
               F=CTHETA(J)+F*UNSRT
               CTHETA(J)=F-FPRIME
  40        CONTINUE
  50     CONTINUE
         DO 60 I=1,N
            THETA(I)=REAL(CTHETA(I))
  60     CONTINUE
      ENDIF
!     PRINT*,'NEW THETAS',(THETA(I),I=1,N)  @@DIA
!     PRINT*,'NEW CTHETAS',(CTHETA(I),I=1,N)  @@DIA
      CALL EIGEN(THETA,N,ROOT(1))
!     PRINT*,'FINAL NUS',(ROOT(I),I=1,N)  @@DIA
      RETURN
      END SUBROUTINE STBLIZ
                                                                                                                                  
!     ..........DIF..........
                                                                                                                                  
      SUBROUTINE DIF(PAR,Z,AT,IS,XDDS,YDDS,MAXRW1)
!CCCC APRIL 1996.  ADD XDDS AND YDDS, PASS IFMEAN THROUGH COMMON
!CCCC SUBROUTINE DIF(PAR,Z,AT,IS,IFMEAN)
!
!     PURPOSE--FINDS THE ANALYTICAL DERIVATIVES OF THE
!              RESIDUALS WITH RESPECT TO THE STOCHASTIC PARAMETERS
!
!CCCC APRIL 1996.  USE DPCODD.INC
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODD.INC'
      DIMENSION XDDS(MAXRW1,MXSER)
      DIMENSION YDDS(MAXRW1,MXSER)
!CCCC PARAMETER (MXSER=3,MXNOB=1024,MXNOB1=MXNOB+1,MXPAR=45,
!CCCC>MXPAR1=MXPAR+1)
      DOUBLE PRECISION S
!CCCC COMMON /INDEX/ INDEX,MAXLAG,NOB,NSER,NPHI0,NPHI,NSEAS
!CCCC COMMON /LAG/ LAG(MXSER,MXSER),NAR(MXSER,MXSER),NMA(MXSER)
!CCCC COMMON/DDSDAT/ YDDS(MXNOB,MXSER),XDDS(MXNOB,MXSER)
      DIMENSION PAR(MXPAR),AT(MXNOB1),Z(MXNOB,MXPAR1)
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IZ=1
      KPAR=NPHI0+NPHI+1
!
!     FIND DERIVATIVES WITH RESPECT TO MEANS IF ESTIMATED
!
         IF(IFMEAN.GE.1)THEN
!
!     RESTORE ORIGINAL DATA VALUES
!
            DO 5 I=1,NSER
               DO 6 J=1,NOB
                  XDDS(J,I)=YDDS(J,I)
   6           CONTINUE
   5        CONTINUE
            KPAR=KPAR+NSER
            IPAR=NSER+1
            JPAR=IPAR+NPHI0
            DO 30  IZ=1,NSER
               SUM=0.0
               IF(IZ.EQ.INDEX)SUM=-1.0
               IF(IZ.GT.INDEX.AND.LAG(IZ,INDEX).EQ.0)THEN
                  SUM=-PAR(IPAR)
                  IPAR=IPAR+1
               ENDIF
               DO 10 J=1,NAR(IZ,INDEX)
                  SUM=SUM+PAR(JPAR)
                  JPAR=JPAR+1
  10           CONTINUE
               DO 35  IT=1,NOB
                  S=SUM
                  K=KPAR
                  DO 20 J=1,MIN(IT-1,NMA(INDEX))
                      S=S+PAR(K)*Z(IT-J,IZ)
                      K=K+1
  20              CONTINUE
                  Z(IT,IZ)=S
  35           CONTINUE
  30        CONTINUE
         ENDIF
!
!     FIND DERIVATIVES WITH RESPECT TO PHI0'S
!
         DO 60 I=1+INDEX,NSER
            IF(LAG(I,INDEX).EQ.0)THEN
               DO 50  IT=1,NOB
                  K=KPAR
                  S=XDDS(IT,I)
                  DO 40 J=1,MIN(IT-1,NMA(INDEX))
                      S=S+PAR(K)*Z(IT-J,IZ)
                      K=K+1
  40              CONTINUE
                  Z(IT,IZ)=S
  50           CONTINUE
               IZ=IZ+1
            ENDIF
60      CONTINUE
!
!     FIND DERIVATIVES WITH RESPECT TO PHI'S - PHI1 FIRST
!
         DO 140 I=1,NSER
            LAGT=LAG(I,INDEX)
            IF(LAGT.EQ.0)LAGT=1
            N=1
  65        CONTINUE
            DO 80  IT=1,LAGT
                S=0.0
                IF(IFMEAN.GE.1)S=PAR(I)
                K=KPAR
                DO 70 J=1,MIN(IT-1,NMA(INDEX))
                    S=S+PAR(K)*Z(IT-J,IZ)
                      K=K+1
  70            CONTINUE
                 Z(IT,IZ)=S
  80        CONTINUE
            DO 100  IT=LAGT+1,NOB
               S=0.0
               IF(IFMEAN.GE.1)S=PAR(I)
               K=KPAR
               DO 90 J=1,MIN(IT-1,NMA(INDEX))
                   S=S+PAR(K)*Z(IT-J,IZ)
                   K=K+1
  90            CONTINUE
                Z(IT,IZ)=S-XDDS(IT-LAGT,I)
 100        CONTINUE
!
!     DO THE REST OF THE PHI'S FOR THIS SERIES
!
            IZ=IZ+1
            IF(IFMEAN.GE.1)THEN
               N=N+1
               LAGT=LAGT+1
               IF(N.LE.NAR(I,INDEX))GO TO 65
            ELSE
               DO 130 J=2,NAR(I,INDEX)
                  DO 110  IT=I,J
                     Z(IT,IZ)=0.0
  110             CONTINUE
                  DO 120  IT=J+1,NOB
                     Z(IT,IZ)=Z(IT-1,IZ-1)
  120             CONTINUE
                  IZ=IZ+1
  130          CONTINUE
            ENDIF
140     CONTINUE
!
!     FIND DERIVATIVES WITH RESPECT TO THETA - THETA1 FIRST
!
         Z(1,IZ)=0.0
         DO 160  IT=2,NOB
            S=0.0
            K=KPAR
            DO 150 J=1,MIN(IT-1,NMA(INDEX))
               S=S+PAR(K)*Z(IT-J,IZ)
               K=K+1
  150       CONTINUE
            Z(IT,IZ)=S+AT(IT-1)
  160   CONTINUE
!
!     DO THE REST OF THE THETA TERMS
!
         DO 180 I=2,NMA(INDEX)
            IZ=IZ+1
            DO 170  IT=1,I
               Z(IT,IZ)=0.0
  170       CONTINUE
            DO 185  IT=I+1,NOB
               Z(IT,IZ)=Z(IT-1,IZ-1)
  185       CONTINUE
  180    CONTINUE
      IS=MAXLAG
      RETURN
      END SUBROUTINE DIF
                                                                                                                                  
!     ..........XA..........
                                                                                                                                  
      SUBROUTINE XA(X1,X2,XTX,N,M,IR,IC,MAXLAG,NOB,MXINV,MXINV1)
!CCCC APRIL 1996.  ADD ARRAY DIMENSION TO ARGUMENT LIST
!CCCC SUBROUTINE XA(X1,X2,XTX,N,M,IR,IC,MAXLAG,NOB)
!     PURPOSE--FORMS THE OFF DIAGONAL BLOCKS
!
!     NOTE--IR AND IC ARE THE POSITION OF THE UPPER LEFT ELEMEN
!
!CCCC APRIL 1996.  USE IMPLICIT DIMENSION
!CCCC PARAMETER (MXSER=3,MXNOB=1024,MXPAR=45,MXINV=50,MXINV1=MXINV+1)
      DOUBLE PRECISION XTX,SUM
!CCCC DIMENSION X1(MXNOB),X2(MXNOB),XTX(MXINV,MXINV1)
      DIMENSION X1(*),X2(*),XTX(MXINV,MXINV1)
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      NOBM1=NOB-1
      NP1=N+1
!
!     FORM UPPER RIGHT HALF
!
!     GET INITIAL WINDOW
!
      DO 20 I=N-1,1,-1
         SUM=0.0
         DO 10 J=MAXLAG,NOBM1
            SUM=SUM+X1(J-I)*X2(J)
  10     CONTINUE
         XTX(IR,IC+I)=SUM
!
!     WINDOW REST OF DIAGONAL
!
         DO 25 J=1,MIN(M-1,N-I-1)
             SUM=SUM+X1(N-J-I)*X2(N-J)-X1(NOB-J-I)*X2(NOB-J)
             XTX(IR+J,IC+I+J)=SUM
  25     CONTINUE
  20  CONTINUE
!
!     FORM LOWER LEFT HALF
!
!     GET INITIAL WINDOW
!
      DO 40 I=0,M-1
         SUM=0.0
         DO 30 J=MAXLAG,NOBM1
            SUM=SUM+X1(J)*X2(J-I)
  30     CONTINUE
         XTX(IR+I,IC)=SUM
!
!     WINDOW REST OF DIAGONAL
!
         DO 45 J=1,M-I-1
            SUM=SUM+X1(M-J)*X2(M-J-I)-X1(NOB-J)*X2(NOB-J-I)
            XTX(IR+I+J,IC+J)=SUM
  45     CONTINUE
  40  CONTINUE
!
      RETURN
      END SUBROUTINE XA
                                                                                                                                  
!     ..........MODEL..........
                                                                                                                                  
      SUBROUTINE MODEL(PAR,AT,IFIRST,XDDS,YDDS,MAXRW1)
!CCCC APRIL 1996.  ADD XDDS, YDDS TO ARGUMENT LIST
!CCCC SUBROUTINE MODEL(PAR,AT,IFIRST)
!
!     PURPOSE--FINDS THE RESIDUALS (AT'S) OF THE MODEL
!
      DOUBLE PRECISION SUM
!CCCC APRIL 1996.  REPLACE FOLLOWING WITH DPCODD.INC
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODD.INC'
      DIMENSION XDDS(MAXRW1,MXSER)
      DIMENSION YDDS(MAXRW1,MXSER)
!CCCC PARAMETER (MXSER=3,MXNOB=1024,MXNOB1=MXNOB+1,MXPAR=45)
!CCCC COMMON /INDEX/ INDEX,MAXLAG,NOB,NSER,NPHI0,NPHI,NSEAS
!CCCC COMMON /LAG/ LAG(MXSER,MXSER),NAR(MXSER,MXSER),NMA(MXSER)
!CCCC COMMON /FLAG/ IFMEAN,IFAT,IFSEAS,IFGREEN,IFSPECTR
!CCCC COMMON /SEASON/ NPOLY,NEXP,NSIN,ISEAS(30),SEAS(30),DELTA
!CCCC COMMON/DDSDAT/ YDDS(MXNOB,MXSER),XDDS(MXNOB,MXSER)
!CCCC DIMENSION PAR(MXPAR),AT(MXNOB1)
      DIMENSION PAR(*),AT(*)
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IPAR=1
!
!     SEASONALITY PARAMETERS
!
      IF(IFSEAS.EQ.1.OR.IFSEAS.EQ.3)THEN
!
!     LOAD SEASONALITY ARRAY
!
         N=1
         DO 10 I=1,NSEAS
            SEAS(ISEAS(I))=PAR(IPAR)
            IPAR=IPAR+1
10       CONTINUE
!
!     SUBTRACT SEASONALITY FROM DATA
!
!CCCC APRIL 1996.  ADD XDDS, YDDS ARGUMENTS, NOB PASSED THROUGH COMMON
!CCCC    CALL DETPAR(NOB)
         CALL DETPAR(XDDS,YDDS,MAXRW1)
         IF(IFIRST.EQ.0) RETURN
!
!     SUBTRACT MEAN IF ESTIMATED
!
      ELSEIF(IFMEAN.GE.1.AND.IFSEAS.EQ.0)THEN
         DO 40 I=1,NSER
            DO 30 J=1,NOB
               XDDS(J,I)=YDDS(J,I)-PAR(IPAR)
  30        CONTINUE
            IPAR=IPAR+1
  40     CONTINUE
      ENDIF
      IF(IFSEAS.EQ.1)THEN
         DO 20 I=1,NOB
            AT(I)=XDDS(I,1)
  20     CONTINUE
         RETURN
      ENDIF
!
!     FIND FIRST MAXLAG AT'S
!
      JPAR=IPAR
      IF(IFAT.EQ.0)THEN
         DO 50 I=1,MAXLAG
            AT(I)=0.0
  50     CONTINUE
      ELSEIF(IFAT.GT.0)THEN
         DO 100  IT=1,MAXLAG
           IPAR=JPAR
           SUM=0.0
!
!     FIND PHI(0) TERMS
!
           DO 60 I=INDEX+1,NSER
              IF(LAG(I,INDEX).EQ.0)THEN
                 SUM=SUM+PAR(IPAR)*XDDS(IT,I)
                 IPAR=IPAR+1
              ENDIF
60         CONTINUE
!
!     FIND REST OF PHI TERMS
!
           KPAR=IPAR
           DO 80 I=1,NSER
              LAGT=LAG(I,INDEX)
              IF(LAGT.EQ.0) LAGT=1
              IPAR=KPAR
              DO 70 J=LAGT,MIN(IT,LAGT+NAR(I,INDEX))-1
                 SUM=SUM-PAR(IPAR)*XDDS(IT-J,I)
                 IPAR=IPAR+1
  70          CONTINUE
              KPAR=KPAR+NAR(I,INDEX)
  80       CONTINUE
!
!     DOTHETA TERMS
!
           DO 90 I=1,MIN(IT-1,NMA(INDEX))
              SUM=SUM+PAR(KPAR)*AT(IT-I)
              KPAR=KPAR+1
  90       CONTINUE
           AT(IT)=XDDS(IT,INDEX)+SUM
 100     CONTINUE
      ELSE
         NP=IPAR+NPHI0+NPHI+NMA(INDEX)-1
         DO 1 I=1,MAXLAG
            AT(I)=PAR(NP+I)
 1       CONTINUE
      ENDIF
!
!     FIND REST OF AT'S
!
      DO 140  IT=MAXLAG+1,NOB
        IPAR=JPAR
        SUM=0.0
        DO 110 I=INDEX+1,NSER
           IF(LAG(I,INDEX).EQ.0)THEN
              SUM=SUM+PAR(IPAR)*XDDS(IT,I)
              IPAR=IPAR+1
           ENDIF
110     CONTINUE
        DO 120 I=1,NSER
           LAGT=LAG(I,INDEX)
           IF(LAGT.EQ.0) LAGT=1
           DO 125 J=LAGT,LAGT+NAR(I,INDEX)-1
               SUM=SUM-PAR(IPAR)*XDDS(IT-J,I)
               IPAR=IPAR+1
  125      CONTINUE
  120   CONTINUE
        DO 130 I=1,NMA(INDEX)
           SUM=SUM+PAR(IPAR)*AT(IT-I)
           IPAR=IPAR+1
  130   CONTINUE
        AT(IT)=XDDS(IT,INDEX)+SUM
  140 CONTINUE
      MXLAG1 = MAXLAG+1
      RETURN
      END SUBROUTINE MODEL
                                                                                                                                  
!     ..........XX..........
                                                                                                                                  
      SUBROUTINE XXZ(XDDS,XTX,N,IR,IC,MAXLAG,NOB,MXINV,MXINV1)
!CCCC APRIL 1996.  ADD MXINV, MXINV1 TO ARGUMENT LIST, RENAME TO
!CCCC AVOID NAME CONFLICT.
!CCCC SUBROUTINE XX(XDDS,XTX,N,IR,IC,MAXLAG,NOB)
!
!     PURPOSE--FORMS THE LOWER TRIANGLE OF THE DIAGONAL BLOCKS
!
!     IR AND IC ARE THE POSITION OF THE UPPER LEFT CORNER
!CCCC APRIL 1996.  USE ARGUMENT LIST TO DIMENSION ARRAYS
!CCCC PARAMETER (MXSER=3,MXNOB=1024,MXPAR=45,MXINV=50,MXINV1=MXINV+1)
      DOUBLE PRECISION XTX,SUM
!CCCC DIMENSION XDDS(MXNOB),XTX(MXINV,MXINV1)
      DIMENSION XDDS(*),XTX(MXINV,MXINV1)
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      K=NOB+1
      L=N+1
      DO 20 I=0,N-1
         SUM=0.0
!
!     FORM INITIAL WINDOW
!
         DO 10 J=MAXLAG,NOB-1
            SUM=SUM+XDDS(J-I)*XDDS(J)
  10     CONTINUE
         XTX(IR+I,IC)=SUM
!
!     WINDOW REST OF DIAGONAL
!
         DO 25 J=1,N-I-1
            IF(J.EQ.0) GO TO 20
            SUM=SUM+XDDS(N-J)*XDDS(N-J-I)-XDDS(NOB-J)*XDDS(NOB-J-I)
            XTX(IR+I+J,IC+J)=SUM
  25     CONTINUE
  20  CONTINUE
      RETURN
      END SUBROUTINE XXZ
                                                                                                                                  
!     ..........FREQ..........
                                                                                                                                  
      SUBROUTINE FREQ(NR,ROOT,DELTA)
!
!     PURPOSE--CALCULATES FREQUENCY AND DAMPING FROM THE ROOTS
!
!CCCC THE FOLLOWING LINE WAS FIXED--JJF    NOVEMBER 1994
!CCCC COMPLEX ROOT(1),CROOT
      COMPLEX ROOT(*),CROOT
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!CCCC WRITE(ICOUT,10)
!CC10 FORMAT (/,46X,'DISCRETE',14X,'NATURAL',/,43X,
!CCCC>'COMPLEX ROOTS',11X,
!CCCC>'FREQUENCY',10X,'DAMPING',/,43X,'REAL',6X,'IMAG',12X,'(HZ)',
!CCCC>14X,'RATIO',/,41X,52('-'))
!CCCC CALL DPWRST('XXX','WRIT')
!
         DO 40 I=1,NR
!
!     FIND FREQUENCY AND DAMPING FOR 2ND ORDER ROOT
!
            AIROOT=AIMAG(ROOT(I))
               IF(AIROOT.GT.0)THEN
                  CROOT=CLOG(ROOT(I))/DELTA
                  ACROOT=CABS(CROOT)
                  W=ACROOT/6.2831853
                  Z=-REAL(CROOT)/ACROOT
!
!CCCC             WRITE(ICOUT,20) ROOT(I),W,Z
!CC20             FORMAT (40X,F7.4,' +/-',F7.4,2(7X,E11.4))
!CCCC             CALL DPWRST('XXX','WRIT')
!
!     FIND BREAK FREQUENCY FOR REAL ROOTS
!
               ELSEIF(AIROOT.EQ.0)THEN
                  RROOT=REAL(ROOT(I))
                     IF(RROOT.LE.0)THEN
!
!     NEGATIVE ROOT - SET BREAK FREQUENCY AT NYQUIST FREQUENCYDDS(
!
                        W=0.5/DELTA
                     ELSE
!     POSITIVE ROOT - CALCULATE BREAK FREQUENCY
!
                        W=-LOG(RROOT)/(DELTA*6.2831853)
                     ENDIF
!
!CCCC             WRITE(ICOUT,30) REAL(ROOT(I)),W
!CC30             FORMAT (40X,F7.4,18X,E11.4)
!CCCC             CALL DPWRST('XXX','WRIT')
!
               ENDIF
40       CONTINUE
!
      RETURN
      END SUBROUTINE FREQ
                                                                                                                                  
!     ..........EIGEN..........
                                                                                                                                  
      SUBROUTINE EIGEN(PAR,N,ROOT)
!
!     PURPOSE--USES A MODIFIED BAIRSTOW METHOD FOR ROOT EXTRACTION
!
!CCCC APRIL 1996.  USE DPCODD.INC
!CCCC PARAMETER (MXPAR=45,MP=6*MXPAR/5,MP1=MP+1,MXPAR1=MXPAR+1)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODD.INC'
!CCCC THE FOLLOWING LINE WAS FIXED--JJF    NOVEMBER 1994
!CCCC REAL PAR(1),A(MXPAR1)
!CCCC REAL PAR(MXPAR),A(MXPAR1)
      REAL PAR(*),A(MXPAR1)
      COMPLEX ROOT(MXPAR)
      DIMENSION H(MP),B(MP1),C(MP1),U(MXPAR),V(MXPAR)
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      TEMP=0.
      DENOM=1.
      N1=N+1
      H(1)=-1.
      A(1)=-1.
      A(N1)=1./PAR(N)
      DO 5 I=2,N
         II=N1-I
         A(I)=-PAR(II)*A(N1)
    5 CONTINUE
      IREV=1
      NC=N+1
!
!     THIS LOOP STORES THE COEFFICIENTS OF THE ORIGINAL POLYNOMIAL INTO H
!
      DO 10 I=1,NC
         B(I)=0.
         H(I)=A(I)
   10 CONTINUE
      P=0.
      Q=0.
      R=0.
!
!     THE FOLLOWING SECTION CHECKS TO SEE IF 0 IS A ROOT
!
   30 CONTINUE
      IF(H(1).NE.0)GO TO 40
      NC=NC-1
      V(NC)=0.
      U(NC)=0.
!
!     THIS LOOP STORES THE COEFFICIENTS OF THE DEPRESSED POLYNOMIAL INTO H
!
      DO 20 I=1,NC
         H(I)=H(I+1)
   20 CONTINUE
      GO TO 30
!
!     GENERATE FIRST OR SECOND ORDER ROOTS
!
   40 CONTINUE
      IF(NC.EQ.1)GO TO 320
      IF(NC.NE.2)GO TO 50
      R=-H(1)/H(2)
      GO TO 200
!
   50 CONTINUE
      IF(NC.NE.3)GO TO 60
      P=H(2)/H(3)
      Q=H(1)/H(3)
      GO TO 240
!
!     REVERSE COEFFICIENT ORDER IF WILL SPEED CONVERGENCE OF P,Q,& R
!
   60 CONTINUE
      IF(DABS(H(NC-1)/H(NC)).GE.DABS(H(2)/H(1)))GO TO 100
!
!     REVERSE COEFFICIENTS AND SET IREV FOR INTERCHANGE
!
      IREV=-IREV
      M=NC/2
      DO 70 I=1,M
         NL=NC+1-I
         TEMP=H(NL)
         H(NL)=H(I)
         H(I)=TEMP
   70 CONTINUE
!
!     INITIAL GUESS OF P,Q,& R AFTER FIRST FACTOR AND ITS ROOTS FOUND
!
      IF(Q.EQ.0)GO TO 80
      P=P/Q
      Q=1./Q
      GO TO 90
!
   80 CONTINUE
      P=0.
   90 CONTINUE
      IF(R.EQ.0)GO TO 100
      R=1./R
!
!     SET ERROR CRITERION AND FIRST 2 B'S AND C'S
!
  100 CONTINUE
      E=5.E-10
      B(NC)=H(NC)
      C(NC)=H(NC)
      B(NC+1)=0.
      C(NC+1)=0.
      NP=NC-1
!
!     LOOP TO FIND THE LINEAR AND QUADRATIC FACTORS OF THE GIVEN POLYNOMIA
!
  190 CONTINUE
      DO 110 J=1,1000
!
!     THIS SECTION FINDS THE LINEAR FACTORS
!
         DO 120 K=1,NP
            I=NC-K
            B(I)=H(I)+R*B(I+1)
            C(I)=B(I)+R*C(I+1)
  120    CONTINUE
!
!     CHECK IF R IS CONSTANT TERM OF FACTOR - ELSE NEW R
!
         IF(DABS(B(1)/H(1)).LE.E)GO TO 200
         IF(C(2).EQ.0.)GO TO 130
         R=R-B(1)/C(2)
         GO TO 140
  130    CONTINUE
         R=R+1
!
!     THIS SECTION FINDS THE QUADRATIC FACTORS
!
  140    CONTINUE
         DO 150 K=1,NP
            I=NC-K
            B(I)=H(I)-P*B(I+1)-Q*B(I+2)
            C(I)=B(I)-P*C(I+1)-Q*C(I+2)
  150    CONTINUE
!
!     CHECK IF P&Q ARE COEFFICIENTS OF QUADRATIC FACTOR - ELSE NEW P&Q
!
!CCCC    IF(H(2).EQ.0.) IF(DABS(B(2)/H(1))-E)160,160,170
!CCCC    IF(DABS(B(2)/H(2))-E)160,160,170
         IF(H(2).EQ.0.) THEN
           IF(DABS(B(2)/H(1))-E.LE.0.)GO TO 160
           GO TO 170
         ENDIF
         IF(DABS(B(2)/H(2))-E.LE.0.)GO TO 160
         GO TO 170
!
  160    CONTINUE
!CCCC    IF(DABS(B(1)/H(1))-E)240,240,170
         IF(DABS(B(1)/H(1))-E.LE.0.0)GO TO 240
  170    CONTINUE
         DENOM=C(3)**2-(C(2)-B(2))*C(4)
         IF(DENOM.EQ.0)GO TO 180
         P=P+(B(2)*C(3)-B(1)*C(4))/DENOM
         Q=Q+(-B(2)*(C(2)-B(2))+B(1)*C(3))/DENOM
         GO TO 110
!
  180    CONTINUE
         P=P-2.
         Q=Q*(Q+1.)
  110 CONTINUE
!
!     CHANGE E IF NOT CONVERGED AFTER 1000 ITERATIONS AND TRY AGAIN
!
      E=E*10.
      GO TO 190
!
!     THIS SECTION FINDS THE ROOTS OF THE LINEAR FACTORS
!
  200 CONTINUE
      NC=NC-1
      V(NC)=0.
      IF(IREV.EQ.-1)GO TO 210
      U(NC)=R
      GO TO 220
  210 CONTINUE
      U(NC)=1./R
!
!     STORE DEPRESSED POLYNOMIAL IN H TO FIND NEXT FACTOR
!
  220 CONTINUE
      DO 230 I=1,NC
         H(I)=B(I+1)
  230 CONTINUE
      GO TO 40
!
!     THIS SECTION FINDS THE ROOTS OF THE QUADRATIC FACTORS
!
  240 CONTINUE
      NC=NC-2
      IF(IREV.EQ.-1)GO TO 250
      QP=Q
      PP=P/2.
      GO TO 260
!
  250 CONTINUE
      QP=1./Q
      PP=P/(Q*2.)
  260 CONTINUE
      DISCRM=PP**2-QP
!
!     DETERMINE IF QUADRATIC ROOT IS REAL OR IMAGINARY
!
      IF(DISCRM.GE.0.)GO TO 270
      U(NC+1)=-PP
      U(NC)=-PP
      V(NC+1)=DSQRT(-DISCRM)
      V(NC)=-V(NC+1)
      GO TO 280
!
  270 CONTINUE
      IF(PP.EQ.0.)GO TO 290
      U(NC+1)=-(PP/DABS(PP))*(DABS(PP)+DSQRT(DISCRM))
      GO TO 300
  290 CONTINUE
      U(NC+1)=-DSQRT(DISCRM)
  300 CONTINUE
      V(NC+1)=0.
      U(NC)=QP/U(NC+1)
      V(NC)=0.
!
!     STORE DEPRESSED POLYNOMIAL IN H TO FIND NEXT FACTOR
!
  280 CONTINUE
      DO 310 I=1,NC
         H(I)=B(I+2)
  310 CONTINUE
      GO TO 40
  320 CONTINUE
      DO 330 I=1,N
         ROOT(I)=CMPLX(SNGL(U(I)),SNGL(V(I)))
  330 CONTINUE
      RETURN
      END SUBROUTINE EIGEN
                                                                                                                                  
!     ..........GREEN..........
                                                                                                                                  
      SUBROUTINE GREENZ(THT0,THETA,ROOT,N,M,LAG2,TITLE,VAR,   &
                       IOUNI2,IOUNI3)
!CCCC APRIL 1996.  ADD IOUNIT, IOUNI2, RENAME LAG TO LAG2, PASS
!CCCC DELTA THROUGH COMMON
!CCCC SUBROUTINE GREEN(THT0,THETA,ROOT,N,M,LAG,TITLE,DELTA,VAR)
!
!     PURPOSE--FINDS THE SMALL G'S OF THE GREEN'S FUNCTION
!
      CHARACTER*8 TITLE,TTL
!CCCC COMPLEX CROOT
!CCCC COMPLEX ROOT,CNUM,DEN,D(45),G(45)
!CCCC DIMENSION ROOT(1),THETA(1)
!CCCC COMPLEX ROOT(*),THETA(*),CNUM,DEN,D(45),G(45),CROOT
!CCCC COMPLEX ROOT(*),CNUM,DEN,D(45),G(45),CROOT
      COMPLEX ROOT(*),G(45),CROOT
!CCCC COMPLEX*16 CNUM,DEN,D(45)
      COMPLEX    CNUM,DEN,D(45)
      DIMENSION THETA(*)
!CCCC APRIL 1996.  USE DPCODD.INC
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODD.INC'
!CCCC COMMON /INDEX/ INDEX,MAXLAG,NOB,NSER,NPHI0,NPHI,NSEAS
!CCCC COMMON /JOSHI/ XX
!CCCC COMMON /FLAG/ IFMEAN,IFAT,IFSEAS,IFGREEN,IFSPECTR
      COMPLEX COMPSPECTR(45)
      DOUBLE PRECISION TOTGREEN, COMPGREEN(45), TOTSPECTR, RCOMPSPEC(45)
      DOUBLE PRECISION OMEGA, BETA
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IF(ISUBG4.EQ.'EENZ')THEN
        WRITE(ICOUT,52)LAG2,VAR
   52   FORMAT('LAG2,VAR = ',I8,G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RESVAR=0.0
      TTL=TITLE
         DO 50 I=1,N
               CNUM=THT0*ROOT(I)**(N-1)
                  DO 10 J=1,M
                     CNUM=CNUM-THETA(J)*ROOT(I)**(N-1-J)
10                CONTINUE
                  DEN=CMPLX(1.0,0.0)
                  DO 20 J=1,N
                     IF(I.NE.J) DEN=DEN*(ROOT(I)-ROOT(J))
20                CONTINUE
                  G(I)=CNUM/DEN
                  IF(AIMAG(ROOT(I)) .GE. 0.0)THEN
!
!     PRINT TERM CORRESPONDING TO THIS G
!
                  IF(AIMAG(ROOT(I)).EQ.0)THEN
!
!CCCC                WRITE(ICOUT,30) TTL,REAL(G(I)),REAL(ROOT(I))
!CC30                FORMAT(5X,A,3X,E11.5,'*(',E11.5,')**J',/)
!CCCC                CALL DPWRST('XXX','WRIT')
!
                  ELSE
                     CAR=CABS(ROOT(I))
!
!CCCC                WRITE(ICOUT,40)
!CCCC>                TTL,2.*CABS(G(I)),CAR,ACOS(REAL(ROOT(I))/CAR)
!CCCC>               /(DELTA*6.2831853),ATAN2(AIMAG(G(I)),REAL(G(I)))
!CCCC                WRITE(3,40)
!CCCC>                TTL,2.*CABS(G),CAR,ACOS(REAL(ROOT(I))/CAR)
!CCCC>               /(DELTA*6.2831853),ATAN2(AIMAG(G),REAL(G))
!CC40                FORMAT (5X,A,3X,E11.5,'*(',E11.5,')**J*',
!CCCC>               'COS{2*PI*',E11.5,'*J+(',E11.5,')}'/)
!CCCC                CALL DPWRST('XXX','WRIT')
!
                  ENDIF
               TTL='       +'
            ENDIF
50       CONTINUE
!
!CCCC WRITE(ICOUT,60) LAG2
!CC60 FORMAT(45X,'FOR j GREATER THAN OR EQUAL TO',I3,///)
!CC     ***************** ICOUNT IS FOR COUNTING THE NUMBER OF MODES ***
!CCCC CALL DPWRST('XXX','WRIT')
!
      J=0
      ICOUNT=0
      TOTGREEN=1.
      DO 701 I=1,N
         IF(AIMAG(ROOT(I)) .GE. 0.0)THEN
            IF(AIMAG(ROOT(I)).EQ.0)THEN
               ICOUNT=ICOUNT+1
               COMPGREEN(ICOUNT)=REAL(G(I))
            ELSE
               ICOUNT=ICOUNT+1
               COMPGREEN(ICOUNT)=2.0*REAL(G(I))
            ENDIF
            TOTGREEN=1.
         ENDIF
701   CONTINUE
!CCCC APRIL 1996.  USE IOUNI3
!CCCC WRITE(3,703) DELTA*J,TOTGREEN,(COMPGREEN(K),K=1,ICOUNT)
      IF(ICOUNT.GE.1.AND.ICOUNT.LE.45)THEN
      WRITE(IOUNI2,703) DELTA*J,TOTGREEN,(COMPGREEN(K),K=1,ICOUNT)
703   FORMAT(F8.4,2X,F11.4,45(2X,F11.4))
      ELSE
      WRITE(IOUNI2,1703) DELTA*J,TOTGREEN
1703  FORMAT(F8.4,2X,F11.4)
      ENDIF
!
      DO 802 J=1,100
      ICOUNT=0
      TOTGREEN=0.
      DO 801 I=1,N
               IF(AIMAG(ROOT(I)) .GE. 0.0)THEN
                  IF(AIMAG(ROOT(I)).EQ.0)THEN
!
!CC     *********EXPONENTIAL MODE ************************
!
                     ICOUNT=ICOUNT+1
                     COMPGREEN(ICOUNT) = G(I)*ROOT(I)**J
                   ELSE
!
!CC     ********** SINUSOIDAL MODE ****************************
!
                     CAR=CABS(ROOT(I))
                     OMEGA=ACOS(REAL(ROOT(I))/CAR)
                     BETA=ATAN2(AIMAG(G(I)),REAL(G(I)))
                     ICOUNT=ICOUNT+1
                     COMPGREEN(ICOUNT)=CAR**J*COS(OMEGA*J+BETA)
                     COMPGREEN(ICOUNT)=2.*CABS(G(I))*COMPGREEN(ICOUNT)
                  ENDIF
                  TOTGREEN=TOTGREEN+COMPGREEN(ICOUNT)
              ENDIF
801   CONTINUE
!CCCC APRIL 1996.  USE IOUNIT
        IF(IFGREEN .EQ. 1)THEN
!CCCC      WRITE(3,803) DELTA*J,TOTGREEN,(COMPGREEN(K),K=1,ICOUNT)
          IF(ICOUNT.GE.1.AND.ICOUNT.LE.45)THEN
           WRITE(IOUNI2,803) DELTA*J,TOTGREEN,(COMPGREEN(K),K=1,ICOUNT)
803         FORMAT(F8.4,2X,F11.4,45(2X,F11.4))
          ELSE
            WRITE(IOUNI2,1803) DELTA*J,TOTGREEN
1803        FORMAT(F8.4,2X,F11.4)
          ENDIF
        ENDIF
802   CONTINUE
      IFLAG=0
      EPS=0.0001
      DO 70 I=1,N
        IF(CABS(ROOT(I)) .GT. 1.0+EPS)THEN
            IFLAG = IFLAG+1
        ENDIF
!
!CCCC CHANGED HERE
!CCCC WRITE(ICOUT,*)I,ROOT(I),IFLAG
!CCCC CALL DPWRST('XXX','WRIT')
!
70    CONTINUE
!
      WRITE(ICOUT,999)
  999 FORMAT(1H )
      CALL DPWRST('XXX','WRIT')
!
      WRITE(ICOUT,81)
   81 FORMAT(1H ,   &
      'Index  Roots    Roots   Natural  Damping   Green    Green',   &
      '   Var.')
      CALL DPWRST('XXX','WRIT')
!
      WRITE(ICOUT,82)
   82 FORMAT(1H ,   &
      '       Real     Imag     Freq.    Ratio    Real     Imag ',   &
      '   D(I)')
      CALL DPWRST('XXX','WRIT')
!
      WRITE(ICOUT,84)
   84 FORMAT(1H ,   &
      '---------------------------------------------------------',   &
      '---------')
      CALL DPWRST('XXX','WRIT')
!
!CCCC CHANGED HERE
!CCCC WRITE(ICOUT,*)IFLAG
!CCCC CALL DPWRST('XXX','WRIT')
!CCCC PAUSE
!
      IF(IFLAG.LE.0)THEN
         Z=0.0
         DO 80 I=1,N
            D(I)=CMPLX(0.0,0.0)
            EPS=0.0001
!
            DO 90 J=1,N
               DENOM=1.0-(ROOT(I)*ROOT(J))
!CCCC          PATCH--THE FOLLOWING 2 LINES MUST BE FIXED
               IF(ABS(DENOM).LE.EPS)DENOM=EPS
               D(I) = D(I)+G(I)*G(J)/DENOM
   90       CONTINUE
!
!CCCC       D(I)=D(I)*XX/(NOB-N-M-1)    RES. VAR INSERTED LATER
            RESVAR=XX/(NOB-N-M-1)
!
   80    CONTINUE
      ENDIF
!
!     FIND FREQUENCY AND DAMPING FOR 2ND ORDER ROOT
!
      DO 85 I=1,N
         AIROOT=AIMAG(ROOT(I))
         ZD=(-999.0)
         IF(AIROOT.NE.0)THEN
            CROOT=CLOG(ROOT(I))/DELTA
            ACROOT=CABS(CROOT)
            W=ACROOT/6.2831853
            ZD=-REAL(CROOT)/ACROOT
!
!        FIND BREAK FREQUENCY FOR REAL ROOTS.
!        IF NEGATIVE ROOT - SET BREAK FREQUENCY AT NYQUIST FREQ
!        IF POSITIVE ROOT -CALCULATE BREAK FREQUENCY
!
         ELSEIF(AIROOT.EQ.0)THEN
            RROOT=REAL(ROOT(I))
            IF(RROOT.LE.0)THEN
               W=0.5/DELTA
            ELSE
               W=-LOG(RROOT)/(DELTA*6.2831853)
            ENDIF
         ENDIF
!
         IF(IFLAG.LE.0.AND.AIMAG(ROOT(I)).NE.0)THEN
            WRITE(ICOUT,101)I,REAL(ROOT(I)),AIMAG(ROOT(I)),W,ZD,   &
            REAL(G(I)),AIMAG(G(I)),REAL(D(I))
  101       FORMAT(1H ,I2,2X,6F9.3,E10.3)
            CALL DPWRST('XXX','WRIT')
!
         ELSEIF(IFLAG.GT.0.AND.AIMAG(ROOT(I)).NE.0)THEN
            WRITE(ICOUT,102)I,REAL(ROOT(I)),AIMAG(ROOT(I)),W,ZD,   &
            REAL(G(I)),AIMAG(G(I))
  102       FORMAT(1H ,I2,6F9.3,'  Infin.')
            CALL DPWRST('XXX','WRIT')
!
         ELSEIF(IFLAG.LE.0.AND.AIMAG(ROOT(I)).EQ.0)THEN
            WRITE(ICOUT,103)I,REAL(ROOT(I)),AIMAG(ROOT(I)),W,   &
            REAL(G(I)),AIMAG(G(I)),REAL(D(I))
  103       FORMAT(1H ,I2,2X,3F9.3,'  Undef. ',2F9.3,E10.3)
            CALL DPWRST('XXX','WRIT')
!
         ELSEIF(IFLAG.GT.0.AND.AIMAG(ROOT(I)).EQ.0)THEN
            WRITE(ICOUT,104)I,REAL(ROOT(I)),AIMAG(ROOT(I)),W,   &
            REAL(G(I)),AIMAG(G(I))
  104       FORMAT(1H ,I2,2X,3F9.3,'  Undef. ',2F9.3,'  Infin.')
            CALL DPWRST('XXX','WRIT')
!
         ENDIF
!
         Z = Z+REAL(D(I))
   85 CONTINUE
!
      IF(IFLAG.GT.0)THEN
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','WRIT')
!CCCC AUGUST, 1995.  MODIFIY FOLLOWING LINE FOR CRAY.
!CCCC CRAY DOESN'T ALLOW FREE FORMAT FOR INTERNAL WRITE.
!CCCC    WRITE(ICOUT,*)'Caution--Root > 1 ==> Non-Stationary'
         WRITE(ICOUT,9085)
 9085    FORMAT('Caution--Root > 1 ==> Non-Stationary')
         CALL DPWRST('XXX','WRIT')
      ENDIF
!
!CCCC STEP XX--CALCULATE SPECTRA
!
      DO 805 J=1,500
!
      ICOUNT=0
                                                                                                                                  
      TOTSPECTR=0.
      DEN = CEXP(CMPLX(0.0,-2.0*3.141593*0.001*J))
      DO 806 I=1,N
               IF(AIMAG(ROOT(I)) .GE. 0.0)THEN
                  IF(AIMAG(ROOT(I)).EQ.0)THEN
!
!CC     *********EXPONENTIAL MODE ************************
!
                     ICOUNT=ICOUNT+1
                     COMPSPECTR(ICOUNT) =   &
                     2.*DELTA*D(I)*(1.-ROOT(I)*ROOT(I))   &
                     /((1.-ROOT(I)*DEN)*(1.-(ROOT(I)/DEN)))
                   ELSE
!
!CC     ********** SINUSOIDAL MODE ****************************
!
                     ICOUNT=ICOUNT+1
                     COMPSPECTR(ICOUNT) =   &
                     4.*DELTA*D(I)*(1.-ROOT(I)*ROOT(I))   &
                     /((1.-ROOT(I)*DEN)*(1.-(ROOT(I)/DEN)))
!
!CCCC CHANGE HERE TWICE
!CCCC WRITE(ICOUT,*) J,I,ICOUNT,D(I),ROOT(I)
!CCCC CALL DPWRST('XXX','WRIT')
!
!CCCC WRITE(ICOUT,*)DEN,DELTA,G(I),THETA(I)
!CCCC CALL DPWRST('XXX','WRIT')
!
!CCCC WRITE(ICOUT,*)COMPSPECTR(ICOUNT)
!CCCC CALL DPWRST('XXX','WRIT')
!
                  ENDIF
                  COMPSPECTR(ICOUNT)=COMPSPECTR(ICOUNT)*RESVAR
                  RCOMPSPEC(ICOUNT)=REAL(COMPSPECTR(ICOUNT))
!
!CCCC PATCH--THE FOLLOWING LINE MUST BE FIXED
!
                  RCOMPSPEC(ICOUNT)=ABS(RCOMPSPEC(ICOUNT))
                  TOTSPECTR=TOTSPECTR+RCOMPSPEC(ICOUNT)
!
!CCCC CHANGED HERE
!CCCC WRITE(ICOUT,*)RCOMPSPEC(ICOUNT),TOTSPECTR
!CCCC CALL DPWRST('XXX','WRIT')
!
              ENDIF
806   CONTINUE
      IF(IFSPECTR .EQ. 1)THEN
!
!CCCC APRIL 1996.  USE IOUNI2
!CCCC WRITE(4,808) J*0.001/DELTA,TOTSPECTR,(RCOMPSPEC(K),K=1,ICOUNT)
      WRITE(IOUNI3,808)   &
        J*0.001/DELTA,TOTSPECTR,(RCOMPSPEC(K),K=1,ICOUNT)
  808 FORMAT(E11.4,2X,E11.4,45(2X,E11.4))
!
      ENDIF
805   CONTINUE
                                                                                                                                  
      RETURN
      END SUBROUTINE GREENZ
                                                                                                                                  
!     ..........SPECTRUM..........
                                                                                                                                  
      SUBROUTINE SPCTRM(THT0,THETA,PHI,N,M,LAG2,IOUNIT)
!CCCC APRIL 1996.  ADD TO ARGUMENT LIST, MAKE NAME 6 CHARACTERS
!CCCC RENAME LAG TO LAG2, PASS DELTA THROUGH COMMON
!CCCC SUBROUTINE SPECTRUM(THT0,THETA,PHI,N,M,LAG,DELTA)
!
!      PURPOSE--CALCULATES THE SPECTRUM WHEN THE NUMBER OF
!               MOVING AVERAGE PARAMETERS IS MORE THAN OR EQUAL TO AR
!
!CCCC APRIL 1996.  USE DPCODD.INC
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODD.INC'
!CCCC DOUBLE PRECISION XX
!CCCC COMMON /INDEX/ INDEX,MAXLAG,NOB,NSER,NPHI0,NPHI,NSEAS
!CCCC COMMON /JOSHI/ XX
!CCCC DIMENSION PHI(1),THETA(1)
      DIMENSION PHI(*),THETA(*)
      DOUBLE PRECISION TOTSPECTR
!CCCC COMMON /FLAG/ IFMEAN,IFAT,IFSEAS,IFGREEN,IFSPECTR
      COMPLEX CNUM,DEN
                                                                                                                                  
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IF(ISUBG4.EQ.'DDS3')THEN
        WRITE(ICOUT,52)LAG2,THT0
   52   FORMAT('LAG2,THTO = ',I8,G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      WRITE(*,*) 'N=',N
      DO 405 I=1,N
      WRITE(*,*) 'SPEC-PHI',I,'=',PHI(I)
405   CONTINUE
      WRITE(*,*) 'M=',M
      DO 408 I=1,M
      WRITE(*,*) 'SPEC-THETA',I,'=',THETA(I)
408   CONTINUE
      WRITE(*,*) '****RSS***=', XX
      DO 400 J=1,500
      TOTSPECTR=2.*DELTA*XX/(NOB-N-M-1)
      DEN=CEXP(CMPLX(0.,0.001*2.*3.14*J*N))
      CNUM=CEXP(CMPLX(0.,0.001*2.*3.14*J*M))
      DO 401 I=1,N
        DEN=DEN-(PHI(I)*CEXP(CMPLX(0.,0.001*2.*3.14*J*(N-I))))
401   CONTINUE
      DO 402 I=1,M
        CNUM=CNUM-(THETA(I)*CEXP(CMPLX(0.,0.001*2.*3.14*J*(M-I))))
402   CONTINUE
      TOTSPECTR=TOTSPECTR*(CABS(CNUM))**2
      TOTSPECTR=TOTSPECTR/((CABS(DEN))**2)
!CCCC APRIL 1996.  USE UNIT NUMBER INSTEAD OF HARD-CODING
      IF(IFSPECTR .EQ. 1)THEN
!CCCC    WRITE(4,403) J*0.001/DELTA,TOTSPECTR
         WRITE(IOUNIT,403) J*0.001/DELTA,TOTSPECTR
403      FORMAT(E11.4,2X,E11.4)
      ENDIF
400   CONTINUE
!
      RETURN
      END SUBROUTINE SPCTRM

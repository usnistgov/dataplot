      SUBROUTINE DPDDS(XTEMP1,MAXNXT,
     1                 ICAPSW,IFORSW,
     1                 IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--CARRY OUT A DDS (DATA-DEPENDENT SYSTEM) ANALYSIS
C              (1-SAMPLE)
C     EXAMPLE--DDS Y 6 5 DELT
C              DDS Y 6 5  (== DDS Y 6 5 1)
C              DDS Y      (== DDS Y 6 5 1)
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--94/4
C     ORIGINAL VERSION--MARCH     1994.
C     UPDATED         --APRIL     1996. DDS CODE MODIFIED (ALAN):
C                                       A) SOME DIMENSIONS TO DPDDS, USE
C                                          EQUIVALENCE
C                                       B) I/O CONSISTENT WITH DATAPLOT
C                                       C) USE IERROR RATHER THAN STOP
C                                       D) INCLUDE FILE FOR DDS COMMON
C                                          BLOCKS AND PARAMETER STATEMENTS
C                                       THESE CHANGES PROPOGATE TO LOWER
C                                       LEVEL DDS ROUTINES
C     UPDATED         --MAY       2011. USE DPPARS AND DPPAR3
C     UPDATED         --JULY      2019. TWEAK SCRATCH SPACE
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
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
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODD.INC'
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCODA.INC'
C
      DIMENSION XTEMP1(*)
C
      DIMENSION PRED2(MAXOBV)
      DIMENSION RES2(MAXOBV)
      DIMENSION XDDS(MAXOBV,MXSER)
      DIMENSION YDDS(MAXOBV,MXSER)
      DIMENSION Y2(100)
      DIMENSION AT(MXNOB1,MXSER)
C
      EQUIVALENCE (X3D(1),PRED2(1))
      EQUIVALENCE (X3D(MAXOBV+1),RES2(1))
      EQUIVALENCE (D(1),XDDS(1,1))
      EQUIVALENCE (DSYMB(1),YDDS(1,1))
      EQUIVALENCE (DFILL(1),AT(1,1))
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPDD'
      ISUBN2='S   '
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
C               ********************************
C               **  TREAT THE DDS    CASE     **
C               ********************************
C
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
C
C               *********************************
C               **  STEP 2--                   **
C               **  EXTRACT THE VARIABLE LIST  **
C               *********************************
C
      ISTEPN='2'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PDDS')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
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
C               ****************************************
C               **  STEP 22--                         **
C               **  CHECK THE VALIDITY OF ARGUMENTS   **
C               **  2 - 4 (THESE SHOULD BE PARAMETERS **
C               **  OR NUMBERS).                      **
C               ****************************************
C
      ISTEPN='22'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PDDS')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IORDAR=2
      IORDMA=1
C
      IF(NUMVAR.GE.2)THEN
        IORDAR=INT(PVAR(2))
      ENDIF
C
      IF(NUMVAR.GE.3)THEN
        IORDMA=INT(PVAR(3))
      ENDIF
C
      DELTAT=1.0
      IF(NUMVAR.GE.4)THEN
        DELTAT=PVAR(4)
      ENDIF
C
      ICOL=1
      NUMVA2=1
      CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1            INAME,IVARN1,IVARN2,IVARTY,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1            MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1            MAXCP4,MAXCP5,MAXCP6,
     1            V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1            Y,XTEMP1,XTEMP1,N1,NLOCA2,NLOCA3,ICASE,
     1            IBUGA3,ISUBRO,IFOUND,IERROR)
       IF(IERROR.EQ.'YES')GOTO9000
C
C               *********************************
C               **  STEP 52--                  **
C               **  FORM THE DDS ANALYSIS      **
C               *********************************
C
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
        DO5215I=1,N1
          WRITE(ICOUT,5216)I,Y(I)
 5216     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
 5215   CONTINUE
      ENDIF
C
CCCCC APRIL 1996.  ADD XDDS, YDDS, Y2 TO ARGUMENT LIST (DIMENSIONING DONE
CCCCC IN DPDDS RATHER THAN IN DPDDS2 AND DPDDS3 TO ALLOW USE OF EQUIVALENCE
CCCCC WITH DATAPLOT SCRATCH ARRAYS)
      CALL DPDDS2(Y,N1,IORDAR,IORDMA,DELTAT,NUMVAR,ILOCV,
CCCCC1            XTEMP1,XTEMP2,MAXNXT,PRED2,RES2,RESSD,
CCCCC1            RESDF,IBUGA3,IERROR)
     1            XDDS,YDDS,AT,Y2,MAXNXT,
     1            PRED2,RES2,RESSD,RESDF,
     1            IBUGA3,ISUBRO,IERROR)
C
C               ***************************************
C               **  STEP 15--                        **
C               **  UPDATE INTERNAL DATAPLOT TABLES  **
C               ***************************************
C
      ISTEPN='15'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PDDS')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICOLPR=MAXCP1
      ICOLRE=MAXCP2
      IREPU='OFF'
      IRESU='ON'
      NLEFT=N1
      CALL UPDAPR(ICOLPR,ICOLRE,PRED2,RES2,PRED,RES,ISUB,NLEFT,
     1IREPU,REPSD,REPDF,IRESU,RESSD,RESDF,ALFCDF,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,ILOCN,IBUGA3,IERROR)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
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
C
      RETURN
      END

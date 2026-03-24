      SUBROUTINE DP1IPT(ICAPSW,IFORSW,ISUBRO,IBUGA2,IBUGA3,IBUGQ,   &
                        IFOUND,IERROR)
!
!     PURPOSE--PERFORM A ONE SAMPLE INTERLABORATORY PROFICIENCY
!              TESTING PROGRAM ACCORDING TO ASTM E 2489 - 06
!              STANDARD.
!
!              THE DATA CONSISTS OF DATA GROUPED BY LABS.  THE
!              COMMAND SYNTAX IS:
!
!                  ONE SAMPLE PROFICIENCY TEST  Y  LABID
!
!              COMMAND IS TO GENERATE THE FOLLOWING 3 TABLES:
!              1) TEST RESULTS SORTED BY LAB ID
!              2) TEST RESULTS SORTED BY TEST RESULTS IN DESCENDING
!                 ORDER, LABEL POINTS AS TYPICAL, UNUSUAL, ETC.
!              3) TABLE WITH DATA SORTED INTO BINS
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2008/10
!     ORIGINAL VERSION--OCTOBER   2008.
!     UPDATED         --FEBRUARY  2011. USE DPPARS, DPPAR3
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES----------------
!
      CHARACTER*4 ICAPSW
      CHARACTER*4 IFORSW
      CHARACTER*4 ISUBRO
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGQ
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      CHARACTER*4 ICASE
      CHARACTER*40 INAME
      PARAMETER (MAXSPN=20)
      CHARACTER*4 IVARN1(MAXSPN)
      CHARACTER*4 IVARN2(MAXSPN)
      CHARACTER*4 IVARTY(MAXSPN)
      REAL PVAR(MAXSPN)
      INTEGER ILIS(MAXSPN)
      INTEGER NRIGHT(MAXSPN)
      INTEGER ICOLR(MAXSPN)
!
!----------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZI.INC'
      INCLUDE 'DPCOZC.INC'
!CCCC PARAMETER (MAXROW=MAXOBV/20)
      PARAMETER (MAXROW=35)
!
      DIMENSION Y1(MAXOBV)
      DIMENSION YTEMP(MAXOBV)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION YTEMP2(MAXOBV)
      DIMENSION XTEMP2(MAXOBV)
      DIMENSION AMAT(MAXOBV)
      INTEGER LABID(MAXOBV)
      INTEGER LABTMP(MAXOBV)
      INTEGER IDIGI2(MAXOBV)
      INTEGER NCVALU(MAXOBV)
      INTEGER NCOLSP(MAXOBV)
      INTEGER NTOT(MAXOBV)
      INTEGER ROWSEP(MAXOBV)
      INTEGER NCTIT2(MAXOBV)
      CHARACTER*20 IVALZZ(MAXROW,6)
!
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),YTEMP(1))
      EQUIVALENCE (GARBAG(IGARB3),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB4),YTEMP2(1))
      EQUIVALENCE (GARBAG(IGARB5),XTEMP2(1))
      EQUIVALENCE (GARBAG(IGARB6),AMAT(1))
!
      EQUIVALENCE (IGARBG(IIGAR1),LABID(1))
      EQUIVALENCE (IGARBG(IIGAR2),LABTMP(1))
      EQUIVALENCE (IGARBG(IIGAR3),IDIGI2(1))
      EQUIVALENCE (IGARBG(IIGAR4),NCVALU(1))
      EQUIVALENCE (IGARBG(IIGAR5),NCOLSP(1))
      EQUIVALENCE (IGARBG(IIGAR6),NTOT(1))
      EQUIVALENCE (IGARBG(IIGAR7),ROWSEP(1))
      EQUIVALENCE (IGARBG(IIGAR8),NCTIT2(1))
      EQUIVALENCE (CGARBG(1),IVALZZ(1,1))
!
!-----COMMON----------------------------------------------------
!
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
!
!----------------------------------------------------------------
!
!-----COMMON VARIABLES (GENERAL)---------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT------------------------------------------------
!
      ISUBN1='DP1I'
      ISUBN2='PT  '
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
      IERROR='NO'
      IFOUND='YES'
!
!               **************************************************
!               **  TREAT THE ONE SAMPLE PROFICIENCY TEST CASE  **
!               **************************************************
!
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'1IPT')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DP1IPT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA2,IBUGA3,IBUGQ,ISUBRO
   52   FORMAT('IBUGA2,IBUGA3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *********************************
!               **  STEP 4--                   **
!               **  EXTRACT THE VARIABLE LIST  **
!               *********************************
!
      ISTEPN='4'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'1IPT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      INAME='ONE SAMPLE PROFICIENCY TEST'
      MINNA=2
      MAXNA=100
      MINN2=2
      IFLAGE=1
      IFLAGM=0
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=2
      MAXNVA=2
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'1IPT')THEN
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
                            ICOLR(I),PVAR(I)
  287       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',   &
                   'ICOLR(I),PVAR(I) = ',I8,2X,A4,A4,2X,3I8,G15.7)
            CALL DPWRST('XXX','BUG ')
  285     CONTINUE
        ENDIF
      ENDIF
!
      ICOL=1
      CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                  INAME,IVARN1,IVARN2,IVARTY,   &
                  ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,   &
                  MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                  MAXCP4,MAXCP5,MAXCP6,   &
                  V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                  Y1,TEMP1,TEMP1,NS,NLOCA2,NLOCA3,ICASE,   &
                  IBUGA3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
        DO 310 I=1,NS
          LABID(I)=INT(TEMP1(I)+0.1)
  310   CONTINUE
!
!               ****************************************************
!               **  STEP 9--                                      **
!               **  CARRY OUT THE ONE SAMPLE PROFICIENCY TEST     **
!               ****************************************************
!
      ISTEPN='9'
!
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'1IPT')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,711)
  711   FORMAT('***** FROM DP1IPT, AS WE ARE ABOUT TO CALL DP1IP2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,712)MAXN,NS,NUMVAR
  712   FORMAT('MAXN,NS,NUMVAR = ',3I8)
        CALL DPWRST('XXX','BUG ')
        DO 715 I=1,NS
          WRITE(ICOUT,716)I,Y1(I),LABID(I)
  716     FORMAT('I,Y1(I),LABID(I) = ',I6,2X,2G15.7)
          CALL DPWRST('XXX','BUG ')
  715   CONTINUE
      ENDIF
!
      IWRITE='OFF'
      CALL DP1IP2(Y1,LABID,NS,   &
                  YTEMP,LABTMP,TEMP1,YTEMP2,XTEMP2,   &
                  AMAT,IDIGI2,NCVALU,NCOLSP,NTOT,ROWSEP,NCTIT2,   &
                  IVALZZ,   &
                  IWRITE,MAXOBV,IFORSW,MAXROW,   &
                  CLLIMI,CLWIDT,   &
                  YMED,YIQR,REPSD,   &
                  ICAPSW,ICAPTY,   &
                  ISUBRO,IBUGA3,IERROR)
!
!               ***************************************
!               **  STEP 10--                        **
!               **  UPDATE INTERNAL DATAPLOT TABLES  **
!               ***************************************
!
      ISTEPN='10'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'1IPT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IH='MEDY'
      IH2='    '
      VALUE0=YMED
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA2,IERROR)
!
      IH='IQRY'
      IH2='    '
      VALUE0=YIQR
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA2,IERROR)
!
      IH='REPS'
      IH2='DY  '
      VALUE0=REPSD
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA2,IERROR)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'1IPT')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DP1IPT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IFOUND,IERROR
 9014   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DP1IPT
      SUBROUTINE DP1IP2(Y,LABID,N,   &
                        YTEMP,LABTMP,TEMP1,YTEMP2,XTEMP2,   &
                        AMAT,IDIGI2,NCVALU,NCOLSP,NTOT,ROWSEP,NCTIT2,   &
                        IVALUE,   &
                        IWRITE,MAXOBV,IFORSW,MAXROW,   &
                        CLLIMI,CLWIDT,   &
                        YMED,YIQR,REPSD,   &
                        ICAPSW,ICAPTY,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--PERFORM A ONE-SAMPLE INTERLABORATORY PROFICIENCY
!              STUDY ACCORDING TO ASTM E 2489 - 06 STANDARD.
!
!              PROFICIENCY TESTING IS THE USE OF INTERLABORATORY
!              COMPARISONS FOR THE DETERMINATION OF LABORATORY
!              TESTING TESTING OR MEASUREMENT PERFORMANCE.
!
!              METHOD A COVERS TESTING PGROGRAMS USING A SINGLE
!              SAMPLE (EACH LABORATORY SUBMITS A SINGLE TEST
!              RESULT).  SO THE DATA CONSISTS OF A RESPONSE
!              VARIABLE AND A LAB-ID VARIABLE.  THIS ROUTINE
!              GENERATES THE FOLLOWING OUTPUTS:
!
!                 1) A TABLE OF THE LAB-ID AND TEST RESULT WHERE
!                    THE TABLE IS SORTED BY LAB-ID.
!
!                 2) A TABLE OF THE LAB-ID SORTED BY THE VALUE OF
!                    THE TEST RESULT (SORTED FROM HIGH TO LOW).
!
!                 3) A FREQUENCY TABLE OF THE TEST VALUES.
!
!     PRINTING--YES
!     SUBROUTINES NEEDED--MEDIAN, LOWHIN, UPPHIN
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2008/10
!     ORIGINAL VERSION--OCTOBER   2008.
!     UPDATED         --JANUARY   2012.
!     UPDATED         --JANUARY   2014. LATEX DOES NOT ALLOW MULTI-PAGE
!                                       TABLES, SO NEED TO INCLUDE
!                                       TRAILER/HEADER FOR TABLE FOR
!                                       EVERY CALL TO DPDT5B.  HOWEVER,
!                                       FOR OTHER FORMATS, WANT TO PRINT
!                                       AS SINGLE TABLE, SO NEED TO BE
!                                       MORE CAREFUL WITH THE LOGIC.
!
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES--------------
!
      CHARACTER*4 IFORSW
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IRELAT
      CHARACTER*4 IHSTO2
!
!----------------------------------------------------------------
!
      REAL Y(*)
      REAL YTEMP(*)
      REAL TEMP1(*)
      REAL YTEMP2(*)
      REAL XTEMP2(*)
      REAL CLLIMI(*)
      REAL CLWIDT(*)
      INTEGER LABID(*)
      INTEGER LABTMP(*)
!
      INCLUDE 'DPCOST.INC'
!
      LOGICAL IFLAGA
      LOGICAL IFLAGB
      INTEGER IFLAG4
      INTEGER IFLAG5
!
      PARAMETER(NUMCLI=6)
      PARAMETER(MAXLIN=3)
!CCCC PARAMETER(MAXROW=350)
      CHARACTER*65 ITITLE
      CHARACTER*60 ITITL9
      CHARACTER*4  ALIGN(NUMCLI)
      CHARACTER*4  VALIGN(NUMCLI)
      INTEGER      IDIGI2(MAXROW,NUMCLI)
      INTEGER      NTOT(MAXROW)
      INTEGER      ROWSEP(MAXROW)
      CHARACTER*20 ITITL2(MAXLIN,NUMCLI)
      CHARACTER*20 IVALUE(MAXROW,NUMCLI)
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
      CHARACTER*60 ITTEMP
!
      CHARACTER*4 IRTFMD
      COMMON/COMRTF/IRTFMD
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT------------------------------------------------
!
      IERROR='NO'
      IRELAT='OFF'
      IRHSTG='    '
      ISUBN1='DP1I'
      ISUBN2='P2  '
!
      ICNT3=0
!
      NUMDIG=4
      IF(IFORSW.EQ.'1')NUMDIG=2
      IF(IFORSW.EQ.'2')NUMDIG=2
      IF(IFORSW.EQ.'3')NUMDIG=3
      IF(IFORSW.EQ.'4')NUMDIG=4
      IF(IFORSW.EQ.'5')NUMDIG=5
      IF(IFORSW.EQ.'6')NUMDIG=6
      IF(IFORSW.EQ.'7')NUMDIG=7
      IF(IFORSW.EQ.'8')NUMDIG=7
      IF(IFORSW.EQ.'9')NUMDIG=7
      IF(IFORSW.EQ.'0')NUMDIG=7
      IF(IFORSW.EQ.'-2')NUMDIG=-2
      IF(IFORSW.EQ.'-3')NUMDIG=-3
      IF(IFORSW.EQ.'-4')NUMDIG=-4
      IF(IFORSW.EQ.'-5')NUMDIG=-5
      IF(IFORSW.EQ.'-6')NUMDIG=-6
      IF(IFORSW.EQ.'-7')NUMDIG=-7
      IF(IFORSW.EQ.'-8')NUMDIG=-8
      IF(IFORSW.EQ.'-9')NUMDIG=-9
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'1IP2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DP1IP2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)N
   52   FORMAT('N = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,Y(I),LABID(I)
   56     FORMAT('I,Y(I),LABID(I) = ',I8,G15.7,I8)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      IF(N.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR IN ONE-SAMPLE PROFICIENCY TEST--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,102)
  102   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE ',   &
               'ONE-SAMPLE PROFICIENCY TEST')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,103)N
  103   FORMAT('      MUST BE AT LEAST 2; THE ENTERED NUMBER OF ',   &
               'OBSERVATIONS = ',I8)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               ***********************************************
!               **  STEP 2.1--                               **
!               **  PERFORM THE BASIC CALCULATIONS.  OBTAIN: **
!               **  1) REPEATABILITY STANDARD DEVIATION      **
!               **  2) REPRODUCABILITY STANDARD DEVIATION    **
!               **  3) H CONSISTENCY STATISTIC               **
!               **  4) K CONSISTENCY STATISTIC               **
!               ***********************************************
!
      ISTEPN='2.1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'1IP2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IWRITE='OFF'
      CALL SORT(Y,N,TEMP1)
      ICNT=0
      DO 1010 I=N,1,-1
        ICNT=ICNT+1
        YTEMP(ICNT)=TEMP1(I)
 1010 CONTINUE
!
      CALL MEDIAN(YTEMP,N,IWRITE,TEMP1,MAXOBV,YMED,IBUGA3,IERROR)
      CALL LOWHIN(YTEMP,N,IWRITE,TEMP1,MAXOBV,YLOWHI,IBUGA3,IERROR)
      CALL UPPHIN(YTEMP,N,IWRITE,TEMP1,MAXOBV,YUPPHI,IBUGA3,IERROR)
      YIQR=YUPPHI - YLOWHI
      ATEMP=3.0*YIQR
      YOUTUP=YUPPHI + ATEMP
      YOUTLO=YLOWHI - ATEMP
      ATEMP=1.5*YIQR
      YINNUP=YUPPHI + ATEMP
      YINNLO=YLOWHI - ATEMP
      REPSD=YIQR/1.35
!
      IF(MOD(N,2).EQ.1)THEN
        NMED1=(N/2) + 1
        NMED2=-99
      ELSE
        NMED1=N/2
        NMED2=NMED1+1
      ENDIF
      IF(MOD(NMED1,2).EQ.1)THEN
        NUPP1=(NMED1/2)+1
        NUPP2=-99
      ELSE
        NUPP1=NMED1/2
        NUPP2=NUPP1+1
      ENDIF
      NLOW1=NUPP1
      NLOW2=NUPP2
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'1IP2')THEN
        WRITE(ICOUT,1091)
 1091   FORMAT('NMED1,NMED2,NUPP1,NUPP2,NLOW1,NLOW2 = ',6I8)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      IF(IPRINT.NE.'ON')GO TO 9000
!
!     PRINT HEADER LINE
!
      IRTFMD='OFF'
      IFNTSZ=-1
      IFLAGA=.TRUE.
      IFLAGB=.TRUE.
      ISIZE=-1
      ITTEMP='E2489 - 06: One-Sample Proficiency Analysis'
      NCTEMP=43
      NTOTAL=NCTEMP
      NBLNK1=1
      NBLNK2=1
      ITYPE=1
      CALL DPDTXT(ITTEMP,NCTEMP,CPUMIN,NUMDIG,   &
                  NTOTAL,NBLNK1,NBLNK2,IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,   &
                  ISUBRO,IBUGA3,IERROR)
      ISIZE=-99
      IFNTSZ=0
!
!     TABLE 1 - TEST RESULTS SORTED BY LAB-ID
!
      CALL SORTC5(LABID,Y,N,LABTMP,YTEMP)
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'1IP2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7007)
 7007   FORMAT('***** TABLE 1 - AFTER SORTC5:')
        CALL DPWRST('XXX','BUG ')
        DO 7008 I=1,N
          WRITE(ICOUT,7009)I,LABTMP(I),YTEMP(I)
 7009     FORMAT('I,LABTMP(I),YTEMP(I) = ',2I8,G15.7)
          CALL DPWRST('XXX','BUG ')
 7008   CONTINUE
      ENDIF
!
      ITITLE(1:38)='Table 1: Test Results Sorted by Lab ID'
      NCTITL=38
      ITITL9=' '
      NCTIT9=0
!
      NUMCOL=2
      NUMLIN=1
!
      ITITL2(1,1)='Lab'
      NCTIT2(1,1)=3
      NCOLSP(1,1)=1
      ITITL2(1,2)='Test Result'
      NCTIT2(1,2)=11
      NCOLSP(1,2)=1
!
      IWHTML(1)=150
      IWHTML(2)=200
      IINC1=1200
      IINC2=1800
      IWRTF(1)=IINC1
      IWRTF(2)=IWRTF(1)+IINC2
!
      NMAX=0
      ICNT=0
      ICNT2=0
      DO 1210 I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=18
        IF(I.EQ.1)NTOT(I)=15
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
 1210 CONTINUE
      DO 1213 J=1,N
!
        ICNT=ICNT+1
        IF(ICNT.GT.MAXROW)THEN
          ICNT=ICNT-1
          IF(ICAPTY.EQ.'LATE')THEN
            IFRST=.TRUE.
            ILAST=.TRUE.
            IFLAGS=.TRUE.
            IFLAGE=.TRUE.
          ELSE
            IFRST=.TRUE.
            IFLAGS=.TRUE.
            IF(ICNT2.GT.0)THEN
              IFRST=.FALSE.
              IFLAGS=.FALSE.
            ENDIF
            IFLAGE=.FALSE.
            ILAST=.FALSE.
            IF(J.EQ.ICNT3)THEN
              ILAST=.TRUE.
              IFLAGE=.TRUE.
            ENDIF
          ENDIF
          CALL DPDT5B(ITITLE,NCTITL,   &
                      ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                      MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                      IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                      IDIGI2,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                      NCOLSP,ROWSEP,   &
                      ICAPSW,ICAPTY,IFRST,ILAST,   &
                      IFLAGS,IFLAGE,   &
                      ISUBRO,IBUGA3,IERROR)
          ICNT=1
          ICNT2=ICNT2+1
        ENDIF
!
        DO 1215 I=1,NUMCOL
          IDIGI2(ICNT,I)=NUMDIG
          IF(I.EQ.1)IDIGI2(ICNT,I)=0
          IVALUE(ICNT,I)=' '
          NCVALU(ICNT,I)=0
 1215   CONTINUE
        AMAT(ICNT,1)=REAL(LABTMP(J))
        AMAT(ICNT,2)=YTEMP(J)
        ROWSEP(ICNT)=0
        IF(J.EQ.N)ROWSEP(ICNT)=1
 1213 CONTINUE
!
      IF(ICNT.GT.0)THEN
        IFRST=.TRUE.
        ILAST=.TRUE.
        IF(ICAPTY.EQ.'LATE')THEN
          IFLAGS=.TRUE.
        ELSE
          IFLAGS=.TRUE.
          IF(ICNT2.GT.0)IFLAGS=.FALSE.
        ENDIF
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
      ENDIF
!
!     TABLE 2 - SORT IN DESCENDING ORDER FOR Y2
!
!               ALSO ADD SOME INFORMATION ABOUT MEDIAN, LOWER/UPPER
!               HINGES, AND DEFINE POINT AS TYPICAL OR OUTLIER.
!
!               2019/08: FOR REPEAT VALUES, USER OPTION TO LIST
!                        INDIVIDUAL LABS FOR UNUSUAL OR EXTREMELY
!                        UNUSUAL CASES.  THIS CAN HELP IDENTIFY
!                        SPECIFIC LABORATORIES.
!
      NLOW1=N-NLOW1+1
      IF(NLOW2.GT.0)NLOW2=N-NLOW2+1
      CALL SORTC3(YTEMP,LABTMP,N,Y,LABID)
      ICNT=0
      DO 7101 I=N,1,-1
        ICNT=ICNT+1
        YTEMP(ICNT)=Y(I)
        LABTMP(ICNT)=LABID(I)
 7101 CONTINUE
      DO 7103 I=1,N
        Y(I)=YTEMP(I)
        LABID(I)=LABTMP(I)
 7103 CONTINUE
!
!     NOW CHECK FOR MULTIPLE OCCURENCES
!
      ICNT3=1
      YTEMP(ICNT3)=Y(1)
      LABTMP(ICNT3)=LABID(1)
      XTEMP2(ICNT3)=1.0
      TEMP1(ICNT3)=1.0
!
      IF(I1PTOC.EQ.'UNUS')THEN
        DO 77120 I=2,N
          IF(Y(I).EQ.Y(I-1))THEN
            IF(Y(I).GT.YINNUP .OR. Y(I).LT.YINNLO)THEN
              ICNT3=ICNT3+1
              YTEMP(ICNT3)=Y(I)
              LABTMP(ICNT3)=LABID(I)
              XTEMP2(ICNT3)=1.0
            ELSE
              XTEMP2(ICNT3)=XTEMP2(ICNT3)+1.0
            ENDIF
          ELSE
            ICNT3=ICNT3+1
            YTEMP(ICNT3)=Y(I)
            LABTMP(ICNT3)=LABID(I)
            XTEMP2(ICNT3)=1.0
          ENDIF
77120   CONTINUE
      ELSEIF(I1PTOC.EQ.'EXTR')THEN
        DO 77130 I=2,N
          IF(Y(I).EQ.Y(I-1))THEN
            IF(Y(I).GT.YOUTUP .OR. Y(I).LT.YOUTLO)THEN
              ICNT3=ICNT3+1
              YTEMP(ICNT3)=Y(I)
              LABTMP(ICNT3)=LABID(I)
              XTEMP2(ICNT3)=1.0
            ELSE
              XTEMP2(ICNT3)=XTEMP2(ICNT3)+1.0
            ENDIF
          ELSE
            ICNT3=ICNT3+1
            YTEMP(ICNT3)=Y(I)
            LABTMP(ICNT3)=LABID(I)
            XTEMP2(ICNT3)=1.0
          ENDIF
77130   CONTINUE
      ELSE
        DO 7120 I=2,N
          IF(Y(I).EQ.Y(I-1))THEN
            XTEMP2(ICNT3)=XTEMP2(ICNT3)+1.0
          ELSE
            ICNT3=ICNT3+1
            YTEMP(ICNT3)=Y(I)
            LABTMP(ICNT3)=LABID(I)
            XTEMP2(ICNT3)=1.0
          ENDIF
 7120   CONTINUE
      ENDIF
      TEMP1(1)=0.0
      DO 7122 I=2,ICNT3
        TEMP1(I)=TEMP1(I-1) + XTEMP2(I-1)
 7122 CONTINUE
!
      ITITLE(1:48)='Table 2: Test Results Sorted in Descending Order'
      NCTITL=48
      ITITL9=' '
      NCTIT9=0
!
      NUMCOL=5
      NUMLIN=2
!
      DO 1420 J=1,NUMCLI
        DO 1430 I=1,MAXLIN
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
          NCOLSP(I,J)=1
 1430   CONTINUE
 1420 CONTINUE
!
      ITITL2(1,1)='Count of'
      NCTIT2(1,1)=8
      NCOLSP(1,1)=1
      ITITL2(2,1)='Labs'
      NCTIT2(2,1)=4
      NCOLSP(2,1)=1
!
      ITITL2(2,2)='Lab'
      NCTIT2(2,2)=3
      NCOLSP(2,2)=1
!
      ITITL2(1,3)='Test'
      NCTIT2(1,3)=4
      ITITL2(2,3)='Results'
      NCTIT2(2,3)=7
!
      ITITL2(1,4)='Number of'
      NCTIT2(1,4)=9
      ITITL2(2,4)='Occurrences'
      NCTIT2(2,4)=11
!
      ITITL2(2,5)='Category'
      NCTIT2(2,5)=8
!
      IWHTML(1)=150
      IWHTML(2)=150
      IWHTML(3)=200
      IWHTML(4)=200
      IWHTML(5)=200
      IINC1=1400
      IINC2=1200
      IINC3=1700
      IINC4=1900
      IWRTF(1)=IINC3
      IWRTF(2)=IWRTF(1)+IINC2
      IWRTF(3)=IWRTF(2)+IINC3
      IWRTF(4)=IWRTF(3)+IINC3
      IWRTF(5)=IWRTF(4)+IINC4
      NMAX=0
      DO 1440 I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IF(I.EQ.1)NTOT(I)=18
        IF(I.EQ.2)NTOT(I)=10
        IF(I.EQ.5)NTOT(I)=21
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
        IF(I.EQ.1)ITYPCO(I)='ALPH'
        IF(I.EQ.5)ITYPCO(I)='ALPH'
 1440 CONTINUE
!
      ICNT=0
      ICNT2=0
      DO 1450 J=1,ICNT3
!
        NOCC=INT(XTEMP2(J)+0.1)
        ICUM=INT(TEMP1(J)+0.1)+1
        ICNT=ICNT+1
!
        IF(ICNT.GT.MAXROW)THEN
          ICNT=ICNT-1
          IF(ICAPTY.EQ.'LATE')THEN
            IFRST=.TRUE.
            ILAST=.TRUE.
            IFLAGS=.TRUE.
            IFLAGE=.TRUE.
          ELSE
            IFRST=.TRUE.
            IFLAGS=.TRUE.
            IF(ICNT2.GT.0)THEN
              IFRST=.FALSE.
              IFLAGS=.FALSE.
            ENDIF
            IFLAGE=.FALSE.
            ILAST=.FALSE.
            IF(J.EQ.ICNT3)THEN
              ILAST=.TRUE.
              IFLAGE=.TRUE.
            ENDIF
          ENDIF
          CALL DPDT5B(ITITLE,NCTITL,   &
                      ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                      MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                      IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                      IDIGI2,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                      NCOLSP,ROWSEP,   &
                      ICAPSW,ICAPTY,IFRST,ILAST,   &
                      IFLAGS,IFLAGE,   &
                      ISUBRO,IBUGA3,IERROR)
          ICNT=1
          ICNT2=ICNT2+1
        ENDIF
!
        DO 1455 I=1,NUMCOL
!
          IDIGI2(ICNT,I)=0
          IF(I.EQ.3)IDIGI2(ICNT,I)=NUMDIG
          IVALUE(ICNT,I)=' '
          NCVALU(ICNT,I)=0
          AMAT(ICNT,I)=0.0
!
 1455   CONTINUE
!
        AMAT(ICNT,2)=REAL(LABTMP(J))
        AMAT(ICNT,3)=YTEMP(J)
        AMAT(ICNT,4)=XTEMP2(J)
!
        IF(YTEMP(J).GT.YOUTUP)THEN
          IVALUE(ICNT,5)='Extremely Unusual'
          NCVALU(ICNT,5)=17
        ELSEIF(YTEMP(J).GT.YINNUP.AND.YTEMP(J).LE.YOUTUP)THEN
          IVALUE(ICNT,5)='Unusual'
          NCVALU(ICNT,5)=7
        ELSEIF(YTEMP(J).LT.YOUTLO)THEN
          IVALUE(ICNT,5)='Extremely Unusual'
          NCVALU(ICNT,5)=17
        ELSEIF(YTEMP(J).LT.YINNLO.AND.YTEMP(J).GE.YOUTLO)THEN
          IVALUE(ICNT,5)='Unusual'
          NCVALU(ICNT,5)=7
        ELSE
          IVALUE(ICNT,5)='Typical'
          NCVALU(ICNT,5)=7
        ENDIF
!
        IFLAG4=0
        IFLAG5=0
        IF(NMED1.GE.ICUM .AND. NMED1.LE.ICUM+NOCC-1)THEN
          IVALUE(ICNT,1)='    from Top'
          WRITE(IVALUE(ICNT,1)(1:3),'(I3)')NMED1
          NCVALU(ICNT,1)=12
          IF(NMED2.LT.0)THEN
            IFLAG4=1
            IFLAG5=1
          ELSE
            IFLAG5=1
          ENDIF
        ELSEIF(NMED2.GE.ICUM .AND. NMED2.LE.ICUM+NOCC-1)THEN
          IVALUE(ICNT,1)='    from Top'
          WRITE(IVALUE(ICNT,1)(1:3),'(I3)')NMED2
          NCVALU(ICNT,1)=12
        ELSEIF(NUPP1.GE.ICUM .AND. NUPP1.LE.ICUM+NOCC-1)THEN
          IVALUE(ICNT,1)='    from Top'
          WRITE(IVALUE(ICNT,1)(1:3),'(I3)')NUPP1
          NCVALU(ICNT,1)=12
          IF(NUPP2.LT.0)THEN
            IFLAG4=1
            IFLAG5=1
          ELSE
            IFLAG5=1
          ENDIF
        ELSEIF(NUPP2.GE.ICUM .AND. NUPP2.LE.ICUM+NOCC-1)THEN
          IVALUE(ICNT,1)='    from Top'
          WRITE(IVALUE(ICNT,1)(1:3),'(I3)')NUPP2
          NCVALU(ICNT,1)=12
        ELSEIF(NLOW1.GE.ICUM .AND. NLOW1.LE.ICUM+NOCC-1)THEN
          IVALUE(ICNT,1)='    from Bottom'
          WRITE(IVALUE(ICNT,1)(1:3),'(I3)')NUPP1
          NCVALU(ICNT,1)=15
          IF(NLOW2.LT.0)THEN
            IFLAG4=1
            IFLAG5=1
          ELSE
            IFLAG5=1
          ENDIF
        ELSEIF(NLOW2.GE.ICUM .AND. NLOW2.LE.ICUM+NOCC-1)THEN
          IVALUE(ICNT,1)='    from Bottom'
          WRITE(IVALUE(ICNT,1)(1:3),'(I3)')NUPP2
          NCVALU(ICNT,1)=15
        ENDIF
!
        ROWSEP(ICNT)=0
        IF(IFLAG4.EQ.1 .AND. IFLAG5.EQ.1)THEN
          ROWSEP(ICNT)=3
        ELSEIF(IFLAG4.EQ.1)THEN
          ROWSEP(ICNT)=2
        ELSEIF(IFLAG5.EQ.1)THEN
          ROWSEP(ICNT)=1
        ENDIF
        IF(J.EQ.ICNT3)ROWSEP(ICNT)=1
!
 1450 CONTINUE
!
      IF(ICNT.GT.0)THEN
        IFRST=.TRUE.
        ILAST=.TRUE.
        IF(ICAPTY.EQ.'LATE')THEN
          IFLAGS=.TRUE.
        ELSE
          IFLAGS=.TRUE.
          IF(ICNT2.GT.0)IFLAGS=.FALSE.
        ENDIF
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
      ENDIF
!
      IRTFMD='OFF'
      IFLAGA=.TRUE.
      IFLAGB=.FALSE.
      NTOTAL=40
      NBLNK1=1
      NBLNK2=0
      ITYPE=2
      ITTEMP='Median of Test Results:'
      NCTEMP=23
      CALL DPDTXT(ITTEMP,NCTEMP,YMED,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      IFLAGA=.FALSE.
      ITTEMP='Upper Hinge (Median of Top Half):'
      NCTEMP=33
      NBLNK1=0
      CALL DPDTXT(ITTEMP,NCTEMP,YUPPHI,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='Lower Hinge (Median of Bottom Half):'
      NCTEMP=36
      CALL DPDTXT(ITTEMP,NCTEMP,YLOWHI,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='Interquartile Range:'
      NCTEMP=20
      CALL DPDTXT(ITTEMP,NCTEMP,YIQR,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='3 X Interquartile Range:'
      NCTEMP=24
      NBLNK1=1
      ATEMP=3.0*YIQR
      CALL DPDTXT(ITTEMP,NCTEMP,ATEMP,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='Outer Fence (Upper):'
      NCTEMP=20
      NBLNK1=0
      CALL DPDTXT(ITTEMP,NCTEMP,YOUTUP,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='Outer Fence (Lower):'
      NCTEMP=20
      CALL DPDTXT(ITTEMP,NCTEMP,YOUTLO,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='1.5 X Interquartile Range:'
      NCTEMP=26
      NBLNK1=1
      ATEMP=1.5*YIQR
      CALL DPDTXT(ITTEMP,NCTEMP,ATEMP,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='Inner Fence (Upper):'
      NCTEMP=20
      NBLNK1=0
      CALL DPDTXT(ITTEMP,NCTEMP,YINNUP,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='Inner Fence (Lower):'
      NCTEMP=20
      NBLNK1=0
      CALL DPDTXT(ITTEMP,NCTEMP,YINNLO,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='Reproducibility Standard Deviation'
      NCTEMP=34
      NBLNK1=1
      CALL DPDTXT(ITTEMP,NCTEMP,CPUMIN,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='(IQR/1.35):'
      NCTEMP=11
      NBLNK1=0
      NBLNK2=1
      IFLAGB=.TRUE.
      CALL DPDTXT(ITTEMP,NCTEMP,REPSD,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
!
!     TABLE 3 - SORT BY DESCENDING TEST RESULT - BINNED DATA
!
      CLWID=CLWIDT(1)
      IF(CLWID.NE.CPUMIN .AND. CLWID.LE.0.0)GO TO 9000
      XSTART=CLLIMI(1)
      XSTOP=CLLIMI(2)
      IHSTO2='ON'
      CALL DPBIN(Y,N,IRELAT,CLWID,XSTART,XSTOP,IRHSTG,   &
                 TEMP1,MAXOBV,IHSTCW,IHSTO2,   &
                 YTEMP2,XTEMP2,NBINS,IBUGA3,IERROR)
      DELTA=XTEMP2(2) - XTEMP2(1)
!
      ITITLE(1:50)='Table 3: Binned Test Results Sorted in Descending '
      ITITLE(51:55)='Order'
      NCTITL=55
      ITITL9=' '
      NCTIT9=0
!
      NUMCOL=6
      NUMLIN=3
!
      DO 1520 J=1,NUMCLI
        DO 1530 I=1,MAXLIN
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
          NCOLSP(I,J)=1
 1530   CONTINUE
 1520 CONTINUE
!
      ITITL2(3,1)='Lab'
      NCTIT2(3,1)=3
!
      ITITL2(2,2)='Test'
      NCTIT2(2,2)=4
      ITITL2(3,2)='Results'
      NCTIT2(3,2)=7
!
      ITITL2(1,3)='Size Class Range'
      NCTIT2(1,3)=16
      NCOLSP(1,3)=3
      ITITL2(2,3)='Lower'
      NCTIT2(2,3)=5
      ITITL2(3,3)='End'
      NCTIT2(3,3)=3
      ITITL2(2,5)='Upper'
      NCTIT2(2,5)=5
      ITITL2(3,5)='End'
      NCTIT2(3,5)=3
!
      NCOLSP(1,4)=0
      NCOLSP(1,5)=0
!
      ITITL2(2,6)='Number of'
      NCTIT2(2,6)=9
      ITITL2(3,6)='Occurrences'
      NCTIT2(3,6)=11
!
      IWHTML(1)=100
      IWHTML(2)=150
      IWHTML(3)=150
      IWHTML(4)=75
      IWHTML(5)=150
      IWHTML(6)=200
      IINC1=1400
      IINC2=1200
      IINC3=1700
      IINC4=1900
      IWRTF(1)=IINC2
      IWRTF(2)=IWRTF(1)+IINC3
      IWRTF(3)=IWRTF(2)+IINC3
      IWRTF(4)=IWRTF(3)+IINC2
      IWRTF(5)=IWRTF(4)+IINC3
      IWRTF(6)=IWRTF(5)+IINC3
      NMAX=0
      ICNT=0
      ICNT2=0
      DO 1540 I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IF(I.EQ.4)NTOT(I)=8
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
        IF(I.EQ.4)ITYPCO(I)='ALPH'
 1540 CONTINUE
!
      DO 1550 K=NBINS,1,-1
!
        XL=XTEMP2(K) - (DELTA/2.0)
        XU=XTEMP2(K) + (DELTA/2.0)
        ICNT3=0
        DO 1560 J=1,N
!
          IF(Y(J).GE.XL .AND. Y(J).LT.XU)THEN
            AHOLD=Y(J)
            IHOLD=LABID(J)
            ICNT=ICNT+1
            ICNT3=ICNT3+1
          ELSE
            GO TO 1560
          ENDIF
!
          IF(ICNT.GT.MAXROW)THEN
            ICNT=ICNT-1
            ROWSEP(ICNT)=1
            IF(ICAPTY.EQ.'LATE')THEN
              IFRST=.TRUE.
              ILAST=.TRUE.
              IFLAGS=.TRUE.
              IFLAGE=.TRUE.
            ELSE
              IFRST=.TRUE.
              IFLAGS=.TRUE.
              IF(ICNT2.GT.0)THEN
                IFRST=.FALSE.
                IFLAGS=.FALSE.
              ENDIF
              IFLAGE=.FALSE.
              ILAST=.FALSE.
              IF(J.EQ.ICNT3)THEN
                ILAST=.TRUE.
                IFLAGE=.TRUE.
              ENDIF
            ENDIF
            CALL DPDT5B(ITITLE,NCTITL,   &
                        ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                        MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                        IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                        IDIGI2,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                        NCOLSP,ROWSEP,   &
                        ICAPSW,ICAPTY,IFRST,ILAST,   &
                        IFLAGS,IFLAGE,   &
                        ISUBRO,IBUGA3,IERROR)
            ICNT=1
            ICNT2=ICNT2+1
          ENDIF
!
          DO 1565 I=1,NUMCOL
!
            IDIGI2(ICNT,I)=NUMDIG
            IF(I.EQ.1 .OR. I.EQ.4 .OR. I.EQ.6)IDIGI2(ICNT,I)=0
            IVALUE(ICNT,I)=' '
            NCVALU(ICNT,I)=0
            AMAT(ICNT,I)=0.0
!
 1565     CONTINUE
!
          AMAT(ICNT,1)=REAL(IHOLD)
          AMAT(ICNT,2)=AHOLD
          AMAT(ICNT,3)=XL
          AMAT(ICNT,5)=XU
          AMAT(ICNT,6)=REAL(ICNT3)
          IVALUE(ICNT,4)='<= X <'
          NCVALU(ICNT,4)=6
!
          ROWSEP(ICNT)=0
 1560   CONTINUE
 1550 CONTINUE
!
      IF(ICNT.GT.0)THEN
        ROWSEP(ICNT)=1
        IFRST=.TRUE.
        ILAST=.TRUE.
        IF(ICAPTY.EQ.'LATE')THEN
          IFLAGS=.TRUE.
        ELSE
          IFLAGS=.TRUE.
          IF(ICNT2.GT.0)IFLAGS=.FALSE.
        ENDIF
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
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'1IP2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DP1IP2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR,IBUGA3
 9012   FORMAT('IERROR,IBUGA3 = ',A4,1X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)N
 9013   FORMAT('N = ',2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DP1IP2
      SUBROUTINE DP2DGR(X1,N1,X2,N2,IWRITE,Y1,Y2,TEMP,NOUT,MAXOBV,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE CREATES A "GRIDDED" SET OF VALUES FOR THE
!              2-DIMENSIONAL CASE.  THIS CAN ALSO BE DONE VIA TWO
!              SEQUENCE COMMANDS, BUT IS PROVIDED AS A CONVENIENCE.
!     INPUT  ARGUMENTS--X1     = THE SINGLE PRECISION VECTOR CONTAINING
!                                DESIRED VALUES FOR THE FIRST RESPONSE.
!                     --X2     = THE SINGLE PRECISION VECTOR CONTAINING
!                                DESIRED VALUES FOR THE SECOND RESPONSE.
!                     --N1     = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X1.
!                     --N2     = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X2.
!     OUTPUT ARGUMENTS--Y1     = THE SINGLE PRECISION VECTOR CONTAINING
!                                THE GRIDDED VALUES FOR X1.
!                     --Y2     = THE SINGLE PRECISION VECTOR CONTAINING
!                                THE GRIDDED VALUES FOR X2.
!                     --NOUT   = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTORS Y1 AND Y2.
!     OUTPUT--THE SINGLE PRECISION VECTORS Y1 AND Y2 CONTAINING
!             THE GRIDDED VECTORS.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATION INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATION INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2019.08
!     ORIGINAL VERSION--AUGUST    2019.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DIMENSION X1(*)
      DIMENSION X2(*)
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION TEMP(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DP2D'
      ISUBN2='GR  '
      IERROR='NO'
      IWRITE='OFF'
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'2DGR')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DP2DGR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N1,N2
   52   FORMAT('IBUGA3,ISUBRO,N1,N2 = ',2(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,MAX(N1,N2)
          WRITE(ICOUT,56)I,X1(I),X2(I)
   56     FORMAT('I,X1(I),X2(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!    ********************************************
!    **  STEP 1--                              **
!    **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!    ********************************************
!
      IF(N1.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN 2D GRID (DP2DGR)--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      THE NUMBER OF VALUES FOR THE FIRST RESPONSE ',   &
               'VARIABLE IS LESS THAN 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)N1
  115   FORMAT('      THE NUMBER OF RESPONSE VALUES IS ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(N2.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,118)
  118   FORMAT('      THE NUMBER OF VALUES FOR THE SECOND RESPONSE ',   &
               'VARIABLE IS LESS THAN 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)N2
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!     ********************************************************
!     **  STEP 2--                                          **
!     **  EXTRACT DISTINCT VALUES OF THE INPUT VARIABLES    **
!     **  AND THEN SORT.                                    **
!     ********************************************************
!
      CALL DISTIN(X1,N1,IWRITE,TEMP,N1DIST,IBUGA3,IERROR)
      CALL SORT(TEMP,N1DIST,X1)
!
      CALL DISTIN(X2,N2,IWRITE,TEMP,N2DIST,IBUGA3,IERROR)
      CALL SORT(TEMP,N2DIST,X2)
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'2DGR')THEN
        WRITE(ICOUT,1091)
 1091   FORMAT('AFTER DETERMINE DISTINCT VALUES OF INPUT VARIABLES')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1092)N1DIST,N2DIST
 1092   FORMAT('N1DIST,N2DIST = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO 1099 I=1,MAX(N1DIST,N2DIST)
          WRITE(ICOUT,1093)I,X1(I),X2(I)
 1093     FORMAT('I,X1(I),X2(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 1099   CONTINUE
      ENDIF
!
!     ****************************************************
!     **  STEP 3--                                      **
!     **  NOW CREATE THE GRIDDED VARIABLES              **
!     **  VALUES OF FIRST VARIABLE.                     **
!     ****************************************************
!
!
      NOUT=0
      DO 2110 I=1,N1DIST
        HOLD1=X1(I)
        DO 2120 J=1,N2DIST
          HOLD2=X2(J)
          IF(NOUT.GE.MAXOBV)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,111)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2118)
 2118       FORMAT('      THE NUMBER OF ALLOWED OUTPUT VALUES HAS ',   &
                   'BEEN EXCEEDED.')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
          NOUT=NOUT+1
          Y1(NOUT)=HOLD1
          Y2(NOUT)=HOLD2
 2120   CONTINUE
 2110 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'2DGR')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DP2DGR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IERROR,NOUT
 9013   FORMAT('IERROR,NOUT = ',A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,NOUT
          WRITE(ICOUT,9016)I,Y1(I),Y2(I)
 9016     FORMAT('I,Y1(I),Y2(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DP2DGR
      SUBROUTINE DP3DGR(X1,N1,X2,N2,X3,N3,IWRITE,Y1,Y2,Y3,TEMP,   &
                        NOUT,MAXOBV,ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE CREATES A "GRIDDED" SET OF VALUES FOR THE
!              3-DIMENSIONAL CASE.  THIS CAN ALSO BE DONE VIA SEVERAL
!              SEQUENCE COMMANDS, BUT IS PROVIDED AS A CONVENIENCE.
!     INPUT  ARGUMENTS--X1     = THE SINGLE PRECISION VECTOR CONTAINING
!                                DESIRED VALUES FOR THE FIRST RESPONSE.
!                     --X2     = THE SINGLE PRECISION VECTOR CONTAINING
!                                DESIRED VALUES FOR THE SECOND RESPONSE.
!                     --X3     = THE SINGLE PRECISION VECTOR CONTAINING
!                                DESIRED VALUES FOR THE THIRD RESPONSE.
!                     --N1     = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X1.
!                     --N2     = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X2.
!                     --N3     = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X3.
!     OUTPUT ARGUMENTS--Y1     = THE SINGLE PRECISION VECTOR CONTAINING
!                                THE GRIDDED VALUES FOR X1.
!                     --Y2     = THE SINGLE PRECISION VECTOR CONTAINING
!                                THE GRIDDED VALUES FOR X2.
!                     --Y3     = THE SINGLE PRECISION VECTOR CONTAINING
!                                THE GRIDDED VALUES FOR X3.
!                     --NOUT   = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTORS Y1, Y2 AND Y3.
!     OUTPUT--THE SINGLE PRECISION VECTORS Y1, Y2 AND Y3 CONTAINING
!             THE GRIDDED VECTORS.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATION INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATION INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2019.08
!     ORIGINAL VERSION--AUGUST    2019.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DIMENSION X1(*)
      DIMENSION X2(*)
      DIMENSION X3(*)
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION Y3(*)
      DIMENSION TEMP(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DP3D'
      ISUBN2='GR  '
      IERROR='NO'
      IWRITE='OFF'
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'3DGR')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DP3DGR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N1,N2,N3
   52   FORMAT('IBUGA3,ISUBRO,N1,N2,N3 = ',2(A4,2X),3I8)
        CALL DPWRST('XXX','BUG ')
        NTEMP=MAX(N1,N2)
        NTEMP=MAX(NTEMP,N3)
        DO 55 I=1,NTEMP
          WRITE(ICOUT,56)I,X1(I),X2(I),X3(I)
   56     FORMAT('I,X1(I),X2(I),X3(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!    ********************************************
!    **  STEP 1--                              **
!    **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!    ********************************************
!
      IF(N1.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN 3D GRID (DP3DGR)--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      THE NUMBER OF VALUES FOR THE FIRST RESPONSE ',   &
               'VARIABLE IS LESS THAN 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)N1
  115   FORMAT('      THE NUMBER OF RESPONSE VALUES IS ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(N2.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,118)
  118   FORMAT('      THE NUMBER OF VALUES FOR THE SECOND RESPONSE ',   &
               'VARIABLE IS LESS THAN 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)N2
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(N3.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,128)
  128   FORMAT('      THE NUMBER OF VALUES FOR THE THIRD RESPONSE ',   &
               'VARIABLE IS LESS THAN 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)N3
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!     ********************************************************
!     **  STEP 2--                                          **
!     **  EXTRACT DISTINCT VALUES OF THE INPUT VARIABLES    **
!     **  AND THEN SORT.                                    **
!     ********************************************************
!
      CALL DISTIN(X1,N1,IWRITE,TEMP,N1DIST,IBUGA3,IERROR)
      CALL SORT(TEMP,N1DIST,X1)
!
      CALL DISTIN(X2,N2,IWRITE,TEMP,N2DIST,IBUGA3,IERROR)
      CALL SORT(TEMP,N2DIST,X2)
!
      CALL DISTIN(X3,N3,IWRITE,TEMP,N3DIST,IBUGA3,IERROR)
      CALL SORT(TEMP,N3DIST,X3)
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'3DGR')THEN
        WRITE(ICOUT,1091)
 1091   FORMAT('AFTER DETERMINE DISTINCT VALUES OF INPUT VARIABLES')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1092)N1DIST,N2DIST,N3DIST
 1092   FORMAT('N1DIST,N2DIST,N3DIST = ',3I8)
        CALL DPWRST('XXX','BUG ')
        NTEMP=MAX(N1DIST,N2DIST)
        NTEMP=MAX(NTEMP,N3DIST)
        DO 1099 I=1,NTEMP
          WRITE(ICOUT,1093)I,X1(I),X2(I),X3(I)
 1093     FORMAT('I,X1(I),X2(I),X3(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
 1099   CONTINUE
      ENDIF
!
!     ****************************************************
!     **  STEP 3--                                      **
!     **  NOW CREATE THE GRIDDED VARIABLES              **
!     **  VALUES OF FIRST VARIABLE.                     **
!     ****************************************************
!
!
      NOUT=0
      DO 2110 I=1,N1DIST
        HOLD1=X1(I)
        DO 2120 J=1,N2DIST
          HOLD2=X2(J)
          DO 2130 K=1,N3DIST
            HOLD3=X3(K)
            IF(NOUT.GE.MAXOBV)THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,111)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2118)
 2118         FORMAT('      THE NUMBER OF ALLOWED OUTPUT VALUES HAS ',   &
                     'BEEN EXCEEDED.')
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GO TO 9000
            ENDIF
            NOUT=NOUT+1
            Y1(NOUT)=HOLD1
            Y2(NOUT)=HOLD2
            Y3(NOUT)=HOLD3
 2130     CONTINUE
 2120   CONTINUE
 2110 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'3DGR')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DP3DGR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IERROR,NOUT
 9013   FORMAT('IERROR,NOUT = ',A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,NOUT
          WRITE(ICOUT,9016)I,Y1(I),Y2(I),Y3(I)
 9016     FORMAT('I,Y1(I),Y2(I),Y3(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DP3DGR
      SUBROUTINE DP4DGR(X1,N1,X2,N2,X3,N3,X4,N4,IWRITE,Y1,Y2,Y3,Y4,   &
                        TEMP,NOUT,MAXOBV,ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE CREATES A "GRIDDED" SET OF VALUES FOR THE
!              3-DIMENSIONAL CASE.  THIS CAN ALSO BE DONE VIA SEVERAL
!              SEQUENCE COMMANDS, BUT IS PROVIDED AS A CONVENIENCE.
!     INPUT  ARGUMENTS--X1     = THE SINGLE PRECISION VECTOR CONTAINING
!                                DESIRED VALUES FOR THE FIRST RESPONSE.
!                     --X2     = THE SINGLE PRECISION VECTOR CONTAINING
!                                DESIRED VALUES FOR THE SECOND RESPONSE.
!                     --X3     = THE SINGLE PRECISION VECTOR CONTAINING
!                                DESIRED VALUES FOR THE THIRD RESPONSE.
!                     --X4     = THE SINGLE PRECISION VECTOR CONTAINING
!                                DESIRED VALUES FOR THE FOURTH RESPONSE.
!                     --N1     = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X1.
!                     --N2     = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X2.
!                     --N3     = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X3.
!                     --N4     = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X4.
!     OUTPUT ARGUMENTS--Y1     = THE SINGLE PRECISION VECTOR CONTAINING
!                                THE GRIDDED VALUES FOR X1.
!                     --Y2     = THE SINGLE PRECISION VECTOR CONTAINING
!                                THE GRIDDED VALUES FOR X2.
!                     --Y3     = THE SINGLE PRECISION VECTOR CONTAINING
!                                THE GRIDDED VALUES FOR X3.
!                     --Y4     = THE SINGLE PRECISION VECTOR CONTAINING
!                                THE GRIDDED VALUES FOR X4.
!                     --NOUT   = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTORS Y1, Y2, Y3 AND Y4.
!     OUTPUT--THE SINGLE PRECISION VECTORS Y1, Y2, Y3 AND Y4 CONTAINING
!             THE GRIDDED VECTORS.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATION INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATION INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2019/08
!     ORIGINAL VERSION--AUGUST    2019.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DIMENSION X1(*)
      DIMENSION X2(*)
      DIMENSION X3(*)
      DIMENSION X4(*)
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION Y3(*)
      DIMENSION Y4(*)
      DIMENSION TEMP(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DP4D'
      ISUBN2='GR  '
      IERROR='NO'
      IWRITE='OFF'
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'3DGR')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DP3DGR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N1,N2,N3,N4
   52   FORMAT('IBUGA3,ISUBRO,N1,N2,N3,N4 = ',2(A4,2X),4I8)
        CALL DPWRST('XXX','BUG ')
        NTEMP=MAX(N1,N2)
        NTEMP=MAX(NTEMP,N3)
        NTEMP=MAX(NTEMP,N4)
        DO 55 I=1,NTEMP
          WRITE(ICOUT,56)I,X1(I),X2(I),X3(I),X4(I)
   56     FORMAT('I,X1(I),X2(I),X3(I),X4(I) = ',I8,4G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!    ********************************************
!    **  STEP 1--                              **
!    **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!    ********************************************
!
      IF(N1.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN 4D GRID (DP4DGR)--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      THE NUMBER OF VALUES FOR THE FIRST RESPONSE ',   &
               'VARIABLE IS LESS THAN 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)N1
  115   FORMAT('      THE NUMBER OF RESPONSE VALUES IS ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(N2.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,118)
  118   FORMAT('      THE NUMBER OF VALUES FOR THE SECOND RESPONSE ',   &
               'VARIABLE IS LESS THAN 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)N2
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(N3.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,128)
  128   FORMAT('      THE NUMBER OF VALUES FOR THE THIRD RESPONSE ',   &
               'VARIABLE IS LESS THAN 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)N3
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(N4.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,138)
  138   FORMAT('      THE NUMBER OF VALUES FOR THE FOURTH RESPONSE ',   &
               'VARIABLE IS LESS THAN 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)N3
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!     ********************************************************
!     **  STEP 2--                                          **
!     **  EXTRACT DISTINCT VALUES OF THE INPUT VARIABLES    **
!     **  AND THEN SORT.                                    **
!     ********************************************************
!
      CALL DISTIN(X1,N1,IWRITE,TEMP,N1DIST,IBUGA3,IERROR)
      CALL SORT(TEMP,N1DIST,X1)
!
      CALL DISTIN(X2,N2,IWRITE,TEMP,N2DIST,IBUGA3,IERROR)
      CALL SORT(TEMP,N2DIST,X2)
!
      CALL DISTIN(X3,N3,IWRITE,TEMP,N3DIST,IBUGA3,IERROR)
      CALL SORT(TEMP,N3DIST,X3)
!
      CALL DISTIN(X4,N4,IWRITE,TEMP,N4DIST,IBUGA3,IERROR)
      CALL SORT(TEMP,N4DIST,X4)
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'3DGR')THEN
        WRITE(ICOUT,1091)
 1091   FORMAT('AFTER DETERMINE DISTINCT VALUES OF INPUT VARIABLES')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1092)N1DIST,N2DIST,N3DIST,N4DIST
 1092   FORMAT('N1DIST,N2DIST,N3DIST,N4DIST = ',4I8)
        CALL DPWRST('XXX','BUG ')
        NTEMP=MAX(N1DIST,N2DIST)
        NTEMP=MAX(NTEMP,N3DIST)
        NTEMP=MAX(NTEMP,N4DIST)
        DO 1099 I=1,NTEMP
          WRITE(ICOUT,1093)I,X1(I),X2(I),X3(I),X4(I)
 1093     FORMAT('I,X1(I),X2(I),X3(I),X4(I) = ',I8,4G15.7)
          CALL DPWRST('XXX','BUG ')
 1099   CONTINUE
      ENDIF
!
!     ****************************************************
!     **  STEP 3--                                      **
!     **  NOW CREATE THE GRIDDED VARIABLES              **
!     **  VALUES OF FIRST VARIABLE.                     **
!     ****************************************************
!
!
      NOUT=0
      DO 2110 I=1,N1DIST
        HOLD1=X1(I)
        DO 2120 J=1,N2DIST
          HOLD2=X2(J)
          DO 2130 K=1,N3DIST
            HOLD3=X3(K)
            DO 2140 L=1,N4DIST
              HOLD4=X4(L)
              IF(NOUT.GE.MAXOBV)THEN
                WRITE(ICOUT,999)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,111)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,2118)
 2118           FORMAT('      THE NUMBER OF ALLOWED OUTPUT VALUES HAS ',   &
                       'BEEN EXCEEDED.')
                CALL DPWRST('XXX','BUG ')
                IERROR='YES'
                GO TO 9000
              ENDIF
              NOUT=NOUT+1
              Y1(NOUT)=HOLD1
              Y2(NOUT)=HOLD2
              Y3(NOUT)=HOLD3
              Y4(NOUT)=HOLD4
 2140       CONTINUE
 2130     CONTINUE
 2120   CONTINUE
 2110 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'3DGR')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DP3DGR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IERROR,NOUT
 9013   FORMAT('IERROR,NOUT = ',A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,NOUT
          WRITE(ICOUT,9016)I,Y1(I),Y2(I),Y3(I),Y4(I)
 9016     FORMAT('I,Y1(I),Y2(I),Y3(I),Y4(J) = ',I8,4G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DP4DGR
      SUBROUTINE DP2IPT(ICAPSW,IFORSW,ISUBRO,IBUGA2,IBUGA3,IBUGQ,   &
                        IFOUND,IERROR)
!
!     PURPOSE--PERFORM A TWO-SAMPLE INTERLABORATORY PROFICIENCY
!              TESTING PROGRAM ACCORDING TO ASTM E 2489 - 06
!              STANDARD.
!
!              THE DATA CONSISTS OF DATA GROUPED BY LABS.  THE
!              COMMAND SYNTAX IS:
!
!                  TWO SAMPLE PROFICIENCY TEST  Y1  Y2  LABID
!
!              COMMAND IS TO GENERATE THE FOLLOWING TABLES:
!              1) TEST RESULTS SORTED BY LAB ID
!              2) TEST RESULTS SORTED BY TEST RESULTS IN DESCENDING
!                 ORDER, LABEL POINTS AS TYPICAL, UNUSUAL, ETC.
!                 (EACH LAB SEPARATELY)
!              3) RANDOM ERROR QUANTITIES IN DESCENDING ORDER
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2008/10
!     ORIGINAL VERSION--OCTOBER   2008.
!     UPDATED         --FEBRUARY  2011. USE DPPARS, DPPAR3
!     UPDATED         --APRIL     2014. SUPPORT VARIABLE LABELS
!                                       FOR THE TWO SAMPLES
!                                       (Y1LAB, Y2LAB)
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES----------------
!
      CHARACTER*4 ICAPSW
      CHARACTER*4 IFORSW
      CHARACTER*4 ISUBRO
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGQ
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      CHARACTER*4 ICASE
      CHARACTER*40 INAME
      PARAMETER (MAXSPN=20)
      CHARACTER*4 IVARN1(MAXSPN)
      CHARACTER*4 IVARN2(MAXSPN)
      CHARACTER*4 IVARTY(MAXSPN)
      REAL PVAR(MAXSPN)
      INTEGER ILIS(MAXSPN)
      INTEGER NRIGHT(MAXSPN)
      INTEGER ICOLR(MAXSPN)
!
      CHARACTER*14 Y1LAB
      CHARACTER*14 Y2LAB
!
!----------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZI.INC'
      INCLUDE 'DPCOZC.INC'
      PARAMETER (MAXROW=MAXOBV/20)
!
      DIMENSION Y1(MAXOBV)
      DIMENSION Y2(MAXOBV)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TEMP3(MAXOBV)
      DIMENSION TEMP4(MAXOBV)
      DIMENSION TEMP5(MAXOBV)
      DIMENSION TEMP6(MAXOBV)
      DIMENSION TEMP7(MAXOBV)
      DIMENSION AMAT(MAXOBV)
      INTEGER LABID(MAXOBV)
      INTEGER LABTMP(MAXOBV)
      INTEGER IINDX(MAXOBV)
      INTEGER LABCOD(MAXOBV)
      INTEGER IDIGI2(MAXOBV)
      INTEGER NCVALU(MAXOBV)
      INTEGER NCOLSP(MAXOBV)
      INTEGER NTOT(MAXOBV)
      INTEGER ROWSEP(MAXOBV)
      INTEGER NCTIT2(MAXOBV)
      CHARACTER*20 IVALZZ(MAXROW,7)
!
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),Y2(1))
      EQUIVALENCE (GARBAG(IGARB3),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB4),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB5),TEMP3(1))
      EQUIVALENCE (GARBAG(IGARB6),TEMP4(1))
      EQUIVALENCE (GARBAG(IGARB7),TEMP5(1))
      EQUIVALENCE (GARBAG(IGARB8),TEMP6(1))
      EQUIVALENCE (GARBAG(IGARB9),TEMP7(1))
      EQUIVALENCE (GARBAG(IGAR10),AMAT(1))
!
      EQUIVALENCE (IGARBG(IIGAR1),LABID(1))
      EQUIVALENCE (IGARBG(IIGAR2),LABTMP(1))
      EQUIVALENCE (IGARBG(IIGAR3),IINDX(1))
      EQUIVALENCE (IGARBG(IIGAR4),LABCOD(1))
      EQUIVALENCE (IGARBG(IIGAR5),IDIGI2(1))
      EQUIVALENCE (IGARBG(IIGAR6),NCVALU(1))
      EQUIVALENCE (IGARBG(IIGAR7),NCOLSP(1))
      EQUIVALENCE (IGARBG(IIGAR8),NTOT(1))
      EQUIVALENCE (IGARBG(IIGAR9),ROWSEP(1))
      EQUIVALENCE (IGARBG(IIGR10),NCTIT2(1))
!
      EQUIVALENCE (CGARBG(1),IVALZZ(1,1))
!
!-----COMMON----------------------------------------------------
!
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
!
!----------------------------------------------------------------
!
!-----COMMON VARIABLES (GENERAL)---------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT------------------------------------------------
!
      ISUBN1='DP2I'
      ISUBN2='PT  '
      IERROR='NO'
      IFOUND='YES'
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
!               **************************************************
!               **  TREAT THE TWO SAMPLE PROFICIENCY TEST CASE  **
!               **************************************************
!
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'2IPT')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DP2IPT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA2,IBUGA3,IBUGQ,ISUBRO
   52   FORMAT('IBUGA2,IBUGA3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *********************************
!               **  STEP 4--                   **
!               **  EXTRACT THE VARIABLE LIST  **
!               *********************************
!
      ISTEPN='4'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'2IPT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      INAME='TWO SAMPLE PROFICIENCY TEST'
      MINNA=3
      MAXNA=100
      MINN2=2
      IFLAGE=1
      IFLAGM=0
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=3
      MAXNVA=3
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'2IPT')THEN
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
                            ICOLR(I),PVAR(I)
  287       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',   &
                   'ICOLR(I),PVAR(I) = ',I8,2X,A4,A4,2X,3I8,G15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,288)IVARLB(ICOLR(I))
  288       FORMAT('IVARLB(ICOLR(I)) = ',A40)
            CALL DPWRST('XXX','BUG ')
  285     CONTINUE
        ENDIF
      ENDIF
!
      ICOL=1
      CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                  INAME,IVARN1,IVARN2,IVARTY,   &
                  ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,   &
                  MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                  MAXCP4,MAXCP5,MAXCP6,   &
                  V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                  Y1,Y2,TEMP1,NS,NLOCA2,NLOCA3,ICASE,   &
                  IBUGA3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
        DO 310 I=1,NS
          LABID(I)=INT(TEMP1(I)+0.1)
  310   CONTINUE
!
      Y1LAB=' '
      Y2LAB=' '
      IF(IVARLB(ICOLR(1)).EQ.' ')THEN
        Y1LAB(1:4)=IVARN1(1)(1:4)
        Y1LAB(5:8)=IVARN2(1)(1:4)
      ELSE
        Y1LAB(1:14)=IVARLB(ICOLR(1))(1:14)
      ENDIF
      IF(IVARLB(ICOLR(2)).EQ.' ')THEN
        Y2LAB(1:4)=IVARN1(2)(1:4)
        Y2LAB(5:8)=IVARN2(2)(1:4)
      ELSE
        Y2LAB(1:14)=IVARLB(ICOLR(2))(1:14)
      ENDIF
!
!               **************************************************
!               **  STEP 8--                                    **
!               **  PREPARE FOR ENTRANCE INTO DP1IP2--          **
!               **************************************************
!
      ISTEPN='8'
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'2IPT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!               ****************************************************
!               **  STEP 9--                                      **
!               **  CARRY OUT THE TWO SAMPLE PROFICIENCY TEST     **
!               ****************************************************
!
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'2IPT')THEN
        ISTEPN='9'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,711)
  711   FORMAT('***** FROM DP2IPT, AS WE ARE ABOUT TO CALL DP2IP2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,712)MAXN,NS,NUMVAR
  712   FORMAT('MAXN,NS,NUMVAR = ',3I8)
        CALL DPWRST('XXX','BUG ')
        DO 715 I=1,NS
          WRITE(ICOUT,716)I,Y1(I),Y2(I),LABID(I)
  716     FORMAT('I,Y1(I),Y2(I),LABID(I) = ',I6,2X,2G15.7,I8)
          CALL DPWRST('XXX','BUG ')
  715   CONTINUE
      ENDIF
!
      IWRITE='OFF'
      CALL DP2IP2(Y1,Y2,LABID,NS,Y1LAB,Y2LAB,   &
                  TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6,TEMP7,   &
                  LABTMP,IINDX,LABCOD,   &
                  AMAT,IDIGI2,NCVALU,NCOLSP,NTOT,ROWSEP,NCTIT2,   &
                  IVALZZ,   &
                  IWRITE,MAXOBV,   &
                  IFORSW,MAXROW,   &
                  Y1MED,Y1IQR,REPSD1,   &
                  Y2MED,Y2IQR,REPSD2,   &
                  REPSDR,REPEAT,POOLSD,   &
                  ICAPSW,ICAPTY,   &
                  ISUBRO,IBUGA3,IERROR)
!
!               ***************************************
!               **  STEP 10--                        **
!               **  UPDATE INTERNAL DATAPLOT TABLES  **
!               ***************************************
!
      ISTEPN='10'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'2IPT')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IH='MEDY'
      IH2='    '
      VALUE0=Y2MED
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA2,IERROR)
!
      IH='IQRY'
      IH2='    '
      VALUE0=Y2IQR
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA2,IERROR)
!
      IH='REPS'
      IH2='DY  '
      VALUE0=REPSD2
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA2,IERROR)
!
      IH='MEDX'
      IH2='    '
      VALUE0=Y1MED
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA2,IERROR)
!
      IH='IQRX'
      IH2='    '
      VALUE0=Y1IQR
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA2,IERROR)
!
      IH='REPS'
      IH2='DX  '
      VALUE0=REPSD1
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA2,IERROR)
!
      IH='REPS'
      IH2='DR  '
      VALUE0=REPSDR
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA2,IERROR)
!
      IH='REPE'
      IH2='ATSD'
      VALUE0=REPEAT
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA2,IERROR)
!
      IH='POOL'
      IH2='SD  '
      VALUE0=POOLSD
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA2,IERROR)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'2IPT')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DP2IPT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)IFOUND,IERROR
 9016   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DP2IPT
      SUBROUTINE DP2IP2(Y1,Y2,LABID,N,Y1LAB,Y2LAB,   &
                        RANERR,Y1SORT,Y2SORT,   &
                        TEMP1,TEMP2,TEMP3,TEMP4,   &
                        LABTMP,IINDX,LABCOD,   &
                        AMAT,IDIGI2,NCVALU,NCOLSP,NTOT,ROWSEP,NCTIT2,   &
                        IVALUE,   &
                        IWRITE,MAXOBV,   &
                        IFORSW,MAXROW,   &
                        Y1MED,Y1IQR,REPSD1,   &
                        Y2MED,Y2IQR,REPSD2,   &
                        REPSDR,REPEAT,POOLSD,   &
                        ICAPSW,ICAPTY,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--PERFORM A TWO-SAMPLE INTERLABORATORY PROFICIENCY
!              STUDY ACCORDING TO ASTM E 2489 - 06 STANDARD.
!
!              PROFICIENCY TESTING IS THE USE OF INTERLABORATORY
!              COMPARISONS FOR THE DETERMINATION OF LABORATORY
!              TESTING TESTING OR MEASUREMENT PERFORMANCE.
!
!              METHOD B COVERS TESTING PROGRAMS USING TWO
!              SAMPLES.  SAMPLES ARE ISSUED IN PAIRS AND EACH
!              LAB REPORTS A SINGLE RESULT FOR EACH SAMPLE.
!              SO THE DATA CONSISTS OF TWO RESPONSE
!              VARIABLES AND A LAB-ID VARIABLE.  THIS ROUTINE
!              GENERATES THE FOLLOWING FOUR TABLES:
!
!              1) TABLE 1: TESTS RESULTS SORTED BY LAB ID.
!
!              2) TABLE 2: RANDOM ERROR QUANTITIIES IN DESCENDING
!                 ORDER.
!
!              3) TABLE 3: FOR EACH INDIVIDUAL SAMPLE, SORT BY
!                 DESCENDING TEST REULT WITH TEST RESULTS
!                 CATEGORIZED.
!
!              4) TABLE 4: SUMMARIZE RESULTS.
!
!     PRINTING--YES
!     SUBROUTINES NEEDED--MEDIAN, LOWHIN, UPPHIN
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2008/10
!     ORIGINAL VERSION--OCTOBER   2008.
!     UPDATED         --JANUARY   2012. A FEW TWEAKS TO OUTPUT
!                                       USE DPDTA1 AND DPDT5B TO
!                                       PRINT TABLES
!     UPDATED         --JANUARY   2014. LATEX DOES NOT ALLOW MULTI-PAGE
!                                       TABLES, SO NEED TO INCLUDE
!                                       TRAILER/HEADER FOR TABLE FOR
!                                       EVERY CALL TO DPDT5B.  HOWEVER,
!                                       FOR OTHER FORMATS, WANT TO PRINT
!                                       AS SINGLE TABLE, SO NEED TO BE
!                                       MORE CAREFUL WITH THE LOGIC.
!     UPDATED         --APRIL     2014. SUPPORT VARIABLE LABELS
!                                       FOR THE TWO SAMPLES
!                                       (Y1LAB, Y2LAB)
!     UPDATED         --AUGUST    2014. FOR TABLE 4, USE ILATRS TO
!                                       RESIZE TABLE.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES--------------
!
      CHARACTER*14 Y1LAB
      CHARACTER*14 Y2LAB
!
      CHARACTER*4 IFORSW
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*1 IBASLC
      CHARACTER*4 ILATRZ
!
!----------------------------------------------------------------
!
      REAL Y1(*)
      REAL Y2(*)
      REAL Y1SORT(*)
      REAL Y2SORT(*)
      REAL RANERR(*)
      REAL TEMP1(*)
      REAL TEMP2(*)
      REAL TEMP3(*)
      REAL TEMP4(*)
      INTEGER LABID(*)
      INTEGER LABTMP(*)
      INTEGER LABCOD(*)
      INTEGER IINDX(*)
!
      PARAMETER(NUMCLI=7)
      PARAMETER(MAXLIN=4)
!CCCC PARAMETER(MAXROW=350)
      CHARACTER*65 ITITLE
      CHARACTER*60 ITITL9
      CHARACTER*4  ALIGN(NUMCLI)
      CHARACTER*4  VALIGN(NUMCLI)
      INTEGER      IDIGI2(MAXROW,NUMCLI)
      INTEGER      NTOT(MAXROW)
      INTEGER      ROWSEP(MAXROW)
      CHARACTER*20 ITITL2(MAXLIN,NUMCLI)
      CHARACTER*20 IVALUE(MAXROW,NUMCLI)
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
      INCLUDE 'DPCOST.INC'
!
      LOGICAL IFLAGA
      LOGICAL IFLAGB
      INTEGER IFLAG4
      INTEGER IFLAG5
!
      CHARACTER*70 ITTEMP
!
      CHARACTER*4 IRTFMD
      COMMON/COMRTF/IRTFMD
!
      COMMON/HTML44/IFNTSZ
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT------------------------------------------------
!
      IERROR='NO'
      ISUBN1='DP2I'
      ISUBN2='P2  '
!
      ICNT3=0
!
      NUMDIG=4
      IF(IFORSW.EQ.'1')NUMDIG=2
      IF(IFORSW.EQ.'2')NUMDIG=2
      IF(IFORSW.EQ.'3')NUMDIG=3
      IF(IFORSW.EQ.'4')NUMDIG=4
      IF(IFORSW.EQ.'5')NUMDIG=5
      IF(IFORSW.EQ.'6')NUMDIG=6
      IF(IFORSW.EQ.'7')NUMDIG=7
      IF(IFORSW.EQ.'8')NUMDIG=7
      IF(IFORSW.EQ.'9')NUMDIG=7
      IF(IFORSW.EQ.'0')NUMDIG=7
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
      DO 31 I=14,1,-1
        NCY1=I
        IF(Y1LAB(I:I).NE.' ')GO TO 39
   31 CONTINUE
   39 CONTINUE
!
      DO 41 I=14,1,-1
        NCY2=I
        IF(Y2LAB(I:I).NE.' ')GO TO 49
   41 CONTINUE
   49 CONTINUE
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'2IP2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DP2IP2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)N,NUMDIG
   52   FORMAT('N,NUMDIG = ',2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)Y1LAB,Y2LAB
   53   FORMAT('Y1LAB,Y2LAB = ',A14,2X,A14)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,Y1(I),Y2(I),LABID(I)
   56     FORMAT('I,Y(I),LABID(I) = ',I8,2G15.7,I8)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      IF(N.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR IN TWO-SAMPLE PROFICIENCY TEST--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,102)
  102   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE ',   &
               'TWO-SAMPLE PROFICIENCY TEST')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,103)N
  103   FORMAT('      MUST BE AT LEAST 2; THE ENTERED NUMBER OF ',   &
               'OBSERVATIONS = ',I8)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               ***********************************************
!               **  STEP 2.1--                               **
!               **  PERFORM THE BASIC CALCULATIONS.  OBTAIN: **
!               **  1) REPEATABILITY STANDARD DEVIATION      **
!               **  2) REPRODUCABILITY STANDARD DEVIATION    **
!               **  3) H CONSISTENCY STATISTIC               **
!               **  4) K CONSISTENCY STATISTIC               **
!               ***********************************************
!
      ISTEPN='2.1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'2IP2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     FIRST, SORT Y1, Y2, AND LABID BY LABID (DO NOT
!     ASSUME THAT DATA IS PRE-SORTED).
!
      DO 1001 I=1,N
        LABTMP(I)=I
 1001 CONTINUE
      CALL SORTC4(LABID,LABTMP,N,LABID,IINDX)
      DO 1002 I=1,N
        LABCOD(I)=IINDX(I)
 1002 CONTINUE
      DO 1003 I=1,N
        TEMP1(I)=Y1(I)
        TEMP2(I)=Y2(I)
 1003 CONTINUE
      DO 1005 I=1,N
        ITEMP=IINDX(I)
        Y1(ITEMP)=TEMP1(I)
        Y2(ITEMP)=TEMP2(I)
 1005 CONTINUE
!
      CALL MEDIAN(Y1,N,IWRITE,TEMP1,MAXOBV,Y1MED,IBUGA3,IERROR)
      CALL LOWHIN(Y1,N,IWRITE,TEMP1,MAXOBV,Y1LOWH,IBUGA3,IERROR)
      CALL UPPHIN(Y1,N,IWRITE,TEMP1,MAXOBV,Y1UPPH,IBUGA3,IERROR)
      Y1IQR=Y1UPPH - Y1LOWH
      ATEMP=3.0*Y1IQR
      Y1OUTU=Y1UPPH + ATEMP
      Y1OUTL=Y1LOWH - ATEMP
      ATEMP=1.5*Y1IQR
      Y1INNU=Y1UPPH + ATEMP
      Y1INNL=Y1LOWH - ATEMP
      REPSD1=Y1IQR/1.35
!
      CALL MEDIAN(Y2,N,IWRITE,TEMP1,MAXOBV,Y2MED,IBUGA3,IERROR)
      CALL LOWHIN(Y2,N,IWRITE,TEMP1,MAXOBV,Y2LOWH,IBUGA3,IERROR)
      CALL UPPHIN(Y2,N,IWRITE,TEMP1,MAXOBV,Y2UPPH,IBUGA3,IERROR)
      Y2IQR=Y2UPPH - Y2LOWH
      ATEMP=3.0*Y2IQR
      Y2OUTU=Y2UPPH + ATEMP
      Y2OUTL=Y2LOWH - ATEMP
      ATEMP=1.5*Y2IQR
      Y2INNU=Y2UPPH + ATEMP
      Y2INNL=Y2LOWH - ATEMP
      REPSD2=Y2IQR/1.35
!
      ATEMP=Y1MED - Y2MED
      DO 1010 I=1,N
        RANERR(I)=(Y1(I) - Y2(I)) - ATEMP
 1010 CONTINUE
!
      CALL MEDIAN(RANERR,N,IWRITE,TEMP1,MAXOBV,YRMED,IBUGA3,IERROR)
      CALL LOWHIN(RANERR,N,IWRITE,TEMP1,MAXOBV,YRLOWH,IBUGA3,IERROR)
      CALL UPPHIN(RANERR,N,IWRITE,TEMP1,MAXOBV,YRUPPH,IBUGA3,IERROR)
      YRIQR=YRUPPH - YRLOWH
      ATEMP=3.0*YRIQR
      YROUTU=YRUPPH + ATEMP
      YROUTL=YRLOWH - ATEMP
      ATEMP=1.5*YRIQR
      YRINNU=YRUPPH + ATEMP
      YRINNL=YRLOWH - ATEMP
      REPSDR=YRIQR/1.35
      REPEAT=REPSDR/SQRT(2.0)
!
      AN=REAL(N)
      ANUM=(AN-1.0)*REPSD1**2 + (AN-1.0)*REPSD2**2
      ADEN=AN + AN -2.0
      POOLSD=SQRT(ANUM/ADEN)
!
      IF(MOD(N,2).EQ.1)THEN
        NMED1=(N/2) + 1
        NMED2=-99
      ELSE
        NMED1=N/2
        NMED2=NMED1+1
      ENDIF
      IF(MOD(NMED1,2).EQ.1)THEN
        NUPP1=(NMED1/2)+1
        NUPP2=-99
      ELSE
        NUPP1=NMED1/2
        NUPP2=NUPP1+1
      ENDIF
      NLOW1=NUPP1
      NLOW2=NUPP2
      NLOW1=N-NLOW1+1
      IF(NLOW2.GT.0)NLOW2=N-NLOW2+1
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'2IP2')THEN
        WRITE(ICOUT,1091)NMED1,NMED2,NUPP1,NUPP2,NLOW1,NLOW2
 1091   FORMAT('NMED1,NMED2,NUPP1,NUPP2,NLOW1,NLOW2 = ',6I8)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1092)Y1MED,Y1LOWH,Y1UPPH
 1092   FORMAT('Y1MED,Y1LOWH,Y1UPPH = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1093)Y2MED,Y2LOWH,Y2UPPH
 1093   FORMAT('Y2MED,Y2LOWH,Y2UPPH = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      IF(IPRINT.NE.'ON')GO TO 9000
!
!     PRINT HEADER LINE
!
      IRTFMD='OFF'
      IFNTSZ=-1
      IFLAGA=.TRUE.
      IFLAGB=.TRUE.
      ISIZE=-1
      ITTEMP='E2489 - 06: Two-Sample Proficiency Analysis'
      NCTEMP=43
      NTOTAL=NCTEMP
      NBLNK1=1
      NBLNK2=1
      ITYPE=1
      CALL DPDTXT(ITTEMP,NCTEMP,CPUMIN,NUMDIG,   &
                  NTOTAL,NBLNK1,NBLNK2,IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,   &
                  ISUBRO,IBUGA3,IERROR)
      ISIZE=-99
      IFNTSZ=0
!
!     TABLE 1 - TEST RESULTS SORTED BY LAB-ID
!
      ITITLE(1:38)='Table 1: Test Results Sorted by Lab ID'
      NCTITL=38
      ITITL9=' '
      NCTIT9=0
!
      NUMCOL=3
      NUMLIN=2
!
      ITITL2(1,1)=' '
      NCTIT2(1,1)=0
      NCOLSP(1,1)=1
      ITITL2(2,1)='Lab'
      NCTIT2(2,1)=3
      NCOLSP(2,1)=1
!CCCC ITITL2(1,2)='Sample One'
!CCCC NCTIT2(1,2)=10
      ITITL2(1,2)(1:NCY1)=Y1LAB(1:NCY1)
      NCTIT2(1,2)=NCY1
      NCOLSP(1,2)=1
      ITITL2(2,2)='Test Results'
      NCTIT2(2,2)=12
      NCOLSP(2,2)=1
!CCCC ITITL2(1,3)='Sample Two'
!CCCC NCTIT2(1,3)=10
      ITITL2(1,3)(1:NCY2)=Y2LAB(1:NCY2)
      NCTIT2(1,3)=NCY2
      NCOLSP(1,3)=1
      ITITL2(2,3)='Test Results'
      NCTIT2(2,3)=12
      NCOLSP(2,3)=1
!
      IWHTML(1)=150
      IWHTML(2)=200
      IWHTML(3)=200
      IINC=1440
      IWRTF(1)=IINC
      IWRTF(2)=IWRTF(1)+IINC
      IWRTF(3)=IWRTF(2)+IINC
!
      NMAX=0
      ICNT=0
      ICNT2=0
      DO 1210 I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=18
        IF(I.EQ.1)NTOT(I)=15
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
 1210 CONTINUE
      DO 1213 J=1,N
!
        ICNT=ICNT+1
        IF(ICNT.GT.MAXROW)THEN
          ICNT=ICNT-1
          IF(ICAPTY.EQ.'LATE')THEN
            IFRST=.TRUE.
            ILAST=.TRUE.
            IFLAGS=.TRUE.
            IFLAGE=.TRUE.
          ELSE
            IFRST=.TRUE.
            IFLAGS=.TRUE.
            IF(ICNT2.GT.0)THEN
              IFRST=.FALSE.
              IFLAGS=.FALSE.
            ENDIF
            IFLAGE=.FALSE.
            ILAST=.FALSE.
            IF(J.EQ.ICNT3)THEN
              ILAST=.TRUE.
              IFLAGE=.TRUE.
            ENDIF
          ENDIF
          CALL DPDT5B(ITITLE,NCTITL,   &
                      ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                      MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                      IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                      IDIGI2,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                      NCOLSP,ROWSEP,   &
                      ICAPSW,ICAPTY,IFRST,ILAST,   &
                      IFLAGS,IFLAGE,   &
                      ISUBRO,IBUGA3,IERROR)
          ICNT=1
          ICNT2=ICNT2+1
        ENDIF
!
        DO 1215 I=1,NUMCOL
          IDIGI2(ICNT,I)=NUMDIG
          IF(I.EQ.1)IDIGI2(ICNT,I)=0
          IVALUE(ICNT,I)=' '
          NCVALU(ICNT,I)=0
 1215   CONTINUE
        AMAT(ICNT,1)=REAL(LABID(J))
        AMAT(ICNT,2)=Y1(J)
        AMAT(ICNT,3)=Y2(J)
        ROWSEP(ICNT)=0
        IF(J.EQ.N)ROWSEP(ICNT)=1
 1213 CONTINUE
!
      IF(ICNT.GT.1)THEN
        IFRST=.TRUE.
        ILAST=.TRUE.
        IF(ICAPTY.EQ.'LATE')THEN
          IFLAGS=.TRUE.
        ELSE
          IFLAGS=.TRUE.
          IF(ICNT2.GT.0)IFLAGS=.FALSE.
        ENDIF
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
      ENDIF
!
!     TABLE 2 - SORT IN DESCENDING ORDER FOR RANDOM ERROR
!
      CALL SORTC3(RANERR,LABCOD,N,TEMP4,IINDX)
      ICNT=0
      DO 1310 I=N,1,-1
        ICNT=ICNT+1
        ITEMP=IINDX(I)
        TEMP3(ICNT)=TEMP4(I)
        Y1SORT(ICNT)=Y1(ITEMP)
        Y2SORT(ICNT)=Y2(ITEMP)
        LABTMP(ICNT)=LABID(ITEMP)
 1310 CONTINUE
      IFLAG=0
!
      ITITLE(1:52)=   &
       'Table 2: Random Error Quantities in Descending Order'
      NCTITL=52
      ITITL9=' '
      NCTIT9=0
!
      NUMCOL=6
      NUMLIN=3
!
      DO 1320 J=1,NUMCLI
        DO 1330 I=1,MAXLIN
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
          NCOLSP(I,J)=1
 1330   CONTINUE
 1320 CONTINUE
!
      ITITL2(2,1)='Count of'
      NCTIT2(2,1)=8
      NCOLSP(2,1)=1
      ITITL2(3,1)='Labs'
      NCTIT2(3,1)=4
      NCOLSP(3,1)=1
!
      ITITL2(3,2)='Lab'
      NCTIT2(3,2)=3
      NCOLSP(3,2)=1
!
!CCCC ITITL2(2,3)='Sample One'
!CCCC NCTIT2(2,3)=10
      ITITL2(2,3)(1:NCY1)=Y1LAB(1:NCY1)
      NCTIT2(2,3)=NCY1
      ITITL2(3,3)='Test Results'
      NCTIT2(3,3)=12
!
!CCCC ITITL2(2,4)='Sample Two'
!CCCC NCTIT2(2,4)=10
      ITITL2(2,4)(1:NCY2)=Y2LAB(1:NCY2)
      NCTIT2(2,4)=NCY2
      ITITL2(3,4)='Test Results'
      NCTIT2(3,4)=12
!
      ITITL2(1,5)='Random Error'
      NCTIT2(1,5)=12
      ITITL2(2,5)='Quantities'
      NCTIT2(2,5)=10
      ITITL2(3,5)='(X-Y)-(Xmed-Ymed)'
      NCTIT2(3,5)=17
!
      ITITL2(1,6)='Within-'
      NCTIT2(1,6)=7
      ITITL2(2,6)='Laboratory'
      NCTIT2(2,6)=10
      ITITL2(3,6)='Category'
      NCTIT2(3,6)=8
!
      IWHTML(1)=150
      IWHTML(2)=100
      IWHTML(3)=150
      IWHTML(4)=150
      IWHTML(5)=200
      IWHTML(6)=200
!
      IF(ICAPSW.EQ.'ON' .AND. ICAPTY.EQ.'RTF')THEN
        IPTSZ=16
        CALL DPCONA(92,IBASLC)
        WRITE(ICOUT,8199)IBASLC,IPTSZ
 8199   FORMAT(A1,'fs',I2)
        CALL DPWRST(ICOUT,'WRIT')
      ENDIF
!
      IINC=1400
      IINC1=1200
      IINC2=1600
      IINC3=1800
      IINC4=800
      IWRTF(1)=IINC2
      IWRTF(2)=IWRTF(1)+IINC1
      IWRTF(3)=IWRTF(2)+IINC2
      IWRTF(4)=IWRTF(3)+IINC2
      IWRTF(5)=IWRTF(4)+IINC3
      IWRTF(6)=IWRTF(5)+IINC3
!
      NMAX=0
      ICNT=0
      ICNT2=0
      DO 1340 I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        IF(I.EQ.1)ALIGN(I)='l'
        NTOT(I)=15
        IF(I.EQ.2)NTOT(I)=10
        IF(I.EQ.3)NTOT(I)=18
        IF(I.EQ.4)NTOT(I)=19
        IF(I.EQ.5)NTOT(I)=21
        IF(I.EQ.6)NTOT(I)=21
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
        IF(I.EQ.1)ITYPCO(I)='ALPH'
        IF(I.EQ.6)ITYPCO(I)='ALPH'
 1340 CONTINUE
!
      DO 1350 J=1,N
        ICNT=ICNT+1
        DO 1360 I=1,NUMCOL
!
          IF(ICNT.GT.MAXROW)THEN
            ICNT=ICNT+1
            IF(ICAPTY.EQ.'LATE')THEN
              IFRST=.TRUE.
              ILAST=.TRUE.
              IFLAGS=.TRUE.
              IFLAGE=.TRUE.
            ELSE
              IFRST=.TRUE.
              IFLAGS=.TRUE.
              IF(ICNT2.GT.0)THEN
                IFRST=.FALSE.
                IFLAGS=.FALSE.
              ENDIF
              IFLAGE=.FALSE.
              ILAST=.FALSE.
              IF(J.EQ.ICNT3)THEN
                ILAST=.TRUE.
                IFLAGE=.TRUE.
              ENDIF
            ENDIF
!
!           2014/08: FOR TABLE 4, SET LATEX RESIZE OPTION TO LET
!           LATEX AUTOMATICALLY RESIZE THE TEXT.
!
            ILATRZ=ILATRS
            ILATRS='ON'
!
            CALL DPDT5B(ITITLE,NCTITL,   &
                        ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                        MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                        IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                        IDIGI2,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                        NCOLSP,ROWSEP,   &
                        ICAPSW,ICAPTY,IFRST,ILAST,   &
                        IFLAGS,IFLAGE,   &
                        ISUBRO,IBUGA3,IERROR)
            ILATRS=ILATRZ
            ICNT=1
            ICNT2=ICNT2+1
          ENDIF
!
          IDIGI2(ICNT,I)=NUMDIG
          IF(I.LE.2 .OR. I.EQ.6)IDIGI2(ICNT,I)=0
          IVALUE(ICNT,I)=' '
          NCVALU(ICNT,I)=0
          AMAT(ICNT,I)=0.0
!
 1360   CONTINUE
!
        AMAT(ICNT,2)=REAL(LABTMP(J))
        AMAT(ICNT,3)=Y1SORT(J)
        AMAT(ICNT,4)=Y2SORT(J)
        AMAT(ICNT,5)=TEMP3(J)
!
        IF(TEMP3(J).GT.YROUTU)THEN
          IVALUE(ICNT,6)='Extremely Unusual'
          NCVALU(ICNT,6)=17
        ELSEIF(TEMP3(J).GT.YRINNU.AND.TEMP3(J).LE.YROUTU)THEN
          IVALUE(ICNT,6)='Unusual'
          NCVALU(ICNT,6)=7
        ELSEIF(TEMP3(J).LT.YROUTL)THEN
          IVALUE(ICNT,6)='Extremely Unusual'
          NCVALU(ICNT,6)=17
        ELSEIF(TEMP3(J).LT.YRINNL.AND.TEMP3(J).GE.YROUTL)THEN
          IVALUE(ICNT,6)='Unusual'
          NCVALU(ICNT,6)=7
        ELSE
          IVALUE(ICNT,6)='Typical'
          NCVALU(ICNT,6)=7
        ENDIF
!
        IFLAG4=0
        IFLAG5=0
        IF(NMED1.EQ.J)THEN
          IVALUE(ICNT,1)='    from Top'
          WRITE(IVALUE(ICNT,1)(1:3),'(I3)')NMED1
          NCVALU(ICNT,1)=12
          IF(NMED2.LT.0)THEN
            IFLAG4=1
            IFLAG5=1
          ELSE
            IFLAG5=1
          ENDIF
        ELSEIF(NMED2.EQ.J)THEN
          IVALUE(ICNT,1)='    from Top'
          WRITE(IVALUE(ICNT,1)(1:3),'(I3)')NMED2
          NCVALU(ICNT,1)=12
        ELSEIF(NUPP1.EQ.J)THEN
          IVALUE(ICNT,1)='    from Top'
          WRITE(IVALUE(ICNT,1)(1:3),'(I3)')NUPP1
          NCVALU(ICNT,1)=12
          IF(NUPP2.LT.0)THEN
            IFLAG4=1
            IFLAG5=1
          ELSE
            IFLAG5=1
          ENDIF
        ELSEIF(NUPP2.EQ.J)THEN
          IVALUE(ICNT,1)='    from Top'
          WRITE(IVALUE(ICNT,1)(1:3),'(I3)')NUPP2
          NCVALU(ICNT,1)=12
        ELSEIF(NLOW1.EQ.J)THEN
          IVALUE(ICNT,1)='    from Bottom'
          WRITE(IVALUE(ICNT,1)(1:3),'(I3)')NUPP1
          NCVALU(ICNT,1)=15
          IF(NLOW2.LT.0)THEN
            IFLAG4=1
            IFLAG5=1
          ELSE
            IFLAG5=1
          ENDIF
        ELSEIF(NLOW2.EQ.J)THEN
          IVALUE(ICNT,1)='    from Bottom'
          WRITE(IVALUE(ICNT,1)(1:3),'(I3)')NUPP2
          NCVALU(ICNT,1)=15
        ENDIF
!
        ROWSEP(ICNT)=0
        IF(IFLAG4.EQ.1 .AND. IFLAG5.EQ.1)THEN
          ROWSEP(ICNT)=3
        ELSEIF(IFLAG4.EQ.1)THEN
          ROWSEP(ICNT)=2
        ELSEIF(IFLAG5.EQ.1)THEN
          ROWSEP(ICNT)=1
        ENDIF
        IF(J.EQ.N)ROWSEP(ICNT)=1
!
 1350 CONTINUE
!
      IF(ICNT.GT.0)THEN
        IFRST=.TRUE.
        ILAST=.TRUE.
        IF(ICAPTY.EQ.'LATE')THEN
          IFLAGS=.TRUE.
        ELSE
          IFLAGS=.TRUE.
          IF(ICNT2.GT.0)IFLAGS=.FALSE.
        ENDIF
        IFLAGE=.TRUE.
!
!       2014/08: FOR TABLE 4, SET LATEX RESIZE OPTION TO LET
!       LATEX AUTOMATICALLY RESIZE THE TEXT.
!
        ILATRZ=ILATRS
        ILATRS='ON'
!
        CALL DPDT5B(ITITLE,NCTITL,   &
                    ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                    MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                    IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                    IDIGI2,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                    NCOLSP,ROWSEP,   &
                    ICAPSW,ICAPTY,IFRST,ILAST,   &
                    IFLAGS,IFLAGE,   &
                    ISUBRO,IBUGA3,IERROR)
        ILATRS=ILATRZ
      ENDIF
!
      IRTFMD='OFF'
      IFLAGA=.TRUE.
      IFLAGB=.FALSE.
      ITTEMP='Median for '
      ITTEMP(12:12+NCY1-1)=Y1LAB(1:NCY1)
      NCTEMP=12+NCY1
      ITTEMP(NCTEMP:NCTEMP+8)=' (Xmed): '
      NCTEMP=NCTEMP+8
      NTOTAL=40
      NBLNK1=1
      NBLNK2=0
      ITYPE=2
      CALL DPDTXT(ITTEMP,NCTEMP,Y1MED,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      IFLAGA=.FALSE.
!CCCC ITTEMP='Median for Sample Two (Ymed): '
      ITTEMP='Median for '
      ITTEMP(12:12+NCY2-1)=Y2LAB(1:NCY2)
      NCTEMP=12+NCY2
      ITTEMP(NCTEMP:NCTEMP+8)=' (Ymed): '
      NCTEMP=NCTEMP+8
      NBLNK1=0
      NBLNK2=1
      CALL DPDTXT(ITTEMP,NCTEMP,Y2MED,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='Random Error Quantities Statistics:'
      NCTEMP=35
      NBLNK1=0
      NBLNK2=0
      CALL DPDTXT(ITTEMP,NCTEMP,CPUMIN,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='Median:'
      NCTEMP=7
      NBLNK1=0
      NBLNK2=0
      CALL DPDTXT(ITTEMP,NCTEMP,YRMED,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='Upper Hinge (Median of Top Half):'
      NCTEMP=33
      CALL DPDTXT(ITTEMP,NCTEMP,YRUPPH,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='Lower Hinge (Median of Bottom Half):'
      NCTEMP=36
      CALL DPDTXT(ITTEMP,NCTEMP,YRLOWH,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='Interquartile Range:'
      NCTEMP=20
      CALL DPDTXT(ITTEMP,NCTEMP,YRIQR,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='3 X Interquartile Range:'
      NCTEMP=24
      NBLNK1=1
      ATEMP=3.0*YRIQR
      CALL DPDTXT(ITTEMP,NCTEMP,ATEMP,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='Outer Fence (Upper):'
      NCTEMP=20
      NBLNK1=0
      CALL DPDTXT(ITTEMP,NCTEMP,YROUTU,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='Outer Fence (Lower):'
      NCTEMP=20
      CALL DPDTXT(ITTEMP,NCTEMP,YROUTL,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='1.5 X Interquartile Range:'
      NCTEMP=26
      NBLNK1=1
      ATEMP=1.5*YRIQR
      CALL DPDTXT(ITTEMP,NCTEMP,ATEMP,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='Inner Fence (Upper):'
      NCTEMP=20
      NBLNK1=0
      CALL DPDTXT(ITTEMP,NCTEMP,YRINNU,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='Inner Fence (Lower):'
      NCTEMP=20
      NBLNK1=0
      CALL DPDTXT(ITTEMP,NCTEMP,YRINNL,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='Repeatability Standard Deviation'
      NCTEMP=32
      NBLNK1=1
      CALL DPDTXT(ITTEMP,NCTEMP,CPUMIN,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='(IQR/1.35)/SQRT(2):'
      NCTEMP=19
      NBLNK1=0
      NBLNK2=1
      IFLAGB=.TRUE.
      CALL DPDTXT(ITTEMP,NCTEMP,REPEAT,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
!
!     TABLE 3 - SORT IN DESCENDING ORDER FOR Y2
!
      CALL SORTC3(Y1,LABCOD,N,TEMP4,IINDX)
      ICNT=0
      DO 1410 I=N,1,-1
        ICNT=ICNT+1
        ITEMP=IINDX(I)
        Y1SORT(ICNT)=TEMP4(I)
        Y2SORT(ICNT)=Y2(ITEMP)
        LABTMP(ICNT)=LABID(ITEMP)
 1410 CONTINUE
!
      ITITLE(1:49)='Table 3a: Test Results Sorted in Descending Order'
!CCCC ITITLE(50:64)=' for Sample One'
!CCCC NCTITL=64
      ITITLE(50:54)=' for '
      ITITLE(55:55+NCY1-1)=Y1LAB(1:NCY1)
      NCTITL=55+NCY1-1
      ITITL9=' '
      NCTIT9=0
!
      NUMCOL=6
      NUMLIN=3
!
      DO 1420 J=1,NUMCLI
        DO 1430 I=1,MAXLIN
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
          NCOLSP(I,J)=1
 1430   CONTINUE
 1420 CONTINUE
!
      ITITL2(2,1)='Count of'
      NCTIT2(2,1)=8
      NCOLSP(2,1)=1
      ITITL2(3,1)='Labs'
      NCTIT2(3,1)=4
      NCOLSP(3,1)=1
!
      ITITL2(3,2)='Lab'
      NCTIT2(3,2)=3
      NCOLSP(3,2)=1
!
!CCCC ITITL2(2,3)='Sample One'
!CCCC NCTIT2(2,3)=10
      ITITL2(2,3)(1:NCY1)=Y1LAB(1:NCY1)
      NCTIT2(2,3)=NCY1
      ITITL2(3,3)='Test Results'
      NCTIT2(3,3)=12
!
      ITITL2(1,4)='Between-Laboratory'
      NCTIT2(1,4)=18
      ITITL2(2,4)='Category'
      NCTIT2(2,4)=8
!CCCC ITITL2(3,4)='for Sample One'
!CCCC NCTIT2(3,4)=14
      ITITL2(3,4)(1:NCY1)=Y1LAB(1:NCY1)
      NCTIT2(3,4)=NCY1
!
!CCCC ITITL2(2,5)='Sample Two'
!CCCC NCTIT2(2,5)=10
      ITITL2(2,5)(1:NCY2)=Y2LAB(1:NCY2)
      NCTIT2(2,5)=NCY2
      ITITL2(3,5)='Test Results'
      NCTIT2(3,5)=12
!
      ITITL2(1,6)='Between-Laboratory'
      NCTIT2(1,6)=18
      ITITL2(2,6)='Category'
      NCTIT2(2,6)=8
!CCCC ITITL2(3,6)='for Sample Two'
!CCCC NCTIT2(3,6)=14
      ITITL2(3,6)(1:NCY2)=Y2LAB(1:NCY2)
      NCTIT2(3,6)=NCY2
!
      IWHTML(1)=150
      IWHTML(2)=100
      IWHTML(3)=150
      IWHTML(4)=200
      IWHTML(5)=150
      IWHTML(6)=200
      IFNTSZ=-1
!
      IINC=1400
      IINC1=1200
      IINC2=1700
      IINC3=1900
      IWRTF(1)=IINC2
      IWRTF(2)=IWRTF(1)+IINC1
      IWRTF(3)=IWRTF(2)+IINC2
      IWRTF(4)=IWRTF(3)+IINC3
      IWRTF(5)=IWRTF(4)+IINC2
      IWRTF(6)=IWRTF(5)+IINC3
      NMAX=0
      ICNT=0
      ICNT2=0
      DO 1440 I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        IF(I.EQ.1)ALIGN(I)='l'
        NTOT(I)=15
        IF(I.EQ.2)NTOT(I)=10
        IF(I.EQ.4)NTOT(I)=21
        IF(I.EQ.6)NTOT(I)=21
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
        IF(I.EQ.1)ITYPCO(I)='ALPH'
        IF(I.EQ.4)ITYPCO(I)='ALPH'
        IF(I.EQ.6)ITYPCO(I)='ALPH'
 1440 CONTINUE
!
      IFLAG=0
      DO 1450 J=1,N
        ICNT=ICNT+1
        DO 1455 I=1,NUMCOL
!
          IF(ICNT.GT.MAXROW)THEN
            ICNT=ICNT-1
            IF(ICAPTY.EQ.'LATE')THEN
              IFRST=.TRUE.
              ILAST=.TRUE.
              IFLAGS=.TRUE.
              IFLAGE=.TRUE.
            ELSE
              IFRST=.TRUE.
              IFLAGS=.TRUE.
              IF(ICNT2.GT.0)THEN
                IFRST=.FALSE.
                IFLAGS=.FALSE.
              ENDIF
              IFLAGE=.FALSE.
              ILAST=.FALSE.
              IF(J.EQ.ICNT3)THEN
                ILAST=.TRUE.
                IFLAGE=.TRUE.
              ENDIF
            ENDIF
!
!           2014/08: FOR TABLE 4, SET LATEX RESIZE OPTION TO LET
!           LATEX AUTOMATICALLY RESIZE THE TEXT.
!
            ILATRZ=ILATRS
            ILATRS='ON'
!
            CALL DPDT5B(ITITLE,NCTITL,   &
                        ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                        MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                        IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                        IDIGI2,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                        NCOLSP,ROWSEP,   &
                        ICAPSW,ICAPTY,IFRST,ILAST,   &
                        IFLAGS,IFLAGE,   &
                        ISUBRO,IBUGA3,IERROR)
            ILATRS=ILATRZ
            ICNT=1
            ICNT2=ICNT2+1
          ENDIF
!
          IDIGI2(ICNT,I)=NUMDIG
          IF(I.LE.2 .OR. I.EQ.4 .OR. I.EQ.6)IDIGI2(ICNT,I)=0
          IVALUE(ICNT,I)=' '
          NCVALU(ICNT,I)=0
          AMAT(ICNT,I)=0.0
!
 1455   CONTINUE
!
        AMAT(ICNT,2)=REAL(LABTMP(J))
        AMAT(ICNT,3)=Y1SORT(J)
        AMAT(ICNT,5)=Y2SORT(J)
!
        IF(Y1SORT(J).GT.Y1OUTU)THEN
          IVALUE(ICNT,4)='Extremely Unusual'
          NCVALU(ICNT,4)=17
        ELSEIF(Y1SORT(J).GT.Y1INNU.AND.Y1SORT(J).LE.Y1OUTU)THEN
          IVALUE(ICNT,4)='Unusual'
          NCVALU(ICNT,4)=7
        ELSEIF(Y1SORT(J).LT.Y1OUTL)THEN
          IVALUE(ICNT,4)='Extremely Unusual'
          NCVALU(ICNT,4)=17
        ELSEIF(Y1SORT(J).LT.Y1INNL.AND.Y1SORT(J).GE.Y1OUTL)THEN
          IVALUE(ICNT,4)='Unusual'
          NCVALU(ICNT,4)=7
        ELSE
          IVALUE(ICNT,4)='Typical'
          NCVALU(ICNT,4)=7
        ENDIF
!
        IF(Y2SORT(J).GT.Y2OUTU)THEN
          IVALUE(ICNT,6)='Extremely Unusual'
          NCVALU(ICNT,6)=17
        ELSEIF(Y2SORT(J).GT.Y2INNU.AND.Y2SORT(J).LE.Y2OUTU)THEN
          IVALUE(ICNT,6)='Unusual'
          NCVALU(ICNT,6)=7
        ELSEIF(Y2SORT(J).LT.Y2OUTL)THEN
          IVALUE(ICNT,6)='Extremely Unusual'
          NCVALU(ICNT,6)=17
        ELSEIF(Y2SORT(J).LT.Y2INNL.AND.Y2SORT(J).GE.Y2OUTL)THEN
          IVALUE(ICNT,6)='Unusual'
          NCVALU(ICNT,6)=7
        ELSE
          IVALUE(ICNT,6)='Typical'
          NCVALU(ICNT,6)=7
        ENDIF
!
        IFLAG4=0
        IFLAG5=0
        IF(NMED1.EQ.J)THEN
          IVALUE(ICNT,1)='    from Top'
          WRITE(IVALUE(ICNT,1)(1:3),'(I3)')NMED1
          NCVALU(ICNT,1)=12
          IF(NMED2.LT.0)THEN
            IFLAG4=1
            IFLAG5=1
          ELSE
            IFLAG5=1
          ENDIF
        ELSEIF(NMED2.EQ.J)THEN
          IVALUE(ICNT,1)='    from Top'
          WRITE(IVALUE(ICNT,1)(1:3),'(I3)')NMED2
          NCVALU(ICNT,1)=12
        ELSEIF(NUPP1.EQ.J)THEN
          IVALUE(ICNT,1)='    from Top'
          WRITE(IVALUE(ICNT,1)(1:3),'(I3)')NUPP1
          NCVALU(ICNT,1)=12
          IF(NUPP2.LT.0)THEN
            IFLAG4=1
            IFLAG5=1
          ELSE
            IFLAG5=1
          ENDIF
        ELSEIF(NUPP2.EQ.J)THEN
          IVALUE(ICNT,1)='    from Top'
          WRITE(IVALUE(ICNT,1)(1:3),'(I3)')NUPP2
          NCVALU(ICNT,1)=12
        ELSEIF(NLOW1.EQ.J)THEN
          IVALUE(ICNT,1)='    from Bottom'
          WRITE(IVALUE(ICNT,1)(1:3),'(I3)')NUPP1
          NCVALU(ICNT,1)=15
          IF(NLOW2.LT.0)THEN
            IFLAG4=1
            IFLAG5=1
          ELSE
            IFLAG5=1
          ENDIF
        ELSEIF(NLOW2.EQ.J)THEN
          IVALUE(ICNT,1)='    from Bottom'
          WRITE(IVALUE(ICNT,1)(1:3),'(I3)')NUPP2
          NCVALU(ICNT,1)=15
        ENDIF
!
        ROWSEP(ICNT)=0
        IF(IFLAG4.EQ.1 .AND. IFLAG5.EQ.1)THEN
          ROWSEP(ICNT)=3
        ELSEIF(IFLAG4.EQ.1)THEN
          ROWSEP(ICNT)=2
        ELSEIF(IFLAG5.EQ.1)THEN
          ROWSEP(ICNT)=1
        ENDIF
        IF(J.EQ.N)ROWSEP(ICNT)=1
!
 1450 CONTINUE
!
      IF(ICNT.GT.0)THEN
        IFRST=.TRUE.
        ILAST=.TRUE.
        IF(ICAPTY.EQ.'LATE')THEN
          IFLAGS=.TRUE.
        ELSE
          IFLAGS=.TRUE.
          IF(ICNT2.GT.0)IFLAGS=.FALSE.
        ENDIF
        IFLAGE=.TRUE.
!
!       2014/08: FOR TABLE 4, SET LATEX RESIZE OPTION TO LET
!       LATEX AUTOMATICALLY RESIZE THE TEXT.
!
        ILATRZ=ILATRS
        ILATRS='ON'
!
        CALL DPDT5B(ITITLE,NCTITL,   &
                    ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                    MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                    IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                    IDIGI2,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                    NCOLSP,ROWSEP,   &
                    ICAPSW,ICAPTY,IFRST,ILAST,   &
                    IFLAGS,IFLAGE,   &
                    ISUBRO,IBUGA3,IERROR)
        ILATRS=ILATRZ
      ENDIF
!
      IRTFMD='OFF'
      IFLAGA=.TRUE.
      IFLAGB=.FALSE.
!CCCC ITTEMP='Sample One Statistics:'
      ITTEMP(1:NCY1)=Y1LAB(1:NCY1)
      NCTEMP=NCY1+1
      ITTEMP(NCTEMP:NCTEMP+11)=' Statistics:'
      NCTEMP=NCTEMP+11
      NTOTAL=40
      NBLNK1=1
      NBLNK2=0
      ITYPE=2
      CALL DPDTXT(ITTEMP,NCTEMP,CPUMIN,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      IFLAGA=.FALSE.
      ITTEMP='Median of Test Results:'
      NCTEMP=23
      NBLNK1=0
      CALL DPDTXT(ITTEMP,NCTEMP,Y1MED,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='Upper Hinge (Median of Top Half):'
      NCTEMP=33
      NBLNK1=0
      CALL DPDTXT(ITTEMP,NCTEMP,Y1UPPH,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='Lower Hinge (Median of Bottom Half):'
      NCTEMP=36
      CALL DPDTXT(ITTEMP,NCTEMP,Y1LOWH,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='Interquartile Range:'
      NCTEMP=20
      CALL DPDTXT(ITTEMP,NCTEMP,Y1IQR,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='3 X Interquartile Range:'
      NCTEMP=24
      NBLNK1=1
      ATEMP=3.0*Y1IQR
      CALL DPDTXT(ITTEMP,NCTEMP,ATEMP,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='Outer Fence (Upper):'
      NCTEMP=20
      NBLNK1=0
      CALL DPDTXT(ITTEMP,NCTEMP,Y1OUTU,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='Outer Fence (Lower):'
      NCTEMP=20
      CALL DPDTXT(ITTEMP,NCTEMP,Y1OUTL,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='1.5 X Interquartile Range:'
      NCTEMP=26
      NBLNK1=1
      ATEMP=1.5*Y1IQR
      CALL DPDTXT(ITTEMP,NCTEMP,ATEMP,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='Inner Fence (Upper):'
      NCTEMP=20
      NBLNK1=0
      CALL DPDTXT(ITTEMP,NCTEMP,Y1INNU,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='Inner Fence (Lower):'
      NCTEMP=20
      NBLNK1=0
      CALL DPDTXT(ITTEMP,NCTEMP,Y1INNL,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='Reproducibility Standard Deviation'
      NCTEMP=34
      NBLNK1=1
      CALL DPDTXT(ITTEMP,NCTEMP,CPUMIN,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='(IQR/1.35):'
      NCTEMP=11
      NBLNK1=0
      NBLNK2=1
      IFLAGB=.TRUE.
      ISIZE=-2
      CALL DPDTXT(ITTEMP,NCTEMP,REPSD1,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
!
!     TABLE 3B - SORT IN DESCENDING ORDER FOR Y1
!
      CALL SORTC3(Y2,LABCOD,N,TEMP4,IINDX)
      ICNT=0
      DO 1460 I=N,1,-1
        ICNT=ICNT+1
        ITEMP=IINDX(I)
        Y2SORT(ICNT)=TEMP4(I)
        Y1SORT(ICNT)=Y1(ITEMP)
        LABTMP(ICNT)=LABID(ITEMP)
 1460 CONTINUE
!
      ITITLE(1:49)='Table 3b: Test Results Sorted in Descending Order'
!CCCC ITITLE(50:64)=' for Sample Two'
!CCCC NCTITL=64
      ITITLE(50:54)=' for '
      ITITLE(55:55+NCY2-1)=Y2LAB(1:NCY2)
      NCTITL=55+NCY2-1
      ITITL9=' '
      ITITL9=' '
      NCTIT9=0
      ICNT=0
      ICNT2=0
!
      IFLAG=0
      DO 1470 J=1,N
        ICNT=ICNT+1
        DO 1480 I=1,NUMCOL
!
          IF(ICNT.GT.MAXROW)THEN
            ICNT=ICNT-1
            IF(ICAPTY.EQ.'LATE')THEN
              IFRST=.TRUE.
              ILAST=.TRUE.
              IFLAGS=.TRUE.
              IFLAGE=.TRUE.
            ELSE
              IFRST=.TRUE.
              IFLAGS=.TRUE.
              IF(ICNT2.GT.0)THEN
                IFRST=.FALSE.
                IFLAGS=.FALSE.
              ENDIF
              IFLAGE=.FALSE.
              ILAST=.FALSE.
              IF(J.EQ.ICNT3)THEN
                ILAST=.TRUE.
                IFLAGE=.TRUE.
              ENDIF
            ENDIF
!
!           2014/08: FOR TABLE 4, SET LATEX RESIZE OPTION TO LET
!           LATEX AUTOMATICALLY RESIZE THE TEXT.
!
            ILATRZ=ILATRS
            ILATRS='ON'
!
            CALL DPDT5B(ITITLE,NCTITL,   &
                        ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                        MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                        IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                        IDIGI2,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                        NCOLSP,ROWSEP,   &
                        ICAPSW,ICAPTY,IFRST,ILAST,   &
                        IFLAGS,IFLAGE,   &
                        ISUBRO,IBUGA3,IERROR)
            ILATRS=ILATRZ
            ICNT=1
            ICNT2=ICNT2+1
          ENDIF
!
          IDIGI2(ICNT,I)=NUMDIG
          IF(I.LE.2 .OR. I.EQ.4 .OR. I.EQ.6)IDIGI2(ICNT,I)=0
          IVALUE(ICNT,I)=' '
          NCVALU(ICNT,I)=0
          AMAT(ICNT,I)=0.0
!
 1480   CONTINUE
!
        AMAT(ICNT,2)=REAL(LABTMP(J))
        AMAT(ICNT,3)=Y1SORT(J)
        AMAT(ICNT,5)=Y2SORT(J)
!
        IF(Y1SORT(J).GT.Y1OUTU)THEN
          IVALUE(ICNT,4)='Extremely Unusual'
          NCVALU(ICNT,4)=17
        ELSEIF(Y1SORT(J).GT.Y1INNU.AND.Y1SORT(J).LE.Y1OUTU)THEN
          IVALUE(ICNT,4)='Unusual'
          NCVALU(ICNT,4)=7
        ELSEIF(Y1SORT(J).LT.Y1OUTL)THEN
          IVALUE(ICNT,4)='Extremely Unusual'
          NCVALU(ICNT,4)=17
        ELSEIF(Y1SORT(J).LT.Y1INNL.AND.Y1SORT(J).GE.Y1OUTL)THEN
          IVALUE(ICNT,4)='Unusual'
          NCVALU(ICNT,4)=7
        ELSE
          IVALUE(ICNT,4)='Typical'
          NCVALU(ICNT,4)=7
        ENDIF
!
        IF(Y2SORT(J).GT.Y2OUTU)THEN
          IVALUE(ICNT,6)='Extremely Unusual'
          NCVALU(ICNT,6)=17
        ELSEIF(Y2SORT(J).GT.Y2INNU.AND.Y2SORT(J).LE.Y2OUTU)THEN
          IVALUE(ICNT,6)='Unusual'
          NCVALU(ICNT,6)=7
        ELSEIF(Y2SORT(J).LT.Y2OUTL)THEN
          IVALUE(ICNT,6)='Extremely Unusual'
          NCVALU(ICNT,6)=17
        ELSEIF(Y2SORT(J).LT.Y2INNL.AND.Y2SORT(J).GE.Y2OUTL)THEN
          IVALUE(ICNT,6)='Unusual'
          NCVALU(ICNT,6)=7
        ELSE
          IVALUE(ICNT,6)='Typical'
          NCVALU(ICNT,6)=7
        ENDIF
!
        IFLAG4=0
        IFLAG5=0
        IF(NMED1.EQ.J)THEN
          IVALUE(ICNT,1)='    from Top'
          WRITE(IVALUE(ICNT,1)(1:3),'(I3)')NMED1
          NCVALU(ICNT,1)=12
          IF(NMED2.LT.0)THEN
            IFLAG4=1
            IFLAG5=1
          ELSE
            IFLAG5=1
          ENDIF
        ELSEIF(NMED2.EQ.J)THEN
          IVALUE(ICNT,1)='    from Top'
          WRITE(IVALUE(ICNT,1)(1:3),'(I3)')NMED2
          NCVALU(ICNT,1)=12
        ELSEIF(NUPP1.EQ.J)THEN
          IVALUE(ICNT,1)='    from Top'
          WRITE(IVALUE(ICNT,1)(1:3),'(I3)')NUPP1
          NCVALU(ICNT,1)=12
          IF(NUPP2.LT.0)THEN
            IFLAG4=1
            IFLAG5=1
          ELSE
            IFLAG5=1
          ENDIF
        ELSEIF(NUPP2.EQ.J)THEN
          IVALUE(ICNT,1)='    from Top'
          WRITE(IVALUE(ICNT,1)(1:3),'(I3)')NUPP2
          NCVALU(ICNT,1)=12
        ELSEIF(NLOW1.EQ.J)THEN
          IVALUE(ICNT,1)='    from Bottom'
          WRITE(IVALUE(ICNT,1)(1:3),'(I3)')NUPP1
          NCVALU(ICNT,1)=15
          IF(NLOW2.LT.0)THEN
            IFLAG4=1
            IFLAG5=1
          ELSE
            IFLAG5=1
          ENDIF
        ELSEIF(NLOW2.EQ.J)THEN
          IVALUE(ICNT,1)='    from Bottom'
          WRITE(IVALUE(ICNT,1)(1:3),'(I3)')NUPP2
          NCVALU(ICNT,1)=15
        ENDIF
!
        ROWSEP(ICNT)=0
        IF(IFLAG4.EQ.1 .AND. IFLAG5.EQ.1)THEN
          ROWSEP(ICNT)=3
        ELSEIF(IFLAG4.EQ.1)THEN
          ROWSEP(ICNT)=2
        ELSEIF(IFLAG5.EQ.1)THEN
          ROWSEP(ICNT)=1
        ENDIF
        IF(J.EQ.N)ROWSEP(ICNT)=1
!
 1470 CONTINUE
!
      IF(ICNT.GT.0)THEN
        IFRST=.TRUE.
        ILAST=.TRUE.
        IF(ICAPTY.EQ.'LATE')THEN
          IFLAGS=.TRUE.
        ELSE
          IFLAGS=.TRUE.
          IF(ICNT2.GT.0)IFLAGS=.FALSE.
        ENDIF
        IFLAGE=.TRUE.
!
!       2014/08: FOR TABLE 4, SET LATEX RESIZE OPTION TO LET
!       LATEX AUTOMATICALLY RESIZE THE TEXT.
!
        ILATRZ=ILATRS
        ILATRS='ON'
!
        CALL DPDT5B(ITITLE,NCTITL,   &
                    ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                    MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                    IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                    IDIGI2,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                    NCOLSP,ROWSEP,   &
                    ICAPSW,ICAPTY,IFRST,ILAST,   &
                    IFLAGS,IFLAGE,   &
                    ISUBRO,IBUGA3,IERROR)
        ILATRS=ILATRZ
      ENDIF
!
      IRTFMD='OFF'
      IFLAGA=.TRUE.
      IFLAGB=.FALSE.
!CCCC ITTEMP='Sample Two Statistics:'
!CCCC NCTEMP=22
      ITTEMP(1:NCY2)=Y2LAB(1:NCY2)
      NCTEMP=NCY2+1
      ITTEMP(NCTEMP:NCTEMP+11)=' Statistics:'
      NCTEMP=NCTEMP+11
      NTOTAL=40
      NBLNK1=1
      NBLNK2=0
      ITYPE=2
      CALL DPDTXT(ITTEMP,NCTEMP,CPUMIN,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      IFLAGA=.FALSE.
      ITTEMP='Median of Test Results:'
      NCTEMP=23
      NBLNK1=0
      CALL DPDTXT(ITTEMP,NCTEMP,Y2MED,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='Upper Hinge (Median of Top Half):'
      NCTEMP=33
      NBLNK1=0
      CALL DPDTXT(ITTEMP,NCTEMP,Y2UPPH,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='Lower Hinge (Median of Bottom Half):'
      NCTEMP=36
      CALL DPDTXT(ITTEMP,NCTEMP,Y2LOWH,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='Interquartile Range:'
      NCTEMP=20
      CALL DPDTXT(ITTEMP,NCTEMP,Y2IQR,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='3 X Interquartile Range:'
      NCTEMP=24
      NBLNK1=1
      ATEMP=3.0*Y2IQR
      CALL DPDTXT(ITTEMP,NCTEMP,ATEMP,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='Outer Fence (Upper):'
      NCTEMP=20
      NBLNK1=0
      CALL DPDTXT(ITTEMP,NCTEMP,Y2OUTU,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='Outer Fence (Lower):'
      NCTEMP=20
      CALL DPDTXT(ITTEMP,NCTEMP,Y2OUTL,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='1.5 X Interquartile Range:'
      NCTEMP=26
      NBLNK1=1
      ATEMP=1.5*Y2IQR
      CALL DPDTXT(ITTEMP,NCTEMP,ATEMP,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='Inner Fence (Upper):'
      NCTEMP=20
      NBLNK1=0
      CALL DPDTXT(ITTEMP,NCTEMP,Y2INNU,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='Inner Fence (Lower):'
      NCTEMP=20
      NBLNK1=0
      CALL DPDTXT(ITTEMP,NCTEMP,Y2INNL,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='Reproducibility Standard Deviation'
      NCTEMP=34
      NBLNK1=1
      CALL DPDTXT(ITTEMP,NCTEMP,CPUMIN,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='(IQR/1.35):'
      NCTEMP=11
      NBLNK1=0
      NBLNK2=1
      IFLAGB=.TRUE.
      CALL DPDTXT(ITTEMP,NCTEMP,REPSD2,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
!
!     TABLE 4 - SUMMARY OF RESULTS
!
!CCCC ITITLE='Table 4: Summary of Results for Samples One and Two'
!CCCC NCTITL=51
      ITITLE(1:32)='Table 4: Summary of Results for '
      NCTEMP=33
      ITITLE(NCTEMP:NCTEMP+NCY1-1)=Y1LAB(1:NCY1)
      NCTEMP=NCTEMP+NCY1
      ITITLE(NCTEMP:NCTEMP+4)=' and '
      NCTEMP=NCTEMP+5
      ITITLE(NCTEMP:NCTEMP+NCY2-1)=Y2LAB(1:NCY2)
      NCTEMP=NCTEMP+NCY2-1
      NCTITL=NCTEMP
      ITITL9=' '
      NCTIT9=0
!
      NUMCOL=7
      NUMLIN=4
!
      DO 1520 J=1,NUMCLI
        DO 1530 I=1,MAXLIN
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
          NCOLSP(I,J)=1
 1530   CONTINUE
 1520 CONTINUE
!
      ITITL2(4,1)='Lab'
      NCTIT2(4,1)=3
      NCOLSP(4,1)=1
!
!CCCC ITITL2(1,2)='Sample'
!CCCC NCTIT2(1,2)=6
!CCCC ITITL2(2,2)='One'
!CCCC NCTIT2(2,2)=3
      ITITL2(1,2)=' '
      NCTIT2(1,2)=0
      ITITL2(2,2)(1:NCY1)=Y1LAB(1:NCY1)
      NCTIT2(2,2)=NCY1
      ITITL2(3,2)='Test'
      NCTIT2(3,2)=4
      ITITL2(4,2)='Result'
      NCTIT2(4,2)=6
!
      ITITL2(1,3)='Between-'
      NCTIT2(1,3)=8
      ITITL2(2,3)='Laboratory'
      NCTIT2(2,3)=10
      ITITL2(3,3)='Category for'
      NCTIT2(3,3)=12
!CCCC ITITL2(4,3)='Sample One'
!CCCC NCTIT2(4,3)=10
      ITITL2(4,3)(1:NCY1)=Y1LAB(1:NCY1)
      NCTIT2(4,3)=NCY1
!
!CCCC ITITL2(1,4)='Sample'
!CCCC NCTIT2(1,4)=6
!CCCC ITITL2(2,4)='Two'
!CCCC NCTIT2(2,4)=3
      ITITL2(1,4)=' '
      NCTIT2(1,4)=0
      ITITL2(2,4)(1:NCY2)=Y2LAB(1:NCY2)
      NCTIT2(2,4)=NCY2
      ITITL2(3,4)='Test'
      NCTIT2(3,4)=4
      ITITL2(4,4)='Result'
      NCTIT2(4,4)=6
!
      ITITL2(1,5)='Between-'
      NCTIT2(1,5)=8
      ITITL2(2,5)='Laboratory'
      NCTIT2(2,5)=10
      ITITL2(3,5)='Category for'
      NCTIT2(3,5)=12
!CCCC ITITL2(4,5)='Sample Two'
!CCCC NCTIT2(4,5)=10
      ITITL2(4,5)(1:NCY2)=Y2LAB(1:NCY2)
      NCTIT2(4,5)=NCY2
!
      ITITL2(1,6)='Random Error'
      NCTIT2(1,6)=12
      ITITL2(2,6)='Quantities'
      NCTIT2(2,6)=10
      ITITL2(3,6)='(X-Y)-'
      NCTIT2(3,6)=6
      ITITL2(4,6)='(Xmed-Ymed)'
      NCTIT2(4,6)=11
!
      ITITL2(2,7)='Within-'
      NCTIT2(2,7)=7
      ITITL2(3,7)='Laboratory'
      NCTIT2(3,7)=10
      ITITL2(4,7)='Category'
      NCTIT2(4,7)=8
!
      IWHTML(1)=100
      IWHTML(2)=150
      IWHTML(3)=200
      IWHTML(4)=150
      IWHTML(5)=200
      IWHTML(6)=150
      IWHTML(7)=200
      IINC=800
      IINC1=1200
      IINC2=1600
      IINC3=1800
      IWRTF(1)=IINC
      IWRTF(2)=IWRTF(1)+IINC1
      IWRTF(3)=IWRTF(2)+IINC3
      IWRTF(4)=IWRTF(3)+IINC1
      IWRTF(5)=IWRTF(4)+IINC3
      IWRTF(6)=IWRTF(5)+IINC2
      IWRTF(7)=IWRTF(6)+IINC3
!
      NMAX=0
      ICNT=0
      ICNT2=0
      DO 1540 I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IF(I.EQ.1)NTOT(I)=10
        IF(I.EQ.3)NTOT(I)=21
        IF(I.EQ.5)NTOT(I)=21
        IF(I.EQ.6)NTOT(I)=19
        IF(I.EQ.7)NTOT(I)=21
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
        IF(I.EQ.3)ITYPCO(I)='ALPH'
        IF(I.EQ.5)ITYPCO(I)='ALPH'
        IF(I.EQ.7)ITYPCO(I)='ALPH'
 1540 CONTINUE
!
      DO 1550 J=1,N
        ICNT=ICNT+1
        ROWSEP(ICNT)=0
        IF(J.EQ.N)ROWSEP(ICNT)=1
        DO 1560 I=1,NUMCOL
!
          IF(ICNT.GT.MAXROW)THEN
            ICNT=ICNT-1
            IF(ICAPTY.EQ.'LATE')THEN
              IFRST=.TRUE.
              ILAST=.TRUE.
              IFLAGS=.TRUE.
              IFLAGE=.TRUE.
            ELSE
              IFRST=.TRUE.
              IFLAGS=.TRUE.
              IF(ICNT2.GT.0)THEN
                IFRST=.FALSE.
                IFLAGS=.FALSE.
              ENDIF
              IFLAGE=.FALSE.
              ILAST=.FALSE.
              IF(J.EQ.ICNT3)THEN
                ILAST=.TRUE.
                IFLAGE=.TRUE.
              ENDIF
            ENDIF
!
!           2014/08: FOR TABLE 4, SET LATEX RESIZE OPTION TO LET
!           LATEX AUTOMATICALLY RESIZE THE TEXT.
!
            ILATRZ=ILATRS
            ILATRS='ON'
!
            CALL DPDT5B(ITITLE,NCTITL,   &
                        ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                        MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                        IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                        IDIGI2,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                        NCOLSP,ROWSEP,   &
                        ICAPSW,ICAPTY,IFRST,ILAST,   &
                        IFLAGS,IFLAGE,   &
                        ISUBRO,IBUGA3,IERROR)
            ILATRS=ILATRZ
            ICNT=1
            ICNT2=ICNT2+1
          ENDIF
!
          IDIGI2(ICNT,I)=NUMDIG
          IF(I.EQ.1 .OR. I.EQ.3 .OR. I.EQ.5 .OR. I.EQ.7)IDIGI2(ICNT,I)=0
          IVALUE(ICNT,I)=' '
          NCVALU(ICNT,I)=0
          AMAT(ICNT,I)=0.0
!
 1560   CONTINUE
!
        AMAT(ICNT,1)=REAL(LABID(J))
        AMAT(ICNT,2)=Y1(J)
        AMAT(ICNT,4)=Y2(J)
        AMAT(ICNT,6)=RANERR(J)
!
        IF(Y1(J).GT.Y1OUTU)THEN
          IVALUE(ICNT,3)='Extremely Unusual'
          NCVALU(ICNT,3)=17
        ELSEIF(Y1(J).GT.Y1INNU.AND.Y1(J).LE.Y1OUTU)THEN
          IVALUE(ICNT,3)='Unusual'
          NCVALU(ICNT,3)=7
        ELSEIF(Y1(J).LT.Y1OUTL)THEN
          IVALUE(ICNT,3)='Extremely Unusual'
          NCVALU(ICNT,3)=17
        ELSEIF(Y1(J).LT.Y1INNL.AND.Y1(J).GE.Y1OUTL)THEN
          IVALUE(ICNT,3)='Unusual'
          NCVALU(ICNT,3)=7
        ELSE
          IVALUE(ICNT,3)='Typical'
          NCVALU(ICNT,3)=7
        ENDIF
!
        IF(Y2(J).GT.Y2OUTU)THEN
          IVALUE(ICNT,5)='Extremely Unusual'
          NCVALU(ICNT,5)=17
        ELSEIF(Y2(J).GT.Y2INNU.AND.Y2(J).LE.Y2OUTU)THEN
          IVALUE(ICNT,5)='Unusual'
          NCVALU(ICNT,5)=7
        ELSEIF(Y2(J).LT.Y2OUTL)THEN
          IVALUE(ICNT,5)='Extremely Unusual'
          NCVALU(ICNT,5)=17
        ELSEIF(Y2(J).LT.Y2INNL.AND.Y2(J).GE.Y2OUTL)THEN
          IVALUE(ICNT,5)='Unusual'
          NCVALU(ICNT,5)=7
        ELSE
          IVALUE(ICNT,5)='Typical'
          NCVALU(ICNT,5)=7
        ENDIF
!
        IF(RANERR(J).GT.YROUTU)THEN
          IVALUE(ICNT,7)='Extremely Unusual'
          NCVALU(ICNT,7)=17
        ELSEIF(RANERR(J).GT.YRINNU.AND.RANERR(J).LE.YROUTU)THEN
          IVALUE(ICNT,7)='Unusual'
          NCVALU(ICNT,7)=7
        ELSEIF(RANERR(J).LT.YROUTL)THEN
          IVALUE(ICNT,7)='Extremely Unusual'
          NCVALU(ICNT,7)=17
        ELSEIF(RANERR(J).LT.YRINNL.AND.RANERR(J).GE.YROUTL)THEN
          IVALUE(ICNT,7)='Unusual'
          NCVALU(ICNT,7)=7
        ELSE
          IVALUE(ICNT,7)='Typical'
          NCVALU(ICNT,7)=7
        ENDIF
!
 1550 CONTINUE
!
      IF(ICNT.GT.0)THEN
        IFRST=.TRUE.
        ILAST=.TRUE.
        IF(ICAPTY.EQ.'LATE')THEN
          IFLAGS=.TRUE.
        ELSE
          IFLAGS=.TRUE.
          IF(ICNT2.GT.0)IFLAGS=.FALSE.
        ENDIF
        IFLAGE=.TRUE.
!
!       2014/08: FOR TABLE 4, SET LATEX RESIZE OPTION TO LET
!       LATEX AUTOMATICALLY RESIZE THE TEXT.
!
        ILATRZ=ILATRS
        ILATRS='ON'
!
        CALL DPDT5B(ITITLE,NCTITL,   &
                    ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                    MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                    IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                    IDIGI2,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                    NCOLSP,ROWSEP,   &
                    ICAPSW,ICAPTY,IFRST,ILAST,   &
                    IFLAGS,IFLAGE,   &
                    ISUBRO,IBUGA3,IERROR)
        ILATRS=ILATRZ
      ENDIF
!
      IRTFMD='OFF'
      IFLAGA=.TRUE.
      IFLAGB=.FALSE.
      ITTEMP='Precision Estimates for Two-Sample Proficiency:'
      NCTEMP=47
      NTOTAL=50
      NBLNK1=1
      NBLNK2=0
      ITYPE=2
      CALL DPDTXT(ITTEMP,NCTEMP,CPUMIN,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      IFLAGA=.FALSE.
!CCCC ITTEMP='Sample One Reproducibility Standard Deviation:'
!CCCC NCTEMP=46
      ITTEMP(1:NCY1)=Y1LAB(1:NCY1)
      NCTEMP=NCY1+1
      ITTEMP(NCTEMP:NCTEMP+36)=' Reproducibility Standard Deviation:'
      NCTEMP=NCTEMP+36
      CALL DPDTXT(ITTEMP,NCTEMP,REPSD1,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      NBLNK1=0
!CCCC ITTEMP='Sample Two Reproducibility Standard Deviation:'
!CCCC NCTEMP=46
      ITTEMP(1:NCY2)=Y2LAB(1:NCY2)
      NCTEMP=NCY2+1
      ITTEMP(NCTEMP:NCTEMP+36)=' Reproducibility Standard Deviation:'
      NCTEMP=NCTEMP+36
      CALL DPDTXT(ITTEMP,NCTEMP,REPSD2,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
!
!     CHECK THE RATIO OF REPRODUCIBILITY STANDARD DEVIATIONS.
!     IF NOT IN THE RANGE 0.7 < RATIO < 1.4, PRINT A WARNING
!     MESSAGE.
!
      RATIO=REPSD2/REPSD1
      ITTEMP='Ratio (Sample X/Sample Y) of'
      NCTEMP=28
      CALL DPDTXT(ITTEMP,NCTEMP,CPUMIN,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='Reproducibility Standard Deviations:'
      NCTEMP=36
      CALL DPDTXT(ITTEMP,NCTEMP,RATIO,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
!
!     CHECK IF RATIO IS "ACCEPTABLE" FOR POOLED
!
      IF(RATIO.LT.0.7 .OR. RATIO.GT.1.4)THEN
        NBLNK1=1
        ITTEMP='WARNING: The ratio of the reproducibility'
        NCTEMP=41
        CALL DPDTXT(ITTEMP,NCTEMP,CPUMIN,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
        NBLNK1=0
        ITTEMP='standard deviations is less than 0.7 or'
        NCTEMP=40
        CALL DPDTXT(ITTEMP,NCTEMP,CPUMIN,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
        ITTEMP='greater than 1.4.  This indicates that the'
        NCTEMP=42
        CALL DPDTXT(ITTEMP,NCTEMP,CPUMIN,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
        ITTEMP='two materials are significantly different so'
        NCTEMP=44
        CALL DPDTXT(ITTEMP,NCTEMP,CPUMIN,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
        ITTEMP='pooled estimates of precision are'
        NCTEMP=33
        CALL DPDTXT(ITTEMP,NCTEMP,CPUMIN,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
        ITTEMP='questionable and that two one sample'
        NCTEMP=36
        CALL DPDTXT(ITTEMP,NCTEMP,CPUMIN,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
        NBLNK2=1
        ITTEMP='proficiency analyses may be more appropriate.'
        NCTEMP=45
        CALL DPDTXT(ITTEMP,NCTEMP,CPUMIN,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
        NBLNK2=0
      ELSE
        NBLNK1=1
        ITTEMP='If the ratio is between 0.7 and 1.4, the'
        NCTEMP=40
        CALL DPDTXT(ITTEMP,NCTEMP,CPUMIN,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
        NBLNK1=0
        ITTEMP='samples are considered similar enough for'
        NCTEMP=41
        CALL DPDTXT(ITTEMP,NCTEMP,CPUMIN,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
        ITTEMP='determination of a pooled reproducibility'
        NCTEMP=41
        CALL DPDTXT(ITTEMP,NCTEMP,CPUMIN,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
        ITTEMP='standard deviation.'
        NBLNK2=1
        NCTEMP=19
        CALL DPDTXT(ITTEMP,NCTEMP,CPUMIN,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
        NBLNK2=0
      ENDIF
!
      ITTEMP='Pooled Reproducibility Standard Deviation:'
      NCTEMP=42
      CALL DPDTXT(ITTEMP,NCTEMP,POOLSD,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
      ITTEMP='Repeatability Standard Deviation:'
      NCTEMP=33
      NBLNK1=0
      IFLAGB=.TRUE.
      ISIZE=0
      CALL DPDTXT(ITTEMP,NCTEMP,REPEAT,NUMDIG,NTOTAL,NBLNK1,NBLNK2,   &
                  IFLAGA,IFLAGB,ISIZE,   &
                  ICAPSW,ICAPTY,ITYPE,ISUBRO,IBUGA3,IERROR)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
!
      IFNTSZ=0
      IRTFMD='VERB'
      IF(ICAPSW.EQ.'ON' .AND. ICAPTY.EQ.'RTF')THEN
        IPTSZ=20
        WRITE(ICOUT,8199)IBASLC,IPTSZ
        CALL DPWRST(ICOUT,'WRIT')
      ENDIF
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'2IP2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DP2IP2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR,IBUGA3
 9012   FORMAT('IERROR,IBUGA3 = ',A4,1X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)N
 9013   FORMAT('N = ',2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DP2IP2
      SUBROUTINE DP2CHS(Y1,Y2,X1,MAXNXT,   &
                        ICAPSW,IFORSW,   &
                        IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--COMPUTE A 2-SAMPLE CHI-SQUARE TEST
!              THAT 2 SAMPLES ARE FROM THE SAME DISTRIBUTION
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--98/12
!     ORIGINAL VERSION--DECEMBER  1998.
!     UPDATED         --MARCH     2006. SUPPORT FOR DIFFERENT DEFAULT
!                                       BINNING ALGORITHMS
!     UPDATED         --MARCH     2010. USE DPPARS AND DPPAR3
!     UPDATED         --MARCH     2010. ADD "GROUP" OPTION TO
!                                       DISTINGUISH RAW DATA CASE
!                                       FROM BINNED DATA CASE WHEN
!                                       THERE ARE MORE THAN TWO
!                                       VARIABLES
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASPL
      CHARACTER*4 ICAPSW
      CHARACTER*4 IFORSW
      CHARACTER*4 IGROUP
      CHARACTER*4 IBUGA2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IDATSW
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 ISUBN0
      CHARACTER*4 ICOMSV
      CHARACTER*4 ICOM2S
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      CHARACTER*4 ICASE
      CHARACTER*40 INAME
      PARAMETER (MAXSPN=20)
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
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION X1(*)
      DIMENSION ZX1(MAXOBV)
      DIMENSION ZX2(MAXOBV)
      DIMENSION ZY1(MAXOBV)
      DIMENSION ZY2(MAXOBV)
      DIMENSION XTEMP(MAXOBV)
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(IGARB1),ZY1(1))
      EQUIVALENCE (GARBAG(IGARB2),ZY2(1))
      EQUIVALENCE (GARBAG(IGARB3),ZX1(1))
      EQUIVALENCE (GARBAG(IGARB4),ZX2(1))
      EQUIVALENCE (GARBAG(IGARB5),XTEMP(1))
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOS2.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOF2.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
      ISUBN1='DP2C'
      ISUBN2='CH  '
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
!               ******************************************
!               **  TREAT THE CHI-SQUARE 2 SAMPLE CASE  **
!               ******************************************
!
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'2CHS')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DP2CHS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASPL,MAXNXT
   52   FORMAT('ICASPL,MAXNXT = ',A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGA2,IBUGA3,IBUGQ,ISUBRO
   53   FORMAT('IBUGA2,IBUGA3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ***************************
!               **  STEP 1--             **
!               **  EXTRACT THE COMMAND  **
!               ***************************
!
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'2CHS')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!  RECOGNIZE THE FOLLOWING FORMS FOR THE COMMAND:
!     CHI SQUARE 2 SAMPLE TEST Y1 Y2
!     CHISQUARE 2 SAMPLE TEST Y1 Y2
!     CHI SQUARE TWO SAMPLE TEST Y1 Y2
!     CHISQUARE TWO SAMPLE TEST Y1 Y2
!     2 SAMPLE CHI SQUARE TEST Y1 Y2
!     2 SAMPLE CHISQUARE TEST Y1 Y2
!     TWO SAMPLE CHI SQUARE TEST Y1 Y2
!     TWO SAMPLE CHISQUARE TEST Y1 Y2
!  THE WORD TEST IS OPTIONAL.  IN ADDITION, FOR PRE-BINNED DATA,
!  THERE CAN BE AN X VARIABLE AT THE END.
!
      IGROUP='OFF'
      ICOMSV=ICOM
      ICOM2S=ICOM2
      IFLAGS=0
!
      IF(ICOM.EQ.'GROU')THEN
        IGROUP='ON'
        ICOM=IHARG(1)
        ICOM2=IHARG2(1)
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGA2,IERROR)
        IFLAGS=1
      ENDIF
!
      IF(ICOM.EQ.'CHI')THEN
        IF(NUMARG.GE.3.AND.IHARG(1).EQ.'SQUA'.AND.   &
           IHARG(2).EQ.'2'.AND.IHARG(3).EQ.'SAMP')THEN
           ISHIFT=3
           GO TO 112
        ELSEIF(NUMARG.GE.3.AND.IHARG(1).EQ.'SQUA'.AND.   &
           IHARG(2).EQ.'TWO'.AND.IHARG(3).EQ.'SAMP')THEN
           ISHIFT=3
           GO TO 112
        ENDIF
      ELSEIF(ICOM.EQ.'CHIS')THEN
        IF(NUMARG.GE.2.AND.IHARG(1).EQ.'2'.AND.IHARG(2).EQ.'SAMP')THEN
           ISHIFT=2
           GO TO 112
        ELSEIF(NUMARG.GE.2.AND.IHARG(1).EQ.'TWO'.AND.   &
           IHARG(2).EQ.'SAMP')THEN
           ISHIFT=2
           GO TO 112
        ENDIF
      ELSEIF(ICOM.EQ.'2'.OR.ICOM.EQ.'TWO')THEN
        IF(NUMARG.GE.3.AND.IHARG(1).EQ.'SAMP'.AND.   &
           IHARG(2).EQ.'CHI'.AND.IHARG(3).EQ.'SQUA')THEN
           ISHIFT=3
           GO TO 112
        ELSEIF(NUMARG.GE.2.AND.IHARG(1).EQ.'SAMP'.AND.   &
           IHARG(2).EQ.'CHIS')THEN
           ISHIFT=2
           GO TO 112
        ENDIF
      ENDIF
!
! ----------NO MATCH FOUND----------
!
      ICASPL='    '
      IFOUND='NO'
      GO TO 9000
!
  112 CONTINUE
      ICASPL='2CHS'
      CALL ADJUST(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      IFOUND='YES'
      IF(IHARG(1).EQ.'TEST')THEN
        ISHIFT=1
        CALL ADJUST(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      ENDIF
!
!               ****************************************
!               **  STEP 2--                          **
!               **  EXTRACT THE VARIABLE LIST         **
!               ****************************************
!
      ISTEPN='2'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'2CHS')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     IF "GROUPED" SPECIFIED, THEN LAST VARIABLE INTERPRETED
!     AS GROUP-ID VARIABLE.  CURRENLTY, ONLY SUPPORT THE CASE
!     WHERE THERE ARE EQUI-SIZED BINS AND ALL RESPONSE VARIABLES
!     USE SAME BINNING.
!
      INAME='TWO SAMPLE CHI-SQUARE TEST'
      MINNA=1
      MAXNA=100
      MINN2=3
      IFLAGE=0
      IFLAGM=1
      MINNVA=2
      MAXNVA=20
      IF(IGROUP.EQ.'ON')THEN
        IFLAGE=1
        MINNVA=3
      ENDIF
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'2CHS')THEN
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
      CLWID=CLWIDT(1)
      XSTART=CLLIMI(1)
      XSTOP=CLLIMI(2)
!
      NUMVA2=1
      NUMVAT=NUMVAR
      IDATSW='RAW'
!
      IF(IGROUP.EQ.'ON')THEN
        IDATSW='FREQ'
        NUMVAT=NUMVAR-1
        ICOL=NUMVAR
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                    INAME,IVARN1,IVARN2,IVARTY,   &
                    ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                    MAXCP4,MAXCP5,MAXCP6,   &
                    V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                    X1,X1,X1,NS3,NLOCA2,NLOCA3,ICASE,   &
                    IBUGA3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
        CLWID=X1(2) - X1(1)
        XSTART=X1(1)
        XSTOP=X1(NS3)
      ELSE
        NS3=0
      ENDIF
!
!               *****************************************
!               **  STEP 3A--                          **
!               **  CASE 1: TWO RESPONSE VARIABLES     **
!               **          WITH NO REPLICATION        **
!               *****************************************
!
      ISTEPN='3A'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'2CHS')   &
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMVAT.LT.2)THEN
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      DO 5210 I=1,NUMVAT-1
        ICOL=I
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                    INAME,IVARN1,IVARN2,IVARTY,   &
                    ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                    MAXCP4,MAXCP5,MAXCP6,   &
                    V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                    Y1,Y1,Y1,NS1,NLOCA2,NLOCA3,ICASE,   &
                    IBUGA3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
        DO 5220 J=I+1,NUMVAT
          ICOL=J
          CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                      INAME,IVARN1,IVARN2,IVARTY,   &
                      ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                      MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                      MAXCP4,MAXCP5,MAXCP6,   &
                      V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                      Y2,Y2,Y2,NS2,NLOCA2,NLOCA3,ICASE,   &
                      IBUGA3,ISUBRO,IFOUND,IERROR)
!
!               *****************************************
!               **  STEP 52--                          **
!               **  PERFORM 2-SAMPLE CHI-SQUARE TEST   **
!               *****************************************
!
          ISTEPN='52'
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'2CHS')THEN
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5211)
 5211       FORMAT('***** FROM DP2CHS, BEFORE CALL DP2CH2--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5212)I,J,NS1,NS2,MAXN
 5212       FORMAT('I,J,NS1,NS2,MAXN = ',5I8)
            CALL DPWRST('XXX','BUG ')
            DO 5215 II=1,MAX(NS1,NS2)
              WRITE(ICOUT,5216)II,Y1(II),Y2(II),X1(II)
 5216         FORMAT('I,Y(I),X(I) = ',I8,3G15.7)
              CALL DPWRST('XXX','BUG ')
 5215       CONTINUE
          ENDIF
!
          CALL DP2CH2(Y1,Y2,X1,NS1,NS2,NS3,   &
                      ICASPL,IDATSW,IRHSTG,   &
                      CLWID,XSTART,XSTOP,   &
                      XTEMP,IHSTCW,IHSTOU,MAXOBV,   &
                      ICAPSW,ICAPTY,IFORSW,   &
                      IVARN1(I),IVARN2(I),IVARN1(J),IVARN2(J),   &
                      IVARN1(NUMVAR),IVARN2(NUMVAR),   &
                      STATVA,STATCD,STATNU,CUTU90,CUTU95,CUTU99,   &
                      ZY1,ZY2,ZX1,ZX2,NFREQ,   &
                      IBUGA3,ISUBRO,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
 5220   CONTINUE
 5210 CONTINUE
!
!               ***************************************
!               **  STEP 7--                         **
!               **  UPDATE INTERNAL DATAPLOT TABLES  **
!               ***************************************
!
      ISTEPN='7'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'2CHS')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISUBN0='DPCH'
!
      IH='STAT'
      IH2='VAL '
      VALUE0=STATVA
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA3,IERROR)
!
      IH='STAT'
      IH2='NU  '
      VALUE0=STATNU
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA3,IERROR)
!
      IH='STAT'
      IH2='CDF '
      VALUE0=STATCD
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA3,IERROR)
!
      IH='CUTU'
      IH2='PP90'
      VALUE0=CUTU90
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA3,IERROR)
!
      IH='CUTU'
      IH2='PP95'
      VALUE0=CUTU95
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA3,IERROR)
!
      IH='CUTU'
      IH2='PP99'
      VALUE0=CUTU99
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA3,IERROR)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
!
      IF(IFOUND.EQ.'NO' .AND. IFLAGS.EQ.1)THEN
        ISHIFT=1
        CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGA2,IERROR)
        IHARG(1)=ICOM
        IHARG(2)=ICOM2
        ICOM=ICOMSV
        ICOM2=ICOM2S
      ENDIF
!
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'2CHS')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DP2CHS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NFREQ,ICASPL
 9013   FORMAT('NFREQ,ICASPL = ',I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DP2CHS
      SUBROUTINE DP2CH2(Y1,Y2,X1,N1,N2,N3,   &
                        ICASPL,IDATSW,IRHSTG,   &
                        CLWID,XSTART,XSTOP,   &
                        XTEMP,IHSTCW,IHSTOU,MAXOBV,   &
                        ICAPSW,ICAPTY,IFORSW,   &
                        IVARID,IVARI2,IVARI3,IVARI4,IVARI5,IVARI6,   &
                        STATVA,STATCD,STATNU,CUTH90,CUTH95,CUTH99,   &
                        ZY1,ZY2,ZX1,ZX2,M2,IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--COMPUTE A 2-SAMPLE CHI-SQUARE TEST
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--98/11
!     ORIGINAL VERSION--DECEMBER  1998.
!     UPDATED         --MARCH     2006. SUPPORT FOR DIFFERENT DEFAULT
!                                       BINNING ALGORITHMS
!     UPDATED         --MARCH     2011. USE DPDTA1, DPDTA5 TO PRINT
!                                       OUTPUT
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASPL
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 IVARID
      CHARACTER*4 IVARI2
      CHARACTER*4 IVARI3
      CHARACTER*4 IVARI4
      CHARACTER*4 IVARI5
      CHARACTER*4 IVARI6
      CHARACTER*4 IDATSW
      CHARACTER*4 IRHSTG
      CHARACTER*4 IHSTCW
      CHARACTER*4 IHSTOU
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IWRIT2
!
!---------------------------------------------------------------------
!
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION X1(*)
      DIMENSION ZY1(*)
      DIMENSION ZY2(*)
      DIMENSION ZX1(*)
      DIMENSION ZX2(*)
      DIMENSION XTEMP(*)
!
      PARAMETER (NUMALP=7)
      REAL ALPHA(NUMALP)
!
      PARAMETER(NUMCLI=5)
      PARAMETER(MAXLIN=3)
      PARAMETER (MAXROW=NUMALP)
      PARAMETER (MAXRO2=35)
      CHARACTER*40 IDIST
      CHARACTER*60 ITITLE
      CHARACTER*60 ITITLZ
      CHARACTER*1  ITITL9
      CHARACTER*60 ITEXT(MAXRO2)
      CHARACTER*4  ALIGN(NUMCLI)
      CHARACTER*4  VALIGN(NUMCLI)
      REAL         AVALUE(MAXRO2)
      INTEGER      NCTEXT(MAXRO2)
      INTEGER      IDIGIT(MAXRO2)
      INTEGER      NTOT(MAXRO2)
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
      LOGICAL IFLAGS
      LOGICAL IFLAGE
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
      DATA ALPHA/50.0, 80.0, 90.0, 95.0, 97.5, 99.0, 99.9/
!
!-----START POINT-----------------------------------------------------
!
!
      ISUBN1='DP2C'
      ISUBN2='H2  '
!
      IWRIT2='OFF'
      IERROR='NO'
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'2CH2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)
   71   FORMAT('***** AT THE BEGINNING OF DP2CH2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)ICASPL,IDATSW,N1,N2,N3
   72   FORMAT('ICASPL,IDATSW,N1,N2,N3 = ',A4,2X,A4,2X,4I8)
        CALL DPWRST('XXX','BUG ')
        DO 85 I=1,N1
          WRITE(ICOUT,86)I,Y1(I),Y2(I),X1(I)
   86     FORMAT('I,Y1(I),Y2(I),X1(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
   85   CONTINUE
      ENDIF
!
      MAXOB2=MAXOBV
      IDIST='TWO SAMPLE CHI-SQUARE'
      CALL DP2CH3(Y1,Y2,X1,N1,N2,N3,   &
                  IDATSW,IRHSTG,   &
                  CLWID,XSTART,XSTOP,   &
                  CLWID2,DXSTAR,DXSTOP,   &
                  XTEMP,IHSTCW,IHSTOU,MAXOBV,MAXOB2,   &
                  STATVA,STATCD,STATNU,NCELLS,   &
                  Y1MEAN,Y1SD,Y1MIN,Y1MAX,   &
                  Y2MEAN,Y2SD,Y2MIN,Y2MAX,   &
                  ZY1,ZY2,ZX1,ZX2,M2,   &
                  IBUGA3,ISUBRO,IERROR)
      PVAL=1.0 - STATCD
!
!               *******************************
!               **   STEP 32--               **
!               **   WRITE OUT EVERYTHING    **
!               **   FOR A CHI-SQUARED TEST  **
!               *******************************
!
      ISTEPN='32'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'2CH2')   &
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
      ITITLE='Chi-Square Two Sample Test'
      NCTITL=26
      ITITLZ=' '
      NCTITZ=0
      ICNT=0
!
      ICNT=ICNT+1
      ITEXT(ICNT)='First Response Variable:  '
      WRITE(ITEXT(ICNT)(27:30),'(A4)')IVARID(1:4)
      WRITE(ITEXT(ICNT)(31:34),'(A4)')IVARI2(1:4)
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Second Response Variable: '
      WRITE(ITEXT(ICNT)(27:30),'(A4)')IVARI3(1:4)
      WRITE(ITEXT(ICNT)(31:34),'(A4)')IVARI4(1:4)
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      IF(N3.GT.0)THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='Group-ID Variable:        '
        WRITE(ITEXT(ICNT)(27:30),'(A4)')IVARI5(1:4)
        WRITE(ITEXT(ICNT)(31:34),'(A4)')IVARI6(1:4)
        NCTEXT(ICNT)=34
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
      ENDIF
!
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='H0: The Two Samples Come From the'
      NCTEXT(ICNT)=33
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='    Same (Unspecified) Distribution'
      NCTEXT(ICNT)=35
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: The Two Samples Come From'
      NCTEXT(ICNT)=29
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='    Different Distributions'
      NCTEXT(ICNT)=27
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample One Summary Statistics:'
      NCTEXT(ICNT)=30
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Observations:'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=REAL(N1)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Mean:'
      NCTEXT(ICNT)=12
      AVALUE(ICNT)=Y1MEAN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Standard Deviation:'
      NCTEXT(ICNT)=26
      AVALUE(ICNT)=Y1SD
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Minimum:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=Y1MIN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Maximum:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=Y1MAX
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Two Summary Statistics:'
      NCTEXT(ICNT)=30
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Observations:'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=REAL(N2)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Mean:'
      NCTEXT(ICNT)=12
      AVALUE(ICNT)=Y2MEAN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Standard Deviation:'
      NCTEXT(ICNT)=26
      AVALUE(ICNT)=Y2SD
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Minimum:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=Y2MIN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Maximum:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=Y2MAX
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Non-Empty Cells:'
      NCTEXT(ICNT)=26
      AVALUE(ICNT)=REAL(NCELLS)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Class Width For Bins:'
      NCTEXT(ICNT)=21
      AVALUE(ICNT)=CLWID2
      IF(IDATSW.EQ.'FREQ')AVALUE(ICNT)=CLWID
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Lower Class Limit:'
      NCTEXT(ICNT)=18
      AVALUE(ICNT)=DXSTAR
      IF(IDATSW.EQ.'FREQ')AVALUE(ICNT)=XSTART
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Upper Class Limit:'
      NCTEXT(ICNT)=18
      AVALUE(ICNT)=DXSTOP
      IF(IDATSW.EQ.'FREQ')AVALUE(ICNT)=XSTOP
      IDIGIT(ICNT)=NUMDIG
!
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Chi-Squared Test Statistic:'
      NCTEXT(ICNT)=27
      AVALUE(ICNT)=STATVA
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Degrees of Freedom:'
      NCTEXT(ICNT)=19
      AVALUE(ICNT)=STATNU
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='CDF of Test Statistic:'
      NCTEXT(ICNT)=22
      AVALUE(ICNT)=STATCD
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='P-Value:'
      NCTEXT(ICNT)=8
      AVALUE(ICNT)=PVAL
      IDIGIT(ICNT)=NUMDIG
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
      ITITLE='Conclusions (Upper 1-Tailed Test)'
      NCTITL=33
      ITITL9=' '
      NCTIT9=0
!
      ITITL2(1,1)=' '
      NCTIT2(1,1)=0
      ITITL2(2,1)='Null'
      NCTIT2(2,1)=4
      ITITL2(3,1)='Hypothesis'
      NCTIT2(3,1)=10
      ITITL2(1,2)=' '
      NCTIT2(1,2)=0
      ITITL2(2,2)='Confidence'
      NCTIT2(2,2)=10
      ITITL2(3,2)='Level'
      NCTIT2(3,2)=5
      ITITL2(1,3)=' '
      NCTIT2(1,3)=0
      ITITL2(2,3)='Critical'
      NCTIT2(2,3)=8
      ITITL2(3,3)='Value (>)'
      NCTIT2(3,3)=9
      ITITL2(1,4)='Null Hypothesis'
      NCTIT2(1,4)=15
      ITITL2(2,4)='Acceptance'
      NCTIT2(2,4)=10
      ITITL2(3,4)='Interval'
      NCTIT2(3,4)=8
      ITITL2(1,5)='Null'
      NCTIT2(1,5)=4
      ITITL2(2,5)='Hypothesis'
      NCTIT2(2,5)=10
      ITITL2(3,5)='Conclusion'
      NCTIT2(3,5)=10
!
      NMAX=0
      NUMCOL=5
      DO 5210 I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IF(I.EQ.1)NTOT(I)=12
        IF(I.EQ.4)NTOT(I)=18
        NMAX=NMAX+NTOT(I)
        IF(I.EQ.3)THEN
          ITYPCO(I)='NUME'
        ELSE
          ITYPCO(I)='ALPH'
        ENDIF
        IF(I.EQ.2)THEN
          IDIGIT(I)=1
        ELSEIF(I.EQ.3)THEN
          IDIGIT(I)=2
        ELSE
          IDIGIT(I)=NUMDIG
        ENDIF
        IWHTML(1)=150
        IWHTML(2)=125
        IWHTML(3)=125
        IWHTML(4)=150
        IWHTML(5)=150
        IINC=1600
        IINC2=1400
        IINC3=2200
        IWRTF(1)=IINC
        IWRTF(2)=IWRTF(1)+IINC
        IWRTF(3)=IWRTF(2)+IINC2
        IWRTF(4)=IWRTF(3)+IINC3
        IWRTF(5)=IWRTF(4)+IINC2
!
        DO 5289 J=1,NUMALP
          AMAT(J,1)=0.0
          AMAT(J,2)=0.0
          AMAT(J,4)=0.0
          AMAT(J,5)=0.0
          ALPHAT=ALPHA(J)/100.0
          CALL CHSPPF(ALPHAT,INT(STATNU+0.1),CV)
          AMAT(J,3)=CV
          IF(J.EQ.3)CUTH90=CV
          IF(J.EQ.4)CUTH95=CV
          IF(J.EQ.5)CUTH99=CV
          IVALUE(J,1)='Same'
          NCVALU(J,1)=4
          IF(STATVA.LE.CV)THEN
            IVALUE(J,5)(1:6)='ACCEPT'
          ELSE
            IVALUE(J,5)(1:6)='REJECT'
          ENDIF
          NCVALU(J,5)=6
          IF(J.EQ.1)THEN
            IVALUE(J,2)='50.0%'
            NCVALU(J,2)=5
            IVALUE(J,4)='(0,0.500)'
            NCVALU(J,4)=9
          ELSEIF(J.EQ.2)THEN
            IVALUE(J,2)='80.0%'
            NCVALU(J,2)=5
            IVALUE(J,4)='(0,0.800)'
            NCVALU(J,4)=9
          ELSEIF(J.EQ.3)THEN
            IVALUE(J,2)='90.0%'
            NCVALU(J,2)=5
            IVALUE(J,4)='(0,0.900)'
            NCVALU(J,4)=9
          ELSEIF(J.EQ.4)THEN
            IVALUE(J,2)='95.0%'
            NCVALU(J,2)=5
            IVALUE(J,4)='(0,0.950)'
            NCVALU(J,4)=9
          ELSEIF(J.EQ.5)THEN
            IVALUE(J,2)='97.5%'
            NCVALU(J,2)=5
            IVALUE(J,4)='(0,0.975)'
            NCVALU(J,4)=9
          ELSEIF(J.EQ.6)THEN
            IVALUE(J,2)='99.0%'
            NCVALU(J,2)=5
            IVALUE(J,4)='(0,0.990)'
            NCVALU(J,4)=9
          ELSEIF(J.EQ.7)THEN
            IVALUE(J,2)='99.9%'
            NCVALU(J,2)=5
            IVALUE(J,4)='(0,0.999)'
            NCVALU(J,4)=9
          ENDIF
 5289   CONTINUE
!
 5210 CONTINUE
!
      ICNT=NUMALP
      NUMLIN=3
      NUMCOL=5
      IFRST=.TRUE.
      ILAST=.TRUE.
      IFLAGS=.TRUE.
      IFLAGE=.TRUE.
      CALL DPDTA5(ITITLE,NCTITL,   &
                  ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                  MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                  IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'2CH2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DP2CH2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICASPL,IDATSW,IERROR,M2,N1
 9012   FORMAT('ICASPL,IDATSW,IERROR,M2,N1 = ',3(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        DO 9020 I=1,M2
          WRITE(ICOUT,9021)I,ZY1(I),ZY2(I),ZX1(I),ZX2(I)
 9021     FORMAT('I,ZY1(I),ZY2(I),ZX1(I),ZX2(I) = ',I8,4E15.7)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DP2CH2
      SUBROUTINE DP2CH3(Y1,Y2,X1,N1,N2,N3,   &
                        IDATSW,IRHSTG,   &
                        CLWID,XSTART,XSTOP,   &
                        CLWID2,DXSTAR,DXSTOP,   &
                        XTEMP,IHSTCW,IHSTOU,MAXOBV,MAXOB2,   &
                        STATVA,STATCD,STATNU,NCELLS,   &
                        Y1MEAN,Y1SD,Y1MIN,Y1MAX,   &
                        Y2MEAN,Y2SD,Y2MIN,Y2MAX,   &
                        ZY1,ZY2,ZX1,ZX2,M2,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--COMPUTE A 2-SAMPLE CHI-SQUARE TEST
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2011/7
!     ORIGINAL VERSION--JULY      2011. EXTRACTED FROM DP2CH2 ROUTINE
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IDATSW
      CHARACTER*4 IRHSTG
      CHARACTER*4 IHSTCW
      CHARACTER*4 IHSTOU
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 IWRIT2
      CHARACTER*4 IRELAT
      CHARACTER*4 IOP
!
      CHARACTER*40 IDIST
!
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DTEMP1
      DOUBLE PRECISION DTEMP2
      DOUBLE PRECISION DTEMP3
      DOUBLE PRECISION DFACT1
      DOUBLE PRECISION DFACT2
!
!---------------------------------------------------------------------
!
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION X1(*)
      DIMENSION ZY1(*)
      DIMENSION ZY2(*)
      DIMENSION ZX1(*)
      DIMENSION ZX2(*)
      DIMENSION XTEMP(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!
      ISUBN1='DP2C'
      ISUBN2='H3  '
      IRELAT='OFF'
      IWRIT2='OFF'
      IERROR='NO'
!
      AN1=0.0
      AN2=0.0
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      NMIN=MIN(N1,N2)
      IF(IDATSW.EQ.'FREQ')NMIN=MIN(NMIN,N3)
!
      IF(NMIN.LT.2)THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
   31   FORMAT('***** ERROR IN CHI-SQUARE TWO SAMPLE TEST--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,32)
   32   FORMAT('      THE NUMBER OF OBSERVATIONS FOR EACH VARIABLE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,33)
   33   FORMAT('      MUST BE AT LEAST 1;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,34)N1
   34   FORMAT('      THE NUMBER OF OBSERVATIONS FOR VARIABLE 1 = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,35)N2
   35   FORMAT('      THE NUMBER OF OBSERVATIONS FOR VARIABLE 2 = ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(IDATSW.EQ.'FREQ')THEN
          WRITE(ICOUT,36)N3
   36     FORMAT('      THE NUMBER OF OBSERVATIONS FOR VARIABLE 3 = ',   &
                 I8)
        CALL DPWRST('XXX','BUG ')
        ENDIF
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'2CH3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,70)
   70   FORMAT('***** AT THE BEGINNING OF DP2CH3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)CLWID,XSTART,XSTOP
   71   FORMAT('CLWID,XSTART,XSTOP = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)IDATSW,N1,N2,N3
   72   FORMAT('IDATSW,N1,N2,N3 = ',A4,2X,3I8)
        CALL DPWRST('XXX','BUG ')
        DO 85 I=1,MAX(N1,N2)
          WRITE(ICOUT,86)I,Y1(I),Y2(I),X1(I)
   86     FORMAT('I,Y1(I),Y2(I),X1(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
   85   CONTINUE
      ENDIF
!
!               **************************************
!               **  STEP 4--                        **
!               **  IF DATA NOT ALREADY BINNED, THEN**
!               **  BIN THE DATA                    **
!               **************************************
!
      IF(IDATSW.EQ.'RAW')THEN
        CLWID2=CLWID
        DXSTAR=XSTART
        DXSTOP=XSTOP
!
        IFLAG=0
        CALL SUMRAW(Y1,N1,IDIST,IFLAG,   &
                    Y1MEAN,Y1VAR,Y1SD,Y1MIN,Y1MAX,   &
                    ISUBRO,IBUGA3,IERROR)
!
        CALL SUMRAW(Y2,N2,IDIST,IFLAG,   &
                    Y2MEAN,Y2VAR,Y2SD,Y2MIN,Y2MAX,   &
                    ISUBRO,IBUGA3,IERROR)
!
        IF(CLWID.NE.CPUMIN.AND.XSTART.NE.CPUMIN.AND.   &
          XSTOP.NE.CPUMAX)GO TO 200
          IF(CLWID.EQ.CPUMIN)THEN
            CLWID2=MIN(0.3*Y1SD,0.3*Y2SD)
          ENDIF
          IF(XSTART.EQ.CPUMIN)THEN
            DXSTAR=MIN(Y1MEAN-6.0*Y1SD,Y2MEAN-6.0*Y2SD)
          ENDIF
          IF(XSTOP.EQ.CPUMAX)THEN
!CCCC       DXSTOP=MIN(Y1MEAN+6.0*Y1SD,Y2MEAN+6.0*Y2SD)
            DXSTOP=MAX(Y1MEAN+6.0*Y1SD,Y2MEAN+6.0*Y2SD)
          ENDIF
  200   CONTINUE
!
        AN1=REAL(N1)
        CALL DPBIN(Y1,N1,IRELAT,CLWID2,DXSTAR,DXSTOP,IRHSTG,   &
                   XTEMP,MAXOBV,IHSTCW,IHSTOU,   &
                   ZY1,ZX1,M2A,IBUGA3,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
        IF(M2A.GT.MAXOB2)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,31)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,232)M2A,MAXOB2
  232     FORMAT('      THE NUMBER OF BINS (',I8,') IS GREATER THAN ',   &
               I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
!
        CALL SORTC(ZX1,ZY1,M2A,ZX1,ZY1)
        AN2=REAL(N2)
        CALL DPBIN(Y2,N2,IRELAT,CLWID2,DXSTAR,DXSTOP,IRHSTG,   &
                   XTEMP,MAXOBV,IHSTCW,IHSTOU,   &
                   ZY2,ZX2,M2B,IBUGA3,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
        CALL SORTC(ZX2,ZY2,M2B,ZX2,ZY2)
        M2=MAX(M2A,M2B)
!
!       WRITE BINNED DATA TO DPST1F.DAT
!
        IOP='OPEN'
        IFLG1=1
        IFLG2=1
        IFLG3=0
        IFLG4=0
        IFLG5=0
        CALL DPAUFI(IOP,IFLG1,IFLG2,IFLG3,IFLG4,IFLG5,   &
                    IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                    IBUGA3,ISUBRO,IERROR)
        DO 235 I=1,M2A
          WRITE(IOUNI1,'(E15.7,F10.1)')ZX1(I),ZY1(I)
  235   CONTINUE
        DO 238 I=1,M2B
          WRITE(IOUNI2,'(E15.7,F10.1)')ZX2(I),ZY2(I)
  238   CONTINUE
        IOP='CLOS'
        CALL DPAUFI(IOP,IFLG1,IFLG2,IFLG3,IFLG4,IFLG5,   &
                    IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                    IBUGA3,ISUBRO,IERROR)
!
      ELSEIF(IDATSW.EQ.'FREQ')THEN
        IDIST='TWO SAMPLE CHI-SQUARE'
        IFLAG1=0
        IFLAG2=0
        CALL SUMGRP(Y1,X1,N1,IDIST,IFLAG1,IFLAG2,   &
                    XTEMP,ZY1,ZX1,MAXNXT,   &
                    Y1MEAN,Y1VAR,Y1SD,Y1MIN,Y1MAX,NTOTZZ,   &
                    ISUBRO,IBUGA3,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
        AN1=REAL(NTOTZZ)
!
        CALL SUMGRP(Y2,X1,N2,IDIST,IFLAG1,IFLAG2,   &
                    XTEMP,ZY1,ZX1,MAXNXT,   &
                    Y2MEAN,Y2VAR,Y2SD,Y2MIN,Y2MAX,NTOTZZ,   &
                    ISUBRO,IBUGA3,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
        AN2=REAL(NTOTZZ)
!
        DO 1009 I=1,N1
          ZY1(I)=Y1(I)
          ZY2(I)=Y2(I)
          ZX1(I)=X1(I)
          ZX2(I)=X1(I)
 1009   CONTINUE
        CALL SORTC(ZX1,ZY1,N1,ZX2,ZY1)
        CALL SORTC(ZX1,ZY2,N1,ZX1,ZY2)
        M2=N1
      ENDIF
!
!               ****************************************
!               **  STEP 4.1--                        **
!               **  COMPUTE CHI-SQUARE TEST STATISTIC **
!               **  EXPECTED                          **
!               ****************************************
!
      DSUM1=0.0D0
      DFACT1=DBLE(SQRT(AN2/AN1))
      DFACT2=DBLE(SQRT(AN1/AN2))
      NCELLS=0
      DO 1199 I=1,M2
        IF(ZY1(I).EQ.0.0 .AND. ZY2(I).EQ.0.0)GO TO 1199
        NCELLS=NCELLS+1
        DTEMP1=DBLE(ZY1(I))
        DTEMP2=DBLE(ZY2(I))
        DTEMP3=(DFACT1*DTEMP1 - DFACT2*DTEMP2)**2/(DTEMP1+DTEMP2)
        DSUM1=DSUM1 + DTEMP3
 1199 CONTINUE
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'2CH3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1301)NCELLS,DSUM1,AN1,AN2
 1301   FORMAT('NCELLS,DSUM1,AN1,AN2 = ',I8,3G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      STAT=REAL(DSUM1)
      IDF=NCELLS
      IF(N1.EQ.N2)IDF=IDF-1
!
      CALL CHSCDF(STAT,IDF,CDF)
      PVAL=1.0 - CDF
!
      STATVA=STAT
      STATCD=CDF
      STATNU=IDF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'2CH3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DP2CH3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IDATSW,IERROR,M2,N1
 9012   FORMAT('IDATSW,IERROR,M2,N1 = ',2(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)STATVA,STATCD,PVAL,STATNU
 9014   FORMAT('STATVA,STATCD,PVAL,STATNU = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        DO 9020 I=1,M2
          WRITE(ICOUT,9021)I,ZY1(I),ZY2(I),ZX1(I),ZX2(I)
 9021     FORMAT('I,ZY1(I),ZY2(I),ZX1(I),ZX2(I) = ',I8,4E15.7)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DP2CH3
      SUBROUTINE DP2DCH(Y1,Y2,YTEMP,N,IWRITE,MAXNXT,   &
      Y3,Y4,NHULL,   &
      IN,IA,IB,IH,IL,   &
      IBUGA3,IERROR)
!
!     PURPOSE--COMPUTE THE 2D CONVEX HULL OF A SET OF POINTS.
!              USE ACM ALGORITHM 523 TO COMPUTE THE
!              CONVEX HULL.
!     EXAMPLES--LET ZY ZX = 2D CONVEX HULL Y X
!     INPUT  ARGUMENTS--Y1  Y-AXIS VECTOR
!                       Y2  X-AXIS VECTOR
!     OUTPUT ARGUMENTS--Y3 Y-AXIS VECTOR OF THE CONVEX HULL
!                       Y4 X-AXIS VECTOR OF THE CONVEX HULL
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2008/4
!     ORIGINAL VERSION--APRIL    2008.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION Y3(*)
      DIMENSION Y4(*)
      DIMENSION YTEMP(*)
!
      INTEGER IN(*)
      INTEGER IA(*)
      INTEGER IB(*)
      INTEGER IH(*)
      INTEGER IL(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DP2D'
      ISUBN2='CH  '
!
      IERROR='NO'
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DP2DCH--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,IWRITE,N
   52   FORMAT('IBUGA3,IWRITE,N = ',A4,2X,A4,2X,I10)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,Y1(I),Y2(I)
   56     FORMAT('I,Y1(I),Y2(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ******************************************
!               **  CALL THE   CONVEX ROUTINE FROM      **
!               **  ACM ALGORITHM 523.                  **
!               ******************************************
!
!     NOTE: FOR THE ACM ALGORITHM, THE DATA IS STORED IN
!           COLUMN ORDER.  COPY TO A TEMPORARY ARRAY THAT
!           EMULATES COLUMN ORDER.
!
      NMAX=MAXNXT/2
      IF(N.GT.MAXNXT)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR FROM 2D CONVEX HULL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)MAXNXT
  103   FORMAT('      THE MAXIMUM NUMBER OF POINTS, ',I8,',',   &
               'EXCEEDED.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,105)N
  105   FORMAT('      THE REQUESTED NUMBER OF POINTS = ',I10)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      ICNT=0
      DO 200 I=1,N
        ICNT=ICNT+1
        YTEMP(ICNT)=Y2(I)
        ICNT=ICNT+1
        YTEMP(ICNT)=Y1(I)
        IN(I)=I
  200 CONTINUE
!
      CALL CONVEX(N,YTEMP,N,IN,IA,IB,IH,NHULL,IL)
!
!     THE VERTICES IN IH ARE GIVEN IN ORDER OF OCCURENCE.
!     USE LINKED LIST IN IL TO SORT COORDINATES INTO AN
!     APPROPRIATE SEQUENCE FOR PLOTTING.
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,211)
  211   FORMAT('***** BEFORE SORTC3 ******')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,214)NHULL
  214   FORMAT('NHULL = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO 215 I=1,NHULL
          WRITE(ICOUT,216)I,IH(I),IL(I)
  216     FORMAT('I,IH(I),IL(I) = ',3I8)
          CALL DPWRST('XXX','BUG ')
  215   CONTINUE
      ENDIF
!
      IK=IL(1)
      DO 250 I=1,NHULL
        J=IH(IK)
        Y3(I)=Y1(J)
        Y4(I)=Y2(J)
        IK=IL(IK)
  250 CONTINUE
!
!     MANY COMPUTATION GEOMETRY ALGORITHMS ASSUME CONVEX HULL
!     IN "STANDARD" FORM.  STANDARD FORM MEANS COUNTER CLOCKWISE
!     DIRECTION WITH FIRST POINT BEING THE MINIMUM Y VALUE.
!     THE POINTS ARE NOW IN COUNTER CLOCKWISE ORDER, SO JUST NEED
!     TO PERFORM A CIRCULAR SHIFT.
!
!     SHIFT CONVEX HULL SO THAT FIRST POINT HAS THE MINIMUM Y
!     VALUE (IN CASE OF TIES, PICK ONE WITH THE MINIMUM X VALUE.
!
      IINDX=1
      YMIN=Y3(1)
      XSAVE=Y4(1)
      DO 300 I=2,NHULL
        IF(Y3(I).LT.YMIN)THEN
          IINDX=I
          YMIN=Y3(I)
          XSAVE=Y4(I)
        ELSEIF(Y3(I).EQ.YMIN .AND. Y4(I).LT.XSAVE)THEN
          IINDX=I
          YMIN=Y3(I)
          XSAVE=Y4(I)
        ENDIF
  300 CONTINUE
!
      IF(IINDX.GT.1)THEN
        NSHIFT=-IINDX + 1
        DO 400 I=1,NHULL
          INDX1=MOD(I-NSHIFT-1,NHULL)+1
          II=2*(I-1) + 1
          YTEMP(II)=Y3(INDX1)
          YTEMP(II+1)=Y4(INDX1)
  400   CONTINUE
        DO 410 I=1,NHULL
          II=2*(I-1) + 1
          Y3(I)=YTEMP(II)
          Y4(I)=YTEMP(II+1)
  410   CONTINUE
      ENDIF
!
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DP2DCH--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NHULL
 9014   FORMAT('NHULL = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,NHULL
          WRITE(ICOUT,9016)I,Y3(I),Y4(I)
 9016     FORMAT('I,Y3(I),Y4(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DP2DCH
      SUBROUTINE DP1KS3(ICASPL,IDIST,NUMSHA,IFORSW,IKSCVM,IGOFFS,   &
                        IGOFFM,PID,IVARID,IVARI2,NREPL,   &
                        N,XMEAN,XSD,XMIN,XMAX,   &
                        A,B,MINMAX,   &
                        SHAPE1,SHAPE2,SHAPE3,SHAPE4,   &
                        SHAPE5,SHAPE6,SHAPE7,   &
                        KSLOC,KSSCAL,ICAPSW,ICAPTY,IRTFFF,IRTFFP,   &
                        STATVA,PVAL,CDF1,CDF2,CDF3,YSTAT,NMCSAM,NCNT,   &
                        XTEMP,MAXNXT,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--PRINT THE OUTPUT FOR THE KOLMOGOROV-SMIRNOV TEST
!              (UNCENSORED, UNGROUPED CASE) IN ASCII, HTML, LATEX,
!              OR RTF FORMAT
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!         --DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2009/9
!     ORIGINAL VERSION--SEPTEMBER 2009.
!     UPDATED         --AUGUST    2010. IF IKSCVM IS "NONE", OMIT
!                                       CERTAIN PARTS OF PRINT OUT
!     UPDATED         --JUNE      2011. IF IGOFFM = NULL, NO P-VALUES
!                                       OR CRITICAL VALUES
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      REAL PID(*)
      REAL YSTAT(*)
      REAL XTEMP(*)
!
      CHARACTER*4 IVARID(*)
      CHARACTER*4 IVARI2(*)
!
      CHARACTER*4 ICASPL
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 IKSCVM
      CHARACTER*4 IGOFFS
      CHARACTER*4 IGOFFM
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IWRITE
      CHARACTER*4 IERROR
!
      CHARACTER*60 IDIST
      CHARACTER*40 IRTFFF
      CHARACTER*40 IRTFFP
!
      CHARACTER*4 IRTFMD
      COMMON/COMRTF/IRTFMD
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      REAL KSLOC
      REAL KSSCAL
!
!---------------------------------------------------------------------
!
      REAL CV90(40)
      REAL CV95(40)
      REAL CV99(40)
!
      PARAMETER (NUMALP=8)
      REAL ALPHA(NUMALP)
!
!CCCC INCLUDE 'DPCOST.INC'
!
      CHARACTER*1 IBASLC
      PARAMETER(NUMCLI=4)
      PARAMETER(MAXLIN=2)
      PARAMETER (MAXROW=50)
      CHARACTER*60 ITITLE
      CHARACTER*60  ITITLZ
      CHARACTER*60  ITITL9
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
      INCLUDE 'DPCOP2.INC'
!
      DATA CV90/   &
       0.950,0.776,0.636,0.565,0.509,0.468,0.436,0.410,0.387,0.369,   &
       0.352,0.338,0.325,0.314,0.304,0.295,0.286,0.279,0.271,0.265,   &
       0.259,0.253,0.247,0.242,0.238,0.233,0.229,0.225,0.221,0.218,   &
       0.214,0.211,0.208,0.205,0.202,0.199,0.196,0.194,0.191,0.189/
      DATA CV95/   &
       0.975,0.842,0.708,0.624,0.563,0.519,0.483,0.454,0.430,0.409,   &
       0.391,0.375,0.361,0.349,0.338,0.327,0.318,0.309,0.301,0.294,   &
       0.287,0.281,0.275,0.269,0.264,0.259,0.254,0.250,0.246,0.242,   &
       0.238,0.234,0.231,0.227,0.224,0.221,0.218,0.215,0.213,0.210/
      DATA CV99/   &
       0.995,0.929,0.829,0.734,0.669,0.617,0.576,0.542,0.513,0.489,   &
       0.468,0.449,0.432,0.418,0.404,0.392,0.381,0.371,0.361,0.352,   &
       0.344,0.337,0.330,0.323,0.317,0.311,0.305,0.300,0.295,0.290,   &
       0.285,0.281,0.277,0.273,0.269,0.265,0.262,0.258,0.255,0.252/
!
      DATA ALPHA/   &
       0.0, 50.0, 75.0, 90.0, 95.0, 97.5, 99.0, 99.5/
!
!-----START POINT-----------------------------------------------------
!
!
      ISUBN1='DP1K'
      ISUBN2='S3  '
      IERROR='NO'
      IWRITE='OFF'
      CALL DPCONA(92,IBASLC)
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'1KS3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)
   71   FORMAT('***** AT THE BEGINNING OF DP1KS3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)ICASPL,IDIST,MINMAX
   72   FORMAT('MINMAX,ICASPL,IDIST = ',I5,2X,A4,2X,A60)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,73)N,XMIN,XMAX,XMEAN,XSD
   73   FORMAT('N,XMIN,XMAX,XMEAN,XSD = ',I8,4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *******************************************
!               **   STEP 41--                           **
!               **   WRITE OUT INITIAL HEADER TABLE      **
!               **   FOR NORMAL MLE ESTIMATE             **
!               *******************************************
!
      ISTEPN='41'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'1KS3')   &
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
      ITITLE='Kolmogorov-Smirnov Goodness of Fit Test'
      NCTITL=39
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
      DO 4101 I=1,NREPL
        ICNT=ICNT+1
        ITEXT(ICNT)='Factor Variable  : '
        WRITE(ITEXT(ICNT)(17:17),'(I1)')I
        WRITE(ITEXT(ICNT)(20:23),'(A4)')IVARID(I+1)(1:4)
        WRITE(ITEXT(ICNT)(24:27),'(A4)')IVARI2(I+1)(1:4)
        NCTEXT(ICNT)=27
        AVALUE(ICNT)=PID(I+1)
        IDIGIT(ICNT)=NUMDIG
 4101 CONTINUE
!
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='H0: The distribution fits the data'
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: The distribution does not fit the data'
      NCTEXT(ICNT)=43
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      IEND=46
      DO 4111 I=46,1,-1
        IF(IDIST(I:I).NE.' ')THEN
          IEND=I
          GO TO 4119
        ENDIF
 4111 CONTINUE
      IEND=1
 4119 CONTINUE
      CALL EXTBOU(ICASPL,IBOUND)
!
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)(1:14)='Distribution: '
      ISTRT=15
      ISTOP=15+IEND-1
      ITEXT(ICNT)(ISTRT:ISTOP)=IDIST(1:IEND)
      NCTEXT(ICNT)=ISTOP
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      IF(IBOUND.EQ.0)THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='Location Parameter:'
        NCTEXT(ICNT)=19
        AVALUE(ICNT)=KSLOC
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='Scale Parameter:'
        NCTEXT(ICNT)=16
        AVALUE(ICNT)=KSSCAL
        IDIGIT(ICNT)=NUMDIG
      ELSE
        ICNT=ICNT+1
        ITEXT(ICNT)='Lower Limit Parameter:'
        NCTEXT(ICNT)=22
        AVALUE(ICNT)=A
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='Upper Limit Parameter:'
        NCTEXT(ICNT)=22
        AVALUE(ICNT)=B
        IDIGIT(ICNT)=NUMDIG
      ENDIF
      IF(NUMSHA.GE.1)THEN
        DO 4140 I=1,NUMSHA
          ICNT=ICNT+1
          ITEXT(ICNT)='Shape Parameter  :'
          WRITE(ITEXT(ICNT)(17:17),'(I1)')I
          NCTEXT(ICNT)=18
          IF(I.EQ.1)THEN
            AVALUE(ICNT)=SHAPE1
          ELSEIF(I.EQ.2)THEN
            AVALUE(ICNT)=SHAPE2
          ELSEIF(I.EQ.3)THEN
            AVALUE(ICNT)=SHAPE3
          ELSEIF(I.EQ.4)THEN
            AVALUE(ICNT)=SHAPE4
          ELSEIF(I.EQ.5)THEN
            AVALUE(ICNT)=SHAPE5
          ELSEIF(I.EQ.6)THEN
            AVALUE(ICNT)=SHAPE6
          ELSEIF(I.EQ.7)THEN
            AVALUE(ICNT)=SHAPE7
          ENDIF
          IDIGIT(ICNT)=NUMDIG
 4140   CONTINUE
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
      AVALUE(ICNT)=XMIN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Maximum:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=XMAX
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Mean:'
      NCTEXT(ICNT)=12
      AVALUE(ICNT)=XMEAN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample SD:'
      NCTEXT(ICNT)=10
      AVALUE(ICNT)=XSD
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Kolmogorov-Smirnov Test Statistic Value:'
      NCTEXT(ICNT)=40
      AVALUE(ICNT)=STATVA
      IDIGIT(ICNT)=NUMDIG
      IF(IKSCVM.EQ.'SIMU' .AND. IGOFFM.NE.'NULL')THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='Number of Monte Carlo Simulations:'
        NCTEXT(ICNT)=34
        AVALUE(ICNT)=REAL(NMCSAM)
        IDIGIT(ICNT)=NUMDIG
        IF(NMCSAM.NE.NCNT)THEN
          ICNT=ICNT+1
          ITEXT(ICNT)='Number of Samples Rejected:'
          NCTEXT(ICNT)=27
          AVALUE(ICNT)=REAL(NMCSAM-NCNT)
          IDIGIT(ICNT)=NUMDIG
        ENDIF
        ICNT=ICNT+1
        ITEXT(ICNT)='CDF Value:'
        NCTEXT(ICNT)=10
        STACDF=1.0 - PVAL
        AVALUE(ICNT)=STACDF
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='P-Value:'
        NCTEXT(ICNT)=7
        AVALUE(ICNT)=PVAL
        IDIGIT(ICNT)=NUMDIG
      ENDIF
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
      ITITLZ=' '
      NCTITZ=0
      IF(IKSCVM.NE.'NONE' .AND. IGOFFM.NE.'NULL')THEN
        IF(IGOFFS.EQ.'ON')THEN
          ITITLZ='(Fully Specified Model)'
          NCTITZ=23
        ELSE
          ITITLZ='(Parameters Estimated from the Data)'
          NCTITZ=36
        ENDIF
      ENDIF
                                                                                                                                  
      IFRST=.TRUE.
      IF(IKSCVM.EQ.'SIMU')THEN
        ILAST=.TRUE.
      ELSEIF(IKSCVM.EQ.'NONE' .OR. IGOFFM.EQ.'NULL')THEN
        ILAST=.TRUE.
      ELSE
        ILAST=.FALSE.
      ENDIF
      NCTITZ=0
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                  AVALUE,IDIGIT,   &
                  NTOT,NUMROW,   &
                  ICAPSW,ICAPTY,ILAST,IFRST,   &
                  ISUBRO,IBUGA3,IERROR)
      ITITLE=' '
      NCTITL=0
      ITITL9=' '
      NCTIT9=0
!
      IF(IKSCVM.EQ.'NONE')GO TO 9000
      IF(IGOFFM.EQ.'NONE')GO TO 9000
      IF(IKSCVM.EQ.'SIMU')THEN
        IF(IGOFFS.EQ.'ON')THEN
          ITITL9='(Fully Specified Model)'
          NCTIT9=23
        ELSE
          ITITL9='(Parameters Estimated from the Data)'
          NCTIT9=36
        ENDIF
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
        DO 2521 I=1,NUMCOL
          VALIGN(I)='b'
          ALIGN(I)='r'
          NTOT(I)=15
          IF(I.EQ.2)NTOT(I)=5
          NMAX=NMAX+NTOT(I)
          IDIGIT(I)=NUMDIG
          ITYPCO(I)='NUME'
 2521   CONTINUE
        ITYPCO(2)='ALPH'
        IDIGIT(1)=1
        IDIGIT(3)=3
        DO 2523 I=1,NUMROW
          DO 2525 J=1,NUMCOL
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
              IF(I.GE.2)THEN
                P100=ALPHA(I)
                CALL PERCEN(P100,YSTAT,NCNT,IWRITE,XTEMP,MAXNXT,   &
                            XPERC,IBUGA3,IERROR)
                XPERC2=RND(XPERC,3)
                AMAT(I,J)=XPERC2
              ENDIF
            ENDIF
 2525     CONTINUE
 2523   CONTINUE
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
        CALL DPDTA4(ITITL9,NCTIT9,   &
                    ITITLE,NCTITL,ITITL2,NCTIT2,   &
                    MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                    IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,   &
                    IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                    ICAPSW,ICAPTY,IFRST,ILAST,   &
                    ISUBRO,IBUGA3,IERROR)
!
        CDF1=AMAT(4,3)
        CDF2=AMAT(5,3)
        CDF3=AMAT(7,3)
!
        ITITL9=' '
        NCTIT9=0
        ITITLE='Conclusions (Upper 1-Tailed Test)'
        NCTITL=33
        NUMLIN=1
        NUMROW=3
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
        DO 2821 I=1,NUMCOL
          VALIGN(I)='b'
          ALIGN(I)='r'
          NTOT(I)=15
          IF(I.EQ.1 .OR. I.EQ.2)NTOT(I)=7
          IF(I.EQ.3)NTOT(I)=17
          NMAX=NMAX+NTOT(I)
!CCCC     IDIGIT(I)=NUMDIG
          IDIGIT(I)=3
          ITYPCO(I)='ALPH'
 2821   CONTINUE
        ITYPCO(3)='NUME'
        IDIGIT(1)=0
        IDIGIT(2)=0
        DO 2823 I=1,NUMROW
          DO 2825 J=1,NUMCOL
            NCVALU(I,J)=0
            IVALUE(I,J)=' '
            NCVALU(I,J)=0
            AMAT(I,J)=0.0
 2825     CONTINUE
 2823   CONTINUE
        IVALUE(1,1)='10%'
        IVALUE(2,1)='5%'
        IVALUE(3,1)='1%'
        IVALUE(1,2)='90%'
        IVALUE(2,2)='95%'
        IVALUE(3,2)='99%'
        NCVALU(1,1)=3
        NCVALU(2,1)=2
        NCVALU(3,1)=2
        NCVALU(1,2)=3
        NCVALU(2,2)=3
        NCVALU(3,2)=3
        IVALUE(1,4)='Accept H0'
        IVALUE(2,4)='Accept H0'
        IVALUE(3,4)='Accept H0'
        NCVALU(1,4)=9
        NCVALU(2,4)=9
        NCVALU(3,4)=9
        IF(STATVA.GT.CDF1)IVALUE(1,4)='Reject H0'
        IF(STATVA.GT.CDF2)IVALUE(2,4)='Reject H0'
        IF(STATVA.GT.CDF3)IVALUE(3,4)='Reject H0'
        AMAT(1,3)=RND(CDF1,IDIGIT(3))
        AMAT(2,3)=RND(CDF2,IDIGIT(3))
        AMAT(3,3)=RND(CDF3,IDIGIT(3))
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
!       FOR LATEX, WE WANT TO ENSURE THAT TRAILING LINE IS PART
!       OF THE TABLE SO THAT IT WILL BE PRINTED IN THE PROPER PLACE.
!
        IF(ICAPTY.EQ.'LATE')THEN
          ILAST=.FALSE.
        ELSE
          ILAST=.TRUE.
        ENDIF
!
        CALL DPDTA4(ITITL9,NCTIT9,   &
                    ITITLE,NCTITL,ITITL2,NCTIT2,   &
                    MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                    IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,   &
                    IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                    ICAPSW,ICAPTY,IFRST,ILAST,   &
                    ISUBRO,IBUGA3,IERROR)
!
        IF(IPRINT.EQ.'ON')THEN
!
        ITITLE(1:26)='*Critical Values Based on '
        WRITE(ITITLE(27:34),'(I8)')NCNT
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
          IRTFMD='OFF'
          IPTSZ=14
          WRITE(ICOUT,8199)IBASLC,IPTSZ
 8199     FORMAT(A1,'fs',I2)
          CALL DPWRST(ICOUT,'WRIT')
          IF(IRTFFF.EQ.'Courier New')THEN
            ITEMP=1
          ELSEIF(IRTFFF.EQ.'Lucida Console')THEN
            ITEMP=8
          ENDIF
          WRITE(ICOUT,8301)IBASLC,ITEMP
          CALL DPWRST(ICOUT,'WRIT')
          CALL DPRTFZ(ITITLE,NCTITL,CPUMIN,NUMDIG)
!CCCC     CALL DPRTF6(NHEAD)
!CCCC     CALL DPRTF6(NHEAD)
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
 8301     FORMAT(A1,'f',I1)
          CALL DPWRST(ICOUT,'WRIT')
!
!         END TABLE AND RESET "ASIS" MODE
!
          IF(IRTFFF.EQ.'Courier New')THEN
            ITEMP=1
          ELSEIF(IRTFFF.EQ.'Lucida Console')THEN
            ITEMP=8
          ENDIF
          WRITE(ICOUT,8091)IBASLC,ITEMP
 8091     FORMAT(A1,'f',I1)
          CALL DPWRST(ICOUT,'WRIT')
!
          CALL DPRTF6(NHEAD)
          CALL DPRTF6(NHEAD)
!
          IRTFMD='VERB'
!
        ELSE
          WRITE(ICOUT,2589)ITITLE(1:58)
 2589     FORMAT(A60)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        ENDIF
!
      ELSE
        ITITL9=' '
        NCTIT9=0
        ITITLE='Conclusions (Upper 1-Tailed Test)'
        NCTITL=33
        NUMLIN=1
        NUMROW=3
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
        DO 2421 I=1,NUMCOL
          VALIGN(I)='b'
          ALIGN(I)='r'
          NTOT(I)=15
          IF(I.EQ.1 .OR. I.EQ.2)NTOT(I)=7
          IF(I.EQ.3)NTOT(I)=17
          NMAX=NMAX+NTOT(I)
!CCCC     IDIGIT(I)=NUMDIG
          IDIGIT(I)=3
          ITYPCO(I)='ALPH'
 2421   CONTINUE
        ITYPCO(3)='NUME'
        IDIGIT(1)=0
        IDIGIT(2)=0
        DO 2423 I=1,NUMROW
          DO 2425 J=1,NUMCOL
            NCVALU(I,J)=0
            IVALUE(I,J)=' '
            NCVALU(I,J)=0
            AMAT(I,J)=0.0
 2425     CONTINUE
 2423   CONTINUE
        IVALUE(1,1)='10%'
        IVALUE(2,1)='5%'
        IVALUE(3,1)='1%'
        IVALUE(1,2)='90%'
        IVALUE(2,2)='95%'
        IVALUE(3,2)='99%'
        NCVALU(1,1)=3
        NCVALU(2,1)=2
        NCVALU(3,1)=2
        NCVALU(1,2)=3
        NCVALU(2,2)=3
        NCVALU(3,2)=3
        IVALUE(1,4)='Accept H0'
        IVALUE(2,4)='Accept H0'
        IVALUE(3,4)='Accept H0'
        NCVALU(1,4)=9
        NCVALU(2,4)=9
        NCVALU(3,4)=9
        IF(N.LE.40)THEN
          CDF1=CV90(N)
          CDF2=CV95(N)
          CDF3=CV99(N)
        ELSE
          AN=REAL(N)
          CDF1=1.22/SQRT(REAL(N))
          CDF2=1.36/SQRT(REAL(N))
          CDF3=1.63/SQRT(REAL(N))
          AFACT=SQRT(AN + SQRT(AN/10.))
          CDF1=1.22/AFACT
          CDF2=1.36/AFACT
          CDF3=1.63/AFACT
        ENDIF
        IF(STATVA.GT.CDF1)IVALUE(1,4)='Reject H0'
        IF(STATVA.GT.CDF2)IVALUE(2,4)='Reject H0'
        IF(STATVA.GT.CDF3)IVALUE(3,4)='Reject H0'
        AMAT(1,3)=RND(CDF1,IDIGIT(3))
        AMAT(2,3)=RND(CDF2,IDIGIT(3))
        AMAT(3,3)=RND(CDF3,IDIGIT(3))
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
        ILAST=.TRUE.
!
        CALL DPDTA4(ITITL9,NCTIT9,   &
                    ITITLE,NCTITL,ITITL2,NCTIT2,   &
                    MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                    IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,   &
                    IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                    ICAPSW,ICAPTY,IFRST,ILAST,   &
                    ISUBRO,IBUGA3,IERROR)
!
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'1KS3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DP1KS3--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DP1KS3
      SUBROUTINE DP2KST(Y1,Y2,MAXNXT,   &
                        ICAPSW,IFORSW,   &
                        IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--COMPUTE A 2-SAMPLE KOLMOGOROV-SMIRNOV TEST
!              THAT 2 SAMPLES ARE FROM THE SAME DISTRIBUTION
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--98/12
!     ORIGINAL VERSION--DECEMBER  1998.
!     UPDATED         --JULY      2001. FIXED ALGORITM
!     UPDATED         --MARCH     2011. USE DPPARS AND DPPAR3
!     UPDATED         --MARCH     2011. IF MORE THAN 2 VARIABLES
!                                       SPECIFIED, PERFORM ALL
!                                       PAIRWISE TESTS
!     UPDATED         --JUNE      2016. OPTION TO BASE TEST ON USER
!                                       SPECIFIED NUMBER OF PERCENTILES
!                                       RATHER THAN RAW DATA (I2SNPR)
!     UPDATED         --JUNE      2016. KS AS SYNONYM FOR KOLM SMIR
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASPL
      CHARACTER*4 ICAPSW
      CHARACTER*4 IFORSW
      CHARACTER*4 IBUGA2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      CHARACTER*4 ICASE
      CHARACTER*4 IVARID
      CHARACTER*4 IVARI2
      CHARACTER*4 IVARI3
      CHARACTER*4 IVARI4
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
      CHARACTER*4 IFLAGU
      LOGICAL IFRST
      LOGICAL ILAST
!
!---------------------------------------------------------------------
!
      DIMENSION Y1(*)
      DIMENSION Y2(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
!
      REAL YCOMB(2*MAXOBV)
      REAL TEMP1(MAXOBV)
      REAL TEMP2(MAXOBV)
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOS2.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOF2.INC'
!
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(IGARB1),YCOMB(1))
      EQUIVALENCE (GARBAG(IGARB3),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB4),TEMP2(1))
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
      ISUBN1='DP2K'
      ISUBN2='ST  '
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
!               **************************************************
!               **  TREAT THE KOLMOGOROV-SMIRNOV 2 SAMPLE CASE  **
!               **************************************************
!
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'2KST')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DP2KST--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASPL,MAXNXT
   52   FORMAT('ICASPL,MAXNXT = ',A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGA2,IBUGA3,IBUGQ,ISUBRO
   53   FORMAT('IBUGA2,IBUGA3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ***************************
!               **  STEP 1--             **
!               **  EXTRACT THE COMMAND  **
!               ***************************
!
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'2KST')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!  RECOGNIZE THE FOLLOWING FORMS FOR THE COMMAND:
!     KOLMOGOROV SMIRNOV 2 SAMPLE TEST Y1 Y2
!     KOLMOGOROV SMIRNOV TWO SAMPLE TEST Y1 Y2
!     2 SAMPLE KOLMOGOROV SMIRNOV TEST Y1 Y2
!     TWO SAMPLE KOLMOGOROV SMIRNOV TEST Y1 Y2
!  THE WORD TEST IS OPTIONAL.
!
      IF(ICOM.EQ.'KOLM')THEN
        IF(NUMARG.GE.3.AND.IHARG(1).EQ.'SMIR'.AND.   &
           IHARG(2).EQ.'2'.AND.IHARG(3).EQ.'SAMP')THEN
           ISHIFT=3
           GO TO 112
        ELSEIF(NUMARG.GE.3.AND.IHARG(1).EQ.'SMIR'.AND.   &
           IHARG(2).EQ.'TWO'.AND.IHARG(3).EQ.'SAMP')THEN
           ISHIFT=3
           GO TO 112
        ENDIF
      ELSEIF(ICOM.EQ.'KS')THEN
        IF(NUMARG.GE.2.AND.IHARG(1).EQ.'2   '.AND.   &
           IHARG(2).EQ.'SAMP')THEN
           ISHIFT=2
           GO TO 112
        ELSEIF(NUMARG.GE.2.AND.IHARG(1).EQ.'TWO '.AND.   &
           IHARG(2).EQ.'SAMP')THEN
           ISHIFT=2
           GO TO 112
        ENDIF
      ELSEIF(ICOM.EQ.'2'.OR.ICOM.EQ.'TWO')THEN
        IF(NUMARG.GE.3.AND.IHARG(1).EQ.'SAMP'.AND.   &
           IHARG(2).EQ.'KOLM'.AND.IHARG(3).EQ.'SMIR')THEN
           ISHIFT=3
           GO TO 112
        ELSEIF(NUMARG.GE.2.AND.IHARG(1).EQ.'SAMP'.AND.   &
           IHARG(2).EQ.'KOLM')THEN
           ISHIFT=2
           GO TO 112
        ENDIF
      ENDIF
!
! ----------NO MATCH FOUND----------
!
      ICASPL='    '
      IFOUND='NO'
      GO TO 9000
!
  112 CONTINUE
      ICASPL='2KST'
      CALL ADJUST(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      IFOUND='YES'
      IF(IHARG(1).EQ.'TEST')THEN
        ISHIFT=1
        CALL ADJUST(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      ENDIF
!
!               ****************************************
!               **  STEP 2--                          **
!               **  EXTRACT THE VARIABLE LIST         **
!               ****************************************
!
      ISTEPN='2'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'2KST')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      INAME='TWO SAMPLE KOLMOGOROV SMIRNOV TEST'
      MINNA=1
      MAXNA=100
      MINN2=3
      IFLAGE=0
      IFLAGM=1
      MINNVA=2
      MAXNVA=MAXSPN
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'2KST')THEN
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
!               *****************************************
!               **  STEP 3A--                          **
!               **  CASE 1: TWO RESPONSE VARIABLES     **
!               **          WITH NO REPLICATION        **
!               *****************************************
!
      ISTEPN='3A'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'2KST')   &
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMVA2=1
      DO 5210 I=1,NUMVAR
        DO 5220 J=I+1,NUMVAR
          ICOL=I
          CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                      INAME,IVARN1,IVARN2,IVARTY,   &
                      ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                      MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                      MAXCP4,MAXCP5,MAXCP6,   &
                      V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                      Y1,Y1,Y1,NS1,NLOCA2,NLOCA3,ICASE,   &
                      IBUGA3,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
          ICOL=J
          CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                      INAME,IVARN1,IVARN2,IVARTY,   &
                      ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                      MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                      MAXCP4,MAXCP5,MAXCP6,   &
                      V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                      Y2,Y2,Y2,NS2,NLOCA2,NLOCA3,ICASE,   &
                      IBUGA3,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
!               *****************************************
!               **  STEP 52--                          **
!               **  PERFORM 2-SAMPLE K-S TEST          **
!               *****************************************
!
          ISTEPN='52'
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'2KST')THEN
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5211)
 5211       FORMAT('***** FROM DP2KST, BEFORE CALL DP2KS2--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5212)I,J,NS1,NS2,MAXN
 5212       FORMAT('I,J,NS1,NS2,MAXN = ',5I8)
            CALL DPWRST('XXX','BUG ')
            DO 5215 II=1,MAX(NS1,NS2)
              WRITE(ICOUT,5216)II,Y1(II),Y2(II)
 5216         FORMAT('I,Y(I),X(I) = ',I8,2G15.7)
              CALL DPWRST('XXX','BUG ')
 5215       CONTINUE
          ENDIF
!
          IVARID=IVARN1(I)
          IVARI2=IVARN2(I)
          IVARI3=IVARN1(J)
          IVARI4=IVARN2(J)
          CALL DP2KS2(Y1,Y2,NS1,NS2,YCOMB,TEMP1,TEMP2,   &
                      ICAPSW,ICAPTY,IFORSW,I2SNPR,MAXOBV,   &
                      IVARID,IVARI2,IVARI3,IVARI4,   &
                      STATVA,STATCD,CUTU90,CUTU95,CUTU99,   &
                      ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
!               ***************************************
!               **  STEP 8C--                        **
!               **  UPDATE INTERNAL DATAPLOT TABLES  **
!               ***************************************
!
          ISTEPN='8C'
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'2KST')   &
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
          IF(NUMVAR.GT.2)THEN
            IFLAGU='FILE'
          ELSE
            IFLAGU='ON'
          ENDIF
          IFRST=.FALSE.
          ILAST=.FALSE.
          IF(I.EQ.1 .AND. J.EQ.2)IFRST=.TRUE.
          IF(I.EQ.NUMVAR .AND. J.EQ.NUMVAR)ILAST=.TRUE.
          PVAL=CPUMIN
          CUT0=CPUMIN
          CUT50=CPUMIN
          CUT75=CPUMIN
          CUT975=CPUMIN
          CUT999=CPUMIN
          CALL DPFRT5(STATVA,STATCD,PVAL,   &
                      CUT0,CUT50,CUT75,CUT90,CUT95,   &
                      CUT975,CUT99,CUT999,   &
                      IFLAGU,IFRST,ILAST,   &
                      IBUGA2,IBUGA3,ISUBRO,IERROR)
 5220   CONTINUE
 5210 CONTINUE
!
!               ***************************************
!               **  STEP 7--                         **
!               **  UPDATE INTERNAL DATAPLOT TABLES  **
!               ***************************************
!
      ISTEPN='7'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'2KST')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISUBN0='DP2K'
!
      IH='STAT'
      IH2='VAL '
      VALUE0=STATVA
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA3,IERROR)
!
!CCCC IH='STAT'
!CCCC IH2='CDF '
!CCCC VALUE0=STATCD
!CCCC CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
!CCCC1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
!CCCC1IANS,IWIDTH,IBUGA3,IERROR)
!
      IH='CUTU'
      IH2='PP90'
      VALUE0=CUTU90
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA3,IERROR)
!
      IH='CUTU'
      IH2='PP95'
      VALUE0=CUTU95
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA3,IERROR)
!
      IH='CUTU'
      IH2='PP99'
      VALUE0=CUTU99
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA3,IERROR)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'2KST')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DP2KST--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR,ICASPL
 9012   FORMAT('IFOUND,IERROR,ICASPL = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DP2KST
      SUBROUTINE DP2KS2(Y1,Y2,N1,N2,YCOMB,TEMP1,TEMP2,   &
                        ICAPSW,ICAPTY,IFORSW,I2SNPR,MAXNXT,   &
                        IVARID,IVARI2,IVARI3,IVARI4,   &
                        STATVA,STATCD,CUTU90,CUTU95,CUTU99,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--COMPUTE A 2-SAMPLE KOLMOGOROV-SMIRNOV TEST
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--98/11
!     ORIGINAL VERSION--DECEMBER  1998.
!     UPDATED         --JULY      2001. FIX ALGORITHM
!     UPDATED         --DECEMBER  2003. BASE CRITICAL VALUES ON
!                                       CONOVER TABLES
!     UPDATED         --MARCH     2011. USE DPDTA1, DPDTA5 TO PRINT
!                                       OUTPUT
!     UPDATED         --JUNE      2016. OPTION TO BASE TEST ON USER
!                                       SPECIFIED NUMBER OF PERCENTILES
!                                       RATHER THAN RAW DATA (I2SNPR)
!     UPDATED         --AUGUST    2019. CALL LIST TO KENTAU
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 IVARID
      CHARACTER*4 IVARI2
      CHARACTER*4 IVARI3
      CHARACTER*4 IVARI4
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IWRITE
!
!---------------------------------------------------------------------
!
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION YCOMB(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
!
      PARAMETER (NUMALP=3)
      REAL ALPHA(NUMALP)
!
      PARAMETER(NUMCLI=5)
      PARAMETER(MAXLIN=3)
      PARAMETER (MAXROW=NUMALP)
      PARAMETER (MAXRO2=30)
      CHARACTER*60 ITITLE
      CHARACTER*60 ITITLZ
      CHARACTER*1  ITITL9
      CHARACTER*60 ITEXT(MAXRO2)
      CHARACTER*4  ALIGN(NUMCLI)
      CHARACTER*4  VALIGN(NUMCLI)
      REAL         AVALUE(MAXRO2)
      INTEGER      NCTEXT(MAXRO2)
      INTEGER      IDIGIT(MAXRO2)
      INTEGER      NTOT(MAXRO2)
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
      LOGICAL IFLAGS
      LOGICAL IFLAGE
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      DATA ALPHA/90.0, 95.0, 99.0/
!
!
      ISUBN1='DP2K'
      ISUBN2='S2  '
      IWRITE='OFF'
      IERROR='NO'
!
!     2016/06: IF REQUESTED, REPLACE RAW DATA WITH SPECIFIED
!              NUMBER OF PERCENTILES.  THIS OPTION CAN BE USEFUL
!              FOR LARGE DATA SETS.
!
      IF(I2SNPR.GT.0 .AND. I2SNPR.LT.MIN(N1,N2))THEN
        CALL PERCE2(I2SNPR,Y1,N1,IWRITE,TEMP2,MAXNXT,TEMP1,   &
                      IBUGA3,ISUBRO,IERROR)
        DO 110 I=1,I2SNPR
          Y1(I)=TEMP1(I)
  110   CONTINUE
        N1=I2SNPR
        CALL PERCE2(I2SNPR,Y2,N2,IWRITE,TEMP2,MAXNXT,TEMP1,   &
                      IBUGA3,ISUBRO,IERROR)
        DO 120 I=1,I2SNPR
          Y2(I)=TEMP1(I)
  120   CONTINUE
        N2=I2SNPR
      ENDIF
!
      CALL DP2KS3(Y1,Y2,N1,N2,YCOMB,   &
                  STATVA,STATCD,CUTU90,CUTU95,CUTU99,   &
                  ISUBRO,IBUGA3,IERROR)
!
      Y1MIN=Y1(1)
      Y1MAX=Y1(N1)
      CALL MEAN(Y1,N1,IWRITE,Y1MEAN,IBUGA3,IERROR)
      CALL SD(Y1,N1,IWRITE,Y1SD,IBUGA3,IERROR)
      Y2MIN=Y2(1)
      Y2MAX=Y2(N2)
      CALL MEAN(Y2,N2,IWRITE,Y2MEAN,IBUGA3,IERROR)
      CALL SD(Y2,N2,IWRITE,Y2SD,IBUGA3,IERROR)
!
!               *************************************************
!               **   STEP 32--                                 **
!               **   WRITE OUT EVERYTHING                      **
!               **   FOR A TWO SAMPLE KOLMOGOROV-SMIRNOV TEST  **
!               *************************************************
!
      ISTEPN='32'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'2KS2')   &
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
      ITITLE='Kolmogorov-Smirnov Two Sample Test'
      NCTITL=34
      ITITLZ=' '
      NCTITZ=0
!
      ICNT=1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='First Response Variable:  '
      WRITE(ITEXT(ICNT)(27:30),'(A4)')IVARID(1:4)
      WRITE(ITEXT(ICNT)(31:34),'(A4)')IVARI2(1:4)
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Second Response Variable: '
      WRITE(ITEXT(ICNT)(27:30),'(A4)')IVARI3(1:4)
      WRITE(ITEXT(ICNT)(31:34),'(A4)')IVARI4(1:4)
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='H0: The Two Samples Come From the'
      NCTEXT(ICNT)=33
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='    Same (Unspecified) Distribution'
      NCTEXT(ICNT)=35
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: The Two Samples Come From'
      NCTEXT(ICNT)=29
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='    Different Distributions'
      NCTEXT(ICNT)=27
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample One Summary Statistics:'
      NCTEXT(ICNT)=30
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Observations:'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=REAL(N1)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Mean:'
      NCTEXT(ICNT)=12
      AVALUE(ICNT)=Y1MEAN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Standard Deviation:'
      NCTEXT(ICNT)=26
      AVALUE(ICNT)=Y1SD
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Minimum:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=Y1MIN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Maximum:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=Y1MAX
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Two Summary Statistics:'
      NCTEXT(ICNT)=30
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Observations:'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=REAL(N2)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Mean:'
      NCTEXT(ICNT)=12
      AVALUE(ICNT)=Y2MEAN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Standard Deviation:'
      NCTEXT(ICNT)=26
      AVALUE(ICNT)=Y2SD
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Minimum:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=Y2MIN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Maximum:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=Y2MAX
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Test Statistic Value:'
      NCTEXT(ICNT)=21
      AVALUE(ICNT)=STATVA
      IDIGIT(ICNT)=NUMDIG
!
!CCCC ICNT=ICNT+1
!CCCC ITEXT(ICNT)='Test Statistic Standard Error:'
!CCCC NCTEXT(ICNT)=30
!CCCC AVALUE(ICNT)=DSD
!CCCC IDIGIT(ICNT)=NUMDIG
!
      NUMROW=ICNT
      DO 5010 I=1,NUMROW
        NTOT(I)=15
 5010 CONTINUE
!
      IFRST=.TRUE.
      ILAST=.TRUE.
!
      ISTEPN='42A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ADK2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                  AVALUE,IDIGIT,   &
                  NTOT,NUMROW,   &
                  ICAPSW,ICAPTY,ILAST,IFRST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      ISTEPN='42D'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ADK2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ITITL9=' '
      NCTIT9=0
      ITITLE='Conclusions (Upper 1-Tailed Test)'
      NCTITL=33
!
      DO 5030 J=1,5
        DO 5040 I=1,3
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
 5040   CONTINUE
 5030 CONTINUE
!
      ITITL2(2,1)='Null'
      NCTIT2(2,1)=4
      ITITL2(3,1)='Hypothesis'
      NCTIT2(3,1)=10
!
      ITITL2(2,2)='Significance'
      NCTIT2(2,2)=12
      ITITL2(3,2)='Level'
      NCTIT2(3,2)=5
!
      ITITL2(2,3)='Test '
      NCTIT2(2,3)=4
      ITITL2(3,3)='Statistic'
      NCTIT2(3,3)=9
!
      ITITL2(2,4)='Critical'
      NCTIT2(2,4)=8
      ITITL2(3,4)='Region (>=)'
      NCTIT2(3,4)=11
!
      ITITL2(1,5)='Null'
      NCTIT2(1,5)=4
      ITITL2(2,5)='Hypothesis'
      NCTIT2(2,5)=10
      ITITL2(3,5)='Conclusion'
      NCTIT2(3,5)=10
!
      NMAX=0
      NUMCOL=5
      DO 5050 I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IF(I.EQ.1)NTOT(I)=12
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
        IDIGIT(I)=NUMDIG
        IF(I.EQ.1 .OR. I.EQ.2 .OR. I.EQ.5)THEN
          ITYPCO(I)='ALPH'
        ENDIF
        IWHTML(1)=150
        IWHTML(2)=125
        IWHTML(3)=150
        IWHTML(4)=150
        IWHTML(5)=150
        IINC=1600
        IINC2=1400
        IINC3=2200
        IWRTF(1)=IINC
        IWRTF(2)=IWRTF(1)+IINC
        IWRTF(3)=IWRTF(2)+IINC2
        IWRTF(4)=IWRTF(3)+IINC
        IWRTF(5)=IWRTF(4)+IINC
!
        DO 5060 J=1,NUMALP
!
          IVALUE(J,1)='Same'
          NCVALU(J,1)=5
          AMAT(J,3)=STATVA
          IF(J.EQ.1)THEN
            AMAT(J,4)=CUTU90
          ELSEIF(J.EQ.2)THEN
            AMAT(J,4)=CUTU95
          ELSEIF(J.EQ.3)THEN
            AMAT(J,4)=CUTU99
          ENDIF
          IVALUE(J,5)(1:6)='REJECT'
          IF(STATVA.LT.AMAT(J,4))THEN
            IVALUE(J,5)(1:6)='ACCEPT'
          ENDIF
          NCVALU(J,5)=6
!
          ALPHAT=ALPHA(J)
          ALPHAT=ALPHAT
          WRITE(IVALUE(J,2)(1:4),'(F4.1)')ALPHAT
          IVALUE(J,2)(5:5)='%'
          NCVALU(J,2)=5
 5060   CONTINUE
!
 5050 CONTINUE
!
      ICNT=NUMALP
      NUMLIN=3
      NUMCOL=5
      IFRST=.TRUE.
      ILAST=.TRUE.
      IFLAGS=.TRUE.
      IFLAGE=.TRUE.
      CALL DPDTA5(ITITLE,NCTITL,   &
                  ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                  MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                  IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'2KS2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DP2KS2--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DP2KS2
      SUBROUTINE DP2KS3(Y1,Y2,N1,N2,YCOMB,   &
                        STATVA,STATCD,CUTU90,CUTU95,CUTU99,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--COMPUTE A 2-SAMPLE KOLMOGOROV-SMIRNOV TEST
!
!              THIS WAS EXTRACTED FROM THE DP2KS2 ROUTINE IN
!              ORDER TO ALLOW IT TO BE COMPUTED AS A SEPARATE
!              STATISTC:
!
!                 LET A = TWO SAMPLE KOLM SMIR TEST Y1 Y2
!                 LET A = TWO SAMPLE KOLM SMIR CRITICAL VALUE Y1 Y2
!
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2011/3
!     ORIGINAL VERSION--MARCH     2011. EXTRACTED FROM DP2KS2
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION YCOMB(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!
      ISUBN1='DP2K'
      ISUBN2='S3  '
!
      IERROR='NO'
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      NMIN=MIN(N1,N2)
      NTOT=N1+N2
!
      IF(NMIN.LT.2)THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
   31   FORMAT('***** ERROR IN TWO SAMPLE KOLMOGOROV-SMIRNOV TEST--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,32)
   32   FORMAT('      THE NUMBER OF OBSERVATIONS FOR EACH VARIABLE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,33)
   33   FORMAT('      MUST BE AT LEAST 2;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,34)N1
   34   FORMAT('      THE NUMBER OF OBSERVATIONS FOR VARIABLE 1 = ',I6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,35)N2
   35   FORMAT('      THE NUMBER OF OBSERVATIONS FOR VARIABLE 2 = ',I6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'2KS3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)
   71   FORMAT('***** AT THE BEGINNING OF DP2KS3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)N1,N2
   72   FORMAT('N1,N2 = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO 85 I=1,MAX(N1,N2)
          WRITE(ICOUT,86)I,Y1(I),Y2(I)
   86     FORMAT('I,Y1(I),Y2(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   85   CONTINUE
      ENDIF
!
!               **************************************
!               **  STEP 4--                        **
!               **  COMPUTE THE EMPIRICAL CDF       **
!               **  FUNCTIONS                       **
!               **************************************
!
!  FOR K-S TEST, ONLY UNBINNED DATA SUPPORTED.
!
      DO 210 I=1,N1
        YCOMB(I)=Y1(I)
  210 CONTINUE
      DO 220 I=1,N2
        YCOMB(I+N1)=Y2(I)
  220 CONTINUE
!
      CALL SORT(YCOMB,NTOT,YCOMB)
      CALL SORT(Y1,N1,Y1)
      CALL SORT(Y2,N2,Y2)
      AN1=REAL(N1)
      AN2=REAL(N2)
      ANTOT=REAL(NTOT)
!
      D=0.0
!
      DO 910 I=1,NTOT
        IFREQ=0
        DO 920 J=1,N1
          IF(Y1(J).LE.YCOMB(I))THEN
            IFREQ=IFREQ+1
          ELSE
            GO TO 929
          ENDIF
  920   CONTINUE
  929   CONTINUE
        ZY1=REAL(IFREQ)/AN1
        IFREQ=0
        DO 930 J=1,N2
          IF(Y2(J).LE.YCOMB(I))THEN
            IFREQ=IFREQ+1
          ELSE
            GO TO 939
          ENDIF
  930   CONTINUE
  939   CONTINUE
        ZY2=REAL(IFREQ)/AN2
        D=MAX(D,ABS(ZY1-ZY2))
!
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'2KS3')THEN
          WRITE(ICOUT,942)I,ZY1,ZY2,D
  942     FORMAT('I,ZY1,ZY2,D = ',I5,4G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
  910 CONTINUE
!
!               ************************************************
!               **  STEP 4.1--                                **
!               **  COMPUTE KOLMOGOROV-SMIRNOV TEST STATISTIC **
!               **  EXPECTED                                  **
!               ************************************************
!
      STAT=D
!
!  COMPUTE CRITICAL VALUES FOR 0.90, 0.95, AND 0.99.
!  USE FUNCTION FOR N <=100, USE APPROXIMATION FOR N > 100.
!
!  12/2003: BASE CRITICAL VALUES ON CONOVER TABLES.
!
      IF(N1.EQ.N2)THEN
        IF(N1.EQ.3)THEN
          CDF1=2./3.
          CDF2=1.0
          CDF3=1.0
        ELSEIF(N1.EQ.4)THEN
          CDF1=3./4.
          CDF2=3./4.
          CDF3=1.0
        ELSEIF(N1.EQ.5)THEN
          CDF1=3./5.
          CDF2=4./5.
          CDF3=4./5.
        ELSEIF(N1.EQ.6)THEN
          CDF1=4./6.
          CDF2=4./6.
          CDF3=5./6.
        ELSEIF(N1.EQ.7)THEN
          CDF1=4./7.
          CDF2=5./7.
          CDF3=5./7.
        ELSEIF(N1.EQ.8)THEN
          CDF1=4./8.
          CDF2=5./8.
          CDF3=6./8.
        ELSEIF(N1.EQ.9)THEN
          CDF1=5./9.
          CDF2=5./9.
          CDF3=6./9.
        ELSEIF(N1.EQ.10)THEN
          CDF1=5./10.
          CDF2=6./10.
          CDF3=7./10.
        ELSEIF(N1.EQ.11)THEN
          CDF1=5./11.
          CDF2=6./11.
          CDF3=7./11.
        ELSEIF(N1.EQ.12)THEN
          CDF1=5./12.
          CDF2=6./12.
          CDF3=7./12.
        ELSEIF(N1.EQ.13)THEN
          CDF1=6./13.
          CDF2=6./13.
          CDF3=8./13.
        ELSEIF(N1.EQ.14)THEN
          CDF1=6./14.
          CDF2=7./14.
          CDF3=8./14.
        ELSEIF(N1.EQ.15)THEN
          CDF1=6./15.
          CDF2=7./15.
          CDF3=8./15.
        ELSEIF(N1.EQ.16)THEN
          CDF1=6./16.
          CDF2=7./16.
          CDF3=9./16.
        ELSEIF(N1.EQ.17)THEN
          CDF1=7./17.
          CDF2=7./17.
          CDF3=9./17.
        ELSEIF(N1.EQ.18)THEN
          CDF1=7./18.
          CDF2=8./18.
          CDF3=9./18.
        ELSEIF(N1.EQ.19)THEN
          CDF1=7./19.
          CDF2=8./19.
          CDF3=9./19.
        ELSEIF(N1.EQ.20)THEN
          CDF1=7./20.
          CDF2=8./20.
          CDF3=10./20.
        ELSEIF(N1.EQ.21)THEN
          CDF1=7./21.
          CDF2=8./21.
          CDF3=10./21.
        ELSEIF(N1.EQ.22)THEN
          CDF1=8./22.
          CDF2=8./22.
          CDF3=10./22.
        ELSEIF(N1.EQ.22)THEN
          CDF1=8./22.
          CDF2=8./22.
          CDF3=10./22.
        ELSEIF(N1.EQ.23)THEN
          CDF1=8./23.
          CDF2=9./23.
          CDF3=10./23.
        ELSEIF(N1.EQ.24)THEN
          CDF1=8./24.
          CDF2=9./24.
          CDF3=11./24.
        ELSEIF(N1.EQ.25)THEN
          CDF1=8./25.
          CDF2=9./25.
          CDF3=11./25.
        ELSEIF(N1.EQ.26)THEN
          CDF1=8./26.
          CDF2=9./26.
          CDF3=11./26.
        ELSEIF(N1.EQ.27)THEN
          CDF1=8./27.
          CDF2=9./27.
          CDF3=11./27.
        ELSEIF(N1.EQ.28)THEN
          CDF1=9./28.
          CDF2=10./28.
          CDF3=12./28.
        ELSEIF(N1.EQ.29)THEN
          CDF1=9./29.
          CDF2=10./29.
          CDF3=12./29.
        ELSEIF(N1.EQ.30)THEN
          CDF1=9./30.
          CDF2=10./30.
          CDF3=12./30.
        ELSEIF(N1.EQ.31)THEN
          CDF1=9./31.
          CDF2=10./31.
          CDF3=12./31.
        ELSEIF(N1.EQ.32)THEN
          CDF1=9./32.
          CDF2=10./32.
          CDF3=12./32.
        ELSEIF(N1.EQ.33)THEN
          CDF1=9./33.
          CDF2=11./33.
          CDF3=13./33.
        ELSEIF(N1.EQ.34)THEN
          CDF1=10./34.
          CDF2=11./34.
          CDF3=13./34.
        ELSEIF(N1.EQ.35)THEN
          CDF1=10./35.
          CDF2=11./35.
          CDF3=13./35.
        ELSEIF(N1.EQ.36)THEN
          CDF1=10./36.
          CDF2=11./36.
          CDF3=13./36.
        ELSEIF(N1.EQ.37)THEN
          CDF1=10./37.
          CDF2=11./37.
          CDF3=13./37.
        ELSEIF(N1.EQ.38)THEN
          CDF1=10./38.
          CDF2=11./38.
          CDF3=14./38.
        ELSEIF(N1.EQ.39)THEN
          CDF1=10./39.
          CDF2=11./39.
          CDF3=14./39.
        ELSEIF(N1.EQ.40)THEN
          CDF1=10./40.
          CDF2=12./40.
          CDF3=14./40.
        ELSE
          CDF1=1.73/SQRT(REAL(N1))
          CDF2=1.92/SQRT(REAL(N1))
          CDF3=2.30/SQRT(REAL(N1))
        ENDIF
      ELSEIF(N1.NE.N2)THEN
        N1T=MIN(N1,N2)
        N2T=MAX(N1,N2)
        IF(N1T.EQ.1)THEN
          IF(N2T.LE.10)THEN
            CDF1=1.
            CDF2=1.
            CDF3=1.
          ELSE
            CDF1=1.22*SQRT((AN1+AN2)/(AN1*AN2))
            CDF2=1.36*SQRT((AN1+AN2)/(AN1*AN2))
            CDF3=1.63*SQRT((AN1+AN2)/(AN1*AN2))
          ENDIF
        ELSEIF(N1T.EQ.2)THEN
          IF(N2T.LE.4)THEN
            CDF1=1.
            CDF2=1.
            CDF3=1.
          ELSEIF(N2T.EQ.5)THEN
            CDF1=4./5.
            CDF2=1.
            CDF3=1.
          ELSEIF(N2T.EQ.6)THEN
            CDF1=5./6.
            CDF2=1.
            CDF3=1.
          ELSEIF(N2T.EQ.7)THEN
            CDF1=6./7.
            CDF2=1.
            CDF3=1.
          ELSEIF(N2T.EQ.8)THEN
            CDF1=7./8.
            CDF2=7./8.
            CDF3=1.
          ELSEIF(N2T.EQ.9)THEN
            CDF1=8./9.
            CDF2=8./9.
            CDF3=1.
          ELSEIF(N2T.EQ.10)THEN
            CDF1=4./5.
            CDF2=9./10.
            CDF3=1.
          ELSE
            CDF1=1.22*SQRT((AN1+AN2)/(AN1*AN2))
            CDF2=1.36*SQRT((AN1+AN2)/(AN1*AN2))
            CDF3=1.63*SQRT((AN1+AN2)/(AN1*AN2))
          ENDIF
        ELSEIF(N1T.EQ.3)THEN
          IF(N2T.EQ.4)THEN
            CDF1=3./4.
            CDF2=1.
            CDF3=1.
          ELSEIF(N2T.EQ.5)THEN
            CDF1=4./5.
            CDF2=4./5.
            CDF3=1.
          ELSEIF(N2T.EQ.6)THEN
            CDF1=2./3.
            CDF2=5./6.
            CDF3=1.
          ELSEIF(N2T.EQ.7)THEN
            CDF1=5./7.
            CDF2=6./7.
            CDF3=6./7.
          ELSEIF(N2T.EQ.8)THEN
            CDF1=3./4.
            CDF2=3./4.
            CDF3=1.
          ELSEIF(N2T.EQ.9)THEN
            CDF1=2./3.
            CDF2=7./9.
            CDF3=8./9.
          ELSEIF(N2T.EQ.10)THEN
            CDF1=7./10.
            CDF2=4./5.
            CDF3=9./10.
          ELSEIF(N2T.EQ.11)THEN
            CDF1=(7./10. + 2./3.)/2.0
            CDF2=(4./5. + 3./4.)/2.0
            CDF3=(9./10. + 11./12.)/2.0
          ELSEIF(N2T.EQ.12)THEN
            CDF1=2./3.
            CDF2=3./4.
            CDF3=11./12.
          ELSE
            CDF1=1.22*SQRT((AN1+AN2)/(AN1*AN2))
            CDF2=1.36*SQRT((AN1+AN2)/(AN1*AN2))
            CDF3=1.63*SQRT((AN1+AN2)/(AN1*AN2))
          ENDIF
        ELSEIF(N1T.EQ.4)THEN
          IF(N2T.EQ.5)THEN
            CDF1=3./4.
            CDF2=4./5.
            CDF3=1.
          ELSEIF(N2T.EQ.6)THEN
            CDF1=2./3.
            CDF2=3./4.
            CDF3=5./6.
          ELSEIF(N2T.EQ.7)THEN
            CDF1=5./7.
            CDF2=6./7.
            CDF3=6./7.
          ELSEIF(N2T.EQ.8)THEN
            CDF1=5./8.
            CDF2=3./4.
            CDF3=7./8.
          ELSEIF(N2T.EQ.9)THEN
            CDF1=2./3.
            CDF2=3./4.
            CDF3=8./9.
          ELSEIF(N2T.EQ.10)THEN
            CDF1=13./20.
            CDF2=7./10.
            CDF3=4./5.
          ELSEIF(N2T.EQ.11)THEN
            CDF1=(13./20. + 2./3.)/2.0
            CDF2=(7./10. + 2./3.)/2.0
            CDF3=(4./5. + 5./6.)/2.0
          ELSEIF(N2T.EQ.12 .OR. N2T.EQ.13)THEN
            CDF1=2./3.
            CDF2=2./3.
            CDF3=5./6.
          ELSEIF(N2T.GE.14 .AND. N2T.LE.16)THEN
            CDF1=5./8.
            CDF2=11./16.
            CDF3=13./16.
          ELSE
            CDF1=1.22*SQRT((AN1+AN2)/(AN1*AN2))
            CDF2=1.36*SQRT((AN1+AN2)/(AN1*AN2))
            CDF3=1.63*SQRT((AN1+AN2)/(AN1*AN2))
          ENDIF
        ELSEIF(N1T.EQ.5)THEN
          IF(N2T.EQ.6)THEN
            CDF1=2./3.
            CDF2=2./3.
            CDF3=5./6.
          ELSEIF(N2T.EQ.7)THEN
            CDF1=23./35.
            CDF2=5./7.
            CDF3=6./7.
          ELSEIF(N2T.EQ.8)THEN
            CDF1=5./8.
            CDF2=27./40.
            CDF3=4./5.
          ELSEIF(N2T.EQ.9)THEN
            CDF1=3./5.
            CDF2=31./45.
            CDF3=4./5.
          ELSEIF(N2T.GE.10 .AND. N2T.LE.12)THEN
            CDF1=3./5.
            CDF2=7./10.
            CDF3=4./5.
          ELSEIF(N2T.GE.13 .AND. N2T.LE.17)THEN
            CDF1=3./5.
            CDF2=2./3.
            CDF3=11./15.
          ELSEIF(N2T.GE.18 .AND. N2T.LE.20)THEN
            CDF1=11./20.
            CDF2=3./5.
            CDF3=3./4.
          ELSE
            CDF1=1.22*SQRT((AN1+AN2)/(AN1*AN2))
            CDF2=1.36*SQRT((AN1+AN2)/(AN1*AN2))
            CDF3=1.63*SQRT((AN1+AN2)/(AN1*AN2))
          ENDIF
        ELSEIF(N1T.EQ.6)THEN
          IF(N2T.EQ.7)THEN
            CDF1=4./7.
            CDF2=29./42.
            CDF3=5./6.
          ELSEIF(N2T.EQ.8)THEN
            CDF1=7./12.
            CDF2=2./3.
            CDF3=3./4.
          ELSEIF(N2T.EQ.9)THEN
            CDF1=5./9.
            CDF2=2./3.
            CDF3=7./9.
          ELSEIF(N2T.EQ.10)THEN
            CDF1=17./30.
            CDF2=19./30.
            CDF3=11./15.
          ELSEIF(N2T.GE.11 .AND. N2T.LE.14)THEN
            CDF1=7./12.
            CDF2=7./12.
            CDF3=3./4.
          ELSEIF(N2T.GE.15 .AND. N2T.LE.20)THEN
            CDF1=5./9.
            CDF2=11./18.
            CDF3=13./18.
          ELSEIF(N2T.GE.21 .AND. N2T.LE.24)THEN
            CDF1=1./2.
            CDF2=7./12.
            CDF3=2./3.
          ELSE
            CDF1=1.22*SQRT((AN1+AN2)/(AN1*AN2))
            CDF2=1.36*SQRT((AN1+AN2)/(AN1*AN2))
            CDF3=1.63*SQRT((AN1+AN2)/(AN1*AN2))
          ENDIF
        ELSEIF(N1T.EQ.7)THEN
          IF(N2T.EQ.8)THEN
            CDF1=33./56.
            CDF2=5./8.
            CDF3=3./4.
          ELSEIF(N2T.EQ.9)THEN
            CDF1=5./9.
            CDF2=40./63.
            CDF3=47./63.
          ELSEIF(N2T.GE.10 .AND. N2T.LE.11)THEN
            CDF1=39./70.
            CDF2=43./70.
            CDF3=5./7.
          ELSEIF(N2T.GE.12 .AND. N2T.LE.20)THEN
            CDF1=1./2.
            CDF2=4./7.
            CDF3=5./7.
          ELSEIF(N2T.GE.21 .AND. N2T.LE.28)THEN
            CDF1=13./28.
            CDF2=15./28.
            CDF3=9./14.
          ELSE
            CDF1=1.22*SQRT((AN1+AN2)/(AN1*AN2))
            CDF2=1.36*SQRT((AN1+AN2)/(AN1*AN2))
            CDF3=1.63*SQRT((AN1+AN2)/(AN1*AN2))
          ENDIF
        ELSEIF(N1T.EQ.8)THEN
          IF(N2T.EQ.9)THEN
            CDF1=13./24.
            CDF2=5./8.
            CDF3=3./4.
          ELSEIF(N2T.EQ.10)THEN
            CDF1=21./40.
            CDF2=23./40.
            CDF3=7./10.
          ELSEIF(N2T.GE.11 .AND. N2T.LE.13)THEN
            CDF1=1./2.
            CDF2=7./12.
            CDF3=2./3.
          ELSEIF(N2T.GE.14 .AND. N2T.LE.23)THEN
            CDF1=1./2.
            CDF2=9./16.
            CDF3=5./8.
          ELSEIF(N2T.GE.24 .AND. N2T.LE.32)THEN
            CDF1=7./16.
            CDF2=1./2.
            CDF3=19./32.
          ELSE
            CDF1=1.22*SQRT((AN1+AN2)/(AN1*AN2))
            CDF2=1.36*SQRT((AN1+AN2)/(AN1*AN2))
            CDF3=1.63*SQRT((AN1+AN2)/(AN1*AN2))
          ENDIF
        ELSEIF(N1T.EQ.9)THEN
          IF(N2T.EQ.10)THEN
            CDF1=1./2.
            CDF2=26./45.
            CDF3=31./45.
          ELSEIF(N2T.GE.11 .AND. N2T.LE.13)THEN
            CDF1=1./2.
            CDF2=5./9.
            CDF3=2./3.
          ELSEIF(N2T.GE.14 .AND. N2T.LE.16)THEN
            CDF1=22./45.
            CDF2=8./15.
            CDF3=29./45.
          ELSEIF(N2T.GE.17 .AND. N2T.LE.26)THEN
            CDF1=4./9.
            CDF2=1./2.
            CDF3=11./18.
          ELSEIF(N2T.GE.27 .AND. N2T.LE.36)THEN
            CDF1=5./12.
            CDF2=17./36.
            CDF3=5./9.
          ELSE
            CDF1=1.22*SQRT((AN1+AN2)/(AN1*AN2))
            CDF2=1.36*SQRT((AN1+AN2)/(AN1*AN2))
            CDF3=1.63*SQRT((AN1+AN2)/(AN1*AN2))
          ENDIF
        ELSEIF(N1T.EQ.10)THEN
          IF(N2T.GE.11 .AND. N2T.LE.17)THEN
            CDF1=7./15.
            CDF2=1./2.
            CDF3=19./30.
          ELSEIF(N2T.GE.18 .AND. N2T.LE.29)THEN
            CDF1=9./20.
            CDF2=1./2.
            CDF3=3./5.
          ELSEIF(N2T.GE.30 .AND. N2T.LE.40)THEN
            CDF1=2./5.
            CDF2=9./20.
            CDF3=1.63*SQRT((AN1+AN2)/(AN1*AN2))
          ELSE
            CDF1=1.22*SQRT((AN1+AN2)/(AN1*AN2))
            CDF2=1.36*SQRT((AN1+AN2)/(AN1*AN2))
            CDF3=1.63*SQRT((AN1+AN2)/(AN1*AN2))
          ENDIF
        ELSEIF(N1T.GE.11 .AND. N1T.LE.14)THEN
          IF(N2T.GE.12 .AND. N2T.LE.15)THEN
            CDF1=9./20.
            CDF2=1./2.
            CDF3=7./12.
          ELSEIF(N2T.GE.17 .AND. N2T.LE.18)THEN
            CDF1=7./16.
            CDF2=23./48.
            CDF3=7./12.
          ELSEIF(N2T.GE.19 .AND. N2T.LE.20)THEN
            CDF1=5./12.
            CDF2=7./15.
            CDF3=17./30.
          ELSE
            CDF1=1.22*SQRT((AN1+AN2)/(AN1*AN2))
            CDF2=1.36*SQRT((AN1+AN2)/(AN1*AN2))
            CDF3=1.63*SQRT((AN1+AN2)/(AN1*AN2))
          ENDIF
        ELSEIF(N1T.EQ.15)THEN
          IF(N2T.GE.16 .AND. N2T.LE.20)THEN
            CDF1=2./5.
            CDF2=13./30.
            CDF3=31./60.
          ELSE
            CDF1=1.22*SQRT((AN1+AN2)/(AN1*AN2))
            CDF2=1.36*SQRT((AN1+AN2)/(AN1*AN2))
            CDF3=1.63*SQRT((AN1+AN2)/(AN1*AN2))
          ENDIF
        ELSEIF(N1T.EQ.16)THEN
          IF(N2T.GE.17 .AND. N2T.LE.20)THEN
            CDF1=31./80.
            CDF2=17./40.
            CDF3=41./80.
          ELSE
            CDF1=1.22*SQRT((AN1+AN2)/(AN1*AN2))
            CDF2=1.36*SQRT((AN1+AN2)/(AN1*AN2))
            CDF3=1.63*SQRT((AN1+AN2)/(AN1*AN2))
          ENDIF
        ELSE
          CDF1=1.22*SQRT((AN1+AN2)/(AN1*AN2))
          CDF2=1.36*SQRT((AN1+AN2)/(AN1*AN2))
          CDF3=1.63*SQRT((AN1+AN2)/(AN1*AN2))
        ENDIF
      ENDIF
!
      STATVA=STAT
      STATCD=CDF2
      CUTU90=CDF1
      CUTU95=CDF2
      CUTU99=CDF3
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'2KS3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DP2KS3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)STATVA,STATCD,CUTU90,CUTU95,CUTU99
 9013   FORMAT('STATVA,STATCD,CUTU90,CUTU95,CUTU99 = ',5G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!
      RETURN
      END SUBROUTINE DP2KS3
      SUBROUTINE DP2RC2(Y1,Y2,N,ICASA2,   &
                        TEMP1,TEMP2,TEMP3,MAXNXT,   &
                        IVARID,IVARI2,IVARI3,IVARI4,   &
                        ICAPSW,ICAPTY,IFORSW,IRCRTA,   &
                        STATVA,STATCD,PVAL,PVALLT,PVALUT,   &
                        CUTU90,CUTU95,CTU975,CUTU99,CTU995,CTU999,   &
                        CUTL90,CUTL95,CTL975,CUTL99,CTL995,CTL999,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--CARRY OUT A 2-SAMPLE TEST FOR INDEPENDENCE BASED ON
!              THE SPEARMAN RHO RANK CORRELATION
!     EXAMPLE--RANK CORRELATION INDEPENDENCE TEST Y1 Y2
!              LOWER TAILED RANK CORRELATION INDEPENDENCE TEST Y1 Y2
!              UPPER TAILED RANK CORRELATION INDEPENDENCE TEST Y1 Y2
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2013/3
!     ORIGINAL VERSION--MARCH     2013.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 ICASA2
      CHARACTER*4 IFORSW
      CHARACTER*4 IRCRTA
      CHARACTER*4 IVARID
      CHARACTER*4 IVARI2
      CHARACTER*4 IVARI3
      CHARACTER*4 IVARI4
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
      CHARACTER*40 IDIST
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
!
      PARAMETER (NUMALP=6)
      REAL ALPHA(NUMALP)
      PARAMETER (NUMAL2=4)
      REAL ALPHA2(NUMAL2)
!
      PARAMETER(NUMCLI=4)
      PARAMETER(MAXLIN=3)
      PARAMETER (MAXROW=NUMALP)
      PARAMETER (MAXRO2=30)
      CHARACTER*60 ITITLE
      CHARACTER*60 ITITLZ
      CHARACTER*60 ITITL9
      CHARACTER*60 ITEXT(MAXRO2)
      CHARACTER*4  ALIGN(NUMCLI)
      CHARACTER*4  VALIGN(NUMCLI)
      REAL         AVALUE(MAXRO2)
      INTEGER      NCTEXT(MAXRO2)
      INTEGER      IDIGIT(MAXRO2)
      INTEGER      NTOT(MAXRO2)
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
      LOGICAL IFLAGS
      LOGICAL IFLAGE
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
      DATA ALPHA/0.90, 0.95, 0.975, 0.99, 0.995, 0.999/
      DATA ALPHA2/0.80, 0.90, 0.95, 0.99/
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DP2R'
      ISUBN2='C2  '
      IERROR='NO'
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'2RC2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DP2RC2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N
   52   FORMAT('IBUGA3,ISUBRO,N = ',2(A4,2X),I8)
        CALL DPWRST('XXX','WRIT')
        DO 56 I=1,N
          WRITE(ICOUT,57)I,Y1(I),Y2(I)
   57     FORMAT('I,Y1(I),Y2(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
!
!               *************************************
!               **  STEP 21--                     **
!               **  CARRY OUT CALCULATIONS        **
!               **  FOR AN RANK CORRELATION TEST  **
!               ************************************
!
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'2RC2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IFLAG=0
      IDIST='NULL'
      CALL SUMRAW(Y1,N,IDIST,IFLAG,   &
                  YMEAN1,YVAR1,YSD1,YMIN1,YMAX1,   &
                  ISUBRO,IBUGA3,IERROR)
!
      CALL SUMRAW(Y2,N,IDIST,IFLAG,   &
                  YMEAN2,YVAR2,YSD2,YMIN2,YMAX2,   &
                  ISUBRO,IBUGA3,IERROR)
!
      CALL RANKCR(Y1,Y2,N,IRCRTA,IWRITE,   &
                  TEMP1,TEMP2,TEMP3,MAXNXT,   &
                  STATVA,STATCD,PVAL,PVALLT,PVALUT,   &
                  CUTU90,CUTU95,CTU975,CUTU99,CTU995,CTU999,   &
                  CUTL90,CUTL95,CTL975,CUTL99,CTL995,CTL999,   &
                  IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               ******************************
!               **   STEP 42--              **
!               **   WRITE OUT EVERYTHING   **
!               **   FOR KENDELL TAU  TEST  **
!               ******************************
!
      ISTEPN='42'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'2RC2')   &
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
      ITITLE='Two Sample Rank Correlation Test for Independence'
      NCTITL=49
      ITITLZ=' '
      NCTITZ=0
!
      ICNT=1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='First Response Variable:  '
      WRITE(ITEXT(ICNT)(27:30),'(A4)')IVARID(1:4)
      WRITE(ITEXT(ICNT)(31:34),'(A4)')IVARI2(1:4)
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Second Response Variable: '
      WRITE(ITEXT(ICNT)(27:30),'(A4)')IVARI3(1:4)
      WRITE(ITEXT(ICNT)(31:34),'(A4)')IVARI4(1:4)
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='H0: The Two Samples are Independent'
      NCTEXT(ICNT)=35
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      IF(ICASA2.EQ.'LOWE')THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='Ha: The Two Samples Are Negatively Correlated'
        NCTEXT(ICNT)=45
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
      ELSEIF(ICASA2.EQ.'UPPE')THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='Ha: The Two Samples Are Positively Correlated'
        NCTEXT(ICNT)=45
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
      ELSE
        ICNT=ICNT+1
        ITEXT(ICNT)='Ha: The Two Samples Are Not Independent'
        NCTEXT(ICNT)=39
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
      ENDIF
!
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Observations:'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=REAL(N)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample One Summary Statistics:'
      NCTEXT(ICNT)=30
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Mean:'
      NCTEXT(ICNT)=12
      AVALUE(ICNT)=YMEAN1
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Standard Deviation:'
      NCTEXT(ICNT)=26
      AVALUE(ICNT)=YSD1
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Minimum:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=YMIN1
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Maximum:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=YMAX1
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Two Summary Statistics:'
      NCTEXT(ICNT)=30
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Mean:'
      NCTEXT(ICNT)=12
      AVALUE(ICNT)=YMEAN2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Standard Deviation:'
      NCTEXT(ICNT)=26
      AVALUE(ICNT)=YSD2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Minimum:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=YMIN2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Maximum:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=YMAX2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Test:'
      NCTEXT(ICNT)=5
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Spearman Rho Rank Correlation Value:'
      NCTEXT(ICNT)=36
      AVALUE(ICNT)=STATVA
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='CDF Value (Normal Approximation):'
      NCTEXT(ICNT)=33
      AVALUE(ICNT)=STATCD
      IDIGIT(ICNT)=NUMDIG
      IF(ICASA2.EQ.'LOWE')THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='Lower Tailed P-Value (Normal Approximation):'
        NCTEXT(ICNT)=44
        AVALUE(ICNT)=PVALLT
        IDIGIT(ICNT)=NUMDIG
      ELSEIF(ICASA2.EQ.'UPPE')THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='Upper Tailed P-Value (Normal Approximation):'
        NCTEXT(ICNT)=44
        AVALUE(ICNT)=PVALUT
        IDIGIT(ICNT)=NUMDIG
      ELSE
        ICNT=ICNT+1
        ITEXT(ICNT)='Two-Sided P-Value (Normal Approximation):'
        NCTEXT(ICNT)=41
        AVALUE(ICNT)=PVAL
        IDIGIT(ICNT)=NUMDIG
      ENDIF
!
      NUMROW=ICNT
      DO 5010 I=1,NUMROW
        NTOT(I)=15
 5010 CONTINUE
!
      IFRST=.TRUE.
      ILAST=.TRUE.
!
      ISTEPN='42A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ADK2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                  AVALUE,IDIGIT,   &
                  NTOT,NUMROW,   &
                  ICAPSW,ICAPTY,ILAST,IFRST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      ISTEPN='42D'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ADK2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICASA2.EQ.'LOWE')THEN
        ITITLE='Conclusions (Lower 1-Tailed Test)'
        NCTITL=33
        ITITL9='H0: Samples are Independent'
        NCTIT9=27
      ELSEIF(ICASA2.EQ.'UPPE')THEN
        ITITLE='Conclusions (Upper 1-Tailed Test)'
        NCTITL=33
        ITITL9='H0: Samples are Independent'
        NCTIT9=27
      ELSE
        ITITLE='Conclusions (Two-Tailed Test)'
        NCTITL=29
        ITITL9='H0: Samples are Independent'
        NCTIT9=27
      ENDIF
!
      DO 5030 J=1,NUMCLI
        DO 5040 I=1,3
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
 5040   CONTINUE
 5030 CONTINUE
!
      ITITL2(2,1)='Significance'
      NCTIT2(2,1)=12
      ITITL2(3,1)='Level'
      NCTIT2(3,1)=5
!
      ITITL2(2,2)='Test '
      NCTIT2(2,2)=4
      ITITL2(3,2)='Statistic'
      NCTIT2(3,2)=9
!
      IF(ICASA2.EQ.'LOWE')THEN
        ITITL2(2,3)='Critical'
        NCTIT2(2,3)=8
        ITITL2(3,3)='Region (<)'
        NCTIT2(3,3)=10
      ELSEIF(ICASA2.EQ.'UPPE')THEN
        ITITL2(2,3)='Critical'
        NCTIT2(2,3)=8
        ITITL2(3,3)='Region (>)'
        NCTIT2(3,3)=10
      ELSE
        ITITL2(2,3)='Critical'
        NCTIT2(2,3)=8
        ITITL2(3,3)='Region (+/-)'
        NCTIT2(3,3)=12
      ENDIF
!
      ITITL2(1,4)='Null'
      NCTIT2(1,4)=4
      ITITL2(2,4)='Hypothesis'
      NCTIT2(2,4)=10
      ITITL2(3,4)='Conclusion'
      NCTIT2(3,4)=10
!
      NMAX=0
      NUMCOL=NUMCLI
      DO 5050 I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
        IDIGIT(I)=NUMDIG
        IF(I.EQ.1 .OR. I.EQ.4)THEN
          ITYPCO(I)='ALPH'
        ENDIF
 5050 CONTINUE
!
      IWHTML(1)=125
      IWHTML(2)=175
      IWHTML(3)=175
      IWHTML(4)=175
      IINC=1800
      IINC2=1400
      IWRTF(1)=IINC
      IWRTF(2)=IWRTF(1)+IINC
      IWRTF(3)=IWRTF(2)+IINC
      IWRTF(4)=IWRTF(3)+IINC
!
      ICNT=NUMALP
      IF(ICASA2.EQ.'LOWE')THEN
        DO 5060 J=1,NUMALP
!
          AMAT(J,2)=STATVA
          IF(J.EQ.1)THEN
            AMAT(J,3)=CUTL90
          ELSEIF(J.EQ.2)THEN
            AMAT(J,3)=CUTL95
          ELSEIF(J.EQ.3)THEN
            AMAT(J,3)=CTL975
          ELSEIF(J.EQ.4)THEN
            AMAT(J,3)=CUTL99
          ELSEIF(J.EQ.5)THEN
            AMAT(J,3)=CTL995
          ELSEIF(J.EQ.6)THEN
            AMAT(J,3)=CTL999
          ENDIF
          IVALUE(J,4)(1:6)='REJECT'
          IF(STATVA.LT.AMAT(J,3))THEN
            IVALUE(J,4)(1:6)='ACCEPT'
          ENDIF
          NCVALU(J,4)=6
!
          ALPHAT=100.0*ALPHA(J)
          WRITE(IVALUE(J,1)(1:4),'(F4.1)')ALPHAT
          IVALUE(J,1)(5:5)='%'
          NCVALU(J,1)=5
 5060   CONTINUE
      ELSEIF(ICASA2.EQ.'UPPE')THEN
        DO 5070 J=1,NUMALP
!
          AMAT(J,2)=STATVA
          IF(J.EQ.1)THEN
            AMAT(J,3)=CUTU90
          ELSEIF(J.EQ.2)THEN
            AMAT(J,3)=CUTU95
          ELSEIF(J.EQ.3)THEN
            AMAT(J,3)=CTU975
          ELSEIF(J.EQ.4)THEN
            AMAT(J,3)=CUTU99
          ELSEIF(J.EQ.5)THEN
            AMAT(J,3)=CTU995
          ELSEIF(J.EQ.6)THEN
            AMAT(J,3)=CTU999
          ENDIF
          IVALUE(J,4)(1:6)='REJECT'
          IF(STATVA.LT.AMAT(J,3))THEN
            IVALUE(J,4)(1:6)='ACCEPT'
          ENDIF
          NCVALU(J,4)=6
!
          ALPHAT=100.0*ALPHA(J)
          WRITE(IVALUE(J,1)(1:4),'(F4.1)')ALPHAT
          IVALUE(J,1)(5:5)='%'
          NCVALU(J,1)=5
 5070   CONTINUE
      ELSE
        ICNT=NUMAL2
        DO 5080 J=1,NUMAL2
!
          AMAT(J,2)=STATVA
          IF(J.EQ.1)THEN
            AMAT(J,3)=CUTU90
          ELSEIF(J.EQ.2)THEN
            AMAT(J,3)=CUTU95
          ELSEIF(J.EQ.3)THEN
            AMAT(J,3)=CTU975
          ELSEIF(J.EQ.4)THEN
            AMAT(J,3)=CTU995
          ENDIF
          IVALUE(J,4)(1:6)='REJECT'
          IF(ABS(STATVA).LT.AMAT(J,3))THEN
            IVALUE(J,4)(1:6)='ACCEPT'
          ENDIF
          NCVALU(J,4)=6
!
          ALPHAT=100.0*ALPHA2(J)
          WRITE(IVALUE(J,1)(1:4),'(F4.1)')ALPHAT
          IVALUE(J,1)(5:5)='%'
          NCVALU(J,1)=5
 5080   CONTINUE
      ENDIF
!
      NUMLIN=3
      IFRST=.TRUE.
      ILAST=.TRUE.
      IFLAGS=.TRUE.
      IFLAGE=.TRUE.
      CALL DPDTA5(ITITLE,NCTITL,   &
                  ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                  MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                  IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'2RC2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DP2RC2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      RETURN
      END SUBROUTINE DP2RC2
      SUBROUTINE DP2SIN(MAXNXT,ICAPSW,IFORSW,   &
                        IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--CARRY OUT A 2-SAMPLE TEST FOR INDEPENDENCE BASED ON
!              EITHER THE SPEARMAN RHO OR KENDELL TAU CORRELATION
!     EXAMPLE--SPEARMAN RHO TWO SAMPLE INDEPENDENCE TEST Y1 Y2
!              KENDELL TAU TWO SAMPLE INDEPENDENCE TEST Y1 Y2
!              SPEARMAN RHO TWO SAMPLE INDEPENDENCE TEST Y1 Y2 Y3 Y4
!              SPEARMAN RHO TWO SAMPLE INDEPENDENCE TEST Y1 TO Y10
!              LOWER TAILED KENDELL TAU TWO SAMPLE INDEPENDENCE TEST Y1 Y2
!              UPPER TAILED KENDELL TAU TWO SAMPLE INDEPENDENCE TEST Y1 Y2
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2013/2
!     ORIGINAL VERSION--FEBRUARY  2013.
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
      CHARACTER*4 ICASAN
      CHARACTER*4 ICASA2
      CHARACTER*4 ICTMP1
      CHARACTER*4 ICTMP2
      CHARACTER*4 ICTMP3
      CHARACTER*4 ICTMP4
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      CHARACTER*4 ICASE
      CHARACTER*4 IVARID
      CHARACTER*4 IVARI2
      CHARACTER*4 IVARI3
      CHARACTER*4 IVARI4
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
      CHARACTER*4 IFLAGU
      LOGICAL IFRST
      LOGICAL ILAST
!
!---------------------------------------------------------------------
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZI.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOST.INC'
!
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TEMP3(MAXOBV)
      DIMENSION ITEMP1(MAXOBV)
      EQUIVALENCE(GARBAG(IGARB1),TEMP1(1))
      EQUIVALENCE(GARBAG(IGARB3),TEMP2(1))
      EQUIVALENCE(GARBAG(IGARB5),TEMP3(1))
      EQUIVALENCE(IGARBG(IIGAR1),ITEMP1(1))
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DP2S'
      ISUBN2='IN  '
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
      IFOUND='NO'
      IERROR='NO'
!
!               ************************************************
!               **  TREAT THE FISHER TWO SAMPLE RANDOMIZATION **
!               **  TEST CASE                                 **
!               ************************************************
!
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'2SIN')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DP2SIN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA2,IBUGA3,IBUGQ,ISUBRO,MAXNXT
   52   FORMAT('IBUGA2,IBUGA3,IBUGQ,ISUBRO = ',4(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *********************************************************
!               **  STEP 1--                                           **
!               **  EXTRACT THE COMMAND                                **
!               *********************************************************
!
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'2SIN')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ILASTZ=9999
      ICASAN='NULL'
      ICASA2='TWOS'
!
!     LOOK FOR:
!
!          KENDELL TAU  INDEPENDENCE TEST
!          LOWER TAILED KENDELL TAU  INDEPENDENCE TEST
!          UPPER TAILED KENDELL TAU  INDEPENDENCE TEST
!          RANK CORRELATION INDEPENDENCE TEST
!          LOWER TAILED RANK CORRELATION INDEPENDENCE TEST
!          UPPER TAILED RANK CORRELATION INDEPENDENCE TEST
!          SPEARMAN RHO INDEPENDENCE TEST
!          LOWER TAILED SPEARMAN RHO INDEPENDENCE TEST
!          UPPER TAILED SPEARMAN RHO INDEPENDENCE TEST
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
          ILASTZ=I+4
        ELSEIF(ICTMP1.EQ.'LOWE' .AND. ICTMP2.EQ.'TAIL')THEN
          ICASA2='LOWE'
        ELSEIF(ICTMP1.EQ.'UPPE' .AND. ICTMP2.EQ.'TAIL')THEN
          ICASA2='UPPE'
        ELSEIF(ICTMP1.EQ.'SPEA' .AND. ICTMP2.EQ.'RHO ' .AND.   &
               ICTMP3.EQ.'INDE' .AND. ICTMP4.EQ.'TEST')THEN
          ICASAN='SRHO'
          IFOUND='YES'
          ILASTZ=I+3
        ELSEIF(ICTMP1.EQ.'RANK' .AND. ICTMP2.EQ.'CORR' .AND.   &
               ICTMP3.EQ.'INDE' .AND. ICTMP4.EQ.'TEST')THEN
          ICASAN='SRHO'
          IFOUND='YES'
          ILASTZ=I+3
        ELSEIF(ICTMP1.EQ.'KEND' .AND. ICTMP2.EQ.'TAU ' .AND.   &
               ICTMP3.EQ.'INDE' .AND. ICTMP4.EQ.'TEST')THEN
          ICASAN='KTAU'
          IFOUND='YES'
          ILASTZ=I+3
        ENDIF
  100 CONTINUE
!
      IF(IFOUND.EQ.'NO')GO TO 9000
!
      ISHIFT=ILASTZ
      CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                  IBUGA2,IERROR)
!
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'2SIN')THEN
        WRITE(ICOUT,91)ICASAN,ICASA2,ISHIFT
   91   FORMAT('DP2SIN: ICASAN,ICASA2,ISHIFT = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ****************************************
!               **  STEP 2--                          **
!               **  EXTRACT THE VARIABLE LIST         **
!               ****************************************
!
      ISTEPN='2'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'2SIN')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICASAN.EQ.'RACR')THEN
        INAME='RANK CORRELATION INDEPENDENCE TEST'
      ELSE
        INAME='KENDALL TAU INDEPENDENCE TEST'
      ENDIF
      MINNA=1
      MAXNA=100
      MINN2=2
      IFLAGE=1
      IFLAGM=1
      MINNVA=2
      MAXNVA=MAXSPN
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'2SIN')THEN
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
!               ******************************************************
!               **  STEP 3A--                                       **
!               **  CASE 1: TWO RESPONSE VARIABLES, NO REPLICATION  **
!               ******************************************************
!
      ISTEPN='3A'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'2SIN')   &
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMVA2=1
      DO 5210 I=1,NUMVAR
        ICOL=I
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                    INAME,IVARN1,IVARN2,IVARTY,   &
                    ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                    MAXCP4,MAXCP5,MAXCP6,   &
                    V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                    Y,Y,Y,NS1,NLOCA2,NLOCA3,ICASE,   &
                    IBUGA3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
        ISTRT2=I+1
        ISTOP2=NUMVAR
!
        DO 5220 J=ISTRT2,ISTOP2
!
          ICOL=J
          CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                      INAME,IVARN1,IVARN2,IVARTY,   &
                      ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                      MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                      MAXCP4,MAXCP5,MAXCP6,   &
                      V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                      X,X,X,NS2,NLOCA2,NLOCA3,ICASE,   &
                      IBUGA3,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
!               *****************************************************
!               **  STEP 52--                                      **
!               **  PERFORM A SPEARMAN RHO/KENDELL TAU TWO SAMPLE  **
!               **  INDEPENDENCE TEST                              **
!               *****************************************************
!
          ISTEPN='52'
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'2SIN')THEN
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5211)
 5211       FORMAT('***** FROM DP2SIN, BEFORE CALL DP2SI2--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5212)I,J,NS1,NS2,MAXN
 5212       FORMAT('I,J,NS1,NS2,MAXN = ',5I8)
            CALL DPWRST('XXX','BUG ')
            DO 5215 II=1,MAX(NS1,NS2)
              WRITE(ICOUT,5216)II,Y(II),X(II)
 5216         FORMAT('I,Y(I),X(I) = ',I8,2G15.7)
              CALL DPWRST('XXX','BUG ')
 5215       CONTINUE
          ENDIF
!
          IVARID=IVARN1(I)
          IVARI2=IVARN2(I)
          IVARI3=IVARN1(J)
          IVARI4=IVARN2(J)
          IF(ICASAN.EQ.'KTAU')THEN
            CALL DP2SI2(Y,X,NS1,ICASA2,   &
                        TEMP1,TEMP2,MAXNXT,   &
                        IVARID,IVARI2,IVARI3,IVARI4,   &
                        ICAPSW,ICAPTY,IFORSW,IKTATA,   &
                        STATVA,STATCD,PVALUE,PVALLT,PVALUT,   &
                        CUTU90,CUTU95,CTU975,CUTU99,CTU995,   &
                        CUTL90,CUTL95,CTL975,CUTL99,CTL995,   &
                        IBUGA3,ISUBRO,IERROR)
            CTU999=CPUMIN
            CTL999=CPUMIN
          ELSE
            CALL DP2RC2(Y,X,NS1,ICASA2,   &
                        TEMP1,TEMP2,TEMP3,MAXNXT,   &
                        IVARID,IVARI2,IVARI3,IVARI4,   &
                        ICAPSW,ICAPTY,IFORSW,IRCRTA,   &
                        STATVA,STATCD,PVALUE,PVALLT,PVALUT,   &
                        CUTU90,CUTU95,CTU975,CUTU99,CTU995,CTU999,   &
                        CUTL90,CUTL95,CTL975,CUTL99,CTL995,CTL999,   &
                        IBUGA3,ISUBRO,IERROR)
          ENDIF
          IF(IERROR.EQ.'YES')GO TO 9000
!
!               ***************************************
!               **  STEP 8C--                        **
!               **  UPDATE INTERNAL DATAPLOT TABLES  **
!               ***************************************
!
          ISTEPN='8C'
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'2SIN')   &
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
          IF(NUMVAR.GT.2)THEN
            IFLAGU='FILE'
          ELSE
            IFLAGU='ON'
          ENDIF
          IFRST=.FALSE.
          ILAST=.FALSE.
          IF(I.EQ.1 .AND. J.EQ.2)IFRST=.TRUE.
          IF(I.EQ.NUMVAR .AND. J.EQ.NUMVAR)ILAST=.TRUE.
          CALL DP2SI5(STATVA,STATCD,PVALUE,PVALLT,PVALUT,   &
                      CUTU90,CUTU95,CTU975,CUTU99,CTU995,CTU999,   &
                      CUTL90,CUTL95,CTL975,CUTL99,CTL995,CTL999,   &
                      IFLAGU,IFRST,ILAST,   &
                      IBUGA2,IBUGA3,ISUBRO,IERROR)
!
 5220   CONTINUE
 5210 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'2SIN')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DP2SIN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)IFOUND,IERROR
 9016   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DP2SIN
      SUBROUTINE DP2SI2(Y1,Y2,N,ICASA2,   &
                        TEMP1,TEMP2,MAXNXT,   &
                        IVARID,IVARI2,IVARI3,IVARI4,   &
                        ICAPSW,ICAPTY,IFORSW,IKTATA,   &
                        STATVA,STATCD,PVAL,PVALLT,PVALUT,   &
                        CUTU90,CUTU95,CTU975,CUTU99,CTU995,   &
                        CUTL90,CUTL95,CTL975,CUTL99,CTL995,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--CARRY OUT A 2-SAMPLE TEST FOR INDEPENDENCE BASED ON
!              THE KENDELL TAU CORRELATION
!     EXAMPLE--KENDELL TAU INDEPENDENCE TEST Y1 Y2
!              LOWER TAILED KENDELL TAU INDEPENDENCE TEST Y1 Y2
!              UPPER TAILED KENDELL TAU INDEPENDENCE TEST Y1 Y2
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2013/2
!     ORIGINAL VERSION--FEBRUARY  2013.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 ICASA2
      CHARACTER*4 IFORSW
      CHARACTER*4 IKTATA
      CHARACTER*4 IVARID
      CHARACTER*4 IVARI2
      CHARACTER*4 IVARI3
      CHARACTER*4 IVARI4
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
      CHARACTER*40 IDIST
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
!
      PARAMETER (NUMALP=5)
      REAL ALPHA(NUMALP)
      PARAMETER (NUMAL2=4)
      REAL ALPHA2(NUMAL2)
!
      PARAMETER(NUMCLI=4)
      PARAMETER(MAXLIN=3)
      PARAMETER (MAXROW=NUMALP)
      PARAMETER (MAXRO2=30)
      CHARACTER*60 ITITLE
      CHARACTER*60 ITITLZ
      CHARACTER*60 ITITL9
      CHARACTER*60 ITEXT(MAXRO2)
      CHARACTER*4  ALIGN(NUMCLI)
      CHARACTER*4  VALIGN(NUMCLI)
      REAL         AVALUE(MAXRO2)
      INTEGER      NCTEXT(MAXRO2)
      INTEGER      IDIGIT(MAXRO2)
      INTEGER      NTOT(MAXRO2)
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
      LOGICAL IFLAGS
      LOGICAL IFLAGE
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
      DATA ALPHA/0.90, 0.95, 0.975, 0.99, 0.995/
      DATA ALPHA2/0.80, 0.90, 0.95, 0.99/
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DP2S'
      ISUBN2='I2  '
      IERROR='NO'
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'2SI2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DP2SI2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N
   52   FORMAT('IBUGA3,ISUBRO,N = ',2(A4,2X),I8)
        CALL DPWRST('XXX','WRIT')
        DO 56 I=1,N
          WRITE(ICOUT,57)I,Y1(I),Y2(I)
   57     FORMAT('I,Y1(I),Y2(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
!
!               ******************************
!               **  STEP 21--               **
!               **  CARRY OUT CALCULATIONS  **
!               **  FOR AN          F TEST  **
!               ******************************
!
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'2SI2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IFLAG=0
      IDIST='NULL'
      CALL SUMRAW(Y1,N,IDIST,IFLAG,   &
                  YMEAN1,YVAR1,YSD1,YMIN1,YMAX1,   &
                  ISUBRO,IBUGA3,IERROR)
!
      CALL SUMRAW(Y2,N,IDIST,IFLAG,   &
                  YMEAN2,YVAR2,YSD2,YMIN2,YMAX2,   &
                  ISUBRO,IBUGA3,IERROR)
!
      CALL KENTAU(Y1,Y2,N,ICASA2,IKTATA,IWRITE,   &
                  TEMP1,TEMP2,MAXNXT,   &
                  STATVA,AKTAUA,AKTAUB,AKTAUC,   &
                  STATCD,PVAL,PVALLT,PVALUT,   &
                  CUTU90,CUTU95,CTU975,CUTU99,CTU995,   &
                  CUTL90,CUTL95,CTL975,CUTL99,CTL995,   &
                  IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               ******************************
!               **   STEP 42--              **
!               **   WRITE OUT EVERYTHING   **
!               **   FOR KENDELL TAU  TEST  **
!               ******************************
!
      ISTEPN='42'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'2SI2')   &
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
      ITITLE='Two Sample Kendall Tau Test for Independence'
      NCTITL=44
      ITITLZ=' '
      NCTITZ=0
!
      ICNT=1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='First Response Variable:  '
      WRITE(ITEXT(ICNT)(27:30),'(A4)')IVARID(1:4)
      WRITE(ITEXT(ICNT)(31:34),'(A4)')IVARI2(1:4)
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Second Response Variable: '
      WRITE(ITEXT(ICNT)(27:30),'(A4)')IVARI3(1:4)
      WRITE(ITEXT(ICNT)(31:34),'(A4)')IVARI4(1:4)
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='H0: The Two Samples are Independent'
      NCTEXT(ICNT)=35
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      IF(ICASA2.EQ.'LOWE')THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='Ha: Pairs of Samples Tend to be Discordant'
        NCTEXT(ICNT)=42
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
      ELSEIF(ICASA2.EQ.'UPPE')THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='Ha: Pairs of Samples Tend to be Concordant'
        NCTEXT(ICNT)=42
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
      ELSE
        ICNT=ICNT+1
        ITEXT(ICNT)='Ha: Pairs of Samples Tend to be Either'
        NCTEXT(ICNT)=38
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        ICNT=ICNT+1
        ITEXT(ICNT)='    Concordant or Discordant'
        NCTEXT(ICNT)=28
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
      ENDIF
!
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Observations:'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=REAL(N)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample One Summary Statistics:'
      NCTEXT(ICNT)=30
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Mean:'
      NCTEXT(ICNT)=12
      AVALUE(ICNT)=YMEAN1
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Standard Deviation:'
      NCTEXT(ICNT)=26
      AVALUE(ICNT)=YSD1
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Minimum:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=YMIN1
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Maximum:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=YMAX1
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Two Summary Statistics:'
      NCTEXT(ICNT)=30
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Mean:'
      NCTEXT(ICNT)=12
      AVALUE(ICNT)=YMEAN2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Standard Deviation:'
      NCTEXT(ICNT)=26
      AVALUE(ICNT)=YSD2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Minimum:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=YMIN2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Maximum:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=YMAX2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Test:'
      NCTEXT(ICNT)=5
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Kendall Tau Test Statistic Value:'
      NCTEXT(ICNT)=33
      AVALUE(ICNT)=STATVA
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='CDF Value (Normal Approximation):'
      NCTEXT(ICNT)=33
      AVALUE(ICNT)=STATCD
      IDIGIT(ICNT)=NUMDIG
      IF(ICASA2.EQ.'LOWE')THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='Lower Tailed P-Value (Normal Approximation):'
        NCTEXT(ICNT)=44
        AVALUE(ICNT)=PVALLT
        IDIGIT(ICNT)=NUMDIG
      ELSEIF(ICASA2.EQ.'UPPE')THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='Upper Tailed P-Value (Normal Approximation):'
        NCTEXT(ICNT)=44
        AVALUE(ICNT)=PVALUT
        IDIGIT(ICNT)=NUMDIG
      ELSE
        ICNT=ICNT+1
        ITEXT(ICNT)='Two-Sided P-Value (Normal Approximation):'
        NCTEXT(ICNT)=41
        AVALUE(ICNT)=PVAL
        IDIGIT(ICNT)=NUMDIG
      ENDIF
!
      NUMROW=ICNT
      DO 5010 I=1,NUMROW
        NTOT(I)=15
 5010 CONTINUE
!
      IFRST=.TRUE.
      ILAST=.TRUE.
!
      ISTEPN='42A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ADK2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                  AVALUE,IDIGIT,   &
                  NTOT,NUMROW,   &
                  ICAPSW,ICAPTY,ILAST,IFRST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      ISTEPN='42D'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ADK2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICASA2.EQ.'LOWE')THEN
        ITITLE='Conclusions (Lower 1-Tailed Test)'
        NCTITL=33
        ITITL9='H0: Samples are Independent'
        NCTIT9=27
      ELSEIF(ICASA2.EQ.'UPPE')THEN
        ITITLE='Conclusions (Upper 1-Tailed Test)'
        NCTITL=33
        ITITL9='H0: Samples are Independent'
        NCTIT9=27
      ELSE
        ITITLE='Conclusions (Two-Tailed Test)'
        NCTITL=29
        ITITL9='H0: Samples are Independent'
        NCTIT9=27
      ENDIF
!
      DO 5030 J=1,NUMCLI
        DO 5040 I=1,3
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
 5040   CONTINUE
 5030 CONTINUE
!
      ITITL2(2,1)='Significance'
      NCTIT2(2,1)=12
      ITITL2(3,1)='Level'
      NCTIT2(3,1)=5
!
      ITITL2(2,2)='Test '
      NCTIT2(2,2)=4
      ITITL2(3,2)='Statistic'
      NCTIT2(3,2)=9
!
      IF(ICASA2.EQ.'LOWE')THEN
        ITITL2(2,3)='Critical'
        NCTIT2(2,3)=8
        ITITL2(3,3)='Region (<)'
        NCTIT2(3,3)=10
      ELSEIF(ICASA2.EQ.'UPPE')THEN
        ITITL2(2,3)='Critical'
        NCTIT2(2,3)=8
        ITITL2(3,3)='Region (>)'
        NCTIT2(3,3)=10
      ELSE
        ITITL2(2,3)='Critical'
        NCTIT2(2,3)=8
        ITITL2(3,3)='Region (+/-)'
        NCTIT2(3,3)=12
      ENDIF
!
      ITITL2(1,4)='Null'
      NCTIT2(1,4)=4
      ITITL2(2,4)='Hypothesis'
      NCTIT2(2,4)=10
      ITITL2(3,4)='Conclusion'
      NCTIT2(3,4)=10
!
      NMAX=0
      NUMCOL=NUMCLI
      DO 5050 I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
        IDIGIT(I)=NUMDIG
        IF(I.EQ.1 .OR. I.EQ.4)THEN
          ITYPCO(I)='ALPH'
        ENDIF
 5050 CONTINUE
!
      IWHTML(1)=125
      IWHTML(2)=175
      IWHTML(3)=175
      IWHTML(4)=175
      IINC=1800
      IINC2=1400
      IWRTF(1)=IINC
      IWRTF(2)=IWRTF(1)+IINC
      IWRTF(3)=IWRTF(2)+IINC
      IWRTF(4)=IWRTF(3)+IINC
!
      ICNT=NUMALP
      IF(ICASA2.EQ.'LOWE')THEN
        DO 5060 J=1,NUMALP
!
          AMAT(J,2)=STATVA
          IF(J.EQ.1)THEN
            AMAT(J,3)=CUTL90
          ELSEIF(J.EQ.2)THEN
            AMAT(J,3)=CUTL95
          ELSEIF(J.EQ.3)THEN
            AMAT(J,3)=CTL975
          ELSEIF(J.EQ.4)THEN
            AMAT(J,3)=CUTL99
          ELSEIF(J.EQ.5)THEN
            AMAT(J,3)=CTL995
          ENDIF
          IVALUE(J,4)(1:6)='REJECT'
          IF(STATVA.LT.AMAT(J,3))THEN
            IVALUE(J,4)(1:6)='ACCEPT'
          ENDIF
          NCVALU(J,4)=6
!
          ALPHAT=100.0*ALPHA(J)
          WRITE(IVALUE(J,1)(1:4),'(F4.1)')ALPHAT
          IVALUE(J,1)(5:5)='%'
          NCVALU(J,1)=5
 5060   CONTINUE
      ELSEIF(ICASA2.EQ.'UPPE')THEN
        DO 5070 J=1,NUMALP
!
          AMAT(J,2)=STATVA
          IF(J.EQ.1)THEN
            AMAT(J,3)=CUTU90
          ELSEIF(J.EQ.2)THEN
            AMAT(J,3)=CUTU95
          ELSEIF(J.EQ.3)THEN
            AMAT(J,3)=CTU975
          ELSEIF(J.EQ.4)THEN
            AMAT(J,3)=CUTU99
          ELSEIF(J.EQ.5)THEN
            AMAT(J,3)=CTU995
          ENDIF
          IVALUE(J,4)(1:6)='REJECT'
          IF(STATVA.LT.AMAT(J,3))THEN
            IVALUE(J,4)(1:6)='ACCEPT'
          ENDIF
          NCVALU(J,4)=6
!
          ALPHAT=100.0*ALPHA(J)
          WRITE(IVALUE(J,1)(1:4),'(F4.1)')ALPHAT
          IVALUE(J,1)(5:5)='%'
          NCVALU(J,1)=5
 5070   CONTINUE
      ELSE
        ICNT=NUMAL2
        DO 5080 J=1,NUMAL2
!
          AMAT(J,2)=STATVA
          IF(J.EQ.1)THEN
            AMAT(J,3)=CUTU90
          ELSEIF(J.EQ.2)THEN
            AMAT(J,3)=CUTU95
          ELSEIF(J.EQ.3)THEN
            AMAT(J,3)=CTU975
          ELSEIF(J.EQ.4)THEN
            AMAT(J,3)=CTU995
          ENDIF
          IVALUE(J,4)(1:6)='REJECT'
          IF(ABS(STATVA).LT.AMAT(J,3))THEN
            IVALUE(J,4)(1:6)='ACCEPT'
          ENDIF
          NCVALU(J,4)=6
!
          ALPHAT=100.0*ALPHA2(J)
          WRITE(IVALUE(J,1)(1:4),'(F4.1)')ALPHAT
          IVALUE(J,1)(5:5)='%'
          NCVALU(J,1)=5
 5080   CONTINUE
      ENDIF
!
      NUMLIN=3
      IFRST=.TRUE.
      ILAST=.TRUE.
      IFLAGS=.TRUE.
      IFLAGE=.TRUE.
      CALL DPDTA5(ITITLE,NCTITL,   &
                  ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                  MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                  IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'2SI2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DP2SI2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      RETURN
      END SUBROUTINE DP2SI2
      SUBROUTINE DP2SI5(STATVA,STATCD,PVAL,PVALLT,PVALUT,   &
                        CUTU90,CUTU95,CTU975,CUTU99,CTU995,CTU999,   &
                        CUTL90,CUTL95,CTL975,CUTL99,CTL995,CTL999,   &
                        IFLAGU,IFRST,ILAST,   &
                        IBUGA2,IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--UTILITY ROUTINE USED BY DP2SIN TO UPDATE VARIOUS
!              INTERNAL PARAMETERS AFTER A KENDALL TAU INDEPENDENCE TEST
!              OR RANK CORRELATION INDEPENDENCE TEST.
!
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORAOTRY
!                 NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2013/3
!     ORIGINAL VERSION--MARCH     2013.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IFLAGU
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
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOHO.INC'
!
      CHARACTER*4 IOP
      SAVE IOUNI1
!
!-----COMMON----------------------------------------------------------
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'2SI5')THEN
        ISTEPN='1'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DP2SI5--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)STATVA,STATCD,PVAL,PVALLT,PVALUT
   53   FORMAT('STATVA,STATCD,PVAL,PVALLT,PVALUT = ',5G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)CUTL90,CUTL95,CTL975,CUTL99,CTL995,CTL999
   54   FORMAT('CUTL90,CUTL95,CTL975,CUTL99,CTL995,CTL999 = ',6G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)CUTU90,CUTU95,CTU975,CUTU99,CTU995,CTU999
   55   FORMAT('CUTU90,CUTU95,CTU975,CUTU99,CTU995,CTU999 = ',6G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(IFLAGU.EQ.'FILE')THEN
!
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
          IF(IERROR.EQ.'YES')GO TO 9000
!
          WRITE(IOUNI1,295)
  295     FORMAT(11X,'STATVAL',8X,'STATCDF',   &
                  9X,'PVALUE',9X,'PVALLT',9X,'PVALUT',   &
                  7X,'CUTLOW90',7X,'CUTLOW95',7X,'CTLOW975',   &
                  7X,'CUTLOW99',7X,'CTLOW995',7X,'CTLOW999',   &
                  7X,'CUTUPP90',7X,'CUTUPP95',7X,'CTUPP975',   &
                  7X,'CUTUPP99',7X,'CTUPP995',7X,'CTUPP999')
        ENDIF
        WRITE(IOUNI1,299)STATVA,STATCD,PVAL,PVALLT,PVALUT,   &
                         CUTL90,CUTL95,CTL975,CUTL99,CTL995,CTL999,   &
                         CUTU90,CUTU95,CTU975,CUTU99,CTU995,CTU999
  299   FORMAT(18E15.7)
      ELSEIF(IFLAGU.EQ.'ON')THEN
        IF(STATVA.NE.CPUMIN)THEN
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
        IF(PVALLT.NE.CPUMIN)THEN
          IH='PVAL'
          IH2='UELT'
          VALUE0=PVALLT
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(PVALUT.NE.CPUMIN)THEN
          IH='PVAL'
          IH2='UEUT'
          VALUE0=PVALUT
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(CUTU90.NE.CPUMIN)THEN
          IH='CUTU'
          IH2='PP90'
          VALUE0=CUTU90
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(CUTL90.NE.CPUMIN)THEN
          IH='CUTL'
          IH2='OW90'
          VALUE0=CUTL90
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(CUTU95.NE.CPUMIN)THEN
          IH='CUTU'
          IH2='PP95'
          VALUE0=CUTU95
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(CUTL95.NE.CPUMIN)THEN
          IH='CUTL'
          IH2='OW95'
          VALUE0=CUTL95
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(CTU975.NE.CPUMIN)THEN
          IH='CTUP'
          IH2='P975'
          VALUE0=CTU975
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(CTL975.NE.CPUMIN)THEN
          IH='CTLO'
          IH2='W975'
          VALUE0=CTL975
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(CUTU99.NE.CPUMIN)THEN
          IH='CUTU'
          IH2='PP99'
          VALUE0=CUTU99
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(CUTL99.NE.CPUMIN)THEN
          IH='CUTL'
          IH2='OW99'
          VALUE0=CUTL99
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(CTU995.NE.CPUMIN)THEN
          IH='CTUP'
          IH2='P995'
          VALUE0=CTU995
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(CTL995.NE.CPUMIN)THEN
          IH='CTLO'
          IH2='W995'
          VALUE0=CTL995
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(CTU999.NE.CPUMIN)THEN
          IH='CUTU'
          IH2='P999'
          VALUE0=CTU999
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(CTL999.NE.CPUMIN)THEN
          IH='CUTL'
          IH2='O999'
          VALUE0=CTL999
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
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'2SI5')THEN
            ISTEPN='3A'
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,301)IERROR
  301       FORMAT('AFTER CALL DPCLFI, IERROR = ',A4)
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
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'2SI5')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DP2SI5--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DP2SI5
      SUBROUTINE DPABAS(XTEMP1,XTEMP2,MAXNXT,   &
                        ICASAN,ICASDI,ICAPSW,IFORSW,   &
                        IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--COMPUTE A AND B BASIS TOLERANCE LIMITS
!              FOR NORMAL, LOGNORMAL, WEIBULL, NON-PARAMETRIC CASES
!     EXAMPLE--B BASIS NORMAL TOLERANCE LIMITS Y
!     REFERENCE--MARK VANGEL
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     EXAMPLE--TOLERANCE LIMITS Y
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--98/4
!     ORIGINAL VERSION--APRIL     1998.
!     UPDATED         --MAY       2011. USE DPPARS ROUTINE
!     UPATED          --MAY       2011. REWRITTEN TO HANDLE MULTIPLE
!                                       RESPONSE VARIABLES, GROUP-ID
!                                       VARIABLES, OR A LAB-ID VARIABLE
!     UPATED          --JUNE      2019. MODIFICATION TO SCRATCH STORAGE
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASAN
      CHARACTER*4 ICASDI
      CHARACTER*4 ICAPSW
      CHARACTER*4 IFORSW
      CHARACTER*4 IBUGA2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IDATSW
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
      CHARACTER*4 IFLAGU
      LOGICAL IFRST
      LOGICAL ILAST
!
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
      DIMENSION Y1(MAXOBV)
!
      DIMENSION XDESGN(MAXOBV,7)
      DIMENSION XIDTEM(MAXOBV)
      DIMENSION XIDTE2(MAXOBV)
      DIMENSION XIDTE3(MAXOBV)
      DIMENSION XIDTE4(MAXOBV)
      DIMENSION XIDTE5(MAXOBV)
      DIMENSION XIDTE6(MAXOBV)
      DIMENSION TEMP1(MAXOBV)
!
      INCLUDE 'DPCOZZ.INC'
!
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB3),XIDTEM(1))
      EQUIVALENCE (GARBAG(IGARB4),XIDTE2(1))
      EQUIVALENCE (GARBAG(IGARB5),XIDTE3(1))
      EQUIVALENCE (GARBAG(IGARB6),XIDTE4(1))
      EQUIVALENCE (GARBAG(IGARB7),XIDTE5(1))
      EQUIVALENCE (GARBAG(IGARB8),XIDTE6(1))
      EQUIVALENCE (GARBAG(IGARB9),XDESGN(1,1))
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOST.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
      IFOUND='NO'
      IREPL='OFF'
      IMULT='OFF'
      ISUBN1='DPAB'
      ISUBN2='AS  '
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
      NTOT=0
!
!               ***********************************************
!               **  TREAT THE TOLERANCE LIMITS TEST  CASE    **
!               ***********************************************
!
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'ABAS')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPABAS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASAN,MAXNXT
   52   FORMAT('ICASAN,MAXNXT = ',A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGA2,IBUGA3,IBUGQ,ISUBRO
   53   FORMAT('IBUGA2,IBUGA3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *********************************************************
!               **  STEP 1--                                           **
!               **  EXTRACT THE COMMAND                                **
!               **  LOOK FOR ONE OF THE FOLLOWING COMMANDS:            **
!               **    1) <ABASIS/BBASIS> <NORMAL/LOGNORMAL/WEIBULL> Y  **
!               **    2) MULTIPLE <ABASIS/BBASIS>                      **
!               **       <NORMAL/LOGNORMAL/WEIBULL> Y1 ... YK          **
!               **    3) REPLICATED <ABASIS/BBASIS>                    **
!               **       <NORMAL/LOGNORMAL/WEIBULL> Y X1 ... XK        **
!               *********************************************************
!
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'ABAS')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ILASTZ=0
      ICASAN='BBAS'
      ICASDI='WEIB'
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
        ELSEIF(ICTMP1.EQ.'A   ' .AND. ICTMP2.EQ.'BASI')THEN
          IFOUND='YES'
          ICASAN='ABAS'
          ILASTZ=MAX(ILASTZ,I+1)
        ELSEIF(ICTMP1.EQ.'ABAS')THEN
          IFOUND='YES'
          ICASAN='ABAS'
          ILASTZ=MAX(ILASTZ,I)
        ELSEIF(ICTMP1.EQ.'B   ' .AND. ICTMP2.EQ.'BASI')THEN
          IFOUND='YES'
          ICASAN='BBAS'
          ILASTZ=MAX(ILASTZ,I+1)
        ELSEIF(ICTMP1.EQ.'BBAS')THEN
          IFOUND='YES'
          ICASAN='BBAS'
          ILASTZ=MAX(ILASTZ,I)
        ELSEIF(ICTMP1.EQ.'TOLE' .AND. ICTMP2.EQ.'LIMI')THEN
          ILASTZ=MAX(ILASTZ,I+1)
        ELSEIF(ICTMP1.EQ.'TOLE' .AND. ICTMP2.EQ.'INTE')THEN
          ILASTZ=MAX(ILASTZ,I+1)
        ELSEIF(ICTMP1.EQ.'TOLE')THEN
          ILASTZ=MAX(ILASTZ,I)
        ELSEIF(ICTMP1.EQ.'REPL')THEN
          IREPL='ON'
          ILASTZ=MAX(ILASTZ,I)
        ELSEIF(ICTMP1.EQ.'MULT')THEN
          IMULT='ON'
          ILASTZ=MAX(ILASTZ,I)
        ELSEIF(ICTMP1.EQ.'NORM')THEN
          ICASDI='NORM'
          ILASTZ=MAX(ILASTZ,I)
        ELSEIF(ICTMP1.EQ.'LOGN')THEN
          ICASDI='LOGN'
          ILASTZ=MAX(ILASTZ,I)
        ELSEIF(ICTMP1.EQ.'WEIB')THEN
          ICASDI='WEIB'
          ILASTZ=MAX(ILASTZ,I)
        ELSEIF(ICTMP1.EQ.'NONP')THEN
          ICASAN='NONP'
          ILASTZ=MAX(ILASTZ,I)
        ELSEIF(ICTMP1.EQ.'NON '.AND.ICTMP2.EQ.'PARA')THEN
          ICASAN='NONP'
          ILASTZ=MAX(ILASTZ,I+1)
        ENDIF
  100 CONTINUE
!
      IF(IFOUND.EQ.'NO')GO TO 9000
!
      ISHIFT=ILASTZ
      CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                  IBUGA2,IERROR)
!
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'ABAS')THEN
        WRITE(ICOUT,91)ICASAN,ICASDI,IMULT,IREPL,ISHIFT
   91   FORMAT('DPABAS: ICASAN,ICASDI,IMULT,IREPL,ISHIFT=',4(A4,2X),I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(IMULT.EQ.'ON')THEN
        IF(IREPL.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
  101     FORMAT('***** ERROR IN ABASIS/BBASIS TOLERANCE LIMITS--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,102)
  102     FORMAT('      YOU CANNOT SPECIFY BOTH "MULTIPLE" AND ',   &
                 '"REPLICATION"')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,104)
  104     FORMAT('      FOR THE ABASIS/BBASIS TOLERANCE LIMITS TEST ',   &
                 'COMMAND.')
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'ABAS')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      INAME='TOLERANCE LIMITS'
      MINNA=1
      MAXNA=100
      MINN2=2
      IF(IREPL.EQ.'ON')THEN
        IFLAGM=0
        IFLAGE=1
      ELSE
        IFLAGM=1
        IFLAGE=0
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'ABAS')THEN
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'ABAS')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'ABAS')THEN
        WRITE(ICOUT,521)NRESP,NREPL
  521   FORMAT('NRESP,NREPL = ',2I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ******************************************************
!               **  STEP 6--                                        **
!               **  GENERATE THE ABASIS/BBASIS TOLERANCE LIMITS FOR **
!               **  THE VARIOUS CASES                               **
!               ******************************************************
!
      ISTEPN='6'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'ABAS')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!               ******************************************
!               **  STEP 8A--                           **
!               **  CASE 1: NO REPLICATION VARIABLES    **
!               ******************************************
!
      IF(NREPL.LT.1)THEN
        ISTEPN='8A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'ABAS')   &
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
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'ABAS')THEN
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
                      Y1,XTEMP1,XTEMP1,NLOCAL,NLOCA2,NLOCA3,ICASE,   &
                      IBUGA3,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
!         *****************************************************
!         **  STEP 8B--                                      **
!         *****************************************************
!
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'ABAS')THEN
            ISTEPN='8B'
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,822)
  822       FORMAT('***** FROM THE MIDDLE  OF DPABAS--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,823)ICASAN,NUMVAR,IDATSW,NLOCAL
  823       FORMAT('ICASAN,NUMVAR,IDATSW,NQ = ',   &
                   A4,I8,2X,A4,I8)
            CALL DPWRST('XXX','BUG ')
            IF(NLOCAL.GE.1)THEN
              DO 825 I=1,NLOCAL
                WRITE(ICOUT,826)I,Y1(I)
  826           FORMAT('I,Y1(I) = ',I8,G15.7)
                CALL DPWRST('XXX','BUG ')
  825         CONTINUE
            ENDIF
          ENDIF
!
          CALL DPABA2(Y1,NLOCAL,   &
                      XTEMP1,MAXNXT,   &
                      ICASAN,ICASDI,ICAPSW,ICAPTY,IFORSW,   &
                      PID,IVARID,IVARI2,NREPL,   &
                      ABASIS,BBASIS,   &
                      ISUBRO,IBUGA3,IERROR)
!
!               ***************************************
!               **  STEP 8C--                        **
!               **  UPDATE INTERNAL DATAPLOT TABLES  **
!               ***************************************
!
          ISTEPN='8C'
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'ABAS')   &
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
          IF(NRESP.GT.1)THEN
            IFLAGU='FILE'
          ELSE
            IFLAGU='ON'
          ENDIF
          IFRST=.FALSE.
          ILAST=.FALSE.
          IF(IRESP.EQ.1)IFRST=.TRUE.
          IF(IRESP.EQ.NRESP)ILAST=.TRUE.
          ATEMP=ABASIS
          IF(ICASAN.EQ.'BBAS')ATEMP=BBASIS
          CALL DPABA5(ICASAN,ATEMP,   &
                      IFLAGU,IFRST,ILAST,   &
                      IBUGA2,IBUGA3,ISUBRO,IERROR)
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
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'ABAS')   &
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
!       **  CALL DPABA2 TO GENERATE TOLERANCE LIMITS.      **
!       *****************************************************
!
!
        IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'ABAS')THEN
          ISTEPN='9C'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,941)
  941     FORMAT('***** FROM THE MIDDLE  OF DPABAS--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,942)ICASAN,NUMVAR,NLOCAL,NREPL
  942     FORMAT('ICASAN,NUMVAR,NLOCAL,NREPL = ',   &
                 A4,3I8)
          CALL DPWRST('XXX','BUG ')
          IF(NLOCAL.GE.1)THEN
            DO 945 I=1,NLOCAL
              WRITE(ICOUT,946)I,Y1(I),XDESGN(I,1),XDESGN(I,2)
  946         FORMAT('I,Y1(I),XDESGN(I,1),XDESGN(I,2) = ',   &
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
                TEMP1(K)=Y1(I)
              ENDIF
 1130       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              CALL DPABA2(TEMP1,NTEMP,   &
                          XTEMP1,MAXNXT,   &
                          ICASAN,ICASDI,ICAPSW,ICAPTY,IFORSW,   &
                          PID,IVARN1,IVARN2,NREPL,   &
                          ABASIS,BBASIS,   &
                          ISUBRO,IBUGA3,IERROR)
              IFLAGU='FILE'
              IFRST=.FALSE.
              ILAST=.FALSE.
              IF(NCURVE.EQ.1)IFRST=.TRUE.
              IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
              ATEMP=ABASIS
              IF(ICASAN.EQ.'BBAS')ATEMP=BBASIS
              CALL DPABA5(ICASAN,ATEMP,   &
                          IFLAGU,IFRST,ILAST,   &
                          IBUGA2,IBUGA3,ISUBRO,IERROR)
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
                TEMP1(K)=Y1(I)
              ENDIF
 1290       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              CALL DPABA2(TEMP1,NTEMP,   &
                          XTEMP1,MAXNXT,   &
                          ICASAN,ICASDI,ICAPSW,ICAPTY,IFORSW,   &
                          PID,IVARN1,IVARN2,NREPL,   &
                          ABASIS,BBASIS,   &
                          ISUBRO,IBUGA3,IERROR)
              IFLAGU='FILE'
              IFRST=.FALSE.
              ILAST=.FALSE.
              IF(NCURVE.EQ.1)IFRST=.TRUE.
              IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
              ATEMP=ABASIS
              IF(ICASAN.EQ.'BBAS')ATEMP=BBASIS
              CALL DPABA5(ICASAN,ATEMP,   &
                          IFLAGU,IFRST,ILAST,   &
                          IBUGA2,IBUGA3,ISUBRO,IERROR)
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
                TEMP1(K)=Y1(I)
              ENDIF
 1390       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              CALL DPABA2(TEMP1,NTEMP,   &
                          XTEMP1,MAXNXT,   &
                          ICASAN,ICASDI,ICAPSW,ICAPTY,IFORSW,   &
                          PID,IVARN1,IVARN2,NREPL,   &
                          ABASIS,BBASIS,   &
                          ISUBRO,IBUGA3,IERROR)
              IFLAGU='FILE'
              IFRST=.FALSE.
              ILAST=.FALSE.
              IF(NCURVE.EQ.1)IFRST=.TRUE.
              IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
              ATEMP=ABASIS
              IF(ICASAN.EQ.'BBAS')ATEMP=BBASIS
              CALL DPABA5(ICASAN,ATEMP,   &
                          IFLAGU,IFRST,ILAST,   &
                          IBUGA2,IBUGA3,ISUBRO,IERROR)
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
                TEMP1(K)=Y1(I)
              ENDIF
 1490       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              CALL DPABA2(TEMP1,NTEMP,   &
                          XTEMP1,MAXNXT,   &
                          ICASAN,ICASDI,ICAPSW,ICAPTY,IFORSW,   &
                          PID,IVARN1,IVARN2,NREPL,   &
                          ABASIS,BBASIS,   &
                          ISUBRO,IBUGA3,IERROR)
              IFLAGU='FILE'
              IFRST=.FALSE.
              ILAST=.FALSE.
              IF(NCURVE.EQ.1)IFRST=.TRUE.
              IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
              ATEMP=ABASIS
              IF(ICASAN.EQ.'BBAS')ATEMP=BBASIS
              CALL DPABA5(ICASAN,ATEMP,   &
                          IFLAGU,IFRST,ILAST,   &
                          IBUGA2,IBUGA3,ISUBRO,IERROR)
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
                TEMP1(K)=Y1(I)
              ENDIF
 1590       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              CALL DPABA2(TEMP1,NTEMP,   &
                          XTEMP1,MAXNXT,   &
                          ICASAN,ICASDI,ICAPSW,ICAPTY,IFORSW,   &
                          PID,IVARN1,IVARN2,NREPL,   &
                          ABASIS,BBASIS,   &
                          ISUBRO,IBUGA3,IERROR)
              IFLAGU='FILE'
              IFRST=.FALSE.
              ILAST=.FALSE.
              IF(NCURVE.EQ.1)IFRST=.TRUE.
              IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
              ATEMP=ABASIS
              IF(ICASAN.EQ.'BBAS')ATEMP=BBASIS
              CALL DPABA5(ICASAN,ATEMP,   &
                          IFLAGU,IFRST,ILAST,   &
                          IBUGA2,IBUGA3,ISUBRO,IERROR)
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
                TEMP1(K)=Y1(I)
              ENDIF
 1690       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              CALL DPABA2(TEMP1,NTEMP,   &
                          XTEMP1,MAXNXT,   &
                          ICASAN,ICASDI,ICAPSW,ICAPTY,IFORSW,   &
                          PID,IVARN1,IVARN2,NREPL,   &
                          ABASIS,BBASIS,   &
                          ISUBRO,IBUGA3,IERROR)
              IFLAGU='FILE'
              IFRST=.FALSE.
              ILAST=.FALSE.
              IF(NCURVE.EQ.1)IFRST=.TRUE.
              IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
              ATEMP=ABASIS
              IF(ICASAN.EQ.'BBAS')ATEMP=BBASIS
              CALL DPABA5(ICASAN,ATEMP,   &
                          IFLAGU,IFRST,ILAST,   &
                          IBUGA2,IBUGA3,ISUBRO,IERROR)
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
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'ABAS')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPABAS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR,ICASAN
 9012   FORMAT('IFOUND,IERROR,ICASAN = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPABAS
      SUBROUTINE DPABA2(Y,N,   &
                        XTEMP,MAXNXT,   &
                        ICASAN,ICASDI,ICAPSW,ICAPTY,IFORSW,   &
                        PID,IVARID,IVARI2,NREPL,   &
                        ABASIS,BBASIS,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS ROUTINE COMPUTES THE B BASIS AND A BASIS
!              TOLERANCE LIMITS
!     EXAMPLE--B BASIS NORMAL TOLERANCE LIMITS Y
!     REFERENCE--XX
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--98/3
!     ORIGINAL VERSION--MARCH     1998.
!     UPDATED         --MAY       2011. USE DPDTA1 TO PRINT OUTPUT
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASAN
      CHARACTER*4 ICASDI
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
      CHARACTER*4 IVARID(*)
      CHARACTER*4 IVARI2(*)
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION XTEMP(*)
      DIMENSION PID(*)
!
      PARAMETER (MAXROW=20)
      CHARACTER*60 ITITLE
      CHARACTER*60 ITITLZ
      CHARACTER*40 ITEXT(MAXROW)
      REAL         AVALUE(MAXROW)
      INTEGER      NCTEXT(MAXROW)
      INTEGER      IDIGIT(MAXROW)
      INTEGER      NTOT(MAXROW)
      LOGICAL IFRST
      LOGICAL ILAST
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPAB'
      ISUBN2='A2  '
!
      IERROR='NO'
      IWRITE='OFF'
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ABA2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPABA2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,ICASAN,ICASDI,N
   52   FORMAT('IBUGA3,ISUBRO,ICASAN,ICASDI,N = ',4(A4,2X),I8)
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ABA2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPABA3(Y,N,   &
                  XTEMP,MAXNXT,   &
                  ICASAN,ICASDI,   &
                  T10,V10,NDF,GAMMA,ALPHA,YMEAN,YSD,YMIN,YMAX,   &
                  ABASIS,BBASIS,   &
                  ISUBRO,IBUGA3,IERROR)
!
!               *********************************
!               **   STEP 42--                 **
!               **   WRITE OUT EVERYTHING      **
!               **   FOR TOLERANCE LIMITS      **
!               **********************************
!
      ISTEPN='42'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ABA2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
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
      IF(ICASDI.EQ.'NORM')THEN
        ITITLE='Normal A Basis Tolerance Limits'
        NCTITL=31
        IF(ICASAN.EQ.'BBAS')ITITLE(8:8)='B'
      ELSEIF(ICASDI.EQ.'LOGN')THEN
        ITITLE='Lognormal A Basis Tolerance Limits'
        NCTITL=34
        IF(ICASAN.EQ.'BBAS')ITITLE(11:11)='B'
      ELSEIF(ICASDI.EQ.'WEIB')THEN
        ITITLE='Weibull A Basis Tolerance Limits'
        NCTITL=32
        IF(ICASAN.EQ.'BBAS')ITITLE(9:9)='B'
      ELSEIF(ICASDI.EQ.'NONP')THEN
        ITITLE='Non-Parametric A Basis Tolerance Limits'
        NCTITL=39
        IF(ICASAN.EQ.'BBAS')ITITLE(16:16)='B'
      ENDIF
!
      IF(ICASAN.EQ.'BBAS')THEN
        ITITLZ=' '
        NCTITZ=0
      ELSE
        ITITLZ=' '
        NCTITZ=0
      ENDIF
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
      ICNT=1
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
      ITEXT(ICNT)='Sample Minimum:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=YMIN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Maximum:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=YMAX
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Tolerance Values:'
      NCTEXT(ICNT)=17
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Confidence Value:'
      NCTEXT(ICNT)=17
      AVALUE(ICNT)=0.95
      IDIGIT(ICNT)=NUMDIG
      IF(ICASAN.EQ.'BBAS')THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='Coverage Value:'
        NCTEXT(ICNT)=15
        AVALUE(ICNT)=0.90
        IDIGIT(ICNT)=NUMDIG
      ELSE
        ICNT=ICNT+1
        ITEXT(ICNT)='Coverage Value:'
        NCTEXT(ICNT)=15
        AVALUE(ICNT)=0.99
        IDIGIT(ICNT)=NUMDIG
      ENDIF
!
      IF(ICASDI.EQ.'WEIB')THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='Shape Parameter:'
        NCTEXT(ICNT)=16
        AVALUE(ICNT)=GAMMA
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='Scale Parameter:'
        NCTEXT(ICNT)=16
        AVALUE(ICNT)=ALPHA
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='Tolerance Limit Factor:'
        NCTEXT(ICNT)=23
        AVALUE(ICNT)=V10
        IDIGIT(ICNT)=NUMDIG
      ELSEIF(ICASDI.EQ.'LOGN' .OR. ICASDI.EQ.'NORM')THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='Degrees of Freedom:'
        NCTEXT(ICNT)=19
        AVALUE(ICNT)=REAL(NDF)
        IDIGIT(ICNT)=0
        ICNT=ICNT+1
        ITEXT(ICNT)='Tolerance Limit Factor:'
        NCTEXT(ICNT)=23
        AVALUE(ICNT)=T10
        IDIGIT(ICNT)=NUMDIG
      ENDIF
      ICNT=ICNT+1
      IF(ICASAN.EQ.'ABAS')THEN
        ITEXT(ICNT)='A Basis Value:'
        AVALUE(ICNT)=ABASIS
      ELSE
        ITEXT(ICNT)='B Basis Value:'
        AVALUE(ICNT)=BBASIS
      ENDIF
      NCTEXT(ICNT)=14
      IDIGIT(ICNT)=NUMDIG
!
      NUMROW=ICNT
      DO 2310 I=1,NUMROW
        NTOT(I)=15
 2310 CONTINUE
!
      IFRST=.TRUE.
      ILAST=.TRUE.
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                  AVALUE,IDIGIT,   &
                  NTOT,NUMROW,   &
                  ICAPSW,ICAPTY,ILAST,IFRST,   &
                  ISUBRO,IBUGA3,IERROR)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ABA2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPABA2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      RETURN
      END SUBROUTINE DPABA2
      SUBROUTINE DPABA3(Y,N,   &
                        XTEMP,MAXNXT,   &
                        ICASAN,ICASDI,   &
                        T10,V10,NDF,GAMMA,ALPHA,YMEAN,YSD,YMIN,YMAX,   &
                        ABASIS,BBASIS,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS ROUTINE COMPUTES THE B BASIS AND A BASIS
!              TOLERANCE LIMITS
!     EXAMPLE--B BASIS NORMAL TOLERANCE LIMITS Y
!     REFERENCE--XX
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2011/5
!     ORIGINAL VERSION--MAY       2011. EXTRACTED FROM DPABA2 IN ORDER
!                                       TO ADD IT TO LIST OF SUPPORTED
!                                       STATISTICS
!     UPDATED         --DECEMBER  2016. CORRECT VN FACTOR FOR A-BASIS
!                                       (WAS USING B-BASIS)
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASAN
      CHARACTER*4 ICASDI
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
      CHARACTER*20 ITYPE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION XTEMP(*)
!
      DIMENSION NBBASN(107)
      DIMENSION NBBASR(107)
      DIMENSION NBBS2R(28)
      DIMENSION ABBS2K(28)
      DIMENSION NABASN(100)
      DIMENSION NABS2R(106)
      DIMENSION AABS2K(106)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!CCCC DATA NVAL/29,46,61,89,203,215,227,321,615,4005,4109,4213/
!CCCC DATA IVAL/ 1, 2, 3, 5, 14, 15, 16, 24, 50, 370, 380, 390/
!
!CCCC FOLLOWING TABLES FROM MIL-HANDBOOK 17.
!
      DATA (NBBASN(I),I=1,107)/   &
          28,   29,   46,   61,   76,   89,  103,  116,  129,  142,   &
         154,  167,  179,  191,  203,  215,  227,  239,  251,  263,   &
         275,  298,  321,  345,  368,  391,  413,  436,  459,  481,   &
         504,  526,  549,  571,  593,  615,  638,  660,  682,  704,   &
         726,  781,  836,  890,  945,  999, 1053, 1107, 1161, 1269,   &
        1376, 1483, 1590, 1696, 1803, 1909, 2015, 2120, 2226, 2331,   &
        2437, 2542, 2647, 2752, 2857, 2962, 3066, 3171, 3276, 3380,   &
        3484, 3589, 3693, 3797, 3901, 4005, 4109, 4213, 4317, 4421,   &
        4525, 4629, 4733, 4836, 4940, 5044, 5147, 5251, 5354, 5613,   &
        5871, 6130, 6388, 6645, 6903, 7161, 7418, 7727, 8036, 8344,   &
        8652, 8960, 9268, 9576, 9884,10191,10499/
      DATA (NBBASR(I),I=1,107)/   &
           0,    1,    2,    3,    4,    5,    6,    7,    8,    9,   &
          10,   11,   12,   13,   14,   15,   16,   17,   18,   19,   &
          20,   22,   24,   26,   28,   30,   32,   34,   36,   38,   &
          40,   42,   44,   46,   48,   50,   52,   54,   56,   58,   &
          60,   65,   70,   75,   80,   85,   90,   95,  100,  110,   &
         120,  130,  140,  150,  160,  170,  180,  190,  200,  210,   &
         220,  230,  240,  250,  260,  270,  280,  290,  300,  310,   &
         320,  330,  340,  350,  360,  370,  380,  390,  400,  410,   &
         420,  430,  440,  450,  460,  470,  480,  490,  500,  525,   &
         550,  575,  600,  625,  650,  675,  700,  730,  760,  790,   &
         820,  850,  880,  910,  940,  970, 1000/
      DATA (NBBS2R(I),I=1,28)/   &
        0,  2,  3,  4,  4,  5,  5,  6,  6,  6,  7,  7,  7,  8,  8,   &
        8,  8,  9,  9, 10, 10, 10, 11, 11, 11, 11, 11, 12/
      DATA (ABBS2K(I),I=1,28)/   &
        0., 35.177, 7.859, 4.505, 4.101, 3.064, 2.858, 2.382, 2.253,   &
        2.137, 1.897, 1.814, 1.738, 1.599, 1.540, 1.485, 1.434,   &
        1.354, 1.311, 1.253, 1.218, 1.184, 1.143, 1.114, 1.087,   &
        1.060, 1.035, 1.010/
      DATA (NABASN(I),I=1,100)/   &
         298,  299,  473,  628,  773,  913, 1049, 1182, 1312, 1441,   &
        1568, 1693, 1818, 1941, 2064, 2185, 2306, 2426, 2546, 2665,   &
        2784, 2902, 3020, 3137, 3254, 3371, 3487, 3603, 3719, 3834,   &
        3949, 4064, 4179, 4293, 4407, 4521, 4635, 4749, 4862, 4975,   &
        5088, 5201, 5314, 5427, 5539, 5651, 5764, 5876, 5988, 6099,   &
        6211, 6323, 6434, 6545, 6657, 6769, 6879, 6990, 7100, 7211,   &
        7322, 7432, 7543, 7653, 7763, 7874, 7984, 8094, 8204, 8314,   &
        8423, 8533, 8643, 8753, 8862, 8972, 9081, 9190, 9300, 9518,   &
        9627, 9736, 9854, 9954,10063,10172,10281,10390,10498,10607,   &
       10716,10824,10933,11041,11150,11258,11366,11475,11583,11691/
      DATA (NABS2R(I),I=1,106)/   &
           2,    3,    4,    5,    6,    7,    8,    9,   &
          10,   11,   12,   13,   14,   15,   16,   17,   18,   19,   &
          20,   21,   22,   23,   24,   25,   26,   27,   28,   29,   &
          30,   31,   32,   33,   34,   35,   36,   37,   38,   39,   &
          40,   41,   42,   43,   44,   45,   46,   47,   48,   49,   &
          50,   52,   54,   56,   58,   60,   62,   64,   66,   68,   &
          70,   72,   74,   76,   78,   80,   82,   84,   86,   88,   &
          90,   92,   94,   96,   98,  100,  105,  110,  115,  120,   &
         125,  130,  135,  140,  145,  150,  155,  160,  165,  170,   &
         175,  180,  185,  190,  195,  200,  205,  210,  215,  220,   &
         225,  230,  235,  240,  245,  250,  275,  299/
      DATA (AABS2K(I),I=1,106)/   &
       80.00380,16.91220, 9.49579, 6.89049, 5.57681, 4.78352, 4.25011,   &
        3.86502, 3.57267, 3.34227, 3.15540, 3.00033, 2.86924, 2.75672,   &
        2.65889, 2.57290, 2.49660, 2.42833, 2.36683, 2.31106, 2.26020,   &
        2.21359, 2.17067, 2.13100, 2.09419, 2.05991, 2.02790, 1.99791,   &
        1.96975, 1.94324, 1.91822, 1.89457, 1.87215, 1.85088, 1.83065,   &
        1.81139, 1.79301, 1.77546, 1.75868, 1.74260, 1.72718, 1.71239,   &
        1.69817, 1.68449, 1.67132, 1.65862, 1.64638, 1.63456, 1.62313,   &
        1.60139, 1.58101, 1.56184, 1.54377, 1.52670, 1.51053, 1.49520,   &
        1.48063, 1.46675, 1.45352, 1.44089, 1.42881, 1.41724, 1.40614,   &
        1.39549, 1.38525, 1.37541, 1.36592, 1.35678, 1.34796, 1.33944,   &
        1.33120, 1.32324, 1.31553, 1.30806, 1.29036, 1.27392, 1.25859,   &
        1.24425, 1.23080, 1.21814, 1.20620, 1.19491, 1.18421, 1.17406,   &
        1.16440, 1.15519, 1.14640, 1.13801, 1.12997, 1.12226, 1.11486,   &
        1.10776, 1.10092, 1.09434, 1.08799, 1.08187, 1.07595, 1.07024,   &
        1.06471, 1.05935, 1.05417, 1.04914, 1.04426, 1.03952, 1.01773,   &
        1.00000/
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPAB'
      ISUBN2='A3  '
      IERROR='NO'
      IWRITE='OFF'
!
      BASIS=CPUMIN
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ABA3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPABA3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,ICASAN,ICASDI,N,MAXNXT
   52   FORMAT('IBUGA3,ISUBRO,ICASAN,ICASDI,N,MAXNXT = ',4(A4,2X),2I8)
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ABA3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(N.LE.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1111)
 1111   FORMAT('***** ERROR IN A/B BASIS TOLERANCE LIMITS--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1112)
 1112   FORMAT('      THE NUMBER OF OBSERVATIONS IS LESS THAN 2.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1113)N
 1113   FORMAT('SAMPLE SIZE = ',I8)
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
      GO TO 9000
 1139 CONTINUE
!
!               ******************************
!               **  STEP 41--               **
!               **  CARRY OUT CALCULATIONS  **
!               **  FOR TOLERANCE LIMITS    **
!               ******************************
!
      ISTEPN='41'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ABA3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL MEAN(Y,N,IWRITE,YMEAN,IBUGA3,IERROR)
      CALL SD(Y,N,IWRITE,YSD,IBUGA3,IERROR)
      CALL MINIM(Y,N,IWRITE,YMIN,IBUGA3,IERROR)
      CALL MAXIM(Y,N,IWRITE,YMAX,IBUGA3,IERROR)
      NDF=N-1
      IF(ICASAN.EQ.'BBAS')THEN
        CALL NORPPF(0.9,Z)
      ELSE
        CALL NORPPF(0.99,Z)
      ENDIF
      ANC=SQRT(REAL(N))*Z
      SIG=0.95
      CALL NCTPPF(SIG,REAL(NDF),ANC,T10)
      T10=T10/SQRT(REAL(N))
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ABA3')THEN
        WRITE(ICOUT,4131)
 4131   FORMAT('***** FROM DPABA3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,4132)YMEAN,YSD,NDF,Z,ANC,T10
 4132   FORMAT('YMEAN,YSD,NDF,Z,ANC,T10 = ',2G15.7,I8,3G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      ABASIS=0.0
      BBASIS=0.0
      IF(ICASDI.EQ.'NORM')THEN
        ITYPE='NORMAL'
        BASIS=YMEAN-T10*YSD
      ELSEIF(ICASDI.EQ.'LOGN')THEN
        ITYPE='LOG-NORMAL'
        BASIS=EXP(YMEAN-T10*YSD)
      ELSEIF(ICASDI.EQ.'WEIB')THEN
        ITYPE='WEIBULL'
        IF(N.LE.9)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,1111)
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,2011)
 2011     FORMAT('      FOR THE WEIBULL TOLERANCE LIMIT, N MUST BE',   &
                 'GREATER THAN 10.')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,2013)N
 2013     FORMAT('      N IS EQUAL TO ',I6)
          CALL DPWRST('XXX','WRIT')
          IERROR='YES'
          GO TO 9000
        ELSEIF(N.EQ.10)THEN
          V10=6.710924
          IF(ICASAN.EQ.'ABAS')V10=12.573
        ELSEIF(N.EQ.11)THEN
          V10=6.476953
          IF(ICASAN.EQ.'ABAS')V10=12.093
        ELSEIF(N.EQ.12)THEN
          V10=6.286106
          IF(ICASAN.EQ.'ABAS')V10=11.701
        ELSEIF(N.EQ.13)THEN
          V10=6.126751
          IF(ICASAN.EQ.'ABAS')V10=11.375
        ELSEIF(N.EQ.14)THEN
          V10=5.991525
          IF(ICASAN.EQ.'ABAS')V10=11.098
        ELSEIF(N.EQ.15)THEN
          V10=5.875097
          IF(ICASAN.EQ.'ABAS')V10=10.861
        ELSE
          IF(ICASAN.EQ.'ABAS')THEN
            V10=6.649+EXP(2.55-0.526*LOG(REAL(N))+4.76/REAL(N))
          ELSE
            V10=3.803+EXP(1.79-0.516*LOG(REAL(N))+5.1/REAL(N))
          ENDIF
        ENDIF
        CALL WBLEST(Y,N,ALPHA,GAMMA,IERROR)
!CCCC   MARCH 2008: FOR A-BASIS, P2 = 0.01, NOT 0.10.
        P2=0.10
!CCCC   IF(ICASAN.EQ.'ABAS')P2=0.10
        IF(ICASAN.EQ.'ABAS')P2=0.01
        Q2=ALPHA*(-LOG(1.0-P2))**(1.0/GAMMA)
        RLCB2=-V10/(GAMMA*SQRT(REAL(N)))
        BASIS=Q2*EXP(RLCB2)
      ELSEIF(ICASDI.EQ.'NONP')THEN
        ITYPE='NON-PARAMETRIC'
!
! APPROXIMATE THE INDICES FOR THE NONPARAMETRIC
! ESTIMATES OF THE ALLOWABLES, SECTION 7.7.8, MIL-HDBK-17.
!
        CALL SORT(Y,N,XTEMP)
        IF(ICASAN.EQ.'BBAS')THEN
          IF(N.LE.1.OR.N.GT.10499)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,3002)
 3002       FORMAT('***** ERROR: VALUE OF N FOR NON-PARAMETERIC ',   &
                   'B BASIS VALUE OUTSIDE ALLOWABLE (2,10499) RANGE.')
            CALL DPWRST('XXX','WRIT')
            IERROR='YES'
            GO TO 9000
          ELSEIF(N.LE.28)THEN
            ASMALL=XTEMP(1)
            ABIG=XTEMP(NBBS2R(N))
            IF(ASMALL.EQ.ABIG)THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','WRIT')
              WRITE(ICOUT,3005)
 3005         FORMAT('***** ERROR: X(1) = X(R) FOR HANSON-KOOPMAN ',   &
                     'CALCULATION.  NO B BASIS CALCULATED.')
              CALL DPWRST('XXX','WRIT')
              IERROR='YES'
              GO TO 9000
            ENDIF
            AK=ABBS2K(N)
            BASIS=ABIG*(ASMALL/ABIG)**AK
          ELSEIF(N.LE.10499)THEN
            DO 3010 I=2,107
              IF(N.GE.NBBASN(I-1).AND.N.LT.NBBASN(I))THEN
                BASIS=XTEMP(NBBASR(I-1))
                GO TO 3019
              ENDIF
 3010       CONTINUE
            BASIS=XTEMP(1000)
 3019       CONTINUE
          ENDIF
        ELSE
          IF(N.LE.1.OR.N.GT.11691)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,3102)
 3102       FORMAT('***** ERROR: VALUE OF N FOR NON-PARAMETERIC ',   &
                   'A BASIS VALUE OUTSIDE ALLOWABLE (2,11691) RANGE.')
            CALL DPWRST('XXX','WRIT')
            IERROR='YES'
            GO TO 9000
          ELSEIF(N.LE.298)THEN
            ASMALL=XTEMP(1)
            ABIG=XTEMP(N)
            DO 3120 I=2,106
              IF(N.GE.NABS2R(I-1).AND.N.LT.NABS2R(I))THEN
                AK=AABS2K(I-1)
                GO TO 3129
              ENDIF
 3120       CONTINUE
            AK=1.0
 3129       CONTINUE
            BASIS=ABIG*(ASMALL/ABIG)**AK
          ELSEIF(N.LE.11691)THEN
            DO 3110 I=2,100
              IF(N.GE.NABASN(I-1).AND.N.LT.NABASN(I))THEN
                BASIS=XTEMP(I-1)
                GO TO 3119
              ENDIF
 3110       CONTINUE
            BASIS=XTEMP(100)
 3119       CONTINUE
          ENDIF
        ENDIF
      ENDIF
      IF(ICASAN.EQ.'ABAS')ABASIS=BASIS
      IF(ICASAN.EQ.'BBAS')BBASIS=BASIS
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ABA3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPABA3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      RETURN
      END SUBROUTINE DPABA3
      SUBROUTINE DPABA5(ICASAN,BASIS,   &
                        IFLAGU,IFRST,ILAST,   &
                        IBUGA2,IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--UTILITY ROUTINE USED BY DPABAS.  THIS ROUTINE
!              UPDATES THE PARAMETER "ABASIS" OR "BBASIS"
!              "PVALUE" AND VARIOUS CUTOFF POINTS AFTER A FREQUENCY TEST.
!
!              THIS ROUTINE MAY ALSO BE CALLED BY OTHER ROUTINES AS
!              WELL.
!
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORAOTRY
!                 NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2011/3
!     ORIGINAL VERSION--MARCH     2011.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASAN
      CHARACTER*4 IFLAGU
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
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOHO.INC'
!
      CHARACTER*4 IOP
      SAVE IOUNI1
!
!-----COMMON----------------------------------------------------------
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'ABA5')THEN
        ISTEPN='1'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPABA5--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASAN,BASIS
   53   FORMAT('ICASAN,BASIS = ',A4,2X,G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(IFLAGU.EQ.'FILE')THEN
!
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
          IF(IERROR.EQ.'YES')GO TO 9000
!
          IF(ICASAN.EQ.'ABAS')THEN
            WRITE(IOUNI1,295)
  295       FORMAT(8X,'A BASIS')
          ELSE
            WRITE(IOUNI1,296)
  296       FORMAT(8X,'B BASIS')
          ENDIF
        ENDIF
        WRITE(IOUNI1,299)BASIS
  299   FORMAT(E15.7)
      ELSEIF(IFLAGU.EQ.'ON')THEN
        IF(ICASAN.EQ.'ABAS')THEN
          IH='ABAS'
          IH2='IS  '
          VALUE0=BASIS
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ELSE
          IH='BBAS'
          IH2='IS  '
          VALUE0=BASIS
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
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'ABA5')THEN
            ISTEPN='3A'
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,301)IERROR
  301       FORMAT('AFTER CALL DPCLFI, IERROR = ',A4)
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
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'ABA5')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DPABA5--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPABA5
      SUBROUTINE DPACSA(MAXNXT,ICASAN,   &
                        IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--COMPUTE ACCEPTANCE SAMPLING PLANS.  FOLLOWING ARE
!              CURRENTLY SUPPORTED:
!              1) BINOMIAL SINGLE SAMPLE
!              12 BINOMIAL DOUBLE SAMPLE
!     EXAMPLE--SINGLE SAMPLE P1 P2 ALPHA BETA
!            --DOUBLE SAMPLE P1 P2 ALPHA BETA
!     REFERENCE--XX
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--99/3
!     ORIGINAL VERSION--MARCH     1999.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASAN
!
      CHARACTER*4 IBUGA2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 MESSAG
      CHARACTER*4 ICASEQ
      CHARACTER*4 IHWUSE
      CHARACTER*4 IH11
      CHARACTER*4 IH12
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IUSE1
      CHARACTER*4 IUSE2
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 IHOST1
      CHARACTER*4 ISUBN0
!
!---------------------------------------------------------------------
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCODA.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPAC'
      ISUBN2='SA  '
      ICASEQ='UNKN'
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
      N1=(-999)
      N2=(-999)
      NS1=(-999)
      NS2=(-999)
      ICOL1=(-999)
      ICOL2=(-999)
      IUSE1='-999'
      IUSE2='-999'
      ILOCV=(-999)
      MINN2=2
      NLEFT=0
      VALUE1=(-999.0)
      VALUE2=(-999.0)
!
!               *************************************************
!               **  TREAT THE SINGLE SAMPLE PLAN         CASE  **
!               *************************************************
!
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'ACSA')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPACSA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA2,IBUGA3,IBUGQ,MAXNXT,NUMARG
   52   FORMAT('IBUGA2,IBUGA3,IBUGQ,MAXNXT,NUMARG = ',3(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        DO 59 I=1,NUMARG
          WRITE(ICOUT,57)I,IHARG(I),IHARG2(I),ARG(I)
   57     FORMAT('I,IHARG(I),IHARG2(I),ARG(I) = ',I5,2A4,G15.7)
   59   CONTINUE
      ENDIF
!
!               *******************************************************
!               **  STEP 2--                                         **
!               **  CHECK FOR THE PROPER NUMBER OF INPUT ARGUMENTS.  **
!               *******************************************************
!
      ISTEPN='2'
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'ACSA')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      MINNA=1
      MAXNA=100
      CALL CHECKA(NUMARG,MINNA,MAXNA,IANS,IWIDTH,ISUBN1,ISUBN2,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               ****************************************
!               **  STEP 11--                         **
!               **  CHECK THE VALIDITY OF ARGUMENT 1  **
!               **  (THIS SHULD BE A PARAMETER.)      **
!               ****************************************
!
      ISTEPN='11'
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'ACSA')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IH11=IHARG(1)
      IH12=IHARG2(1)
      IHWUSE='P'
      MESSAG='YES'
!
      DO 1100 I=1,NUMNAM
        I2=I
        IF(IH11.EQ.IHNAME(I).AND.IH12.EQ.IHNAM2(I).AND.   &
           IUSE(I).EQ.'P')THEN
           P1=VALUE(I2)
           GO TO 1199
        ELSEIF(IH11.EQ.IHNAME(I).AND.IH12.EQ.IHNAM2(I).AND.   &
           IUSE(I).NE.'P')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1191)
 1191     FORMAT('***** ERROR IN DPACSA--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1192)
 1192     FORMAT('      FOR THE SINGLE SAMPLE ACCEPTANCE PLAN.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1193)
 1193     FORMAT('      THE FIRST ARGUMENT MUST BE A PARAMETER OR ',   &
                 'SCALAR')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1194)
 1194     FORMAT('      (AS OPPOSED TO A VARIABLE OR FUNCTION).')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1195)
 1195     FORMAT('      ARGUMENT ONE WAS NOT A PARAMETER HERE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1196)
 1196     FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,1197)(IANS(II),II=1,MIN(IWIDTH,80))
 1197       FORMAT(80A1)
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IERROR='YES'
          GO TO 9000
        ENDIF
 1100 CONTINUE
      P1=ARG(1)
!
 1199 CONTINUE
!
!               ****************************************
!               **  STEP 12--                         **
!               **  CHECK THE VALIDITY OF ARGUMENT 2  **
!               **  (THIS SHULD BE A PARAMETER.)      **
!               ****************************************
!
      ISTEPN='12'
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'ACSA')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IH11=IHARG(2)
      IH12=IHARG2(2)
      IHWUSE='P'
      MESSAG='YES'
!
      DO 1200 I=1,NUMNAM
        I2=I
        IF(IH12.EQ.IHNAME(I).AND.IH12.EQ.IHNAM2(I).AND.   &
           IUSE(I).EQ.'P')THEN
           P2=VALUE(I2)
           GO TO 1299
        ELSEIF(IH12.EQ.IHNAME(I).AND.IH12.EQ.IHNAM2(I).AND.   &
           IUSE(I).NE.'P')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1191)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1192)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1293)
 1293     FORMAT('      THE SECOND ARGUMENT MUST BE A PARAMETER OR ',   &
                 'SCALAR')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1194)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1295)
 1295     FORMAT('      ARGUMENT TWO WAS NOT A PARAMETER HERE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1196)
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,1197)(IANS(II),II=1,MIN(IWIDTH,80))
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IERROR='YES'
          GO TO 9000
        ENDIF
 1200 CONTINUE
      P2=ARG(2)
      GO TO 1299
!
 1299 CONTINUE
!
!               ****************************************
!               **  STEP 13--                         **
!               **  CHECK THE VALIDITY OF ARGUMENT 3  **
!               **  (THIS SHULD BE A PARAMETER.)      **
!               ****************************************
!
      ISTEPN='13'
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'ACSA')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IH11=IHARG(3)
      IH12=IHARG2(3)
      IHWUSE='P'
      MESSAG='YES'
!
      DO 1300 I=1,NUMNAM
        I2=I
        IF(IH11.EQ.IHNAME(I).AND.IH12.EQ.IHNAM2(I).AND.   &
           IUSE(I).EQ.'P')THEN
           ALPHA=VALUE(I2)
           GO TO 1399
        ELSEIF(IH11.EQ.IHNAME(I).AND.IH12.EQ.IHNAM2(I).AND.   &
           IUSE(I).NE.'P')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1191)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1192)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1393)
 1393     FORMAT('      THE THIRD ARGUMENT MUST BE A PARAMETER OR ',   &
                 'SCALAR')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1194)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1395)
 1395     FORMAT('      ARGUMENT THREE WAS NOT A PARAMETER HERE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1196)
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,1197)(IANS(II),II=1,MIN(IWIDTH,80))
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IERROR='YES'
          GO TO 9000
        ENDIF
 1300 CONTINUE
      ALPHA=ARG(3)
      GO TO 1399
!
 1399 CONTINUE
!
!               ****************************************
!               **  STEP 11--                         **
!               **  CHECK THE VALIDITY OF ARGUMENT 4  **
!               **  (THIS SHULD BE A PARAMETER.)      **
!               ****************************************
!
      ISTEPN='14'
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'ACSA')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IH11=IHARG(4)
      IH12=IHARG2(4)
      IHWUSE='P'
      MESSAG='YES'
!
      DO 1400 I=1,NUMNAM
        I2=I
        IF(IH11.EQ.IHNAME(I).AND.IH12.EQ.IHNAM2(I).AND.   &
           IUSE(I).EQ.'P')THEN
           BETA=VALUE(I2)
           GO TO 1499
        ELSEIF(IH11.EQ.IHNAME(I).AND.IH12.EQ.IHNAM2(I).AND.   &
           IUSE(I).NE.'P')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1191)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1192)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1493)
 1493     FORMAT('      THE FOURTH ARGUMENT MUST BE A PARAMETER OR ',   &
                 'SCALAR')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1194)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1495)
 1495     FORMAT('      ARGUMENT FOUR WAS NOT A PARAMETER HERE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1196)
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,1197)(IANS(II),II=1,MIN(IWIDTH,80))
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IERROR='YES'
          GO TO 9000
        ENDIF
 1400 CONTINUE
      BETA=ARG(4)
      GO TO 1499
!
 1499 CONTINUE
!
!               ***********************************
!               **  STEP 42--                    **
!               **  CHECK FOR PROPER VALUES FOR  **
!               **  INPUT PARAMETERS             **
!               ***********************************
!
      ISTEPN='42'
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'ACSA')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(P1.LE.0.0 .OR. P1.GE.1.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1191)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4203)
 4203   FORMAT('      THE VALUE OF THE FIRST PARAMETER (P1) MUST ',   &
               'BE IN THE INTERVAL (0,1).')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4205)P1
 4205   FORMAT('      P1 = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
      IF(P2.LE.0.0 .OR. P2.GE.1.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1191)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4213)
 4213   FORMAT('      THE VALUE OF THE SECOND PARAMETER (P2) MUST ',   &
               'BE IN THE INTERVAL (0,1).')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4215)P2
 4215   FORMAT('      P2 = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
      IF(ALPHA.LE.0.0 .OR. ALPHA.GE.1.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1191)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4223)
 4223   FORMAT('      THE VALUE OF THE THIRD PARAMETER (ALPHA) MUST ',   &
               'BE IN THE INTERVAL (0,1).')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4225)
 4225   FORMAT('      ALPHA = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
      IF(BETA.LE.0.0 .OR. BETA.GE.1.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1191)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4233)
 4233   FORMAT('      THE VALUE OF THE FOURTH PARAMETER (BETA) MUST ',   &
               'BE IN THE INTERVAL (0,1).')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4235)BETA
 4235   FORMAT('      BETA = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
      IF(P1.GE.P2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1191)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4245)P1
 4245   FORMAT('      ACCEPTABLE QUALITY LEVEL, ',F10.5,' IS SET')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4247)P2
 4247   FORMAT('      HIGHER THAN THE LOT PERCENT DEFECTIVE, ',F10.5,   &
               '.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               ***********************************
!               **  STEP 52--                    **
!               **  COMPUTE THE ACCEPTANCE PLAN  **
!               ***********************************
!
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'ACSA')THEN
        ISTEPN='52'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5211)
 5211   FORMAT('***** FROM DPACSA, AS WE ARE ABOUT TO CALL SSNC--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5212)P1,P2,ALPHA,BETA
 5212   FORMAT('P1,P2,ALPHA,BETA = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(ICASAN.EQ.'SSNC')THEN
        IERROR='NO'
        CALL SSNC(P1,P2,ALPHA,BETA,N,IC,IBUGA3,IERROR)
!
!               ***************************************
!               **  STEP 61--                        **
!               **  UPDATE INTERNAL DATAPLOT TABLES  **
!               ***************************************
!
        ISTEPN='61'
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'ACSA')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        ISUBN0='ACSA'
!
        IH='SSN '
        IH2='    '
        VALUE0=REAL(N)
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
        IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
        IANS,IWIDTH,IBUGA3,IERROR)
!
        IH='SSC '
        IH2='    '
        VALUE0=ALPHA
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
        IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
        IANS,IWIDTH,IBUGA3,IERROR)
!
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'ACSA')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPACSA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)IFOUND,IERROR
 9016   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPACSA
      SUBROUTINE DPADA3(ICASPL,IDIST,NUMSHA,IFORSW,IADCVM,IGOFFS,   &
                        IGOFFM,PID,IVARID,IVARI2,NREPL,   &
                        N,XMEAN,XSD,XMIN,XMAX,   &
                        A,B,MINMAX,   &
                        SHAPE1,SHAPE2,SHAPE3,SHAPE4,   &
                        SHAPE5,SHAPE6,SHAPE7,   &
                        KSLOC,KSSCAL,ICAPSW,ICAPTY,IRTFFF,IRTFFP,   &
                        STATVA,STATV2,CDF1,CDF2,CDF3,CDF4,   &
                        PVAL,YSTAT,NMCSAM,NCNT,   &
                        XTEMP,MAXNXT,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--PRINT THE OUTPUT FOR THE ANDERSON-DARLING TEST
!              (UNCENSORED, UNGROUPED CASE) IN ASCII, HTML, LATEX,
!              OR RTF FORMAT
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!         --DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2009/10
!     ORIGINAL VERSION--OCTOBER   2009.
!     UPDATED         --OCTOBER   2010. IF CRITICAL VALUES OPTION
!                                       IS "NONE", OMIT PARTS OF THE
!                                       PRINT OUT
!     UPDATED         --JUNE      2011. IF IGOFFM = NULL, NO P-VALUES
!                                       OR CRITICAL VALUES
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      REAL PID(*)
      REAL YSTAT(*)
      REAL XTEMP(*)
!
      CHARACTER*4 IVARID(*)
      CHARACTER*4 IVARI2(*)
!
      CHARACTER*4 ICASPL
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 IADCVM
      CHARACTER*4 IADCVT
      CHARACTER*4 IGOFFS
      CHARACTER*4 IGOFFM
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IWRITE
      CHARACTER*4 IERROR
!
      CHARACTER*60 IDIST
      CHARACTER*40 IRTFFF
      CHARACTER*40 IRTFFP
!
      CHARACTER*4 IRTFMD
      COMMON/COMRTF/IRTFMD
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      REAL KSLOC
      REAL KSSCAL
!
!---------------------------------------------------------------------
!
      REAL GPTABL(10,9)
      REAL GATABL(12,6)
      REAL CATABL(13,6)
!
!
      PARAMETER (NUMALP=8)
      REAL ALPHA(NUMALP)
!
      CHARACTER*1 IBASLC
      PARAMETER(NUMCLI=4)
      PARAMETER(MAXLIN=2)
      PARAMETER (MAXROW=50)
      CHARACTER*60 ITITLE
      CHARACTER*60 ITITLZ
      CHARACTER*60 ITITL9
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
      INCLUDE 'DPCOP2.INC'
!
      DATA (GPTABL(1,J),J=1,8)/   &
       0.339, 0.471, 0.641, 0.771, 0.905, 1.086, 1.226, 1.559/
      DATA (GPTABL(2,J),J=1,8)/   &
       0.356, 0.499, 0.685, 0.830, 0.978, 1.180, 1.336, 1.707/
      DATA (GPTABL(3,J),J=1,8)/   &
       0.376, 0.534, 0.741, 0.903, 1.069, 1.296, 1.471, 1.893/
      DATA (GPTABL(4,J),J=1,8)/   &
       0.386, 0.550, 0.766, 0.935, 1.110, 1.348, 1.532, 1.966/
      DATA (GPTABL(5,J),J=1,8)/   &
       0.397, 0.569, 0.796, 0.974, 1.158, 1.409, 1.603, 2.064/
      DATA (GPTABL(6,J),J=1,8)/   &
       0.410, 0.591, 0.831, 1.020, 1.215, 1.481, 1.687, 2.176/
      DATA (GPTABL(7,J),J=1,8)/   &
       0.426, 0.617, 0.873, 1.074, 1.283, 1.567, 1.788, 2.314/
      DATA (GPTABL(8,J),J=1,8)/   &
       0.445, 0.649, 0.924, 1.140, 1.365, 1.672, 1.909, 2.475/
      DATA (GPTABL(9,J),J=1,8)/   &
       0.468, 0.688, 0.985, 1.221, 1.465, 1.799, 2.058, 2.674/
      DATA (GPTABL(10,J),J=1,8)/   &
       0.496, 0.735, 1.061, 1.321, 1.590, 1.958, 2.243, 2.922/
!
      DATA (GATABL(I,1),I=1,12)/   &
       0.486, 0.477, 0.475, 0.473, 0.472, 0.472, 0.471,   &
       0.471, 0.471, 0.470, 0.470, 0.470 /
      DATA (GATABL(I,2),I=1,12)/   &
       0.657, 0.643, 0.639, 0.637, 0.635, 0.635, 0.634,   &
       0.633, 0.633, 0.632, 0.632, 0.631 /
      DATA (GATABL(I,3),I=1,12)/   &
       0.786, 0.768, 0.762, 0.759, 0.758, 0.757, 0.755,   &
       0.754, 0.754, 0.754, 0.753, 0.752 /
      DATA (GATABL(I,4),I=1,12)/   &
       0.917, 0.894, 0.886, 0.883, 0.881, 0.880, 0.878,   &
       0.877, 0.876, 0.876, 0.875, 0.873 /
      DATA (GATABL(I,5),I=1,12)/   &
       1.092, 1.062, 1.052, 1.048, 1.045, 1.043, 1.041,   &
       0.040, 1.039, 1.038, 1.037, 1.035 /
      DATA (GATABL(I,6),I=1,12)/   &
       1.227, 1.190, 1.178, 1.173, 1.170, 1.168, 1.165,   &
       1.164, 1.163, 1.162, 1.161, 1.159 /
!
      DATA (CATABL(I,1),I=1,13)/   &
       0.835, 0.992, 1.04,  1.04,  1.02,  0.975, 0.914,   &
       0.875, 0.812, 0.774, 0.743, 0.689, 0.615 /
      DATA (CATABL(I,2),I=1,13)/   &
       1.14,  1.52,  1.63,  1.65,  1.61,  1.51,  1.40,   &
       1.30,  1.16,  1.08,  1.02,  0.927, 0.780 /
      DATA (CATABL(I,3),I=1,13)/   &
       1.40,  2.06,  2.27,  2.33,  2.28,  2.12,  1.94,   &
       1.76,  1.53,  1.41,  1.30,  1.14,  0.949 /
      DATA (CATABL(I,4),I=1,13)/   &
       1.77,  3.20,  3.77,  4.14,  4.25,  4.05,  3.57,   &
       3.09,  2.48,  2.14,  1.92,  1.52,  1.225 /
      DATA (CATABL(I,5),I=1,13)/   &
       2.00,  4.27,  5.58,  6.43,  7.20,  7.58,  6.91,   &
       5.86,  4.23,  3.37,  2.76,  2.05,  1.52  /
      DATA (CATABL(I,6),I=1,13)/   &
       2.16,  5.24,  7.50,  9.51, 11.50, 14.57, 14.96,   &
      13.80, 10.20,  7.49,  5.32,  3.30,  1.90  /
!
      DATA ALPHA/   &
       0.0, 50.0, 75.0, 90.0, 95.0, 97.5, 99.0, 99.5/
!
!-----START POINT-----------------------------------------------------
!
!
      ISUBN1='DPAD'
      ISUBN2='A3  '
      IERROR='NO'
      IWRITE='OFF'
      CALL DPCONA(92,IBASLC)
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ADA3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)
   71   FORMAT('***** AT THE BEGINNING OF DPADA3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)MINMAX,ICASPL,IDIST
   72   FORMAT('MINMAX,ICASPL,IDIST = ',I5,2X,A4,2X,A60)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,73)N,XMIN,XMAX,XMEAN,XSD
   73   FORMAT('N,XMIN,XMAX,XMEAN,XSD = ',I8,4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *******************************************
!               **   STEP 1--                            **
!               **   DETERMINE CRITICAL VALUES FROM      **
!               **   PUBLISHED TABLES (AVAILABLE FOR     **
!               **   ABOUT A DOZEN DISTRIBUTIONS ONLY)   **
!               *******************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ADA3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IADCVT=IADCVM
      STATV2=STATVA
      IF(IADCVM.EQ.'TABL')THEN
        IF(ICASPL.EQ.'NORM' .OR. ICASPL.EQ.'LOGN')THEN
          STATV2=STATVA*(1.0 + 4.0/REAL(N) - 25.0/(REAL(N)*REAL(N)))
          IF(N.GT.100)THEN
            CUT90=0.656
            CUT95=0.787
            CUT975=0.918
            CUT99=1.092
          ELSEIF(N.GT.50)THEN
            CUT90=0.631
            CUT95=0.754
            CUT975=0.884
            CUT99=1.047
          ELSEIF(N.GT.20)THEN
            CUT90=0.616
            CUT95=0.735
            CUT975=0.861
            CUT99=1.021
          ELSEIF(N.GT.10)THEN
            CUT90=0.591
            CUT95=0.704
            CUT975=0.815
            CUT99=0.969
          ELSE
            CUT90=0.578
            CUT95=0.683
            CUT975=0.779
            CUT99=0.926
          ENDIF
        ELSEIF(ICASPL.EQ.'EXPO')THEN
          STATV2=STATVA*(1.0 + 0.6/REAL(N))
          IF(N.GT.100)THEN
            CUT90=1.078
            CUT95=1.341
            CUT975=1.606
            CUT99=1.957
          ELSEIF(N.GT.50)THEN
            CUT90=1.070
            CUT95=1.330
            CUT975=1.595
            CUT99=1.951
          ELSEIF(N.GT.20)THEN
            CUT90=1.062
            CUT95=1.323
            CUT975=1.582
            CUT99=1.945
          ELSEIF(N.GT.10)THEN
            CUT90=1.045
            CUT95=1.300
            CUT975=1.556
            CUT99=1.927
          ELSE
            CUT90=1.022
            CUT95=1.265
            CUT975=1.515
            CUT99=1.888
          ENDIF
        ELSEIF(ICASPL.EQ.'WEIB' .OR. ICASPL.EQ.'EV2 ')THEN
          STATV2=(1.0+0.2/SQRT(REAL(N)))*STATVA
          CUT90=0.637
          CUT95=0.757
          CUT975=0.877
          CUT99=1.038
        ELSEIF(ICASPL.EQ.'EV1 ')THEN
          STATV2=(1.0+1.0/(5.0*SQRT(REAL(N))))*STATVA
          CUT90=0.637
          CUT95=0.757
          CUT975=0.877
          CUT99=1.038
        ELSEIF(ICASPL.EQ.'LOGI')THEN
          STATV2=STATVA*(1.0+0.25/REAL(N))
          CUT90=0.563
          CUT95=0.660
          CUT975=0.769
          CUT99=0.906
        ELSEIF(ICASPL.EQ.'UNIF')THEN
          CUT90=1.933
          CUT95=2.492
          CUT975=3.070
          CUT99=3.857
        ELSEIF(ICASPL.EQ.'GAMM')THEN
          IF(SHAPE1.LE.1.5)THEN
            CUT90 =GATABL(1,2)
            CUT95 =GATABL(1,3)
            CUT975=GATABL(1,4)
            CUT99 =GATABL(1,5)
          ELSEIF(SHAPE1.LE.2.5)THEN
            CUT90 =GATABL(2,2)
            CUT95 =GATABL(2,3)
            CUT975=GATABL(2,4)
            CUT99 =GATABL(2,5)
          ELSEIF(SHAPE1.LE.3.5)THEN
            CUT90 =GATABL(3,2)
            CUT95 =GATABL(3,3)
            CUT975=GATABL(3,4)
            CUT99 =GATABL(3,5)
          ELSEIF(SHAPE1.LE.4.5)THEN
            CUT90 =GATABL(4,2)
            CUT95 =GATABL(4,3)
            CUT975=GATABL(4,4)
            CUT99 =GATABL(4,5)
          ELSEIF(SHAPE1.LE.5.5)THEN
            CUT90 =GATABL(5,2)
            CUT95 =GATABL(5,3)
            CUT975=GATABL(5,4)
            CUT99 =GATABL(5,5)
          ELSEIF(SHAPE1.LE.7.0)THEN
            CUT90 =GATABL(6,2)
            CUT95 =GATABL(6,3)
            CUT975=GATABL(6,4)
            CUT99 =GATABL(6,5)
          ELSEIF(SHAPE1.LE.9.0)THEN
            CUT90 =GATABL(7,2)
            CUT95 =GATABL(7,3)
            CUT975=GATABL(7,4)
            CUT99 =GATABL(7,5)
          ELSEIF(SHAPE1.LE.11.0)THEN
            CUT90 =GATABL(8,2)
            CUT95 =GATABL(8,3)
            CUT975=GATABL(8,4)
            CUT99 =GATABL(8,5)
          ELSEIF(SHAPE1.LE.13.5)THEN
            CUT90 =GATABL(9,2)
            CUT95 =GATABL(9,3)
            CUT975=GATABL(9,4)
            CUT99 =GATABL(9,5)
          ELSEIF(SHAPE1.LE.17.5)THEN
            CUT90 =GATABL(10,2)
            CUT95 =GATABL(10,3)
            CUT975=GATABL(10,4)
            CUT99 =GATABL(10,5)
          ELSEIF(SHAPE1.LE.22.5)THEN
            CUT90 =GATABL(11,2)
            CUT95 =GATABL(11,3)
            CUT975=GATABL(11,4)
            CUT99 =GATABL(11,5)
          ELSE
            CUT90 =GATABL(12,2)
            CUT95 =GATABL(12,3)
            CUT975=GATABL(12,4)
            CUT99 =GATABL(12,5)
          ENDIF
        ELSEIF(ICASPL.EQ.'GPAR')THEN
          G=SHAPE1
          IF(G.LE.-0.90)THEN
            CUT90=GPTABL(1,3)
            CUT95=GPTABL(1,4)
            CUT975=GPTABL(1,5)
            CUT99=GPTABL(1,6)
          ELSEIF(G.GE.0.50)THEN
            CUT90=GPTABL(10,3)
            CUT95=GPTABL(10,4)
            CUT975=GPTABL(10,5)
            CUT99=GPTABL(10,6)
          ELSEIF(G.GT.-0.90 .AND. G.LE.-0.50)THEN
            A1=-0.5
            A2=-0.9
            AFACT=(G-A2)/(A1-A2)
            CUT90= GPTABL(1,3) + AFACT*(GPTABL(2,3)-GPTABL(1,3))
            CUT95= GPTABL(1,4) + AFACT*(GPTABL(2,4)-GPTABL(1,4))
            CUT975=GPTABL(1,5) + AFACT*(GPTABL(2,5)-GPTABL(1,5))
            CUT99= GPTABL(1,6) + AFACT*(GPTABL(2,6)-GPTABL(1,6))
          ELSEIF(G.GT.-0.50 .AND. G.LE.-0.20)THEN
            A1=-0.2
            A2=-0.5
            AFACT=(G-A2)/(A1-A2)
            CUT90= GPTABL(2,3) + AFACT*(GPTABL(3,3)-GPTABL(2,3))
            CUT95= GPTABL(2,4) + AFACT*(GPTABL(3,4)-GPTABL(2,4))
            CUT975=GPTABL(2,5) + AFACT*(GPTABL(3,5)-GPTABL(2,5))
            CUT99= GPTABL(2,6) + AFACT*(GPTABL(3,6)-GPTABL(2,6))
          ELSEIF(G.GT.-0.20 .AND. G.LE.-0.10)THEN
            A1=-0.1
            A2=-0.2
            AFACT=(G-A2)/(A1-A2)
            CUT90= GPTABL(3,3) + AFACT*(GPTABL(4,3)-GPTABL(3,3))
            CUT95= GPTABL(3,4) + AFACT*(GPTABL(4,4)-GPTABL(3,4))
            CUT975=GPTABL(3,5) + AFACT*(GPTABL(4,5)-GPTABL(3,5))
            CUT99= GPTABL(3,6) + AFACT*(GPTABL(4,6)-GPTABL(3,6))
          ELSEIF(G.GT.-0.10 .AND. G.LE.0.0)THEN
            A1=0.0
            A2=-0.1
            AFACT=(G-A2)/(A1-A2)
            CUT90= GPTABL(4,3) + AFACT*(GPTABL(5,3)-GPTABL(4,3))
            CUT95= GPTABL(4,4) + AFACT*(GPTABL(5,4)-GPTABL(4,4))
            CUT975=GPTABL(4,5) + AFACT*(GPTABL(5,5)-GPTABL(4,5))
            CUT99= GPTABL(4,6) + AFACT*(GPTABL(5,6)-GPTABL(4,6))
          ELSEIF(G.GT.0.0 .AND. G.LE.0.10)THEN
            A1=0.1
            A2=0.0
            AFACT=(G-A2)/(A1-A2)
            CUT90= GPTABL(5,3) + AFACT*(GPTABL(6,3)-GPTABL(5,3))
            CUT95= GPTABL(5,4) + AFACT*(GPTABL(6,4)-GPTABL(5,4))
            CUT975=GPTABL(5,5) + AFACT*(GPTABL(6,5)-GPTABL(5,5))
            CUT99= GPTABL(5,6) + AFACT*(GPTABL(6,6)-GPTABL(5,6))
          ELSEIF(G.GT.0.10 .AND. G.LE.0.20)THEN
            A1=0.2
            A2=0.1
            AFACT=(G-A2)/(A1-A2)
            CUT90= GPTABL(6,3) + AFACT*(GPTABL(7,3)-GPTABL(6,3))
            CUT95= GPTABL(6,4) + AFACT*(GPTABL(7,4)-GPTABL(6,4))
            CUT975=GPTABL(6,5) + AFACT*(GPTABL(7,5)-GPTABL(6,5))
            CUT99= GPTABL(6,6) + AFACT*(GPTABL(7,6)-GPTABL(6,6))
          ELSEIF(G.GT.0.20 .AND. G.LE.0.30)THEN
            A1=0.3
            A2=0.2
            AFACT=(G-A2)/(A1-A2)
            CUT90= GPTABL(7,3) + AFACT*(GPTABL(8,3)-GPTABL(7,3))
            CUT95= GPTABL(7,4) + AFACT*(GPTABL(8,4)-GPTABL(7,4))
            CUT975=GPTABL(7,5) + AFACT*(GPTABL(8,5)-GPTABL(7,5))
            CUT99= GPTABL(7,6) + AFACT*(GPTABL(8,6)-GPTABL(7,6))
          ELSEIF(G.GT.0.30 .AND. G.LE.0.40)THEN
            A1=0.4
            A2=0.3
            AFACT=(G-A2)/(A1-A2)
            CUT90= GPTABL(8,3) + AFACT*(GPTABL(9,3)-GPTABL(8,3))
            CUT95= GPTABL(8,4) + AFACT*(GPTABL(9,4)-GPTABL(8,4))
            CUT975=GPTABL(8,5) + AFACT*(GPTABL(9,5)-GPTABL(8,5))
            CUT99= GPTABL(8,6) + AFACT*(GPTABL(9,6)-GPTABL(8,6))
          ELSEIF(G.GT.0.40 .AND. G.LT.0.50)THEN
            A1=0.5
            A2=0.4
            AFACT=(G-A2)/(A1-A2)
            CUT90= GPTABL(9,3) + AFACT*(GPTABL(10,3)-GPTABL(9,3))
            CUT95= GPTABL(9,4) + AFACT*(GPTABL(10,4)-GPTABL(9,4))
            CUT975=GPTABL(9,5) + AFACT*(GPTABL(10,5)-GPTABL(9,5))
            CUT99= GPTABL(9,6) + AFACT*(GPTABL(10,6)-GPTABL(9,6))
          ENDIF
        ELSEIF(ICASPL.EQ.'DEXP')THEN
          IF(N.LE.10)THEN
            CUT90=0.714
            CUT95=0.869
            CUT975=1.023
            CUT99=1.234
          ELSEIF(N.GE.10 .AND. N.LE.14)THEN
            AFACT=REAL(N-10)/REAL(15-10)
            CUT90=0.714 + (0.807 - 0.714)*AFACT
            CUT95=0.869 + (0.991 - 0.869)*AFACT
            CUT975=1.023 + (1.160 - 1.023)*AFACT
            CUT99=1.234 + (1.415 - 1.234)*AFACT
          ELSEIF(N.EQ.15)THEN
            CUT90=0.807
            CUT95=0.991
            CUT975=1.160
            CUT99=1.415
          ELSEIF(N.GE.16 .AND. N.LE.19)THEN
            AFACT=REAL(N-15)/REAL(20-15)
            CUT90=0.807 + (0.760 - 0.807)*AFACT
            CUT95=0.991 + (0.930 - 0.991)*AFACT
            CUT975=1.160 + (1.103 - 1.160)*AFACT
            CUT99=1.415 + (1.336 - 1.415)*AFACT
          ELSEIF(N.EQ.20)THEN
            CUT90=0.760
            CUT95=0.930
            CUT975=1.103
            CUT99=1.336
          ELSEIF(N.GE.21 .AND. N.LE.34)THEN
            AFACT=REAL(N-20)/REAL(35-20)
            CUT90=0.760 + (0.797 - 0.760)*AFACT
            CUT95=0.930 + (0.987 - 0.930)*AFACT
            CUT975=1.103 + (1.179 - 1.103)*AFACT
            CUT99=1.336 + (1.438 - 1.336)*AFACT
          ELSEIF(N.EQ.35)THEN
            CUT90=0.797
            CUT95=0.987
            CUT975=1.179
            CUT99=1.438
          ELSEIF(N.GE.36 .AND. N.LE.49)THEN
            AFACT=REAL(N-35)/REAL(50-35)
            CUT90=0.797 + (0.783 - 0.797)*AFACT
            CUT95=0.987 + (0.961 - 0.987)*AFACT
            CUT975=1.179 + (1.137 - 1.179)*AFACT
            CUT99=1.438 + (1.373 - 1.438)*AFACT
          ELSEIF(N.EQ.50)THEN
            CUT90=0.783
            CUT95=0.961
            CUT975=1.137
            CUT99=1.373
          ELSEIF(N.GE.51 .AND. N.LE.74)THEN
            AFACT=REAL(N-50)/REAL(75-50)
            CUT90=0.783 + (0.797 - 0.783)*AFACT
            CUT95=0.961 + (0.984 - 0.961)*AFACT
            CUT975=1.137 + (1.178 - 1.137)*AFACT
            CUT99=1.373 + (1.442 - 1.373)*AFACT
          ELSEIF(N.EQ.75)THEN
            CUT90=0.797
            CUT95=0.984
            CUT975=1.178
            CUT99=1.442
          ELSEIF(N.GE.76 .AND. N.LE.99)THEN
            AFACT=REAL(N-75)/REAL(100-75)
            CUT90=0.797 + (0.792 - 0.797)*AFACT
            CUT95=0.984 + (0.972 - 0.984)*AFACT
            CUT975=1.178 + (1.156 - 1.178)*AFACT
            CUT99=1.442 + (1.408 - 1.442)*AFACT
          ELSEIF(N.EQ.100)THEN
            CUT90=0.792
            CUT95=0.972
            CUT975=1.156
            CUT99=1.408
          ELSE
            CUT90=0.798
            CUT95=0.983
            CUT975=1.177
            CUT99=1.442
          ENDIF
        ELSEIF(ICASPL.EQ.'CAUC')THEN
          IF(N.LE.5)THEN
            CUT90=CATABL(1,3)
            CUT95=CATABL(1,4)
            CUT975=CATABL(1,5)
            CUT99=CATABL(1,6)
          ELSEIF(N.GT.5 .AND. N.LE.8)THEN
            AFACT=REAL(N-5)/REAL(8-5)
            CUT90=CATABL(1,3) + (CATABL(2,3) - CATABL(1,3))*AFACT
            CUT95=CATABL(1,4) + (CATABL(2,4) - CATABL(1,4))*AFACT
            CUT975=CATABL(1,5) + (CATABL(2,5) - CATABL(1,5))*AFACT
            CUT99=CATABL(1,6) + (CATABL(2,6) - CATABL(1,6))*AFACT
          ELSEIF(N.GE.9 .AND. N.LE.10)THEN
            AFACT=REAL(N-8)/REAL(10-8)
            CUT90=CATABL(2,3) + (CATABL(3,3) - CATABL(2,3))*AFACT
            CUT95=CATABL(2,4) + (CATABL(3,4) - CATABL(2,4))*AFACT
            CUT975=CATABL(2,5) + (CATABL(3,5) - CATABL(2,5))*AFACT
            CUT99=CATABL(2,6) + (CATABL(3,6) - CATABL(2,6))*AFACT
          ELSEIF(N.GE.11 .AND. N.LE.12)THEN
            AFACT=REAL(N-10)/REAL(12-10)
            CUT90=CATABL(3,3) + (CATABL(4,3) - CATABL(3,3))*AFACT
            CUT95=CATABL(3,4) + (CATABL(4,4) - CATABL(3,4))*AFACT
            CUT975=CATABL(3,5) + (CATABL(4,5) - CATABL(3,5))*AFACT
            CUT99=CATABL(3,6) + (CATABL(4,6) - CATABL(3,6))*AFACT
          ELSEIF(N.GE.13 .AND. N.LE.15)THEN
            AFACT=REAL(N-12)/REAL(15-12)
            CUT90=CATABL(4,3) + (CATABL(5,3) - CATABL(4,3))*AFACT
            CUT95=CATABL(4,4) + (CATABL(5,4) - CATABL(4,4))*AFACT
            CUT975=CATABL(4,5) + (CATABL(5,5) - CATABL(4,5))*AFACT
            CUT99=CATABL(4,6) + (CATABL(5,6) - CATABL(4,6))*AFACT
          ELSEIF(N.GE.16 .AND. N.LE.20)THEN
            AFACT=REAL(N-15)/REAL(20-15)
            CUT90=CATABL(5,3) + (CATABL(6,3) - CATABL(5,3))*AFACT
            CUT95=CATABL(5,4) + (CATABL(6,4) - CATABL(5,4))*AFACT
            CUT975=CATABL(5,5) + (CATABL(6,5) - CATABL(5,5))*AFACT
            CUT99=CATABL(5,6) + (CATABL(6,6) - CATABL(5,6))*AFACT
          ELSEIF(N.GE.21 .AND. N.LE.25)THEN
            AFACT=REAL(N-20)/REAL(25-20)
            CUT90=CATABL(6,3) + (CATABL(7,3) - CATABL(6,3))*AFACT
            CUT95=CATABL(6,4) + (CATABL(7,4) - CATABL(6,4))*AFACT
            CUT975=CATABL(6,5) + (CATABL(7,5) - CATABL(6,5))*AFACT
            CUT99=CATABL(6,6) + (CATABL(7,6) - CATABL(6,6))*AFACT
          ELSEIF(N.GE.26 .AND. N.LE.30)THEN
            AFACT=REAL(N-25)/REAL(30-25)
            CUT90=CATABL(7,3) + (CATABL(8,3) - CATABL(7,3))*AFACT
            CUT95=CATABL(7,4) + (CATABL(8,4) - CATABL(7,4))*AFACT
            CUT975=CATABL(7,5) + (CATABL(8,5) - CATABL(7,5))*AFACT
            CUT99=CATABL(7,6) + (CATABL(8,6) - CATABL(7,6))*AFACT
          ELSEIF(N.GE.31 .AND. N.LE.40)THEN
            AFACT=REAL(N-30)/REAL(40-30)
            CUT90=CATABL(8,3) + (CATABL(9,3) - CATABL(8,3))*AFACT
            CUT95=CATABL(8,4) + (CATABL(9,4) - CATABL(8,4))*AFACT
            CUT975=CATABL(8,5) + (CATABL(9,5) - CATABL(8,5))*AFACT
            CUT99=CATABL(8,6) + (CATABL(9,6) - CATABL(8,6))*AFACT
          ELSEIF(N.GE.41 .AND. N.LE.50)THEN
            AFACT=REAL(N-40)/REAL(50-40)
            CUT90=CATABL(9,3) + (CATABL(10,3) - CATABL(9,3))*AFACT
            CUT95=CATABL(9,4) + (CATABL(10,4) - CATABL(9,4))*AFACT
            CUT975=CATABL(9,5) + (CATABL(10,5) - CATABL(9,5))*AFACT
            CUT99=CATABL(9,6) + (CATABL(10,6) - CATABL(9,6))*AFACT
          ELSEIF(N.GE.51 .AND. N.LE.60)THEN
            AFACT=REAL(N-50)/REAL(60-50)
            CUT90=CATABL(10,3) + (CATABL(11,3) - CATABL(10,3))*AFACT
            CUT95=CATABL(10,4) + (CATABL(11,4) - CATABL(10,4))*AFACT
            CUT975=CATABL(10,5) + (CATABL(11,5) - CATABL(10,5))*AFACT
            CUT99=CATABL(10,6) + (CATABL(11,6) - CATABL(10,6))*AFACT
          ELSEIF(N.GE.61 .AND. N.LE.100)THEN
            AFACT=REAL(N-50)/REAL(100-50)
            CUT90=CATABL(11,3) + (CATABL(12,3) - CATABL(11,3))*AFACT
            CUT95=CATABL(11,4) + (CATABL(12,4) - CATABL(11,4))*AFACT
            CUT975=CATABL(11,5) + (CATABL(12,5) - CATABL(11,5))*AFACT
            CUT99=CATABL(11,6) + (CATABL(12,6) - CATABL(11,6))*AFACT
          ELSE
            CUT90=CATABL(13,3)
            CUT95=CATABL(13,4)
            CUT975=CATABL(13,5)
            CUT99=CATABL(13,6)
          ENDIF
        ENDIF
        CDF1=CUT90
        CDF2=CUT95
        CDF3=CUT975
        CDF4=CUT99
      ELSE
        IF(IADCVM.NE.'NONE')IADCVM='SIMU'
      ENDIF
!
!               *******************************************
!               **   STEP 41--                           **
!               **   WRITE OUT INITIAL HEADER TABLE      **
!               **   FOR NORMAL MLE ESTIMATE             **
!               *******************************************
!
      ISTEPN='41'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ADA3')   &
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
      ITITLE='Anderson-Darling Goodness of Fit Test'
      NCTITL=37
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
      DO 4101 I=1,NREPL
        ICNT=ICNT+1
        ITEXT(ICNT)='Factor Variable  : '
        WRITE(ITEXT(ICNT)(17:17),'(I1)')I
        WRITE(ITEXT(ICNT)(20:23),'(A4)')IVARID(I+1)(1:4)
        WRITE(ITEXT(ICNT)(24:27),'(A4)')IVARI2(I+1)(1:4)
        NCTEXT(ICNT)=27
        AVALUE(ICNT)=PID(I+1)
        IDIGIT(ICNT)=NUMDIG
 4101 CONTINUE
!
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='H0: The distribution fits the data'
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: The distribution does not fit the data'
      NCTEXT(ICNT)=43
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      IEND=46
      DO 4111 I=46,1,-1
        IF(IDIST(I:I).NE.' ')THEN
          IEND=I
          GO TO 4119
        ENDIF
 4111 CONTINUE
      IEND=1
 4119 CONTINUE
      CALL EXTBOU(ICASPL,IBOUND)
!
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)(1:14)='Distribution: '
      ISTRT=15
      ISTOP=15+IEND-1
      ITEXT(ICNT)(ISTRT:ISTOP)=IDIST(1:IEND)
      NCTEXT(ICNT)=ISTOP
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      IF(IBOUND.EQ.0)THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='Location Parameter:'
        NCTEXT(ICNT)=19
        AVALUE(ICNT)=KSLOC
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='Scale Parameter:'
        NCTEXT(ICNT)=16
        AVALUE(ICNT)=KSSCAL
        IDIGIT(ICNT)=NUMDIG
      ELSE
        ICNT=ICNT+1
        ITEXT(ICNT)='Lower Limit Parameter:'
        NCTEXT(ICNT)=22
        AVALUE(ICNT)=A
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='Upper Limit Parameter:'
        NCTEXT(ICNT)=22
        AVALUE(ICNT)=B
        IDIGIT(ICNT)=NUMDIG
      ENDIF
      IF(NUMSHA.GE.1)THEN
        DO 4140 I=1,NUMSHA
          ICNT=ICNT+1
          ITEXT(ICNT)='Shape Parameter  :'
          WRITE(ITEXT(ICNT)(17:17),'(I1)')I
          NCTEXT(ICNT)=18
          IF(I.EQ.1)THEN
            AVALUE(ICNT)=SHAPE1
          ELSEIF(I.EQ.2)THEN
            AVALUE(ICNT)=SHAPE2
          ELSEIF(I.EQ.3)THEN
            AVALUE(ICNT)=SHAPE3
          ELSEIF(I.EQ.4)THEN
            AVALUE(ICNT)=SHAPE4
          ELSEIF(I.EQ.5)THEN
            AVALUE(ICNT)=SHAPE5
          ELSEIF(I.EQ.6)THEN
            AVALUE(ICNT)=SHAPE6
          ELSEIF(I.EQ.7)THEN
            AVALUE(ICNT)=SHAPE7
          ENDIF
          IDIGIT(ICNT)=NUMDIG
 4140   CONTINUE
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
      AVALUE(ICNT)=XMIN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Maximum:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=XMAX
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Mean:'
      NCTEXT(ICNT)=12
      AVALUE(ICNT)=XMEAN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample SD:'
      NCTEXT(ICNT)=10
      AVALUE(ICNT)=XSD
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Anderson-Darling Test Statistic Value:'
      NCTEXT(ICNT)=38
      AVALUE(ICNT)=STATVA
      IDIGIT(ICNT)=NUMDIG
!
      IF(IGOFFM.EQ.'NULL')GO TO 4149
      IF(IADCVM.EQ.'SIMU')THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='Number of Monte Carlo Simulations:'
        NCTEXT(ICNT)=34
        AVALUE(ICNT)=REAL(NMCSAM)
        IDIGIT(ICNT)=NUMDIG
        IF(NMCSAM.NE.NCNT)THEN
          ICNT=ICNT+1
          ITEXT(ICNT)='Number of Samples Rejected:'
          NCTEXT(ICNT)=27
          AVALUE(ICNT)=REAL(NMCSAM-NCNT)
          IDIGIT(ICNT)=NUMDIG
        ENDIF
        ICNT=ICNT+1
        ITEXT(ICNT)='CDF Value:'
        NCTEXT(ICNT)=10
        STACDF=1.0 - PVAL
        AVALUE(ICNT)=STACDF
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='P-Value:'
        NCTEXT(ICNT)=7
        AVALUE(ICNT)=PVAL
        IDIGIT(ICNT)=NUMDIG
      ELSEIF(IADCVM.NE.'NONE')THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='Adjusted Test Statistic Value:'
        NCTEXT(ICNT)=30
        AVALUE(ICNT)=STATV2
        IDIGIT(ICNT)=NUMDIG
      ENDIF
!
 4149 CONTINUE
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
      ITITLZ=' '
      NCTITZ=0
      IF(IGOFFM.NE.'NULL')THEN
        IF(IADCVM.EQ.'TABL')THEN
          ITITLZ='(Critical Values from Published Tables)'
          NCTITZ=39
        ELSEIF(IADCVM.EQ.'SIMU')THEN
          IF(IGOFFS.EQ.'ON')THEN
            ITITLZ='(Fully Specified Model)'
            NCTITZ=23
          ELSE
            ITITLZ='(Parameters Estimated from the Data)'
            NCTITZ=36
          ENDIF
        ENDIF
      ENDIF
      IFRST=.TRUE.
      ILAST=.TRUE.
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                  AVALUE,IDIGIT,   &
                  NTOT,NUMROW,   &
                  ICAPSW,ICAPTY,ILAST,IFRST,   &
                  ISUBRO,IBUGA3,IERROR)
      IF(IADCVM.EQ.'NONE')GO TO 9000
      IF(IGOFFM.EQ.'NULL')GO TO 9000
      ITITLE=' '
      NCTITL=0
      ITITL9=' '
      NCTIT9=0
!
      IF(IADCVM.EQ.'SIMU')THEN
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
        DO 2521 I=1,NUMCOL
          VALIGN(I)='b'
          ALIGN(I)='r'
          NTOT(I)=15
          IF(I.EQ.2)NTOT(I)=5
          NMAX=NMAX+NTOT(I)
          IDIGIT(I)=NUMDIG
          ITYPCO(I)='NUME'
 2521   CONTINUE
        ITYPCO(2)='ALPH'
        IDIGIT(1)=1
        IDIGIT(3)=3
        DO 2523 I=1,NUMROW
          DO 2525 J=1,NUMCOL
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
              IF(I.GE.2)THEN
                P100=ALPHA(I)
                CALL PERCEN(P100,YSTAT,NCNT,IWRITE,XTEMP,MAXNXT,   &
                            XPERC,IBUGA3,IERROR)
                XPERC2=RND(XPERC,3)
                AMAT(I,J)=XPERC2
              ENDIF
            ENDIF
 2525     CONTINUE
 2523   CONTINUE
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
        CALL DPDTA4(ITITL9,NCTIT9,   &
                    ITITLE,NCTITL,ITITL2,NCTIT2,   &
                    MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                    IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,   &
                    IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                    ICAPSW,ICAPTY,IFRST,ILAST,   &
                    ISUBRO,IBUGA3,IERROR)
!
        CDF1=AMAT(4,3)
        CDF2=AMAT(5,3)
        CDF3=AMAT(6,3)
        CDF4=AMAT(7,3)
!
        ITITL9=' '
        NCTIT9=0
        ITITLE='Conclusions (Upper 1-Tailed Test)'
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
        DO 2821 I=1,NUMCOL
          VALIGN(I)='b'
          ALIGN(I)='r'
          NTOT(I)=15
          IF(I.EQ.1 .OR. I.EQ.2)NTOT(I)=7
          IF(I.EQ.3)NTOT(I)=17
          NMAX=NMAX+NTOT(I)
!CCCC     IDIGIT(I)=NUMDIG
          IDIGIT(I)=3
          ITYPCO(I)='ALPH'
 2821   CONTINUE
        ITYPCO(3)='NUME'
        IDIGIT(1)=0
        IDIGIT(2)=0
        DO 2823 I=1,NUMROW
          DO 2825 J=1,NUMCOL
            NCVALU(I,J)=0
            IVALUE(I,J)=' '
            NCVALU(I,J)=0
            AMAT(I,J)=0.0
 2825     CONTINUE
 2823   CONTINUE
        IVALUE(1,1)='10%'
        IVALUE(2,1)='5%'
        IVALUE(3,1)='2.5%'
        IVALUE(4,1)='1%'
        IVALUE(1,2)='90%'
        IVALUE(2,2)='95%'
        IVALUE(3,2)='97.5%'
        IVALUE(4,2)='99%'
        NCVALU(1,1)=3
        NCVALU(2,1)=2
        NCVALU(3,1)=4
        NCVALU(4,1)=2
        NCVALU(1,2)=3
        NCVALU(2,2)=3
        NCVALU(3,2)=5
        NCVALU(4,2)=3
        IVALUE(1,4)='Accept H0'
        IVALUE(2,4)='Accept H0'
        IVALUE(3,4)='Accept H0'
        IVALUE(4,4)='Accept H0'
        NCVALU(1,4)=9
        NCVALU(2,4)=9
        NCVALU(3,4)=9
        NCVALU(4,4)=9
        IF(STATVA.GT.CDF1)IVALUE(1,4)='Reject H0'
        IF(STATVA.GT.CDF2)IVALUE(2,4)='Reject H0'
        IF(STATVA.GT.CDF3)IVALUE(3,4)='Reject H0'
        IF(STATVA.GT.CDF4)IVALUE(4,4)='Reject H0'
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
!       FOR LATEX, WE WANT TO ENSURE THAT TRAILING LINE IS PART
!       OF THE TABLE SO THAT IT WILL BE PRINTED IN THE PROPER PLACE.
!
        IF(ICAPTY.EQ.'LATE')THEN
          ILAST=.FALSE.
        ELSE
          ILAST=.TRUE.
        ENDIF
!
        CALL DPDTA4(ITITL9,NCTIT9,   &
                    ITITLE,NCTITL,ITITL2,NCTIT2,   &
                    MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                    IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,   &
                    IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                    ICAPSW,ICAPTY,IFRST,ILAST,   &
                    ISUBRO,IBUGA3,IERROR)
!
        IF(IPRINT.EQ.'ON')THEN
!
        ITITLE(1:26)='*Critical Values Based on '
        WRITE(ITITLE(27:34),'(I8)')NCNT
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
          IRTFMD='OFF'
          IPTSZ=14
          WRITE(ICOUT,8199)IBASLC,IPTSZ
 8199     FORMAT(A1,'fs',I2)
          CALL DPWRST(ICOUT,'WRIT')
          IF(IRTFFF.EQ.'Courier New')THEN
            ITEMP=1
          ELSEIF(IRTFFF.EQ.'Lucida Console')THEN
            ITEMP=8
          ENDIF
          WRITE(ICOUT,8301)IBASLC,ITEMP
          CALL DPWRST(ICOUT,'WRIT')
          CALL DPRTFZ(ITITLE,NCTITL,CPUMIN,NUMDIG)
!CCCC     CALL DPRTF6(NHEAD)
!CCCC     CALL DPRTF6(NHEAD)
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
 8301     FORMAT(A1,'f',I1)
          CALL DPWRST(ICOUT,'WRIT')
!
!         END TABLE AND RESET "ASIS" MODE
!
          IF(IRTFFF.EQ.'Courier New')THEN
            ITEMP=1
          ELSEIF(IRTFFF.EQ.'Lucida Console')THEN
            ITEMP=8
          ENDIF
          WRITE(ICOUT,8091)IBASLC,ITEMP
 8091     FORMAT(A1,'f',I1)
          CALL DPWRST(ICOUT,'WRIT')
!
          CALL DPRTF6(NHEAD)
          CALL DPRTF6(NHEAD)
!
          IRTFMD='VERB'
!
        ELSE
          WRITE(ICOUT,2589)ITITLE(1:58)
 2589     FORMAT(A60)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        ENDIF
!
      ELSE
!
        ITITL9=' '
        NCTIT9=0
        ITITLE='Conclusions (Upper 1-Tailed Test)'
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
        DO 2421 I=1,NUMCOL
          VALIGN(I)='b'
          ALIGN(I)='r'
          NTOT(I)=15
          IF(I.EQ.1 .OR. I.EQ.2)NTOT(I)=7
          IF(I.EQ.3)NTOT(I)=17
          NMAX=NMAX+NTOT(I)
!CCCC     IDIGIT(I)=NUMDIG
          IDIGIT(I)=3
          ITYPCO(I)='ALPH'
 2421   CONTINUE
        ITYPCO(3)='NUME'
        IDIGIT(1)=0
        IDIGIT(2)=0
        DO 2423 I=1,NUMROW
          DO 2425 J=1,NUMCOL
            NCVALU(I,J)=0
            IVALUE(I,J)=' '
            NCVALU(I,J)=0
            AMAT(I,J)=0.0
 2425     CONTINUE
 2423   CONTINUE
        IVALUE(1,1)='10%'
        IVALUE(2,1)='5%'
        IVALUE(3,1)='2.5%'
        IVALUE(4,1)='1%'
        IVALUE(1,2)='90%'
        IVALUE(2,2)='95%'
        IVALUE(3,2)='97.5%'
        IVALUE(4,2)='99%'
        NCVALU(1,1)=3
        NCVALU(2,1)=2
        NCVALU(3,1)=4
        NCVALU(4,1)=2
        NCVALU(1,2)=3
        NCVALU(2,2)=3
        NCVALU(3,2)=5
        NCVALU(4,2)=3
        IVALUE(1,4)='Accept H0'
        IVALUE(2,4)='Accept H0'
        IVALUE(3,4)='Accept H0'
        IVALUE(4,4)='Accept H0'
        NCVALU(1,4)=9
        NCVALU(2,4)=9
        NCVALU(3,4)=9
        NCVALU(4,4)=9
        IF(STATVA.GT.CDF1)IVALUE(1,4)='Reject H0'
        IF(STATVA.GT.CDF2)IVALUE(2,4)='Reject H0'
        IF(STATVA.GT.CDF3)IVALUE(3,4)='Reject H0'
        IF(STATVA.GT.CDF4)IVALUE(4,4)='Reject H0'
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
        ILAST=.TRUE.
!
        CALL DPDTA4(ITITL9,NCTIT9,   &
                    ITITLE,NCTITL,ITITL2,NCTIT2,   &
                    MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                    IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,   &
                    IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                    ICAPSW,ICAPTY,IFRST,ILAST,   &
                    ISUBRO,IBUGA3,IERROR)
!
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IADCVM=IADCVT
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ADA3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPADA3--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPADA3
      SUBROUTINE DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGS2,IERROR)
!
!     PURPOSE--ADD A PARAMETER WITH NAME GIVEN IN    IH,IH2
!              AND WITH VALUE     VALUE0
!              INTO DATAPLOT'S INTERNAL ARRAY.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--93/12
!     ORIGINAL VERSION--DECEMBER  1993.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 IHOST1
      CHARACTER*4 ISUBN0
!
      CHARACTER*4 IHNAME
      CHARACTER*4 IHNAM2
      CHARACTER*4 IUSE
      CHARACTER*4 IANS
      CHARACTER*4 IBUGS2
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!CCCC CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      DIMENSION IHNAME(*)
      DIMENSION IHNAM2(*)
      DIMENSION IUSE(*)
      DIMENSION IVALUE(*)
      DIMENSION VALUE(*)
      DIMENSION IANS(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPAD'
      ISUBN2='DP  '
      IERROR='NO'
!
      IVAL=0
!
      IF(IBUGS2.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)ISUBN0
   51   FORMAT('***** AT THE BEGINNING OF DPADDP CALLED BY--',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGS2,IERROR,IH,IH2,VALUE0
   52   FORMAT('IBUGS2,IERROR,IH,IH2,VALUE0 = ',4(A4,2X),G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)IHOST1,ISUBN0,MAXNAM,NUMNAM
   54   FORMAT('IHOST1,ISUBN0,MAXNAM,NUMNAM = ',2(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        DO 60 I=1,NUMNAM
          WRITE(ICOUT,61)I,IHNAME(I),IHNAM2(I),IUSE(I),IVALUE(I),   &
                         VALUE(I)
   61     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVALUE(I),VALUE(I) = ',   &
                 I8,2X,A4,A4,2X,A4,I8,G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,62)I,IHNAME(I),IHNAM2(I)
   62     FORMAT('I,IHNAME(I),IHNAM2(I) = ',I8,2X,A4,A4,6X,3I8)
          CALL DPWRST('XXX','BUG ')
   60   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               **************************************************
!               **  DETERMINE IF THE NAME IS ALREADY IN         **
!               **  IN THE INTERNAL ARRAY.                      **
!               **  ADD OR UPDATE ACCORDINGLY.                  **
!               **************************************************
!
      ICUTMX=NUMBPW
      IF(IHOST1.EQ.'CDC '.OR.IHOST1.EQ.'CYBE')ICUTMX=48
      IF(IHOST1.EQ.'205 ')ICUTMX=48
      CUTOFF=2**(ICUTMX-3)
!
      DO 1150 I=1,NUMNAM
        I2=I
        IF(IH.EQ.IHNAME(I).AND.IH2.EQ.IHNAM2(I).AND.   &
           IUSE(I).EQ.'P')THEN
          VALUE(I2)=VALUE0
          VAL=VALUE(I2)
          IF((-CUTOFF).LE.VAL.AND.VAL.LE.CUTOFF)IVAL=INT(VAL+0.5)
          IF(VAL.GT.CUTOFF)IVAL=INT(CUTOFF)
          IF(VAL.LT.(-CUTOFF))IVAL=INT(-CUTOFF)
          IVALUE(I2)=IVAL
          GO TO 1190
        ENDIF
 1150 CONTINUE
!
      IF(NUMNAM.GE.MAXNAM)THEN
        WRITE(ICOUT,1151)ISUBN0
 1151   FORMAT('***** ERROR IN DPADDP AS CALLED FROM--',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1155)MAXNAM
 1155   FORMAT('      THE MAXIMUM ALLOWABLE NUMBER OF NAMES, ',I8,   &
               ', HAS JUST BEEN EXCEEDED.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1157)
 1157   FORMAT('      SUGGESTED ACTION--ENTER     STAT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1158)
 1158   FORMAT('      TO DETERMINE THE IMPORTANT (VERSUS UNIMPORTANT)')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1160)
 1160   FORMAT('      VARAIBLE AND PARAMETERS, AND THEN REUSE SOME ',   &
               'OF THE NAMES.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1162)
 1162   FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,1163)(IANS(I),I=1,MIN(80,IWIDTH))
 1163     FORMAT('      ',80A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      NUMNAM=NUMNAM+1
      ILOC=NUMNAM
      IHNAME(ILOC)=IH
      IHNAM2(ILOC)=IH2
      IUSE(ILOC)='P'
      VALUE(ILOC)=VALUE0
      VAL=VALUE(ILOC)
      IF((-CUTOFF).LE.VAL.AND.VAL.LE.CUTOFF)IVAL=INT(VAL+0.5)
      IF(VAL.GT.CUTOFF)IVAL=INT(CUTOFF)
      IF(VAL.LT.(-CUTOFF))IVAL=INT(-CUTOFF)
      IVALUE(ILOC)=IVAL
!
 1190 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
!
      IF(IBUGS2.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)ISUBN0
 9011   FORMAT('***** AT THE END       OF DPADDP CALLED BY--',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9018)IERROR,MAXNAM,NUMNAM
 9018   FORMAT('IERROR,MAXNAM,NUMNAM = ',A4,2X,2I8)
        CALL DPWRST('XXX','BUG ')
        DO 9020 I=1,NUMNAM
          WRITE(ICOUT,9021)I,IHNAME(I),IHNAM2(I),IUSE(I),IVALUE(I),   &
                          VALUE(I)
 9021     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVALUE(I),VALUE(I) = ',   &
                 I8,2X,A4,A4,2X,A4,I8,G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,9022)I,IHNAME(I),IHNAM2(I)
 9022     FORMAT('I,IHNAME(I),IHNAM2(I)= ',I8,2X,A4,A4,6X)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPADDP
      SUBROUTINE DPADDV(IH,IH2,Y,N,ISUB,   &
                        IHNAME,IHNAM2,IUSE,IVALUE,VALUE,IN,   &
                        IVSTAR,IVSTOP,MAXNAM,NUMNAM,V,MAXN,   &
                        MAXCOL,NUMCOL,   &
                        PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                        IBUGS2,ISUBRO,IERROR)
!
!     PURPOSE--ADD A VARIABLE WITH NAME GIVEN IN  IH, IH2  AND WITH
!              VALUES IN  Y  INTO DATAPLOT'S INTERNAL NAME TABLE.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2016/8
!     ORIGINAL VERSION--AUGUST    2016.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 IHNAME
      CHARACTER*4 IHNAM2
      CHARACTER*4 IUSE
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION ISUB(*)
      DIMENSION IHNAME(*)
      DIMENSION IHNAM2(*)
      DIMENSION IUSE(*)
      DIMENSION IVALUE(*)
      DIMENSION VALUE(*)
      DIMENSION IN(*)
      DIMENSION IVSTAR(*)
      DIMENSION IVSTOP(*)
      DIMENSION V(*)
      DIMENSION PRED(*)
      DIMENSION RES(*)
      DIMENSION YPLOT(*)
      DIMENSION XPLOT(*)
      DIMENSION X2PLOT(*)
      DIMENSION TAGPLO(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPAD'
      ISUBN2='DV  '
      IERROR='NO'
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'ADDV')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPADDV--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGS2,ISUBRO,MAXNAM,NUMNAM
   52   FORMAT('IBUGS2,ISUBRO,MAXNAM,NUMNAM = ',2(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)MAXN,MAXCOL,NUMCOL
   54   FORMAT('MAXN,MAXCOL,NUMCOL = ',3I8)
        CALL DPWRST('XXX','BUG ')
        DO 60 I=1,NUMNAM
          WRITE(ICOUT,61)I,IHNAME(I),IHNAM2(I),IUSE(I),   &
                         IVALUE(I),VALUE(I)
   61     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVALUE(I),VALUE(I) = ',   &
                 I8,2X,A4,A4,2X,A4,I8,G15.7)
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
   71     FORMAT('J,MAXN,IJ,V(IJ) = ',2I8,I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   70   CONTINUE
      ENDIF
!
!               **************************************************
!               **  DETERMINE IF THE NAME IS ALREADY IN         **
!               **  IN THE INTERNAL ARRAY.                      **
!               **  ADD OR UPDATE ACCORDINGLY.                  **
!               **************************************************
!
      DO 1110 I=1,NUMNAM
        I2=I
        IF(IH.EQ.IHNAME(I2).AND.IH2.EQ.IHNAM2(I2))THEN
          IF(IUSE(I2).EQ.'V')THEN
            ICOLL=IVALUE(I2)
            NSX=0
            DO 1120 J=1,N
!
              IF(ISUB(J).EQ.0)GO TO 1120
              NSX=NSX+1
!
              IJ=MAXN*(ICOLL-1)+J
              IF(ICOLL.LE.MAXCOL)V(IJ)=Y(NSX)
              IF(ICOLL.EQ.MAXCP1)PRED(NSX)=Y(NSX)
              IF(ICOLL.EQ.MAXCP2)RES(NSX)=Y(NSX)
              IF(ICOLL.EQ.MAXCP3)YPLOT(NSX)=Y(NSX)
              IF(ICOLL.EQ.MAXCP4)XPLOT(NSX)=Y(NSX)
              IF(ICOLL.EQ.MAXCP5)X2PLOT(NSX)=Y(NSX)
              IF(ICOLL.EQ.MAXCP6)TAGPLO(NSX)=Y(NSX)
!
 1120       CONTINUE
            IN(I)=N
            GO TO 1190
          ELSE
            WRITE(ICOUT,1151)ISUBN0
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1123)IH,IH2
 1123       FORMAT('      ',A4,A4,' WAS FOUND IN THE NAME TABLE, ',   &
                   'BUT NOT AS A VARIABLE.')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
        ENDIF
 1110 CONTINUE
!
      IF(NUMNAM.GE.MAXNAM)THEN
        WRITE(ICOUT,1151)ISUBN0
 1151   FORMAT('***** ERROR IN DPADDV AS CALLED FROM--',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1155)MAXNAM
 1155   FORMAT('      THE MAXIMUM NUMBER OF NAMES, ',I8,   &
               ', HAS BEEN EXCEEDED.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1162)
 1162   FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(NUMCOL.GE.MAXCOL)THEN
        WRITE(ICOUT,1151)ISUBN0
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1175)MAXCOL
 1175   FORMAT('      THE MAXIMUM NUMBER OF COLUMNS, ',I8,   &
               ', HAS BEEN EXCEEDED.')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      NUMNAM=NUMNAM+1
      ILOC=NUMNAM
      IHNAME(ILOC)=IH
      IHNAM2(ILOC)=IH2
      IUSE(ILOC)='V'
      VALUE(ILOC)=0.0
      NUMCOL=NUMCOL+1
      IVALUE(ILOC)=NUMCOL
!
      NSX=0
      ICOLL=NUMCOL
      DO 1180 J=1,N
!
        IF(ISUB(J).EQ.0)GO TO 1180
        NSX=NSX+1
!
        IJ=MAXN*(ICOLL-1)+J
        IF(ICOLL.LE.MAXCOL)V(IJ)=Y(NSX)
        IF(ICOLL.EQ.MAXCP1)PRED(NSX)=Y(NSX)
        IF(ICOLL.EQ.MAXCP2)RES(NSX)=Y(NSX)
        IF(ICOLL.EQ.MAXCP3)YPLOT(NSX)=Y(NSX)
        IF(ICOLL.EQ.MAXCP4)XPLOT(NSX)=Y(NSX)
        IF(ICOLL.EQ.MAXCP5)X2PLOT(NSX)=Y(NSX)
        IF(ICOLL.EQ.MAXCP6)TAGPLO(NSX)=Y(NSX)
!
 1180 CONTINUE
      IN(ILOC)=N
!
 1190 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'ADDV')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPADDV--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IERROR,MAXNAM,NUMNAM
 9013   FORMAT('IERROR,MAXNAM,NUMNAM = ',A4,2X,2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)MAXN,MAXCOL,NUMCOL
 9014   FORMAT('MAXN,MAXCOL,NUMCOL = ',3I8)
        CALL DPWRST('XXX','BUG ')
        DO 9020 I=1,NUMNAM
          WRITE(ICOUT,9021)I,IHNAME(I),IHNAM2(I),IUSE(I),   &
                           IVALUE(I),VALUE(I)
 9021     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVALUE(I),VALUE(I) = ',   &
                 I8,2X,A4,A4,2X,A4,I8,E15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,9022)I,IHNAME(I),IHNAM2(I),IN(I),   &
                           IVSTAR(I),IVSTOP(I)
 9022     FORMAT('I,IHNAME(I),IHNAM2(I),IN(I),IVSTAR(I),IVSTOP(I)  = ',   &
                 I8,2X,2A4,6X,3I8)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        DO 9030 J=1,NUMCOL
          IJ=MAXN*(J-1)+1
          WRITE(ICOUT,9031)J,MAXN,IJ,V(IJ)
 9031     FORMAT('J,MAXN,IJ,V(IJ) = ',3I8,G15.7)
          CALL DPWRST('XXX','BUG ')
 9030   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPADDV
      SUBROUTINE DPADKS(YTEMP,XTEMP,MAXNXT,   &
                        ICAPSW,IFORSW,IMULT,   &
                        IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--CARRY OUT K-SAMPLE ANDERSON-DARLING TEST
!              (ARE BATCHES SIMILAR?)
!     EXAMPLE--ANDERSON-DARLING K-SAMPLE TEST Y X
!     REFERENCE--CODE ADAPTED FROM MARK VANGEL'S RECIPE CODE
!     WRITTEN BY--ALAN HECKERT
!                 (IMPLEMENTS CODE PROVIDED BY MARK VANGEL)
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--98/4
!     ORIGINAL VERSION--APRIL     1998.
!     UPDATED         --FEBRUARY  2010. USE DPPARS
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICAPSW
      CHARACTER*4 IFORSW
      CHARACTER*4 IMULT
      CHARACTER*4 IBUGA2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 ISUBN0
      CHARACTER*4 IHOST1
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
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
      DIMENSION YTEMP(*)
      DIMENSION XTEMP(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
!
      DOUBLE PRECISION XPS(MAXOBV)
      DOUBLE PRECISION XPSU(MAXOBV)
      DOUBLE PRECISION WK3(MAXOBV)
!
      DIMENSION IPBCH(MAXOBV)
      DIMENSION IWK2(MAXOBV)
      DIMENSION ISIZE(MAXOBV)
      DIMENSION NTIE(MAXOBV)
!
      INCLUDE 'DPCOZD.INC'
      INCLUDE 'DPCOZI.INC'
!
      EQUIVALENCE(DGARBG(IDGAR1),XPS(1))
      EQUIVALENCE(DGARBG(IDGAR2),XPSU(1))
      EQUIVALENCE(DGARBG(IDGAR3),WK3(1))
!
      EQUIVALENCE(IGARBG(IIGAR1),IPBCH(1))
      EQUIVALENCE(IGARBG(IIGAR2),IWK2(1))
      EQUIVALENCE(IGARBG(IIGAR3),ISIZE(1))
      EQUIVALENCE(IGARBG(IIGAR4),NTIE(1))
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPAD'
      ISUBN2='KS  '
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
!               *****************************************************
!               **  TREAT THE ANDERSON-DARLING K-SAMPLE TEST CASE  **
!               *****************************************************
!
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'ADKS')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPADKS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA2,IBUGA3,IBUGQ,ISUBRO
   52   FORMAT('IBUGA2,IBUGA3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)MAXNXT
   55   FORMAT('MAXNXT = ',I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ****************************************
!               **  STEP 2--                          **
!               **  EXTRACT THE VARIABLE LIST         **
!               ****************************************
!
      ISTEPN='2'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'ADKS')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      INAME='ANDERSON-DARLING K-SAMPLE TEST'
      MINNA=1
      MAXNA=100
      MINN2=4
      IFLAGE=1
      IFLAGM=0
      MINNVA=2
      MAXNVA=2
      IF(IMULT.EQ.'ON')THEN
        IFLAGE=0
        IFLAGM=0
        MINNVA=2
        MAXNVA=30
      ENDIF
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'ADKS')THEN
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
!               *****************************************
!               **  STEP 3A--                          **
!               **  CASE 1: TWO RESPONSE VARIABLES     **
!               **          WITH NO REPLICATION        **
!               *****************************************
!
      IF(IMULT.EQ.'OFF')THEN
        ISTEPN='3A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'ADKS')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        ICOL=1
        NUMVA2=2
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                    INAME,IVARN1,IVARN2,IVARTY,   &
                    ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                    MAXCP4,MAXCP5,MAXCP6,   &
                    V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                    Y,X,XTEMP,NS1,NLOCA2,NLOCA3,ICASE,   &
                    IBUGA3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
!               *****************************************
!               **  STEP 52--                          **
!               **  DO ANDERSON-DARLING K-SAMPLE TEST  **
!               *****************************************
!
      ISTEPN='52'
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'ADKS')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5211)
 5211   FORMAT('***** FROM DPADKS, AS WE ARE ABOUT TO CALL DPADK2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5212)NS1,MAXN
 5212   FORMAT('NS1,MAXN = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO 5215 I=1,NS1
          WRITE(ICOUT,5216)I,Y(I),X(I)
 5216     FORMAT('I,Y(I),X(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 5215   CONTINUE
      ENDIF
!
      CALL DPADK2(Y,X,NS1,   &
                  XTEMP,YTEMP,XPS,XPSU,WK3,IPBCH,ISIZE,IWK2,NTIE,   &
                  ICAPSW,ICAPTY,IFORSW,IMULT,IVARN1,IVARN2,   &
                  STATVA,   &
                  CUT50,CUT75,CUT90,CUT95,CUT975,CUT99,CUT999,   &
                  IBUGA3,ISUBRO,IERROR)
!
!               *******************************************************
!               **  STEP 4A--                                        **
!               **  CASE 2: MULTIPLE RESPONSE VARIABLES.  NOTE THAT  **
!               **          THE MULTIPLE LABS ARE CONVERTED INTO     **
!               **          A "Y X" STACKED PAIR WHERE "X" IS        **
!               **          THE LAB-ID VARIABLE.                     **
!               *******************************************************
!
      ELSEIF(IMULT.EQ.'ON')THEN
        ISTEPN='4A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'ADKS')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        ICOL=1
        NUMVA2=NUMVAR
        CALL DPPAR8(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                    INAME,IVARN1,IVARN2,IVARTY,   &
                    ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                    MAXCP4,MAXCP5,MAXCP6,   &
                    V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                    XTEMP,Y,X,NS,ICASE,   &
                    IBUGA3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
        NUMVAR=2
!
        IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'ADKS')THEN
          ISTEPN='4B'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,442)
  442     FORMAT('***** FROM THE MIDDLE  OF DPADKS--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,443)ICASAN,NUMVAR,NS
  443     FORMAT('ICASAN,NUMVAR,NS = ',A4,2I8)
          CALL DPWRST('XXX','BUG ')
          IF(NS.GE.1)THEN
            DO 445 I=1,NS
              WRITE(ICOUT,446)I,Y(I),X(I)
  446         FORMAT('I,Y(I),X(I) = ',I8,2G15.7)
              CALL DPWRST('XXX','BUG ')
  445       CONTINUE
          ENDIF
        ENDIF
!
      CALL DPADK2(Y,X,NS,   &
                  XTEMP,YTEMP,XPS,XPSU,WK3,IPBCH,ISIZE,IWK2,NTIE,   &
                  ICAPSW,ICAPTY,IFORSW,IMULT,IVARN1,IVARN2,   &
                  STATVA,   &
                  CUT50,CUT75,CUT90,CUT95,CUT975,CUT99,CUT999,   &
                  IBUGA3,ISUBRO,IERROR)
!
      ENDIF
!
!               ***************************************
!               **  STEP 61--                        **
!               **  UPDATE INTERNAL DATAPLOT TABLES  **
!               ***************************************
!
      ISTEPN='61'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'ADKS')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISUBN0='ADKS'
!
      IH='STAT'
      IH2='VAL '
      VALUE0=STATVA
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA3,IERROR)
!
      IH='CUTO'
      IH2='FF50'
      VALUE0=CUT50
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA3,IERROR)
!
      IH='CUTO'
      IH2='FF75'
      VALUE0=CUT75
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA3,IERROR)
!
      IH='CUTO'
      IH2='FF90'
      VALUE0=CUT90
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA3,IERROR)
!
      IH='CUTO'
      IH2='FF95'
      VALUE0=CUT95
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA3,IERROR)
!
      IH='CUTO'
      IH2='FF99'
      VALUE0=CUT99
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA3,IERROR)
!
      IH='CUTO'
      IH2='F999'
      VALUE0=CUT999
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA3,IERROR)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'ADKS')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPADKS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA2,IBUGA3,IBUGQ
 9012   FORMAT('IBUGA2,IBUGA3,IBUGQ = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NS1,IERROR
 9014   FORMAT('NS1,IERROR = ',I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPADKS
      SUBROUTINE DPADK2(Y,TAG,N,   &
                        XTEMP,YTEMP,XPS,XPSU,WK3,IPBCH,ISIZE,IWK2,NTIE,   &
                        ICAPSW,ICAPTY,IFORSW,IMULT,IVARID,IVARI2,   &
                        ADKSTA,   &
                        CUT50,CUT75,CUT90,CUT95,CUT975,CUT99,CUT999,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS ROUTINE COMPUTES THE ANDERSON-DARLING K-SAMPLE TEST
!              (ARE BATCHES SIMILAR?)
!     EXAMPLE--ANDERSON-DARLING K-SAMPLE TEST Y TAG
!     REFERENCE--ADAPTED FROM MARK VANGEL'S RECIPE CODE
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATIION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--98/4
!     ORIGINAL VERSION--APRIL     1998.
!     UPDATED         --MARCH     2011. REFORMAT OUTPUT AND PRINT
!                                       TABLES USING DPDTA1, DPDTA5
!     UPDATED         --MARCH     2011. COMPUTE CRITICAL VALUES FOR
!                                       SIGNIFICANCE LEVELS OTHER THAN
!                                       95%.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 IMULT
      CHARACTER*4 IVARID(*)
      CHARACTER*4 IVARI2(*)
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION TAG(*)
      DIMENSION YTEMP(*)
      DIMENSION XTEMP(*)
!
      DIMENSION IPBCH(*)
      DIMENSION ISIZE(*)
      DIMENSION IWK2(*)
      DIMENSION NTIE(*)
!
      DOUBLE PRECISION XPS(*)
      DOUBLE PRECISION XPSU(*)
      DOUBLE PRECISION WK3(*)
!
      PARAMETER (NUMALP=7)
      REAL ALPHA(NUMALP)
      REAL ADC(NUMALP)
!
      PARAMETER(NUMCLI=5)
      PARAMETER(MAXLIN=3)
      PARAMETER (MAXROW=NUMALP)
      PARAMETER (MAXRO2=20)
      CHARACTER*60 ITITLE
      CHARACTER*60 ITITLZ
      CHARACTER*1  ITITL9
      CHARACTER*60 ITEXT(MAXRO2)
      CHARACTER*4  ALIGN(NUMCLI)
      CHARACTER*4  VALIGN(NUMCLI)
      REAL         AVALUE(MAXRO2)
      INTEGER      NCTEXT(MAXRO2)
      INTEGER      IDIGIT(MAXRO2)
      INTEGER      NTOT(MAXRO2)
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
      LOGICAL IFLAGS
      LOGICAL IFLAGE
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
      DATA ALPHA/   &
       50.0, 75.0, 90.0, 95.0, 97.5, 99.0, 99.9/
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPAD'
      ISUBN2='K2  '
!
      IERROR='NO'
      CUT50=CPUMIN
      CUT75=CPUMIN
      CUT90=CPUMIN
      CUT95=CPUMIN
      CUT975=CPUMIN
      CUT99=CPUMIN
      CUT999=CPUMIN
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ADK2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPADK2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,IFORSW,IMULT,N
   52   FORMAT('IBUGA3,ISUBRO,IFORSW,IMULT,N = ',4(A4,2X),I8)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,53)IVARID(1),IVARI2(1),IVARID(2),IVARI2(2)
   53   FORMAT('IVARID(1),IVARI2(1),IVARID(2),IVARI2(2) = ',   &
               A4,A4,2X,A4,A4)
        CALL DPWRST('XXX','WRIT')
        DO 56 I=1,N
          WRITE(ICOUT,57)I,Y(I),TAG(I)
   57     FORMAT('I,Y(I),TAG(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
!
      CALL DPADK3(Y,TAG,N,ALPHA,NUMALP,   &
                  XTEMP,YTEMP,XPS,XPSU,WK3,IPBCH,ISIZE,IWK2,NTIE,   &
                  ADKSTA,ADC,DSD,IFLAG,NBCH,MINSIZ,MAXSIZ,   &
                  IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      CUT50=ADC(1)
      CUT75=ADC(2)
      CUT90=ADC(3)
      CUT95=ADC(4)
      CUT975=ADC(5)
      CUT99=ADC(6)
      CUT999=ADC(7)
!
!               ******************************************
!               **   STEP 43--                          **
!               **   WRITE OUT EVERYTHING               **
!               **   FOR ANDERSON-DARLING K-SAMPLE TEST **
!               ******************************************
!
      ISTEPN='42'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ADK2')   &
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
      ITITLE='Anderson-Darling K-Sample Test for Common Groups'
      NCTITL=48
      ITITLZ=' '
      NCTITZ=0
!
      ICNT=1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      IF(IMULT.EQ.'OFF')THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='Response Variable: '
        WRITE(ITEXT(ICNT)(20:23),'(A4)')IVARID(1)(1:4)
        WRITE(ITEXT(ICNT)(24:27),'(A4)')IVARI2(1)(1:4)
        NCTEXT(ICNT)=27
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
!
        ICNT=ICNT+1
        ITEXT(ICNT)='Group-ID Variable: '
        WRITE(ITEXT(ICNT)(20:23),'(A4)')IVARID(2)(1:4)
        WRITE(ITEXT(ICNT)(24:27),'(A4)')IVARI2(2)(1:4)
        NCTEXT(ICNT)=27
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
      ENDIF
!
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='H0: The Groups Are Homogeneous'
      NCTEXT(ICNT)=30
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: The Groups Are Not Homogeneous'
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
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
      ITEXT(ICNT)='Total Number of Observations:'
      NCTEXT(ICNT)=29
      AVALUE(ICNT)=REAL(N)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Groups:'
      NCTEXT(ICNT)=17
      AVALUE(ICNT)=REAL(NBCH)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Minimum Batch Size:'
      NCTEXT(ICNT)=19
      AVALUE(ICNT)=REAL(MINSIZ)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Maximum Batch Size:'
      NCTEXT(ICNT)=19
      AVALUE(ICNT)=REAL(MAXSIZ)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Test Statistic Value:'
      NCTEXT(ICNT)=21
      AVALUE(ICNT)=ADKSTA
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Test Statistic Standard Error:'
      NCTEXT(ICNT)=30
      AVALUE(ICNT)=DSD
      IDIGIT(ICNT)=NUMDIG
!
      IF(IFLAG.EQ.1)THEN
        ICNT=ICNT+1
        ITEXT(ICNT)=' '
        NCTEXT(ICNT)=1
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        ICNT=ICNT+1
        ITEXT(ICNT)='Note: In computing the critical value, the'
        NCTEXT(ICNT)=42
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        ICNT=ICNT+1
        ITEXT(ICNT)='      variance is negative, so no critical'
        NCTEXT(ICNT)=42
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        ICNT=ICNT+1
        ITEXT(ICNT)='      value was computed.  This may occur'
        NCTEXT(ICNT)=42
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        ICNT=ICNT+1
        ITEXT(ICNT)='      if some of the batch sample sizes'
        NCTEXT(ICNT)=39
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        ICNT=ICNT+1
        ITEXT(ICNT)='      are substantially different.'
        NCTEXT(ICNT)=34
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
      ENDIF
!
      NUMROW=ICNT
      DO 5010 I=1,NUMROW
        NTOT(I)=15
 5010 CONTINUE
!
      IFRST=.TRUE.
      ILAST=.TRUE.
!
      ISTEPN='42A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ADK2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                  AVALUE,IDIGIT,   &
                  NTOT,NUMROW,   &
                  ICAPSW,ICAPTY,ILAST,IFRST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      ISTEPN='42D'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ADK2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IFLAG.EQ.1)GO TO 9000
!
      ITITL9=' '
      NCTIT9=0
      ITITLE='Conclusions (Upper 1-Tailed Test)'
      NCTITL=33
!
      DO 5030 J=1,5
        DO 5040 I=1,3
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
 5040   CONTINUE
 5030 CONTINUE
!
      ITITL2(2,1)='Null'
      NCTIT2(2,1)=4
      ITITL2(3,1)='Hypothesis'
      NCTIT2(3,1)=10
!
      ITITL2(2,2)='Significance'
      NCTIT2(2,2)=12
      ITITL2(3,2)='Level'
      NCTIT2(3,2)=5
!
      ITITL2(2,3)='Test '
      NCTIT2(2,3)=4
      ITITL2(3,3)='Statistic'
      NCTIT2(3,3)=9
!
      ITITL2(2,4)='Critical'
      NCTIT2(2,4)=8
      ITITL2(3,4)='Region (>=)'
      NCTIT2(3,4)=11
!
      ITITL2(1,5)='Null'
      NCTIT2(1,5)=4
      ITITL2(2,5)='Hypothesis'
      NCTIT2(2,5)=10
      ITITL2(3,5)='Conclusion'
      NCTIT2(3,5)=10
!
      NMAX=0
      NUMCOL=5
      DO 5050 I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IF(I.EQ.1)NTOT(I)=12
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
        IDIGIT(I)=NUMDIG
        IF(I.EQ.1 .OR. I.EQ.2 .OR. I.EQ.5)THEN
          ITYPCO(I)='ALPH'
        ENDIF
        IWHTML(1)=150
        IWHTML(2)=125
        IWHTML(3)=150
        IWHTML(4)=150
        IWHTML(5)=150
        IINC=1600
        IINC2=1400
        IINC3=2200
        IWRTF(1)=IINC
        IWRTF(2)=IWRTF(1)+IINC
        IWRTF(3)=IWRTF(2)+IINC2
        IWRTF(4)=IWRTF(3)+IINC
        IWRTF(5)=IWRTF(4)+IINC
!
        DO 5060 J=1,NUMALP
!
          IVALUE(J,1)='Homogeneous'
          NCVALU(J,1)=11
          AMAT(J,3)=ADKSTA
          AMAT(J,4)=ADC(J)
          IVALUE(J,5)(1:6)='REJECT'
          IF(ADKSTA.LT.ADC(J))THEN
            IVALUE(J,5)(1:6)='ACCEPT'
          ENDIF
          NCVALU(J,5)=6
!
          ALPHAT=ALPHA(J)
          ALPHAT=ALPHAT
          WRITE(IVALUE(J,2)(1:4),'(F4.1)')ALPHAT
          IVALUE(J,2)(5:5)='%'
          NCVALU(J,2)=5
 5060   CONTINUE
!
 5050 CONTINUE
!
      ICNT=NUMALP
      NUMLIN=3
      NUMCOL=5
      IFRST=.TRUE.
      ILAST=.TRUE.
      IFLAGS=.TRUE.
      IFLAGE=.TRUE.
      CALL DPDTA5(ITITLE,NCTITL,   &
                  ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                  MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                  IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ADK2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPADK2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9012)N,IBUGA3,IERROR
 9012   FORMAT('N,IBUGA3,IERROR = ',I8,2X,A4,2X,A4)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9013)NBCH,ADKSTA,ICONC1
 9013   FORMAT('NBCH,ADKSTA,ICONC1 = ',I8,2X,G15.7,2X,A6)
        CALL DPWRST('XXX','WRIT')
        DO 9015 I=1,NBCH
          WRITE(ICOUT,9016)I,ISIZE(I)
 9016     FORMAT('I,ISIZE(I) = ',2I8)
          CALL DPWRST('XXX','WRIT')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPADK2
      SUBROUTINE DPADK3(Y,TAG,N,ALPHA,NUMALP,   &
                        XTEMP,YTEMP,XPS,XPSU,WK3,IPBCH,ISIZE,IWK2,NTIE,   &
                        ADKSTA,ADC,DSD,IFLAG,NBCH,MINSIZ,MAXSIZ,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS ROUTINE COMPUTES THE ANDERSON-DARLING K-SAMPLE TEST
!              (ARE BATCHES SIMILAR?)
!
!              EXTRACTED FROM DPADK2 IN ORDER TO ADD TO LIST OF
!              SUPPORTED STATISTICS:
!
!                 LET A = ANDERSON DARLING K-SAMPLE STATISTIC Y X
!
!     EXAMPLE--ANDERSON-DARLING K-SAMPLE TEST Y TAG
!     REFERENCE--ADAPTED FROM MARK VANGEL'S RECIPE CODE
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATIION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2011/3
!     ORIGINAL VERSION--MARCH     2011. EXTRACTED FROM DPADK2
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION TAG(*)
      DIMENSION ALPHA(*)
      DIMENSION ADC(*)
      DIMENSION YTEMP(*)
      DIMENSION XTEMP(*)
!
      DIMENSION IPBCH(*)
      DIMENSION ISIZE(*)
      DIMENSION IWK2(*)
      DIMENSION NTIE(*)
!
      DOUBLE PRECISION XPS(*)
      DOUBLE PRECISION XPSU(*)
      DOUBLE PRECISION WK3(*)
!
      DOUBLE PRECISION DADKST
      DOUBLE PRECISION DADC
      DOUBLE PRECISION DA, DB, DC, DD
      DOUBLE PRECISION DG, DS, DT
      DOUBLE PRECISION DK, DN
      DOUBLE PRECISION DVAR, DSD
      DOUBLE PRECISION DPPF
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPAD'
      ISUBN2='K3  '
!
      IERROR='NO'
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ADK3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPADK3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N
   52   FORMAT('IBUGA3,ISUBRO,N = ',2(A4,2X),I8)
        CALL DPWRST('XXX','WRIT')
        DO 56 I=1,N
          WRITE(ICOUT,57)I,Y(I),TAG(I)
   57     FORMAT('I,Y(I),TAG(I) = ',I8,2G15.7)
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ADK3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(N.LT.4)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1111)
 1111   FORMAT('***** ERROR IN ANDERSON-DARLING K-SAMPLE TEST--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1112)
 1112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE ',   &
               'VARIABLE IS LESS THAN FOUR.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1113)N
 1113   FORMAT('SAMPLE SIZE = ',I8)
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
      HOLD=TAG(1)
      DO 1235 I=2,N
      IF(TAG(I).NE.HOLD)GO TO 1239
 1235 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1111)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1231)
 1231 FORMAT('      THERE IS ONLY ONE BATCH IN THE DATA.')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1233)
 1233 FORMAT('      THE ANDERSON-DARLING K-SAMPLE TEST WILL NOT BE ',   &
             'PERFORMED.')
      CALL DPWRST('XXX','WRIT')
      IERROR='YES'
      GO TO 9000
 1239 CONTINUE
!
!               *****************************************
!               **  STEP 41--                          **
!               **  CARRY OUT CALCULATIONS             **
!               **  FOR ANDERSON-DARLING K-SAMPLE TEST **
!               *****************************************
!
      ISTEPN='41'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ADK3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     SORT AND POOL Y AND TAG VARIABLE
!
      CALL SORTC(Y,TAG,N,Y,TAG)
!
!     COMPUTE DISTINCT VALUES OF Y AND TAG VARIABLE
!
      IWRITE='OFF'
      CALL DISTIN(TAG,N,IWRITE,XTEMP,NBCH,IBUGA3,IERROR)
      CALL DISTIN(Y,N,IWRITE,YTEMP,NDIST,IBUGA3,IERROR)
!
      DO 4110 I=1,N
        XPS(I)=DBLE(Y(I))
        IPBCH(I)=INT(TAG(I))
 4110 CONTINUE
      DO 4120 I=1,NDIST
        XPSU(I)=DBLE(YTEMP(I))
 4120 CONTINUE
!
      DO 4130 I=1,NBCH
        HOLD=XTEMP(I)
        ISIZE(I)=0
        DO 4140 J=1,N
          IF(TAG(J).EQ.HOLD)ISIZE(I)=ISIZE(I)+1
 4140   CONTINUE
 4130 CONTINUE
!
      MINSIZ=9999999
      MAXSIZ=0
      DO 4145 I=1,NBCH
        IF(ISIZE(I).GT.MAXSIZ)MAXSIZ=ISIZE(I)
        IF(ISIZE(I).LT.MINSIZ)MINSIZ=ISIZE(I)
 4145 CONTINUE
!
      IERR=0
      DO 4150 I=1,NBCH
        IF(ISIZE(I).LE.1)THEN
          IERR=1
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,1111)
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,4151)INT(XTEMP(I)+0.1)
 4151     FORMAT('      BATCH ',I10,' ONLY HAS A SINGLE VALUE.')
          CALL DPWRST('XXX','WRIT')
        ENDIF
 4150 CONTINUE
      IF(IERR.EQ.1)THEN
        WRITE(ICOUT,1233)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,4153)
 4153   FORMAT('     TRY RUNNING THIS TEST WITH THESE BATCHES ',   &
               'OMITTED.')
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      CALL ANDYK(N,NBCH,XPS,XPSU,IPBCH,NTIE,ISIZE,WK3,IWK2,DADKST)
      ADKSTA=REAL(DADKST)
!
!               ******************************************
!               **   STEP 42---                         **
!               **   CALCULATE 5% CRITICAL VALUE        **
!               **   FOR ANDERSON-DARLING K-SAMPLE TEST **
!               ******************************************
!
      ISTEPN='42'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ADK3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DG=0.0D0
      DO 4610 I=1,N-2
        DO 4620 J=I+1,N-1
          DG=DG + 1.D0/DBLE((N-I)*J)
 4620   CONTINUE
 4610 CONTINUE
      DT=0.0D0
      DO 4630 I=1,N-1
        DT=DT + 1.0D0/DBLE(I)
 4630 CONTINUE
      DS=0.0D0
      DO 4640 I=1,NBCH
        DS=DS + 1.0D0/DBLE(ISIZE(I))
 4640 CONTINUE
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ADK3')THEN
        WRITE(ICOUT,4641)DG,DT,DS
 4641   FORMAT('DG,DT,DS = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      DK=DBLE(NBCH)
      DA=(4.0D0*DG-6.0D0)*(DK-1.0D0) + (10.D0-6.0D0*DG)*DS
      DB=(2.0D0*DG - 4.0D0)*DK*DK + 8.0D0*DT*DK +   &
         (2.0D0*DG - 14.0D0*DT -4.0D0)*DS -8.0D0*DT + 4.0D0*DG -6.0D0
      DC=(6.0D0*DT + 2.0D0*DG -2.0D0)*DK*DK +   &
         (4.0D0*DT - 4.0D0*DG + 6.0D0)*DK + (2.0*DT - 6.0D0)*DS +   &
         4.0D0*DT
      DD=(2.0D0*DT + 6.0D0)*DK*DK - 4.0*DT*DK
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ADK3')THEN
        WRITE(ICOUT,4643)DK,DA,DB,DD
 4643   FORMAT('DK,DA,DB,DD = ',4G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      DN=DBLE(N)
      DVAR=(DA*DN**3 +DB*DN**2 + DC*DN + DD)/   &
           DBLE((N-1)*(N-2)*(N-3)*(NBCH-1)**2)
!
!     CHECK FOR NEGATIVE VARIANCE
!
      IF(DVAR.LT.0.0D0)THEN
        IFLAG=1
        DSD=0.0D0
        DADC=0.0D0
      ELSE
        IFLAG=0
        DSD=DSQRT(DVAR)
        DO 4644 I=1,NUMALP
          ALPHAT=ALPHA(I)/100.0
          CALL NODPPF(DBLE(ALPHAT),DPPF)
          DADC=1.0D0 + DSD*   &
          (DPPF + 0.678D0/DSQRT(DK-1.0D0) - 0.362D0/(DK-1.0D0))
          ADC(I)=REAL(DADC)
!
          IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ADK3')THEN
            WRITE(ICOUT,4645)I,ALPHAT,DVAR,DSD,DADC
 4645       FORMAT('I,ALPHAT,DVAR,DSD,DADC = ',I8,4G15.7)
            CALL DPWRST('XXX','WRIT')
          ENDIF
!
 4644   CONTINUE
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ADK3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPADK3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9012)N,IBUGA3,IERROR
 9012   FORMAT('N,IBUGA3,IERROR = ',I8,2X,A4,2X,A4)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9013)NBCH,ADKSTA
 9013   FORMAT('NBCH,ADKSTA = ',I8,2X,G15.7)
        CALL DPWRST('XXX','WRIT')
        DO 9015 I=1,NBCH
          WRITE(ICOUT,9016)I,ISIZE(I)
 9016     FORMAT('I,ISIZE(I) = ',2I8)
          CALL DPWRST('XXX','WRIT')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPADK3
      SUBROUTINE DPAGCO(P,N,ALPHA,IWRITE,ALOWLM,AUPPLM,IBUGA3,IERROR)
!
!     PURPOSE--FOR A GIVEN P, N, AND ALPHA, COMPUTE THE
!              AGRESTI-COULL LOWER AND UPPER BINOMIAL CONFIDENCE
!              LIMITS.  THIS IS USEFUL FOR GENERATING BINOMIAL
!              CONFIDENCE LIMITS WHEN ONLY SUMMARY INFORMATION
!              IS AVAILABLE.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     REFERENCES--AGRESTI AND COULL (1998), "APPROXIMATE IS BETTER THAN
!                 "EXACT" FOR INTERVAL ESTIMATION OF BINOMIAL
!                 PROPORTIONS", AMERICAN STATISTICIAN, 52, 119-126.
!               --BROWN, CAI, AND DASGUPTA (2001), "INTERVAL ESTIMATION
!                 FOR A BINOMIAL PROPORTION", STATISTICAL SCIENCE,
!                 VOL. 16, NO. 2, PP. 101-133.
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2007/2
!     ORIGINAL VERSION--FEBRUARY  2007.
!     UPDATED         --MARCH     2014. SUPPORT FOR ALTERNATIVE INTERVALS.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      REAL P
      REAL ALPHA
      REAL ALOWLM
      REAL AUPPLM
      INTEGER N
!
      INCLUDE 'DPCOST.INC'
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPAG'
      ISUBN2='CO  '
!
      IERROR='NO'
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPAGCO--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,IWRITE
   52   FORMAT('IBUGA3,IWRITE = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)N,P,ALPHA
   53   FORMAT('N,P,ALPHA = ',I8,2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ********************************
!               **  STEP 1--                  **
!               **  CHECK FOR INPUT ERRORS    **
!               ********************************
!
      IF(N.LT.1)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,151)
  151   FORMAT('***** ERROR IN DPAGCO--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,152)
  152   FORMAT('      THE INPUT SAMPLE SIZE FOR THE AGRESTI-COULL')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,154)
  154   FORMAT('      LIMITS IS LESS THAN 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,157)N
  157   FORMAT('      THE INPUT SAMPLE SIZE            = ',I8)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
      IF(P.LT.0.0 .OR. P.GT.1.0)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,161)
  161   FORMAT('***** ERROR IN DPAGCO--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,162)
  162   FORMAT('      THE BINOMIAL PROBABILITY OF SUCCESS PARAMETER')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,164)
  164   FORMAT('      IS OUTSIDE THE (0,1) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,167)P
  167   FORMAT('      THE PROBABILITY OF SUCCESS PARAMETER = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
      ALPHSV=ALPHA
      IF(ALPHA.GT.1.0 .AND. ALPHA.LE.100.0)ALPHA=ALPHA/100.0
      IF(ALPHA.LE.0.0 .OR. ALPHA.GE.1.0)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,171)
  171   FORMAT('***** ERROR IN DPAGCO--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,172)
  172   FORMAT('      THE VALUE OF ALPHA IS OUTSIDE THE (0,1) ',   &
               'INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,177)ALPHA
  177   FORMAT('      THE VALUE OF ALPHA = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
!               ******************************************
!               **  STEP 2--                            **
!               **  COMPUTE THE AGRESTI-COULL INTERVALS **
!               ******************************************
!
!     NOTE 9/15/2008: ENSURE THAT ALPHA IS IN THE (0.5.1)
!                     INTERVAL.
!
!     NOTE 11/2017:   FIX BUG WHEN ALPHA GIVEN AS 0.05 RATHER THAN 0.95.
!
      ALP=ALPHA
      IF(ALP.LT.0.5)THEN
        P1=ALP/2.0
        P2=1.0-(ALP/2.0)
      ELSE
        ALP=1.0 - ALPHA
        P1=ALP/2.0
        P2=1.0-(ALP/2.0)
      ENDIF
!
      AN=REAL(N)
      Q=1.0-P
!
!     NOTE 2014/03: NOTE THAT WHAT WE ARE CALLING "AGRESTI-COULL" IS
!                   NOW COMNONLY REFERRED TO AS THE "WILSON" INTERVAL.
!                   WHAT IS TYPICALLY CALLED AGRESTI-COULL IN THE
!                   LITERATURE IS WHAT THE AGRESTI-COULL PAPER REFERRED
!                   TO AS THE "ADJUSTED WALD".  THE BROWN, CAI, AND
!                   DASGUPTA PAPER PERFORMED A DETAILED ANALYSIS OF
!                   VARIOUS BINOMIAL APPROXIMATIONS.  THEY RECOMMEND
!                   THAT FOR N < 40, EITHER THE WILSON OR A BAYESIAN
!                   METHOD BASED ON JEFFREYS PRIORS BE USED.  FOR
!                   N > 40, THESE 3 METHODS HAVE COMPARABLE PERFORMANCE.
!
!                   IN DATAPLOT, YOU CAN SPECIFY THE DESIRED METHOD WITH
!                   THE COMMAND
!
!                       SET BINOMIAL METHOD <WILSON/ADJUSTED WALD/JEFFREYS>
!
      IF(IBINME.EQ.'WILS' .OR. IBINME.EQ.'EXAC')THEN
        CALL NORPPF(P2,ZALPHA)
        TERM1=ZALPHA*ZALPHA/(2.0*AN)
        TERM2=ZALPHA*SQRT((P*Q/AN) + ZALPHA*ZALPHA/(4.0*AN*AN))
        TERM3=1.0 + ZALPHA*ZALPHA/AN
        TERM4=(P + TERM1 + TERM2)/TERM3
        TERM5=(P + TERM1 - TERM2)/TERM3
        ALOWLM=MIN(TERM4,TERM5)
        AUPPLM=MAX(TERM4,TERM5)
!
!     ADJUSTED WALD METHOD:
!
      ELSEIF(IBINME.EQ.'WALD')THEN
        AN=REAL(N)
        CALL NORPPF(P2,ZALPHA)
        AK2=ZALPHA**2
        AX=AN*P
        IX=INT(AX+0.5)
        AX=REAL(IX) + (AK2/2.0)
        ANTEMP=AN + (AK2/2.0)
        PTEMP=AX/ANTEMP
        A1=PTEMP - ZALPHA*SQRT(PTEMP*(1.0-PTEMP))/SQRT(ANTEMP)
        A2=PTEMP + ZALPHA*SQRT(PTEMP*(1.0-PTEMP))/SQRT(ANTEMP)
        ALOWLM=MIN(A1,A2)
        AUPPLM=MAX(A1,A2)
      ELSEIF(IBINME.EQ.'JEFF')THEN
        AX=AN*P
        IX=INT(AX+0.5)
        AX=REAL(IX)
        ALPHA=AX+0.5
        BETA=AN-AX+0.5
        CALL BETPPF(P1,ALPHA,BETA,ALOWLM)
        CALL BETPPF(P2,ALPHA,BETA,AUPPLM)
      ENDIF
      IF(AUPPLM.GT.1.0)AUPPLM=1.0
      IF(ALOWLM.LT.0.0)ALOWLM=0.0
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPAGCO--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR
 9012   FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)ALOWLM,AUPPLM
 9014   FORMAT('ALOWLM,AUPPLM = ',G15.7,2X,G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPAGCO
      SUBROUTINE DPAGC1(P,N,ALPHA,IDIR,IWRITE,ALIMIT,IBUGA3,IERROR)
!
!     PURPOSE--FOR A GIVEN P, N, AND ALPHA, COMPUTE THE ONE-SIDED
!              AGRESTI-COULL BINOMIAL CONFIDENCE LIMITS (IDIR SPECIFIES
!              WHETHER IT IS A LOWER LIMIT OR AN UPPER LIMIT).  THIS IS
!              USEFUL FOR GENERATING BINOMIAL CONFIDENCE LIMITS WHEN
!              ONLY SUMMARY INFORMATION IS AVAILABLE.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     REFERENCES--AGRESTI AND COULL (1998), "APPROXIMATE IS BETTER THAN
!                 "EXACT" FOR INTERVAL ESTIMATION OF BINOMIAL
!                 PROPORTIONS", AMERICAN STATISTICIAN, 52, 119-126.
!               --BROWN, CAI, AND DASGUPTA (2001), "INTERVAL ESTIMATION
!                 FOR A BINOMIAL PROPORTION", STATISTICAL SCIENCE,
!                 VOL. 16, NO. 2, PP. 101-133.
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2010/3
!     ORIGINAL VERSION--MARCH     2010.
!     UPDATED         --MARCH     2014. SUPPORT FOR ALTERNATIVE INTERVALS.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IDIR
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      REAL P
      REAL ALPHA
      REAL ALOWLM
      REAL AUPPLM
      INTEGER N
!
      INCLUDE 'DPCOST.INC'
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPAG'
      ISUBN2='C1  '
      IERROR='NO'
!
      ALOWLM=0.0
      AUPPLM=0.0
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPAGC1--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,IDIR,IWRITE
   52   FORMAT('IBUGA3,IDIR,IWRITE = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)N,P,ALPHA
   53   FORMAT('N,P,ALPHA = ',I8,2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ********************************
!               **  STEP 1--                  **
!               **  CHECK FOR INPUT ERRORS    **
!               ********************************
!
      IF(N.LT.1)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,151)
  151   FORMAT('***** ERROR IN DPAGC1--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,152)
  152   FORMAT('      THE INPUT SAMPLE SIZE FOR THE AGRESTI-COULL')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,154)
  154   FORMAT('      LIMITS IS LESS THAN 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,157)N
  157   FORMAT('      THE INPUT SAMPLE SIZE            = ',I8)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
      IF(P.LT.0.0 .OR. P.GT.1.0)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,161)
  161   FORMAT('***** ERROR IN DPAGC1--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,162)
  162   FORMAT('      THE BINOMIAL PROBABILITY OF SUCCESS PARAMETER')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,164)
  164   FORMAT('      IS OUTSIDE THE (0,1) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,167)P
  167   FORMAT('      THE PROBABILITY OF SUCCESS PARAMETER = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
      ALPHSV=ALPHA
      IF(ALPHA.GT.1.0 .AND. ALPHA.LE.100.0)ALPHA=ALPHA/100.0
      IF(ALPHA.LE.0.0 .OR. ALPHA.GE.1.0)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,171)
  171   FORMAT('***** ERROR IN DPAGC1--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,172)
  172   FORMAT('      THE VALUE OF ALPHA IS OUTSIDE THE (0,1) ',   &
               'INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,177)ALPHA
  177   FORMAT('      THE VALUE OF ALPHA = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
!               ******************************************
!               **  STEP 2--                            **
!               **  COMPUTE THE AGRESTI-COULL INTERVALS **
!               ******************************************
!
!     NOTE 9/15/2008: ENSURE THAT ALPHA IS IN THE (0.5.1)
!                     INTERVAL.
!
      ALP=ALPHA
      IF(ALP.LT.0.5)THEN
        ALP=1.0-ALP
      ENDIF
!
!     FOR THE ONE-SIDED TEST, USE ALPHA RATHER THAN ALPHA/2
!
      ALP=1.0 - ALPHA
      P1=ALP
      P2=1.0-ALP
      AN=REAL(N)
      Q=1.0-P
!
!     NOTE 2014/03: NOTE THAT WHAT WE ARE CALLING "AGRESTI-COULL" IS
!                   NOW COMNONLY REFERRED TO AS THE "WILSON" INTERVAL.
!                   WHAT IS TYPICALLY CALLED AGRESTI-COULL IN THE
!                   LITERATURE IS WHAT THE AGRESTI-COULL PAPER REFERRED
!                   TO AS THE "ADJUSTED WALD".  THE BROWN, CAI, AND
!                   DASGUPTA PAPER PERFORMED A DETAILED ANALYSIS OF
!                   VARIOUS BINOMIAL APPROXIMATIONS.  THEY RECOMMEND
!                   THAT FOR N < 40, EITHER THE WILSON OR A BAYESIAN
!                   METHOD BASED ON JEFFREYS PRIORS BE USED.  FOR
!                   N > 40, THESE 3 METHODS HAVE COMPARABLE PERFORMANCE.
!
!                   IN DATAPLOT, YOU CAN SPECIFY THE DESIRED METHOD WITH
!                   THE COMMAND
!
!                       SET BINOMIAL METHOD <WILSON/ADJUSTED WALD/JEFFREYS>
!
      IF(IBINME.EQ.'WILS' .OR. IBINME.EQ.'EXAC')THEN
        CALL NORPPF(P2,ZALPHA)
        TERM1=ZALPHA*ZALPHA/(2.0*AN)
        TERM2=ZALPHA*SQRT((P*Q/AN) + ZALPHA*ZALPHA/(4.0*AN*AN))
        TERM3=1.0 + ZALPHA*ZALPHA/AN
        TERM4=(P + TERM1 + TERM2)/TERM3
        TERM5=(P + TERM1 - TERM2)/TERM3
        ALOWLM=MIN(TERM4,TERM5)
        AUPPLM=MAX(TERM4,TERM5)
!
!     ADJUSTED WALD METHOD:
!
      ELSEIF(IBINME.EQ.'WALD')THEN
        AN=REAL(N)
        CALL NORPPF(P2,ZALPHA)
        AK2=ZALPHA**2
        AX=AN*P
        IX=INT(AX+0.5)
        AX=REAL(IX) + (AK2/2.0)
        ANTEMP=AN + (AK2/2.0)
        PTEMP=AX/ANTEMP
        ALOWLM=PTEMP - ZALPHA*SQRT(PTEMP*(1.0-PTEMP))/SQRT(ANTEMP)
        AUPPLM=PTEMP + ZALPHA*SQRT(PTEMP*(1.0-PTEMP))/SQRT(ANTEMP)
      ELSEIF(IBINME.EQ.'JEFF')THEN
        AX=AN*P
        IX=INT(AX+0.5)
        AX=REAL(IX)
        ALPHA=AX+0.5
        BETA=AN-AX+0.5
        CALL BETPPF(P1,ALPHA,BETA,ALOWLM)
        CALL BETPPF(P2,ALPHA,BETA,AUPPLM)
      ENDIF
      IF(AUPPLM.GT.1.0)AUPPLM=1.0
      IF(ALOWLM.LT.0.0)ALOWLM=0.0
!
      IF(IDIR.EQ.'LOWE')THEN
        ALIMIT=ALOWLM
      ELSEIF(IDIR.EQ.'UPPE')THEN
        ALIMIT=AUPPLM
      ELSE
        ALIMIT=CPUMIN
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPAGC1--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR
 9012   FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)ALOWLM,AUPPLM,ALIMIT
 9014   FORMAT('ALOWLM,AUPPLM,ALIMIT = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPAGC1
      SUBROUTINE DPAGN2(X,NROW,NCOL,IVARN1,IVARN2,   &
                        DYS,DYSMA,DYSMB,DVEC,BAN,BANLAT,BANLBT,   &
                        NER,KWAN,NCLUT,LAT,LBT,   &
                        ICASAN,ICAPSW,ICAPTY,IFORSW,MAXNXT,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--PERFORM A HIERARCHIAL CLUSTER ANALYSIS USING AN
!              AGGLOMERATIVE NESTING METHOD FOR <= 100 ROWS USING
!              KAUFFMAN AND ROUSSEEUW "AGNES" ALGORITHM.
!     REFERENCES--KAUFMAN AND ROUSSEEUW (1990), "FINDING GROUPS IN DATA:
!                 AN INTRODUCTION TO CLUSTER ANALYSIS", WILEY.
!               --ROUSSEEUW (1987), "SILHOUETTES: A GRAPHICAL AID TO THE
!                 INTERPRETATION AND VALIDATION OF CLUSTER ANALYSIS",
!                 JOURNAL OF COMPUTATIONAL AND APPLIED MATHEMATICS,
!                 VOL. 20, PP. 53-65, NORTH HOLLAND.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     REFERENCE--ASTM MANUAL STP-15D, PAGES 78-84, 100-105
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2017/08
!     ORIGINAL VERSION--AUGUST      2017.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DIMENSION X(NROW,NCOL)
      DIMENSION DYS(*)
      DIMENSION DYSMA(*)
      DIMENSION DYSMB(*)
      DIMENSION DVEC(*)
      DIMENSION BAN(*)
      DIMENSION BANLAT(*)
      DIMENSION BANLBT(*)
!
      INTEGER NER(*)
      INTEGER KWAN(*)
      INTEGER NCLUT(*)
      INTEGER LAT(*)
      INTEGER LBT(*)
!
      CHARACTER*4 IVARN1(*)
      CHARACTER*4 IVARN2(*)
      CHARACTER*4 ICASAN
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IFLAG
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IOP
      CHARACTER*20 IFORMT
      CHARACTER*3 LAB1
!
      INCLUDE 'DPCOST.INC'
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPAG'
      ISUBN2='N2  '
      IWRITE='OFF'
!
      IFLAGO=0
      ICNT=0
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'AGN2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10)
   10   FORMAT('AT THE BEGINNING OF DPAGN2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,12)NROW,NCOL,ICASAN,ICAPSW,ICAPTY,IFORSW
   12   FORMAT('NROW,NCOL,ICASAN,ICAPSW,ICAPTY,IFORSW = ',3I8,4(2X,A4))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,13)IAGNME,IAGNTY,IAGNSC,IAGNDI,IAGNPR
   13   FORMAT('IAGNME,IAGNTY,IAGNSC,IAGNDI,IAGNPR = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        DO 15 I=1,NROW
          WRITE(ICOUT,17)I,X(I,1),X(I,2),X(I,3)
   17     FORMAT('I,X(I,1),X(I,2),X(I,3) = ',I8,2X,3G15.7)
          CALL DPWRST('XXX','BUG ')
   15   CONTINUE
      ENDIF
!
!               ********************************
!               **   STEP 1--                 **
!               **   CHECK FOR MISSING VALUES **
!               ********************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'AGN2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     FIRST CHECK WHETHER ANY ROWS OR COLUMNS CONTAIN ONLY
!     MISSING DATA.  THIS WILL BE TREATED AS AN ERROR CONDITION.
!
!     CHECK ROWS FIRST
!
      DO 80 I=1,NROW
        DO 90 J=1,NCOL
          IF(X(I,J).NE.PSTAMV)GO TO 99
   90   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,91)
   91   FORMAT('****** ERROR IN AGNES CLUSTERING--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,93)I
   93   FORMAT('       ROW (OBSERVATION) ',I8,' CONTAINS ONLY ',   &
               'MISSING DATA.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
   99   CONTINUE
   80 CONTINUE
!
!     NOW CHECK COLUMNS
!
      NMISS=0
      NMAT=0
!
      DO 730 J=1,NCOL
        NMISSV=0
        DO 740 I=1,NROW
          IF(X(I,J).EQ.PSTAMV)THEN
            NMISSV=NMISSV + 1
          ENDIF
  740   CONTINUE
        IF(NMISSV.EQ.NROW)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,91)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,743)J
  743     FORMAT('       COLUMN (VARIABLE) ',I8,' CONTAINS ONLY ',   &
                 'MISSING DATA.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ELSEIF(NMISSV.EQ.0)THEN
          NMAT=1
        ELSE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,746)IVARN1(J),IVARN2(J),NMISSV
  746     FORMAT('VARIABLE ',2A4,' CONTAINS ',I8,' MISSING VALUES.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,743)J
        ENDIF
        NMISS=NMISS + NMISSV
  730 CONTINUE
!
      IF(NMISS.GT.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,163)
  163   FORMAT('THE TOTAL NUMBER OF MISSING VALUES IS ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(NMAT.EQ.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,165)
  165     FORMAT('****** WARNING IN AGNES CLUSTERING--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,167)
  167     FORMAT('       NO VARIABLES ARE DEFINED FOR ALL ',   &
                 'OBSERVATIONS.')
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
!
!               ******************************
!               **   STEP 2--               **
!               **   SCALE IF REQUESTED     **
!               ******************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'AGN2')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,168)NMISS,PSTAMV
  168   FORMAT('NMISS,PSTAMV = ',I8,2X,G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!     IF NROW = NCOL, ASSUME OUR DATA IS A DISSIMILARITY MATRIX.
!     OTHERWISE, ASSUME OUR DATA IS MEASUREMENT DATA.  DO NOT SCALE
!     DISSIMILARITY DATA EVEN IF SCALING OPTION TURNED ON.
!
      IF(NROW.EQ.NCOL .AND. IAGNTY.EQ.'DISS')THEN
        JDYSS=1
      ELSE
        JDYSS=0
      ENDIF
      NSTAN=0
      IF(JDYSS.EQ.1)GO TO 299
      IF(IAGNSC.EQ.'OFF')GO TO 299
!
      NSTAN=1
      DO 201 JJ=1,NCOL
        NROWT=0
        DO 203 II=1,NROW
          IF(X(II,JJ).NE.PSTAMV)THEN
            NROWT=NROWT+1
            DYS(NROWT)=X(II,JJ)
          ENDIF
  203   CONTINUE
        IF(ISTALO.EQ.'MEAN')THEN
          CALL MEAN(DYS,NROWT,IWRITE,XMEAN,IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'MEDI')THEN
          CALL MEDIAN(DYS,NROWT,IWRITE,DYSMB,MAXNXT,XMEAN,   &
                      IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'MIDM')THEN
          CALL MIDMEA(DYS,NROWT,IWRITE,DYSMB,MAXNXT,XMEAN,   &
                      IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'HARM')THEN
          CALL HARMEA(DYS,NROWT,IWRITE,XMEAN,IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'MINI')THEN
          CALL MINIM(DYS,NROWT,IWRITE,XMEAN,IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'GEOM')THEN
          CALL GEOMEA(DYS,NROWT,IWRITE,XMEAN,IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'BILO')THEN
          CALL BIWLOC(DYS,NROWT,IWRITE,DYSMA,DYSMB,MAXNXT,XMEAN,   &
                      IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'H15 ')THEN
          NCUT=0
          C=1.5
          CALL H15(DYS,NROWT,C,NCUT,XMEAN,XSC,DYSMA,DYSMB,MAXNXT,   &
                      IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'H10 ')THEN
          NCUT=0
          C=1.0
          CALL H15(DYS,NROWT,C,NCUT,XMEAN,XSC,DYSMA,DYSMB,MAXNXT,   &
                      IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'H12 ')THEN
          NCUT=0
          C=1.2
          CALL H15(DYS,NROWT,C,NCUT,XMEAN,XSC,DYSMA,DYSMB,MAXNXT,   &
                      IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'H17 ')THEN
          NCUT=0
          C=1.7
          CALL H15(DYS,NROWT,C,NCUT,XMEAN,XSC,DYSMA,DYSMB,MAXNXT,   &
                      IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'H20 ')THEN
          NCUT=0
          C=2.0
          CALL H15(DYS,NROWT,C,NCUT,XMEAN,XSC,DYSMA,DYSMB,MAXNXT,   &
                      IBUGA3,IERROR)
        ELSE
          CALL MEAN(DYS,NROWT,IWRITE,XMEAN,IBUGA3,IERROR)
        ENDIF
!
        IF(ISTASC.EQ.'SD  ')THEN
          CALL SD(DYS,NROWT,IWRITE,XSD,IBUGA3,IERROR)
        ELSEIF(ISTASC.EQ.'H15S')THEN
          NCUT=0
          C=1.5
          CALL H15(DYS,NROWT,C,NCUT,XLOC,XSD,DYSMA,DYSMB,MAXNXT,   &
                      IBUGA3,IERROR)
        ELSEIF(ISTASC.EQ.'H10S')THEN
          NCUT=0
          C=1.0
          CALL H15(DYS,NROWT,C,NCUT,XLOC,XSD,DYSMA,DYSMB,MAXNXT,   &
                      IBUGA3,IERROR)
        ELSEIF(ISTASC.EQ.'H12S')THEN
          NCUT=0
          C=1.2
          CALL H15(DYS,NROWT,C,NCUT,XLOC,XSD,DYSMA,DYSMB,MAXNXT,   &
                      IBUGA3,IERROR)
        ELSEIF(ISTASC.EQ.'H17S')THEN
          NCUT=0
          C=1.7
          CALL H15(DYS,NROWT,C,NCUT,XLOC,XSD,DYSMA,DYSMB,MAXNXT,   &
                      IBUGA3,IERROR)
        ELSEIF(ISTASC.EQ.'H20S')THEN
          NCUT=0
          C=2.0
          CALL H15(DYS,NROWT,C,NCUT,XLOC,XSD,DYSMA,DYSMB,MAXNXT,   &
                      IBUGA3,IERROR)
        ELSEIF(ISTASC.EQ.'BISC')THEN
          CALL BIWSCA(DYS,NROWT,IWRITE,DYSMA,DYSMB,MAXNXT,XSD,   &
                      IBUGA3,IERROR)
        ELSEIF(ISTASC.EQ.'MAD ')THEN
          CALL MAD(DYS,NROWT,IWRITE,DYSMA,DYSMB,MAXNXT,XSD,   &
                   IBUGA3,IERROR)
        ELSEIF(ISTASC.EQ.'MADN')THEN
          CALL MAD(DYS,NROWT,IWRITE,DYSMA,DYSMB,MAXNXT,XSD,   &
                   IBUGA3,IERROR)
          XSD=XSD/0.67449
        ELSEIF(ISTASC.EQ.'AAD ')THEN
          CALL AAD(DYS,NROWT,IWRITE,DYSMA,MAXNXT,XSD,'MEAN',   &
                   IBUGA3,IERROR)
        ELSEIF(ISTASC.EQ.'IQRA')THEN
          CALL LOWQUA(DYS,NROWT,IWRITE,DYSMA,MAXNXT,RIGH1,   &
                      IBUGA3,IERROR)
          CALL UPPQUA(DYS,NROWT,IWRITE,DYSMA,MAXNXT,RIGH2,   &
                      IBUGA3,IERROR)
          XSD=RIGH2-RIGH1
        ELSEIF(ISTASC.EQ.'NIQR')THEN
          CALL LOWQUA(DYS,NROWT,IWRITE,DYSMA,MAXNXT,RIGH1,   &
                      IBUGA3,IERROR)
          CALL UPPQUA(DYS,NROWT,IWRITE,DYSMA,MAXNXT,RIGH2,   &
                      IBUGA3,IERROR)
          XSD=0.7413*(RIGH2-RIGH1)
        ELSEIF(ISTASC.EQ.'SNSC')THEN
!CCCC     XSD=SN(DYS,NROWT,DSYMA,DSYMB,DVEC)
        ELSEIF(ISTASC.EQ.'MAXI')THEN
          CALL MINIM(DYS,NROWT,IWRITE,XMIN,IBUGA3,IERROR)
          CALL MAXIM(DYS,NROWT,IWRITE,XMAX,IBUGA3,IERROR)
          XSD=XMAX - XMIN
        ELSE
          CALL SD(DYS,NROWT,IWRITE,XMEAN,IBUGA3,IERROR)
        ENDIF
!
        IF(XSD.LE.0.0)THEN
          WRITE(ICOUT,91)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,206)JJ
  206     FORMAT('       VARIABLE ',I4,' HAS ZERO STANDARD DEVIATION ',   &
                 'WHEN SCALING REQUESTED.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
        DO 205 II=1,NROW
          IF(X(II,JJ).NE.PSTAMV)THEN
            AVAL=(X(II,JJ)-XMEAN)/XSD
            X(II,JJ)=AVAL
          ENDIF
  205   CONTINUE
  201 CONTINUE
!
  299 CONTINUE
!
!     OPEN THE AUXILLARY FILES
!
      IFLAGO=1
      IOP='OPEN'
      IFLG11=1
      IFLG21=1
      IFLG31=1
      IFLAG4=1
      IFLAG5=0
      CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                  IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               ************************************
!               **   STEP 3--                     **
!               **   PERFORM THE CLUSTER ANALYSIS **
!               ************************************
!
!      THIS CODE IS A SOMEWHAT MODIFED VERSION OF CODE IN THE
!      AGNES MAIN ROUTINE.
!
      JHALT=0
      NN=NROW
      JPP=NCOL
      RNN=REAL(NN)
      IFLAG='PAM'
!
!       NDYST = 1 => EUCLIDEAN DISTANCES
!             = 2 => MANHATTAN DISTANCES
!
!       JALG  = 1 => AGGLOMERATIVE NESTING (AGNES)
!             = 2 => DIVISIVE ANALYSIS     (DIANA)
!
      NDYST=2
      IF(IAGNDI.EQ.'EUCL')NDYST=1
!
      LARGE=2
      IF(IAGNPR.EQ.'FINA')LARGE=1
!
      JALG=1
      IF(ICASAN.EQ.'DIAN')JALG=2
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'AGN2')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,169)NN,JPP,NDYST,LARGE,JDYSS,JALG
  169   FORMAT('NN,JPP,NDYST,LARGE,JDYSS,JALG = ',6I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
                                                                                                                                  
                                                                                                                                  
      IF(IPRINT.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,301)
  301   FORMAT(10X,'**********************************************')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,302)
  302   FORMAT(10X,'*                                            *')
        CALL DPWRST('XXX','BUG ')
        IF(JALG.EQ.1)THEN
          WRITE(ICOUT,303)
  303     FORMAT(10X,'*  ROUSSEEUW/KAUFFMAN AGGLOMERATIVE NESTING  *')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,304)
  304     FORMAT(10X,'*  CLUSTERING (USING THE AGNES ROUTINE).     *')
          CALL DPWRST('XXX','BUG ')
        ELSE
          WRITE(ICOUT,308)
  308     FORMAT(10X,'*  ROUSSEEUW/KAUFFMAN AGGLOMERATIVE NESTING  *')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,305)
  305     FORMAT(10X,'*  CLUSTERING (USING THE DIANA ROUTINE).     *')
          CALL DPWRST('XXX','BUG ')
        ENDIF
        WRITE(ICOUT,302)
        CALL DPWRST('XXX','BUG ')
        IF(JDYSS.EQ.1)THEN
          WRITE(ICOUT,306)
  306     FORMAT(10X,'*  DATA IS A DISSIMILARITY MATRIX.           *')
          CALL DPWRST('XXX','BUG ')
        ELSE
          WRITE(ICOUT,307)
  307     FORMAT(10X,'*  DATA IS MEASUREMENT DATA.                 *')
          CALL DPWRST('XXX','BUG ')
        ENDIF
        WRITE(ICOUT,302)
        CALL DPWRST('XXX','BUG ')
        IF(IAGNME.EQ.'AVER')THEN
          WRITE(ICOUT,311)
  311     FORMAT(10X,'*  USE AVERAGE LINKAGE METHOD.               *')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(IAGNME.EQ.'WARD')THEN
          WRITE(ICOUT,312)
  312     FORMAT(10X,'*  USE WARD METHOD.                          *')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(IAGNME.EQ.'GOWE')THEN
          WRITE(ICOUT,313)
  313     FORMAT(10X,'*  USE GOWER METHOD.                         *')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(IAGNME.EQ.'CENT')THEN
          WRITE(ICOUT,314)
  314     FORMAT(10X,'*  USE CENTROID METHOD.                      *')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(IAGNME.EQ.'SING')THEN
          WRITE(ICOUT,315)
  315     FORMAT(10X,'*  SINGLE LINKAGE (NEAREST NEIGHBOR METHOD.  *')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(IAGNME.EQ.'COMP')THEN
          WRITE(ICOUT,316)
  316     FORMAT(10X,'*  USE COMPLETE LINKAGE METHOD.              *')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(IAGNME.EQ.'WAVL')THEN
          WRITE(ICOUT,317)
  317     FORMAT(10X,'*  USE WEIGHTED AVERAGE LINKAGE METHOD.      *')
          CALL DPWRST('XXX','BUG ')
        ENDIF
        WRITE(ICOUT,302)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,301)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(LARGE.GT.0 .AND. IPRINT.EQ.'ON' .AND. JDYSS.EQ.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9031)
 9031   FORMAT('STANDARDIZED MEASUREMENTS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9131)
 9131   FORMAT('-------------------------')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IF(NMISS.GT.0)THEN
          WRITE(ICOUT,9032)
 9032     FORMAT('( 99.99 DENOTES A MISSING VALUE)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        JPEND=JPP
        IF(JPP.GT.8)JPEND=8
        DO 60 L=1,NN
          IFORMT='(A3,   F9.2)'
          LAB1='000'
          WRITE(LAB1,'(I3)')L
          WRITE(ICOUT,9033)LAB1,(X(L,J),J=1,JPEND)
 9033     FORMAT(A3,2X,8F9.2)
          CALL DPWRST('XXX','BUG ')
          IF(JPP.GT.8)THEN
            WRITE(ICOUT,9040)(X(L,J),J=9,JPP)
 9040       FORMAT(5X,8F9.2)
            CALL DPWRST('XXX','BUG ')
          ENDIF
   60   CONTINUE
      ENDIF
!
      IF(JDYSS.EQ.0)THEN
!
!       IF RAW DATA ENTERED, CREATE THE DISSIMILARITY MATRIX.
!
        CALL DYSTAP(NN,NCOL,NROW,NCOL,X,DYS,NDYST,PSTAMV,JHALT,   &
                    ISUBRO,IBUGA3)
        IF(JHALT.EQ.1)THEN
          WRITE(ICOUT,91)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,870)
  870     FORMAT('       ERROR IN COMPUTING THE DISSSIMILARITY MATRIX.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
      ELSE
!
!       IF DISSIMILARITY MATRIX ENTERED AS INPUT, COPY LOWER DIAGONAL
!       TO DYS.
!
        DO 74 II=1,MAXNXT
          DYS(II)=0.0
   74   CONTINUE
!
        ICNT=1
        DO 71 II=2,NROW
          DO 73 JJ=1,II-1
            IF(X(II,JJ).LT.0.0)THEN
              WRITE(ICOUT,91)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,872)II,JJ
  872         FORMAT('       ROW ',I5,' COLUMN ',I5,' OF THE ',   &
                     'DISSIMILARITY MATRIX IS NON-POSITIVE.')
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GO TO 9000
            ELSEIF(X(II,JJ).NE.X(JJ,II))THEN
              WRITE(ICOUT,91)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,874)
  874         FORMAT('       THE DISSIMILARITY MATRIX IS NOT ',  &
                     'SYMMETRIC.')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,876)II,JJ,X(II,JJ),JJ,II,X(JJ,II)
  876         FORMAT('       X(',I5,',',I5,') = ',F12.4,' AND ',   &
                     'X(',I5,',',I5,') = ',F12.4)
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GO TO 9000
            ENDIF
            ICNT=ICNT+1
            DYS(ICNT)=X(II,JJ)
   73     CONTINUE
   71   CONTINUE
      ENDIF
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'AGN2')THEN
        ISTEPN='2B'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,76)
   76   FORMAT('AFTER CREATE DYS ARRAY:')
        CALL DPWRST('XXX','BUG ')
        DO 78 II=1,ICNT
          WRITE(ICOUT,79)II,DYS(II)
   79     FORMAT('II,DYS(II) = ',I5,F10.3)
          CALL DPWRST('XXX','BUG ')
   78   CONTINUE
      ENDIF
!
      IF(IPRINT.EQ.'ON' .AND. LARGE.EQ.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9060)
 9060   FORMAT('DISSIMILARITY MATRIX')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9061)
 9061   FORMAT('-------------------------')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        LAB1='001'
        WRITE(ICOUT,9033)LAB1
        CALL DPWRST('XXX','BUG ')
        DO 120 L=2,NN
          LSUBT=L-1
          JPEND=LSUBT
          IF(LSUBT.GT.8)JPEND=8
          DO 110 J=1,LSUBT
            NLJ=MEET(L,J)
            DVEC(J)=DYS(NLJ)
  110     CONTINUE
          IFORMT='(A3,   F9.2)'
          LAB1='000'
          IF(L.LE.9)THEN
            WRITE(LAB1(3:3),'(I1)')L
          ELSEIF(L.LE.99)THEN
            WRITE(LAB1(2:3),'(I2)')L
          ELSE
            WRITE(LAB1,'(I3)')L
          ENDIF
          WRITE(ICOUT,9033)LAB1,(DVEC(J),J=1,JPEND)
          CALL DPWRST('XXX','BUG ')
          IF(LSUBT.GT.8)THEN
            WRITE(ICOUT,9040)(DVEC(J),J=9,LSUBT)
            CALL DPWRST('XXX','BUG ')
          ENDIF
  120   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9070)
 9070   FORMAT('CLUSTER RESULTS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9071)
 9071   FORMAT('---------------')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(JALG.EQ.2)THEN
        CALL SPLYT(NN,KWAN,NER,BAN,DYS,   &
                   NCLUT,LAT,LBT,BANLAT,BANLBT,   &
                   IOUNI2,IOUNI3,IOUNI4,ISUBRO,IBUGA3)
!
        AVAL=0.0
        WRITE(IOUNI1,'(I7,E15.7)')NER(1),AVAL
        DO 152 II=2,NN
          WRITE(IOUNI1,'(I7,E15.7)')NER(II),BAN(II-1)
  152   CONTINUE
!
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'AGN2')THEN
          WRITE(ICOUT,151)
  151     FORMAT('AFTER CALL SPLYT:')
          CALL DPWRST('XXX','BUG ')
          DO 153 II=1,NN
            WRITE(ICOUT,154)II,KWAN(II),NER(II),BAN(II)
  154       FORMAT('II,KWAN(II),NER(II),BAN(II) = ',3I8,G15.7)
            CALL DPWRST('XXX','BUG ')
  153     CONTINUE
        ENDIF
!
        CALL BANDY(NN,BAN,NER,IOUNI5,IAGNBA,ISUBRO,IERROR)
      ELSE
        CALL AVERL(NN,KWAN,NER,BAN,DYS,   &
                   NCLUT,LAT,LBT,BANLAT,BANLBT,   &
                   IOUNI2,IOUNI3,IOUNI4,IAGNME,ISUBRO,IBUGA3)
!
        AVAL=0.0
        WRITE(IOUNI1,'(I7,E15.7)')NER(1),AVAL
        DO 155 II=2,NN
          WRITE(IOUNI1,'(I7,E15.7)')NER(II),BAN(II-1)
  155   CONTINUE
!
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'AGN2')THEN
          WRITE(ICOUT,156)
  156     FORMAT('AFTER CALL AVERL:')
          CALL DPWRST('XXX','BUG ')
          DO 158 II=1,NN
            WRITE(ICOUT,154)II,KWAN(II),NER(II),BAN(II)
            CALL DPWRST('XXX','BUG ')
  158     CONTINUE
        ENDIF
        CALL BANAG(NN,BAN,NER,IOUNI5,IAGNBA,ISUBRO,IERROR)
      ENDIF
!
!               *****************************************
!               **   STEP 4B--                         **
!               **   CREATE VALUES FOR:                **
!               **       1) DENDOGRAM                  **
!               **       2) PROFILE PLOT               **
!               *****************************************
!
      ISTEPN='4B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'AGN2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!NIST DO8105II=1,NROW
!NIST   WRITE(IOUNI1,'(I5)')NCLUV(II)
!8105 CONTINUE
!
!NIST DO8110II=1,NROW
!NIST   ICLUS1=NCLUV(II)
!NIST   DO8112JJ=1,NCOL
!NIST     DYSMA(JJ)=X(II,JJ)
!8112   CONTINUE
!NIST   ICASPL='VEDI'
!NIST   DO8114KK=1,NCLUST
!NIST     DVEC(KK)=CPUMIN
!NIST     NSEL(KK)=0
!8114   CONTINUE
!
!NIST   DO8120JJ=1,NROW
!NIST     IF(II.EQ.JJ)GO TO 8120
!NIST     ICLUS2=NCLUV(JJ)
!NIST     DO8122KK=1,NCOL
!NIST       DYSMB(KK)=X(JJ,KK)
!8122     CONTINUE
!NIST     CALL VECARI(DYSMA,DYSMB,NCOL,ICASPL,IWRITE,
!NIST1                BETER,N3,ADIST,ITYP3,
!NIST1                IBUGA3,ISUBRO,IERROR)
!NIST     IF(ICLUS1.EQ.ICLUS2)THEN
!NIST       NSEL(ICLUS1)=NSEL(ICLUS1)+1
!NIST       IF(NSEL(ICLUS1).EQ.1)THEN
!NIST         DVEC(ICLUS1)=ADIST
!NIST       ELSE
!NIST         TERM1=(ADIST - DVEC(ICLUS1))/REAL(NSEL(ICLUS1))
!NIST         DVEC(ICLUS1)=DVEC(ICLUS1) + TERM1
!NIST       ENDIF
!NIST     ELSE
!NIST       NSEL(ICLUS2)=NSEL(ICLUS2)+1
!NIST       IF(NSEL(ICLUS2).EQ.1)THEN
!NIST         DVEC(ICLUS2)=ADIST
!NIST       ELSE
!NIST         TERM1=(ADIST - DVEC(ICLUS2))/REAL(NSEL(ICLUS2))
!NIST         DVEC(ICLUS2)=DVEC(ICLUS2) + TERM1
!NIST       ENDIF
!NIST     ENDIF
!8120   CONTINUE
!
!NIST   AI=DVEC(ICLUS1)
!NIST   BI=CPUMAX
!NIST   DO8130JJ=1,NCLUST
!NIST     IF(JJ.EQ.ICLUS1)GO TO 8130
!NIST     IF(DVEC(JJ).LT.BI)BI=DVEC(JJ)
!8130   CONTINUE
!NIST   SYL=(BI - AI)/MAX(AI,BI)
!NIST   WRITE(IOUNI4,'(2E15.7)')REAL(NCLUV(II)),SYL
!
!8110 CONTINUE
!8190 CONTINUE
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9085)
 9085   FORMAT('THIS RUN HAS BEEN SUCCESSFULLY COMPLETED.')
        CALL DPWRST('XXX','BUG ')
!NIST   WRITE(ICOUT,8091)
!8091   FORMAT('THE CLUSTER ID VALUES ARE WRITTEN TO dpst1f.dat')
!NIST   CALL DPWRST('XXX','BUG ')
!NIST   WRITE(ICOUT,8093)
!8093   FORMAT('THE WITHIN-CLUSTER SUM OF SQUARES AND ',
!NIST1         'THE NUMBER OF POINTS')
!NIST   CALL DPWRST('XXX','BUG ')
!NIST   WRITE(ICOUT,8095)
!8095   FORMAT('COORDINATES OF MEDOIDS ARE WRITTEN TO dpst2f.dat')
!NIST   CALL DPWRST('XXX','BUG ')
!NIST   WRITE(ICOUT,8097)
!8097   FORMAT('THE DISSIMILARITY MATRIX IS WRITTEN TO dpst3f.dat')
!NIST   CALL DPWRST('XXX','BUG ')
!NIST   IF(NROW*NCOL.LE.2*MAXNXT)THEN
!NIST     WRITE(ICOUT,8099)
!8099     FORMAT('THE SILHOUETTE VALUES ARE WRITTEN TO dpst4f.dat')
!NIST     CALL DPWRST('XXX','BUG ')
!NIST   ENDIF
      ENDIF
!
!               ******************
!               **   STEP 90--  **
!               **   EXIT       **
!               ******************
!
 9000 CONTINUE
!
      IF(IFLAGO.EQ.1)THEN
        IOP='CLOS'
        CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,   &
                    IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                    IBUGA3,ISUBRO,IERROR)
      ENDIF
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'AGN2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPAGN2--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPAGN2
      SUBROUTINE DPALIA(IBUGS2,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--CREATE USER-DEFINED COMMANDS.
!
!              FOR EXAMPLE:
!
!                  ALIAS RX  RESET DATA; CALL CLIPBOARD
!                  ALIAS V   PSVIEW ^FILEOUT
!
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2021/04
!     ORIGINAL VERSION--APRIL     2021.
!
!-----NON-COMMON VARIABLES----------------------------------------
!
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      INCLUDE 'DPCOPA.INC'
!
      CHARACTER (LEN=MAXSTR) :: ICANS
      CHARACTER (LEN=MAXSTR) :: ISTRIN
      CHARACTER (LEN=10)     :: IFORMT
      CHARACTER (LEN=4)      :: ICASE
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='YES'
      IERROR='NO'
      ISTRIN=' '
      ICANS=' '
      IFORMT=' '
!
      NCSTRI=0
      IROW=1
!
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'ALIA')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('AT THE BEGINNING OF DPALIA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGS2,ISUBRO,IFOUND,IERROR
   52   FORMAT('IBUGS2,ISUBRO,IFOUND,IERROR = ',4(A4,2X))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)MAXALI,NUMALI,IWIDTH
   53   FORMAT('MAXALI,NUMALI,IWIDTH = ',3I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMALI.GE.1)THEN
          DO 56 I=1,NUMALI
            WRITE(ICOUT,57)I,IALNAM(I),IALIAS(I)(1:100)
   57       FORMAT('I,IALNAM(I),IALIAS(I)(1:100) = ',I5,2X,A8,2X,A100)
            CALL DPWRST('XXX','BUG ')
   56     CONTINUE
        ENDIF
      ENDIF
!
!               ***************************************************
!               **  STEP 1--                                     **
!               **  DETERMINE THE ELEMENT NUMBER FOR THE ALIAS.  **
!               ***************************************************
!
      IF(ICOM.NE.'ALIA')GO TO 9000
!
      IF(NUMARG.GE.1 .AND. IHARG(1).EQ.'CLEA' .AND.   &
         IHARG2(1).EQ.'R   ')THEN
        IF(NUMARG.EQ.2 .AND. IHARG(2).EQ.'ALL ')THEN
          DO 80 I=1,NUMALI
            IALIAS(I)=' '
            IALNAM(I)=' '
   80     CONTINUE
          NUMALI=0
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,81)
   81     FORMAT('ALL ALIAS COMMANDS HAVE BEEN CLEARED')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(NUMARG.EQ.2)THEN
          DO 91 I=1,NUMALI
            IF(IHARG(2).EQ.IALNAM(I)(1:4) .AND.   &
               IHARG2(2).EQ.IALNAM(I)(5:8))THEN
              IF(I.EQ.NUMALI)THEN
                IALNAM(I)=' '
                IALIAS(I)=' '
                NUMALI=NUMALI-1
              ELSE
                DO 96 J=I,NUMALI-1
                  IALIAS(J)=' '
                  IALIAS(J)=IALIAS(I+1)
                  IALNAM(J)=IALNAM(I+1)
   96           CONTINUE
                IALIAS(NUMALI)=' '
                IALNAM(NUMALI)=' '
                NUMALI=NUMALI-1
              ENDIF
              GO TO 92
            ENDIF
   91     CONTINUE
   92     CONTINUE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,97)IHARG(2),IHARG2(2)
   97     FORMAT('ALIAS ',2A4,' HAS BEEN CLEARED.')
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GO TO 9000
      ENDIF
!
      IF(NUMARG.LE.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR IN ALIAS (DPALIA)--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)
  102   FORMAT('      THE ALIAS COMMAND REQUIRES AT LEAST TWO ',   &
               'ARGUMENTS.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)ICOM,ICOM2
  103   FORMAT('      THE COMMAND IS: ',2A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,105)IHARG(1),IHARG2(1)
  105   FORMAT('      THE FIRST ARGUMENT IS: ',2A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,106)NUMARG
  106   FORMAT('      NUMARG = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!     CHECK TO SEE IF ALIAS NAME CURRENTLY USED
!
      ICASE='NEW'
!
      IF(NUMALI.GE.1)THEN
        DO 110 I=1,NUMALI
          IF(IHARG(1).EQ.IALNAM(I)(1:4) .AND.   &
             IHARG2(1).EQ.IALNAM(I)(5:8))THEN
            IROW=I
            IALIAS(I)=' '
            ICASE='OLD'
            GO TO 119
          ENDIF
  110   CONTINUE
  119   CONTINUE
      ENDIF
!
      IF(ICASE.EQ.'NEW')THEN
!
        IF(NUMALI.GE.MAXALI)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,122)
  122     FORMAT('      THE MAXIMUM NUMBER OF ALIASES HAS BEEN ',   &
                 'EXCEEDED.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,123)MAXALI
  123     FORMAT('      THE MAXIMUM NUMBER OF ALIASES ALLOWED IS:   ',   &
                 I6)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,125)NUMALI
  125     FORMAT('      THE NUMBER OF CURRENTLY DEFINED ALIASES IS: ',   &
                 I6)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
!
        NUMALI=NUMALI+1
        IALNAM(NUMALI)(1:4)=IHARG(1)
        IALNAM(NUMALI)(5:8)=IHARG2(1)
        IALIAS(NUMALI)=' '
        IROW=NUMALI
      ENDIF
!
!               *************************************************
!               **  STEP 2--                                   **
!               **  DETERMINE THE COMMAND STRING.              **
!               *************************************************
!
      ICANS=' '
      DO 130 I=1,IWIDTH
        ICANS(I:I)=IANSLC(I)
  130 CONTINUE
!
      ISTART=1
      ISTOP=IWIDTH
      IWORD=3
      CALL DPEXW3(ICANS,ISTART,ISTOP,IWORD,   &
                  ICOL1,ICOL2,ISTRIN,NCSTRI,   &
                  IBUGS2,ISUBRO,IERROR)
!
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'ALIA')THEN
        WRITE(ICOUT,131)ICOL1,ICOL2,NCSTRI
  131   FORMAT('AFTER CALL DPEXW3: ICOL1,ICOL2,NCSTRI = ',3I6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,133)ISTRIN(1:NCSTRI)
  133   FORMAT('ISTRIN(1:NCSTRI) = ',A)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(NCSTRI.GE.1)THEN
        IALIAS(IROW)(1:NCSTRI)=ISTRIN(1:NCSTRI)
        IFOUND='YES'
!
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,141)IALNAM(IROW)
  141     FORMAT('THE ALIAS, ',A8,', HAS BEEN SET TO')
          CALL DPWRST('XXX','BUG ')
          IFORMT='(A    )'
          WRITE(IFORMT(3:6),'(I4)')NCSTRI
          WRITE(ICOUT,IFORMT)IALIAS(IROW)(1:NCSTRI)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GO TO 9000
      ELSE
        NCSTRI=0
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,151)
  151   FORMAT('NO COMMAND DEFINED FOR THE ALIAS.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'ALIA')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('AT THE END       OF DPALIA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)IFOUND,IERROR,NUMALI
 9021   FORMAT('IFOUND,IERROR,NUMALI = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        IF(NUMALI.GE.1)THEN
          DO 9026 I=1,NUMALI
            WRITE(ICOUT,57)I,IALNAM(I),IALIAS(I)(1:100)
            CALL DPWRST('XXX','BUG ')
 9026     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPALIA
      SUBROUTINE DPALLA(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                        IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--FORM
!              1) ALLAN VARIANCE PLOT;
!              2) ALLAN STANDARD DEVIATION PLOT;
!
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--87/1
!     ORIGINAL VERSION--JANUARY   1987.
!     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE COMMON
!     UPDATED         --JANUARY   2012. USE DPPARS
!     UPDATED         --JANUARY   2012. SUPPORT REPLICATION AND
!                                       MULTIPLE KEYWORDS
!     UPDATED         --JUNE      2019. TWEAK TO SCRATCH STORAGE
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
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IFOUN1
      CHARACTER*4 IFOUN2
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
!
      DIMENSION Y1(MAXOBV)
      DIMENSION Y2(MAXOBV)
      DIMENSION X1(MAXOBV)
      DIMENSION XIDTEM(MAXOBV)
      DIMENSION XIDTE2(MAXOBV)
      DIMENSION XIDTE3(MAXOBV)
      DIMENSION XTEMP1(MAXOBV)
      DIMENSION XTEMP2(MAXOBV)
      DIMENSION ZY(MAXOBV)
      DIMENSION XDESGN(MAXOBV,2)
!
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),Y2(1))
      EQUIVALENCE (GARBAG(IGARB3),XTEMP1(1))
      EQUIVALENCE (GARBAG(IGARB4),XTEMP2(1))
      EQUIVALENCE (GARBAG(IGARB5),XIDTEM(1))
      EQUIVALENCE (GARBAG(IGARB6),XIDTE2(1))
      EQUIVALENCE (GARBAG(IGARB7),XIDTE3(1))
      EQUIVALENCE (GARBAG(IGARB8),ZY(1))
      EQUIVALENCE (GARBAG(IGARB9),X1(1))
      EQUIVALENCE (GARBAG(IGAR10),XDESGN(1,1))
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      IMULT='OFF'
      IREPL='OFF'
!
      ISUBN1='DPAL'
      ISUBN2='LA  '
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
!               ******************************************************
!               **  TREAT THE FOLLOWING CASES--                      *
!               **        1) ALLAN VARIANCE PLOT                     *
!               **        2) ALLAN SD       PLOT                     *
!               ******************************************************
!
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'ALLA')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPALLA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASPL,IAND1,IAND2,MAXCOL
   52   FORMAT('ICASPL,IAND1,IAND2,MAXCOL = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGG2,IBUGG3,IBUGQ,ISUBRO
   53   FORMAT('IBUGG2,IBUGG3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ******************************************************
!               **  STEP 1--                                        **
!               **  EXTRACT THE COMMAND                             **
!               **  LOOK FOR ONE OF THE FOLLOWING COMMANDS:         **
!               **    1) ALLAN VARIANCE PLOT Y                      **
!               **       ALLAN SD       PLOT Y                      **
!               **    2) MULTIPLE ALLAN VARIANCE PLOT Y1 ... YK     **
!               **       MULTIPLE ALLAN SD       PLOT Y1 ... YK     **
!               **    3) REPLICATED ALLAN VARIANCE PLOT Y X1  X2    **
!               **       REPLICATED ALLAN SD       PLOT Y X1  X2    **
!               ******************************************************
!
!     NOTE: AV, AS, AND ASD ARE SYNONYMS FOR ALLAN VARIANCE AND
!           ALLAN SD PLOT.
!
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ALLA')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICOM.EQ.'ALLA')GO TO 89
      IF(ICOM.EQ.'AV  ')GO TO 89
      IF(ICOM.EQ.'AS  ')GO TO 89
      IF(ICOM.EQ.'ASD ')GO TO 89
      IF(ICOM.EQ.'MULT')THEN
        IMULT='ON'
        GO TO 89
      ENDIF
      IF(ICOM.EQ.'REPL')THEN
        IREPL='ON'
        GO TO 89
      ENDIF
      GO TO 9000
!
   89 CONTINUE
      ICASPL='ALVA'
      ILASTC=-9999
!
      IF(ICOM.EQ.'ALLA')THEN
        IFOUN1='YES'
      ELSEIF(ICOM.EQ.'AV  ')THEN
        IFOUN1='YES'
      ELSEIF(ICOM.EQ.'AS  ')THEN
        IFOUN1='YES'
      ELSEIF(ICOM.EQ.'ASD ')THEN
        IFOUN1='YES'
      ELSEIF(ICOM.EQ.'MULT')THEN
        IMULT='ON'
      ELSEIF(ICOM.EQ.'REPL')THEN
        IREPL='ON'
      ENDIF
!
      ISTOP=NUMARG-1
      DO 90 I=1,NUMARG
        IF(IHARG(I).EQ.'PLOT')THEN
          ISTOP=I
          GO TO 99
        ENDIF
   90 CONTINUE
   99 CONTINUE
!
      IFOUND='NO'
      DO 100 I=1,ISTOP
        IF(IHARG(I).EQ.'=')THEN
          IFOUND='NO'
          GO TO 9000
        ELSEIF(IHARG(I).EQ.'AV')THEN
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I)
          ICASPL='ALVA'
        ELSEIF(IHARG(I).EQ.'ALLA' .AND. IHARG(I+1).EQ.'VARI')THEN
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+1)
          ICASPL='ALVA'
        ELSEIF(IHARG(I).EQ.'AS' .OR. IHARG(I).EQ.'ASD')THEN
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I)
          ICASPL='ALSD'
        ELSEIF(IHARG(I).EQ.'ALLA' .AND. IHARG(I+1).EQ.'SD  ')THEN
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+1)
          ICASPL='ALSD'
        ELSEIF(IHARG(I).EQ.'ALLA' .AND. IHARG(I+1).EQ.'STAN' .AND.   &
               IHARG(I+2).EQ.'DEVI')THEN
          IFOUN1='YES'
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I+2)
          ICASPL='ALSD'
        ELSEIF(IHARG(I).EQ.'PLOT')THEN
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I)
        ELSEIF(IHARG(I).EQ.'REPL')THEN
          IREPL='ON'
        ELSEIF(IHARG(I).EQ.'MULT')THEN
          IMULT='ON'
        ENDIF
  100 CONTINUE
!
      IF(IFOUN1.EQ.'YES' .AND. IFOUN2.EQ.'YES')IFOUND='YES'
      IF(IFOUND.EQ.'NO')GO TO 9000
!
      IF(IMULT.EQ.'ON')THEN
        IF(IREPL.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
  101     FORMAT('***** ERROR IN ALLAN VARIANCE PLOT--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,102)
  102     FORMAT('      YOU CANNOT SPECIFY BOTH "MULTIPLE" AND ',   &
                 '"REPLICATION" FOR THE ALLAN VARIANCE PLOT.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
      ENDIF
!
      IF(ILASTC.GE.1)THEN
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
        ILASTC=0
      ENDIF
!
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'ALLA')THEN
        WRITE(ICOUT,112)ICASPL,IMULT,IREPL
  112   FORMAT('ICASPL,IMULT,IREPL = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ****************************************
!               **  STEP 2--                          **
!               **  EXTRACT THE VARIABLE LIST         **
!               ****************************************
!
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ALLA')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      INAME='ALLAN VARIANCE PLOT'
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
      MAXNVA=3
      IF(IREPL.EQ.'ON')THEN
        MINNVA=2
        MAXNVA=3
      ELSEIF(IMULT.EQ.'ON')THEN
        MINNVA=1
        MAXNVA=30
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ALLA')THEN
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
!               **  GENERATE THE ALLAN VARIANCE PLOTS FOR **
!               **  THE VARIOUS CASES.                    **
!               ********************************************
!
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ALLA')THEN
        ISTEPN='6'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,601)NRESP,NREPL
  601   FORMAT('NRESP,NREPL = ',2I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(NRESP.GE.1 .AND. NREPL.EQ.0)THEN
        ISTEPN='8A'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ALLA')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!       LOOP THROUGH EACH OF THE RESPONSE VARIABLES
!
        NPLOTP=0
        DO 810 IRESP=1,NRESP
          NCURVE=IRESP
!
          IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ALLA')THEN
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
          IF(IERROR.EQ.'YES')GO TO 9000
!
!               *****************************************************
!               **  STEP 8B--                                      **
!               **  FORM THE VERTICAL AND HORIZONTAL AXIS          **
!               **  VALUES Y(.) AND X(.) FOR THE PLOT.             **
!               *****************************************************
!
          CALL DPALL2(Y1,Y2,NLOCAL,NCURVE,ICASPL,MAXN,   &
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
      ELSEIF(NRESP.EQ.1 .AND. NREPL.GE.1)THEN
        ISTEPN='9A'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ALLA')   &
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
!
          ICOLC=1
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
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ALLA')THEN
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,931)
  931     FORMAT('***** FROM THE MIDDLE  OF DPALLA--')
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
                ZY(K)=Y1(I)
              ENDIF
 1130       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            IF(NTEMP.GT.0)THEN
              CALL DPALL2(ZY,Y2,NTEMP,NCURVE,ICASPL,MAXN,   &
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
            IF(NTEMP.GT.0)THEN
              CALL DPALL2(ZY,Y2,NTEMP,NCURVE,ICASPL,MAXN,   &
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
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'ALLA')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPALLA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9013   FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',3I8,3(2X,A4))
        CALL DPWRST('XXX','BUG ')
        IF(NPLOTP.GE.1)THEN
          DO 9015 I=1,NPLOTP
            WRITE(ICOUT,9016)I,Y(I),X(I),D(I)
 9016       FORMAT('I,Y(I),X(I),D(I) = ',I8,3F12.5)
            CALL DPWRST('XXX','BUG ')
 9015     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPALLA
      SUBROUTINE DPALL2(Y1,Y2,N,NCURVE,ICASPL,MAXN,   &
                        Y,X,D,NPLOTP,NPLOTV,   &
                        IBUGG3,ISUBRO,IERROR)
!
!     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
!              THAT WILL DEFINE
!              1) ALLAN VARIANCE PLOT
!     NOTE-- IN ORDER THAT THE RESULTS OF THIS ALLAN ... PLOT ANALYSIS
!            BE VALID AND PROPERLY INTERPRETED, THE INPUT DATA
!            IN X SHOULD BE EQUI-SPACED IN TIME
!            (OR WHATEVER VARIABLE CORRESPONDS TO TIME).
!
!              THE HORIZONTAL AXIS OF THE PERIODOGRAM PRODUCED
!              BY THIS SUBROUTINE IS GROUP SIZE.
!
!     INPUT ARGUMENTS--Y1     = THE SINGLE PRECISION VECTOR OF
!                               (UNSORTED) OBSERVATIONS
!                               FOR THE FIRST  VARIABLE.
!                    --Y2     = THE SINGLE PRECISION VECTOR OF
!                               (UNSORTED) OBSERVATIONS.
!                               FOR THE SECOND VARIABLE.
!                      N      = THE INTEGER NUMBER OF OBSERVATIONS
!                               IN THE VECTOR X.
!     PRINTING--YES.
!     RESTRICTIONS--THE SAMPLE SIZE N MUST BE
!                   SMALLER THAN OR EQUAL TO 1000.
!                 --THE SAMPLE SIZE N MUST BE GREATER
!                   THAN OR EQUAL TO 3.
!     OTHER DATAPAC   SUBROUTINES NEEDED--PLOTC0, PLOTSP, AND CHSPPF.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--SQRT.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     COMMENT--THE USUAL MAXIMUM NUMBER OF GROUP SIZES
!              FOR WHICH THE ALLAN VARIANCE PLOT IS
!              COMPUTED IS N/2 WHERE N IS
!              THE SAMPLE SIZE (LENGTH OF THE
!              DATA RECORD IN THE VECTOR X).
!     REFERENCES--ALLAN NBS PUBLICATION XXX
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1966)
!               EXCEPTION--HOLLERITH STRINGS IN FORMAT STATEMENTS
!                          DENOTED BY QUOTES RATHER THAN NH.
!     VERSION NUMBER--86/7
!     ORIGINAL VERSION--APRIL     1986.
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
      DIMENSION Y2(*)
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION D(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPAL'
      ISUBN2='L2  '
!
      IERROR='NO'
!
      Y2BAR=0.0
!
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'ALL2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,70)
   70   FORMAT('***** AT THE BEGINNING OF DPALL2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)N,ICASPL,MAXN
   71   FORMAT('N,ICASPL,MAXN = ',I8,2X,A4,I8)
        CALL DPWRST('XXX','BUG ')
        DO 73 I=1,N
          WRITE(ICOUT,74)I,Y1(I),Y2(I)
   74     FORMAT('I, Y1(I), Y2(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   73   CONTINUE
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
   31   FORMAT('***** ERROR IN ALLAN VARIANCE PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,32)
   32   FORMAT('      THE NUMBER OF OBSERVATIONS MUST BE AT LEAST 2.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,34)N
   34   FORMAT('      THE NUMBER OF OBSERVATIONS = ',I6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      HOLD=Y1(1)
      DO 60 I=1,N
        IF(Y1(I).NE.HOLD)GO TO 69
   60 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,31)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,62)HOLD
   62 FORMAT('      ALL ELEMENTS IN Y1 ARE IDENTICALLY EQUAL TO ',G15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
   69 CONTINUE
!
!               *******************************
!               **  STEP 1--                 **
!               **  COMPUTE THE SAMPLE MEAN  **
!               *******************************
!
      AN=N
      SUM=0.0
      DO 100 I=1,N
      SUM=SUM+Y1(I)
  100 CONTINUE
      Y1BAR=SUM/AN
!
!               *************************************
!               **  STEP 2--                       **
!               **  COMPUTE THE SAMPLE VARIANCE    **
!               **  AND SUM OF SQUARED DEVIATIONS  **
!               *************************************
!
      SUM=0.0
      DO 200 I=1,N
      SUM=SUM+(Y1(I)-Y1BAR)*(Y1(I)-Y1BAR)
  200 CONTINUE
      SSQY1=SUM
      VARBY1=SSQY1/AN
      VARY1=SSQY1/(AN-1.0)
      SDBY1=0.0
      IF(VARBY1.GT.0.0)SDBY1=SQRT(VARBY1)
      SDY1=0.0
      IF(VARY1.GT.0.0)SDY1=SQRT(VARY1)
!
!               **************************************
!               **  STEP 4--                        **
!               **  BRANCH TO THE APPROPRIATE CASE  **
!               **  AND DETERMINE PLOT COORDINATES  **
!               **************************************
!
!               *********************************************************
!               **  STEP 4.1--                                          *
!               **  COMPUTE ALLAN VARIANCE AND ALLAN STANDARD DEVIATION *
!               **  FOR Y1                                              *
!               **  REFERENCE--ALLAN, NBS PUBLICATION XXX               *
!               *********************************************************
!
      IF(ICASPL.EQ.'ALVA')GO TO 1100
      IF(ICASPL.EQ.'ALSD')GO TO 1100
      GO TO 1900
!
 1100 CONTINUE
!
      J=0
!
      NHALF=N/2
      NIMAX=NHALF
      IF(NHALF.GT.MAXN)NIMAX=MAXN
!
      DO 1110 NI=1,NIMAX
        ANI=NI
        J=J+1
!
        IMIN1=0
        IMAX1=0
        IMIN2=0
        IMAX2=0
!
        SSQD=0.0
        IRATIO=N/NI
        KMAX=IRATIO/2
        AKMAX=KMAX
        DO 1120 K=1,KMAX
!
         IMIN1=IMAX2+1
         IMAX1=IMIN1+(NI-1)
         IMIN2=IMAX1+1
         IMAX2=IMIN2+(NI-1)
!
         SUM=0.0
         DO 1130 I=IMIN1,IMAX1
           SUM=SUM+Y1(I)
 1130    CONTINUE
         Y3=SUM/ANI
!
         SUM=0.0
         DO 1140 I=IMIN2,IMAX2
           SUM=SUM+Y1(I)
 1140    CONTINUE
         Y4=SUM/ANI
!
         DEL=Y4-Y3
         DELSQ=DEL*DEL
         SSQD=SSQD+DELSQ
!
 1120   CONTINUE
!
        AV=SSQD/(2.0*AKMAX)
        ASD=0.0
        IF(AV.GT.0.0)ASD=SQRT(AV)
!
        Y(NPLOTP+J)=0.0
        IF(ICASPL.EQ.'ALVA')Y(NPLOTP+J)=AV
        IF(ICASPL.EQ.'ALSD')Y(NPLOTP+J)=ASD
        X(NPLOTP+J)=J
        D(NPLOTP+J)=REAL(NCURVE)
!
 1110 CONTINUE
      NPLOTP=NPLOTP+J
      NPLOTV=2
      GO TO 9000
!
 1900 CONTINUE
!
!               ******************
!               **   STEP 90--  **
!               **   EXIT       **
!               ******************
!
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'ALL2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPALL2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICASPL,IERROR,NPLOTP,NPLOTV
 9012   FORMAT('ICASPL,IERROR,NPLOTP,NPLOTV = ',2(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)N,NHALF,MAXN,NIMAX,IRATIO,KMAX
 9013   FORMAT('N,NHALF,MAXN,NIMAX,IRATIO,KMAX = ',6I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)IMIN1,IMAX1,IMIN2,IMAX2
 9015   FORMAT('IMIN1,IMAX1,IMIN2,IMAX2 = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)Y3,Y4,DEL,DELSQ,SSQD,AV,ASD
 9016   FORMAT('Y3,Y4,DEL,DELSQ,SSQD,AV,ASD = ',7E11.4)
        CALL DPWRST('XXX','BUG ')
        DO 9020 I=1,NPLOTP
          WRITE(ICOUT,9021)I,Y(I),X(I),D(I)
 9021     FORMAT('I,Y(I),X(I),D(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPALL2
      SUBROUTINE DPAMPL(IHARG,IARGT,ARG,NUMARG,   &
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
!     PURPOSE--DRAW ONE OR MORE AMPLIFIERS (DEPENDING ON HOW MANY
!              NUMBERS ARE PROVIDED).  THE COORDINATES ARE IN
!              STANDARDIZED UNITS OF 0 TO 100.
!     NOTE--THE INPUT COORDINATES DEFINE THE BACK CENTER AND THE FRONT
!           TIP OF THE AMPLIFIER.
!     NOTE-THE USUAL INPUT NUMBER OF COORDINATES IS 2
!          AND THEREFORE THE USUAL INPUT NUMBER OF NUMBERS IS 2*2 = 4.
!     NOTE--IF 2 NUMBERS ARE PROVIDED, THEN THE BACK CENTER OF THE
!           DRAWN AMPLIFIER WILL BE AT THE LAST CURSOR POSITION, AND THE
!           FRONT POINT OF THE DRAWN AMPLIFIER WILL BE AT THE (X,Y)
!           POINT (EITHER ABSOLUTE OR RELATIVE) AS DEFINED BY THE 2
!           NUMBERS.
!     NOTE--IF 4 NUMBERS ARE PROVIDED, THEN THE BACK CENTER OF THE
!           DRAWN AMPLIFIER WILL BE AT THE ABSOLUTE (X,Y) POSITION AS
!           DEFINED BY THE FIRST 2 NUMBERS, AND THE FRONT POINT OF THE
!           DRAWN AMPLIFIER WILL BE AT THE (X,Y) POINT (EITHER ABSOLUTE
!           OR RELATIVE) AS DEFINED BY THE THIRD AND FOURTH NUMBERS.
!     NOTE--IF 6 NUMBERS ARE PROVIDED, THEN 2 AMPLIFIERS WILL BE DRAWN.
!           THE BACK CENTER OF THE FIRST DRAWN AMPLIFIER WILL BE AT THE
!           (X,Y) POSITION AS RESULTING FROM THE FIRST AND SECOND
!           NUMBERS, AND THE FRONT POINT OF THE FIRST DRAWN AMPLIFIER
!           WILL BE AT THE (X,Y) POINT (EITHER ABSOLUTE OR RELATIVE)
!           AS DEFINED BY THE THIRD AND FOURTH NUMBERS.  THE SECOND
!           DRAWN AMPLIFIER WILL GO FROM THE (X,Y) POSITION AS RESULTING
!           FROM THE THIRD AND FOURTH NUMBERS, TO THE (X,Y) POINT
!           (EITHER ABSOLUTE OR RELATIVE) AS DEFINED BY THE FIFTH AND
!           SIXTH NUMBERS.
!     NOTE--IF 8 NUMBERS ARE PROVIDED, THEN 3 AMPLIFIERS WILL BE DRAWN.
!     NOTE--AND SO FORTH FOR 10, 12, 14 ... NUMBERS.
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
!                 PHONE--301-975-2855
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
!     UPDATED         --DECEMBER  2018. CHECK FOR "DISCRETE", "NULL", OR
!                                       "NONE" DEVICES
!     UPDATED         --DECEMBER  2018. SUPPORT FOR "DEVICE ... SCALE"
!                                       COMMAND
!     UPDATED         --JUNE      2019. CREATE SCRATCH STORAGE IN DPAMPL
!                                       RATHER THAN DPAMP2
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
      DIMENSION PX(1000)
      DIMENSION PY(1000)
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(IGARB1),PX(1))
      EQUIVALENCE (GARBAG(IGARB2),PY(1))
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
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
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'AMPL')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPAMPL--')
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
      IFIG='AMPL'
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
          IF(IARGT(I).EQ.'NUMB')GO TO 1120
          GO TO 1130
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
        IF(UNITSW.EQ.'DATA')CALL DPCODS('Y',Y5,Y5,IBUGD2,ISUBRO,IERROR)
        IF(ITYPEO.EQ.'RELA')Y2=Y1+Y2
!
        CALL DPAMP2(X1,Y1,X2,Y2,PX,PY,   &
                    IFIG,ILINPA,ILINCO,ILINC2,MAXLNZ,PLINTH,   &
                    AREGBA,IREBLI,IREBCO,IREBC2,MAXRGZ,PREBTH,   &
                    IREFSW,IREFCO,IREFC2,   &
                    IREPTY,IREPLI,IREPCO,IREPC2,PREPTH,PREPSP,   &
                    PTEXHE,PTEXWI,PTEXVG,PTEXHG)
!
        X1=X2
        Y1=Y2
        GO TO 1160
!
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
 1131 FORMAT('***** ERROR IN AMPLIFIER (DPAMPL)--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1132)
 1132 FORMAT('      ILLEGAL FORM FOR THE AMPLIFIER COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1134)
 1134 FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE PROPER FORM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1135)
 1135 FORMAT('      SUPPOSE IT IS DESIRED TO DRAW AN AMPLIFIER ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1136)
 1136 FORMAT('      WITH BACK CENTER AT 20 20 ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1137)
 1137 FORMAT('      AND FRONT TIP AT 40 60')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1141)
 1141 FORMAT('      THEN THE ALLOWABLE FORMS ARE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1142)
 1142 FORMAT('      AMPLIFIER 20 20 40 60 ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1143)
 1143 FORMAT('      AMPLIFIER ABSOLUTE 20 20 40 60 ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1145)
 1145 FORMAT('      AMPLIFIER RELATIVE 20 20 40 60 ')
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'AMPL')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPAMPL--')
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
      END SUBROUTINE DPAMPL
      SUBROUTINE DPAMP2(X1,Y1,X2,Y2,PX,PY,   &
                        IFIG,ILINPA,ILINCO,ILINC2,MAXLN,PLINTH,   &
                        AREGBA,IREBLI,IREBCO,IREBC2,MAXRG,PREBTH,   &
                        IREFSW,IREFCO,IREFC2,   &
                        IREPTY,IREPLI,IREPCO,IREPC2,PREPTH,PREPSP,   &
                        PTEXHE,PTEXWI,PTEXVG,PTEXHG)
!
!     PURPOSE--DRAW AN AMPLIFIER WITH THE BACK CENTER AT (X1,Y1)
!              AND THE FRONT TIP AT (X2,Y2).
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--APRIL     1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --JANUARY   1989. MODIFY CALLS TO DPDRPL (ALAN)
!     UPDATED         --JANUARY   1989. MODIFY CALL  TO DPFIRE (ALAN)
!     UPDATED         --JUNE      2019. MOVE CREATION OF SCRATCH
!                                       STORAGE TO DPAMPL
!     UPDATED         --OCTOBER   2020. ARGUMENTS FOR RGB COLOR SUPPORT
!
!-----NON-COMMON VARIABLES-------------------------------------
!
      DIMENSION PX(*)
      DIMENSION PY(*)
!
      CHARACTER*4 IFIG
      CHARACTER*4 IPATT2
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
      CHARACTER*4 ICOLF
      CHARACTER*4 ICOLP
      CHARACTER*4 ICOL
      CHARACTER*4 IFLAG
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
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'AMP2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPAMP2--')
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
!
!               *********************************
!               **  STEP 1--                   **
!               **  DETERMINE THE COORDINATES  **
!               **  FOR THE AMPLIFIER          **
!               *********************************
!
      DELX=X2-X1
      DELY=Y2-Y1
      LEN=INT(SQRT((X2-X1)**2+(Y2-Y1)**2)+0.1)
      ALEN=LEN
      IF(ABS(DELX).GE.0.00001)THEN
        THETA=ATAN(DELY/DELX)
      ELSEIF(ABS(DELX).LT.0.00001.AND.DELY.GE.0.0)THEN
        THETA=3.1415926/2.0
      ELSEIF(ABS(DELX).LT.0.00001.AND.DELY.LT.0.0)THEN
        THETA=-3.1415926/2.0
      ENDIF
!
      JXDEL=INT(ALEN+0.1)
      JYDEL=INT((SQRT(3.0)/3.0)*ALEN + 0.1)
!
      XDEL=REAL(JXDEL)
      YDEL=REAL(JYDEL)
!
      K=0
!
      X=ALEN
      Y=0
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
!
      X=0.0
      Y=-YDEL
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
!
      X=0.0
      Y=YDEL
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
!
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
      IF(IREFSW(1).EQ.'OFF')GO TO 2190
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
      CALL DPFIRE(PX,PY,NP,   &
                  IFIG,IPATT,PTHICK,PXGAP,PYGAP,   &
                  ICOLF,ICOLFR,ICOLFG,ICOLFB,   &
                  ICOLP,ICOLPR,ICOLPG,ICOLPB,   &
                  IPATT2)
 2190 CONTINUE
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
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'AMP2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPAMP2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9039)IERRG4
 9039   FORMAT('IERRG4 = ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPAMP2
      SUBROUTINE DPAND(IHARG,IARGT,ARG,NUMARG,   &
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
!     PURPOSE--DRAW ONE OR MORE LOGICAL ANDS (DEPENDING ON HOW MANY
!              NUMBERS ARE PROVIDED).  THE COORDINATES ARE IN
!              STANDARDIZED UNITS OF 0 TO 100.
!     NOTE--THE INPUT COORDINATES DEFINE THE BACK CENTER AND THE FRONT
!           CENTER OF THE LOGICAL AND.
!     NOTE-THE USUAL INPUT NUMBER OF COORDINATES IS 2 AND THEREFORE THE
!          USUAL INPUT NUMBER OF NUMBERS IS 2*2 = 4.
!     NOTE--IF 2 NUMBERS ARE PROVIDED, THEN THE DRAWN LOGICAL AND WILL
!           GO FROM THE LAST CURSOR POSITION TO THE (X,Y) POINT (EITHER
!           ABSOLUTE OR RELATIVE) AS DEFINED BY THE 2 NUMBERS.
!     NOTE--IF 4 NUMBERS ARE PROVIDED, THEN THE DRAWN LOGICAL AND WILL
!           GO FROM THE ABSOLUTE (X,Y) POSITION AS DEFINED BY THE FIRST
!           2 NUMBERS TO THE (X,Y) POINT (EITHER ABSOLUTE OR RELATIVE)
!           AS DEFINED BY THE THIRD AND FOURTH NUMBERS.
!     NOTE--IF 6 NUMBERS ARE PROVIDED, THEN THE DRAWN LOGICAL AND WILL
!           GO FROM THE (X,Y) POSITION AS RESULTING FROM THE THIRD AND
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
!                 PHONE--301-975-2855
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
!     UPDATED         --DECEMBER  2018. SUPPORT FOR "DEVICE ... SCALE"
!                                       COMMAND
!     UPDATED         --JUNE      2019. CREATE SCRATCH STORAGE IN DPAND
!                                       RATHER THAN DPAND2
!     UPDATED         --OCTOBER   2020. SUPPORT FOR RGB COLORS
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
!CCCC ADD FOLLOWING LINE MARCH 1997.
      CHARACTER*4 IDFONT
!CCCC ADD FOLLOWING LINE JULY 1997.
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
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
      DIMENSION PX(1000)
      DIMENSION PY(1000)
      EQUIVALENCE (GARBAG(IGARB1),PX(1))
      EQUIVALENCE (GARBAG(IGARB2),PY(1))
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
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
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'AND')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPAND--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)NUMARG,NUMDEV,IFOUND
   53   FORMAT('NUMARG,NUMDEV,IFOUND = ',2I8,2X,A4)
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
   63   FORMAT('IREBLI(1),IREBCO(1),PREBTH(1),AREGBA(1) = ',2(A4,2X),   &
               2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,64)IREFSW(1),IREFCO(1)
   64   FORMAT('IREFSW(1),IREFCO(1) = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,65)IREPTY(1),IREPLI(1),IREPCO(1),PREPTH(1),PREPSP(1)
   65   FORMAT('IREPTY(1),IREPLI(1),IREPCO(1),PREPTH(1),PREPSP(1) = ',   &
               3(A4,2X),2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,69)PTEXHE,PTEXWI,PTEXVG,PTEXHG
   69   FORMAT('PTEXHE,PTEXWI,PTEXVG,PTEXHG = ',4G15.7)
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
        WRITE(ICOUT,88)IBUGG4,ISUBG4,IERRG4,IBUGD2
   88   FORMAT('IBUGG4,ISUBG4,IERRG4,IBUGD2 = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IFIG='AND'
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
          IERRG4='YES'
          WRITE(ICOUT,1131)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1132)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1134)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1135)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1136)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1137)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1141)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1142)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1143)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1145)
          CALL DPWRST('XXX','BUG ')
          GO TO 9000
        ENDIF
!
        IF(ILOCFN.GT.NUMARG)THEN
          IERRG4='YES'
          WRITE(ICOUT,1131)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1132)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1134)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1135)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1136)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1137)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1141)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1142)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1143)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1145)
          CALL DPWRST('XXX','BUG ')
          GO TO 9000
        ENDIF
!
        DO 1120 I=ILOCFN,NUMARG
          IF(IARGT(I).EQ.'NUMB')GO TO 1120
!
          IERRG4='YES'
          WRITE(ICOUT,1131)
 1131     FORMAT('***** ERROR IN AND COMMAND (DPAND)--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1132)
 1132     FORMAT('      ILLEGAL FORM FOR THE AND COMMAND.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1134)
 1134     FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE ',   &
                 'PROPER FORM--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1135)
 1135     FORMAT('      SUPPOSE IT IS DESIRED TO DRAW A LOGICAL AND ')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1136)
 1136     FORMAT('      WITH THE MIDDLE OF THE FLAT SIDE  ',   &
                 'AT THE POINT 20 20 ')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1137)
 1137     FORMAT('      AND WITH THE MIDDLE OF ROUNDED SIDE AT 40 60')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1141)
 1141     FORMAT('      THEN THE ALLOWABLE FORMS ARE--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1142)
 1142     FORMAT('            AND 20 20 40 60 ')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1143)
 1143     FORMAT('      AND ABSOLUTE 20 20 40 60 ')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1145)
 1145     FORMAT('      AND RELATIVE 20 20 40 60 ')
          CALL DPWRST('XXX','BUG ')
          GO TO 9000
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
        IF(UNITSW.EQ.'DATA')CALL DPCODS('X',X2,X2,   &
           IBUGD2,ISUBRO,IERROR)
        IF(ITYPEO.EQ.'RELA')X2=X1+X2
        J=J+1
        IF(J.GT.NUMARG)GO TO 1190
        Y2=ARG(J)
        IF(UNITSW.EQ.'DATA')CALL DPCODS('Y',Y2,Y2,   &
           IBUGD2,ISUBRO,IERROR)
        IF(ITYPEO.EQ.'RELA')Y2=Y1+Y2
!
        CALL DPAND2(X1,Y1,X2,Y2,PX,PY,   &
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
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'AND')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPAND--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ILOCFN,NUMNUM
 9012   FORMAT('ILOCFN,NUMNUM = ',2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)X1,Y1,X2,Y2
 9013   FORMAT('X1,Y1,X2,Y2 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)PXSTAR,PYSTAR,PXEND,PYEND
 9015   FORMAT('PXSTAR,PYSTAR,PXEND,PYEND = ',4E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9017)IFIG,IFOUND,IERROR
 9017   FORMAT('IFIG,IFOUND,IERROR = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPAND
      SUBROUTINE DPAND2(X1,Y1,X2,Y2,PX,PY,   &
                        IFIG,ILINPA,ILINCO,ILINC2,MAXLN,PLINTH,   &
                        AREGBA,IREBLI,IREBCO,IREBC2,MAXRG,PREBTH,   &
                        IREFSW,IREFCO,IREFC2,   &
                        IREPTY,IREPLI,IREPCO,IREPC2,PREPTH,PREPSP,   &
                        PTEXHE,PTEXWI,PTEXVG,PTEXHG)
!
!     PURPOSE--DRAW A LOGICAL AND (= AN AND BOX)
!              WITH THE MIDDLE OF THE FLAT SIDE
!              AT THE POINT (X1,Y1),
!              AND WITH THE MIDDLE OF THE CURVED SIDE
!              AT THE POINT (X2,Y2).
!     NOTE--THE HEIGHT OF THE BOX WILL BE EQUAL TO
!           THE ABOVE-DESCRIBED WIDTH OF THE BOX
!           (THAT IS, THE HEIGHT
!           OF THE BOX WILL BE EQUAL TO
!           THE WIDTH FROM (X1,Y1) TO (X2,Y2).
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--APRIL     1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --JANUARY   1989. MODIFY CALLS TO DPDRPL (ALAN)
!     UPDATED         --JANUARY   1989. MODIFY CALL  TO DPFIRE (ALAN)
!     UPDATED         --JUNE      2019. MOVE CREATION OF SCRATCH
!                                       STORAGE TO DPAND
!     UPDATED         --OCTOBER   2020. SUPPORT FOR RGB COLOR
!
!-----NON-COMMON VARIABLES-------------------------------------
!
      DIMENSION PX(*)
      DIMENSION PY(*)
!
      CHARACTER*4 IFIG
      CHARACTER*4 IPATT2
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
      CHARACTER*4 ICOLF
      CHARACTER*4 ICOLP
      CHARACTER*4 ICOL
      CHARACTER*4 IFLAG
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
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'AND2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPAND2--')
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
!
!               *********************************
!               **  STEP 1--                   **
!               **  DETERMINE THE COORDINATES  **
!               **  FOR THE LOGICAL AND        **
!               *********************************
!
      DELX=X2-X1
      DELY=Y2-Y1
      ALEN=0.0
      TERM=(X2-X1)**2+(Y2-Y1)**2
      IF(TERM.GT.0.0)ALEN=SQRT(TERM)
      R=ALEN/2.0
      IF(ABS(DELX).GE.0.00001)THETA=ATAN(DELY/DELX)
      IF(ABS(DELX).LT.0.00001.AND.DELY.GE.0.0)THETA=3.1415926/2.0
      IF(ABS(DELX).LT.0.00001.AND.DELY.LT.0.0)THETA=-3.1415926/2.0
!
      K=0
!
      X=R
      Y=-R
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
!
      DO 5110 I=1,181,5
        PHI2=I-91
        PHI2=PHI2*(2.0*3.1415926)/360.0
        X=R*COS(PHI2)+R
        Y=R*SIN(PHI2)
        CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
        K=K+1
        PX(K)=XP
        PY(K)=YP
 5110 CONTINUE
!
      X=0
      Y=R
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
!
      X=0
      Y=-R
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
!
      X=R
      Y=-R
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
      IF(IREFSW(1).EQ.'OFF')GO TO 2190
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
      CALL DPFIRE(PX,PY,NP,   &
                  IFIG,IPATT,PTHICK,PXGAP,PYGAP,   &
                  ICOLF,ICOLFR,ICOLFG,ICOLFB,   &
                  ICOLP,ICOLPR,ICOLPG,ICOLPB,   &
                  IPATT2)
 2190 CONTINUE
!
!               *********************************
!               **  STEP 3--                   **
!               **  DRAW OUT THE FIGURE  AND   **
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
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'AND2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPAND2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NP,IERRG4
 9014   FORMAT('NP,IERRG4 = ',I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,NP
          WRITE(ICOUT,9016)I,PX(I),PY(I)
 9016     FORMAT('I,PX(I),PY(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPAND2
      SUBROUTINE DPANDR(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,PANINC,   &
                        IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--GENERATE AN ANDREWS PLOT--
!              A MULTIVARIATE TECHNICQUE WHICH PLOTS THE FOLLOWING
!              TRANSFORMATION--
!                Fi(T) = X1/SQRT(2) + X2*SIN(T) + X3*COS(T) +
!                        X4*SIN(2*T) + X5*COS(2T) + ...
!              ONE CURVE IS GENERATED FOR EACH ROW OF DATA (THE NUMBER
!              OF VARIABLES DOES NOT AFFECT THE NUMBER OF CURVES
!              GENERATED).
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--92/11
!     ORIGINAL VERSION--NOVEMBER  1992.
!     UPDATED         --MARCH     2009. USE DPPARS
!     UPDATED         --JULY      2019. TWEAK USE OF SCRATCH ARRAYS
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
      CHARACTER*4 ICASE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
!
!  MAXAND IS THE MAXIMUM NUMBER OF VARIABLES TO USE IN CREATING THE
!  ANDREWS CURVE
!
!CCCC PARAMETER(MAXAND=20)
!CCCC PARAMETER(MAXAND=40)
      PARAMETER(MAXAND=35)
!
      CHARACTER*40 INAME
      CHARACTER*4 IVARN1(MAXAND)
      CHARACTER*4 IVARN2(MAXAND)
      CHARACTER*4 IVARTY(MAXAND)
      DIMENSION PVAR(MAXAND)
      DIMENSION ILIS(MAXAND)
      DIMENSION NRIGHT(MAXAND)
      DIMENSION ICOLR(MAXAND)
      DIMENSION Z(MAXOBV,MAXAND)
      DIMENSION XJUNK1(1)
      DIMENSION XJUNK2(1)
      EQUIVALENCE (GARBAG(IGARB1),Z(1,1))
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
!
      ISUBN1='DPAN'
      ISUBN2='DR  '
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
!               ***********************************
!               **  TREAT THE ANDREWS PLOT CASE  **
!               ***********************************
!
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ANDR')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPANDR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGG2,IBUGG3,IBUGQ,ISUBRO
   52   FORMAT('IBUGG2,IBUGG3,IBUGQ,ISUBRO = ',A4,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,IAND1,IAND2
   53   FORMAT('ICASPL,IAND1,IAND2 = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ***************************
!               **  STEP 1--             **
!               **  EXTRACT THE COMMAND  **
!               ***************************
!
      ISTEPN='11'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ANDR')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICASPL='ANDR'
!
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PLOT')THEN
        ILASTC=1
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      ENDIF
      IFOUND='YES'
!
!               *********************************
!               **  STEP 2--                   **
!               **  EXTRACT THE VARIABLE LIST  **
!               *********************************
!
      INAME='ANDREWS PLOT'
      MINNA=1
      MAXNA=100
      MINN2=2
      IFLAGE=1
      IFLAGM=1
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
!
      CALL DPPARS(IHARG,IHARG2,IARGT,ARG,NUMARG,IANS,IWIDTH,   &
                  IHNAME,IHNAM2,IUSE,NUMNAM,IN,IVALUE,VALUE,   &
                  JMIN,JMAX,   &
                  MINN2,MINNA,MAXNA,MAXAND,IFLAGE,INAME,   &
                  IVARN1,IVARN2,IVARTY,PVAR,   &
                  ILIS,NRIGHT,ICOLR,ISUB,NQ,ILOCQ,NUMVAR,   &
                  MINNA,MAXAND,   &
                  IFLAGM,IFLAGP,   &
                  IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               ***************************************************
!               **  STEP 3--                                     **
!               **  FOR EACH OF THE RESPONSE VARIABLES, EXTRACT  **
!               **  THE DATA SUBSET.                             **
!               ***************************************************
!
      ISTEPN='3'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ANDR')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 300 K=1,NUMVAR
!
!       DECEMBER 2010: USE DPPAR3.  THIS ALLOWS THE VARIABLES
!       TO BE MATRICES AS WELL AS VARIABLES.
!
        ICOL=K
        NUMVA2=1
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                    INAME,IVARN1,IVARN2,IVARTY,   &
                    ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                    MAXCP4,MAXCP5,MAXCP6,   &
                    V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                    Z(1,K),XJUNK1,XJUNK2,NLOCAL,NLOCA2,NLOCA3,ICASE,   &
                    IBUGG3,ISUBRO,IFOUND,IERROR)
!
        IF(K.EQ.1)THEN
          NSAVE=NLOCAL
        ELSE
          IF(NLOCAL.NE.NSAVE)THEN
            WRITE(ICOUT,301)
  301       FORMAT('****** ERROR IN ANDREWS PLOT--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,303)IVARN1(K),IVARN2(K)
  303       FORMAT('       VARIABLE ',A4,A4,' DOES NOT HAVE THE ',   &
                   'EXPECTED NUMBER OF OBSERVATIONS.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,305)NLOCAL
  305       FORMAT('       NUMBER OF OBSERVATIONS          = ',I8)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,307)NSAVE
  307       FORMAT('       NUMBER OF OBSERVATIONS EXPECTED = ',I8)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
        ENDIF
!
  300 CONTINUE
      NZ=NUMVAR
!
!               *******************************************************
!               **  STEP 4--                                         **
!               **  FORM THE VERTICAL AND HORIZONTAL AXIS            **
!               **  VALUES Y(.) AND X(.) FOR THE PLOT.               **
!               **  DEFINE THE VECTOR D(.) SO THAT EACH ANDREW'S     **
!               **  CURVE HAS ITS OWNS TAG NUMBER.                   **
!               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).    **
!               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).    **
!               *******************************************************
!
      ISTEPN='4'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ANDR')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPANR2(Z,NZ,ICASPL,PANINC,   &
                  NLOCAL,MAXOBV,MAXAND,MAXPOP,   &
                  Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ANDR')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPANDR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IFOUND,IERROR,NZ
 9013   FORMAT('IFOUND,IERROR,NZ = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9014   FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',   &
               I8,I8,I8,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        IF(NZ.GE.1)THEN
          DO 9022 I=1,NZ
            WRITE(ICOUT,9023)I,(Z(I,K),K=1,NUMVAR)
 9023       FORMAT('I,Z(I,K) = ',I8,20E15.7)
            CALL DPWRST('XXX','BUG ')
 9022     CONTINUE
        ENDIF
        IF(NPLOTP.GE.1)THEN
          DO 9052 I=1,NPLOTP
            WRITE(ICOUT,9053)I,Y(I),X(I),D(I)
 9053       FORMAT('I,Y(I),X(I),D(I) = ',I8,3F12.5)
            CALL DPWRST('XXX','BUG ')
 9052    CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPANDR
      SUBROUTINE DPANR2(Z,NZ,ICASPL,PANINC,   &
      NOBS,MAXOBV,MAXAND,MAXPOP,   &
      Y2,X2,D2,N2,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
!     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
!              THAT WILL DEFINE
!              A ANDREWS PLOT
!              (USEFUL FOR MULTIVARIATE ANALYSIS).
!     WRITTEN BY--ALAN HECKERT
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--92/11
!     ORIGINAL VERSION--NOVEMBER  1992.
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
      DIMENSION Z(MAXOBV,MAXAND)
!
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPAN'
      ISUBN2='R2  '
      IERROR='NO'
!
      PI=3.1415926
      NINC=INT(2.0*PI/PANINC+0.5)
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      IF(NZ.GE.1)GO TO 39
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,31)
   31 FORMAT('***** ERROR IN DPANR2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,32)
   32 FORMAT('      THE NUMBER OF OBSERVATIONS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,33)
   33 FORMAT('      MUST BE AT LEAST 1;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,34)NZ
   34 FORMAT('      THE ENTERED NUMBER OF OBSERVATIONS HERE = ',I6)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
   39 CONTINUE
!
      IF(IBUGG3.EQ.'OFF'.AND.ISUBRO.NE.'ANR2')GO TO 90
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,71)
   71 FORMAT('***** AT THE BEGINNING OF DPANR2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,72)ICASPL,NZ,NOBS,NPLOTV
   72 FORMAT('ICASPL,NZ,NOBS,NPLOTV = ',A4,2X,3I8)
      CALL DPWRST('XXX','BUG ')
      IF(NZ.LE.0)GO TO 83
      DO 81 I=1,NZ
      WRITE(ICOUT,82)I,(Z(I,K),K=1,NZ)
   82 FORMAT('I,Z(I,K) = ',I8,20E12.5)
      CALL DPWRST('XXX','BUG ')
   81 CONTINUE
   83 CONTINUE
   90 CONTINUE
!
!               ****************************************
!               **  STEP 11--                         **
!               **  DETERMINE PLOT COORDINATES        **
!               ****************************************
!
      ICOUNT=0
      NTEMP=NZ-1
      IF(MOD(NTEMP,2).EQ.0)THEN
        NSIN=NTEMP/2
        NCOS=NSIN
      ELSE
        NSIN=NTEMP/2
        NCOS=NSIN
        NSIN=NSIN+1
      ENDIF
!
      DO 100 ICASE=1,NOBS
        TVALUE=-PI
        DO 200 J=1,NINC
          ICOUNT=ICOUNT+1
          IF(ICOUNT.GT.MAXPOP)THEN
            WRITE(ICOUT,201)
 201  FORMAT(1X,'ERROR IN DPANR2.  MAXIMUM NUMBER OF PLOT POINTS')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,202)
 202  FORMAT(1X,'WAS EXCEEDED.')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
          X2(ICOUNT)=TVALUE
          D2(ICOUNT)=REAL(ICASE)
          Y2(ICOUNT)=Z(ICASE,1)/SQRT(2.0)
          IF(NSIN.GE.1)THEN
            DO 300 K=1,NSIN
              INDX=2+(K-1)*2
              Y2(ICOUNT)=Y2(ICOUNT)+Z(ICASE,INDX)*SIN(K*TVALUE)
 300        CONTINUE
          ENDIF
          IF(NCOS.GE.1)THEN
            DO 400 K=1,NCOS
              INDX=3+(K-1)*2
              Y2(ICOUNT)=Y2(ICOUNT)+Z(ICASE,INDX)*COS(K*TVALUE)
 400        CONTINUE
          ENDIF
          TVALUE=TVALUE+PANINC
 200    CONTINUE
 100  CONTINUE
!
      N2=ICOUNT
      NPLOTV=2
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGG3.EQ.'OFF'.AND.ISUBRO.NE.'ANR2')GO TO 9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPANR2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)ICASPL,NZ,N2,IERROR
 9012 FORMAT('ICASPL,NZ,N2,IERROR = ',A4,2I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9031)N2,NPLOTV
 9031 FORMAT('N2,NPLOTV = ',2I8)
      CALL DPWRST('XXX','BUG ')
      DO 9035 I=1,N2
      WRITE(ICOUT,9036)I,Y2(I),X2(I),D2(I)
 9036 FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,2E15.7,F9.2)
      CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPANR2
      SUBROUTINE DPANIN(IHARG,IARGT,ARG,NUMARG,DEFAIN,   &
      ANDINC,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE ANDREWS INCREMENT
!              THIS DEFINES THE RESOLUTION ALONG THE X AXIS
!              FOR ANDREWS PLOTS.
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --IARGT  (A  HOLLERITH VECTOR)
!                     --ARG    (A  FLOATING POINT VECTOR)
!                     --NUMARG (AN INTEGER VARIABLE)
!                     --DEFAIN (A  FLOATING POINT VARIABLE)
!     OUTPUT ARGUMENTS--ANDINC  (A  FLOATING POINT VARIABLE)
!                     --IFOUND ('YES' OR 'NO' )
!                     --IERROR ('YES' OR 'NO' )
!     WRITTEN BY-ALAN HECKERT
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--NOVEMBER 1992.
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
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'INCR')GO TO 1110
      GO TO 1199
!
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1150
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      IF(IARGT(NUMARG).EQ.'NUMB')GO TO 1160
      GO TO 1120
!
 1120 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1121)
 1121 FORMAT('***** ERROR IN DPANIN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR ANDREWS INCREMENT ',   &
      'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1124)
 1124 FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE ',   &
      'PROPER FORM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1131)
 1131 FORMAT('      ANDREWS INCREMENT .01')
      CALL DPWRST('XXX','BUG ')
      GO TO 1199
!
 1150 CONTINUE
      HOLD=DEFAIN
      GO TO 1180
!
 1160 CONTINUE
      HOLD=ARG(NUMARG)
      IF(HOLD.LE.0.0)HOLD=DEFAIN
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
      ANDINC=HOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)ANDINC
 1181 FORMAT('THE ANDREWS INCREMENT HAS JUST BEEN SET TO ',   &
      E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 1199
!
 1199 CONTINUE
      RETURN
      END SUBROUTINE DPANIN
      SUBROUTINE DPANGL(IHARG,IARGT,ARG,NUMARG,   &
      IATXSW,   &
      ADEFAN,IDEFDI,   &
      ATEXAN,ITEXDI,   &
      IBUGD2,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE ANGLE AT WHICH OR TEXT IS TO
!              BE PRINTED (AS, FOR EXAMPLE, IN DIAGRAMS).
!              THE SPECIFIED ANGLE VALUE WILL BE PLACED
!              IN THE FLOATING POINT VARIABLE ATEXAN.
!     CAUTION--IATXSW IS BOTH AN INPUT AND OUTPUT ARGUMENT
!              TO THIS SUBROUTINE--IT MAY BE CHANGED HEREIN.
!     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
!                     --IARGT  (A  CHARACTER VECTOR)
!                     --ARG    (A  FLOATING POINT VECTOR)
!                     --NUMARG (AN INTEGER VARIABLE)
!                     --IATXSW (A  CHARACTER VARIABLE)
!                     --ADEFAN (A  FLOATING POINT VARIABLE)
!                     --IDEFDI (A  CHARACTER VARIABLE)
!     OUTPUT ARGUMENTS--ATEXAN  (A  FLOATING POINT VARIABLE)
!                     --ITEXDI (A CHARACTER VARIABLE)
!                     --IATXSW (A CHARACTER VARIABLE)
!                     --IFOUND ('YES' OR 'NO' )
!                     --IBUGD2
!                     --IERROR ('YES' OR 'NO' )
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--NOVEMBER 1980.
!     UPDATED         --MAY       1982.
!     UPDATED         --JUNE      1992. SET ITEXDI
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IARGT
      CHARACTER*4 IATXSW
      CHARACTER*4 IDEFDI
      CHARACTER*4 ITEXDI
      CHARACTER*4 IBUGD2
      CHARACTER*4 ISUBRO
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
      IF(IBUGD2.EQ.'OFF')GO TO 90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPANGL--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IATXSW,ADEFAN,IDEFDI
   53 FORMAT('IATXSW,ADEFAN,IDEFDI = ',A4,2X,E15.7,2X,A4)
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
      IF(IHARG(1).EQ.'UNIT')GO TO 9000
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1150
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      IF(IHARG(NUMARG).EQ.'?')GO TO 8100
      IF(IARGT(NUMARG).EQ.'NUMB')GO TO 1160
!
      IF(IHARG(NUMARG).EQ.'RADI')GO TO 1140
      IF(IHARG(NUMARG).EQ.'DEGR')GO TO 1140
      IF(IHARG(NUMARG).EQ.'GRAD')GO TO 1140
      GO TO 1120
!
 1120 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,1121)
 1121 FORMAT('***** ERROR IN DPANGL--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR ANGLE ',   &
      'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1124)
 1124 FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE ',   &
      'PROPER FORM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1125)
 1125 FORMAT('      SUPPOSE THE ANALYST DESIRES THE  ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1126)
 1126 FORMAT('      ANGLE UNITS TO BE MEASURED IN DEGREES ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1127)
 1127 FORMAT('      AND WISHES TO HAVE SUCCEEDING TEXT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1128)
 1128 FORMAT('      PRINTED OUT AT AN ANGLE OF 45 DEGREES, ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1129)
 1129 FORMAT('      THEN THE ALLOWABLE FORM IS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1130)
 1130 FORMAT('      ANGLE UNITS DEGREES ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1131)
 1131 FORMAT('      ANGLE 45 ')
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
!
 1140 CONTINUE
      IFOUND='YES'
      IATXSW=IHARG(NUMARG)
!
      IF(IFEEDB.EQ.'OFF')GO TO 1149
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1141)IATXSW
 1141 FORMAT('THE ANGLE UNITS HAVE JUST BEEN SET TO ',   &
      A4)
      CALL DPWRST('XXX','BUG ')
 1149 CONTINUE
      GO TO 9000
!
 1150 CONTINUE
      HOLD=ADEFAN
      GO TO 1180
!
 1160 CONTINUE
      HOLD=ARG(NUMARG)
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
      ATEXAN=HOLD
!
!CCCC THE FOLLOWING 15 LINES WERE ADDED JUNE 1992 (JJF)
      IF(IATXSW.EQ.'RADI')THEN
         IF(-0.1.LE.ATEXAN.AND.ATEXAN.LE.0.1)ITEXDI='HORI'
         IF(1.4.LE.ATEXAN.AND.ATEXAN.LE.1.7)ITEXDI='DIAG'
         IF(3.0.LE.ATEXAN.AND.ATEXAN.LE.3.3)ITEXDI='VERT'
      ENDIF
      IF(IATXSW.EQ.'DEGR')THEN
         IF(-1.0.LE.ATEXAN.AND.ATEXAN.LE.1.0)ITEXDI='HORI'
         IF(44.0.LE.ATEXAN.AND.ATEXAN.LE.46.0)ITEXDI='DIAG'
         IF(89.0.LE.ATEXAN.AND.ATEXAN.LE.91.0)ITEXDI='VERT'
      ENDIF
      IF(IATXSW.EQ.'GRAD')THEN
         IF(-1.0.LE.ATEXAN.AND.ATEXAN.LE.1.0)ITEXDI='HORI'
         IF(49.0.LE.ATEXAN.AND.ATEXAN.LE.51.0)ITEXDI='DIAG'
         IF(99.0.LE.ATEXAN.AND.ATEXAN.LE.101.0)ITEXDI='VERT'
      ENDIF
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IF(IATXSW.EQ.'RADI')WRITE(ICOUT,1181)ATEXAN
 1181 FORMAT('THE ANGLE HAS JUST BEEN SET TO ',   &
      E15.7,' RADIANS')
      IF(IATXSW.EQ.'RADI')CALL DPWRST('XXX','BUG ')
      IF(IATXSW.EQ.'DEGR')WRITE(ICOUT,1182)ATEXAN
 1182 FORMAT('THE ANGLE HAS JUST BEEN SET TO ',   &
      E15.7,' DEGREES')
      IF(IATXSW.EQ.'DEGR')CALL DPWRST('XXX','BUG ')
      IF(IATXSW.EQ.'GRAD')WRITE(ICOUT,1183)ATEXAN
 1183 FORMAT('THE ANGLE HAS JUST BEEN SET TO ',   &
      E15.7,' GRADS')
      IF(IATXSW.EQ.'GRAD')CALL DPWRST('XXX','BUG ')
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
      WRITE(ICOUT,8111)ATEXAN
 8111 FORMAT('THE CURRENT (TEXT) ANGLE  IS ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8112)ADEFAN
 8112 FORMAT('THE DEFAULT (TEXT) ANGLE  IS ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8121)IATXSW
 8121 FORMAT('THE CURRENT (TEXT) ANGLE UNITS  IS ',A4)
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
 9011 FORMAT('***** AT THE END       OF DPANGL--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,ISUBRO,IFOUND,IERROR
 9012 FORMAT('IBUGD2,ISUBRO,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IATXSW,ADEFAN,IDEFDI
 9013 FORMAT('IATXSW,ADEFAN,IDEFDI = ',A4,2X,E15.7,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)ATEXAN,ITEXDI
 9014 FORMAT('ATEXAN,ITEXDI = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPANGL
      SUBROUTINE DPANGU(IHARG,NUMARG,   &
      IDEFAU,   &
      IATXSW,   &
      IBUGD2,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE ANGLE UNITS IN WHICH
!              THE ANGLE FOR SCRIPT OR TEXT IS TO
!              BE PRINTED (AS, FOR EXAMPLE, IN DIAGRAMS).
!              OR IN WHICH
!              TRIGONOMETRIC CALCULATIONS ARE TO BE CARRIED OUT,
!              THE SPECIFIED ANGLE UNITS WILL BE PLACED
!              IN THE CHARACTER VARIABLE IATXSW.
!     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
!                     --NUMARG (AN INTEGER VARIABLE)
!                     --IDEFAU (A  CHARACTER VARIABLE)
!     OUTPUT ARGUMENTS--IATXSW (A CHARACTER VARIABLE)
!                     --IFOUND ('YES' OR 'NO' )
!                     --IBUGD2
!                     --IERROR ('YES' OR 'NO' )
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
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
      CHARACTER*4 IDEFAU
      CHARACTER*4 IATXSW
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
!---------------------------------------------------------------------
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
   51 FORMAT('***** AT THE BEGINNING OF DPANGU--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IDEFAU
   53 FORMAT('IDEFAU = ',A4)
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
      IF(NUMARG.LE.0)GO TO 9000
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'UNIT')GO TO 1110
      GO TO 9000
!
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'UNIT')GO TO 1150
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1150
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      IF(IHARG(NUMARG).EQ.'?')GO TO 8100
!
      IF(IHARG(NUMARG).EQ.'RADI')GO TO 1160
      IF(IHARG(NUMARG).EQ.'DEGR')GO TO 1160
      IF(IHARG(NUMARG).EQ.'GRAD')GO TO 1160
      GO TO 1120
!
 1120 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,1121)
 1121 FORMAT('***** ERROR IN DPANGU--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR ANGLE UNITS ',   &
      'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1124)
 1124 FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE ',   &
      'PROPER FORM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1125)
 1125 FORMAT('      SUPPOSE THE ANALYST DESIRES THE  ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1126)
 1126 FORMAT('      ANGLE UNITS TO BE MEASURED IN DEGREES ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1129)
 1129 FORMAT('      THEN THE ALLOWABLE FORM IS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1130)
 1130 FORMAT('      ANGLE UNITS DEGREES ')
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
!
 1150 CONTINUE
      IHOLD=IDEFAU
      GO TO 1180
!
 1160 CONTINUE
      IHOLD=IHARG(NUMARG)
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
      IATXSW=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)IATXSW
 1181 FORMAT('THE ANGLE UNITS HAVE JUST BEEN SET TO ',   &
      A4)
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
      WRITE(ICOUT,8111)IATXSW
 8111 FORMAT('THE CURRENT (TEXT) ANGLE UNITS IS ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8112)IDEFAU
 8112 FORMAT('THE DEFAULT (TEXT) ANGLE UNITS IS ',A4)
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
 9011 FORMAT('***** AT THE END       OF DPANGU--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,ISUBRO,IFOUND,IERROR
 9012 FORMAT('IBUGD2,ISUBRO,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IDEFAU,IATXSW
 9013 FORMAT('IDEFAU,IATXSW = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPANGU
      SUBROUTINE DPANIM(IHARG,NUMARG,IANISW,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE ANIMATION SWITCH IANISW.
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --NUMARG
!     OUTPUT ARGUMENTS--IANISW  ('ON'  OR 'OFF')
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
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--APRIL     1989.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IANISW
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
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
      IFOUND='NO'
      IERROR='NO'
!
      IF(NUMARG.EQ.0)THEN
        IANISW='ON'
      ELSEIF(IHARG(NUMARG).EQ.'ON' .OR. IHARG(NUMARG).EQ.'AUTO')THEN
        IANISW='ON'
      ELSEIF(IHARG(NUMARG).EQ.'OFF' .OR. IHARG(NUMARG).EQ.'DEFA')THEN
        IANISW='OFF'
      ELSE
        GO TO 1199
      ENDIF
!
      IFOUND='YES'
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1181)IANISW
 1181   FORMAT('THE ANIMATION SWITCH HAS JUST BEEN TURNED ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
 1199 CONTINUE
      RETURN
      END SUBROUTINE DPANIM
      SUBROUTINE DPANO2(Y,F1,W,N,NUMFAC,IVARID,IVARI2,               &
                        F1ID,F1N,F1MEAN,F1EFFE,F1EFSD,               &
                        CELLME,CELLV,CELLN,                          &
                        AINN,AINMEA,AINEFF,AINESD,                   &
                        MAXOBV,MAXLEV,MAXFAC,                        &
                        N1,ISET,AN1,E1,SS1,RESMS1,FVAL,F1CDF2,RSD,   &
                        B,SDB,FCUM,REPSD,REPDF,RESSD,RESDF,          &
                        PRED2,RES2,ALFCDF,                           &
                        Z,                                           &
                        ICAPSW,ICAPTY,IFORSW,IAOV2T,IAOVTY,          &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--DO A MULTI-WAY ANOVA
!              FOR 1, 2, 3, 4, OR 5 FACTORS.
!              THE ASSUMED MODEL IS RESPONSE = CONSTANT + FACTOR-1 EFFECT + ...
!                                              FACTOR-NUMFAC EFFECT + ERROR
!     NOTE-- LINES NEAR 390 NEEDS TO BE GENERALIZED FOR
!            UNEQUAL NUMBER OF OBS PER CELL.
!     PRINTING--YES
!     SUBROUTINES NEEDED--FCDF
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--APRIL     1978.
!     UPDATED         --NOVEMBER  1978.
!     UPDATED         --JULY      1979.
!     UPDATED         --FEBRUARY  1981.
!     UPDATED         --JULY      1981.
!     UPDATED         --OCTOBER   1981.
!     UPDATED         --MARCH     1982.
!     UPDATED         --MAY       1982.
!     UPDATED         --MARCH     1988.      ADD LOFCDF
!     UPDATED         --AUGUST    1988. CHANGED DIMENSIONS 100 TO 500
!     UPDATED         --JUNE      1989. 0-TRAP WHEN IRESDF = 0
!     UPDATED         --JUNE      1990. DIMENSION Z IN DPANOV
!     UPDATED         --MAY       1995. EQUIVALENCE FOR MACINTOSH
!     UPDATED         --JANUARY   1996. MAKE MAXIMUM NUMBER OF LEVELS
!                                       SETTABLE VIA PARAMETER
!                                       STATEMENT (AND PUT IN CHECKS
!                                       FOR EXCEEDING THIS MAXIMUM)
!     UPDATED         --FEBRUARY  1997. BUG FIX AT STEP 8
!     UPDATED         --JANUARY   1998. SIMPLIFY CODE, MAJOR REWRITE
!     UPDATED         --APRIL     1999. BUG FIX, MOVE 11690 CONTINUE
!     UPDATED         --JUNE      2002. RESSD FOR MODEL TO DPST3F.DAT
!     UPDATED         --NOVEMBER  2003. SUPPORT FOR HTML, LATEX OUTPUT
!     UPDATED         --MAY       2011. USE DPDTA1, DPDT5B TO PRINT
!                                       TABLES
!     UPDATED         --MAY       2011. USE DPAUFI TO OPEN/CLOSE
!                                       AUXILLARY FILES
!     UPDATED         --JUNE      2018. PRINT VARIABLE NAMES IN OUTPUT
!     UPDATED         --NOVEMBER  2023. SUPPORT FOR 2-TERM INTERACTIONS
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 IAOV2T
      CHARACTER*4 IAOVTY
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
      CHARACTER*4 IVARID(*)
      CHARACTER*4 IVARI2(*)
!
      CHARACTER*4 IREP
!CCCC ADD FOLLOWING LINE 4/98
      CHARACTER*4 ICASBL
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      CHARACTER*4 IOP
      CHARACTER*4 I2TERM
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION W(*)
      DIMENSION F1(MAXOBV,MAXFAC)
!
      DIMENSION F1ID(MAXLEV,MAXFAC)
      DIMENSION F1N(MAXLEV,MAXFAC)
      DIMENSION F1MEAN(MAXLEV,MAXFAC)
      DIMENSION F1EFFE(MAXLEV,MAXFAC)
      DIMENSION F1EFSD(MAXLEV,MAXFAC)
!
      DIMENSION B(*)
      DIMENSION SDB(*)
      DIMENSION FCUM(*)
      DIMENSION PRED2(*)
      DIMENSION RES2(*)
!
      DIMENSION N1(*)
      DIMENSION ISET(*)
      DIMENSION AN1(*)
      DIMENSION E1(*)
!
      DIMENSION SS1(*)
      DIMENSION RESMS1(*)
      DIMENSION FVAL(*)
      DIMENSION F1CDF2(*)
      DIMENSION RSD(*)
      DIMENSION Z(*)
!
      DIMENSION CELLN(*)
      DIMENSION CELLME(*)
      DIMENSION CELLV(*)
      DIMENSION AINN(*)
      DIMENSION AINMEA(*)
      DIMENSION AINEFF(*)
      DIMENSION AINESD(*)
!
      DIMENSION SS2TIN(50)
      REAL      MSEIN(50)
      DIMENSION IDFIN(50)
!
      PARAMETER(NUMCLI=7)
      PARAMETER(MAXLIN=2)
      PARAMETER (MAXROW=60)
      CHARACTER*60 ITITLE
      CHARACTER*60 ITITLZ
      CHARACTER*60 ITITL9
      CHARACTER*60 ITEXT(MAXROW)
      CHARACTER*4  ALIGN(NUMCLI)
      CHARACTER*4  VALIGN(NUMCLI)
      REAL         AVALUE(MAXROW)
      INTEGER      NCTEXT(MAXROW)
      INTEGER      IDIGIT(MAXROW)
      INTEGER      IDIGI2(MAXROW,NUMCLI)
      INTEGER      NTOT(MAXROW)
      INTEGER      ROWSEP(MAXROW)
      CHARACTER*60 ITITL2(MAXLIN,NUMCLI)
      CHARACTER*30 IVALUE(MAXROW,NUMCLI)
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
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
      ISUBN1='DPAN'
      ISUBN2='O2  '
!
      FITCD2=CPUMIN
      AN=N
      J1=0
      NIOLD=0
      NUMINT=0
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ANO2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPANO2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,IAOV2T,IAOVTY,N,NUMFAC
   52   FORMAT('IBUGA3,ISUBRO,IAOV2T,IAOVTY,N,NUMFAC = ',4(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,Y(I),(F1(I,J),J=1,MIN(NUMFAC,5)),W(I)
   56     FORMAT('I,Y(I),(F1(I,J),J=1,MIN(NUMFAC,5)) = ',I8,7E11.4)
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
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR IN ANOVA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)
  102   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE ANOVA')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)
  103   FORMAT('      COMMAND MUST BE AT LEAST 2;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,104)N
  104   FORMAT('      THE ENTERED NUMBER OF OBSERVATIONS HERE = ',I6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(NUMFAC.LT.1.OR.NUMFAC.GT.MAXFAC)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,132)
  132   FORMAT('      THE NUMBER OF FACTORS FOR THE ANOVA')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,133)MAXFAC
  133   FORMAT('      MUST BE AT LEAST 1 AND AT MOST ',I6,';')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,134)NUMFAC
  134   FORMAT('      THE ENTERED NUMBER OF FACTORS HERE = ',I6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      HOLD=Y(1)
      DO 140 I=1,N
      IF(Y(I).NE.HOLD)GO TO 149
  140 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,101)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,142)
  142 FORMAT('      ALL RESPONSE VARIABLE ELEMENTS FOR THE ANOVA')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,143)HOLD
  143 FORMAT('      ARE IDENTICALLY EQUAL TO ',G15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
  149 CONTINUE
!
      DO 150 J=1,NUMFAC
        HOLD=F1(1,J)
        DO 155 I=1,N
          HOLD2=F1(I,J)
          IF(HOLD2.NE.HOLD)GO TO 150
  155   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,152)J
  152   FORMAT('      ALL ELEMENTS OF FACTOR ',I5,' IN THE ANOVA')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,153)HOLD
  153   FORMAT('      ARE IDENTICALLY EQUAL TO ',E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
  150 CONTINUE
!
      I2TERM=IAOV2T
      IF(NUMFAC.LT.2)I2TERM='NO'
!
!               ***********************************************
!               **  STEP 1.1--                               **
!               **  DETERMINE THE NUMBER OF DISTINCT VALUES  **
!               **  FOR EACH FACTOR                          **
!               ***********************************************
!
      ISTEPN='1.1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ANO2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!CCCC FOLLOWING INITIALIZATION NEEDED FOR LAHEY COMPILER.  OCTOBER 1998
      DO 1159 I=1,MAXFAC
        N1(I)=0
 1159 CONTINUE
!
      DO 1160 K=1,NUMFAC
        N1(K)=0
        DO 160 I=1,N
          IF(N1(K).LE.0)GO TO 180
          DO 170 J=1,N1(K)
            IF(F1(I,K).EQ.F1ID(J,K))GO TO 160
  170     CONTINUE
  180     CONTINUE
          N1(K)=N1(K)+1
          IF(N1(K).GT.MAXLEV)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,101)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,190)MAXLEV,K
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
  190     FORMAT('      THE MAXIMUM NUMBER OF LEVELS, ',I10,   &
                 ' EXCEEDED FOR FACTOR ',I5)
          F1ID(N1(K),K)=F1(I,K)
  160   CONTINUE
        IF(N1(K).LE.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,165)K
  165     FORMAT('      N = 0 FOR FACTOR ',I5)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
        AN1(K)=REAL(N1(K))
 1160 CONTINUE
!
!               **************************************
!               **  STEP 2--                        **
!               **  SORT THE LEVELS OF EACH FACTOR  **
!               **  SO AS TO PUT THEM IN ORDER FOR  **
!               **  PRESENTATION PURPOSES.          **
!               **************************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ANO2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 1900 K=1,NUMFAC
        CALL SORT(F1ID(1,K),N1(K),F1ID(1,K))
 1900 CONTINUE
!
!               *************************************************
!               **  STEP 22--                                  **
!               **  WRITE INFO TO FILES DPST1F.DAT, DPST2F.DAT **
!               *************************************************
!
      ISTEPN='22'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ANO2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IOP='OPEN'
      IFLAG1=1
      IFLAG2=1
      IFLAG3=1
      IFLAG4=1
      IFLAG5=0
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                  IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               ********************************************
!               **  STEP 3--                              **
!               **  DETERMINE IF HAVE                     **
!               **  REPLICATION WITHIN CELLS.             **
!               **  IF SO, COMPUTE (FOR EACH CELL)--      **
!               **         1) NUMBER OF OBSERVATIONS;     **
!               **         2) MEAN;                       **
!               **         3) SUM OF SQUARED DEVIATIONS.  **
!               **  NOTE: THIS SECTION NEEDS TO BE        **
!               **        IF MAXIMUM NUMBER OF FACTORS IS **
!               **        UPGRADED (I.E., MAXFAC)         **
!               ********************************************
!
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ANO2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IREP='NO'
      IREPDF=0
      REPDF=0.0
      REPSS=0.0
      REPSD=0.0
!
      ISTEPN='3.5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ANO2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      WRITE(IOUNI4,3501)
 3501 FORMAT('CELL COUNT     CELL MEAN      CELL SD        ',   &
             'FACTOR LEVELS ...')
      K=0
      ICASBL='YES'
      DO 3510 ISET1=1,N1(1)
        ISET(1)=ISET1
        DO 3520 ISET2=1,MAX(1,N1(2))
        ISET(2)=ISET2
        DO 3530 ISET3=1,MAX(1,N1(3))
        ISET(3)=ISET3
        DO 3540 ISET4=1,MAX(1,N1(4))
        ISET(4)=ISET4
        DO 3550 ISET5=1,MAX(1,N1(5))
        ISET(5)=ISET5
        DO 3563 ISET6=1,MAX(1,N1(6))
        ISET(6)=ISET6
        DO 3573 ISET7=1,MAX(1,N1(7))
        ISET(7)=ISET7
        DO 3583 ISET8=1,MAX(1,N1(8))
        ISET(8)=ISET8
        DO 3593 ISET9=1,MAX(1,N1(9))
        ISET(9)=ISET9
        DO 3598 ISET10=1,MAX(1,N1(10))
        ISET(10)=ISET10
!
          IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ANO2')THEN
            ISTEPN='3.5B'
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,3511)ISET1,ISET2,ISET3,ISET4,ISET5
 3511       FORMAT('ISET1,ISET2,ISET3,ISET4,ISET5=',5I5)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3512)ISET6,ISET7,ISET8,ISET9,ISET10
 3512       FORMAT('ISET6,ISET7,ISET8,ISET9,ISET10=',5I5)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
          K=K+1
          CELLN(K)=0.0
          CELLME(K)=0.0
!
          NI=0
          DO 3560 I=1,N
            DO 3565 L=1,NUMFAC
              IF(F1(I,L).NE.F1ID(ISET(L),L))GO TO 3560
 3565       CONTINUE
            NI=NI+1
            Z(NI)=Y(I)
 3560     CONTINUE
!
          CELLN(K)=REAL(NI)
          IF(NI.LE.0)GO TO 3590
          IF(NI.EQ.1)THEN
            CELLME(K)=Z(NI)
            GO TO 3590
          ENDIF
          IREP='YES'
          SUM=0.0
          DO 3570 I=1,NI
            SUM=SUM+Z(I)
 3570     CONTINUE
          CELLME(K)=SUM/CELLN(K)
!
          IF(K.EQ.1)NIOLD=NI
          IF(NI.NE.NIOLD.AND.ICASBL.EQ.'YES')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3571)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            ICASBL='NO'
          ENDIF
          NIOLD=NI
          SUM=0.0
          DO 3580 I=1,NI
            SUM=SUM+(Z(I)-CELLME(K))**2
 3580     CONTINUE
          CELLV(K)=SUM/(CELLN(K)-1.0)
!
          WRITE(IOUNI4,'(3E15.7,10I5)')CELLN(K),CELLME(K),CELLV(K),   &
                                   (ISET(LL),LL=1,NUMFAC)
!
          REPSS=REPSS+SUM
          IREPDF=IREPDF+NI-1
 3590     CONTINUE
 3571     FORMAT('WARNING: UNBALANCED CASE DETECTED.  SOME ',   &
             'COMPUTATIONS MAY NOT BE ACCURATE.')
 3598   CONTINUE
 3593   CONTINUE
 3583   CONTINUE
 3573   CONTINUE
 3563   CONTINUE
 3550   CONTINUE
 3540   CONTINUE
 3530   CONTINUE
 3520   CONTINUE
 3510 CONTINUE
      GO TO 3900
!
 3900 CONTINUE
      NUMCEL=K
      IF(IREP.EQ.'YES')THEN
        REPDF=IREPDF
        REPMS=REPSS/REPDF
        IF(REPMS.LE.0.0)REPSD=0.0
        IF(REPMS.GT.0.0)REPSD=SQRT(REPMS)
      ENDIF
!
      IF(ICASBL.EQ.'NO')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3901)
 3901   FORMAT('      UNBALANCED CASE DETETECTED.  THE ANOVA TABLE ',   &
               'WILL NOT BE GENERATED.')
        CALL DPWRST('XXX','BUG ')
        I2TERM='NO'
      ELSE
!
!       IF CELL COUNT IS 1, THEN DO NOT COMPUTE INTERACTION
!       TERMS
!
        IF(CELLN(1).EQ.1)I2TERM='NO'
      ENDIF
!
!               ******************************
!               **  STEP 4--                **
!               **  COMPUTE THE GRAND MEAN  **
!               ******************************
!
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ANO2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      SUM=0.0
      DO 4100 I=1,N
        SUM=SUM+Y(I)
 4100 CONTINUE
      GMEAN=SUM/AN
!
      SUM=0.0
      DO 4200 I=1,N
        SUM=SUM+(Y(I)-GMEAN)**2
 4200 CONTINUE
      GSS=SUM
      GVAR=GSS/(AN-1.0)
      IF(GVAR.LE.0.0)GSD=0.0
      IF(GVAR.GT.0.0)GSD=SQRT(GVAR)
!
!               ***********************************************
!               **  STEP 5.1--                               **
!               **  DETERMINE (FOR EACH LEVEL OF EACH FACTOR)**
!               **      1) NUMBER OF OBSERVATIONS;           **
!               **      2) MEAN;                             **
!               **      3) ESTIMATED EFFECT (COEFFICIENT)    **
!               ***********************************************
!
      ISTEPN='5.1'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 5190 K=1,NUMFAC
        DO 5100 J=1,N1(K)
          SUM1=0.0
          SUM2=0.0
          DO 5120 I=1,N
            IF(F1(I,K).EQ.F1ID(J,K))THEN
              SUM1=SUM1+1.0
              SUM2=SUM2+Y(I)
            ENDIF
 5120     CONTINUE
          F1N(J,K)=SUM1
          F1MEAN(J,K)=SUM2/SUM1
          F1EFFE(J,K)=F1MEAN(J,K)-GMEAN
 5100   CONTINUE
 5190 CONTINUE
!
!               ******************************************
!               **  STEP 6--                            **
!               **  COMPUTE THE FOLLOWING--             **
!               **     1) PREDICTED VALUES;             **
!               **     2) RESIDUALS;                    **
!               **     3) RESIDUAL STANDARD DEVIATION;  **
!               **     4) RESIDUAL DEGREES OF FREEDOM;  **
!               **  IF HAVE REPLICATION,                **
!               **  THEN ALSO CARRY OUT                 **
!               **  THE LACK OF FIT F TEST.             **
!               ******************************************
!
      ISTEPN='6'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ANO2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      RESSS=0.0
      IRESDF=0
      RESDF=0.0
      RESMS=0.0
      RESSD=0.0
      ALFCDF=(-999.99)
!
      DO 6000 I=1,N
        DO 6900 K=1,NUMFAC
          DO 6100 ISET1=1,N1(K)
            J1=ISET1
            IF(F1(I,K).EQ.F1ID(ISET1,K))GO TO 6115
 6100     CONTINUE
 6115     CONTINUE
          E1(K)=F1EFFE(J1,K)
 6900   CONTINUE
!
        PRED2(I)=GMEAN
        DO 6910 K=1,NUMFAC
         PRED2(I)=PRED2(I)+E1(K)
 6910   CONTINUE
        RES2(I)=Y(I)-PRED2(I)
 6000 CONTINUE
!
      IRESDF=N-1
      DO 6920 K=1,NUMFAC
        IRESDF=IRESDF-(N1(K)-1)
 6920 CONTINUE
      RESDF=IRESDF
      IF(IRESDF.GE.1)GO TO 6009
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,101)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6002)
 6002 FORMAT('      RESIDUAL DEGREES OF FREEDOM = 0')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6003)
 6003 FORMAT('      THE PRESCRIBED MODEL PROVIDES')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6004)
 6004 FORMAT('      AN EXACT FIT FOR THE DATA.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6005)
 6005 FORMAT('      THE NUMBER OF PARAMETERS IN THE MODEL')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6006)
 6006 FORMAT('      EQUALS THE NUMBER OF DATA POINTS.')
      CALL DPWRST('XXX','BUG ')
 6009 CONTINUE
!
      SUM=0.0
      DO 6210 I=1,N
        SUM=SUM+RES2(I)*RES2(I)
 6210 CONTINUE
      RESSS=SUM
      RESMS=0.0
      IF(IRESDF.GE.1)RESMS=RESSS/RESDF
      IF(RESMS.LE.0.0)RESSD=0.0
      IF(RESMS.GT.0.0)RESSD=SQRT(RESMS)
!
      IF(IREP.EQ.'NO')GO TO 6990
      IFITDF=IRESDF-IREPDF
      FITDF=IFITDF
      IF(IFITDF.LE.0)GO TO 6990
      IF(IREPDF.LE.0)GO TO 6990
      FITSS=RESSS-REPSS
      FITMS=FITSS/FITDF
      FITFVA=FITMS/REPMS
      CALL FCDF(FITFVA,IFITDF,IREPDF,FITCDF)
      FITCD2=100.0*FITCDF
      ALFCDF=FITCDF
 6990 CONTINUE
!
!               ************************************************
!               **  STEP 7--                                  **
!               **  COMPUTE THE ESTIMATED STANDARD DEVIATION  **
!               **  OF THE GRAND MEAN                         **
!               **  AND THE ESTIMATED STANDARD DEVIATION      **
!               **  OF THE ESTIMATED EFFECTS.                 **
!               ************************************************
!
      ISTEPN='7'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ANO2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      GMEASD=0.0
      IF(N.GT.0)GMEASD=RESSD/SQRT(AN)
!
      DO 7190 K=1,NUMFAC
        DO 7100 ISET1=1,N1(K)
          ANI=F1N(ISET1,K)
          CONST=((1.0/ANI)-(1.0/AN))
          F1EFSD(ISET1,K)=0.0
          IF(CONST.GT.0.0)F1EFSD(ISET1,K)=RESSD*SQRT(CONST)
 7100   CONTINUE
 7190 CONTINUE
!
!               ********************************
!               **  STEP 8--                  **
!               **  PERFORM THE F TEST        **
!               **  TO TEST THE SIGNIFICANCE  **
!               **  OF EACH FACTOR            **
!               ********************************
!
      ISTEPN='8'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ANO2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IRESDF.LE.0.OR.RESMS.LE.0.0)GO TO 8900
!
      DO 8190 K=1,NUMFAC
!
        SUM=0.0
        DO 8100 J=1,N1(K)
          SUM=SUM+F1N(J,K)*F1EFFE(J,K)*F1EFFE(J,K)
 8100   CONTINUE
        SS1(K)=SUM
        IDF1=N1(K)-1
        DF1=IDF1
        RESMS1(K)=SS1(K)/DF1
        IF(RESMS1(K).LE.0.0)RSD(K)=0.0
        IF(RESMS1(K).GT.0.0)RSD(K)=SQRT(RESMS1(K))
        FVAL(K)=RESMS1(K)/RESMS
        CALL FCDF(FVAL(K),IDF1,IRESDF,FCUM(K))
        F1CDF2(K)=100.0*FCUM(K)
 8190 CONTINUE
 8900 CONTINUE
!
!               *************************************************
!               **  STEP 9.1--                                 **
!               **  DETERMINE THE RESIDUAL STANDARD DEVIATION  **
!               **  FOR FACTOR K ONLY.                         **
!               *************************************************
!
      ISTEPN='9.1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ANO2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 9190 K=1,NUMFAC
        SUM=0.0
        DO 9100 I=1,N
        DO 9110 J=1,N1(K)
          J1=J
          IF(F1(I,K).EQ.F1ID(J,K))GO TO 9120
 9110   CONTINUE
 9120   CONTINUE
        WMEAN=F1MEAN(J1,K)
        SUM=SUM+(Y(I)-WMEAN)**2
 9100   CONTINUE
        WSS1=SUM
        WDF1=AN-AN1(K)
        WVAR1=WSS1/WDF1
        WSD1=0.0
        IF(WVAR1.LE.0.0)WSD1=0.0
        IF(WVAR1.GT.0.0)WSD1=SQRT(WVAR1)
        RSD(K)=WSD1
 9190 CONTINUE
!
!               ******************************************************
!               **  STEP 10--
!               **  COPY OVER INTO THE OUTPUT VECTORS B(.) AND SDB(.)--
!               **       1) THE GRAND MEAN;
!               **       2) THE ESTIMATED EFFECTS;
!               **       3) THE STANDARD DEVIATIONS OF GRAND MEAN
!               **          AND EFFECTS.
!               ******************************************************
!
      ISTEPN='10'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ANO2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      K=1
      B(K)=GMEAN
      SDB(K)=GMEASD
!
      DO 10190 L=1,NUMFAC
!
        DO 10100 ISET1=1,N1(L)
          K=K+1
          B(K)=F1EFFE(ISET1,L)
          SDB(K)=F1EFSD(ISET1,L)
10100   CONTINUE
10190 CONTINUE
!
!               ****************************
!               **  STEP 11--             **
!               **  WRITE EVERYTHING OUT  **
!               ****************************
!
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ANO2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IDEFAV=1
      SSAV=N*GMEAN
      SSTO=RESSS+SSAV
      DO 13402 L=1,NUMFAC
        SSTO=SSTO+SS1(L)
13402 CONTINUE
      IDEFTO=N
!
!     2023/11: IF 2-TERM INTERACTION EFFECTS REQUESTED, THEN
!              COMPUTE VARIOUS SUM OF SQUARES.  NOTE THAT DATAPLOT
!              COMPUTES FACTOR SUM OF SQUARES IN A SOMEWHAT DIFFERENT
!              MANNER, SO SOME REDUNDANCY MAY BE DONE HERE TO GET IN
!              A FORM NEEDED FOR INTERACTION COMPUTATIONS.
!
      IF(I2TERM.EQ.'ON' .OR. I2TERM.EQ.'YES')THEN
!
        ISTEPN='110'
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ANO2')   &
           CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        ICNT=0
        ICNT4=0
        DO 13410 KK=1,NUMFAC
          NLEVEL1=N1(KK)
          DO 13420 LL=KK+1,NUMFAC
            NLEVEL2=N1(KK)
!
            SUM2=0.0
            DO 13430 ISET1=1,NLEVEL1
              AMEAN1=F1MEAN(ISET1,KK)
              DO 13435 ISET2=1,NLEVEL2
                AMEAN2=F1MEAN(ISET2,LL)
                NTEMP=0
                SUM=0.0
                DO 13450 II=1,N
                  IF(F1(II,KK).EQ.F1ID(ISET1,KK) .AND.   &
                     F1(II,LL).EQ.F1ID(ISET2,LL))THEN
                    NTEMP=NTEMP+1
                    SUM=SUM + Y(II)
                  ENDIF
13450           CONTINUE
                ICNT4=ICNT4+1
                AINN(ICNT4)=REAL(NTEMP)
                AVAL=0.0
                IF(NTEMP.GT.0)THEN
                  AMEAN3=SUM/REAL(NTEMP)
                  AVAL=AMEAN3 - AMEAN1 - AMEAN2 + GMEAN
                  AINMEA(ICNT4)=AMEAN3
                  AINEFF(ICNT4)=AVAL
                  AVAL=AVAL**2
                  AN=REAL(N)
                  ANI=REAL(NTEMP)
                  CONST=((1.0/ANI) - (1.0/AN))
                  AINESD(ICNT4)=0.0
                  IF(CONST.GT.0.0)AINESD(ICNT4)=RESSD*SQRT(CONST)
                ELSE
                  AINMEA(ICNT4)=0.0
                  AINEFF(ICNT4)=0.0
                  AINESD(ICNT4)=0.0
                ENDIF
                SUM2=SUM2 + AVAL
!
                IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ANO2')THEN
                  WRITE(ICOUT,13455)KK,LL,ISET1,ISET2,NTEMP,SUM,SUM2
13455             FORMAT('KK,LL,ISET1,ISET2,NTEMP,SUM,SUM2 = ',   &
                         5I5,2G15.7)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,13457)AMEAN1,AMEAN2,GMEAN,AMEAN3,AVAL
13457             FORMAT('AMEAN1,AMEAN2,GMEAN,AMEAN3,AVAL = ',5G15.7)
                  CALL DPWRST('XXX','BUG ')
                ENDIF
                                                                                                                                  
13435         CONTINUE
13430       CONTINUE
            ICNT=ICNT+1
            IDFIN(ICNT)=(NLEVEL1-1)*(NLEVEL2-1)
            MSEIN(ICNT)=SUM2
            SS2TIN(ICNT)=SUM2*REAL(IDFIN(ICNT))
!
            IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ANO2')THEN
              WRITE(ICOUT,13461)LL,KK,ICNT,IDFIN(ICNT),SS2TIN(ICNT),   &
                                MSEIN(ICNT)
13461         FORMAT('LL,KK,ICNT,IDFIN(ICNT),SS2TIN(ICNT),',   &
                     'MSEIN(ICNT) = ',4I5,2G15.7)
              CALL DPWRST('XXX','BUG ')
            ENDIF
!
13420     CONTINUE
13410   CONTINUE
!
!       ADJUST SSE TO ACCOUNT FOR INTERACTIONS
!
        NUMINT=ICNT
        SUM=0.
        IDFTMP=0
        DO 13480 II=1,NUMFAC
          SUM=SUM + SS1(II)
          IDFTMP=IDFTMP + (N1(II) - 1)
13480   CONTINUE
        DO 13485 II=1,NUMINT
          SUM=SUM + SS2TIN(II)
          IDFTMP=IDFTMP + IDFIN(II)
13485   CONTINUE
!
        RESSS=GSS - SUM
        RESDF=REAL((N-1) - IDFTMP)
        IRESDF=INT(RESDF+0.1)
        RESMS=RESSS/RESDF
!
        DO 13490 K=1,NUMFAC
          IDF1=N1(K)-1
          FVAL(K)=RESMS1(K)/RESMS
          CALL FCDF(FVAL(K),IDF1,IRESDF,FCUM(K))
          F1CDF2(K)=100.0*FCUM(K)
13490   CONTINUE
        NTERM=NUMFAC
        DO 13493 K=1,NUMINT
          IDF1=IDFIN(K)
          NTERM=NTERM+1
          FVAL(NTERM)=MSEIN(K)/RESMS
          CALL FCDF(FVAL(NTERM),IDF1,IRESDF,FCUM(NTERM))
          F1CDF2(NTERM)=100.0*FCUM(NTERM)
13493   CONTINUE
!
      ENDIF
!
!     PRINT SUMMARY STATISTICS TABLE
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
      ITITLE(1:43)='  -Way Fixed Effects Analysis of Variance--'
      WRITE(ITITLE(1:2),'(I2)')NUMFAC
      IF(ICASBL.EQ.'NO')THEN
        ITITLE(44:58)='Unbalanced Case'
        NCTITL=58
      ELSE
        ITITLE(44:56)='Balanced Case'
        NCTITL=56
      ENDIF
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
      DO 13405 II=1,NUMFAC
        ICNT=ICNT+1
        ITEXT(ICNT)='Factor    Variable: '
        WRITE(ITEXT(ICNT)(8:9),'(I2)')II
        WRITE(ITEXT(ICNT)(21:24),'(A4)')IVARID(II+1)(1:4)
        WRITE(ITEXT(ICNT)(25:28),'(A4)')IVARI2(II+1)(1:4)
        NCTEXT(ICNT)=28
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
13405 CONTINUE
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
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Observations:'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=REAL(N)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Factors:'
      NCTEXT(ICNT)=18
      AVALUE(ICNT)=REAL(NUMFAC)
      IDIGIT(ICNT)=0
      DO 11102 L=1,NUMFAC
        ICNT=ICNT+1
        ITEXT(ICNT)='Number of Levels for Factor   :'
        WRITE(ITEXT(ICNT)(29:30),'(I2)')L
        NCTEXT(ICNT)=31
        AVALUE(ICNT)=REAL(N1(L))
        IDIGIT(ICNT)=0
11102 CONTINUE
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Distinct Cells:'
      NCTEXT(ICNT)=25
      AVALUE(ICNT)=REAL(NUMCEL)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Grand Mean:'
      NCTEXT(ICNT)=11
      AVALUE(ICNT)=GMEAN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Grand Standard Deviation:'
      NCTEXT(ICNT)=25
      AVALUE(ICNT)=GSD
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Residual Standard Deviation:'
      NCTEXT(ICNT)=28
      AVALUE(ICNT)=RESSD
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Residual Degrees of Freedom:'
      NCTEXT(ICNT)=28
      AVALUE(ICNT)=REAL(IRESDF)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      IF(IREP.EQ.'NO')THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='No Replication Case:'
        NCTEXT(ICNT)=20
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
      ELSE
        ICNT=ICNT+1
        ITEXT(ICNT)='Replication Case:'
        NCTEXT(ICNT)=17
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        ICNT=ICNT+1
        ITEXT(ICNT)='Replication Standard Deviation:'
        NCTEXT(ICNT)=31
        AVALUE(ICNT)=REPSD
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='Replication Degrees of Freedom:'
        NCTEXT(ICNT)=31
        AVALUE(ICNT)=REAL(IREPDF)
        IDIGIT(ICNT)=0
        IF(IFITDF.LT.1)THEN
          ICNT=ICNT+1
          ITEXT(ICNT)='Lack of Fit F Test cannot be done'
          NCTEXT(ICNT)=33
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
          ICNT=ICNT+1
          ITEXT(ICNT)='because there are 0 degrees of freedom'
          NCTEXT(ICNT)=38
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
          ICNT=ICNT+1
          ITEXT(ICNT)='in the numerator of the F ratio.  This'
          NCTEXT(ICNT)=38
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
          ICNT=ICNT+1
          ITEXT(ICNT)='happens when the number of parameters'
          NCTEXT(ICNT)=37
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
          ICNT=ICNT+1
          ITEXT(ICNT)='fitted is identical to the number of'
          NCTEXT(ICNT)=36
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
          ICNT=ICNT+1
          ITEXT(ICNT)='distinct subsets.'
          NCTEXT(ICNT)=17
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
        ELSE
          ICNT=ICNT+1
          ITEXT(ICNT)='Lack of Fit F Ratio:'
          NCTEXT(ICNT)=20
          AVALUE(ICNT)=FITFVA
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Lack of Fit F Ratio CDF (%):'
          NCTEXT(ICNT)=28
          AVALUE(ICNT)=FITCD2
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Lack of Fit Degrees of Freedom 1:'
          NCTEXT(ICNT)=33
          AVALUE(ICNT)=REAL(IFITDF)
          IDIGIT(ICNT)=0
          ICNT=ICNT+1
          ITEXT(ICNT)='Lack of Fit Degrees of Freedom 2:'
          NCTEXT(ICNT)=33
          AVALUE(ICNT)=REAL(IREPDF)
          IDIGIT(ICNT)=0
        ENDIF
      ENDIF
!
      NUMROW=ICNT
      DO 1105 I=1,NUMROW
        NTOT(I)=15
 1105 CONTINUE
!
      IFRST=.TRUE.
      ILAST=.TRUE.
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                  AVALUE,IDIGIT,   &
                  NTOT,NUMROW,   &
                  ICAPSW,ICAPTY,ILAST,IFRST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      IF(ICASBL.EQ.'NO')GO TO 13299
      ITITLE(1:13)='ANOVA Table'
      NCTITL=11
      ITITL9=' '
      NCTIT9=0
!
      NUMCOL=7
      NUMLIN=1
!
      ITITL2(1,1)='Source'
      NCTIT2(1,1)=6
      NCOLSP(1,1)=1
      ITITL2(1,2)='DF'
      NCTIT2(1,2)=2
      NCOLSP(1,2)=1
      ITITL2(1,3)='Sum of Squares'
      NCTIT2(1,3)=14
      NCOLSP(1,3)=1
      ITITL2(1,4)='Mean Square'
      NCTIT2(1,4)=11
      NCOLSP(1,4)=1
      ITITL2(1,5)='F Statistic'
      NCTIT2(1,5)=11
      NCOLSP(1,5)=1
      ITITL2(1,6)='F CDF'
      NCTIT2(1,6)=5
      NCOLSP(1,6)=1
      ITITL2(1,7)='Sig'
      NCTIT2(1,7)=3
      NCOLSP(1,7)=1
!
      NMAX=0
      DO 13010 I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IF(IAOV2T.EQ.'ON' .OR. IAOV2T.EQ.'YES')THEN
          IF(I.EQ.1)NTOT(I)=22
        ELSE
          IF(I.EQ.1)NTOT(I)=18
        ENDIF
        IF(I.EQ.2)NTOT(I)=4
        IF(I.EQ.6)NTOT(I)=9
        IF(I.EQ.7)NTOT(I)=4
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
        IF(I.EQ.1 .OR. I.EQ.6 .OR. I.EQ.7)ITYPCO(I)='ALPH'
        DO 13020 J=1,MAXROW
          IDIGI2(J,I)=-1
          IF(I.EQ.3 .OR. I.EQ.4)THEN
            IDIGI2(J,I)=NUMDIG
          ELSEIF(I.EQ.2)THEN
            IDIGI2(J,I)=0
          ELSEIF(I.EQ.5)THEN
            IDIGI2(J,I)=4
          ENDIF
13020   CONTINUE
13010 CONTINUE
!
      DO 13110 J=1,MAXROW
        DO 13120 I=1,NUMCOL
          IVALUE(J,I)=' '
          NCVALU(J,I)=0
          AMAT(J,I)=0.0
13120   CONTINUE
        ROWSEP(J)=0
13110 CONTINUE
!
!     TOP ROW (TOTAL)
!
      ICNT=1
      IVALUE(ICNT,1)='Total (Corrected)'
      NCVALU(ICNT,1)=17
      AMAT(ICNT,2)=REAL(N-1)
      AMAT(ICNT,3)=GSS
      AMAT(ICNT,4)=GSS/REAL(N-1)
      IDIGI2(ICNT,5)=-1
      IDIGI2(ICNT,6)=-1
      IDIGI2(ICNT,7)=-1
      ROWSEP(ICNT)=1
!
!     LOOP THROUGH EACH FACTOR
!
      DO 13210 L=1,NUMFAC
        ICNT=ICNT+1
        IVALUE(ICNT,1)='Factor   '
        WRITE(IVALUE(ICNT,1)(8:9),'(I2)')L
        NCVALU(ICNT,1)=9
        AMAT(ICNT,2)=REAL(N1(L)-1)
        AMAT(ICNT,3)=SS1(L)
        AMAT(ICNT,4)=SS1(L)/REAL(N1(L)-1)
        AMAT(ICNT,5)=FVAL(L)
        WRITE(IVALUE(ICNT,6)(1:8),'(F8.3)')F1CDF2(L)
        IVALUE(ICNT,6)(9:9)='%'
        NCVALU(ICNT,6)=9
        IF(F1CDF2(L).GE.99.0)THEN
          IVALUE(ICNT,7)='**'
          NCVALU(ICNT,7)=2
        ELSEIF(F1CDF2(L).GE.95.0)THEN
          IVALUE(ICNT,7)='*'
          NCVALU(ICNT,7)=1
        ELSE
          IVALUE(ICNT,7)=''
          NCVALU(ICNT,7)=0
        ENDIF
        IF(IAOV2T.NE.'ON' .AND. IAOV2T.NE.'YES')THEN
          IF(L.EQ.NUMFAC)ROWSEP(ICNT)=1
        ENDIF
13210 CONTINUE
!
!     LOOP THROUGH 2-TERM INTERACTION TERMS (IF REQUESTED)
!
      IF(I2TERM.EQ.'ON' .OR. I2TERM.EQ.'YES')THEN
        ICNT2=NUMFAC
        ICNT3=0
        DO 13215 II=1,NUMFAC-1
          DO 13218 JJ=II+1,NUMFAC
            ICNT2=ICNT2+1
            ICNT3=ICNT3+1
            ICNT=ICNT+1
            IVALUE(ICNT,1)='Interaction:    and '
            WRITE(IVALUE(ICNT,1)(14:15),'(I2)')II
            WRITE(IVALUE(ICNT,1)(21:22),'(I2)')JJ
            NCVALU(ICNT,1)=22
            AMAT(ICNT,2)=REAL(IDFIN(ICNT3))
            AMAT(ICNT,3)=SS2TIN(ICNT3)
            AMAT(ICNT,4)=MSEIN(ICNT3)
            AMAT(ICNT,5)=FVAL(ICNT2)
            WRITE(IVALUE(ICNT,6)(1:8),'(F8.3)')F1CDF2(ICNT2)
            IVALUE(ICNT,6)(9:9)='%'
            NCVALU(ICNT,6)=9
            IF(F1CDF2(ICNT2).GE.99.0)THEN
              IVALUE(ICNT,7)='**'
              NCVALU(ICNT,7)=2
            ELSEIF(F1CDF2(ICNT2).GE.95.0)THEN
              IVALUE(ICNT,7)='*'
              NCVALU(ICNT,7)=1
            ELSE
              IVALUE(ICNT,7)=''
              NCVALU(ICNT,7)=0
            ENDIF
            IF(ICNT3.EQ.NUMINT)ROWSEP(ICNT)=1
13218     CONTINUE
13215   CONTINUE
      ENDIF
!
!     LAST ROW (RESIDUAL)
!
      ICNT=ICNT+1
      IVALUE(ICNT,1)='Residual'
      NCVALU(ICNT,1)=8
      AMAT(ICNT,2)=REAL(IRESDF)
      AMAT(ICNT,3)=RESSS
      AMAT(ICNT,4)=RESMS
      IDIGI2(ICNT,5)=-1
      IDIGI2(ICNT,6)=-1
      IDIGI2(ICNT,7)=-1
!
      IWHTML(1)=125
      IWHTML(2)=25
      IWHTML(3)=125
      IWHTML(4)=125
      IWHTML(5)=125
      IWHTML(6)=125
      IWHTML(7)=25
      IINC=1800
      IINC2=200
      IWRTF(1)=IINC
      IWRTF(2)=IWRTF(1)+IINC2
      IWRTF(3)=IWRTF(2)+IINC
      IWRTF(4)=IWRTF(3)+IINC
      IWRTF(5)=IWRTF(4)+IINC
      IWRTF(6)=IWRTF(5)+IINC
      IWRTF(7)=IWRTF(6)+IINC2
!
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
13299 CONTINUE
!
      ITITLE='Estimation'
      NCTITL=10
!
      NUMCOL=6
      NUMLIN=1
!
      ITITL2(1,1)=' '
      NCTIT2(1,1)=0
      IF(I2TERM.EQ.'NO' .OR. I2TERM.EQ.'OFF')THEN
        ITITL2(1,2)='Level-ID'
        NCTIT2(1,2)=8
        ITITL2(1,3)='NI'
        NCTIT2(1,3)=2
        ITITL2(1,4)='Mean'
        NCTIT2(1,4)=4
        ITITL2(1,5)='Effect'
        NCTIT2(1,5)=6
        ITITL2(1,6)='SD(Effect)'
        NCTIT2(1,6)=10
      ELSE
        ITITL2(1,2)=' '
        NCTIT2(1,2)=0
        ITITL2(1,3)='Level-ID'
        NCTIT2(1,3)=8
        ITITL2(1,4)='NI'
        NCTIT2(1,4)=2
        ITITL2(1,5)='Mean'
        NCTIT2(1,5)=4
        ITITL2(1,6)='Effect'
        NCTIT2(1,6)=6
        ITITL2(1,7)='SD(Effect)'
        NCTIT2(1,7)=10
        NUMCOL=7
      ENDIF
!
      IWHTML(1)=125
      IWHTML(2)=125
      IWHTML(3)=125
      IWHTML(4)=125
      IWHTML(5)=125
      IWHTML(6)=125
      IWHTML(7)=125
      IWRTF(1)=IINC
      IWRTF(2)=IWRTF(1)+IINC
      IWRTF(3)=IWRTF(2)+IINC
      IWRTF(4)=IWRTF(3)+IINC
      IWRTF(5)=IWRTF(4)+IINC
      IWRTF(6)=IWRTF(5)+IINC
      IWRTF(7)=IWRTF(6)+IINC
      IFRST=.TRUE.
      ILAST=.TRUE.
      IFLAGS=.TRUE.
      IFLAGE=.TRUE.
!
      NMAX=0
      IF(I2TERM.EQ.'NO' .OR. I2TERM.EQ.'OFF')THEN
        DO 23010 I=1,NUMCOL
          VALIGN(I)='b'
          ALIGN(I)='r'
          NTOT(I)=11
          IF(I.EQ.1)NTOT(I)=10
          IF(I.EQ.2)NTOT(I)=9
          IF(I.EQ.3)NTOT(I)=8
          NMAX=NMAX+NTOT(I)
          ITYPCO(I)='NUME'
          IF(I.EQ.1)ITYPCO(I)='ALPH'
          IDIGIT(I)=5
          IF(I.LE.3)IDIGIT(I)=0
23010   CONTINUE
      ELSE
        DO 23015 I=1,NUMCOL
          VALIGN(I)='b'
          ALIGN(I)='r'
          NTOT(I)=11
          IF(I.EQ.1)NTOT(I)=12
          IF(I.EQ.2)NTOT(I)=9
          IF(I.EQ.3)NTOT(I)=9
          IF(I.EQ.4)NTOT(I)=8
          NMAX=NMAX+NTOT(I)
          ITYPCO(I)='NUME'
          IF(I.EQ.1)ITYPCO(I)='ALPH'
          IDIGIT(I)=5
          IF(I.LE.4)IDIGIT(I)=0
          IF(I.EQ.2)IDIGIT(I)=-1
23015   CONTINUE
      ENDIF
!
      ICNT=0
      DO 11590 L=1,NUMFAC
        DO 11595 I=1,N1(L)
          ICNT=ICNT+1
          IF(ICNT.GT.55)THEN
            CALL DPDTA5(ITITLE,NCTITL,   &
                        ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                        MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                        IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                        IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                        ICAPSW,ICAPTY,IFRST,ILAST,   &
                        IFLAGS,IFLAGE,   &
                        ISUBRO,IBUGA3,IERROR)
            ICNT=1
          ENDIF
          IF(I.EQ.1)THEN
            IVALUE(ICNT,1)='Factor   '
            WRITE(IVALUE(ICNT,1)(8:9),'(I2)')L
          ELSE
            IVALUE(ICNT,1)='         '
          ENDIF
          NCVALU(ICNT,1)=9
          IF(I2TERM.EQ.'NO' .OR. I2TERM.EQ.'OFF')THEN
            AMAT(ICNT,2)=F1ID(I,L)
            AMAT(ICNT,3)=F1N(I,L)
            AMAT(ICNT,4)=F1MEAN(I,L)
            AMAT(ICNT,5)=F1EFFE(I,L)
            AMAT(ICNT,6)=F1EFSD(I,L)
          ELSE
            AMAT(ICNT,2)=0.0
            AMAT(ICNT,3)=F1ID(I,L)
            AMAT(ICNT,4)=F1N(I,L)
            AMAT(ICNT,5)=F1MEAN(I,L)
            AMAT(ICNT,6)=F1EFFE(I,L)
            AMAT(ICNT,7)=F1EFSD(I,L)
          ENDIF
11595 CONTINUE
11590 CONTINUE
!
      IF(I2TERM.EQ.'YES' .OR. I2TERM.EQ.'ON')THEN
        IDIGIT(2)=0
        ICNT4=0
        DO 15410 KK=1,NUMFAC
          NLEVEL1=N1(KK)
          DO 15420 LL=KK+1,NUMFAC
            NLEVEL2=N1(KK)
            DO 15430 ISET1=1,NLEVEL1
              DO 15435 ISET2=1,NLEVEL2
                ICNT4=ICNT4+1
                ICNT=ICNT+1
                IF(ICNT.GT.55)THEN
                  CALL DPDTA5(ITITLE,NCTITL,   &
                              ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                              MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                              IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                              IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,   &
                              NMAX,   &
                              ICAPSW,ICAPTY,IFRST,ILAST,   &
                              IFLAGS,IFLAGE,   &
                              ISUBRO,IBUGA3,IERROR)
                  ICNT=1
                ENDIF
                IF(ISET1.EQ.1 .AND. ISET2.EQ.1)THEN
                  IVALUE(ICNT,1)='     and   :'
                  WRITE(IVALUE(ICNT,1)(3:4),'(I2)')KK
                  WRITE(IVALUE(ICNT,1)(10:11),'(I2)')LL
                  NCVALU(ICNT,1)=12
                ELSE
                  IVALUE(ICNT,1)=' '
                  NCVALU(ICNT,1)=0
                ENDIF
                AMAT(ICNT,2)=F1ID(ISET1,KK)
                AMAT(ICNT,3)=F1ID(ISET2,LL)
                AMAT(ICNT,4)=AINN(ICNT4)
                AMAT(ICNT,5)=AINMEA(ICNT4)
                AMAT(ICNT,6)=AINEFF(ICNT4)
                AMAT(ICNT,7)=AINESD(ICNT4)
15435         CONTINUE
15430       CONTINUE
15420     CONTINUE
15410   CONTINUE
      ENDIF
!
      IF(ICNT.GE.1)THEN
        CALL DPDTA5(ITITLE,NCTITL,   &
                    ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                    MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                    IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                    IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                    ICAPSW,ICAPTY,IFRST,ILAST,   &
                    IFLAGS,IFLAGE,   &
                    ISUBRO,IBUGA3,IERROR)
      ENDIF
!
      IF(I2TERM.EQ.'NO' .OR. I2TERM.EQ.'OFF')THEN
        ITITLE='Models'
        NCTITL=6
      ELSE
        ITITLE='Models (based on main effects only)'
        NCTITL=36
      ENDIF
!
      NUMCOL=2
      NUMLIN=1
!
      ITITL2(1,1)='Model'
      NCTIT2(1,1)=5
      ITITL2(1,2)='Residual Standard Deviation'
      NCTIT2(1,2)=27
!
      NMAX=0
      DO 24010 I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='l'
        NTOT(I)=30
        IF(I.EQ.2)NTOT(I)=27
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
        IF(I.EQ.1)ITYPCO(I)='ALPH'
        IDIGIT(I)=NUMDIG
24010 CONTINUE
!
      ICNT=0
      ICNT=ICNT+1
      IVALUE(ICNT,1)='Constant               Only--'
      NCVALU(ICNT,1)=30
      AMAT(ICNT,2)=GSD
      DO 12827 L=1,NUMFAC
        ICNT=ICNT+1
        IVALUE(ICNT,1)='Constant and Factor    Only--'
        WRITE(IVALUE(ICNT,1)(21:22),'(I2)')L
        NCVALU(ICNT,1)=30
        AMAT(ICNT,2)=RSD(L)
12827 CONTINUE
      ICNT=ICNT+1
      IVALUE(ICNT,1)='Constant and All    Factors--'
      WRITE(IVALUE(ICNT,1)(18:19),'(I2)')L
      NCVALU(ICNT,1)=30
      AMAT(ICNT,2)=RESSD
!
      IWHTML(1)=300
      IWHTML(2)=200
      IINC3=3000
      IWRTF(1)=IINC3
      IWRTF(2)=IWRTF(1)+IINC
!
      IFRST=.TRUE.
      ILAST=.TRUE.
      IFLAGS=.TRUE.
      IFLAGE=.TRUE.
      CALL DPDTA5(ITITLE,NCTITL,   &
                  ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                  MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                  IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                  ICAPSW,ICAPTY,IFRST,ILAST,   &
                  IFLAGS,IFLAGE,   &
                  ISUBRO,IBUGA3,IERROR)
!
!CCCCC THE FOLLOWING SECTION WAS ADDED FEBRUARY  1998
!               ********************************************
!               **  STEP 12.5                             **
!               **  WRITE INFO OUT TO FILES--             **
!               **     1) DPST1F.DAT--FACTOR ELEMENTS     **
!               **        (DF, SUMSQ, MSQ, F STAT, F CDF) **
!               **     2) DPST2F.DAT--EFFECT ESTIMATES    **
!               **        (FACTOR ID, LEVEL ID, NI, MEAN, **
!               **        EFFECT, SD (EFFECT)             **
!               **     3) RESSD FOR EACH MODEL            **
!               ********************************************
!
      ISTEPN='12.5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ANO2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 14100 L=1,NUMFAC
        WRITE(IOUNI1,14110)L,N1(L)-1,SS1(L),SS1(L)/(N1(L)-1),FVAL(L),   &
                           F1CDF2(L)
14100 CONTINUE
14110 FORMAT(I5,2X,I5,4(1X,E15.7))
!
      DO 14120 L=1,NUMFAC
        DO 14130 J=1,N1(L)
        WRITE(IOUNI2,14122)L,F1ID(J,L),F1N(J,L),F1MEAN(J,L),   &
                           F1EFFE(J,L),F1EFSD(J,L)
14122   FORMAT(I5,2X,F6.0,2X,F6.0,3(1X,E15.7))
14130   CONTINUE
14120 CONTINUE
!
      WRITE(IOUNI3,14620)GSD
14620 FORMAT(F20.10)
      IF(NUMFAC.GE.1)THEN
        DO 14627 I=1,NUMFAC
          WRITE(IOUNI3,14621)RSD(I)
14621     FORMAT(F20.10)
14627   CONTINUE
      ENDIF
      WRITE(IOUNI3,14633)RESSD
14633 FORMAT(F20.10)
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,14211)
14211   FORMAT('DPST1F.DAT: FACTOR DF, SUM OF SQUARES, MEAN SQUARE, ',   &
               'F STAT, F CDF')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,14212)
14212   FORMAT('DPST2F.DAT: FACTOR-ID, LEVEL-ID, NI, MEAN, EFFECT, ',   &
               'SD(EFFECT)')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,14214)
14214   FORMAT('DPST3F.DAT: RESIDUAL STANDARD DEVIATION OF MODELS')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               **************************************
!               **  STEP 13--                       **
!               **  CLOSE       THE STORAGE FILES.  **
!               **************************************
!
      ISTEPN='13'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ANO2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IOP='CLOS'
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                  IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ANO2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPANO2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9023)REPSS,REPMS,REPSD,REPDF
 9023   FORMAT('REPSS,REPMS,REPSD,REPDF = ',4E15.7)
        CALL DPWRST('XXX','BUG ')
        DO 9025 I=1,N
          WRITE(ICOUT,9026)I,Y(I),F1(I,1),F1(I,1),W(I),PRED2(I),RES2(I)
 9026     FORMAT('I,Y(I),F1(I),F2(I),W(I),PRED2(I),RES2(I) = ',   &
                 I8,6E11.4)
          CALL DPWRST('XXX','BUG ')
 9025   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPANO2
      SUBROUTINE DPANO4(YMEAN,YSD,YN,NGROUP,ALPHA,IVARID,IVARI2,IANOMC,   &
                        ICASE1,ICASE2,Y,X,N,XDIST,TEMP1,                  &
                        PRED,RES,                                         &
                        SSTOT,SSTR,SSE,MSE,MSTR,IDFTO,IDFTR,IDFE,         &
                        FSTAT,FVAL,PVALUE,RESSD,                          &
                        ICAPSW,ICAPTY,IFORSW,                             &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--PERFORM A ONE WAY ANALYSIS OF VARIANCE (ANOVA).  THE DATA
!              IS ASSUMED TO BE A RESPONSE VARIABLE AND A GROUP-ID
!              VARIABLE.
!
!              COMPARED TO THE STANDARD ANOVA COMMAND IN DATAPLOT, THIS
!              ROUTINE ADDS THE FOLLOWING:
!
!                1. SUPPORT FOR SUMMARY DATA (MEAN, SD, SAMPLE SIZE) IN
!                   ADDITION TO RAW DATA
!
!                2. SUPPORT FOR RANDOM EFFECTS.  THIS ADDS A "VARIANCE
!                   "COMPONENTS" TABLE.
!
!                3. ADDITIONAL TABLES FOR: CONFIDENCE LEVEL FOR
!                   TREATMENT MEANS, CONFIDENCE INTERVAL FOR DIFFERENCE
!                   OF TREATMENT MEANS (I.E., MULTIPLE COMPARISONS),
!                   BONFERRONI CONFIDENCE INTERVALS FOR DIFFERENCE OF
!                   TREATMENT MEANS, AND TUKEY-CRAMER CONFIDENCE
!                   INTERVALS FOR DIFFERENCE OF TREATMENT MEANS.
!                   NOTE THAT THESE TABLES ARE ONLY GENERATED FOR THE
!                   FIXED EFFECTS CASE.
!
!                4. SUPPORT FOR UNBALANCED DATA.
!
!     PRINTING--YES
!     SUBROUTINES NEEDED--FCDF
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2023/10
!     ORIGINAL VERSION--SEPTEMBER 2023.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 IANOMC
      CHARACTER*4 ICASE1
      CHARACTER*4 ICASE2
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
      CHARACTER*4 IVARID(*)
      CHARACTER*4 IVARI2(*)
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IOP
      CHARACTER*4 ICASEZ
      CHARACTER*4 IWRITE
!
!---------------------------------------------------------------------
!
      DIMENSION YMEAN(*)
      DIMENSION YSD(*)
      DIMENSION YN(*)
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION XDIST(*)
      DIMENSION TEMP1(*)
      DIMENSION PRED(*)
      DIMENSION RES(*)
!
      REAL MSTR
      REAL MSE
      REAL MSEPCT
      REAL MSERAT
!
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
      DOUBLE PRECISION QTRNG
!
      PARAMETER(NUMCLI=8)
      PARAMETER(MAXLIN=3)
      PARAMETER (MAXROW=60)
      CHARACTER*70 ITITLE
      CHARACTER*70 ITITLZ
      CHARACTER*70 ITITL9
      CHARACTER*60 ITEXT(MAXROW)
      CHARACTER*4  ALIGN(NUMCLI)
      CHARACTER*4  VALIGN(NUMCLI)
      REAL         AVALUE(MAXROW)
      INTEGER      NCTEXT(MAXROW)
      INTEGER      IDIGIT(MAXROW)
      INTEGER      IDIGI2(MAXROW,NUMCLI)
      INTEGER      NTOT(MAXROW)
      INTEGER      ROWSEP(MAXROW)
      CHARACTER*60 ITITL2(MAXLIN,NUMCLI)
      CHARACTER*30 IVALUE(MAXROW,NUMCLI)
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
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
      ISUBN1='DPAN'
      ISUBN2='O4  '
      IWRITE='OFF '
!
      SSTOT=CPUMIN
      SSTR=CPUMIN
      SSE=CPUMIN
      MSE=CPUMIN
      MSTR=CPUMIN
      FSTAT=CPUMIN
      FVAL=CPUMIN
      PVALUE=CPUMIN
      RESSD=CPUMIN
      SA2PCT=CPUMIN
      MSEPCT=CPUMIN
!
      IDFTO=-1
      IDFTR=-1
      IDFE=-1
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ANO4')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPANO4--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,IANOMC,NGROUP,N,ALPHA
   52   FORMAT('IBUGA3,ISUBRO,IANOMC,NGROUP,N,ALPHA = ',   &
               3(A4,2X),2I8,G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASE1,ICASE2
   53   FORMAT('ICASE1,ICASE2 = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        IF(ICASE1.EQ.'SUMM')THEN
          DO 55 I=1,NGROUP
            WRITE(ICOUT,56)I,YMEAN(I),YSD(I),YN(I)
   56       FORMAT('I,YMEAN(I),YSD(I),YN(I) = ',I8,3G15.7)
            CALL DPWRST('XXX','BUG ')
   55     CONTINUE
        ELSE
          DO 65 I=1,N
            WRITE(ICOUT,66)I,Y(I),X(I)
   66       FORMAT('I,Y(I),X(I) = ',I8,2G15.7)
            CALL DPWRST('XXX','BUG ')
   65     CONTINUE
        ENDIF
      ENDIF
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      IF(ICASE1.EQ.'SUMM')THEN
        IF(NGROUP.LE.1)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
  101     FORMAT('***** ERROR IN ONE WAY SUMMARY FIXED EFFECTS ANOVA--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,102)
  102     FORMAT('      THE NUMBER OF GROUPS FOR THE ANOVA COMMAND ',   &
                 'BE AT LEAST 2.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,104)NGROUP
  104     FORMAT('      THE NUMBER OF GROUPS HERE = ',I6)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
!
        DO 140 I=1,NGROUP
          NI=INT(YN(I)+0.1)
          IF(YSD(I).LE.0.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,101)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,142)I,YSD(I)
  142       FORMAT('      THE STANDARD DEVIATION FOR GROUP ',I6,   &
                   'IS NON-POSITIVE (',G15.7,').')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ELSEIF(NI.LE.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,101)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,147)I,NI
  147       FORMAT('      THE SAMPLE SIZE FOR GROUP ',I6,   &
                   'IS < 1 (',I6,').')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
  140   CONTINUE
      ELSE
        IF(N.LE.3)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,151)
  151     FORMAT('***** ERROR IN ONE WAY FIXED EFFECTS ANOVA--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,152)
  152     FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE ANOVA ',   &
                 'COMMAND MUST BE AT LEAST 3.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,154)NGROUP
  154     FORMAT('      THE NUMBER OF OBSERVATIONS = ',I6)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
      ENDIF
!
      IFLAG1=1
      IFLAG2=1
      IFLAG3=1
      IFLAG4=1
      IFLAG5=1
      IOP='OPEN'
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                  IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               ****************************************************
!               **  STEP 2--                                      **
!               **  COMPUTE THE TOTAL SAMPLE SIZE AND GRAND MEAN  **
!               ****************************************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ANO4')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICASE1.EQ.'RAW ')THEN
        CALL MEAN(Y,N,IWRITE,YGRAND,IBUGA3,IERROR)
        CALL SD(Y,N,IWRITE,GSD,IBUGA3,IERROR)
        NTOTAL=N
      ELSE
        NTOTAL=0
        DSUM1=0.0D0
        DO 210 I=1,NGROUP
          NI=INT(YN(I) + 0.1)
          NTOTAL=NTOTAL+NI
          DSUM1=DSUM1 + DBLE(NI)*DBLE(YMEAN(I))
  210   CONTINUE
        YGRAND=REAL(DSUM1/DBLE(NTOTAL))
      ENDIF
!
!               ******************************************************
!               **  STEP 3--                                        **
!               **  COMPUTE THE VARIOUS SUM OF SQUARES AND MEAN     **
!               **  SQUARE ERROR TERMS.                             **
!               ******************************************************
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ANO4')THEN
        ISTEPN='3'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,301)YGRAND,DSUM1,NTOTAL,N
  301   FORMAT('YGRAND,DSUM1,NTOTAL,N = ',2G15.7,2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(ICASE1.EQ.'RAW ')THEN
        ICASEZ='FVAL'
        CALL SSQTOT(Y,X,NTOTAL,XDIST,TEMP1,   &
                    YMEAN,YSD,YN,NGROUP,IWRITE,ICASEZ,   &
                    SSTOT,SSTR,SSE,SSX,DFTO,DFTR,DFE,   &
                    MSTR,MSE,FSTAT,   &
                    IBUGA3,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
        IDFTR=INT(DFTR+0.01)
        IDFTO=INT(DFTO+0.01)
        IDFE=INT(DFE+0.01)
      ELSE
        DSUM1=0.0D0
        DSUM2=0.0D0
        DO 310 I=1,NGROUP
          NI=INT(YN(I) + 0.1)
          DSUM1=DSUM1 + DBLE(NI)*DBLE((YMEAN(I) - YGRAND)**2)
          DSUM2=DSUM2 + DBLE(NI-1)*DBLE(YSD(I)**2)
  310   CONTINUE
        SSTR=REAL(DSUM1)
        IDFTR=NGROUP-1
        MSTR=SSTR/REAL(IDFTR)
        SSE=REAL(DSUM2)
        IDFE=NTOTAL-NGROUP
        MSE=SSE/REAL(IDFE)
        SSTOT=SSTR+SSE
        IDFTO=NTOTAL-1
        FSTAT=MSTR/MSE
      ENDIF
!
!     VARIANCE COMPONENTS FOR RANDOM EFFECTS
!
      IF(ICASE2.EQ.'RAND')THEN
        SUM1=0.0
        SUM2=0.0
        C1=1.0/REAL(NGROUP-1)
        DO 370 I=1,NGROUP
          ANII=YN(I)
          SUM1=SUM1 + ANII
          SUM2=SUM2 + ANII**2
  370   CONTINUE
        AN0=C1*(SUM1 - (SUM2/SUM1))
        SA2=(MSTR - MSE)/AN0
        IF(SA2.LT.0.0)SA2=0.0
        SA2RAT=SA2/(SA2 + MSE)
        SA2PCT=100.*SA2/(SA2 + MSE)
        MSERAT=MSE/(SA2+MSE)
        MSEPCT=100.*MSE/(SA2+MSE)
!
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ANO4')THEN
          WRITE(ICOUT,379)C1,SUM1,SUM2,AN0,SA2
  379     FORMAT('C1,SUM1,SUM2,AN0,SA2 = ',5G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
      ENDIF
      CALL FCDF(FSTAT,IDFTR,IDFE,FVAL)
      PVALUE=1.0 - FVAL
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ANO4')THEN
        WRITE(ICOUT,391)YGRAND,SSTOT,SSTR,SSE
  391   FORMAT('YGRAND,SSTOT,SSTR,SSE = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,393)MSTR,MSE,FSTAT,FVAL,PVALUE
  393   FORMAT('MSTR,MSE,FSTAT,FVAL,PVALUE = ',5G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ****************************
!               **  STEP 8--              **
!               **  WRITE EVERYTHING OUT  **
!               ****************************
!
      ISTEPN='8'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ANO4')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     PRINT SUMMARY STATISTICS TABLE
!
      IF(ICASE2.EQ.'RAND')THEN
        ITITLE(1:45)='One-Way Random Effects Analysis of Variance: '
        IF(ICASE1.EQ.'SUMM')THEN
          ITITLE(46:58)='Summary Data'
          NCTITL=58
        ELSE
          ITITLE(46:53)='Raw Data'
          NCTITL=53
        ENDIF
        ITITLZ='H0: Variance of Effects = 0'
        NCTITZ=27
      ELSE
        ITITLE(1:44)='One-Way Fixed Effects Analysis of Variance: '
        IF(ICASE1.EQ.'SUMM')THEN
          ITITLE(45:57)='Summary Data'
          NCTITL=57
        ELSE
          ITITLE(45:52)='Raw Data'
          NCTITL=52
        ENDIF
        ITITLZ='H0: Effect(1) = Effect(2) = ... - Effect(k) = 0'
        NCTITZ=47
      ENDIF
!
      IF(ICASE1.EQ.'SUMM')THEN
        ICNT=1
        ITEXT(ICNT)='Means Variable: '
        WRITE(ITEXT(ICNT)(23:26),'(A4)')IVARID(1)(1:4)
        WRITE(ITEXT(ICNT)(27:30),'(A4)')IVARI2(1)(1:4)
        NCTEXT(ICNT)=30
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
!
        ICNT=ICNT+1
        ITEXT(ICNT)='SD Variable: '
        WRITE(ITEXT(ICNT)(23:26),'(A4)')IVARID(2)(1:4)
        WRITE(ITEXT(ICNT)(27:30),'(A4)')IVARI2(2)(1:4)
        NCTEXT(ICNT)=30
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
!
        ICNT=ICNT+1
        ITEXT(ICNT)='Sample Size Variable: '
        WRITE(ITEXT(ICNT)(23:26),'(A4)')IVARID(3)(1:4)
        WRITE(ITEXT(ICNT)(27:30),'(A4)')IVARI2(3)(1:4)
        NCTEXT(ICNT)=30
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
      ELSE
        ICNT=1
        ITEXT(ICNT)='Response Variable: '
        WRITE(ITEXT(ICNT)(23:26),'(A4)')IVARID(1)(1:4)
        WRITE(ITEXT(ICNT)(27:30),'(A4)')IVARI2(1)(1:4)
        NCTEXT(ICNT)=30
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
!
        ICNT=ICNT+1
        ITEXT(ICNT)='Group-ID Variable: '
        WRITE(ITEXT(ICNT)(23:26),'(A4)')IVARID(2)(1:4)
        WRITE(ITEXT(ICNT)(27:30),'(A4)')IVARI2(2)(1:4)
        NCTEXT(ICNT)=30
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
!
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
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Total Number of Observations:'
      NCTEXT(ICNT)=28
      AVALUE(ICNT)=REAL(NTOTAL)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Levels:'
      NCTEXT(ICNT)=17
      AVALUE(ICNT)=REAL(NGROUP)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Grand Mean:'
      NCTEXT(ICNT)=11
      AVALUE(ICNT)=YGRAND
      IDIGIT(ICNT)=NUMDIG
      IF(ICASE1.EQ.'RAW ')THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='Grand Standard Deviation:'
        NCTEXT(ICNT)=25
        AVALUE(ICNT)=GSD
        IDIGIT(ICNT)=NUMDIG
      ENDIF
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      NUMROW=ICNT
      DO 1105 I=1,NUMROW
        NTOT(I)=15
 1105 CONTINUE
!
      IFRST=.TRUE.
      ILAST=.TRUE.
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                  AVALUE,IDIGIT,   &
                  NTOT,NUMROW,   &
                  ICAPSW,ICAPTY,ILAST,IFRST,   &
                  ISUBRO,IBUGA3,IERROR)
!
!     PRINT OUT STANDARD ANOVA TABLE
!
      ITITLE(1:26)='ANOVA Table: Fixed Effects'
      NCTITL=26
      ITITL9=' '
      NCTIT9=0
!
      NUMCOL=8
      NUMLIN=1
!
      ITITL2(1,1)='Source'
      NCTIT2(1,1)=6
      NCOLSP(1,1)=1
      ITITL2(1,2)='DF'
      NCTIT2(1,2)=2
      NCOLSP(1,2)=1
      ITITL2(1,3)='Sum of Squares'
      NCTIT2(1,3)=14
      NCOLSP(1,3)=1
      ITITL2(1,4)='Mean Square'
      NCTIT2(1,4)=11
      NCOLSP(1,4)=1
      ITITL2(1,5)='F Statistic'
      NCTIT2(1,5)=11
      NCOLSP(1,5)=1
      ITITL2(1,6)='F CDF (%)'
      NCTIT2(1,6)=9
      NCOLSP(1,6)=1
      ITITL2(1,7)='P-Value'
      NCTIT2(1,7)=7
      NCOLSP(1,7)=1
      ITITL2(1,8)='Sig'
      NCTIT2(1,8)=3
      NCOLSP(1,8)=1
!
      NMAX=0
      DO 13010 I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IF(I.EQ.1)NTOT(I)=20
        IF(I.EQ.2)NTOT(I)=5
        IF(I.EQ.6)NTOT(I)=12
        IF(I.EQ.7)NTOT(I)=12
        IF(I.EQ.8)NTOT(I)=4
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
        IF(I.EQ.1 .OR. I.EQ.8)ITYPCO(I)='ALPH'
        DO 13020 J=1,MAXROW
          IDIGI2(J,I)=-1
          IF(I.EQ.3 .OR. I.EQ.4)THEN
            IDIGI2(J,I)=NUMDIG
          ELSEIF(I.EQ.2)THEN
            IDIGI2(J,I)=0
          ELSEIF(I.EQ.5)THEN
            IDIGI2(J,I)=4
          ELSEIF(I.EQ.6)THEN
            IDIGI2(J,I)=4
          ELSEIF(I.EQ.7)THEN
            IDIGI2(J,I)=6
          ENDIF
13020   CONTINUE
13010 CONTINUE
!
      DO 13110 J=1,MAXROW
        DO 13120 I=1,NUMCOL
          IVALUE(J,I)=' '
          NCVALU(J,I)=0
          AMAT(J,I)=0.0
13120   CONTINUE
        ROWSEP(J)=0
13110 CONTINUE
!
!     TOP ROW (TOTAL)
!
      ICNT=1
      IVALUE(ICNT,1)='Total (Corrected)'
      NCVALU(ICNT,1)=17
      AMAT(ICNT,2)=REAL(IDFTO)
      AMAT(ICNT,3)=SSTOT
      IDIGI2(ICNT,4)=-1
      IDIGI2(ICNT,5)=-1
      IDIGI2(ICNT,6)=-1
      IDIGI2(ICNT,7)=-1
      IDIGI2(ICNT,8)=-1
      ROWSEP(ICNT)=1
!
!     TREATMENT (BETWEEN)
!
      ICNT=ICNT+1
      IVALUE(ICNT,1)='Treatment (Between)'
      NCVALU(ICNT,1)=19
      AMAT(ICNT,2)=REAL(IDFTR)
      AMAT(ICNT,3)=SSTR
      AMAT(ICNT,4)=MSTR
      AMAT(ICNT,5)=FSTAT
      AMAT(ICNT,6)=100.0*FVAL
      AMAT(ICNT,7)=PVALUE
      IF(FVAL.GE.0.99)THEN
        IVALUE(ICNT,8)='**'
        NCVALU(ICNT,8)=2
      ELSEIF(FVAL.GE.0.95)THEN
        IVALUE(ICNT,8)='*'
        NCVALU(ICNT,8)=1
      ELSE
        IVALUE(ICNT,8)=' '
        NCVALU(ICNT,8)=0
      ENDIF
!
!     LAST ROW (RESIDUAL = ERROR = WITHIN)
!
      ICNT=ICNT+1
      IVALUE(ICNT,1)='Residual (Within)'
      NCVALU(ICNT,1)=17
      AMAT(ICNT,2)=REAL(IDFE)
      AMAT(ICNT,3)=SSE
      AMAT(ICNT,4)=MSE
      IDIGI2(ICNT,5)=-1
      IDIGI2(ICNT,6)=-1
      IDIGI2(ICNT,7)=-1
!
      IWHTML(1)=125
      IWHTML(2)=25
      IWHTML(3)=125
      IWHTML(4)=125
      IWHTML(5)=125
      IWHTML(6)=125
      IWHTML(7)=25
      IINC=1800
      IINC2=200
      IWRTF(1)=IINC
      IWRTF(2)=IWRTF(1)+IINC2
      IWRTF(3)=IWRTF(2)+IINC
      IWRTF(4)=IWRTF(3)+IINC
      IWRTF(5)=IWRTF(4)+IINC
      IWRTF(6)=IWRTF(5)+IINC
      IWRTF(7)=IWRTF(6)+IINC2
!
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
!     GENERATE VARIANCE COMPONENTS TABLE FOR RANDOM EFFECTS MODEL
!
      IF(ICASE2.EQ.'RAND')THEN
        ITITLE='Variance Components Table: Random Effects'
        NCTITL=41
        ITITL9=' '
        NCTIT9=0
!
        NUMCOL=7
        NUMLIN=3
!
        ITITL2(1,1)=' '
        NCTIT2(1,1)=0
        NCOLSP(1,1)=1
        ITITL2(2,1)=' '
        NCTIT2(2,1)=0
        NCOLSP(2,1)=1
        ITITL2(3,1)='Source'
        NCTIT2(3,1)=6
        NCOLSP(3,1)=1
!
        ITITL2(1,2)=' '
        NCTIT2(1,2)=0
        NCOLSP(1,2)=1
        ITITL2(2,2)='Variance'
        NCTIT2(2,2)=8
        NCOLSP(2,2)=1
        ITITL2(3,2)='Component'
        NCTIT2(3,2)=9
        NCOLSP(3,2)=1
!
        ITITL2(1,3)='Lower'
        NCTIT2(1,3)=5
        NCOLSP(1,3)=1
        ITITL2(2,3)='Confidence'
        NCTIT2(2,3)=10
        NCOLSP(2,3)=1
        ITITL2(3,3)='Limit'
        NCTIT2(3,3)=5
        NCOLSP(3,3)=1
!
        ITITL2(1,4)='Upper'
        NCTIT2(1,4)=5
        NCOLSP(1,4)=1
        ITITL2(2,4)='Confidence'
        NCTIT2(2,4)=10
        NCOLSP(2,4)=1
        ITITL2(3,4)='Limit'
        NCTIT2(3,4)=5
        NCOLSP(3,4)=1
!
        ITITL2(1,5)='Intraclass'
        NCTIT2(1,5)=10
        NCOLSP(1,5)=1
        ITITL2(2,5)='Correlation'
        NCTIT2(2,5)=11
        NCOLSP(2,5)=1
        ITITL2(3,5)='Coefficient (%)'
        NCTIT2(3,5)=15
        NCOLSP(3,5)=1
!
        ITITL2(1,6)='Lower'
        NCTIT2(1,6)=5
        NCOLSP(1,6)=1
        ITITL2(2,6)='Confidence'
        NCTIT2(2,6)=10
        NCOLSP(2,6)=1
        ITITL2(3,6)='Limit'
        NCTIT2(3,6)=5
        NCOLSP(3,6)=1
!
        ITITL2(1,7)='Upper'
        NCTIT2(1,7)=5
        NCOLSP(1,7)=1
        ITITL2(2,7)='Confidence'
        NCTIT2(2,7)=10
        NCOLSP(2,7)=1
        ITITL2(3,7)='Limit'
        NCTIT2(3,7)=5
        NCOLSP(3,7)=1
!
        NMAX=0
        DO 14010 I=1,NUMCOL
          ITYPCO(I)='NUME'
          VALIGN(I)='b'
          ALIGN(I)='r'
          NTOT(I)=15
          IF(I.EQ.1)THEN
            NTOT(I)=20
            ITYPCO(I)='ALPH'
          ELSEIF(I.EQ.5)THEN
            NTOT(I)=17
          ENDIF
          NMAX=NMAX+NTOT(I)
          DO 14020 J=1,MAXROW
            IDIGI2(J,I)=NUMDIG
            IF(I.EQ.1)THEN
              IDIGI2(J,I)=-1
            ELSEIF(I.EQ.5)THEN
              IDIGI2(J,I)=2
            ENDIF
14020     CONTINUE
14010   CONTINUE
!
        DO 14110 J=1,MAXROW
          DO 14120 I=1,NUMCOL
            IVALUE(J,I)=' '
            NCVALU(J,I)=0
            AMAT(J,I)=0.0
14120     CONTINUE
          ROWSEP(J)=0
14110   CONTINUE
        ICNT=0
!
!     FIRST ROW (TREATMENT/BETWEEN)
!
        ALPHAT=ALPHA
        IF(ALPHAT.GE.1.0 .AND. ALPHAT.LT.100.0)ALPHAT=ALPHAT/100.0
        IF(ALPHAT.LE.0.0 .OR. ALPHAT.GE.1.0)THEN
          ALPHAT=0.05
        ELSEIF(ALPHAT.GT.0.5)THEN
          ALPHAT=1.0 - ALPHAT
        ENDIF
        ALPHAL=ALPHAT/2.0
        ALPHAU=1.0 - (ALPHAT/2.0)
!
        ICNT=ICNT+1
        IVALUE(ICNT,1)='Treatment (Between)'
        NCVALU(ICNT,1)=19
        AMAT(ICNT,2)=SA2
        AMAT(ICNT,5)=SA2PCT
!
        NU1=NGROUP-1
        NU2=NTOTAL-NGROUP
        CALL FPPF(ALPHAL,NU1,NU2,FL)
        CALL FPPF(ALPHAU,NU1,NU2,FU)
        F0=FSTAT
        CONST1=1.0/AN0
        AL=CONST1*((F0/FU) - 1.0)
        AU=CONST1*((F0/FL) - 1.0)
        ALL=AL/(1.0+AL)
        AUL=AU/(1.0+AU)
        NU=NGROUP-1
        CALL CHSPPF(ALPHAL,NU,CHSL)
        CALL CHSPPF(ALPHAU,NU,CHSU)
        CONST1=1.0 - (FU/F0)
        CONST2=1.0 - (FL/F0)
        ANUM1=SSTR*CONST1
        ANUM2=SSTR*CONST2
        ADEN1=AN0*CHSL
        ADEN2=AN0*CHSU
        ALR=ANUM1/ADEN2
        AUR=ANUM2/ADEN1
!
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ANO4')THEN
          WRITE(ICOUT,1601)NU1,NU2,F0,FL,FU
 1601     FORMAT('NU1,NU2,F0,FL,FU = ',2I6,3G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1603)AL,AU,ALL,AUL
 1603     FORMAT('AL,AU,ALL,AUL = ',4G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1605)NU,CHSL,CHSU,CONST1,CONST2
 1605     FORMAT('NU,CHSL,CHSU,CONST1,CONST2 = ',I8,6G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1607)ANUM1,ANUM2,ADEN1,ADEN2,ALR,AUR
 1607     FORMAT('ANUM1,ANUM2,ADEN1,ADEN2,ALR,AUR = ',6G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        AMAT(ICNT,3)=ALR
        AMAT(ICNT,4)=AUR
        IF(AMAT(ICNT,3).LT.0.0)AMAT(ICNT,3)=0.0
        AMAT(ICNT,6)=100.*ALL
        AMAT(ICNT,7)=100.*AUL
        IF(AMAT(ICNT,6).LT.0.0)AMAT(ICNT,6)=0.0
        IF(AMAT(ICNT,7).GT.100.0)AMAT(ICNT,7)=100.0
!
!       SECOND ROW (RESIDUAL = ERROR = WITHIN)
!
        ICNT=ICNT+1
        IVALUE(ICNT,1)='Residual (Within)'
        NCVALU(ICNT,1)=17
        AMAT(ICNT,2)=MSE
        AMAT(ICNT,5)=MSEPCT
!
        C1=REAL(NTOTAL-NGROUP)*MSE
        NU=NTOTAL-NGROUP
        ALPHAL=ALPHAT/2.0
        CALL CHSPPF(ALPHAL,NU,ALCL)
        ALPHAU=1.0 - (ALPHAT/2.0)
        CALL CHSPPF(ALPHAU,NU,AUCL)
        C3=C1/ALCL
        C4=C1/AUCL
        AMAT(ICNT,3)=MIN(C3,C4)
        AMAT(ICNT,4)=MAX(C3,C4)
        IDIGI2(ICNT,6)=-1
        IDIGI2(ICNT,7)=-1
!
        IWHTML(1)=150
        IWHTML(2)=125
        IWHTML(3)=125
        IWHTML(4)=125
        IWHTML(5)=125
        IWHTML(6)=125
        IWHTML(7)=125
        IINC=1800
        IINC2=1600
        IWRTF(1)=IINC
        IWRTF(2)=IWRTF(1)+IINC2
        IWRTF(3)=IWRTF(2)+IINC2
        IWRTF(4)=IWRTF(3)+IINC2
        IWRTF(5)=IWRTF(4)+IINC2
        IWRTF(6)=IWRTF(5)+IINC2
        IWRTF(7)=IWRTF(6)+IINC2
!
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
        GO TO 9000
      ENDIF
!
!     FOLLOWING TABLES ARE GENERATED FOR FIXED EFFECTS
!
!       1. EFFECT ESTIMATES
!
!       2. MODEL RESIDUAL STANDARD DEVIATIONS
!
!       3. CONFIDENCE INTERVAL FOR TREATMENT MEANS
!
!              Ybar(i) +/- t(1-alpha/2,N-k)*SQRT(MSE/n(i))
!
!       4. CONFIDENCE INTERVAL FOR PAIRWISE DIFFERENCES
!          BETWEEN TREATMENT MEANS
!
!       5. PAIRWISE MULTIPLE COMPARISONS (BONFERRONI OR
!          OR FISHER LEAST SIGNIFICANT DIFFERENCE, TUKEY
!          HONEST SIGNIFICANT DIFFERENCE)
!
!     EFFECT ESTIMATES
!
      ALPHAT=ALPHA
      IF(ALPHAT.GE.1.0 .AND. ALPHAT.LT.100.0)ALPHAT=ALPHAT/100.0
      IF(ALPHAT.LE.0.0 .OR. ALPHAT.GE.1.0)THEN
        ALPHAT=0.05
      ELSEIF(ALPHAT.GT.0.5)THEN
        ALPHAT=1.0 - ALPHAT
      ENDIF
      ADF=REAL(NTOTAL-NGROUP)
      CALL TPPF(ALPHAT,ADF,TVAL)
      TVAL=ABS(TVAL)
!
      ITITLE=' '
      ITITLE(1:25)='Estimation: Grand Mean = '
      WRITE(ITITLE(26:40),'(E15.7)')YGRAND
      NCTITL=40
      ITITL9=' '
      NCTIT9=0
!
      DO 3001 II=1,3
        DO 3003 JJ=1,7
          ITITL2(II,JJ)=' '
          NCTIT2(II,JJ)=0
 3003  CONTINUE
 3001 CONTINUE
!
      WRITE(IOUNI1,3008)
 3008 FORMAT('Level-ID          NI           MEAN',   &
             '         EFFECT     SD(EFFECT)')
!
      ITITL2(1,1)='Level'
      NCTIT2(1,1)=5
      ITITL2(2,1)='ID'
      NCTIT2(2,1)=2
      ITITL2(2,2)='N(i)'
      NCTIT2(2,2)=4
      ITITL2(2,3)='Mean'
      NCTIT2(2,3)=4
      ITITL2(2,4)='Effect'
      NCTIT2(2,4)=6
      ITITL2(2,5)='SD(Effect)'
      NCTIT2(2,5)=10
!
      NMAX=0
      NUMCOL=5
      NUMLIN=2
      DO 3010 I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        ITYPCO(I)='NUME'
        IDIGIT(I)=NUMDIG
        NTOT(I)=15
        IF(I.EQ.1 .OR. I.EQ.2)THEN
          NTOT(I)=5
          IDIGIT(I)=0
        ENDIF
        NMAX=NMAX+NTOT(I)
 3010 CONTINUE
      IWHTML(1)=50
      IWHTML(2)=50
      IWHTML(3)=150
      IWHTML(4)=150
      IWHTML(5)=150
      IINC=1600
      IINC2=200
      IINC3=1000
      IWRTF(1)=IINC2
      IWRTF(2)=IWRTF(1)+IINC2
      IWRTF(3)=IWRTF(2)+IINC
      IWRTF(4)=IWRTF(3)+IINC
      IWRTF(5)=IWRTF(4)+IINC
!
      ICNT=0
      DO 3020 I=1,NGROUP
!
        IF(ICNT.GE.MAXROW)THEN
          IFRST=.TRUE.
          ILAST=.TRUE.
          IFLAGS=.TRUE.
          IFLAGE=.TRUE.
          CALL DPDTA5(ITITLE,NCTITL,   &
                      ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                      MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                      IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                      IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                      ICAPSW,ICAPTY,IFRST,ILAST,   &
                      IFLAGS,IFLAGE,   &
                      ISUBRO,IBUGA3,IERROR)
          ICNT=0
        ENDIF
!
                                                                                                                                  
        ICNT=ICNT+1
        DO 3025 JJ=1,NUMCOL
          IVALUE(ICNT,JJ)=' '
          NCVALU(ICNT,JJ)=0
 3025   CONTINUE
        NI=INT(YN(I)+0.1)
        ANI=REAL(NI)
        CONST=(1.0/ANI) - (1.0/REAL(NTOTAL))
!CCCC   TERM1=SQRT(MSE/ANI)
        TERM1=SQRT(CONST*MSE)
        AMAT(ICNT,1)=REAL(I)
        AMAT(ICNT,2)=ANI
        AMAT(ICNT,3)=YMEAN(I)
        AMAT(ICNT,4)=YMEAN(I) - YGRAND
        AMAT(ICNT,5)=TERM1
        WRITE(IOUNI1,'(F8.0,F12.0,3E15.7)')REAL(I),ANI,YMEAN(I),   &
                                           AMAT(ICNT,4),AMAT(ICNT,5)
 3020 CONTINUE
!
      IF(ICNT.GE.1)THEN
        IFRST=.TRUE.
        ILAST=.TRUE.
        IFLAGS=.TRUE.
        IFLAGE=.TRUE.
        CALL DPDTA5(ITITLE,NCTITL,   &
                    ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                    MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                    IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                    IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                    ICAPSW,ICAPTY,IFRST,ILAST,   &
                    IFLAGS,IFLAGE,   &
                    ISUBRO,IBUGA3,IERROR)
      ENDIF
!
!     RESIDUAL STANDARD DEVIATIONS FROM MODELS (RAW DATA CASE)
!
      IF(ICASE1.EQ.'SUMM')GO TO 2699
      DSUM1=0.0D0
      DSUM2=0.0D0
      DO 2610 I=1,NTOTAL
        YVAL=Y(I)
        XVAL=X(I)
        IINDX=0
        DO 2620 J=1,NGROUP
          IF(XVAL.EQ.XDIST(J))THEN
            IINDX=J
            GO TO 2629
          ENDIF
 2620   CONTINUE
        GO TO 2699
!
 2629   CONTINUE
        PRED(I)=YMEAN(IINDX)
        RES(I)=YVAL - PRED(I)
        DSUM1=DSUM1 + (YVAL-YGRAND)**2
        DSUM2=DSUM2 + (YVAL-YMEAN(IINDX))**2
 2610 CONTINUE
!
      RESSD=REAL(DSQRT(DSUM2/DBLE(IDFE)))
      RESSD2=REAL(DSQRT(DSUM1/DBLE(NTOTAL-1)))
!
      ITITLE=' '
      ITITLE='Models'
      NCTITL=6
      ITITL9=' '
      NCTIT9=0
!
      DO 2630 II=1,3
        DO 2640 JJ=1,7
          ITITL2(II,JJ)=' '
          NCTIT2(II,JJ)=0
 2640  CONTINUE
 2630 CONTINUE
!
      ITITL2(1,1)='Model'
      NCTIT2(1,1)=5
      ITITL2(1,2)='Residual Standard Deviation'
      NCTIT2(1,2)=27
!
      NMAX=0
      NUMCOL=2
      NUMLIN=1
      DO 2650 I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        ITYPCO(I)='NUME'
        IDIGIT(I)=NUMDIG
        NTOT(I)=15
        IF(I.EQ.1)THEN
          NTOT(I)=25
          IDIGIT(I)=0
          ITYPCO(I)='ALPH'
          ALIGN(I)='l'
        ELSEIF(I.EQ.2)THEN
          NTOT(I)=30
        ENDIF
        NMAX=NMAX+NTOT(I)
 2650 CONTINUE
      IWHTML(1)=200
      IWHTML(2)=200
      IINC=1600
      IWRTF(1)=IINC
      IWRTF(2)=IWRTF(1)+IINC
!
      ICNT=1
      IVALUE(ICNT,1)='Constant'
      NCVALU(ICNT,1)=8
      AMAT(ICNT,1)=0.0
      AMAT(ICNT,2)=RESSD2
!
      ICNT=2
      IVALUE(ICNT,1)='Constant + Factor'
      NCVALU(ICNT,1)=17
      AMAT(ICNT,1)=0.0
      AMAT(ICNT,2)=RESSD
!
      IFRST=.TRUE.
      ILAST=.TRUE.
      IFLAGS=.TRUE.
      IFLAGE=.TRUE.
      CALL DPDTA5(ITITLE,NCTITL,   &
                  ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                  MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                  IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                  ICAPSW,ICAPTY,IFRST,ILAST,   &
                  IFLAGS,IFLAGE,   &
                  ISUBRO,IBUGA3,IERROR)
!
 2699 CONTINUE
!
!     CONFIDENCE INTERVALS FOR TREATMENT MEANS
!
      ALPHAT=ALPHA
      IF(ALPHAT.GE.1.0 .AND. ALPHAT.LT.100.0)ALPHAT=ALPHAT/100.0
      IF(ALPHAT.LE.0.0 .OR. ALPHAT.GE.1.0)THEN
        ALPHAT=0.05
      ELSEIF(ALPHAT.GT.0.5)THEN
        ALPHAT=1.0 - ALPHAT
      ENDIF
      ADF=REAL(NTOTAL-NGROUP)
      CALL TPPF(ALPHAT,ADF,TVAL)
      TVAL=ABS(TVAL)
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ANO4')THEN
        WRITE(ICOUT,4006)ALPHA,ALPHAT
 4006   FORMAT('ALPHA,ALPHAT = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(ALPHAT.EQ.0.05)THEN
        ITITLE(1:44)='95% Confidence Intervals for Treatment Means'
        NCTITL=44
      ELSEIF(ALPHAT.EQ.0.10)THEN
        ITITLE(1:44)='90% Confidence Intervals for Treatment Means'
        NCTITL=44
      ELSEIF(ALPHAT.EQ.0.01)THEN
        ITITLE(1:44)='99% Confidence Intervals for Treatment Means'
        NCTITL=44
      ELSE
        ITITLE(1:46)='    % Confidence Intervals for Treatment Means'
        WRITE(ITITLE(1:4),'(F4.1)')ALPHAT
        NCTITL=46
      ENDIF
      ITITL9=' '
      NCTIT9=0
!
      DO 4001 II=1,3
        DO 4003 JJ=1,7
          ITITL2(II,JJ)=' '
          NCTIT2(II,JJ)=0
 4003  CONTINUE
 4001 CONTINUE
!
      WRITE(IOUNI2,4008)
 4008 FORMAT('    I           MEAN           SD',   &
             '    SAMPLE SIZE           LCL            UCL')
!
      ITITL2(3,1)='I'
      NCTIT2(3,1)=1
      ITITL2(2,2)='Treatment'
      NCTIT2(2,2)=9
      ITITL2(3,2)='Mean'
      NCTIT2(3,2)=4
      ITITL2(2,3)='Treatment'
      NCTIT2(2,3)=9
      ITITL2(3,3)='SD'
      NCTIT2(3,3)=2
      ITITL2(1,4)='Treatment'
      NCTIT2(1,4)=9
      ITITL2(2,4)='Sample'
      NCTIT2(2,4)=6
      ITITL2(3,4)='Size'
      NCTIT2(3,4)=4
      ITITL2(1,5)='Lower'
      NCTIT2(1,5)=5
      ITITL2(2,5)='Confidence'
      NCTIT2(2,5)=10
      ITITL2(3,5)='Limit'
      NCTIT2(3,5)=5
      ITITL2(1,6)='Upper'
      NCTIT2(1,6)=5
      ITITL2(2,6)='Confidence'
      NCTIT2(2,6)=10
      ITITL2(3,6)='Limit'
      NCTIT2(3,6)=5
!
      NMAX=0
      NUMCOL=6
      NUMLIN=3
      DO 4010 I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        ITYPCO(I)='NUME'
        IDIGIT(I)=NUMDIG
        NTOT(I)=15
        IF(I.EQ.1)THEN
          NTOT(I)=5
          IDIGIT(I)=0
        ELSEIF(I.EQ.4)THEN
          IDIGIT(I)=0
        ENDIF
        NMAX=NMAX+NTOT(I)
 4010 CONTINUE
      IWHTML(1)=50
      IWHTML(2)=150
      IWHTML(3)=150
      IWHTML(4)=150
      IWHTML(5)=150
      IWHTML(6)=150
      IINC=1600
      IINC2=200
      IINC3=1000
      IWRTF(1)=IINC2
      IWRTF(2)=IWRTF(1)+IINC
      IWRTF(3)=IWRTF(2)+IINC
      IWRTF(4)=IWRTF(3)+IINC
      IWRTF(5)=IWRTF(4)+IINC
      IWRTF(6)=IWRTF(5)+IINC
!
      ICNT=0
      DO 4020 I=1,NGROUP
!
        IF(ICNT.GE.MAXROW)THEN
          IFRST=.TRUE.
          ILAST=.TRUE.
          IFLAGS=.TRUE.
          IFLAGE=.TRUE.
          CALL DPDTA5(ITITLE,NCTITL,   &
                      ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                      MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                      IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                      IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                      ICAPSW,ICAPTY,IFRST,ILAST,   &
                      IFLAGS,IFLAGE,   &
                      ISUBRO,IBUGA3,IERROR)
          ICNT=0
        ENDIF
!
                                                                                                                                  
        ICNT=ICNT+1
        DO 4025 JJ=1,NUMCOL
          IVALUE(ICNT,JJ)=' '
          NCVALU(ICNT,JJ)=0
 4025   CONTINUE
        NI=INT(YN(I)+0.1)
        ANI=REAL(NI)
        TERM1=SQRT(MSE/ANI)
        AMAT(ICNT,1)=REAL(I)
        AMAT(ICNT,2)=YMEAN(I)
        AMAT(ICNT,3)=YSD(I)
        AMAT(ICNT,4)=ANI
        AMAT(ICNT,5)=YMEAN(I) - TVAL*TERM1
        AMAT(ICNT,6)=YMEAN(I) + TVAL*TERM1
        WRITE(IOUNI2,'(F5.0,5E15.7)')REAL(I),YMEAN(I),YSD(I),ANI,   &
                                     AMAT(ICNT,5),AMAT(ICNT,6)
 4020 CONTINUE
!
      IF(ICNT.GE.1)THEN
        IFRST=.TRUE.
        ILAST=.TRUE.
        IFLAGS=.TRUE.
        IFLAGE=.TRUE.
        CALL DPDTA5(ITITLE,NCTITL,   &
                    ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                    MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                    IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                    IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                    ICAPSW,ICAPTY,IFRST,ILAST,   &
                    IFLAGS,IFLAGE,   &
                    ISUBRO,IBUGA3,IERROR)
      ENDIF
!
!     CONFIDENCE INTERVALS FOR DIFFERENCES OF TREATMENT MEANS
!
      IF(ALPHAT.EQ.0.05)THEN
        ITITLE(1:47)='95% Confidence Intervals for the Difference of '
        ITITLE(48:62)='Treatment Means'
        NCTITL=62
      ELSEIF(ALPHAT.EQ.0.10)THEN
        ITITLE(1:47)='90% Confidence Intervals for the Difference of '
        ITITLE(48:62)='Treatment Means'
        NCTITL=62
      ELSEIF(ALPHAT.EQ.0.01)THEN
        ITITLE(1:47)='99% Confidence Intervals for the Difference of '
        ITITLE(48:62)='Treatment Means'
        NCTITL=62
      ELSE
        ITITLE(1:49)='    % Confidence Intervals for the Difference of '
        ITITLE(50:64)='Treatment Means'
        NCTITL=64
        WRITE(ITITLE(1:4),'(F4.1)')ALPHAT
      ENDIF
      ITITL9=' '
      NCTIT9=0
!
      DO 5001 II=1,3
        DO 5003 JJ=1,7
          ITITL2(II,JJ)=' '
          NCTIT2(II,JJ)=0
 5003  CONTINUE
 5001 CONTINUE
!
      WRITE(IOUNI3,5008)
 5008 FORMAT('    I    J XBAR(I)-XBAR(J)           LCL',   &
             '            UCL        P-VALUE')
!
      ITITL2(3,1)='I'
      NCTIT2(3,1)=1
      ITITL2(3,2)='J'
      NCTIT2(3,2)=1
      ITITL2(1,3)='Difference of'
      NCTIT2(1,3)=13
      ITITL2(2,3)='Treatment'
      NCTIT2(2,3)=9
      ITITL2(3,3)='Means'
      NCTIT2(3,3)=5
      ITITL2(1,4)='Lower'
      NCTIT2(1,4)=5
      ITITL2(2,4)='Confidence'
      NCTIT2(2,4)=10
      ITITL2(3,4)='Limit'
      NCTIT2(3,4)=5
      ITITL2(1,5)='Upper'
      NCTIT2(1,5)=5
      ITITL2(2,5)='Confidence'
      NCTIT2(2,5)=10
      ITITL2(3,5)='Limit'
      NCTIT2(3,5)=5
      ITITL2(1,6)='Fisher'
      NCTIT2(1,6)=6
      ITITL2(2,6)='LSD'
      NCTIT2(2,6)=3
      ITITL2(3,6)='P-Value'
      NCTIT2(3,6)=7
!
      NMAX=0
      NUMCOL=6
      NUMLIN=3
      DO 5010 I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        ITYPCO(I)='NUME'
        IDIGIT(I)=NUMDIG
        NTOT(I)=15
        IF(I.EQ.1 .OR. I.EQ.2)THEN
          NTOT(I)=5
          IDIGIT(I)=0
        ENDIF
        NMAX=NMAX+NTOT(I)
 5010 CONTINUE
      IWHTML(1)=50
      IWHTML(2)=50
      IWHTML(3)=150
      IWHTML(4)=150
      IWHTML(5)=150
      IWHTML(6)=150
      IINC=1600
      IINC2=200
      IINC3=1000
      IWRTF(1)=IINC2
      IWRTF(2)=IWRTF(1)+IINC2
      IWRTF(3)=IWRTF(2)+IINC
      IWRTF(4)=IWRTF(3)+IINC
      IWRTF(5)=IWRTF(4)+IINC
      IWRTF(6)=IWRTF(5)+IINC
!
      ADF=REAL(NTOTAL-NGROUP)
      CALL TPPF(ALPHAT,ADF,TVAL)
      TVAL=ABS(TVAL)
      ICNT=0
      DO 5081 I=1,NGROUP
        DO 5083 J=1,NGROUP
          IF(I.LT.J)THEN
!
            IF(ICNT.GE.MAXROW)THEN
              IFRST=.TRUE.
              ILAST=.TRUE.
              IFLAGS=.TRUE.
              IFLAGE=.TRUE.
              CALL DPDTA5(ITITLE,NCTITL,   &
                          ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                          MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                          IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                          IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                          ICAPSW,ICAPTY,IFRST,ILAST,   &
                          IFLAGS,IFLAGE,   &
                          ISUBRO,IBUGA3,IERROR)
              ICNT=0
            ENDIF
!
            ICNT=ICNT+1
            IVALUE(ICNT,1)=' '
            NCVALU(ICNT,1)=0
            IVALUE(ICNT,2)=' '
            NCVALU(ICNT,2)=0
            IVALUE(ICNT,3)=' '
            NCVALU(ICNT,3)=0
            IVALUE(ICNT,4)=' '
            NCVALU(ICNT,4)=0
            IVALUE(ICNT,5)=' '
            NCVALU(ICNT,5)=0
            IVALUE(ICNT,6)=' '
            NCVALU(ICNT,6)=0
            AMAT(ICNT,1)=REAL(I)
            AMAT(ICNT,2)=REAL(J)
            ADIFF=YMEAN(I) - YMEAN(J)
            AMAT(ICNT,3)=ADIFF
            NII=INT(YN(I)+0.1)
            NIJ=INT(YN(J)+0.1)
            TERM1=MSE*(1.0/REAL(NII) + 1.0/REAL(NIJ))
            AMAT(ICNT,4)=ADIFF - TVAL*SQRT(TERM1)
            AMAT(ICNT,5)=ADIFF + TVAL*SQRT(TERM1)
            STATVA=ADIFF/SQRT(TERM1)
            CALL TCDF(STATVA,ADF,STATCD)
            IF(STATVA.LE.0.0)THEN
              PVAL=2.0*STATCD
            ELSE
              PVAL=2.0*(1.0 - STATCD)
            ENDIF
            AMAT(ICNT,6)=PVAL
            WRITE(IOUNI3,'(2F5.0,4E15.7)')AMAT(ICNT,1),AMAT(ICNT,2),   &
                  AMAT(ICNT,3),AMAT(ICNT,4),AMAT(ICNT,5),AMAT(ICNT,6)
          ENDIF
 5083   CONTINUE
 5081 CONTINUE
!
      IF(ICNT.GE.1)THEN
        IFRST=.TRUE.
        ILAST=.TRUE.
        IFLAGS=.TRUE.
        IFLAGE=.TRUE.
        CALL DPDTA5(ITITLE,NCTITL,   &
                    ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                    MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                    IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                    IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                    ICAPSW,ICAPTY,IFRST,ILAST,   &
                    IFLAGS,IFLAGE,   &
                    ISUBRO,IBUGA3,IERROR)
      ENDIF
!
!     ALWAYS GENERATE BONFERRONI INTERVAL
!
!CCCC IF(IANOMC.EQ.'BONF')THEN
!
!        BONFERRONI MULTIPLE COMPARISONS
!
        ALPHAT=ALPHA
        IF(ALPHAT.GE.1.0 .AND. ALPHAT.LT.100.0)ALPHAT=ALPHAT/100.0
        IF(ALPHAT.LE.0.0 .OR. ALPHAT.GE.100.0)THEN
          ALPHAT=0.05
        ELSEIF(ALPHAT.GT.0.5)THEN
          ALPHAT=1.0 - ALPHAT
        ENDIF
        AK=REAL(NGROUP)
        DENOM=AK*(AK-1.0)/2.0
        ALPHAP=ALPHAT/DENOM
        ADF=REAL(NTOTAL-NGROUP)
        CALL TPPF(ALPHAP,ADF,TVALB)
        TVALB=ABS(TVALB)
!
        IF(ALPHAT.EQ.0.05)THEN
          ITITLE(1:47)='95% Bonferroni Intervals for the Difference of '
          ITITLE(48:62)='Treatment Means'
          NCTITL=62
        ELSEIF(ALPHAT.EQ.0.10)THEN
          ITITLE(1:47)='90% Bonferroni Intervals for the Difference of '
          ITITLE(48:62)='Treatment Means'
          NCTITL=62
        ELSEIF(ALPHAT.EQ.0.01)THEN
          ITITLE(1:47)='99% Bonferroni Intervals for the Difference of '
          ITITLE(48:62)='Treatment Means'
          NCTITL=62
        ELSE
        ITITLE(1:49)='    % Bonferroni Intervals for the Difference of '
          ITITLE(50:64)='Treatment Means'
          NCTITL=64
          WRITE(ITITLE(1:4),'(F4.1)')ALPHAT
        ENDIF
        ITITL9(1:28)='Bonferroni Adjusted Alpha = '
        WRITE(ITITL9(29:34),'(F6.4)')ALPHAP
        NCTIT9=34
!
        DO 6001 II=1,3
          DO 6003 JJ=1,7
            ITITL2(II,JJ)=' '
            NCTIT2(II,JJ)=0
 6003  CONTINUE
 6001 CONTINUE
!
        WRITE(IOUNI4,6008)
 6008   FORMAT('    I    J XBAR(I)-XBAR(J)           LCL',   &
               '            UCL        P-VALUE')
!
        ITITL2(1,1)='I'
        NCTIT2(1,1)=1
        ITITL2(1,2)='J'
        NCTIT2(1,2)=1
        ITITL2(1,3)='Difference of'
        NCTIT2(1,3)=13
        ITITL2(2,3)='Treatment'
        NCTIT2(2,3)=9
        ITITL2(3,3)='Means'
        NCTIT2(3,3)=5
        ITITL2(1,4)='Lower'
        NCTIT2(1,4)=5
        ITITL2(2,4)='Confidence'
        NCTIT2(2,4)=10
        ITITL2(3,4)='Limit'
        NCTIT2(3,4)=5
        ITITL2(1,5)='Upper'
        NCTIT2(1,5)=5
        ITITL2(2,5)='Confidence'
        NCTIT2(2,5)=10
        ITITL2(3,5)='Limit'
        NCTIT2(3,5)=5
        ITITL2(3,6)='P-Value'
        NCTIT2(3,6)=7
!
        NMAX=0
        NUMCOL=6
        NUMLIN=3
        DO 6010 I=1,NUMCOL
          VALIGN(I)='b'
          ALIGN(I)='r'
          ITYPCO(I)='NUME'
          IDIGIT(I)=NUMDIG
          NTOT(I)=15
          IF(I.EQ.1 .OR. I.EQ.2)THEN
            NTOT(I)=5
            IDIGIT(I)=0
          ENDIF
          NMAX=NMAX+NTOT(I)
 6010 CONTINUE
        IWHTML(1)=50
        IWHTML(2)=50
        IWHTML(3)=150
        IWHTML(4)=150
        IWHTML(5)=150
        IWHTML(6)=150
        IINC=1600
        IINC2=200
        IINC3=1000
        IWRTF(1)=IINC2
        IWRTF(2)=IWRTF(1)+IINC2
        IWRTF(3)=IWRTF(2)+IINC
        IWRTF(4)=IWRTF(3)+IINC
        IWRTF(5)=IWRTF(4)+IINC
        IWRTF(6)=IWRTF(5)+IINC
!
        ICNT=0
        DO 6081 I=1,NGROUP
          DO 6083 J=1,NGROUP
            IF(I.LT.J)THEN
!
              IF(ICNT.GE.MAXROW)THEN
                IFRST=.TRUE.
                ILAST=.TRUE.
                IFLAGS=.TRUE.
                IFLAGE=.TRUE.
                CALL DPDTA5(ITITLE,NCTITL,   &
                            ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                            MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                            IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                            ICAPSW,ICAPTY,IFRST,ILAST,   &
                            IFLAGS,IFLAGE,   &
                            ISUBRO,IBUGA3,IERROR)
                ICNT=0
              ENDIF
!
              ICNT=ICNT+1
              IVALUE(ICNT,1)=' '
              NCVALU(ICNT,1)=0
              IVALUE(ICNT,2)=' '
              NCVALU(ICNT,2)=0
              IVALUE(ICNT,3)=' '
              NCVALU(ICNT,3)=0
              IVALUE(ICNT,4)=' '
              NCVALU(ICNT,4)=0
              IVALUE(ICNT,5)=' '
              NCVALU(ICNT,5)=0
              IVALUE(ICNT,6)=' '
              NCVALU(ICNT,6)=0
              AMAT(ICNT,1)=REAL(I)
              AMAT(ICNT,2)=REAL(J)
              ADIFF=YMEAN(I) - YMEAN(J)
              AMAT(ICNT,3)=ADIFF
              NII=INT(YN(I)+0.1)
              NIJ=INT(YN(J)+0.1)
              TERM1=MSE*(1.0/REAL(NII) + 1.0/REAL(NIJ))
              AMAT(ICNT,4)=ADIFF - TVALB*SQRT(TERM1)
              AMAT(ICNT,5)=ADIFF + TVALB*SQRT(TERM1)
              STATVA=ADIFF/SQRT(TERM1)
              CALL TCDF(STATVA,ADF,STATCD)
              IF(STATVA.LE.0.0)THEN
                PVAL=2.0*STATCD
              ELSE
                PVAL=2.0*(1.0 - STATCD)
              ENDIF
              AMAT(ICNT,6)=PVAL
              WRITE(IOUNI4,'(2F5.0,4E15.7)')AMAT(ICNT,1),AMAT(ICNT,2),   &
                    AMAT(ICNT,3),AMAT(ICNT,4),AMAT(ICNT,5),AMAT(ICNT,6)
            ENDIF
 6083     CONTINUE
 6081   CONTINUE
!
        IF(ICNT.GE.1)THEN
          IFRST=.TRUE.
          ILAST=.TRUE.
          IFLAGS=.TRUE.
          IFLAGE=.TRUE.
          CALL DPDTA5(ITITLE,NCTITL,   &
                    ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                    MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                    IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                    IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                    ICAPSW,ICAPTY,IFRST,ILAST,   &
                    IFLAGS,IFLAGE,   &
                    ISUBRO,IBUGA3,IERROR)
         ENDIF
!
!     ALWAYS GENERATE TUKEY-KRAMER MULTIPLE COMPARISONS
!
!CCCC ELSEIF(IANOMC.EQ.'TUKE')THEN
!
!       TUKEY-KRAMER MULTIPLE COMPARISONS
!
        ALPHAT=ALPHA
        IF(ALPHAT.GE.1.0 .AND. ALPHAT.LT.100.0)ALPHAT=ALPHAT/100.0
        IF(ALPHAT.LE.0.0 .OR. ALPHAT.GE.100.0)THEN
          ALPHAT=0.05
        ELSEIF(ALPHAT.GT.0.5)THEN
          ALPHAT=1.0 - ALPHAT
        ENDIF
        IF(ALPHAT.NE.0.10 .AND. ALPHAT.NE.0.05 .AND.   &
           ALPHAT.NE.0.01)ALPHAT=0.05
                                                                                                                                  
        ALPHA1=0.90
        ALPHA2=0.95
        ALPHA3=0.99
        DTERM1=QTRNG(DBLE(ALPHA1),DBLE(NGROUP*(NTOTAL-1)),   &
                     DBLE(NGROUP),IFAULT)
        CV90T=REAL(DTERM1)
        DTERM1=QTRNG(DBLE(ALPHA2),DBLE(NGROUP*(NTOTAL-1)),   &
                     DBLE(NGROUP),IFAULT)
        CV95T=REAL(DTERM1)
        DTERM1=QTRNG(DBLE(ALPHA3),DBLE(NGROUP*(NTOTAL-1)),   &
                     DBLE(NGROUP),IFAULT)
        CV99T=REAL(DTERM1)
!
        ITITLE='Tukey-Kramer Honest Significant Difference (HSD)'
        NCTITL=48
        ITITL9='Multiple Comparisons: alpha = '
        WRITE(ITITL9(31:34),'(F4.2)')ALPHAT
        NCTIT9=34
!
        DO 7001 II=1,3
          DO 7003 JJ=1,7
            ITITL2(II,JJ)=' '
            NCTIT2(II,JJ)=0
 7003     CONTINUE
 7001   CONTINUE
!
        WRITE(IOUNI5,7008)
 7008   FORMAT('    I    J |XBAR(I)-XBAR(J)|      STDERR',   &
               '            LCL            UCL         90% CV',   &
               '         95% CV         99% CV')
        ITITL2(3,1)='I'
        NCTIT2(3,1)=1
        ITITL2(3,2)='J'
        NCTIT2(3,2)=1
        ITITL2(1,3)='|Difference|'
        NCTIT2(1,3)=12
        ITITL2(2,3)='of Treatment'
        NCTIT2(2,3)=12
        ITITL2(3,3)='Means'
        NCTIT2(3,3)=5
        ITITL2(2,4)='Standard'
        NCTIT2(2,4)=8
        ITITL2(3,4)='Error'
        NCTIT2(3,4)=5
        ITITL2(1,5)='Lower'
        NCTIT2(1,5)=5
        ITITL2(2,5)='Confidence'
        NCTIT2(2,5)=10
        ITITL2(3,5)='Limit'
        NCTIT2(3,5)=5
        ITITL2(1,6)='Upper'
        NCTIT2(1,6)=5
        ITITL2(2,6)='Confidence'
        NCTIT2(2,6)=10
        ITITL2(3,6)='Limit'
        NCTIT2(3,6)=5
!
        NMAX=0
        NUMCOL=6
        NUMLIN=3
        DO 7010 I=1,NUMCOL
          VALIGN(I)='b'
          ALIGN(I)='r'
          ITYPCO(I)='NUME'
          IDIGIT(I)=NUMDIG
          NTOT(I)=15
          IF(I.EQ.1 .OR. I.EQ.2)THEN
            NTOT(I)=5
            IDIGIT(I)=0
          ENDIF
          NMAX=NMAX+NTOT(I)
 7010 CONTINUE
        IWHTML(1)=50
        IWHTML(2)=50
        IWHTML(3)=150
        IWHTML(4)=150
        IWHTML(5)=150
        IWHTML(6)=150
        IINC=1600
        IINC2=200
        IINC3=1000
        IWRTF(1)=IINC2
        IWRTF(2)=IWRTF(1)+IINC2
        IWRTF(3)=IWRTF(2)+IINC
        IWRTF(4)=IWRTF(3)+IINC
        IWRTF(5)=IWRTF(4)+IINC
        IWRTF(6)=IWRTF(5)+IINC
!
        ICNT=0
        DO 7081 I=1,NGROUP
          DO 7083 J=1,NGROUP
            IF(I.LT.J)THEN
!
              IF(ICNT.GE.MAXROW)THEN
                IFRST=.TRUE.
                ILAST=.TRUE.
                IFLAGS=.TRUE.
                IFLAGE=.TRUE.
                CALL DPDTA5(ITITLE,NCTITL,   &
                            ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                            MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                            IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                            ICAPSW,ICAPTY,IFRST,ILAST,   &
                            IFLAGS,IFLAGE,   &
                            ISUBRO,IBUGA3,IERROR)
                ICNT=0
              ENDIF
!
              ICNT=ICNT+1
              IVALUE(ICNT,1)=' '
              NCVALU(ICNT,1)=0
              IVALUE(ICNT,2)=' '
              NCVALU(ICNT,2)=0
              IVALUE(ICNT,3)=' '
              NCVALU(ICNT,3)=0
              IVALUE(ICNT,4)=' '
              NCVALU(ICNT,4)=0
              IVALUE(ICNT,5)=' '
              NCVALU(ICNT,5)=0
              IVALUE(ICNT,6)=' '
              NCVALU(ICNT,6)=0
              AMAT(ICNT,1)=REAL(I)
              AMAT(ICNT,2)=REAL(J)
              ADIFF=ABS(YMEAN(I) - YMEAN(J))
              AMAT(ICNT,3)=ADIFF
              NII=INT(YN(I)+0.1)
              NIJ=INT(YN(J)+0.1)
              TERM1=(MSE/2.0)*(1.0/REAL(NII) + 1.0/REAL(NIJ))
              TERM1=SQRT(TERM1)
              AMAT(ICNT,4)=TERM1
              IF(ALPHAT.EQ.0.10)THEN
                AMAT(ICNT,5)=ADIFF - CV90T*TERM1
                AMAT(ICNT,6)=ADIFF + CV90T*TERM1
              ELSEIF(ALPHAT.EQ.0.05)THEN
                AMAT(ICNT,5)=ADIFF - CV95T*TERM1
                AMAT(ICNT,6)=ADIFF + CV95T*TERM1
              ELSEIF(ALPHAT.EQ.0.01)THEN
                AMAT(ICNT,5)=ADIFF - CV99T*TERM1
                AMAT(ICNT,6)=ADIFF + CV99T*TERM1
              ENDIF
              WRITE(IOUNI5,'(2F5.0,7E15.7)')REAL(NII),REAL(NIJ),   &
                    ADIFF,TERM1,AMAT(ICNT,5),AMAT(ICNT,6),CV90T*TERM1,   &
                    CV95T*TERM1,CV99T*TERM1
            ENDIF
 7083     CONTINUE
 7081   CONTINUE
!
        IF(ICNT.GE.1)THEN
          IFRST=.TRUE.
          ILAST=.TRUE.
          IFLAGS=.TRUE.
          IFLAGE=.TRUE.
          CALL DPDTA5(ITITLE,NCTITL,   &
                    ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                    MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                    IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                    IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                    ICAPSW,ICAPTY,IFRST,ILAST,   &
                    IFLAGS,IFLAGE,   &
                    ISUBRO,IBUGA3,IERROR)
         ENDIF
!CCCC  ENDIF
!
!               **************************************
!               **  STEP 13--                       **
!               **  CLOSE       THE STORAGE FILES.  **
!               **************************************
!
      ISTEPN='13'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ANO4')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IOP='CLOS'
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                  IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ANO4')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPANO4--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
        IF(ICASE1.EQ.'RAW ')THEN
          DO 9020 I=1,NTOTAL
            WRITE(ICOUT,9025)I,Y(I),PRED(I),RES(I)
 9025       FORMAT('I,Y(I),PRED(I),RES(I) = ',I8,3G15.7)
            CALL DPWRST('XXX','BUG ')
 9020     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPANO4
      SUBROUTINE DPANOL(IHARG,IARGT,ARG,NUMARG,DEFAL1,DEFAL2,   &
      ANOPL1,ANOPL2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE ANOP LIMITS
!              (THE PROPORTION LIMITS ARE THE SAME AS THE ANOP LIMITS).
!              WHICH DEFINE THE TARGET INTERVAL OF INTEREST
!              IN THE ANOP PROCEDURE AND THE ANOP PLOT
!              (AND IN THE PROPORTION PLOT).
!              THE SPECIFIED LIMITS WILL BE PLACED
!              IN THE FLOATING POINT VARIABLE ANOPL1 AND ANOPL2.
                                                                                                                                  
!                     --IARGT  (A  HOLLERITH VECTOR)
!                     --ARG    (A  FLOATING POINT VECTOR)
!                     --NUMARG (AN INTEGER VARIABLE)
!     OUTPUT ARGUMENTS--DEFAL1 = A FLOATING POINT VARIABLE
!                                CONTAINING THE LOWER LIMIT
!                                OF THE INTERVAL OF INTEREST.
!                     --DEFAL2 = A FLOATING POINT VARIABLE
!                                CONTAINING THE UPPER LIMIT
!                                OF THE INTERVAL OF INTEREST.
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
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--APRIL     1982.
!     UPDATED         --MAY       1982.
!     UPDATED         --SEPTEMBER 1988.
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
      IF(NUMARG.LE.1)GO TO 1150
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1150
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      IF(IHARG(NUMARG).EQ.'?')GO TO 8100
      NUMAM1=NUMARG-1
      IF(NUMAM1.GE.2.AND.IARGT(NUMAM1).EQ.'NUMB'.AND.   &
      IARGT(NUMARG).EQ.'NUMB')GO TO 1160
      GO TO 1120
!
 1120 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,1121)
 1121 FORMAT('***** ERROR IN DPANOL--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR ANOP LIMITS ',   &
      'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1124)
 1124 FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE ',   &
      'PROPER FORM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1125)
 1125 FORMAT('      SUPPOSE THE ANALYST DESIRES THE  ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1126)
 1126 FORMAT('      INTERVAL OF INTEREST IN AN ANOP OR ANOP PLOT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1127)
 1127 FORMAT('      TO BE 120 TO 1000')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1129)
 1129 FORMAT('      THEN THE ALLOWABLE FORM IS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1130)
 1130 FORMAT('      ANOP LIMITS 120 1000 ')
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
!
 1150 CONTINUE
      HOLD1=DEFAL1
      HOLD2=DEFAL2
      GO TO 1180
!
 1160 CONTINUE
      HOLD1=ARG(NUMAM1)
      HOLD2=ARG(NUMARG)
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
      ANOPL1=HOLD1
      ANOPL2=HOLD2
      IF(HOLD1.GT.HOLD2)ANOPL1=HOLD2
      IF(HOLD1.GT.HOLD2)ANOPL2=HOLD1
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)ANOPL1,ANOPL2
 1181 FORMAT('THE PROPORTION/ANOP LIMITS HAS JUST BEEN SET TO ',   &
      E15.7,' AND ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 9000
!
 8100 CONTINUE
      IFOUND='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8111)ANOPL1,ANOPL2
 8111 FORMAT('THE CURRENT PROPORTION/ANOP LIMITS ARE ',   &
      E15.7,' AND ',E15.7)
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE DPANOL
      SUBROUTINE DPANOV(ICAPSW,IFORSW,   &
                        IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--CARRY OUT AN ANALYSIS OF VARIANCE.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--APRIL     1978.
!     UPDATED         --JULY      1978.
!     UPDATED         --NOVEMBER  1978.
!     UPDATED         --FEBRUARY  1981.
!     UPDATED         --JULY      1981.
!     UPDATED         --SEPTEMBER 1981.
!     UPDATED         --DECEMBER  1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --MARCH     1988. ADD LOFCDF
!     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE
!                                       COMMON
!     UPDATED         --FEBRUARY  1998. SLIGHT RECODING FOR BETTER
!                                       EFFICIENCY IN DPANO2
!                                       TO INCREASE MAXIMUM ALLOWED
!                                       NUMBER OF FACTORS, ONLY HAVE
!                                       TO CHANGE VALUE OF MAXFAC AND
!                                       ONE BLOCK OF CODE (STEP 3.5)
!                                       IN DPANO2.
!     UPDATED         --OCTOBER   2003. SUPPORT FOR HTML, LATEX OUTPUT
!     UPDATED         --MAY       2011. IFORSW
!     UPDATED         --JULY      2019. TWEAK USE OF SCRATCH STORAGE
!     UPDATED         --SEPTEMBER 2023. ADD:
!                                          ONE WAY ANOVA Y X
!                                          ONE WAY RANDOM EFFECTS ANOVA Y X
!                                          ONE WAY SUMMARY ANOVA YMEAN YSD NI
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
      CHARACTER*4 IREPU
      CHARACTER*4 IRESU
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN0
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 ICASE2
      CHARACTER*4 ICASE3
      CHARACTER*4 ICASE4
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
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
!CCCC FOLLOWING SECTION MODIFIED FEBRUARY 1998.
      PARAMETER (MAXLEV=500)
      PARAMETER (MAXFAC=10)
!
      DIMENSION F1(MAXOBV,MAXFAC)
!CCCC DIMENSION F1(MAXOBV)
!CCCC DIMENSION F2(MAXOBV)
!CCCC DIMENSION F3(MAXOBV)
!CCCC DIMENSION F4(MAXOBV)
!CCCC DIMENSION F5(MAXOBV)
      DIMENSION F1ID(MAXLEV,MAXFAC)
      DIMENSION F1N(MAXLEV,MAXFAC)
      DIMENSION F1MEAN(MAXLEV,MAXFAC)
      DIMENSION F1EFFE(MAXLEV,MAXFAC)
      DIMENSION F1EFSD(MAXLEV,MAXFAC)
      DIMENSION CELLME(MAXOBV)
      DIMENSION CELLV(MAXOBV)
      DIMENSION CELLN(MAXOBV)
      DIMENSION AINN(MAXOBV)
      DIMENSION AINMEA(MAXOBV)
      DIMENSION AINEFF(MAXOBV)
      DIMENSION AINESD(MAXOBV)
!
      DIMENSION PRED2(MAXOBV)
      DIMENSION RES2(MAXOBV)
      DIMENSION W(MAXOBV)
      DIMENSION Z(MAXOBV)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TEMP3(MAXOBV)
      DIMENSION TEMP4(MAXOBV)
!
      DIMENSION B(500)
      DIMENSION SDB(500)
      DIMENSION FCUM(500)
      DIMENSION N1(MAXFAC)
      DIMENSION ISET(MAXFAC)
      DIMENSION AN1(MAXFAC)
      DIMENSION E1(MAXFAC)
!
      DIMENSION SS1(500)
      DIMENSION RESMS1(500)
      DIMENSION FVAL(500)
      DIMENSION F1CDF2(500)
      DIMENSION RSD(500)
!
!CCCC FOLLOWING LINES ADDED JUNE, 1990
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(IGARB1),AN1(1))
      EQUIVALENCE (GARBAG(IGARB1+1000),E1(1))
      EQUIVALENCE (GARBAG(IGARB1+2000),SS1(1))
      EQUIVALENCE (GARBAG(IGARB1+3000),RESMS1(1))
      EQUIVALENCE (GARBAG(IGARB1+4000),FVAL(1))
      EQUIVALENCE (GARBAG(IGARB1+5000),F1CDF2(1))
      EQUIVALENCE (GARBAG(IGARB1+6000),RSD(1))
      EQUIVALENCE (GARBAG(IGARB1+7000),B(1))
      EQUIVALENCE (GARBAG(IGARB1+8000),SDB(1))
      EQUIVALENCE (GARBAG(IGARB1+9000),FCUM(1))
      EQUIVALENCE (GARBAG(IGARB2),F1ID(1,1))
      EQUIVALENCE (GARBAG(IGARB2+10000),F1N(1,1))
      EQUIVALENCE (GARBAG(IGARB2+20000),F1MEAN(1,1))
      EQUIVALENCE (GARBAG(IGARB2+30000),F1EFFE(1,1))
      EQUIVALENCE (GARBAG(IGARB2+40000),F1EFSD(1,1))
      EQUIVALENCE (GARBAG(IGARB3),PRED2(1))
      EQUIVALENCE (GARBAG(IGARB4),RES2(1))
      EQUIVALENCE (GARBAG(IGARB5),Z(1))
      EQUIVALENCE (GARBAG(IGARB6),W(1))
      EQUIVALENCE (GARBAG(IGARB7),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB8),TEMP2(1))
      EQUIVALENCE (GARBAG(IGAR10),TEMP3(1))
      EQUIVALENCE (GARBAG(JGAR11),TEMP4(1))
      EQUIVALENCE (GARBAG(JGAR12),CELLME(1))
      EQUIVALENCE (GARBAG(JGAR13),CELLV(1))
      EQUIVALENCE (GARBAG(JGAR14),CELLN(1))
      EQUIVALENCE (GARBAG(JGAR15),AINN(1))
      EQUIVALENCE (GARBAG(JGAR16),AINMEA(1))
      EQUIVALENCE (GARBAG(JGAR17),AINEFF(1))
      EQUIVALENCE (GARBAG(JGAR18),AINESD(1))
      EQUIVALENCE (GARBAG(JGAR19),F1(1,1))
!CCCC END CHANGE
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOST.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPAN'
      ISUBN2='OV  '
      IERROR='NO'
      IFOUND='NO'
      ICASE2='KWAY'
      ICASE3='FIXE'
      ICASE4='RAW '
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
!               *******************************************
!               **  TREAT THE ANALYSIS OF VARIANCE CASE  **
!               *******************************************
!
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'ANOV')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPANOV--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA2,IBUGA3,IBUGQ,ISUBRO
   52   FORMAT('IBUGA2,IBUGA3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ***************************
!               **  STEP 1--             **
!               **  EXTRACT THE COMMAND  **
!               ***************************
!
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'ANOV')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICOM.EQ.'LET ')GO TO 9000
      IF(ICOM.EQ.'ANOV'.AND.ICOM2.EQ.'A   ')THEN
        ILASTC=0
      ELSEIF(NUMARG.GE.2.AND.ICOM.EQ.'ANAL'.AND.   &
             IHARG(1).EQ.'OF  '.AND.IHARG(2).EQ.'VARI')THEN
        ILASTC=2
      ELSEIF(NUMARG.GE.1.AND.   &
            (ICOM.EQ.'ONE ' .OR. ICOM.EQ.'1   ') .AND.   &
            IHARG(1).EQ.'WAY ' .AND. IHARG(2).EQ.'ANOV')THEN
        ILASTC=2
        ICASE2='1WAY'
      ELSEIF(NUMARG.GE.3.AND.   &
            (ICOM.EQ.'ONE ' .OR. ICOM.EQ.'1   ') .AND.   &
             IHARG(1).EQ.'WAY '.AND.   &
             IHARG(2).EQ.'ANAL' .AND.IHARG(3).EQ.'OF  '.AND.   &
             IHARG(4).EQ.'VARI')THEN
        ILASTC=4
        ICASE2='1WAY'
!CCCC ELSEIF(NUMARG.GE.1.AND.
!CCCC1      (ICOM.EQ.'TWO ' .OR. ICOM.EQ.'2   ') .AND.
!CCCC1       IHARG(1).EQ.'WAY '.AND.IHARG(2).EQ.'ANOV')THEN
!CCCC   ILASTC=2
!CCCC   ICASE2='2WAY'
!CCCC ELSEIF(NUMARG.GE.4.AND.
!CCCC1      (ICOM.EQ.'TWO ' .OR. ICOM.EQ.'2   ') .AND.
!CCCC1      IHARG(1).EQ.'WAY ' .AND.
!CCCC1      IHARG(2).EQ.'ANAL' .AND.IHARG(3).EQ.'OF  '.AND.
!CCCC1      IHARG(4).EQ.'VARI')THEN
!CCCC   ILASTC=4
!CCCC   ICASE2='2WAY'
      ELSEIF(NUMARG.GE.4.AND.   &
            (ICOM.EQ.'ONE ' .OR. ICOM.EQ.'1   ') .AND.   &
             IHARG(1).EQ.'WAY '.AND.   &
             IHARG(2).EQ.'FIXE' .AND.IHARG(3).EQ.'EFFE'.AND.   &
             IHARG(4).EQ.'ANOV')THEN
        ILASTC=4
        ICASE2='1WAY'
!CCCC ELSEIF(NUMARG.GE.4.AND.
!CCCC1      (ICOM.EQ.'TWO ' .OR. ICOM.EQ.'2   ') .AND.
!CCCC1       IHARG(1).EQ.'WAY '.AND.
!CCCC1       IHARG(2).EQ.'FIXE' .AND.IHARG(3).EQ.'EFFE'.AND.
!CCCC1       IHARG(4).EQ.'ANOV')THEN
!CCCC   ILASTC=4
!CCCC   ICASE2='2WAY'
      ELSEIF(NUMARG.GE.6.AND.   &
            (ICOM.EQ.'ONE ' .OR. ICOM.EQ.'1   ') .AND.   &
             IHARG(1).EQ.'WAY '.AND.   &
             IHARG(2).EQ.'FIXE' .AND.IHARG(3).EQ.'EFFE'.AND.   &
             IHARG(4).EQ.'ANAL' .AND.IHARG(5).EQ.'OF  '.AND.   &
             IHARG(6).EQ.'VARI')THEN
        ILASTC=6
        ICASE2='1WAY'
!CCCC ELSEIF(NUMARG.GE.6.AND.
!CCCC1      (ICOM.EQ.'TWO ' .OR. ICOM.EQ.'2   ') .AND.
!CCCC1       IHARG(1).EQ.'WAY '.AND.
!CCCC1       IHARG(2).EQ.'FIXE' .AND.IHARG(3).EQ.'EFFE'.AND.
!CCCC1       IHARG(4).EQ.'ANAL' .AND.IHARG(5).EQ.'OF  '.AND.
!CCCC1       IHARG(6).EQ.'VARI')THEN
!CCCC   ILASTC=6
!CCCC   ICASE2='2WAY'
      ELSEIF(NUMARG.GE.4.AND.   &
            (ICOM.EQ.'ONE ' .OR. ICOM.EQ.'1   ') .AND.   &
             IHARG(1).EQ.'WAY '.AND.   &
             IHARG(2).EQ.'RAND' .AND.IHARG(3).EQ.'EFFE'.AND.   &
             IHARG(4).EQ.'ANOV')THEN
        ILASTC=4
        ICASE2='1WAY'
        ICASE3='RAND'
!CCCC ELSEIF(NUMARG.GE.4.AND.
!CCCC1      (ICOM.EQ.'TWO ' .OR. ICOM.EQ.'2   ') .AND.
!CCCC1       IHARG(1).EQ.'WAY '.AND.
!CCCC1       IHARG(2).EQ.'RAND' .AND.IHARG(3).EQ.'EFFE'.AND.
!CCCC1       IHARG(4).EQ.'ANOV')THEN
!CCCC   ILASTC=4
!CCCC   ICASE2='2WAY'
!CCCC   ICASE3='RAND'
      ELSEIF(NUMARG.GE.6.AND.   &
            (ICOM.EQ.'ONE ' .OR. ICOM.EQ.'1   ') .AND.   &
             IHARG(1).EQ.'WAY '.AND.   &
             IHARG(2).EQ.'RAND' .AND.IHARG(3).EQ.'EFFE'.AND.   &
             IHARG(4).EQ.'ANAL' .AND.IHARG(5).EQ.'OF  '.AND.   &
             IHARG(6).EQ.'VARI')THEN
        ILASTC=6
        ICASE2='1WAY'
        ICASE3='RAND'
!CCCC ELSEIF(NUMARG.GE.6.AND.
!CCCC1      (ICOM.EQ.'TWO ' .OR. ICOM.EQ.'2   ') .AND.
!CCCC1       IHARG(1).EQ.'WAY '.AND.
!CCCC1       IHARG(2).EQ.'RAND' .AND.IHARG(3).EQ.'EFFE'.AND.
!CCCC1       IHARG(4).EQ.'ANAL' .AND.IHARG(5).EQ.'OF  '.AND.
!CCCC1       IHARG(6).EQ.'VARI')THEN
!CCCC   ILASTC=6
!CCCC   ICASE2='2WAY'
!CCCC   ICASE3='RAND'
      ELSEIF(NUMARG.GE.3.AND.   &
            (ICOM.EQ.'ONE ' .OR. ICOM.EQ.'1   ') .AND.   &
             IHARG(1).EQ.'WAY '.AND.   &
             IHARG(2).EQ.'SUMM' .AND.IHARG(3).EQ.'ANOV')THEN
        ILASTC=3
        ICASE2='1WAY'
        ICASE4='SUMM'
      ELSEIF(NUMARG.GE.5.AND.   &
            (ICOM.EQ.'ONE ' .OR. ICOM.EQ.'1   ') .AND.   &
             IHARG(1).EQ.'WAY '.AND.   &
             IHARG(2).EQ.'SUMM'.AND.IHARG(3).EQ.'ANAL'.AND.   &
             IHARG(4).EQ.'OF  '.AND.IHARG(5).EQ.'VARI')THEN
        ILASTC=5
        ICASE2='1WAY'
        ICASE4='SUMM'
      ELSEIF(NUMARG.GE.5.AND.   &
            (ICOM.EQ.'ONE ' .OR. ICOM.EQ.'1   ') .AND.   &
             IHARG(1).EQ.'WAY ' .AND.   &
             IHARG(2).EQ.'SUMM' .AND. IHARG(3).EQ.'RAND' .AND.   &
             IHARG(4).EQ.'EFFE' .AND. IHARG(5).EQ.'ANOV')THEN
        ILASTC=5
        ICASE2='1WAY'
        ICASE3='RAND'
        ICASE4='SUMM'
      ELSEIF(NUMARG.GE.5.AND.   &
            (ICOM.EQ.'ONE ' .OR. ICOM.EQ.'1   ') .AND.   &
             IHARG(1).EQ.'WAY '.AND.   &
             IHARG(2).EQ.'RAND' .AND.IHARG(3).EQ.'EFFE' .AND.   &
             IHARG(4).EQ.'SUMM' .AND. IHARG(5).EQ.'ANOV')THEN
        ILASTC=5
        ICASE2='1WAY'
        ICASE3='RAND'
        ICASE4='SUMM'
      ELSEIF(NUMARG.GE.7.AND.   &
            (ICOM.EQ.'ONE ' .OR. ICOM.EQ.'1   ') .AND.   &
             IHARG(1).EQ.'WAY ' .AND.   &
             IHARG(2).EQ.'RAND' .AND. IHARG(3).EQ.'EFFE' .AND.   &
             IHARG(4).EQ.'SUMM' .AND. IHARG(5).EQ.'ANAL'.AND.   &
             IHARG(6).EQ.'OF  ' .AND. IHARG(7).EQ.'VARI')THEN
        ILASTC=7
        ICASE2='1WAY'
        ICASE3='RAND'
        ICASE4='SUMM'
      ELSEIF(NUMARG.GE.7.AND.   &
            (ICOM.EQ.'ONE ' .OR. ICOM.EQ.'1   ') .AND.   &
             IHARG(1).EQ.'WAY ' .AND.   &
             IHARG(2).EQ.'SUMM' .AND. IHARG(3).EQ.'RAND' .AND.   &
             IHARG(4).EQ.'EFFE' .AND. IHARG(5).EQ.'ANAL'.AND.   &
             IHARG(6).EQ.'OF  ' .AND. IHARG(7).EQ.'VARI')THEN
        ILASTC=7
        ICASE2='1WAY'
        ICASE3='RAND'
        ICASE4='SUMM'
      ELSE
        IFOUND='NO'
        GO TO 9000
      ENDIF
!
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'ANOV')THEN
        WRITE(ICOUT,192)ICASE2,ICASE3,ICASE4,ILASTC
  192   FORMAT('ICASE2,ICASE3,ICASE4,ILASTC = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      IFOUND='YES'
!
!               *********************************
!               **  STEP 2--                   **
!               **  EXTRACT THE VARIABLE LIST  **
!               *********************************
!
      ISTEPN='2'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'ANOV')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,92)ICASE2,ICASE3,ICASE4,IHARG(1)
   92   FORMAT('ICASE2,ICASE3,ICASE4,IHARG(1) = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      INAME='ANALYSIS OF VARIANCE'
      MINNA=1
      MAXNA=100
      MINN2=2
      IFLAGE=1
      IFLAGM=0
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=2
      MAXNVA=MAXFAC+1
      IF(ICASE2.EQ.'1WAY')THEN
        IF(ICASE4.EQ.'RAW ')THEN
          MAXNVA=2
        ELSE
          MINNVA=3
          MAXNVA=3
        ENDIF
      ELSEIF(ICASE2.EQ.'2WAY')THEN
        MAXNVA=3
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
                  IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'ANOV')THEN
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
      NUMFAC=NUMVAR-1
      J=0
      IMAX=NRIGHT(1)
      IF(NQ.LT.NRIGHT(1))IMAX=NQ
      DO 660 I=1,IMAX
        IF(ISUB(I).EQ.0)GO TO 660
        J=J+1
!
        IJ=MAXN*(ICOLR(1)-1)+I
        IF(ICOLR(1).LE.MAXCOL)Y(J)=V(IJ)
        IF(ICOLR(1).EQ.MAXCP1)Y(J)=PRED(I)
        IF(ICOLR(1).EQ.MAXCP2)Y(J)=RES(I)
        IF(ICOLR(1).EQ.MAXCP3)Y(J)=YPLOT(I)
        IF(ICOLR(1).EQ.MAXCP4)Y(J)=XPLOT(I)
        IF(ICOLR(1).EQ.MAXCP5)Y(J)=X2PLOT(I)
        IF(ICOLR(1).EQ.MAXCP6)Y(J)=TAGPLO(I)
!
        IF(ICASE2.EQ.'1WAY' .AND. ICASE4.EQ.'RAW ')THEN
          IJ=MAXN*(ICOLR(2)-1)+I
          IF(ICOLR(1).LE.MAXCOL)W(J)=V(IJ)
          IF(ICOLR(1).EQ.MAXCP1)W(J)=PRED(I)
          IF(ICOLR(1).EQ.MAXCP2)W(J)=RES(I)
          IF(ICOLR(1).EQ.MAXCP3)W(J)=YPLOT(I)
          IF(ICOLR(1).EQ.MAXCP4)W(J)=XPLOT(I)
          IF(ICOLR(1).EQ.MAXCP5)W(J)=X2PLOT(I)
          IF(ICOLR(1).EQ.MAXCP6)W(J)=TAGPLO(I)
        ELSEIF((ICASE2.EQ.'1WAY' .AND. ICASE4.EQ.'SUMM') .OR.   &
               (ICASE2.EQ.'2WAY' .AND. ICASE4.EQ.'RAW '))THEN
          IJ=MAXN*(ICOLR(2)-1)+I
          IF(ICOLR(1).LE.MAXCOL)W(J)=V(IJ)
          IF(ICOLR(1).EQ.MAXCP1)W(J)=PRED(I)
          IF(ICOLR(1).EQ.MAXCP2)W(J)=RES(I)
          IF(ICOLR(1).EQ.MAXCP3)W(J)=YPLOT(I)
          IF(ICOLR(1).EQ.MAXCP4)W(J)=XPLOT(I)
          IF(ICOLR(1).EQ.MAXCP5)W(J)=X2PLOT(I)
          IF(ICOLR(1).EQ.MAXCP6)W(J)=TAGPLO(I)
          IJ=MAXN*(ICOLR(3)-1)+I
          IF(ICOLR(1).LE.MAXCOL)Z(J)=V(IJ)
          IF(ICOLR(1).EQ.MAXCP1)Z(J)=PRED(I)
          IF(ICOLR(1).EQ.MAXCP2)Z(J)=RES(I)
          IF(ICOLR(1).EQ.MAXCP3)Z(J)=YPLOT(I)
          IF(ICOLR(1).EQ.MAXCP4)Z(J)=XPLOT(I)
          IF(ICOLR(1).EQ.MAXCP5)Z(J)=X2PLOT(I)
          IF(ICOLR(1).EQ.MAXCP6)Z(J)=TAGPLO(I)
        ELSE
          DO 659 LL=1,NUMFAC
            ICOLT=ICOLR(LL+1)
            IJ=MAXN*(ICOLT-1)+I
            IF(ICOLT.LE.MAXCOL)F1(J,LL)=V(IJ)
            IF(ICOLT.EQ.MAXCP1)F1(J,LL)=PRED(I)
            IF(ICOLT.EQ.MAXCP2)F1(J,LL)=RES(I)
            IF(ICOLT.EQ.MAXCP3)F1(J,LL)=YPLOT(I)
            IF(ICOLT.EQ.MAXCP4)F1(J,LL)=XPLOT(I)
            IF(ICOLT.EQ.MAXCP5)F1(J,LL)=X2PLOT(I)
            IF(ICOLT.EQ.MAXCP6)F1(J,LL)=TAGPLO(I)
 659      CONTINUE
        ENDIF
!
  660 CONTINUE
      NS=J
!
!               **************************************************
!               **  STEP 8--                                    **
!               **  PREPARE FOR ENTRANCE INTO DPANO2--          **
!               **  SET THE WEIGHT VECTOR TO UNITY THROUGHOUT.  **
!               **************************************************
!
      ISTEPN='8'
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'ANOV')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICASE2.NE.'1WAY' .AND. ICASE2.NE.'2WAY')THEN
        DO 680 I=1,NS
          W(I)=1.0
  680   CONTINUE
      ENDIF
!
!               ***************************
!               **  STEP 9--             **
!               **  CARRY OUT THE ANOVA  **
!               ***************************
!
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'ANOV')THEN
        ISTEPN='9'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,711)
  711   FORMAT('***** FROM DPANOV, AS WE ARE ABOUT TO CALL DPANO2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,712)ICOLR(1),MAXN,NS,NUMFAC
  712   FORMAT('ICOLR(1),MAXN,NS,NUMFAC = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,714)ICASE2,ICASE3,ICASE4
  714   FORMAT('ICASE2,ICASE3,ICASE4 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        IF(ICASE2.EQ.'KWAY')THEN
          DO 715 I=1,NS
            WRITE(ICOUT,716)I,Y(I),(F1(I,LL),LL=1,MIN(MAXFAC,5)),W(I)
  716       FORMAT('I,Y(I),F1(I),F2(I),F3(I),F4(I),F5(I),W(I) = ',   &
                   I6,2X,7F10.5)
            CALL DPWRST('XXX','BUG ')
  715     CONTINUE
        ELSE
          IF(ICASE2.EQ.'SUMM')THEN
            DO 735 I=1,NS
              WRITE(ICOUT,736)I,Y(I),W(I),Z(I)
  736         FORMAT('I,Y(I),W(I),Z(I) = ',I8,2X,3G15.7)
              CALL DPWRST('XXX','BUG ')
  735       CONTINUE
          ELSE
            DO 725 I=1,NS
              WRITE(ICOUT,726)I,Y(I),W(I)
  726         FORMAT('I,Y(I),W(I) = ',I8,2X,G15.7,F10.5)
              CALL DPWRST('XXX','BUG ')
  725       CONTINUE
          ENDIF
        ENDIF
      ENDIF
!
!CCCC JUNE, 1990.  DIMENSION Z IN DPANOV RATHER THAT DPANO2 (SO CAN
!CCCC EQUIVALENCE TO GARBAGE COMMON).
!CCCC ARGUMENT LIST MODIFIED, ADDITIONAL DIMENSIONING IN
!CCCC DPANOV INSTEAD OF DPANO2.  FEBRUARY 1998.
!CCCC CALL DPANO2(Y,F1,F2,F3,F4,F5,W,NS,NUMFAC,
      IF(ICASE2.EQ.'KWAY')THEN
        CALL DPANO2(Y,F1,W,NS,NUMFAC,IVARN1,IVARN2,   &
                    F1ID,F1N,F1MEAN,F1EFFE,F1EFSD,   &
                    CELLME,CELLV,CELLN,   &
                    AINN,AINMEA,AINEFF,AINESD,   &
                    MAXOBV,MAXLEV,MAXFAC,   &
                    N1,ISET,AN1,E1,SS1,RESMS1,FVAL,F1CDF2,RSD,   &
                    B,SDB,FCUM,REPSD,REPDF,RESSD,RESDF,   &
                    PRED2,RES2,ALFCDF,   &
                    Z,   &
                    ICAPSW,ICAPTY,IFORSW,IAOV2T,IAOVTY,   &
                    IBUGA3,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
      ELSEIF(ICASE2.EQ.'1WAY')THEN
        IH='ALPH'
        IH2='A   '
        IHWUSE='P'
        MESSAG='NO'
        CALL CHECKN(IH,IH2,IHWUSE,   &
                    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                    ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
        IF(IERROR.EQ.'YES')THEN
          ALPHA=0.05
        ELSE
          ALPHA=VALUE(ILOC)
        ENDIF
        IF(ICASE4.EQ.'SUMM')THEN
          CALL DPANO4(Y,W,Z,NS,ALPHA,IVARN1,IVARN2,IANOMC,          &
                      ICASE4,ICASE3,                                &
                      TEMP1,TEMP2,NS,TEMP3,TEMP4,                   &
                      PRED2,RES2,                                   &
                      SSTOT,SSTR,SSE,AMSE,AMSTR,IDFTO,IDFTR,IDFE,   &
                      FSTAT,FVALUE,PVALUE,RESSD,                    &
                      ICAPSW,ICAPTY,IFORSW,                         &
                      IBUGA3,ISUBRO,IERROR)
        ELSE
          CALL DPANO4(TEMP1,TEMP2,Z,NS,ALPHA,IVARN1,IVARN2,IANOMC,   &
                      ICASE4,ICASE3,                                 &
                      Y,W,NS,TEMP3,TEMP4,                            &
                      PRED2,RES2,                                    &
                      SSTOT,SSTR,SSE,AMSE,AMSTR,IDFTO,IDFTR,IDFE,    &
                      FSTAT,FVALUE,PVALUE,RESSD,                     &
                      ICAPSW,ICAPTY,IFORSW,                          &
                      IBUGA3,ISUBRO,IERROR)
        ENDIF
        IF(IERROR.EQ.'YES')GO TO 9000
!
        IH='STAT'
        IH2='VAL '
        VALUE0=FSTAT
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,                     &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
        IH='STAT'
        IH2='CDF '
        VALUE0=FVALUE
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,                     &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
        IH='PVAL'
        IH2='UE  '
        VALUE0=PVALUE
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,                     &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
!
        IH='MSTR'
        IH2='    '
        VALUE0=AMSTR
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,                     &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
        IH='MSE '
        IH2='    '
        VALUE0=AMSE
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,                     &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
        IH='SSTR'
        IH2='    '
        VALUE0=SSTR
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,                     &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
        IH='SSE '
        IH2='    '
        VALUE0=SSE
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,                     &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
        IH='SSTO'
        IH2='TAL '
        VALUE0=SSTOT
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,                     &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
        GO TO 9000
      ELSEIF(ICASE2.EQ.'2WAY')THEN
      ENDIF
!
!               ***************************************
!               **  STEP 10--                        **
!               **  UPDATE INTERNAL DATAPLOT TABLES  **
!               ***************************************
!
      ISTEPN='10'
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'ANOV')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICOLPR=MAXCP1
      ICOLRE=MAXCP2
      IREPU='ON'
      IRESU='ON'
      CALL UPDAPR(ICOLPR,ICOLRE,PRED2,RES2,PRED,RES,ISUB,NRIGHT(1),   &
      IREPU,REPSD,REPDF,IRESU,RESSD,RESDF,ALFCDF,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,ILOCN,IBUGA3,IERROR)
!
!     2011/09: SAVE "STATCDF" VALUES
!
      DO 810 I=1,NUMFAC
        IF(I.EQ.1 .AND. NUMFAC.EQ.1)THEN
          IH='STAT'
          IH2='CDF '
        ELSE
          IH='STAT'
          IH2='CDF '
          IF(I.LE.9)THEN
            WRITE(IH2(4:4),'(I1)')I
          ELSEIF(I.LE.99)THEN
            WRITE(IH2(3:4),'(I2)')I
          ENDIF
        ENDIF
        VALUE0=F1CDF2(I)
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,                     &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
  810 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'ANOV')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPANOV--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NS,NUMFAC
 9014   FORMAT('NS,NUMFAC = ',2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)IFOUND,IERROR
 9016   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPANOV
      SUBROUTINE DPANPP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                        IANGLU,MAXNPP,                         &
                        ANOPL1,ANOPL2,                         &
                        IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--FORM A ANOP (ANALYSIS OF PROPORTIONS) PLOT
!              (USEFUL FOR DETERMINING WHICH INDEPENDENT VARIABLE
!              CONTRIBUTES MOST TO EXTREMAL OBSERVATIONS
!              IN THE RESPONSE VARIABLE).
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--87/6
!     ORIGINAL VERSION--JUNE      1987.
!     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE COMMON
!                                       MOVE SOME DIMENSIONS FROM DPANP2
!     UPDATED         --OCTOBER   1992. FIX GARBAGE EQUIVALENCE
!     UPDATED         --MAY       2011. USE DPPARS, DPPAR3, DPPAR8
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
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      CHARACTER*4 ICASE
      CHARACTER*4 IMULT
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
      DIMENSION Y1(MAXOBV)
      DIMENSION Y2(MAXOBV)
!CCCC FOLLOWING LINES ADDED JUNE, 1990
      INCLUDE 'DPCOZZ.INC'
      DIMENSION XD(MAXOBV)
      DIMENSION PIR(MAXOBV)
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),Y2(1))
!CCCC THE FOLLOWING 2 LINES WERE FIXED   OCTOBER 1992
!CCCC EQUIVALENCE (GARBAG(IGARB2),XD(1))
!CCCC EQUIVALENCE (GARBAG(IGARB2),PIR(1))
      EQUIVALENCE (GARBAG(IGARB3),XD(1))
      EQUIVALENCE (GARBAG(IGARB4),PIR(1))
!CCCC END CHANGE
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPAN'
      ISUBN2='PP  '
!
      IFOUND='NO'
      IERROR='NO'
      IMULT='OFF'
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ANPP')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPANPP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,IAND1,IAND2,IANGLU
   53   FORMAT('ICASPL,IAND1,IAND2,IANGLU = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)IBUGG2,IBUGG3,IBUGQ,ISUBRO
   54   FORMAT('IBUGG2,IBUGG3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,57)IFOUND,IERROR,ICASPL,MAXN,MAXNPP
   57   FORMAT('IFOUND,IERROR,ICASPL,MAXN,MAXNPP = ',3(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,61)ANOPL1,ANOPL2
   61   FORMAT('ANOPL1,ANOPL2 = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ***********************************
!               **  TREAT THE ANOP     PLOT CASE **
!               ***********************************
!
!               ***************************
!               **  STEP 11--            **
!               **  EXTRACT THE COMMAND  **
!
      ISTEPN='11'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ANPP')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICOM.EQ.'MULT')THEN
        IF(IHARG(1).EQ.'PROP'.AND.IHARG(2).EQ.'PLOT')THEN
          ILASTC=2
        ELSEIF(IHARG(1).EQ.'ANOP'.AND.IHARG(2).EQ.'PLOT')THEN
          ILASTC=2
        ELSEIF(IHARG(1).EQ.'ANAL'.AND.IHARG(2).EQ.'OF' .AND.   &
               IHARG(3).EQ.'PROP'.AND.IHARG(4).EQ.'PLOT')THEN
          ILASTC=3
        ELSE
          GO TO 9000
        ENDIF
        IMULT='ON'
      ELSEIF(ICOM.EQ.'PROP')THEN
        IF(IHARG(1).EQ.'PLOT')THEN
          ILASTC=1
        ELSE
          GO TO 9000
        ENDIF
      ELSEIF(ICOM.EQ.'ANOP')THEN
        IF(IHARG(1).EQ.'PLOT')THEN
          ILASTC=1
        ELSE
          GO TO 9000
        ENDIF
      ELSEIF(ICOM.EQ.'ANAL')THEN
        IF(IHARG(1).EQ.'OF' .AND. IHARG(2).EQ.'PROP' .OR.   &
           IHARG(3).EQ.'PLOT')THEN
          ILASTC=3
        ELSE
          GO TO 9000
        ENDIF
      ELSE
        GO TO 9000
      ENDIF
!
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      IFOUND='YES'
      ICASPL='ANPP'
!
!               ****************************************
!               **  STEP 2--                          **
!               **  EXTRACT THE VARIABLE LIST         **
!               ****************************************
!
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ANPP')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      INAME='ANALYSIS OF PROPORTIONS PLOT'
      MINNA=2
      MAXNA=100
      MINN2=2
      IFLAGE=1
      IFLAGM=0
      IF(IMULT.EQ.'ON')THEN
        IFLAGE=0
        IFLAGM=1
        MINNVA=2
        MAXNVA=MAXSPN
      ELSE
        IFLAGE=1
        IFLAGM=0
        MINNVA=2
        MAXNVA=2
      ENDIF
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ANOP')THEN
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
!               *******************************************************
!               **  STEP 3--                                          *
!               **  FORM THE VERTICAL AND HORIZONTAL AXIS VARIABLES   *
!               **  (Y(.) AND X(.), RESPECTIVELY) FOR THE PLOT.       *
!               **  FORM THE CURVE DESIGNATION VARIABLE D(.)  .       *
!               **  THIS WILL BE BOTH ONES FOR BOTH CASES             *
!               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).     *
!               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).     *
!               *******************************************************
!
      ISTEPN='3'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ANPP')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!               *****************************************
!               **  STEP 3A--                          **
!               **  CASE 1: TWO RESPONSE VARIABLES     **
!               **          WITH NO REPLICATION        **
!               *****************************************
!
!     NOTE: ONLY ALLOW MATRIX ARGUMENTS FOR "MULTIPLE" CASE.
!           FOR CASE WHERE SECOND VARIABLE IS A GROUP-ID VARIABLE,
!           MATRIX ARGUMENTS DON'T MAKE SENSE.
!
      IF(IMULT.EQ.'OFF' .AND. NUMVAR.EQ.2)THEN
        ISTEPN='3A'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ANPP')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        ICOL=1
        NUMVA2=2
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                    INAME,IVARN1,IVARN2,IVARTY,   &
                    ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                    MAXCP4,MAXCP5,MAXCP6,   &
                    V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                    Y1,Y2,Y2,NLOCAL,NLOCA2,NLOCA2,ICASE,   &
                    IBUGG3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
!               *************************************************
!               **  STEP 3B--                                  **
!               **  PREPARE FOR ENTRANCE INTO DPANP2--         **
!               *************************************************
!
        ISTEPN='3B'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ANPP')THEN
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,331)
  331     FORMAT('***** FROM DPANPP, AS WE ARE ABOUT TO CALL DPANP2--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,332)NLOCAL
  332     FORMAT('NLOCAL = ',I8)
          CALL DPWRST('XXX','BUG ')
          DO 335 I=1,NLOCAL
            WRITE(ICOUT,336)I,Y(I),X(I)
  336       FORMAT('I,Y(I),X(I) = ',I8,2G15.7)
            CALL DPWRST('XXX','BUG ')
  335     CONTINUE
        ENDIF
!
        CALL DPANP2(Y1,Y2,NLOCAL,ICASPL,MAXN,   &
                    ANOPL1,ANOPL2,   &
                    Y,X,D,NPLOTP,NPLOTV,   &
                    XD,PIR,   &
                    IBUGG3,ISUBRO,IERROR)
!
!               *******************************************************
!               **  STEP 4A--                                        **
!               **  CASE 2: MULTIPLE RESPONSE VARIABLES.  NOTE THAT  **
!               **          ANOP PLOT, THE MULTIPLE LABS ARE         **
!               **          CONVERTED INTO A "Y X" STACKED PAIR      **
!               **          WHERE "X" IS THE LAB-ID VARIABLE.        **
!               *******************************************************
!
      ELSEIF(IMULT.EQ.'ON' .OR. NUMVAR.GE.3)THEN
        ISTEPN='4A'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ANNP')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        ICOL=1
        CALL DPPAR8(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                    INAME,IVARN1,IVARN2,IVARTY,   &
                    ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,   &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                    MAXCP4,MAXCP5,MAXCP6,   &
                    V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                    XD,Y1,Y2,NLOCAL,ICASE,   &
                    IBUGG3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
        NUMVAR=2
!
        IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'ANNP')THEN
          ISTEPN='4B'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,442)
  442     FORMAT('***** FROM THE MIDDLE  OF DPANOP--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,443)ICASPL,NUMVAR,NLOCAL
  443     FORMAT('ICASPL,NUMVAR,NLOCAL = ',A4,2I8)
          CALL DPWRST('XXX','BUG ')
          IF(NLOCAL.GE.1)THEN
            DO 445 I=1,NLOCAL
              WRITE(ICOUT,446)I,Y1(I),Y2(I)
  446         FORMAT('I,Y(I),X(I) = ',I8,2G15.7)
              CALL DPWRST('XXX','BUG ')
  445       CONTINUE
          ENDIF
        ENDIF
!
        CALL DPANP2(Y1,Y2,NLOCAL,ICASPL,MAXN,   &
                    ANOPL1,ANOPL2,   &
                    Y,X,D,NPLOTP,NPLOTV,   &
                    XD,PIR,   &
                    IBUGG3,ISUBRO,IERROR)
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ANPP')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPANPP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9013   FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',   &
               3I8,2X,2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        IF(NPLOTP.GT.0)THEN
          DO 9020 I=1,NPLOTP
            WRITE(ICOUT,9021)I,Y(I),X(I),D(I)
 9021       FORMAT('I,Y(I),X(I),D(I) = ',I8,3F12.5)
            CALL DPWRST('XXX','BUG ')
 9020     CONTINUE
        ENDIF
        WRITE(ICOUT,9041)ANOPL1,ANOPL2
 9041   FORMAT('ANOPL1,ANOPL2 = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPANPP
      SUBROUTINE DPANP2(Y,X,N,ICASPL,MAXN,   &
                        ANOPL1,ANOPL2,   &
                        Y2,X2,D2,N2,NPLOTV,   &
                        XD,PIR,   &
                        IBUGG3,ISUBRO,IERROR)
!CCCC JUNE, 1990.  XD AND PIR NOW DIMENSIONED IN DPANPP
!
!     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
!              THAT WILL DEFINE
!              AN ANOP (ANALYSIS OF PROPORITONS) PLOT.
!              THE PLOT WILL CONSIST OF 2 COMPONENTS--
!                  1) A PROPORTIONS LINE TRACE
!                     WITH LEVELS OF THE INDEPENDENT VARIABLE (HORIZONTALLY)
!                     AND THE PROPORTION OF OBSERVATIONS IN THAT LEVEL
!                     WHICH FALL INTO THE REPONSE VARIABLE TARGET REGION
!                     (THAT IS, BETWEEN ANOPL1 AND ANOPL2, INCLUSIVELY)
!                     (VERTICALLY)
!                  2) A GRAND PROPORTIONS HORIZONTAL LINE WHICH RUNS ACROSS
!                     THE ENTIRE PLOT AND WHICH GIVES THE
!                     PROPORTION OF OBSERVATIONS (OVER THE ENTIRE DATA SET)
!                     WHICH FALL INTO THE RESPONSE VARIABLE TARGET REGION.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--87/6
!     ORIGINAL VERSION--JUNE      1987.
!     UPDATED--         JUNE      1990.  SOME DIMENSIONS NOW DONE IN DPANPP
!     UPDATED--         APRIL     1992.  COMMENT OUT
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
!CCCC ADD FOLLOWING LINE NOVEMBER 1994.
      CHARACTER*4 ICASPL
!
      CHARACTER*4 IWRITE
!
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
!
!CCCC JUNE, 1990.  FOLLOWING 2 LINES NOW DIMENSIONED IN DPANPP
!CCCC DIMENSION XD(MAXOBV)
!CCCC DIMENSION PIR(MAXOBV)
      DIMENSION XD(*)
      DIMENSION PIR(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPAN'
      ISUBN2='P2  '
!
      IERROR='NO'
      IWRITE='OFF'
!
      AN=N
!
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'ANP2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPANP2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGG3,ISUBRO
   52   FORMAT('IBUGG3,ISUBRO = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,MAXN,N,NPLOTV
   53   FORMAT('ICASPL,MAXN,N,NPLOTV = ',A4,2X,3I8)
        CALL DPWRST('XXX','BUG ')
        IF(N.GT.0)THEN
          DO 61 I=1,N
            WRITE(ICOUT,62)I,Y(I),X(I)
   62       FORMAT('I,Y(I),X(I) = ',I8,2G15.7)
            CALL DPWRST('XXX','BUG ')
   61     CONTINUE
        ENDIF
        WRITE(ICOUT,71)ANOPL1,ANOPL2
   71   FORMAT('ANOPL1,ANOPL2 = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ********************************************
!               **  STEP 11--                             **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      IF(N.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1111)
 1111   FORMAT('***** ERROR IN ANALYSIS OF PROPORTION PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1112)
 1112   FORMAT('      THE NUMBER OF OBSERVATIONS MUST BE AT LEAST 2;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1114)N
 1114   FORMAT('      THE ENTERED NUMBER OF OBSERVATIONS HERE = ',I6)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      HOLD=Y(1)
      DO 1130 I=1,N
        IF(Y(I).NE.HOLD)GO TO 1139
 1130 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1132)
 1132 FORMAT('      ALL RESPONSE VARIABLE ELEMENTS ARE IDENTICALLY ',   &
             'EQUAL TO ',G15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
 1139 CONTINUE
!
!               **************************************************
!               **  STEP 21--                                   **
!               **  DETERMINE THE NUMBER OF OBSERVATIONS        **
!               **  (AND THE PROPORTION) OF ALL OBSERVATIONS    **
!               **  WHICH FALL IN THE RESPONSE VARIABLE         **
!               **  TARGET REGION                               **
!               **  (BASED ON THE TOTAL DATA SET).              **
!               **  N AND AN   = TOTAL NUMBER OF OBSERVATIONS   **
!               **  NR AND ANR = TOTAL NUMBER OF OBSERVATIONS   **
!               **               IN THE TARGET REGION.          **
!               **  PR         = PROPROTION OF OBSERVATIONS     **
!               **               IN THE TARGET REGION.          **
!               **************************************************
!
      YMIN=ANOPL1
      IF(ANOPL1.GT.ANOPL2)YMIN=ANOPL2
      YMAX=ANOPL2
      IF(ANOPL1.GT.ANOPL2)YMAX=ANOPL1
!
      NR=0
      DO 2120 J=1,N
      IF(YMIN.LE.Y(J).AND.Y(J).LE.YMAX)NR=NR+1
 2120 CONTINUE
      ANR=NR
!
      PR=100.0*(ANR/AN)
!
!               **************************************************
!               **  STEP 22--                                   **
!               **  DETERMINE THE DISTINCT VALUES               **
!               **  OF THE VARIABLE X                           **
!               **************************************************
!
      CALL DISTIN(X,N,IWRITE,XD,NXD,IBUGG3,IERROR)
!
!               ****************************************************
!               **  STEP 23--                                     **
!               **  LOOP THROUGH THE DISTINCT LEVELS OF X.        **
!               **  FOR EACH DISTINCT LEVEL OF X,                 **
!               **  DETERMINE THE NUMBER OF OBSERVATIONS          **
!               **  (AND THE PROPORTION) OF ALL OBSERVATIONS      **
!               **  WHICH FALL IN THE RESPONSE VARIABLE           **
!               **  TARGET REGION                                 **
!               **  (BASED ON THE DATA FROM THIS LEVEL ONLY).     **
!               **  NI AND ANI   = NUMBER OF OBSERVATIONS         **
!               **                 IN LEVEL I OF THE IND. VAR.    **
!               **  NIR AND ANIR = NUMBER OF OBSERVATIONS         **
!               **                 IN LEVEL I OF THE IND. VAR.    **
!               **                 AND IN THE TARGET REGION.      **
!               **  PIR          = PROPROTION OF OBSERVATIONS     **
!               **                 IN LEVEL I OF THE IND. VAR.    **
!               **                 AND IN THE TARGET REGION.      **
!               ****************************************************
!
      DO 2300 I=1,NXD
        XDI=XD(I)
!
        NI=0
        DO 2310 J=1,N
          IF(X(J).EQ.XDI)NI=NI+1
 2310   CONTINUE
        ANI=NI
!
        NIR=0
        DO 2330 J=1,N
          IF(X(J).EQ.XDI.AND.   &
            YMIN.LE.Y(J).AND.Y(J).LE.YMAX)NIR=NIR+1
 2330   CONTINUE
        ANIR=NIR
!
        PIR(I)=100.0*(ANIR/ANI)
!
 2300 CONTINUE
!
!               ****************************************************
!               **  STEP 24--                                     **
!               **  DETERMINIMUME THE MIN DISTINCT X VALUE        **
!               **  DETERMAXIMUME THE MIN DISTINCT X VALUE        **
!               ****************************************************
!
      XDMIN=XD(1)
      XDMAX=XD(1)
      DO 2400 I=1,NXD
      IF(XD(I).LT.XDMIN)XDMIN=XD(I)
      IF(XD(I).GT.XDMAX)XDMAX=XD(I)
 2400 CONTINUE
!
!               *******************************************
!               **  STEP 51--                            **
!               **  FORM PLOT COORDINATES                **
!               **  WITH 2 COMPONENTS--                  **
!               **     1) PROPORTIONS TRACE              **
!               **     2) TOTAL PROPORTIONS HORIZ. LINE  **
!               *******************************************
!
      J=0
      DO 5110 I=1,NXD
        J=J+1
        Y2(J)=PIR(I)
        X2(J)=XD(I)
        D2(J)=1.0
 5110 CONTINUE
!
      J=J+1
      Y2(J)=PR
      X2(J)=XDMIN
      D2(J)=2.0
      J=J+1
      Y2(J)=PR
      X2(J)=XDMAX
      D2(J)=2.0
!
      N2=J
      NPLOTV=3
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'ANP2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPANP2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICASPL,MAXN,N2,NXD,IERROR
 9012   FORMAT('ICASPL,MAXN,N2,NXD,IERROR = ',A4,3I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,N2
          WRITE(ICOUT,9016)I,Y2(I),X2(I),D2(I)
 9016     FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,2G15.7,F9.2)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
        DO 9032 I=1,NXD
         WRITE(ICOUT,9033)I,XD(I),PIR(I)
 9033    FORMAT('I,XD(I),PIR(I) = ',I8,2G15.7)
         CALL DPWRST('XXX','BUG ')
 9032   CONTINUE
        WRITE(ICOUT,9042)AN,ANR,PR
 9042   FORMAT('AN,ANR,PR = ',3E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9043)ANI,ANIR,PIR(NXD)
 9043   FORMAT('ANI,ANIR,PIR(NXD) = ',3E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9051)XDMIN,XDMAX
 9051   FORMAT('XDMIN,XDMAX = ',2E15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPANP2
      SUBROUTINE DPAPNU(IHREF1,IHREF2,KNUMB,IVAL,   &
      IH1,IH2,IBUGS2,ISUBRO,IERROR)
!
!     PURPOSE--FOR A GIVEN CHARACTER*4 REFERENCE PAIR IHREF1/IHREF2,
!              A GIVEN TARGET POSITION OF THE 8 IN IHREF1 AND IHREF2,
!              AND A GIVEN INTEGER IVAL,
!              FORM THE CHARACTER*4 IH1/IH2 PAIR
!              WITH THE SAME BODY AS IHREF1/IHREF2
!              BUT WITH IVAL APPENDED.
!     NOTE--THE TARGET POSTION IS THE FIRST LOCATION
!           INTO WHICH THE NUMBER IS TO BE APPENDED.
!
!     ORIGINAL VERSION--DECEMBER   1986.
!
!---------------------------------------------------------------------
!
      CHARACTER*4 IHREF1
      CHARACTER*4 IHREF2
      CHARACTER*4 IH1
      CHARACTER*4 IH2
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*8 IH8
      CHARACTER*4 IHOUT
      CHARACTER*4 IVALID
      CHARACTER*1 IHOUT1
      CHARACTER*4 IHOUT4
      CHARACTER*8 IHOUT8
!
      DIMENSION IHOUT(40)
!
!-----COMMON VARIABLES (GENERAL)-----------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
!
      KNUMB2=KNUMB
!
      IF(IBUGS2.EQ.'OFF'.AND.ISUBRO.NE.'APNU')GO TO 90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPAPNU--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGS2,ISUBRO,IERROR
   52 FORMAT('IBUGS2,ISUBRO,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IHREF1,IHREF2,KNUMB,IVAL
   53 FORMAT('IHREF1,IHREF2,KNUMB,IVAL = ',A4,2X,A4,2I8)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
!
!               ****************************************************
!               **  STEP 11--                                     **
!               **  FORM IH8 WHICH WILL BE A CHARACTER*8          **
!               **  COMBINATION OF IH1 AND IH2.                   **
!               **  COPY IHREF1 INTO THE FIRST  HALF OF IH8.      **
!               **  COPY IHREF2 INTO THE SECOND HALF OF IH8.      **
!               **  THEN BLANK OUT THE END OF IH8.                **
!               ****************************************************
!
      IH8(1:4)=IHREF1(1:4)
      IH8(5:8)=IHREF2(1:4)
!
      IF(KNUMB2.LE.0)KNUMB2=1
      IF(KNUMB2.GE.9)GO TO 2100
      DO 1100 K=KNUMB2,8
      IH8(K:K)=' '
 1100 CONTINUE
!
!               *************************************
!               **  STEP 12--                      **
!               **  CONVERT IVAL INTO ALPHABETIC.  **
!               *************************************
!
      CALL DPCOIH(IVAL,IHOUT,NOUT,IVALID,IBUGS2,ISUBRO,IERROR)
      IF(IVALID.EQ.'NO')IERROR='YES'
      IF(IVALID.EQ.'NO')GO TO 9000
!
      IF(NOUT.LE.0)GO TO 1290
      IHOUT8='        '
      KMAX=NOUT
      IF(KMAX.GT.8)KMAX=8
      DO 1200 K=1,KMAX
      IHOUT4=IHOUT(K)
      IHOUT1=IHOUT4(1:1)
      IHOUT8(K:K)=IHOUT1
 1200 CONTINUE
 1290 CONTINUE
!
!               ********************************************
!               **  STEP 13--                             **
!               **  APPEND THE ALPHABETIC REPRESENTATION  **
!               **  OF IVAL AT THE PROPER POSITION        **
!               **  IN IH1IH2.                            **
!               ********************************************
!
      IF(NOUT.LE.0)GO TO 9000
!
      L=0
      DO 1300 K=KNUMB2,8
      L=L+1
      IF(L.LE.NOUT)IH8(K:K)=IHOUT8(L:L)
 1300 CONTINUE
!
!               ***********************************************
!               **  STEP 21--                                **
!               **  COPY IH8 INTO 2 COMPONENTS--IH1 AND IH2  **
!               ***********************************************
!
 2100 CONTINUE
      IH1(1:4)=IH8(1:4)
      IH2(1:4)=IH8(5:8)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
      IF(IBUGS2.EQ.'OFF'.AND.ISUBRO.NE.'APNU')GO TO 9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPAPNU--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGS2,ISUBRO,IERROR
 9012 FORMAT('IBUGS2,ISUBRO,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IHREF1,IHREF2,KNUMB,IVAL
 9013 FORMAT('IHREF1,IHREF2,KNUMB,IVAL = ',A4,2X,A4,2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)IH8,IH1,IH2
 9014 FORMAT('IH8,IH1,IH2 = ',A8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)KNUMB2,NOUT,IVALID
 9015 FORMAT('KNUMB2,NOUT,IVALID = ',2I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9016)IHOUT1,IHOUT4,IHOUT8
 9016 FORMAT('IHOUT1,IHOUT4,IHOUT8 = ',A1,2X,A4,2X,A8)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPAPNU
      SUBROUTINE DPAPN2(IHREF1,IHREF2,IVAL,MAXNAM,   &
                        IH1,IH2,IBUGS2,ISUBRO,IERROR)
!
!     PURPOSE--FOR A GIVEN CHARACTER*4 REFERENCE PAIR IHREF1/IHREF2, AND
!              A GIVEN INTEGER IVAL, FORM THE CHARACTER*4 IH1/IH2 PAIR
!              WITH THE SAME BODY AS IHREF1/IHREF2 BUT WITH IVAL APPENDED.
!     NOTE--THE APPENDING IS DONE TO THE FIRST BLANK POSITION OR (IF ALL
!           8 POSITIONS ARE FILLED), THE APPENDING IS DONE STARTING IN
!           POSITION 7 (THEREBY OVERWRITING) THE CHARACTERS IN 7 AND 8
!           EXAMPLE--IF IHREF1/IHREF2 IS ABC AND IVAL IS 6
!                    THEN IH1/IH2 IS ABC6
!                  --IF IHREF1/IHREF2 IS ABCDEFGH AND IVAL IS 6
!                    THEN IH1/IH2 IS ABCDEF6
!                  --IF IHREF1/IHREF2 IS ABCDEFGH AND IVAL IS 24
!                    THEN IH1/IH2 IS ABCDEF24
!     NOTE--IVAL SHOULD ASSUMED TO BE BETWEEN 0 AND 99 (NOT TESTED FOR)
!           IF IVAL IS BIGGER THAN THIS AND IF THERE ARE ENOUGH
!           TRAILING BLANKS IN IHREF1/IHREF2 TO ACCOMODATE, THEN
!           THE FULL VALUE WILL BE APPENDED.
!           ON THE OTHER HAND, IF IHREF1/IHREF2 HAS 7 OR 8 CHARACTERS,
!           AND IF IVAL IS 3 OR MORE DIGITS, THEN IVAL WILL BE TRUNCATED.
!
!     ORIGINAL VERSION--SEPTEMBER 1987.
!     UPDATED         --MARCH     2021. INCREASE MAXIMUM NUMBER FOR
!                                       IVAL FROM 99 TO MAXNAM WHICH
!                                       IS NOW PASSED AS ARGUMENT
!
!---------------------------------------------------------------------
!
      CHARACTER*4 IHREF1
      CHARACTER*4 IHREF2
      CHARACTER*4 IH1
      CHARACTER*4 IH2
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*8 IH8
      CHARACTER*4 IHOUT
      CHARACTER*4 IVALID
      CHARACTER*1 IHOUT1
      CHARACTER*4 IHOUT4
      CHARACTER*8 IHOUT8
!
      DIMENSION IHOUT(40)
!
!-----COMMON VARIABLES (GENERAL)-----------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
!
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'APN2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPAPN2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGS2,ISUBRO,IERROR
   52   FORMAT('IBUGS2,ISUBRO,IERROR = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IHREF1,IHREF2,IVAL,MAXNAM
   53   FORMAT('IHREF1,IHREF2,IVAL,MAXNAM = ',2(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(IVAL.GT.MAXNAM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,91)
   91   FORMAT('***** ERROR IN DPAPN2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,92)IVAL,MAXNAM
   92   FORMAT('      THE VARIABLE NUMBER, ',I6,' IS GREATER THAN ',   &
               'THE ALLOWED MAXIMUM OF ',I6,'.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               ****************************************************
!               **  STEP 11--                                     **
!               **  FORM IH8 WHICH WILL BE A CHARACTER*8          **
!               **  COMBINATION OF IH1 AND IH2.                   **
!               **  COPY IHREF1 INTO THE FIRST  HALF OF IH8.      **
!               **  COPY IHREF2 INTO THE SECOND HALF OF IH8.      **
!               **  THEN BLANK OUT THE END OF IH8.                **
!               ****************************************************
!
      IH8(1:8)='        '
      IH8(1:4)=IHREF1(1:4)
      IH8(5:8)=IHREF2(1:4)
!
!               ****************************************************
!               **  STEP 12--                                     **
!               **  DETERMINE THE TARGET POSITION =               **
!               **  THE FIRST NON-BLANK POSITION IN               **
!               **  IHREF1/IHREF2                                 **
!               **  (BUT IF 7 AND BEYOND, SET IT TO 7)            **
!               ****************************************************
!
      IFIRBL=9
      DO 1100 I=1,8
        IREV=8-I+1
        IF(IH8(IREV:IREV).NE.' ')GO TO 1190
        IFIRBL=IREV
 1100 CONTINUE
 1190 CONTINUE
      IF(IFIRBL.GE.7)IFIRBL=7
!
!     2021/03: BASED ON IVAL, ADJUST IFIRBL
!
      IF(IVAL.GE.10000 .AND. IVAL.LE.99999)THEN
        IF(IFIRBL.GT.3)IFIRBL=3
      ELSEIF(IVAL.GE.1000 .AND. IVAL.LE.9999)THEN
        IF(IFIRBL.GT.4)IFIRBL=4
      ELSEIF(IVAL.GE.100 .AND. IVAL.LE.999)THEN
        IF(IFIRBL.GT.5)IFIRBL=5
      ELSEIF(IVAL.GE.10 .AND. IVAL.LE.99)THEN
        IF(IFIRBL.GT.6)IFIRBL=6
      ENDIF
!
!               ***********************************************
!               **  STEP 13--                                **
!               **  CONVERT IVAL INTO ALPHABETIC.            **
!               **  NOTE--NOUT = NUMBER OF RESULTING DIGITS  **
!               ***********************************************
!
      CALL DPCOIH(IVAL,IHOUT,NOUT,IVALID,IBUGS2,ISUBRO,IERROR)
      IF(IVALID.EQ.'NO')IERROR='YES'
      IF(IVALID.EQ.'NO')GO TO 9000
!
      IF(NOUT.GT.0)THEN
        IHOUT8='        '
        KMAX=NOUT
        IF(KMAX.GT.8)KMAX=8
        DO 1300 K=1,KMAX
          IHOUT4=IHOUT(K)
          IHOUT1=IHOUT4(1:1)
          IHOUT8(K:K)=IHOUT1
 1300   CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 14--                             **
!               **  APPEND THE ALPHABETIC REPRESENTATION  **
!               **  OF IVAL AT THE PROPER POSITION        **
!               **  IN IH8.                               **
!               **  IF THERE ARE MORE DIGITS IN IVAL      **
!               **  THAN SPACE IN IH8 ALLOWS, THEN        **
!               **  TRUNCATE REMAINING DIGITS             **
!               ********************************************
!
      IF(NOUT.LE.0)GO TO 9000
!
      L=0
      DO 1400 K=IFIRBL,8
        L=L+1
        IF(L.LE.NOUT)IH8(K:K)=IHOUT8(L:L)
 1400 CONTINUE
!
!               ***********************************************
!               **  STEP 21--                                **
!               **  COPY IH8 INTO 2 COMPONENTS--IH1 AND IH2  **
!               ***********************************************
!
      IH1(1:4)=IH8(1:4)
      IH2(1:4)=IH8(5:8)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'APN2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPAPN2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IERROR,IHREF1,IHREF2,IVAL
 9013   FORMAT('IERROR,IHREF1,IHREF2,IVAL = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IH8,IH1,IH2
 9014   FORMAT('IH8,IH1,IH2 = ',A8,2(2X,A4))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)IFIRBL,NOUT,IVALID
 9015   FORMAT('IFIRBL,NOUT,IVALID = ',2I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)IHOUT1,IHOUT4,IHOUT8
 9016   FORMAT('IHOUT1,IHOUT4,IHOUT8 = ',A1,2X,A4,2X,A8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPAPN2
      SUBROUTINE DPAPPE(IBUGS2,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--APPEND A VARIABLE X TO A VARIABLE Y.
!     EXAMPLE--APPEND X Y    WHICH APPENDS X TO Y
!     NOTE--SIMILAR TO THE    EXTEND   COMMAND
!           BUT WITH THE ARGUMENTS REVERSED.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION (IN DPLET)--APRIL     1984.
!     UPDATED                    --JUNE      1990.  ADD ISUBRO TO CALL LIST
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IBUGS2
      CHARACTER*4 IBUGQ
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!CCCC FOLLOWING LINE ADDED JUNE 1990.
      CHARACTER*4 ISUBRO
!
      CHARACTER*4 IVAR11
      CHARACTER*4 IVAR12
      CHARACTER*4 IVAR21
      CHARACTER*4 IVAR22
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPAP'
      ISUBN2='PE  '
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
      I2=0
      N1=0
      N2=0
      ICOL1=0
      ICOL2=0
!
      IVAR11='UNKN'
      IVAR12='UNKN'
      IVAR21='UNKN'
      IVAR22='UNKN'
      ILIST1=(-999)
      ILIST2=(-999)
      N1PN2=(-999)
      N1PI=(-999)
      IJ1=(-999)
      IJ2=(-999)
      N1NEW=(-999)
      IROW1=(-999)
      IROWN=(-99)
!
!               **********************************************
!               **  TREAT THE CASE OF APPENDING A VARIABLE  **
!               **  BY THE CONTENTS OF ANOTHER VARIABLE.    **
!               **********************************************
!
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'APPE')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPAPPE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGS2,IBUGQ
   52   FORMAT('IBUGS2,IBUGQ = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *******************************************************
!               **  STEP 2--                                         **
!               **  CHECK FOR THE PROPER NUMBER OF INPUT ARGUMENTS.  **
!               *******************************************************
!
      ISTEPN='2'
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'APPE')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      MINNA=2
      MAXNA=100
      CALL CHECKA(NUMARG,MINNA,MAXNA,IANS,IWIDTH,ISUBN1,ISUBN2,   &
                  IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               *******************************************************
!               **  STEP 3--                                         **
!               **  EXAMINE THE FIRST  VARIABLE.                     **
!               **  IS IT IN THE TABLE?                              **
!               **  IS IT A VARIABLE?                                **
!               **  IVAR11 AND IVAR12 = THE NAME OF THE FIRST        **
!               **                      VARIABLE.                    **
!               **  ILIST1 = THE LINE IN THE INTERNAL TABLE          **
!               **           WHERE THE FIRST  VARIABLE IS FOUND.     **
!               **  ICOL1  = THE DATA COLUMN FOR THE FIRST  VARIABLE.**
!               **  N1     = THE NUMBER OF OBSERVATIONS FOR THE      **
!               **           FIRST  VARIABLE.                        **
!               *******************************************************
!
      ISTEPN='3'
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'APPE')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IVAR11=IHARG(1)
      IVAR12=IHARG2(1)
!
      DO 310 I=1,NUMNAM
      I2=I
      IF(IVAR11.EQ.IHNAME(I).AND.IVAR12.EQ.IHNAM2(I).AND.   &
      IUSE(I).EQ.'V')GO TO 380
      IF(IVAR11.EQ.IHNAME(I).AND.IVAR12.EQ.IHNAM2(I).AND.   &
      IUSE(I).NE.'V')GO TO 330
  310 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,321)
  321 FORMAT('***** ERROR IN DPAPPE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,322)
  322 FORMAT('      THE FIRST  VARIABLE NAME REFERENCED ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,323)IVAR11,IVAR12
  323 FORMAT('      (= ',A4,A4,')')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,324)
  324 FORMAT('      WAS NOT FOUND IN THE INTERNAL NAME TABLE,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,325)
  325 FORMAT('      SUGGESTED ACTION--USE THE STATUS COMMAND')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,326)
  326 FORMAT('      TO FIND OUT THE FULL LIST OF USED NAMES.')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
!
  330 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,331)
  331 FORMAT('***** ERROR IN DPAPPE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,332)
  332 FORMAT('      THE FIRST  VARIABLE NAME REFERENCED ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,333)IVAR11,IVAR12
  333 FORMAT('      (= ',A4,A4,')')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,334)
  334 FORMAT('      SHOULD HAVE BEEN A VARIABLE, BUT WAS NOT.')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
!
  380 CONTINUE
      ILIST1=I2
      ICOL1=IVALUE(ILIST1)
      N1=IN(ILIST1)
!
!               ****************************************************************
!               **  STEP 4--
!               **  EXAMINE THE SECOND VARIABLE.
!               **  IS IT IN THE TABLE?
!               **  IS IT A VARIABLE?
!               **  IVAR21 AND IVAR22 = THE NAME OF THE SECOND VARIABLE.
!               **  ILIST2 = THE LINE IN THE INTERNAL TABLE
!               **           WHERE THE SECOND VARIABLE IS FOUND.
!               **  ICOL2  = THE DATA COLUMN FOR THE SECOND VARIABLE.
!               **  N2     = THE NUMBER OF OBSERVATIONS FOR THE SECOND VARIABLE.
!               ****************************************************************
!
      ISTEPN='4'
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'APPE')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IVAR21=IHARG(2)
      IVAR22=IHARG2(2)
!
      DO 410 I=1,NUMNAM
      I2=I
      IF(IVAR21.EQ.IHNAME(I).AND.IVAR22.EQ.IHNAM2(I).AND.   &
      IUSE(I).EQ.'V')GO TO 480
      IF(IVAR21.EQ.IHNAME(I).AND.IVAR22.EQ.IHNAM2(I).AND.   &
      IUSE(I).NE.'V')GO TO 430
  410 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,421)
  421 FORMAT('***** ERROR IN DPAPPE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,422)
  422 FORMAT('      THE SECOND VARIABLE NAME REFERENCED ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,423)IVAR21,IVAR22
  423 FORMAT('      (= ',A4,A4,')')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,424)
  424 FORMAT('      WAS NOT FOUND IN THE INTERNAL NAME TABLE,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,425)
  425 FORMAT('      SUGGESTED ACTION--USE THE STATUS COMMAND')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,426)
  426 FORMAT('      TO FIND OUT THE FULL LIST OF USED NAMES.')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
!
  430 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,431)
  431 FORMAT('***** ERROR IN DPAPPE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,432)
  432 FORMAT('      THE SECOND VARIABLE NAME REFERENCED ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,433)IVAR21,IVAR22
  433 FORMAT('      (= ',A4,A4,')')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,434)
  434 FORMAT('      SHOULD HAVE BEEN A VARIABLE, BUT WAS NOT.')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
!
  480 CONTINUE
      ILIST2=I2
      ICOL2=IVALUE(ILIST2)
      N2=IN(ILIST2)
!
      ISTEPN='6'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!               ***********************************************
!               **  STEP 6--                                 **
!               **  DO A PRELIMINARY CHECK--                 **
!               **  WILL APPENDING VARIABLE 1 TO VARIABLE 2  **
!               **  MAKE VARIABLE 2 TOO LONG?                **
!               **  (THAT IS, WILL IT EXCEED MAXN)?          **
!               ***********************************************
!
      N1PN2=N1+N2
      IF(N1PN2.LE.MAXN)GO TO 690
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,621)
  621 FORMAT('***** ERROR IN DPAPPE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,622)IVAR11,IVAR12
  622 FORMAT('      THE APPENDING OF VARIABLE ',A4,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,623)IVAR21,IVAR22
  623 FORMAT('      TO VARIABLE ',A4,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,624)IVAR21,IVAR22
  624 FORMAT('      WILL MAKE VARIABLE ',A4,A4,' TOO LONG.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,625)IVAR11,IVAR12,N1
  625 FORMAT('      NUMBER OF OBSERVATIONS IN ',A4,A4,' = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,626)IVAR21,IVAR22,N2
  626 FORMAT('      NUMBER OF OBSERVATIONS IN ',A4,A4,' = ' ,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,627)IVAR11,IVAR12,N1PN2
  627 FORMAT('      NEW NUMBER OF OBSERVATIONS IN ',A4,A4,   &
      ' WOULD = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,628)MAXN
  628 FORMAT('      ALLOWABLE NUMBER OF OBSERVATIONS    = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,629)
  629 FORMAT('      THEREFORE, NO APPENDING CARRIED OUT.')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
!
  690 CONTINUE
!
!               ****************************************************
!               **  STEP 10--                                     **
!               **  APPEND VARIABLE 1 BY VARIABLE 2               **
!               ****************************************************
!
      ISTEPN='10'
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'APPE')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      N2PI=0
      DO 2100 I=1,N1
        N2PI=N2+I
        IJ1=MAXN*(ICOL2-1)+N2PI
        IJ2=MAXN*(ICOL1-1)+I
        IF(ICOL2.LE.MAXCOL)V(IJ1)=V(IJ2)
        IF(ICOL2.EQ.MAXCP1)PRED(N2PI)=Y(IJ2)
        IF(ICOL2.EQ.MAXCP2)RES(N2PI)=Y(IJ2)
 2100 CONTINUE
      N2NEW=N2PI
!
!               *******************************************
!               **  STEP 11--                            **
!               **  CARRY OUT THE LIST UPDATING AND      **
!               **  GENERATE THE INFORMATIVE PRINTING.   **
!               *******************************************
!
      ISTEPN='11'
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'APPE')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IHNAME(ILIST2)=IVAR21
      IHNAM2(ILIST2)=IVAR22
      IUSE(ILIST2)='V'
      IVALUE(ILIST2)=ICOL2
      VALUE(ILIST2)=ICOL2
      IN(ILIST2)=N2NEW
!
      DO 2400 J4=1,NUMNAM
      IF(IUSE(J4).EQ.'V'.AND.IVALUE(J4).EQ.ICOL2)GO TO 2405
      GO TO 2400
 2405 CONTINUE
      IUSE(J4)='V'
      IVALUE(J4)=ICOL2
      VALUE(J4)=ICOL2
      IN(J4)=N2NEW
 2400 CONTINUE
!
      IF(IPRINT.EQ.'OFF')GO TO 2459
      IF(IFEEDB.EQ.'OFF')GO TO 2459
      NNUM=N1
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2411)IVAR21,IVAR22,NNUM
 2411 FORMAT('THE NUMBER OF VALUES ADDED TO ',   &
      'THE VARIABLE ',A4,A4,' = ',I8)
      CALL DPWRST('XXX','BUG ')
!
      IROW1=N2+1
      IROWN=N2+N1
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IJ=MAXN*(ICOL2-1)+IROW1
      IF(ICOL2.LE.MAXCOL)WRITE(ICOUT,2421)IVAR21,IVAR22,V(IJ),IROW1
      IF(ICOL2.LE.MAXCOL)CALL DPWRST('XXX','BUG ')
      IF(ICOL2.EQ.MAXCP1)WRITE(ICOUT,2421)IVAR21,IVAR22,PRED(IROW1),   &
      IROW1
      IF(ICOL2.EQ.MAXCP1)CALL DPWRST('XXX','BUG ')
      IF(ICOL2.EQ.MAXCP2)WRITE(ICOUT,2421)IVAR21,IVAR22,RES(IROW1),   &
      IROW1
 2421 FORMAT('THE FIRST           VALUE ADDED TO ',A4,A4,   &
      ' = ',E15.7,'   (ROW ',I6,')')
      IF(ICOL2.EQ.MAXCP2)CALL DPWRST('XXX','BUG ')
      IJ=MAXN*(ICOL2-1)+IROWN
      IF(ICOL2.LE.MAXCOL.AND.   &
      NNUM.NE.1)WRITE(ICOUT,2431)NNUM,IVAR21,IVAR22,V(IJ),IROWN
 2431 FORMAT('THE LAST (',I5,'-TH) VALUE ADDED TO ',A4,A4,   &
      ' = ',E15.7,'   (ROW ',I6,')')
      IF(ICOL2.LE.MAXCOL.AND.   &
      NNUM.NE.1)CALL DPWRST('XXX','BUG ')
      IF(ICOL2.EQ.MAXCP1.AND.   &
      NNUM.NE.1)WRITE(ICOUT,2431)NNUM,IVAR21,IVAR22,PRED(IROWN),IROWN
      IF(ICOL2.LE.MAXCOL.AND.   &
      NNUM.NE.1)CALL DPWRST('XXX','BUG ')
      IF(ICOL2.EQ.MAXCP2.AND.   &
      NNUM.NE.1)WRITE(ICOUT,2431)NNUM,IVAR21,IVAR22,RES(IROWN),IROWN
      IF(ICOL2.EQ.MAXCP2.AND.   &
      NNUM.NE.1)CALL DPWRST('XXX','BUG ')
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2453)IVAR21,IVAR22,N2NEW
 2453 FORMAT('THE NEW     LENGTH OF  ',   &
      'THE VARIABLE ',A4,A4,' = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
 2459 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'APPE')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPAPPE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)IVAR11,IVAR12,ILIST1,ICOL1,N1
 9021   FORMAT('IVAR11,IVAR12,ILIST1,ICOL1,N1 = ',2(A4,2X),3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9022)IVAR22,IVAR22,ILIST2,ICOL2,N2
 9022   FORMAT('IVAR22,IVAR22,ILIST2,ICOL2,N2 = ',2(A4,2X),3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9023)N1PI,N1PN2,N1NEW,IROW1,IROWN,IJ1,IJ2
 9023   FORMAT('N1PI,N1PN2,N1NEW,IROW1,IROWN,IJ1,IJ2 = ',6I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPAPPE
      SUBROUTINE DPARC(IHARG,IARGT,ARG,NUMARG,   &
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
!     PURPOSE--DRAW ONE OR MORE ARCS (DEPENDING ON HOW MANY NUMBERS ARE
!              PROVIDED).  THE COORDINATES ARE IN STANDARDIZED UNITS
!              OF 0 TO 100.
!     NOTE--THE INPUT COORDINATES DEFINE 3 SUCCESSIVE POINTS ON THE ARC.
!     NOTE-THE USUAL INPUT NUMBER OF COORDINATES IS 3 AND THEREFORE THE
!           USUAL INPUT NUMBER OF NUMBERS IS 2*3 = 6.
!     NOTE--IF 4 NUMBERS ARE PROVIDED, THEN THE DRAWN ARC WILL GO FROM
!           THE LAST CURSOR POSITION THROUGH THE (X,Y) POINT (EITHER
!           ABSOLUTE OR RELATIVE) AS DEFINED BY THE FIRST AND SECOND
!           NUMBERS TO THE (X,Y) POINT (EITHER ABSOLUTE OR RELATIVE)
!           AS DEFINED BY THE THIRD AND FOURTH NUMBERS.
!     NOTE--IF 6 NUMBERS ARE PROVIDED, THEN THE DRAWN ARC WILL GO FROM
!           THE ABSOLUTE (X,Y) POSITION AS RESULTING FORM THE FIRST AND
!           SECOND NUMBERS THROUGH THE (X,Y) POINT (EITHER ABSOLUTE OR
!           RELATIVE) AS DEFINED BY THE THIRD AND FOURTH NUMBERS TO THE
!           (X,Y) POINT (EITHER ABSOLUTE OR RELATIVE) AS DEFINED BY THE
!           FIFTH AND SIXTH NUMBERS
!     NOTE--AND SO FORTH FOR 10, 14, 18, ... NUMBERS.
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
!                 PHONE--301-975-2855
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
!     UPDATED         --DECEMBER  2018. SUPPORT FOR "DEVICE ... SCALE"
!                                       COMMAND
!     UPDATED         --JUNE      2019. CREATE SCRATCH STORAGE IN DPARC
!                                       RATHER THAN DPARC2
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
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
!
      DIMENSION PX(1000)
      DIMENSION PY(1000)
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(IGARB1),PX(1))
      EQUIVALENCE (GARBAG(IGARB2),PY(1))
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
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
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'ARC')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPARC--')
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
      IFIG='ARC'
      NUMPT=3
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
          IF(IARGT(I).EQ.'NUMB')GO TO 1120
          GO TO 1130
 1120   CONTINUE
        IFOUND='YES'
!
!               ****************************
!               **  STEP 3--              **
!               **  DRAW OUT THE ARC(S)   **
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
        J=J+1
        IF(J.GT.NUMARG)GO TO 1190
        X3=ARG(J)
        IF(UNITSW.EQ.'DATA')CALL DPCODS('X',X3,X3,IBUGD2,ISUBRO,IERROR)
        IF(ITYPEO.EQ.'RELA')X3=X2+X3
        J=J+1
        IF(J.GT.NUMARG)GO TO 1190
        Y3=ARG(J)
        IF(UNITSW.EQ.'DATA')CALL DPCODS('Y',Y5,Y5,IBUGD2,ISUBRO,IERROR)
        IF(ITYPEO.EQ.'RELA')Y3=Y2+Y3
!
        CALL DPARC2(X1,Y1,X2,Y2,X3,Y3,   &
                    PX,PY,   &
                    IFIG,ILINPA,ILINCO,ILINC2,MAXLNZ,PLINTH,   &
                    AREGBA,IREBLI,IREBCO,IREBC2,MAXRGZ,PREBTH,   &
                    IREFSW,IREFCO,IREFC2,   &
                    IREPTY,IREPLI,IREPCO,IREPC2,PREPTH,PREPSP,   &
                    PTEXHE,PTEXWI,PTEXVG,PTEXHG)
!
        X1=X3
        Y1=Y3
!
        GO TO 1160
!
 1190   CONTINUE
!
        PXEND=X3
        PYEND=Y3
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
      GO TO 09000
!
!     ERROR SECTION
!
 1130 CONTINUE
      IERRG4='YES'
      WRITE(ICOUT,1131)
 1131 FORMAT('***** ERROR IN ARC (DPARC)--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1132)
 1132 FORMAT('      ILLEGAL FORM FOR THE ARC COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1134)
 1134 FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE PROPER FORM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1135)
 1135 FORMAT('      SUPPOSE IT IS DESIRED TO DRAW AN ARC ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1136)
 1136 FORMAT('      WITH ONE END OF MAJOR AXIS AT THE POINT 20 20 ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1137)
 1137 FORMAT('      ONE END OF THE MINOR AXIS AT THE POINT 30 10')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1138)
 1138 FORMAT('      AND WITH THE OTHER END OF THE MAJOR AXIS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1139)
 1139 FORMAT('      AT THE POINT 40 20')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1141)
 1141 FORMAT('      THEN THE ALLOWABLE FORMS ARE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1142)
 1142 FORMAT('      ARC 20 20 30 10 40 20 ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1143)
 1143 FORMAT('      ARC ABSOLUTE 20 20 30 10 40 20 ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1145)
 1145 FORMAT('      ARC RELATIVE 20 20 30 10 40 20 ')
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'ARC')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPARC--')
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
      END SUBROUTINE DPARC
      SUBROUTINE DPARC2(X1,Y1,X2,Y2,X3,Y3,   &
                        PX,PY,   &
                        IFIG,ILINPA,ILINCO,ILINC2,MAXLN,PLINTH,   &
                        AREGBA,IREBLI,IREBCO,IREBC2,MAXRG,PREBTH,   &
                        IREFSW,IREFCO,IREFC2,   &
                        IREPTY,IREPLI,IREPCO,IREPC2,PREPTH,PREPSP,   &
                        PTEXHE,PTEXWI,PTEXVG,PTEXHG)
!
!     PURPOSE--DRAW A ARC WITH ONE END OF THE ARC AT (X1,Y1)
!              SOME MIDDLE POINT AT (X2,Y2),
!              AND THE OTHER END OF THE ARC AT (X3,Y3).
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--APRIL     1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --JANUARY   1989. MODIFY CALLS TO DPDRPL (ALAN)
!     UPDATED         --JANUARY   1989. MODIFY CALL  TO DPFIRE (ALAN)
!     UPDATED         --JUNE      2019. CREATE SCRATCH STORAGE IN DPARC
!                                       RATHER THAN DPARC2
!     UPDATED         --OCTOBER   2020. SUPPORT FOR RGB COLOR
!
!-----NON-COMMON VARIABLES-------------------------------------
!
      DIMENSION PX(*)
      DIMENSION PY(*)
!
      CHARACTER*4 IFIG
      CHARACTER*4 IPATT2
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
      CHARACTER*4 ICOLF
      CHARACTER*4 ICOLP
      CHARACTER*4 ICOL
      CHARACTER*4 IFLAG
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
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'ARC2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPARC2--')
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
!
!               *********************************
!               **  STEP 1--                   **
!               **  DETERMINE THE COORDINATES  **
!               **  FOR THE ARC                **
!               *********************************
!
      PI=3.1415926
!
      THETA=0.0
      THETA1=0.0
      THETA2=0.0
      THETA3=0.0
!
!               *******************************************************
!               **  STEP 1.1--                                       **
!               **  COMPUTE THE INTERCEPT AND SLOPE OF THE LINE      **
!               **  THROUGH THE MIDPOINT OF POINTS 1 AND 2 AND       **
!               **  PERPENDICULAR TO THE SEGMENT BETWEEN POINTS      **
!               **  1 AND 2.                                         **
!               *******************************************************
!
      DELX12=X2-X1
      DELY12=Y2-Y1
!
      IF(DELX12.EQ.0.0)THEN
        AM12=CPUMAX
        B12=CPUMAX
        AM12P=0.0
        B12P=Y1
      ELSEIF(DELY12.EQ.0.0)THEN
        AM12=0.0
        B12=Y1
        AM12P=CPUMAX
        B12P=CPUMAX
      ELSE
        AM12=DELY12/DELX12
        B12=-AM12*X1+Y1
        X12=(X1+X2)/2.0
        Y12=(Y1+Y2)/2.0
        AM12P=-1.0/AM12
        B12P=-AM12P*X12+Y12
      ENDIF
!
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'ARC2')THEN
        WRITE(ICOUT,716)DELX12,DELY12,B12,AM12,B12P,AM12P
  716   FORMAT('DELX12,DELY12,B12,AM12,B12P,AM12P = ',6G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *******************************************************
!               **  STEP 1.2--                                       **
!               **  COMPUTE THE INTERCEPT AND SLOPE OF THE LINE      **
!               **  THROUGH THE MIDPOINT OF POINTS 2 AND 3           **
!               **  AND PERPENDICULAR TO THE SEGMENT BETWEEN POINTS  **
!               **  2 AND 3.                                         **
!               *******************************************************
!
      DELX23=X3-X2
      DELY23=Y3-Y2
!
      IF(DELX23.EQ.0.0)THEN
        AM23=CPUMAX
        B23=CPUMAX
        AM23P=0.0
        B23P=Y2
      ELSEIF(DELY23.EQ.0.0)THEN
        AM23=0.0
        B23=Y2
        AM23P=CPUMAX
        B23P=CPUMAX
      ELSE
        AM23=DELY23/DELX23
        B23=-AM23*X2+Y2
        X23=(X2+X3)/2.0
        Y23=(Y2+Y3)/2.0
        AM23P=-1.0/AM23
        B23P=-AM23P*X23+Y23
      ENDIF
!
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'ARC2')THEN
        WRITE(ICOUT,726)DELX23,DELY23,B23,AM23,B23P,AM23P
  726   FORMAT('DELX23,DELY23,B23,AM23,B23P,AM23P = ',6G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ***************************************************
!               **  STEP 1.3--                                   **
!               **  COMPUTE THE COORDINATES OF THE CENTER POINT  **
!               **  OF THE CIRCLE DEFINED BY THE 3 ARC POINTS.   **
!               ***************************************************
!
      ANUM=-(B12P-B23P)
      ADEN=AM12P-AM23P
      XCENT=CPUMAX
      IF(ADEN.NE.0.0)XCENT=ANUM/ADEN
      YCENT=AM12P*XCENT+B12P
!
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'ARC2')THEN
        WRITE(ICOUT,731)ANUM,ADEN,XCENT,YCENT
  731   FORMAT('ANUM,ADEN,XCENT,YCENT = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ****************************************************
!               **  STEP 1.4--                                    **
!               **  COMPUTE THE ANGLE OF ROTATION OF THE FIGURE.  **
!               ****************************************************
!
      DELX=X3-X1
      DELY=Y3-Y1
!
      IF(ABS(DELX).GE.0.00001.AND.DELX.LT.0.0)THEN
        THETA=PI+ATAN(DELY/DELX)
      ELSEIF(ABS(DELX).GE.0.00001.AND.DELX.GT.0.0)THEN
        THETA=ATAN(DELY/DELX)
      ELSEIF(ABS(DELX).LT.0.00001.AND.DELY.LT.0.0)THEN
        THETA=1.5*(PI/2.0)
      ELSEIF(ABS(DELX).LT.0.00001.AND.DELX.EQ.0.0)THEN
        THETA=PI/2.0
      ELSEIF(ABS(DELX).LT.0.00001.AND.DELY.GT.0.0)THEN
        THETA=PI/2.0
      ENDIF
!
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'ARC2')THEN
        WRITE(ICOUT,741)DELX,DELY,THETA
  741   FORMAT('DELX,DELY,THETA = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ********************************************************
!               **  STEP 1.5--                                        **
!               **  COMPUTE THE RADIUS OF THE CIRCLE.                 **
!               **  COMPUTE THE ANGLE FROM THE CENTER POINT TO POINT 1.*
!               ********************************************************
!
      DELXC1=2.0*(X1-XCENT)
      DELYC1=2.0*(Y1-YCENT)
      ALEN=0.0
      TERM=DELXC1**2+DELYC1**2
      IF(TERM.GT.0.0)ALEN=SQRT(TERM)
      R=ALEN/2.0
!
      IF(ABS(DELXC1).GE.0.00001.AND.DELXC1.GE.0.0)THEN
        THETA1=ATAN(DELYC1/DELXC1)
      ELSEIF(ABS(DELXC1).GE.0.00001.AND.DELXC1.LT.0.0)THEN
        THETA1=PI+ATAN(DELYC1/DELXC1)
      ELSEIF(ABS(DELXC1).LT.0.00001.AND.DELYC1.GE.0.0)THEN
        THETA1=PI/2.0
      ELSEIF(ABS(DELXC1).LT.0.00001.AND.DELYC1.LT.0.0)THEN
        THETA1=1.5*(PI/2.0)
      ENDIF
      IF(THETA1.LT.0.0)THETA1=THETA1+2.0*PI
!
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'ARC2')THEN
        WRITE(ICOUT,751)ALEN,R
  751   FORMAT('ALEN,R = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,752)DELXC1,DELYC1,THETA1
  752   FORMAT('DELXC1,DELYC1,THETA1 = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ********************************************************
!               **  STEP 1.6--                                         *
!               **  COMPUTE THE ANGLE FROM THE CENTER POINT TO POINT 2.*
!               ********************************************************
!
      DELXC2=2.0*(X2-XCENT)
      DELYC2=2.0*(Y2-YCENT)
      IF(ABS(DELXC2).GE.0.00001.AND.DELXC2.GE.0.0)THEN
        THETA2=ATAN(DELYC2/DELXC2)
      ELSEIF(ABS(DELXC2).GE.0.00001.AND.DELXC2.LT.0.0)THEN
        THETA2=PI+ATAN(DELYC2/DELXC2)
      ELSEIF(ABS(DELXC2).LT.0.00001.AND.DELYC2.GE.0.0)THEN
        THETA2=PI/2.0
      ELSEIF(ABS(DELXC2).LT.0.00001.AND.DELYC2.LT.0.0)THEN
        THETA2=1.5*(PI/2.0)
      ENDIF
      IF(THETA2.LT.0.0)THETA2=THETA2+2.0*PI
!
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'ARC2')THEN
        WRITE(ICOUT,761)DELXC2,DELYC2,THETA2
  761   FORMAT('DELXC2,DELYC2,THETA2 = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *********************************************************
!               **  STEP 1.7--                                         **
!               **  COMPUTE THE ANGLE FROM THE CENTER POINT TO POINT 3.**
!               *********************************************************
!
      DELXC3=2.0*(X3-XCENT)
      DELYC3=2.0*(Y3-YCENT)
      IF(ABS(DELXC3).GE.0.00001.AND.DELXC3.GE.0.0)THEN
        THETA3=ATAN(DELYC3/DELXC3)
      ELSEIF(ABS(DELXC3).GE.0.00001.AND.DELXC3.LT.0.0)THEN
        THETA3=PI+ATAN(DELYC3/DELXC3)
      ELSEIF(ABS(DELXC3).LT.0.00001.AND.DELYC3.GE.0.0)THEN
        THETA3=PI/2.0
      ELSEIF(ABS(DELXC3).LT.0.00001.AND.DELYC3.LT.0.0)THEN
        THETA3=1.5*(PI/2.0)
      ENDIF
      IF(THETA3.LT.0.0)THETA3=THETA3+2.0*PI
!
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'ARC2')THEN
        WRITE(ICOUT,771)DELXC3,DELYC3,THETA3
  771   FORMAT('DELXC3,DELYC3,THETA3 = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ******************************
!               **  STEP 1.8--              **
!               **  COMPUTE THE ARC POINTS  **
!               ******************************
!
      K=0
!
      K=K+1
      PX(K)=X1
      PY(K)=Y1
!
      IF(THETA1.LE.THETA3.AND.THETA3.LE.THETA2)THEN
        THETA1=THETA1+2.0*PI
      ELSEIF(THETA2.LE.THETA1.AND.THETA1.LE.THETA3)THEN
        THETA1=THETA1+2.0*PI
        THETA2=THETA2+2.0*PI
      ELSEIF(THETA3.LE.THETA1.AND.THETA1.LE.THETA2)THEN
        THETA1=THETA1+2.0*PI
      ELSEIF(THETA2.LE.THETA3.AND.THETA3.LE.THETA1)THEN
        THETA2=THETA2+2.0*PI
        THETA3=THETA3+2.0*PI
      ENDIF
!
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'ARC2')THEN
        WRITE(ICOUT,3009)THETA1,THETA2,THETA3
 3009   FORMAT('THETA1,THETA2,THETA3 = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      DELTHE=THETA3-THETA1
      IMAX=101
      AIMAX=IMAX
      DO 3010 I=1,IMAX
      AI=I
      P=(AI-1.0)/(AIMAX-1.0)
      PHI2=THETA1+P*DELTHE
      X=XCENT+R*COS(PHI2)
      Y=YCENT+R*SIN(PHI2)
      K=K+1
      PX(K)=X
      PY(K)=Y
 3010 CONTINUE
!
      NP=K
!
!               ***********************
!               **  STEP 2--         **
!               **  FILL THE FIGURE  **
!               **  (IF CALLED FOR)  **
!               ***********************
!
      IF(IREFSW(1).EQ.'OFF')GO TO 2190
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
      CALL DPFIRE(PX,PY,NP,   &
                  IFIG,IPATT,PTHICK,PXGAP,PYGAP,   &
                  ICOLF,ICOLFR,ICOLFG,ICOLFB,   &
                  ICOLP,ICOLPR,ICOLPG,ICOLPB,   &
                  IPATT2)
 2190 CONTINUE
!
!               ***************************
!               **  STEP 3--             **
!               **  DRAW OUT THE FIGURE  **
!               ***************************
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
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'ARC2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPARC2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)XCENT,YCENT,R,NP,IERRG4
 9012   FORMAT('XCENT,YCENT,R,NP,IERRG4 = ',3G15.7,I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,NP
          WRITE(ICOUT,9016)I,PX(I),PY(I)
 9016     FORMAT('I,PX(I),PY(I) = ',I8,2E15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPARC2
      SUBROUTINE DPARCL(IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,   &
                        IDEFCO,MAXARR,IARRCO,IARRC2,IRGBMX,   &
                        IBUGP2,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE COLOR FOR A ARROW.  THE COLOR FOR ARROW I
!              WILL BE PLACED IN THE I-TH ELEMENT OF THE CHARACTER
!              VECTOR IARRCO(.).
!     INPUT  ARGUMENTS--IHARG  (A CHARACTER VECTOR)
!                     --IARGT  (A CHARACTER VECTOR)
!                     --IARG   (A CHARACTER VECTOR)
!                     --NUMARG
!                     --IDEFCO
!                     --MAXARR
!     OUTPUT ARGUMENTS--IARRCO (A CHARACTER VECTOR WHOSE I-TH ELEMENT
!                              CONTAINS THE COLOR FOR ARROW I.
!                     --IFOUND ('YES' OR 'NO' )
!                     --IERROR ('YES' OR 'NO' )
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABOARATORY
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
      CHARACTER*4 IDEFCO
      CHARACTER*4 IBUGP2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!---------------------------------------------------------------------
!
      CHARACTER*4 IHARG(*)
      CHARACTER*4 IHARG2(*)
      CHARACTER*4 IARGT(*)
      CHARACTER*4 IARRCO(*)
      DIMENSION IARG(*)
      DIMENSION ARG(*)
      DIMENSION IARRC2(MAXARR,3)
!
      CHARACTER*4 IHOLD
      CHARACTER*4 ICASCL
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      ICASCL='STAN'
      ISUBN1='DPAR'
      ISUBN2='CL  '
      IHOLD='    '
      JHOLD1=-1
      JHOLD2=-1
      JHOLD3=-1
!
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'ARCL')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPARCL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGP2,ISUBRO,IFOUND,IERROR
   52   FORMAT('IBUGP2,ISUBRO,IFOUND,IERROR = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IDEBCO,MAXARR,NUMARR,NUMARG,IRGBMX
   53   FORMAT('IDEBCO,MAXARR,NUMARR,NUMARG,IRGBMX = ',A4,2X,4I8)
        CALL DPWRST('XXX','BUG ')
        DO 65 I=1,NUMARG
          WRITE(ICOUT,66)I,IARG(I),IHARG(I)
   66     FORMAT('I,IARG(I),IHARG(I) = ',2I8,2X,A4)
          CALL DPWRST('XXX','BUG ')
   65   CONTINUE
        DO 75 I=1,10
          WRITE(ICOUT,76)I,IARRCO(I),IARRC2(I,1),IARRC2(I,2),IARRC2(I,3)
   76     FORMAT('I,IARRCO(I),IARRC2(I,1),IARRC2(I,2),IARRC2(I,3) = ',   &
                 I8,2X,A4,2X,3I5)
          CALL DPWRST('XXX','BUG ')
   75   CONTINUE
      ENDIF
!
      ISTEPN='1'
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'ARCL')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.EQ.0)GO TO 9000
      IF(IHARG(1).EQ.'RGB ' .AND. IHARG(2).EQ.'COLO')THEN
        ICASCL='RGB '
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGP2,IERROR)
      ELSEIF(IHARG(2).EQ.'RGB ' .AND. IHARG(3).EQ.'COLO')THEN
        ICASCL='RGB '
        DO 45 II=2,NUMARG-1
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
!
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COLO')THEN
!
      ISTEPN='2'
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'ARCL')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        IF(NUMARG.LE.1        .OR. IHARG(2).EQ.'ON'   .OR.   &
           IHARG(2).EQ.'OFF'  .OR. IHARG(2).EQ.'AUTO' .OR.   &
           IHARG(2).EQ.'DEFA')THEN
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
!
        IFOUND='YES'
        IF(ICASCL.EQ.'RGB ')THEN
          DO 1138 I=1,MAXARR
            IARRC2(I,1)=JHOLD1
            IARRC2(I,2)=JHOLD2
            IARRC2(I,3)=JHOLD3
 1138     CONTINUE
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            I=1
            WRITE(ICOUT,1139)IARRC2(I,1),IARRC2(I,2),IARRC2(I,3)
 1139       FORMAT('ALL ARROW RGB COLORS HAVE JUST BEEN SET TO ',3I5)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSE
          DO 1135 I=1,MAXARR
            IARRCO(I)=IHOLD
 1135     CONTINUE
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            I=1
            WRITE(ICOUT,1136)IARRCO(I)
 1136       FORMAT('ALL ARROW COLORS HAVE JUST BEEN SET TO ',A4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ENDIF
        GO TO 9000
!
      ELSEIF(NUMARG.GE.2.AND.IHARG(2).EQ.'COLO')THEN
!
      ISTEPN='3'
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'ARCL')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        IF(IARGT(1).NE.'NUMB')THEN
          IERROR='YES'
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1141)
 1141     FORMAT('***** ERROR IN ARROW COLOR (DPARCL)--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1142)
 1142     FORMAT('      IN THE ARROW ... COLOR COMMAND,')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1143)
 1143     FORMAT('      THE ARROW IS IDENTIFIED BY A NUMBER, AS IN--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1144)
 1144     FORMAT('      ARROW 3 COLOR GREEN')
          CALL DPWRST('XXX','BUG ')
          GO TO 9000
        ENDIF
!
        I=IARG(1)
        IF(I.LT.1 .OR. I.GT.MAXARR)THEN
          IERROR='YES'
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1141)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1152)
 1152     FORMAT('      IN THE ARROW ... COLOR COMMAND,THE NUMBER ',   &
                 'OF ARROWS')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1154)MAXARR
 1154     FORMAT('      MUST BE BETWEEN 1 AND ',I8,' (INCLUSIVELY);')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1155)
 1155     FORMAT('      SUCH WAS NOT THE CASE HERE--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1156)I
 1156     FORMAT('      A REFERENCE WAS MADE TO THE ',I8,'-TH ARROW.')
          CALL DPWRST('XXX','BUG ')
          GO TO 9000
        ENDIF
!
        IF(NUMARG.LE.2        .OR. IHARG(3).EQ.'ON'   .OR.   &
           IHARG(3).EQ.'OFF'  .OR. IHARG(3).EQ.'AUTO' .OR.   &
           IHARG(3).EQ.'DEFA')THEN
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
!
        IFOUND='YES'
        IF(ICASCL.EQ.'RGB ')THEN
          IARRC2(I,1)=JHOLD1
          IARRC2(I,2)=JHOLD2
          IARRC2(I,3)=JHOLD3
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1286)I,IARRC2(I,1),IARRC2(I,2),IARRC2(I,3)
 1286       FORMAT('THE RGB COLOR FOR ARROW ',I8,   &
                   ' HAS JUST BEEN SET TO ',3I5)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSE
          IARRCO(I)=IHOLD
!
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1186)I,IARRCO(I)
 1186       FORMAT('THE COLOR FOR ARROW ',I8,' HAS JUST BEEN SET TO ',   &
                   A4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ENDIF
      ENDIF
!
 9000 CONTINUE
!
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'ARCL')THEN
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPARCL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR,NUMARR
 9012   FORMAT('IFOUND,IERROR,NUMARR = ',2(A4,2X),I5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IHOLD,JHOLD1,JHOLD2,JHOLD3
 9014   FORMAT('IHOLD,JHOLD1,JHOLD2,JHOLD3 = ',A4,2X,3I5)
        CALL DPWRST('XXX','BUG ')
        DO 9075 I=1,10
          WRITE(ICOUT,76)I,IARRCO(I),IARRC2(I,1),IARRC2(I,2),IARRC2(I,3)
          CALL DPWRST('XXX','BUG ')
 9075   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPARCL
      SUBROUTINE DPARCO(IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,IANS,IWIDTH,   &
      MAXARR,PARRXC,PARRYC,NUMARR,IBUGP2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE 2 PAIRS OF (X,Y) COORDINATES
!              FOR AN ARROW.
!              THE FIRST PAIR WILL BE FOR THE TAIL OF THE ARROW;
!              THE SECOND PAIR WILL BE FOR THE HEAD OF THE ARROW.
!              THE (X1,Y1), (X2,Y2) COORDINATES WILL BE PLACED IN THE
!              FIRST AND SECOND ELEMENTS (RESPECTIVELY) OF
!              THE 2 ARRAYS PARRXC(.,.) AND PARRYC(.,.)
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --IARGT  (A HOLLERITH VECTOR)
!                     --IARG   (A HOLLERITH VECTOR)
!                     --ARG    (A HOLLERITH VECTOR)
!                     --NUMARG
!                     --MAXARR
!     OUTPUT ARGUMENTS--PARRXC (A FLOATING POINT VECTOR
!                              WHOSE (I,1)-TH ELEMENT CONTAINS THE
!                              X COORDINATE FOR THE TAIL OF ARROW I;
!                              WHOSE (I,2)-TH ELEMENT CONTAINS THE
!                              X COORDINATE FOR THE HEAD OF ARROW I;
!                     --PARRYC (A FLOATING POINT VECTOR
!                              WHOSE (I,1)-TH ELEMENT CONTAINS THE
!                              Y COORDINATE FOR THE TAIL OF ARROW I;
!                              WHOSE (I,2)-TH ELEMENT CONTAINS THE
!                              Y COORDINATE FOR THE HEAD OF ARROW I;
!                     --NUMARR = THE NUMBER OF ARROWS DEFINED SO FAR
!                              (ACTUALLY, THE HIGHEST REFERENCED ARROW SO FAR)
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
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--SEPTEMBER 1980.
!     UPDATED         --MARCH     1981.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
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
!
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 IHWORD
      CHARACTER*4 IHWOR2
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
      DIMENSION IHARG2(*)
      DIMENSION IARGT(*)
      DIMENSION IARG(*)
      DIMENSION ARG(*)
!
      DIMENSION IHNAME(*)
      DIMENSION IHNAM2(*)
      DIMENSION IUSE(*)
      DIMENSION IN(*)
      DIMENSION IVALUE(*)
      DIMENSION VALUE(*)
      DIMENSION IANS(*)
!
      DIMENSION PARRXC(100,2)
      DIMENSION PARRYC(100,2)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPAR'
      ISUBN2='CO  '
      IFOUND='NO'
      IERROR='NO'
!
      HOLD1=0.0
      HOLD2=0.0
      HOLD3=0.0
      HOLD4=0.0
!
      IF(NUMARG.EQ.0)GO TO 9000
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COOR')GO TO 1110
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'COOR')GO TO 1140
      GO TO 9000
!
 1110 CONTINUE
      IF(NUMARG.LE.1)GO TO 1120
      IF(IHARG(2).EQ.'ON')GO TO 1120
      IF(IHARG(2).EQ.'OFF')GO TO 1120
      IF(IHARG(2).EQ.'AUTO')GO TO 1120
      IF(IHARG(2).EQ.'DEFA')GO TO 1120
      IF(NUMARG.GE.5)GO TO 1125
!
      IERROR='YES'
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1111)
 1111 FORMAT('***** ERROR IN DPARCO--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1112)
 1112 FORMAT('      IN THE ARROW ... COORDINATES COMMAND,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1113)
 1113 FORMAT('      THE COORDINATES ARE SPECIFIED BY 4 NUMBERS, ',   &
      'AS IN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1114)
 1114 FORMAT('      ARROW 3 COORDINATES 30 80 31 79')
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
!
 1120 CONTINUE
      HOLD1=CPUMIN
      HOLD2=CPUMIN
      HOLD3=CPUMIN
      HOLD4=CPUMIN
      NUMARR=0
      GO TO 1130
!
 1125 CONTINUE
      DO 1126 J=2,5
      IF(IARGT(J).EQ.'NUMB')GO TO 1127
      GO TO 1128
 1127 CONTINUE
      IF(J.EQ.2)HOLD1=ARG(J)
      IF(J.EQ.3)HOLD2=ARG(J)
      IF(J.EQ.4)HOLD3=ARG(J)
      IF(J.EQ.5)HOLD4=ARG(J)
      GO TO 1126
 1128 CONTINUE
      IHWORD=IHARG(J)
      IHWOR2=IHARG2(J)
      IHWUSE='P'
      MESSAG='YES'
      CALL CHECKN(IHWORD,IHWOR2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      IF(J.EQ.2)HOLD1=VALUE(ILOC)
      IF(J.EQ.3)HOLD2=VALUE(ILOC)
      IF(J.EQ.4)HOLD3=VALUE(ILOC)
      IF(J.EQ.5)HOLD4=VALUE(ILOC)
 1126 CONTINUE
      NUMARR=MAXARR
      GO TO 1130
!
 1130 CONTINUE
      IFOUND='YES'
      DO 1135 I=1,MAXARR
      PARRXC(I,1)=HOLD1
      PARRYC(I,1)=HOLD2
      PARRXC(I,2)=HOLD3
      PARRYC(I,2)=HOLD4
 1135 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1139
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1136)
 1136 FORMAT('ALL ARROW COORDINATES HAVE JUST BEEN SET TO--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1137)PARRXC(I,1),PARRYC(I,1)
 1137 FORMAT('    (X,Y) FOR TAIL OF ARROW = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1138)PARRXC(I,2),PARRYC(I,2)
 1138 FORMAT('    (X,Y) FOR HEAD OF ARROW = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
 1139 CONTINUE
      GO TO 9000
!
 1140 CONTINUE
      IF(IARGT(1).EQ.'NUMB')GO TO 1150
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1141)
 1141 FORMAT('***** ERROR IN DPARCO--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1142)
 1142 FORMAT('      IN THE ARROW ... COORDINATES COMMAND,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1143)
 1143 FORMAT('      THE ARROW IS IDENTIFIED BY A NUMBER, AS IN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1144)
 1144 FORMAT('      ARROW 3 COORDINATES 30 80 31 79')
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
!
 1150 CONTINUE
      I=IARG(1)
      IF(1.LE.I.AND.I.LE.MAXARR)GO TO 1160
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1151)
 1151 FORMAT('***** ERROR IN DPARCO--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1152)
 1152 FORMAT('      IN THE ARROW ... COORDINATES COMMAND,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1153)
 1153 FORMAT('      THE NUMBER OF ARROWS MUST BE ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1154)MAXARR
 1154 FORMAT('      BETWEEN 1 AND ',I8,' (INCLUSIVELY);')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1155)
 1155 FORMAT('      SUCH WAS NOT THE CASE HERE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1156)I
 1156 FORMAT('      A REFERENCE WAS MADE TO THE ',I8,'-TH ',   &
      'ARROW.')
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
!
 1160 CONTINUE
      IF(NUMARG.LE.2)GO TO 1170
      IF(IHARG(3).EQ.'ON')GO TO 1170
      IF(IHARG(3).EQ.'OFF')GO TO 1170
      IF(IHARG(3).EQ.'AUTO')GO TO 1170
      IF(IHARG(3).EQ.'DEFA')GO TO 1170
      IF(NUMARG.GE.6)GO TO 1175
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
      GO TO 9000
!
 1170 CONTINUE
      HOLD1=CPUMIN
      HOLD2=CPUMIN
      HOLD3=CPUMIN
      HOLD4=CPUMIN
      IF(I.EQ.NUMARR)NUMARR=I-1
      GO TO 1180
!
 1175 CONTINUE
      DO 1176 J=3,6
      IF(IARGT(J).EQ.'NUMB')GO TO 1177
      GO TO 1178
 1177 CONTINUE
      IF(J.EQ.3)HOLD1=ARG(J)
      IF(J.EQ.4)HOLD2=ARG(J)
      IF(J.EQ.5)HOLD3=ARG(J)
      IF(J.EQ.6)HOLD4=ARG(J)
      GO TO 1176
 1178 CONTINUE
      IHWORD=IHARG(J)
      IHWOR2=IHARG2(J)
      IHWUSE='P'
      MESSAG='YES'
      CALL CHECKN(IHWORD,IHWOR2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      IF(J.EQ.3)HOLD1=VALUE(ILOC)
      IF(J.EQ.4)HOLD2=VALUE(ILOC)
      IF(J.EQ.5)HOLD3=VALUE(ILOC)
      IF(J.EQ.6)HOLD4=VALUE(ILOC)
 1176 CONTINUE
      IF(I.GT.NUMARR)NUMARR=I
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
      PARRXC(I,1)=HOLD1
      PARRYC(I,1)=HOLD2
      PARRXC(I,2)=HOLD3
      PARRYC(I,2)=HOLD4
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1186)I
 1186 FORMAT('THE COORDINATES FOR ARROW ',I8,   &
      ' HAVE JUST BEEN SET TO--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1137)PARRXC(I,1),PARRYC(I,1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1138)PARRXC(I,2),PARRYC(I,2)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGP2.EQ.'OFF')GO TO 9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPARCO--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IFOUND,IERROR
 9012 FORMAT('IFOUND,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPARCO
      SUBROUTINE DPARMA(MAXNXT,   &
                        ISUBRO,IBUGA2,IBUGA3,IBUGQ,IFOUND,IERROR)
!
!     PURPOSE--CARRY OUT AN ARIMA ANALYSIS (1-SAMPLE)
!     EXAMPLE--ARMA Y 1 0 1
!              THIS FITS AN AR(1) AND A MA(1) WITH NO DIFFERENCING.
!              THE DATAPLOT ARIMA MODEL ALLOWS UP TO 7 TERMS:
!                 ARMA P1 D1 Q1 P2 D2 Q2 S2
!              WHERE
!                 P1 = ORDER OF AUTOREGRESSIVE TERM
!                 D1 = NUMBER OF DIFFERENCES (TYPICALLY EITHER
!                      0 FOR NO DIFFERENCE, 1 FOR A SINGLE DIFFERENCE)
!                 Q1 = ORDER OF MOVING AVERAGE TERM
!                 S1 = SEASONAL PERIOD (THIS IS TYPICALLY 1, I.E.
!                      THIS IS THE NON-SEASONAL TERM)
!                      DATAPLOT ALWAYS SETS THIS TO 1 SO NOT
!                      ENTERED BY THE USER
!                 P1 = ORDER OF SEASONAL AUTOREGRESSIVE TERM
!                 D2 = NUMBER OF DIFFERENCING FOR SEASONAL TERM
!                 Q2 = ORDER OF SEASONAL MOVING AVERAGE
!                 S2 = PERIOD FOR SEASONAL DIFFERENCING
!
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--99/2
!     ORIGINAL VERSION--MAY       1999.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IBUGA2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGQ
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 MESSAG
      CHARACTER*4 IHWUSE
      CHARACTER*4 ICASEQ
      CHARACTER*4 IH11
      CHARACTER*4 IH12
      CHARACTER*4 IH21
      CHARACTER*4 IH22
      CHARACTER*4 IH31
      CHARACTER*4 IH32
      CHARACTER*4 IH41
      CHARACTER*4 IH42
      CHARACTER*4 IH51
      CHARACTER*4 IH52
      CHARACTER*4 IH61
      CHARACTER*4 IH62
      CHARACTER*4 IH71
      CHARACTER*4 IH72
      CHARACTER*4 IH81
      CHARACTER*4 IH82
      CHARACTER*4 IHP
      CHARACTER*4 IHP2
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBRO
      CHARACTER*4 IUSE1
      CHARACTER*4 IUSE2
      CHARACTER*4 IUSE3
      CHARACTER*4 IUSE4
      CHARACTER*4 IUSE5
      CHARACTER*4 IUSE6
      CHARACTER*4 IUSE7
      CHARACTER*4 IUSE8
      CHARACTER*4 IREPU
      CHARACTER*4 IRESU
      CHARACTER*4 IPVFLG
      CHARACTER*4 IFXFLG
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
!
      DOUBLE PRECISION Y1(MAXOBV)
      DOUBLE PRECISION STP(100)
      DOUBLE PRECISION SCALE(100)
      DOUBLE PRECISION PV(MAXOBV)
      DOUBLE PRECISION SDPV(MAXOBV)
      DOUBLE PRECISION SDRES(MAXOBV)
      DOUBLE PRECISION FCST(MAXOBV,1)
      DOUBLE PRECISION FCSTSD(MAXOBV,1)
      DOUBLE PRECISION DRES(MAXOBV)
      PARAMETER(MAXPAR=100)
      DOUBLE PRECISION VCV(MAXPAR,MAXPAR)
      DOUBLE PRECISION PAR(MAXPAR)
!
      INTEGER IFIXED(MAXOBV)
!
      DIMENSION PRED2(MAXOBV)
      DIMENSION RES2(MAXOBV)
!
      INCLUDE 'DPCOZD.INC'
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZI.INC'
!
      EQUIVALENCE (DGARBG(IDGAR1),Y1(1))
      EQUIVALENCE (DGARBG(IDGAR2),PV(1))
      EQUIVALENCE (DGARBG(IDGAR3),SDPV(1))
!
      EQUIVALENCE (GARBAG(IGARB1),PRED2(1))
      EQUIVALENCE (GARBAG(IGARB3),RES2(1))
      EQUIVALENCE (GARBAG(IGARB5),SDRES(1))
      EQUIVALENCE (GARBAG(IGARB7),DRES(1))
      EQUIVALENCE (GARBAG(IGARB9),VCV(1,1))
      EQUIVALENCE (GARBAG(JGAR11),STP(1))
      EQUIVALENCE (GARBAG(JGAR13),PAR(1))
      EQUIVALENCE (GARBAG(JGAR14),SCALE(1))
      EQUIVALENCE (GARBAG(JGAR16),FCST(1,1))
      EQUIVALENCE (GARBAG(JGAR18),FCSTSD(1,1))
!
      EQUIVALENCE (IGARBG(IIGAR1),IFIXED(1))
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPAR'
      ISUBN2='MA  '
      IFOUND='YES'
      IERROR='NO'
      IUSE1='-999'
      IUSE2='-999'
      ICASEQ='UNKN'
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
      N1=(-999)
      N2=(-999)
      NUMVAR=(-999)
      ILOCV=(-999)
      ICOL1=(-999)
      ICOL2=(-999)
      MINN2=2
      NLEFT=0
!
      VALUE1=(-999.0)
      VALUE2=(-999.0)
!
!               ********************************
!               **  TREAT THE ARMA    CASE    **
!               ********************************
!
      IF(ISUBRO.EQ.'ARMA' .OR. IBUGA2.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPARMA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ISUBRO,IBUGA2,IBUGA3,IBUGQ,MAXNXT
   52   FORMAT('ISUBRO,IBUGA2,IBUGA3,IBUGQ,MAXNXT = ',4(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *******************************************************
!               **  STEP 2--                                         **
!               **  CHECK FOR THE PROPER NUMBER OF INPUT ARGUMENTS.  **
!               *******************************************************
!
      ISTEPN='2'
      IF(ISUBRO.EQ.'ARMA' .OR. IBUGA2.EQ.'ON')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      MINNA=1
      MAXNA=100
      CALL CHECKA(NUMARG,MINNA,MAXNA,IANS,IWIDTH,ISUBN1,ISUBN2,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               ****************************************
!               **  STEP 11--                         **
!               **  CHECK THE VALIDITY OF ARGUMENT 1  **
!               **  (THIS SHOULD BE A VARIABLE)       **
!               ****************************************
!
      ISTEPN='11'
      IF(ISUBRO.EQ.'ARMA' .OR. IBUGA2.EQ.'ON')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IH11=IHARG(1)
      IH12=IHARG2(1)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IH11,IH12,IHWUSE,   &
                  IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                  ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      IUSE1=IUSE(ILOCV)
      ICOL1=IVALUE(ILOCV)
      N1=IN(ILOCV)
      NUMVAR=1
!
!               ********************************************************
!               **  STEP 12--                                         **
!               **  IF ARGUMENT 1 IS A VARIABLE,                      **
!               **  CHECK THAT THE INPUT NUMBER OF OBSERVATIONS (N1)  **
!               **  FOR ARGUMENT 1 IS 2 OR MORE.                      **
!               ********************************************************
!
      ISTEPN='12'
      IF(ISUBRO.EQ.'ARMA' .OR. IBUGA2.EQ.'ON')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IUSE1.NE.'V')GO TO 1290
      IF(N1.GE.MINN2)GO TO 1290
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1211)
 1211 FORMAT('***** ERROR IN DPARMA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1212)
 1212 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS FOR WHICH AN ARMA')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1214)
 1214 FORMAT('      ANALYSIS WAS TO HAVE BEEN CARRIED OUT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1215)MINN2
 1215 FORMAT('      MUST BE ',I8,' OR LARGER; SUCH WAS NOT THE CASE ',   &
             'HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1217)IH11,IH12,N1
 1217 FORMAT('      VARIABLE ',2A4,' HAS ',I8,' OBSERVATIONS.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1219)
 1219 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,1220)(IANS(I),I=1,MIN(80,IWIDTH))
 1220   FORMAT(80A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GO TO 9000
 1290 CONTINUE
!
!               ****************************************
!               **  STEP 22--                         **
!               **  CHECK THE VALIDITY OF ARGUMENT 2  **
!               **  (THIS SHOULD BE A                 **
!               **  A PARAMETER, OR A NUMBER).        **
!               ****************************************
!
      ISTEPN='22'
      IF(ISUBRO.EQ.'ARMA' .OR. IBUGA2.EQ.'ON')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.1)IORDAR=1
      IF(NUMARG.GE.2)THEN
         IH21=IHARG(2)
         IH22=IHARG2(2)
         IF(IARGT(2).EQ.'NUMB')THEN
            VALUE2=ARG(2)
            IORDAR=IARG(2)
            IUSE2='P'
         ENDIF
      ENDIF
!
!               ****************************************
!               **  STEP 23--                         **
!               **  CHECK THE VALIDITY OF ARGUMENT 3  **
!               **  (THIS SHOULD BE A                 **
!               **  A PARAMETER, OR A NUMBER).        **
!               ****************************************
!
      ISTEPN='23'
      IF(ISUBRO.EQ.'ARMA' .OR. IBUGA2.EQ.'ON')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.2)IDIFF=0
      IF(NUMARG.GE.3)THEN
         IH31=IHARG(3)
         IH32=IHARG2(3)
         IF(IARGT(3).EQ.'NUMB')THEN
            VALUE3=ARG(3)
            IDIFF=IARG(3)
            IUSE3='P'
         ENDIF
      ENDIF
!
!               ****************************************
!               **  STEP 24--                         **
!               **  CHECK THE VALIDITY OF ARGUMENT 4  **
!               **  (THIS SHOULD BE A                 **
!               **  A PARAMETER, OR A NUMBER).        **
!               ****************************************
!
      ISTEPN='24'
      IF(ISUBRO.EQ.'ARMA' .OR. IBUGA2.EQ.'ON')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.3)IORDMA=0
      IF(NUMARG.GE.4)THEN
         IH41=IHARG(4)
         IH42=IHARG2(4)
         IF(IARGT(4).EQ.'NUMB')THEN
            VALUE4=ARG(4)
            IORDMA=IARG(4)
            IUSE4='P'
         ENDIF
      ENDIF
!
!               ****************************************
!               **  STEP 25--                         **
!               **  CHECK THE VALIDITY OF ARGUMENT 5  **
!               **  (THIS SHOULD BE A                 **
!               **  A PARAMETER, OR A NUMBER).        **
!               ****************************************
!
      ISTEPN='25'
      IF(ISUBRO.EQ.'ARMA' .OR. IBUGA2.EQ.'ON')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.4)IORSAR=0
      IF(NUMARG.GE.5)THEN
         IH51=IHARG(5)
         IH52=IHARG2(5)
         IF(IARGT(5).EQ.'NUMB')THEN
            VALUE4=ARG(5)
            IORSAR=IARG(5)
            IUSE5='P'
         ENDIF
      ENDIF
!
!               ****************************************
!               **  STEP 26--                         **
!               **  CHECK THE VALIDITY OF ARGUMENT 6  **
!               **  (THIS SHOULD BE A                 **
!               **  A PARAMETER, OR A NUMBER).        **
!               ****************************************
!
      ISTEPN='26'
      IF(ISUBRO.EQ.'ARMA' .OR. IBUGA2.EQ.'ON')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.5)ISDIFF=0
      IF(NUMARG.GE.6)THEN
         IH61=IHARG(6)
         IH62=IHARG2(6)
         IF(IARGT(6).EQ.'NUMB')THEN
            VALUE6=ARG(6)
            ISDIFF=IARG(6)
            IUSE6='P'
         ENDIF
      ENDIF
!
!               ****************************************
!               **  STEP 27--                         **
!               **  CHECK THE VALIDITY OF ARGUMENT 7  **
!               **  (THIS SHOULD BE A                 **
!               **  A PARAMETER, OR A NUMBER).        **
!               ****************************************
!
      ISTEPN='27'
      IF(ISUBRO.EQ.'ARMA' .OR. IBUGA2.EQ.'ON')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.6)IORSMA=0
      IF(NUMARG.GE.7)THEN
         IH71=IHARG(7)
         IH72=IHARG2(7)
         IF(IARGT(7).EQ.'NUMB')THEN
            VALUE7=ARG(7)
            IORSMA=IARG(7)
            IUSE7='P'
         ENDIF
      ENDIF
!
!               ****************************************
!               **  STEP 28--                         **
!               **  CHECK THE VALIDITY OF ARGUMENT 8  **
!               **  (THIS SHOULD BE A                 **
!               **  A PARAMETER, OR A NUMBER).        **
!               ****************************************
!
      ISTEPN='28'
      IF(ISUBRO.EQ.'ARMA' .OR. IBUGA2.EQ.'ON')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.7)ISPER=12
      IF(NUMARG.GE.8)THEN
         IH81=IHARG(8)
         IH82=IHARG2(8)
         IF(IARGT(8).EQ.'NUMB')THEN
            VALUE8=ARG(8)
            ISPER=IARG(8)
            IUSE8='P'
         ENDIF
      ENDIF
!
!               *******************************************************
!               **  STEP 31--                                        **
!               **  FOR AN ARIMA ANALYSIS, CHECK TO SEE IF THE FIRST **
!               **  ARGUMENT IS A VARIABLE.                          **
!               *******************************************************
!
      ISTEPN='31'
      IF(ISUBRO.EQ.'ARMA' .OR. IBUGA2.EQ.'ON')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IUSE1.NE.'V')GO TO 3140
      GO TO 3190
!
 3140 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1211)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3142)
 3142 FORMAT('      FOR AN ARMA ANALYSIS, THE FIRST ARGUMENT MUST BE ',   &
             'A VARIABLE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3147)
 3147 FORMAT('      (AS OPPOSED TO A PARAMETER OR FUNCTION).')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3148)
 3148 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3149)
 3149 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,1220)(IANS(I),I=1,IWIDTH)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GO TO 9000
!
 3190 CONTINUE
!
!               *****************************************
!               **  STEP 40--                          **
!               **  CHECK TO SEE THE TYPE CASE--       **
!               **    1) UNQUALIFIED (THAT IS, FULL);  **
!               **    2) SUBSET/EXCEPT; OR             **
!               **    3) FOR.                          **
!               *****************************************
!
      ISTEPN='40'
      IF(ISUBRO.EQ.'ARMA' .OR. IBUGA2.EQ.'ON')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICASEQ='FULL'
      ILOCQ=NUMARG+1
      IF(NUMARG.LT.1)GO TO 4090
      DO 4000 J=1,NUMARG
      J1=J
      IF(IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ') GO TO 4010
      IF(IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  ') GO TO 4010
      IF(IHARG(J).EQ.'FOR '.AND.IHARG2(J).EQ.'    ') GO TO 4020
 4000 CONTINUE
      GO TO 4090
 4010 CONTINUE
      ICASEQ='SUBS'
      ILOCQ=J1
      GO TO 4090
 4020 CONTINUE
      ICASEQ='FOR'
      ILOCQ=J1
      GO TO 4090
 4090 CONTINUE
!
      IF(ISUBRO.EQ.'ARMA' .OR. IBUGA2.EQ.'ON')THEN
        WRITE(ICOUT,4091)NUMARG,ILOCQ
 4091   FORMAT('NUMARG,ILOCQ = ',2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ***********************************************
!               **  STEP 41--                                **
!               **  TEMPORARILY FORM THE VARIABLE Y(.)       **
!               **  WHICH WILL HOLD THE DATA FROM SAMPLE 1.  **
!               **  FORM THIS VARIABLE BY                    **
!               **  BRANCHING TO THE APPROPRIATE SUBCASE     **
!               **  (FULL, SUBSET, OR FOR).                  **
!               ***********************************************
!
      IF(IUSE1.NE.'V')GO TO 4190
!
      ISTEPN='41'
      IF(ISUBRO.EQ.'ARMA' .OR. IBUGA2.EQ.'ON')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICASEQ.EQ.'FULL')GO TO 4110
      IF(ICASEQ.EQ.'SUBS')GO TO 4120
      IF(ICASEQ.EQ.'FOR')GO TO 4130
!
 4110 CONTINUE
      DO 4115 I=1,N1
      ISUB(I)=1
 4115 CONTINUE
      NQ=N1
      GO TO 4150
!
 4120 CONTINUE
      NIOLD=N1
      CALL DPSUBS(NIOLD,ILOCS,NS,IBUGQ,IERROR)
      NQ=NIOLD
      GO TO 4150
!
 4130 CONTINUE
      NIOLD=N1
      CALL DPFOR(NIOLD,NFOR,IROW1,IROWN,   &
      NLOCAL,ILOCS,NS,IBUGQ,IERROR)
      NQ=NFOR
      GO TO 4150
!
 4150 CONTINUE
      IF(NQ.GE.MINN2)GO TO 4160
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1211)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4152)
 4152 FORMAT('      AFTER THE APPROPRIATE SUBSET HAS BEEN EXTRACTED,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4153)IH11,IH12
 4153 FORMAT('      THE NUMBER OF OBSERVATIONS REMAINING',   &
             'FROM VARIABLE ',2A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4154)
 4154 FORMAT('      (FOR WHICH AN ARMA ANALYSIS IS TO BE CARRIED OUT(')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4156)MINN2
 4156 FORMAT('      MUST BE ',I8,' OR LARGER;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4157)NQ
 4157 FORMAT('      SUCH WAS NOT THE CASE HERE.  (N = ',I8,')')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4158)
 4158 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,1220)(IANS(I),I=1,MIN(80,IWIDTH))
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GO TO 9000
!
 4160 CONTINUE
      J=0
      IMAX=N1
      IF(NQ.LT.N1)IMAX=NQ
      DO 4170 I=1,IMAX
        IF(ISUB(I).EQ.0)GO TO 4170
        J=J+1
!
        IJ=MAXN*(ICOL1-1)+I
        IF(ICOL1.LE.MAXCOL)Y1(J)=DBLE(V(IJ))
        IF(ICOL1.EQ.MAXCP1)Y1(J)=DBLE(PRED(I))
        IF(ICOL1.EQ.MAXCP2)Y1(J)=DBLE(RES(I))
        IF(ICOL1.EQ.MAXCP3)Y1(J)=DBLE(YPLOT(I))
        IF(ICOL1.EQ.MAXCP4)Y1(J)=DBLE(XPLOT(I))
        IF(ICOL1.EQ.MAXCP5)Y1(J)=DBLE(X2PLOT(I))
        IF(ICOL1.EQ.MAXCP6)Y1(J)=DBLE(TAGPLO(I))
!
 4170 CONTINUE
      N1=J
!
 4190 CONTINUE
!
!               ***********************************************
!               **  STEP 4.50--                              **
!               **  CHECK FOR ARPAR VARIABLE THAT CONTAINS   **
!               **  STARTING VALUES FOR PARAMETERS           **
!               ***********************************************
!
      DO 4505 I=1,MAXPAR
        PAR(I)=0.1D0
 4505 CONTINUE
      IPVFLG='OFF'
      IHP='ARPA'
      IHP2='R   '
      IHWUSE='V'
      MESSAG='NO'
      CALL CHECKN(IHP,IHP2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'YES')GO TO 4590
      IPVFLG='ON'
      NTEMP=IN(ILOCP)
      ICOLT=IVALUE(ILOCP)
      DO 4510 I=1,MIN(NTEMP,MAXPAR)
        IJ=MAXN*(ICOLT-1)+I
        PAR(I)=DBLE(V(IJ))
 4510 CONTINUE
 4590 CONTINUE
!
!               *************************************************
!               **  STEP 4.60--                                **
!               **  CHECK FOR ARFIXED VARIABLE THAT CONTAINS   **
!               **  1 IF PARAMETER IS FIXED, 0 OTHERWISE       **
!               *************************************************
!
      IFXFLG='OFF'
      IHP='ARFI'
      IHP2='IXED'
      IHWUSE='V'
      MESSAG='NO'
      CALL CHECKN(IHP,IHP2,IHWUSE,   &
                  IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                  ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'YES')GO TO 4690
      IFXFLG='ON'
      NTEMP=IN(ILOCP)
      ICOLT=IVALUE(ILOCP)
      DO 4605 I=1,MAXOBV
        IFIXED(I)=0
 4605 CONTINUE
      DO 4610 I=1,NTEMP
        IJ=MAXN*(ICOLT-1)+I
        IFIXED(I)=0
        IF(I.LE.MAXPAR)PAR(I)=INT(V(IJ))
        IF(IFIXED(I).LE.0 .OR. IFIXED(I).GE.2)IFIXED(I)=0
 4610 CONTINUE
 4690 CONTINUE
!
!               *************************************************
!               **  STEP 4.70--                                **
!               **  CHECK FOR NFORECAS PARAMETER THAT          **
!               **  SPECIFIES NUMBER OF FORECASTS AHEAD TO MAKE**
!               *************************************************
!
      NFORE=0
      IHP='NFOR'
      IHP2='ECAS'
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IHP,IHP2,IHWUSE,   &
                  IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                  ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'YES')GO TO 4790
      NFORE=INT(VALUE(ILOCP) + 0.1)
      IF(NFORE.LT.1)NFORE=0
 4790 CONTINUE
!
      AIC=9999.0
      AICC=9999.0
!
!               *********************************
!               **  STEP 52--                  **
!               **  PERFORM THE ARIMA ANALYSIS **
!               *********************************
!
      ISTEPN='52'
      IF(ISUBRO.EQ.'ARMA' .OR. IBUGA2.EQ.'ON')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ISUBRO.EQ.'ARMA' .OR. IBUGA2.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1211)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5212)N1,N2,N1,N2,MAXN
 5212   FORMAT('N1,N2,N1,N2,MAXN = ',5I8)
        CALL DPWRST('XXX','BUG ')
        DO 5215 I=1,N1
         WRITE(ICOUT,5216)I,Y1(I)
 5216    FORMAT('I,Y1(I) = ',I8,2E15.7)
         CALL DPWRST('XXX','BUG ')
 5215   CONTINUE
        WRITE(ICOUT,5231)IBUGA3
 5231   FORMAT('IBUGA3 = ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      CALL DPARM2(Y1,N1,   &
                  IORDAR,IDIFF,IORDMA,IORSAR,ISDIFF,IORSMA,ISPER,   &
                  PAR,STP,SCALE,PV,SDPV,SDRES,DRES,VCV,MAXPAR,IFIXED,   &
                  PRED2,RES2,RESSD,RESDF,   &
                  FCST,FCSTSD,MAXOBV,   &
                  IPVFLG,IFXFLG,NFORE,   &
                  AIC,AICC,   &
                  ISUBRO,IBUGA3,IERROR)
!
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               ***************************************
!               **  STEP 15--                        **
!               **  UPDATE INTERNAL DATAPLOT TABLES  **
!               ***************************************
!
      ISTEPN='15'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'ARMA')   &
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
      IH='AIC '
      IH2='    '
      VALUE0=AIC
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA2,IERROR)
!
      IH='AICC'
      IH2='    '
      VALUE0=AICC
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
      IANS,IWIDTH,IBUGA2,IERROR)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(ISUBRO.EQ.'ARMA' .OR. IBUGA2.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPARMA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NLEFT,NS
 9014   FORMAT('NLEFT,NS = ',2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)ICASEQ,IFOUND,IERROR
 9016   FORMAT('ICASEQ,IFOUND,IERROR = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPARMA
      SUBROUTINE DPARM2(Y1,N1,   &
                        IORDAR,IDIFF,IORDMA,IORSAR,ISDIFF,IORSMA,ISPER,   &
                        PAR,STP,SCALE,PV,SDPV,SDRES,DRES2,VCV,MAXPAR,   &
                        IFIXED,PRED2,RES2,RESSD,RESDF,   &
                        FCST,FCSTSD,MAXNXT,   &
                        IPVFLG,IFXFLG,NFORE,   &
                        AIC,AICC,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS ROUTINE CARRIES OUT A 1-SAMPLE ARIMA ANALYSIS
!     EXAMPLE--ARMA Y 1 0 1
!              THIS FITS AN AR(1) AND A MA(1) WITH NO DIFFERENCING.
!              THE DATAPLOT ARIMA MODEL ALLOWS UP TO 7 TERMS:
!                 ARMA P1 D1 Q1 P2 D2 Q2 S2
!              WHERE
!                 P1 = ORDER OF AUTOREGRESSIVE TERM
!                 D1 = NUMBER OF DIFFERENCES (TYPICALLY EITHER
!                      0 FOR NO DIFFERENCE, 1 FOR A SINGLE DIFFERENCE)
!                 Q1 = ORDER OF MOVING AVERAGE TERM
!                 S1 = SEASONAL PERIOD (THIS IS TYPICALLY 1, I.E.
!                      THIS IS THE NON-SEASONAL TERM)
!                      DATAPLOT ALWAYS SETS THIS TO 1 SO NOT
!                      ENTERED BY THE USER
!                 P1 = ORDER OF SEASONAL AUTOREGRESSIVE TERM
!                 D2 = NUMBER OF DIFFERENCING FOR SEASONAL TERM
!                 Q2 = ORDER OF SEASONAL MOVING AVERAGE
!                 S2 = PERIOD FOR SEASONAL DIFFERENCING
!
!     SAMPLE DATA IS IN INPUT VECTOR Y1 (WITH N1 OBSERVATIONS).
!     DATAPLOT USES THE NIST STARPAC LIBRARY (WRITTEN BY
!     JANET DONALDSON AND PETER TYRON.  STARPAC IS BASED ON THE
!     NON-LINEAR LEAST SQUARES ROUTINES OF DENNIS AND SCHNABEL.
!
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--99/5
!     ORIGINAL VERSION--MAY       1999.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 IPVFLG
      CHARACTER*4 IFXFLG
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IOP
      CHARACTER*4 ISUBRO
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION STOPSS
      DOUBLE PRECISION STOPP
      DOUBLE PRECISION DELTA
      DOUBLE PRECISION RSD
!
      INTEGER MSPEC(4,2)
      INTEGER IFIXED(*)
      INTEGER IFCST0(1)
!
      DOUBLE PRECISION Y1(*)
      DOUBLE PRECISION PAR(*)
      DOUBLE PRECISION STP(*)
      DOUBLE PRECISION SCALE(*)
      DOUBLE PRECISION PV(*)
      DOUBLE PRECISION SDPV(*)
      DOUBLE PRECISION SDRES(*)
      DOUBLE PRECISION VCV(MAXPAR,MAXPAR)
      DOUBLE PRECISION DRES2(*)
      DOUBLE PRECISION FCST(MAXNXT,1)
      DOUBLE PRECISION FCSTSD(MAXNXT,1)
!
      DIMENSION PRED2(*)
      DIMENSION RES2(*)
!
!-----COMMON FOR STARPAC LIBRARY
!
      PARAMETER(LDSTAK=100000)
      DOUBLE PRECISION DSTAK(LDSTAK)
      COMMON/CSTAK/DSTAK
      COMMON/ERRCHK/IERR,IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5
      COMMON/STARPC/IRESDF
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPAR'
      ISUBN2='M2  '
      IERROR='NO'
!
      N=(-99)
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPARM2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,N1,NFORE
   52   FORMAT('IBUGA3,N1,NFORE = ',A4,2X,2I8)
        CALL DPWRST('XXX','WRIT')
        DO 56 I=1,N1
          WRITE(ICOUT,57)I,Y1(I)
   57     FORMAT('I,Y1(I) = ',I8,E15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
        WRITE(ICOUT,65)IORDAR,IDIFF,IORDMA
   65   FORMAT('IORDAR,IDIFF,IORDMA = ',3I8)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,66)IORSAR,ISDIFF,IORSMA,ISPER
   66   FORMAT('IORSAR,ISDIFF,IORSMA,ISPER = ',4I8)
        CALL DPWRST('XXX','WRIT')
      ENDIF
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
 1111 FORMAT('***** ERROR IN DPARM2--THE NUMBER OF OBSERVATIONS ',   &
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
 1121 FORMAT('***** NOTE FROM DPARM2--VARIABLE 1 ',   &
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
 1131 FORMAT('***** NOTE FROM DPARM2--VARIABLE 1 ',   &
      'HAS ALL ELEMENTS = ',E15.7)
      CALL DPWRST('XXX','WRIT')
      GO TO 9000
 1139 CONTINUE
!
!               **************************************************
!               **  STEP 2.1--                                  **
!               **   OPEN THE STORAGE FILES                     **
!               **************************************************
!
      ISTEPN='2.1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ARM2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IOP='OPEN'
      IFLAG1=1
      IFLAG2=1
      IFLAG3=1
      IFLAG4=1
      IFLAG5=1
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                  IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               *************************************
!               **  STEP 31--                      **
!               **  CARRY OUT CALCULATIONS         **
!               **  FOR A 1-SAMPLE ARIMA ANALYSIS  **
!               *************************************
!
      ISTEPN='31'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IFXFLG.EQ.'OFF')THEN
        IFIXED(1)=-1
      ENDIF
      IVARPX=1
      MIT=500
      NPRT=22202
      STOPP=-1.0
      STOPSS=-1.0
      DELTA=-1.0
      SCALE(1)=-1.0
      STP(1)=-1.0
      IVCV=MAXPAR
      NPARE=0
!
      NFAC=1
      IF(IORSAR.GT.0 .OR. IORSMA.GT.0 .OR. ISDIFF.GT.0)NFAC=2
      MSPEC(1,1)=IORDAR
      MSPEC(2,1)=IDIFF
      MSPEC(3,1)=IORDMA
      MSPEC(4,1)=1
      MSPEC(1,2)=IORSAR
      MSPEC(2,2)=ISDIFF
      MSPEC(3,2)=IORSMA
      MSPEC(4,2)=ISPER
      IF(MSPEC(1,2).EQ.0 .AND. MSPEC(2,2).EQ.0 .AND.   &
        MSPEC(3,2).EQ.0)MSPEC(4,2)=0
      NPAR=1 + MSPEC(1,1) + MSPEC(3,1) + MSPEC(1,2) + MSPEC(3,2)
      IF(IPVFLG.EQ.'OFF')THEN
        DO 3200 I=1,NPAR
          PAR(I)=0.1D0
 3200   CONTINUE
        PAR(MSPEC(1,1)+MSPEC(1,2)+1)=0.0D0
      ENDIF
!
      CALL AIMES(Y1,N1,MSPEC,NFAC,PAR,NPAR,DRES2,LDSTAK,   &
                 IFIXED,STP,MIT,STOPSS,STOPP,   &
                 SCALE,DELTA,IVARPX,NPRT,   &
                 NPARE,RSD,PV,SDPV,SDRES,VCV,IVCV)
!
      IF(IERR.NE.0)IERROR='YES'
      RESSD=REAL(RSD)
      RESDF=REAL(IRESDF)
      DO 3810 I=1,N1
        PRED2(I)=REAL(PV(I))
        RES2(I)=REAL(DRES2(I))
 3810 CONTINUE
!
!CCCC FEBRUARY 2003: COMPUTE AIC: AIC(NPAR) = N*LOG(RESSD**2)+2*NPAR
!
      AN=REAL(N1)
      AIC=AN*LOG(RESSD**2)+2.0*REAL(NPAR)
      AP=REAL(IORDAR)
      AQ=REAL(IORDMA)
      AFACT=2.0*(AP + AQ + 1.0)*AN/(AN - AP - AQ - 2.0)
      AICC=REAL(N1)*LOG(RESSD**2)+AFACT
!
      NPRT=0
      NFCST=0
      NFCST0=0
!CCCC IF(NFORE.GT.0)NFCST=NFORE
      IFCST0(1)=0
      IFCST=MAXOBV
      IERR=0
!
      CALL AIMFS(Y1,N1,MSPEC,NFAC,PAR,NPAR,LDSTAK,   &
                 NFCST,NFCST0,IFCST0,NPRT,FCST,IFCST,FCSTSD)
      IF(IERR.NE.0)IERROR='YES'
!
!  THIS DONE IN STARPAC CODE
!
!CCCC DO3820I=1,NPAR
!CCCC   WRITE(IOUNI1,3821)PAR(I)
!3821   FORMAT(E15.7,1X,E15.7)
!3820 CONTINUE
!
!  THIS DONE IN STARPAC CODE
!
!CCCC DO3830I=1,N1
!CCCC   WRITE(IOUNI2,3831)PV(I),SDPV(I),REAL(DRES2(I)),SDRES(I)
!3831   FORMAT(4(E15.7,1X))
!3830 CONTINUE
!
!CCCC NTEMP=(N1/10)+1
!CCCC DO3840I=1,NTEMP
!CCCC   WRITE(IOUNI5,3841)FCST(I,1),FCSTSD(I,1)
!3841   FORMAT(2(E15.7,1X))
!3840 CONTINUE
!
!
      IF(IPRINT.EQ.'OFF')GO TO 8189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,8112)
 8112 FORMAT(6X,'PARAMETERS,  SD(PARAMETERS), 1/SD(PAR), LOWER AND ',   &
      'UPPER')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,8113)
 8113 FORMAT(6X,'95% CONFIDENCE INTERVAL WRITTEN OUT TO FILE ',   &
      'DPST1F.DAT')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,8114)
 8114 FORMAT(6X,'ORDER IS:')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,8115)
 8115 FORMAT(6X,'   1. AUTO_REGRESSIVE TERMS')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,8116)
 8116 FORMAT(6X,'   2. SEASONAL AUTO_REGRESSIVE TERMS')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,8117)
 8117 FORMAT(6X,'   3. MU (MEAN TERM)')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,8118)
 8118 FORMAT(6X,'   4. MOVING AVERAGE TERMS')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,8119)
 8119 FORMAT(6X,'   5. SEASONAL MOVING AVERAGE TERMS')
      CALL DPWRST('XXX','WRIT')
!
      WRITE(ICOUT,8122)
 8122 FORMAT(6X,'FOLLOWING WRITTEN OUT TO FILE DPST2F.DAT')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,8123)
 8123 FORMAT(6X,'   1. ROW NUMBER')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,8124)
 8124 FORMAT(6X,'   2. PREDICTED VALUES')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,8125)
 8125 FORMAT(6X,'   3. STANDARD DEVIATION OF PREDICTED VALUES')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,8126)
 8126 FORMAT(6X,'   4. RESIDUALS')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,8127)
 8127 FORMAT(6X,'   5. STANDARDIZED RESIDUALS')
      CALL DPWRST('XXX','WRIT')
!
      WRITE(ICOUT,8132)
 8132 FORMAT(6X,'RESULTS OF ITERATIONS WRITTEN OUT TO FILE DPST3F.DAT')
      CALL DPWRST('XXX','WRIT')
!
      WRITE(ICOUT,8142)
 8142 FORMAT(6X,'PARAMETER VARIANCE-COVARIANCE MATRIX  WRITTEN OUT ',   &
      'TO FILE DPST4F.DAT')
      CALL DPWRST('XXX','WRIT')
!
      WRITE(ICOUT,8152)
 8152 FORMAT(6X,'FORECAST, STANDARD DEVIATION OF FORECASTS, AND')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,8153)
 8153 FORMAT(6X,'95% CONFIDENCE INTERVAL FOR FORECAST ',   &
      'WRITTEN TO FILE DPST5F.DAT')
      CALL DPWRST('XXX','WRIT')
!
 8189 CONTINUE
!
      GO TO 8200
!
!               **************************************
!               **  STEP 92--                       **
!               **  CLOSE       THE STORAGE FILES.  **
!               **************************************
!
 8200 CONTINUE
!
      ISTEPN='82'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ARM2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IOP='CLOS'
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                  IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPARM2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9015)IERROR,N,N1,IORDAR,IDIFF,IORDMA
 9015   FORMAT('IERROR,N,N1,IORDAR,IDIFF,IORDMA = ',A4,2X,2I8,3I5)
        CALL DPWRST('XXX','WRIT')
        DO 9016 I=1,N1
          WRITE(ICOUT,9017)I,Y1(I)
 9017     FORMAT('I,Y1(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
 9016   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPARM2
      SUBROUTINE DPARPA(IHARG,IHARG2,IARGT,IARG,NUMARG,IDEFPA,   &
                        MAXARR,IARRPA,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE PATTERN FOR AN ARROW.
!              THE PATTERN FOR ARROW I WILL BE PLACED
!              IN THE I-TH ELEMENT OF THE HOLLERITH
!              VECTOR IARRPA(.).
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --IARGT  (A HOLLERITH VECTOR)
!                     --IARG   (A HOLLERITH VECTOR)
!                     --NUMARG
!                     --IDEFPA
!                     --MAXARR
!     OUTPUT ARGUMENTS--IARRPA (A HOLLERITH VECTOR
!                              WHOSE I-TH ELEMENT CONTAINS THE
!                              PATTERN FOR ARROW I.
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
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--SEPTEMBER 1980.
!     UPDATED         --MAY       1982.
!     UPDATED         --AUGUST    1995.  DASH2 BUG
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
!CCCC AUGUST 1995.  ADD FOLLOWING LINE
      CHARACTER*4 IHARG2
      CHARACTER*4 IARGT
      CHARACTER*4 IDEFPA
      CHARACTER*4 IARRPA
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHOLD
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
!CCCC AUGUST 1995.  ADD FOLLOWING LINE
      DIMENSION IHARG2(*)
      DIMENSION IARGT(*)
      DIMENSION IARG(*)
!
      DIMENSION IARRPA(*)
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
      IF(NUMARG.EQ.0)GO TO 9000
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PATT')GO TO 1110
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'PATT')GO TO 1140
      GO TO 9000
!
 1110 CONTINUE
      IF(NUMARG.LE.1)GO TO 1120
      IF(IHARG(2).EQ.'ON')GO TO 1120
      IF(IHARG(2).EQ.'OFF')GO TO 1120
      IF(IHARG(2).EQ.'AUTO')GO TO 1120
      IF(IHARG(2).EQ.'DEFA')GO TO 1120
      GO TO 1125
!
 1120 CONTINUE
      IHOLD=IDEFPA
      GO TO 1130
!
 1125 CONTINUE
      IHOLD=IHARG(2)
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(2).EQ.'2')IHOLD='DA2'
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(2).EQ.'3')IHOLD='DA3'
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(2).EQ.'4')IHOLD='DA4'
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(2).EQ.'5')IHOLD='DA5'
      GO TO 1130
!
 1130 CONTINUE
      IFOUND='YES'
      DO 1135 I=1,MAXARR
      IARRPA(I)=IHOLD
 1135 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1139
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1136)IARRPA(I)
 1136 FORMAT('ALL ARROW PATTERNS HAVE JUST BEEN SET TO ',   &
      A4)
      CALL DPWRST('XXX','BUG ')
 1139 CONTINUE
      GO TO 9000
!
 1140 CONTINUE
      IF(IARGT(1).EQ.'NUMB')GO TO 1150
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1141)
 1141 FORMAT('***** ERROR IN DPARPA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1142)
 1142 FORMAT('      IN THE ARROW ... PATTERN COMMAND,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1143)
 1143 FORMAT('      THE ARROW IS IDENTIFIED BY A NUMBER, AS IN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1144)
 1144 FORMAT('      ARROW 3 PATTERN SOLID')
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
!
 1150 CONTINUE
      I=IARG(1)
      IF(1.LE.I.AND.I.LE.MAXARR)GO TO 1160
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1151)
 1151 FORMAT('***** ERROR IN DPARPA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1152)
 1152 FORMAT('      IN THE ARROW ... PATTERN COMMAND,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1153)
 1153 FORMAT('      THE NUMBER OF ARROWS MUST BE ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1154)MAXARR
 1154 FORMAT('      BETWEEN 1 AND ',I8,' (INCLUSIVELY);')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1155)
 1155 FORMAT('      SUCH WAS NOT THE CASE HERE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1156)I
 1156 FORMAT('      A REFERENCE WAS MADE TO THE ',I8,'-TH ',   &
      'ARROW.')
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
!
 1160 CONTINUE
      IF(NUMARG.LE.2)GO TO 1170
      IF(IHARG(3).EQ.'ON')GO TO 1170
      IF(IHARG(3).EQ.'OFF')GO TO 1170
      IF(IHARG(3).EQ.'AUTO')GO TO 1170
      IF(IHARG(3).EQ.'DEFA')GO TO 1170
      GO TO 1175
!
 1170 CONTINUE
      IHOLD=IDEFPA
      GO TO 1180
!
 1175 CONTINUE
      IHOLD=IHARG(3)
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(3).EQ.'2')IHOLD='DA2'
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(3).EQ.'3')IHOLD='DA3'
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(3).EQ.'4')IHOLD='DA4'
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(3).EQ.'5')IHOLD='DA5'
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
      IARRPA(I)=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1186)I,IARRPA(I)
 1186 FORMAT('THE PATTERN FOR ARROW ',I8,   &
      ' HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 9000
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE DPARPA
      SUBROUTINE DPARRO(IHARG,IARGT,ARG,NUMARG,   &
                        PXSTAR,PYSTAR,PXEND,PYEND,   &
                        IARRPA,IARRCO,IARRC2,MAXARZ,PARRTH,   &
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
!  AUGUST, 1987: USE PATTERN, THICKNESS, AND COLOR SETTINGS FROM
!                ARROW COMMON BLOCK RATHER THAN LINE COMMON BLOCK.  DID
!                A GLOBAL CHANGE FROM ILINPA, ILINCO, PLINTH  TO IARRPA,
!                IARRCO, PARRTH
!
!     PURPOSE--DRAW ONE OR MORE ARROWS (DEPENDING ON HOW MANY NUMBERS
!              ARE PROVIDED).  THE COORDINATES ARE IN STANDARDIZED UNITS
!              OF 0 TO 100.
!     NOTE--THE INPUT COORDINATES DEFINE THE ENDS OF THE LINE SEGMENTS.
!     NOTE-THE USUAL INPUT NUMBER OF COORDINATES IS 2
!          AND THEREFORE THE USUAL INPUT NUMBER OF NUMBERS IS 2*2 = 4.
!     NOTE--IF 2 NUMBERS ARE PROVIDED, THEN THE DRAWN LINE WILL GO FROM
!           THE LAST CURSOR POSITION TO THE (X,Y) POINT (EITHER ABSOLUTE
!           OR RELATIVE) AS DEFINED BY THE 2 NUMBERS.
!     NOTE--IF 4 NUMBERS ARE PROVIDED, THEN THE DRAWN ARROW WILL GO FROM
!           THE ABSOLUTE (X,Y) POSITION AS DEFINED BY THE FIRST 2
!           NUMBERS TO THE (X,Y) POINT (EITHER ABSOLUTE OR RELATIVE)
!           AS DEFINED BY THE THIRD AND FOURTH NUMBERS.
!     NOTE--IF 6 NUMBERS ARE PROVIDED, THEN THE DRAWN ARROW WILL GO FROM
!           THE (X,Y) POSITION AS RESULTING FROM THE THIRD AND FOURTH
!           NUMBERS TO THE (X,Y) POINT (EITHER ABSOLUTE OR RELATIVE)
!           AS DEFINED BY THE FIFTH AND SIXTH NUMBERS.
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
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--APRIL     1981.
!     UPDATED         --MARCH     1982.
!     UPDATED         --MAY       1982.
!     UPDATED         --NOVEMBER  1982.
!     UPDATED         --JANUARY   1989. CALL LIST FOR OFFSET VAR (ALAN)
!     UPDATED         --JANUARY   1989. USE COMMON PARAMETERS (ALAN)
!     UPDATED         --MARCH     1997. SUPPORT FOR DEVICE FONT (ALAN)
!     UPDATED         --JULY      1997. SUPPORT FOR "DATA" UNITS (ALAN)
!     UPDATED         --DECEMBER  2018. SUPPORT FOR "DEVICE ... SCALE"
!                                       COMMAND
!     UPDATED         --JUNE      2019. MOVE CREATION OF SCRATCH
!                                       STORAGE FROM DPARR2 TO DPARRO
!     UPDATED         --OCTOBER   2020. SUPPORT FOR RGB COLOR
!
!-----NON-COMMON VARIABLES-----------------------------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IARGT
!
      CHARACTER*4 IARRPA
      CHARACTER*4 IARRCO
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
      DIMENSION IARRPA(*)
      DIMENSION IARRCO(*)
      DIMENSION IARRC2(MAXARZ,3)
      DIMENSION PARRTH(*)
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
      DIMENSION PX(1000)
      DIMENSION PY(1000)
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(IGARB1),PX(1))
      EQUIVALENCE (GARBAG(IGARB2),PY(1))
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
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
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'ARRO')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPARRO--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IARRPA(1),IARRCO(1),PARRTH(1)
   52   FORMAT('IARRPA(1),IARRCO(1),PARRTH(1) = ',2(A4,2X),G15.7)
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
      IFIG='ARRO'
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
          IF(IARGT(I).EQ.'NUMB')GO TO 1120
          GO TO 1130
 1120   CONTINUE
        IFOUND='YES'
!
!               ****************************
!               **  STEP 3--              **
!               **  DRAW OUT THE FIGURE   **
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
        CALL DPARR2(X1,Y1,X2,Y2,PX,PY,   &
                    IFIG,IARRPA,IARRCO,IARRC2,MAXARZ,PARRTH,   &
                    AREGBA,IREBLI,IREBCO,IREBC2,MAXRGZ,PREBTH,   &
                    IREFSW,IREFCO,IREFC2,   &
                    IREPTY,IREPLI,IREPCO,IREPC2,PREPTH,PREPSP,   &
                    PTEXHE,PTEXWI,PTEXVG,PTEXHG)
!
        X1=X2
        Y1=Y2
        GO TO 1160
!
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
 1131 FORMAT('***** ERROR IN ARROW (DPARRO)--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1132)
 1132 FORMAT('      ILLEGAL FORM FOR THE ARROW COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1134)
 1134 FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE PROPER FORM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1135)
 1135 FORMAT('      SUPPOSE IT IS DESIRED TO DRAW AN ARROW')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1136)
 1136 FORMAT('      WITH ONE END AT THE POINT 20 20 ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1137)
 1137 FORMAT('      AND WITH OPPOSITE END AT THE POINT 40 60')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1141)
 1141 FORMAT('      THEN THE ALLOWABLE FORMS ARE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1142)
 1142 FORMAT('      ARROW 20 20 40 60 ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1143)
 1143 FORMAT('      ARROW ABSOLUTE 20 20 40 60 ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1145)
 1145 FORMAT('      ARROW RELATIVE 20 20 40 60 ')
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'ARRO')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPARRO--')
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
      END SUBROUTINE DPARRO
      SUBROUTINE DPARR2(X1,Y1,X2,Y2,PX,PY,   &
                        IFIG,IARRPA,IARRCO,IARRC2,MAXAR,PARRTH,   &
                        AREGBA,IREBLI,IREBCO,IREBC2,MAXRG,PREBTH,   &
                        IREFSW,IREFCO,IREFC2,   &
                        IREPTY,IREPLI,IREPCO,IREPC2,PREPTH,PREPSP,   &
                        PTEXHE,PTEXWI,PTEXVG,PTEXHG)
!
!  AUGUST, 1987: GLOBAL CHANGE OF ILINPA, ILINCO, PLINTH TO
!  IARRPA, IARRCO, PARRTH
!
!     PURPOSE--DRAW AN ARROW WITH THE BACK OF THE ARROW AT (X1,Y1)
!              AND THE TIP AT (X2,Y2).
!     NOTE--THE ARROW HEAD WILL HAVE A STEM LENGTH OF PTEXWI
!           AND WILL HAVE A BASE WIDTH OF PTEXHE.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--APRIL     1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --JANUARY   1989. MODIFY CALLS TO DPDRPL (ALAN)
!     UPDATED         --JANUARY   1989. MODIFY CALL  TO DPFIRE (ALAN)
!     UPDATED         --JANUARY   1989. USE COMMON PARAMETERS (ALAN)
!     UPDATED         --JUNE      2019. MOVE CREATION OF SCRATCH
!                                       STORAGE FROM DPARR2 TO DPARRO
!     UPDATED         --OCTOBER   2020. SUPPORT FOR RGB COLOR
!
!-----NON-COMMON VARIABLES-------------------------------------
!
      DIMENSION PX(*)
      DIMENSION PY(*)
!
      CHARACTER*4 IFIG
      CHARACTER*4 IPATT2
!
      CHARACTER*4 IARRPA
      CHARACTER*4 IARRCO
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
      CHARACTER*4 ICOLF
      CHARACTER*4 ICOLP
      CHARACTER*4 ICOL
      CHARACTER*4 IFLAG
!
      DIMENSION IARRPA(*)
      DIMENSION IARRCO(*)
      DIMENSION IARRC2(MAXAR,3)
      DIMENSION PARRTH(*)
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
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'ARR2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPARR2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IARRPA(1),IARRCO(1),PARRTH(1)
   52   FORMAT('IARRPA(1),IARRCO(1),PARRTH(1) = ',2(A4,2X),G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)X1,Y1,X2,Y2
   53   FORMAT('X1,Y1,X2,Y2 = ',4G15.7)
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
!
!               *********************************
!               **  STEP 1--                   **
!               **  DETERMINE THE COORDINATES  **
!               **  FOR THE ARROW              **
!               *********************************
!
      DELX=X2-X1
      DELY=Y2-Y1
      LEN=INT(SQRT((X2-X1)**2+(Y2-Y1)**2) + 0.1)
      ALEN=REAL(LEN)
      IF(ABS(DELX).GE.0.00001)THEN
        THETA=ATAN(DELY/DELX)
      ELSEIF(ABS(DELX).LT.0.00001.AND.DELY.GE.0.0)THEN
        THETA=3.1415926/2.0
      ELSEIF(ABS(DELX).LT.0.00001.AND.DELY.LT.0.0)THEN
        THETA=-3.1415926/2.0
      ENDIF
!
      XDEL=PTEXWI
      YDEL=PTEXHE
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
      X=ALEN
      Y=0
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
!
      X=ALEN-XDEL
      Y=-YDEL
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
!
      X=ALEN-XDEL
      Y=YDEL
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
!
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
      IF(IREFSW(1).EQ.'OFF')GO TO 2190
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
      CALL DPFIRE(PX,PY,NP,   &
                  IFIG,IPATT,PTHICK,PXGAP,PYGAP,   &
                  ICOLF,ICOLFR,ICOLFG,ICOLFB,   &
                  ICOLP,ICOLPR,ICOLPG,ICOLPB,   &
                  IPATT2)
 2190 CONTINUE
!
!               ***************************
!               **  STEP 3--             **
!               **  DRAW OUT THE FIGURE  **
!               ***************************
!
      IPATT=IARRPA(1)
      PTHICK=PARRTH(1)
      ICOL=IARRCO(1)
      ICOLR=IARRC2(1,1)
      ICOLG=IARRC2(1,2)
      ICOLB=IARRC2(1,3)
      IFLAG='ON'
!
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
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'ARR2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPARR2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9039)IERRG4
 9039   FORMAT('IERRG4 = ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPARR2
      SUBROUTINE DPARR3(X1,Y1,X2,Y2,   &
                        IFIG,ITRCSW,   &
                        IARRPA,IARRCO,IARRCR,IARRCG,IARRCB,PARRTH,   &
                        IREFSW,IREFCO,IREFCR,IREFCG,IREFCB,   &
                        IREPCO,IREPCR,IREPCG,IREPCB,PREPTH,PREPSP,   &
                        PTEXHE,PTEXWI,PTEXVG,PTEXHG)
!
!  THIS IS A SLIGHTLY MODIFIED VERSION OF DPARR2.  THIS VERSION IS
!  CALLED FOR THE ARROW ... COORDINATES CASE AND THE CHARACTER ARROW
!  CASE.  MAKE A SEPARATE ROUTINE FOR EASIER SEGMENTATION.  ALSO
!  DELETE UNUSED PARAMETERS.
!
!  AUGUST, 1987: GLOBAL CHANGE OF ILINPA, ILINCO, PLINTH TO
!  IARRPA, IARRCO, PARRTH
!
!     PURPOSE--DRAW AN ARROW
!              WITH THE BACK OF THE ARROW AT (X1,Y1)
!              AND THE TIP AT (X2,Y2).
!     NOTE--THE ARROW HEAD WILL HAVE A STEM LENGTH OF PTEXWI
!           AND WILL HAVE A BASE WIDTH OF PTEXHE.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--APRIL     1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --DECEMBER  2009. MAKE CERTAIN ARGUMENTS
!                                       SCALAR (FOR COMPATIBILITY
!                                       WITH VERSION 11 OF INTEL
!                                       COMPILER)
!
!-----NON-COMMON VARIABLES-------------------------------------
!
      CHARACTER*4 IFIG
      CHARACTER*4 IPATT2
      CHARACTER*4 ITRCSW
!
      CHARACTER*4 IARRPA
      CHARACTER*4 IARRCO
!
      CHARACTER*4 IREFSW
      CHARACTER*4 IREFCO
      CHARACTER*4 IREPCO
!
      CHARACTER*4 IPATT
      CHARACTER*4 ICOLF
      CHARACTER*4 ICOLP
      CHARACTER*4 ICOL
      CHARACTER*4 IFLAG
!
      DIMENSION PX(10)
      DIMENSION PY(10)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOSU.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'ARR3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPARR3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IFIG,IARRPA,IARRCO,IARRCR,IARRCG,IARRCB,PARRTH
   52   FORMAT('IFIG,IARRPA,IARRCO,IARRCR,IARRCG,IARRCB,PARRTH = ',   &
               3(A4,2X),3I5,G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)X1,Y1,X2,Y2
   53   FORMAT('X1,Y1,X2,Y2 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,64)IREFSW,IREFCO,IREFCR,IREFCG,IREFCB
   64   FORMAT('IREFSW,IREFCO,IREFCR,IREFCG,IREFCG,IREFCB = ',   &
               2(A4,2X),3I5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,65)IREPCO,IREPCR,IREPCG,IREPCB,PREPTH,PREPSP
   65   FORMAT('IREPCO,IREPCR,IREPCG,IREPCB,PREPTH,PREPSP = ',   &
               A4,3I5,2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,69)PTEXHE,PTEXWI,PTEXVG,PTEXHG
   69   FORMAT('PTEXHE,PTEXWI,PTEXVG,PTEXHG = ',4E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,79)IBUGG4,ISUBG4,IERRG4
   79   FORMAT('IBUGG4,ISUBG4,IERRG4 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *********************************
!               **  STEP 1--                   **
!               **  DETERMINE THE COORDINATES  **
!               **  FOR THE ARROW              **
!               *********************************
!
      DELX=X2-X1
      DELY=Y2-Y1
      LEN=INT(SQRT((X2-X1)**2+(Y2-Y1)**2) + 0.1)
      ALEN=LEN
      IF(ABS(DELX).GE.0.00001)THETA=ATAN(DELY/DELX)
      IF(ABS(DELX).LT.0.00001.AND.DELY.GE.0.0)THETA=3.1415926/2.0
      IF(ABS(DELX).LT.0.00001.AND.DELY.LT.0.0)THETA=-3.1415926/2.0
!
      XDEL=PTEXWI
      YDEL=PTEXHE
!
      K=0
!
      X=0.
      Y=0.
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
!
!  NOTE: IN THIS CASE, WANT ARROW HEAD TO BE EXACTLY AT (PX2,PY2).
!  DRAWING AT ANGLE THROWS THIS OFF SOMEWHAT.  ADJUST ALL THE ARROW
!  HEAD POINTS SO THAT THE ARROW HEAD IS PLOTTED EXACTLY AT THE
!  POINT (LEAVE START POINT ALONE).
      X=ALEN
      Y=0.
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
!
      PXINC=PX(K)-X2
      PYINC=PY(K)-Y2
!
      X=ALEN-XDEL
      Y=-YDEL
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
!
      X=ALEN-XDEL
      Y=YDEL
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
!
      X=ALEN
      Y=0.
      CALL TRANS(X,Y,X1,Y1,THETA,DELX,DELY,XP,YP,KXP,KYP)
      K=K+1
      PX(K)=XP
      PY(K)=YP
!
      NP=K
!
      DO 200 I=2,NP
      PX(I)=PX(I)-PXINC
      PY(I)=PY(I)-PYINC
 200  CONTINUE
!
!               ***********************
!               **  STEP 2--         **
!               **  FILL THE FIGURE  **
!               **  (IF CALLED FOR)  **
!               ***********************
!
!CCCC IF(IREFSW(1).EQ.'OFF')GO TO 2190
      IF(IREFSW.EQ.'OFF')GO TO 2190
      IPATT='SOLI'
      IPATT2='SOLI'
      PTHICK=PREPTH
      PXGAP=PREPSP
      PYGAP=PREPSP
      ICOLF=IREFCO
      ICOLP=IREPCO
      NP=4
      CALL DPFIRE(PX(2),PY(2),NP,   &
                  IFIG,IPATT,PTHICK,PXGAP,PYGAP,   &
                  ICOLF,ICOLFR,ICOLFG,ICOLFB,   &
                  ICOLP,ICOLPR,ICOLPG,ICOLPB,   &
                  IPATT2)
 2190 CONTINUE
!
!               ***************************
!               **  STEP 3--             **
!               **  DRAW OUT THE FIGURE  **
!               ***************************
!
!  2 FACTORS CONTROL APPEARANCE OF VECTOR:
!
!  ITRCSW CONTROLS WHETHER JUST THE ARROW HEAD OR THE ARROW HEAD
!         AND THE VECTOR ARE DRAWN
!  IVCOPN CONTROLS WHETHER THE BASE OF THE ARROW HEAD IS DRAWN OR NOT
!
      IPATT=IARRPA
      PTHICK=PARRTH
      ICOL=IARRCO
      ICOLR=IARRCR
      ICOLG=IARRCG
      ICOLB=IARRCB
      IFLAG='ON'
!
!  DRAW AS CLOSED ARROW (I.E., DRAW THE BASE OF THE TRIANGLE)
!
      IF(IVCOPN.EQ.'OPEN')GO TO 2000
      NP=5
      INDX=1
      IF(ITRCSW.EQ.'OFF')THEN
        NP=4
        INDX=2
      ENDIF
      CALL DPDRPL(PX(INDX),PY(INDX),NP,   &
                  IFIG,IPATT,PTHICK,   &
                  ICOL,ICOLR,ICOLG,ICOLB,   &
                  JPATT,JTHICK,PTHIC2,JCOL,IFLAG)
      GO TO 9000
!
!  DRAW AS OPEN ARROW (I.E., LEAVE OFF THE BASE OF THE TRIANGLE)
!
 2000 CONTINUE
      NP=3
      INDX=1
      IF(ITRCSW.EQ.'OFF')THEN
        NP=2
        INDX=2
      ENDIF
      CALL DPDRPL(PX(INDX),PY(INDX),NP,   &
                  IFIG,IPATT,PTHICK,   &
                  ICOL,ICOLR,ICOLG,ICOLB,   &
                  JPATT,JTHICK,PTHIC2,JCOL,IFLAG)
      NP=2
      INDX=4
      CALL DPDRPL(PX(INDX),PY(INDX),NP,   &
                  IFIG,IPATT,PTHICK,   &
                  ICOL,ICOLR,ICOLG,ICOLB,   &
                  JPATT,JTHICK,PTHIC2,JCOL,IFLAG)
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'ARR3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPARR3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9039)IERRG4
 9039   FORMAT('IERRG4 = ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPARR3
      SUBROUTINE DPARTH(IHARG,IARGT,IARG,ARG,NUMARG,PDEFTH,   &
      MAXARR,PARRTH,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE THICKNESS FOR AN ARROW.
!              THE THICKNESS FOR ARROW I WILL BE PLACED
!              IN THE I-TH ELEMENT OF THE REAL
!              VECTOR PARRTH(.).
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --IARGT  (A HOLLERITH VECTOR)
!                     --IARG   (A HOLLERITH VECTOR)
!                     --ARG    (A REAL VECTOR)
!                     --NUMARG
!                     --PDEFTH
!                     --MAXARR
!     OUTPUT ARGUMENTS--PARRTH (A REAL VECTOR
!                              WHOSE I-TH ELEMENT CONTAINS THE
!                              THICKNESS FOR ARROW I.
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
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--SEPTEMBER 1980.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IARGT
      REAL        PDEFTH
      REAL        PARRTH
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      REAL        PHOLD
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
      DIMENSION IARGT(*)
      DIMENSION IARG(*)
      DIMENSION ARG(*)
!
      DIMENSION PARRTH(*)
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
      IF(NUMARG.EQ.0)GO TO 9000
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'THIC')GO TO 1110
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'THIC')GO TO 1140
      GO TO 9000
!
 1110 CONTINUE
      IF(NUMARG.LE.1)GO TO 1120
      IF(IHARG(2).EQ.'ON')GO TO 1120
      IF(IHARG(2).EQ.'OFF')GO TO 1120
      IF(IHARG(2).EQ.'AUTO')GO TO 1120
      IF(IHARG(2).EQ.'DEFA')GO TO 1120
      GO TO 1125
!
 1120 CONTINUE
      PHOLD=PDEFTH
      GO TO 1130
!
 1125 CONTINUE
      PHOLD=ARG(2)
      GO TO 1130
!
 1130 CONTINUE
      IFOUND='YES'
      DO 1135 I=1,MAXARR
      PARRTH(I)=PHOLD
 1135 CONTINUE
!
      IF(IFEEDB.EQ.'OFF')GO TO 1139
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1136)PARRTH(I)
 1136 FORMAT('ALL ARROW THICKNESSS HAVE JUST BEEN SET TO ',   &
      E15.7)
      CALL DPWRST('XXX','BUG ')
 1139 CONTINUE
      GO TO 9000
!
 1140 CONTINUE
      IF(IARGT(1).EQ.'NUMB')GO TO 1150
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1141)
 1141 FORMAT('***** ERROR IN DPARTH--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1142)
 1142 FORMAT('      IN THE ARROW ... THICKNESS COMMAND,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1143)
 1143 FORMAT('      THE ARROW IS IDENTIFIED BY A NUMBER, AS IN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1144)
 1144 FORMAT('      ARROW 3 THICKNESS 0.3')
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
!
 1150 CONTINUE
      I=IARG(1)
      IF(1.LE.I.AND.I.LE.MAXARR)GO TO 1160
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1151)
 1151 FORMAT('***** ERROR IN DPARTH--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1152)
 1152 FORMAT('      IN THE ARROW ... THICKNESS COMMAND,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1153)
 1153 FORMAT('      THE NUMBER OF ARROWS MUST BE ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1154)MAXARR
 1154 FORMAT('      BETWEEN 1 AND ',I8,' (INCLUSIVELY);')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1155)
 1155 FORMAT('      SUCH WAS NOT THE CASE HERE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1156)I
 1156 FORMAT('      A REFERENCE WAS MADE TO THE ',I8,'-TH ',   &
      'ARROW.')
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
!
 1160 CONTINUE
      IF(NUMARG.LE.2)GO TO 1170
      IF(IHARG(3).EQ.'ON')GO TO 1170
      IF(IHARG(3).EQ.'OFF')GO TO 1170
      IF(IHARG(3).EQ.'AUTO')GO TO 1170
      IF(IHARG(3).EQ.'DEFA')GO TO 1170
      GO TO 1175
!
 1170 CONTINUE
      PHOLD=PDEFTH
      GO TO 1180
!
 1175 CONTINUE
      PHOLD=ARG(3)
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
      PARRTH(I)=PHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1186)I,PARRTH(I)
 1186 FORMAT('THE THICKNESS FOR ARROW ',I8,   &
      ' HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 9000
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE DPARTH
      SUBROUTINE DPASSO(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                        IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--GENERATE AN ASSOCIATION PLOT--
!              THIS PLOT IS USED TO ANALYZE ASSOCIATION IN
!              TWO-WAY TABLES.
!                  ASSOCIATION PLOT N11 N12 N21 N22
!                  ASSOCIATION PLOT Y1 Y2
!                  ASSOCIATION PLOT TABLE
!     EXAMPLES--ASSOCIATION PLOT Y1 Y2
!             --ASSOCIATION PLOT TABLE
!             --ASSOCIATION PLOT N11 N12 N21 N22
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2007/6
!     ORIGINAL VERSION--JUNE      2007.
!     UPDATED         --FEBRUARY  2011. USE DPPARS, DPPAR3, DPPAR6
!     UPDATED         --JUNE      2019. TWEAK SCRATCH STORAGE
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
      CHARACTER*4 ICASE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      CHARACTER*40 INAME
!
      PARAMETER (MAXSPN=20)
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
      REAL Y1(MAXOBV)
      REAL Y2(MAXOBV)
      REAL TEMP1(MAXOBV)
      REAL TEMP2(MAXOBV)
      REAL XIDTEM(MAXOBV)
      REAL XIDTE2(MAXOBV)
!
      PARAMETER(MAXLEV=300)
      REAL XMAT(MAXLEV,MAXLEV)
      REAL EXPFRE(MAXLEV,MAXLEV)
      REAL RESFRE(MAXLEV,MAXLEV)
!
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),Y2(1))
      EQUIVALENCE (GARBAG(IGARB3),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB4),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB5),XIDTEM(1))
      EQUIVALENCE (GARBAG(IGARB6),XIDTE2(1))
      EQUIVALENCE (GARBAG(IGARB7),XMAT(1,1))
      EQUIVALENCE (GARBAG(IGARB8),EXPFRE(1,1))
      EQUIVALENCE (GARBAG(IGARB9),RESFRE(1,1))
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
      IFOUND='NO'
      ISUBN1='DPAS'
      ISUBN2='SO  '
      ICASPL='ASSO'
      ICASE='PARA'
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
      N11=(-999)
      N21=(-999)
      N12=(-999)
      N22=(-999)
      AN11=0.0
      AN21=0.0
      AN12=0.0
      AN22=0.0
      NS1=(-999)
      NS2=(-999)
      NS3=(-999)
      NS4=(-999)
      MINN2=2
!
!               ****************************************
!               **  TREAT THE ASSOCIATION PLOT CASE   **
!               ****************************************
!
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ASSO')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPASSO--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGG2,IBUGG3,IBUGQ,ISUBRO
   52   FORMAT('IBUGG2,IBUGG3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,IAND1,IAND2,MAXN,NS
   53   FORMAT('ICASPL,IAND1,IAND2,MAXN,NS = ',3(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ***************************
!               **  STEP 1--             **
!               **  EXTRACT THE COMMAND  **
!               ***************************
!
      ISTEPN='11'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ASSO')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICOM.EQ.'ASSO' .AND. NUMARG.GE.1 .AND.   &
         IHARG(1).EQ.'PLOT')THEN
        ILASTC=1
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
        IFOUND='YES'
      ELSE
        IFOUND='NO'
        GO TO 9000
      ENDIF
!
!               *********************************
!               **  STEP 4--                   **
!               **  EXTRACT THE VARIABLE LIST  **
!               *********************************
!
      ISTEPN='4'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ASSO')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      INAME='ASSOCIATION PLOT'
      MINNA=1
      MAXNA=100
      MINN2=2
      IFLAGE=0
      IFLAGM=9
      IFLAGP=9
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
                  IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ASSO')THEN
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
                            ICOLR(I),PVAR(I)
  287       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',   &
                   'ICOLR(I),PVAR(I) = ',I8,2X,A4,A4,2X,3I8,G15.7)
            CALL DPWRST('XXX','BUG ')
  285     CONTINUE
        ENDIF
      ENDIF
!
!               ***********************************
!               **  STEP 22--                    **
!               **  CHECK FOR PROPER VALUES FOR  **
!               **  INPUT PARAMETERS             **
!               ***********************************
!
      IF(IVARTY(1).EQ.'PARA' .OR. IVARTY(1).EQ.'NUMB')THEN
        N11=INT(PVAR(1)+0.5)
        N21=INT(PVAR(2)+0.5)
        N12=INT(PVAR(3)+0.5)
        N22=INT(PVAR(4)+0.5)
        AN11=REAL(N11)
        AN21=REAL(N21)
        AN12=REAL(N12)
        AN22=REAL(N22)
        ICASE='PARA'
!
        ISTEPN='22'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ASSO')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        IF(N11.LT.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2201)
 2201     FORMAT('***** ERROR FROM ASSOCIATION PLOT--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2203)
 2203     FORMAT('      THE VALUE OF THE FIRST PARAMETER (N11 = THE ',   &
                 'NUMBER OF SUCCESSES')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2204)
 2204     FORMAT('      FOR THE FIRST VARIABLE MUST BE NON-NEGATIVE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2205)N11
 2205     FORMAT('      N11 = ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
!
        ELSEIF(N21.LT.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2201)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2303)
 2303     FORMAT('      THE VALUE OF THE SECOND PARAMETER (N21 = THE ',   &
                 'NUMBER OF FAILURES')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2304)
 2304     FORMAT('      FOR THE FIRST VARIABLE MUST BE NON-NEGATIVE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2305)N21
 2305     FORMAT('      N21 = ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
!
        ELSEIF(N12.LT.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2201)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2403)
 2403     FORMAT('      THE VALUE OF THE THIRD PARAMETER (N12 = THE ',   &
                 'NUMBER OF SUCCESSES')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2404)
 2404     FORMAT('      FOR THE SECOND VARIABLE MUST BE NON-NEGATIVE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2405)N12
 2405     FORMAT('      N12 = ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
!
        ELSEIF(N22.LT.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2201)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2503)
 2503     FORMAT('      THE VALUE OF THE FOURTH PARAMETER (N22 = THE ',   &
                 'NUMBER OF FAILURES')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2504)
 2504     FORMAT('      FOR THE SECOND VARIABLE MUST BE NON-NEGATIVE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2505)N22
 2505     FORMAT('      N22 = ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
!
      ELSEIF(IVARTY(1).EQ.'VARI')THEN
!
        ICASE='VARI'
        ICOL=1
        IF(NUMVAR.NE.2)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2201)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2603)
 2603     FORMAT('      MORE THAN OR LESS THAN TWO VARIABLES GIVEN.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2605)NUMVAR
 2605     FORMAT('      THE NUMBER OF VARIABLES GIVEN  = ',I5)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                    INAME,IVARN1,IVARN2,IVARTY,   &
                    ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,   &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                    MAXCP4,MAXCP5,MAXCP6,   &
                    V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                    Y1,Y2,Y2,NLOCAL,NLOCA2,NLOCA3,ICASE,   &
                    IBUGG3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
        NS1=NLOCAL
        NS2=NLOCA2
!
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ASSO')THEN
          WRITE(ICOUT,2801)NS1,NS2
 2801     FORMAT('NS1,NS2=',2I8)
          CALL DPWRST('XXX','BUG ')
          DO 2810 II=1,NS1
            WRITE(ICOUT,2811)II,Y1(II),Y2(II)
 2811       FORMAT('II,Y1(II),Y2(II)=',I8,2G15.7)
            CALL DPWRST('XXX','BUG ')
 2810     CONTINUE
        ENDIF
!
      ELSEIF(IVARTY(1).EQ.'MATR')THEN
        ICASE='MATR'
        ICOL=1
        NUMVAR=1
        CALL DPPAR6(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                    INAME,IVARN1,IVARN2,IVARTY,   &
                    ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,   &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                    MAXCP4,MAXCP5,MAXCP6,   &
                    V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                    XMAT,MAXLEV,NROW,NCOL,ICASE,   &
                    IBUGG3,ISUBRO,IFOUND,IERROR)
        ICASE='TABL'
        IF(IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!               *************************************
!               **  STEP 61--                      **
!               **  GENERATE THE ASSOCIATION PLOT  **
!               *************************************
!
      ISTEPN='61'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ASSO')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,6001)NLOCAL,ICASPL
 6001   FORMAT('NLOCAL,ICASPL=',I5,1X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      CALL DPASS2(Y1,Y2,NS1,   &
                  AN11,AN21,AN12,AN22,   &
                  XMAT,EXPFRE,RESFRE,MAXLEV,NROW,NCOL,   &
                  XIDTEM,XIDTE2,TEMP1,TEMP2,   &
                  ICASE,   &
                  Y,X,D,DCOLOR,   &
                  NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
!               *****************
!               **  STEP 9--   **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ASSO')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPASSO--')
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
      END SUBROUTINE DPASSO
      SUBROUTINE DPASS2(Y1,Y2,N,   &
                        AN11,AN21,AN12,AN22,   &
                        XMAT,EXPFRE,RESFRE,MAXLEV,NROW,NCOL,   &
                        XIDTEM,XIDTE2,TEMP1,TEMP2,   &
                        ICASE,   &
                        Y,X,D,DCOLOR,   &
                        N2,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
!     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
!              THAT WILL DEFINE AN ASSOCIATION PLOT
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2007/6
!     ORIGINAL VERSION--JUNE      2007.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASE
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IOP
!
!---------------------------------------------------------------------
!
      DIMENSION XMAT(MAXLEV,MAXLEV)
      DIMENSION EXPFRE(MAXLEV,MAXLEV)
      DIMENSION RESFRE(MAXLEV,MAXLEV)
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
!
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION D(*)
      DIMENSION DCOLOR(*)
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOF2.INC'
      CHARACTER*10 IFORMT
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPAS'
      ISUBN2='S2  '
      IERROR='NO'
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'ASS2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPASS2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGG3,ISUBRO,ICASE,N
   52   FORMAT('IBUGG3,ISUBRO,ICASE,N = ',3(A4,2X),I8)
        CALL DPWRST('XXX','WRIT')
        IF(ICASE.EQ.'VARI')THEN
          WRITE(ICOUT,55)N
   55     FORMAT('N = ',I8)
          CALL DPWRST('XXX','WRIT')
          DO 56 I=1,N
            WRITE(ICOUT,57)I,Y1(I),Y2(I)
   57       FORMAT('I,Y1(I),Y2(I) = ',I8,2G15.7)
            CALL DPWRST('XXX','WRIT')
   56     CONTINUE
        ELSEIF(ICASE.EQ.'PARA')THEN
          WRITE(ICOUT,75)AN11,AN21,AN12,AN22
   75     FORMAT('AN11,AN21,AN12,AN22 = ',4G15.7)
          CALL DPWRST('XXX','WRIT')
        ELSEIF(ICASE.EQ.'TABL')THEN
          DO 81 I=1,NROW
            DO 83 J=1,NCOL
              WRITE(ICOUT,85)I,J,XMAT(I,J)
   85         FORMAT('I,J,XMAT(I,J) = ',2I8,G15.7)
              CALL DPWRST('XXX','WRIT')
   83       CONTINUE
   81     CONTINUE
        ENDIF
      ENDIF
!
!               ********************************************
!               **  STEP 1--                              **
!               **  BRANCH TO APPROPRIATE CASE (PARAMETER **
!               **  OR VARIABLE)                          **
!               ********************************************
!
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'ASS2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICASE.EQ.'PARA')GO TO 1000
      IF(ICASE.EQ.'VARI')GO TO 2000
      IF(ICASE.EQ.'TABL')GO TO 3000
!
!               ********************************************
!               **  STEP 11--                             **
!               **  PARAMETER CASE                        **
!               ********************************************
!
 1000 CONTINUE
!
      ISTEPN='11'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'ASS2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!               ********************************************
!               **  STEP 12--                             **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      N11=INT(AN11+0.5)
      N21=INT(AN21+0.5)
      N12=INT(AN12+0.5)
      N22=INT(AN22+0.5)
!
      ISTEPN='12'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'ASS2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(N11.LT.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1201)
 1201   FORMAT('***** ERROR FROM THE ASSOCIATION PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1203)
 1203   FORMAT('      THE VALUE OF THE FIRST PARAMETER (N11 = ',   &
               'ROW 1, COLUMN 1')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1204)
 1204   FORMAT('      MUST BE NON-NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1205)N11
 1205   FORMAT('      N11 = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(N21.LT.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1201)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1303)
 1303   FORMAT('      THE VALUE OF THE SECOND PARAMETER (N21 = ',   &
               'ROW 2, COLUMN 1')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1305)N21
 1305   FORMAT('      N21 = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(N12.LT.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1201)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1403)
 1403   FORMAT('      THE VALUE OF THE THIRD PARAMETER (N12 = ',   &
               'ROW 1, COLUMN 2')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1405)N12
 1405   FORMAT('      N12 = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(N22.LT.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1201)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1503)
 1503   FORMAT('      THE VALUE OF THE FOURTH PARAMETER (N22 = ',   &
               'ROW 2, COLUMN 2')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1504)
 1504   FORMAT('      MUST BE NON-NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1505)N22
 1505   FORMAT('      N22 = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      XMAT(1,1)=AN11
      XMAT(2,1)=AN21
      XMAT(1,2)=AN12
      XMAT(2,2)=AN22
      NROW=2
      NCOL=2
!
      GO TO 4000
!
!               ********************************************
!               **  STEP 20--                             **
!               **  VARIABLE  CASE                        **
!               ********************************************
!
 2000 CONTINUE
!
!               ********************************************
!               **  STEP 21--                             **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      ISTEPN='21'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'ASS2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(N.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1201)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2101)
 2101   FORMAT('      THE NUMBER OF OBSERVATIONS IS LESS THAN 2. ')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2103)N
 2103   FORMAT('SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               ******************************************************
!               **  STEP 2.2--                                      **
!               **  DETERMINE THE NUMBER OF DISTINCT VALUES         **
!               **  FOR THE GROUP VARIABLES (Y1, Y2).               **
!               ******************************************************
!
      ISTEPN='22'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'ASS2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DISTIN(Y1,N,IWRITE,XIDTEM,NUMSE1,IBUGG3,IERROR)
      CALL SORT(XIDTEM,NUMSE1,XIDTEM)
      CALL DISTIN(Y2,N,IWRITE,XIDTE2,NUMSE2,IBUGG3,IERROR)
      CALL SORT(XIDTE2,NUMSE2,XIDTE2)
!
      IF(NUMSE1.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2201)
 2201   FORMAT('***** ERROR IN ASSOCIATION PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2202)
 2202   FORMAT('      NUMBER OF SETS    NUMSE1 = 0 ')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(NUMSE2.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2201)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2204)
 2204   FORMAT('      NUMBER OF SETS    NUMSE2 = 0 ')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      AN=N
      ANUMS1=NUMSE1
      ANUMS2=NUMSE2
!
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'ASS2')THEN
        WRITE(ICOUT,2191)N,ANUMS1,ANUMS2
 2191   FORMAT('N,ANUMS1,ANUMS2 = ',I8,2G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
!               ***********************************************
!               **  STEP 2.3--                               **
!               **  CROSS-TABULATE THE TWO VARIABLES         **
!               ***********************************************
!
      ISTEPN='23'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'ASS2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IWRITE='OFF'
!
!     COMPUTE COUNTS FOR EACH CELL
!
      J=0
      DO 2310 ISET1=1,NUMSE1
        DO 2320 ISET2=1,NUMSE2
          XMAT(ISET1,ISET2)=0.0
          K=0
          DO 2330 I=1,N
            IF(XIDTEM(ISET1).EQ.Y1(I).AND.XIDTE2(ISET2).EQ.Y2(I))THEN
              K=K+1
            ENDIF
 2330     CONTINUE
          XMAT(ISET1,ISET2)=REAL(K)
 2320   CONTINUE
 2310 CONTINUE
!
      NROW=NUMSE1
      NCOL=NUMSE2
      GO TO 4000
!
 3000 CONTINUE
!
!               ********************************************
!               **  STEP 31--                             **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               **  ALL TABLE ENTRIES SHOULD BE           **
!               **  NON-NEGATIVE INTEGERS.  NEGATIVE      **
!               **  VALUES WILL BE FLAGGED AS ERRORS      **
!               **  WHILE NON-INTEGER VALUES WILL BE      **
!               **  ROUNDED TO NEAREST INTEGER.           **
!               **  SINCE WE ARE SCANNING TABLE, COMPUTE  **
!               **  ROW AND COLUMN TOTALS.                **
!               **  NOTE THAT FOR THIS COMMAND IS         **
!               **  COMPUTED ON A 2X2 CONTINGENCY TABLE.  **
!               **  THEREFORE:                            **
!               **  1) IF NUMBER OF COLUMNS NOT EQUAL     **
!               **     TWO, FLAG AN ERROR.                **
!               **  2) IF NUMBER OF ROWS EQUAL TWO, THEN  **
!               **     EXTRACT THE RELEVANT 4 VALUES AND  **
!               **     GO TO THE PARAMETER CASE.          **
!               **  3) IF NUMBER OF ROWS GREATER THAN     **
!               **     TWO, THEN NEED TO CROSS-TABULATE   **
!               **     (I.E., HAVE THE VARIABLE CASE).    **
!               ********************************************
!
      ISTEPN='31'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'ASS2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IERROR='NO'
!
      IF(NCOL.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1201)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,3101)
 3101   FORMAT('      THE NUMBER OF COLUMNS IN THE INPUT MATRIX')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,3103)
 3103   FORMAT('      IS LESS THAN TWO.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,3105)NCOL
 3105   FORMAT('      THE NUMBER OF COLUMNS = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(NROW.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1201)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,3111)
 3111   FORMAT('      THE NUMBER OF ROWS IN THE INPUT MATRIX')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,3113)
 3113   FORMAT('      IS LESS THAN TWO.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,3115)NROW
 3115   FORMAT('      THE NUMBER OF ROWS = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!     ROUND TABLE ENTRIES TO NEAREST INTEGER AND CHECK
!     FOR NEGATIVE FREQUENCIES
!
      DO 3200 I=1,NROW
        DO 3300 J=1,NCOL
          ITEMP=INT(XMAT(I,J)+0.5)
          IF(ITEMP.LT.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,1201)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,3201)I,J
 3201       FORMAT('      ROW ',I8,' COLUMN ',I8,' OF THE INPUT ',   &
                   'TABLE')
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,3203)XMAT(I,J)
 3203       FORMAT('      CONTAINS A NEGATIVE FREQUENCY ( = ',G15.7,   &
                   ')')
            CALL DPWRST('XXX','WRIT')
            IERROR='YES'
            GO TO 9000
          ENDIF
          XMAT(I,J)=REAL(ITEMP)
          IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'ASS2')THEN
            WRITE(ICOUT,3285)I,J,XMAT(I,J)
 3285       FORMAT('I,J,XMAT(I,J) = ',2I8,G15.7)
            CALL DPWRST('XXX','WRIT')
          ENDIF
 3300   CONTINUE
 3200 CONTINUE
!
      GO TO 4000
!
 4000 CONTINUE
!
      ISTEPN='41'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'ASS2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     STEP 1: COMPUTE THE ROW TOTALS (TEMP1), COLUMN
!             TOTALS (TEMP2), AND EXPECTED FREQUENCIES
!             (EXPFRE).
!
      SUM2=0.0
      DO 4100 I=1,NROW
        SUM1=0.0
        DO 4110 J=1,NCOL
          SUM1=SUM1+XMAT(I,J)
          SUM2=SUM2+XMAT(I,J)
 4110   CONTINUE
        TEMP1(I)=SUM1
!
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'ASS2')THEN
          WRITE(ICOUT,4111)I,TEMP1(I),SUM2
 4111     FORMAT('I,TEMP1(I),SUM2 = ',I8,2G15.7)
          CALL DPWRST('XXX','WRIT')
        ENDIF
!
 4100 CONTINUE
      ATOTAL=SUM2
!
      DO 4150 J=1,NCOL
        SUM1=0.0
        DO 4160 I=1,NROW
          SUM1=SUM1+XMAT(I,J)
 4160   CONTINUE
        TEMP2(J)=SUM1
!
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'ASS2')THEN
          WRITE(ICOUT,4161)J,TEMP2(J)
 4161     FORMAT('J,TEMP2(J) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
        ENDIF
!
 4150 CONTINUE
!
!     STEP 2: COMPUTE THE EXPECTED FREQUENCES AND THE
!             STANDARDIZED RESIDUALS.
!
      ISTEPN='42'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'ASS2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      AMXRES=0.0
      AMXFRE=0.0
      DO 4200 I=1,NROW
        DO 4210 J=1,NCOL
          EXPFRE(I,J)=TEMP1(I)*TEMP2(J)/ATOTAL
          ATEMP=SQRT(EXPFRE(I,J))
          IF(ATEMP.GT.AMXFRE)AMXFRE=ATEMP
!
          IF(EXPFRE(I,J).LE.0.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,1201)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,4201)I,J
 4201       FORMAT('      ROW ',I8,' COLUMN ',I8,' OF THE EXPECTED')
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,4203)
 4203       FORMAT('      FREQUENCY TABLE IS ZERO.  UNABLE TO ',   &
                   'GENERATE THE ASSOCIATION PLOT.')
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,4205)
 4205       FORMAT('      SUGGESTED FIX: COMBINE ROWS OR ',   &
                   'COLUMNS THAT HAVE ZERO FREQUENCY.')
            CALL DPWRST('XXX','WRIT')
            IERROR='YES'
            GO TO 9000
          ENDIF
!
          RESFRE(I,J)=(XMAT(I,J) - EXPFRE(I,J))/SQRT(EXPFRE(I,J))
          ATEMP=ABS(RESFRE(I,J))
          IF(ATEMP.GT.AMXRES)AMXRES=ATEMP
!
          IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'ASS2')THEN
            WRITE(ICOUT,4211)I,J,XMAT(I,J),EXPFRE(I,J),RESFRE(I,J)
 4211       FORMAT('I,J,XMAT(I,J),EXPFRE(I,J),RESFRE(I,J) = ',   &
                   2I8,3G15.7)
            CALL DPWRST('XXX','WRIT')
          ENDIF
!
 4210   CONTINUE
 4200 CONTINUE
!
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'ASS2')THEN
        WRITE(ICOUT,4218)ATOTAL,AMXFRE,AMXRES
 4218   FORMAT('ATOTAL,AMXFRE,AMXRES = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      IF(AMXRES.LE.0.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,4291)
 4291   FORMAT('MAXIMUM RESIDUAL IS ZERO.  NO PLOT GENERATED.')
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!
!     STEP 3: NOW GENERATE THE PLOT COORDINATES FOR THE
!             ASSOCIATION PLOT.  AT EACH ENTRY OF THE TABLE
!             (I.E., ROW I, COLUMN J), GENERATE A BOX WITH
!             THE FOLLOWING WIDTH AND HEIGHT:
!
!             1) WIDTH OF BOX IS PROPORTIONAL TO
!                SQRT(EXPECTED FREQUENCY)
!
!             2) HEIGHT OF BOX IS PROPORTIONAL TO THE
!                STANDARDIZED RESIDUAL.
!
      ISTEPN='43'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'ASS2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICNT=0
      ICNT2=0
      DO 4310 I=1,NROW
        DO 4320 J=1,NCOL
!
          XCOOR=REAL(J)
          YCOOR=REAL(I)
          AWIDTH=SQRT(EXPFRE(I,J))/AMXFRE
          AWIDTH=0.4*AWIDTH
          AHEIGH=RESFRE(I,J)/AMXRES
          AHEIGH=0.4*AHEIGH
!
          ICNT2=ICNT2+1
          IF(RESFRE(I,J).GE.0.0)THEN
            ICNT=ICNT+1
            X(ICNT)=XCOOR - AWIDTH
            Y(ICNT)=YCOOR
            D(ICNT)=REAL(ICNT2)
            DCOLOR(ICNT)=1.0
            ICNT=ICNT+1
            X(ICNT)=XCOOR + AWIDTH
            Y(ICNT)=YCOOR
            D(ICNT)=REAL(ICNT2)
            DCOLOR(ICNT)=1.0
            ICNT=ICNT+1
            X(ICNT)=XCOOR + AWIDTH
            Y(ICNT)=YCOOR + AHEIGH
            D(ICNT)=REAL(ICNT2)
            DCOLOR(ICNT)=1.0
            ICNT=ICNT+1
            X(ICNT)=XCOOR - AWIDTH
            Y(ICNT)=YCOOR + AHEIGH
            D(ICNT)=REAL(ICNT2)
            DCOLOR(ICNT)=1.0
            ICNT=ICNT+1
            X(ICNT)=XCOOR - AWIDTH
            Y(ICNT)=YCOOR
            D(ICNT)=REAL(ICNT2)
            DCOLOR(ICNT)=1.0
          ELSE
            ICNT=ICNT+1
            X(ICNT)=XCOOR - AWIDTH
            Y(ICNT)=YCOOR + AHEIGH
            D(ICNT)=REAL(ICNT2)
            DCOLOR(ICNT)=2.0
            ICNT=ICNT+1
            X(ICNT)=XCOOR + AWIDTH
            Y(ICNT)=YCOOR + AHEIGH
            D(ICNT)=REAL(ICNT2)
            DCOLOR(ICNT)=2.0
            ICNT=ICNT+1
            X(ICNT)=XCOOR + AWIDTH
            Y(ICNT)=YCOOR
            D(ICNT)=REAL(ICNT2)
            DCOLOR(ICNT)=2.0
            ICNT=ICNT+1
            X(ICNT)=XCOOR - AWIDTH
            Y(ICNT)=YCOOR
            D(ICNT)=REAL(ICNT2)
            DCOLOR(ICNT)=2.0
            ICNT=ICNT+1
            X(ICNT)=XCOOR - AWIDTH
            Y(ICNT)=YCOOR + AHEIGH
            D(ICNT)=REAL(ICNT2)
            DCOLOR(ICNT)=2.0
          ENDIF
!
          IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'ASS2')THEN
            WRITE(ICOUT,4318)I,J,RESFRE(I,J),XCOOR,YCOOR,AWIDTH,AHEIGH
 4318       FORMAT('I,J,RESFRE(I,J),XCOOR,YCOOR,AWIDTH,AHEIGH = ',   &
                   2I8,5G15.7)
            CALL DPWRST('XXX','WRIT')
          ENDIF
!
 4320   CONTINUE
 4310 CONTINUE
!
      N2=ICNT
      NPLOTV=2
!
      IOP='OPEN'
      IFLAG1=1
      IFLAG2=1
      IFLAG3=1
      IFLAG4=0
      IFLAG5=0
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                  IBUGG3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      IFORMT='(   E15.7)'
      IF(NCOL.LE.9)THEN
        WRITE(IFORMT(4:4),'(I1)')NCOL
      ELSEIF(NCOL.LE.99)THEN
        WRITE(IFORMT(3:4),'(I2)')NCOL
      ELSEIF(NCOL.LE.999)THEN
        WRITE(IFORMT(2:4),'(I3)')NCOL
      ELSE
        GO TO 7019
      ENDIF
      DO 7010 I=1,NROW
        WRITE(IOUNI1,IFORMT)(XMAT(I,J),J=1,NCOL)
        WRITE(IOUNI2,IFORMT)(EXPFRE(I,J),J=1,NCOL)
        WRITE(IOUNI3,IFORMT)(RESFRE(I,J),J=1,NCOL)
 7010 CONTINUE
 7019 CONTINUE
!
      IOP='CLOS'
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                  IBUGG3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'ASS2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPASS2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICASPL,N,N2,IERROR
 9012   FORMAT('ICASPL,N,N2,IERROR = ',A4,2I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9031)N2,NPLOTV
 9031   FORMAT('N2,NPLOTV = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO 9035 I=1,N2
          WRITE(ICOUT,9036)I,Y(I),X(I),D(I)
 9036     FORMAT('I,Y(I),X(I),D(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
 9035   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPASS2
      SUBROUTINE DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                        IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--OPEN/CLOSE ONE OR MORE OF THE DATAPLOT AUXILLARY
!              FILES (dpst1f.dat, ...., dpst5f.dat).
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
!     VERSION NUMBER--2010/02
!     ORIGINAL VERSION--FEBRUARY  2010. EXTRACTED AS SEPARATE ROUTINE
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IOP
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN0
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOF2.INC'
!
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
                    IERRF1, IENDF1, IREWI1,   &
                    IFILE2, ISTAT2, IFORM2, IACCE2, IPROT2, ICURS2,   &
                    IERRF2, IENDF2, IREWI2,   &
                    IFILE3, ISTAT3, IFORM3, IACCE3, IPROT3, ICURS3,   &
                    IERRF3, IENDF3, IREWI3,   &
                    IFILE4, ISTAT4, IFORM4, IACCE4, IPROT4, ICURS4,   &
                    IERRF4, IENDF4, IREWI4,   &
                    IFILE5, ISTAT5, IFORM5, IACCE5, IPROT5, ICURS5,   &
                    IERRF5, IENDF5, IREWI5
!
!---------------------------------------------------------------------
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN0='AUFI'
!
      IF(IOP.EQ.'OPEN')THEN
!
        IF(IFLAG1.EQ.1)THEN
          IOUNI1=IST1NU
          IFILE1=IST1NA
          ISTAT1=IST1ST
          IFORM1=IST1FO
          IACCE1=IST1AC
          IPROT1=IST1PR
          ICURS1=IST1CS
          IERRF1='NO'
!
          IREWI1='ON'
          CALL DPOPFI(IOUNI1,IFILE1,ISTAT1,IFORM1,IACCE1,IPROT1,ICURS1,   &
                      IREWI1,ISUBN0,IERRF1,IBUGA3,ISUBRO,IERROR)
          IF(IERRF1.EQ.'YES')GO TO 9000
        ENDIF
!
        IF(IFLAG2.EQ.1)THEN
          IOUNI2=IST2NU
          IFILE2=IST2NA
          ISTAT2=IST2ST
          IFORM2=IST2FO
          IACCE2=IST2AC
          IPROT2=IST2PR
          ICURS2=IST2CS
          IERRF2='NO'
!
          IREWI2='ON'
          CALL DPOPFI(IOUNI2,IFILE2,ISTAT2,IFORM2,IACCE2,IPROT2,ICURS2,   &
                      IREWI2,ISUBN0,IERRF2,IBUGA3,ISUBRO,IERROR)
          IF(IERRF2.EQ.'YES')GO TO 9000
        ENDIF
!
        IF(IFLAG3.EQ.1)THEN
          IOUNI3=IST3NU
          IFILE3=IST3NA
          ISTAT3=IST3ST
          IFORM3=IST3FO
          IACCE3=IST3AC
          IPROT3=IST3PR
          ICURS3=IST3CS
          IERRF3='NO'
!
          IREWI3='ON'
          CALL DPOPFI(IOUNI3,IFILE3,ISTAT3,IFORM3,IACCE3,IPROT3,ICURS3,   &
                      IREWI3,ISUBN0,IERRF3,IBUGA3,ISUBRO,IERROR)
          IF(IERRF3.EQ.'YES')GO TO 9000
        ENDIF
!
        IF(IFLAG4.EQ.1)THEN
          IOUNI4=IST4NU
          IFILE4=IST4NA
          ISTAT4=IST4ST
          IFORM4=IST4FO
          IACCE4=IST4AC
          IPROT4=IST4PR
          ICURS4=IST4CS
          IERRF4='NO'
!
          IREWI4='ON'
          CALL DPOPFI(IOUNI4,IFILE4,ISTAT4,IFORM4,IACCE4,IPROT4,ICURS4,   &
                      IREWI4,ISUBN0,IERRF4,IBUGA3,ISUBRO,IERROR)
          IF(IERRF4.EQ.'YES')GO TO 9000
        ENDIF
!
        IF(IFLAG5.EQ.1)THEN
          IOUNI5=IST5NU
          IFILE5=IST5NA
          ISTAT5=IST5ST
          IFORM5=IST5FO
          IACCE5=IST5AC
          IPROT5=IST5PR
          ICURS5=IST5CS
          IERRF5='NO'
!
          IREWI5='ON'
          CALL DPOPFI(IOUNI5,IFILE5,ISTAT5,IFORM5,IACCE5,IPROT5,ICURS5,   &
                      IREWI5,ISUBN0,IERRF5,IBUGA3,ISUBRO,IERROR)
          IF(IERRF5.EQ.'YES')GO TO 9000
        ENDIF
!
      ELSEIF(IOP.EQ.'CLOS')THEN
!
        IF(IFLAG1.EQ.1)THEN
          IERRF1='NO'
          IENDF1='OFF'
          IREWI1='ON'
          CALL DPCLFI(IOUNI1,IFILE1,ISTAT1,IFORM1,IACCE1,IPROT1,ICURS1,   &
                      IENDF1,IREWI1,ISUBN0,IERRF1,IBUGA3,ISUBRO,IERROR)
        ENDIF
!
        IF(IFLAG2.EQ.1)THEN
          IERRF2='NO'
          IENDF2='OFF'
          IREWI2='ON'
          CALL DPCLFI(IOUNI2,IFILE2,ISTAT2,IFORM2,IACCE2,IPROT2,ICURS2,   &
                      IENDF2,IREWI2,ISUBN0,IERRF2,IBUGA3,ISUBRO,IERROR)
        ENDIF
!
        IF(IFLAG3.EQ.1)THEN
          IERRF3='NO'
          IENDF3='OFF'
          IREWI3='ON'
          CALL DPCLFI(IOUNI3,IFILE3,ISTAT3,IFORM3,IACCE3,IPROT3,ICURS3,   &
                      IENDF3,IREWI3,ISUBN0,IERRF3,IBUGA3,ISUBRO,IERROR)
        ENDIF
!
        IF(IFLAG4.EQ.1)THEN
          IERRF4='NO'
          IENDF4='OFF'
          IREWI4='ON'
          CALL DPCLFI(IOUNI4,IFILE4,ISTAT4,IFORM4,IACCE4,IPROT4,ICURS4,   &
                      IENDF4,IREWI4,ISUBN0,IERRF4,IBUGA3,ISUBRO,IERROR)
          IF(IERRF4.EQ.'YES')GO TO 9000
        ENDIF
!
        IF(IFLAG5.EQ.1)THEN
          IERRF5='NO'
          IENDF5='OFF'
          IREWI5='ON'
          CALL DPCLFI(IOUNI5,IFILE5,ISTAT5,IFORM5,IACCE5,IPROT5,ICURS5,   &
                      IENDF5,IREWI5,ISUBN0,IERRF5,IBUGA3,ISUBRO,IERROR)
          IF(IERRF5.EQ.'YES')GO TO 9000
        ENDIF
!
      ENDIF
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE DPAUFI
      SUBROUTINE DPAUPL(IHARG,NUMARG,IAUTSW,IAUTEX,IFOUND,IERROR)
!
!     PURPOSE--SPECIFY THE AUTOPLOT SWITCH WHICH IN TURN
!              DETERMINES WHETHER SAVED PLOT COMMANDS
!              SHOULD BE AUTOMATICALLY RE-EXECUTED AFTER
!              EVERY SUCCEEDING NON-PLOT COMMAND.
!              THIS CAPABILITY IS USEFUL IF ONE WISHES TO BUILD-UP
!              AN ANNOTATED PLOT BY ITERATIVELY ENTERING SUCCESSIVE
!              PLOT CONTROL COMMANDS.
!              AFTER EACH SUCH PLOT CONTROL COMMAND
!              IS ENTERED, THE SAVED PLOT STATEMENTS
!              WILL BE REECECUTED WITHOUT NEEDING
!              TO ENTER AN EXPLICIT PLOT OR REPLOT COMMAND.
!              THE SPECIFIED AUTOPLOT SWITCH SPECIFICATION
!              WILL BE PLACED IN THE HOLLERITH VARIABLE IAUTSW.
!     NOTE--IAUTEX (AN EXECUTION SWITCH) WILL ALWAYS
!           BE SET TO 'OFF' IN THIS SUBROUTINE.
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --NUMARG (AN INTEGER VARIABLE)
!     OUTPUT ARGUMENTS--IAUTSW (A HOLLERITH VARIABLE)
!                     --IAUTEX (A HOLLARITH VARIABLE)
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
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--86/7
!     ORIGINAL VERSION--MAY       1986.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IAUTSW
      CHARACTER*4 IAUTEX
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
      IFOUND='NO'
      IERROR='NO'
!
      IF(NUMARG.LE.0)GO TO 1150
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1160
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1160
      GO TO 1199
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
      IAUTSW=IHOLD
      IAUTEX='OFF'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)IAUTSW
 1181 FORMAT('THE AUTOPLOT SWITCH HAS JUST BEEN SET TO ',   &
      A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 1199
!
 1199 CONTINUE
      RETURN
      END SUBROUTINE DPAUPL
      SUBROUTINE DPAUTX(IHARG,NUMARG,IATXSW,IFOUND,IERROR)
!
!     PURPOSE--SPECIFY THE AUTO TEXT SWITCH WHICH IN TURN
!              DETERMINES WHETHER ENTERED COMMANDS WILL BE
!              PREPENDED WITH A "TEXT" STATEMENT.
!              THIS CAPABILITY IS USEFUL FOR MAKING WORD SLIDES
!              OF LONG BLOCKS OF TEXT.
!              THE SPECIFIED AUTO TEXT SWITCH SPECIFICATION
!              WILL BE PLACED IN THE HOLLERITH VARIABLE IATXSW.
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --NUMARG (AN INTEGER VARIABLE)
!     OUTPUT ARGUMENTS--IATXSW  (A HOLLERITH VARIABLE)
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
!     VERSION NUMBER--2002/8
!     ORIGINAL VERSION--AUGUST    2002.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IATXSW
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
      IFOUND='NO'
      IERROR='NO'
!
      IF(NUMARG.LE.0)GO TO 1150
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1160
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1160
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1160
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
      IATXSW=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)IATXSW
 1181 FORMAT('THE AUTO TEXT SWITCH HAS JUST BEEN SET TO ',   &
      A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 1199
!
 1199 CONTINUE
      RETURN
      END SUBROUTINE DPAUTX

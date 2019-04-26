      SUBROUTINE DPRLPP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                  IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--GIVEN Z-SCORES WITH THEIR ASSOCIATED LAB-ID's, GENERATE
C              A PLOT OF RELATIVE LAB PERFORMANCE (RLP) VERSUS
C              THE RESCALED SUM (RSZ).
C
C              THE RLP IS DEFINED AS:
C
C                  RLP = SQRT(SSQ/NMAT)
C
C              WHERE NMAT IS THE NUMBER OF MATERIALS AND
C
C                  SSQ = SUM[i=1 to n][Z(i)**2]
C
C              WHERE n IS THE NUMBER OF Z-SCORES FOR A GIVEN LAB.
C
C              THE RSZ IS DEFINED AS:
C
C                  RSCSUM = SUM[i=1 to n][X(i)]/SQRT(N)
C
C
C              THIS COMMAND IS USED IN ISO 13528 TYPE PROFICIENCY
C              ANALYSES.  IT COMBINES Z-SCORES FROM MULTIPLE
C              MATERIALS AND MULTIPLE ROUNDS AND IS ONE TOOL USED TO
C              IDENTIFY PROBLEMATIC LABORATORIES.
C
C              NOTE THAT THE ISO 13528 STANDARD SPECIFIES A NUMBER
C              OF DIFFERENT METHODS FOR COMPUTING Z-SCORES, SO THIS
C              COMMAND ASSUMES THAT THE Z-SCORE HAS ALREADY BEEN
C              COMPUTED.
C
C              THE COMMAND HAS THE FOLLOWING FORMAT:
C
C                  RPL PLOT Z LABID MATID
C
C              WHERE Z IS THE Z-SCORE OF THE RESPONSE, LABID IS THE
C              LAB-ID, AND MATID IS THE MATERIAL-ID (MATERIAL-ID ENTERS
C              IN ONLY TO COMPUTE THE NUMBER OF DISTINCT MATERIALS).
C
C     EXAMPLE--RPL PLOT Z LABID MATID
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2012/2
C     ORIGINAL VERSION--FEBRUARY   2012.
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
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IHP
      CHARACTER*4 IHP2
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
C
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
      INCLUDE 'DPCOZZ.INC'
C
      DIMENSION Z(MAXOBV)
      DIMENSION ALAB(MAXOBV)
      DIMENSION AMATID(MAXOBV)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TEMP3(MAXOBV)
C
      EQUIVALENCE (GARBAG(IGARB1),Z(1))
      EQUIVALENCE (GARBAG(IGARB2),ALAB(1))
      EQUIVALENCE (GARBAG(IGARB3),AMATID(1))
      EQUIVALENCE (GARBAG(IGARB4),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB5),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB6),TEMP3(1))
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
C
      CHARACTER*4 ISUBSW
      CHARACTER*4 ISUBTY
      CHARACTER*4 IDEFSB
C
      COMMON /RSUBR/
     1ASUBXL(MAXSUB),
     1ASUBXU(MAXSUB),
     1ASUBYL(MAXSUB),
     1ASUBYU(MAXSUB)
C
      COMMON /ISUBR/
     1ISUBNU
C
      COMMON /CSUBR/
     1ISUBTY(MAXSUB),
     1ISUBSW(MAXSUB),
     1IDEFSB
C
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      IERROR='NO'
      IFOUND='NO'
C
      ISUBN1='DPRL'
      ISUBN2='PP  '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
C               ****************************************
C               **  TREAT THE RLP         PLOT CASE   **
C               ****************************************
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'RLPP')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPRLPP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGG2,IBUGG3,IBUGQ,ISUBRO
   52   FORMAT('IBUGG2,IBUGG3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,IAND1,IAND2,MAXN
   53   FORMAT('ICASPL,IAND1,IAND2,MAXN = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***************************
C               **  STEP 1--             **
C               **  EXTRACT THE COMMAND  **
C               ***************************
C
      ISTEPN='11'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'RLPP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.GE.1 .AND. ICOM.EQ.'ISO ' .AND.
     1   IHARG(1).EQ.'1352' .AND. IHARG(2).EQ.'RLP ' .AND.
     1   IHARG(3).EQ.'PLOT')THEN
        ILASTC=3
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
        IFOUND='YES'
        ICASPL='RLP'
      ELSE
        GOTO9000
      ENDIF
C
C               ****************************************
C               **  STEP 2--                          **
C               **  EXTRACT THE VARIABLE LIST         **
C               ****************************************
C
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'RLPP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='RLP PLOT'
      MINNA=1
      MAXNA=100
      MINN2=2
      IFLAGE=1
      IFLAGM=0
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=2
      MAXNVA=3
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
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'RLPP')THEN
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
     1                      ICOLR(I),IVARTY(I)
  287       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',
     1             'ICOLR(I),IVARTY(I) = ',I8,2X,A4,A4,2X,3I8,2X,A4)
            CALL DPWRST('XXX','BUG ')
  285     CONTINUE
        ENDIF
      ENDIF
C
C               **********************************************
C               **  STEP 33--                               **
C               **  FORM THE SUBSETTED VARIABLES            **
C               **       Z(.)                               **
C               **       ALABID(.)                          **
C               **       AMATID(.)                          **
C               **********************************************
C
      ISTEPN='33'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'RLPP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICOL=1
      CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1            INAME,IVARN1,IVARN2,IVARTY,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1            MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1            MAXCP4,MAXCP5,MAXCP6,
     1            V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1            Z,ALAB,AMATID,TEMP1,TEMP1,TEMP1,TEMP1,NS,
     1            IBUGG3,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(NUMVAR.EQ.2)THEN
        DO3310I=1,NS
          AMATID(I)=1.0
 3310   CONTINUE
      ENDIF
C
      IHP='CAPV'
      IHP2='ALUE'
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IHP,IHP2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
      IF(IERROR.EQ.'YES')THEN
        XCAP=CPUMIN
      ELSE
        XCAP=VALUE(ILOCP)
      ENDIF
C
C               *******************************************************
C               **  STEP 8--                                         **
C               **  FORM THE VERTICAL AND HORIZONTAL AXIS            **
C               **  VALUES Y(.) AND X(.) FOR THE PLOT.               **
C               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).    **
C               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).    **
C               *******************************************************
C
      ISTEPN='5'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'RLPP')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,5001)NS,ICASPL
 5001   FORMAT('NS,ICASPL=',I8,1X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      CALL DPRLP2(Z,ALAB,AMATID,NS,
     1            ICASPL,MAXOBV,IRLPLA,XCAP,
     1            TEMP1,TEMP2,TEMP3,
     1            Y,X,D,X3D,
     1            NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
      IF(IERROR.EQ.'NO')THEN
        ISUBNU=ISUBNU+1
        ISUBSW(ISUBNU)='ON'
        ASUBXL(ISUBNU)=-2.0
        ASUBXU(ISUBNU)=2.0
        ASUBYL(ISUBNU)=0.0
        ASUBYU(ISUBNU)=1.5
      ENDIF
C
C               *****************
C               **  STEP 9--   **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'RLPP')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPRLPP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IFOUND,IERROR
 9013   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NPLOTV,NPLOTP,ICASPL,IAND1,IAND2
 9014   FORMAT('NPLOTV,NPLOTP,ICASPL,IAND1,IAND2 = ',
     1         2I8,2X,2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPRLP2(Z,ALAB,AMATID,N,
     1                  ICASPL,MAXOBV,IRLPLA,XCAP,
     1                  XIDTEM,XIDTE2,TEMP1,
     1                  Y,X,D,X3D,
     1                  NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--GIVEN Z-SCORES WITH THEIR ASSOCIATED LAB-ID's, GENERATE
C              A PLOT OF RELATIVE LAB PERFORMANCE (RLP) VERSUS
C              THE RESCALED SUM (RSZ).
C
C              THE RLP IS DEFINED AS:
C
C                  RLP = SQRT(SSQ/NMAT)
C
C              WHERE NMAT IS THE NUMBER OF MATERIALS AND
C
C                  SSQ = SUM[i=1 to n][Z(i)**2]
C
C              WHERE n IS THE NUMBER OF Z-SCORES FOR A GIVEN LAB.
C
C              THE RSZ IS DEFINED AS:
C
C                  RSCSUM = SUM[i=1 to n][X(i)]/SQRT(N)
C
C
C              THIS COMMAND IS USED IN ISO 13528 TYPE PROFICIENCY
C              ANALYSES.  IT COMBINES Z-SCORES FROM MULTIPLE
C              MATERIALS AND MULTIPLE ROUNDS AND IS ONE TOOL USED TO
C              IDENTIFY PROBLEMATIC LABORATORIES.
C
C              NOTE THAT THE ISO 13528 STANDARD SPECIFIES A NUMBER
C              OF DIFFERENT METHODS FOR COMPUTING Z-SCORES, SO THIS
C              COMMAND ASSUMES THAT THE Z-SCORE HAS ALREADY BEEN
C              COMPUTED.
C
C              THE COMMAND HAS THE FOLLOWING FORMAT:
C
C                  RLP PLOT Z LABID MATID
C
C              WHERE Z IS THE Z-SCORE OF THE RESPONSE, LABID IS THE
C              LAB-ID, AND MATID IS THE MATERIAL-ID (MATERIAL-ID ENTERS
C              IN ONLY TO COMPUTE THE NUMBER OF DISTINCT MATERIALS).
C
C     REFERENCE--XXXXX
C              --ISO 13528 (2005), "Statistical Methods for use in
C                proficiency testing by interlaboratory comparisons,"
C                First Edition, 2005-09-01, pp. 56-57.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2012/2
C     ORIGINAL VERSION--FEBRUARY  2012.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IRLPLA
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DIMENSION Z(*)
      DIMENSION ALAB(*)
      DIMENSION AMATID(*)
C
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
      DIMENSION TEMP1(*)
C
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION D(*)
      DIMENSION X3D(*)
C
      DOUBLE PRECISION DSUM1
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPRL'
      ISUBN2='P2  '
      IWRITE='OFF'
C
      IERROR='NO'
      NPLOTP=0
      NPLOTV=3
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'RLP2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)
   71   FORMAT('***** AT THE BEGINNING OF DPRLP2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)IBUGG3,ISUBRO,ICASPL,IRLPLA,N
   72   FORMAT('IBUGG3,ISUBRO,ICASPL,IRLPLA,N = ',4(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        IF(N.GT.0)THEN
          DO81I=1,N
            WRITE(ICOUT,82)I,Z(I),ALAB(I),AMATID(I)
   82       FORMAT('I,Z(I),ALAB(I),AMATID(I) = ',I8,3G15.7)
            CALL DPWRST('XXX','BUG ')
   81     CONTINUE
        ENDIF
      ENDIF
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      IF(N.LT.2)THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
   31   FORMAT('***** ERROR IN RPL PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,32)
   32   FORMAT('      THE NUMBER OF OBSERVATIONS MUST BE AT LEAST 2.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,34)N
   34   FORMAT('      THE ENTERED NUMBER OF OBSERVATIONS HERE = ',I6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               ********************************************
C               **  STEP 2--                              **
C               **  COMPUTE UNIQUE VALUES OF LAB AND      **
C               **  MATERIAL.                             **
C               ********************************************
C
      IWRITE='OFF'
      NPLOTP=0
      CALL DISTIN(ALAB,N,IWRITE,XIDTEM,NLAB,IBUGG3,IERROR)
      CALL SORT(XIDTEM,NLAB,XIDTEM)
      CALL DISTIN(AMATID,N,IWRITE,XIDTE2,NMAT,IBUGG3,IERROR)
      CALL SORT(XIDTE2,NMAT,XIDTE2)
C
C               ********************************************
C               **  STEP 3--                              **
C               **  GENERATE THE PLOT COORDINATES.        **
C               ********************************************
C
      ANMAT=REAL(NMAT)
      DO2010J=1,NLAB
        HOLD=XIDTEM(J)
        K=0
        DO2020I=1,N
          IF(ALAB(I).EQ.HOLD)THEN
            K=K+1
            TEMP1(K)=Z(I)
          ENDIF
 2020   CONTINUE
        IF(K.GE.1)THEN
          CALL RSCSUM(TEMP1,K,XCAP,IWRITE,RSZ,IBUGG3,ISUBRO,IERROR)
          CALL SSQ(TEMP1,K,XCAP,IWRITE,ATEMP,IBUGG3,ISUBRO,IERROR)
          RLP=SQRT(ATEMP/ANMAT)
C
          NPLOTP=NPLOTP+1
          Y(NPLOTP)=RLP
          X(NPLOTP)=RSZ
          D(NPLOTP)=1.0
          X3D(NPLOTP)=0.0
C
          IF(IRLPLA.EQ.'ALL')THEN
            NPLOTP=NPLOTP+1
            Y(NPLOTP)=RLP
            X(NPLOTP)=RSZ
            D(NPLOTP)=2.0
            X3D(NPLOTP)=HOLD
          ELSEIF(IRLPLA.EQ.'ACTI')THEN
            IF(RLP.GT.1.5 .OR. ABS(RSZ).GT.3.0)THEN
              NPLOTP=NPLOTP+1
              Y(NPLOTP)=RLP
              X(NPLOTP)=RSZ
              D(NPLOTP)=2.0
              X3D(NPLOTP)=HOLD
            ENDIF
          ELSEIF(IRLPLA.EQ.'WARN')THEN
            IF(RLP.GT.1.5 .OR. ABS(RSZ).GT.2.0)THEN
              NPLOTP=NPLOTP+1
              Y(NPLOTP)=RLP
              X(NPLOTP)=RSZ
              D(NPLOTP)=2.0
              X3D(NPLOTP)=HOLD
            ENDIF
          ENDIF
        ENDIF
C
 2010 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'RLP2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPRLP2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IERROR,NPLOTP,NPLOTV
 9013   FORMAT('IERROR,NPLOTP,NPLOTV = ',A4,2X,2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NPLOTP.GT.0)THEN
          DO9035I=1,NPLOTP
            WRITE(ICOUT,9036)I,Y(I),X(I),D(I)
 9036       FORMAT('I,Y(I),X(I),D(I) = ',I8,3G15.7)
            CALL DPWRST('XXX','BUG ')
 9035     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPROAC(IHARG,IARGT,ARG,NUMARG,DEFRAC,
     1ROOTAC,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE ROOT ACCURACY.
C              THE DIFFERENCE IN FUNCTION VALUES AFTER EACH
C              ITERATION OF A ROOT EXTRACTION WILL BE COMPARED
C              TO THE SPECIFIED ROOT ACCURACY.
C              THE SPECIFIED ROOT ACCURACY VALUE WILL BE PLACED
C              IN THE FLOATING POINT VARIABLE ROOTAC.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --ARG    (A  FLOATING POINT VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C                     --DEFRAC (A  FLOATING POINT VARIABLE)
C     OUTPUT ARGUMENTS--ROOTAC  (A  FLOATING POINT VARIABLE)
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--NOVEMBER 1980.
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
      DIMENSION ARG(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(NUMARG.EQ.0)GOTO1199
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'=')GOTO1199
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'ACCU')GOTO1110
      GOTO1199
C
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'ACCU')GOTO1150
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
 1121 FORMAT('***** ERROR IN DPROAC--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR ROOT ACCURACY ',
     1'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1124)
 1124 FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE ',
     1'PROPER FORM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1125)
 1125 FORMAT('      SUPPOSE THE THE ANALYST WILL BE CARRYING OUT  ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1126)
 1126 FORMAT('      A ROOT-EXTRACTION, ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1127)
 1127 FORMAT('      AND SUPPOSE THE ANALYST WISHES TO TERMINATE  ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1128)
 1128 FORMAT('      THE ROOT-FINDING PROCESS WHENEVER SUCCESSIVE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1129)
 1129 FORMAT('      X DIFFERENCES ARE .00001 OR SMALLER; ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1130)
 1130 FORMAT('      THEN THE ALLOWABLE FORM IS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1131)
 1131 FORMAT('      ROOT ACCURACY .00001 ')
      CALL DPWRST('XXX','BUG ')
      GOTO1199
C
 1150 CONTINUE
      HOLD=DEFRAC
      GOTO1180
C
 1160 CONTINUE
      HOLD=ARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      ROOTAC=HOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)ROOTAC
 1181 FORMAT('THE ROOT ACCURACY HAS JUST BEEN SET TO ',
     1E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPROC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                 IBUGG2,IBUGG3,ISUBRO,IBUGQ,IFOUND,IERROR)
C
C     PURPOSE--GENERATE A ROC CURVE.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2007/7
C     ORIGINAL VERSION--JULY      2007.
C     UPDATED         --APRIL     2011. USE DPPARS AND DPPAR5
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IAND1
      CHARACTER*4 IAND2
      CHARACTER*4 IBUGG2
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGQ
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
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
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
C
      DIMENSION Y1(MAXOBV)
      DIMENSION Y2(MAXOBV)
      DIMENSION Y3(MAXOBV)
      DIMENSION XGROUP(MAXOBV)
      DIMENSION XGROU2(MAXOBV)
      DIMENSION XIDTEM(MAXOBV)
      DIMENSION XIDTE2(MAXOBV)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TEMP3(MAXOBV)
      DIMENSION TEMP4(MAXOBV)
      DIMENSION TEMP5(MAXOBV)
C
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),Y2(1))
      EQUIVALENCE (GARBAG(IGARB3),XGROUP(1))
      EQUIVALENCE (GARBAG(IGARB4),XGROU2(1))
      EQUIVALENCE (GARBAG(IGARB5),XIDTEM(1))
      EQUIVALENCE (GARBAG(IGARB6),XIDTE2(1))
      EQUIVALENCE (GARBAG(IGARB7),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB8),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB9),TEMP3(1))
      EQUIVALENCE (GARBAG(IGAR10),TEMP4(1))
      EQUIVALENCE (GARBAG(JGAR11),TEMP5(1))
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCODA.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      ISUBN1='DPRO'
      ISUBN2='C   '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
C               ********************************
C               **  TREAT THE ROC CURVE CASE  **
C               ********************************
C
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'ROC ')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPROC--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,IAND1,IAND2,MAXNPP
   53   FORMAT('ICASPL,IAND1,IAND2,MAXNPP = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)IBUGG2,IBUGG3,ISUBRO,IBUGQ
   54   FORMAT('IBUGG2,IBUGG3,ISUBRO,IBUGQ = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *******************************************
C               **  STEP 1--                             **
C               **  SEARCH FOR ROC CURVE                 **
C               *******************************************
C
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'RPLO')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.GE.1.AND.
     1  (IHARG(1).EQ.'PLOT' .OR. IHARG(1).EQ.'CURV'))THEN
        ICASPL='ROC '
        ILASTC=1
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
        IFOUND='YES'
        INAME='ROC CURVE'
        IHARG(NUMARG+1)='    '
        IHARG2(NUMARG+1)='    '
      ELSEIF(NUMARG.GE.2.AND.
     1  IHARG(1).EQ.'ROC '.AND.
     1  (IHARG(2).EQ.'PLOT' .OR. IHARG(2).EQ.'CURV'))THEN
        ICASPL='PROC'
        INAME='PSUEDO ROC CURVE'
        ILASTC=2
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
        IFOUND='YES'
        IHARG(NUMARG+1)='    '
        IHARG2(NUMARG+1)='    '
      ELSE
        ICASPL='    '
        GOTO9000
      ENDIF
C
C               ****************************************
C               **  STEP 2--                          **
C               **  EXTRACT THE VARIABLE LIST         **
C               ****************************************
C
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PROC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      MINNA=3
      MAXNA=100
      MINN2=2
      IFLAGE=1
      IFLAGM=0
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      IF(ICASPL.EQ.'ROC')THEN
        MINNVA=3
        MAXNVA=4
      ELSE
        MINNVA=4
        MAXNVA=5
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
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PROC')THEN
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
     1                      ICOLR(I),IVARTY(I)
  287       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',
     1             'ICOLR(I),IVARTY(I) = ',I8,2X,A4,A4,2X,3I8,2X,A4)
            CALL DPWRST('XXX','BUG ')
  285     CONTINUE
        ENDIF
      ENDIF
C
C               **********************************************
C               **  STEP 33--                               **
C               **  FORM THE SUBSETTED VARIABLES            **
C               **********************************************
C
      ISTEPN='33'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PROC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICOL=1
      IF(ICASPL.EQ.'ROC')THEN
        CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              Y1,Y2,XGROUP,XGROU2,TEMP1,TEMP1,TEMP1,NS,
     1              IBUGG3,ISUBRO,IFOUND,IERROR)
      ELSE
        CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              Y1,Y2,Y3,XGROUP,XGROU2,TEMP1,TEMP1,NS,
     1              IBUGG3,ISUBRO,IFOUND,IERROR)
      ENDIF
      IF(IERROR.EQ.'YES')GOTO9000
C
C               *****************************************************
C               **  STEP 41--                                      **
C               **  FORM THE VERTICAL AND HORIZONTAL AXIS          **
C               **  VARIABLES (Y(.) AND X(.), RESPECTIVELY) FOR    **
C               **  THE PLOT.                                      **
C               **  FORM THE CURVE DESIGNATION VARIABLE D(.)  .    **
C               **  DEFINE THE NUMBER OF PLOT POINTS   (NPLOTP).   **
C               **  DEFINE THE NUMBER OF PLOT VARIABLES(NPLOTV).   **
C               *****************************************************
C
      ISTEPN='61'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PROC')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        DO3180I=1,NS
          WRITE(ICOUT,3182)I,Y1(I),Y2(I),XGROUP(I),XGROU2(I)
 3182     FORMAT('I,Y1(I),Y2(I),XGROUP(I),XGROU2(I)=',I8,4G15.7)
          CALL DPWRST('XXX','BUG ')
 3180   CONTINUE
      ENDIF
C
      IF(ICASPL.EQ.'ROC')THEN
        CALL DPROC2(Y1,Y2,XGROUP,XGROU2,NS,NUMVAR,
     1              ICASPL,MAXN,
     1              XIDTEM,XIDTE2,TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,
     1              Y,X,X3D,D,NPLOTP,NPLOTV,AUC,
     1              IBUGG3,ISUBRO,IERROR)
      ELSE
        CALL DPROC3(Y1,Y2,Y3,XGROUP,XGROU2,NS,NUMVAR,
     1              ICASPL,MAXN,
     1              XIDTEM,XIDTE2,TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,
     1              Y,X,X3D,D,NPLOTP,NPLOTV,
     1              IBUGG3,ISUBRO,IERROR)
      ENDIF
C
C               ***************************************
C               **  STEP 62--                        **
C               **  UPDATE INTERNAL DATAPLOT TABLES  **
C               ***************************************
C
      ISTEPN='62'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ROC ')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ISUBN0='ROC '
C
      IF(NUMVAR.LE.3)THEN
        IH='AUC '
        IH2='    '
        VALUE0=AUC
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGG2,IERROR)
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'ROC ')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DPROC--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR,ICASPL
 9012   FORMAT('IFOUND,IERROR,ICASPL = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTV,NPLOTP,NS,IAND1,IAND2
 9013   FORMAT('NPLOTV,NPLOTP,NS,IAND1,IAND2 = ',3I8,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9020)
 9020   FORMAT('I,Y(.),X(.),D(.),ISUB(.)--')
        CALL DPWRST('XXX','BUG ')
        DO9021I=1,NPLOTP
          WRITE(ICOUT,9022)I,Y(I),X(I),D(I),ISUB(I)
 9022     FORMAT(I8,3G15.7,I8)
          CALL DPWRST('XXX','BUG ')
 9021   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPROC2(Y1,Y2,XGROUP,XSET,N,NUMV2,
     1            ICASPL,MAXN,
     1            XIDTEM,XIDTE2,TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,
     1            YPLOT,XPLOT,X3D,D2,NPLOTP,NPLOTV,AUC,
     1            IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--FORM A ROC CURVE.
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
C     VERSION NUMBER--2007/7
C     ORIGINAL VERSION--JULY      2007.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      CHARACTER*4 IWRITE
C
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION XGROUP(*)
      DIMENSION XSET(*)
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
      DIMENSION TEMP4(*)
      DIMENSION TEMP5(*)
      DIMENSION YPLOT(*)
      DIMENSION XPLOT(*)
      DIMENSION X3D(*)
      DIMENSION D2(*)
C
      INCLUDE 'DPCOF2.INC'
C
      CHARACTER*80 IFILE1
      CHARACTER*12 ISTAT1
      CHARACTER*12 IFORM1
      CHARACTER*12 IACCE1
      CHARACTER*12 IPROT1
      CHARACTER*12 ICURS1
      CHARACTER*4 IERRF1
      CHARACTER*4 IENDF1
      CHARACTER*4 IREWI1
C
C-----COMMON----------------------------------------------------------
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPRO'
      ISUBN2='C2  '
      IERROR='NO'
      IWRITE='OFF'
C
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'ROC2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPROC2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)NUMV2,N,MAXN
   52   FORMAT('NUMV2,N,MAXN = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,IBUGG3,IERROR
   53   FORMAT('ICASPL,IBUGG3,IERROR = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,MIN(N,100)
          WRITE(ICOUT,56)I,Y1(I),Y2(I),XGROUP(I),XSET(I)
   56     FORMAT('I,Y1(I),Y2(I),XGROUP(I),XSET(I) = ',I8,4G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'ROC2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               ****************************************************
C               **  STEP 2--                                      **
C               **  COMPUTE COORDINATES FOR ROC CURVE             **
C               ****************************************************
C
      ISTEPN='2'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'ROC2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMV2.EQ.3)THEN
        CALL DISTIN(XGROUP,N,IWRITE,XIDTEM,NUMSET,IBUGG3,IERROR)
        CALL SORT(XIDTEM,NUMSET,XIDTEM)
C
        XPLOT(1)=0.0
        YPLOT(1)=0.0
        D2(1)=1.0
        XPLOT(2)=1.0
        YPLOT(2)=1.0
        D2(2)=1.0
C
        J=2
        ITAG=2
        ICNT=0
        DO1000ISET=1,NUMSET
          HOLD=XIDTEM(ISET)
C
          K=0
          DO1010I=1,N
            IF(XGROUP(I).EQ.HOLD)THEN
              K=K+1
              TEMP1(K)=Y1(I)
              TEMP2(K)=Y2(I)
            ENDIF
 1010     CONTINUE
C
          IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'ROC2')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1051)ISET,K
 1051       FORMAT('***** SET ',I8,' HAS ',I8,' ELEMENTS.')
            CALL DPWRST('XXX','BUG ')
            IF(K.GT.0)THEN
              DO1055I=1,K
                WRITE(ICOUT,1057)I,TEMP1(I),TEMP2(I)
 1057           FORMAT('I,TEMP1(I),TEMP2(I) = ',I8,2G15.7)
                CALL DPWRST('XXX','BUG ')
 1055         CONTINUE
            ENDIF
          ENDIF
C
          CALL SENSIT(TEMP1,TEMP2,K,IWRITE,TEMP3,SENS,IBUGG3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
          CALL SPECIF(TEMP1,TEMP2,K,IWRITE,TEMP3,SPEC,IBUGG3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
          ICNT=ICNT+1
          J=J+1
          YPLOT(J)=SENS
          XPLOT(J)=1.0 - SPEC
          D2(J)=REAL(ITAG)
C
          IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'ROC2')THEN
            WRITE(ICOUT,1061)SENS,SPEC
 1061       FORMAT('SENSIT, SPEC = ',2G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
 1000   CONTINUE
C
        ICNT2=2
        DO1090I=1,ICNT
          J=J+1
          ICNT2=ICNT2+1
          ITAG=ITAG+1
          YPLOT(J)=YPLOT(ICNT2)
          XPLOT(J)=XPLOT(ICNT2)
          D2(J)=REAL(ITAG)
 1090   CONTINUE
C
        N2=J
        NPLOTP=N2
        NPLOTV=2
C
C       COMPUTE AUC STATISTIC USING INTEGRATION.
C
        K=1
        TEMP1(K)=0.0
        TEMP2(K)=0.0
        DO1200I=1,NPLOTP
          IF(D2(I).EQ.2.0)THEN
            K=K+1
            TEMP1(K)=YPLOT(I)
            TEMP2(K)=XPLOT(I)
          ENDIF
 1200   CONTINUE
        K=K+1
        TEMP1(K)=1.0
        TEMP2(K)=1.0
C
        NUMV2=2
        IWRITE='OFF'
        CALL INTVEC(TEMP1,TEMP2,K,NUMV2,IWRITE,AUC,IBUGG3,IERROR)
C
C       FOR 4 VARIABLE CASE:
C
C       1) XGROUP IDENTIFIES THE GROUP (I.E., MACHINE)
C       2) XSET   IDENTIFIES SETTING WITH GROUP (I.E., THE
C                 SETTINGS FOR A SPECIFIC MACHINE)
C
      ELSEIF(NUMV2.EQ.4)THEN
        CALL DISTIN(XGROUP,N,IWRITE,XIDTEM,NUMSET,IBUGG3,IERROR)
        CALL SORT(XIDTEM,NUMSET,XIDTEM)
        CALL DISTIN(XSET,N,IWRITE,XIDTE2,NUMSE2,IBUGG3,IERROR)
        CALL SORT(XIDTE2,NUMSE2,XIDTE2)
C
        XPLOT(1)=0.0
        YPLOT(1)=0.0
        D2(1)=1.0
        XPLOT(2)=1.0
        YPLOT(2)=1.0
        D2(2)=1.0
C
        IOUNI1=IST1NU
        IFILE1=IST1NA
        ISTAT1=IST1ST
        IFORM1=IST1FO
        IACCE1=IST1AC
        IPROT1=IST1PR
        ICURS1=IST1CS
        ISUBN0='TAB2'
        IERRF1='NO'
C
        IREWI1='ON'
        CALL DPOPFI(IOUNI1,IFILE1,ISTAT1,IFORM1,IACCE1,IPROT1,ICURS1,
     1              IREWI1,ISUBN0,IERRF1,IBUGG3,ISUBRO,IERROR)
        IF(IERRF1.EQ.'YES')GOTO9000
C
        J=2
        ITAG=1
C
        DO2000ISET=1,NUMSET
          HOLD=XIDTEM(ISET)
          ITAG=ITAG+1
          TEMP3(1)=0.0
          TEMP4(1)=0.0
          ICNT2=1
C
          DO3000ISET2=1,NUMSE2
            HOLD2=XIDTE2(ISET2)
C
            IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'ROC2')THEN
              WRITE(ICOUT,3011)
 3011         FORMAT('ISET,ISET2,HOLD,HOLD2 = ',2I8,2G15.7)
              CALL DPWRST('XXX','BUG ')
            ENDIF
C
            K=0
            DO2010I=1,N
              IF(XGROUP(I).EQ.HOLD .AND. XSET(I).EQ.HOLD2)THEN
                K=K+1
                TEMP1(K)=Y1(I)
                TEMP2(K)=Y2(I)
              ENDIF
 2010       CONTINUE
C
            IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'ROC2')THEN
              WRITE(ICOUT,3013)
 3013         FORMAT('K = ',I8)
              CALL DPWRST('XXX','BUG ')
              DO3015II=1,K
                WRITE(ICOUT,3017)
 3017           FORMAT('II,TEMP1(II),TEMP2(II) = ',I8,2G15.7)
                CALL DPWRST('XXX','BUG ')
 3015         CONTINUE
            ENDIF
C
            CALL SENSIT(TEMP1,TEMP2,K,IWRITE,TEMP5,SENS,IBUGG3,IERROR)
            IF(IERROR.EQ.'YES')GOTO9000
            CALL SPECIF(TEMP1,TEMP2,K,IWRITE,TEMP5,SPEC,IBUGG3,IERROR)
            IF(IERROR.EQ.'YES')GOTO9000
C
            J=J+1
            YPLOT(J)=SENS
            XPLOT(J)=1.0 - SPEC
            D2(J)=REAL(ITAG)
C
            ICNT2=ICNT2+1
            TEMP3(ICNT2)=XPLOT(J)
            TEMP4(ICNT2)=YPLOT(J)
C
 3000     CONTINUE
C
          ICNT2=ICNT2+1
          TEMP3(ICNT2)=1.0
          TEMP4(ICNT2)=1.0
          NUMV2=2
          IWRITE='OFF'
          CALL INTVEC(TEMP3,TEMP4,ICNT2,NUMV2,IWRITE,AUC,
     1                IBUGG3,IERROR)
          WRITE(IOUNI1,2029)ISET,AUC
 2029     FORMAT(I8,2X,E15.7)
C
 2000   CONTINUE
C
        IENDF1='OFF'
        IREWI1='ON'
        CALL DPCLFI(IOUNI1,IFILE1,ISTAT1,IFORM1,IACCE1,IPROT1,ICURS1,
     1              IENDF1,IREWI1,ISUBN0,IERRF1,IBUGG3,ISUBRO,IERROR)
        IF(IERRF1.EQ.'YES')GOTO9000
C
        N2=J
        NPLOTP=N2
        NPLOTV=2
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'ROC2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DPROC2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTV,NPLOTP,N,ICASPL
 9013   FORMAT('NPLOTV,NPLOTP,N,ICASPL = ',
     1         I8,I8,I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9020)
 9020   FORMAT('I,YPLOT(.),XPLOT(.),X3D(.),D2(.)--')
        CALL DPWRST('XXX','BUG ')
        DO9021I=1,NPLOTP
          WRITE(ICOUT,9022)I,YPLOT(I),XPLOT(I),X3D(I),D2(I)
 9022     FORMAT(I8,4G15.7)
          CALL DPWRST('XXX','BUG ')
 9021   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPROC3(Y1,Y2,Y3,XGROUP,XSET,N,NUMV2,
     1            ICASPL,MAXN,
     1            XIDTEM,XIDTE2,TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,
     1            YPLOT,XPLOT,X3D,D2,NPLOTP,NPLOTV,
     1            IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--FORM A PSUEDO ROC CURVE.
C
C              THIS IS A VARIANT OF THE ROC CURVE.  WHERE THE
C              ROC CURVE PLOTS SENSITIVITY VERSUS (1 - SPECIFICITY),
C              THE PSUEDO ROC CURVE PLOTS PROBABILITY CORRECT
C              VERSUS PROBABILITY FALSE POSITIVE.
C
C              THIS VARIANT IS MOTIVATED BY THE CASE WHERE
C              THE "GROUND TRUTH" IS ALWAYS "1" (I.E., PRESENT).
C              IN ADDITION, THE "OBSERVED" CAN BE MORE FLEXIBLE
C              THAN SIMPLY PRESENT OR ABSENT.  IN THIS CASE,
C              WE DEFINE A FALSE NEGATIVE AS TOO LOW AN ALARM
C              AND A FALSE POSITIVE AS TOO HIGH AN ALARM.
C
C              THE DATA CONSISTS OF:
C
C                  Y1 = 1   CORRECT MATCH
C                     = 0   INCORRECT MATCH
C                  Y2 = 1   FALSE POSITIVE
C                     = 0   NO FALSE POSITIVE
C                  Y3 = 1   FALSE NEGATIVE
C                     = 0   NO FALSE NEGATIVE
C
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2007/7
C     ORIGINAL VERSION--JULY      2007.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      CHARACTER*4 IWRITE
C
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION Y3(*)
      DIMENSION XGROUP(*)
      DIMENSION XSET(*)
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
      DIMENSION TEMP4(*)
      DIMENSION TEMP5(*)
      DIMENSION YPLOT(*)
      DIMENSION XPLOT(*)
      DIMENSION X3D(*)
      DIMENSION D2(*)
C
      INCLUDE 'DPCOF2.INC'
C
      CHARACTER*80 IFILE1
      CHARACTER*12 ISTAT1
      CHARACTER*12 IFORM1
      CHARACTER*12 IACCE1
      CHARACTER*12 IPROT1
      CHARACTER*12 ICURS1
      CHARACTER*4 IERRF1
      CHARACTER*4 IENDF1
      CHARACTER*4 IREWI1
C
C-----COMMON----------------------------------------------------------
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPRO'
      ISUBN2='C3  '
      IERROR='NO'
      IWRITE='OFF'
C
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'ROC3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPROC3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)NUMV2,N,MAXN
   52   FORMAT('NUMV2,N,MAXN = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,IBUGG3,IERROR
   53   FORMAT('ICASPL,IBUGG3,IERROR = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,MIN(N,100)
          WRITE(ICOUT,56)I,Y1(I),Y2(I),XGROUP(I),XSET(I)
   56     FORMAT('I,Y1(I),Y2(I),XGROUP(I),XSET(I) = ',I8,4G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ************************************************
C               **  STEP 1--                                  **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS      **
C               **  1) ROWS OF Y1, Y2, AND Y3 MUST SUM TO 1   **
C               ************************************************
C
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'ROC3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO100I=1,N
C
        ITEMP1=INT(Y1(I)+0.5)
        IF(ITEMP1.LT.0 .OR. ITEMP1.GT.1)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
  101     FORMAT('***** ERROR IN PSUEDO ROC CURVE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,103)
  103     FORMAT('      RESPONSE VARIABLE 1 (CORRECT MATCH) SHOULD')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,105)I,Y1(I)
  105     FORMAT('      BE EITHER 0 OR 1.  ROW ',I8,' HAS THE VALUE ',
     1           G15.7)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
        Y1(I)=REAL(ITEMP1)
C
        ITEMP2=INT(Y2(I)+0.5)
        IF(ITEMP2.GT.1)ITEMP2=1
        IF(ITEMP2.LT.0 .OR. ITEMP2.GT.1)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,113)
  113     FORMAT('      RESPONSE VARIABLE 2 (FALSE POSITIVE) SHOULD')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,115)I,Y2(I)
  115     FORMAT('      BE EITHER 0 OR 1.  ROW ',I8,' HAS THE VALUE ',
     1           G15.7)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
        Y2(I)=REAL(ITEMP2)
C
        ITEMP3=INT(Y3(I)+0.5)
        IF(ITEMP3.GT.1)ITEMP3=1
        IF(ITEMP3.LT.0 .OR. ITEMP3.GT.1)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,123)
  123     FORMAT('      RESPONSE VARIABLE 3 (FALSE NEGATIVE) SHOULD')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,125)I,Y3(I)
  125     FORMAT('      BE EITHER 0 OR 1.  ROW ',I8,' HAS THE VALUE ',
     1           G15.7)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
        Y3(I)=REAL(ITEMP3)
C
C       IF ITEMP1 = 1, BOTH ITEMP2 AND ITEMP3 SHOULD BE ZERO.
C
        IF(ITEMP1.EQ.1)THEN
          IF(ITEMP2.EQ.1 .OR. ITEMP3.EQ.1)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,101)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,133)
  133       FORMAT('      IF A CORECT MATCH SPECIFIED, THEN BOTH ',
     1             'THE FALSE POSITIVE')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,134)
  134       FORMAT('      AND THE FALSE NEGATIVE SHOULD BE 0.  SUCH')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,135)
  135       FORMAT('      WAS NOT THE CASE FOR ROW ',I8,'.')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
C
C       IF ITEMP1 = 0, EITHER ITEMP2 OR ITEMP3 SHOULD BE ZERO.
C
        ELSEIF(ITEMP1.EQ.0)THEN
          IF(ITEMP2.EQ.0 .AND. ITEMP3.EQ.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,101)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,143)
  143       FORMAT('      IF AN INCORECT MATCH SPECIFIED, THEN ',
     1             'EITHER THE FALSE POSITIVE')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,144)
  144       FORMAT('      OR THE FALSE NEGATIVE SHOULD BE 1.  SUCH')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,145)
  145       FORMAT('      WAS NOT THE CASE FOR ROW ',I8,'.')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
        ENDIF
  100 CONTINUE
C
C               ****************************************************
C               **  STEP 2--                                      **
C               **  COMPUTE COORDINATES FOR ROC CURVE             **
C               ****************************************************
C
      ISTEPN='2'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'ROC3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMV2.EQ.4)THEN
        CALL DISTIN(XGROUP,N,IWRITE,XIDTEM,NUMSET,IBUGG3,IERROR)
        CALL SORT(XIDTEM,NUMSET,XIDTEM)
C
        J=0
        ITAG=0
        DO1000ISET=1,NUMSET
          HOLD=XIDTEM(ISET)
C
          K=0
          DO1010I=1,N
            IF(XGROUP(I).EQ.HOLD)THEN
              K=K+1
              TEMP1(K)=Y1(I)
              TEMP2(K)=Y2(I)
            ENDIF
 1010     CONTINUE
C
          IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'ROC3')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1051)ISET,K
 1051       FORMAT('***** SET ',I8,' HAS ',I8,' ELEMENTS.')
            CALL DPWRST('XXX','BUG ')
            IF(K.GT.0)THEN
              DO1055I=1,K
                WRITE(ICOUT,1057)I,TEMP1(I),TEMP2(I)
 1057           FORMAT('I,TEMP1(I),TEMP2(I) = ',I8,2G15.7)
                CALL DPWRST('XXX','BUG ')
 1055         CONTINUE
            ENDIF
          ENDIF
C
C         COMPUTE PROPORTION CORRECT AND PROPORTION OF FALSE
C         POSITIVES.
C
          CALL SUMDP(TEMP1,K,IWRITE,PID,IBUGG3,IERROR)
          PID=PID/REAL(K)
          IF(IERROR.EQ.'YES')GOTO9000
          CALL SUMDP(TEMP2,K,IWRITE,PFP,IBUGG3,IERROR)
          PFP=PFP/REAL(K)
          IF(IERROR.EQ.'YES')GOTO9000
          J=J+1
          ITAG=ITAG+1
          YPLOT(J)=PID
          XPLOT(J)=PFP
          D2(J)=REAL(ITAG)
C
          IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'ROC3')THEN
            WRITE(ICOUT,1061)PID,PFP
 1061       FORMAT('PID,PFP = ',2G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
 1000   CONTINUE
C
        N2=J
        NPLOTP=N2
        NPLOTV=2
C
C       FOR 4 VARIABLE CASE:
C
C       1) XGROUP IDENTIFIES THE GROUP (I.E., MACHINE)
C       2) XSET   IDENTIFIES SETTING WITH GROUP (I.E., THE
C                 SETTINGS FOR A SPECIFIC MACHINE)
C
      ELSEIF(NUMV2.EQ.5)THEN
        CALL DISTIN(XGROUP,N,IWRITE,XIDTEM,NUMSET,IBUGG3,IERROR)
        CALL SORT(XIDTEM,NUMSET,XIDTEM)
        CALL DISTIN(XSET,N,IWRITE,XIDTE2,NUMSE2,IBUGG3,IERROR)
        CALL SORT(XIDTE2,NUMSE2,XIDTE2)
C
        J=0
        ITAG=0
C
        DO2000ISET=1,NUMSET
          HOLD=XIDTEM(ISET)
          ITAG=ITAG+1
          TEMP3(1)=0.0
          TEMP4(1)=0.0
          ICNT2=1
C
          DO3000ISET2=1,NUMSE2
            HOLD2=XIDTE2(ISET2)
C
            IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'ROC3')THEN
              WRITE(ICOUT,3011)
 3011         FORMAT('ISET,ISET2,HOLD,HOLD2 = ',2I8,2G15.7)
              CALL DPWRST('XXX','BUG ')
            ENDIF
C
            K=0
            DO2010I=1,N
              IF(XGROUP(I).EQ.HOLD .AND. XSET(I).EQ.HOLD2)THEN
                K=K+1
                TEMP1(K)=Y1(I)
                TEMP2(K)=Y2(I)
              ENDIF
 2010       CONTINUE
C
            IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'ROC3')THEN
              WRITE(ICOUT,3013)
 3013         FORMAT('K = ',I8)
              CALL DPWRST('XXX','BUG ')
              DO3015II=1,K
                WRITE(ICOUT,3017)
 3017           FORMAT('II,TEMP1(II),TEMP2(II) = ',I8,2G15.7)
                CALL DPWRST('XXX','BUG ')
 3015         CONTINUE
            ENDIF
C
            CALL SUMDP(TEMP1,K,IWRITE,PID,IBUGG3,IERROR)
            PID=PID/REAL(K)
            IF(IERROR.EQ.'YES')GOTO9000
            CALL SUMDP(TEMP2,K,IWRITE,PFP,IBUGG3,IERROR)
            PFP=PFP/REAL(K)
            IF(IERROR.EQ.'YES')GOTO9000
C
            J=J+1
            YPLOT(J)=PID
            XPLOT(J)=PFP
            D2(J)=REAL(ITAG)
C
 3000     CONTINUE
 2000   CONTINUE
C
        N2=J
        NPLOTP=N2
        NPLOTV=2
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'ROC3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DPROC3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTV,NPLOTP,N,ICASPL
 9013   FORMAT('NPLOTV,NPLOTP,N,ICASPL = ',
     1         I8,I8,I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9020)
 9020   FORMAT('I,YPLOT(.),XPLOT(.),X3D(.),D2(.)--')
        CALL DPWRST('XXX','BUG ')
        DO9021I=1,NPLOTP
          WRITE(ICOUT,9022)I,YPLOT(I),XPLOT(I),X3D(I),D2(I)
 9022     FORMAT(I8,4G15.7)
          CALL DPWRST('XXX','BUG ')
 9021   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPROEY(IHARG,IARGT,ARG,NUMARG,
     1X3DEYE,Y3DEYE,Z3DEYE,
     1X3DMID,Y3DMID,Z3DMID,
     1AEYEXC,AEYEYC,AEYEZC,
     1IFOUND,IERROR)
C
C     PURPOSE--ROTATE THE CURRENT EYE COORDINATES
C              LEFT, RIGHT, UP, DOWN, XY, XZ, OR YZ
C              DEFAULT DIRECTION = LEFT
C              DEFAULT ANGLE     = 10 DEGREES
C     COMMAND EXAMPLE = ROTATE EYE LEFT 45
C
C     0 ARGUMENT CASE
C        ROTATE ==> ROTATE EYE LEFT 10
C     1 ARGUMENT CASE
C        ROTATE 17    ==> ROTATE EYE LEFT 17
C        ROTATE EYE   ==> ROTATE EYE LEFT 10
C        ROTATE LEFT  ==> ROTATE EYE LEFT 10
C        ROTATE RIGHT ==> ROTATE EYE RIGHT 10
C        ROTATE UP    ==> ROTATE EYE UP 10
C        ROTATE DOWN  ==> ROTATE EYE DOWN 10
C        ROTATE XY    ==> ROTATE EYE XY   10
C        ROTATE XZ    ==> ROTATE EYE XZ   10
C        ROTATE YZ    ==> ROTATE EYE YZ   10
C     2 ARGUMENT CASE
C        ROTATE EYE 17   ==> ROTATE EYE LEFT 17
C        ROTATE LEFT  17 ==> ROTATE EYE LEFT 17
C        ROTATE RIGHT 17 ==> ROTATE EYE RIGHT 17
C        ROTATE UP    17 ==> ROTATE EYE UP 17
C        ROTATE DOWN  17 ==> ROTATE EYE DOWN 17
C        ROTATE XY    17 ==> ROTATE EYE XY 17
C        ROTATE XZ    17 ==> ROTATE EYE XZ 17
C        ROTATE YZ    17 ==> ROTATE EYE YZ 17
C        ROTATE EYE LEFT  ==> ROTATE EYE LEFT 10
C        ROTATE EYE RIGHT ==> ROTATE EYE LEFT 10
C        ROTATE EYE UP    ==> ROTATE EYE UP    10
C        ROTATE EYE DOWN  ==> ROTATE EYE DOWN  10
C        ROTATE EYE XY    ==> ROTATE EYE XY    10
C        ROTATE EYE XZ    ==> ROTATE EYE XZ    10
C        ROTATE EYE YZ    ==> ROTATE EYE YZ    10
C     3 ARGUMENT CASE
C        ROTATE EYE LEFT  17
C        ROTATE EYE RIGHT 17
C        ROTATE EYE UP    17
C        ROTATE EYE DOWN  17
C        ROTATE EYE XY    17
C        ROTATE EYE XZ    17
C        ROTATE EYE YZ    17
C
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --ARG    (A  FLOATING POINT VECTOR)
C                     --NUMARG
C                     --X3DEYE  = X-COORDINATE OF EYE
C                     --Y3DEYE  = Y-COORDINATE OF EYE
C                     --Z3DEYE  = Z-COORDINATE OF EYE
C                     --X3DMID  = X-COORDINATE OF MID-FIGURE
C                     --Y3DMID  = Y-COORDINATE OF MID-FIGURE
C                     --Z3DMID  = Z-COORDINATE OF MID-FIGURE
C     OUTPUT ARGUMENTS--AEYEXC  = X-COORDINATE OF EYE (POST-ROTAT.)
C                     --AEYEYC  = Y-COORDINATE OF EYE (POST-ROTAT.)
C                     --AEYEZC  = Z-COORDINATE OF EYE (POST-ROTAT.)
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--93/10
C     ORIGINAL VERSION--SEPTEMBER  1993.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IARGT
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
CCCCC OCTOBER 1993.  ADD FOLLOWING LINE
      CHARACTER*4 IDIR
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
      DIMENSION IARGT(*)
      DIMENSION ARG(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
      ANGDEF=10.0
C
C               ********************************************
C               **  STEP 1--                              **
C               **  BRANCH ACCORDING TO THE CASE          **
C               ********************************************
C
      IF(NUMARG.EQ.0)THEN
         ANGLE=ANGDEF
         IDIR='LEFT'
         GOTO1000
      ENDIF
C
      IF(NUMARG.GE.1)THEN
         IF(IHARG(NUMARG).EQ.'?')GOTO2000
      ENDIF
C
      IF(NUMARG.EQ.1)THEN
         IF(IARGT(1).EQ.'NUMB')THEN
            ANGLE=ARG(1)
            IDIR='LEFT'
            GOTO1000
         ELSE
            ANGLE=ANGDEF
            IDIR='LEFT'
            IF(IHARG(1).EQ.'EYE ')IDIR='LEFT'
            IF(IHARG(1).EQ.'LEFT')IDIR='LEFT'
            IF(IHARG(1).EQ.'RIGH')IDIR='RIGH'
            IF(IHARG(1).EQ.'UP  ')IDIR='UP  '
            IF(IHARG(1).EQ.'DOWN')IDIR='DOWN'
            IF(IHARG(1).EQ.'XY  ')IDIR='XY  '
            IF(IHARG(1).EQ.'YX  ')IDIR='XY  '
            IF(IHARG(1).EQ.'XZ  ')IDIR='XZ  '
            IF(IHARG(1).EQ.'ZX  ')IDIR='XZ  '
            IF(IHARG(1).EQ.'YZ  ')IDIR='YZ  '
            IF(IHARG(1).EQ.'ZY  ')IDIR='YZ  '
            GOTO1000
         ENDIF
      ENDIF
C
      IF(NUMARG.EQ.2)THEN
         IF(IARGT(2).EQ.'NUMB')THEN
            ANGLE=ARG(2)
            IDIR='LEFT'
            IF(IHARG(1).EQ.'EYE ')IDIR='LEFT'
            IF(IHARG(1).EQ.'LEFT')IDIR='LEFT'
            IF(IHARG(1).EQ.'RIGH')IDIR='RIGH'
            IF(IHARG(1).EQ.'UP  ')IDIR='UP  '
            IF(IHARG(1).EQ.'DOWN')IDIR='DOWN'
            IF(IHARG(1).EQ.'XY  ')IDIR='XY  '
            IF(IHARG(1).EQ.'YX  ')IDIR='XY  '
            IF(IHARG(1).EQ.'XZ  ')IDIR='XZ  '
            IF(IHARG(1).EQ.'ZX  ')IDIR='XZ  '
            IF(IHARG(1).EQ.'YZ  ')IDIR='YZ  '
            IF(IHARG(1).EQ.'ZY  ')IDIR='YZ  '
            GOTO1000
         ELSE
            ANGLE=ANGDEF
            IDIR='LEFT'
            IF(IHARG(2).EQ.'EYE ')IDIR='LEFT'
            IF(IHARG(2).EQ.'LEFT')IDIR='LEFT'
            IF(IHARG(2).EQ.'RIGH')IDIR='RIGH'
            IF(IHARG(2).EQ.'UP  ')IDIR='UP  '
            IF(IHARG(2).EQ.'DOWN')IDIR='DOWN'
            IF(IHARG(1).EQ.'XY  ')IDIR='XY  '
            IF(IHARG(1).EQ.'YX  ')IDIR='XY  '
            IF(IHARG(1).EQ.'XZ  ')IDIR='XZ  '
            IF(IHARG(1).EQ.'ZX  ')IDIR='XZ  '
            IF(IHARG(1).EQ.'YZ  ')IDIR='YZ  '
            IF(IHARG(1).EQ.'ZY  ')IDIR='YZ  '
            GOTO1000
         ENDIF
      ENDIF
C
      IF(NUMARG.EQ.3)THEN
         IF(IARGT(3).EQ.'NUMB')THEN
            ANGLE=ARG(3)
            IDIR='LEFT'
            IF(IHARG(2).EQ.'EYE ')IDIR='LEFT'
            IF(IHARG(2).EQ.'LEFT')IDIR='LEFT'
            IF(IHARG(2).EQ.'RIGH')IDIR='RIGH'
            IF(IHARG(2).EQ.'UP  ')IDIR='UP  '
            IF(IHARG(2).EQ.'DOWN')IDIR='DOWN'
            IF(IHARG(1).EQ.'XY  ')IDIR='XY  '
            IF(IHARG(1).EQ.'YX  ')IDIR='XY  '
            IF(IHARG(1).EQ.'XZ  ')IDIR='XZ  '
            IF(IHARG(1).EQ.'ZX  ')IDIR='XZ  '
            IF(IHARG(1).EQ.'YZ  ')IDIR='YZ  '
            IF(IHARG(1).EQ.'ZY  ')IDIR='YZ  '
            GOTO1000
         ELSE
            ANGLE=ANGDEF
            IDIR='LEFT'
            GOTO1000
         ENDIF
      ENDIF
C
      GOTO8000
C
C               ********************************************
C               **  STEP 11--                             **
C               **  DO THE ROTATION                       **
C               ********************************************
C
 1000 CONTINUE
      IFOUND='YES'
      THETA=(ANGLE/360.0)*2*3.14159
      X1=X3DEYE
      Y1=Y3DEYE
      Z1=Z3DEYE
      X2=X3DEYE-X3DMID
      Y2=Y3DEYE-Y3DMID
      Z2=Z3DEYE-Z3DMID
C
      IF(IDIR.EQ.'LEFT'.OR.IDIR.EQ.'RIGH')THEN
         IF(IDIR.EQ.'RIGH')THETA=(-THETA)
         X3=X2*COS(THETA)-Y2*SIN(THETA)
         Y3=X2*SIN(THETA)+Y2*COS(THETA)
         Z3=Z2
         GOTO1100
      ENDIF
C
      IF(IDIR.EQ.'UP'.OR.IDIR.EQ.'DOWN')THEN
         IF(IDIR.EQ.'DOWN')THETA=(-THETA)
CTODO    X3=X2*COS(A1)+Y2*COS(A2)+Z2*COS(A3) DPTR32, MATH DICT. 337
CTODO    Y3=X2*COS(B1)+Y2*COS(B2)+Z2*COS(B3)
CTODO    Z3=X2*COS(C1)+Y2*COS(C2)+Z2*COS(C3)
         GOTO1100
      ENDIF
C
      IF(IDIR.EQ.'XY  ')THEN
         THETA=(-THETA)
         X3=X2*COS(THETA)-Y2*SIN(THETA)
         Y3=X2*SIN(THETA)+Y2*COS(THETA)
         Z3=Z2
         GOTO1100
      ENDIF
C
      IF(IDIR.EQ.'XZ  ')THEN
         THETA=(-THETA)
         X3=X2*COS(THETA)-Z2*SIN(THETA)
         Y3=Y2
         Z3=X2*SIN(THETA)+Z2*COS(THETA)
         GOTO1100
      ENDIF
C
      IF(IDIR.EQ.'YZ  ')THEN
         THETA=(-THETA)
         X3=X2
         Y3=Z2*SIN(THETA)+Y2*COS(THETA)
         Z3=Z2*COS(THETA)-Y2*SIN(THETA)
         GOTO1100
      ENDIF
C
 1100 CONTINUE
      X4=X3+X3DMID
      Y4=Y3+Y3DMID
      Z4=Z3+Z3DMID
      AEYEXC=X4
      AEYEYC=Y4
      AEYEZC=Z4
      IF(IFEEDB.EQ.'ON')THEN
         WRITE(ICOUT,999)
  999    FORMAT(1X)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1111)
 1111    FORMAT('OLD & NEW (X,Y,Z) EYE COORDINATES--')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1121)X1,X4
 1121    FORMAT('    X = ',2F10.3)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1122)Y1,Y4
 1122    FORMAT('    Y = ',2F10.3)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1123)Z1,Z4
 1123    FORMAT('    Z = ',2F10.3)
         CALL DPWRST('XXX','BUG ')
      ENDIF
      GOTO9000
C
C               ********************************************
C               **  STEP 12--                             **
C               **  TREAT THE    ?    CASE--              **
C               **  DUMP OUT CURRENT AND DEFAULT VALUES.  **
C               ********************************************
C
 2000 CONTINUE
      IFOUND='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,2011)
 2011 FORMAT('THE CURRENT (X,Y,Z) EYE COORDINATES ARE')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,2021)X3DEYE
 2021 FORMAT('    X = ',E15.7)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,2022)Y3DEYE
 2022 FORMAT('    Y = ',E15.7)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,2023)Z3DEYE
 2023 FORMAT('    Z = ',E15.7)
      CALL DPWRST('XXX','WRIT')
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,2031)
 2031 FORMAT('THE DEFAULT ROTATION DIRECTION IS LEFT (= XY)')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,2032)
 2032 FORMAT('THE DEFAULT ROTATION ANGLE IS 10 DEGREES')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,2033)
 2033 FORMAT(' THEREFORE, ROTATE == ROTATE EYE LEFT 10')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,2041)
 2041 FORMAT('SYNTAX: ROTATE EYE <DIRECTION> <ANGLE>')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,2042)
 2042 FORMAT('<DIRECTION> = LEFT, RIGHT, UP, DOWN, XY, XZ, YZ')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,2043)
 2043 FORMAT('<ANGLE> = -360 TO +360 DEGREES')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,2044)
 2044 FORMAT('EXAMPLE--ROTATE EYE LEFT 60')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,2045)
 2045 FORMAT('EXAMPLE--ROTATE EYE YZ 45')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,2046)
 2046 FORMAT('EXAMPLE--ROTATE      (== ROTATE EYE LEFT 10)')
      CALL DPWRST('XXX','WRIT')
      GOTO9000
C
C               ********************************************
C               **  STEP 80--                             **
C               **  TREAT THE    ERROR    CASE            **
C               ********************************************
C
 8000 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,8011)
 8011 FORMAT('***** ERROR IN DPROEY--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8012)
 8012 FORMAT('      ILLEGAL SYNTAX FOR    ROTATE EYE    COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8013)
 8013 FORMAT('    SYNTAX: ROTATE EYE <DIRECTION> <ANGLE>')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8014)
 8014 FORMAT('    <DIRECTION> = LEFT, RIGHT, UP, DOWN, XY, XZ, YZ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8015)
 8015 FORMAT('    <ANGLE> = -360 TO +360 DEGREES')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8016)
 8016 FORMAT('    EXAMPLE--ROTATE EYE LEFT 60')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8017)
 8017 FORMAT('    EXAMPLE--ROTATE EYE YZ 45')
      CALL DPWRST('XXX','BUG ')
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
      SUBROUTINE DPROO2(MODEL,NUMCHA,PARAM,IPARN,IPARN2,NUMPV,
     1                  IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,
     1                  IVARN,IVARN2,NUMVAR,XMIN,XMAX,ROOTS2,NROOTS,
     1                  ROOTAC,IFLGFB,
     1                  IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,
     1                  NUMNAM,MAXNAM,IFTEXP,IFTORD,IFORSW,
     1                  PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,V,MAXN,
     1                  ISUBRO,IBUGA3,IBUGCO,IBUGEV,IERROR)
C
C     2015/09: ADD LINES TO ARGUMENT LIST FOR FUNCTION BLOCK
C              AUGMENTATION
C
C     PURPOSE--COMPUTE THE ROOTS OF A FUNCTION
C              THAT ARE KNOWN TO BE BETWEEN THE LIMITS
C              XMIN AND XMAX.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--NOVEMBER  1978.
C     UPDATED         --FEBRUARY  1981.
C     UPDATED         --JULY      1981.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --FEBRUARY  1994. ACTIVATE ROOT ACCURACY
C     UPDATED         --SEPTEMBER 2015. SUPPORT FOR "FUNCTION BLOCKS"
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
      CHARACTER*4 IFTEXP
      CHARACTER*4 IFTORD
      CHARACTER*4 IFORSW
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGCO
      CHARACTER*4 IBUGEV
      CHARACTER*4 IERROR
C
      CHARACTER*4 ILAB
      CHARACTER*4 IH
      CHARACTER*4 IH2
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IFOUND
C
C---------------------------------------------------------------------
C
      DIMENSION MODEL(*)
      DIMENSION PARAM(*)
      DIMENSION IPARN(*)
      DIMENSION IPARN2(*)
      DIMENSION IVARN(*)
      DIMENSION IVARN2(*)
      DIMENSION ROOTS2(*)
C
      DIMENSION ITYPEH(*)
      DIMENSION IW21HO(*)
      DIMENSION IW22HO(*)
      DIMENSION W2HOLD(*)
C
      DIMENSION PRED(*)
      DIMENSION RES(*)
      DIMENSION XPLOT(*)
      DIMENSION YPLOT(*)
      DIMENSION X2PLOT(*)
      DIMENSION TAGPLO(*)
      DIMENSION V(*)
C
      DIMENSION IN(*)
      DIMENSION IVALUE(*)
      DIMENSION VALUE(*)
C
      CHARACTER*4 IHNAME(*)
      CHARACTER*4 IHNAM2(*)
      CHARACTER*4 IUSE(*)
C
      DIMENSION ILOCV(10)
      DIMENSION ILAB(10)
C
C     2015/08: FUNCTION BLOCK
C
      INCLUDE 'DPCOFB.INC'
C
      CHARACTER*8 IFBNAM
      CHARACTER*8 IFBANS
C
      CHARACTER*4 IFEESV
      COMMON/IFEED/IFEESV
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPRO'
      ISUBN2='O2  '
      IERROR='NO'
      IFOUND='NO'
C
C     THE FOLLOWING ACCURACY SETTING WAS SWITCHED DUE TO FAILURE
C     TO CONVERGE FOR SOME FUNCTIONS ON 32-BIT VAX
C     (BUT DID CONVERGE ON 36-BIT UNIVAC)
CCCCC ROOTAC=0.0000001
CCCCC PASS ROOTAC AS ARGUMENT.  FEBRUARY 1994.
CCCCC ROOTAC=0.000001
      CUTOFF=0.001
      DIFF=(-999.)
      RATIO=(-999.)
      IPASS=2
      NROOTS=0
C
      J2=0
C
      X2=0.0
      X3MIN=0.0
      X3MAX=0.0
      CALC1=0.0
      RATIO=0.0
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ROO2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('AT THE BEGINNING OF DPROO2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,IBUGCO,IBUGEV,IANGLU
   52   FORMAT('IBUGA3,IBUGCO,IBUGEV,IANGLU = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)NUMCHA,NUMPV,NUMVAR,IFLGFB
   53   FORMAT('NUMCHA,NUMPV,NUMVAR,IFLGFB = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)(MODEL(J),J=1,MIN(100,NUMCHA))
   54   FORMAT('MODEL(I) = ',100A1)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,NUMPV
          WRITE(ICOUT,56)I,PARAM(I),IPARN(I),IPARN2(I)
   56     FORMAT('I,PARAM(I),IPARN(I),IPARN2(I) = ',I8,G15.7,A4,A4)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
        DO60I=1,NUMVAR
          WRITE(ICOUT,61)I,IVARN(I),IVARN2(I)
   61     FORMAT('I, IVARN(I),IVARN2(I) = ',I8,2X,A4,A4)
          CALL DPWRST('XXX','BUG ')
   60   CONTINUE
        WRITE(ICOUT,62)XMIN,XMAX
   62   FORMAT('XMIN, XMAX = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***************************************************
C               **  STEP 1--                                     **
C               **  DETERMINE THE LOCATIONS (IN THE LIST IPARN)  **
C               **  OF THE VARIABLES OF THE FUNCTION.            **
C               ***************************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ROO2')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IFBNAM=' '
      IFBANS=' '
C
      IF(IFLGFB.LE.0)THEN
        DO100I=1,NUMVAR
          IH=IVARN(I)
          IH2=IVARN2(I)
          DO200J=1,NUMPV
           J2=J
           IF(IPARN(J).EQ.IH.AND.IPARN2(J).EQ.IH2)THEN
             ILOCV(I)=J2
             GOTO210
           ENDIF
  200     CONTINUE
  210     CONTINUE
  100   CONTINUE
      ELSE
        IF(IFLGFB.EQ.1)THEN
          IFBNAM=IFBNA1
          IFBANS=IFBAN1
          IH=IFBPL1(1)(1:4)
          IH2=IFBPL1(1)(5:8)
        ELSEIF(IFLGFB.EQ.2)THEN
          IFBNAM=IFBNA2
          IFBANS=IFBAN2
          IH=IFBPL2(1)(1:4)
          IH2=IFBPL2(1)(5:8)
        ELSEIF(IFLGFB.EQ.3)THEN
          IFBNAM=IFBNA3
          IFBANS=IFBAN3
          IH=IFBPL3(1)(1:4)
          IH2=IFBPL3(1)(5:8)
        ENDIF
      ENDIF
C
C               *************************************************
C               **  STEP 2--                                   **
C               **  WRITE OUT PRELIMINARY SUMMARY INFORMATION  **
C               *************************************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ROO2')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPRINT.EQ.'ON' .AND. IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,401)
  401   FORMAT('ROOTS OF AN EQUATION')
        CALL DPWRST('XXX','BUG ')
        IF(IFLGFB.LE.0)THEN
          ILAB(1)='    '
          ILAB(2)='  FU'
          ILAB(3)='NCTI'
          ILAB(4)='ON--'
          NUMWDL=4
          CALL DPPRIF(ILAB,NUMWDL,MODEL,NUMCHA,IBUGA3)
        ENDIF
C
        WRITE(ICOUT,402)IVARN(1),IVARN2(1)
  402   FORMAT('      ROOT VARIABLE                     = ',A4,A4)
        CALL DPWRST('XXX','BUG ')
C
        WRITE(ICOUT,403)XMIN
  403   FORMAT('      SPECIFIED LOWER LIMIT OF INTERVAL = ',F20.10)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,404)XMAX
  404   FORMAT('      SPECIFIED UPPER LIMIT OF INTERVAL = ',F20.10)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      NUMSEG=100
      NUMPT=NUMSEG+1
      ANUMPT=NUMPT
C
C               *******************************************************
C               **  STEP 3--                                         **
C               **  PARTITION THE INTERVAL FROM XMIN TO XMAX INTO    **
C               **     NUMSEG      EQUALLY-SPACED SEGMENTS.  STEP    **
C               **  THROUGH EACH OF THE      NUMSEG + 1      POINTS  **
C               **  WHICH DEFINE THE SEGMENTS--                      **
C               **  ALL THE WHILE LOOKING FOR FUNCTION CROSS-OVERS.  **
C               *******************************************************
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ROO2')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO1000I=1,NUMPT
        AI=I
        P=(AI-1.0)/(ANUMPT-1.0)
        X2=(1.0-P)*XMIN+P*XMAX
        X3MAX=X2
C
        IF(IFLGFB.LE.0)THEN
          DO1100K=1,NUMVAR
            JLOC=ILOCV(K)
            PARAM(JLOC)=X2
 1100     CONTINUE
          CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM,IPARN,IPARN2,NUMPV,
     1                IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,CALC2,
     1                IBUGCO,IBUGEV,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
        ELSE
C
C         FUNCTION BLOCK CASE:
C
C            STEP 1: COMPUTE FUNCTION BLOCK (BUT FIRST SET CURRENT
C                    VALUE OF DESIRED PARAMETER)
C
          DO1105II=1,NUMNAM
            IF(IH.EQ.IHNAME(II) .AND. IH2.EQ.IHNAM2(II) .AND.
     1         IUSE(II).EQ.'P')THEN
              VALUE(II)=X2
              IVALUE(II)=INT(X2+0.5)
              GOTO1109
            ENDIF
 1105     CONTINUE
C
C         PARAMETER NAME NOT FOUND IN CURRENT LIST, SO NEED TO ADD
C         TO NAME LIST
C
          IF(NUMNAM.LT.MAXNAM)THEN
            NUMNAM=NUMNAM+1
            IHNAME(NUMNAM)=IH
            IHNAM2(NUMNAM)=IH2
            IUSE(NUMNAM)='P'
            VALUE(NUMNAM)=X2
            IVALUE(NUMNAM)=INT(X2+ 0.5)
          ELSE
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1361)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1107)
 1107       FORMAT('      THE MAXIMUM NUMBER OF NAMES EXCEEDED.')
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
 1109     CONTINUE
C
          IFEEDB='OFF'
          CALL DPFBEX(IFBNAM,IANGLU,ISEED,IFTEXP,IFTORD,IFORSW,
     1                IBUGA3,IBUGA3,IBUGCO,IBUGEV,IBUGEV,
     1                ISUBRO,IFOUND,IERROR)
          IFEEDB=IFEESV
C
C            STEP 2: RETRIEVE RESPONSE
C
          DO1120II=1,NUMNAM
            IF(IFBANS(1:4).EQ.IHNAME(II) .AND.
     1         IFBANS(5:8).EQ.IHNAM2(II))THEN
              IF(IUSE(II).EQ.'P')THEN
                CALC2=VALUE(II)
                GOTO1129
              ELSEIF(IUSE(II).EQ.'V')THEN
                ICOLR=IVALUE(II)
                IJ=MAXN*(ICOLR-1)+1
                IF(ICOLR.LE.MAXCOL)CALC2=V(IJ)
                IF(ICOLR.EQ.MAXCP1)CALC2=PRED(1)
                IF(ICOLR.EQ.MAXCP2)CALC2=RES(1)
                IF(ICOLR.EQ.MAXCP3)CALC2=YPLOT(1)
                IF(ICOLR.EQ.MAXCP4)CALC2=XPLOT(1)
                IF(ICOLR.EQ.MAXCP5)CALC2=X2PLOT(1)
                IF(ICOLR.EQ.MAXCP6)CALC2=TAGPLO(1)
                GOTO1129
              ENDIF
            ENDIF
 1120     CONTINUE
C
C         PARAMETER/VARIABLE NAME NOT FOUND
C
          WRITE(ICOUT,1361)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1121)
 1121     FORMAT('      EXPECTED PARAMETER/VARIABLE NOT FOUND IN NAME ',
     1           'TABLE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1123)IFBANS
 1123     FORMAT('      EXPECTED NAME = ',A8)
          CALL DPWRST('XXX','BUG ')
C
 1129     CONTINUE
C
        ENDIF
C
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ROO2')THEN
          WRITE(ICOUT,1302)X2,CALC2
 1302     FORMAT('X2,CALC2 = ',2G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(CALC2.EQ.0.0)THEN
          NROOTS=NROOTS+1
          ROOTS2(NROOTS)=X2
        ENDIF
C
        IF(I.EQ.1)GOTO1390
C
        IF(CALC1.LT.0.0.AND.CALC2.GT.0.0)GOTO1350
        IF(CALC1.GT.0.0.AND.CALC2.LT.0.0)GOTO1350
        GOTO1390
C
 1350   CONTINUE
C
C       THE FOLLOWING LINE WAS MOVED 25 LINES UP
C       (MODIFICATION SUGGESTED BY TED PRINCE, NBS)
CCCCC   X3MAX=X2
C
C               ********************************************************
C               **  STEP 4--                                          **
C               **  PERFORM THE FOLLOWING SUB-SECTION OF CODE ONLY    **
C               **  WHEN A CROSS-OVER HAS BEEN FOUND WHILE STEPPING   **
C               **  THROUGH THE   NUMSEG + 1    POINTS IN THE INTERVAL.*
C               **  THE PURPOSE OF THE FOLLOWING SUB-SECTION OF CODE  **
C               **  IS TO DETERMINE MORE PRECISELY THE ROOT           **
C               **  WHEN A CROSS-OVER HAS BEEN DETECTED.              **
C               ********************************************************
C
        ICOUMX=1000
        ICOUNT=0
 1360   CONTINUE
        ICOUNT=ICOUNT+1
        IF(ICOUNT.GT.ICOUMX)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1321)
 1321     FORMAT('***** CAUTION FROM DPROO2--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1322)
 1322     FORMAT('      THE NUMBER OF INTERATIONS IN THE ROOT-FINDING')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1324)ICOUMX
 1324     FORMAT('      PROCESS HAS JUST EXCEEDED ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1325)X3
 1325     FORMAT('            ROOT = ',E15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1326)ROOTAC
 1326     FORMAT('            DESIRED ACCURACY   = ',E15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1327)DIFF
 1327     FORMAT('            ACTUAL DELTA X     = ',E15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1328)RATIO
 1328     FORMAT('            ACTUAL DELTA X / X = ',E15.7)
          CALL DPWRST('XXX','BUG ')
          GOTO1370
        ENDIF
C
        X3=(X3MIN+X3MAX)/2.0
C
C
        IF(IFLGFB.LE.0)THEN
          DO3100K=1,NUMVAR
            JLOC=ILOCV(K)
            PARAM(JLOC)=X3
 3100     CONTINUE
          CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM,IPARN,IPARN2,NUMPV,
     1                IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,CALC3,
     1                IBUGCO,IBUGEV,IERROR)
        ELSE
C
C         FUNCTION BLOCK CASE:
C
C            STEP 1: COMPUTE FUNCTION BLOCK (BUT FIRST SET CURRENT
C                    VALUE OF DESIRED PARAMETER)
C
          IFEEDB='OFF'
C
          DO3105II=1,NUMNAM
            IF(IH.EQ.IHNAME(II) .AND. IH2.EQ.IHNAM2(II) .AND.
     1         IUSE(II).EQ.'P')THEN
              VALUE(II)=X3
              IVALUE(II)=INT(X3+0.5)
              GOTO3109
            ENDIF
 3105     CONTINUE
C
C         PARAMETER NAME NOT FOUND IN CURRENT LIST, SO NEED TO ADD
C         TO NAME LIST
C
          IF(NUMNAM.LT.MAXNAM)THEN
            NUMNAM=NUMNAM+1
            IHNAME(NUMNAM)=IH
            IHNAM2(NUMNAM)=IH2
            IUSE(NUMNAM)='P'
            VALUE(NUMNAM)=X3
            IVALUE(NUMNAM)=INT(X3+ 0.5)
          ELSE
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1361)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1107)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
 3109     CONTINUE
C
          CALL DPFBEX(IFBNAM,IANGLU,ISEED,IFTEXP,IFTORD,IFORSW,
     1                IBUGA3,IBUGA3,IBUGCO,IBUGEV,IBUGEV,
     1                ISUBRO,IFOUND,IERROR)
C
C            STEP 2: RETRIEVE RESPONSE
C
          DO3120II=1,NUMNAM
            IF(IFBANS(1:4).EQ.IHNAME(II) .AND.
     1         IFBANS(5:8).EQ.IHNAM2(II))THEN
              IF(IUSE(II).EQ.'P')THEN
                CALC3=VALUE(II)
                GOTO3129
              ELSEIF(IUSE(II).EQ.'V')THEN
                ICOLR=IVALUE(II)
                IJ=MAXN*(ICOLR-1)+1
                IF(ICOLR.LE.MAXCOL)CALC3=V(IJ)
                IF(ICOLR.EQ.MAXCP1)CALC3=PRED(1)
                IF(ICOLR.EQ.MAXCP2)CALC3=RES(1)
                IF(ICOLR.EQ.MAXCP3)CALC3=YPLOT(1)
                IF(ICOLR.EQ.MAXCP4)CALC3=XPLOT(1)
                IF(ICOLR.EQ.MAXCP5)CALC3=X2PLOT(1)
                IF(ICOLR.EQ.MAXCP6)CALC3=TAGPLO(1)
                GOTO3129
              ENDIF
            ENDIF
 3120     CONTINUE
C
C         PARAMETER/VARIABLE NAME NOT FOUND
C
          WRITE(ICOUT,1361)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1121)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1123)IFBANS
          CALL DPWRST('XXX','BUG ')
C
 3129     CONTINUE
C
          IFEEDB=IFEESV
C
        ENDIF
C
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ROO2')THEN
          WRITE(ICOUT,1303)X3,CALC3
 1303     FORMAT('X3,CALC3 = ',2G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        PROD1=CALC1*CALC3
        PROD2=CALC2*CALC3
        IF(PROD1.GT.0.0)X3MIN=X3
        IF(PROD2.GT.0.0)X3MAX=X3
C
        ABSX3=ABS(X3)
        DIFF=ABS(X3MAX-X3MIN)
        IF(ABSX3.LE.CUTOFF.AND.DIFF.LE.ROOTAC)GOTO1370
        IF(ABSX3.LE.CUTOFF.AND.DIFF.GT.ROOTAC)GOTO1340
        RATIO=ABS(DIFF/X3)
        IF(ABSX3.GT.CUTOFF.AND.RATIO.LE.ROOTAC)GOTO1370
        IF(ABSX3.GT.CUTOFF.AND.RATIO.GT.ROOTAC)GOTO1340
 1340   CONTINUE
C
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ROO2')THEN
          WRITE(ICOUT,3145)CUTOFF,ROOTAC,DIFF,RATIO,ABSX3
 3145     FORMAT('CUTOFF,ROOTAC,DIFF,RATIO,ABSX3 = ',5E15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(PROD2.EQ.0.0)GOTO1370
        IF(PROD1.GT.0.0.OR.PROD2.GT.0.0)GOTO1360
C
 1365   CONTINUE
        WRITE(ICOUT,1361)
 1361   FORMAT('***** ERROR IN ROOTS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1362)
 1362   FORMAT('      IMPOSSIBLE CONDITION ARISING: PROD1 OR PROD2 ',
     1         'NOT EQUAL ZERO')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1363)PROD1,PROD2,X3MIN,X3,X3MAX,CALC1,CALC3,CALC2
 1363   FORMAT('PROD1,PROD2,X3MIN,X3,X3MAX,CALC1,CALC3,CALC2 = ',
     1         8E10.3)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
C
 1370   CONTINUE
        NROOTS=NROOTS+1
        ROOTS2(NROOTS)=X3
        GOTO1390
C
 1390   CONTINUE
        X3MIN=X3MAX
        CALC1=CALC2
C
 1000 CONTINUE
C
C               ***************************
C               **  STEP 5--             **
C               **  WRITE OUT THE ROOTS  **
C               ***************************
C
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ROO2')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPRINT.EQ.'ON' .AND. IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1405)NROOTS
 1405   FORMAT('      NUMBER OF ROOTS FOUND IN INTERVAL = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IF(NROOTS.GT.0)THEN
          DO1410I=1,NROOTS
            WRITE(ICOUT,1411)I,ROOTS2(I)
 1411       FORMAT('ROOT ',I5,' = ',G15.7)
            CALL DPWRST('XXX','BUG ')
 1410     CONTINUE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ROO2')THEN
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END      OF DPROO2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR,NROOTS,NUMVAR,NUMSEG
 9012   FORMAT('IERROR,NROOTS,NUMVAR,NUMSEG = ',A4,2X,3I8)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,NROOTS
          WRITE(ICOUT,9016)I,ROOTS2(I)
 9016     FORMAT('I,ROOTS2(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
        WRITE(ICOUT,9023)CALC1,CALC2,CALC3
 9023   FORMAT('CALC1,CALC2,CALC3 = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9024)X2,X3MIN,X3,X3MAX
 9024   FORMAT('X2,X3MIN,X3,X3MAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPROOT(ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,
     1                  IA,PARAM,IPARN,IPARN2,
     1                  ROOTAC,IFTEXP,IFTORD,IFORSW,IANGLU,
     1                  ISUBRO,IBUGA3,IBUGCO,IBUGEV,IBUGQ,IERROR)
C
C     PURPOSE--TREAT THE LET CASE FOR
C              FINDING THE ROOTS OF AN EQUATION.
C     EXAMPLE--LET X = ROOTS X**3+2*X**2-4*X+5 FOR X = -100 200
C            --LET X = F1 FOR X = 0 B
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--JANUARY   1979.
C     UPDATED--       --FEBRUARY  1979.
C     UPDATED         --MARCH     1979.
C     UPDATED         --JULY      1981.
C     UPDATED         --SEPTEMBER 1981.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --FEBRUARY  1994. ACTIVATE ROOT ACCURACY
C     UPDATED         --SPETEMBER 2015. SUPPORT FUNCTION BLOCK
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ITYPEH
      CHARACTER*4 IW21HO
      CHARACTER*4 IW22HO
      CHARACTER*4 IA
      CHARACTER*4 IPARN
      CHARACTER*4 IPARN2
      CHARACTER*4 IFTEXP
      CHARACTER*4 IFTORD
      CHARACTER*4 IFORSW
      CHARACTER*4 IANGLU
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGCO
      CHARACTER*4 IBUGEV
      CHARACTER*4 IBUGQ
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
      INCLUDE 'DPCOFB.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
C               *******************************
C               **  TREAT THE ROOTS SUBCASE  **
C               **  OF THE LET COMMAND       **
C               *******************************
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ROOT')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPROOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGA2,IBUGCO,IBUGEV,IBUGQ
   53   FORMAT('IBUGA3,IBUGCO,IBUGEV,IBUGQ = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               **********************************
C               **  STEP 1--                    **
C               **  INITIALIZE SOME VARIABLES.  **
C               **********************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ROOT')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ISUBN1='DPRO'
      ISUBN2='OT  '
      IFOUND='NO'
      IERROR='NO'
      NEWNAM='NO'
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
      ILOCMX=0
      NUMLIM=0
      ILOC3=0
      MAXN2=MAXCHF
      MAXN3=MAXCHF
C
C               *******************************************************
C               **  STEP 2--                                         **
C               **  EXAMINE THE LEFT-HAND SIDE--                     **
C               **  IS THE VARIABLE NAME TO LEFT OF = SIGN           **
C               **  ALREADY IN THE NAME LIST?                        **
C               **  NOTE THAT     ILISTL    IS THE LINE IN THE TABLE **
C               **  OF THE NAME ON THE LEFT.                         **
C               *******************************************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ROOT')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
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
C
      NEWNAM='YES'
      ILISTL=NUMNAM+1
      IF(ILISTL.GT.MAXNAM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2201)
 2201   FORMAT('***** ERROR IN LET ... ROOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2202)
 2202   FORMAT('      THE NUMBER OF VARIABLE, PARAMETER, AND FUNCTION')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2203)MAXNAM
 2203   FORMAT('      NAMES HAS JUST EXCEEDED THE ALLOWABLE ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2204)
 2204   FORMAT('      ENTER      STAT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2205)
 2205   FORMAT('      TO FIND OUT THE FULL LIST OF USED NAMES, AND ',
     1         'THEN')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2206)
 2206   FORMAT('      REDEFINE (REUSE) SOME OF THE ALREADY-USED NAMES')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
 2900 CONTINUE
C
C               *******************************************************
C               **  STEP 3.1--                                       **
C               **  EXTRACT THE RIGHT-SIDE FUNCTIONAL EXPRESSION     **
C               **  FROM THE INPUT COMMAND LINE (STARTING WITH THE   **
C               **  FIRST NON-BLANK LOCATION AFTER THE EQUAL SIGN    **
C               **  AND ENDING WITH THE END OF THE LINE OR WITH THE  **
C               **  LAST NON-BLANK CHARACTER BEFORE     WRT  .       **
C               **  PLACE THE FUNCTION IN IFUNC2(.)  .               **
C               *******************************************************
C
      ISTEPN='3.1'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ROOT')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
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
 3102 FORMAT('      INVALID COMMAND FORM FOR ROOT-FINDING.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3103)
 3103 FORMAT('      GENERAL FORM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3104)
 3104 FORMAT('      LET ... = ROOTS ... WRT  ... ',
     1       'FOR ... = ... TO ...')
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ROOT')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO3600I=1,N2
        I2=I
        IF(IFUNC2(I).EQ.'=')THEN
          ILOCE2=I2
          IMIN=ILOCE2+1
          IF(IMIN.LE.N2)THEN
            DO3650II=IMIN,N2
              IREV=N2-II+IMIN
              IREVP1=IREV+1
              IFUNC2(IREVP1)=IFUNC2(IREV)
 3650       CONTINUE
            J=ILOCE2
            IFUNC2(J)='-'
            J=ILOCE2+1
            IFUNC2(J)='('
            J=N2+2
            IFUNC2(J)=')'
            N2=J
          ENDIF
          GOTO3900
        ENDIF
 3600 CONTINUE
C
 3900 CONTINUE
C
C               ********************************************************
C               **  STEP 4--                                          **
C               **  DETERMINE IF THE EXPRESSION HAS ANY FUNCTION NAMES**
C               **  INBEDDED.  IF SO, REPLACE THE FUNCTION NAMES BY   **
C               **  EACH FUNCTION'S DEFINITION.  DO SO REPEATEDLY     **
C               **  UNTIL ALL FUNCTION REFERENCES HAVE BEEN ANNIHILATED*
C               **  AND THE EXPRESSION IS LEFT ONLY WITH CONSTANTS,   **
C               **  PARAMETERS, AND VARIABLES--NO FUNCTIONS.  PLACE   **
C               **  THE RESULTING FUNCTIONAL EXPRESSION INTO IFUNC3(.)**
C               ********************************************************
C
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ROOT')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IFLGFB.LE.0)THEN
        CALL DPEXFU(IFUNC2,N2,IHNAME,IHNAM2,IUSE,IVSTAR,IVSTOP,
     1              NUMNAM,IANS,IWIDTH,IFUNC,NUMCHF,MAXCHF,IFUNC3,
     1              N3,MAXN3,
     1              IBUGA3,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
        IF(IBUGA3.EQ.'ON')THEN
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
          WRITE(ICOUT,5081)IDUMV(1),IDUMV2(1)
 5081     FORMAT('ROOT VARIABLE         = ',A4,A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
      ENDIF
C
C               *************************************
C               **  STEP 5--                       **
C               **  EXTRACT QUALIFIER INFORMATION. **
C               *************************************
C
C               **************************************************
C               **  STEP 5.1--                                  **
C               **  DETERMINE THE DUMMY VARIABLE FOR THE ROOT.  **
C               **************************************************
C
      ISTEPN='5.1'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ROOT')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
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
      IF(IFOUN1.EQ.'YES'.AND.IFOUN2.EQ.'YES')THEN
        IDUMV(1)=IHOUT
        IDUMV2(1)=IHOUT2
        NUMDV=1
        GOTO5190
      ENDIF
C
      IKEY='FOR '
      IKEY2='    '
      ISHIFT=1
      ILOCA=1
      ILOCB=NUMARG
      INCLUN='NO'
      CALL DPEXQU(IKEY,IKEY2,ISHIFT,ILOCA,ILOCB,IHARG,IHARG2,NUMARG,
     1            INCLUN,IANS,IWIDTH,IHNAME,IHNAM2,IVALUE,VALUE,
     1            IUSE,IN,NUMNAM,
     1            IFOUN1,IFOUN2,ILOC1,ILOC2,IHOUT,IHOUT2,ILOUT,
     1            IVOUT,VOUT,IUOUT,
     1            INOUT,IBUGA3,IERROR)
      IF(IFOUN1.EQ.'YES'.AND.IFOUN2.EQ.'YES')THEN
        IDUMV(1)=IHOUT
        IDUMV2(1)=IHOUT2
        NUMDV=1
        GOTO5190
      ENDIF
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2201)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5182)
 5182 FORMAT('      INVALID COMMAND FORM FOR ROOT-FINDING.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,5183)
 5183 FORMAT('      NO VARIABLE FOR ROOT-FINDING DEFINED.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3102)
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
C               **  DETERMINE THE LIMITS FOR   THE ROOTS.       **
C               **************************************************
C
      ISTEPN='5.2'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ROOT')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
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
     1            INCLUN,IANS,IWIDTH,IHNAME,IHNAM2,IVALUE,VALUE,
     1            IUSE,IN,NUMNAM,
     1            IFOUN1,IFOUN2,ILOC1,ILOC2,IHOUT,IHOUT2,ILOUT,
     1            IVOUT,VOUT,IUOUT,
     1            INOUT,IBUGA3,IERROR)
      IF(IFOUN1.EQ.'YES'.AND.IFOUN2.EQ.'YES')THEN
        XMIN=VOUT
        NUMLIM=NUMLIM+1
      ENDIF
C
      IKEY='FOR '
      IKEY2='    '
      ISHIFT=4
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
     1            INCLUN,IANS,IWIDTH,IHNAME,IHNAM2,IVALUE,VALUE,IUSE,
     1            IN,NUMNAM,
     1            IFOUN1,IFOUN2,ILOC1,ILOC2,IHOUT,IHOUT2,ILOUT,IVOUT,
     1            VOUT,IUOUT,
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
        WRITE(ICOUT,5182)
        CALL DPWRST('XXX','BUG ')
        IF(NUMLIM.EQ.0)THEN
          WRITE(ICOUT,5283)
 5283     FORMAT('      NO LIMITS FOR ROOT-FINDING DEFINED.')
        ELSEIF(NUMLIM.EQ.1)THEN
          WRITE(ICOUT,5284)
 5284     FORMAT('      ONLY ONE LIMIT FOR ROOT-FINDING DEFINED.')
        ELSE
          WRITE(ICOUT,5285)NUMLIM
 5285     FORMAT('      NUMBER OF LIMITS DEFINED = ',I8)
        ENDIF
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3102)
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
C
C               **********************************************
C               **  STEP 6.3--                              **
C               **  SCAN THE QUALIFIERS FOR VARIABLE,       **
C               **  PARAMETER, FUNCTION, AND VALUE CHANGES  **
C               **  IN THE FUNCTION.                        **
C               **********************************************
C
      ISTEPN='6.3'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ROOT')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NCHANG=0
      DO6300IFORI=1,10
C
        IKEY='FOR '
        IKEY2='    '
        ISHIFT=1
        IF(IFORI.EQ.1)ILOCA=ILOCMX
        IF(IFORI.NE.1)ILOCA=ILOC3
        ILOCB=NUMARG
        INCLUN='NO'
        CALL DPEXQU(IKEY,IKEY2,ISHIFT,ILOCA,ILOCB,
     1              IHARG,IHARG2,NUMARG,
     1              INCLUN,IANS,IWIDTH,IHNAME,IHNAM2,IVALUE,
     1              VALUE,IUSE,IN,NUMNAM,
     1              IFOUN1,IFOUN2,ILOC1,ILOC2,IHOUT,IHOUT2,ILOUT,
     1              IVOUT,VOUT,IUOUT,
     1              INOUT,IBUGA3,IERROR)
        IF(IERROR.EQ.'YES')GOTO6380
        IF(IFOUN1.EQ.'NO'.OR.IFOUN2.EQ.'NO')GOTO6390
C
        ILOC3=ILOC2+2
        IF(ILOC3.GT.NUMARG)GOTO6380
        NCHANG=NCHANG+1
        IOLD(NCHANG)=IHARG(ILOC2)
        IOLD2(NCHANG)=IHARG2(ILOC2)
        INEW(NCHANG)=IHARG(ILOC3)
        INEW2(NCHANG)=IHARG2(ILOC3)
C
 6300 CONTINUE
      GOTO6390
C
 6380 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2201)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3102)
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ROOT')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPRINT.EQ.'ON' .AND. IFEEDB.EQ.'ON' .AND. NCHANG.GT.0 .AND.
     1   IFLGFB.LE.0)THEN
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
C               ********************************************************
C               **  STEP 6.7--                                        **
C               **  MAKE A NON-CALCULATING PASS AT THE FUNCTION SO AS **
C               **  TO EXTRACT ALL PARAMETER AND VARIABLE NAMES.      **
C               ********************************************************
C
      ISTEPN='6.8'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ROOT')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IPASS=1
      IF(IFLGFB.LE.0)THEN
        CALL COMPIM(IFUNC3,N3,IPASS,PARAM,IPARN,IPARN2,NUMPV,
     1              IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,AJUNK,
     1              IBUGCO,IBUGEV,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
      ELSE
        GOTO7701
      ENDIF
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ROOT')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IP=0
      IV=0
      IF(NUMPV.GT.0)THEN
        DO7600J=1,NUMPV
          IHPARN=IPARN(J)
          IHPAR2=IPARN2(J)
          IF(IHPARN.EQ.IDUMV(1).AND.IHPAR2.EQ.IDUMV2(1))THEN
            IV=IV+1
            LOCDUM=J
          ELSE
            IHWUSE='P'
            MESSAG='YES'
            CALL CHECKN(IHPARN,IHPAR2,IHWUSE,
     1                  IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,
     1                  NUMNAM,MAXNAM,
     1                  ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
            IF(IERRO2.EQ.'YES')THEN
              IP=IP+1
              PARAM(J)=0.0
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,7606)IHPARN,IHPAR2
 7606         FORMAT('NOTE--',A4,A4,' HAS BEEN TEMPORARILY SET TO ZERO')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,7607)
 7607         FORMAT('             FOR THE ROOT-FINDING PROCESS.')
              CALL DPWRST('XXX','BUG ')
            ENDIF
            IP=IP+1
            PARAM(J)=VALUE(ILOCP)
          ENDIF
 7600   CONTINUE
      ENDIF
C
C               ******************************
C               **  STEP 8--                **
C               **  DETERMINE THE ROOTS  .  **
C               ******************************
C
 7701 CONTINUE
C
      ISTEPN='8'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ROOT')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7711)
 7711   FORMAT('***** FROM DPROOT, IMMEDIATELY BEFORE CALLING ',
     1         'ROOTS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7712)N3,NUMPV
 7712   FORMAT('N3,NUMPV = ',I8,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7713)NUMDV,XMIN,XMAX
 7713   FORMAT('NUMDV,XMIN,XMAX = ',I8,2G15.7)
        CALL DPWRST('XXX','BUG ')
        DO7714I=1,NUMDV
          WRITE(ICOUT,7715)I,IDUMV(I),IDUMV2(I)
 7715     FORMAT('I,IDUMV(I),IDUMV2(I) = ',I8,2X,A4,A4)
          CALL DPWRST('XXX','BUG ')
 7714   CONTINUE
      ENDIF
C
      CALL DPROO2(IFUNC3,N3,PARAM,IPARN,IPARN2,NUMPV,
     1            IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,
     1            IDUMV,IDUMV2,NUMDV,XMIN,XMAX,ROOTS2,NROOTS,
     1            ROOTAC,IFLGFB,
     1            IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,
     1            NUMNAM,MAXNAM,IFTEXP,IFTORD,IFORSW,
     1            PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,V,MAXN,
     1            ISUBRO,IBUGA3,IBUGCO,IBUGEV,IERROR)
      AROOTS=NROOTS
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ROOT')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IHL=IHLEFT
      IHL2=IHLEF2
      ICASUP='V'
      CALL DPINVP(IHL,IHL2,ICASUP,ROOTS2,NROOTS,AROOTS,NROOTS,
     1ISUBN1,ISUBN2,IBUGA3,IERROR)
C
      IHL='NROO'
      IHL2='TS  '
      ICASUP='P'
      CALL DPINVP(IHL,IHL2,ICASUP,ROOTS2,NROOTS,AROOTS,NROOTS,
     1ISUBN1,ISUBN2,IBUGA3,IERROR)
C
C               ****************
C               **  STEP 90-- **
C               **  EXIT      **
C               ****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ROOT')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DPROOT--')
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,NUMNAM
          WRITE(ICOUT,9016)I,IHNAME(I),IHNAM2(I),IUSE(I),
     1                     IVSTAR(I),IVSTOP(I)
 9016     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVSTAR(I),IVSTOP(I)=',
     1           I8,2X,A4,A4,2X,A4,I8,I8)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
        WRITE(ICOUT,9017)NUMCHF,MAXCHF,IWIDTH,N2,N3,NUMPV,IFLGFB
 9017   FORMAT('NUMCHF,MAXCHF,IWIDTH,N2,N3,NUMPV,IFLGFB = ',7I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9018)(IFUNC(I),I=1,MIN(115,IWIDTH))
 9018   FORMAT('IFUNC(.) = ',115A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9019)(IFUNC2(I),I=1,MIN(115,N2))
 9019   FORMAT('IFUNC2(.) = ',115A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)(IFUNC3(I),I=1,MIN(120,N3))
 9021   FORMAT('IFUNC3(.) = ',120A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9023)IHLEFT,IHLEF2
 9023   FORMAT('IHLEFT,IHLEF2 = ',A4,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9024)ICASUP,IFOUND,IERROR
 9024   FORMAT('ICASUP,IFOUND,IERROR = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9025)XMIN,XMAX,NROOTS
 9025   FORMAT('XMIN,XMAX,NROOTS = ',2G15.7,I8)
        CALL DPWRST('XXX','BUG ')
        DO9027I=1,NROOTS
          WRITE(ICOUT,9028)I,ROOTS2(I)
 9028     FORMAT('I,ROOTS2(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
 9027   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPROSE(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                  IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--GENERATE A ROSE PLOT (A VARIATION OF A ROSE PLOT):
C              ROSE PLOT Y
C              ROSE PLOT Y1 Y2
C     REFERENCE--WAINER (1997), "VISUAL REVELATIONS:  GRAPHICAL
C                TALES OF FATE AND DECEPTION FROM NAPOLEAN BONAPORTE
C                TO ROSS PEROT", COPERNICUS, CHAPTER 11.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-75-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2007/4
C     ORIGINAL VERSION--APRIL     2007.
C     UPDATED         --APRIL     2011. USE DPPARS AND DPPAR3
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
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 IDATSW
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      CHARACTER*4 ICASE
      PARAMETER (MAXSPN=10)
      CHARACTER*40 INAME
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
C
      DIMENSION Y1(MAXOBV)
      DIMENSION Y2(MAXOBV)
      DIMENSION X1(MAXOBV)
      DIMENSION XIDTEM(MAXOBV)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TEMP3(MAXOBV)
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),Y2(1))
      EQUIVALENCE (GARBAG(IGARB3),X1(1))
      EQUIVALENCE (GARBAG(IGARB4),XIDTEM(1))
      EQUIVALENCE (GARBAG(IGARB5),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB6),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB7),TEMP3(1))
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      ISUBN1='DPRO'
      ISUBN2='SE  '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
C               ***************************
C               **  TREAT THE ROSE PLOT  **
C               ***************************
C
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'ROSE')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPROSE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASPL,IAND1,IAND2
   52   FORMAT('ICASPL,IAND1,IAND2 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGG2,IBUGG3,IBUGQ,ISUBRO
   53   FORMAT('IBUGG2,IBUGG3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
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
     1   ICOM.EQ.'ROSE'.AND.IHARG(1).EQ.'PLOT')THEN
        ILASTC=1
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      ELSE
        GOTO9000
      ENDIF
C
      IFOUND='YES'
      ICASPL='PIEC'
C
C               ****************************************
C               **  STEP 2--                          **
C               **  EXTRACT THE VARIABLE LIST         **
C               ****************************************
C
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ROSE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='ROSE PLOT'
      MINNA=1
      MAXNA=100
      MINN2=2
      IFLAGE=1
      IFLAGM=0
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=1
      MAXNVA=2
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
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ROSE')THEN
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
C     EXTRACT THE VARIABLE.
C
      ICOL=1
      CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1            INAME,IVARN1,IVARN2,IVARTY,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1            MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1            MAXCP4,MAXCP5,MAXCP6,
     1            V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1            Y1,Y2,Y1,NLOCAL,NS,NS,ICASE,
     1            IBUGG3,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
C               *****************************************************
C               **  STEP 8--                                       **
C               **  FORM THE VERTICAL AND HORIZONTAL AXIS          **
C               **  VALUES Y(.) AND X(.) FOR THE PLOT.             **
C               **  RESET THE VECTOR D(.) TO ALL ONES.             **
C               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).  **
C               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).  **
C               *****************************************************
C
      CALL DPROS2(Y1,Y2,X1,NLOCAL,NUMVAR,
     1            XIDTEM,TEMP1,TEMP2,TEMP3,
     1            Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'ROSE')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPROSE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9013   FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',
     1         I8,I8,I8,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        IF(NPLOTP.GT.0)THEN
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
      SUBROUTINE DPROS2(Y1,Y2,X,N,NUMV2,
     1                  XIDTEM,XIDTE2,TEMP1,TEMP3,
     1                  YPLOT,XPLOT,D2,NPLOTP,NPLOTV,
     1                  IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
C              THAT WILL DEFINE A ROSE PLOT
C     REFERENCE--WAINER (1997), "VISUAL REVELATIONS:  GRAPHICAL
C                TALES OF FATE AND DECEPTION FROM NAPOLEAN BONAPORTE
C                TO ROSS PEROT", COPERNICUS, CHAPTER 11.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2007/4
C     ORIGINAL VERSION--APRIL     2007.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGG3
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IWRIT2
C
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION X(*)
      DIMENSION YPLOT(*)
      DIMENSION XPLOT(*)
      DIMENSION D2(*)
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP3(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----DATA STATEMENTS-------------------------------------------------
C
      DATA PI/3.1415926535878/
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPRO'
      ISUBN2='S2  '
C
      IERROR='NO'
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      IF(N.LE.1)THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
   31   FORMAT('***** ERROR IN ROSE PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,32)
   32   FORMAT('      THE NUMBER OF OBSERVATIONS MUST BE AT LEAST 2;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,34)N
   34   FORMAT('      THE ENTERED NUMBER OF OBSERVATIONS HERE = ',I6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'ROS2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,70)
   70   FORMAT('***** AT THE BEGINNING OF DPROS2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)NUMV2,N
   71   FORMAT('NUMV2,N = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO73I=1,MIN(N,100)
          WRITE(ICOUT,74)I,Y1(I),Y2(I)
   74     FORMAT('I, Y1(I),Y2(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
   73   CONTINUE
      ENDIF
C
C               *******************************************
C               **  STEP 4--                             **
C               **  DETERMINE PLOT COORDINATES           **
C               **  THREE CASES:                         **
C               **    1) ONE VARIABLE                    **
C               **    2) TWO VARIABLE - CROSS-TABULATE   **
C               **       (IN PARTICULAR 2X2 TABLES       **
C               **    3) THREE VARIABLE - CROSS-TABULATE **
C               **       FIRST TWO VARIABLES, THRID      **
C               **       VARIABLE IS A GROUP-ID VARIABLE **
C               **       (ONE ROSE PLOT WILL BE          **
C               **       GENERATED FOR EACH GROUP)       **
C               *******************************************
C
      IF(NUMV2.EQ.1)THEN
        GOTO1000
      ELSEIF(NUMV2.EQ.2)THEN
        GOTO2000
      ELSE
        GOTO9000
      ENDIF
C
C     THIS PLOT USES THE RELATIONSHIPS:
C
C          X = R*COS(THETA)
C          Y = R*SIN(THETA)
C
C     IN THE STANDARD PIE CHART, THE ANGLE IS PROPORTIONAL
C     TO THE DATA VALUE, WE CENTER THE CIRCLE AT (0,0) AND WE
C     SET R = 1.  FOR THE ROSE PLOT, THE ANGLES ARE CONSTANT
C     AND WE MAKE THE SQUARE ROOT OF THE RADIUS PROPORTIONAL
C     TO THE DATA VALUE (SCALE SO THAT THE LARGEST DATA VALUE
C     HAS R = 1).  THE ROSE PLOT WILL ALSO BE CENTERED AT (0,0).
C
 1000 CONTINUE
C
C     FOR THE SINGLE VARIABLE CASE, THE VALUES ARE INTERPRETED
C     AS PROPORTIONS OR COUNTS (I.E., THE VALUE DIVIDED BY THE
C     SUM OF THE VALUES GIVES THE PROPORTION FOR THAT GROUP).  SO
C     NEGATIVE VALUES ARE NOT ALLOWED.
C
      DO1010I=1,N
        IF(Y1(I).LT.0.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,31)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1012)
 1012     FORMAT('      A NEGATIVE PROPORTION/COUNT WAS ENCOUNTERED.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1014)I,Y1(I)
 1014     FORMAT('      ROW ',I8,' = ',G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
 1010 CONTINUE
C
      NUMCLA=N
      ANGINC=2.0*PI/REAL(NUMCLA)
C
      YMAX=Y1(1)
      DO1060J=1,NUMCLA
        YMAX=MAX(YMAX,Y1(J))
 1060 CONTINUE
C
      DO1070J=1,NUMCLA
        TEMP1(J)=SQRT(Y1(J)/YMAX)
 1070 CONTINUE
C
C     NOTE: SINCE A PRIMARY APPLICATION OF THIS PLOT IS TO
C           DISPLAY 2X2 TABLES, SCALE TO GO FROM -PI TO PI
C           RATHER THAN 0 TO 2*PI.
C
      K=0
      J2=0
      DO1120J=1,NUMCLA
C
        R=TEMP1(J)
        ANGSTA=PI - (J-1)*ANGINC
        ANGSTO=ANGSTA-ANGINC
C
        K=K+1
        J2=J2+1
C
        XPLOT(K)=0.0
        YPLOT(K)=0.0
        D2(K)=J2
C
        ANG=ANGSTA
        K=K+1
        XPLOT(K)=R*COS(ANG)
        YPLOT(K)=R*SIN(ANG)
        D2(K)=J2
C
 1125   CONTINUE
        ANG=ANG - 0.015
        IF(ANG.LT.ANGSTO)THEN
          K=K+1
          XPLOT(K)=R*COS(ANGSTO)
          YPLOT(K)=R*SIN(ANGSTO)
          D2(K)=J2
          K=K+1
          XPLOT(K)=0.0
          YPLOT(K)=0.0
          D2(K)=J2
          GOTO1120
        ELSE
          K=K+1
          XPLOT(K)=R*COS(ANG)
          YPLOT(K)=R*SIN(ANG)
          D2(K)=J2
        ENDIF
C
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'ROS2')THEN
          WRITE(ICOUT,1121)J,J2,K,ANGSTA,ANGSTO,ANG
 1121     FORMAT('J,J2,K,ANSTA,ANGSTO,ANG = ',3I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1123)R,XPLOT(K),YPLOT(K)
 1123     FORMAT('R,XPLOT(K),YPLOT(K) = ',3G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        GOTO1125
C
 1120 CONTINUE
C
      NPLOTP=K
      NPLOTV=3
      GOTO9000
C
 2000 CONTINUE
C
C     FOR THE TWO VARIABLE CASE, A CROSS-TABULATION IS PERFORMED.
C     THIS IS MOST TYPICALLY APPLIED FOR THE CASE OF 2X2 TABLES,
C     BUT THE CODE BELOW WILL IN FACT HANDLE RXC TABLES.  IF N = 2,
C     THEN ASSUME THAT DATA IS ENTERED AS A 2X2 TABLE:
C
C           TRUE POSITIVES     FALSE NEGATIVES
C           FALSE POSITIVES    TRUE NEGATIVES
C
      IF(N.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2101)
 2101   FORMAT('      THE NUMBER OF OBSERVATIONS FOR VARIABLE 1 ',
     1         'IS NON-POSITIVE')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2103)N1
 2103   FORMAT('SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(N.EQ.2)THEN
        X(1)=Y2(1)
        X(2)=Y2(2)
        X(3)=Y1(2)
        X(4)=Y1(1)
        NUMCLA=4
        GOTO3000
      ENDIF
C
C               ******************************************************
C               **  STEP 2.2--                                      **
C               **  DETERMINE THE NUMBER OF DISTINCT VALUES         **
C               **  FOR THE GROUP VARIABLES (Y1, Y2).               **
C               ******************************************************
C
      ISTEPN='22'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'ROS2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IWRITE='OFF'
      CALL DISTIN(Y1,N,IWRITE,XIDTEM,NUMSE1,IBUGG3,IERROR)
      CALL SORT(XIDTEM,NUMSE1,XIDTEM)
      CALL DISTIN(Y2,N,IWRITE,XIDTE2,NUMSE2,IBUGG3,IERROR)
      CALL SORT(XIDTE2,NUMSE2,XIDTE2)
C
      IF(NUMSE1.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2202)
 2202   FORMAT('      NUMBER OF SETS    NUMSE1 = 0 ')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(NUMSE2.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2204)
 2204   FORMAT('      NUMBER OF SETS    NUMSE2 = 0 ')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      AN=N
      ANUMS1=NUMSE1
      ANUMS2=NUMSE2
C
C     COMPUTE COUNTS FOR EACH CELL.  IF 2X2 TABLE DETECTED
C     WHERE DISTINCT VALUES ARE 1 AND 0, TREAT LIKE 2X2 TABLE
C     ABOVE.
C
      IF(NUMSE1.EQ.2 .AND. NUMSE2.EQ.2)THEN
        IF(XIDTEM(1).EQ.0.0 .AND. XIDTEM(2).EQ.1.0)THEN
          IF(XIDTE2(1).EQ.0.0 .AND. XIDTE2(2).EQ.1.0)THEN
            N11=0
            N12=0
            N21=0
            N22=0
            DO2260I=1,N
              IF(Y1(I).EQ.1.0 .AND. Y2(I).EQ.1.0)THEN
                N11=N11+1
              ELSEIF(Y1(I).EQ.1.0 .AND. Y2(I).EQ.0.0)THEN
                N12=N12+1
              ELSEIF(Y1(I).EQ.0.0 .AND. Y2(I).EQ.1.0)THEN
                N21=N21+1
              ELSEIF(Y1(I).EQ.0.0 .AND. Y2(I).EQ.0.0)THEN
                N22=N22+1
              ENDIF
 2260       CONTINUE
            X(1)=REAL(N11)
            X(2)=REAL(N12)
            X(3)=REAL(N21)
            X(4)=REAL(N22)
            NUMCLA=4
            GOTO3000
          ENDIF
        ENDIF
      ENDIF
C
      J=0
      DO2310ISET1=1,NUMSE1
        DO2320ISET2=1,NUMSE2
C
          K=0
          DO2330I=1,N
            IF(XIDTEM(ISET1).EQ.Y1(I).AND.XIDTE2(ISET2).EQ.Y2(I))THEN
              K=K+1
            ENDIF
 2330     CONTINUE
          J=J+1
          X(J)=REAL(K)
C
 2320   CONTINUE
 2310 CONTINUE
      NUMCLA=J
C
      GOTO3000
C
 3000 CONTINUE
C
      DO3010I=1,NUMCLA
        IF(X(I).LT.0.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,31)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3012)
 3012     FORMAT('      FOR THE TWO-VARIABLE CASE, A NEGATIVE ',
     1           'COUNT WAS ENCOUNTERED.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
 3010 CONTINUE
C
      ANGINC=2.0*PI/REAL(NUMCLA)
C
      YMAX=X(1)
      DO3060J=1,NUMCLA
        YMAX=MAX(YMAX,X(J))
 3060 CONTINUE
C
      DO3070J=1,NUMCLA
        TEMP1(J)=SQRT(X(J)/YMAX)
 3070 CONTINUE
C
      K=0
      J2=0
      DO3120J=1,NUMCLA
C
        R=TEMP1(J)
        ANGSTA=PI - (J-1)*ANGINC
        ANGSTO=ANGSTA-ANGINC
C
        K=K+1
        J2=J2+1
C
        XPLOT(K)=0.0
        YPLOT(K)=0.0
        D2(K)=J2
C
        ANG=ANGSTA
        K=K+1
        XPLOT(K)=R*COS(ANG)
        YPLOT(K)=R*SIN(ANG)
        D2(K)=J2
C
 3125   CONTINUE
        ANG=ANG - 0.015
        IF(ANG.LT.ANGSTO)THEN
          K=K+1
          XPLOT(K)=R*COS(ANGSTO)
          YPLOT(K)=R*SIN(ANGSTO)
          D2(K)=J2
          K=K+1
          XPLOT(K)=0.0
          YPLOT(K)=0.0
          D2(K)=J2
          GOTO3120
        ELSE
          K=K+1
          XPLOT(K)=R*COS(ANG)
          YPLOT(K)=R*SIN(ANG)
          D2(K)=J2
        ENDIF
C
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'ROS2')THEN
          WRITE(ICOUT,3121)J,J2,K,ANGSTA,ANGSTO,ANG
 3121     FORMAT('J,J2,K,ANSTA,ANGSTO,ANG = ',3I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3123)R,XPLOT(K),YPLOT(K)
 3123     FORMAT('R,XPLOT(K),YPLOT(K) = ',3G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        GOTO3125
C
 3120 CONTINUE
C
      NPLOTP=K
      NPLOTV=3
      GOTO9000
C
C               ******************
C               **   STEP 90--  **
C               **   EXIT       **
C               ******************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'ROS2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPROS2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)NPLOTP
 9012   FORMAT('NPLOTP = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,NPLOTP
          WRITE(ICOUT,9016)I,YPLOT(I),XPLOT(I),D2(I)
 9016     FORMAT('I,YPLOT(I),XPLOT(I),D2(I) = ',I8,2G15.7,F9.2)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPROTA(X,Y,XREF,YREF,ANGLE,AMAX,XP,YP)
C
C     ROTATE THE POINT (X,Y) ABOUT THE
C     REFERENCE POINT (XREF,YREF).
C     THE ANGLE OF ROTATION IS ANGLE.
C     AMAX (STANDING FOR MAXIMUM ANGLE) IS
C     THE ANGLE FOR 1 FULL ROTATION
C     (360.0 FOR DEGREES, 2*PI FOR RADIANS,
C     400 FOR GRADS)--THIS IMPLICITELY DEFINES
C     THE UNITS FOR THE ANGLE.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--OCTOBER   1980.
C     UPDATED         --APRIL     1981.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      THETA=(ANGLE/AMAX)*2.0*3.1415926
C
      XROT=(X-XREF)*COS(THETA)-(Y-YREF)*SIN(THETA)
      YROT=(X-XREF)*SIN(THETA)+(Y-YREF)*COS(THETA)
C
      XP=XREF+XROT
      YP=YREF+YROT
      GOTO9000
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE DPROWL(IHARG,IARGT,IARG,NUMARG,IDEFR1,IDEFR2,
     1IFROW1,IFROW2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE ROW LIMITS
C              WHICH WILL DEFINE THE EXTREME
C              ROWS (WITHIN A FILE) TO BE SCANNED IN CARRYING
C              OUT THE READ AND SERIAL READ COMMANDS.
C              THE 2 LIMITS ARE CONTAINED IN THE
C              2 ARGUMENTS IFROW1 AND IFROW2, RESPECTIVELY.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --IARG   (AN INTEGER VECTOR)
C                     --NUMARG
C                     --IDEFR1
C                     --IDEFR2
C     OUTPUT ARGUMENTS--IFROW1 (AN INTEGER VARIABLE
C                       CONTAINING THE MINIMUM ROW
C                       IN THE DATA FILE TO BE SCANNED
C                       DURING A    READ    OR A    SERIAL READ.
C                     --IFROW2 (AN INTEGER VARIABLE
C                       CONTAINING THE MAXIMUM ROW
C                       IN THE DATA FILE TO BE SCANNED
C                       DURING A    READ    OR A    SERIAL READ.
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--NOVEMBER  1980.
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
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      IHOLD1=0
      IHOLD2=0
C
C               ****************************************************
C               **  TREAT THE CASE WHEN                           **
C               **  THE ROW    LIMITS ARE TO BE CHANGED           **
C               ****************************************************
C
 1100 CONTINUE
      IF(NUMARG.LE.0)GOTO1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'LIMI')GOTO1110
      GOTO1190
C
 1110 CONTINUE
      IF(NUMARG.EQ.1)GOTO1120
      IF(IHARG(NUMARG).EQ.'ON')GOTO1120
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1120
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1120
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1120
      IF(NUMARG.GE.3.AND.IARGT(2).EQ.'NUMB'.AND.
     1IARGT(3).EQ.'NUMB')GOTO1130
      GOTO1190
C
 1120 CONTINUE
      I1=IDEFR1
      I2=IDEFR2
      IF(I1.LE.I2)IHOLD1=I1
      IF(I1.LE.I2)IHOLD2=I2
      IF(I1.GT.I2)IHOLD1=I2
      IF(I1.GT.I2)IHOLD2=I1
      GOTO1180
C
 1130 CONTINUE
      I1=IARG(2)
      I2=IARG(3)
      IF(I1.LE.I2)IHOLD1=I1
      IF(I1.LE.I2)IHOLD2=I2
      IF(I1.GT.I2)IHOLD1=I2
      IF(I1.GT.I2)IHOLD2=I1
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      IFROW1=IHOLD1
      IFROW2=IHOLD2
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1185)
 1185 FORMAT('THE ROW LIMITS (FOR READ AND SERIAL READ)')
      CALL DPWRST('XXX','BUG ')
      IF(IFROW2.NE.IDEFR2)WRITE(ICOUT,1186)IFROW1,IFROW2
 1186 FORMAT('HAVE JUST BEEN SET TO ',I8,2X,I8)
      IF(IFROW2.NE.IDEFR2)CALL DPWRST('XXX','BUG ')
      IF(IFROW2.EQ.IDEFR2)WRITE(ICOUT,1187)IFROW1
 1187 FORMAT('HAVE JUST BEEN SET TO ',I8,2X,'INFINITY')
      IF(IFROW2.EQ.IDEFR2)CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1900
C
 1190 CONTINUE
C
C               ****************************************************
C               **  TREAT THE CASE WHEN                           **
C               **  THE ROW    MINIMUM IS TO BE CHANGED           **
C               ****************************************************
C
 1200 CONTINUE
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'MINI')GOTO1210
      GOTO1290
C
 1210 CONTINUE
      IF(NUMARG.EQ.1)GOTO1220
      IF(IHARG(NUMARG).EQ.'ON')GOTO1220
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1220
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1220
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1220
      IF(NUMARG.GE.2.AND.IARGT(2).EQ.'NUMB')GOTO1230
      GOTO1290
C
 1220 CONTINUE
      IHOLD1=IDEFR1
      GOTO1280
C
 1230 CONTINUE
      IHOLD1=IARG(2)
      GOTO1280
C
 1280 CONTINUE
      IFOUND='YES'
      IFROW1=IHOLD1
C
      IF(IFEEDB.EQ.'OFF')GOTO1289
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1285)
 1285 FORMAT('THE ROW MINIMUM (FOR READ AND SERIAL READ)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1286)IFROW1
 1286 FORMAT('HAS JUST BEEN SET TO ',I8)
      CALL DPWRST('XXX','BUG ')
 1289 CONTINUE
      GOTO1900
C
 1290 CONTINUE
C
C               ****************************************************
C               **  TREAT THE CASE WHEN                           **
C               **  THE ROW    MAXIMUM IS TO BE CHANGED           **
C               ****************************************************
C
 1300 CONTINUE
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'MAXI')GOTO1310
      GOTO1390
C
 1310 CONTINUE
      IF(NUMARG.EQ.1)GOTO1320
      IF(IHARG(NUMARG).EQ.'ON')GOTO1320
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1320
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1320
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1320
      IF(NUMARG.GE.2.AND.IARGT(2).EQ.'NUMB')GOTO1330
      GOTO1390
C
 1320 CONTINUE
      IHOLD2=IDEFR2
      GOTO1380
C
 1330 CONTINUE
      IHOLD2=IARG(2)
      GOTO1380
C
 1380 CONTINUE
      IFOUND='YES'
      IFROW2=IHOLD2
C
      IF(IFEEDB.EQ.'OFF')GOTO1389
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1385)
 1385 FORMAT('THE ROW MAXIMUM (FOR READ AND SERIAL READ)')
      CALL DPWRST('XXX','BUG ')
      IF(IFROW2.NE.IDEFR2)WRITE(ICOUT,1386)IFROW2
 1386 FORMAT('HAS JUST BEEN SET TO ',I8)
      IF(IFROW2.NE.IDEFR2)CALL DPWRST('XXX','BUG ')
      IF(IFROW2.EQ.IDEFR2)WRITE(ICOUT,1387)
 1387 FORMAT('HAS JUST BEEN SET TO ','INFINITY')
      IF(IFROW2.EQ.IDEFR2)CALL DPWRST('XXX','BUG ')
 1389 CONTINUE
      GOTO1900
C
 1390 CONTINUE
C
 1900 CONTINUE
      RETURN
      END
      SUBROUTINE DPRPCO(IHARG,NUMARG,IDERPC,MAXREG,IREPCO,
     1IBUGP2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE REGION PATTERN COLORS = THE COLORS
C              OF THE LINES MAKING UP A PATTERN WITHIN A REGION.
C              THESE ARE LOCATED IN THE VECTOR IREPCO(.).
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --NUMARG
C                     --IDERPC
C                     --MAXREG
C                     --IBUGP2 ('ON' OR 'OFF' )
C     OUTPUT ARGUMENTS--IREPCO (A CHARACTER VECTOR)
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
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
      CHARACTER*4 IDERPC
      CHARACTER*4 IREPCO
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
      DIMENSION IREPCO(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      ISUBN1='DPRP'
      ISUBN2='CO  '
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
   51 FORMAT('***** AT THE BEGINNING OF DPRPCO--')
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
      WRITE(ICOUT,55)IDERPC
   55 FORMAT('IDERPC = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,60)NUMARG
   60 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO65I=1,NUMARG
      WRITE(ICOUT,66)IHARG(I)
   66 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,70)IREPCO(1)
   70 FORMAT('IREPCO(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO75I=1,10
      WRITE(ICOUT,76)I,IREPCO(I)
   76 FORMAT('I,IREPCO(I) = ',I8,2X,A4)
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
      IF(IHARG(3).EQ.'ALL')IHOLD1='    '
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
      IREPCO(1)=IDERPC
      GOTO1270
C
 1220 CONTINUE
      NUMREG=NUMARG-2
      IF(NUMREG.GT.MAXREG)NUMREG=MAXREG
      DO1225I=1,NUMREG
      J=I+2
      IHOLD1=IHARG(J)
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2=IDERPC
      IF(IHOLD1.EQ.'OFF')IHOLD2=IDERPC
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDERPC
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDERPC
      IREPCO(I)=IHOLD2
 1225 CONTINUE
      GOTO1270
C
 1270 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1279
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO1278I=1,NUMREG
      WRITE(ICOUT,1276)I,IREPCO(I)
 1276 FORMAT('THE COLOR OF REGION PATTERN ',I6,
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
      IF(IHOLD1.EQ.'ON')IHOLD2=IDERPC
      IF(IHOLD1.EQ.'OFF')IHOLD2=IDERPC
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDERPC
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDERPC
      DO1315I=1,NUMREG
      IREPCO(I)=IHOLD2
 1315 CONTINUE
      GOTO1370
C
 1370 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1316)IREPCO(I)
 1316 FORMAT('THE COLOR OF ALL REGION PATTERNS',
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
 9011 FORMAT('***** AT THE END       OF DPRPCO--')
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
      WRITE(ICOUT,9015)IDERPC
 9015 FORMAT('IDERPC = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9020)NUMARG
 9020 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO9025I=1,NUMARG
      WRITE(ICOUT,9026)IHARG(I)
 9026 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      WRITE(ICOUT,9030)IREPCO(1)
 9030 FORMAT('IREPCO(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO9035I=1,10
      WRITE(ICOUT,9036)I,IREPCO(I)
 9036 FORMAT('I,IREPCO(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPRPLI(IHARG,IHARG2,NUMARG,IDERPL,MAXREG,IREPLI,
CCCCC AUGUST 1995.  ADD IHARG2 FOR DASH2, ETC
CCCCC SUBROUTINE DPRPLI(IHARG,NUMARG,IDERPL,MAXREG,IREPLI,
     1IBUGP2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE PATTERN LINES = THE LINES TYPES
C              OF THE PATTERN WITHIN THE REGIONS.
C              THESE ARE LOCATED IN THE VECTOR IREPLI(.).
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --NUMARG
C                     --IDERPL
C                     --MAXREG
C                     --IBUGP2 ('ON' OR 'OFF' )
C     OUTPUT ARGUMENTS--IREPLI (A CHARACTER VECTOR)
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--DECEMBER  1983.
C     UPDATED         --AUGUST    1995.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
CCCCC AUGUST 1995.  ADD FOLLOWING LINE
      CHARACTER*4 IHARG2
      CHARACTER*4 IDERPL
      CHARACTER*4 IREPLI
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
CCCCC AUGUST 1995.  ADD FOLLOWING LINE
      DIMENSION IHARG2(*)
      DIMENSION IREPLI(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      ISUBN1='DPRP'
      ISUBN2='LI  '
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
   51 FORMAT('***** AT THE BEGINNING OF DPRPLI--')
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
      WRITE(ICOUT,55)IDERPL
   55 FORMAT('IDERPL = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,60)NUMARG
   60 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO65I=1,NUMARG
      WRITE(ICOUT,66)IHARG(I)
   66 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,70)IREPLI(1)
   70 FORMAT('IREPLI(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO75I=1,10
      WRITE(ICOUT,76)I,IREPLI(I)
   76 FORMAT('I,IREPLI(I) = ',I8,2X,A4)
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
      IF(NUMARG.LE.2)GOTO9000
      IF(NUMARG.EQ.3)GOTO1130
      IF(NUMARG.EQ.4)GOTO1140
      IF(NUMARG.EQ.5)GOTO1150
      GOTO1160
C
 1130 CONTINUE
      GOTO1200
C
 1140 CONTINUE
      IF(IHARG(5).EQ.'ALL')IHOLD1='    '
      IF(IHARG(5).EQ.'ALL')GOTO1300
      GOTO1200
C
 1150 CONTINUE
CCCCC APRIL 1996.  CHANGE IHOLD TO IHOLD1 BELOW
      IF(IHARG(5).EQ.'ALL')THEN
        IHOLD1=IHARG(6)
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(6).EQ.'2')IHOLD1='DA2'
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(6).EQ.'3')IHOLD1='DA3'
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(6).EQ.'4')IHOLD1='DA4'
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(6).EQ.'5')IHOLD1='DA5'
        GOTO1300
      ENDIF
      IF(IHARG(6).EQ.'ALL')THEN
        IHOLD1=IHARG(5)
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(5).EQ.'2')IHOLD1='DA2'
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(5).EQ.'3')IHOLD1='DA3'
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(5).EQ.'4')IHOLD1='DA4'
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(5).EQ.'5')IHOLD1='DA5'
        GOTO1300
      ENDIF
      GOTO1200
C
 1160 CONTINUE
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
      IF(NUMARG.LE.3)GOTO1210
      GOTO1220
C
 1210 CONTINUE
      NUMREG=1
      IREPLI(1)='    '
      GOTO1270
C
 1220 CONTINUE
      NUMREG=NUMARG-3
      IF(NUMREG.GT.MAXREG)NUMREG=MAXREG
      DO1225I=1,NUMREG
      J=I+3
      IHOLD1=IHARG(J)
      IF(IHOLD1.EQ.'DASH'.AND.IHARG2(J).EQ.'2')IHOLD1='DA2'
      IF(IHOLD1.EQ.'DASH'.AND.IHARG2(J).EQ.'3')IHOLD1='DA3'
      IF(IHOLD1.EQ.'DASH'.AND.IHARG2(J).EQ.'4')IHOLD1='DA4'
      IF(IHOLD1.EQ.'DASH'.AND.IHARG2(J).EQ.'5')IHOLD1='DA5'
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2='SOLI'
      IF(IHOLD1.EQ.'OFF')IHOLD2='    '
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDERPL
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDERPL
      IREPLI(I)=IHOLD2
 1225 CONTINUE
      GOTO1270
C
 1270 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1279
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO1278I=1,NUMREG
      WRITE(ICOUT,1276)I,IREPLI(I)
 1276 FORMAT('THE LINE TYPE FOR REGION PATTERN ',I6,
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
      IF(IHOLD1.EQ.'ON')IHOLD2='SOLI'
      IF(IHOLD1.EQ.'OFF')IHOLD2='    '
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDERPL
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDERPL
      DO1315I=1,NUMREG
      IREPLI(I)=IHOLD2
 1315 CONTINUE
      GOTO1370
C
 1370 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1316)IREPLI(I)
 1316 FORMAT('THE LINE TYPE FOR ALL REGION PATTERNS',
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
 9011 FORMAT('***** AT THE END       OF DPRPLI--')
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
      WRITE(ICOUT,9015)IDERPL
 9015 FORMAT('IDERPL = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9020)NUMARG
 9020 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO9025I=1,NUMARG
      WRITE(ICOUT,9026)IHARG(I)
 9026 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      WRITE(ICOUT,9030)IREPLI(1)
 9030 FORMAT('IREPLI(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO9035I=1,10
      WRITE(ICOUT,9036)I,IREPLI(I)
 9036 FORMAT('I,IREPLI(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPRPLO(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                  IBUGG2,IBUGG3,ISUBRO,IBUGQ,IFOUND,IERROR)
C
C     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
C              THAT WILL DEFINE A REPAIR PLOT FOR MULTIPLE
C              SYSTEMS.
C     REFERENCE--TOBIAS AND TRINDADE (1995), "APPLIED
C                RELIABILITY", SECOND EDITION, CHAPMAN AND HALL,
C                PP. 314.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2006/10
C     ORIGINAL VERSION--OCTOBER    2006.
C     UPDATED         --APRIL      2011. USE DPPAR AND DPPAR3
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IAND1
      CHARACTER*4 IAND2
      CHARACTER*4 IBUGG2
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGQ
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      CHARACTER*4 ICASE
      PARAMETER (MAXSPN=10)
      CHARACTER*40 INAME
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
C
      DIMENSION Y1(MAXOBV)
      DIMENSION X1(MAXOBV)
      DIMENSION XCEN(MAXOBV)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TEMP3(MAXOBV)
      DIMENSION TEMP4(MAXOBV)
      DIMENSION TEMP5(MAXOBV)
C
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),X1(1))
      EQUIVALENCE (GARBAG(IGARB3),XCEN(1))
      EQUIVALENCE (GARBAG(IGARB4),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB5),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB6),TEMP3(1))
      EQUIVALENCE (GARBAG(IGARB7),TEMP4(1))
      EQUIVALENCE (GARBAG(IGARB8),TEMP5(1))
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      ISUBN1='DPRP'
      ISUBN2='PL  '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'RPLO')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPRPLO--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASPL,IAND1,IAND2,MAXCOL
   52   FORMAT('ICASPL,IAND1,IAND2,MAXCOL = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGG2,IBUGG3,IBUGQ,ISUBRO
   53   FORMAT('IBUGG2,IBUGG3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *********************************************
C               **  TREAT THE REPAIR PLOT                  **
C               *********************************************
C
C               *******************************************
C               **  STEP 1--                             **
C               **  SEARCH FOR REPAIR PLOT               **
C               *******************************************
C
      ISTEPN='11'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'RPLO')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASPL='REPA'
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PLOT')THEN
        ILASTC=1
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
        IFOUND='YES'
      ELSE
        ICASPL='    '
        IFOUND='NO'
        GOTO9000
      ENDIF
C
C               ****************************************
C               **  STEP 2--                          **
C               **  EXTRACT THE VARIABLE LIST         **
C               ****************************************
C
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'RPLO')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='REPAIR PLOT'
      MINNA=1
      MAXNA=100
      MINN2=2
      IFLAGE=1
      IFLAGM=0
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=1
      MAXNVA=3
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
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'RPLO')THEN
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
C     EXTRACT THE VARIABLES.
C
      ICOL=1
      CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1            INAME,IVARN1,IVARN2,IVARTY,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1            MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1            MAXCP4,MAXCP5,MAXCP6,
     1            V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1            Y1,X1,XCEN,NS,NGROUP,NCENS,ICASE,
     1            IBUGG3,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      IF(NUMVAR.LT.2)NGROUP=0
      IF(NUMVAR.LT.3)NCENS=0
C
C               *****************************************************
C               **  STEP 41--                                      **
C               **  FORM THE VERTICAL AND HORIZONTAL AXIS          **
C               **  VARIABLES (Y(.) AND X(.), RESPECTIVELY) FOR    **
C               **  THE PLOT.                                      **
C               **  FORM THE CURVE DESIGNATION VARIABLED(.)  .     **
C               **  THIS WILL BE ALL ONES.                         **
C               **  DEFINE THE NUMBER OF PLOT POINTS   (NPLOTP).   **
C               **  DEFINE THE NUMBER OF PLOT VARIABLES(NPLOTV).   **
C               *****************************************************
C
      ISTEPN='41'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'RPLO')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPRPL2(Y1,NS,X1,NGROUP,XCEN,NCENS,ICASPL,MAXN,
     1            TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,
     1            Y,X,D,NPLOTP,NPLOTV,
     1            IBUGG3,ISUBRO,IERROR)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'RPLO')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPRPLO--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9013   FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',
     1         I8,I8,I8,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        IF(NPLOTP.GT.0)THEN
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
      SUBROUTINE DPRPL2(Y1,N,X1,NGROUP,XCEN,NCENS,ICASPL,MAXN,
     1XIDTEM,TEMP2,TEMP3,TEMP4,TEMP5,
     1Y,X,D,NPLOTP,NPLOTV,
     1IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
C              THAT WILL DEFINE A REPAIR PLOT.
C              PLOT THE REPAIR TIMES FOR EACH GROUP, EACH GROUP
C              MAY HAVE A SINGLE CENSORING TIME.
C     INPUT ARGUMENTS--Y1     = THE SINGLE PRECISION VECTOR OF
C                               (UNSORTED) REPAIR/CENSORING TIMES.
C                    --X1     = THE OPTIONAL SINGLE PRECISION VECTOR
C                               GROUP-ID VALUES
C                    --XCENS  = THE OPTIONAL SINGLE PRECISION VECTOR
C                               OF CENSOR VALUES (1 = REPAIR
C                               TIME, 0 = CENSOR TIME).
C                      NY     = THE INTEGER NUMBER OF OBSERVATIONS
C                               IN THE VECTOR Y1.
C                      NX     = THE INTEGER NUMBER OF OBSERVATIONS
C                               IN THE VECTOR X1.
C                      NC     = THE INTEGER NUMBER OF OBSERVATIONS
C                               IN THE VECTOR XCEN.
C     REFERENCE--TOBIAS AND TRINDADE (1995), "APPLIED
C                RELIABILITY", SECOND EDITION, CHAPMAN AND HALL,
C                PP. 314.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2006/10
C     ORIGINAL VERSION--OCTOBER   2006.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DIMENSION Y1(*)
      DIMENSION X1(*)
      DIMENSION XCEN(*)
C
      DIMENSION XIDTEM(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
      DIMENSION TEMP4(*)
      DIMENSION TEMP5(*)
C
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION D(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPRP'
      ISUBN2='L2  '
C
      IERROR='NO'
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'RPL2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPRPL2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGG3,ISUBRO,IERROR
   52   FORMAT('IBUGG3,ISUBRO,IERROR = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)N,NGROUP,NCENS,ICASPL,MAXN
   53   FORMAT('N,NGROUP,NCENS,ICASPL,MAXN = ',3I10,2X,A4,I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,56)I,Y1(I),X1(I),XCEN(I)
   56     FORMAT('I, Y1(I),X1(I),XCEN(I) = ',I10,3G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      IF(N.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN REPAIR PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS MUST BE AT LEAST 2;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,114)N
  114   FORMAT('      THE ENTERED NUMBER OF OBSERVATIONS HERE = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      HOLD=Y1(1)
      DO120I=1,N
      IF(Y1(I).NE.HOLD)GOTO129
  120 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,121)
  121 FORMAT('***** ERROR IN REPAIR PLOT--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,122)HOLD
  122 FORMAT('      ALL ELEMENTS IN RESPONSE VARIABLE ARE ',
     1       'IDENTICALLY EQUAL TO ',G15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
  129 CONTINUE
C
C               ****************************************************
C               **  STEP 12--                                     **
C               **  COMPUTE COORDINATES FOR MEAN REPAIR FUNCTION  **
C               **  PLOT                                          **
C               ****************************************************
C
C     CASE 1: NO GROUP OR CENSORING VARIABLE
C
      IF(NGROUP.EQ.0 .AND. NCENS.EQ.0)THEN
        CALL SORT(Y1,N,Y1)
        DO1000I=1,N
          Y(I)=1.0
          X(I)=Y1(I)
          D(I)=1.0
 1000   CONTINUE
        NPLOTP=N
C
C       CASE 2: GROUP VARIABLE, BUT NO CENSORING VARIABLE
C
      ELSEIF(NCENS.EQ.0)THEN
C
C       STEP 1: DETERMINE UNIQUE GROUPS
C
        NUMSET=0
        DO1051I=1,N
          IF(NUMSET.EQ.0)GOTO1053
          DO1052J=1,NUMSET
            IF(X1(I).EQ.XIDTEM(J))GOTO1051
 1052     CONTINUE
 1053     CONTINUE
          NUMSET=NUMSET+1
          XIDTEM(NUMSET)=X1(I)
 1051   CONTINUE
        CALL SORT(XIDTEM,NUMSET,XIDTEM)
C
C       STEP 2: GENERATE TRACES FOR EACH GROUP
C
        J=0
        DO1090ISET=1,NUMSET
C
          K=0
          DO1091I=1,N
            IF(X1(I).EQ.XIDTEM(ISET))THEN
              K=K+1
              TEMP2(K)=Y1(I)
            ENDIF
1091      CONTINUE
          NI=K
          CALL SORT(TEMP2,NI,TEMP2)
          DO1096I=1,NI
            J=J+1
            Y(J)=XIDTEM(ISET)
            X(J)=TEMP2(I)
            D(J)=REAL(ISET)
1096      CONTINUE
1090    CONTINUE
        NPLOTP=J
C
C       CASE 3: BOTH GROUP VARIABLE AND CENSORING VARIABLE
C
      ELSE
C
C       STEP 1: DETERMINE UNIQUE GROUPS
C
        NUMSET=0
        DO1111I=1,N
          IF(NUMSET.EQ.0)GOTO1113
          DO1112J=1,NUMSET
            IF(X1(I).EQ.XIDTEM(J))GOTO1111
 1112     CONTINUE
 1113     CONTINUE
          NUMSET=NUMSET+1
          XIDTEM(NUMSET)=X1(I)
 1111   CONTINUE
        CALL SORT(XIDTEM,NUMSET,XIDTEM)
C
C       STEP 2A: EXTRACT RESPONSE AND CENSORING DATA FOR EACH
C                GROUP
C
        J=0
        ISETMX=NUMSET
        DO1120ISET=1,NUMSET
C
          K=0
          DO1121I=1,N
            IF(X1(I).EQ.XIDTEM(ISET))THEN
              K=K+1
              TEMP2(K)=Y1(I)
              TEMP3(K)=XCEN(I)
            ENDIF
1121      CONTINUE
          NI=K
C
C       STEP 2B: PROCESS THE CENSORING VARIABLE.  THERE CAN
C                BE AT MOST ONE CENSORING POINT FOR EACH
C                GROUP.
C
          CALL SORTC(TEMP2,TEMP3,NI,TEMP4,TEMP5)
          DO1160I=1,NI
            TEMP2(I)=TEMP4(I)
            TEMP3(I)=TEMP5(I)
 1160     CONTINUE
          AREP=TEMP3(1)
          ACEN=TEMP3(NI)
          IF(NI.LE.1)THEN
            NTEMPR=1
            NTEMPC=0
          ELSE
            IF(AREP.EQ.ACEN)THEN
              NTEMPR=NI
              NTEMPC=0
              DO1170I=1,NI
                IF(TEMP3(I).NE.AREP)THEN
                  WRITE(ICOUT,999)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,121)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,1171)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,1172)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,1173)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,1174)XIDTEM(ISET)
                  CALL DPWRST('XXX','BUG ')
                  IERROR='YES'
                  GOTO9000
                ENDIF
 1170         CONTINUE
            ELSE
              NTEMPR=NI-1
              NTEMPC=1
              DO1180I=1,NTEMPR
                IF(TEMP3(I).NE.AREP)THEN
                  WRITE(ICOUT,999)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,121)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,1171)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,1172)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,1173)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,1174)XIDTEM(ISET)
                  CALL DPWRST('XXX','BUG ')
                  IERROR='YES'
                  GOTO9000
                ENDIF
 1180         CONTINUE
            ENDIF
          ENDIF
 1171 FORMAT('      FOR EACH SYSTEM, THERE SHOULD BE AT MOST')
 1172 FORMAT('      CENSORING TIME AND IT MUST BE THE MAXIMUM')
 1173 FORMAT('      VALUE FOR THAT SYSTEM.')
 1174 FORMAT('      SUCH WAS NOT THE CASE FOR SYSTEM ',G15.7)
C
C       STEP 2C: TRACE 1 IS SIMPLY ALL OF THE REPAIR TIMES
C                (I.E., OMIT THE CENSORING TIME).  THEN TRACES
C                2 - NUMBER OF SYSTEMS + 1 ARE THE REPAIR PLUS
C                CENSORING TIMES FOR EACH SYSTEM.
C
          DO1191I=1,NTEMPR
            J=J+1
            Y(J)=XIDTEM(ISET)
            X(J)=TEMP2(I)
            D(J)=1.0
1191      CONTINUE
C
          DO1196I=1,NI
            J=J+1
            Y(J)=XIDTEM(ISET)
            X(J)=TEMP2(I)
            D(J)=REAL(ISET+1)
1196      CONTINUE
C
1120    CONTINUE
        NPLOTP=J
      ENDIF
C
      NPLOTV=2
C
C               ******************
C               **   STEP 90--  **
C               **   EXIT       **
C               ******************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'RPL2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPRPL2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGG3,ISUBRO,IERROR
 9012   FORMAT('IBUGG3,ISUBRO,IERROR = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)N,ICASPL,MAXN
 9013   FORMAT('N,ICASPL,MAXN = ',I8,2X,A4,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)NPLOTP,NPLOTV
 9021   FORMAT('NPLOTP,NPLOTV = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO9022I=1,NPLOTP
          WRITE(ICOUT,9023)I,Y(I),X(I),D(I)
 9023     FORMAT('I,Y(I),X(I),D(I) = ',I8,3E15.7)
          CALL DPWRST('XXX','BUG ')
 9022  CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPRPSP(IHARG,IARGT,ARG,NUMARG,PDERPS,MAXREG,PREPSP,
     1IBUGP2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE REGION PATTERN SPACINGS = THE SPACINGS
C              BETWEEN THE LINES WHICH MAKE UP THE PATTERNS WITHIN THE REGIONS.
C              THESE ARE LOCATED IN THE VECTOR PREPSP(.).
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --IARGT  (A  CHARACTER VECTOR)
C                     --ARG
C                     --NUMARG
C                     --PDERPS
C                     --MAXREG
C                     --IBUGP2 ('ON' OR 'OFF' )
C     OUTPUT ARGUMENTS--PREPSP (A FLOATING POINT VECTOR)
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
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
      CHARACTER*4 IARGT
C
      CHARACTER*4 IBUGP2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD1
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      DIMENSION IHARG(*)
      DIMENSION IARGT(*)
      DIMENSION ARG(*)
      DIMENSION PREPSP(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      ISUBN1='DPRP'
      ISUBN2='SP  '
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
   51 FORMAT('***** AT THE BEGINNING OF DPRPSP--')
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
      WRITE(ICOUT,55)PDERPS
   55 FORMAT('PDERPS = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,60)NUMARG
   60 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO65I=1,NUMARG
      WRITE(ICOUT,66)IHARG(I),IARGT(I),ARG(I)
   66 FORMAT('IHARG(I),IARGT(I),ARG(I) = ',A4,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,70)PREPSP(1)
   70 FORMAT('PREPSP(1) = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      DO75I=1,10
      WRITE(ICOUT,76)I,PREPSP(I)
   76 FORMAT('I,PREPSP(I) = ',I8,2X,E15.7)
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
      IF(IHARG(3).EQ.'ALL')IHOLD1='    '
      IF(IHARG(3).EQ.'ALL')HOLD1=PDERPS
      IF(IHARG(3).EQ.'ALL')GOTO1300
      GOTO1200
C
 1140 CONTINUE
      IF(IHARG(3).EQ.'ALL')IHOLD1=IHARG(4)
      IF(IHARG(3).EQ.'ALL')HOLD1=ARG(4)
      IF(IHARG(3).EQ.'ALL')GOTO1300
      IF(IHARG(4).EQ.'ALL')IHOLD1=IHARG(3)
      IF(IHARG(4).EQ.'ALL')HOLD1=ARG(3)
      IF(IHARG(4).EQ.'ALL')GOTO1300
      GOTO1200
C
 1150 CONTINUE
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
      IF(NUMARG.LE.2)GOTO1210
      GOTO1220
C
 1210 CONTINUE
      NUMREG=1
      PREPSP(1)=PDERPS
      GOTO1270
C
 1220 CONTINUE
      NUMREG=NUMARG-2
      IF(NUMREG.GT.MAXREG)NUMREG=MAXREG
      DO1225I=1,NUMREG
      J=I+2
      IHOLD1=IHARG(J)
      HOLD1=ARG(J)
      HOLD2=HOLD1
      IF(IHOLD1.EQ.'ON')HOLD2=PDERPS
      IF(IHOLD1.EQ.'OFF')HOLD2=PDERPS
      IF(IHOLD1.EQ.'AUTO')HOLD2=PDERPS
      IF(IHOLD1.EQ.'DEFA')HOLD2=PDERPS
      PREPSP(I)=HOLD2
 1225 CONTINUE
      GOTO1270
C
 1270 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1279
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO1278I=1,NUMREG
      WRITE(ICOUT,1276)I,PREPSP(I)
 1276 FORMAT('THE SPACING BETWEEN (LINES WITHIN) PATTERN ',I6,
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
      IF(IHOLD1.EQ.'ON')HOLD2=PDERPS
      IF(IHOLD1.EQ.'OFF')HOLD2=PDERPS
      IF(IHOLD1.EQ.'AUTO')HOLD2=PDERPS
      IF(IHOLD1.EQ.'DEFA')HOLD2=PDERPS
      DO1315I=1,NUMREG
      PREPSP(I)=HOLD2
 1315 CONTINUE
      GOTO1370
C
 1370 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1316)PREPSP(I)
 1316 FORMAT('THE SPACING BETWEEN (LINES WITHIN) ALL PATTERNS',
     1' HAS JUST BEEN SET TO ',E15.7)
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
 9011 FORMAT('***** AT THE END       OF DPRPSP--')
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
      WRITE(ICOUT,9015)PDERPS
 9015 FORMAT('PDERPS = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9020)NUMARG
 9020 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO9025I=1,NUMARG
      WRITE(ICOUT,9026)IHARG(I),IARGT(I),ARG(I)
 9026 FORMAT('IHARG(I),IARGT(I),ARG(I) = ',A4,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      WRITE(ICOUT,9030)PREPSP(1)
 9030 FORMAT('PREPSP(1) = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      DO9035I=1,10
      WRITE(ICOUT,9036)I,PREPSP(I)
 9036 FORMAT('I,PREPSP(I) = ',I8,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPRPTH(IHARG,IARGT,ARG,NUMARG,PDERPT,MAXREG,PREPTH,
     1IBUGP2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE REGION PATTERN THICKNESSES = THE THICKNESSES
C              OF THE LINES WHICH MAKE UP THE PATTERNS WITHIN THE REGIONS.
C              THESE ARE LOCATED IN THE VECTOR PREPTH(.).
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --IARGT  (A  CHARACTER VECTOR)
C                     --ARG
C                     --NUMARG
C                     --PDERPT
C                     --MAXREG
C                     --IBUGP2 ('ON' OR 'OFF' )
C     OUTPUT ARGUMENTS--PREPTH (A FLOATING POINT VECTOR)
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
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
      CHARACTER*4 IARGT
C
      CHARACTER*4 IBUGP2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD1
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      DIMENSION IHARG(*)
      DIMENSION IARGT(*)
      DIMENSION ARG(*)
      DIMENSION PREPTH(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      ISUBN1='DPRP'
      ISUBN2='TH  '
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
   51 FORMAT('***** AT THE BEGINNING OF DPRPTH--')
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
      WRITE(ICOUT,55)PDERPT
   55 FORMAT('PDERPT = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,60)NUMARG
   60 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO65I=1,NUMARG
      WRITE(ICOUT,66)IHARG(I),IARGT(I),ARG(I)
   66 FORMAT('IHARG(I),IARGT(I),ARG(I) = ',A4,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,70)PREPTH(1)
   70 FORMAT('PREPTH(1) = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      DO75I=1,10
      WRITE(ICOUT,76)I,PREPTH(I)
   76 FORMAT('I,PREPTH(I) = ',I8,2X,E15.7)
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
      IF(IHARG(3).EQ.'ALL')IHOLD1='    '
      IF(IHARG(3).EQ.'ALL')HOLD1=PDERPT
      IF(IHARG(3).EQ.'ALL')GOTO1300
      GOTO1200
C
 1140 CONTINUE
      IF(IHARG(3).EQ.'ALL')IHOLD1=IHARG(4)
      IF(IHARG(3).EQ.'ALL')HOLD1=ARG(4)
      IF(IHARG(3).EQ.'ALL')GOTO1300
      IF(IHARG(4).EQ.'ALL')IHOLD1=IHARG(3)
      IF(IHARG(4).EQ.'ALL')HOLD1=ARG(2)
      IF(IHARG(4).EQ.'ALL')GOTO1300
      GOTO1200
C
 1150 CONTINUE
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
      IF(NUMARG.LE.2)GOTO1210
      GOTO1220
C
 1210 CONTINUE
      NUMREG=1
      PREPTH(1)=PDERPT
      GOTO1270
C
 1220 CONTINUE
      NUMREG=NUMARG-2
      IF(NUMREG.GT.MAXREG)NUMREG=MAXREG
      DO1225I=1,NUMREG
      J=I+2
      IHOLD1=IHARG(J)
      HOLD1=ARG(J)
      HOLD2=HOLD1
      IF(IHOLD1.EQ.'ON')HOLD2=PDERPT
      IF(IHOLD1.EQ.'OFF')HOLD2=PDERPT
      IF(IHOLD1.EQ.'AUTO')HOLD2=PDERPT
      IF(IHOLD1.EQ.'DEFA')HOLD2=PDERPT
      PREPTH(I)=HOLD2
 1225 CONTINUE
      GOTO1270
C
 1270 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1279
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO1278I=1,NUMREG
      WRITE(ICOUT,1276)I,PREPTH(I)
 1276 FORMAT('THE THICKNESS OF (LINES WITHIN) PATTERN ',I6,
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
      IF(IHOLD1.EQ.'ON')HOLD2=PDERPT
      IF(IHOLD1.EQ.'OFF')HOLD2=PDERPT
      IF(IHOLD1.EQ.'AUTO')HOLD2=PDERPT
      IF(IHOLD1.EQ.'DEFA')HOLD2=PDERPT
      DO1315I=1,NUMREG
      PREPTH(I)=HOLD2
 1315 CONTINUE
      GOTO1370
C
 1370 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1316)PREPTH(I)
 1316 FORMAT('THE THICKNESS OF (LINES WITHIN) ALL PATTERNS',
     1' HAS JUST BEEN SET TO ',E15.7)
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
 9011 FORMAT('***** AT THE END       OF DPRPTH--')
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
      WRITE(ICOUT,9015)PDERPT
 9015 FORMAT('PDERPT = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9020)NUMARG
 9020 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO9025I=1,NUMARG
      WRITE(ICOUT,9026)IHARG(I),IARGT(I),ARG(I)
 9026 FORMAT('IHARG(I),IARGT(I),ARG(I) = ',A4,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      WRITE(ICOUT,9030)PREPTH(1)
 9030 FORMAT('PREPTH(1) = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      DO9035I=1,10
      WRITE(ICOUT,9036)I,PREPTH(I)
 9036 FORMAT('I,PREPTH(I) = ',I8,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPRPTY(IHARG,NUMARG,IDERPT,MAXREG,IREPTY,
     1IBUGP2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE PATTERN TYPES = THE TYPES
C              OF THE PATTERN WITHIN THE REGIONS.
C              THESE ARE LOCATED IN THE VECTOR IREPTY(.).
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --NUMARG
C                     --IDERPT
C                     --MAXREG
C                     --IBUGP2 ('ON' OR 'OFF' )
C     OUTPUT ARGUMENTS--IREPTY (A CHARACTER VECTOR)
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
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
      CHARACTER*4 IDERPT
      CHARACTER*4 IREPTY
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
      DIMENSION IREPTY(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      ISUBN1='DPRP'
      ISUBN2='TY  '
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
   51 FORMAT('***** AT THE BEGINNING OF DPRPTY--')
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
      WRITE(ICOUT,55)IDERPT
   55 FORMAT('IDERPT = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,60)NUMARG
   60 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO65I=1,NUMARG
      WRITE(ICOUT,66)IHARG(I)
   66 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,70)IREPTY(1)
   70 FORMAT('IREPTY(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO75I=1,10
      WRITE(ICOUT,76)I,IREPTY(I)
   76 FORMAT('I,IREPTY(I) = ',I8,2X,A4)
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
      IF(IHARG(3).EQ.'ALL')IHOLD1='    '
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
C               **  TREAT THE SINGLE     SPECIFICATION  CASE  **
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
      IREPTY(1)='    '
      GOTO1270
C
 1220 CONTINUE
      NUMREG=NUMARG-2
      IF(NUMREG.GT.MAXREG)NUMREG=MAXREG
      DO1225I=1,NUMREG
      J=I+2
      IHOLD1=IHARG(J)
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2='SOLI'
      IF(IHOLD1.EQ.'OFF')IHOLD2='    '
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDERPT
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDERPT
      IREPTY(I)=IHOLD2
 1225 CONTINUE
      GOTO1270
C
 1270 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1279
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO1278I=1,NUMREG
      WRITE(ICOUT,1276)I,IREPTY(I)
 1276 FORMAT('THE TYPE FOR REGION PATTERN ',I6,
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
      IF(IHOLD1.EQ.'ON')IHOLD2='SOLI'
      IF(IHOLD1.EQ.'OFF')IHOLD2='    '
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDERPT
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDERPT
      DO1315I=1,NUMREG
      IREPTY(I)=IHOLD2
 1315 CONTINUE
      GOTO1370
C
 1370 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1316)IREPTY(I)
 1316 FORMAT('THE TYPE FOR ALL REGION PATTERNS',
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
 9011 FORMAT('***** AT THE END       OF DPRPTY--')
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
      WRITE(ICOUT,9015)IDERPT
 9015 FORMAT('IDERPT = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9020)NUMARG
 9020 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO9025I=1,NUMARG
      WRITE(ICOUT,9026)IHARG(I)
 9026 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      WRITE(ICOUT,9030)IREPTY(1)
 9030 FORMAT('IREPTY(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO9035I=1,10
      WRITE(ICOUT,9036)I,IREPTY(I)
 9036 FORMAT('I,IREPTY(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPROLA(ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,
     1                  IA,PARAM,IPARN,IPARN2,
     1                  IWRITE,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--THIS SUBROUTINE DOES THE FOLLOWING:
C
C              1) IT CHECKS THE FILE "DPZCHF.DAT" TO SEE IF THE
C                 SPECIFIED VARIABLE NAME IS FOUND.  IF SO, IT
C                 READS THE CHARCTER DATA STORED IN DPZCHF.DAT
C                 AND SAVES IT IN THE ROWLABEL ARRAY.
C
C              2) IF THE VARIABLE NAME IS NOT FOUND IN THE
C                 CHARACTER DATA LIST, THEN CHECK THE NORMAL
C                 NUMERIC VARIABLE LIST.  IF FOUND, CONVERT THIS
C                 NUMERIC VARIABLE TO ROW LABELS (E.G., THE LAB-ID
C                 MIGHT BE USED AS THE ROW LABEL).
C
C              EXAMPLE:
C                 LET ROWLABEL = IX
C
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
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
C     VERSION NUMBER--2004/1
C     ORIGINAL VERSION--JANUARY   2004.
C     UPDATED         --AUGUST    2012. CHECK FOR NUMERIC VARIABLE
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
      CHARACTER*4 ICASEL
C
      CHARACTER*4 MESSAG
      CHARACTER*4 ICASEQ
      CHARACTER*4 IHWUSE
C
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 IHLEFT
      CHARACTER*4 IHLEF2
      CHARACTER*4 IHRIGH
      CHARACTER*4 IHRIG2
C
      CHARACTER*4 ITYPEH(*)
      CHARACTER*4 IW21HO(*)
      CHARACTER*4 IW22HO(*)
      CHARACTER*4 IA(*)
      CHARACTER*4 IPARN(*)
      CHARACTER*4 IPARN2(*)
      CHARACTER*4 IANGLU
      CHARACTER*4 IBUGCO
      CHARACTER*4 IBUGEV
      CHARACTER*4 ICTEXT(100)
C
      DIMENSION W2HOLD(*)
      DIMENSION PARAM(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOF2.INC'
C
      CHARACTER*80 IFILE
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
      CHARACTER*500 IATEMP
      CHARACTER*10 IFRMT
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPRO'
      ISUBN2='LA  '
C
      IFLAGV=0
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ROLA')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPROLA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO
   52   FORMAT('IBUGA3,ISUBRO = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ********************************************
C               **  STEP 2--                              **
C               **  OPEN THE DPZCHF.DAT FILE.             **
C               ********************************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ROLA')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IHRIGH=IHARG(3)
      IHRIG2=IHARG2(3)
C
      IOUNIT=IZCHNU
      IFILE=IZCHNA
      ISTAT=IZCHST
      IFORM=IZCHFO
      IACCES=IZCHAC
      IPROT=IZCHPR
      ICURST=IZCHCS
C
      ISUBN0='READ'
      IERRFI='NO'
      CALL DPOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,
     1            ICURST,
     1            IREWIN,ISUBN0,IERRFI,IBUGA3,ISUBRO,IERROR)
      IF(IERRFI.EQ.'YES')THEN
        IFLAGV=1
        GOTO8000
CCCCC   IERROR='YES'
CCCCC   WRITE(ICOUT,999)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN DPROLA--')
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,118)
CC118   FORMAT('      UNABLE TO OPEN THE CHARACTER DATA FILE:')
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,119)IFILE
  119   FORMAT('      ',A80)
CCCCC   CALL DPWRST('XXX','BUG ')
        GOTO8000
      ENDIF
C
      READ(IOUNIT,'(I8)',END=171,ERR=171)NUMVAR
C
      DO130I=1,NUMVAR
        READ(IOUNIT,'(A4,A4)',END=181,ERR=181)IH,IH2
        IF(IHRIGH.EQ.IH .AND. IHRIG2.EQ.IH2)THEN
          IVAR=I
          IFLAGV=0
          GOTO199
        ENDIF
  130 CONTINUE
C
      IFLAGV=1
      GOTO8000
C
CCCCC WRITE(ICOUT,999)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,111)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,131)IHRIGH,IHRIG2
CC131 FORMAT('***** VARIABLE ',A4,A4,' NOT FOUND IN THE CHARACTER ',
CCCCC1       'DATA FILE:')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,119)IFILE
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC IERROR='YES'
CCCCC GOTO8000
C
  171 CONTINUE
      IFLAGV=1
      GOTO8000
CCCCC WRITE(ICOUT,999)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,111)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,173)
CC173 FORMAT('      ERROR READING THE NUMBER OF CHARACTER VARIABLES ',
CCCCC1       'IN THE CHARACTER DATA FILE:')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,119)IFILE
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC IERROR='YES'
CCCCC GOTO8000
C
  181 CONTINUE
      IFLAGV=1
      GOTO8000
CCCCC WRITE(ICOUT,999)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,111)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,183)
CC183 FORMAT('      ERROR READING THE VARIABLE NAMES ',
CCCCC1       'IN THE CHARACTER DATA FILE:')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,119)IFILE
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC IERROR='YES'
CCCCC GOTO8000
C
  199 CONTINUE
C
C               *************************************************
C               **  STEP 3--                                   **
C               **  DEFINE THE ROW    LABELS.                  **
C               **  STORE UNIQUE VALUES IN IROWLB.             **
C               *************************************************
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ROLA')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO205I=1,MAXOBV
        IROWLB(I)=' '
  205 CONTINUE
C
      IFRMT='(A   )'
      WRITE(IFRMT(3:5),'(I3)')25*IVAR
      IFRST=(IVAR-1)*25 + 1
      ILAST=IVAR*25 - 1
C
      DO210I=1,MAXOBV
        IATEMP=' '
        READ(IOUNIT,IFRMT,END=499,ERR=491)IATEMP
        IROWLB(I)=IATEMP(IFRST:ILAST)
        IROW=I
  210 CONTINUE
      GOTO499
C
  491 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,493)IROW
  493 FORMAT('      ERROR READING ROW ',I8,' OF THE CHARACTER ',
     1       'VARIABLES IN THE CHARACTER DATA FILE:')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,119)IFILE
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO8000
C
C
C               ******************************
C               **  STEP 3--                **
C               **  WRITE OUT A FEW LINES   **
C               **  OF SUMMARY INFORMATION  **
C               **  ABOUT THE CODING.       **
C               ******************************
C
  499 CONTINUE
C
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,811)IROW
  811   FORMAT('NUMBER OF ROW LABELS CREATED = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,813)IROWLB(1)(1:24)
  813   FORMAT('FIRST ROW LABEL                      = ',A24)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,815)IROW,IROWLB(1)(1:24)
  815   FORMAT('LAST ROW LABEL (',I8,')   = ',A24)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***************************************
C               **  STEP 88--                        **
C               **  CLOSE THE DPZCHF.DAT FILE.       **
C               ***************************************
C
 8000 CONTINUE
C
      IENDFI='OFF'
      IREWIN='ON'
      CALL DPCLFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1            IENDFI,IREWIN,ISUBN0,IERRFI,IBUGA3,ISUBRO,IERROR)
      IZCHCS='CLOSED'
      IF(IFLAGV.EQ.0)GOTO9000
C
C               ********************************************
C               **  STEP 91--                             **
C               **  LOOK FOR THE VARIABLE NAME IN REGULAR **
C               **  NAME TABLE.                           **
C               ********************************************
C
      ISTEPN='91'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ROLA')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IHWUSE='V'
      MESSAG='NO'
      CALL CHECKN(IHRIGH,IHRIG2,IHWUSE,
     1            IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,
     1            NUMNAM,MAXNAM,
     1            ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
      IF(IERROR.EQ.'YES')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,901)
  901   FORMAT('      THE SPECIFIED VARIABLE NAME ON THE RIGHT OF ',
     1         'THE = SIGN')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,903)
  903   FORMAT('      WAS NOT FOUND IN EITHER CHARACTER VARIABLE ',
     1         'NAME LIST')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,905)
  905   FORMAT('      OR IN THE INTERNAL VARIABLE NAME LIST.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      ILIS=ILOCV
      NRIGHT=IN(ILOCV)
      ICOLR=IVALUE(ILOCV)
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
      ICASEQ='FULL'
      ILOCQ=NUMARG+1
      IF(NUMARG.GE.5)THEN
        DO911J=1,NUMARG
          J1=J
          IF(IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ')THEN
            ICASEQ='SUBS'
            ILOCQ=J1
            GOTO916
          ELSEIF(IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  ')THEN
            ICASEQ='SUBS'
            ILOCQ=J1
            GOTO916
          ELSEIF(IHARG(J).EQ.'FOR '.AND.IHARG2(J).EQ.'    ')THEN
            ICASEQ='FOR'
            ILOCQ=J1
            GOTO916
          ENDIF
  911   CONTINUE
      ENDIF
  916 CONTINUE
C
      IF(ICASEQ.EQ.'FULL')THEN
        DO921I=1,NRIGHT
          ISUB(I)=1
  921   CONTINUE
        NQ=NRIGHT
      ELSEIF(ICASEQ.EQ.'SUBS')THEN
        NIOLD=NRIGHT
        CALL DPSUBS(NIOLD,ILOCS,NS,IBUGA3,IERROR)
        NQ=NIOLD
      ELSEIF(ICASEQ.EQ.'FOR')THEN
        NIOLD=NRIGHT
        CALL DPFOR(NIOLD,NFOR,IROW1,IROWN,
     1             NLOCAL,ILOCS,NS,IBUGA3,IERROR)
        NQ=NFOR
      ENDIF
C
      J=0
      IMAX=NRIGHT
      IF(NQ.LT.NRIGHT)IMAX=NQ
      DO960I=1,IMAX
        IF(ISUB(I).EQ.0)GOTO960
        J=J+1
C
        IJ=MAXN*(ICOLR-1)+I
        IF(ICOLR.LE.MAXCOL)AVAL=V(IJ)
        IF(ICOLR.EQ.MAXCP1)AVAL=PRED(I)
        IF(ICOLR.EQ.MAXCP2)AVAL=RES(I)
        IF(ICOLR.EQ.MAXCP3)AVAL=YPLOT(I)
        IF(ICOLR.EQ.MAXCP4)AVAL=XPLOT(I)
        IF(ICOLR.EQ.MAXCP5)AVAL=X2PLOT(I)
        IF(ICOLR.EQ.MAXCP6)AVAL=TAGPLO(I)
C
C       NOW CONVERT ATEMP TO ROW LABEL
C
        IVAL=INT(AVAL+0.5)
        CALL DPCONH(IVAL,AVAL,ICTEXT,NCTEXT,IBUGA3,IERROR)
        IF(NCTEXT.LE.0)THEN
          IROWLB(J)=' '
        ELSE
          IROWLB(J)=' '
          DO965II=1,MIN(24,NCTEXT)
            IROWLB(J)(II:II)=ICTEXT(II)(1:1)
  965     CONTINUE
          IF(IROWLB(J)(NCTEXT:NCTEXT).EQ.'.')THEN
            IROWLB(J)(NCTEXT:NCTEXT)=' '
          ENDIF
        ENDIF
C
  960 CONTINUE
      IROW=J
C
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,811)IROW
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,813)IROWLB(1)(1:24)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,815)IROW,IROWLB(IROW)(1:24)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ROLA')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DPROLA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR
 9012   FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IROW
 9013   FORMAT('IROW = ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(IROW.GT.0)THEN
          DO9015I=1,MIN(IROW,20)
            WRITE(ICOUT,9016)I,IROWLB(I)
 9016       FORMAT('I,IROWLB(I) = ',I8,A24)
            CALL DPWRST('XXX','BUG ')
 9015     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPRSL(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN SIMPLEX LOWER CASE.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
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
      CHARACTER*4 IOPERA
C
C---------------------------------------------------------------------
C
      DIMENSION IOP(*)
      DIMENSION X(*)
      DIMENSION Y(*)
C
      DIMENSION IOPERA(300)
      DIMENSION IX(300)
      DIMENSION IY(300)
C
      DIMENSION IXMIND(30)
      DIMENSION IXMAXD(30)
      DIMENSION IXDELD(30)
      DIMENSION ISTARD(30)
      DIMENSION NUMCOO(30)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----DATA STATEMENTS-------------------------------------------------
C
C     DEFINE CHARACTER    601--LOWER CASE A
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',   6,   5/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',   6,  -9/
      DATA IOPERA(   3),IX(   3),IY(   3)/'MOVE',   6,   2/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',   4,   4/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',   2,   5/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',  -1,   5/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',  -3,   4/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',  -5,   2/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',  -6,  -1/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',  -6,  -3/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',  -5,  -6/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',  -3,  -8/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',  -1,  -9/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',   2,  -9/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',   4,  -8/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',   6,  -6/
C
      DATA IXMIND(   1)/  -9/
      DATA IXMAXD(   1)/  10/
      DATA IXDELD(   1)/  19/
      DATA ISTARD(   1)/   1/
      DATA NUMCOO(   1)/  16/
C
C     DEFINE CHARACTER    602--LOWER CASE B
C
      DATA IOPERA(  17),IX(  17),IY(  17)/'MOVE',  -6,  12/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',  -6,  -9/
      DATA IOPERA(  19),IX(  19),IY(  19)/'MOVE',  -6,   2/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',  -4,   4/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',  -2,   5/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',   1,   5/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',   3,   4/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',   5,   2/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',   6,  -1/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',   6,  -3/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',   5,  -6/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',   3,  -8/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',   1,  -9/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',  -2,  -9/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',  -4,  -8/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',  -6,  -6/
C
      DATA IXMIND(   2)/ -10/
      DATA IXMAXD(   2)/   9/
      DATA IXDELD(   2)/  19/
      DATA ISTARD(   2)/  17/
      DATA NUMCOO(   2)/  16/
C
C     DEFINE CHARACTER    603--LOWER CASE C
C
      DATA IOPERA(  33),IX(  33),IY(  33)/'MOVE',   6,   2/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',   4,   4/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',   2,   5/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',  -1,   5/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',  -3,   4/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',  -5,   2/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',  -6,  -1/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',  -6,  -3/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',  -5,  -6/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',  -3,  -8/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',  -1,  -9/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',   2,  -9/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',   4,  -8/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW',   6,  -6/
C
      DATA IXMIND(   3)/  -9/
      DATA IXMAXD(   3)/   9/
      DATA IXDELD(   3)/  18/
      DATA ISTARD(   3)/  33/
      DATA NUMCOO(   3)/  14/
C
C     DEFINE CHARACTER    604--LOWER CASE D
C
      DATA IOPERA(  47),IX(  47),IY(  47)/'MOVE',   6,  12/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',   6,  -9/
      DATA IOPERA(  49),IX(  49),IY(  49)/'MOVE',   6,   2/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW',   4,   4/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW',   2,   5/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',  -1,   5/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',  -3,   4/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',  -5,   2/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',  -6,  -1/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',  -6,  -3/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',  -5,  -6/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',  -3,  -8/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',  -1,  -9/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW',   2,  -9/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',   4,  -8/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',   6,  -6/
C
      DATA IXMIND(   4)/  -9/
      DATA IXMAXD(   4)/  10/
      DATA IXDELD(   4)/  19/
      DATA ISTARD(   4)/  47/
      DATA NUMCOO(   4)/  16/
C
C     DEFINE CHARACTER    605--LOWER CASE E
C
      DATA IOPERA(  63),IX(  63),IY(  63)/'MOVE',  -6,  -1/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',   6,  -1/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',   6,   1/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW',   5,   3/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',   4,   4/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',   2,   5/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',  -1,   5/
      DATA IOPERA(  70),IX(  70),IY(  70)/'DRAW',  -3,   4/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',  -5,   2/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',  -6,  -1/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',  -6,  -3/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',  -5,  -6/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',  -3,  -8/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',  -1,  -9/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',   2,  -9/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',   4,  -8/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',   6,  -6/
C
      DATA IXMIND(   5)/  -9/
      DATA IXMAXD(   5)/   9/
      DATA IXDELD(   5)/  18/
      DATA ISTARD(   5)/  63/
      DATA NUMCOO(   5)/  17/
C
C     DEFINE CHARACTER    606--LOWER CASE F
C
      DATA IOPERA(  80),IX(  80),IY(  80)/'MOVE',   5,  12/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',   3,  12/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',   1,  11/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',   0,   8/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW',   0,  -9/
      DATA IOPERA(  85),IX(  85),IY(  85)/'MOVE',  -3,   5/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',   4,   5/
C
      DATA IXMIND(   6)/  -5/
      DATA IXMAXD(   6)/   7/
      DATA IXDELD(   6)/  12/
      DATA ISTARD(   6)/  80/
      DATA NUMCOO(   6)/   7/
C
C     DEFINE CHARACTER    607--LOWER CASE G
C
      DATA IOPERA(  87),IX(  87),IY(  87)/'MOVE',   6,   5/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',   6, -11/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',   5, -14/
      DATA IOPERA(  90),IX(  90),IY(  90)/'DRAW',   4, -15/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',   2, -16/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW',  -1, -16/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',  -3, -15/
      DATA IOPERA(  94),IX(  94),IY(  94)/'MOVE',   6,   2/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',   4,   4/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',   2,   5/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',  -1,   5/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',  -3,   4/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',  -5,   2/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',  -6,  -1/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',  -6,  -3/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',  -5,  -6/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',  -3,  -8/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',  -1,  -9/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',   2,  -9/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',   4,  -8/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',   6,  -6/
C
      DATA IXMIND(   7)/  -9/
      DATA IXMAXD(   7)/  10/
      DATA IXDELD(   7)/  19/
      DATA ISTARD(   7)/  87/
      DATA NUMCOO(   7)/  21/
C
C     DEFINE CHARACTER    608--LOWER CASE H
C
      DATA IOPERA( 108),IX( 108),IY( 108)/'MOVE',  -5,  12/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',  -5,  -9/
      DATA IOPERA( 110),IX( 110),IY( 110)/'MOVE',  -5,   1/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',  -2,   4/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',   0,   5/
      DATA IOPERA( 113),IX( 113),IY( 113)/'DRAW',   3,   5/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',   5,   4/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',   6,   1/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',   6,  -9/
C
      DATA IXMIND(   8)/  -9/
      DATA IXMAXD(   8)/  10/
      DATA IXDELD(   8)/  19/
      DATA ISTARD(   8)/ 108/
      DATA NUMCOO(   8)/   9/
C
C     DEFINE CHARACTER    609--LOWER CASE I
C
      DATA IOPERA( 117),IX( 117),IY( 117)/'MOVE',  -1,  12/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',   0,  11/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',   1,  12/
      DATA IOPERA( 120),IX( 120),IY( 120)/'DRAW',   0,  13/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW',  -1,  12/
      DATA IOPERA( 122),IX( 122),IY( 122)/'MOVE',   0,   5/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',   0,  -9/
C
      DATA IXMIND(   9)/  -4/
      DATA IXMAXD(   9)/   4/
      DATA IXDELD(   9)/   8/
      DATA ISTARD(   9)/ 117/
      DATA NUMCOO(   9)/   7/
C
C     DEFINE CHARACTER    610--LOWER CASE J
C
      DATA IOPERA( 124),IX( 124),IY( 124)/'MOVE',   0,  12/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',   1,  11/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW',   2,  12/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW',   1,  13/
      DATA IOPERA( 128),IX( 128),IY( 128)/'DRAW',   0,  12/
      DATA IOPERA( 129),IX( 129),IY( 129)/'MOVE',   1,   5/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',   1, -12/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',   0, -15/
      DATA IOPERA( 132),IX( 132),IY( 132)/'DRAW',  -2, -16/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',  -4, -16/
C
      DATA IXMIND(  10)/  -5/
      DATA IXMAXD(  10)/   5/
      DATA IXDELD(  10)/  10/
      DATA ISTARD(  10)/ 124/
      DATA NUMCOO(  10)/  10/
C
C     DEFINE CHARACTER    611--LOWER CASE K
C
      DATA IOPERA( 134),IX( 134),IY( 134)/'MOVE',  -5,  12/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',  -5,  -9/
      DATA IOPERA( 136),IX( 136),IY( 136)/'MOVE',   5,   5/
      DATA IOPERA( 137),IX( 137),IY( 137)/'DRAW',  -5,  -5/
      DATA IOPERA( 138),IX( 138),IY( 138)/'MOVE',  -1,  -1/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW',   6,  -9/
C
      DATA IXMIND(  11)/  -9/
      DATA IXMAXD(  11)/   8/
      DATA IXDELD(  11)/  17/
      DATA ISTARD(  11)/ 134/
      DATA NUMCOO(  11)/   6/
C
C     DEFINE CHARACTER    612--LOWER CASE L
C
      DATA IOPERA( 140),IX( 140),IY( 140)/'MOVE',   0,  12/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',   0,  -9/
C
      DATA IXMIND(  12)/  -4/
      DATA IXMAXD(  12)/   4/
      DATA IXDELD(  12)/   8/
      DATA ISTARD(  12)/ 140/
      DATA NUMCOO(  12)/   2/
C
C     DEFINE CHARACTER    613--LOWER CASE M
C
      DATA IOPERA( 142),IX( 142),IY( 142)/'MOVE', -11,   5/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW', -11,  -9/
      DATA IOPERA( 144),IX( 144),IY( 144)/'MOVE', -11,   1/
      DATA IOPERA( 145),IX( 145),IY( 145)/'DRAW',  -8,   4/
      DATA IOPERA( 146),IX( 146),IY( 146)/'DRAW',  -6,   5/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',  -3,   5/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',  -1,   4/
      DATA IOPERA( 149),IX( 149),IY( 149)/'DRAW',   0,   1/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',   0,  -9/
      DATA IOPERA( 151),IX( 151),IY( 151)/'MOVE',   0,   1/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',   3,   4/
      DATA IOPERA( 153),IX( 153),IY( 153)/'DRAW',   5,   5/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',   8,   5/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW',  10,   4/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',  11,   1/
      DATA IOPERA( 157),IX( 157),IY( 157)/'DRAW',  11,  -9/
C
      DATA IXMIND(  13)/ -15/
      DATA IXMAXD(  13)/  15/
      DATA IXDELD(  13)/  30/
      DATA ISTARD(  13)/ 142/
      DATA NUMCOO(  13)/  16/
C
C     DEFINE CHARACTER    614--LOWER CASE N
C
      DATA IOPERA( 158),IX( 158),IY( 158)/'MOVE',  -5,   5/
      DATA IOPERA( 159),IX( 159),IY( 159)/'DRAW',  -5,  -9/
      DATA IOPERA( 160),IX( 160),IY( 160)/'MOVE',  -5,   1/
      DATA IOPERA( 161),IX( 161),IY( 161)/'DRAW',  -2,   4/
      DATA IOPERA( 162),IX( 162),IY( 162)/'DRAW',   0,   5/
      DATA IOPERA( 163),IX( 163),IY( 163)/'DRAW',   3,   5/
      DATA IOPERA( 164),IX( 164),IY( 164)/'DRAW',   5,   4/
      DATA IOPERA( 165),IX( 165),IY( 165)/'DRAW',   6,   1/
      DATA IOPERA( 166),IX( 166),IY( 166)/'DRAW',   6,  -9/
C
      DATA IXMIND(  14)/  -9/
      DATA IXMAXD(  14)/  10/
      DATA IXDELD(  14)/  19/
      DATA ISTARD(  14)/ 158/
      DATA NUMCOO(  14)/   9/
C
C     DEFINE CHARACTER    615--LOWER CASE O
C
      DATA IOPERA( 167),IX( 167),IY( 167)/'MOVE',  -1,   5/
      DATA IOPERA( 168),IX( 168),IY( 168)/'DRAW',  -3,   4/
      DATA IOPERA( 169),IX( 169),IY( 169)/'DRAW',  -5,   2/
      DATA IOPERA( 170),IX( 170),IY( 170)/'DRAW',  -6,  -1/
      DATA IOPERA( 171),IX( 171),IY( 171)/'DRAW',  -6,  -3/
      DATA IOPERA( 172),IX( 172),IY( 172)/'DRAW',  -5,  -6/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW',  -3,  -8/
      DATA IOPERA( 174),IX( 174),IY( 174)/'DRAW',  -1,  -9/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW',   2,  -9/
      DATA IOPERA( 176),IX( 176),IY( 176)/'DRAW',   4,  -8/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW',   6,  -6/
      DATA IOPERA( 178),IX( 178),IY( 178)/'DRAW',   7,  -3/
      DATA IOPERA( 179),IX( 179),IY( 179)/'DRAW',   7,  -1/
      DATA IOPERA( 180),IX( 180),IY( 180)/'DRAW',   6,   2/
      DATA IOPERA( 181),IX( 181),IY( 181)/'DRAW',   4,   4/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',   2,   5/
      DATA IOPERA( 183),IX( 183),IY( 183)/'DRAW',  -1,   5/
C
      DATA IXMIND(  15)/  -9/
      DATA IXMAXD(  15)/  10/
      DATA IXDELD(  15)/  19/
      DATA ISTARD(  15)/ 167/
      DATA NUMCOO(  15)/  17/
C
C     DEFINE CHARACTER    616--LOWER CASE P
C
      DATA IOPERA( 184),IX( 184),IY( 184)/'MOVE',  -6,   5/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',  -6, -16/
      DATA IOPERA( 186),IX( 186),IY( 186)/'MOVE',  -6,   2/
      DATA IOPERA( 187),IX( 187),IY( 187)/'DRAW',  -4,   4/
      DATA IOPERA( 188),IX( 188),IY( 188)/'DRAW',  -2,   5/
      DATA IOPERA( 189),IX( 189),IY( 189)/'DRAW',   1,   5/
      DATA IOPERA( 190),IX( 190),IY( 190)/'DRAW',   3,   4/
      DATA IOPERA( 191),IX( 191),IY( 191)/'DRAW',   5,   2/
      DATA IOPERA( 192),IX( 192),IY( 192)/'DRAW',   6,  -1/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW',   6,  -3/
      DATA IOPERA( 194),IX( 194),IY( 194)/'DRAW',   5,  -6/
      DATA IOPERA( 195),IX( 195),IY( 195)/'DRAW',   3,  -8/
      DATA IOPERA( 196),IX( 196),IY( 196)/'DRAW',   1,  -9/
      DATA IOPERA( 197),IX( 197),IY( 197)/'DRAW',  -2,  -9/
      DATA IOPERA( 198),IX( 198),IY( 198)/'DRAW',  -4,  -8/
      DATA IOPERA( 199),IX( 199),IY( 199)/'DRAW',  -6,  -6/
C
      DATA IXMIND(  16)/ -10/
      DATA IXMAXD(  16)/   9/
      DATA IXDELD(  16)/  19/
      DATA ISTARD(  16)/ 184/
      DATA NUMCOO(  16)/  16/
C
C     DEFINE CHARACTER    617--LOWER CASE Q
C
      DATA IOPERA( 200),IX( 200),IY( 200)/'MOVE',   6,   5/
      DATA IOPERA( 201),IX( 201),IY( 201)/'DRAW',   6, -16/
      DATA IOPERA( 202),IX( 202),IY( 202)/'MOVE',   6,   2/
      DATA IOPERA( 203),IX( 203),IY( 203)/'DRAW',   4,   4/
      DATA IOPERA( 204),IX( 204),IY( 204)/'DRAW',   2,   5/
      DATA IOPERA( 205),IX( 205),IY( 205)/'DRAW',  -1,   5/
      DATA IOPERA( 206),IX( 206),IY( 206)/'DRAW',  -3,   4/
      DATA IOPERA( 207),IX( 207),IY( 207)/'DRAW',  -5,   2/
      DATA IOPERA( 208),IX( 208),IY( 208)/'DRAW',  -6,  -1/
      DATA IOPERA( 209),IX( 209),IY( 209)/'DRAW',  -6,  -3/
      DATA IOPERA( 210),IX( 210),IY( 210)/'DRAW',  -5,  -6/
      DATA IOPERA( 211),IX( 211),IY( 211)/'DRAW',  -3,  -8/
      DATA IOPERA( 212),IX( 212),IY( 212)/'DRAW',  -1,  -9/
      DATA IOPERA( 213),IX( 213),IY( 213)/'DRAW',   2,  -9/
      DATA IOPERA( 214),IX( 214),IY( 214)/'DRAW',   4,  -8/
      DATA IOPERA( 215),IX( 215),IY( 215)/'DRAW',   6,  -6/
C
      DATA IXMIND(  17)/  -9/
      DATA IXMAXD(  17)/  10/
      DATA IXDELD(  17)/  19/
      DATA ISTARD(  17)/ 200/
      DATA NUMCOO(  17)/  16/
C
C     DEFINE CHARACTER    618--LOWER CASE R
C
      DATA IOPERA( 216),IX( 216),IY( 216)/'MOVE',  -3,   5/
      DATA IOPERA( 217),IX( 217),IY( 217)/'DRAW',  -3,  -9/
      DATA IOPERA( 218),IX( 218),IY( 218)/'MOVE',  -3,  -1/
      DATA IOPERA( 219),IX( 219),IY( 219)/'DRAW',  -2,   2/
      DATA IOPERA( 220),IX( 220),IY( 220)/'DRAW',   0,   4/
      DATA IOPERA( 221),IX( 221),IY( 221)/'DRAW',   2,   5/
      DATA IOPERA( 222),IX( 222),IY( 222)/'DRAW',   5,   5/
C
      DATA IXMIND(  18)/  -7/
      DATA IXMAXD(  18)/   6/
      DATA IXDELD(  18)/  13/
      DATA ISTARD(  18)/ 216/
      DATA NUMCOO(  18)/   7/
C
C     DEFINE CHARACTER    619--LOWER CASE S
C
      DATA IOPERA( 223),IX( 223),IY( 223)/'MOVE',   6,   2/
      DATA IOPERA( 224),IX( 224),IY( 224)/'DRAW',   5,   4/
      DATA IOPERA( 225),IX( 225),IY( 225)/'DRAW',   2,   5/
      DATA IOPERA( 226),IX( 226),IY( 226)/'DRAW',  -1,   5/
      DATA IOPERA( 227),IX( 227),IY( 227)/'DRAW',  -4,   4/
      DATA IOPERA( 228),IX( 228),IY( 228)/'DRAW',  -5,   2/
      DATA IOPERA( 229),IX( 229),IY( 229)/'DRAW',  -4,   0/
      DATA IOPERA( 230),IX( 230),IY( 230)/'DRAW',  -2,  -1/
      DATA IOPERA( 231),IX( 231),IY( 231)/'DRAW',   3,  -2/
      DATA IOPERA( 232),IX( 232),IY( 232)/'DRAW',   5,  -3/
      DATA IOPERA( 233),IX( 233),IY( 233)/'DRAW',   6,  -5/
      DATA IOPERA( 234),IX( 234),IY( 234)/'DRAW',   6,  -6/
      DATA IOPERA( 235),IX( 235),IY( 235)/'DRAW',   5,  -8/
      DATA IOPERA( 236),IX( 236),IY( 236)/'DRAW',   2,  -9/
      DATA IOPERA( 237),IX( 237),IY( 237)/'DRAW',  -1,  -9/
      DATA IOPERA( 238),IX( 238),IY( 238)/'DRAW',  -4,  -8/
      DATA IOPERA( 239),IX( 239),IY( 239)/'DRAW',  -5,  -6/
C
      DATA IXMIND(  19)/  -8/
      DATA IXMAXD(  19)/   9/
      DATA IXDELD(  19)/  17/
      DATA ISTARD(  19)/ 223/
      DATA NUMCOO(  19)/  17/
C
C     DEFINE CHARACTER    620--LOWER CASE T
C
      DATA IOPERA( 240),IX( 240),IY( 240)/'MOVE',   0,  12/
      DATA IOPERA( 241),IX( 241),IY( 241)/'DRAW',   0,  -5/
      DATA IOPERA( 242),IX( 242),IY( 242)/'DRAW',   1,  -8/
      DATA IOPERA( 243),IX( 243),IY( 243)/'DRAW',   3,  -9/
      DATA IOPERA( 244),IX( 244),IY( 244)/'DRAW',   5,  -9/
      DATA IOPERA( 245),IX( 245),IY( 245)/'MOVE',  -3,   5/
      DATA IOPERA( 246),IX( 246),IY( 246)/'DRAW',   4,   5/
C
      DATA IXMIND(  20)/  -5/
      DATA IXMAXD(  20)/   7/
      DATA IXDELD(  20)/  12/
      DATA ISTARD(  20)/ 240/
      DATA NUMCOO(  20)/   7/
C
C     DEFINE CHARACTER    621--LOWER CASE U
C
      DATA IOPERA( 247),IX( 247),IY( 247)/'MOVE',  -5,   5/
      DATA IOPERA( 248),IX( 248),IY( 248)/'DRAW',  -5,  -5/
      DATA IOPERA( 249),IX( 249),IY( 249)/'DRAW',  -4,  -8/
      DATA IOPERA( 250),IX( 250),IY( 250)/'DRAW',  -2,  -9/
      DATA IOPERA( 251),IX( 251),IY( 251)/'DRAW',   1,  -9/
      DATA IOPERA( 252),IX( 252),IY( 252)/'DRAW',   3,  -8/
      DATA IOPERA( 253),IX( 253),IY( 253)/'DRAW',   6,  -5/
      DATA IOPERA( 254),IX( 254),IY( 254)/'MOVE',   6,   5/
      DATA IOPERA( 255),IX( 255),IY( 255)/'DRAW',   6,  -9/
C
      DATA IXMIND(  21)/  -9/
      DATA IXMAXD(  21)/  10/
      DATA IXDELD(  21)/  19/
      DATA ISTARD(  21)/ 247/
      DATA NUMCOO(  21)/   9/
C
C     DEFINE CHARACTER    622--LOWER CASE V
C
      DATA IOPERA( 256),IX( 256),IY( 256)/'MOVE',  -6,   5/
      DATA IOPERA( 257),IX( 257),IY( 257)/'DRAW',   0,  -9/
      DATA IOPERA( 258),IX( 258),IY( 258)/'MOVE',   6,   5/
      DATA IOPERA( 259),IX( 259),IY( 259)/'DRAW',   0,  -9/
C
      DATA IXMIND(  22)/  -8/
      DATA IXMAXD(  22)/   8/
      DATA IXDELD(  22)/  16/
      DATA ISTARD(  22)/ 256/
      DATA NUMCOO(  22)/   4/
C
C     DEFINE CHARACTER    623--LOWER CASE W
C
      DATA IOPERA( 260),IX( 260),IY( 260)/'MOVE',  -8,   5/
      DATA IOPERA( 261),IX( 261),IY( 261)/'DRAW',  -4,  -9/
      DATA IOPERA( 262),IX( 262),IY( 262)/'MOVE',   0,   5/
      DATA IOPERA( 263),IX( 263),IY( 263)/'DRAW',  -4,  -9/
      DATA IOPERA( 264),IX( 264),IY( 264)/'MOVE',   0,   5/
      DATA IOPERA( 265),IX( 265),IY( 265)/'DRAW',   4,  -9/
      DATA IOPERA( 266),IX( 266),IY( 266)/'MOVE',   8,   5/
      DATA IOPERA( 267),IX( 267),IY( 267)/'DRAW',   4,  -9/
C
      DATA IXMIND(  23)/ -11/
      DATA IXMAXD(  23)/  11/
      DATA IXDELD(  23)/  22/
      DATA ISTARD(  23)/ 260/
      DATA NUMCOO(  23)/   8/
C
C     DEFINE CHARACTER    624--LOWER CASE X
C
      DATA IOPERA( 268),IX( 268),IY( 268)/'MOVE',  -5,   5/
      DATA IOPERA( 269),IX( 269),IY( 269)/'DRAW',   6,  -9/
      DATA IOPERA( 270),IX( 270),IY( 270)/'MOVE',   6,   5/
      DATA IOPERA( 271),IX( 271),IY( 271)/'DRAW',  -5,  -9/
C
      DATA IXMIND(  24)/  -8/
      DATA IXMAXD(  24)/   9/
      DATA IXDELD(  24)/  17/
      DATA ISTARD(  24)/ 268/
      DATA NUMCOO(  24)/   4/
C
C     DEFINE CHARACTER    625--LOWER CASE Y
C
      DATA IOPERA( 272),IX( 272),IY( 272)/'MOVE',  -6,   5/
      DATA IOPERA( 273),IX( 273),IY( 273)/'DRAW',   0,  -9/
      DATA IOPERA( 274),IX( 274),IY( 274)/'MOVE',   6,   5/
      DATA IOPERA( 275),IX( 275),IY( 275)/'DRAW',   0,  -9/
      DATA IOPERA( 276),IX( 276),IY( 276)/'DRAW',  -2, -13/
      DATA IOPERA( 277),IX( 277),IY( 277)/'DRAW',  -4, -15/
      DATA IOPERA( 278),IX( 278),IY( 278)/'DRAW',  -6, -16/
      DATA IOPERA( 279),IX( 279),IY( 279)/'DRAW',  -7, -16/
C
      DATA IXMIND(  25)/  -8/
      DATA IXMAXD(  25)/   8/
      DATA IXDELD(  25)/  16/
      DATA ISTARD(  25)/ 272/
      DATA NUMCOO(  25)/   8/
C
C     DEFINE CHARACTER    626--LOWER CASE Z
C
      DATA IOPERA( 280),IX( 280),IY( 280)/'MOVE',   6,   5/
      DATA IOPERA( 281),IX( 281),IY( 281)/'DRAW',  -5,  -9/
      DATA IOPERA( 282),IX( 282),IY( 282)/'MOVE',  -5,   5/
      DATA IOPERA( 283),IX( 283),IY( 283)/'DRAW',   6,   5/
      DATA IOPERA( 284),IX( 284),IY( 284)/'MOVE',  -5,  -9/
      DATA IOPERA( 285),IX( 285),IY( 285)/'DRAW',   6,  -9/
C
      DATA IXMIND(  26)/  -8/
      DATA IXMAXD(  26)/   9/
      DATA IXDELD(  26)/  17/
      DATA ISTARD(  26)/ 280/
      DATA NUMCOO(  26)/   6/
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
C               ******************************************
C               **  TREAT THE ROMAN SIMPLEX UPPER CASE  **
C               **  HERSHEY CHARACTER SET CASE          **
C               ******************************************
C               ******************************************
C
C
      IF(IBUGD2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPRSL--')
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
C               **************************************************
C               **  STEP 1--                                    **
C               **  SEARCH FOR THE INPUT CHARACTER(S).          **
C               **  MAP THE CHARACTER(S) INTO A NUMERIC VALUE.  **
C               **************************************************
C               **************************************************
C
      CALL DPCHAL(ICHAR2,ICHARN,IBUGD2,IFOUND)
      IF(IFOUND.EQ.'NO')GOTO9000
      GOTO1000
C
C               **************************************
C               **************************************
C               **  STEP 2--                        **
C               **  EXTRACT THE COORDINATES         **
C               **  FOR THIS PARTICULAR CHARACTER.  **
C               **************************************
C               **************************************
C
 1000 CONTINUE
      ISTART=ISTARD(ICHARN)
      NC=NUMCOO(ICHARN)
      ISTOP=ISTART+NC-1
      J=0
      DO1100I=ISTART,ISTOP
      J=J+1
      IOP(J)=IOPERA(I)
      X(J)=IX(I)
      Y(J)=IY(I)
 1100 CONTINUE
      NUMCO=J
      IXMINS=IXMIND(ICHARN)
      IXMAXS=IXMAXD(ICHARN)
      IXDELS=IXDELD(ICHARN)
C
      GOTO9000
C
C               *****************
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPRSL--')
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
      SUBROUTINE DPRSN(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN SIMPLEX NUMERIC.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
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
      CHARACTER*4 IOPERA
C
C---------------------------------------------------------------------
C
      DIMENSION IOP(*)
      DIMENSION X(*)
      DIMENSION Y(*)
C
      DIMENSION IOPERA(300)
      DIMENSION IX(300)
      DIMENSION IY(300)
C
      DIMENSION IXMIND(30)
      DIMENSION IXMAXD(30)
      DIMENSION IXDELD(30)
      DIMENSION ISTARD(30)
      DIMENSION NUMCOO(30)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----DATA STATEMENTS-------------------------------------------------
C
C     DEFINE CHARACTER    700--0
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',  -1,  12/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',  -4,  11/
      DATA IOPERA(   3),IX(   3),IY(   3)/'DRAW',  -6,   8/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',  -7,   3/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',  -7,   0/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',  -6,  -5/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',  -4,  -8/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',  -1,  -9/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',   1,  -9/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',   4,  -8/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',   6,  -5/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',   7,   0/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',   7,   3/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',   6,   8/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',   4,  11/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',   1,  12/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',  -1,  12/
C
      DATA IXMIND(   1)/ -10/
      DATA IXMAXD(   1)/  10/
      DATA IXDELD(   1)/  20/
      DATA ISTARD(   1)/   1/
      DATA NUMCOO(   1)/  17/
C
C     DEFINE CHARACTER    701--1
C
      DATA IOPERA(  18),IX(  18),IY(  18)/'MOVE',  -4,   8/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',  -2,   9/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',   1,  12/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',   1,  -9/
C
      DATA IXMIND(   2)/ -10/
      DATA IXMAXD(   2)/  10/
      DATA IXDELD(   2)/  20/
      DATA ISTARD(   2)/  18/
      DATA NUMCOO(   2)/   4/
C
C     DEFINE CHARACTER    702--2
C
      DATA IOPERA(  22),IX(  22),IY(  22)/'MOVE',  -6,   7/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',  -6,   8/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',  -5,  10/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',  -4,  11/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',  -2,  12/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',   2,  12/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',   4,  11/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',   5,  10/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',   6,   8/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',   6,   6/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',   5,   4/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',   3,   1/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',  -7,  -9/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',   7,  -9/
C
      DATA IXMIND(   3)/ -10/
      DATA IXMAXD(   3)/  10/
      DATA IXDELD(   3)/  20/
      DATA ISTARD(   3)/  22/
      DATA NUMCOO(   3)/  14/
C
C     DEFINE CHARACTER    703--3
C
      DATA IOPERA(  36),IX(  36),IY(  36)/'MOVE',  -5,  12/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',   6,  12/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',   0,   4/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',   3,   4/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',   5,   3/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',   6,   2/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',   7,  -1/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',   7,  -3/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',   6,  -6/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',   4,  -8/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW',   1,  -9/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',  -2,  -9/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',  -5,  -8/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',  -6,  -7/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW',  -7,  -5/
C
      DATA IXMIND(   4)/ -10/
      DATA IXMAXD(   4)/  10/
      DATA IXDELD(   4)/  20/
      DATA ISTARD(   4)/  36/
      DATA NUMCOO(   4)/  15/
C
C     DEFINE CHARACTER    704--4
C
      DATA IOPERA(  51),IX(  51),IY(  51)/'MOVE',   3,  12/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',  -7,  -2/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',   8,  -2/
      DATA IOPERA(  54),IX(  54),IY(  54)/'MOVE',   3,  12/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',   3,  -9/
C
      DATA IXMIND(   5)/ -10/
      DATA IXMAXD(   5)/  10/
      DATA IXDELD(   5)/  20/
      DATA ISTARD(   5)/  51/
      DATA NUMCOO(   5)/   5/
C
C     DEFINE CHARACTER    705--5
C
      DATA IOPERA(  56),IX(  56),IY(  56)/'MOVE',   5,  12/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',  -5,  12/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',  -6,   3/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',  -5,   4/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW',  -2,   5/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',   1,   5/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',   4,   4/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',   6,   2/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',   7,  -1/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',   7,  -3/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW',   6,  -6/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',   4,  -8/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',   1,  -9/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',  -2,  -9/
      DATA IOPERA(  70),IX(  70),IY(  70)/'DRAW',  -5,  -8/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',  -6,  -7/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',  -7,  -5/
C
      DATA IXMIND(   6)/ -10/
      DATA IXMAXD(   6)/  10/
      DATA IXDELD(   6)/  20/
      DATA ISTARD(   6)/  56/
      DATA NUMCOO(   6)/  17/
C
C     DEFINE CHARACTER    706--6
C
      DATA IOPERA(  73),IX(  73),IY(  73)/'MOVE',   6,   9/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',   5,  11/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',   2,  12/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',   0,  12/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',  -3,  11/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',  -5,   8/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',  -6,   3/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',  -6,  -2/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',  -5,  -6/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',  -3,  -8/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',   0,  -9/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW',   1,  -9/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',   4,  -8/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',   6,  -6/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',   7,  -3/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',   7,  -2/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',   6,   1/
      DATA IOPERA(  90),IX(  90),IY(  90)/'DRAW',   4,   3/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',   1,   4/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW',   0,   4/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',  -3,   3/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',  -5,   1/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',  -6,  -2/
C
      DATA IXMIND(   7)/ -10/
      DATA IXMAXD(   7)/  10/
      DATA IXDELD(   7)/  20/
      DATA ISTARD(   7)/  73/
      DATA NUMCOO(   7)/  23/
C
C     DEFINE CHARACTER    707--7
C
      DATA IOPERA(  96),IX(  96),IY(  96)/'MOVE',   7,  12/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',  -3,  -9/
      DATA IOPERA(  98),IX(  98),IY(  98)/'MOVE',  -7,  12/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',   7,  12/
C
      DATA IXMIND(   8)/ -10/
      DATA IXMAXD(   8)/  10/
      DATA IXDELD(   8)/  20/
      DATA ISTARD(   8)/  96/
      DATA NUMCOO(   8)/   4/
C
C     DEFINE CHARACTER    708--8
C
      DATA IOPERA( 100),IX( 100),IY( 100)/'MOVE',  -2,  12/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',  -5,  11/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',  -6,   9/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',  -6,   7/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',  -5,   5/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',  -3,   4/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',   1,   3/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',   4,   2/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',   6,   0/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',   7,  -2/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',   7,  -5/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',   6,  -7/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',   5,  -8/
      DATA IOPERA( 113),IX( 113),IY( 113)/'DRAW',   2,  -9/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',  -2,  -9/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',  -5,  -8/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',  -6,  -7/
      DATA IOPERA( 117),IX( 117),IY( 117)/'DRAW',  -7,  -5/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',  -7,  -2/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',  -6,   0/
      DATA IOPERA( 120),IX( 120),IY( 120)/'DRAW',  -4,   2/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW',  -1,   3/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW',   3,   4/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',   5,   5/
      DATA IOPERA( 124),IX( 124),IY( 124)/'DRAW',   6,   7/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',   6,   9/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW',   5,  11/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW',   2,  12/
      DATA IOPERA( 128),IX( 128),IY( 128)/'DRAW',  -2,  12/
C
      DATA IXMIND(   9)/ -10/
      DATA IXMAXD(   9)/  10/
      DATA IXDELD(   9)/  20/
      DATA ISTARD(   9)/ 100/
      DATA NUMCOO(   9)/  29/
C
C     DEFINE CHARACTER    709--9
C
      DATA IOPERA( 129),IX( 129),IY( 129)/'MOVE',   6,   5/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',   5,   2/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',   3,   0/
      DATA IOPERA( 132),IX( 132),IY( 132)/'DRAW',   0,  -1/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',  -1,  -1/
      DATA IOPERA( 134),IX( 134),IY( 134)/'DRAW',  -4,   0/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',  -6,   2/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',  -7,   5/
      DATA IOPERA( 137),IX( 137),IY( 137)/'DRAW',  -7,   6/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW',  -6,   9/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW',  -4,  11/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW',  -1,  12/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',   0,  12/
      DATA IOPERA( 142),IX( 142),IY( 142)/'DRAW',   3,  11/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',   5,   9/
      DATA IOPERA( 144),IX( 144),IY( 144)/'DRAW',   6,   5/
      DATA IOPERA( 145),IX( 145),IY( 145)/'DRAW',   6,   0/
      DATA IOPERA( 146),IX( 146),IY( 146)/'DRAW',   5,  -5/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',   3,  -8/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',   0,  -9/
      DATA IOPERA( 149),IX( 149),IY( 149)/'DRAW',  -2,  -9/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',  -5,  -8/
      DATA IOPERA( 151),IX( 151),IY( 151)/'DRAW',  -6,  -6/
C
      DATA IXMIND(  10)/ -10/
      DATA IXMAXD(  10)/  10/
      DATA IXDELD(  10)/  20/
      DATA ISTARD(  10)/ 129/
      DATA NUMCOO(  10)/  23/
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
C               ******************************************
C               **  TREAT THE ROMAN SIMPLEX UPPER CASE  **
C               **  HERSHEY CHARACTER SET CASE          **
C               ******************************************
C               ******************************************
C
C
      IF(IBUGD2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPRSN--')
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
C               **************************************************
C               **  STEP 1--                                    **
C               **  SEARCH FOR THE INPUT CHARACTER(S).          **
C               **  MAP THE CHARACTER(S) INTO A NUMERIC VALUE.  **
C               **************************************************
C               **************************************************
C
      CALL DPCHNU(ICHAR2,ICHARN,IBUGD2,IFOUND)
      IF(IFOUND.EQ.'NO')GOTO9000
      GOTO1000
C
C               **************************************
C               **************************************
C               **  STEP 2--                        **
C               **  EXTRACT THE COORDINATES         **
C               **  FOR THIS PARTICULAR CHARACTER.  **
C               **************************************
C               **************************************
C
 1000 CONTINUE
      ISTART=ISTARD(ICHARN)
      NC=NUMCOO(ICHARN)
      ISTOP=ISTART+NC-1
      J=0
      DO1100I=ISTART,ISTOP
      J=J+1
      IOP(J)=IOPERA(I)
      X(J)=IX(I)
      Y(J)=IY(I)
 1100 CONTINUE
      NUMCO=J
      IXMINS=IXMIND(ICHARN)
      IXMAXS=IXMAXD(ICHARN)
      IXDELS=IXDELD(ICHARN)
C
      GOTO9000
C
C               *****************
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPRSN--')
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
      SUBROUTINE DPRSS(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN SIMPLEX SYMBOLS.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--87/4
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MARCH     1981.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --MARCH     1987.
C     UPDATED         --MAY       1987.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICHAR2
      CHARACTER*4 IOP
      CHARACTER*4 IBUGD2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IOPERA
C
C---------------------------------------------------------------------
C
      DIMENSION IOP(*)
      DIMENSION X(*)
      DIMENSION Y(*)
C
      DIMENSION IOPERA(300)
      DIMENSION IX(300)
      DIMENSION IY(300)
C
      DIMENSION IXMIND(30)
      DIMENSION IXMAXD(30)
      DIMENSION IXDELD(30)
      DIMENSION ISTARD(30)
      DIMENSION NUMCOO(30)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----DATA STATEMENTS-------------------------------------------------
C
C     DEFINE CHARACTER    710--. (PERIOD)
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',   0,  -7/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',  -1,  -8/
      DATA IOPERA(   3),IX(   3),IY(   3)/'DRAW',   0,  -9/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',   1,  -8/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',   0,  -7/
C
      DATA IXMIND(   1)/  -5/
      DATA IXMAXD(   1)/   5/
      DATA IXDELD(   1)/  10/
      DATA ISTARD(   1)/   1/
      DATA NUMCOO(   1)/   5/
C
C     DEFINE CHARACTER    711--, (COMMA)
C
      DATA IOPERA(   6),IX(   6),IY(   6)/'MOVE',   1,  -8/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',   0,  -9/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',  -1,  -8/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',   0,  -7/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',   1,  -8/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',   1, -10/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',   0, -12/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',  -1, -13/
C
      DATA IXMIND(   2)/  -5/
      DATA IXMAXD(   2)/   5/
      DATA IXDELD(   2)/  10/
      DATA ISTARD(   2)/   6/
      DATA NUMCOO(   2)/   8/
C
C     DEFINE CHARACTER    712--: (COLON)
C
      DATA IOPERA(  14),IX(  14),IY(  14)/'MOVE',   0,   5/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',  -1,   4/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',   0,   3/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',   1,   4/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',   0,   5/
      DATA IOPERA(  19),IX(  19),IY(  19)/'MOVE',   0,  -7/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',  -1,  -8/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',   0,  -9/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',   1,  -8/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',   0,  -7/
C
      DATA IXMIND(   3)/  -5/
      DATA IXMAXD(   3)/   5/
      DATA IXDELD(   3)/  10/
      DATA ISTARD(   3)/  14/
      DATA NUMCOO(   3)/  10/
C
C     DEFINE CHARACTER    713--; (SEMICOLON)
C
      DATA IOPERA(  24),IX(  24),IY(  24)/'MOVE',   0,   5/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',  -1,   4/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',   0,   3/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',   1,   4/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',   0,   5/
      DATA IOPERA(  29),IX(  29),IY(  29)/'MOVE',   1,  -8/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',   0,  -9/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',  -1,  -8/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',   0,  -7/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',   1,  -8/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',   1, -10/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',   0, -12/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',  -1, -13/
C
      DATA IXMIND(   4)/  -5/
      DATA IXMAXD(   4)/   5/
      DATA IXDELD(   4)/  10/
      DATA ISTARD(   4)/  24/
      DATA NUMCOO(   4)/  13/
C
C     DEFINE CHARACTER    714--! (EXCLAMATION POINT)
C
      DATA IOPERA(  37),IX(  37),IY(  37)/'MOVE',   0,  12/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',   0,  -2/
      DATA IOPERA(  39),IX(  39),IY(  39)/'MOVE',   0,  -7/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',  -1,  -8/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',   0,  -9/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',   1,  -8/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',   0,  -7/
C
      DATA IXMIND(   5)/  -5/
      DATA IXMAXD(   5)/   5/
      DATA IXDELD(   5)/  10/
      DATA ISTARD(   5)/  37/
      DATA NUMCOO(   5)/   7/
C
C     DEFINE CHARACTER    715--? (QUESTION MARK)
C
      DATA IOPERA(  44),IX(  44),IY(  44)/'MOVE',  -6,   7/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',  -6,   8/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW',  -5,  10/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',  -4,  11/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',  -2,  12/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',   2,  12/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW',   4,  11/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW',   5,  10/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',   6,   8/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',   6,   6/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',   5,   4/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',   4,   3/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',   0,   1/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',   0,  -2/
      DATA IOPERA(  58),IX(  58),IY(  58)/'MOVE',   0,  -7/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',  -1,  -8/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW',   0,  -9/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',   1,  -8/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',   0,  -7/
C
      DATA IXMIND(   6)/  -9/
      DATA IXMAXD(   6)/   9/
      DATA IXDELD(   6)/  18/
      DATA ISTARD(   6)/  44/
      DATA NUMCOO(   6)/  19/
C
C     DEFINE CHARACTER    734--& (AMPERSAND)
C
      DATA IOPERA(  63),IX(  63),IY(  63)/'MOVE',  10,   3/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',  10,   4/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',   9,   5/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW',   8,   5/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',   7,   4/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',   6,   2/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',   4,  -3/
      DATA IOPERA(  70),IX(  70),IY(  70)/'DRAW',   2,  -6/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',   0,  -8/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',  -2,  -9/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',  -6,  -9/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',  -8,  -8/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',  -9,  -7/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW', -10,  -5/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW', -10,  -3/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',  -9,  -1/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',  -8,   0/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',  -1,   4/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',   0,   5/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',   1,   7/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',   1,   9/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW',   0,  11/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',  -2,  12/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',  -4,  11/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',  -5,   9/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',  -5,   7/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',  -4,   4/
      DATA IOPERA(  90),IX(  90),IY(  90)/'DRAW',  -2,   1/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',   3,  -6/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW',   5,  -8/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',   7,  -9/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',   9,  -9/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',  10,  -8/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',  10,  -7/
C
      DATA IXMIND(   7)/ -13/
      DATA IXMAXD(   7)/  13/
      DATA IXDELD(   7)/  26/
      DATA ISTARD(   7)/  63/
      DATA NUMCOO(   7)/  34/
C
C     DEFINE CHARACTER    719--$ (DOLLAR SIGN)
C
      DATA IOPERA(  97),IX(  97),IY(  97)/'MOVE',  -2,  16/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',  -2, -13/
      DATA IOPERA(  99),IX(  99),IY(  99)/'MOVE',   2,  16/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',   2, -13/
      DATA IOPERA( 101),IX( 101),IY( 101)/'MOVE',   7,   9/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',   5,  11/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',   2,  12/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',  -2,  12/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',  -5,  11/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',  -7,   9/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',  -7,   7/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',  -6,   5/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',  -5,   4/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',  -3,   3/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',   3,   1/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',   5,   0/
      DATA IOPERA( 113),IX( 113),IY( 113)/'DRAW',   6,  -1/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',   7,  -3/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',   7,  -6/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',   5,  -8/
      DATA IOPERA( 117),IX( 117),IY( 117)/'DRAW',   2,  -9/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',  -2,  -9/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',  -5,  -8/
      DATA IOPERA( 120),IX( 120),IY( 120)/'DRAW',  -7,  -6/
C
      DATA IXMIND(   8)/ -10/
      DATA IXMAXD(   8)/  10/
      DATA IXDELD(   8)/  20/
      DATA ISTARD(   8)/  97/
      DATA NUMCOO(   8)/  24/
C
C     DEFINE CHARACTER    720--/ (SLASH)
C
      DATA IOPERA( 121),IX( 121),IY( 121)/'MOVE',   9,  16/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW',  -9, -16/
C
      DATA IXMIND(   9)/ -11/
      DATA IXMAXD(   9)/  11/
      DATA IXDELD(   9)/  22/
      DATA ISTARD(   9)/ 121/
      DATA NUMCOO(   9)/   2/
C
C     DEFINE CHARACTER    721--( (LEFT PARENTHESES)
C
      DATA IOPERA( 123),IX( 123),IY( 123)/'MOVE',   4,  16/
      DATA IOPERA( 124),IX( 124),IY( 124)/'DRAW',   2,  14/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',   0,  11/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW',  -2,   7/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW',  -3,   2/
      DATA IOPERA( 128),IX( 128),IY( 128)/'DRAW',  -3,  -2/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',  -2,  -7/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',   0, -11/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',   2, -14/
      DATA IOPERA( 132),IX( 132),IY( 132)/'DRAW',   4, -16/
C
      DATA IXMIND(  10)/  -7/
      DATA IXMAXD(  10)/   7/
      DATA IXDELD(  10)/  14/
      DATA ISTARD(  10)/ 123/
      DATA NUMCOO(  10)/  10/
C
C     DEFINE CHARACTER    722--) (RIGHT PARENTHESES)
C
      DATA IOPERA( 133),IX( 133),IY( 133)/'MOVE',  -4,  16/
      DATA IOPERA( 134),IX( 134),IY( 134)/'DRAW',  -2,  14/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',   0,  11/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',   2,   7/
      DATA IOPERA( 137),IX( 137),IY( 137)/'DRAW',   3,   2/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW',   3,  -2/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW',   2,  -7/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW',   0, -11/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',  -2, -14/
      DATA IOPERA( 142),IX( 142),IY( 142)/'DRAW',  -4, -16/
C
      DATA IXMIND(  11)/  -7/
      DATA IXMAXD(  11)/   7/
      DATA IXDELD(  11)/  14/
      DATA ISTARD(  11)/ 133/
      DATA NUMCOO(  11)/  10/
C
C     DEFINE CHARACTER    728--* (ASTERISK)
C
      DATA IOPERA( 143),IX( 143),IY( 143)/'MOVE',   0,   6/
      DATA IOPERA( 144),IX( 144),IY( 144)/'DRAW',   0,  -6/
      DATA IOPERA( 145),IX( 145),IY( 145)/'MOVE',  -5,   3/
      DATA IOPERA( 146),IX( 146),IY( 146)/'DRAW',   5,  -3/
      DATA IOPERA( 147),IX( 147),IY( 147)/'MOVE',   5,   3/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',  -5,  -3/
C
      DATA IXMIND(  12)/  -8/
      DATA IXMAXD(  12)/   8/
      DATA IXDELD(  12)/  16/
      DATA ISTARD(  12)/ 143/
      DATA NUMCOO(  12)/   6/
C
C     DEFINE CHARACTER    724--- (HYPHEN OR MINUS SIGN)
C
      DATA IOPERA( 149),IX( 149),IY( 149)/'MOVE',  -9,   0/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',   9,   0/
C
      DATA IXMIND(  13)/ -13/
      DATA IXMAXD(  13)/  13/
      DATA IXDELD(  13)/  26/
      DATA ISTARD(  13)/ 149/
      DATA NUMCOO(  13)/   2/
C
C     DEFINE CHARACTER    725--+ (PLUS SIGN)
C
      DATA IOPERA( 151),IX( 151),IY( 151)/'MOVE',   0,   9/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',   0,  -9/
      DATA IOPERA( 153),IX( 153),IY( 153)/'MOVE',  -9,   0/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',   9,   0/
C
      DATA IXMIND(  14)/ -13/
      DATA IXMAXD(  14)/  13/
      DATA IXDELD(  14)/  26/
      DATA ISTARD(  14)/ 151/
      DATA NUMCOO(  14)/   4/
C
C     DEFINE CHARACTER    726--= (EQUAL SIGN)
C
      DATA IOPERA( 155),IX( 155),IY( 155)/'MOVE',  -9,   3/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',   9,   3/
      DATA IOPERA( 157),IX( 157),IY( 157)/'MOVE',  -9,  -3/
      DATA IOPERA( 158),IX( 158),IY( 158)/'DRAW',   9,  -3/
C
      DATA IXMIND(  15)/ -13/
      DATA IXMAXD(  15)/  13/
      DATA IXDELD(  15)/  26/
      DATA ISTARD(  15)/ 155/
      DATA NUMCOO(  15)/   4/
C
C     DEFINE CHARACTER    716--' (SINGLE QUOTE)
C
      DATA IOPERA( 159),IX( 159),IY( 159)/'MOVE',   0,  12/
      DATA IOPERA( 160),IX( 160),IY( 160)/'DRAW',   0,   5/
C
      DATA IXMIND(  16)/  -4/
      DATA IXMAXD(  16)/   4/
      DATA IXDELD(  16)/   8/
      DATA ISTARD(  16)/ 159/
      DATA NUMCOO(  16)/   2/
C
C     DEFINE CHARACTER    717--  (DOUBLE QUOTE)
C
      DATA IOPERA( 161),IX( 161),IY( 161)/'MOVE',  -4,  12/
      DATA IOPERA( 162),IX( 162),IY( 162)/'DRAW',  -4,   5/
      DATA IOPERA( 163),IX( 163),IY( 163)/'MOVE',   4,  12/
      DATA IOPERA( 164),IX( 164),IY( 164)/'DRAW',   4,   5/
C
      DATA IXMIND(  17)/  -8/
      DATA IXMAXD(  17)/   8/
      DATA IXDELD(  17)/  16/
      DATA ISTARD(  17)/ 161/
      DATA NUMCOO(  17)/   4/
C
C     DEFINE CHARACTER    718--  (DEGREES)
C
      DATA IOPERA( 165),IX( 165),IY( 165)/'MOVE',  -1,  12/
      DATA IOPERA( 166),IX( 166),IY( 166)/'DRAW',  -3,  11/
      DATA IOPERA( 167),IX( 167),IY( 167)/'DRAW',  -4,   9/
      DATA IOPERA( 168),IX( 168),IY( 168)/'DRAW',  -4,   7/
      DATA IOPERA( 169),IX( 169),IY( 169)/'DRAW',  -3,   5/
      DATA IOPERA( 170),IX( 170),IY( 170)/'DRAW',  -1,   4/
      DATA IOPERA( 171),IX( 171),IY( 171)/'DRAW',   1,   4/
      DATA IOPERA( 172),IX( 172),IY( 172)/'DRAW',   3,   5/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW',   4,   7/
      DATA IOPERA( 174),IX( 174),IY( 174)/'DRAW',   4,   9/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW',   3,  11/
      DATA IOPERA( 176),IX( 176),IY( 176)/'DRAW',   1,  12/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW',  -1,  12/
C
      DATA IXMIND(  18)/  -7/
      DATA IXMAXD(  18)/   7/
      DATA IXDELD(  18)/  14/
      DATA ISTARD(  18)/ 165/
      DATA NUMCOO(  18)/  13/
C
C     DEFINE CHARACTER   2747--  (NO   SPACE BLANK)
C
      DATA IOPERA( 178),IX( 178),IY( 178)/'MOVE', -32, -32/
C
      DATA IXMIND(  19)/   0/
      DATA IXMAXD(  19)/   0/
      DATA IXDELD(  19)/   0/
      DATA ISTARD(  19)/ 178/
      DATA NUMCOO(  19)/   1/
C
C     DEFINE CHARACTER   2748--  (HALF SPACE BLANK)
C
      DATA IOPERA( 179),IX( 179),IY( 179)/'MOVE', -32, -32/
C
      DATA IXMIND(  20)/  -4/
      DATA IXMAXD(  20)/   4/
      DATA IXDELD(  20)/   8/
      DATA ISTARD(  20)/ 179/
      DATA NUMCOO(  20)/   1/
C
C     DEFINE CHARACTER   2749--  (FULL SPACE BLANK)
C
      DATA IOPERA( 180),IX( 180),IY( 180)/'MOVE', -32, -32/
C
      DATA IXMIND(  21)/  -8/
      DATA IXMAXD(  21)/   8/
      DATA IXDELD(  21)/  16/
      DATA ISTARD(  21)/ 180/
      DATA NUMCOO(  21)/   1/
C
C     DEFINE CHARACTER    730--  (LEFT  APOSTRAPHE)
C
      DATA IOPERA( 181),IX( 181),IY( 181)/'MOVE',   1,  12/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',   0,  11/
      DATA IOPERA( 183),IX( 183),IY( 183)/'DRAW',  -1,   9/
      DATA IOPERA( 184),IX( 184),IY( 184)/'DRAW',  -1,   7/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',   0,   6/
      DATA IOPERA( 186),IX( 186),IY( 186)/'DRAW',   1,   7/
      DATA IOPERA( 187),IX( 187),IY( 187)/'DRAW',   0,   8/
C
      DATA IXMIND(  22)/  -5/
      DATA IXMAXD(  22)/   5/
      DATA IXDELD(  22)/  10/
      DATA ISTARD(  22)/ 181/
      DATA NUMCOO(  22)/   7/
C
C     DEFINE CHARACTER    731--  (RIGHT APOSTRAPHE)
C
      DATA IOPERA( 188),IX( 188),IY( 188)/'MOVE',   0,  10/
      DATA IOPERA( 189),IX( 189),IY( 189)/'DRAW',  -1,  11/
      DATA IOPERA( 190),IX( 190),IY( 190)/'DRAW',   0,  12/
      DATA IOPERA( 191),IX( 191),IY( 191)/'DRAW',   1,  11/
      DATA IOPERA( 192),IX( 192),IY( 192)/'DRAW',   1,   9/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW',   0,   7/
      DATA IOPERA( 194),IX( 194),IY( 194)/'DRAW',  -1,   6/
C
      DATA IXMIND(  23)/  -5/
      DATA IXMAXD(  23)/   5/
      DATA IXDELD(  23)/  10/
      DATA ISTARD(  23)/ 188/
      DATA NUMCOO(  23)/   7/
C
C     DEFINE CHARACTER    XXX--| (KEYBOARD VERTICAL BAR)
C
      DATA IOPERA( 195),IX( 195),IY( 195)/'MOVE',   0,  12/
      DATA IOPERA( 196),IX( 196),IY( 196)/'DRAW',   0,  -9/
C
      DATA IXMIND(  24)/  -4/
      DATA IXMAXD(  24)/   4/
      DATA IXDELD(  24)/   8/
      DATA ISTARD(  24)/ 195/
      DATA NUMCOO(  24)/   2/
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
C               ******************************************
C               **  TREAT THE ROMAN SIMPLEX UPPER CASE  **
C               **  HERSHEY CHARACTER SET CASE          **
C               ******************************************
C               ******************************************
C
C
      IF(IBUGD2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPRSS--')
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
C               **************************************************
C               **  STEP 1--                                    **
C               **  SEARCH FOR THE INPUT CHARACTER(S).          **
C               **  MAP THE CHARACTER(S) INTO A NUMERIC VALUE.  **
C               **************************************************
C               **************************************************
C
      CALL DPCHSY(ICHAR2,ICHARN,IBUGD2,IFOUND)
      IF(IFOUND.EQ.'NO')GOTO9000
      GOTO1000
C
C               **************************************
C               **************************************
C               **  STEP 2--                        **
C               **  EXTRACT THE COORDINATES         **
C               **  FOR THIS PARTICULAR CHARACTER.  **
C               **************************************
C               **************************************
C
 1000 CONTINUE
      ISTART=ISTARD(ICHARN)
      NC=NUMCOO(ICHARN)
      ISTOP=ISTART+NC-1
      J=0
      DO1100I=ISTART,ISTOP
      J=J+1
      IOP(J)=IOPERA(I)
      X(J)=IX(I)
      Y(J)=IY(I)
 1100 CONTINUE
      NUMCO=J
      IXMINS=IXMIND(ICHARN)
      IXMAXS=IXMAXD(ICHARN)
      IXDELS=IXDELD(ICHARN)
C
      GOTO9000
C
C               *****************
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPRSS--')
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
      SUBROUTINE DPRSSL(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN SIMPLEX SCRIPT LOWER CASE.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
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
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
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
   51 FORMAT('***** AT THE BEGINNING OF DPRSSL--')
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
      CALL DRSSL1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1019 CONTINUE
C
      IF(ICHARN.GE.15)GOTO1020
      GOTO1029
 1020 CONTINUE
      CALL DRSSL2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
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
 9011 FORMAT('***** AT THE END       OF DPRSSL--')
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
      SUBROUTINE DPRSSU(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN SIMPLEX SCRIPT UPPER CASE.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
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
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
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
   51 FORMAT('***** AT THE BEGINNING OF DPRSSU--')
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
      IF(ICHARN.LE.10)GOTO1010
      GOTO1019
 1010 CONTINUE
      CALL DRSSU1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1019 CONTINUE
C
      IF(11.LE.ICHARN.AND.ICHARN.LE.19)GOTO1020
      GOTO1029
 1020 CONTINUE
      CALL DRSSU2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1029 CONTINUE
C
      IF(ICHARN.GE.20)GOTO1030
      GOTO1039
 1030 CONTINUE
      CALL DRSSU3(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
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
 9011 FORMAT('***** AT THE END       OF DPRSSU--')
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
      SUBROUTINE DPRSU(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN SIMPLEX UPPER CASE.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
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
      CHARACTER*4 IOPERA
C
C---------------------------------------------------------------------
C
      DIMENSION IOP(*)
      DIMENSION X(*)
      DIMENSION Y(*)
C
      DIMENSION IOPERA(300)
      DIMENSION IX(300)
      DIMENSION IY(300)
C
      DIMENSION IXMIND(30)
      DIMENSION IXMAXD(30)
      DIMENSION IXDELD(30)
      DIMENSION ISTARD(30)
      DIMENSION NUMCOO(30)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----DATA STATEMENTS-------------------------------------------------
C
C     DEFINE CHARACTER    501--UPPER CASE A
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',   0,  12/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',  -8,  -9/
      DATA IOPERA(   3),IX(   3),IY(   3)/'MOVE',   0,  12/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',   8,  -9/
      DATA IOPERA(   5),IX(   5),IY(   5)/'MOVE',  -5,  -2/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',   5,  -2/
C
      DATA IXMIND(   1)/  -9/
      DATA IXMAXD(   1)/   9/
      DATA IXDELD(   1)/  18/
      DATA ISTARD(   1)/   1/
      DATA NUMCOO(   1)/   6/
C
C     DEFINE CHARACTER    502--UPPER CASE B
C
      DATA IOPERA(   7),IX(   7),IY(   7)/'MOVE',  -7,  12/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',  -7,  -9/
      DATA IOPERA(   9),IX(   9),IY(   9)/'MOVE',  -7,  12/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',   2,  12/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',   5,  11/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',   6,  10/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',   7,   8/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',   7,   6/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',   6,   4/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',   5,   3/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',   2,   2/
      DATA IOPERA(  18),IX(  18),IY(  18)/'MOVE',  -7,   2/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',   2,   2/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',   5,   1/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',   6,   0/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',   7,  -2/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',   7,  -5/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',   6,  -7/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',   5,  -8/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',   2,  -9/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',  -7,  -9/
C
      DATA IXMIND(   2)/ -11/
      DATA IXMAXD(   2)/  10/
      DATA IXDELD(   2)/  21/
      DATA ISTARD(   2)/   7/
      DATA NUMCOO(   2)/  21/
C
C     DEFINE CHARACTER    503--UPPER CASE C
C
      DATA IOPERA(  28),IX(  28),IY(  28)/'MOVE',   8,   7/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',   7,   9/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',   5,  11/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',   3,  12/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',  -1,  12/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',  -3,  11/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',  -5,   9/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',  -6,   7/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',  -7,   4/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',  -7,  -1/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',  -6,  -4/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',  -5,  -6/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',  -3,  -8/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',  -1,  -9/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',   3,  -9/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',   5,  -8/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',   7,  -6/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',   8,  -4/
C
      DATA IXMIND(   3)/ -10/
      DATA IXMAXD(   3)/  11/
      DATA IXDELD(   3)/  21/
      DATA ISTARD(   3)/  28/
      DATA NUMCOO(   3)/  18/
C
C     DEFINE CHARACTER    504--UPPER CASE D
C
      DATA IOPERA(  46),IX(  46),IY(  46)/'MOVE',  -7,  12/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',  -7,  -9/
      DATA IOPERA(  48),IX(  48),IY(  48)/'MOVE',  -7,  12/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',   0,  12/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW',   3,  11/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW',   5,   9/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',   6,   7/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',   7,   4/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',   7,  -1/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',   6,  -4/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',   5,  -6/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',   3,  -8/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',   0,  -9/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',  -7,  -9/
C
      DATA IXMIND(   4)/ -11/
      DATA IXMAXD(   4)/  10/
      DATA IXDELD(   4)/  21/
      DATA ISTARD(   4)/  46/
      DATA NUMCOO(   4)/  14/
C
C     DEFINE CHARACTER    505--UPPER CASE E
C
      DATA IOPERA(  60),IX(  60),IY(  60)/'MOVE',  -6,  12/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',  -6,  -9/
      DATA IOPERA(  62),IX(  62),IY(  62)/'MOVE',  -6,  12/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',   7,  12/
      DATA IOPERA(  64),IX(  64),IY(  64)/'MOVE',  -6,   2/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',   2,   2/
      DATA IOPERA(  66),IX(  66),IY(  66)/'MOVE',  -6,  -9/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',   7,  -9/
C
      DATA IXMIND(   5)/ -10/
      DATA IXMAXD(   5)/   9/
      DATA IXDELD(   5)/  19/
      DATA ISTARD(   5)/  60/
      DATA NUMCOO(   5)/   8/
C
C     DEFINE CHARACTER    506--UPPER CASE F
C
      DATA IOPERA(  68),IX(  68),IY(  68)/'MOVE',  -6,  12/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',  -6,  -9/
      DATA IOPERA(  70),IX(  70),IY(  70)/'MOVE',  -6,  12/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',   7,  12/
      DATA IOPERA(  72),IX(  72),IY(  72)/'MOVE',  -6,   2/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',   2,   2/
C
      DATA IXMIND(   6)/ -10/
      DATA IXMAXD(   6)/   8/
      DATA IXDELD(   6)/  18/
      DATA ISTARD(   6)/  68/
      DATA NUMCOO(   6)/   6/
C
C     DEFINE CHARACTER    507--UPPER CASE G
C
      DATA IOPERA(  74),IX(  74),IY(  74)/'MOVE',   8,   7/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',   7,   9/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',   5,  11/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',   3,  12/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',  -1,  12/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',  -3,  11/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',  -5,   9/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',  -6,   7/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',  -7,   4/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',  -7,  -1/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW',  -6,  -4/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',  -5,  -6/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',  -3,  -8/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',  -1,  -9/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',   3,  -9/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',   5,  -8/
      DATA IOPERA(  90),IX(  90),IY(  90)/'DRAW',   7,  -6/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',   8,  -4/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW',   8,  -1/
      DATA IOPERA(  93),IX(  93),IY(  93)/'MOVE',   3,  -1/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',   8,  -1/
C
      DATA IXMIND(   7)/ -10/
      DATA IXMAXD(   7)/  11/
      DATA IXDELD(   7)/  21/
      DATA ISTARD(   7)/  74/
      DATA NUMCOO(   7)/  21/
C
C     DEFINE CHARACTER    508--UPPER CASE H
C
      DATA IOPERA(  95),IX(  95),IY(  95)/'MOVE',  -7,  12/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',  -7,  -9/
      DATA IOPERA(  97),IX(  97),IY(  97)/'MOVE',   7,  12/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',   7,  -9/
      DATA IOPERA(  99),IX(  99),IY(  99)/'MOVE',  -7,   2/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',   7,   2/
C
      DATA IXMIND(   8)/ -11/
      DATA IXMAXD(   8)/  11/
      DATA IXDELD(   8)/  22/
      DATA ISTARD(   8)/  95/
      DATA NUMCOO(   8)/   6/
C
C     DEFINE CHARACTER    509--UPPER CASE I
C
      DATA IOPERA( 101),IX( 101),IY( 101)/'MOVE',   0,  12/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',   0,  -9/
C
      DATA IXMIND(   9)/  -4/
      DATA IXMAXD(   9)/   4/
      DATA IXDELD(   9)/   8/
      DATA ISTARD(   9)/ 101/
      DATA NUMCOO(   9)/   2/
C
C     DEFINE CHARACTER    510--UPPER CASE J
C
      DATA IOPERA( 103),IX( 103),IY( 103)/'MOVE',   4,  12/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',   4,  -4/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',   3,  -7/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',   2,  -8/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',   0,  -9/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',  -2,  -9/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',  -4,  -8/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',  -5,  -7/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',  -6,  -4/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',  -6,  -2/
C
      DATA IXMIND(  10)/  -8/
      DATA IXMAXD(  10)/   8/
      DATA IXDELD(  10)/  16/
      DATA ISTARD(  10)/ 103/
      DATA NUMCOO(  10)/  10/
C
C     DEFINE CHARACTER    511--UPPER CASE K
C
      DATA IOPERA( 113),IX( 113),IY( 113)/'MOVE',  -7,  12/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',  -7,  -9/
      DATA IOPERA( 115),IX( 115),IY( 115)/'MOVE',   7,  12/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',  -7,  -2/
      DATA IOPERA( 117),IX( 117),IY( 117)/'MOVE',  -2,   3/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',   7,  -9/
C
      DATA IXMIND(  11)/ -11/
      DATA IXMAXD(  11)/  10/
      DATA IXDELD(  11)/  21/
      DATA ISTARD(  11)/ 113/
      DATA NUMCOO(  11)/   6/
C
C     DEFINE CHARACTER    512--UPPER CASE L
C
      DATA IOPERA( 119),IX( 119),IY( 119)/'MOVE',  -6,  12/
      DATA IOPERA( 120),IX( 120),IY( 120)/'DRAW',  -6,  -9/
      DATA IOPERA( 121),IX( 121),IY( 121)/'MOVE',  -6,  -9/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW',   6,  -9/
C
      DATA IXMIND(  12)/ -10/
      DATA IXMAXD(  12)/   7/
      DATA IXDELD(  12)/  17/
      DATA ISTARD(  12)/ 119/
      DATA NUMCOO(  12)/   4/
C
C     DEFINE CHARACTER    513--UPPER CASE M
C
      DATA IOPERA( 123),IX( 123),IY( 123)/'MOVE',  -8,  12/
      DATA IOPERA( 124),IX( 124),IY( 124)/'DRAW',  -8,  -9/
      DATA IOPERA( 125),IX( 125),IY( 125)/'MOVE',  -8,  12/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW',   0,  -9/
      DATA IOPERA( 127),IX( 127),IY( 127)/'MOVE',   8,  12/
      DATA IOPERA( 128),IX( 128),IY( 128)/'DRAW',   0,  -9/
      DATA IOPERA( 129),IX( 129),IY( 129)/'MOVE',   8,  12/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',   8,  -9/
C
      DATA IXMIND(  13)/ -12/
      DATA IXMAXD(  13)/  12/
      DATA IXDELD(  13)/  24/
      DATA ISTARD(  13)/ 123/
      DATA NUMCOO(  13)/   8/
C
C     DEFINE CHARACTER    514--UPPER CASE N
C
      DATA IOPERA( 131),IX( 131),IY( 131)/'MOVE',  -7,  12/
      DATA IOPERA( 132),IX( 132),IY( 132)/'DRAW',  -7,  -9/
      DATA IOPERA( 133),IX( 133),IY( 133)/'MOVE',  -7,  12/
      DATA IOPERA( 134),IX( 134),IY( 134)/'DRAW',   7,  -9/
      DATA IOPERA( 135),IX( 135),IY( 135)/'MOVE',   7,  12/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',   7,  -9/
C
      DATA IXMIND(  14)/ -11/
      DATA IXMAXD(  14)/  11/
      DATA IXDELD(  14)/  22/
      DATA ISTARD(  14)/ 131/
      DATA NUMCOO(  14)/   6/
C
C     DEFINE CHARACTER    515--UPPER CASE O
C
      DATA IOPERA( 137),IX( 137),IY( 137)/'MOVE',  -2,  12/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW',  -4,  11/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW',  -6,   9/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW',  -7,   7/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',  -8,   4/
      DATA IOPERA( 142),IX( 142),IY( 142)/'DRAW',  -8,  -1/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',  -7,  -4/
      DATA IOPERA( 144),IX( 144),IY( 144)/'DRAW',  -6,  -6/
      DATA IOPERA( 145),IX( 145),IY( 145)/'DRAW',  -4,  -8/
      DATA IOPERA( 146),IX( 146),IY( 146)/'DRAW',  -2,  -9/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',   2,  -9/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',   4,  -8/
      DATA IOPERA( 149),IX( 149),IY( 149)/'DRAW',   6,  -6/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',   7,  -4/
      DATA IOPERA( 151),IX( 151),IY( 151)/'DRAW',   8,  -1/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',   8,   4/
      DATA IOPERA( 153),IX( 153),IY( 153)/'DRAW',   7,   7/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',   6,   9/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW',   4,  11/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',   2,  12/
      DATA IOPERA( 157),IX( 157),IY( 157)/'DRAW',  -2,  12/
C
      DATA IXMIND(  15)/ -11/
      DATA IXMAXD(  15)/  11/
      DATA IXDELD(  15)/  22/
      DATA ISTARD(  15)/ 137/
      DATA NUMCOO(  15)/  21/
C
C     DEFINE CHARACTER    516--UPPER CASE P
C
      DATA IOPERA( 158),IX( 158),IY( 158)/'MOVE',  -7,  12/
      DATA IOPERA( 159),IX( 159),IY( 159)/'DRAW',  -7,  -9/
      DATA IOPERA( 160),IX( 160),IY( 160)/'MOVE',  -7,  12/
      DATA IOPERA( 161),IX( 161),IY( 161)/'DRAW',   2,  12/
      DATA IOPERA( 162),IX( 162),IY( 162)/'DRAW',   5,  11/
      DATA IOPERA( 163),IX( 163),IY( 163)/'DRAW',   6,  10/
      DATA IOPERA( 164),IX( 164),IY( 164)/'DRAW',   7,   8/
      DATA IOPERA( 165),IX( 165),IY( 165)/'DRAW',   7,   5/
      DATA IOPERA( 166),IX( 166),IY( 166)/'DRAW',   6,   3/
      DATA IOPERA( 167),IX( 167),IY( 167)/'DRAW',   5,   2/
      DATA IOPERA( 168),IX( 168),IY( 168)/'DRAW',   2,   1/
      DATA IOPERA( 169),IX( 169),IY( 169)/'DRAW',  -7,   1/
C
      DATA IXMIND(  16)/ -11/
      DATA IXMAXD(  16)/  10/
      DATA IXDELD(  16)/  21/
      DATA ISTARD(  16)/ 158/
      DATA NUMCOO(  16)/  12/
C
C     DEFINE CHARACTER    517--UPPER CASE Q
C
      DATA IOPERA( 170),IX( 170),IY( 170)/'MOVE',  -2,  12/
      DATA IOPERA( 171),IX( 171),IY( 171)/'DRAW',  -4,  11/
      DATA IOPERA( 172),IX( 172),IY( 172)/'DRAW',  -6,   9/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW',  -7,   7/
      DATA IOPERA( 174),IX( 174),IY( 174)/'DRAW',  -8,   4/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW',  -8,  -1/
      DATA IOPERA( 176),IX( 176),IY( 176)/'DRAW',  -7,  -4/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW',  -6,  -6/
      DATA IOPERA( 178),IX( 178),IY( 178)/'DRAW',  -4,  -8/
      DATA IOPERA( 179),IX( 179),IY( 179)/'DRAW',  -2,  -9/
      DATA IOPERA( 180),IX( 180),IY( 180)/'DRAW',   2,  -9/
      DATA IOPERA( 181),IX( 181),IY( 181)/'DRAW',   4,  -8/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',   6,  -6/
      DATA IOPERA( 183),IX( 183),IY( 183)/'DRAW',   7,  -4/
      DATA IOPERA( 184),IX( 184),IY( 184)/'DRAW',   8,  -1/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',   8,   4/
      DATA IOPERA( 186),IX( 186),IY( 186)/'DRAW',   7,   7/
      DATA IOPERA( 187),IX( 187),IY( 187)/'DRAW',   6,   9/
      DATA IOPERA( 188),IX( 188),IY( 188)/'DRAW',   4,  11/
      DATA IOPERA( 189),IX( 189),IY( 189)/'DRAW',   2,  12/
      DATA IOPERA( 190),IX( 190),IY( 190)/'DRAW',  -2,  12/
      DATA IOPERA( 191),IX( 191),IY( 191)/'MOVE',   1,  -5/
      DATA IOPERA( 192),IX( 192),IY( 192)/'DRAW',   7, -11/
C
      DATA IXMIND(  17)/ -11/
      DATA IXMAXD(  17)/  11/
      DATA IXDELD(  17)/  22/
      DATA ISTARD(  17)/ 170/
      DATA NUMCOO(  17)/  23/
C
C     DEFINE CHARACTER    518--UPPER CASE R
C
      DATA IOPERA( 193),IX( 193),IY( 193)/'MOVE',  -7,  12/
      DATA IOPERA( 194),IX( 194),IY( 194)/'DRAW',  -7,  -9/
      DATA IOPERA( 195),IX( 195),IY( 195)/'MOVE',  -7,  12/
      DATA IOPERA( 196),IX( 196),IY( 196)/'DRAW',   2,  12/
      DATA IOPERA( 197),IX( 197),IY( 197)/'DRAW',   5,  11/
      DATA IOPERA( 198),IX( 198),IY( 198)/'DRAW',   6,  10/
      DATA IOPERA( 199),IX( 199),IY( 199)/'DRAW',   7,   8/
      DATA IOPERA( 200),IX( 200),IY( 200)/'DRAW',   7,   6/
      DATA IOPERA( 201),IX( 201),IY( 201)/'DRAW',   6,   4/
      DATA IOPERA( 202),IX( 202),IY( 202)/'DRAW',   5,   3/
      DATA IOPERA( 203),IX( 203),IY( 203)/'DRAW',   2,   2/
      DATA IOPERA( 204),IX( 204),IY( 204)/'DRAW',  -7,   2/
      DATA IOPERA( 205),IX( 205),IY( 205)/'MOVE',   0,   2/
      DATA IOPERA( 206),IX( 206),IY( 206)/'DRAW',   7,  -9/
C
      DATA IXMIND(  18)/ -11/
      DATA IXMAXD(  18)/  10/
      DATA IXDELD(  18)/  21/
      DATA ISTARD(  18)/ 193/
      DATA NUMCOO(  18)/  14/
C
C     DEFINE CHARACTER    519--UPPER CASE S
C
      DATA IOPERA( 207),IX( 207),IY( 207)/'MOVE',   7,   9/
      DATA IOPERA( 208),IX( 208),IY( 208)/'DRAW',   5,  11/
      DATA IOPERA( 209),IX( 209),IY( 209)/'DRAW',   2,  12/
      DATA IOPERA( 210),IX( 210),IY( 210)/'DRAW',  -2,  12/
      DATA IOPERA( 211),IX( 211),IY( 211)/'DRAW',  -5,  11/
      DATA IOPERA( 212),IX( 212),IY( 212)/'DRAW',  -7,   9/
      DATA IOPERA( 213),IX( 213),IY( 213)/'DRAW',  -7,   7/
      DATA IOPERA( 214),IX( 214),IY( 214)/'DRAW',  -6,   5/
      DATA IOPERA( 215),IX( 215),IY( 215)/'DRAW',  -5,   4/
      DATA IOPERA( 216),IX( 216),IY( 216)/'DRAW',  -3,   3/
      DATA IOPERA( 217),IX( 217),IY( 217)/'DRAW',   3,   1/
      DATA IOPERA( 218),IX( 218),IY( 218)/'DRAW',   5,   0/
      DATA IOPERA( 219),IX( 219),IY( 219)/'DRAW',   6,  -1/
      DATA IOPERA( 220),IX( 220),IY( 220)/'DRAW',   7,  -3/
      DATA IOPERA( 221),IX( 221),IY( 221)/'DRAW',   7,  -6/
      DATA IOPERA( 222),IX( 222),IY( 222)/'DRAW',   5,  -8/
      DATA IOPERA( 223),IX( 223),IY( 223)/'DRAW',   2,  -9/
      DATA IOPERA( 224),IX( 224),IY( 224)/'DRAW',  -2,  -9/
      DATA IOPERA( 225),IX( 225),IY( 225)/'DRAW',  -5,  -8/
      DATA IOPERA( 226),IX( 226),IY( 226)/'DRAW',  -7,  -6/
C
      DATA IXMIND(  19)/ -10/
      DATA IXMAXD(  19)/  10/
      DATA IXDELD(  19)/  20/
      DATA ISTARD(  19)/ 207/
      DATA NUMCOO(  19)/  20/
C
C     DEFINE CHARACTER    520--UPPER CASE T
C
      DATA IOPERA( 227),IX( 227),IY( 227)/'MOVE',   0,  12/
      DATA IOPERA( 228),IX( 228),IY( 228)/'DRAW',   0,  -9/
      DATA IOPERA( 229),IX( 229),IY( 229)/'MOVE',  -7,  12/
      DATA IOPERA( 230),IX( 230),IY( 230)/'DRAW',   7,  12/
C
      DATA IXMIND(  20)/  -8/
      DATA IXMAXD(  20)/   8/
      DATA IXDELD(  20)/  16/
      DATA ISTARD(  20)/ 227/
      DATA NUMCOO(  20)/   4/
C
C     DEFINE CHARACTER    521--UPPER CASE U
C
      DATA IOPERA( 231),IX( 231),IY( 231)/'MOVE',  -7,  12/
      DATA IOPERA( 232),IX( 232),IY( 232)/'DRAW',  -7,  -3/
      DATA IOPERA( 233),IX( 233),IY( 233)/'DRAW',  -6,  -6/
      DATA IOPERA( 234),IX( 234),IY( 234)/'DRAW',  -4,  -8/
      DATA IOPERA( 235),IX( 235),IY( 235)/'DRAW',  -1,  -9/
      DATA IOPERA( 236),IX( 236),IY( 236)/'DRAW',   1,  -9/
      DATA IOPERA( 237),IX( 237),IY( 237)/'DRAW',   4,  -8/
      DATA IOPERA( 238),IX( 238),IY( 238)/'DRAW',   6,  -6/
      DATA IOPERA( 239),IX( 239),IY( 239)/'DRAW',   7,  -3/
      DATA IOPERA( 240),IX( 240),IY( 240)/'DRAW',   7,  12/
C
      DATA IXMIND(  21)/ -11/
      DATA IXMAXD(  21)/  11/
      DATA IXDELD(  21)/  22/
      DATA ISTARD(  21)/ 231/
      DATA NUMCOO(  21)/  10/
C
C     DEFINE CHARACTER    522--UPPER CASE V
C
      DATA IOPERA( 241),IX( 241),IY( 241)/'MOVE',  -8,  12/
      DATA IOPERA( 242),IX( 242),IY( 242)/'DRAW',   0,  -9/
      DATA IOPERA( 243),IX( 243),IY( 243)/'MOVE',   8,  12/
      DATA IOPERA( 244),IX( 244),IY( 244)/'DRAW',   0,  -9/
C
      DATA IXMIND(  22)/  -9/
      DATA IXMAXD(  22)/   9/
      DATA IXDELD(  22)/  18/
      DATA ISTARD(  22)/ 241/
      DATA NUMCOO(  22)/   4/
C
C     DEFINE CHARACTER    523--UPPER CASE W
C
      DATA IOPERA( 245),IX( 245),IY( 245)/'MOVE', -10,  12/
      DATA IOPERA( 246),IX( 246),IY( 246)/'DRAW',  -5,  -9/
      DATA IOPERA( 247),IX( 247),IY( 247)/'MOVE',   0,  12/
      DATA IOPERA( 248),IX( 248),IY( 248)/'DRAW',  -5,  -9/
      DATA IOPERA( 249),IX( 249),IY( 249)/'MOVE',   0,  12/
      DATA IOPERA( 250),IX( 250),IY( 250)/'DRAW',   5,  -9/
      DATA IOPERA( 251),IX( 251),IY( 251)/'MOVE',  10,  12/
      DATA IOPERA( 252),IX( 252),IY( 252)/'DRAW',   5,  -9/
C
      DATA IXMIND(  23)/ -12/
      DATA IXMAXD(  23)/  12/
      DATA IXDELD(  23)/  24/
      DATA ISTARD(  23)/ 245/
      DATA NUMCOO(  23)/   8/
C
C     DEFINE CHARACTER    524--UPPER CASE X
C
      DATA IOPERA( 253),IX( 253),IY( 253)/'MOVE',  -7,  12/
      DATA IOPERA( 254),IX( 254),IY( 254)/'DRAW',   7,  -9/
      DATA IOPERA( 255),IX( 255),IY( 255)/'MOVE',   7,  12/
      DATA IOPERA( 256),IX( 256),IY( 256)/'DRAW',  -7,  -9/
C
      DATA IXMIND(  24)/ -10/
      DATA IXMAXD(  24)/  10/
      DATA IXDELD(  24)/  20/
      DATA ISTARD(  24)/ 253/
      DATA NUMCOO(  24)/   4/
C
C     DEFINE CHARACTER    525--UPPER CASE Y
C
      DATA IOPERA( 257),IX( 257),IY( 257)/'MOVE',  -8,  12/
      DATA IOPERA( 258),IX( 258),IY( 258)/'DRAW',   0,   2/
      DATA IOPERA( 259),IX( 259),IY( 259)/'DRAW',   0,  -9/
      DATA IOPERA( 260),IX( 260),IY( 260)/'MOVE',   8,  12/
      DATA IOPERA( 261),IX( 261),IY( 261)/'DRAW',   0,   2/
C
      DATA IXMIND(  25)/  -9/
      DATA IXMAXD(  25)/   9/
      DATA IXDELD(  25)/  18/
      DATA ISTARD(  25)/ 257/
      DATA NUMCOO(  25)/   5/
C
C     DEFINE CHARACTER    526--UPPER CASE Z
C
      DATA IOPERA( 262),IX( 262),IY( 262)/'MOVE',   7,  12/
      DATA IOPERA( 263),IX( 263),IY( 263)/'DRAW',  -7,  -9/
      DATA IOPERA( 264),IX( 264),IY( 264)/'MOVE',  -7,  12/
      DATA IOPERA( 265),IX( 265),IY( 265)/'DRAW',   7,  12/
      DATA IOPERA( 266),IX( 266),IY( 266)/'MOVE',  -7,  -9/
      DATA IOPERA( 267),IX( 267),IY( 267)/'DRAW',   7,  -9/
C
      DATA IXMIND(  26)/ -10/
      DATA IXMAXD(  26)/  10/
      DATA IXDELD(  26)/  20/
      DATA ISTARD(  26)/ 262/
      DATA NUMCOO(  26)/   6/
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
C               ******************************************
C               **  TREAT THE ROMAN SIMPLEX UPPER CASE  **
C               **  HERSHEY CHARACTER SET CASE          **
C               ******************************************
C               ******************************************
C
C
      IF(IBUGD2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPRSU--')
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
C               **************************************************
C               **  STEP 1--                                    **
C               **  SEARCH FOR THE INPUT CHARACTER(S).          **
C               **  MAP THE CHARACTER(S) INTO A NUMERIC VALUE.  **
C               **************************************************
C               **************************************************
C
      CALL DPCHAL(ICHAR2,ICHARN,IBUGD2,IFOUND)
      IF(IFOUND.EQ.'NO')GOTO9000
      GOTO1000
C
C               **************************************
C               **************************************
C               **  STEP 2--                        **
C               **  EXTRACT THE COORDINATES         **
C               **  FOR THIS PARTICULAR CHARACTER.  **
C               **************************************
C               **************************************
C
 1000 CONTINUE
      ISTART=ISTARD(ICHARN)
      NC=NUMCOO(ICHARN)
      ISTOP=ISTART+NC-1
      J=0
      DO1100I=ISTART,ISTOP
      J=J+1
      IOP(J)=IOPERA(I)
      X(J)=IX(I)
      Y(J)=IY(I)
 1100 CONTINUE
      NUMCO=J
      IXMINS=IXMIND(ICHARN)
      IXMAXS=IXMAXD(ICHARN)
      IXDELS=IXDELD(ICHARN)
C
      GOTO9000
C
C               *****************
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPRSU--')
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
      SUBROUTINE DPRTF1(IHEAD,NHEAD,CAPTN,NCAP)
C
C     PURPOSE--THIS ROUTINE IS A UTILITY ROUTINE FOR CREATING
C              RTF OUTPUT.  THIS ROUTINE IS USED TO INITIATE
C              THE RTF OUTPUT AND STARTS THE FIRST TABLE.
C              THE ONLY OPTIONAL ELEMENT IS THE CAPTION.
C     INPUT  ARGUMENTS--IHEAD  = THE CHARACTER STRING CONTAINING
C                                THE TEXT FOR THE HEADER
C                     --NHEAD  = THE INTEGER NUMBER THAT SPECIFIES
C                                THE NUMBER OF CHARACTERS IN THE
C                                HEADER.
C                     --CAPTN  = THE CHARACTER STRING CONTAINING
C                                THE CAPTION.
C                     --NCAP   = THE INTEGER NUMBER THAT SPECIFIES
C                                THE NUMBER OF CHARACTERS IN THE
C                                CAPTION.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABOARATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2005/2
C     ORIGINAL VERSION--FEBRUARY  2005.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*(*) CAPTN
      CHARACTER*(*) IHEAD
C
      CHARACTER*1  IBASLC
      CHARACTER*10 IFORMT
C
      INCLUDE 'DPCOST.INC'
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
C  STEP 1: END ASIS MODE AND WRITE A HEADER
C
C
      CALL DPCONA(92,IBASLC)
 8001 FORMAT('{',A1,'pard')
 8002 FORMAT(A1,'par}')
C8003 FORMAT('{',A1,'qc',A1,'fs',I2,A1,'b')
 8003 FORMAT('{',A1,'qc',A1,'b')
 8007 FORMAT('}')
 8008 FORMAT(A1,'line')
 8009 FORMAT(A1,'line ',A1,'line')
      WRITE(ICOUT,8001)IBASLC
      CALL DPWRST('XXX','WRIT')
      IF(NHEAD.GE.1)THEN
        ATEMP=1.5*REAL(IRTFPS)
        ITEMP=INT(ATEMP)
        WRITE(ICOUT,8003)IBASLC,IBASLC
        CALL DPWRST('XXX','WRIT')
        IFORMT=' '
        IFORMT(1:5)='(A  )'
        WRITE(IFORMT(3:4),'(I2)')NHEAD
        WRITE(ICOUT,IFORMT)IHEAD(1:NHEAD)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,8007)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,8008)IBASLC
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
C  STEP 2: START TABLE AND DEFINE A CAPTION
C
 8013 FORMAT('{',A1,'qc',A1,'b')
      IF(NCAP.GT.0)THEN
        WRITE(ICOUT,8013)IBASLC,IBASLC
        CALL DPWRST('XXX','WRIT')
        IFORMT=' '
        IFORMT(1:6)='(A   )'
        WRITE(IFORMT(3:5),'(I3)')NCAP
        WRITE(ICOUT,IFORMT)CAPTN(1:NCAP)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,8007)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,8008)IBASLC
        CALL DPWRST('XXX','WRIT')
      ENDIF
      WRITE(ICOUT,8002)IBASLC
      CALL DPWRST('XXX','WRIT')
C
      RETURN
      END
      SUBROUTINE DPRTF4(IVALUE,NCHAR,NHEAD,IFLAG1,IFLAG2)
C
C     PURPOSE--THIS ROUTINE IS A UTILUTY ROUTINE FOR CREATING
C              RTF OUTPUT.  THIS ROUTINE IS USED TO GENERATE
C              A HEADER ROW FOR A TABLE.  YOU CAN ALSO OPTIONALLY
C              ADD A RULE LINE BEFORE OR AFTER THE HEADER.
C
C     INPUT  ARGUMENTS--IVALUE  = THE CHARACTER STRING ARRAY
C                                 CONTAINING THE TEXT FOR THE
C                                 HEADER VALUES.
C                     --NCHAR   = THE INTEGER ARRAY THAT SPECIFIES
C                                 THE NUMBER OF CHARACTERS IN THE
C                                 HEADER VALUES.
C                     --NHEAD   = THE INTEGER VALUE THAT SPECIFIES
C                                 THE NUMBER OF HEADER VALUES.
C                     --IFLAG1  = A LOGICAL VALUE THAT SPECIFIES
C                                 WHETHER A RULE LINE IS DRAWN BEFORE
C                                 THE HEADER.
C                     --IFLAG2  = A LOGICAL VALUE THAT SPECIFIES
C                                 WHETHER A RULE LINE IS DRAWN AFTER
C                                 THE HHEADER.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABOARATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2005/2
C     ORIGINAL VERSION--FEBRUARY  2005.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*(*) IVALUE(NHEAD)
      INTEGER NCHAR(NHEAD)
C
      PARAMETER (MAXHED=1024)
      INTEGER IWIDTH(MAXHED)
      INTEGER NUMDIG(MAXHED)
      CHARACTER*8 ALIGN(MAXHED)
      CHARACTER*8 VALIGN(MAXHED)
      COMMON/HTML4/IWIDTH,NUMDIG,ALIGN,VALIGN
C
      LOGICAL IFLAG1
      LOGICAL IFLAG2
C
      CHARACTER*1  IBASLC
      CHARACTER*20 IFORMT
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      CALL DPCONA(92,IBASLC)
C
C  STEP 1: GENERATE A HEADER LINE
C
 8001 FORMAT('{',A1,'trowd',A1,'trgraph90')
      WRITE(ICOUT,8001)IBASLC,IBASLC
      CALL DPWRST('XXX','WRIT')
C
 8011 FORMAT(A1,'clvertalt',A1,'cellx',I3)
 8012 FORMAT(A1,'clvertalc',A1,'cellx',I3)
 8013 FORMAT(A1,'clvertalb',A1,'cellx',I3)
 8111 FORMAT(A1,'clvertalt',A1,'cellx',I4)
 8112 FORMAT(A1,'clvertalc',A1,'cellx',I4)
 8113 FORMAT(A1,'clvertalb',A1,'cellx',I4)
 8211 FORMAT(A1,'clvertalt',A1,'cellx',I5)
 8212 FORMAT(A1,'clvertalc',A1,'cellx',I5)
 8213 FORMAT(A1,'clvertalb',A1,'cellx',I5)
 8014 FORMAT(A1,'clbrdrt',A1,'brdrw15',A1,'brdrs')
 8015 FORMAT(A1,'clbrdrb',A1,'brdrw15',A1,'brdrs')
      DO8010I=1,NHEAD
        IF(IFLAG1)THEN
          WRITE(ICOUT,8014)IBASLC,IBASLC,IBASLC
          CALL DPWRST('XXX','WRIT')
        ENDIF
        IF(IFLAG2)THEN
          WRITE(ICOUT,8015)IBASLC,IBASLC,IBASLC
          CALL DPWRST('XXX','WRIT')
        ENDIF
        IF(VALIGN(I).EQ.'b')THEN
          IF(IWIDTH(I).LE.999)THEN
            WRITE(ICOUT,8013)IBASLC,IBASLC,IWIDTH(I)
          ELSEIF(IWIDTH(I).LE.9999)THEN
            WRITE(ICOUT,8113)IBASLC,IBASLC,IWIDTH(I)
          ELSE
            WRITE(ICOUT,8213)IBASLC,IBASLC,IWIDTH(I)
          ENDIF
        ELSEIF(VALIGN(I).EQ.'c')THEN
          IF(IWIDTH(I).LE.999)THEN
            WRITE(ICOUT,8012)IBASLC,IBASLC,IWIDTH(I)
          ELSEIF(IWIDTH(I).LE.9999)THEN
            WRITE(ICOUT,8112)IBASLC,IBASLC,IWIDTH(I)
          ELSE
            WRITE(ICOUT,8212)IBASLC,IBASLC,IWIDTH(I)
          ENDIF
        ELSE
          IF(IWIDTH(I).LE.999)THEN
            WRITE(ICOUT,8011)IBASLC,IBASLC,IWIDTH(I)
          ELSEIF(IWIDTH(I).LE.9999)THEN
            WRITE(ICOUT,8111)IBASLC,IBASLC,IWIDTH(I)
          ELSE
            WRITE(ICOUT,8211)IBASLC,IBASLC,IWIDTH(I)
          ENDIF
        ENDIF
        CALL DPWRST('XXX','WRIT')
 8010 CONTINUE
C
 8021 FORMAT(A1,'pard',A1,'intbl',A1,'ql {')
 8022 FORMAT(A1,'pard',A1,'intbl',A1,'qc {')
 8023 FORMAT(A1,'pard',A1,'intbl',A1,'qr {')
      IFORMT=' '
      IFORMT(1:5)='(A  )'
 8027 FORMAT('}',A1,'cell')
      DO8020I=1,NHEAD
        IF(ALIGN(I).EQ.'l')THEN
          WRITE(ICOUT,8021)IBASLC,IBASLC,IBASLC
        ELSEIF(ALIGN(I).EQ.'c')THEN
          WRITE(ICOUT,8022)IBASLC,IBASLC,IBASLC
        ELSE
          WRITE(ICOUT,8023)IBASLC,IBASLC,IBASLC
        ENDIF
        CALL DPWRST('XXX','WRIT')
        IF(NCHAR(I).GT.0)THEN
          WRITE(IFORMT(3:4),'(I2)')NCHAR(I)
          WRITE(ICOUT,IFORMT)IVALUE(I)(1:NCHAR(I))
          CALL DPWRST('XXX','WRIT')
        ELSE
          ITEMP=1
          WRITE(IFORMT(3:4),'(I2)')ITEMP
          WRITE(ICOUT,IFORMT) ' '
          CALL DPWRST('XXX','WRIT')
        ENDIF
        WRITE(ICOUT,8027)IBASLC
        CALL DPWRST('XXX','WRIT')
 8020 CONTINUE
C
 8039 FORMAT(A1,'row}')
      WRITE(ICOUT,8039)IBASLC
      CALL DPWRST('XXX','WRIT')
C
      RETURN
      END
      SUBROUTINE DPRT4B(IVALUE,NCHAR,NHEAD,NCOLSP,IFLAG1,IFLAG2)
C
C     PURPOSE--THIS ROUTINE IS A UTILUTY ROUTINE FOR CREATING
C              RTF OUTPUT.  THIS ROUTINE IS USED TO GENERATE
C              A HEADER ROW FOR A TABLE.  YOU CAN ALSO OPTIONALLY
C              ADD A RULE LINE BEFORE OR AFTER THE HEADER.
C
C     INPUT  ARGUMENTS--IVALUE  = THE CHARACTER STRING ARRAY
C                                 CONTAINING THE TEXT FOR THE
C                                 HEADER VALUES.
C                     --NCHAR   = THE INTEGER ARRAY THAT SPECIFIES
C                                 THE NUMBER OF CHARACTERS IN THE
C                                 HEADER VALUES.
C                     --NHEAD   = THE INTEGER VALUE THAT SPECIFIES
C                                 THE NUMBER OF HEADER VALUES.
C                     --NCOLSP  = THE INTEGER ARRAY THAT SPECIFIES
C                                 THE COLUMN SPAN FOR THE GIVEN COLUMN
C                     --IFLAG1  = A LOGICAL VALUE THAT SPECIFIES
C                                 WHETHER A RULE LINE IS DRAWN BEFORE
C                                 THE HEADER.
C                     --IFLAG2  = A LOGICAL VALUE THAT SPECIFIES
C                                 WHETHER A RULE LINE IS DRAWN AFTER
C                                 THE HEADER.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABOARATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2011/1
C     ORIGINAL VERSION--JANUARY   2011.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*(*) IVALUE(NHEAD)
      INTEGER NCHAR(NHEAD)
      INTEGER NCOLSP(NHEAD)
C
      PARAMETER (MAXHED=1024)
      INTEGER IWIDTH(MAXHED)
      INTEGER NUMDIG(MAXHED)
      CHARACTER*8 ALIGN(MAXHED)
      CHARACTER*8 VALIGN(MAXHED)
      COMMON/HTML4/IWIDTH,NUMDIG,ALIGN,VALIGN
C
      LOGICAL IFLAG1
      LOGICAL IFLAG2
C
      CHARACTER*1  IBASLC
      CHARACTER*20 IFORMT
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      CALL DPCONA(92,IBASLC)
C
C  STEP 1: GENERATE A HEADER LINE
C
 8001 FORMAT('{',A1,'trowd',A1,'trgraph90')
      WRITE(ICOUT,8001)IBASLC,IBASLC
      CALL DPWRST('XXX','WRIT')
C
 8011 FORMAT(A1,'clvertalt',A1,'cellx',I3)
 8012 FORMAT(A1,'clvertalc',A1,'cellx',I3)
 8013 FORMAT(A1,'clvertalb',A1,'cellx',I3)
 8111 FORMAT(A1,'clvertalt',A1,'cellx',I4)
 8112 FORMAT(A1,'clvertalc',A1,'cellx',I4)
 8113 FORMAT(A1,'clvertalb',A1,'cellx',I4)
 8211 FORMAT(A1,'clvertalt',A1,'cellx',I5)
 8212 FORMAT(A1,'clvertalc',A1,'cellx',I5)
 8213 FORMAT(A1,'clvertalb',A1,'cellx',I5)
 8014 FORMAT(A1,'clbrdrt',A1,'brdrw15',A1,'brdrs')
 8015 FORMAT(A1,'clbrdrb',A1,'brdrw15',A1,'brdrs')
 8016 FORMAT(A1,'clbrdrr',A1,'brdrw15',A1,'brdrs')
C
C     TRANSLATE "\'7C" TO BE A RIGHT BORDER (FORMAT 8016)
C     AND MAKE THE TEXT BLANK.
C
      DO8010I=1,NHEAD
        IF(IFLAG1)THEN
          WRITE(ICOUT,8014)IBASLC,IBASLC,IBASLC
          CALL DPWRST('XXX','WRIT')
        ENDIF
        IF(IFLAG2)THEN
          WRITE(ICOUT,8015)IBASLC,IBASLC,IBASLC
          CALL DPWRST('XXX','WRIT')
        ENDIF
        IF(IVALUE(I)(7:8).EQ.'7C')THEN
          CALL DPCOAN(IVALUE(I)(5:5),IJUNK1)
          CALL DPCOAN(IVALUE(I)(6:6),IJUNK2)
          IF(IJUNK1.EQ.92 .AND. IJUNK2.EQ.39)THEN
            WRITE(ICOUT,8016)IBASLC,IBASLC,IBASLC
            CALL DPWRST('XXX','WRIT')
            IVALUE(I)=' '
            NCHAR(I)=0
          ENDIF
        ENDIF
C
C       CHECK FOR COLUMN SPAN
C
C       FOR RTF, THE COLUMN WIDTHS ARE CUMULATIVE, SO
C       SET TO WIDTH OF LAST COLUMN.
C
        IF(NCOLSP(I).LE.0)THEN
          GOTO8010
        ELSEIF(NCOLSP(I).EQ.1)THEN
          IWIDT=IWIDTH(I)
        ELSE
          IWIDT=IWIDTH(I+NCOLSP(I)-1)
        ENDIF
C
        IF(VALIGN(I).EQ.'b')THEN
          IF(IWIDT.LE.999)THEN
            WRITE(ICOUT,8013)IBASLC,IBASLC,IWIDT
          ELSEIF(IWIDT.LE.9999)THEN
            WRITE(ICOUT,8113)IBASLC,IBASLC,IWIDT
          ELSE
            WRITE(ICOUT,8213)IBASLC,IBASLC,IWIDT
          ENDIF
        ELSEIF(VALIGN(I).EQ.'c')THEN
          IF(IWIDT.LE.999)THEN
            WRITE(ICOUT,8012)IBASLC,IBASLC,IWIDT
          ELSEIF(IWIDT.LE.9999)THEN
            WRITE(ICOUT,8112)IBASLC,IBASLC,IWIDT
          ELSE
            WRITE(ICOUT,8212)IBASLC,IBASLC,IWIDT
          ENDIF
        ELSE
          IF(IWIDT.LE.999)THEN
            WRITE(ICOUT,8011)IBASLC,IBASLC,IWIDT
          ELSEIF(IWIDT.LE.9999)THEN
            WRITE(ICOUT,8111)IBASLC,IBASLC,IWIDT
          ELSE
            WRITE(ICOUT,8211)IBASLC,IBASLC,IWIDT
          ENDIF
        ENDIF
        CALL DPWRST('XXX','WRIT')
 8010 CONTINUE
C
 8021 FORMAT(A1,'pard',A1,'intbl',A1,'ql {')
 8022 FORMAT(A1,'pard',A1,'intbl',A1,'qc {')
 8023 FORMAT(A1,'pard',A1,'intbl',A1,'qr {')
      IFORMT=' '
      IFORMT(1:5)='(A  )'
 8027 FORMAT('}',A1,'cell')
      DO8020I=1,NHEAD
        IF(NCOLSP(I).LE.0)GOTO8020
        IF(ALIGN(I).EQ.'c' .OR. NCOLSP(I).GT.1)THEN
          WRITE(ICOUT,8022)IBASLC,IBASLC,IBASLC
        ELSEIF(ALIGN(I).EQ.'l')THEN
          WRITE(ICOUT,8021)IBASLC,IBASLC,IBASLC
        ELSE
          WRITE(ICOUT,8023)IBASLC,IBASLC,IBASLC
        ENDIF
        CALL DPWRST('XXX','WRIT')
        IF(NCHAR(I).GT.0)THEN
          WRITE(IFORMT(3:4),'(I2)')NCHAR(I)
          WRITE(ICOUT,IFORMT)IVALUE(I)(1:NCHAR(I))
          CALL DPWRST('XXX','WRIT')
        ELSE
          ITEMP=1
          WRITE(IFORMT(3:4),'(I2)')ITEMP
          WRITE(ICOUT,IFORMT) ' '
          CALL DPWRST('XXX','WRIT')
        ENDIF
        WRITE(ICOUT,8027)IBASLC
        CALL DPWRST('XXX','WRIT')
 8020 CONTINUE
C
 8039 FORMAT(A1,'row}')
      WRITE(ICOUT,8039)IBASLC
      CALL DPWRST('XXX','WRIT')
C
      RETURN
      END
      SUBROUTINE DPRTF5(IVALUE,NCHAR,AVALUE,NHEAD,IFLAG1)
C
C     PURPOSE--THIS ROUTINE IS A UTILUTY ROUTINE FOR CREATING
C              RTF OUTPUT.  THIS ROUTINE IS USED TO GENERATE
C              A DATA ROW FOR A TABLE.  THE FIRST FIELD CAN BE
C              A TEXT VALUE (FOR A ROW LABEL).
C
C     INPUT  ARGUMENTS--IVALUE  = THE CHARACTER STRING CONTAINING
C                                 THE TEXT FOR THE FIRST COLUMN.
C                     --NCHAR   = THE INTEGER ARRAY THAT SPECIFIES
C                                 THE NUMBER OF CHARACTERS IN THE
C                                 FIRST TEXT FIELD.
C                     --AVALUE  = A REAL ARRAY CONTAINING THE DATA
C                                 TO BE GENERATED.
C                     --NHEAD   = THE INTEGER VALUE THAT SPECIFIES
C                                 THE NUMBER OF NUMERIC VALUES.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABOARATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2005/2
C     ORIGINAL VERSION--FEBRUARY  2005.
C     UPDATED         --APRIL     2009. ADDITIONAL FORMATTING OPTIONS
C     UPDATED         --JANUARY   2011. MODIFY HOW FONTS ARE SET
C                                       1) SET PROPORTIONAL FONT FOR
C                                          FIRST COLUMN
C                                       2) SET FIXED FONT FOR SECOND
C                                          (NUMERIC COLUMN)
C                                       3) RESET PROPORTIONAL FONT
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*(*) IVALUE
      REAL AVALUE(*)
      INTEGER NCHAR
C
      PARAMETER (MAXHED=1024)
      INTEGER IWIDTH(MAXHED)
      INTEGER NUMDIG(MAXHED)
      CHARACTER*8 ALIGN(MAXHED)
      CHARACTER*8 VALIGN(MAXHED)
      COMMON/HTML4/IWIDTH,NUMDIG,ALIGN,VALIGN
C
      LOGICAL IFLAG1
C
      CHARACTER*1  IBASLC
      CHARACTER*20 IFORMT
C
      INCLUDE 'DPCOST.INC'
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      CALL DPCONA(92,IBASLC)
C
C     STEP 0: SET PROPORTIONAL FONT FOR CHARACTER COLUMN ONE
C
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
      WRITE(ICOUT,8091)IBASLC,ITEMP
      CALL DPWRST(ICOUT,'WRIT')
C
C
C  STEP 1: GENERATE A HEADER LINE
C
 8001 FORMAT('{',A1,'trowd',A1,'trgraph90')
      WRITE(ICOUT,8001)IBASLC,IBASLC
      CALL DPWRST('XXX','WRIT')
C
 8011 FORMAT(A1,'clvertalt',A1,'cellx',I3)
 8012 FORMAT(A1,'clvertalc',A1,'cellx',I3)
 8013 FORMAT(A1,'clvertalb',A1,'cellx',I3)
 8111 FORMAT(A1,'clvertalt',A1,'cellx',I4)
 8112 FORMAT(A1,'clvertalc',A1,'cellx',I4)
 8113 FORMAT(A1,'clvertalb',A1,'cellx',I4)
 8211 FORMAT(A1,'clvertalt',A1,'cellx',I5)
 8212 FORMAT(A1,'clvertalc',A1,'cellx',I5)
 8213 FORMAT(A1,'clvertalb',A1,'cellx',I5)
 8014 FORMAT(A1,'clbrdrt',A1,'brdrw15',A1,'brdrs')
 8015 FORMAT(A1,'clbrdrb',A1,'brdrw15',A1,'brdrs')
      NCOLS=NHEAD
      IF(NCHAR.GT.0)NCOLS=NCOLS+1
      DO8010I=1,NCOLS
        IF(IFLAG1)THEN
          WRITE(ICOUT,8015)IBASLC,IBASLC,IBASLC
          CALL DPWRST('XXX','WRIT')
        ENDIF
        IF(VALIGN(I).EQ.'b')THEN
          IF(IWIDTH(I).LE.999)THEN
            WRITE(ICOUT,8013)IBASLC,IBASLC,IWIDTH(I)
          ELSEIF(IWIDTH(I).LE.9999)THEN
            WRITE(ICOUT,8113)IBASLC,IBASLC,IWIDTH(I)
          ELSE
            WRITE(ICOUT,8213)IBASLC,IBASLC,IWIDTH(I)
          ENDIF
        ELSEIF(VALIGN(I).EQ.'c')THEN
          IF(IWIDTH(I).LE.999)THEN
            WRITE(ICOUT,8012)IBASLC,IBASLC,IWIDTH(I)
          ELSEIF(IWIDTH(I).LE.9999)THEN
            WRITE(ICOUT,8112)IBASLC,IBASLC,IWIDTH(I)
          ELSE
            WRITE(ICOUT,8212)IBASLC,IBASLC,IWIDTH(I)
          ENDIF
        ELSE
          IF(IWIDTH(I).LE.999)THEN
            WRITE(ICOUT,8011)IBASLC,IBASLC,IWIDTH(I)
          ELSEIF(IWIDTH(I).LE.9999)THEN
            WRITE(ICOUT,8111)IBASLC,IBASLC,IWIDTH(I)
          ELSE
            WRITE(ICOUT,8211)IBASLC,IBASLC,IWIDTH(I)
          ENDIF
        ENDIF
        CALL DPWRST('XXX','WRIT')
 8010 CONTINUE
C
 8021 FORMAT(A1,'pard',A1,'intbl',A1,'ql {')
 8022 FORMAT(A1,'pard',A1,'intbl',A1,'qc {')
 8023 FORMAT(A1,'pard',A1,'intbl',A1,'qr {')
      IFORMT=' '
      IFORMT(1:5)='(A  )'
 8027 FORMAT('}',A1,'cell')
C
C  PRINT ROW LABEL
C
      IF(NCHAR.GT.0)THEN
        IF(ALIGN(1).EQ.'l')THEN
          WRITE(ICOUT,8021)IBASLC,IBASLC,IBASLC
        ELSEIF(ALIGN(1).EQ.'c')THEN
          WRITE(ICOUT,8022)IBASLC,IBASLC,IBASLC
        ELSE
          WRITE(ICOUT,8023)IBASLC,IBASLC,IBASLC
        ENDIF
        CALL DPWRST('XXX','WRIT')
        WRITE(IFORMT(3:4),'(I2)')NCHAR
        WRITE(ICOUT,IFORMT)IVALUE(1:NCHAR)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,8027)IBASLC
        CALL DPWRST('XXX','WRIT')
        IADD=1
      ELSE
        IADD=0
      ENDIF
C
C  PRINT NUMERIC VALUES
C
 8091 FORMAT(a1,'f',I1)
      IF(IRTFFF.EQ.'Courier New')THEN
        ITEMP=1
      ELSEIF(IRTFFF.EQ.'Lucida Console')THEN
        ITEMP=8
      ENDIF 
      WRITE(ICOUT,8091)IBASLC,ITEMP
      CALL DPWRST(ICOUT,'WRIT')
C
C     APRIL 2009: SUPPORT THE FOLLOWING FORMATTING OPTIONS
C
C                  NUMDIG(I) > 0          => Fyy.xx FORMAT
C                  NUMDIG(I) = 0          => I12 FORMAT
C                  NUMDIG(I) = -1         => BLANK
C                  NUMDIG(I) = -2         => G15.7
C                  NUMDIG(I) = -3 to -20  => Eyy.xx
C                  NUMDIG(I) = -99        => '**'
C
 8035 FORMAT(1X)
 8031 FORMAT(G15.7)
 8033 FORMAT(I12)
 8037 FORMAT('**')
      DO8020I=1,NHEAD
        IF(ALIGN(I+IADD).EQ.'l')THEN
          WRITE(ICOUT,8021)IBASLC,IBASLC,IBASLC
        ELSEIF(ALIGN(I+IADD).EQ.'c')THEN
          WRITE(ICOUT,8022)IBASLC,IBASLC,IBASLC
        ELSE
          WRITE(ICOUT,8023)IBASLC,IBASLC,IBASLC
        ENDIF
        CALL DPWRST('XXX','WRIT')
C
        IF(NUMDIG(I+IADD).EQ.-1)THEN
          WRITE(ICOUT,8035)
          CALL DPWRST('XXX','WRIT')
        ELSEIF(NUMDIG(I+IADD).EQ.-99)THEN
          WRITE(ICOUT,8037)
          CALL DPWRST('XXX','WRIT')
        ELSE
          IXX=ABS(NUMDIG(I+IADD))
          IFORMT=' '
          NRIGHT=MIN(IXX,12)
          IF(ABS(AVALUE(I+IADD)).LT.10.0)THEN
            NLEFT=1
          ELSEIF(ABS(AVALUE(I+IADD)).LT.100.0)THEN
            NLEFT=2
          ELSEIF(ABS(AVALUE(I+IADD)).LT.1000.0)THEN
            NLEFT=3
          ELSEIF(ABS(AVALUE(I+IADD)).LT.10000.0)THEN
            NLEFT=4
          ELSEIF(ABS(AVALUE(I+IADD)).LT.100000.0)THEN
            NLEFT=5
          ELSEIF(ABS(AVALUE(I+IADD)).LT.1000000.0)THEN
            NLEFT=6
          ELSEIF(ABS(AVALUE(I+IADD)).LT.10000000.0)THEN
            NLEFT=7
          ELSEIF(ABS(AVALUE(I+IADD)).LT.100000000.0)THEN
            NLEFT=8
          ELSEIF(ABS(AVALUE(I+IADD)).LT.1000000000.0)THEN
            NLEFT=9
          ELSE
            NLEFT=10
          ENDIF
          IF(AVALUE(I+IADD).LT.0.0)NLEFT=NLEFT+1
          NTOT=NRIGHT+NLEFT+2
          IF(NUMDIG(I+IADD).GT.0)THEN
            IFORMT(1:8)='(F  .  )'
            WRITE(IFORMT(3:4),'(I2)')NTOT
            WRITE(IFORMT(6:7),'(I2)')NRIGHT
            WRITE(ICOUT,IFORMT)AVALUE(I+IADD)
            CALL DPWRST('XXX','WRIT')
          ELSEIF(NUMDIG(I+IADD).EQ.0)THEN
            IFORMT(1:5)='(I  )'
            WRITE(IFORMT(3:4),'(I2)')NLEFT
            IF(AVALUE(I+IADD).GE.0.0)THEN
              WRITE(ICOUT,IFORMT)INT(AVALUE(I+IADD)+0.5)
            ELSE
              WRITE(ICOUT,IFORMT)INT(AVALUE(I+IADD)-0.5)
            ENDIF
            CALL DPWRST('XXX','WRIT')
          ELSEIF(NUMDIG(I+IADD).EQ.-2)THEN
            IFORMT(1:7)='(G15.7)'
            WRITE(ICOUT,IFORMT)AVALUE(I+IADD)
            CALL DPWRST('XXX','WRIT')
          ELSEIF(NUMDIG(I+IADD).LT.-2 .AND. NUMDIG(I+IADD).GT.-20)THEN
            IFORMT(1:8)='(E  .  )'
            IXX=ABS(NUMDIG(I))
            IYY=IXX+8
            WRITE(IFORMT(3:4),'(I2)')IYY
            WRITE(IFORMT(6:7),'(I2)')IXX
            WRITE(ICOUT,IFORMT)AVALUE(I+IADD)
            CALL DPWRST('XXX','WRIT')
          ELSE
            WRITE(ICOUT,'(A1)') ' '
          ENDIF
        ENDIF
C
        WRITE(ICOUT,8027)IBASLC
        CALL DPWRST('XXX','WRIT')
 8020 CONTINUE
C
 8039 FORMAT(A1,'row}')
      WRITE(ICOUT,8039)IBASLC
      CALL DPWRST('XXX','WRIT')
C
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
      WRITE(ICOUT,8091)IBASLC,ITEMP
      CALL DPWRST(ICOUT,'WRIT')
C
      RETURN
      END
      SUBROUTINE DPRTF6(NHEAD)
C
C     PURPOSE--THIS ROUTINE IS A UTILUTY ROUTINE FOR CREATING
C              RTF OUTPUT.  THIS ROUTINE IS USED TO CLOSE A
C              TABLE (PRINT 2 BLANK LINES).
C     INPUT  ARGUMENTS--IHEAD  = THE CHARACTER STRING CONTAINING
C                                THE TEXT FOR THE HEADER
C                     --NHEAD  = THE INTEGER NUMBER THAT SPECIFIES
C                                THE NUMBER OF CHARACTERS IN THE
C                                HEADER.
C                     --CAPTN  = THE CHARACTER STRING CONTAINING
C                                THE CAPTION.
C                     --NCAP   = THE INTEGER NUMBER THAT SPECIFIES
C                                THE NUMBER OF CHARACTERS IN THE
C                                CAPTION.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABOARATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2005/2
C     ORIGINAL VERSION--FEBRUARY  2005.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*1  IBASLC
C
      INCLUDE 'DPCOST.INC'
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
C  STEP 1: WRITE SOME LINE BREAKS
C
C
      CALL DPCONA(92,IBASLC)
 8009 FORMAT(A1,'line ',A1,'line')
      WRITE(ICOUT,8009)IBASLC,IBASLC
      CALL DPWRST('XXX','WRIT')
C
      RETURN
      END
      SUBROUTINE DPRTF7(IHEAD,NHEAD,AVAL,NUMDIG)
C
C     PURPOSE--THIS ROUTINE IS A UTILUTY ROUTINE FOR CREATING
C              RTF OUTPUT.  THIS ROUTINE IS USED TO WRITE A
C              A SINGLE LINE OF OUTPUT.
C     INPUT  ARGUMENTS--IHEAD  = THE CHARACTER STRING CONTAINING
C                                THE TEXT FOR THE LINE
C                     --NHEAD  = THE INTEGER NUMBER THAT SPECIFIES
C                                THE NUMBER OF CHARACTERS IN THE
C                                LINE.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABOARATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2005/2
C     ORIGINAL VERSION--FEBRUARY  2005.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*(*) IHEAD
C
      CHARACTER*1  IBASLC
      CHARACTER*1  IQUOTE
      CHARACTER*25 IFORMT
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
C  STEP 1: END ASIS MODE AND WRITE A HEADER
C
      CALL DPCONA(92,IBASLC)
      IFORMT=' '
      ICOUT=' '
C
C  STEP 2: START TABLE AND DEFINE A CAPTION
C
 8005 FORMAT('{',A1,'ql ')
 8007 FORMAT(A1,'line')
C
      IF(NHEAD.GE.1)THEN
        IF(AVAL.NE.CPUMIN)THEN
          IF(NUMDIG.GT.0)THEN
            AVALT=RND(AVAL,NUMDIG)
            IXX=NUMDIG
            IYY=IXX+8
            IFORMT(1:21)='(A  ,2X,F  .  ,2X,A1)'
            WRITE(IFORMT(3:4),'(I2)')NHEAD
            WRITE(IFORMT(10:11),'(I2)')IYY
            WRITE(IFORMT(13:14),'(I2)')IXX
            WRITE(ICOUT,8005)IBASLC
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,IFORMT)IHEAD(1:NHEAD),AVALT,'}'
            CALL DPWRST('XXX','WRIT')
          ELSEIF(NUMDIG.LT.0)THEN
            NUMDI2=-NUMDIG
            AVALT=RND(AVAL,NUMDI2)
            IXX=-NUMDIG
            IYY=IXX+8
            IFORMT(1:21)='(A  ,2X,E  .  ,2X,A1)'
            WRITE(IFORMT(3:4),'(I2)')NHEAD
            WRITE(IFORMT(10:11),'(I2)')IYY
            WRITE(IFORMT(13:14),'(I2)')IXX
            WRITE(ICOUT,8005)IBASLC
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,IFORMT)IHEAD(1:NHEAD),AVALT,'}'
            CALL DPWRST('XXX','WRIT')
          ELSEIF(NUMDIG.EQ.0)THEN
            IF(AVAL.GE.0.0)THEN
              IVALT=INT(AVAL + 0.5)
            ELSE
              IVALT=INT(AVAL - 0.5)
            ENDIF
            IFORMT(1:18)='(A  ,2X,I10,A1)'
            WRITE(IFORMT(3:4),'(I2)')NHEAD
            WRITE(ICOUT,IFORMT)IHEAD(1:NHEAD),IVALT,'{'
            CALL DPWRST('XXX','WRIT')
          ENDIF
        ELSE
          IFORMT(1:11)='(A  ,2X,A1)'
          WRITE(IFORMT(3:4),'(I2)')NHEAD
          WRITE(ICOUT,8005)IBASLC
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,IFORMT)IHEAD(1:NHEAD),'}'
          CALL DPWRST('XXX','WRIT')
        ENDIF
        WRITE(ICOUT,8007)IBASLC
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPRTF8(IHEAD,NHEAD,ITEMP,IFLAG1)
C
C     PURPOSE--THIS ROUTINE IS A UTILUTY ROUTINE FOR CREATING
C              RTF OUTPUT.  THIS ROUTINE IS USED TO INITIATE
C              THE RTF OUTPUT AND GENERATE AN OVERALL TITLE.
C     INPUT  ARGUMENTS--IHEAD  = THE CHARACTER STRING CONTAINING
C                                THE TEXT FOR THE HEADER
C                     --NHEAD  = THE INTEGER NUMBER THAT SPECIFIES
C                                THE NUMBER OF CHARACTERS IN THE
C                                HEADER.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABOARATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2005/2
C     ORIGINAL VERSION--FEBRUARY  2005.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      LOGICAL IFLAG1
      LOGICAL IFLAG2
C
      CHARACTER*(*) IHEAD
C
      CHARACTER*1  IBASLC
      CHARACTER*1  IQUOTE
      CHARACTER*40 IFORMT
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
C  STEP 1: END ASIS MODE AND WRITE A HEADER
C
      CALL DPCONA(92,IBASLC)
      CALL DPCONA(39,IQUOTE)
C
 8001 FORMAT(A1,'par}')
 8003 FORMAT(A1,'pagebb')
 8004 FORMAT(A1,'f',I1)
 8014 FORMAT(A1,'f',I2)
 8005 FORMAT('{',A1,'pard')
      IF(IFLAG1)THEN
CCCCC   WRITE(ICOUT,8001)IBASLC
CCCCC   CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,8005)IBASLC
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,8003)IBASLC
        CALL DPWRST('XXX','WRIT')
        IF(ITEMP.LE.9)THEN
          WRITE(ICOUT,8004)IBASLC,ITEMP
          CALL DPWRST('XXX','WRIT')
        ELSE
          WRITE(ICOUT,8014)IBASLC,ITEMP
          CALL DPWRST('XXX','WRIT')
        ENDIF
CCCCC   WRITE(ICOUT,8005)IBASLC
CCCCC   CALL DPWRST('XXX','WRIT')
      ENDIF
C
      IF(NHEAD.GE.1)THEN
        IFORMT=' '
        IFORMT='( { ,A1, qc  ,A   , }  ,A1, line )'
        IFORMT(2:2)=IQUOTE
        IFORMT(4:4)=IQUOTE
        IFORMT(9:9)=IQUOTE
        IFORMT(13:13)=IQUOTE
        IFORMT(20:20)=IQUOTE
        IFORMT(23:23)=IQUOTE
        IFORMT(28:28)=IQUOTE
        IFORMT(33:33)=IQUOTE
        WRITE(IFORMT(16:18),'(I3)')NHEAD
        WRITE(ICOUT,IFORMT)IBASLC,IHEAD(1:NHEAD),IBASLC
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,8001)IBASLC
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPRTF9(IVALUE,NCHAR,AVALUE,NHEAD,IFLAG1,IVAL2,NCHAR2)
C
C     PURPOSE--THIS ROUTINE IS A UTILUTY ROUTINE FOR CREATING
C              RTF OUTPUT.  THIS ROUTINE IS USED TO GENERATE
C              A DATA ROW FOR A TABLE.  THE FIRST FIELD CAN BE
C              A TEXT VALUE (FOR A ROW LABEL).  IN ADDITION, THE
C              LAST FIELD IS ALSO A CHARACTER FIELD.
C
C     INPUT  ARGUMENTS--IVALUE  = THE CHARACTER STRING CONTAINING
C                                 THE TEXT FOR THE FIRST COLUMN.
C                     --NCHAR   = THE INTEGER ARRAY THAT SPECIFIES
C                                 THE NUMBER OF CHARACTERS IN THE
C                                 FIRST TEXT FIELD.
C                     --AVALUE  = A REAL ARRAY CONTAINING THE DATA
C                                 TO BE GENERATED.
C                     --NHEAD   = THE INTEGER VALUE THAT SPECIFIES
C                                 THE NUMBER OF NUMERIC VALUES.
C                     --IVAL2   = THE CHARACTER STRING CONTAINING
C                                 THE TEXT FOR THE LAST COLUMN.
C                     --NCHAR2  = THE INTEGER ARRAY THAT SPECIFIES
C                                 THE NUMBER OF CHARACTERS IN THE
C                                 LAST TEXT FIELD.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABOARATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2006/11
C     ORIGINAL VERSION--NOVEMBER  2006.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*(*) IVALUE
      CHARACTER*(*) IVAL2
      REAL AVALUE(*)
      INTEGER NCHAR
      INTEGER NCHAR2
C
      PARAMETER (MAXHED=1024)
      INTEGER IWIDTH(MAXHED)
      INTEGER NUMDIG(MAXHED)
      CHARACTER*8 ALIGN(MAXHED)
      CHARACTER*8 VALIGN(MAXHED)
      COMMON/HTML4/IWIDTH,NUMDIG,ALIGN,VALIGN
C
      LOGICAL IFLAG1
C
      CHARACTER*1  IBASLC
      CHARACTER*20 IFORMT
C
      INCLUDE 'DPCOST.INC'
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      CALL DPCONA(92,IBASLC)
C
C  STEP 1: GENERATE A HEADER LINE
C
 8001 FORMAT('{',A1,'trowd',A1,'trgraph90')
      WRITE(ICOUT,8001)IBASLC,IBASLC
      CALL DPWRST('XXX','WRIT')
C
 8011 FORMAT(A1,'clvertalt',A1,'cellx',I3)
 8012 FORMAT(A1,'clvertalc',A1,'cellx',I3)
 8013 FORMAT(A1,'clvertalb',A1,'cellx',I3)
 8111 FORMAT(A1,'clvertalt',A1,'cellx',I4)
 8112 FORMAT(A1,'clvertalc',A1,'cellx',I4)
 8113 FORMAT(A1,'clvertalb',A1,'cellx',I4)
 8211 FORMAT(A1,'clvertalt',A1,'cellx',I5)
 8212 FORMAT(A1,'clvertalc',A1,'cellx',I5)
 8213 FORMAT(A1,'clvertalb',A1,'cellx',I5)
 8014 FORMAT(A1,'clbrdrt',A1,'brdrw15',A1,'brdrs')
 8015 FORMAT(A1,'clbrdrb',A1,'brdrw15',A1,'brdrs')
      NCOLS=NHEAD
      IF(NCHAR.GT.0)NCOLS=NCOLS+1
      DO8010I=1,NCOLS+1
        IF(IFLAG1)THEN
          WRITE(ICOUT,8015)IBASLC,IBASLC,IBASLC
          CALL DPWRST('XXX','WRIT')
        ENDIF
        IF(VALIGN(I).EQ.'b')THEN
          IF(IWIDTH(I).LE.999)THEN
            WRITE(ICOUT,8013)IBASLC,IBASLC,IWIDTH(I)
          ELSEIF(IWIDTH(I).LE.9999)THEN
            WRITE(ICOUT,8113)IBASLC,IBASLC,IWIDTH(I)
          ELSE
            WRITE(ICOUT,8213)IBASLC,IBASLC,IWIDTH(I)
          ENDIF
        ELSEIF(VALIGN(I).EQ.'c')THEN
          IF(IWIDTH(I).LE.999)THEN
            WRITE(ICOUT,8012)IBASLC,IBASLC,IWIDTH(I)
          ELSEIF(IWIDTH(I).LE.9999)THEN
            WRITE(ICOUT,8112)IBASLC,IBASLC,IWIDTH(I)
          ELSE
            WRITE(ICOUT,8212)IBASLC,IBASLC,IWIDTH(I)
          ENDIF
        ELSE
          IF(IWIDTH(I).LE.999)THEN
            WRITE(ICOUT,8011)IBASLC,IBASLC,IWIDTH(I)
          ELSEIF(IWIDTH(I).LE.9999)THEN
            WRITE(ICOUT,8111)IBASLC,IBASLC,IWIDTH(I)
          ELSE
            WRITE(ICOUT,8211)IBASLC,IBASLC,IWIDTH(I)
          ENDIF
        ENDIF
        CALL DPWRST('XXX','WRIT')
 8010 CONTINUE
C
 8021 FORMAT(A1,'pard',A1,'intbl',A1,'ql {')
 8022 FORMAT(A1,'pard',A1,'intbl',A1,'qc {')
 8023 FORMAT(A1,'pard',A1,'intbl',A1,'qr {')
      IFORMT=' '
      IFORMT(1:5)='(A  )'
 8027 FORMAT('}',A1,'cell')
C
C  PRINT ROW LABEL
C
      IF(NCHAR.GT.0)THEN
        IF(ALIGN(1).EQ.'l')THEN
          WRITE(ICOUT,8021)IBASLC,IBASLC,IBASLC
        ELSEIF(ALIGN(1).EQ.'c')THEN
          WRITE(ICOUT,8022)IBASLC,IBASLC,IBASLC
        ELSE
          WRITE(ICOUT,8023)IBASLC,IBASLC,IBASLC
        ENDIF
        CALL DPWRST('XXX','WRIT')
        WRITE(IFORMT(3:4),'(I2)')NCHAR
        WRITE(ICOUT,IFORMT)IVALUE(1:NCHAR)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,8027)IBASLC
        CALL DPWRST('XXX','WRIT')
        IADD=1
      ELSE
        IADD=0
      ENDIF
C
C  PRINT NUMERIC VALUES
C
 8091 FORMAT(a1,'f',I1)
      IF(IRTFFF.EQ.'Courier New')THEN
        ITEMP=1
      ELSEIF(IRTFFF.EQ.'Lucida Console')THEN
        ITEMP=8
      ENDIF 
      WRITE(ICOUT,8091)IBASLC,ITEMP
      CALL DPWRST(ICOUT,'WRIT')
 8035 FORMAT(1X)
 8031 FORMAT(G15.7)
 8033 FORMAT(I12)
      DO8020I=1,NHEAD
        IF(ALIGN(I+IADD).EQ.'l')THEN
          WRITE(ICOUT,8021)IBASLC,IBASLC,IBASLC
        ELSEIF(ALIGN(I+IADD).EQ.'c')THEN
          WRITE(ICOUT,8022)IBASLC,IBASLC,IBASLC
        ELSE
          WRITE(ICOUT,8023)IBASLC,IBASLC,IBASLC
        ENDIF
        CALL DPWRST('XXX','WRIT')
C
        IFORMT=' '
        NRIGHT=MIN(NUMDIG(I+IADD),9)
        IF(ABS(AVALUE(I+IADD)).LT.10.0)THEN
          NLEFT=1
        ELSEIF(ABS(AVALUE(I+IADD)).LT.100.0)THEN
          NLEFT=2
        ELSEIF(ABS(AVALUE(I+IADD)).LT.1000.0)THEN
          NLEFT=3
        ELSEIF(ABS(AVALUE(I+IADD)).LT.10000.0)THEN
          NLEFT=4
        ELSEIF(ABS(AVALUE(I+IADD)).LT.100000.0)THEN
          NLEFT=5
        ELSEIF(ABS(AVALUE(I+IADD)).LT.1000000.0)THEN
          NLEFT=6
        ELSE
          NLEFT=7
        ENDIF
        IF(AVALUE(I+IADD).LT.0.0)NLEFT=NLEFT+1
        NTOT=NRIGHT+NLEFT+2
        IF(NUMDIG(I+IADD).GT.0)THEN
          IFORMT(1:7)='(F  . )'
          WRITE(IFORMT(3:4),'(I2)')NTOT
          WRITE(IFORMT(6:6),'(I1)')NRIGHT
          WRITE(ICOUT,IFORMT)AVALUE(I+IADD)
          CALL DPWRST('XXX','WRIT')
        ELSEIF(NUMDIG(I+IADD).EQ.0)THEN
          IFORMT(1:5)='(I  )'
          WRITE(IFORMT(3:4),'(I2)')NLEFT
          IF(AVALUE(I+IADD).GE.0.0)THEN
            WRITE(ICOUT,IFORMT)INT(AVALUE(I+IADD)+0.5)
          ELSE
            WRITE(ICOUT,IFORMT)INT(AVALUE(I+IADD)-0.5)
          ENDIF
          CALL DPWRST('XXX','WRIT')
        ELSEIF(NUMDIG(I+IADD).EQ.-1)THEN
          WRITE(ICOUT,8035)
          CALL DPWRST('XXX','WRIT')
        ELSEIF(NUMDIG(I+IADD).EQ.-2)THEN
          IFORMT(1:7)='(G  .7)'
          NTOT=12+NLEFT
          WRITE(IFORMT(3:4),'(I2)')NTOT
          WRITE(ICOUT,IFORMT)AVALUE(I+IADD)
          CALL DPWRST('XXX','WRIT')
        ELSE
          WRITE(ICOUT,'(A1)') ' '
          CALL DPWRST('XXX','WRIT')
        ENDIF
C
        WRITE(ICOUT,8027)IBASLC
        CALL DPWRST('XXX','WRIT')
 8020 CONTINUE
C
C  PRINT CHARACTER DATA IN LAST FIELD
C
      IF(NCHAR2.GT.0)THEN
        IFORMT=' '
        IFORMT(1:5)='(A  )'
        IF(ALIGN(NCOLS+1).EQ.'l')THEN
          WRITE(ICOUT,8021)IBASLC,IBASLC,IBASLC
        ELSEIF(ALIGN(NCOLS+1).EQ.'c')THEN
          WRITE(ICOUT,8022)IBASLC,IBASLC,IBASLC
        ELSE
          WRITE(ICOUT,8023)IBASLC,IBASLC,IBASLC
        ENDIF
        CALL DPWRST('XXX','WRIT')
        IFORMT(3:4)='  '
        WRITE(IFORMT(3:4),'(I2)')NCHAR2
        WRITE(ICOUT,IFORMT)IVAL2(1:NCHAR2)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,8027)IBASLC
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
 8039 FORMAT(A1,'row}')
      WRITE(ICOUT,8039)IBASLC
      CALL DPWRST('XXX','WRIT')
C
      IF(IRTFFF.EQ.'Times New Roman')THEN
        ITEMP=0
      ELSEIF(IRTFFF.EQ.'Lucida Sans')THEN
        ITEMP=6
      ELSEIF(IRTFFF.EQ.'Arial')THEN
        ITEMP=2
      ELSEIF(IRTFFF.EQ.'Bookman')THEN
        ITEMP=3
      ELSEIF(IRTFFF.EQ.'Georgia')THEN
        ITEMP=4
      ELSEIF(IRTFFF.EQ.'Tahoma')THEN
        ITEMP=5
      ELSEIF(IRTFFF.EQ.'Verdana')THEN
        ITEMP=7
      ENDIF 
      WRITE(ICOUT,8091)IBASLC,ITEMP
      CALL DPWRST(ICOUT,'WRIT')
C
      RETURN
      END
      SUBROUTINE DPRTFA(IVALUE,NCHAR,NHEAD,IFLAG1)
C
C     PURPOSE--THIS ROUTINE IS A UTILUTY ROUTINE FOR CREATING
C              RTF OUTPUT.  THIS ROUTINE IS USED TO GENERATE
C              A DATA ROW FOR A TABLE.  FOR THIS ROUTINE, EACH
C              OF THE FIELDS WILL BE GIVEN AS CHARACTER STRINGS.
C
C     INPUT  ARGUMENTS--IVALUE  = THE CHARACTER STRING CONTAINING
C                                 THE TEXT FOR THE FIRST COLUMN.
C                     --NCHAR   = THE INTEGER ARRAY THAT SPECIFIES
C                                 THE NUMBER OF CHARACTERS IN THE
C                                 FIRST TEXT FIELD.
C                     --NHEAD   = THE INTEGER VALUE THAT SPECIFIES
C                                 THE NUMBER OF NUMERIC VALUES.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABOARATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2007/3
C     ORIGINAL VERSION--MARCH     2007.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*(*) IVALUE(*)
      INTEGER NCHAR(*)
C
      PARAMETER (MAXHED=1024)
      INTEGER IWIDTH(MAXHED)
      INTEGER NUMDIG(MAXHED)
      CHARACTER*8 ALIGN(MAXHED)
      CHARACTER*8 VALIGN(MAXHED)
      COMMON/HTML4/IWIDTH,NUMDIG,ALIGN,VALIGN
C
      LOGICAL IFLAG1
C
      CHARACTER*1  IBASLC
      CHARACTER*20 IFORMT
C
      INCLUDE 'DPCOST.INC'
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      CALL DPCONA(92,IBASLC)
C
C  STEP 1: GENERATE A HEADER LINE
C
 8001 FORMAT('{',A1,'trowd',A1,'trgraph90')
      WRITE(ICOUT,8001)IBASLC,IBASLC
      CALL DPWRST('XXX','WRIT')
C
 8011 FORMAT(A1,'clvertalt',A1,'cellx',I3)
 8012 FORMAT(A1,'clvertalc',A1,'cellx',I3)
 8013 FORMAT(A1,'clvertalb',A1,'cellx',I3)
 8111 FORMAT(A1,'clvertalt',A1,'cellx',I4)
 8112 FORMAT(A1,'clvertalc',A1,'cellx',I4)
 8113 FORMAT(A1,'clvertalb',A1,'cellx',I4)
 8211 FORMAT(A1,'clvertalt',A1,'cellx',I5)
 8212 FORMAT(A1,'clvertalc',A1,'cellx',I5)
 8213 FORMAT(A1,'clvertalb',A1,'cellx',I5)
 8014 FORMAT(A1,'clbrdrt',A1,'brdrw15',A1,'brdrs')
 8015 FORMAT(A1,'clbrdrb',A1,'brdrw15',A1,'brdrs')
C
      NCOLS=NHEAD
      DO8010I=1,NCOLS
        IF(IFLAG1)THEN
          WRITE(ICOUT,8015)IBASLC,IBASLC,IBASLC
          CALL DPWRST('XXX','WRIT')
        ENDIF
        IF(VALIGN(I).EQ.'b')THEN
          IF(IWIDTH(I).LE.999)THEN
            WRITE(ICOUT,8013)IBASLC,IBASLC,IWIDTH(I)
          ELSEIF(IWIDTH(I).LE.9999)THEN
            WRITE(ICOUT,8113)IBASLC,IBASLC,IWIDTH(I)
          ELSE
            WRITE(ICOUT,8213)IBASLC,IBASLC,IWIDTH(I)
          ENDIF
        ELSEIF(VALIGN(I).EQ.'c')THEN
          IF(IWIDTH(I).LE.999)THEN
            WRITE(ICOUT,8012)IBASLC,IBASLC,IWIDTH(I)
          ELSEIF(IWIDTH(I).LE.9999)THEN
            WRITE(ICOUT,8112)IBASLC,IBASLC,IWIDTH(I)
          ELSE
            WRITE(ICOUT,8212)IBASLC,IBASLC,IWIDTH(I)
          ENDIF
        ELSE
          IF(IWIDTH(I).LE.999)THEN
            WRITE(ICOUT,8011)IBASLC,IBASLC,IWIDTH(I)
          ELSEIF(IWIDTH(I).LE.9999)THEN
            WRITE(ICOUT,8111)IBASLC,IBASLC,IWIDTH(I)
          ELSE
            WRITE(ICOUT,8211)IBASLC,IBASLC,IWIDTH(I)
          ENDIF
        ENDIF
        CALL DPWRST('XXX','WRIT')
 8010 CONTINUE
C
 8021 FORMAT(A1,'pard',A1,'intbl',A1,'ql {')
 8022 FORMAT(A1,'pard',A1,'intbl',A1,'qc {')
 8023 FORMAT(A1,'pard',A1,'intbl',A1,'qr {')
      IFORMT=' '
      IFORMT(1:5)='(A  )'
 8027 FORMAT('}',A1,'cell')
C
C  PRINT ROW LABEL
C
      IF(IRTFFF.EQ.'Courier New')THEN
        ITEMP=1
      ELSEIF(IRTFFF.EQ.'Lucida Console')THEN
        ITEMP=8
      ENDIF 
      WRITE(ICOUT,8091)IBASLC,ITEMP
      CALL DPWRST(ICOUT,'WRIT')
C
      DO8020I=1,NHEAD
        IF(NCHAR(I).GT.0)THEN
          IF(ALIGN(I).EQ.'l')THEN
            WRITE(ICOUT,8021)IBASLC,IBASLC,IBASLC
          ELSEIF(ALIGN(I).EQ.'c')THEN
            WRITE(ICOUT,8022)IBASLC,IBASLC,IBASLC
          ELSE
            WRITE(ICOUT,8023)IBASLC,IBASLC,IBASLC
          ENDIF
          CALL DPWRST('XXX','WRIT')
          WRITE(IFORMT(3:4),'(I2)')NCHAR(I)
          WRITE(ICOUT,IFORMT)IVALUE(I)(1:NCHAR(I))
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,8027)IBASLC
          CALL DPWRST('XXX','WRIT')
        ENDIF
 8020 CONTINUE
C
 8039 FORMAT(A1,'row}')
      WRITE(ICOUT,8039)IBASLC
      CALL DPWRST('XXX','WRIT')
C
      IF(IRTFFF.EQ.'Times New Roman')THEN
        ITEMP=0
      ELSEIF(IRTFFF.EQ.'Lucida Sans')THEN
        ITEMP=6
      ELSEIF(IRTFFF.EQ.'Arial')THEN
        ITEMP=2
      ELSEIF(IRTFFF.EQ.'Bookman')THEN
        ITEMP=3
      ELSEIF(IRTFFF.EQ.'Georgia')THEN
        ITEMP=4
      ELSEIF(IRTFFF.EQ.'Tahoma')THEN
        ITEMP=5
      ELSEIF(IRTFFF.EQ.'Verdana')THEN
        ITEMP=7
      ENDIF 
 8091 FORMAT(a1,'f',I1)
      WRITE(ICOUT,8091)IBASLC,ITEMP
      CALL DPWRST(ICOUT,'WRIT')
C
      RETURN
      END
      SUBROUTINE DPRTFX(IVALUE,NCHAR,AVALUE,NHEAD,ITYPE,IFLAGA,IFLAGB)
C
C     PURPOSE--THIS ROUTINE IS A UTILUTY ROUTINE FOR CREATING
C              RTF OUTPUT.  THIS ROUTINE IS USED TO GENERATE
C              A DATA ROW FOR A TABLE.  IT IS SIMILAR TO DPRTF5,
C              BUT IT ALLOWS CHARACTER AND NUMERIC FIELDS TO BE
C              MIXED.
C
C     INPUT  ARGUMENTS--IVALUE  = THE ARRAY OF CHARACTER STRINGS CONTAINING
C                                 THE TEXT FOR THE CHARACTER FIELDS.
C                     --NCHAR   = THE INTEGER ARRAY THAT SPECIFIES
C                                 THE NUMBER OF CHARACTERS IN THE
C                                 TEXT FIELDS.
C                     --AVALUE  = A REAL ARRAY CONTAINING THE DATA
C                                 FOR THE NUMERIC FIELDS.
C                     --NHEAD   = THE INTEGER VALUE THAT SPECIFIES
C                                 THE NUMBER OF COMBINED NUMERIC AND
C                                 TEXT FIELDS.
C                     --IFLAGA  = A LOGICIAL VARIABLE THAT SPECIFIES
C                                 WHETHER A LINE IS DRAWN BEFORE THE ROW.
C                     --IFLAGB  = A LOGICIAL VARIABLE THAT SPECIFIES
C                                 WHETHER A LINE IS DRAWN AFTER THE ROW.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABOARATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2008/10
C     ORIGINAL VERSION--OCTOBER   2008.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*(*) IVALUE(*)
      CHARACTER*4   ITYPE(*)
      REAL AVALUE(*)
      INTEGER NCHAR(*)
C
      PARAMETER (MAXHED=1024)
      INTEGER IWIDTH(MAXHED)
      INTEGER NUMDIG(MAXHED)
      CHARACTER*8 ALIGN(MAXHED)
      CHARACTER*8 VALIGN(MAXHED)
      COMMON/HTML4/IWIDTH,NUMDIG,ALIGN,VALIGN
C
      LOGICAL IFLAGA
      LOGICAL IFLAGB
C
      CHARACTER*1  IBASLC
      CHARACTER*20 IFORMT
C
      INCLUDE 'DPCOST.INC'
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      CALL DPCONA(92,IBASLC)
C
C  STEP 1: GENERATE A HEADER LINE
C
 8001 FORMAT('{',A1,'trowd',A1,'trgraph90')
      WRITE(ICOUT,8001)IBASLC,IBASLC
      CALL DPWRST('XXX','WRIT')
C
 8011 FORMAT(A1,'clvertalt',A1,'cellx',I3)
 8012 FORMAT(A1,'clvertalc',A1,'cellx',I3)
 8013 FORMAT(A1,'clvertalb',A1,'cellx',I3)
 8111 FORMAT(A1,'clvertalt',A1,'cellx',I4)
 8112 FORMAT(A1,'clvertalc',A1,'cellx',I4)
 8113 FORMAT(A1,'clvertalb',A1,'cellx',I4)
 8211 FORMAT(A1,'clvertalt',A1,'cellx',I5)
 8212 FORMAT(A1,'clvertalc',A1,'cellx',I5)
 8213 FORMAT(A1,'clvertalb',A1,'cellx',I5)
 8014 FORMAT(A1,'clbrdrt',A1,'brdrw15',A1,'brdrs')
 8015 FORMAT(A1,'clbrdrb',A1,'brdrw15',A1,'brdrs')
C
      NCOLS=NHEAD
C
      DO8010I=1,NCOLS
        IF(IFLAGB)THEN
          WRITE(ICOUT,8014)IBASLC,IBASLC,IBASLC
          CALL DPWRST('XXX','WRIT')
        ENDIF
        IF(IFLAGA)THEN
          WRITE(ICOUT,8015)IBASLC,IBASLC,IBASLC
          CALL DPWRST('XXX','WRIT')
        ENDIF
        IF(VALIGN(I).EQ.'b')THEN
          IF(IWIDTH(I).LE.999)THEN
            WRITE(ICOUT,8013)IBASLC,IBASLC,IWIDTH(I)
          ELSEIF(IWIDTH(I).LE.9999)THEN
            WRITE(ICOUT,8113)IBASLC,IBASLC,IWIDTH(I)
          ELSE
            WRITE(ICOUT,8213)IBASLC,IBASLC,IWIDTH(I)
          ENDIF
        ELSEIF(VALIGN(I).EQ.'c')THEN
          IF(IWIDTH(I).LE.999)THEN
            WRITE(ICOUT,8012)IBASLC,IBASLC,IWIDTH(I)
          ELSEIF(IWIDTH(I).LE.9999)THEN
            WRITE(ICOUT,8112)IBASLC,IBASLC,IWIDTH(I)
          ELSE
            WRITE(ICOUT,8212)IBASLC,IBASLC,IWIDTH(I)
          ENDIF
        ELSE
          IF(IWIDTH(I).LE.999)THEN
            WRITE(ICOUT,8011)IBASLC,IBASLC,IWIDTH(I)
          ELSEIF(IWIDTH(I).LE.9999)THEN
            WRITE(ICOUT,8111)IBASLC,IBASLC,IWIDTH(I)
          ELSE
            WRITE(ICOUT,8211)IBASLC,IBASLC,IWIDTH(I)
          ENDIF
        ENDIF
        CALL DPWRST('XXX','WRIT')
 8010 CONTINUE
C
 8021 FORMAT(A1,'pard',A1,'intbl',A1,'ql {')
 8022 FORMAT(A1,'pard',A1,'intbl',A1,'qc {')
 8023 FORMAT(A1,'pard',A1,'intbl',A1,'qr {')
 8027 FORMAT('}',A1,'cell')
C
 8091 FORMAT(a1,'f',I1)
 8035 FORMAT(1X)
 8031 FORMAT(G15.7)
 8033 FORMAT(I12)
 8036 FORMAT(A2)
C
      ICNTA=0
      ICNTN=0
      DO8020I=1,NHEAD
C
        IF(ITYPE(I).EQ.'ALPH')THEN
C
C         PRINT CHARACTER FIELD
C
          IFORMT=' '
          IFORMT(1:5)='(A  )'
          ICNTA=ICNTA+1
          IF(NCHAR(ICNTA).GT.0)THEN
            IF(ALIGN(I).EQ.'l')THEN
              WRITE(ICOUT,8021)IBASLC,IBASLC,IBASLC
            ELSEIF(ALIGN(I).EQ.'c')THEN
              WRITE(ICOUT,8022)IBASLC,IBASLC,IBASLC
            ELSE
              WRITE(ICOUT,8023)IBASLC,IBASLC,IBASLC
            ENDIF
            CALL DPWRST('XXX','WRIT')
            WRITE(IFORMT(3:4),'(I2)')NCHAR(ICNTA)
            WRITE(ICOUT,IFORMT)IVALUE(ICNTA)(1:NCHAR(ICNTA))
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,8027)IBASLC
            CALL DPWRST('XXX','WRIT')
          ENDIF
        ELSE
C
C         PRINT NUMERIC FIELD
C
          ICNTN=ICNTN+1
          IF(IRTFFF.EQ.'Courier New')THEN
            ITEMP=1
          ELSEIF(IRTFFF.EQ.'Lucida Console')THEN
            ITEMP=8
          ENDIF 
          WRITE(ICOUT,8091)IBASLC,ITEMP
          CALL DPWRST(ICOUT,'WRIT')
C
          IF(ALIGN(I).EQ.'l')THEN
            WRITE(ICOUT,8021)IBASLC,IBASLC,IBASLC
          ELSEIF(ALIGN(I).EQ.'c')THEN
            WRITE(ICOUT,8022)IBASLC,IBASLC,IBASLC
          ELSE
            WRITE(ICOUT,8023)IBASLC,IBASLC,IBASLC
          ENDIF
          CALL DPWRST('XXX','WRIT')
C
          IFORMT=' '
          NRIGHT=MIN(ABS(NUMDIG(I)),9)
          IF(ABS(AVALUE(ICNTN)).LT.10.0)THEN
            NLEFT=1
          ELSEIF(ABS(AVALUE(ICNTN)).LT.100.0)THEN
            NLEFT=2
          ELSEIF(ABS(AVALUE(ICNTN)).LT.1000.0)THEN
            NLEFT=3
          ELSEIF(ABS(AVALUE(ICNTN)).LT.10000.0)THEN
            NLEFT=4
          ELSEIF(ABS(AVALUE(ICNTN)).LT.100000.0)THEN
            NLEFT=5
          ELSEIF(ABS(AVALUE(ICNTN)).LT.1000000.0)THEN
            NLEFT=6
          ELSE
            NLEFT=7
          ENDIF
          NTOT=NRIGHT+NLEFT+2
          IF(NUMDIG(I).GT.0)THEN
            IFORMT(1:7)='(F  . )'
            WRITE(IFORMT(3:4),'(I2)')NTOT
            WRITE(IFORMT(6:6),'(I1)')NRIGHT
            WRITE(ICOUT,IFORMT)AVALUE(ICNTN)
            CALL DPWRST('XXX','WRIT')
          ELSEIF(NUMDIG(I).EQ.0)THEN
            IFORMT(1:5)='(I  )'
            WRITE(IFORMT(3:4),'(I2)')NLEFT
            IF(AVALUE(ICNTN).GE.0.0)THEN
              WRITE(ICOUT,IFORMT)INT(AVALUE(ICNTN)+0.5)
            ELSE
              WRITE(ICOUT,IFORMT)INT(AVALUE(ICNTN)-0.5)
            ENDIF
            CALL DPWRST('XXX','WRIT')
          ELSEIF(NUMDIG(I).EQ.-1)THEN
            WRITE(ICOUT,8035)
            CALL DPWRST('XXX','WRIT')
          ELSEIF(NUMDIG(I).EQ.-2)THEN
            IFORMT(1:7)='(G  .7)'
            NTOT=12+NLEFT
            WRITE(IFORMT(3:4),'(I2)')NTOT
            WRITE(ICOUT,IFORMT)AVALUE(ICNTN)
            CALL DPWRST('XXX','WRIT')
          ELSEIF(NUMDIG(I).EQ.-99)THEN
            WRITE(ICOUT,8036)'**'
            CALL DPWRST('XXX','WRIT')
          ELSEIF(NUMDIG(I).LT.-2 .AND. NUMDIG(I).GT.-20)THEN
            IFORMT(1:7)='(E  . )'
            WRITE(IFORMT(3:4),'(I2)')NTOT
            WRITE(IFORMT(6:6),'(I1)')NRIGHT
            WRITE(ICOUT,IFORMT)AVALUE(ICNTN)
            CALL DPWRST('XXX','WRIT')
          ELSE
            WRITE(ICOUT,'(A1)') ' '
          ENDIF
C
          WRITE(ICOUT,8027)IBASLC
          CALL DPWRST('XXX','WRIT')
        ENDIF
 8020 CONTINUE
C
 8039 FORMAT(A1,'row}')
      WRITE(ICOUT,8039)IBASLC
      CALL DPWRST('XXX','WRIT')
C
      IF(IRTFFF.EQ.'Times New Roman')THEN
        ITEMP=0
      ELSEIF(IRTFFF.EQ.'Lucida Sans')THEN
        ITEMP=6
      ELSEIF(IRTFFF.EQ.'Arial')THEN
        ITEMP=2
      ELSEIF(IRTFFF.EQ.'Bookman')THEN
        ITEMP=3
      ELSEIF(IRTFFF.EQ.'Georgia')THEN
        ITEMP=4
      ELSEIF(IRTFFF.EQ.'Tahoma')THEN
        ITEMP=5
      ELSEIF(IRTFFF.EQ.'Verdana')THEN
        ITEMP=7
      ENDIF 
      WRITE(ICOUT,8091)IBASLC,ITEMP
      CALL DPWRST(ICOUT,'WRIT')
C
      RETURN
      END
      SUBROUTINE DPRTFY(IVALUE,NCHAR,AVALUE,NHEAD,ITYPE,IFLAGA,IFLAGB)
C
C     PURPOSE--THIS ROUTINE IS A UTILUTY ROUTINE FOR CREATING
C              RTF OUTPUT.  THIS ROUTINE IS USED TO GENERATE
C              A DATA ROW FOR A TABLE.  IT IS SIMILAR TO DPRTF5,
C              BUT IT ALLOWS CHARACTER AND NUMERIC FIELDS TO BE
C              MIXED.
C
C              THIS IS A VARIATION OF DPRTFX.  IT DIFFERS IN THE
C              COUNTERS FOR THE NUMERIC AND ALPHANUMERIC FIELDS.
C
C     INPUT  ARGUMENTS--IVALUE  = THE ARRAY OF CHARACTER STRINGS CONTAINING
C                                 THE TEXT FOR THE CHARACTER FIELDS.
C                     --NCHAR   = THE INTEGER ARRAY THAT SPECIFIES
C                                 THE NUMBER OF CHARACTERS IN THE
C                                 TEXT FIELDS.
C                     --AVALUE  = A REAL ARRAY CONTAINING THE DATA
C                                 FOR THE NUMERIC FIELDS.
C                     --NHEAD   = THE INTEGER VALUE THAT SPECIFIES
C                                 THE NUMBER OF COMBINED NUMERIC AND
C                                 TEXT FIELDS.
C                     --IFLAGA  = A LOGICIAL VARIABLE THAT SPECIFIES
C                                 WHETHER A LINE IS DRAWN BEFORE THE ROW.
C                     --IFLAGB  = A LOGICIAL VARIABLE THAT SPECIFIES
C                                 WHETHER A LINE IS DRAWN AFTER THE ROW.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABOARATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2008/10
C     ORIGINAL VERSION--OCTOBER   2008.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*(*) IVALUE(*)
      CHARACTER*4   ITYPE(*)
      REAL AVALUE(*)
      INTEGER NCHAR(*)
C
      PARAMETER (MAXHED=1024)
      INTEGER IWIDTH(MAXHED)
      INTEGER NUMDIG(MAXHED)
      CHARACTER*8 ALIGN(MAXHED)
      CHARACTER*8 VALIGN(MAXHED)
      COMMON/HTML4/IWIDTH,NUMDIG,ALIGN,VALIGN
C
      LOGICAL IFLAGA
      LOGICAL IFLAGB
C
      CHARACTER*1  IBASLC
      CHARACTER*20 IFORMT
C
      INCLUDE 'DPCOST.INC'
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      CALL DPCONA(92,IBASLC)
C
C  STEP 1: GENERATE A HEADER LINE
C
 8001 FORMAT('{',A1,'trowd',A1,'trgraph90')
      WRITE(ICOUT,8001)IBASLC,IBASLC
      CALL DPWRST('XXX','WRIT')
C
 8011 FORMAT(A1,'clvertalt',A1,'cellx',I3)
 8012 FORMAT(A1,'clvertalc',A1,'cellx',I3)
 8013 FORMAT(A1,'clvertalb',A1,'cellx',I3)
 8111 FORMAT(A1,'clvertalt',A1,'cellx',I4)
 8112 FORMAT(A1,'clvertalc',A1,'cellx',I4)
 8113 FORMAT(A1,'clvertalb',A1,'cellx',I4)
 8211 FORMAT(A1,'clvertalt',A1,'cellx',I5)
 8212 FORMAT(A1,'clvertalc',A1,'cellx',I5)
 8213 FORMAT(A1,'clvertalb',A1,'cellx',I5)
 8014 FORMAT(A1,'clbrdrt',A1,'brdrw15',A1,'brdrs')
 8015 FORMAT(A1,'clbrdrb',A1,'brdrw15',A1,'brdrs')
 8016 FORMAT(A1,'clbrdrr',A1,'brdrw15',A1,'brdrs')
C
      NCOLS=NHEAD
C
      DO8010I=1,NCOLS
        IF(IFLAGB)THEN
          WRITE(ICOUT,8014)IBASLC,IBASLC,IBASLC
          CALL DPWRST('XXX','WRIT')
        ENDIF
        IF(IFLAGA)THEN
          WRITE(ICOUT,8015)IBASLC,IBASLC,IBASLC
          CALL DPWRST('XXX','WRIT')
        ENDIF
C
        IF(IVALUE(I)(4:5).EQ.'7C')THEN
          CALL DPCOAN(IVALUE(I)(2:2),IJUNK1)
          CALL DPCOAN(IVALUE(I)(3:3),IJUNK2)
          IF(IJUNK1.EQ.92 .AND. IJUNK2.EQ.39)THEN
            WRITE(ICOUT,8016)IBASLC,IBASLC,IBASLC
            CALL DPWRST('XXX','WRIT')
            IVALUE(I)=' '
            NCHAR(I)=0
          ENDIF
        ENDIF
C
        IF(VALIGN(I).EQ.'b')THEN
          IF(IWIDTH(I).LE.999)THEN
            WRITE(ICOUT,8013)IBASLC,IBASLC,IWIDTH(I)
          ELSEIF(IWIDTH(I).LE.9999)THEN
            WRITE(ICOUT,8113)IBASLC,IBASLC,IWIDTH(I)
          ELSE
            WRITE(ICOUT,8213)IBASLC,IBASLC,IWIDTH(I)
          ENDIF
        ELSEIF(VALIGN(I).EQ.'c')THEN
          IF(IWIDTH(I).LE.999)THEN
            WRITE(ICOUT,8012)IBASLC,IBASLC,IWIDTH(I)
          ELSEIF(IWIDTH(I).LE.9999)THEN
            WRITE(ICOUT,8112)IBASLC,IBASLC,IWIDTH(I)
          ELSE
            WRITE(ICOUT,8212)IBASLC,IBASLC,IWIDTH(I)
          ENDIF
        ELSE
          IF(IWIDTH(I).LE.999)THEN
            WRITE(ICOUT,8011)IBASLC,IBASLC,IWIDTH(I)
          ELSEIF(IWIDTH(I).LE.9999)THEN
            WRITE(ICOUT,8111)IBASLC,IBASLC,IWIDTH(I)
          ELSE
            WRITE(ICOUT,8211)IBASLC,IBASLC,IWIDTH(I)
          ENDIF
        ENDIF
        CALL DPWRST('XXX','WRIT')
 8010 CONTINUE
C
 8021 FORMAT(A1,'pard',A1,'intbl',A1,'ql {')
 8022 FORMAT(A1,'pard',A1,'intbl',A1,'qc {')
 8023 FORMAT(A1,'pard',A1,'intbl',A1,'qr {')
 8027 FORMAT('}',A1,'cell')
C
 8091 FORMAT(a1,'f',I1)
 8035 FORMAT(1X)
 8036 FORMAT(A2)
 8135 FORMAT(A1,'pard',A1,'intbl',A1,'ql { }',A1,'cell')
 8031 FORMAT(G15.7)
 8033 FORMAT(I12)
C
      ICNT=0
      DO8020I=1,NHEAD
C
        IF(ITYPE(I).EQ.'ALPH')THEN
C
C         PRINT CHARACTER FIELD
C
          IFORMT=' '
          IFORMT(1:5)='(A  )'
          ICNT=ICNT+1
          IF(NCHAR(ICNT).GT.0)THEN
            IF(ALIGN(I).EQ.'l')THEN
              WRITE(ICOUT,8021)IBASLC,IBASLC,IBASLC
            ELSEIF(ALIGN(I).EQ.'c')THEN
              WRITE(ICOUT,8022)IBASLC,IBASLC,IBASLC
            ELSE
              WRITE(ICOUT,8023)IBASLC,IBASLC,IBASLC
            ENDIF
            CALL DPWRST('XXX','WRIT')
            WRITE(IFORMT(3:4),'(I2)')NCHAR(ICNT)
            WRITE(ICOUT,IFORMT)IVALUE(ICNT)(1:NCHAR(ICNT))
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,8027)IBASLC
            CALL DPWRST('XXX','WRIT')
          ELSE
            WRITE(ICOUT,8135)IBASLC,IBASLC,IBASLC,IBASLC
            CALL DPWRST('XXX','WRIT')
          ENDIF
        ELSE
C
C         PRINT NUMERIC FIELD
C
          ICNT=ICNT+1
          IF(IRTFFF.EQ.'Courier New')THEN
            ITEMP=1
          ELSEIF(IRTFFF.EQ.'Lucida Console')THEN
            ITEMP=8
          ENDIF 
          WRITE(ICOUT,8091)IBASLC,ITEMP
          CALL DPWRST(ICOUT,'WRIT')
C
          IF(ALIGN(I).EQ.'l')THEN
            WRITE(ICOUT,8021)IBASLC,IBASLC,IBASLC
          ELSEIF(ALIGN(I).EQ.'c')THEN
            WRITE(ICOUT,8022)IBASLC,IBASLC,IBASLC
          ELSE
            WRITE(ICOUT,8023)IBASLC,IBASLC,IBASLC
          ENDIF
          CALL DPWRST('XXX','WRIT')
C
          IFORMT=' '
          NRIGHT=MIN(ABS(NUMDIG(I)),9)
          IF(ABS(AVALUE(ICNT)).LT.10.0)THEN
            NLEFT=1
          ELSEIF(ABS(AVALUE(ICNT)).LT.100.0)THEN
            NLEFT=2
          ELSEIF(ABS(AVALUE(ICNT)).LT.1000.0)THEN
            NLEFT=3
          ELSEIF(ABS(AVALUE(ICNT)).LT.10000.0)THEN
            NLEFT=4
          ELSEIF(ABS(AVALUE(ICNT)).LT.100000.0)THEN
            NLEFT=5
          ELSEIF(ABS(AVALUE(ICNT)).LT.1000000.0)THEN
            NLEFT=6
          ELSE
            NLEFT=7
          ENDIF
          IF(AVALUE(ICNT).LT.0.0)NLEFT=NLEFT+1
          NTOT=NRIGHT+NLEFT+2
          IF(NUMDIG(I).GT.0)THEN
            IFORMT(1:7)='(F  . )'
            WRITE(IFORMT(3:4),'(I2)')NTOT
            WRITE(IFORMT(6:6),'(I1)')NRIGHT
            WRITE(ICOUT,IFORMT)AVALUE(ICNT)
            CALL DPWRST('XXX','WRIT')
          ELSEIF(NUMDIG(I).EQ.0)THEN
            IFORMT(1:5)='(I  )'
            WRITE(IFORMT(3:4),'(I2)')NLEFT
            IF(AVALUE(ICNT).GE.0.0)THEN
              WRITE(ICOUT,IFORMT)INT(AVALUE(ICNT)+0.5)
            ELSE
              WRITE(ICOUT,IFORMT)INT(AVALUE(ICNT)-0.5)
            ENDIF
            CALL DPWRST('XXX','WRIT')
          ELSEIF(NUMDIG(I).EQ.-1)THEN
            WRITE(ICOUT,8035)
            CALL DPWRST('XXX','WRIT')
          ELSEIF(NUMDIG(I).EQ.-2)THEN
            IFORMT(1:7)='(G  .7)'
            NTOT=12+NLEFT
            WRITE(IFORMT(3:4),'(I2)')NTOT
            WRITE(ICOUT,IFORMT)AVALUE(ICNT)
            CALL DPWRST('XXX','WRIT')
          ELSEIF(NUMDIG(I).EQ.-99)THEN
            WRITE(ICOUT,8036)'**'
            CALL DPWRST('XXX','WRIT')
          ELSEIF(NUMDIG(I).LT.-2 .AND. NUMDIG(I).GT.-20)THEN
            IFORMT(1:7)='(E  . )'
            WRITE(IFORMT(3:4),'(I2)')NTOT
            WRITE(IFORMT(6:6),'(I1)')NRIGHT
            WRITE(ICOUT,IFORMT)AVALUE(ICNT)
            CALL DPWRST('XXX','WRIT')
          ELSE
            WRITE(ICOUT,'(A1)') ' '
          ENDIF
C
          WRITE(ICOUT,8027)IBASLC
          CALL DPWRST('XXX','WRIT')
        ENDIF
 8020 CONTINUE
C
 8039 FORMAT(A1,'row}')
      WRITE(ICOUT,8039)IBASLC
      CALL DPWRST('XXX','WRIT')
C
      IF(IRTFFF.EQ.'Times New Roman')THEN
        ITEMP=0
      ELSEIF(IRTFFF.EQ.'Lucida Sans')THEN
        ITEMP=6
      ELSEIF(IRTFFF.EQ.'Arial')THEN
        ITEMP=2
      ELSEIF(IRTFFF.EQ.'Bookman')THEN
        ITEMP=3
      ELSEIF(IRTFFF.EQ.'Georgia')THEN
        ITEMP=4
      ELSEIF(IRTFFF.EQ.'Tahoma')THEN
        ITEMP=5
      ELSEIF(IRTFFF.EQ.'Verdana')THEN
        ITEMP=7
      ENDIF 
      WRITE(ICOUT,8091)IBASLC,ITEMP
      CALL DPWRST(ICOUT,'WRIT')
C
      RETURN
      END
      SUBROUTINE DPRTFZ(IHEAD,NHEAD,AVAL,NUMDIG)
C
C     PURPOSE--THIS ROUTINE IS A UTILUTY ROUTINE FOR CREATING
C              RTF OUTPUT.  THIS ROUTINE IS USED TO WRITE A
C              A SINGLE LINE OF OUTPUT.
C     INPUT  ARGUMENTS--IHEAD  = THE CHARACTER STRING CONTAINING
C                                THE TEXT FOR THE LINE
C                     --NHEAD  = THE INTEGER NUMBER THAT SPECIFIES
C                                THE NUMBER OF CHARACTERS IN THE
C                                LINE.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABOARATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2005/2
C     ORIGINAL VERSION--FEBRUARY  2005.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*(*) IHEAD
C
      CHARACTER*1  IBASLC
      CHARACTER*1  IQUOTE
      CHARACTER*25 IFORMT
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
C  STEP 1: END ASIS MODE AND WRITE A HEADER
C
      CALL DPCONA(92,IBASLC)
C
C  STEP 2: START TABLE AND DEFINE A CAPTION
C
 8005 FORMAT('{',A1,'ql ')
 8007 FORMAT(A1,'line')
C
      IF(NHEAD.GE.1)THEN
        IFORMT=' '
        IF(AVAL.NE.CPUMIN)THEN
          IF(NUMDIG.GT.0)THEN
            AVALT=RND(AVAL,NUMDIG)
            IXX=NUMDIG
            IYY=IXX+8
            IFORMT(1:21)='(A  ,2X,Gyy.xx,2X,A1)'
            WRITE(IFORMT(3:4),'(I2)')NHEAD
            WRITE(IFORMT(10:11),'(I2)')IYY
            WRITE(IFORMT(13:14),'(I2)')IXX
            WRITE(ICOUT,8005)IBASLC
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,IFORMT)IHEAD(1:NHEAD),AVALT,'}'
            CALL DPWRST('XXX','WRIT')
          ELSEIF(NUMDIG.LT.0)THEN
            NUMDI2=-NUMDIG
            AVALT=RND(AVAL,NUMDI2)
            IXX=-NUMDIG
            IYY=IXX+8
            IFORMT(1:21)='(A  ,2X,Eyy.xx,2X,A1)'
            WRITE(IFORMT(3:4),'(I2)')NHEAD
            WRITE(IFORMT(10:11),'(I2)')IYY
            WRITE(IFORMT(13:14),'(I2)')IXX
            WRITE(ICOUT,8005)IBASLC
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,IFORMT)IHEAD(1:NHEAD),AVALT,'}'
            CALL DPWRST('XXX','WRIT')
          ELSEIF(NUMDIG.EQ.0)THEN
            IF(AVAL.GE.0.0)THEN
              IVALT=INT(AVAL + 0.5)
            ELSE
              IVALT=INT(AVAL - 0.5)
            ENDIF
            IFORMT(1:18)='(A  ,2X,I10,2X,A1)'
            WRITE(IFORMT(3:4),'(I2)')NHEAD
            WRITE(ICOUT,IFORMT)IHEAD(1:NHEAD),IVALT,'}'
            CALL DPWRST('XXX','WRIT')
          ENDIF
        ELSE
          IFORMT(1:11)='(A  ,2X,A1)'
          WRITE(IFORMT(3:4),'(I2)')NHEAD
          WRITE(ICOUT,8005)IBASLC
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,IFORMT)IHEAD(1:NHEAD),'}'
          CALL DPWRST('XXX','WRIT')
        ENDIF
        WRITE(ICOUT,8007)IBASLC
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPRTIL(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN TRIPLEX ITALIC LOWER CASE.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
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
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
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
   51 FORMAT('***** AT THE BEGINNING OF DPRTIL--')
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
      IF(ICHARN.LE.7)GOTO1010
      GOTO1019
 1010 CONTINUE
      CALL DRTIL1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1019 CONTINUE
C
      IF(8.LE.ICHARN.AND.ICHARN.LE.15)GOTO1020
      GOTO1029
 1020 CONTINUE
      CALL DRTIL2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1029 CONTINUE
C
      IF(16.LE.ICHARN.AND.ICHARN.LE.23)GOTO1030
      GOTO1039
 1030 CONTINUE
      CALL DRTIL3(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1039 CONTINUE
C
      IF(ICHARN.GE.24)GOTO1040
      GOTO1049
 1040 CONTINUE
      CALL DRTIL4(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
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
 9011 FORMAT('***** AT THE END       OF DPRTIL--')
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
      SUBROUTINE DPRTIN(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN TRIPLEX ITALIC NUMERIC.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
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
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
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
   51 FORMAT('***** AT THE BEGINNING OF DPRTIN--')
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
      IF(ICHARN.LE.7)GOTO1010
      GOTO1019
 1010 CONTINUE
      CALL DRTIN1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1019 CONTINUE
C
      IF(ICHARN.GE.8)GOTO1020
      GOTO1029
 1020 CONTINUE
      CALL DRTIN2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
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
 9011 FORMAT('***** AT THE END       OF DPRTIN--')
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
      SUBROUTINE DPRTIU(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN TRIPLEX ITALIC UPPER CASE.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
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
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
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
   51 FORMAT('***** AT THE BEGINNING OF DPRTIU--')
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
      CALL DRTIU1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1019 CONTINUE
C
      IF(7.LE.ICHARN.AND.ICHARN.LE.13)GOTO1020
      GOTO1029
 1020 CONTINUE
      CALL DRTIU2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1029 CONTINUE
C
      IF(14.LE.ICHARN.AND.ICHARN.LE.19)GOTO1030
      GOTO1039
 1030 CONTINUE
      CALL DRTIU3(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1039 CONTINUE
C
      IF(ICHARN.GE.20)GOTO1040
      GOTO1049
 1040 CONTINUE
      CALL DRTIU4(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1049 CONTINUE
C
      IFOUND='NO'
      GOTO9000
C
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
 9011 FORMAT('***** AT THE END       OF DPRTIU--')
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

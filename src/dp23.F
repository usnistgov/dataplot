      SUBROUTINE DPPROF(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                  IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--GENERATE A PROFILE PLOT--
C              A MULTIVARIATE TECHNICQUE WHICH PLOTS A STANDARDIZED
C              (0 TO 1) VARIABLE VERSUS DUMMY VARIABLE NUMBER.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--88/2
C     ORIGINAL VERSION--FEBRUARY  1988.
C     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE COMMON
C     UPDATED         --MARCH     2011. USE DPPARS
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
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      CHARACTER*40 INAME
      PARAMETER (MAXSPN=50)
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
      DIMENSION Z1(MAXOBV)
      DIMENSION Z2(MAXOBV)
      DIMENSION Z3(MAXOBV)
      DIMENSION YSUB(MAXOBV)
      DIMENSION YFULL(MAXOBV)
      DIMENSION XTEMP(MAXOBV)
CCCCC FOLLOWING LINES ADDED JUNE, 1990
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(IGARB1),Z1(1))
      EQUIVALENCE (GARBAG(IGARB2),Z2(1))
      EQUIVALENCE (GARBAG(IGARB3),Z3(1))
      EQUIVALENCE (GARBAG(IGARB4),YSUB(1))
      EQUIVALENCE (GARBAG(IGARB5),YFULL(1))
      EQUIVALENCE (GARBAG(IGARB6),XTEMP(1))
CCCCC END CHANGE
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IERROR='NO'
      ISUBN1='DPPR'
      ISUBN2='OF  '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
C               ***********************************
C               **  TREAT THE PROFILE PLOT CASE  **
C               ***********************************
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PROF')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPPROF--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGG2,IBUGG3,IBUGQ,ISUBRO
   52   FORMAT('IBUGG2,IBUGG3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,IAND1,IAND2
   53   FORMAT('ICASPL,IAND1,IAND2 = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***************************
C               **  STEP 1--             **
C               **  EXTRACT THE COMMAND  **
C               ***************************
C
      ISTEPN='11'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PROF')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASPL='PROF'
C
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PLOT')THEN
        IFOUND='YES'
        ILASTC=1
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      ELSE
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PROF')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='PROFILE PLOT'
      MINNA=1
      MAXNA=100
      MINN2=1
      IFLAGE=1
      IFLAGM=0
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
     1            IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PROF')THEN
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
      IWRITE='OFF'
      DO2200K=1,NUMVAR
        JF=0
        JS=0
        IMAX=NRIGHT(K)
        IF(NQ.LT.NRIGHT(1))IMAX=NQ
        DO2210I=1,IMAX
C
C         CREATE THE "FULL" VARIABLE
C
          JF=JF+1
          IJ=MAXN*(ICOLR(K)-1)+I
          IF(ICOLR(K).LE.MAXCOL)YFULL(JF)=V(IJ)
          IF(ICOLR(K).EQ.MAXCP1)YFULL(JF)=PRED(I)
          IF(ICOLR(K).EQ.MAXCP2)YFULL(JF)=RES(I)
          IF(ICOLR(K).EQ.MAXCP3)YFULL(JF)=YPLOT(I)
          IF(ICOLR(K).EQ.MAXCP4)YFULL(JF)=XPLOT(I)
          IF(ICOLR(K).EQ.MAXCP5)YFULL(JF)=X2PLOT(I)
          IF(ICOLR(K).EQ.MAXCP6)YFULL(JF)=TAGPLO(I)
 2210   CONTINUE
        NFULL=JF
        CALL MINIM(YFULL,NFULL,IWRITE,XMIN,IBUGG3,IERROR)
        CALL MAXIM(YFULL,NFULL,IWRITE,XMAX,IBUGG3,IERROR)
        Z2(K)=XMIN
        Z3(K)=XMAX
C
C       CREATE THE "SUBSET" VARIABLE
C
        DO2240I=1,IMAX
          IF(ISUB(I).EQ.0)GOTO2240
          JS=JS+1
          IJ=MAXN*(ICOLR(K)-1)+I
C
          IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PROF')THEN
             WRITE(ICOUT,2241)I,JS,MAXN,ICOLR(I),IJ,NRIGHT(I),NQ,IMAX
 2241        FORMAT('I,JS,MAXN,ICOLR(I),IJ,NRIGHT(I),NQ,IMAX = ',8I8)
             CALL DPWRST('XXX','BUG ')
          ENDIF
C
          IF(ICOLR(K).LE.MAXCOL)YSUB(JS)=V(IJ)
          IF(ICOLR(K).EQ.MAXCP1)YSUB(JS)=PRED(I)
          IF(ICOLR(K).EQ.MAXCP2)YSUB(JS)=RES(I)
          IF(ICOLR(K).EQ.MAXCP3)YSUB(JS)=YPLOT(I)
          IF(ICOLR(K).EQ.MAXCP4)YSUB(JS)=XPLOT(I)
          IF(ICOLR(K).EQ.MAXCP5)YSUB(JS)=X2PLOT(I)
          IF(ICOLR(K).EQ.MAXCP6)YSUB(JS)=TAGPLO(I)
C
 2240   CONTINUE
        NSUB=JS
C
        CALL MEDIAN(YSUB,NSUB,IWRITE,XTEMP,MAXN,XMED,IBUGG3,IERROR)
        Z1(K)=XMED
C
 2200 CONTINUE
      NZ=NUMVAR
C
C               ********************************************************
C               **  STEP 31--                                         **
C               **  FORM THE VERTICAL AND HORIZONTAL AXIS             **
C               **  VALUES Y(.) AND X(.) FOR THE PLOT.                **
C               **  DEFINE THE VECTOR D(.) TO 1'S, 2'S, AND 3'S       **
C               **  FOR THE PLOTTED VALUE, THE LOWER CONFIDENCE LINE, **
C               **  AND THE UPPER CONFIDENCE LINE.                    **
C               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).     **
C               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).     **
C               ********************************************************
C
      ISTEPN='8'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PROF')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPPRO2(Z1,Z2,Z3,NZ,ICASPL,
     1            Y,X,D,NPLOTP,NPLOTV,
     1            IBUGG3,ISUBRO,IERROR)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PROF')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPPROF--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IFOUND,IERROR
 9013   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9014   FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',
     1         I8,I8,I8,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)NSUB,NFULL,NZ,NPLOTP
 9021   FORMAT('NSUB,NFULL,NZ,NPLOTP = ',4I8)
        CALL DPWRST('XXX','BUG ')
        IF(NSUB.GT.0)THEN
          DO9022I=1,NSUB
            WRITE(ICOUT,9023)I,YSUB(I)
 9023       FORMAT('I,YSUB(I) = ',I8,E15.7)
            CALL DPWRST('XXX','BUG ')
 9022     CONTINUE
        ENDIF
        IF(NFULL.GT.0)THEN
          DO9032I=1,NFULL
            WRITE(ICOUT,9033)I,YFULL(I)
 9033       FORMAT('I,YFULL(I) = ',I8,E15.7)
            CALL DPWRST('XXX','BUG ')
 9032     CONTINUE
        ENDIF
        IF(NZ.GT.0)THEN
          DO9042I=1,NZ
            WRITE(ICOUT,9043)I,Z1(I),Z2(I),Z3(I)
 9043       FORMAT('I,Z1(I),Z2(I),Z3(I) = ',I8,3G15.7)
            CALL DPWRST('XXX','BUG ')
 9042     CONTINUE
        ENDIF
        IF(NPLOTP.GT.0)THEN
          DO9052I=1,NPLOTP
            WRITE(ICOUT,9053)I,Y(I),X(I),D(I)
 9053       FORMAT('I,Y(I),X(I),D(I) = ',I8,3F12.5)
            CALL DPWRST('XXX','BUG ')
 9052     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPPROJ(ICOM,IHARG,NUMARG,I3DPRO,
     1IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE 3-D PROJECTION SWITCH I3DPRO.
C              THE 2 SETTINGS ARE
C                 1) ORTHOGRAPHIC (THE DEFAULT)
C                 2) PERSPECTIVE
C     INPUT  ARGUMENTS--ICOM
C                     --IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG
C     OUTPUT ARGUMENTS--I3DPRO   ('ORTH'  OR 'PERS')
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DI3DPROION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--88/10
C     ORIGINAL VERSION--SEPTEMBER 1988.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
      CHARACTER*4 I3DPRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
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
      IF(ICOM.EQ.'ORTH')GOTO1110
      IF(ICOM.EQ.'PERS')GOTO1120
      IF(ICOM.EQ.'PROJ')GOTO1130
C
 1110 CONTINUE
      IF(NUMARG.LE.0)GOTO1150
      IF(IHARG(1).EQ.'ON')GOTO1150
      IF(IHARG(1).EQ.'OFF')GOTO1160
      GOTO1199
C
 1120 CONTINUE
      IF(NUMARG.LE.0)GOTO1160
      IF(IHARG(1).EQ.'ON')GOTO1160
      IF(IHARG(1).EQ.'OFF')GOTO1150
      GOTO1199
C
 1130 CONTINUE
      IF(NUMARG.LE.0)GOTO1150
      IF(IHARG(1).EQ.'ON')GOTO1150
      IF(IHARG(1).EQ.'OFF')GOTO1160
      IF(IHARG(1).EQ.'AUTO')GOTO1150
      IF(IHARG(1).EQ.'DEFA')GOTO1150
      IF(IHARG(1).EQ.'ORTH')GOTO1150
      IF(IHARG(1).EQ.'PERS')GOTO1160
      GOTO1199
C
 1150 CONTINUE
      I3DPRO='ORTH'
      GOTO1180
C
 1160 CONTINUE
      I3DPRO='PERS'
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE PROJECTION SWITCH (AFFECTING 3-D PLOTS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)I3DPRO
 1182 FORMAT('           HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPPROM(IHARG,NUMARG,IPROSW,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE PROMPT SWITCH IPROSW.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG
C     OUTPUT ARGUMENTS--IPROSW  ('ON'  OR 'OFF')
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
C     VERSION NUMBER--86/1
C     ORIGINAL VERSION--DECEMBER  1985.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IPROSW
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
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
      IF(NUMARG.EQ.0)GOTO1150
      IF(NUMARG.GE.1)GOTO1110
      GOTO1199
C
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1160
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      GOTO1199
C
 1150 CONTINUE
      IPROSW='ON'
      GOTO1180
C
 1160 CONTINUE
      IPROSW='OFF'
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)IPROSW
 1181 FORMAT('THE PROMPT SWITCH HAS JUST BEEN TURNED ',
     1A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPPRO2(Z1,Z2,Z3,NZ,ICASPL,
     1Y2,X2,D2,N2,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
C              THAT WILL DEFINE
C              A PROFILE PLOT
C              (USEFUL FOR MULTIVARIATE ANALYSIS).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--88/2
C     ORIGINAL VERSION--JANUARY   1988.
C     UPDATED         --APRIL     1992.  DELETE K
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DIMENSION Z1(*)
      DIMENSION Z2(*)
      DIMENSION Z3(*)
C
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPPR'
      ISUBN2='O2  '
      IERROR='NO'
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      IF(NZ.LT.1)THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
   31   FORMAT('***** ERROR IN PROFILE PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,32)
   32   FORMAT('      THE NUMBER OF OBSERVATIONS IS NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,34)NZ
   34   FORMAT('      THE ENTERED NUMBER OF OBSERVATIONS HERE = ',I6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'PRO2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)
   71   FORMAT('***** AT THE BEGINNING OF DPPRO2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)ICASPL,NZ,N2,NPLOTV
   72   FORMAT('ICASPL,NZ,N2,NPLOTV = ',A4,2X,3I8)
        CALL DPWRST('XXX','BUG ')
        IF(NZ.GT.0)THEN
          DO81I=1,NZ
            WRITE(ICOUT,82)I,Z1(I),Z2(I),Z3(I)
   82       FORMAT('I,Z1(I),Z2(I),Z3(I) = ',I8,3F15.7)
            CALL DPWRST('XXX','BUG ')
   81     CONTINUE
        ENDIF
      ENDIF
C
C               ****************************************
C               **  STEP 11--                         **
C               **  DETERMINE PLOT COORDINATES        **
C               ****************************************
C
      J=0
      DO1100I=1,NZ
        ANUM=Z1(I)-Z2(I)
        ADEN=Z3(I)-Z2(I)
        P=0.0
        IF(ADEN.GT.0.0)P=ANUM/ADEN
        J=J+1
        Y2(J)=P
        X2(J)=J
        D2(J)=1.0
 1100 CONTINUE
      N2=J
      NPLOTV=2
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'PRO2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPPRO2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9031)N2,NPLOTV
 9031   FORMAT('N2,NPLOTV = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO9035I=1,N2
          WRITE(ICOUT,9036)I,Y2(I),X2(I),D2(I)
 9036     FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,2G15.7,F9.2)
          CALL DPWRST('XXX','BUG ')
 9035   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPPRPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                  IANGLU,MAXNPP,
     1                  ICONT,NUMHPP,NUMVPP,IMANUF,
     1                  XMATN,YMATN,XMITN,YMITN,
     1                  ISQUAR,
     1                  IVGMSW,IHGMSW,
     1                  IMPSW,IMPNR,IMPNC,IMPCO,
     1                  PMXMIN,PMXMAX,PMYMIN,PMYMAX,
     1                  MAXNXT,ALOWFR,ALOWDG,IFORSW,
     1                  ANOPL1,ANOPL2,ISEED,IBOOSS,BARHEF,BARWEF,
     1                  ICAPSW,
     1                  IBUGG2,IBUGG3,IBUGCO,IBUGEV,IBUGQ,
     1                  IBUGUG,IBUGU2,IBUGU3,IBUGU4,ISUBRO,
     1                  IFOUND,IERROR)
C
C     PURPOSE--GENERATE EITHER
C              1) A PARTIAL REGRESSION PLOT
C              2) A PARTIAL LEVERAGE PLOT
C              3) A PARTIAL RESIDUAL PLOT
C              4) A CCPR PLOT
C              FOR EXAMPLE, THE COMMAND
C                 PARTIAL REGRESSION PLOT Y X1 TO XK
C              WILL GENERATE PARTIAL REGRESSION PLOTS OF Y VS X1,
C              Y VS X2, ETC. AS A MULTIPLOT ON A SINGLE PAGE.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2002/6
C     ORIGINAL VERSION--JUNE      2002.
C     UPDATED         --FEBRUARY  2005. CALL LIST TO MAINAN
C     UPDATED         --MARCH     2006. CALL LIST TO MAINGR
C     UPDATED         --AUGUST    2007. CALL LIST TO MAINGR
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES---------------
C
      INCLUDE 'DPCOPA.INC'
C
      CHARACTER*4 ICASPL
      CHARACTER*4 ICASP2
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICASAN
      CHARACTER*4 ICONT
      CHARACTER*4 IPOWE
      CHARACTER*4 IAND1
      CHARACTER*4 IAND2
      CHARACTER*4 IANGLU
      CHARACTER*4 IFORSW
      CHARACTER*4 IFTEXP
      CHARACTER*4 IFTORD
      CHARACTER*4 ICPSWZ
C
      CHARACTER*4 IBUGG2
      CHARACTER*4 IBUGG3
      CHARACTER*4 IBUGUG
      CHARACTER*4 IBUGU2
      CHARACTER*4 IBUGU3
      CHARACTER*4 IBUGU4
      CHARACTER*4 IBUGCO
      CHARACTER*4 IBUGEV
      CHARACTER*4 IBUGQ
C
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
      CHARACTER*4 IEMPTY
      CHARACTER*4 ISQUAR
      CHARACTER*4 IVGMSW
      CHARACTER*4 IHGMSW
      CHARACTER*4 IREPCH
      CHARACTER*4 IMPSW
      CHARACTER*4 IFPLFZ
      CHARACTER*4 IFPLTZ
      CHARACTER*4 IFPLPZ
      CHARACTER*4 IFPLLZ
      CHARACTER*4 IFPLL2
      CHARACTER*4 IFPLXZ
      CHARACTER*4 IFPLYZ
      CHARACTER*4 IFPLDZ
      CHARACTER*4 IFPLZT
      CHARACTER*4 IFPLZ2
      CHARACTER*4 IFPLZ3
      CHARACTER*4 IFPLZ4
      CHARACTER*4 ILFLAX
      CHARACTER*4 ILFLAY
      CHARACTER*4 IFPLLD
      CHARACTER*4 IFPLDI
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 IFEED9
      CHARACTER*4 IMANUF
      CHARACTER*4 IPLOTT
      CHARACTER*4 ICT
      CHARACTER*4 IC2T
      CHARACTER*4 IHT(5)
      CHARACTER*4 IH2T(5)
C
C  MAXY IS THE MAXIMUM NUMBER OF VARIABLES TO USE IN CREATING THE
C  PARTIAL REGRESSION PLOT   CURVE
C
      PARAMETER(MAXY=50)
      CHARACTER*40 INAME
      CHARACTER*4 IVARN1(MAXY)
      CHARACTER*4 IVARN2(MAXY)
      CHARACTER*4 IVARTY(MAXY)
      DIMENSION ILIS(MAXY)
      DIMENSION PVAR(MAXY)
      DIMENSION NRIGHT(MAXY)
      DIMENSION ICOLL(MAXY)
C
      DIMENSION TEMP(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TEMP3(MAXOBV)
      DIMENSION XTEMP1(MAXOBV)
      DIMENSION XTEMP2(MAXOBV)
C
C-----COMMON------------------------------------------------------
C
      INCLUDE 'DPCOZ3.INC'
      INCLUDE 'DPCOPC.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOSP.INC'
C
      EQUIVALENCE (G3RBAG(KGARB1),TEMP(1))
      EQUIVALENCE (G3RBAG(KGARB2),TEMP2(1))
      EQUIVALENCE (G3RBAG(KGARB3),TEMP3(1))
      EQUIVALENCE (G3RBAG(KGARB4),XTEMP1(1))
      EQUIVALENCE (G3RBAG(KGARB5),XTEMP2(1))
C
C-----COMMON VARIABLES (GENERAL)----------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-------------------------------------------------
C
      IFOUND='YES'
      IERROR='NO'
      ISUBN1='DPPR'
      ISUBN2='PL  '
C
      IF(ICASPL.NE.'CCPR')ICASPL='PRPL'
      IFPLLD='ON'
      IFPLDI='LINE'
      IBOOSS=100
C
      IFLAGV=5
C
C               ***********************************************
C               **  TREAT THE PARTIAL REGRESSION PLOT   CASE **
C               ***********************************************
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.NE.'PRPL')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPPRPL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASPL,IAND1,IAND2,NUMARG
   52   FORMAT('ICASPL,IAND1,IAND2,NUMARG = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGG2,IBUGG3,IBUGQ,ISUBRO
   53   FORMAT('IBUGG2,IBUGG3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        IF(NUMARG.GT.0)THEN
          DO61I=1,NUMARG
            WRITE(ICOUT,62)I,IHARG(I),IARGT(I)
   62       FORMAT('I,IHARG(I),IARGT(I) = ',I8,2X,A4,2X,A4)
            CALL DPWRST('XXX','BUG ')
   61     CONTINUE
        ENDIF
        WRITE(ICOUT,71)IFPLLA,IFPLTA,IFPLPT,IFPLFI,IFPLFR
   71   FORMAT('IFPLLA,IFPLTA,IFPLPT,IFPLFI,IFPLFR = ',5(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ******************************************************
C               **  STEP 1--                                        **
C               **  SHIFT COMMAND LINE ARGMENTS                     **
C               ******************************************************
C
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PRPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'REGR'.AND.IHARG(2).EQ.'PLOT')THEN
        ICASPL='PREG'
        ISHIFT=2
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGG2,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C  SYNONYM: ADDED VARIABLE PLOT
C
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'VARI'.AND.IHARG(2).EQ.'PLOT')THEN
        ICASPL='PREG'
        ISHIFT=2
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGG2,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
      ENDIF
C
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LEVE'.AND.IHARG(2).EQ.'PLOT')THEN
        ICASPL='PLEV'
        ISHIFT=2
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGG2,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
      ENDIF
C
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'RESI'.AND.IHARG(2).EQ.'PLOT')THEN
        ICASPL='PRES'
        ISHIFT=2
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGG2,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
      ENDIF
C
      IF(ICASPL.EQ.'CCPR'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'PLOT')THEN
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGG2,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C  SYNONYM: COMPONENT PLUS RESIDUAL PLOT
C
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'PLUS'.AND.IHARG(2).EQ.'RESI'.AND.
     1   IHARG(3).EQ.'PLOT')THEN
        ICASPL='PRES'
        ISHIFT=3
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGG2,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
      ENDIF
C
      ICOM='FIT '
      ICOM2='    '
      IFOUND='YES'
C
C               *******************************************************
C               **  STEP 2--                                         **
C               **  CHECK FOR THE PROPER NUMBER OF INPUT ARGUMENTS.  **
C               *******************************************************
C
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PRPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='PARTIAL REGRESSION PLOT'
      MINNA=1
      MAXNA=100
      MINN2=2
      IFLAGE=1
      IFLAGM=0
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=2
      MAXNVA=MAXY
C
      CALL DPPARS(IHARG,IHARG2,IARGT,ARG,NUMARG,IANS,IWIDTH,
     1            IHNAME,IHNAM2,IUSE,NUMNAM,IN,IVALUE,VALUE,
     1            JMIN,JMAX,
     1            MINN2,MINNA,MAXNA,MAXY,IFLAGE,INAME,
     1            IVARN1,IVARN2,IVARTY,PVAR,
     1            ILIS,NRIGHT,ICOLL,ISUB,NQ,ILOCQ,NUMVAR,
     1            MINNVA,MAXNVA,
     1            IFLAGM,IFLAGP,
     1            IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PRPL')THEN
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
     1                      ICOLL(I)
  287       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',
     1             'ICOLL(I) = ',I8,2X,A4,A4,2X,3I8)
            CALL DPWRST('XXX','BUG ')
  285     CONTINUE
        ENDIF
      ENDIF
C
C               **************************************************
C               **   STEP 0.5--                                 **
C               **   PERFORM MULTILINEAR FIT                    **
C               **************************************************
C
      ISTEPN='0.5'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'PRPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICPSWZ='OFF'
      CALL MAINAN(ICASAN,ISEED,ANOPL1,ANOPL2,
     1TEMP,TEMP2,XTEMP1,XTEMP2,MAXNXT,
     1IFTEXP,IFTORD,
     1ALOWFR,ALOWDG,
     1IBOOSS,
     1ICPSWZ,
     1IFORSW,
     1IBUGG2,IBUGG2,IBUGG3,
     1IBUGCO,IBUGEV,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C               **************************************************
C               **   STEP 1--                                   **
C               **   SAVE INITIAL SETTINGS                      **
C               **************************************************
C
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'PRPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IFLAG=1
      CALL DPSPM5(IFLAG,IMPSW,IMPCO,IMPNR,IMPNC,
     1            IBUGG2,ISUBRO,IFOUND,IERROR)
C
      ILFLAX='OFF'
      ILFLAY='OFF'
      IF(IY1MIN.EQ.'FIXE'.AND.IY1MAX.EQ.'FIXE')THEN
        ILFLAY='ON'
      ENDIF
      IF(IX1MIN.EQ.'FIXE'.AND.IX2MAX.EQ.'FIXE')THEN
        ILFLAX='ON'
      ENDIF
C
      IFPLL2=IFPLLA
      IFPLTZ=IFPLTA
      IFPLFZ=IFPLFR
      IFPLPZ=IFPLPT
      IFPLLZ=IFPLLD
      IFPLZT=IFPLST
      IFPLZ2=IFPLS2
      IFPLZ3=IFPLS3
      IFPLZ4=IFPLS4
      IFPLXZ=IFPLXA
      IFPLYZ=IFPLYA
      IFPLDZ=IFPLDI
      IF(IFPLFR.EQ.'USER'.AND.IFPLLA.EQ.'BOX')IFPLLA='ON'
      IF(IFPLFR.EQ.'CONN')IFPLFR='DEFA'
      IF(IFPLLA.EQ.'BOX ')THEN
        IFPLLD='ON'
        IF(IFPLDI.EQ.'BLAN')IFPLDI='LINE'
      ENDIF
C
      IFEED9=IFEEDB
C
      IMPSW3=IMPSW
      IMPCO2=IMPCO
      IMPNR2=IMPNR
      IMPNC2=IMPNC
      IMPSW='ON'
      IMPCO=1
      IMPCO9=IMPCO
C
      NPLOTS=NUMVAR-1
C
      IF(IMPNR*IMPNC.LT.NPLOTS)THEN
        IMPNC=INT(SQRT(REAL(NPLOTS-1)))+1
        IMPNR=1
        IF(NPLOTS.GE.11)THEN
          IMPNR=INT(NPLOTS/IMPNC)+1
        ELSEIF(NPLOTS.GE.7)THEN
          IMPNR=3
        ELSEIF(NPLOTS.GE.3)THEN
          IMPNR=2
        ENDIF
      ENDIF
C
      IROWT=IMPNR
      ICOLT=IMPNC
      IF(IFPLLA.EQ.'BOX')THEN
        IMPNR=IMPNR+1
        IMPNC=IMPNC+1
        IROWT=IROWT+1
        ICOLT=ICOLT+1
      ENDIF
C
C               *************************************
C               **   STEP 21--                     **
C               **   GENERATE THE PLOTS            **
C               *************************************
C
      ISTEPN='21'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DPPRPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASPL.EQ.'PREG')THEN
        ICT='PART'
        IC2T='IAL '
        NCCOMM=2
        IHT(1)='REGR'
        IH2T(1)='ESSI'
        IHT(2)='PLOT'
        IH2T(2)='    '
        IPLOTT='PREG'
      ELSEIF(ICASPL.EQ.'PLEV')THEN
        ICT='PART'
        IC2T='IAL '
        NCCOMM=2
        IHT(1)='LEVE'
        IH2T(1)='RAGE'
        IHT(2)='PLOT'
        IH2T(2)='    '
        IPLOTT='PLEV'
      ELSEIF(ICASPL.EQ.'PRES')THEN
        ICT='PART'
        IC2T='IAL '
        NCCOMM=2
        IHT(1)='RESI'
        IH2T(1)='DUAL'
        IHT(2)='PLOT'
        IH2T(2)='    '
        IPLOTT='PRES'
      ELSEIF(ICASPL.EQ.'CCPR')THEN
        ICT='CCPR'
        IC2T='    '
        NCCOMM=1
        IHT(1)='PLOT'
        IH2T(1)='    '
        IPLOTT='CCPR'
      ELSE
        ICT='PART'
        IC2T='IAL '
        NCCOMM=2
        IHT(1)='REGR'
        IH2T(1)='ESSI'
        IPLOTT='PREG'
      ENDIF
      GOTO5299
C
C               **************************************************
C               **   GENERATE ONE OF THE FOLLOWING COMMANDS     **
C               **      PARTIAL REGRESSION PLOT Y X1 X2 .... XI **
C               **      PARTIAL RESIDUAL   PLOT Y X1 X2 .... XI **
C               **      PARTIAL LEVERAGE   PLOT Y X1 X2 .... XI **
C               **   WHERE XI IS THE SPECIFIC VARIABLE THE      **
C               **   PLOT IS BEING GENERATED FOR.               **
C               **************************************************
 5299 CONTINUE
C
      IF(NPLOTS.LT.1)GOTO8000
C
      ISHIFT=NCCOMM
      CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1            IBUGG2,IERROR)
      ICOM=ICT
      ICOM2=IC2T
      IF(NCCOMM.GT.0)THEN
        DO5301II=1,NCCOMM
          IHARG(II)=IHT(II)
          IHARG2(II)=IH2T(II)
          IARG(II)=0
          ARG(II)=0.0
          IARGT(II)='WORD'
 5301   CONTINUE
      ENDIF
      IFRST=NCCOMM+2
      NUMARG=NUMARG+1
      IHARG(NUMARG)='    '
      IHARG2(NUMARG)='    '
      IARG(NUMARG)=0
      ARG(NUMARG)=0.0
      IARGT(NUMARG)=IARGT(IFRST)
      NARGT=NUMARG
C
      IPLOT=0
      IF(IFPLLA.EQ.'BOX')THEN
        NPLOTS=NPLOTS+IMPNR+IMPNC-1
      ENDIF
      DO5300IRES=1,IROWT
        DO5400IFAC=1,ICOLT
C
          IPLOT=IPLOT+1
          IF(IPLOT.GT.NPLOTS)GOTO8000
          IHARG(NUMARG)=IHARG(IFRST+IPLOT-1)
          IHARG2(NUMARG)=IHARG2(IFRST+IPLOT-1)
          IARG(NUMARG)=IARG(IFRST+IPLOT-1)
          ARG(NUMARG)=ARG(IFRST+IPLOT-1)
          IARGT(NUMARG)=IARGT(IFRST+IPLOT-1)
C
          IXLIST=IFAC
          IROW=INT(IPLOT/IMPNC)+1
          IF(MOD(IPLOT,IMPNC).EQ.0)IROW=IROW-1
          ICOL=MOD(IPLOT,IMPNC)
          IF(ICOL.EQ.0)ICOL=IMPNC
C
          IEMPTY='NO'
          ITEMP=IFAC
          IF(IFPLLA.EQ.'BOX')THEN
            ICOL=ICOL-1
            ITEMP=IFAC-1
            IF(ITEMP.EQ.0)IEMPTY='YES'
            IF(IROW.EQ.IMPNR)IEMPTY='YES'
          ENDIF
C
          IF(IEMPTY.EQ.'YES')THEN
            DO5304I=1,MAXSUB
              ISU2SW(I)=ISUBSW(I)
              ISUBSW(I)='OFF'
 5304       CONTINUE
          ENDIF
          IOPTN=3
          IDX=1
          IDY=1
          ICASP2='FACT'
C
CCCCC NOTE: DPSPM4 IMPLEMENTS "SUB-REGIONS" ON PLOTS.  THESE DON'T
CCCCC       SEEM PARTICULARLY RELEVANT FOR THESE PLOTS, SO COMMENT
CCCCC       OUT FOR NOW.  HOWEVER, LEAVE IN CASE WE DECIDE LATER TO
CCCCC       IMPLEMENT THEM.
C
CCCCC     CALL DPSPM4(ICASP2,IOPTN,IDX,IDY,
CCCCC1                ISUBNU,ISUBSW,
CCCCC1                ASUBXL,ASUBXU,ASUBYL,ASUBYU,
CCCCC1                ISUBN9,ISUBSZ,
CCCCC1                ASBXL2,ASBXU2,ASBYL2,ASBYU2,
CCCCC1                PFPXSL,PFPXSU,PFPYSL,PFPYSU,
CCCCC1                IBUGG2,ISUBRO,IERROR)
C
          ICASP2=ICASPL
          IRES2=IRES
          IXLST2=IXLIST+1
          IX=IFAC+1
          CALL DPSPM1(ICASP2,IVARN1,IVARN2,ICOLL,
     1                IMPNR,IMPNC,IROW,ICOL,IRES2,IX,IPLOT,
     1                NPLOTS,NUMVAR,
     1                ICHAP2,ILINP2,
     1                GY1MNS,GY1MXS,GY2MNS,GY2MXS,
     1                GX1MNS,GX1MXS,GX2MNS,GX2MXS,
     1                IY1MNS,IY1MXS,IY2MNS,IY2MXS,
     1                IX1MNS,IX1MXS,IX2MNS,IX2MXS,
     1                IX1TSV,IX2TSV,IY1TSV,IY2TSV,
     1                IX1ZSV,IX2ZSV,IY1ZSV,IY2ZSV,
     1                PX1LD2,PX2LD2,
     1                IY1LJ2,IY1LD2,PY1LD2,PY1LA2,
     1                IX1LT2,IX2LT2,IY1LT2,IY2LT2,
     1                NCX1L2,NCX2L2,NCY1L2,NCY2L2,
     1                PFPXLL,PFPXUL,PFPYLL,PFPYUL,IXLST2,
     1                IFPLLA,IFPLLD,IPLOTT,IFPLFR,IFPLXA,IFPLYA,
     1                IFPLDI,ISPX1L,
     1                ISPMXT,ISPMXL,ISPMYT,ISPMYL,
     1                IFPLTD,PFPLTD,IVNMEX,
     1                IBUGG2,ISUBRO)
C
          IF(IEMPTY.EQ.'YES')THEN
            DO5306I=1,100
              ICHAPA(I)='BLAN'
              ILINPA(I)='BLAN'
              ISPISW(I)='OFF'
              IBARSW(I)='OFF'
 5306       CONTINUE
          ENDIF
C
          CALL MAINGR(ANOPL1,ANOPL2,NPLOTV,NPLOTP,NS,ICASPL,
     1                MAXNPP,ISEED,IBOOSS,
     1                IX1TSV,IX2TSV,IY1TSV,IY2TSV,
     1                IX1ZSV,IX2ZSV,IY1ZSV,IY2ZSV,
     1                BARHEF,BARWEF,
     1                IRHSTG,IHSTCW,IHSTEB,IHSTOU,IASHWT,IHSTMC,IHSTOP,
     1                ICAPSW,IFORSW,
     1                IGUIFL,IERRFA,
     1                IAND1,IAND2,ICONT,NUMHPP,NUMVPP,
CCCCC1                TEMP,TEMP2,TEMP3,XTEMP1,XTEMP2,MAXNXT,
     1                MAXNXT,
     1                ISUBRO,IFOUND,IERROR)
C
CCCCC NOTE: DPSPM3 SETS AN X2LABEL BASED ON CORRELATION, EFFECT
CCCCC       SIZE, OR NUMBER OF REJECTIONS.  THIS DOESN"T SEEM
CCCCC       PARTICULARLY USEFUL FOR THESE PLOTS, SO COMMENT OUT
CCCCC       FOR NOW.  HOWEVER, LEAVE CODE HERE IN CASE WE DECIDE TO
CCCCC       ACTIVATE LATER.
C
CCCCC     IF(IEMPTY.EQ.'NO')THEN
CCCCC       CALL DPSPM3(ICASPL,IOUNI5,
CCCCC1                  IROW,ICOL,
CCCCC1                  PX2LD2,NPLOTP,
CCCCC1                  IFORSW,
CCCCC1                  IFPX2L,ISPX2P,ISPX2S,
CCCCC1                  IHRIGH,IHRIG2,IHWUSE,
CCCCC1                  ISUBN1,ISUBN2,MESSAG,
CCCCC1                  IBUGG2,ISUBRO,IERROR)
CCCCC     ENDIF
C
          ICONT=IDCONT(1)
          IPOWE=IDPOWE(1)
          NUMHPP=IDNHPP(1)
          IMPARG=2
          CALL DPGRAP(Y,X,X3D,D,N,NPLOTP,ICASPL,ICONT,IPOWE,NUMHPP,
     1                XMATN,YMATN,XMITN,YMITN,
     1                ISQUAR,
     1                IVGMSW,IHGMSW,
     1                IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,
     1                IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,
     1                YPLOT,XPLOT,X2PLOT,TAGPLO,
     1                IMPSW,IMPNR,IMPNC,IMPCO,IMPCO9,
     1                IMPARG,
     1                PMXMIN,PMXMAX,PMYMIN,PMYMAX,
     1                MAXCOL,
     1                DSIZE,DSYMB,DCOLOR,DFILL,
     1                ICAPSW,
     1                IBUGUG,IBUGU2,IBUGU3,IBUGU4,ISUBRO,
     1                IERROR)
          IF(IERROR.EQ.'NO')IAND1=IAND2
          IF(IERROR.EQ.'YES')GOTO5499
C
          IF(IFPLFI.EQ.'NONE')GOTO5499
          IF(IEMPTY.EQ.'YES')GOTO5499
C
          IMPCO=IMPCO-1
          IF(IMPCO.LE.1)IERASW='OFF'
C
          ICNTPL=0
          IOUNI5=-99
          CALL DPSPM2(ICASPL,IVARN1,IVARN2,ICOLL,NUMVAR,NPLOTP,
     1                IRES,IX,
     1                TEMP,TEMP2,TEMP3,XTEMP1,XTEMP2,MAXNXT,
     1                ALOWFR,ALOWDG,
     1                IANGLU,MAXNPP,IAND1,IAND2,
     1                IFPLFI,IFPLTA,
     1                XMATN,YMATN,XMITN,YMITN,
     1                ISQUAR,
     1                IVGMSW,IHGMSW,
     1                IMPSW,IMPNR,IMPNC,IMPCO,IMPCO9,
     1                IREPCH,
     1                PMXMIN,PMXMAX,PMYMIN,PMYMAX,
     1                ICNTPL,IOUNI5,
     1                IBUGG2,IBUGG3,IBUGCO,IBUGEV,IBUGQ,
     1                IBUGUG,IBUGU2,IBUGU3,IBUGU4,
     1                ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GOTO5499

 5499     CONTINUE
          IERROR='NO'
C
          ISHIFT=NCCOMM
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGG2,IERROR)
          ICOM=ICT
          ICOM2=IC2T
          IF(NCCOMM.GT.0)THEN
            DO5491II=1,NCCOMM
              IHARG(II)=IHT(II)
              IHARG2(II)=IH2T(II)
              IARG(II)=0
              ARG(II)=0.0
              IARGT(II)='WORD'
 5491       CONTINUE
          ENDIF
          IFRST=NCCOMM+2
          IHARG(NUMARG)='    '
          IHARG2(NUMARG)='    '
          IARG(NUMARG)=0
          ARG(NUMARG)=0.0
          IARGT(NUMARG)=IARGT(IFRST)
          NARGT=NUMARG
C
        PX1LDS=PX1LD2
        GX1MIN=GX1MNS
        GX1MAX=GX1MXS
        GX2MIN=GX2MNS
        GX2MAX=GX2MXS
        GY1MIN=GY1MNS
        GY1MAX=GY1MXS
        GY2MIN=GY2MNS
        GY2MAX=GY2MXS
        IX1MIN=IX1MNS
        IX1MAX=IX1MXS
        IX2MIN=IX2MNS
        IX2MAX=IX2MXS
        IY1MIN=IY1MNS
        IY1MAX=IY1MXS
        IY2MIN=IY2MNS
        IY2MAX=IY2MXS
        PX1ZDS=PX1ZD2
        PX2ZDS=PX2ZD2
        PY1ZDS=PY1ZD2
        PY2ZDS=PY2ZD2
        IF(IEMPTY.EQ.'YES')THEN
          DO5407I=1,MAXSUB
            ISUBSW(I)=ISU2SW(I)
 5407     CONTINUE
        ENDIF
        DO5408I=1,100
            ICHAPA(I)=ICHAP2(I)
            ILINPA(I)=ILINP2(I)
            ISPISW(I)=ISPIS2(I)
            IBARSW(I)=IBARS2(I)
 5408   CONTINUE
        IF(IERROR.EQ.'YES')GOTO5400
C
 5400 CONTINUE
 5300 CONTINUE
      GOTO8000
C
C
C               **************************************************
C               **   STEP 28--                                  **
C               **   REINSTATE INITIAL SETTINGS                 **
C               **************************************************
C
 8000 CONTINUE
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'PRPL')THEN
        ISTEPN='28'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,8807)IMANUF,NUMDEV,IDMANU(1)
 8807   FORMAT('IMANUF,NUMDEV,IDMANU(1) = ',A4,I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IFLAG=2
      CALL DPSPM5(IFLAG,IMPSW,IMPCO,IMPNR,IMPNC,
     1            IBUGG2,ISUBRO,IFOUND,IERROR)
      IFPLLA=IFPLL2
      IFPLTA=IFPLTZ
      IFPLFR=IFPLFZ
      IFPLPT=IFPLPZ
      IFPLLD=IFPLLZ
      IFPLXA=IFPLXZ
      IFPLYA=IFPLYZ
      IFPLDI=IFPLDZ
      IFPLST=IFPLZT
      IFPLS2=IFPLZ2
      IFPLS3=IFPLZ3
      IFPLS4=IFPLZ4
      IFEEDB=IFEED9
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPPRPL--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IFOUND,IERROR
 9012 FORMAT('IFOUND,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9013 FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',
     1I8,I8,I8,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)NUMARG
 9014 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NUMARG.LE.0)GOTO9029
      DO9021I=1,NUMARG
      WRITE(ICOUT,9022)I,IHARG(I),IARGT(I)
 9022 FORMAT('I,IHARG(I),IARGT(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9021 CONTINUE
 9029 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPPRPO(ICOM,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                  IPPDE1,IPPDE2,
     1                  IBUGS2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE PREPLOT/POSTPLOT DEVICE
C              THAT IS, THE CURRENT DEVICE IN WHICH
C              THE USER WANTS A USER-SPECIFIED
C              PREPLOT LINE TO BE WRITTEN OUT,
C              AND A USER-DEFINED POSTPLOT LINE
C              TO BE WRITTEN OUT.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C     OUTPUT ARGUMENTS--IPPDE1  (A HOLLERITH VARIABLE)
C                       IPPDE2  (A HOLLERITH VARIABLE)
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
C     VERSION NUMBER--86/9
C     ORIGINAL VERSION--OCTOBER  1986.
C     UPDATED         --FEBRUARY 1987.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
      CHARACTER*4 IHARG2
CCCCC CHARACTER*4 IARG   JULY 1987
CCCCC CHARACTER*4 ARG     JULY 1987
      CHARACTER*4 IARGT
C
      CHARACTER*4 IPPDE1
      CHARACTER*4 IPPDE2
      CHARACTER*4 IBUGS2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD1
      CHARACTER*4 IHOLD2
C
      CHARACTER*4 IHARG1
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
      DIMENSION IHARG2(*)
      DIMENSION IARG(*)
      DIMENSION ARG(*)
      DIMENSION IARGT(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
      IFOUND='YES'
C
      IHARG1=IHARG(1)
C
      IF(ICOM.EQ.'PRE')GOTO1109
      IF(ICOM.EQ.'PREP')GOTO1109
      IF(ICOM.EQ.'POST')GOTO1109
      ISHIFT=1
      CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1IBUGS2,IERROR)
 1109 CONTINUE
C
      IF(NUMARG.LE.0)GOTO1120
C
      IF(IHARG(NUMARG).EQ.'ON')GOTO1120
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1120
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1120
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1120
C
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'POST')GOTO1120
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'DEVI')GOTO1120
      IF(NUMARG.EQ.1)GOTO1130
C
      IF(NUMARG.EQ.2.AND.IHARG(1).EQ.'POST'
     1              .AND.IHARG(2).EQ.'DEVI')GOTO1120
      IF(NUMARG.EQ.2.AND.IHARG(1).EQ.'POST'
     1              .AND.IHARG(2).NE.'DEVI')GOTO1130
      IF(NUMARG.EQ.2.AND.IHARG(1).EQ.'DEVI')GOTO1130
C
      IF(NUMARG.EQ.3.AND.IHARG(1).EQ.'POST'
     1              .AND.IHARG(2).EQ.'DEVI')GOTO1130
      IF(NUMARG.EQ.3.AND.IHARG(1).EQ.'POST'
     1              .AND.IHARG(2).NE.'DEVI')GOTO1140
      IF(NUMARG.EQ.3.AND.IHARG(1).EQ.'DEVI')GOTO1140
C
      GOTO1140
C
 1120 CONTINUE
      IHOLD1='NONE'
      IHOLD2='    '
      GOTO1180
C
 1130 CONTINUE
      IHOLD1=IHARG(NUMARG)
      IHOLD2='    '
      GOTO1180
C
 1140 CONTINUE
      NUMAM1=NUMARG-1
      IHOLD1=IHARG(NUMAM1)
      IHOLD2=IHARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IPPDE1=IHOLD1
      IPPDE2=IHOLD2
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1188)IPPDE1,IPPDE2
 1188 FORMAT('THE PREPLOT/POSTPLOT DEVICE HAS JUST BEEN SET TO ',
     1A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPPRSW(IHARG,NUMARG,IPRIN2,IFOUND,IERROR)
C
C     PURPOSE--SPECIFY THE PRINTING SWITCH WHICH IN TURN
C              DETERMINES WHETHER ANY SUBSEQUENT NON-GRAPHICAL OUTPUT
C              WILL BE PRINTED OR NOT.
C              THIS CAPABILITY IS USEFUL IF ONE WISHES TO SUPPRESS
C              OUTPUT FROM ALL PRELIMINARY AND INTERMEDIATE
C              CALCULATIONS AND JUST HAVE THE FINAL PLOTS THEMSELVES
C              APPEAR ON THE SCREEN.
C              THE SPECIFIED PRINTING SWITCH SPECIFICATION
C              WILL BE PLACED IN THE HOLLERITH VARIABLE IPRIN2.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C     OUTPUT ARGUMENTS--IPRIN2 (A HOLLERITH VARIABLE)
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
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--NOVEMBER  1980.
C     UPDATED         --FEBRUARY  1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --JANUARY   2015. SAVE/RESTORE OPTION
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IPRIN2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD
C
      CHARACTER*4 IPRISV
      COMMON/IPRINT/IPRISV
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
      IHOLD='    '
C
      IF(NUMARG.LE.0 .OR. IHARG(NUMARG).EQ.'ON' .OR.
     1   IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA')THEN
        IHOLD='ON'
        GOTO1180
      ELSEIF(IHARG(NUMARG).EQ.'OFF')THEN
        IHOLD='OFF'
        GOTO1180
      ELSEIF(IHARG(NUMARG).EQ.'SAVE')THEN
        IPRISV=IPRINT
        GOTO1199
      ELSEIF(IHARG(NUMARG).EQ.'REST')THEN
        IPRINT=IPRISV
        GOTO1180
      ELSE
        GOTO1199
      ENDIF
C
 1180 CONTINUE
      IFOUND='YES'
      IPRIN2=IHOLD
      IPRINT=IPRIN2
C
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1181)IPRIN2
 1181   FORMAT('THE PRINTING SWITCH HAS JUST BEEN SET TO ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPPYRA(IHARG,IARGT,ARG,NUMARG,
     1                  PXSTAR,PYSTAR,PXEND,PYEND,
     1                  ILINPA,ILINCO,PLINTH,
     1                  AREGBA,IREBLI,IREBCO,PREBTH,
     1                  IREFSW,IREFCO,
     1                  IREPTY,IREPLI,IREPCO,PREPTH,PREPSP,
     1                  PTEXHE,PTEXWI,PTEXVG,PTEXHG,
     1                  IGRASW,IDIASW,
     1                  PGRAXF,PGRAYF,PDIAXC,PDIAYC,PDIAX2,PDIAY2,
     1                  PDIAHE,PDIAWI,PDIAVG,PDIAHG,
     1                  NUMDEV,IDMANU,IDMODE,IDMOD2,IDMOD3,
     1                  IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,
     1                  IDNVOF,IDNHOF,IDFONT,UNITSW,PDSCAL,
     1                  IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DRAW ONE OR MORE PYRAMIDS (DEPENDING ON HOW MANY NUMBERS
C              ARE PROVIDED).  THE COORDINATES ARE IN STANDARDIZED UNITS
C              OF 0 TO 100.
C     NOTE--THE INPUT COORDINATES DEFINE THE VERTICES OF THE FRONT FACE
C           OF THE PYRAMID.
C     NOTE-THE USUAL INPUT NUMBER OF COORDINATES IS 3
C          AND THEREFORE THE USUAL INPUT NUMBER OF NUMBERS IS 2*3 = 6.
C     NOTE--IF 4 NUMBERS ARE PROVIDED, THEN THE DRAWN PYRAMID WILL GO
C           FROM THE LAST CURSOR POSITION (ASSUMED TO BE AT VERTEX 1)
C           THROUGH THE (X,Y) POINT (EITHER ABSOLUTE OR RELATIVE) AS
C           DEFINED BY THE FIRST AND SECOND NUMBERS (ASSUMED TO BE AT
C           VERTEX 2) TO THE (X,Y) POINT (EITHER ABSOLUTE OR RELATIVE)
C           AS DEFINED BY THE THIRD AND FOURTH NUMBERS (ASSUMED TO BE AT
C           VERTEX 3) AND CONTINUING BACK THE START POINT TO CLOSE THE
C           PYRAMID.
C     NOTE--IF 6 NUMBERS ARE PROVIDED, THEN THE DRAWN PYRAMID WILL GO
C           FROM THE ABSOLUTE (X,Y) POSITION AS RESULTING FORM THE FIRST
C           AND SECOND NUMBERS (ASSUMED TO BE AT VERTEX 1) THROUGH THE
C           (X,Y) POINT (EITHER ABSOLUTE OR RELATIVE) AS DEFINED BY THE
C           THIRD AND FOURTH NUMBERS (ASSUMED TO BE AT VERTEX 2) TO THE
C           (X,Y) POINT (EITHER ABSOLUTE OR RELATIVE) AS DEFINED BY THE
C           FIFTH AND SIXTH NUMBERS (ASSUMED TO BE AT VERTEX 3) AND THEN
C           CONTINUING BACK THE START POINT TO CLOSE THE PYRAMID.
C     NOTE--AND SO FORTH FOR 10, 14, 18, ... NUMBERS.
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
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--87/5
C     ORIGINAL VERSION--APRIL     1987.
C     UPDATED         --JANUARY   1989. CALL LIST FOR OFFSET VAR (ALAN)
C     UPDATED         --MARCH     1997. SUPPORT FOR DEVICE FONT (ALAN)
C     UPDATED         --JULY      1997. SUPPORT FOR "DATA" UNITS (ALAN)
C     UPDATED         --DECEMBER  2018. CHECK FOR DISCRETE, NULL, OR
C                                       NONE DEVICE
C     UPDATED         --DECEMBER  2018. SUPPORT FOR "DEVICE ... SCALE"
C                                       COMMAND
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
CCCCC ADD FOLLOWING LINE MARCH 1997.
      CHARACTER*4 IDFONT
CCCCC ADD FOLLOWING LINE JULY 1997.
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
      DIMENSION PLINTH(*)
C
      DIMENSION AREGBA(*)
      DIMENSION IREBLI(*)
      DIMENSION IREBCO(*)
      DIMENSION PREBTH(*)
      DIMENSION IREFSW(*)
      DIMENSION IREFCO(*)
      DIMENSION IREPTY(*)
      DIMENSION IREPLI(*)
      DIMENSION IREPCO(*)
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
CCCCC ADD FOLLOWING LINE MARCH 1997.
      DIMENSION IDFONT(*)
      DIMENSION IDNVPP(*)
      DIMENSION IDNHPP(*)
      DIMENSION IDUNIT(*)
C
      DIMENSION IDNVOF(*)
      DIMENSION IDNHOF(*)
C
C-----COMMON----------------------------------------------------------
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
CCCCC IBUGG4=IBUGD2
CCCCC ISUBG4=ISUBRO
C
      ILOCFN=0
      NUMNUM=0
C
      X1=0.0
      Y1=0.0
      X2=0.0
      Y2=0.0
C
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'PYRA')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPPYRA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)NUMARG
   53 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO55I=1,NUMARG
      WRITE(ICOUT,56)I,IHARG(I),IARGT(I),ARG(I)
   56 FORMAT('I,IHARG(I),IARGT(I),ARG(I) = ',I8,2X,A4,2X,A4,E15.7)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
      WRITE(ICOUT,57)PXSTAR,PYSTAR
   57 FORMAT('PXSTAR,PYSTAR = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,58)PXEND,PYEND
   58 FORMAT('PXEND,PYEND = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,61)ILINPA(1),ILINCO(1),PLINTH(1)
   61 FORMAT('ILINPA(1),ILINCO(1),PLINTH(1) = ',A4,2X,A4,E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,62)AREGBA(1)
   62 FORMAT('AREGBA(1) = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,63)IREBLI(1),IREBCO(1),PREBTH(1)
   63 FORMAT('IREBLI(1),IREBCO(1),PREBTH(1) = ',A4,2X,A4,E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,64)IREFSW(1),IREFCO(1)
   64 FORMAT('IREFSW(1),IREFCO(1) = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,65)IREPTY(1),IREPLI(1),IREPCO(1),PREPTH(1),PREPSP(1)
   65 FORMAT('IREPTY(1),IREPLI(1),IREPCO(1),PREPTH(1),PREPSP(1) = ',
     1A4,2X,A4,2X,A4,2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,69)PTEXHE,PTEXWI
   69 FORMAT('PTEXHE,PTEXWI= ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,70)PTEXVG,PTEXHG
   70 FORMAT('PTEXVG,PTEXHG= ',2E15.6)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,76)IGRASW,IDIASW
   76 FORMAT('IGRASW,IDIASW = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,77)PGRAXF,PGRAYF,PDIAXC,PDIAYC
   77 FORMAT('PGRAXF,PGRAYF,PDIAXC,PDIAYC = ',4E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,78)PDIAHE,PDIAWI,PDIAVG,PDIAHG
   78 FORMAT('PDIAHE,PDIAWI,PDIAVG,PDIAHG = ',4E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,80)NUMDEV
   80 FORMAT('NUMDEV= ',I8)
      CALL DPWRST('XXX','BUG ')
      DO81I=1,NUMDEV
      WRITE(ICOUT,82)IDMANU(I),IDMODE(I),IDMOD2(I),IDMOD3(I)
   82 FORMAT('IDMANU(I),IDMODE(I),IDMOD2(I),IDMOD3(I) = ',
     1A4,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,83)IDPOWE(I),IDCONT(I),IDCOLO(I)
   83 FORMAT('IDPOWE(I),IDCONT(I),IDCOLO(I) = ',
     1A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,84)IDNVPP(I),IDNHPP(I),IDUNIT(I)
   84 FORMAT('IDNVPP(I),IDNHPP(I),IDUNIT(I) = ',
     1I8,I8,I8)
      CALL DPWRST('XXX','BUG ')
   81 CONTINUE
      WRITE(ICOUT,87)IFOUND
   87 FORMAT('IFOUND= ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,88)IBUGG4,ISUBG4,IERRG4
   88 FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,89)IBUGD2,IERROR
   89 FORMAT('IBUGD2,IERROR= ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
      IFIG='PYRA'
      NUMPT=3
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
      IBACCO='JUNK'
C
      CALL DPOPPL(IGRASW,
     1IBELSW,NUMRIN,IERASW,
     1IBACCO)
C
C               *****************************************
C               **  STEP 2--                           **
C               **  SEARCH FOR COMMAND SPECIFICATIONS  **
C               *****************************************
C
      IF(NUMARG.GE.2.AND.
     1IARGT(1).EQ.'NUMB'.AND.IARGT(2).EQ.'NUMB')
     1GOTO1111
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'ABSO'.AND.
     1IARGT(2).EQ.'NUMB'.AND.IARGT(3).EQ.'NUMB')
     1GOTO1112
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'RELA'.AND.
     1IARGT(2).EQ.'NUMB'.AND.IARGT(3).EQ.'NUMB')
     1GOTO1113
      GOTO1130
C
 1111 CONTINUE
      ITYPEO='ABSO'
      ILOCFN=1
      GOTO1119
C
 1112 CONTINUE
      ITYPEO='ABSO'
      ILOCFN=2
      GOTO1119
C
 1113 CONTINUE
      ITYPEO='RELA'
      ILOCFN=2
      GOTO1119
 1119 CONTINUE
C
      IF(ILOCFN.GT.NUMARG)GOTO1129
      DO1120I=ILOCFN,NUMARG
      IF(IARGT(I).EQ.'NUMB')GOTO1120
      GOTO1129
 1120 CONTINUE
      IFOUND='YES'
      GOTO1149
 1129 CONTINUE
      GOTO1130
C
 1130 CONTINUE
      IERRG4='YES'
      WRITE(ICOUT,1131)
 1131 FORMAT('***** ERROR IN DPPYRA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1132)
 1132 FORMAT('      ILLEGAL FORM FOR DRAW ',
     1'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1134)
 1134 FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE ',
     1'PROPER FORM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1135)
 1135 FORMAT('      SUPPOSE IT IS DESIRED TO DRAW A PYRAMID WITH ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1136)
 1136 FORMAT('      FRONT FACE VERTICES (20,20), (50,20), (35,40)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1141)
 1141 FORMAT('      THEN ALLOWABLE FORMS ARE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1142)
 1142 FORMAT('      PYRAMID 20 20 50 20 35 40')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1143)
 1143 FORMAT('      PYRAMID ABSOLUTE 20 20 50 20 35 40')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
 1149 CONTINUE
C
C               ****************************
C               **  STEP 3--              **
C               **  DRAW OUT THE LINE(S)  **
C               ****************************
C
      NUMNUM=NUMARG-ILOCFN+1
      IF(NUMNUM.LT.NUMPT2)GOTO1151
      GOTO1152
C
 1151 CONTINUE
      J=ILOCFN-1
      X1=PXSTAR
      Y1=PYSTAR
      GOTO1159
C
 1152 CONTINUE
      J=ILOCFN
      IF(J.GT.NUMARG)GOTO1190
      X1=ARG(J)
CCCCC THE FOLLOWING LINE WAS ADDED JULY 1997
      IF(UNITSW.EQ.'DATA')CALL DPCODS('X',X1,X1,IBUGD2,ISUBRO,IERROR)
      J=J+1
      IF(J.GT.NUMARG)GOTO1190
      Y1=ARG(J)
CCCCC THE FOLLOWING LINE WAS ADDED JULY 1997
      IF(UNITSW.EQ.'DATA')CALL DPCODS('Y',Y1,Y1,IBUGD2,ISUBRO,IERROR)
      GOTO1159
 1159 CONTINUE
C
 1160 CONTINUE
      J=J+1
      IF(J.GT.NUMARG)GOTO1190
      X2=ARG(J)
CCCCC THE FOLLOWING LINE WAS ADDED JULY 1997
      IF(UNITSW.EQ.'DATA')CALL DPCODS('X',X2,X2,IBUGD2,ISUBRO,IERROR)
      IF(ITYPEO.EQ.'RELA')X2=X1+X2
      J=J+1
      IF(J.GT.NUMARG)GOTO1190
      Y2=ARG(J)
CCCCC THE FOLLOWING LINE WAS ADDED JULY 1997
      IF(UNITSW.EQ.'DATA')CALL DPCODS('Y',Y2,Y2,IBUGD2,ISUBRO,IERROR)
      IF(ITYPEO.EQ.'RELA')Y2=Y1+Y2
C
      J=J+1
      IF(J.GT.NUMARG)GOTO1190
      X3=ARG(J)
CCCCC THE FOLLOWING LINE WAS ADDED JULY 1997
      IF(UNITSW.EQ.'DATA')CALL DPCODS('X',X3,X3,IBUGD2,ISUBRO,IERROR)
      IF(ITYPEO.EQ.'RELA')X3=X2+X3
      J=J+1
      IF(J.GT.NUMARG)GOTO1190
      Y3=ARG(J)
CCCCC THE FOLLOWING LINE WAS ADDED JULY 1997
      IF(UNITSW.EQ.'DATA')CALL DPCODS('Y',Y3,Y3,IBUGD2,ISUBRO,IERROR)
      IF(ITYPEO.EQ.'RELA')Y3=Y2+Y3
C
      CALL DPPYR2(X1,Y1,X2,Y2,X3,Y3,
     1IFIG,
     1ILINPA,ILINCO,PLINTH,
     1AREGBA,
     1IREBLI,IREBCO,PREBTH,
     1IREFSW,IREFCO,
     1IREPTY,IREPLI,IREPCO,PREPTH,PREPSP,
     1PTEXHE,PTEXWI,PTEXVG,PTEXHG)
C
      X1=X3
      Y1=Y3
C
      GOTO1160
 1190 CONTINUE
C
      PXEND=X3
      PYEND=Y3
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
     1PGRAXF,PGRAYF,
     1IGRASW,PDIAXC,PDIAYC,PDIAX2,PDIAY2,
     1PDIAHE,PDIAWI,PDIAVG,PDIAHG)
C
      CALL DPCLDE
C
 8000 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'PYRA')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPPYRA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)ILOCFN,NUMNUM
 9012 FORMAT('ILOCFN,NUMNUM = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)X1,Y1,X2,Y2,X3,Y3
 9013 FORMAT('X1,Y1,X2,Y2,X3,Y3 = ',6E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)PXSTAR,PYSTAR
 9015 FORMAT('PXSTAR,PYSTAR = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9016)PXEND,PYEND
 9016 FORMAT('PXEND,PYEND = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9017)IFIG
 9017 FORMAT('IFIG = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9027)IFOUND
 9027 FORMAT('IFOUND = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9028)IBUGG4,ISUBG4,IERRG4
 9028 FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9029)IBUGD2,IERROR
 9029 FORMAT('IBUGD2,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPPYR2(X1,Y1,X2,Y2,X3,Y3,
     1IFIG,
     1ILINPA,ILINCO,PLINTH,
     1AREGBA,
     1IREBLI,IREBCO,PREBTH,
     1IREFSW,IREFCO,
     1IREPTY,IREPLI,IREPCO,PREPTH,PREPSP,
     1PTEXHE,PTEXWI,PTEXVG,PTEXHG)
C
C     PURPOSE--DRAW A PYRAMID
C              WITH FRONT FACE VERTICES AT (X1,Y1),
C              (X2,Y2), AND (X3,Y3).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--87/5
C     ORIGINAL VERSION--APRIL     1987.
C     UPDATED         --JANUARY   1989.  MODIFY CALLS TO DPDRPL (ALAN)
C     UPDATED         --JANUARY   1989.  MODIFY CALL  TO DPFIRE (ALAN)
C
C-----NON-COMMON VARIABLES-------------------------------------
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
      DIMENSION PX(10)
      DIMENSION PY(10)
CCCCC DIMENSION PX3(10)
CCCCC DIMENSION PY3(10)
C
      DIMENSION ILINPA(*)
      DIMENSION ILINCO(*)
      DIMENSION PLINTH(*)
C
      DIMENSION AREGBA(*)
      DIMENSION IREBLI(*)
      DIMENSION IREBCO(*)
      DIMENSION PREBTH(*)
      DIMENSION IREFSW(*)
      DIMENSION IREFCO(*)
      DIMENSION IREPTY(*)
      DIMENSION IREPLI(*)
      DIMENSION IREPCO(*)
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
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'PYR2')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPPYR2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)X1,Y1
   53 FORMAT('X1,Y1 = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)X2,Y2
   54 FORMAT('X2,Y2 = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IFIG
   59 FORMAT('IFIG = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,61)ILINPA(1),ILINCO(1),PLINTH(1)
   61 FORMAT('ILINPA(1),ILINCO(1),PLINTH(1) = ',A4,2X,A4,E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,62)AREGBA(1)
   62 FORMAT('AREGBA(1) = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,63)IREBLI(1),IREBCO(1),PREBTH(1)
   63 FORMAT('IREBLI(1),IREBCO(1),PREBTH(1) = ',A4,2X,A4,E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,64)IREFSW(1),IREFCO(1)
   64 FORMAT('IREFSW(1),IREFCO(1) = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,65)IREPTY(1),IREPLI(1),IREPCO(1),PREPTH(1),PREPSP(1)
   65 FORMAT('IREPTY(1),IREPLI(1),IREPCO(1),PREPTH(1),PREPSP(1) = ',
     1A4,2X,A4,2X,A4,2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,69)PTEXHE,PTEXWI
   69 FORMAT('PTEXHE,PTEXWI= ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,70)PTEXVG,PTEXHG
   70 FORMAT('PTEXVG,PTEXHG= ',2E15.6)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,79)IBUGG4,ISUBG4,IERRG4
   79 FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               *********************************
C               **  STEP 1--                   **
C               **  SET THE SPECS              **
C               **  WHICH CONTROL THE          **
C               **  APPEARANCE OF THE          **
C               **  RESULTING CUBE.            **
C               *********************************
C
      DELX21=ABS(X2-X1)
      DELY32=ABS(Y3-Y2)
C
      P3DX=0.1
      P3DY=0.3
C
C               *************************
C               **  STEP 2--           **
C               **  FILL THE FIGURE    **
C               **  (IF CALLED FOR)    **
C               *************************
C
      IF(IREFSW(1).EQ.'OFF')GOTO2190
C
      IPATT=IREPTY(1)
      PTHICK=PREPTH(1)
      PXGAP=PREPSP(1)
      PYGAP=PREPSP(1)
      ICOLF=IREFCO(1)
      ICOLP=IREPCO(1)
C
      IF(IREFSW(1).EQ.'ON')GOTO2110
      IF(IREFSW(1).EQ.'ONF')GOTO2110
      IF(IREFSW(1).EQ.'ONS')GOTO2120
      IF(IREFSW(1).EQ.'ONFS')GOTO2110
      IF(IREFSW(1).EQ.'ONSF')GOTO2110
C
C               ********************************
C               **  STEP 2.1--                **
C               **  FRONT FACE ONLY           **
C               ********************************
C
 2110 CONTINUE
      PX(1)=X1
      PY(1)=Y1
C
      PX(2)=X2
      PY(2)=Y2
C
      PX(3)=X3
      PY(3)=Y3
C
      PX(4)=X1
      PY(4)=Y1
C
      NP=4
C
      IPATT2='SOLI'
      CALL DPFIRE(PX,PY,NP,
     1IFIG,IPATT,PTHICK,PXGAP,PYGAP,ICOLF,ICOLP,IPATT2)
C
      IF(IREFSW(1).EQ.'ON')GOTO2120
      IF(IREFSW(1).EQ.'ONF')GOTO2190
      IF(IREFSW(1).EQ.'ONS')GOTO2120
      IF(IREFSW(1).EQ.'ONFS')GOTO2120
      IF(IREFSW(1).EQ.'ONSF')GOTO2120
C
C               ********************************
C               **  STEP 2.2--                **
C               **  SIDE (= RIGHT) FACE ONLY  **
C               ********************************
C
 2120 CONTINUE
      PX(1)=X3
      PY(1)=Y3
C
      PX(2)=X2-P3DX*DELX21
      PY(2)=Y2+P3DY*DELY32
C
      PX(3)=X2
      PY(3)=Y2
C
      PX(4)=X3
      PY(4)=Y3
C
      NP=4
C
      IPATT2='SOLI'
      CALL DPFIRE(PX,PY,NP,
     1IFIG,IPATT,PTHICK,PXGAP,PYGAP,ICOLF,ICOLP,IPATT2)
C
      GOTO2190
C
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
C
      PX(1)=X1
      PY(1)=Y1
C
      PX(2)=X2
      PY(2)=Y2
C
      PX(3)=X3
      PY(3)=Y3
C
      PX(4)=X1
      PY(4)=Y1
C
      NP=4
C
      IFLAG='ON'
CCCCC CALL DPDRPL(PX,PY,NP,PX3,PY3,NP3,
CCCCC1IFIG,IPATT,PTHICK,ICOL)
      CALL DPDRPL(PX,PY,NP,
     1IFIG,IPATT,PTHICK,ICOL,
     1JPATT,JTHICK,PTHIC2,JCOL,IFLAG)
C
      PX(1)=X3
      PY(1)=Y3
C
      PX(2)=X2-0.1*DELX21
      PY(2)=Y2+0.3*DELY32
C
      PX(3)=X2
      PY(3)=Y2
C
      NP=3
C
      IFLAG='ON'
CCCCC CALL DPDRPL(PX,PY,NP,PX3,PY3,NP3,
CCCCC1IFIG,IPATT,PTHICK,ICOL)
      CALL DPDRPL(PX,PY,NP,
     1IFIG,IPATT,PTHICK,ICOL,
     1JPATT,JTHICK,PTHIC2,JCOL,IFLAG)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
      IF(IBUGG4.EQ.'ON' .OR. ISUBG4.EQ.'PYR2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPPYR2--')
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,NP
          WRITE(ICOUT,9016)I,PX(I),PY(I)
 9016     FORMAT('I,PX(I),PY(I) = ',I8,2E15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
        WRITE(ICOUT,9022)DELX21,DELY32,P3DX,P3DY
 9022   FORMAT('DELX21,DELY32,P3DX,P3DY = ',4E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9039)IBUGG4,ISUBG4,IERRG4,NP
 9039   FORMAT('IBUGG4,ISUBG4,IERRG4,NP = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPQCC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1ICONT,IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--GENERATE ONE OF THE FOLLOWING Q (= QUESENBERRY)
C              CONTROL CHARTS--
C              1) Q MEAN
C              2) Q RANGE
C              3) Q STANDARD DEVIATION
C              4) Q CUSUM
C              5) Q P
C              6) Q PN
C              7) Q C
C              8) Q U
C     REFERENCE--QUESENBERRY, CHARLES P.  SPC Q CHARTS FOR START-UP
C                PROCESSES AND SHORT OR LONG RUNS.
C                JOURNAL OF QUALITY TECNOLOGY, JULY 1991,
C                PAGES 213-224.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--93/12
C     ORIGINAL VERSION--DECEMBER  1993.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IAND1
      CHARACTER*4 IAND2
      CHARACTER*4 ICONT
      CHARACTER*4 IBUGG2
      CHARACTER*4 IBUGG3
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 ICASEQ
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 IERRO2
      CHARACTER*4 IHLEFT
      CHARACTER*4 IHLEF2
      CHARACTER*4 IHHOR
      CHARACTER*4 IHHOR2
C
      CHARACTER*4 IHEXT
      CHARACTER*4 IHEXT2
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
C
      DIMENSION Y1(MAXOBV)
      DIMENSION Y2(MAXOBV)
      DIMENSION X1(MAXOBV)
C
      DIMENSION XIDTEM(MAXOBV)
      DIMENSION TEMP(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(IGARB1),X1(1))
      EQUIVALENCE (GARBAG(IGARB2),Y1(1))
      EQUIVALENCE (GARBAG(IGARB3),Y2(1))
      EQUIVALENCE (GARBAG(IGARB4),XIDTEM(1))
      EQUIVALENCE (GARBAG(IGARB5),TEMP(1))
      EQUIVALENCE (GARBAG(IGARB6),TEMP2(1))
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IERROR='NO'
      ISUBN1='DPQC'
      ISUBN2='C   '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
      MAXV2=2
      MINN2=2
C
      ICOLH=0
C
C               **************************************
C               **  TREAT THE Q CONTROL CHART CASE  **
C               **************************************
C
      IF(IBUGG2.EQ.'OFF'.AND.ISUBRO.NE.'PQCC')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPQCC--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICASPL,IAND1,IAND2
   52 FORMAT('ICASPL,IAND1,IAND2 = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)ICONT,IBUGG2,IBUGG3,IBUGQ
   53 FORMAT('ICONT,IBUGG2,IBUGG3,IBUGQ = ',A4,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)ISUBRO
   54 FORMAT('ISUBRO = ',A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               ***************************
C               **  STEP 1--             **
C               **  EXTRACT THE COMMAND  **
C               ***************************
C
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PQCC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICOM=IHARG(1)
      ICOM2=IHARG2(1)
      ISHIFT=1
      CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1IBUGG2,IERROR)
C
C               ***************************************
C               **  STEP 1.1--                       **
C               **  SEARCH FOR Q MEAN CONTROL CHART  **
C               ***************************************
C
      ICASPL='MECC'
C
      IF(NUMARG.GE.3.AND.
     1ICOM.EQ.'X'.AND.IHARG(1).EQ.'BAR'.AND.IHARG(2).EQ.'CONT'.AND.
     1IHARG(3).EQ.'CHAR')GOTO113
      IF(NUMARG.GE.2.AND.
     1ICOM.EQ.'XBAR'.AND.IHARG(1).EQ.'CONT'.AND.IHARG(2).EQ.'CHAR')
     1GOTO112
      IF(NUMARG.GE.2.AND.
     1ICOM.EQ.'MEAN'.AND.IHARG(1).EQ.'CONT'.AND.IHARG(2).EQ.'CHAR')
     1GOTO112
      IF(NUMARG.GE.2.AND.
     1ICOM.EQ.'AVER'.AND.IHARG(1).EQ.'CONT'.AND.IHARG(2).EQ.'CHAR')
     1GOTO112
      IF(NUMARG.GE.1.AND.
     1ICOM.EQ.'CONT'.AND.IHARG(1).EQ.'CHAR')
     1GOTO111
      IF(NUMARG.GE.1.AND.
     1ICOM.EQ.'MEAN'.AND.IHARG(1).EQ.'CHAR')
     1GOTO111
      IF(NUMARG.GE.1.AND.
     1ICOM.EQ.'XBAR'.AND.IHARG(1).EQ.'CHAR')
     1GOTO111
      IF(NUMARG.GE.1.AND.
     1ICOM.EQ.'AVER'.AND.IHARG(1).EQ.'CHAR')
     1GOTO111
C
C               ************************************************
C               **  STEP 1.2--                                **
C               **  SEARCH FOR Q STANDARD DEV. CONTROL CHART  **
C               ************************************************
C
      ICASPL='SDCC'
C
      IF(NUMARG.GE.3.AND.
     1ICOM.EQ.'STAN'.AND.IHARG(1).EQ.'DEVI'.AND.IHARG(2).EQ.'CONT'.AND.
     1IHARG(3).EQ.'CHAR')GOTO113
      IF(NUMARG.GE.2.AND.
     1ICOM.EQ.'SD'.AND.IHARG(1).EQ.'CONT'.AND.IHARG(2).EQ.'CHAR')
     1GOTO112
      IF(NUMARG.GE.2.AND.
     1ICOM.EQ.'S'.AND.IHARG(1).EQ.'CONT'.AND.IHARG(2).EQ.'CHAR')
     1GOTO112
      IF(NUMARG.GE.1.AND.
     1ICOM.EQ.'SD'.AND.IHARG(1).EQ.'CHAR')
     1GOTO111
      IF(NUMARG.GE.1.AND.
     1ICOM.EQ.'S'.AND.IHARG(1).EQ.'CHAR')
     1GOTO111
C
C               ****************************************
C               **  STEP 1.3--                        **
C               **  SEARCH FOR Q RANGE CONTROL CHART  **
C               ****************************************
C
      ICASPL='RACC'
C
      IF(NUMARG.GE.2.AND.
     1ICOM.EQ.'RANG'.AND.IHARG(1).EQ.'CONT'.AND.IHARG(2).EQ.'CHAR')
     1GOTO112
      IF(NUMARG.GE.2.AND.
     1ICOM.EQ.'R'.AND.IHARG(1).EQ.'CONT'.AND.IHARG(2).EQ.'CHAR')
     1GOTO112
      IF(NUMARG.GE.1.AND.
     1ICOM.EQ.'RANG'.AND.IHARG(1).EQ.'CHAR')
     1GOTO111
      IF(NUMARG.GE.1.AND.
     1ICOM.EQ.'R'.AND.IHARG(1).EQ.'CHAR')
     1GOTO111
C
C               ****************************************
C               **  STEP 1.4--                        **
C               **  SEARCH FOR Q CUSUM CONTROL CHART  **
C               ****************************************
C
      ICASPL='CUCC'
C
      IF(NUMARG.GE.3.AND.
     1ICOM.EQ.'CUMU'.AND.IHARG(1).EQ.'SUM'.AND.IHARG(2).EQ.'CONT'.AND.
     1IHARG(3).EQ.'CHAR')GOTO113
      IF(NUMARG.GE.2.AND.
     1ICOM.EQ.'CUSU'.AND.IHARG(1).EQ.'CONT'.AND.IHARG(2).EQ.'CHAR')
     1GOTO112
C
C               ****************************************
C               **  STEP 1.5--                        **
C               **  SEARCH FOR Q P CONTROL CHART      **
C               ****************************************
C
      ICASPL='PCC'
C
      IF(NUMARG.GE.2.AND.
     1ICOM.EQ.'P'.AND.IHARG(1).EQ.'CONT'.AND.IHARG(2).EQ.'CHAR')
     1GOTO112
      IF(NUMARG.GE.1.AND.
     1ICOM.EQ.'P'.AND.IHARG(1).EQ.'CHAR')
     1GOTO111
C
C               ****************************************
C               **  STEP 1.6--                        **
C               **  SEARCH FOR Q PN CONTROL CHART     **
C               ****************************************
C
      ICASPL='PNCC'
C
      IF(NUMARG.GE.2.AND.
     1ICOM.EQ.'PN'.AND.IHARG(1).EQ.'CONT'.AND.IHARG(2).EQ.'CHAR')
     1GOTO112
      IF(NUMARG.GE.1.AND.
     1ICOM.EQ.'PN'.AND.IHARG(1).EQ.'CHAR')
     1GOTO111
      IF(NUMARG.GE.2.AND.
     1ICOM.EQ.'NP'.AND.IHARG(1).EQ.'CONT'.AND.IHARG(2).EQ.'CHAR')
     1GOTO112
      IF(NUMARG.GE.1.AND.
     1ICOM.EQ.'NP'.AND.IHARG(1).EQ.'CHAR')
     1GOTO111
C
C               ****************************************
C               **  STEP 1.7--                        **
C               **  SEARCH FOR Q C CONTROL CHART      **
C               ****************************************
C
      ICASPL='CCC'
C
      IF(NUMARG.GE.2.AND.
     1ICOM.EQ.'C'.AND.IHARG(1).EQ.'CONT'.AND.IHARG(2).EQ.'CHAR')
     1GOTO112
      IF(NUMARG.GE.1.AND.
     1ICOM.EQ.'C'.AND.IHARG(1).EQ.'CHAR')
     1GOTO111
C
C               ****************************************
C               **  STEP 1.8--                        **
C               **  SEARCH FOR Q U CONTROL CHART      **
C               ****************************************
C
      ICASPL='UCC'
C
      IF(NUMARG.GE.2.AND.
     1ICOM.EQ.'U'.AND.IHARG(1).EQ.'CONT'.AND.IHARG(2).EQ.'CHAR')
     1GOTO112
      IF(NUMARG.GE.1.AND.
     1ICOM.EQ.'U'.AND.IHARG(1).EQ.'CHAR')
     1GOTO111
C
      ICASPL='    '
C
      IFOUND='NO'
      GOTO9000
C
  111 CONTINUE
      ILASTC=1
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      GOTO180
C
  112 CONTINUE
      ILASTC=2
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      GOTO180
C
  113 CONTINUE
      ILASTC=3
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      GOTO180
C
  180 CONTINUE
      IFOUND='YES'
      GOTO190
C
  190 CONTINUE
C
C               ***********************************************************
C               **  STEP 1--                                             **
C               **  CHECK FOR THE PROPER NUMBER OF INPUT ARGUMENTS.      **
C               ***********************************************************
C
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PQCC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      MINNA=1
      MAXNA=100
      CALL CHECKA(NUMARG,MINNA,MAXNA,IANS,IWIDTH,ISUBN1,ISUBN2,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
C               ********************************************
C               **  STEP 2--                              **
C               **  CHECK THE VALIDITY OF ARGUMENT 1      **
C               **  (THIS WILL BE THE RESPONSE VARIABLE)  **
C               ********************************************
C
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PQCC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IHLEFT=IHARG(1)
      IHLEF2=IHARG2(1)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IHLEFT,IHLEF2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      ICOLL=IVALUE(ILOCV)
      NLEFT=IN(ILOCV)
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PQCC')THEN
         WRITE(ICOUT,211)IHLEFT,ICOLL,NLEFT
  211    FORMAT('IHLEFT,ICOLL,NLEFT = ',A4,I8,I8)
         CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***************************************************************
C               **  STEP 3--                                                 **
C               **  CHECK THAT THE INPUT NUMBER OF OBSERVATIONS (NLEFT)      **
C               **  FOR THE RESPONSE VARIABLE IS 2 OR LARGER.                **
C               ***************************************************************
C
      ISTEPN='3'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PQCC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NLEFT.GE.MINN2)GOTO390
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,311)
  311 FORMAT('***** ERROR IN DPQCC--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,312)
  312 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS')
      CALL DPWRST('XXX','BUG ')
      IF(ICASPL.EQ.'MECC')WRITE(ICOUT,321)
  321 FORMAT('      (FOR WHICH A Q MEAN CONTROL CHART ')
      IF(ICASPL.EQ.'MECC')CALL DPWRST('XXX','BUG ')
      IF(ICASPL.EQ.'SDCC')WRITE(ICOUT,322)
  322 FORMAT('      (FOR WHICH A Q STANDARD DEVIATION CONTROL CHART ')
      IF(ICASPL.EQ.'SDCC')CALL DPWRST('XXX','BUG ')
      IF(ICASPL.EQ.'RACC')WRITE(ICOUT,323)
  323 FORMAT('      (FOR WHICH A Q RANGE CONTROL CHART ')
      IF(ICASPL.EQ.'RACC')CALL DPWRST('XXX','BUG ')
      IF(ICASPL.EQ.'CUCC')WRITE(ICOUT,324)
  324 FORMAT('      (FOR WHICH A Q CUSUM CONTROL CHART ')
      IF(ICASPL.EQ.'CUCC')CALL DPWRST('XXX','BUG ')
      IF(ICASPL.EQ.'PCC')WRITE(ICOUT,325)
  325 FORMAT('      (FOR WHICH A Q P CONTROL CHART ')
      IF(ICASPL.EQ.'PCC')CALL DPWRST('XXX','BUG ')
      IF(ICASPL.EQ.'PNCC')WRITE(ICOUT,326)
  326 FORMAT('      (FOR WHICH A Q NP CONTROL CHART ')
      IF(ICASPL.EQ.'PNCC')CALL DPWRST('XXX','BUG ')
      IF(ICASPL.EQ.'CCC')WRITE(ICOUT,327)
  327 FORMAT('      (FOR WHICH A Q C CONTROL CHART ')
      IF(ICASPL.EQ.'CCC')CALL DPWRST('XXX','BUG ')
      IF(ICASPL.EQ.'UCC')WRITE(ICOUT,328)
  328 FORMAT('      (FOR WHICH A Q U CONTROL CHART ')
      IF(ICASPL.EQ.'UCC')CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,334)
  334 FORMAT('      WAS TO HAVE BEEN FORMED)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,335)MINN2
  335 FORMAT('      MUST BE ',I8,' OR LARGER;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,336)
  336 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,337)
  337 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,338)(IANS(I),I=1,IWIDTH)
  338 FORMAT('      ',80A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
  390 CONTINUE
C
C               *****************************************
C               **  STEP 4--                           **
C               **  CHECK TO SEE THE TYPE SUBCASE      **
C               **  (BASED ON THE QUALIFIER)--         **
C               **    1) UNQUALIFIED (THAT IS, FULL);  **
C               **    2) SUBSET/EXCEPT; OR             **
C               **    3) FOR.                          **
C               *****************************************
C
      ISTEPN='4'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PQCC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASEQ='FULL'
      ILOCQ=NUMARG+1
      IF(NUMARG.LT.1)GOTO480
      DO400J=1,NUMARG
      J1=J
      IF(IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ') GOTO410
      IF(IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  ') GOTO410
      IF(IHARG(J).EQ.'FOR '.AND.IHARG2(J).EQ.'    ') GOTO420
  400 CONTINUE
      GOTO490
  410 CONTINUE
      ICASEQ='SUBS'
      ILOCQ=J1
      GOTO490
  420 CONTINUE
      ICASEQ='FOR'
      ILOCQ=J1
      GOTO490
C
  480 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,481)
  481 FORMAT('***** INTERNAL ERROR IN DPQCC')
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
  487 FORMAT('      ',80A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
  490 CONTINUE
      IF(IBUGG2.EQ.'OFF'.AND.ISUBRO.NE.'PQCC')GOTO495
      WRITE(ICOUT,491)NUMARG,ILOCQ,ICASEQ
  491 FORMAT('NUMARG,ILOCQ,ICASEQ = ',I8,I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
  495 CONTINUE
C
C               ************************************************************
C               **  STEP 5--                                              **
C               **  IF A SECOND ARGUMENT EXISTS, THEN THIS                **
C               **  INDICATES THAT THE VALUES IN THE                      **
C               **  FIRST VARIABLE ARE TO BE GROUPED                      **
C               **  BASED ON VALUES OF THE SECOND VARIABLE;               **
C               **  THAT IS, THE SECOND VARAIBLE DEFINES THE              **
C               **  GROUP NUMBERS WITHIN WHICH THE MEANS,                 **
C               **  STANDARD DEVIATIONS, RANGES, AND                      **
C               **  CUMULATIVE SUMS ARE TO BE COMPUTED.                   **
C               **  THE VALUES IN THE SECOND VARIABLE                     **
C               **  ARE THE X VALUES FOR EACH MEAN, STANDARD DEVIATION,   **
C               **  ETC.  IN THE RESULTING Q CONTROL CHART.                 **
C               **  THE VALUES IN THE SECOND VARIABLE                     **
C               **  NEED NOT HAVE BEEN PREVIOUSLY                         **
C               **  SORTED OR HAVE COMMON VALUES ADJACENT.                **
C               **  IF WE HAVE THE 2-VARIABLE CASE,                       **
C               **  CHECK THE VALIDITY OF THE SECOND (X) VARIABLE.        **
C               ************************************************************
C
      ISTEPN='5'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PQCC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMV2=ILOCQ-1
      IF(NUMV2.EQ.1)GOTO599
      IF(NUMV2.EQ.2)GOTO530
      IF(NUMV2.EQ.3)GOTO540
      GOTO510
C
  510 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,511)
  511 FORMAT('***** ERROR IN DPQCC--')
      CALL DPWRST('XXX','BUG ')
      IF(ICASPL.EQ.'MECC')WRITE(ICOUT,512)
  512 FORMAT('      FOR A Q MEAN CONTROL CHART, ')
      IF(ICASPL.EQ.'MECC')CALL DPWRST('XXX','BUG ')
      IF(ICASPL.EQ.'SDCC')WRITE(ICOUT,513)
  513 FORMAT('      FOR A Q STANDARD DEVIATION CONTROL CHART, ')
      IF(ICASPL.EQ.'SDCC')CALL DPWRST('XXX','BUG ')
      IF(ICASPL.EQ.'RACC')WRITE(ICOUT,514)
  514 FORMAT('      FOR A Q RANGE CONTROL CHART, ')
      IF(ICASPL.EQ.'RACC')CALL DPWRST('XXX','BUG ')
      IF(ICASPL.EQ.'CUCC')WRITE(ICOUT,515)
  515 FORMAT('      FOR A Q CUSUM CONTROL CHART, ')
      IF(ICASPL.EQ.'CUCC')CALL DPWRST('XXX','BUG ')
      IF(ICASPL.EQ.'PCC')WRITE(ICOUT,516)
  516 FORMAT('      (FOR WHICH A Q P CONTROL CHART ')
      IF(ICASPL.EQ.'PCC')CALL DPWRST('XXX','BUG ')
      IF(ICASPL.EQ.'PNCC')WRITE(ICOUT,517)
  517 FORMAT('      (FOR WHICH A Q NP CONTROL CHART ')
      IF(ICASPL.EQ.'PNCC')CALL DPWRST('XXX','BUG ')
      IF(ICASPL.EQ.'CCC')WRITE(ICOUT,518)
  518 FORMAT('      (FOR WHICH A Q C CONTROL CHART ')
      IF(ICASPL.EQ.'CCC')CALL DPWRST('XXX','BUG ')
      IF(ICASPL.EQ.'UCC')WRITE(ICOUT,519)
  519 FORMAT('      (FOR WHICH A Q U CONTROL CHART ')
      IF(ICASPL.EQ.'UCC')CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,523)
  523 FORMAT('      THE NUMBER OF VARIABLES ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,524)
  524 FORMAT('      MUST BE EITHER 1 OR 2  ;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,525)
  525 FORMAT('      SUCH WAS NOT THE CASE HERE;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,526)
  526 FORMAT('      THE SPECIFIED NUMBER')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,527)NUMV2
  527 FORMAT('      OF VARIABLES WAS ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,528)
  528 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,529)(IANS(I),I=1,IWIDTH)
  529 FORMAT('      ',80A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
  530 CONTINUE
      IHHOR=IHARG(2)
      IHHOR2=IHARG2(2)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IHHOR,IHHOR2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      ICOLH=IVALUE(ILOCV)
      NHOR=IN(ILOCV)
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PQCC')THEN
         WRITE(ICOUT,531)IHHOR,ICOLH,NHOR
  531    FORMAT('IHHOR,ICOLH,NHOR   = ',A4,I8,I8)
         CALL DPWRST('XXX','BUG ')
      ENDIF
      IF(NHOR.NE.NLEFT)GOTO570
      GOTO599
C
  540 CONTINUE
C     IHEXT AS IN "EXTRA"
      IHEXT=IHARG(2)
      IHEXT2=IHARG2(2)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IHEXT,IHEXT2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      ICOLE=IVALUE(ILOCV)
      NEXT=IN(ILOCV)
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PQCC')THEN
         WRITE(ICOUT,541)IHEXT,ICOLE,NEXT
  541    FORMAT('IHEXT,ICOLE,NEXT   = ',A4,I8,I8)
         CALL DPWRST('XXX','BUG ')
      ENDIF
      IF(NEXT.NE.NLEFT)GOTO570
C
      IHHOR=IHARG(3)
      IHHOR2=IHARG2(3)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IHHOR,IHHOR2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      ICOLH=IVALUE(ILOCV)
      NHOR=IN(ILOCV)
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PQCC')THEN
         WRITE(ICOUT,542)IHHOR,ICOLH,NHOR
  542    FORMAT('IHHOR,ICOLH,NHOR   = ',A4,I8,I8)
         CALL DPWRST('XXX','BUG ')
      ENDIF
      IF(NHOR.NE.NLEFT)GOTO570
      GOTO599
C
  570 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,571)
  571 FORMAT('***** ERROR IN DPQCC--')
      CALL DPWRST('XXX','BUG ')
      IF(ICASPL.EQ.'MECC')WRITE(ICOUT,572)
  572 FORMAT('      FOR A Q MEAN CONTROL CHART, ')
      IF(ICASPL.EQ.'MECC')CALL DPWRST('XXX','BUG ')
      IF(ICASPL.EQ.'SDCC')WRITE(ICOUT,573)
  573 FORMAT('      FOR A Q STANDARD DEVIATION CONTROL CHART,')
      IF(ICASPL.EQ.'SDCC')CALL DPWRST('XXX','BUG ')
      IF(ICASPL.EQ.'RACC')WRITE(ICOUT,574)
  574 FORMAT('      FOR A Q RANGE CONTROL CHART, ')
      IF(ICASPL.EQ.'RACC')CALL DPWRST('XXX','BUG ')
      IF(ICASPL.EQ.'CUCC')WRITE(ICOUT,575)
  575 FORMAT('      FOR A Q CUSUM CONTROL CHART,')
      IF(ICASPL.EQ.'CUCC')CALL DPWRST('XXX','BUG ')
      IF(ICASPL.EQ.'PCC')WRITE(ICOUT,576)
  576 FORMAT('      (FOR WHICH A P CONTROL CHART ')
      IF(ICASPL.EQ.'PCC')CALL DPWRST('XXX','BUG ')
      IF(ICASPL.EQ.'PNCC')WRITE(ICOUT,577)
  577 FORMAT('      (FOR WHICH A NP CONTROL CHART ')
      IF(ICASPL.EQ.'PNCC')CALL DPWRST('XXX','BUG ')
      IF(ICASPL.EQ.'CCC')WRITE(ICOUT,578)
  578 FORMAT('      (FOR WHICH A Q C CONTROL CHART ')
      IF(ICASPL.EQ.'CCC')CALL DPWRST('XXX','BUG ')
      IF(ICASPL.EQ.'UCC')WRITE(ICOUT,579)
  579 FORMAT('      (FOR WHICH A Q U CONTROL CHART ')
      IF(ICASPL.EQ.'UCC')CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,584)
  584 FORMAT('      WHEN HAVE 2 (OR 3) VARAIBLES SPECIFIED, ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,585)
  585 FORMAT('      THE NUMBER OF ELEMENTS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,586)
  586 FORMAT('      IN THE 2 (OR 3) VARIABLES ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,587)
  587 FORMAT('      MUST BE THE SAME; ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,588)
  588 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,589)
  589 FORMAT('      THE FIRST  VARIABLE  (RESPONSE VALUES)--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,590)IHLEFT,NLEFT
  590 FORMAT('                  ',A4,'  HAS ',I8,' ELEMENTS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,591)
  591 FORMAT('      THE 2ND VARIABLE--')
      CALL DPWRST('XXX','BUG ')
      IF(NUMV2.EQ.3)WRITE(ICOUT,592)IHEXT,NEXT
      IF(NUMV2.EQ.3)CALL DPWRST('XXX','BUG ')
      IF(NUMV2.EQ.2)WRITE(ICOUT,592)IHHOR,NHOR
  592 FORMAT('                  ',A4,'  HAS ',I8,' ELEMENTS')
      IF(NUMV2.EQ.2)CALL DPWRST('XXX','BUG ')
      IF(NUMV2.EQ.3)WRITE(ICOUT,593)
  593 FORMAT('      THE 3ND VARIABLE  (HORIZ. AXIS VALUES)--')
      IF(NUMV2.EQ.3)CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,594)IHHOR,NHOR
  594 FORMAT('                  ',A4,'  HAS ',I8,' ELEMENTS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,595)
  595 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,596)(IANS(I),I=1,IWIDTH)
  596 FORMAT('      ',80A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
  599 CONTINUE
C
C               *************************************************
C               **  STEP 6--                                   **
C               **  BRANCH TO THE APPROPRIATE SUBCASE;         **
C               **  (BASED ON THE QUALIFIER)                   **
C               **  THEN FORM THE RESPONSE VARIABLE            **
C               **  AND THE SECOND VARIABLE (IF EXISTENT)      **
C               *************************************************
C
      ISTEPN='6'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PQCC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASEQ.EQ.'FULL')GOTO610
      IF(ICASEQ.EQ.'SUBS')GOTO620
      IF(ICASEQ.EQ.'FOR')GOTO630
C
  610 CONTINUE
      DO615I=1,NLEFT
      ISUB(I)=1
  615 CONTINUE
      NQ=NLEFT
      GOTO650
C
  620 CONTINUE
      NIOLD=NLEFT
      CALL DPSUBS(NIOLD,ILOCS,NS,IBUGQ,IERROR)
      NQ=NIOLD
      GOTO650
C
  630 CONTINUE
      NIOLD=NLEFT
      CALL DPFOR(NIOLD,NFOR,IROW1,IROWN,
     1NLOCAL,ILOCS,NS,IBUGQ,IERROR)
      NQ=NFOR
      GOTO650
C
  650 CONTINUE
      J=0
      IMAX=NLEFT
      IF(NQ.LT.NLEFT)IMAX=NQ
      DO660I=1,IMAX
      IF(ISUB(I).EQ.0)GOTO660
      J=J+1
C
      IJ=MAXN*(ICOLL-1)+I
      IF(ICOLL.LE.MAXCOL)Y1(J)=V(IJ)
      IF(ICOLL.EQ.MAXCP1)Y1(J)=PRED(I)
      IF(ICOLL.EQ.MAXCP2)Y1(J)=RES(I)
      IF(ICOLL.EQ.MAXCP3)Y1(J)=YPLOT(I)
      IF(ICOLL.EQ.MAXCP4)Y1(J)=XPLOT(I)
      IF(ICOLL.EQ.MAXCP5)Y1(J)=X2PLOT(I)
      IF(ICOLL.EQ.MAXCP6)Y1(J)=TAGPLO(I)
      IF(NUMV2.LE.1)GOTO660
C
      IF(NUMV2.EQ.2)GOTO652
      GOTO653
C
  652 CONTINUE
      IJ=MAXN*(ICOLH-1)+I
      IF(ICOLH.LE.MAXCOL)X1(J)=V(IJ)
      IF(ICOLH.EQ.MAXCP1)X1(J)=PRED(I)
      IF(ICOLH.EQ.MAXCP2)X1(J)=RES(I)
      IF(ICOLH.EQ.MAXCP3)X1(J)=YPLOT(I)
      IF(ICOLH.EQ.MAXCP4)X1(J)=XPLOT(I)
      IF(ICOLH.EQ.MAXCP5)X1(J)=X2PLOT(I)
      IF(ICOLH.EQ.MAXCP6)X1(J)=TAGPLO(I)
      GOTO660
C
  653 CONTINUE
      IJ=MAXN*(ICOLE-1)+I
      IF(ICOLE.LE.MAXCOL)Y2(J)=V(IJ)
      IF(ICOLE.EQ.MAXCP1)Y2(J)=PRED(I)
      IF(ICOLE.EQ.MAXCP2)Y2(J)=RES(I)
      IF(ICOLE.EQ.MAXCP3)Y2(J)=YPLOT(I)
      IF(ICOLE.EQ.MAXCP4)Y2(J)=XPLOT(I)
      IF(ICOLE.EQ.MAXCP5)Y2(J)=X2PLOT(I)
      IF(ICOLE.EQ.MAXCP6)Y2(J)=TAGPLO(I)
C
      IJ=MAXN*(ICOLH-1)+I
      IF(ICOLH.LE.MAXCOL)X1(J)=V(IJ)
      IF(ICOLH.EQ.MAXCP1)X1(J)=PRED(I)
      IF(ICOLH.EQ.MAXCP2)X1(J)=RES(I)
      IF(ICOLH.EQ.MAXCP3)X1(J)=YPLOT(I)
      IF(ICOLH.EQ.MAXCP4)X1(J)=XPLOT(I)
      IF(ICOLH.EQ.MAXCP5)X1(J)=X2PLOT(I)
      IF(ICOLH.EQ.MAXCP6)X1(J)=TAGPLO(I)
      GOTO660
C
  660 CONTINUE
      NLOCAL=J
C
C               ****************************************************************
C               **  STEP 8--                                                  **
C               **  DETERMINE IF THE ANALYST                                  **
C               **  HAS SPECIFIED
C               **      LSL (LOWER SPEC LIMIT)
C               **      USL (UPPER SPEC LIMIT)
C               **      USLCOST (UPPER SPEC LIMIT COST)
C               **      TARGET
C               **  FOR THE Q CONTROL CHART ANALYSIS.                           **
C               ****************************************************************
C
      ISTEPN='8'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PQCC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CCLSL=CPUMIN
      IH='LSL '
      IH2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
      IF(IERRO2.EQ.'NO')CCLSL=VALUE(ILOCP)
C
      CCUSL=CPUMIN
      IH='USL '
      IH2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
      IF(IERRO2.EQ.'NO')CCUSL=VALUE(ILOCP)
C
      CCTARG=CPUMIN
      IH='TARG'
      IH2='ET  '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
      IF(IERRO2.EQ.'NO')CCTARG=VALUE(ILOCP)
C
C               *************************************************************
C               **  STEP 9--                                               **
C               **  COMPUTE THE APPROPRIATE Q CONTROL CHART STATISTIC--      **
C               **  MEAN, STANDARD DEVIATION, RANGE, CUSUM,                **
C               **  P, NP, C, U.                                           **
C               **  COMPUTE CONFIDENCE LINES.                              **
C               **  FORM THE VERTICAL AND HORIZONTAL AXIS                  **
C               **  VALUES Y(.) AND X(.) FOR THE PLOT.                     **
C               **  DEFINE THE VECTOR D(.) TO 1'S, 2'S, AND 3'S            **
C               **  FOR THE PLOTTED VALUE, THE LOWER CONFIDENCE LINE,      **
C               **  AND THE UPPER CONFIDENCE LINE.                         **
C               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).          **
C               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).          **
C               *************************************************************
C
      ISTEPN='8'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PQCC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPQCC2(Y1,Y2,X1,NLOCAL,NUMV2,ICASPL,ISIZE,ICONT,
     1            XIDTEM,TEMP,CCLSL,CCUSL,CCTARG,
     1            Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.NE.'PQCC')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPQCC--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR,ISIZE
 9012   FORMAT('IFOUND,IERROR,ISIZE = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9013   FORMAT('PNLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',
     1         3I8,3(2X,A4))
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
      SUBROUTINE DPQCC2(Y,YN,X,N,NUMV2,ICASPL,ISIZE,ICONT,
     1                  XIDTEM,TEMP,CCLSL,CCUSL,CCTARG,
     1                  Y2,X2,D2,N2,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
C              THAT WILL DEFINE A Q (= QUESENBERRY) CONTROL CHART
C              OF THE FOLLOWING TYPES--
C                 1) Q MEAN CONTROL CHART    Y X
C                 2) Q STANDARD DEVIATION CONTROL CHART    Y X
C                 3) Q RANGE CONTROL CHART    Y X
C                 4) Q CUSUM CONTROL CHART    Y X
C                 5) Q P CONTROL CHART    NUMDEF NUMTOT X
C                 6) Q PN CONTROL CHART    NUMDEF NUMTOT X
C                 7) Q U CONTROL CHART    NUMDEF SIZE X
C                 8) Q P CONTROL CHART    NUMDEF SIZE X
C     NOTE--USE P AND PN CHARTS IF KNOW HOW MANY ITEMS HAVE DEFECTS
C         --USE U AND C CHARTS IF KNOW HOW MANY DEFECTS
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     REFERENCE--QUESENBERRY, CHARLES P.  SPC Q CHARTS FOR START-UP
C                PROCESSES AND SHORT OR LONG RUNS.
C                JOURNAL OF QUALITY TECNOLOGY, JULY 1991,
C                PAGES 213-224.
C     REFERENCE--ASTM MANUAL STP-15D, PAGES 78-84, 100-105
C     REFERENCE--ISHIKAWA, GUIDE TO QUALITY CONTROL
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--93/12
C     ORIGINAL VERSION--DECEMBER  1993.
C     UPDATED         --OCTOBER   2006. CALL LIST TO TCDF
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 ICONT
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION YN(*)
      DIMENSION X(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
C
      DIMENSION XIDTEM(*)
      DIMENSION TEMP(*)
C
CCCCC DIMENSION A3(30)
      DIMENSION C4(30)
      DIMENSION B3(30)
      DIMENSION B4(30)
      DIMENSION D22(30)
      DIMENSION D3(30)
      DIMENSION D4(30)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----DATA STATEMENTS-------------------------------------------------
C
CCCCC DATA(A3(I),I=    1,   25)
CCCCC1/9.999,2.659,1.954,1.628,1.427,
CCCCC1 1.287,1.182,1.099,1.032,0.975,
CCCCC1 0.927,0.886,0.850,0.817,0.789,
CCCCC1 0.763,0.739,0.718,0.698,0.680,
CCCCC1 0.663,0.647,0.633,0.619,0.606/
      DATA(C4(I),I=    1,   25)
     1/9.9999,0.7979,0.8862,0.9213,0.9400,
     1 0.9515,0.9594,0.9650,0.9693,0.9727,
     1 0.9754,0.9776,0.9794,0.9810,0.9823,
     1 0.9835,0.9845,0.9854,0.9862,0.9869,
     1 0.9876,0.9882,0.9887,0.9892,0.9896/
      DATA(B3(I),I=    1,   25)
     1/0.000,0.000,0.000,0.000,0.000,0.030,0.118,0.185,0.239,0.284,
     1 0.321,0.354,0.382,0.406,0.428,0.448,0.466,0.482,0.497,0.510,
     1 0.523,0.534,0.545,0.555,0.565/
      DATA(B4(I),I=    1,   25)
     1/9.999,3.267,2.568,2.266,2.089,1.970,1.882,1.815,1.761,1.716,
     1 1.679,1.646,1.618,1.594,1.572,1.552,1.534,1.518,1.503,1.490,
     1 1.477,1.466,1.455,1.445,1.435/
      DATA(D22(I),I=    1,   25)
     1/9.999,3.686,4.358,4.698,4.918,5.078,5.203,5.307,5.394,5.469,
     1 5.534,5.592,5.646,5.693,5.737,5.779,5.817,5.854,5.888,5.922,
     1 5.950,5.979,6.006,6.031,6.058/
      DATA(D3(I),I=    1,   25)
     1/0.000,0.000,0.000,0.000,0.000,0.000,0.076,0.136,0.184,0.223,
     1 0.256,0.284,0.308,0.329,0.348,0.364,0.379,0.392,0.404,0.414,
     1 0.425,0.434,0.443,0.452,0.459/
      DATA(D4(I),I=    1,   25)
     1/9.999,3.267,2.575,2.282,2.115,2.004,1.924,1.864,1.816,1.777,
     1 1.744,1.716,1.692,1.671,1.652,1.636,1.621,1.608,1.596,1.586,
     1 1.575,1.566,1.557,1.548,1.541/
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPQC'
      ISUBN2='C2  '
C
      I2=0
      ISIZE2=0
C
      AN=0.0
      XBARG=0.0
      SDG=0.0
      RANGEG=0.0
      YUPPER=0.0
      YLOWER=0.0
C
      ANUMSE=0.0
      SDI=0.0
      SIGMAE=0.0
      RANGEE=0.0
      SADJ=0.0
      RADJ=0.0
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(N.GE.1)GOTO39
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,31)
   31 FORMAT('***** ERROR IN DPQCC2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,32)
   32 FORMAT('      THE NUMBER OF OBSERVATIONS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,33)
   33 FORMAT('      MUST BE AT LEAST 1;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,34)N
   34 FORMAT('      THE ENTERED NUMBER OF OBSERVATIONS HERE = ',I6)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
   39 CONTINUE
C
      IF(N.GE.2)GOTO49
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,46)
   46 FORMAT('***** ERROR IN DPQCC2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,47)
   47 FORMAT('      THE NUMBER OF OBSERVATIONS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,48)
   48 FORMAT('      WAS EXACTLY EQUAL TO 1.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
   49 CONTINUE
C
      HOLD=Y(1)
      DO60I=1,N
      IF(Y(I).NE.HOLD)GOTO69
   60 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,61)
   61 FORMAT('***** ERROR IN DPQCC2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,62)
   62 FORMAT('      ALL RESPONSE VARIABLE ELEMENTS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,63)HOLD
   63 FORMAT('      ARE IDENTICALLY EQUAL TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
   69 CONTINUE
C
 
      IF(IBUGG3.EQ.'OFF'.AND.ISUBRO.NE.'QCC2')GOTO90
      WRITE(ICOUT,70)
   70 FORMAT('AT THE BEGINNING OF DPQCC2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,71)N,ICASPL,NUMV2,ISIZE,ICONT
   71 FORMAT('N,ICASPL,NUMV2,ISIZE,ICONT = ',I8,2X,A4,I8,I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
      DO72I=1,N
      WRITE(ICOUT,73)I,Y(I),X(I)
   73 FORMAT('I, Y(I), X(I) = ',I8,3F15.7)
      CALL DPWRST('XXX','BUG ')
   72 CONTINUE
      IF(NUMV2.LE.2)GOTO79
      DO75I=1,N
      WRITE(ICOUT,76)I,YN(I),X(I)
   76 FORMAT('I,YN(I),X(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
   75 CONTINUE
   79 CONTINUE
   90 CONTINUE
C
C               ********************************************************
C               **  STEP 1--                                          **
C               **  DETERMINE THE NUMBER OF DISTINCT VALUES           **
C               **  FOR VARIABLE 2 (THE GROUP VARIABLE).              **
C               **  IF ALL VALUES ARE DISTINCT, THEN THIS             **
C               **  IMPLIES WE HAVE THE NO REPLICATION CASE           **
C               **  WHICH IS AN ERROR CONDITION FOR A Q CONTROL CHART.  **
C               ********************************************************
C
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'QCC2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMSET=(-999)
      IF(NUMV2.EQ.1)GOTO199
      IF(NUMV2.EQ.2)GOTO150
C
  150 CONTINUE
      NUMSET=0
      DO160I=1,N
      IF(NUMSET.EQ.0)GOTO165
      DO170J=1,NUMSET
      IF(X(I).EQ.XIDTEM(J))GOTO160
  170 CONTINUE
  165 CONTINUE
      NUMSET=NUMSET+1
      XIDTEM(NUMSET)=X(I)
  160 CONTINUE
      CALL SORT(XIDTEM,NUMSET,XIDTEM)
C
      IF(NUMSET.GE.1)GOTO194
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,191)
  191 FORMAT('***** ERROR IN DPQCC2 SUBROUTINE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,192)
  192 FORMAT('      NUMBER OF SETS    NUMSET = 0 ')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
  194 CONTINUE
C
      IF(ICASPL.EQ.'PCC')GOTO199
      IF(ICASPL.EQ.'PNCC')GOTO199
      IF(ICASPL.EQ.'UCC')GOTO199
      IF(ICASPL.EQ.'CCC')GOTO199
C
      IF(NUMSET.NE.N)GOTO199
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,195)
  195 FORMAT('***** ERROR IN DPQCC2 SUBROUTINE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,196)
  196 FORMAT('      NUMBER OF SETS    NUMSET   IDENTICAL TO ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,197)
  197 FORMAT('      NUMBER OF OBSERVATIONS   N   .')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,198)NUMSET
  198 FORMAT('      NUMSET = N = ',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
  199 CONTINUE
C
      AN=N
      ANUMSE=NUMSET
C
C               *******************************************
C               **  STEP 3.0--                           **
C               **  DETERMINE STATISTICS FOR THE ENTIRE  **
C               **  DATA SET                             **
C               *******************************************
C
      ISTEPN='3.0'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'QCC2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMV2.EQ.1)GOTO1090
C
      SUMXBG=0.0
      SUMSDG=0.0
      SUMRAG=0.0
      SUMSIE=0.0
      SUMRIE=0.0
      J=0
      DO1010ISET=1,NUMSET
      J=J+1
C
      K=0
      DO1020I=1,N
      IF(X(I).EQ.XIDTEM(ISET))K=K+1
      IF(X(I).EQ.XIDTEM(ISET))TEMP(K)=Y(I)
 1020 CONTINUE
      NI=K
      ANI=NI
C
      SUM=0.0
      IF(NI.LE.0)GOTO1040
      DO1030I=1,NI
      SUM=SUM+TEMP(I)
 1030 CONTINUE
      XBARI=SUM/ANI
C
      SUM=0.0
      DO1032I=1,NI
      SUM=SUM+(TEMP(I)-XBARI)**2
 1032 CONTINUE
      DENOM=ANI-1.0
      VARI=0.0
      IF(NI.GE.2)VARI=SUM/DENOM
      SDI=0.0
      IF(VARI.GT.0.0)SDI=SQRT(VARI)
C
      XTMIN=TEMP(1)
      XTMAX=TEMP(1)
      DO1034I=1,NI
      IF(TEMP(I).LT.XTMIN)XTMIN=TEMP(I)
      IF(TEMP(I).GT.XTMAX)XTMAX=TEMP(I)
 1034 CONTINUE
      RANGEI=XTMAX-XTMIN
      GOTO1049
C
 1040 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1041)
 1041 FORMAT('***** INTERNAL ERROR IN DPQCC2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1042)
 1042 FORMAT('NI FOR SOME CLASS = 0')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1043)ISET,XIDTEM(ISET),NI
 1043 FORMAT('ISET,XIDTEM(ISET),NI = ',I8,E15.7,I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 1049 CONTINUE
C
      SUMXBG=SUMXBG+ANI*XBARI
      SUMSDG=SUMSDG+ANI*SDI
      SUMRAG=SUMRAG+ANI*RANGEI
      C4LARG=1.0
      IF(NI.LE.25)SUMSIE=SUMSIE+SDI/C4(NI)
      IF(NI.GE.26)SUMSIE=SUMSIE+SDI/C4LARG
      D22LAR=2.0*SQRT(2.0*LOG(2.0*ANI))
      IF(NI.LE.25)SUMRIE=SUMRIE+RANGEI/D22(NI)
      IF(NI.GE.26)SUMRIE=SUMRIE+RANGEI/D22LAR
C
      IF(IBUGG3.EQ.'OFF'.AND.ISUBRO.NE.'QCC2')GOTO1069
      WRITE(ICOUT,1061)ISET,NI,ANI
 1061 FORMAT('ISET,NI,ANI = ',I8,I8,E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1062)XBARI
 1062 FORMAT('XBARI = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1063)SDI,C4(NI),C4LARG,SUMSIE
 1063 FORMAT('SDI,C4(NI),C4LARG,SUMSIE = ',4E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1064)RANGEI,D22(NI),D22LAR,SUMRIE
 1064 FORMAT('RANGEI,D22(NI),D22LAR,SUMRIE = ',4E15.7)
      CALL DPWRST('XXX','BUG ')
 1069 CONTINUE
C
 1010 CONTINUE
C
      XBARG=SUMXBG/AN
      SDG=SUMSDG/AN
      RANGEG=SUMRAG/AN
      SIGMAE=SUMSIE/ANUMSE
      RANGEE=SUMRIE/ANUMSE
C
 1090 CONTINUE
C
C               **************************************************************
C               **  STEP 4--                                                **
 
C               **  IN ORDER TO DETERMINE THE PROPER PLOT COOORDINATES      **
C               **  FOR THE DESIRED PLOT,                                   **
C               **  BRANCH TO THE PROPER SUBCASE--                          **
C               **         1) Q MEAN CONTROL CHART;                           **
C               **         2) Q STANDARD DEVIATION CONTROL CHART;             **
C               **         3) Q RANGE CONTROL CHART;                          **
C               **         4) Q CUSUM CONTROL CHART;                          **
C               **         5) Q P CONTROL CHART;                              **
C               **         6) Q PN CONTROL CHART;                             **
C               **         7) Q C CONTROL CHART;                              **
C               **         8) Q U CONTROL CHART;                              **
C               **************************************************************
C
      ISTEPN='4'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'QCC2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASPL.EQ.'MECC')GOTO1100
      IF(ICASPL.EQ.'SDCC')GOTO1200
      IF(ICASPL.EQ.'RACC')GOTO1300
      IF(ICASPL.EQ.'CUCC')GOTO1400
      IF(ICASPL.EQ.'PCC')GOTO1500
      IF(ICASPL.EQ.'PNCC')GOTO1600
      IF(ICASPL.EQ.'UCC')GOTO1700
      IF(ICASPL.EQ.'CCC')GOTO1800
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1051)
 1051 FORMAT('***** INTERNAL ERROR IN DPQCC2')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1052)
 1052 FORMAT('      AT BRANCH POINT 261--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1053)
 1053 FORMAT('      ICASPL NOT EQUAL ONE OF THE ALLOWABLE 8--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1054)
 1054 FORMAT('      MECC, SDCC, RACC, CSCC, PCC, PNCC, UCC, CCC.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1056)ICASPL
 1056 FORMAT('      ICASPL = ',A4)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
C               *******************************************
C               **  STEP 5.1--                           **
C               **  TREAT THE Q MEAN CONTROL CHART CASE  **
C               *******************************************
C
 1100 CONTINUE
C
      ISTEPN='5.1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'QCC2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      J=0
      DO1110K=3,N
         KM1=K-1
         AKM1=KM1
         KM2=K-2
C
         SUM=0.0
         DO1120I=1,KM1
            SUM=SUM+Y(I)
 1120    CONTINUE
         XBAKM1=SUM/AKM1
C
         SUM=0.0
         DO1130I=1,KM1
            SUM=SUM+(Y(I)-XBAKM1)**2
 1130    CONTINUE
         SKM1=SQRT(SUM/(AKM1-1.0))
C
         ANUM=Y(K)-XBAKM1
         ADENOM=SKM1*SQRT((1.0/AKM1)+1.0)
         RATIO=ANUM/ADENOM
CCCCC    CALL TCDF(RATIO,KM2,CDF)
         CALL TCDF(RATIO,REAL(KM2),CDF)
         CALL NORPPF(CDF,PPF)
         J=J+1
         Y2(J)=PPF
         X2(J)=J
         D2(J)=1.0
 1110 CONTINUE
      N2=J
      NPLOTV=2
      GOTO9000
C
C               **********************************************************
C               **  STEP 5.2--                                          **
C               **  TREAT THE Q STANDARD DEVIATION CONTROL CHART CASE  **
C               **********************************************************
C
 1200 CONTINUE
C
      ISTEPN='5.2'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'QCC2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      J=0
      DO1210ISET=1,NUMSET
C
      K=0
      DO1220I=1,N
      IF(X(I).EQ.XIDTEM(ISET))K=K+1
      IF(X(I).EQ.XIDTEM(ISET))TEMP(K)=Y(I)
 1220 CONTINUE
      NI=K
      ANI=NI
C
      IF(NI.GE.1)GOTO1239
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1231)
 1231 FORMAT('***** INTERNAL ERROR IN DPQCC2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1232)
 1232 FORMAT('NI FOR SOME CLASS = 0')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1233)ISET,XIDTEM(ISET),NI
 1233 FORMAT('ISET,XIDTEM(ISET),NI = ',I8,E15.7,I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 1239 CONTINUE
C
      SUM=0.0
      DO1240I=1,NI
      SUM=SUM+TEMP(I)
 1240 CONTINUE
      XBARI=SUM/ANI
C
      IF(NI.LE.1)GOTO1210
C
      SUM=0.0
      DO1250I=1,NI
      SUM=SUM+(TEMP(I)-XBARI)**2
 1250 CONTINUE
      DENOM=ANI-1.0
      VARI=0.0
      IF(NI.GE.2)VARI=SUM/DENOM
      SDI=0.0
      IF(VARI.GT.0.0)SDI=SQRT(VARI)
C
      C4LARG=1.0
      IF(NI.LE.25)SADJ=C4(NI)*SIGMAE
      IF(NI.GE.26)SADJ=C4LARG*SIGMAE
C
      YMID=SADJ
C
      B4LARG=1.0+3.0/SQRT(2.0*(ANI-1.0))
      IF(NI.LE.25)YUPPER=B4(NI)*SADJ
      IF(NI.GE.26)YUPPER=B4LARG*SADJ
C
      B3LARG=1.0-3.0/SQRT(2.0*(ANI-1.0))
      IF(NI.LE.25)YLOWER=B3(NI)*SADJ
      IF(NI.GE.26)YLOWER=B3LARG*SADJ
C
      IF(IBUGG3.EQ.'OFF'.AND.ISUBRO.NE.'QCC2')GOTO1269
      WRITE(ICOUT,1261)ISET,NI,ANI
 1261 FORMAT('ISET,NI,ANI = ',I8,I8,E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1262)XBARI
 1262 FORMAT('XBARI = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1263)SDI,C4(NI),C4LARG,SIGMAE,SADJ
 1263 FORMAT('SDI,C4(NI),C4LARG,SIGMAE,SADJ = ',5E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1264)SADJ,YMID
 1264 FORMAT('SADJ,YMID = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1265)NI,ANI,B4(NI),B4LARG,YUPPER
 1265 FORMAT('NI,ANI,B4(NI),B4LARG,YUPPER = ',I8,4E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1266)NI,ANI,B3(NI),B3LARG,YLOWER
 1266 FORMAT('NI,ANI,B3(NI),B3LARG,YLOWER = ',I8,4E15.7)
      CALL DPWRST('XXX','BUG ')
 1269 CONTINUE
C
      J=J+1
      Y2(J)=SDI
      X2(J)=XIDTEM(ISET)
      D2(J)=1.0
C
      J=J+1
      Y2(J)=YMID
      X2(J)=XIDTEM(ISET)
      D2(J)=2.0
C
      J=J+1
      Y2(J)=YUPPER
      X2(J)=XIDTEM(ISET)
      D2(J)=3.0
C
      J=J+1
      Y2(J)=YLOWER
      X2(J)=XIDTEM(ISET)
      D2(J)=4.0
C
      IF(CCTARG.EQ.CPUMIN)GOTO1271
      J=J+1
      Y2(J)=CCTARG
      X2(J)=XIDTEM(ISET)
      D2(J)=5.0
 1271 CONTINUE
C
      IF(CCUSL.EQ.CPUMIN)GOTO1272
      J=J+1
      Y2(J)=CCUSL
      X2(J)=XIDTEM(ISET)
      D2(J)=6.0
 1272 CONTINUE
C
      IF(CCLSL.EQ.CPUMIN)GOTO1273
      J=J+1
      Y2(J)=CCLSL
      X2(J)=XIDTEM(ISET)
      D2(J)=7.0
 1273 CONTINUE
C
 1210 CONTINUE
      N2=J
      NPLOTV=3
      GOTO9000
C
C               ********************************************
C               **  STEP 5.3--                            **
C               **  TREAT THE Q RANGE CONTROL CHART CASE  **
C               ********************************************
C
 1300 CONTINUE
C
      ISTEPN='5.3'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'QCC2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      D4FACT=1.25
      D3FACT=1.0/1.25
C
      J=0
      DO1310ISET=1,NUMSET
C
      K=0
      DO1320I=1,N
      IF(X(I).EQ.XIDTEM(ISET))K=K+1
      IF(X(I).EQ.XIDTEM(ISET))TEMP(K)=Y(I)
 1320 CONTINUE
      NI=K
      ANI=NI
C
      IF(NI.GE.1)GOTO1339
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1331)
 1331 FORMAT('***** INTERNAL ERROR IN DPQCC2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1332)
 1332 FORMAT('NI FOR SOME CLASS = 0')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1333)ISET,XIDTEM(ISET),NI
 1333 FORMAT('ISET,XIDTEM(ISET),NI = ',I8,E15.7,I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 1339 CONTINUE
C
      IF(NI.LE.1)GOTO1310
C
      XTMIN=TEMP(1)
      XTMAX=TEMP(1)
      DO1340I=1,NI
      IF(TEMP(I).LT.XTMIN)XTMIN=TEMP(I)
      IF(TEMP(I).GT.XTMAX)XTMAX=TEMP(I)
 1340 CONTINUE
      RANGEI=XTMAX-XTMIN
C
      D22LAR=2.0*SQRT(2.0*LOG(2.0*ANI))
      IF(NI.LE.25)RADJ=D22(NI)*RANGEE
      IF(NI.GE.26)RADJ=D22LAR*RANGEE
C
      YMID=RADJ
C
      D4LARG=1.0+3.0*D4FACT/SQRT(2.0*(ANI-1.0))
      IF(NI.LE.25)YUPPER=D4(NI)*RADJ
      IF(NI.GE.26)YUPPER=D4LARG*RADJ
C
      D3LARG=1.0-3.0*D3FACT/SQRT(2.0*(ANI-1.0))
      IF(NI.LE.25)YLOWER=D3(NI)*RADJ
      IF(NI.GE.26)YLOWER=D3LARG*RADJ
C
      IF(IBUGG3.EQ.'OFF'.AND.ISUBRO.NE.'QCC2')GOTO1369
      WRITE(ICOUT,1361)ISET,NI,ANI
 1361 FORMAT('ISET,NI,ANI = ',I8,I8,E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1362)RANGEI
 1362 FORMAT('RANGEI = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1363)RANGEI,D22(NI),D22LAR,RANGEE,SADJ
 1363 FORMAT('RANGEI,D22(NI),D22LAR,RANGEE,SADJ = ',5E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1364)RADJ,YMID
 1364 FORMAT('RADJ,YMID = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1365)NI,ANI,D4(NI),D4LARG,YUPPER
 1365 FORMAT('NI,ANI,D4(NI),D4LARG,YUPPER = ',I8,4E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1366)NI,ANI,D3(NI),D3LARG,YLOWER
 1366 FORMAT('NI,ANI,D3(NI),D3LARG,YLOWER = ',I8,4E15.7)
      CALL DPWRST('XXX','BUG ')
 1369 CONTINUE
C
      J=J+1
      Y2(J)=RANGEI
      X2(J)=XIDTEM(ISET)
      D2(J)=1.0
C
      J=J+1
      Y2(J)=YMID
      X2(J)=XIDTEM(ISET)
      D2(J)=2.0
C
      J=J+1
      Y2(J)=YUPPER
      X2(J)=XIDTEM(ISET)
      D2(J)=3.0
C
      J=J+1
      Y2(J)=YLOWER
      X2(J)=XIDTEM(ISET)
      D2(J)=4.0
C
      IF(CCTARG.EQ.CPUMIN)GOTO1371
      J=J+1
      Y2(J)=CCTARG
      X2(J)=XIDTEM(ISET)
      D2(J)=5.0
 1371 CONTINUE
C
      IF(CCUSL.EQ.CPUMIN)GOTO1372
      J=J+1
      Y2(J)=CCUSL
      X2(J)=XIDTEM(ISET)
      D2(J)=6.0
 1372 CONTINUE
C
      IF(CCLSL.EQ.CPUMIN)GOTO1373
      J=J+1
      Y2(J)=CCLSL
      X2(J)=XIDTEM(ISET)
      D2(J)=7.0
 1373 CONTINUE
C
 1310 CONTINUE
      N2=J
      NPLOTV=3
      GOTO9000
C
C               ******************************************************
C               **  STEP 5.4--                                      **
C               **  DETERMINE PLOT COORDINATES                      **
C               **  FOR THE Q CUSUM CONTROL CHART PLOT SUBCASE.       **
C               ******************************************************
C
 1400 CONTINUE
C
      ISTEPN='3.4'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'QCC2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      WRITE(ICOUT,1405)
 1405 FORMAT('CUSUM CAPABILITY NOT YET AVAILABLE.')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
C               ********************************************************
C               **  STEP 5.5--                                        **
C               **  TREAT THE Q P CONTROL CHART CASE                   **
C               **  PROPORTION DEFECTIVE PER BATCH (SUBSAMPLE)        **
C               **  NUMBER DEFECTIVE PER BATCH / TOTAL NUMBER IN BATCH
C               **  THE INPUT IS A DUAL SERIES--
C               **     1) NUMBER OF DEFECTIVE ITEMS IN THE SUBSAMPLE
C               **     2) TOTAL NUMBER OF ITEMS IN THE SAMPLE
C               **  THE CONFIDENCE BAND IS GOTTEN BY ASSUMING BINOMIAL**
C               ********************************************************
C
 1500 CONTINUE
C
      ISTEPN='5.5'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'QCC2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      SUM1=0.0
      SUM2=0.0
      DO1510ISET=1,NUMSET
      SUM1=SUM1+Y(ISET)
      SUM2=SUM2+YN(ISET)
 1510 CONTINUE
      CTOTAL=SUM1
      ANTOT=SUM2
      PBARG=CTOTAL/ANTOT
      PRBARG=100.0*PBARG
C
      J=0
      DO1550ISET=1,NUMSET
C
      CI=Y(ISET)
      ANI=YN(ISET)
      NI=INT(ANI+0.5)
      IF(NI.LE.0)GOTO1550
C
      PI=CI/ANI
      PROPI=100.0*PI
      TAGI=XIDTEM(ISET)
C
      J=J+1
      Y2(J)=PROPI
      X2(J)=TAGI
      D2(J)=1.0
C
      J=J+1
      YMID=PRBARG
      Y2(J)=YMID
      X2(J)=TAGI
      D2(J)=2.0
C
      J=J+1
      VARPI=0.0
      IF(ANI.GT.0.0)VARPI=PBARG*(1.0-PBARG)/ANI
      SDPI=0.0
      IF(VARPI.GT.0.0)SDPI=SQRT(VARPI)
      SDPRI=100.0*SDPI
      YUPPER=YMID+3.0*SDPRI
      IF(YUPPER.GT.100.0)YUPPER=100.0
      Y2(J)=YUPPER
      X2(J)=TAGI
      D2(J)=3.0
C
      J=J+1
      YLOWER=YMID-3.0*SDPRI
      IF(YLOWER.LT.0.0)YLOWER=0.0
      Y2(J)=YLOWER
      X2(J)=TAGI
      D2(J)=4.0
C
      IF(CCTARG.EQ.CPUMIN)GOTO1571
      J=J+1
      Y2(J)=CCTARG
      X2(J)=XIDTEM(ISET)
      D2(J)=5.0
 1571 CONTINUE
C
      IF(CCUSL.EQ.CPUMIN)GOTO1572
      J=J+1
      Y2(J)=CCUSL
      X2(J)=XIDTEM(ISET)
      D2(J)=6.0
 1572 CONTINUE
C
      IF(CCLSL.EQ.CPUMIN)GOTO1573
      J=J+1
      Y2(J)=CCLSL
      X2(J)=XIDTEM(ISET)
      D2(J)=7.0
 1573 CONTINUE
C
 1550 CONTINUE
      N2=J
      NPLOTV=3
      GOTO9000
C
C               ********************************************************
C               **  STEP 5.6--                                        **
C               **  TREAT THE Q PN CONTROL CHART CASE                   **
C               **  TOTAL NUMBER DEFECTIVE IN A BATCH (SUBSAMPLE)     **
C               **  SUM UP THE NUMBER OF DEFECTIVES PER BATCH (SUBSAMPLE)
C               **  THE NUMBER WILL BE  A NON-NEGATIVE INTEGER
C               **  THE INPUT IS A DUAL SERIES--
C               **     1) NUMBER OF DEFECTIVE ITEMS IN THE SUBSAMPLE
C               **     2) TOTAL NUMBER OF ITEMS IN THE SAMPLE
C               **  THE CONFIDENCE BAND IS GOTTEN BY ASSUMING BINOMIAL**
C               **  NOTE--THE PN CHART SHOULD BE USED ONLY WHEN
C               **        THE SUBSAMPLE SIZE IS CONSTANT.
C               **        FOR VARYING SUBSAMPLE SIZE, USE THE P CHART
C               **        (ISHIKAWA, GUIDE TO QUALITY CONTROL, PAGE 77)
C               ********************************************************
C
 1600 CONTINUE
C
      ISTEPN='5.6'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'QCC2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      SUM1=0.0
      SUM2=0.0
      ANUMSE=NUMSET
      DO1610ISET=1,NUMSET
      SUM1=SUM1+Y(ISET)
      SUM2=SUM2+YN(ISET)
 1610 CONTINUE
      CTOTAL=SUM1
      ANTOT=SUM2
      PBARG=CTOTAL/ANTOT
      ANBARG=ANTOT/ANUMSE
      CBARG=PBARG*ANBARG
C
      J=0
      DO1650ISET=1,NUMSET
C
      CI=Y(ISET)
      ANI=YN(ISET)
      NI=INT(ANI+0.5)
      IF(NI.LE.0)GOTO1650
C
      PI=CI/ANI
      TAGI=XIDTEM(ISET)
C
      J=J+1
      Y2(J)=CI
      X2(J)=TAGI
      D2(J)=1.0
C
      J=J+1
      YMID=CBARG
      Y2(J)=YMID
      X2(J)=TAGI
      D2(J)=2.0
C
      J=J+1
      VARCI=0.0
      IF(ANBARG.GT.0.0)VARCI=ANBARG*PBARG*(1.0-PBARG)
      SDCI=0.0
      IF(VARCI.GT.0.0)SDCI=SQRT(VARCI)
      YUPPER=YMID+3.0*SDCI
      Y2(J)=YUPPER
      X2(J)=TAGI
      D2(J)=3.0
C
      J=J+1
      YLOWER=YMID-3.0*SDCI
      IF(YLOWER.LT.0.0)YLOWER=0.0
      Y2(J)=YLOWER
      X2(J)=TAGI
      D2(J)=4.0
C
      IF(CCTARG.EQ.CPUMIN)GOTO1671
      J=J+1
      Y2(J)=CCTARG
      X2(J)=XIDTEM(ISET)
      D2(J)=5.0
 1671 CONTINUE
C
      IF(CCUSL.EQ.CPUMIN)GOTO1672
      J=J+1
      Y2(J)=CCUSL
      X2(J)=XIDTEM(ISET)
      D2(J)=6.0
 1672 CONTINUE
C
      IF(CCLSL.EQ.CPUMIN)GOTO1673
      J=J+1
      Y2(J)=CCLSL
      X2(J)=XIDTEM(ISET)
      D2(J)=7.0
 1673 CONTINUE
C
 1650 CONTINUE
      N2=J
      NPLOTV=3
      GOTO9000
C
C               ********************************************************
C               **  STEP 5.7--                                        **
C               **  TREAT THE Q U CONTROL CHART CASE (POISSON)         **
C               **  DEFECTIVE PER UNIT
C               **  DEFECTIVE PER UNIT AREA
C               **  NUMBER DEFECTIVE PER SUB-BATCH / LENGTH OR AREA
C               **  THE INPUT IS A DUAL SERIES--
C               **     1) NUMBER OF DEFECTIVE ITEMS IN THE SUBSAMPLE
C               **     2) LENGTH OR AREA OF THE ITEM
C               **  THE CONFIDENCE BAND IS GOTTEN BY ASSUMING POISSON**
C               ********************************************************
C
 1700 CONTINUE
C
      ISTEPN='5.7'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'QCC2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      SUM1=0.0
      SUM2=0.0
      DO1710ISET=1,NUMSET
      SUM1=SUM1+Y(ISET)
      SUM2=SUM2+YN(ISET)
 1710 CONTINUE
      CTOTAL=SUM1
      SIZTOT=SUM2
      CBARG=CTOTAL/SIZTOT
C
      J=0
      DO1750ISET=1,NUMSET
C
      CI=Y(ISET)
      SIZEI=YN(ISET)
      NSIZEI=INT(SIZEI+0.5)
      IF(NSIZEI.LE.0)GOTO1750
C
      TAGI=XIDTEM(ISET)
C
      J=J+1
      Y2(J)=(-1.0)
      IF(SIZEI.NE.0.0)Y2(J)=CI/SIZEI
      X2(J)=TAGI
      D2(J)=1.0
C
      J=J+1
      YMID=CBARG
      Y2(J)=YMID
      X2(J)=TAGI
      D2(J)=2.0
C
      J=J+1
      VARCI=0.0
      IF(ANI.GT.0.0)VARCI=CBARG/SIZEI
      SDCI=0.0
      IF(VARCI.GT.0.0)SDCI=SQRT(VARCI)
      YUPPER=YMID+3.0*SDCI
      Y2(J)=YUPPER
      X2(J)=TAGI
      D2(J)=3.0
C
      J=J+1
      YLOWER=YMID-3.0*SDCI
      IF(YLOWER.LT.0.0)YLOWER=0.0
      Y2(J)=YLOWER
      X2(J)=TAGI
      D2(J)=4.0
C
      IF(CCTARG.EQ.CPUMIN)GOTO1771
      J=J+1
      Y2(J)=CCTARG
      X2(J)=XIDTEM(ISET)
      D2(J)=5.0
 1771 CONTINUE
C
      IF(CCUSL.EQ.CPUMIN)GOTO1772
      J=J+1
      Y2(J)=CCUSL
      X2(J)=XIDTEM(ISET)
      D2(J)=6.0
 1772 CONTINUE
C
      IF(CCLSL.EQ.CPUMIN)GOTO1773
      J=J+1
      Y2(J)=CCLSL
      X2(J)=XIDTEM(ISET)
      D2(J)=7.0
 1773 CONTINUE
C
 1750 CONTINUE
      N2=J
      NPLOTV=3
      GOTO9000
C
C               ********************************************************
C               **  STEP 5.8--                                        **
C               **  TREAT THE Q C CONTROL CHART CASE (POISSON)         **
C               **  TOTAL NUMBER DEFECTIVE IN A BATCH (SUBSAMPLE)    **
C               **  SUM OF DEFECTIVES IN A BATCH (SUBSAMPLE)         **
C               **  THE INPUT IS USUALLY A SERIES OF INTEGERS        **
C               **  THE VALUE WILL BE A NON-NEGATIVE INTEGER         **
C               **  THE CONFIDENCE BAND IS GOTTEN BY ASSUMING POISSON**
C               **  NOTE--THE C CHART SHOULD BE USED ONLY WHEN
C               **        THE SUBSAMPLE SIZE IS CONSTANT.
C               **        FOR VARYING SUBSAMPLE SIZE, USE THE U CHART
C               **        (ISHIKAWA, GUIDE TO QUALITY CONTROL, PAGE 77)
C               ********************************************************
C
 1800 CONTINUE
C
      ISTEPN='5.8'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'QCC2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      SUM1=0.0
      SUM2=0.0
      ANUMSE=NUMSET
      DO1810ISET=1,NUMSET
      SUM1=SUM1+Y(ISET)
      IF(NUMV2.LE.2)SUM2=SUM2+1
      IF(NUMV2.GE.3)SUM2=SUM2+YN(ISET)
 1810 CONTINUE
      CTOTAL=SUM1
      CBARG=CTOTAL/ANUMSE
C
      J=0
      DO1850ISET=1,NUMSET
C
      CI=Y(ISET)
      SIZEI=YN(ISET)
      NSIZEI=INT(SIZEI+0.5)
      IF(NSIZEI.LE.0)GOTO1850
C
      TAGI=XIDTEM(ISET)
C
      J=J+1
      Y2(J)=CI
      X2(J)=TAGI
      D2(J)=1.0
C
      J=J+1
      YMID=CBARG
      Y2(J)=YMID
      X2(J)=TAGI
      D2(J)=2.0
C
      J=J+1
      VARCI=0.0
      IF(ANI.GT.0.0)VARCI=CBARG
      SDCI=0.0
      IF(VARCI.GT.0.0)SDCI=SQRT(VARCI)
      YUPPER=YMID+3.0*SDCI
      Y2(J)=YUPPER
      X2(J)=TAGI
      D2(J)=3.0
C
      J=J+1
      YLOWER=YMID-3.0*SDCI
      IF(YLOWER.LT.0.0)YLOWER=0.0
      Y2(J)=YLOWER
      X2(J)=TAGI
      D2(J)=4.0
C
      IF(CCTARG.EQ.CPUMIN)GOTO1871
      J=J+1
      Y2(J)=CCTARG
      X2(J)=XIDTEM(ISET)
      D2(J)=5.0
 1871 CONTINUE
C
      IF(CCUSL.EQ.CPUMIN)GOTO1872
      J=J+1
      Y2(J)=CCUSL
      X2(J)=XIDTEM(ISET)
      D2(J)=6.0
 1872 CONTINUE
C
      IF(CCLSL.EQ.CPUMIN)GOTO1873
      J=J+1
      Y2(J)=CCLSL
      X2(J)=XIDTEM(ISET)
      D2(J)=7.0
 1873 CONTINUE
C
 1850 CONTINUE
      N2=J
      NPLOTV=3
      GOTO9000
C
C               ******************
C               **   STEP 90--  **
C               **   EXIT       **
C               ******************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'OFF'.AND.ISUBRO.NE.'QCC2')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPQCC2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)ICASPL,N,NUMSET,N2,IERROR
 9012 FORMAT('ICASPL,N,NUMSET,N2,IERROR = ',A4,3I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)NUMV2,ISIZE
 9013 FORMAT('NUMV2,ISIZE = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)AN,XBARG,SDG,RANGEG
 9014 FORMAT('AN,XBARG,SDG,RANGEG = ',4E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)ANUMSE,SIGMAE,RANGEE
 9015 FORMAT('ANUMSE,SIGMAE,RANGEE = ',3E15.7)
      CALL DPWRST('XXX','BUG ')
      DO9020I=1,N2
      WRITE(ICOUT,9021)I,Y2(I),X2(I),D2(I)
 9021 FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,2E15.7,F9.2)
      CALL DPWRST('XXX','BUG ')
 9020 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPQUAD(IHARG,NUMARG,IDEFPR,IHMXPR,
     1IPREC,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE PREICSION SWITCH
C              AS QUADRUPLE PRECISION.
C              THIS IN TURN SPECIFIES THAT SUBSEQUENT
C              CALCULATIONS WILL ALL BE CARRIED OUT
C              IN QUADRUPLE PRECISION.
C              THE SPECIFIED PRECISION SWITCH SPECIFICATION
C              WILL BE PLACED IN THE HOLLERITH VARIABLE IPREC.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C                     --IDEFPR (A  HOLLERITH VARIABLE)
C                     --IHMXPR (A  HOLLERITH VARIABLE)
C     OUTPUT ARGUMENTS--IPREC  (A HOLLERITH VARIABLE)
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
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--NOVEMBER  1980.
C     UPDATED         --SEPTEMBER 1981.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IDEFPR
      CHARACTER*4 IHMXPR
      CHARACTER*4 IPREC
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
      IFOUND='YES'
C
      IF(NUMARG.LE.0)GOTO1120
      IF(IHARG(NUMARG).EQ.'ON')GOTO1130
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1120
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1130
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1120
      GOTO1130
C
 1120 CONTINUE
      IHOLD=IDEFPR
      GOTO1160
C
 1130 CONTINUE
      IHOLD='QUAD'
      GOTO1160
C
 1160 CONTINUE
      IF(IHOLD.EQ.'DOUB'.AND.IHMXPR.EQ.'SING')GOTO1170
      IF(IHOLD.EQ.'TRIP'.AND.IHMXPR.EQ.'SING')GOTO1170
      IF(IHOLD.EQ.'TRIP'.AND.IHMXPR.EQ.'DOUB')GOTO1170
      IF(IHOLD.EQ.'QUAD'.AND.IHMXPR.EQ.'SING')GOTO1170
      IF(IHOLD.EQ.'QUAD'.AND.IHMXPR.EQ.'DOUB')GOTO1170
      IF(IHOLD.EQ.'QUAD'.AND.IHMXPR.EQ.'TRIP')GOTO1170
      GOTO1180
C
 1170 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1172)
 1172 FORMAT('***** ERROR IN DPQUAD--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1173)
 1173 FORMAT('      THE DESIRED PRECISION IS HIGHER')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1174)
 1174 FORMAT('      THAN PERMITTED ON THIS COMPUTER.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1175)IHOLD
 1175 FORMAT('      DESIRED PRECISION           = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1176)IHMXPR
 1176 FORMAT('      MAXIMUM ALLOWABLE PRECISION = ',A4)
      CALL DPWRST('XXX','BUG ')
      GOTO1199
C
 1180 CONTINUE
      IPREC=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1188)IPREC
 1188 FORMAT('THE PRECISION SWITCH HAS JUST BEEN SET TO ',
     1A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPQUAN(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                  IANGLU,MAXNPP,IBOOSS,ISEED,
     1                  IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--FORM A QUANTILE PLOT
C              (USEFUL FOR DISTRIBUTIONALLY COMPARING 2 DATA SETS).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--87/5
C     ORIGINAL VERSION--MAY       1987.
C     UPDATED         --MARCH     1988. ACTIVATE QUANTILE-QUANTILE
C     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE COMMON
C                                       MOVE SOME DIMENSIONS FROM DPQUA2
C     UPDATED         --FEBRUARY  2011. USE DPPARS, DPPAR3
C     UPDATED         --FEBRUARY  2011. SUPPORT FOR "HIGHLIGHTED" OPTION
C     UPDATED         --JUNE      2016. ALLOW USER-SPECIFED PERCENTILES
C     UPDATED         --JUNE      2016. SAVE A0, A1, PPCC VALUES FROM
C                                       PLOT
C     UPDATED         --JUNE      2016. BOOTSTRAP FOR POINT WISE
C                                       CONFIDENCE INTERVALS
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
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 ICASE
      CHARACTER*4 IHIGH
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 ISUBN0
C
      PARAMETER (MAXSPN=10)
      CHARACTER*4 IVARN1(MAXSPN)
      CHARACTER*4 IVARN2(MAXSPN)
      CHARACTER*4 IVARTY(MAXSPN)
      REAL PVAR(MAXSPN)
      INTEGER ILIS(MAXSPN)
      INTEGER NRIGHT(MAXSPN)
      INTEGER ICOLR(MAXSPN)
      CHARACTER*40 INAME
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZI.INC'
C
      DIMENSION Y1(MAXOBV)
      DIMENSION Y2(MAXOBV)
      DIMENSION Y3(MAXOBV)
      DIMENSION Y4(MAXOBV)
      DIMENSION XD(MAXOBV)
      DIMENSION YD(MAXOBV)
      DIMENSION XHIGH(MAXOBV)
      DIMENSION XDIST(MAXOBV)
      DIMENSION Y1SAVE(MAXOBV)
      DIMENSION Y2SAVE(MAXOBV)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TEMP3(MAXOBV)
      DIMENSION TEMP4(MAXOBV)
      DIMENSION TEMP5(MAXOBV)
C
      INTEGER ITEMP1(MAXOBV)
C
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),Y2(1))
      EQUIVALENCE (GARBAG(IGARB3),Y3(1))
      EQUIVALENCE (GARBAG(IGARB4),Y4(1))
      EQUIVALENCE (GARBAG(IGARB5),XD(1))
      EQUIVALENCE (GARBAG(IGARB6),YD(1))
      EQUIVALENCE (GARBAG(IGARB7),Y1SAVE(1))
      EQUIVALENCE (GARBAG(IGARB8),Y2SAVE(1))
      EQUIVALENCE (GARBAG(IGARB9),XHIGH(1))
      EQUIVALENCE (GARBAG(IGAR10),XDIST(1))
      EQUIVALENCE (GARBAG(JGAR11),TEMP1(1))
      EQUIVALENCE (GARBAG(JGAR12),TEMP2(1))
      EQUIVALENCE (GARBAG(JGAR13),TEMP3(1))
      EQUIVALENCE (GARBAG(JGAR14),TEMP4(1))
      EQUIVALENCE (GARBAG(JGAR15),TEMP5(1))
      EQUIVALENCE (IGARBG(IIGAR1),ITEMP1(1))
C
CCCCC END CHANGE
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPQU'
      ISUBN2='AN  '
      IFOUND='NO'
      IERROR='NO'
      IHIGH='OFF'
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'QUAN')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPQUAN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,IAND1,IAND2,MAXN,MAXNPP,IQQNPR
   53   FORMAT('ICASPL,IAND1,IAND2,MAXN,MAXNPP,IQQNPR = ',3(A4,2X),3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)IANGLU,IBUGG2,IBUGG3,IBUGQ,ISUBRO
   54   FORMAT('IANGLU,IBUGG2,IBUGG3,IBUGQ,ISUBRO = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,57)IFOUND,IERROR,NS
   57   FORMAT('IFOUND,IERROR,NS = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***********************************
C               **  TREAT THE QUANTILE PLOT CASE **
C               ***********************************
C
C               ***************************
C               **  STEP 11--            **
C               **  EXTRACT THE COMMAND  **
C               ***************************
C
      ISTEPN='11'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'QUAN')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICOM.EQ.'QUAN')THEN
        IF(IHARG(1).EQ.'QUAN' .AND. IHARG(2).EQ.'PLOT')THEN
          ILASTC=2
          IFOUND='YES'
        ELSEIF(IHARG(1).EQ.'QUAN' .AND. IHARG(2).EQ.'HIGH' .AND.
     1         IHARG(3).EQ.'PLOT')THEN
          ILASTC=3
          IFOUND='YES'
          IHIGH='ON'
        ELSEIF(IHARG(1).EQ.'QUAN' .AND. IHARG(2).EQ.'SUBS' .AND.
     1         IHARG(3).EQ.'PLOT')THEN
          ILASTC=3
          IFOUND='YES'
          IHIGH='ON'
        ENDIF
      ELSEIF(ICOM.EQ.'HIGH' .OR. ICOM.EQ.'SUBS')THEN
        IF(IHARG(1).EQ.'QUAN' .AND. IHARG(2).EQ.'QUAN' .AND.
     1     IHARG(3).EQ.'PLOT')THEN
          ILASTC=3
          IFOUND='YES'
          IHIGH='ON'
        ENDIF
      ENDIF
C
      IF(IFOUND.EQ.'NO')GOTO9000
C
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      ICASPL='QUAN'
C
C               ****************************************
C               **  STEP 2--                          **
C               **  EXTRACT THE VARIABLE LIST         **
C               ****************************************
C
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'QUAN')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='QUANTILE-QUANTILE PLOT'
      MINNA=1
      MAXNA=100
      MINN2=2
      IFLAGE=0
      IFLAGM=1
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=2
      MAXNVA=2
      IF(IHIGH.EQ.'ON')THEN
        MINNVA=3
        MAXNVA=3
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'QUAN')THEN
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
      DO290I=1,MAX(NRIGHT(1),NRIGHT(2))
        XHIGH(I)=1.0
  290 CONTINUE
C
C     IN ORDER TO ACCOMODATE MATRIX ARGUMENTS, CALL EACH
C     VARIABLE SEPARATELY.
C
      NUMVA2=1
      ICOL=1
      CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1            INAME,IVARN1,IVARN2,IVARTY,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1            MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1            MAXCP4,MAXCP5,MAXCP6,
     1            V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1            Y1,Y1,Y1,NS1,NTEMP,NTEMP,ICASE,
     1            IBUGG3,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      ICOL=2
      CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1            INAME,IVARN1,IVARN2,IVARTY,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1            MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1            MAXCP4,MAXCP5,MAXCP6,
     1            V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1            Y2,Y2,Y2,NS2,NTEMP,NTEMP,ICASE,
     1            IBUGG3,ISUBRO,IFOUND,IERROR)
C
      IF(IHIGH.EQ.'ON')THEN
        ICOL=3
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              XHIGH,XHIGH,XHIGH,NHIGH,NTEMP,NTEMP,ICASE,
     1              IBUGG3,ISUBRO,IFOUND,IERROR)
      ELSE
        NHIGH=0
      ENDIF
C
C               ********************************************************
C               **  STEP 41--                                          *
C               **  FORM THE VERTICAL AND HORIZONTAL AXIS              *
C               **  VARIABLES (Y(.) AND X(.), RESPECTIVELY) FOR THE    *
C               **  PLOT.  FORM THE CURVE DESIGNATION VARIABLE D(.)  . *
C               **  THIS WILL BE BOTH ONES FOR BOTH CASES              *
C               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).      *
C               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).      *
C               ********************************************************
C
      ISTEPN='41'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'QUAN')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPQUA2(Y1,NS1,Y2,NS2,XHIGH,NHIGH,ICASPL,MAXN,IQQNPR,
     1            TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,ITEMP1,
     1            Y,X,D,NPLOTP,NPLOTV,
     1            Y1SAVE,Y2SAVE,XDIST,
     1            IQQBOO,IBOOSS,ISEED,A0,A1,PPCC,
     1            IBUGG3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IH='PPCC'
      IH2='    '
      VALUE0=PPCC
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1            IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1            IANS,IWIDTH,IBUGG3,IERROR)
C
      IH='PPA0'
      IH2='    '
      VALUE0=A0
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1            IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1            IANS,IWIDTH,IBUGG3,IERROR)
C
      IH='PPA1'
      IH2='    '
      VALUE0=A1
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1            IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1            IANS,IWIDTH,IBUGG3,IERROR)
C
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'QUAN')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPQUAN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTV,NPLOTP,ICASPL,IAND1,IAND2
 9013   FORMAT('NPLOTV,NPLOTP,ICASPL,IAND1,IAND2 = ',
     1         2I8,2X,2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)ICASPL,IHIGH,MAXN,NUMVAR
 9014   FORMAT('ICASPL,IHIGH,MAXN,NUMVAR = ',A4,2X,A4,2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)NS1,NS2,NHIGH
 9015   FORMAT('NS1,NS2,NHIGH = ',3I8)
        CALL DPWRST('XXX','BUG ')
        IF(NPLOTP.GT.0)THEN
          DO9020I=1,NPLOTP
            WRITE(ICOUT,9021)I,Y(I),X(I),D(I)
 9021       FORMAT('I,Y(I),X(I),D(I) = ',I8,3G15.7)
            CALL DPWRST('XXX','BUG ')
 9020     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPQUA2(Y,NY,X,NX,XHIGH,NHIGH,ICASPL,MAXN,IQQNPR,
     1                  TEMP1,TEMP2,TEMP3,TEMP4,AINDEX,INDX,
     1                  Y2,X2,D2,N2,NPLOTV,
     1                  YSAVE,XSAVE,XDIST,
     1                  IQQBOO,IBOOSS,ISEED,A0,A1,PPCC,
     1                  IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS THAT WILL DEFINE
C              A QUANTILE PLOT
C              (USEFUL FOR DISTRIBUTIONALLY COMPARING 2 DATA SETS).
C     NOTE--THE QUANTILES FOR THE FIRST  ARGUMENT WILL APPEAR VERTICALLY;
C           THE QUANTILES FOR THE SECOND ARGUMENT WILL APPEAR HORIZONTALLY.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--87/6
C     ORIGINAL VERSION--JUNE      1987.
C     UPDATED         --MARCH     1988.  PUT IN DIAGONAL REFERENCE LINE
C     UPDATED         --JUNE      1990.  MOVE SOME DIMENSIONS TO DPQUAN
C     UPDATED         --APRIL     1992.  N TO NX IN DEBUG STATEMENTS
C     UPDATED         --NOVEMBER  1994.  EQUATE ICASE TO ICASPL
C     UPDATED         --FEBRUARY  2011.  SUPPORT FOR "HIGHLIGHT" OPTION
C     UPDATED         --JUNE      2016. ALLOW USER-SPECIFED PERCENTILES
C     UPDATED         --JUNE      2016. SAVE A0, A1, PPCC VALUES FROM
C                                       PLOT
C     UPDATED         --JUNE      2016. DON'T TREAT N=1 OR ALL DATA
C                                       VALUES EQUAL AS AN ERROR.  TREAT
C                                       AS A "DEGENERATE" CASE.
C     UPDATED         --JUNE      2016. BOOTSTRAP FOR POINT WISE
C                                       CONFIDENCE INTERVALS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IQQBOO
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ICASE
      CHARACTER*4 ICASJB
      CHARACTER*4 IOP
CCCCC ADD FOLLOWING LINE NOVEMBER 1994.
      CHARACTER*4 ICASPL
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IWRITE
      CHARACTER*1 IATEMP
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION XHIGH(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
      DIMENSION YSAVE(*)
      DIMENSION XSAVE(*)
      DIMENSION XDIST(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
      DIMENSION TEMP4(*)
      DIMENSION AINDEX(*)
C
      INTEGER INDX(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPQU'
      ISUBN2='A2  '
      IERROR='NO'
      IWRITE='OFF'
      ICASE=ICASPL
      ICASJB='BOOT'
C
      ANY=NY
      ANX=NX
      NTAG=0
      NXSAVE=0
      NYSAVE=0
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'QUA2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPQUA2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGG3,ISUBRO,ICASPL,IQQBOO
   52   FORMAT('IBUGG3,ISUBRO,ICASPL,IQQBOO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)MAXN,NX,NY,NHIGH,IQQNPR,IBOOSS,ISEED
   53   FORMAT('MAXN,NX,NY,NHIGH,IQQNPR,IBOOSS,ISEED = ',7I8)
        CALL DPWRST('XXX','BUG ')
        IF(NY.GE.1)THEN
          DO61I=1,NY
            WRITE(ICOUT,62)I,Y(I)
   62       FORMAT('I,Y(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
   61     CONTINUE
        ENDIF
        IF(NX.GE.1)THEN
          DO71I=1,NX
            WRITE(ICOUT,72)I,X(I)
   72       FORMAT('I,X(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
   71     CONTINUE
        ENDIF
        IF(NHIGH.GE.1)THEN
          DO81I=1,NHIGH
            WRITE(ICOUT,82)I,XHIGH(I)
   82       FORMAT('I,XHIGH(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
   81     CONTINUE
        ENDIF
      ENDIF
C
C               ********************************************
C               **  STEP 11--                             **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      ISTEPN='11'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'QUA2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     2016/06: ONLY REQUIRE N >= 1.
C
CCCCC IF(NY.LT.2)THEN
      IF(NY.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1111)
 1111   FORMAT('***** ERROR IN QUANTILE-QUANTILE PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1112)
 1112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE FIRST ',
     1         'RESPONSE VARIABLE IS LESS THAN ONE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1114)NY
 1114   FORMAT('      THE NUMBER OF OBSERVATIONS = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ELSEIF(NX.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1122)
 1122   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE SECOND ',
     1         'RESPONSE VARIABLE IS LESS THAN ONE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1114)NX
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ELSEIF(NHIGH.GT.0 .AND. NHIGH.NE.MIN(NX,NY))THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1125)
 1125   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE HIGHLIGHTING ',
     1         'VARIABLE IS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1126)
 1126   FORMAT('      NOT EQUAL TO THE NUMBER OF OBSERVATIONS IN THE ',
     1         'SHORTER RESPONSE VARIABLE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1127)NY
 1127   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE FIRST     ',
     1         'RESPONSE VARIABLE = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1128)NX
 1128   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE SECOND    ',
     1         'RESPONSE VARIABLE = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1129)NHIGH
 1129   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE HIGHLIGHT ',
     1         'VARIABLE          = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
CCCCC HOLD=Y(1)
CCCCC DO1130I=1,NY
CCCCC   IF(Y(I).NE.HOLD)GOTO1139
C1130 CONTINUE
CCCCC WRITE(ICOUT,999)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,1111)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,1132)
C1132 FORMAT('      ALL ELEMENTS FOR THE FIRST RESPONSE VARIABLE')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,1133)HOLD
C1133 FORMAT('      ARE IDENTICALLY EQUAL TO ',G15.7)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,999)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC IERROR='YES'
CCCCC GOTO9000
C1139 CONTINUE
C
CCCCC HOLD=X(1)
CCCCC DO1140I=1,NY
CCCCC   IF(X(I).NE.HOLD)GOTO1149
C1140 CONTINUE
CCCCC WRITE(ICOUT,999)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,1111)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,1142)
C1142 FORMAT('      ALL ELEMENTS FOR THE SECOND RESPONSE VARIABLE')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,1143)HOLD
C1143 FORMAT('      ARE IDENTICALLY EQUAL TO ',G15.7)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,999)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC IERROR='YES'
CCCCC GOTO9000
C1149 CONTINUE
C
      IF(IQQBOO.EQ.'ON')THEN
        DO1210II=1,NX
          XSAVE(II)=X(II)
 1210   CONTINUE
        NXSAVE=NX
        DO1220II=1,NY
          YSAVE(II)=Y(II)
 1220   CONTINUE
        NYSAVE=NY
C
        IOP='OPEN'
        IFLAG1=1
        IFLAG2=0
        IFLAG3=0
        IFLAG4=0
        IFLAG5=0
        CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1              IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1              IBUGG3,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
      ENDIF
C
      IPASS=-1
C
C               ****************************************************
C               **  STEP 21--                                     **
C               **  SORT Y AND SORT X                             **
C               ****************************************************
C
 2000 CONTINUE
      IPASS=IPASS+1
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'QUA2')THEN
        WRITE(ICOUT,2002)
 2002   FORMAT('IPASS: ',I10)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IPASS.GT.0)THEN
        IF(IQQBOO.EQ.'OFF' .OR. IPASS.GT.IBOOSS)THEN
          GOTO8000
        ELSE
          DO2001II=1,NXSAVE
            X(II)=XSAVE(II)
 2001     CONTINUE
          NX=NXSAVE
          DO2003II=1,NYSAVE
            Y(II)=YSAVE(II)
 2003     CONTINUE
          NY=NYSAVE
C
C         FOR BOOTSTRAP, FIX X ARRAY BUT CREATE BOOTSTRAP
C         ARRAY FOR Y ARRAY.
C
          IJACIN=0
CCCCC     CALL DPJBS3(X,NX,ICASJB,IJACIN,ISEED,TEMP4,NX2,
CCCCC1                INDX,AINDEX,
CCCCC1                IBUGG3,IERROR)
CCCCC     DO2006II=1,NX
CCCCC       X(II)=TEMP4(II)
C2006     CONTINUE
C
          CALL DPJBS3(Y,NY,ICASJB,IJACIN,ISEED,TEMP4,NY2,
     1                INDX,AINDEX,
     1                IBUGG3,IERROR)
          DO2008II=1,NY
            Y(II)=TEMP4(II)
 2008     CONTINUE
C
        ENDIF
      ENDIF
C
      ISTEPN='21'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'QUA2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NHIGH.LE.0)THEN
        IF(IQQNPR.GT.0)THEN
          CALL PERCE2(IQQNPR,X,NX,IWRITE,TEMP3,MAXN,TEMP1,
     1                IBUGG3,ISUBRO,IERROR)
          DO2010II=1,IQQNPR
            X(II)=TEMP1(II)
 2010     CONTINUE
          NX=IQQNPR
C
          CALL PERCE2(IQQNPR,Y,NY,IWRITE,TEMP3,MAXN,TEMP2,
     1                IBUGG3,ISUBRO,IERROR)
          DO2020II=1,IQQNPR
            Y(II)=TEMP2(II)
 2020     CONTINUE
          NY=IQQNPR
C
        ELSE
          CALL SORT(X,NX,X)
          CALL SORT(Y,NY,Y)
        ENDIF
      ELSEIF(NY.LE.NX)THEN
        CALL SORT(X,NX,X)
        CALL SORTC(Y,XHIGH,NY,Y,XDIST)
        DO2101I=1,NY
          XHIGH(I)=XDIST(I)
 2101   CONTINUE
      ELSEIF(NY.GT.NX)THEN
        CALL SORT(Y,NY,Y)
        CALL SORTC(X,XHIGH,NX,X,XDIST)
        DO2103I=1,NX
          XHIGH(I)=XDIST(I)
 2103   CONTINUE
      ENDIF
C
C               *****************************************
C               **  STEP 22--                          **
C               **  DETERMINE THE TYPE CASE            **
C               **  EQUAL SAMPLE SIZES OR NOT)         **
C               **  AND BRANCH ACORDINGLY              **
C               *****************************************
C
      ISTEPN='22'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'QUA2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NY.LT.NX)THEN
        CALL PERCE2(NY,X,NX,IWRITE,TEMP3,MAXN,TEMP2,
     1              IBUGG3,ISUBRO,IERROR)
        DO2120II=1,NY
          X(II)=TEMP2(II)
 2120   CONTINUE
        NX=NY
      ELSEIF(NY.GT.NX)THEN
        CALL PERCE2(NX,Y,NY,IWRITE,TEMP3,MAXN,TEMP2,
     1              IBUGG3,ISUBRO,IERROR)
        DO2130II=1,NX
          Y(II)=TEMP2(II)
 2130   CONTINUE
        NY=NX
      ENDIF
C
C               *******************************************
C               **  STEP 51--                            **
C               **  FORM PLOT COORDINATES                **
C               *******************************************
C
      ISTEPN='51'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'QUA2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPASS.EQ.0)THEN
        IF(NHIGH.GT.0)THEN
          CALL CODE(XHIGH,NHIGH,IWRITE,XDIST,D2,MAXN,IBUGG3,IERROR)
          CALL MAXIM(XDIST,NHIGH,IWRITE,XMAX,IBUGG3,IERROR)
        ENDIF
C
        J=0
        DO5111I=1,NX
          J=J+1
          Y2(J)=Y(J)
          X2(J)=X(J)
          IF(NHIGH.EQ.0)THEN
            D2(J)=1.0
          ELSE
            D2(J)=XDIST(J)
          ENDIF
 5111   CONTINUE
C
        N2=J
        CALL LINFIT(Y2,X2,N2,
     1            A0,A1,RESSD,RESDF,PPCC,SDA0,SDA1,CCALBE,
     1            ISUBRO,IBUGG3,IERROR)
C
        IF(NHIGH.EQ.0)THEN
          NTEMP=1
        ELSE
          NTEMP=INT(XMAX+0.1)
        ENDIF
C
        NTEMP=NTEMP+1
        AMIN=X(1)
        AMIN2=X(1)
        IF(Y(1).LT.X(1))AMIN=Y(1)
        J=J+1
        Y2(J)=AMIN
        X2(J)=AMIN
        D2(J)=REAL(NTEMP)
C
        AMAX=X(NX)
        AMAX2=X(NX)
        IF(Y(NY).GT.X(NX))AMAX=Y(NY)
        J=J+1
        Y2(J)=AMAX
        X2(J)=AMAX
        D2(J)=REAL(NTEMP)
C
C       2016/06: GENERATE FITTED LINE ON THE PLOT
C
        NTEMP=NTEMP+1
        XVAL=AMIN2
        YVAL=A0 + A1*XVAL
        J=J+1
        X2(J)=XVAL
        Y2(J)=YVAL
        D2(J)=REAL(NTEMP)
C
        XVAL=AMAX2
        YVAL=A0 + A1*XVAL
        J=J+1
        X2(J)=XVAL
        Y2(J)=YVAL
        D2(J)=REAL(NTEMP)
C
        N2=J
        NPLOTV=3
        NTAG=NTEMP
      ELSE
        DO5211I=1,NX
          WRITE(IOUNI1,5213)IPASS,I,Y(I),X(I)
 5213     FORMAT(2I10,2E15.7)
 5211   CONTINUE
      ENDIF
C
      IF(IQQBOO.EQ.'ON')GOTO2000
      GOTO9000
C
 8000 CONTINUE
      IF(IQQBOO.EQ.'ON')THEN
C
C       STEP 1: CLOSE THE FILE CONTAINING THE BOOTSTRAP POINTS
C
        IOP='CLOS'
        CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1              IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1              IBUGG3,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
C       STEP 2: RE-OPEN THE FILE
C
        IOP='OPEN'
        CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1              IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1              IBUGG3,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
C       STEP 3: NOW LOOP THROUGH THE POINTS
C
        NTEMP=MIN(NX,NY)
        DO8010II=1,NTEMP
          REWIND(IOUNI1)
          IVAL=II
          ISKIP1=IVAL-1
          ISKIP2=NTEMP-IVAL
          ICNT=0
C
          DO8020JJ=1,IBOOSS
            DO8030KK=1,ISKIP1
              READ(IOUNI1,'(A1)',END=8091,ERR=8093)IATEMP
 8030       CONTINUE
            ICNT=ICNT+1
            READ(IOUNI1,5213,END=8091,ERR=8093)IJUK1,IJUNK2,
     1                                         TEMP1(ICNT),TEMP2(ICNT)
            DO8040KK=1,ISKIP2
              READ(IOUNI1,'(A1)',END=8091,ERR=8093)IATEMP
 8040       CONTINUE
 8020     CONTINUE
          P025=2.5
          CALL PERCEN(P025,TEMP1,ICNT,IWRITE,TEMP3,MAXN,
     1                Y025,IBUGG3,IERROR)
          CALL PERCEN(P025,TEMP2,ICNT,IWRITE,TEMP3,MAXN,
     1                X025,IBUGG3,IERROR)
          P975=97.5
          CALL PERCEN(P975,TEMP1,ICNT,IWRITE,TEMP3,MAXN,
     1                Y975,IBUGG3,IERROR)
          CALL PERCEN(P975,TEMP2,ICNT,IWRITE,TEMP3,MAXN,
     1                X975,IBUGG3,IERROR)
          N2=N2+1
          X2(N2)=X025
          Y2(N2)=Y025
          D2(N2)=REAL(NTAG+1)
          N2=N2+1
          X2(N2)=X975
          Y2(N2)=Y975
          D2(N2)=REAL(NTAG+2)
C
 8010   CONTINUE
        GOTO8099
C
C       STEP 4: UNEXPECTED END OF FILE ENCOUNTERED
C
 8091   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8111)
 8111   FORMAT('      UNEXPECTED END OF FILE READING BOOTSTRAP FILE.')
        CALL DPWRST('XXX','BUG ')
        GOTO8099
C
C       STEP 5: UNEXPECTED ERROR READING FILE
C
 8093   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8113)
 8113   FORMAT('      UNEXPECTED ERROR READING BOOTSTRAP FILE.')
        CALL DPWRST('XXX','BUG ')
        GOTO8099
C
C       STEP 6: FINAL CLOSE OF FILE
C
 8099   CONTINUE
        IOP='CLOS'
        CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1              IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1              IBUGG3,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
      ENDIF
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'QUA2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPQUA2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)N2,ICASPL,ICASE,IERROR
 9012   FORMAT('N2,ICASPL,ICASE,IERROR = ',I8,2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,N2
          WRITE(ICOUT,9016)I,Y2(I),X2(I),D2(I)
 9016     FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
        WRITE(ICOUT,9053)NY,NX,AMIN,AMAX
 9053   FORMAT('NY,NX,AMIN,AMAX = ',2I8,2G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPQUCO(XTEMP1,XTEMP2,MAXNXT,ICASAN,
     1                  ICAPSW,IFORSW,IMULT,IREPL,
     1                  ISUBRO,IBUGA2,IBUGA3,IBUGQ,IFOUND,IERROR)
C
C     PURPOSE--GENERATE CONFIDENCE LIMITS FOR QUANTILES (MEDIAN IS
C              A SPECIAL CASE).  METHOD BASED ON MARITZ-JARRETT
C              ESTIMATE FOR STANDARD ERROR.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     REFERENCE--"INTRODUCTION TO ROBUST ESTIMATION AND HYPOTHESIS
C                TESTING", RAND R. WILCOX, ACADEMIC PRESS, 1997.
C                1977.
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2003/2
C     ORIGINAL VERSION--FEBRUARY  2003.
C     UPDATED         --OCTOBER   2003. ADD SUPPORT FOR HTML, LATEX
C                                       OUTPUT
C     UPDATED         --MARCH     2010. USE DPDTA1, DPDTA4 TO GENERATE
C                                       HTML, LATEX, RTF FORMAT
C     UPDATED         --MARCH     2010. SUPPORT FOR MULTIPLE RESPONSE
C                                       VARIABLES AND FOR GROUP-ID
C                                       VARIABLES (I.E., REPLICATION
C                                       CASE)
C     UPDATED         --MARCH     2010. USE DPPAR3 TO EXTRACT EITHER A
C                                       RESPONSE VARIABLE OR A MATRIX
C                                       NAME
C     UPDATED         --AUGUST    2019. ADD CTL999, CTU999
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
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 ICASEQ
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 ICASAN
      CHARACTER*4 ICASE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IFLAGU
      CHARACTER*4 IREPL
      CHARACTER*4 IMULT
C
      LOGICAL IFRST
      LOGICAL ILAST
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
      ISUBN1='DPQU'
      ISUBN2='CO  '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
      IFOUND='YES'
      IERROR='NO'
C
C               *************************************************
C               **  TREAT THE QUANTILE CONFIDENCE LIMITS CASE  **
C               *************************************************
C
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'QUCO')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPQUCO--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA2,IBUGA3,IBUGQ,ICASAN,MAXNXT
   52   FORMAT('IBUGA2,IBUGA3,IBUGQ,ICASAN,MAXNXT = ',4(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *********************************
C               **  STEP 1--                   **
C               **  EXTRACT THE VARIABLE LIST  **
C               *********************************
C
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'QUCO')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='QUANTILE CONFIDENCE LIMITS'
      MAXNA=100
      MINNVA=1
      MAXNVA=100
      MINNA=1
      IFLAGE=1
      IF(IREPL.EQ.'ON')THEN
        MAXNVA=7
      ELSE
        MAXNVA=100
        IFLAGE=0
      ENDIF
      MINN2=2
      IFLAGM=1
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'QUCO')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,181)
  181   FORMAT('***** AFTER CALL DPPARS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,182)NQ,NUMVAR,IMULT,IREPL
  182   FORMAT('NQ,NUMVAR,IMULT,IREPL = ',2I8,2X,A4,2X,A4)
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'QUCO')
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
  101     FORMAT('***** ERROR IN QUANTILE CONFIDENCE LIMITS--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,211)
  211     FORMAT('      FOR THE REPLICATION CASE, THE NUMBER OF ',
     1           'REPLICATION VARIABLES')
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'QUCO')THEN
        WRITE(ICOUT,221)NRESP,NREPL
  221   FORMAT('NRESP,NREPL = ',2I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C     ******************************************************
C     **  STEP 3--                                        **
C     **  DETERMINE QUANTILE TO USE (FROM P100)           **
C     ******************************************************
C
      IF(ICASAN.EQ.'MECI')THEN
        P100=0.50
      ELSE
        IH='P100'
        IH2='    '
        IHWUSE='P'
        MESSAG='YES'
        CALL CHECKN(IH,IH2,IHWUSE,
     1  IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1  ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        P100=VALUE(ILOCP)
        IF(P100.GE.1.0 .AND. P100.LE.100.0)P100=P100/100.0
      ENDIF
C
      IF(P100.LE.0.0 .OR. P100.GE.1.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,302)
  302   FORMAT('      THE QUANTILE FOR WHICH THE CONFIDENCE INTERVAL ',
     1         'IS TO BE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,303)
  303   FORMAT('      COMPUTED MUST BE BETWEEN 0 AND 1, BUT WAS NOT.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,304)P100
  304   FORMAT('      PARAMETER P100   = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,306)
  306   FORMAT('      USE THE LET COMMAND TO PRE-DEFINE P100:')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,307)
  307   FORMAT('          LET P100 = 0.5')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C
C               ******************************************************
C               **  STEP 3--                                        **
C               **  GENERATE THE CONFIDENCE LIMITS FOR THE VARIOUS  **
C               **  CASES                                           **
C               ******************************************************
C
      ISTEPN='3'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'QUCO')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               *****************************************
C               **  STEP 3A--                          **
C               **  CASE 1: SINGLE RESPONSE VARIABLE   **
C               **          WITH NO REPLICATION        **
C               *****************************************
C
      IF(IMULT.EQ.'OFF' .AND. NREPL.EQ.0)THEN
        ISTEPN='3A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'QUCO')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        PID(1)=CPUMIN
        IVARID(1)=IVARN1(1)
        IVARI2(1)=IVARN2(1)
C
        ICOL=1
        NUMVA2=1
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              Y,XTEMP1,XTEMP2,NLOCAL,NLOCA2,NLOCA3,ICASE,
     1              IBUGA3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
C               ******************************************************
C               **  STEP 3B--                                       **
C               **  PREPARE FOR ENTRANCE INTO DPQUC2--              **
C               ******************************************************
C
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'QUCO')THEN
          ISTEPN='3B'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,331)
  331     FORMAT('***** FROM DPQUCO, AS WE ARE ABOUT TO CALL DPQUC2--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,332)NLOCAL,MAXN,P100
  332     FORMAT('NLOCAL,MAXN,P100 = ',2I8,G15.7)
          CALL DPWRST('XXX','BUG ')
          DO335I=1,NLOCAL
            WRITE(ICOUT,336)I,Y(I)
  336       FORMAT('I,Y(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
  335     CONTINUE
          WRITE(ICOUT,338)ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP
  338     FORMAT('ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP = ',5(A4,2X))
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,339)ICASAN,ISUBRO,IBUGA3,IERROR
  339     FORMAT('ICASAN,ISUBRO,IBUGA3,IERROR = ',4A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IERROR='NO'
        CALL DPQUC2(Y,NLOCAL,P100,
     1              XTEMP1,MAXNXT,
     1              PID,IVARID,IVARI2,NREPL,
     1              CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1              CTL999,CTU999,
     1              ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,
     1              ICASAN,ISUBRO,IBUGA3,IERROR)
C
        IFLAGU='ON'
        IFRST=.FALSE.
        ILAST=.FALSE.
        CALL DPCNF3(CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1              CTL999,CTU999,
     1              IFLAGU,IFRST,ILAST,ICASAN,
     1              IBUGA2,IBUGA3,ISUBRO,IERROR)
C
C               *******************************************
C               **  STEP 4A--                            **
C               **  CASE 2: MULTIPLE RESPONSE VARIABLES  **
C               *******************************************
C
      ELSEIF(IMULT.EQ.'ON')THEN
        ISTEPN='4A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'QUCO')
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
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'QUCO')THEN
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
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'QUCO')THEN
            ISTEPN='4B'
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,422)
  422       FORMAT('***** FROM THE MIDDLE  OF DPQUCO--')
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
          CALL DPQUC2(Y,NLOCAL,P100,
     1                XTEMP1,MAXNXT,
     1                PID,IVARID,IVARI2,NREPL,
     1                CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1                CTL999,CTU999,
     1                ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,
     1                ICASAN,ISUBRO,IBUGA3,IERROR)
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
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'QUCO')
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
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'QUCO')
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
C
        IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'QUCO')THEN
          ISTEPN='5C'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,541)
  541     FORMAT('***** FROM THE MIDDLE  OF DPQUCO--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,542)ICASAN,NUMVAR,NLOCAL,NREPL
  542     FORMAT('ICASAN,NUMVAR,NLOCAL,NREPL = ',A4,2X,3I8)
          CALL DPWRST('XXX','BUG ')
          IF(NLOCAL.GE.1)THEN
            DO545I=1,NLOCAL
              WRITE(ICOUT,546)I,Y(I),XDESGN(I,1),XDESGN(I,2)
  546         FORMAT('I,Y(I),XDESGN(I,1),XDESGN(I,2) = ',
     1               I8,3F12.5)
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
              ENDIF
 1130       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            IF(NTEMP.GT.0)THEN
              CALL DPQUC2(TEMP1,NTEMP,P100,
     1                    XTEMP1,MAXNXT,
     1                    PID,IVARID,IVARI2,NREPL,
     1                    CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1                    CTL999,CTU999,
     1                    ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,
     1                    ICASAN,ISUBRO,IBUGA3,IERROR)
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
              ENDIF
 1290       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              CALL DPQUC2(TEMP1,NTEMP,P100,
     1                    XTEMP1,MAXNXT,
     1                    PID,IVARID,IVARI2,NREPL,
     1                    CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1                    CTL999,CTU999,
     1                    ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,
     1                    ICASAN,ISUBRO,IBUGA3,IERROR)
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
              ENDIF
 1390       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            IF(NTEMP.GT.0)THEN
              CALL DPQUC2(TEMP1,NTEMP,P100,
     1                    XTEMP1,MAXNXT,
     1                    PID,IVARID,IVARI2,NREPL,
     1                    CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1                    CTL999,CTU999,
     1                    ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,
     1                    ICASAN,ISUBRO,IBUGA3,IERROR)
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
              ENDIF
 1490       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            IF(NTEMP.GT.0)THEN
              CALL DPQUC2(TEMP1,NTEMP,P100,
     1                    XTEMP1,MAXNXT,
     1                    PID,IVARID,IVARI2,NREPL,
     1                    CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1                    CTL999,CTU999,
     1                    ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,
     1                    ICASAN,ISUBRO,IBUGA3,IERROR)
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
              ENDIF
 1590       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            IF(NTEMP.GT.0)THEN
              CALL DPQUC2(TEMP1,NTEMP,P100,
     1                    XTEMP1,MAXNXT,
     1                    PID,IVARID,IVARI2,NREPL,
     1                    CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1                    CTL999,CTU999,
     1                    ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,
     1                    ICASAN,ISUBRO,IBUGA3,IERROR)
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
              ENDIF
 1690       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            IF(NTEMP.GT.0)THEN
              CALL DPQUC2(TEMP1,NTEMP,P100,
     1                    XTEMP1,MAXNXT,
     1                    PID,IVARID,IVARI2,NREPL,
     1                    CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1                    CTL999,CTU999,
     1                    ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,
     1                    ICASAN,ISUBRO,IBUGA3,IERROR)
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'QUCO')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPQUCO--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)ICASEQ,NRIGHT(1),NS
 9014   FORMAT('ICASEQ,NRIGHT(1),NS = ',A4,2X,2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)IFOUND,IERROR
 9016   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPQUC2(Y,N,P100,
     1                  XTEMP1,MAXNXT,
     1                  PID,IVARID,IVARI2,NREPL,
     1                  CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,
     1                  CTL999,CTU999,
     1                  ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,
     1                  ICASAN,ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS ROUTINE GENERATES QUANTILE CONFIDENCE LIMITS
C              FOR THE DATA IN THE INPUT VECTOR Y.
C              THE MEDIAN IS A SPECIAL CASE.  SPECIFICALLY,
C                   X(0.5) +/- NORPPF(1-ALPHA/2)*QUASE
C              WHERE QUASE IS THE MARITZ-JARRETT ESTIMATE OF
C              THE QUANTILE STANDARD ERROR.
C              METHOD FROM PAGE 87 OF THE RAND WILCOX BOOK
C              "INTRODUCTION TO ROBUST ESTIMATION AND HYPOTHESIS
C              TESTING", ACADEMIC PRESS, 1997.
C              ALSO VIA THE HETTMANSPERGER-SHEATHER INTERPOLATION
C              METHOD (ALSO PAGE 87 OF WILCOX).
C     NOTE--ASSUMPTION--MODEL IS   RESPONSE = CONSTANT + ERROR.
C     INPUT  ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR
C                                OF OBSERVATIONS
C                       N      = THE INTEGER NUMBER OF
C                                OBSERVATIONS IN THE VECTOR Y.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2003/2
C     ORIGINAL VERSION--FEBRUARY  2003.
C     UPDATED         --OCTOBER   2003. ADD SUPPORT FOR HTML, LATEX
C                                       OUTPUT
C     UPDATED         --MARCH     2010. USE DPDTA2 AND DPDTA4 TO
C                                       GENERATE OUTPUT (ADDS RTF
C                                       SUPPORT)
C     UPDATED         --MARCH     2010. SOME MODIFICATIONS TO THE
C                                       OUTPUT (AESTHETIC, NOT
C                                       SUBSTANTIVE)
C     UPDATED         --AUGUST    2019. ADD CTL999, CTU999
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASAN
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ICASA2
      CHARACTER*4 IQUASE
      CHARACTER*4 IQUAME
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*40 IRTFFF
      CHARACTER*40 IRTFFP
C
      CHARACTER*4 IVARID(*)
      CHARACTER*4 IVARI2(*)
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION XTEMP1(*)
      DIMENSION PID(*)
C
      PARAMETER (NUMALP=8)
C
      DIMENSION CONF(NUMALP)
      DIMENSION T(NUMALP)
      DIMENSION TSDM(NUMALP)
      DIMENSION ALOWER(NUMALP)
      DIMENSION AUPPER(NUMALP)
      DIMENSION ALOWE2(NUMALP)
      DIMENSION AUPPE2(NUMALP)
C
      PARAMETER(NUMCLI=5)
      PARAMETER(MAXLIN=2)
      PARAMETER (MAXROW=20)
      CHARACTER*60 ITITLE
      CHARACTER*60 ITITLZ
      CHARACTER*60 ITEXT(MAXROW)
      REAL         AVALUE(MAXROW)
      INTEGER      NCTEXT(MAXROW)
      INTEGER      IDIGIT(MAXROW)
      INTEGER      NTOT(MAXROW)
      LOGICAL IFRST
      LOGICAL ILAST
C
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION DPPF
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'QUC2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPQUC2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)N,MAXNXT,NREPL,P100
   52   FORMAT('N,MAXNXT,NREPL,P100 = ',3I8,G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,53)IVARID(1),IVARI2(1),PID(1)
   53   FORMAT('IVARID(1),IVARI2(1),PID(1) = ',A4,A4,G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,54)ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP
   54   FORMAT('ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP = ',5(A4,2X))
        CALL DPWRST('XXX','WRIT')
        DO56I=1,N
          WRITE(ICOUT,57)I,Y(I)
   57     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
        WRITE(ICOUT,58)ICASAN,ISUBRO,IBUGA3,IERROR
   58   FORMAT('ICASAN,ISUBRO,IBUGA3,IERROR = ',4(A4,2X))
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      ISUBN1='DPQU'
      ISUBN2='C2  '
      IWRITE='OFF'
CCCCC IERROR='NO'
      ICASA2='QUCO'
      IQUAME='ORDE'
      IQUASE='MJ'
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
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'QUC2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N.LT.3)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN QUANTILE CONFIDENCE LIMITS--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS IN THE RESPONSE ',
     1         'VARIABLE IS LESS THAN 3')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,113)N
  113   FORMAT('SAMPLE SIZE = ',I8)
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
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,131)HOLD
  131 FORMAT('      THE RESPONSE VARIABLE HAS ALL ELEMENTS = ',G15.7)
      CALL DPWRST('XXX','WRIT')
      GOTO9000
  139 CONTINUE
C
C               ***************************************************
C               **  STEP 3--                                     **
C               **  COMPUTE THE QUANTILE              ESTIMATE   **
C               **  COMPUTE THE QUANTILE     STANDARD ERROR      **
C               ***************************************************
C
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'QUC2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IWRITE='OFF'
C
      CALL MINIM(Y,N,IWRITE,XMIN,IBUGA3,IERROR)
      CALL MAXIM(Y,N,IWRITE,XMAX,IBUGA3,IERROR)
      IF(ICASAN.EQ.'MECI')THEN
        CALL MEDIAN(Y,N,IWRITE,XTEMP1,MAXNXT,XMED,IBUGA3,IERROR)
        XQUANT=XMED
      ELSE
        CALL MEDIAN(Y,N,IWRITE,XTEMP1,MAXNXT,XMED,IBUGA3,IERROR)
        CALL QUANT(P100,Y,N,IWRITE,XTEMP1,MAXNXT,IQUAME,XQUANT,
     1  IBUGA3,IERROR)
      ENDIF
      CALL QUANSE(P100,Y,N,IWRITE,XTEMP1,MAXNXT,IQUASE,XQUASE,
     1IBUGA3,IERROR)
C
C               ***************************************
C               **  STEP 4--                         **
C               **  COMPUTE CONFIDENCE LIMITS        **
C               **  FOR VARIOUS PROBABILITY VALUES.  **
C               ***************************************
C
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'QUC2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CONF(1)=50.0
      CONF(2)=75.0
      CONF(3)=90.0
      CONF(4)=95.0
      CONF(5)=99.0
      CONF(6)=99.9
      CONF(7)=99.99
      CONF(8)=99.999
C
      DO1400I=1,8
        PCONF=CONF(I)/100.0
        CDF=0.5+PCONF/2.0
        CALL NORPPF(CDF,T(I))
        TSDM(I)=T(I)*XQUASE
        ALOWER(I)=XQUANT-TSDM(I)
        AUPPER(I)=XQUANT+TSDM(I)
 1400 CONTINUE
      CUTL90=ALOWER(3)
      CUTU90=AUPPER(3)
      CUTL95=ALOWER(4)
      CUTU95=AUPPER(4)
      CUTL99=ALOWER(5)
      CUTU99=AUPPER(5)
      CTL999=ALOWER(6)
      CTU999=AUPPER(6)
C
C               ***************************************
C               **  STEP 5--                         **
C               **  COMPUTE CONFIDENCE LIMITS        **
C               **  FOR HETTMANSPERGER-SHEATHER      **
C               **  INTERPOLATION METHOD.            **
C               ***************************************
C  
      IF(ICASAN.EQ.'MECI')THEN
        P=0.5
        AN=REAL(N)
        CALL SORT(Y,N,Y)
        DO2010I=1,8
          ALPHA=(100.0-CONF(I))/100.
          CALL BINPPF(DBLE(ALPHA/2.0),DBLE(P),N,DPPF)
          AK=REAL(DPPF)
          CALL BINCDF(DBLE(AN-AK),DBLE(P),N,DCDF)
          CDF1=REAL(DCDF)
          CALL BINCDF(DBLE(AK-1.0),DBLE(P),N,DCDF)
          CDF2=REAL(DCDF)
          GK=CDF1-CDF2
          IF(GK.GE.1.0-ALPHA)THEN
            CALL BINCDF(DBLE(AN-AK-1.0),DBLE(P),N,DCDF)
            CDF1=REAL(DCDF)
            CALL BINCDF(DBLE(AK-1.0),DBLE(P),N,DCDF)
            CDF2=REAL(DCDF)
            GKP1=CDF1-CDF2
            AKP=AK+1.0
          ELSE
            AK=AK-1.0
            CALL BINCDF(DBLE(AN-AK),DBLE(P),N,DCDF)
            CDF1=REAL(DCDF)
            CALL BINCDF(DBLE(AK-1.0),DBLE(P),N,DCDF)
            CDF2=REAL(DCDF)
            GKP1=CDF1-CDF2
            AKP=AK+1.0
          ENDIF
          ANMK=AN-AK
          ANMKP=ANMK+1.0
          AIVAR=(GK-1.0+ALPHA)/(GK-GKP1)
          ALAMB=((AN-AK)*AIVAR)/(AK+(AN-2.0*AK)*AIVAR)
          ALOWE2(I)=ALAMB*Y(INT(AKP)) + (1.0-ALAMB)*Y(INT(AK))
          AUPPE2(I)=ALAMB*Y(INT(ANMK)) + (1.0-ALAMB)*Y(INT(ANMKP))
 2010   CONTINUE
      ENDIF
C
C     ADD A FUDGE FACTOR SO THAT CONFIDENCE LEVEL WILL
C     BE PRINTED CORRECTLY TO 3 DECIMAL PLACES.
C
      CONF(1)=50.0001
      CONF(2)=75.0001
      CONF(3)=90.0001
      CONF(4)=95.0001
      CONF(5)=99.0001
      CONF(6)=99.9001
      CONF(7)=99.9901
      CONF(8)=99.9991
C
C               ****************************
C               **  STEP 7--              **
C               **  WRITE EVERYTHING OUT  **
C               ****************************
C
      ISTEPN='7'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'QUC2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPRINT.EQ.'OFF')GOTO9000
C
      IF(ICASAN.EQ.'MECI')THEN
        ITITLE='Confidence Limits for the Median'
        NCTITL=32
      ELSE
        ITITLE='Confidence Limits for Quantile (Q0 =        )'
        WRITE(ITITLE(39:44),'(F6.3)')P100
        NCTITL=45
      ENDIF
      ITITLZ='(Based on Maritz-Jarrett Standard Error for Quantiles)'
      NCTITZ=54
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
      ITEXT(ICNT)='Sample Median:'
      NCTEXT(ICNT)=14
      AVALUE(ICNT)=XMED
      IDIGIT(ICNT)=NUMDIG
      IF(ICASAN.EQ.'QUCI')THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='Sample Quantile:'
        NCTEXT(ICNT)=16
        AVALUE(ICNT)=XQUANT
      ENDIF
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Quantile Standard Error:'
      NCTEXT(ICNT)=31
      AVALUE(ICNT)=XQUASE
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'QUC2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1            AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ISTEPN='5B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CNF2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPDT11(CONF,T,TSDM,ALOWER,AUPPER,
     1            ICASAN,ICAPSW,ICAPTY,NUMDIG,
     1            ISUBRO,IBUGA3,IERROR)
C
      IF(ICASAN.EQ.'MECI')THEN
        ICASA2='QUC2'
        CALL DPDT11(CONF,T,TSDM,ALOWE2,AUPPE2,
     1              ICASA2,ICAPSW,ICAPTY,NUMDIG,
     1              ISUBRO,IBUGA3,IERROR)
       ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'QUC2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPQUC2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9012)N,IBUGA3,IERROR
 9012   FORMAT('N,IBUGA3,IERROR = ',I8,2X,A4,2X,A4)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9013)XMED,XQUANT,XQUASE
 9013   FORMAT('XMED,XQUANT,XQUASE = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPQUTE(TEMP1,TEMP2,MAXNXT,
     1                  ICAPSW,IFORSW,IMULT,
     1                  IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--CARRY OUT QUADE TEST NON-PARAMETRIC TWO-WAY ANOVA
C     EXAMPLE--QUADE TEST Y X1 X2
C     REFERENCE--CONOVER (1999), "PRACTICAL NONPARAMETRIC STATISTICS",
C                THIRD EDITION, WILEY, PP. 373-380.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2011/7
C     ORIGINAL VERSION--JULY      2011.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICAPSW
      CHARACTER*4 IFORSW
      CHARACTER*4 IMULT
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
C
      LOGICAL IFRST
      LOGICAL ILAST
      CHARACTER*4 IFLAGU
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
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZD.INC'
C
      DIMENSION XTEMP2(MAXOBV)
      DIMENSION DBLOCK(MAXOBV)
      DIMENSION DTREAT(MAXOBV)
      DIMENSION RJ(MAXOBV)
      DIMENSION QRANK(MAXOBV)
      DOUBLE PRECISION YRANK(MAXOBV)
C
      EQUIVALENCE(GARBAG(IGARB1),XTEMP2(1))
      EQUIVALENCE(GARBAG(IGARB2),DBLOCK(1))
      EQUIVALENCE(GARBAG(IGARB3),DTREAT(1))
      EQUIVALENCE(GARBAG(IGARB4),RJ(1))
      EQUIVALENCE(GARBAG(IGARB5),QRANK(1))
      EQUIVALENCE(DGARBG(IDGAR1),YRANK(1))
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPQU'
      ISUBN2='TE  '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
      IFOUND='YES'
      IERROR='NO'
C
C               ******************************************
C               **  TREAT THE QUADE TEST CASE           **
C               ******************************************
C
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'QUTE')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPQUTE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA2,IBUGA3,IBUGQ,ISUBRO,MAXNXT
   52   FORMAT('IBUGA2,IBUGA3,IBUBQ,ISUBRO,MAXNXT = ',4(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICAPSW,ICAPTY,IFORSW
   53   FORMAT('ICAPSW,ICAPTY,IFORSW = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *********************************
C               **  STEP 1--                   **
C               **  EXTRACT THE VARIABLE LIST  **
C               *********************************
C
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'QUTE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IMULT='OFF'
      INAME='QUADE TEST'
      MAXNA=100
      MINNVA=1
      MAXNVA=MAXSPN
      MINNA=1
      IFLAGE=1
      IFLAGM=0
      IF(IMULT.EQ.'ON')THEN
        IFLAGM=0
      ENDIF
      MINN2=2
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'QUTE')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,181)
  181   FORMAT('***** AFTER CALL DPPARS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,182)NQ,NUMVAR,IMULT
  182   FORMAT('NQ,NUMVAR,IMULT = ',2I8,2X,A4)
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
C               **********************************
C               **  STEP 3--                    **
C               **  CARRY OUT THE QUADE TEST    **
C               **********************************
C
      ISTEPN='3'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'QUTE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               *****************************************
C               **  STEP 3A--                          **
C               **  CASE 1: THREE RESPONSE VARIABLES   **
C               **          NO MATRIX, NO MULTIPLE     **
C               *****************************************
C
      IF(IMULT.EQ.'OFF')THEN
        ISTEPN='3A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'QUTE')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        ICOL=1
        NUMVA2=3
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              Y,X,XTEMP2,NS1,NS1,NS1,ICASE,
     1              IBUGA3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
        IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'QUTE')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5211)
 5211     FORMAT('***** FROM DPQUTE, AS WE ARE ABOUT TO CALL DPQUT2--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5212)NS1
 5212     FORMAT('NS1 = ',I8)
          CALL DPWRST('XXX','BUG ')
          DO5215I=1,NS1
            WRITE(ICOUT,5216)I,Y(I),X(I),XTEMP2(I)
 5216       FORMAT('I,Y(I),X(I),XTEMP2(I) = ',I8,3G15.7)
            CALL DPWRST('XXX','BUG ')
 5215     CONTINUE
        ENDIF
C
        CALL DPQUT2(Y,X,XTEMP2,NS1,IVARN1,IVARN2,
     1              DBLOCK,DTREAT,YRANK,RJ,QRANK,
     1              TEMP1,TEMP2,MAXNXT,
     1              STATVA,STATCD,PVAL,
     1              CUT0,CUT50,CUT75,CUT90,CUT95,CUT99,CUT999,
     1              ICAPSW,ICAPTY,IFORSW,IMULT,
     1              IBUGA3,ISUBRO,IERROR)
C
C               ***************************************
C               **  STEP 61--                        **
C               **  UPDATE INTERNAL DATAPLOT TABLES  **
C               ***************************************
C
        ISTEPN='61'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'QUTE')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IFLAGU='ON'
        IFRST=.TRUE.
        ILAST=.TRUE.
        CALL DPFRT5(STATVA,STATCD,PVAL,
     1              CUT0,CUT50,CUT75,CUT90,CUT95,
     1              CUT975,CUT99,CUT999,
     1              IFLAGU,IFRST,ILAST,
     1              IBUGA2,IBUGA3,ISUBRO,IERROR)
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'FRI2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPQUTE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)IFOUND,IERROR,STATVA,STATCD
 9016   FORMAT('IFOUND,IERROR,STATVA,STATCD = ',2(A4,2X),2G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPQUT2(Y,BLOCK,TREAT,N,IVARID,IVARI2,
     1                  DBLOCK,DTREAT,YRANK,RJ,QRANK,
     1                  TEMP1,TEMP2,MAXNXT,
     1                  STATVA,STATCD,PVAL,
     1                  CUT0,CUT50,CUT75,CUT90,CUT95,CUT99,CUT999,
     1                  ICAPSW,ICAPTY,IFORSW,IMULT,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--THIS ROUTINE CARRIES OUT QUADE'S TEST
C              NON-PARAMETRIC TWO-WAY ANOVA
C     EXAMPLE--QUADE TEST Y BLOCK TREAT
C     REFERENCE--CONOVER (1999), "PRACTICAL NONPARAMETRIC STATISTICS",
C                THIRD EDITION, WILEY, PP. 373-380.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2011/7
C     ORIGINAL VERSION--JULY      2011.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 IMULT
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
      CHARACTER*4 IVARID(*)
      CHARACTER*4 IVARI2(*)
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IOP
      CHARACTER*3 IATEMP
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION BLOCK(*)
      DIMENSION TREAT(*)
      DIMENSION RJ(*)
      DIMENSION QRANK(*)
      DIMENSION DBLOCK(*)
      DIMENSION DTREAT(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
C
      DOUBLE PRECISION YRANK(*)
C
      PARAMETER (NUMALP=8)
      REAL ALPHA(NUMALP)
C
      PARAMETER(NUMCLI=6)
      PARAMETER(MAXLIN=2)
      PARAMETER (MAXROW=50)
      CHARACTER*60 ITITLE
      CHARACTER*60 ITITLZ
      CHARACTER*1  ITITL9
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
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      DATA ALPHA/
     1 0.0, 50.0, 75.0, 90.0, 95.0, 97.5, 99.0, 99.9/
C
      ISUBN1='DPFR'
      ISUBN2='I2  '
C
      IERROR='NO'
      IWRITE='OFF'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'QUT2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPQUT2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N
   52   FORMAT('IBUGA3,ISUBRO,N = ',2(A4,2X),I8)
        CALL DPWRST('XXX','WRIT')
        DO56I=1,N
          WRITE(ICOUT,57)I,Y(I),BLOCK(I),TREAT(I)
   57     FORMAT('I,Y(I),BLOCK(I),TREAT(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
C
      MAXNX2=MAXNXT
      CALL DPQUT3(Y,BLOCK,TREAT,N,
     1            DBLOCK,DTREAT,RJ,TEMP1,TEMP2,QRANK,
     1            YRANK,
     1            MAXNXT,MAXNX2,
     1            STATVA,STATCD,PVAL,
     1            NBLOCK,NTREAT,NUMDF1,NUMDF2,
     1            T1,T2,A1,C1,SSTR,SSTO,
     1            IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      CUT0=0.0
      CALL FPPF(.50,NUMDF1,NUMDF2,CUT50)
      CALL FPPF(.75,NUMDF1,NUMDF2,CUT75)
      CALL FPPF(.90,NUMDF1,NUMDF2,CUT90)
      CALL FPPF(.95,NUMDF1,NUMDF2,CUT95)
      CALL FPPF(.975,NUMDF1,NUMDF2,CUT975)
      CALL FPPF(.99,NUMDF1,NUMDF2,CUT99)
      CALL FPPF(.999,NUMDF1,NUMDF2,CUT999)
C
      ANB=REAL(NBLOCK)
      AK=REAL(NTREAT)
C
      IDF=(NBLOCK-1)*(NTREAT-1)
      CALL TPPF(0.95,REAL(IDF),T95)
      CALL TPPF(0.975,REAL(IDF),T975)
      CALL TPPF(0.995,REAL(IDF),T995)
      TERM1=2.0*ANB*(SSTO - SSTR)/REAL(IDF)
      CONTRA=SQRT(TERM1)
      CONTR1=T95*CONTRA
      CONTR2=T975*CONTRA
      CONTR3=T995*CONTRA
C
      IOP='OPEN'
      IFLG1=1
      IFLG2=1
      IFLG3=0
      IFLG4=0
      IFLG5=0
      CALL DPAUFI(IOP,IFLG1,IFLG2,IFLG3,IFLG4,IFLG5,
     1            IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1            IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      WRITE(IOUNI1,2405)
 2405 FORMAT(4X,'RESPONSE',13X,'RANK',11X,'BLOCK',8X,'TREATMENT')
      DO2410I=1,N
        WRITE(IOUNI1,2411)Y(I),YRANK(I),BLOCK(I),TREAT(I)
 2411   FORMAT(1X,E15.7,F15.2,F15.2,F15.2)
 2410 CONTINUE
C
      WRITE(IOUNI2,2421)CONTRA
 2421 FORMAT(1X,'Contrast term:          ',E15.7)
      WRITE(IOUNI2,2422)CONTR1
 2422 FORMAT(1X,'Contrast term*t(0.95):  ',E15.7)
      WRITE(IOUNI2,2423)CONTR2
 2423 FORMAT(1X,'Contrast term*t(0.975): ',E15.7)
      WRITE(IOUNI2,2424)CONTR3
 2424 FORMAT(1X,'Contrast term*t(0.995): ',E15.7)
      WRITE(IOUNI2,2425)
 2425 FORMAT(10X,'I',10X,'J',8X,'R(I)-R(J)')
C
      DO2430I=1,NTREAT
        DO2439J=1,NTREAT
          IF(I.LT.J)THEN
            ADIFF=RJ(I)-RJ(J)
            IATEMP='   '
            IF(ABS(ADIFF).GE.CONTR1)IATEMP(1:1)='*'
            IF(ABS(ADIFF).GE.CONTR2)IATEMP(2:2)='*'
            IF(ABS(ADIFF).GE.CONTR3)IATEMP(3:3)='*'
            WRITE(IOUNI2,2437)I,J,ADIFF,IATEMP
 2437       FORMAT(3X,I8,3X,I8,5X,E15.7,A3)
          ENDIF
 2439   CONTINUE
 2430 CONTINUE
C
      IOP='CLOS'
      CALL DPAUFI(IOP,IFLG1,IFLG2,IFLG3,IFLG4,IFLG5,
     1            IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1            IBUGA3,ISUBRO,IERROR)
C
C               *****************************
C               **   STEP 42-              **
C               **   WRITE OUT THE TABLE   **
C               *****************************
C
      ISTEPN='42'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'QUT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               ******************************
C               **   STEP 43--              **
C               **   WRITE OUT EVERYTHING   **
C               **   FOR QUADE TEST      **
C               ******************************
C
      ISTEPN='43'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'QUT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPRINT.EQ.'OFF')GOTO9000
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
      ITITLE='Quade Two Factor Test'
      NCTITL=21
      ITITLZ=' '
      NCTITZ=0
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
      IF(IMULT.EQ.'OFF')THEN
C
        ICNT=ICNT+1
        ITEXT(ICNT)='First Group-ID Variable: '
        WRITE(ITEXT(ICNT)(26:29),'(A4)')IVARID(2)(1:4)
        WRITE(ITEXT(ICNT)(30:33),'(A4)')IVARI2(2)(1:4)
        NCTEXT(ICNT)=33
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
C
        ICNT=ICNT+1
        ITEXT(ICNT)='Second Group-ID Variable: '
        WRITE(ITEXT(ICNT)(27:30),'(A4)')IVARID(3)(1:4)
        WRITE(ITEXT(ICNT)(31:34),'(A4)')IVARI2(3)(1:4)
        NCTEXT(ICNT)=34
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
C
      ELSE
      ENDIF
C
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='H0: Treatments Have Identical Effects'
      NCTEXT(ICNT)=37
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: Treatments Do Not Have Identical Effects'
      NCTEXT(ICNT)=44
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
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
      ITEXT(ICNT)='Total Number of Observations:'
      NCTEXT(ICNT)=29
      AVALUE(ICNT)=REAL(N)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Blocks:'
      NCTEXT(ICNT)=17
      AVALUE(ICNT)=REAL(NBLOCK)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Treatments:'
      NCTEXT(ICNT)=21
      AVALUE(ICNT)=REAL(NTREAT)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='Test:'
      NCTEXT(ICNT)=5
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Quade Test Statistic:'
      NCTEXT(ICNT)=21
      AVALUE(ICNT)=STATVA
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Total Sum of Squares (A2):'
      NCTEXT(ICNT)=26
      AVALUE(ICNT)=SSTO
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Treatment Sum of Squares (B):'
      NCTEXT(ICNT)=29
      AVALUE(ICNT)=SSTR
      IDIGIT(ICNT)=NUMDIG
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
C
      NUMROW=ICNT
      DO4210I=1,NUMROW
        NTOT(I)=15
 4210 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
C
      ISTEPN='42A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'QUT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1            AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ITITLE=' '
      NCTITL=0
      ITITL9=' '
      NCTIT9=0
      ITITLE='Percent Points of the F Reference Distribution'
      NCTITL=46
      NUMLIN=1
      NUMROW=8
      NUMCOL=3
      ITITL2(1,1)='Percent Point'
      ITITL2(1,2)=' '
      ITITL2(1,3)='Value'
      NCTIT2(1,1)=13
      NCTIT2(1,2)=1
      NCTIT2(1,3)=5
C
      NMAX=0
      DO4221I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IF(I.EQ.2)NTOT(I)=5
        NMAX=NMAX+NTOT(I)
        IDIGIT(I)=NUMDIG
        ITYPCO(I)='NUME'
 4221 CONTINUE
      ITYPCO(2)='ALPH'
      IDIGIT(1)=1
      IDIGIT(3)=3
      DO4223I=1,NUMROW
        DO4225J=1,NUMCOL
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
            IF(I.EQ.1)THEN
              AMAT(I,J)=RND(CUT0,IDIGIT(J))
            ELSEIF(I.EQ.2)THEN
              AMAT(I,J)=RND(CUT50,IDIGIT(J))
            ELSEIF(I.EQ.3)THEN
              AMAT(I,J)=RND(CUT75,IDIGIT(J))
            ELSEIF(I.EQ.4)THEN
              AMAT(I,J)=RND(CUT90,IDIGIT(J))
            ELSEIF(I.EQ.5)THEN
              AMAT(I,J)=RND(CUT95,IDIGIT(J))
            ELSEIF(I.EQ.6)THEN
              AMAT(I,J)=RND(CUT975,IDIGIT(J))
            ELSEIF(I.EQ.7)THEN
              AMAT(I,J)=RND(CUT99,IDIGIT(J))
            ELSEIF(I.EQ.8)THEN
              AMAT(I,J)=RND(CUT999,IDIGIT(J))
            ENDIF
          ENDIF
 4225   CONTINUE
 4223 CONTINUE
C
      IWHTML(1)=150
      IWHTML(2)=50
      IWHTML(3)=150
      IWRTF(1)=2000
      IWRTF(2)=IWRTF(1)+500
      IWRTF(3)=IWRTF(2)+2000
      IFRST=.TRUE.
      ILAST=.TRUE.
C
      ISTEPN='42C'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'QUT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPDTA4(ITITL9,NCTIT9,
     1            ITITLE,NCTITL,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            ISUBRO,IBUGA3,IERROR)
C
      CDF1=CUT90
      CDF2=CUT95
      CDF3=CUT975
      CDF4=CUT99
C
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
C
      NMAX=0
      DO4321I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IF(I.EQ.1 .OR. I.EQ.2)NTOT(I)=7
        IF(I.EQ.3)NTOT(I)=17
        NMAX=NMAX+NTOT(I)
        IDIGIT(I)=3
        ITYPCO(I)='ALPH'
 4321 CONTINUE
      ITYPCO(3)='NUME'
      IDIGIT(1)=0
      IDIGIT(2)=0
      DO4323I=1,NUMROW
        DO4325J=1,NUMCOL
          NCVALU(I,J)=0
          IVALUE(I,J)=' '
          NCVALU(I,J)=0
          AMAT(I,J)=0.0
 4325   CONTINUE
 4323 CONTINUE
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
      IF(STATVA.GT.CUT90)IVALUE(1,4)='Reject H0'
      IF(STATVA.GT.CUT95)IVALUE(2,4)='Reject H0'
      IF(STATVA.GT.CUT975)IVALUE(3,4)='Reject H0'
      IF(STATVA.GT.CUT99)IVALUE(4,4)='Reject H0'
      AMAT(1,3)=RND(CUT90,IDIGIT(3))
      AMAT(2,3)=RND(CUT95,IDIGIT(3))
      AMAT(3,3)=RND(CUT975,IDIGIT(3))
      AMAT(4,3)=RND(CUT99,IDIGIT(3))
C
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
C
      ISTEPN='42E'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KRU2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPDTA4(ITITL9,NCTIT9,
     1            ITITLE,NCTITL,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            ISUBRO,IBUGA3,IERROR)
C
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'QUT2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPQUT2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9012)STATVA,STATCD,PVAL
 9012   FORMAT('STATVA,STATCD,PVAL = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPQUT3(Y,BLOCK,TREAT,N,
     1                  DBLOCK,DTREAT,RJ,TEMP1,TEMP2,QRANK,
     1                  YRANK,
     1                  MAXNXT,MAXNX2,
     1                  STATVA,STATCD,PVAL,
     1                  NBLOCK,NTREAT,NUMDF1,NUMDF2,
     1                  T1,T2,A1,C1,SSTR,SSTO,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--THIS ROUTINE CARRIES OUT QUADE'S TEST
C              NON-PARAMETRIC TWO-WAY ANOVA
C     EXAMPLE--QUADE TEST Y BLOCK TREAT
C     REFERENCE--CONOVER (1999), "PRACTICAL NONPARAMETRIC STATISTICS",
C                THIRD EDITION, WILEY, PP. 373-380.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2011/7
C     ORIGINAL VERSION--JULY      2011.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      DOUBLE PRECISION DA2
      DOUBLE PRECISION DB
      DOUBLE PRECISION SJ
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION BLOCK(*)
      DIMENSION TREAT(*)
      DIMENSION RJ(*)
      DIMENSION DBLOCK(*)
      DIMENSION DTREAT(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION QRANK(*)
      DOUBLE PRECISION YRANK(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPQU'
      ISUBN2='T3  '
      IERROR='NO'
      IWRITE='OFF'
C
      STATVA=CPUMIN
      STATCD=CPUMIN
      PVAL=CPUMIN
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'QUT3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPQUT3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N,MAXNXT
   52   FORMAT('IBUGA3,ISUBRO,N,MAXNXT = ',2(A4,2X),2I8)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,53)A1,C1,T1,T2
   53   FORMAT('A1,C1,T1,T2 = ',4G15.7)
        CALL DPWRST('XXX','WRIT')
        DO56I=1,N
          WRITE(ICOUT,57)I,Y(I),BLOCK(I),TREAT(I)
   57     FORMAT('I,Y(I),BLOCK(I),TREAT(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
C
C               ********************************************
C               **  STEP 11--                             **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'QUT3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      HOLD=Y(1)
      DO1135I=2,N
      IF(Y(I).NE.HOLD)GOTO1139
 1135 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1131)
 1131 FORMAT('***** ERROR FROM QUADE TEST--')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1133)HOLD
 1133 FORMAT('      THE RESPONSE VARIABLE HAS ALL ELEMENTS = ',G15.7)
      CALL DPWRST('XXX','WRIT')
      IERROR='YES'
      GOTO9000
 1139 CONTINUE
C
      HOLD=BLOCK(1)
      DO1235I=2,N
      IF(BLOCK(I).NE.HOLD)GOTO1239
 1235 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1131)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1231)HOLD
 1231 FORMAT('      THE FIRST FACTOR VARIABLE HAS ALL ELEMENTS = ',
     1       G15.7)
      CALL DPWRST('XXX','WRIT')
      IERROR='YES'
      GOTO9000
 1239 CONTINUE
C
      HOLD=TREAT(1)
      DO1335I=2,N
      IF(TREAT(I).NE.HOLD)GOTO1339
 1335 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1131)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1331)HOLD
 1331 FORMAT('      THE SECOND FACTOR VARIABLE HAS ALL ELEMENTS = ',
     1       G15.7)
      CALL DPWRST('XXX','WRIT')
      GOTO9000
 1339 CONTINUE
C
C               ******************************
C               **  STEP 2--                **
C               **  CARRY OUT CALCULATIONS  **
C               **  FOR QUADE TEST          **
C               ******************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'QUT3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C  STEP 2A: COMPUTE NUMBER OF DISTINCT BLOCKS AND TREATMENTS
C
      CALL DISTIN(BLOCK,N,IWRITE,DBLOCK,NBLOCK,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      IF(NBLOCK.GT.MAXNX2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1131)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1232)NBLOCK,MAXNX2
 1232     FORMAT('      THE NUMBER OF BLOCKS (',I8,') IS GREATER ',
     1           'THAN',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
      ENDIF
      CALL DISTIN(TREAT,N,IWRITE,DTREAT,NTREAT,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      IF(NTREAT.GT.MAXNX2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1131)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1237)NTREAT,MAXNX2
 1237     FORMAT('      THE NUMBER OF TREATMENTS (',I8,') IS GREATER ',
     1           'THAN ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
      ENDIF
C
C  STEP 2B: COMPUTE THE RANGES WITHIN EACH BLOCK
C
      ISTEPN='2B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'QUT3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO2010I=1,N
        YRANK(I)=-1.0D0
 2010 CONTINUE
C
      DO2110I=1,NBLOCK
        HOLD=DBLOCK(I)
        ICOUNT=0
        YMIN=CPUMAX
        YMAX=CPUMIN
        DO2120J=1,N
          IF(BLOCK(J).EQ.HOLD)THEN
            ICOUNT=ICOUNT+1
            RJ(ICOUNT)=Y(J)
            IF(RJ(ICOUNT).LT.YMIN)YMIN=RJ(ICOUNT)
            IF(RJ(ICOUNT).GT.YMAX)YMAX=RJ(ICOUNT)
          ENDIF
 2120   CONTINUE
        QRANK(I)=YMAX - YMIN
        CALL RANK(RJ,ICOUNT,IWRITE,TEMP1,TEMP2,MAXNX2,
     1            IBUGA3,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        ICOUNT=0
        DO2130J=1,N
          IF(BLOCK(J).EQ.HOLD)THEN
            ICOUNT=ICOUNT+1
            YRANK(J)=DBLE(TEMP1(ICOUNT))
          ENDIF
 2130   CONTINUE
 2110 CONTINUE
      CALL RANK(QRANK,NBLOCK,IWRITE,TEMP1,TEMP2,MAXNX2,IBUGA3,IERROR)
      DO2135I=1,NBLOCK
        QRANK(I)=TEMP1(I)
 2135 CONTINUE
C
      AFACT=REAL(NTREAT+1)/2.0
      DA2=0.0D0
      DO2140I=1,NBLOCK
        HOLD=DBLOCK(I)
        ICOUNT=0
        SJ=0.0D0
        DO2150J=1,N
          IF(BLOCK(J).EQ.HOLD)THEN
            SIJ=QRANK(I)*(YRANK(J) - AFACT)
            DA2=DA2 + DBLE(SIJ)**2
          ENDIF
 2150   CONTINUE
 2140 CONTINUE
C
      DB=0.0D0
      DO2160I=1,NTREAT
        HOLD=DTREAT(I)
        ICOUNT=0
        SJ=0.0D0
        DO2170J=1,N
          IF(TREAT(J).EQ.HOLD)THEN
            ITEMP=INT(BLOCK(J)+0.1)
            SIJ=QRANK(ITEMP)*(YRANK(J) - AFACT)
            SJ=SJ + DBLE(SIJ)
          ENDIF
 2170   CONTINUE
        DB=DB + SJ**2
 2160 CONTINUE
      DB=DB/DBLE(NBLOCK)
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'QUT3')THEN
        WRITE(ICOUT,2161)DA2,DB,AFACT
 2161   FORMAT('DA2,DB,AFACT = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        DO2180I=1,N
          WRITE(ICOUT,2182)I,Y(I),YRANK(I)
 2182     FORMAT('I,Y(I),YRANK(I) = ',I8,G15.7,F12.2)
          CALL DPWRST('XXX','BUG ')
 2180   CONTINUE
        DO2187I=1,NBLOCK
          WRITE(ICOUT,2188)I,QRANK(I)
 2188     FORMAT('I,QRANK(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
 2187   CONTINUE
      ENDIF
C
C  STEP 2C: NOW COMPUTE RANK SUMS FOR EACH TREATMENT
C
      ISTEPN='2C'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'QUT3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C  STEP 4: NOW COMPUTE VARIOUS QUANTITIES
C
      SSTO=REAL(DA2)
      SSTR=REAL(DB)
C
      IF(DA2.EQ.DB)THEN
      ELSE
        STATVA=(DBLE(NBLOCK) -1)*DB/(DA2 - DB)
        NUMDF1=NTREAT-1
        NUMDF2=(NBLOCK-1)*(NTREAT-1)
        CALL FCDF(STATVA,NUMDF1,NUMDF2,STATCD)
        PVAL=1.0 - STATCD
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'QUT3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPQUT3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9012)STATVA,STATCD,PVAL
 9012   FORMAT('STATVA,STATCD,PVAL = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPRAND(ICASRA,ISEED,ILOCNU,NUMSHA,
     1                  SHAPE1,SHAPE2,SHAPE3,SHAPE4,
     1                  SHAPE5,SHAPE6,SHAPE7,
     1                  IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--GENERATE RANDOM NUMBERS
C              FROM ONE OF THE FOLLOWING DISTRIBUTIONS--
C              1 ) UNIFORM
C              2 ) NORMAL
C              3 ) LOGISTIC
C              4 ) DOUBLE EXPONENTIAL
C              5 ) CAUCHY
C              6 ) TUKEY LAMBDA
C              7 ) LOGNORMAL
C              8 ) HALFNORMAL
C              9 ) T
C              10) CHI-SQUARED
C              11) F
C              12) EXPONENTIAL
C              13) GAMMA
C              14) BETA
C              15) WEIBULL
C              16) EXTREME VALUE TYPE 1
C              17) EXTREME VALUE TYPE 2
C              18) PARETO
C              19) BINOMIAL
C              20) GEOMETRIC
C              21) POISSON
C              22) NEGATIVE BINOMIAL
C              23) SEMI-CIRCULAR
C              24) TRIANGULAR
C              25) INVERSE GAUSSIAN    MAY 1990
C              26) WALD    MAY 1990
C              27) RECIPROCAL INVERSE GAUSSIAN    MAY 1990
C              28) FATIGUE LIFE    MAY 1990
C              29) GENERALIZED PARETO      DECEMBER   1993
C              30) POWER FUNCTION          APRIL      1995
C              31) HYPERGEOMETRIC          AUGUST     1995
C              32) NON-CENTRAL CHI-SQUARE  AUGUST     1995
C              33) NON-CENTRAL F           AUGUST     1995
C              34) DOUBLY NON-CENTRAL F    AUGUST     1995
C              35) FOLDED NORMAL           OCTOBER    1995
C              36) HALF-CAUCHY             OCTOBER    1995
C              37) NORMAL MIXTURE          MAY        1998
C              38) POWER LAW               JUNE       1998
C              39) GENERALIZED TUKEY-LAMBDA AUGUST    2001
C              40) INVERTED WEIBULL        SEPTEMBER  2001
C              41) DOUBLE WEIBULL          OCTOBER    2001
C              42) DOUBLE GAMMA            OCTOBER    2001
C              43) LOG    GAMMA            OCTOBER    2001
C              44) INVERTED GAMMA          OCTOBER    2001
C              45) COSINE                  OCTOBER    2001
C              46) ANGLIT                  OCTOBER    2001
C              47) HYPERBOLIC SECANT       OCTOBER    2001
C              48) ARCSIN                  OCTOBER    2001
C              49) LOG DOUBLE EXPONENTIAL  OCTOBER    2001
C              50) GENERALIZED EXTREM VALU OCTOBER    2001
C              51) EXPONENTIATED WEIBULL   OCTOBER    2001
C              52) GOMPERTZ                OCTOBER    2001
C              53) HALF-LOGISTIC           OCTOBER    2001
C              54) POWER EXPONENTIAL       OCTOBER    2001
C              55) ALPHA                   OCTOBER    2001
C              56) BRADFORD                OCTOBER    2001
C              57) RECIPROCAL              OCTOBER    2001
C              58) JOHNSON SB              OCTOBER    2001
C              59) JOHNSON SU              OCTOBER    2001
C              60) POWER NORMAL            OCTOBER    2001
C              61) LOG-LOGISTIC            OCTOBER    2001
C              62) GEOMETRIC EXTR EXPO     NOVEMBER   2001
C              63) POWER LOGNORMAL         NOVEMBER   2001
C              64) BETA-BINOMIAL           DECEMBER   2001
C              65) TWO-SIDED POWER         MAY        2002
C              66) BIWEIBULL               MAY        2002
C              66) LOGARITHMIC SERIES      AUGUST     2002
C              67) G-AND-H                 JANUARY    2003
C              68) SLASH                   JANUARY    2003
C              69) LANDAU                  APRIL      2003
C              70) INVERTED BETA           MAY        2003
C              71) ERROR (=SUBBOTIN        MAY        2003
C                         =EXPONENTIAL POWER
C                         =GENERAL ERROR)
C              72) TRAPEZOID               JUNE       2003
C              73) VON MISES               JUNE       2003
C              74) PARETO SECOND KIND      JUNE       2003
C              75) WRAPPED CAUCHY          JUNE       2003
C              76) GENERALIZED TRAPEZOID   JUNE       2003
C              77) TRUNCATED NORMAL        JULY       2003
C              78) CHI                     JULY       2003
C              79) FOLDED CAUCHY           JULY       2003
C              80) MIELKE'S BETA-KAPPA     JULY       2003
C              81) GENERALIZED EXPONENTIAL JULY       2003
C              82) TRUNCATED   EXPONENTIAL JULY       2003
C              83) GENERALIZED GAMMA       SEPTEMBER  2003
C              84) FOLDED T                NOVEMBER   2003
C              85) SKEWED NORMAL           NOVEMBER   2003
C              86) SKEWED T                NOVEMBER   2003
C              87) ZIPF                    NOVEMBER   2003
C                  (RENAME AS ZETA)        MAY        2006
C              88) GOMPERTZ-MAKEHAM        DECEMBER   2003
C              89) GENERALIZED INVERSE GAUSSIAN   DECEMBER   2003
C                  (NOT ACTIVATED YET)
C              90) LOG SKEWED NORMAL       MARCH      2004
C              91) LOG SKEWED T            MARCH      2004
C              92) NON-CENTRAL T           MARCH      2004
C              93) DOUBLY NON-CENTRAL T    MARCH      2004
C              94) GENERALIZED HALF-LOGISTIC  MARCH   2004
C              95) GENERALIZED LOGISTIC    MARCH      2004
C              96) POLYA                   MARCH      2004
C              97) HERMITE                 APRIL      2004
C              98) YULE                    APRIL      2004
C              99) WARING                  APRIL      2004
C             100) GENERALIZED WARING      APRIL      2004
C             101) NON-CENTRAL BETA        MAY        2004
C             102) DOUBLY NON-CENTRAL BETA MAY        2004
C             103) SKEW DOUBLE EXPONENTIAL JUNE       2004
C             104) ASYMMETRIC DOUBLE EXPONENTIAL   JUNE  2004
C             105) MAXWELL                 JUNE       2004
C             106) RAYLEIGH                JUNE       2004
C             107) MCLEISH                 AUGUST     2004
C             108) BESSEL I-FUNCTION       AUGUST     2004
C             109) BESSEL K-FUNCTION       AUGUST     2004 (NOT WORK)
C             110) GENERALIZED MCLEISH     SEPTEMBER  2004
C             111) HYPERBOLIC              SEPTEMBER  2004 (NOT WORK)
C             112) GENERALIZED LOGISTIC TYPE 5   FEBRUARY  2006
C             113) WAKEBY                  FEBRUARY  2006
C             114) BETA NORMAL             MARCH     2006
C             115) GENERALIZED LOGISTIC TYPE 2 MARCH 2006
C             116) GENERALIZED LOGISTIC TYPE 3 MARCH 2006
C             117) GENERALIZED LOGISTIC TYPE 4 MARCH 2006
C             118) ASYMMETRIC LOG DOUBLE EXPONENTIAL  MARCH  2006
C             119) BETA GEOMETRIC          MAY    2006
C             120) BOREL TANNER            MAY    2006
C             121) LAGRANGE POISSON        JUNE   2006
C             122) LEADS IN COIN TOSSING   JUNE   2006
C                  (DISCRETE ARCSINE)
C             123) MATCHING                JUNE   2006
C             124) CLASSICAL OCCUPANCY     JUNE   2006 (NOT ACTIVE)
C             125) LOG BETA                JUNE   2006
C             126) POLYA AEPPLI            JUNE   2006
C             127) LOST GAMES              JUNE   2006
C             128) NEYMAN TYPE A           JUNE   2006 (NOT ACTIVE)
C             129) DXG                     JUNE   2006 (NOT ACTIVE)
C             130) GENERALIZED LOGARITHMIC SERIES JUNE   2006
C             131) GENERALIZED NEGATIVE BINOMIAL  JULY   2006
C             132) GEETA                   JULY   2006
C             133) QUASI BINOMIAL TYPE I   JULY   2006
C             134) CONSUL                  AUGUST 2006
C             135) DISCRETE WEIBULL        NOVEMBER  2006
C             136) GENERALIZED LOST GAMES  NOVEMBER  2006
C             137) TRUNCATED GENERALIZED
C                  NEGATIVE BINOMIAL       JANUARY 2006
C             138) KATZ                    JANUARY   2007
C             139) TOPP AND LEONE          FEBRUARY 2007
C             140) GENERALIZED TOPP AND LEONE   FEBRUARY 2007
C             141) REFLECTED GENERALIZED TOPP AND LEONE  FEBRUARY 2007
C             142) LAGRANGE KATZ           FEBRUARY 2007 (NOT ACTIVE)
C             143) SLOPE                   SEPTEMBER 2007
C             144) OGIVE                   SEPTEMBER 2007
C             145) TWO-SIDED SLOPE         SEPTEMBER 2007
C             146) TWO-SIDED OGIVE         SEPTEMBER 2007
C             147) UNEVEN TWO-SIDED POWER  OCTOBER 2007
C             148) DOUBLY UNIFORM PARETO   OCTOBER 2007
C             149) BURR TYPE 1 (= UNIFORM) OCTOBER 2007
C             150) BURR TYPE 2             OCTOBER 2007
C             151) BURR TYPE 3             OCTOBER 2007
C             152) BURR TYPE 4             OCTOBER 2007
C             153) BURR TYPE 5             OCTOBER 2007
C             154) BURR TYPE 6             OCTOBER 2007
C             155) BURR TYPE 7             OCTOBER 2007
C             156) BURR TYPE 8             OCTOBER 2007
C             157) BURR TYPE 9             OCTOBER 2007
C             158) BURR TYPE 10            OCTOBER 2007
C             159) BURR TYPE 11            OCTOBER 2007
C             160) BURR TYPE 12            OCTOBER 2007
C             160) KUMARASWAMY             OCTOBER 2007
C             161) REFLECTED POWER         DECEMBER 2007
C             162) MUTH                    JANUARY 2008
C             163) LOGISTIC-EXPONENTIAL    FEBRUARY 2008
C             164) TRUNCATED PARETO        MARCH    2008
C             165) BRITTLE FRACTURE        MARCH    2008
C             166) 3-PARAMETER LOGISTIC-EXPONENTIAL  MARCH 2008
C             167) BOOTSTRAP INDEX         DECEMBER 1988
C             168) RANDOM PERMUTATION      DECEMBER 1988
C             169) RANDOM SUBSET           APRIL    2008
C             170) RANDOM K-SET OF N-SET   APRIL    2008
C             171) RANDOM COMPOSITION      APRIL    2008
C             172) KAPPA                   MAY      2008
C             173) PEARSON TYPE 3          MAY      2008
C             174) RANDOM PARTITION        JUNE     2008
C             175) RANDOM EQUIVALENCE RELA JUNE     2008
C             176) RANDOM YOUNG TABLEAUX   JULY     2008
C             177) END EFFECTS WEIBULL     JULY     2010
C             178) BRITTLE FIBER WEIBULL   AUGUST   2010
C             179) ARCTANGENT              JANUARY  2011
C             180) SINE                    MARCH    2013
C             181) EXCLUSION ZONE UNIFORM  MARCH    2013
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
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--APRIL     1978.
C     UPDATED         --MAY       1978.
C     UPDATED         --JUNE      1978.
C     UPDATED         --MAY       1978.
C     UPDATED         --NOVEMBER  1978.
C     UPDATED         --JUNE      1981.
C     UPDATED         --SEPTEMBER 1981.
C     UPDATED         --OCTOBER   1981.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --DECEMBER  1988. DISCRETE UNIFORM
C     UPDATED         --DECEMBER  1988. BOOTSTRAP INDEX
C     UPDATED         --DECEMBER  1988. RANDOM PERMUTATION
C     UPDATED         --JANUARY   1989. JACKNIFE INDEX
C     UPDATED         --MAY       1993. MINMAX FOR EV1/EV2/WEIB DIST.
C     UPDATED         --OCTOBER   1993. JACKNIFE INDEX TO DPMATC
C     UPDATED         --DECEMBER  1993. GENERALIZED PARETO
C     UPDATED         --MARCH     1994. DPCOS2.INC
C     UPDATED         --APRIL     1995. POWER FUNCTION
C     UPDATED         --AUGUST    1995. HYPERGEOMETRIC, NON-CENTRAL
C                                       CHI-SQUARE, SINGLY AND DOUBLY
C                                       NON-CENTRAL F
C     UPDATED         --MAY       1998. NORMAL MIXTURE
C     UPDATED         --JUNE      1998. POWER LAW
C     UPDATED         --AUGUST    2001. GENERALIZED LAMBDA
C     UPDATED         --SEPTEMBER 2001. INVERTED WEIBULL
C     UPDATED         --OCTOBER   2001. DOUBLE WEIBULL
C     UPDATED         --OCTOBER   2001. DOUBLE GAMMA
C     UPDATED         --OCTOBER   2001. LOG GAMMA
C     UPDATED         --OCTOBER   2001. INVERTED GAMMA
C     UPDATED         --OCTOBER   2001. COSINE
C     UPDATED         --OCTOBER   2001. ANGLIT
C     UPDATED         --OCTOBER   2001. HYPERBOLIC SECANT
C     UPDATED         --OCTOBER   2001. ARCSIN
C     UPDATED         --OCTOBER   2001. LOG DOUBLE EXPONENTIAL
C     UPDATED         --OCTOBER   2001. GENERALIZED EXTREME VALUE
C     UPDATED         --OCTOBER   2001. EXPONENTIATED WEIBULL
C     UPDATED         --OCTOBER   2001. GOMPERTZ
C     UPDATED         --OCTOBER   2001. HALF-LOGISTIC
C     UPDATED         --OCTOBER   2001. POWER EXPONENTIAL
C     UPDATED         --OCTOBER   2001. ALPHA
C     UPDATED         --OCTOBER   2001. BRADFORD
C     UPDATED         --OCTOBER   2001. RECIPROCAL
C     UPDATED         --OCTOBER   2001. JOHNSON SU
C     UPDATED         --OCTOBER   2001. JOHNSON SB
C     UPDATED         --OCTOBER   2001. POWER NORMAL
C     UPDATED         --OCTOBER   2001. LOG-LOGISTIC
C     UPDATED         --NOVEMBER  2001. GEOMETRIC EXTREME EXPO
C     UPDATED         --NOVEMBER  2001. POWER LOGNORMAL
C     UPDATED         --DECEMBER  2001. BETA-BINOMIAL
C     UPDATED         --MAY       2002. TWO-SIDED POWER
C     UPDATED         --MAY       2002. BIWEIBULL
C     UPDATED         --AUGUST    2002. LOGARITHMIC SERIES
C     UPDATED         --JANUARY   2003. G-AND-H, SLASH
C     UPDATED         --APRIL     2003. ADD SHAPE PARAMETER FOR
C                                       LOGNORMAL
C     UPDATED         --APRIL     2003. LANDAU
C     UPDATED         --MAY       2003. INVERTED BETA
C     UPDATED         --MAY       2003. ERROR (=SUBBOTIN=EXPOENTIAL
C                                       POWER=GENERAL ERROR)
C     UPDATED         --JUNE      2003. TRAPEZOID, VON MISES,
C                                       PARETO SECOND KIND,
C                                       WRAPPED CAUCHY,
C                                       GENERALIZED TRAPEZOID
C     UPDATED         --JULY      2003. CHI, TRUNCATED NORMAL,
C                                       FOLDED CAUCHY,
C                                       MIELKE'S BETA-KAPPA,
C                                       GENERALIZED EXPONENTIAL,
C                                       TRUNCATED EXPONENTIAL
C     UPDATED         --SEPTEMBER 2003. GENERALIZED GAMMA
C     UPDATED         --NOVEMBER  2003. FOLDED T
C     UPDATED         --NOVEMBER  2003. SKEWED NORMAL
C     UPDATED         --NOVEMBER  2003. SKEWED T
C     UPDATED         --NOVEMBER  2003. ZIPF
C     UPDATED         --DECEMBER  2003. GOMPERTZ-MAKEHAM
C     UPDATED         --DECEMBER  2003. GENERALIZED INVERSE GAUSSIAN
C                                       (NOT IMPLEMENTED YET)
C     UPDATED         --MARCH     2004. LOG SKEWED NORMAL
C     UPDATED         --MARCH     2004. LOG SKEWED T
C     UPDATED         --MARCH     2004. ALTERNATE DEFINITION OF
C                                       GEOMETRIC
C     UPDATED         --MARCH     2004. NON-CENTRAL T
C     UPDATED         --MARCH     2004. DOUBLY NON-CENTRAL T
C     UPDATED         --MARCH     2004. GENERALIZED HALF-LOGISTIC
C     UPDATED         --MARCH     2004. GENERALIZED LOGISTIC
C     UPDATED         --MARCH     2004. POLYA
C     UPDATED         --APRIL     2004. HERMITE
C     UPDATED         --APRIL     2004. YULE
C     UPDATED         --APRIL     2004. WARING
C     UPDATED         --APRIL     2004. GENERALIZED WARING
C     UPDATED         --MAY       2004. NON-CENTRAL BETA
C     UPDATED         --MAY       2004. DOUBLY NON-CENTRAL BETA
C     UPDATED         --MAY       2004. REAL VALUES FOR CHI-SQUARE
C                                       RANDOM NUMBERS
C     UPDATED         --MAY       2004. NON-CENTRAL CHI-SQUARE AS
C                                       SEPARATE SUBROUTINE
C     UPDATED         --JUNE      2004. SKEW DOUBLE EXPONENTIAL
C     UPDATED         --JUNE      2004. ASYMMETRIC DOUBLE EXPONENTIAL
C     UPDATED         --JUNE      2004. ARGUMENT LIST TO GEPRAN
C     UPDATED         --JUNE      2004. MAXWELL, RAYLEIGH
C     UPDATED         --JULY      2004. ALTERNATE DEFINITIION FOR
C                                       GOMPERTZ-MAKEHAM
C     UPDATED         --OCTOBER   2004. FOR PARETO, TREAT A AS A
C                                       SHAPE PARAMETER
C     UPDATED         --JULY      2005. CALL LIST TO LGARAN AND SNRAN
C     UPDATED         --FEBRUARY  2006. GENERALIZED LOGISTIC TYPE 5
C     UPDATED         --FEBRUARY  2006. WAKEBY
C     UPDATED         --FEBRUARY  2006. ARGUMENT LIST TO GLDRAN
C     UPDATED         --MARCH     2006. BETA-NORMAL
C     UPDATED         --MARCH     2006. GENERALIZED LOGISTIC TYPE 2
C     UPDATED         --MARCH     2006. GENERALIZED LOGISTIC TYPE 3
C     UPDATED         --MARCH     2006. GENERALIZED LOGISTIC TYPE 4
C     UPDATED         --MARCH     2006. ASYMMETRIC DOUBLE EXPONENTIAL
C     UPDATED         --MAY       2006. BETA GEOMETRIC
C     UPDATED         --MAY       2006. RENAME ZIPF AS ZETA
C     UPDATED         --MAY       2006. BOREL-TANNER
C     UPDATED         --MAY       2006. BETA-NEGATIVE BINOMIAL AS
C                                       SYNOMYM FOR GENERALIZED
C                                       WARING
C     UPDATED         --JUNE      2006. LAGRANGE-POISSON
C     UPDATED         --JUNE      2006. LEADS IN COIN TOSSING
C     UPDATED         --JUNE      2006. MATCHING
C     UPDATED         --JUNE      2006. CLASSICAL OCCUPANCY
C     UPDATED         --JUNE      2006. LOG BETA
C     UPDATED         --JUNE      2006. GENERALIZED LOGARITHMIC
C                                       SERIES
C     UPDATED         --JULY      2006. GENERALIZED NEGATIVE
C                                       BINOMIAL
C     UPDATED         --JULY      2006. GEETA
C     UPDATED         --JULY      2006. QUASI BINOMIAL TYPE 1
C     UPDATED         --AUGUST    2006. CONSUL
C     UPDATED         --AUGUST    2006. LAGRANGE KATZ
C     UPDATED         --SEPTEMBER 2006. KATZ
C     UPDATED         --OCTOBER   2006. FRACTIONAL DEGREES OF
C                                       FREEDOM FOR T DISTRIBUTION
C     UPDATED         --NOVEMBER  2006. DISCRETE WEIBULL
C     UPDATED         --NOVEMBER  2006. GENERALIZED LOST GAMES
C     UPDATED         --FEBRUARY  2007. TOPP AND LEONE
C     UPDATED         --FEBRUARY  2007. GENERALIZED TOPP AND LEONE
C     UPDATED         --FEBRUARY  2007. REFLECTED GENERALIZED TOPP
C                                       AND LEONE
C     UPDATED         --SEPTEMBER 2007. SLOPE
C     UPDATED         --SEPTEMBER 2007. OGIVE
C     UPDATED         --SEPTEMBER 2007. TWO-SIDED SLOPE
C     UPDATED         --SEPTEMBER 2007. TWO-SIDED OGIVE
C     UPDATED         --OCTOBER   2007. BURR TYPE 1 (= UNIFORM)
C     UPDATED         --OCTOBER   2007. BURR TYPE 2
C     UPDATED         --OCTOBER   2007. BURR TYPE 3
C     UPDATED         --OCTOBER   2007. BURR TYPE 4
C     UPDATED         --OCTOBER   2007. BURR TYPE 5
C     UPDATED         --OCTOBER   2007. BURR TYPE 6
C     UPDATED         --OCTOBER   2007. BURR TYPE 7
C     UPDATED         --OCTOBER   2007. BURR TYPE 8
C     UPDATED         --OCTOBER   2007. BURR TYPE 9
C     UPDATED         --OCTOBER   2007. BURR TYPE 10
C     UPDATED         --OCTOBER   2007. BURR TYPE 11
C     UPDATED         --OCTOBER   2007. BURR TYPE 12
C     UPDATED         --OCTOBER   2007. DOUBLY PARETO UNIFORM
C     UPDATED         --OCTOBER   2007. KUMARASWAMY
C     UPDATED         --DECEMBER  2007. REFLECTED POWER
C     UPDATED         --JANUARY   2008. MUTH
C     UPDATED         --FEBRUARY  2008. LOGISTIC-EXPONENTIAL
C     UPDATED         --FEBRUARY  2008. TRUNCATED PARETO
C     UPDATED         --MARCH     2008. BRITTLE FRACTURE
C     UPDATED         --MARCH     2008. 3-PARAMETER LOGISTIC-EXPONENTIAL
C     UPDATED         --APRIL     2008. RANDOM SUBSET
C     UPDATED         --APRIL     2008. RANDOM K-SET OF N-SET
C     UPDATED         --APRIL     2008. RANDOM COMPOSITION
C     UPDATED         --MAY       2008. RENAME CALL FOR MIELKE'S
C                                       BETA-KAPPA, BETA PARAMETER IS
C                                       ACTUALLY A SCALE PARAMETER
C     UPDATED         --MAY       2008. KAPPA
C     UPDATED         --MAY       2008. PEARSON TYPE 3
C     UPDATED         --MAY       2008. RANDOM PARTITION
C     UPDATED         --JUNE      2008. RANDOM EQUIVALENCE RELATION
C     UPDATED         --JULY      2008. RANDOM YOUNG TABLEAUX
C     UPDATED         --JULY      2008. MODIFY GIG PARAMETERIZATION
C     UPDATED         --SEPTEMBER 2009. USE EXTPA1
C     UPDATED         --SEPTEMBER 2009. EXTRACT MOST OF THE CALLS
C                                       TO RANDOM NUMBER ROUTINES TO
C                                       "DPRAN2" TO ENABLE EASIER
C                                       CALLING BY OTHER ROUTINES
C                                       (E.G., THE BOOTSTRAP COMMAND)
C     UPDATED         --JULY      2010. END EFFECTS WEIBULL
C     UPDATED         --AUGUST    2010. BRITTLE FIBER WEIBULL
C     UPDATED         --JANUARY   2011. ARCTANGENT
C     UPDATED         --MARCH     2013. SINE
C     UPDATED         --MARCH     2013. EXCLUSION ZONE UNIFORM
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASRA
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGQ
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 NEWNAM
      CHARACTER*4 NEWCOL
      CHARACTER*4 MESSAG
      CHARACTER*4 ICASEQ
      CHARACTER*4 IHWUSE
      CHARACTER*4 IHP
      CHARACTER*4 IHP2
      CHARACTER*4 ILEFT
      CHARACTER*4 ILEFT2
      CHARACTER*4 IHRIGH
      CHARACTER*4 IHRIG2
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      CHARACTER*60 IDIST
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOS2.INC'
      INCLUDE 'DPCOST.INC'
C
      REAL    TEMP3(MAXOBV)
      REAL    TEMP4(MAXOBV)
      INTEGER ITEMP1(MAXOBV)
      INTEGER ITEMP2(MAXOBV)
      INTEGER ITEMP4(MAXOBV)
      INCLUDE 'DPCOZI.INC'
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (IGARBG(IIGAR1),ITEMP1)
      EQUIVALENCE (IGARBG(IIGAR2),ITEMP2)
      EQUIVALENCE (IGARBG(IIGAR4),ITEMP4)
      EQUIVALENCE (GARBAG(IGARB1),TEMP3)
      EQUIVALENCE (GARBAG(IGARB2),TEMP4)
C
      COMMON/NIJWIL/NLAST,KLAST
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----DATA STATEMENTS-------------------------------------------------
C
CCCCC DATA EPS/0.000001/
CCCCC DATA ALAMLG/0.00001/
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPRA'
      ISUBN2='ND  '
      IFOUND='NO'
      IERROR='NO'
      IFOUND='YES'
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
      NS2=0
      RANLOC=0.0
      RANSCA=1.0
C
C               ***********************************************
C               **  TREAT THE RANDOM NUMBER GENERATION CASE  **
C               **       1) FOR A FULL VARIABLE, OR          **
C               **       2) FOR PART OF A VARIABLE.          **
C               ***********************************************
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RAND')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPRAND--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,IBUGQ
   52   FORMAT('IBUGA3,IBUGQ = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASRA,ISEED,ILOCNU,MINMAX
   53   FORMAT('ICASRA,ISEED,ILOCNU,MINMAX = ',A4,3I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               **********************************
C               **  STEP 1--                    **
C               **  INITIALIZE SOME VARIABLES.  **
C               **********************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RAND')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NEWNAM='NO'
      NEWCOL='NO'
C
C               *******************************************************
C               **  STEP 2--                                         **
C               **  CHECK FOR THE PROPER NUMBER OF INPUT ARGUMENTS.  **
C               *******************************************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RAND')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      MINNA=3
      MAXNA=100
      CALL CHECKA(NUMARG,MINNA,MAXNA,IANS,IWIDTH,ISUBN1,ISUBN2,
     1IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
C               ********************************************************
C               **  STEP 3--                                           *
C               **  EXAMINE THE LEFT-HAND SIDE--                       *
C               **  IS THE PARAMETER OR VARIABLE NAME TO LEFT OF =     *
C               **  SIGN ALREADY IN THE NAME LIST?                     *
C               **  NOTE THAT     ILEFT      IS THE NAME OF THE        *
C               **  VARIABLE ON THE LEFT.                              *
C               **  NOTE THAT     ILISTL    IS THE LINE IN THE TABLE   *
C               **  OF THE NAME ON THE LEFT.                           *
C               **  NOTE THAT     ICOLL    IS THE DATA COLUMN (1 TO 12)*
C               **  FOR THE NAME OF THE LEFT.                          *
C               ********************************************************
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RAND')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ILEFT=IHARG(1)
      ILEFT2=IHARG2(1)
      DO310I=1,NUMNAM
        I2=I
        IF(ILEFT.EQ.IHNAME(I).AND.ILEFT2.EQ.IHNAM2(I).AND.
     1     IUSE(I).EQ.'P')THEN
           ILISTL=I2
           GOTO330
        ELSEIF(ILEFT.EQ.IHNAME(I).AND.ILEFT2.EQ.IHNAM2(I).AND.
     1     IUSE(I).EQ.'V')THEN
           ILISTL=I2
           ICOLL=IVALUE(ILISTL)
           NLEFT=IN(ILISTL)
           GOTO390
        ENDIF
  310 CONTINUE
      NEWNAM='YES'
      ILISTL=NUMNAM+1
      IF(ILISTL.GT.MAXNAM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,321)
  321   FORMAT('***** ERROR IN DPRAND--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,322)
  322   FORMAT('      THE NUMBER OF VARIABLE AND/OR PARAMETER NAMES')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,323)MAXNAM
  323   FORMAT('      HAS JUST EXCEEDED THE MAXIMUM ALLOWABLE ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,324)
  324   FORMAT('      SUGGESTED ACTION--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,325)
  325   FORMAT('      ENTER      STATUS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,326)
  326   FORMAT('      TO FIND OUT THE FULL LIST OF USED NAMES, AND')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,327)
  327   FORMAT('      THEN REDEFINE (REUSE) SOME OF THE ALREADY USED ',
     1         'NAMES.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
  330 CONTINUE
      NLEFT=0
      ICOLL=NUMCOL+1
      IF(ICOLL.GT.MAXCOL)THEN
        WRITE(ICOUT,321)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,342)
  342   FORMAT('      THE NUMBER OF DATA COLUMNS HAS JUST EXCEEDED')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,343)MAXCOL
  343   FORMAT('      THE MAXIMUM ALLOWABLE ',I8,'.  SUGGESTED ',
     1         'ACTION--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,325)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,326)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,347)
  347   FORMAT('      THEN DELETE SOME OF THE ALREADY USED NAMES.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
  390 CONTINUE
C
C               *******************************************************
C               **  STEP 4--                                         **
C               **  CHECK THAT THE INPUT CASE (ICASRA)               **
C               **  IS ONE OF THE ALLOWABLE 100+ DISTRIBUTIONS       **
C               *******************************************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RAND')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               *****************************************
C               **  STEP 6--                           **
C               **  CHECK TO SEE THE TYPE SUBCASE      **
C               **  (BASED ON THE QUALIFIER)           **
C               **    1) UNQUALIFIED (THAT IS, FULL);  **
C               **    2) SUBSET/EXCEPT; OR             **
C               **    3) FOR.                          **
C               *****************************************
C
      ISTEPN='6'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RAND')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     APRIL 2008: CHECK FOR "SUBSET" CONFLICT WITH "RANDOM SUBSET"
C                 CASE.
C
C     MAY 2008: RANDOM PARTITION AND RANDOM EQUIVALENCE CLASS
C               COMMANDS DO NOT USE THE TYPICAL
C               "FOR I = 1 1 N" CLAUSE.
C
C     JULY 2008: RANDOM YOUNG TABLEAUX USES SYNTAX:
C
C                LET N = <VALUE>
C                LET Y = RANDOM YOUNG TABLEAUX LAMBDA
C
C                WHERE LAMBDA IS AN ARRAY DEFINING THE PARTITION
C
      IF(ICASRA.EQ.'RANP' .OR. ICASRA.EQ.'RANE')GOTO750
C
      IF(ICASRA.EQ.'RAYT')THEN
        IHRIGH=IHARG(6)
        IHRIG2=IHARG2(6)
        IHWUSE='V'
        MESSAG='YES'
        CALL CHECKN(IHRIGH,IHRIG2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        ICOLR=IVALUE(ILOCV)
        NLEFT=IN(ILOCV)
        J=0
        DO701I=1,NLEFT
          J=J+1
          IJ=MAXN*(ICOLR-1)+I
          IF(ICOLR.LE.MAXCOL)TEMP4(J)=V(IJ)
          IF(ICOLR.EQ.MAXCP1)TEMP4(J)=PRED(I)
          IF(ICOLR.EQ.MAXCP2)TEMP4(J)=RES(I)
          IF(ICOLR.EQ.MAXCP3)TEMP4(J)=YPLOT(I)
          IF(ICOLR.EQ.MAXCP4)TEMP4(J)=XPLOT(I)
          IF(ICOLR.EQ.MAXCP5)TEMP4(J)=X2PLOT(I)
          IF(ICOLR.EQ.MAXCP6)TEMP4(J)=TAGPLO(I)
  701   CONTINUE
        GOTO750
      ENDIF
C
      ICASEQ='FULL'
      ILOCQ=NUMARG+1
      IF(NUMARG.LT.1)GOTO670
      DO610J=1,NUMARG
        J1=J
        IF(ICASRA.NE.'SUBS')THEN
          IF(IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ')GOTO620
        ELSE
          IF(IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  '.AND.
     1       IHARG(J+1).EQ.'SUBS'.AND.IHARG2(J+1).EQ.'ET  ')THEN
            J1=J+1
            GOTO620
          ENDIF
        ENDIF
        IF(IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  ')GOTO620
        IF(IHARG(J).EQ.'FOR '.AND.IHARG2(J).EQ.'    ')GOTO630
  610 CONTINUE
      GOTO680
C
  620 CONTINUE
      ICASEQ='SUBS'
      ILOCQ=J1
      GOTO680
C
  630 CONTINUE
      ICASEQ='FOR'
      ILOCQ=J1
      GOTO680
C
  670 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,671)
  671 FORMAT('***** INTERNAL ERROR IN DPRAND')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,672)
  672 FORMAT('      AT BRANCH POINT 5081--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,673)
  673 FORMAT('      NUMARG LESS THAN 1 EVEN THOUGH')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,674)
  674 FORMAT('      NUMARG HAD PREVIOUSLY PASSED THIS TEST')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,675)NUMARG
  675 FORMAT('      ONCE ALREADY.  VALUE OF NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,676)
  676 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,677)(IANS(I),I=1,MIN(80,IWIDTH))
  677   FORMAT(80A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GOTO9000
C
  680 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RAND')THEN
        WRITE(ICOUT,681)NUMARG,ILOCQ,ICASEQ
  681   FORMAT('NUMARG,ILOCQ,ICASEQ = ',2I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ******************************************************
C               **  STEP 7--                                        **
C               **  BRANCH TO THE APPROPRIATE SUBCASE               **
C               **  (BASED ON THE QUALIFIER);                       **
C               **  DETERMINE THE NUMBER (= NRAN)                   **
C               **  OF RANDOM NUMBERS TO BE GENERATED.              **
C               **  NOTE THAT THE VARIABLE NIISUB                   **
C               **  IS THE LENGTH OF THE RESULTING                  **
C               **  VARIABLE ISUB(.).                               **
C               **  NOTE THAT DPFOR AUTOMATICALLY EXTENDS           **
C               **  THE INPUT LENGTH OF ISUB(.) IF NECESSARY.       **
C               **  (HENCE THE REDEFINITION OF NIISUB TO NINEW      **
C               **  AFTER THE CALL TO DPFOR.                        **
C               ******************************************************
C
      ISTEPN='7'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RAND')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC OCTOBER 1993.  JACKNIFE INDEX TO DPMATC.
CCCCC IF(ICASRA.EQ.'JACK')GOTO1280
      IF(ICASEQ.EQ.'SUBS')THEN
        NIISUB=MAXN
        CALL DPSUBS(NIISUB,ILOCS,NS,IBUGQ,IERROR)
        NRAN=NS
      ELSEIF(ICASEQ.EQ.'FOR')THEN
        IF(NEWNAM.EQ.'NO')NIISUB=NLEFT
        IF(NEWNAM.EQ.'YES')NIISUB=MAXN
        CALL DPFOR(NIISUB,NINEW,IROW1,IROWN,
     1             NLOCAL,ILOCS,NS,IBUGQ,IERROR)
        NIISUB=NINEW
        NRAN=NS
      ELSE
        IF(NEWNAM.EQ.'NO')NIISUB=NLEFT
        IF(NEWNAM.EQ.'YES')NIISUB=MAXN
        DO715I=1,NIISUB
          ISUB(I)=1
  715   CONTINUE
        NRAN=NIISUB
      ENDIF
C
  750 CONTINUE
C
      IF(NRAN.LT.1)THEN
        WRITE(ICOUT,321)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,762)
  762   FORMAT('      THE SPECIFIED NUMBER OF RANDOM ITEMS MUST BE ',
     1         '1 OR LARGER.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,769)NRAN
  769   FORMAT('      THE SPECIFIED NUMBER OF ITEMS =  ',I8)
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C               ******************************************
C               **  STEP 8--                            **
C               **  GENERATE    NRAN    RANDOM NUMBERS  **
C               **  FROM THE SPECIFIED DISTRIBUTION.    **
C               **  STORE THEM TEMPORARILY IN           **
C               **  THE VECTOR Y(.).                    **
C               ******************************************
C
      ISTEPN='8'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RAND')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     MARCH 2013: EXCLUSION ZONE UNIFORM IS A SPECIAL CASE THAT
C                 IS NOT RECOGNIZED IN EXTDIS AND EXTPA1.
C
      IF(NUMSHA.GE.1)THEN
        CALL EXTPA1(ICASRA,IDIST,A,B,
     1              SHAPE1,SHAPE2,SHAPE3,SHAPE4,
     1              SHAPE5,SHAPE6,SHAPE7,
     1              IADEDF,IGEPDF,IMAKDF,IBEIDF,
     1              ILGADF,ISKNDF,IGLDDF,IBGEDF,
     1              IGETDF,ICONDF,IGOMDF,IKATDF,
     1              IGIGDF,IGEODF,
     1              IBFWLI,IEEWLI,
     1              ISUBRO,IBUGA3,IERROR)
      ENDIF
C
      IF(ICASRA.EQ.'UNEX')THEN
        IHP='A   '
        IHP2='    '
        IHWUSE='P'
        MESSAG='NO'
        CALL CHECKN(IHP,IHP2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
        IF(IERROR.EQ.'YES')THEN
          A=0.0
        ELSE
          A=VALUE(ILOCP)
        ENDIF
C
        IHP='B   '
        IHP2='    '
        IHWUSE='P'
        MESSAG='NO'
        CALL CHECKN(IHP,IHP2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
        IF(IERROR.EQ.'YES')THEN
          B=1.0
        ELSE
          B=VALUE(ILOCP)
        ENDIF
C
        IHP='DIAM'
        IHP2='    '
        IHWUSE='P'
        MESSAG='YES'
        CALL CHECKN(IHP,IHP2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        SHAPE1=VALUE(ILOCP)
      ENDIF
C
      IF(ICASRA.EQ.'SUBS')THEN
        CALL RANSUB(NRAN,ISEED,Y)
      ELSEIF(ICASRA.EQ.'KNSE')THEN
        IHP='N   '
        IHP2='    '
        IHWUSE='P'
        MESSAG='YES'
        CALL CHECKN(IHP,IHP2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        NPAR=INT(VALUE(ILOCP)+0.5)
C
        IF(NRAN.GT.NPAR)THEN
          WRITE(ICOUT,321)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3862)
 3862     FORMAT('      FOR THE  K-SET OF N-SET    CASE, THE VALUE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3863)
 3863     FORMAT('      OF K MUST BE LESS THAN OR EQUAL TO THE VALUE ',
     1           'OF N.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8197)
 8197     FORMAT('      SUCH WAS NOT THE CASE HERE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3868)NRAN
 3868     FORMAT('      THE SPECIFIED VALUE OF K  =  ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3869)NPAR
 3869     FORMAT('      THE SPECIFIED VALUE OF N  =  ',I8)
          CALL DPWRST('XXX','BUG ')
          GOTO9000
        ENDIF
        CALL RANKSB(NRAN,NPAR,ISEED,Y,ITEMP1)
      ELSEIF(ICASRA.EQ.'RANC')THEN
        IHP='N   '
        IHP2='    '
        IHWUSE='P'
        MESSAG='YES'
        CALL CHECKN(IHP,IHP2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        NPAR=INT(VALUE(ILOCP)+0.5)
C
        IF(NPAR.LT.1)THEN
          WRITE(ICOUT,321)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3872)
 3872     FORMAT('      FOR THE RANDOM COMPOSITION CASE, THE VALUE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3873)
 3873     FORMAT('      OF N MUST BE AT LEAST 1.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8197)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3879)NPAR
 3879     FORMAT('      THE SPECIFIED VALUE OF N  =  ',I8)
          CALL DPWRST('XXX','BUG ')
          GOTO9000
        ENDIF
C
        IF(NRAN.LT.1 .OR. NRAN.GT.NPAR)THEN
          WRITE(ICOUT,321)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3882)
 3882     FORMAT('      FOR THE RANDOM COMPOSITION CASE, THE VALUE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3883)
 3883     FORMAT('      OF K MUST BE LESS THAN OR EQUAL TO THE VALUE ',
     1           'OF N')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3884)
 3884     FORMAT('      AND GREATER THAN OR EQUAL TO ONE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8197)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3888)NRAN
 3888     FORMAT('      THE SPECIFIED VALUE OF K  =  ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3889)NPAR
 3889     FORMAT('      THE SPECIFIED VALUE OF N  =  ',I8)
          CALL DPWRST('XXX','BUG ')
          GOTO9000
        ENDIF
        CALL RANCOM(NRAN,NPAR,ISEED,Y,ITEMP1)
      ELSEIF(ICASRA.EQ.'RANP')THEN
        IHP='N   '
        IHP2='    '
        IHWUSE='P'
        MESSAG='YES'
        CALL CHECKN(IHP,IHP2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        NPAR=INT(VALUE(ILOCP)+0.5)
C
        IF(NPAR.LT.1)THEN
          WRITE(ICOUT,321)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3922)
 3922     FORMAT('      FOR THE RANDOM PARTITION CASE, THE VALUE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3923)
 3923     FORMAT('      OF N MUST BE AT LEAST 1.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8197)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3925)NPAR
 3925     FORMAT('      THE SPECIFIED VALUE OF N  =  ',I8)
          CALL DPWRST('XXX','BUG ')
          GOTO9000
        ENDIF
C
        CALL RANPAR(K,NPAR,ISEED,Y,ITEMP1,ITEMP2)
        NRAN=K
        DO3929II=1,NRAN
          ISUB(II)=1
 3929   CONTINUE
        ICASEQ='FOR'
        IROWN=NRAN
        NIISUB=NRAN
        NLEFT=NRAN
      ELSEIF(ICASRA.EQ.'RANE')THEN
        IHP='N   '
        IHP2='    '
        IHWUSE='P'
        MESSAG='YES'
        CALL CHECKN(IHP,IHP2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        NPAR=INT(VALUE(ILOCP)+0.5)
C
        IF(NPAR.LT.1)THEN
          WRITE(ICOUT,321)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3932)
 3932     FORMAT('      FOR THE RANDOM EQUIVALENCE RELATION CASE, ',
     1           'THE VALUE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3933)
 3933     FORMAT('      OF N MUST BE AT LEAST 1.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8197)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3935)NPAR
 3935     FORMAT('      THE SPECIFIED VALUE OF N  =  ',I8)
          CALL DPWRST('XXX','BUG ')
          GOTO9000
        ENDIF
C
        IF(NPAR.NE.NLAST)THEN
          NLAST=1
        ENDIF
        CALL RANEQU(NPAR,LTEMP,ITEMP1,ITEMP2,TEMP3,ITEMP4,ISEED,Y)
        NRAN=NPAR
        DO3939II=1,NRAN
          ISUB(II)=1
          Y(II)=REAL(ITEMP1(II))
 3939   CONTINUE
        ICASEQ='FOR'
        IROWN=NRAN
        NIISUB=NRAN
        NLEFT=NRAN
      ELSEIF(ICASRA.EQ.'RAYT')THEN
        IHP='N   '
        IHP2='    '
        IHWUSE='P'
        MESSAG='YES'
        CALL CHECKN(IHP,IHP2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        NPAR=INT(VALUE(ILOCP)+0.5)
C
        IF(NPAR.LT.1)THEN
          WRITE(ICOUT,321)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3942)
 3942     FORMAT('      FOR THE RANDOM YOUNG TABLEAUX CASE, ',
     1           'THE VALUE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3943)
 3943     FORMAT('      OF N MUST BE AT LEAST 1.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8197)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3945)NPAR
 3945     FORMAT('      THE SPECIFIED VALUE OF N  =  ',I8)
          CALL DPWRST('XXX','BUG ')
          GOTO9000
        ENDIF
C
        ISUM=0
        DO3948I=1,NLEFT
          ITEMP1(I)=INT(TEMP4(I)+0.5)
          ISUM=ISUM + ITEMP1(I)
 3948   CONTINUE
        IF(NLEFT.LT.NPAR)THEN
          DO3949I=NLEFT+1,NPAR
            ITEMP1(I)=0
 3949     CONTINUE
        ENDIF
C
        CALL RANYTB(NPAR,ITEMP1,ITEMP2,ISEED)
        NRAN=NPAR
        DO3952II=1,NRAN
          ISUB(II)=1
          Y(II)=REAL(ITEMP2(II))
 3952   CONTINUE
        ICASEQ='FOR'
        IROWN=NRAN
        NIISUB=NRAN
        NLEFT=NRAN
      ELSE
        IHP='RANL'
        IHP2='OC  '
        IHWUSE='P'
        MESSAG='NO'
        CALL CHECKN(IHP,IHP2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
        IF(IERROR.EQ.'YES')THEN
          RANLOC=0.0
        ELSE
          RANLOC=VALUE(ILOCV)
        ENDIF
        IHP='RANS'
        IHP2='CALE'
        IHWUSE='P'
        MESSAG='NO'
        CALL CHECKN(IHP,IHP2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
        IF(IERROR.EQ.'YES')THEN
          RANSCA=1.0
        ELSE
          RANSCA=VALUE(ILOCV)
          IF(RANSCA.LE.0.0)RANSCA=1.0
        ENDIF
C
        IF(ICASRA.EQ.'GMCL' .OR. ICASRA.EQ.'TRAP' .OR.
     1     ICASRA.EQ.'GTRA' .OR. ICASRA.EQ.'UTSP' .OR.
     1     ICASRA.EQ.'GLGP' .OR.
     1     ICASRA.EQ.'PARE' .OR. ICASRA.EQ.'PAR2'
     1    )THEN
          CONTINUE
        ELSE
          IHP='A   '
          IHP2='    '
          IHWUSE='P'
          MESSAG='NO'
          CALL CHECKN(IHP,IHP2,IHWUSE,
     1                IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1                ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
          IF(IERROR.EQ.'YES')THEN
            A=0.0
          ELSE
            A=VALUE(ILOCV)
          ENDIF
C
          IHP='B   '
          IHP2='    '
          IHWUSE='P'
          MESSAG='NO'
          CALL CHECKN(IHP,IHP2,IHWUSE,
     1                IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1                ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
          IF(IERROR.EQ.'YES')THEN
            B=1.0
          ELSE
            B=VALUE(ILOCV)
          ENDIF
C
        ENDIF
C
        CALL DPRAN2(ICASRA,ISEED,Y,NRAN,TEMP3,
     1              A,B,MINMAX,
     1              SHAPE1,SHAPE2,SHAPE3,SHAPE4,SHAPE5,SHAPE6,SHAPE7,
     1              IADEDF,IGEPDF,IMAKDF,IBEIDF,
     1              ILGADF,ISKNDF,IGLDDF,IBGEDF,IGETDF,ICONDF,
     1              IGOMDF,IKATDF,IGIGDF,IGEODF,
     1              IBUGA3,ISUBRO,IFOUND,IERROR)
C
        IF(IFOUND.EQ.'NO')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,321)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5953)
 5953     FORMAT('      THE RANDOM NUMBER CASE WAS NOT RECOGNIZED.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5956)ICASRA
 5956     FORMAT('      THE VALUE OF ICASRA = ',A4)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5957)
 5957     FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,5958)(IANS(I),I=1,MIN(80,IWIDTH))
 5958       FORMAT(80A1)
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IERROR='YES'
          GOTO9000
        ENDIF
C
        DO5970JJ=1,NRAN
          Y(JJ)=RANLOC + RANSCA*Y(JJ)
 5970   CONTINUE
C
      ENDIF
C
C               ******************************************************
C               **  STEP 8--                                        **
C               **  IF CALLED FOR (THAT IS, IF IBUGA3 IS ON),       **
C               **  PRINT OUT THE INTERMEDIATE VARIABLE Y(.).       **
C               **  THIS IS USEFUL FOR DIAGNOSTIC PURPOSES          **
C               **  IN REVIEWING THE OUTPUT FROM THIS SUBROUTINE.   **
C               ******************************************************
C
      ISTEPN='9'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RAND')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,4011)
 4011   FORMAT('OUTPUT FROM MIDDLE OF DPRAND AFTER ALL XXXRAN ',
     1         'HAVE BEEN CALLED--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4012)NRAN
 4012   FORMAT('NRAN = ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(NRAN.GE.1)THEN
          DO4014I=1,NRAN
            WRITE(ICOUT,4015)I,Y(I)
 4015       FORMAT('I,Y(I) = ',I8,F12.5)
            CALL DPWRST('XXX','BUG ')
 4014       CONTINUE
        ENDIF
      ENDIF
C
C               ******************************************************
C               **  STEP 9--                                        **
C               **  COPY THE RANDOM NUMBERS                         **
C               **  FROM THE INTERMEDIATE VECTOR Y(.)               **
C               **  TO THE APPROPRIATE COLUMN                       **
C               **  (BASED ON THE QUALIFIER--FULL, SUBSET, OR FOR)  **
C               **  IN THE INTERNAL DATAPLOT DATA TABLE.            **
C               ******************************************************
C
      ISTEPN='10'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RAND')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NS2=0
      DO4060I=1,NIISUB
        IJ=MAXN*(ICOLL-1)+I
        IF(ISUB(I).EQ.0)GOTO4060
        NS2=NS2+1
        IF(ICOLL.LE.MAXCOL)V(IJ)=Y(NS2)
        IF(ICOLL.EQ.MAXCP1)PRED(I)=Y(NS2)
        IF(ICOLL.EQ.MAXCP2)RES(I)=Y(NS2)
        IF(ICOLL.EQ.MAXCP3)YPLOT(I)=Y(NS2)
        IF(ICOLL.EQ.MAXCP4)XPLOT(I)=Y(NS2)
        IF(ICOLL.EQ.MAXCP5)X2PLOT(I)=Y(NS2)
        IF(ICOLL.EQ.MAXCP6)TAGPLO(I)=Y(NS2)
        IF(NS2.EQ.1)IROW1=I
        IROWN=I
 4060 CONTINUE
C
C               *******************************************
C               **  STEP 10--                            **
C               **  CARRY OUT THE LIST UPDATING AND      **
C               **  GENERATE THE INFORMATIVE PRINTING.   **
C               *******************************************
C
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RAND')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASEQ.EQ.'FULL'.AND.NEWNAM.EQ.'NO')NINEW=NLEFT
      IF(ICASEQ.EQ.'FULL'.AND.NEWNAM.EQ.'YES')NINEW=MAXN
      IF(ICASEQ.EQ.'SUBS'.AND.NEWNAM.EQ.'NO'.AND.
     1NLEFT.GE.IROWN)NINEW=NLEFT
      IF(ICASEQ.EQ.'SUBS'.AND.NEWNAM.EQ.'NO'.AND.
     1NLEFT.LT.IROWN)NINEW=IROWN
      IF(ICASEQ.EQ.'SUBS'.AND.NEWNAM.EQ.'YES')NINEW=IROWN
      IF(ICASEQ.EQ.'FOR'.AND.NEWNAM.EQ.'NO'.AND.
     1NLEFT.GE.IROWN)NINEW=NLEFT
      IF(ICASEQ.EQ.'FOR'.AND.NEWNAM.EQ.'NO'.AND.
     1NLEFT.LT.IROWN)NINEW=IROWN
      IF(ICASEQ.EQ.'FOR'.AND.NEWNAM.EQ.'YES')NINEW=IROWN
C
      IHNAME(ILISTL)=ILEFT
      IHNAM2(ILISTL)=ILEFT2
      IUSE(ILISTL)='V'
      IVALUE(ILISTL)=ICOLL
      VALUE(ILISTL)=ICOLL
      IN(ILISTL)=NINEW
C
      IF(NEWNAM.EQ.'YES')NUMNAM=NUMNAM+1
      IF(NEWNAM.EQ.'YES')NUMCOL=NUMCOL+1
C
      DO4600J4=1,NUMNAM
        IF(IUSE(J4).EQ.'V'.AND.IVALUE(J4).EQ.ICOLL)GOTO4605
        GOTO4600
 4605   CONTINUE
        IUSE(J4)='V'
        IVALUE(J4)=ICOLL
        VALUE(J4)=ICOLL
        IN(J4)=NINEW
 4600 CONTINUE
C
      IF(IPRINT.EQ.'OFF')GOTO4559
      IF(IFEEDB.EQ.'OFF')GOTO4559
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4511)ILEFT,ILEFT2,NS2
 4511 FORMAT('THE NUMBER OF VALUES GENERATED FOR ',
     1'THE VARIABLE ',A4,A4,' = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
C
      IJ=MAXN*(ICOLL-1)+IROW1
      IF(ICOLL.LE.MAXCOL)THEN
         WRITE(ICOUT,4521)ILEFT,ILEFT2,V(IJ),IROW1
 4521    FORMAT('THE FIRST           COMPUTED VALUE OF ',
     1   A4,A4,' = ',E15.7,'   (ROW ',I6,')')
         CALL DPWRST('XXX','BUG ')
      ELSE IF(ICOLL.EQ.MAXCP1)THEN
         WRITE(ICOUT,4521)ILEFT,ILEFT2,PRED(IROW1),IROW1
         CALL DPWRST('XXX','BUG ')
      ELSE IF(ICOLL.EQ.MAXCP2)THEN
         WRITE(ICOUT,4521)ILEFT,ILEFT2,RES(IROW1),IROW1
         CALL DPWRST('XXX','BUG ')
      ELSE IF(ICOLL.EQ.MAXCP3)THEN
         WRITE(ICOUT,4521)ILEFT,ILEFT2,YPLOT(IROW1),IROW1
         CALL DPWRST('XXX','BUG ')
      ELSE IF(ICOLL.EQ.MAXCP4)THEN
         WRITE(ICOUT,4521)ILEFT,ILEFT2,XPLOT(IROW1),IROW1
         CALL DPWRST('XXX','BUG ')
      ELSE IF(ICOLL.EQ.MAXCP5)THEN
         WRITE(ICOUT,4521)ILEFT,ILEFT2,X2PLOT(IROW1),IROW1
         CALL DPWRST('XXX','BUG ')
      ELSE IF(ICOLL.EQ.MAXCP6)THEN
         WRITE(ICOUT,4521)ILEFT,ILEFT2,TAGPLO(IROW1),IROW1
         CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IJ=MAXN*(ICOLL-1)+IROWN
      IF(NS2.NE.1)THEN
         IF(ICOLL.LE.MAXCOL)THEN
            WRITE(ICOUT,4531)NS2,ILEFT,ILEFT2,V(IJ),IROWN
 4531       FORMAT('THE LAST (',I5,'-TH) COMPUTED VALUE OF ',
     1      A4,A4,' = ',E15.7,'   (ROW ',I6,')')
            CALL DPWRST('XXX','BUG ')
         ELSE IF(ICOLL.EQ.MAXCP1)THEN
            WRITE(ICOUT,4531)NS2,ILEFT,ILEFT2,PRED(IROWN),IROWN
            CALL DPWRST('XXX','BUG ')
         ELSE IF(ICOLL.EQ.MAXCP2)THEN
            WRITE(ICOUT,4531)NS2,ILEFT,ILEFT2,RES(IROWN),IROWN
            CALL DPWRST('XXX','BUG ')
         ELSE IF(ICOLL.EQ.MAXCP3)THEN
            WRITE(ICOUT,4531)NS2,ILEFT,ILEFT2,YPLOT(IROWN),IROWN
            CALL DPWRST('XXX','BUG ')
         ELSE IF(ICOLL.EQ.MAXCP4)THEN
            WRITE(ICOUT,4531)NS2,ILEFT,ILEFT2,XPLOT(IROWN),IROWN
            CALL DPWRST('XXX','BUG ')
         ELSE IF(ICOLL.EQ.MAXCP5)THEN
            WRITE(ICOUT,4531)NS2,ILEFT,ILEFT2,X2PLOT(IROWN),IROWN
            CALL DPWRST('XXX','BUG ')
         ELSE IF(ICOLL.EQ.MAXCP6)THEN
            WRITE(ICOUT,4531)NS2,ILEFT,ILEFT2,TAGPLO(IROWN),IROWN
            CALL DPWRST('XXX','BUG ')
         ENDIF
      ENDIF
      IF(NS2.NE.1)GOTO4590
      WRITE(ICOUT,4546)
 4546 FORMAT('SINCE THE GENERATED SAMPLE SIZE WAS ONLY 1,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4542)
 4542 FORMAT('THE ABOVE VALUE WAS THE SOLE VALUE COMPUTED.')
      CALL DPWRST('XXX','BUG ')
 4590 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4612)ILEFT,ILEFT2,ICOLL
 4612 FORMAT('THE CURRENT COLUMN FOR THE VARIABLE ',A4,A4,' = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4613)ILEFT,ILEFT2,NINEW
 4613 FORMAT('THE CURRENT LENGTH OF THE VARIABLE ',A4,A4,' = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
 4559 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RAND')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPRAND--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR,IBUGA3,IBUGQ
 9012   FORMAT('IFOUND,IERROR,IBUGA3,IBUGQ = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)ICASRA,ISEED,ILOCNU,NS2,MINMAX
 9014   FORMAT('ICASRA,ISEED,ILOCNU,NS2,MINMAX = ',A4,4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)NS,NIISUB,NRAN
 9016   FORMAT('NS,NIISUB,NRAN = ',I8,I8,I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPRAN2(ICASRA,ISEED,Y,NRAN,TEMP1,
     1                  A,B,MINMAX,
     1                  SHAPE1,SHAPE2,SHAPE3,SHAPE4,
     1                  SHAPE5,SHAPE6,SHAPE7,
     1                  IADEDF,IGEPDF,IMAKDF,IBEIDF,
     1                  ILGADF,ISKNDF,IGLDDF,IBGEDF,IGETDF,ICONDF,
     1                  IGOMDF,IKATDF,IGIGDF,IGEODF,
     1                  IBUGA3,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--THIS ROUTINE IS SPLIT OFF FROM DPRAND IN ORDER
C              TO ALLOW OTHER ROUTINES TO CALL THE RANDOM NUMBER
C              ROUTINES IN A GENERIC WAY.
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
C     VERSION NUMBER--2009/9
C     ORIGINAL VERSION--SEPTEMBER 2009. SPLIT OFF FROM DPRAND
C     UPDATED         --JULY      2010. END EFFECTS WEIBULL
C     UPDATED         --AUGUST    2010. BRITTLE FIBER WEIBULL
C     UPDATED         --JANUARY   2011. ARCTANGENT
C     UPDATED         --MARCH     2013. SINE
C     UPDATED         --MARCH     2013. EXCLUSION ZONE UNIFORM
C     UPDATED         --APRIL     2014. "G" AND "H" AS DISTINCT FROM
C                                       "G AND H" DISTRIBUTIONS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASRA
      CHARACTER*4 IADEDF
      CHARACTER*4 IGEPDF
      CHARACTER*4 IMAKDF
      CHARACTER*4 IBEIDF
      CHARACTER*4 ILGADF
      CHARACTER*4 ISKNDF
      CHARACTER*4 IGLDDF
      CHARACTER*4 IBGEDF
      CHARACTER*4 IGETDF
      CHARACTER*4 ICONDF
      CHARACTER*4 IGOMDF
      CHARACTER*4 IKATDF
      CHARACTER*4 IGIGDF
      CHARACTER*4 IGEODF
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C-----COMMON----------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION TEMP1(*)
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----DATA STATEMENTS-------------------------------------------------
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPRA'
      ISUBN2='N2  '
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RAN2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPRAN2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGA3,ICASRA,NRAN,ISEED,MINMAX
   53   FORMAT('IBUGA3,ICASRA,NRAN,ISEED,MINMAX = ',A4,2X,A4,2X,3I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***********************************************
C               **  GENERATE THE RANDOM NUMBERS              **
C               ***********************************************
C
      IF(ICASRA.EQ.'UNIF')THEN
        CALL UNIRAN(NRAN,ISEED,Y)
      ELSEIF(ICASRA.EQ.'NORM')THEN
        CALL NORRAN(NRAN,ISEED,Y)
      ELSEIF(ICASRA.EQ.'LOGI')THEN
        CALL LOGRAN(NRAN,ISEED,Y)
      ELSEIF(ICASRA.EQ.'DEXP')THEN
        CALL DEXRAN(NRAN,ISEED,Y)
      ELSEIF(ICASRA.EQ.'CAUC')THEN
        CALL CAURAN(NRAN,ISEED,Y)
      ELSEIF(ICASRA.EQ.'TULA')THEN
        CALL LAMRAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'LOGN' .OR. ICASRA.EQ.'3LGN')THEN
        CALL LGNRAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'HNOR')THEN
        CALL HFNRAN(NRAN,ISEED,Y)
      ELSEIF(ICASRA.EQ.'TPP')THEN
        CALL TRAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'CHIS')THEN
        CALL CHSRAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'FPP')THEN
        CALL FRAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'EXPO')THEN
        CALL EXPRAN(NRAN,ISEED,Y)
      ELSEIF(ICASRA.EQ.'GAMM' .OR. ICASRA.EQ.'3GAM')THEN
        CALL GAMRAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'BETA')THEN
        CALL BETRAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'WEIB' .OR. ICASRA.EQ.'3WEI')THEN
        CALL WEIRAN(NRAN,SHAPE1,MINMAX,ISEED,Y)
      ELSEIF(ICASRA.EQ.'EV1 ')THEN
        CALL EV1RAN(NRAN,MINMAX,ISEED,Y)
      ELSEIF(ICASRA.EQ.'EV2 ' .OR. ICASRA.EQ.'3EV2')THEN
        CALL EV2RAN(NRAN,SHAPE1,MINMAX,ISEED,Y)
      ELSEIF(ICASRA.EQ.'PARE')THEN
        ZLOC=SHAPE2
        IF(ZLOC.LE.0.0)ZLOC=1.0
        CALL PARRAN(NRAN,SHAPE1,ZLOC,ISEED,Y)
      ELSEIF(ICASRA.EQ.'BINO')THEN
        CALL BINRAN(NRAN,SHAPE1,INT(SHAPE2+0.1),ISEED,Y)
      ELSEIF(ICASRA.EQ.'GEOM')THEN
        IF(IGEODF.EQ.'DLMF')THEN
          CALL GE2RAN(NRAN,SHAPE1,ISEED,Y)
        ELSE
          CALL GEORAN(NRAN,SHAPE1,ISEED,Y)
        ENDIF
      ELSEIF(ICASRA.EQ.'POIS')THEN
        CALL POIRAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'NEBI')THEN
        CALL NBRAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'SEMC')THEN
        IF(SHAPE1.EQ.CPUMIN)THEN
          ASCALE=1.0
        ELSE
          ASCALE=1.0
        ENDIF
        CALL SEMRAN(NRAN,ASCALE,ISEED,Y)
      ELSEIF(ICASRA.EQ.'TRIA')THEN
        CALL TRIRAN(NRAN,SHAPE1,A,B,ISEED,Y)
      ELSEIF(ICASRA.EQ.'DUNI')THEN
        CALL DUNRAN(NRAN,INT(SHAPE1+0.1),ISEED,Y)
      ELSEIF(ICASRA.EQ.'BOOT')THEN
        CALL DUNRA2(NRAN,NRAN,ISEED,Y)
      ELSEIF(ICASRA.EQ.'PERM')THEN
        CALL RANPER(NRAN,ISEED,Y)
      ELSEIF(ICASRA.EQ.'INGA')THEN
        CALL IGRAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'WALD')THEN
        ATEMP=1.0
        CALL IGRAN(NRAN,SHAPE1,ATEMP,ISEED,Y)
      ELSEIF(ICASRA.EQ.'RIGA')THEN
        CALL RIGRAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'FATL')THEN
        CALL FLRAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'GPAR')THEN
        CALL GEPRAN(NRAN,SHAPE1,MINMAX,IGEPDF,ISEED,Y)
      ELSEIF(ICASRA.EQ.'POWF')THEN
        CALL POWRAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'HYPG')THEN
        DO1352II=1,NRAN
          CALL HYPRAN(INT(SHAPE1+0.1),INT(SHAPE2+0.1),INT(SHAPE3+0.1),
     1                ISEED,JX)
          IF(JX.EQ.-1)THEN
            WRITE(ICOUT,1354)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1356)INT(SHAPE1+0.1),INT(SHAPE2+0.1),
     1                       INT(SHAPE3+0.1)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
 1354     FORMAT('****** ERROR IN GENERATING HYPERGEOMETRIC RANDOM ',
     1           'NUMBERS.')
 1356     FORMAT('       THE VALUES OF K, M, AND N = ',3I8)
          Y(II)=REAL(JX)
 1352   CONTINUE
      ELSEIF(ICASRA.EQ.'NCCS')THEN
        CALL NCCRAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'NCF ')THEN
        CALL NCFRAN(NRAN,SHAPE1,SHAPE2,SHAPE3,ISEED,Y)
      ELSEIF(ICASRA.EQ.'DNCF')THEN
        CALL DNFRAN(NRAN,SHAPE1,SHAPE2,SHAPE3,SHAPE4,ISEED,Y)
      ELSEIF(ICASRA.EQ.'FNOR')THEN
        CALL FNRRAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'HCAU')THEN
        CALL HFCRAN(NRAN,ISEED,Y)
      ELSEIF(ICASRA.EQ.'NORX')THEN
        CALL NMXRAN(NRAN,SHAPE1,SHAPE2,SHAPE3,SHAPE4,SHAPE5,ISEED,Y)
      ELSEIF(ICASRA.EQ.'POWL')THEN
        CALL PWLRAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'GTLA')THEN
        CALL GLDRAN(NRAN,SHAPE1,SHAPE2,ISEED,IGLDDF,Y)
      ELSEIF(ICASRA.EQ.'IWEI')THEN
        CALL IWERAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'DWEI')THEN
        CALL DWERAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'DGAM')THEN
        CALL DGARAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'LGAM')THEN
        CALL LGARAN(NRAN,SHAPE1,ILGADF,ISEED,Y)
      ELSEIF(ICASRA.EQ.'IGAM' .OR. ICASRA.EQ.'3IGA')THEN
        CALL IGARAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'COSI')THEN
        CALL COSRAN(NRAN,ISEED,Y)
      ELSEIF(ICASRA.EQ.'SINE')THEN
        CALL SINRAN(NRAN,ISEED,Y)
      ELSEIF(ICASRA.EQ.'ANGL')THEN
        CALL ANGRAN(NRAN,ISEED,Y)
      ELSEIF(ICASRA.EQ.'HSEC')THEN
        CALL HSERAN(NRAN,ISEED,Y)
      ELSEIF(ICASRA.EQ.'ARSI')THEN
        CALL ARSRAN(NRAN,ISEED,Y)
      ELSEIF(ICASRA.EQ.'LDEX')THEN
        CALL LDERAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'GEV ')THEN
        CALL GEVRAN(NRAN,SHAPE1,MINMAX,ISEED,Y)
      ELSEIF(ICASRA.EQ.'EWEI')THEN
        CALL EWERAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'GOMP')THEN
        CALL GOMRAN(NRAN,SHAPE1,SHAPE2,IGOMDF,ISEED,Y)
      ELSEIF(ICASRA.EQ.'HALO')THEN
        SHAPE1=-1.0
        CALL HFLRAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'GHLO')THEN
        CALL HFLRAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'PEXP')THEN
        CALL PEXRAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'ALPH')THEN
        CALL ALPRAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'BRAD')THEN
        CALL BRARAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'RECI')THEN
        CALL RECRAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'JOSB')THEN
        CALL JSBRAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'JOSU')THEN
        CALL JSURAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'POWN')THEN
        CALL PNRRAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'LOGL')THEN
        CALL LLGRAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'GEEX')THEN
        CALL GEERAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'PLGN')THEN
        CALL PLNRAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'BBIN')THEN
        CALL BBNRAN(SHAPE1,SHAPE2,INT(SHAPE3+0.1),NRAN,ISEED,Y)
      ELSEIF(ICASRA.EQ.'POLY')THEN
        CALL BBNRAN(SHAPE2,SHAPE1,INT(SHAPE3+0.1),NRAN,ISEED,Y)
      ELSEIF(ICASRA.EQ.'TSPO')THEN
        CALL TSPRAN(NRAN,SHAPE1,SHAPE2,A,B,ISEED,Y)
      ELSEIF(ICASRA.EQ.'BWEI')THEN
        CALL BWERAN(NRAN,SHAPE1,SHAPE2,SHAPE3,SHAPE4,SHAPE5,ISEED,Y)
      ELSEIF(ICASRA.EQ.'LOGS')THEN
        CALL DLGRAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'GHPP')THEN
        CALL GHRAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'GPP')THEN
        SHAPE2=0.0
        CALL GHRAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'HPP')THEN
        SHAPE1=0.0
        CALL GHRAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'SLAS')THEN
        CALL SLARAN(NRAN,ISEED,Y)
      ELSEIF(ICASRA.EQ.'LAND')THEN
        CALL LANRAN(NRAN,ISEED,Y)
      ELSEIF(ICASRA.EQ.'IBET')THEN
        CALL IBRAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'ERRO')THEN
        CALL ERRRAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'TRAP')THEN
        CALL TRARAN(NRAN,SHAPE1,SHAPE2,SHAPE3,SHAPE4,ISEED,Y)
      ELSEIF(ICASRA.EQ.'VONM')THEN
        CALL VONRAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'PAR2')THEN
        ZLOC=SHAPE2
        IF(ZLOC.LE.0.0)ZLOC=1.0
        CALL PA2RAN(NRAN,SHAPE1,ZLOC,ISEED,Y)
      ELSEIF(ICASRA.EQ.'WCAU')THEN
        CALL WCARAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'GTRA')THEN
        CALL GTRRAN(NRAN,SHAPE1,SHAPE2,SHAPE3,SHAPE4,
     1              SHAPE5,SHAPE6,SHAPE7,ISEED,Y)
      ELSEIF(ICASRA.EQ.'TNOR')THEN
        CALL TNRRAN(NRAN,A,B,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'CHI ')THEN
        CALL CHRAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'FCAU')THEN
        CALL FCARAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'MBKA')THEN
        CALL MIERAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'GEXP')THEN
        CALL GEXRAN(NRAN,SHAPE1,SHAPE2,SHAPE3,ISEED,Y)
      ELSEIF(ICASRA.EQ.'TEXP')THEN
        CALL TNERAN(NRAN,SHAPE1,SHAPE2,SHAPE3,ISEED,Y)
      ELSEIF(ICASRA.EQ.'GGAM')THEN
        CALL GGDRAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'FT  ')THEN
        CALL FTRAN(NRAN,INT(SHAPE1+0.1),ISEED,Y)
      ELSEIF(ICASRA.EQ.'SNOR')THEN
        CALL SNRAN(NRAN,SHAPE1,ISKNDF,ISEED,Y)
      ELSEIF(ICASRA.EQ.'TSKE')THEN
        CALL STRAN(NRAN,INT(SHAPE1+0.1),SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'ZETA')THEN
        CALL ZETRAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'GOMM')THEN
        IF(IMAKDF.EQ.'DLMF')THEN
          CALL MAKRAN(NRAN,SHAPE1,SHAPE2,SHAPE3,ISEED,Y)
        ELSEIF(IMAKDF.EQ.'MEEK')THEN
          XI=SHAPE1/SHAPE3
          THETA=SHAPE2/SHAPE1
          ALAMB=SHAPE3
          CALL MAKRAN(NRAN,XI,ALAMB,THETA,ISEED,Y)
        ELSEIF(IMAKDF.EQ.'REPA')THEN
          CALL MA2RAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
        ENDIF
      ELSEIF(ICASRA.EQ.'GIGA'.AND.IGIGDF.EQ.'3PAR')THEN
        CALL GIGRAN(NRAN,SHAPE1,SHAPE2,SHAPE3,ISEED,Y)
      ELSEIF(ICASRA.EQ.'GIGA'.AND.IGIGDF.EQ.'2PAR')THEN
        CALL GI2RAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'LSNO')THEN
        CALL LSNRAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'LSKT')THEN
        CALL LSTRAN(NRAN,INT(SHAPE1+0.1),SHAPE2,SHAPE3,ISEED,Y)
      ELSEIF(ICASRA.EQ.'NCT ')THEN
        CALL NCTRAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'DNCT')THEN
        CALL DNTRAN(NRAN,SHAPE1,SHAPE2,SHAPE3,ISEED,Y)
      ELSEIF(ICASRA.EQ.'GLOG')THEN
        CALL GLORAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'HERM')THEN
        CALL HERRAN(SHAPE1,SHAPE2,NRAN,ISEED,Y)
      ELSEIF(ICASRA.EQ.'YULE')THEN
        CALL YULRAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'WARI')THEN
        B=1.0
        BETA=SHAPE2
        ALPHA=SHAPE1-SHAPE2
        CALL GWARAN(NRAN,BETA,B,ALPHA,ISEED,Y)
      ELSEIF(ICASRA.EQ.'GWAR' .OR. ICASRA.EQ.'BNBI')THEN
        CALL GWARAN(NRAN,SHAPE1,SHAPE2,SHAPE3,ISEED,Y)
      ELSEIF(ICASRA.EQ.'NCBE')THEN
        CALL NCBRAN(NRAN,SHAPE1,SHAPE2,SHAPE3,ISEED,Y)
      ELSEIF(ICASRA.EQ.'DNCB')THEN
        CALL DNBRAN(NRAN,SHAPE1,SHAPE2,SHAPE3,SHAPE4,ISEED,Y)
      ELSEIF(ICASRA.EQ.'SDEX')THEN
        CALL SDERAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'ADEX')THEN
        CALL ADERAN(NRAN,SHAPE1,IADEDF,ISEED,Y)
      ELSEIF(ICASRA.EQ.'MAXW')THEN
        CALL MAXRAN(NRAN,ISEED,Y)
      ELSEIF(ICASRA.EQ.'RAYL')THEN
        CALL RAYRAN(NRAN,ISEED,Y)
      ELSEIF(ICASRA.EQ.'GALP')THEN
        CALL GALRAN(NRAN,SHAPE1,SHAPE2,IADEDF,ISEED,Y)
      ELSEIF(ICASRA.EQ.'MCLE')THEN
        CALL MCLRAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'BEIP')THEN
        CALL BEIRAN(NRAN,SHAPE1,SHAPE2,INT(SHAPE3+0.5),IBEIDF,ISEED,Y)
      ELSEIF(ICASRA.EQ.'BEIK')THEN
CCCCC   CALL BEKRAN(NRAN,S1SQ,S2SQ,ANU,ISEED,Y)
      ELSEIF(ICASRA.EQ.'GMCL')THEN
        CALL GMCRAN(NRAN,ALPHA,A,ISEED,Y)
      ELSEIF(ICASRA.EQ.'HBOL')THEN
CCCCC   CALL HBORAN(NRAN,ALPHA,XI,ISEED,Y)
      ELSEIF(ICASRA.EQ.'G5LO')THEN
        CALL GL5RAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'WAKE')THEN
        CALL WAKRAN(NRAN,SHAPE2,SHAPE1,SHAPE3,SHAPE4,ISEED,Y)
      ELSEIF(ICASRA.EQ.'BNOR')THEN
        CALL BNORAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'G2LO')THEN
        CALL GL2RAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'G3LO')THEN
        CALL GL3RAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'G4LO')THEN
        CALL GL4RAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'ALDE')THEN
        CALL ALDRAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'BGEO')THEN
        CALL BGERAN(SHAPE1,SHAPE2,NRAN,ISEED,Y,IBGEDF)
      ELSEIF(ICASRA.EQ.'ZIPF')THEN
        CALL ZIPRAN(NRAN,SHAPE1,INT(SHAPE2+0.1),ISEED,Y)
      ELSEIF(ICASRA.EQ.'BTAN')THEN
        CALL BTARAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'LPOI')THEN
        CALL LPORAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'LICT')THEN
        CALL LCTRAN(NRAN,INT(SHAPE1+0.1),ISEED,Y)
      ELSEIF(ICASRA.EQ.'MATC')THEN
        CALL MATRAN(NRAN,INT(SHAPE1+0.1),ISEED,Y)
CCCCC ELSEIF(ICASRA.EQ.'OCCU')THEN
      ELSEIF(ICASRA.EQ.'LBET')THEN
        CALL LBERAN(NRAN,SHAPE1,SHAPE2,SHAPE3,SHAPE4,ISEED,Y)
      ELSEIF(ICASRA.EQ.'AEPP')THEN
        CALL PAPRAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'LOST')THEN
        CALL LOSRAN(NRAN,SHAPE1,INT(SHAPE2+0.1),ISEED,Y)
      ELSEIF(ICASRA.EQ.'GLOS')THEN
        CALL GLSRAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'GNBI')THEN
        CALL GNBRAN(NRAN,SHAPE1,SHAPE2,SHAPE3,ISEED,Y)
      ELSEIF(ICASRA.EQ.'GEET')THEN
        CALL GETRAN(NRAN,SHAPE1,SHAPE2,IGETDF,ISEED,Y)
      ELSEIF(ICASRA.EQ.'QBIN')THEN
        CALL QBIRAN(NRAN,SHAPE1,SHAPE2,SHAPE3,ISEED,Y)
      ELSEIF(ICASRA.EQ.'CONS')THEN
        CALL CONRAN(NRAN,SHAPE1,SHAPE2,ICONDF,ISEED,Y)
      ELSEIF(ICASRA.EQ.'LKAT')THEN
        CALL LKRAN(NRAN,SHAPE1,SHAPE2,SHAPE3,ISEED,Y)
      ELSEIF(ICASRA.EQ.'KATZ')THEN
        CALL KATRAN(NRAN,DBLE(SHAPE1),DBLE(SHAPE2),IKATDF,ISEED,Y)
      ELSEIF(ICASRA.EQ.'DISW')THEN
        CALL DIWRAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'GLGP')THEN
        CALL GLGRAN(NRAN,SHAPE1,INT(SHAPE2+0.1),SHAPE3,ISEED,Y)
      ELSEIF(ICASRA.EQ.'TGNB')THEN
        CALL GNTRAN(NRAN,SHAPE1,SHAPE2,SHAPE3,INT(SHAPE4+0.1),ISEED,Y)
      ELSEIF(ICASRA.EQ.'TOPL')THEN
        CALL TOPRAN(NRAN,DBLE(SHAPE1),ISEED,Y)
      ELSEIF(ICASRA.EQ.'RGTL')THEN
        CALL RGTRAN(NRAN,DBLE(SHAPE1),DBLE(SHAPE2),ISEED,Y)
      ELSEIF(ICASRA.EQ.'GTOL')THEN
        CALL GTLRAN(NRAN,DBLE(SHAPE1),DBLE(SHAPE2),ISEED,Y)
      ELSEIF(ICASRA.EQ.'SLOP')THEN
        CALL SLORAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'OGIV')THEN
        CALL OGIRAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'TSSL')THEN
        CALL TSSRAN(NRAN,SHAPE2,SHAPE1,A,B,ISEED,Y)
      ELSEIF(ICASRA.EQ.'TSOG')THEN
CCCCC   CALL TSORAN(NRAN,AN,THETA,ALOWLM,AUPPLM,ISEED,Y)
        CALL TSORAN(NRAN,AN,THETA,A,B,ISEED,Y)
      ELSEIF(ICASRA.EQ.'BUR2')THEN
        CALL BU2RAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'BUR3')THEN
        CALL BU3RAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'BU12')THEN
        CALL B12RAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'BU10')THEN
        CALL B10RAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'BUR4')THEN
        CALL BU4RAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'BUR5')THEN
        CALL BU5RAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'BUR6')THEN
        CALL BU6RAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'BUR7')THEN
        CALL BU7RAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'BUR8')THEN
        CALL BU8RAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'BU11')THEN
        CALL B11RAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'BUR9')THEN
        CALL BU9RAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'DPUN')THEN
        CALL DPURAN(NRAN,AM,AN,ALPHA,BETA,ISEED,Y)
      ELSEIF(ICASRA.EQ.'UTSP')THEN
        CALL UTSRAN(NRAN,SHAPE1,SHAPE2,SHAPE3,SHAPE4,SHAPE5,SHAPE6,
     1              ISEED,Y)
      ELSEIF(ICASRA.EQ.'KUMA')THEN
        CALL KUMRAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'RPOW')THEN
        CALL RPORAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'MUTH')THEN
        CALL MUTRAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'LEXP')THEN
        CALL LEXRAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'TPAR')THEN
        CALL TNPRAN(NRAN,SHAPE1,SHAPE2,SHAPE3,ISEED,Y)
      ELSEIF(ICASRA.EQ.'BFRA')THEN
        CALL BFRRAN(NRAN,SHAPE1,SHAPE2,SHAPE3,ISEED,Y)
      ELSEIF(ICASRA.EQ.'L3EX')THEN
        CALL LE3RAN(NRAN,SHAPE1,SHAPE2,SHAPE3,ISEED,Y)
      ELSEIF(ICASRA.EQ.'KAPP')THEN
        CALL KAPRAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'PEA3')THEN
        CALL PE3RAN(NRAN,SHAPE1,ISEED,Y)
      ELSEIF(ICASRA.EQ.'EEWE')THEN
        CALL EEWRAN(NRAN,SHAPE1,SHAPE2,SHAPE3,SHAPE4,SHAPE5,ISEED,Y)
      ELSEIF(ICASRA.EQ.'BFWE')THEN
        CALL BFWRAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'ARCT')THEN
        CALL ATNRAN(NRAN,SHAPE1,SHAPE2,ISEED,Y)
      ELSEIF(ICASRA.EQ.'UNEX')THEN
        CALL UNERAN(NRAN,ISEED,A,B,SHAPE1,Y,TEMP1)
      ELSE
        IFOUND='NO'
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RAN2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPRAND--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR,IBUGA3,IFOUND
 9012   FORMAT('IERROR,IBUGA3,IFOUND = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPRAW(X,FREQ,NX,IWRITE,MAXNXT,Y,NY,IBUGA3,IERROR)
C
C     PURPOSE--SOMETIMES DATA IS MADE AVAILABLE AS A FREQUENCY
C              TABLE.  HOWEVER, FOR A PARTICULAR TYPE OF ANALSYSIS
C              YOU MAY NEED THE DATA IN RAW (I.E., IF YOU HAVE
C              A FREQUENCY OF 10 FOR THE VALUE 1, SIMPLY GENERATE
C              THE VALUE 1 TEN TIMES).  NEED TO CHECK FOR ARRAY
C              EXCEEDING MAXIMUM ALLOWABLE.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2004/4
C     ORIGINAL VERSION--APRIL     2004.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION Y(*)
      DIMENSION FREQ(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPRA'
      ISUBN2='W   '
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPRAW--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,IWRITE
   52   FORMAT('IBUGA3,IWRITE = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)NX,MAXNXT
   53   FORMAT('NX,MAXNXT = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,NX
          WRITE(ICOUT,56)I,X(I),FREQ(I)
   56     FORMAT('I,X(I), FREQ(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               **************************************
C               **  CONVERT FROM FREQUENCY TO RAW   **
C               **************************************
C
      IF(NX.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR--NUMBER OF CLASSES FOR FREQUENCY TO ',
     1         'RAW COMMAND IS LESS THAN 1.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      NY=0
      DO200I=1,NX
C
        NTEMP=INT(FREQ(I)+0.5)
        IF(NTEMP.LT.1)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,201)I,FREQ(I)
  201     FORMAT('***** ERROR--CLASS ',I8,' HAS NON-POSITIVE ',
     1          'FREQUENCY (= ',F12.5,')')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
C
        NTOT=NY+NTEMP
        IF(NTOT.GT.MAXNXT)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,203)MAXNXT
  203     FORMAT('***** ERROR--MAXIMUM NUMBER OF ROWS (',I8,') ')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,205)
  205     FORMAT('      IN CONVERTING FREQUENCY DATA TO RAW DATA.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
C
        DO210J=1,NTEMP
          NY=NY+1
          Y(NY)=X(I)
  210   CONTINUE
  200 CONTINUE
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
 9011   FORMAT('***** AT THE END       OF DPRAW--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR
 9012   FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NX,NY
 9013   FORMAT('NX,NY = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,NY
          WRITE(ICOUT,9016)I,Y(I)
 9016     FORMAT('I,Y(I) = ',I8,E15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPRBCO(IHARG,NUMARG,IDERBC,MAXREG,IREBCO,
     1IBUGP2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE REGION BORDER COLORS = THE COLORS
C              OF THE BORDER LINE AROUND THE REGIONS.
C              THESE ARE LOCATED IN THE VECTOR IREBCO(.).
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --NUMARG
C                     --IDERBC
C                     --MAXREG
C                     --IBUGP2 ('ON' OR 'OFF' )
C     OUTPUT ARGUMENTS--IREBCO (A CHARACTER VECTOR)
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
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--DECEMBER  1983.
C     UPDATED         --MAY       1994. PRINT MESSAGE STATING THAT 
C                                       THIS IS AN OBSOLETE COMMAND
C                                       (USE LINE COLOR COMMAND).
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IDERBC
      CHARACTER*4 IREBCO
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
      DIMENSION IREBCO(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPRB'
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
   51 FORMAT('***** AT THE BEGINNING OF DPRBCO--')
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
      WRITE(ICOUT,55)IDERBC
   55 FORMAT('IDERBC = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,60)NUMARG
   60 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO65I=1,NUMARG
      WRITE(ICOUT,66)IHARG(I)
   66 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,70)IREBCO(1)
   70 FORMAT('IREBCO(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO75I=1,10
      WRITE(ICOUT,76)I,IREBCO(I)
   76 FORMAT('I,IREBCO(I) = ',I8,2X,A4)
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
      IREBCO(1)=IDERBC
      GOTO1270
C
 1220 CONTINUE
      NUMREG=NUMARG-2
      IF(NUMREG.GT.MAXREG)NUMREG=MAXREG
      DO1225I=1,NUMREG
      J=I+2
      IHOLD1=IHARG(J)
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2=IDERBC
      IF(IHOLD1.EQ.'OFF')IHOLD2=IDERBC
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDERBC
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDERBC
      IREBCO(I)=IHOLD2
 1225 CONTINUE
      GOTO1270
C
 1270 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1279
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO1278I=1,NUMREG
      WRITE(ICOUT,1276)I,IREBCO(I)
 1276 FORMAT('THE COLOR OF REGION BORDER ',I6,
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
      IF(IHOLD1.EQ.'ON')IHOLD2=IDERBC
      IF(IHOLD1.EQ.'OFF')IHOLD2=IDERBC
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDERBC
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDERBC
      DO1315I=1,NUMREG
      IREBCO(I)=IHOLD2
 1315 CONTINUE
      GOTO1370
C
 1370 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1316)IREBCO(I)
 1316 FORMAT('THE COLOR OF ALL REGION BORDERS',
     1' HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1319 CONTINUE
      IFOUND='YES'
CCCCC FOLLOWING SECTION ADDED MAY 1994.
      WRITE(ICOUT,2100)
 2100 FORMAT('****** WARNING.  THE REGION BORDER COLOR COMMAND IS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2101)
 2101 FORMAT('       NOT USED.  THE BORDER COLOR FOR REGIONS IS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2102)
 2102 FORMAT('       SET WITH THE LINE COLOR COMMAND.          ******')
      CALL DPWRST('XXX','BUG ')
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
 9011 FORMAT('***** AT THE END       OF DPRBCO--')
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
      WRITE(ICOUT,9015)IDERBC
 9015 FORMAT('IDERBC = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9020)NUMARG
 9020 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO9025I=1,NUMARG
      WRITE(ICOUT,9026)IHARG(I)
 9026 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      WRITE(ICOUT,9030)IREBCO(1)
 9030 FORMAT('IREBCO(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO9035I=1,10
      WRITE(ICOUT,9036)I,IREBCO(I)
 9036 FORMAT('I,IREBCO(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPRBLI(IHARG,IHARG2,NUMARG,IDERBL,MAXREG,IREBLI,
CCCCC AUGUST 1995.  ADD IHARG2 FOR DASH2, ETC
CCCCC SUBROUTINE DPRBLI(IHARG,NUMARG,IDERBL,MAXREG,IREBLI,
     1IBUGP2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE BORDER LINES = THE LINES TYPES
C              OF THE BORDER AROUND THE REGIONS.
C              THESE ARE LOCATED IN THE VECTOR IREBLI(.).
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --NUMARG
C                     --IDERBL
C                     --MAXREG
C                     --IBUGP2 ('ON' OR 'OFF' )
C     OUTPUT ARGUMENTS--IREBLI (A CHARACTER VECTOR)
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
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--DECEMBER  1983.
C     UPDATED         --MAY       1994. PRINT MESSAGE SAYING TO USE THE
C                                       LINE COMMAND INSTEAD.
C     UPDATED         --AUGUST    1995. DASH2 BUG
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
CCCCC AUGUST 1995.  ADD FOLLOWING LINE
      CHARACTER*4 IHARG2
      CHARACTER*4 IDERBL
      CHARACTER*4 IREBLI
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
      DIMENSION IREBLI(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPRB'
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
   51 FORMAT('***** AT THE BEGINNING OF DPRBLI--')
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
      WRITE(ICOUT,55)IDERBL
   55 FORMAT('IDERBL = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,60)NUMARG
   60 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO65I=1,NUMARG
      WRITE(ICOUT,66)IHARG(I)
   66 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,70)IREBLI(1)
   70 FORMAT('IREBLI(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO75I=1,10
      WRITE(ICOUT,76)I,IREBLI(I)
   76 FORMAT('I,IREBLI(I) = ',I8,2X,A4)
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
      IREBLI(1)='    '
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
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDERBL
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDERBL
      IREBLI(I)=IHOLD2
 1225 CONTINUE
      GOTO1270
C
 1270 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1279
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO1278I=1,NUMREG
      WRITE(ICOUT,1276)I,IREBLI(I)
 1276 FORMAT('THE LINE TYPE FOR REGION BORDER ',I6,
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
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDERBL
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDERBL
      DO1315I=1,NUMREG
      IREBLI(I)=IHOLD2
 1315 CONTINUE
      GOTO1370
C
 1370 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1316)IREBLI(I)
 1316 FORMAT('THE LINE TYPE FOR ALL REGION BORDERS',
     1' HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1319 CONTINUE
      IFOUND='YES'
CCCCC ADD FOLLOWING SECTION MAY 1994.
      WRITE(ICOUT,2100)
 2100 FORMAT('****** WARNING.  THE REGION BORDER LINE COMMAND IS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2101)
 2101 FORMAT('       NOT USED.  THE BORDER LINE STYLE FOR')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2102)
 2102 FORMAT('       REGIONS IS SET WITH THE LINE COLOR COMMAND.*****')
      CALL DPWRST('XXX','BUG ')
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
 9011 FORMAT('***** AT THE END       OF DPRBLI--')
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
      WRITE(ICOUT,9015)IDERBL
 9015 FORMAT('IDERBL = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9020)NUMARG
 9020 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO9025I=1,NUMARG
      WRITE(ICOUT,9026)IHARG(I)
 9026 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      WRITE(ICOUT,9030)IREBLI(1)
 9030 FORMAT('IREBLI(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO9035I=1,10
      WRITE(ICOUT,9036)I,IREBLI(I)
 9036 FORMAT('I,IREBLI(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPRBTH(IHARG,IARGT,ARG,NUMARG,PDERBT,MAXREG,PREBTH,
     1IBUGP2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE REGION (BORDER) LINE THICKNESSES = THE THICKNESSES
C              OF THE BORDER LINE AROUND THE REGIONS.
C              THESE ARE LOCATED IN THE VECTOR PREBTH(.).
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --IARGT  (A  CHARACTER VECTOR)
C                     --ARG
C                     --NUMARG
C                     --PDERBT
C                     --MAXREG
C                     --IBUGP2 ('ON' OR 'OFF' )
C     OUTPUT ARGUMENTS--PREBTH (A FLOATING POINT VECTOR)
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
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--DECEMBER  1983.
C     UPDATED         --MAY       1994. PRINT MESSAGE TO USE LINE
C                                       THICKNESS COMMAND INSTEAD.
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
      DIMENSION PREBTH(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPRB'
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
   51 FORMAT('***** AT THE BEGINNING OF DPRBTH--')
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
      WRITE(ICOUT,55)PDERBT
   55 FORMAT('PDERBT = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,60)NUMARG
   60 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO65I=1,NUMARG
      WRITE(ICOUT,66)IHARG(I),IARGT(I),ARG(I)
   66 FORMAT('IHARG(I),IARGT(I),ARG(I) = ',A4,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,70)PREBTH(1)
   70 FORMAT('PREBTH(1) = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      DO75I=1,10
      WRITE(ICOUT,76)I,PREBTH(I)
   76 FORMAT('I,PREBTH(I) = ',I8,2X,E15.7)
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
      IF(IHARG(3).EQ.'ALL')HOLD1=PDERBT
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
      PREBTH(1)=PDERBT
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
      IF(IHOLD1.EQ.'ON')HOLD2=PDERBT
      IF(IHOLD1.EQ.'OFF')HOLD2=PDERBT
      IF(IHOLD1.EQ.'AUTO')HOLD2=PDERBT
      IF(IHOLD1.EQ.'DEFA')HOLD2=PDERBT
      PREBTH(I)=HOLD2
 1225 CONTINUE
      GOTO1270
C
 1270 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1279
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO1278I=1,NUMREG
      WRITE(ICOUT,1276)I,PREBTH(I)
 1276 FORMAT('THE THICKNESS OF REGION BORDER ',I6,
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
      IF(IHOLD1.EQ.'ON')HOLD2=PDERBT
      IF(IHOLD1.EQ.'OFF')HOLD2=PDERBT
      IF(IHOLD1.EQ.'AUTO')HOLD2=PDERBT
      IF(IHOLD1.EQ.'DEFA')HOLD2=PDERBT
      DO1315I=1,NUMREG
      PREBTH(I)=HOLD2
 1315 CONTINUE
      GOTO1370
C
 1370 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1316)PREBTH(I)
 1316 FORMAT('THE THICKNESS OF ALL REGION BORDERS',
     1' HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1319 CONTINUE
      IFOUND='YES'
CCCCC ADD FOLLOWING SECTION MAY 1994.
      WRITE(ICOUT,2100)
 2100 FORMAT('****** WARNING.  THE REGION THICKNESS COMMAND IS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2101)
 2101 FORMAT('       NOT USED.  THE BORDER THICKNESS FOR REGIONS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2102)
 2102 FORMAT('       IS SET WITH THE LINE THICKNESS COMMAND.  ******')
      CALL DPWRST('XXX','BUG ')
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
 9011 FORMAT('***** AT THE END       OF DPRBTH--')
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
      WRITE(ICOUT,9015)PDERBT
 9015 FORMAT('PDERBT = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9020)NUMARG
 9020 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO9025I=1,NUMARG
      WRITE(ICOUT,9026)IHARG(I),IARGT(I),ARG(I)
 9026 FORMAT('IHARG(I),IARGT(I),ARG(I) = ',A4,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      WRITE(ICOUT,9030)PREBTH(1)
 9030 FORMAT('PREBTH(1) = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      DO9035I=1,10
      WRITE(ICOUT,9036)I,PREBTH(I)
 9036 FORMAT('I,PREBTH(I) = ',I8,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPRCIL(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN COMPLEX ITALIC LOWER CASE.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
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
   51 FORMAT('***** AT THE BEGINNING OF DPRCIL--')
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
      CALL DRCIL1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1019 CONTINUE
C
      IF(11.LE.ICHARN.AND.ICHARN.LE.20)GOTO1020
      GOTO1029
 1020 CONTINUE
      CALL DRCIL2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1029 CONTINUE
C
      IF(ICHARN.GE.21)GOTO1030
      GOTO1039
 1030 CONTINUE
      CALL DRCIL3(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
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
 9011 FORMAT('***** AT THE END       OF DPRCIL--')
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
      SUBROUTINE DPRCIN(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN COMPLEX ITALIC NUMERIC.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
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
   51 FORMAT('***** AT THE BEGINNING OF DPRCIN--')
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
      CALL DRCIN1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1019 CONTINUE
C
      IF(ICHARN.GE.9)GOTO1020
      GOTO1029
 1020 CONTINUE
      CALL DRCIN2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
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
 9011 FORMAT('***** AT THE END       OF DPRCIN--')
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
      SUBROUTINE DPRCIU(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN COMPLEX ITALIC UPPER CASE.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
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
   51 FORMAT('***** AT THE BEGINNING OF DPRCIU--')
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
      CALL DRCIU1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1019 CONTINUE
C
      IF(ICHARN.GE.15)GOTO1020
      GOTO1029
 1020 CONTINUE
      CALL DRCIU2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
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
 9011 FORMAT('***** AT THE END       OF DPRCIU--')
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
      SUBROUTINE DPRCL(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN COMPLEX LOWER CASE.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
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
   51 FORMAT('***** AT THE BEGINNING OF DPRCL--')
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
      IF(ICHARN.LE.12)GOTO1010
      GOTO1019
 1010 CONTINUE
      CALL DRCL1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1019 CONTINUE
C
      IF(ICHARN.GE.13)GOTO1020
      GOTO1029
 1020 CONTINUE
      CALL DRCL2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
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
 9011 FORMAT('***** AT THE END       OF DPRCL--')
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
      SUBROUTINE DPRCN(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN COMPLEX NUMERIC.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
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
   51 FORMAT('***** AT THE BEGINNING OF DPRCN--')
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
      CALL DRCN1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1019 CONTINUE
C
      IF(ICHARN.GE.10)GOTO1020
      GOTO1029
 1020 CONTINUE
      CALL DRCN2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
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
 9011 FORMAT('***** AT THE END       OF DPRCN--')
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
      SUBROUTINE DPRCS(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN COMPLEX SYMBOLS.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
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
      INCLUDE 'DPCOP2.INC'
C
C-----DATA STATEMENTS-------------------------------------------------
C
C     DEFINE CHARACTER   2210--. (PERIOD)
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
C     DEFINE CHARACTER   2211--, (COMMA)
C
      DATA IOPERA(   6),IX(   6),IY(   6)/'MOVE',   0,  -9/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',  -1,  -8/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',   0,  -7/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',   1,  -8/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',   1, -10/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',   0, -12/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',  -1, -13/
C
      DATA IXMIND(   2)/  -5/
      DATA IXMAXD(   2)/   5/
      DATA IXDELD(   2)/  10/
      DATA ISTARD(   2)/   6/
      DATA NUMCOO(   2)/   7/
C
C     DEFINE CHARACTER   2212--: (COLON)
C
      DATA IOPERA(  13),IX(  13),IY(  13)/'MOVE',   0,   5/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',  -1,   4/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',   0,   3/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',   1,   4/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',   0,   5/
      DATA IOPERA(  18),IX(  18),IY(  18)/'MOVE',   0,  -7/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',  -1,  -8/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',   0,  -9/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',   1,  -8/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',   0,  -7/
C
      DATA IXMIND(   3)/  -5/
      DATA IXMAXD(   3)/   5/
      DATA IXDELD(   3)/  10/
      DATA ISTARD(   3)/  13/
      DATA NUMCOO(   3)/  10/
C
C     DEFINE CHARACTER   2213--; (SEMICOLON)
C
      DATA IOPERA(  23),IX(  23),IY(  23)/'MOVE',   0,   5/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',  -1,   4/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',   0,   3/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',   1,   4/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',   0,   5/
      DATA IOPERA(  28),IX(  28),IY(  28)/'MOVE',   0,  -9/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',  -1,  -8/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',   0,  -7/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',   1,  -8/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',   1, -10/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',   0, -12/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',  -1, -13/
C
      DATA IXMIND(   4)/  -5/
      DATA IXMAXD(   4)/   5/
      DATA IXDELD(   4)/  10/
      DATA ISTARD(   4)/  23/
      DATA NUMCOO(   4)/  12/
C
C     DEFINE CHARACTER   2214--! (EXCLAMATION POINT)
C
      DATA IOPERA(  35),IX(  35),IY(  35)/'MOVE',   0,  12/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',  -1,  10/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',   0,  -2/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',   1,  10/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',   0,  12/
      DATA IOPERA(  40),IX(  40),IY(  40)/'MOVE',   0,  10/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',   0,   4/
      DATA IOPERA(  42),IX(  42),IY(  42)/'MOVE',   0,  -7/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',  -1,  -8/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',   0,  -9/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',   1,  -8/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW',   0,  -7/
C
      DATA IXMIND(   5)/  -5/
      DATA IXMAXD(   5)/   5/
      DATA IXDELD(   5)/  10/
      DATA ISTARD(   5)/  35/
      DATA NUMCOO(   5)/  12/
C
C     DEFINE CHARACTER   2215--? (QUESTION MARK)
C
      DATA IOPERA(  47),IX(  47),IY(  47)/'MOVE',  -5,   8/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',  -4,   7/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',  -5,   6/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW',  -6,   7/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW',  -6,   8/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',  -5,  10/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',  -4,  11/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',  -2,  12/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',   1,  12/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',   4,  11/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',   5,  10/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',   6,   8/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',   6,   6/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW',   5,   4/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',   4,   3/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',   0,   1/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',   0,  -2/
      DATA IOPERA(  64),IX(  64),IY(  64)/'MOVE',   1,  12/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',   3,  11/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW',   4,  10/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',   5,   8/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',   5,   6/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',   4,   4/
      DATA IOPERA(  70),IX(  70),IY(  70)/'DRAW',   2,   2/
      DATA IOPERA(  71),IX(  71),IY(  71)/'MOVE',   0,  -7/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',  -1,  -8/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',   0,  -9/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',   1,  -8/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',   0,  -7/
C
      DATA IXMIND(   6)/  -9/
      DATA IXMAXD(   6)/   9/
      DATA IXDELD(   6)/  18/
      DATA ISTARD(   6)/  47/
      DATA NUMCOO(   6)/  29/
C
C     DEFINE CHARACTER   2272--& (AMPERSAND)
C
      DATA IOPERA(  76),IX(  76),IY(  76)/'MOVE',   9,   4/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',   8,   3/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',   9,   2/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',  10,   3/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',  10,   4/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',   9,   5/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',   8,   5/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',   7,   4/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW',   6,   2/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',   4,  -3/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',   2,  -6/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',   0,  -8/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',  -2,  -9/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',  -5,  -9/
      DATA IOPERA(  90),IX(  90),IY(  90)/'DRAW',  -8,  -8/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',  -9,  -6/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW',  -9,  -3/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',  -8,  -1/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',  -2,   3/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',   0,   5/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',   1,   7/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',   1,   9/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',   0,  11/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',  -2,  12/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',  -4,  11/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',  -5,   9/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',  -5,   7/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',  -4,   4/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',  -2,   1/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',   3,  -6/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',   5,  -8/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',   8,  -9/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',   9,  -9/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',  10,  -8/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',  10,  -7/
      DATA IOPERA( 111),IX( 111),IY( 111)/'MOVE',  -5,  -9/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',  -7,  -8/
      DATA IOPERA( 113),IX( 113),IY( 113)/'DRAW',  -8,  -6/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',  -8,  -3/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',  -7,  -1/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',  -5,   1/
      DATA IOPERA( 117),IX( 117),IY( 117)/'MOVE',  -5,   7/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',  -4,   5/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',   4,  -6/
      DATA IOPERA( 120),IX( 120),IY( 120)/'DRAW',   6,  -8/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW',   8,  -9/
C
      DATA IXMIND(   7)/ -12/
      DATA IXMAXD(   7)/  13/
      DATA IXDELD(   7)/  25/
      DATA ISTARD(   7)/  76/
      DATA NUMCOO(   7)/  46/
C
C     DEFINE CHARACTER   2274--$ (DOLLAR SIGN)
C
      DATA IOPERA( 122),IX( 122),IY( 122)/'MOVE',  -2,  16/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',  -2, -13/
      DATA IOPERA( 124),IX( 124),IY( 124)/'MOVE',   2,  16/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',   2, -13/
      DATA IOPERA( 126),IX( 126),IY( 126)/'MOVE',   6,   9/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW',   5,   8/
      DATA IOPERA( 128),IX( 128),IY( 128)/'DRAW',   6,   7/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',   7,   8/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',   7,   9/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',   5,  11/
      DATA IOPERA( 132),IX( 132),IY( 132)/'DRAW',   2,  12/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',  -2,  12/
      DATA IOPERA( 134),IX( 134),IY( 134)/'DRAW',  -5,  11/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',  -7,   9/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',  -7,   7/
      DATA IOPERA( 137),IX( 137),IY( 137)/'DRAW',  -6,   5/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW',  -5,   4/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW',  -3,   3/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW',   3,   1/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',   5,   0/
      DATA IOPERA( 142),IX( 142),IY( 142)/'DRAW',   7,  -2/
      DATA IOPERA( 143),IX( 143),IY( 143)/'MOVE',  -7,   7/
      DATA IOPERA( 144),IX( 144),IY( 144)/'DRAW',  -5,   5/
      DATA IOPERA( 145),IX( 145),IY( 145)/'DRAW',  -3,   4/
      DATA IOPERA( 146),IX( 146),IY( 146)/'DRAW',   3,   2/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',   5,   1/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',   6,   0/
      DATA IOPERA( 149),IX( 149),IY( 149)/'DRAW',   7,  -2/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',   7,  -6/
      DATA IOPERA( 151),IX( 151),IY( 151)/'DRAW',   5,  -8/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',   2,  -9/
      DATA IOPERA( 153),IX( 153),IY( 153)/'DRAW',  -2,  -9/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',  -5,  -8/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW',  -7,  -6/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',  -7,  -5/
      DATA IOPERA( 157),IX( 157),IY( 157)/'DRAW',  -6,  -4/
      DATA IOPERA( 158),IX( 158),IY( 158)/'DRAW',  -5,  -5/
      DATA IOPERA( 159),IX( 159),IY( 159)/'DRAW',  -6,  -6/
C
      DATA IXMIND(   8)/ -10/
      DATA IXMAXD(   8)/  10/
      DATA IXDELD(   8)/  20/
      DATA ISTARD(   8)/ 122/
      DATA NUMCOO(   8)/  38/
C
C     DEFINE CHARACTER   2220--/ (SLASH)
C
      DATA IOPERA( 160),IX( 160),IY( 160)/'MOVE',   9,  16/
      DATA IOPERA( 161),IX( 161),IY( 161)/'DRAW',  -9, -16/
C
      DATA IXMIND(   9)/ -11/
      DATA IXMAXD(   9)/  11/
      DATA IXDELD(   9)/  22/
      DATA ISTARD(   9)/ 160/
      DATA NUMCOO(   9)/   2/
C
C     DEFINE CHARACTER   2221--( (LEFT PARENTHESES)
C
      DATA IOPERA( 162),IX( 162),IY( 162)/'MOVE',   4,  16/
      DATA IOPERA( 163),IX( 163),IY( 163)/'DRAW',   2,  14/
      DATA IOPERA( 164),IX( 164),IY( 164)/'DRAW',   0,  11/
      DATA IOPERA( 165),IX( 165),IY( 165)/'DRAW',  -2,   7/
      DATA IOPERA( 166),IX( 166),IY( 166)/'DRAW',  -3,   2/
      DATA IOPERA( 167),IX( 167),IY( 167)/'DRAW',  -3,  -2/
      DATA IOPERA( 168),IX( 168),IY( 168)/'DRAW',  -2,  -7/
      DATA IOPERA( 169),IX( 169),IY( 169)/'DRAW',   0, -11/
      DATA IOPERA( 170),IX( 170),IY( 170)/'DRAW',   2, -14/
      DATA IOPERA( 171),IX( 171),IY( 171)/'DRAW',   4, -16/
      DATA IOPERA( 172),IX( 172),IY( 172)/'MOVE',   2,  14/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW',   0,  10/
      DATA IOPERA( 174),IX( 174),IY( 174)/'DRAW',  -1,   7/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW',  -2,   2/
      DATA IOPERA( 176),IX( 176),IY( 176)/'DRAW',  -2,  -2/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW',  -1,  -7/
      DATA IOPERA( 178),IX( 178),IY( 178)/'DRAW',   0, -10/
      DATA IOPERA( 179),IX( 179),IY( 179)/'DRAW',   2, -14/
C
      DATA IXMIND(  10)/  -7/
      DATA IXMAXD(  10)/   7/
      DATA IXDELD(  10)/  14/
      DATA ISTARD(  10)/ 162/
      DATA NUMCOO(  10)/  18/
C
C     DEFINE CHARACTER   2222--) (RIGHT PARENTHESES)
C
      DATA IOPERA( 180),IX( 180),IY( 180)/'MOVE',  -4,  16/
      DATA IOPERA( 181),IX( 181),IY( 181)/'DRAW',  -2,  14/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',   0,  11/
      DATA IOPERA( 183),IX( 183),IY( 183)/'DRAW',   2,   7/
      DATA IOPERA( 184),IX( 184),IY( 184)/'DRAW',   3,   2/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',   3,  -2/
      DATA IOPERA( 186),IX( 186),IY( 186)/'DRAW',   2,  -7/
      DATA IOPERA( 187),IX( 187),IY( 187)/'DRAW',   0, -11/
      DATA IOPERA( 188),IX( 188),IY( 188)/'DRAW',  -2, -14/
      DATA IOPERA( 189),IX( 189),IY( 189)/'DRAW',  -4, -16/
      DATA IOPERA( 190),IX( 190),IY( 190)/'MOVE',  -2,  14/
      DATA IOPERA( 191),IX( 191),IY( 191)/'DRAW',   0,  10/
      DATA IOPERA( 192),IX( 192),IY( 192)/'DRAW',   1,   7/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW',   2,   2/
      DATA IOPERA( 194),IX( 194),IY( 194)/'DRAW',   2,  -2/
      DATA IOPERA( 195),IX( 195),IY( 195)/'DRAW',   1,  -7/
      DATA IOPERA( 196),IX( 196),IY( 196)/'DRAW',   0, -10/
      DATA IOPERA( 197),IX( 197),IY( 197)/'DRAW',  -2, -14/
C
      DATA IXMIND(  11)/  -7/
      DATA IXMAXD(  11)/   7/
      DATA IXDELD(  11)/  14/
      DATA ISTARD(  11)/ 180/
      DATA NUMCOO(  11)/  18/
C
C     DEFINE CHARACTER   2219--* (ASTERISK)
C
      DATA IOPERA( 198),IX( 198),IY( 198)/'MOVE',   0,  12/
      DATA IOPERA( 199),IX( 199),IY( 199)/'DRAW',   0,   0/
      DATA IOPERA( 200),IX( 200),IY( 200)/'MOVE',  -5,   9/
      DATA IOPERA( 201),IX( 201),IY( 201)/'DRAW',   5,   3/
      DATA IOPERA( 202),IX( 202),IY( 202)/'MOVE',   5,   9/
      DATA IOPERA( 203),IX( 203),IY( 203)/'DRAW',  -5,   3/
C
      DATA IXMIND(  12)/  -8/
      DATA IXMAXD(  12)/   8/
      DATA IXDELD(  12)/  16/
      DATA ISTARD(  12)/ 198/
      DATA NUMCOO(  12)/   6/
C
C     DEFINE CHARACTER   2231--- (HYPHEN OR MINUS SIGN)
C
      DATA IOPERA( 204),IX( 204),IY( 204)/'MOVE',  -9,   0/
      DATA IOPERA( 205),IX( 205),IY( 205)/'DRAW',   9,   0/
C
      DATA IXMIND(  13)/ -13/
      DATA IXMAXD(  13)/  13/
      DATA IXDELD(  13)/  26/
      DATA ISTARD(  13)/ 204/
      DATA NUMCOO(  13)/   2/
C
C     DEFINE CHARACTER   2232--+ (PLUS SIGN)
C
      DATA IOPERA( 206),IX( 206),IY( 206)/'MOVE',   0,   9/
      DATA IOPERA( 207),IX( 207),IY( 207)/'DRAW',   0,  -9/
      DATA IOPERA( 208),IX( 208),IY( 208)/'MOVE',  -9,   0/
      DATA IOPERA( 209),IX( 209),IY( 209)/'DRAW',   9,   0/
C
      DATA IXMIND(  14)/ -13/
      DATA IXMAXD(  14)/  13/
      DATA IXDELD(  14)/  26/
      DATA ISTARD(  14)/ 206/
      DATA NUMCOO(  14)/   4/
C
C     DEFINE CHARACTER   2238--= (EQUAL SIGN)
C
      DATA IOPERA( 210),IX( 210),IY( 210)/'MOVE',  -9,   3/
      DATA IOPERA( 211),IX( 211),IY( 211)/'DRAW',   9,   3/
      DATA IOPERA( 212),IX( 212),IY( 212)/'MOVE',  -9,  -3/
      DATA IOPERA( 213),IX( 213),IY( 213)/'DRAW',   9,  -3/
C
      DATA IXMIND(  15)/ -13/
      DATA IXMAXD(  15)/  13/
      DATA IXDELD(  15)/  26/
      DATA ISTARD(  15)/ 210/
      DATA NUMCOO(  15)/   4/
C
C     DEFINE CHARACTER   2216--' (SINGLE QUOTE)
C
      DATA IOPERA( 214),IX( 214),IY( 214)/'MOVE',   0,  12/
      DATA IOPERA( 215),IX( 215),IY( 215)/'DRAW',  -1,   5/
      DATA IOPERA( 216),IX( 216),IY( 216)/'MOVE',   1,  12/
      DATA IOPERA( 217),IX( 217),IY( 217)/'DRAW',  -1,   5/
C
      DATA IXMIND(  16)/  -4/
      DATA IXMAXD(  16)/   4/
      DATA IXDELD(  16)/   8/
      DATA ISTARD(  16)/ 214/
      DATA NUMCOO(  16)/   4/
C
C     DEFINE CHARACTER   2217--  (DOUBLE QUOTE)
C
      DATA IOPERA( 218),IX( 218),IY( 218)/'MOVE',  -4,  12/
      DATA IOPERA( 219),IX( 219),IY( 219)/'DRAW',  -5,   5/
      DATA IOPERA( 220),IX( 220),IY( 220)/'MOVE',  -3,  12/
      DATA IOPERA( 221),IX( 221),IY( 221)/'DRAW',  -5,   5/
      DATA IOPERA( 222),IX( 222),IY( 222)/'MOVE',   4,  12/
      DATA IOPERA( 223),IX( 223),IY( 223)/'DRAW',   3,   5/
      DATA IOPERA( 224),IX( 224),IY( 224)/'MOVE',   5,  12/
      DATA IOPERA( 225),IX( 225),IY( 225)/'DRAW',   3,   5/
C
      DATA IXMIND(  17)/  -8/
      DATA IXMAXD(  17)/   8/
      DATA IXDELD(  17)/  16/
      DATA ISTARD(  17)/ 218/
      DATA NUMCOO(  17)/   8/
C
C     DEFINE CHARACTER   2218--  (DEGREES)
C
      DATA IOPERA( 226),IX( 226),IY( 226)/'MOVE',  -1,  12/
      DATA IOPERA( 227),IX( 227),IY( 227)/'DRAW',  -3,  11/
      DATA IOPERA( 228),IX( 228),IY( 228)/'DRAW',  -4,   9/
      DATA IOPERA( 229),IX( 229),IY( 229)/'DRAW',  -4,   7/
      DATA IOPERA( 230),IX( 230),IY( 230)/'DRAW',  -3,   5/
      DATA IOPERA( 231),IX( 231),IY( 231)/'DRAW',  -1,   4/
      DATA IOPERA( 232),IX( 232),IY( 232)/'DRAW',   1,   4/
      DATA IOPERA( 233),IX( 233),IY( 233)/'DRAW',   3,   5/
      DATA IOPERA( 234),IX( 234),IY( 234)/'DRAW',   4,   7/
      DATA IOPERA( 235),IX( 235),IY( 235)/'DRAW',   4,   9/
      DATA IOPERA( 236),IX( 236),IY( 236)/'DRAW',   3,  11/
      DATA IOPERA( 237),IX( 237),IY( 237)/'DRAW',   1,  12/
      DATA IOPERA( 238),IX( 238),IY( 238)/'DRAW',  -1,  12/
C
      DATA IXMIND(  18)/  -7/
      DATA IXMAXD(  18)/   7/
      DATA IXDELD(  18)/  14/
      DATA ISTARD(  18)/ 226/
      DATA NUMCOO(  18)/  13/
C
C     DEFINE CHARACTER   2747--  (NO   SPACE BLANK)
C
      DATA IOPERA( 239),IX( 239),IY( 239)/'MOVE',   0, -32/
      DATA IOPERA( 240),IX( 240),IY( 240)/'MOVE',   0, -32/
C
      DATA IXMIND(  19)/   0/
      DATA IXMAXD(  19)/   0/
      DATA IXDELD(  19)/   0/
      DATA ISTARD(  19)/ 239/
      DATA NUMCOO(  19)/   2/
C
C     DEFINE CHARACTER   2748--  (HALF SPACE BLANK)
C
      DATA IOPERA( 241),IX( 241),IY( 241)/'MOVE',  -4, -32/
      DATA IOPERA( 242),IX( 242),IY( 242)/'MOVE',   4, -32/
C
      DATA IXMIND(  20)/  -4/
      DATA IXMAXD(  20)/   4/
      DATA IXDELD(  20)/   8/
      DATA ISTARD(  20)/ 241/
      DATA NUMCOO(  20)/   2/
C
C     DEFINE CHARACTER   2749--  (FULL SPACE BLANK)
C
      DATA IOPERA( 243),IX( 243),IY( 243)/'MOVE',  -8, -32/
      DATA IOPERA( 244),IX( 244),IY( 244)/'MOVE',   8, -32/
C
      DATA IXMIND(  21)/  -8/
      DATA IXMAXD(  21)/   8/
      DATA IXDELD(  21)/  16/
      DATA ISTARD(  21)/ 243/
      DATA NUMCOO(  21)/   2/
C
C     DEFINE CHARACTER   2252--  (LEFT  APOSTRAPHE)
C
      DATA IOPERA( 245),IX( 245),IY( 245)/'MOVE',   1,  12/
      DATA IOPERA( 246),IX( 246),IY( 246)/'DRAW',   0,  11/
      DATA IOPERA( 247),IX( 247),IY( 247)/'DRAW',  -1,   9/
      DATA IOPERA( 248),IX( 248),IY( 248)/'DRAW',  -1,   7/
      DATA IOPERA( 249),IX( 249),IY( 249)/'DRAW',   0,   6/
      DATA IOPERA( 250),IX( 250),IY( 250)/'DRAW',   1,   7/
      DATA IOPERA( 251),IX( 251),IY( 251)/'DRAW',   0,   8/
C
      DATA IXMIND(  22)/  -5/
      DATA IXMAXD(  22)/   5/
      DATA IXDELD(  22)/  10/
      DATA ISTARD(  22)/ 245/
      DATA NUMCOO(  22)/   7/
C
C     DEFINE CHARACTER   2251--  (RIGHT APOSTRAPHE)
C
      DATA IOPERA( 252),IX( 252),IY( 252)/'MOVE',   0,  10/
      DATA IOPERA( 253),IX( 253),IY( 253)/'DRAW',  -1,  11/
      DATA IOPERA( 254),IX( 254),IY( 254)/'DRAW',   0,  12/
      DATA IOPERA( 255),IX( 255),IY( 255)/'DRAW',   1,  11/
      DATA IOPERA( 256),IX( 256),IY( 256)/'DRAW',   1,   9/
      DATA IOPERA( 257),IX( 257),IY( 257)/'DRAW',   0,   7/
      DATA IOPERA( 258),IX( 258),IY( 258)/'DRAW',  -1,   6/
C
      DATA IXMIND(  23)/  -5/
      DATA IXMAXD(  23)/   5/
      DATA IXDELD(  23)/  10/
      DATA ISTARD(  23)/ 252/
      DATA NUMCOO(  23)/   7/
C
C     DEFINE CHARACTER    XXX--| (KEYBOARD VERTICAL BAR)
C
      DATA IOPERA( 259),IX( 259),IY( 259)/'MOVE',   0,  12/
      DATA IOPERA( 260),IX( 260),IY( 260)/'DRAW',   0,  -9/
C
C
      DATA IXMIND(  24)/  -4/
      DATA IXMAXD(  24)/   4/
      DATA IXDELD(  24)/   8/
      DATA ISTARD(  24)/ 259/
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
   51 FORMAT('***** AT THE BEGINNING OF DPRCS--')
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
 9011 FORMAT('***** AT THE END       OF DPRCS--')
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
      SUBROUTINE DPRCSL(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN COMPLEX SCRIPT LOWER CASE.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
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
   51 FORMAT('***** AT THE BEGINNING OF DPRCSL--')
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
      IF(ICHARN.LE.12)GOTO1010
      GOTO1019
 1010 CONTINUE
      CALL DRCSL1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1019 CONTINUE
C
      IF(13.LE.ICHARN.AND.ICHARN.LE.23)GOTO1020
      GOTO1029
 1020 CONTINUE
      CALL DRCSL2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
      GOTO9000
 1029 CONTINUE
C
      IF(ICHARN.GE.24)GOTO1030
      GOTO1039
 1030 CONTINUE
      CALL DRCSL3(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
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
 9011 FORMAT('***** AT THE END       OF DPRCSL--')
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

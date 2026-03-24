      SUBROUTINE DPPROF(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                        IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--GENERATE A PROFILE PLOT--
!              A MULTIVARIATE TECHNICQUE WHICH PLOTS A STANDARDIZED
!              (0 TO 1) VARIABLE VERSUS DUMMY VARIABLE NUMBER.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--88/2
!     ORIGINAL VERSION--FEBRUARY  1988.
!     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE COMMON
!     UPDATED         --MARCH     2011. USE DPPARS
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
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      CHARACTER*40 INAME
      PARAMETER (MAXSPN=50)
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
      DIMENSION Z1(MAXOBV)
      DIMENSION Z2(MAXOBV)
      DIMENSION Z3(MAXOBV)
      DIMENSION YSUB(MAXOBV)
      DIMENSION YFULL(MAXOBV)
      DIMENSION XTEMP(MAXOBV)
!CCCC FOLLOWING LINES ADDED JUNE, 1990
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(IGARB1),Z1(1))
      EQUIVALENCE (GARBAG(IGARB2),Z2(1))
      EQUIVALENCE (GARBAG(IGARB3),Z3(1))
      EQUIVALENCE (GARBAG(IGARB4),YSUB(1))
      EQUIVALENCE (GARBAG(IGARB5),YFULL(1))
      EQUIVALENCE (GARBAG(IGARB6),XTEMP(1))
!CCCC END CHANGE
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
      ISUBN1='DPPR'
      ISUBN2='OF  '
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
!               ***********************************
!               **  TREAT THE PROFILE PLOT CASE  **
!               ***********************************
!
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
!
!               ***************************
!               **  STEP 1--             **
!               **  EXTRACT THE COMMAND  **
!               ***************************
!
      ISTEPN='11'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PROF')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICASPL='PROF'
!
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PLOT')THEN
        IFOUND='YES'
        ILASTC=1
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      ELSE
        IFOUND='NO'
        GO TO 9000
      ENDIF
!
!               ****************************************
!               **  STEP 2--                          **
!               **  EXTRACT THE VARIABLE LIST         **
!               ****************************************
!
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PROF')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
          DO 285 I=1,NUMVAR
            WRITE(ICOUT,287)I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),   &
                            ICOLR(I),IVARTY(I)
  287       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',   &
                   'ICOLR(I),IVARTY(I) = ',I8,2X,A4,A4,2X,3I8,2X,A4)
            CALL DPWRST('XXX','BUG ')
  285     CONTINUE
        ENDIF
      ENDIF
!
      IWRITE='OFF'
      DO 2200 K=1,NUMVAR
        JF=0
        JS=0
        IMAX=NRIGHT(K)
        IF(NQ.LT.NRIGHT(1))IMAX=NQ
        DO 2210 I=1,IMAX
!
!         CREATE THE "FULL" VARIABLE
!
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
!
!       CREATE THE "SUBSET" VARIABLE
!
        DO 2240 I=1,IMAX
          IF(ISUB(I).EQ.0)GO TO 2240
          JS=JS+1
          IJ=MAXN*(ICOLR(K)-1)+I
!
          IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PROF')THEN
             WRITE(ICOUT,2241)I,JS,MAXN,ICOLR(I),IJ,NRIGHT(I),NQ,IMAX
 2241        FORMAT('I,JS,MAXN,ICOLR(I),IJ,NRIGHT(I),NQ,IMAX = ',8I8)
             CALL DPWRST('XXX','BUG ')
          ENDIF
!
          IF(ICOLR(K).LE.MAXCOL)YSUB(JS)=V(IJ)
          IF(ICOLR(K).EQ.MAXCP1)YSUB(JS)=PRED(I)
          IF(ICOLR(K).EQ.MAXCP2)YSUB(JS)=RES(I)
          IF(ICOLR(K).EQ.MAXCP3)YSUB(JS)=YPLOT(I)
          IF(ICOLR(K).EQ.MAXCP4)YSUB(JS)=XPLOT(I)
          IF(ICOLR(K).EQ.MAXCP5)YSUB(JS)=X2PLOT(I)
          IF(ICOLR(K).EQ.MAXCP6)YSUB(JS)=TAGPLO(I)
!
 2240   CONTINUE
        NSUB=JS
!
        CALL MEDIAN(YSUB,NSUB,IWRITE,XTEMP,MAXN,XMED,IBUGG3,IERROR)
        Z1(K)=XMED
!
 2200 CONTINUE
      NZ=NUMVAR
!
!               ********************************************************
!               **  STEP 31--                                         **
!               **  FORM THE VERTICAL AND HORIZONTAL AXIS             **
!               **  VALUES Y(.) AND X(.) FOR THE PLOT.                **
!               **  DEFINE THE VECTOR D(.) TO 1'S, 2'S, AND 3'S       **
!               **  FOR THE PLOTTED VALUE, THE LOWER CONFIDENCE LINE, **
!               **  AND THE UPPER CONFIDENCE LINE.                    **
!               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).     **
!               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).     **
!               ********************************************************
!
      ISTEPN='8'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PROF')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPPRO2(Z1,Z2,Z3,NZ,ICASPL,   &
                  Y,X,D,NPLOTP,NPLOTV,   &
                  IBUGG3,ISUBRO,IERROR)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
 9014   FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',   &
               I8,I8,I8,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)NSUB,NFULL,NZ,NPLOTP
 9021   FORMAT('NSUB,NFULL,NZ,NPLOTP = ',4I8)
        CALL DPWRST('XXX','BUG ')
        IF(NSUB.GT.0)THEN
          DO 9022 I=1,NSUB
            WRITE(ICOUT,9023)I,YSUB(I)
 9023       FORMAT('I,YSUB(I) = ',I8,E15.7)
            CALL DPWRST('XXX','BUG ')
 9022     CONTINUE
        ENDIF
        IF(NFULL.GT.0)THEN
          DO 9032 I=1,NFULL
            WRITE(ICOUT,9033)I,YFULL(I)
 9033       FORMAT('I,YFULL(I) = ',I8,E15.7)
            CALL DPWRST('XXX','BUG ')
 9032     CONTINUE
        ENDIF
        IF(NZ.GT.0)THEN
          DO 9042 I=1,NZ
            WRITE(ICOUT,9043)I,Z1(I),Z2(I),Z3(I)
 9043       FORMAT('I,Z1(I),Z2(I),Z3(I) = ',I8,3G15.7)
            CALL DPWRST('XXX','BUG ')
 9042     CONTINUE
        ENDIF
        IF(NPLOTP.GT.0)THEN
          DO 9052 I=1,NPLOTP
            WRITE(ICOUT,9053)I,Y(I),X(I),D(I)
 9053       FORMAT('I,Y(I),X(I),D(I) = ',I8,3F12.5)
            CALL DPWRST('XXX','BUG ')
 9052     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPPROF
      SUBROUTINE DPPROJ(ICOM,IHARG,NUMARG,I3DPRO,   &
      IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE 3-D PROJECTION SWITCH I3DPRO.
!              THE 2 SETTINGS ARE
!                 1) ORTHOGRAPHIC (THE DEFAULT)
!                 2) PERSPECTIVE
!     INPUT  ARGUMENTS--ICOM
!                     --IHARG  (A  HOLLERITH VECTOR)
!                     --NUMARG
!     OUTPUT ARGUMENTS--I3DPRO   ('ORTH'  OR 'PERS')
!                     --IFOUND ('YES' OR 'NO' )
!                     --IERROR ('YES' OR 'NO' )
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DI3DPROION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--88/10
!     ORIGINAL VERSION--SEPTEMBER 1988.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
      CHARACTER*4 I3DPRO
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
      IF(ICOM.EQ.'ORTH')GO TO 1110
      IF(ICOM.EQ.'PERS')GO TO 1120
      IF(ICOM.EQ.'PROJ')GO TO 1130
!
 1110 CONTINUE
      IF(NUMARG.LE.0)GO TO 1150
      IF(IHARG(1).EQ.'ON')GO TO 1150
      IF(IHARG(1).EQ.'OFF')GO TO 1160
      GO TO 1199
!
 1120 CONTINUE
      IF(NUMARG.LE.0)GO TO 1160
      IF(IHARG(1).EQ.'ON')GO TO 1160
      IF(IHARG(1).EQ.'OFF')GO TO 1150
      GO TO 1199
!
 1130 CONTINUE
      IF(NUMARG.LE.0)GO TO 1150
      IF(IHARG(1).EQ.'ON')GO TO 1150
      IF(IHARG(1).EQ.'OFF')GO TO 1160
      IF(IHARG(1).EQ.'AUTO')GO TO 1150
      IF(IHARG(1).EQ.'DEFA')GO TO 1150
      IF(IHARG(1).EQ.'ORTH')GO TO 1150
      IF(IHARG(1).EQ.'PERS')GO TO 1160
      GO TO 1199
!
 1150 CONTINUE
      I3DPRO='ORTH'
      GO TO 1180
!
 1160 CONTINUE
      I3DPRO='PERS'
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
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
      GO TO 1199
!
 1199 CONTINUE
      RETURN
      END SUBROUTINE DPPROJ
      SUBROUTINE DPPROM(IHARG,NUMARG,IPROSW,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE PROMPT SWITCH IPROSW.
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --NUMARG
!     OUTPUT ARGUMENTS--IPROSW  ('ON'  OR 'OFF')
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
!     VERSION NUMBER--86/1
!     ORIGINAL VERSION--DECEMBER  1985.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IPROSW
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
      IF(NUMARG.EQ.0)GO TO 1150
      IF(NUMARG.GE.1)GO TO 1110
      GO TO 1199
!
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1150
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1160
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1150
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1150
      GO TO 1199
!
 1150 CONTINUE
      IPROSW='ON'
      GO TO 1180
!
 1160 CONTINUE
      IPROSW='OFF'
      GO TO 1180
!
 1180 CONTINUE
      IFOUND='YES'
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)IPROSW
 1181 FORMAT('THE PROMPT SWITCH HAS JUST BEEN TURNED ',   &
      A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 1199
!
 1199 CONTINUE
      RETURN
      END SUBROUTINE DPPROM
      SUBROUTINE DPPRO2(Z1,Z2,Z3,NZ,ICASPL,   &
      Y2,X2,D2,N2,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
!     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
!              THAT WILL DEFINE
!              A PROFILE PLOT
!              (USEFUL FOR MULTIVARIATE ANALYSIS).
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--88/2
!     ORIGINAL VERSION--JANUARY   1988.
!     UPDATED         --APRIL     1992.  DELETE K
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
      DIMENSION Z1(*)
      DIMENSION Z2(*)
      DIMENSION Z3(*)
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
      ISUBN1='DPPR'
      ISUBN2='O2  '
      IERROR='NO'
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
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
        GO TO 9000
      ENDIF
!
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
          DO 81 I=1,NZ
            WRITE(ICOUT,82)I,Z1(I),Z2(I),Z3(I)
   82       FORMAT('I,Z1(I),Z2(I),Z3(I) = ',I8,3F15.7)
            CALL DPWRST('XXX','BUG ')
   81     CONTINUE
        ENDIF
      ENDIF
!
!               ****************************************
!               **  STEP 11--                         **
!               **  DETERMINE PLOT COORDINATES        **
!               ****************************************
!
      J=0
      DO 1100 I=1,NZ
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
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
        DO 9035 I=1,N2
          WRITE(ICOUT,9036)I,Y2(I),X2(I),D2(I)
 9036     FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,2G15.7,F9.2)
          CALL DPWRST('XXX','BUG ')
 9035   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPPRO2
      SUBROUTINE DPPRPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                        IANGLU,MAXNPP,   &
                        ICONT,NUMHPP,NUMVPP,IMANUF,   &
                        XMATN,YMATN,XMITN,YMITN,   &
                        ISQUAR,   &
                        IVGMSW,IHGMSW,   &
                        IMPSW,IMPNR,IMPNC,IMPCO,   &
                        PMXMIN,PMXMAX,PMYMIN,PMYMAX,   &
                        MAXNXT,ALOWFR,ALOWDG,IFORSW,   &
                        ANOPL1,ANOPL2,ISEED,IBOOSS,BARHEF,BARWEF,   &
                        ICAPSW,   &
                        IBUGG2,IBUGG3,IBUGCO,IBUGEV,IBUGQ,   &
                        IBUGUG,IBUGU2,IBUGU3,IBUGU4,ISUBRO,   &
                        IFOUND,IERROR)
!
!     PURPOSE--GENERATE EITHER
!              1) A PARTIAL REGRESSION PLOT
!              2) A PARTIAL LEVERAGE PLOT
!              3) A PARTIAL RESIDUAL PLOT
!              4) A CCPR PLOT
!              FOR EXAMPLE, THE COMMAND
!                 PARTIAL REGRESSION PLOT Y X1 TO XK
!              WILL GENERATE PARTIAL REGRESSION PLOTS OF Y VS X1,
!              Y VS X2, ETC. AS A MULTIPLOT ON A SINGLE PAGE.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2002/6
!     ORIGINAL VERSION--JUNE      2002.
!     UPDATED         --FEBRUARY  2005. CALL LIST TO MAINAN
!     UPDATED         --MARCH     2006. CALL LIST TO MAINGR
!     UPDATED         --AUGUST    2007. CALL LIST TO MAINGR
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES---------------
!
      INCLUDE 'DPCOPA.INC'
!
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
!
      CHARACTER*4 IBUGG2
      CHARACTER*4 IBUGG3
      CHARACTER*4 IBUGUG
      CHARACTER*4 IBUGU2
      CHARACTER*4 IBUGU3
      CHARACTER*4 IBUGU4
      CHARACTER*4 IBUGCO
      CHARACTER*4 IBUGEV
      CHARACTER*4 IBUGQ
!
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
!
!  MAXY IS THE MAXIMUM NUMBER OF VARIABLES TO USE IN CREATING THE
!  PARTIAL REGRESSION PLOT   CURVE
!
      PARAMETER(MAXY=50)
      CHARACTER*40 INAME
      CHARACTER*4 IVARN1(MAXY)
      CHARACTER*4 IVARN2(MAXY)
      CHARACTER*4 IVARTY(MAXY)
      DIMENSION ILIS(MAXY)
      DIMENSION PVAR(MAXY)
      DIMENSION NRIGHT(MAXY)
      DIMENSION ICOLL(MAXY)
!
      DIMENSION TEMP(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TEMP3(MAXOBV)
      DIMENSION XTEMP1(MAXOBV)
      DIMENSION XTEMP2(MAXOBV)
!
!-----COMMON------------------------------------------------------
!
      INCLUDE 'DPCOZ3.INC'
      INCLUDE 'DPCOPC.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOSP.INC'
!
      EQUIVALENCE (G3RBAG(KGARB1),TEMP(1))
      EQUIVALENCE (G3RBAG(KGARB2),TEMP2(1))
      EQUIVALENCE (G3RBAG(KGARB3),TEMP3(1))
      EQUIVALENCE (G3RBAG(KGARB4),XTEMP1(1))
      EQUIVALENCE (G3RBAG(KGARB5),XTEMP2(1))
!
!-----COMMON VARIABLES (GENERAL)----------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-------------------------------------------------
!
      IFOUND='YES'
      IERROR='NO'
      ISUBN1='DPPR'
      ISUBN2='PL  '
!
      IF(ICASPL.NE.'CCPR')ICASPL='PRPL'
      IFPLLD='ON'
      IFPLDI='LINE'
      IBOOSS=100
!
      IFLAGV=5
!
!               ***********************************************
!               **  TREAT THE PARTIAL REGRESSION PLOT   CASE **
!               ***********************************************
!
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PRPL')THEN
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
          DO 61 I=1,NUMARG
            WRITE(ICOUT,62)I,IHARG(I),IARGT(I)
   62       FORMAT('I,IHARG(I),IARGT(I) = ',I8,2X,A4,2X,A4)
            CALL DPWRST('XXX','BUG ')
   61     CONTINUE
        ENDIF
        WRITE(ICOUT,71)IFPLLA,IFPLTA,IFPLPT,IFPLFI,IFPLFR
   71   FORMAT('IFPLLA,IFPLTA,IFPLPT,IFPLFI,IFPLFR = ',5(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ******************************************************
!               **  STEP 1--                                        **
!               **  SHIFT COMMAND LINE ARGMENTS                     **
!               ******************************************************
!
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PRPL')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'REGR'.AND.IHARG(2).EQ.'PLOT')THEN
        ICASPL='PREG'
        ISHIFT=2
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGG2,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!  SYNONYM: ADDED VARIABLE PLOT
!
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'VARI'.AND.IHARG(2).EQ.'PLOT')THEN
        ICASPL='PREG'
        ISHIFT=2
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGG2,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'LEVE'.AND.IHARG(2).EQ.'PLOT')THEN
        ICASPL='PLEV'
        ISHIFT=2
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGG2,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'RESI'.AND.IHARG(2).EQ.'PLOT')THEN
        ICASPL='PRES'
        ISHIFT=2
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGG2,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
      IF(ICASPL.EQ.'CCPR'.AND.NUMARG.GE.1.AND.IHARG(1).EQ.'PLOT')THEN
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGG2,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
!  SYNONYM: COMPONENT PLUS RESIDUAL PLOT
!
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'PLUS'.AND.IHARG(2).EQ.'RESI'.AND.   &
         IHARG(3).EQ.'PLOT')THEN
        ICASPL='PRES'
        ISHIFT=3
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                    IBUGG2,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
      ICOM='FIT '
      ICOM2='    '
      IFOUND='YES'
!
!               *******************************************************
!               **  STEP 2--                                         **
!               **  CHECK FOR THE PROPER NUMBER OF INPUT ARGUMENTS.  **
!               *******************************************************
!
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PRPL')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
!
      CALL DPPARS(IHARG,IHARG2,IARGT,ARG,NUMARG,IANS,IWIDTH,   &
                  IHNAME,IHNAM2,IUSE,NUMNAM,IN,IVALUE,VALUE,   &
                  JMIN,JMAX,   &
                  MINN2,MINNA,MAXNA,MAXY,IFLAGE,INAME,   &
                  IVARN1,IVARN2,IVARTY,PVAR,   &
                  ILIS,NRIGHT,ICOLL,ISUB,NQ,ILOCQ,NUMVAR,   &
                  MINNVA,MAXNVA,   &
                  IFLAGM,IFLAGP,   &
                  IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
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
          DO 285 I=1,NUMVAR
            WRITE(ICOUT,287)I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),   &
                            ICOLL(I)
  287       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',   &
                   'ICOLL(I) = ',I8,2X,A4,A4,2X,3I8)
            CALL DPWRST('XXX','BUG ')
  285     CONTINUE
        ENDIF
      ENDIF
!
!               **************************************************
!               **   STEP 0.5--                                 **
!               **   PERFORM MULTILINEAR FIT                    **
!               **************************************************
!
      ISTEPN='0.5'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'PRPL')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICPSWZ='OFF'
      CALL MAINAN(ICASAN,ISEED,ANOPL1,ANOPL2,   &
      TEMP,TEMP2,XTEMP1,XTEMP2,MAXNXT,   &
      IFTEXP,IFTORD,   &
      ALOWFR,ALOWDG,   &
      IBOOSS,   &
      ICPSWZ,   &
      IFORSW,   &
      IBUGG2,IBUGG2,IBUGG3,   &
      IBUGCO,IBUGEV,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!               **************************************************
!               **   STEP 1--                                   **
!               **   SAVE INITIAL SETTINGS                      **
!               **************************************************
!
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'PRPL')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IFLAG=1
      CALL DPSPM5(IFLAG,IMPSW,IMPCO,IMPNR,IMPNC,   &
                  IBUGG2,ISUBRO,IFOUND,IERROR)
!
      ILFLAX='OFF'
      ILFLAY='OFF'
      IF(IY1MIN.EQ.'FIXE'.AND.IY1MAX.EQ.'FIXE')THEN
        ILFLAY='ON'
      ENDIF
      IF(IX1MIN.EQ.'FIXE'.AND.IX2MAX.EQ.'FIXE')THEN
        ILFLAX='ON'
      ENDIF
!
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
!
      IFEED9=IFEEDB
!
      IMPSW3=IMPSW
      IMPCO2=IMPCO
      IMPNR2=IMPNR
      IMPNC2=IMPNC
      IMPSW='ON'
      IMPCO=1
      IMPCO9=IMPCO
!
      NPLOTS=NUMVAR-1
!
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
!
      IROWT=IMPNR
      ICOLT=IMPNC
      IF(IFPLLA.EQ.'BOX')THEN
        IMPNR=IMPNR+1
        IMPNC=IMPNC+1
        IROWT=IROWT+1
        ICOLT=ICOLT+1
      ENDIF
!
!               *************************************
!               **   STEP 21--                     **
!               **   GENERATE THE PLOTS            **
!               *************************************
!
      ISTEPN='21'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DPPRPL')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
      GO TO 5299
!
!               **************************************************
!               **   GENERATE ONE OF THE FOLLOWING COMMANDS     **
!               **      PARTIAL REGRESSION PLOT Y X1 X2 .... XI **
!               **      PARTIAL RESIDUAL   PLOT Y X1 X2 .... XI **
!               **      PARTIAL LEVERAGE   PLOT Y X1 X2 .... XI **
!               **   WHERE XI IS THE SPECIFIC VARIABLE THE      **
!               **   PLOT IS BEING GENERATED FOR.               **
!               **************************************************
 5299 CONTINUE
!
      IF(NPLOTS.LT.1)GO TO 8000
!
      ISHIFT=NCCOMM
      CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                  IBUGG2,IERROR)
      ICOM=ICT
      ICOM2=IC2T
      IF(NCCOMM.GT.0)THEN
        DO 5301 II=1,NCCOMM
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
!
      IPLOT=0
      IF(IFPLLA.EQ.'BOX')THEN
        NPLOTS=NPLOTS+IMPNR+IMPNC-1
      ENDIF
      DO 5300 IRES=1,IROWT
        DO 5400 IFAC=1,ICOLT
!
          IPLOT=IPLOT+1
          IF(IPLOT.GT.NPLOTS)GO TO 8000
          IHARG(NUMARG)=IHARG(IFRST+IPLOT-1)
          IHARG2(NUMARG)=IHARG2(IFRST+IPLOT-1)
          IARG(NUMARG)=IARG(IFRST+IPLOT-1)
          ARG(NUMARG)=ARG(IFRST+IPLOT-1)
          IARGT(NUMARG)=IARGT(IFRST+IPLOT-1)
!
          IXLIST=IFAC
          IROW=INT(IPLOT/IMPNC)+1
          IF(MOD(IPLOT,IMPNC).EQ.0)IROW=IROW-1
          ICOL=MOD(IPLOT,IMPNC)
          IF(ICOL.EQ.0)ICOL=IMPNC
!
          IEMPTY='NO'
          ITEMP=IFAC
          IF(IFPLLA.EQ.'BOX')THEN
            ICOL=ICOL-1
            ITEMP=IFAC-1
            IF(ITEMP.EQ.0)IEMPTY='YES'
            IF(IROW.EQ.IMPNR)IEMPTY='YES'
          ENDIF
!
          IF(IEMPTY.EQ.'YES')THEN
            DO 5304 I=1,MAXSUB
              ISU2SW(I)=ISUBSW(I)
              ISUBSW(I)='OFF'
 5304       CONTINUE
          ENDIF
          IOPTN=3
          IDX=1
          IDY=1
          ICASP2='FACT'
!
!CCCC NOTE: DPSPM4 IMPLEMENTS "SUB-REGIONS" ON PLOTS.  THESE DON'T
!CCCC       SEEM PARTICULARLY RELEVANT FOR THESE PLOTS, SO COMMENT
!CCCC       OUT FOR NOW.  HOWEVER, LEAVE IN CASE WE DECIDE LATER TO
!CCCC       IMPLEMENT THEM.
!
!CCCC     CALL DPSPM4(ICASP2,IOPTN,IDX,IDY,
!CCCC1                ISUBNU,ISUBSW,
!CCCC1                ASUBXL,ASUBXU,ASUBYL,ASUBYU,
!CCCC1                ISUBN9,ISUBSZ,
!CCCC1                ASBXL2,ASBXU2,ASBYL2,ASBYU2,
!CCCC1                PFPXSL,PFPXSU,PFPYSL,PFPYSU,
!CCCC1                IBUGG2,ISUBRO,IERROR)
!
          ICASP2=ICASPL
          IRES2=IRES
          IXLST2=IXLIST+1
          IX=IFAC+1
          CALL DPSPM1(ICASP2,IVARN1,IVARN2,ICOLL,   &
                      IMPNR,IMPNC,IROW,ICOL,IRES2,IX,IPLOT,   &
                      NPLOTS,NUMVAR,   &
                      ICHAP2,ILINP2,   &
                      GY1MNS,GY1MXS,GY2MNS,GY2MXS,   &
                      GX1MNS,GX1MXS,GX2MNS,GX2MXS,   &
                      IY1MNS,IY1MXS,IY2MNS,IY2MXS,   &
                      IX1MNS,IX1MXS,IX2MNS,IX2MXS,   &
                      IX1TSV,IX2TSV,IY1TSV,IY2TSV,   &
                      IX1ZSV,IX2ZSV,IY1ZSV,IY2ZSV,   &
                      PX1LD2,PX2LD2,   &
                      IY1LJ2,IY1LD2,PY1LD2,PY1LA2,   &
                      IX1LT2,IX2LT2,IY1LT2,IY2LT2,   &
                      NCX1L2,NCX2L2,NCY1L2,NCY2L2,   &
                      PFPXLL,PFPXUL,PFPYLL,PFPYUL,IXLST2,   &
                      IFPLLA,IFPLLD,IPLOTT,IFPLFR,IFPLXA,IFPLYA,   &
                      IFPLDI,ISPX1L,   &
                      ISPMXT,ISPMXL,ISPMYT,ISPMYL,   &
                      IFPLTD,PFPLTD,IVNMEX,   &
                      IBUGG2,ISUBRO)
!
          IF(IEMPTY.EQ.'YES')THEN
            DO 5306 I=1,100
              ICHAPA(I)='BLAN'
              ILINPA(I)='BLAN'
              ISPISW(I)='OFF'
              IBARSW(I)='OFF'
 5306       CONTINUE
          ENDIF
!
          CALL MAINGR(ANOPL1,ANOPL2,NPLOTV,NPLOTP,NS,ICASPL,   &
                      MAXNPP,ISEED,IBOOSS,   &
                      IX1TSV,IX2TSV,IY1TSV,IY2TSV,   &
                      IX1ZSV,IX2ZSV,IY1ZSV,IY2ZSV,   &
                      BARHEF,BARWEF,   &
                      IRHSTG,IHSTCW,IHSTEB,IHSTOU,IASHWT,IHSTMC,IHSTOP,   &
                      ICAPSW,IFORSW,   &
                      IGUIFL,IERRFA,IFRALI,   &
                      IAND1,IAND2,ICONT,NUMHPP,NUMVPP,   &
                      MAXNXT,   &
                      ISUBRO,IFOUND,IERROR)
!
!CCCC NOTE: DPSPM3 SETS AN X2LABEL BASED ON CORRELATION, EFFECT
!CCCC       SIZE, OR NUMBER OF REJECTIONS.  THIS DOESN"T SEEM
!CCCC       PARTICULARLY USEFUL FOR THESE PLOTS, SO COMMENT OUT
!CCCC       FOR NOW.  HOWEVER, LEAVE CODE HERE IN CASE WE DECIDE TO
!CCCC       ACTIVATE LATER.
!
!CCCC     IF(IEMPTY.EQ.'NO')THEN
!CCCC       CALL DPSPM3(ICASPL,IOUNI5,
!CCCC1                  IROW,ICOL,
!CCCC1                  PX2LD2,NPLOTP,
!CCCC1                  IFORSW,
!CCCC1                  IFPX2L,ISPX2P,ISPX2S,
!CCCC1                  IHRIGH,IHRIG2,IHWUSE,
!CCCC1                  ISUBN1,ISUBN2,MESSAG,
!CCCC1                  IBUGG2,ISUBRO,IERROR)
!CCCC     ENDIF
!
          ICONT=IDCONT(1)
          IPOWE=IDPOWE(1)
          NUMHPP=IDNHPP(1)
          IMPARG=2
          CALL DPGRAP(Y,X,X3D,D,N,NPLOTP,ICASPL,ICONT,IPOWE,NUMHPP,   &
                      XMATN,YMATN,XMITN,YMITN,   &
                      ISQUAR,   &
                      IVGMSW,IHGMSW,   &
                      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,   &
                      IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,   &
                      YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                      IMPSW,IMPNR,IMPNC,IMPCO,IMPCO9,   &
                      IMPARG,   &
                      PMXMIN,PMXMAX,PMYMIN,PMYMAX,   &
                      MAXCOL,   &
                      DSIZE,DSYMB,DCOLOR,DFILL,   &
                      ICAPSW,   &
                      IBUGUG,IBUGU2,IBUGU3,IBUGU4,ISUBRO,   &
                      IERROR)
          IF(IERROR.EQ.'NO')IAND1=IAND2
          IF(IERROR.EQ.'YES')GO TO 5499
!
          IF(IFPLFI.EQ.'NONE')GO TO 5499
          IF(IEMPTY.EQ.'YES')GO TO 5499
!
          IMPCO=IMPCO-1
          IF(IMPCO.LE.1)IERASW='OFF'
!
          ICNTPL=0
          IOUNI5=-99
          CALL DPSPM2(ICASPL,IVARN1,IVARN2,ICOLL,NUMVAR,NPLOTP,   &
                      IRES,IX,   &
                      TEMP,TEMP2,TEMP3,XTEMP1,XTEMP2,MAXNXT,   &
                      ALOWFR,ALOWDG,   &
                      IANGLU,MAXNPP,IAND1,IAND2,   &
                      IFPLFI,IFPLTA,   &
                      XMATN,YMATN,XMITN,YMITN,   &
                      ISQUAR,   &
                      IVGMSW,IHGMSW,   &
                      IMPSW,IMPNR,IMPNC,IMPCO,IMPCO9,   &
                      IREPCH,   &
                      PMXMIN,PMXMAX,PMYMIN,PMYMAX,   &
                      ICNTPL,IOUNI5,   &
                      IBUGG2,IBUGG3,IBUGCO,IBUGEV,IBUGQ,   &
                      IBUGUG,IBUGU2,IBUGU3,IBUGU4,   &
                      ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GO TO 5499
                                                                                                                                  
 5499     CONTINUE
          IERROR='NO'
!
          ISHIFT=NCCOMM
          CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                      IBUGG2,IERROR)
          ICOM=ICT
          ICOM2=IC2T
          IF(NCCOMM.GT.0)THEN
            DO 5491 II=1,NCCOMM
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
!
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
          DO 5407 I=1,MAXSUB
            ISUBSW(I)=ISU2SW(I)
 5407     CONTINUE
        ENDIF
        DO 5408 I=1,100
            ICHAPA(I)=ICHAP2(I)
            ILINPA(I)=ILINP2(I)
            ISPISW(I)=ISPIS2(I)
            IBARSW(I)=IBARS2(I)
 5408   CONTINUE
        IF(IERROR.EQ.'YES')GO TO 5400
!
 5400 CONTINUE
 5300 CONTINUE
      GO TO 8000
!
!
!               **************************************************
!               **   STEP 28--                                  **
!               **   REINSTATE INITIAL SETTINGS                 **
!               **************************************************
!
 8000 CONTINUE
!
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'PRPL')THEN
        ISTEPN='28'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,8807)IMANUF,NUMDEV,IDMANU(1)
 8807   FORMAT('IMANUF,NUMDEV,IDMANU(1) = ',A4,I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IFLAG=2
      CALL DPSPM5(IFLAG,IMPSW,IMPCO,IMPNR,IMPNC,   &
                  IBUGG2,ISUBRO,IFOUND,IERROR)
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
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGG2.EQ.'OFF')GO TO 9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPPRPL--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IFOUND,IERROR
 9012 FORMAT('IFOUND,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9013 FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',   &
      I8,I8,I8,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)NUMARG
 9014 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NUMARG.LE.0)GO TO 9029
      DO 9021 I=1,NUMARG
      WRITE(ICOUT,9022)I,IHARG(I),IARGT(I)
 9022 FORMAT('I,IHARG(I),IARGT(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9021 CONTINUE
 9029 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPPRPL
      SUBROUTINE DPPRPO(ICOM,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                        IPPDE1,IPPDE2,   &
                        IBUGS2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE PREPLOT/POSTPLOT DEVICE
!              THAT IS, THE CURRENT DEVICE IN WHICH
!              THE USER WANTS A USER-SPECIFIED
!              PREPLOT LINE TO BE WRITTEN OUT,
!              AND A USER-DEFINED POSTPLOT LINE
!              TO BE WRITTEN OUT.
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --NUMARG (AN INTEGER VARIABLE)
!     OUTPUT ARGUMENTS--IPPDE1  (A HOLLERITH VARIABLE)
!                       IPPDE2  (A HOLLERITH VARIABLE)
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
!     VERSION NUMBER--86/9
!     ORIGINAL VERSION--OCTOBER  1986.
!     UPDATED         --FEBRUARY 1987.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
      CHARACTER*4 IHARG2
!CCCC CHARACTER*4 IARG   JULY 1987
!CCCC CHARACTER*4 ARG     JULY 1987
      CHARACTER*4 IARGT
!
      CHARACTER*4 IPPDE1
      CHARACTER*4 IPPDE2
      CHARACTER*4 IBUGS2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHOLD1
      CHARACTER*4 IHOLD2
!
      CHARACTER*4 IHARG1
!
!---------------------------------------------------------------------
!
      DIMENSION IHARG(*)
      DIMENSION IHARG2(*)
      DIMENSION IARG(*)
      DIMENSION ARG(*)
      DIMENSION IARGT(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      IFOUND='YES'
!
      IHARG1=IHARG(1)
!
      IF(ICOM.EQ.'PRE')GO TO 1109
      IF(ICOM.EQ.'PREP')GO TO 1109
      IF(ICOM.EQ.'POST')GO TO 1109
      ISHIFT=1
      CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
      IBUGS2,IERROR)
 1109 CONTINUE
!
      IF(NUMARG.LE.0)GO TO 1120
!
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1120
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1120
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1120
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1120
!
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'POST')GO TO 1120
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'DEVI')GO TO 1120
      IF(NUMARG.EQ.1)GO TO 1130
!
      IF(NUMARG.EQ.2.AND.IHARG(1).EQ.'POST'   &
                    .AND.IHARG(2).EQ.'DEVI')GO TO 1120
      IF(NUMARG.EQ.2.AND.IHARG(1).EQ.'POST'   &
                    .AND.IHARG(2).NE.'DEVI')GO TO 1130
      IF(NUMARG.EQ.2.AND.IHARG(1).EQ.'DEVI')GO TO 1130
!
      IF(NUMARG.EQ.3.AND.IHARG(1).EQ.'POST'   &
                    .AND.IHARG(2).EQ.'DEVI')GO TO 1130
      IF(NUMARG.EQ.3.AND.IHARG(1).EQ.'POST'   &
                    .AND.IHARG(2).NE.'DEVI')GO TO 1140
      IF(NUMARG.EQ.3.AND.IHARG(1).EQ.'DEVI')GO TO 1140
!
      GO TO 1140
!
 1120 CONTINUE
      IHOLD1='NONE'
      IHOLD2='    '
      GO TO 1180
!
 1130 CONTINUE
      IHOLD1=IHARG(NUMARG)
      IHOLD2='    '
      GO TO 1180
!
 1140 CONTINUE
      NUMAM1=NUMARG-1
      IHOLD1=IHARG(NUMAM1)
      IHOLD2=IHARG(NUMARG)
      GO TO 1180
!
 1180 CONTINUE
      IPPDE1=IHOLD1
      IPPDE2=IHOLD2
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1188)IPPDE1,IPPDE2
 1188 FORMAT('THE PREPLOT/POSTPLOT DEVICE HAS JUST BEEN SET TO ',   &
      A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 1199
!
 1199 CONTINUE
      RETURN
      END SUBROUTINE DPPRPO
      SUBROUTINE DPPRSW(IHARG,NUMARG,IPRIN2,IFOUND,IERROR)
!
!     PURPOSE--SPECIFY THE PRINTING SWITCH WHICH IN TURN
!              DETERMINES WHETHER ANY SUBSEQUENT NON-GRAPHICAL OUTPUT
!              WILL BE PRINTED OR NOT.
!              THIS CAPABILITY IS USEFUL IF ONE WISHES TO SUPPRESS
!              OUTPUT FROM ALL PRELIMINARY AND INTERMEDIATE
!              CALCULATIONS AND JUST HAVE THE FINAL PLOTS THEMSELVES
!              APPEAR ON THE SCREEN.
!              THE SPECIFIED PRINTING SWITCH SPECIFICATION
!              WILL BE PLACED IN THE HOLLERITH VARIABLE IPRIN2.
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --NUMARG (AN INTEGER VARIABLE)
!     OUTPUT ARGUMENTS--IPRIN2 (A HOLLERITH VARIABLE)
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
!     ORIGINAL VERSION--NOVEMBER  1980.
!     UPDATED         --FEBRUARY  1982.
!     UPDATED         --MAY       1982.
!     UPDATED         --JANUARY   2015. SAVE/RESTORE OPTION
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IPRIN2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHOLD
!
      CHARACTER*4 IPRISV
      COMMON/IPRINT/IPRISV
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
      IHOLD='    '
!
      IF(NUMARG.LE.0 .OR. IHARG(NUMARG).EQ.'ON' .OR.   &
         IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA')THEN
        IHOLD='ON'
        GO TO 1180
      ELSEIF(IHARG(NUMARG).EQ.'OFF')THEN
        IHOLD='OFF'
        GO TO 1180
      ELSEIF(IHARG(NUMARG).EQ.'SAVE')THEN
        IPRISV=IPRINT
        GO TO 1199
      ELSEIF(IHARG(NUMARG).EQ.'REST')THEN
        IPRINT=IPRISV
        GO TO 1180
      ELSE
        GO TO 1199
      ENDIF
!
 1180 CONTINUE
      IFOUND='YES'
      IPRIN2=IHOLD
      IPRINT=IPRIN2
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1181)IPRIN2
 1181   FORMAT('THE PRINTING SWITCH HAS JUST BEEN SET TO ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GO TO 1199
!
 1199 CONTINUE
      RETURN
      END SUBROUTINE DPPRSW
      SUBROUTINE DPPTES(MAXNXT,ICAPSW,IFORSW,ISEED,   &
                        IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--CARRY OUT A TWO-SAMPLE <STAT> PERMUTATION TEST
!              OR THE K-SAMPLE <STAT> PERMUATION TEST
!     EXAMPLE--TWO SAMPLE MEAN PERMUATION TEST Y1 Y2
!            --TWO SAMPLE MEDIAN PERMUATION TEST Y1 Y2
!            --K SAMPLE ONE WAY ANOVA F STATISTIC PERMUATION TEST Y X
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2023/08
!     ORIGINAL VERSION--AUGUST    2023.
!     UPDATED         --SEPTEMBER 2023. SUPPORT FOR K SAMPLE TEST
!     UPDATED         --OCTOBER   2024. CHECK FOR CONFLICT WITH
!                                       TWO SAMPLE KOLM SMIR AND
!                                       TWO SAMPLE CHI-SQUARE
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
      CHARACTER*4 ICASE1
      CHARACTER*4 ICASE2
      CHARACTER*4 ICASE3
      CHARACTER*4 ICTMP1
      CHARACTER*4 ICTMP2
      CHARACTER*4 ICTMP3
      CHARACTER*4 IFOUN2
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 IHOST1
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 ICASE
      CHARACTER*4 ISTACS
      CHARACTER*4 ISTADF
      CHARACTER*4 ISTARA
      CHARACTER*60 ISTANM
!
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
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZD.INC'
      INCLUDE 'DPCOZI.INC'
!
      DIMENSION YCOMB(2*MAXOBV)
      DIMENSION TAG(2*MAXOBV)
      DIMENSION TAG1(2*MAXOBV)
      DIMENSION Y1(MAXOBV)
      DIMENSION Y2(MAXOBV)
      DIMENSION YSAVE(MAXOBV)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TEMP3(MAXOBV)
      DIMENSION TEMP4(MAXOBV)
      DIMENSION TEMP5(MAXOBV)
      DIMENSION TEMP6(MAXOBV)
      DIMENSION TEMP7(MAXOBV)
      DIMENSION TEMP8(MAXOBV)
      DIMENSION TEMP9(MAXOBV)
!
      DOUBLE PRECISION DTEMP1(MAXOBV)
      DOUBLE PRECISION DTEMP2(MAXOBV)
      DOUBLE PRECISION DTEMP3(MAXOBV)
!
      INTEGER ITEMP1(MAXOBV)
      INTEGER ITEMP2(MAXOBV)
      INTEGER ITEMP3(MAXOBV)
      INTEGER ITEMP4(MAXOBV)
      INTEGER ITEMP5(MAXOBV)
      INTEGER ITEMP6(MAXOBV)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
!
      EQUIVALENCE (GARBAG(IGARB1),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB2),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB3),TEMP3(1))
      EQUIVALENCE (GARBAG(IGARB4),TEMP4(1))
      EQUIVALENCE (GARBAG(IGARB5),TEMP5(1))
      EQUIVALENCE (GARBAG(IGARB6),TEMP6(1))
      EQUIVALENCE (GARBAG(IGARB7),YSAVE(1))
      EQUIVALENCE (GARBAG(IGARB9),YCOMB(1))
      EQUIVALENCE (GARBAG(JGAR11),TAG(1))
      EQUIVALENCE (GARBAG(JGAR13),TAG1(1))
      EQUIVALENCE (GARBAG(JGAR15),Y1(1))
      EQUIVALENCE (GARBAG(JGAR16),Y2(1))
      EQUIVALENCE (GARBAG(JGAR17),TEMP7(1))
      EQUIVALENCE (GARBAG(JGAR18),TEMP8(1))
      EQUIVALENCE (GARBAG(JGAR19),TEMP9(1))
!
      EQUIVALENCE (IGARBG(IIGAR1),ITEMP1(1))
      EQUIVALENCE (IGARBG(IIGAR2),ITEMP2(1))
      EQUIVALENCE (IGARBG(IIGAR3),ITEMP3(1))
      EQUIVALENCE (IGARBG(IIGAR4),ITEMP4(1))
      EQUIVALENCE (IGARBG(IIGAR5),ITEMP5(1))
      EQUIVALENCE (IGARBG(IIGAR6),ITEMP6(1))
!
      EQUIVALENCE (DGARBG(IDGAR1),DTEMP1(1))
      EQUIVALENCE (DGARBG(IDGAR2),DTEMP2(1))
      EQUIVALENCE (DGARBG(IDGAR3),DTEMP3(1))
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPPT'
      ISUBN2='ES  '
      IFOUND='YES'
      IERROR='NO'
!
      MAXNXT=MAXOBV
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
!               ******************************************
!               **  TREAT THE PERMUTATION TEST CASE     **
!               ******************************************
!
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'PTES')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPPTES--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA2,IBUGA3,IBUGQ,ISUBRO,MAXNXT
   52   FORMAT('IBUGA2,IBUGA3,IBUGQ,ISUBRO,MAXNXT = ',4(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *********************************************************
!               **  STEP 1--                                           **
!               **  EXTRACT THE COMMAND                                **
!               *********************************************************
!
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PTES')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ILASTZ=9999
      ICASAN='PTES'
      ICASE2='TWOT'
      ICASE1='TEST'
      ICASE3='2SAM'
!
!     LOOK FOR:
!
!          LOWER TAILED
!          UPPER TAILED
!          TWO SAMPLE
!          K SAMPLE
!          PERMUTATION TEST
!
!     CHECK FOR CONFLICT WITH TWO SAMPLE KS OR TWO SAMPLE
!     CHI-SQUARE TEST
!
      ISTRT=0
      IF(ICOM.EQ.'LOWE' .AND. IHARG(1).EQ.'TAIL' .AND.   &
        (IHARG(2).EQ.'TWO ' .OR. IHARG(2).EQ.'2   ') .AND.   &
         IHARG(3).EQ.'SAMP')THEN
        IF(IHARG(4).EQ.'KOLM' .AND. IHARG(5).EQ.'SMIR')GO TO 9000
        IF(IHARG(4).EQ.'CHI ' .AND. IHARG(5).EQ.'SQUA')GO TO 9000
        IF(IHARG(4).EQ.'KS  ')GO TO 9000
        IF(IHARG(4).EQ.'CHIS')GO TO 9000
        ICASE2='LOWE'
        ISTRT=4
      ELSEIF(ICOM.EQ.'UPPE' .AND. IHARG(1).EQ.'TAIL' .AND.   &
        (IHARG(2).EQ.'TWO ' .OR. IHARG(2).EQ.'2   ') .AND.   &
         IHARG(3).EQ.'SAMP')THEN
        IF(IHARG(4).EQ.'KOLM' .AND. IHARG(5).EQ.'SMIR')GO TO 9000
        IF(IHARG(2).EQ.'CHI ' .AND. IHARG(3).EQ.'SQUA')GO TO 9000
        IF(IHARG(4).EQ.'KS  ')GO TO 9000
        IF(IHARG(4).EQ.'CHIS')GO TO 9000
        ICASE2='UPPE'
        ISTRT=4
      ELSEIF(ICOM.EQ.'LOWE' .AND. IHARG(1).EQ.'TAIL' .AND.   &
         IHARG(2).EQ.'K   ' .AND. IHARG(3).EQ.'SAMP')THEN
        IF(IHARG(4).EQ.'KOLM' .AND. IHARG(5).EQ.'SMIR')GO TO 9000
        IF(IHARG(2).EQ.'CHI ' .AND. IHARG(3).EQ.'SQUA')GO TO 9000
        IF(IHARG(4).EQ.'KS  ')GO TO 9000
        IF(IHARG(4).EQ.'CHIS')GO TO 9000
        ICASE2='LOWE'
        ICASE3='KSAM'
        ISTRT=4
      ELSEIF(ICOM.EQ.'UPPE' .AND. IHARG(1).EQ.'TAIL' .AND.   &
         IHARG(2).EQ.'K   ' .AND. IHARG(3).EQ.'SAMP')THEN
        IF(IHARG(4).EQ.'KOLM' .AND. IHARG(5).EQ.'SMIR')GO TO 9000
        IF(IHARG(2).EQ.'CHI ' .AND. IHARG(3).EQ.'SQUA')GO TO 9000
        IF(IHARG(4).EQ.'KS  ')GO TO 9000
        IF(IHARG(4).EQ.'CHIS')GO TO 9000
        ICASE2='UPPE'
        ICASE3='KSAM'
        ISTRT=4
      ELSEIF(ICOM.EQ.'TWO ' .AND. IHARG(1).EQ.'SAMP')THEN
        IF(IHARG(2).EQ.'KOLM' .AND. IHARG(3).EQ.'SMIR')GO TO 9000
        IF(IHARG(2).EQ.'CHI ' .AND. IHARG(3).EQ.'SQUA')GO TO 9000
        IF(IHARG(4).EQ.'KS  ')GO TO 9000
        IF(IHARG(4).EQ.'CHIS')GO TO 9000
        ICASE2='TWOS'
        ISTRT=2
      ELSEIF(ICOM.EQ.'2   ' .AND. IHARG(1).EQ.'SAMP')THEN
        IF(IHARG(2).EQ.'KOLM' .AND. IHARG(3).EQ.'SMIR')GO TO 9000
        IF(IHARG(2).EQ.'CHI ' .AND. IHARG(3).EQ.'SQUA')GO TO 9000
        IF(IHARG(2).EQ.'KS  ')GO TO 9000
        IF(IHARG(2).EQ.'CHIS')GO TO 9000
        ICASE2='TWOS'
        ISTRT=2
      ELSEIF(ICOM.EQ.'K   ' .AND. IHARG(1).EQ.'SAMP')THEN
        IF(IHARG(2).EQ.'KOLM' .AND. IHARG(3).EQ.'SMIR')GO TO 9000
        IF(IHARG(2).EQ.'CHI ' .AND. IHARG(3).EQ.'SQUA')GO TO 9000
        IF(IHARG(2).EQ.'KS  ')GO TO 9000
        IF(IHARG(2).EQ.'CHIS')GO TO 9000
        ICASE2='TWOS'
        ICASE3='KSAM'
        ISTRT=2
      ENDIF
      ISTOP=ISTRT
!
      DO 100 I=1,NUMARG-2
!
        ICTMP1=IHARG(I)
        ICTMP2=IHARG(I+1)
        ICTMP3=IHARG(I+2)
!
        IF(ICTMP1.EQ.'=')THEN
          IFOUND='NO'
          GO TO 9000
        ELSEIF(ICTMP1.EQ.'PERM' .AND. ICTMP2.EQ.'TEST')THEN
          IFOUND='YES'
          ICASE1='TEST'
          ISTOP=I-1
          ILASTZ=I+1
        ENDIF
  100 CONTINUE
!
      IF(IFOUND.EQ.'NO')GO TO 9000
!
!     IDENTIFY THE STATISTIC
!
      IF(ISTRT.GT.ISTOP)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR IN TWO SAMPLE PERMUTATION TEST (DPPTES)--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)
  102   FORMAT('      NO STATISTIC GIVEN.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      CALL EXTSTA(ICOM,ICOM2,IHARG,IHARG2,IARGT,ARG,NUMARG,   &
                  ISTRT,ISTOP,   &
                  ISTACS,ISTANM,ISTANR,ISTADF,ISTARA,   &
                  IFOUN2,ILOCV,ISUBRO,IBUGA2,IERROR)
      IF(IFOUN2.EQ.'NO  ')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      SPECIFIED STATISTIC NOT MATCHED.')
        CALL DPWRST('XXX','BUG ')
        DO 115 JJ=ISTRT,ISTOP
          WRITE(ICOUT,117)JJ,IHARG(JJ),IHARG2(JJ)
  117     FORMAT('     ARGUMENT ',I4,': ',2A4)
          CALL DPWRST('XXX','BUG ')
  115   CONTINUE
        IERROR='YES'
        GO TO 9000
      ENDIF
      IF(ICASE3.EQ.'2SAM')THEN
        IF(ISTARA.EQ.'OFF' .AND. ISTANR.GT.1)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,122)
  122     FORMAT('      SPECIFIED STATISTIC REQUIRES EXACTLY ONE ',   &
                 'RESPONSE VARIABLE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,124)ISTANM
  124     FORMAT('      SPECIFIED STATISTIC: ',A60)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
      ELSE
        IF(ISTANR.NE.2)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,126)
  126     FORMAT('      SPECIFIED STATISTIC REQUIRES ONE RESPONSE ',   &
                 'VARIABLE AND ONE GROUP-ID VARIABLE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,124)ISTANM
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
      ENDIF
!
      ISHIFT=ILASTZ
      CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                  IBUGA2,IERROR)
!
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PTES')THEN
        WRITE(ICOUT,91)ICASE1,ICASE2,ICASE3,ISHIFT
   91   FORMAT('DPPTES: ICASE1,ICASE2,ICASE3,ISHIFT = ',3(A4,2X),I5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,92)ISTANR,ISTACS,ISTARA,ISTANM
   92   FORMAT('ISTANR,ISTACS,ISTARA,ISTANM = ',I5,2(2X,A4),2X,A60)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ****************************************
!               **  STEP 2--                          **
!               **  EXTRACT THE VARIABLE LIST         **
!               ****************************************
!
      ISTEPN='2'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PTES')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      INAME='PERMUTATION TEST'
      MINNA=2
      MAXNA=100
      MINN2=2
      IFLAGE=0
      IFLAGM=1
      MINNVA=2
      MAXNVA=MAXSPN
      IF(ICASE3.EQ.'KSAM')MAXNVA=2
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PTES')THEN
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
      IF(ICASE3.EQ.'KSAM')GO TO 6000
!
      ISTEPN='3A'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PTES')   &
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
                      Y1,Y1,Y1,N1,NLOCA2,NLOCA3,ICASE,   &
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
                      Y2,Y2,Y2,N2,NLOCA2,NLOCA3,ICASE,   &
                      IBUGA3,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
!               *****************************************
!               **  STEP 52--                          **
!               **  PERFORM 2-SAMPLE PERMUTATION TEST  **
!               *****************************************
!
          ISTEPN='52'
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'PTES')THEN
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5211)
 5211       FORMAT('***** FROM DPPTES, BEFORE CALL DPPTE2--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5212)I,J,N1,N2,MAXN
 5212       FORMAT('I,J,N1,N2,MAXN = ',5I8)
            CALL DPWRST('XXX','BUG ')
            DO 5215 II=1,MAX(N1,N2)
              WRITE(ICOUT,5216)II,Y1(II),Y2(II)
 5216         FORMAT('I,Y1(I),Y2(I) = ',I8,2G15.7)
              CALL DPWRST('XXX','BUG ')
 5215       CONTINUE
          ENDIF
!
          IVARID=IVARN1(I)
          IVARI2=IVARN2(I)
          IVARI3=IVARN1(J)
          IVARI4=IVARN2(J)
          CALL DPPTE2(Y1,N1,Y2,N2,YCOMB,YSAVE,   &
                      TAG,TAG1,TEMP1,TEMP2,TEMP3,   &
                      TEMP4,TEMP5,TEMP6,TEMP7,TEMP8,MAXOBV,   &
                      ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                      DTEMP1,DTEMP2,DTEMP3,   &
                      ICAPSW,ICAPTY,IFORSW,ISEED,   &
                      ICASE1,ICASE2,IPTESS,IPTESC,PPTEVA,   &
                      ISTACS,ISTARA,ISTANM,ISTANR,   &
                      IVARID,IVARI2,IVARI3,IVARI4,   &
                      STATVA,STATCD,PVAL2T,PVALLT,PVALUT,   &
                      PMEAN,PMED,PSD,P001,P005,P01,P025,P05,P10,P20,P50,   &
                      P80,P90,P95,P975,P99,P995,P999,   &
                      IBUGA3,ISUBRO,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
!               ***************************************
!               **  STEP 8C--                        **
!               **  UPDATE INTERNAL DATAPLOT TABLES  **
!               ***************************************
!
          ISTEPN='8C'
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FTE2')   &
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
        IF(PVAL2T.NE.CPUMIN)THEN
          IH='PVAL'
          IH2='UE2T'
          VALUE0=PVAL2T
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
        IF(P001.NE.CPUMIN)THEN
          IH='P001'
          IH2='    '
          VALUE0=P001
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(P005.NE.CPUMIN)THEN
          IH='P005'
          IH2='    '
          VALUE0=P005
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(P01.NE.CPUMIN)THEN
          IH='P01'
          IH2='    '
          VALUE0=P01
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(P025.NE.CPUMIN)THEN
          IH='P025'
          IH2='    '
          VALUE0=P025
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(P05.NE.CPUMIN)THEN
          IH='P05'
          IH2='    '
          VALUE0=P05
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(P10.NE.CPUMIN)THEN
          IH='P10'
          IH2='    '
          VALUE0=P10
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(P20.NE.CPUMIN)THEN
          IH='P20'
          IH2='    '
          VALUE0=P20
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(P50.NE.CPUMIN)THEN
          IH='P50'
          IH2='    '
          VALUE0=P50
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(P80.NE.CPUMIN)THEN
          IH='P80'
          IH2='    '
          VALUE0=P80
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(P90.NE.CPUMIN)THEN
          IH='P90'
          IH2='    '
          VALUE0=P90
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(P95.NE.CPUMIN)THEN
          IH='P95'
          IH2='    '
          VALUE0=P95
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(P975.NE.CPUMIN)THEN
          IH='P975'
          IH2='    '
          VALUE0=P975
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(P99.NE.CPUMIN)THEN
          IH='P99'
          IH2='    '
          VALUE0=P99
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(P995.NE.CPUMIN)THEN
          IH='P995'
          IH2='    '
          VALUE0=P995
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
        IF(P999.NE.CPUMIN)THEN
          IH='P999'
          IH2='    '
          VALUE0=P999
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
!
 5220   CONTINUE
 5210 CONTINUE
      GO TO 9000
!
 6000 CONTINUE
!
      ISTEPN='3B'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'PTES')   &
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMVA2=2
      ICOL=1
      CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                  INAME,IVARN1,IVARN2,IVARTY,   &
                  ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                  MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                  MAXCP4,MAXCP5,MAXCP6,   &
                  V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                  Y1,Y2,Y1,N,NLOCA2,NLOCA3,ICASE,   &
                  IBUGA3,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               *****************************************
!               **  STEP 62--                          **
!               **  PERFORM K-SAMPLE PERMUTATION TEST  **
!               *****************************************
!
      ISTEPN='62'
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'PTES')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6211)
 6211   FORMAT('***** FROM DPPTES, BEFORE CALL DPPTE3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6212)N,MAXN
 6212   FORMAT('N,MAXN = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO 6215 II=1,N
          WRITE(ICOUT,6216)II,Y1(II),Y2(II)
 6216     FORMAT('I,Y1(I),Y2(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 6215   CONTINUE
      ENDIF
!
      IVARID=IVARN1(1)
      IVARI2=IVARN2(1)
      IVARI3=IVARN1(2)
      IVARI4=IVARN2(2)
      CALL DPPTE3(Y1,Y2,N,YSAVE,   &
                  TAG1,TEMP1,TEMP2,   &
                  TEMP4,TEMP5,TEMP6,TEMP7,TEMP8,TEMP9,MAXOBV,   &
                  ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                  DTEMP1,DTEMP2,DTEMP3,   &
                  ICAPSW,ICAPTY,IFORSW,ISEED,   &
                  ICASE2,IPTESS,PPTEVA,   &
                  ISTACS,ISTARA,ISTANM,ISTANR,   &
                  IVARID,IVARI2,IVARI3,IVARI4,   &
                  STATVA,STATCD,PVAL2T,PVALLT,PVALUT,   &
                  PMEAN,PMED,PSD,P001,P005,P01,P025,P05,P10,P20,P50,   &
                  P80,P90,P95,P975,P99,P995,P999,   &
                  IBUGA3,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
!               ***************************************
!               **  STEP 8D--                        **
!               **  UPDATE INTERNAL DATAPLOT TABLES  **
!               ***************************************
!
      ISTEPN='8D'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FTE2')   &
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
      IF(PVAL2T.NE.CPUMIN)THEN
        IH='PVAL'
        IH2='UE2T'
        VALUE0=PVAL2T
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
      IF(P001.NE.CPUMIN)THEN
        IH='P001'
        IH2='    '
        VALUE0=P001
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P005.NE.CPUMIN)THEN
        IH='P005'
        IH2='    '
        VALUE0=P005
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P01.NE.CPUMIN)THEN
        IH='P01'
        IH2='    '
        VALUE0=P01
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P025.NE.CPUMIN)THEN
        IH='P025'
        IH2='    '
        VALUE0=P025
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P05.NE.CPUMIN)THEN
        IH='P05'
        IH2='    '
        VALUE0=P05
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P10.NE.CPUMIN)THEN
        IH='P10'
        IH2='    '
        VALUE0=P10
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P20.NE.CPUMIN)THEN
        IH='P20'
        IH2='    '
        VALUE0=P20
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P50.NE.CPUMIN)THEN
        IH='P50'
        IH2='    '
        VALUE0=P50
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P80.NE.CPUMIN)THEN
        IH='P80'
        IH2='    '
        VALUE0=P80
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P90.NE.CPUMIN)THEN
        IH='P90'
        IH2='    '
        VALUE0=P90
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P95.NE.CPUMIN)THEN
        IH='P95'
        IH2='    '
        VALUE0=P95
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P975.NE.CPUMIN)THEN
        IH='P975'
        IH2='    '
        VALUE0=P975
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P99.NE.CPUMIN)THEN
        IH='P99'
        IH2='    '
        VALUE0=P99
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P995.NE.CPUMIN)THEN
        IH='P995'
        IH2='    '
        VALUE0=P995
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P999.NE.CPUMIN)THEN
        IH='P999'
        IH2='    '
        VALUE0=P999
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'PTES')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPPTES--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)IFOUND,IERROR,ICASE1,ICASE2,ICASE3
 9016   FORMAT('IFOUND,IERROR,ICASE1,ICASE2,ICASE3 = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPPTES
      SUBROUTINE DPPTE2(Y1,N1,Y2,N2,YCOMB,YSAVE,   &
                        TAG,TAG1,TEMP1,TEMP2,TEMP3,   &
                        XTEMP1,XTEMP2,XTEMP3,XTEMP4,XTEMP5,MAXNXT,   &
                        ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                        DTEMP1,DTEMP2,DTEMP3,   &
                        ICAPSW,ICAPTY,IFORSW,ISEED,   &
                        ICASE1,ICASE2,IPTESS,IPTESC,PPTEVA,   &
                        ISTACS,ISTARA,ISTANM,ISTANR,   &
                        IVARID,IVARI2,IVARI3,IVARI4,   &
                        STATVA,STATCD,PVAL2T,PVALLT,PVALUT,   &
                        PMEAN,PMED,PSD,P001,P005,P01,P025,P05,P10,P20,   &
                        P50,P80,P90,P95,P975,P99,P995,P999,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS ROUTINE COMPUTES A TWO SAMPLE PERMUTATION TEST FOR A
!              USER SPECIFIED STATISTIC.
!     EXAMPLE--TWO SAMPLE MEDIAN PERMUTATION TEST Y1 Y2
!              TWO SAMPLE RATIO OF AVERAGE ABSOLUTE DEVIATIONS
!                         PERMUTATION TEST Y1 Y2
!              SAMPLE 1 IS IN INPUT VECTOR Y1 (WITH N1 OBSERVATIONS)
!              SAMPLE 2 IS IN INPUT VECTOR Y2 (WITH N2 OBSERVATIONS).
!     REFERENCE--HIGGINS (2004), "INTRODUCTION TO MODERN NONPARAMETRIC
!                STATISTICS", DUXBURY PRESS, PP. 52-53.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2023/08
!     ORIGINAL VERSION--AUGUST    2023.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 ICASE1
      CHARACTER*4 ICASE2
      CHARACTER*4 IPTESC
      CHARACTER*4 ISTACS
      CHARACTER*4 ISTARA
      CHARACTER*60 ISTANM
      CHARACTER*4 IVARID
      CHARACTER*4 IVARI2
      CHARACTER*4 IVARI3
      CHARACTER*4 IVARI4
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IOP
      CHARACTER*4 IDIR
      CHARACTER*4 IFLAGD
!
!---------------------------------------------------------------------
!
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION YCOMB(*)
      DIMENSION YSAVE(*)
      DIMENSION TAG(*)
      DIMENSION TAG1(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
      DIMENSION XTEMP3(*)
      DIMENSION XTEMP4(*)
      DIMENSION XTEMP5(*)
!
      DOUBLE PRECISION DTEMP1(*)
      DOUBLE PRECISION DTEMP2(*)
      DOUBLE PRECISION DTEMP3(*)
!
      INTEGER ITEMP1(*)
      INTEGER ITEMP2(*)
      INTEGER ITEMP3(*)
      INTEGER ITEMP4(*)
      INTEGER ITEMP5(*)
      INTEGER ITEMP6(*)
!
      PARAMETER(NUMCLI=5)
      PARAMETER(MAXLIN=3)
      PARAMETER(NUMALP=4)
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
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPPT'
      ISUBN2='E2  '
      IERROR='NO'
!
      N=-99
      STATVA=CPUMIN
      STATCD=CPUMIN
      PVAL2T=CPUMIN
      PVALLT=CPUMIN
      PVALUT=CPUMIN
      BMEAN=CPUMIN
      PSD=CPUMIN
      P001=CPUMIN
      P005=CPUMIN
      P01=CPUMIN
      P025=CPUMIN
      P05=CPUMIN
      P10=CPUMIN
      P20=CPUMIN
      P50=CPUMIN
      P80=CPUMIN
      P90=CPUMIN
      P95=CPUMIN
      P975=CPUMIN
      P99=CPUMIN
      P995=CPUMIN
      P999=CPUMIN
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'PTE2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPPTE2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N1,N2,MAXNXT
   52   FORMAT('IBUGA3,ISUBRO,MAXNXT,N1,N2 = ',2(A4,2X),3I8)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,53)ICASE1,ICASE2,IPTESC,IPTESS,PPTEVA
   53   FORMAT('ICASE1,ICASE2,IPTESC,IPTESS,PPTEVA = ',   &
               3(A4,2X),I8,G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,54)ISEED,ISTANR,ISTACS,ISTARA,ISTANM
   54   FORMAT('ISEED,ISTANR,ISTACS,ISTARA,ISTANM = ',2I8,2(2X,A4),A60)
        CALL DPWRST('XXX','WRIT')
        DO 56 I=1,N1
          WRITE(ICOUT,57)I,Y1(I)
   57     FORMAT('I,Y1(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
        DO 66 I=1,N2
          WRITE(ICOUT,67)I,Y2(I)
   67     FORMAT('I,Y2(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   66   CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PTE2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(N1.LE.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR IN TWO SAMPLE PERMUTATION TEST--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE FIRST ',   &
               'RESPONSE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      VARIABLE MUST BE 3 OR LARGER.  SUCH WAS NOT THE ',   &
               'CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N1
  117   FORMAT('      THE NUMBER OF OBSERVATIONS   = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(N2.LE.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,122)
  122   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE SECOND ',   &
               'RESPONSE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N2
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               ********************************************
!               **  STEP 2--                              **
!               **  COMPUTE STATISTIC FOR ORIGINAL DATA   **
!               **  CREATE THE COMBINED ARRAY             **
!               ********************************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PTE2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL MEAN(Y1,N1,IWRITE,YMEAN1,IBUGA3,IERROR)
      CALL MEDIAN(Y1,N1,IWRITE,TEMP1,MAXNXT,YMED1,IBUGA3,IERROR)
      CALL SD(Y1,N1,IWRITE,YSD1,IBUGA3,IERROR)
      CALL MEAN(Y2,N2,IWRITE,YMEAN2,IBUGA3,IERROR)
      CALL MEDIAN(Y2,N2,IWRITE,TEMP1,MAXNXT,YMED2,IBUGA3,IERROR)
      CALL SD(Y2,N2,IWRITE,YSD2,IBUGA3,IERROR)
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PTE2')THEN
        WRITE(ICOUT,201)YMEAN1,YMED1,YSD1,YMEAN2,YMED2,YSD2
  201   FORMAT('YMEAN1,YMED1,YSD1,YMEAN2,YMED2,YSD2 = ',6G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      N=N1+N2
!
      YCOMB(1:N1)=Y1(1:N1)
      YCOMB(N1+1:N1+N2)=Y2(1:N2)
      TAG(1:N1)=1.0
      TAG(N1+1:N)=2.0
!
      PCONST=PPTEVA
      IF(ISTARA.EQ.'ON' .OR. IPTESC.EQ.'RATI')THEN
        IF(PCONST.LE.0.0)PCONST=1.0
      ELSE
        IF(PCONST.EQ.CPUMIN)PCONST=0.0
      ENDIF
!
      IF(ISTARA.EQ.'ON')THEN
        NUMV2=2
        CALL CMPSTA(Y1,Y2,TEMP1,   &
                    XTEMP1,XTEMP2,XTEMP3,XTEMP4,XTEMP5,   &
                    MAXNXT,N1,N2,N1,NUMV2,ISTACS,ISTARA,   &
                    ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                    DTEMP1,DTEMP2,DTEMP3,   &
                    STATVA,   &
                    ISUBRO,IBUGA3,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
      ELSE
        IF(ISTANR.GT.1)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,222)
  222     FORMAT('      THE TWO SAMPLE PERMUATION TEST IS ONLY ',   &
                 'SUPPORTED FOR UNIVARIATE STATISTICS.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,224)ISTANM,ISTANR
  224     FORMAT('      THE STATISTIC: ',A60,' REQUIRES ',I2,   &
                 'RESPONSE VARIABLES.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
!
        NUMV2=1
        CALL CMPSTA(Y1,TEMP1,TEMP1,   &
                    XTEMP1,XTEMP2,XTEMP3,XTEMP4,XTEMP5,   &
                    MAXNXT,N1,N1,N1,NUMV2,ISTACS,ISTARA,   &
                    ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                    DTEMP1,DTEMP2,DTEMP3,   &
                    ANUM,   &
                    ISUBRO,IBUGA3,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
        CALL CMPSTA(Y2,TEMP1,TEMP1,   &
                    XTEMP1,XTEMP2,XTEMP3,XTEMP4,XTEMP5,   &
                    MAXNXT,N2,N2,N2,NUMV2,ISTACS,ISTARA,   &
                    ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                    DTEMP1,DTEMP2,DTEMP3,   &
                    ADEN,   &
                    ISUBRO,IBUGA3,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
        IF(IPTESC.EQ.'DIFF')THEN
          STATVA=ANUM - ADEN
          STATVA=STATVA - PCONST
        ELSEIF(IPTESC.EQ.'RATI')THEN
          IF(ANUM.GE.ADEN)THEN
            IF(ADEN.NE.0.0)THEN
              STATVA=ANUM/ADEN
              IFLAGD='DEFA'
            ELSE
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,101)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,272)
  272         FORMAT('      THE DENOMINATOR OF THE RATIO STATISTIC ',   &
                     'IS ZERO.')
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GO TO 9000
            ENDIF
          ELSE
            IF(ANUM.NE.0.0)THEN
              STATVA=ADEN/ANUM
              IFLAGD='FLIP'
            ELSE
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,101)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,272)
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GO TO 9000
            ENDIF
          ENDIF
        ENDIF
      ENDIF
!
!               **********************************************
!               **   STEP 3--                               **
!               **   GENERATE THE RANDOM PERMUTATIONS AND   **
!               **   COMPUTE THE DESIRED STATISTIC FOR EACH **
!               **   RANDOM PERMUTATION.                    **
!               **********************************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PTE2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     DETERMINE TOTAL SIZE (N*NUMBER OF PERMUTATION SAMPLES)
!
      NITER=IPTESS
!CCCC IF(NITER.LE.4000)THEN
!CCCC   IF(N1.GE.15 .AND. N2.GE.15)THEN
!CCCC     NITER=IPTESS
!CCCC   ELSEIF(N1.GE.25 .OR. N2.GE.25)THEN
!CCCC     NITER=IPTESS
!CCCC   ELSE
!CCCC     AVAL=BINOM(N1+N2,N1)
!CCCC     NITER=MIN(IPTESS,INT(AVAL))
!CCCC   ENDIF
!CCCC ELSE
!CCCC   AVAL=BINOM(N1+N2,N1)
!CCCC   NITER=MIN(IPTESS,INT(AVAL))
!CCCC ENDIF
      NITEMP=MAXNXT/N
      IF(NITEMP.GT.NITER)NITEMP=NITER
      IF(NITEMP.GT.1600)NITEMP=1600
      ICNT2=0
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PTE2')THEN
        WRITE(ICOUT,301)IPTESS,NITER,NITEMP
  301   FORMAT('IPTESS,NITER,NITEMP = ',3I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IOP='OPEN'
      IFLAG1=1
      IFLAG2=1
      IFLAG3=0
      IFLAG4=0
      IFLAG5=0
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                  IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      WRITE(IOUNI2,*) 'X             TAG'
!
  310 CONTINUE
!
!       GENERATE NITEMP RANDOM PERMUTATIONS
!
        NKEEP=N
        PVAL=1.0
        IDIST=0
        CALL RANPE2(N,NKEEP,PVAL,NITEMP,IDIST,MAXNXT,ISEED,   &
                    TEMP1,TAG1,XTEMP1,NOUT,   &
                    ISUBRO,IBUGA3,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PTE2')THEN
          WRITE(ICOUT,314)MAXNXT,NOUT
  314     FORMAT('AFTER RANPE2, MAXNXT,NOUT = ',2I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        DO 316 KK=1,NOUT
          WRITE(IOUNI2,'(2E15.7)')TEMP1(KK),TAG1(KK)
  316   CONTINUE
!
        DO 320 J=1,NITEMP
          ICNT2=ICNT2+1
          IF(ICNT2.GT.NITER)THEN
            ICNT2=ICNT2-1
            GO TO 390
          ENDIF
!
!         COMPUTE STATISTIC FOR THE RANDOM PERMUTATIONS
!
          NSTRT1=(J-1)*N + 1
          NSTOP1=NSTRT1+N1-1
          NSTRT2=NSTOP1+1
          NSTOP2=NSTRT2+N2-1
!
!         EXTRACT THE PERMUTED VARIABLES
!
          ICNT3=0
          ICNT4=0
          DO 340 KK=NSTRT1,NSTOP1
            ICNT3=ICNT3+1
            TEMP2(ICNT3)=YCOMB(INT(TEMP1(KK)+0.1))
  340     ENDDO
          DO 350 KK=NSTRT2,NSTOP2
            ICNT4=ICNT4+1
            TEMP3(ICNT4)=YCOMB(INT(TEMP1(KK)+0.1))
  350     ENDDO
          IF(ICNT3.NE.N1)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,101)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,341)
  341       FORMAT('      NUMBER OF VALUES IN PERMUTED SAMPLE 1 ',   &
                   'IS NOT EQUAL TO SAMPLE SIZE.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,343)N1
  343       FORMAT('      EXPECTED SAMPLE SIZE: ',I8)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,345)ICNT3
  345       FORMAT('      RETURNED SAMPLE SIZE: ',I8)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
          IF(ICNT4.NE.N2)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,101)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,346)
  346       FORMAT('      NUMBER OF VALUES IN PERMUTED SAMPLE 2 ',   &
                   'IS NOT EQUAL TO SAMPLE SIZE.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,343)N2
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,345)ICNT4
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
!
          IF(ISTARA.EQ.'ON')THEN
            NUMV2=2
            CALL CMPSTA(TEMP2,TEMP3,TEMP3,   &
                        XTEMP1,XTEMP2,XTEMP3,XTEMP4,XTEMP5,   &
                        MAXNXT,N1,N2,N1,NUMV2,ISTACS,ISTARA,   &
                        ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                        DTEMP1,DTEMP2,DTEMP3,   &
                        RIGHT,   &
                        ISUBRO,IBUGA3,IERROR)
            IF(IERROR.EQ.'YES')GO TO 9000
          ELSE
            NUMV2=1
            CALL CMPSTA(TEMP2,TEMP2,TEMP2,   &
                        XTEMP1,XTEMP2,XTEMP3,XTEMP4,XTEMP5,   &
                        MAXNXT,N1,N1,N1,NUMV2,ISTACS,ISTARA,   &
                        ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                        DTEMP1,DTEMP2,DTEMP3,   &
                        ANUM,   &
                        ISUBRO,IBUGA3,IERROR)
            IF(IERROR.EQ.'YES')GO TO 9000
            CALL CMPSTA(TEMP3,TEMP3,TEMP3,   &
                        XTEMP1,XTEMP2,XTEMP3,XTEMP4,XTEMP5,   &
                        MAXNXT,N2,N2,N2,NUMV2,ISTACS,ISTARA,   &
                        ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                        DTEMP1,DTEMP2,DTEMP3,   &
                        ADEN,   &
                        ISUBRO,IBUGA3,IERROR)
            IF(IERROR.EQ.'YES')GO TO 9000
            IF(IPTESC.EQ.'DIFF')THEN
              YSAVE(ICNT2)=ANUM - ADEN - PCONST
            ELSEIF(IPTESC.EQ.'RATI')THEN
              IF(IFLAGD.EQ.'DEFA')THEN
                IF(ADEN.NE.0.0)THEN
                  YSAVE(ICNT2)=ANUM/ADEN
                ELSE
                  WRITE(ICOUT,999)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,101)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,272)
                  CALL DPWRST('XXX','BUG ')
                  IERROR='YES'
                  GO TO 9000
                ENDIF
              ELSE
                IF(ANUM.NE.0.0)THEN
                  YSAVE(ICNT2)=ANUM/ADEN
                ELSE
                  WRITE(ICOUT,999)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,101)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,272)
                  CALL DPWRST('XXX','BUG ')
                  IERROR='YES'
                  GO TO 9000
                ENDIF
              ENDIF
            ENDIF
          ENDIF
  320   CONTINUE
        IF(ICNT2.GE.NITER)GO TO 390
        GO TO 310
!
  390 CONTINUE
!
!               **************************************************
!               **  STEP 4--                                    **
!               **  WRITE OUT COMPUTED STATISTICS TO DPST1F.DAT **
!               **************************************************
!
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PTE2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      WRITE(IOUNI1,412)
  412 FORMAT('COMPUTED STATISTICS FROM TWO SAMPLE PERMUTATION TEST')
      DO 410 II=1,ICNT2
        WRITE(IOUNI1,'(E15.7)')YSAVE(II)
  410 CONTINUE
!
!
!               **************************************************
!               **  STEP 5--                                    **
!               **  DETERMINE SELECTED PERCENTILES              **
!               **************************************************
!
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PTE2')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,506)ICNT2
  506   FORMAT('ICNT2 = ',I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      CALL SORT(YSAVE,ICNT2,YSAVE)
      CALL MEAN(YSAVE,ICNT2,IWRITE,PMEAN,IBUGA3,IERROR)
      CALL MEDIAN(YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,PMED,IBUGA3,IERROR)
      CALL SD(YSAVE,ICNT2,IWRITE,PSD,IBUGA3,IERROR)
!
      IDIR='LOWE'
      CALL DPGOF8(YSAVE,ICNT2,STATVA,PVALLT,IDIR,IBUGA3,ISUBRO,IERROR)
      IDIR='UPPE'
      CALL DPGOF8(YSAVE,ICNT2,STATVA,PVALUT,IDIR,IBUGA3,ISUBRO,IERROR)
      IF(PVALLT.LE.0.5)THEN
        PVAL2T=2.0*PVALLT
      ELSE
        PVAL2T=2.0*PVALUT
      ENDIF
      STATCD=PVALLT
!
      PPERC=0.1
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P001,IBUGA3,IERROR)
!
      PPERC=0.5
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P005,IBUGA3,IERROR)
!
      PPERC=1.0
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P01,IBUGA3,IERROR)
!
      PPERC=2.5
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P025,IBUGA3,IERROR)
!
      PPERC=5.0
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P05,IBUGA3,IERROR)
!
      PPERC=10.0
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P10,IBUGA3,IERROR)
!
      PPERC=20.0
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P20,IBUGA3,IERROR)
!
      PPERC=50.0
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P50,IBUGA3,IERROR)
!
      PPERC=80.0
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P80,IBUGA3,IERROR)
!
      PPERC=90.0
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P90,IBUGA3,IERROR)
!
      PPERC=95.0
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P95,IBUGA3,IERROR)
!
      PPERC=97.5
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P975,IBUGA3,IERROR)
!
      PPERC=99.0
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P99,IBUGA3,IERROR)
!
      PPERC=99.5
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P995,IBUGA3,IERROR)
!
      PPERC=99.9
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P999,IBUGA3,IERROR)
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PTE2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,501)PMEAN,PMED,PSD,ICNT2
  501   FORMAT('PMEAN,PMED,PSD,ICNT2 = ',3G15.7,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,503)P001,P005,P01,P025
  503   FORMAT('P001,P005,P01,P025 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,505)P05,P10,P20,P50
  505   FORMAT('P05,P10,P20,P50 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,507)P80,P90,P95,P975
  507   FORMAT('P80,P90,P95,P975 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,509)P99,P995,P999
  509   FORMAT('P99,P995,P999 = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *******************************************************
!               **   STEP 6--                                        **
!               **   WRITE OUT EVERYTHING  FOR THE PERMUTATION TEST  **
!               *******************************************************
!
      ISTEPN='6'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PTE2')   &
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
!     HYPOTHESIS TEST CASE
!
      ISTEPN='6A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PTE2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IPTESC.EQ.'RATI' .OR. ISTARA.EQ.'ON')THEN
        ITITLE='Two Sample Permutation Test (Ratio)'
        NCTITL=35
      ELSE
        ITITLE='Two Sample Permutation Test (Difference)'
        NCTITL=40
      ENDIF
      NLAST=1
      DO 601 JJ=60,1,-1
        IF(ISTANM(JJ:JJ).NE.' ')THEN
          NLAST=JJ
          GO TO 609
        ENDIF
  601 CONTINUE
  609 CONTINUE
      ITITLZ(1:NLAST)=ISTANM(1:NLAST)
      NCTITZ=NLAST
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
      IF(IPTESC.EQ.'DIFF')THEN
        IF(PCONST.NE.0.0)THEN
          ICNT=ICNT+1
          ITEXT(ICNT)='Value Subtracted From Difference:'
          NCTEXT(ICNT)=33
          AVALUE(ICNT)=PCONST
          IDIGIT(ICNT)=NUMDIG
        ENDIF
        ICNT=ICNT+1
        ITEXT(ICNT)='H0: Difference = 0'
        NCTEXT(ICNT)=18
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        IF(ICASE2.EQ.'LOWE')THEN
          ICNT=ICNT+1
          ITEXT(ICNT)='Ha: Difference < 0'
          NCTEXT(ICNT)=18
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
        ELSEIF(ICASE2.EQ.'UPPE')THEN
          ICNT=ICNT+1
          ITEXT(ICNT)='Ha: Difference > 0'
          NCTEXT(ICNT)=18
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
        ELSE
          ICNT=ICNT+1
          ITEXT(ICNT)='Ha: Difference not equal 0'
          NCTEXT(ICNT)=26
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
        ENDIF
      ELSE
        ICNT=ICNT+1
        ITEXT(ICNT)='H0: Ratio = 1'
        NCTEXT(ICNT)=13
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        ICNT=ICNT+1
        ITEXT(ICNT)='Ha: Ratio not equal 1'
        NCTEXT(ICNT)=26
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
      AVALUE(ICNT)=YMEAN1
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Median:'
      NCTEXT(ICNT)=14
      AVALUE(ICNT)=YMED1
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Standard Deviation:'
      NCTEXT(ICNT)=26
      AVALUE(ICNT)=YSD1
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
      AVALUE(ICNT)=YMEAN2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Median:'
      NCTEXT(ICNT)=14
      AVALUE(ICNT)=YMED2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Standard Deviation:'
      NCTEXT(ICNT)=26
      AVALUE(ICNT)=YSD2
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
      ITEXT(ICNT)='Number of Permutation Samples:'
      NCTEXT(ICNT)=30
      AVALUE(ICNT)=REAL(NITER)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Statistic Value:'
      NCTEXT(ICNT)=16
      AVALUE(ICNT)=STATVA
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Test CDF Value:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=STATCD
      IDIGIT(ICNT)=NUMDIG
      IF(ICASE2.EQ.'LOWE')THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='Test P-Value:'
        NCTEXT(ICNT)=13
        AVALUE(ICNT)=PVALLT
        IDIGIT(ICNT)=NUMDIG
      ELSEIF(ICASE2.EQ.'UPPE')THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='Test P-Value:'
        NCTEXT(ICNT)=13
        AVALUE(ICNT)=PVALUT
        IDIGIT(ICNT)=NUMDIG
      ELSE
        ICNT=ICNT+1
        ITEXT(ICNT)='Test P-Value:'
        NCTEXT(ICNT)=13
        AVALUE(ICNT)=PVAL2T
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
      IF(ICASE2.EQ.'UPPE')THEN
        ITITLE='Conclusions (Upper 1-Tailed Test)'
        NCTITL=33
        ITITL9=' '
        NCTIT9=0
      ELSEIF(ICASE2.EQ.'LOWE')THEN
        ITITLE='Conclusions (Lower 1-Tailed Test)'
        NCTITL=33
        ITITL9=' '
        NCTIT9=0
      ELSE
        ITITLE='Conclusions (Two-Tailed Test)'
        NCTITL=29
        ITITL9=' '
        NCTIT9=0
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
      IF(ICASE2.EQ.'UPPE')THEN
        ITITL2(2,3)='Critical'
        NCTIT2(2,3)=8
        ITITL2(3,3)='Region (>=)'
        NCTIT2(3,3)=11
        ICNT=3
      ELSEIF(ICASE2.EQ.'LOWE')THEN
        ITITL2(2,3)='Critical'
        NCTIT2(2,3)=8
        ITITL2(3,3)='Region (<=)'
        NCTIT2(3,3)=11
        ICNT=3
      ELSE
        ITITL2(2,3)='Critical'
        NCTIT2(2,3)=8
        ITITL2(3,3)='Region (<=)'
        NCTIT2(3,3)=11
        ITITL2(2,4)='Critical'
        NCTIT2(2,4)=8
        ITITL2(3,4)='Region (>=)'
        NCTIT2(3,4)=11
        ICNT=4
      ENDIF
!
      ICNT=ICNT+1
      ITITL2(1,ICNT)='Null'
      NCTIT2(1,ICNT)=4
      ITITL2(2,ICNT)='Hypothesis'
      NCTIT2(2,ICNT)=10
      ITITL2(3,ICNT)='Conclusion'
      NCTIT2(3,ICNT)=10
!
      NMAX=0
      NUMCOL=4
      IF(ICASE2.EQ.'TWOS')NUMCOL=5
      DO 5050 I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
        IDIGIT(I)=NUMDIG
        IF(I.EQ.1 .OR. I.EQ.ICNT)THEN
          ITYPCO(I)='ALPH'
        ENDIF
 5050 CONTINUE
!
      IWHTML(1)=125
      IWHTML(2)=175
      IWHTML(3)=175
      IWHTML(4)=175
      IWHTML(5)=175
      IINC=1800
      IINC2=1400
      IWRTF(1)=IINC
      IWRTF(2)=IWRTF(1)+IINC
      IWRTF(3)=IWRTF(2)+IINC
      IWRTF(4)=IWRTF(3)+IINC
      IWRTF(5)=IWRTF(3)+IINC
!
      IF(ICASE2.EQ.'UPPE')THEN
        DO 5060 J=1,NUMALP
!
          IF(J.EQ.1)THEN
            IVALUE(J,1)='80.0%'
            NCVALU(J,1)=5
            AMAT(J,3)=P80
          ELSEIF(J.EQ.2)THEN
            IVALUE(J,1)='90.0%'
            NCVALU(J,1)=5
            AMAT(J,3)=P90
          ELSEIF(J.EQ.3)THEN
            IVALUE(J,1)='95.0%'
            NCVALU(J,1)=5
            AMAT(J,3)=P95
          ELSEIF(J.EQ.4)THEN
            IVALUE(J,1)='99.0%'
            NCVALU(J,1)=5
            AMAT(J,3)=P99
          ENDIF
          AMAT(J,2)=STATVA
          IVALUE(J,4)(1:6)='REJECT'
          IF(STATVA.LT.AMAT(J,3))THEN
            IVALUE(J,4)(1:6)='ACCEPT'
          ENDIF
          NCVALU(J,4)=6
!
 5060   CONTINUE
      ELSEIF(ICASE2.EQ.'LOWE')THEN
        DO 5160 J=1,NUMALP
!
          IF(J.EQ.1)THEN
            IVALUE(J,1)='80.0%'
            NCVALU(J,1)=5
            AMAT(J,3)=P20
          ELSEIF(J.EQ.2)THEN
            IVALUE(J,1)='90.0%'
            NCVALU(J,1)=5
            AMAT(J,3)=P10
          ELSEIF(J.EQ.3)THEN
            IVALUE(J,1)='95.0%'
            NCVALU(J,1)=5
            AMAT(J,3)=P05
          ELSEIF(J.EQ.4)THEN
            IVALUE(J,1)='99.0%'
            NCVALU(J,1)=5
            AMAT(J,3)=P01
          ENDIF
          AMAT(J,2)=STATVA
          IVALUE(J,4)(1:6)='REJECT'
          IF(STATVA.GT.AMAT(J,3))THEN
            IVALUE(J,4)(1:6)='ACCEPT'
          ENDIF
          NCVALU(J,4)=6
!
 5160   CONTINUE
      ELSE
        DO 5260 J=1,NUMALP
!
          IF(J.EQ.1)THEN
            IVALUE(J,1)='80.0%'
            NCVALU(J,1)=5
            AMAT(J,3)=P10
            AMAT(J,4)=P90
          ELSEIF(J.EQ.2)THEN
            IVALUE(J,1)='90.0%'
            NCVALU(J,1)=5
            AMAT(J,3)=P05
            AMAT(J,4)=P95
          ELSEIF(J.EQ.3)THEN
            IVALUE(J,1)='95.0%'
            NCVALU(J,1)=5
            AMAT(J,3)=P025
            AMAT(J,4)=P975
          ELSEIF(J.EQ.4)THEN
            IVALUE(J,1)='99.0%'
            NCVALU(J,1)=5
            AMAT(J,3)=P005
            AMAT(J,4)=P995
          ENDIF
          AMAT(J,2)=STATVA
          IVALUE(J,5)(1:6)='ACCEPT'
          IF(STATVA.LT.AMAT(J,3) .OR. STATVA.GT.AMAT(J,4))THEN
            IVALUE(J,5)(1:6)='REJECT'
          ENDIF
          NCVALU(J,5)=6
!
 5260   CONTINUE
      ENDIF
!
      ICNT=NUMALP
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
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
!
      IOP='CLOS'
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                  IBUGA3,ISUBRO,IERROR)
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'PTE2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPPTE2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      RETURN
      END SUBROUTINE DPPTE2
      SUBROUTINE DPPTE3(Y,X,N,YSAVE,   &
                        TAG1,TEMP1,TEMP2,   &
                        XTEMP1,XTEMP2,XTEMP3,XTEMP4,XTEMP5,XTEMP6,   &
                        MAXNXT,   &
                        ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                        DTEMP1,DTEMP2,DTEMP3,   &
                        ICAPSW,ICAPTY,IFORSW,ISEED,   &
                        ICASE2,IPTESS,PPTEVA,   &
                        ISTACS,ISTARA,ISTANM,ISTANR,   &
                        IVARID,IVARI2,IVARI3,IVARI4,   &
                        STATVA,STATCD,PVAL2T,PVALLT,PVALUT,   &
                        PMEAN,PMED,PSD,P001,P005,P01,P025,P05,P10,P20,   &
                        P50,P80,P90,P95,P975,P99,P995,P999,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS ROUTINE COMPUTES A K-SAMPLE PERMUTATION TEST FOR A
!              USER SPECIFIED STATISTIC.
!     EXAMPLE--K SAMPLE ONE WAY ANOVA F STATISTIC PERMUTATION TEST Y X
!              Y IS AN INPUT VECTOR THAT CONTAINS THE RESPONSE VALUES
!              X IS AN INPUT VECTOR THAT CONTAINS THE GROUP-ID
!     REFERENCE--HIGGINS (2004), "INTRODUCTION TO MODERN NONPARAMETRIC
!                STATISTICS", DUXBURY PRESS, CHAPTER 3.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2023/09
!     ORIGINAL VERSION--SEPTEMBER 2023.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 ICASE2
      CHARACTER*4 ISTACS
      CHARACTER*4 ISTARA
      CHARACTER*60 ISTANM
      CHARACTER*4 IVARID
      CHARACTER*4 IVARI2
      CHARACTER*4 IVARI3
      CHARACTER*4 IVARI4
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IOP
      CHARACTER*4 IDIR
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION YSAVE(*)
      DIMENSION TAG1(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
      DIMENSION XTEMP3(*)
      DIMENSION XTEMP4(*)
      DIMENSION XTEMP5(*)
      DIMENSION XTEMP6(*)
!
      DOUBLE PRECISION DTEMP1(*)
      DOUBLE PRECISION DTEMP2(*)
      DOUBLE PRECISION DTEMP3(*)
!
      INTEGER ITEMP1(*)
      INTEGER ITEMP2(*)
      INTEGER ITEMP3(*)
      INTEGER ITEMP4(*)
      INTEGER ITEMP5(*)
      INTEGER ITEMP6(*)
!
      PARAMETER(NUMCLI=5)
      PARAMETER(MAXLIN=3)
      PARAMETER(NUMALP=4)
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
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPPT'
      ISUBN2='E2  '
      IERROR='NO'
!
      STATVA=CPUMIN
      STATCD=CPUMIN
      PVAL2T=CPUMIN
      PVALLT=CPUMIN
      PVALUT=CPUMIN
      BMEAN=CPUMIN
      PSD=CPUMIN
      P001=CPUMIN
      P005=CPUMIN
      P01=CPUMIN
      P025=CPUMIN
      P05=CPUMIN
      P10=CPUMIN
      P20=CPUMIN
      P50=CPUMIN
      P80=CPUMIN
      P90=CPUMIN
      P95=CPUMIN
      P975=CPUMIN
      P99=CPUMIN
      P995=CPUMIN
      P999=CPUMIN
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'PTE3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPPTE3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N1,N2,MAXNXT
   52   FORMAT('IBUGA3,ISUBRO,MAXNXT,N1,N2 = ',2(A4,2X),3I8)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,53)ICASE2,IPTESC,IPTESS,PPTEVA
   53   FORMAT('ICASE2,IPTESC,IPTESS,PPTEVA = ',   &
               2(A4,2X),I8,G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,54)ISEED,ISTANR,ISTACS,ISTARA,ISTANM
   54   FORMAT('ISEED,ISTANR,ISTACS,ISTARA,ISTANM = ',2I8,2(2X,A4),A60)
        CALL DPWRST('XXX','WRIT')
        DO 56 I=1,N
          WRITE(ICOUT,57)I,Y(I),X(I)
   57     FORMAT('I,Y(I),X(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PTE3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(N.LE.4)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR IN K-SAMPLE PERMUTATION TEST--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS MUST BE 4 OR LARGER.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      SUCH WAS NOT THE CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE NUMBER OF OBSERVATIONS   = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!     THIS TEST CURRENTLY ONLY WORKS FOR STATISTICS BASED ON A
!     SINGLE RESPONSE VARIABLE AND A GROUP-ID VARIABLE.  FILTER
!     OUT THE ACCEPTABLE CASES.
!
      IF(ISTACS.EQ.'1ANF')GO TO 190     ! ONE WAY ANOVA F STATISTIC
      IF(ISTACS.EQ.'SSTO')GO TO 190     ! ONE WAY ANOVA SUM OF SQUARES TOTAL
      IF(ISTACS.EQ.'SSTR')GO TO 190     ! ONE WAY ANOVA SUM OF SQUARES TREATEMENT
      IF(ISTACS.EQ.'SSER')GO TO 190     ! ONE WAY ANOVA SUM OF SQUARES ERROR
      IF(ISTACS.EQ.'MSER')GO TO 190     ! ONE WAY ANOVA MEAN SQUARE ERROR
      IF(ISTACS.EQ.'MSTR')GO TO 190     ! ONE WAY ANOVA MEAN SQUARE TREATMENT
      IF(ISTACS.EQ.'CCVA')GO TO 190     ! COMMON COEFFICIENT OF VARIATION
      IF(ISTACS.EQ.'UCCV')GO TO 190     ! COMMON BIAS CORRECTED COEFFICIENT OF VARIATION
      IF(ISTACS.EQ.'REPE')GO TO 190     ! REPEATABILITY STANDARD DEVIATION
      IF(ISTACS.EQ.'REPR')GO TO 190     ! REPRODUCIBILITY STANDARD DEVIATION
      IF(ISTACS.EQ.'ADKS')GO TO 190     ! ANDERSON-DARLING K-SAMPLE TEST
      IF(ISTACS.EQ.'CVOT')GO TO 190     ! COCHRAN VARIANCE OUTLIER TEST
      IF(ISTACS.EQ.'CVMO')GO TO 190     ! COCHRAN MINIMUM VARIANCE OUTLIER TEST
!CCCC IF(ISTACS.EQ.'KLTE')GO TO 190     ! KLOTZ TEST
      IF(ISTACS.EQ.'SRTE')GO TO 190     ! SQUARED RANKS TEST
      IF(ISTACS.EQ.'METE')GO TO 190     ! MEDIAN TEST
      IF(ISTACS.EQ.'KWTE')GO TO 190     ! KRUSKAL WALLIS TEST
!CCCC IF(ISTACS.EQ.'WSHT')GO TO 190     ! COMMON WEIBULL SHAPE TEST
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,101)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,152)ISTANM
  152 FORMAT('      THIS TEST IS NOT SUPPORTED FOR: ',A60)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
!
  190 CONTINUE
!
!               ********************************************
!               **  STEP 2--                              **
!               **  COMPUTE STATISTIC FOR ORIGINAL DATA   **
!               ********************************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PTE3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMV2=2
      CALL CMPSTA(Y,X,XTEMP6,   &
                  XTEMP1,XTEMP2,XTEMP3,XTEMP4,XTEMP5,   &
                  MAXNXT,N,N,N,NUMV2,ISTACS,ISTARA,   &
                  ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                  DTEMP1,DTEMP2,DTEMP3,   &
                  STATVA,   &
                  ISUBRO,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               **********************************************
!               **   STEP 3--                               **
!               **   GENERATE THE RANDOM PERMUTATIONS AND   **
!               **   COMPUTE THE DESIRED STATISTIC FOR EACH **
!               **   RANDOM PERMUTATION.                    **
!               **********************************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PTE3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     DETERMINE TOTAL SIZE (N*NUMBER OF PERMUTATION SAMPLES)
!
      NITER=IPTESS
      NITEMP=MAXNXT/N
      IF(NITEMP.GT.NITER)NITEMP=NITER
      IF(NITEMP.GT.1600)NITEMP=1600
      ICNT2=0
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PTE3')THEN
        WRITE(ICOUT,301)IPTESS,NITER,NITEMP
  301   FORMAT('IPTESS,NITER,NITEMP = ',3I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IOP='OPEN'
      IFLAG1=1
      IFLAG2=1
      IFLAG3=0
      IFLAG4=0
      IFLAG5=0
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                  IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      WRITE(IOUNI2,*) 'X             TAG'
!
  310 CONTINUE
!
!       GENERATE NITEMP RANDOM PERMUTATIONS
!
        NKEEP=N
        PVAL=1.0
        IDIST=0
        CALL RANPE2(N,NKEEP,PVAL,NITEMP,IDIST,MAXNXT,ISEED,   &
                    TEMP1,TAG1,XTEMP1,NOUT,   &
                    ISUBRO,IBUGA3,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PTE3')THEN
          WRITE(ICOUT,314)MAXNXT,NOUT
  314     FORMAT('AFTER RANPE2, MAXNXT,NOUT = ',2I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        DO 316 KK=1,NOUT
          WRITE(IOUNI2,'(2E15.7)')TEMP1(KK),TAG1(KK)
  316   CONTINUE
!
        DO 320 J=1,NITEMP
          ICNT2=ICNT2+1
          IF(ICNT2.GT.NITER)THEN
            ICNT2=ICNT2-1
            GO TO 390
          ENDIF
!
!         COMPUTE STATISTIC FOR THE RANDOM PERMUTATIONS
!
          NSTRT1=(J-1)*N + 1
          NSTOP1=NSTRT1+N-1
!
!         EXTRACT THE PERMUTED VARIABLES
!
          ICNT3=0
          ICNT4=0
          DO 340 KK=NSTRT1,NSTOP1
            ICNT3=ICNT3+1
            TEMP2(ICNT3)=Y(INT(TEMP1(KK)+0.1))
  340     ENDDO
          IF(ICNT3.NE.N)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,101)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,341)
  341       FORMAT('      NUMBER OF VALUES IN PERMUTED SAMPLE 1 ',   &
                   'IS NOT EQUAL TO SAMPLE SIZE.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,343)N1
  343       FORMAT('      EXPECTED SAMPLE SIZE: ',I8)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,345)ICNT3
  345       FORMAT('      RETURNED SAMPLE SIZE: ',I8)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
!
          NUMV2=2
          CALL CMPSTA(TEMP2,X,XTEMP6,   &
                      XTEMP1,XTEMP2,XTEMP3,XTEMP4,XTEMP5,   &
                      MAXNXT,N,N,N,NUMV2,ISTACS,ISTARA,   &
                      ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,   &
                      DTEMP1,DTEMP2,DTEMP3,   &
                      YSAVE(ICNT2),   &
                      ISUBRO,IBUGA3,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
  320   CONTINUE
        IF(ICNT2.GE.NITER)GO TO 390
        GO TO 310
!
  390 CONTINUE
!
!               **************************************************
!               **  STEP 4--                                    **
!               **  WRITE OUT COMPUTED STATISTICS TO DPST1F.DAT **
!               **************************************************
!
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PTE3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      WRITE(IOUNI1,412)
  412 FORMAT('COMPUTED STATISTICS FROM K-SAMPLE PERMUTATION TEST')
      DO 410 II=1,ICNT2
        WRITE(IOUNI1,'(E15.7)')YSAVE(II)
  410 CONTINUE
!
!
!               **************************************************
!               **  STEP 5--                                    **
!               **  DETERMINE SELECTED PERCENTILES              **
!               **************************************************
!
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PTE3')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,506)ICNT2
  506   FORMAT('ICNT2 = ',I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      CALL SORT(YSAVE,ICNT2,YSAVE)
      CALL MEAN(YSAVE,ICNT2,IWRITE,PMEAN,IBUGA3,IERROR)
      CALL MEDIAN(YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,PMED,IBUGA3,IERROR)
      CALL SD(YSAVE,ICNT2,IWRITE,PSD,IBUGA3,IERROR)
!
      IDIR='LOWE'
      CALL DPGOF8(YSAVE,ICNT2,STATVA,PVALLT,IDIR,IBUGA3,ISUBRO,IERROR)
      IDIR='UPPE'
      CALL DPGOF8(YSAVE,ICNT2,STATVA,PVALUT,IDIR,IBUGA3,ISUBRO,IERROR)
      IF(PVALLT.LE.0.5)THEN
        PVAL2T=2.0*PVALLT
      ELSE
        PVAL2T=2.0*PVALUT
      ENDIF
      STATCD=PVALLT
!
      PPERC=0.1
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P001,IBUGA3,IERROR)
!
      PPERC=0.5
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P005,IBUGA3,IERROR)
!
      PPERC=1.0
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P01,IBUGA3,IERROR)
!
      PPERC=2.5
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P025,IBUGA3,IERROR)
!
      PPERC=5.0
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P05,IBUGA3,IERROR)
!
      PPERC=10.0
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P10,IBUGA3,IERROR)
!
      PPERC=20.0
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P20,IBUGA3,IERROR)
!
      PPERC=50.0
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P50,IBUGA3,IERROR)
!
      PPERC=80.0
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P80,IBUGA3,IERROR)
!
      PPERC=90.0
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P90,IBUGA3,IERROR)
!
      PPERC=95.0
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P95,IBUGA3,IERROR)
!
      PPERC=97.5
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P975,IBUGA3,IERROR)
!
      PPERC=99.0
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P99,IBUGA3,IERROR)
!
      PPERC=99.5
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P995,IBUGA3,IERROR)
!
      PPERC=99.9
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P999,IBUGA3,IERROR)
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PTE3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,501)PMEAN,PMED,PSD,ICNT2
  501   FORMAT('PMEAN,PMED,PSD,ICNT2 = ',3G15.7,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,503)P001,P005,P01,P025
  503   FORMAT('P001,P005,P01,P025 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,505)P05,P10,P20,P50
  505   FORMAT('P05,P10,P20,P50 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,507)P80,P90,P95,P975
  507   FORMAT('P80,P90,P95,P975 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,509)P99,P995,P999
  509   FORMAT('P99,P995,P999 = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *******************************************************
!               **   STEP 6--                                        **
!               **   WRITE OUT EVERYTHING  FOR THE PERMUTATION TEST  **
!               *******************************************************
!
      ISTEPN='6'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PTE3')   &
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
      ISTEPN='6A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PTE3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ITITLE='K-Sample Permutation Test'
      NCTITL=25
      NLAST=1
      DO 601 JJ=60,1,-1
        IF(ISTANM(JJ:JJ).NE.' ')THEN
          NLAST=JJ
          GO TO 609
        ENDIF
  601 CONTINUE
  609 CONTINUE
      ITITLZ(1:NLAST)=ISTANM(1:NLAST)
      NCTITZ=NLAST
!
      ICNT=1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Response Variable:  '
      WRITE(ITEXT(ICNT)(21:25),'(A4)')IVARID(1:4)
      WRITE(ITEXT(ICNT)(26:29),'(A4)')IVARI2(1:4)
      NCTEXT(ICNT)=29
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Group-ID Variable:  '
      WRITE(ITEXT(ICNT)(21:25),'(A4)')IVARI3(1:4)
      WRITE(ITEXT(ICNT)(26:29),'(A4)')IVARI4(1:4)
      NCTEXT(ICNT)=28
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
!     THE SPECIFIC HYPOTHESIS CAN VARY DEPENDING ON THE STATISTIC,
!     SO DON'T PRINT A SPECIFIC HYPOTHESIS.
!
!CCCC ICNT=ICNT+1
!CCCC ITEXT(ICNT)='H0: Difference = 0'
!CCCC NCTEXT(ICNT)=18
!CCCC AVALUE(ICNT)=0.0
!CCCC IDIGIT(ICNT)=-1
!CCCC IF(ICASE2.EQ.'LOWE')THEN
!CCCC   ICNT=ICNT+1
!CCCC   ITEXT(ICNT)='Ha: Difference < 0'
!CCCC   NCTEXT(ICNT)=18
!CCCC   AVALUE(ICNT)=0.0
!CCCC   IDIGIT(ICNT)=-1
!CCCC ELSEIF(ICASE2.EQ.'UPPE')THEN
!CCCC   ICNT=ICNT+1
!CCCC   ITEXT(ICNT)='Ha: Difference > 0'
!CCCC   NCTEXT(ICNT)=18
!CCCC   AVALUE(ICNT)=0.0
!CCCC   IDIGIT(ICNT)=-1
!CCCC ELSE
!CCCC   ICNT=ICNT+1
!CCCC   ITEXT(ICNT)='Ha: Difference not equal 0'
!CCCC   NCTEXT(ICNT)=26
!CCCC   AVALUE(ICNT)=0.0
!CCCC   IDIGIT(ICNT)=-1
!CCCC ENDIF
!
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
      ITEXT(ICNT)='Number of Permutation Samples:'
      NCTEXT(ICNT)=30
      AVALUE(ICNT)=REAL(NITER)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Statistic Value:'
      NCTEXT(ICNT)=16
      AVALUE(ICNT)=STATVA
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Test CDF Value:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=STATCD
      IDIGIT(ICNT)=NUMDIG
      IF(ICASE2.EQ.'LOWE')THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='Test P-Value:'
        NCTEXT(ICNT)=13
        AVALUE(ICNT)=PVALLT
        IDIGIT(ICNT)=NUMDIG
      ELSEIF(ICASE2.EQ.'UPPE')THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='Test P-Value:'
        NCTEXT(ICNT)=13
        AVALUE(ICNT)=PVALUT
        IDIGIT(ICNT)=NUMDIG
      ELSE
        ICNT=ICNT+1
        ITEXT(ICNT)='Test P-Value:'
        NCTEXT(ICNT)=13
        AVALUE(ICNT)=PVAL2T
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
      IF(ICASE2.EQ.'UPPE')THEN
        ITITLE='Conclusions (Upper 1-Tailed Test)'
        NCTITL=33
        ITITL9=' '
        NCTIT9=0
      ELSEIF(ICASE2.EQ.'LOWE')THEN
        ITITLE='Conclusions (Upper 1-Tailed Test)'
        NCTITL=33
        ITITL9=' '
        NCTIT9=0
      ELSE
        ITITLE='Conclusions (Two-Tailed Test)'
        NCTITL=29
        ITITL9=' '
        NCTIT9=0
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
      IF(ICASE2.EQ.'UPPE')THEN
        ITITL2(2,3)='Critical'
        NCTIT2(2,3)=8
        ITITL2(3,3)='Region (>=)'
        NCTIT2(3,3)=11
        ICNT=3
      ELSEIF(ICASE2.EQ.'LOWE')THEN
        ITITL2(2,3)='Critical'
        NCTIT2(2,3)=8
        ITITL2(3,3)='Region (<=)'
        NCTIT2(3,3)=11
        ICNT=3
      ELSE
        ITITL2(2,3)='Critical'
        NCTIT2(2,3)=8
        ITITL2(3,3)='Region (<=)'
        NCTIT2(3,3)=11
        ITITL2(2,4)='Critical'
        NCTIT2(2,4)=8
        ITITL2(3,4)='Region (>=)'
        NCTIT2(3,4)=11
        ICNT=4
      ENDIF
!
      ICNT=ICNT+1
      ITITL2(1,ICNT)='Null'
      NCTIT2(1,ICNT)=4
      ITITL2(2,ICNT)='Hypothesis'
      NCTIT2(2,ICNT)=10
      ITITL2(3,ICNT)='Conclusion'
      NCTIT2(3,ICNT)=10
!
      NMAX=0
      NUMCOL=4
      IF(ICASE2.EQ.'TWOS')NUMCOL=5
      DO 5050 I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
        IDIGIT(I)=NUMDIG
        IF(I.EQ.1 .OR. I.EQ.ICNT)THEN
          ITYPCO(I)='ALPH'
        ENDIF
 5050 CONTINUE
!
      IWHTML(1)=125
      IWHTML(2)=175
      IWHTML(3)=175
      IWHTML(4)=175
      IWHTML(5)=175
      IINC=1800
      IINC2=1400
      IWRTF(1)=IINC
      IWRTF(2)=IWRTF(1)+IINC
      IWRTF(3)=IWRTF(2)+IINC
      IWRTF(4)=IWRTF(3)+IINC
      IWRTF(5)=IWRTF(3)+IINC
!
      IF(ICASE2.EQ.'UPPE')THEN
        DO 5060 J=1,NUMALP
!
          IF(J.EQ.1)THEN
            IVALUE(J,1)='80.0%'
            NCVALU(J,1)=5
            AMAT(J,3)=P80
          ELSEIF(J.EQ.2)THEN
            IVALUE(J,1)='90.0%'
            NCVALU(J,1)=5
            AMAT(J,3)=P90
          ELSEIF(J.EQ.3)THEN
            IVALUE(J,1)='95.0%'
            NCVALU(J,1)=5
            AMAT(J,3)=P95
          ELSEIF(J.EQ.4)THEN
            IVALUE(J,1)='99.0%'
            NCVALU(J,1)=5
            AMAT(J,3)=P99
          ENDIF
          AMAT(J,2)=STATVA
          IVALUE(J,4)(1:6)='REJECT'
          IF(STATVA.LT.AMAT(J,3))THEN
            IVALUE(J,4)(1:6)='ACCEPT'
          ENDIF
          NCVALU(J,4)=6
!
 5060   CONTINUE
      ELSEIF(ICASE2.EQ.'LOWE')THEN
        DO 5160 J=1,NUMALP
!
          IF(J.EQ.1)THEN
            IVALUE(J,1)='80.0%'
            NCVALU(J,1)=5
            AMAT(J,3)=P20
          ELSEIF(J.EQ.2)THEN
            IVALUE(J,1)='90.0%'
            NCVALU(J,1)=5
            AMAT(J,3)=P10
          ELSEIF(J.EQ.3)THEN
            IVALUE(J,1)='95.0%'
            NCVALU(J,1)=5
            AMAT(J,3)=P05
          ELSEIF(J.EQ.4)THEN
            IVALUE(J,1)='99.0%'
            NCVALU(J,1)=5
            AMAT(J,3)=P01
          ENDIF
          AMAT(J,2)=STATVA
          IVALUE(J,4)(1:6)='REJECT'
          IF(STATVA.GT.AMAT(J,3))THEN
            IVALUE(J,4)(1:6)='ACCEPT'
          ENDIF
          NCVALU(J,4)=6
!
 5160   CONTINUE
      ELSE
        DO 5260 J=1,NUMALP
!
          IF(J.EQ.1)THEN
            IVALUE(J,1)='80.0%'
            NCVALU(J,1)=5
            AMAT(J,3)=P10
            AMAT(J,4)=P90
          ELSEIF(J.EQ.2)THEN
            IVALUE(J,1)='90.0%'
            NCVALU(J,1)=5
            AMAT(J,3)=P05
            AMAT(J,4)=P95
          ELSEIF(J.EQ.3)THEN
            IVALUE(J,1)='95.0%'
            NCVALU(J,1)=5
            AMAT(J,3)=P025
            AMAT(J,4)=P975
          ELSEIF(J.EQ.4)THEN
            IVALUE(J,1)='99.0%'
            NCVALU(J,1)=5
            AMAT(J,3)=P005
            AMAT(J,4)=P995
          ENDIF
          AMAT(J,2)=STATVA
          IVALUE(J,5)(1:6)='ACCEPT'
          IF(STATVA.LT.AMAT(J,3) .OR. STATVA.GT.AMAT(J,4))THEN
            IVALUE(J,5)(1:6)='REJECT'
          ENDIF
          NCVALU(J,5)=6
!
 5260   CONTINUE
      ENDIF
!
      ICNT=NUMALP
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
      GO TO 9000
!
 9000 CONTINUE
!
      IOP='CLOS'
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                  IBUGA3,ISUBRO,IERROR)
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'PTE3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPPTE3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      RETURN
      END SUBROUTINE DPPTE3
      SUBROUTINE DPPYRA(IHARG,IARGT,ARG,NUMARG,   &
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
!     PURPOSE--DRAW ONE OR MORE PYRAMIDS (DEPENDING ON HOW MANY NUMBERS
!              ARE PROVIDED).  THE COORDINATES ARE IN STANDARDIZED UNITS
!              OF 0 TO 100.
!     NOTE--THE INPUT COORDINATES DEFINE THE VERTICES OF THE FRONT FACE
!           OF THE PYRAMID.
!     NOTE-THE USUAL INPUT NUMBER OF COORDINATES IS 3
!          AND THEREFORE THE USUAL INPUT NUMBER OF NUMBERS IS 2*3 = 6.
!     NOTE--IF 4 NUMBERS ARE PROVIDED, THEN THE DRAWN PYRAMID WILL GO
!           FROM THE LAST CURSOR POSITION (ASSUMED TO BE AT VERTEX 1)
!           THROUGH THE (X,Y) POINT (EITHER ABSOLUTE OR RELATIVE) AS
!           DEFINED BY THE FIRST AND SECOND NUMBERS (ASSUMED TO BE AT
!           VERTEX 2) TO THE (X,Y) POINT (EITHER ABSOLUTE OR RELATIVE)
!           AS DEFINED BY THE THIRD AND FOURTH NUMBERS (ASSUMED TO BE AT
!           VERTEX 3) AND CONTINUING BACK THE START POINT TO CLOSE THE
!           PYRAMID.
!     NOTE--IF 6 NUMBERS ARE PROVIDED, THEN THE DRAWN PYRAMID WILL GO
!           FROM THE ABSOLUTE (X,Y) POSITION AS RESULTING FORM THE FIRST
!           AND SECOND NUMBERS (ASSUMED TO BE AT VERTEX 1) THROUGH THE
!           (X,Y) POINT (EITHER ABSOLUTE OR RELATIVE) AS DEFINED BY THE
!           THIRD AND FOURTH NUMBERS (ASSUMED TO BE AT VERTEX 2) TO THE
!           (X,Y) POINT (EITHER ABSOLUTE OR RELATIVE) AS DEFINED BY THE
!           FIFTH AND SIXTH NUMBERS (ASSUMED TO BE AT VERTEX 3) AND THEN
!           CONTINUING BACK THE START POINT TO CLOSE THE PYRAMID.
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
!     VERSION NUMBER--87/5
!     ORIGINAL VERSION--APRIL     1987.
!     UPDATED         --JANUARY   1989. CALL LIST FOR OFFSET VAR (ALAN)
!     UPDATED         --MARCH     1997. SUPPORT FOR DEVICE FONT (ALAN)
!     UPDATED         --JULY      1997. SUPPORT FOR "DATA" UNITS (ALAN)
!     UPDATED         --DECEMBER  2018. CHECK FOR DISCRETE, NULL, OR
!                                       NONE DEVICE
!     UPDATED         --DECEMBER  2018. SUPPORT FOR "DEVICE ... SCALE"
!                                       COMMAND
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
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      IERRG4=IERROR
!CCCC IBUGG4=IBUGD2
!CCCC ISUBG4=ISUBRO
!
      ILOCFN=0
      NUMNUM=0
!
      X1=0.0
      Y1=0.0
      X2=0.0
      Y2=0.0
!
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'PYRA')GO TO 90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPPYRA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)NUMARG
   53 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 55 I=1,NUMARG
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
   65 FORMAT('IREPTY(1),IREPLI(1),IREPCO(1),PREPTH(1),PREPSP(1) = ',   &
      A4,2X,A4,2X,A4,2E15.7)
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
      DO 81 I=1,NUMDEV
      WRITE(ICOUT,82)IDMANU(I),IDMODE(I),IDMOD2(I),IDMOD3(I)
   82 FORMAT('IDMANU(I),IDMODE(I),IDMOD2(I),IDMOD3(I) = ',   &
      A4,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,83)IDPOWE(I),IDCONT(I),IDCOLO(I)
   83 FORMAT('IDPOWE(I),IDCONT(I),IDCOLO(I) = ',   &
      A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,84)IDNVPP(I),IDNHPP(I),IDUNIT(I)
   84 FORMAT('IDNVPP(I),IDNHPP(I),IDUNIT(I) = ',   &
      I8,I8,I8)
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
!
      IFIG='PYRA'
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
      IARGT(1).EQ.'NUMB'.AND.IARGT(2).EQ.'NUMB')   &
      GO TO 1111
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'ABSO'.AND.   &
      IARGT(2).EQ.'NUMB'.AND.IARGT(3).EQ.'NUMB')   &
      GO TO 1112
      IF(NUMARG.GE.3.AND.IHARG(1).EQ.'RELA'.AND.   &
      IARGT(2).EQ.'NUMB'.AND.IARGT(3).EQ.'NUMB')   &
      GO TO 1113
      GO TO 1130
!
 1111 CONTINUE
      ITYPEO='ABSO'
      ILOCFN=1
      GO TO 1119
!
 1112 CONTINUE
      ITYPEO='ABSO'
      ILOCFN=2
      GO TO 1119
!
 1113 CONTINUE
      ITYPEO='RELA'
      ILOCFN=2
      GO TO 1119
 1119 CONTINUE
!
      IF(ILOCFN.GT.NUMARG)GO TO 1129
      DO 1120 I=ILOCFN,NUMARG
      IF(IARGT(I).EQ.'NUMB')GO TO 1120
      GO TO 1129
 1120 CONTINUE
      IFOUND='YES'
      GO TO 1149
 1129 CONTINUE
      GO TO 1130
!
 1130 CONTINUE
      IERRG4='YES'
      WRITE(ICOUT,1131)
 1131 FORMAT('***** ERROR IN DPPYRA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1132)
 1132 FORMAT('      ILLEGAL FORM FOR DRAW ',   &
      'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1134)
 1134 FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE ',   &
      'PROPER FORM--')
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
      GO TO 9000
 1149 CONTINUE
!
!               ****************************
!               **  STEP 3--              **
!               **  DRAW OUT THE LINE(S)  **
!               ****************************
!
      NUMNUM=NUMARG-ILOCFN+1
      IF(NUMNUM.LT.NUMPT2)GO TO 1151
      GO TO 1152
!
 1151 CONTINUE
      J=ILOCFN-1
      X1=PXSTAR
      Y1=PYSTAR
      GO TO 1159
!
 1152 CONTINUE
      J=ILOCFN
      IF(J.GT.NUMARG)GO TO 1190
      X1=ARG(J)
!CCCC THE FOLLOWING LINE WAS ADDED JULY 1997
      IF(UNITSW.EQ.'DATA')CALL DPCODS('X',X1,X1,IBUGD2,ISUBRO,IERROR)
      J=J+1
      IF(J.GT.NUMARG)GO TO 1190
      Y1=ARG(J)
!CCCC THE FOLLOWING LINE WAS ADDED JULY 1997
      IF(UNITSW.EQ.'DATA')CALL DPCODS('Y',Y1,Y1,IBUGD2,ISUBRO,IERROR)
      GO TO 1159
 1159 CONTINUE
!
 1160 CONTINUE
      J=J+1
      IF(J.GT.NUMARG)GO TO 1190
      X2=ARG(J)
!CCCC THE FOLLOWING LINE WAS ADDED JULY 1997
      IF(UNITSW.EQ.'DATA')CALL DPCODS('X',X2,X2,IBUGD2,ISUBRO,IERROR)
      IF(ITYPEO.EQ.'RELA')X2=X1+X2
      J=J+1
      IF(J.GT.NUMARG)GO TO 1190
      Y2=ARG(J)
!CCCC THE FOLLOWING LINE WAS ADDED JULY 1997
      IF(UNITSW.EQ.'DATA')CALL DPCODS('Y',Y2,Y2,IBUGD2,ISUBRO,IERROR)
      IF(ITYPEO.EQ.'RELA')Y2=Y1+Y2
!
      J=J+1
      IF(J.GT.NUMARG)GO TO 1190
      X3=ARG(J)
!CCCC THE FOLLOWING LINE WAS ADDED JULY 1997
      IF(UNITSW.EQ.'DATA')CALL DPCODS('X',X3,X3,IBUGD2,ISUBRO,IERROR)
      IF(ITYPEO.EQ.'RELA')X3=X2+X3
      J=J+1
      IF(J.GT.NUMARG)GO TO 1190
      Y3=ARG(J)
!CCCC THE FOLLOWING LINE WAS ADDED JULY 1997
      IF(UNITSW.EQ.'DATA')CALL DPCODS('Y',Y3,Y3,IBUGD2,ISUBRO,IERROR)
      IF(ITYPEO.EQ.'RELA')Y3=Y2+Y3
!
      CALL DPPYR2(X1,Y1,X2,Y2,X3,Y3,IFIG,   &
                  ILINPA,ILINCO,ILINC2,MAXLNZ,PLINTH,   &
                  AREGBA,IREBLI,IREBCO,IREBC2,MAXRGZ,PREBTH,   &
                  IREFSW,IREFCO,IREFC2,   &
                  IREPTY,IREPLI,IREPCO,IREPC2,PREPTH,PREPSP,   &
                  PTEXHE,PTEXWI,PTEXVG,PTEXHG)
!
      X1=X3
      Y1=Y3
!
      GO TO 1160
 1190 CONTINUE
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
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'PYRA')GO TO 9090
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
!
      RETURN
      END SUBROUTINE DPPYRA
      SUBROUTINE DPPYR2(X1,Y1,X2,Y2,X3,Y3,IFIG,   &
                        ILINPA,ILINCO,ILINC2,MAXLN,PLINTH,   &
                        AREGBA,IREBLI,IREBCO,IREBC2,MAXRG,PREBTH,   &
                        IREFSW,IREFCO,IREFC2,   &
                        IREPTY,IREPLI,IREPCO,IREPC2,PREPTH,PREPSP,   &
                        PTEXHE,PTEXWI,PTEXVG,PTEXHG)
!
!     PURPOSE--DRAW A PYRAMID WITH FRONT FACE VERTICES AT (X1,Y1),
!              (X2,Y2), AND (X3,Y3).
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--87/5
!     ORIGINAL VERSION--APRIL     1987.
!     UPDATED         --JANUARY   1989. MODIFY CALLS TO DPDRPL (ALAN)
!     UPDATED         --JANUARY   1989. MODIFY CALL  TO DPFIRE (ALAN)
!     UPDATED         --OCTOBER   2020. SUPPORT FOR RGB COLORS
!
!-----NON-COMMON VARIABLES-------------------------------------
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
      DIMENSION PX(10)
      DIMENSION PY(10)
!CCCC DIMENSION PX3(10)
!CCCC DIMENSION PY3(10)
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
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IF(IBUGG4.EQ.'ON' .OR. ISUBG4.EQ.'PYR2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPPYR2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)X1,Y1,X2,Y2
   53   FORMAT('X1,Y1,X2,Y2 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,61)ILINPA(1),ILINCO(1),PLINTH(1)
   61   FORMAT('ILINPA(1),ILINCO(1),PLINTH(1) = ',2(A4,2X),G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,63)IREBLI(1),IREBCO(1),PREBTH(1),AREGBA(1)
   63   FORMAT('IREBLI(1),IREBCO(1),PREBTH(1),AREGBA(1) = ',   &
               2(A4,2X),G15.7)
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
        WRITE(ICOUT,79)IFIG,IBUGG4,ISUBG4,IERRG4
   79   FORMAT('IFIG,IBUGG4,ISUBG4,IERRG4 = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *********************************
!               **  STEP 1--                   **
!               **  SET THE SPECS              **
!               **  WHICH CONTROL THE          **
!               **  APPEARANCE OF THE          **
!               **  RESULTING CUBE.            **
!               *********************************
!
      DELX21=ABS(X2-X1)
      DELY32=ABS(Y3-Y2)
!
      P3DX=0.1
      P3DY=0.3
!
!               *************************
!               **  STEP 2--           **
!               **  FILL THE FIGURE    **
!               **  (IF CALLED FOR)    **
!               *************************
!
      IF(IREFSW(1).EQ.'OFF')GO TO 2190
!
      IPATT=IREPTY(1)
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
!
      IF(IREFSW(1).EQ.'ON')GO TO 2110
      IF(IREFSW(1).EQ.'ONF')GO TO 2110
      IF(IREFSW(1).EQ.'ONS')GO TO 2120
      IF(IREFSW(1).EQ.'ONFS')GO TO 2110
      IF(IREFSW(1).EQ.'ONSF')GO TO 2110
!
!               ********************************
!               **  STEP 2.1--                **
!               **  FRONT FACE ONLY           **
!               ********************************
!
 2110 CONTINUE
      PX(1)=X1
      PY(1)=Y1
!
      PX(2)=X2
      PY(2)=Y2
!
      PX(3)=X3
      PY(3)=Y3
!
      PX(4)=X1
      PY(4)=Y1
!
      NP=4
!
      IPATT2='SOLI'
      CALL DPFIRE(PX,PY,NP,   &
                  IFIG,IPATT,PTHICK,PXGAP,PYGAP,   &
                  ICOLF,ICOLFR,ICOLFG,ICOLFB,   &
                  ICOLP,ICOLPR,ICOLPG,ICOLPB,   &
                  IPATT2)
!
      IF(IREFSW(1).EQ.'ON')GO TO 2120
      IF(IREFSW(1).EQ.'ONF')GO TO 2190
      IF(IREFSW(1).EQ.'ONS')GO TO 2120
      IF(IREFSW(1).EQ.'ONFS')GO TO 2120
      IF(IREFSW(1).EQ.'ONSF')GO TO 2120
!
!               ********************************
!               **  STEP 2.2--                **
!               **  SIDE (= RIGHT) FACE ONLY  **
!               ********************************
!
 2120 CONTINUE
      PX(1)=X3
      PY(1)=Y3
!
      PX(2)=X2-P3DX*DELX21
      PY(2)=Y2+P3DY*DELY32
!
      PX(3)=X2
      PY(3)=Y2
!
      PX(4)=X3
      PY(4)=Y3
!
      NP=4
!
      IPATT2='SOLI'
      CALL DPFIRE(PX,PY,NP,   &
                  IFIG,IPATT,PTHICK,PXGAP,PYGAP,   &
                  ICOLF,ICOLFR,ICOLFG,ICOLFB,   &
                  ICOLP,ICOLPR,ICOLPG,ICOLPB,   &
                  IPATT2)
!
      GO TO 2190
!
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
!
      PX(1)=X1
      PY(1)=Y1
!
      PX(2)=X2
      PY(2)=Y2
!
      PX(3)=X3
      PY(3)=Y3
!
      PX(4)=X1
      PY(4)=Y1
!
      NP=4
!
      IFLAG='ON'
      ICOLR=ILINC2(1,1)
      ICOLG=ILINC2(1,2)
      ICOLB=ILINC2(1,3)
      CALL DPDRPL(PX,PY,NP,   &
                  IFIG,IPATT,PTHICK,   &
                  ICOL,ICOLR,ICOLG,ICOLB,   &
                  JPATT,JTHICK,PTHIC2,JCOL,IFLAG)
!
      PX(1)=X3
      PY(1)=Y3
!
      PX(2)=X2-0.1*DELX21
      PY(2)=Y2+0.3*DELY32
!
      PX(3)=X2
      PY(3)=Y2
!
      NP=3
!
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
      IF(IBUGG4.EQ.'ON' .OR. ISUBG4.EQ.'PYR2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPPYR2--')
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,NP
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
!
      RETURN
      END SUBROUTINE DPPYR2
      SUBROUTINE DPQCC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
      ICONT,IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--GENERATE ONE OF THE FOLLOWING Q (= QUESENBERRY)
!              CONTROL CHARTS--
!              1) Q MEAN
!              2) Q RANGE
!              3) Q STANDARD DEVIATION
!              4) Q CUSUM
!              5) Q P
!              6) Q PN
!              7) Q C
!              8) Q U
!     REFERENCE--QUESENBERRY, CHARLES P.  SPC Q CHARTS FOR START-UP
!                PROCESSES AND SHORT OR LONG RUNS.
!                JOURNAL OF QUALITY TECNOLOGY, JULY 1991,
!                PAGES 213-224.
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
!
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
!
      CHARACTER*4 IHEXT
      CHARACTER*4 IHEXT2
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
!
      DIMENSION Y1(MAXOBV)
      DIMENSION Y2(MAXOBV)
      DIMENSION X1(MAXOBV)
!
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
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERROR='NO'
      ISUBN1='DPQC'
      ISUBN2='C   '
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
      MAXV2=2
      MINN2=2
      ICOLH=0
!
!               **************************************
!               **  TREAT THE Q CONTROL CHART CASE  **
!               **************************************
!
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'PQCC')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPQCC--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASPL,IAND1,IAND2,ICONT
   52   FORMAT('ICASPL,IAND1,IAND2,ICONT = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGG2,IBUGG3,IBUGQ,ISUBRO
   53   FORMAT('IBUGG2,IBUGG3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ***************************
!               **  STEP 1--             **
!               **  EXTRACT THE COMMAND  **
!               ***************************
!
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PQCC')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICOM=IHARG(1)
      ICOM2=IHARG2(1)
      ISHIFT=1
      CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
      IBUGG2,IERROR)
!
!               ***************************************
!               **  STEP 1.1--                       **
!               **  SEARCH FOR Q MEAN CONTROL CHART  **
!               ***************************************
!
      ICASPL='MECC'
!
      IF(NUMARG.GE.3.AND.   &
      ICOM.EQ.'X'.AND.IHARG(1).EQ.'BAR'.AND.IHARG(2).EQ.'CONT'.AND.   &
      IHARG(3).EQ.'CHAR')GO TO 113
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'XBAR'.AND.IHARG(1).EQ.'CONT'.AND.IHARG(2).EQ.'CHAR')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'MEAN'.AND.IHARG(1).EQ.'CONT'.AND.IHARG(2).EQ.'CHAR')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'AVER'.AND.IHARG(1).EQ.'CONT'.AND.IHARG(2).EQ.'CHAR')   &
      GO TO 112
      IF(NUMARG.GE.1.AND.   &
      ICOM.EQ.'CONT'.AND.IHARG(1).EQ.'CHAR')   &
      GO TO 111
      IF(NUMARG.GE.1.AND.   &
      ICOM.EQ.'MEAN'.AND.IHARG(1).EQ.'CHAR')   &
      GO TO 111
      IF(NUMARG.GE.1.AND.   &
      ICOM.EQ.'XBAR'.AND.IHARG(1).EQ.'CHAR')   &
      GO TO 111
      IF(NUMARG.GE.1.AND.   &
      ICOM.EQ.'AVER'.AND.IHARG(1).EQ.'CHAR')   &
      GO TO 111
!
!               ************************************************
!               **  STEP 1.2--                                **
!               **  SEARCH FOR Q STANDARD DEV. CONTROL CHART  **
!               ************************************************
!
      ICASPL='SDCC'
!
      IF(NUMARG.GE.3.AND.   &
      ICOM.EQ.'STAN'.AND.IHARG(1).EQ.'DEVI'.AND.IHARG(2).EQ.'CONT'.AND.   &
      IHARG(3).EQ.'CHAR')GO TO 113
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'SD'.AND.IHARG(1).EQ.'CONT'.AND.IHARG(2).EQ.'CHAR')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'S'.AND.IHARG(1).EQ.'CONT'.AND.IHARG(2).EQ.'CHAR')   &
      GO TO 112
      IF(NUMARG.GE.1.AND.   &
      ICOM.EQ.'SD'.AND.IHARG(1).EQ.'CHAR')   &
      GO TO 111
      IF(NUMARG.GE.1.AND.   &
      ICOM.EQ.'S'.AND.IHARG(1).EQ.'CHAR')   &
      GO TO 111
!
!               ****************************************
!               **  STEP 1.3--                        **
!               **  SEARCH FOR Q RANGE CONTROL CHART  **
!               ****************************************
!
      ICASPL='RACC'
!
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'RANG'.AND.IHARG(1).EQ.'CONT'.AND.IHARG(2).EQ.'CHAR')   &
      GO TO 112
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'R'.AND.IHARG(1).EQ.'CONT'.AND.IHARG(2).EQ.'CHAR')   &
      GO TO 112
      IF(NUMARG.GE.1.AND.   &
      ICOM.EQ.'RANG'.AND.IHARG(1).EQ.'CHAR')   &
      GO TO 111
      IF(NUMARG.GE.1.AND.   &
      ICOM.EQ.'R'.AND.IHARG(1).EQ.'CHAR')   &
      GO TO 111
!
!               ****************************************
!               **  STEP 1.4--                        **
!               **  SEARCH FOR Q CUSUM CONTROL CHART  **
!               ****************************************
!
      ICASPL='CUCC'
!
      IF(NUMARG.GE.3.AND.   &
      ICOM.EQ.'CUMU'.AND.IHARG(1).EQ.'SUM'.AND.IHARG(2).EQ.'CONT'.AND.   &
      IHARG(3).EQ.'CHAR')GO TO 113
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'CUSU'.AND.IHARG(1).EQ.'CONT'.AND.IHARG(2).EQ.'CHAR')   &
      GO TO 112
!
!               ****************************************
!               **  STEP 1.5--                        **
!               **  SEARCH FOR Q P CONTROL CHART      **
!               ****************************************
!
      ICASPL='PCC'
!
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'P'.AND.IHARG(1).EQ.'CONT'.AND.IHARG(2).EQ.'CHAR')   &
      GO TO 112
      IF(NUMARG.GE.1.AND.   &
      ICOM.EQ.'P'.AND.IHARG(1).EQ.'CHAR')   &
      GO TO 111
!
!               ****************************************
!               **  STEP 1.6--                        **
!               **  SEARCH FOR Q PN CONTROL CHART     **
!               ****************************************
!
      ICASPL='PNCC'
!
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'PN'.AND.IHARG(1).EQ.'CONT'.AND.IHARG(2).EQ.'CHAR')   &
      GO TO 112
      IF(NUMARG.GE.1.AND.   &
      ICOM.EQ.'PN'.AND.IHARG(1).EQ.'CHAR')   &
      GO TO 111
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'NP'.AND.IHARG(1).EQ.'CONT'.AND.IHARG(2).EQ.'CHAR')   &
      GO TO 112
      IF(NUMARG.GE.1.AND.   &
      ICOM.EQ.'NP'.AND.IHARG(1).EQ.'CHAR')   &
      GO TO 111
!
!               ****************************************
!               **  STEP 1.7--                        **
!               **  SEARCH FOR Q C CONTROL CHART      **
!               ****************************************
!
      ICASPL='CCC'
!
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'C'.AND.IHARG(1).EQ.'CONT'.AND.IHARG(2).EQ.'CHAR')   &
      GO TO 112
      IF(NUMARG.GE.1.AND.   &
      ICOM.EQ.'C'.AND.IHARG(1).EQ.'CHAR')   &
      GO TO 111
!
!               ****************************************
!               **  STEP 1.8--                        **
!               **  SEARCH FOR Q U CONTROL CHART      **
!               ****************************************
!
      ICASPL='UCC'
!
      IF(NUMARG.GE.2.AND.   &
      ICOM.EQ.'U'.AND.IHARG(1).EQ.'CONT'.AND.IHARG(2).EQ.'CHAR')   &
      GO TO 112
      IF(NUMARG.GE.1.AND.   &
      ICOM.EQ.'U'.AND.IHARG(1).EQ.'CHAR')   &
      GO TO 111
!
      ICASPL='    '
!
      IFOUND='NO'
      GO TO 9000
!
  111 CONTINUE
      ILASTC=1
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      GO TO 180
!
  112 CONTINUE
      ILASTC=2
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      GO TO 180
!
  113 CONTINUE
      ILASTC=3
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      GO TO 180
!
  180 CONTINUE
      IFOUND='YES'
      GO TO 190
!
  190 CONTINUE
!
!               ***********************************************************
!               **  STEP 1--                                             **
!               **  CHECK FOR THE PROPER NUMBER OF INPUT ARGUMENTS.      **
!               ***********************************************************
!
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PQCC')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      MINNA=1
      MAXNA=100
      CALL CHECKA(NUMARG,MINNA,MAXNA,IANS,IWIDTH,ISUBN1,ISUBN2,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               ********************************************
!               **  STEP 2--                              **
!               **  CHECK THE VALIDITY OF ARGUMENT 1      **
!               **  (THIS WILL BE THE RESPONSE VARIABLE)  **
!               ********************************************
!
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PQCC')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IHLEFT=IHARG(1)
      IHLEF2=IHARG2(1)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IHLEFT,IHLEF2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      ICOLL=IVALUE(ILOCV)
      NLEFT=IN(ILOCV)
!
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PQCC')THEN
         WRITE(ICOUT,211)IHLEFT,ICOLL,NLEFT
  211    FORMAT('IHLEFT,ICOLL,NLEFT = ',A4,2I8)
         CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ***************************************************************
!               **  STEP 3--                                                 **
!               **  CHECK THAT THE INPUT NUMBER OF OBSERVATIONS (NLEFT)      **
!               **  FOR THE RESPONSE VARIABLE IS 2 OR LARGER.                **
!               ***************************************************************
!
      ISTEPN='3'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PQCC')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NLEFT.LT.MINN2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,311)
  311   FORMAT('***** ERROR IN QUESENBERRY CONTROL CHART (DPQCC)--')
        CALL DPWRST('XXX','BUG ')
        IF(ICASPL.EQ.'MECC')THEN
          WRITE(ICOUT,312)
  312     FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE Q MEAN ',   &
                 'CONTROL CHART')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(ICASPL.EQ.'SDCC')THEN
          WRITE(ICOUT,322)
  322     FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE Q STANDARD ',   &
                 'DEVIATION CONTROL CHART')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(ICASPL.EQ.'RACC')THEN
          WRITE(ICOUT,323)
  323     FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE Q RANGE ',   &
                 'CONTROL CHART')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(ICASPL.EQ.'CUCC')THEN
          WRITE(ICOUT,324)
  324     FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE Q CUSUM ',   &
                 'CONTROL CHART')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(ICASPL.EQ.'PCC')THEN
          WRITE(ICOUT,325)
  325     FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE Q P ',   &
                 'CONTROL CHART')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(ICASPL.EQ.'PNCC')THEN
          WRITE(ICOUT,326)
  326     FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE Q NP ',   &
                 'CONTROL CHART')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(ICASPL.EQ.'CCC')THEN
          WRITE(ICOUT,327)
  327     FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE Q C ',   &
                 'CONTROL CHART')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(ICASPL.EQ.'UCC')THEN
          WRITE(ICOUT,328)
  328     FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE Q U ',   &
                 'CONTROL CHART')
          CALL DPWRST('XXX','BUG ')
        ENDIF
        WRITE(ICOUT,335)MINN2
  335   FORMAT('      MUST BE ',I8,' OR LARGER;  SUCH WAS NOT THE ',   &
               'CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,337)
  337   FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,338)(IANS(I),I=1,MIN(80,IWIDTH))
  338     FORMAT('      ',80A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               *****************************************
!               **  STEP 4--                           **
!               **  CHECK TO SEE THE TYPE SUBCASE      **
!               **  (BASED ON THE QUALIFIER)--         **
!               **    1) UNQUALIFIED (THAT IS, FULL);  **
!               **    2) SUBSET/EXCEPT; OR             **
!               **    3) FOR.                          **
!               *****************************************
!
      ISTEPN='4'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PQCC')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ICASEQ='FULL'
      ILOCQ=NUMARG+1
      IF(NUMARG.LT.1)GO TO 480
      DO 400 J=1,NUMARG
      J1=J
      IF(IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ') GO TO 410
      IF(IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  ') GO TO 410
      IF(IHARG(J).EQ.'FOR '.AND.IHARG2(J).EQ.'    ') GO TO 420
  400 CONTINUE
      GO TO 490
  410 CONTINUE
      ICASEQ='SUBS'
      ILOCQ=J1
      GO TO 490
  420 CONTINUE
      ICASEQ='FOR'
      ILOCQ=J1
      GO TO 490
!
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
      WRITE(ICOUT,337)
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,338)(IANS(I),I=1,MIN(80,IWIDTH))
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GO TO 9000
!
  490 CONTINUE
!
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'PQCC')THEN
        WRITE(ICOUT,491)NUMARG,ILOCQ,ICASEQ
  491   FORMAT('NUMARG,ILOCQ,ICASEQ = ',2I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ************************************************************
!               **  STEP 5--                                              **
!               **  IF A SECOND ARGUMENT EXISTS, THEN THIS                **
!               **  INDICATES THAT THE VALUES IN THE                      **
!               **  FIRST VARIABLE ARE TO BE GROUPED                      **
!               **  BASED ON VALUES OF THE SECOND VARIABLE;               **
!               **  THAT IS, THE SECOND VARAIBLE DEFINES THE              **
!               **  GROUP NUMBERS WITHIN WHICH THE MEANS,                 **
!               **  STANDARD DEVIATIONS, RANGES, AND                      **
!               **  CUMULATIVE SUMS ARE TO BE COMPUTED.                   **
!               **  THE VALUES IN THE SECOND VARIABLE                     **
!               **  ARE THE X VALUES FOR EACH MEAN, STANDARD DEVIATION,   **
!               **  ETC.  IN THE RESULTING Q CONTROL CHART.               **
!               **  THE VALUES IN THE SECOND VARIABLE                     **
!               **  NEED NOT HAVE BEEN PREVIOUSLY                         **
!               **  SORTED OR HAVE COMMON VALUES ADJACENT.                **
!               **  IF WE HAVE THE 2-VARIABLE CASE,                       **
!               **  CHECK THE VALIDITY OF THE SECOND (X) VARIABLE.        **
!               ************************************************************
!
      ISTEPN='5'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PQCC')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMV2=ILOCQ-1
      IF(NUMV2.EQ.1)GO TO 599
      IF(NUMV2.EQ.2)GO TO 530
      IF(NUMV2.EQ.3)GO TO 540
      GO TO 510
!
  510 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,311)
      CALL DPWRST('XXX','BUG ')
      IF(ICASPL.EQ.'MECC')THEN
        WRITE(ICOUT,512)
  512   FORMAT('      FOR A Q MEAN CONTROL CHART,')
        CALL DPWRST('XXX','BUG ')
      ELSEIF(ICASPL.EQ.'SDCC')THEN
        WRITE(ICOUT,513)
  513   FORMAT('      FOR A Q STANDARD DEVIATION CONTROL CHART,')
        CALL DPWRST('XXX','BUG ')
      ELSEIF(ICASPL.EQ.'RACC')THEN
        WRITE(ICOUT,514)
  514   FORMAT('      FOR A Q RANGE CONTROL CHART, ')
        CALL DPWRST('XXX','BUG ')
      ELSEIF(ICASPL.EQ.'CUCC')THEN
        WRITE(ICOUT,515)
  515   FORMAT('      FOR A Q CUSUM CONTROL CHART, ')
        CALL DPWRST('XXX','BUG ')
      ELSEIF(ICASPL.EQ.'PCC')THEN
        WRITE(ICOUT,516)
  516   FORMAT('      (FOR WHICH A Q P CONTROL CHART ')
        CALL DPWRST('XXX','BUG ')
      ELSEIF(ICASPL.EQ.'PNCC')THEN
        WRITE(ICOUT,517)
  517   FORMAT('      (FOR WHICH A Q NP CONTROL CHART ')
        CALL DPWRST('XXX','BUG ')
      ELSEIF(ICASPL.EQ.'CCC')THEN
        WRITE(ICOUT,518)
  518   FORMAT('      (FOR WHICH A Q C CONTROL CHART ')
        CALL DPWRST('XXX','BUG ')
      ELSEIF(ICASPL.EQ.'UCC')THEN
        WRITE(ICOUT,519)
  519   FORMAT('      (FOR WHICH A Q U CONTROL CHART ')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      WRITE(ICOUT,523)
  523 FORMAT('      THE NUMBER OF VARIABLES MUST BE EITHER 1 OR 2;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,525)
  525 FORMAT('      SUCH WAS NOT THE CASE HERE;  THE SPECIFIED NUMBER')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,527)NUMV2
  527 FORMAT('      OF VARIABLES WAS ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,337)
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,338)(IANS(I),I=1,MIN(80,IWIDTH))
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GO TO 9000
!
  530 CONTINUE
      IHHOR=IHARG(2)
      IHHOR2=IHARG2(2)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IHHOR,IHHOR2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      ICOLH=IVALUE(ILOCV)
      NHOR=IN(ILOCV)
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PQCC')THEN
         WRITE(ICOUT,531)IHHOR,ICOLH,NHOR
  531    FORMAT('IHHOR,ICOLH,NHOR   = ',A4,I8,I8)
         CALL DPWRST('XXX','BUG ')
      ENDIF
      IF(NHOR.NE.NLEFT)GO TO 570
      GO TO 599
!
  540 CONTINUE
!     IHEXT AS IN "EXTRA"
      IHEXT=IHARG(2)
      IHEXT2=IHARG2(2)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IHEXT,IHEXT2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      ICOLE=IVALUE(ILOCV)
      NEXT=IN(ILOCV)
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PQCC')THEN
         WRITE(ICOUT,541)IHEXT,ICOLE,NEXT
  541    FORMAT('IHEXT,ICOLE,NEXT   = ',A4,I8,I8)
         CALL DPWRST('XXX','BUG ')
      ENDIF
      IF(NEXT.NE.NLEFT)GO TO 570
!
      IHHOR=IHARG(3)
      IHHOR2=IHARG2(3)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IHHOR,IHHOR2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      ICOLH=IVALUE(ILOCV)
      NHOR=IN(ILOCV)
!
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PQCC')THEN
         WRITE(ICOUT,542)IHHOR,ICOLH,NHOR
  542    FORMAT('IHHOR,ICOLH,NHOR   = ',A4,I8,I8)
         CALL DPWRST('XXX','BUG ')
      ENDIF
      IF(NHOR.NE.NLEFT)GO TO 570
      GO TO 599
!
  570 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,311)
      CALL DPWRST('XXX','BUG ')
      IF(ICASPL.EQ.'MECC')THEN
        WRITE(ICOUT,512)
        CALL DPWRST('XXX','BUG ')
      ELSEIF(ICASPL.EQ.'SDCC')THEN
        WRITE(ICOUT,513)
        CALL DPWRST('XXX','BUG ')
      ELSEIF(ICASPL.EQ.'RACC')THEN
        WRITE(ICOUT,514)
        CALL DPWRST('XXX','BUG ')
      ELSEIF(ICASPL.EQ.'CUCC')THEN
        WRITE(ICOUT,515)
        CALL DPWRST('XXX','BUG ')
      ELSEIF(ICASPL.EQ.'PCC')THEN
        WRITE(ICOUT,516)
        CALL DPWRST('XXX','BUG ')
      ELSEIF(ICASPL.EQ.'PNCC')THEN
        WRITE(ICOUT,517)
        CALL DPWRST('XXX','BUG ')
      ELSEIF(ICASPL.EQ.'CCC')THEN
        WRITE(ICOUT,518)
        CALL DPWRST('XXX','BUG ')
      ELSEIF(ICASPL.EQ.'UCC')THEN
        WRITE(ICOUT,519)
        CALL DPWRST('XXX','BUG ')
      ENDIF
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
      WRITE(ICOUT,337)
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,338)(IANS(I),I=1,MIN(80,IWIDTH))
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GO TO 9000
!
  599 CONTINUE
!
!               *************************************************
!               **  STEP 6--                                   **
!               **  BRANCH TO THE APPROPRIATE SUBCASE;         **
!               **  (BASED ON THE QUALIFIER)                   **
!               **  THEN FORM THE RESPONSE VARIABLE            **
!               **  AND THE SECOND VARIABLE (IF EXISTENT)      **
!               *************************************************
!
      ISTEPN='6'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PQCC')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICASEQ.EQ.'FULL')GO TO 610
      IF(ICASEQ.EQ.'SUBS')GO TO 620
      IF(ICASEQ.EQ.'FOR')GO TO 630
!
  610 CONTINUE
      DO 615 I=1,NLEFT
      ISUB(I)=1
  615 CONTINUE
      NQ=NLEFT
      GO TO 650
!
  620 CONTINUE
      NIOLD=NLEFT
      CALL DPSUBS(NIOLD,ILOCS,NS,IBUGQ,IERROR)
      NQ=NIOLD
      GO TO 650
!
  630 CONTINUE
      NIOLD=NLEFT
      CALL DPFOR(NIOLD,NFOR,IROW1,IROWN,   &
      NLOCAL,ILOCS,NS,IBUGQ,IERROR)
      NQ=NFOR
      GO TO 650
!
  650 CONTINUE
      J=0
      IMAX=NLEFT
      IF(NQ.LT.NLEFT)IMAX=NQ
      DO 660 I=1,IMAX
      IF(ISUB(I).EQ.0)GO TO 660
      J=J+1
!
      IJ=MAXN*(ICOLL-1)+I
      IF(ICOLL.LE.MAXCOL)Y1(J)=V(IJ)
      IF(ICOLL.EQ.MAXCP1)Y1(J)=PRED(I)
      IF(ICOLL.EQ.MAXCP2)Y1(J)=RES(I)
      IF(ICOLL.EQ.MAXCP3)Y1(J)=YPLOT(I)
      IF(ICOLL.EQ.MAXCP4)Y1(J)=XPLOT(I)
      IF(ICOLL.EQ.MAXCP5)Y1(J)=X2PLOT(I)
      IF(ICOLL.EQ.MAXCP6)Y1(J)=TAGPLO(I)
      IF(NUMV2.LE.1)GO TO 660
!
      IF(NUMV2.EQ.2)GO TO 652
      GO TO 653
!
  652 CONTINUE
      IJ=MAXN*(ICOLH-1)+I
      IF(ICOLH.LE.MAXCOL)X1(J)=V(IJ)
      IF(ICOLH.EQ.MAXCP1)X1(J)=PRED(I)
      IF(ICOLH.EQ.MAXCP2)X1(J)=RES(I)
      IF(ICOLH.EQ.MAXCP3)X1(J)=YPLOT(I)
      IF(ICOLH.EQ.MAXCP4)X1(J)=XPLOT(I)
      IF(ICOLH.EQ.MAXCP5)X1(J)=X2PLOT(I)
      IF(ICOLH.EQ.MAXCP6)X1(J)=TAGPLO(I)
      GO TO 660
!
  653 CONTINUE
      IJ=MAXN*(ICOLE-1)+I
      IF(ICOLE.LE.MAXCOL)Y2(J)=V(IJ)
      IF(ICOLE.EQ.MAXCP1)Y2(J)=PRED(I)
      IF(ICOLE.EQ.MAXCP2)Y2(J)=RES(I)
      IF(ICOLE.EQ.MAXCP3)Y2(J)=YPLOT(I)
      IF(ICOLE.EQ.MAXCP4)Y2(J)=XPLOT(I)
      IF(ICOLE.EQ.MAXCP5)Y2(J)=X2PLOT(I)
      IF(ICOLE.EQ.MAXCP6)Y2(J)=TAGPLO(I)
!
      IJ=MAXN*(ICOLH-1)+I
      IF(ICOLH.LE.MAXCOL)X1(J)=V(IJ)
      IF(ICOLH.EQ.MAXCP1)X1(J)=PRED(I)
      IF(ICOLH.EQ.MAXCP2)X1(J)=RES(I)
      IF(ICOLH.EQ.MAXCP3)X1(J)=YPLOT(I)
      IF(ICOLH.EQ.MAXCP4)X1(J)=XPLOT(I)
      IF(ICOLH.EQ.MAXCP5)X1(J)=X2PLOT(I)
      IF(ICOLH.EQ.MAXCP6)X1(J)=TAGPLO(I)
      GO TO 660
!
  660 CONTINUE
      NLOCAL=J
!
!               ****************************************************************
!               **  STEP 8--                                                  **
!               **  DETERMINE IF THE ANALYST                                  **
!               **  HAS SPECIFIED
!               **      LSL (LOWER SPEC LIMIT)
!               **      USL (UPPER SPEC LIMIT)
!               **      USLCOST (UPPER SPEC LIMIT COST)
!               **      TARGET
!               **  FOR THE Q CONTROL CHART ANALYSIS.                           **
!               ****************************************************************
!
      ISTEPN='8'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PQCC')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CCLSL=CPUMIN
      IH='LSL '
      IH2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
      IF(IERRO2.EQ.'NO')CCLSL=VALUE(ILOCP)
!
      CCUSL=CPUMIN
      IH='USL '
      IH2='    '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
      IF(IERRO2.EQ.'NO')CCUSL=VALUE(ILOCP)
!
      CCTARG=CPUMIN
      IH='TARG'
      IH2='ET  '
      IHWUSE='P'
      MESSAG='NO'
      CALL CHECKN(IH,IH2,IHWUSE,   &
      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERRO2)
      IF(IERRO2.EQ.'NO')CCTARG=VALUE(ILOCP)
!
!               *************************************************************
!               **  STEP 9--                                               **
!               **  COMPUTE THE APPROPRIATE Q CONTROL CHART STATISTIC--    **
!               **  MEAN, STANDARD DEVIATION, RANGE, CUSUM,                **
!               **  P, NP, C, U.                                           **
!               **  COMPUTE CONFIDENCE LINES.                              **
!               **  FORM THE VERTICAL AND HORIZONTAL AXIS                  **
!               **  VALUES Y(.) AND X(.) FOR THE PLOT.                     **
!               **  DEFINE THE VECTOR D(.) TO 1'S, 2'S, AND 3'S            **
!               **  FOR THE PLOTTED VALUE, THE LOWER CONFIDENCE LINE,      **
!               **  AND THE UPPER CONFIDENCE LINE.                         **
!               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).          **
!               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).          **
!               *************************************************************
!
      ISTEPN='8'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'PQCC')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPQCC2(Y1,Y2,X1,NLOCAL,NUMV2,ICASPL,ISIZE,ICONT,   &
                  XIDTEM,TEMP,CCLSL,CCUSL,CCTARG,   &
                  Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
 9013   FORMAT('PNLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',   &
               3I8,3(2X,A4))
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
      END SUBROUTINE DPQCC
      SUBROUTINE DPQCC2(Y,YN,X,N,NUMV2,ICASPL,ISIZE,ICONT,   &
                        XIDTEM,TEMP,CCLSL,CCUSL,CCTARG,   &
                        Y2,X2,D2,N2,NPLOTV,IBUGG3,ISUBRO,IERROR)
!
!     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
!              THAT WILL DEFINE A Q (= QUESENBERRY) CONTROL CHART
!              OF THE FOLLOWING TYPES--
!                 1) Q MEAN CONTROL CHART    Y X
!                 2) Q STANDARD DEVIATION CONTROL CHART    Y X
!                 3) Q RANGE CONTROL CHART    Y X
!                 4) Q CUSUM CONTROL CHART    Y X
!                 5) Q P CONTROL CHART    NUMDEF NUMTOT X
!                 6) Q PN CONTROL CHART    NUMDEF NUMTOT X
!                 7) Q U CONTROL CHART    NUMDEF SIZE X
!                 8) Q P CONTROL CHART    NUMDEF SIZE X
!     NOTE--USE P AND PN CHARTS IF KNOW HOW MANY ITEMS HAVE DEFECTS
!         --USE U AND C CHARTS IF KNOW HOW MANY DEFECTS
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     REFERENCE--QUESENBERRY, CHARLES P.  SPC Q CHARTS FOR START-UP
!                PROCESSES AND SHORT OR LONG RUNS.
!                JOURNAL OF QUALITY TECNOLOGY, JULY 1991,
!                PAGES 213-224.
!     REFERENCE--ASTM MANUAL STP-15D, PAGES 78-84, 100-105
!     REFERENCE--ISHIKAWA, GUIDE TO QUALITY CONTROL
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--93/12
!     ORIGINAL VERSION--DECEMBER  1993.
!     UPDATED         --OCTOBER   2006. CALL LIST TO TCDF
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASPL
      CHARACTER*4 ICONT
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION YN(*)
      DIMENSION X(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
!
      DIMENSION XIDTEM(*)
      DIMENSION TEMP(*)
!
!CCCC DIMENSION A3(30)
      DIMENSION C4(30)
      DIMENSION B3(30)
      DIMENSION B4(30)
      DIMENSION D22(30)
      DIMENSION D3(30)
      DIMENSION D4(30)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
!CCCC DATA(A3(I),I=    1,   25)
!CCCC1/9.999,2.659,1.954,1.628,1.427,
!CCCC1 1.287,1.182,1.099,1.032,0.975,
!CCCC1 0.927,0.886,0.850,0.817,0.789,
!CCCC1 0.763,0.739,0.718,0.698,0.680,
!CCCC1 0.663,0.647,0.633,0.619,0.606/
      DATA(C4(I),I=    1,   25)   &
      /9.9999,0.7979,0.8862,0.9213,0.9400,   &
       0.9515,0.9594,0.9650,0.9693,0.9727,   &
       0.9754,0.9776,0.9794,0.9810,0.9823,   &
       0.9835,0.9845,0.9854,0.9862,0.9869,   &
       0.9876,0.9882,0.9887,0.9892,0.9896/
      DATA(B3(I),I=    1,   25)   &
      /0.000,0.000,0.000,0.000,0.000,0.030,0.118,0.185,0.239,0.284,   &
       0.321,0.354,0.382,0.406,0.428,0.448,0.466,0.482,0.497,0.510,   &
       0.523,0.534,0.545,0.555,0.565/
      DATA(B4(I),I=    1,   25)   &
      /9.999,3.267,2.568,2.266,2.089,1.970,1.882,1.815,1.761,1.716,   &
       1.679,1.646,1.618,1.594,1.572,1.552,1.534,1.518,1.503,1.490,   &
       1.477,1.466,1.455,1.445,1.435/
      DATA(D22(I),I=    1,   25)   &
      /9.999,3.686,4.358,4.698,4.918,5.078,5.203,5.307,5.394,5.469,   &
       5.534,5.592,5.646,5.693,5.737,5.779,5.817,5.854,5.888,5.922,   &
       5.950,5.979,6.006,6.031,6.058/
      DATA(D3(I),I=    1,   25)   &
      /0.000,0.000,0.000,0.000,0.000,0.000,0.076,0.136,0.184,0.223,   &
       0.256,0.284,0.308,0.329,0.348,0.364,0.379,0.392,0.404,0.414,   &
       0.425,0.434,0.443,0.452,0.459/
      DATA(D4(I),I=    1,   25)   &
      /9.999,3.267,2.575,2.282,2.115,2.004,1.924,1.864,1.816,1.777,   &
       1.744,1.716,1.692,1.671,1.652,1.636,1.621,1.608,1.596,1.586,   &
       1.575,1.566,1.557,1.548,1.541/
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPQC'
      ISUBN2='C2  '
!
      I2=0
      ISIZE2=0
!
      AN=0.0
      XBARG=0.0
      SDG=0.0
      RANGEG=0.0
      YUPPER=0.0
      YLOWER=0.0
!
      ANUMSE=0.0
      SDI=0.0
      SIGMAE=0.0
      RANGEE=0.0
      SADJ=0.0
      RADJ=0.0
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(N.LE.2)THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
   31   FORMAT('***** ERROR IN QUESENBERRY CONTROL CHART (DPQCC2)--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,32)
   32   FORMAT('      THE NUMBER OF OBSERVATIONS IS LESS THAN THREE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,34)N
   34   FORMAT('      THE ENTERED NUMBER OF OBSERVATIONS  = ',I6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      HOLD=Y(1)
      DO 60 I=1,N
        IF(Y(I).NE.HOLD)GO TO 69
   60 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,31)
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
      GO TO 9000
   69 CONTINUE
!
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'QCC2')THEN
        WRITE(ICOUT,70)
   70   FORMAT('AT THE BEGINNING OF DPQCC2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)ICASPL,ICONT,N,NUMV2,ISIZE
   71   FORMAT('ICASPL,ICONT,N,NUMV2,ISIZE,ICONT = ',2(A4,2X),3I8)
        CALL DPWRST('XXX','BUG ')
        DO 72 I=1,N
          WRITE(ICOUT,73)I,Y(I),X(I)
   73     FORMAT('I, Y(I), X(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
   72   CONTINUE
        IF(NUMV2.GT.2)THEN
          DO 75 I=1,N
            WRITE(ICOUT,76)I,YN(I),X(I)
   76       FORMAT('I,YN(I),X(I) = ',I8,2E15.7)
            CALL DPWRST('XXX','BUG ')
   75      CONTINUE
        ENDIF
      ENDIF
!
!               ********************************************************
!               **  STEP 1--                                          **
!               **  DETERMINE THE NUMBER OF DISTINCT VALUES           **
!               **  FOR VARIABLE 2 (THE GROUP VARIABLE).              **
!               **  IF ALL VALUES ARE DISTINCT, THEN THIS             **
!               **  IMPLIES WE HAVE THE NO REPLICATION CASE           **
!               **  WHICH IS AN ERROR CONDITION FOR A Q CONTROL CHART.  **
!               ********************************************************
!
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'QCC2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMSET=(-999)
      IF(NUMV2.EQ.1)GO TO 199
      IF(NUMV2.EQ.2)GO TO 150
!
  150 CONTINUE
      NUMSET=0
      DO 160 I=1,N
        IF(NUMSET.EQ.0)GO TO 165
        DO 170 J=1,NUMSET
          IF(X(I).EQ.XIDTEM(J))GO TO 160
  170   CONTINUE
  165   CONTINUE
        NUMSET=NUMSET+1
        XIDTEM(NUMSET)=X(I)
  160 CONTINUE
      CALL SORT(XIDTEM,NUMSET,XIDTEM)
!
      IF(NUMSET.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,192)
  192   FORMAT('      NUMBER OF SETS    NUMSET = 0 ')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IF(ICASPL.EQ.'PCC')GO TO 199
      IF(ICASPL.EQ.'PNCC')GO TO 199
      IF(ICASPL.EQ.'UCC')GO TO 199
      IF(ICASPL.EQ.'CCC')GO TO 199
!
      IF(NUMSET.EQ.N)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,196)
  196   FORMAT('      NUMBER OF SETS    NUMSET   IDENTICAL TO ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,197)
  197   FORMAT('      NUMBER OF OBSERVATIONS   N   .')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,198)NUMSET
  198   FORMAT('      NUMSET = N = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
  199 CONTINUE
!
      AN=N
      ANUMSE=NUMSET
!
!               *******************************************
!               **  STEP 3.0--                           **
!               **  DETERMINE STATISTICS FOR THE ENTIRE  **
!               **  DATA SET                             **
!               *******************************************
!
      ISTEPN='3.0'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'QCC2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMV2.EQ.1)GO TO 1090
!
      SUMXBG=0.0
      SUMSDG=0.0
      SUMRAG=0.0
      SUMSIE=0.0
      SUMRIE=0.0
      J=0
      DO 1010 ISET=1,NUMSET
        J=J+1
!
        K=0
        DO 1020 I=1,N
          IF(X(I).EQ.XIDTEM(ISET))K=K+1
          IF(X(I).EQ.XIDTEM(ISET))TEMP(K)=Y(I)
 1020   CONTINUE
        NI=K
        ANI=NI
!
        SUM=0.0
        IF(NI.LE.0)GO TO 1040
        DO 1030 I=1,NI
          SUM=SUM+TEMP(I)
 1030   CONTINUE
        XBARI=SUM/ANI
!
        SUM=0.0
        DO 1032 I=1,NI
          SUM=SUM+(TEMP(I)-XBARI)**2
 1032   CONTINUE
        DENOM=ANI-1.0
        VARI=0.0
        IF(NI.GE.2)VARI=SUM/DENOM
        SDI=0.0
        IF(VARI.GT.0.0)SDI=SQRT(VARI)
!
        XTMIN=TEMP(1)
        XTMAX=TEMP(1)
        DO 1034 I=1,NI
          IF(TEMP(I).LT.XTMIN)XTMIN=TEMP(I)
          IF(TEMP(I).GT.XTMAX)XTMAX=TEMP(I)
 1034   CONTINUE
        RANGEI=XTMAX-XTMIN
        GO TO 1049
!
 1040   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1042)
 1042   FORMAT('NI FOR SOME CLASS = 0')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1043)ISET,XIDTEM(ISET),NI
 1043   FORMAT('ISET,XIDTEM(ISET),NI = ',I8,E15.7,I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
!
 1049   CONTINUE
!
        SUMXBG=SUMXBG+ANI*XBARI
        SUMSDG=SUMSDG+ANI*SDI
        SUMRAG=SUMRAG+ANI*RANGEI
        C4LARG=1.0
        IF(NI.LE.25)SUMSIE=SUMSIE+SDI/C4(NI)
        IF(NI.GE.26)SUMSIE=SUMSIE+SDI/C4LARG
        D22LAR=2.0*SQRT(2.0*LOG(2.0*ANI))
        IF(NI.LE.25)SUMRIE=SUMRIE+RANGEI/D22(NI)
        IF(NI.GE.26)SUMRIE=SUMRIE+RANGEI/D22LAR
!
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'QCC2')THEN
          WRITE(ICOUT,1061)ISET,NI,ANI
 1061     FORMAT('ISET,NI,ANI = ',2I8,E15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1062)XBARI
 1062     FORMAT('XBARI = ',E15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1063)SDI,C4(NI),C4LARG,SUMSIE
 1063     FORMAT('SDI,C4(NI),C4LARG,SUMSIE = ',4E15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1064)RANGEI,D22(NI),D22LAR,SUMRIE
 1064     FORMAT('RANGEI,D22(NI),D22LAR,SUMRIE = ',4E15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
 1010 CONTINUE
!
      XBARG=SUMXBG/AN
      SDG=SUMSDG/AN
      RANGEG=SUMRAG/AN
      SIGMAE=SUMSIE/ANUMSE
      RANGEE=SUMRIE/ANUMSE
!
 1090 CONTINUE
!
!               **************************************************************
!               **  STEP 4--                                                **
!               **  IN ORDER TO DETERMINE THE PROPER PLOT COOORDINATES      **
!               **  FOR THE DESIRED PLOT,                                   **
!               **  BRANCH TO THE PROPER SUBCASE--                          **
!               **         1) Q MEAN CONTROL CHART;                           **
!               **         2) Q STANDARD DEVIATION CONTROL CHART;             **
!               **         3) Q RANGE CONTROL CHART;                          **
!               **         4) Q CUSUM CONTROL CHART;                          **
!               **         5) Q P CONTROL CHART;                              **
!               **         6) Q PN CONTROL CHART;                             **
!               **         7) Q C CONTROL CHART;                              **
!               **         8) Q U CONTROL CHART;                              **
!               **************************************************************
!
      ISTEPN='4'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'QCC2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICASPL.EQ.'MECC')GO TO 1100
      IF(ICASPL.EQ.'SDCC')GO TO 1200
      IF(ICASPL.EQ.'RACC')GO TO 1300
      IF(ICASPL.EQ.'CUCC')GO TO 1400
      IF(ICASPL.EQ.'PCC')GO TO 1500
      IF(ICASPL.EQ.'PNCC')GO TO 1600
      IF(ICASPL.EQ.'UCC')GO TO 1700
      IF(ICASPL.EQ.'CCC')GO TO 1800
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,31)
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
      GO TO 9000
!
!               *******************************************
!               **  STEP 5.1--                           **
!               **  TREAT THE Q MEAN CONTROL CHART CASE  **
!               *******************************************
!
 1100 CONTINUE
!
      ISTEPN='5.1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'QCC2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      J=0
      DO 1110 K=3,N
         KM1=K-1
         AKM1=KM1
         KM2=K-2
!
         SUM=0.0
         DO 1120 I=1,KM1
            SUM=SUM+Y(I)
 1120    CONTINUE
         XBAKM1=SUM/AKM1
!
         SUM=0.0
         DO 1130 I=1,KM1
            SUM=SUM+(Y(I)-XBAKM1)**2
 1130    CONTINUE
         SKM1=SQRT(SUM/(AKM1-1.0))
!
         ANUM=Y(K)-XBAKM1
         ADENOM=SKM1*SQRT((1.0/AKM1)+1.0)
         RATIO=ANUM/ADENOM
!CCCC    CALL TCDF(RATIO,KM2,CDF)
         CALL TCDF(RATIO,REAL(KM2),CDF)
         CALL NORPPF(CDF,PPF)
         J=J+1
         Y2(J)=PPF
         X2(J)=J
         D2(J)=1.0
 1110 CONTINUE
      N2=J
      NPLOTV=2
      GO TO 9000
!
!               **********************************************************
!               **  STEP 5.2--                                          **
!               **  TREAT THE Q STANDARD DEVIATION CONTROL CHART CASE  **
!               **********************************************************
!
 1200 CONTINUE
!
      ISTEPN='5.2'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'QCC2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      J=0
      DO 1210 ISET=1,NUMSET
!
      K=0
      DO 1220 I=1,N
      IF(X(I).EQ.XIDTEM(ISET))K=K+1
      IF(X(I).EQ.XIDTEM(ISET))TEMP(K)=Y(I)
 1220 CONTINUE
      NI=K
      ANI=NI
!
      IF(NI.GE.1)GO TO 1239
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
      GO TO 9000
 1239 CONTINUE
!
      SUM=0.0
      DO 1240 I=1,NI
      SUM=SUM+TEMP(I)
 1240 CONTINUE
      XBARI=SUM/ANI
!
      IF(NI.LE.1)GO TO 1210
!
      SUM=0.0
      DO 1250 I=1,NI
      SUM=SUM+(TEMP(I)-XBARI)**2
 1250 CONTINUE
      DENOM=ANI-1.0
      VARI=0.0
      IF(NI.GE.2)VARI=SUM/DENOM
      SDI=0.0
      IF(VARI.GT.0.0)SDI=SQRT(VARI)
!
      C4LARG=1.0
      IF(NI.LE.25)SADJ=C4(NI)*SIGMAE
      IF(NI.GE.26)SADJ=C4LARG*SIGMAE
!
      YMID=SADJ
!
      B4LARG=1.0+3.0/SQRT(2.0*(ANI-1.0))
      IF(NI.LE.25)YUPPER=B4(NI)*SADJ
      IF(NI.GE.26)YUPPER=B4LARG*SADJ
!
      B3LARG=1.0-3.0/SQRT(2.0*(ANI-1.0))
      IF(NI.LE.25)YLOWER=B3(NI)*SADJ
      IF(NI.GE.26)YLOWER=B3LARG*SADJ
!
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'QCC2')THEN
        WRITE(ICOUT,1261)ISET,NI,ANI
 1261   FORMAT('ISET,NI,ANI = ',2I8,E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1262)XBARI
 1262   FORMAT('XBARI = ',E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1263)SDI,C4(NI),C4LARG,SIGMAE,SADJ
 1263   FORMAT('SDI,C4(NI),C4LARG,SIGMAE,SADJ = ',5E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1264)SADJ,YMID
 1264   FORMAT('SADJ,YMID = ',2E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1265)NI,ANI,B4(NI),B4LARG,YUPPER
 1265   FORMAT('NI,ANI,B4(NI),B4LARG,YUPPER = ',I8,4E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1266)NI,ANI,B3(NI),B3LARG,YLOWER
 1266   FORMAT('NI,ANI,B3(NI),B3LARG,YLOWER = ',I8,4E15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      J=J+1
      Y2(J)=SDI
      X2(J)=XIDTEM(ISET)
      D2(J)=1.0
!
      J=J+1
      Y2(J)=YMID
      X2(J)=XIDTEM(ISET)
      D2(J)=2.0
!
      J=J+1
      Y2(J)=YUPPER
      X2(J)=XIDTEM(ISET)
      D2(J)=3.0
!
      J=J+1
      Y2(J)=YLOWER
      X2(J)=XIDTEM(ISET)
      D2(J)=4.0
!
      IF(CCTARG.EQ.CPUMIN)GO TO 1271
      J=J+1
      Y2(J)=CCTARG
      X2(J)=XIDTEM(ISET)
      D2(J)=5.0
 1271 CONTINUE
!
      IF(CCUSL.EQ.CPUMIN)GO TO 1272
      J=J+1
      Y2(J)=CCUSL
      X2(J)=XIDTEM(ISET)
      D2(J)=6.0
 1272 CONTINUE
!
      IF(CCLSL.EQ.CPUMIN)GO TO 1273
      J=J+1
      Y2(J)=CCLSL
      X2(J)=XIDTEM(ISET)
      D2(J)=7.0
 1273 CONTINUE
!
 1210 CONTINUE
      N2=J
      NPLOTV=3
      GO TO 9000
!
!               ********************************************
!               **  STEP 5.3--                            **
!               **  TREAT THE Q RANGE CONTROL CHART CASE  **
!               ********************************************
!
 1300 CONTINUE
!
      ISTEPN='5.3'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'QCC2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      D4FACT=1.25
      D3FACT=1.0/1.25
!
      J=0
      DO 1310 ISET=1,NUMSET
!
      K=0
      DO 1320 I=1,N
      IF(X(I).EQ.XIDTEM(ISET))K=K+1
      IF(X(I).EQ.XIDTEM(ISET))TEMP(K)=Y(I)
 1320 CONTINUE
      NI=K
      ANI=NI
!
      IF(NI.GE.1)GO TO 1339
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,31)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1332)
 1332 FORMAT('NI FOR SOME CLASS = 0')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1333)ISET,XIDTEM(ISET),NI
 1333 FORMAT('ISET,XIDTEM(ISET),NI = ',I8,E15.7,I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
 1339 CONTINUE
!
      IF(NI.LE.1)GO TO 1310
!
      XTMIN=TEMP(1)
      XTMAX=TEMP(1)
      DO 1340 I=1,NI
      IF(TEMP(I).LT.XTMIN)XTMIN=TEMP(I)
      IF(TEMP(I).GT.XTMAX)XTMAX=TEMP(I)
 1340 CONTINUE
      RANGEI=XTMAX-XTMIN
!
      D22LAR=2.0*SQRT(2.0*LOG(2.0*ANI))
      IF(NI.LE.25)RADJ=D22(NI)*RANGEE
      IF(NI.GE.26)RADJ=D22LAR*RANGEE
!
      YMID=RADJ
!
      D4LARG=1.0+3.0*D4FACT/SQRT(2.0*(ANI-1.0))
      IF(NI.LE.25)YUPPER=D4(NI)*RADJ
      IF(NI.GE.26)YUPPER=D4LARG*RADJ
!
      D3LARG=1.0-3.0*D3FACT/SQRT(2.0*(ANI-1.0))
      IF(NI.LE.25)YLOWER=D3(NI)*RADJ
      IF(NI.GE.26)YLOWER=D3LARG*RADJ
!
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'QCC2')THEN
        WRITE(ICOUT,1361)ISET,NI,ANI
 1361   FORMAT('ISET,NI,ANI = ',I8,I8,E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1362)RANGEI
 1362   FORMAT('RANGEI = ',E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1363)RANGEI,D22(NI),D22LAR,RANGEE,SADJ
 1363   FORMAT('RANGEI,D22(NI),D22LAR,RANGEE,SADJ = ',5E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1364)RADJ,YMID
 1364   FORMAT('RADJ,YMID = ',2E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1365)NI,ANI,D4(NI),D4LARG,YUPPER
 1365   FORMAT('NI,ANI,D4(NI),D4LARG,YUPPER = ',I8,4E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1366)NI,ANI,D3(NI),D3LARG,YLOWER
 1366   FORMAT('NI,ANI,D3(NI),D3LARG,YLOWER = ',I8,4E15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      J=J+1
      Y2(J)=RANGEI
      X2(J)=XIDTEM(ISET)
      D2(J)=1.0
!
      J=J+1
      Y2(J)=YMID
      X2(J)=XIDTEM(ISET)
      D2(J)=2.0
!
      J=J+1
      Y2(J)=YUPPER
      X2(J)=XIDTEM(ISET)
      D2(J)=3.0
!
      J=J+1
      Y2(J)=YLOWER
      X2(J)=XIDTEM(ISET)
      D2(J)=4.0
!
      IF(CCTARG.EQ.CPUMIN)GO TO 1371
      J=J+1
      Y2(J)=CCTARG
      X2(J)=XIDTEM(ISET)
      D2(J)=5.0
 1371 CONTINUE
!
      IF(CCUSL.EQ.CPUMIN)GO TO 1372
      J=J+1
      Y2(J)=CCUSL
      X2(J)=XIDTEM(ISET)
      D2(J)=6.0
 1372 CONTINUE
!
      IF(CCLSL.EQ.CPUMIN)GO TO 1373
      J=J+1
      Y2(J)=CCLSL
      X2(J)=XIDTEM(ISET)
      D2(J)=7.0
 1373 CONTINUE
!
 1310 CONTINUE
      N2=J
      NPLOTV=3
      GO TO 9000
!
!               ******************************************************
!               **  STEP 5.4--                                      **
!               **  DETERMINE PLOT COORDINATES                      **
!               **  FOR THE Q CUSUM CONTROL CHART PLOT SUBCASE.     **
!               ******************************************************
!
 1400 CONTINUE
!
      ISTEPN='3.4'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'QCC2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      WRITE(ICOUT,1405)
 1405 FORMAT('CUSUM CAPABILITY NOT YET AVAILABLE.')
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
!
!               ********************************************************
!               **  STEP 5.5--                                        **
!               **  TREAT THE Q P CONTROL CHART CASE                   **
!               **  PROPORTION DEFECTIVE PER BATCH (SUBSAMPLE)        **
!               **  NUMBER DEFECTIVE PER BATCH / TOTAL NUMBER IN BATCH
!               **  THE INPUT IS A DUAL SERIES--
!               **     1) NUMBER OF DEFECTIVE ITEMS IN THE SUBSAMPLE
!               **     2) TOTAL NUMBER OF ITEMS IN THE SAMPLE
!               **  THE CONFIDENCE BAND IS GOTTEN BY ASSUMING BINOMIAL**
!               ********************************************************
!
 1500 CONTINUE
!
      ISTEPN='5.5'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'QCC2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      SUM1=0.0
      SUM2=0.0
      DO 1510 ISET=1,NUMSET
      SUM1=SUM1+Y(ISET)
      SUM2=SUM2+YN(ISET)
 1510 CONTINUE
      CTOTAL=SUM1
      ANTOT=SUM2
      PBARG=CTOTAL/ANTOT
      PRBARG=100.0*PBARG
!
      J=0
      DO 1550 ISET=1,NUMSET
!
      CI=Y(ISET)
      ANI=YN(ISET)
      NI=INT(ANI+0.5)
      IF(NI.LE.0)GO TO 1550
!
      PI=CI/ANI
      PROPI=100.0*PI
      TAGI=XIDTEM(ISET)
!
      J=J+1
      Y2(J)=PROPI
      X2(J)=TAGI
      D2(J)=1.0
!
      J=J+1
      YMID=PRBARG
      Y2(J)=YMID
      X2(J)=TAGI
      D2(J)=2.0
!
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
!
      J=J+1
      YLOWER=YMID-3.0*SDPRI
      IF(YLOWER.LT.0.0)YLOWER=0.0
      Y2(J)=YLOWER
      X2(J)=TAGI
      D2(J)=4.0
!
      IF(CCTARG.EQ.CPUMIN)GO TO 1571
      J=J+1
      Y2(J)=CCTARG
      X2(J)=XIDTEM(ISET)
      D2(J)=5.0
 1571 CONTINUE
!
      IF(CCUSL.EQ.CPUMIN)GO TO 1572
      J=J+1
      Y2(J)=CCUSL
      X2(J)=XIDTEM(ISET)
      D2(J)=6.0
 1572 CONTINUE
!
      IF(CCLSL.EQ.CPUMIN)GO TO 1573
      J=J+1
      Y2(J)=CCLSL
      X2(J)=XIDTEM(ISET)
      D2(J)=7.0
 1573 CONTINUE
!
 1550 CONTINUE
      N2=J
      NPLOTV=3
      GO TO 9000
!
!               ********************************************************
!               **  STEP 5.6--                                        **
!               **  TREAT THE Q PN CONTROL CHART CASE                 **
!               **  TOTAL NUMBER DEFECTIVE IN A BATCH (SUBSAMPLE)     **
!               **  SUM UP THE NUMBER OF DEFECTIVES PER BATCH (SUBSAMPLE)
!               **  THE NUMBER WILL BE  A NON-NEGATIVE INTEGER        **
!               **  THE INPUT IS A DUAL SERIES--                      **
!               **     1) NUMBER OF DEFECTIVE ITEMS IN THE SUBSAMPLE  **
!               **     2) TOTAL NUMBER OF ITEMS IN THE SAMPLE         **
!               **  THE CONFIDENCE BAND IS GOTTEN BY ASSUMING BINOMIAL**
!               **  NOTE--THE PN CHART SHOULD BE USED ONLY WHEN       **
!               **        THE SUBSAMPLE SIZE IS CONSTANT.             **
!               **        FOR VARYING SUBSAMPLE SIZE, USE THE P CHART **
!               **        (ISHIKAWA, GUIDE TO QUALITY CONTROL, PAGE 77)*
!               ********************************************************
!
 1600 CONTINUE
!
      ISTEPN='5.6'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'QCC2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      SUM1=0.0
      SUM2=0.0
      ANUMSE=NUMSET
      DO 1610 ISET=1,NUMSET
      SUM1=SUM1+Y(ISET)
      SUM2=SUM2+YN(ISET)
 1610 CONTINUE
      CTOTAL=SUM1
      ANTOT=SUM2
      PBARG=CTOTAL/ANTOT
      ANBARG=ANTOT/ANUMSE
      CBARG=PBARG*ANBARG
!
      J=0
      DO 1650 ISET=1,NUMSET
!
      CI=Y(ISET)
      ANI=YN(ISET)
      NI=INT(ANI+0.5)
      IF(NI.LE.0)GO TO 1650
!
      PI=CI/ANI
      TAGI=XIDTEM(ISET)
!
      J=J+1
      Y2(J)=CI
      X2(J)=TAGI
      D2(J)=1.0
!
      J=J+1
      YMID=CBARG
      Y2(J)=YMID
      X2(J)=TAGI
      D2(J)=2.0
!
      J=J+1
      VARCI=0.0
      IF(ANBARG.GT.0.0)VARCI=ANBARG*PBARG*(1.0-PBARG)
      SDCI=0.0
      IF(VARCI.GT.0.0)SDCI=SQRT(VARCI)
      YUPPER=YMID+3.0*SDCI
      Y2(J)=YUPPER
      X2(J)=TAGI
      D2(J)=3.0
!
      J=J+1
      YLOWER=YMID-3.0*SDCI
      IF(YLOWER.LT.0.0)YLOWER=0.0
      Y2(J)=YLOWER
      X2(J)=TAGI
      D2(J)=4.0
!
      IF(CCTARG.EQ.CPUMIN)GO TO 1671
      J=J+1
      Y2(J)=CCTARG
      X2(J)=XIDTEM(ISET)
      D2(J)=5.0
 1671 CONTINUE
!
      IF(CCUSL.EQ.CPUMIN)GO TO 1672
      J=J+1
      Y2(J)=CCUSL
      X2(J)=XIDTEM(ISET)
      D2(J)=6.0
 1672 CONTINUE
!
      IF(CCLSL.EQ.CPUMIN)GO TO 1673
      J=J+1
      Y2(J)=CCLSL
      X2(J)=XIDTEM(ISET)
      D2(J)=7.0
 1673 CONTINUE
!
 1650 CONTINUE
      N2=J
      NPLOTV=3
      GO TO 9000
!
!               ********************************************************
!               **  STEP 5.7--                                        **
!               **  TREAT THE Q U CONTROL CHART CASE (POISSON)        **
!               **  DEFECTIVE PER UNIT
!               **  DEFECTIVE PER UNIT AREA
!               **  NUMBER DEFECTIVE PER SUB-BATCH / LENGTH OR AREA
!               **  THE INPUT IS A DUAL SERIES--
!               **     1) NUMBER OF DEFECTIVE ITEMS IN THE SUBSAMPLE
!               **     2) LENGTH OR AREA OF THE ITEM
!               **  THE CONFIDENCE BAND IS GOTTEN BY ASSUMING POISSON**
!               ********************************************************
!
 1700 CONTINUE
!
      ISTEPN='5.7'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'QCC2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      SUM1=0.0
      SUM2=0.0
      DO 1710 ISET=1,NUMSET
      SUM1=SUM1+Y(ISET)
      SUM2=SUM2+YN(ISET)
 1710 CONTINUE
      CTOTAL=SUM1
      SIZTOT=SUM2
      CBARG=CTOTAL/SIZTOT
!
      J=0
      DO 1750 ISET=1,NUMSET
!
      CI=Y(ISET)
      SIZEI=YN(ISET)
      NSIZEI=INT(SIZEI+0.5)
      IF(NSIZEI.LE.0)GO TO 1750
!
      TAGI=XIDTEM(ISET)
!
      J=J+1
      Y2(J)=(-1.0)
      IF(SIZEI.NE.0.0)Y2(J)=CI/SIZEI
      X2(J)=TAGI
      D2(J)=1.0
!
      J=J+1
      YMID=CBARG
      Y2(J)=YMID
      X2(J)=TAGI
      D2(J)=2.0
!
      J=J+1
      VARCI=0.0
      IF(ANI.GT.0.0)VARCI=CBARG/SIZEI
      SDCI=0.0
      IF(VARCI.GT.0.0)SDCI=SQRT(VARCI)
      YUPPER=YMID+3.0*SDCI
      Y2(J)=YUPPER
      X2(J)=TAGI
      D2(J)=3.0
!
      J=J+1
      YLOWER=YMID-3.0*SDCI
      IF(YLOWER.LT.0.0)YLOWER=0.0
      Y2(J)=YLOWER
      X2(J)=TAGI
      D2(J)=4.0
!
      IF(CCTARG.EQ.CPUMIN)GO TO 1771
      J=J+1
      Y2(J)=CCTARG
      X2(J)=XIDTEM(ISET)
      D2(J)=5.0
 1771 CONTINUE
!
      IF(CCUSL.EQ.CPUMIN)GO TO 1772
      J=J+1
      Y2(J)=CCUSL
      X2(J)=XIDTEM(ISET)
      D2(J)=6.0
 1772 CONTINUE
!
      IF(CCLSL.EQ.CPUMIN)GO TO 1773
      J=J+1
      Y2(J)=CCLSL
      X2(J)=XIDTEM(ISET)
      D2(J)=7.0
 1773 CONTINUE
!
 1750 CONTINUE
      N2=J
      NPLOTV=3
      GO TO 9000
!
!               ********************************************************
!               **  STEP 5.8--                                        **
!               **  TREAT THE Q C CONTROL CHART CASE (POISSON)         **
!               **  TOTAL NUMBER DEFECTIVE IN A BATCH (SUBSAMPLE)    **
!               **  SUM OF DEFECTIVES IN A BATCH (SUBSAMPLE)         **
!               **  THE INPUT IS USUALLY A SERIES OF INTEGERS        **
!               **  THE VALUE WILL BE A NON-NEGATIVE INTEGER         **
!               **  THE CONFIDENCE BAND IS GOTTEN BY ASSUMING POISSON**
!               **  NOTE--THE C CHART SHOULD BE USED ONLY WHEN
!               **        THE SUBSAMPLE SIZE IS CONSTANT.
!               **        FOR VARYING SUBSAMPLE SIZE, USE THE U CHART
!               **        (ISHIKAWA, GUIDE TO QUALITY CONTROL, PAGE 77)
!               ********************************************************
!
 1800 CONTINUE
!
      ISTEPN='5.8'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'QCC2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      SUM1=0.0
      SUM2=0.0
      ANUMSE=NUMSET
      DO 1810 ISET=1,NUMSET
      SUM1=SUM1+Y(ISET)
      IF(NUMV2.LE.2)SUM2=SUM2+1
      IF(NUMV2.GE.3)SUM2=SUM2+YN(ISET)
 1810 CONTINUE
      CTOTAL=SUM1
      CBARG=CTOTAL/ANUMSE
!
      J=0
      DO 1850 ISET=1,NUMSET
!
      CI=Y(ISET)
      SIZEI=YN(ISET)
      NSIZEI=INT(SIZEI+0.5)
      IF(NSIZEI.LE.0)GO TO 1850
!
      TAGI=XIDTEM(ISET)
!
      J=J+1
      Y2(J)=CI
      X2(J)=TAGI
      D2(J)=1.0
!
      J=J+1
      YMID=CBARG
      Y2(J)=YMID
      X2(J)=TAGI
      D2(J)=2.0
!
      J=J+1
      VARCI=0.0
      IF(ANI.GT.0.0)VARCI=CBARG
      SDCI=0.0
      IF(VARCI.GT.0.0)SDCI=SQRT(VARCI)
      YUPPER=YMID+3.0*SDCI
      Y2(J)=YUPPER
      X2(J)=TAGI
      D2(J)=3.0
!
      J=J+1
      YLOWER=YMID-3.0*SDCI
      IF(YLOWER.LT.0.0)YLOWER=0.0
      Y2(J)=YLOWER
      X2(J)=TAGI
      D2(J)=4.0
!
      IF(CCTARG.EQ.CPUMIN)GO TO 1871
      J=J+1
      Y2(J)=CCTARG
      X2(J)=XIDTEM(ISET)
      D2(J)=5.0
 1871 CONTINUE
!
      IF(CCUSL.EQ.CPUMIN)GO TO 1872
      J=J+1
      Y2(J)=CCUSL
      X2(J)=XIDTEM(ISET)
      D2(J)=6.0
 1872 CONTINUE
!
      IF(CCLSL.EQ.CPUMIN)GO TO 1873
      J=J+1
      Y2(J)=CCLSL
      X2(J)=XIDTEM(ISET)
      D2(J)=7.0
 1873 CONTINUE
!
 1850 CONTINUE
      N2=J
      NPLOTV=3
      GO TO 9000
!
!               ******************
!               **   STEP 90--  **
!               **   EXIT       **
!               ******************
!
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'QCC2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPQCC2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICASPL,N,NUMSET,N2,IERROR
 9012   FORMAT('ICASPL,N,NUMSET,N2,IERROR = ',A4,3I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NUMV2,ISIZE
 9013   FORMAT('NUMV2,ISIZE = ',2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)AN,XBARG,SDG,RANGEG
 9014   FORMAT('AN,XBARG,SDG,RANGEG = ',4E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)ANUMSE,SIGMAE,RANGEE
 9015   FORMAT('ANUMSE,SIGMAE,RANGEE = ',3E15.7)
        CALL DPWRST('XXX','BUG ')
        DO 9020 I=1,N2
          WRITE(ICOUT,9021)I,Y2(I),X2(I),D2(I)
 9021     FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,2E15.7,F9.2)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPQCC2
      SUBROUTINE DPQUAD(IHARG,NUMARG,IDEFPR,IHMXPR,   &
      IPREC,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE PREICSION SWITCH
!              AS QUADRUPLE PRECISION.
!              THIS IN TURN SPECIFIES THAT SUBSEQUENT
!              CALCULATIONS WILL ALL BE CARRIED OUT
!              IN QUADRUPLE PRECISION.
!              THE SPECIFIED PRECISION SWITCH SPECIFICATION
!              WILL BE PLACED IN THE HOLLERITH VARIABLE IPREC.
!     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
!                     --NUMARG (AN INTEGER VARIABLE)
!                     --IDEFPR (A  HOLLERITH VARIABLE)
!                     --IHMXPR (A  HOLLERITH VARIABLE)
!     OUTPUT ARGUMENTS--IPREC  (A HOLLERITH VARIABLE)
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
!     ORIGINAL VERSION--NOVEMBER  1980.
!     UPDATED         --SEPTEMBER 1981.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IDEFPR
      CHARACTER*4 IHMXPR
      CHARACTER*4 IPREC
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
      IFOUND='YES'
!
      IF(NUMARG.LE.0)GO TO 1120
      IF(IHARG(NUMARG).EQ.'ON')GO TO 1130
      IF(IHARG(NUMARG).EQ.'OFF')GO TO 1120
      IF(IHARG(NUMARG).EQ.'AUTO')GO TO 1130
      IF(IHARG(NUMARG).EQ.'DEFA')GO TO 1120
      GO TO 1130
!
 1120 CONTINUE
      IHOLD=IDEFPR
      GO TO 1160
!
 1130 CONTINUE
      IHOLD='QUAD'
      GO TO 1160
!
 1160 CONTINUE
      IF(IHOLD.EQ.'DOUB'.AND.IHMXPR.EQ.'SING')GO TO 1170
      IF(IHOLD.EQ.'TRIP'.AND.IHMXPR.EQ.'SING')GO TO 1170
      IF(IHOLD.EQ.'TRIP'.AND.IHMXPR.EQ.'DOUB')GO TO 1170
      IF(IHOLD.EQ.'QUAD'.AND.IHMXPR.EQ.'SING')GO TO 1170
      IF(IHOLD.EQ.'QUAD'.AND.IHMXPR.EQ.'DOUB')GO TO 1170
      IF(IHOLD.EQ.'QUAD'.AND.IHMXPR.EQ.'TRIP')GO TO 1170
      GO TO 1180
!
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
      GO TO 1199
!
 1180 CONTINUE
      IPREC=IHOLD
!
      IF(IFEEDB.EQ.'OFF')GO TO 1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1188)IPREC
 1188 FORMAT('THE PRECISION SWITCH HAS JUST BEEN SET TO ',   &
      A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GO TO 1199
!
 1199 CONTINUE
      RETURN
      END SUBROUTINE DPQUAD
      SUBROUTINE DPQUAN(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,   &
                        IANGLU,MAXNPP,IBOOSS,ISEED,   &
                        IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--FORM A QUANTILE PLOT
!              (USEFUL FOR DISTRIBUTIONALLY COMPARING 2 DATA SETS).
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--87/5
!     ORIGINAL VERSION--MAY       1987.
!     UPDATED         --MARCH     1988. ACTIVATE QUANTILE-QUANTILE
!     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE COMMON
!                                       MOVE SOME DIMENSIONS FROM DPQUA2
!     UPDATED         --FEBRUARY  2011. USE DPPARS, DPPAR3
!     UPDATED         --FEBRUARY  2011. SUPPORT FOR "HIGHLIGHTED" OPTION
!     UPDATED         --JUNE      2016. ALLOW USER-SPECIFED PERCENTILES
!     UPDATED         --JUNE      2016. SAVE A0, A1, PPCC VALUES FROM
!                                       PLOT
!     UPDATED         --JUNE      2016. BOOTSTRAP FOR POINT WISE
!                                       CONFIDENCE INTERVALS
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
      CHARACTER*4 ICASE
      CHARACTER*4 IHIGH
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 ISUBN0
!
      PARAMETER (MAXSPN=10)
      CHARACTER*4 IVARN1(MAXSPN)
      CHARACTER*4 IVARN2(MAXSPN)
      CHARACTER*4 IVARTY(MAXSPN)
      REAL PVAR(MAXSPN)
      INTEGER ILIS(MAXSPN)
      INTEGER NRIGHT(MAXSPN)
      INTEGER ICOLR(MAXSPN)
      CHARACTER*40 INAME
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZI.INC'
!
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
!
      INTEGER ITEMP1(MAXOBV)
!
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
!
!CCCC END CHANGE
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPQU'
      ISUBN2='AN  '
      IFOUND='NO'
      IERROR='NO'
      IHIGH='OFF'
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
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
!
!               ***********************************
!               **  TREAT THE QUANTILE PLOT CASE **
!               ***********************************
!
!               ***************************
!               **  STEP 11--            **
!               **  EXTRACT THE COMMAND  **
!               ***************************
!
      ISTEPN='11'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'QUAN')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICOM.EQ.'QUAN')THEN
        IF(IHARG(1).EQ.'QUAN' .AND. IHARG(2).EQ.'PLOT')THEN
          ILASTC=2
          IFOUND='YES'
        ELSEIF(IHARG(1).EQ.'QUAN' .AND. IHARG(2).EQ.'HIGH' .AND.   &
               IHARG(3).EQ.'PLOT')THEN
          ILASTC=3
          IFOUND='YES'
          IHIGH='ON'
        ELSEIF(IHARG(1).EQ.'QUAN' .AND. IHARG(2).EQ.'SUBS' .AND.   &
               IHARG(3).EQ.'PLOT')THEN
          ILASTC=3
          IFOUND='YES'
          IHIGH='ON'
        ENDIF
      ELSEIF(ICOM.EQ.'HIGH' .OR. ICOM.EQ.'SUBS')THEN
        IF(IHARG(1).EQ.'QUAN' .AND. IHARG(2).EQ.'QUAN' .AND.   &
           IHARG(3).EQ.'PLOT')THEN
          ILASTC=3
          IFOUND='YES'
          IHIGH='ON'
        ENDIF
      ENDIF
!
      IF(IFOUND.EQ.'NO')GO TO 9000
!
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      ICASPL='QUAN'
!
!               ****************************************
!               **  STEP 2--                          **
!               **  EXTRACT THE VARIABLE LIST         **
!               ****************************************
!
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'QUAN')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
      DO 290 I=1,MAX(NRIGHT(1),NRIGHT(2))
        XHIGH(I)=1.0
  290 CONTINUE
!
!     IN ORDER TO ACCOMODATE MATRIX ARGUMENTS, CALL EACH
!     VARIABLE SEPARATELY.
!
      NUMVA2=1
      ICOL=1
      CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                  INAME,IVARN1,IVARN2,IVARTY,   &
                  ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                  MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                  MAXCP4,MAXCP5,MAXCP6,   &
                  V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                  Y1,Y1,Y1,NS1,NTEMP,NTEMP,ICASE,   &
                  IBUGG3,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      ICOL=2
      CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                  INAME,IVARN1,IVARN2,IVARTY,   &
                  ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                  MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                  MAXCP4,MAXCP5,MAXCP6,   &
                  V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                  Y2,Y2,Y2,NS2,NTEMP,NTEMP,ICASE,   &
                  IBUGG3,ISUBRO,IFOUND,IERROR)
!
      IF(IHIGH.EQ.'ON')THEN
        ICOL=3
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                    INAME,IVARN1,IVARN2,IVARTY,   &
                    ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                    MAXCP4,MAXCP5,MAXCP6,   &
                    V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                    XHIGH,XHIGH,XHIGH,NHIGH,NTEMP,NTEMP,ICASE,   &
                    IBUGG3,ISUBRO,IFOUND,IERROR)
      ELSE
        NHIGH=0
      ENDIF
!
!               ********************************************************
!               **  STEP 41--                                          *
!               **  FORM THE VERTICAL AND HORIZONTAL AXIS              *
!               **  VARIABLES (Y(.) AND X(.), RESPECTIVELY) FOR THE    *
!               **  PLOT.  FORM THE CURVE DESIGNATION VARIABLE D(.)  . *
!               **  THIS WILL BE BOTH ONES FOR BOTH CASES              *
!               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).      *
!               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).      *
!               ********************************************************
!
      ISTEPN='41'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'QUAN')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPQUA2(Y1,NS1,Y2,NS2,XHIGH,NHIGH,ICASPL,MAXN,IQQNPR,   &
                  TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,ITEMP1,   &
                  Y,X,D,NPLOTP,NPLOTV,   &
                  Y1SAVE,Y2SAVE,XDIST,   &
                  IQQBOO,IBOOSS,ISEED,A0,A1,PPCC,   &
                  IBUGG3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      IH='PPCC'
      IH2='    '
      VALUE0=PPCC
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                  IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                  IANS,IWIDTH,IBUGG3,IERROR)
!
      IH='PPA0'
      IH2='    '
      VALUE0=A0
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                  IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                  IANS,IWIDTH,IBUGG3,IERROR)
!
      IH='PPA1'
      IH2='    '
      VALUE0=A1
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,   &
                  IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                  IANS,IWIDTH,IBUGG3,IERROR)
!
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
 9013   FORMAT('NPLOTV,NPLOTP,ICASPL,IAND1,IAND2 = ',   &
               2I8,2X,2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)ICASPL,IHIGH,MAXN,NUMVAR
 9014   FORMAT('ICASPL,IHIGH,MAXN,NUMVAR = ',A4,2X,A4,2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)NS1,NS2,NHIGH
 9015   FORMAT('NS1,NS2,NHIGH = ',3I8)
        CALL DPWRST('XXX','BUG ')
        IF(NPLOTP.GT.0)THEN
          DO 9020 I=1,NPLOTP
            WRITE(ICOUT,9021)I,Y(I),X(I),D(I)
 9021       FORMAT('I,Y(I),X(I),D(I) = ',I8,3G15.7)
            CALL DPWRST('XXX','BUG ')
 9020     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPQUAN
      SUBROUTINE DPQUA2(Y,NY,X,NX,XHIGH,NHIGH,ICASPL,MAXN,IQQNPR,   &
                        TEMP1,TEMP2,TEMP3,TEMP4,AINDEX,INDX,   &
                        Y2,X2,D2,N2,NPLOTV,   &
                        YSAVE,XSAVE,XDIST,   &
                        IQQBOO,IBOOSS,ISEED,A0,A1,PPCC,   &
                        IBUGG3,ISUBRO,IERROR)
!
!     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS THAT WILL DEFINE
!              A QUANTILE PLOT
!              (USEFUL FOR DISTRIBUTIONALLY COMPARING 2 DATA SETS).
!     NOTE--THE QUANTILES FOR THE FIRST  ARGUMENT WILL APPEAR VERTICALLY;
!           THE QUANTILES FOR THE SECOND ARGUMENT WILL APPEAR HORIZONTALLY.
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
!     UPDATED         --MARCH     1988.  PUT IN DIAGONAL REFERENCE LINE
!     UPDATED         --JUNE      1990.  MOVE SOME DIMENSIONS TO DPQUAN
!     UPDATED         --APRIL     1992.  N TO NX IN DEBUG STATEMENTS
!     UPDATED         --NOVEMBER  1994.  EQUATE ICASE TO ICASPL
!     UPDATED         --FEBRUARY  2011.  SUPPORT FOR "HIGHLIGHT" OPTION
!     UPDATED         --JUNE      2016. ALLOW USER-SPECIFED PERCENTILES
!     UPDATED         --JUNE      2016. SAVE A0, A1, PPCC VALUES FROM
!                                       PLOT
!     UPDATED         --JUNE      2016. DON'T TREAT N=1 OR ALL DATA
!                                       VALUES EQUAL AS AN ERROR.  TREAT
!                                       AS A "DEGENERATE" CASE.
!     UPDATED         --JUNE      2016. BOOTSTRAP FOR POINT WISE
!                                       CONFIDENCE INTERVALS
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IQQBOO
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ICASE
      CHARACTER*4 ICASJB
      CHARACTER*4 IOP
!CCCC ADD FOLLOWING LINE NOVEMBER 1994.
      CHARACTER*4 ICASPL
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IWRITE
      CHARACTER*1 IATEMP
!
!---------------------------------------------------------------------
!
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
!
      INTEGER INDX(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPQU'
      ISUBN2='A2  '
      IERROR='NO'
      IWRITE='OFF'
      ICASE=ICASPL
      ICASJB='BOOT'
!
      ANY=NY
      ANX=NX
      NTAG=0
      NXSAVE=0
      NYSAVE=0
!
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
          DO 61 I=1,NY
            WRITE(ICOUT,62)I,Y(I)
   62       FORMAT('I,Y(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
   61     CONTINUE
        ENDIF
        IF(NX.GE.1)THEN
          DO 71 I=1,NX
            WRITE(ICOUT,72)I,X(I)
   72       FORMAT('I,X(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
   71     CONTINUE
        ENDIF
        IF(NHIGH.GE.1)THEN
          DO 81 I=1,NHIGH
            WRITE(ICOUT,82)I,XHIGH(I)
   82       FORMAT('I,XHIGH(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
   81     CONTINUE
        ENDIF
      ENDIF
!
!               ********************************************
!               **  STEP 11--                             **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      ISTEPN='11'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'QUA2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     2016/06: ONLY REQUIRE N >= 1.
!
!CCCC IF(NY.LT.2)THEN
      IF(NY.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1111)
 1111   FORMAT('***** ERROR IN QUANTILE-QUANTILE PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1112)
 1112   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE FIRST ',   &
               'RESPONSE VARIABLE IS LESS THAN ONE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1114)NY
 1114   FORMAT('      THE NUMBER OF OBSERVATIONS = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(NX.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1122)
 1122   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE SECOND ',   &
               'RESPONSE VARIABLE IS LESS THAN ONE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1114)NX
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(NHIGH.GT.0 .AND. NHIGH.NE.MIN(NX,NY))THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1125)
 1125   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE HIGHLIGHTING ',   &
               'VARIABLE IS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1126)
 1126   FORMAT('      NOT EQUAL TO THE NUMBER OF OBSERVATIONS IN THE ',   &
               'SHORTER RESPONSE VARIABLE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1127)NY
 1127   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE FIRST     ',   &
               'RESPONSE VARIABLE = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1128)NX
 1128   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE SECOND    ',   &
               'RESPONSE VARIABLE = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1129)NHIGH
 1129   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE HIGHLIGHT ',   &
               'VARIABLE          = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!CCCC HOLD=Y(1)
!CCCC DO1130I=1,NY
!CCCC   IF(Y(I).NE.HOLD)GO TO 1139
!1130 CONTINUE
!CCCC WRITE(ICOUT,999)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,1111)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,1132)
!1132 FORMAT('      ALL ELEMENTS FOR THE FIRST RESPONSE VARIABLE')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,1133)HOLD
!1133 FORMAT('      ARE IDENTICALLY EQUAL TO ',G15.7)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,999)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC IERROR='YES'
!CCCC GO TO 9000
!1139 CONTINUE
!
!CCCC HOLD=X(1)
!CCCC DO1140I=1,NY
!CCCC   IF(X(I).NE.HOLD)GO TO 1149
!1140 CONTINUE
!CCCC WRITE(ICOUT,999)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,1111)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,1142)
!1142 FORMAT('      ALL ELEMENTS FOR THE SECOND RESPONSE VARIABLE')
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,1143)HOLD
!1143 FORMAT('      ARE IDENTICALLY EQUAL TO ',G15.7)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,999)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC IERROR='YES'
!CCCC GO TO 9000
!1149 CONTINUE
!
      IF(IQQBOO.EQ.'ON')THEN
        DO 1210 II=1,NX
          XSAVE(II)=X(II)
 1210   CONTINUE
        NXSAVE=NX
        DO 1220 II=1,NY
          YSAVE(II)=Y(II)
 1220   CONTINUE
        NYSAVE=NY
!
        IOP='OPEN'
        IFLAG1=1
        IFLAG2=0
        IFLAG3=0
        IFLAG4=0
        IFLAG5=0
        CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                    IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                    IBUGG3,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
      ENDIF
!
      IPASS=-1
!
!               ****************************************************
!               **  STEP 21--                                     **
!               **  SORT Y AND SORT X                             **
!               ****************************************************
!
 2000 CONTINUE
      IPASS=IPASS+1
!
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'QUA2')THEN
        WRITE(ICOUT,2002)
 2002   FORMAT('IPASS: ',I10)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(IPASS.GT.0)THEN
        IF(IQQBOO.EQ.'OFF' .OR. IPASS.GT.IBOOSS)THEN
          GO TO 8000
        ELSE
          DO 2001 II=1,NXSAVE
            X(II)=XSAVE(II)
 2001     CONTINUE
          NX=NXSAVE
          DO 2003 II=1,NYSAVE
            Y(II)=YSAVE(II)
 2003     CONTINUE
          NY=NYSAVE
!
!         FOR BOOTSTRAP, FIX X ARRAY BUT CREATE BOOTSTRAP
!         ARRAY FOR Y ARRAY.
!
          IJACIN=0
!CCCC     CALL DPJBS3(X,NX,ICASJB,IJACIN,ISEED,TEMP4,NX2,
!CCCC1                INDX,AINDEX,
!CCCC1                IBUGG3,IERROR)
!CCCC     DO2006II=1,NX
!CCCC       X(II)=TEMP4(II)
!2006     CONTINUE
!
          CALL DPJBS3(Y,NY,ICASJB,IJACIN,ISEED,TEMP4,NY2,   &
                      INDX,AINDEX,   &
                      IBUGG3,IERROR)
          DO 2008 II=1,NY
            Y(II)=TEMP4(II)
 2008     CONTINUE
!
        ENDIF
      ENDIF
!
      ISTEPN='21'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'QUA2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NHIGH.LE.0)THEN
        IF(IQQNPR.GT.0)THEN
          CALL PERCE2(IQQNPR,X,NX,IWRITE,TEMP3,MAXN,TEMP1,   &
                      IBUGG3,ISUBRO,IERROR)
          DO 2010 II=1,IQQNPR
            X(II)=TEMP1(II)
 2010     CONTINUE
          NX=IQQNPR
!
          CALL PERCE2(IQQNPR,Y,NY,IWRITE,TEMP3,MAXN,TEMP2,   &
                      IBUGG3,ISUBRO,IERROR)
          DO 2020 II=1,IQQNPR
            Y(II)=TEMP2(II)
 2020     CONTINUE
          NY=IQQNPR
!
        ELSE
          CALL SORT(X,NX,X)
          CALL SORT(Y,NY,Y)
        ENDIF
      ELSEIF(NY.LE.NX)THEN
        CALL SORT(X,NX,X)
        CALL SORTC(Y,XHIGH,NY,Y,XDIST)
        DO 2101 I=1,NY
          XHIGH(I)=XDIST(I)
 2101   CONTINUE
      ELSEIF(NY.GT.NX)THEN
        CALL SORT(Y,NY,Y)
        CALL SORTC(X,XHIGH,NX,X,XDIST)
        DO 2103 I=1,NX
          XHIGH(I)=XDIST(I)
 2103   CONTINUE
      ENDIF
!
!               *****************************************
!               **  STEP 22--                          **
!               **  DETERMINE THE TYPE CASE            **
!               **  EQUAL SAMPLE SIZES OR NOT)         **
!               **  AND BRANCH ACORDINGLY              **
!               *****************************************
!
      ISTEPN='22'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'QUA2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NY.LT.NX)THEN
        CALL PERCE2(NY,X,NX,IWRITE,TEMP3,MAXN,TEMP2,   &
                    IBUGG3,ISUBRO,IERROR)
        DO 2120 II=1,NY
          X(II)=TEMP2(II)
 2120   CONTINUE
        NX=NY
      ELSEIF(NY.GT.NX)THEN
        CALL PERCE2(NX,Y,NY,IWRITE,TEMP3,MAXN,TEMP2,   &
                    IBUGG3,ISUBRO,IERROR)
        DO 2130 II=1,NX
          Y(II)=TEMP2(II)
 2130   CONTINUE
        NY=NX
      ENDIF
!
!               *******************************************
!               **  STEP 51--                            **
!               **  FORM PLOT COORDINATES                **
!               *******************************************
!
      ISTEPN='51'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'QUA2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IPASS.EQ.0)THEN
        IF(NHIGH.GT.0)THEN
          CALL CODE(XHIGH,NHIGH,IWRITE,XDIST,D2,MAXN,IBUGG3,IERROR)
          CALL MAXIM(XDIST,NHIGH,IWRITE,XMAX,IBUGG3,IERROR)
        ENDIF
!
        J=0
        DO 5111 I=1,NX
          J=J+1
          Y2(J)=Y(J)
          X2(J)=X(J)
          IF(NHIGH.EQ.0)THEN
            D2(J)=1.0
          ELSE
            D2(J)=XDIST(J)
          ENDIF
 5111   CONTINUE
!
        N2=J
        CALL LINFIT(Y2,X2,N2,   &
                  A0,A1,RESSD,RESDF,PPCC,SDA0,SDA1,CCALBE,   &
                  ISUBRO,IBUGG3,IERROR)
!
        IF(NHIGH.EQ.0)THEN
          NTEMP=1
        ELSE
          NTEMP=INT(XMAX+0.1)
        ENDIF
!
        NTEMP=NTEMP+1
        AMIN=X(1)
        AMIN2=X(1)
        IF(Y(1).LT.X(1))AMIN=Y(1)
        J=J+1
        Y2(J)=AMIN
        X2(J)=AMIN
        D2(J)=REAL(NTEMP)
!
        AMAX=X(NX)
        AMAX2=X(NX)
        IF(Y(NY).GT.X(NX))AMAX=Y(NY)
        J=J+1
        Y2(J)=AMAX
        X2(J)=AMAX
        D2(J)=REAL(NTEMP)
!
!       2016/06: GENERATE FITTED LINE ON THE PLOT
!
        NTEMP=NTEMP+1
        XVAL=AMIN2
        YVAL=A0 + A1*XVAL
        J=J+1
        X2(J)=XVAL
        Y2(J)=YVAL
        D2(J)=REAL(NTEMP)
!
        XVAL=AMAX2
        YVAL=A0 + A1*XVAL
        J=J+1
        X2(J)=XVAL
        Y2(J)=YVAL
        D2(J)=REAL(NTEMP)
!
        N2=J
        NPLOTV=3
        NTAG=NTEMP
      ELSE
        DO 5211 I=1,NX
          WRITE(IOUNI1,5213)IPASS,I,Y(I),X(I)
 5213     FORMAT(2I10,2E15.7)
 5211   CONTINUE
      ENDIF
!
      IF(IQQBOO.EQ.'ON')GO TO 2000
      GO TO 9000
!
 8000 CONTINUE
      IF(IQQBOO.EQ.'ON')THEN
!
!       STEP 1: CLOSE THE FILE CONTAINING THE BOOTSTRAP POINTS
!
        IOP='CLOS'
        CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                    IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                    IBUGG3,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
!       STEP 2: RE-OPEN THE FILE
!
        IOP='OPEN'
        CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                    IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                    IBUGG3,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
!       STEP 3: NOW LOOP THROUGH THE POINTS
!
        NTEMP=MIN(NX,NY)
        DO 8010 II=1,NTEMP
          REWIND(IOUNI1)
          IVAL=II
          ISKIP1=IVAL-1
          ISKIP2=NTEMP-IVAL
          ICNT=0
!
          DO 8020 JJ=1,IBOOSS
            DO 8030 KK=1,ISKIP1
              READ(IOUNI1,'(A1)',END=8091,ERR=8093)IATEMP
 8030       CONTINUE
            ICNT=ICNT+1
            READ(IOUNI1,5213,END=8091,ERR=8093)IJUK1,IJUNK2,   &
                                               TEMP1(ICNT),TEMP2(ICNT)
            DO 8040 KK=1,ISKIP2
              READ(IOUNI1,'(A1)',END=8091,ERR=8093)IATEMP
 8040       CONTINUE
 8020     CONTINUE
          P025=2.5
          CALL PERCEN(P025,TEMP1,ICNT,IWRITE,TEMP3,MAXN,   &
                      Y025,IBUGG3,IERROR)
          CALL PERCEN(P025,TEMP2,ICNT,IWRITE,TEMP3,MAXN,   &
                      X025,IBUGG3,IERROR)
          P975=97.5
          CALL PERCEN(P975,TEMP1,ICNT,IWRITE,TEMP3,MAXN,   &
                      Y975,IBUGG3,IERROR)
          CALL PERCEN(P975,TEMP2,ICNT,IWRITE,TEMP3,MAXN,   &
                      X975,IBUGG3,IERROR)
          N2=N2+1
          X2(N2)=X025
          Y2(N2)=Y025
          D2(N2)=REAL(NTAG+1)
          N2=N2+1
          X2(N2)=X975
          Y2(N2)=Y975
          D2(N2)=REAL(NTAG+2)
!
 8010   CONTINUE
        GO TO 8099
!
!       STEP 4: UNEXPECTED END OF FILE ENCOUNTERED
!
 8091   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8111)
 8111   FORMAT('      UNEXPECTED END OF FILE READING BOOTSTRAP FILE.')
        CALL DPWRST('XXX','BUG ')
        GO TO 8099
!
!       STEP 5: UNEXPECTED ERROR READING FILE
!
 8093   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8113)
 8113   FORMAT('      UNEXPECTED ERROR READING BOOTSTRAP FILE.')
        CALL DPWRST('XXX','BUG ')
        GO TO 8099
!
!       STEP 6: FINAL CLOSE OF FILE
!
 8099   CONTINUE
        IOP='CLOS'
        CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                    IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                    IBUGG3,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
      ENDIF
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
        DO 9015 I=1,N2
          WRITE(ICOUT,9016)I,Y2(I),X2(I),D2(I)
 9016     FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
        WRITE(ICOUT,9053)NY,NX,AMIN,AMAX
 9053   FORMAT('NY,NX,AMIN,AMAX = ',2I8,2G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPQUA2
      SUBROUTINE DPQUCO(XTEMP1,XTEMP2,MAXNXT,ICASAN,   &
                        ICAPSW,IFORSW,IMULT,IREPL,   &
                        ISUBRO,IBUGA2,IBUGA3,IBUGQ,IFOUND,IERROR)
!
!     PURPOSE--GENERATE CONFIDENCE LIMITS FOR QUANTILES (MEDIAN IS
!              A SPECIAL CASE).  METHOD BASED ON MARITZ-JARRETT
!              ESTIMATE FOR STANDARD ERROR.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     REFERENCE--"INTRODUCTION TO ROBUST ESTIMATION AND HYPOTHESIS
!                TESTING", RAND R. WILCOX, ACADEMIC PRESS, 1997.
!                1977.
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2003/2
!     ORIGINAL VERSION--FEBRUARY  2003.
!     UPDATED         --OCTOBER   2003. ADD SUPPORT FOR HTML, LATEX
!                                       OUTPUT
!     UPDATED         --MARCH     2010. USE DPDTA1, DPDTA4 TO GENERATE
!                                       HTML, LATEX, RTF FORMAT
!     UPDATED         --MARCH     2010. SUPPORT FOR MULTIPLE RESPONSE
!                                       VARIABLES AND FOR GROUP-ID
!                                       VARIABLES (I.E., REPLICATION
!                                       CASE)
!     UPDATED         --MARCH     2010. USE DPPAR3 TO EXTRACT EITHER A
!                                       RESPONSE VARIABLE OR A MATRIX
!                                       NAME
!     UPDATED         --AUGUST    2019. ADD CTL999, CTU999
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
!
      LOGICAL IFRST
      LOGICAL ILAST
!
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
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
!
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
!
      DIMENSION XDESGN(MAXOBV,6)
      DIMENSION XIDTEM(MAXOBV)
      DIMENSION XIDTE2(MAXOBV)
      DIMENSION XIDTE3(MAXOBV)
      DIMENSION XIDTE4(MAXOBV)
      DIMENSION XIDTE5(MAXOBV)
      DIMENSION XIDTE6(MAXOBV)
!
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
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPQU'
      ISUBN2='CO  '
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
      IFOUND='YES'
      IERROR='NO'
!
!               *************************************************
!               **  TREAT THE QUANTILE CONFIDENCE LIMITS CASE  **
!               *************************************************
!
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
!
!               *********************************
!               **  STEP 1--                   **
!               **  EXTRACT THE VARIABLE LIST  **
!               *********************************
!
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'QUCO')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
      IF(NUMVAR.GT.1 .AND. IREPL.EQ.'OFF')IMULT='ON'
!
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
          DO 185 I=1,NUMVAR
            WRITE(ICOUT,187)I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),   &
                            ICOLR(I)
  187       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',   &
                   'ICOLR(I) = ',I8,2X,A4,A4,2X,3I8)
            CALL DPWRST('XXX','BUG ')
  185     CONTINUE
        ENDIF
      ENDIF
!
!               ***********************************************
!               **  STEP 2--                                 **
!               **  DETERMINE:                               **
!               **  1) NUMBER OF REPLICATION VARIABLES (0-6) **
!               **  2) NUMBER OF RESPONSE    VARIABLES (>= 1)**
!               ***********************************************
!
      ISTEPN='2'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'QUCO')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NRESP=0
      NREPL=0
!
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
  211     FORMAT('      FOR THE REPLICATION CASE, THE NUMBER OF ',   &
                 'REPLICATION VARIABLES')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,213)NREPL
  213     FORMAT('      THE NUMBER OF REPLICATION VARIABLES = ',I5)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
      ELSE
        NRESP=1
      ENDIF
!
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'QUCO')THEN
        WRITE(ICOUT,221)NRESP,NREPL
  221   FORMAT('NRESP,NREPL = ',2I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!     ******************************************************
!     **  STEP 3--                                        **
!     **  DETERMINE QUANTILE TO USE (FROM P100)           **
!     ******************************************************
!
      IF(ICASAN.EQ.'MECI')THEN
        P100=0.50
      ELSE
        IH='P100'
        IH2='    '
        IHWUSE='P'
        MESSAG='YES'
        CALL CHECKN(IH,IH2,IHWUSE,   &
        IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
        ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
        P100=VALUE(ILOCP)
        IF(P100.GE.1.0 .AND. P100.LE.100.0)P100=P100/100.0
      ENDIF
!
      IF(P100.LE.0.0 .OR. P100.GE.1.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,302)
  302   FORMAT('      THE QUANTILE FOR WHICH THE CONFIDENCE INTERVAL ',   &
               'IS TO BE')
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
        GO TO 9000
      ENDIF
!
!
!               ******************************************************
!               **  STEP 3--                                        **
!               **  GENERATE THE CONFIDENCE LIMITS FOR THE VARIOUS  **
!               **  CASES                                           **
!               ******************************************************
!
      ISTEPN='3'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'QUCO')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!               *****************************************
!               **  STEP 3A--                          **
!               **  CASE 1: SINGLE RESPONSE VARIABLE   **
!               **          WITH NO REPLICATION        **
!               *****************************************
!
      IF(IMULT.EQ.'OFF' .AND. NREPL.EQ.0)THEN
        ISTEPN='3A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'QUCO')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        PID(1)=CPUMIN
        IVARID(1)=IVARN1(1)
        IVARI2(1)=IVARN2(1)
!
        ICOL=1
        NUMVA2=1
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                    INAME,IVARN1,IVARN2,IVARTY,   &
                    ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                    MAXCP4,MAXCP5,MAXCP6,   &
                    V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                    Y,XTEMP1,XTEMP2,NLOCAL,NLOCA2,NLOCA3,ICASE,   &
                    IBUGA3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
!               ******************************************************
!               **  STEP 3B--                                       **
!               **  PREPARE FOR ENTRANCE INTO DPQUC2--              **
!               ******************************************************
!
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
          DO 335 I=1,NLOCAL
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
!
        IERROR='NO'
        CALL DPQUC2(Y,NLOCAL,P100,   &
                    XTEMP1,MAXNXT,   &
                    PID,IVARID,IVARI2,NREPL,   &
                    CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,   &
                    CTL999,CTU999,   &
                    ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,   &
                    ICASAN,ISUBRO,IBUGA3,IERROR)
!
        IFLAGU='ON'
        IFRST=.FALSE.
        ILAST=.FALSE.
        CALL DPCNF3(CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,   &
                    CTL999,CTU999,   &
                    IFLAGU,IFRST,ILAST,ICASAN,   &
                    IBUGA2,IBUGA3,ISUBRO,IERROR)
!
!               *******************************************
!               **  STEP 4A--                            **
!               **  CASE 2: MULTIPLE RESPONSE VARIABLES  **
!               *******************************************
!
      ELSEIF(IMULT.EQ.'ON')THEN
        ISTEPN='4A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'QUCO')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!       LOOP THROUGH EACH OF THE RESPONSE VARIABLES
!
        NCURVE=0
        DO 410 IRESP=1,NRESP
          NCURVE=NCURVE+1
!
          IINDX=ICOLR(IRESP)
          PID(1)=CPUMIN
          IVARID(1)=IVARN1(IRESP)
          IVARI2(1)=IVARN2(IRESP)
!
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'QUCO')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,411)IRESP,NCURVE
  411       FORMAT('IRESP,NCURVE = ',2I5)
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
                      Y,XTEMP1,XTEMP2,NLOCAL,NLOCA2,NLOCA3,ICASE,   &
                      IBUGA3,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GO TO 9000
!
!         *****************************************************
!         **  STEP 4B--                                      **
!         *****************************************************
!
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
              DO 425 I=1,NLOCAL
                WRITE(ICOUT,426)I,Y(I)
  426           FORMAT('I,Y(I) = ',I8,F12.5)
                CALL DPWRST('XXX','BUG ')
  425         CONTINUE
            ENDIF
          ENDIF
!
          CALL DPQUC2(Y,NLOCAL,P100,   &
                      XTEMP1,MAXNXT,   &
                      PID,IVARID,IVARI2,NREPL,   &
                      CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,   &
                      CTL999,CTU999,   &
                      ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,   &
                      ICASAN,ISUBRO,IBUGA3,IERROR)
!
          IFLAGU='FILE'
          IFRST=.FALSE.
          ILAST=.FALSE.
          IF(IRESP.EQ.1)IFRST=.TRUE.
          IF(IRESP.EQ.NRESP)ILAST=.TRUE.
          CALL DPCNF3(CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,   &
                      CTL999,CTU999,   &
                      IFLAGU,IFRST,ILAST,ICASAN,   &
                      IBUGA2,IBUGA3,ISUBRO,IERROR)
!
  410   CONTINUE
!
!               ****************************************************
!               **  STEP 5A--                                     **
!               **  CASE 3: ONE OR MORE REPLICATION VARIABLES.    **
!               **          FOR THIS CASE, ALL VARIABLES MUST     **
!               **          HAVE THE SAME LENGTH.                 **
!               ****************************************************
!
      ELSEIF(IREPL.EQ.'ON')THEN
        ISTEPN='5A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'QUCO')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        J=0
        IMAX=NRIGHT(1)
        IF(NQ.LT.NRIGHT(1))IMAX=NQ
        DO 510 I=1,IMAX
          IF(ISUB(I).EQ.0)GO TO 510
          J=J+1
!
!         RESPONSE VARIABLE IN Y
!
          ICOLC=1
          IJ=MAXN*(ICOLR(ICOLC)-1)+I
          IF(ICOLR(ICOLC).LE.MAXCOL)Y(J)=V(IJ)
          IF(ICOLR(ICOLC).EQ.MAXCP1)Y(J)=PRED(I)
          IF(ICOLR(ICOLC).EQ.MAXCP2)Y(J)=RES(I)
          IF(ICOLR(ICOLC).EQ.MAXCP3)Y(J)=YPLOT(I)
          IF(ICOLR(ICOLC).EQ.MAXCP4)Y(J)=XPLOT(I)
          IF(ICOLR(ICOLC).EQ.MAXCP5)Y(J)=X2PLOT(I)
          IF(ICOLR(ICOLC).EQ.MAXCP6)Y(J)=TAGPLO(I)
!
          IF(NREPL.GE.1)THEN
            DO 520 IR=1,MIN(NREPL,6)
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
!
  510   CONTINUE
        NLOCAL=J
!
        ISTEPN='5B'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'QUCO')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        PID(1)=CPUMIN
        IVARID(1)=IVARN1(1)
        IVARI2(1)=IVARN2(1)
        IADD=1
        DO 540 II=1,NREPL
          IVARID(II+IADD)=IVARN1(II+IADD)
          IVARI2(II+IADD)=IVARN2(II+IADD)
  540   CONTINUE
!
!       *****************************************************
!       **  STEP 5C--                                      **
!       **                                                 **
!       **  FOR THIS CASE, WE NEED TO LOOP THROUGH THE     **
!       **  VARIOUS REPLICATIONS.                          **
!       *****************************************************
!
!
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
            DO 545 I=1,NLOCAL
              WRITE(ICOUT,546)I,Y(I),XDESGN(I,1),XDESGN(I,2)
  546         FORMAT('I,Y(I),XDESGN(I,1),XDESGN(I,2) = ',   &
                     I8,3F12.5)
              CALL DPWRST('XXX','BUG ')
  545       CONTINUE
          ENDIF
        ENDIF
!
!       *****************************************************
!       **  STEP 5C--                                      **
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
!       **  STEP 5D--                                      **
!       **  NOW LOOP THROUGH THE VARIOUS REPLICATIONS      **
!       *****************************************************
!
        NPLOTP=0
        NCURVE=0
        IF(NREPL.EQ.1)THEN
          J=0
          DO 1110 ISET1=1,NUMSE1
            K=0
            PID(IADD+1)=XIDTEM(ISET1)
            DO 1130 I=1,NLOCAL
              IF(XIDTEM(ISET1).EQ.XDESGN(I,1))THEN
                K=K+1
                TEMP1(K)=Y(I)
              ENDIF
 1130       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            IF(NTEMP.GT.0)THEN
              CALL DPQUC2(TEMP1,NTEMP,P100,   &
                          XTEMP1,MAXNXT,   &
                          PID,IVARID,IVARI2,NREPL,   &
                          CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,   &
                          CTL999,CTU999,   &
                          ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,   &
                          ICASAN,ISUBRO,IBUGA3,IERROR)
            ENDIF
!
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NUMSE1)ILAST=.TRUE.
            CALL DPCNF3(CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,   &
                        CTL999,CTU999,   &
                        IFLAGU,IFRST,ILAST,ICASAN,   &
                        IBUGA2,IBUGA3,ISUBRO,IERROR)
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
                TEMP1(K)=Y(I)
              ENDIF
 1290       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              CALL DPQUC2(TEMP1,NTEMP,P100,   &
                          XTEMP1,MAXNXT,   &
                          PID,IVARID,IVARI2,NREPL,   &
                          CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,   &
                          CTL999,CTU999,   &
                          ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,   &
                          ICASAN,ISUBRO,IBUGA3,IERROR)
            ENDIF
            NPLOT2=NPLOTP
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            CALL DPCNF3(CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,   &
                        CTL999,CTU999,   &
                        IFLAGU,IFRST,ILAST,ICASAN,   &
                        IBUGA2,IBUGA3,ISUBRO,IERROR)
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
                TEMP1(K)=Y(I)
              ENDIF
 1390       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            IF(NTEMP.GT.0)THEN
              CALL DPQUC2(TEMP1,NTEMP,P100,   &
                          XTEMP1,MAXNXT,   &
                          PID,IVARID,IVARI2,NREPL,   &
                          CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,   &
                          CTL999,CTU999,   &
                          ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,   &
                          ICASAN,ISUBRO,IBUGA3,IERROR)
            ENDIF
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            CALL DPCNF3(CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,   &
                        CTL999,CTU999,   &
                        IFLAGU,IFRST,ILAST,ICASAN,   &
                        IBUGA2,IBUGA3,ISUBRO,IERROR)
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
                TEMP1(K)=Y(I)
              ENDIF
 1490       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            IF(NTEMP.GT.0)THEN
              CALL DPQUC2(TEMP1,NTEMP,P100,   &
                          XTEMP1,MAXNXT,   &
                          PID,IVARID,IVARI2,NREPL,   &
                          CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,   &
                          CTL999,CTU999,   &
                          ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,   &
                          ICASAN,ISUBRO,IBUGA3,IERROR)
            ENDIF
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            CALL DPCNF3(CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,   &
                        CTL999,CTU999,   &
                        IFLAGU,IFRST,ILAST,ICASAN,   &
                        IBUGA2,IBUGA3,ISUBRO,IERROR)
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
                TEMP1(K)=Y(I)
              ENDIF
 1590       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            IF(NTEMP.GT.0)THEN
              CALL DPQUC2(TEMP1,NTEMP,P100,   &
                          XTEMP1,MAXNXT,   &
                          PID,IVARID,IVARI2,NREPL,   &
                          CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,   &
                          CTL999,CTU999,   &
                          ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,   &
                          ICASAN,ISUBRO,IBUGA3,IERROR)
            ENDIF
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            CALL DPCNF3(CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,   &
                        CTL999,CTU999,   &
                        IFLAGU,IFRST,ILAST,ICASAN,   &
                        IBUGA2,IBUGA3,ISUBRO,IERROR)
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
                TEMP1(K)=Y(I)
              ENDIF
 1690       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            IF(NTEMP.GT.0)THEN
              CALL DPQUC2(TEMP1,NTEMP,P100,   &
                          XTEMP1,MAXNXT,   &
                          PID,IVARID,IVARI2,NREPL,   &
                          CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,   &
                          CTL999,CTU999,   &
                          ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,   &
                          ICASAN,ISUBRO,IBUGA3,IERROR)
            ENDIF
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            CALL DPCNF3(CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,   &
                        CTL999,CTU999,   &
                        IFLAGU,IFRST,ILAST,ICASAN,   &
                        IBUGA2,IBUGA3,ISUBRO,IERROR)
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
!
      RETURN
      END SUBROUTINE DPQUCO
      SUBROUTINE DPQUC2(Y,N,P100,   &
                        XTEMP1,MAXNXT,   &
                        PID,IVARID,IVARI2,NREPL,   &
                        CUTL90,CUTU90,CUTL95,CUTU95,CUTL99,CUTU99,   &
                        CTL999,CTU999,   &
                        ICAPSW,ICAPTY,IFORSW,IRTFFF,IRTFFP,   &
                        ICASAN,ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS ROUTINE GENERATES QUANTILE CONFIDENCE LIMITS
!              FOR THE DATA IN THE INPUT VECTOR Y.
!              THE MEDIAN IS A SPECIAL CASE.  SPECIFICALLY,
!                   X(0.5) +/- NORPPF(1-ALPHA/2)*QUASE
!              WHERE QUASE IS THE MARITZ-JARRETT ESTIMATE OF
!              THE QUANTILE STANDARD ERROR.
!              METHOD FROM PAGE 87 OF THE RAND WILCOX BOOK
!              "INTRODUCTION TO ROBUST ESTIMATION AND HYPOTHESIS
!              TESTING", ACADEMIC PRESS, 1997.
!              ALSO VIA THE HETTMANSPERGER-SHEATHER INTERPOLATION
!              METHOD (ALSO PAGE 87 OF WILCOX).
!     NOTE--ASSUMPTION--MODEL IS   RESPONSE = CONSTANT + ERROR.
!     INPUT  ARGUMENTS--Y      = THE SINGLE PRECISION VECTOR
!                                OF OBSERVATIONS
!                       N      = THE INTEGER NUMBER OF
!                                OBSERVATIONS IN THE VECTOR Y.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2003/2
!     ORIGINAL VERSION--FEBRUARY  2003.
!     UPDATED         --OCTOBER   2003. ADD SUPPORT FOR HTML, LATEX
!                                       OUTPUT
!     UPDATED         --MARCH     2010. USE DPDTA2 AND DPDTA4 TO
!                                       GENERATE OUTPUT (ADDS RTF
!                                       SUPPORT)
!     UPDATED         --MARCH     2010. SOME MODIFICATIONS TO THE
!                                       OUTPUT (AESTHETIC, NOT
!                                       SUBSTANTIVE)
!     UPDATED         --AUGUST    2019. ADD CTL999, CTU999
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASAN
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ICASA2
      CHARACTER*4 IQUASE
      CHARACTER*4 IQUAME
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*40 IRTFFF
      CHARACTER*40 IRTFFP
!
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
      DIMENSION XTEMP1(*)
      DIMENSION PID(*)
!
      PARAMETER (NUMALP=8)
!
      DIMENSION CONF(NUMALP)
      DIMENSION T(NUMALP)
      DIMENSION TSDM(NUMALP)
      DIMENSION ALOWER(NUMALP)
      DIMENSION AUPPER(NUMALP)
      DIMENSION ALOWE2(NUMALP)
      DIMENSION AUPPE2(NUMALP)
!
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
!
      DOUBLE PRECISION DCDF
      DOUBLE PRECISION DPPF
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
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
        DO 56 I=1,N
          WRITE(ICOUT,57)I,Y(I)
   57     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
        WRITE(ICOUT,58)ICASAN,ISUBRO,IBUGA3,IERROR
   58   FORMAT('ICASAN,ISUBRO,IBUGA3,IERROR = ',4(A4,2X))
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      ISUBN1='DPQU'
      ISUBN2='C2  '
      IWRITE='OFF'
!CCCC IERROR='NO'
      ICASA2='QUCO'
      IQUAME='ORDE'
      IQUASE='MJ'
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
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'QUC2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(N.LT.3)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN QUANTILE CONFIDENCE LIMITS--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS IN THE RESPONSE ',   &
               'VARIABLE IS LESS THAN 3')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,113)N
  113   FORMAT('SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      HOLD=Y(1)
      DO 135 I=2,N
      IF(Y(I).NE.HOLD)GO TO 139
  135 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,131)HOLD
  131 FORMAT('      THE RESPONSE VARIABLE HAS ALL ELEMENTS = ',G15.7)
      CALL DPWRST('XXX','WRIT')
      GO TO 9000
  139 CONTINUE
!
!               ***************************************************
!               **  STEP 3--                                     **
!               **  COMPUTE THE QUANTILE              ESTIMATE   **
!               **  COMPUTE THE QUANTILE     STANDARD ERROR      **
!               ***************************************************
!
!
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'QUC2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IWRITE='OFF'
!
      CALL MINIM(Y,N,IWRITE,XMIN,IBUGA3,IERROR)
      CALL MAXIM(Y,N,IWRITE,XMAX,IBUGA3,IERROR)
      IF(ICASAN.EQ.'MECI')THEN
        CALL MEDIAN(Y,N,IWRITE,XTEMP1,MAXNXT,XMED,IBUGA3,IERROR)
        XQUANT=XMED
      ELSE
        CALL MEDIAN(Y,N,IWRITE,XTEMP1,MAXNXT,XMED,IBUGA3,IERROR)
        CALL QUANT(P100,Y,N,IWRITE,XTEMP1,MAXNXT,IQUAME,XQUANT,   &
        IBUGA3,IERROR)
      ENDIF
      CALL QUANSE(P100,Y,N,IWRITE,XTEMP1,MAXNXT,IQUASE,XQUASE,   &
      IBUGA3,IERROR)
!
!               ***************************************
!               **  STEP 4--                         **
!               **  COMPUTE CONFIDENCE LIMITS        **
!               **  FOR VARIOUS PROBABILITY VALUES.  **
!               ***************************************
!
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'QUC2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CONF(1)=50.0
      CONF(2)=75.0
      CONF(3)=90.0
      CONF(4)=95.0
      CONF(5)=99.0
      CONF(6)=99.9
      CONF(7)=99.99
      CONF(8)=99.999
!
      DO 1400 I=1,8
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
!
!               ***************************************
!               **  STEP 5--                         **
!               **  COMPUTE CONFIDENCE LIMITS        **
!               **  FOR HETTMANSPERGER-SHEATHER      **
!               **  INTERPOLATION METHOD.            **
!               ***************************************
!
      IF(ICASAN.EQ.'MECI')THEN
        P=0.5
        AN=REAL(N)
        CALL SORT(Y,N,Y)
        DO 2010 I=1,8
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
!
!     ADD A FUDGE FACTOR SO THAT CONFIDENCE LEVEL WILL
!     BE PRINTED CORRECTLY TO 3 DECIMAL PLACES.
!
      CONF(1)=50.0001
      CONF(2)=75.0001
      CONF(3)=90.0001
      CONF(4)=95.0001
      CONF(5)=99.0001
      CONF(6)=99.9001
      CONF(7)=99.9901
      CONF(8)=99.9991
!
!               ****************************
!               **  STEP 7--              **
!               **  WRITE EVERYTHING OUT  **
!               ****************************
!
      ISTEPN='7'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'QUC2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IPRINT.EQ.'OFF')GO TO 9000
!
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
      IF(NREPL.GT.0)THEN
        NRESP=1
        DO 4101 I=1,NREPL
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
!
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
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
!
      NUMROW=ICNT
      DO 4210 I=1,NUMROW
        NTOT(I)=15
 4210 CONTINUE
!
      IFRST=.TRUE.
      ILAST=.TRUE.
!
      ISTEPN='5A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'QUC2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                  AVALUE,IDIGIT,   &
                  NTOT,NUMROW,   &
                  ICAPSW,ICAPTY,ILAST,IFRST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      ISTEPN='5B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'CNF2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPDT11(CONF,T,TSDM,ALOWER,AUPPER,   &
                  ICASAN,ICAPSW,ICAPTY,NUMDIG,   &
                  ISUBRO,IBUGA3,IERROR)
!
      IF(ICASAN.EQ.'MECI')THEN
        ICASA2='QUC2'
        CALL DPDT11(CONF,T,TSDM,ALOWE2,AUPPE2,   &
                    ICASA2,ICAPSW,ICAPTY,NUMDIG,   &
                    ISUBRO,IBUGA3,IERROR)
       ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPQUC2
      SUBROUTINE DPQUTE(TEMP1,TEMP2,MAXNXT,   &
                        ICAPSW,IFORSW,IMULT,   &
                        IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--CARRY OUT QUADE TEST NON-PARAMETRIC TWO-WAY ANOVA
!     EXAMPLE--QUADE TEST Y X1 X2
!     REFERENCE--CONOVER (1999), "PRACTICAL NONPARAMETRIC STATISTICS",
!                THIRD EDITION, WILEY, PP. 373-380.
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
!     ORIGINAL VERSION--JULY      2011.
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
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
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
!
!---------------------------------------------------------------------
!
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZD.INC'
!
      DIMENSION XTEMP2(MAXOBV)
      DIMENSION DBLOCK(MAXOBV)
      DIMENSION DTREAT(MAXOBV)
      DIMENSION RJ(MAXOBV)
      DIMENSION QRANK(MAXOBV)
      DOUBLE PRECISION YRANK(MAXOBV)
!
      EQUIVALENCE(GARBAG(IGARB1),XTEMP2(1))
      EQUIVALENCE(GARBAG(IGARB2),DBLOCK(1))
      EQUIVALENCE(GARBAG(IGARB3),DTREAT(1))
      EQUIVALENCE(GARBAG(IGARB4),RJ(1))
      EQUIVALENCE(GARBAG(IGARB5),QRANK(1))
      EQUIVALENCE(DGARBG(IDGAR1),YRANK(1))
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPQU'
      ISUBN2='TE  '
!
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
      IFOUND='YES'
      IERROR='NO'
!
!               ******************************************
!               **  TREAT THE QUADE TEST CASE           **
!               ******************************************
!
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
!
!               *********************************
!               **  STEP 1--                   **
!               **  EXTRACT THE VARIABLE LIST  **
!               *********************************
!
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'QUTE')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
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
          DO 185 I=1,NUMVAR
            WRITE(ICOUT,187)I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),   &
                            ICOLR(I)
  187       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',   &
                   'ICOLR(I) = ',I8,2X,A4,A4,2X,3I8)
            CALL DPWRST('XXX','BUG ')
  185     CONTINUE
        ENDIF
      ENDIF
!
!               **********************************
!               **  STEP 3--                    **
!               **  CARRY OUT THE QUADE TEST    **
!               **********************************
!
      ISTEPN='3'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'QUTE')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!               *****************************************
!               **  STEP 3A--                          **
!               **  CASE 1: THREE RESPONSE VARIABLES   **
!               **          NO MATRIX, NO MULTIPLE     **
!               *****************************************
!
      IF(IMULT.EQ.'OFF')THEN
        ISTEPN='3A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'QUTE')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        ICOL=1
        NUMVA2=3
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                    INAME,IVARN1,IVARN2,IVARTY,   &
                    ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                    MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                    MAXCP4,MAXCP5,MAXCP6,   &
                    V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                    Y,X,XTEMP2,NS1,NS1,NS1,ICASE,   &
                    IBUGA3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
        IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'QUTE')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5211)
 5211     FORMAT('***** FROM DPQUTE, AS WE ARE ABOUT TO CALL DPQUT2--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5212)NS1
 5212     FORMAT('NS1 = ',I8)
          CALL DPWRST('XXX','BUG ')
          DO 5215 I=1,NS1
            WRITE(ICOUT,5216)I,Y(I),X(I),XTEMP2(I)
 5216       FORMAT('I,Y(I),X(I),XTEMP2(I) = ',I8,3G15.7)
            CALL DPWRST('XXX','BUG ')
 5215     CONTINUE
        ENDIF
!
        CALL DPQUT2(Y,X,XTEMP2,NS1,IVARN1,IVARN2,   &
                    DBLOCK,DTREAT,YRANK,RJ,QRANK,   &
                    TEMP1,TEMP2,MAXNXT,   &
                    STATVA,STATCD,PVAL,   &
                    CUT0,CUT50,CUT75,CUT90,CUT95,CUT99,CUT999,   &
                    ICAPSW,ICAPTY,IFORSW,IMULT,   &
                    IBUGA3,ISUBRO,IERROR)
!
!               ***************************************
!               **  STEP 61--                        **
!               **  UPDATE INTERNAL DATAPLOT TABLES  **
!               ***************************************
!
        ISTEPN='61'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'QUTE')   &
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
        IFLAGU='ON'
        IFRST=.TRUE.
        ILAST=.TRUE.
        CALL DPFRT5(STATVA,STATCD,PVAL,   &
                    CUT0,CUT50,CUT75,CUT90,CUT95,   &
                    CUT975,CUT99,CUT999,   &
                    IFLAGU,IFRST,ILAST,   &
                    IBUGA2,IBUGA3,ISUBRO,IERROR)
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPQUTE
      SUBROUTINE DPQUT2(Y,BLOCK,TREAT,N,IVARID,IVARI2,   &
                        DBLOCK,DTREAT,YRANK,RJ,QRANK,   &
                        TEMP1,TEMP2,MAXNXT,   &
                        STATVA,STATCD,PVAL,   &
                        CUT0,CUT50,CUT75,CUT90,CUT95,CUT99,CUT999,   &
                        ICAPSW,ICAPTY,IFORSW,IMULT,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS ROUTINE CARRIES OUT QUADE'S TEST
!              NON-PARAMETRIC TWO-WAY ANOVA
!     EXAMPLE--QUADE TEST Y BLOCK TREAT
!     REFERENCE--CONOVER (1999), "PRACTICAL NONPARAMETRIC STATISTICS",
!                THIRD EDITION, WILEY, PP. 373-380.
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
!     ORIGINAL VERSION--JULY      2011.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 IMULT
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
      CHARACTER*4 IVARID(*)
      CHARACTER*4 IVARI2(*)
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IOP
      CHARACTER*3 IATEMP
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION BLOCK(*)
      DIMENSION TREAT(*)
      DIMENSION RJ(*)
      DIMENSION QRANK(*)
      DIMENSION DBLOCK(*)
      DIMENSION DTREAT(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
!
      DOUBLE PRECISION YRANK(*)
!
      PARAMETER (NUMALP=8)
      REAL ALPHA(NUMALP)
!
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
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      DATA ALPHA/   &
       0.0, 50.0, 75.0, 90.0, 95.0, 97.5, 99.0, 99.9/
!
      ISUBN1='DPFR'
      ISUBN2='I2  '
!
      IERROR='NO'
      IWRITE='OFF'
!
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
        DO 56 I=1,N
          WRITE(ICOUT,57)I,Y(I),BLOCK(I),TREAT(I)
   57     FORMAT('I,Y(I),BLOCK(I),TREAT(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
!
      MAXNX2=MAXNXT
      CALL DPQUT3(Y,BLOCK,TREAT,N,   &
                  DBLOCK,DTREAT,RJ,TEMP1,TEMP2,QRANK,   &
                  YRANK,   &
                  MAXNXT,MAXNX2,   &
                  STATVA,STATCD,PVAL,   &
                  NBLOCK,NTREAT,NUMDF1,NUMDF2,   &
                  T1,T2,A1,C1,SSTR,SSTO,   &
                  IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      CUT0=0.0
      CALL FPPF(.50,NUMDF1,NUMDF2,CUT50)
      CALL FPPF(.75,NUMDF1,NUMDF2,CUT75)
      CALL FPPF(.90,NUMDF1,NUMDF2,CUT90)
      CALL FPPF(.95,NUMDF1,NUMDF2,CUT95)
      CALL FPPF(.975,NUMDF1,NUMDF2,CUT975)
      CALL FPPF(.99,NUMDF1,NUMDF2,CUT99)
      CALL FPPF(.999,NUMDF1,NUMDF2,CUT999)
!
      ANB=REAL(NBLOCK)
      AK=REAL(NTREAT)
!
      IDF=(NBLOCK-1)*(NTREAT-1)
      CALL TPPF(0.95,REAL(IDF),T95)
      CALL TPPF(0.975,REAL(IDF),T975)
      CALL TPPF(0.995,REAL(IDF),T995)
      TERM1=2.0*ANB*(SSTO - SSTR)/REAL(IDF)
      CONTRA=SQRT(TERM1)
      CONTR1=T95*CONTRA
      CONTR2=T975*CONTRA
      CONTR3=T995*CONTRA
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
      IF(IERROR.EQ.'YES')GO TO 9000
!
      WRITE(IOUNI1,2405)
 2405 FORMAT(4X,'RESPONSE',13X,'RANK',11X,'BLOCK',8X,'TREATMENT')
      DO 2410 I=1,N
        WRITE(IOUNI1,2411)Y(I),YRANK(I),BLOCK(I),TREAT(I)
 2411   FORMAT(1X,E15.7,F15.2,F15.2,F15.2)
 2410 CONTINUE
!
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
!
      DO 2430 I=1,NTREAT
        DO 2439 J=1,NTREAT
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
!
      IOP='CLOS'
      CALL DPAUFI(IOP,IFLG1,IFLG2,IFLG3,IFLG4,IFLG5,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,   &
                  IBUGA3,ISUBRO,IERROR)
!
!               *****************************
!               **   STEP 42-              **
!               **   WRITE OUT THE TABLE   **
!               *****************************
!
      ISTEPN='42'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'QUT2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!               ******************************
!               **   STEP 43--              **
!               **   WRITE OUT EVERYTHING   **
!               **   FOR QUADE TEST      **
!               ******************************
!
      ISTEPN='43'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'QUT2')   &
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
      ITITLE='Quade Two Factor Test'
      NCTITL=21
      ITITLZ=' '
      NCTITZ=0
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
      IF(IMULT.EQ.'OFF')THEN
!
        ICNT=ICNT+1
        ITEXT(ICNT)='First Group-ID Variable: '
        WRITE(ITEXT(ICNT)(26:29),'(A4)')IVARID(2)(1:4)
        WRITE(ITEXT(ICNT)(30:33),'(A4)')IVARI2(2)(1:4)
        NCTEXT(ICNT)=33
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
!
        ICNT=ICNT+1
        ITEXT(ICNT)='Second Group-ID Variable: '
        WRITE(ITEXT(ICNT)(27:30),'(A4)')IVARID(3)(1:4)
        WRITE(ITEXT(ICNT)(31:34),'(A4)')IVARI2(3)(1:4)
        NCTEXT(ICNT)=34
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
!
      ELSE
      ENDIF
!
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
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
!
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
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
!
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
!
      NUMROW=ICNT
      DO 4210 I=1,NUMROW
        NTOT(I)=15
 4210 CONTINUE
!
      IFRST=.TRUE.
      ILAST=.TRUE.
!
      ISTEPN='42A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'QUT2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                  AVALUE,IDIGIT,   &
                  NTOT,NUMROW,   &
                  ICAPSW,ICAPTY,ILAST,IFRST,   &
                  ISUBRO,IBUGA3,IERROR)
!
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
!
      NMAX=0
      DO 4221 I=1,NUMCOL
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
      DO 4223 I=1,NUMROW
        DO 4225 J=1,NUMCOL
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
!
      IWHTML(1)=150
      IWHTML(2)=50
      IWHTML(3)=150
      IWRTF(1)=2000
      IWRTF(2)=IWRTF(1)+500
      IWRTF(3)=IWRTF(2)+2000
      IFRST=.TRUE.
      ILAST=.TRUE.
!
      ISTEPN='42C'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'QUT2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPDTA4(ITITL9,NCTIT9,   &
                  ITITLE,NCTITL,ITITL2,NCTIT2,   &
                  MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                  IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,   &
                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                  ICAPSW,ICAPTY,IFRST,ILAST,   &
                  ISUBRO,IBUGA3,IERROR)
!
      CDF1=CUT90
      CDF2=CUT95
      CDF3=CUT975
      CDF4=CUT99
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
      DO 4321 I=1,NUMCOL
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
      DO 4323 I=1,NUMROW
        DO 4325 J=1,NUMCOL
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
      ISTEPN='42E'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'KRU2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DPDTA4(ITITL9,NCTIT9,   &
                  ITITLE,NCTITL,ITITL2,NCTIT2,   &
                  MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                  IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,NUMROW,   &
                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                  ICAPSW,ICAPTY,IFRST,ILAST,   &
                  ISUBRO,IBUGA3,IERROR)
!
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPQUT2
      SUBROUTINE DPQUT3(Y,BLOCK,TREAT,N,   &
                        DBLOCK,DTREAT,RJ,TEMP1,TEMP2,QRANK,   &
                        YRANK,   &
                        MAXNXT,MAXNX2,   &
                        STATVA,STATCD,PVAL,   &
                        NBLOCK,NTREAT,NUMDF1,NUMDF2,   &
                        T1,T2,A1,C1,SSTR,SSTO,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS ROUTINE CARRIES OUT QUADE'S TEST
!              NON-PARAMETRIC TWO-WAY ANOVA
!     EXAMPLE--QUADE TEST Y BLOCK TREAT
!     REFERENCE--CONOVER (1999), "PRACTICAL NONPARAMETRIC STATISTICS",
!                THIRD EDITION, WILEY, PP. 373-380.
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
!     ORIGINAL VERSION--JULY      2011.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      DOUBLE PRECISION DA2
      DOUBLE PRECISION DB
      DOUBLE PRECISION SJ
!
!---------------------------------------------------------------------
!
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
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPQU'
      ISUBN2='T3  '
      IERROR='NO'
      IWRITE='OFF'
!
      STATVA=CPUMIN
      STATCD=CPUMIN
      PVAL=CPUMIN
!
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
        DO 56 I=1,N
          WRITE(ICOUT,57)I,Y(I),BLOCK(I),TREAT(I)
   57     FORMAT('I,Y(I),BLOCK(I),TREAT(I) = ',I8,3G15.7)
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'QUT3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      HOLD=Y(1)
      DO 1135 I=2,N
      IF(Y(I).NE.HOLD)GO TO 1139
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
      GO TO 9000
 1139 CONTINUE
!
      HOLD=BLOCK(1)
      DO 1235 I=2,N
      IF(BLOCK(I).NE.HOLD)GO TO 1239
 1235 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1131)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1231)HOLD
 1231 FORMAT('      THE FIRST FACTOR VARIABLE HAS ALL ELEMENTS = ',   &
             G15.7)
      CALL DPWRST('XXX','WRIT')
      IERROR='YES'
      GO TO 9000
 1239 CONTINUE
!
      HOLD=TREAT(1)
      DO 1335 I=2,N
      IF(TREAT(I).NE.HOLD)GO TO 1339
 1335 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1131)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1331)HOLD
 1331 FORMAT('      THE SECOND FACTOR VARIABLE HAS ALL ELEMENTS = ',   &
             G15.7)
      CALL DPWRST('XXX','WRIT')
      GO TO 9000
 1339 CONTINUE
!
!               ******************************
!               **  STEP 2--                **
!               **  CARRY OUT CALCULATIONS  **
!               **  FOR QUADE TEST          **
!               ******************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'QUT3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!  STEP 2A: COMPUTE NUMBER OF DISTINCT BLOCKS AND TREATMENTS
!
      CALL DISTIN(BLOCK,N,IWRITE,DBLOCK,NBLOCK,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      IF(NBLOCK.GT.MAXNX2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1131)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1232)NBLOCK,MAXNX2
 1232     FORMAT('      THE NUMBER OF BLOCKS (',I8,') IS GREATER ',   &
                 'THAN',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
      ENDIF
      CALL DISTIN(TREAT,N,IWRITE,DTREAT,NTREAT,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      IF(NTREAT.GT.MAXNX2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1131)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1237)NTREAT,MAXNX2
 1237     FORMAT('      THE NUMBER OF TREATMENTS (',I8,') IS GREATER ',   &
                 'THAN ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
      ENDIF
!
!  STEP 2B: COMPUTE THE RANGES WITHIN EACH BLOCK
!
      ISTEPN='2B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'QUT3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 2010 I=1,N
        YRANK(I)=-1.0D0
 2010 CONTINUE
!
      DO 2110 I=1,NBLOCK
        HOLD=DBLOCK(I)
        ICOUNT=0
        YMIN=CPUMAX
        YMAX=CPUMIN
        DO 2120 J=1,N
          IF(BLOCK(J).EQ.HOLD)THEN
            ICOUNT=ICOUNT+1
            RJ(ICOUNT)=Y(J)
            IF(RJ(ICOUNT).LT.YMIN)YMIN=RJ(ICOUNT)
            IF(RJ(ICOUNT).GT.YMAX)YMAX=RJ(ICOUNT)
          ENDIF
 2120   CONTINUE
        QRANK(I)=YMAX - YMIN
        CALL RANK(RJ,ICOUNT,IWRITE,TEMP1,TEMP2,MAXNX2,   &
                  IBUGA3,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
        ICOUNT=0
        DO 2130 J=1,N
          IF(BLOCK(J).EQ.HOLD)THEN
            ICOUNT=ICOUNT+1
            YRANK(J)=DBLE(TEMP1(ICOUNT))
          ENDIF
 2130   CONTINUE
 2110 CONTINUE
      CALL RANK(QRANK,NBLOCK,IWRITE,TEMP1,TEMP2,MAXNX2,IBUGA3,IERROR)
      DO 2135 I=1,NBLOCK
        QRANK(I)=TEMP1(I)
 2135 CONTINUE
!
      AFACT=REAL(NTREAT+1)/2.0
      DA2=0.0D0
      DO 2140 I=1,NBLOCK
        HOLD=DBLOCK(I)
        ICOUNT=0
        SJ=0.0D0
        DO 2150 J=1,N
          IF(BLOCK(J).EQ.HOLD)THEN
            SIJ=QRANK(I)*(YRANK(J) - AFACT)
            DA2=DA2 + DBLE(SIJ)**2
          ENDIF
 2150   CONTINUE
 2140 CONTINUE
!
      DB=0.0D0
      DO 2160 I=1,NTREAT
        HOLD=DTREAT(I)
        ICOUNT=0
        SJ=0.0D0
        DO 2170 J=1,N
          IF(TREAT(J).EQ.HOLD)THEN
            ITEMP=INT(BLOCK(J)+0.1)
            SIJ=QRANK(ITEMP)*(YRANK(J) - AFACT)
            SJ=SJ + DBLE(SIJ)
          ENDIF
 2170   CONTINUE
        DB=DB + SJ**2
 2160 CONTINUE
      DB=DB/DBLE(NBLOCK)
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'QUT3')THEN
        WRITE(ICOUT,2161)DA2,DB,AFACT
 2161   FORMAT('DA2,DB,AFACT = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        DO 2180 I=1,N
          WRITE(ICOUT,2182)I,Y(I),YRANK(I)
 2182     FORMAT('I,Y(I),YRANK(I) = ',I8,G15.7,F12.2)
          CALL DPWRST('XXX','BUG ')
 2180   CONTINUE
        DO 2187 I=1,NBLOCK
          WRITE(ICOUT,2188)I,QRANK(I)
 2188     FORMAT('I,QRANK(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
 2187   CONTINUE
      ENDIF
!
!  STEP 2C: NOW COMPUTE RANK SUMS FOR EACH TREATMENT
!
      ISTEPN='2C'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'QUT3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!  STEP 4: NOW COMPUTE VARIOUS QUANTITIES
!
      SSTO=REAL(DA2)
      SSTR=REAL(DB)
!
      IF(DA2.EQ.DB)THEN
      ELSE
        STATVA=(DBLE(NBLOCK) -1)*DB/(DA2 - DB)
        NUMDF1=NTREAT-1
        NUMDF2=(NBLOCK-1)*(NTREAT-1)
        CALL FCDF(STATVA,NUMDF1,NUMDF2,STATCD)
        PVAL=1.0 - STATCD
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPQUT3
      SUBROUTINE DPRAND(ICASRA,ISEED,ILOCNU,NUMSHA,   &
                        SHAPE1,SHAPE2,SHAPE3,SHAPE4,   &
                        SHAPE5,SHAPE6,SHAPE7,   &
                        IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--GENERATE RANDOM NUMBERS
!              FROM ONE OF THE FOLLOWING DISTRIBUTIONS--
!              1 ) UNIFORM
!              2 ) NORMAL
!              3 ) LOGISTIC
!              4 ) DOUBLE EXPONENTIAL
!              5 ) CAUCHY
!              6 ) TUKEY LAMBDA
!              7 ) LOGNORMAL
!              8 ) HALFNORMAL
!              9 ) T
!              10) CHI-SQUARED
!              11) F
!              12) EXPONENTIAL
!              13) GAMMA
!              14) BETA
!              15) WEIBULL
!              16) EXTREME VALUE TYPE 1
!              17) EXTREME VALUE TYPE 2
!              18) PARETO
!              19) BINOMIAL
!              20) GEOMETRIC
!              21) POISSON
!              22) NEGATIVE BINOMIAL
!              23) SEMI-CIRCULAR
!              24) TRIANGULAR
!              25) INVERSE GAUSSIAN    MAY 1990
!              26) WALD    MAY 1990
!              27) RECIPROCAL INVERSE GAUSSIAN    MAY 1990
!              28) FATIGUE LIFE    MAY 1990
!              29) GENERALIZED PARETO      DECEMBER   1993
!              30) POWER FUNCTION          APRIL      1995
!              31) HYPERGEOMETRIC          AUGUST     1995
!              32) NON-CENTRAL CHI-SQUARE  AUGUST     1995
!              33) NON-CENTRAL F           AUGUST     1995
!              34) DOUBLY NON-CENTRAL F    AUGUST     1995
!              35) FOLDED NORMAL           OCTOBER    1995
!              36) HALF-CAUCHY             OCTOBER    1995
!              37) NORMAL MIXTURE          MAY        1998
!              38) POWER LAW               JUNE       1998
!              39) GENERALIZED TUKEY-LAMBDA AUGUST    2001
!              40) INVERTED WEIBULL        SEPTEMBER  2001
!              41) DOUBLE WEIBULL          OCTOBER    2001
!              42) DOUBLE GAMMA            OCTOBER    2001
!              43) LOG    GAMMA            OCTOBER    2001
!              44) INVERTED GAMMA          OCTOBER    2001
!              45) COSINE                  OCTOBER    2001
!              46) ANGLIT                  OCTOBER    2001
!              47) HYPERBOLIC SECANT       OCTOBER    2001
!              48) ARCSIN                  OCTOBER    2001
!              49) LOG DOUBLE EXPONENTIAL  OCTOBER    2001
!              50) GENERALIZED EXTREM VALU OCTOBER    2001
!              51) EXPONENTIATED WEIBULL   OCTOBER    2001
!              52) GOMPERTZ                OCTOBER    2001
!              53) HALF-LOGISTIC           OCTOBER    2001
!              54) POWER EXPONENTIAL       OCTOBER    2001
!              55) ALPHA                   OCTOBER    2001
!              56) BRADFORD                OCTOBER    2001
!              57) RECIPROCAL              OCTOBER    2001
!              58) JOHNSON SB              OCTOBER    2001
!              59) JOHNSON SU              OCTOBER    2001
!              60) POWER NORMAL            OCTOBER    2001
!              61) LOG-LOGISTIC            OCTOBER    2001
!              62) GEOMETRIC EXTR EXPO     NOVEMBER   2001
!              63) POWER LOGNORMAL         NOVEMBER   2001
!              64) BETA-BINOMIAL           DECEMBER   2001
!              65) TWO-SIDED POWER         MAY        2002
!              66) BIWEIBULL               MAY        2002
!              66) LOGARITHMIC SERIES      AUGUST     2002
!              67) G-AND-H                 JANUARY    2003
!              68) SLASH                   JANUARY    2003
!              69) LANDAU                  APRIL      2003
!              70) INVERTED BETA           MAY        2003
!              71) ERROR (=SUBBOTIN        MAY        2003
!                         =EXPONENTIAL POWER
!                         =GENERAL ERROR)
!              72) TRAPEZOID               JUNE       2003
!              73) VON MISES               JUNE       2003
!              74) PARETO SECOND KIND      JUNE       2003
!              75) WRAPPED CAUCHY          JUNE       2003
!              76) GENERALIZED TRAPEZOID   JUNE       2003
!              77) TRUNCATED NORMAL        JULY       2003
!              78) CHI                     JULY       2003
!              79) FOLDED CAUCHY           JULY       2003
!              80) MIELKE'S BETA-KAPPA     JULY       2003
!              81) GENERALIZED EXPONENTIAL JULY       2003
!              82) TRUNCATED   EXPONENTIAL JULY       2003
!              83) GENERALIZED GAMMA       SEPTEMBER  2003
!              84) FOLDED T                NOVEMBER   2003
!              85) SKEWED NORMAL           NOVEMBER   2003
!              86) SKEWED T                NOVEMBER   2003
!              87) ZIPF                    NOVEMBER   2003
!                  (RENAME AS ZETA)        MAY        2006
!              88) GOMPERTZ-MAKEHAM        DECEMBER   2003
!              89) GENERALIZED INVERSE GAUSSIAN   DECEMBER   2003
!                  (NOT ACTIVATED YET)
!              90) LOG SKEWED NORMAL       MARCH      2004
!              91) LOG SKEWED T            MARCH      2004
!              92) NON-CENTRAL T           MARCH      2004
!              93) DOUBLY NON-CENTRAL T    MARCH      2004
!              94) GENERALIZED HALF-LOGISTIC  MARCH   2004
!              95) GENERALIZED LOGISTIC    MARCH      2004
!              96) POLYA                   MARCH      2004
!              97) HERMITE                 APRIL      2004
!              98) YULE                    APRIL      2004
!              99) WARING                  APRIL      2004
!             100) GENERALIZED WARING      APRIL      2004
!             101) NON-CENTRAL BETA        MAY        2004
!             102) DOUBLY NON-CENTRAL BETA MAY        2004
!             103) SKEW DOUBLE EXPONENTIAL JUNE       2004
!             104) ASYMMETRIC DOUBLE EXPONENTIAL   JUNE  2004
!             105) MAXWELL                 JUNE       2004
!             106) RAYLEIGH                JUNE       2004
!             107) MCLEISH                 AUGUST     2004
!             108) BESSEL I-FUNCTION       AUGUST     2004
!             109) BESSEL K-FUNCTION       AUGUST     2004 (NOT WORK)
!             110) GENERALIZED MCLEISH     SEPTEMBER  2004
!             111) HYPERBOLIC              SEPTEMBER  2004 (NOT WORK)
!             112) GENERALIZED LOGISTIC TYPE 5   FEBRUARY  2006
!             113) WAKEBY                  FEBRUARY  2006
!             114) BETA NORMAL             MARCH     2006
!             115) GENERALIZED LOGISTIC TYPE 2 MARCH 2006
!             116) GENERALIZED LOGISTIC TYPE 3 MARCH 2006
!             117) GENERALIZED LOGISTIC TYPE 4 MARCH 2006
!             118) ASYMMETRIC LOG DOUBLE EXPONENTIAL  MARCH  2006
!             119) BETA GEOMETRIC          MAY    2006
!             120) BOREL TANNER            MAY    2006
!             121) LAGRANGE POISSON        JUNE   2006
!             122) LEADS IN COIN TOSSING   JUNE   2006
!                  (DISCRETE ARCSINE)
!             123) MATCHING                JUNE   2006
!             124) CLASSICAL OCCUPANCY     JUNE   2006 (NOT ACTIVE)
!             125) LOG BETA                JUNE   2006
!             126) POLYA AEPPLI            JUNE   2006
!             127) LOST GAMES              JUNE   2006
!             128) NEYMAN TYPE A           JUNE   2006 (NOT ACTIVE)
!             129) DXG                     JUNE   2006 (NOT ACTIVE)
!             130) GENERALIZED LOGARITHMIC SERIES JUNE   2006
!             131) GENERALIZED NEGATIVE BINOMIAL  JULY   2006
!             132) GEETA                   JULY   2006
!             133) QUASI BINOMIAL TYPE I   JULY   2006
!             134) CONSUL                  AUGUST 2006
!             135) DISCRETE WEIBULL        NOVEMBER  2006
!             136) GENERALIZED LOST GAMES  NOVEMBER  2006
!             137) TRUNCATED GENERALIZED
!                  NEGATIVE BINOMIAL       JANUARY 2006
!             138) KATZ                    JANUARY   2007
!             139) TOPP AND LEONE          FEBRUARY 2007
!             140) GENERALIZED TOPP AND LEONE   FEBRUARY 2007
!             141) REFLECTED GENERALIZED TOPP AND LEONE  FEBRUARY 2007
!             142) LAGRANGE KATZ           FEBRUARY 2007 (NOT ACTIVE)
!             143) SLOPE                   SEPTEMBER 2007
!             144) OGIVE                   SEPTEMBER 2007
!             145) TWO-SIDED SLOPE         SEPTEMBER 2007
!             146) TWO-SIDED OGIVE         SEPTEMBER 2007
!             147) UNEVEN TWO-SIDED POWER  OCTOBER 2007
!             148) DOUBLY UNIFORM PARETO   OCTOBER 2007
!             149) BURR TYPE 1 (= UNIFORM) OCTOBER 2007
!             150) BURR TYPE 2             OCTOBER 2007
!             151) BURR TYPE 3             OCTOBER 2007
!             152) BURR TYPE 4             OCTOBER 2007
!             153) BURR TYPE 5             OCTOBER 2007
!             154) BURR TYPE 6             OCTOBER 2007
!             155) BURR TYPE 7             OCTOBER 2007
!             156) BURR TYPE 8             OCTOBER 2007
!             157) BURR TYPE 9             OCTOBER 2007
!             158) BURR TYPE 10            OCTOBER 2007
!             159) BURR TYPE 11            OCTOBER 2007
!             160) BURR TYPE 12            OCTOBER 2007
!             160) KUMARASWAMY             OCTOBER 2007
!             161) REFLECTED POWER         DECEMBER 2007
!             162) MUTH                    JANUARY 2008
!             163) LOGISTIC-EXPONENTIAL    FEBRUARY 2008
!             164) TRUNCATED PARETO        MARCH    2008
!             165) BRITTLE FRACTURE        MARCH    2008
!             166) 3-PARAMETER LOGISTIC-EXPONENTIAL  MARCH 2008
!             167) BOOTSTRAP INDEX         DECEMBER 1988
!             168) RANDOM PERMUTATION      DECEMBER 1988
!             169) RANDOM SUBSET           APRIL    2008
!             170) RANDOM K-SET OF N-SET   APRIL    2008
!             171) RANDOM COMPOSITION      APRIL    2008
!             172) KAPPA                   MAY      2008
!             173) PEARSON TYPE 3          MAY      2008
!             174) RANDOM PARTITION        JUNE     2008
!             175) RANDOM EQUIVALENCE RELA JUNE     2008
!             176) RANDOM YOUNG TABLEAUX   JULY     2008
!             177) END EFFECTS WEIBULL     JULY     2010
!             178) BRITTLE FIBER WEIBULL   AUGUST   2010
!             179) ARCTANGENT              JANUARY  2011
!             180) SINE                    MARCH    2013
!             181) EXCLUSION ZONE UNIFORM  MARCH    2013
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
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--APRIL     1978.
!     UPDATED         --MAY       1978.
!     UPDATED         --JUNE      1978.
!     UPDATED         --MAY       1978.
!     UPDATED         --NOVEMBER  1978.
!     UPDATED         --JUNE      1981.
!     UPDATED         --SEPTEMBER 1981.
!     UPDATED         --OCTOBER   1981.
!     UPDATED         --MARCH     1982.
!     UPDATED         --MAY       1982.
!     UPDATED         --DECEMBER  1988. DISCRETE UNIFORM
!     UPDATED         --DECEMBER  1988. BOOTSTRAP INDEX
!     UPDATED         --DECEMBER  1988. RANDOM PERMUTATION
!     UPDATED         --JANUARY   1989. JACKNIFE INDEX
!     UPDATED         --MAY       1993. MINMAX FOR EV1/EV2/WEIB DIST.
!     UPDATED         --OCTOBER   1993. JACKNIFE INDEX TO DPMATC
!     UPDATED         --DECEMBER  1993. GENERALIZED PARETO
!     UPDATED         --MARCH     1994. DPCOS2.INC
!     UPDATED         --APRIL     1995. POWER FUNCTION
!     UPDATED         --AUGUST    1995. HYPERGEOMETRIC, NON-CENTRAL
!                                       CHI-SQUARE, SINGLY AND DOUBLY
!                                       NON-CENTRAL F
!     UPDATED         --MAY       1998. NORMAL MIXTURE
!     UPDATED         --JUNE      1998. POWER LAW
!     UPDATED         --AUGUST    2001. GENERALIZED LAMBDA
!     UPDATED         --SEPTEMBER 2001. INVERTED WEIBULL
!     UPDATED         --OCTOBER   2001. DOUBLE WEIBULL
!     UPDATED         --OCTOBER   2001. DOUBLE GAMMA
!     UPDATED         --OCTOBER   2001. LOG GAMMA
!     UPDATED         --OCTOBER   2001. INVERTED GAMMA
!     UPDATED         --OCTOBER   2001. COSINE
!     UPDATED         --OCTOBER   2001. ANGLIT
!     UPDATED         --OCTOBER   2001. HYPERBOLIC SECANT
!     UPDATED         --OCTOBER   2001. ARCSIN
!     UPDATED         --OCTOBER   2001. LOG DOUBLE EXPONENTIAL
!     UPDATED         --OCTOBER   2001. GENERALIZED EXTREME VALUE
!     UPDATED         --OCTOBER   2001. EXPONENTIATED WEIBULL
!     UPDATED         --OCTOBER   2001. GOMPERTZ
!     UPDATED         --OCTOBER   2001. HALF-LOGISTIC
!     UPDATED         --OCTOBER   2001. POWER EXPONENTIAL
!     UPDATED         --OCTOBER   2001. ALPHA
!     UPDATED         --OCTOBER   2001. BRADFORD
!     UPDATED         --OCTOBER   2001. RECIPROCAL
!     UPDATED         --OCTOBER   2001. JOHNSON SU
!     UPDATED         --OCTOBER   2001. JOHNSON SB
!     UPDATED         --OCTOBER   2001. POWER NORMAL
!     UPDATED         --OCTOBER   2001. LOG-LOGISTIC
!     UPDATED         --NOVEMBER  2001. GEOMETRIC EXTREME EXPO
!     UPDATED         --NOVEMBER  2001. POWER LOGNORMAL
!     UPDATED         --DECEMBER  2001. BETA-BINOMIAL
!     UPDATED         --MAY       2002. TWO-SIDED POWER
!     UPDATED         --MAY       2002. BIWEIBULL
!     UPDATED         --AUGUST    2002. LOGARITHMIC SERIES
!     UPDATED         --JANUARY   2003. G-AND-H, SLASH
!     UPDATED         --APRIL     2003. ADD SHAPE PARAMETER FOR
!                                       LOGNORMAL
!     UPDATED         --APRIL     2003. LANDAU
!     UPDATED         --MAY       2003. INVERTED BETA
!     UPDATED         --MAY       2003. ERROR (=SUBBOTIN=EXPOENTIAL
!                                       POWER=GENERAL ERROR)
!     UPDATED         --JUNE      2003. TRAPEZOID, VON MISES,
!                                       PARETO SECOND KIND,
!                                       WRAPPED CAUCHY,
!                                       GENERALIZED TRAPEZOID
!     UPDATED         --JULY      2003. CHI, TRUNCATED NORMAL,
!                                       FOLDED CAUCHY,
!                                       MIELKE'S BETA-KAPPA,
!                                       GENERALIZED EXPONENTIAL,
!                                       TRUNCATED EXPONENTIAL
!     UPDATED         --SEPTEMBER 2003. GENERALIZED GAMMA
!     UPDATED         --NOVEMBER  2003. FOLDED T
!     UPDATED         --NOVEMBER  2003. SKEWED NORMAL
!     UPDATED         --NOVEMBER  2003. SKEWED T
!     UPDATED         --NOVEMBER  2003. ZIPF
!     UPDATED         --DECEMBER  2003. GOMPERTZ-MAKEHAM
!     UPDATED         --DECEMBER  2003. GENERALIZED INVERSE GAUSSIAN
!                                       (NOT IMPLEMENTED YET)
!     UPDATED         --MARCH     2004. LOG SKEWED NORMAL
!     UPDATED         --MARCH     2004. LOG SKEWED T
!     UPDATED         --MARCH     2004. ALTERNATE DEFINITION OF
!                                       GEOMETRIC
!     UPDATED         --MARCH     2004. NON-CENTRAL T
!     UPDATED         --MARCH     2004. DOUBLY NON-CENTRAL T
!     UPDATED         --MARCH     2004. GENERALIZED HALF-LOGISTIC
!     UPDATED         --MARCH     2004. GENERALIZED LOGISTIC
!     UPDATED         --MARCH     2004. POLYA
!     UPDATED         --APRIL     2004. HERMITE
!     UPDATED         --APRIL     2004. YULE
!     UPDATED         --APRIL     2004. WARING
!     UPDATED         --APRIL     2004. GENERALIZED WARING
!     UPDATED         --MAY       2004. NON-CENTRAL BETA
!     UPDATED         --MAY       2004. DOUBLY NON-CENTRAL BETA
!     UPDATED         --MAY       2004. REAL VALUES FOR CHI-SQUARE
!                                       RANDOM NUMBERS
!     UPDATED         --MAY       2004. NON-CENTRAL CHI-SQUARE AS
!                                       SEPARATE SUBROUTINE
!     UPDATED         --JUNE      2004. SKEW DOUBLE EXPONENTIAL
!     UPDATED         --JUNE      2004. ASYMMETRIC DOUBLE EXPONENTIAL
!     UPDATED         --JUNE      2004. ARGUMENT LIST TO GEPRAN
!     UPDATED         --JUNE      2004. MAXWELL, RAYLEIGH
!     UPDATED         --JULY      2004. ALTERNATE DEFINITIION FOR
!                                       GOMPERTZ-MAKEHAM
!     UPDATED         --OCTOBER   2004. FOR PARETO, TREAT A AS A
!                                       SHAPE PARAMETER
!     UPDATED         --JULY      2005. CALL LIST TO LGARAN AND SNRAN
!     UPDATED         --FEBRUARY  2006. GENERALIZED LOGISTIC TYPE 5
!     UPDATED         --FEBRUARY  2006. WAKEBY
!     UPDATED         --FEBRUARY  2006. ARGUMENT LIST TO GLDRAN
!     UPDATED         --MARCH     2006. BETA-NORMAL
!     UPDATED         --MARCH     2006. GENERALIZED LOGISTIC TYPE 2
!     UPDATED         --MARCH     2006. GENERALIZED LOGISTIC TYPE 3
!     UPDATED         --MARCH     2006. GENERALIZED LOGISTIC TYPE 4
!     UPDATED         --MARCH     2006. ASYMMETRIC DOUBLE EXPONENTIAL
!     UPDATED         --MAY       2006. BETA GEOMETRIC
!     UPDATED         --MAY       2006. RENAME ZIPF AS ZETA
!     UPDATED         --MAY       2006. BOREL-TANNER
!     UPDATED         --MAY       2006. BETA-NEGATIVE BINOMIAL AS
!                                       SYNOMYM FOR GENERALIZED
!                                       WARING
!     UPDATED         --JUNE      2006. LAGRANGE-POISSON
!     UPDATED         --JUNE      2006. LEADS IN COIN TOSSING
!     UPDATED         --JUNE      2006. MATCHING
!     UPDATED         --JUNE      2006. CLASSICAL OCCUPANCY
!     UPDATED         --JUNE      2006. LOG BETA
!     UPDATED         --JUNE      2006. GENERALIZED LOGARITHMIC
!                                       SERIES
!     UPDATED         --JULY      2006. GENERALIZED NEGATIVE
!                                       BINOMIAL
!     UPDATED         --JULY      2006. GEETA
!     UPDATED         --JULY      2006. QUASI BINOMIAL TYPE 1
!     UPDATED         --AUGUST    2006. CONSUL
!     UPDATED         --AUGUST    2006. LAGRANGE KATZ
!     UPDATED         --SEPTEMBER 2006. KATZ
!     UPDATED         --OCTOBER   2006. FRACTIONAL DEGREES OF
!                                       FREEDOM FOR T DISTRIBUTION
!     UPDATED         --NOVEMBER  2006. DISCRETE WEIBULL
!     UPDATED         --NOVEMBER  2006. GENERALIZED LOST GAMES
!     UPDATED         --FEBRUARY  2007. TOPP AND LEONE
!     UPDATED         --FEBRUARY  2007. GENERALIZED TOPP AND LEONE
!     UPDATED         --FEBRUARY  2007. REFLECTED GENERALIZED TOPP
!                                       AND LEONE
!     UPDATED         --SEPTEMBER 2007. SLOPE
!     UPDATED         --SEPTEMBER 2007. OGIVE
!     UPDATED         --SEPTEMBER 2007. TWO-SIDED SLOPE
!     UPDATED         --SEPTEMBER 2007. TWO-SIDED OGIVE
!     UPDATED         --OCTOBER   2007. BURR TYPE 1 (= UNIFORM)
!     UPDATED         --OCTOBER   2007. BURR TYPE 2
!     UPDATED         --OCTOBER   2007. BURR TYPE 3
!     UPDATED         --OCTOBER   2007. BURR TYPE 4
!     UPDATED         --OCTOBER   2007. BURR TYPE 5
!     UPDATED         --OCTOBER   2007. BURR TYPE 6
!     UPDATED         --OCTOBER   2007. BURR TYPE 7
!     UPDATED         --OCTOBER   2007. BURR TYPE 8
!     UPDATED         --OCTOBER   2007. BURR TYPE 9
!     UPDATED         --OCTOBER   2007. BURR TYPE 10
!     UPDATED         --OCTOBER   2007. BURR TYPE 11
!     UPDATED         --OCTOBER   2007. BURR TYPE 12
!     UPDATED         --OCTOBER   2007. DOUBLY PARETO UNIFORM
!     UPDATED         --OCTOBER   2007. KUMARASWAMY
!     UPDATED         --DECEMBER  2007. REFLECTED POWER
!     UPDATED         --JANUARY   2008. MUTH
!     UPDATED         --FEBRUARY  2008. LOGISTIC-EXPONENTIAL
!     UPDATED         --FEBRUARY  2008. TRUNCATED PARETO
!     UPDATED         --MARCH     2008. BRITTLE FRACTURE
!     UPDATED         --MARCH     2008. 3-PARAMETER LOGISTIC-EXPONENTIAL
!     UPDATED         --APRIL     2008. RANDOM SUBSET
!     UPDATED         --APRIL     2008. RANDOM K-SET OF N-SET
!     UPDATED         --APRIL     2008. RANDOM COMPOSITION
!     UPDATED         --MAY       2008. RENAME CALL FOR MIELKE'S
!                                       BETA-KAPPA, BETA PARAMETER IS
!                                       ACTUALLY A SCALE PARAMETER
!     UPDATED         --MAY       2008. KAPPA
!     UPDATED         --MAY       2008. PEARSON TYPE 3
!     UPDATED         --MAY       2008. RANDOM PARTITION
!     UPDATED         --JUNE      2008. RANDOM EQUIVALENCE RELATION
!     UPDATED         --JULY      2008. RANDOM YOUNG TABLEAUX
!     UPDATED         --JULY      2008. MODIFY GIG PARAMETERIZATION
!     UPDATED         --SEPTEMBER 2009. USE EXTPA1
!     UPDATED         --SEPTEMBER 2009. EXTRACT MOST OF THE CALLS
!                                       TO RANDOM NUMBER ROUTINES TO
!                                       "DPRAN2" TO ENABLE EASIER
!                                       CALLING BY OTHER ROUTINES
!                                       (E.G., THE BOOTSTRAP COMMAND)
!     UPDATED         --JULY      2010. END EFFECTS WEIBULL
!     UPDATED         --AUGUST    2010. BRITTLE FIBER WEIBULL
!     UPDATED         --JANUARY   2011. ARCTANGENT
!     UPDATED         --MARCH     2013. SINE
!     UPDATED         --MARCH     2013. EXCLUSION ZONE UNIFORM
!     UPDATED         --FEBRUARY  2021. CHECK FOR ERROR FROM DPSUBS OR
!                                       DPFOR
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICASRA
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGQ
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
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
!
      CHARACTER*60 IDIST
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOS2.INC'
      INCLUDE 'DPCOST.INC'
!
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
!
      COMMON/NIJWIL/NLAST,KLAST
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
!CCCC DATA EPS/0.000001/
!CCCC DATA ALAMLG/0.00001/
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPRA'
      ISUBN2='ND  '
      IFOUND='NO'
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
      NS2=0
      NRAN=0
      RANLOC=0.0
      RANSCA=1.0
!
!               ***********************************************
!               **  TREAT THE RANDOM NUMBER GENERATION CASE  **
!               **       1) FOR A FULL VARIABLE, OR          **
!               **       2) FOR PART OF A VARIABLE.          **
!               ***********************************************
!
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
!
!               **********************************
!               **  STEP 1--                    **
!               **  INITIALIZE SOME VARIABLES.  **
!               **********************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RAND')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NEWNAM='NO'
      NEWCOL='NO'
!
!               *******************************************************
!               **  STEP 2--                                         **
!               **  CHECK FOR THE PROPER NUMBER OF INPUT ARGUMENTS.  **
!               *******************************************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RAND')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      MINNA=3
      MAXNA=100
      CALL CHECKA(NUMARG,MINNA,MAXNA,IANS,IWIDTH,ISUBN1,ISUBN2,   &
      IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               ********************************************************
!               **  STEP 3--                                           *
!               **  EXAMINE THE LEFT-HAND SIDE--                       *
!               **  IS THE PARAMETER OR VARIABLE NAME TO LEFT OF =     *
!               **  SIGN ALREADY IN THE NAME LIST?                     *
!               **  NOTE THAT     ILEFT      IS THE NAME OF THE        *
!               **  VARIABLE ON THE LEFT.                              *
!               **  NOTE THAT     ILISTL    IS THE LINE IN THE TABLE   *
!               **  OF THE NAME ON THE LEFT.                           *
!               **  NOTE THAT     ICOLL    IS THE DATA COLUMN (1 TO 12)*
!               **  FOR THE NAME OF THE LEFT.                          *
!               ********************************************************
!
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RAND')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ILEFT=IHARG(1)
      ILEFT2=IHARG2(1)
      DO 310 I=1,NUMNAM
        I2=I
        IF(ILEFT.EQ.IHNAME(I).AND.ILEFT2.EQ.IHNAM2(I).AND.   &
           IUSE(I).EQ.'P')THEN
           ILISTL=I2
           GO TO 330
        ELSEIF(ILEFT.EQ.IHNAME(I).AND.ILEFT2.EQ.IHNAM2(I).AND.   &
           IUSE(I).EQ.'V')THEN
           ILISTL=I2
           ICOLL=IVALUE(ILISTL)
           NLEFT=IN(ILISTL)
           GO TO 390
        ELSEIF(ILEFT.EQ.IHNAME(I).AND.ILEFT2.EQ.IHNAM2(I))THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,321)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,210)ILEFT,ILEFT2
  210     FORMAT('      AN ATTEMPT WAS MADE TO USE ',2A4,' AS A ',   &
                 'VARIABLE')
          CALL DPWRST('XXX','BUG ')
          IF(IUSE(I).EQ.'F')THEN
            WRITE(ICOUT,211)
  211       FORMAT('      EVEN THOUGH IT ALREADY EXISTS AS A STRING.')
            CALL DPWRST('XXX','BUG ')
          ELSEIF(IUSE(I).EQ.'M')THEN
            WRITE(ICOUT,212)
  212       FORMAT('      EVEN THOUGH IT ALREADY EXISTS AS A MATRIX.')
            CALL DPWRST('XXX','BUG ')
          ELSE
            WRITE(ICOUT,213)IUSE(I)
  213       FORMAT('      EVEN THOUGH IT ALREADY EXISTS AS A ',A4,'.')
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IERROR='YES'
          GO TO 9000
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
  327   FORMAT('      THEN REDEFINE (REUSE) SOME OF THE ALREADY USED ',   &
               'NAMES.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
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
  343   FORMAT('      THE MAXIMUM ALLOWABLE ',I8,'.  SUGGESTED ',   &
               'ACTION--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,325)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,326)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,347)
  347   FORMAT('      THEN DELETE SOME OF THE ALREADY USED NAMES.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
  390 CONTINUE
!
!               *******************************************************
!               **  STEP 4--                                         **
!               **  CHECK THAT THE INPUT CASE (ICASRA)               **
!               **  IS ONE OF THE ALLOWABLE 100+ DISTRIBUTIONS       **
!               *******************************************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RAND')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!               *****************************************
!               **  STEP 6--                           **
!               **  CHECK TO SEE THE TYPE SUBCASE      **
!               **  (BASED ON THE QUALIFIER)           **
!               **    1) UNQUALIFIED (THAT IS, FULL);  **
!               **    2) SUBSET/EXCEPT; OR             **
!               **    3) FOR.                          **
!               *****************************************
!
      ISTEPN='6'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RAND')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     APRIL 2008: CHECK FOR "SUBSET" CONFLICT WITH "RANDOM SUBSET"
!                 CASE.
!
!     MAY 2008: RANDOM PARTITION AND RANDOM EQUIVALENCE CLASS
!               COMMANDS DO NOT USE THE TYPICAL
!               "FOR I = 1 1 N" CLAUSE.
!
!     JULY 2008: RANDOM YOUNG TABLEAUX USES SYNTAX:
!
!                LET N = <VALUE>
!                LET Y = RANDOM YOUNG TABLEAUX LAMBDA
!
!                WHERE LAMBDA IS AN ARRAY DEFINING THE PARTITION
!
      IF(ICASRA.EQ.'RANP' .OR. ICASRA.EQ.'RANE')GO TO 750
!
      IF(ICASRA.EQ.'RAYT')THEN
        IHRIGH=IHARG(6)
        IHRIG2=IHARG2(6)
        IHWUSE='V'
        MESSAG='YES'
        CALL CHECKN(IHRIGH,IHRIG2,IHWUSE,   &
                    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                    ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
        ICOLR=IVALUE(ILOCV)
        NLEFT=IN(ILOCV)
        J=0
        DO 701 I=1,NLEFT
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
        GO TO 750
      ENDIF
!
      ICASEQ='FULL'
      ILOCQ=NUMARG+1
      IF(NUMARG.LT.1)GO TO 670
      DO 610 J=1,NUMARG
        J1=J
        IF(ICASRA.NE.'SUBS')THEN
          IF(IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ')GO TO 620
        ELSE
          IF(IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  '.AND.   &
             IHARG(J+1).EQ.'SUBS'.AND.IHARG2(J+1).EQ.'ET  ')THEN
            J1=J+1
            GO TO 620
          ENDIF
        ENDIF
        IF(IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  ')GO TO 620
        IF(IHARG(J).EQ.'FOR '.AND.IHARG2(J).EQ.'    ')GO TO 630
  610 CONTINUE
      GO TO 680
!
  620 CONTINUE
      ICASEQ='SUBS'
      ILOCQ=J1
      GO TO 680
!
  630 CONTINUE
      ICASEQ='FOR'
      ILOCQ=J1
      GO TO 680
!
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
      GO TO 9000
!
  680 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RAND')THEN
        WRITE(ICOUT,681)NUMARG,ILOCQ,ICASEQ
  681   FORMAT('NUMARG,ILOCQ,ICASEQ = ',2I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ******************************************************
!               **  STEP 7--                                        **
!               **  BRANCH TO THE APPROPRIATE SUBCASE               **
!               **  (BASED ON THE QUALIFIER);                       **
!               **  DETERMINE THE NUMBER (= NRAN)                   **
!               **  OF RANDOM NUMBERS TO BE GENERATED.              **
!               **  NOTE THAT THE VARIABLE NIISUB                   **
!               **  IS THE LENGTH OF THE RESULTING                  **
!               **  VARIABLE ISUB(.).                               **
!               **  NOTE THAT DPFOR AUTOMATICALLY EXTENDS           **
!               **  THE INPUT LENGTH OF ISUB(.) IF NECESSARY.       **
!               **  (HENCE THE REDEFINITION OF NIISUB TO NINEW      **
!               **  AFTER THE CALL TO DPFOR.                        **
!               ******************************************************
!
      ISTEPN='7'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RAND')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!CCCC OCTOBER 1993.  JACKNIFE INDEX TO DPMATC.
!CCCC IF(ICASRA.EQ.'JACK')GO TO 1280
      IF(ICASEQ.EQ.'SUBS')THEN
        NIISUB=MAXN
        CALL DPSUBS(NIISUB,ILOCS,NS,IBUGQ,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
        NRAN=NS
      ELSEIF(ICASEQ.EQ.'FOR')THEN
        IF(NEWNAM.EQ.'NO')NIISUB=NLEFT
        IF(NEWNAM.EQ.'YES')NIISUB=MAXN
        CALL DPFOR(NIISUB,NINEW,IROW1,IROWN,   &
                   NLOCAL,ILOCS,NS,IBUGQ,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
        NIISUB=NINEW
        NRAN=NS
      ELSE
        IF(NEWNAM.EQ.'NO')NIISUB=NLEFT
        IF(NEWNAM.EQ.'YES')NIISUB=MAXN
        DO 715 I=1,NIISUB
          ISUB(I)=1
  715   CONTINUE
        NRAN=NIISUB
      ENDIF
!
  750 CONTINUE
!
      IF(NRAN.LT.1)THEN
        WRITE(ICOUT,321)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,762)
  762   FORMAT('      THE SPECIFIED NUMBER OF RANDOM ITEMS MUST BE ',   &
               '1 OR LARGER.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,769)NRAN
  769   FORMAT('      THE SPECIFIED NUMBER OF ITEMS =  ',I8)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!               ******************************************
!               **  STEP 8--                            **
!               **  GENERATE    NRAN    RANDOM NUMBERS  **
!               **  FROM THE SPECIFIED DISTRIBUTION.    **
!               **  STORE THEM TEMPORARILY IN           **
!               **  THE VECTOR Y(.).                    **
!               ******************************************
!
      ISTEPN='8'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RAND')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     MARCH 2013: EXCLUSION ZONE UNIFORM IS A SPECIAL CASE THAT
!                 IS NOT RECOGNIZED IN EXTDIS AND EXTPA1.
!
      IF(NUMSHA.GE.1)THEN
        CALL EXTPA1(ICASRA,IDIST,A,B,   &
                    SHAPE1,SHAPE2,SHAPE3,SHAPE4,   &
                    SHAPE5,SHAPE6,SHAPE7,   &
                    IADEDF,IGEPDF,IMAKDF,IBEIDF,   &
                    ILGADF,ISKNDF,IGLDDF,IBGEDF,   &
                    IGETDF,ICONDF,IGOMDF,IKATDF,   &
                    IGIGDF,IGEODF,   &
                    IBFWLI,IEEWLI,   &
                    ISUBRO,IBUGA3,IERROR)
      ENDIF
!
      IF(ICASRA.EQ.'UNEX')THEN
        IHP='A   '
        IHP2='    '
        IHWUSE='P'
        MESSAG='NO'
        CALL CHECKN(IHP,IHP2,IHWUSE,   &
                    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                    ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
        IF(IERROR.EQ.'YES')THEN
          A=0.0
        ELSE
          A=VALUE(ILOCP)
        ENDIF
!
        IHP='B   '
        IHP2='    '
        IHWUSE='P'
        MESSAG='NO'
        CALL CHECKN(IHP,IHP2,IHWUSE,   &
                    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                    ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
        IF(IERROR.EQ.'YES')THEN
          B=1.0
        ELSE
          B=VALUE(ILOCP)
        ENDIF
!
        IHP='DIAM'
        IHP2='    '
        IHWUSE='P'
        MESSAG='YES'
        CALL CHECKN(IHP,IHP2,IHWUSE,   &
                    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                    ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
        SHAPE1=VALUE(ILOCP)
      ENDIF
!
      IF(ICASRA.EQ.'SUBS')THEN
        CALL RANSUB(NRAN,ISEED,Y)
      ELSEIF(ICASRA.EQ.'KNSE')THEN
        IHP='N   '
        IHP2='    '
        IHWUSE='P'
        MESSAG='YES'
        CALL CHECKN(IHP,IHP2,IHWUSE,   &
                    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                    ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
        NPAR=INT(VALUE(ILOCP)+0.5)
!
        IF(NRAN.GT.NPAR)THEN
          WRITE(ICOUT,321)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3862)
 3862     FORMAT('      FOR THE  K-SET OF N-SET    CASE, THE VALUE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3863)
 3863     FORMAT('      OF K MUST BE LESS THAN OR EQUAL TO THE VALUE ',   &
                 'OF N.')
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
          GO TO 9000
        ENDIF
        CALL RANKSB(NRAN,NPAR,ISEED,Y,ITEMP1)
      ELSEIF(ICASRA.EQ.'RANC')THEN
        IHP='N   '
        IHP2='    '
        IHWUSE='P'
        MESSAG='YES'
        CALL CHECKN(IHP,IHP2,IHWUSE,   &
                    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                    ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
        NPAR=INT(VALUE(ILOCP)+0.5)
!
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
          GO TO 9000
        ENDIF
!
        IF(NRAN.LT.1 .OR. NRAN.GT.NPAR)THEN
          WRITE(ICOUT,321)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3882)
 3882     FORMAT('      FOR THE RANDOM COMPOSITION CASE, THE VALUE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3883)
 3883     FORMAT('      OF K MUST BE LESS THAN OR EQUAL TO THE VALUE ',   &
                 'OF N')
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
          GO TO 9000
        ENDIF
        CALL RANCOM(NRAN,NPAR,ISEED,Y,ITEMP1)
      ELSEIF(ICASRA.EQ.'RANP')THEN
        IHP='N   '
        IHP2='    '
        IHWUSE='P'
        MESSAG='YES'
        CALL CHECKN(IHP,IHP2,IHWUSE,   &
                    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                    ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
        NPAR=INT(VALUE(ILOCP)+0.5)
!
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
          GO TO 9000
        ENDIF
!
        CALL RANPAR(K,NPAR,ISEED,Y,ITEMP1,ITEMP2)
        NRAN=K
        DO 3929 II=1,NRAN
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
        CALL CHECKN(IHP,IHP2,IHWUSE,   &
                    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                    ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
        NPAR=INT(VALUE(ILOCP)+0.5)
!
        IF(NPAR.LT.1)THEN
          WRITE(ICOUT,321)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3932)
 3932     FORMAT('      FOR THE RANDOM EQUIVALENCE RELATION CASE, ',   &
                 'THE VALUE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3933)
 3933     FORMAT('      OF N MUST BE AT LEAST 1.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8197)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3935)NPAR
 3935     FORMAT('      THE SPECIFIED VALUE OF N  =  ',I8)
          CALL DPWRST('XXX','BUG ')
          GO TO 9000
        ENDIF
!
        IF(NPAR.NE.NLAST)THEN
          NLAST=1
        ENDIF
        CALL RANEQU(NPAR,LTEMP,ITEMP1,ITEMP2,TEMP3,ITEMP4,ISEED,Y)
        NRAN=NPAR
        DO 3939 II=1,NRAN
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
        CALL CHECKN(IHP,IHP2,IHWUSE,   &
                    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                    ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
        NPAR=INT(VALUE(ILOCP)+0.5)
!
        IF(NPAR.LT.1)THEN
          WRITE(ICOUT,321)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3942)
 3942     FORMAT('      FOR THE RANDOM YOUNG TABLEAUX CASE, ',   &
                 'THE VALUE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3943)
 3943     FORMAT('      OF N MUST BE AT LEAST 1.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8197)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3945)NPAR
 3945     FORMAT('      THE SPECIFIED VALUE OF N  =  ',I8)
          CALL DPWRST('XXX','BUG ')
          GO TO 9000
        ENDIF
!
        ISUM=0
        DO 3948 I=1,NLEFT
          ITEMP1(I)=INT(TEMP4(I)+0.5)
          ISUM=ISUM + ITEMP1(I)
 3948   CONTINUE
        IF(NLEFT.LT.NPAR)THEN
          DO 3949 I=NLEFT+1,NPAR
            ITEMP1(I)=0
 3949     CONTINUE
        ENDIF
!
        CALL RANYTB(NPAR,ITEMP1,ITEMP2,ISEED)
        NRAN=NPAR
        DO 3952 II=1,NRAN
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
        CALL CHECKN(IHP,IHP2,IHWUSE,   &
                    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                    ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
        IF(IERROR.EQ.'YES')THEN
          RANLOC=0.0
        ELSE
          RANLOC=VALUE(ILOCV)
        ENDIF
        IHP='RANS'
        IHP2='CALE'
        IHWUSE='P'
        MESSAG='NO'
        CALL CHECKN(IHP,IHP2,IHWUSE,   &
                    IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                    ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
        IF(IERROR.EQ.'YES')THEN
          RANSCA=1.0
        ELSE
          RANSCA=VALUE(ILOCV)
          IF(RANSCA.LE.0.0)RANSCA=1.0
        ENDIF
!
        IF(ICASRA.EQ.'GMCL' .OR. ICASRA.EQ.'TRAP' .OR.   &
           ICASRA.EQ.'GTRA' .OR. ICASRA.EQ.'UTSP' .OR.   &
           ICASRA.EQ.'GLGP' .OR.   &
           ICASRA.EQ.'PARE' .OR. ICASRA.EQ.'PAR2'   &
          )THEN
          CONTINUE
        ELSE
          IHP='A   '
          IHP2='    '
          IHWUSE='P'
          MESSAG='NO'
          CALL CHECKN(IHP,IHP2,IHWUSE,   &
                      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
          IF(IERROR.EQ.'YES')THEN
            A=0.0
          ELSE
            A=VALUE(ILOCV)
          ENDIF
!
          IHP='B   '
          IHP2='    '
          IHWUSE='P'
          MESSAG='NO'
          CALL CHECKN(IHP,IHP2,IHWUSE,   &
                      IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,   &
                      ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
          IF(IERROR.EQ.'YES')THEN
            B=1.0
          ELSE
            B=VALUE(ILOCV)
          ENDIF
!
        ENDIF
!
        CALL DPRAN2(ICASRA,ISEED,Y,NRAN,TEMP3,   &
                    A,B,MINMAX,   &
                    SHAPE1,SHAPE2,SHAPE3,SHAPE4,SHAPE5,SHAPE6,SHAPE7,   &
                    IADEDF,IGEPDF,IMAKDF,IBEIDF,   &
                    ILGADF,ISKNDF,IGLDDF,IBGEDF,IGETDF,ICONDF,   &
                    IGOMDF,IKATDF,IGIGDF,IGEODF,   &
                    IBUGA3,ISUBRO,IFOUND,IERROR)
!
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
          GO TO 9000
        ENDIF
!
        DO 5970 JJ=1,NRAN
          Y(JJ)=RANLOC + RANSCA*Y(JJ)
 5970   CONTINUE
!
      ENDIF
!
!               ******************************************************
!               **  STEP 8--                                        **
!               **  IF CALLED FOR (THAT IS, IF IBUGA3 IS ON),       **
!               **  PRINT OUT THE INTERMEDIATE VARIABLE Y(.).       **
!               **  THIS IS USEFUL FOR DIAGNOSTIC PURPOSES          **
!               **  IN REVIEWING THE OUTPUT FROM THIS SUBROUTINE.   **
!               ******************************************************
!
      ISTEPN='9'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RAND')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,4011)
 4011   FORMAT('OUTPUT FROM MIDDLE OF DPRAND AFTER ALL XXXRAN ',   &
               'HAVE BEEN CALLED--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,4012)NRAN
 4012   FORMAT('NRAN = ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(NRAN.GE.1)THEN
          DO 4014 I=1,NRAN
            WRITE(ICOUT,4015)I,Y(I)
 4015       FORMAT('I,Y(I) = ',I8,F12.5)
            CALL DPWRST('XXX','BUG ')
 4014       CONTINUE
        ENDIF
      ENDIF
!
!               ******************************************************
!               **  STEP 9--                                        **
!               **  COPY THE RANDOM NUMBERS                         **
!               **  FROM THE INTERMEDIATE VECTOR Y(.)               **
!               **  TO THE APPROPRIATE COLUMN                       **
!               **  (BASED ON THE QUALIFIER--FULL, SUBSET, OR FOR)  **
!               **  IN THE INTERNAL DATAPLOT DATA TABLE.            **
!               ******************************************************
!
      ISTEPN='10'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RAND')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NS2=0
      DO 4060 I=1,NIISUB
        IJ=MAXN*(ICOLL-1)+I
        IF(ISUB(I).EQ.0)GO TO 4060
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
!
!               *******************************************
!               **  STEP 10--                            **
!               **  CARRY OUT THE LIST UPDATING AND      **
!               **  GENERATE THE INFORMATIVE PRINTING.   **
!               *******************************************
!
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RAND')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ICASEQ.EQ.'FULL'.AND.NEWNAM.EQ.'NO')NINEW=NLEFT
      IF(ICASEQ.EQ.'FULL'.AND.NEWNAM.EQ.'YES')NINEW=MAXN
      IF(ICASEQ.EQ.'SUBS'.AND.NEWNAM.EQ.'NO'.AND.   &
      NLEFT.GE.IROWN)NINEW=NLEFT
      IF(ICASEQ.EQ.'SUBS'.AND.NEWNAM.EQ.'NO'.AND.   &
      NLEFT.LT.IROWN)NINEW=IROWN
      IF(ICASEQ.EQ.'SUBS'.AND.NEWNAM.EQ.'YES')NINEW=IROWN
      IF(ICASEQ.EQ.'FOR'.AND.NEWNAM.EQ.'NO'.AND.   &
      NLEFT.GE.IROWN)NINEW=NLEFT
      IF(ICASEQ.EQ.'FOR'.AND.NEWNAM.EQ.'NO'.AND.   &
      NLEFT.LT.IROWN)NINEW=IROWN
      IF(ICASEQ.EQ.'FOR'.AND.NEWNAM.EQ.'YES')NINEW=IROWN
!
      IHNAME(ILISTL)=ILEFT
      IHNAM2(ILISTL)=ILEFT2
      IUSE(ILISTL)='V'
      IVALUE(ILISTL)=ICOLL
      VALUE(ILISTL)=ICOLL
      IN(ILISTL)=NINEW
!
      IF(NEWNAM.EQ.'YES')NUMNAM=NUMNAM+1
      IF(NEWNAM.EQ.'YES')NUMCOL=NUMCOL+1
!
      DO 4600 J4=1,NUMNAM
        IF(IUSE(J4).EQ.'V'.AND.IVALUE(J4).EQ.ICOLL)GO TO 4605
        GO TO 4600
 4605   CONTINUE
        IUSE(J4)='V'
        IVALUE(J4)=ICOLL
        VALUE(J4)=ICOLL
        IN(J4)=NINEW
 4600 CONTINUE
!
      IF(IPRINT.EQ.'OFF')GO TO 4559
      IF(IFEEDB.EQ.'OFF')GO TO 4559
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4511)ILEFT,ILEFT2,NS2
 4511 FORMAT('THE NUMBER OF VALUES GENERATED FOR ',   &
      'THE VARIABLE ',A4,A4,' = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
!
      IJ=MAXN*(ICOLL-1)+IROW1
      IF(ICOLL.LE.MAXCOL)THEN
         WRITE(ICOUT,4521)ILEFT,ILEFT2,V(IJ),IROW1
 4521    FORMAT('THE FIRST           COMPUTED VALUE OF ',   &
         A4,A4,' = ',E15.7,'   (ROW ',I6,')')
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
!
      IJ=MAXN*(ICOLL-1)+IROWN
      IF(NS2.NE.1)THEN
         IF(ICOLL.LE.MAXCOL)THEN
            WRITE(ICOUT,4531)NS2,ILEFT,ILEFT2,V(IJ),IROWN
 4531       FORMAT('THE LAST (',I5,'-TH) COMPUTED VALUE OF ',   &
                   2A4,' = ',E15.7,'   (ROW ',I6,')')
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
      IF(NS2.NE.1)GO TO 4590
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
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
 9016   FORMAT('NS,NIISUB,NRAN = ',3I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPRAND
      SUBROUTINE DPRAN2(ICASRA,ISEED,Y,NRAN,TEMP1,   &
                        A,B,MINMAX,   &
                        SHAPE1,SHAPE2,SHAPE3,SHAPE4,   &
                        SHAPE5,SHAPE6,SHAPE7,   &
                        IADEDF,IGEPDF,IMAKDF,IBEIDF,   &
                        ILGADF,ISKNDF,IGLDDF,IBGEDF,IGETDF,ICONDF,   &
                        IGOMDF,IKATDF,IGIGDF,IGEODF,   &
                        IBUGA3,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--THIS ROUTINE IS SPLIT OFF FROM DPRAND IN ORDER
!              TO ALLOW OTHER ROUTINES TO CALL THE RANDOM NUMBER
!              ROUTINES IN A GENERIC WAY.
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
!     VERSION NUMBER--2009/9
!     ORIGINAL VERSION--SEPTEMBER 2009. SPLIT OFF FROM DPRAND
!     UPDATED         --JULY      2010. END EFFECTS WEIBULL
!     UPDATED         --AUGUST    2010. BRITTLE FIBER WEIBULL
!     UPDATED         --JANUARY   2011. ARCTANGENT
!     UPDATED         --MARCH     2013. SINE
!     UPDATED         --MARCH     2013. EXCLUSION ZONE UNIFORM
!     UPDATED         --APRIL     2014. "G" AND "H" AS DISTINCT FROM
!                                       "G AND H" DISTRIBUTIONS
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
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
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!-----COMMON----------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION TEMP1(*)
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPRA'
      ISUBN2='N2  '
      IFOUND='YES'
      IERROR='NO'
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RAN2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPRAN2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGA3,ICASRA,NRAN,ISEED,MINMAX
   53   FORMAT('IBUGA3,ICASRA,NRAN,ISEED,MINMAX = ',2(A4,2X),3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)SHAPE1,SHAPE2
   55   FORMAT('SHAPE1,SHAPE2 = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ***********************************************
!               **  GENERATE THE RANDOM NUMBERS              **
!               ***********************************************
!
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
        DO 1352 II=1,NRAN
          CALL HYPRAN(INT(SHAPE1+0.1),INT(SHAPE2+0.1),INT(SHAPE3+0.1),   &
                      ISEED,JX)
          IF(JX.EQ.-1)THEN
            WRITE(ICOUT,1354)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1356)INT(SHAPE1+0.1),INT(SHAPE2+0.1),   &
                             INT(SHAPE3+0.1)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
 1354     FORMAT('****** ERROR IN GENERATING HYPERGEOMETRIC RANDOM ',   &
                 'NUMBERS.')
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
        CALL GTRRAN(NRAN,SHAPE1,SHAPE2,SHAPE3,SHAPE4,   &
                    SHAPE5,SHAPE6,SHAPE7,ISEED,Y)
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
!CCCC   CALL BEKRAN(NRAN,S1SQ,S2SQ,ANU,ISEED,Y)
      ELSEIF(ICASRA.EQ.'GMCL')THEN
        CALL GMCRAN(NRAN,ALPHA,A,ISEED,Y)
      ELSEIF(ICASRA.EQ.'HBOL')THEN
!CCCC   CALL HBORAN(NRAN,ALPHA,XI,ISEED,Y)
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
!CCCC ELSEIF(ICASRA.EQ.'OCCU')THEN
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
!CCCC   CALL TSORAN(NRAN,AN,THETA,ALOWLM,AUPPLM,ISEED,Y)
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
        CALL UTSRAN(NRAN,SHAPE1,SHAPE2,SHAPE3,SHAPE4,SHAPE5,SHAPE6,   &
                    ISEED,Y)
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
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
        DO 9020 II=1,NRAN
          WRITE(ICOUT,9022)II,Y(II)
 9022     FORMAT('II,Y(II) = ',I10,G15.7)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPRAN2
      SUBROUTINE DPRAW(X,FREQ,NX,IWRITE,MAXNXT,Y,NY,IBUGA3,IERROR)
!
!     PURPOSE--SOMETIMES DATA IS MADE AVAILABLE AS A FREQUENCY
!              TABLE.  HOWEVER, FOR A PARTICULAR TYPE OF ANALSYSIS
!              YOU MAY NEED THE DATA IN RAW (I.E., IF YOU HAVE
!              A FREQUENCY OF 10 FOR THE VALUE 1, SIMPLY GENERATE
!              THE VALUE 1 TEN TIMES).  NEED TO CHECK FOR ARRAY
!              EXCEEDING MAXIMUM ALLOWABLE.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2004/4
!     ORIGINAL VERSION--APRIL     2004.
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
      DIMENSION X(*)
      DIMENSION Y(*)
      DIMENSION FREQ(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPRA'
      ISUBN2='W   '
      IERROR='NO'
!
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
        DO 55 I=1,NX
          WRITE(ICOUT,56)I,X(I),FREQ(I)
   56     FORMAT('I,X(I), FREQ(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               **************************************
!               **  CONVERT FROM FREQUENCY TO RAW   **
!               **************************************
!
      IF(NX.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR--NUMBER OF CLASSES FOR FREQUENCY TO ',   &
               'RAW COMMAND IS LESS THAN 1.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      NY=0
      DO 200 I=1,NX
!
        NTEMP=INT(FREQ(I)+0.5)
        IF(NTEMP.LT.1)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,201)I,FREQ(I)
  201     FORMAT('***** ERROR--CLASS ',I8,' HAS NON-POSITIVE ',   &
                'FREQUENCY (= ',F12.5,')')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
!
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
          GO TO 9000
        ENDIF
!
        DO 210 J=1,NTEMP
          NY=NY+1
          Y(NY)=X(I)
  210   CONTINUE
  200 CONTINUE
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
 9011   FORMAT('***** AT THE END       OF DPRAW--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR
 9012   FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NX,NY
 9013   FORMAT('NX,NY = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,NY
          WRITE(ICOUT,9016)I,Y(I)
 9016     FORMAT('I,Y(I) = ',I8,E15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DPRAW
      SUBROUTINE DPRBCO(IHARG,IARG,NUMARG,IDERBC,MAXREG,IREBCO,   &
                        ICASCL,IREBC2,IRGBMX,   &
                        IBUGP2,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE REGION BORDER COLORS = THE COLORS
!              OF THE BORDER LINE AROUND THE REGIONS.
!              THESE ARE LOCATED IN THE VECTOR IREBCO(.).
!     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
!                     --NUMARG
!                     --IDERBC
!                     --MAXREG
!                     --IBUGP2 ('ON' OR 'OFF' )
!     OUTPUT ARGUMENTS--IREBCO (A CHARACTER VECTOR)
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
!     ORIGINAL VERSION--DECEMBER  1983.
!     UPDATED         --MAY       1994. PRINT MESSAGE STATING THAT
!                                       THIS IS AN OBSOLETE COMMAND
!                                       (USE LINE COLOR COMMAND).
!     UPDATED         --OCTOBER   2020. SUPPORT FOR RGB COLOR
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IDERBC
      CHARACTER*4 ICASCL
      CHARACTER*4 IBUGP2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHOLD1
      CHARACTER*4 IHOLD2
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      CHARACTER*4 IHARG(*)
      CHARACTER*4 IREBCO(*)
      DIMENSION IARG(*)
      DIMENSION IREBC2(MAXREG,3)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPRB'
      ISUBN2='CO  '
      IHOLD1='-999'
      IHOLD2='-999'
!
      NUMREG=0
      JHOLD1=-1
      JHOLD2=-1
      JHOLD3=-1
!
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'RBCO')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPRBCO--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASCL,IBUGP2,ISUBRO,IFOUND,IERROR
   52   FORMAT('ICASCL,IBUGP2,ISUBRO,IFOUND,IERROR = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)MAXREG,NUMREG,NUMARG,IRGBMX
   53   FORMAT('MAXREG,NUMREG,NUMARG,IRGBMX = ',4I8)
        CALL DPWRST('XXX','BUG ')
        DO 65 I=1,NUMARG
          WRITE(ICOUT,66)I,IARG(I),IHARG(I)
   66     FORMAT('I,IARG(I),IHARG(I) = ',2I8,2X,A4)
          CALL DPWRST('XXX','BUG ')
   65   CONTINUE
        DO 75 I=1,10
          WRITE(ICOUT,76)I,IREBCO(I),IREBC2(I,1),IREBC2(I,2),IREBC2(I,3)
   76     FORMAT('I,IREBCO(I),IREBC2(I,1),IREBC2(I,2),IREBC2(I,3) = ',   &
                 I8,2X,A4,2X,3I5)
          CALL DPWRST('XXX','BUG ')
   75   CONTINUE
      ENDIF
!
!               **************************************
!               **  STEP 1--                        **
!               **  BRANCH TO THE APPROPRIATE CASE  **
!               **************************************
!
      ISTEPN='1'
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'RBCO')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.1)GO TO 9000
      IF(ICASCL.EQ.'RGB ')GO TO 2000
!
!     THIS IS THE "STANDARD" CASE
!
      IF(NUMARG.EQ.2)THEN
        GO TO 1200
      ELSEIF(NUMARG.EQ.3)THEN
        IF(IHARG(3).EQ.'ALL')THEN
          IHOLD1='    '
          GO TO 1300
        ENDIF
        GO TO 1200
      ELSEIF(NUMARG.EQ.4)THEN
        IF(IHARG(3).EQ.'ALL')THEN
          IHOLD1=IHARG(4)
          GO TO 1300
        ELSEIF(IHARG(4).EQ.'ALL')THEN
          IHOLD1=IHARG(3)
          GO TO 1300
        ENDIF
        GO TO 1200
      ELSE
        GO TO 1200
      ENDIF
!
!               *************************************************
!               **  STEP 2--                                   **
!               **  TREAT THE SINGLE      SPECIFICATION  CASE  **
!               *************************************************
!
 1200 CONTINUE
      ISTEPN='2'
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'RBCO')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.2)THEN
        NUMREG=1
        IREBCO(1)=IDERBC
      ELSE
        NUMREG=NUMARG-2
        IF(NUMREG.GT.MAXREG)NUMREG=MAXREG
        DO 1225 I=1,NUMREG
          J=I+2
          IHOLD1=IHARG(J)
          IHOLD2=IHOLD1
          IF(IHOLD1.EQ.'ON')IHOLD2=IDERBC
          IF(IHOLD1.EQ.'OFF')IHOLD2=IDERBC
          IF(IHOLD1.EQ.'AUTO')IHOLD2=IDERBC
          IF(IHOLD1.EQ.'DEFA')IHOLD2=IDERBC
          IREBCO(I)=IHOLD2
 1225   CONTINUE
      ENDIF
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        DO 1278 I=1,NUMREG
          WRITE(ICOUT,1276)I,IREBCO(I)
 1276     FORMAT('THE COLOR OF REGION BORDER ',I6,   &
                 ' HAS JUST BEEN SET TO ',A4)
          CALL DPWRST('XXX','BUG ')
 1278   CONTINUE
      ENDIF
      IFOUND='YES'
      GO TO 9000
!
!               **************************
!               **  STEP 3--            **
!               **  TREAT THE ALL CASE  **
!               **************************
!
 1300 CONTINUE
      ISTEPN='3'
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'RBCO')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMREG=MAXREG
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2=IDERBC
      IF(IHOLD1.EQ.'OFF')IHOLD2=IDERBC
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDERBC
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDERBC
      DO 1315 I=1,NUMREG
        IREBCO(I)=IHOLD2
 1315 CONTINUE
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        I=1
        WRITE(ICOUT,1316)IREBCO(I)
 1316   FORMAT('THE COLOR OF ALL REGION BORDERS',   &
               ' HAS JUST BEEN SET TO ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IFOUND='YES'
      WRITE(ICOUT,1900)
 1900 FORMAT('****** WARNING.  THE REGION BORDER COLOR COMMAND IS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1901)
 1901 FORMAT('       NOT USED.  THE BORDER COLOR FOR REGIONS IS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1902)
 1902 FORMAT('       SET WITH THE LINE COLOR COMMAND.          ******')
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
!
!     RGB COLORS CASE: 3 COLORS SHOULD BE GIVEN
!
!                      REGION BORDER COLOR
!                      REGION BORDER COLOR IRED IBLUE IGREEN
!                      REGION BORDER COLOR IRED IBLUE IGREEN ALL
!                      REGION BORDER COLOR ALL IRED IBLUE IGREEN
!                      REGION BORDER COLOR IRED1 IBLUE1 IGREEN1 IRED2 ...
!
!                      THE "RGB" KEYWORD HAS ALREADY BEEN STRIPPED
!                      OUT.  NOTE THAT THE DEFAULT COLOR IS -999
!                      (I.E., NO RGB COLOR VALUES SPECIFIED).
!
 2000 CONTINUE
!
      JHOLD1=-999
      JHOLD2=-999
      JHOLD3=-999
      NUMREG=MAXREG
!
      IF(NUMARG.EQ.3 .AND. IHARG(3).EQ.'ALL')THEN
        GO TO 2300
      ELSEIF(NUMARG.EQ.6)THEN
        IF(IHARG(3).EQ.'ALL')THEN
          JHOLD1=IARG(4)
          JHOLD2=IARG(5)
          JHOLD3=IARG(6)
          IF(JHOLD1.LT.0 .OR. JHOLD1.GT.IRGBMX)JHOLD1=-1
          IF(JHOLD2.LT.0 .OR. JHOLD2.GT.IRGBMX)JHOLD1=-1
          IF(JHOLD3.LT.0 .OR. JHOLD3.GT.IRGBMX)JHOLD1=-1
          GO TO 2300
        ELSEIF(IHARG(6).EQ.'ALL')THEN
          JHOLD1=IARG(3)
          JHOLD2=IARG(4)
          JHOLD3=IARG(5)
          IF(JHOLD1.LT.0 .OR. JHOLD1.GT.IRGBMX)JHOLD1=-1
          IF(JHOLD2.LT.0 .OR. JHOLD2.GT.IRGBMX)JHOLD1=-1
          IF(JHOLD3.LT.0 .OR. JHOLD3.GT.IRGBMX)JHOLD1=-1
          GO TO 2300
        ENDIF
      ENDIF
!
!               *************************************************
!               **  STEP 2--                                   **
!               **  TREAT THE SINGLE      SPECIFICATION  CASE  **
!               *************************************************
!
      ISTEPN='22'
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'RBCO')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.2)THEN
        NUMREG=1
        IREBC2(1,1)=-1
        IREBC2(1,2)=-1
        IREBC2(1,3)=-1
      ELSE
        NTEMP=NUMARG-2
        NUMREG=NTEMP/3
        IF(NUMREG.LT.1)THEN
          IREBC2(1,1)=-1
          IREBC2(1,2)=-1
          IREBC2(1,3)=-1
        ELSEIF(NUMREG.GT.MAXREG)THEN
          NUMREG=MAXREG
        ENDIF
        DO 2225 I=1,NUMREG
          J1=(I-1)*3+3
          J2=J1+1
          J3=J1+2
          JHOLD1=IARG(J1)
          JHOLD2=IARG(J2)
          JHOLD3=IARG(J3)
          IF(JHOLD1.LT.0 .OR. JHOLD1.GT.IRGBMX)JHOLD1=-1
          IF(JHOLD2.LT.0 .OR. JHOLD2.GT.IRGBMX)JHOLD2=-1
          IF(JHOLD3.LT.0 .OR. JHOLD3.GT.IRGBMX)JHOLD3=-1
          IREBC2(I,1)=JHOLD1
          IREBC2(I,2)=JHOLD2
          IREBC2(I,3)=JHOLD3
 2225   CONTINUE
      ENDIF
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        DO 2278 I=1,NUMREG
          WRITE(ICOUT,2276)I,IREBC2(I,1),IREBC2(I,2),IREBC2(I,3)
 2276     FORMAT('THE RGB FILL COLORS OF REGION ',I6,   &
                 ' HAVE JUST BEEN SET TO ',3I8)
          CALL DPWRST('XXX','BUG ')
 2278   CONTINUE
      ENDIF
      IFOUND='YES'
      GO TO 9000
!
!               **************************
!               **  STEP 3--            **
!               **  TREAT THE ALL CASE  **
!               **************************
!
 2300 CONTINUE
      ISTEPN='23'
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'RBCO')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 2315 I=1,NUMREG
        IREBC2(I,1)=JHOLD1
        IREBC2(I,2)=JHOLD2
        IREBC2(I,3)=JHOLD3
 2315 CONTINUE
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        I=1
        WRITE(ICOUT,2316)IREBC2(I,1),IREBC2(I,2),IREBC2(I,3)
 2316   FORMAT('THE RGB FILL COLORS OF ALL REGIONS HAVE JUST ',   &
               'BEEN SET TO ',3I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IFOUND='YES'
!
      WRITE(ICOUT,1900)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1901)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1902)
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGP2.EQ.'ON' .OR. ISUBRO.EQ.'RBCO')THEN
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPRBCO--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IHOLD1,IHOLD2
 9014   FORMAT('IHOLD1,IHOLD2 = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)JHOLD1,JHOLD2,JHOLD3
 9016   FORMAT('JHOLD1,JHOLD2 = ',3I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPRBCO
      SUBROUTINE DPRBLI(IHARG,IHARG2,NUMARG,IDERBL,MAXREG,IREBLI,   &
                        IBUGP2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE BORDER LINES = THE LINES TYPES
!              OF THE BORDER AROUND THE REGIONS.
!              THESE ARE LOCATED IN THE VECTOR IREBLI(.).
!     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
!                     --NUMARG
!                     --IDERBL
!                     --MAXREG
!                     --IBUGP2 ('ON' OR 'OFF' )
!     OUTPUT ARGUMENTS--IREBLI (A CHARACTER VECTOR)
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
!     ORIGINAL VERSION--DECEMBER  1983.
!     UPDATED         --MAY       1994. PRINT MESSAGE SAYING TO USE THE
!                                       LINE COMMAND INSTEAD.
!     UPDATED         --AUGUST    1995. DASH2 BUG
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
!CCCC AUGUST 1995.  ADD FOLLOWING LINE
      CHARACTER*4 IHARG2
      CHARACTER*4 IDERBL
      CHARACTER*4 IREBLI
!
      CHARACTER*4 IBUGP2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHOLD1
      CHARACTER*4 IHOLD2
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      DIMENSION IHARG(*)
!CCCC AUGUST 1995.  ADD FOLLOWING LINE
      DIMENSION IHARG2(*)
      DIMENSION IREBLI(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPRB'
      ISUBN2='LI  '
!
      NUMREG=0
      IHOLD1='-999'
      IHOLD2='-999'
!
      IF(IBUGP2.EQ.'OFF')GO TO 90
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
      DO 65 I=1,NUMARG
      WRITE(ICOUT,66)IHARG(I)
   66 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,70)IREBLI(1)
   70 FORMAT('IREBLI(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO 75 I=1,10
      WRITE(ICOUT,76)I,IREBLI(I)
   76 FORMAT('I,IREBLI(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
   75 CONTINUE
   90 CONTINUE
!
!               **************************************
!               **  STEP 1--                        **
!               **  BRANCH TO THE APPROPRIATE CASE  **
!               **************************************
!
      ISTEPN='1'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.2)GO TO 9000
      IF(NUMARG.EQ.3)GO TO 1130
      IF(NUMARG.EQ.4)GO TO 1140
      IF(NUMARG.EQ.5)GO TO 1150
      GO TO 1160
!
 1130 CONTINUE
      GO TO 1200
!
 1140 CONTINUE
      IF(IHARG(5).EQ.'ALL')IHOLD1='    '
      IF(IHARG(5).EQ.'ALL')GO TO 1300
      GO TO 1200
!
 1150 CONTINUE
!CCCC APRIL 1996.  CHANGE IHOLD TO IHOLD1 BELOW
      IF(IHARG(5).EQ.'ALL')THEN
        IHOLD1=IHARG(6)
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(6).EQ.'2')IHOLD1='DA2'
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(6).EQ.'3')IHOLD1='DA3'
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(6).EQ.'4')IHOLD1='DA4'
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(6).EQ.'5')IHOLD1='DA5'
        GO TO 1300
      ENDIF
      IF(IHARG(6).EQ.'ALL')THEN
        IHOLD1=IHARG(5)
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(5).EQ.'2')IHOLD1='DA2'
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(5).EQ.'3')IHOLD1='DA3'
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(5).EQ.'4')IHOLD1='DA4'
        IF(IHOLD1.EQ.'DASH'.AND.IHARG2(5).EQ.'5')IHOLD1='DA5'
        GO TO 1300
      ENDIF
      GO TO 1200
!
 1160 CONTINUE
      GO TO 1200
!
!               *************************************************
!               **  STEP 2--                                   **
!               **  TREAT THE SINGLE     SPECIFICATION  CASE  **
!               *************************************************
!
 1200 CONTINUE
      ISTEPN='2'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.3)GO TO 1210
      GO TO 1220
!
 1210 CONTINUE
      NUMREG=1
      IREBLI(1)='    '
      GO TO 1270
!
 1220 CONTINUE
      NUMREG=NUMARG-3
      IF(NUMREG.GT.MAXREG)NUMREG=MAXREG
      DO 1225 I=1,NUMREG
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
      GO TO 1270
!
 1270 CONTINUE
      IF(IFEEDB.EQ.'OFF')GO TO 1279
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO 1278 I=1,NUMREG
      WRITE(ICOUT,1276)I,IREBLI(I)
 1276 FORMAT('THE LINE TYPE FOR REGION BORDER ',I6,   &
      ' HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1278 CONTINUE
 1279 CONTINUE
      IFOUND='YES'
      GO TO 9000
!
!               **************************
!               **  STEP 3--            **
!               **  TREAT THE ALL CASE  **
!               **************************
!
 1300 CONTINUE
      ISTEPN='3'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMREG=MAXREG
      IHOLD2=IHOLD1
      IF(IHOLD1.EQ.'ON')IHOLD2='SOLI'
      IF(IHOLD1.EQ.'OFF')IHOLD2='    '
      IF(IHOLD1.EQ.'AUTO')IHOLD2=IDERBL
      IF(IHOLD1.EQ.'DEFA')IHOLD2=IDERBL
      DO 1315 I=1,NUMREG
      IREBLI(I)=IHOLD2
 1315 CONTINUE
      GO TO 1370
!
 1370 CONTINUE
      IF(IFEEDB.EQ.'OFF')GO TO 1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1316)IREBLI(I)
 1316 FORMAT('THE LINE TYPE FOR ALL REGION BORDERS',   &
      ' HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1319 CONTINUE
      IFOUND='YES'
!CCCC ADD FOLLOWING SECTION MAY 1994.
      WRITE(ICOUT,2100)
 2100 FORMAT('****** WARNING.  THE REGION BORDER LINE COMMAND IS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2101)
 2101 FORMAT('       NOT USED.  THE BORDER LINE STYLE FOR')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2102)
 2102 FORMAT('       REGIONS IS SET WITH THE LINE COLOR COMMAND.*****')
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGP2.EQ.'OFF')GO TO 9090
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
      DO 9025 I=1,NUMARG
      WRITE(ICOUT,9026)IHARG(I)
 9026 FORMAT('IHARG(I) = ',A4)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      WRITE(ICOUT,9030)IREBLI(1)
 9030 FORMAT('IREBLI(1) = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO 9035 I=1,10
      WRITE(ICOUT,9036)I,IREBLI(I)
 9036 FORMAT('I,IREBLI(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPRBLI
      SUBROUTINE DPRBTH(IHARG,IARGT,ARG,NUMARG,PDERBT,MAXREG,PREBTH,   &
                        IBUGP2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE THE REGION (BORDER) LINE THICKNESSES = THE THICKNESSES
!              OF THE BORDER LINE AROUND THE REGIONS.
!              THESE ARE LOCATED IN THE VECTOR PREBTH(.).
!     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
!                     --IARGT  (A  CHARACTER VECTOR)
!                     --ARG
!                     --NUMARG
!                     --PDERBT
!                     --MAXREG
!                     --IBUGP2 ('ON' OR 'OFF' )
!     OUTPUT ARGUMENTS--PREBTH (A FLOATING POINT VECTOR)
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
!     ORIGINAL VERSION--DECEMBER  1983.
!     UPDATED         --MAY       1994. PRINT MESSAGE TO USE LINE
!                                       THICKNESS COMMAND INSTEAD.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IHARG
      CHARACTER*4 IARGT
!
      CHARACTER*4 IBUGP2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IHOLD1
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      DIMENSION IHARG(*)
      DIMENSION IARGT(*)
      DIMENSION ARG(*)
      DIMENSION PREBTH(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPRB'
      ISUBN2='TH  '
!
      NUMREG=0
      IHOLD1='-999'
      HOLD1=-999.0
      HOLD2=-999.0
!
      IF(IBUGP2.EQ.'OFF')GO TO 90
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
      DO 65 I=1,NUMARG
      WRITE(ICOUT,66)IHARG(I),IARGT(I),ARG(I)
   66 FORMAT('IHARG(I),IARGT(I),ARG(I) = ',A4,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,70)PREBTH(1)
   70 FORMAT('PREBTH(1) = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      DO 75 I=1,10
      WRITE(ICOUT,76)I,PREBTH(I)
   76 FORMAT('I,PREBTH(I) = ',I8,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
   75 CONTINUE
   90 CONTINUE
!
!               **************************************
!               **  STEP 1--                        **
!               **  BRANCH TO THE APPROPRIATE CASE  **
!               **************************************
!
      ISTEPN='1'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.1)GO TO 9000
      IF(NUMARG.EQ.2)GO TO 1120
      IF(NUMARG.EQ.3)GO TO 1130
      IF(NUMARG.EQ.4)GO TO 1140
      GO TO 1150
!
 1120 CONTINUE
      GO TO 1200
!
 1130 CONTINUE
      IF(IHARG(3).EQ.'ALL')IHOLD1='    '
      IF(IHARG(3).EQ.'ALL')HOLD1=PDERBT
      IF(IHARG(3).EQ.'ALL')GO TO 1300
      GO TO 1200
!
 1140 CONTINUE
      IF(IHARG(3).EQ.'ALL')IHOLD1=IHARG(4)
      IF(IHARG(3).EQ.'ALL')HOLD1=ARG(4)
      IF(IHARG(3).EQ.'ALL')GO TO 1300
      IF(IHARG(4).EQ.'ALL')IHOLD1=IHARG(3)
      IF(IHARG(4).EQ.'ALL')HOLD1=ARG(3)
      IF(IHARG(4).EQ.'ALL')GO TO 1300
      GO TO 1200
!
 1150 CONTINUE
      GO TO 1200
!
!               *************************************************
!               **  STEP 2--                                   **
!               **  TREAT THE SINGLE     SPECIFICATION  CASE  **
!               *************************************************
!
 1200 CONTINUE
      ISTEPN='2'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NUMARG.LE.2)GO TO 1210
      GO TO 1220
!
 1210 CONTINUE
      NUMREG=1
      PREBTH(1)=PDERBT
      GO TO 1270
!
 1220 CONTINUE
      NUMREG=NUMARG-2
      IF(NUMREG.GT.MAXREG)NUMREG=MAXREG
      DO 1225 I=1,NUMREG
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
      GO TO 1270
!
 1270 CONTINUE
      IF(IFEEDB.EQ.'OFF')GO TO 1279
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO 1278 I=1,NUMREG
      WRITE(ICOUT,1276)I,PREBTH(I)
 1276 FORMAT('THE THICKNESS OF REGION BORDER ',I6,   &
      ' HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1278 CONTINUE
 1279 CONTINUE
      IFOUND='YES'
      GO TO 9000
!
!               **************************
!               **  STEP 3--            **
!               **  TREAT THE ALL CASE  **
!               **************************
!
 1300 CONTINUE
      ISTEPN='3'
      IF(IBUGP2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMREG=MAXREG
      HOLD2=HOLD1
      IF(IHOLD1.EQ.'ON')HOLD2=PDERBT
      IF(IHOLD1.EQ.'OFF')HOLD2=PDERBT
      IF(IHOLD1.EQ.'AUTO')HOLD2=PDERBT
      IF(IHOLD1.EQ.'DEFA')HOLD2=PDERBT
      DO 1315 I=1,NUMREG
      PREBTH(I)=HOLD2
 1315 CONTINUE
      GO TO 1370
!
 1370 CONTINUE
      IF(IFEEDB.EQ.'OFF')GO TO 1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1316)PREBTH(I)
 1316 FORMAT('THE THICKNESS OF ALL REGION BORDERS',   &
      ' HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1319 CONTINUE
      IFOUND='YES'
!CCCC ADD FOLLOWING SECTION MAY 1994.
      WRITE(ICOUT,2100)
 2100 FORMAT('****** WARNING.  THE REGION THICKNESS COMMAND IS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2101)
 2101 FORMAT('       NOT USED.  THE BORDER THICKNESS FOR REGIONS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2102)
 2102 FORMAT('       IS SET WITH THE LINE THICKNESS COMMAND.  ******')
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGP2.EQ.'OFF')GO TO 9090
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
      DO 9025 I=1,NUMARG
      WRITE(ICOUT,9026)IHARG(I),IARGT(I),ARG(I)
 9026 FORMAT('IHARG(I),IARGT(I),ARG(I) = ',A4,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      WRITE(ICOUT,9030)PREBTH(1)
 9030 FORMAT('PREBTH(1) = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      DO 9035 I=1,10
      WRITE(ICOUT,9036)I,PREBTH(I)
 9036 FORMAT('I,PREBTH(I) = ',I8,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DPRBTH
      SUBROUTINE DPRCBD(MAXNXT,ICAPSW,IFORSW,ISEED,   &
                        IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--CARRY OUT A RANDOMIZED COMPLETE BLOCK DESIGN FOR ONE
!              TREATMENT FACTOR AND ONE BLOCKING FACTOR.  THE F-TEST
!              WILL BE COMPUTED (ESSENTIALL A FIXED EFFECTS TWO-WAY
!              ANOVA).  OPTIONALLY A PERMUTATON TEST WILL BE PERFORMED.
!     EXAMPLE--RANDOMIZED COMPLETE BLOCK TEST Y TREAT BLOCK
!     REFERENCE--HIGGINS (2004), "INTRODUCTION TO MODERN NONPARAMETRIC
!                STATISTICS", DUXBURY PRESS, CHAPTER 4.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2024/05
!     ORIGINAL VERSION--MAY       2024.
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
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 IHOST1
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 ICASE
!
      CHARACTER*4 IVARID
      CHARACTER*4 IVARI2
      CHARACTER*4 IVARI3
      CHARACTER*4 IVARI4
      CHARACTER*4 IVARI5
      CHARACTER*4 IVARI6
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
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZI.INC'
      PARAMETER (MAXNX2=5000)
!
      DIMENSION YRESP(MAXOBV)
      DIMENSION TREAT(MAXOBV)
      DIMENSION BLOCK(MAXOBV)
      DIMENSION YSAVE(MAXOBV)
      DIMENSION YSAVEQ(MAXOBV)
      DIMENSION DTREAT(MAXOBV)
      DIMENSION DBLOCK(MAXOBV)
      DIMENSION XTMEAN(MAXOBV)
      DIMENSION XTMEA2(MAXOBV)
      DIMENSION XBMEAN(MAXOBV)
      DIMENSION TAG1(MAXOBV)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TEMP3(MAXOBV)
      DIMENSION XTEMP1(MAXOBV)
      DIMENSION AINDEX(MAXOBV)
      DIMENSION YTEMP(MAXOBV)
      DIMENSION XCELL(MAXNX2,MAXNX2)
      INTEGER   IBTAG(MAXOBV)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
!
      EQUIVALENCE (GARBAG(IGARB1),YRESP(1))
      EQUIVALENCE (GARBAG(IGARB2),TREAT(1))
      EQUIVALENCE (GARBAG(IGARB3),BLOCK(1))
      EQUIVALENCE (GARBAG(IGARB4),YSAVE(1))
      EQUIVALENCE (GARBAG(IGARB5),DTREAT(1))
      EQUIVALENCE (GARBAG(IGARB6),DBLOCK(1))
      EQUIVALENCE (GARBAG(IGARB7),XTMEAN(1))
      EQUIVALENCE (GARBAG(IGARB8),XBMEAN(1))
      EQUIVALENCE (GARBAG(IGARB9),TAG1(1))
      EQUIVALENCE (GARBAG(IGAR10),TEMP1(1))
      EQUIVALENCE (GARBAG(JGAR11),TEMP2(1))
      EQUIVALENCE (GARBAG(JGAR12),XTEMP1(1))
      EQUIVALENCE (GARBAG(JGAR13),AINDEX(1))
      EQUIVALENCE (GARBAG(JGAR14),YTEMP(1))
      EQUIVALENCE (GARBAG(JGAR15),XCELL(1,1))
      EQUIVALENCE (GARBAG(JGAR16),XTMEA2(1))
      EQUIVALENCE (GARBAG(JGAR17),YSAVEQ(1))
      EQUIVALENCE (GARBAG(JGAR18),TEMP3(1))
      EQUIVALENCE (IGARBG(IIGAR1),IBTAG(1))
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPRC'
      ISUBN2='BD  '
      IFOUND='NO'
      IERROR='NO'
!
      MAXNXT=MAXOBV
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
!
!               **************************************************
!               **  TREAT THE RANDOMIZED COMPLETE BLOCK CASE    **
!               **************************************************
!
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'RCBD')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPRCBD--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA2,IBUGA3,IBUGQ,ISUBRO,MAXNXT
   52   FORMAT('IBUGA2,IBUGA3,IBUGQ,ISUBRO,MAXNXT = ',4(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *********************************************************
!               **  STEP 1--                                           **
!               **  EXTRACT THE COMMAND                                **
!               *********************************************************
!
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'RCBD')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ILASTZ=9999
      ICASAN='RCBD'
!
      ISTRT=0
      IF(ICOM.EQ.'RAND' .AND. IHARG(1).EQ.'COMP' .AND.   &
         IHARG(2).EQ.'BLOC' .AND. IHARG(3).EQ.'DESI' .AND.   &
         IHARG(4).EQ.'TEST')THEN
        ILASTZ=4
        IFOUND='YES'
      ELSEIF(ICOM.EQ.'RAND' .AND. IHARG(1).EQ.'COMP' .AND.   &
         IHARG(2).EQ.'BLOC' .AND. IHARG(3).EQ.'DESI')THEN
        ILASTZ=3
        IFOUND='YES'
      ELSEIF(ICOM.EQ.'RAND' .AND. IHARG(1).EQ.'COMP' .AND.   &
         IHARG(2).EQ.'BLOC' .AND. IHARG(3).EQ.'TEST')THEN
        ILASTZ=3
        IFOUND='YES'
      ELSEIF(ICOM.EQ.'RAND' .AND. IHARG(1).EQ.'COMP' .AND.   &
         IHARG(2).EQ.'BLOC')THEN
        ILASTZ=2
        IFOUND='YES'
      ENDIF
      IF(IFOUND.EQ.'NO')GO TO 9000
!
      DO 100 I=1,NUMARG
        IF(IHARG(I).EQ.'=')THEN
          IFOUND='NO'
          GO TO 9000
        ENDIF
  100 CONTINUE
!
      ISHIFT=ILASTZ
      CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,   &
                  IBUGA2,IERROR)
!
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'RCBD')THEN
        WRITE(ICOUT,91)ICASAN,ISHIFT
   91   FORMAT('DPRCBD: ICASAN,ISHIFT = ',A4,2X,I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ****************************************
!               **  STEP 2--                          **
!               **  EXTRACT THE VARIABLE LIST         **
!               ****************************************
!
      ISTEPN='2'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'RCBD')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      INAME='RANDOMIZED COMPLETE BLOCK DESIGN TEST'
      MINNA=3
      MAXNA=100
      MINN2=4
      IFLAGE=1
      IFLAGM=0
      MINNVA=3
      MAXNVA=3
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'RCBD')THEN
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
      ISTEPN='3'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'RCBD')   &
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMVA2=3
      ICOL=1
      CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,   &
                  INAME,IVARN1,IVARN2,IVARTY,   &
                  ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,   &
                  MAXCOL,MAXCP1,MAXCP2,MAXCP3,   &
                  MAXCP4,MAXCP5,MAXCP6,   &
                  V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,   &
                  YRESP,TREAT,BLOCK,N,N,N,ICASE,   &
                  IBUGA3,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!               **********************************************
!               **  STEP 62--                               **
!               **  PERFORM RANDOMIZED COMPLETE BLOCK TEST  **
!               ***********************************************
!
      ISTEPN='62'
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'RCBD')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6211)
 6211   FORMAT('***** FROM DPRCBD, BEFORE CALL DPRCB2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6212)N,MAXN
 6212   FORMAT('N,MAXN = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO 6215 II=1,N
          WRITE(ICOUT,6216)II,YRESP(II),TREAT(II),BLOCK(II)
 6216     FORMAT('I,YRESP(II),TREAT(II),BLOCK(II) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
 6215   CONTINUE
      ENDIF
!
      IVARID=IVARN1(1)
      IVARI2=IVARN2(1)
      IVARI3=IVARN1(2)
      IVARI4=IVARN2(2)
      IVARI5=IVARN1(3)
      IVARI6=IVARN2(3)
      CALL DPRCB2(YRESP,TREAT,BLOCK,N,                             &
                  DBLOCK,DTREAT,XTMEAN,XBMEAN,XCELL,               &
                  YSAVE,TAG1,AINDEX,YTEMP,                         &
                  TEMP1,TEMP2,TEMP3,XTEMP1,IBTAG,XTMEA2,YSAVEQ,    &
                  ICAPSW,ICAPTY,IFORSW,ISEED,MAXNXT,MAXNX2,        &
                  IRCBPT,IPTESS,PPTEVA,                            &
                  IVARID,IVARI2,IVARI3,IVARI4,IVARI5,IVARI6,       &
                  STATVA,STATCD,PVALUT,STATCP,PVALP,               &
                  PMEAN,PMED,PSD,P001,P005,P01,P025,P05,P10,P20,   &
                  P50,P80,P90,P95,P975,P99,P995,P999,              &
                  IBUGA3,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
!               ***************************************
!               **  STEP 8D--                        **
!               **  UPDATE INTERNAL DATAPLOT TABLES  **
!               ***************************************
!
      ISTEPN='8D'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FTE2')   &
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(STATVA.NE.CPUMIN)THEN
        IH='STAT'
        IH2='VAL '
        VALUE0=STATVA
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,                     &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(STATCD.NE.CPUMIN)THEN
        IH='STAT'
        IH2='CDF '
        VALUE0=STATCD
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,                       &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(STATCP.NE.CPUMIN)THEN
        IH='STAT'
        IH2='CDFP'
        VALUE0=STATCP
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,                       &
                      IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                      IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(PVALUT.NE.CPUMIN)THEN
        IH='PVAL'
        IH2='UE  '
        VALUE0=PVALUT
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,                     &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(PVALP.NE.CPUMIN)THEN
        IH='PVAL'
        IH2='UEPT'
        VALUE0=PVALP
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,                     &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P001.NE.CPUMIN)THEN
        IH='P001'
        IH2='    '
        VALUE0=P001
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,                     &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P005.NE.CPUMIN)THEN
        IH='P005'
        IH2='    '
        VALUE0=P005
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,                     &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P01.NE.CPUMIN)THEN
        IH='P01'
        IH2='    '
        VALUE0=P01
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,                     &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P025.NE.CPUMIN)THEN
        IH='P025'
        IH2='    '
        VALUE0=P025
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,                     &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P05.NE.CPUMIN)THEN
        IH='P05'
        IH2='    '
        VALUE0=P05
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,                     &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P10.NE.CPUMIN)THEN
        IH='P10'
        IH2='    '
        VALUE0=P10
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,                     &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P20.NE.CPUMIN)THEN
        IH='P20'
        IH2='    '
        VALUE0=P20
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,                     &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P50.NE.CPUMIN)THEN
        IH='P50'
        IH2='    '
        VALUE0=P50
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,                     &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P80.NE.CPUMIN)THEN
        IH='P80'
        IH2='    '
        VALUE0=P80
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,                     &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P90.NE.CPUMIN)THEN
        IH='P90'
        IH2='    '
        VALUE0=P90
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,                     &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P95.NE.CPUMIN)THEN
        IH='P95'
        IH2='    '
        VALUE0=P95
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,                     &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P975.NE.CPUMIN)THEN
        IH='P975'
        IH2='    '
        VALUE0=P975
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,                     &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P99.NE.CPUMIN)THEN
        IH='P99'
        IH2='    '
        VALUE0=P99
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,                     &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P995.NE.CPUMIN)THEN
        IH='P995'
        IH2='    '
        VALUE0=P995
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,                     &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      IF(P999.NE.CPUMIN)THEN
        IH='P999'
        IH2='    '
        VALUE0=P999
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,                     &
                    IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,   &
                    IANS,IWIDTH,IBUGA3,IERROR)
      ENDIF
!
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'RCBD')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPRCBD--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)IFOUND,IERROR
 9016   FORMAT('IFOUND,IERROR= ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPRCBD
      SUBROUTINE DPRCB2(Y,TREAT,BLOCK,N,                                 &
                        DBLOCK,DTREAT,XTMEAN,XBMEAN,XCELL,               &
                        YSAVE,TAG1,AINDEX,YTEMP,                         &
                        TEMP1,TEMP2,TEMP3,XTEMP1,IBTAG,XTMEA2,YSAVEQ,    &
                        ICAPSW,ICAPTY,IFORSW,ISEED,MAXNXT,MAXNX2,        &
                        IRCBPT,IPTESS,PPTEVA,                            &
                        IVARID,IVARI2,IVARI3,IVARI4,IVARI5,IVARI6,       &
                        STATVA,STATCD,PVALUT,STATCP,PVALP,               &
                        PMEAN,PMED,PSD,P001,P005,P01,P025,P05,P10,P20,   &
                        P50,P80,P90,P95,P975,P99,P995,P999,              &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS ROUTINE PERFORMS A PARAMETRIC ANALYSIS OF A
!              RANDOMIZED COMPLETE BLOCK DESIGN.  THAT IS, THERE IS
!              A TREATMENT VARIABLE OF INTEREST AND AN ASSOCIATED
!              BLOCKING VARIABLE.
!
!              THIS VERSION RETURNS THE CDF AND P-VALUE BASED ON THE
!              PARAMETRIC F-TEST.  DPCRB2 ALSO PERFORMS A PERMUTATION
!              BASED TEST.
!
!     EXAMPLE--RANDOMIZED COMPLETE BLOCK Y TREAT BLOCK
!     REFERENCE--HIGGINS (2004), "INTRODUCTION TO MODERN NONPARAMETRIC
!                STATISTICS", DUXBURY PRESS, CHAPTER 4.
!     DESCRIPTION--THE ALGORITHMS FOR BOTH THE F-TEST AND THE PERMUATION
!                  TEST ARE GIVEN ON PP. 128-129 OF HIGGINS.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2025/06
!     ORIGINAL VERSION--JUNE      2025.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 IRCBPT
      CHARACTER*4 IVARID
      CHARACTER*4 IVARI2
      CHARACTER*4 IVARI3
      CHARACTER*4 IVARI4
      CHARACTER*4 IVARI5
      CHARACTER*4 IVARI6
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      DIMENSION Y(*)
      DIMENSION TREAT(*)
      DIMENSION BLOCK(*)
      DIMENSION YSAVE(*)
      DIMENSION YSAVEQ(*)
      DIMENSION DTREAT(*)
      DIMENSION DBLOCK(*)
      DIMENSION XTMEAN(*)
      DIMENSION XTMEA2(*)
      DIMENSION XBMEAN(*)
      DIMENSION TAG1(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
      DIMENSION XTEMP1(*)
      DIMENSION AINDEX(*)
      DIMENSION YTEMP(*)
      DIMENSION XCELL(MAXNX2,MAXNX2)
      INTEGER   IBTAG(*)
!
      REAL MSTR
      REAL MSB
      REAL MSE
      REAL MSI
      DOUBLE PRECISION DTERM1
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IOP
      CHARACTER*4 IDIR
      CHARACTER*4 IREPL
!
!---------------------------------------------------------------------
!
      PARAMETER(NUMCLI=8)
      PARAMETER(MAXLIN=3)
      PARAMETER(NUMALP=4)
      PARAMETER (MAXROW=30)
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
      INTEGER      IDIGI2(MAXRO2,NUMCLI)
      INTEGER      ROWSEP(MAXRO2)
      INTEGER      NTOT(MAXRO2)
      CHARACTER*60 ITITL2(MAXLIN,NUMCLI)
      CHARACTER*15 IVALUE(MAXRO2,NUMCLI)
      CHARACTER*4  ITYPCO(NUMCLI)
      INTEGER      NCTIT2(MAXLIN,NUMCLI)
      INTEGER      NCVALU(MAXRO2,NUMCLI)
      INTEGER      NCOLSP(MAXLIN,NUMCLI)
      INTEGER      IWHTML(NUMCLI)
      INTEGER      IWRTF(NUMCLI)
      REAL         AMAT(MAXRO2,NUMCLI)
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
      ISUBN1='DPRC'
      ISUBN2='B2  '
      IERROR='NO'
!
      STATVA=CPUMIN
      STATCD=CPUMIN
      PVALUT=CPUMIN
      BMEAN=CPUMIN
      PSD=CPUMIN
      P001=CPUMIN
      P005=CPUMIN
      P01=CPUMIN
      P025=CPUMIN
      P05=CPUMIN
      P10=CPUMIN
      P20=CPUMIN
      P50=CPUMIN
      P80=CPUMIN
      P90=CPUMIN
      P95=CPUMIN
      P975=CPUMIN
      P99=CPUMIN
      P995=CPUMIN
      P999=CPUMIN
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RCB2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPRCB2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N,MAXNXT,ISEED
   52   FORMAT('IBUGA3,ISUBRO,N,MAXNXT,ISEED = ',2(A4,2X),3I8)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,53)ICASAN,IRCBPT,IPTESS,PPTEVA
   53   FORMAT('ICASAN,IRCBPT,IPTESS,PPTEVA = ',2(A4,2X),I8,G15.7)
        CALL DPWRST('XXX','WRIT')
        DO I=1,N
          WRITE(ICOUT,57)I,Y(I),TREAT(I),BLOCK(I)
   57     FORMAT('I,Y(I),TREAT(I),BLOCK(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','WRIT')
        ENDDO
      ENDIF
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CALL DPRCB3 TO GET F-TEST RESULTS     **
!               ********************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RCB2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     SORT BY BLOCK-ID
!
      CALL SORTI(BLOCK,N,BLOCK,AINDEX)
      DO 210 II=1,N
        J=INT(AINDEX(II)+0.5)
        TEMP1(II)=TREAT(J)
        TEMP2(II)=Y(J)
  210 CONTINUE
      DO 220 II=1,N
        TREAT(II)=TEMP1(II)
        Y(II)=TEMP2(II)
  220 CONTINUE
!
!     EXTRACT DISTINCT VALUES IN BLOCK VARIABLE
!
      CALL DISTIN(BLOCK,N,IWRITE,DBLOCK,NBLOCK,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RCB2')THEN
        WRITE(ICOUT,231)
  231   FORMAT('AFTER SORT BY BLOCK:')
        CALL DPWRST('XXX','WRIT')
        DO II=1,N
          WRITE(ICOUT,233)II,Y(II),TREAT(II),BLOCK(II)
  233     FORMAT('II,Y(II),TREAT(II),BLOCK(II) ',I8,3G15.7)
          CALL DPWRST('XXX','WRIT')
        ENDDO
        DO II=1,NBLOCK
          WRITE(ICOUT,237)II,DBLOCK(II)
  237     FORMAT('II,DBLOCK(II) ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
        ENDDO
      ENDIF
!
      CALL DPRCB3(Y,TREAT,BLOCK,N,                           &
                  DBLOCK,DTREAT,TEMP3,XCELL,XTMEAN,XBMEAN,   &
                  MAXNXT,MAXNX2,                             &
                  STATVA,STATCD,PVAL,IREPL,IR,               &
                  NBLOCK,NTREAT,NUMDF1,NUMDF2,               &
                  XGRAND,SSTR,SSB,SSE,SSTOT,SSI,             &
                  MSTR,MSB,MSE,MSI,                          &
                  IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      CRDMSE=(SSB+SSI+SSE)/REAL(IR*NTREAT*(NBLOCK-1))
      RELEFF=CRDMSE/MSE
      IF(IRCBPT.EQ.'OFF')GO TO 600
!
!               **********************************************
!               **   STEP 2--                               **
!               **   GENERATE THE RANDOM PERMUTATIONS AND   **
!               **   COMPUTE THE DESIRED STATISTIC FOR EACH **
!               **   RANDOM PERMUTATION.                    **
!               **********************************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RCB2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     DETERMINE TOTAL SIZE (N*NUMBER OF PERMUTATION SAMPLES)
!
      NITER=IPTESS
      NITEMP=MAXNXT/N
      IF(NITEMP.GT.NITER)NITEMP=NITER
      IF(NITEMP.GT.1600)NITEMP=1600
      ICNT2=0
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RCB2')THEN
        WRITE(ICOUT,301)IPTESS,NITER,NITEMP
  301   FORMAT('IPTESS,NITER,NITEMP = ',3I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IOP='OPEN'
      IFLAG1=1
      IFLAG2=1
      IFLAG3=0
      IFLAG4=0
      IFLAG5=0
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,       &
                  IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      WRITE(IOUNI2,*) 'X             TAG              Y            ',   &
                      'TREATMENT         BLOCK'
!
      ILOOP=0
  310 CONTINUE
!
!       GENERATE NITEMP RANDOM PERMUTATIONS
!
        ILOOP=ILOOP+1
        IDIST=0
!CCCC   IDIST=1
        CALL RANPE3(N,NITEMP,IDIST,MAXNXT,ISEED,      &
                    TEMP1,TAG1,XTEMP1,                &
                    BLOCK,DBLOCK,IBTAG,NBLOCK,NOUT,   &
                    ISUBRO,IBUGA3,IERROR)
        IF(IERROR.EQ.'YES')GO TO 9000
!
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RCB2')THEN
          WRITE(ICOUT,314)MAXNXT,NOUT
  314     FORMAT('AFTER RANPE3, MAXNXT,NOUT = ',2I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        IF(ILOOP.GT.1)THEN
          IINC=(ILOOP-1)*NITEMP
          DO KK=1,NOUT
            TAG1(KK)=REAL(IINC) + TAG1(KK)
          ENDDO
        ENDIF
        DO KK=1,NOUT
          IVAL=INT(TAG1(KK)+0.1)
          IF(IVAL.GT.NITER)GO TO 318
          YVAL=Y(INT(TEMP1(KK)+0.1))
          KK2=MOD(KK,IR*NTREAT*NBLOCK)
          IF(KK2.EQ.0)KK2=IR*NTREAT*NBLOCK
          WRITE(IOUNI2,'(5E15.7)')TEMP1(KK),TAG1(KK),YVAL,TREAT(KK2),BLOCK(KK2)
        ENDDO
  318   CONTINUE
!
        DO 320 J=1,NITEMP
          ICNT2=ICNT2+1
          IF(ICNT2.GT.NITER)THEN
            ICNT2=ICNT2-1
            GO TO 390
          ENDIF
!
!         COMPUTE STATISTIC FOR THE RANDOM PERMUTATIONS
!
          NSTRT1=(J-1)*N + 1
          NSTOP1=NSTRT1+N-1
!
!         EXTRACT THE PERMUTED VARIABLES
!
          ICNT3=0
          ICNT4=0
          DO KK=NSTRT1,NSTOP1
            ICNT3=ICNT3+1
            YTEMP(ICNT3)=Y(INT(TEMP1(KK)+0.1))
          ENDDO
!
          IF(ICNT3.NE.N)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,101)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,321)ICNT2
  321       FORMAT('      NUMBER OF VALUES IN PERMUTED SAMPLE ',I6,   &
                   ' IS NOT EQUAL TO SAMPLE SIZE.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,323)N1
  323       FORMAT('      EXPECTED SAMPLE SIZE: ',I8)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,325)ICNT3
  325       FORMAT('      RETURNED SAMPLE SIZE: ',I8)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
!
!         COMPUTE THE RANDOMIZED COMPLETE BLOCK DESIGN STATISTIC
!
          YSAVE(ICNT2)=0.0
!
          IRT=IR
          CALL DPRCB3(YTEMP,TREAT,BLOCK,N,                       &
                      DBLOCK,DTREAT,TEMP3,XCELL,XTMEA2,XBMEAN,   &
                      MAXNXT,MAXNX2,                             &
                      STATVT,STATCT,PVALT,IREPL,IRT,             &
                      NBLOCK,NTREAT,NUMDF1,NUMDF2,               &
                      AP1,AP2,AP3,AP4,AP5,AP9,                   &
                      AP6,AP7,AP8,AP10,                          &
                      IBUGA3,ISUBRO,IERROR)
          IF(IERROR.EQ.'YES')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,101)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,101)
  101       FORMAT('      ERROR IN COMPUTING RANDOMIZED COMPLETE ',   &
                   'BLOCK DESIGN STATISTIC')
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,353)
  353       FORMAT('      FOR ONE OF THE PERMUTATIONS.  PERMUTATION ',   &
                   'TEST NOT PERFORMED.')
            CALL DPWRST('XXX','WRIT')
            IRCB=0
            GO TO 600
          ENDIF
!
          IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RCB2')THEN
            WRITE(ICOUT,361)
  361       FORMAT('AFTER CALL DPRCB3')
            CALL DPWRST('XXX','BUG ')
            DO KK=1,NTREAT
              WRITE(ICOUT,363)ICNT2,XTMEA2(KK)
  363         FORMAT('ICNT2,XTMEA2(KK) = ',I6,G15.7)
              CALL DPWRST('XXX','BUG ')
            ENDDO
          ENDIF
!
          YSAVE(ICNT2)=STATVT
!
!         COMPUTE: QSTAR = MAX[ij]ABS(XBAR(i.) - XBAR(j.))
!
!         THIS WILL BE USED FOR MULTIPLE COMPARISON PROCEDURE
!
          QSTAR=CPUMIN
          DO II=1,NTREAT
            HOLD1=XTMEA2(II)
            DO JJ=II+1,NTREAT
              HOLD2=XTMEA2(JJ)
              AVAL=ABS(HOLD1-HOLD2)
              IF(AVAL.GT.QSTAR)QSTAR=AVAL
            ENDDO
          ENDDO
          YSAVEQ(ICNT2)=QSTAR
!
          IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RCB2')THEN
            WRITE(ICOUT,359)ICNT2,STATVT,YSAVE(ICNT2)
  359       FORMAT('ICNT2,STATVT,YSAVE(ICNT2) = ',I6,2G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
  320   CONTINUE
        IF(ICNT2.GE.NITER)GO TO 390
        GO TO 310
!
  390 CONTINUE
      IRCB=1
!
!               **************************************************
!               **  STEP 4--                                    **
!               **  WRITE OUT COMPUTED STATISTICS TO DPST1F.DAT **
!               **************************************************
!
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RCB2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      WRITE(IOUNI1,412)
  412 FORMAT('COMPUTED STATISTICS FROM RANDOMIZED COMPLETE BLOCK ',   &
             'DESIGN TEST')
      DO 410 II=1,ICNT2
        WRITE(IOUNI1,'(2E15.7)')YSAVE(II),YSAVEQ(II)
  410 CONTINUE
!
!
!               **************************************************
!               **  STEP 5--                                    **
!               **  DETERMINE SELECTED PERCENTILES              **
!               **************************************************
!
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RCB2')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,506)ICNT2
  506   FORMAT('ICNT2 = ',I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      CALL SORT(YSAVE,ICNT2,YSAVE)
      CALL MEAN(YSAVE,ICNT2,IWRITE,PMEAN,IBUGA3,IERROR)
      CALL MEDIAN(YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,PMED,IBUGA3,IERROR)
      CALL SD(YSAVE,ICNT2,IWRITE,PSD,IBUGA3,IERROR)
!
      IDIR='UPPE'
      CALL DPGOF8(YSAVE,ICNT2,STATVA,PVALP,IDIR,IBUGA3,ISUBRO,IERROR)
      STATCP=1.0-PVALP
!
      PPERC=0.1
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P001,IBUGA3,IERROR)
!
      PPERC=0.5
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P005,IBUGA3,IERROR)
!
      PPERC=1.0
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P01,IBUGA3,IERROR)
!
      PPERC=2.5
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P025,IBUGA3,IERROR)
!
      PPERC=5.0
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P05,IBUGA3,IERROR)
!
      PPERC=10.0
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P10,IBUGA3,IERROR)
!
      PPERC=20.0
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P20,IBUGA3,IERROR)
!
      PPERC=50.0
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P50,IBUGA3,IERROR)
!
      PPERC=80.0
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P80,IBUGA3,IERROR)
!
      PPERC=90.0
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P90,IBUGA3,IERROR)
      CALL PERCEN(PPERC,YSAVEQ,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P90Q,IBUGA3,IERROR)
!
      PPERC=95.0
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P95,IBUGA3,IERROR)
      CALL PERCEN(PPERC,YSAVEQ,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P95Q,IBUGA3,IERROR)
!
      PPERC=97.5
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P975,IBUGA3,IERROR)
!
      PPERC=99.0
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P99,IBUGA3,IERROR)
      CALL PERCEN(PPERC,YSAVEQ,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P99Q,IBUGA3,IERROR)
!
      PPERC=99.5
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P995,IBUGA3,IERROR)
!
      PPERC=99.9
      CALL PERCEN(PPERC,YSAVE,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P999,IBUGA3,IERROR)
      CALL PERCEN(PPERC,YSAVEQ,ICNT2,IWRITE,TEMP1,MAXNXT,   &
                  P999Q,IBUGA3,IERROR)
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RCB2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,501)PMEAN,PMED,PSD,ICNT2
  501   FORMAT('PMEAN,PMED,PSD,ICNT2 = ',3G15.7,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,503)P001,P005,P01,P025
  503   FORMAT('P001,P005,P01,P025 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,505)P05,P10,P20,P50
  505   FORMAT('P05,P10,P20,P50 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,507)P80,P90,P95,P975
  507   FORMAT('P80,P90,P95,P975 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,509)P99,P995,P999
  509   FORMAT('P99,P995,P999 = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,511)P90Q,P95Q,P99Q,P999Q
  511   FORMAT('P90Q,P95Q,P99Q,P999Q = ',5G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *******************************************************
!               **   STEP 6A--                                       **
!               **   WRITE OUT EVERYTHING  FOR THE F-TEST            **
!               *******************************************************
!
  600 CONTINUE
!
      ISTEPN='6A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RCB2')   &
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
      ISTEPN='6A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RCB2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!     PRINT SUMMARY STATISTICS TABLE
!
      ITITLE(1:41)='Randomized Complete Block Design (F-Test)'
      NCTITL=41
      IF(IREPL.EQ.'OFF')THEN
        ITITLZ='No Replication Case'
        NCTITZ=19
      ELSE
        ITITLZ='Replication Case'
        NCTITZ=16
      ENDIF
!
      ICNT=1
      ITEXT(ICNT)='Response Variable: '
      WRITE(ITEXT(ICNT)(23:26),'(A4)')IVARID(1:4)
      WRITE(ITEXT(ICNT)(27:30),'(A4)')IVARI2(1:4)
      NCTEXT(ICNT)=30
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Treatment Variable: '
      WRITE(ITEXT(ICNT)(23:26),'(A4)')IVARI3(1:4)
      WRITE(ITEXT(ICNT)(27:30),'(A4)')IVARI4(1:4)
      NCTEXT(ICNT)=30
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Blocking Variable: '
      WRITE(ITEXT(ICNT)(23:26),'(A4)')IVARI5(1:4)
      WRITE(ITEXT(ICNT)(27:30),'(A4)')IVARI6(1:4)
      NCTEXT(ICNT)=30
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='H0: The Treatments Are Equal'
      NCTEXT(ICNT)=28
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: The Treatments Are Not All Equal'
      NCTEXT(ICNT)=36
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
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Test:'
      NCTEXT(ICNT)=5
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Total Number of Observations:'
      NCTEXT(ICNT)=28
      AVALUE(ICNT)=REAL(N)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Treatment Levels:'
      NCTEXT(ICNT)=27
      AVALUE(ICNT)=REAL(NTREAT)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Blocking Levels:'
      NCTEXT(ICNT)=26
      AVALUE(ICNT)=REAL(NBLOCK)
      IDIGIT(ICNT)=0
      IF(IREPL.EQ.'YES')THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='Number of Replications:'
        NCTEXT(ICNT)=23
        AVALUE(ICNT)=REAL(IR)
        IDIGIT(ICNT)=0
      ENDIF
      ICNT=ICNT+1
      ITEXT(ICNT)='Grand Mean:'
      NCTEXT(ICNT)=11
      AVALUE(ICNT)=XGRAND
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Relative Efficiency:'
      NCTEXT(ICNT)=20
      AVALUE(ICNT)=RELEFF
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Test CDF Value:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=STATCD
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Test P-Value:'
      NCTEXT(ICNT)=13
      AVALUE(ICNT)=PVAL
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      NUMROW=ICNT
      DO I=1,NUMROW
        NTOT(I)=15
      ENDDO
!
      IFRST=.TRUE.
      ILAST=.TRUE.
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,   &
                  AVALUE,IDIGIT,                              &
                  NTOT,NUMROW,                                &
                  ICAPSW,ICAPTY,ILAST,IFRST,                  &
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
      DO I=1,NUMCOL
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
        DO J=1,MAXROW
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
        ENDDO
      ENDDO
!
      DO J=1,MAXRO2
        DO I=1,NUMCOL
          IVALUE(J,I)=' '
          NCVALU(J,I)=0
          AMAT(J,I)=0.0
        ENDDO
        ROWSEP(J)=0
      ENDDO
!
!     TOTAL
!
      ICNT=1
      IVALUE(ICNT,1)='Total'
      NCVALU(ICNT,1)=5
      AMAT(ICNT,2)=REAL(NTREAT*NBLOCK-1)
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
      IVALUE(ICNT,1)='Treatment'
      NCVALU(ICNT,1)=10
      AMAT(ICNT,2)=REAL(NTREAT-1)
      AMAT(ICNT,3)=SSTR
      AMAT(ICNT,4)=MSTR
      AMAT(ICNT,5)=STATVA
      AMAT(ICNT,6)=100.0*STATCD
      AMAT(ICNT,7)=PVAL
      IF(STATCD.GE.0.99)THEN
        IVALUE(ICNT,8)='**'
        NCVALU(ICNT,8)=2
      ELSEIF(STATCD.GE.0.95)THEN
        IVALUE(ICNT,8)='*'
        NCVALU(ICNT,8)=1
      ELSE
        IVALUE(ICNT,8)=' '
        NCVALU(ICNT,8)=0
      ENDIF
!
!     BLOCKING
!
      ICNT=ICNT+1
      IVALUE(ICNT,1)='Blocking'
      NCVALU(ICNT,1)=8
      NUMDF1B=NBLOCK-1
      AMAT(ICNT,2)=REAL(NUMDF1B)
      AMAT(ICNT,3)=SSB
      AMAT(ICNT,4)=MSB
!
      STATVAB=MSB/MSE
      CALL FCDF(STATVAB,NUMDF1B,NUMDF2,STATCDB)
      PVALB=1.0 - STATCDB
!
      AMAT(ICNT,5)=STATVAB
      AMAT(ICNT,6)=100.0*STATCDB
      AMAT(ICNT,7)=PVALB
      IF(STATCDB.GE.0.99)THEN
        IVALUE(ICNT,8)='**'
        NCVALU(ICNT,8)=2
      ELSEIF(STATCDB.GE.0.95)THEN
        IVALUE(ICNT,8)='*'
        NCVALU(ICNT,8)=1
      ELSE
        IVALUE(ICNT,8)=' '
        NCVALU(ICNT,8)=0
      ENDIF
      IF(IR.EQ.1)ROWSEP(ICNT)=1
!
!     INTERACTION TERM IF REPLICATION
!
      IF(IR.GT.1)THEN
        ICNT=ICNT+1
        IVALUE(ICNT,1)='Interaction'
        NCVALU(ICNT,1)=11
        NUMDF1I=(NBLOCK-1)*(NTREAT-1)
        AMAT(ICNT,2)=REAL((NUMDF1I))
        AMAT(ICNT,3)=SSI
        AMAT(ICNT,4)=MSI
!
        STATVAI=MSI/MSE
        CALL FCDF(STATVAI,NUMDF1I,NUMDF2,STATCDI)
        PVALI=1.0 - STATCDI
!
        AMAT(ICNT,5)=STATVAI
        AMAT(ICNT,6)=100.0*STATCDI
        AMAT(ICNT,7)=PVALI
        IF(STATCDI.GE.0.99)THEN
          IVALUE(ICNT,8)='**'
          NCVALU(ICNT,8)=2
        ELSEIF(STATCDI.GE.0.95)THEN
          IVALUE(ICNT,8)='*'
          NCVALU(ICNT,8)=1
        ELSE
          IVALUE(ICNT,8)=' '
          NCVALU(ICNT,8)=0
        ENDIF
        ROWSEP(ICNT)=1
      ENDIF
!
!     ERROR
!
      ICNT=ICNT+1
      IVALUE(ICNT,1)='Error'
      NCVALU(ICNT,1)=5
      IF(IR.EQ.1)THEN
        AMAT(ICNT,2)=REAL((NTREAT-1)*(NBLOCK-1))
      ELSE
        AMAT(ICNT,2)=REAL(N - NTREAT*NBLOCK)
      ENDIF
      AMAT(ICNT,3)=SSE
      AMAT(ICNT,4)=MSE
      IDIGI2(ICNT,5)=-1
      IDIGI2(ICNT,6)=-1
      IDIGI2(ICNT,7)=-1
      IDIGI2(ICNT,8)=-1
!
      IWHTML(1)=125
      IWHTML(2)=25
      IWHTML(3)=125
      IWHTML(4)=125
      IWHTML(5)=125
      IWHTML(6)=125
      IWHTML(7)=125
      IWHTML(8)=25
      IINC=1800
      IINC2=200
      IWRTF(1)=IINC
      IWRTF(2)=IWRTF(1)+IINC2
      IWRTF(3)=IWRTF(2)+IINC
      IWRTF(4)=IWRTF(3)+IINC
      IWRTF(5)=IWRTF(4)+IINC
      IWRTF(6)=IWRTF(5)+IINC
      IWRTF(7)=IWRTF(6)+IINC
      IWRTF(8)=IWRTF(7)+IINC2
!
      IFRST=.TRUE.
      ILAST=.TRUE.
      IFLAGS=.TRUE.
      IFLAGE=.TRUE.
      CALL DPDT5B(ITITLE,NCTITL,                                &
                  ITITL9,NCTIT9,ITITL2,NCTIT2,                  &
                  MAXLIN,NUMLIN,NUMCLI,NUMCOL,                  &
                  IVALUE,NCVALU,AMAT,ITYPCO,MAXRO2,ICNT,        &
                  IDIGI2,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                  NCOLSP,ROWSEP,                                &
                  ICAPSW,ICAPTY,IFRST,ILAST,                    &
                  IFLAGS,IFLAGE,                                &
                  ISUBRO,IBUGA3,IERROR)
!
!       TUKEY-KRAMER MULTIPLE COMPARISONS
!
!       I DON'T THINK I HAVE THIS RIGHT YET, SO BYPASS FOR NOW
!
      IFLAGT=0
      IF(IFLAGT.EQ.0)GO TO 5099
      NDF1=NTREAT
      NDF2=(NTREAT-1)*(NBLOCK-1)
      ALPHAT=0.05
      DTERM1=QTRNG(DBLE(1.0 - ALPHAT),DBLE(NDF1),DBLE(NDF2),IFAULT)
      CV95T=REAL(DTERM1)
      ALPHAT=0.10
      DTERM1=QTRNG(DBLE(1.0 - ALPHAT),DBLE(NDF1),DBLE(NDF2),IFAULT)
      CV90T=REAL(DTERM1)
      ALPHAT=0.01
      DTERM1=QTRNG(DBLE(1.0 - ALPHAT),DBLE(NDF1),DBLE(NDF2),IFAULT)
      CV99T=REAL(DTERM1)
!
      ITITLE='Tukey Multiple Comparisons (upper-Tailed Test)'
      NCTITL=46
      ITITL9=' '
      NCTIT9=0
!
      DO II=1,3
        DO JJ=1,7
          ITITL2(II,JJ)=' '
          NCTIT2(II,JJ)=0
        ENDDO
      ENDDO
!
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
      ITITL2(3,4)='90% CV'
      NCTIT2(3,4)=6
      ITITL2(3,5)='95% CV'
      NCTIT2(3,5)=6
      ITITL2(3,6)='99% CV'
      NCTIT2(3,6)=6
      ITITL2(3,7)='99.9% CV'
      NCTIT2(3,7)=8
!
      NMAX=0
      NUMCOL=7
      NUMLIN=3
      DO I=1,NUMCOL
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
      ENDDO
      IWHTML(1)=50
      IWHTML(2)=50
      IWHTML(3)=150
      IWHTML(4)=150
      IWHTML(5)=150
      IWHTML(6)=150
      IWHTML(7)=150
      IINC=1600
      IINC2=200
      IINC3=1000
      IWRTF(1)=IINC2
      IWRTF(2)=IWRTF(1)+IINC2
      IWRTF(3)=IWRTF(2)+IINC
      IWRTF(4)=IWRTF(3)+IINC
      IWRTF(5)=IWRTF(4)+IINC
      IWRTF(6)=IWRTF(5)+IINC
      IWRTF(7)=IWRTF(6)+IINC
!
      ICNT=0
      DO I=1,NTREAT
        DO J=1,NTREAT
          IF(I.LT.J)THEN
!
            IF(ICNT.GE.MAXROW)THEN
              IFRST=.TRUE.
              ILAST=.TRUE.
              IFLAGS=.TRUE.
              IFLAGE=.TRUE.
              CALL DPDTA5(ITITLE,NCTITL,                                &
                          ITITL9,NCTIT9,ITITL2,NCTIT2,                  &
                          MAXLIN,NUMLIN,NUMCLI,NUMCOL,                  &
                          IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,        &
                          IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                          ICAPSW,ICAPTY,IFRST,ILAST,                    &
                          IFLAGS,IFLAGE,                                &
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
            ADIFF=ABS(XTMEAN(I) - XTMEAN(J))
            AMAT(ICNT,3)=ADIFF
            NII=INT(IR*NBLOCK)
            NIJ=INT(IR*NBLOCK)
            TERM1=(MSE/2.0)*(1.0/REAL(NII) + 1.0/REAL(NIJ))
            TERM1=SQRT(TERM1)
            AMAT(ICNT,4)=TERM1
            AMAT(ICNT,5)=ADIFF + CV90T*TERM1
            AMAT(ICNT,6)=ADIFF + CV95T*TERM1
            AMAT(ICNT,7)=ADIFF + CV99T*TERM1
          ENDIF
        ENDDO
      ENDDO
!
      IF(ICNT.GE.1)THEN
        IFRST=.TRUE.
        ILAST=.TRUE.
        IFLAGS=.TRUE.
        IFLAGE=.TRUE.
        CALL DPDTA5(ITITLE,NCTITL,                                &
                    ITITL9,NCTIT9,ITITL2,NCTIT2,                  &
                    MAXLIN,NUMLIN,NUMCLI,NUMCOL,                  &
                    IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,        &
                    IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                    ICAPSW,ICAPTY,IFRST,ILAST,                    &
                    IFLAGS,IFLAGE,                                &
                    ISUBRO,IBUGA3,IERROR)
      ENDIF
!
 5099 CONTINUE
!
      IF(IRCBPT.EQ.'OFF')GO TO 9000
!
!               *******************************************************
!               **   STEP 6B--                                       **
!               **   WRITE OUT EVERYTHING  FOR THE PERMUTATION TEST  **
!               *******************************************************
!
      ISTEPN='6B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RCB2')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ITITLE='Randomized Complete Block Permutation Test'
      NCTITL=42
      ITITLZ='(Critical Values Determined from Permutation Test)'
      NCTITZ=51
!
      ICNT=1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Response Variable:  '
      WRITE(ITEXT(ICNT)(22:25),'(A4)')IVARID(1:4)
      WRITE(ITEXT(ICNT)(26:29),'(A4)')IVARI2(1:4)
      NCTEXT(ICNT)=29
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Treatment Variable:  '
      WRITE(ITEXT(ICNT)(22:25),'(A4)')IVARI3(1:4)
      WRITE(ITEXT(ICNT)(26:29),'(A4)')IVARI4(1:4)
      NCTEXT(ICNT)=29
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
!
      ICNT=ICNT+1
      ITEXT(ICNT)='Block Variable:  '
      WRITE(ITEXT(ICNT)(22:25),'(A4)')IVARI5(1:4)
      WRITE(ITEXT(ICNT)(26:29),'(A4)')IVARI6(1:4)
      NCTEXT(ICNT)=29
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
      ITEXT(ICNT)='H0: The Treatments Are Equal'
      NCTEXT(ICNT)=28
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: The Treatments Are Not All Equal'
      NCTEXT(ICNT)=36
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
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
      ITEXT(ICNT)='Test:'
      NCTEXT(ICNT)=5
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Permutation Samples:'
      NCTEXT(ICNT)=30
      AVALUE(ICNT)=REAL(NITER)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Statistic Value:'
      NCTEXT(ICNT)=16
      AVALUE(ICNT)=STATVA
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Test CDF Value:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=STATCP
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Test P-Value:'
      NCTEXT(ICNT)=13
      AVALUE(ICNT)=PVALP
      IDIGIT(ICNT)=NUMDIG
!
      NUMROW=ICNT
      DO I=1,NUMROW
        NTOT(I)=15
      ENDDO
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RCB2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ITITLE='Conclusions (Upper 1-Tailed Test)'
      NCTITL=33
      ITITL9=' '
      NCTIT9=0
!
      DO J=1,NUMCLI
        DO I=1,3
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
        ENDDO
      ENDDO
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
      ITITL2(2,3)='Critical'
      NCTIT2(2,3)=8
      ITITL2(3,3)='Region (>=)'
      NCTIT2(3,3)=11
      ICNT=3
!
      ICNT=ICNT+1
      ITITL2(1,ICNT)='Null'
      NCTIT2(1,ICNT)=4
      ITITL2(2,ICNT)='Hypothesis'
      NCTIT2(2,ICNT)=10
      ITITL2(3,ICNT)='Conclusion'
      NCTIT2(3,ICNT)=10
!
      NMAX=0
      NUMCOL=4
      DO I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
        IDIGIT(I)=NUMDIG
        IF(I.EQ.1 .OR. I.EQ.ICNT)THEN
          ITYPCO(I)='ALPH'
        ENDIF
      ENDDO
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
      DO J=1,NUMALP
!
        IF(J.EQ.1)THEN
          IVALUE(J,1)='80.0%'
          NCVALU(J,1)=5
          AMAT(J,3)=P80
        ELSEIF(J.EQ.2)THEN
          IVALUE(J,1)='90.0%'
          NCVALU(J,1)=5
          AMAT(J,3)=P90
        ELSEIF(J.EQ.3)THEN
          IVALUE(J,1)='95.0%'
          NCVALU(J,1)=5
          AMAT(J,3)=P95
        ELSEIF(J.EQ.4)THEN
          IVALUE(J,1)='99.0%'
          NCVALU(J,1)=5
          AMAT(J,3)=P99
        ENDIF
        AMAT(J,2)=STATVA
        IVALUE(J,4)(1:6)='REJECT'
        IF(STATVA.LT.AMAT(J,3))THEN
          IVALUE(J,4)(1:6)='ACCEPT'
        ENDIF
        NCVALU(J,4)=6
!
      ENDDO
!
      ICNT=NUMALP
      NUMLIN=3
      IFRST=.TRUE.
      ILAST=.TRUE.
      IFLAGS=.TRUE.
      IFLAGE=.TRUE.
      CALL DPDTA5(ITITLE,NCTITL,                                &
                  ITITL9,NCTIT9,ITITL2,NCTIT2,                  &
                  MAXLIN,NUMLIN,NUMCLI,NUMCOL,                  &
                  IVALUE,NCVALU,AMAT,ITYPCO,MAXRO2,ICNT,        &
                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                  ICAPSW,ICAPTY,IFRST,ILAST,                    &
                  IFLAGS,IFLAGE,                                &
                  ISUBRO,IBUGA3,IERROR)
!
      ITITLE='Tukey Multiple Comparisons (upper-Tailed Test)'
      NCTITL=46
      ITITL9=' '
      NCTIT9=0
!
      DO II=1,3
        DO JJ=1,7
          ITITL2(II,JJ)=' '
          NCTIT2(II,JJ)=0
        ENDDO
      ENDDO
!
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
      ITITL2(3,4)='90% CV'
      NCTIT2(3,4)=6
      ITITL2(3,5)='95% CV'
      NCTIT2(3,5)=6
      ITITL2(3,6)='99% CV'
      NCTIT2(3,6)=6
      ITITL2(3,7)='99.9% CV'
      NCTIT2(3,7)=8
!
      NMAX=0
!     NUMCOL=7     FOR NOW, DO NOT INCLUDE THE 99.9% COLUMN
      NUMCOL=6
      NUMLIN=3
      DO I=1,NUMCOL
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
      ENDDO
      IWHTML(1)=50
      IWHTML(2)=50
      IWHTML(3)=125
      IWHTML(4)=125
      IWHTML(5)=125
      IWHTML(6)=125
      IWHTML(7)=125
      IINC=1600
      IINC2=200
      IINC3=1000
      IWRTF(1)=IINC2
      IWRTF(2)=IWRTF(1)+IINC2
      IWRTF(3)=IWRTF(2)+IINC
      IWRTF(4)=IWRTF(3)+IINC
      IWRTF(5)=IWRTF(4)+IINC
      IWRTF(6)=IWRTF(5)+IINC
      IWRTF(7)=IWRTF(6)+IINC
!
      ICNT=0
      DO I=1,NTREAT
        DO J=I+1,NTREAT
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
          ADIFF=ABS(XTMEAN(I) - XTMEAN(J))
          AMAT(ICNT,3)=ADIFF
          AMAT(ICNT,4)=P90Q
          AMAT(ICNT,5)=P95Q
          AMAT(ICNT,6)=P99Q
          AMAT(ICNT,7)=P999Q
        ENDDO
      ENDDO
!
      IFRST=.TRUE.
      ILAST=.TRUE.
      IFLAGS=.TRUE.
      IFLAGE=.TRUE.
      CALL DPDTA5(ITITLE,NCTITL,                                &
                  ITITL9,NCTIT9,ITITL2,NCTIT2,                  &
                  MAXLIN,NUMLIN,NUMCLI,NUMCOL,                  &
                  IVALUE,NCVALU,AMAT,ITYPCO,MAXRO2,ICNT,        &
                  IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                  ICAPSW,ICAPTY,IFRST,ILAST,                    &
                  IFLAGS,IFLAGE,                                &
                  ISUBRO,IBUGA3,IERROR)
!
 9000 CONTINUE
!
      IOP='CLOS'
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,       &
                  IBUGA3,ISUBRO,IERROR)
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RCB2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPRCB2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      RETURN
      END SUBROUTINE DPRCB2
      SUBROUTINE DPRCB3(Y,TREAT,BLOCK,N,                           &
                        DBLOCK,DTREAT,TEMP1,XCELL,XTMEAN,XBMEAN,   &
                        MAXNXT,MAXNX2,                             &
                        STATVA,STATCD,PVAL,IREPL,IR,               &
                        NBLOCK,NTREAT,NUMDF1,NUMDF2,               &
                        XGRAND,SSTR,SSB,SSE,SSTOT,SSI,             &
                        MSTR,MSB,MSE,MSI,                          &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS ROUTINE PERFORMS A PARAMETRIC ANALYSIS OF A
!              RANDOMIZED COMPLETE BLOCK DESIGN (RCBD).  NOTE THAT
!              A CRBD ANALYSIS IS ESSENTIALLY A TWO-WAY ANOVA WHERE
!              ONE OF THE FACTORS IS ESSENTIALLY A NUISANCE FACTOR
!              (THE BLOCKING FACTOR).  FOR EXAMPLE, IF TESTING A
!              FERTILZER, SOIL TYPE MAY BE USED AS A BLOCKING FACTOR
!              (I.E., A BLOCK WILL CONTAIN THE TREATMENTS WITH SIMILAR
!              SOIL TYPE.
!
!              WE HAVE PUT THIS IN A SEPARATE ROUTINE FROM ANOVA TO
!              ALLOW A PERMUTATION IMPLEMENTATION.  CURRENTLY THE
!              PERMUTATION TEST IS ONLY SUPPORTED FOR THE CASE WHERE
!              THERE IS NO REPLICATION.
!
!              THIS VERSION RETURNS THE CDF AND P-VALUE BASED ON THE
!              PARAMETRIC F-TEST.  DPCRB2 ALSO PERFORMS A PERMUTATION
!              BASED TEST.
!
!     EXAMPLE--RANDOMIZED COMPLETE BLOCK Y TREAT BLOCK
!     REFERENCE--HIGGINS (2004), "INTRODUCTION TO MODERN NONPARAMETRIC
!                STATISTICS", DUXBURY PRESS, CHAPTER 4.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2024/03
!     ORIGINAL VERSION--JUNE      2025.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IREPL
!
      REAL MSTR
      REAL MSB
      REAL MSE
      REAL MSI
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION BLOCK(*)
      DIMENSION TREAT(*)
      DIMENSION DBLOCK(*)
      DIMENSION DTREAT(*)
      DIMENSION TEMP1(*)
      DIMENSION XCELL(MAXNX2,MAXNX2)
      DIMENSION XTMEAN(MAXNX2)
      DIMENSION XBMEAN(MAXNX2)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPRC'
      ISUBN2='B3  '
      IERROR='NO'
      IWRITE='OFF'
      IREPL='OFF'
!
      STATVA=CPUMIN
      STATCD=CPUMIN
      PVAL=CPUMIN
      XGRAND=CPUMIN
      SSTOT=0.0
      SSTR=0.0
      SSE=0.0
      SSB=0.0
      SSI=0.0
      MSE=0.0
      MSTR=0.0
      MSB=0.0
      MSI=0.0
      IR=1
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RCB3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPRCB3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N,MAXNXT
   52   FORMAT('IBUGA3,ISUBRO,N,MAXNXT = ',2(A4,2X),2I8)
        CALL DPWRST('XXX','WRIT')
        DO I=1,N
          WRITE(ICOUT,57)I,Y(I),BLOCK(I),TREAT(I)
   57     FORMAT('I,Y(I),BLOCK(I),TREAT(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','WRIT')
        ENDDO
      ENDIF
!
!               ********************************************
!               **  STEP 11--                             **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RCB3')   &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      HOLD=Y(1)
      DO 1135 I=2,N
      IF(Y(I).NE.HOLD)GO TO 1139
 1135 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1131)
 1131 FORMAT('***** ERROR FROM RANDOMIZED COMPLETE BLOCK TEST (DPRCB3)--')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1133)HOLD
 1133 FORMAT('      THE RESPONSE VARIABLE HAS ALL ELEMENTS = ',G15.7)
      CALL DPWRST('XXX','WRIT')
      IERROR='YES'
      GO TO 9000
 1139 CONTINUE
!
      HOLD=BLOCK(1)
      DO 1235 I=2,N
      IF(BLOCK(I).NE.HOLD)GO TO 1239
 1235 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1131)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1231)HOLD
 1231 FORMAT('      THE FIRST FACTOR VARIABLE HAS ALL ELEMENTS = ',   &
             G15.7)
      CALL DPWRST('XXX','WRIT')
      IERROR='YES'
      GO TO 9000
 1239 CONTINUE
!
      HOLD=TREAT(1)
      DO 1335 I=2,N
      IF(TREAT(I).NE.HOLD)GO TO 1339
 1335 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1131)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1331)HOLD
 1331 FORMAT('      THE SECOND FACTOR VARIABLE HAS ALL ELEMENTS = ',   &
             G15.7)
      CALL DPWRST('XXX','WRIT')
      IERROR='YES'
      GO TO 9000
 1339 CONTINUE
!
!               ******************************************************
!               **  STEP 2--                                        **
!               **  DETERMINE NUMBER OF DISTINCT BLOCKS AND NUMBER  **
!               **  OF DISTINCT TREATMENTS                          **
!               ******************************************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RCB3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DISTIN(BLOCK,N,IWRITE,DBLOCK,NBLOCK,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      CALL SORT(DBLOCK,NBLOCK,DBLOCK)
      IF(NBLOCK.GT.MAXNX2 .OR. NBLOCK.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1131)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1232)NBLOCK,MAXNX2
 1232   FORMAT('      THE NUMBER OF BLOCKS (',I8,') IS LESS THAN ',  &
               'ONE OR GREATER THAN',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      CALL DISTIN(TREAT,N,IWRITE,DTREAT,NTREAT,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      CALL SORT(DTREAT,NTREAT,DTREAT)
      IF(NTREAT.GT.MAXNX2 .OR. NTREAT.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1131)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1237)NTREAT,MAXNX2
 1237   FORMAT('      THE NUMBER OF TREATMENTS (',I8,') IS LESS ',   &
               'THAN 2 OR GREATER THAN ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      IVAL=NTREAT*NBLOCK
      IF(IVAL.EQ.N)THEN
        IR=1
        IREPL='OFF'
      ELSE
        IREM=MOD(N,IVAL)
        IF(IREM.EQ.0)THEN
          IR=N/IVAL
          IREPL='ON'
        ELSE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1131)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1242)
 1242     FORMAT('      UNBALANCED REPLICATIONS CASE DETECTED.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1244)NTREAT
 1244     FORMAT('      THE NUMBER OF TREATMENTS    = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1246)NBLOCK
 1246     FORMAT('      THE NUMBER OF BLOCKS        = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1248)N
 1248     FORMAT('      THE NUMBER OF OBSERVATIONS  = ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
      ENDIF
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RCB3')THEN
        WRITE(ICOUT,1336)NTREAT,NBLOCK,IR
 1336   FORMAT('NTREAT,NBLOCK,IR = ',3I6)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
!               **************************************************
!               **  STEP 3--                                    **
!               **  COMPUTE TREATMENT AND BLOCK MEANS AND CHECK **
!               **  THAT THERE ARE NO EMPTY CELLS.              **
!               **************************************************
!
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RCB3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL MEAN(Y,N,IWRITE,XGRAND,IBUGA3,IERROR)
      SUM3=0.0D0
      NTEMP1=0
      NTEMP3=0
      XBMEAN(1:NBLOCK)=0.0
      XTMEAN(1:NTREAT)=0.0
!
      DO II=1,NTREAT
        HOLD1=DTREAT(II)
        XTMEAN(II)=0.0
        ICNTT=0
        DO JJ=1,NBLOCK
          HOLD2=DBLOCK(JJ)
          ICNTB=0
          NTEMP=0
          DO KK=1,N
            IF(TREAT(KK).EQ.HOLD1 .AND. BLOCK(KK).EQ.HOLD2)THEN
              NTEMP=NTEMP+1
              TEMP1(NTEMP)=Y(KK)
            ENDIF
          ENDDO
!
          IF(NTEMP.LT.1)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1131)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1337)HOLD1,HOLD2
 1337       FORMAT('      TREATMENT ',G12.3,' AND BLOCK ',G12.3,   &
                   'IS EMPTY.')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ELSEIF(NTEMP.NE.IR)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1131)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1347)HOLD1,HOLD2,NTEMP
 1347       FORMAT('      TREATMENT ',G12.3,' AND BLOCK ',G12.3,   &
                   'HAS ',I6,' OBSERVATIONS.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1349)IR
 1349       FORMAT('      NUMBER OF OBSERVATIONS EXPECTED = ',I6)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
!
!         SUM THE VALUES IN THE CELL
!
          ASUM=0.0
          IF(NTEMP.EQ.1)THEN
            ASUM=TEMP1(1)
          ELSE
            CALL SUMDP(TEMP1,NTEMP,IWRITE,ASUM,IBUGA3,IERROR)
          ENDIF
          XCELL(II,JJ)=ASUM
!
          XTMEAN(II)=XTMEAN(II)+ASUM
          SUM3=SUM3+ASUM
!
        ENDDO
        XTMEAN(II)=XTMEAN(II)/REAL(IR*NBLOCK)
!
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RCB3')THEN
          WRITE(ICOUT,1353)II,XTMEAN(II)
 1353     FORMAT('II,XTMEAN(II) = ',I6,G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
      ENDDO
!
      DO JJ=1,NBLOCK
        XBMEAN(JJ)=0.0
        SUM1=0.0
        DO II=1,NTREAT
          SUM1=SUM1 + XCELL(II,JJ)
!
          IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RCB3')THEN
            WRITE(ICOUT,1426)JJ,II,XCELL(II,JJ)
 1426       FORMAT('JJ,II,XCELL(II,JJ) = ',2I6,G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
        ENDDO
        XBMEAN(JJ)=SUM1/REAL(NTREAT*IR)
!
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RCB3')THEN
          WRITE(ICOUT,1423)JJ,SUM1,XBMEAN(JJ)
 1423     FORMAT('JJ,SUM1,XBMEAN(JJ) = ',I6,2G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
      ENDDO
!
      DO JJ=1,NBLOCK
         DO II=1,NTREAT
            XCELL(II,JJ)=XCELL(II,JJ)/REAL(IR)
!
            IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RCB3')THEN
              WRITE(ICOUT,1433)JJ,II,XCELL(II,JJ)
 1433         FORMAT('JJ,II,XCELL(II,JJ) = ',2I6,G15.7)
              CALL DPWRST('XXX','BUG ')
            ENDIF
!
         ENDDO
      ENDDO
!
      NTOT=NTREAT*NBLOCK*IR
      SSTOT=0.0
      SSTR=0.0
      SSB=0.0
      SSE=0.0
      SSI=0.0
      DO II=1,N
        SSTOT=SSTOT + (Y(II) - XGRAND)**2
      ENDDO
      DO II=1,NBLOCK
        SSB=SSB + (XBMEAN(II) - XGRAND)**2
      ENDDO
      SSB=REAL(NTREAT*IR)*SSB
      DO II=1,NTREAT
        SSTR=SSTR + (XTMEAN(II) - XGRAND)**2
      ENDDO
      SSTR=REAL(NBLOCK*IR)*SSTR
      IF(IR.GT.1)THEN
        DO II=1,NTREAT
          DO JJ=1,NBLOCK
            SSI=SSI + (XCELL(II,JJ) - XBMEAN(JJ) - XTMEAN(II) + XGRAND)**2
          ENDDO
        ENDDO
        SSI=REAL(IR)*SSI
        MSI=SSI/(REAL(NTREAT-1)*REAL(NBLOCK-1))
        SSE=SSTOT - SSTR - SSB - SSI
        MSE=SSE/REAL(N - NBLOCK*NTREAT)
      ELSE
        DO II=1,NTREAT
          DO JJ=1,NBLOCK
            SSE=SSE + (XCELL(II,JJ) - XBMEAN(JJ) - XTMEAN(II) + XGRAND)**2
          ENDDO
        ENDDO
        MSE=SSE/(REAL(NTREAT-1)*REAL(NBLOCK-1))
        SSI=0.0
        MSI=0.0
      ENDIF
      MSTR=SSTR/REAL(NTREAT-1)
      MSB=SSB/REAL(NBLOCK-1)
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RCB3')THEN
        WRITE(ICOUT,1355)NTOT,XGRAND,SSTOT,SSTR,SSB,SSI,SSE
 1355   FORMAT('NTOT,XGRAND,SSTOT,SSTR,SSB,SSI,SSE = ',I6,6G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1356)MSTR,MSB,MSI,MSE
 1356   FORMAT('MSTR,MSB,MSI,MSE = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        DO KK=1,NTREAT
          WRITE(ICOUT,1358)KK,XTMEAN(KK)
 1358     FORMAT('KK,XTMEAN(KK) = ',I6,G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDDO
        DO KK=1,NBLOCK
          WRITE(ICOUT,1368)KK,XBMEAN(KK)
 1368     FORMAT('KK,XBMEAN(KK) = ',I6,G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDDO
      ENDIF
!
!               **************************************************
!               **  STEP 4--                                    **
!               **  COMPUTE F STATISTIC AND CDF AND P-VALUE     **
!               **************************************************
!
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RCB3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NUMDF1=NTREAT-1
      IF(IR.EQ.1)THEN
        NUMDF2=(NBLOCK-1)*(NTREAT-1)
      ELSE
        NUMDF2=N - NBLOCK*NTREAT
      ENDIF
      STATVA=MSTR/MSE
      CALL FCDF(STATVA,NUMDF1,NUMDF2,STATCD)
      PVAL=1.0 - STATCD
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RCB3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPRCB3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9012)NUMDF1,NUMDF2,STATVA,STATCD,PVAL
 9012   FORMAT('NUMDF1,NUMDF2,STATVA,STATCD,PVAL = ',2I8,3G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9014)IREPL,IR
 9014   FORMAT('IREPL,IR = ',A4,2X,I4)
        CALL DPWRST('XXX','WRIT')
        DO II=1,NTREAT
          DO JJ=1,NBLOCK
            WRITE(ICOUT,9034)II,JJ,XCELL(II,JJ)
 9034       FORMAT('II,JJ,XCELL(II,JJ) = ',2I8,G15.7)
            CALL DPWRST('XXX','WRIT')
          ENDDO
        ENDDO
      ENDIF
!
      RETURN
      END SUBROUTINE DPRCB3
      SUBROUTINE DPRCIL(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
                        IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR ROMAN COMPLEX ITALIC LOWER CASE.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
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
   51 FORMAT('***** AT THE BEGINNING OF DPRCIL--')
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
      IF(ICHARN.LE.10)GO TO 1010
      GO TO 1019
 1010 CONTINUE
      CALL DRCIL1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
      GO TO 9000
 1019 CONTINUE
!
      IF(11.LE.ICHARN.AND.ICHARN.LE.20)GO TO 1020
      GO TO 1029
 1020 CONTINUE
      CALL DRCIL2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
      GO TO 9000
 1029 CONTINUE
!
      IF(ICHARN.GE.21)GO TO 1030
      GO TO 1039
 1030 CONTINUE
      CALL DRCIL3(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
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
      END SUBROUTINE DPRCIL
      SUBROUTINE DPRCIN(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
                        IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR ROMAN COMPLEX ITALIC NUMERIC.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
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
   51 FORMAT('***** AT THE BEGINNING OF DPRCIN--')
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
      CALL DRCIN1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
      GO TO 9000
 1019 CONTINUE
!
      IF(ICHARN.GE.9)GO TO 1020
      GO TO 1029
 1020 CONTINUE
      CALL DRCIN2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
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
      END SUBROUTINE DPRCIN
      SUBROUTINE DPRCIU(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
                        IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR ROMAN COMPLEX ITALIC UPPER CASE.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
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
   51 FORMAT('***** AT THE BEGINNING OF DPRCIU--')
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
      CALL DRCIU1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
      GO TO 9000
 1019 CONTINUE
!
      IF(ICHARN.GE.15)GO TO 1020
      GO TO 1029
 1020 CONTINUE
      CALL DRCIU2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
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
      END SUBROUTINE DPRCIU
      SUBROUTINE DPRCL(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
                       IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR ROMAN COMPLEX LOWER CASE.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
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
   51 FORMAT('***** AT THE BEGINNING OF DPRCL--')
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
      IF(ICHARN.LE.12)GO TO 1010
      GO TO 1019
 1010 CONTINUE
      CALL DRCL1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
      GO TO 9000
 1019 CONTINUE
!
      IF(ICHARN.GE.13)GO TO 1020
      GO TO 1029
 1020 CONTINUE
      CALL DRCL2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
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
      END SUBROUTINE DPRCL
      SUBROUTINE DPRCN(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
                       IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR ROMAN COMPLEX NUMERIC.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
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
   51 FORMAT('***** AT THE BEGINNING OF DPRCN--')
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
      CALL DRCN1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
      GO TO 9000
 1019 CONTINUE
!
      IF(ICHARN.GE.10)GO TO 1020
      GO TO 1029
 1020 CONTINUE
      CALL DRCN2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
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
      END SUBROUTINE DPRCN
      SUBROUTINE DPRCS(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
                       IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR ROMAN COMPLEX SYMBOLS.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--87/4
!     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MARCH     1981.
!     UPDATED         --MARCH     1982.
!     UPDATED         --MAY       1982.
!     UPDATED         --MARCH     1987.
!     UPDATED         --MAY       1987.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ICHAR2
      CHARACTER*4 IOP
      CHARACTER*4 IBUGD2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IOPERA
!
!---------------------------------------------------------------------
!
      DIMENSION IOP(*)
      DIMENSION X(*)
      DIMENSION Y(*)
!
      DIMENSION IOPERA(300)
      DIMENSION IX(300)
      DIMENSION IY(300)
!
      DIMENSION IXMIND(30)
      DIMENSION IXMAXD(30)
      DIMENSION IXDELD(30)
      DIMENSION ISTARD(30)
      DIMENSION NUMCOO(30)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
!     DEFINE CHARACTER   2210--. (PERIOD)
!
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',   0,  -7/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',  -1,  -8/
      DATA IOPERA(   3),IX(   3),IY(   3)/'DRAW',   0,  -9/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',   1,  -8/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',   0,  -7/
!
      DATA IXMIND(   1)/  -5/
      DATA IXMAXD(   1)/   5/
      DATA IXDELD(   1)/  10/
      DATA ISTARD(   1)/   1/
      DATA NUMCOO(   1)/   5/
!
!     DEFINE CHARACTER   2211--, (COMMA)
!
      DATA IOPERA(   6),IX(   6),IY(   6)/'MOVE',   0,  -9/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',  -1,  -8/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',   0,  -7/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',   1,  -8/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',   1, -10/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',   0, -12/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',  -1, -13/
!
      DATA IXMIND(   2)/  -5/
      DATA IXMAXD(   2)/   5/
      DATA IXDELD(   2)/  10/
      DATA ISTARD(   2)/   6/
      DATA NUMCOO(   2)/   7/
!
!     DEFINE CHARACTER   2212--: (COLON)
!
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
!
      DATA IXMIND(   3)/  -5/
      DATA IXMAXD(   3)/   5/
      DATA IXDELD(   3)/  10/
      DATA ISTARD(   3)/  13/
      DATA NUMCOO(   3)/  10/
!
!     DEFINE CHARACTER   2213--; (SEMICOLON)
!
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
!
      DATA IXMIND(   4)/  -5/
      DATA IXMAXD(   4)/   5/
      DATA IXDELD(   4)/  10/
      DATA ISTARD(   4)/  23/
      DATA NUMCOO(   4)/  12/
!
!     DEFINE CHARACTER   2214--! (EXCLAMATION POINT)
!
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
!
      DATA IXMIND(   5)/  -5/
      DATA IXMAXD(   5)/   5/
      DATA IXDELD(   5)/  10/
      DATA ISTARD(   5)/  35/
      DATA NUMCOO(   5)/  12/
!
!     DEFINE CHARACTER   2215--? (QUESTION MARK)
!
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
!
      DATA IXMIND(   6)/  -9/
      DATA IXMAXD(   6)/   9/
      DATA IXDELD(   6)/  18/
      DATA ISTARD(   6)/  47/
      DATA NUMCOO(   6)/  29/
!
!     DEFINE CHARACTER   2272--& (AMPERSAND)
!
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
!
      DATA IXMIND(   7)/ -12/
      DATA IXMAXD(   7)/  13/
      DATA IXDELD(   7)/  25/
      DATA ISTARD(   7)/  76/
      DATA NUMCOO(   7)/  46/
!
!     DEFINE CHARACTER   2274--$ (DOLLAR SIGN)
!
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
!
      DATA IXMIND(   8)/ -10/
      DATA IXMAXD(   8)/  10/
      DATA IXDELD(   8)/  20/
      DATA ISTARD(   8)/ 122/
      DATA NUMCOO(   8)/  38/
!
!     DEFINE CHARACTER   2220--/ (SLASH)
!
      DATA IOPERA( 160),IX( 160),IY( 160)/'MOVE',   9,  16/
      DATA IOPERA( 161),IX( 161),IY( 161)/'DRAW',  -9, -16/
!
      DATA IXMIND(   9)/ -11/
      DATA IXMAXD(   9)/  11/
      DATA IXDELD(   9)/  22/
      DATA ISTARD(   9)/ 160/
      DATA NUMCOO(   9)/   2/
!
!     DEFINE CHARACTER   2221--( (LEFT PARENTHESES)
!
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
!
      DATA IXMIND(  10)/  -7/
      DATA IXMAXD(  10)/   7/
      DATA IXDELD(  10)/  14/
      DATA ISTARD(  10)/ 162/
      DATA NUMCOO(  10)/  18/
!
!     DEFINE CHARACTER   2222--) (RIGHT PARENTHESES)
!
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
!
      DATA IXMIND(  11)/  -7/
      DATA IXMAXD(  11)/   7/
      DATA IXDELD(  11)/  14/
      DATA ISTARD(  11)/ 180/
      DATA NUMCOO(  11)/  18/
!
!     DEFINE CHARACTER   2219--* (ASTERISK)
!
      DATA IOPERA( 198),IX( 198),IY( 198)/'MOVE',   0,  12/
      DATA IOPERA( 199),IX( 199),IY( 199)/'DRAW',   0,   0/
      DATA IOPERA( 200),IX( 200),IY( 200)/'MOVE',  -5,   9/
      DATA IOPERA( 201),IX( 201),IY( 201)/'DRAW',   5,   3/
      DATA IOPERA( 202),IX( 202),IY( 202)/'MOVE',   5,   9/
      DATA IOPERA( 203),IX( 203),IY( 203)/'DRAW',  -5,   3/
!
      DATA IXMIND(  12)/  -8/
      DATA IXMAXD(  12)/   8/
      DATA IXDELD(  12)/  16/
      DATA ISTARD(  12)/ 198/
      DATA NUMCOO(  12)/   6/
!
!     DEFINE CHARACTER   2231--- (HYPHEN OR MINUS SIGN)
!
      DATA IOPERA( 204),IX( 204),IY( 204)/'MOVE',  -9,   0/
      DATA IOPERA( 205),IX( 205),IY( 205)/'DRAW',   9,   0/
!
      DATA IXMIND(  13)/ -13/
      DATA IXMAXD(  13)/  13/
      DATA IXDELD(  13)/  26/
      DATA ISTARD(  13)/ 204/
      DATA NUMCOO(  13)/   2/
!
!     DEFINE CHARACTER   2232--+ (PLUS SIGN)
!
      DATA IOPERA( 206),IX( 206),IY( 206)/'MOVE',   0,   9/
      DATA IOPERA( 207),IX( 207),IY( 207)/'DRAW',   0,  -9/
      DATA IOPERA( 208),IX( 208),IY( 208)/'MOVE',  -9,   0/
      DATA IOPERA( 209),IX( 209),IY( 209)/'DRAW',   9,   0/
!
      DATA IXMIND(  14)/ -13/
      DATA IXMAXD(  14)/  13/
      DATA IXDELD(  14)/  26/
      DATA ISTARD(  14)/ 206/
      DATA NUMCOO(  14)/   4/
!
!     DEFINE CHARACTER   2238--= (EQUAL SIGN)
!
      DATA IOPERA( 210),IX( 210),IY( 210)/'MOVE',  -9,   3/
      DATA IOPERA( 211),IX( 211),IY( 211)/'DRAW',   9,   3/
      DATA IOPERA( 212),IX( 212),IY( 212)/'MOVE',  -9,  -3/
      DATA IOPERA( 213),IX( 213),IY( 213)/'DRAW',   9,  -3/
!
      DATA IXMIND(  15)/ -13/
      DATA IXMAXD(  15)/  13/
      DATA IXDELD(  15)/  26/
      DATA ISTARD(  15)/ 210/
      DATA NUMCOO(  15)/   4/
!
!     DEFINE CHARACTER   2216--' (SINGLE QUOTE)
!
      DATA IOPERA( 214),IX( 214),IY( 214)/'MOVE',   0,  12/
      DATA IOPERA( 215),IX( 215),IY( 215)/'DRAW',  -1,   5/
      DATA IOPERA( 216),IX( 216),IY( 216)/'MOVE',   1,  12/
      DATA IOPERA( 217),IX( 217),IY( 217)/'DRAW',  -1,   5/
!
      DATA IXMIND(  16)/  -4/
      DATA IXMAXD(  16)/   4/
      DATA IXDELD(  16)/   8/
      DATA ISTARD(  16)/ 214/
      DATA NUMCOO(  16)/   4/
!
!     DEFINE CHARACTER   2217--  (DOUBLE QUOTE)
!
      DATA IOPERA( 218),IX( 218),IY( 218)/'MOVE',  -4,  12/
      DATA IOPERA( 219),IX( 219),IY( 219)/'DRAW',  -5,   5/
      DATA IOPERA( 220),IX( 220),IY( 220)/'MOVE',  -3,  12/
      DATA IOPERA( 221),IX( 221),IY( 221)/'DRAW',  -5,   5/
      DATA IOPERA( 222),IX( 222),IY( 222)/'MOVE',   4,  12/
      DATA IOPERA( 223),IX( 223),IY( 223)/'DRAW',   3,   5/
      DATA IOPERA( 224),IX( 224),IY( 224)/'MOVE',   5,  12/
      DATA IOPERA( 225),IX( 225),IY( 225)/'DRAW',   3,   5/
!
      DATA IXMIND(  17)/  -8/
      DATA IXMAXD(  17)/   8/
      DATA IXDELD(  17)/  16/
      DATA ISTARD(  17)/ 218/
      DATA NUMCOO(  17)/   8/
!
!     DEFINE CHARACTER   2218--  (DEGREES)
!
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
!
      DATA IXMIND(  18)/  -7/
      DATA IXMAXD(  18)/   7/
      DATA IXDELD(  18)/  14/
      DATA ISTARD(  18)/ 226/
      DATA NUMCOO(  18)/  13/
!
!     DEFINE CHARACTER   2747--  (NO   SPACE BLANK)
!
      DATA IOPERA( 239),IX( 239),IY( 239)/'MOVE',   0, -32/
      DATA IOPERA( 240),IX( 240),IY( 240)/'MOVE',   0, -32/
!
      DATA IXMIND(  19)/   0/
      DATA IXMAXD(  19)/   0/
      DATA IXDELD(  19)/   0/
      DATA ISTARD(  19)/ 239/
      DATA NUMCOO(  19)/   2/
!
!     DEFINE CHARACTER   2748--  (HALF SPACE BLANK)
!
      DATA IOPERA( 241),IX( 241),IY( 241)/'MOVE',  -4, -32/
      DATA IOPERA( 242),IX( 242),IY( 242)/'MOVE',   4, -32/
!
      DATA IXMIND(  20)/  -4/
      DATA IXMAXD(  20)/   4/
      DATA IXDELD(  20)/   8/
      DATA ISTARD(  20)/ 241/
      DATA NUMCOO(  20)/   2/
!
!     DEFINE CHARACTER   2749--  (FULL SPACE BLANK)
!
      DATA IOPERA( 243),IX( 243),IY( 243)/'MOVE',  -8, -32/
      DATA IOPERA( 244),IX( 244),IY( 244)/'MOVE',   8, -32/
!
      DATA IXMIND(  21)/  -8/
      DATA IXMAXD(  21)/   8/
      DATA IXDELD(  21)/  16/
      DATA ISTARD(  21)/ 243/
      DATA NUMCOO(  21)/   2/
!
!     DEFINE CHARACTER   2252--  (LEFT  APOSTRAPHE)
!
      DATA IOPERA( 245),IX( 245),IY( 245)/'MOVE',   1,  12/
      DATA IOPERA( 246),IX( 246),IY( 246)/'DRAW',   0,  11/
      DATA IOPERA( 247),IX( 247),IY( 247)/'DRAW',  -1,   9/
      DATA IOPERA( 248),IX( 248),IY( 248)/'DRAW',  -1,   7/
      DATA IOPERA( 249),IX( 249),IY( 249)/'DRAW',   0,   6/
      DATA IOPERA( 250),IX( 250),IY( 250)/'DRAW',   1,   7/
      DATA IOPERA( 251),IX( 251),IY( 251)/'DRAW',   0,   8/
!
      DATA IXMIND(  22)/  -5/
      DATA IXMAXD(  22)/   5/
      DATA IXDELD(  22)/  10/
      DATA ISTARD(  22)/ 245/
      DATA NUMCOO(  22)/   7/
!
!     DEFINE CHARACTER   2251--  (RIGHT APOSTRAPHE)
!
      DATA IOPERA( 252),IX( 252),IY( 252)/'MOVE',   0,  10/
      DATA IOPERA( 253),IX( 253),IY( 253)/'DRAW',  -1,  11/
      DATA IOPERA( 254),IX( 254),IY( 254)/'DRAW',   0,  12/
      DATA IOPERA( 255),IX( 255),IY( 255)/'DRAW',   1,  11/
      DATA IOPERA( 256),IX( 256),IY( 256)/'DRAW',   1,   9/
      DATA IOPERA( 257),IX( 257),IY( 257)/'DRAW',   0,   7/
      DATA IOPERA( 258),IX( 258),IY( 258)/'DRAW',  -1,   6/
!
      DATA IXMIND(  23)/  -5/
      DATA IXMAXD(  23)/   5/
      DATA IXDELD(  23)/  10/
      DATA ISTARD(  23)/ 252/
      DATA NUMCOO(  23)/   7/
!
!     DEFINE CHARACTER    XXX--| (KEYBOARD VERTICAL BAR)
!
      DATA IOPERA( 259),IX( 259),IY( 259)/'MOVE',   0,  12/
      DATA IOPERA( 260),IX( 260),IY( 260)/'DRAW',   0,  -9/
!
!
      DATA IXMIND(  24)/  -4/
      DATA IXMAXD(  24)/   4/
      DATA IXDELD(  24)/   8/
      DATA ISTARD(  24)/ 259/
      DATA NUMCOO(  24)/   2/
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
!               ******************************************
!               **  TREAT THE ROMAN SIMPLEX UPPER CASE  **
!               **  HERSHEY CHARACTER SET CASE          **
!               ******************************************
!               ******************************************
!
!
      IF(IBUGD2.EQ.'OFF')GO TO 90
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
!
!               **************************************************
!               **************************************************
!               **  STEP 1--                                    **
!               **  SEARCH FOR THE INPUT CHARACTER(S).          **
!               **  MAP THE CHARACTER(S) INTO A NUMERIC VALUE.  **
!               **************************************************
!               **************************************************
!
      CALL DPCHSY(ICHAR2,ICHARN,IBUGD2,IFOUND)
      IF(IFOUND.EQ.'NO')GO TO 9000
      GO TO 1000
!
!               **************************************
!               **************************************
!               **  STEP 2--                        **
!               **  EXTRACT THE COORDINATES         **
!               **  FOR THIS PARTICULAR CHARACTER.  **
!               **************************************
!               **************************************
!
 1000 CONTINUE
      ISTART=ISTARD(ICHARN)
      NC=NUMCOO(ICHARN)
      ISTOP=ISTART+NC-1
      J=0
      DO 1100 I=ISTART,ISTOP
      J=J+1
      IOP(J)=IOPERA(I)
      X(J)=IX(I)
      Y(J)=IY(I)
 1100 CONTINUE
      NUMCO=J
      IXMINS=IXMIND(ICHARN)
      IXMAXS=IXMAXD(ICHARN)
      IXDELS=IXDELD(ICHARN)
!
      GO TO 9000
!
!               *****************
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!               *****************
!
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GO TO 9090
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
      END SUBROUTINE DPRCS
      SUBROUTINE DPRCSL(ICHAR2,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
                        IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR ROMAN COMPLEX SCRIPT LOWER CASE.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
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
   51 FORMAT('***** AT THE BEGINNING OF DPRCSL--')
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
      IF(ICHARN.LE.12)GO TO 1010
      GO TO 1019
 1010 CONTINUE
      CALL DRCSL1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
      GO TO 9000
 1019 CONTINUE
!
      IF(13.LE.ICHARN.AND.ICHARN.LE.23)GO TO 1020
      GO TO 1029
 1020 CONTINUE
      CALL DRCSL2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
      GO TO 9000
 1029 CONTINUE
!
      IF(ICHARN.GE.24)GO TO 1030
      GO TO 1039
 1030 CONTINUE
      CALL DRCSL3(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
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
      END SUBROUTINE DPRCSL

      SUBROUTINE DPFACT(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                  IANGLU,MAXNPP,MAXNXT,
     1                  ICONT,NUMHPP,NUMVPP,IMANUF,
     1                  XMATN,YMATN,XMITN,YMITN,
     1                  ISQUAR,IVGMSW,IHGMSW,
     1                  IMPSW,IMPNR,IMPNC,IMPCO,
     1                  PMXMIN,PMXMAX,PMYMIN,PMYMAX,
     1                  ALOWFR,ALOWDG,IFORSW,
     1                  ANOPL1,ANOPL2,ISEED,IBOOSS,BARHEF,BARWEF,
     1                  ICAPSW,
     1                  IBUGG2,IBUGG3,IBUGCO,IBUGEV,IBUGQ,
     1                  IBUGUG,IBUGU2,IBUGU3,IBUGU4,ISUBRO,
     1                  IFOUND,IERROR)
C
C     PURPOSE--GENERATE A FACTOR PLOT.  THAT IS,
C
C                 FACTOR PLOT Y X1 X2 X3 X4 X5 X6
C
C              PLOTS Y VS X1, Y VS X2, ETC. AS A MULTIPLOT ON
C              A SINGLE PAGE.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--99/10
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--OCTOBER   1999.
C     UPDATED       --AUGUST      2007. CALL LIST TO MAINGR
C     UPDATED       --JUNE        2014. WRITE YPLOT, XPLOT, TAGPLOT TO
C                                       DPST5F.DAT
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES---------------
C
      INCLUDE 'DPCOPA.INC'
C
      CHARACTER*4 ICASPL
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICONT
      CHARACTER*4 IPOWE
      CHARACTER*4 IAND1
      CHARACTER*4 IAND2
      CHARACTER*4 IANGLU
      CHARACTER*4 IFORSW
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
C
      CHARACTER*4 IEMPTY
      CHARACTER*4 ISQUAR
      CHARACTER*4 IVGMSW
      CHARACTER*4 IHGMSW
      CHARACTER*4 IREPCH
      CHARACTER*4 IMPSW
      CHARACTER*4 IFEED9
      CHARACTER*4 IMANUF
C
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
      CHARACTER*4 IFPLLD
      CHARACTER*4 IFPLDI
      CHARACTER*4 ILFLAX
      CHARACTER*4 ILFLAY
      CHARACTER*4 IFPLSV
      CHARACTER*4 ISUBSZ
C
      CHARACTER*4 IOP
      CHARACTER*4 IFITA2
C
      CHARACTER*4 IPLOTT
      CHARACTER*4 ICT
      CHARACTER*4 IC2T
      CHARACTER*4 IHT(5)
      CHARACTER*4 IH2T(5)
C
C  MAXY IS THE MAXIMUM NUMBER OF VARIABLES TO USE IN CREATING THE
C  FACTOR PLOT   CURVE
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
      CHARACTER*4 IHRIGH
      CHARACTER*4 IHRIG2
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
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
      ISUBN1='DPFA'
      ISUBN2='CT  '
      ICASPL='FACT'
      IFPLLD='ON'
      IFPLDI='LINE'
      ICT=' '
      IFLAGV=5
      NCCOMM=0
C
C     WRITE XPLOT, YPLOT, TAGPLOT TO "dpst5f.dat"
C
      IOP='OPEN'
      IFLG11=0
      IFLG21=0
      IFLG31=0
      IFLAG4=0
      IFLAG5=1
      CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,
     1            IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1            IBUGG2,ISUBRO,IERROR)
      ICNTPL=0
      IFITA2=IFITAU
      IFITAU='OFF'
      IF(IERROR.EQ.'YES')GOTO9000
C
C               *****************************************
C               **  TREAT THE FACTOR PLOT   CASE       **
C               *****************************************
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'FACT')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPFACT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASPL,IAND1,IAND2,NUMARG,MAXY
   52   FORMAT('ICASPL,IAND1,IAND2,NUMARG,MAXY = ',3(A4,2X),2I8)
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'FACT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PLOT')THEN
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGG2,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
      ENDIF
      ICOM='PLOT'
      ICOM2='    '
      IFOUND='YES'
C
C               *******************************************************
C               **  STEP 2--                                         **
C               **  CHECK FOR THE PROPER NUMBER OF INPUT ARGUMENTS.  **
C               *******************************************************
C
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'FACT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='FACTOR PLOT'
      MINNA=1
      MAXNA=100
      MINN2=2
      IFLAGE=1
      IF(IFPLPT.EQ.'HIST')IFLAGE=0
      IF(IFPLPT.EQ.'PERC')IFLAGE=0
      IF(IFPLPT.EQ.'RUNS')IFLAGE=0
      IF(IFPLPT.EQ.'SPEC')IFLAGE=0
      IF(IFPLPT.EQ.'LAG ')IFLAGE=0
      IF(IFPLPT.EQ.'AUTO')IFLAGE=0
      IF(IFPLPT.EQ.'KERN')IFLAGE=0
      IFLAGM=1
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'FACT')THEN
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
C               **   STEP 1--                                   **
C               **   SAVE INITIAL SETTINGS                      **
C               **************************************************
C
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'FACT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IFLAG=1
      IFPLSV=IFPLFR
      ISPMFR=IFPLFR
      CALL DPSPM5(IFLAG,IMPSW,IMPCO,IMPNR,IMPNC,
     1            IBUGG2,ISUBRO,IFOUND,IERROR)
      IFPLFR=IFPLSV
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
      IF(IFPLPT.EQ.'YOUD')THEN
        IFPLTA='ON'
      ENDIF
C
      IFEED9=IFEEDB
C
      IF(IFPLTA.EQ.'ON')THEN
        ISHIFT=ILOCQ-1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGG2,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        ISHIFT=NUMVAR-1
        CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGG2,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        DO1509I=1,NUMVAR-1
          IHARG(I)=IVARN1(I)
          IHARG2(I)=IVARN2(I)
 1509   CONTINUE
        NUMVAR=NUMVAR-1
        IF(IFPLPT.EQ.'HIST'.OR.IFPLPT.EQ.'RUNS'.OR.IFPLPT.EQ.'PERC'.OR.
     1     IFPLPT.EQ.'AUTO'.OR.IFPLPT.EQ.'SPEC'.OR.IFPLPT.EQ.'LAG ')THEN
          IF(NUMVAR.LT.1)GOTO9000
        ELSE
          IF(NUMVAR.LT.2)GOTO9000
        ENDIF
        ILOCQ=ILOCQ-1
      ENDIF
C
      IMPSW3=IMPSW
      IMPCO2=IMPCO
      IMPNR2=IMPNR
      IMPNC2=IMPNC
      IMPSW='ON'
      IMPCO=1
      IMPCO9=IMPCO
C
      IFPLRV=INT(PFPLRV+0.5)
      IF(IFPLRV.LT.1)IFPLRV=1
      NPLOTS=NUMVAR
      IFACTV=NPLOTS-IFPLRV
      IF(IFACTV.LT.1)THEN
        IFACTV=1
        IFPLRV=NPLOTS-1
      ENDIF
C
      NPLOTS=IFPLRV*IFACTV
C
      IF(IFPLRV.GT.1)THEN
        IMPNR=IFPLRV
        IMPNC=IFACTV
      ELSEIF(IMPNR*IMPNC.LT.NPLOTS)THEN
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
      IROWT=IFPLRV
      ICOLT=IFACTV
      IF(IFPLLA.EQ.'BOX')THEN
        IMPNR=IMPNR+1
        IMPNC=IMPNC+1
        IROWT=IFPLRV+1
        ICOLT=IFACTV+1
      ENDIF
C
C               *************************************
C               **   STEP 21--                     **
C               **   GENERATE THE SCATTER PLOTS    **
C               *************************************
C
      ISTEPN='21'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'DPFACT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C
C  2-VARIABLE PLOTS
C
      IVAR=2
      IF(IFPLPT.EQ.'PLOT')THEN
        ICT='PLOT'
        IC2T='    '
        NCCOMM=0
        IPLOTT='FPLO'
      ELSEIF(IFPLPT.EQ.'STAT')THEN
        ICT=IFPLST
        IC2T=IFPLS2
        NCCOMM=0
        IF(IFPLS3.NE.'    ')THEN
          NCCOMM=NCCOMM+1
          IHT(NCCOMM)=IFPLS3
          IH2T(NCCOMM)=IFPLS4
        ENDIF
        NCCOMM=NCCOMM+1
        IHT(NCCOMM)='STAT'
        IH2T(NCCOMM)='ISTI'
        NCCOMM=NCCOMM+1
        IHT(NCCOMM)='PLOT'
        IH2T(NCCOMM)='    '
        IPLOTT='STAT'
      ELSEIF(IFPLPT.EQ.'BIHI')THEN
        ICT='RELA'
        IC2T='TIVE'
        IHT(1)='BIHI'
        IH2T(1)='STOG'
        NCCOMM=1
        IPLOTT='BIHI'
      ELSEIF(IFPLPT.EQ.'QQPL')THEN
        ICT='QUAN'
        IC2T='TILE'
        IHT(1)='QUAN'
        IH2T(1)='TILE'
        IHT(2)='PLOT'
        IH2T(2)='    '
        NCCOMM=2
        IPLOTT='QQFP'
      ELSEIF(IFPLPT.EQ.'BOXC')THEN
        ICT='BOX '
        IC2T='    '
        IHT(1)='COX '
        IH2T(1)='    '
        IHT(2)='LINE'
        IH2T(2)='ARIT'
        IHT(3)='PLOT'
        IH2T(3)='    '
        NCCOMM=3
        IPLOTT='BOXC'
C
C       UNIVARIATE PLOTS
C
      ELSEIF(IFPLPT.EQ.'HIST'.OR.IFPLPT.EQ.'PERC'.OR.
     1       IFPLPT.EQ.'RUNS'.OR.IFPLPT.EQ.'SPEC'.OR.
     1       IFPLPT.EQ.'LAG '.OR.IFPLPT.EQ.'AUTO'.OR.
     1       IFPLPT.EQ.'KERN'.OR.
     1       IFPLPT.EQ.'PROB'.OR.IFPLPT.EQ.'PPCC')THEN
        IVAR=1
        IFPLRV=NUMVAR
        NPLOTS=NUMVAR
        IFACTV=0
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
        IF(IFPLLA.EQ.'BOX')IFPLLA='ON'
C
        IF(IFPLPT.EQ.'HIST')THEN
          ICT='RELA'
          IC2T='TIVE'
          IHT(1)='HIST'
          IH2T(1)='OGRA'
          NCCOMM=1
          IPLOTT='HIST'
        ELSEIF(IFPLPT.EQ.'KERN')THEN
          ICT='KERN'
          IC2T='EL  '
          IHT(1)='DENS'
          IH2T(1)='ITY '
          IHT(2)='DENS'
          IH2T(2)='ITY '
          NCCOMM=2
          IPLOTT='KERN'
        ELSEIF(IFPLPT.EQ.'RUNS')THEN
          ICT='RUN '
          IC2T='    '
          IHT(1)='SEQU'
          IH2T(1)='ENCE'
          IHT(2)='PLOT'
          IH2T(2)='    '
          NCCOMM=2
          IPLOTT='RUNS'
        ELSEIF(IFPLPT.EQ.'PERC')THEN
          ICT='PERC'
          IC2T='CENT'
          IHT(1)='POIN'
          IH2T(1)='T   '
          IHT(2)='PLOT'
          IH2T(2)='    '
          NCCOMM=2
          IPPTB2=IPPTBI
          IPPTBI='UNBI'
          IPLOTT='PERC'
        ELSEIF(IFPLPT.EQ.'AUTO')THEN
          ICT='AUTO'
          IC2T='CORR'
          IHT(1)='PLOT'
          IH2T(1)='    '
          NCCOMM=1
          IPLOTT='AUTO'
        ELSEIF(IFPLPT.EQ.'SPEC')THEN
          ICT='SPEC'
          IC2T='TRAL'
          IHT(1)='PLOT'
          IH2T(1)='    '
          NCCOMM=1
          IPLOTT='SPEC'
        ELSEIF(IFPLPT.EQ.'LAG ')THEN
          ICT='LAG '
          IC2T='    '
          IHT(1)='PLOT'
          IH2T(1)='    '
          NCCOMM=1
          IPLOTT='LAG '
        ELSEIF(IFPLPT.EQ.'PROB')THEN
          IF(IFPLP1.EQ.'    ')THEN
            ICT='NORM'
            IC2T='AL  '
            IHT(1)='PROB'
            IH2T(1)='ABIL'
            IHT(2)='PLOT'
            IH2T(2)='    '
            NCCOMM=2
          ELSE
            ICT=IFPLP1
            IC2T='    '
            NCCOMM=0
            IF(IFPLP2.NE.'    ')THEN
              NCCOMM=NCCOMM+1
              IHT(NCCOMM)=IFPLP2
              IH2T(NCCOMM)='    '
            ENDIF
            IF(IFPLP3.NE.'    ')THEN
              NCCOMM=NCCOMM+1
              IHT(NCCOMM)=IFPLP3
              IH2T(NCCOMM)='    '
            ENDIF
            IF(IFPLP4.NE.'    ')THEN
              NCCOMM=NCCOMM+1
              IHT(NCCOMM)=IFPLP4
              IH2T(NCCOMM)='    '
            ENDIF
            IF(IFPLP5.NE.'    ')THEN
              NCCOMM=NCCOMM+1
              IHT(NCCOMM)=IFPLP5
              IH2T(NCCOMM)='    '
            ENDIF
            NCCOMM=NCCOMM+1
            IHT(NCCOMM)='PROB'
            IH2T(NCCOMM)='ABIL'
            NCCOMM=NCCOMM+1
            IHT(NCCOMM)='PLOT'
            IH2T(NCCOMM)='    '
          ENDIF
          IPLOTT='PROB'
        ELSEIF(IFPLPT.EQ.'PPCC')THEN
          ICT=IFPLC1
          IC2T='    '
          NCCOMM=0
          IF(IFPLC2.NE.'    ')THEN
            NCCOMM=NCCOMM+1
            IHT(NCCOMM)=IFPLC2
            IH2T(NCCOMM)='    '
          ENDIF
          IF(IFPLC3.NE.'    ')THEN
            NCCOMM=NCCOMM+1
            IHT(NCCOMM)=IFPLC3
            IH2T(NCCOMM)='    '
          ENDIF
          IF(IFPLC4.NE.'    ')THEN
            NCCOMM=NCCOMM+1
            IHT(NCCOMM)=IFPLC4
            IH2T(NCCOMM)='    '
          ENDIF
          IF(IFPLC5.NE.'    ')THEN
            NCCOMM=NCCOMM+1
            IHT(NCCOMM)=IFPLC5
            IH2T(NCCOMM)='    '
          ENDIF
          NCCOMM=NCCOMM+1
          IHT(NCCOMM)='PPCC'
          IH2T(NCCOMM)='    '
          NCCOMM=NCCOMM+1
          IHT(NCCOMM)='PLOT'
          IH2T(NCCOMM)='    '
          IPLOTT='PPCC'
        ENDIF
      ENDIF
C
C               *************************************
C               **   GENERATE PLOTS                **
C               *************************************
C
      IF(NPLOTS.LT.1)GOTO8000
C
      ISHIFT=ILOCQ-1
      CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1            IBUGG2,IERROR)
      ISHIFT=NCCOMM+IVAR
      IF(IFPLTA.EQ.'ON' .AND. IVAR.EQ.2)ISHIFT=ISHIFT+1
      CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1            IBUGG2,IERROR)
      ICOM=ICT
      ICOM2=IC2T
      IF(NCCOMM.GT.0)THEN
        DO5301II=1,NCCOMM
          IHARG(II)=IHT(II)
          IHARG2(II)=IH2T(II)
 5301   CONTINUE
      ENDIF
      IHARG(NCCOMM+1)=IVARN1(1)
      IHARG2(NCCOMM+1)=IVARN2(1)
      IF(IVAR.GE.2)THEN
        IHARG(NCCOMM+2)=IVARN1(2)
        IHARG2(NCCOMM+2)=IVARN2(2)
        IF(IFPLTA.EQ.'ON')THEN
          IHARG(NCCOMM+3)=IVARN1(NUMVAR+1)
          IHARG2(NCCOMM+3)=IVARN2(NUMVAR+1)
        ENDIF
      ENDIF
      NARGT=NUMARG
C
      IPLOT=0
      IF(IVAR.EQ.1)THEN
        IROWT=IFPLRV
        ICOLT=1
      ELSE
        IF(IFPLLA.EQ.'BOX')THEN
          NPLOTS=NPLOTS+IMPNR+IMPNC-1
        ENDIF
      ENDIF
C
      DO5300IRES=1,IROWT
        DO5400IFAC=1,ICOLT
C
          IPLOT=IPLOT+1
          IEMPTY='NO'
C
C         ONE RESPONSE VARIABLE CASE
C
          IF(IVAR.EQ.1)THEN
            IHARG(NCCOMM+1)=IVARN1(IRES)
            IHARG2(NCCOMM+1)=IVARN2(IRES)
            IX=0
            IXLIST=1
            IROW=INT(IPLOT/IMPNC)+1
            IF(MOD(IPLOT,IMPNC).EQ.0)IROW=IROW-1
            ICOL=MOD(IPLOT,IMPNC)
            IF(ICOL.EQ.0)ICOL=IMPNC
            IF(IFPLLA.EQ.'BOX')THEN
              ICOL=ICOL-1
              IF(ICOL.EQ.0)IEMPTY='YES'
              IF(IROW.EQ.IMPNR)IEMPTY='YES'
            ENDIF
            IDY=IRES
            IDX=1
            IXZZ=IRES
C
C         TWO RESPONSE VARIABLE CASE
C
          ELSE
            IXLIST=IFAC
            IROW=INT(IPLOT/IMPNC)+1
            IF(MOD(IPLOT,IMPNC).EQ.0)IROW=IROW-1
            ICOL=MOD(IPLOT,IMPNC)
            IF(ICOL.EQ.0)ICOL=IMPNC
C
            ITEMP=IFAC
            IF(IFPLLA.EQ.'BOX')THEN
              ICOL=ICOL-1
              ITEMP=IFAC-1
              IF(ITEMP.EQ.0)IEMPTY='YES'
              IF(IROW.EQ.IMPNR)IEMPTY='YES'
            ENDIF
C
            IF(IRES.LE.IFPLRV)THEN
              IHARG(NCCOMM+1)=IVARN1(IRES)
              IHARG2(NCCOMM+1)=IVARN2(IRES)
              IDY=IRES
            ELSE
              IHARG(NCCOMM+1)=IVARN1(IFPLRV)
              IHARG2(NCCOMM+1)=IVARN2(IFPLRV)
              IDY=IFPLRV
            ENDIF
C
            IX=IFPLRV+ITEMP
            IDX=ITEMP
            IF(IDX.LE.0)IDX=1
            IF(IX.GT.IFPLRV)THEN
              IHARG(NCCOMM+2)=IVARN1(IX)
              IHARG2(NCCOMM+2)=IVARN2(IX)
            ELSE
              IHARG(NCCOMM+2)=IVARN1(IFPLRV+1)
              IHARG2(NCCOMM+2)=IVARN2(IFPLRV+1)
            ENDIF
            IXZZ=IX
          ENDIF
C
          IF(IEMPTY.EQ.'YES')THEN
            DO5304I=1,MAXSUB
              ISU2SW(I)=ISUBSW(I)
              ISUBSW(I)='OFF'
 5304       CONTINUE
          ENDIF
          IOPTN=3
          CALL DPSPM4(ICASPL,IOPTN,IDX,IDY,
     1                ISUBNU,ISUBSW,
     1                ASUBXL,ASUBXU,ASUBYL,ASUBYU,
     1                ISUBN9,ISUBSZ,
     1                ASBXL2,ASBXU2,ASBYL2,ASBYU2,
     1                PFPXSL,PFPXSU,PFPYSL,PFPYSU,
     1                IBUGG2,ISUBRO,IERROR)
C
          ICASPL='FACT'
          CALL DPSPM1(ICASPL,IVARN1,IVARN2,ICOLL,
     1                IMPNR,IMPNC,IROW,ICOL,IRES,IXZZ,IPLOT,
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
     1                PFPXLL,PFPXUL,PFPYLL,PFPYUL,IXLIST,
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
     1                MAXNXT,
     1                ISUBRO,IFOUND,IERROR)
          IF(IEMPTY.EQ.'NO')THEN
            CALL DPSPM3(ICASPL,IOUNI5,
     1                  IROW,ICOL,
     1                  PX2LD2,NPLOTP,
     1                  IFORSW,
     1                  IFPX2L,ISPX2P,ISPX2S,
     1                  IHRIGH,IHRIG2,IHWUSE,
     1                  ISUBN1,ISUBN2,MESSAG,
     1                  IBUGG2,ISUBRO,IERROR)
          ENDIF
C
          IF(IVAR.EQ.1)THEN
            ISHIFT=NARGT-NUMARG
            IF(ISHIFT.GT.0)THEN
              CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                    IBUGG2,IERROR)
            ELSEIF(ISHIFT.LT.0)THEN
              ISHIFT=-ISHIFT
              CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                    IBUGG2,IERROR)
            ENDIF
            ICOM=ICT
            ICOM2=IC2T
            DO6101II=1,NCCOMM
              IHARG(II)=IHT(II)
              IHARG2(II)=IH2T(II)
 6101       CONTINUE
            IHARG(NCCOMM+1)=IVARN1(1)
            IHARG2(NCCOMM+1)=IVARN2(1)
          ENDIF
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
C
          ICNTPL=ICNTPL+1
          IF(N.GT.0)THEN
            DO3115II=1,N
              WRITE(IOUNI5,3118)ICNTPL,Y(II),X(II),D(II)
 3115       CONTINUE
 3118       FORMAT(I12,3E15.7)
          ENDIF
C
          IF(IERROR.EQ.'NO')IAND1=IAND2
          IF(IERROR.EQ.'YES')GOTO5499
C
          IF(IVAR.EQ.1)GOTO5499
          IF(IFPLPT.NE.'PLOT')GOTO5499
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
          IF(IVAR.EQ.2)THEN
            ISHIFT=NARGT-NUMARG
            IF(ISHIFT.GT.0)THEN
              CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                    IBUGG2,IERROR)
            ELSEIF(ISHIFT.LT.0)THEN
              ISHIFT=-ISHIFT
              CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                    IBUGG2,IERROR)
            ENDIF
            ICOM=ICT
            ICOM2=IC2T
            IF(NCCOMM.GT.0)THEN
              DO5401II=1,NCCOMM
                IHARG(II)=IHT(II)
                IHARG2(II)=IH2T(II)
 5401         CONTINUE
            ENDIF
            IHARG(NCCOMM+1)=IVARN1(1)
            IHARG2(NCCOMM+1)=IVARN2(1)
            IHARG(NCCOMM+2)=IVARN1(1)
            IHARG2(NCCOMM+2)=IVARN2(1)
            IF(IFPLTA.EQ.'ON')THEN
              IHARG(NCCOMM+3)=IVARN1(NUMVAR+1)
              IHARG2(NCCOMM+3)=IVARN2(NUMVAR+1)
            ENDIF
          ENDIF
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
C
C
C               **************************************************
C               **   STEP 28--                                  **
C               **   REINSTATE INITIAL SETTINGS                 **
C               **************************************************
C
 8000 CONTINUE
C
      ISTEPN='28'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'4PLO')THEN
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
C
      IFEEDB=IFEED9
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
C
      IOP='CLOS'
      CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,
     1            IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1            IBUGG2,ISUBRO,IERROR)
      IFITAU=IFITA2
C
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'FACT')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFACT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9013   FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',3I8,3(2X,A4))
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFAIR(NPTS,NLAB,
     1                  AMEAN,ASD,N,
     1                  XFAIR,XFAIS2,SEFWK1,SEFWK2,
     1                  DLOWFW,DHIGFW,DLOWF2,DHIGF2,DLOWF3,DHIGF3,
     1                  IWRITE,
     1                  ICAPSW,ICAPTY,IFLAG9,NUMDIG,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--IMPLEMENT FAIRWEATHER APPROACH TO CONSENSUS MEANS
C     PRINTING--YES
C     SUBROUTINES NEEDED--NONE
C     REFERENCES--ADAPTED FROM MATLAB SCRIPT PROVIDED BY
C                 ANDREW RUHKIN OF THE NIST STATISTICAL
C                 ENGINEERING DIVISION
C               --FAIRWEATHER (1972), "A METHOD FOR OBTAINING
C                 AN EXACT CONFIDENCE INTERVAL FOR THE COMMON
C                 MEAN OF SEVERAL NORMAL POPULATIONS",
C                 APPLIED STATISTICS, 21, PP. 229-233.
C               --M. G. COX (2002), "THE EVALUATION OF KEY
C                 COMPARISON DATA", METROLOGIA, 39, PP. 589-595.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2006/4
C     ORIGINAL VERSION--APRIL     2006.
C     UPDATED         --OCTOBER   2006. CALL LIST TO TPPF
C     UPDATED         --UPDATED   2010. USE DPDTA1 TO PRINT
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES--------------
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
      REAL AMEAN(*)
      REAL ASD(*)
C
      REAL APPF
      REAL XFAIR
      REAL XFAIS2
      REAL SEFWK1
      REAL SEFWK2
C
      LOGICAL IFLAG9
C
      INTEGER N(*)
C
C----------------------------------------------------------------
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
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT------------------------------------------------
C
      IERROR='NO'
      ISUBN1='DPFA'
      ISUBN2='IR  '
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FAIR')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPFAIR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IWRITE,NPTS,NLAB
   52   FORMAT('IWRITE,NPTS,NLAB = ',A4,2X,2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C     STEP 1: COMPUTE THE FAIRWEATHER CONSENSUS MEAN
C
      IFLAG9=.TRUE.
      DSUM1=0.0D0
      DO910I=1,NLAB
        DNI=DABS(DBLE(N(I)))
        IF(N(I).GT.5)THEN
          DSUM1=DSUM1 + (DNI-3.0D0)/(DNI-1.0D0)
        ELSE
          IFLAG9=.FALSE.
          XFAIR=CPUMIN
          DLOWFW=0.0D0
          DHIGFW=0.0D0
          GOTO9000
        ENDIF
  910 CONTINUE
      DU1=DSUM1
C
      DSUM1=0.0D0
      DO920I=1,NLAB
        DNI=DBLE(N(I))
        IF(N(I).LT.0)THEN
          DVARI=DBLE(ASD(I))**2
          U=DVARI
        ELSE
          DVARI=DBLE(ASD(I))**2
          U=DVARI/DNI
        ENDIF
        CK=(DABS(DNI)-3.0D0)/(DABS(DNI)-1.0D0)
        CF=CK/DU1
        WF=CF/DSQRT(U)
        DSUM1=DSUM1 +  WF
  920 CONTINUE
      DSS=DSUM1
C
      DSUM1=0.0D0
      DO930I=1,NLAB
        DNI=DBLE(N(I))
        DMEAN=DBLE(AMEAN(I))
        IF(N(I).LT.0)THEN
          DVARI=DBLE(ASD(I))**2
          U=DVARI
        ELSE
          DVARI=DBLE(ASD(I))**2
          U=DVARI/DNI
        ENDIF
        CK=(DABS(DNI)-3.0D0)/(DABS(DNI)-1.0D0)
        CF=CK/DU1
        WF=CF/DSQRT(U)
        DWI=WF/DSS
        DSUM1=DSUM1 + DWI*DMEAN
  930 CONTINUE
      XFAIR=REAL(DSUM1)
C
      DP=DBLE(NLAB)
      DPP=1.0D0/DBLE(NLAB-1)
      DRR=DP**(DP*DPP/2.0D0)
      IDF=NLAB-1
      ALPHA=0.975
      CALL TPPF(REAL(ALPHA),REAL(IDF),APPF)
      DPH=DBLE(APPF)/DRR/(DSQRT(DP-1.0D0))
C
      DSUM2=0.0D0
      DSUM3=0.0D0
      DSUM4=0.0D0
      DSUM5=0.0D0
C
      DPROD1=1.0D0
      DO940I=1,NLAB
        DNI=DBLE(N(I))
        DMEAN=DBLE(AMEAN(I))
        IF(N(I).LT.0)THEN
          DVARI=DBLE(ASD(I))**2
          U=DVARI
        ELSE
          DVARI=DBLE(ASD(I))**2
          U=DVARI/DNI
        ENDIF
        CK=(DABS(DNI)-3.0D0)/(DABS(DNI)-1.0D0)
        CF=CK/DU1
        WF=CF/DSQRT(U)
        DWI=WF/DSS
        DSUM2=DSUM2 + DWI*(DMEAN - DBLE(XFAIR))**2
        DPROD1=DPROD1*DWI
        DSUM3=DSUM3 + CF*CF/(DABS(DNI)-5.0D0)
        DSUM4=DSUM4 + WF**4/(CK*CK*(DABS(DNI)-5.0D0))
        DSUM5=DSUM5 + WF**2/CK
  940 CONTINUE
      DPH2=1.0D0/DRR/(DSQRT(DP-1.0D0))
      DPROD1=DPROD1**DPP
      DRI=DPH*DSQRT(DSUM2)/DSQRT(DPROD1)
      SEFWK1=DPH2*DSQRT(DSUM2)/DSQRT(DPROD1)
      SEFWK2=2.0*SEFWK2
      SU2=DSUM3
      SU=DSUM4
      UD=DSUM5
      NR=INT(4.0D0 + (1.0D0/SU2))
      ALPHA=0.975
      CALL TPPF(REAL(ALPHA),REAL(NR),APPF)
      FC=DSQRT((DBLE(NR)-2.0D0)/(DBLE(NR)*DU1))
      TF=FC*DBLE(APPF)
      NU=INT(4.0 + (UD*UD/SU))
C
      DLOWF2=DBLE(XFAIR) - (TF/DSS)
      DHIGF2=DBLE(XFAIR) + (TF/DSS)
C
      CALL TPPF(REAL(ALPHA),REAL(NU),APPF)
      RC=DSQRT(UD*(DBLE(NU) - 2.0D0)/DBLE(NU))
      DLOWF3=DBLE(XFAIR) - (RC*DBLE(APPF))
      DHIGF3=DBLE(XFAIR) + (RC*DBLE(APPF))
C
C
      DLOWFW=DBLE(XFAIR) - DRI
      DHIGFW=DBLE(XFAIR) + DRI
C
      IF(.NOT.IFLAG9)GOTO9000
      IF(IPRINT.EQ.'OFF')GOTO9000
C
      ITITLE=' '
      NCTITL=0
      ITITLZ=' '
      NCTITZ=0
C
      ICNT=1
      ITEXT(ICNT)=' 6. Method: Fairweather'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='    Estimate of Consensus Mean:'
      NCTEXT(ICNT)=31
      AVALUE(ICNT)=XFAIR
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='    Degrees of Freedom (Fairweather):'
      NCTEXT(ICNT)=37
      AVALUE(ICNT)=NR
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='    Degrees of Freedom (Cox):'
      NCTEXT(ICNT)=28
      AVALUE(ICNT)=NU
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='    Lower 95% (Fairweather) Confidence Limit:'
      NCTEXT(ICNT)=45
      AVALUE(ICNT)=DLOWF2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='    Upper 95% (Fairweather) Confidence Limit:'
      NCTEXT(ICNT)=45
      AVALUE(ICNT)=DHIGF2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='    Lower 95% (Cox) Confidence Limit:'
      NCTEXT(ICNT)=37
      AVALUE(ICNT)=DLOWF3
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='    Upper 95% (Cox) Confidence Limit:'
      NCTEXT(ICNT)=37
      AVALUE(ICNT)=DHIGF3
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='    Lower 95% (minmax) Confidence Limit:'
      NCTEXT(ICNT)=40
      AVALUE(ICNT)=DLOWFW
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='    Upper 95% (minmax) Confidence Limit:'
      NCTEXT(ICNT)=40
      AVALUE(ICNT)=DHIGFW
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='    Note: Fairweather Best Usage:'
      NCTEXT(ICNT)=33
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='          Minimum Sample Size for Lab > 5'
      NCTEXT(ICNT)=41
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FAIR')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFAIR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPTS,NLAB,XFAIR,XFAIS2
 9013   FORMAT('NPTS,NLAB,XFAIR,XFAIS2 = ',2I8,2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)DLOWFW,DHIGFW
 9015   FORMAT('DLOWFW,DHIGFW = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFAN2(X,NROW,NCOL,NCLUST,IVARN1,IVARN2,
     1                  DSS,DVEC,P,DP,PT,ESP,EF,RDRAW,
     1                  NCLUV,NELEM,NFUZZ,
     1                  ICAPSW,ICAPTY,IFORSW,MAXNXT,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--PERFORM A "FUZZY" CLUSTER ANALYSIS USING KAUFFMAN AND
C              ROUSSEEUW "FANNY" ALGORITHM.
C     REFERENCES--KAUFMAN AND ROUSSEEUW (1990), "FINDING GROUPS IN DATA:
C                 AN INTRODUCTION TO CLUSTER ANALYSIS", WILEY.
C               --ROUSSEEUW (1987), "SILHOUETTES: A GRAPHICAL AID TO THE
C                 INTERPRETATION AND VALIDATION OF CLUSTER ANALYSIS",
C                 JOURNAL OF COMPUTATIONAL AND APPLIED MATHEMATICS,
C                 VOL. 20, PP. 53-65, NORTH HOLLAND.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     REFERENCE--ASTM MANUAL STP-15D, PAGES 78-84, 100-105
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2017/08
C     ORIGINAL VERSION--AUGUST      2017.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      DIMENSION X(NROW,NCOL)
      DIMENSION P(NROW,NCOL)
      DIMENSION DP(NROW,NCOL)
      DIMENSION DSS(*)
      DIMENSION DVEC(*)
      DIMENSION PT(*)
      DIMENSION ESP(*)
      DIMENSION EF(*)
      DIMENSION RDRAW(*)
C
      INTEGER NCLUV(*)
      INTEGER NELEM(*)
      INTEGER NFUZZ(*)
C
      CHARACTER*4 IVARN1(*)
      CHARACTER*4 IVARN2(*)
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 ICASPL
      CHARACTER*4 ITYP3
      CHARACTER*4 IOP
      CHARACTER*3 LAB
C
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPFA'
      ISUBN2='N2  '
      IWRITE='OFF'
      IFLAGO=0
C
      ICNT=0
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FAN2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,70)
   70   FORMAT('AT THE BEGINNING OF DPFAN2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)NROW,NCOL,NCLUST,IFANMS,IFANSC,IFANDI,IFANTY
   72   FORMAT('NROW,NCOL,NCLUST,IFANMS,IFANSC,IFANDI,IFANTY = ',
     1         4I8,3(2X,A4))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,18)ICAPSW,ICAPTY,IFORSW,MAXNXT
   18   FORMAT('ICAPSW,ICAPTY,IFORSW,MAXNXT = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO75I=1,NROW
          WRITE(ICOUT,77)I,(X(I,J),J=1,5)
   77     FORMAT('I,(X(I,J),J=1,5) = ',I8,2X,5G15.7)
          CALL DPWRST('XXX','BUG ')
   75   CONTINUE
      ENDIF
C
C               ********************************
C               **   STEP 1--                 **
C               **   CHECK FOR MISSING VALUES **
C               ********************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FAN2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     FIRST CHECK WHETHER ANY ROWS OR COLUMNS CONTAIN ONLY
C     MISSING DATA.  THIS WILL BE TREATED AS AN ERROR CONDITION.
C
C     CHECK ROWS FIRST
C
      DO80I=1,NROW
        DO90J=1,NCOL
          IF(X(I,J).NE.PSTAMV)GOTO99
   90   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,91)
   91   FORMAT('****** ERROR IN PAM CLUSTERING--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,93)I
   93   FORMAT('       ROW (OBSERVATION) ',I8,' CONTAINS ONLY ',
     1         'MISSING DATA.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
   99   CONTINUE
   80 CONTINUE
C
C     NOW CHECK COLUMNS
C
      NMISS=0
      NMAT=0
C
      DO730J=1,NCOL
        NMISSV=0
        DO740I=1,NROW
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
  743     FORMAT('       COLUMN (VARIABLE) ',I8,' CONTAINS ONLY ',
     1           'MISSING DATA.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
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
C
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
  165     FORMAT('****** WARNING IN PAM CLUSTERING--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,167)
  167     FORMAT('       NO VARIABLES ARE DEFINED FOR ALL ',
     1           'OBSERVATIONS.')
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
C               ******************************
C               **   STEP 2--               **
C               **   SCALE IF REQUESTED     **
C               ******************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'PAM2')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,168)NMISS,PSTAMV
  168   FORMAT('NMISS,PSTAMV = ',I8,2X,G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C     IF NROW = NCOL, ASSUME OUR DATA IS A DISSIMILARITY MATRIX.
C     OTHERWISE, ASSUME OUR DATA IS MEASUREMENT DATA.  DO NOT SCALE
C     DISSIMILARITY DATA EVEN IF SCALING OPTION TURNED ON.
C
      LARGE=2
      IF(IFANPR.EQ.'FINA')LARGE=1
      IF(NROW.EQ.NCOL .AND. IFANTY.EQ.'DISS')THEN
        JDYSS=1
      ELSE
        JDYSS=0
      ENDIF
      NSTAN=0
      IF(JDYSS.EQ.1)GOTO299
      IF(IFANSC.EQ.'OFF')GOTO299
C
      NSTAN=1
      DO201JJ=1,NCOL
        NROWT=0
        DO203II=1,NROW
          IF(X(II,JJ).NE.PSTAMV)THEN
            NROWT=NROWT+1
            DSS(NROWT)=X(II,JJ)
          ENDIF
  203   CONTINUE
        IF(ISTALO.EQ.'MEAN')THEN
          CALL MEAN(DSS,NROWT,IWRITE,XMEAN,IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'MEDI')THEN
          CALL MEDIAN(DSS,NROWT,IWRITE,ESP,MAXNXT,XMEAN,
     1                IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'MIDM')THEN
          CALL MIDMEA(DSS,NROWT,IWRITE,ESP,MAXNXT,XMEAN,
     1                IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'HARM')THEN
          CALL HARMEA(DSS,NROWT,IWRITE,XMEAN,IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'MINI')THEN
          CALL MINIM(DSS,NROWT,IWRITE,XMEAN,IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'GEOM')THEN
          CALL GEOMEA(DSS,NROWT,IWRITE,XMEAN,IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'BILO')THEN
          CALL BIWLOC(DSS,NROWT,IWRITE,EF,ESP,MAXNXT,XMEAN,
     1                IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'H15 ')THEN
          NCUT=0
          C=1.5
          CALL H15(DSS,NROWT,C,NCUT,XMEAN,XSC,EF,ESP,MAXNXT,
     1                IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'H10 ')THEN
          NCUT=0
          C=1.0
          CALL H15(DSS,NROWT,C,NCUT,XMEAN,XSC,EF,ESP,MAXNXT,
     1                IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'H12 ')THEN
          NCUT=0
          C=1.2
          CALL H15(DSS,NROWT,C,NCUT,XMEAN,XSC,EF,ESP,MAXNXT,
     1                IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'H17 ')THEN
          NCUT=0
          C=1.7
          CALL H15(DSS,NROWT,C,NCUT,XMEAN,XSC,EF,ESP,MAXNXT,
     1                IBUGA3,IERROR)
        ELSEIF(ISTALO.EQ.'H20 ')THEN
          NCUT=0
          C=2.0
          CALL H15(DSS,NROWT,C,NCUT,XMEAN,XSC,EF,ESP,MAXNXT,
     1                IBUGA3,IERROR)
        ELSE
          CALL MEAN(DSS,NROWT,IWRITE,XMEAN,IBUGA3,IERROR)
        ENDIF
C
        IF(ISTASC.EQ.'SD  ')THEN
          CALL SD(DSS,NROWT,IWRITE,XSD,IBUGA3,IERROR)
        ELSEIF(ISTASC.EQ.'H15S')THEN
          NCUT=0
          C=1.5
          CALL H15(DSS,NROWT,C,NCUT,XLOC,XSD,EF,ESP,MAXNXT,
     1                IBUGA3,IERROR)
        ELSEIF(ISTASC.EQ.'H10S')THEN
          NCUT=0
          C=1.0
          CALL H15(DSS,NROWT,C,NCUT,XLOC,XSD,EF,ESP,MAXNXT,
     1                IBUGA3,IERROR)
        ELSEIF(ISTASC.EQ.'H12S')THEN
          NCUT=0
          C=1.2
          CALL H15(DSS,NROWT,C,NCUT,XLOC,XSD,EF,ESP,MAXNXT,
     1                IBUGA3,IERROR)
        ELSEIF(ISTASC.EQ.'H17S')THEN
          NCUT=0
          C=1.7
          CALL H15(DSS,NROWT,C,NCUT,XLOC,XSD,EF,ESP,MAXNXT,
     1                IBUGA3,IERROR)
        ELSEIF(ISTASC.EQ.'H20S')THEN
          NCUT=0
          C=2.0
          CALL H15(DSS,NROWT,C,NCUT,XLOC,XSD,EF,ESP,MAXNXT,
     1                IBUGA3,IERROR)
        ELSEIF(ISTASC.EQ.'BISC')THEN
          CALL BIWSCA(DSS,NROWT,IWRITE,EF,ESP,MAXNXT,XSD,
     1                IBUGA3,IERROR)
        ELSEIF(ISTASC.EQ.'MAD ')THEN
          CALL MAD(DSS,NROWT,IWRITE,EF,ESP,MAXNXT,XSD,
     1             IBUGA3,IERROR)
        ELSEIF(ISTASC.EQ.'MADN')THEN
          CALL MAD(DSS,NROWT,IWRITE,EF,ESP,MAXNXT,XSD,
     1             IBUGA3,IERROR)
          XSD=XSD/0.67449
        ELSEIF(ISTASC.EQ.'AAD ')THEN
          CALL AAD(DSS,NROWT,IWRITE,EF,MAXNXT,XSD,'MEAN',
     1             IBUGA3,IERROR)
        ELSEIF(ISTASC.EQ.'IQRA')THEN
          CALL LOWQUA(DSS,NROWT,IWRITE,EF,MAXNXT,RIGH1,
     1                IBUGA3,IERROR)
          CALL UPPQUA(DSS,NROWT,IWRITE,EF,MAXNXT,RIGH2,
     1                IBUGA3,IERROR)
          XSD=RIGH2-RIGH1
        ELSEIF(ISTASC.EQ.'NIQR')THEN
          CALL LOWQUA(DSS,NROWT,IWRITE,EF,MAXNXT,RIGH1,
     1                IBUGA3,IERROR)
          CALL UPPQUA(DSS,NROWT,IWRITE,EF,MAXNXT,RIGH2,
     1                IBUGA3,IERROR)
          XSD=0.7413*(RIGH2-RIGH1)
        ELSEIF(ISTASC.EQ.'SNSC')THEN
          XSD=SN(DSS,NROWT,ESP,EF,RDRAW)
        ELSEIF(ISTASC.EQ.'MAXI')THEN
          CALL MINIM(DSS,NROWT,IWRITE,XMIN,IBUGA3,IERROR)
          CALL MAXIM(DSS,NROWT,IWRITE,XMAX,IBUGA3,IERROR)
          XSD=XMAX - XMIN
        ELSE
          CALL SD(DSS,NROWT,IWRITE,XMEAN,IBUGA3,IERROR)
        ENDIF
C
        IF(XSD.LE.0.0)THEN
          WRITE(ICOUT,91)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,206)JJ
  206     FORMAT('       VARIABLE ',I4,' HAS ZERO STANDARD DEVIATION ',
     1           'WHEN SCALING REQUESTED.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
        DO205II=1,NROW
          IF(X(II,JJ).NE.PSTAMV)THEN
            AVAL=(X(II,JJ)-XMEAN)/XSD
            X(II,JJ)=AVAL
          ENDIF
  205   CONTINUE
  201 CONTINUE
C
C
  299 CONTINUE
C
C     OPEN THE AUXILLARY FILES
C
      IOP='OPEN'
      IFLG11=1
      IFLG21=1
      IFLG31=1
      IFLAG4=0
      IFLAG5=0
      CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,
     1            IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1            IBUGA3,ISUBRO,IERROR)
      IFLAGO=1
      IF(IERROR.EQ.'YES')GOTO9000
C
C               ************************************
C               **   STEP 3--                     **
C               **   PERFORM THE CLUSTER ANALYSIS **
C               ************************************
C
C     THIS CODE IS A SOMEWHAT MODIFED VERSION OF CODE IN THE
C     FANNY MAIN ROUTINE.
C
      NN=NROW
      JPP=NCOL
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FAN2')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,169)NCLUST,NN,JPP,NDYST,LARGE,JDYSS
  169   FORMAT('NCLUST,NN,JPP,NDYST,LARGE,JDYSS = ',6I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
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
        WRITE(ICOUT,303)
  303   FORMAT(10X,'*  ROUSSEEUW/KAUFFMAN FUZZY CLUSTERING       *')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,304)
  304   FORMAT(10X,'*  (USING THE FANNY ROUTINE).                *')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,302)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,301)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(JDYSS.EQ.0)THEN
C
C       IF RAW DATA ENTERED, CREATE THE DISSIMILARITY MATRIX.
C
        CALL DYSTAF(NROW,NCOL,NROW,NCOL,X,DSS,NDYST,AMISS,JHALT,
     1              ISUBRO,IBUGA3)
        IF(JHALT.EQ.1)THEN
          WRITE(ICOUT,91)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,870) 
  870     FORMAT('       ERROR IN COMPUTING THE DISSSIMILARITY MATRIX.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
C
      ELSE
C
C       IF DISSIMILARITY MATRIX ENTERED AS INPUT, COPY LOWER DIAGONAL
C       TO DSS.
C
C       NOTE THAT FANNY EXPECTS THE DISSSIMILARITIES IN COLUMN ORDER
C       (AGNES AND PAM EXPECT IT IN ROW ORDER)
C
        DO74II=1,MAXNXT
          DSS(II)=0.0
   74   CONTINUE
C
        ICNT=0
        DO71JJ=1,NCOL
          DO73II=JJ+1,NROW
            IF(X(II,JJ).LT.0.0)THEN
              WRITE(ICOUT,91)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,872)II,JJ
  872         FORMAT('       ROW ',I5,' COLUMN ',I5,' OF THE ',
     1               'DISSIMILARITY MATRIX IS NON-POSITIVE.')
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GOTO9000
            ELSEIF(X(II,JJ).NE.X(JJ,II))THEN
              WRITE(ICOUT,91)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,874)
  874         FORMAT('       THE DISSIMILARITY MATRIX IS NOT '
     1               'SYMMETRIC.')
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GOTO9000
            ENDIF
            ICNT=ICNT+1
            DSS(ICNT)=X(II,JJ)
   73     CONTINUE
   71   CONTINUE
      ENDIF
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FAN2')THEN
        ISTEPN='2B'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,76)
   76   FORMAT('AFTER CREATE DSS ARRAY:')
        CALL DPWRST('XXX','BUG ')
        DO78II=1,ICNT
          WRITE(ICOUT,79)II,DSS(II)
   79     FORMAT('II,DSS(II) = ',I5,F10.3)
          CALL DPWRST('XXX','BUG ')
   78   CONTINUE
      ENDIF
C
      IF(LARGE.EQ.2 .AND. IPRINT.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9060)
 9060   FORMAT('DISSIMILARITY MATRIX')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9061)
 9061   FORMAT('--------------------')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        LAB='001'
        WRITE(ICOUT,9033)LAB
 9033   FORMAT(A3,2X,8F9.2)
        CALL DPWRST('XXX','BUG ')
C
        DO 120 L=2,NN
          LSUBT=L-1
          JPEND=LSUBT
          IF(LSUBT.GT.8)JPEND=8 
          DO 110 J=1,LSUBT
            NLJ=NN*(J-1)+L-(J*(J+1))/2
            DVEC(J)=DSS(NLJ)
  110     CONTINUE
          LAB='000'
          IF(L.LE.9)THEN
            WRITE(LAB(3:3),'(I1)')L
          ELSEIF(L.LE.99)THEN
            WRITE(LAB(2:3),'(I2)')L
          ELSE
            WRITE(LAB(1:3),'(I3)')L
          ENDIF
C
          WRITE(ICOUT,9033)LAB,(DVEC(J),J=1,JPEND)
          CALL DPWRST('XXX','BUG ')
          IF(LSUBT.GT.8)THEN
            WRITE(ICOUT,9040)(DVEC(J),J=9,LSUBT)
 9040       FORMAT(5X,8F9.2)
            CALL DPWRST('XXX','BUG ')
          ENDIF
  120   CONTINUE
      ENDIF
C
      S=0.0 
      NHALF=NN*(NN-1)/2+1 
      L=1 
  130 CONTINUE
      L=L+1 
      IF(DSS(L).GT.S)S=DSS(L)
      IF(L.LT.NHALF)GO TO 130 
C
C     ORIGINAL FANNY CODE ALLOWS FOR SPECIFICATION OF MINIMUM AND
C     MAXIMUM VALUES FOR THE NUMBER OF CLUSTERS.  CURRENTLY, WE ONLY
C     IMPLEMENT FOR A SINGLE VALUE FOR THE NUMBER OF CLUSTERS.
C
      KBEG=NCLUST
      KEND=NCLUST
      DO 140 KK=KBEG,KEND 
        IF(KK.GT.KBEG)THEN
          KMP=KK-1
CNIST     WRITE(ICOUT,9068)KMP,KK
C9068     FORMAT(' I am finished with',I3,' clusters, working on',I3)
        ENDIF
        IF(IPRINT.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,9070)
 9070     FORMAT('********************************')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,9071)KK
 9071     FORMAT('*  NUMBER OF CLUSTERS',I6,4X,'*')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,9070)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        CALL FUZZY(NROW,NROW,P,DP,PT,DSS,ESP,EF,EDA,EDB,KK,
     1             IBUGA3,ISUBRO)
C
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FAN2')THEN
          WRITE(ICOUT,9077)
 9077     FORMAT('DPFAN2: BETWEEN FUZZY AND CADDY')
          CALL DPWRST('XXX','BUG ')
          DO9073II=1,NROW
            WRITE(ICOUT,9074)II,(P(II,JJ),JJ=1,MIN(KK,10))
 9074       FORMAT('II,(P(II,JJ),JJ=1,K) = ',I6,10G15.7)
            CALL DPWRST('XXX','BUG ')
 9073     CONTINUE
        ENDIF
C
        CALL CADDY(NROW,NROW,P,KK,KTRUE,
     1             NFUZZ,NCLUV,PT,NELEM,EDA,EDB,
     1             IOUNI1,IOUNI2,IBUGA3,ISUBRO)
C
CNIST   IF(LGRAP.GT.0 .AND. KTRUE.GT.1 .AND. KTRUE.LT.NN)THEN
CNIST     CALL FYGUR(KTRUE,NN,MAXNN,MAXKK,MAXHH,NCLUV,NSEND,NELEM,
CNIST1               NEGBR,SYL,DVEC,PT,DSS,S,NUM(11),NUM(12),NUM(13))
CNIST   ENDIF
C
  140 CONTINUE
C
C
C               *****************************************
C               **   STEP 4B--                         **
C               **   CREATE VALUES FOR SILHOUETTE PLOT **
C               *****************************************
C
      ISTEPN='4B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FAN2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     COMPUTE THE s(i) VALUE AS
C
C        s(i) = (b(i) - a(i))/max{a(i),b(i)}
C
C     WHERE
C
C        a(i)   = AVERAGE DISSIMILARITY OF THE i-TH POINT WITH
C                 ALL OTHER POINTS IN THE CLUSTER TO WHICH IT
C                 BELONGS
C
C        b(i)   = LOWEST AVERAGE DISSIMILARITY OF THE i-TH POINT
C                 WITH ALL OTHER CLUSTERS.
C
C     USE ONE-PASS MEAN ALGORITHMS TO KEEP TRACK OF AVERAGE
C     DISSIMILARITY OF ALL CLUSTERS.  THE ONE-PASS FORMUALA IS
C
C         M(K)=X1                            K = 1
C             =M(K-1) + (X(K) - M(K-1))/K    K = 2, ...., N
C
      IF(JDYSS.EQ.1)THEN
C
C       CASE WHERE INPUT DATA IS A DISSIMILARITY MATRIX
C
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FAN2')THEN
          WRITE(ICOUT,8111)NROW
 8111     FORMAT('DISSIMILARITY CASE: NROW = ',I5)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        DO8110II=1,NROW
          ICLUS1=NCLUV(II)
C
          DO8114KK=1,NCLUST
            NFUZZ(KK)=0
 8114     CONTINUE
C
          DO8120JJ=1,NROW
            IF(II.EQ.JJ)GOTO8120
            ICLUS2=NCLUV(JJ)
            ADIST=X(II,JJ)
C
            IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FAN2')THEN
              WRITE(ICOUT,8117)II,JJ,ICLUS1,ICLUS2,ADIST
 8117         FORMAT('II,JJ,ICLUS1,ICLUS2,ADIST = ',4I5,G15.7)
              CALL DPWRST('XXX','BUG ')
            ENDIF
C
            IF(ICLUS1.EQ.ICLUS2)THEN
C
C             COMPUTE A(I) TERM
C
              NFUZZ(ICLUS1)=NFUZZ(ICLUS1)+1
              IF(NFUZZ(ICLUS1).EQ.1)THEN
                DVEC(ICLUS1)=ADIST
              ELSE
                TERM1=(ADIST - DVEC(ICLUS1))/REAL(NFUZZ(ICLUS1))
                DVEC(ICLUS1)=DVEC(ICLUS1) + TERM1
              ENDIF
            ELSE
              NFUZZ(ICLUS2)=NFUZZ(ICLUS2)+1
              IF(NFUZZ(ICLUS2).EQ.1)THEN
                DVEC(ICLUS2)=ADIST
              ELSE
                TERM1=(ADIST - DVEC(ICLUS2))/REAL(NFUZZ(ICLUS2))
                DVEC(ICLUS2)=DVEC(ICLUS2) + TERM1
              ENDIF
            ENDIF
 8120     CONTINUE
C
          AI=DVEC(ICLUS1)
          BI=CPUMAX
          DO8130JJ=1,NCLUST
            IF(JJ.EQ.ICLUS1)GOTO8130
            IF(DVEC(JJ).LT.BI)BI=DVEC(JJ)
 8130     CONTINUE
          SYL=0.0
          IF(AI.LT.BI)THEN
            SYL=1.0 - (AI/BI)
          ELSEIF(AI.GT.BI)THEN
            SYL=(BI/AI) - 1.0
          ENDIF
CCCCC     SYL=(BI - AI)/MAX(AI,BI)
C
          WRITE(IOUNI3,'(3E15.7)')REAL(II),REAL(NCLUV(II)),SYL
C
          IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FAN2')THEN
            WRITE(ICOUT,8131)II,NCLUV(II),AI,BI,SYL
 8131       FORMAT('II,NCLUV(II),AI,BI,SYL = ',2I6,3G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
 8110   CONTINUE
      ELSE
C
C       CASE WHERE INPUT DATA IS MEASUREMENT DATA
C
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FAN2')THEN
          WRITE(ICOUT,8121)NROW
 8121     FORMAT('MEASUREMENT DATA CASE: NROW = ',I5)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        DO8210II=1,NROW
          ICLUS1=NCLUV(II)
          DO8212JJ=1,NCOL
            RDRAW(JJ)=X(II,JJ)
 8212     CONTINUE
          ICASPL='VEDI'
          DO8214KK=1,NCLUST
            ESP(KK)=CPUMIN
            NFUZZ(KK)=0
 8214     CONTINUE
C
          DO8220JJ=1,NROW
            IF(II.EQ.JJ)GOTO8220
            ICLUS2=NCLUV(JJ)
            DO8222KK=1,NCOL
              ESP(KK)=X(JJ,KK)
 8222       CONTINUE
            CALL VECARI(RDRAW,ESP,NCOL,ICASPL,IWRITE,
     1                  EF,N3,ADIST,ITYP3,
     1                  IBUGA3,ISUBRO,IERROR)
            IF(ICLUS1.EQ.ICLUS2)THEN
              NFUZZ(ICLUS1)=NFUZZ(ICLUS1)+1
              IF(NFUZZ(ICLUS1).EQ.1)THEN
                DVEC(ICLUS1)=ADIST
              ELSE
                TERM1=(ADIST - DVEC(ICLUS1))/REAL(NFUZZ(ICLUS1))
                DVEC(ICLUS1)=DVEC(ICLUS1) + TERM1
              ENDIF
            ELSE
              NFUZZ(ICLUS2)=NFUZZ(ICLUS2)+1
              IF(NFUZZ(ICLUS2).EQ.1)THEN
                DVEC(ICLUS2)=ADIST
              ELSE
                TERM1=(ADIST - DVEC(ICLUS2))/REAL(NFUZZ(ICLUS2))
                DVEC(ICLUS2)=DVEC(ICLUS2) + TERM1
              ENDIF
            ENDIF
 8220     CONTINUE
C
          AI=DVEC(ICLUS1)
          BI=CPUMAX
          DO8230JJ=1,NCLUST
            IF(JJ.EQ.ICLUS1)GOTO8230
            IF(DVEC(JJ).LT.BI)BI=DVEC(JJ)
 8230     CONTINUE
          SYL=(BI - AI)/MAX(AI,BI)
          WRITE(IOUNI3,'(2E15.7)')REAL(NCLUV(II)),SYL
C
 8210   CONTINUE
      ENDIF
C
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9450)
 9450   FORMAT('THIS RUN HAS BEEN SUCCESSFULLY COMPLETED.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8091)
 8091   FORMAT('THE FUZZY CLUSTERING PROBABILITIES ARE WRITTEN TO ',
     1         'dpst1f.dat')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8092)
 8092   FORMAT('THE CLUSTER ID VALUES FOR THE CLOSEST HARD CLUSTER ',
     1         'ARE WRITTEN TO dpst2f.dat')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8099)
 8099   FORMAT('THE SILHOUETTE VALUES FOR THE CLOSEST HARD CLUSTER ',
     1         'ARE WRITTEN TO dpst3f.dat')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ******************
C               **   STEP 90--  **
C               **   EXIT       **
C               ******************
C
 9000 CONTINUE
C
      IF(IFLAGO.EQ.1)THEN
        IOP='CLOS'
        CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,
     1              IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1              IBUGA3,ISUBRO,IERROR)
      ENDIF
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FAN2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFAN2--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFBEX(IFBNAM,IANGLU,ISEED,IFTEXP,IFTORD,IFORSW,
     1                  IBUGA2,IBUGA3,IBUGCO,IBUGEV,IBUGQ,
     1                  ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--CARRY OUT A SUBSET OF THE LET COMMAND TO BE USED BY
C              THE "FUNCTION BLOCK".
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABOARATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2015/8
C     ORIGINAL VERSION--AUGUST    2015.
C     UPDATED         --DECEMBER  2015.  LET ... = EXECUTE ...
C     UPDATED         --MARCH     2019.  CALL LIST TO DPNONP
C     UPDATED         --JULY      2019.  TWEAK SCRATCH STORAGE
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IFBNAM
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
      INCLUDE 'DPCOFB.INC'
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
      ISUBN1='DPFB'
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
C               **  TREAT THE FUNCTION BLOCK CASE  **
C               *************************************
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'FBEX')THEN
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPFBEX--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IFBNAM,IANGLU,IFTEXP,IFORSW,ISEED
   52   FORMAT('IFBNAM,IANGLU,IFTEXP,IFORSW,ISEED, = ',
     1         A8,2X,3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGA2,IBUGA3,IBUGCO,IBUGEV,IBUGQ
   53   FORMAT('IBUGA2,IBUGA3,IBUGCO,IBUGEV,IBUGQ = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)IFBNA1,IFBNA2,IFBNA3
   55   FORMAT('IFBNA1,IFBNA2,IFBNA3 = ',2(A8,2X),A8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,57)IFBCN1,IFBCN2,IFBCN3
   57   FORMAT('IFBCN1,IFBCN2,IFBCN3 = ',3I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ******************************************
C               **  STEP 1--                            **
C               **  CHECK IF FUNCTION BLOCK IS DEFINED  **
C               ******************************************
C
      IFLAG=0
      IF(IFBNAM.EQ.IFBNA1)THEN
        IFLAG=1
        IFBCNT=IFBCN1
        IFBCN2=IFBCP1
      ELSEIF(IFBNAM.EQ.IFBNA2)THEN
        IFLAG=2
        IFBCNT=IFBCN2
        IFBCN2=IFBCP2
      ELSEIF(IFBNAM.EQ.IFBNA3)THEN
        IFLAG=3
        IFBCNT=IFBCN3
        IFBCN2=IFBCP3
      ELSE
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR IN FUNCTION BLOCK--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)IFBNAM
  102   FORMAT('      FUNCTION BLOCK ',A8,' HAS NOT BEEN DEFINED.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(IFBCNT.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)IFBNAM
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
      DO1000KK=1,IFBCNT
C
C       STEP 1: PUT THE FUNCTION BLOCK LINE INTO IANSLC
C
        IF(IFLAG.EQ.1)THEN
          DO1010II=1,256
            IANSLC(II)=' '
            IANSLC(II)(1:1)=IFBLI1(KK)(II:II)
 1010     CONTINUE
        ELSEIF(IFLAG.EQ.2)THEN
          DO1020II=1,256
            IANSLC(II)=' '
            IANSLC(II)(1:1)=IFBLI2(KK)(II:II)
 1020     CONTINUE
        ELSEIF(IFLAG.EQ.3)THEN
          DO1030II=1,256
            IANSLC(II)=' '
            IANSLC(II)(1:1)=IFBLI3(KK)(II:II)
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
        IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'FBEX')THEN
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
        IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'FBEX')THEN
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
C      CURRENTLY, FUNCTION BLOCKS ARE SUPPORTED BY THE FOLLOWING
C      COMMANDS:
C
C         1. PLOT FUNCTION
C         2. 3D-PLOT FUNCTION
C         3. LET ... = ROOTS ...
C         4. LET ... = OPTIMIZE ...
C         5. LET ... = INTEGRAL ...
C         6. LET ... = NUMERICAL DERIVATIVE ...
C
C      THE FOLLOWING COMMANDS THAT PERFORM FUNCTION EVALUATIONS ARE
C      NOT YET SUPPORTED:
C
C         1. ORTHOGONAL DISTANCE FIT ...
C         2. FIT Y = ...  (NON-LINEAR FIT)
C         3. PRE-FIT ...
C         4. LET ... = RUNGE KUTTA ...
C         5. LET ... = DERIVATIVE ...
C         6. LET ... = RECURSIVE FUNCTION ...  (COMMAND NOT IMPLEMENTED)
C
C      OF THESE, THE DERIVATIVE COMMAND WILL NOT BE IMPLEMNTED (FOR
C      FUNCTION BLOCKS, THE NUMERICAL DERIVATIVE CAN BE USED).  THE
C      OTHER COMMANDS WILL BE UPDATED TO SUPPORT FUNCTION BLOCKS.
C
C      CURRENTLY, RESTRICT TO:
C
C         1. PATTERN/DATA
C         2. RANDOM NUMBERS
C         3. MATH LET SUB-COMMANDS (BUT NOT MATRIX COMMANDS)
C         4. STATISTICS LET SUB-COMMANDS
C         5. ARITHMETIC OPERATIONS
C         6. LET ... = EXECUTE ...
C
C            NOTE THAT FOLLOWING MATH LET SUB-COMMANDS ARE NOT
C            HANDLED IN DPMATC AND ARE NOT SUPPORTED IN FUNCTION
C            BLOCKS:
C
C                 A. DERIVATIVE
C                 B. NUMERICAL DERIVATIVE
C                 C. INTEGRAL
C                 D. RUNGE-KUTTA
C                 E. OPTIMIZE
C                 F. ROOTS
C
C
C            THE ABOVE EITHER SUPPORT FUNCTION BLOCKS OR IT IS
C            PLANNED TO SUPPORT FUNCTIONS BLOCKS, SO THERE IS A
C            BIT OF CIRCULARITY IN TRYING TO INCLUDE THEM.
C
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
        IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'FBEX')THEN
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
        IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'FBEX')THEN
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
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'FBEX')THEN
            WRITE(ICOUT,4001)
 4001       FORMAT('BEFORE CALL CKSTAT')
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          CALL CKSTAT(IBUGA3,IFOUN8,ICASL8,ILOCV,ISTANR)
          IF(IFOUN8.EQ.'YES'.AND.ICASL8.NE.'UNKN'.AND.
     1       ILOCV.GE.1)THEN
            ICASLE='STAT'
            CALL DPSTAC(ICASL8,ILOCV,ISTANR,
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
        DO2190K=1,IFBCN2
          IH='    '
          IH2='    '
          IF(IFLAG.EQ.1)THEN
            IH=IFBPL1(K)(1:4)
            IH2=IFBPL1(K)(5:8)
          ELSEIF(IFLAG.EQ.2)THEN
            IH=IFBPL2(K)(1:4)
            IH2=IFBPL2(K)(5:8)
          ELSEIF(IFLAG.EQ.3)THEN
            IH=IFBPL3(K)(1:4)
            IH2=IFBPL3(K)(5:8)
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
C       DON'T SUPPORT THIS AS IT IS MORE USEFUL TO CALL DPFBEX FROM
C       DPFUEV (I.E., ALLOW FUNCTION EVALUATION TO HANDLE FUNCTION
C       BLOCKS).
C
        IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'FBEX')THEN
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
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'FBEX')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFBEX--')
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
      SUBROUTINE DPFEED(IHARG,NUMARG,
     1IFEED2,IFOUND,IERROR)
C
C     PURPOSE--SPECIFY THE FEEDBACK SWITCH WHICH IN TURN
C              DETERMINES WHETHER ANY SUBSEQUENT FEEDBACK OUTPUT
C              (LIKE, SAY, FROM A SUBSET SPECIFICATION)
C              WILL BE PRINTED OR NOT.
C              THIS CAPABILITY IS USEFUL IF ONE WISHES TO SUPPRESS
C              FEEDBACK OUTPUT FROM ALL SWITCH SETTING COMMANDS
C              SO AS TO NOT CLUTTER UP THE SCREEN
C              IN FORMING (FOR EXAMPLE) DIAGRAMMATIC GRAPHICS.
C              THE SPECIFIED FEEDBACK SWITCH SPECIFICATION
C              WILL BE PLACED IN THE HOLLERITH VARIABLE IFEED2.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C     OUTPUT ARGUMENTS--IFEED2 (A HOLLERITH VARIABLE)
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
C     ORIGINAL VERSION--MAY       1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --JANUARY   2009. ADD "SAVE/RESTORE" OPTION
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IFEED2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
C
      CHARACTER*4 IFEESV
      COMMON/IFEED/IFEESV
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
      IF(NUMARG.LE.0)GOTO1150
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1160
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      IF(IHARG(NUMARG).EQ.'SAVE')GOTO1170
      IF(IHARG(NUMARG).EQ.'REST')GOTO1175
      GOTO1199
C
 1150 CONTINUE
      IHOLD='ON'
      GOTO1180
C
 1160 CONTINUE
      IHOLD='OFF'
      GOTO1180
C
 1170 CONTINUE
      IFOUND='YES'
      IFEESV=IFEEDB
      GOTO1199
C
 1175 CONTINUE
      IFOUND='YES'
      IFEEDB=IFEESV
      GOTO1199
C
 1180 CONTINUE
      IFOUND='YES'
      IFEED2=IHOLD
      IFEEDB=IFEED2
C
CCCCC GOTO1189
CCCCC IF(IFEEDB.EQ.'OFF')GOTO1189
CCCCC WRITE(ICOUT,999)
CC999 FORMAT(1X)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,1181)IFEED2
C1181 FORMAT('THE FEEDBACK SWITCH HAS JUST BEEN SET TO ',
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC1A4)
C1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPFENC(IHARG,NUMARG,
     1IFENSW,IFOUND,IERROR)
C
C     PURPOSE--SPECIFY THE FENCE SWITCH WHICH IN TURN
C              DETERMINES WHETHER SUCCEEDING BOX PLOTS WILL HAVE
C              VALUES BEYOND THE INNER FENCE AND OUTER FENCE INDICATED.
C              THE SPECIFIED FENCE SWITCH SPECIFICATION
C              WILL BE PLACED IN THE CHARACTER VARIABLE IFENSW.
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C     OUTPUT ARGUMENTS--IFENSW (A CHARACTER VARIABLE)
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
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--83/7
C     ORIGINAL VERSION--JULY      1983.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IFENSW
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
C
      IF(NUMARG.LE.0)GOTO1150
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1160
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1160
      GOTO1150
C
 1150 CONTINUE
      IHOLD='ON'
      GOTO1180
C
 1160 CONTINUE
      IHOLD='OFF'
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      IFENSW=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)IFENSW
 1181 FORMAT('THE FENCE SWITCH (FOR BOX PLOTS) HAS JUST ',
     1'BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPFICN(ICOM,IHARG,IHARG2,IARGT,ARG,NUMARG,
     1                  IPARNC,IPANC2,IPAROP,
     1                  PARLIM,PARLLM,PARULM,
     1                  NUMCON,MAXCON,IFOUND,IERROR,IBUG)
C
C     PURPOSE--DEFINE CONSTRAINTS TO BE USED
C              IN CONJUNCTION WITH THE FIT COMMAND
C              (AND THE PRE-FIT COMMAND).
C              THE SPECIFIED CONSTRAINED PARAMETER NAME WILL BE PLACED
C              IN AN ELEMENT OF THE HOLLERITH VARIABLES
C              IPARNC(.) AND IPANC2(.).
C              THE SPECIFIED MATHEMATICAL OPERATION
C              (< OR <= OR = OR >= OR >)
C              INVOLVED WITH THE CONSTRAINT
C              WILL BE PLACED IN THE CORRESPONDING ELEMENT
C              OF THE HOLLARIRTH VECTOR IPAROP(.).
C              THE SPECIFIED NUMBER WHICH SERVES AS THE BOUNDARY VALUE
C              IN THE CONSTRAINT WILL BE PLACED IN THE CORRESPONDING
C              ELEMENT OF THE FLOATING POINT VECTOR PARLIM(.).
C     INPUT  ARGUMENTS--ICOM   (A  HOLLERITH VECTOR)
C                     --IHARG  (A  HOLLERITH VECTOR)
C                     --IHARG2 (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --ARG    (A  FLOATING POINT VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C     OUTPUT ARGUMENTS--IPARNC (A  HOLLERITH VECTOR)
C                     --IPANC2 (A  HOLLERITH VECTOR)
C                     --IPAROP (A  HOLLERITH VECTOR)
C                     --PARLIM (A  FLOATING POINT VECTOR)
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
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--JUNE      1979.
C     UPDATED         --JULY      1979.
C     UPDATED         --DECEMBER  1980.
C     UPDATED         --JANUARY   1981.
C     UPDATED         --NOVEMBER  1981.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
      CHARACTER*4 IHARG2
      CHARACTER*4 IARGT
      CHARACTER*4 IPARNC
      CHARACTER*4 IPANC2
      CHARACTER*4 IPAROP
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
      CHARACTER*4 IBUG
C
      CHARACTER*4 IH1
      CHARACTER*4 IH2
      CHARACTER*4 NEWCON
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
      DIMENSION IHARG2(*)
      DIMENSION IARGT(*)
      DIMENSION ARG(*)
C
      DIMENSION IPARNC(*)
      DIMENSION IPANC2(*)
      DIMENSION IPAROP(*)
      DIMENSION PARLIM(*)
      DIMENSION PARLLM(*)
      DIMENSION PARULM(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPFI'
      ISUBN2='CN  '
      NEWCON='UNKN'
C
      ICON=0
C
      IF(IBUG.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,61)
   61 FORMAT('***** AT THE BEGINNING OF DPFICN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,62)NUMARG
   62 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,63)ICOM
   63 FORMAT('ICOM = ',A4)
      CALL DPWRST('XXX','BUG ')
      IF(NUMARG.LE.0)GOTO67
      DO65I=1,NUMARG
      WRITE(ICOUT,66)I,IHARG(I),IHARG2(I),ARG(I)
   66 FORMAT('I,IHARG(I),IHARG2(I),ARG(I) = ',
     1I8,2X,A4,2X,A4,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
   67 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,72)NUMCON,MAXCON,NEWCON,IBUG
   72 FORMAT('NUMCON,MAXCON,NEWCON,IBUG = ',I8,I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      IF(NUMCON.LE.0)GOTO77
      DO75I=1,NUMCON
      WRITE(ICOUT,76)I,IPARNC(I),IPANC2(I),IPAROP(I),PARLIM(I)
   76 FORMAT('I,IPARNC(I),IPANC2(I),IPAROP(I),PARLIM(I) = ',
     1I8,2X,A4,2X,A4,2X,A4,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
   75 CONTINUE
   77 CONTINUE
C
   90 CONTINUE
C
C               **********************************************
C               **  STEP 1--                                **
C               **  DETERMINE IF HAVE THE TOTAL RESET CASE  **
C               **********************************************
C
      ISTEPN='1'
      IF(IBUG.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(NUMARG.GE.1.AND.ICOM.EQ.'FIT'.AND.IHARG(1).EQ.'CONS'.AND.
     1IHARG2(1).EQ.'TRAI')GOTO100
      GOTO900
C
  100 CONTINUE
      IF(NUMARG.LE.1)GOTO110
      IF(NUMARG.EQ.2.AND.IHARG(2).EQ.'ON')GOTO110
      IF(NUMARG.EQ.2.AND.IHARG(2).EQ.'OFF')GOTO110
      IF(NUMARG.EQ.2.AND.IHARG(2).EQ.'AUTO')GOTO110
      IF(NUMARG.EQ.2.AND.IHARG(2).EQ.'DEFA')GOTO110
      GOTO190
C
  110 CONTINUE
      IFOUND='YES'
      DO120I=1,MAXCON
      IPARNC(I)='    '
      IPANC2(I)='    '
      IPAROP(I)='NONE'
      PARLIM(I)=CPUMIN
  120 CONTINUE
      NUMCON=0
C
      IF(IFEEDB.EQ.'OFF')GOTO129
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,121)
  121 FORMAT('ALL PARAMETERS HAVE JUST BEEN SET SO AS ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,122)
  122 FORMAT('    TO BE UNCONSTRAINED')
      CALL DPWRST('XXX','BUG ')
  129 CONTINUE
      GOTO900
C
  190 CONTINUE
C
C               ********************************************************
C               **  STEP 2--                                          **
C               **  DETERMINE IF NAME OF PARAMETER TO BE CONSTRAINED  **
C               **  ALREADY EXISTS IN CONSTRAINT TABLE.               **
C               ********************************************************
C
      ISTEPN='2'
      IF(IBUG.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IH1=IHARG(2)
      IH2=IHARG2(2)
C
CC    NEWCON='NO'
CC    ICON=0
CC    IF(NUMCON.LE.0)GOTO220
CC    DO200I=1,NUMCON
CC    I2=I
CC    IF(IH1.EQ.IPARNC(I).AND.IH2.EQ.IPANC2(I))GOTO210
CC200 CONTINUE
CC    GOTO220
CC
CC210 CONTINUE
CC    ICON=I2
CC    GOTO290
CC
CC220 CONTINUE
      ICON=NUMCON+1
      IF(ICON.LE.MAXCON)GOTO229
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,221)
  221 FORMAT('***** ERROR IN DPFICN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,222)
  222 FORMAT('      THE NUMBER OF CONSTRAINTS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,224)
  224 FORMAT('      HAS JUST EXCEEDED THE MAXIMUM SIZE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,225)MAXCON
  225 FORMAT('      (',I5,') OF THE INTERNAL CONSTRAINT TABLE.')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO900
  229 CONTINUE
C
      NEWCON='YES'
      NUMCON=ICON
      GOTO290
C
  290 CONTINUE
C
C               ***********************************************
C               **  STEP 3--                                 **
C               **  ENTER THE PARAMETER NAME (IF NECESSARY)  **
C               **  INTO THE NAME VECTORS IPARNC(.) AND      **
C               **  IPANC2(.)                                **
C               ***********************************************
C
      ISTEPN='3'
      IF(IBUG.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IPARNC(ICON)=IH1
      IPANC2(ICON)=IH2
C
C               ******************************************
C               **  STEP 4--                            **
C               **  ENTER THE CONSTRAINT OPERATION      **
C               **  INTO THE OPERATION VECTOR IPAROP(.) **
C               ******************************************
C
      ISTEPN='4'
      IF(IBUG.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IPAROP(ICON)='NONE'
      IF(NUMARG.LE.3)GOTO410
      IF(IHARG(3).EQ.'ON')GOTO410
      IF(IHARG(3).EQ.'OFF')GOTO410
      IF(IHARG(3).EQ.'DEFA')GOTO410
      IF(IHARG(3).EQ.'AUTO')GOTO410
C
      IF(IHARG(3).EQ.'<'.AND.IHARG(4).NE.'=')GOTO420
      IF(IHARG(3).EQ.'<'.AND.IHARG(4).EQ.'=')GOTO430
      IF(IHARG(3).EQ.'='.AND.IHARG(4).EQ.'<')GOTO430
      IF(IHARG(3).EQ.'='.AND.IHARG(4).NE.'<'.AND.
     1IHARG(4).NE.'>')GOTO440
      IF(IHARG(3).EQ.'>'.AND.IHARG(4).EQ.'=')GOTO450
      IF(IHARG(3).EQ.'='.AND.IHARG(4).EQ.'>')GOTO450
      IF(IHARG(3).EQ.'>'.AND.IHARG(4).NE.'=')GOTO460
      GOTO470
C
  410 CONTINUE
      IPAROP(ICON)='NONE'
      GOTO490
C
  420 CONTINUE
      IPAROP(ICON)='<'
      GOTO490
C
  430 CONTINUE
      IPAROP(ICON)='<='
      GOTO490
C
  440 CONTINUE
      IPAROP(ICON)='='
      GOTO490
C
  450 CONTINUE
      IPAROP(ICON)='>='
      GOTO490
C
  460 CONTINUE
      IPAROP(ICON)='>'
      GOTO490
C
  470 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,471)
  471 FORMAT('ERROR IN DPFICN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,472)
  472 FORMAT('      THE SECOND ARGUMENT IN THE FIT CONSTRAINT ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,473)
  473 FORMAT('      COMMAND SHOULD BE ONE OF THE FOLLOWING 5  ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,474)
  474 FORMAT('      MATHEMATICAL OPERATIONS-- <   <=   =   >=   >')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,475)
  475 FORMAT('      OR SHOULD BE ONE OF THE FOLLOWING 4 WORDS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,476)
  476 FORMAT('      ON    OFF    AUTOMATIC    DEFAULT,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,477)
  477 FORMAT('      BUT WAS NOT.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,478)
  478 FORMAT('      THE FOLLOWING ILLUSTRATIVE EXAMPLE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,479)
  479 FORMAT('      DEMONSTRATES THE ALLOWABLE FORM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,480)
  480 FORMAT('      SUPPOSE THE ANALYST WISHES TO')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,481)
  481 FORMAT('      CONSTRAIN THE PARAMETER ALPHA IN A FIT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,482)
  482 FORMAT('      TO BE STRICTLY GREATER THAN 0 AND')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,483)
  483 FORMAT('      ALSO TO BE LESS THAN OR EQUAL TO 100,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,484)
  484 FORMAT('      THEN THE FOLLOWING MAY BE ENTERED--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,485)
  485 FORMAT('      FIT CONSTRAINT ALPHA > 0')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,486)
  486 FORMAT('      FIT CONSTRAINT ALPHA <= 100')
      CALL DPWRST('XXX','BUG ')
      IF(NEWCON.EQ.'NO')GOTO489
      NUMCON=NUMCON-1
      IPARNC(ICON)='    '
      IPANC2(ICON)='    '
  489 CONTINUE
      GOTO900
C
  490 CONTINUE
C
C               **************************************
C               **  STEP 5--                        **
C               **  ENTER THE CONSTRAINT LIMITS     **
C               **  INTO THE VECTOR PARLIM(.)       **
C               **************************************
C
      ISTEPN='5'
      IF(IBUG.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPAROP(ICON).EQ.'NONE')GOTO590
      IF(IARGT(NUMARG).EQ.'NUMB')GOTO510
      GOTO570
C
  510 CONTINUE
      IFOUND='YES'
      PARLIM(ICON)=ARG(NUMARG)
      GOTO590
C
  570 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,571)
  571 FORMAT('ERROR IN DPFICN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,572)
  572 FORMAT('      THE THIRD ARGUMENT IN THE FIT CONSTRAINT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,573)
  573 FORMAT('      COMMAND SHOULD BE A NUMBER ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,574)
  574 FORMAT('      OR A PREVIOUSLY-DEFINED PARAMETER,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,575)
  575 FORMAT('      BUT WAS NOT.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,576)
  576 FORMAT('      THE FOLLOWING ILLUSTRATIVE EXAMPLE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,577)
  577 FORMAT('      DEMONSTRATES THE ALLOWABLE FORM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,578)
  578 FORMAT('      SUPPOSE THE ANALYST WISHES TO')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,579)
  579 FORMAT('      CONSTRAIN THE PARAMETER ALPHA IN A FIT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,580)
  580 FORMAT('      TO BE STRICTLY GREATER THAN 0 AND')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,581)
  581 FORMAT('      ALSO TO BE LESS THAN OR EQUAL TO 100,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,582)
  582 FORMAT('      THEN THE FOLLOWING MAY BE ENTERED--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,583)
  583 FORMAT('      FIT CONSTRAINT ALPHA > 0')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,584)
  584 FORMAT('      FIT CONSTRAINT ALPHA <= 100')
      CALL DPWRST('XXX','BUG ')
      IF(NEWCON.EQ.'NO')GOTO589
      NUMCON=NUMCON-1
      IPARNC(ICON)='    '
      IPANC2(ICON)='    '
  589 CONTINUE
      GOTO900
  590 CONTINUE
C
C               ****************************
C               **  STEP 6--              **
C               **  WRITE OUT A MESSAGE.  **
C               ****************************
C
      ISTEPN='6'
      IF(IBUG.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPAROP(ICON).EQ.'NONE')GOTO610
      IF(IPAROP(ICON).EQ.'<')GOTO620
      IF(IPAROP(ICON).EQ.'<=')GOTO630
      IF(IPAROP(ICON).EQ.'=')GOTO640
      IF(IPAROP(ICON).EQ.'>=')GOTO650
      IF(IPAROP(ICON).EQ.'>')GOTO660
      GOTO690
C
  610 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO619
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,611)IPARNC(ICON),IPANC2(ICON)
  611 FORMAT('THE PARAMETER ',A4,A4,' HAS JUST BEEN SET')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,612)
  612 FORMAT('    SO AS TO BE UNCONSTRAINED')
      CALL DPWRST('XXX','BUG ')
  619 CONTINUE
      GOTO670
C
  620 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO629
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,621)IPARNC(ICON),IPANC2(ICON)
  621 FORMAT('THE PARAMETER ',A4,A4,' HAS JUST BEEN CONSTRAINED ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,622)PARLIM(ICON)
  622 FORMAT('    TO BE STRICTLY LESS THAN ',E15.7)
      CALL DPWRST('XXX','BUG ')
  629 CONTINUE
      GOTO690
C
  630 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO639
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,631)IPARNC(ICON),IPANC2(ICON)
  631 FORMAT('THE PARAMETER ',A4,A4,' HAS JUST BEEN CONSTRAINED ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,632)PARLIM(ICON)
  632 FORMAT('    TO BE LESS THAN OR EQUAL TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
  639 CONTINUE
      GOTO690
C
  640 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO649
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,641)IPARNC(ICON),IPANC2(ICON)
  641 FORMAT('THE PARAMETER ',A4,A4,' HAS JUST BEEN CONSTRAINED ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,642)PARLIM(ICON)
  642 FORMAT('    TO BE IDENTICALLY EQUAL TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
  649 CONTINUE
      GOTO690
C
  650 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO659
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,651)IPARNC(ICON),IPANC2(ICON)
  651 FORMAT('THE PARAMETER ',A4,A4,' HAS JUST BEEN CONSTRAINED ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,652)PARLIM(ICON)
  652 FORMAT('    TO BE GREATER THAN OR EQUAL TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
  659 CONTINUE
      GOTO690
C
  660 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO669
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,661)IPARNC(ICON),IPANC2(ICON)
  661 FORMAT('THE PARAMETER ',A4,A4,' HAS JUST BEEN CONSTRAINED ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,662)PARLIM(ICON)
  662 FORMAT('    TO BE STRICTLY GREATER THAN ',E15.7)
      CALL DPWRST('XXX','BUG ')
  669 CONTINUE
      GOTO690
C
  670 CONTINUE
      NUMCO2=NUMCON
      IF(NUMCON.LE.0)GOTO679
      DO671I=1,NUMCON
      IF(I.GT.NUMCO2)GOTO679
      I2=I
      IF(IH1.EQ.IPARNC(I).AND.IH2.EQ.IPANC2(I))GOTO672
      GOTO671
C
  672 CONTINUE
      J=I
      JM1=J-1
      JMIN=I+1
      JMAX=NUMCO2
      IF(JMIN.GT.JMAX)GOTO674
      DO673J=JMIN,JMAX
      JM1=J-1
      IPARNC(JM1)=IPARNC(J)
      IPANC2(JM1)=IPANC2(J)
      IPAROP(JM1)=IPAROP(J)
      PARLIM(JM1)=PARLIM(J)
  673 CONTINUE
  674 CONTINUE
      NUMCO2=JM1
C
  671 CONTINUE
  679 CONTINUE
      NUMCON=NUMCO2
      GOTO690
C
  690 CONTINUE
C
C               ****************
C               **  STEP 9--  **
C               **  EXIT      **
C               ****************
C
  900 CONTINUE
      IF(IBUG.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,901)
  901   FORMAT('***** AT THE END OF DPFICN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,902)NUMCON,MAXCON,ICON,NEWCON,IBUG
  902   FORMAT('NUMCON,MAXCON,ICON,NEWCON,IBUG = ',3I8,2(2X,A4))
        CALL DPWRST('XXX','BUG ')
        IF(NUMCON.GE.1)THEN
          DO910I=1,NUMCON
            WRITE(ICOUT,911)I,IPARNC(I),IPANC2(I),IPAROP(I),
     1                      PARLIM(I),PARLLM(I),PARULM(I)
  911       FORMAT('I,IPARNC(I),IPANC2(I),IPAROP(I),PARLIM(I)',
     1             'PARLLM(I),PARULM(I) = ',I8,3(2X,A4),3G15.7)
            CALL DPWRST('XXX','BUG ')
  910     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFIFO(IHARG,NUMARG,
     1IOUTTY,IFOUND,IERROR)
C
C     PURPOSE--SET THE FORMAT/TYPE SWITCH FOR THE OUTPUT FILE.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C     OUTPUT ARGUMENTS--IOUTTY (A HOLLERITH VARIABLE)
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
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--92/4
C     ORIGINAL VERSION--MARCH     1992.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IOUTTY
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
C
      IF(NUMARG.LE.0)GOTO1150
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      IF(IHARG(NUMARG).EQ.'?')GOTO1160
      GOTO1170
C
 1150 CONTINUE
      IHOLD='ASCI'
      GOTO1180
C
 1160 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1169
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1161)IOUTTY
 1161 FORMAT('THE CURRENT FORMAT OF THE OUTPUT FILE IS ',A4)
      CALL DPWRST('XXX','BUG ')
 1169 CONTINUE
      IFOUND='YES'
      GOTO1199
C
 1170 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      IOUTTY=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)IOUTTY
 1181 FORMAT('THE OUTPUT FILE FORMAT SWITCH HAS JUST ',
     1'BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPFIIT(IHARG,IARGT,IARG,NUMARG,IDEFFI,
     1IFITIT,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE UPPER BOUND FOR THE NUMBER OF FIT ITERATIONS.
C              THE SPECIFIED FIT ITERATION VALUE WILL BE PLACED
C              IN THE INTEGER VARIABLE IFITIT.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --IARG   (AN INTEGER VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C                     --IDEFFI (AN INTEGER VARIABLE)
C     OUTPUT ARGUMENTS--IFITIT (AN INTEGER VARIABLE)
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
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(NUMARG.EQ.0)GOTO1199
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'=')GOTO1199
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'ITER')GOTO1110
      GOTO1199
C
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'ITER')GOTO1150
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
 1121 FORMAT('***** ERROR IN DPFIIT--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR FIT ITERATIONS ',
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
 1126 FORMAT('      A NON-LINEAR FIT , ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1127)
 1127 FORMAT('      AND SUPPOSE THE ANALYST WISHES TO TERMINATE  ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1128)
 1128 FORMAT('      THE FIT IF THE NUMBER OF ITERATIONS ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1129)
 1129 FORMAT('      HAPPENS TO REACH 30;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1130)
 1130 FORMAT('      THEN THE ALLOWABLE FORM IS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1131)
 1131 FORMAT('      FIT ITERATIONS 30 ')
      CALL DPWRST('XXX','BUG ')
      GOTO1199
C
 1150 CONTINUE
      IHOLD=IDEFFI
      GOTO1180
C
 1160 CONTINUE
      IHOLD=IARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      IFITIT=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)IFITIT
 1181 FORMAT('THE FIT ITERATIONS HAVE JUST BEEN SET TO ',
     1I8)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPFILE(IANS,IWIDTH,IWORD,
     1                  IOFILE,IBUGS2,ISUBRO,IERROR)
C
C     PURPOSE--SCAN THE    IWORD-TH   WORD OF THE INPUT LINE.
C              AND DETERMINE IF IT IS A FILE NAME.
C              THE CRITERION IS THAT IF THAT WORD
C              CONTAINS THE CHARACTER    IFCHAR   ,
C              THEN IT IS CONSIDERED A FILE NAME,
C              OTHERWISE IT IS CONSIDERED NOT TO BE A FILE NAME.
C     OUTPUT ARGUMENT--IOFILE ('YES' OR 'NO')
C     NOTE--THIS SUBROUTINE IS "SYSTEM-DEPENDENT" IN THE SENSE
C           THAT IFCHAR MAY DIFFER FROM ONE SYSTEM TO ANOTHER.
C     NOTE--IFCHAR IS SET AT TIMPLEMENTATION TIME
C           IN THE SUBROUTINE INITFO.
C     NOTE--THE DEFAULT SETTING FOR IFCHAR IS . (= PERIOD).
C           THUS YOU MAY ENTER  READ X. Y Z
C           TO TELL DATAPLOT TO READ VARIABLES Y AND Z
C           FROM FILE X
C           AS OPPOSED TO ENTERING   READ X Y Z
C           TO TELL DATAPLOT TO READ VARIABLES X, Y, AND Z
C           FROM THE TERMINAL.
C           READ X. Y Z
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
C     ORIGINAL VERSION--NOVEMBER  1977.
C     UPDATED         --OCTOBER   1978.
C     UPDATED         --NOVEMBER  1980.
C     UPDATED         --JUNE      1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --DECEMBER  1986.
C     UPDATED         --DECEMBER  1988. DESLATTES FILE NAME INSIDE QUOTE PROBLEM
C     UPDATED         --JULY      2002. OPTION (IFILQU=ON/OFF) TO
C                                       DETERMINE IF FILE NAME CAN
C                                       BE ENCLOSED IN QUOTES
C     UPDATED         --JULY      2003. BUG: EVEN THOUGH FILE NAMES
C                                       MAY BE RESTRICTED TO 80
C                                       CHARACTERS, THE COMMAND LINE
C                                       CONTAINING THEM CAN BE
C                                       LONGER.  ADJUST DIMENSIONING
C                                       TO ACCOUNT FOR THIS.  ALSO ADD
C                                       CHECK FOR FILE NAMES EXCEEDING
C                                       80 CHARACTERS.
C     UPDATED         --FEBRUARY  2008. ADD FILE NAME QUOTE NOFILE
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IANS(*)
      CHARACTER*4 IOFILE
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 IANSI
      CHARACTER*1024 ICANS
      CHARACTER*1024 ISTRIN
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      PARAMETER (MAXFNC=80)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPFI'
      ISUBN2='LE  '
      IERROR='NO'
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'FILE')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPFILE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IWIDTH,IWORD
   52   FORMAT('IWIDTH,IWORD = ',2I8)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,53)(IANS(I),I=1,MIN(100,IWIDTH))
   53     FORMAT('IANS(.) = ',100A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        WRITE(ICOUT,54)IFCHARS,IFILQU
   54   FORMAT('IFCHAR,IFILQU = ',A1,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***************************************
C               **  STEP 1--                         **
C               **  DETERMINE IF HAVE THE FILE CASE  **
C               ***************************************
C
      ISTEPN='1'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'FILE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO1110I=1,MIN(1024,IWIDTH)
        IANSI=IANS(I)
        ICANS(I:I)=IANSI(1:1)
 1110 CONTINUE
C
      ISTART=1
      ISTOP=MIN(IWIDTH,1024)
      CALL DPEXWO(ICANS,ISTART,ISTOP,IWORD,
     1ICOL1,ICOL2,ISTRIN,NCSTRI,
     1IBUGS2,ISUBRO,IERROR)
C
      IOFILE='NO'
      IF(NCSTRI.LE.0)GOTO1290
C     THE FOLLOWING LINE WAS INSERTED DECEMBER 1988 TO
C     SOLVE THE DESLATTES PROBLEM    WRITE "(EXAMPLE--ABC.DEF)"
C     JULY 2002: MAKE QUOTE OPTIONAL (PC FILES CAN HAVE SPACES,
C     SO ENCLOSE IN QUOTES TO EXTRACT)
CCCCC IF(ICANS(1:1).EQ.'"')GOTO1290
CCCCC
CCCCC FEBRUARY 2008: IF FILE NAME QUOTE IS "OFF" OR "NOFILE",
CCCCC                THEN DON'T CHECK FOR FILE NAME.
CCCCC
CCCCC IF(ICANS(1:1).EQ.'"' .AND. IFILQU.EQ.'OFF')GOTO1290
      IF(ICANS(ICOL1:ICOL1).EQ.'"' .AND. IFILQU.EQ.'OFF')GOTO1290
      IF(ICANS(ICOL1:ICOL1).EQ.'"' .AND. IFILQU.EQ.'NOFI')GOTO1290
      IF(ICOL1.GT.ICOL2)GOTO1290
      DO1200I=ICOL1,ICOL2
      IF(ICANS(I:I).EQ.IFCHAR)GOTO1250
 1200 CONTINUE
      GOTO1290
 1250 CONTINUE
      IOFILE='YES'
      NC=ICOL2-ICOL1+1
      IF(IFILQU.EQ.'ON' .AND. ICANS(ICOL1:ICOL1).EQ.'"')NC=NC-1
      IF(IFILQU.EQ.'ON' .AND. ICANS(ICOL2:ICOL2).EQ.'"')NC=NC-1
      IF(NC.GT.MAXFNC)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1251)MAXFNC
 1251   FORMAT('***** FATAL ERROR: FILE NAME EXCEEDS MAXIMUM ',
     1         'LENGTH OF ',I8,' CHARACTERS.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1253)NC
 1253   FORMAT('      REQUESTED FILE NAME HAS ',I8,' CHARACTERS.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
      GOTO1290
 1290 CONTINUE
C
C               ****************
C               **  STEP 90-- **
C               **  EXIT.     **
C               ****************
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'OFF'.AND.ISUBRO.NE.'FILE')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END      OF DPFILE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IWIDTH,IWORD
 9012 FORMAT('IWIDTH,IWORD = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,9013)(IANS(I),I=1,MIN(100,IWIDTH))
 9013   FORMAT('IANS(.) = ',100A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      WRITE(ICOUT,9014)IFCHAR
 9014 FORMAT('IFCHAR = ',A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)ICOL1,ICOL2,NCSTRI
 9015 FORMAT('ICOL1,ICOL2,NCSTRI = ',3I8)
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,9021)(ICANS(I:I),I=1,MIN(100,IWIDTH))
 9021   FORMAT('ICANS(.:.) = ',100A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9022)(ISTRIN(I:I),I=1,MIN(100,IWIDTH))
 9022   FORMAT('ISTRIN(.:.) = ',100A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      WRITE(ICOUT,9031)IBUGS2,IERROR
 9031 FORMAT('IBUGS2,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9032)IOFILE
 9032 FORMAT('IOFILE = ',A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPFIL2(ICHAR,IMIN,IMAX,IANS2,IWID,
     1LOCCHA,NAM,NPACKC,IBUG,IERROR)
C
C     PURPOSE--EXTRACT QUALIFIER, FILE, OR SUBFILE
C              NAME FROM A STRING.
C     INPUT  ARGUMENTS--IMIN   = INTEGER VARIABLE
C                                CONTAINING THE START LOCATION
C                                (IN THE VECTOR IANS2(.))
C                                FOR THE SEARCH.
C                     --IMAX   = INTEGER VARIABLE
C                                CONTAINING THE STOP LOCATION
C                                (IN THE VECTOR IANS2(.))
C                                FOR THE SEARCH.
C                     --ICHAR  = HOLLERITH VARIABLE GIVING
C                                THE SOUGHT-AFTER CHARACTER
C                                IN THE SEARCH.
C                     --IANS2  = HOLLERITH VECTOR BEING SEARCHED.
C                     --IWID   = THE NUMBER OF ELEMENTS
C                                IN THE HOLLERITH VECTOR IANS2(.)
C     OUTPUT ARGUMENTS--LOCCHA = INTEGER VARIABLE
C                                CONTAINING THE LOCATION
C                                (IN THE VECTR IANS2(.))
C                                WHERE THE CHARACTER WAS FOUND.
C                     --NAM    = HOLLERITH VECTOR
C                                INTO WHICH THE PACKED NAME
C                                IS PLACED.
C                     --NPACKC = INTEGER VARIABLE
C                                CONTAINING THE NUMBER OF WORDS
C                                IN THE VARIABLE NAM(.) FOR
C                                THE PACKED VERSION OF THE
C                                QUALIFIER, FILE, AND/OR SUBFILE NAME
C                                (WHERE THE WORDS ARE PACKED--
C                                4, 6, 10, ETC. CHARACTERS PER WORD).
C     NOTE--IF THE NAME DOES NOT EXIST,
C           THE LOCCHA IS SET TO IMIN-1,
C           NAM(.) IS FILLED WITH BLANKS,
C           AND NPACKC IS SET TO 0   .
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
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--JUNE        1981.
C     UPDATED         --NOVEMBER  1981.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICHAR
      CHARACTER*4 IANS2
      CHARACTER*4 NAM
      CHARACTER*4 IBUG
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION IANS2(*)
      DIMENSION NAM(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IERROR='NO'
C
      IF(IBUG.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPFIL2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHAR,IMIN,IMAX
   52 FORMAT('ICHAR,IMIN,IMAX = ',A4,2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IWID
   53 FORMAT('IWID = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)(IANS2(I),I=1,IWID)
   54 FORMAT('IANS2(.)--',120A1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,55)IBUG,IERROR
   55 FORMAT('IBUG,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               ****************************************
C               **  STEP 1--                          **
C               **  ZERO-OUT AND BLANK-OUT            **
C               **  THE OUTPUT VARIABLES AND VECTOR.  **
C               ****************************************
C
      LOCCHA=IMIN-1
      NPACKC=0
C
      DO1110J=1,10
      NAM(J)=' '
 1110 CONTINUE
C
C               *******************************************
C               **  STEP 2--                             **
C               **  SEARCH FOR THE TARGET CHARACTER;     **
C               **  DETERMINE ITS LOCATION IN IANS2(.);  **
C               **  PLACE THE LOCATION VALUE IN LOCCHA.  **
C               *******************************************
C
      IF(ICHAR.EQ.'END')GOTO1126
      IF(IMAX.LE.0)GOTO1190
      IF(IMIN.GT.IMAX)GOTO1190
      DO1120I=IMIN,IMAX
      I2=I
      IF(IBUG.EQ.'ON')WRITE(ICOUT,1111)I,IANS2(I),ICHAR
 1111 FORMAT('I,IANS2(I),ICHAR = ',I6,A6,A6)
      IF(IBUG.EQ.'ON')CALL DPWRST('XXX','BUG ')
      IF(IANS2(I).EQ.ICHAR)GOTO1125
 1120 CONTINUE
      GOTO1190
 1125 CONTINUE
      LOCCHA=I2
      GOTO1129
 1126 CONTINUE
      LOCCHA=IMAX+1
      GOTO1129
 1129 CONTINUE
C
C               *************************************************
C               **  STEP 3--                                   **
C               **  EXTRACT THE NAME BETWEEN LOCATION IMIN     **
C               **  AND THE LOCATION OF THE TARGET CHARACTER.  **
C               **  PACK THE NAME INTO NAM(.)                  **
C               **  COMPUTE NPACKC = THE NUMBER OF PACKED WORDS**
C               **  IN NAM(.) NEEDED FOR THE NAME.             **
C               *************************************************
C
      NUMCH=0
      IMAX2=LOCCHA-1
      IF(IMAX2.LE.0)GOTO1190
      IF(IMIN.GT.IMAX2)GOTO1190
      DO1130I=IMIN,IMAX2
CCCCC J=((I-IMIN)/NUMBPC)+1
      J=((I-IMIN)/NUMCPW)+1
      IF(IANS2(I).EQ.' ')GOTO1130
      NUMCH=NUMCH+1
      ISTAR3=(NUMBPC*(NUMCH-1)) - (NUMBPW*(J-1))
      ISTAR3=IABS(ISTAR3)
      CALL DPCHEX(0,NUMBPC,IANS2(I),ISTAR3,NUMBPC,NAM(J))
 1130 CONTINUE
      NPACKC=J
C
 1190 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
      IF(IBUG.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFIL2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR,ICHAR,IMIN,IMAX,IWID
 9012   FORMAT('IERROR,ICHAR,IMIN,IMAX = ',2(A4,2X),2X,3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)(IANS2(I),I=1,IWID)
 9014   FORMAT('IANS2(.)--',120A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)LOCCHA,NPACKC
 9016   FORMAT('LOCCHA,NPACKC = ',2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9017)(NAM(I),I=1,10)
 9017   FORMAT('NAM(.)--',10A6)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFILL(IHARG,NUMARG,
     1IDEFFI,
     1ITEXFI,
     1IBUGD2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE FILL SWITCH (ON OR OFF) FOR
C              TEXT SCRIPT AND OTHER DIAGRAMMATIC FIGURES
C              ON A PLOT.
C              THE FILL SWITCH WILL BE PLACED
C              IN THE CHARACTER VARIABLE ITEXFI.
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --NUMARG
C                     --IDEFFI
C                     --IBUGD2
C     OUTPUT ARGUMENTS--ITEXFI
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
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--APRIL     1981.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IDEFFI
      CHARACTER*4 ITEXFI
      CHARACTER*4 IBUGD2
      CHARACTER*4 ISUBRO
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
      IF(IBUGD2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPFILL--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IDEFFI
   53 FORMAT('IDEFFI = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)NUMARG
   54 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO55I=1,NUMARG
      WRITE(ICOUT,56)I,IHARG(I)
   56 FORMAT('I,IHARG(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
C
C               ************************************
C               **  TREAT THE FILL          CASE  **
C               ************************************
C
      IF(NUMARG.LE.0)GOTO1161
      IF(IHARG(NUMARG).EQ.'ON')GOTO1161
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1162
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1161
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1165
      GOTO1170
C
 1161 CONTINUE
      ITEXFI='ON'
      GOTO1180
C
 1162 CONTINUE
      ITEXFI='OFF'
      GOTO1180
C
 1165 CONTINUE
      ITEXFI=IDEFFI
      GOTO1180
C
 1170 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,1171)
 1171 FORMAT('***** ERROR IN DPFILL--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1172)
 1172 FORMAT('      ILLEGAL ENTRY FOR FILL ',
     1'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1173)
 1173 FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE ',
     1'PROPER FORM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1174)
 1174 FORMAT('      SUPPOSE THE THE ANALYST WISHES ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1175)
 1175 FORMAT('      TO HAVE ALL TEXT AND FIGURES FILLED,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1177)
 1177 FORMAT('      THEN ALLOWABLE FORMS ARE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1178)
 1178 FORMAT('           FILL ON ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1179)
 1179 FORMAT('           FILL ')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
 1180 CONTINUE
      IFOUND='YES'
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE FILL (FOR TEXT AND FIGURES) ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)ITEXFI
 1182 FORMAT('HAS JUST BEEN SET TO ',A4)
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
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPFILL')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,ISUBRO,IFOUND,IERROR
 9012 FORMAT('IBUGD2,ISUBRO,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IDEFFI,ITEXFI
 9013 FORMAT('IDEFFI,ITEXFI = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPFIMA(PXMIN,PYMIN,PXMAX,PYMAX,
     1                  ICASPL,ICAS3D,
     1                  IMARCO,IMARC2)
C
C     PURPOSE--FILL  THE MARGIN REGION ON THE SCREEN
C              (THE REGION OUTSIDE THE FRAME LINES).
C
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
C     VERSION NUMBER--83.6
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MAY       1983.
C     UPDATED         --FEBRUARY  1988. STAR PLOT
C     UPDATED         --JUNE      1988. CALL TO GRFIRE
C     UPDATED         --JANUARY   1989. MODIFY CALL  TO GRFIRE (ALAN)
C     UPDATED         --OCTOBER   2020. SUPPORT FOR RGB COLOR
C
C-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 ICAS3D
      CHARACTER*4 IMARCO
C
      DIMENSION IMARC2(3)
C
      CHARACTER*4 IFIG
      CHARACTER*4 IPATT
      CHARACTER*4 ICOLB
      CHARACTER*4 ICOLP
      CHARACTER*4 ICOL
C
      CHARACTER*4 ICASE
      CHARACTER*4 ICASE2
C
      CHARACTER*4 IHORPA
      CHARACTER*4 IVERPA
      CHARACTER*4 IDUPPA
      CHARACTER*4 IDDOPA
C
      CHARACTER*4 IPATT2
C
      DIMENSION PX(10)
      DIMENSION PY(10)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
C
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGG4.EQ.'ON' .OR. ISUBG4.EQ.'FIMA')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPFIMA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)PXMIN,PYMIN,PXMAX,PYMAX
   52   FORMAT('PXMIN,PYMIN,PXMAX,PYMAX = ',4F10.5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,ICAS3D,IMARCO
   53   FORMAT('ICASPL,ICAS3D,IMARCO = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,59)IBUGG4,ISUBG4,IERRG4
   59   FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IPATT2='SOLI'
C
      IF(ICASPL.EQ.'PIEC')GOTO9000
      IF(ICASPL.EQ.'STAR')GOTO9000
      IF(ICAS3D.EQ.'ON')GOTO9000
C
C               **********************************
C               **  STEP 0--                    **
C               **  COPY OVER THE MARGIN COLOR  **
C               **********************************
C
      ICASE='REGI'
      IFIG='BOX'
      IPATT='SOLI'
      IF(IGCOLO.EQ.'OFF')IPATT='EMPT'
      PTHICK=0.0
      PXGAP=0.0
      PYGAP=0.0
      ICOLB=IMARCO
      ICOLP=IMARCO
      ICOLFR=IMARC2(1)
      ICOLFG=IMARC2(2)
      ICOLFB=IMARC2(3)
C
C               **********************************************
C               **  STEP 1--                                **
C               **  TRANSLATE THE CHARACTER REPRESENTATION  **
C               **  OF THE FILL COLOR                       **
C               **  INTO A NUMERIC REPRESENTATION           **
C               **  WHICH CAN BE UNDERSTOOD BY THE          **
C               **  GRAPHICS DEVICE.                        **
C               **********************************************
C
      ICOL=ICOLB
C
C     2020/10: SUPPORT FOR RGB COLOR.
C
C               CALL RGB ROUTINE FIRST.  IF RGB NOT SUPPORTED FOR THE
C               CURRENT DEVICE OR RGB COLORS ARE NOT SPECIFIED, THEN
C               FALL BACK TO NON-RGB COLOR MODEL.
C
      CALL GRTRC2(ICOLFR,ICOLFG,ICOLFB,ARED,AGREEN,ABLUE,
     1            AALPHA,IRGBFL)
      IF(IRGBFL.EQ.1)THEN
        ICASE2='REGI'
        CALL GRSEC2(ICASE2,IRGBFL,ARED,AGREEN,ABLUE,AALPHA)
      ELSE
        CALL GRTRCO(ICASE,ICOL,JCOL)
        JCOLB=JCOL
C
C               *******************************
C               **  STEP 2--                 **
C               **  SET THE FILL   COLOR     **
C               **  ON THE GRAPHICS DEVICE.  **
C               *******************************
C
        CALL GRSECO(ICASE,ICOL,JCOL)
      ENDIF
C
C               **********************************************
C               **  STEP 3--                                **
C               **  TRANSLATE THE CHARACTER REPRESENTATION  **
C               **  OF THE FILL PATTERN                     **
C               **  INTO A NUMERIC REPRESENTATION           **
C               **  WHICH CAN BE UNDERSTOOD BY THE          **
C               **  GRAPHICS DEVICE.                        **
C               **********************************************
C
      CALL GRTRPA(ICASE,IPATT,PXGAP,PYGAP,
     1JPATT,IHORPA,IVERPA,IDUPPA,IDDOPA,PXGAP2,PYGAP2)
C
C               *******************************
C               **  STEP 4--                 **
C               **  SET THE FILL PATTERN     **
C               **  ON THE GRAPHICS DEVICE.  **
C               *******************************
C
      CALL GRSEPA(ICASE,IPATT,PXGAP,PYGAP,
     1JPATT,IHORPA,IVERPA,IDUPPA,IDDOPA,PXGAP2,PYGAP2)
C
C               **********************************************
C               **  STEP 5--                                **
C               **  TRANSLATE THE CHARACTER REPRESENTATION  **
C               **  OF THE PATTERN COLOR                    **
C               **  INTO A NUMERIC REPRESENTATION           **
C               **  WHICH CAN BE UNDERSTOOD BY THE          **
C               **  GRAPHICS DEVICE.                        **
C               **********************************************
C
      ICOL=ICOLP
      CALL GRTRC2(ICOLFR,ICOLFG,ICOLFB,ARED,AGREEN,ABLUE,
     1            AALPHA,IRGBFL)
      IF(IRGBFL.EQ.1)THEN
        ICASE2='REGI'
        CALL GRSEC2(ICASE2,IRGBFL,ARED,AGREEN,ABLUE,AALPHA)
      ELSE
        CALL GRTRCO(ICASE,ICOL,JCOL)
        JCOLP=JCOL
C
C               *******************************
C               **  STEP 6--                 **
C               **  SET THE PATTERN COLOR    **
C               **  ON THE GRAPHICS DEVICE.  **
C               *******************************
C
        CALL GRSECO(ICASE,ICOL,JCOL)
      ENDIF
C
C               **********************************************
C               **  STEP 7--                                **
C               **  TRANSLATE THE  DESIRED                  **
C               **  LINE THICKNESS (OF THE PATTERN)         **
C               **  INTO A NUMERIC REPRESENTATION           **
C               **  WHICH CAN BE UNDERSTOOD BY THE          **
C               **  GRAPHICS DEVICE.                        **
C               **********************************************
C
      CALL GRTRTH(ICASE,PTHICK,JTHICK,PTHIC2)
C
C               *******************************
C               **  STEP 8--                 **
C               **  SET THE LINE THICKNESS   **
C               **  (OF THE PATTERN)         **
C               **  ON THE GRAPHICS DEVICE.  **
C               *******************************
C
      CALL GRSETH(ICASE,PTHICK,JTHICK,PTHIC2)
C
C               ***********************************
C               **  STEP 11--                    **
C               **  FILL  THE REGION             **
C               **  BELOW THE BOTTOM FRAME LINE  **
C               ***********************************
C
      PX(1)=0.0
      PY(1)=0.0
      PX(2)=100.0
      PY(2)=0.0
      PX(3)=100.0
      PY(3)=PYMIN
      PX(4)=0.0
      PY(4)=PYMIN
      NP=4
      CALL GRFIRE(PX,PY,NP,IFIG,
     1               IPATT,JPATT,IHORPA,IVERPA,IDUPPA,IDDOPA,
     1               PXGAP2,PYGAP2,PTHICK,JTHICK,PTHIC2,
     1               ICOLB,JCOLB,ICOLP,JCOLP,
     1               ICOLFR,ICOLFG,ICOLFB,ICOLFR,ICOLFG,ICOLFB,
     1               IRGBFL,IPATT2)
C
C               ********************************************
C               **  STEP 12--                             **
C               **  FILL  THE REGION                      **
C               **  TO THE RIGHT OF THE RIGHT FRAME LINE  **
C               ********************************************
C
      PX(1)=PXMAX
      PY(1)=PYMIN
      PX(2)=100.0
      PY(2)=PYMIN
      PX(3)=100.0
      PY(3)=100.0
      PX(4)=PXMAX
      PY(4)=100.0
      NP=4
      CALL GRFIRE(PX,PY,NP,IFIG,
     1            IPATT,JPATT,IHORPA,IVERPA,IDUPPA,IDDOPA,
     1            PXGAP2,PYGAP2,PTHICK,JTHICK,PTHIC2,
     1            ICOLB,JCOLB,ICOLP,JCOLP,
     1            ICOLFR,ICOLFG,ICOLFB,ICOLFR,ICOLFG,ICOLFB,
     1            IRGBFL,IPATT2)
C
C               ********************************
C               **  STEP 13--                 **
C               **  FILL  THE REGION          **
C               **  ABOVE THE TOP FRAME LINE  **
C               ********************************
C
      PX(1)=0.0
      PY(1)=PYMAX
      PX(2)=PXMAX
      PY(2)=PYMAX
      PX(3)=PXMAX
      PY(3)=100.0
      PX(4)=0.0
      PY(4)=100.0
      NP=4
      CALL GRFIRE(PX,PY,NP,IFIG,
     1            IPATT,JPATT,IHORPA,IVERPA,IDUPPA,IDDOPA,
     1            PXGAP2,PYGAP2,PTHICK,JTHICK,PTHIC2,
     1            ICOLB,JCOLB,ICOLP,JCOLP,
     1            ICOLFR,ICOLFG,ICOLFB,ICOLFR,ICOLFG,ICOLFB,
     1            IRGBFL,IPATT2)
C
C               ******************************************
C               **  STEP 14--                           **
C               **  FILL  THE REGION                    **
C               **  TO THE LEFT OF THE LEFT FRAME LINE  **
C               ******************************************
C
      PX(1)=0.0
      PY(1)=PYMIN
      PX(2)=PXMIN
      PY(2)=PYMIN
      PX(3)=PXMIN
      PY(3)=PYMAX
      PX(4)=0.0
      PY(4)=PYMAX
      NP=4
      CALL GRFIRE(PX,PY,NP,IFIG,
     1            IPATT,JPATT,IHORPA,IVERPA,IDUPPA,IDDOPA,
     1            PXGAP2,PYGAP2,
     1            PTHICK,JTHICK,PTHIC2,
     1            ICOLB,JCOLB,ICOLP,JCOLP,
     1            ICOLFR,ICOLFG,ICOLFB,ICOLFR,ICOLFG,ICOLFB,
     1            IRGBFL,IPATT2)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG4.EQ.'ON' .OR. ISUBG4.EQ.'FIMA')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFIMA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)PXMIN,PYMIN,PXMAX,PYMAX
 9012   FORMAT('PXMIN,PYMIN,PXMAX,PYMAX = ',4F10.5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9019)IERRG4
 9019   FORMAT('IERRG4 = ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFIPW(IHARG,IARGT,ARG,NUMARG,DEFFPW,
     1FITPOW,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE POWER IN THE FIT CRITERION
C              IN THE FIT COMMAND (AND THE PRE-FIT COMMAND).
C              THE SPECIFIED FIT POWER VALUE WILL BE PLACED
C              IN THE FLOATING POINT VARIABLE FITPOW.
C     NOTE--POWER = 2 YIELDS THE LEAST SQUARES CRITERION.
C         --POWER = 1 YIELDS THE L1 CRITERION.
C         --POWER = INFINITY YIELDS THE MINIMAX CRITERION.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --ARG    (A  FLOATING POINT VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C                     --DEFFPW (A  FLOATING POINT VARIABLE)
C     OUTPUT ARGUMENTS--FITPOW (A  FLOATING POINT VARIABLE)
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
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(NUMARG.EQ.0)GOTO1199
      IF(NUMARG.GE.2.AND.IHARG(2).EQ.'=')GOTO1199
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'POWE')GOTO1110
      GOTO1199
C
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'POWE')GOTO1150
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
 1121 FORMAT('***** ERROR IN DPFIPW--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR FIT POWER ',
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
 1126 FORMAT('      A FIT , ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1127)
 1127 FORMAT('      AND SUPPOSE THE ANALYST WISHES TO USE  ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1128)
 1128 FORMAT('      POWER OF 1.5 IN THE FIT CRITERION; ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1130)
 1130 FORMAT('      THEN THE ALLOWABLE FORM IS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1131)
 1131 FORMAT('      FIT POWER 1.5 ')
      CALL DPWRST('XXX','BUG ')
      GOTO1199
C
 1150 CONTINUE
      HOLD=DEFFPW
      GOTO1180
C
 1160 CONTINUE
      HOLD=ARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      FITPOW=HOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)FITPOW
 1181 FORMAT('THE FIT POWER HAS JUST BEEN SET TO ',
     1E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPFIRE(PX,PY,NP,
     1                  IFIG,IPATT,PTHICK,PXGAP,PYGAP,
     1                  ICOLF,ICOLFR,ICOLFG,ICOLFB,
     1                  ICOLP,ICOLPR,ICOLPG,ICOLPB,
     1                  IPATT2)
C
C  ABOVE LINE ADDED SEPTEMBER, 1987
C  CONTAINS THE PATTERN FOR THE LINE (I.E., SOLID DASH, ETC.)
C
C
C     PURPOSE--FOR A GENERAL GRAPHICS DEVICE,
C              FILL THE REGION
C              DEFINED BY THE VERTICES AS GIVEN
C              IN THE PX(.) AND PY(.) VECTORS.
C              THIS REGION HAS SPECIFIED FILL PATTERN,
C              BACKGROUND COLOR, PATTERN LINE THICKNESS,
C              PATTERN LINE GAPCING, AND PATTERN COLOR.
C
C     NOTE--THE COORDINATES IN (PX(.),PY(.)) ARE IN
C           STANDARDIZED (0.0 TO 100.0) UNITS.
C     NOTE--THERE ARE NP SUCH COORDINATE PAIRS.
C           (BUT NP SHOULD ALWAYS = 2 FOR THIS SUBROUTINE).
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
C     VERSION NUMBER--83.6
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MAY       1983.
C     UPDATED         --JANUARY   1989. ADDED PARAMETER TO CALL LIST (ALAN)
C     UPDATED         --JANUARY   1989. MODIFY CALL  TO GRFIRE (ALAN)
C     UPDATED         --JANUARY   1989. BUGS FOR BAR PLOT COMMAND (ALAN)
C     UPDATED         --MARCH     1990. MOVE CALL TO SEPA BEFORE COLOR
C                                       ROUTINES.  EITHER SET PATTERN
C                                       OR FILL COLOR, BUT NOT BOTH (PATTERN
C                                       COLOR WAS OVER-RIDING FILL COLOR)
C     UPDATED         --OCTOBER   2020. SUPPORT FOR RGB COLOR
C
C-----NON-COMMON VARIABLES (GRAPHICS)----------------------------------
C
      CHARACTER*4 IFIG
      CHARACTER*4 IPATT
      CHARACTER*4 ICOL
      CHARACTER*4 ICOLF
      CHARACTER*4 ICOLP
      CHARACTER*4 ICASE
      CHARACTER*4 IHORPA
      CHARACTER*4 IVERPA
      CHARACTER*4 IDUPPA
      CHARACTER*4 IDDOPA
      CHARACTER*4 IPATT2
C
      DIMENSION PX(*)
      DIMENSION PY(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'FIRE')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPFIRE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)NP,IFIG,IPATT,ICOLF,ICOLP
   53   FORMAT('NP,IFIG,IPATT,ICOLF,ICOLP = ',I8,4(2X,A4))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)ICOLFR,ICOLFG,ICOLFB,ICOLPR,ICOLPG,ICOLPB
   54   FORMAT('ICOLFR,ICOLFG,ICOLFB,ICOLPR,ICOLPG,ICOLPB = ',6I5)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,NP
          WRITE(ICOUT,56)PX(I),PY(I)
   56     FORMAT('PX(I),PY(I) = ',2G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
        WRITE(ICOUT,64)PTHICK,PXGAP,PYGAP
   64   FORMAT('PTHICK,PXGAP,PYGAP = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,69)IBUGG4,ISUBG4,IERRG4
   69   FORMAT('IBUGG4,ISUBG4,IERRG4 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      ICASE='REGI'
C  FOLLOWING BLOCK MOVED MARCH, 1990.  PATTERN COLOR WAS
C  OVERRIDING FILL COLOR.  DETERMINE WHICH ONE TO CALL
C  (EITHER PATTERN OR FILL, BUT NOT BOTH)
C
C               **********************************************
C               **  STEP X--                                **
C               **  TRANSLATE THE CHARACTER REPRESENTATION  **
C               **  OF THE FILL PATTERN                     **
C               **  INTO A NUMERIC REPRESENTATION           **
C               **  WHICH CAN BE UNDERSTOOD BY THE          **
C               **  GRAPHICS DEVICE.                        **
C               **********************************************
C
      CALL GRTRPA(ICASE,IPATT,PXGAP,PYGAP,
     1JPATT,IHORPA,IVERPA,IDUPPA,IDDOPA,PXGAP2,PYGAP2)
C
C               *******************************
C               **  STEP X--                 **
C               **  SET THE FILL PATTERN     **
C               **  ON THE GRAPHICS DEVICE.  **
C               *******************************
C
      CALL GRSEPA(ICASE,IPATT,PXGAP,PYGAP,
     1JPATT,IHORPA,IVERPA,IDUPPA,IDDOPA,PXGAP2,PYGAP2)
C
      IF(IPATT.EQ.'SOLI' .OR. IPATT.EQ.'FILL')THEN
C
C       2020/10: SUPPORT FOR RGB COLOR.
C
C                 CALL RGB ROUTINE FIRST.  IF RGB NOT SUPPORTED FOR THE
C                 CURRENT DEVICE OR RGB COLORS ARE NOT SPECIFIED, THEN
C                 FALL BACK TO NON-RGB COLOR MODEL.
C
        CALL GRTRC2(ICOLFR,ICOLFG,ICOLFB,ARED,AGREEN,ABLUE,
     1              AALPHA,IRGBFL)
        IF(IRGBFL.EQ.1)THEN
          CALL GRSEC2(ICASE,IRGBFL,ARED,AGREEN,ABLUE,AALPHA)
        ELSE
C               **********************************************
C               **  STEP 1--                                **
C               **  TRANSLATE THE CHARACTER REPRESENTATION  **
C               **  OF THE FILL COLOR                       **
C               **  INTO A NUMERIC REPRESENTATION           **
C               **  WHICH CAN BE UNDERSTOOD BY THE          **
C               **  GRAPHICS DEVICE.                        **
C               **********************************************
C
          ICOL=ICOLF
          CALL GRTRCO(ICASE,ICOL,JCOL)
          JCOLF=JCOL
          JCOLP=JCOL
C
C               *******************************
C               **  STEP 2--                 **
C               **  SET THE FILL   COLOR     **
C               **  ON THE GRAPHICS DEVICE.  **
C               *******************************
C
          CALL GRSECO(ICASE,ICOL,JCOL)
        ENDIF
      ELSE
        CALL GRTRC2(ICOLFR,ICOLFG,ICOLFB,ARED,AGREEN,ABLUE,
     1              AALPHA,IRGBFL)
        IF(IRGBFL.EQ.1)THEN
          CALL GRSEC2(ICASE,IRGBFL,ARED,AGREEN,ABLUE,AALPHA)
        ELSE
          ICOL=ICOLP
          CALL GRTRCO(ICASE,ICOL,JCOL)
          JCOLP=JCOL
C
C               *******************************
C               **  STEP 6--                 **
C               **  SET THE PATTERN COLOR    **
C               **  ON THE GRAPHICS DEVICE.  **
C               *******************************
C
          CALL GRSECO(ICASE,ICOL,JCOL)
        ENDIF
      ENDIF
C
C               **********************************************
C               **  STEP 7--                                **
C               **  TRANSLATE THE  DESIRED                  **
C               **  LINE THICKNESS (OF THE PATTERN)         **
C               **  INTO A NUMERIC REPRESENTATION           **
C               **  WHICH CAN BE UNDERSTOOD BY THE          **
C               **  GRAPHICS DEVICE.                        **
C               **********************************************
C
      CALL GRTRTH(ICASE,PTHICK,JTHICK,PTHIC2)
C
C               *******************************
C               **  STEP 8--                 **
C               **  SET THE LINE THICKNESS   **
C               **  (OF THE PATTERN)         **
C               **  ON THE GRAPHICS DEVICE.  **
C               *******************************
C
      CALL GRSETH(ICASE,PTHICK,JTHICK,PTHIC2)
C
C               *********************
C               **  STEP 11--      **
C               **  FILL  THE BOX  **
C               *********************
C
      CALL GRFIRE(PX,PY,NP,IFIG,
     1            IPATT,JPATT,IHORPA,IVERPA,
     1            IDUPPA,IDDOPA,PXGAP2,PYGAP2,
     1            PTHICK,JTHICK,PTHIC2,
     1            ICOLF,JCOLF,ICOLP,JCOLP,
     1            ICOLFR,ICOLFG,ICOLFB,ICOLPR,ICOLPG,ICOLPB,
     1            IRGBFL,IPATT2)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'FIRE')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFIRE--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFIRT(MAXNXT,ICAPSW,IFORSW,
     1                  IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--CARRY OUT A 2-SAMPLE FISHER RANDOMIZATION TEST
C     EXAMPLE--FISHER TWO SAMPLE RANDOMIZATION TEST Y1 Y2
C              FISHER TWO SAMPLE RANDOMIZATION TEST Y1 Y2 Y3 Y4
C              FISHER TWO SAMPLE RANDOMIZATION TEST Y1 TO Y10
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2011/6
C     ORIGINAL VERSION--JUNE      2011.
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
      CHARACTER*4 ICASAN
      CHARACTER*4 ICTMP1
      CHARACTER*4 ICTMP2
      CHARACTER*4 ICTMP3
      CHARACTER*4 ICTMP4
      CHARACTER*4 ICTMP5
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
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
C
      CHARACTER*4 IFLAGU
      LOGICAL IFRST
      LOGICAL ILAST
C
C---------------------------------------------------------------------
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZI.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOST.INC'
C
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION ITEMP1(MAXOBV)
      EQUIVALENCE(GARBAG(IGARB1),TEMP1(1))
      EQUIVALENCE(GARBAG(IGARB3),TEMP2(1))
      EQUIVALENCE(IGARBG(IIGAR1),ITEMP1(1))
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPFI'
      ISUBN2='RT  '
      IFOUND='NO'
      IERROR='NO'
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
C               ************************************************
C               **  TREAT THE FISHER TWO SAMPLE RANDOMIZATION **
C               **  TEST CASE                                 **
C               ************************************************
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'FIRT')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPFIRT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA2,IBUGA3,IBUGQ,ISUBRO,MAXNXT
   52   FORMAT('IBUGA2,IBUGA3,IBUGQ,ISUBRO = ',4(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *********************************************************
C               **  STEP 1--                                           **
C               **  EXTRACT THE COMMAND                                **
C               *********************************************************
C
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FIRT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ILASTZ=9999
      ICASAN='2FRT'
C
C     LOOK FOR:
C
C          FISHER TWO SAMPLE RANDOMIZATION TEST
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
        ICTMP5=IHARG(I+4)
C
        IF(ICTMP1.EQ.'=')THEN
          IFOUND='NO'
          GOTO9000
        ELSEIF(ICTMP1.EQ.'FISH' .AND. ICTMP2.EQ.'TWO ' .AND.
     1         ICTMP3.EQ.'SAMP' .AND. ICTMP4.EQ.'RAND' .AND.
     1         ICTMP5.EQ.'TEST')THEN
          IFOUND='YES'
          ILASTZ=I+4
        ELSEIF(ICTMP1.EQ.'FISH' .AND. ICTMP2.EQ.'TWO' .AND.
     1         ICTMP3.EQ.'SAMP' .AND. ICTMP4.EQ.'RAND ')THEN
          IFOUND='YES'
          ILASTZ=I+3
        ELSEIF(ICTMP1.EQ.'FISH' .AND. ICTMP2.EQ.'TWO' .AND.
     1         ICTMP3.EQ.'SAMP' .AND. ICTMP4.EQ.'TEST ')THEN
          IFOUND='YES'
          ILASTZ=I+3
        ENDIF
  100 CONTINUE
C
      IF(IFOUND.EQ.'NO')GOTO9000
C
      ISHIFT=ILASTZ
      CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1            IBUGA2,IERROR)
C
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FIRT')THEN
        WRITE(ICOUT,91)ICASAN,ISHIFT
   91   FORMAT('DPFIRT: ICASAN,ISHIFT = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ****************************************
C               **  STEP 2--                          **
C               **  EXTRACT THE VARIABLE LIST         **
C               ****************************************
C
      ISTEPN='2'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FIRT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='FISHER TWO SAMPLE RANDOMIZATION TEST'
      MINNA=1
      MAXNA=100
      MINN2=2
      IFLAGE=0
      IFLAGM=1
      MINNVA=2
      MAXNVA=MAXSPN
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FIRT')THEN
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
C               ******************************************************
C               **  STEP 3A--                                       **
C               **  CASE 1: TWO RESPONSE VARIABLES, NO REPLICATION  **
C               **          HANDLE MULTIPLE RESPONSE VARIABLES      **
C               **          DIFFERENTLY FOR ONE SAMPLE AND TWO      **
C               **          SAMPLE TESTS.                           **
C               ******************************************************
C
      ISTEPN='3A'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FIRT')
     1  CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMVA2=1
      DO5210I=1,NUMVAR
        ICOL=I
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              Y,Y,Y,NS1,NLOCA2,NLOCA3,ICASE,
     1              IBUGA3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
        ISTRT2=I+1
        ISTOP2=NUMVAR
C
        DO5220J=ISTRT2,ISTOP2
C
          ICOL=J
          CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1                INAME,IVARN1,IVARN2,IVARTY,
     1                ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1                MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1                MAXCP4,MAXCP5,MAXCP6,
     1                V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1                X,X,X,NS2,NLOCA2,NLOCA3,ICASE,
     1                IBUGA3,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
C               *****************************************************
C               **  STEP 52--                                      **
C               **  PERFORM A FISHER TWO SAMPLE RANDOMIZATION TEST **
C               *****************************************************
C
          ISTEPN='52'
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'FIRT')THEN
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5211)
 5211       FORMAT('***** FROM DPFIRT, BEFORE CALL DPMNN2--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5212)I,J,NS1,NS2,MAXN
 5212       FORMAT('I,J,NS1,NS2,MAXN = ',5I8)
            CALL DPWRST('XXX','BUG ')
            DO5215II=1,MAX(NS1,NS2)
              WRITE(ICOUT,5216)II,Y(II),X(II)
 5216         FORMAT('I,Y(I),X(I) = ',I8,2G15.7)
              CALL DPWRST('XXX','BUG ')
 5215       CONTINUE
          ENDIF
C
          IVARID=IVARN1(I)
          IVARI2=IVARN2(I)
          IVARI3=IVARN1(J)
          IVARI4=IVARN2(J)
          CALL DPFIR2(Y,NS1,X,NS2,ICASAN,
     1               TEMP1,TEMP2,ITEMP1,MAXNXT,
     1               ICAPSW,ICAPTY,IFORSW,
     1               IVARID,IVARI2,IVARI3,IVARI4,
     1               STATVA,PVAL2T,PVALLT,
     1               IBUGA3,ISUBRO,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
C               ***************************************
C               **  STEP 8C--                        **
C               **  UPDATE INTERNAL DATAPLOT TABLES  **
C               ***************************************
C
          ISTEPN='8C'
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FIRT')
     1      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
          IF(NUMVAR.GT.2)THEN
            IFLAGU='FILE'
          ELSE
            IFLAGU='ON'
          ENDIF
          IFRST=.FALSE.
          ILAST=.FALSE.
          IF(I.EQ.1 .AND. J.EQ.2)IFRST=.TRUE.
          IF(I.EQ.NUMVAR .AND. J.EQ.NUMVAR)ILAST=.TRUE.
          CALL DPFIR5(STATVA,PVAL2T,PVALLT,
     1                IFLAGU,IFRST,ILAST,
     1                IBUGA2,IBUGA3,ISUBRO,IERROR)
C
 5220   CONTINUE
 5210 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'FIRT')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFIRT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)IFOUND,IERROR
 9016   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFIR2(Y1,N1,Y2,N2,ICASAN,
     1                  TEMP1,TEMP2,ITEMP1,MAXNXT,
     1                  ICAPSW,ICAPTY,IFORSW,
     1                  IVARID,IVARI2,IVARI3,IVARI4,
     1                  STATVA,PVAL2T,PVALLT,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--THIS ROUTINE CARRIES OUT A 2-SAMPLE FISHER RANDOMIZATION
C              TEST.
C              SAMPLE 1 IS IN INPUT VECTOR Y1 (WITH N1 OBSERVATIONS).
C              SAMPLE 2 IS IN INPUT VECTOR Y2 (WITH N2 OBSERVATIONS).
C     EXAMPLE--FISHER TWO SAMPLE RANDOMIZATION TEST Y1 Y2
C     REFERENCE--RICHARDS (1996), "FISHER'S RANDOMIZATION TEST FOR
C                TWO SMALL INDEPENDENT SAMPLES", APPLIED STATISTICS,
C                VOL. 45, NO. 3, PP. 394-398.
C                THIS ROUTINE CALLS RICHARD'S ALGORITHM (FISHER)
C                TO IMPLEMENT THIS TEST.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2011/6
C     ORIGINAL VERSION--JUNE      2011.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IVARID
      CHARACTER*4 IVARI2
      CHARACTER*4 IVARI3
      CHARACTER*4 IVARI4
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 ICASAN
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION ITEMP1(*)
C
      REAL MEANX
      REAL MEANY
C
      PARAMETER (MAXROW=25)
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
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPFI'
      ISUBN2='R2  '
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FIR2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPFIR2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,ICASAN
   52   FORMAT('IBUGA3,ISUBRO,ICASAN = ',2(A4,2X),A4)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,53)IVARID,IVARI2,IVARI3,IVARI4
   53   FORMAT('IVARID,IVARI2,IVARI3,IVARI4 = ',3(A4,2X),A4)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,55)N1,N2,NUMDIG
   55   FORMAT('N1,N2,NUMDIG = ',3I8)
        CALL DPWRST('XXX','WRIT')
        IF(N1.GE.1)THEN
          DO56I=1,MAX(N1,N2)
            WRITE(ICOUT,57)I,Y1(I),Y2(I)
   57       FORMAT('I,Y1(I),Y2(I) = ',I8,2G15.7)
            CALL DPWRST('XXX','WRIT')
   56     CONTINUE
        ENDIF
      ENDIF
C
C               ************************************
C               **   STEP 1--                     **
C               **   CALL FISHER TO COMPUTE THE   **
C               **   BASIC TEST STATISTIC         **
C               ************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIR2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(MAXNXT.GE.1000000)THEN
        MAXSAM=22
      ELSE
        MAXSAM=20
      ENDIF
      SUMX=CPUMIN
      SUMY=CPUMIN
      MEANX=CPUMIN
      MEANY=CPUMIN
      PTEMP=CPUMIN
      CALL FISHER(Y1,N1,Y2,N2,ITOTAL,POSSIB,PVAL,
     1            SUMX,SUMY,MEANX,MEANY,
     1            TEMP1,TEMP2,ITEMP1,MAXSAM,MAXNXT,
     1            IFAULT,IBUGA3)
C
      IF(IFAULT.EQ.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
  101   FORMAT('****** ERROR IN FISHER TWO-SAMPLE RANDOMIZATION TEST--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,103)
  103   FORMAT('       MAXIMUM STORAGE SPACE EXCEEDED.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,105)N1
  105   FORMAT('       NUMBER OF OBSERVATIONS FOR SAMPLE ONE  = ',I8)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,107)N2
  107   FORMAT('       NUMBER OF OBSERVATIONS FOR SAMPLE TWO  = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ELSEIF(IFAULT.EQ.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,113)MAXSAM
  113   FORMAT('       SAMPLE SIZE > ',I8)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,105)N1
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,107)N2
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      STATVA=SUMX
      PVALLT=PVAL
      PVAL2T=2.0*PVALLT
C
C     P-VALUE RETURNED IS FOR THE LOWER-TAILED TEST.  FOR
C     EQUAL SAMPLE SIZES, THE EXACT P-VALUE FOR THE TWO-TAILED
C     TEST CAN BE OBTAINED SIMPLY MULTIPLYING ONE-TAILED TEST
C     BY 2.  HOWEVER, FOR UNEQUAL SAMPLE SIZES, THIS IS ONLY
C     APPROXIMATE.  FOR THIS CASE, THE UPPER TAIL VALUES CAN
C     BE OBTAINED FROM THE FOLLOWING PROCEDURE:
C
C        1) LET
C
C           X      = SAMPLE WITH SMALLER MEAN
C           M      = SAMPLE SIZE FOR X
C           Y      = SAMPLE WITH LARGER MEAN
C           N      = SAMPLE SIZE FOR Y
C           D      = MEAN OF X  -  MEAN OF Y
C           T      = TOTAL OF ALL SAMPLE OBSERVATIONS (SUM OF X + SUM OF Y)
C
C        2) FIND THE MINIMUM SUM FOR M SAMPLE OBSERVATIONS THAT SATISFIES
C
C            SUM OF X >= M*(T- N*D)/(M + N)
C
C     NOTE THAT THE ORIGINAL CALL TO FISHER WILL AUTOMATICALLY
C     EXCHANGE Y1 AND Y2 IF Y2 (AND N1 AND N2) IF Y2 HAS THE SMALLER
C     MEAN.
C
C     PUT Y1 AND Y2 IN A COMMON VARIABLE AND SORT THIS VARIABLE (AND
C     CARRY ALONG A VARIABLE THAT IDENTIFIES WHICH SAMPLE THE
C     OBSERVATION BELONGS TO).
C
C     FOR NOW, REPORT THE APPROXIMATE P-VALUE FOR THE TWO-TAILED CASE.
C     THE ABOVE ALGORITHM CAN GET A BIT COMPLICATED TO AUTOMATE SINCE
C     WE MAY NEED TO TEST MANY DIFFERENT SUBSETS.  IF MINIMUM SUM WITH
C     EXACTLY M OBSERVATIONS AND GREATER THAN OR EQUAL TO THRESHOLD
C     REACHED AT SMALLEST M OR M + 1 SAMPLES, THIS CAN BE DONE IN A
C     RELATIVELY STRAIGHTFORWARD WAY.  IF M + 2 OR MORE SAMPLES REQUIRED,
C     THEN THIS GETS A BIT MORE COMPLICATED.
C
CCCCC M=N1
CCCCC N=N2
CCCCC T=SUMX + SUMY
CCCCC D=ABS(MEANX - MEANY)
CCCCC ASUM=REAL(M)*(T - REAL(N)*D)/REAL(M+N)
CCCCC DO210I=1,N1
CCCCC   TEMP1(I)=Y1(I)
CCCCC   TEMP2(I)=1.0
CC210 CONTINUE
CCCCC ICNT=N1
CCCCC DO220I=1,N2
CCCCC   ICNT=ICNT+1
CCCCC   TEMP1(ICNT)=Y2(I)
CCCCC   TEMP2(ICNT)=2.0
CC220 CONTINUE
CCCCC NCOMB=N1+N2
CCCCC CALL SORTC(TEMP1,TEMP2,NCOMB,TEMP1,TEMP3)
C
C     FIRST, FIND WHICH POINT IN THE ARRAY HAS SUFFICIENTLY
C     LARGE SUM WITH EXACTLY M VALUES
C
CCCCC DO300ICNT=M,NCOMB
CCCCC   ISTRT=ICNT-M+1
CCCCC   CALL SUMDP(TEMP1(ISTRT),ICNT,IWRITE,SUMT,IBUGA3,IERROR)
CCCCC   IF(SUMT.GE.ASUM)THEN
CCCCC     print *,'m,icnt,asum,sumt=',m,icnt,asum,sumt
CCCCC     ISTOP=ICNT
CCCCC     GOTO309
CCCCC   ENDIF
C
C               *************************************************
C               **   STEP 22--                                 **
C               **   WRITE OUT EVERYTHING FOR A                **
C               **   FISHER TWO SAMPLE RANDOMIZATION TEST      **
C               *************************************************
C
      ISTEPN='22'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIR2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPRINT.EQ.'OFF')GOTO9000
C
      IF(ICASAN.EQ.'LOWE')THEN
        ITITLE='Two Sample Lower-Tailed Fisher Randomization Test'
        NCTITL=49
      ELSE
        ITITLE='Two Sample Two-Sided Fisher Randomization Test'
        NCTITL=46
      ENDIF
      ITITLZ='(Independent Samples)'
      NCTITZ=21
C
      ICNT=1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='First Response Variable: '
      WRITE(ITEXT(ICNT)(26:29),'(A4)')IVARID(1:4)
      WRITE(ITEXT(ICNT)(30:33),'(A4)')IVARI2(1:4)
      NCTEXT(ICNT)=33
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Second Response Variable: '
      WRITE(ITEXT(ICNT)(27:30),'(A4)')IVARI3(1:4)
      WRITE(ITEXT(ICNT)(31:34),'(A4)')IVARI4(1:4)
      NCTEXT(ICNT)=34
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
      ITEXT(ICNT)='H0: E(X) = E(Y)'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      IF(ICASAN.EQ.'LOWE')THEN
        ITEXT(ICNT)='Ha: E(X) < E(Y)'
        NCTEXT(ICNT)=15
      ELSE
        ITEXT(ICNT)='Ha: E(X) <> E(Y)'
        NCTEXT(ICNT)=16
      ENDIF
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
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
      ITEXT(ICNT)='Sample with Smaller Mean:'
      NCTEXT(ICNT)=25
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Observations:'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=REAL(N1)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Mean:'
      NCTEXT(ICNT)=5
      AVALUE(ICNT)=MEANX
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sum of Observations:'
      NCTEXT(ICNT)=20
      AVALUE(ICNT)=SUMX
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample with Larger Mean:'
      NCTEXT(ICNT)=24
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Observations:'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=REAL(N2)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Mean:'
      NCTEXT(ICNT)=5
      AVALUE(ICNT)=MEANY
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sum of Observations:'
      NCTEXT(ICNT)=20
      AVALUE(ICNT)=SUMY
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Difference of Means:'
      NCTEXT(ICNT)=20
      AVALUE(ICNT)=MEANX - MEANY
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='Test Statistic:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=STATVA
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Approximate P-Value (two-tailed test):'
      NCTEXT(ICNT)=38
      AVALUE(ICNT)=PVAL2T
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Exact P-Value (lower-tailed test):'
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=PVALLT
      IDIGIT(ICNT)=NUMDIG
C
      NUMROW=ICNT
      DO2110I=1,NUMROW
        NTOT(I)=15
 2110 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
C
      ISTEPN='21A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIR2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1            AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FIR2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFIR2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9013)STATVA,PVAL2T,PVALLT
 9013   FORMAT('STATVA,PVAL2T,PVALLT = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFIR5(STATVA,PVAL2T,PVALLT,
     1                  IFLAGU,IFRST,ILAST,
     1                  IBUGA2,IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--UTILITY ROUTINE USED BY DPFIRT TO UPDATE VARIOUS
C              INTERNAL PARAMETERS AFTER A FISHER TWO SAMPLE
C              RANDOMIZATION TEST.
C
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORAOTRY
C                 NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2011/6
C     ORIGINAL VERSION--JUNE      2011.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IFLAGU
      CHARACTER*4 IBUGA2
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      LOGICAL IFRST
      LOGICAL ILAST
C
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IOP
C
      SAVE IOUNI1
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'FIR5')THEN
        ISTEPN='1'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPFIR5--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)STATVA,PVAL2T,PVALLT
   53   FORMAT('STATVA,PVAL2T,PVALLT = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IFLAGU.EQ.'FILE')THEN
C
        IF(IFRST)THEN
          IOP='OPEN'
          IFLAG1=1
          IFLAG2=0
          IFLAG3=0
          IFLAG4=0
          IFLAG5=0
          CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1                IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1                IBUGA3,ISUBRO,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          WRITE(IOUNI1,295)
  295     FORMAT(10X,'STATVAL',9X,'PVAL2T',9X,'PVALLT')
        ENDIF
        WRITE(IOUNI1,298)STATVA,PVAL2T,PVALLT
  298   FORMAT(3E15.7)
      ELSEIF(IFLAGU.EQ.'ON')THEN
        IH='STAT'
        IH2='VALU'
        VALUE0=STATVA
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='PVAL'
        IH2='UE  '
        VALUE0=PVAL2T
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='PVAL'
        IH2='UELT'
        VALUE0=PVALLT
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
      ENDIF
C
      IF(IFLAGU.EQ.'FILE')THEN
        IF(ILAST)THEN
          IOP='CLOS'
          IFLAG1=1
          IFLAG2=0
          IFLAG3=0
          IFLAG4=0
          IFLAG5=0
          CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1                IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1                IBUGA3,ISUBRO,IERROR)
C
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'FIR5')THEN
            ISTEPN='3A'
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,301)IERROR,IOUNI1
  301       FORMAT('AFTER CALL DPCLFI, IERROR,IOUNI1 = ',A4,2X,I5)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          IF(IERROR.EQ.'YES')GOTO9000
        ENDIF
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'FIR5')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DPFIR5--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFISD(IHARG,IARGT,ARG,NUMARG,DEFFSD,
     1FITSD,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE LOWER BOUND FOR THE FIT STANDARD DEVIATION.
C              THE RESIDUAL STANDARD DEVIATION AFTER EACH
C              ITERATION OF A FIT WILL BE COMPARED
C              TO THE SPECIFIED FIT STANDARD DEVIATION.
C              THE SPECIFIED FIT STANDARD DEVIATION VALUE WILL BE PLACED
C              IN THE FLOATING POINT VARIABLE FITSD.
C              THE RESIDUAL STANDARD DEVIATION WILL BE
C              COMPARED TO THE FIT STANDARD DEVIATION VALUE.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --ARG    (A  FLOATING POINT VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C                     --DEFFSD (A  FLOATING POINT VARIABLE)
C     OUTPUT ARGUMENTS--FITSD  (A  FLOATING POINT VARIABLE)
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
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(NUMARG.EQ.0)GOTO1199
      IF(NUMARG.GE.3.AND.IHARG(3).EQ.'=')GOTO1199
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'STAN'.AND.
     1IHARG(2).EQ.'DEVI')GOTO1110
      GOTO1199
C
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'DEVI')GOTO1150
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
 1121 FORMAT('***** ERROR IN DPFISD--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR FIT STANDARD DEVIATION ',
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
 1126 FORMAT('      A NON-LINEAR FIT , ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1127)
 1127 FORMAT('      AND SUPPOSE THE ANALYST WISHES TO TERMINATE  ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1128)
 1128 FORMAT('      THE FIT ITERATIONS WHENEVER THE RESIDUAL ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1129)
 1129 FORMAT('      STANDARD DEVIATION REACHES .0001 OR SMALLER; ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1130)
 1130 FORMAT('      THEN THE ALLOWABLE FORM IS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1131)
 1131 FORMAT('      FIT STANDARD DEVIATION .0001 ')
      CALL DPWRST('XXX','BUG ')
      GOTO1199
C
 1150 CONTINUE
      HOLD=DEFFSD
      GOTO1180
C
 1160 CONTINUE
      HOLD=ARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      FITSD=HOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)FITSD
 1181 FORMAT('THE FIT STANDARD DEVIATION HAS JUST BEEN SET TO ',
     1E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPFISH(MAXNXT,ICASAN,ICAPSW,IFORSW,
     1                  IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--COMPUTE THE FISHER EXACT TEST
C     EXAMPLE--FISHER EXACT TEST Y1 Y2
C            --FISHER EXACT TEST N11 N21 N12 N22
C            --FISHER EXACT TEST M
C     REFERENCE--XXX
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2007/3
C     ORIGINAL VERSION--MARCH     2007.
C     UPDATED  VERSION--FEBRUARY  2011. USE DPPARS, DPPAR3, DPPAR6
C     UPDATED  VERSION--JULY      2019. TWEAK SCRATCH STORAGE
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASAN
      CHARACTER*4 ICAPSW
      CHARACTER*4 IFORSW
C
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
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 ISUBN0
      CHARACTER*4 ICASE
C
      CHARACTER*40 INAME
      PARAMETER (MAXSPN=20)
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
      PARAMETER(MAXLEV=200)
      PARAMETER(IWKMX=1000000)
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZD.INC'
      INCLUDE 'DPCOZI.INC'
C
      REAL TEMP1(MAXOBV)
      REAL TEMP2(MAXOBV)
      REAL TEMP3(MAXOBV)
      REAL XIDTEM(MAXOBV)
      REAL XIDTE2(MAXOBV)
      REAL RWORK(10*MAXOBV)
C
      INTEGER IWORK(10*MAXOBV)
C
      REAL XMAT2(MAXLEV,MAXLEV)
      DOUBLE PRECISION XMAT(MAXLEV,MAXLEV)
      DOUBLE PRECISION ROWTOT(MAXOBV)
      DOUBLE PRECISION COLTOT(MAXOBV)
      DOUBLE PRECISION DWORK(8*MAXOBV)
C
      EQUIVALENCE (GARBAG(IGARB1),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB2),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB3),TEMP3(1))
      EQUIVALENCE (GARBAG(IGARB4),XIDTEM(1))
      EQUIVALENCE (GARBAG(IGARB5),XIDTE2(1))
      EQUIVALENCE (GARBAG(IGARB6),XMAT2(1,1))
      EQUIVALENCE (GARBAG(IGARB7),RWORK(1))
      EQUIVALENCE (GARBAG(JGAR20),XMAT(1,1))
C
      EQUIVALENCE (DGARBG(IDGAR1),ROWTOT(1))
      EQUIVALENCE (DGARBG(IDGAR2),COLTOT(1))
      EQUIVALENCE (DGARBG(IDGAR3),DWORK(1))
C
      EQUIVALENCE (IGARBG(IIGAR1),IWORK(1))
C
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPFI'
      ISUBN2='SH  '
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
      N11=(-999)
      N21=(-999)
      N12=(-999)
      N22=(-999)
C
      ICASE='PARA'
C
C               ***************************************************
C               **  TREAT THE FISHER EXACT TEST CASE  **
C               ***************************************************
C
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FISH')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPFISH--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA2,IBUGA3,IBUGQ,ISUBRO,ICASAN
   52   FORMAT('IBUGA2,IBUGA3,IBUGQ,ISUBRO,ICASAN = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)MAXNXT,NUMARG,IFORSW
   55   FORMAT('MAXNXT,NUMARG,IFORSW = ',2I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO59I=1,NUMARG
          WRITE(ICOUT,57)I,IHARG(I),IHARG2(I),ARG(I)
   57     FORMAT('I,IHARG(I),IHARG2(I),ARG(I) = ',I5,A4,A4,G15.7)
   59   CONTINUE
      ENDIF
C
C               *********************************
C               **  STEP 4--                   **
C               **  EXTRACT THE VARIABLE LIST  **
C               *********************************
C
      ISTEPN='4'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FISH')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='FISHER EXACT TEST'
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FISH')THEN
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
     1                      ICOLR(I),PVAR(I)
  287       FORMAT('I,IVARN1(I),IVARN2(I),ILIS(I),NRIGHT(I),',
     1             'ICOLR(I),PVAR(I) = ',I8,2X,A4,A4,2X,3I8,G15.7)
            CALL DPWRST('XXX','BUG ')
  285     CONTINUE
        ENDIF
      ENDIF
C
C               ***********************************
C               **  STEP 22--                    **
C               **  CHECK FOR PROPER VALUES FOR  **
C               **  INPUT PARAMETERS             **
C               ***********************************
C
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
C
        ISTEPN='22'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FISH')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IF(N11.LT.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2201)
 2201     FORMAT('***** ERROR FROM FISHER EXACT TEST--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2203)
 2203     FORMAT('      THE VALUE OF THE FIRST PARAMETER (N11 = THE ',
     1           'NUMBER OF SUCCESSES')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2204)
 2204     FORMAT('      FOR THE FIRST VARIABLE MUST BE NON-NEGATIVE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2205)N11
 2205     FORMAT('      N11 = ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
C
        ELSEIF(N21.LT.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2201)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2303)
 2303     FORMAT('      THE VALUE OF THE SECOND PARAMETER (N21 = THE ',
     1           'NUMBER OF FAILURES')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2304)
 2304     FORMAT('      FOR THE FIRST VARIABLE MUST BE NON-NEGATIVE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2305)N21
 2305     FORMAT('      N21 = ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
C
        ELSEIF(N12.LT.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2201)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2403)
 2403     FORMAT('      THE VALUE OF THE THIRD PARAMETER (N12 = THE ',
     1           'NUMBER OF SUCCESSES')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2404)
 2404     FORMAT('      FOR THE SECOND VARIABLE MUST BE NON-NEGATIVE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2405)N12
 2405     FORMAT('      N12 = ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
C
        ELSEIF(N22.LT.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2201)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2503)
 2503     FORMAT('      THE VALUE OF THE FOURTH PARAMETER (N22 = THE ',
     1           'NUMBER OF FAILURES')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2504)
 2504     FORMAT('      FOR THE SECOND VARIABLE MUST BE NON-NEGATIVE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2505)N22
 2505     FORMAT('      N22 = ',I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
C
      ELSEIF(IVARTY(1).EQ.'VARI')THEN
C
        ICASE='VARI'
        ICOL=1
        IF(NUMVAR.GT.2)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2201)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2603)
 2603     FORMAT('      MORE THAN TWO VARIABLES GIVEN.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2605)NUMVAR
 2605     FORMAT('      THE NUMBER OF VARIABLES GIVEN  = ',I5)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              Y,X,X,NLOCAL,NLOCA2,NLOCA3,ICASE,
     1              IBUGA3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        NS1=NLOCAL
        NS2=NLOCA2
C
      ELSEIF(IVARTY(1).EQ.'MATR')THEN
        ICASE='MATR'
        ICOL=1
        NUMVAR=1
        CALL DPPAR6(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              XMAT2,MAXLEV,NROW,NCOL,ICASE,
     1              IBUGA3,ISUBRO,IFOUND,IERROR)
        DO5090J=1,MAXLEV
          DO5093I=1,MAXLEV
            XMAT(I,J)=DBLE(XMAT2(I,J))
 5093     CONTINUE
 5090   CONTINUE
        ICASE='TABL'
        IF(IERROR.EQ.'YES')GOTO9000
      ENDIF
C
C               *************************************
C               **  STEP 61--                      **
C               **  COMPUTE THE FISHER EXACT TEST  **
C               *************************************
C
      ISTEPN='61'
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'FISH')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6111)
 6111   FORMAT('***** FROM DPFISH--READY TO COMPUTE TEST')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6112)AN11,AN21,AN12,AN22
 6112   FORMAT('AN11,AN21,AN12,AN22 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      CALL DPFIS2(Y,NS1,X,NS2,
     1            AN11,AN21,AN12,AN22,
     1            XMAT,MAXLEV,NROW,NCOL,
     1            XIDTEM,XIDTE2,TEMP1,TEMP2,TEMP3,MAXOBW,
     1            ROWTOT,COLTOT,
     1            ICASE,
     1            ICAPSW,ICAPTY,IFORSW,
     1            STATVA,PVAL,CDF,
     1            RWORK,DWORK,IWORK,IWKMX,
     1            ISUBRO,IBUGA3,IERROR)
C
C               ***************************************
C               **  STEP 62--                        **
C               **  UPDATE INTERNAL DATAPLOT TABLES  **
C               ***************************************
C
      ISTEPN='62'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FISH')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ISUBN0='FISH'
C
      IH='STAT'
      IH2='VAL '
      VALUE0=STATVA
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGA3,IERROR)
C
      IH='STAT'
      IH2='CDF '
      VALUE0=CDF
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGA3,IERROR)
C
      IH='PVAL'
      IH2='UE  '
      VALUE0=PVAL
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGA3,IERROR)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FISH')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFISH--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)IERROR
 9016   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFIS2(Y1,N1,Y2,N2,
     1                  AN11,AN21,AN12,AN22,
     1                  XMAT,MAXLEV,NROW,NCOL,
     1                  XIDTEM,XIDTE2,TEMP1,TEMP2,TEMP3,MAXNXT,
     1                  ROWTOT,COLTOT,
     1                  ICASE,
     1                  ICAPSW,ICAPTY,IFORSW,
     1                  STATVA,PVAL,CDF,
     1                  RWORK,DWORK,IWORK,IWKMX,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--PERFORM A FISHER EXACT TEST FOR INDEPENDENCE.
C              THE INPUT CAN BE ENTERED IN THE FOLLOWING WAYS:
C
C              1) THE COMMON CASE OF A 2X2 TABLE CAN BE
C                 ENTERED AS 4 PARAMETERS:
C
C                    N11 = NUMBER OF SUCCESSES FOR VARIABLE 1
C                    N21 = NUMBER OF FAILURES  FOR VARIABLE 1
C                    N12 = NUMBER OF SUCCESSES FOR VARIABLE 2
C                    N22 = NUMBER OF SUCCESSES FOR VARIABLE 2
C
C              2) AS RAW DATA, THAT IS TWO VARIABLES.  A
C                 CROSS-TABULATION IS PERFORMED TO GENERATE
C                 AN RXC TABLE OF COUNTS.
C
C              3) AS A MATRIX, I.E., THE RXC TABLE HAS ALREADY
C                 BEEN GENERATED.
C
C              THE FISHER EXACT TEST IS COMPUTED USING ACM
C              ALGORITHM 643.
C
C     EXAMPLE--FISHER EXACT TEST Y1 Y2
C            --FISHER EXACT TEST N11 N21 N12 N22
C            --FISHER EXACT TEST M
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGYU LABORATORY
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
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
      CHARACTER*4 ICASE
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
C
      CHARACTER*4 IWRITE
      CHARACTER*6 ICONC1
      CHARACTER*6 ICONC2
      CHARACTER*6 ICONC3
      CHARACTER*6 ICONC4
      CHARACTER*6 ICONC5
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IOP
C
C---------------------------------------------------------------------
C
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
      DIMENSION RWORK(*)
C
      INTEGER IWORK(*)
C
      DOUBLE PRECISION XMAT(MAXLEV,MAXLEV)
      DOUBLE PRECISION ROWTOT(*)
      DOUBLE PRECISION COLTOT(*)
      DOUBLE PRECISION DWORK(*)
C
      PARAMETER (NUMALP=5)
CCCCC DIMENSION SIGVAL(NUMALP)
CCCCC DIMENSION ALOWCL(NUMALP)
CCCCC DIMENSION AUPPCL(NUMALP)
CCCCC DIMENSION ALOWC2(NUMALP)
CCCCC DIMENSION AUPPC2(NUMALP)
C
      DOUBLE PRECISION GTOTAL
      DOUBLE PRECISION EMIN
      DOUBLE PRECISION EXPECT
      DOUBLE PRECISION PERCNT
      DOUBLE PRECISION PRE
      DOUBLE PRECISION PRT
C
      PARAMETER(NUMCLI=4)
      PARAMETER(MAXLIN=3)
      PARAMETER (MAXROW=NUMALP)
      PARAMETER (MAXRO2=20)
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
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
C
CCCCC DATA SIGVAL /0.50, 0.80, 0.90, 0.95, 0.99/
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPFI'
      ISUBN2='S2  '
C
      IERROR='NO'
      IWRITE='NO'
C
      IOP='OPEN'
      IFLAG1=1
      IFLAG2=0
      IFLAG3=0
      IFLAG4=0
      IFLAG5=0
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1            IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1            IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      WRITE(IOUNI1,41)
   41 FORMAT(5X,'ROW  COLUMN',9X,'ROWTOT',9X,'COLTOT',6X,'EXPECTED',
     1      8X,'OBSERVED')
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FIS2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPFIS2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,ICASE,MAXNXT
   52   FORMAT('IBUGA3,ISUBRO,ICASE,MAXNXT = ',3(A4,2X),I8)
        CALL DPWRST('XXX','WRIT')
        IF(ICASE.EQ.'VARI')THEN
          WRITE(ICOUT,55)N1
   55     FORMAT('N1 = ',I8)
          CALL DPWRST('XXX','WRIT')
          DO56I=1,N1
            WRITE(ICOUT,57)I,Y1(I)
   57       FORMAT('I,Y1(I) = ',I8,E15.7)
            CALL DPWRST('XXX','WRIT')
   56     CONTINUE
          WRITE(ICOUT,65)N2
   65     FORMAT('N2 = ',I8)
          CALL DPWRST('XXX','WRIT')
          DO66I=1,N2
            WRITE(ICOUT,67)I,Y2(I)
   67       FORMAT('I,Y2(I) = ',I8,E15.7)
            CALL DPWRST('XXX','WRIT')
   66     CONTINUE
        ELSEIF(ICASE.EQ.'TABL')THEN
          WRITE(ICOUT,81)NROW,NCOL
   81     FORMAT('NROW,NCOL = ',2I8)
          CALL DPWRST('XXX','WRIT')
          DO82I=1,NROW
            WRITE(ICOUT,83)(XMAT(I,J),J=1,MIN(NCOL,5))
   83       FORMAT('I,XMAT(I,J) = ',I8,5G15.7)
            CALL DPWRST('XXX','WRIT')
   82     CONTINUE
        ELSE
          WRITE(ICOUT,75)AN11,AN21,AN12,AN22
   75     FORMAT('AN11,AN21,AN12,AN22 = ',4G15.7)
          CALL DPWRST('XXX','WRIT')
        ENDIF
      ENDIF
 
C               ********************************************
C               **  STEP 0--                              **
C               **  BRANCH TO APPROPRIATE CASE (PARAMETER **
C               **  OR VARIABLE)                          **
C               ********************************************
C
      ISTEPN='00'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIS2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASE.EQ.'PARA')GOTO1000
      IF(ICASE.EQ.'VARI')GOTO2000
      IF(ICASE.EQ.'TABL')GOTO3000
C
C               ********************************************
C               **  STEP 11--                             **
C               **  PARAMETER CASE                        **
C               ********************************************
C
 1000 CONTINUE
C
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIS2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               ********************************************
C               **  STEP 12--                             **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      N11=INT(AN11+0.5)
      N21=INT(AN21+0.5)
      N12=INT(AN12+0.5)
      N22=INT(AN22+0.5)
C
      ISTEPN='12'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIS2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N11.LT.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1201)
 1201   FORMAT('***** ERROR FROM THE FISHER EXACT TEST--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1203)
 1203   FORMAT('      THE VALUE OF THE FIRST PARAMETER (N11 = THE ',
     1         'NUMBER OF SUCCESSES')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1204)
 1204   FORMAT('      FOR THE FIRST VARIABLE MUST BE NON-NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1205)N11
 1205   FORMAT('      N11 = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(N21.LT.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1201)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1303)
 1303   FORMAT('      THE VALUE OF THE SECOND PARAMETER (N21 = THE ',
     1         'NUMBER OF FAILURES')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1304)
 1304   FORMAT('      FOR THE FIRST VARIABLE MUST BE NON-NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1305)N21
 1305   FORMAT('      N21 = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(N12.LT.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1201)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1403)
 1403   FORMAT('      THE VALUE OF THE THIRD PARAMETER (N12 = THE ',
     1         'NUMBER OF SUCCESSES')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1404)
 1404   FORMAT('      FOR THE SECOND VARIABLE MUST BE NON-NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1405)N12
 1405   FORMAT('      N12 = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(N22.LT.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1201)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1503)
 1503   FORMAT('      THE VALUE OF THE FOURTH PARAMETER (N22 = THE ',
     1         'NUMBER OF FAILURES')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1504)
 1504   FORMAT('      FOR THE SECOND VARIABLE MUST BE NON-NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1505)N22
 1505   FORMAT('      N22 = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               ********************************************
C               **  STEP 12--                             **
C               **  COMPUTE THE FISHER EXACT TEST         **
C               ********************************************
C
      XMAT(1,1)=DBLE(AN11)
      XMAT(2,1)=DBLE(AN21)
      XMAT(1,2)=DBLE(AN12)
      XMAT(2,2)=DBLE(AN22)
      ROWTOT(1)=DBLE(AN11 + AN12)
      ROWTOT(2)=DBLE(AN21 + AN22)
      COLTOT(1)=DBLE(AN11 + AN21)
      COLTOT(2)=DBLE(AN12 + AN22)
      GTOTAL=ROWTOT(1) + ROWTOT(2)
      NROW=2
      NCOL=2
C
      IINDX=0
      DO1600J=1,2
        DO1610I=1,2
          IINDX=IINDX+1
          EXP=ROWTOT(I)*COLTOT(J)/GTOTAL
          OBS=XMAT(I,J)
          WRITE(IOUNI1,1605)I,J,ROWTOT(I),COLTOT(J),EXP,OBS
 1605     FORMAT(I8,I8,4E15.7)
C
 1610   CONTINUE
 1600 CONTINUE
      GOTO4000
C
C               ********************************************
C               **  STEP 20--                             **
C               **  VARIABLE  CASE                        **
C               ********************************************
C
 2000 CONTINUE
C
C               ********************************************
C               **  STEP 21--                             **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIS2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N1.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1201)
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
      IF(N2.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1201)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2106)
 2106   FORMAT('      THE NUMBER OF OBSERVATIONS FOR VARIABLE 2 ',
     1         'IS NON-POSITIVE')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2103)N2
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               ******************************************************
C               **  STEP 2.2--                                      **
C               **  DETERMINE THE NUMBER OF DISTINCT VALUES         **
C               **  FOR THE GROUP VARIABLES (Y1, Y2).               **
C               ******************************************************
C
      ISTEPN='22'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIS2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DISTIN(Y1,N1,IWRITE,XIDTEM,NUMSE1,IBUGA3,IERROR)
      CALL SORT(XIDTEM,NUMSE1,XIDTEM)
      CALL DISTIN(Y2,N2,IWRITE,XIDTE2,NUMSE2,IBUGA3,IERROR)
      CALL SORT(XIDTE2,NUMSE2,XIDTE2)
C
      IF(NUMSE1.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2201)
 2201   FORMAT('***** ERROR IN FISHER EXACT TEST--')
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
        WRITE(ICOUT,2201)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2204)
 2204   FORMAT('      NUMBER OF SETS    NUMSE2 = 0 ')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      AN1=N1
      AN2=N2
      ANUMS1=NUMSE1
      ANUMS2=NUMSE2
C
C               ***********************************************
C               **  STEP 2.3--                               **
C               **  COMPUTE COUNTS FOR EACH CELL             **
C               ***********************************************
C
      ISTEPN='23'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIS2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IWRITE='OFF'
C
      J=0
      DO2310ISET1=1,NUMSE1
        DO2320ISET2=1,NUMSE2
C
          K=0
          DO2330I=1,N1
            IF(XIDTEM(ISET1).EQ.Y1(I).AND.XIDTE2(ISET2).EQ.Y2(I))THEN
C
              K=K+1
            ENDIF
 2330     CONTINUE
          NTEMP=K
          J=J+1
          TEMP1(J)=REAL(K)
          TEMP2(J)=XIDTEM(ISET1)
          TEMP3(J)=XIDTE2(ISET2)
C
 2320   CONTINUE
 2310 CONTINUE
      NTEMP2=J
C
C     COMPUTE ROW AND COLUMN TOTALS AND GRAND TOTAL.
C
      J=0
      GTOTAL=0.0D0
C
      DO2340ISET1=1,NUMSE1
        ROWTOT(ISET1)=0.0D0
        DO2350ISET2=1,NUMSE2
          J=J+1
          ROWTOT(ISET1)=ROWTOT(ISET1) + DBLE(TEMP1(J))
          GTOTAL=GTOTAL + DBLE(TEMP1(J))
 2350   CONTINUE
C
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIS2')THEN
          WRITE(ICOUT,2352)ISET1,ROWTOT(ISET1)
 2352     FORMAT('ISET1,ROWTOT(ISET1)=',I5,1X,G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
 2340 CONTINUE
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIS2')THEN
        WRITE(ICOUT,2355)GTOTAL
 2355   FORMAT('GTOTAL=',G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      DO2360ISET2=1,NUMSE2
        COLTOT(ISET2)=0.0D0
        VALTMP=XIDTE2(ISET2)
        DO2370J=1,NTEMP2
          IF(TEMP3(J).EQ.XIDTE2(ISET2))THEN
            COLTOT(ISET2)=COLTOT(ISET2) + DBLE(TEMP1(J))
          ENDIF
 2370   CONTINUE
C
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIS2')THEN
          WRITE(ICOUT,2372)ISET2,COLTOT(ISET2)
 2372     FORMAT('ISET2,COLTOT(ISET2)=',I5,1X,G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
 2360 CONTINUE
C
      NROW=NUMSE1
      NCOL=NUMSE2
C
      J=0
C
      DO2380ISET1=1,NUMSE1
        DO2390ISET2=1,NUMSE2
          J=J+1
          EXP=ROWTOT(ISET1)*COLTOT(ISET2)/GTOTAL
          OBS=TEMP1(J)
          XMAT(ISET1,ISET2)=DBLE(OBS)
          WRITE(IOUNI1,2385)ISET1,ISET2,ROWTOT(ISET1),COLTOT(ISET2),
     1                      EXP,OBS
 2385     FORMAT(I8,I8,E15.7,E15.7,E15.7,E15.7)
 2390   CONTINUE
 2380 CONTINUE
      GOTO4000
C
 3000 CONTINUE
C
C               ********************************************
C               **  STEP 31--                             **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               **  ALL TABLE ENTRIES SHOULD BE           **
C               **  NON-NEGATIVE INTEGERS.  NEGATIVE      **
C               **  VALUES WILL BE FLAGGED AS ERRORS      **
C               **  WHILE NON-INTEGER VALUES WILL BE      **
C               **  ROUNDED TO NEAREST INTEGER.           **
C               **  SINCE WE ARE SCANNING TABLE, COMPUTE  **
C               **  ROW AND COLUMN TOTALS.                **
C               ********************************************
C
      ISTEPN='31'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIS2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IERROR='NO'
      NUMERR=0
      MAXERR=10
C
      DO3001I=1,NROW
        ROWTOT(I)=0.0D0
 3001 CONTINUE
      GTOTAL=0.0D0
C
      DO3010J=1,NCOL
        COLTOT(J)=0.0D0
        DO3020I=1,NROW
          IF(XMAT(I,J).LT.0.0D0)THEN
            NUMERR=NUMERR+1
            IF(NUMERR.GT.MAXERR)GOTO9000
            IERROR='YES'
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,1201)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,3021)I,J
 3021       FORMAT('      ROW ',I8,' AND COLUMN ',I8,
     1             ' OF THE INPUT TABLE')
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,3023)XMAT(I,J)
 3023       FORMAT('      IS NEGATIVE.  THE VALIE IS ',G15.7)
            CALL DPWRST('XXX','WRIT')
          ELSE
            ITEMP=INT(XMAT(I,J)+0.5D0)
            XMAT(I,J)=DBLE(ITEMP)
            COLTOT(J)=COLTOT(J) + XMAT(I,J)
            ROWTOT(I)=ROWTOT(I) + XMAT(I,J)
            GTOTAL=GTOTAL + XMAT(I,J)
          ENDIF
 3020   CONTINUE
 3010 CONTINUE
C
      DO3110I=1,NROW
        DO3120J=1,NCOL
          EXP=ROWTOT(I)*COLTOT(J)/GTOTAL
          WRITE(IOUNI1,2385)I,J,ROWTOT(I),COLTOT(J),
     1                      EXP,XMAT(I,J)
 3120   CONTINUE
 3110 CONTINUE
C
      IF(IERROR.EQ.'YES')GOTO9000
C
C               **********************************************
C               **  STEP 32--                               **
C               **  COMPUTE THE FISHER EXACT TEST STATISTIC **
C               **********************************************
C
      ISTEPN='32'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIS2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      AN1=REAL(GTOTAL)
      AN2=REAL(GTOTAL)
C
      GOTO4000
C
C               ********************************************
C               **  STEP 41--                             **
C               **  FOR ALL INPUT METHODS (SCALAR,        **
C               **  TWO VARIABLES, TABLE), CALL FEXACT    **
C               **  AND PRINT THE RESULTS.                **
C               ********************************************
C
 4000 CONTINUE
C
      ISTEPN='41'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIS2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IOP='CLOS'
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1            IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1            IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
C     NOTE THAT EXPECT, PERCNT, AND EMIN ARE USED TO DEFINE
C     WHEN CHI-SQUARE APPROXIMATIONS CAN BE USED.  WE USE THE
C     DEFAULT "COCHRAN CONDITION" SETTINGS.  ONCE BASIC CODE IS
C     DEBUGGED, WE WILL MAKE THESE VALUES SETTABLE VIA SET
C     COMMANDS.
C
      LDTABL=MAXLEV
CCCCC EXPECT=5.0D0
      EXPECT=-1.0D0
      PERCNT=80.0D0
      EMIN=1.0D0
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FIS2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,4011)
 4011   FORMAT('***** BEFORE CALL FEXACT')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,4012)LDTABL,EXPECT,PERCNT,EMIN
 4012   FORMAT('LDTABL,EXPECT,PERCNT,EMIN=',4G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,4013)NROW,NCOL,IWKMX
 4013   FORMAT('NROW,NCOL,IWKMX = ',3I8)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      CALL FEXACT(NROW,NCOL,XMAT,LDTABL,EXPECT,PERCNT,
     1             EMIN,PRT,PRE,
     1             RWORK,DWORK,IWORK,IWKMX)
      STATVA=REAL(PRT)
      PVAL=REAL(PRE)
      CDF=1.0 - PVAL
C
      IWRITE='OFF'
C
      ICONC1='REJECT'
      ICONC2='REJECT'
      ICONC3='REJECT'
      ICONC4='REJECT'
      ICONC5='REJECT'
C
      IF(0.250.LE.CDF.AND.CDF.LE.0.750)ICONC1='ACCEPT'
      IF(0.100.LE.CDF.AND.CDF.LE.0.90)ICONC2='ACCEPT'
      IF(0.050.LE.CDF.AND.CDF.LE.0.95)ICONC3='ACCEPT'
      IF(0.025.LE.CDF.AND.CDF.LE.0.975)ICONC4='ACCEPT'
      IF(0.005.LE.CDF.AND.CDF.LE.0.995)ICONC5='ACCEPT'
C
C               ******************************
C               **   STEP 42--              **
C               **   WRITE OUT EVERYTHING   **
C               **   FOR FISHER EXACT TEST  **
C               ******************************
C
      ISTEPN='42'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FIS2')
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
      ITITLE='Fisher Exact Test for Independence (RxC Table)'
      NCTITL=46
      ITITLZ=' '
      NCTITZ=0
C
      ICNT=0
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='H0: The Two Variables Are Independent'
      NCTEXT(ICNT)=38
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: The Two Variables Are Not Independent'
      NCTEXT(ICNT)=42
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample 1:'
      NCTEXT(ICNT)=9
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Observations:'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=AN1
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Levels (rows):'
      NCTEXT(ICNT)=24
      AVALUE(ICNT)=REAL(NROW)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample 2:'
      NCTEXT(ICNT)=9
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Observations:'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=AN2
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Levels (Columns):'
      NCTEXT(ICNT)=27
      AVALUE(ICNT)=REAL(NCOL)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Probability of Observed Table:'
      NCTEXT(ICNT)=30
      AVALUE(ICNT)=STATVA
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='CDF Value of Test Statistic:'
      NCTEXT(ICNT)=26
      AVALUE(ICNT)=CDF
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='P-Value:'
      NCTEXT(ICNT)=8
      AVALUE(ICNT)=PVAL
      IDIGIT(ICNT)=NUMDIG
C
      NUMROW=ICNT
      DO5010I=1,NUMROW
        NTOT(I)=15
 5010 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,
     1            NCTEXT,AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ITITLE(1:14)='Two-Sided Test'
      NCTITL=14
      ITITL9=' '
      NCTIT9=0
C
      ITITL2(1,1)=' '
      NCTIT2(1,1)=0
      ITITL2(2,1)='Null'
      NCTIT2(2,1)=4
      ITITL2(3,1)='Hypothesis'
      NCTIT2(3,1)=10
C
      ITITL2(1,2)=' '
      NCTIT2(1,2)=0
      ITITL2(2,2)='Confidence'
      NCTIT2(2,2)=10
      ITITL2(3,2)='Level'
      NCTIT2(3,2)=5
C
      ITITL2(1,3)='Null Hypothesis'
      NCTIT2(1,3)=15
      ITITL2(2,3)='Acceptance'
      NCTIT2(2,3)=10
      ITITL2(3,3)='Interval'
      NCTIT2(3,3)=8
C
      ITITL2(1,4)='Null'
      NCTIT2(1,4)=4
      ITITL2(2,4)='Hypothesis'
      NCTIT2(2,4)=10
      ITITL2(3,4)='Conclusion'
      NCTIT2(3,4)=10
C
      NMAX=0
      NUMCOL=4
      DO5210I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IF(I.EQ.3 .OR. I.EQ.4)NTOT(I)=18
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='ALPH'
        IF(I.EQ.2)THEN
          IDIGIT(I)=1
        ELSE
          IDIGIT(I)=NUMDIG
        ENDIF
        IWHTML(1)=150
        IWHTML(2)=125
        IWHTML(3)=150
        IWHTML(4)=150
        IINC=1600
        IINC2=1400
        IINC3=2200
        IWRTF(1)=IINC
        IWRTF(2)=IWRTF(1)+IINC
        IWRTF(3)=IWRTF(2)+IINC3
        IWRTF(4)=IWRTF(3)+IINC2
C
        DO5289J=1,NUMALP
          IF(J.EQ.1)THEN
            IVALUE(J,2)='50.0%'
            NCVALU(J,2)=5
            IVALUE(J,3)='(0.250,0.750)'
            NCVALU(J,3)=13
            IVALUE(J,4)(1:6)=ICONC1(1:6)
            NCVALU(J,4)=6
          ELSEIF(J.EQ.2)THEN
            IVALUE(J,2)='80.0%'
            NCVALU(J,2)=5
            IVALUE(J,3)='(0.100,0.900)'
            NCVALU(J,3)=13
            IVALUE(J,4)(1:6)=ICONC2(1:6)
            NCVALU(J,4)=6
          ELSEIF(J.EQ.3)THEN
            IVALUE(J,2)='90.0%'
            NCVALU(J,2)=5
            IVALUE(J,3)='(0.050,0.950)'
            NCVALU(J,3)=13
            IVALUE(J,4)(1:6)=ICONC3(1:6)
            NCVALU(J,4)=6
          ELSEIF(J.EQ.4)THEN
            IVALUE(J,2)='95.0%'
            NCVALU(J,2)=5
            IVALUE(J,3)='(0.025,0.975)'
            NCVALU(J,3)=13
            IVALUE(J,4)(1:6)=ICONC4(1:6)
            NCVALU(J,4)=6
          ELSEIF(J.EQ.5)THEN
            IVALUE(J,2)='99.0%'
            NCVALU(J,2)=5
            IVALUE(J,3)='(0.005,0.995)'
            NCVALU(J,3)=13
            IVALUE(J,4)(1:6)=ICONC5(1:6)
            NCVALU(J,4)=6
          ENDIF
          AMAT(J,1)=0.0
          AMAT(J,2)=0.0
          AMAT(J,4)=0.0
          IVALUE(J,1)='Independent'
          NCVALU(J,1)=11
 5289   CONTINUE
C
 5210 CONTINUE
C
      ICNT=NUMALP
      NUMLIN=3
      NUMCOL=4
      IFRST=.TRUE.
      ILAST=.TRUE.
      IFLAGS=.TRUE.
      IFLAGE=.TRUE.
      CALL DPDTA5(ITITLE,NCTITL,
     1            ITITL9,NCTIT9,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            IFLAGS,IFLAGE,
     1            ISUBRO,IBUGA3,IERROR)
C
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FIS2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFIS2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9013)AN11,AN21,AN12,AN22
 9013   FORMAT('AN11,AN21,AN12,AN22=',4G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9015)AN1,AN2
 9015   FORMAT('AN1,AN2=',2G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9017)N11,N21,N12,N22
 9017   FORMAT('N11,N21,N12,N22=',4I8)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFITH(IHARG,IARGT,ARG,NUMARG,PDEFFT,MAXFIL,PFILTH,
     1IBUGP2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE FILL THICKNESSES.
C              THESE ARE LOCATED IN THE VECTOR PFILTH(.).
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --IARGT  (A  CHARACTER VECTOR)
C                     --ARG
C                     --NUMARG
C                     --PDEFFT
C                     --MAXFIL
C                     --IBUGP2 ('ON' OR 'OFF' )
C     OUTPUT ARGUMENTS--PFILTH (A FLOATING POINT VECTOR)
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
      DIMENSION PFILTH(*)
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
      NUMFIL=0
      IHOLD1='-999'
      HOLD1=-999.0
      HOLD2=-999.0
C
      IF(IBUGP2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPFITH--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGP2,IFOUND,IERROR
   52 FORMAT('IBUGP2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)MAXFIL,NUMFIL
   53 FORMAT('MAXFIL,NUMFIL = ',I8,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)IHOLD1,HOLD1,HOLD2
   54 FORMAT('IHOLD1,HOLD1,HOLD2 = ',A4,2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,55)PDEFFT
   55 FORMAT('PDEFFT = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,60)NUMARG
   60 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO65I=1,NUMARG
      WRITE(ICOUT,66)IHARG(I),IARGT(I),ARG(I)
   66 FORMAT('IHARG(I),IARGT(I),ARG(I) = ',A4,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,70)PFILTH(1)
   70 FORMAT('PFILTH(1) = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      DO75I=1,10
      WRITE(ICOUT,76)I,PFILTH(I)
   76 FORMAT('I,PFILTH(I) = ',I8,2X,E15.7)
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
      IF(NUMARG.LE.0)GOTO9000
      IF(NUMARG.EQ.1)GOTO1110
      IF(NUMARG.EQ.2)GOTO1120
      IF(NUMARG.EQ.3)GOTO1130
      GOTO1140
C
 1110 CONTINUE
      GOTO1200
C
 1120 CONTINUE
      IF(IHARG(2).EQ.'ALL')IHOLD1='    '
      IF(IHARG(2).EQ.'ALL')HOLD1=PDEFFT
      IF(IHARG(2).EQ.'ALL')GOTO1300
      GOTO1200
C
 1130 CONTINUE
      IF(IHARG(2).EQ.'ALL')IHOLD1=IHARG(3)
      IF(IHARG(2).EQ.'ALL')HOLD1=ARG(3)
      IF(IHARG(2).EQ.'ALL')GOTO1300
      IF(IHARG(3).EQ.'ALL')IHOLD1=IHARG(2)
      IF(IHARG(3).EQ.'ALL')HOLD1=ARG(2)
      IF(IHARG(3).EQ.'ALL')GOTO1300
      GOTO1200
C
 1140 CONTINUE
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
      IF(NUMARG.LE.1)GOTO1210
      GOTO1220
C
 1210 CONTINUE
      NUMFIL=1
      PFILTH(1)=PDEFFT
      GOTO1270
C
 1220 CONTINUE
      NUMFIL=NUMARG-1
      IF(NUMFIL.GT.MAXFIL)NUMFIL=MAXFIL
      DO1225I=1,NUMFIL
      J=I+1
      IHOLD1=IHARG(J)
      HOLD1=ARG(J)
      HOLD2=HOLD1
      IF(IHOLD1.EQ.'ON')HOLD2=PDEFFT
      IF(IHOLD1.EQ.'OFF')HOLD2=PDEFFT
      IF(IHOLD1.EQ.'AUTO')HOLD2=PDEFFT
      IF(IHOLD1.EQ.'DEFA')HOLD2=PDEFFT
      PFILTH(I)=HOLD2
 1225 CONTINUE
      GOTO1270
C
 1270 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1279
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO1278I=1,NUMFIL
      WRITE(ICOUT,1276)I,PFILTH(I)
 1276 FORMAT('FILL THICKNESS ',I6,' HAS JUST BEEN SET TO ',
     1E15.7)
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
      NUMFIL=MAXFIL
      HOLD2=HOLD1
      IF(IHOLD1.EQ.'ON')HOLD2=PDEFFT
      IF(IHOLD1.EQ.'OFF')HOLD2=PDEFFT
      IF(IHOLD1.EQ.'AUTO')HOLD2=PDEFFT
      IF(IHOLD1.EQ.'DEFA')HOLD2=PDEFFT
      DO1315I=1,NUMFIL
      PFILTH(I)=HOLD2
 1315 CONTINUE
      GOTO1370
C
 1370 CONTINUE
      IF(IFEEDB.EQ.'OFF')GOTO1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      I=1
      WRITE(ICOUT,1316)PFILTH(I)
 1316 FORMAT('ALL FILL THICKNESSES HAVE JUST BEEN SET TO ',
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
 9011 FORMAT('***** AT THE END       OF DPFITH--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGP2,IFOUND,IERROR
 9012 FORMAT('IBUGP2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)MAXFIL,NUMFIL
 9013 FORMAT('MAXFIL,NUMFIL = ',I8,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)IHOLD1,HOLD1,HOLD2
 9014 FORMAT('IHOLD1,HOLD1,HOLD2 = ',A4,2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)PDEFFT
 9015 FORMAT('PDEFFT = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9020)NUMARG
 9020 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO9025I=1,NUMARG
      WRITE(ICOUT,9026)IHARG(I),IARGT(I),ARG(I)
 9026 FORMAT('IHARG(I),IARGT(I),ARG(I) = ',A4,2X,A4,I8)
      CALL DPWRST('XXX','BUG ')
 9025 CONTINUE
      WRITE(ICOUT,9030)PFILTH(1)
 9030 FORMAT('PFILTH(1) = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      DO9035I=1,10
      WRITE(ICOUT,9036)I,PFILTH(I)
 9036 FORMAT('I,PFILTH(I) = ',I8,2X,E15.7)
      CALL DPWRST('XXX','BUG ')
 9035 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPFIWI(IHARG,IARGT,ARG,NUMARG,DEFFW,
     1FILWID,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE WIDTH (USUALLY INTEGER) OF THE FILTER
C              FOR A SMOOTHING OPERATION
C              FOR USE IN THE SMOOTH COMMAND.
C              THE SPECIFIED WIDTH WILL BE PLACED
C              IN THE FLOATING POINT VARIABLE FILWID.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --ARG    (A  HOLLERITH VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C                     --DEFFW  (A FLOATING POINT VARIABLE)
C     OUTPUT ARGUMENTS--FILWID (A FLOATING POINT INTEGER VARIABLE)
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
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--MAY      1981.
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
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(NUMARG.LE.0)GOTO1150
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'WIDT')GOTO1150
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
 1121 FORMAT('***** ERROR IN DPFIWI--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR FILTER WIDTH ',
     1'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1124)
 1124 FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE ',
     1'PROPER FORM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1125)
 1125 FORMAT('      SUPPOSE THE THE ANALYST WISHES ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1126)
 1126 FORMAT('      TO SET THE FILTER WIDTH = 7 OBSERVATIONS  ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1127)
 1127 FORMAT('      FOR SOME SMOOTHING OPERATION,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1128)
 1128 FORMAT('      THEN AN ALLOWABLE FORM IS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1129)
 1129 FORMAT('      FILTER WIDTH 7 ')
      CALL DPWRST('XXX','BUG ')
      GOTO1199
C
 1150 CONTINUE
      HOLD=DEFFW
      GOTO1180
C
 1160 CONTINUE
      HOLD=ARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      FILWID=HOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)FILWID
 1181 FORMAT('THE FILTER WIDTH HAS JUST BEEN SET TO ',
     1E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPFLTE(YTEMP,MAXNXT,
     1                  ICAPSW,IFORSW,IMULT,
     1                  IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--CARRY OUT F TEST FOR SHIFT IN LOCATION
C     EXAMPLE--F LOCATION TEST Y X
C     REFERENCE--XX
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--97/9
C     ORIGINAL VERSION--SEPTEMBER 1997.
C     UPDATED         --MAY       2011. SUPPORT FOR HTML, RTF AND LATEX
C                                       OUTPUT
C     UPDATED         --MAY       2011. USE DPPARS
C     UPDATED         --MAY       2011. SUPPORT FOR "MULTIPLE" CASE
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
      CHARACTER*4 ICASE
C
      CHARACTER*4 IFLAGU
      LOGICAL IFRST
      LOGICAL ILAST
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
      DIMENSION YTEMP(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
C
      DIMENSION YMEAN(MAXOBV)
      DIMENSION YBARIV(MAXOBV)
      DIMENSION DTAG(MAXOBV)
C
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE(GARBAG(IGARB1),YBARIV(1))
      EQUIVALENCE(GARBAG(IGARB2),DTAG(1))
      EQUIVALENCE(GARBAG(IGARB3),YMEAN(1))
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPFL'
      ISUBN2='TE  '
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
C               **************************************
C               **  TREAT THE F LOCATION TEST CASE  **
C               **************************************
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'FLTE')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPFLTE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA2,IBUGA3,IBUGQ,ISUBRO
   52   FORMAT('IBUGA2,IBUGA3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)IMULT,MAXNXT
   55   FORMAT('IMULT,MAXNXT = ',A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *********************************
C               **  STEP 1--                   **
C               **  EXTRACT THE VARIABLE LIST  **
C               *********************************
C
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FLTE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='F LOCATION TEST'
      MAXNA=100
      MINNVA=1
      MAXNVA=100
      MINNA=1
      IFLAGE=1
      IFLAGM=0
      IF(IMULT.EQ.'ON')THEN
        IFLAGE=0
        IFLAGM=1
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FLTE')THEN
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
C               *******************************************************
C               **  STEP 3--                                         **
C               **  GENERATE THE F LOCATION     TEST FOR THE VARIOUS **
C               **  CASES                                            **
C               *******************************************************
C
      ISTEPN='3'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FLTE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               *****************************************
C               **  STEP 3A--                          **
C               **  CASE 1: TWO RESPONSE VARIABLES     **
C               **          WITH NO REPLICATION        **
C               *****************************************
C
      IF(IMULT.EQ.'OFF')THEN
        ISTEPN='3A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FLTE')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        ICOL=1
        NUMVA2=2
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              Y,X,YTEMP,NLOCAL,NLOCA2,NLOCA3,ICASE,
     1              IBUGA3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
C
C               *******************************************
C               **  STEP 3B--                            **
C               **  PREPARE FOR ENTRANCE INTO DPFLT2--   **
C               *******************************************
C
        ISTEPN='3B'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FLTE')THEN
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,331)
  331     FORMAT('***** FROM DPFLTE, AS WE ARE ABOUT TO CALL DPFLT2--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,332)NLOCAL
  332     FORMAT('NLOCAL = ',I8)
          CALL DPWRST('XXX','BUG ')
          DO335I=1,NLOCAL
            WRITE(ICOUT,336)I,Y(I),X(I)
  336       FORMAT('I,Y(I),X(I) = ',I8,2G15.7)
            CALL DPWRST('XXX','BUG ')
  335     CONTINUE
        ENDIF
C
        CALL DPFLT2(Y,X,NLOCAL,IVARN1,IVARN2,
     1              YTEMP,YMEAN,YBARIV,DTAG,MAXNXT,
     1              STATVA,STATCD,PVAL,
     1              CUT0,CUT50,CUT75,CUT90,CUT95,
     1              CUT975,CUT99,CUT999,
     1              ICAPSW,ICAPTY,IFORSW,IMULT,
     1              ISUBRO,IBUGA3,IERROR)
C
C               ***************************************
C               **  STEP 8C--                        **
C               **  UPDATE INTERNAL DATAPLOT TABLES  **
C               ***************************************
C
          ISTEPN='8C'
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FLTE')
     1      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
          IFLAGU='ON'
          IFRST=.TRUE.
          ILAST=.TRUE.
          CALL DPFRT5(STATVA,STATCD,PVAL,
     1                CUT0,CUT50,CUT75,CUT90,CUT95,
     1                CUT975,CUT99,CUT999,
     1                IFLAGU,IFRST,ILAST,
     1                IBUGA2,IBUGA3,ISUBRO,IERROR)
C
C               *******************************************************
C               **  STEP 4A--                                        **
C               **  CASE 2: MULTIPLE RESPONSE VARIABLES.  NOTE THAT  **
C               **          FOR F LOCATION     TEST, THE MULTIPLE    **
C               **          LABS ARE CONVERTED INTO A "Y X" STACKED  **
C               **          PAIR WHERE "X" IS THE LAB-ID VARIABLE.   **
C               *******************************************************
C
      ELSEIF(IMULT.EQ.'ON')THEN
        ISTEPN='4A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FLTE')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        ICOL=1
        NUMVA2=NUMVAR
        CALL DPPAR8(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              YTEMP,Y,X,NLOCAL,ICASE,
     1              IBUGA3,ISUBRO,IFOUND,IERROR)
        NUMVAR=2
        IF(IERROR.EQ.'YES')GOTO9000
C
        IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'FLTE')THEN
          ISTEPN='4B'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,442)
  442     FORMAT('***** FROM THE MIDDLE  OF DPFLTE--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,443)ICASAN,NUMVAR,NLOCAL
  443     FORMAT('ICASAN,NUMVAR,NLOCAL = ',A4,2I8)
          CALL DPWRST('XXX','BUG ')
          IF(NLOCAL.GE.1)THEN
            DO445I=1,NLOCAL
              WRITE(ICOUT,446)I,Y(I),X(I)
  446         FORMAT('I,Y(I),X(I) = ',I8,2G15.7)
              CALL DPWRST('XXX','BUG ')
  445       CONTINUE
          ENDIF
        ENDIF
C
        CALL DPFLT2(Y,X,NLOCAL,IVARN1,IVARN2,
     1              YTEMP,YMEAN,YBARIV,DTAG,MAXNXT,
     1              STATVA,STATCD,PVAL,
     1              CUT0,CUT50,CUT75,CUT90,CUT95,
     1              CUT975,CUT99,CUT999,
     1              ICAPSW,ICAPTY,IFORSW,IMULT,
     1              ISUBRO,IBUGA3,IERROR)
C
C         ***************************************
C         **  STEP 8C--                        **
C         **  UPDATE INTERNAL DATAPLOT TABLES  **
C         ***************************************
C
          ISTEPN='8C'
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'KRUS')
     1      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
          IFLAGU='ON'
          IFRST=.TRUE.
          ILAST=.TRUE.
          CALL DPFRT5(STATVA,STATCD,PVAL,
     1                CUT0,CUT50,CUT75,CUT90,CUT95,
     1                CUT975,CUT99,CUT999,
     1                IFLAGU,IFRST,ILAST,
     1                IBUGA2,IBUGA3,ISUBRO,IERROR)
C
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'FLTE')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFLTE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NLOCAL,STATVA,STATCD
 9014   FORMAT('NLOCAL,STATVA,STATCD = ',I8,2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)IFOUND,IERROR
 9016   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFLT2(Y,TAG,N,IVARID,IVARI2,
     1                  YTEMP,YMEAN,YBARIV,DTAG,MAXNXT,
     1                  STATVA,STATCD,PVAL,
     1                  CUT0,CUT50,CUT75,CUT90,CUT95,
     1                  CUT975,CUT99,CUT999,
     1                  ICAPSW,ICAPTY,IFORSW,IMULT,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS ROUTINE CARRIES OUT AN F TEST FOR SHIFT IN LOCATION
C     EXAMPLE--F LOCATION'S TEST Y TAG
C     REFERENCE--XX
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--97/9
C     ORIGINAL VERSION--SEPTEMBER 1997.
C     UPDATED         --MAY       2011. USE DPTAB1 AND DPDTA4 TO PRINT
C                                       OUTPUT TABLES.  THIS ADDS
C                                       HTML/LATEX/RTF SUPPORT AS WELL.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 IMULT
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
      CHARACTER*4 IVARID(*)
      CHARACTER*4 IVARI2(*)
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      DOUBLE PRECISION DSUM1
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION TAG(*)
      DIMENSION DTAG(*)
      DIMENSION YTEMP(*)
      DIMENSION YMEAN(*)
      DIMENSION YBARIV(*)
C
      PARAMETER (NUMALP=8)
      REAL ALPHA(NUMALP)
C
      PARAMETER(NUMCLI=4)
      PARAMETER(MAXLIN=1)
      PARAMETER (MAXROW=15)
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
      DATA ALPHA/
     1 0.0, 50.0, 75.0, 90.0, 95.0, 97.5, 99.0, 99.9/
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPFL'
      ISUBN2='T2  '
      IERROR='NO'
      IWRITE='OFF'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FLT2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPFLT2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N,MAXNXT
   52   FORMAT('IBUGA3,ISUBRO,N,MAXNXT = ',2(A4,2X),2I8)
        CALL DPWRST('XXX','WRIT')
        DO56I=1,N
          WRITE(ICOUT,57)I,Y(I),TAG(I)
   57     FORMAT('I,Y(I),TAG(I) = ',I8,2G15.7)
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FLT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N.LE.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1111)
 1111   FORMAT('***** ERROR IN F LOCATION TEST--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1113)
 1113   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE ',
     1         'VARIABLE IS LESS THAN 2.')
        WRITE(ICOUT,1115)N
 1115   FORMAT('      THE SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      HOLD=Y(1)
      DO1135I=2,N
        IF(Y(I).NE.HOLD)GOTO1139
 1135 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1111)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1133)HOLD
 1133 FORMAT('      THE RESPONSE VARIABLE HAS ALL ELEMENTS = ',G15.7)
      CALL DPWRST('XXX','WRIT')
      GOTO9000
 1139 CONTINUE
C
      HOLD=TAG(1)
      DO1235I=2,N
        IF(TAG(I).NE.HOLD)GOTO1239
 1235 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1111)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1231)HOLD
 1231 FORMAT('      THE GROUP-ID VARIABLE HAS ALL ELEMENTS = ',G15.7)
      CALL DPWRST('XXX','WRIT')
      GOTO9000
 1239 CONTINUE
C
C               ******************************
C               **  STEP 21--               **
C               **  CARRY OUT CALCULATIONS  **
C               **  FOR F LOCATION  TEST    **
C               ******************************
C
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FLT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL MEAN(Y,N,IWRITE,YBAR,IBUGA3,IERROR)
      CALL SD(Y,N,IWRITE,YSD,IBUGA3,IERROR)
      CALL DISTIN(TAG,N,IWRITE,DTAG,NUMDIS,IBUGA3,IERROR)
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FLT2')THEN
        WRITE(ICOUT,2111)YBAR
 2111   FORMAT('YBAR = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        DO2115I=1,NUMDIS
          WRITE(ICOUT,2116)I,DTAG(I)
 2116     FORMAT('I,DTAG(I) =',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
 2115   CONTINUE
      ENDIF
C
      DO2200IDIS=1,NUMDIS
         J=0
         DO2300I=1,N
            IF(TAG(I).EQ.DTAG(IDIS))THEN
               J=J+1
               YTEMP(J)=Y(I)
            ENDIF
 2300    CONTINUE
         CALL MEAN(YTEMP,J,IWRITE,YMEAN(IDIS),IBUGA3,IERROR)
         DO2400I=1,N
           IF(TAG(I).EQ.DTAG(IDIS))YBARIV(I)=YMEAN(IDIS)
 2400    CONTINUE
 2200 CONTINUE
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FLT2')THEN
        DO2205I=1,N
          WRITE(ICOUT,2206)I,DTAG(I),YBARIV(I)
 2206     FORMAT('I,DTAG(I),YBARIV(I)=',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 2205   CONTINUE
      ENDIF
C
      DSUM1=0.D0
      DO2600I=1,N
        DSUM1=DSUM1 + (YBARIV(I)-YBAR)**2
 2600 CONTINUE
      SSQ=SNGL(DSUM1)
      NUMDF=NUMDIS-1
      ANUMMS=SSQ/REAL(NUMDF)
C
      DSUM1=0.D0
      DO2610I=1,N
        DSUM1=DSUM1 + (Y(I)-YBARIV(I))**2
 2610 CONTINUE
      SSQ=SNGL(DSUM1)
      IDENDF=N-NUMDIS
      DENMS=SSQ/REAL(IDENDF)
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FLT2')THEN
        WRITE(ICOUT,2612)ANUMMS,DENMS
 2612   FORMAT('ANUMMS,DENMS=',2G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      STATVA=ANUMMS/DENMS
      CALL FCDF(STATVA,NUMDF,IDENDF,STATCD)
      PVAL=1.0 - STATCD
C
      KM1=NUMDIS-1
      NMK=N-NUMDIS
C
      CUT0=0.0
      CALL FPPF(.50,KM1,NMK,CUT50)
      CALL FPPF(.75,KM1,NMK,CUT75)
      CALL FPPF(.90,KM1,NMK,CUT90)
      CALL FPPF(.95,KM1,NMK,CUT95)
      CALL FPPF(.975,KM1,NMK,CUT975)
      CALL FPPF(.99,KM1,NMK,CUT99)
      CALL FPPF(.999,KM1,NMK,CUT999)
C
C               ******************************
C               **   STEP 42--              **
C               **   WRITE OUT EVERYTHING   **
C               **   FOR F LOCATION'S TEST  **
C               ******************************
C
      ISTEPN='42'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FLT2')
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
      ITITLE='F-Test for Shift in Location'
      NCTITL=28
      ITITLZ='(Assumption: Normality)'
      NCTITZ=24
C
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
C
        ICNT=ICNT+1
        ITEXT(ICNT)='Group-ID Variable: '
        WRITE(ITEXT(ICNT)(20:23),'(A4)')IVARID(2)(1:4)
        WRITE(ITEXT(ICNT)(24:27),'(A4)')IVARI2(2)(1:4)
        NCTEXT(ICNT)=27
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
      ENDIF
C
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='H0: Groups are Homogeneous with'
      NCTEXT(ICNT)=31
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='    Respect to Location'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: Groups are Not Homogeneous with'
      NCTEXT(ICNT)=35
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='    Respect to Location'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
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
      AVALUE(ICNT)=REAL(NUMDIS)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='F Location Test Statistic Value:'
      NCTEXT(ICNT)=32
      AVALUE(ICNT)=STATVA
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FLT2')
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
C
      ITITL9=' '
      NCTIT9=0
      ITITLE(1:55)=
     1'Percent Points of the F Reference Distribution'
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
      ILAST=.FALSE.
C
      ISTEPN='42C'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FLT2')
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
      ISTEPN='42D'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FLT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FLT2')
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FLT2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFLT2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9025)STATVA,STATCD,PVAL
 9025   FORMAT('STATVA,STATCD,PVAL = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFLUC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                  IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--GENERATE A FLUCTUATION PLOT--THIS IS A VARIANT OF
C              THE MOSAIC PLOT IN WHICH THE CELL AREAS ARE ALL
C              EQUAL SIZE AND WE THEN "COLORIZE" A PORTION OF THAT
C              CELL AREA BASED ON THE PROPORTION FOR THAT CELL.
C              WE CURRENTLY SUPPORT THIS PLOT FOR TWO-WAY THROUGH
C              SIX-WAY TABLES.  THE DATA CAN BE EITHER RAW DATA
C
C                  X1  = CATEGORY LEVEL FOR VARIABLE 1
C                  X2  = CATEGORY LEVEL FOR VARIABLE 2
C                  X3  = CATEGORY LEVEL FOR VARIABLE 3
C                  X4  = CATEGORY LEVEL FOR VARIABLE 4
C                  X5  = CATEGORY LEVEL FOR VARIABLE 4
C                  X6  = CATEGORY LEVEL FOR VARIABLE 4
C
C              OR A MATRIX.  A MATRIX REPRESENTS DATA THAT
C              IS ALREADY CROSS-TABULATED FOR A TWO-WAY TABLE.
C
C              NOTE THAT WE EXTENED THE FLUCUATION PLOT TO ALLOW
C              ANY OF DATAPLOT'S SUPPORTED STATISTICS TO BE
C              PLOTTED (THE DEFAULT IS COUNTS).
C
C     EXAMPLES--FLUCTUATION PLOT X1
C             --FLUCTUATION PLOT X1 X2
C             --FLUCTUATION PLOT X1 X2 X3
C             --FLUCTUATION PLOT X1 X2 X3 X4
C             --FLUCTUATION PLOT X1 X2 X3 X4 X5
C             --FLUCTUATION PLOT X1 X2 X3 X4 X5 X6
C             --FLUCTUATION PLOT TABLE
C             --FLUCTUATION MEAN PLOT Y X1 X2
C     REFERENCE--UNWIN, THEUS, AND HOFMANN (2006), "GRAPHICS OF
C                LARGE DATA SETS: VISUALIZING A MILLION",
C                SPRINGER, P. 46, CHAPTER 5.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2008/5
C     ORIGINAL VERSION--MAY       2008.
C     UPDATED         --JANUARY   2009. SUPPORT CASE FOR TABLE INPUT
C                                       (THIS IS RESTRICTED TO THE
C                                       CASE WITH TWO CLASSICATION
C                                       VARIABLES--INPUT TABLE CONTAINS
C                                       PREVIOUSLY CROSS-TABULATED
C                                       VALUES)
C     UPDATED         --FEBRUARY  2009. GRUBB
C     UPDATED         --FEBRUARY  2009. GRUBB CDF
C     UPDATED         --FEBRUARY  2009. ONE SAMPLE T TEST
C                                       ONE SAMPLE T TEST CDF
C     UPDATED         --FEBRUARY  2009. CHI-SQUARE SD TEST
C                                       CHI-SQUARE SD TEST CDF
C     UPDATED         --FEBRUARY  2009. FREQUENCY TEST
C                                       FREQUENCY TEST CDF
C     UPDATED         --FEBRUARY  2009. FREQUENCY WITHIN A BLOCK TEST
C                                       FREQUENCY WITHIN A BLOCK TEST CDF
C     UPDATED         --MARCH     2009. PARSE WITH "EXTSTA"
C     UPDATED         --SEPTEMBER 2009. ADD "UNCERTAINTY INTERVALS"
C                                       FOR BINOMIAL PROPORTIONS AND
C                                       MEAN/MEDIAN CONFIDENCE LIMITS
C     UPDATED         --MARCH     2010. DIFFERENT FORMAT FOR
C                                       UNCERTAINTY INTERVALS
C     UPDATED         --APRIL     2010. ADD "CONTOUR" OPTION
C     UPDATED         --JUNE      2010. ADD "SORT" OPTION FOR 2 GROUP-ID
C                                       VARIABLES CASE
C     UPDATED         --JUNE      2010. CMPSTA SUPPORTS 3 RESPONSE
C                                       VARIABLES
C     UPDATED         --SEPTEMBER 2016. MODIFY HOW MATRIX ARGUMENTS ARE
C                                       HANDLED
C     UPDATED         --NOVEMBER  2017. DIFFERENCE OF MEAN AND
C                                       DIFFERENCE OF BINOMIAL
C                                       PROPORTIONS SUPPORT UNCERTAINTY
C                                       INTERVALS
C     UPDATED         --JULY      2019. TWEAK SCRATCH STORAGE
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
      CHARACTER*4 ICASCT
      CHARACTER*4 IHP
      CHARACTER*4 IHP2
      CHARACTER*4 IXVAR
      CHARACTER*4 IX2VAR
      CHARACTER*4 IYVAR
C
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 ICASE
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 ISUBN0
      CHARACTER*4  ISTADF
      CHARACTER*60 ICTNAM
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      PARAMETER (MAXSPN=20)
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
      INCLUDE 'DPCOZD.INC'
C
      DIMENSION Y1(MAXOBV)
      DIMENSION Y2(MAXOBV)
      DIMENSION Y3(MAXOBV)
      DIMENSION Y4(MAXOBV)
      DIMENSION TMP11(MAXOBV)
      DIMENSION TMP12(MAXOBV)
      DIMENSION TMP13(MAXOBV)
      DIMENSION TMP14(MAXOBV)
C
      DIMENSION YLEVEL(MAXOBV)
C
      DIMENSION XH1DIS(MAXOBV)
      DIMENSION XH2DIS(MAXOBV)
      DIMENSION XH3DIS(MAXOBV)
      DIMENSION XH4DIS(MAXOBV)
      DIMENSION XH5DIS(MAXOBV)
      DIMENSION XH6DIS(MAXOBV)
C
      DIMENSION X1(MAXOBV)
      DIMENSION X2(MAXOBV)
      DIMENSION X3(MAXOBV)
      DIMENSION X4(MAXOBV)
      DIMENSION X5(MAXOBV)
      DIMENSION X6(MAXOBV)
C
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TEMP3(MAXOBV)
      DIMENSION TEMP4(MAXOBV)
      DIMENSION TEMP5(MAXOBV)
      DIMENSION TEMP6(MAXOBV)
      DIMENSION TEMP7(MAXOBV)
      DIMENSION TEMP8(MAXOBV)
      DIMENSION TEMP9(MAXOBV)
      DIMENSION TMP10(MAXOBV)
c
      DIMENSION XNTRIA(MAXOBV)
      DIMENSION XACLOW(MAXOBV)
      DIMENSION XACUPP(MAXOBV)
C
      PARAMETER(MAXLEV=1000)
      DIMENSION XMAT(MAXLEV,MAXLEV)
C
      DIMENSION ITEMP1(MAXOBV)
      DIMENSION ITEMP2(MAXOBV)
      DIMENSION ITEMP3(MAXOBV)
      DIMENSION ITEMP4(MAXOBV)
      DIMENSION ITEMP5(MAXOBV)
      DIMENSION ITEMP6(MAXOBV)
      DOUBLE PRECISION DTEMP1(MAXOBV)
      DOUBLE PRECISION DTEMP2(MAXOBV)
      DOUBLE PRECISION DTEMP3(MAXOBV)
C
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),Y2(1))
      EQUIVALENCE (GARBAG(IGARB3),Y3(1))
      EQUIVALENCE (GARBAG(IGARB4),TMP11(1))
      EQUIVALENCE (GARBAG(IGARB5),TMP12(1))
      EQUIVALENCE (GARBAG(IGARB6),TMP13(1))
      EQUIVALENCE (GARBAG(IGARB7),X1(1))
      EQUIVALENCE (GARBAG(IGARB8),X2(1))
      EQUIVALENCE (GARBAG(IGARB9),X3(1))
      EQUIVALENCE (GARBAG(IGAR10),X4(1))
      EQUIVALENCE (GARBAG(JGAR11),X5(1))
      EQUIVALENCE (GARBAG(JGAR12),X6(1))
      EQUIVALENCE (GARBAG(JGAR13),XH1DIS(1))
      EQUIVALENCE (GARBAG(JGAR14),XH2DIS(1))
      EQUIVALENCE (GARBAG(JGAR15),XH3DIS(1))
      EQUIVALENCE (GARBAG(JGAR16),XH4DIS(1))
      EQUIVALENCE (GARBAG(JGAR17),XH5DIS(1))
      EQUIVALENCE (GARBAG(JGAR18),XH6DIS(1))
      EQUIVALENCE (GARBAG(JGAR19),Y4(1))
      EQUIVALENCE (GARBAG(JGAR20),TMP14(1))
      EQUIVALENCE (GARBAG(IGAR11),TEMP1(1))
      EQUIVALENCE (GARBAG(IGAR12),TEMP2(1))
      EQUIVALENCE (GARBAG(IGAR13),TEMP3(1))
      EQUIVALENCE (GARBAG(IGAR14),TEMP4(1))
      EQUIVALENCE (GARBAG(IGAR15),TEMP5(1))
      EQUIVALENCE (GARBAG(IGAR16),TEMP6(1))
      EQUIVALENCE (GARBAG(IGAR17),TEMP7(1))
      EQUIVALENCE (GARBAG(IGAR18),TEMP8(1))
      EQUIVALENCE (GARBAG(IGAR19),TEMP9(1))
      EQUIVALENCE (GARBAG(IGAR20),TMP10(1))
      EQUIVALENCE (GARBAG(IGAR21),XNTRIA(1))
      EQUIVALENCE (GARBAG(IGAR22),XACLOW(1))
      EQUIVALENCE (GARBAG(IGAR23),XACUPP(1))
      EQUIVALENCE (GARBAG(IGAR24),YLEVEL(1))
      EQUIVALENCE (GARBAG(IGAR25),XMAT(1,1))
C
      EQUIVALENCE (IGARBG(IIGAR1),ITEMP1(1))
      EQUIVALENCE (IGARBG(IIGAR2),ITEMP2(1))
      EQUIVALENCE (IGARBG(IIGAR3),ITEMP3(1))
      EQUIVALENCE (IGARBG(IIGAR4),ITEMP4(1))
      EQUIVALENCE (IGARBG(IIGAR5),ITEMP5(1))
      EQUIVALENCE (IGARBG(IIGAR6),ITEMP6(1))
C
      EQUIVALENCE (DGARBG(IDGAR1),DTEMP1(1))
      EQUIVALENCE (DGARBG(IDGAR2),DTEMP2(1))
      EQUIVALENCE (DGARBG(IDGAR3),DTEMP3(1))
C
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IERROR='NO'
      IFOUND='NO'
      ISUBN1='DPFL'
      ISUBN2='UC  '
      ICASPL='FLUC'
      ICASCT=' '
      IYVAR='ON'
      IXVAR='OFF'
      IX2VAR='OFF'
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
C               ****************************************
C               **  TREAT THE FLUCTUATION PLOT CASE   **
C               ****************************************
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'FLUC')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPFLUC--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGG2,IBUGG3,IBUGQ,ISUBRO
   52   FORMAT('IBUGG2,IBUGG3,IBUGQ,ISUBRO = ',A4,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,IAND1,IAND2
   53   FORMAT('ICASPL,IAND1,IAND2 = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)MAXN,N2,NS
   54   FORMAT('MAXN,N2,NS = ',3I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***************************
C               **  STEP 1--             **
C               **  EXTRACT THE COMMAND  **
C               ***************************
C
      ISTEPN='11'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'FLUC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               *******************************************************
C               **  STEP 1.5--                                       **
C               **  SEARCH FOR FLUCUATION <STAT> PLOT                **
C               *******************************************************
C
C
      IF(NUMARG.LE.1)GOTO9000
      IF(ICOM.NE.'FLUC')GOTO9000
C
CCCCC MARCH 2009: USE "EXTSTA" TO PARSE.  NOTE THAT IF NO
CCCCC             STATISTIC IS GIVEN, WE ASSUME THE "COUNTS"
CCCCC             CASE.
C
      JMIN=1
      JMAX=MIN(NUMARG,JMIN+6)
      DO200I=JMIN,JMAX
        IF(IHARG(I).EQ.'CONT' .AND. IHARG(I+1).EQ.'PLOT')THEN
          ICASPL='FLCP'
          JMAX=I-1
          ILASTC=I+1
          GOTO209
        ENDIF
        IF(IHARG(I).EQ.'PLOT')THEN
          JMAX=I-1
          ILASTC=I
          GOTO209
        ENDIF
  200 CONTINUE
      IFOUND='NO'
      GOTO9000
  209 CONTINUE
C
      IFLAGT=0
      IF(JMAX.LT.JMIN)THEN
C
C       THIS IS THE CASE WHERE NO EXPLICIT STATISTIC IS GIVEN.  THIS
C       IS PRIMARILY USED FOR THE CASE WHERE THE RESPONSE IS A
C       PRE-COMPUTED STATISTIC.  IN PARTICULAR, IF A MATRIX ARGUMENT
C       IS USED, THIS WILL OFTEN BE A PRE-COMPUTED CROSS-TABULATION,
C       A CORRELATION MATRIX, AND SO ON.  IN ANY EVENT, WE WILL TREAT
C       THIS CASE AS A "MEAN".  THIS HAS THE EFFECT OF TREATING A
C       PRE-COMPUTED STATISTIC AS THE VALUE.
C
        IFOUND='NO'
      ELSE
        CALL EXTSTA(ICOM,ICOM2,IHARG,IHARG2,IARGT,ARG,NUMARG,JMIN,JMAX,
     1              ICASCT,ICTNAM,ISTANR,ISTADF,IFOUND,ILOCV,
     1              ISUBRO,IBUGG3,IERROR)
      ENDIF
C
      IF(IFOUND.EQ.'YES')THEN
        IYVAR='ON'
        IXVAR='OFF'
        IX2VAR='OFF'
        IF(ISTANR.GE.2)IXVAR='ON'
        IF(ISTANR.GE.3)IX2VAR='ON'
        IF(ICTNAM.EQ.'NUMB')IYVAR='OFF'
      ELSE
        ISTANR=1
        ICASCT='MEAN'
        ICTNAM='MEAN'
        IYVAR='ON'
        IXVAR='OFF'
        IX2VAR='OFF'
        ILOCV=2
        IFOUND='YES'
      ENDIF
C
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
C
C               *********************************
C               **  STEP 2--                   **
C               **  EXTRACT THE VARIABLE LIST  **
C               *********************************
C
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'FLUC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='FLUCTUATION PLOT'
      MINNA=1
      MAXNA=100
      MAXVAR=100
      MINN2=2
      IFLAGE=1
      IF(ICASPL.EQ.'FLCP')IFLAGE=99
      IFLAGM=1
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=-99
      MAXNVA=-99
C
      CALL DPPARS(IHARG,IHARG2,IARGT,ARG,NUMARG,IANS,IWIDTH,
     1            IHNAME,IHNAM2,IUSE,NUMNAM,IN,IVALUE,VALUE,
     1            JMIN,JMAX,
     1            MINN2,MINNA,MAXNA,MAXVAR,IFLAGE,INAME,
     1            IVARN1,IVARN2,IVARTY,PVAR,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,ILOCQ,NUMVAR,
     1            MINNVA,MAXNVA,
     1            IFLAGM,IFLAGP,
     1            IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'FLUC')THEN
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
C     IF MATRIX ARGUMENTS GIVEN, THEN ALL RESPONSES MUST BE MATRICES
C     AND ALL MATRICES MUST HAVE SAME DIMENSION.
C
      IFLAGM=0
      DO260I=1,NUMVAR
        IF(IVARTY(I).EQ.'MATR')IFLAGM=1
  260 CONTINUE
C
      IF(IFLAGM.EQ.1)THEN
C
        NRESP=ISTANR
        NLVARI=0
        IF(ICASPL.EQ.'FLCP')NLVARI=1
        NCRTV=2
C
        DO291I=1,NRESP
          IF(IVARTY(I).NE.'MATR')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,311)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,292)
  292       FORMAT('      IF ONE RESPONSE VARIABLE IS A MATRIX, ',
     1             'THEN ALL MUST BE MATRICES.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,293)I
  293       FORMAT('      RESPONSE VARIABLE ',I5,' IS NOT A MATRIX.')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ELSE
             ILISR=ILIS(I)
             NRTEMP=IN(ILISR)
             ICOL1=IVALUE(ILISR)
             ICOL2=IVALU2(ILISR)
             NCTEMP=(ICOL2 - ICOL1) + 1
             IF(I.EQ.1)THEN
               NROW=NRTEMP
               NCOL=NCTEMP
             ELSE
               IF(NRTEMP.NE.NROW .OR. NCTEMP.NE.NCOL)THEN
                 WRITE(ICOUT,999)
                 CALL DPWRST('XXX','BUG ')
                 WRITE(ICOUT,311)
                 CALL DPWRST('XXX','BUG ')
                 WRITE(ICOUT,296)
  296            FORMAT('      FOR MATRIX RESPONSE VARIABLES, THE ',
     1                  'ROW AND COLUMN DIMENSIONS MUST BE EQUAL.')
                 CALL DPWRST('XXX','BUG ')
                 WRITE(ICOUT,297)NROW,NCOL
  297            FORMAT('      THE FIRST MATRIX HAS ',I5,' ROWS AND ',
     1                  I5,' COLUMNS.')
                 CALL DPWRST('XXX','BUG ')
                 WRITE(ICOUT,298)I,NRTEMP,NCTEMP
  298            FORMAT('      MATRIX ',I2,' HAS ',I5,' ROWS AND ',
     1                  I5,' COLUMNS.')
                 CALL DPWRST('XXX','BUG ')
                 IERROR='YES'
                 GOTO9000
               ENDIF
             ENDIF
          ENDIF
  291   CONTINUE
C
        NTEMP=NRESP + NLVARI
        IF(NTEMP.NE.NUMVAR)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,311)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,272)
  272     FORMAT('      WHEN MATRIX ARGUMENTS ARE GIVEN, THE ',
     1           'NUMBER OF MATRICES')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,274)
  274     FORMAT('      MUST BE THE SAME AS THE NUMBER OF RESPONSE ',
     1           'VARIABLES FOR THE SELECTED STATISTIC.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,276)NUMVAR-NLVARI
  276     FORMAT('      THE NUMBER OF MATRICES ENTERED = ',I5)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,278)ISTANR
  278     FORMAT('      THE NUMBER OF MATRICES EXPECTED = ',I5)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
C
        GOTO400
C
      ENDIF
C
C               ******************************************************
C               **  STEP 3--                                        **
C               **  CHECK FOR ALLOWABLE NUMBER OF CROSS TABULATION  **
C               **  VARIABLES.                                      **
C               ******************************************************
C
     
      ISTEPN='3'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'FLUC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC IF(IVARTY(1).EQ.'MATR')GOTO5000
C
      NRESP=ISTANR
      NCRTV=NUMVAR - NRESP
      IF(ICASPL.EQ.'FLCP')NCRTV=NCRTV-1
      IF(NCRTV.LT.1 .OR. NCRTV.GT.6)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,311)
  311   FORMAT('***** ERROR IN FLUCTUATION PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,312)
  312   FORMAT('      THE NUMBER OF CROSS TABULATION VARIABLES MUST')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,313)
  313   FORMAT('      BE BETWEEN 1 AND 6.  SUCH WAS NOT THE CASE HERE;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,314)NCRTV
  314   FORMAT('      THE SPECIFIED NUMBER OF CROSS TABULATION ',
     1         'VARIABLES WAS ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,318)(IANS(I),I=1,MIN(80,IWIDTH))
  318     FORMAT(80A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               *******************************
C               **  STEP 4--                 **
C               **  CREATE THE VARIABLES     **
C               *******************************
C
  400 CONTINUE
C
      ISTEPN='4'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'FLUC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IFLAGM.EQ.1)THEN
        ICOL=1
        CALL DPPARZ(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              Y1,X1,X2,NLOCAL,
     1              IBUGG3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
        IF(NRESP.GE.2)THEN
          ICOL=2
          CALL DPPARZ(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1                INAME,IVARN1,IVARN2,IVARTY,
     1                ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1                MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1                MAXCP4,MAXCP5,MAXCP6,
     1                V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1                Y2,X3,X4,N2,
     1                IBUGG3,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
        ENDIF
C
        IF(NRESP.GE.3)THEN
          ICOL=3
          CALL DPPARZ(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1                INAME,IVARN1,IVARN2,IVARTY,
     1                ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1                MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1                MAXCP4,MAXCP5,MAXCP6,
     1                V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1                Y3,X3,X4,N3,
     1                IBUGG3,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
        ENDIF
C
        GOTO499
      ENDIF
C
      J=0
      IMAX=NRIGHT(1)
      IF(NQ.LT.NRIGHT(1))IMAX=NQ
      DO410I=1,IMAX
        IF(ISUB(I).EQ.0)GOTO410
        J=J+1
C
        IJ=MAXN*(ICOLR(1)-1)+I
        IF(ISTANR.LT.1)THEN
          Y1(J)=0.0
        ELSE
          IF(ICOLR(1).LE.MAXCOL)Y1(J)=V(IJ)
          IF(ICOLR(1).EQ.MAXCP1)Y1(J)=PRED(I)
          IF(ICOLR(1).EQ.MAXCP2)Y1(J)=RES(I)
          IF(ICOLR(1).EQ.MAXCP3)Y1(J)=YPLOT(I)
          IF(ICOLR(1).EQ.MAXCP4)Y1(J)=XPLOT(I)
          IF(ICOLR(1).EQ.MAXCP5)Y1(J)=X2PLOT(I)
          IF(ICOLR(1).EQ.MAXCP6)Y1(J)=TAGPLO(I)
        ENDIF
C
        IJ=MAXN*(ICOLR(2)-1)+I
        IF(ISTANR.LT.2)THEN
          Y2(J)=0.0
        ELSE
          IF(ICOLR(2).LE.MAXCOL)Y2(J)=V(IJ)
          IF(ICOLR(2).EQ.MAXCP1)Y2(J)=PRED(I)
          IF(ICOLR(2).EQ.MAXCP2)Y2(J)=RES(I)
          IF(ICOLR(2).EQ.MAXCP3)Y2(J)=YPLOT(I)
          IF(ICOLR(2).EQ.MAXCP4)Y2(J)=XPLOT(I)
          IF(ICOLR(2).EQ.MAXCP5)Y2(J)=X2PLOT(I)
          IF(ICOLR(2).EQ.MAXCP6)Y2(J)=TAGPLO(I)
        ENDIF
C
        IJ=MAXN*(ICOLR(3)-1)+I
        IF(ISTANR.LT.3)THEN
          Y3(J)=0.0
        ELSE
          IF(ICOLR(3).LE.MAXCOL)Y3(J)=V(IJ)
          IF(ICOLR(3).EQ.MAXCP1)Y3(J)=PRED(I)
          IF(ICOLR(3).EQ.MAXCP2)Y3(J)=RES(I)
          IF(ICOLR(3).EQ.MAXCP3)Y3(J)=YPLOT(I)
          IF(ICOLR(3).EQ.MAXCP4)Y3(J)=XPLOT(I)
          IF(ICOLR(3).EQ.MAXCP5)Y3(J)=X2PLOT(I)
          IF(ICOLR(3).EQ.MAXCP6)Y3(J)=TAGPLO(I)
        ENDIF
C
        ICNT=ISTANR+1
        IF(NCRTV.GE.1)THEN
          IJ=MAXN*(ICOLR(ICNT)-1)+I
          IF(ICOLR(ICNT).LE.MAXCOL)X1(J)=V(IJ)
          IF(ICOLR(ICNT).EQ.MAXCP1)X1(J)=PRED(I)
          IF(ICOLR(ICNT).EQ.MAXCP2)X1(J)=RES(I)
          IF(ICOLR(ICNT).EQ.MAXCP3)X1(J)=YPLOT(I)
          IF(ICOLR(ICNT).EQ.MAXCP4)X1(J)=XPLOT(I)
          IF(ICOLR(ICNT).EQ.MAXCP5)X1(J)=X2PLOT(I)
          IF(ICOLR(ICNT).EQ.MAXCP6)X1(J)=TAGPLO(I)
        ELSE
          X1(J)=0.0
        ENDIF
C
        ICNT=ISTANR+2
        IF(NCRTV.GE.2)THEN
          IJ=MAXN*(ICOLR(ICNT)-1)+I
          IF(ICOLR(ICNT).LE.MAXCOL)X2(J)=V(IJ)
          IF(ICOLR(ICNT).EQ.MAXCP1)X2(J)=PRED(I)
          IF(ICOLR(ICNT).EQ.MAXCP2)X2(J)=RES(I)
          IF(ICOLR(ICNT).EQ.MAXCP3)X2(J)=YPLOT(I)
          IF(ICOLR(ICNT).EQ.MAXCP4)X2(J)=XPLOT(I)
          IF(ICOLR(ICNT).EQ.MAXCP5)X2(J)=X2PLOT(I)
          IF(ICOLR(ICNT).EQ.MAXCP6)X2(J)=TAGPLO(I)
        ELSE
          X2(J)=0.0
        ENDIF
C
        ICNT=ISTANR+3
        IF(NCRTV.GE.3)THEN
          IJ=MAXN*(ICOLR(ICNT)-1)+I
          IF(ICOLR(ICNT).LE.MAXCOL)X3(J)=V(IJ)
          IF(ICOLR(ICNT).EQ.MAXCP1)X3(J)=PRED(I)
          IF(ICOLR(ICNT).EQ.MAXCP2)X3(J)=RES(I)
          IF(ICOLR(ICNT).EQ.MAXCP3)X3(J)=YPLOT(I)
          IF(ICOLR(ICNT).EQ.MAXCP4)X3(J)=XPLOT(I)
          IF(ICOLR(ICNT).EQ.MAXCP5)X3(J)=X2PLOT(I)
          IF(ICOLR(ICNT).EQ.MAXCP6)X3(J)=TAGPLO(I)
        ELSE
          X3(J)=0.0
        ENDIF
C
        ICNT=ISTANR+4
        IF(NCRTV.GE.4)THEN
          IJ=MAXN*(ICOLR(ICNT)-1)+I
          IF(ICOLR(ICNT).LE.MAXCOL)X4(J)=V(IJ)
          IF(ICOLR(ICNT).EQ.MAXCP1)X4(J)=PRED(I)
          IF(ICOLR(ICNT).EQ.MAXCP2)X4(J)=RES(I)
          IF(ICOLR(ICNT).EQ.MAXCP3)X4(J)=YPLOT(I)
          IF(ICOLR(ICNT).EQ.MAXCP4)X4(J)=XPLOT(I)
          IF(ICOLR(ICNT).EQ.MAXCP5)X4(J)=X2PLOT(I)
          IF(ICOLR(ICNT).EQ.MAXCP6)X4(J)=TAGPLO(I)
        ELSE
          X4(J)=0.0
        ENDIF
C
        ICNT=ISTANR+5
        IF(NCRTV.GE.5)THEN
          IJ=MAXN*(ICOLR(ICNT)-1)+I
          IF(ICOLR(ICNT).LE.MAXCOL)X5(J)=V(IJ)
          IF(ICOLR(ICNT).EQ.MAXCP1)X5(J)=PRED(I)
          IF(ICOLR(ICNT).EQ.MAXCP2)X5(J)=RES(I)
          IF(ICOLR(ICNT).EQ.MAXCP3)X5(J)=YPLOT(I)
          IF(ICOLR(ICNT).EQ.MAXCP4)X5(J)=XPLOT(I)
          IF(ICOLR(ICNT).EQ.MAXCP5)X5(J)=X2PLOT(I)
          IF(ICOLR(ICNT).EQ.MAXCP6)X5(J)=TAGPLO(I)
        ELSE
          X5(J)=0.0
        ENDIF
C
        ICNT=ISTANR+6
        IF(NCRTV.GE.6)THEN
          IJ=MAXN*(ICOLR(ICNT)-1)+I
          IF(ICOLR(ICNT).LE.MAXCOL)X6(J)=V(IJ)
          IF(ICOLR(ICNT).EQ.MAXCP1)X6(J)=PRED(I)
          IF(ICOLR(ICNT).EQ.MAXCP2)X6(J)=RES(I)
          IF(ICOLR(ICNT).EQ.MAXCP3)X6(J)=YPLOT(I)
          IF(ICOLR(ICNT).EQ.MAXCP4)X6(J)=XPLOT(I)
          IF(ICOLR(ICNT).EQ.MAXCP5)X6(J)=X2PLOT(I)
          IF(ICOLR(ICNT).EQ.MAXCP6)X6(J)=TAGPLO(I)
        ELSE
          X6(J)=0.0
        ENDIF
C
  410 CONTINUE
      NLOCAL=J
C
  499 CONTINUE
C
      IF(ICASPL.EQ.'FLCP')THEN
        ICNT=NRESP+NCRTV+1
        J2=0
        IMAX=NRIGHT(ICNT)
        DO490I=1,IMAX
          J2=J2+1
C
          IJ=MAXN*(ICOLR(ICNT)-1)+I
          IF(ICOLR(ICNT).LE.MAXCOL)YLEVEL(J2)=V(IJ)
          IF(ICOLR(ICNT).EQ.MAXCP1)YLEVEL(J2)=PRED(I)
          IF(ICOLR(ICNT).EQ.MAXCP2)YLEVEL(J2)=RES(I)
          IF(ICOLR(ICNT).EQ.MAXCP3)YLEVEL(J2)=YPLOT(I)
          IF(ICOLR(ICNT).EQ.MAXCP4)YLEVEL(J2)=XPLOT(I)
          IF(ICOLR(ICNT).EQ.MAXCP5)YLEVEL(J2)=X2PLOT(I)
          IF(ICOLR(ICNT).EQ.MAXCP6)YLEVEL(J2)=TAGPLO(I)
  490   CONTINUE
        NLEVEL=J2
      ELSE
        NLEVEL=0
      ENDIF
C
C               *************************************
C               **  STEP 61--                      **
C               **  GENERATE THE FLUCTUATION PLOT  **
C               *************************************
C
      ISTEPN='61'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'FLUC')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,6001)NLOCAL,ICASPL
 6001   FORMAT('NLOCAL,ICASPL=',I5,1X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1   ICASCT.EQ.'DMEA' .OR. ICASCT.EQ.'DBPR' .OR.
     1   ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT')THEN
        IHP='ALPH'
        IHP2='A   '
        IHWUSE='P'
        MESSAG='NO'
        CALL CHECKN(IHP,IHP2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,
     1              NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,
     1              ILOCP,IERROR)
        IF(IERROR.EQ.'YES')THEN
          ALPHA=0.05
        ELSE
          ALPHA=VALUE(ILOCP)
          IF(ALPHA.LE.0.0)ALPHA=0.05
          IF(ALPHA.GE.1.0)ALPHA=0.05
        ENDIF
      ELSE
        ALPHA=0.05
      ENDIF
C
CCCCC GOTO6999
C
C5000 CONTINUE
C
C     MATRIX CASE.  IN THIS CASE, WE ASSUME THAT THE RAW
C     DATA HAS ALREADY BEEN CROSS-CLASSIFIED INTO A 2-WAY
C     TABLE OF COUNTS.  IN THIS CASE, WE ONLY GENERATE THE
C     FLUCTUATION PLOT FOR THE COUNTS CASE (I.E., NOT FOR
C     A STATISTIC SUCH AS THE MEAN).
C
CCCCC ICASCT='NUMB'
CCCCC ICTNAM='COUNT'
CCCCC NCRTV=2
CCCCC ICASE='TABL'
C
CCCCC ILISR=ILIS(1)
CCCCC N1=IN(ILISR)
CCCCC ICOL1=IVALUE(ILISR)
CCCCC ICOL2=IVALU2(ILISR)
CCCCC NCOL=(ICOL2 - ICOL1) + 1
CCCCC print *,'ilisr,n1,ncol=',ilisr,n1,ncol
C
CCCCC NLOOP=NCOL
CCCCC IF(NLOOP.LT.1)NLOOP=1
CCCCC IMAX=N1
CCCCC IF(NQ.LT.N1)IMAX=NQ
C
CCCCC JCOL=0
CCCCC DO5571JLOOP=1,NLOOP
CCCCC   J=0
CCCCC   DO5570I=1,IMAX
CCCCC     IF(ISUB(I).EQ.0)GOTO5570
CCCCC     J=J+1
CCCCC     ICOLT=ICOLR(1)+JLOOP-1
CCCCC     IJ=MAXN*(ICOLT-1)+I
C
CCCCC     IF(ICOLT.LE.MAXCOL)XMAT(J,JLOOP)=V(IJ)
CCCCC     IF(ICOLT.EQ.MAXCP1)XMAT(J,JLOOP)=PRED(I)
CCCCC     IF(ICOLT.EQ.MAXCP2)XMAT(J,JLOOP)=RES(I)
CCCCC     IF(ICOLT.EQ.MAXCP3)XMAT(J,JLOOP)=YPLOT(I)
CCCCC     IF(ICOLT.EQ.MAXCP4)XMAT(J,JLOOP)=XPLOT(I)
CCCCC     IF(ICOLT.EQ.MAXCP5)XMAT(J,JLOOP)=X2PLOT(I)
CCCCC     IF(ICOLT.EQ.MAXCP6)XMAT(J,JLOOP)=TAGPLO(I)
C
C5570   CONTINUE
c5571 CONTINUE
C
CCCCC NROW=J
C
C6999 CONTINUE
C
      CALL DPFLU2(Y1,Y2,Y3,X1,X2,X3,X4,X5,X6,NLOCAL,
     1            YLEVEL,NLEVEL,
     1            NUMV2,ICASCT,ICASE,ICASPL,
     1            XH1DIS,XH2DIS,XH3DIS,XH4DIS,XH5DIS,XH6DIS,
     1            TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,
     1            TEMP6,TEMP7,TEMP8,TEMP9,TMP10,
     1            TMP11,TMP12,TMP13,TMP14,
     1            ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1            DTEMP1,DTEMP2,DTEMP3,
     1            XMAT,MAXLEV,NROW,NCOL,
     1            ISEED,IQUASE,IBINME,IBI2ME,ICTAMV,
     1            PSTAMV,PCTAMV,ALPHA,
     1            IXVAR,IX2VAR,IYVAR,
     1            NCRTV,MAXOBV,PFLUFL,PFLUCL,IFLUWI,IFLUUN,
     1            IFLUCD,IFLUBP,IFLUDI,IFLUSO,IFLUSR,IFLUSC,IFLUBD,
     1            STATMN,STATMX,
     1            XACLOW,XACUPP,
     1            Y,X,D,DCOLOR,
     1            NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C               ***************************************
C               **  STEP 71--                        **
C               **  UPDATE INTERNAL DATAPLOT TABLES  **
C               ***************************************
C
      ISTEPN='71'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'FLUC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IH='STAT'
      IH2='MINI'
      VALUE0=STATMN
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGG3,IERROR)
C
      IH='STAT'
      IH2='MAXI'
      VALUE0=STATMX
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGG3,IERROR)
C
C               *****************
C               **  STEP 9--   **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'FLUC')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFLUC--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGG2,IBUGG3,IBUGQ,ISUBRO
 9012   FORMAT('IBUGG2,IBUGG3,IBUGQ,ISUBRO = ',A4,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IFOUND,IERROR
 9013   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NPLOTV,NPLOTP,NLOCAL,ICASPL,IAND1,IAND2
 9014   FORMAT('NPLOTV,NPLOTP,NLOCAL,ICASPL,IAND1,IAND2 = ',
     1         I8,I8,I8,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9041)NLOCAL
 9041   FORMAT('NLOCAL = ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(NLOCAL.GE.1 .AND. ICASE.EQ.'VARI')THEN
          DO9042I=1,NLOCAL
            WRITE(ICOUT,9043)I,Y1(I),Y2(I)
 9043       FORMAT('I,Y1(I),Y2(I) = ',I8,2E15.7)
            CALL DPWRST('XXX','BUG ')
 9042     CONTINUE
        ENDIF
        WRITE(ICOUT,9051)NPLOTP
 9051   FORMAT('NPLOTP = ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(NPLOTP.GE.1)THEN
          DO9052I=1,NPLOTP
            WRITE(ICOUT,9053)I,Y(I),X(I),D(I),DCOLOR(I)
 9053       FORMAT('I,Y(I),X(I),D(I),DCOLOR(I),',I8,4F12.5)
            CALL DPWRST('XXX','BUG ')
 9052     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFLU2(Y1,Y2,Y3,TAG1,TAG2,TAG3,TAG4,TAG5,TAG6,N,
     1                  YLEVEL,NLEVEL,
     1                  NUMV2,ICASCT,ICASE,ICASPL,
     1                  XIDTEM,XIDTE2,XIDTE3,XIDTE4,XIDTE5,XIDTE6,
     1                  TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,
     1                  TEMP6,TEMP7,TEMP8,TEMP9,TMP10,
     1                  TMP11,TMP12,TMP13,TMP14,
     1                  ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1                  DTEMP1,DTEMP2,DTEMP3,
     1                  XMAT,MAXLEV,NROW,NCOL,
     1                  ISEED,IQUASE,IBINME,IBI2ME,ICTAMV,
     1                  PSTAMV,PCTAMV,ALPHA,
     1                  IXVAR,IX2VAR,IYVAR,
     1                  NCRTV,MAXOBV,PFLUFL,PFLUCL,IFLUWI,IFLUUN,
     1                  IFLUCD,IFLUBP,
     1                  IFLUDI,IFLUSO,IFLUSR,IFLUSC,IFLUBD,
     1                  STATMN,STATMX,
     1                  XACLOW,XACUPP,
     1                  Y,X,D,DCOLOR,
     1                  NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
C              THAT WILL DEFINE AN FLUCUATION PLOT
C     REFERENCE--UNWIN, THEUS, AND HOFMANN (2006), "GRAPHICS OF
C                LARGE DATA SETS: VISUALIZING A MILLION", SPRINGER.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2008/5
C     ORIGINAL VERSION--MAY       2008.
C     UPDATED         --JANUARY   2009. SUPPORT CASE FOR TABLE INPUT
C                                       (THIS IS RESTRICTED TO THE
C                                       CASE WITH TWO CLASSICATION
C                                       VARIABLES--INPUT TABLE CONTAINS
C                                       PREVIOUSLY CROSS-TABULATED
C                                       VALUES)
C     UPDATED         --AUGUST    2009. CORRECT ORDERING FOR XVAL AND
C                                       YVAL
C     UPDATED         --SEPTEMBER 2009. ADD "UNCERTAINTY INTERVALS"
C                                       FOR BINOMIAL PROPORTION AND
C                                       MEAN/MEDIAN CONFIDENCE LIMITS
C     UPDATED         --MARCH     2010. FOR "UNCERTAINTY INTERVALS",
C                                       ADD PLOT POINTS FOR POINT
C                                       ESTIMATE
C     UPDATED         --JUNE      2010. SUPPORT FOR "SORTED" OPTION FOR
C                                       THE TWO GROUP-ID VARIABLE CASE
C     UPDATED         --JULY      2011. FOR "UNCERTAINTY INTERVAL" CASE,
C                                       SUPPORT "LOWER/UPPER" OPTIONS
C     UPDATED         --APRIL     2013. SUPPORT FOR "BAR DIRECTION"
C     UPDATED         --NOVEMBER  2017. DIFFERENCE OF MEAN AND
C                                       DIFFERENCE OF BINOMIAL
C                                       PROPORTIONS SUPPORT UNCERTAINTY
C                                       INTERVALS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASCT
      CHARACTER*4 IXVAR
      CHARACTER*4 IX2VAR
      CHARACTER*4 IYVAR
      CHARACTER*4 IQUASE
      CHARACTER*4 IBINME
      CHARACTER*4 IBI2ME
      CHARACTER*4 ICTAMV
      CHARACTER*4 IFLUUN
      CHARACTER*4 IFLUWI
      CHARACTER*4 IFLUCD
      CHARACTER*4 IFLUBP
      CHARACTER*4 IFLUDI
      CHARACTER*4 IFLUSO
      CHARACTER*4 IFLUSR
      CHARACTER*4 IFLUSC
      CHARACTER*4 IFLUBD
      CHARACTER*4 ICASE
      CHARACTER*4 ICASPL
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION Y3(*)
      DIMENSION TAG1(*)
      DIMENSION TAG2(*)
      DIMENSION TAG3(*)
      DIMENSION TAG4(*)
      DIMENSION TAG5(*)
      DIMENSION TAG6(*)
C
      DIMENSION YLEVEL(*)
C
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
      DIMENSION XIDTE3(*)
      DIMENSION XIDTE4(*)
      DIMENSION XIDTE5(*)
      DIMENSION XIDTE6(*)
C
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
      DIMENSION TEMP4(*)
      DIMENSION TEMP5(*)
      DIMENSION TEMP6(*)
      DIMENSION TEMP7(*)
      DIMENSION TEMP8(*)
      DIMENSION TEMP9(*)
      DIMENSION TMP10(*)
      DIMENSION TMP11(*)
      DIMENSION TMP12(*)
      DIMENSION TMP13(*)
      DIMENSION TMP14(*)
C
      DIMENSION XACLOW(*)
      DIMENSION XACUPP(*)
C
      DIMENSION ITEMP1(*)
      DIMENSION ITEMP2(*)
      DIMENSION ITEMP3(*)
      DIMENSION ITEMP4(*)
      DIMENSION ITEMP5(*)
      DIMENSION ITEMP6(*)
C
      DOUBLE PRECISION DTEMP1(*)
      DOUBLE PRECISION DTEMP2(*)
      DOUBLE PRECISION DTEMP3(*)
C
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION D(*)
      DIMENSION DCOLOR(*)
C
      DIMENSION XMAT(MAXLEV,MAXLEV)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPFL'
      ISUBN2='U2  '
      IERROR='NO'
C
      I2=0
      INDEX=0
      ILEVEL=0
C
      AN=0.0
      YUPPER=0.0
      YLOWER=0.0
C
      ANUMS1=0.0
      ANUMS2=0.0
      ANUMS3=0.0
      ANUMS4=0.0
      ANUMS5=0.0
      ANUMS6=0.0
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(N.LT.2 .AND. ICASE.EQ.'VARI')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
   31   FORMAT('***** ERROR IN FLUCUATION PLOT--')
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
      ELSEIF(ICASE.EQ.'TABL' .AND. NROW.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,42)
   42   FORMAT('      FOR THE MATRIX CASE, THE NUMBER OF ROWS IS ',
     1         'NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,43)NROW
   43   FORMAT('      THE NUMBER OF ROWS = ',I8)
        CALL DPWRST('XXX','BUG ')
      ELSEIF(ICASE.EQ.'TABL' .AND. NCOL.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)
   47   FORMAT('      FOR THE MATRIX CASE, THE NUMBER OF COLUMNS IS ',
     1         'NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)NCOL
   48   FORMAT('      THE NUMBER OF COLUMNS = ',I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
CCCCC DO NOT TREAT FOLLOWING AS AN ERROR.
CCCCC PRINT A WARNING, BUT CONTINUE TO PROCESS.
C
CCCCC  IF(IYVAR.EQ.'ON')THEN
CCCCC    HOLD=Y(1)
CCCCC    DO60I=1,N
CCCCC      IF(Y(I).NE.HOLD)GOTO69
CCC60    CONTINUE
CCCCC    WRITE(ICOUT,999)
CCCCC    CALL DPWRST('XXX','BUG ')
CCCCC    WRITE(ICOUT,31)
CCCCC    CALL DPWRST('XXX','BUG ')
CCCCC    WRITE(ICOUT,62)
CCC62    FORMAT('      ALL RESPONSE VARIABLE ELEMENTS')
CCCCC    CALL DPWRST('XXX','BUG ')
CCCCC    WRITE(ICOUT,63)HOLD
CCC63    FORMAT('      ARE IDENTICALLY EQUAL TO ',G15.7)
CCCCC    CALL DPWRST('XXX','BUG ')
CCCCC    WRITE(ICOUT,999)
CCCCC    CALL DPWRST('XXX','BUG ')
CCC69   CONTINUE
CCCCC  ENDIF
C
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'FLU2')THEN
        WRITE(ICOUT,70)
   70   FORMAT('AT THE BEGINNING OF DPFLU2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)N,ICASCT,ICASE,NUMV2,NCRTV,NLEVEL
   71   FORMAT('N,ICASCT,ICASE,NUMV2,NCRTV,NLEVEL = ',
     1         I8,2X,A4,2X,A4,3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,74)PFLUFL,PFLUCL,IFLUWI
   74   FORMAT('PFLUFL,PFLUCL,IFLUWI = ',2G15.7,A4)
        CALL DPWRST('XXX','BUG ')
        DO72I=1,MIN(N,100)
          WRITE(ICOUT,73)I,Y1(I),Y2(I),TAG1(I),TAG2(I),TAG3(I),
     1                   TAG4(I),TAG5(I),TAG6(I)
   73     FORMAT('I,Y(I),Y2(I),TAG1-6(I) = ',I8,9F10.3)
          CALL DPWRST('XXX','BUG ')
   72   CONTINUE
        IF(NLEVEL.GT.0)THEN
          DO82I=1,MIN(NLEVEL,100)
            WRITE(ICOUT,83)I,YLEVEL(I)
   83       FORMAT('I,YLEVEL(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
   82     CONTINUE
        ENDIF
      ENDIF
C
C               ******************************************************
C               **  STEP 1--                                        **
C               **  DETERMINE THE NUMBER OF DISTINCT VALUES         **
C               **  FOR THE GROUP VARIABLES (TAG1, TAG2)            **
C               **  IF ALL VALUES ARE DISTINCT, THEN THIS           **
C               **  IMPLIES WE HAVE THE NO REPLICATION CASE         **
C               **  WHICH IS AN ERROR CONDITION FOR A PLOT.         **
C               ******************************************************
C
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'FLU2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASE.EQ.'TABL')GOTO990
C
      IF(IFLUCD.EQ.'ON')THEN
        CALL CODE(TAG1,N,IWRITE,TEMP1,TEMP2,MAXOBV,IBUGG3,IERROR)
        DO910I=1,N
          TAG1(I)=TEMP1(I)
  910   CONTINUE
      ENDIF
      CALL DISTIN(TAG1,N,IWRITE,XIDTEM,NUMSE1,IBUGG3,IERROR)
      CALL SORT(XIDTEM,NUMSE1,XIDTEM)
C
      IF(NCRTV.GE.2)THEN
        IF(IFLUCD.EQ.'ON')THEN
          CALL CODE(TAG2,N,IWRITE,TEMP1,TEMP2,MAXOBV,IBUGG3,IERROR)
          DO920I=1,N
            TAG2(I)=TEMP1(I)
  920     CONTINUE
        ENDIF
        CALL DISTIN(TAG2,N,IWRITE,XIDTE2,NUMSE2,IBUGG3,IERROR)
        CALL SORT(XIDTE2,NUMSE2,XIDTE2)
      ENDIF
C
      IF(NCRTV.GE.3)THEN
        IF(IFLUCD.EQ.'ON')THEN
          CALL CODE(TAG3,N,IWRITE,TEMP1,TEMP2,MAXOBV,IBUGG3,IERROR)
          DO930I=1,N
            TAG3(I)=TEMP1(I)
  930     CONTINUE
        ENDIF
        CALL DISTIN(TAG3,N,IWRITE,XIDTE3,NUMSE3,IBUGG3,IERROR)
        CALL SORT(XIDTE3,NUMSE3,XIDTE3)
      ELSE
        NUMSE3=0
      ENDIF
      IF(NCRTV.GE.4)THEN
        IF(IFLUCD.EQ.'ON')THEN
          CALL CODE(TAG4,N,IWRITE,TEMP1,TEMP2,MAXOBV,IBUGG3,IERROR)
          DO940I=1,N
            TAG4(I)=TEMP1(I)
  940     CONTINUE
        ENDIF
        CALL DISTIN(TAG4,N,IWRITE,XIDTE4,NUMSE4,IBUGG3,IERROR)
        CALL SORT(XIDTE4,NUMSE4,XIDTE4)
      ELSE
        NUMSE4=0
      ENDIF
      IF(NCRTV.GE.5)THEN
        IF(IFLUCD.EQ.'ON')THEN
          CALL CODE(TAG5,N,IWRITE,TEMP1,TEMP2,MAXOBV,IBUGG3,IERROR)
          DO950I=1,N
            TAG5(I)=TEMP1(I)
  950     CONTINUE
        ENDIF
        CALL DISTIN(TAG5,N,IWRITE,XIDTE5,NUMSE5,IBUGG3,IERROR)
        CALL SORT(XIDTE5,NUMSE5,XIDTE5)
      ELSE
        NUMSE5=0
      ENDIF
      IF(NCRTV.GE.6)THEN
        IF(IFLUCD.EQ.'ON')THEN
          CALL CODE(TAG6,N,IWRITE,TEMP1,TEMP2,MAXOBV,IBUGG3,IERROR)
          DO960I=1,N
            TAG6(I)=TEMP1(I)
  960     CONTINUE
        ENDIF
        CALL DISTIN(TAG6,N,IWRITE,XIDTE6,NUMSE6,IBUGG3,IERROR)
        CALL SORT(XIDTE6,NUMSE6,XIDTE6)
      ELSE
        NUMSE6=0
      ENDIF
C
      IF(NUMSE1.LT.1 .OR. NUMSE1.GT.N)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        ITEMP=1
        WRITE(ICOUT,111)ITEMP,NUMSE1
  111   FORMAT('      THE NUMBER OF SETS FOR THE GROUP ',I1,
     1         ' VARIABLE, ',I8,',')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      IS EITHER LESS THAN ONE OR GREATER THAN THE ',
     1         'NUMBER')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)N
  115   FORMAT('      OF OBSERVATIONS, ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(NCRTV.GE.2 .AND. (NUMSE2.LT.1 .OR. NUMSE2.GT.N))THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        ITEMP=2
        WRITE(ICOUT,111)ITEMP,NUMSE2
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)N
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(NCRTV.GE.3 .AND. (NUMSE3.LT.1 .OR. NUMSE3.GT.N))THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        ITEMP=3
        WRITE(ICOUT,111)ITEMP,NUMSE3
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)N
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(NCRTV.GE.4 .AND. (NUMSE4.LT.1 .OR. NUMSE4.GT.N))THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        ITEMP=4
        WRITE(ICOUT,111)ITEMP,NUMSE4
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)N
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(NCRTV.GE.5 .AND. (NUMSE5.LT.1 .OR. NUMSE5.GT.N))THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        ITEMP=5
        WRITE(ICOUT,111)ITEMP,NUMSE5
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)N
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(NCRTV.GE.6 .AND. (NUMSE6.LT.1 .OR. NUMSE6.GT.N))THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        ITEMP=6
        WRITE(ICOUT,111)ITEMP,NUMSE6
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)N
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      AN=N
      ANUMS1=NUMSE1
      ANUMS2=NUMSE2
      ANUMS3=NUMSE3
      ANUMS4=NUMSE4
      ANUMS5=NUMSE5
      ANUMS6=NUMSE6
C
  990 CONTINUE
C               ***********************************************
C               **  STEP 5--                                 **
C               **  COMPUTE THE VARIOUS CROSS-TAB STATISTICS **
C               ***********************************************
C
      ISTEPN='5.1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'CRT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IWRITE='OFF'
C
      AINC=0.4
C
      IF(NCRTV.EQ.1)THEN
        CALL DPFLU0(Y1,Y2,Y3,TAG1,N,
     1              NUMV2,ICASCT,
     1              XIDTEM,
     1              NUMSE1,
     1              TEMP1,TEMP2,TMP14,TEMP3,TEMP4,TEMP5,
     1              ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1              DTEMP1,DTEMP2,DTEMP3,
     1              ISEED,IQUASE,IBINME,IBI2ME,ALPHA,
     1              IXVAR,IX2VAR,IYVAR,
     1              STATMN,STATMX,TMP13,NMAX,XACLOW,XACUPP,
     1              MAXOBV,PFLUFL,PFLUCL,IFLUUN,
     1              ICTAMV,PCTAMV,PSTAMV,
     1              TEMP6,TEMP7,N2,ISUBRO,IBUGG3,IERROR)
C
CCCCC   NOW GENERATE THE PLOT COORDINATES.  DEFINE TWO RECTANGLES
CCCCC   FOR EACH POINT:
CCCCC
CCCCC     1) A FULL RECTANGLE THAT WILL BE SHADED IN A LIGHTER
CCCCC        SHADE.
CCCCC
CCCCC     2) A RECTANGLE THAT IS PROPORTIONAL TO THE VALUE OF
CCCCC        THE STATISTIC THAT WILL BE SHADED IN A DARKER
CCCCC        COLOR.
C
        IFLAGU=0
        IF((ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1      ICASCT.EQ.'DMEA' .OR. ICASCT.EQ.'DBPR' .OR.
     1      ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT') .AND.
     1      IFLUUN.NE.'OFF')THEN
          IFLAGU=1
        ENDIF
C
        ICNT=0
        ICNT2=0
        AFACT=1.0
        DENOM=STATMX-STATMN
        DO1001I=1,N2
          IF(IFLUWI.EQ.'PROP')THEN
            AFACT=TMP13(I)/REAL(NMAX)
          ENDIF
          IF(ICASPL.EQ.'FLCP')THEN
            STATT=TEMP6(I)
            IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'BRAT')THEN
              IF(IFLUBP.EQ.'LOWE')STATT=XACLOW(I)
              IF(IFLUBP.EQ.'UPPE')STATT=XACUPP(I)
            ENDIF
            IF(STATT.LT.YLEVEL(1))THEN
              ILEVEL=1
            ELSEIF(STATT.GE.YLEVEL(NLEVEL))THEN
              ILEVEL=NLEVEL+1
            ELSE
              DO1005J=2,NLEVEL
                IF(STATT.GE.YLEVEL(J-1) .AND. STATT.LT.YLEVEL(J))THEN
                  ILEVEL=J
                ENDIF
 1005         CONTINUE
            ENDIF
            ACOL=REAL(ILEVEL+1)
          ELSE
            ACOL=2.0
          ENDIF
C
          XVAL=TEMP7(I)
          YVAL=TEMP6(I)
          CALL DPFLUW(Y,X,D,DCOLOR,TEMP6,XACLOW,XACUPP,
     1                XCOOR1,XCOOR2,XCOOR3,XCOOR4,XCOOR5,
     1                YCOOR1,YCOOR2,YCOOR3,YCOOR4,YCOOR5,
     1                ICNT,ICNT2,ACOL,IFLAGU,
     1                I,XVAL,YVAL,AFACT,AINC,STATMN,DENOM,
     1                IFLUBD)
C
 1001   CONTINUE
C
        NPLOTP=ICNT
        NPLOTV=2
C
C       WHEN THERE ARE EXACTLY TWO CROSS-TABULATION VARIABLES, THEN
C       SUPPORT A "SORT" OPTION.  FIRST NEED TO OBTAIN ROW AND COLUMN
C       VALUES FOR THE STATISTICS.  FROM THESE, CREATE "INDEX" VARIABLES.
C
      ELSEIF(NCRTV.EQ.2)THEN
C
C       SORT THE ROWS.  FOR THIS APPLICATION, NEED A RANK.  SINCE THE
C       RANK WILL SERVE AS AN ARRAY INDEX, NEED TO CHECK FOR TIES.
C
        IF(IFLUSO.EQ.'ON' .OR. IFLUSO.EQ.'ROW')THEN
          CALL DPFLU0(Y1,Y2,Y3,TAG1,N,
     1                NUMV2,ICASCT,
     1                XIDTEM,
     1                NUMSE1,
     1                TEMP1,TEMP2,TMP14,TEMP3,TEMP4,TEMP5,
     1                ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1                DTEMP1,DTEMP2,DTEMP3,
     1                ISEED,IQUASE,IBINME,IBI2ME,ALPHA,
     1                IXVAR,IX2VAR,IYVAR,
     1                STATMN,STATMX,TMP13,NMAX,XACLOW,XACUPP,
     1                MAXOBV,PFLUFL,PFLUCL,IFLUUN,
     1                ICTAMV,PCTAMV,PSTAMV,
     1                TEMP9,TEMP7,N2,ISUBRO,IBUGG3,IERROR)
          CALL RANKI(TEMP9,NUMSE1,IWRITE,XIDTE3,TEMP7,ITEMP1,MAXOBV,
     1               IBUGG3,IERROR)
          CALL DISTIN(XIDTE3,NUMSE1,IWRITE,TEMP7,NTEMP,IBUGG3,IERROR)
          IF(NTEMP.NE.NUMSE1)THEN
            DO1006II=1,NUMSE1
              XIDTE3(II)=XIDTEM(II)
 1006       CONTINUE
          ENDIF
          IF(IFLUSR.EQ.'DESC')THEN
            DO4006I=1,N
              IRANK=INT(XIDTE3(I)+0.1)
              IRANK2=NUMSE1 - IRANK + 1
              XIDTE3(I)=REAL(IRANK2)
 4006       CONTINUE
          ENDIF
        ELSE
          IF(IFLUSR.EQ.'DESC')THEN
            DO4007II=1,NUMSE1
              IVAL=NUMSE1 - II + 1
              XIDTE3(II)=XIDTEM(IVAL)
 4007       CONTINUE
          ELSE
            DO1007II=1,NUMSE1
              XIDTE3(II)=XIDTEM(II)
 1007       CONTINUE
          ENDIF
        ENDIF
C
C       SORT THE COLUMNS
C
        IF(IFLUSO.EQ.'ON' .OR. IFLUSO.EQ.'COLU')THEN
          CALL DPFLU0(Y1,Y2,Y3,TAG1,N,
     1                NUMV2,ICASCT,
     1                XIDTEM,
     1                NUMSE1,
     1                TEMP1,TEMP2,TMP14,TEMP3,TEMP4,TEMP5,
     1                ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1                DTEMP1,DTEMP2,DTEMP3,
     1                ISEED,IQUASE,IBINME,IBI2ME,ALPHA,
     1                IXVAR,IX2VAR,IYVAR,
     1                STATMN,STATMX,TMP13,NMAX,XACLOW,XACUPP,
     1                MAXOBV,PFLUFL,PFLUCL,IFLUUN,
     1                ICTAMV,PCTAMV,PSTAMV,
     1                TMP10,TEMP7,N2,ISUBRO,IBUGG3,IERROR)
          CALL RANKI(TMP10,NUMSE2,IWRITE,XIDTE4,TEMP7,ITEMP1,MAXOBV,
     1              IBUGG3,IERROR)
          CALL DISTIN(XIDTE4,NUMSE2,IWRITE,TEMP7,NTEMP,IBUGG3,IERROR)
          IF(NTEMP.NE.NUMSE2)THEN
            DO1008II=1,NUMSE2
              XIDTE4(II)=XIDTE2(II)
 1008       CONTINUE
          ENDIF
          IF(IFLUSC.EQ.'DESC')THEN
            DO4008I=1,N
              IRANK=INT(XIDTE4(I)+0.1)
              IRANK2=NUMSE2 - IRANK + 1
              XIDTE4(I)=REAL(IRANK2)
 4008       CONTINUE
          ENDIF
        ELSE
          IF(IFLUSR.EQ.'DESC')THEN
            DO5008II=1,NUMSE2
              IVAL=NUMSE2 - II + 1
              XIDTE4(II)=XIDTE2(IVAL)
 5008       CONTINUE
          ELSE
             DO1009II=1,NUMSE2
              XIDTE4(II)=XIDTE2(II)
 1009       CONTINUE
          ENDIF
        ENDIF
C
        CALL DPFLU3(Y1,Y2,Y3,TAG1,TAG2,N,
     1              NUMV2,ICASCT,
     1              XIDTEM,XIDTE2,
     1              NUMSE1,NUMSE2,
     1              TEMP1,TEMP2,TMP14,TEMP3,TEMP4,TEMP5,
     1              ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1              DTEMP1,DTEMP2,DTEMP3,
     1              ICASE,XMAT,MAXLEV,NROW,NCOL,
     1              ISEED,IQUASE,IBINME,IBI2ME,ALPHA,
     1              IXVAR,IX2VAR,IYVAR,
     1              STATMN,STATMX,TMP13,NMAX,XACLOW,XACUPP,
     1              MAXOBV,PFLUFL,PFLUCL,IFLUUN,
     1              ICTAMV,PCTAMV,PSTAMV,
     1              TEMP6,TEMP7,TEMP8,N2,ISUBRO,IBUGG3,IERROR)
C
CCCCC   NOW GENERATE THE PLOT COORDINATES.  DEFINE TWO RECTANGLES
CCCCC   FOR EACH POINT:
CCCCC
CCCCC     1) A FULL RECTANGLE THAT WILL BE SHADED IN A LIGHTER
CCCCC        SHADE.
CCCCC
CCCCC     2) A RECTANGLE THAT IS PROPORTIONAL TO THE VALUE OF
CCCCC        THE STATISTIC THAT WILL BE SHADED IN A DARKER
CCCCC        COLOR.
CCCCC
CCCCC     FOR THE BINOMIAL PROPORTION, MEAN CONFIDENCE LIMT, AND
CCCCC     MEDIAN CONFIDENCE LIMIT, OPTIONALLY ADD UNCERTAINTY
CCCCC     RECTANGLES: ONE WILL BE FROM STATISTIC VALUE TO LOWER
CCCCC     INTERVAL WHILE THE OTHER WILL BE FROM STAISTIC TO
CCCCC     UPPER INTERVAL.
CCCCC
CCCCC     4/2010: IF "CONTOUR" OPTION IS SPECIFIED, THEN ADJUST
CCCCC             COLOR OR SMALLER BOX BASED ON LEVEL OF STATISTIC.
CCCCC
CCCCC     4/2013: BAR CAN BE DRAWN EITHER VERTICALLY (THE
CCCCC             DEFAULT) OR HORIZONTALLY.
C
        IFLAGU=0
        IF((ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1      ICASCT.EQ.'DMEA' .OR. ICASCT.EQ.'DBPR' .OR.
     1      ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT') .AND.
     1      IFLUUN.NE.'OFF')THEN
          IFLAGU=1
        ENDIF
C
        ICNT=0
        ICNT2=0
        AFACT=1.0
        DENOM=STATMX-STATMN
        DO1010I=1,N2
          IF(IFLUWI.EQ.'PROP' .AND. ICASE.NE.'TABL')THEN
            AFACT=TMP13(I)/REAL(NMAX)
          ENDIF
          IF(ICASPL.EQ.'FLCP')THEN
            STATT=TEMP6(I)
            IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'BRAT')THEN
              IF(IFLUBP.EQ.'LOWE')STATT=XACLOW(I)
              IF(IFLUBP.EQ.'UPPE')STATT=XACUPP(I)
            ENDIF
            IF(STATT.LT.YLEVEL(1))THEN
              ILEVEL=1
            ELSEIF(STATT.GE.YLEVEL(NLEVEL))THEN
              ILEVEL=NLEVEL+1
            ELSE
              DO1015J=2,NLEVEL
                IF(STATT.GE.YLEVEL(J-1) .AND. STATT.LT.YLEVEL(J))THEN
                  ILEVEL=J
                ENDIF
 1015         CONTINUE
            ENDIF
            ACOL=REAL(ILEVEL+1)
          ELSE
            ACOL=2.0
          ENDIF
CCCCC     XVAL=TEMP8(I)
CCCCC     YVAL=TEMP7(I)
          IF(IFLUSO.EQ.'OFF' .AND. IFLUCD.EQ.'OFF')THEN
            IF(IFLUDI.EQ.'X')THEN
              XVAL=TEMP7(I)
              YVAL=TEMP8(I)
            ELSE
              XVAL=TEMP8(I)
              YVAL=TEMP7(I)
            ENDIF
          ELSE
            IF(IFLUDI.EQ.'X')THEN
              INDEXX=INT(TEMP7(I)+0.1)
              INDEXY=INT(TEMP8(I)+0.1)
              XVAL=XIDTE3(INDEXX)
              YVAL=XIDTE4(INDEXY)
            ELSE
              INDEXX=INT(TEMP8(I)+0.1)
              INDEXY=INT(TEMP7(I)+0.1)
              XVAL=XIDTE4(INDEXX)
              YVAL=XIDTE3(INDEXY)
            ENDIF
          ENDIF
C
          IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'FLU2')THEN
            WRITE(ICOUT,1070)I,INDEXX,INDEXY
 1070       FORMAT('AT DPFLU3: I,INDEXX,INDEXY = ',3I8)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1071)XVAL,YVAL,AFACT,AINC
 1071       FORMAT('XVAL,YVAL,AFACT,AINC = ',4G15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1072)XIDTE3(I),XIDTE4(I)
 1072       FORMAT('XIDTE3(I),XIDTE4(I) = ',2G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          CALL DPFLUW(Y,X,D,DCOLOR,TEMP6,XACLOW,XACUPP,
     1                XCOOR1,XCOOR2,XCOOR3,XCOOR4,XCOOR5,
     1                YCOOR1,YCOOR2,YCOOR3,YCOOR4,YCOOR5,
     1                ICNT,ICNT2,ACOL,IFLAGU,
     1                I,XVAL,YVAL,AFACT,AINC,STATMN,DENOM,
     1                IFLUBD)
C
 1010   CONTINUE
C
        NPLOTP=ICNT
        NPLOTV=2
C
      ELSEIF(NCRTV.EQ.3)THEN
        CALL DPFLU4(Y1,Y2,Y3,TAG1,TAG2,TAG3,N,
     1              NUMV2,ICASCT,
     1              XIDTEM,XIDTE2,XIDTE3,
     1              NUMSE1,NUMSE2,NUMSE3,
     1              TEMP1,TEMP2,TMP14,TEMP3,TEMP4,TEMP5,
     1              ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1              DTEMP1,DTEMP2,DTEMP3,
     1              ISEED,IQUASE,IBINME,IBI2ME,ALPHA,
     1              IXVAR,IX2VAR,IYVAR,
     1              STATMN,STATMX,TMP13,NMAX,XACLOW,XACUPP,
     1              MAXOBV,PFLUFL,PFLUCL,IFLUUN,
     1              ICTAMV,PCTAMV,PSTAMV,
     1              TEMP6,TEMP7,TEMP8,TEMP9,N2,ISUBRO,IBUGG3,IERROR)
C
CCCCC   NOW GENERATE THE PLOT COORDINATES.  DEFINE TWO RECTANGLES
CCCCC   FOR EACH POINT:
CCCCC
CCCCC     1) A FULL RECTANGLE THAT WILL BE SHADED IN A LIGHTER
CCCCC        SHADE.
CCCCC
CCCCC     2) A RECTANGLE THAT IS PROPORTIONAL TO THE VALUE OF
CCCCC        THE STATISTIC THAT WILL BE SHADED IN A DARKER
CCCCC        COLOR.
C
        IFLAGU=0
        IF((ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1      ICASCT.EQ.'DMEA' .OR. ICASCT.EQ.'DBPR' .OR.
     1      ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT') .AND.
     1      IFLUUN.NE.'OFF')THEN
          IFLAGU=1
        ENDIF
C
        ICNT=0
        ICNT2=0
        AFACT=1.0
        DENOM=STATMX-STATMN
        DO1020I=1,N2
          IF(IFLUWI.EQ.'PROP')THEN
            AFACT=TMP13(I)/REAL(NMAX)
          ENDIF
          IF(ICASPL.EQ.'FLCP')THEN
            STATT=TEMP6(I)
            IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'BRAT')THEN
              IF(IFLUBP.EQ.'LOWE')STATT=XACLOW(I)
              IF(IFLUBP.EQ.'UPPE')STATT=XACUPP(I)
            ENDIF
            IF(STATT.LT.YLEVEL(1))THEN
              ILEVEL=1
            ELSEIF(STATT.GE.YLEVEL(NLEVEL))THEN
              ILEVEL=NLEVEL+1
            ELSE
              DO1025J=2,NLEVEL
                IF(STATT.GE.YLEVEL(J-1) .AND. STATT.LT.YLEVEL(J))THEN
                  ILEVEL=J
                ENDIF
 1025         CONTINUE
            ENDIF
            ACOL=REAL(ILEVEL+1)
          ELSE
            ACOL=2.0
          ENDIF
          XVAL=TEMP8(I)
CCCCC     YVAL=ANUMS3*(TEMP7(I) - 1.0) + TEMP9(I)
          YVAL=ANUMS1*(TEMP9(I) - 1.0) + TEMP7(I)
C
          CALL DPFLUW(Y,X,D,DCOLOR,TEMP6,XACLOW,XACUPP,
     1                XCOOR1,XCOOR2,XCOOR3,XCOOR4,XCOOR5,
     1                YCOOR1,YCOOR2,YCOOR3,YCOOR4,YCOOR5,
     1                ICNT,ICNT2,ACOL,IFLAGU,
     1                I,XVAL,YVAL,AFACT,AINC,STATMN,DENOM,
     1                IFLUBD)
C
 1020   CONTINUE
C
        NPLOTP=ICNT
        NPLOTV=2
C
      ELSEIF(NCRTV.EQ.4)THEN
        CALL DPFLU5(Y1,Y2,Y3,TAG1,TAG2,TAG3,TAG4,N,
     1              NUMV2,ICASCT,
     1              XIDTEM,XIDTE2,XIDTE3,XIDTE4,
     1              NUMSE1,NUMSE2,NUMSE3,NUMSE4,
     1              TEMP1,TEMP2,TMP14,TEMP3,TEMP4,TEMP5,
     1              ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1              DTEMP1,DTEMP2,DTEMP3,
     1              ISEED,IQUASE,IBINME,IBI2ME,ALPHA,
     1              IXVAR,IX2VAR,IYVAR,
     1              STATMN,STATMX,TMP13,NMAX,XACLOW,XACUPP,
     1              MAXOBV,PFLUFL,PFLUCL,IFLUUN,
     1              ICTAMV,PCTAMV,PSTAMV,
     1              TEMP6,TEMP7,TEMP8,TEMP9,TMP10,N2,
     1              ISUBRO,IBUGG3,IERROR)
C
CCCCC   NOW GENERATE THE PLOT COORDINATES.  DEFINE TWO RECTANGLES
CCCCC   FOR EACH POINT:
CCCCC
CCCCC     1) A FULL RECTANGLE THAT WILL BE SHADED IN A LIGHTER
CCCCC        SHADE.
CCCCC
CCCCC     2) A RECTANGLE THAT IS PROPORTIONAL TO THE VALUE OF
CCCCC        THE STATISTIC THAT WILL BE SHADED IN A DARKER
CCCCC        COLOR.
C
        IFLAGU=0
        IF((ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1      ICASCT.EQ.'DMEA' .OR. ICASCT.EQ.'DBPR' .OR.
     1      ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT') .AND.
     1      IFLUUN.NE.'OFF')THEN
          IFLAGU=1
        ENDIF
C
        ICNT=0
        ICNT2=0
        AFACT=1.0
        DENOM=STATMX-STATMN
        DO1030I=1,N2
          IF(IFLUWI.EQ.'PROP')THEN
            AFACT=TMP13(I)/REAL(NMAX)
          ENDIF
          IF(ICASPL.EQ.'FLCP')THEN
            STATT=TEMP6(I)
            IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'BRAT')THEN
              IF(IFLUBP.EQ.'LOWE')STATT=XACLOW(I)
              IF(IFLUBP.EQ.'UPPE')STATT=XACUPP(I)
            ENDIF
            IF(STATT.LT.YLEVEL(1))THEN
              ILEVEL=1
            ELSEIF(STATT.GE.YLEVEL(NLEVEL))THEN
              ILEVEL=NLEVEL+1
            ELSE
              DO1035J=2,NLEVEL
                IF(STATT.GE.YLEVEL(J-1) .AND. STATT.LT.YLEVEL(J))THEN
                  ILEVEL=J
                ENDIF
 1035         CONTINUE
            ENDIF
            ACOL=REAL(ILEVEL+1)
          ELSE
            ACOL=2.0
          ENDIF
CCCCC     XVAL=ANUMS4*(TEMP8(I) - 1.0) + TMP10(I)
CCCCC     YVAL=ANUMS3*(TEMP7(I) - 1.0) + TEMP9(I)
          XVAL=ANUMS2*(TMP10(I) - 1.0) + TEMP8(I)
          YVAL=ANUMS1*(TEMP9(I) - 1.0) + TEMP7(I)
C
          CALL DPFLUW(Y,X,D,DCOLOR,TEMP6,XACLOW,XACUPP,
     1                XCOOR1,XCOOR2,XCOOR3,XCOOR4,XCOOR5,
     1                YCOOR1,YCOOR2,YCOOR3,YCOOR4,YCOOR5,
     1                ICNT,ICNT2,ACOL,IFLAGU,
     1                I,XVAL,YVAL,AFACT,AINC,STATMN,DENOM,
     1                IFLUBD)
C
 1030   CONTINUE
C
        NPLOTP=ICNT
        NPLOTV=2
C
      ELSEIF(NCRTV.EQ.5)THEN
        CALL DPFLU6(Y1,Y2,Y3,TAG1,TAG2,TAG3,TAG4,TAG5,N,
     1              NUMV2,ICASCT,
     1              XIDTEM,XIDTE2,XIDTE3,XIDTE4,XIDTE5,
     1              NUMSE1,NUMSE2,NUMSE3,NUMSE4,NUMSE5,
     1              TEMP1,TEMP2,TMP14,TEMP3,TEMP4,TEMP5,
     1              ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1              DTEMP1,DTEMP2,DTEMP3,
     1              ISEED,IQUASE,IBINME,IBI2ME,ALPHA,
     1              IXVAR,IX2VAR,IYVAR,
     1              STATMN,STATMX,TMP13,NMAX,XACLOW,XACUPP,
     1              MAXOBV,PFLUFL,PFLUCL,IFLUUN,
     1              ICTAMV,PCTAMV,PSTAMV,
     1              TEMP6,TEMP7,TEMP8,TEMP9,TMP10,TMP11,N2,
     1              ISUBRO,IBUGG3,IERROR)
C
CCCCC   NOW GENERATE THE PLOT COORDINATES.  DEFINE TWO RECTANGLES
CCCCC   FOR EACH POINT:
CCCCC
CCCCC     1) A FULL RECTANGLE THAT WILL BE SHADED IN A LIGHTER
CCCCC        SHADE.
CCCCC
CCCCC     2) A RECTANGLE THAT IS PROPORTIONAL TO THE VALUE OF
CCCCC        THE STATISTIC THAT WILL BE SHADED IN A DARKER
CCCCC        COLOR.
C
        IFLAGU=0
        IF((ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1      ICASCT.EQ.'DMEA' .OR. ICASCT.EQ.'DBPR' .OR.
     1      ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT') .AND.
     1      IFLUUN.NE.'OFF')THEN
          IFLAGU=1
        ENDIF
C
        ICNT=0
        ICNT2=0
        AFACT=1.0
        DENOM=STATMX-STATMN
        DO1040I=1,N2
          IF(IFLUWI.EQ.'PROP')THEN
            AFACT=TMP13(I)/REAL(NMAX)
          ENDIF
          IF(ICASPL.EQ.'FLCP')THEN
            STATT=TEMP6(I)
            IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'BRAT')THEN
              IF(IFLUBP.EQ.'LOWE')STATT=XACLOW(I)
              IF(IFLUBP.EQ.'UPPE')STATT=XACUPP(I)
            ENDIF
            IF(STATT.LT.YLEVEL(1))THEN
              ILEVEL=1
            ELSEIF(STATT.GE.YLEVEL(NLEVEL))THEN
              ILEVEL=NLEVEL+1
            ELSE
              DO1045J=2,NLEVEL
                IF(STATT.GE.YLEVEL(J-1) .AND. STATT.LT.YLEVEL(J))THEN
                  ILEVEL=J
                ENDIF
 1045         CONTINUE
            ENDIF
            ACOL=REAL(ILEVEL+1)
          ELSE
            ACOL=2.0
          ENDIF
CCCCC     XVAL=ANUMS4*(TEMP8(I) - 1.0) + TMP10(I)
          XVAL=ANUMS2*(TMP10(I) - 1.0) + TEMP8(I)
CCCCC     YVAL=(ANUMS3+ANUMS5)*(TEMP7(I) - 1.0) + 
CCCCC1         ANUMS5*(TEMP9(I) - 1.0) + TMP11(I)
          YVAL=(ANUMS1+ANUMS3)*(TMP11(I) - 1.0) +
     1         ANUMS1*(TEMP9(I) - 1.0) + TEMP7(I)
C
          CALL DPFLUW(Y,X,D,DCOLOR,TEMP6,XACLOW,XACUPP,
     1                XCOOR1,XCOOR2,XCOOR3,XCOOR4,XCOOR5,
     1                YCOOR1,YCOOR2,YCOOR3,YCOOR4,YCOOR5,
     1                ICNT,ICNT2,ACOL,IFLAGU,
     1                I,XVAL,YVAL,AFACT,AINC,STATMN,DENOM,
     1                IFLUBD)
C
 1040   CONTINUE
C
        NPLOTP=ICNT
        NPLOTV=2
C
      ELSEIF(NCRTV.EQ.6)THEN
        CALL DPFLU7(Y1,Y2,Y3,TAG1,TAG2,TAG3,TAG4,TAG5,TAG6,N,
     1              NUMV2,ICASCT,
     1              XIDTEM,XIDTE2,XIDTE3,XIDTE4,XIDTE5,XIDTE6,
     1              NUMSE1,NUMSE2,NUMSE3,NUMSE4,NUMSE5,NUMSE6,
     1              TEMP1,TEMP2,TMP14,TEMP3,TEMP4,TEMP5,
     1              ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1              DTEMP1,DTEMP2,DTEMP3,
     1              ISEED,IQUASE,IBINME,IBI2ME,ALPHA,
     1              IXVAR,IX2VAR,IYVAR,
     1              STATMN,STATMX,TMP13,NMAX,XACLOW,XACUPP,
     1              MAXOBV,PFLUFL,PFLUCL,IFLUUN,
     1              ICTAMV,PCTAMV,PSTAMV,
     1              TEMP6,TEMP7,TEMP8,TEMP9,TMP10,TMP11,TMP12,N2,
     1              ISUBRO,IBUGG3,IERROR)
C
CCCCC   NOW GENERATE THE PLOT COORDINATES.  DEFINE TWO RECTANGLES
CCCCC   FOR EACH POINT:
CCCCC
CCCCC     1) A FULL RECTANGLE THAT WILL BE SHADED IN A LIGHTER
CCCCC        SHADE.
CCCCC
CCCCC     2) A RECTANGLE THAT IS PROPORTIONAL TO THE VALUE OF
CCCCC        THE STATISTIC THAT WILL BE SHADED IN A DARKER
CCCCC        COLOR.
C
        IFLAGU=0
        IF((ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1      ICASCT.EQ.'DMEA' .OR. ICASCT.EQ.'DBPR' .OR.
     1      ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT') .AND.
     1      IFLUUN.NE.'OFF')THEN
          IFLAGU=1
        ENDIF
C
        ICNT=0
        ICNT2=0
        AFACT=1.0
        DENOM=STATMX-STATMN
        DO1050I=1,N2
          IF(IFLUWI.EQ.'PROP')THEN
            AFACT=TMP13(I)/REAL(NMAX)
          ENDIF
          IF(ICASPL.EQ.'FLCP')THEN
            STATT=TEMP6(I)
            IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'BRAT')THEN
              IF(IFLUBP.EQ.'LOWE')STATT=XACLOW(I)
              IF(IFLUBP.EQ.'UPPE')STATT=XACUPP(I)
            ENDIF
            IF(STATT.LT.YLEVEL(1))THEN
              ILEVEL=1
            ELSEIF(STATT.GE.YLEVEL(NLEVEL))THEN
              ILEVEL=NLEVEL+1
            ELSE
              DO1055J=2,NLEVEL
                IF(STATT.GE.YLEVEL(J-1) .AND. STATT.LT.YLEVEL(J))THEN
                  ILEVEL=J
                ENDIF
 1055         CONTINUE
            ENDIF
            ACOL=REAL(ILEVEL+1)
          ELSE
            ACOL=2.0
          ENDIF
CCCCC     XVAL=(ANUMS4+ANUMS6)*(TEMP8(I) - 1.0) + 
CCCCC1         ANUMS5*(TMP10(I) - 1.0) + TMP12(I)
CCCCC     YVAL=(ANUMS3+ANUMS5)*(TEMP7(I) - 1.0) + 
CCCCC1         ANUMS5*(TEMP9(I) - 1.0) + TMP11(I)
          XVAL=(ANUMS2+ANUMS4)*(TMP12(I) - 1.0) +
     1         ANUMS2*(TMP10(I) - 1.0) + TEMP8(I)
          YVAL=(ANUMS1+ANUMS3)*(TMP11(I) - 1.0) +
     1         ANUMS1*(TEMP9(I) - 1.0) + TEMP7(I)
C
          CALL DPFLUW(Y,X,D,DCOLOR,TEMP6,XACLOW,XACUPP,
     1                XCOOR1,XCOOR2,XCOOR3,XCOOR4,XCOOR5,
     1                YCOOR1,YCOOR2,YCOOR3,YCOOR4,YCOOR5,
     1                ICNT,ICNT2,ACOL,IFLAGU,
     1                I,XVAL,YVAL,AFACT,AINC,STATMN,DENOM,
     1                IFLUBD)
C
 1050   CONTINUE
C
        NPLOTP=ICNT
        NPLOTV=2
C
      ENDIF
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'FLU2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFLU2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICASCT,N,NPLOTP,NPLOTV,IERROR
 9012   FORMAT('ICASCT,N,NPLOTP,NPLOTV,IERROR = ',A4,3I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO9035I=1,NPLOTP
          WRITE(ICOUT,9036)I,Y(I),X(I),D(I),DCOLOR(I)
 9036     FORMAT('I,Y(I),X(I),D(I),DCOLOR(I) = ',I8,4G15.7)
          CALL DPWRST('XXX','BUG ')
 9035   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFLU0(Y,Z,Z2,TAG1,N,
     1                  NUMV2,ICASCT,
     1                  XIDTEM,NUMSE1,
     1                  TEMP,TEMPZ,TEMPZ2,XTEMP1,XTEMP2,XTEMP3,
     1                  ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1                  DTEMP1,DTEMP2,DTEMP3,
     1                  ISEED,IQUASE,IBINME,IBI2ME,ALPHA,
     1                  IXVAR,IX2VAR,IYVAR,
     1                  STATMN,STATMX,PSIZE,NMAX,XACLOW,XACUPP,
     1                  MAXOBV,PFLUFL,PFLUCL,IFLUUN,
     1                  ICTAMV,PCTAMV,PSTAMV,
     1                  Y2,X2,N2,ISUBRO,IBUGG3,IERROR)
C
C     PURPOSE--GENERATE A ONE-WAY FLUCUATION PLOT.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     REFERENCE--UNWIN, THEUS, AND HOFMANN (2006), "GRAPHICS OF
C                LARGE DATA SETS: VISUALIZING A MILLION", SPRINGER.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2008/11
C     ORIGINAL VERSION--NOVEMBER  2008.
C     UPDATED         --SEPTEMBER 2009. SUPPORT FOR UNCERTAINTY INTERVALS
C                                       FOR BINOMIAL PROPORTION AND
C                                       MEAN/MEDIAN CONFIDENCE INTERVALS
C     UPDATED         --JANUARY   2010. SUPPORT FOR UNCERTAINTY INTERVALS
C                                       FOR BINOMIAL RATIO
C     UPDATED         --NOVEMBER  2017. DIFFERENCE OF MEAN AND
C                                       DIFFERENCE OF BINOMIAL
C                                       PROPORTIONS SUPPORT UNCERTAINTY
C                                       INTERVALS
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASCT
      CHARACTER*4 IXVAR
      CHARACTER*4 IX2VAR
      CHARACTER*4 IYVAR
      CHARACTER*4 IQUASE
      CHARACTER*4 IBINME
      CHARACTER*4 IBI2ME
      CHARACTER*4 ICTAMV
      CHARACTER*4 IFLUUN
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 IBUGG3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBRO
      CHARACTER*4 IWRITE
      CHARACTER*4 IDIR
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION Z(*)
      DIMENSION Z2(*)
      DIMENSION XIDTEM(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
C
      DIMENSION PSIZE(*)
C
      DIMENSION TAG1(*)
      DIMENSION TEMP(*)
      DIMENSION TEMPZ(*)
      DIMENSION TEMPZ2(*)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
      DIMENSION XTEMP3(*)
C
      DIMENSION XACLOW(*)
      DIMENSION XACUPP(*)
C
      INTEGER ITEMP1(*)
      INTEGER ITEMP2(*)
      INTEGER ITEMP3(*)
      INTEGER ITEMP4(*)
      INTEGER ITEMP5(*)
      INTEGER ITEMP6(*)
C
      DOUBLE PRECISION DTEMP1(*)
      DOUBLE PRECISION DTEMP2(*)
      DOUBLE PRECISION DTEMP3(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPFL'
      ISUBN2='U0  '
C
      I2=0
C
      AN=INT(N+0.01)
C
      ISTEPN='5.1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'FLU0')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPFLU0--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)N,NUMSE1,N2,MAXNXT,NUMSE1,NUMV2,MAXOBV
   52   FORMAT('N,NUMSE1,N2,MAXNXT,NUMSE1,NUMV2,MAXOBV = ',7I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASCT,ICAPSW,ICAPTY,IFORSW,IERROR
   53   FORMAT('ICASCT,ICAPSW,ICAPTY,IFORSW,IERROR = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        DO56I=1,N
          WRITE(ICOUT,57)I,Y(I),Z(I),TAG1(I)
   57     FORMAT('I,Y(I),Z(I),TAG1(I) = ',I8,4G15.7)
          CALL DPWRST('XXX','BUG ')
   56   CONTINUE
      ENDIF
C
C               ***********************************************
C               **  STEP 5--                                 **
C               **  COMPUTE THE VARIOUS CROSS-TAB STATISTICS **
C               ***********************************************
C
      ISTEPN='5.1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'FLU0')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IWRITE='OFF'
C
      STATMN=CPUMAX
      IF(ICASCT.EQ.'NUMB')STATMN=0.0
      STATMX=CPUMIN
      IF(PFLUCL.NE.-9999.0)STATMX=PFLUCL
      IF(PFLUFL.NE.-9999.0)STATMN=PFLUFL
      NMAX=0
      J=0
      NRESP=NUMV2-1
      DO1110ISET1=1,NUMSE1
C
        K=0
        DO1130I=1,N
          IF(XIDTEM(ISET1).EQ.TAG1(I))GOTO1131
          GOTO1130
 1131     CONTINUE
C
          K=K+1
          IF(IYVAR.EQ.'OFF')THEN
            TEMP(K)=0.0
          ELSE
            TEMP(K)=Y(I)
            IF(IXVAR.EQ.'ON')TEMPZ(K)=Z(I)
            IF(IX2VAR.EQ.'ON')TEMPZ2(K)=Z2(I)
          ENDIF
 1130   CONTINUE
        NTEMP=K
C
        NTRIAL=0
        ALOWLM=0.0
        AUPPLM=0.0
        IF(NTEMP.EQ.0)THEN
          IF(ICTAMV.EQ.'ZERO')THEN
            STAT=0.0
            IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1         ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT' .OR.
     1         ICASCT.EQ.'DBPR' .OR. ICASCT.EQ.'DMEA')THEN
              NTRIAL=0
              ALOWLM=0.0
              AUPPLM=0.0
            ENDIF
          ELSEIF(ICTAMV.EQ.'MV  ')THEN
            STAT=PCTAMV
            IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1         ICASCT.EQ.'DMEA' .OR. ICASCT.EQ.'DBPR' .OR.
     1         ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT')THEN
              NTRIAL=0
              ALOWLM=PCTAMV
              AUPPLM=PCTAMV
            ENDIF
          ELSE
            GOTO1110
          ENDIF
        ELSE
          CALL CMPSTA(
     1              TEMP,TEMPZ,TEMPZ2,XTEMP1,XTEMP2,XTEMP3,
     1              MAXNXT,NTEMP,NTEMP,NTEMP,
     1              NRESP,ICASCT,
     1              ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1              DTEMP1,DTEMP2,DTEMP3,
CCCCC1              IQUAME,IQUASE,PSTAMV,
     1              STAT,
     1              ISUBRO,IBUGG3,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
          IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'BRAT')THEN
            PTEMP=STAT
            NTRIAL=NTEMP
            IF(ICASCT.EQ.'BRAT')NTRIAL=ITEMP1(1)
            IF(STAT.EQ.PSTAMV)THEN
              ALOWLM=PSTAMV
              AUPPLM=PSTAMV
            ELSE
              ALPHAT=ALPHA
              IF(ALPHAT.LE.0.5)ALPHAT=1.0 - ALPHA
              IF(IFLUUN.EQ.'LOWE')THEN
                IDIR='LOWE'
                CALL DPAGC1(PTEMP,NTRIAL,ALPHAT,IDIR,IWRITE,
     1                      ALOWLM,IBUGG3,IERROR)
                AUPPLM=STAT
              ELSEIF(IFLUUN.EQ.'UPPE')THEN
                IDIR='UPPE'
                CALL DPAGC1(PTEMP,NTRIAL,ALPHAT,IDIR,IWRITE,
     1                      AUPPLM,IBUGG3,IERROR)
                ALOWLM=STAT
              ELSE
                IF(ICASCT.EQ.'BPRO')THEN
                  CALL DPPRC3(TEMP,NTEMP,ALPHAT,PSTAMV,IBINME,TEMPZ2,
     1                        PTEMP2,ALOWLM,AUPPLM,
     1                        ISUBRO,IBUGG3,IERROR)
                ELSEIF(ICASCT.EQ.'BRAT')THEN
                  CALL DPAGCO(PTEMP,NTRIAL,ALPHAT,IWRITE,
     1                        ALOWLM,AUPPLM,IBUGG3,IERROR)
                ENDIF
              ENDIF
            ENDIF
          ELSEIF(ICASCT.EQ.'MECL')THEN
            XMEAN=STAT
            NTRIAL=NTEMP
            IF(STAT.EQ.PSTAMV)THEN
              ALOWLM=PSTAMV
              AUPPLM=PSTAMV
            ELSE
              CALL SD(TEMP,NTEMP,IWRITE,XSD,IBUGG3,IERROR)
              ALPHAT=ALPHA
              CALL DPMECL(XMEAN,XSD,NTEMP,ALPHAT,IWRITE,
     1                    ALOWLM,AUPPLM,IBUGG3,IERROR)
            ENDIF
          ELSEIF(ICASCT.EQ.'MDCL')THEN
            XMED=STAT
            NTRIAL=NTEMP
            IF(STAT.EQ.PSTAMV)THEN
              ALOWLM=PSTAMV
              AUPPLM=PSTAMV
            ELSE
              XQ=0.5
              CALL QUANSE(XQ,TEMP,NTEMP,IWRITE,XTEMP1,MAXNXT,IQUASE,
     1                    QUASE,IBUGG3,IERROR)
              ALPHAT=ALPHA
              CALL DPMECL(XMED,QUASE,NTEMP,ALPHAT,IWRITE,
     1                    ALOWLM,AUPPLM,IBUGG3,IERROR)
            ENDIF
          ELSEIF(ICASCT.EQ.'DMEA')THEN
            XDIFF=STAT
            NTRIAL=NTEMP
            IF(STAT.EQ.PSTAMV)THEN
              ALOWLM=PSTAMV
              AUPPLM=PSTAMV
            ELSE
              ALPHAT=ALPHA
              ALP=ALPHA
              IF(ALP.LT.0.5)THEN
                ALPHAT=1.0-(ALP/2.0)
              ELSE
                ALP=1.0 - ALPHA
                ALPHAT=1.0-(ALP/2.0)
              ENDIF
              AN=REAL(NTEMP)
              CALL MEAN(TEMP,NTEMP,IWRITE,XMEAN1,IBUGG3,IERROR)
              CALL SD(TEMP,NTEMP,IWRITE,XSD1,IBUGG3,IERROR)
              AVAL1=XSD1**2/AN
              CALL MEAN(TEMPZ,NTEMP,IWRITE,XMEAN2,IBUGG3,IERROR)
              CALL SD(TEMPZ,NTEMP,IWRITE,XSD2,IBUGG3,IERROR)
              AVAL2=XSD2**2/AN
              XSTERR=SQRT(AVAL1 + AVAL2)
              TERM1=(AVAL1 + AVAL2)**2
              TERM2=AVAL1*AVAL1/(AN-1.0) + AVAL2*AVAL2/(AN-1.0)
              V=TERM1/TERM2
              IV=INT(V+0.5)
              CALL TCDF(ALPHAT,REAL(IV),TCV)
              ALOWLM=XDIFF - TCV*XSTERR
              AUPPLM=XDIFF + TCV*XSTERR
            ENDIF
          ELSEIF(ICASCT.EQ.'DBPR')THEN
            IF(STAT.EQ.PSTAMV)THEN
              ALOWLM=PSTAMV
              AUPPLM=PSTAMV
            ELSE
              ALPHAT=ALPHA
              ALP=ALPHA
              IF(ALP.LT.0.5)THEN
                ALPHAT=1.0-(ALP/2.0)
              ELSE
                ALP=1.0 - ALPHA
                ALPHAT=1.0-(ALP/2.0)
              ENDIF
              CALL DPPRC4(TEMP,NTEMP,TEMPZ,NTEMP,ALPHAT,PSTAMV,
     1                    IBI2ME,TEMPZ2,
     1                    XDIFF,ALOWLM,AUPPLM,
     1                    ISUBRO,IBUGG3,IERROR)
            ENDIF
          ENDIF
        ENDIF
C
        J=J+1
        IF(PFLUCL.EQ.-9999.0)THEN
          IF(STAT.GT.STATMX)STATMX=STAT
        ELSE
          IF(STAT.GT.PFLUCL)STAT=PFLUCL
        ENDIF
        IF(PFLUFL.EQ.-9999.0)THEN
          IF(STAT.LT.STATMN)STATMN=STAT
        ELSE
          IF(STAT.LT.PFLUFL)STAT=PFLUFL
        ENDIF
        IF(NTEMP.GT.NMAX)NMAX=NTEMP
        PSIZE(J)=REAL(NTEMP)
C
        Y2(J)=STAT
        X2(J)=XIDTEM(ISET1)
        IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1     ICASCT.EQ.'DMEA' .OR. ICASCT.EQ.'DBPR' .OR.
     1     ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT' .AND.
     1     IFLUUN.NE.'OFF')THEN
          IF(PFLUCL.EQ.-9999.0)THEN
            IF(AUPPLM.GT.STATMX)STATMX=AUPPLM
          ELSE
            IF(AUPPLM.GT.PFLUCL)AUPPLM=PFLUCL
          ENDIF
          IF(PFLUFL.EQ.-9999.0)THEN
            IF(ALOWLM.LT.STATMN)STATMN=ALOWLM
          ELSE
            IF(ALOWLM.LT.PFLUFL)ALOWLM=PFLUFL
          ENDIF
          XACLOW(J)=ALOWLM
          XACUPP(J)=AUPPLM
        ENDIF
C
 1110 CONTINUE
      N2=J
C
      IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'BRAT')THEN
        STATMN=0.0
        STATMX=1.0
      ELSEIF(ICASCT.EQ.'DBPR')THEN
        STATMN=-1.0
        STATMX=1.0
      ELSEIF(ICASCT.EQ.'COUN')THEN
        STATMN=0.0
      ENDIF
C
C               *****************************
C               **   STEP 6--              **
C               **   WRITE OUT THE TABLE   **
C               *****************************
C
      ISTEPN='6'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'FLU0')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               ******************
C               **   STEP 90--  **
C               **   EXIT       **
C               ******************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'FLU0')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFLU0--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICASCT,N,NUMSE1,N2,IERROR
 9012   FORMAT('ICASCT,N,NUMSE1,N2,IERROR = ',A4,3I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NUMV2
 9013   FORMAT('NUMV2 = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO9020I=1,N2
          WRITE(ICOUT,9021)I,Y2(I),X2(I)
 9021     FORMAT('I,Y2(I),X2(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFLU3(Y,Z,Z2,TAG1,TAG2,N,
     1                  NUMV2,ICASCT,
     1                  XIDTEM,XIDTE2,
     1                  NUMSE1,NUMSE2,
     1                  TEMP,TEMPZ,TEMPZ2,XTEMP1,XTEMP2,XTEMP3,
     1                  ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1                  DTEMP1,DTEMP2,DTEMP3,
     1                  ICASE,XMAT,MAXLEV,NROW,NCOL,
     1                  ISEED,IQUASE,IBINME,IBI2ME,ALPHA,
     1                  IXVAR,IX2VAR,IYVAR,
     1                  STATMN,STATMX,PSIZE,NMAX,XACLOW,XACUPP,
     1                  MAXOBV,PFLUFL,PFLUCL,IFLUUN,
     1                  ICTAMV,PCTAMV,PSTAMV,
     1                  Y2,X2,D2,N2,ISUBRO,IBUGG3,IERROR)
C
C     PURPOSE--GENERATE A TWO-WAY FLUCUATION PLOT.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     REFERENCE--UNWIN, THEUS, AND HOFMANN (2006), "GRAPHICS OF
C                LARGE DATA SETS: VISUALIZING A MILLION",
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2008/5
C     ORIGINAL VERSION--MAY       2008.
C     UPDATED         --JANUARY   2009. SUPPORT CASE FOR TABLE INPUT
C                                       (THIS IS RESTRICTED TO THE
C                                       CASE WITH TWO CLASSICATION
C                                       VARIABLES--INPUT TABLE CONTAINS
C                                       PREVIOUSLY CROSS-TABULATED
C                                       VALUES)
C     UPDATED         --SEPTEMBER 2009. UNCERTAINTY INTERVALS FOR
C                                       BINOMIAL PROPORTION,
C                                       MEAN/MEDIAN CONFIDENCE LIMITS
C     UPDATED         --JANUARY   2010. UNCERTAINTY INTERVALS FOR
C                                       BINOMIAL RATIO
C     UPDATED         --NOVEMBER  2017. DIFFERENCE OF MEAN AND
C                                       DIFFERENCE OF BINOMIAL
C                                       PROPORTIONS SUPPORT UNCERTAINTY
C                                       INTERVALS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASCT
      CHARACTER*4 ICASE
      CHARACTER*4 IXVAR
      CHARACTER*4 IX2VAR
      CHARACTER*4 IYVAR
      CHARACTER*4 IQUASE
      CHARACTER*4 IBINME
      CHARACTER*4 IBI2ME
      CHARACTER*4 ICTAMV
      CHARACTER*4 IFLUUN
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 IBUGG3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBRO
      CHARACTER*4 IWRITE
      CHARACTER*4 IDIR
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION Z(*)
      DIMENSION Z2(*)
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
C
      DIMENSION PSIZE(*)
C
      DIMENSION TAG1(*)
      DIMENSION TAG2(*)
      DIMENSION TEMP(*)
      DIMENSION TEMPZ(*)
      DIMENSION TEMPZ2(*)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
      DIMENSION XTEMP3(*)
C
      DIMENSION XACLOW(*)
      DIMENSION XACUPP(*)
C
      INTEGER ITEMP1(*)
      INTEGER ITEMP2(*)
      INTEGER ITEMP3(*)
      INTEGER ITEMP4(*)
      INTEGER ITEMP5(*)
      INTEGER ITEMP6(*)
C
      DOUBLE PRECISION DTEMP1(*)
      DOUBLE PRECISION DTEMP2(*)
      DOUBLE PRECISION DTEMP3(*)
C
      DIMENSION XMAT(MAXLEV,MAXLEV)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPFL'
      ISUBN2='U3  '
C
      IF(ICASE.EQ.'TABL')GOTO2000
      I2=0
      AN=INT(N+0.01)
C
C               ***********************************************
C               **  STEP 5--                                 **
C               **  COMPUTE THE VARIOUS CROSS-TAB STATISTICS **
C               ***********************************************
C
      ISTEPN='5.1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'FLU3')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPFLU3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASCT,ICAPTY,ICAPSW,IFORSW,IERROR
   52   FORMAT('ICASCT,ICAPTY,ICAPSW,IFORSW,IERROR = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)N,NUMSE1,N2,MAXOBV
   53   FORMAT('N,NUMSE1,N2,MAXOBV = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)NUMSE1,NUMSE2,NUMV2
   55   FORMAT('NUMSE1,NUMSE2,NUMV2 = ',4I8)
        CALL DPWRST('XXX','BUG ')
        DO56I=1,N
          WRITE(ICOUT,57)I,Y(I),Z(I),TAG1(I),TAG2(I)
   57     FORMAT('I,Y(I),Z(I),TAG1(I),TAG2(I) = ',I8,4G15.7)
          CALL DPWRST('XXX','BUG ')
   56   CONTINUE
      ENDIF
C
      IWRITE='OFF'
C
      STATMN=CPUMAX
      IF(ICASCT.EQ.'NUMB')STATMN=0.0
      STATMX=CPUMIN
      IF(PFLUCL.NE.-9999.0)STATMX=PFLUCL
      IF(PFLUFL.NE.-9999.0)STATMN=PFLUFL
      NMAX=0
      J=0
      NRESP=NUMV2-2
      DO1110ISET1=1,NUMSE1
        DO1120ISET2=1,NUMSE2
C
          K=0
          DO1130I=1,N
            IF(XIDTEM(ISET1).EQ.TAG1(I).AND.XIDTE2(ISET2).EQ.TAG2(I))
     1        GOTO1131
            GOTO1130
 1131       CONTINUE
C
            K=K+1
            IF(IYVAR.EQ.'OFF')THEN
              TEMP(K)=0.0
            ELSE
              TEMP(K)=Y(I)
              IF(IXVAR.EQ.'ON')TEMPZ(K)=Z(I)
              IF(IX2VAR.EQ.'ON')TEMPZ2(K)=Z2(I)
            ENDIF
 1130     CONTINUE
          NTEMP=K
C
          NTRIAL=0
          ALOWLM=0.0
          AUPPLM=0.0
          IF(NTEMP.EQ.0)THEN
            IF(ICTAMV.EQ.'ZERO')THEN
              STAT=0.0
              IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1           ICASCT.EQ.'DMEA' .OR. ICASCT.EQ.'DBPR' .OR.
     1           ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT')THEN
                NTRIAL=0
                ALOWLM=0.0
                AUPPLM=0.0
              ENDIF
            ELSEIF(ICTAMV.EQ.'MV  ')THEN
              STAT=PCTAMV
              IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1           ICASCT.EQ.'DMEA' .OR. ICASCT.EQ.'DBPR' .OR.
     1           ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT')THEN
                NTRIAL=0
                ALOWLM=PCTAMV
                AUPPLM=PCTAMV
              ENDIF
            ELSE
              GOTO1120
            ENDIF
          ELSE
            CALL CMPSTA(
     1              TEMP,TEMPZ,TEMPZ,XTEMP1,XTEMP2,XTEMP3,
     1              MAXNXT,NTEMP,NTEMP,NTEMP,
     1              NRESP,ICASCT,
     1              ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1              DTEMP1,DTEMP2,DTEMP3,
CCCCC1              IQUAME,IQUASE,PSTAMV,
     1              STAT,
     1              ISUBRO,IBUGG3,IERROR)
            IF(IERROR.EQ.'YES')GOTO9000
            IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'BRAT')THEN
              PTEMP=STAT
              NTRIAL=NTEMP
              IF(ICASCT.EQ.'BRAT')NTRIAL=ITEMP1(1)
              IF(STAT.EQ.PSTAMV)THEN
                ALOWLM=PSTAMV
                AUPPLM=PSTAMV
              ELSE
                ALPHAT=ALPHA
                IF(ALPHAT.LE.0.5)ALPHAT=1.0 - ALPHA
                IF(IFLUUN.EQ.'LOWE')THEN
                  IDIR='LOWE'
                  CALL DPAGC1(PTEMP,NTRIAL,ALPHAT,IDIR,IWRITE,
     1                        ALOWLM,IBUGG3,IERROR)
                  AUPPLM=STAT
                ELSEIF(IFLUUN.EQ.'UPPE')THEN
                  IDIR='UPPE'
                  CALL DPAGC1(PTEMP,NTRIAL,ALPHAT,IDIR,IWRITE,
     1                        AUPPLM,IBUGG3,IERROR)
                  ALOWLM=STAT
                ELSE
                  IF(ICASCT.EQ.'BPRO')THEN
                    CALL DPPRC3(TEMP,NTEMP,ALPHAT,PSTAMV,IBINME,TEMPZ2,
     1                          PTEMP2,ALOWLM,AUPPLM,
     1                          ISUBRO,IBUGG3,IERROR)
                  ELSEIF(ICASCT.EQ.'BRAT')THEN
                    CALL DPAGCO(PTEMP,NTRIAL,ALPHAT,IWRITE,
     1                          ALOWLM,AUPPLM,IBUGG3,IERROR)
                  ENDIF
                ENDIF
              ENDIF
            ELSEIF(ICASCT.EQ.'MECL')THEN
              XMEAN=STAT
              NTRIAL=NTEMP
              IF(STAT.EQ.PSTAMV)THEN
                ALOWLM=PSTAMV
                AUPPLM=PSTAMV
              ELSE
                CALL SD(TEMP,NTEMP,IWRITE,XSD,IBUGG3,IERROR)
                ALPHAT=ALPHA
                CALL DPMECL(XMEAN,XSD,NTEMP,ALPHAT,IWRITE,
     1                      ALOWLM,AUPPLM,IBUGG3,IERROR)
              ENDIF
            ELSEIF(ICASCT.EQ.'MDCL')THEN
              XMED=STAT
              NTRIAL=NTEMP
              IF(STAT.EQ.PSTAMV)THEN
                ALOWLM=PSTAMV
                AUPPLM=PSTAMV
              ELSE
                XQ=0.5
                CALL QUANSE(XQ,TEMP,NTEMP,IWRITE,XTEMP1,MAXNXT,IQUASE,
     1                      QUASE,IBUGG3,IERROR)
                ALPHAT=ALPHA
                CALL DPMECL(XMED,QUASE,NTEMP,ALPHAT,IWRITE,
     1                      ALOWLM,AUPPLM,IBUGG3,IERROR)
              ENDIF
            ELSEIF(ICASCT.EQ.'DMEA')THEN
              XDIFF=STAT
              NTRIAL=NTEMP
              IF(STAT.EQ.PSTAMV)THEN
                ALOWLM=PSTAMV
                AUPPLM=PSTAMV
              ELSE
                ALPHAT=ALPHA
                ALP=ALPHA
                IF(ALP.LT.0.5)THEN
                  ALPHAT=1.0-(ALP/2.0)
                ELSE
                  ALP=1.0 - ALPHA
                  ALPHAT=1.0-(ALP/2.0)
                ENDIF
                AN=REAL(NTEMP)
                CALL MEAN(TEMP,NTEMP,IWRITE,XMEAN1,IBUGG3,IERROR)
                CALL SD(TEMP,NTEMP,IWRITE,XSD1,IBUGG3,IERROR)
                AVAL1=XSD1**2/AN
                CALL MEAN(TEMPZ,NTEMP,IWRITE,XMEAN2,IBUGG3,IERROR)
                CALL SD(TEMPZ,NTEMP,IWRITE,XSD2,IBUGG3,IERROR)
                AVAL2=XSD2**2/AN
                XSTERR=SQRT(AVAL1 + AVAL2)
                TERM1=(AVAL1 + AVAL2)**2
                TERM2=AVAL1*AVAL1/(AN-1.0) + AVAL2*AVAL2/(AN-1.0)
                V=TERM1/TERM2
                IV=INT(V+0.5)
                CALL TCDF(ALPHAT,REAL(IV),TCV)
                ALOWLM=XDIFF - TCV*XSTERR
                AUPPLM=XDIFF + TCV*XSTERR
              ENDIF
            ELSEIF(ICASCT.EQ.'DBPR')THEN
              IF(STAT.EQ.PSTAMV)THEN
                ALOWLM=PSTAMV
                AUPPLM=PSTAMV
              ELSE
                ALPHAT=ALPHA
                ALP=ALPHA
                IF(ALP.LT.0.5)THEN
                  ALPHAT=1.0-(ALP/2.0)
                ELSE
                  ALP=1.0 - ALPHA
                  ALPHAT=1.0-(ALP/2.0)
                ENDIF
                CALL DPPRC4(TEMP,NTEMP,TEMPZ,NTEMP,ALPHAT,PSTAMV,
     1                      IBI2ME,TEMPZ2,
     1                      XDIFF,ALOWLM,AUPPLM,
     1                      ISUBRO,IBUGG3,IERROR)
              ENDIF
            ENDIF
          ENDIF
C
          J=J+1
          IF(PFLUCL.EQ.-9999.0)THEN
            IF(STAT.GT.STATMX)STATMX=STAT
          ELSE
            IF(STAT.GT.PFLUCL)STAT=PFLUCL
          ENDIF
          IF(PFLUFL.EQ.-9999.0)THEN
            IF(STAT.LT.STATMN)STATMN=STAT
          ELSE
            IF(STAT.LT.PFLUFL)STAT=PFLUFL
          ENDIF
          IF(NTEMP.GT.NMAX)NMAX=NTEMP
          PSIZE(J)=REAL(NTEMP)
C
          Y2(J)=STAT
          X2(J)=XIDTEM(ISET1)
          D2(J)=XIDTE2(ISET2)
          IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1       ICASCT.EQ.'DMEA' .OR. ICASCT.EQ.'DBPR' .OR.
     1       ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT' .AND.
     1       IFLUUN.NE.'OFF')THEN
            IF(PFLUCL.EQ.-9999.0)THEN
              IF(AUPPLM.GT.STATMX)STATMX=AUPPLM
            ELSE
              IF(AUPPLM.GT.PFLUCL)AUPPLM=PFLUCL
            ENDIF
            IF(PFLUFL.EQ.-9999.0)THEN
              IF(ALOWLM.LT.STATMN)STATMN=ALOWLM
            ELSE
              IF(ALOWLM.LT.PFLUFL)ALOWLM=PFLUFL
            ENDIF
            XACLOW(J)=ALOWLM
            XACUPP(J)=AUPPLM
          ENDIF
C
 1120   CONTINUE
 1110 CONTINUE
      N2=J
C
      IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'BRAT')THEN
        STATMN=0.0
        STATMX=1.0
      ELSEIF(ICASCT.EQ.'DBPR')THEN
        STATMN=-1.0
        STATMX=1.0
      ELSEIF(ICASCT.EQ.'COUN')THEN
        STATMN=0.0
      ENDIF
C
      GOTO3999
C
 2000 CONTINUE
C
      ISTEPN='6.1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'FLU3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      STATMN=CPUMAX
      IF(ICASCT.EQ.'NUMB')STATMN=0.0
      STATMX=CPUMIN
      IF(PFLUCL.NE.-9999.0)STATMX=PFLUCL
      IF(PFLUFL.NE.-9999.0)STATMN=PFLUFL
C
      ICNT=0
      DO2010I=1,NROW
        DO2020J=1,NCOL
C
C         2016/09: DON'T ASSUME INTEGER.  FOR EXAMPLE, WE MAY WANT A
C                  CORRELATION MATRIX (AS OPPOSSED TO A COUNT).
C
CCCCC     IJUNK=INT(XMAT(I,J)+0.5)
C
CCCCC     STAT=REAL(IJUNK)
          STAT=XMAT(I,J)
          IF(PFLUCL.EQ.-9999.0)THEN
            IF(STAT.GT.STATMX)STATMX=STAT
          ELSE
            IF(STAT.GT.PFLUCL)STAT=PFLUCL
          ENDIF
          IF(PFLUFL.EQ.-9999.0)THEN
            IF(STAT.LT.STATMN)STATMN=STAT
          ELSE
            IF(STAT.LT.PFLUFL)STAT=PFLUFL
          ENDIF
C
          ICNT=ICNT+1
          Y2(ICNT)=STAT
          X2(ICNT)=REAL(I)
          D2(ICNT)=REAL(J)
 2020   CONTINUE
 2010 CONTINUE
      N2=ICNT
C
      GOTO3999
C
 3999 CONTINUE
C
C               *****************************
C               **   STEP 6--              **
C               **   WRITE OUT THE TABLE   **
C               *****************************
C
      ISTEPN='6'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'FLU3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               ******************
C               **   STEP 90--  **
C               **   EXIT       **
C               ******************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'FLU3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFLU3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)STATMN,STATMX,N2
 9016   FORMAT('STATMN,STATMX,N2 = ',2G15.7,I8)
        CALL DPWRST('XXX','BUG ')
        DO9020I=1,N2
          WRITE(ICOUT,9021)I,Y2(I),X2(I),D2(I)
 9021     FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFLU4(Y,Z,Z2,TAG1,TAG2,TAG3,N,
     1                  NUMV2,ICASCT,
     1                  XIDTEM,XIDTE2,XIDTE3,
     1                  NUMSE1,NUMSE2,NUMSE3,
     1                  TEMP,TEMPZ,TEMPZ2,XTEMP1,XTEMP2,XTEMP3,
     1                  ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1                  DTEMP1,DTEMP2,DTEMP3,
     1                  ISEED,IQUASE,IBINME,IBI2ME,ALPHA,
     1                  IXVAR,IX2VAR,IYVAR,
     1                  STATMN,STATMX,PSIZE,NMAX,XACLOW,XACUPP,
     1                  MAXOBV,PFLUFL,PFLUCL,IFLUUN,ICTAMV,
     1                  PCTAMV,PSTAMV,
     1                  Y2,X2,D2,D3,N2,ISUBRO,IBUGG3,IERROR)
C
C     PURPOSE--GENERATE A THREE-WAY FLUCUATION PLOT.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     REFERENCE--UNWIN, THEUS, AND HOFMANN (2006), "GRAPHICS OF
C                LARGE DATA SETS: VISUALIZING A MILLION",
C                SPRINGER.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2008/5
C     ORIGINAL VERSION--MAY       2008.
C     UPDATED         --SEPTEMBER 2009. UNCERTAINTY INTERVALS FOR
C                                       BINOMIAL PROPORTION,
C                                       MEAN/MEDIAN CONFIDENCE LIMITS
C     UPDATED         --JANUARY   2010. SUPPORT FOR UNCERTAINTY INTERVALS
C                                       FOR BINOMIAL RATIO
C     UPDATED         --NOVEMBER  2017. DIFFERENCE OF MEAN AND
C                                       DIFFERENCE OF BINOMIAL
C                                       PROPORTIONS SUPPORT UNCERTAINTY
C                                       INTERVALS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASCT
      CHARACTER*4 IXVAR
      CHARACTER*4 IX2VAR
      CHARACTER*4 IYVAR
      CHARACTER*4 IQUASE
      CHARACTER*4 IBINME
      CHARACTER*4 IBI2ME
      CHARACTER*4 ICTAMV
      CHARACTER*4 IFLUUN
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 IBUGG3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBRO
      CHARACTER*4 IWRITE
      CHARACTER*4 IDIR
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION Z(*)
      DIMENSION Z2(*)
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
      DIMENSION XIDTE3(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
      DIMENSION D3(*)
C
      DIMENSION PSIZE(*)
C
      DIMENSION TAG1(*)
      DIMENSION TAG2(*)
      DIMENSION TAG3(*)
      DIMENSION TEMP(*)
      DIMENSION TEMPZ(*)
      DIMENSION TEMPZ2(*)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
      DIMENSION XTEMP3(*)
C
      DIMENSION XACLOW(*)
      DIMENSION XACUPP(*)
C
      INTEGER ITEMP1(*)
      INTEGER ITEMP2(*)
      INTEGER ITEMP3(*)
      INTEGER ITEMP4(*)
      INTEGER ITEMP5(*)
      INTEGER ITEMP6(*)
C
      DOUBLE PRECISION DTEMP1(*)
      DOUBLE PRECISION DTEMP2(*)
      DOUBLE PRECISION DTEMP3(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPFL'
      ISUBN2='U4  '
C
      I2=0
C
      AN=INT(N+0.01)
C
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'FLU4')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,11)
   11   FORMAT('***** AT THE BEGINNING OF DPFLU5--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,12)ICASCT,ICAPSW,ICAPTY,IFORSW
   12   FORMAT('ICASCT,ICAPSW,ICAPTY,IFORSW,N,NUMV2 = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,13)N,NUMV2,MAXOBV
   13   FORMAT('N,NUMV2,MAXOBV = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,15)NUMSE1,NUMSE2,NUMSE3
   15   FORMAT('NUMSE1,NUMSE2,NUMSE3 = ',3I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***********************************************
C               **  STEP 5--                                 **
C               **  COMPUTE THE VARIOUS CROSS-TAB STATISTICS **
C               ***********************************************
C
      ISTEPN='5.1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'FLU4')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IWRITE='OFF'
C
      STATMN=CPUMAX
      IF(ICASCT.EQ.'NUMB')STATMN=0.0
      STATMX=CPUMIN
      IF(PFLUCL.NE.-9999.0)STATMX=PFLUCL
      IF(PFLUFL.NE.-9999.0)STATMN=PFLUFL
      NMAX=0
      J=0
      NRESP=NUMV2-3
      DO1110ISET1=1,NUMSE1
        DO1120ISET2=1,NUMSE2
        DO1130ISET3=1,NUMSE3
C
          K=0
          DO1180I=1,N
            IF(XIDTEM(ISET1).EQ.TAG1(I).AND.
     1         XIDTE2(ISET2).EQ.TAG2(I).AND.
     1         XIDTE3(ISET3).EQ.TAG3(I))
     1        GOTO1181
            GOTO1180
 1181       CONTINUE
C
            K=K+1
            IF(IYVAR.EQ.'OFF')THEN
              TEMP(K)=0.0
            ELSE
              TEMP(K)=Y(I)
              IF(IXVAR.EQ.'ON')TEMPZ(K)=Z(I)
              IF(IX2VAR.EQ.'ON')TEMPZ2(K)=Z2(I)
            ENDIF
 1180     CONTINUE
          NTEMP=K
C
          NTRIAL=0
          ALOWLM=0.0
          AUPPLM=0.0
          IF(NTEMP.EQ.0)THEN
            IF(ICTAMV.EQ.'ZERO')THEN
              STAT=0.0
              IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1           ICASCT.EQ.'DMEA' .OR. ICASCT.EQ.'DBPR' .OR.
     1           ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT')THEN
                NTRIAL=0
                ALOWLM=0.0
                AUPPLM=0.0
              ENDIF
            ELSEIF(ICTAMV.EQ.'MV  ')THEN
              STAT=PCTAMV
              IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1           ICASCT.EQ.'DMEA' .OR. ICASCT.EQ.'DBPR' .OR.
     1           ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT')THEN
                NTRIAL=0
                ALOWLM=PCTAMV
                AUPPLM=PCTAMV
              ENDIF
            ELSE
              GOTO1130
            ENDIF
          ELSE
            CALL CMPSTA(
     1              TEMP,TEMPZ,TEMPZ2,XTEMP1,XTEMP2,XTEMP3,
     1              MAXNXT,NTEMP,NTEMP,NTEMP,
     1              NRESP,ICASCT,
     1              ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1              DTEMP1,DTEMP2,DTEMP3,
CCCCC1              IQUAME,IQUASE,PSTAMV,
     1              STAT,
     1              ISUBRO,IBUGG3,IERROR)
            IF(IERROR.EQ.'YES')GOTO9000
            IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'BRAT')THEN
              PTEMP=STAT
              NTRIAL=NTEMP
              IF(ICASCT.EQ.'BRAT')NTRIAL=ITEMP1(1)
              IF(STAT.EQ.PSTAMV)THEN
                ALOWLM=PSTAMV
                AUPPLM=PSTAMV
              ELSE
                ALPHAT=ALPHA
                IF(ALPHAT.LE.0.5)ALPHAT=1.0 - ALPHA
                IF(IFLUUN.EQ.'LOWE')THEN
                  IDIR='LOWE'
                  CALL DPAGC1(PTEMP,NTRIAL,ALPHAT,IDIR,IWRITE,
     1                        ALOWLM,IBUGG3,IERROR)
                  AUPPLM=STAT
                ELSEIF(IFLUUN.EQ.'UPPE')THEN
                  IDIR='UPPE'
                  CALL DPAGC1(PTEMP,NTRIAL,ALPHAT,IDIR,IWRITE,
     1                        AUPPLM,IBUGG3,IERROR)
                  ALOWLM=STAT
                ELSE
                  IF(ICASCT.EQ.'BPRO')THEN
                    CALL DPPRC3(TEMP,NTEMP,ALPHAT,PSTAMV,IBINME,TEMPZ2,
     1                          PTEMP2,ALOWLM,AUPPLM,
     1                          ISUBRO,IBUGG3,IERROR)
                  ELSEIF(ICASCT.EQ.'BRAT')THEN
                    CALL DPAGCO(PTEMP,NTRIAL,ALPHAT,IWRITE,
     1                          ALOWLM,AUPPLM,IBUGG3,IERROR)
                  ENDIF
                ENDIF
              ENDIF
            ELSEIF(ICASCT.EQ.'MECL')THEN
              XMEAN=STAT
              NTRIAL=NTEMP
              IF(STAT.EQ.PSTAMV)THEN
                ALOWLM=PSTAMV
                AUPPLM=PSTAMV
              ELSE
                CALL SD(TEMP,NTEMP,IWRITE,XSD,IBUGG3,IERROR)
                ALPHAT=ALPHA
                CALL DPMECL(XMEAN,XSD,NTEMP,ALPHAT,IWRITE,
     1                      ALOWLM,AUPPLM,IBUGG3,IERROR)
              ENDIF
            ELSEIF(ICASCT.EQ.'MDCL')THEN
              XMED=STAT
              NTRIAL=NTEMP
              IF(STAT.EQ.PSTAMV)THEN
                ALOWLM=PSTAMV
                AUPPLM=PSTAMV
              ELSE
                XQ=0.5
                CALL QUANSE(XQ,TEMP,NTEMP,IWRITE,XTEMP1,MAXNXT,IQUASE,
     1                      QUASE,IBUGG3,IERROR)
                ALPHAT=ALPHA
                CALL DPMECL(XMED,QUASE,NTEMP,ALPHAT,IWRITE,
     1                      ALOWLM,AUPPLM,IBUGG3,IERROR)
              ENDIF
            ELSEIF(ICASCT.EQ.'DMEA')THEN
              XDIFF=STAT
              NTRIAL=NTEMP
              IF(STAT.EQ.PSTAMV)THEN
                ALOWLM=PSTAMV
                AUPPLM=PSTAMV
              ELSE
                ALPHAT=ALPHA
                ALP=ALPHA
                IF(ALP.LT.0.5)THEN
                  ALPHAT=1.0-(ALP/2.0)
                ELSE
                  ALP=1.0 - ALPHA
                  ALPHAT=1.0-(ALP/2.0)
                ENDIF
                AN=REAL(NTEMP)
                CALL MEAN(TEMP,NTEMP,IWRITE,XMEAN1,IBUGG3,IERROR)
                CALL SD(TEMP,NTEMP,IWRITE,XSD1,IBUGG3,IERROR)
                AVAL1=XSD1**2/AN
                CALL MEAN(TEMPZ,NTEMP,IWRITE,XMEAN2,IBUGG3,IERROR)
                CALL SD(TEMPZ,NTEMP,IWRITE,XSD2,IBUGG3,IERROR)
                AVAL2=XSD2**2/AN
                XSTERR=SQRT(AVAL1 + AVAL2)
                TERM1=(AVAL1 + AVAL2)**2
                TERM2=AVAL1*AVAL1/(AN-1.0) + AVAL2*AVAL2/(AN-1.0)
                V=TERM1/TERM2
                IV=INT(V+0.5)
                CALL TCDF(ALPHAT,REAL(IV),TCV)
                ALOWLM=XDIFF - TCV*XSTERR
                AUPPLM=XDIFF + TCV*XSTERR
              ENDIF
            ELSEIF(ICASCT.EQ.'DBPR')THEN
              IF(STAT.EQ.PSTAMV)THEN
                ALOWLM=PSTAMV
                AUPPLM=PSTAMV
              ELSE
                ALPHAT=ALPHA
                ALP=ALPHA
                IF(ALP.LT.0.5)THEN
                  ALPHAT=1.0-(ALP/2.0)
                ELSE
                  ALP=1.0 - ALPHA
                  ALPHAT=1.0-(ALP/2.0)
                ENDIF
                CALL DPPRC4(TEMP,NTEMP,TEMPZ,NTEMP,ALPHAT,PSTAMV,
     1                      IBI2ME,TEMPZ2,
     1                      XDIFF,ALOWLM,AUPPLM,
     1                      ISUBRO,IBUGG3,IERROR)
              ENDIF
            ENDIF
          ENDIF
C
          J=J+1
          IF(PFLUCL.EQ.-9999.0)THEN
            IF(STAT.GT.STATMX)STATMX=STAT
          ELSE
            IF(STAT.GT.PFLUCL)STAT=PFLUCL
          ENDIF
          IF(PFLUFL.EQ.-9999.0)THEN
            IF(STAT.LT.STATMN)STATMN=STAT
          ELSE
            IF(STAT.LT.PFLUFL)STAT=PFLUFL
          ENDIF
          IF(NTEMP.GT.NMAX)NMAX=NTEMP
          PSIZE(J)=REAL(NTEMP)
C
          Y2(J)=STAT
          X2(J)=XIDTEM(ISET1)
          D2(J)=XIDTE2(ISET2)
          D3(J)=XIDTE3(ISET3)
          IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1       ICASCT.EQ.'DMEA' .OR. ICASCT.EQ.'DBPR' .OR.
     1       ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT' .AND.
     1       IFLUUN.NE.'OFF')THEN
            IF(PFLUCL.EQ.-9999.0)THEN
              IF(AUPPLM.GT.STATMX)STATMX=AUPPLM
            ELSE
              IF(AUPPLM.GT.PFLUCL)AUPPLM=PFLUCL
            ENDIF
            IF(PFLUFL.EQ.-9999.0)THEN
              IF(ALOWLM.LT.STATMN)STATMN=ALOWLM
            ELSE
              IF(ALOWLM.LT.PFLUFL)ALOWLM=PFLUFL
            ENDIF
            XACLOW(J)=ALOWLM
            XACUPP(J)=AUPPLM
          ENDIF
C
 1130   CONTINUE
 1120   CONTINUE
 1110 CONTINUE
      N2=J
C
      IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'BRAT')THEN
        STATMN=0.0
        STATMX=1.0
      ELSEIF(ICASCT.EQ.'DBPR')THEN
        STATMN=-1.0
        STATMX=1.0
      ELSEIF(ICASCT.EQ.'COUN')THEN
        STATMN=0.0
      ENDIF
C
C               *****************************
C               **   STEP 6--              **
C               **   WRITE OUT THE TABLE   **
C               *****************************
C
      ISTEPN='6'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'FLU4')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               ******************
C               **   STEP 90--  **
C               **   EXIT       **
C               ******************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'FLU4')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFLU4--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICASCT,N,NUMSE1,N2,IERROR
 9012   FORMAT('ICASCT,N,NUMSE1,N2,IERROR = ',A4,3I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NUMV2
 9013   FORMAT('NUMV2 = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)NUMSE1,NUMSE2,NUMSE3,N2
 9015   FORMAT('NUMSE1,NUMSE2,NUMSE3,N2 = ',4I8)
        CALL DPWRST('XXX','BUG ')
        DO9020I=1,N2
          WRITE(ICOUT,9021)I,Y2(I),X2(I),D2(I),D3(I)
 9021     FORMAT('I,Y2(I),X2(I),D2(I),D3(I) = ',I8,4G15.7)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFLU5(Y,Z,Z2,TAG1,TAG2,TAG3,TAG4,N,
     1                  NUMV2,ICASCT,
     1                  XIDTEM,XIDTE2,XIDTE3,XIDTE4,
     1                  NUMSE1,NUMSE2,NUMSE3,NUMSE4,
     1                  TEMP,TEMPZ,TEMPZ2,XTEMP1,XTEMP2,XTEMP3,
     1                  ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1                  DTEMP1,DTEMP2,DTEMP3,
     1                  ISEED,IQUASE,IBINME,IBI2ME,ALPHA,
     1                  IXVAR,IX2VAR,IYVAR,
     1                  STATMN,STATMX,PSIZE,NMAX,XACLOW,XACUPP,
     1                  MAXOBV,PFLUFL,PFLUCL,IFLUUN,
     1                  ICTAMV,PCTAMV,PSTAMV,
     1                  Y2,X2,D2,D3,D4,N2,ISUBRO,IBUGG3,IERROR)
C
C     PURPOSE--GENERATE A FOUR-WAY FLUCUATION PLOT.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     REFERENCE--UNWIN, THEUS, AND HOFMANN (2006), "GRAPHICS OF
C                LARGE DATA SETS: VISUALIZING A MILLION",
C                SPRINGER.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2008/5
C     ORIGINAL VERSION--MAY       2008.
C     UPDATED         --SEPTEMBER 2009. SUPPORT FOR UNCERTAINTY
C                                       INTERVALS FOR BINOMIAL PROPORTION
C                                       AND MEAN/MEDIAN CONFIDENCE
C                                       LIMITS
C     UPDATED         --JANUARY   2010. SUPPORT FOR UNCERTAINTY INTERVALS
C                                       FOR BINOMIAL RATIO
C     UPDATED         --NOVEMBER  2017. DIFFERENCE OF MEAN AND
C                                       DIFFERENCE OF BINOMIAL
C                                       PROPORTIONS SUPPORT UNCERTAINTY
C                                       INTERVALS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASCT
      CHARACTER*4 IXVAR
      CHARACTER*4 IX2VAR
      CHARACTER*4 IYVAR
      CHARACTER*4 IQUASE
      CHARACTER*4 IBINME
      CHARACTER*4 IBI2ME
      CHARACTER*4 ICTAMV
      CHARACTER*4 IFLUUN
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 IBUGG3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBRO
      CHARACTER*4 IWRITE
      CHARACTER*4 IDIR
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION Z(*)
      DIMENSION Z2(*)
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
      DIMENSION XIDTE3(*)
      DIMENSION XIDTE4(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
      DIMENSION D3(*)
      DIMENSION D4(*)
C
      DIMENSION PSIZE(*)
C
      DIMENSION TAG1(*)
      DIMENSION TAG2(*)
      DIMENSION TAG3(*)
      DIMENSION TAG4(*)
      DIMENSION TEMP(*)
      DIMENSION TEMPZ(*)
      DIMENSION TEMPZ2(*)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
      DIMENSION XTEMP3(*)
C
      DIMENSION XACLOW(*)
      DIMENSION XACUPP(*)
C
      INTEGER ITEMP1(*)
      INTEGER ITEMP2(*)
      INTEGER ITEMP3(*)
      INTEGER ITEMP4(*)
      INTEGER ITEMP5(*)
      INTEGER ITEMP6(*)
C
      DOUBLE PRECISION DTEMP1(*)
      DOUBLE PRECISION DTEMP2(*)
      DOUBLE PRECISION DTEMP3(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPFL'
      ISUBN2='U5  '
C
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'FLU5')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,11)
   11   FORMAT('***** AT THE BEGINNING OF DPFLU5--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,12)ICASCT,ICAPSW,ICAPTY,IFORSW
   12   FORMAT('ICASCT,ICAPSW,ICAPTY,IFORSW,N,NUMV2 = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,13)N,NUMV2,MAXOBV
   13   FORMAT('N,NUMV2,MAXOBV = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,15)NUMSE1,NUMSE2,NUMSE3,NUMSE4
   15   FORMAT('NUMSE1,NUMSE2,NUMSE3,NUMSE4 = ',4I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      I2=0
C
      AN=INT(N+0.01)
C
C               ***********************************************
C               **  STEP 5--                                 **
C               **  COMPUTE THE VARIOUS CROSS-TAB STATISTICS **
C               ***********************************************
C
      ISTEPN='5.1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'FLU5')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IWRITE='OFF'
C
      STATMN=CPUMAX
      IF(ICASCT.EQ.'NUMB')STATMN=0.0
      STATMX=CPUMIN
      J=0
      NRESP=NUMV2-4
      IF(PFLUCL.NE.-9999.0)STATMX=PFLUCL
      IF(PFLUFL.NE.-9999.0)STATMN=PFLUFL
      NMAX=0
      DO1110ISET1=1,NUMSE1
        DO1120ISET2=1,NUMSE2
        DO1130ISET3=1,NUMSE3
        DO1140ISET4=1,NUMSE4
C
          IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'FLU5')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1011)
 1011       FORMAT('***** IN THE MIDDLE OF DPFLU5--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1013)ISET1,ISET2,ISET3,ISET4
 1013       FORMAT('ISET1,ISET2,ISET3,ISET4 = ',4I6)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1014)XIDTEM(ISET1),XIDTE2(ISET2)
 1014       FORMAT('XIDTEM(ISET1),XIDTE2(ISET2) = ',2G15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1015)XIDTE3(ISET3),XIDTE4(ISET4)
 1015       FORMAT('XIDTE3(ISET3),XIDTE4(ISET4) = ',2G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          K=0
          DO1180I=1,N
            IF(XIDTEM(ISET1).EQ.TAG1(I).AND.
     1         XIDTE2(ISET2).EQ.TAG2(I).AND.
     1         XIDTE3(ISET3).EQ.TAG3(I).AND.
     1         XIDTE4(ISET4).EQ.TAG4(I))
     1        GOTO1181
            GOTO1180
 1181       CONTINUE
C
            K=K+1
            IF(IYVAR.EQ.'OFF')THEN
              TEMP(K)=0.0
            ELSE
              TEMP(K)=Y(I)
              IF(IXVAR.EQ.'ON')TEMPZ(K)=Z(I)
              IF(IX2VAR.EQ.'ON')TEMPZ2(K)=Z2(I)
            ENDIF
 1180     CONTINUE
          NTEMP=K
C
          IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'FLU5')THEN
            WRITE(ICOUT,1019)NTEMP
 1019       FORMAT('NTEMP = ',I8)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          NTRIAL=0
          ALOWLM=0.0
          AUPPLM=0.0
          IF(NTEMP.EQ.0)THEN
            IF(ICTAMV.EQ.'ZERO')THEN
              STAT=0.0
              IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1           ICASCT.EQ.'DMEA' .OR. ICASCT.EQ.'DBPR' .OR.
     1           ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT')THEN
                NTRIAL=0
                ALOWLM=0.0
                AUPPLM=0.0
              ENDIF
            ELSEIF(ICTAMV.EQ.'MV  ')THEN
              STAT=PCTAMV
              IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1           ICASCT.EQ.'DMEA' .OR. ICASCT.EQ.'DBPR' .OR.
     1           ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT')THEN
                NTRIAL=0
                ALOWLM=PCTAMV
                AUPPLM=PCTAMV
              ENDIF
            ELSE
              GOTO1140
            ENDIF
          ELSE
            CALL CMPSTA(
     1              TEMP,TEMPZ,TEMPZ2,XTEMP1,XTEMP2,XTEMP3,
     1              MAXNXT,NTEMP,NTEMP,NTEMP,
     1              NRESP,ICASCT,
     1              ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1              DTEMP1,DTEMP2,DTEMP3,
CCCCC1              IQUAME,IQUASE,PSTAMV,
     1              STAT,
     1              ISUBRO,IBUGG3,IERROR)
            IF(IERROR.EQ.'YES')GOTO9000
            IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'BRAT')THEN
              PTEMP=STAT
              NTRIAL=NTEMP
              IF(ICASCT.EQ.'BRAT')NTRIAL=ITEMP1(1)
              IF(STAT.EQ.PSTAMV)THEN
                ALOWLM=PSTAMV
                AUPPLM=PSTAMV
              ELSE
                ALPHAT=ALPHA
                IF(ALPHAT.LE.0.5)ALPHAT=1.0 - ALPHA
                IF(IFLUUN.EQ.'LOWE')THEN
                  IDIR='LOWE'
                  CALL DPAGC1(PTEMP,NTRIAL,ALPHAT,IDIR,IWRITE,
     1                        ALOWLM,IBUGG3,IERROR)
                  AUPPLM=STAT
                ELSEIF(IFLUUN.EQ.'UPPE')THEN
                  IDIR='UPPE'
                  CALL DPAGC1(PTEMP,NTRIAL,ALPHAT,IDIR,IWRITE,
     1                        AUPPLM,IBUGG3,IERROR)
                  ALOWLM=STAT
                ELSE
                  IF(ICASCT.EQ.'BPRO')THEN
                    CALL DPPRC3(TEMP,NTEMP,ALPHAT,PSTAMV,IBINME,TEMPZ2,
     1                          PTEMP2,ALOWLM,AUPPLM,
     1                          ISUBRO,IBUGG3,IERROR)
                  ELSEIF(ICASCT.EQ.'BRAT')THEN
                    CALL DPAGCO(PTEMP,NTRIAL,ALPHAT,IWRITE,
     1                          ALOWLM,AUPPLM,IBUGG3,IERROR)
                  ENDIF
                ENDIF
              ENDIF
            ELSEIF(ICASCT.EQ.'MECL')THEN
              XMEAN=STAT
              NTRIAL=NTEMP
              IF(STAT.EQ.PSTAMV)THEN
                ALOWLM=PSTAMV
                AUPPLM=PSTAMV
              ELSE
                CALL SD(TEMP,NTEMP,IWRITE,XSD,IBUGG3,IERROR)
                ALPHAT=ALPHA
                CALL DPMECL(XMEAN,XSD,NTEMP,ALPHAT,IWRITE,
     1                      ALOWLM,AUPPLM,IBUGG3,IERROR)
              ENDIF
            ELSEIF(ICASCT.EQ.'MDCL')THEN
              XMED=STAT
              NTRIAL=NTEMP
              IF(STAT.EQ.PSTAMV)THEN
                ALOWLM=PSTAMV
                AUPPLM=PSTAMV
              ELSE
                XQ=0.5
                CALL QUANSE(XQ,TEMP,NTEMP,IWRITE,XTEMP1,MAXNXT,IQUASE,
     1                      QUASE,IBUGG3,IERROR)
                ALPHAT=ALPHA
                CALL DPMECL(XMED,QUASE,NTEMP,ALPHAT,IWRITE,
     1                      ALOWLM,AUPPLM,IBUGG3,IERROR)
              ENDIF
            ELSEIF(ICASCT.EQ.'DMEA')THEN
              XDIFF=STAT
              NTRIAL=NTEMP
              IF(STAT.EQ.PSTAMV)THEN
                ALOWLM=PSTAMV
                AUPPLM=PSTAMV
              ELSE
                ALPHAT=ALPHA
                ALP=ALPHA
                IF(ALP.LT.0.5)THEN
                  ALPHAT=1.0-(ALP/2.0)
                ELSE
                  ALP=1.0 - ALPHA
                  ALPHAT=1.0-(ALP/2.0)
                ENDIF
                AN=REAL(NTEMP)
                CALL MEAN(TEMP,NTEMP,IWRITE,XMEAN1,IBUGG3,IERROR)
                CALL SD(TEMP,NTEMP,IWRITE,XSD1,IBUGG3,IERROR)
                AVAL1=XSD1**2/AN
                CALL MEAN(TEMPZ,NTEMP,IWRITE,XMEAN2,IBUGG3,IERROR)
                CALL SD(TEMPZ,NTEMP,IWRITE,XSD2,IBUGG3,IERROR)
                AVAL2=XSD2**2/AN
                XSTERR=SQRT(AVAL1 + AVAL2)
                TERM1=(AVAL1 + AVAL2)**2
                TERM2=AVAL1*AVAL1/(AN-1.0) + AVAL2*AVAL2/(AN-1.0)
                V=TERM1/TERM2
                IV=INT(V+0.5)
                CALL TCDF(ALPHAT,REAL(IV),TCV)
                ALOWLM=XDIFF - TCV*XSTERR
                AUPPLM=XDIFF + TCV*XSTERR
              ENDIF
            ELSEIF(ICASCT.EQ.'DBPR')THEN
              IF(STAT.EQ.PSTAMV)THEN
                ALOWLM=PSTAMV
                AUPPLM=PSTAMV
              ELSE
                ALPHAT=ALPHA
                ALP=ALPHA
                IF(ALP.LT.0.5)THEN
                  ALPHAT=1.0-(ALP/2.0)
                ELSE
                  ALP=1.0 - ALPHA
                  ALPHAT=1.0-(ALP/2.0)
                ENDIF
                CALL DPPRC4(TEMP,NTEMP,TEMPZ,NTEMP,ALPHAT,PSTAMV,
     1                      IBI2ME,TEMPZ2,
     1                      XDIFF,ALOWLM,AUPPLM,
     1                      ISUBRO,IBUGG3,IERROR)
              ENDIF
            ENDIF
          ENDIF
C
          J=J+1
          IF(PFLUCL.EQ.-9999.0)THEN
            IF(STAT.GT.STATMX)STATMX=STAT
          ELSE
            IF(STAT.GT.PFLUCL)STAT=PFLUCL
          ENDIF
          IF(PFLUFL.EQ.-9999.0)THEN
            IF(STAT.LT.STATMN)STATMN=STAT
          ELSE
            IF(STAT.LT.PFLUFL)STAT=PFLUFL
          ENDIF
          IF(NTEMP.GT.NMAX)NMAX=NTEMP
          PSIZE(J)=REAL(NTEMP)
C
          Y2(J)=STAT
          X2(J)=XIDTEM(ISET1)
          D2(J)=XIDTE2(ISET2)
          D3(J)=XIDTE3(ISET3)
          D4(J)=XIDTE4(ISET4)
          IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1       ICASCT.EQ.'DMEA' .OR. ICASCT.EQ.'DBPR' .OR.
     1       ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT' .AND.
     1       IFLUUN.NE.'OFF')THEN
            IF(PFLUCL.EQ.-9999.0)THEN
              IF(AUPPLM.GT.STATMX)STATMX=AUPPLM
            ELSE
              IF(AUPPLM.GT.PFLUCL)AUPPLM=PFLUCL
            ENDIF
            IF(PFLUFL.EQ.-9999.0)THEN
              IF(ALOWLM.LT.STATMN)STATMN=ALOWLM
            ELSE
              IF(ALOWLM.LT.PFLUFL)ALOWLM=PFLUFL
            ENDIF
            XACLOW(J)=ALOWLM
            XACUPP(J)=AUPPLM
          ENDIF
C
 1140   CONTINUE
 1130   CONTINUE
 1120   CONTINUE
 1110 CONTINUE
      N2=J
C
      IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'BRAT')THEN
        STATMN=0.0
        STATMX=1.0
      ELSEIF(ICASCT.EQ.'DBPR')THEN
        STATMN=-1.0
        STATMX=1.0
      ELSEIF(ICASCT.EQ.'COUN')THEN
        STATMN=0.0
      ENDIF
C
C               ******************
C               **   STEP 90--  **
C               **   EXIT       **
C               ******************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'FLU5')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFLU5--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICASCT,N,NUMSE1,N2,IERROR
 9012   FORMAT('ICASCT,N,NUMSE1,N2,IERROR = ',A4,3I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NUMV2
 9013   FORMAT('NUMV2 = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)NUMSE1,NUMSE2,NUMSE3,NUMSE4,N2
 9015   FORMAT('NUMSE1,NUMSE2,NUMSE3,NUMSE4,N2 = ',5I8)
        CALL DPWRST('XXX','BUG ')
        DO9020I=1,N2
          WRITE(ICOUT,9021)I,Y2(I),X2(I),D2(I),D3(I),D4(I)
 9021     FORMAT('I,Y2(I),X2(I),D2(I),D3(I),D4(I) = ',I8,5G15.7)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFLU6(Y,Z,Z2,TAG1,TAG2,TAG3,TAG4,TAG5,N,
     1                  NUMV2,ICASCT,
     1                  XIDTEM,XIDTE2,XIDTE3,XIDTE4,XIDTE5,
     1                  NUMSE1,NUMSE2,NUMSE3,NUMSE4,NUMSE5,
     1                  TEMP,TEMPZ,TEMPZ2,XTEMP1,XTEMP2,XTEMP3,
     1                  ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1                  DTEMP1,DTEMP2,DTEMP3,
     1                  ISEED,IQUASE,IBINME,IBI2ME,ALPHA,
     1                  IXVAR,IX2VAR,IYVAR,
     1                  STATMN,STATMX,PSIZE,NMAX,XACLOW,XACUPP,
     1                  MAXOBV,PFLUFL,PFLUCL,IFLUUN,
     1                  ICTAMV,PCTAMV,PSTAMV,
     1                  Y2,X2,D2,D3,D4,D5,N2,
     1                  ISUBRO,IBUGG3,IERROR)
C
C     PURPOSE--GENERATE A FIVE-WAY FLUCUATION PLOT.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     REFERENCE--UNWIN, THEUS, AND HOFMANN (2006), "GRAPHICS OF
C                LARGE DATA SETS: VISUALIZING A MILLION",
C                SPRINGER.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2008/5
C     ORIGINAL VERSION--MAY       2008.
C     UPDATED         --SEPTEMBER 2009. SUPPORT FOR UNCERTAINTY INTERVALS
C                                       FOR BINOMIAL PROPORTION AND
C                                       MEAN/MEDIAN CONFIDENCE INTERVALS
C     UPDATED         --JANUARY   2010. SUPPORT FOR UNCERTAINTY INTERVALS
C                                       FOR BINOMIAL RATIO
C     UPDATED         --NOVEMBER  2017. DIFFERENCE OF MEAN AND
C                                       DIFFERENCE OF BINOMIAL
C                                       PROPORTIONS SUPPORT UNCERTAINTY
C                                       INTERVALS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASCT
      CHARACTER*4 IXVAR
      CHARACTER*4 IX2VAR
      CHARACTER*4 IYVAR
      CHARACTER*4 IQUASE
      CHARACTER*4 IBINME
      CHARACTER*4 IBI2ME
      CHARACTER*4 ICTAMV
      CHARACTER*4 IFLUUN
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IDIR
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION Z(*)
      DIMENSION Z2(*)
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
      DIMENSION XIDTE3(*)
      DIMENSION XIDTE4(*)
      DIMENSION XIDTE5(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
      DIMENSION D3(*)
      DIMENSION D4(*)
      DIMENSION D5(*)
C
      DIMENSION PSIZE(*)
C
      DIMENSION TAG1(*)
      DIMENSION TAG2(*)
      DIMENSION TAG3(*)
      DIMENSION TAG4(*)
      DIMENSION TAG5(*)
      DIMENSION TEMP(*)
      DIMENSION TEMPZ(*)
      DIMENSION TEMPZ2(*)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
      DIMENSION XTEMP3(*)
C
      DIMENSION XACLOW(*)
      DIMENSION XACUPP(*)
C
      INTEGER ITEMP1(*)
      INTEGER ITEMP2(*)
      INTEGER ITEMP3(*)
      INTEGER ITEMP4(*)
      INTEGER ITEMP5(*)
      INTEGER ITEMP6(*)
C
      DOUBLE PRECISION DTEMP1(*)
      DOUBLE PRECISION DTEMP2(*)
      DOUBLE PRECISION DTEMP3(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPFL'
      ISUBN2='U6  '
C
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'FLU6')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,11)
   11   FORMAT('***** AT THE BEGINNING OF DPFLU6--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,12)ICASCT,ICAPSW,ICAPTY,IFORSW
   12   FORMAT('ICASCT,ICAPSW,ICAPTY,IFORSW,N,NUMV2 = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,13)N,NUMV2,MAXOBV
   13   FORMAT('N,NUMV2,MAXOBV = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,15)NUMSE1,NUMSE2,NUMSE3,NUMSE4,NUMSE5
   15   FORMAT('NUMSE1,NUMSE2,NUMSE3,NUMSE4,NUMSE5 = ',5I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      I2=0
C
      AN=INT(N+0.01)
C
C               ***********************************************
C               **  STEP 5--                                 **
C               **  COMPUTE THE VARIOUS CROSS-TAB STATISTICS **
C               ***********************************************
C
      ISTEPN='5.1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'FLU6')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IWRITE='OFF'
C
      STATMN=CPUMAX
      IF(ICASCT.EQ.'NUMB')STATMN=0.0
      STATMX=CPUMIN
      J=0
      NRESP=NUMV2-5
      IF(PFLUCL.NE.-9999.0)STATMX=PFLUCL
      IF(PFLUFL.NE.-9999.0)STATMN=PFLUFL
      NMAX=0
      DO1110ISET1=1,NUMSE1
        DO1120ISET2=1,NUMSE2
        DO1130ISET3=1,NUMSE3
        DO1140ISET4=1,NUMSE4
        DO1150ISET5=1,NUMSE5
C
          IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'FLU6')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1011)
 1011       FORMAT('***** IN THE MIDDLE OF DPFLU6--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1013)ISET1,ISET2,ISET3,ISET4,ISET5
 1013       FORMAT('ISET1,ISET2,ISET3,ISET4,ISET5 = ',5I6)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1014)XIDTEM(ISET1),XIDTE2(ISET2),XIDTE3(ISET3)
 1014       FORMAT('XIDTEM(ISET1),XIDTE2(ISET2),XIDTE3(ISET3) = ',
     1             3G15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1015)XIDTE4(ISET4),XIDTE5(ISET5)
 1015       FORMAT('XIDTE4(ISET4),XIDTE5(ISET5) = ',
     1             2G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          K=0
          DO1180I=1,N
            IF(XIDTEM(ISET1).EQ.TAG1(I).AND.
     1         XIDTE2(ISET2).EQ.TAG2(I).AND.
     1         XIDTE3(ISET3).EQ.TAG3(I).AND.
     1         XIDTE4(ISET4).EQ.TAG4(I).AND.
     1         XIDTE5(ISET5).EQ.TAG5(I))
     1        GOTO1181
            GOTO1180
 1181       CONTINUE
C
            K=K+1
            IF(IYVAR.EQ.'OFF')THEN
              TEMP(K)=0.0
            ELSE
              TEMP(K)=Y(I)
              IF(IXVAR.EQ.'ON')TEMPZ(K)=Z(I)
              IF(IX2VAR.EQ.'ON')TEMPZ2(K)=Z2(I)
            ENDIF
 1180     CONTINUE
          NTEMP=K
C
          IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'FLU6')THEN
            WRITE(ICOUT,1019)NTEMP
 1019       FORMAT('NTEMP = ',I8)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          NTRIAL=0
          ALOWLM=0.0
          AUPPLM=0.0
          IF(NTEMP.EQ.0)THEN
            IF(ICTAMV.EQ.'ZERO')THEN
              STAT=0.0
              IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1           ICASCT.EQ.'DMEA' .OR. ICASCT.EQ.'DBPR' .OR.
     1           ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT')THEN
                NTRIAL=0
                ALOWLM=0.0
                AUPPLM=0.0
              ENDIF
            ELSEIF(ICTAMV.EQ.'MV  ')THEN
              STAT=PCTAMV
              IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1           ICASCT.EQ.'DMEA' .OR. ICASCT.EQ.'DBPR' .OR.
     1           ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT')THEN
                NTRIAL=0
                ALOWLM=PCTAMV
                AUPPLM=PCTAMV
              ENDIF
            ELSE
              GOTO1150
            ENDIF
          ELSE
            CALL CMPSTA(
     1              TEMP,TEMPZ,TEMPZ2,XTEMP1,XTEMP2,XTEMP3,
     1              MAXNXT,NTEMP,NTEMP,NTEMP,
     1              NRESP,ICASCT,
     1              ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1              DTEMP1,DTEMP2,DTEMP3,
CCCCC1              IQUAME,IQUASE,PSTAMV,
     1              STAT,
     1              ISUBRO,IBUGG3,IERROR)
            IF(IERROR.EQ.'YES')GOTO9000
            IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'BRAT')THEN
              PTEMP=STAT
              NTRIAL=NTEMP
              IF(ICASCT.EQ.'BRAT')NTRIAL=ITEMP1(1)
              IF(STAT.EQ.PSTAMV)THEN
                ALOWLM=PSTAMV
                AUPPLM=PSTAMV
              ELSE
                ALPHAT=ALPHA
                IF(ALPHAT.LE.0.5)ALPHAT=1.0 - ALPHA
                IF(IFLUUN.EQ.'LOWE')THEN
                  IDIR='LOWE'
                  CALL DPAGC1(PTEMP,NTRIAL,ALPHAT,IDIR,IWRITE,
     1                        ALOWLM,IBUGG3,IERROR)
                  AUPPLM=STAT
                ELSEIF(IFLUUN.EQ.'UPPE')THEN
                  IDIR='UPPE'
                  CALL DPAGC1(PTEMP,NTRIAL,ALPHAT,IDIR,IWRITE,
     1                        AUPPLM,IBUGG3,IERROR)
                  ALOWLM=STAT
                ELSE
                  IF(ICASCT.EQ.'BPRO')THEN
                    CALL DPPRC3(TEMP,NTEMP,ALPHAT,PSTAMV,IBINME,TEMPZ2,
     1                          PTEMP2,ALOWLM,AUPPLM,
     1                          ISUBRO,IBUGG3,IERROR)
                  ELSEIF(ICASCT.EQ.'BRAT')THEN
                    CALL DPAGCO(PTEMP,NTRIAL,ALPHAT,IWRITE,
     1                          ALOWLM,AUPPLM,IBUGG3,IERROR)
                  ENDIF
                ENDIF
              ENDIF
            ELSEIF(ICASCT.EQ.'MECL')THEN
              XMEAN=STAT
              NTRIAL=NTEMP
              IF(STAT.EQ.PSTAMV)THEN
                ALOWLM=PSTAMV
                AUPPLM=PSTAMV
              ELSE
                CALL SD(TEMP,NTEMP,IWRITE,XSD,IBUGG3,IERROR)
                ALPHAT=ALPHA
                CALL DPMECL(XMEAN,XSD,NTEMP,ALPHAT,IWRITE,
     1                      ALOWLM,AUPPLM,IBUGG3,IERROR)
              ENDIF
            ELSEIF(ICASCT.EQ.'MDCL')THEN
              XMED=STAT
              NTRIAL=NTEMP
              IF(STAT.EQ.PSTAMV)THEN
                ALOWLM=PSTAMV
                AUPPLM=PSTAMV
              ELSE
                XQ=0.5
                CALL QUANSE(XQ,TEMP,NTEMP,IWRITE,XTEMP1,MAXNXT,IQUASE,
     1                      QUASE,IBUGG3,IERROR)
                ALPHAT=ALPHA
                CALL DPMECL(XMED,QUASE,NTEMP,ALPHAT,IWRITE,
     1                      ALOWLM,AUPPLM,IBUGG3,IERROR)
              ENDIF
            ELSEIF(ICASCT.EQ.'DMEA')THEN
              XDIFF=STAT
              NTRIAL=NTEMP
              IF(STAT.EQ.PSTAMV)THEN
                ALOWLM=PSTAMV
                AUPPLM=PSTAMV
              ELSE
                ALPHAT=ALPHA
                ALP=ALPHA
                IF(ALP.LT.0.5)THEN
                  ALPHAT=1.0-(ALP/2.0)
                ELSE
                  ALP=1.0 - ALPHA
                  ALPHAT=1.0-(ALP/2.0)
                ENDIF
                AN=REAL(NTEMP)
                CALL MEAN(TEMP,NTEMP,IWRITE,XMEAN1,IBUGG3,IERROR)
                CALL SD(TEMP,NTEMP,IWRITE,XSD1,IBUGG3,IERROR)
                AVAL1=XSD1**2/AN
                CALL MEAN(TEMPZ,NTEMP,IWRITE,XMEAN2,IBUGG3,IERROR)
                CALL SD(TEMPZ,NTEMP,IWRITE,XSD2,IBUGG3,IERROR)
                AVAL2=XSD2**2/AN
                XSTERR=SQRT(AVAL1 + AVAL2)
                TERM1=(AVAL1 + AVAL2)**2
                TERM2=AVAL1*AVAL1/(AN-1.0) + AVAL2*AVAL2/(AN-1.0)
                V=TERM1/TERM2
                IV=INT(V+0.5)
                CALL TCDF(ALPHAT,REAL(IV),TCV)
                ALOWLM=XDIFF - TCV*XSTERR
                AUPPLM=XDIFF + TCV*XSTERR
              ENDIF
            ELSEIF(ICASCT.EQ.'DBPR')THEN
              IF(STAT.EQ.PSTAMV)THEN
                ALOWLM=PSTAMV
                AUPPLM=PSTAMV
              ELSE
                ALPHAT=ALPHA
                ALP=ALPHA
                IF(ALP.LT.0.5)THEN
                  ALPHAT=1.0-(ALP/2.0)
                ELSE
                  ALP=1.0 - ALPHA
                  ALPHAT=1.0-(ALP/2.0)
                ENDIF
                CALL DPPRC4(TEMP,NTEMP,TEMPZ,NTEMP,ALPHAT,PSTAMV,
     1                      IBI2ME,TEMPZ2,
     1                      XDIFF,ALOWLM,AUPPLM,
     1                      ISUBRO,IBUGG3,IERROR)
              ENDIF
            ENDIF
          ENDIF
C
          J=J+1
          IF(PFLUCL.EQ.-9999.0)THEN
            IF(STAT.GT.STATMX)STATMX=STAT
          ELSE
            IF(STAT.GT.PFLUCL)STAT=PFLUCL
          ENDIF
          IF(PFLUFL.EQ.-9999.0)THEN
            IF(STAT.LT.STATMN)STATMN=STAT
          ELSE
            IF(STAT.LT.PFLUFL)STAT=PFLUFL
          ENDIF
          IF(NTEMP.GT.NMAX)NMAX=NTEMP
          PSIZE(J)=REAL(NTEMP)
C
          Y2(J)=STAT
          X2(J)=XIDTEM(ISET1)
          D2(J)=XIDTE2(ISET2)
          D3(J)=XIDTE3(ISET3)
          D4(J)=XIDTE4(ISET4)
          D5(J)=XIDTE5(ISET5)
          IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1       ICASCT.EQ.'DMEA' .OR. ICASCT.EQ.'DBPR' .OR.
     1       ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT' .AND.
     1       IFLUUN.NE.'OFF')THEN
            IF(PFLUCL.EQ.-9999.0)THEN
              IF(AUPPLM.GT.STATMX)STATMX=AUPPLM
            ELSE
              IF(AUPPLM.GT.PFLUCL)AUPPLM=PFLUCL
            ENDIF
            IF(PFLUFL.EQ.-9999.0)THEN
              IF(ALOWLM.LT.STATMN)STATMN=ALOWLM
            ELSE
              IF(ALOWLM.LT.PFLUFL)ALOWLM=PFLUFL
            ENDIF
            XACLOW(J)=ALOWLM
            XACUPP(J)=AUPPLM
          ENDIF
C
 1150   CONTINUE
 1140   CONTINUE
 1130   CONTINUE
 1120   CONTINUE
 1110 CONTINUE
      N2=J
C
      IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'BRAT')THEN
        STATMN=0.0
        STATMX=1.0
      ELSEIF(ICASCT.EQ.'DBPR')THEN
        STATMN=-1.0
        STATMX=1.0
      ELSEIF(ICASCT.EQ.'COUN')THEN
        STATMN=0.0
      ENDIF
C
C               ******************
C               **   STEP 90--  **
C               **   EXIT       **
C               ******************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'FLU6')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFLU6--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICASCT,N,N2,IERROR
 9012   FORMAT('ICASCT,N,N2,IERROR = ',A4,2I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NUMV2
 9013   FORMAT('NUMV2 = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)NUMSE1,NUMSE2,NUMSE3,NUMSE4,NUMSE5
 9015   FORMAT('NUMSE1,NUMSE2,NUMSE3,NUMSE4,NUMSE5 = ',5I8)
        CALL DPWRST('XXX','BUG ')
        DO9020I=1,N2
          WRITE(ICOUT,9021)I,Y2(I),X2(I),D2(I),D3(I),D4(I),D5(I)
 9021     FORMAT('I,Y2(I),X2(I),D2(I),D3(I),D4(I),D5(I) = ',
     1           I8,6G15.7)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFLU7(Y,Z,Z2,TAG1,TAG2,TAG3,TAG4,TAG5,TAG6,N,
     1                  NUMV2,ICASCT,
     1                  XIDTEM,XIDTE2,XIDTE3,XIDTE4,XIDTE5,XIDTE6,
     1                  NUMSE1,NUMSE2,NUMSE3,NUMSE4,NUMSE5,NUMSE6,
     1                  TEMP,TEMPZ,TEMPZ2,XTEMP1,XTEMP2,XTEMP3,
     1                  ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1                  DTEMP1,DTEMP2,DTEMP3,
     1                  ISEED,IQUASE,IBINME,IBI2ME,ALPHA,
     1                  IXVAR,IX2VAR,IYVAR,
     1                  STATMN,STATMX,PSIZE,NMAX,XACLOW,XACUPP,
     1                  MAXOBV,PFLUFL,PFLUCL,IFLUUN,
     1                  ICTAMV,PCTAMV,PSTAMV,
     1                  Y2,X2,D2,D3,D4,D5,D6,N2,
     1                  ISUBRO,IBUGG3,IERROR)
C
C     PURPOSE--GENERATE A FIVE-WAY FLUCUATION PLOT.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     REFERENCE--UNWIN, THEUS, AND HOFMANN (2006), "GRAPHICS OF
C                LARGE DATA SETS: VISUALIZING A MILLION",
C                SPRINGER.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2008/5
C     ORIGINAL VERSION--MAY       2008.
C     UPDATED         --SEPTEMBER 2009. SUPPORT FOR UNCERTAINTY INTERVALS
C                                       FOR BINOMIAL PROPORTION AND
C                                       MEAN/MEDIAN CONFIDENCE INTERVALS
C     UPDATED         --JANUARY   2010. SUPPORT FOR UNCERTAINTY INTERVALS
C                                       FOR BINOMIAL RATIO
C     UPDATED         --NOVEMBER  2017. DIFFERENCE OF MEAN AND
C                                       DIFFERENCE OF BINOMIAL
C                                       PROPORTIONS SUPPORT UNCERTAINTY
C                                       INTERVALS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASCT
      CHARACTER*4 IXVAR
      CHARACTER*4 IX2VAR
      CHARACTER*4 IYVAR
      CHARACTER*4 IQUASE
      CHARACTER*4 IBINME
      CHARACTER*4 IBI2ME
      CHARACTER*4 ICTAMV
      CHARACTER*4 IFLUUN
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 IBUGG3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBRO
      CHARACTER*4 IWRITE
      CHARACTER*4 IDIR
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION Z(*)
      DIMENSION Z2(*)
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
      DIMENSION XIDTE3(*)
      DIMENSION XIDTE4(*)
      DIMENSION XIDTE5(*)
      DIMENSION XIDTE6(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
      DIMENSION D3(*)
      DIMENSION D4(*)
      DIMENSION D5(*)
      DIMENSION D6(*)
C
      DIMENSION PSIZE(*)
C
      DIMENSION TAG1(*)
      DIMENSION TAG2(*)
      DIMENSION TAG3(*)
      DIMENSION TAG4(*)
      DIMENSION TAG5(*)
      DIMENSION TAG6(*)
      DIMENSION TEMP(*)
      DIMENSION TEMPZ(*)
      DIMENSION TEMPZ2(*)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
      DIMENSION XTEMP3(*)
C
      DIMENSION XACLOW(*)
      DIMENSION XACUPP(*)
C
      INTEGER ITEMP1(*)
      INTEGER ITEMP2(*)
      INTEGER ITEMP3(*)
      INTEGER ITEMP4(*)
      INTEGER ITEMP5(*)
      INTEGER ITEMP6(*)
C
      DOUBLE PRECISION DTEMP1(*)
      DOUBLE PRECISION DTEMP2(*)
      DOUBLE PRECISION DTEMP3(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPFL'
      ISUBN2='U7  '
C
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'FLU6')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,11)
   11   FORMAT('***** AT THE BEGINNING OF DPFLU7--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,12)ICASCT,ICAPSW,ICAPTY,IFORSW
   12   FORMAT('ICASCT,ICAPSW,ICAPTY,IFORSW,N,NUMV2 = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,13)N,NUMV2,MAXOBV
   13   FORMAT('N,NUMV2,MAXOBV = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,15)NUMSE1,NUMSE2,NUMSE3,NUMSE4,NUMSE5,NUMSE6
   15   FORMAT('NUMSE1,NUMSE2,NUMSE3,NUMSE4,NUMSE5,NUMSE6 = ',6I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      I2=0
C
      AN=INT(N+0.01)
C
C               ***********************************************
C               **  STEP 5--                                 **
C               **  COMPUTE THE VARIOUS CROSS-TAB STATISTICS **
C               ***********************************************
C
      ISTEPN='5.1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'FLU6')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IWRITE='OFF'
C
      STATMN=CPUMAX
      IF(ICASCT.EQ.'NUMB')STATMN=0.0
      STATMX=CPUMIN
      J=0
      NRESP=NUMV2-6
      IF(PFLUCL.NE.-9999.0)STATMX=PFLUCL
      IF(PFLUFL.NE.-9999.0)STATMN=PFLUFL
      NMAX=0
      DO1110ISET1=1,NUMSE1
        DO1120ISET2=1,NUMSE2
        DO1130ISET3=1,NUMSE3
        DO1140ISET4=1,NUMSE4
        DO1150ISET5=1,NUMSE5
        DO1160ISET6=1,NUMSE6
C
          IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'FLU6')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1011)
 1011       FORMAT('***** IN THE MIDDLE OF DPFLU7--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1013)ISET1,ISET2,ISET3,ISET4,ISET5,ISET6
 1013       FORMAT('ISET1,ISET2,ISET3,ISET4,ISET5,ISET6 = ',6I6)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1014)XIDTEM(ISET1),XIDTE2(ISET2),XIDTE3(ISET3)
 1014       FORMAT('XIDTEM(ISET1),XIDTE2(ISET2),XIDTE3(ISET3) = ',
     1             3G15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1015)XIDTE4(ISET4),XIDTE5(ISET5),XIDTE6(ISET6)
 1015       FORMAT('XIDTE4(ISET4),XIDTE5(ISET5),XIDTE6(ISET6) = ',
     1             3G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          K=0
          DO1180I=1,N
            IF(XIDTEM(ISET1).EQ.TAG1(I).AND.
     1         XIDTE2(ISET2).EQ.TAG2(I).AND.
     1         XIDTE3(ISET3).EQ.TAG3(I).AND.
     1         XIDTE4(ISET4).EQ.TAG4(I).AND.
     1         XIDTE5(ISET5).EQ.TAG5(I).AND.
     1         XIDTE6(ISET6).EQ.TAG6(I))
     1        GOTO1181
            GOTO1180
 1181       CONTINUE
C
            K=K+1
            IF(IYVAR.EQ.'OFF')THEN
              TEMP(K)=0.0
            ELSE
              TEMP(K)=Y(I)
              IF(IXVAR.EQ.'ON')TEMPZ(K)=Z(I)
              IF(IX2VAR.EQ.'ON')TEMPZ2(K)=Z2(I)
            ENDIF
 1180     CONTINUE
          NTEMP=K
C
          IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'FLU6')THEN
            WRITE(ICOUT,1019)NTEMP
 1019       FORMAT('NTEMP = ',I8)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          NTRIAL=0
          ALOWLM=0.0
          AUPPLM=0.0
          IF(NTEMP.EQ.0)THEN
            IF(ICTAMV.EQ.'ZERO')THEN
              STAT=0.0
              IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1           ICASCT.EQ.'DMEA' .OR. ICASCT.EQ.'DBPR' .OR.
     1           ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT')THEN
                NTRIAL=0
                ALOWLM=0.0
                AUPPLM=0.0
              ENDIF
            ELSEIF(ICTAMV.EQ.'MV  ')THEN
              STAT=PCTAMV
              IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1           ICASCT.EQ.'DMEA' .OR. ICASCT.EQ.'DBPR' .OR.
     1           ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT')THEN
                NTRIAL=0
                ALOWLM=PCTAMV
                AUPPLM=PCTAMV
              ENDIF
            ELSE
              GOTO1160
            ENDIF
          ELSE
            CALL CMPSTA(
     1              TEMP,TEMPZ,TEMPZ2,XTEMP1,XTEMP2,XTEMP3,
     1              MAXNXT,NTEMP,NTEMP,NTEMP2,
     1              NRESP,ICASCT,
     1              ISEED,ITEMP1,ITEMP2,ITEMP3,ITEMP4,ITEMP5,ITEMP6,
     1              DTEMP1,DTEMP2,DTEMP3,
CCCCC1              IQUAME,IQUASE,PSTAMV,
     1              STAT,
     1              ISUBRO,IBUGG3,IERROR)
            IF(IERROR.EQ.'YES')GOTO9000
            IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'BRAT')THEN
              PTEMP=STAT
              NTRIAL=NTEMP
              IF(ICASCT.EQ.'BRAT')NTRIAL=ITEMP1(1)
              IF(STAT.EQ.PSTAMV)THEN
                ALOWLM=PSTAMV
                AUPPLM=PSTAMV
              ELSE
                ALPHAT=ALPHA
                IF(ALPHAT.LE.0.5)ALPHAT=1.0 - ALPHA
                IF(IFLUUN.EQ.'LOWE')THEN
                  IDIR='LOWE'
                  CALL DPAGC1(PTEMP,NTRIAL,ALPHAT,IDIR,IWRITE,
     1                        ALOWLM,IBUGG3,IERROR)
                  AUPPLM=STAT
                ELSEIF(IFLUUN.EQ.'UPPE')THEN
                  IDIR='UPPE'
                  CALL DPAGC1(PTEMP,NTRIAL,ALPHAT,IDIR,IWRITE,
     1                        AUPPLM,IBUGG3,IERROR)
                  ALOWLM=STAT
                ELSE
                  IF(ICASCT.EQ.'BPRO')THEN
                    CALL DPPRC3(TEMP,NTEMP,ALPHAT,PSTAMV,IBINME,TEMPZ2,
     1                          PTEMP2,ALOWLM,AUPPLM,
     1                          ISUBRO,IBUGG3,IERROR)
                  ELSEIF(ICASCT.EQ.'BRAT')THEN
                    CALL DPAGCO(PTEMP,NTRIAL,ALPHAT,IWRITE,
     1                          ALOWLM,AUPPLM,IBUGG3,IERROR)
                  ENDIF
                ENDIF
              ENDIF
            ELSEIF(ICASCT.EQ.'MECL')THEN
              XMEAN=STAT
              NTRIAL=NTEMP
              IF(STAT.EQ.PSTAMV)THEN
                ALOWLM=PSTAMV
                AUPPLM=PSTAMV
              ELSE
                CALL SD(TEMP,NTEMP,IWRITE,XSD,IBUGG3,IERROR)
                ALPHAT=ALPHA
                CALL DPMECL(XMEAN,XSD,NTEMP,ALPHAT,IWRITE,
     1                      ALOWLM,AUPPLM,IBUGG3,IERROR)
              ENDIF
            ELSEIF(ICASCT.EQ.'MDCL')THEN
              XMED=STAT
              NTRIAL=NTEMP
              IF(STAT.EQ.PSTAMV)THEN
                ALOWLM=PSTAMV
                AUPPLM=PSTAMV
              ELSE
                XQ=0.5
                CALL QUANSE(XQ,TEMP,NTEMP,IWRITE,XTEMP1,MAXNXT,IQUASE,
     1                      QUASE,IBUGG3,IERROR)
                ALPHAT=ALPHA
                CALL DPMECL(XMED,QUASE,NTEMP,ALPHAT,IWRITE,
     1                      ALOWLM,AUPPLM,IBUGG3,IERROR)
              ENDIF
            ELSEIF(ICASCT.EQ.'DMEA')THEN
              XDIFF=STAT
              NTRIAL=NTEMP
              IF(STAT.EQ.PSTAMV)THEN
                ALOWLM=PSTAMV
                AUPPLM=PSTAMV
              ELSE
                ALPHAT=ALPHA
                ALP=ALPHA
                IF(ALP.LT.0.5)THEN
                  ALPHAT=1.0-(ALP/2.0)
                ELSE
                  ALP=1.0 - ALPHA
                  ALPHAT=1.0-(ALP/2.0)
                ENDIF
                AN=REAL(NTEMP)
                CALL MEAN(TEMP,NTEMP,IWRITE,XMEAN1,IBUGG3,IERROR)
                CALL SD(TEMP,NTEMP,IWRITE,XSD1,IBUGG3,IERROR)
                AVAL1=XSD1**2/AN
                CALL MEAN(TEMPZ,NTEMP,IWRITE,XMEAN2,IBUGG3,IERROR)
                CALL SD(TEMPZ,NTEMP,IWRITE,XSD2,IBUGG3,IERROR)
                AVAL2=XSD2**2/AN
                XSTERR=SQRT(AVAL1 + AVAL2)
                TERM1=(AVAL1 + AVAL2)**2
                TERM2=AVAL1*AVAL1/(AN-1.0) + AVAL2*AVAL2/(AN-1.0)
                V=TERM1/TERM2
                IV=INT(V+0.5)
                CALL TCDF(ALPHAT,REAL(IV),TCV)
                ALOWLM=XDIFF - TCV*XSTERR
                AUPPLM=XDIFF + TCV*XSTERR
              ENDIF
            ELSEIF(ICASCT.EQ.'DBPR')THEN
              IF(STAT.EQ.PSTAMV)THEN
                ALOWLM=PSTAMV
                AUPPLM=PSTAMV
              ELSE
                ALPHAT=ALPHA
                ALP=ALPHA
                IF(ALP.LT.0.5)THEN
                  ALPHAT=1.0-(ALP/2.0)
                ELSE
                  ALP=1.0 - ALPHA
                  ALPHAT=1.0-(ALP/2.0)
                ENDIF
                CALL DPPRC4(TEMP,NTEMP,TEMPZ,NTEMP,ALPHAT,PSTAMV,
     1                      IBI2ME,TEMPZ2,
     1                      XDIFF,ALOWLM,AUPPLM,
     1                      ISUBRO,IBUGG3,IERROR)
              ENDIF
            ENDIF
          ENDIF
C
          J=J+1
          IF(PFLUCL.EQ.-9999.0)THEN
            IF(STAT.GT.STATMX)STATMX=STAT
          ELSE
            IF(STAT.GT.PFLUCL)STAT=PFLUCL
          ENDIF
          IF(PFLUFL.EQ.-9999.0)THEN
            IF(STAT.LT.STATMN)STATMN=STAT
          ELSE
            IF(STAT.LT.PFLUFL)STAT=PFLUFL
          ENDIF
          IF(NTEMP.GT.NMAX)NMAX=NTEMP
          PSIZE(J)=REAL(NTEMP)
C
          Y2(J)=STAT
          X2(J)=XIDTEM(ISET1)
          D2(J)=XIDTE2(ISET2)
          D3(J)=XIDTE3(ISET3)
          D4(J)=XIDTE4(ISET4)
          D5(J)=XIDTE5(ISET5)
          D6(J)=XIDTE6(ISET6)
          IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'MECL' .OR.
     1       ICASCT.EQ.'DMEA' .OR. ICASCT.EQ.'DBPR' .OR.
     1       ICASCT.EQ.'MDCL' .OR. ICASCT.EQ.'BRAT' .AND.
     1       IFLUUN.NE.'OFF')THEN
            IF(PFLUCL.EQ.-9999.0)THEN
              IF(AUPPLM.GT.STATMX)STATMX=AUPPLM
            ELSE
              IF(AUPPLM.GT.PFLUCL)AUPPLM=PFLUCL
            ENDIF
            IF(PFLUFL.EQ.-9999.0)THEN
              IF(ALOWLM.LT.STATMN)STATMN=ALOWLM
            ELSE
              IF(ALOWLM.LT.PFLUFL)ALOWLM=PFLUFL
            ENDIF
            XACLOW(J)=ALOWLM
            XACUPP(J)=AUPPLM
          ENDIF
C
 1160   CONTINUE
 1150   CONTINUE
 1140   CONTINUE
 1130   CONTINUE
 1120   CONTINUE
 1110 CONTINUE
      N2=J
C
      IF(ICASCT.EQ.'BPRO' .OR. ICASCT.EQ.'BRAT')THEN
        STATMN=0.0
        STATMX=1.0
      ELSEIF(ICASCT.EQ.'DBPR')THEN
        STATMN=-1.0
        STATMX=1.0
      ELSEIF(ICASCT.EQ.'COUN')THEN
        STATMN=0.0
      ENDIF
C
C               ******************
C               **   STEP 90--  **
C               **   EXIT       **
C               ******************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'FLU6')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFLU7--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICASCT,N,N2,IERROR
 9012   FORMAT('ICASCT,N,N2,IERROR = ',A4,2I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NUMV2
 9013   FORMAT('NUMV2 = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)NUMSE1,NUMSE2,NUMSE3,NUMSE4,NUMSE5,NUMSE6
 9015   FORMAT('NUMSE1,NUMSE2,NUMSE3,NUMSE4,NUMSE5,NUMSE6 = ',6I8)
        CALL DPWRST('XXX','BUG ')
        DO9020I=1,N2
          WRITE(ICOUT,9021)I,Y2(I),X2(I),D2(I),D3(I),D4(I),D5(I),D6(I)
 9021     FORMAT('I,Y2(I),X2(I),D2(I),D3(I),D4(I),D5(I),D6(I) = ',
     1           I8,7G15.7)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFLUW(Y,X,D,DCOLOR,TEMP6,XACLOW,XACUPP,
     1                  XCOOR1,XCOOR2,XCOOR3,XCOOR4,XCOOR5,
     1                  YCOOR1,YCOOR2,YCOOR3,YCOOR4,YCOOR5,
     1                  ICNT,ICNT2,ACOL,IFLAGU,
     1                  I,XVAL,YVAL,AFACT,AINC,STATMN,DENOM,
     1                  IFLUBD)
C
C     PURPOSE--UTILITY ROUTINE FOR DPFLU2.  THIS BLOCK OF
C              CODE IS EXECUTED MULTIPLE TIMES, BUT IS WITHIN
C              A LOOP (SO CANNOT EASILY INCLUDE JUST ONCE IN
C              DPFLU2).  SO FOR CONVENIENCE, SETUP AS A SEPARATE
C              SUBROUTINE.
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
C     ORIGINAL VERSION--APRIL     2013. EXTRACT AS DISTINCT SUBROUTINE
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IFLUBD
C
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION D(*)
      DIMENSION DCOLOR(*)
      DIMENSION TEMP6(*)
      DIMENSION XACLOW(*)
      DIMENSION XACUPP(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      D1=AFACT*AINC
      D2=AINC
      D3=(2.0*AINC)*((TEMP6(I) - STATMN)/DENOM)
      D4=2.0*AINC
      D5=(2.0*AINC)*((XACLOW(I) - STATMN)/DENOM)
      D6=(2.0*AINC)*((XACUPP(I) - STATMN)/DENOM)
      IF(IFLUBD.EQ.'HORI')THEN
        XCOOR1=XVAL - D2
        XCOOR2=XVAL + D2
        YCOOR1=YVAL - D1
        YCOOR2=YVAL + D1
        IF(DENOM.NE.0.0)THEN
          XCOOR3=XCOOR1 + D3
        ELSE
          XCOOR3=XCOOR1 + D4
        ENDIF
C
        IF(IFLAGU.EQ.1)THEN
          IF(DENOM.NE.0.0)THEN
            XCOOR4=XCOOR1 + D5
            XCOOR5=XCOOR1 + D6
          ELSE
            XCOOR4=XCOOR3
            XCOOR5=XCOOR3
          ENDIF
        ENDIF
      ELSE
        XCOOR1=XVAL - D1
        XCOOR2=XVAL + D1
        YCOOR1=YVAL - D2
        YCOOR2=YVAL + D2
        IF(DENOM.NE.0.0)THEN
          YCOOR3=YCOOR1 + D3
        ELSE
          YCOOR3=YCOOR1 + D4
        ENDIF
C
        IF(IFLAGU.EQ.1)THEN
          IF(DENOM.NE.0.0)THEN
            YCOOR4=YCOOR1 + D5
            YCOOR5=YCOOR1 + D6
          ELSE
            YCOOR4=YCOOR3
            YCOOR5=YCOOR3
          ENDIF
        ENDIF
      ENDIF
C
      ICNT2=ICNT2+1
      ICNT=ICNT+1
      X(ICNT)=XCOOR1
      Y(ICNT)=YCOOR1
      D(ICNT)=REAL(ICNT2)
      DCOLOR(ICNT)=1.0
C
      ICNT=ICNT+1
      X(ICNT)=XCOOR2
      Y(ICNT)=YCOOR1
      D(ICNT)=REAL(ICNT2)
      DCOLOR(ICNT)=1.0
C
      ICNT=ICNT+1
      X(ICNT)=XCOOR2
      Y(ICNT)=YCOOR2
      D(ICNT)=REAL(ICNT2)
      DCOLOR(ICNT)=1.0
C
      ICNT=ICNT+1
      X(ICNT)=XCOOR1
      Y(ICNT)=YCOOR2
      D(ICNT)=REAL(ICNT2)
      DCOLOR(ICNT)=1.0
C
      ICNT=ICNT+1
      X(ICNT)=XCOOR1
      Y(ICNT)=YCOOR1
      D(ICNT)=REAL(ICNT2)
      DCOLOR(ICNT)=1.0
C
      IF(IFLUBD.EQ.'HORI')THEN
C
         ICNT2=ICNT2+1
         ICNT=ICNT+1
         X(ICNT)=XCOOR1
         Y(ICNT)=YCOOR2
         D(ICNT)=REAL(ICNT2)
         DCOLOR(ICNT)=ACOL
C
         ICNT=ICNT+1
         X(ICNT)=XCOOR1
         Y(ICNT)=YCOOR1
         D(ICNT)=REAL(ICNT2)
         DCOLOR(ICNT)=ACOL
C
         ICNT=ICNT+1
         X(ICNT)=XCOOR3
         Y(ICNT)=YCOOR1
         D(ICNT)=REAL(ICNT2)
         DCOLOR(ICNT)=ACOL
C
         ICNT=ICNT+1
         X(ICNT)=XCOOR3
         Y(ICNT)=YCOOR2
         D(ICNT)=REAL(ICNT2)
         DCOLOR(ICNT)=ACOL
C
         ICNT=ICNT+1
         X(ICNT)=XCOOR1
         Y(ICNT)=YCOOR2
         D(ICNT)=REAL(ICNT2)
         DCOLOR(ICNT)=ACOL
C
         IF(IFLAGU.EQ.1 .AND. XCOOR3.NE.XCOOR4)THEN
           ICNT2=ICNT2+1
           ICNT=ICNT+1
           X(ICNT)=XCOOR4
           Y(ICNT)=YCOOR2
           D(ICNT)=REAL(ICNT2)
           DCOLOR(ICNT)=3.0
C
           ICNT=ICNT+1
           X(ICNT)=XCOOR4
           Y(ICNT)=YCOOR1
           D(ICNT)=REAL(ICNT2)
           DCOLOR(ICNT)=3.0
C
           ICNT=ICNT+1
           X(ICNT)=XCOOR3
           Y(ICNT)=YCOOR1
           D(ICNT)=REAL(ICNT2)
           DCOLOR(ICNT)=3.0
C
           ICNT=ICNT+1
           X(ICNT)=XCOOR3
           Y(ICNT)=YCOOR2
           D(ICNT)=REAL(ICNT2)
           DCOLOR(ICNT)=3.0
C
           ICNT=ICNT+1
           X(ICNT)=XCOOR4
           Y(ICNT)=YCOOR2
           D(ICNT)=REAL(ICNT2)
           DCOLOR(ICNT)=3.0
C
         ENDIF
C
         IF(IFLAGU.GE.1 .AND. XCOOR3.NE.XCOOR5)THEN
           ICNT2=ICNT2+1
           ICNT=ICNT+1
           X(ICNT)=XCOOR3
           Y(ICNT)=YCOOR2
           D(ICNT)=REAL(ICNT2)
           DCOLOR(ICNT)=4.0
C
           ICNT=ICNT+1
           X(ICNT)=XCOOR3
           Y(ICNT)=YCOOR1
           D(ICNT)=REAL(ICNT2)
           DCOLOR(ICNT)=4.0
C
           ICNT=ICNT+1
           X(ICNT)=XCOOR5
           Y(ICNT)=YCOOR1
           D(ICNT)=REAL(ICNT2)
           DCOLOR(ICNT)=4.0
C
           ICNT=ICNT+1
           X(ICNT)=XCOOR5
           Y(ICNT)=YCOOR2
           D(ICNT)=REAL(ICNT2)
           DCOLOR(ICNT)=4.0
C
           ICNT=ICNT+1
           X(ICNT)=XCOOR3
           Y(ICNT)=YCOOR2
           D(ICNT)=REAL(ICNT2)
           DCOLOR(ICNT)=4.0
         ENDIF
C
         IF(IFLAGU.EQ.1)THEN
           ICNT2=ICNT2+1
           ICNT=ICNT+1
           X(ICNT)=XCOOR3
           Y(ICNT)=(YCOOR1 + YCOOR2)/2.0
           D(ICNT)=REAL(ICNT2)
           DCOLOR(ICNT)=5.0
C
           ICNT2=ICNT2+1
           ICNT=ICNT+1
           X(ICNT)=XCOOR3
           Y(ICNT)=YCOOR1
           D(ICNT)=REAL(ICNT2)
           DCOLOR(ICNT)=6.0
C
           ICNT=ICNT+1
           X(ICNT)=XCOOR3
           Y(ICNT)=YCOOR2
           D(ICNT)=REAL(ICNT2)
           DCOLOR(ICNT)=6.0
C
         ENDIF
C
      ELSE
         ICNT2=ICNT2+1
         ICNT=ICNT+1
         X(ICNT)=XCOOR1
         Y(ICNT)=YCOOR1
         D(ICNT)=REAL(ICNT2)
         DCOLOR(ICNT)=ACOL
C
         ICNT=ICNT+1
         X(ICNT)=XCOOR2
         Y(ICNT)=YCOOR1
         D(ICNT)=REAL(ICNT2)
         DCOLOR(ICNT)=ACOL
C
         ICNT=ICNT+1
         X(ICNT)=XCOOR2
         Y(ICNT)=YCOOR3
         D(ICNT)=REAL(ICNT2)
         DCOLOR(ICNT)=ACOL
C
         ICNT=ICNT+1
         X(ICNT)=XCOOR1
         Y(ICNT)=YCOOR3
         D(ICNT)=REAL(ICNT2)
         DCOLOR(ICNT)=ACOL
C
         ICNT=ICNT+1
         X(ICNT)=XCOOR1
         Y(ICNT)=YCOOR1
         D(ICNT)=REAL(ICNT2)
         DCOLOR(ICNT)=ACOL
C
         IF(IFLAGU.EQ.1 .AND. YCOOR3.NE.YCOOR4)THEN
           ICNT2=ICNT2+1
           ICNT=ICNT+1
           X(ICNT)=XCOOR1
           Y(ICNT)=YCOOR4
           D(ICNT)=REAL(ICNT2)
           DCOLOR(ICNT)=3.0
C
           ICNT=ICNT+1
           X(ICNT)=XCOOR2
           Y(ICNT)=YCOOR4
           D(ICNT)=REAL(ICNT2)
           DCOLOR(ICNT)=3.0
C
           ICNT=ICNT+1
           X(ICNT)=XCOOR2
           Y(ICNT)=YCOOR3
           D(ICNT)=REAL(ICNT2)
           DCOLOR(ICNT)=3.0
C
           ICNT=ICNT+1
           X(ICNT)=XCOOR1
           Y(ICNT)=YCOOR3
           D(ICNT)=REAL(ICNT2)
           DCOLOR(ICNT)=3.0
C
           ICNT=ICNT+1
           X(ICNT)=XCOOR1
           Y(ICNT)=YCOOR4
           D(ICNT)=REAL(ICNT2)
           DCOLOR(ICNT)=3.0
C
         ENDIF
C
         IF(IFLAGU.GE.1 .AND. YCOOR3.NE.YCOOR5)THEN
           ICNT2=ICNT2+1
           ICNT=ICNT+1
           X(ICNT)=XCOOR1
           Y(ICNT)=YCOOR3
           D(ICNT)=REAL(ICNT2)
           DCOLOR(ICNT)=4.0
C
           ICNT=ICNT+1
           X(ICNT)=XCOOR2
           Y(ICNT)=YCOOR3
           D(ICNT)=REAL(ICNT2)
           DCOLOR(ICNT)=4.0
C
           ICNT=ICNT+1
           X(ICNT)=XCOOR2
           Y(ICNT)=YCOOR5
           D(ICNT)=REAL(ICNT2)
           DCOLOR(ICNT)=4.0
C
           ICNT=ICNT+1
           X(ICNT)=XCOOR1
           Y(ICNT)=YCOOR5
           D(ICNT)=REAL(ICNT2)
           DCOLOR(ICNT)=4.0
C
           ICNT=ICNT+1
           X(ICNT)=XCOOR1
           Y(ICNT)=YCOOR3
           D(ICNT)=REAL(ICNT2)
           DCOLOR(ICNT)=4.0
         ENDIF
C
         IF(IFLAGU.EQ.1)THEN
           ICNT2=ICNT2+1
           ICNT=ICNT+1
           X(ICNT)=(XCOOR1 + XCOOR2)/2.0
           Y(ICNT)=YCOOR3
           D(ICNT)=REAL(ICNT2)
           DCOLOR(ICNT)=5.0
C
           ICNT2=ICNT2+1
           ICNT=ICNT+1
           X(ICNT)=XCOOR1
           Y(ICNT)=YCOOR3
           D(ICNT)=REAL(ICNT2)
           DCOLOR(ICNT)=6.0
C
           ICNT=ICNT+1
           X(ICNT)=XCOOR2
           Y(ICNT)=YCOOR3
           D(ICNT)=REAL(ICNT2)
           DCOLOR(ICNT)=6.0
C
         ENDIF
C
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFONT(IHARG,NUMARG,
     1IDEFFO,
     1ITEXFO,
     1IBUGD2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE FONT TYPE FOR
C              TITLE, LABEL, AND LEGEND SCRIPT
C              ON A PLOT.
C              THE FONT FOR THE SCRIPT WILL BE PLACED
C              IN THE CHARACTER VARIABLE ITEXFO.
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --NUMARG
C                     --IDEFFO
C                     --IBUGD2
C     OUTPUT ARGUMENTS--ITEXFO
C                     --IERROR ('YES' OR 'NO' )
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
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--SEPTEMBER 1980.
C     UPDATED         --APRIL     1981.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IDEFFO
      CHARACTER*4 ITEXFO
      CHARACTER*4 IBUGD2
      CHARACTER*4 ISUBRO
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
      IF(IBUGD2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPFONT--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IDEFFO
   53 FORMAT('IDEFFO = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)NUMARG
   54 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO55I=1,NUMARG
      WRITE(ICOUT,56)I,IHARG(I)
   56 FORMAT('I,IHARG(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
C
C               ***************************
C               **  TREAT THE FONT CASE  **
C               ***************************
C
      IF(NUMARG.LE.0)GOTO1120
      IF(IHARG(NUMARG).EQ.'ON')GOTO1120
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1120
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1120
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1120
      IF(IHARG(NUMARG).EQ.'?')GOTO8100
      GOTO1140
C
 1120 CONTINUE
      ITEXFO=IDEFFO
      GOTO1180
C
 1140 CONTINUE
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'SIMP')GOTO1141
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'DUPL')GOTO1142
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'TRIP')GOTO1143
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'COMP')GOTO1144
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'TRIP'.AND.
     1IHARG(2).EQ.'ITAL')GOTO1145
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'TRII')GOTO1145
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'COMP'.AND.
     1IHARG(2).EQ.'ITAL')GOTO1146
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'COMI')GOTO1146
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'SIMP'.AND.
     1IHARG(2).EQ.'SCRI')GOTO1147
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'SIMS')GOTO1147
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'COMP'.AND.
     1IHARG(2).EQ.'SCRI')GOTO1148
      IF(NUMARG.EQ.1.AND.IHARG(1).EQ.'COMS')GOTO1148
C
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'TEKT')GOTO1151
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'TEK')GOTO1151
C
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'HEWL'.AND.
     1IHARG(2).EQ.'PACK')GOTO1152
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'HP')GOTO1152
C
      IERROR='YES'
      WRITE(ICOUT,1131)
 1131 FORMAT('***** ERROR IN DPFONT--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1132)
 1132 FORMAT('      ILLEGAL ENTRY FOR FONT ',
     1'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1133)
 1133 FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE ',
     1'PROPER FORM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1134)
 1134 FORMAT('      SUPPOSE THE THE ANALYST WISHES ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1135)
 1135 FORMAT('      TO SET THE FONT TO TRIPLEX ITALIC ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1136)
 1136 FORMAT('      FOR PLOT TITLES, LABELS, ETC.,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1137)
 1137 FORMAT('      THEN 2 ALLOWABLE FORMS ARE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1138)
 1138 FORMAT('            FONT TRIPLEX ITALIC ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1139)
 1139 FORMAT('            FONT TRII ')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
 1141 CONTINUE
      ITEXFO='SIMP'
      GOTO1180
C
 1142 CONTINUE
      ITEXFO='DUPL'
      GOTO1180
C
 1143 CONTINUE
      ITEXFO='TRIP'
      GOTO1180
C
 1144 CONTINUE
      ITEXFO='COMP'
      GOTO1180
C
 1145 CONTINUE
      ITEXFO='TRII'
      GOTO1180
C
 1146 CONTINUE
      ITEXFO='COMI'
      GOTO1180
C
 1147 CONTINUE
      ITEXFO='SIMS'
      GOTO1180
C
 1148 CONTINUE
      ITEXFO='COMS'
      GOTO1180
C
 1151 CONTINUE
      ITEXFO='TEKT'
      GOTO1180
C
 1152 CONTINUE
      ITEXFO='HEWL'
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE FONT (FOR PLOT SCRIPT AND TEXT)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)ITEXFO
 1182 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
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
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8111)ITEXFO
 8111 FORMAT('THE CURRENT FONT IS ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8112)IDEFFO
 8112 FORMAT('THE DEFAULT FONT IS ',A4)
      CALL DPWRST('XXX','BUG ')
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
 9011 FORMAT('***** AT THE END       OF DPFONT--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,ISUBRO,IFOUND,IERROR
 9012 FORMAT('IBUGD2,ISUBRO,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IDEFFO,ITEXFO
 9013 FORMAT('IDEFFO,ITEXFO = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPFOR(NIOLD,NINEW,IROW1,IROWN,
     1                 NLOCAL,ILOCS,NS,IBUGQ,IERROR)
C
C     PURPOSE--DEFINE AN INTEGER 0-1 VECTOR ISUB
C              WHICH WILL BE USED IN OTHER SUBROUTINES
C              FOR EXTRACTING SUBSETS.
C     ALLOWABLE FORMS--FOR XX <  XX
C                      FOR XX <= XX
C                      FOR XX =  XX
C                      FOR XX =  XX XX XX
C                      FOR XX =  XX TO XX
C                      FOR XX >= XX
C                      FOR XX >  XX
C     INPUT  ARGUMENTS--NIOLD  = THE ORIGINAL NUMBER OF
C                                ELEMENTS (ROWS) FOR THE LEFT-SIDE VARIABLE.
C                                (IT MAY BE ZERO).
C     OUTPUT ARGUMENTS--NINEW  = THE NEW NUMBER OF ELEMENTS (ROWS)
C                                FOR THE LEFT-SIDE VARIABLE.
C                                NINEW EQUALS MAX(NIOLD,IROWN)
C                     --IROW1  = THE FIRST ROW TO BE CHANGED.
C                     --IROWN  = THE LAST ROW TO BE CHANGED.
C     NOTE THAT IF THE WORD 'FOR' IS NOT IN THE ARGUMENT LIST,
C     THEN THE OUTPUT PARAMETER WILL BE SET TO NUMARG+1.
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
C     ORIGINAL VERSION--JANUARY  1978.
C     UPDATED         --JANUARY   1978.
C     UPDATED         --FEBRUARY  1978.
C     UPDATED         --JULY      1978.
C     UPDATED         --OCTOBER   1978.
C     UPDATED         --NOVEMBER  1978.
C     UPDATED         --NOVEMBER  1980.
C     UPDATED         --JANUARY   1981.
C     UPDATED         --JULY      1981.
C     UPDATED         --SEPTEMBER 1981.
C     UPDATED         --OCTOBER   1981.
C     UPDATED         --DECEMBER  1981.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IBUGQ
      CHARACTER*4 IERROR
C
      CHARACTER*4 MESSAG
      CHARACTER*4 IHWUSE
      CHARACTER*4 IH
      CHARACTER*4 IH2
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPFO'
      ISUBN2='R   '
      IERROR='NO'
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
      ILOCF=0
      NUMIT=0
      I2=0
C
C               **************************
C               **  TREAT THE FOR CASE  **
C               **************************
C
      IF(IBUGQ.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPFOR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)NIOLD,NINEW,IROW1,IROWN
   52   FORMAT('NIOLD,NINEW,IROW1,IROWN = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)NLOCAL,ILOCS,NS
   53   FORMAT('NLOCAL,ILOCS,NS = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)IBUGQ,IERROR
   54   FORMAT('IBUGQ,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)NUMARG,NUMNAM,MAXNAM,N
   55   FORMAT('NUMARG,NUMNAM,MAXNAM,N = ',5I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,56)IWIDTH,NLOCAL,ILOCF,MAXN
   56   FORMAT('IWIDTH,NLOCAL,ILOCF,MAXN = ',4I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *******************************************************
C               **  STEP 1--                                         **
C               **  INITIALIZE THE SUBSET SIZE (NS) TO MAXN.         **
C               **  CHECK FOR THE PROPER NUMBER OF INPUT ARGUMENTS.  **
C               **  ALSO CHECK THAT THE RELEVANT NUMBER OF           **
C               **  OBSERVATIONS (NLOCAL) IS POSITIVE.               **
C               *******************************************************
C
      ISTEPN='1'
      IF(IBUGQ.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NLOCAL=MAXN
      NS=MAXN
      ILOCF=NUMARG+1
      MINNA=0
      MAXNA=100
      CALL CHECKA(NUMARG,MINNA,MAXNA,IANS,IWIDTH,ISUBN1,ISUBN2,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(NLOCAL.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN DPFOR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS (FROM WHICH A')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      SUBSET WAS TO HAVE BEEN EXTRACTED) IS 0.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               *************************************************
C               **  STEP 2--                                   **
C               **  INITIALIZE ALL ELEMENTS IN ISUB(.) TO 1 .  **
C               *************************************************
C
      ISTEPN='2'
      IF(IBUGQ.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO200I=1,NLOCAL
        ISUB(I)=1
  200 CONTINUE
C
C               ************************************************
C               **  STEP 3.1--                                **
C               **  CHECK TO SEE IF HAVE THE 'FOR' CASE.      **
C               **  LOCATE THE POSITION IN THE ARGUMENT LIST  **
C               **  OF THE LAST OCCURRANCE OF THE WORD 'FOR'. **
C               ************************************************
C
      ISTEPN='3.1'
      IF(IBUGQ.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ILOCF=-1
      IF(NUMARG.LE.0)GOTO9000
      DO300J=1,NUMARG
        JP1=J+1
        IF(IHARG(J).EQ.'FOR '.AND.IHARG2(J).EQ.'    '.AND.
     1     IHARG(JP1).EQ.'I   '.AND.IHARG2(JP1).EQ.'    '.AND.
     1     JP1.LE.NUMARG)THEN
          ILOCF=J
        ENDIF
  300 CONTINUE
      IF(ILOCF.EQ.-1)THEN
        ILOCF=NUMARG+1
        GOTO9000
      ENDIF
C
C               *************************************************
C               **  STEP 3.2--                                 **
C               **  IF EXISTENT,                               **
C               **  PACK < = INTO <=                           **
C               **  PACK = < INTO =<                           **
C               **  PACK > = INTO >=                           **
C               **  PACK = > INTO =>                           **
C               **  THIS IS BECAUSE = SIGNS ARE AUTOMATICALLY  **
C               **  GIVEN A SPACE IN DPTYPE AND TREATED AS     **
C               **  AS A SEPARATE WORD.                        **
C               **  NOTE THAT NUMARG WILL BE CHANGED.          **
C               *************************************************
C
      ISTEPN='3.2'
      IF(IBUGQ.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL ADJUS2(IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
C
C               ***********************************************
C               **  STEP 4--                                 **
C               **  CHECK THAT FOR IS SUCCEEDED BY AT LEAST  **
C               **  3 OTHER ARGUMENTS.                       **
C               ***********************************************
C
      ISTEPN='4'
      IF(IBUGQ.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ILOCF3=ILOCF+3
      IF(ILOCF3.GT.NUMARG)THEN
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,402)
  402   FORMAT('      THE WORD    FOR    SHOULD HAVE BEEN FOLLOWED')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,403)
  403   FORMAT('      BY EXACTLY 3 OR BY EXACTLY 5    WORDS   --')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,404)
  404   FORMAT('      1) A DUMMY VARIABLE NAME;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,405)
  405   FORMAT('      2) AN EQUAL SIGN;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,406)
  406   FORMAT('      3) ONE LIMIT (LOWER OR UPPER) FOR THE DUMMY ',
     1         'VARIABLE;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,409)
  409   FORMAT('      4) THE INCREMENT FOR THE DUMMY VARIABLE;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,410)
  410   FORMAT('      5) THE OTHER LIMIT (UPPER OR LOWER) ',
     1         'FOR THE DUMMY VARIABLE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,421)
  421   FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,422)(IANS(I),I=1,MIN(100,IWIDTH))
  422     FORMAT('      ',100A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               *************************************
C               **  STEP 5--                       **
C               **  FORM THE 3 INTERNAL VALUES--   **
C               **  START, AINC, AND STOP.         **
C               *************************************
C
      ISTEPN='5'
      IF(IBUGQ.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ILOCF2=ILOCF+2
      ILOCF3=ILOCF+3
      ILOCF4=ILOCF+4
      ILOCF5=ILOCF+5
C
      ILOCA=ILOCF3
      IF(IARGT(ILOCA).EQ.'NUMB')THEN
        START=ARG(ILOCA)
        IF(IHARG(ILOCF2).EQ.'=   ')GOTO519
        AINC=0.0
        STOP=ARG(ILOCA)
        IF(IHARG(ILOCF2).EQ.'<   ')THEN
          START=1.0
          AINC=1.0
          STOP=ARG(ILOCA)-1.0
          GOTO580
        ELSEIF(IHARG(ILOCF2).EQ.'<=  ' .OR. IHARG(ILOCF2).EQ.'=<  ')THEN
          START=1.0
          AINC=1.0
          STOP=ARG(ILOCA)
          GOTO580
        ELSEIF(IHARG(ILOCF2).EQ.'>=  ' .OR. IHARG(ILOCF2).EQ.'>=  ')THEN
          START=ARG(ILOCA)
          AINC=1.0
          STOP=NIOLD
          GOTO580
        ELSEIF(IHARG(ILOCF2).EQ.'>   ')THEN
          START=ARG(ILOCA)+1.0
          AINC=1.0
          STOP=NIOLD
          GOTO580
        ENDIF
        GOTO519
      ELSEIF(IARGT(ILOCA).EQ.'WORD')THEN
        IH=IHARG(ILOCA)
        IH2=IHARG2(ILOCA)
        MESSAG='YES'
        IHWUSE='P'
        CALL CHECKN(IH,IH2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        START=VALUE(ILOC)
        IF(IHARG(ILOCF2).EQ.'=   ')GOTO519
        AINC=0.0
        STOP=VALUE(ILOC)
        IF(IHARG(ILOCF2).EQ.'<   ')THEN
          START=1.0
          AINC=1.0
          STOP=VALUE(ILOC)-1.0
          GOTO580
        ELSEIF(IHARG(ILOCF2).EQ.'<=  ' .OR. IHARG(ILOCF2).EQ.'=<  ')THEN
          START=1.0
          AINC=1.0
          STOP=VALUE(ILOC)
          GOTO580
        ELSEIF(IHARG(ILOCF2).EQ.'>=  ' .OR. IHARG(ILOCF2).EQ.'=>  ')THEN
          START=VALUE(ILOC)
          AINC=1.0
          STOP=NIOLD
          GOTO580
        ELSEIF(IHARG(ILOCF2).EQ.'>   ')THEN
          START=VALUE(ILOC)+1.0
          AINC=1.0
          STOP=NIOLD
          GOTO580
        ENDIF
      ENDIF
      GOTO570
C
  519 CONTINUE
C
      ILOCA=ILOCF4
      IF(ILOCA.GT.NUMARG)THEN
        AINC=0.0
        GOTO529
      ELSEIF(ILOCA.EQ.NUMARG.AND.IHARG(ILOCA).EQ.'AND'.AND.
     1   IHARG2(ILOCA).EQ.'    ')THEN
        AINC=0.0
        GOTO529
      ELSEIF(IARGT(ILOCA).EQ.'NUMB')THEN
        AINC=ARG(ILOCA)
        GOTO529
      ELSEIF(IARGT(ILOCA).EQ.'WORD'.AND.IHARG(ILOCA).EQ.'TO  ')THEN
        AINC=1.0
        GOTO529
      ELSEIF(IARGT(ILOCA).EQ.'WORD'.AND.IHARG(ILOCA).NE.'TO  ')THEN
        IH=IHARG(ILOCA)
        IH2=IHARG2(ILOCA)
        MESSAG='YES'
        IHWUSE='P'
        CALL CHECKN(IH,IH2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        AINC=VALUE(ILOC)
        GOTO529
      ENDIF
      GOTO570
C
  529 CONTINUE
      ILOCA=ILOCF5
      IF(ILOCA.GT.NUMARG)THEN
        STOP=START
        GOTO580
      ELSEIF(ILOCA.EQ.NUMARG.AND.IHARG(ILOCA).EQ.'AND'.AND.
     1   IHARG2(ILOCA).EQ.'    ')THEN
        STOP=START
        GOTO580
      ELSEIF(IARGT(ILOCA).EQ.'NUMB')THEN
        STOP=ARG(ILOCA)
        GOTO580
      ELSEIF(IARGT(ILOCA).EQ.'WORD')THEN
        IH=IHARG(ILOCA)
        IH2=IHARG2(ILOCA)
        MESSAG='YES'
        IHWUSE='P'
        CALL CHECKN(IH,IH2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        STOP=VALUE(ILOC)
        GOTO580
      ENDIF
      GOTO570
C
  570 CONTINUE
      WRITE(ICOUT,571)
  571 FORMAT('***** INTERNAL ERROR IN DPFOR--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,572)
  572 FORMAT('      AN ARGUMENT TYPE WHICH SHOULD BE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,573)
  573 FORMAT('      EITHER A NUMBER OR A WORD, IS NEITHER.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,574)IHARG(ILOCA),IHARG2(ILOCA)
  574 FORMAT('      ARGUMENT                  = ',2A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,575)ILOCA
  575 FORMAT('      LOCATION IN ARGUMENT LIST = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,576)IARGT(ILOCA)
  576 FORMAT('      ARGUMENT TYPE             = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,421)
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,422)(IANS(I),I=1,MIN(100,IWIDTH))
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GOTO9000
C
  580 CONTINUE
      IF(START.EQ.STOP)AINC=0.0
      IF(START.LT.STOP.AND.AINC.LT.0.0)AINC=-AINC
      IF(START.GT.STOP.AND.AINC.GT.0.0)AINC=-AINC
C
C               *****************************************************
C               **  STEP 6--                                       **
C               **  FORM THE ISUB(.) VECTOR;                       **
C               **  DETERMINE ALSO--                               **
C               **  THE FIRST ROW CHANGED (IROW1),                 **
C               **  THE ROW INCREMENT (IROWIN),                    **
C               **  THE LAST  ROW CHANGED (IROWN),                 **
C               **  THE NUMBER OF ROWS CHANGED (NS),               **
C               **  AND THE OUTPUT NUMBER OF ROWS (NINEW).         **
C               **  (THAT IS, THE SUBSET SAMPLE SIZE).             **
C               *****************************************************
C
      ISTEPN='6'
      IF(IBUGQ.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO600I=1,MAXN
        ISUB(I)=0
  600 CONTINUE
C
C     2016/07: FOR LARGE VALUES OF MAXOBV, NEED TO BE CAREFUL WITH
C              ARITHMETIC HERE.
C
      IF(AINC.EQ.0.0)THEN
        NUMIT=1
      ELSEIF(AINC.NE.0.0)THEN
        NUMIT=INT((STOP-START)/AINC + 0.1)
        IF(NUMIT.LT.0.0)NUMIT=-NUMIT
        NUMIT=NUMIT+1
        IF(NUMIT.GT.MAXN)NUMIT=MAXN
      ENDIF
C
      L2=0
      DO620I=1,NUMIT
        I2=I
        I2M1=I2-1
        AI=I
        RESULT=START+(AI-1.0)*AINC
        IF(I.NE.1)THEN
          IF(AINC.EQ.0.0 .OR. START.EQ.STOP .OR.
     1      (START.LT.STOP.AND.RESULT.GT.STOP) .OR.
     1      (START.GT.STOP.AND.RESULT.LT.STOP))THEN
            NS=I2M1
            GOTO690
          ENDIF
        ENDIF
        L2=L2+1
C
        IF(L2.GT.MAXN)THEN
          WRITE(ICOUT,111)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,633)MAXN
  633     FORMAT('      THE NUMBER OF GENERATED POINTS HAS JUST ',
     1           'EXCEEDED ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,421)
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,422)(IANS(II),II=1,MIN(100,IWIDTH))
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IERROR='YES'
          GOTO9000
        ENDIF
C
        XTEMP=RESULT
        ITEMP=INT(XTEMP+0.5)
        IF(ITEMP.GT.MAXN)THEN
          WRITE(ICOUT,642)
  642     FORMAT('***** ERROR IN DPFOR--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,643)MAXN
  643     FORMAT('      A REFERENCED ROW NUMBER HAS JUST EXCEEDED ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,421)
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,422)(IANS(II),II=1,MIN(100,IWIDTH))
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IERROR='YES'
          GOTO9000
        ELSEIF(ITEMP.LT.1)THEN
          WRITE(ICOUT,652)
  652     FORMAT('***** ERROR IN DPFOR--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,653)
  653     FORMAT('      A REFERENCED ROW NUMBER IS LESS THAN 1.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,421)
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,422)(IANS(II),II=1,MIN(100,IWIDTH))
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IERROR='YES'
          GOTO9000
        ENDIF
C
        ISUB(ITEMP)=1
        IF(I.EQ.1)IROW1=ITEMP
        IROWN=ITEMP
  620 CONTINUE
      NS=I2
C
  690 CONTINUE
      NINEW=NIOLD
      IF(IROWN.GT.NIOLD)NINEW=IROWN
      IROWIN=INT(AINC+0.5)
C
C               *************************************************
C               **  STEP 7--                                   **
C               **  WRITE OUT A MESSAGE INDICATING             **
C               **  THE FIRST ROW CHANGED (IROW1),             **
C               **  THE ROW INCREMENT (IROWIN),                **
C               **  THE LAST  ROW CHANGED (IROWN),             **
C               **  THE INPUT NUMBER OF ROWS (NIOLD),          **
C               **  THE NUMBER OF ROWS CHANGED (NS),           **
C               **  AND THE OUTPUT NUMBER OF ROWS (NINEW).     **
C               **  (THAT IS, THE SUBSET SAMPLE SIZE).         **
C               **  ALSO, CHECK THAT NS IS POSITIVE.           **
C               *************************************************
C
      ISTEPN='7'
      IF(IBUGQ.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,701)
  701   FORMAT('***** NOTE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,702)IROW1
  702   FORMAT('      ROW START      = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,703)IROWIN
  703   FORMAT('      ROW INCREMENT  = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,704)IROWN
  704   FORMAT('      ROW STOP       = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,705)NIOLD
  705   FORMAT('      INPUT  NUMBER OF ROWS   = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,706)NS
  706   FORMAT('      NUMBER OF ROWS AFFECTED = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,707)NINEW
  707   FORMAT('      OUTPUT NUMBER OF ROWS   = ',I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
CCCCC IF(NS.GE.1)GOTO790
CCCCC WRITE(ICOUT,999)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,711)
CC711 FORMAT('***** ERROR IN DPFOR--')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,712)
CC712 FORMAT('      THE SUBSET IS EMPTY--IT HAS NO ELEMENTS IN IT.')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC IERROR='YES'
CCCCC GOTO9000
C
CC790 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGQ.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFOR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)NIOLD,NINEW,IROW1,IROWN
 9012   FORMAT('NIOLD,NINEW,IROW1,IROWN = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NLOCAL,ILOCS,NS
 9013   FORMAT('NLOCAL,ILOCS,NS = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IBUGQ,IERROR
 9014   FORMAT('IBUGQ,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)NUMARG,NUMNAM,MAXNAM,N,MAXN
 9015   FORMAT('NUMARG,NUMNAM,MAXNAM,N,MAXN = ',5I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)IWIDTH,NLOCAL,ILOCF
 9016   FORMAT('IWIDTH,NLOCAL,ILOCF = ',3I8)
        CALL DPWRST('XXX','BUG ')
        DO9020I=1,NIOLD
          WRITE(ICOUT,9021)I,ISUB(I)
 9021     FORMAT('I,ISUB(I) = ',2I8)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFRAC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,ICONT,
     1                  IANGLU,ISEED,
CCCCC                   JULY 1993.  ADD FOLLOWING LINE.
     1                  IFRAIT,IFRATY,
     1                  IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--GENERATE A FRACTAL PLOT
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--89/1
C     ORIGINAL VERSION--DECEMBER  1988.
C     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE COMMON
C     UPDATED         --APRIL     1992. MAXCP7 AND MAXCP... MISTAKES
C     UPDATED         --JULY      1993. ADD FRACTAL ITERATIONS AND 
C                                       FRACTAL TYPE
C     UPDATED         --FEBRUARY  2011. USE DPPARS, DPPAR5
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IAND1
      CHARACTER*4 IAND2
      CHARACTER*4 ICONT
      CHARACTER*4 IANGLU
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
CCCCC JULY 1993.  ADD FOLLOWING LINE.
      CHARACTER*4 IFRATY
C
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
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
C
      DIMENSION Z1(MAXOBV)
      DIMENSION Z2(MAXOBV)
      DIMENSION Z3(MAXOBV)
      DIMENSION Z4(MAXOBV)
      DIMENSION Z5(MAXOBV)
      DIMENSION Z6(MAXOBV)
      DIMENSION Z7(MAXOBV)
      DIMENSION W(MAXOBV)
      DIMENSION U(MAXPOP)
C
      EQUIVALENCE (GARBAG(IGARB1),Z1(1))
      EQUIVALENCE (GARBAG(IGARB2),Z2(1))
      EQUIVALENCE (GARBAG(IGARB3),Z3(1))
      EQUIVALENCE (GARBAG(IGARB4),Z4(1))
      EQUIVALENCE (GARBAG(IGARB5),Z5(1))
      EQUIVALENCE (GARBAG(IGARB6),Z6(1))
      EQUIVALENCE (GARBAG(IGARB7),Z7(1))
      EQUIVALENCE (GARBAG(IGARB8),W(1))
      EQUIVALENCE (GARBAG(IGARB9),U(1))
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
      ISUBN1='DPFR'
      ISUBN2='AC  '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
C               *************************************
C               **  TREAT THE FRACTAL PLOT CASE    **
C               *************************************
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'FRAC')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPFRAC--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGG2,IBUGG3,IBUGQ,ISUBRO
   52   FORMAT('IBUGG2,IBUGG3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,IAND1,IAND2,ICONT
   53   FORMAT('ICASPL,IAND1,IAND2,ICONT = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)IANGLU,ISEED,MAXPOP
   54   FORMAT('IANGLU,ISEED,MAXPOP = ',A4,2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***************************
C               **  STEP 11--            **
C               **  EXTRACT THE COMMAND  **
C               ***************************
C
      ISTEPN='11'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'FRAC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASPL='FRAC'
C
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PLOT')THEN
        ILASTC=1
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      ELSE
        GOTO9000
      ENDIF
      IFOUND='YES'
C
C               ****************************************
C               **  STEP 2--                          **
C               **  EXTRACT THE VARIABLE LIST         **
C               ****************************************
C
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'FRAC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='FRACTAL PLOT'
      MINNA=6
      MAXNA=100
      MINN2=2
      IFLAGE=1
      IFLAGM=0
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=6
      MAXNVA=7
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'FRAC')THEN
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
      ICOL=1
      CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1            INAME,IVARN1,IVARN2,IVARTY,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1            MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1            MAXCP4,MAXCP5,MAXCP6,
     1            V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1            Z1,Z2,Z3,Z4,Z5,Z6,Z7,NLOCAL,
     1            IBUGG3,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(NUMVAR.LT.7)THEN
        DO3111I=1,NLOCAL
          Z7(I)=1.0
 3111   CONTINUE
      ENDIF
C
      CALL DPFRA2(Z1,Z2,Z3,Z4,Z5,Z6,Z7,NLOCAL,NUMV2,ICASPL,ICONT,
     1            IANGLU,ISEED,W,U,MAXPOP,
CCCCC             JULY 1993.  ADD FOLLOWING LINE (FRACTAL ITERATIONS,
CCCCC                         FRACTAL TYPE)
     1            IFRAIT,IFRATY,
     1            Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'FRAC')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFRAC--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IFOUND,IERROR
 9013   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9016   FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',
     1         3I8,2X,2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9041)NLOCAL,NUMVAR
 9041   FORMAT('NLOCAL,NUMV2 = ',2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NLOCAL.GE.1)THEN
          DO9042I=1,NLOCAL
            WRITE(ICOUT,9043)I,Z1(I),Z2(I),Z3(I),Z4(I),Z5(I),Z6(I)
 9043       FORMAT('I,Z1(I),Z2(I),Z3(I),Z4(I),Z5(I),Z6(I) = ',I8,6E10.3)
            CALL DPWRST('XXX','BUG ')
 9042     CONTINUE
        ENDIF
        IF(NPLOTP.GE.1)THEN
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
      SUBROUTINE DPFRA2(Z1,Z2,Z3,Z4,Z5,Z6,Z7,N,NUMV2,ICASPL,ICONT,
     1                  IANGLU,ISEED,W,U,MAXPOP,
CCCCC                   JULY 1993.  ADD FOLLOWING LINE
     1                  IFRAIT,IFRATY,
     1                  Y2,X2,D2,N2,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
C              THAT WILL DEFINE AN FRACTAL PLOT
C     NOTE--Z1 = INITIAL ROTATION
C           Z2 = X-SCALING
C           Z3 = Y-SCALING
C           Z4 = FINAL ROTATION
C           Z5 = X-TRANSLATION
C           Z6 = Y-TRANSLATION
C           Z7 = PROBABILITY WEIGHTING FOR EACH REGION
C     REFERENCE--WILLIAM DOUGLAS WITHERS, NAVAL ACADEMY
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
C     VERSION NUMBER--88/12
C     ORIGINAL VERSION--DECEMBER  1988.
C     UPDATED         --JULY      1993.  FRACTAL ITERATIONS, FRACTAL
C                                        TYPE.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 ICONT
      CHARACTER*4 IANGLU
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
CCCCC JULY 1993.  ADD FOLLOWING LINE.
      CHARACTER*4 IFRATY
C
C---------------------------------------------------------------------
C
      DIMENSION Z1(*)
      DIMENSION Z2(*)
      DIMENSION Z3(*)
      DIMENSION Z4(*)
      DIMENSION Z5(*)
      DIMENSION Z6(*)
      DIMENSION Z7(*)
C
      DIMENSION W(*)
      DIMENSION U(*)
C
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
C
      DIMENSION A11(100)
      DIMENSION A12(100)
      DIMENSION A21(100)
      DIMENSION A22(100)
C
      EXTERNAL UNIRAN
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGG3.EQ.'OFF'.AND.ISUBRO.NE.'FRA2')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPFRA2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGG3,ISUBRO,IERROR
   52 FORMAT('IBUGG3,ISUBRO,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)ICASPL,ICONT,IANGLU,ISEED,MAXPOP
   53 FORMAT('ICASPL,ICONT,IANGLU,ISEED,MAXPOP = ',
     1A4,2X,A4,2X,A4,2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)NUMV2
   54 FORMAT('NUMV2 = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,61)N
   61 FORMAT('N = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO62I=1,N
      WRITE(ICOUT,63)I,Z1(I),Z2(I),Z3(I),Z4(I),Z5(I),Z6(I),Z7(I)
   63 FORMAT('I,Z1(I),Z2(I),Z3(I),Z4(I),Z5(I),Z6(I),Z7(I) = ',
     1I8,7E9.2)
      CALL DPWRST('XXX','BUG ')
   62 CONTINUE
   90 CONTINUE
C
      INDEX=0
      CONST=1.0
      IF(IANGLU.EQ.'DEGR')CONST=2*3.14159/360.0
CCCCC JULY 1993.  BRANCH ACCORDING TO CASE.
C
C  WHITHER'S FORMAT
C
      IF(IFRATY.EQ.'WHIT')THEN
        DO1100I=1,N
C
        ALPHA=Z1(I)
        SCALEX=Z2(I)
        SCALEY=Z3(I)
        BETA=Z4(I)
C
        SINALP=SIN(CONST*ALPHA)
        COSALP=COS(CONST*ALPHA)
        SINBET=SIN(CONST*BETA)
        COSBET=COS(CONST*BETA)
        A11(I)=COSALP*COSBET*SCALEX-SINALP*SINBET*SCALEY
        A12(I)=(-SINALP*COSBET*SCALEX-COSALP*SINBET*SCALEY)
        A21(I)=COSALP*SINBET*SCALEX+SINALP*COSBET*SCALEY
        A22(I)=(-SINALP*SINBET*SCALEX+COSALP*COSBET*SCALEY)
C
 1100   CONTINUE
C
C  BARNSLEY ROTATION ANGLE FORMAT
C
      ELSEIF(IFRATY.EQ.'ANGL')THEN
        DO1110I=1,N
C
        ALPHA=Z1(I)
        SCALEX=Z2(I)
        SCALEY=Z3(I)
        BETA=Z4(I)
C
        A11(I)=SCALEX*COS(ALPHA)
        A12(I)=-SCALEY*SIN(BETA)
        A21(I)=SCALEX*SIN(ALPHA)
        A22(I)=SCALEY*COS(BETA)
C
 1110   CONTINUE
C
C  BARNSLEY STANDARD FORMAT
C
      ELSE
        DO1120I=1,N
        A11(I)=Z1(I)
        A12(I)=Z2(I)
        A21(I)=Z3(I)
        A22(I)=Z4(I)
 1120   CONTINUE
      ENDIF
C
      SUM=0.0
      DO1210I=1,N
      SUM=SUM+Z7(I)
 1210 CONTINUE
C
      DO1220I=1,N
      W(I)=Z7(I)/SUM
 1220 CONTINUE
C
      CUM=0.0
      DO1230I=1,N
      CUM=CUM+W(I)
      W(I)=CUM
 1230 CONTINUE
C
CCCCC JULY 1993.  ADD FOLLOWING LINES
CCCCC NU=MAXPOP
      NU=IFRAIT
      IF(NU.GT.MAXPOP)NU=IFRAIT
CCCCC END CHANGE
      CALL UNIRAN(NU,ISEED,U)
C
      XNEW=0.0
      YNEW=0.0
      K=0
      JCUT=20
      DO1310J=1,NU
C
      UJ=U(J)
      DO1320I=1,N
      INDEX=I
      IF(UJ.LE.W(I))GOTO1329
 1320 CONTINUE
 1329 CONTINUE
C
      XOLD=XNEW
      YOLD=YNEW
      XTEMP=A11(INDEX)*XOLD+A12(INDEX)*YOLD
      YTEMP=A21(INDEX)*XOLD+A22(INDEX)*YOLD
      XNEW=XTEMP+Z5(INDEX)
      YNEW=YTEMP+Z6(INDEX)
      IF(J.LE.JCUT)GOTO1310
      IF(J.GT.JCUT)K=K+1
      X2(K)=XNEW
      Y2(K)=YNEW
      D2(K)=1.0
 1310 CONTINUE
C
      N2=K
      NPLOTV=2
      GOTO9000
C
C               ******************
C               **   STEP 90--  **
C               **   EXIT       **
C               ******************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'OFF'.AND.ISUBRO.NE.'FRA2')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPFRA2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGG3,ISUBRO,IERROR
 9012 FORMAT('IBUGG3,ISUBRO,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ICASPL,ICONT,IANGLU,ISEED,MAXPOP
 9013 FORMAT('ICASPL,ICONT,IANGLU,ISEED,MAXPOP = ',
     1A4,2X,A4,2X,A4,2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9021)N
 9021 FORMAT('N = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO9022I=1,N
      WRITE(ICOUT,9023)A11(I),A12(I),A21(I),A22(I)
 9023 FORMAT('A11(I),A12(I),A21(I),A22(I) = ',4E15.7)
      CALL DPWRST('XXX','BUG ')
 9022 CONTINUE
      DO9024I=1,N
      WRITE(ICOUT,9025)I,W(I)
 9025 FORMAT('I,W(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9024 CONTINUE
      WRITE(ICOUT,9051)NUMV2
 9051 FORMAT('NUMV2 = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9052)N2,NPLOTV
 9052 FORMAT('N2,NPLOTV = ',2I8)
      CALL DPWRST('XXX','BUG ')
      DO9053I=1,N2
CCCCC WRITE(ICOUT,9054)I,U(I),X2(I),Y2(I)
C9054 FORMAT('I,U(I),X2(I),Y2(I) = ',I8,3E15.7)
CCCCC CALL DPWRST('XXX','BUG ')
 9053 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPFRAG(X,N,IWRITE,YBREAK,NBREAK,YFRAGC,YFRAGL,NFRAG,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS ROUTINE ASSUMES X IS A SEQUENCE OF POINTS WHERE
C              EACH POINT CONTAINS 4 X COORDINATES.  THESE COORDINATES
C              DEFINE THE BOUNDARY OF A "BREAK".  THIS ROUTINE RETURNS
C              THE FOLLOWING:
C
C                 1) YBREAK CONTAINS THE BREAK CENTROID.  THIS IS
C                    SIMPLY THE AVERAGE OF THE 4 POINTS.
C
C                 2) YFRAGC CONTAINS THE "FRAGMENT CENTROID".  GIVEN
C                    TWO SUCCESSIVE BREAKS, DEFINE THE LOWER BOUNDARY
C                    OF THE FRAGMENT BY THE AVERAGE OF THE RIGHT CORNER
C                    POINTS OF THE FIRST BREAK AND THE UPPER BOUNDARY
C                    BY THE AVERAGE OF THE LEFT CORNER POINTS OF THE
C                    SECOND BREAK.
C
C                 3) YFRAGL CONTAINS THE FRAGMENT LENGTH.  THIS IS
C                    SIMPLY THE DISTANCE BETWEEN THE LOWER BOUNDARY
C                    AND THE UPPER BOUNDARY.
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                OBSERVATIONS CONTAINING THE
C                                X-COORDIANTES OF THE BREAK POINTS
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C     OUTPUT ARGUMENTS--YBREAK = THE SINGLE PRECISION VECTOR THAT
C                                CONTAINS THE BREAK CENTROIDS.
C                     --YFRAGC = THE SINGLE PRECISION VECTOR THAT
C                                CONTAINS THE FRAGMENT CENTROIDS.
C                     --YFRAGL = THE SINGLE PRECISION VECTOR THAT
C                                CONTAINS THE FRAGMENT LENGTHS.
C                     --NBREAK = THE INTEGER NUMBER OF BREAK LOCATIONS.
C                     --NFRAG  = THE INTEGER NUMBER OF FRAGMENT
C                                CENTROIDS/LENGTHS.
C     OUTPUT--THE SINGLE PRECISION VECTORS YBREAK, YFRAGC, AND YFRAGL.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     COMMENT--THE INPUT VECTOR X REMAINS UNALTERED.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2019/07
C     ORIGINAL VERSION--JULY      2019.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION YBREAK(*)
      DIMENSION YFRAGC(*)
      DIMENSION YFRAGL(*)
C
      DIMENSION XLEFT(4)
      DIMENSION XRIGHT(4)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPFR'
      ISUBN2='AG  '
      IERROR='NO'
C
      NBREAK=0
      NFRAG=0
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FRAG')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPFRAG--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N
   52   FORMAT('IBUGA3,ISUBRO,N = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      NMOD=MOD(N,4)
      IF(NMOD.NE.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN DPFRAG--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      THE NUMBER OF OBSERVATIONS IN THE RESPONSE ',
     1         'VARIABLE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,115)
  115   FORMAT('      IS NOT DIVISIBLE BY FOUR.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,118)N
  118   FORMAT('      THE VALUE OF THE ARGUMENT IS ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               ********************************************************
C               **  STEP 2--                                          **
C               **  COMPUTE THE BREAK LOCATIONS                       **
C               ********************************************************
C
      IFLAGS=0
      ICNT=0
      DO200I=1,N,4
C
C       BREAK LOCATIONS
C
        AVAL=(X(I) + X(I+1) + X(I+2) + X(I+3))/4.0
        NBREAK=NBREAK+1
        YBREAK(NBREAK)=AVAL
C
C       FRAGMENT CENTROIDS AND LENGTHS
C
        IF(I+7.GT.N)GOTO200
        XLEFT(1)=X(I)
        XLEFT(2)=X(I+1)
        XLEFT(3)=X(I+2)
        XLEFT(4)=X(I+3)
        CALL SORT(XLEFT,4,XLEFT)
        AVAL1=(XLEFT(3) + XLEFT(4))/2.0
C
        XRIGHT(1)=X(I+4)
        XRIGHT(2)=X(I+5)
        XRIGHT(3)=X(I+6)
        XRIGHT(4)=X(I+7)
        CALL SORT(XRIGHT,4,XRIGHT)
        AVAL2=(XRIGHT(1) + XRIGHT(2))/2.0
C
C       CHECK FOR LEFT TO RIGHT OR RIGHT TO LEFT SORTING.  IF
C       SORTING IS NOT CONSISTENT, PRINT AN ERROR MESSAGE, BUT
C       CONTINUE PROCESSING.
C
        IF(I.EQ.1)THEN
          IF(AVAL1.LE.AVAL2)THEN
            IFLAGS=1
          ELSE
            IFLAGS=2
          ENDIF
        ELSE
C
          IF(IFLAGS.EQ.1 .AND. AVAL1.GT.AVAL2)THEN
            ICNT=ICNT+1
            IF(ICNT.EQ.1)THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,211)
  211         FORMAT('***** WARNING IN DPFRAG--')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,213)
  213         FORMAT('      BREAKS DO NOT APPEAR TO BE CONSISTENTLY ',
     1               'SORTED FROM LEFT TO RIGHT.')
              CALL DPWRST('XXX','BUG ')
CCCCC         IERROR='YES'
CCCCC         GOTO9000
            ENDIF
          ELSEIF(IFLAGS.EQ.2 .AND. AVAL1.LT.AVAL2)THEN
            ICNT=ICNT+1
            IF(ICNT.EQ.1)THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,211)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,218)
  218         FORMAT('      BREAKS DO NOT APPEAR TO BE CONSISTENLY ',
     1               'SORTED FROM RIGHT TO LEFT.')
              CALL DPWRST('XXX','BUG ')
CCCCC         IERROR='YES'
CCCCC         GOTO9000
            ENDIF
          ENDIF
C
        ENDIF
C
        NFRAG=NFRAG+1
        YFRAGC(NFRAG)=(AVAL1+AVAL2)/2.0
        YFRAGL(NFRAG)=AVAL2-AVAL1
C
  200 CONTINUE
C
C               ******************************
C               **  STEP 3--                **
C               **  WRITE OUT A FEW LINES   **
C               **  OF SUMMARY INFORMATION. **
C               ******************************
C
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,811)NBREAKS
  811   FORMAT('NUMBER OF BREAKS = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,812)NFRAG
  812   FORMAT('NUMBER OF FRAGMENTS = ',I8)
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FRAG')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DPFRAG--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR,NBREAK,NFRAG
 9012   FORMAT('IERROR,NBREAK,NFRAG = ',A4,2X,2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NBREAK.GE.1)THEN
          DO9015I=1,NBREAK
            WRITE(ICOUT,9016)I,YBREAK(I)
 9016       FORMAT('I,YBREAK(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
 9015     CONTINUE
        ENDIF
        IF(NFRAG.GE.1)THEN
          DO9025I=1,NFRAG
            WRITE(ICOUT,9026)I,YFRAGC(I),YFRAGL(I)
 9026       FORMAT('I,YFRAGC(I),YFRAGL = ',I8,2G15.7)
            CALL DPWRST('XXX','BUG ')
 9025     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFRAM(ICOM,IHARG,NUMARG,
     1IX1FSW,IX2FSW,IY1FSW,IY2FSW,
CCCCC THE FOLLOWING LINE WAS ADDED   SEPTEMBER 1993
     1FRASTY,
     1IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE FRAME SWITCHES (ON/OFF)
C              FOR ANY OF THE 4 FRAME LINES.
C              SUCH FRAME SWITCHES DEFINE WHETHER OR NOT
C              EACH OF THE 4 FRAME LINES EXISTS.
C              THE CONTENTS OF A FRAME SWITCH ARE
C              ON    OR    OFF.
C              THE FRAME SWITCHES FOR THE 4 FRAME LINES
C              ARE CONTAINED IN THE 4 VARIABLES
C              IX1FSW,IX2FSW,IY1FSW,IY2FSW.
C     INPUT  ARGUMENTS--ICOM
C                     --IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG
C     OUTPUT ARGUMENTS--IX1FSW (A HOLLERITH VECTOR)
C                     --IX2FSW (A HOLLERITH VECTOR)
C                     --IY1FSW (A HOLLERITH VECTOR)
C                     --IY2FSW (A HOLLERITH VECTOR)
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
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--OCTOBER   1980.
C     UPDATED         --MAY       1982.
C     UPDATED         --SEPTEMBER 1993. 3-D
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
C
      CHARACTER*4 IX1FSW
      CHARACTER*4 IX2FSW
      CHARACTER*4 IY1FSW
      CHARACTER*4 IY2FSW
C
CCCCC THE FOLLOWING LINE WAS ADDED   SEPTEMBER 1993
      CHARACTER*4 FRASTY
C
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
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'CORN')GOTO1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COOR')GOTO1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COLO')GOTO1900
C
CCCCC THE FOLLOWING SECTION WAS ADDED           SEPTEMBER 1993
CCCCC TO ALLOW FOR 3-D FRAME STYLE  SETTINGS    SEPTEMBER 1993
C               *****************************************************
C               **  TREAT THE CASE WHEN                           **
C               **  THE 3D FRAME STYLE IS TO BE CHANGED     **
C               *****************************************************
C
      IF(ICOM.EQ.'3DFR')GOTO1000
      GOTO1099
C
 1000 CONTINUE
      IF(NUMARG.LE.0)GOTO1010
      IF(IHARG(NUMARG).EQ.'ON')GOTO1010
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1020
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1010
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1010
      IF(IHARG(NUMARG).EQ.'?')GOTO1030
      GOTO1020
C
 1010 CONTINUE
      IFOUND='YES'
      FRASTY='3PRO'
      IF(IFEEDB.EQ.'ON')THEN
         WRITE(ICOUT,999)
  999    FORMAT(1X)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1011)
 1011    FORMAT('THE 3D FRAME SWITCH')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1012)
 1012    FORMAT('HAS JUST BEEN SET TO    3PRONG')
         CALL DPWRST('XXX','BUG ')
         GOTO1900
      ENDIF
C
 1020 CONTINUE
      IFOUND='YES'
C
      IF(IHARG(1).EQ.'OFF'.OR.IHARG(1).EQ.'NONE')THEN
         FRASTY='OFF'
         IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1011)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1021)
 1021       FORMAT('HAS JUST BEEN SET TO    OFF')
            CALL DPWRST('XXX','BUG ')
         ENDIF
         GOTO1900
      ENDIF
C
      IF(IHARG(1).EQ.'3PRO')THEN
         FRASTY='3PRO'
         IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1011)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1022)
 1022       FORMAT('HAS JUST BEEN SET TO    3PRONG')
            CALL DPWRST('XXX','BUG ')
         ENDIF
         GOTO1900
      ENDIF
C
      IF(IHARG(1).EQ.'3PLA')THEN
         FRASTY='3PLA'
         IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1011)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1023)
 1023       FORMAT('HAS JUST BEEN SET TO    3PLANE')
            CALL DPWRST('XXX','BUG ')
         ENDIF
         GOTO1900
      ENDIF
C
      IF(IHARG(1).EQ.'CUBE'.OR.IHARG(1).EQ.'BOX')THEN
         FRASTY='BOX'
         IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1011)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1024)
 1024       FORMAT('HAS JUST BEEN SET TO    BOX')
            CALL DPWRST('XXX','BUG ')
         ENDIF
         GOTO1900
      ENDIF
C
      IF(IHARG(1).EQ.'ZIGZ')THEN
         FRASTY='ZIGZ'
         IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1011)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1025)
 1025       FORMAT('HAS JUST BEEN SET TO    ZIGZAG')
            CALL DPWRST('XXX','BUG ')
         ENDIF
         GOTO1900
      ENDIF
C
 1030 CONTINUE
      IFOUND='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1031)
 1031 FORMAT('THE 3D FRAME SWITCH')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1032)FRASTY
 1032 FORMAT('HAS THE CURRENT SETTING = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1033)
 1033 FORMAT('ALLOWABLE SETTINGS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1034)
 1034 FORMAT('   3PRONG')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1035)
 1035 FORMAT('   3PLANE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1036)
 1036 FORMAT('   BOX')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1037)
 1037 FORMAT('   ZIGZAG')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1038)
 1038 FORMAT('   OFF')
      CALL DPWRST('XXX','BUG ')
      GOTO1900
C
 1099 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                           **
C               **  BOTH HORIZONTAL FRAME LINES ARE TO BE CHANGED  **
C               *****************************************************
C
      IF(ICOM.EQ.'XFRA')GOTO1100
      GOTO1199
C
 1100 CONTINUE
      IF(NUMARG.LE.0)GOTO1110
      IF(IHARG(NUMARG).EQ.'ON')GOTO1110
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1120
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1110
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1110
      IERROR='YES'
      GOTO1900
C
 1110 CONTINUE
      IFOUND='YES'
      IX1FSW='ON'
      IX2FSW='ON'
C
      IF(IFEEDB.EQ.'OFF')GOTO1119
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1115)
 1115 FORMAT('THE XFRAME SWITCH (FOR BOTH HORIZONTAL FRAME LINES) ',
     1'HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1119 CONTINUE
      GOTO1900
C
 1120 CONTINUE
      IFOUND='YES'
      IX1FSW='OFF'
      IX2FSW='OFF'
C
      IF(IFEEDB.EQ.'OFF')GOTO1129
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1125)
 1125 FORMAT('THE XFRAME SWITCH (FOR BOTH HORIZONTAL FRAME LINES) ',
     1'HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1129 CONTINUE
      GOTO1900
C
 1199 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE BOTTOM HORIZONTAL FRAME LINE IS TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X1FR')GOTO1200
      GOTO1299
C
 1200 CONTINUE
      IF(NUMARG.LE.0)GOTO1210
      IF(IHARG(NUMARG).EQ.'ON')GOTO1210
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1220
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1210
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1210
      IERROR='YES'
      GOTO1900
C
 1210 CONTINUE
      IFOUND='YES'
      IX1FSW='ON'
C
      IF(IFEEDB.EQ.'OFF')GOTO1219
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1215)
 1215 FORMAT('THE X1FRAME SWITCH (FOR THE BOTTOM HORIZONTAL ',
     1'FRAME LINE ONLY) HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1219 CONTINUE
      GOTO1900
C
 1220 CONTINUE
      IFOUND='YES'
      IX1FSW='OFF'
C
      IF(IFEEDB.EQ.'OFF')GOTO1229
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1225)
 1225 FORMAT('THE X1FRAME SWITCH (FOR THE BOTTOM HORIZONTAL ',
     1'FRAME LINE ONLY) HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1229 CONTINUE
      GOTO1900
C
 1299 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE TOP    HORIZONTAL FRAME LINE IS TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X2FR')GOTO1300
      GOTO1399
C
 1300 CONTINUE
      IF(NUMARG.LE.0)GOTO1310
      IF(IHARG(NUMARG).EQ.'ON')GOTO1310
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1320
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1310
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1310
      IERROR='YES'
      GOTO1900
C
 1310 CONTINUE
      IFOUND='YES'
      IX2FSW='ON'
C
      IF(IFEEDB.EQ.'OFF')GOTO1319
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1315)
 1315 FORMAT('THE X2FRAME SWITCH (FOR THE TOP HORIZONTAL ',
     1'FRAME LINE ONLY) HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1319 CONTINUE
      GOTO1900
C
 1320 CONTINUE
      IFOUND='YES'
      IX2FSW='OFF'
C
      IF(IFEEDB.EQ.'OFF')GOTO1329
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1325)
 1325 FORMAT('THE X2FRAME SWITCH (FOR THE TOP HORIZONTAL ',
     1'FRAME LINE ONLY) HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1329 CONTINUE
      GOTO1900
C
 1399 CONTINUE
C
C               ***************************************************
C               **  TREAT THE CASE WHEN                          **
C               **  BOTH VERTICAL FRAME LINES ARE TO BE CHANGED  **
C               ***************************************************
C
      IF(ICOM.EQ.'YFRA')GOTO1400
      GOTO1499
C
 1400 CONTINUE
      IF(NUMARG.LE.0)GOTO1410
      IF(IHARG(NUMARG).EQ.'ON')GOTO1410
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1420
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1410
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1410
      IERROR='YES'
      GOTO1900
C
 1410 CONTINUE
      IFOUND='YES'
      IY1FSW='ON'
      IY2FSW='ON'
C
      IF(IFEEDB.EQ.'OFF')GOTO1419
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1415)
 1415 FORMAT('THE YFRAME SWITCH (FOR BOTH VERTICAL FRAME LINES) ',
     1'HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1419 CONTINUE
      GOTO1900
C
 1420 CONTINUE
      IFOUND='YES'
      IY1FSW='OFF'
      IY2FSW='OFF'
C
      IF(IFEEDB.EQ.'OFF')GOTO1429
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1425)
 1425 FORMAT('THE YFRAME SWITCH (FOR BOTH VERTICAL FRAME LINES) ',
     1'HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1429 CONTINUE
      GOTO1900
C
 1499 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE LEFT   VERTICAL   FRAME LINE IS TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y1FR')GOTO1500
      GOTO1599
C
 1500 CONTINUE
      IF(NUMARG.LE.0)GOTO1510
      IF(IHARG(NUMARG).EQ.'ON')GOTO1510
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1520
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1510
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1510
      IERROR='YES'
      GOTO1900
C
 1510 CONTINUE
      IFOUND='YES'
      IY1FSW='ON'
C
      IF(IFEEDB.EQ.'OFF')GOTO1519
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1515)
 1515 FORMAT('THE Y1FRAME SWITCH (FOR THE LEFT VERTICAL ',
     1'FRAME LINE ONLY) HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1519 CONTINUE
      GOTO1900
C
 1520 CONTINUE
      IFOUND='YES'
      IY1FSW='OFF'
C
      IF(IFEEDB.EQ.'OFF')GOTO1529
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1525)
 1525 FORMAT('THE Y1FRAME SWITCH (FOR THE LEFT VERTICAL ',
     1'FRAME LINE ONLY) HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1529 CONTINUE
      GOTO1900
C
 1599 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE RIGHT  VERTCIAL   FRAME LINE IS TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y2FR')GOTO1600
      GOTO1699
C
 1600 CONTINUE
      IF(NUMARG.LE.0)GOTO1610
      IF(IHARG(NUMARG).EQ.'ON')GOTO1610
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1620
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1610
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1610
      IERROR='YES'
      GOTO1900
C
 1610 CONTINUE
      IFOUND='YES'
      IY2FSW='ON'
C
      IF(IFEEDB.EQ.'OFF')GOTO1619
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1615)
 1615 FORMAT('THE Y2FRAME SWITCH (FOR THE RIGHT VERTICAL ',
     1'FRAME LINE ONLY) HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1619 CONTINUE
      GOTO1900
C
 1620 CONTINUE
      IFOUND='YES'
      IY2FSW='OFF'
C
      IF(IFEEDB.EQ.'OFF')GOTO1629
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1625)
 1625 FORMAT('THE Y2FRAME SWITCH (FOR THE RIGHT VERTICAL ',
     1'FRAME LINE ONLY) HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1629 CONTINUE
      GOTO1900
C
 1699 CONTINUE
C
C               **************************************************
C               **  TREAT THE CASE WHEN                         **
C               **  THE ENTIRE 4-SIDED FRAME IS TO BE CHANGED   **
C               **************************************************
C
      IF(ICOM.EQ.'XYFR')GOTO1700
      IF(ICOM.EQ.'YXFR')GOTO1700
      IF(ICOM.EQ.'FRAM')GOTO1700
      GOTO1799
C
 1700 CONTINUE
      IF(NUMARG.LE.0)GOTO1710
      IF(IHARG(NUMARG).EQ.'ON')GOTO1710
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1720
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1710
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1710
      IERROR='YES'
      GOTO1900
C
 1710 CONTINUE
      IFOUND='YES'
      IX1FSW='ON'
      IX2FSW='ON'
      IY1FSW='ON'
      IY2FSW='ON'
C
      IF(IFEEDB.EQ.'OFF')GOTO1719
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1715)
 1715 FORMAT('THE FRAME SWITCH (FOR THE ENTIRE 4-SIDED FRAME) ',
     1'HAS JUST BEEN TURNED ON')
      CALL DPWRST('XXX','BUG ')
 1719 CONTINUE
      GOTO1900
C
 1720 CONTINUE
      IFOUND='YES'
      IX1FSW='OFF'
      IX2FSW='OFF'
      IY1FSW='OFF'
      IY2FSW='OFF'
C
      IF(IFEEDB.EQ.'OFF')GOTO1729
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1725)
 1725 FORMAT('THE FRAME SWITCH (FOR THE ENTIRE 4-SIDED FRAME) ',
     1'HAS JUST BEEN TURNED OFF')
      CALL DPWRST('XXX','BUG ')
 1729 CONTINUE
      GOTO1900
C
 1799 CONTINUE
C
 1900 CONTINUE
      RETURN
      END
      SUBROUTINE DPFRCC(IHARG,IHARG2,IARGT,ARG,NUMARG,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,IANS,IWIDTH,
     1PXMIN,PXMAX,PYMIN,PYMAX,IBUGP2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE FRAME CORNER COORDINATES
C              (LOWER LEFT AND UPPER RIGHT)
C              WHICH IN TURN WILL DEFINE THE SIZE AND SHAPE
C              OF THE PLOT FRAME.
C              THE 2 PAIRS OF COORDINATES ARE CONTAINED IN THE
C              4 VARIABLES    PXMIN,PYMIN    AND    PXMAX,PYMAX
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --ARG    (A  FLOATING POINT VECTOR)
C                     --NUMARG
C     OUTPUT ARGUMENTS--PXMIN = X COOR. FOR LOWER LEFT  CORNER
C                     --PXMAX = X COOR. FOR UPPER RIGHT CORNER
C                     --PYMIN = Y COOR. FOR LOWER LEFT  CORNER
C                     --PYMAX = Y COOR. FOR UPPER RIGHT CORNER
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
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--NOVEMBER  1978.
C     UPDATED         --SEPTEMBER 1980.
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
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPFR'
      ISUBN2='CC  '
      IFOUND='NO'
      IERROR='NO'
C
      IF(IBUGP2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPFRCC--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IFOUND,IERROR
   52 FORMAT('IFOUND,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)PXMIN,PXMAX,PYMIN,PYMAX
   53 FORMAT('PXMIN,PXMAX,PYMIN,PYMAX = ',4E15.7)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **************************************************
C               **  TREAT THE    FRAME     COORDINATES    CASE  **
C               **************************************************
C
      IF(NUMARG.LE.1)GOTO1150
      GOTO1110
C
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      IF(NUMARG.GE.2)GOTO1175
      GOTO1120
C
 1120 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,1121)
 1121 FORMAT('***** ERROR IN DPFRCC--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR FRAME CORNER COORDINATES ',
     1'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1124)
 1124 FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE ',
     1'PROPER FORM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1125)
 1125 FORMAT('      SUPPOSE IT IS DESIRED TO POSITION   ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1126)
 1126 FORMAT('      THE LOWER LEFT CORNER OF THE FRAME')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1127)
 1127 FORMAT('      10% ACROSS THE PAGE AND 20% UP THE PAGE, AND')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1128)
 1128 FORMAT('      THE UPPER RIGHT CORNER OF THE FRAME')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1129)
 1129 FORMAT('      90% ACROSS THE PAGE AND 80% UP THE PAGE,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1130)
 1130 FORMAT('      THEN THE ALLOWABLE FORM IS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1131)
 1131 FORMAT('      FRAME CORNER COORDINATES 10 20 90 80')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
 1150 CONTINUE
      PXMIN=15.
      PYMIN=20.
      PXMAX=85.
      PYMAX=90.
      GOTO1180
C
 1175 CONTINUE
      DO1176J=2,NUMARG
      IF(IARGT(J).EQ.'NUMB')GOTO1177
      GOTO1178
 1177 CONTINUE
      IF(J.EQ.2)PXMIN=ARG(J)
      IF(J.EQ.3)PYMIN=ARG(J)
      IF(J.EQ.4)PXMAX=ARG(J)
      IF(J.EQ.5)PYMAX=ARG(J)
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
      IF(J.EQ.2)PXMIN=VALUE(ILOC)
      IF(J.EQ.3)PYMIN=VALUE(ILOC)
      IF(J.EQ.4)PXMAX=VALUE(ILOC)
      IF(J.EQ.5)PYMAX=VALUE(ILOC)
 1176 CONTINUE
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1185)
 1185 FORMAT('THE FRAME CORNER COORDINATES HAVE JUST BEEN SET ',
     1'AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1186)PXMIN,PYMIN
 1186 FORMAT('    (X,Y) FOR LOWER LEFT  CORNER OF FRAME = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1187)PXMAX,PYMAX
 1187 FORMAT('    (X,Y) FOR UPPER RIGHT CORNER OF FRAME = ',2E15.7)
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
 9011 FORMAT('***** AT THE END       OF DPFRCC--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IFOUND,IERROR
 9012 FORMAT('IFOUND,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)PXMIN,PXMAX,PYMIN,PYMAX
 9013 FORMAT('PXMIN,PXMAX,PYMIN,PYMAX = ',4E15.7)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPFRCL(ICOM,IHARG,IARG,NUMARG,
     1                  IDEFCO,ICASCL,IRGBMX,
     1                  IX1FCO,IX2FCO,IY1FCO,IY2FCO,
     1                  IX1FC2,IX2FC2,IY1FC2,IY2FC2,
     1                  IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE FRAME COLOR SWITCHES FOR ANY OF THE 4 FRAME
C              LINES.  SUCH FRAME COLOR SWITCHES DEFINE THE COLOR FOR
C              EACH OF THE 4 FRAME LINES.  THE CONTENTS OF A FRAME COLOR
C              SWITCH ARE A COLOR.  THE FRAME COLOR SWITCHES FOR THE 4
C              FRAME LINES ARE CONTAINED IN THE 4 VARIABLES
C              IX1FCO,IX2FCO,IY1FCO,IY2FCO.
C     INPUT  ARGUMENTS--ICOM
C                     --IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG
C                     --IDEFCO
C     OUTPUT ARGUMENTS--IX1FCO (A HOLLERITH VECTOR)
C                     --IX2FCO (A HOLLERITH VECTOR)
C                     --IY1FCO (A HOLLERITH VECTOR)
C                     --IY2FCO (A HOLLERITH VECTOR)
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
C     ORIGINAL VERSION--OCTOBER   1980.
C     UPDATED         --MAY       1982.
C     UPDATED         --OCTOBER   2020. SUPPORT FOR RGB COLOR
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
      CHARACTER*4 IDEFCO
      CHARACTER*4 ICASCL
      CHARACTER*4 IX1FCO
      CHARACTER*4 IX2FCO
      CHARACTER*4 IY1FCO
      CHARACTER*4 IY2FCO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
      DIMENSION IARG(*)
      DIMENSION IX1FC2(3)
      DIMENSION IX2FC2(3)
      DIMENSION IY1FC2(3)
      DIMENSION IY2FC2(3)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
      JHOLD1=-1
      JHOLD2=-1
      JHOLD3=-1
C
      IF(NUMARG.LE.0)GOTO1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'COLO')GOTO1090
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'COLO')GOTO1090
      GOTO1900
 1090 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH HORIZONTAL FRAMES    ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'XFRA')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF' .OR.
     1     IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.
     1     IHARG(NUMARG).EQ.'COLO')THEN
          IF(ICASCL.EQ.'RGB ')THEN
            JHOLD1=-1
            JHOLD2=-1
            JHOLD3=-1
          ELSE
            IHOLD=IDEFCO
          ENDIF
        ELSE
          IF(ICASCL.EQ.'RGB ')THEN
            IF(NUMARG.GE.3)THEN
              JHOLD1=IARG(NUMARG-2)
              JHOLD2=IARG(NUMARG-1)
              JHOLD3=IARG(NUMARG)
              IF(JHOLD1.LT.0 .OR. JHOLD1.GT.IRGBMX)JHOLD1=-1
              IF(JHOLD2.LT.0 .OR. JHOLD2.GT.IRGBMX)JHOLD2=-1
              IF(JHOLD3.LT.0 .OR. JHOLD3.GT.IRGBMX)JHOLD3=-1
            ELSE
              JHOLD1=-1
              JHOLD2=-1
              JHOLD3=-1
            ENDIF
          ELSE
            IHOLD=IHARG(NUMARG)
          ENDIF
        ENDIF
C
        IFOUND='YES'
        IF(ICASCL.EQ.'RGB ')THEN
          IX1FC2(1)=JHOLD1
          IX1FC2(2)=JHOLD2
          IX1FC2(3)=JHOLD3
          IX2FC2(1)=JHOLD1
          IX2FC2(2)=JHOLD2
          IX2FC2(3)=JHOLD3
C
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
  999       FORMAT(1X)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1186)
 1186       FORMAT('THE FRAME RGB COLOR (FOR BOTH HORIZONTAL ',
     1             'FRAME LINES)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1187)JHOLD1,JHOLD2,JHOLD3
 1187       FORMAT('HAS JUST BEEN SET TO ',3I5)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSE
          IX1FCO=IHOLD
          IX2FCO=IHOLD
C
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1181)
 1181       FORMAT('THE FRAME COLOR (FOR BOTH HORIZONTAL ',
     1             'FRAME LINES)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1182)IHOLD
 1182       FORMAT('HAS JUST BEEN SET TO ',A4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
          GOTO1900
        ENDIF
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE BOTTOM HORIZONTAL FRAME IS      TO BE CHANGED  **
C               **************************************************************
C
      ELSEIF(ICOM.EQ.'X1FR')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.
     1     IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.
     1     IHARG(NUMARG).EQ.'COLO')THEN
          IF(ICASCL.EQ.'RGB ')THEN
            JHOLD1=-1
            JHOLD2=-1
            JHOLD3=-1
          ELSE
            IHOLD=IDEFCO
          ENDIF
        ELSE
          IF(ICASCL.EQ.'RGB ')THEN
            IF(NUMARG.GE.3)THEN
              JHOLD1=IARG(NUMARG-2)
              JHOLD2=IARG(NUMARG-1)
              JHOLD3=IARG(NUMARG)
              IF(JHOLD1.LT.0 .OR. JHOLD1.GT.IRGBMX)JHOLD1=-1
              IF(JHOLD2.LT.0 .OR. JHOLD2.GT.IRGBMX)JHOLD2=-1
              IF(JHOLD3.LT.0 .OR. JHOLD3.GT.IRGBMX)JHOLD3=-1
            ELSE
              JHOLD1=-1
              JHOLD2=-1
              JHOLD3=-1
            ENDIF
          ELSE
            IHOLD=IHARG(NUMARG)
          ENDIF
        ENDIF
C
        IFOUND='YES'
        IF(ICASCL.EQ.'RGB ')THEN
          IX1FC2(1)=JHOLD1
          IX1FC2(2)=JHOLD2
          IX1FC2(3)=JHOLD3
C
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1286)
 1286       FORMAT('THE FRAME RGB COLOR (FOR THE BOTTOM HORIZONTAL ',
     1             'FRAME LINE)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1187)JHOLD1,JHOLD2,JHOLD3
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSE
          IX1FCO=IHOLD
C
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1281)
 1281       FORMAT('THE FRAME COLOR (FOR THE BOTTOM HORIZONTAL ',
     1             'FRAME LINE)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1282)IHOLD
 1282       FORMAT('HAS JUST BEEN SET TO ',A4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ENDIF
        GOTO1900
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE TOP    HORIZONTAL FRAME IS      TO BE CHANGED  **
C               **************************************************************
C
      ELSEIF(ICOM.EQ.'X2FR')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.
     1     IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.
     1     IHARG(NUMARG).EQ.'COLO')THEN
          IF(ICASCL.EQ.'RGB ')THEN
            JHOLD1=-1
            JHOLD2=-1
            JHOLD3=-1
          ELSE
            IHOLD=IDEFCO
          ENDIF
        ELSE
          IF(ICASCL.EQ.'RGB ')THEN
            IF(NUMARG.GE.3)THEN
              JHOLD1=IARG(NUMARG-2)
              JHOLD2=IARG(NUMARG-1)
              JHOLD3=IARG(NUMARG)
              IF(JHOLD1.LT.0 .OR. JHOLD1.GT.IRGBMX)JHOLD1=-1
              IF(JHOLD2.LT.0 .OR. JHOLD2.GT.IRGBMX)JHOLD2=-1
              IF(JHOLD3.LT.0 .OR. JHOLD3.GT.IRGBMX)JHOLD3=-1
            ELSE
              JHOLD1=-1
              JHOLD2=-1
              JHOLD3=-1
            ENDIF
          ELSE
            IHOLD=IHARG(NUMARG)
          ENDIF
        ENDIF
C
        IFOUND='YES'
        IF(ICASCL.EQ.'RGB ')THEN
          IX2FC2(1)=JHOLD1
          IX2FC2(2)=JHOLD2
          IX2FC2(3)=JHOLD3
C
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1386)
 1386       FORMAT('THE FRAME RGB COLOR (FOR THE TOP HORIZONTAL ',
     1             'FRAME LINE)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1187)JHOLD1,JHOLD2,JHOLD3
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSE
          IX2FCO=IHOLD
C
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1381)
 1381       FORMAT('THE FRAME COLOR (FOR THE TOP HORIZONTAL ',
     1             'FRAME LINE)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1382)IHOLD
 1382       FORMAT('HAS JUST BEEN SET TO ',A4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ENDIF
        GOTO1900
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH VERTICAL   FRAMES    ARE TO BE CHANGED    **
C               *****************************************************
C
      ELSEIF(ICOM.EQ.'YFRA')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF' .OR.
     1     IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.
     1     IHARG(NUMARG).EQ.'COLO')THEN
          IF(ICASCL.EQ.'RGB ')THEN
            JHOLD1=-1
            JHOLD2=-1
            JHOLD3=-1
          ELSE
            IHOLD=IDEFCO
          ENDIF
        ELSE
          IF(ICASCL.EQ.'RGB ')THEN
            IF(NUMARG.GE.3)THEN
              JHOLD1=IARG(NUMARG-2)
              JHOLD2=IARG(NUMARG-1)
              JHOLD3=IARG(NUMARG)
              IF(JHOLD1.LT.0 .OR. JHOLD1.GT.IRGBMX)JHOLD1=-1
              IF(JHOLD2.LT.0 .OR. JHOLD2.GT.IRGBMX)JHOLD2=-1
              IF(JHOLD3.LT.0 .OR. JHOLD3.GT.IRGBMX)JHOLD3=-1
            ELSE
              JHOLD1=-1
              JHOLD2=-1
              JHOLD3=-1
            ENDIF
          ELSE
            IHOLD=IHARG(NUMARG)
          ENDIF
        ENDIF
C
        IFOUND='YES'
        IF(ICASCL.EQ.'RGB ')THEN
          IY1FC2(1)=JHOLD1
          IY1FC2(2)=JHOLD2
          IY1FC2(3)=JHOLD3
          IY2FC2(1)=JHOLD1
          IY2FC2(2)=JHOLD2
          IY2FC2(3)=JHOLD3
C
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1486)
 1486       FORMAT('THE FRAME RGB COLOR (FOR BOTH VERTICAL ',
     1             'FRAME LINES)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1187)JHOLD1,JHOLD2,JHOLD3
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSE
          IY1FCO=IHOLD
          IY2FCO=IHOLD
C
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1481)
 1481       FORMAT('THE FRAME COLOR (FOR BOTH VERTICAL ',
     1             'FRAME LINES)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1482)IHOLD
 1482       FORMAT('HAS JUST BEEN SET TO ',A4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
          GOTO1900
          IFOUND='YES'
        ENDIF
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE LEFT   VERTICAL   FRAME IS      TO BE CHANGED  **
C               **************************************************************
C
      ELSEIF(ICOM.EQ.'Y1FR')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.
     1     IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.
     1     IHARG(NUMARG).EQ.'COLO')THEN
          IF(ICASCL.EQ.'RGB ')THEN
            JHOLD1=-1
            JHOLD2=-1
            JHOLD3=-1
          ELSE
            IHOLD=IDEFCO
          ENDIF
        ELSE
          IF(ICASCL.EQ.'RGB ')THEN
            IF(NUMARG.GE.3)THEN
              JHOLD1=IARG(NUMARG-2)
              JHOLD2=IARG(NUMARG-1)
              JHOLD3=IARG(NUMARG)
              IF(JHOLD1.LT.0 .OR. JHOLD1.GT.IRGBMX)JHOLD1=-1
              IF(JHOLD2.LT.0 .OR. JHOLD2.GT.IRGBMX)JHOLD2=-1
              IF(JHOLD3.LT.0 .OR. JHOLD3.GT.IRGBMX)JHOLD3=-1
            ELSE
              JHOLD1=-1
              JHOLD2=-1
              JHOLD3=-1
            ENDIF
          ELSE
            IHOLD=IHARG(NUMARG)
          ENDIF
        ENDIF
C
        IFOUND='YES'
        IF(ICASCL.EQ.'RGB ')THEN
          IY1FC2(1)=JHOLD1
          IY1FC2(2)=JHOLD2
          IY1FC2(3)=JHOLD3
C
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1586)
 1586       FORMAT('THE FRAME RGB COLOR (FOR THE LEFT VERTICAL ',
     1             'FRAME LINE)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1187)JHOLD1,JHOLD2,JHOLD3
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSE
          IY1FCO=IHOLD
C
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1581)
 1581       FORMAT('THE FRAME COLOR (FOR THE LEFT VERTICAL ',
     1             'FRAME LINE)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1582)IHOLD
 1582       FORMAT('HAS JUST BEEN SET TO ',A4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ENDIF
        IFOUND='YES'
        GOTO1900
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE RIGHT  VERTICAL   FRAME IS      TO BE CHANGED  **
C               **************************************************************
C
      ELSEIF(ICOM.EQ.'Y2FR')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.
     1     IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.
     1     IHARG(NUMARG).EQ.'COLO')THEN
          IF(ICASCL.EQ.'RGB ')THEN
            JHOLD1=-1
            JHOLD2=-1
            JHOLD3=-1
          ELSE
            IHOLD=IDEFCO
          ENDIF
        ELSE
          IF(ICASCL.EQ.'RGB ')THEN
            IF(NUMARG.GE.3)THEN
              JHOLD1=IARG(NUMARG-2)
              JHOLD2=IARG(NUMARG-1)
              JHOLD3=IARG(NUMARG)
              IF(JHOLD1.LT.0 .OR. JHOLD1.GT.IRGBMX)JHOLD1=-1
              IF(JHOLD2.LT.0 .OR. JHOLD2.GT.IRGBMX)JHOLD2=-1
              IF(JHOLD3.LT.0 .OR. JHOLD3.GT.IRGBMX)JHOLD3=-1
            ELSE
              JHOLD1=-1
              JHOLD2=-1
              JHOLD3=-1
            ENDIF
          ELSE
            IHOLD=IHARG(NUMARG)
          ENDIF
        ENDIF
C
        IFOUND='YES'
        IF(ICASCL.EQ.'RGB ')THEN
          IY2FC2(1)=JHOLD1
          IY2FC2(2)=JHOLD2
          IY2FC2(3)=JHOLD3
C
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1686)
 1686       FORMAT('THE FRAME RGB COLOR (FOR THE RIGHT VERTICAL ',
     1             'FRAME LINE)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1187)JHOLD1,JHOLD2,JHOLD3
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSE
          IY2FCO=IHOLD
C
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1681)
 1681       FORMAT('THE FRAME COLOR (FOR THE RIGHT VERTICAL ',
     1             'FRAME LINE)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1682)IHOLD
 1682       FORMAT('HAS JUST BEEN SET TO ',A4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ENDIF
        IFOUND='YES'
        GOTO1900
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  ALL 4 FRAME FRAME LINES ARE TO BE CHANGED      **
C               *****************************************************
C
      ELSEIF(ICOM.EQ.'FRAM' .OR. ICOM.EQ.'XYFR' .OR.
     1       ICOM.EQ.'YXFR')THEN
        IF(IHARG(NUMARG).EQ.'ON'   .OR. IHARG(NUMARG).EQ.'OFF'  .OR.
     1     IHARG(NUMARG).EQ.'AUTO' .OR. IHARG(NUMARG).EQ.'DEFA' .OR.
     1     IHARG(NUMARG).EQ.'COLO')THEN
          IF(ICASCL.EQ.'RGB ')THEN
            JHOLD1=-1
            JHOLD2=-1
            JHOLD3=-1
          ELSE
            IHOLD=IDEFCO
          ENDIF
        ELSE
          IF(ICASCL.EQ.'RGB ')THEN
            IF(NUMARG.GE.3)THEN
              JHOLD1=IARG(NUMARG-2)
              JHOLD2=IARG(NUMARG-1)
              JHOLD3=IARG(NUMARG)
              IF(JHOLD1.LT.0 .OR. JHOLD1.GT.IRGBMX)JHOLD1=-1
              IF(JHOLD2.LT.0 .OR. JHOLD2.GT.IRGBMX)JHOLD2=-1
              IF(JHOLD3.LT.0 .OR. JHOLD3.GT.IRGBMX)JHOLD3=-1
            ELSE
              JHOLD1=-1
              JHOLD2=-1
              JHOLD3=-1
            ENDIF
          ELSE
            IHOLD=IHARG(NUMARG)
          ENDIF
        ENDIF
C
        IFOUND='YES'
        IF(ICASCL.EQ.'RGB ')THEN
          IX1FC2(1)=JHOLD1
          IX1FC2(2)=JHOLD2
          IX1FC2(3)=JHOLD3
          IX2FC2(1)=JHOLD1
          IX2FC2(2)=JHOLD2
          IX2FC2(3)=JHOLD3
          IY1FC2(1)=JHOLD1
          IY1FC2(2)=JHOLD2
          IY1FC2(3)=JHOLD3
          IY2FC2(1)=JHOLD1
          IY2FC2(2)=JHOLD2
          IY2FC2(3)=JHOLD3
C
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1786)
 1786       FORMAT('THE FRAME RGB COLOR (FOR ALL 4 FRAME LINES)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1187)JHOLD1,JHOLD2,JHOLD3
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSE
          IX1FCO=IHOLD
          IX2FCO=IHOLD
          IY1FCO=IHOLD
          IY2FCO=IHOLD
C
          IF(IFEEDB.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1781)
 1781       FORMAT('THE FRAME COLOR (FOR ALL 4 FRAME LINES)')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1782)IHOLD
 1782       FORMAT('HAS JUST BEEN SET TO ',A4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ENDIF
        IFOUND='YES'
        GOTO1900
C
      ENDIF
C
 1900 CONTINUE
      RETURN
      END
      SUBROUTINE DPFRE2(Y,X,XHIGH,N,NCURVE,
     1                  ICASPL,IRELAT,IHIGH,IDATSW,IRHSTG,IHSTCW,
     1                  IHSTEB,IHSTOU,
     1                  CLWID,XSTART,XSTOP,
     1                  XTEMP1,XTEMP2,XIDTEM,MAXOBV,
     1                  Y2,X2,X3D,D2,N2,NPLOTV,
     1                  IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
C              THAT WILL DEFINE
C                   1) A FREQUENCY PLOT,
C                   2) A RELATIVE FREQUENCY PLOT
C                      (THAT IS, WITH AREA = 1).
C                   3) A CUMULATIVE FREQUENCY PLOT
C                   4) A RELATIVE CUMULATIVE FREQUENCY PLOT
C                      (THAT IS, WITH MAX ORDINATE = 1).
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
C     UPDATED         --OCTOBER   1978.
C     UPDATED         --MARCH     1979.
C     UPDATED         --APRIL     1979.
C     UPDATED         --JANUARY   1981.
C     UPDATED         --AUGUST    1981.
C     UPDATED         --OCTOBER   1981.
C     UPDATED         --DECEMBER  1981.
C     UPDATED         --APRIL     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --DECEMBER  1999.  CHECK FOR POINTS OUTSIDE
C                                        INTERVAL
C     UPDATED         --FEBRUARY   2010. FOR "RAW" CASE, PUT RESPONSE
C                                        IN Y RATHER THAN X
C     UPDATED         --FEBRUARY   2010. SUPPORT FOR "HIGHLIGHTED" OPTION
C     UPDATED         --FEBRUARY   2010. SUPPORT FOR NON-EQUISPACED
C                                        FREQUENCY PLOTS
C     UPDATED         --FEBRUARY   2010. OPTION TO SUPPRESS EMPTY BINS
C     UPDATED         --FEBRUARY   2010. OPTION TO INCLUDE OUTLIERS
C     UPDATED         --FEBRUARY   2010. CALL DPBINZ TO HANDLE BINNING
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IRELAT
      CHARACTER*4 IDATSW
      CHARACTER*4 IRHSTG
      CHARACTER*4 IHSTCW
      CHARACTER*4 IHSTEB
      CHARACTER*4 IHSTOU
      CHARACTER*4 IHIGH
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRIT2
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
      DOUBLE PRECISION DCLWID
      DOUBLE PRECISION DXSTAR
      DOUBLE PRECISION DXSTOP
      DOUBLE PRECISION DCLMNJ
      DOUBLE PRECISION DCLMDJ
      DOUBLE PRECISION DCLMXJ
      DOUBLE PRECISION DJ
      DOUBLE PRECISION DXI
      DOUBLE PRECISION DXI2
      DOUBLE PRECISION DDELI
      DOUBLE PRECISION DABSDE
      DOUBLE PRECISION DTOTWI
      DOUBLE PRECISION DD21
      DOUBLE PRECISION DD2N
      DOUBLE PRECISION DN3
      DOUBLE PRECISION DN4
      DOUBLE PRECISION DSUM
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION XHIGH(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION X3D(*)
      DIMENSION D2(*)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
      DIMENSION XIDTEM(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'FRE2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,70)
   70   FORMAT('***** AT THE BEGINNING OF DPFRE2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)IDATSW,IHSTCW,IHSTOU
   71   FORMAT('IDATSW,IHSTCW,IHSTOU = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)N,CLWID,XSTART,XSTOP
   72   FORMAT('N,CLWID,XSTART,XSTOP = ',I6,3E15.7)
        CALL DPWRST('XXX','BUG ')
        DO73I=1,N
          WRITE(ICOUT,74)I,Y(I),X(I)
   74     FORMAT('I, Y(I), X(I) = ',I8,2E15.7)
          CALL DPWRST('XXX','BUG ')
   73   CONTINUE
      ENDIF
C
      ISUBN1='DPFR'
      ISUBN2='E2  '
      IERROR='NO'
      IWRIT2='OFF'
C
      K=-999
      DCLMDJ=-999.0D0
      KP3=0
      AN3=0.0
      DENOM=0.0
      DN4=0.0D0
C
      DCLWID=CLWID
      DXSTAR=XSTART
      DXSTOP=XSTOP
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      IF(N.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
   31   FORMAT('***** ERROR IN FREQUENCY PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,32)
   32   FORMAT('      THE NUMBER OF OBSERVATIONS MUST BE AT LEAST 1;')
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
CCCCC FEBRUARY 2010. IF ALL ELEMENTS THE SAME, THEN PRINT WARNING
CCCCC                AND HANDLE AS A SPECIAL CASE.
C
      IF(IDATSW.EQ.'RAW')THEN
        HOLD=Y(1)
        DO60I=1,N
          IF(Y(I).NE.HOLD)GOTO69
   60   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,61)
   61   FORMAT('***** WARNING IN FREQUENCY PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,62)
   62   FORMAT('      ALL INPUT HORIZONTAL AXIS ELEMENTS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,63)HOLD
   63   FORMAT('      ARE IDENTICALLY EQUAL TO ',G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
C
        X2(N2+1)=HOLD-1.0
        X2(N2+2)=HOLD
        X2(N2+3)=HOLD+1.0
        IF(IRELAT.EQ.'ON')THEN
          Y2(N2+1)=0.0
          Y2(N2+2)=1.0
          Y2(N2+3)=0.0
        ELSE
          Y2(N2+1)=0.0
          Y2(N2+2)=REAL(N)
          Y2(N2+3)=0.0
        ENDIF
        D2(N2+1)=REAL(NCURVE)
        D2(N2+2)=REAL(NCURVE)
        D2(N2+3)=REAL(NCURVE)
        N2=N2+3
        NPLOTV=2
        GOTO9000
      ENDIF
C
   69 CONTINUE
C
C               **********************************************
C               **  STEP 2--                                **
C               **  IF NECESSARY,                           **
C               **  DETERMINE CLASS WIDTH,                  **
C               **  START VALUE, STOP VALUE,                **
C               **  AND NUMBER OF CLASSES.                  **
C               **********************************************
C
      IF(IDATSW.EQ.'RAW')THEN
        CALL DPBINZ(Y,N,CLWID,XSTART,XSTOP,
     1              XTEMP1,MAXOBV,IHSTCW,IHSTOU,
     1              DCLWID,DXSTAR,DXSTOP,
     1              ISUBRO,IBUGG3,IERROR)
C
      ELSEIF(IDATSW.EQ.'FREQ')THEN
        CALL SORT(X,N,XTEMP1)
        NM1=N-1
        DCLWID=XTEMP1(2)-XTEMP1(1)
        DO160I=1,NM1
          IP1=I+1
          DDELI=XTEMP1(IP1)-XTEMP1(I)
          IF(DDELI.LT.DCLWID)DCLWID=DDELI
  160   CONTINUE
        DD21=XTEMP1(1)
        DD2N=XTEMP1(N)
        DXSTAR=DD21-(DCLWID/2.0D0)
        DXSTOP=DD2N+(DCLWID/2.0D0)
C
      ELSEIF(IDATSW.EQ.'FRE2')THEN
        DXSTAR=X(1)
        DXSTOP=XHIGH(N)
      ENDIF
C
      IF(IDATSW.EQ.'FRE2')THEN
        NUMCLA=N
      ELSE
        DTOTWI=DXSTOP-DXSTAR
        ANUMCL=DTOTWI/DCLWID
        NUMCLA=INT(ANUMCL+1.0 + 0.1)
C
        J=NUMCLA-1
        DJ=J
        DCLMXJ=DXSTAR+DJ*DCLWID
        DABSDE=DABS(DCLMXJ-DXSTOP)
        IF(DABSDE.LE.0.0001D0)NUMCLA=NUMCLA-1
      ENDIF
C
C               *******************************************************
C               **  STEP 3--                                         **
C               **  DETERMINE THE FREQUENCY (COUNTS) FOR EACH CLASS  **
C               *******************************************************
C
C     HISTOGRAM SUPPORTS A "HIGHLIGHTED" OPTION.  CURRENTLY, THIS
C     IS NOT SUPPORTED FOR FREQUENCY POLYGON.  HOWEVER, LEAVE BASIC
C     STRUCTURE IN PLACE IN CASE WE WANT TO IMPLEMENT THIS IN THE
C     FUTURE.
C
      IF(IDATSW.EQ.'RAW' .AND. IHIGH.EQ.'ON')THEN
        CALL DISTIN(X,N,IWRIT2,XIDTEM,NDIST,IBUGG3,IERROR)
        CALL SORT(XIDTEM,NDIST,XIDTEM)
      ELSE
        NDIST=1
      ENDIF
      NPOINT=0
C
      DO300IREPL=1,NDIST
C
        IF(IREPL.EQ.1)THEN
          DO301ISET=1,N
            XTEMP2(ISET)=Y(ISET)
  301     CONTINUE
          NTEMP=N
          ATAG=REAL(NCURVE)
        ELSE
          ICNT=0
          AHOLD=XIDTEM(IREPL-1)
          DO306ISET=1,N
            IF(X(ISET).EQ.AHOLD)THEN
              ICNT=ICNT+1
              XTEMP2(ICNT)=Y(ISET)
            ENDIF
  306     CONTINUE
          NTEMP=ICNT
          ATAG=REAL(NDIST - IREPL + 2)
        ENDIF
C
        DO310J=1,NUMCLA
          XTEMP1(J)=0.0
  310   CONTINUE
C
        IF(IDATSW.EQ.'RAW')THEN
          IBELOW=0
          IABOVE=0
          DO420I=1,NTEMP
            DXI=XTEMP2(I)
            IF(DXI.LT.DXSTAR)THEN
              IBELOW=IBELOW+1
              GOTO420
            ELSEIF(DXI.GT.DXSTOP)THEN
              IABOVE=IABOVE+1
              GOTO420
            ENDIF
            DO430J=1,NUMCLA
              J2=J
              DJ=J
              DCLMNJ=DXSTAR+(DJ-1.0D0)*DCLWID
              DCLMXJ=DXSTAR+DJ*DCLWID
              IF(DCLMXJ.GT.DXSTOP)DCLMXJ=DXSTOP
              IF(DCLMNJ.LE.DXI.AND.DXI.LT.DCLMXJ)GOTO440
  430       CONTINUE
            GOTO420
  440       CONTINUE
            XTEMP1(J2)=XTEMP1(J2)+1.0
  420     CONTINUE
C
C         FOR THIS RAW DATA CASE,
C         TREAT THE SPECIAL CASE OF EQUALITY
C         WITH THE UPPER LIMIT OF THE LAST (RIGHT-MOST) CLASS
C
          J=NUMCLA
          DO450I=1,NTEMP
            DJ=J
            DCLMXJ=DXSTAR+DJ*DCLWID
            IF(DCLMXJ.GT.DXSTOP)DCLMXJ=DXSTOP
            DXI=XTEMP2(I)
            IF(DXI.EQ.DCLMXJ)XTEMP1(J)=XTEMP1(J)+1.0
  450     CONTINUE
        ELSEIF(IDATSW.EQ.'FREQ')THEN
          IBELOW=0
          IABOVE=0
          DO520I=1,N
            DXI=X(I)
            IF(DXI.LT.DXSTAR)THEN
              IBELOW=IBELOW+1
              GOTO520
            ELSEIF(DXI.GT.DXSTOP)THEN
              IABOVE=IABOVE+1
              GOTO520
            ENDIF
            DO530J=1,NUMCLA
              J2=J
              DJ=J
              DCLMNJ=DXSTAR+(DJ-1.0D0)*DCLWID
              DCLMXJ=DXSTAR+DJ*DCLWID
              IF(DCLMXJ.GT.DXSTOP)DCLMXJ=DXSTOP
              IF(DCLMNJ.LE.DXI.AND.DXI.LT.DCLMXJ)GOTO540
  530       CONTINUE
            GOTO520
  540       CONTINUE
            XTEMP1(J2)=XTEMP1(J2)+Y(I)
  520     CONTINUE
C
C         FOR THIS FREQUENCY DATA CASE, TREAT THE SPECIAL CASE OF
C         EQUALITY WITH THE UPPER LIMIT OF THE LAST (RIGHT-MOST) CLASS
C         (ALTHOUGH THIS SHOULD NOT HAPPEN WITH THE IDATSW = 'FREQ'
C         CASE.)
C
          J=NUMCLA
          DO550I=1,N
            DJ=J
            DCLMXJ=DXSTAR+DJ*DCLWID
            IF(DCLMXJ.GT.DXSTOP)DCLMXJ=DXSTOP
            DXI=X(I)
            IF(DXI.EQ.DCLMXJ)XTEMP1(J)=XTEMP1(J)+Y(I)
  550     CONTINUE
        ELSEIF(IDATSW.EQ.'FRE2')THEN
          IBELOW=0
          IABOVE=0
          DO570J=1,NUMCLA
            J2=J
            DXI=X(J)
            DXI2=XHIGH(J)
            IF(DXI.LT.DXSTAR)THEN
              IBELOW=IBELOW+1
              GOTO570
            ELSEIF(DXI2.GT.DXSTOP)THEN
              IABOVE=IABOVE+1
              GOTO570
             ELSE
                XTEMP1(J2)=Y(J)
            ENDIF
  570     CONTINUE
        ENDIF
C
        IF(IBELOW.GE.1)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1591)IBELOW,DXSTAR
 1591     FORMAT('***** WARNING: ',I8,' DATA POINTS ARE BELOW THE ',
     1           'MINIMUM CLASS VALUE OF ',G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IF(IABOVE.GE.1)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1691)IABOVE,DXSTOP
 1691     FORMAT('***** WARNING: ',I8,' DATA POINTS ARE ABOVE THE ',
     1           'MAXIMUM CLASS VALUE OF ',G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'FRE2')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,591)
  591     FORMAT('***** IN THE MIDDLE    OF DPFRE2--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,592)DCLWID,DXSTAR,DXSTOP,DTOTWI,ANUMCL,NUMCLA
  592     FORMAT('DCLWID,DXSTAR,DXSTOP,DTOTWI,ANUMCL,NUMCLA= ',
     1           4D11.4,F10.0,I8)
          CALL DPWRST('XXX','BUG ')
          DO593J=1,NUMCLA
            DJ=J
            IF(IDATSW.EQ.'FRE2')THEN
              DCLMNJ=DBLE(X(J))
              DCLMXJ=DBLE(XHIGH(J))
            ELSE
              DCLMNJ=DXSTAR+(DJ-1.0D0)*DCLWID
              DCLMXJ=DXSTAR+DJ*DCLWID
            ENDIF
            IF(DCLMXJ.GT.DXSTOP)DCLMXJ=DXSTOP
            FJ=XTEMP1(J)
            WRITE(ICOUT,594)J,DCLMNJ,DCLMXJ,FJ
  594       FORMAT('J,DCLMNJ,DCLMXJ,FJ = ',I8,3G15.7)
            CALL DPWRST('XXX','BUG ')
  593     CONTINUE
        ENDIF
C
C               **********************************
C               **  STEP 4--                    **
C               **  DETERMINE PLOT COORDINATES  **
C               **********************************
C
        DSUM=0.0D0
        DO1110J=1,NUMCLA
          FJ=XTEMP1(J)
          DSUM=DSUM+DBLE(FJ)
 1110   CONTINUE
        DN3=DSUM
        AN3=DN3
C
        IF(IDATSW.EQ.'FRE2')THEN
          DSUM=0.0D0
          DO1112J=1,NUMCLA
            FJ=XTEMP1(J)*(XHIGH(J) - X(J))
            DSUM=DSUM+FJ
 1112     CONTINUE
          DN4=DSUM
        ENDIF
C
CCCCC   NOTE THAT THERE ARE TWO
CCCCC   WAYS TO DEFINE HEIGHT FOR RELATIVE HISTOGRAM.  ONE WAY DEFINES
CCCCC   THE AREA SO THAT THE AREA SUMS TO 1 (I.E., THE INTEGRAL) AS IN
CCCCC   A PROBABILITY DENSITY FUNCTION.  THE OTHER WAY IS SO THAT THE
CCCCC   THE HEIGHTS SUM TO 1, I.E., THE HEIGHT IS THE PERCENT OF THE
CCCCC   TOTAL.  THE IRHSTG SWITCH NOW DETERMINES WHICH METHOD IS USED.
C
        DENOM=1.0
        IF(IRELAT.EQ.'ON')THEN
          IF(IRHSTG.EQ.'PERC')THEN
            DENOM=DN3
          ELSE
            IF(IDATSW.EQ.'FRE2')THEN
              DENOM=DN4
            ELSE
              DENOM=DN3*DCLWID
            ENDIF
          ENDIF
        ENDIF
C
        NSTRT=NPOINT+1
        DSUM=0.0D0
        DO1120J=1,NUMCLA
          K=J
          NPOINT=NPOINT+1
          D2(N2+NPOINT)=ATAG
          IF(IDATSW.EQ.'FRE2')THEN
            X2(N2+NPOINT)=X(K)
            X3D(N2+NPOINT)=XHIGH(K)
          ELSE
            DJ=J
            DCLMDJ=DXSTAR+(DJ-0.5D0)*DCLWID
            X2(N2+NPOINT)=DCLMDJ
          ENDIF
          FJ=XTEMP1(J)
C
          IF(IREPL.GT.2)THEN
            ABASE=Y2(N2+NPOINT-NUMCLA)
          ELSE
            ABASE=0.0
          ENDIF
C
          IF(ICASPL.EQ.'FREQ')THEN
            Y2(N2+NPOINT)=(FJ/DENOM) + ABASE
          ELSEIF(ICASPL.EQ.'CUMF')THEN
            IF(IRELAT.EQ.'ON' .AND. IRHSTG.EQ.'AREA')THEN
              Y2(N2+NPOINT)=(FJ/DENOM) + ABASE
            ELSE
              DSUM=DSUM+FJ
              CUMFJ=(DSUM/DENOM)
              Y2(N2+NPOINT)=CUMFJ + ABASE
            ENDIF
          ENDIF
 1120   CONTINUE
C
C       FOR CUMULATIVE RELATIVE FREQUENCY PLOT (AREA CASE), COMPUTE
C       CUMULATIVE INTEGRAL OF POINTS.
C
        IF(ICASPL.EQ.'CUMF' .AND. IRELAT.EQ.'ON' .AND.
     1     IRHSTG.EQ.'AREA')THEN
          NSTOP=NPOINT
          NTOT=NSTOP-NSTRT+1
          NJUNK=2
          IWRIT2='OFF'
          CALL CUMINT(Y2(N2+NSTRT),X2(N2+NSTRT),NTOT,NJUNK,
     1                IWRIT2,XTEMP1,
     1                IBUGG3,IERROR)
          DO1129II=NSTRT,NSTOP
            Y2(N2+II)=XTEMP1(II)
 1129     CONTINUE
        ENDIF
C
  300 CONTINUE
C
      N2TEMP=NPOINT
      NPLOTV=2
C
C     FOR FREQUENCY POLYGON, "EMPTY BINS" OPTION ONLY APPLIES TO
C     THE START AND END PORTIONS OF THE PLOT.
C
      IF(IHSTEB.EQ.'OFF')THEN
        ICNT=0
        ISTRT=1
        ISTOP=N2TEMP
C
        DO1140J=1,N2TEMP
          IF(Y2(N2+J).GT.0.0)THEN
            ISTRT=J
            GOTO1149
          ENDIF
 1140   CONTINUE
 1149   CONTINUE
C
        DO1150J=N2TEMP,ISTRT,-1
          IF(Y2(N2+J).GT.0.0)THEN
            ISTOP=J
            GOTO1159
          ENDIF
 1150   CONTINUE
 1159   CONTINUE
C
        IF(ISTRT.GT.1 .OR. ISTOP.LT.N2TEMP)THEN
          DO1160J=ISTRT,ISTOP
            ICNT=ICNT+1
            X2(N2+ICNT)=X2(N2+J)
            Y2(N2+ICNT)=Y2(N2+J)
            X3D(N2+ICNT)=X3D(N2+J)
            D2(N2+ICNT)=D2(N2+J)
 1160     CONTINUE
          N2TEMP=ICNT
        ENDIF
      ENDIF
C
      N2=N2+N2TEMP
C
C               ******************
C               **   STEP 90--  **
C               **   EXIT       **
C               ******************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'FRE2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFRE2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICASPL,IRELAT,IERROR,N2
 9012   FORMAT('ICASPL,IRELAT,IERROR,N2 = ',A4,2X,A4,2X,A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IDATSW,AN3,DENOM
 9013   FORMAT('IDATSW,AN3,DENOM = ',A4,2X,E15.8,E15.8)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,N2
          WRITE(ICOUT,9016)I,Y2(I),X2(I),D2(I)
 9016     FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,2E15.7,F9.2)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFRE5(TAG1,TAG2,NREPL,N,MAXOBV,
     1                 XIDTEM,XIDTE2,
     1                 TEMP1,TEMP2,
     1                 NUMSE1,NUMSE2,
     1                 IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--UTILITY ROUTINE USED BY DPFREQ (AND POSSIBLY OTHER
C              ROUTINES). FOR 1 TO 2 REPLICATION VARIABLES, IT
C              EXTRACTS THE DISTINCT ELEMENTS FROM EACH OF THEM
C              (AND CODES THEM 1 TO K WHERE K IS THE NUMBER OF
C              DISTINCT ELEMENTS).
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORAOTRY
C                 NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2010/2
C     ORIGINAL VERSION--FEBRUARY  2010.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION TAG1(*)
      DIMENSION TAG2(*)
      DIMENSION XIDTEM(*)
      DIMENSION XIDTE2(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
C
C-----COMMON----------------------------------------------------------
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPFR'
      ISUBN2='E5  '
C
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'FRE5')THEN
        ISTEPN='1'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPFRE5--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)N,NREPL
   53   FORMAT('N,NREPL = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,57)I,TAG1(I),TAG2(I)
   57     FORMAT('I,TAG1(I),TAG2(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ******************************************************
C               **  STEP 1--                                        **
C               **  DETERMINE THE NUMBER OF DISTINCT VALUES         **
C               **  FOR THE GROUP VARIABLES (TAG1, TAG2)            **
C               **  IF ALL VALUES ARE DISTINCT, THEN THIS           **
C               **  IMPLIES WE HAVE THE NO REPLICATION CASE         **
C               **  WHICH IS AN ERROR CONDITION.                    **
C               ******************************************************
C
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'FRE5')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IWRITE='OFF'
      NUMSE1=0
      NUMSE2=0
C
      IF(NREPL.GE.1)THEN
        CALL CODE(TAG1,N,IWRITE,TEMP1,TEMP2,MAXOBV,IBUGG3,IERROR)
        DO110I=1,N
          TAG1(I)=TEMP1(I)
  110   CONTINUE
        CALL DISTIN(TAG1,N,IWRITE,XIDTEM,NUMSE1,IBUGG3,IERROR)
        CALL SORT(XIDTEM,NUMSE1,XIDTEM)
      ENDIF
C
      IF(NREPL.GE.2)THEN
        CALL CODE(TAG2,N,IWRITE,TEMP1,TEMP2,MAXOBV,IBUGG3,IERROR)
        DO120I=1,N
          TAG2(I)=TEMP1(I)
  120   CONTINUE
        CALL DISTIN(TAG2,N,IWRITE,XIDTE2,NUMSE2,IBUGG3,IERROR)
        CALL SORT(XIDTE2,NUMSE2,XIDTE2)
      ENDIF
C
      IF(NUMSE1.LT.1 .OR. NUMSE1.GT.N)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,201)
  201   FORMAT('***** ERROR IN DPFRE5 ROUTINE--')
        CALL DPWRST('XXX','BUG ')
        ITEMP=1
        WRITE(ICOUT,202)ITEMP,NUMSE1
  202   FORMAT('      THE NUMBER OF SETS FOR THE GROUP ',I1,
     1         ' VARIABLE, ',I8,',')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,203)
  203   FORMAT('      IS EITHER LESS THAN ONE OR GREATER THAN THE ',
     1         'NUMBER')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,205)N
  205   FORMAT('      OF OBSERVATIONS, ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(NREPL.GE.2 .AND. (NUMSE2.LT.1 .OR. NUMSE2.GT.N))THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,201)
        CALL DPWRST('XXX','BUG ')
        ITEMP=2
        WRITE(ICOUT,202)ITEMP,NUMSE2
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,203)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,205)N
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'FRE5')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9001)
 9001   FORMAT('***** AT THE END OF DPFRE5--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9003)NUMSE1,NUMSE2
 9003   FORMAT('NUMSE1,NUMSE2 = ',2I6)
        CALL DPWRST('XXX','BUG ')
        IF(NREPL.GE.1)THEN
          DO9011I=1,NUMSE1
            WRITE(ICOUT,9013)I,XIDTEM(I)
 9013       FORMAT('I,XIDTEM(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
 9011     CONTINUE
        ENDIF
        IF(NREPL.GE.2)THEN
          DO9021I=1,NUMSE2
            WRITE(ICOUT,9023)I,XIDTE2(I)
 9023       FORMAT('I,XIDTE2(I) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
 9021     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFREQ(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                  CLLIMI,CLWIDT,
     1                  IRHSTG,IHSTCW,IHSTEB,IHSTOU,
     1                  IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--GENERATE ONE OF THE FOLLOWING 4 PLOTS--
C              1) FREQUENCY PLOT;
C              2) RELATIVE FREQUENCY PLOT;
C              3) CUMULATIVE FREQUENCY PLOT;
C              4) RELATIVE CUMULATIVE FREQUENCY PLOT;
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
C     UPDATED         --JUNE      1978.
C     UPDATED         --JULY      1978.
C     UPDATED         --OCTOBER   1978.
C     UPDATED         --APRIL     1979.
C     UPDATED         --JANUARY   1981.
C     UPDATED         --OCTOBER   1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE
C                                       COMMON
C     UPDATED         --FEBRUARY  2010. USE DPPARS
C     UPDATED         --FEBRUARY  2010. SUPPORT FOR "MULTIPLE" AND
C                                       "REPLICATION"
C     UPDATED         --FEBRUARY  2010. SUPPORT FOR NON-EQUISPACED BINS
C     UPDATED         --FEBRUARY  2010. OPTION TO INCLUDE OUTLIERS
C     UPDATED         --MARCH     2010. USE DPPAR3 FOR SINGLE RESPONSE
C                                       VARIABLE OR MULTIPLE CASES
C     UPDATED         --JULY      2019. TWEAK SCRATCH STORAGE
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IAND1
      CHARACTER*4 IAND2
      CHARACTER*4 IRHSTG
      CHARACTER*4 IHSTCW
      CHARACTER*4 IHSTEB
      CHARACTER*4 IHSTOU
      CHARACTER*4 IBUGG2
      CHARACTER*4 IBUGG3
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IFOUN1
      CHARACTER*4 IFOUN2
      CHARACTER*4 IERROR
C
      CHARACTER*4 IRELAT
      CHARACTER*4 IHIGH
      CHARACTER*4 ICASE
      CHARACTER*4 IDATSW
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IREPL
      CHARACTER*4 IMULT
C
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
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
C
      DIMENSION CLLIMI(*)
      DIMENSION CLWIDT(*)
C
      DIMENSION Y1(20*MAXOBV)
      DIMENSION X1(MAXOBV)
      DIMENSION XHIGH(MAXOBV)
      DIMENSION XIDTEM(MAXOBV)
      DIMENSION XIDTE2(MAXOBV)
      DIMENSION XIDTE3(MAXOBV)
      DIMENSION XTEMP1(MAXOBV)
      DIMENSION XTEMP2(MAXOBV)
      DIMENSION ZY(MAXOBV)
      DIMENSION XDESGN(MAXOBV,2)
C
      EQUIVALENCE (GARBAG(IGARB1),X1(1))
      EQUIVALENCE (GARBAG(IGARB2),XHIGH(1))
      EQUIVALENCE (GARBAG(IGARB3),XTEMP1(1))
      EQUIVALENCE (GARBAG(IGARB4),XTEMP2(1))
      EQUIVALENCE (GARBAG(IGARB5),XIDTEM(1))
      EQUIVALENCE (GARBAG(IGARB6),XIDTE2(1))
      EQUIVALENCE (GARBAG(IGARB7),XIDTE3(1))
      EQUIVALENCE (GARBAG(IGARB8),ZY(1))
      EQUIVALENCE (GARBAG(IGARB9),XDESGN(1,1))
      EQUIVALENCE (GARBAG(JGAR11),Y1(1))
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
      ISUBN1='DPFR'
      ISUBN2='EQ  '
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
C               **********************************************
C               **  TREAT THE FREQUENCY PLOT AND            **
C               **  RELATED STATISTICAL DISTRIBUTION PLOTS  **
C               **********************************************
C
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'FREQ')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPFREQ--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASPL,IAND1,IAND2
   52   FORMAT('ICASPL,IAND1,IAND2 = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGG2,IBUGG3,IBUGQ,ISUBRO
   53   FORMAT('IBUGG2,IBUGG3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *************************************************
C               **  STEP 1--                                   **
C               **  EXTRACT THE COMMAND                        **
C               **  LOOK FOR ONE OF THE FOLLOWING COMMANDS:    **
C               **    1) FREQUENCY PLOT Y                      **
C               **    2) FREQUENCY PLOT Y X                    **
C               **    3) FREQUENCY PLOT Y XLOW XHIGH           **
C               **                                             **
C               **    4) MULTIPLE FREQUENCY PLOT Y1 ... YK     **
C               **    5) REPLICATED FREQUENCY PLOT Y X1 ... X2 **
C               *************************************************
C
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'FREQ')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASPL='FREQ'
      IRELAT='OFF'
      IMULT='OFF'
      IREPL='OFF'
      IHIGH='OFF'
      ILASTC=9999
C
      IF(ICOM.EQ.'FREQ')GOTO89
      IF(ICOM.EQ.'RELA')GOTO89
      IF(ICOM.EQ.'CUMU')GOTO89
      IF(ICOM.EQ.'MULT')GOTO89
      IF(ICOM.EQ.'REPL')GOTO89
      GOTO9000
C
   89 CONTINUE
      IF(ICOM.EQ.'FREQ')THEN
        ICASPL='FREQ'
        IFOUN1='YES'
      ELSEIF(ICOM.EQ.'RELA')THEN
        IRELAT='ON'
      ELSEIF(ICOM.EQ.'CUMU')THEN
        ICASPL='CUMF'
      ELSEIF(ICOM.EQ.'MULT')THEN
        IMULT='ON'
      ELSEIF(ICOM.EQ.'REPL')THEN
        IREPL='ON'
      ENDIF
C
      ISTOP=NUMARG-1
      DO90I=1,NUMARG
        IF(IHARG(I).EQ.'PLOT')THEN
          ISTOP=I
          GOTO99
        ENDIF
   90 CONTINUE
   99 CONTINUE
C
      IFOUND='NO'
      DO100I=1,ISTOP
        IF(IHARG(I).EQ.'=')THEN
          IFOUND='NO'
          GOTO9000
        ELSEIF(IHARG(I).EQ.'FREQ')THEN
          IFOUN1='YES'
        ELSEIF(IHARG(I).EQ.'PLOT')THEN
          IFOUN2='YES'
          ILASTC=MIN(ILASTC,I)
        ELSEIF(IHARG(I).EQ.'REPL')THEN
          IREPL='ON'
        ELSEIF(IHARG(I).EQ.'MULT')THEN
          IMULT='ON'
        ELSEIF(IHARG(I).EQ.'CUMU')THEN
          ICASPL='CUMF'
        ELSEIF(IHARG(I).EQ.'RELA')THEN
          IRELAT='ON'
        ENDIF
  100 CONTINUE
C
      IF(IFOUN1.EQ.'YES' .AND. IFOUN2.EQ.'YES')IFOUND='YES'
      IF(IFOUND.EQ.'NO')GOTO9000
C
      IF(IMULT.EQ.'ON')THEN
        IF(IREPL.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
  101     FORMAT('***** ERROR IN FREQUENCY PLOT--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,102)
  102     FORMAT('      YOU CANNOT SPECIFY BOTH "MULTIPLE" AND ',
     1           '"REPLICATION" FOR THE FREQUENCY PLOT.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ENDIF
C
      IF(ILASTC.GE.1)THEN
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
        ILASTC=0
      ENDIF
C
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'FREQ')THEN
        WRITE(ICOUT,112)ICASPL,IRELAT,IMULT,IREPL
  112   FORMAT('ICASPL,IRELAT,IMULT,IREPL = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ****************************************
C               **  STEP 2--                          **
C               **  EXTRACT THE VARIABLE LIST         **
C               ****************************************
C
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'FREQ')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='FREQUENCY PLOT'
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
        MAXNVA=100
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'FREQ')THEN
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
      NRESP=0
      NREPL=0
      NGROUP=0
      IF(IMULT.EQ.'ON')THEN
        NRESP=NUMVAR
        IDATSW='RAW'
      ELSEIF(IREPL.EQ.'ON')THEN
        IDATSW='RAW'
        NRESP=1
        NREPL=NUMVAR-NRESP
        IF(NREPL.LT.1 .OR. NREPL.GT.2)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,511)
  511     FORMAT('      FOR THE REPLICATION CASE, THE NUMBER OF ',
     1           'REPLICATION VARIABLES')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,512)
  512     FORMAT('      MUST BE BETWEEN 1 AND 2;  SUCH WAS NOT THE ',
     1           'CASE HERE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,513)NREPL
  513     FORMAT('      THE NUMBER OF REPLICATION VARIABLES = ',I5)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ELSE
        NRESP=1
        NGROUP=NUMVAR-NRESP
        IF(NGROUP.EQ.0)IDATSW='RAW'
        IF(NGROUP.EQ.1)IDATSW='FREQ'
        IF(NGROUP.EQ.2)IDATSW='FRE2'
        IF(NGROUP.LT.0 .OR. NGROUP.GT.2)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,521)
  521     FORMAT('      THE NUMBER OF CLASS VARIABLES IS LESS THAN ',
     1           'ZERO OR GREATER THAN TWO.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,523)NGROUP
  523     FORMAT('      THE NUMBER OF CLASS VARIABLES = ',I5)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ENDIF
C
C
C               ********************************************************
C               **  STEP 7--                                          **
C               **  DETERMINE IF THE ANALYST                          **
C               **  HAS SPECIFIED    1)  THE CLASS WIDTH,             **
C               **                   2)  THE MIN POINT OF THE FIRST   **
C               **                       CELL,                        **
C               **                   3)  THE MAX POINT OF THE LAST    **
C               **                       CELL,                        **
C               ********************************************************
C
      ISTEPN='7'
      IF(IBUGG2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CLWID=CLWIDT(1)
      XSTART=CLLIMI(1)
      XSTOP=CLLIMI(2)
C
C               *****************************************
C               **  STEP 6--                           **
C               **  GENERATE THE FREQUENCY   PLOTS FOR **
C               **  THE VARIOUS CASES.                 **
C               *****************************************
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'FREQ')THEN
        ISTEPN='6'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,601)NRESP,NREPL,NGROUP
  601   FORMAT('NRESP,NREPL,NGROUP = ',3I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *************************************************
C               **  STEP 7A--                                  **
C               **  CASE 1: SINGLE RESPONSE VARIABLE WITH NO   **
C               **          REPLICATION (RESPONSE VARIABLE CAN **
C               **          BE A MATRIX).                      **
C               *************************************************
C
      IF(NRESP.EQ.1 .AND. NREPL.EQ.0)THEN
        ISTEPN='7A'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'FREQ')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        ICOL=1
        CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              Y1,X1,XHIGH,NLOCAL,NLOCA2,NLOCA3,ICASE,
     1              IBUGG3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
C               *****************************************************
C               **  STEP 7B--                                      **
C               **  FORM THE VERTICAL AND HORIZONTAL AXIS          **
C               **  VALUES Y(.) AND X(.) FOR THE PLOT.             **
C               **  RESET THE VECTOR D(.) TO ALL ONES.             **
C               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).  **
C               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).  **
C               *****************************************************
C
        NCURVE=1
        NPLOTP=0
        CALL DPFRE2(Y1,X1,XHIGH,NLOCAL,NCURVE,
     1              ICASPL,IRELAT,IHIGH,IDATSW,IRHSTG,IHSTCW,
     1              IHSTEB,IHSTOU,
     1              CLWID,XSTART,XSTOP,
     1              XTEMP1,XTEMP2,XIDTEM,MAXOBV,
     1              Y,X,X3D,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C               ******************************************
C               **  STEP 8A--                           **
C               **  CASE 2: MULTIPLE RESPONSE VARIABLES **
C               ******************************************
C
      ELSEIF(NRESP.GT.1)THEN
        ISTEPN='8A'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'FREQ')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C       LOOP THROUGH EACH OF THE RESPONSE VARIABLES
C
        NPLOTP=0
        IDATSW='RAW'
        DO810IRESP=1,NRESP
          NCURVE=IRESP
C
          IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'FREQ')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,811)IRESP,NCURVE
  811       FORMAT('IRESP,NCURVE = ',2I5)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          CLWID=CLWIDT(1)
          XSTART=CLLIMI(1)
          XSTOP=CLLIMI(2)
C
          ICOL=IRESP
          NUMVA2=1
          CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1                INAME,IVARN1,IVARN2,IVARTY,
     1                ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1                MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1                MAXCP4,MAXCP5,MAXCP6,
     1                V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1                Y1,X1,XHIGH,NLOCAL,NLOCA2,NLOCA3,ICASE,
     1                IBUGG3,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
C               *****************************************************
C               **  STEP 8B--                                      **
C               **  FORM THE VERTICAL AND HORIZONTAL AXIS          **
C               **  VALUES Y(.) AND X(.) FOR THE PLOT.             **
C               *****************************************************
C
          CALL DPFRE2(Y1,X1,XHIGH,NLOCAL,NCURVE,
     1                ICASPL,IRELAT,IHIGH,IDATSW,IRHSTG,IHSTCW,
     1                IHSTEB,IHSTOU,
     1                CLWID,XSTART,XSTOP,
     1                XTEMP1,XTEMP2,XIDTEM,MAXOBV,
     1                Y,X,X3D,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
  810   CONTINUE
C
C               *****************************************************
C               **  STEP 9A--                                      **
C               **  CASE 3: ONE OR TWO  REPLICATION VARIABLES.     **
C               **          FOR THIS CASE, THE NUMBER OF RESPONSE  **
C               **          VARIABLES MUST BE EXACTLY 1.           **
C               **          CURRENTLY, GROUPED DATA NOT SUPPORTED  **
C               **          WITH REPLICATION.                      **
C               *****************************************************
C
      ELSEIF(NRESP.EQ.1 .AND. NREPL.GE.1)THEN
        ISTEPN='9A'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'FREQ')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        J=0
        IMAX=NRIGHT(1)
        IF(NQ.LT.NRIGHT(1))IMAX=NQ
        DO910I=1,IMAX
          IF(ISUB(I).EQ.0)GOTO910
          J=J+1
C
C         RESPONSE VARIABLE IN Y1
C
          IJ=MAXN*(ICOLR(1)-1)+I
          IF(ICOLR(1).LE.MAXCOL)Y1(J)=V(IJ)
          IF(ICOLR(1).EQ.MAXCP1)Y1(J)=PRED(I)
          IF(ICOLR(1).EQ.MAXCP2)Y1(J)=RES(I)
          IF(ICOLR(1).EQ.MAXCP3)Y1(J)=YPLOT(I)
          IF(ICOLR(1).EQ.MAXCP4)Y1(J)=XPLOT(I)
          IF(ICOLR(1).EQ.MAXCP5)Y1(J)=X2PLOT(I)
          IF(ICOLR(1).EQ.MAXCP6)Y1(J)=TAGPLO(I)
C
          ICOLC=1
          DO920IR=1,MIN(NREPL,2)
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
C
  910   CONTINUE
        NLOCAL=J
C
C       *****************************************************
C       **  STEP 9B--                                      **
C       **  FORM THE VERTICAL AND HORIZONTAL AXIS          **
C       **  VALUES Y(.) AND X(.) FOR THE PLOT.             **
C       **                                                 **
C       **  FOR THIS CASE, WE NEED TO LOOP THROUGH THE     **
C       **  VARIOUS REPLICATIONS.                          **
C       *****************************************************
C
        ISTEPN='9B'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'FREQ')THEN
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,931)
  931     FORMAT('***** FROM THE MIDDLE  OF FREQ--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,932)ICASPL,NUMVAR,IDATSW,NLOCAL
  932     FORMAT('ICASPL,NUMVAR,IDATSW,NQ = ',A4,I8,2X,A4,I8)
          CALL DPWRST('XXX','BUG ')
          IF(NLOCAL.GE.1)THEN
            DO935I=1,NLOCAL
              WRITE(ICOUT,936)I,Y1(I),XDESGN(I,1),XDESGN(I,2)
  936         FORMAT('I,Y1(I),XDESGN(I,1),XDESGN(I,2) = ',I8,3F12.5)
              CALL DPWRST('XXX','BUG ')
  935       CONTINUE
          ENDIF
        ENDIF
C
C       *****************************************************
C       **  STEP 9C--                                      **
C       **  FIND THE DISTINCT VALUES IN EACH OF THE        **
C       **  REPLICATION VARIABLES.                         **
C       *****************************************************
C
        CALL DPFRE5(XDESGN(1,1),XDESGN(1,2),
     1             NREPL,NLOCAL,MAXOBV,
     1             XIDTEM,XIDTE2,
     1             XTEMP1,XTEMP2,
     1             NUMSE1,NUMSE2,
     1             IBUGG3,ISUBRO,IERROR)
C
C       *****************************************************
C       **  STEP 9D--                                      **
C       **  NOW LOOP THROUGH THE VARIOUS REPLICATIONS      **
C       *****************************************************
C
        NPLOTP=0
        NCURVE=0
        IF(NREPL.EQ.1)THEN
          J=0
          DO1110ISET1=1,NUMSE1
            K=0
            DO1130I=1,NLOCAL
              IF(XIDTEM(ISET1).EQ.XDESGN(I,1))THEN
                K=K+1
                ZY(K)=Y1(I)
              ENDIF
 1130       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            IF(NTEMP.GT.0)THEN
              CALL DPFRE2(ZY,X1,XHIGH,NTEMP,NCURVE,
     1                    ICASPL,IRELAT,IHIGH,IDATSW,IRHSTG,IHSTCW,
     1                    IHSTEB,IHSTOU,
     1                    CLWID,XSTART,XSTOP,
     1                    XTEMP1,XTEMP2,XIDTEM,MAXOBV,
     1                    Y,X,X3D,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
            ENDIF
 1110     CONTINUE
        ELSEIF(NREPL.EQ.2)THEN
          J=0
          NTOT=NUMSE1*NUMSE2
          DO1210ISET1=1,NUMSE1
          DO1220ISET2=1,NUMSE2
            K=0
            DO1290I=1,NLOCAL
              IF(
     1           XIDTEM(ISET1).EQ.XDESGN(I,1) .AND.
     1           XIDTE2(ISET2).EQ.XDESGN(I,2)
     1          )THEN
                K=K+1
                ZY(K)=Y1(I)
              ENDIF
 1290       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            IF(NTEMP.GT.0)THEN
              CALL DPFRE2(ZY,X1,XHIGH,NTEMP,NCURVE,
     1                    ICASPL,IRELAT,IHIGH,IDATSW,IRHSTG,IHSTCW,
     1                    IHSTEB,IHSTOU,
     1                    CLWID,XSTART,XSTOP,
     1                    XTEMP1,XTEMP2,XIDTEM,MAXOBV,
     1                    Y,X,X3D,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
            ENDIF
 1220     CONTINUE
 1210     CONTINUE
        ENDIF
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'FRE2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFREQ--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9013   FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',
     1         I8,I8,I8,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IRELAT,CLWID,XSTART,XSTOP
 9014   FORMAT('IRELAT,CLWID,XSTART,XSTOP = ',A4,2X,3E15.7)
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
      SUBROUTINE DPFRIE(TEMP1,TEMP2,MAXNXT,
     1                  ICAPSW,IFORSW,IMULT,
     1                  IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--CARRY OUT FRIEDMAN TEST
C              NON-PARAMETRIC TWO-WAY ANOVA
C     EXAMPLE--FRIEDMAN TEST Y X1 X2
C     REFERENCE--CONOVER (1999), "PRACTICAL NONPARAMETRIC STATISTICS",
C                THIRD EDITION, WILEY, PP. 369-372.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2003/10
C     ORIGINAL VERSION--OCTOBER   2003.
C     UPDATED         --JANUARY   2007.  CALL LIST TO DPFRI2
C     UPDATED         --APRIL     2011. USE DPPARS
C     UPDATED         --APRIL     2011. SUPPORT FOR "MULTIPLE" CASE
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
      DOUBLE PRECISION YRANK(MAXOBV)
C
      EQUIVALENCE(GARBAG(IGARB1),XTEMP2(1))
      EQUIVALENCE(GARBAG(IGARB2),DBLOCK(1))
      EQUIVALENCE(GARBAG(IGARB3),DTREAT(1))
      EQUIVALENCE(GARBAG(IGARB4),RJ(1))
      EQUIVALENCE(DGARBG(IDGAR1),YRANK(1))
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPFR'
      ISUBN2='IE  '
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
C               ******************************************
C               **  TREAT THE FRIEDMAN TEST CASE        **
C               ******************************************
C
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FRIE')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPFRIE--')
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FRIE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IMULT='OFF'
      INAME='FRIEDMAN TEST'
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FRIE')THEN
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
C               **  CARRY OUT THE FRIEDMAN TEST **
C               **********************************
C
      ISTEPN='3'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FRIE')
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
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FRIE')
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
        IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'FRIE')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5211)
 5211     FORMAT('***** FROM DPFRIE, AS WE ARE ABOUT TO CALL DPFRI2--')
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
        CALL DPFRI2(Y,X,XTEMP2,NS1,IVARN1,IVARN2,
     1              DBLOCK,DTREAT,YRANK,RJ,
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
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FRIE')
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
 9011   FORMAT('***** AT THE END       OF DPFRIE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)IFOUND,IERROR,STATVA,STATCD
 9016   FORMAT('IFOUND,IERROR,STATVA,STATCD = ',2(A4,2X),2G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFRIT(IHARG,IARGT,ARG,NUMARG,IDEFFI,
     1IFRAIT,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE FRACTAL ITERATIONS
C              THIS DEFINES THE MAXIMUM NUMBER OF POINTS TO
C              PLOT FOR FRACTAL PLOTS.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --ARG    (A  FLOATING POINT VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C                     --IDEFFI (A  FLOATING POINT VARIABLE)
C     OUTPUT ARGUMENTS--IFRAIT  (AN INTEGER VARIABLE)
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY-ALAN HECKERT
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--93/7
C     ORIGINAL VERSION--JULY    1993.
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
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(NUMARG.EQ.0)GOTO1199
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'ITER')GOTO1110
      GOTO1199
C
 1110 CONTINUE
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
 1121 FORMAT('***** ERROR IN DPFRIT--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR FRACTAL ITERATIONS ',
     1'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1124)
 1124 FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE ',
     1'PROPER FORM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1131)
 1131 FORMAT('      FRACTAL ITERATIONS 20000')
      CALL DPWRST('XXX','BUG ')
      GOTO1199
C
 1150 CONTINUE
      IHOLD=IDEFFI
      GOTO1180
C
 1160 CONTINUE
      IHOLD=INT(ARG(NUMARG)+0.5)
      IF(IHOLD.LE.0)IHOLD=IDEFFI
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      IFRAIT=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)IFRAIT
 1181 FORMAT('THE FRACTAL ITERATIONS HAS JUST BEEN SET TO ',
     1I8)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPFRI2(Y,BLOCK,TREAT,N,IVARID,IVARI2,
     1                  DBLOCK,DTREAT,YRANK,RJ,
     1                  TEMP1,TEMP2,MAXNXT,
     1                  STATVA,STATCD,PVAL,
     1                  CUT0,CUT50,CUT75,CUT90,CUT95,CUT99,CUT999,
     1                  ICAPSW,ICAPTY,IFORSW,IMULT,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--THIS ROUTINE CARRIES OUT FRIEDMAN'S TEST
C              NON-PARAMETRIC TWO-WAY ANOVA
C     EXAMPLE--FRIEDMAN TEST Y BLOCK TREAT
C     REFERENCE--CONOVER (1999), "PRACTICAL NONPARAMETRIC STATISTICS",
C                THIRD EDITION, WILEY, PP. 369-372.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2003/10
C     ORIGINAL VERSION--OCTOBER   2003.
C     UPDATED         --JANUARY   2006. FIX BUG IN RANKING
C                                       (UNCORRECTED VERSION WORKS
C                                       IF DATA ARE RANKS WITHIN
C                                       THE BLOCK).
C     UPDATED         --JANUARY   2006. SOME INFO THAT WAS SUPPOSSED
C                                       TO GO TO DPST2F.DAT WAS
C                                       GOING TO DPST1F.DAT
C     UPDATED         --OCTOBER   2006. CALL LIST TO TPPF
C     UPDATED         --JANUARY   2007. CALL LIST TO RANK
C     UPDATED         --APRIL     2011. USE DPDTA1 AND DPDTA4 TO PRINT
C                                       OUTPUT TABLES.  THIS ADDS RTF
C                                       SUPPORT AND SPECIFICATION OF
C                                       THE NUMBER OF DIGITS.
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
      IERROR='NO'
      IWRITE='OFF'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FRI2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPFRI2--')
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
      CALL DPFRI3(Y,BLOCK,TREAT,N,
     1            DBLOCK,DTREAT,RJ,TEMP1,TEMP2,YRANK,
     1            MAXNXT,MAXNX2,
     1            STATVA,STATCD,PVAL,
     1            NBLOCK,NTREAT,NUMDF1,NUMDF2,T1,T2,A1,C1,
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
      TERM1=(A1-C1)*2.0*ANB/((ANB-1.0)*(AK-1.0))
      TERM2=1.0 - T1/(ANB*(AK-1.0))
      CONTRA=SQRT(TERM1*TERM2)
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FRI2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               ******************************
C               **   STEP 43--              **
C               **   WRITE OUT EVERYTHING   **
C               **   FOR FRIEDMAN TEST      **
C               ******************************
C
      ISTEPN='43'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FRI2')
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
      ITITLE='Friedman Two Factor Test'
      NCTITL=24
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
      ITEXT(ICNT)='Friedman Test Statistic (Original):'
      NCTEXT(ICNT)=35
      AVALUE(ICNT)=T1
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sum of Squares of Ranks (A1):'
      NCTEXT(ICNT)=29
      AVALUE(ICNT)=A1
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Correction Factor (C1):'
      NCTEXT(ICNT)=29
      AVALUE(ICNT)=C1
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Friedman Test Statistic (Conover):'
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=STATVA
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='CDF of Test Statistic:'
      NCTEXT(ICNT)=22
      AVALUE(ICNT)=STATCD
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='P-Value:'
      NCTEXT(ICNT)=8
      PVAL=1.0 - STATCD
      AVALUE(ICNT)=1.0 - STATCD
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FRI2')
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FRI2')
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FRI2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFRI2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9012)STATVA,STATCD,PVAL
 9012   FORMAT('STATVA,STATCD,PVAL = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFRI3(Y,BLOCK,TREAT,N,
     1                  DBLOCK,DTREAT,RJ,TEMP1,TEMP2,YRANK,
     1                  MAXNXT,MAXNX2,
     1                  STATVA,STATCD,PVAL,
     1                  NBLOCK,NTREAT,NUMDF1,NUMDF2,T1,T2,A1,C1,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--THIS ROUTINE CARRIES OUT FRIEDMAN'S TEST
C              NON-PARAMETRIC TWO-WAY ANOVA
C     EXAMPLE--FRIEDMAN TEST Y BLOCK TREAT
C     REFERENCE--CONOVER (1999), "PRACTICAL NONPARAMETRIC STATISTICS",
C                THIRD EDITION, WILEY, PP. 369-372.
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
C     ORIGINAL VERSION--JULY      2011. EXTRACTED FROM DPFRI2 ROUTINE
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
      DOUBLE PRECISION DSUM1
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
      DOUBLE PRECISION YRANK(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPFR'
      ISUBN2='I3  '
      IERROR='NO'
      IWRITE='OFF'
C
      STATVA=CPUMIN
      STATCD=CPUMIN
      PVAL=CPUMIN
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FRI3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPFRI3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N,MAXNXT
   52   FORMAT('IBUGA3,ISUBRO,N,MAXNXT = ',2(A4,2X),2I8)
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FRI3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      HOLD=Y(1)
      DO1135I=2,N
      IF(Y(I).NE.HOLD)GOTO1139
 1135 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1131)
 1131 FORMAT('***** ERROR FROM FRIEDMAN TEST--')
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
C               **  FOR FRIEDMAN TEST       **
C               ******************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FRI3')
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
C  STEP 2B: COMPUTE TREATMENT RANKS WITHIN EACH BLOCK
C
      ISTEPN='2B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FRI3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO2010I=1,N
        YRANK(I)=-1.0D0
 2010 CONTINUE
C
      DO2110I=1,NBLOCK
        HOLD=DBLOCK(I)
        ICOUNT=0
        DO2120J=1,N
          IF(BLOCK(J).EQ.HOLD)THEN
            ICOUNT=ICOUNT+1
            RJ(ICOUNT)=Y(J)
          ENDIF
 2120   CONTINUE
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
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FRI3')THEN
        DO2140I=1,N
          WRITE(ICOUT,2142)I,Y(I),YRANK(I)
 2142     FORMAT('I,Y(I),YRANK(I) = ',I8,G15.7,F12.2)
          CALL DPWRST('XXX','BUG ')
 2140   CONTINUE
      ENDIF
C
C  STEP 2C: NOW COMPUTE RANK SUMS FOR EACH TREATMENT
C
      ISTEPN='2C'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FRI3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO2210I=1,NTREAT
        HOLD=DTREAT(I)
        DSUM1=0.0D0
        DO2220J=1,N
          IF(TREAT(J).EQ.HOLD)THEN
            DSUM1=DSUM1 + YRANK(J)
          ENDIF
 2220   CONTINUE
        RJ(I)=REAL(DSUM1)
 2210 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FRI3')THEN
        DO2240I=1,NTREAT
          WRITE(ICOUT,2242)I,RJ(I)
 2242     FORMAT('I,RJ(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
 2240   CONTINUE
      ENDIF
C
C  STEP 4: NOW COMPUTE VARIOUS QUANTITIES BASED ON RJ
C
      ANB=REAL(NBLOCK)
      AK=REAL(NTREAT)
      C1=ANB*AK*(AK+1.0)**2/4.0
      DSUM1=0.0D0
      DO2310I=1,N
        DSUM1=DSUM1 + YRANK(I)**2
 2310 CONTINUE
      A1=REAL(DSUM1)
      DSUM1=0.0D0
      DO2320I=1,NTREAT
        DSUM1=DSUM1 + RJ(I)**2
 2320 CONTINUE
      T1=(AK-1.0)*(REAL(DSUM1)-ANB*C1)/(A1-C1)
      T2=(ANB-1.0)*T1/(ANB*(AK-1.0) - T1)
C
      STATVA=T2
      NUMDF1=NTREAT-1
      NUMDF2=(NBLOCK-1)*(NTREAT-1)
      CALL FCDF(STATVA,NUMDF1,NUMDF2,STATCD)
      PVAL=1.0 - STATCD
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FRI3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFRI3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9012)STATVA,STATCD,PVAL
 9012   FORMAT('STATVA,STATCD,PVAL = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFRLI(ICASPL,IFRALI,Y,N,
     1                  GX1MIN,GX1MAX,GY1MIN,GY1MAX,
     1                  IX1TSC,IX1TSW,IY1TSC,IY1TSW,
     1                  IX1JSW,IY1JSW,
     1                  NMJX1T,NMNX1T,IX1NSW,NMJY1T,NMNY1T,IY1NSW,
     1                  PX1COO,X1COOR,NX1COO,
     1                  PY1COO,Y1COOR,NY1COO,
     1                  PX1CMN,X1COMN,NX1CMN,PX1TOL,PX1TOR,
     1                  PY1CMN,Y1COMN,NY1CMN,PY1TOL,PY1TOR,
     1                  ITICUN,PXMIN,PXMAX,PYMIN,PYMAX,
     1                  FMIN,FMAX,
     1                  IBUGG4,ISUBRO,IERROR)
C
C     PURPOSE--IN SOME CASES, WE MAY WANT TO DETERMINE THE FRAME LIMITS
C              THAT WOULD BE COMPUTED FOR A GIVEN DATA SET WITHOUT
C              GENERATING A PLOT.  FOR EXAMPLE, IF WE ARE PLOTTING
C              SUBSETS OF DATA BUT WE WANT LIMITS BASED ON THE FULL DATA
C              SET.  THIS IS A MODIFIED VERSION OF DPDEDL/DPDEFL. IN
C              THIS CASE, WE ARE ONLY LOOKING AT A SINGLE AXIS (I.E.,
C              EITHER Y OR X BUT NOT BOTH TOGETHER).  ALSO, WE DO NOT
C              WANT TO ACTUALLY MODIFY THE COMMON BLOCK SWITCHES (I.E.,
C              DX1MIN, FX1MIN, GX1MIN, ETC.) AND WE ALSO IGNORE SOME
C              SWITCHES THAT DPDEDL AND DPDEFL USE.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--83.6
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MAY       1983.
C
C-----NON-COMMON VARIABLES (GRAPHICS)-----------------------------------
C
      DIMENSION Y(*)
      DIMENSION PX1COO(*)
      DIMENSION X1COOR(*)
      DIMENSION PX1CMN(*)
      DIMENSION X1COMN(*)
      DIMENSION PY1COO(*)
      DIMENSION Y1COOR(*)
      DIMENSION PY1CMN(*)
      DIMENSION Y1COMN(*)
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IFRALI
      CHARACTER*4 IXMIN
      CHARACTER*4 IXMAX
      CHARACTER*4 IX1TSC
      CHARACTER*4 IX1TSW
      CHARACTER*4 IX1JSW
      CHARACTER*4 IX1NSW
      CHARACTER*4 IY1TSC
      CHARACTER*4 IY1TSW
      CHARACTER*4 IY1NSW
      CHARACTER*4 IY1JSW
      CHARACTER*4 ITICUN
      CHARACTER*4 IBUGG4
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
C-----COMMON----------------------------------------------------------
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBRO.EQ.'FRLI')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPFRLI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)GX1MIN,GX1MAX,GY1MIN,GY1MAX
   54   FORMAT('GX1MIN,GX1MAX,GY1MIN,GY1MAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,58)IX1TSC,IX1TSW,IY1TSC,IY1TSW
   58   FORMAT('IX1TSC,IX1TSW,IY1TSC,IY1TSW = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,59)IX1TSC,IY1TSC,NMJX1T,NMJY1T
   59   FORMAT('IX1TSC,IY1TSC,NMJX1T,NMJY1T = ',2(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,60)PX1TOL,PX1TOR,PY1TOL,PY1TOR
   60   FORMAT('PX1TOL,PX1TOR,PY1TOL,PY1TOR = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,61)PXMIN,PXMAX,PYMIN,PYMAX
   61   FORMAT('PXMIN,PXMAX,PYMIN,PYMAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,69)ICASPL,IFRALI,IBUGG4,ISUBRO
   69   FORMAT('ICASPL,IFRALI,IBUGG4,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IERROR='NO'
C
C               *******************************************************
C               **  STEP 1--                                         **
C               **  DETERMINE THE DATA LIMITS.  UNLIKE DPDEDL, WE    **
C               **  IGNORE ANY PRE-SET LIMITS AND WE DO NOT MAKE ANY **
C               **  ADJUSTMENTS BASED ON PLOT TYPE (E.G., BAR PLOTS).**
C               *******************************************************
C
      CALL SORT(Y,N,Y)
      DMIN=Y(1)
      DMAX=Y(N)
C
C               *******************************************************
C               **  STEP 1--                                         **
C               **  DETERMINE FRAME LIMITS ON BOTTOM HORIZONTAL AXIS **
C               *******************************************************
C
      IXMIN='FLOA'
      IXMAX='FLOA'
C
      IF(ICASPL.EQ.'X')THEN
        CALL DPDEF2(DMIN,DMAX,GX1MIN,GX1MAX,IXMIN,IXMAX,IX1TSC,
     1              FMIN,FMAX,NMJX1T)
        IF(IFRALI.EQ.'ON')THEN
          CALL DPDET2(PXMIN,PXMAX,FMIN,FMAX,
     1                IX1TSW,IX1TSC,
     1                NMJX1T,IX1JSW,
     1                PX1COO,X1COOR,NX1COO,
     1                NMNX1T,IX1NSW,
     1                PX1CMN,X1COMN,NX1CMN,
     1                PX1TOL,PX1TOR,ITICUN)
        ENDIF
      ELSE
        CALL DPDEF2(DMIN,DMAX,GY1MIN,GY1MAX,IXMIN,IXMAX,IY1TSC,
     1              FMIN,FMAX,NMJY1T)
        IF(IFRALI.EQ.'ON')THEN
          CALL DPDET2(PXMIN,PXMAX,FMIN,FMAX,
     1                IY1TSW,IY1TSC,
     1                NMJY1T,IY1JSW,
     1                PY1COO,Y1COOR,NY1COO,
     1                NMNY1T,IY1NSW,
     1                PY1CMN,Y1COMN,NY1CMN,
     1                PY1TOL,PY1TOR,ITICUN)
        ENDIF
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBRO.EQ.'FRLI')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFRLI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9052)DMIN,DMAX,FMIN,FMAX
 9052   FORMAT('DMIN,DMAX,FMIN,FMAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFRPA(ICOM,IHARG,IHARG2,NUMARG,
CCCCC AUGUST 1995.  ADD IHARG2 FOR DASH2, ETC
CCCCC SUBROUTINE DPFRPA(ICOM,IHARG,NUMARG,
     1IDEFPA,
     1IX1FPA,IX2FPA,IY1FPA,IY2FPA,
     1IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE FRAME PATTERN SWITCHES
C              FOR ANY OF THE 4 FRAME LINES.
C              SUCH FRAME PATTERN SWITCHES DEFINE THE PATTERN
C              FOR EACH OF THE 4 FRAME LINES.
C              THE CONTENTS OF A FRAME PATTERN SWITCH ARE
C              A PATTERN.
C              THE FRAME PATTERN SWITCHES FOR THE 4 FRAME LINES
C              ARE CONTAINED IN THE 4 VARIABLES
C              IX1FPA,IX2FPA,IY1FPA,IY2FPA.
C     INPUT  ARGUMENTS--ICOM
C                     --IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG
C                     --IDEFPA
C     OUTPUT ARGUMENTS--IX1FPA (A HOLLERITH VECTOR)
C                     --IX2FPA (A HOLLERITH VECTOR)
C                     --IY1FPA (A HOLLERITH VECTOR)
C                     --IY2FPA (A HOLLERITH VECTOR)
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--ALAN HECKERT
C                 COMPUTER SERVICES DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--OCTOBER   1980.
C     UPDATED         --MAY       1982.
C     UPDATED         --AUGUST    1995.  DASH2 BUG
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
CCCCC AUGUST 1995.  ADD FOLLOWING LINE
      CHARACTER*4 IHARG2
      CHARACTER*4 IDEFPA
C
      CHARACTER*4 IX1FPA
      CHARACTER*4 IX2FPA
      CHARACTER*4 IY1FPA
      CHARACTER*4 IY2FPA
C
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
CCCCC AUGUST 1995.  ADD FOLLOWING LINE
      DIMENSION IHARG2(*)
C
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
      IF(NUMARG.LE.0)GOTO1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'PATT')GOTO1090
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'PATT')GOTO1090
      GOTO1900
 1090 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH HORIZONTAL FRAMES    ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'XFRA')GOTO1100
      GOTO1199
C
 1100 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      IF(IHARG(NUMARG).EQ.'PATT')GOTO1150
      GOTO1160
C
 1150 CONTINUE
      IHOLD=IDEFPA
      GOTO1180
C
 1160 CONTINUE
      IHOLD=IHARG(NUMARG)
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(NUMARG).EQ.'2')IHOLD='DA2'
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(NUMARG).EQ.'3')IHOLD='DA3'
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(NUMARG).EQ.'4')IHOLD='DA4'
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(NUMARG).EQ.'5')IHOLD='DA5'
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      IX1FPA=IHOLD
      IX2FPA=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE FRAME PATTERN (FOR BOTH HORIZONTAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)IHOLD
 1182 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1900
C
 1199 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE BOTTOM HORIZONTAL FRAME IS      TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X1FR')GOTO1200
      GOTO1299
C
 1200 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1250
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1250
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1250
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1250
      IF(IHARG(NUMARG).EQ.'PATT')GOTO1250
      GOTO1260
C
 1250 CONTINUE
      IHOLD=IDEFPA
      GOTO1280
C
 1260 CONTINUE
      IHOLD=IHARG(NUMARG)
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(NUMARG).EQ.'2')IHOLD='DA2'
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(NUMARG).EQ.'3')IHOLD='DA3'
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(NUMARG).EQ.'4')IHOLD='DA4'
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(NUMARG).EQ.'5')IHOLD='DA5'
      GOTO1280
C
 1280 CONTINUE
      IFOUND='YES'
      IX1FPA=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1289
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1281)
 1281 FORMAT('THE FRAME PATTERN (FOR THE BOTTOM HORIZONTAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1282)IHOLD
 1282 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1289 CONTINUE
      GOTO1900
C
 1299 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE TOP    HORIZONTAL FRAME IS      TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X2FR')GOTO1300
      GOTO1399
C
 1300 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1350
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1350
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1350
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1350
      IF(IHARG(NUMARG).EQ.'PATT')GOTO1350
      GOTO1360
C
 1350 CONTINUE
      IHOLD=IDEFPA
      GOTO1380
C
 1360 CONTINUE
      IHOLD=IHARG(NUMARG)
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(NUMARG).EQ.'2')IHOLD='DA2'
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(NUMARG).EQ.'3')IHOLD='DA3'
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(NUMARG).EQ.'4')IHOLD='DA4'
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(NUMARG).EQ.'5')IHOLD='DA5'
      GOTO1380
C
 1380 CONTINUE
      IFOUND='YES'
      IX2FPA=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1389
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1381)
 1381 FORMAT('THE FRAME PATTERN (FOR THE TOP HORIZONTAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1382)IHOLD
 1382 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1389 CONTINUE
      GOTO1900
C
 1399 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH VERTICAL   FRAMES    ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'YFRA')GOTO1400
      GOTO1499
C
 1400 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1450
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1450
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1450
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1450
      IF(IHARG(NUMARG).EQ.'PATT')GOTO1450
      GOTO1460
C
 1450 CONTINUE
      IHOLD=IDEFPA
      GOTO1480
C
 1460 CONTINUE
      IHOLD=IHARG(NUMARG)
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(NUMARG).EQ.'2')IHOLD='DA2'
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(NUMARG).EQ.'3')IHOLD='DA3'
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(NUMARG).EQ.'4')IHOLD='DA4'
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(NUMARG).EQ.'5')IHOLD='DA5'
      GOTO1480
C
 1480 CONTINUE
      IFOUND='YES'
      IY1FPA=IHOLD
      IY2FPA=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1489
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1481)
 1481 FORMAT('THE FRAME PATTERN (FOR BOTH VERTICAL ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1482)IHOLD
 1482 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1489 CONTINUE
      GOTO1900
C
 1499 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE LEFT   VERTICAL   FRAME IS      TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y1FR')GOTO1500
      GOTO1599
C
 1500 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1550
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1550
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1550
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1550
      IF(IHARG(NUMARG).EQ.'PATT')GOTO1550
      GOTO1560
C
 1550 CONTINUE
      IHOLD=IDEFPA
      GOTO1580
C
 1560 CONTINUE
      IHOLD=IHARG(NUMARG)
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(NUMARG).EQ.'2')IHOLD='DA2'
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(NUMARG).EQ.'3')IHOLD='DA3'
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(NUMARG).EQ.'4')IHOLD='DA4'
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(NUMARG).EQ.'5')IHOLD='DA5'
      GOTO1580
C
 1580 CONTINUE
      IFOUND='YES'
      IY1FPA=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1589
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1581)
 1581 FORMAT('THE FRAME PATTERN (FOR THE LEFT VERTICAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1582)IHOLD
 1582 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1589 CONTINUE
      GOTO1900
C
 1599 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE RIGHT  VERTICAL   FRAME IS      TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y2FR')GOTO1600
      GOTO1699
C
 1600 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1650
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1650
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1650
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1650
      IF(IHARG(NUMARG).EQ.'PATT')GOTO1650
      GOTO1660
C
 1650 CONTINUE
      IHOLD=IDEFPA
      GOTO1680
C
 1660 CONTINUE
      IHOLD=IHARG(NUMARG)
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(NUMARG).EQ.'2')IHOLD='DA2'
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(NUMARG).EQ.'3')IHOLD='DA3'
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(NUMARG).EQ.'4')IHOLD='DA4'
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(NUMARG).EQ.'5')IHOLD='DA5'
      GOTO1680
C
 1680 CONTINUE
      IFOUND='YES'
      IY2FPA=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1689
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1681)
 1681 FORMAT('THE FRAME PATTERN (FOR THE RIGHT VERTICAL ',
     1'FRAME LINE)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1682)IHOLD
 1682 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1689 CONTINUE
      GOTO1900
C
 1699 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  ALL 4 FRAME FRAME LINES ARE TO BE CHANGED      **
C               *****************************************************
C
      IF(ICOM.EQ.'FRAM')GOTO1700
      IF(ICOM.EQ.'XYFR')GOTO1700
      IF(ICOM.EQ.'YXFR')GOTO1700
      GOTO1799
C
 1700 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1750
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1750
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1750
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1750
      IF(IHARG(NUMARG).EQ.'PATT')GOTO1750
      GOTO1760
C
 1750 CONTINUE
      IHOLD=IDEFPA
      GOTO1780
C
 1760 CONTINUE
      IHOLD=IHARG(NUMARG)
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(NUMARG).EQ.'2')IHOLD='DA2'
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(NUMARG).EQ.'3')IHOLD='DA3'
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(NUMARG).EQ.'4')IHOLD='DA4'
      IF(IHOLD.EQ.'DASH'.AND.IHARG2(NUMARG).EQ.'5')IHOLD='DA5'
      GOTO1780
C
 1780 CONTINUE
      IFOUND='YES'
      IX1FPA=IHOLD
      IX2FPA=IHOLD
      IY1FPA=IHOLD
      IY2FPA=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1789
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1781)
 1781 FORMAT('THE FRAME PATTERN (FOR ALL 4 ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1782)IHOLD
 1782 FORMAT('HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1789 CONTINUE
      GOTO1900
C
 1799 CONTINUE
C
 1900 CONTINUE
      RETURN
      END
      SUBROUTINE DPFRTE(XTEMP1,MAXNXT,
     1                  ICASAN,ICAPSW,IFORSW,
     1                  IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--PERFORM EITHER A FREQUENCY OR FREQUENCY WITHIN A BLOCK
C              TEST FOR RANDOMNESS
C     EXAMPLE--FREQUENCY TEST Y
C              FREQUENCY WITHIN A BLOCK TEST Y
C     REFERENCE--A STATISTICAL TEST SUITE FOR RANDOM AND PSUEDORANDOM
C                NUMBER GENERATORS FOR CRYPTOGRAPHIC APPLICATIONS,
C                ANDREW RUHKIN, JUAN SOTO, JAMES NECHVATAL, MILES SMID,
C                ELAINE BARKER, STEFAN LEIGH, MARK LEVENSON,
C                MARK VANGEL, DAVID BANKS, ALAN HECKERT, JAMES DRAY,
C                SAN VO.  NIST SPECIAL PUBLICATION 800-22,
C                OCTOBER 2000, PP. 14-16.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     EXAMPLE--TOLERANCE LIMITS Y
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--98/11
C     VERSION NUMBER--2003/11
C     ORIGINAL VERSION--NOVEMBER  2003.
C     UPDATED         --MARCH     2011. USE DPPARS ROUTINE
C     UPATED          --MARCH     2011. REWRITTEN TO HANDLE MULTIPLE
C                                       RESPONSE VARIABLES, GROUP-ID
C                                       VARIABLES, OR A LAB-ID VARIABLE
C     UPATED          --JULY      2019. TWEAK HOW SCRATCH STORAGE
C                                       HANDLED
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
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IREPL
      CHARACTER*4 IMULT
      CHARACTER*4 ICTMP1
      CHARACTER*4 ICTMP2
      CHARACTER*4 ICTMP3
      CHARACTER*4 ICTMP4
      CHARACTER*4 ICTMP5
      CHARACTER*4 ICASE
C
      CHARACTER*4 IFLAGU
      LOGICAL IFRST
      LOGICAL ILAST
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
      INCLUDE 'DPCOZZ.INC'
C
      DIMENSION XTEMP1(*)
      DIMENSION YTEMP1(MAXOBV)
      DIMENSION YTEMP2(MAXOBV)
      DIMENSION XDESGN(MAXOBV,7)
      DIMENSION XIDTEM(MAXOBV)
      DIMENSION XIDTE2(MAXOBV)
      DIMENSION XIDTE3(MAXOBV)
      DIMENSION XIDTE4(MAXOBV)
      DIMENSION XIDTE5(MAXOBV)
      DIMENSION XIDTE6(MAXOBV)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
C
      EQUIVALENCE (GARBAG(IGARB1),YTEMP1(1))
      EQUIVALENCE (GARBAG(IGARB2),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB3),XIDTEM(1))
      EQUIVALENCE (GARBAG(IGARB4),XIDTE2(1))
      EQUIVALENCE (GARBAG(IGARB5),XIDTE3(1))
      EQUIVALENCE (GARBAG(IGARB6),XIDTE4(1))
      EQUIVALENCE (GARBAG(IGARB7),XIDTE5(1))
      EQUIVALENCE (GARBAG(IGARB8),XIDTE6(1))
      EQUIVALENCE (GARBAG(IGARB9),YTEMP2(1))
      EQUIVALENCE (GARBAG(IGAR10),TEMP2(1))
      EQUIVALENCE (GARBAG(JGAR11),XDESGN(1,1))
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
      ICASAN='FRTE'
      IREPL='OFF'
      IMULT='OFF'
      ISUBN1='DPFR'
      ISUBN2='TE  '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
      NTOT=0
C
C               ***********************************************
C               **  TREAT THE FREQUENCY        TEST  CASE    **
C               ***********************************************
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'FRTE')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPFRTE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGA2,IBUGA3,IBUGQ,ISUBRO
   53   FORMAT('IBUGA2,IBUGA3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************************************************
C               **  STEP 1--                                       **
C               **  EXTRACT THE COMMAND                            **
C               **  LOOK FOR ONE OF THE FOLLOWING COMMANDS:        **
C               **    1) FREQUENCY TEST   Y                        **
C               **    2) MULTIPLE FREQUENCY TEST   Y1 ... YK       **
C               **    3) REPLICATED FREQUENCY TEST   Y X1 ... XK   **
C               *****************************************************
C
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FRTE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ILASTZ=0
      ICASAN='FRTE'
C
C     LOOK FOR:
C
C          FREQUENCY TEST
C          FREQUENCY WITHIN A BLOCK TEST
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
        ICTMP5=IHARG(I+4)
C
        IF(ICTMP1.EQ.'=')THEN
          IFOUND='NO'
          GOTO9000
        ELSEIF(ICTMP1.EQ.'FREQ' .AND. ICTMP2.EQ.'TEST')THEN
          IFOUND='YES'
          ICASAN='FRTE'
          ILASTZ=I+1
        ELSEIF(ICTMP1.EQ.'FREQ' .AND. ICTMP2.EQ.'WITH' .AND.
     1         ICTMP3.EQ.'A   ' .AND. ICTMP4.EQ.'BLOC' .AND.
     1         ICTMP5.EQ.'TEST')THEN
          IFOUND='YES'
          ICASAN='FBTE'
          ILASTZ=I+4
        ELSEIF(ICTMP1.EQ.'FREQ' .AND. ICTMP2.EQ.'WITH' .AND.
     1         ICTMP3.EQ.'BLOC' .AND. ICTMP4.EQ.'TEST')THEN
          IFOUND='YES'
          ICASAN='FBTE'
          ILASTZ=I+3
        ELSEIF(ICTMP1.EQ.'FREQ' .AND. ICTMP2.EQ.'BLOC' .AND.
     1         ICTMP3.EQ.'TEST')THEN
          IFOUND='YES'
          ICASAN='FBTE'
          ILASTZ=I+2
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FRTE')THEN
        WRITE(ICOUT,91)ICASAN,IMULT,IREPL,ISHIFT
   91   FORMAT('DPFRTE: ICASAN,IMULT,IREPL,ISHIFT = ',3(A4,2X),I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IMULT.EQ.'ON')THEN
        IF(IREPL.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          IF(ICASAN.EQ.'FRTE')THEN
            WRITE(ICOUT,101)
  101       FORMAT('***** ERROR IN FREQUENCY TEST--')
            CALL DPWRST('XXX','BUG ')
          ELSE
            WRITE(ICOUT,102)
  102       FORMAT('***** ERROR IN FREQUENCY WITHIN A BLOCK TEST--')
            CALL DPWRST('XXX','BUG ')
          ENDIF
          WRITE(ICOUT,103)
  103     FORMAT('      YOU CANNOT SPECIFY BOTH "MULTIPLE" AND ',
     1           '"REPLICATION"')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,104)
  104     FORMAT('      FOR THE FREQUENCY TEST COMMAND.')
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FRTE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASAN.EQ.'FBTE')THEN
        INAME='FREQUENCY WITHIN A BLOCK TEST'
      ELSE
        INAME='FREQUENCY TEST'
      ENDIF
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FRTE')THEN
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FRTE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
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
          IF(ICASAN.EQ.'FRTE')THEN
            WRITE(ICOUT,101)
          ELSE
            WRITE(ICOUT,102)
          ENDIF
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FRTE')THEN
        WRITE(ICOUT,521)NRESP,NREPL
  521   FORMAT('NRESP,NREPL = ',2I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(ICASAN.EQ.'FBTE')THEN
        IH='M   '
        IH2='    '
        IHWUSE='P'
        MESSAG='NO'
        CALL CHECKN(IH,IH2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
        IF(IERROR.EQ.'NO')THEN
          AM=VALUE(ILOCP)
          M=INT(AM+0.5)
        ELSE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,102)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5811)
 5811     FORMAT('      THE DESIRED BLOCK SIZE WAS NOT SET.  TO SET ')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5813)
 5813     FORMAT('      THE BLOCK SIZE, ENTER THE COMMAND')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5814)
 5814     FORMAT('      LET M = value')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
        IF(M.LT.20)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5821)
 5821     FORMAT('***** WARNING: FOR THE FREQUENCY WITHIN A BLOCK ',
     1           'TEST, THE ')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5822)
 5822     FORMAT('      RECOMMENDATION FOR THE MINIMUM BLOCK SIZE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5823)M
 5823     FORMAT('      IS 20.  THE SPECIFIED BLOCK SIZE IS ',I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
C               ******************************************************
C               **  STEP 6--                                        **
C               **  GENERATE THE FREQUENCY        TEST FOR THE      **
C               **  VARIOUS CASES                                   **
C               ******************************************************
C
      ISTEPN='6'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FRTE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               ******************************************
C               **  STEP 8A--                           **
C               **  CASE 1: NO REPLICATION VARIABLES    **
C               ******************************************
C
      IF(NREPL.LT.1)THEN
        ISTEPN='8A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FRTE')
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
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FRTE')THEN
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
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'FRTE')THEN
            ISTEPN='8B'
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,822)
  822       FORMAT('***** FROM THE MIDDLE  OF DPFRTE--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,823)ICASAN,NUMVAR,NS1
  823       FORMAT('ICASAN,NUMVAR,NQ = ',A4,2I8)
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
          CALL DPFRT2(Y,NS1,
     1                XTEMP1,MAXNXT,
     1                ICAPSW,ICAPTY,IFORSW,ICASAN,M,
     1                PID,IVARID,IVARI2,NREPL,
     1                STATVA,STATCD,PVAL,
     1                CUT0,CUT50,CUT75,CUT90,CUT95,
     1                CUT975,CUT99,CUT999,
     1                YTEMP1,YTEMP2,
     1                ISUBRO,IBUGA3,IERROR)
C
C               ***************************************
C               **  STEP 8C--                        **
C               **  UPDATE INTERNAL DATAPLOT TABLES  **
C               ***************************************
C
          ISTEPN='8C'
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FRTE')
     1      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
          IF(NRESP.GT.1)THEN
            IFLAGU='FILE'
          ELSE
            IFLAGU='ON'
          ENDIF
          IFRST=.FALSE.
          ILAST=.FALSE.
          IF(IRESP.EQ.1)IFRST=.TRUE.
          IF(IRESP.EQ.NRESP)ILAST=.TRUE.
          CALL DPFRT5(STATVA,STATCD,PVAL,
     1                CUT0,CUT50,CUT75,CUT90,CUT95,
     1                CUT975,CUT99,CUT999,
     1                IFLAGU,IFRST,ILAST,
     1                IBUGA2,IBUGA3,ISUBRO,IERROR)
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
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FRTE')
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
C       **  CALL DPFRT2 TO PERFORM FREQUENCY        TEST.  **
C       *****************************************************
C
C
        IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'FRTE')THEN
          ISTEPN='9C'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,941)
  941     FORMAT('***** FROM THE MIDDLE  OF DPFRTE--')
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
              CALL DPFRT2(TEMP1,NTEMP,
     1                    XTEMP1,MAXNXT,
     1                    ICAPSW,ICAPTY,IFORSW,ICASAN,M,
     1                    PID,IVARN1,IVARN2,NREPL,
     1                    STATVA,STATCD,PVAL,
     1                    CUT0,CUT50,CUT75,CUT90,CUT95,
     1                    CUT975,CUT99,CUT999,
     1                    YTEMP1,YTEMP2,
     1                    ISUBRO,IBUGA3,IERROR)
              IFLAGU='FILE'
              IFRST=.FALSE.
              ILAST=.FALSE.
              IF(NCURVE.EQ.1)IFRST=.TRUE.
              IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
              CALL DPFRT5(STATVA,STATCD,PVAL,
     1                    CUT0,CUT50,CUT75,CUT90,CUT95,
     1                    CUT975,CUT99,CUT999,
     1                    IFLAGU,IFRST,ILAST,
     1                    IBUGA2,IBUGA3,ISUBRO,IERROR)
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
              CALL DPFRT2(TEMP1,NTEMP,
     1                    XTEMP1,MAXNXT,
     1                    ICAPSW,ICAPTY,IFORSW,ICASAN,M,
     1                    PID,IVARN1,IVARN2,NREPL,
     1                    STATVA,STATCD,PVAL,
     1                    CUT0,CUT50,CUT75,CUT90,CUT95,
     1                    CUT975,CUT99,CUT999,
     1                    YTEMP1,YTEMP2,
     1                    ISUBRO,IBUGA3,IERROR)
              IFLAGU='FILE'
              IFRST=.FALSE.
              ILAST=.FALSE.
              IF(NCURVE.EQ.1)IFRST=.TRUE.
              IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
              CALL DPFRT5(STATVA,STATCD,PVAL,
     1                    CUT0,CUT50,CUT75,CUT90,CUT95,
     1                    CUT975,CUT99,CUT999,
     1                    IFLAGU,IFRST,ILAST,
     1                    IBUGA2,IBUGA3,ISUBRO,IERROR)
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
              CALL DPFRT2(TEMP1,NTEMP,
     1                    XTEMP1,MAXNXT,
     1                    ICAPSW,ICAPTY,IFORSW,ICASAN,M,
     1                    PID,IVARN1,IVARN2,NREPL,
     1                    STATVA,STATCD,PVAL,
     1                    CUT0,CUT50,CUT75,CUT90,CUT95,
     1                    CUT975,CUT99,CUT999,
     1                    YTEMP1,YTEMP2,
     1                    ISUBRO,IBUGA3,IERROR)
              IFLAGU='FILE'
              IFRST=.FALSE.
              ILAST=.FALSE.
              IF(NCURVE.EQ.1)IFRST=.TRUE.
              IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
              CALL DPFRT5(STATVA,STATCD,PVAL,
     1                    CUT0,CUT50,CUT75,CUT90,CUT95,
     1                    CUT975,CUT99,CUT999,
     1                    IFLAGU,IFRST,ILAST,
     1                    IBUGA2,IBUGA3,ISUBRO,IERROR)
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
              CALL DPFRT2(TEMP1,NTEMP,
     1                    XTEMP1,MAXNXT,
     1                    ICAPSW,ICAPTY,IFORSW,ICASAN,M,
     1                    PID,IVARN1,IVARN2,NREPL,
     1                    STATVA,STATCD,PVAL,
     1                    CUT0,CUT50,CUT75,CUT90,CUT95,
     1                    CUT975,CUT99,CUT999,
     1                    YTEMP1,YTEMP2,
     1                    ISUBRO,IBUGA3,IERROR)
              IFLAGU='FILE'
              IFRST=.FALSE.
              ILAST=.FALSE.
              IF(NCURVE.EQ.1)IFRST=.TRUE.
              IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
              CALL DPFRT5(STATVA,STATCD,PVAL,
     1                    CUT0,CUT50,CUT75,CUT90,CUT95,
     1                    CUT975,CUT99,CUT999,
     1                    IFLAGU,IFRST,ILAST,
     1                    IBUGA2,IBUGA3,ISUBRO,IERROR)
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
              CALL DPFRT2(TEMP1,NTEMP,
     1                    XTEMP1,MAXNXT,
     1                    ICAPSW,ICAPTY,IFORSW,ICASAN,M,
     1                    PID,IVARN1,IVARN2,NREPL,
     1                    STATVA,STATCD,PVAL,
     1                    CUT0,CUT50,CUT75,CUT90,CUT95,
     1                    CUT975,CUT99,CUT999,
     1                    YTEMP1,YTEMP2,
     1                    ISUBRO,IBUGA3,IERROR)
              IFLAGU='FILE'
              IFRST=.FALSE.
              ILAST=.FALSE.
              IF(NCURVE.EQ.1)IFRST=.TRUE.
              IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
              CALL DPFRT5(STATVA,STATCD,PVAL,
     1                    CUT0,CUT50,CUT75,CUT90,CUT95,
     1                    CUT975,CUT99,CUT999,
     1                    IFLAGU,IFRST,ILAST,
     1                    IBUGA2,IBUGA3,ISUBRO,IERROR)
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
              CALL DPFRT2(TEMP1,NTEMP,
     1                    XTEMP1,MAXNXT,
     1                    ICAPSW,ICAPTY,IFORSW,ICASAN,M,
     1                    PID,IVARN1,IVARN2,NREPL,
     1                    STATVA,STATCD,PVAL,
     1                    CUT0,CUT50,CUT75,CUT90,CUT95,
     1                    CUT975,CUT99,CUT999,
     1                    YTEMP1,YTEMP2,
     1                    ISUBRO,IBUGA3,IERROR)
              IFLAGU='FILE'
              IFRST=.FALSE.
              ILAST=.FALSE.
              IF(NCURVE.EQ.1)IFRST=.TRUE.
              IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
              CALL DPFRT5(STATVA,STATCD,PVAL,
     1                    CUT0,CUT50,CUT75,CUT90,CUT95,
     1                    CUT975,CUT99,CUT999,
     1                    IFLAGU,IFRST,ILAST,
     1                    IBUGA2,IBUGA3,ISUBRO,IERROR)
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
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'FRTE')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFRTE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR,ICASAN
 9012   FORMAT('IFOUND,IERROR,ICASAN = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFRT2(Y,N,
     1                  XTEMP,MAXNXT,
     1                  ICAPSW,ICAPTY,IFORSW,ICASAN,M,
     1                  PID,IVARID,IVARI2,NREPL,
     1                  STATVA,STATCD,PVAL,
     1                  CUT0,CUT50,CUT75,CUT90,CUT95,
     1                  CUT975,CUT99,CUT999,
     1                  YTEMP1,YTEMP2,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS ROUTINE CARRIES OUT EITHER THE FREQUENCY TEST
C              FOR RANDOMNESS OR THE FREQUENCY WITHIN A BLOCK TEST
C              FOR RANDOMNESS.
C     EXAMPLE--FREQUENCY TEST Y
C              FREQUENCY WITHIN A BLOCK TEST Y
C     REFERENCE--A STATISTICAL TEST SUITE FOR RANDOM AND PSUEDORANDOM
C                NUMBER GENERATORS FOR CRYPTOGRAPHIC APPLICATIONS,
C                ANDREW RUHKIN, JUAN SOTO, JAMES NECHVATAL, MILES SMID,
C                ELAINE BARKER, STEFAN LEIGH, MARK LEVENSON,
C                MARK VANGEL, DAVID BANKS, ALAN HECKERT, JAMES DRAY,
C                SAN VO.  NIST SPECIAL PUBLICATION 800-22,
C                OCTOBER 2000, PP. 14-18.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2003/11
C     ORIGINAL VERSION--NOVEMBER  2003.
C     UPDATED         --MARCH     2011. USE DPDTA1 AND DPDTA5 TO PRINT
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
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION XTEMP(*)
      DIMENSION YTEMP1(*)
      DIMENSION YTEMP2(*)
      DIMENSION PID(*)
C
      DOUBLE PRECISION DRESLT
      DOUBLE PRECISION DGAMIP
C
      PARAMETER (NUMALP=7)
C
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
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPFR'
      ISUBN2='T2  '
      IERROR='NO'
C
      SN=0.0
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FRT2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPFRT2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)N,MAXNXT,ICASAN,IBUGA3,ISUBRO
   52   FORMAT('N,MAXNXT,ICASAN,IBUGA3,ISUBRO = ',2I8,3(A4,2X))
        CALL DPWRST('XXX','WRIT')
        DO56I=1,N
          WRITE(ICOUT,57)I,Y(I)
   57     FORMAT('I,Y(I) = ',I8,G15.7)
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
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FRT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N.LE.5)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1111)
 1111   FORMAT('***** ERROR IN FREQUENCY TEST FOR RANDOMNESS.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1113)
 1113   FORMAT('      AT LEAST SIX OBSERVATIONS REQUIRED.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1115)N
 1115   FORMAT('SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               *******************************
C               **  STEP 2--                 **
C               **  COMPUTE THE NUMBER OF    **
C               **  DISTINCT VALUES.         **
C               *******************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FRT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IWRITE='NO'
      CALL DISTIN(Y,N,IWRITE,YTEMP1,NDIST,IBUGA3,IERROR)
C
      IF(IERROR.EQ.'YES')GOTO9000
      IF(NDIST.GT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2001)
 2001   FORMAT('***** ERROR IN FREQUENCY RANDOMNESS TEST.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2003)
 2003   FORMAT('      FOR FREQUENCY TEST, AT MOST TWO DISTINCT ',
     1         'VALUES ARE ALLOWED.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2005)NDIST
 2005   FORMAT('      NUMBER OF DISTINCT VALUES = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(ICASAN.EQ.'FRTE')GOTO2000
      IF(ICASAN.EQ.'FBTE')GOTO3000
C
 2000 CONTINUE
      IF(NDIST.EQ.1)THEN
        DO2010I=1,N
          YTEMP2(I)=1.0
 2010   CONTINUE
      ELSE
        ALOW=MIN(YTEMP1(1),YTEMP1(2))
        AHIGH=MAX(YTEMP1(1),YTEMP1(2))
        SN=0.0
        DO2020I=1,N
          IF(Y(I).EQ.ALOW)THEN
            SN=SN - 1.0
          ELSE
            SN=SN + 1.0
          ENDIF
 2020   CONTINUE
      ENDIF
C
C               ******************************
C               **  STEP 21--               **
C               **  CARRY OUT CALCULATIONS  **
C               **  FOR FREQUENCY     TEST  **
C               ******************************
C
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FRT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IWRITE='OFF'
C
      STATVA=ABS(SN)/SQRT(REAL(N))
C
      ARG1=STATVA
      CALL NORCDF(ARG1,RESULT)
      TERM=2.0*RESULT-1.0
      PVAL=1.0-TERM
      STATCD=RESULT
C
      CUT0=0.
C
      ALPHA=.5
      P2=1.0-ALPHA/2.0
      CALL NORPPF(P2,CUT50)
C
      ALPHA=.25
      P2=1.0-ALPHA/2.0
      CALL NORPPF(P2,CUT75)
C
      ALPHA=.10
      P2=1.0-ALPHA/2.0
      CALL NORPPF(P2,CUT90)
C
      ALPHA=.05
      P2=1.0-ALPHA/2.0
      CALL NORPPF(P2,CUT95)
C
      ALPHA=.025
      P2=1.0-ALPHA/2.0
      CALL NORPPF(P2,CUT975)
C
      ALPHA=.01
      P2=1.0-ALPHA/2.0
      CALL NORPPF(P2,CUT99)
C
      ALPHA=.001
      P2=1.0-ALPHA/2.0
      CALL NORPPF(P2,CUT999)
C
C               *********************************
C               **   STEP 52--                 **
C               **   WRITE OUT EVERYTHING      **
C               **   FOR FREQUENCY TEST        **
C               *********************************
C
      ISTEPN='22'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FRT2')
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
      ITITLE='Frequency Test for Randomness'
      NCTITL=29
      ITITLZ=' '
      NCTITZ=0
C
      ICNT=1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
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
      ITEXT(ICNT)='H0: The Data Are Random'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: The Data Are Not Random'
      NCTEXT(ICNT)=27
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
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
      ITEXT(ICNT)='Sum of +1 and -1 Values:'
      NCTEXT(ICNT)=24
      AVALUE(ICNT)=SN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Frequency Test Statistic:'
      NCTEXT(ICNT)=25
      AVALUE(ICNT)=STATVA
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='CDF Value:'
      NCTEXT(ICNT)=10
      AVALUE(ICNT)=STATCD
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='P-Value:'
      NCTEXT(ICNT)=8
      AVALUE(ICNT)=PVAL
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      NUMROW=ICNT
      DO2110I=1,NUMROW
        NTOT(I)=15
 2110 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
C
      ISTEPN='42A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FRT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1            AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ISTEPN='42D'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FRT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ITITL9=' '
      NCTIT9=0
      ITITLE='Conclusions'
      NCTITL=11
C
      DO5030J=1,5
        DO5040I=1,3
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
 5040   CONTINUE
 5030 CONTINUE
C
      ITITL2(2,1)='Null'
      NCTIT2(2,1)=4
      ITITL2(3,1)='Hypothesis'
      NCTIT2(3,1)=10
C
      ITITL2(2,2)='Confidence'
      NCTIT2(2,2)=10
      ITITL2(3,2)='Level'
      NCTIT2(3,2)=5
C
      ITITL2(2,3)='Test'
      NCTIT2(2,3)=4
      ITITL2(3,3)='Statistic'
      NCTIT2(3,3)=9
C
      ITITL2(2,4)='Critical'
      NCTIT2(2,4)=8
      ITITL2(3,4)='Value (+/-)'
      NCTIT2(3,4)=11
C
      ITITL2(1,5)='Null'
      NCTIT2(1,5)=4
      ITITL2(2,5)='Hypothesis'
      NCTIT2(2,5)=10
      ITITL2(3,5)='Conclusion'
      NCTIT2(3,5)=10
C
      NMAX=0
      NUMCOL=5
      DO2050I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IF(I.EQ.1)NTOT(I)=12
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='ALPH'
        IF(I.EQ.3 .OR. I.EQ.4)ITYPCO(I)='NUME'
        IDIGIT(I)=NUMDIG
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
        IWRTF(3)=IWRTF(2)+IINC3
        IWRTF(4)=IWRTF(3)+IINC3
        IWRTF(5)=IWRTF(4)+IINC3
C
        DO2060J=1,NUMALP
C
          AMAT(J,I)=0.0
          AMAT(J,3)=STATVA
          IVALUE(J,1)='Random'
          NCVALU(J,1)=6
          IVALUE(J,5)(1:6)='REJECT'
          IF(J.EQ.1)THEN
            IVALUE(J,2)(1:5)='50.0%'
            AMAT(J,4)=CUT50
            IF(STATVA.LT.CUT50)IVALUE(J,5)(1:6)='ACCEPT'
          ELSEIF(J.EQ.2)THEN
            IVALUE(J,2)(1:5)='75.0%'
            AMAT(J,4)=CUT75
            IF(STATVA.LT.CUT75)IVALUE(J,5)(1:6)='ACCEPT'
          ELSEIF(J.EQ.3)THEN
            IVALUE(J,2)(1:5)='90.0%'
            AMAT(J,4)=CUT90
            IF(STATVA.LT.CUT90)IVALUE(J,5)(1:6)='ACCEPT'
          ELSEIF(J.EQ.4)THEN
            IVALUE(J,2)(1:5)='95.0%'
            AMAT(J,4)=CUT95
            IF(STATVA.LT.CUT95)IVALUE(J,5)(1:6)='ACCEPT'
          ELSEIF(J.EQ.5)THEN
            IVALUE(J,2)(1:5)='97.5%'
            AMAT(J,4)=CUT975
            IF(STATVA.LT.CUT975)IVALUE(J,5)(1:6)='ACCEPT'
          ELSEIF(J.EQ.6)THEN
            IVALUE(J,2)(1:5)='99.0%'
            AMAT(J,4)=CUT99
            IF(STATVA.LT.CUT99)IVALUE(J,5)(1:6)='ACCEPT'
          ELSEIF(J.EQ.7)THEN
            IVALUE(J,2)(1:5)='99.9%'
            AMAT(J,4)=CUT999
            IF(STATVA.LT.CUT999)IVALUE(J,5)(1:6)='ACCEPT'
          ENDIF
          NCVALU(J,2)=5
          NCVALU(J,5)=6
C
 2060   CONTINUE
 2050 CONTINUE
C
      ICNT=NUMALP
      NUMLIN=3
      NUMCOL=5
      IFRST=.TRUE.
      ILAST=.TRUE.
      IFLAGS=.TRUE.
      IFLAGE=.TRUE.
      CALL DPDTA5(ITITLE,NCTITL,
     1            ITITL9,NCTIT9,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            IFLAGS,IFLAGE,
     1            ISUBRO,IBUGA3,IERROR)
      GOTO9000
C
 3000 CONTINUE
C
      NBLOCK=N/M
      AMNSZ=0.01*REAL(N)
C
      IF(NBLOCK.GE.100)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3011)
 3011   FORMAT('***** WARNING: THE NUMBER OF BLOCKS IS GREATER')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3012)
 3012   FORMAT('      THAN THE RECOMMENDED MAXIMUM OF 100.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3013)N
 3013   FORMAT('      SAMPLE SIZE       = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3014)M
 3014   FORMAT('      BLOCK SIZE        = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3015)NBLOCK
 3015   FORMAT('      NUMBER OF BLOCKS  = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(M.LE.INT(AMNSZ))THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3021)
 3021   FORMAT('***** WARNING: THE BLOCK SIZE IS LESS THAN THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3022)INT(AMNSZ)
 3022   FORMAT('      RECOMMENDED MINIMUM OF ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3023)N
 3023   FORMAT('      SAMPLE SIZE                     = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3024)M
 3024   FORMAT('      BLOCK SIZE                      = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3025)NBLOCK
 3025   FORMAT('      NUMBER OF BLOCKS                = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3026)INT(AMNSZ)
 3026   FORMAT('      RECOMMENDED MINIMUM BLOCK SIZE  = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(M.GT.N)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3031)
 3031   FORMAT('      THE BLOCK SIZE IS GREATER THAN THE SAMPLE SIZE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3033)N
 3033   FORMAT('      SAMPLE SIZE                     = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3034)M
 3034   FORMAT('      BLOCK SIZE                      = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3035)NBLOCK
 3035   FORMAT('      NUMBER OF BLOCKS                = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3036)INT(AMNSZ)
 3036   FORMAT('      RECOMMENDED MINIMUM BLOCK SIZE  = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(NDIST.NE.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3041)NDIST
 3041   FORMAT('      THE RESPONSE VARIBLE CONTAINS ',I8,' DISTINCT ',
     1         'VALUES.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      ALOW=MIN(YTEMP1(1),YTEMP1(2))
      AHIGH=MAX(YTEMP1(1),YTEMP1(2))
      AM=REAL(M)
C
      SUM=0.0
      DO3110K=1,NBLOCK
        ISTRT=(K-1)*M+1
        ISTOP=K*M
        AONES=0
        DO3120I=ISTRT,ISTOP
          IF(Y(I).EQ.AHIGH)AONES=AONES+1.0
 3120   CONTINUE
        API=AONES/AM
        SUM=SUM + (API-0.5)**2
 3110 CONTINUE
C
      STATVA=4.0*AM*SUM
      DRESLT=1.0D0 - DGAMIP(DBLE(NBLOCK)/2.0D0,DBLE(STATVA)/2.0D0)
      PVAL=REAL(DRESLT)
C
C               *********************************
C               **   STEP 32--                 **
C               **   WRITE OUT EVERYTHING      **
C               **   FOR FREQUENCY TEST        **
C               *********************************
C
      ISTEPN='32'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FRT2')
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
      ITITLE='Frequency Within a Block Test for Randomness'
      NCTITL=44
      ITITLZ=' '
      NCTITZ=0
C
      ICNT=1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='Response Variable: '
      WRITE(ITEXT(ICNT)(20:23),'(A4)')IVARID(1)(1:4)
      WRITE(ITEXT(ICNT)(24:27),'(A4)')IVARI2(1)(1:4)
      NCTEXT(ICNT)=27
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      IF(NREPL.GT.0)THEN
        IADD=1
        DO6101I=1,NREPL
          ICNT=ICNT+1
          ITEMP=I+IADD
          ITEXT(ICNT)='Factor Variable  : '
          WRITE(ITEXT(ICNT)(17:17),'(I1)')I
          WRITE(ITEXT(ICNT)(20:23),'(A4)')IVARID(ITEMP)(1:4)
          WRITE(ITEXT(ICNT)(24:27),'(A4)')IVARI2(ITEMP)(1:4)
          NCTEXT(ICNT)=27
          AVALUE(ICNT)=PID(ITEMP)
          IDIGIT(ICNT)=NUMDIG
 6101   CONTINUE
      ENDIF
C
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='H0: The Data Are Random'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: The Data Are Not Random'
      NCTEXT(ICNT)=27
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
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
      ITEXT(ICNT)='Block Size:'
      NCTEXT(ICNT)=11
      AVALUE(ICNT)=REAL(M)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Observations Within a Block:'
      NCTEXT(ICNT)=38
      AVALUE(ICNT)=REAL(NBLOCK)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Frequency Within A Block Test Statistic:'
      NCTEXT(ICNT)=40
      AVALUE(ICNT)=STATVA
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='P-Value:'
      NCTEXT(ICNT)=8
      AVALUE(ICNT)=PVAL
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      NUMROW=ICNT
      DO6110I=1,NUMROW
        NTOT(I)=15
 6110 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
C
      ISTEPN='42A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FRT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1            AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ISTEPN='42D'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FRT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ITITL9=' '
      NCTIT9=0
      ITITLE='Conclusions'
      NCTITL=11
C
      DO6130J=1,4
        DO6140I=1,3
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
 6140   CONTINUE
 6130 CONTINUE
C
      ITITL2(2,1)='Null'
      NCTIT2(2,1)=4
      ITITL2(3,1)='Hypothesis'
      NCTIT2(3,1)=10
C
      ITITL2(2,2)='Confidence'
      NCTIT2(2,2)=10
      ITITL2(3,2)='Level'
      NCTIT2(3,2)=5
C
      ITITL2(3,3)='P-Value'
      NCTIT2(3,3)=7
C
      ITITL2(1,4)='Null'
      NCTIT2(1,4)=4
      ITITL2(2,4)='Hypothesis'
      NCTIT2(2,4)=10
      ITITL2(3,4)='Conclusion'
      NCTIT2(3,4)=10
C
      NMAX=0
      NUMCOL=4
      DO6150I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IF(I.EQ.1)NTOT(I)=12
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='ALPH'
        IF(I.EQ.3)ITYPCO(I)='NUME'
        IDIGIT(I)=NUMDIG
        IWHTML(1)=150
        IWHTML(2)=125
        IWHTML(3)=150
        IWHTML(4)=150
        IINC=1600
        IINC2=1400
        IINC3=2200
        IWRTF(1)=IINC
        IWRTF(2)=IWRTF(1)+IINC
        IWRTF(3)=IWRTF(2)+IINC3
        IWRTF(4)=IWRTF(3)+IINC3
C
        DO6160J=1,NUMALP
C
          AMAT(J,I)=0.0
          AMAT(J,3)=PVAL
          IVALUE(J,1)='Random'
          NCVALU(J,1)=6
          IVALUE(J,4)(1:6)='REJECT'
          IF(J.EQ.1)THEN
            IVALUE(J,2)(1:5)='50.0%'
            IF(PVAL.GE.0.50)IVALUE(J,4)(1:6)='ACCEPT'
          ELSEIF(J.EQ.2)THEN
            IVALUE(J,2)(1:5)='75.0%'
            IF(PVAL.GE.0.25)IVALUE(J,4)(1:6)='ACCEPT'
          ELSEIF(J.EQ.3)THEN
            IVALUE(J,2)(1:5)='90.0%'
            IF(PVAL.GE.0.10)IVALUE(J,4)(1:6)='ACCEPT'
          ELSEIF(J.EQ.4)THEN
            IVALUE(J,2)(1:5)='95.0%'
            IF(PVAL.GE.0.05)IVALUE(J,4)(1:6)='ACCEPT'
          ELSEIF(J.EQ.5)THEN
            IVALUE(J,2)(1:5)='97.5%'
            IF(PVAL.GE.0.025)IVALUE(J,4)(1:6)='ACCEPT'
          ELSEIF(J.EQ.6)THEN
            IVALUE(J,2)(1:5)='99.0%'
            IF(PVAL.GE.0.01)IVALUE(J,4)(1:6)='ACCEPT'
          ELSEIF(J.EQ.7)THEN
            IVALUE(J,2)(1:5)='99.9%'
            IF(PVAL.GE.0.001)IVALUE(J,4)(1:6)='ACCEPT'
          ENDIF
          NCVALU(J,2)=5
          NCVALU(J,4)=6
C
 6160   CONTINUE
 6150 CONTINUE
C
      ICNT=NUMALP
      NUMLIN=3
      NUMCOL=4
      IFRST=.TRUE.
      ILAST=.TRUE.
      IFLAGS=.TRUE.
      IFLAGE=.TRUE.
      CALL DPDTA5(ITITLE,NCTITL,
     1            ITITL9,NCTIT9,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            IFLAGS,IFLAGE,
     1            ISUBRO,IBUGA3,IERROR)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FRT2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFRT2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9012)N,IBUGA3,IERROR
 9012   FORMAT('N,IBUGA3,IERROR = ',I8,2X,A4,2X,A4)
        CALL DPWRST('XXX','WRIT')
        DO9016I=1,N
          WRITE(ICOUT,9017)I,Y(I),XTEMP(I)
 9017     FORMAT('I,Y(I),XTEMP(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','WRIT')
 9016   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFRT3(X,N,IWRITE,XTEMP,STATVA,STATCD,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE FREQUENCY STATISTIC (AND
C              ALTERNATIVELY THE CDF VALUE).
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED OR SORTED) OBSERVATIONS.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C     OUTPUT ARGUMENTS--STATVA = THE SINGLE PRECISION VALUE OF THE
C                                COMPUTED STATISTIC.
C                     --STATCD = THE SINGLE PRECISION VALUE OF THE
C                                COMPUTED CDF OF THE TEST STATISTIC.
C     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
C             TEST STATISTIC.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--DISTIN
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2009.2
C     ORIGINAL VERSION--FEBRUARY  2009.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IWRTSV
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION XTEMP(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPFR'
      ISUBN2='T3  '
      IWRTSV=IWRITE
      IERROR='NO'
C
      SN=0.0
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FRT3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPFRT3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3
   52   FORMAT('IBUGA3 = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)N
   53   FORMAT('N = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               *******************************
C               **  COMPUTE FREQUENCY STATISTIC  **
C               *******************************
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      STATVA=-99.0
      STATCD=-99.0
      IWRITE='OFF'
C
      AN=N
C
      IF(N.LE.5)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN FREQUENCY STATISTIC--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS FOR THE ',
     1         'RESPONSE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      VARIABLE MUST BE 6 OR LARGER.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,116)
  116   FORMAT('      SUCH WAS NOT THE CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS HERE = ',I8,
     1         '.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
C               *****************************************
C               **  STEP 2--                           **
C               **  COMPUTE THE FREQUENCY STATISTIC.   **
C               *****************************************
C
      CALL DISTIN(X,N,IWRITE,XTEMP,NDIST,IBUGA3,IERROR)
C
      IF(IERROR.EQ.'YES')GOTO9000
      IF(NDIST.GT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2001)
 2001   FORMAT('***** ERROR IN FREQUENCY RANDOMNESS TEST.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2003)
 2003   FORMAT('      FOR FREQUENCY TEST, AT MOST TWO DISTINCT ',
     1         'VALUES ARE ALLOWED.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2005)NDIST
 2005   FORMAT('      NUMBER OF DISTINCT VALUES = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(NDIST.EQ.1)THEN
        DO2010I=1,N
          XTEMP(I)=1.0
 2010   CONTINUE
      ELSE
        ALOW=MIN(XTEMP(1),XTEMP(2))
        AHIGH=MAX(XTEMP(1),XTEMP(2))
        SN=0.0
        DO2020I=1,N
          IF(X(I).EQ.ALOW)THEN
            SN=SN - 1.0
          ELSE
            SN=SN + 1.0
          ENDIF
 2020   CONTINUE
      ENDIF
C
      STATVA=ABS(SN)/SQRT(REAL(N))
C
      ARG1=STATVA
      CALL NORCDF(ARG1,RESULT)
      TERM=2.0*RESULT-1.0
      STATCD=1.0-TERM
C
C               *******************************
C               **  STEP 3--                 **
C               **  WRITE OUT A LINE         **
C               **  OF SUMMARY INFORMATION.  **
C               *******************************
C
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,811)N,STATVA
  811   FORMAT('THE VALUE OF THE FREQUENCY STATISTIC OF THE ',I8,
     1         ' OBSERVATIONS = ',G15.7)
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
      IWRITE=IWRTSV
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FRT3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFRT3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR
 9012   FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)STATVA,STATCD
 9015   FORMAT('STATVA,STATCD = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFRT4(X,N,M,IWRITE,XTEMP,STATVA,STATCD,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE FREQUENCY WITHIN A BLOCK
C              STATISTIC (AND ALTERNATIVELY THE CDF VALUE).
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED OR SORTED) OBSERVATIONS.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C                     --M      = THE INTEGER NUMBER OF OBSERVATIONS
C                                PER BLOCK.
C     OUTPUT ARGUMENTS--STATVA = THE SINGLE PRECISION VALUE OF THE
C                                COMPUTED STATISTIC.
C                     --STATCD = THE SINGLE PRECISION VALUE OF THE
C                                COMPUTED CDF OF THE TEST STATISTIC.
C     OUTPUT--THE COMPUTED SINGLE PRECISION VALUE OF THE
C             TEST STATISTIC.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--DISTIN
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2009.2
C     ORIGINAL VERSION--FEBRUARY  2009.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IWRTSV
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION XTEMP(*)
C
      DOUBLE PRECISION DRESLT
      DOUBLE PRECISION DGAMIP
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPFR'
      ISUBN2='T4  '
      IERROR='NO'
      IWRTSV=IWRITE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FRT4')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPFRT4--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3
   52   FORMAT('IBUGA3 = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)N,M
   53   FORMAT('N,M = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               **************************************************
C               **  COMPUTE FREQUENCY WITHIN A BLOCK STATISTIC  **
C               **************************************************
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      AN=N
C
      IF(N.LE.5)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN FREQUENCY IN BLOCK STATISTIC--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS FOR THE ',
     1         'RESPONSE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      VARIABLE MUST BE 6 OR LARGER.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,116)
  116   FORMAT('      SUCH WAS NOT THE CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS HERE = ',I8,
     1         '.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
      STATVA=-99.0
      STATCD=-99.0
      IWRITE='OFF'
C
      IWRITE='NO'
      CALL DISTIN(X,N,IWRITE,XTEMP,NDIST,IBUGA3,IERROR)
C
      NBLOCK=N/M
      AMNSZ=0.01*REAL(N)
C
      IF(NBLOCK.GE.100)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3010)
 3010   FORMAT('***** WARNING IN FREQUENCY IN BLOCK STATISTIC--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3011)
 3011   FORMAT('      THE NUMBER OF BLOCKS IS GREATER')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3012)
 3012   FORMAT('      THAN THE RECOMMENDED MAXIMUM OF 100.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3013)N
 3013   FORMAT('      SAMPLE SIZE       = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3014)M
 3014   FORMAT('      BLOCK SIZE        = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3015)NBLOCK
 3015   FORMAT('      NUMBER OF BLOCKS  = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(M.LE.INT(AMNSZ))THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3010)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3021)
 3021   FORMAT('      THE BLOCK SIZE IS LESS THAN THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3022)INT(AMNSZ)
 3022   FORMAT('      RECOMMENDED MINIMUM OF ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3023)N
 3023   FORMAT('      SAMPLE SIZE                     = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3024)M
 3024   FORMAT('      BLOCK SIZE                      = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3025)NBLOCK
 3025   FORMAT('      NUMBER OF BLOCKS                = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3026)INT(AMNSZ)
 3026   FORMAT('      RECOMMENDED MINIMUM BLOCK SIZE  = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(M.GT.N)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3031)
 3031   FORMAT('      THE BLOCK SIZE IS GREATER THAN THE SAMPLE ',
     1         'SIZE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3033)N
 3033   FORMAT('      SAMPLE SIZE                     = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3034)M
 3034   FORMAT('      BLOCK SIZE                      = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3035)NBLOCK
 3035   FORMAT('      NUMBER OF BLOCKS                = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3036)INT(AMNSZ)
 3036   FORMAT('      RECOMMENDED MINIMUM BLOCK SIZE  = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(NDIST.NE.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3041)NDIST
 3041   FORMAT('      THE RESPONSE VARIBLE CONTAINS ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3043)
 3043   FORMAT('      DISTINCT VALUES.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      ALOW=MIN(XTEMP(1),XTEMP(2))
      AHIGH=MAX(XTEMP(1),XTEMP(2))
      AM=REAL(M)
C
      SUM=0.0
      DO3110K=1,NBLOCK
        ISTRT=(K-1)*M+1
        ISTOP=K*M
        AONES=0
        DO3120I=ISTRT,ISTOP
          IF(X(I).EQ.AHIGH)AONES=AONES+1.0
 3120   CONTINUE
        API=AONES/AM
        SUM=SUM + (API-0.5)**2
 3110 CONTINUE
C
      STATVA=4.0*AM*SUM
      DRESLT=1.0D0 - DGAMIP(DBLE(NBLOCK)/2.0D0,DBLE(STATVA)/2.0D0)
      STATCD=REAL(DRESLT)
C
C               *******************************
C               **  STEP 3--                 **
C               **  WRITE OUT A LINE         **
C               **  OF SUMMARY INFORMATION.  **
C               *******************************
C
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,811)N,STATVA
  811   FORMAT('THE VALUE OF THE FREQUENCY STATISTIC OF THE ',I8,
     1         ' OBSERVATIONS = ',G15.7)
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
      IWRITE=IWRTSV
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FRT4')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFRT4--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR
 9012   FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)STATVA,STATCD
 9015   FORMAT('STATVA,STATCD = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFRT5(STATVA,STATCD,PVAL,
     1                  CUT0,CUT50,CUT75,CUT90,CUT95,
     1                  CUT975,CUT99,CUT999,
     1                  IFLAGU,IFRST,ILAST,
     1                  IBUGA2,IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--UTILITY ROUTINE USED BY DPFRTE.  THIS ROUTINE
C              UPDATES THE PARAMETERS "STATVAL", "STATCDF", AND
C              "PVALUE" AND VARIOUS CUTOFF POINTS AFTER A FREQUENCY TEST.
C
C              THIS ROUTINE MAY ALSO BE CALLED BY OTHER ROUTINES AS
C              WELL.
C
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORAOTRY
C                 NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2011/3
C     ORIGINAL VERSION--MARCH     2011.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IFLAGU
      CHARACTER*4 IBUGA2
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      LOGICAL IFRST
      LOGICAL ILAST
C
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IOP
C
      SAVE IOUNI1
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'FRT5')THEN
        ISTEPN='1'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPFRT5--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)STATVA,STATCD,PVAL
   53   FORMAT('STATVA,STATCD,PVAL = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)CUT0,CUT50,CUT75,CUT90
   54   FORMAT('CUT0,CUT50,CUT75,CUT90 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)CUT95,CUT975,CUT99,CUT999
   55   FORMAT('CUT95,CUT975,CUT99 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IFLAGU.EQ.'FILE')THEN
C
        IF(IFRST)THEN
          IOP='OPEN'
          IFLAG1=1
          IFLAG2=0
          IFLAG3=0
          IFLAG4=0
          IFLAG5=0
          CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1                IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1                IBUGA3,ISUBRO,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          WRITE(IOUNI1,295)
  295     FORMAT(11X,'STATVAL',8X,'STATCDF',8X,'PVALUE',
     1           7X,'CUTOFF0',7X,'CUTOFF50',7X,'CUTOFF75',
     1           7X,'CUTOFF90',7X,'CUTOFF95',7X,'CUTOF975',
     1           7X,'CUTOFF99',7X,'CUTOF999')
        ENDIF
        WRITE(IOUNI1,299)STATVA,STATCD,PVAL,CUT0,CUT50,CUT75,
     1                   CUT90,CUT95,CUT975,CUT99,CUT999
  299   FORMAT(11E15.7)
      ELSEIF(IFLAGU.EQ.'ON')THEN
        IF(STATVA.NE.CPUMIN)THEN
          IH='STAT'
          IH2='VAL '
          VALUE0=STATVA
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(STATCD.NE.CPUMIN)THEN
          IH='STAT'
          IH2='CDF '
          VALUE0=STATCD
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(PVAL.NE.CPUMIN)THEN
          IH='PVAL'
          IH2='UE  '
          VALUE0=PVAL
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(CUT0.NE.CPUMIN)THEN
          IH='CUTO'
          IH2='FF0'
          VALUE0=CUT0
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(CUT50.NE.CPUMIN)THEN
          IH='CUTO'
          IH2='FF50'
          VALUE0=CUT50
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(CUT75.NE.CPUMIN)THEN
          IH='CUTO'
          IH2='FF75'
          VALUE0=CUT75
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(CUT90.NE.CPUMIN)THEN
          IH='CUTO'
          IH2='FF90'
          VALUE0=CUT90
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(CUT95.NE.CPUMIN)THEN
          IH='CUTO'
          IH2='FF95'
          VALUE0=CUT95
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(CUT975.NE.CPUMIN)THEN
          IH='CUTO'
          IH2='F975'
          VALUE0=CUT975
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(CUT99.NE.CPUMIN)THEN
          IH='CUTO'
          IH2='FF99'
          VALUE0=CUT99
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(CUT999.NE.CPUMIN)THEN
          IH='CUTO'
          IH2='F999'
          VALUE0=CUT999
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
      ENDIF
C
      IF(IFLAGU.EQ.'FILE')THEN
        IF(ILAST)THEN
          IOP='CLOS'
          IFLAG1=1
          IFLAG2=0
          IFLAG3=0
          IFLAG4=0
          IFLAG5=0
          CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1                IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1                IBUGA3,ISUBRO,IERROR)
C
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'FRT5')THEN
            ISTEPN='3A'
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,301)IERROR,IOUNI1
  301       FORMAT('AFTER CALL DPCLFI, IERROR,IOUNI1 = ',A4,2X,I5)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          IF(IERROR.EQ.'YES')GOTO9000
        ENDIF
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'FRT5')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DPFRT5--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFRTH(ICOM,IHARG,ARG,NUMARG,
     1PDEFTH,
     1PFRATH,
     1IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE FRAME THICKNESS
C              CURRENTLY ALL 4 FRAME LINES MUST
C              BE SET TO THE SAME THICKNESS.
C              THE FRAME THICKNESS SWITCHES FOR THE FRAME
C              IS CONTAINED IN THE VARIABLE
C              PFRATH
C     INPUT  ARGUMENTS--ICOM
C                     --IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG
C                     --PDEFCO
C     OUTPUT ARGUMENTS--PFRATH (A REAL VARIABLE)
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--ALAN HECKERT
C                 COMPUTER SERVICES DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--OCTOBER   1980.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICOM
      CHARACTER*4 IHARG
C
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      REAL        PHOLD
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
      DIMENSION ARG(*)
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
      IF(NUMARG.LE.0)GOTO1900
      IF(NUMARG.GE.1.AND.IHARG(1).EQ.'THIC')GOTO1090
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'MARK'.AND.
     1IHARG(2).EQ.'THIC')GOTO1090
      GOTO1900
 1090 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH HORIZONTAL FRAMES    ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'XFRA')GOTO1100
      GOTO1199
C
 1100 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      IF(IHARG(NUMARG).EQ.'THIC')GOTO1150
      GOTO1160
C
 1150 CONTINUE
      PHOLD=PDEFTH
      GOTO1180
C
 1160 CONTINUE
      PHOLD=ARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      PFRATH=PHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE FRAME THICKNESS (FOR ALL FRAME LINES ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)PHOLD
 1182 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1900
C
 1199 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE BOTTOM HORIZONTAL FRAME IS      TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X1FR')GOTO1200
      GOTO1299
C
 1200 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1250
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1250
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1250
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1250
      IF(IHARG(NUMARG).EQ.'THIC')GOTO1250
      GOTO1260
C
 1250 CONTINUE
      PHOLD=PDEFTH
      GOTO1280
C
 1260 CONTINUE
      PHOLD=ARG(NUMARG)
      GOTO1280
C
 1280 CONTINUE
      IFOUND='YES'
      PFRATH=PHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1289
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1281)
 1281 FORMAT('THE FRAME THICKNESS (FOR ALL FRAME LINES) ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1282)PHOLD
 1282 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1289 CONTINUE
      GOTO1900
C
 1299 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE TOP    HORIZONTAL FRAME IS      TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'X2FR')GOTO1300
      GOTO1399
C
 1300 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1350
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1350
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1350
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1350
      IF(IHARG(NUMARG).EQ.'THIC')GOTO1350
      GOTO1360
C
 1350 CONTINUE
      PHOLD=PDEFTH
      GOTO1380
C
 1360 CONTINUE
      PHOLD=ARG(NUMARG)
      GOTO1380
C
 1380 CONTINUE
      IFOUND='YES'
      PFRATH=PHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1389
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1381)
 1381 FORMAT('THE FRAME THICKNESS (FOR ALL FRAME LINES) ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1382)PHOLD
 1382 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1389 CONTINUE
      GOTO1900
C
 1399 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  BOTH VERTICAL   FRAMES    ARE TO BE CHANGED    **
C               *****************************************************
C
      IF(ICOM.EQ.'YFRA')GOTO1400
      GOTO1499
C
 1400 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1450
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1450
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1450
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1450
      IF(IHARG(NUMARG).EQ.'THIC')GOTO1450
      GOTO1460
C
 1450 CONTINUE
      PHOLD=PDEFTH
      GOTO1480
C
 1460 CONTINUE
      PHOLD=ARG(NUMARG)
      GOTO1480
C
 1480 CONTINUE
      IFOUND='YES'
      PFRATH=PHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1489
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1481)
 1481 FORMAT('THE FRAME THICKNESS (FOR ALL FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1482)PHOLD
 1482 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1489 CONTINUE
      GOTO1900
C
 1499 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE LEFT   VERTICAL   FRAME IS      TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y1FR')GOTO1500
      GOTO1599
C
 1500 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1550
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1550
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1550
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1550
      IF(IHARG(NUMARG).EQ.'THIC')GOTO1550
      GOTO1560
C
 1550 CONTINUE
      PHOLD=PDEFTH
      GOTO1580
C
 1560 CONTINUE
      PHOLD=ARG(NUMARG)
      GOTO1580
C
 1580 CONTINUE
      IFOUND='YES'
      PFRATH=PHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1589
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1581)
 1581 FORMAT('THE FRAME THICKNESS (FOR ALL FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1582)PHOLD
 1582 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1589 CONTINUE
      GOTO1900
C
 1599 CONTINUE
C
C               **************************************************************
C               **  TREAT THE CASE WHEN                                     **
C               **  ONLY THE RIGHT  VERTICAL   FRAME IS      TO BE CHANGED  **
C               **************************************************************
C
      IF(ICOM.EQ.'Y2FR')GOTO1600
      GOTO1699
C
 1600 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1650
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1650
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1650
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1650
      IF(IHARG(NUMARG).EQ.'THIC')GOTO1650
      GOTO1660
C
 1650 CONTINUE
      PHOLD=PDEFTH
      GOTO1680
C
 1660 CONTINUE
      PHOLD=ARG(NUMARG)
      GOTO1680
C
 1680 CONTINUE
      IFOUND='YES'
      PFRATH=PHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1689
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1681)
 1681 FORMAT('THE FRAME THICKNESS (FOR ALL FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1682)PHOLD
 1682 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1689 CONTINUE
      GOTO1900
C
 1699 CONTINUE
C
C               *****************************************************
C               **  TREAT THE CASE WHEN                            **
C               **  ALL 4 FRAME FRAME LINES ARE TO BE CHANGED      **
C               *****************************************************
C
      IF(ICOM.EQ.'FRAM')GOTO1700
      IF(ICOM.EQ.'XYFR')GOTO1700
      IF(ICOM.EQ.'YXFR')GOTO1700
      GOTO1799
C
 1700 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1750
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1750
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1750
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1750
      IF(IHARG(NUMARG).EQ.'THIC')GOTO1750
      GOTO1760
C
 1750 CONTINUE
      PHOLD=PDEFTH
      GOTO1780
C
 1760 CONTINUE
      PHOLD=ARG(NUMARG)
      GOTO1780
C
 1780 CONTINUE
      IFOUND='YES'
      PFRATH=PHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1789
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1781)
 1781 FORMAT('THE FRAME THICKNESS (FOR ALL 4 ',
     1'FRAME LINES)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1782)PHOLD
 1782 FORMAT('HAS JUST BEEN SET TO ',E15.7)
      CALL DPWRST('XXX','BUG ')
 1789 CONTINUE
      GOTO1900
C
 1799 CONTINUE
C
 1900 CONTINUE
      RETURN
      END
      SUBROUTINE DPFRTY(IHARG,NUMARG,
     1IDEFFT,
     1IFRATY,
     1IBUGS2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE FRACTAL TYPE
C              CAN BE <WHITHERS/ANGLE/BARNSLEY> (DEFAULT IS BARNSLEY)
C              THIS SWITCH CONTROLS HOW THE ARGUMENTS TO THE
C              FRACTAL PLOT COMMAND ARE INTERPERTED.
C
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C                     --IDEFFT (A  CHARACTER VARIABLE)
C                     --IBUGS2 (A  CHARACTER VARIABLE)
C     OUTPUT ARGUMENTS--IFRATY (A CHARACTER VARIABLE)
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
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--93/7
C     ORIGINAL VERSION--JULY     1993.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IDEFFT
      CHARACTER*4 IFRATY
      CHARACTER*4 IBUGS2
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
      IF(IBUGS2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPFRTY--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IDEFFT
   53 FORMAT('IDEFFT = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)NUMARG
   54 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO55I=1,NUMARG
      WRITE(ICOUT,56)I,IHARG(I)
   56 FORMAT('I,IHARG(I) = ',I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(NUMARG.LE.1)GOTO1150
      IF(NUMARG.GT.2)GOTO9000
C
      IF(IHARG(2).EQ.'AUTO')GOTO1150
      IF(IHARG(2).EQ.'DEFA')GOTO1150
      GOTO1160
C
 1150 CONTINUE
      IHOLD=IDEFFT
      GOTO1180
C
 1160 CONTINUE
      IHOLD=IHARG(2)
      IF(IHOLD.EQ.'BARN')GOTO1180
      IF(IHOLD.EQ.'WHIT')GOTO1180
      IF(IHOLD.EQ.'ROTA')IHOLD='ANGL'
      IF(IHOLD.EQ.'ANGL')GOTO1180
      GOTO1170
C
 1170 CONTINUE
      IERROR='YES'
      IFOUND='YES'
      WRITE(ICOUT,1171)IHOLD
 1171 FORMAT('THE FRACTAL TYPE SWITCH ',A4,' IS NOT RECOGNIZED')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1172)
 1172 FORMAT('IT SHOLUD BE: BARNSLEY, WHITHERS, OR ANGLE')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
 1180 CONTINUE
      IFOUND='YES'
      IFRATY=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)IFRATY
 1181 FORMAT('THE FRACTAL TYPE SWITCH HAS JUST BEEN SET TO ',A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO9000
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPFRTY')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGS2,IFOUND,IERROR
 9012 FORMAT('IBUGS2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IDEFFT,IFRATY
 9013 FORMAT('IDEFFT,IFRATY = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPFTES(MAXNXT,ICAPSW,IFORSW,
     1                  IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--CARRY OUT A TWO-SAMPLE F-TEST
C     EXAMPLE--F TEST Y1 Y2
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
C     ORIGINAL VERSION--JULY      1984.
C     UPDATED         --FEBRUARY  1994. ADD COMMENTS ABOVE
C     UPDATED         --DECEMBER  1994. COPY F TEST PARAMETERS
C     UPDATED         --JANUARY   2004. SUPPORT FOR HTML, LATEX
C     UPDATED         --MARCH     2011. USE DPPARS AND DPPAR3
C     UPDATED         --MARCH     2011. IF MORE THAN 2 VARIABLES
C                                       SPECIFIED, PERFORM ALL
C                                       PAIRWISE TESTS
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
C
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
C
      CHARACTER*4 IFLAGU
      LOGICAL IFRST
      LOGICAL ILAST
C
C---------------------------------------------------------------------
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPFT'
      ISUBN2='ES  '
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
C               **  TREAT THE F TEST CASE     **
C               ********************************
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'FTES')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPFTES--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA2,IBUGA3,IBUGQ,ISUBRO,MAXNXT
   52   FORMAT('IBUGA2,IBUGA3,IBUGQ,ISUBRO,MAXNXT = ',4(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ****************************************
C               **  STEP 2--                          **
C               **  EXTRACT THE VARIABLE LIST         **
C               ****************************************
C
      ISTEPN='2'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FTES')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='F-TEST'
      MINNA=1
      MAXNA=100
      MINN2=2
      IFLAGE=0
      IFLAGM=1
      MINNVA=2
      MAXNVA=MAXSPN
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FTES')THEN
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
C               *****************************************
C               **  STEP 3A--                          **
C               **  CASE 1: TWO RESPONSE VARIABLES     **
C               **          WITH NO REPLICATION        **
C               *****************************************
C
      ISTEPN='3A'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FTES')
     1  CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMVA2=1
      DO5210I=1,NUMVAR
        DO5220J=I+1,NUMVAR
          ICOL=I
          CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1                INAME,IVARN1,IVARN2,IVARTY,
     1                ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1                MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1                MAXCP4,MAXCP5,MAXCP6,
     1                V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1                Y,Y,Y,NS1,NLOCA2,NLOCA3,ICASE,
     1                IBUGA3,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          ICOL=J
          CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1                INAME,IVARN1,IVARN2,IVARTY,
     1                ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1                MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1                MAXCP4,MAXCP5,MAXCP6,
     1                V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1                X,X,X,NS2,NLOCA2,NLOCA3,ICASE,
     1                IBUGA3,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
C               *****************************************
C               **  STEP 52--                          **
C               **  PERFORM 2-SAMPLE F-TEST            **
C               *****************************************
C
          ISTEPN='52'
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'FTES')THEN
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5211)
 5211       FORMAT('***** FROM DPFTES, BEFORE CALL DPFTES--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5212)I,J,NS1,NS2,MAXN
 5212       FORMAT('I,J,NS1,NS2,MAXN = ',5I8)
            CALL DPWRST('XXX','BUG ')
            DO5215II=1,MAX(NS1,NS2)
              WRITE(ICOUT,5216)II,Y(II),X(II)
 5216         FORMAT('I,Y(I),X(I) = ',I8,2G15.7)
              CALL DPWRST('XXX','BUG ')
 5215       CONTINUE
          ENDIF
C
          IVARID=IVARN1(I)
          IVARI2=IVARN2(I)
          IVARI3=IVARN1(J)
          IVARI4=IVARN2(J)
          CALL DPFTE2(Y,NS1,X,NS2,MAXNXT,
     1                ICAPSW,ICAPTY,IFORSW,
     1                STATVA,STANU1,STANU2,POOLSD,STATCD,PVAL,
     1                IVARID,IVARI2,IVARI3,IVARI4,
     1                CUTU50,CUTU75,CUTU90,CUTU95,CUT975,
     1                CUTU99,CUT999,
     1                IBUGA3,ISUBRO,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
C               ***************************************
C               **  STEP 8C--                        **
C               **  UPDATE INTERNAL DATAPLOT TABLES  **
C               ***************************************
C
          ISTEPN='8C'
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'FTE2')
     1      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
          IF(NUMVAR.GT.2)THEN
            IFLAGU='FILE'
          ELSE
            IFLAGU='ON'
          ENDIF
          IFRST=.FALSE.
          ILAST=.FALSE.
          IF(I.EQ.1 .AND. J.EQ.2)IFRST=.TRUE.
          IF(I.EQ.NUMVAR .AND. J.EQ.NUMVAR)ILAST=.TRUE.
          CALL DPFTE5(STATVA,STATCD,PVAL,STANU1,STANU2,POOLSD,
     1                CUTU50,CUTU75,CUTU90,CUTU95,CUT975,
     1                CUTU99,CUT999,
     1                IFLAGU,IFRST,ILAST,
     1                IBUGA2,IBUGA3,ISUBRO,IERROR)
C
 5220   CONTINUE
 5210 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'FTES')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFTES--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)IFOUND,IERROR
 9016   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFTE2(Y1,N1,Y2,N2,MAXNXT,
     1                  ICAPSW,ICAPTY,IFORSW,
     1                  STATVA,STANU1,STANU2,POOLSD,STATCD,PVAL,
     1                  IVARID,IVARI2,IVARI3,IVARI4,
     1                  CUTU50,CUTU75,CUTU90,CUTU95,CUT975,
     1                  CUTU99,CUT999,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--THIS ROUTINE CARRIES OUT AN F TEST (NECESSARILY 2-SAMPLE)
C     EXAMPLE--F TEST Y1 Y2
C              SAMPLE 1 IS IN INPUT VECTOR Y1 (WITH N1 OBSERVATIONS).
C              SAMPLE 2 IS IN INPUT VECTOR Y2 (WITH N2 OBSERVATIONS).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--94/2
C     ORIGINAL VERSION--FEBRUARY  1994.
C     UPDATED         --DECEMBER  1994. COPY F TEST PARAMETERS
C     UPDATED         --JANUARY   2004. SUPPORT FOR HTML, LATEX
C     UPDATED         --MARCH     2011. USE DPDTA1, DPDTA5 TO PRINT
C                                       OUTPUT
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
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
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION Y1(*)
      DIMENSION Y2(*)
C
      PARAMETER (NUMALP=7)
      REAL ALPHA(NUMALP)
C
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
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
      DATA ALPHA/0.50, 0.75, 0.90, 0.95, 0.975, 0.99, 0.999/
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPFT'
      ISUBN2='E2  '
      IERROR='NO'
C
      N=(-99)
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FTE2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPFTE2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N1,N2
   52   FORMAT('IBUGA3,ISUBRO,N1,N2 = ',2(A4,2X),2I8)
        CALL DPWRST('XXX','WRIT')
        DO56I=1,N1
          WRITE(ICOUT,57)I,Y1(I)
   57     FORMAT('I,Y1(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
        DO66I=1,N2
          WRITE(ICOUT,67)I,Y2(I)
   67     FORMAT('I,Y2(I) = ',I8,E15.7)
          CALL DPWRST('XXX','WRIT')
   66   CONTINUE
      ENDIF
C
C               ******************************
C               **  STEP 21--               **
C               **  CARRY OUT CALCULATIONS  **
C               **  FOR AN          F TEST  **
C               ******************************
C
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FTE2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPFTE3(Y1,N1,Y2,N2,MAXNXT,
     1            Y1MEAN,Y1SD,Y2MEAN,Y2SD,
     1            SDNUM,SDDEN,IDFNUM,IDFDEN,
     1            STATVA,STANU1,STANU2,POOLSD,STATCD,PVAL,
     1            IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      CALL FPPF(.50,IDFNUM,IDFDEN,CUTU50)
      CALL FPPF(.75,IDFNUM,IDFDEN,CUTU75)
      CALL FPPF(.90,IDFNUM,IDFDEN,CUTU90)
      CALL FPPF(.95,IDFNUM,IDFDEN,CUTU95)
      CALL FPPF(.975,IDFNUM,IDFDEN,CUT975)
      CALL FPPF(.99,IDFNUM,IDFDEN,CUTU99)
      CALL FPPF(.999,IDFNUM,IDFDEN,CUT999)
C
C               ******************************
C               **   STEP 42--              **
C               **   WRITE OUT EVERYTHING   **
C               **   FOR AN         F TEST  **
C               ******************************
C
      ISTEPN='42'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FTE2')
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
      ITITLE='Two Sample F-Test for Equal Standard Deviations'
      NCTITL=47
      ITITLZ=' '
      NCTITZ=0
C
      ICNT=1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='First Response Variable:  '
      WRITE(ITEXT(ICNT)(27:30),'(A4)')IVARID(1:4)
      WRITE(ITEXT(ICNT)(31:34),'(A4)')IVARI2(1:4)
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='Second Response Variable: '
      WRITE(ITEXT(ICNT)(27:30),'(A4)')IVARI3(1:4)
      WRITE(ITEXT(ICNT)(31:34),'(A4)')IVARI4(1:4)
      NCTEXT(ICNT)=34
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
      ITEXT(ICNT)='H0: Sigma1 = Sigma2'
      NCTEXT(ICNT)=19
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: Sigma1 not equal Sigma2'
      NCTEXT(ICNT)=27
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
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
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
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
      ITEXT(ICNT)='Standard Deviation (Numerator):'
      NCTEXT(ICNT)=31
      AVALUE(ICNT)=SDNUM
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Standard Deviation (Denomerator):'
      NCTEXT(ICNT)=33
      AVALUE(ICNT)=SDDEN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Degrees of Freedom (Numerator):'
      NCTEXT(ICNT)=31
      AVALUE(ICNT)=IDFNUM
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Degrees of Freedom (Denomerator):'
      NCTEXT(ICNT)=33
      AVALUE(ICNT)=IDFDEN
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Pooled Standard Deviation:'
      NCTEXT(ICNT)=26
      AVALUE(ICNT)=POOLSD
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='F-Test Statistic Value:'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=STATVA
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='F-Test CDF Value:'
      NCTEXT(ICNT)=17
      AVALUE(ICNT)=STATCD
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='F-Test P-Value:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=PVAL
      IDIGIT(ICNT)=NUMDIG
C
      NUMROW=ICNT
      DO5010I=1,NUMROW
        NTOT(I)=15
 5010 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
C
      ISTEPN='42A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ADK2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1            AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ISTEPN='42D'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'ADK2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ITITLE='Conclusions (Upper 1-Tailed Test)'
      NCTITL=33
      ITITL9='H0: sigma1 = sigma2; sigma1 <> sigma2'
      NCTIT9=37
C
      DO5030J=1,NUMCLI
        DO5040I=1,3
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
 5040   CONTINUE
 5030 CONTINUE
C
      ITITL2(2,1)='Significance'
      NCTIT2(2,1)=12
      ITITL2(3,1)='Level'
      NCTIT2(3,1)=5
C
      ITITL2(2,2)='Test '
      NCTIT2(2,2)=4
      ITITL2(3,2)='Statistic'
      NCTIT2(3,2)=9
C
      ITITL2(2,3)='Critical'
      NCTIT2(2,3)=8
      ITITL2(3,3)='Region (>=)'
      NCTIT2(3,3)=11
C
      ITITL2(1,4)='Null'
      NCTIT2(1,4)=4
      ITITL2(2,4)='Hypothesis'
      NCTIT2(2,4)=10
      ITITL2(3,4)='Conclusion'
      NCTIT2(3,4)=10
C
      NMAX=0
      NUMCOL=NUMCLI
      DO5050I=1,NUMCOL
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
C
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
C
      DO5060J=1,NUMALP
C
        AMAT(J,2)=STATVA
        IF(J.EQ.1)THEN
          AMAT(J,3)=CUTU50
        ELSEIF(J.EQ.2)THEN
          AMAT(J,3)=CUTU75
        ELSEIF(J.EQ.3)THEN
          AMAT(J,3)=CUTU90
        ELSEIF(J.EQ.4)THEN
          AMAT(J,3)=CUTU95
        ELSEIF(J.EQ.5)THEN
          AMAT(J,3)=CUT975
        ELSEIF(J.EQ.6)THEN
          AMAT(J,3)=CUTU99
        ELSEIF(J.EQ.7)THEN
          AMAT(J,3)=CUT999
        ENDIF
        IVALUE(J,4)(1:6)='REJECT'
        IF(STATVA.LT.AMAT(J,3))THEN
          IVALUE(J,4)(1:6)='ACCEPT'
        ENDIF
        NCVALU(J,4)=6
C
        ALPHAT=100.0*ALPHA(J)
        WRITE(IVALUE(J,1)(1:4),'(F4.1)')ALPHAT
        IVALUE(J,1)(5:5)='%'
        NCVALU(J,1)=5
 5060 CONTINUE
C
      ICNT=NUMALP
      NUMLIN=3
      IFRST=.TRUE.
      ILAST=.TRUE.
      IFLAGS=.TRUE.
      IFLAGE=.TRUE.
      CALL DPDTA5(ITITLE,NCTITL,
     1            ITITL9,NCTIT9,ITITL2,NCTIT2,
     1            MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1            IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1            IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1            ICAPSW,ICAPTY,IFRST,ILAST,
     1            IFLAGS,IFLAGE,
     1            ISUBRO,IBUGA3,IERROR)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FTE2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFTE2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFTE3(Y1,N1,Y2,N2,MAXNXT,
     1                  Y1MEAN,Y1SD,Y2MEAN,Y2SD,
     1                  SDNUM,SDDEN,IDFNUM,IDFDEN,
     1                  STATVA,STANU1,STANU2,POOLSD,STATCD,PVAL,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--THIS ROUTINE CARRIES OUT AN F TEST.  EXTRACTED FROM
C              DPFTE2 TO MAKE IT CALLABLE FROM CMPSTA (I.E., A
C              SUPPORTED STATISTIC).
C     EXAMPLE--LET A = F TEST Y1 Y2
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2011/3
C     ORIGINAL VERSION--MARCHARY  2011. EXTRACTED FROM DPFTE2
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
C---------------------------------------------------------------------
C
      DIMENSION Y1(*)
      DIMENSION Y2(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPFT'
      ISUBN2='E3  '
      IERROR='NO'
C
      N=(-99)
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FTE3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPFTE3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N1,N2,MAXNXT
   52   FORMAT('IBUGA3,ISUBRO,N1,N2,MAXNXT = ',2(A4,2X),3I8)
        CALL DPWRST('XXX','WRIT')
        DO56I=1,N1
          WRITE(ICOUT,57)I,Y1(I)
   57     FORMAT('I,Y1(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
        DO66I=1,N2
          WRITE(ICOUT,67)I,Y2(I)
   67     FORMAT('I,Y2(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   66   CONTINUE
      ENDIF
C
C               ********************************************
C               **  STEP 11--                             **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FTE3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N1.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1111)
 1111   FORMAT('***** ERROR IN F-TEST')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1113)
 1113   FORMAT('      THE NUMBER OF OBSERVATIONS IN THE FIRST ',
     1         'RESPONSE VARIABLE IS LESS THAN TWO.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1115)N1
 1115   FORMAT('SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ELSEIF(N2.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1111)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1123)
 1123   FORMAT('      THE NUMBER OF OBSERVATIONS IN THE SECOND ',
     1         'RESPONSE VARIABLE IS LESS THAN TWO.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1115)N2
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      HOLD=Y1(1)
      DO1135I=2,N1
        IF(Y1(I).NE.HOLD)GOTO1139
 1135 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1111)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1131)HOLD
 1131 FORMAT('      THE FIRST RESPONSE VARIABLE HAS ALL ELEMENTS = ',
     1       G15.7)
      CALL DPWRST('XXX','WRIT')
      IERROR='YES'
      GOTO9000
 1139 CONTINUE
C
      HOLD=Y2(1)
      DO1145I=2,N2
        IF(Y2(I).NE.HOLD)GOTO1149
 1145 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1111)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1141)HOLD
 1141 FORMAT('      THE SECOND RESPONSE VARIABLE HAS ALL ELEMENTS = ',
     1       G15.7)
      CALL DPWRST('XXX','WRIT')
      IERROR='YES'
      GOTO9000
 1149 CONTINUE
C
C               ******************************
C               **  STEP 21--               **
C               **  CARRY OUT CALCULATIONS  **
C               **  FOR AN          F TEST  **
C               ******************************
C
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FTE3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IWRITE='OFF'
C
      CALL MEAN(Y1,N1,IWRITE,Y1MEAN,IBUGA3,IERROR)
      CALL SD(Y1,N1,IWRITE,Y1SD,IBUGA3,IERROR)
      Y1VAR=Y1SD**2
C
      CALL MEAN(Y2,N2,IWRITE,Y2MEAN,IBUGA3,IERROR)
      CALL SD(Y2,N2,IWRITE,Y2SD,IBUGA3,IERROR)
      Y2VAR=Y2SD**2
C
      AN1=N1
      AN2=N2
C
      IF(Y1SD.GE.Y2SD)THEN
         SDNUM=Y1SD
         SDDEN=Y2SD
         IDFNUM=N1-1
         IDFDEN=N2-1
      ELSE
         SDNUM=Y2SD
         SDDEN=Y1SD
         IDFNUM=N2-1
         IDFDEN=N1-1
      ENDIF
      RATIO=(SDNUM/SDDEN)**2
      CALL FCDF(RATIO,IDFNUM,IDFDEN,CDF)
      DFNUM=IDFNUM
      DFDEN=IDFDEN
C
      POOLSS=DFNUM*SDNUM*SDNUM+DFDEN*SDDEN*SDDEN
      POOLDF=DFNUM+DFDEN
      POOLVA=0.0
      IF(POOLDF.GT.0.0)POOLVA=POOLSS/POOLDF
      POOLSD=0.0
      IF(POOLVA.GT.0.0)POOLSD=SQRT(POOLVA)
C
      STATVA=RATIO
      STATCD=CDF
      PVAL=1.0 - CDF
      STANU1=IDFNUM
      STANU2=IDFDEN
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FTE3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFTE3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFTE5(STATVA,STATCD,PVAL,STANU1,STANU2,POOLSD,
     1                  CUTU50,CUTU75,CUTU90,CUTU95,CUT975,
     1                  CUTU99,CUT999,
     1                  IFLAGU,IFRST,ILAST,
     1                  IBUGA2,IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--UTILITY ROUTINE USED BY DPFTES.  THIS ROUTINE UPDATES THE
C              PARAMETERS "STATVAL", "STATCDF", "PVALUE", "STANU1",
C              "STANU2", "POOLSD", AND VARIOUS CUTOFF POINTS AFTER A
C              F TEST.
C
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORAOTRY
C                 NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2011/3
C     ORIGINAL VERSION--MARCH     2011.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IFLAGU
      CHARACTER*4 IBUGA2
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      LOGICAL IFRST
      LOGICAL ILAST
C
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IOP
C
      SAVE IOUNI1
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'FTE5')THEN
        ISTEPN='1'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPFTE5--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)STATVA,STATCD,PVAL
   53   FORMAT('STATVA,STATCD,PVAL = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)CUTL95,CUTU95,CUTL99,CUTU99
   54   FORMAT('CUTL95,CUTU95,CUTL99,CUTU99 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)STANU1,STANU2,POOLSD
   55   FORMAT('STANU1,STANU2,POOLSD = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IFLAGU.EQ.'FILE')THEN
C
        IF(IFRST)THEN
          IOP='OPEN'
          IFLAG1=1
          IFLAG2=0
          IFLAG3=0
          IFLAG4=0
          IFLAG5=0
          CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1                IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1                IBUGA3,ISUBRO,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          WRITE(IOUNI1,295)
  295     FORMAT(11X,'STATVAL',8X,'STATCDF',8X,'PVALUE',
     1            8X,'STATNU1',8X,'STATNU2',9X,'POOLSD',
     1            7X,'CUTUPP50',7X,'CUTUPP75',
     1            7X,'CUTUPP90',7X,'CUTUPP95',7X,'CUTUP975',
     1            7X,'CUTLOW99',7X,'CUTUPP99')
        ENDIF
        WRITE(IOUNI1,299)STATVA,STATCD,PVAL,STANU1,STANU2,POOLSD,
     1                   CUTU50,CUTU75,CUTU90,CUTU95,CUT975,
     1                   CUTU99,CUT999
  299   FORMAT(13E15.7)
      ELSEIF(IFLAGU.EQ.'ON')THEN
        IF(STATVA.NE.CPUMIN)THEN
          IH='STAT'
          IH2='VAL '
          VALUE0=STATVA
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(STATCD.NE.CPUMIN)THEN
          IH='STAT'
          IH2='CDF '
          VALUE0=STATCD
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(PVAL.NE.CPUMIN)THEN
          IH='PVAL'
          IH2='UE  '
          VALUE0=PVAL
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(STANU1.NE.CPUMIN)THEN
          IH='STAT'
          IH2='NU1 '
          VALUE0=STANU1
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(STANU2.NE.CPUMIN)THEN
          IH='STAT'
          IH2='NU2 '
          VALUE0=STANU2
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(POOLSD.NE.CPUMIN)THEN
          IH='POOL'
          IH2='SD  '
          VALUE0=POOLSD
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(CUTU50.NE.CPUMIN)THEN
          IH='CUTU'
          IH2='PP50'
          VALUE0=CUTU50
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(CUTU75.NE.CPUMIN)THEN
          IH='CUTU'
          IH2='PP75'
          VALUE0=CUTU75
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(CUTU90.NE.CPUMIN)THEN
          IH='CUTU'
          IH2='PP90'
          VALUE0=CUTU90
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(CUTU95.NE.CPUMIN)THEN
          IH='CUTU'
          IH2='PP95'
          VALUE0=CUTU95
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(CUT975.NE.CPUMIN)THEN
          IH='CUTU'
          IH2='P975'
          VALUE0=CUT975
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(CUTU99.NE.CPUMIN)THEN
          IH='CUTU'
          IH2='PP99'
          VALUE0=CUTU99
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
        IF(CUT999.NE.CPUMIN)THEN
          IH='CUTU'
          IH2='P999'
          VALUE0=CUT999
          CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1                IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1                IANS,IWIDTH,IBUGA3,IERROR)
        ENDIF
C
      ENDIF
C
      IF(IFLAGU.EQ.'FILE')THEN
        IF(ILAST)THEN
          IOP='CLOS'
          IFLAG1=1
          IFLAG2=0
          IFLAG3=0
          IFLAG4=0
          IFLAG5=0
          CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1                IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1                IBUGA3,ISUBRO,IERROR)
C
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'FTE5')THEN
            ISTEPN='3A'
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,301)IERROR,IOUNI1
  301       FORMAT('AFTER CALL DPCLFI, IERROR,IOUNI1 = ',A4,2X,I5)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          IF(IERROR.EQ.'YES')GOTO9000
        ENDIF
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'FTE5')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DPFTE5--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFUEV(ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,
     1                  IA,PARAM,IPARN,IPARN2,
     1                  IFOUNZ,IBEGIN,IEND,ITYPE,IHOL,IHOL2,
     1                  INT1,FLOAT1,IERRO1,
     1                  NUMCL,NUMPL,NUMAOL,ITYW1L,ICAT1L,INLI1L,ITYW2L,
     1                  NUMCR,NUMPR,NUMAOR,ITYW1R,ICAT1R,INLI1R,ITYW2R,
     1                  IANGLU,
     1                  IBUGA3,IBUGCO,IBUGEV,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--TREAT THE TYPE 6 LET CASE--
C              COMPUTING A GENERAL FUNCTION
C              (FOR A PARAMETER, A FULL VARIABLE,
C              OR PART OF A VARIABLE).
C     OUTPUT--A PARAMETER OR A VARIABLE.
C     EXAMPLE--IN THE FOLLOWING EXAMPLES,
C              A REPRESENTS A PREVIOUSLY-DEFINED PARAMETER
C              B REPRESENTS A PREVIOUSLY-DEFINED PARAMETER
C              X REPRESENTS A PREVIOUSLY-DEFINED VARIABLE (VECTOR)
C              Y REPRESENTS A PREVIOUSLY-DEFINED VARIABLE (VECTOR)
C              U REPRESENTS AN UNYET-DEFINED TERM
C              I REPRESENTS A DUMMY VARIABLE
C                     --LET A    = I                         (ILLEGAL)
C                     --LET A    = X(2)                      (A PARAMETER)
C                     --LET A    = 3*SIN(4)                  (A PARAMETER)
C                     --LET A    = B*SIN(B)                  (A PARAMETER)
C                     --LET A    = X*SIN(X)                  (ILLEGAL)
C
C                     --LET Y    = I                         (ILLEGAL)
C                     --LET Y    = X(2)                      (ILLEGAL)
C                     --LET Y    = 3*SIN(4)                  (ILLEGAL)
C                     --LET Y    = B*SIN(B)                  (ILLEGAL)
C                     --LET Y    = X*SIN(X)                  (A FULL VARIABLE)
C
C                     --LET Y(I) = I                         (A FULL VARIABLE)
C                     --LET Y(I) = X(2)                      (A FULL VARIABLE)
C                     --LET Y(I) = 3*SIN(4)                  (A FULL VARIABLE)
C                     --LET Y(I) = B*SIN(B)                  (A FULL VARIABLE)
C                     --LET Y(I) = X*SIN(X)                  (A FULL VARIABLE)
C
C                     --LET Y(2) = I                         (ILLEGAL)
C                     --LET Y(2) = X(2)                      (AN EL. OF A VAR.)
C                     --LET Y(2) = 3*SIN(4)                  (AN EL. OF A VAR.)
C                     --LET Y(2) = B*SIN(B)                  (AN EL. OF A VAR.)
C                     --LET Y(2) = X*SIN(X)                  (ILLEGAL)
C
C                     --LET U    = I                         (ILLEGAL)
C                     --LET U    = X(2)                      (A PARAMETER)
C                     --LET U    = 3*SIN(4)                  (A PARAMETER)
C                     --LET U    = B*SIN(B)                  (A PARAMETER)
C                     --LET U    = X*SIN(X)                  (A FULL VARIABLE)
C
C                     --LET U(I) = I                         (ILLEGAL)
C                     --LET U(I) = X(2)                      (ILLEGAL)
C                     --LET U(I) = 3*SIN(4)                  (ILLEGAL)
C                     --LET U(I) = B*SIN(B)                  (ILLEGAL)
C                     --LET U(I) = X*SIN(X)                  (A FULL VARIABLE)
C
C                     --LET U(2) = I                         (ILLEGAL)
C                     --LET U(2) = X(2)                      (AN EL. OF A VAR.)
C                     --LET U(2) = 3*SIN(4)                  (AN EL. OF A VAR.)
C                     --LET U(2) = B*SIN(B)                  (AN EL. OF A VAR.)
C                     --LET U(2) = X*SIN(X)                  (ILLEGAL)
C                ********************************
C
C                     --LET A    = I         SUBSET 2 3 5    (ILLEGAL)
C                     --LET A    = X(2)      SUBSET 2 3 5    (ILLEGAL)
C                     --LET A    = 3*SIN(4)  SUBSET 2 3 5    (ILLEGAL)
C                     --LET A    = B*SIN(B)  SUBSET 2 3 5    (ILLEGAL)
C                     --LET A    = X*SIN(X)  SUBSET 2 3 5    (ILLEGAL)
C
C                     --LET Y    = I         SUBSET 2 3 5    (A PARTIAL VAR.)
C                     --LET Y    = X(2)      SUBSET 2 3 5    (A PARTIAL VAR.)
C                     --LET Y    = 3*SIN(4)  SUBSET 2 3 5    (A PARTIAL VAR.)
C                     --LET Y    = B*SIN(B)  SUBSET 2 3 5    (A PARTIAL VAR.)
C                     --LET Y    = X*SIN(X)  SUBSET 2 3 5    (A PARTIAL VAR.)
C
C                     --LET Y(I) = I         SUBSET 2 3 5    (A PARTIAL VAR.)
C                     --LET Y(I) = X(2)      SUBSET 2 3 5    (A PARTIAL VAR.)
C                     --LET Y(I) = 3*SIN(4)  SUBSET 2 3 5    (A PARTIAL VAR.)
C                     --LET Y(I) = B*SIN(B)  SUBSET 2 3 5    (A PARTIAL VAR.)
C                     --LET Y(I) = X*SIN(X)  SUBSET 2 3 5    (A PARTIAL VAR.)
C
C                     --LET Y(2) = I         SUBSET 2 3 5    (ILLEGAL)
C                     --LET Y(2) = X(2)      SUBSET 2 3 5    (ILLEGAL)
C                     --LET Y(2) = 3*SIN(4)  SUBSET 2 3 5    (ILLEGAL)
C                     --LET Y(2) = B*SIN(B)  SUBSET 2 3 5    (ILLEGAL)
C                     --LET Y(2) = X*SIN(X)  SUBSET 2 3 5    (ILLEGAL)
C
C                     --LET U    = I         SUBSET 2 3 5    (A PARTIAL VAR.)
C                     --LET U    = X(2)      SUBSET 2 3 5    (A PARTIAL VAR.)
C                     --LET U    = 3*SIN(4)  SUBSET 2 3 5    (A PARTIAL VAR.)
C                     --LET U    = B*SIN(B)  SUBSET 2 3 5    (A PARTIAL VAR.)
C                     --LET U    = X*SIN(X)  SUBSET 2 3 5    (A PARTIAL VAR.)
C
C                     --LET U(I) = I         SUBSET 2 3 5    (A PARTIAL VAR.)
C                     --LET U(I) = X(2)      SUBSET 2 3 5    (A PARTIAL VAR.)
C                     --LET U(I) = 3*SIN(4)  SUBSET 2 3 5    (A PARTIAL VAR.)
C                     --LET U(I) = B*SIN(B)  SUBSET 2 3 5    (A PARTIAL VAR.)
C                     --LET U(I) = X*SIN(X)  SUBSET 2 3 5    (A PARTIAL VAR.)
C
C                     --LET U(2) = I         SUBSET 2 3 5    (ILLEGAL)
C                     --LET U(2) = X(2)      SUBSET 2 3 5    (ILLEGAL)
C                     --LET U(2) = 3*SIN(4)  SUBSET 2 3 5    (ILLEGAL)
C                     --LET U(2) = B*SIN(B)  SUBSET 2 3 5    (ILLEGAL)
C                     --LET U(2) = X*SIN(X)  SUBSET 2 3 5    (ILLEGAL)
C
C                ********************************
C
C                     --LET A    = I         FOR I = 1 2 10  (ILLEGAL)
C                     --LET A    = X(2)      FOR I = 1 2 10  (ILLEGAL)
C                     --LET A    = 3*SIN(4)  FOR I = 1 2 10  (ILLEGAL)
C                     --LET A    = B*SIN(B)  FOR I = 1 2 10  (ILLEGAL)
C                     --LET A    = X*SIN(X)  FOR I = 1 2 10  (ILLEGAL)
C
C                     --LET Y    = I         FOR I = 1 2 10  (A PARTIAL VAR.)
C                     --LET Y    = X(2)      FOR I = 1 2 10  (A PARTIAL VAR.)
C                     --LET Y    = 3*SIN(4)  FOR I = 1 2 10  (A PARTIAL VAR.)
C                     --LET Y    = B*SIN(B)  FOR I = 1 2 10  (A PARTIAL VAR.)
C                     --LET Y    = X*SIN(X)  FOR I = 1 2 10  (A PARTIAL VAR.)
C
C                     --LET Y(I) = I         FOR I = 1 2 10  (A PARTIAL VAR.)
C                     --LET Y(I) = X(2)      FOR I = 1 2 10  (A PARTIAL VAR.)
C                     --LET Y(I) = 3*SIN(4)  FOR I = 1 2 10  (A PARTIAL VAR.)
C                     --LET Y(I) = B*SIN(B)  FOR I = 1 2 10  (A PARTIAL VAR.)
C                     --LET Y(I) = X*SIN(X)  FOR I = 1 2 10  (A PARTIAL VAR.)
C
C                     --LET Y(2) = I         FOR I = 1 2 10  (ILLEGAL)
C                     --LET Y(2) = X(2)      FOR I = 1 2 10  (ILLEGAL)
C                     --LET Y(2) = 3*SIN(4)  FOR I = 1 2 10  (ILLEGAL)
C                     --LET Y(2) = B*SIN(B)  FOR I = 1 2 10  (ILLEGAL)
C                     --LET Y(2) = X*SIN(X)  FOR I = 1 2 10  (ILLEGAL)
C
C                     --LET U    = I         FOR I = 1 2 10  (A PARTIAL VAR.)
C                     --LET U    = X(2)      FOR I = 1 2 10  (A PARTIAL VAR.)
C                     --LET U    = 3*SIN(4)  FOR I = 1 2 10  (A PARTIAL VAR.)
C                     --LET U    = B*SIN(B)  FOR I = 1 2 10  (A PARTIAL VAR.)
C                     --LET U    = X*SIN(X)  FOR I = 1 2 10  (A PARTIAL VAR.)
C
C                     --LET U(I) = I         FOR I = 1 2 10  (A PARTIAL VAR.)
C                     --LET U(I) = X(2)      FOR I = 1 2 10  (A PARTIAL VAR.)
C                     --LET U(I) = 3*SIN(4)  FOR I = 1 2 10  (A PARTIAL VAR.)
C                     --LET U(I) = B*SIN(B)  FOR I = 1 2 10  (A PARTIAL VAR.)
C                     --LET U(I) = X*SIN(X)  FOR I = 1 2 10  (A PARTIAL VAR.)
C
C                     --LET U(2) = I         FOR I = 1 2 10  (ILLEGAL)
C                     --LET U(2) = X(2)      FOR I = 1 2 10  (ILLEGAL)
C                     --LET U(2) = 3*SIN(4)  FOR I = 1 2 10  (ILLEGAL)
C                     --LET U(2) = B*SIN(B)  FOR I = 1 2 10  (ILLEGAL)
C                     --LET U(2) = X*SIN(X)  FOR I = 1 2 10  (ILLEGAL)
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
C     ORIGINAL VERSION (IN DPLET)--DECEMBER 1977.
C     ORIGINAL VERSION AS A SEPARATE SUBROUTINE--MARCH 1978.
C     UPDATED         --MAY       1982.
C     UPDATED         --JULY      1978.
C     UPDATED         --NOVEMBER  1978.
C     UPDATED         --FEBRUARY  1979.
C     UPDATED         --MARCH     1979.
C     UPDATED         --JUNE      1981.
C     UPDATED         --SEPTEMBER 1981.
C     UPDATED         --OCTOBER   1981.
C     UPDATED         --NOVEMBER  1981.
C     UPDATED         --JANUARY   1982.
C     UPDATED         --APRIL     1982.
C     UPDATED         --MARCH     1986.
C     UPDATED         --JANUARY   1988. CUTOFF VALUE FOR CDC COMPUTERS
C     UPDATED         --MARCH     1988. FIX LET PRED=... SUBSET/FOR/ALL
C     UPDATED         --DECEMBER  1988. FIX LET Y(K) = X(K) INSIDE LOOP
C     UPDATED         --FEBRUARY  1989. CUTOFF VALUE FOR CDC 205 COMPUTER
C     UPDATED         --MARCH     2003. FOR PARAMETERS, CHECK FOR
C                                       IVALUE > LARGEST MACHINE INTEGER
C     UPDATED         --FEBRUARY  2005. IF FUNCTION DEFINED WITH
C                                       "LET STRING", CASE PRESERVED.
C                                       WHEN FUNCTION EXTRACTED IN
C                                       THIS CONTEXT, NEED TO CONVERT
C                                       LOWER CASE TO UPPER CASE
C     UPDATED         --JULY      2007. FIX BUG WHEN HAVE EMPTY SUBSET
C     UPDATED         --JULY      2020. SOME RECODING FOR BETTER
C                                       READABILITY
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ITYPEH
      CHARACTER*4 IW21HO
      CHARACTER*4 IW22HO
      CHARACTER*4 IA
      CHARACTER*4 IPARN
      CHARACTER*4 IPARN2
      CHARACTER*4 IFOUNZ
      CHARACTER*4 ITYPE
      CHARACTER*4 IHOL
      CHARACTER*4 IHOL2
      CHARACTER*4 IERRO1
      CHARACTER*4 ITYW1L
      CHARACTER*4 ICAT1L
      CHARACTER*4 INLI1L
      CHARACTER*4 ITYW2L
      CHARACTER*4 ITYW1R
      CHARACTER*4 ICAT1R
      CHARACTER*4 INLI1R
      CHARACTER*4 ITYW2R
      CHARACTER*4 IANGLU
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGCO
      CHARACTER*4 IBUGEV
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWD1
      CHARACTER*4 IWD2
      CHARACTER*4 IWD12
      CHARACTER*4 IWD22
      CHARACTER*4 IVOLDR
      CHARACTER*4 IVOLR2
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 NEWNAM
      CHARACTER*4 NEWCOL
      CHARACTER*4 IVNEWR
      CHARACTER*4 IVNER2
      CHARACTER*4 ICASEL
      CHARACTER*4 ICASER
      CHARACTER*4 ICASEQ
      CHARACTER*4 ICASIF
      CHARACTER*4 IPJ
      CHARACTER*4 IPJ2
      CHARACTER*4 IHSET
      CHARACTER*4 IHSET2
      CHARACTER*4 ILEFT
      CHARACTER*4 ILEFT2
      CHARACTER*4 IRIGHT
      CHARACTER*4 IRIGH2
      CHARACTER*4 IARG4F
      CHARACTER*4 IARG4T
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZC.INC'
      CHARACTER*4 IFSAVE(MAXF1)
      EQUIVALENCE (CGARBG(1),IFSAVE(1))
C
C---------------------------------------------------------------------
C
      DIMENSION IFOUNZ(*)
      DIMENSION IBEGIN(*)
      DIMENSION IEND(*)
      DIMENSION ITYPE(*)
      DIMENSION IHOL(*)
      DIMENSION IHOL2(*)
      DIMENSION INT1(*)
      DIMENSION FLOAT1(*)
      DIMENSION IERRO1(*)
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
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPFU'
      ISUBN2='EV  '
      IFOUND='NO'
      IERROR='NO'
C
      NILEFT=0
      NIRIGH=0
      ICOLR=0
C
C  CONVERT FUNCTION TABLE TO UPPER CASE, BUT SAVE ORIGINAL FIRST
C
      DO10I=1,NUMCHF
        IFSAVE(I)=IFUNC(I)
CCCCC   CALL DPCOAN(IFSAVE(I)(1:1),IATEMP)
CCCCC   IF(IATEMP.GE.97 .AND. IATEMP.LE.122)THEN
CCCCC     IATEMP=IATEMP-32
CCCCC     CALL DPCONA(IATEMP,IFSAVE(I)(1:1))
CCCCC   ENDIF
   10 CONTINUE
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
C               *******************************************************
C               **  TREAT THE CASE OF A GENERAL FUNCTION EVALUATION  **
C               **        1) FOR A PARAMETER,                        **
C               **        2) FOR A FULL VARIABLE, OR                 **
C               **        3) FOR PART OF A VARIABLE.                 **
C               *******************************************************
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FUEV')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPFUEV--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGA3,IBUGCO,IBUGEV,IBUGQ,IANGLU
   53   FORMAT('IBUGA3,IBUGCO,IBUGEV,IBUGQ,IANGLU = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)ICAT1L,ITYW1L,ITYW2L,IERRO1(1)
   55   FORMAT('ICAT1L,ITYW1L,ITYW2L,IERROR(1) = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,56)NUMAOL,NUMCL,NUMPL
   56   FORMAT('NUMAOL,NUMCL,NUMPL = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,57)NUMNAM,IBEGIN(1),IEND(1),INLI1L
   57   FORMAT('NUMNAM,IBEGIN(1),IEND(1),INLI1L = ',3I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO60I=1,NUMNAM
          WRITE(ICOUT,61)I,IHNAME(I),IHNAM2(I),IUSE(I),
     1                   IVALUE(I),VALUE(I)
   61     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVALUE(I),VALUE(I) = ',
     1           I8,3(2X,A4),I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   60   CONTINUE
      ENDIF
C
C               **********************************
C               **  STEP 1--                    **
C               **  INITIALIZE SOME VARIABLES.  **
C               **********************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FUEV')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NEWNAM='NO'
      NEWCOL='NO'
C
      MAXN2=MAXCHF
      MAXN3=MAXCHF
      MAXN4=MAXCHF
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FUEV')THEN
        WRITE(ICOUT,91)
   91   FORMAT('I,IFOUNZ(I),ITYPE(I),IHOL(I),IHOL2(I),INT1(I),',
     1         'FLOAT1(I)--')
        CALL DPWRST('XXX','BUG ')
        DO92I=1,30
          WRITE(ICOUT,93)I,IFOUNZ(I),ITYPE(I),IHOL(I),IHOL2(I),INT1(I),
     1                   FLOAT1(I)
   93     FORMAT(I3,4(2X,A4),2X,I8,2X,G15.7)
          CALL DPWRST('XXX','BUG ')
   92   CONTINUE
      ENDIF
C
C               ****************************************************************
C               **  STEP 2--                                                   *
C               **  EXAMINE THE LEFT-HAND SIDE--                               *
C               **  IS THE PARAMETER OR VARIABLE NAME TO LEFT OF = SIGN        *
C               **  ALREADY IN THE NAME LIST?                                  *
C               **  IS IT A PARAMETER OR A VARIABLE?                           *
C               **  NOTE THAT     ILEFT     IS THE NAME OF THE VARIABLE        *
C               **  ON THE LEFT.                                               *
C               **  NOTE THAT     ILISTL    IS THE LINE IN THE TABLE           *
C               **  OF THE NAME ON THE LEFT.                                   *
C               **  NOTE THAT     ICOLL    IS THE DATA COLUMN (1 TO 12)        *
C               **  FOR THE NAME OF THE LEFT.                                  *
C               **  WHEN THIS STEP IS FINISHED,                                *
C               **  ICASEL   WILL HAVE ONE OF THE FOLLOWING 3 VALUES--         *
C               **           1) PARAM                                          *
C               **           2) VAR                                            *
C               **           3) UNKNOWN (YET TO BE DEFINED; DEPENDS ON RIGHT). *
C               ****************************************************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FUEV')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASEL='UNKN'
      ILEFT=IHOL(2)
      ILEFT2=IHOL2(2)
      DO2000I=1,NUMNAM
        I2=I
        IF(ILEFT.EQ.IHNAME(I).AND.ILEFT2.EQ.IHNAM2(I))THEN
          IF(IUSE(I).EQ.'P')THEN
            ICASEL='PARA'
            ILISTL=I2
            GOTO2900
          ELSEIF(IUSE(I).EQ.'V')THEN
            ICASEL='VAR'
            ILISTL=I2
            ICOLL=IVALUE(ILISTL)
            NILEFT=IN(ILISTL)
            GOTO2900
          ENDIF
        ENDIF
 2000 CONTINUE
      ICASEL='UNKN'
      NEWNAM='YES'
      ILISTL=NUMNAM+1
      IF(ILISTL.GT.MAXNAM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2801)
 2801   FORMAT('***** ERROR IN DPFUEV--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2802)
 2802   FORMAT('      THE NUMBER OF VARIABLE AND/OR PARAMETER')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2803)MAXNAM
 2803   FORMAT('      NAMES HAS JUST EXCEEDED THE MAX ALLOWABLE ',
     1         I8,'  .')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2804)
 2804   FORMAT('      SUGGESTED ACTION--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2805)
 2805   FORMAT('      ENTER      STAT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2806)
 2806   FORMAT('      TO FIND OUT THE FULL LIST OF USED NAMES, AND ',
     1         'THEN')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2807)
 2807   FORMAT('      REDEFINE (REUSE) SOME OF THE ALREADY USED NAMES.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO19000
      ENDIF
      IN(ILISTL)=0
C
 2900 CONTINUE
C
C               **************************************************
C               **  STEP 3--                                    **
C               **  EXAMINE THE RIGHT-HAND SIDE--               **
C               **  1)  FIRST, SCREEN OUT THE DUMMY             **
C               **      AND THE ELEMENT SPECIFICATION CASES;    **
C               **  2)  THEN EXTRACT THE FUNCTIONAL EXPRESSION; **
C               **  3)  DETERMINE THE TYPE OF QUALIFIERS--      **
C               **      A)  NONE (= FULL = UNQUALIFIED);        **
C               **      B)  SUBSET/EXCEPT; OR                   **
C               **      C)  FOR.                                **
C               **  4)  EXAMINE THE FUNCTION    AL EXPRESSION   **
C               **      FOR PARAMETERS AND VARIABLES.           **
C               **  WHEN THIS STEP IS FINISHED,                 **
C               **  ICASER  WILL BE FULLY DETERMINED AND        **
C               **  WILL HAVE ONE OF THE FOLLOWING 4 VALUES--   **
C               **          1) DUMMY;                           **
C               **          2) ELEMENT;                         **
C               **          3) PARAM (NO VARIABLES);            **
C               **          4) VAR (AT LEAST ONE VARIABLE).     **
C               **  WHEN THIS STEP IS FINISHED,                 **
C               **  ICASEQ  WILL BE FILLY DETERMINED AND        **
C               **  WILL HAVE ONE OF THE FOLLOWING 3 VALUES--   **
C               **          1) FULL;                            **
C               **          2) SUBSET/EXCEPT OR;                **
C               **          3) FOR.                             **
C               **************************************************
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FUEV')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASER='UNKN'
      ICASEQ='UNKN'
      IF(NUMCR.EQ.1 .AND. NUMPR.EQ.0 .AND. NUMAOR.EQ.0 .AND.
     1   ITYW1R.EQ.'WORD' .AND. INLI1R.EQ.'NO')THEN
        ICASER='DUMM'
        IF(IFOUNZ(11).EQ.'NO'.AND.IFOUNZ(21).EQ.'NO')ICASEQ='FULL'
        IF(IFOUNZ(11).EQ.'YES')ICASEQ='SUBS'
        IF(IFOUNZ(21).EQ.'YES')ICASEQ='FOR'
        GOTO3990
      ELSEIF(1.LE.NUMCR .AND. NUMCR.LE.4 .AND. NUMPR.EQ.2 .AND.
     1       NUMAOR.EQ.0 .AND. ITYW1R.EQ.'WORD' .AND.
     1       ICAT1R.EQ.'VARP' .AND. INLI1R.EQ.'YES')THEN
        IF(ITYW2R.EQ.'NUMB' .OR. ITYW2R.EQ.'WORD')THEN
          ICASER='ELEM'
          IRIGHT=IHOL(7)
          IRIGH2=IHOL2(7)
          DO3030I=1,NUMNAM
            I2=I
            IF(IRIGHT.EQ.IHNAME(I) .AND. IRIGH2.EQ.IHNAM2(I) .AND.
     1         IUSE(I).EQ.'P')THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2801)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,3032)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,3044)
 3044         FORMAT('      WAS FOUND IN THE INTERNAL NAME LIST, BUT ',
     1               'AS A')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,3046)
 3046         FORMAT('      PARAMETER, AND NOT AS A VARIABLE AS IT ',
     1               'SHOULD BE HERE.')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,3035)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,3036)(IANS(II),II=1,MIN(100,IWIDTH))
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GOTO19000
            ELSEIF(IRIGHT.EQ.IHNAME(I) .AND. IRIGH2.EQ.IHNAM2(I) .AND.
     1             IUSE(I).EQ.'V')THEN
              ILISTR=I2
              ICOLR=IVALUE(ILISTR)
              NIRIGH=IN(ILISTR)
C
              IARGIR=INT1(9)
              IF(IARGIR.GE.1 .AND. IARGIR.LE.MAXN)THEN
                IF(IFOUNZ(11).EQ.'NO'.AND.IFOUNZ(21).EQ.'NO')THEN
                   ICASEQ='FULL'
                ELSEIF(IFOUNZ(11).EQ.'YES')THEN
                  ICASEQ='SUBS'
                ELSEIF(IFOUNZ(21).EQ.'YES')THEN
                  ICASEQ='FOR'
                ENDIF
                GOTO3990
              ENDIF
              WRITE(ICOUT,2801)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,3062)
 3062         FORMAT('      THE SPECIFIED ARGUMENT (ROW NUMBER) ON THE')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,3064)
 3064         FORMAT('      RIGHT SIDE OF THE = SIGN IS SMALLER THAN')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,3065)
 3065         FORMAT('      1 OR LARGER THAN THE MAXIMUM ALLOWABLE '
     1               'NUMBER')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,3066)MAXN
 3066         FORMAT('      (',I6,')  FOR A GIVEN VARIABLE.')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,3067)IARGIR
 3067         FORMAT('      THE VALUE (IARGIR) = ',I8)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,3035)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,3036)(IANS(II),II=1,MIN(100,IWIDTH))
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GOTO19000
            ENDIF
 3030     CONTINUE
C
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2801)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3032)
 3032     FORMAT('      THE VARIABLE NAME ON THE RIGHT OF THE = SIGN')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3034)
 3034     FORMAT('      WAS NOT FOUND IN THE INTERNAL NAME LIST.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3035)
 3035     FORMAT('      THE COMMAND LINE WAS AS FOLLOWS--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3036)(IANS(I),I=1,MIN(100,IWIDTH))
 3036     FORMAT(100A1)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO19000
        ENDIF
      ENDIF
      GOTO3090
C
 3090 CONTINUE
      ICASEQ='UNKN'
C
C     LOCATE THE EQUAL SIGN.
C
      DO3100I=1,IWIDTH
        I2=I
        IF(IANS(I).EQ.'=')GOTO3150
 3100 CONTINUE
      GOTO3400
 3150 CONTINUE
      ISTART=I2
C
      IF(ISTART.GT.IWIDTH)GOTO3400
      DO3200I=ISTART,IWIDTH
        I2=I
        IP1=I+1
        IP2=I+2
        IP3=I+3
        IP4=I+4
        IP5=I+5
        IP6=I+6
        IP7=I+7
        IF(IP7.GT.IWIDTH)THEN
          ICASEQ='FULL'
          ISTOP=IWIDTH
          GOTO3290
        ENDIF
        IF(IANS(I).EQ.' '   .AND. IANS(IP1).EQ.'S' .AND.
     1     IANS(IP2).EQ.'U' .AND. IANS(IP3).EQ.'B' .AND.
     1     IANS(IP4).EQ.'S' .AND. IANS(IP5).EQ.'E' .AND.
     1     IANS(IP6).EQ.'T' .AND. IANS(IP7).EQ.' ')THEN
          ICASEQ='SUBS'
          ISTOP=I2
          GOTO3290
        ELSEIF(IANS(I).EQ.' '   .AND. IANS(IP1).EQ.'F' .AND.
     1         IANS(IP2).EQ.'O' .AND. IANS(IP3).EQ.'R' .AND.
     1         IANS(IP4).EQ.' ')THEN
          ICASEQ='FOR'
          ISTOP=I2
          GOTO3290
        ELSEIF(IANS(I).EQ.' '   .AND. IANS(IP1).EQ.'I' .AND.
     1         IANS(IP2).EQ.'F' .AND. IANS(IP3).EQ.' ')THEN
          ICASEQ='IF'
          ISTOP=I2
          GOTO3290
        ENDIF
 3200 CONTINUE
C
 3290 CONTINUE
C
C               ***************************************
C               **  STEP 3.1--                       **
C               **  EXTRACT THE UNDERLYING FUNCTION  **
C               **  FROM FUNCTION DEFINITIONS.       **
C               ***************************************
C
      ISTEPN='3.1'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FUEV')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC J=0
CCCCC IMIN=ISTART+1
CCCCC DO3370I=IMIN,ISTOP
CCCCC J=J+1
CCCCC IA(J)=IANS(I)
C3370 CONTINUE
CCCCC NUMCHA=J
C
      IWD1='=   '
      IWD12='    '
      IF(ICASEQ.EQ.'FULL')THEN
        IWD2='    '
        IWD22='    '
      ELSEIF(ICASEQ.EQ.'SUBS'.AND.IHOL(11).EQ.'SUBS')THEN
        IWD2='SUBS'
        IWD22='ET  '
      ELSEIF(ICASEQ.EQ.'SUBS'.AND.IHOL(11).EQ.'EXCE')THEN
        IWD2='EXCE'
        IWD22='PT  '
      ELSEIF(ICASEQ.EQ.'FOR ')THEN
        IWD2='FOR '
        IWD22='    '
      ELSEIF(ICASEQ.EQ.'IF  ')THEN
        IWD2='IF  '
        IWD22='    '
      ENDIF
      CALL DPEXST(IANS,IWIDTH,IWD1,IWD12,IWD2,IWD22,MAXN2,
     1            IFUNC2,N2,IBUGA3,IFOUND,IERROR)
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FUEV')THEN
        WRITE(ICOUT,3011)
 3011   FORMAT('***** FROM DPFUEV, AFTER  THE CALL TO DPEXST--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IERROR.EQ.'YES')GOTO19000
      IF(IFOUND.EQ.'YES')GOTO3379
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2801)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3372)
 3372 FORMAT('      INVALID COMMAND FORM FOR FUNCTION EVALUATION.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3373)
 3373 FORMAT('      GENERAL FORM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3374)
 3374 FORMAT('      LET ... = ...  SUBSET ... ... ...')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3035)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3036)(IANS(I),I=1,MIN(100,IWIDTH))
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO19000
C
 3379 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FUEV')THEN
        WRITE(ICOUT,3012)
 3012   FORMAT('***** FROM DPFUEV, BEFORE THE CALL TO DPEXFU--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      DO3018I=1,N2
        IA(I)=IFUNC2(I)
        ITEMP=ICHAR(IFUNC2(I)(1:1))
        IF(ITEMP.GE.97 .AND. ITEMP.LE.122)THEN
          ITEMP=ITEMP-32
          IFUNC2(I)(1:1)=CHAR(ITEMP)
        ENDIF
 3018 CONTINUE
C
      CALL DPEXFU(IFUNC2,N2,IHNAME,IHNAM2,IUSE,IVSTAR,IVSTOP,
     1            NUMNAM,IANS,IWIDTH,IFUNC,NUMCHF,MAXCHF,IFUNC3,
     1            N3,MAXN3,
     1            IBUGA3,IERROR)
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FUEV')THEN
        WRITE(ICOUT,3013)
 3013   FORMAT('***** FROM DPFUEV, AFTER  THE CALL TO DPEXFU--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IERROR.EQ.'YES')GOTO19000
C
      J=0
      DO3380I=1,N3
        J=J+1
        IA(J)=IFUNC3(I)
        ITEMP=ICHAR(IA(J)(1:1))
        IF(ITEMP.GE.97 .AND. ITEMP.LE.122)THEN
          ITEMP=ITEMP-32
          IA(J)(1:1)=CHAR(ITEMP)
        ENDIF
 3380 CONTINUE
      NUMCHA=J
      GOTO3500
C
 3400 CONTINUE
      WRITE(ICOUT,3411)
 3411 FORMAT('***** INTERNAL ERROR IN DPFUEV')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3412)
 3412 FORMAT('      AT BRANCH POINT 3400--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3413)
 3413 FORMAT('      ISTART GREATER THAN ISTOP.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3035)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3036)(IANS(I),I=1,MIN(100,IWIDTH))
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO19000
C
 3500 CONTINUE
      ICASER='UNKN'
      IPASS=1
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FUEV')THEN
        WRITE(ICOUT,3014)
 3014   FORMAT('***** FROM DPFUEV, BEFORE THE CALL TO COMPIM--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      CALL COMPIM(IA,NUMCHA,IPASS,PARAM,IPARN,IPARN2,NUMPAR,
     1            IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,AJUNK,
     1            IBUGCO,IBUGEV,IERROR)
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FUEV')THEN
        WRITE(ICOUT,3015)
 3015   FORMAT('***** FROM DPFUEV, AFTER  THE CALL TO COMPIM--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IERROR.EQ.'YES')GOTO19000
C
      NUMP=0
      NUMV=0
      NIOLDR=0
      IVOLDR='JUNK'
      IVOLR2='JUNK'
      IF(NUMPAR.EQ.0)GOTO3900
      DO3600J=1,NUMPAR
        DO3700I=1,NUMNAM
          I2=I
          IF(IPARN(J).EQ.IHNAME(I) .AND. IPARN2(J).EQ.IHNAM2(I) .AND.
     1       IUSE(I).EQ.'P')THEN
            NUMP=NUMP+1
            GOTO3600
          ELSEIF(IPARN(J).EQ.IHNAME(I) .AND.
     1           IPARN2(J).EQ.IHNAM2(I) .AND. IUSE(I).EQ.'V')THEN
            NUMV=NUMV+1
            NIRIGH=IN(I2)
            NIOLDR=NINEWR
            IVOLDR=IVNEWR
            IVOLR2=IVNER2
            NINEWR=IN(I2)
            IVNEWR=IHNAME(I2)
            IVNER2=IHNAM2(I2)
            IF(NUMV.GE.2.AND.NINEWR.NE.NIOLDR)THEN
              WRITE(ICOUT,2801)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,3822)
 3822         FORMAT('      ALL VARIABLES USED IN A FUNCTIONAL ',
     1               'EXPRESSION')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,3824)
 3824         FORMAT('      MUST HAVE THE SAME LENGTH (NUMBER OF ',
     1               'ELEMENTS);')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,3826)
 3826         FORMAT('      SUCH WAS NOT THE CASE HERE FOR--')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,3827)IVOLDR,IVOLR2,NIOLDR
 3827         FORMAT('      VARIABLE ',2A4,' WITH ',I8,' ELEMENTS')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,3828)IVNEWR,IVNER2,NINEWR
 3828         FORMAT('      VARIABLE ',A4,A4,' WITH ',I8,' ELEMENTS')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,3035)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,3036)(IANS(II),II=1,MIN(100,IWIDTH))
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GOTO19000
            ENDIF
            GOTO3600
          ENDIF
 3700   CONTINUE
        WRITE(ICOUT,2801)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3802)
 3802   FORMAT('      A VARIABLE OR PARAMETER NAME USED')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3803)
 3803   FORMAT('      IN AN EXPRESSION IS NOT YET DEFINED.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3804)IPARN(J),IPARN2(J)
 3804   FORMAT('      VARIABLE OR PARAMETER NAME = ',2A4)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO19000
C
 3600 CONTINUE
C
 3900 CONTINUE
      ICASER='VAR'
      IF(NUMV.LE.0)ICASER='PARA'
C
 3990 CONTINUE
C
C               *******************************
C               **  STEP 4--                 **
C               **  DETERMINE THE SUBCASE    **
C               **  AND BRANCH ACCORDINGLY.  **
C               *******************************
C
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FUEV')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IARG4F=IFOUNZ(4)
      IARG4T=ITYPE(4)
      IARG4I=INT1(4)
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FUEV')THEN
        WRITE(ICOUT,4001)ICASEL,ICASER,ICASEQ,IARG4F,IARG4T
 4001   FORMAT('***** IN DPFUEV, AT START OF STEP 4; ',
     1         'ICASEL,ICASER,ICASEQ,IARG4F,IARG4T = ',4(A4,1X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(ICASEQ.EQ.'FULL' .OR. ICASEQ.EQ.'IF')THEN
C
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FUEV')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,4111)
 4111     FORMAT('***** IN MIDDLE OF DPFUEV, AT 4100--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,4112)ICASEL,ICASER,IHOL(4),IHOL2(4)
 4112     FORMAT('ICASEL,ICASER,IHOL(4),IHOL2(4) = ',3(A4,2X),A4)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,4113)IARG4F,IARG4T,IARG4I
 4113     FORMAT('IARG4F,IARG4T,IARG4I = ',2(A4,2X),I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(ICASEL.EQ.'PARA'.AND.IARG4F.EQ.'NO')THEN
          IF(ICASER.EQ.'PARA' .OR. ICASER.EQ.'ELEM')GOTO5000
        ENDIF
C
        IF(ICASEL.EQ.'VAR'.AND.IARG4F.EQ.'NO'.AND.
     1         ICASER.EQ.'VAR')GOTO7000
C
        IF(ICASEL.EQ.'VAR'.AND.IARG4F.EQ.'YES')THEN
          IF(IHOL(4).EQ.'I   '.AND.IHOL2(4).EQ.'    ')GOTO6000
          IF(IARG4T.EQ.'WORD'.AND.IARG4I.LE.0)GOTO7000
          IF(IARG4T.EQ.'WORD'.AND.IARG4I.GE.1)GOTO6000
          IF(IARG4T.EQ.'NUMB'.AND.ICASER.EQ.'PARA')GOTO6000
          IF(IARG4T.EQ.'NUMB'.AND.ICASER.EQ.'ELEM')GOTO6000
        ENDIF
C
        IF(ICASEL.EQ.'UNKN'.AND.IARG4F.EQ.'NO')THEN
          IF(ICASER.EQ.'PARA' .OR. ICASER.EQ.'ELEM')GOTO5000
          IF(ICASER.EQ.'VAR')GOTO7000
        ENDIF
C
        IF(ICASEL.EQ.'UNKN'.AND.IARG4F.EQ.'YES')THEN
          IF(IARG4T.EQ.'WORD'.AND.IARG4I.LE.0.AND.
     1       ICASER.EQ.'VAR')GOTO7000
          IF(IARG4T.EQ.'WORD'.AND.IARG4I.GE.1.AND.
     1       ICASER.EQ.'VAR')GOTO6000
          IF(IARG4T.EQ.'WORD'.AND.IARG4I.GE.1.AND.
     1       ICASER.EQ.'PARA')GOTO6000
          IF(IARG4T.EQ.'WORD'.AND.IARG4I.GE.1.AND.
     1       ICASER.EQ.'ELEM')GOTO6000
          IF(IARG4T.EQ.'NUMB'.AND.ICASER.EQ.'PARA')GOTO6000
          IF(IARG4T.EQ.'NUMB'.AND.ICASER.EQ.'ELEM')GOTO6000
        ENDIF
        GOTO4800
      ELSEIF(ICASEQ.EQ.'SUBS')THEN
        IF(ICASEL.EQ.'VAR'.AND.IARG4F.EQ.'NO')GOTO8000
        IF(ICASEL.EQ.'VAR'.AND.IARG4F.EQ.'YES'.AND.IARG4T.EQ.'WORD')
     1     GOTO8000
        IF(ICASEL.EQ.'UNKN'.AND.IARG4F.EQ.'NO')GOTO8000
        IF(ICASEL.EQ.'UNKN'.AND.IARG4F.EQ.'YES'.AND.IARG4T.EQ.'WORD')
     1     GOTO8000
        GOTO4800
      ELSEIF(ICASEQ.EQ.'FOR')THEN
        IF(ICASEL.EQ.'VAR'.AND.IARG4F.EQ.'NO')GOTO9000
        IF(ICASEL.EQ.'VAR'.AND.IARG4F.EQ.'YES'.AND.IARG4T.EQ.'WORD')
     1     GOTO9000
        IF(ICASEL.EQ.'UNKN'.AND.IARG4F.EQ.'NO')GOTO9000
        IF(ICASEL.EQ.'UNKN'.AND.IARG4F.EQ.'YES'.AND.IARG4T.EQ.'WORD')
     1     GOTO9000
        GOTO4800
      ENDIF
C
 4800 CONTINUE
      WRITE(ICOUT,2801)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4812)
 4812 FORMAT('      ILLEGAL SYNTAX FOR LET COMMAND')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4814)
 4814 FORMAT('      POSSIBLE CAUSE--UNDEFINED PARAMETER/VARIABLE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4815)
 4815 FORMAT('      ON RIGHT-HAND SIDE OF EQUAL SIGN.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4816)ICASEL,ICASER,ICASEQ
 4816 FORMAT(6X,'ICASEL, ICASER, ICASEQ = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3035)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3036)(IANS(I),I=1,MIN(100,IWIDTH))
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO19000
C
C               *****************************************************
C               **  STEP 5--                                       **
C               **  TREAT THE PARAMETER CASE.                      **
C               **  EXAMPLES--                                     **
C               **            LET A    = X(2)                      **
C               **            LET A    = 3*SIN(4)                  **
C               **            LET A    = B*SIN(B)                  **
C               **            LET U    = X(2)                      **
C               **            LET U    = 3*SIN(4)                  **
C               **            LET U    = B*SIN(B)                  **
C               **  WHERE A WAS A PREVIOUSLY-DEFINED PARAMETER     **
C               **  AND WHERE U WAS PREVIOUSLY UNDEFINED.          **
C               **  CARRY OUT THE LIST UPDATING  AND               **
C               **  GENERATE THE INFORMATIVE PRINTING.             **
C               **  THEN EXIT.                                     **
C               *****************************************************
C
 5000 CONTINUE
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FUEV')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASEQ.EQ.'IF')THEN
        ICASIF='TRUE'
        IHSET=IHOL(12)
        IHSET2=IHOL2(12)
C
C       2018/05: FIRST TEST FOR STRING (IF SUPPORTS CHECKING
C                FOR EQUALITY OF STRINGS).  IF NOT A STRING,
C                THEN CHECK FOR PARAMETER (WHICH IS THE MORE
C                LIKELY CASE).
C
        NISET=0
        IHWUSE='F'
        MESSAG='NO'
        CALL CHECKN(IHSET,IHSET2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)

        IF(IERROR.EQ.'YES')THEN
          IHWUSE='P'
          MESSAG='YES'
          CALL CHECKN(IHSET,IHSET2,IHWUSE,
     1                IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1                ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
          IF(IERROR.EQ.'YES')GOTO19000
          NISET=IN(ILOC)
        ENDIF
C
        CALL DPIF(ILOCS,ICASIF,IBUGQ,ISUBRO,IERROR)
      ENDIF
C
      IF(ICASER.EQ.'ELEM')THEN
        IF(ICASEQ.EQ.'IF'.AND.ICASIF.EQ.'FALS')GOTO5119
        IARG9I=INT1(9)
        IJ=MAXN*(ICOLR-1)+IARG9I
        IF(ICOLR.LE.MAXCOL)RIGHT=V(IJ)
        IF(ICOLR.EQ.MAXCP1)RIGHT=PRED(IARG9I)
        IF(ICOLR.EQ.MAXCP2)RIGHT=RES(IARG9I)
        IF(ICOLR.EQ.MAXCP3)RIGHT=YPLOT(IARG9I)
        IF(ICOLR.EQ.MAXCP4)RIGHT=XPLOT(IARG9I)
        IF(ICOLR.EQ.MAXCP5)RIGHT=X2PLOT(IARG9I)
        IF(ICOLR.EQ.MAXCP6)RIGHT=TAGPLO(IARG9I)
        GOTO5500
      ELSEIF(ICASER.EQ.'PARA')THEN
        IF(ICASEQ.EQ.'IF'.AND.ICASIF.EQ.'FALS')GOTO5119
        IF(NUMPAR.LE.0)GOTO5490
        DO5400J=1,NUMPAR
          IPJ=IPARN(J)
          IPJ2=IPARN2(J)
          DO5450I=1,NUMNAM
            I2=I
            IF(IPJ.EQ.IHNAME(I).AND.IPJ2.EQ.IHNAM2(I).AND.
     1         IUSE(I).EQ.'P')THEN
              PARAM(J)=VALUE(I2)
              GOTO5400
            ENDIF
 5450     CONTINUE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2801)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5482)
 5482     FORMAT('      AT BRANCH POINT 5481--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5483)
 5483     FORMAT('      PARAMETER NAME FOR FUNCTION EVALUATION')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5484)
 5484     FORMAT('      NOT FOUND IN INTERNAL LIST.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5485)IPJ,IPJ2
 5485     FORMAT('      PARAMETER NAME = ',A4,A4)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3035)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3036)(IANS(I),I=1,MIN(100,IWIDTH))
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO19000
 5400   CONTINUE
        GOTO5490
      ELSE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2801)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5102)
 5102   FORMAT('      AT BRANCH POINT 5101--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5103)ICASER
 5103   FORMAT('      ICASER = ',A4,' DETECTED')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5104)
 5104   FORMAT('      IN STEP 5 (PARAMETER CALCULATION).')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3035)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3036)(IANS(I),I=1,MIN(100,IWIDTH))
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO19000
      ENDIF
C
 5490 CONTINUE
      IPASS=2
      CALL COMPIM(IA,NUMCHA,IPASS,PARAM,IPARN,IPARN2,NUMPAR,
     1            IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,RIGHT,
     1            IBUGCO,IBUGEV,IERROR)
      IF(IERROR.EQ.'YES')GOTO19000
      GOTO5500
C
 5500 CONTINUE
      IFOUND='YES'
      IHNAME(ILISTL)=ILEFT
      IHNAM2(ILISTL)=ILEFT2
      IUSE(ILISTL)='P'
      VALUE(ILISTL)=RIGHT
CCCCC IVALUE(ILISTL)=VALUE(ILISTL)+0.5
C
CCCCC MARCH 2002.  CHANGE CODE BELOW.  BASE ON LARGEST INTEGER AS
CCCCC GIVEN IN DPCOMC.
CCCCC CUTOFF=2**(NUMBPW-3)
C3/02 ICUTMX=NUMBPW
C3/02 IF(IHOST1.EQ.'CDC '.OR.IHOST1.EQ.'CYBE')ICUTMX=48
CCCCC THE FOLLOWING LINE WAS ADDED FEBRUARY 1989
C3/02 IF(IHOST1.EQ.'205 ')ICUTMX=48
C3/02 CUTOFF=2**(ICUTMX-3)
      CUTOFF=REAL(I1MACH(9)-1)
C
      IF((-CUTOFF).LE.RIGHT.AND.RIGHT.LE.CUTOFF)THEN
        IVALUE(ILISTL)=INT(RIGHT+0.5)
      ELSEIF(RIGHT.GT.CUTOFF)THEN
        IVALUE(ILISTL)=I1MACH(9)-1
      ELSEIF(RIGHT.LT.(-CUTOFF))THEN
        IVALUE(ILISTL)=-(I1MACH(9)-1)
      ELSE
        IVALUE(ILISTL)=0
      ENDIF
      IN(ILISTL)=1
C
      IF(NEWNAM.EQ.'YES')NUMNAM=NUMNAM+1
C
      IF(IPRINT.EQ.'ON' .AND. IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,5111)ILEFT,ILEFT2,RIGHT
 5111   FORMAT('THE COMPUTED VALUE OF THE CONSTANT ',2A4,' = ',E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ENDIF
 5119 CONTINUE
      GOTO19000
C
C               *****************************************************
C               **  STEP 6--                                       **
C               **  TREAT THE ELEMENT SPECIFICATION CASE.          **
C               **  EXAMPLES--                                     **
C               **            LET Y(2) = X(2)                      **
C               **            LET Y(2) = 3*SIN(4)                  **
C               **            LET Y(2) = B*SIN(B)                  **
C               **            LET U(2) = X(2)                      **
C               **            LET U(2) = 3*SIN(4)                  **
C               **            LET U(2) = B*SIN(B)                  **
C               **  WHERE Y WAS A PREVIOUSLY-DEFINED VARIABLE      **
C               **  AND WHERE U WAS PREVIOUSLY UNDEFINED.          **
C               **  CARRY OUT THE LIST UPDATING  AND               **
C               **  GENERATE THE INFORMATIVE PRINTING.             **
C               **  THEN EXIT.                                     **
C               *****************************************************
C
 6000 CONTINUE
      ISTEPN='6'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FUEV')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IARG4I=INT1(4)
C
      IF(IARG4I.LT.1 .OR. IARG4I.GT.MAXN)THEN
        WRITE(ICOUT,2801)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6062)IARG4I,ILEFT,ILEFT2
 6062   FORMAT('      THE SPECIFIED ROW (',I8,') OF VARIABLE ',2A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6063)
 6063   FORMAT('      ON THE LEFT SIDE OF THE EQUAL SIGN WAS LESS THAN')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6065)MAXN
 6065   FORMAT('      1 OR GREATER THAN THE MAXIMUM ALLOWABLE ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO19000
      ENDIF
C
      IF(ICASEL.EQ.'VAR')ICOLL=IVALUE(ILISTL)
      IF(ICASEL.EQ.'UNKN')ICOLL=NUMCOL+1
      IF(ICOLL.GT.MAXCOL)THEN
        WRITE(ICOUT,2801)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6052)
 6052   FORMAT('      THE NUMBER OF DATA COLUMNS HAS JUST EXCEEDED THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6054)MAXCOL
 6054   FORMAT('      MAXIMUM ALLOWABLE ',I8,'.  SUGGESTED ACTION--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6055)
 6055   FORMAT('      ENTER      STAT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6056)
 6056   FORMAT('      TO FIND OUT THE FULL LIST OF USED COLUMNS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6057)
 6057   FORMAT('      AND THEN OVERWRITE SOME COLUMN.   ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3035)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3036)(IANS(I),I=1,MIN(100,IWIDTH))
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO19000
      ENDIF
C
      IF(ICASEL.EQ.'VAR'.AND.IARG4I.LE.NILEFT)NINEW=NILEFT
      IF(ICASEL.EQ.'VAR'.AND.IARG4I.GT.NILEFT)NINEW=IARG4I
      IF(ICASEL.EQ.'UNKN')NINEW=IARG4I
C
      IF(ICASER.EQ.'ELEM')THEN
        IARG9I=INT1(9)
        IJ=MAXN*(ICOLR-1)+IARG9I
        IF(ICOLR.LE.MAXCOL)RIGHT=V(IJ)
        IF(ICOLR.EQ.MAXCP1)RIGHT=PRED(IARG9I)
        IF(ICOLR.EQ.MAXCP2)RIGHT=RES(IARG9I)
        IF(ICOLR.EQ.MAXCP3)RIGHT=YPLOT(IARG9I)
        IF(ICOLR.EQ.MAXCP4)RIGHT=XPLOT(IARG9I)
        IF(ICOLR.EQ.MAXCP5)RIGHT=X2PLOT(IARG9I)
        IF(ICOLR.EQ.MAXCP6)RIGHT=TAGPLO(IARG9I)
        IJ=MAXN*(ICOLL-1)+IARG4I
        IF(ICOLL.LE.MAXCOL)V(IJ)=RIGHT
        IF(ICOLL.EQ.MAXCP1)PRED(IARG4I)=RIGHT
        IF(ICOLL.EQ.MAXCP2)RES(IARG4I)=RIGHT
        IF(ICOLL.EQ.MAXCP3)YPLOT(IARG4I)=RIGHT
        IF(ICOLL.EQ.MAXCP4)XPLOT(IARG4I)=RIGHT
        IF(ICOLL.EQ.MAXCP5)X2PLOT(IARG4I)=RIGHT
        IF(ICOLL.EQ.MAXCP6)TAGPLO(IARG4I)=RIGHT
        GOTO6500
      ELSEIF(ICASER.EQ.'PARA')THEN
        IF(NUMPAR.LE.0)GOTO6490
        DO6400J=1,NUMPAR
          IPJ=IPARN(J)
          IPJ2=IPARN2(J)
          DO6450I=1,NUMNAM
            I2=I
            IF(IPJ.EQ.IHNAME(I).AND.IPJ2.EQ.IHNAM2(I).AND.
     1         IUSE(I).EQ.'P')THEN
              PARAM(J)=VALUE(I2)
              GOTO6400
            ENDIF
 6450     CONTINUE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2801)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,6482)
 6482     FORMAT('      AT BRANCH POINT 6481--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,6483)
 6483     FORMAT('      PARAMETER NAME FOR FUNCTION EVALUATION WAS ',
     1           'NOT FOUND IN THE INTERNAL LIST.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,6485)IPJ,IPJ2
 6485     FORMAT('      PARAMETER NAME = ',A4,A4)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3035)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3036)(IANS(I),I=1,MIN(100,IWIDTH))
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO19000
 6400   CONTINUE
        GOTO6490
C
      ENDIF
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2801)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6102)
 6102 FORMAT('      AT BRANCH POINT 6101--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6103)ICASER
 6103 FORMAT('      ICASER = ',A4,' DETECTED IN STEP 6 (ELEMENT ',
     1       'CALCULATION).')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3035)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3036)(IANS(I),I=1,MIN(100,IWIDTH))
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO19000
C
 6490 CONTINUE
      IPASS=2
      CALL COMPIM(IA,NUMCHA,IPASS,PARAM,IPARN,IPARN2,NUMPAR,
     1            IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,RIGHT,
     1            IBUGCO,IBUGEV,IERROR)
      IF(IERROR.EQ.'YES')GOTO19000
      IJ=MAXN*(ICOLL-1)+IARG4I
      IF(ICOLL.LE.MAXCOL)V(IJ)=RIGHT
      IF(ICOLL.EQ.MAXCP1)PRED(IARG4I)=RIGHT
      IF(ICOLL.EQ.MAXCP2)RES(IARG4I)=RIGHT
      IF(ICOLL.EQ.MAXCP3)YPLOT(IARG4I)=RIGHT
      IF(ICOLL.EQ.MAXCP4)XPLOT(IARG4I)=RIGHT
      IF(ICOLL.EQ.MAXCP5)X2PLOT(IARG4I)=RIGHT
      IF(ICOLL.EQ.MAXCP6)TAGPLO(IARG4I)=RIGHT
C
 6500 CONTINUE
      IFOUND='YES'
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
      DO6600J4=1,NUMNAM
        IF(IUSE(J4).EQ.'V'.AND.IVALUE(J4).EQ.ICOLL)THEN
          IUSE(J4)='V'
          IVALUE(J4)=ICOLL
          VALUE(J4)=ICOLL
          IN(J4)=NINEW
        ENDIF
 6600 CONTINUE
C
      IF(IPRINT.EQ.'ON' .AND. IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6111)ILEFT,ILEFT2,IARG4I,RIGHT
 6111   FORMAT('THE COMPUTED VALUE OF ',2A4,'(',I6,') = ',E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6112)ILEFT,ILEFT2,ICOLL
 6112   FORMAT('THE CURRENT COLUMN FOR THE VARIABLE ',2A4,' = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6113)ILEFT,ILEFT2,NINEW
 6113   FORMAT('THE CURRENT LENGTH OF  THE VARIABLE ',2A4,' = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      GOTO19000
C
C               *****************************************************
C               **  STEP 7--                                       **
C               **  TREAT THE FULL VARIABLE CASE.                  **
C               **  EXAMPLES--                                     **
C               **            LET Y    = X*SIN(X)                  **
C               **            LET Y(I) = I                         **
C               **            LET Y(I) = X(2)                      **
C               **            LET Y(I) = 3*SIN(4)                  **
C               **            LET Y(I) = B*SIN(B)                  **
C               **            LET Y(I) = X*SIN(X)                  **
C               **            LET U    = X*SIN(X)                  **
C               **            LET U(I) = X*SIN(X)                  **
C               **  WHERE Y WAS A PREVIOUSLY-DEFINED VARIABLE      **
C               **  AND WHERE U WAS PREVIOUSLY UNDEFINED.          **
C               **  THEN JUMP TO STEP NUMBER 10 BELOW              **
C               **  FOR THE THE LIST UPDATING AND                  **
C               **  GENERATE THE INFORMATIVE PRINTING.             **
C               **  THEN EXIT.                                     **
C               *****************************************************
C
 7000 CONTINUE
      ISTEPN='7'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FUEV')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASEL.EQ.'VAR')ICOLL=IVALUE(ILISTL)
      IF(ICASEL.EQ.'UNKN')ICOLL=NUMCOL+1
      IF(ICOLL.LE.MAXCOL)GOTO7090
      IF(ICASEL.EQ.'VAR')GOTO7090
C
      WRITE(ICOUT,2801)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7052)ICOLL
 7052 FORMAT('      THE NUMBER OF DATA COLUMNS (',I8,')')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7053)MAXCOL
 7053 FORMAT('      HAS JUST EXCEEDED THE MAX ALLOWABLE (',I8,').')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7054)
 7054 FORMAT('      SUGGESTED ACTION--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7055)
 7055 FORMAT('      ENTER      STAT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7056)
 7056 FORMAT('      TO FIND OUT THE FULL LIST OF USED COLUMNS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7057)
 7057 FORMAT('      AND THEN OVERWRITE SOME COLUMN.   ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3035)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3036)(IANS(I),I=1,MIN(100,IWIDTH))
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO19000
C
 7090 CONTINUE
      NINEW=NILEFT
      IF(ICASER.EQ.'VAR')NINEW=NIRIGH
C
      IF(ICASER.EQ.'DUMM')THEN
        NS2=0
        DO7150I=1,NINEW
          NS2=NS2+1
          RIGHT=I
          IJ=MAXN*(ICOLL-1)+I
          IF(ICOLL.LE.MAXCOL)V(IJ)=RIGHT
          IF(ICOLL.EQ.MAXCP1)PRED(I)=RIGHT
          IF(ICOLL.EQ.MAXCP2)RES(I)=RIGHT
          IF(ICOLL.EQ.MAXCP3)YPLOT(I)=RIGHT
          IF(ICOLL.EQ.MAXCP4)XPLOT(I)=RIGHT
          IF(ICOLL.EQ.MAXCP5)X2PLOT(I)=RIGHT
          IF(ICOLL.EQ.MAXCP6)TAGPLO(I)=RIGHT
          IF(NS2.EQ.1)IROW1=I
          IROWN=I
 7150   CONTINUE
        GOTO10000
      ELSEIF(ICASER.EQ.'ELEM')THEN
        IARG9I=INT1(9)
        IJ=MAXN*(ICOLR-1)+IARG9I
        IF(ICOLR.LE.MAXCOL)RIGHT=V(IJ)
        IF(ICOLR.EQ.MAXCP1)RIGHT=PRED(IARG9I)
        IF(ICOLR.EQ.MAXCP2)RIGHT=RES(IARG9I)
        IF(ICOLR.EQ.MAXCP3)RIGHT=YPLOT(IARG9I)
        IF(ICOLR.EQ.MAXCP4)RIGHT=XPLOT(IARG9I)
        IF(ICOLR.EQ.MAXCP5)RIGHT=X2PLOT(IARG9I)
        IF(ICOLR.EQ.MAXCP6)RIGHT=TAGPLO(IARG9I)
        NS2=0
        DO7250I=1,NINEW
          NS2=NS2+1
          IJ=MAXN*(ICOLL-1)+I
          IF(ICOLL.LE.MAXCOL)V(IJ)=RIGHT
          IF(ICOLL.EQ.MAXCP1)PRED(I)=RIGHT
          IF(ICOLL.EQ.MAXCP2)RES(I)=RIGHT
          IF(ICOLL.EQ.MAXCP3)YPLOT(I)=RIGHT
          IF(ICOLL.EQ.MAXCP4)XPLOT(I)=RIGHT
          IF(ICOLL.EQ.MAXCP5)X2PLOT(I)=RIGHT
          IF(ICOLL.EQ.MAXCP6)TAGPLO(I)=RIGHT
          IF(NS2.EQ.1)IROW1=I
          IROWN=I
 7250   CONTINUE
        GOTO10000
      ELSEIF(ICASER.EQ.'PARA' .OR. ICASER.EQ.'VAR')THEN
        IPASS=2
        NS2=0
        DO7350I=1,NINEW
          NS2=NS2+1
C
          IF(NUMPAR.LE.0)GOTO7390
          DO7355J=1,NUMPAR
            IPJ=IPARN(J)
            IPJ2=IPARN2(J)
            DO7356K=1,NUMNAM
              K2=K
              IF(IPJ.EQ.IHNAME(K).AND.IPJ2.EQ.IHNAM2(K).AND.
     1           IUSE(K).EQ.'P')THEN
                PARAM(J)=VALUE(K2)
                GOTO7355
              ELSEIF(IPJ.EQ.IHNAME(K).AND.IPJ2.EQ.IHNAM2(K).AND.
     1           IUSE(K).EQ.'V')THEN
                ICOLK2=IVALUE(K2)
                IJ=MAXN*(ICOLK2-1)+I
                IF(ICOLK2.LE.MAXCOL)PARAM(J)=V(IJ)
                IF(ICOLK2.EQ.MAXCP1)PARAM(J)=PRED(I)
                IF(ICOLK2.EQ.MAXCP2)PARAM(J)=RES(I)
                IF(ICOLK2.EQ.MAXCP3)PARAM(J)=YPLOT(I)
                IF(ICOLK2.EQ.MAXCP4)PARAM(J)=XPLOT(I)
                IF(ICOLK2.EQ.MAXCP5)PARAM(J)=X2PLOT(I)
                IF(ICOLK2.EQ.MAXCP6)PARAM(J)=TAGPLO(I)
                GOTO7355
              ENDIF
 7356       CONTINUE
C
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2801)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,7382)
 7382       FORMAT('      AT BRANCH POINT 7381--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,7383)
 7383       FORMAT('      PARAMETER/VARIABLE NAME FOR FUNCTION ',
     1             'EVALUATION')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,7384)
 7384       FORMAT('      NOT FOUND IN INTERNAL LIST.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,7385)IPJ,IPJ2
 7385       FORMAT('      PARAMETER/VARIABLE NAME = ',2A4)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3035)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3036)(IANS(II),II=1,MIN(100,IWIDTH))
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO19000
 7355     CONTINUE
          GOTO7390
C
 7390     CONTINUE
          CALL COMPIM(IA,NUMCHA,IPASS,PARAM,IPARN,IPARN2,NUMPAR,
     1                IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,RIGHT,
     1                IBUGCO,IBUGEV,IERROR)
          IF(IERROR.EQ.'YES')GOTO19000
          IFOUND='YES'
          IJ=MAXN*(ICOLL-1)+I
          IF(ICOLL.LE.MAXCOL)V(IJ)=RIGHT
          IF(ICOLL.EQ.MAXCP1)PRED(I)=RIGHT
          IF(ICOLL.EQ.MAXCP2)RES(I)=RIGHT
          IF(ICOLL.EQ.MAXCP3)YPLOT(I)=RIGHT
          IF(ICOLL.EQ.MAXCP4)XPLOT(I)=RIGHT
          IF(ICOLL.EQ.MAXCP5)X2PLOT(I)=RIGHT
          IF(ICOLL.EQ.MAXCP6)TAGPLO(I)=RIGHT
          IF(NS2.EQ.1)IROW1=I
          IROWN=I
 7350   CONTINUE
        GOTO10000
      ENDIF
C
C               *****************************************************
C               **  STEP 8--                                       **
C               **  TREAT THE PARTIAL VARIABLE SUBSET CASE.        **
C               **  EXAMPLES--                                     **
C               **            LET Y    = I         SUBSET 2 3 5    **
C               **            LET Y    = X(2)      SUBSET 2 3 5    **
C               **            LET Y    = 3*SIN(4)  SUBSET 2 3 5    **
C               **            LET Y    = B*SIN(B)  SUBSET 2 3 5    **
C               **            LET Y    = X*SIN(X)  SUBSET 2 3 5    **
C               **            LET Y(I) = I         SUBSET 2 3 5    **
C               **            LET Y(I) = X(2)      SUBSET 2 3 5    **
C               **            LET Y(I) = 3*SIN(4)  SUBSET 2 3 5    **
C               **            LET Y(I) = B*SIN(B)  SUBSET 2 3 5    **
C               **            LET Y(I) = X*SIN(X)  SUBSET 2 3 5    **
C               **            LET U    = I         SUBSET 2 3 5    **
C               **            LET U    = X(2)      SUBSET 2 3 5    **
C               **            LET U    = 3*SIN(4)  SUBSET 2 3 5    **
C               **            LET U    = B*SIN(B)  SUBSET 2 3 5    **
C               **            LET U    = X*SIN(X)  SUBSET 2 3 5    **
C               **            LET U(I) = I         SUBSET 2 3 5    **
C               **            LET U(I) = X(2)      SUBSET 2 3 5    **
C               **            LET U(I) = 3*SIN(4)  SUBSET 2 3 5    **
C               **            LET U(I) = B*SIN(B)  SUBSET 2 3 5    **
C               **            LET U(I) = X*SIN(X)  SUBSET 2 3 5    **
C               **  WHERE Y WAS A PREVIOUSLY-DEFINED VARIABLE      **
C               **  AND WHERE U WAS PREVIOUSLY UNDEFINED.          **
C               **  THEN JUMP TO STEP NUMBER 10 BELOW              **
C               **  FOR THE THE LIST UPDATING  AND                 **
C               **  GENERATE THE INFORMATIVE PRINTING.             **
C               **  THEN EXIT.                                     **
C               *****************************************************
C
 8000 CONTINUE
      ISTEPN='8'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FUEV')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASEL.EQ.'VAR')ICOLL=IVALUE(ILISTL)
      IF(ICASEL.EQ.'UNKN')ICOLL=NUMCOL+1
      IF(ICOLL.LE.MAXCOL)GOTO8090
      IF(ICASEL.EQ.'VAR')GOTO8090
C
      WRITE(ICOUT,2801)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8052)
 8052 FORMAT('      THE NUMBER OF DATA COLUMNS HAS JUST EXCEEDED THE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8054)MAXCOL
 8054 FORMAT('      MAXIMUM ALLOWABLE ',I8,'.  SUGGESTED ACTION--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8055)
 8055 FORMAT('      ENTER      STAT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8056)
 8056 FORMAT('      TO FIND OUT THE FULL LIST OF USED COLUMNS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8057)
 8057 FORMAT('      AND THEN OVERWRITE SOME COLUMN.   ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3035)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3036)(IANS(II),II=1,MIN(100,IWIDTH))
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO19000
C
 8090 CONTINUE
      IHSET=IHOL(12)
      IHSET2=IHOL2(12)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IHSET,IHSET2,IHWUSE,
     1            IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1            ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
      IF(IERROR.EQ.'YES')GOTO19000
C
CCCCC JULY 2007: FIX EMPTY SUBSET BUG.  IF AN EMPTY SUBSET
CCCCC            IS GIVEN, SET IERROR AND RETURN.  CHECK AT
CCCCC            BOTH INPUT (I.E., N BEFORE SUBSET) AND
CCCCC            OUTPUT (I.E., N AFTER SUBSET).
C
      NISET=IN(ILOC)
C
      IF(NISET.LT.1)THEN
        IERROR='WARN'
        GOTO19000
      ENDIF
C
      CALL DPSUBS(NISET,ILOCS,NS,IBUGQ,IERROR)
C
      NINEW=NISET
      IF(ICASER.EQ.'VAR')NINEW=NIRIGH
C
      IF(ICASER.EQ.'DUMM')THEN
        NS2=0
        DO8150I=1,NISET
          IF(ISUB(I).EQ.0)GOTO8150
          NS2=NS2+1
          RIGHT=I
          IJ=MAXN*(ICOLL-1)+I
          IF(ICOLL.LE.MAXCOL)V(IJ)=RIGHT
          IF(ICOLL.EQ.MAXCP1)PRED(I)=RIGHT
          IF(ICOLL.EQ.MAXCP2)RES(I)=RIGHT
          IF(ICOLL.EQ.MAXCP3)YPLOT(I)=RIGHT
          IF(ICOLL.EQ.MAXCP4)XPLOT(I)=RIGHT
          IF(ICOLL.EQ.MAXCP5)X2PLOT(I)=RIGHT
          IF(ICOLL.EQ.MAXCP6)TAGPLO(I)=RIGHT
          IF(NS2.EQ.1)IROW1=I
          IROWN=I
 8150   CONTINUE
C
        IF(NS2.LT.1)THEN
          IERROR='WARN'
          GOTO19000
        ENDIF
C
        GOTO10000
      ELSEIF(ICASER.EQ.'ELEM')THEN
        IARG9I=INT1(9)
        IJ=MAXN*(ICOLR-1)+IARG9I
C
        IF(ICOLR.LE.MAXCOL)RIGHT=V(IJ)
        IF(ICOLR.EQ.MAXCP1)RIGHT=PRED(IARG9I)
        IF(ICOLR.EQ.MAXCP2)RIGHT=RES(IARG9I)
        IF(ICOLR.EQ.MAXCP3)RIGHT=YPLOT(IARG9I)
        IF(ICOLR.EQ.MAXCP4)RIGHT=XPLOT(IARG9I)
        IF(ICOLR.EQ.MAXCP5)RIGHT=X2PLOT(IARG9I)
        IF(ICOLR.EQ.MAXCP6)RIGHT=TAGPLO(IARG9I)
        NS2=0
        DO8250I=1,NISET
          IF(ISUB(I).EQ.0)GOTO8250
          NS2=NS2+1
          IJ=MAXN*(ICOLL-1)+I
          IF(ICOLL.LE.MAXCOL)V(IJ)=RIGHT
          IF(ICOLL.EQ.MAXCP1)PRED(I)=RIGHT
          IF(ICOLL.EQ.MAXCP2)RES(I)=RIGHT
          IF(ICOLL.EQ.MAXCP3)YPLOT(I)=RIGHT
          IF(ICOLL.EQ.MAXCP4)XPLOT(I)=RIGHT
          IF(ICOLL.EQ.MAXCP5)X2PLOT(I)=RIGHT
          IF(ICOLL.EQ.MAXCP6)TAGPLO(I)=RIGHT
          IF(NS2.EQ.1)IROW1=I
          IROWN=I
 8250   CONTINUE
C
        IF(NS2.LT.1)THEN
          IERROR='WARN'
          GOTO19000
        ENDIF
C
        GOTO10000
      ELSEIF(ICASER.EQ.'PARA' .OR. ICASER.EQ.'VAR')THEN
        IPASS=2
        IMAX=NISET
        IF(NINEW.LT.IMAX)IMAX=NINEW
        NS2=0
        DO8350I=1,IMAX
          IF(ISUB(I).EQ.0)GOTO8350
          NS2=NS2+1
C
          IF(NUMPAR.LE.0)GOTO8390
          DO8355J=1,NUMPAR
            IPJ=IPARN(J)
            IPJ2=IPARN2(J)
            DO8356K=1,NUMNAM
              K2=K
              IF(IPJ.EQ.IHNAME(K).AND.IPJ2.EQ.IHNAM2(K).AND.
     1           IUSE(K).EQ.'P')THEN
                PARAM(J)=VALUE(K2)
                GOTO8355
              ELSEIF(IPJ.EQ.IHNAME(K).AND.IPJ2.EQ.IHNAM2(K).AND.
     1               IUSE(K).EQ.'V')THEN
                ICOLK2=IVALUE(K2)
                IJ=MAXN*(ICOLK2-1)+I
                IF(ICOLK2.LE.MAXCOL)PARAM(J)=V(IJ)
                IF(ICOLK2.EQ.MAXCP1)PARAM(J)=PRED(I)
                IF(ICOLK2.EQ.MAXCP2)PARAM(J)=RES(I)
                IF(ICOLK2.EQ.MAXCP3)PARAM(J)=YPLOT(I)
                IF(ICOLK2.EQ.MAXCP4)PARAM(J)=XPLOT(I)
                IF(ICOLK2.EQ.MAXCP5)PARAM(J)=X2PLOT(I)
                IF(ICOLK2.EQ.MAXCP6)PARAM(J)=TAGPLO(I)
                GOTO8355
              ENDIF
 8356       CONTINUE
C
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2801)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,8382)
 8382       FORMAT('      AT BRANCH POINT 8381--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,8383)
 8383       FORMAT('      PARAMETER/VARIABLE NAME FOR FUNCTION ',
     1             'EVALUATION')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,8384)
 8384       FORMAT('      NOT FOUND IN INTERNAL LIST.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,8385)IPJ,IPJ2
 8385       FORMAT('      PARAMETER/VARIABLE NAME = ',A4,A4)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3035)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3036)(IANS(II),II=1,MIN(100,IWIDTH))
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO19000
C
 8355     CONTINUE
C
 8390     CONTINUE
          CALL COMPIM(IA,NUMCHA,IPASS,PARAM,IPARN,IPARN2,NUMPAR,
     1                IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,RIGHT,
     1                IBUGCO,IBUGEV,IERROR)
          IF(IERROR.EQ.'YES')GOTO19000
          IFOUND='YES'
          IJ=MAXN*(ICOLL-1)+I
          IF(ICOLL.LE.MAXCOL)V(IJ)=RIGHT
          IF(ICOLL.EQ.MAXCP1)PRED(I)=RIGHT
          IF(ICOLL.EQ.MAXCP2)RES(I)=RIGHT
          IF(ICOLL.EQ.MAXCP3)YPLOT(I)=RIGHT
          IF(ICOLL.EQ.MAXCP4)XPLOT(I)=RIGHT
          IF(ICOLL.EQ.MAXCP5)X2PLOT(I)=RIGHT
          IF(ICOLL.EQ.MAXCP6)TAGPLO(I)=RIGHT
          IF(NS2.EQ.1)IROW1=I
          IROWN=I
 8350   CONTINUE
C
        IF(NS2.LT.1)THEN
          IERROR='WARN'
          GOTO19000
        ENDIF
C
        GOTO10000
      ENDIF
C
C               *****************************************************
C               **  STEP 9--                                       **
C               **  TREAT THE PARTIAL VARIABLE FOR CASE.           **
C               **  EXAMPLES--                                     **
C               **            LET Y    = I         FOR I = 1 2 10  **
C               **            LET Y    = X(2)      FOR I = 1 2 10  **
C               **            LET Y    = 3*SIN(4)  FOR I = 1 2 10  **
C               **            LET Y    = B*SIN(B)  FOR I = 1 2 10  **
C               **            LET Y    = X*SIN(X)  FOR I = 1 2 10  **
C               **            LET Y(I) = I         FOR I = 1 2 10  **
C               **            LET Y(I) = X(2)      FOR I = 1 2 10  **
C               **            LET Y(I) = 3*SIN(4)  FOR I = 1 2 10  **
C               **            LET Y(I) = B*SIN(B)  FOR I = 1 2 10  **
C               **            LET Y(I) = X*SIN(X)  FOR I = 1 2 10  **
C               **            LET U    = I         FOR I = 1 2 10  **
C               **            LET U    = X(2)      FOR I = 1 2 10  **
C               **            LET U    = 3*SIN(4)  FOR I = 1 2 10  **
C               **            LET U    = B*SIN(B)  FOR I = 1 2 10  **
C               **            LET U    = X*SIN(X)  FOR I = 1 2 10  **
C               **            LET U(I) = I         FOR I = 1 2 10  **
C               **            LET U(I) = X(2)      FOR I = 1 2 10  **
C               **            LET U(I) = 3*SIN(4)  FOR I = 1 2 10  **
C               **            LET U(I) = B*SIN(B)  FOR I = 1 2 10  **
C               **            LET U(I) = X*SIN(X)  FOR I = 1 2 10  **
C               **  WHERE Y WAS A PREVIOUSLY-DEFINED VARIABLE      **
C               **  AND WHERE U WAS PREVIOUSLY UNDEFINED.          **
C               **  THEN JUMP TO STEP NUMBER 10 BELOW              **
C               **  FOR THE THE LIST UPDATING  AND                 **
C               **  GENERATE THE INFORMATIVE PRINTING.             **
C               **  THEN EXIT.                                     **
C               *****************************************************
C
 9000 CONTINUE
      ISTEPN='9'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FUEV')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASEL.EQ.'VAR')ICOLL=IVALUE(ILISTL)
      IF(ICASEL.EQ.'UNKN')ICOLL=NUMCOL+1
      IF(ICOLL.LE.MAXCOL)GOTO9090
      IF(ICASEL.EQ.'VAR')GOTO9090
C
      WRITE(ICOUT,2801)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9052)
 9052 FORMAT('      THE NUMBER OF DATA COLUMNS HAS JUST EXCEEDED THE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9054)MAXCOL
 9054 FORMAT('      MAXIMUM ALLOWABLE ',I8,'.  SUGGESTED ACTION--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9055)
 9055 FORMAT('      ENTER      STAT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9056)
 9056 FORMAT('      TO FIND OUT THE FULL LIST OF USED COLUMNS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9057)
 9057 FORMAT('      AND THEN OVERWRITE SOME COLUMN.   ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3035)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3036)(IANS(I),I=1,MIN(100,IWIDTH))
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO19000
C
 9090 CONTINUE
      NIOLD=IN(ILISTL)
      CALL DPFOR(NIOLD,NIFOR,IROW1,IROWN,
     1           NLOCAL,ILOCS,NS,IBUGQ,IERROR)
C
      NINEW=NIFOR
      IF(ICASER.EQ.'VAR')NINEW=NIRIGH
C
      IF(ICASER.EQ.'DUMM')THEN
        NS2=0
        DO9150I=1,NIFOR
          IF(ISUB(I).EQ.0)GOTO9150
          NS2=NS2+1
          RIGHT=I
          IJ=MAXN*(ICOLL-1)+I
          IF(ICOLL.LE.MAXCOL)V(IJ)=RIGHT
          IF(ICOLL.EQ.MAXCP1)PRED(I)=RIGHT
          IF(ICOLL.EQ.MAXCP2)RES(I)=RIGHT
          IF(ICOLL.EQ.MAXCP3)YPLOT(I)=RIGHT
          IF(ICOLL.EQ.MAXCP4)XPLOT(I)=RIGHT
          IF(ICOLL.EQ.MAXCP5)X2PLOT(I)=RIGHT
          IF(ICOLL.EQ.MAXCP6)TAGPLO(I)=RIGHT
          IF(NS2.EQ.1)IROW1=I
          IROWN=I
 9150   CONTINUE
        GOTO10000
      ELSEIF(ICASER.EQ.'ELEM')THEN
        IARG9I=INT1(9)
        IJ=MAXN*(ICOLR-1)+IARG9I
        IF(ICOLR.LE.MAXCOL)RIGHT=V(IJ)
        IF(ICOLR.EQ.MAXCP1)RIGHT=PRED(IARG9I)
        IF(ICOLR.EQ.MAXCP2)RIGHT=RES(IARG9I)
        IF(ICOLR.EQ.MAXCP3)RIGHT=YPLOT(IARG9I)
        IF(ICOLR.EQ.MAXCP4)RIGHT=XPLOT(IARG9I)
        IF(ICOLR.EQ.MAXCP5)RIGHT=X2PLOT(IARG9I)
        IF(ICOLR.EQ.MAXCP6)RIGHT=TAGPLO(IARG9I)
        NS2=0
        DO9250I=1,NIFOR
          IF(ISUB(I).EQ.0)GOTO9250
          NS2=NS2+1
          IJ=MAXN*(ICOLL-1)+I
          IF(ICOLL.LE.MAXCOL)V(IJ)=RIGHT
          IF(ICOLL.EQ.MAXCP1)PRED(I)=RIGHT
          IF(ICOLL.EQ.MAXCP2)RES(I)=RIGHT
          IF(ICOLL.EQ.MAXCP3)YPLOT(I)=RIGHT
          IF(ICOLL.EQ.MAXCP4)XPLOT(I)=RIGHT
          IF(ICOLL.EQ.MAXCP5)X2PLOT(I)=RIGHT
          IF(ICOLL.EQ.MAXCP6)TAGPLO(I)=RIGHT
          IF(NS2.EQ.1)IROW1=I
          IROWN=I
 9250   CONTINUE
        GOTO10000
      ELSEIF(ICASER.EQ.'PARA' .OR. ICASER.EQ.'VAR')THEN
        IPASS=2
        IMAX=NIFOR
        IF(NINEW.LT.IMAX)IMAX=NINEW
        NS2=0
        DO9350I=1,IMAX
          IF(ISUB(I).EQ.0)GOTO9350
          NS2=NS2+1
C
          IF(NUMPAR.LE.0)GOTO9390
          DO9355J=1,NUMPAR
            IPJ=IPARN(J)
            IPJ2=IPARN2(J)
            DO9356K=1,NUMNAM
              K2=K
              IF(IPJ.EQ.IHNAME(K).AND.IPJ2.EQ.IHNAM2(K).AND.
     1           IUSE(K).EQ.'P')THEN
                PARAM(J)=VALUE(K2)
                GOTO9355
              ELSEIF(IPJ.EQ.IHNAME(K).AND.IPJ2.EQ.IHNAM2(K).AND.
     1           IUSE(K).EQ.'V')THEN
                ICOLK2=IVALUE(K2)
                IJ=MAXN*(ICOLK2-1)+I
                IF(ICOLK2.LE.MAXCOL)PARAM(J)=V(IJ)
                IF(ICOLK2.EQ.MAXCP1)PARAM(J)=PRED(I)
                IF(ICOLK2.EQ.MAXCP2)PARAM(J)=RES(I)
                IF(ICOLK2.EQ.MAXCP3)PARAM(J)=YPLOT(I)
                IF(ICOLK2.EQ.MAXCP4)PARAM(J)=XPLOT(I)
                IF(ICOLK2.EQ.MAXCP5)PARAM(J)=X2PLOT(I)
                IF(ICOLK2.EQ.MAXCP6)PARAM(J)=TAGPLO(I)
                GOTO9355
              ENDIF
 9356       CONTINUE
C
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2801)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9382)
 9382       FORMAT('      AT BRANCH POINT 9381--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9393)
 9393       FORMAT('      PARAMETER/VARIABLE NAME FOR FUNCTION ',
     1             'EVALUATION')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9384)
 9384       FORMAT('      NOT FOUND IN INTERNAL LIST.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9385)IPJ,IPJ2
 9385       FORMAT('      PARAMETER/VARIABLE NAME = ',A4,A4)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3035)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3036)(IANS(II),II=1,MIN(100,IWIDTH))
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO19000
C
 9355     CONTINUE
C
 9390     CONTINUE
          CALL COMPIM(IA,NUMCHA,IPASS,PARAM,IPARN,IPARN2,NUMPAR,
     1                IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,RIGHT,
     1                IBUGCO,IBUGEV,IERROR)
          IF(IERROR.EQ.'YES')GOTO19000
          IFOUND='YES'
          IJ=MAXN*(ICOLL-1)+I
          IF(ICOLL.LE.MAXCOL)V(IJ)=RIGHT
          IF(ICOLL.EQ.MAXCP1)PRED(I)=RIGHT
          IF(ICOLL.EQ.MAXCP2)RES(I)=RIGHT
          IF(ICOLL.EQ.MAXCP3)YPLOT(I)=RIGHT
          IF(ICOLL.EQ.MAXCP4)XPLOT(I)=RIGHT
          IF(ICOLL.EQ.MAXCP5)X2PLOT(I)=RIGHT
          IF(ICOLL.EQ.MAXCP6)TAGPLO(I)=RIGHT
          IF(NS2.EQ.1)IROW1=I
          IROWN=I
 9350   CONTINUE
        GOTO10000
C
      ENDIF
C
C               *******************************************
C               **  STEP 10--                            **
C               **  CARRY OUT THE LIST UPDATING AND      **
C               **  GENERATE THE INFORMATIVE PRINTING    **
C               **  FOR STEP NUMBERS 7, 8, AND 9 ABOVE.  **
C               *******************************************
C
10000 CONTINUE
      IFOUND='YES'
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
      DO10100J4=1,NUMNAM
        IF(IUSE(J4).EQ.'V'.AND.IVALUE(J4).EQ.ICOLL)THEN
          IUSE(J4)='V'
          IVALUE(J4)=ICOLL
          VALUE(J4)=ICOLL
          IN(J4)=NINEW
        ENDIF
10100 CONTINUE
C
      IF(IPRINT.EQ.'ON' .AND.  IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10011)ILEFT,ILEFT2,NS2
10011   FORMAT('THE NUMBER OF VALUES GENERATED FOR ',
     1         'THE VARIABLE ',2A4,' = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
C
        IJ=MAXN*(ICOLL-1)+IROW1
        IJN=MAXN*(ICOLL-1)+IROWN
        IF(ICOLL.LE.MAXCOL)THEN
          WRITE(ICOUT,10021)ILEFT,ILEFT2,V(IJ),IROW1
10021     FORMAT('THE FIRST          COMPUTED VALUE OF ',2A4,
     1           ' = ',E13.6,' (ROW ',I5,')')
          CALL DPWRST('XXX','BUG ')
          IF(NS2.NE.1)THEN
            WRITE(ICOUT,10031)NS2,ILEFT,ILEFT2,V(IJN),IROWN
10031       FORMAT('THE LAST (',I5,'TH) COMPUTED VALUE OF ',2A4,
     1             ' = ',E13.6,' (ROW ',I5,')')
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSEIF(ICOLL.EQ.MAXCP1)THEN
          WRITE(ICOUT,10021)ILEFT,ILEFT2,PRED(IROW1),IROW1
          CALL DPWRST('XXX','BUG ')
          IF(NS2.NE.1)THEN
            WRITE(ICOUT,10031)ILEFT,ILEFT2,PRED(IROWN),IROW1
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSEIF(ICOLL.EQ.MAXCP2)THEN
          WRITE(ICOUT,10031)ILEFT,ILEFT2,RES(IROW1),IROW1
          CALL DPWRST('XXX','BUG ')
          IF(NS2.NE.1)THEN
            WRITE(ICOUT,10031)ILEFT,ILEFT2,RES(IROWN),IROW1
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSEIF(ICOLL.EQ.MAXCP3)THEN
          WRITE(ICOUT,10021)ILEFT,ILEFT2,YPLOT(IROW1),IROW1
          CALL DPWRST('XXX','BUG ')
          IF(NS2.NE.1)THEN
            WRITE(ICOUT,10031)ILEFT,ILEFT2,YPLOT(IROWN),IROW1
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSEIF(ICOLL.EQ.MAXCP4)THEN
          WRITE(ICOUT,10021)ILEFT,ILEFT2,XPLOT(IROW1),IROW1
          CALL DPWRST('XXX','BUG ')
          IF(NS2.NE.1)THEN
            WRITE(ICOUT,10031)ILEFT,ILEFT2,XPLOT(IROWN),IROW1
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSEIF(ICOLL.EQ.MAXCP5)THEN
          WRITE(ICOUT,10021)ILEFT,ILEFT2,X2PLOT(IROW1),IROW1
          CALL DPWRST('XXX','BUG ')
          IF(NS2.NE.1)THEN
            WRITE(ICOUT,10031)ILEFT,ILEFT2,X2PLOT(IROWN),IROW1
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSEIF(ICOLL.EQ.MAXCP6)THEN
          WRITE(ICOUT,10021)ILEFT,ILEFT2,TAGPLO(IROW1),IROW1
          CALL DPWRST('XXX','BUG ')
          IF(NS2.NE.1)THEN
            WRITE(ICOUT,10031)ILEFT,ILEFT2,TAGPLO(IROWN),IROW1
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ENDIF
C
        IF(NS2.LE.1)THEN
          WRITE(ICOUT,10041)
10041     FORMAT('SINCE THE GENERATED SAMPLE SIZE WAS ONLY 1,')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,10042)
10042     FORMAT('THE ABOVE VALUE WAS THE SOLE VALUE COMPUTED.')
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10092)ILEFT,ILEFT2,ICOLL
10092   FORMAT('THE CURRENT COLUMN FOR THE VARIABLE ',2A4,' = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,10093)ILEFT,ILEFT2,NINEW
10093   FORMAT('THE CURRENT LENGTH OF  THE VARIABLE ',2A4,' = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        GOTO19000
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
19000 CONTINUE
C
C  RESTORE ORIGINAL FUNCTION TABLE
C
      DO19001I=1,NUMCHF
        IFUNC(I)=IFSAVE(I)
19001 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FUEV')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,19011)
19011   FORMAT('***** AT THE END       OF DPFUEV--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,19016)IFOUND,IERROR,ICASEQ,ICASIF,NUMNAM
19016   FORMAT('IFOUND,IERROR,ICASEQ,ICASIF,NUMNAM = ',4(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO19020I=1,NUMNAM
          WRITE(ICOUT,19021)I,IHNAME(I),IHNAM2(I),IUSE(I),
     1                      IVALUE(I),VALUE(I)
19021     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVALUE(I),VALUE(I) = ',
     1           I8,3(2X,A4),I8,G15.7)
          CALL DPWRST('XXX','BUG ')
19020   CONTINUE
      ENDIF
C
      RETURN
      END

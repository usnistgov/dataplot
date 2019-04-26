C     ..........DPDDS3..........
 
      SUBROUTINE DPDDS3(Y1,N1,IORDAR,IORDMA,DELTAT,NUMVAR,ILOCV,
CCCCC1XTEMP1,XTEMP2,MAXNXT,PRED2,RES2,RESSD,RESDF,IBUGA3,IERROR)
     1XTEMP1,XTEMP2,XDDS,YDDS,AT,MAXNXT,PRED2,RES2,RESSD,RESDF,
     1IBUGA3,IERROR)
CCCCC APRIL 1996.  ADD XDDS, YDDS, AT TO ARGUMENT LIST
CCCCC PROGRAM DPDDS3
CCCCC  UPDATED--AUGUST        1995.  CRAY DOESN'T LIKE "*" FORMAT
CCCCC                                FOR INTERNAL WRITE 
******************************************************************
*                                                                *
*    THIS PROGRAM FITS UNIVARIATE (ARMA) AND EXTENDED (EARMA)    *
*  MODELS TO TIME SERIES DATA, WITH OPTIONAL DETERMINISTIC TREND *
*  ESTIMATION FOR UNIVARIATE MODELS. IT IS DESIGNED TO BE USED   *
*  WITH THE BOOK:                                                *
*        "TIME SERIES AND SYSTEM ANALYSIS WITH APPLICATIONS"     *
*                  BY S. M. PANDIT AND S. M. WU                  *
*                                                                *
*              THIS CODE WRITTEN BY WILLIAM WITTIG               *
*                              AT                                *
*               MICHIGAN TECHNOLOGICAL UNIVERSITY                *
*                             1982                               *
*                                                                *
*         SUBROUTINE LS ADAPTED FROM CODE WRITTEN AT             *
*              UNIVERSITY OF WISCONSIN-MADISON                   *
*                                                                *
*      THIS CODE IS INTENDED FOR INSTRUCTIONAL USE ONLY!         *
*                                                                *
*****NOTE***** THIS CODE HAS BEEN REVISED FROM APPENDIX III      *
*              VERSION, WRITTEN FOR UNIVAC 1100/80, TO RUN ON    *
*              IBM 4381.  EVERY CHANGED STATEMENT IS ACCOMPANIED *
*              BY THE ORIGINAL STATEMENT WITH 'CCCCC' AT THE     *
*              BEGINNING.                                        *
*                                                                *
****WARNING*** THE FORTRAN 77 PRETEST DOLOOP FEATURE            *
*              IS USED EXTENSIVLY!                               *
*                                                                *
*  PARAMETER STORAGE IN THE PAR ARRAY IS AS FOLLOWS:             *
*  1. ESTIMATED SEASONALITY PARAMETERS, 2. MEANS, 3. PHI0 TERMS, *
*  4. PHI TERMS: SERIES 1, SERIES 2, ETC., 5. THETA TERMS        *
*                                                                *
*  ZERO LAG INVERSE FUNCTION COEFFICIENTS ARE STORED AT THE      *
*  END OF THE LAST COLUMN OF THE XTX ARRAY. (SUBROUTINE EAR)     *
*  JANUARY 1995.  ALAN HECKERT MADE THE FOLLOWING CHANGES FOR    *
*  NON-PC SYSTEMS.
*  1) CHANGED REAL*4 AND REAL*8 TO REAL AND DOUBLE PRECISION     *
*     (REAL*4 ETC. NOT ANSI STANDARD).  THERE ARE STILL 2        *
*     COMPLEX*16 STATEMENTS, WHICH MAY NEED TO BE MODIFIED ON    *
*     SOME SYSTEMS.                                              *
*  2) THE COMMON BLOCK DATA WAS RENAMED DDSDAT.                  *
*  3) THE "/" IN SOME FORMAT STATEMENTS WAS MODIFIED (BOMBS ON   *
*     UNIX WITH INTERNAL READ/WRITE WHICH DATAPLOT USES TO FUNNEL*
*     ALL OUTPUT THROUGH SINGLE ROUTINE.                         *
C     UPDATED         --APRIL     1996. DDS CODE MODIFIED (ALAN):
C                                       A) SOME DIMENSIONS TO DPDDS, USE
C                                          EQUIVALENCE
C                                       B) I/O CONSISTENT WITH DATAPLOT
C                                       C) USE IERROR RATHER THAN STOP
C                                       D) INCLUDE FILE FOR DDS COMMON
C                                          BLOCKS AND PARAMETER STATEMENTS
C                                       THESE CHANGES PROPOGATE TO LOWER
C                                       LEVEL DDS ROUTINES
******************************************************************
C
C     CALLING SEQUENCE:
C
C        DPDDS3 - DETPAR EAR GJR INVAL LS ATSCOR ANALYS
C
C        DETPAR - .
C        EAR - MODEL XX XA
C        GJR - .
C        INVAL - EAR GJR STBLIZ
C        LS - MODEL DIF STBLIZ GJR
C        ATSCOR - .
C        ANALYS - SPECTRUM GREEN FREQ EIGEN
C
C        STBLIZ - EIGEN
C        DIF - .
C        XA - .
C        MODEL - DETPAR
C        XX - .
C        FREQ - .
C        EIGEN - .
C        GREEN - .
C        SPECTRUM - .
C
CCCCC APRIL 1996.  FOLLOWING PUT IN INCLUDE FILE
CCCCC PARAMETER (MXSER=3,MXSR2=MXSER**2,MXNOB=1024,MXPAR=45,MXINV=50,
CCCCC>          MXNOB1=MXNOB+1,MXINV1=MXINV+1)
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODD.INC'
      DIMENSION XDDS(MAXOBV,MXSER)
      DIMENSION YDDS(MAXOBV,MXSER)
C
CCCCC DOUBLE PRECISION XTX(MXINV,MXINV1),XX
CCCCC COMMON /INDEX/ INDEX,MAXLAG,NOB,NSER,NPHI0,NPHI,NSEAS
CCCCC COMMON /LAG/ LAG(MXSER,MXSER),NAR(MXSER,MXSER),NMA(MXSER)
CCCCC COMMON /FLAG/ IFMEAN,IFAT,IFSEAS,IFGREEN,IFSPECTR
CCCCC COMMON /SEASON/ NPOLY,NEXP,NSIN,ISEAS(30),SEAS(30),DELTA
CCCCC APRIL 1996.  YDDS, XDDS DIMENSIONED IN 
CCCCC COMMON/DDSDAT/ YDDS(MXNOB,MXSER),XDDS(MXNOB,MXSER)
CCCCC COMMON /BLOK1/ STEP,CONTOL,ITMAX,IFREF
CCCCC COMMON /JOSHI/ XX
      DOUBLE PRECISION XTX(MXINV,MXINV1)
CCCCC APRIL 1996.  USE DPOPFI TO OPEN FILES
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
      CHARACTER*80 IFILE2
      CHARACTER*12 ISTAT2
      CHARACTER*12 IFORM2
      CHARACTER*12 IACCE2
      CHARACTER*12 IPROT2
      CHARACTER*12 ICURS2
      CHARACTER*4 IERRF2
      CHARACTER*4 IENDF2
      CHARACTER*4 IREWI2
C
      CHARACTER*80 IFILE3
      CHARACTER*12 ISTAT3
      CHARACTER*12 IFORM3
      CHARACTER*12 IACCE3
      CHARACTER*12 IPROT3
      CHARACTER*12 ICURS3
      CHARACTER*4 IERRF3
      CHARACTER*4 IENDF3
      CHARACTER*4 IREWI3
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
      DIMENSION Y1(*)
      DIMENSION PRED2(*)
      DIMENSION RES2(*)
C
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
CCCCC MAY 1995.  ADD FOLLOWING LINE
      CHARACTER*4 ISUBRO
CCCCC APRIL 1996.  ADD FOLLOWING LINE
      CHARACTER*4 ISUBN0
C
      DIMENSION AT(MXNOB1,MXSER),PAR(MXPAR),AVG(MXSER),VAR(MXSER)
     >          ,OSSQ(MXSER),SCA(MXSER)
CCCCC CHARACTER*40 TITLE
      CHARACTER FORMT*50
C
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'DDS3')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,51)
   51 FORMAT('**** AT THE BEGINNING OF DPDDS3--')
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
      DO56I=1,N1
      WRITE(ICOUT,57)I,Y1(I)
   57 FORMAT('I,Y1(I) = ',I8,E15.7)
      CALL DPWRST('XXX','WRIT')
   56 CONTINUE
      WRITE(ICOUT,65)IORDAR,IORDMA
   65 FORMAT('IORDAR,IORDMA = ',2I8)
      CALL DPWRST('XXX','WRIT')
   90 CONTINUE
CCCCC WRITE(ICOUT,65)IORDAR,IORDMA
CCC65 FORMAT('IORDAR,IORDMA = ',2I8)
CCCCC CALL DPWRST('XXX','WRIT')
C
      NPHI0=0
      NPHI=0
      NSEASP=0
      NSEAS=0
      NEWFLAG=0
C
      DO2000 JJ1 = 1,MXSER
         DO2001 JJ2 = 1,MXSER
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
C
      ITEST=1
C
C     READ INPUT DATA
C
      IF(ITEST.EQ.1)THEN
CCCCC    WRITE(IOUT,1000)
1000     FORMAT(' THE PROGRAM READS FROM FILE "5" AND WRITES TO '
     >   ,/,' FILE "6/12."  IF YOUR EXEC IS NOT SET UP THIS WAY,',/,
     >    ' ENTER A "1", OR ENTER A "0" TO CONTINUE.  NOTE THAT ',/,
     >    ' ALL RESULTS ARE ECHOED TO THE SCREEN.')
CCCCC    READ(IIN,*) ICONT
         ICONT=0
CCCCC APRIL 1996.  SET IERROR FLAG INSTEAD OF STOP
CCCCC    IF(ICONT.EQ.1) STOP 'RE-DO YOUR EXEC TO FIT THE PROGRAM.'
         IF(ICONT.EQ.1) THEN
           WRITE(ICOUT,1001)
 1001      FORMAT('*****ERROR IN DPDDS, BAD VALUE FOR ICONT FLAG')
           CALL DPWRST('XXX','WRIT')
           IERROR='YES'
           GOTO9999
         ENDIF
CCCCC    READ(5,10) TITLE
10       FORMAT(A)
C
CCCCC    READ(5,*) NOB,NSER,DELTA,NMOD,INCPH,INCTH
         NOB=N1
         NSER=1
         DELTA=DELTAT
         NMOD=1
         INCPH=1
         INCTH=1
C
CCCCC    READ(5,*) ((NAR(J,I),J=1,NSER),NMA(I),I=1,NSER)
         NAR(1,1)=IORDAR
         NMA(1)=IORDMA
C
CCCCC    READ(5,*) STEP,CONTOL,ITMAX,IFREF
         STEP=.001
         CONTOL=.00001
         ITMAX=20
         IFREF=0
C
CCCCC    READ(5,*) IFMEAN,IFAT,IFDIF,IFLAG,IFSEAS,IFSER,IFPRT
         IFMEAN=0
         IFAT=0
         IFDIF=0
         IFLAG=0
         IFSEAS=0
         IFSER=0
         IFPRT=1
C
CCCCC    READ(5,*) IFATS,IFGREEN,IFSPECTR
         IFATS=1
         IFGREEN=1
         IFSPECTR=1
C
      ENDIF
C
CCCCC    WRITE(ICOUT,*) TITLE
CCCCC WRITE(ICOUT,'(2I5,E15.8,3I4)') NOB,NSER,DELTA,NMOD,INCPH,INCTH
CCCCC    DO15I=1,NSER
CCC15        WRITE(ICOUT,*) (NAR(J,I),J=1,NSER),NMA(I)
CCCCC  WRITE(ICOUT,'(2E15.8,2I6)') STEP,CONTOL,ITMAX,IFREF
CCCCC WRITE(ICOUT,'(7I6)') IFMEAN,IFAT,IFDIF,IFLAG,IFSEAS,IFSER,IFPRT
C
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
CCCCC WRITE(ICOUT,43)IORDAR
CCC43 FORMAT(1H ,'Order of Autoregressive Terms    = ',I8)
CCCCC WRITE(ICOUT,44)IORDMA
CCC44 FORMAT(1H ,'Order of Moving Average Terms    = ',I8)
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
C
C     ECHO CHECK INPUT
CCCCC WRITE(ICOUT,20) NSER
CCCCC WRITE(3,20) NSER
CCC20 FORMAT (5X,'SERIES',I4)
CCC20 FORMAT (5X,'SERIES',I4,': ARMA(',I3,6X,I3,',',I3,' )',/)
CCCCC WRITE(ICOUT,*) TITLE
CCCCC WRITE(3,30) (TITLE(JJ1),JJ1=1,17)
CCC30 FORMAT (6X,'DATA DEPENDENT SYSTEMS MODELING ROUTINE - MTU 1982
CCCCC>',
CCCCC>//,6X,'THIS PROGRAM FITS UNIVARIATE ARMA AND EXTENDED ',/,6X
CCCCC>,'MODELS TO TIME SERIES DATA, WITH OPTIONAL DETERMINISTIC TREND
CCCCC>',/,6X,'ESTIMATION FOR UNIVARIATE MODELS.',
CCCCC>'  IT IS DESIGNED TO BE USED'
CCCCC>,/,6X,'WITH THE BOOK:' ,
CCCCC>/,6X,'   "TIME SERIES AND SYSTEM ANALYSIS WITH APPLICATIONS"'
CCCCC>,/,6X,'             BY S. M. PANDIT AND S. M. WU' ,
CCCCC>//,6X,'    THIS CODE IS INTENDED FOR INSTRUCTIONAL USE ONLY!'
CCCCC>,/,2X,70('*'),////,1X,17A4)
CCCCC WRITE(ICOUT,60) IFMEAN,IFAT,IFDIF,NMOD,IFPRT,IFLAG,IFSEAS,IFSER,
CCCCC>INCPH,INCTH
CCCCC WRITE(3,60) IFMEAN,IFAT,IFDIF,NMOD,IFPRT,IFLAG,IFSEAS,IFSER,
CCCCC>INCPH,INCTH
CCC60 FORMAT(/,15X,'PROGRAM CONTROL FLAGS',/,5X,'IFMEAN:',I3,4X,
CCCCC>'IFAT:',I3,'  IFDIF:',I3,'  NMOD:',I3,'  IFPRT:',I3,
CCCCC> /,6X,'IFLAG:',
CCCCC>I3,'  IFSEAS:',I3,'  IFSER:',I3,' INCPH:',I3,'  INCTH:',I3)
CCCCC WRITE(ICOUT,70) STEP,CONTOL,ITMAX,IFREF
CCCCC WRITE(3,70) STEP,CONTOL,ITMAX,IFREF
CCC70 FORMAT (5X,'STEP=',E10.4,'   CONVERGENCE TOLERANCE=',E10.4
CCCCC>,/,5X,I5,' ITERATIONS MAX    IFREF:',I4,//)
CCCCC WRITE(ICOUT,600) MXSER,MXNOB,MXPAR,MXINV
CC600  FORMAT(/,' --- PROGRAM SIZE LIMITS ---',
CCCCC>       /,'      MAX # SERIES=',I4,
CCCCC>       /,'      MAX # OBSER =',I4,
CCCCC>       /,'      MAX # PARAM =',I4,
CCCCC>       /,'      MAX # INVER =',I4,
CCCCC>       /)
CCCCC WRITE(ICOUT,601)
CC601   FORMAT(/,' (***LAST UPDATE***13 AUG 84 /19:02:14|)',
CCCCC>       /,' (1.SUMMARY ELT IN 11.                   )',
CCCCC>       /,' (2.IF NMOD=1, AT-S IN 12.               )',
CCCCC>       /,' (3.FIX: AT(MXNOB)->AT(MXNOB1) IN MODEL  )',
CCCCC>       /,/)
CCCCC WRITE(ICOUT,40)NSER,NOB,DELTA
CCCCC WRITE(3,40)NSER,NOB,DELTA
CCC40 FORMAT (1X,I6,' SERIES WITH',I5,' OBSERVATIONS, DELTA =',E10.4,//,
CCCCC>      '    INPUT STARTING MODEL ORDERS:',/)
CCCCC    DO50I=1,NSER
CCC50       WRITE(ICOUT,*) I,(NAR(J,I),J=1,NSER),NMA(I)
C     CHECK FOR ERRORS IN INPUT
CCCCC IF(NSER.GT.MXSER) STOP'TOO MANY DATA SETS, MXSER=3'
CCCCC IF(NOB.GT.MXNOB) STOP'TOO MANY OBSERVATIONS, MXNOB=1024'
CCCCC IF(IFSEAS.GT.0.AND.IFDIF.NE.0)
CCCCC>   STOP'NUMERICAL DERIVATIVES MUST BE USED WITH SEASONALITY'
CCCCC IF(IFSEAS.GT.0.AND.NSER.GT.1)
CCCCC>   STOP'ONLY ONE SERIES MAY BE USED WITH SEASONALITY'
CCCCC IF(IFSEAS.EQ.1.AND.NMOD.LT.0)
CCCCC>   STOP'NO STOCHASTIC INITIAL VALUES FOR IFSEAS = 1'
CCCCC IF((IFSEAS.EQ.1.OR.IFSEAS.EQ.3).AND.NMOD.GT.1)
CCCCC>   STOP'ONLY ONE MODEL AT A TIME FOR IFSEAS=1 OR 3'
C
C     DETERMINE LAGS FOR MULTI-VARIATE MODELS
C
CCCCC APRIL 1996.  IN FOLLOWING BLOCK, USE IRD AS THE INPUT UNIT
         IF(NSER.GT.1)THEN
            DO100I=1,NSER
                  IF(IFLAG.EQ.0)THEN
                     DO80J=I+1,NSER
80                      LAG(J,I)=0.0
                   ELSE
                      IF(I.EQ.1)THEN
CCCCC                    READ(5,*) (LAG(J,I),J=2,NSER)
                         READ(IRD,*) (LAG(J,I),J=2,NSER)
                      ELSEIF(I.EQ.NSER)THEN
CCCCC                    READ(5,*) (LAG(J,I),J=1,I-1)
                         READ(IRD,*) (LAG(J,I),J=1,I-1)
                      ELSE
CCCCC                   READ(5,*) (LAG(J,I),J=1,I-1),(LAG(J,I),J=I+1,NSER)
                        READ(IRD,*) (LAG(J,I),J=1,I-1),
     >                              (LAG(J,I),J=I+1,NSER)
                      ENDIF
                 ENDIF
CCCCC            WRITE(ICOUT,90) I,(LAG(J,I),J=1,NSER)
CCC90            FORMAT (5X,'SERIES',I4,':  LAGS=',10(I5,','),/)
CCCCC            CALL DPWRST('XXX','WRIT')
100         CONTINUE
         ENDIF
C
C     READ SEASONALITY INPUT IF SEASONALITY FLAG IS SET
C
CCCCC APRIL 1996.  IN FOLLOWING BLOCK, USE IRD AS THE INPUT UNIT, SET
CCCCC IERROR FLAG INSTEAD OF STOP
         IF(IFSEAS.GT.0)THEN
CCCCC       READ(5,*) NPOLY,NEXP,NSIN
            READ(IRD,*) NPOLY,NEXP,NSIN
            NSEASP=NPOLY+2*NEXP+4*NSIN+1
CCCCC       IF(NSEASP.GT.30)
CCCCC>         STOP'TOO MANY SEASONALITY PARAMETERS, MAX=30'
            IF(NSEASP.GT.30)THEN
              WRITE(ICOUT,1011)
 1011         FORMAT('*****ERROR IN DPDDS--TOO MANY SEASONAILITY ',
     1               'PARAMETERS, MAXIMUM = 30.')
              CALL DPWRST('XXX','WRIT')
              IERROR='YES'
              GOTO9999
            ENDIF
CCCCC       READ(5,*) (SEAS(I),ISEAS(I),I=1,NSEASP)
            READ(IRD,*) (SEAS(I),ISEAS(I),I=1,NSEASP)
            WRITE(ICOUT,111) NPOLY,NEXP
111         FORMAT(5X,'SEASONALITY:',I3,' POLYNOMIAL,',I3,
     >             ' EXPONENTIAL')
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,112) NSIN
112         FORMAT(' AND',I3,' SINUSOIDAL TRENDS')
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,113) 
 113        FORMAT (' ')
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,110) (I,SEAS(I),I=1,NSEASP)
CCCCC       WRITE(3,110) NPOLY,NEXP,NSIN,(I,SEAS(I),I=1,NSEASP)
110         FORMAT(5X,5('BETA(',I2,')=',E11.4,5X))
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,116) 
 116        FORMAT (' ')
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,117) 
 117        FORMAT(5X,'ESTIMATION FLAGS:')
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,115) (I,ISEAS(I),I=1,NSEASP)
CCCCC       WRITE(3,115) (I,ISEAS(I),I=1,NSEASP)
115         FORMAT(7X,5('IC(',I2,')=',I4,14X))
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,116) 
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,116) 
            CALL DPWRST('XXX','WRIT')
C
C     MODIFY SEASONALITY PARAMETERS FOR SAMPLING INTERVAL
C
            N=NPOLY+1
               DO120I=1,NEXP
                  N=N+2
                  SEAS(N)=SEAS(N)*DELTA
  120          CONTINUE
               DO130I=1,NSIN
                  N=N+2
                  SEAS(N)=SEAS(N)*DELTA
                  N=N+2
                  SEAS(N)=SEAS(N)*DELTA*6.2831853
  130          CONTINUE
C
C     LOAD SEASONALITY PARAMETERS INTO PARAMETER ARRAY
C
               IF(IFSEAS.EQ.1.OR.IFSEAS.EQ.3)THEN
                     DO140I=1,NSEASP
                        IF(ISEAS(I).EQ.0)THEN
                           PAR(IPAR)=SEAS(I)
                           ISEAS(IPAR)=I
                           IPAR=IPAR+1
                        ENDIF
140                  CONTINUE
                  NSEAS=IPAR-1
               ENDIF
         ENDIF
C
C     READ IN DATA SERIES
C
CCCCC AUGUST, 1995.  MODIFIY FOLLOWING LINE FOR CRAY.
CCCCC CRAY DOESN'T ALLOW FREE FORMAT FOR INTERNAL WRITE.
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DDS3')THEN
CCCCC    WRITE(ICOUT,*)'JUST BEFORE THE DEFINITION OF YDDS'
         WRITE(ICOUT,9072)
 9072    FORMAT('JUST BEFORE THE DEFINITION OF YDDS')
         CALL DPWRST('XXX','WRIT')
CCCCC    WRITE(ICOUT,*)NOB,NSER
         WRITE(ICOUT,9073)NOB,NSER
 9073    FORMAT(I8,3X,I8)
         CALL DPWRST('XXX','WRIT')
      ENDIF
C
CCCCC READ(5,*) ((YDDS(I,J),I=1,NOB),J=1,NSER)
      DO144I=1,NOB
         YDDS(I,1)=Y1(I)
  144 CONTINUE
 
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'DDS3')THEN
CCCCC AUGUST, 1995.  MODIFIY FOLLOWING LINE FOR CRAY.
CCCCC CRAY DOESN'T ALLOW FREE FORMAT FOR INTERNAL WRITE.
CCCCC    WRITE(ICOUT,*)'THE YDDS DEFINITION LOOP HAS JUST BEEN EXECUTED'
         WRITE(ICOUT,9075)
 9075    FORMAT('THE YDDS DEFINITION LOOP HAS JUST BEEN EXECUTED')
         CALL DPWRST('XXX','WRIT')
      ENDIF
C
C     DETREND DATA TO ESTIMATE STOCHASTIC PART OF COMBINED MODELS
C
CCCCC APRIL 1996.  ADD XDDS, YDDS TO CALL LIST, NOB PASSED THROUGH
CCCCC COMMON
CCCCC IF(IFSEAS.EQ.2)   CALL DETPAR(NOB)
      IF(IFSEAS.EQ.2)   CALL DETPAR(XDDS,YDDS)
C
C     FIND AVERAGE AND VARIANCE OF EACH SERIES AND PRINT
C
CCCCC WRITE(ICOUT,145)
CC145 FORMAT (//,'   CHARACTERISTICS OF THE DATA',/)
CCCCC CALL DPWRST('XXX','WRIT')
C
         DO190I=1,NSER
            SUM=0.0
            VAR(I)=0.0
            DO150J=1,NOB
                  SUM=SUM+YDDS(J,I)
  150       CONTINUE
CCCCC       AVG(I)=SUM/NOB
            AVG(I)=SUM/REAL(NOB)
            DO160J=1,NOB
                  VAR(I)=VAR(I)+(YDDS(J,I)-AVG(I))**2
  160       CONTINUE
            VAR(I)=VAR(I)/REAL(NOB-1)
            SDI=SQRT(VAR(I))
C
            WRITE(ICOUT,170)
  170       FORMAT(1H ,'Series      Obs.  Average      Variance    SD')
            CALL DPWRST('XXX','WRIT')
C
            WRITE(ICOUT,171) I,N1,AVG(I),VAR(I),SDI
  171       FORMAT(1H ,I3,2X,I8,4X,3G12.5)
            CALL DPWRST('XXX','WRIT')
C
C     SCALE DATA FOR EARMA MODELS
C
            IF(NSER.GT.1)THEN
               SCA(I)=VAR(I)
               IF(IFMEAN.GT.0) SCA(I)=SCA(I)+AVG(I)**2
               SCA(I)=SQRT(SCA(I))
               DO185J=1,NOB
                   YDDS(J,I)=YDDS(J,I)/SCA(I)
  185          CONTINUE
               AVG(I)=AVG(I)/SCA(I)
           ENDIF
C
C     SUBTRACT AVERAGE FROM DATA IF MEAN IS NOT ESTIMATED
C
           IF(IFMEAN.LE.0.AND.IFSEAS.EQ.0)THEN
              IF(IFMEAN.LT.0) AVG(I)=XDDS(-IFMEAN,I)
              DO180J=1,NOB
                YDDS(J,I)=YDDS(J,I)-AVG(I)
                XDDS(J,I)=YDDS(J,I)
  180         CONTINUE
           ENDIF
190      CONTINUE
C
C     CHECK FOR INITIAL VALUE FLAG
C
         IF(NMOD.LT.0)THEN
            IFINV=1
            NMOD=-NMOD
         ENDIF
CCCCC APRIL 1996.  OPEN FILES VIA THE DPOPFI ROUTINE
C
C
         IF(IFATS .EQ. 1)THEN
CCCCC          OPEN(UNIT=2,FILE='residuals.out',STATUS='UNKNOWN')
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
               CALL DPOPFI(IOUNI1,IFILE1,ISTAT1,IFORM1,IACCE1,IPROT1,
     1                     ICURS1,
     1                     IREWI1,ISUBN0,IERRF1,IBUGA3,ISUBRO,IERROR)
               IF(IERRF1.EQ.'YES')THEN
                 WRITE(ICOUT,1091)
 1091            FORMAT('***** ERROR IN DDS--UNABLE TO OPEN ',
     >                  'residuals.out')
                 CALL DPWRST('XXX','WRIT')
                 IERROR='YES'
                 GOTO9999
               ENDIF
         ENDIF
         IF(IFGREEN .EQ. 1)THEN
CCCCC          OPEN(UNIT=3,FILE='green.out',STATUS='UNKNOWN')
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
               CALL DPOPFI(IOUNI2,IFILE2,ISTAT2,IFORM2,IACCE2,IPROT2,
     1                     ICURS2,
     1                     IREWI2,ISUBN0,IERRF2,IBUGA3,ISUBRO,IERROR)
               IF(IERRF2.EQ.'YES')THEN
                 WRITE(ICOUT,1092)
 1092            FORMAT('***** ERROR IN DDS--UNABLE TO OPEN green.out')
                 CALL DPWRST('XXX','WRIT')
                 IERROR='YES'
                 GOTO9999
               ENDIF
         ENDIF
         IF(IFSPECTR .EQ. 1)THEN
CCCCC          OPEN(UNIT=4,FILE='spectrum.out',STATUS='UNKNOWN')
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
               CALL DPOPFI(IOUNI3,IFILE3,ISTAT3,IFORM3,IACCE3,IPROT3,
     1                     ICURS3,
     1                     IREWI3,ISUBN0,IERRF3,IBUGA3,ISUBRO,IERROR)
               IF(IERRF3.EQ.'YES')THEN
                 WRITE(ICOUT,1093)
 1093            FORMAT('***** ERROR IN DDS--UNABLE TO OPEN ',
     >                  'spectrum.out')
                 CALL DPWRST('XXX','WRIT')
                 IERROR='YES'
                 GOTO9999
               ENDIF
         ENDIF
C
C     LOOP ON NUMBER OF MODELS
C
         DO290 MOD=1,NMOD
C
C     SET INITIAL VALUES OF MEANS IF ESTIMATED
C
            IF(IFMEAN.GT.0.AND.IFSEAS.EQ.0)THEN
               IPAR=NSEAS+1
                  DO210I=1,NSER
                     PAR(IPAR)=AVG(I)
                     IF(IFMEAN.GT.1) PAR(IPAR)=YDDS(IFMEAN-1,I)
                     IPAR=IPAR+1
C
C     SUBTRACT AVERAGE TO RESET DATA FOR INITIAL VALUES OF NEW MODEL
C
                        DO210J=1,NOB
                           XDDS(J,I)=YDDS(J,I)-AVG(I)
  210                   CONTINUE
            ENDIF
C
C     SKIP STOCHASTIC ESTIMATION FOR DETERMINISTIC MODELS
C
         NPAR=NSEAS
         IF(IFSEAS.EQ.1)THEN
             NEWFLAG=1
             GO TO 280
         ENDIF
C
C     CHECK SERIES INDEX FLAG
C
            IF(IFSER.GT.0.AND.IFSER.LE.NSER)THEN
               INDEX=IFSER
               GO TO 280
            ENDIF
C
C     LOOP ON NUMBER OF DATA SERIES
C
             INDEX=1
C
  280 CONTINUE
            IF(INDEX .LE. NSER)THEN
CCCCC       DO280 INDEX=1,NSER
             IF(NEWFLAG .EQ. 1)THEN
                 NEWFLAG = 0
                 GO TO 250
             ENDIF
C
C     FIND NUMBER OF PHI0'S AND PHI'S, AND MAXLAG FOR THIS SERIES
C
220            NPHI0=0
               NPHI=0
               MAXLAG=1
                  DO230I=INDEX+1,NSER
230                  IF(LAG(I,INDEX).EQ.0)NPHI0=NPHI0+1
                     DO235I=1,NSER
                       ML=LAG(I,INDEX)+NAR(I,INDEX)-1
                       IF(ML.GT.MAXLAG) MAXLAG=ML
                       NPHI=NPHI+NAR(I,INDEX)
  235                CONTINUE
                     MAXLAG=MAX(MAXLAG,NMA(INDEX))
                     NPAR=NPHI0+NPHI+NMA(INDEX)+IPAR-1
C
C     SET INITIAL VALUES OF INITIAL AT'S IF ESTIMATED
C
                     IF(IFAT.LT.0)THEN
                        DO240I=1,MAXLAG
                           PAR(NPAR+I)=0.1
  240                   CONTINUE
                        NPAR=NPAR+MAXLAG
                     ENDIF
CCCCC APRIL 1996.  SET ERROR FLAG INSTEAD OF STOP
CCCCC                IF(NPAR.GT.MXPAR) STOP'TOO MANY PARAMETERS, MXPAR=45'
                     IF(NPAR.GT.MXPAR) THEN
                       WRITE(ICOUT,9110)
                       CALL DPWRST('XXX','WRIT')
                       IERROR='YES'
                       GOTO9999
                     ENDIF
 9110 FORMAT('***** ERROR IN DDS--MAXIMUM OF 45 PARAMETERS EXCEEDED.')
CCCCC               WRITE(ICOUT,'(/////)')
CCCCC               CALL DPWRST('XXX','WRIT')
C                   IF(IFPRT.GT.0)
C    >              WRITE(ICOUT,*) INDEX,(NAR(J,INDEX),J=1,NSER),NMA(INDEX)
C                   CALL DPWRST('XXX','WRIT')
C
C     PURE AR MODELS
C
                  IF(NMA(INDEX).EQ.0.AND.IFINV.NE.1)THEN
                     ISIZ=NPHI+NPHI0
                     ISIZ1=ISIZ+1
CCCCC APRIL 1996.  ADD XDDS, YDDS TO ARGUMENT LIST
CCCCC                CALL EAR(XTX,NAR(1,INDEX),LAG(1,INDEX),PAR,ISIZ)
                     CALL EAR(XTX,NAR(1,INDEX),LAG(1,INDEX),PAR,ISIZ,
     >                        XDDS,YDDS)
                     CALL GJR(XTX,ISIZ,4,MXINV,MXINV1,IERROR)
                     IPAR=IPAR-1
C
C     LOAD PHI0'S INTO PAR
C
                     DO245I=1,NPHI0
                           PAR(I+IPAR)=XTX(NPHI+I,ISIZ1)
  245                CONTINUE
                     IPAR=IPAR+NPHI0
C
C     LOAD PHI'S INTO PAR
C
                      DO246I=1,NPHI
                           PAR(I+IPAR)=XTX(I,ISIZ1)
  246                 CONTINUE
                     IPAR=IPAR-NPHI0+1
                     GO TO 250
                  ENDIF
C
C     READ INITIAL VALUES IF IFINV = 1 (NMOD LT 0)
C
CCCCC APRIL 1996.  USE IRD
                  IF(IFINV.EQ.1)THEN
                     IF(IFMEAN.GT.0.AND.IFSEAS.EQ.0)
CCCCC>               READ(5,*) (PAR(I),I=IPAR-NSER,IPAR-1)
CCCCC                READ(5,*) (PAR(I),I=IPAR,NPAR)
     >               READ(IRD,*) (PAR(I),I=IPAR-NSER,IPAR-1)
                     READ(IRD,*) (PAR(I),I=IPAR,NPAR)
                  ELSE
C
C     FIND INITIAL VALUES BY INVERSE FUNCTION METHOD
C
CCCCC APRIL 1995.  ADD XDDS, YDDS TO ARGUMENT LIST
CCCCC                CALL INVAL(XTX,PAR,SCA,NPAR)
                     CALL INVAL(XTX,PAR,SCA,NPAR,XDDS,YDDS,IERROR)
                  ENDIF
C
C     SCALE INITIAL PARAMETERS FOR EARMA MODELS
C
                  IF(NSER.GT.1)THEN
                     N=NSEAS+1
                     IF(IFMEAN.GE.1) N=N+NSER
C
C     SCALE ZERO LAG TERMS
C
                        DO247I=INDEX+1,NSER
                           IF(LAG(I,INDEX).EQ.0)THEN
                              PAR(N)=PAR(N)*SCA(I)/SCA(INDEX)
                              N=N+1
                           ENDIF
247                     CONTINUE
C
C     SCALE REST OF PHI TERMS
C
                         DO249I=1,NSER
                           IF(I.NE.INDEX)THEN
                             NN=N
                             SCALE=SCA(I)/SCA(INDEX)
                             DO248J=1,NAR(I,INDEX)
                               PAR(NN)=PAR(NN)*SCALE
                               NN=NN+1
  248                        CONTINUE
                           ENDIF
                           N=N+NAR(I,INDEX)
  249                    CONTINUE
                    ENDIF
C
C     DO NON-LINEAR LEAST SQUARES SEARCH TO FIND BEST PARAMETERS
C
  250 CONTINUE
CCCCC APRIL 1996.  ADD XDDS, YDDS TO ARGUMENT LIST
               CALL LS(NPAR,PAR,AT(1,INDEX),FORMT,IFDIF,SCA,IFPRT,
CCCCC1         SEXT,NDF,IORDAR,IORDMA)
     1         SEXT,NDF,IORDAR,IORDMA,XDDS,YDDS,IERROR)
               IF(IERROR.EQ.'YES')GOTO9999
C
C     CHECK FOR AUTO-REFINEMENT OF PARAMETERS
C
                  IF(IFREF.GT.0)THEN
                     IFREF=-IFREF
                     STEP=STEP*10.**FLOAT(IFREF)
                     CONTOL=CONTOL*10.**FLOAT(IFREF)
CCCCC                IF(IFPRT.GT.0) WRITE(3,255) STEP,CONTOL
                     IF(IFPRT.GT.0) THEN
                       WRITE(ICOUT,256)
256                    FORMAT(' ')
                       CALL DPWRST('XXX','WRIT')
                       WRITE(ICOUT,256)
                       CALL DPWRST('XXX','WRIT')
                       WRITE(ICOUT,255) STEP
255                    FORMAT(' NEW CONVERGENCE PARAMETERS:',5X,'STEP='
     >                        ,E12.4,5X)
                       CALL DPWRST('XXX','WRIT')
                       WRITE(ICOUT,257) CONTOL
257                    FORMAT(1X,' CONVERGENCE TOLERANCE=',E12.4)
                       CALL DPWRST('XXX','WRIT')
                     ENDIF
C
C     REDO ESTIMATION AND RESTORE ESTIMATION PARAMETERS
C
CCCCC APRIL 1996.  ADD XDDS, YDDS TO ARGUMENT LIST
                     CALL LS(NPAR,PAR,AT(1,INDEX),FORMT,IFDIF,SCA,IFPRT,
CCCCC1                       SEXT,NDF,IORDAR,IORDMA)
     1                       SEXT,NDF,IORDAR,IORDMA,XDDS,YDDS,IERROR)
                     IF(IERROR.EQ.'YES')GOTO9999
                     IFREF=-IFREF
                     STEP=STEP*10.**FLOAT(IFREF)
                     CONTOL=CONTOL*10.**FLOAT(IFREF)
                  ENDIF
C
CCCCC COPY PREDICTED VALUES AND RESIDUALS INTO PRED2 AND RES2
CCCCC FOR USE BY DATAPLOT
C
      DO750I=1,NOB
         RES2(I)=AT(I,1)
         PRED2(I)=Y1(I)-RES2(I)
  750 CONTINUE
      RESSD=SEXT
      RESDF=NDF
C
C     PRINT THE RESIDUALS FOR THE CURRENT MODEL
C
CCCCC APRIL 1996.  USE IOUNI1 INSTEAD OF HARD-CODED 2.
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
          DO776 I =1,NSER
CCCCC       WRITE(2,777) I
CC777       FORMAT('SERIES NUMBER', I3)
            DO778J=1,NOB
              WRITE(IOUNI1,779) AT(J,I)
779           FORMAT(F10.3)
778         CONTINUE
776       CONTINUE
        ENDIF
C
C     FIND AND PRINT F-TEST VALUE
C
         IF(NMOD.GT.1)THEN
               IF(MOD.GT.1)THEN
                  FTST=(OSSQ(INDEX)/AT(NOB+1,INDEX)-1.)*
     >            FLOAT((NOB-NPAR)/(NSER*INCPH+INCTH))
                  WRITE(ICOUT,264) 
 264              FORMAT (' ')
                  CALL DPWRST('XXX','WRIT')
                  WRITE(ICOUT,264) 
                  CALL DPWRST('XXX','WRIT')
                  WRITE(ICOUT,265) FTST
CCCCC             WRITE(3,265) FTST
265               FORMAT(' F-TEST WITH LAST MODEL FOR THIS SERIES ='
     >                   ,F10.3)
                  CALL DPWRST('XXX','WRIT')
               ENDIF
            OSSQ(INDEX)=AT(NOB+1,INDEX)
         ENDIF
      NDEX = NSER
      IF(IFSER.GT.0) NDEX = IFSER
CCCCC IF(NMOD.EQ.1) WRITE(12,'(5E15.8)') (AT(I,NDEX),I=1,NOB)
C
C     FIND CORRELATIONS OF RESIDUALS
C
CCCCC APRIL 1996.  ADD MXNOB1, MXSER TO ARGUMENT LIST
CCCCC          CALL ATSCOR(AT,NOB,INDEX,IFSER)
               CALL ATSCOR(AT,NOB,INDEX,IFSER,MXNOB1,MXSER)
C
C     FIND ROOTS, FREQUENCIES, DAMPING, AND GREEN'S FUNCTION COEFFICIENTS
C
CCCCC APRIL 1996.  IN FOLLOWING BLOCK OF CODE, USE IOUNI2 RATHER THAN 3.
               IF(IFSEAS.NE.1.AND.IFPRT.GT.0)THEN
                 IF(IFGREEN .EQ. 1)THEN
                   WRITE(IOUNI2,781)
  781              FORMAT('This is Dataplot file GREEN.OUT ',
     >                    '(DDS command)')
                   WRITE(IOUNI2,782) NAR(1,INDEX), NMA(INDEX)
  782              FORMAT('Greens function for ARMA(',I2,',',I2,
     >                    ') model')
                   WRITE(IOUNI2,783)
  783              FORMAT(1H )
                   WRITE(IOUNI2,784)
  784              FORMAT('index   Green (total)   Green (comp. 1) ...')
                   WRITE(IOUNI2,785)
  785              FORMAT('------------------------------------------',
     >                    '(line 5)')
                 ENDIF
CCCCC APRIL 1996.  IN FOLLOWING BLOCK OF CODE, USE IOUNI3 RATHER THAN 4.
                 IF(IFSPECTR .EQ. 1)THEN
                   WRITE(IOUNI3,791)
  791              FORMAT('This is Dataplot file  SPECTRUM.OUT  ',
     >                    '(DDS command)')
                   WRITE(IOUNI3,792) NAR(1,INDEX), NMA(INDEX)
  792              FORMAT('Spectrum for ARMA(',I2,',',I2,') model')
                   WRITE(IOUNI3,793)
  793              FORMAT(1H )
                   WRITE(IOUNI3,794)
  794              FORMAT('index   Spectrum (total)   Spectrum ',
     >                    '(comp. 1) ','...')
                   WRITE(IOUNI3,795)
  795              FORMAT('------------------------------------------',
     >                    '(line 5)')
                 ENDIF
CCCCC APRIL 1996.  ADD I/O UNITS TO ARGUMENT LIST, DELTA PASSED
CCCCC THROUGH COMMON.
CCCCC            CALL ANALYS(PAR,DELTA,VAR)
                 CALL ANALYS(PAR,VAR,IOUNI2,IOUNI3)
               ENDIF
C
C     UPDATE MODEL ORDERS FOR NEXT MODEL
C
                  DO270I=1,NSER
                     NAR(I,INDEX)=NAR(I,INDEX)+INCPH
  270             CONTINUE
                  NMA(INDEX)=NMA(INDEX)+INCTH
CCCCC             WRITE(ICOUT,285)
CCCCC             CALL DPWRST('XXX','WRIT')
                  INDEX=INDEX+1
                  IF(IFSER .EQ. 0) GO TO 280
        ENDIF
CCCCC       WRITE(3,285)
CC285       FORMAT (///,1X,72('*'),//)
290      CONTINUE
C
CCCCC WRITE(IOUT,'(42A1)')  ' A SUMMARY OF MODEL(S) IS(ARE) IN FILE 6.'
CCCCC IF(NMOD.EQ.1) WRITE(IOUT,2004) NDEX
2004  FORMAT(' ',I5,'-TH SERIES RESIDUALS ARE IN FILE 12.')
C
CCCCC WRITE OUT MESSAGES ABOUT THE OUTPUT FILES
C
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
C
CCCCC APRIL 1996.  USE DPCLFI ROUTINE
      IF(IFATS .EQ. 1)THEN
CCCCC   CLOSE(2)
C
        IENDF1='OFF'
        IREWI1='ON'
        CALL DPCLFI(IOUNI1,IFILE1,ISTAT1,IFORM1,IACCE1,IPROT1,ICURS1,
     1  IENDF1,IREWI1,ISUBN0,IERRF1,IBUGA3,ISUBRO,IERROR)
        IF(IERRF1.EQ.'YES')THEN
          WRITE(ICOUT,9211)
 9211     FORMAT('**** ERROR IN DDS TRYING TO CLOSE RESIDUAL.OUT FILE.')
          IERROR='YES'
          GOTO9999
        ENDIF
C
      ENDIF
      IF(IFGREEN .EQ. 1)THEN
CCCCC   CLOSE(3)
        IENDF2='OFF'
        IREWI2='ON'
        CALL DPCLFI(IOUNI2,IFILE2,ISTAT2,IFORM2,IACCE2,IPROT2,ICURS2,
     1  IENDF2,IREWI2,ISUBN0,IERRF2,IBUGA3,ISUBRO,IERROR)
        IF(IERRF2.EQ.'YES')THEN
          WRITE(ICOUT,9221)
 9221     FORMAT('**** ERROR IN DDS TRYING TO CLOSE GREEN.OUT FILE.')
          IERROR='YES'
          GOTO9999
        ENDIF
C
      ENDIF
      IF(IFSPECTR .EQ. 1)THEN
CCCCC   CLOSE(4)
        IENDF3='OFF'
        IREWI3='ON'
        CALL DPCLFI(IOUNI3,IFILE3,ISTAT3,IFORM3,IACCE3,IPROT3,ICURS3,
     1  IENDF3,IREWI3,ISUBN0,IERRF3,IBUGA3,ISUBRO,IERROR)
        IF(IERRF3.EQ.'YES')THEN
          WRITE(ICOUT,9231)
 9231     FORMAT('**** ERROR IN DDS TRYING TO CLOSE SPECTRUM.OUT FILE.')
          IERROR='YES'
          GOTO9999
        ENDIF
      ENDIF
C
CCCCC STOP
CCCCC APRIL 1996.  ADD FOLLOWING LINE
 9999 CONTINUE
      RETURN
      END
      SUBROUTINE DETPAR(XDDS,YDDS)
C     ..........DETPAR..........
CCCCC APRIL 1996.  ADD XDDS, YDDS TO THE CALL LIST, PASS NOB THROUGH
CCCCC COMMON
CCCCC SUBROUTINE DETPAR(NOB)
C
C     PURPOSE--SUBTRACTS THE DETERMINISTIC TRENDS FROM THE DATA
C
CCCCC COMMENT OUT FOLLOWING 3 LINES, ADD DDS INCLUDE FILE
CCCCC PARAMETER (MXSER=3,MXNOB=1024,MXPAR=45)
CCCCC COMMON/DDSDAT/ YDDS(MXNOB,MXSER),XDDS(MXNOB,MXSER)
CCCCC COMMON /SEASON/ NPOLY,NEXP,NSIN,ISEAS(30),SEAS(30),DELTA
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODD.INC'
      DIMENSION XDDS(MAXOBV,MXSER)
      DIMENSION YDDS(MAXOBV,MXSER)
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
       DO40 IT=1,NOB
            N=2
            S=SEAS(1)
               DO10I=1,NPOLY
                  S=S+SEAS(N)*(DELTA*IT)**I
                  N=N+1
  10           CONTINUE
               DO20I=1,NEXP
                  S=S+SEAS(N)*EXP(SEAS(N+1)*IT)
                  N=N+2
  20           CONTINUE
               DO30I=1,NSIN
                  S=S+SEAS(N)*EXP(SEAS(N+1)*IT)*
     >              (SEAS(N+2)*SIN(SEAS(N+3)*IT)+SQRT(1-SEAS(N+2)**2)*
     >              COS(SEAS(N+3)*IT))
                 N=N+4
  30           CONTINUE
            XDDS(IT,1)=YDDS(IT,1)-S
  40  CONTINUE
      RETURN
      END
 
C     ..........EAR..........
 
      SUBROUTINE EAR(XTX,NAR2,LAG2,PAR,ISIZ,XDDS,YDDS)
CCCCC APRIL 1996.  ADD XDDS, YDDS ARGUMENTS
CCCCC RENAME LAG AND NAR TO LAG2 AND NAR2 TO AVOID NAME CONFLICT
CCCCC WITH DPCODD.INC COMMON BLOCK
CCCCC SUBROUTINE EAR(XTX,NAR,LAG,PAR,ISIZ)
C
C     PURPOSE--FORMS THE XTX AND XTY MATRICES FOR EAR MODELS
C
CCCCC APRIL 1996.  FOLLOWING DECLARATIONS IN DDS INCLUDE FILE
CCCCC PARAMETER (MXSER=3,MXNOB=1024,MXPAR=45,MXINV=50,MXINV1=MXINV+1)
      DOUBLE PRECISION XTX,SUM
CCCCC COMMON /INDEX/ INDEX,MAXLAG,NOB,NSER,NPHI0,NPHI,NSEAS
CCCCC COMMON /FLAG/ IFMEAN,IFAT,IFSEAS,IFGREEN,IFSPECTR
CCCCC COMMON/DDSDAT/ YDDS(MXNOB,MXSER),XDDS(MXNOB,MXSER)
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODD.INC'
      DIMENSION XDDS(MAXOBV,MXSER)
      DIMENSION YDDS(MAXOBV,MXSER)
      DIMENSION ATEMP(1)
C
CCCCC APRIL 1996.  GLOBALLY RENAME LAG AND NAR TO LAG2 AND NAR2
CCCCC DIMENSION XTX(MXINV,MXINV1),LAG(MXSER),LAGT(MXSER)
CCCCC DIMENSION PAR(MXPAR),NAR(MXSER)
      DIMENSION XTX(MXINV,MXINV1),LAG2(MXSER),LAGT(MXSER)
      DIMENSION PAR(MXPAR),NAR2(MXSER)
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
C
C     SUBTRACT MEAN AND SEASONALITY FOR INITIAL VALUES
C
CCCCC APRIL 1996.  ADD XDDS, YDDS TO ARGUMENT LIST
CCCCC IF(IFSEAS.EQ.3) CALL MODEL(PAR,AT,0)
CCCCC IF(IFSEAS.EQ.3) CALL MODEL(PAR,AT,0,XDDS,YDDS)
      IF(IFSEAS.EQ.3) CALL MODEL(PAR,ATEMP,0,XDDS,YDDS)
      AT=ATEMP(1)
C
C     SET TEMPORARY LAGS
C
      DO10I=1,NSER
CCCCC   LAGT(I)=LAG(I)
        LAGT(I)=LAG2(I)
        IF(LAGT(I).EQ.0) LAGT(I)=1
   10 CONTINUE
C
C     FORM INVARIANT PART OF XTX BY BLOCK COLUMNS
C
      IROW=1
      ICOL=1
      DO30I=1,NSER
C
C     FORM DIAGONAL BLOCK
C
CCCCC APRIL 1996.  ADD MXINV, MXINV1 TO ARGUMENT LIST, RENAME TO
CCCCC AVOID CONFLICT WITH XX COMMON BLOCK
CCCCC   CALL XX(XDDS(2-LAGT(I),I),XTX,NAR(I),IROW,ICOL,MAXLAG,NOB)
        CALL XXZ(XDDS(2-LAGT(I),I),XTX,NAR2(I),IROW,ICOL,MAXLAG,NOB,
     >          MXINV,MXINV1)
C
C     FORM REST OF COLUMN
C
         DO20J=I+1,NSER
CCCCC      IROW=IROW+NAR(J-1)
           IROW=IROW+NAR2(J-1)
CCCCC      CALL XA(XDDS(2-LAGT(I),I),XDDS(2-LAGT(J),J),XTX,NAR(I),
CCCCC APRIL 1996.  ADD ARRAY DIMENSION TO ARGUMENT LIST
CCCCC>             NAR(J),IROW,ICOL,MAXLAG,NOB)
           CALL XA(XDDS(2-LAGT(I),I),XDDS(2-LAGT(J),J),XTX,NAR2(I),
     >             NAR2(J),IROW,ICOL,MAXLAG,NOB,MXINV,MXINV1)
  20     CONTINUE
CCCCC    ICOL=ICOL+NAR(I)
         ICOL=ICOL+NAR2(I)
         IROW=ICOL
  30  CONTINUE
C
C    FORM IMMEDIATE BLOCKS - LOOP DOWN THE ROWS
C
      DO50I=INDEX+1,NSER
CCCCC   IF(LAG(I).EQ.0)THEN
        IF(LAG2(I).EQ.0)THEN
          ICOL=1
C
C     LOOP ACROSS THE BLOCKS
C
          DO40J=1,NSER
C
C     LOOP ON ELEMENTS PER BLOCK
C
CCCCC     DO40K=1,NAR(J)
          DO40K=1,NAR2(J)
             SUM=0.0
             DO35L=MAXLAG+1,NOB
               SUM=SUM-XDDS(L-K-LAGT(J)+1,J)*XDDS(L,I)
  35         CONTINUE
             XTX(IROW,ICOL)=SUM
             ICOL=ICOL+1
  40     CONTINUE
C
C     FORM IMMEDIATE DIAGONAL BLOCK
C
          DO70J=INDEX+1,I
CCCCC       IF(LAG(J).EQ.0)THEN
            IF(LAG2(J).EQ.0)THEN
              SUM=0.0
              DO60K=MAXLAG+1,NOB
                SUM=SUM+XDDS(K,I)*XDDS(K,J)
  60          CONTINUE
              XTX(IROW,ICOL)=SUM
              ICOL=ICOL+1
            ENDIF
70        CONTINUE
          IROW=IROW+1
        ENDIF
50    CONTINUE
C
C     FORM XTY - SECTION CORRESPONDING TO INVARIANT PART
C
      IROW=1
      DO90I=1,NSER
CCCCC DO90J=1,NAR(I)
      DO90J=1,NAR2(I)
        SUM=0.0
        DO80K=MAXLAG+1,NOB
          SUM=SUM+XDDS(K-J-LAGT(I)+1,I)*XDDS(K,INDEX)
  80    CONTINUE
        XTX(IROW,ISIZ+1)=SUM
        IROW=IROW+1
  90  CONTINUE
C
C     IMMEDIATE PART
C
      DO110I=INDEX+1,NSER
CCCCC   IF(LAG(I).EQ.0)THEN
        IF(LAG2(I).EQ.0)THEN
          SUM=0.0
          DO100J=MAXLAG+1,NOB
            SUM=SUM-XDDS(J,I)*XDDS(J,INDEX)
  100     CONTINUE
          XTX(IROW,ISIZ+1)=SUM
          IROW=IROW+1
       ENDIF
  110 CONTINUE
C
C    DEFINE UPPER TRIANGLE OF XTX
C
      DO120I=1,ISIZ-1
      DO120J=I+1,ISIZ
        XTX(I,J)=XTX(J,I)
  120 CONTINUE
      RETURN
      END
 
C     ..........GJR..........
 
      SUBROUTINE GJR (A,N,IW,ISIZ,ISIZ1,IERROR)
CCCCC APRIL 1996.  ADD IERROR FLAG
CCCCC SUBROUTINE GJR (A,N,IW,ISIZ,ISIZ1)
C
C     PURPOSE--DOES A GAUSS-JORDAN REDUCTION
C
CCCCC APRIL 1996.  USE DPCODD.INC
CCCCC PARAMETER (MXINV=50)
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODD.INC'
      CHARACTER*4 IERROR
C
      DOUBLE PRECISION A
      DIMENSION A(ISIZ,ISIZ1),JC(MXINV)
C     IW=:, 1- FIND INVERSE, 4- SOLVE AX=B, 5- BOTH
C     JC IS THE PERMUTATION VECTOR
C     KI IS THE OPTION KEY FOR MATRIX INVERSION
C     L IS THE COLUMN CONTROL FOR AX=B
C     M IS THE COLUMN CONTOL FOR MATRIX INVERSION
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
      K=0
      M=1
      L=N+(IW/4)
      KI=2-MOD(IW,2)
CCCCC GO TO (10,30), KI
      IF(KI.EQ.1)THEN
C     INITIALIZE JC FOR INVERSION
        DO20I=1,N
          JC(I)=I
  20    CONTINUE
      ENDIF
C     SEARCH FOR PIVOT ROW
      DO120I=1,N
CCCCC   GO TO (50,40), KI
        IF(KI.EQ.2) M=I
        IF(I.EQ.N) GO TO 95
        X=-1.
        DO60J=I,N
          IF(X.GT.ABS(A(J,I))) GO TO 60
          X=ABS(A(J,I))
          K=J
60      CONTINUE
        IF(K.EQ.I) GO TO 95
CCCCC   GO TO (70,80), KI
        IF(KI.EQ.1)THEN
            MU=JC(I)
            JC(I)=JC(K)
            JC(K)=MU
        ENDIF
C     INTERCHANGE ROW I AND ROW K
        DO90J=M,L
          X=A(I,J)
          A(I,J)=A(K,J)
          A(K,J)=X
  90    CONTINUE
C
C     TEST FOR SINGULARITY
C
  95    CONTINUE
        IF(ABS(A(I,I)).EQ.0.)THEN
C
C     MATRIX IS SINGULAR
C
CCCCC APRIL 1996.  USE IERROR FLAG RATHER THAN STOP
CCCCC     PRINT*,'***ERROR*** IN GJR. MATRIX IS SINGULAR'
CCCCC     STOP'line 714'
          WRITE(ICOUT,9011)
 9011     FORMAT('***** ERROR--FROM DDS ROUTINE GJR, SINGULAR MATRIX ',
     >           'ENCOUNTERED.')
          IERROR='YES'
          GOTO9999 
        ENDIF
        X=A(I,I)
        A(I,I)=1.
C
C     REDUCTION OF THE I-TH ROW
C
        DO100J=M,L
100       A(I,J)=A(I,J)/X
C
C     REDUCTION OF ALL REMAINING ROWS
C
        DO120K=1,N
          IF(K.EQ.I) GO TO 120
          X=A(K,I)
          A(K,I)=0.
          DO110J=M,L
            A(K,J)=A(K,J)-X*A(I,J)
110       CONTINUE
120     CONTINUE
C
C     AX=B AND DET.(A) ARE NOW COMPUTED
C
CCCCC GO TO (130,180), KI
C
C     PERMUTATION OF THE COLUMNS FOR MATRIX INVERSION
C
      IF(KI.EQ.1)THEN
         DO170J=1,N
            IF(JC(J).EQ.J) GO TO 170
            JJ=J+1
            DO140I=JJ,N
              IF(JC(I).EQ.J) GO TO 150
140         CONTINUE
150         JC(I)=JC(J)
            DO160K=1,N
              X=A(K,I)
              A(K,I)=A(K,J)
              A(K,J)=X
160         CONTINUE
170      CONTINUE
      ENDIF
      JC(1)=N
C
CCCCC APRIL 1996.  ADD FOLLOWING LINE
 9999 CONTINUE
      RETURN
      END
 
C     ..........INVAL..........
 
      SUBROUTINE INVAL(XTX,PAR,SCA,NPAR,XDDS,YDDS,IERROR)
CCCCC SUBROUTINE INVAL(XTX,PAR,SCA,NPAR)
CCCCC APRIL 1996.  ADD TO ARGUMENT LIST
C
C     PURPOSE--FINDS THE INITIAL VALUES OF THE STOCHASTIC PARAMETER
C              USING THE INVERSE FUNCTION METHOD
C
CCCCC APRIL 1996.  USE DDS INCLUDE FILE FOR FOLLOWING.
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODD.INC'
      DIMENSION XDDS(MAXOBV,MXSER)
      DIMENSION YDDS(MAXOBV,MXSER)
      CHARACTER*4 IERROR
CCCCC CHARACTER*4 IFLAG
C
CCCCC PARAMETER (MXSER=3,MXNOB=1024,MXPAR=45,MXINV=50,MXINV1=MXINV+1)
      DOUBLE PRECISION XTX(MXINV,MXINV1)
CCCCC COMMON /INDEX/ INDEX,MAXLAG,NOB,NSER,NPHI0,NPHI,NSEAS
CCCCC COMMON /LAG/ LAG(MXSER,MXSER),NAR(MXSER,MXSER),NMA(MXSER)
CCCCC COMMON /FLAG/ IFMEAN,IFAT,IFSEAS,IFGREEN,IFSPECTR
      DIMENSION PAR(MXPAR),N(MXSER),THETA(MXPAR),SCA(MXSER)
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
C     FIND MAXIMUM AUTOREGRESSIVE ORDER AND INVERSE FUNCTION ORDER
C
      NM=1
      DO10I=1,NSER
         IF(NAR(I,INDEX).GT.NM) NM=NAR(I,INDEX)
  10  CONTINUE
C
C     SET INDEXES
C
      M=NMA(INDEX)
      NP=0
      IF(NAR(INDEX,INDEX).EQ.0.OR.NM.EQ.1.AND.M.EQ.1) NP=1
      NM=MAX(NM,M)+M+NP
      ISIZ=NSER*NM+NPHI0
      NXTY=ISIZ+1
      OFFSET=NSEAS
      IF(IFMEAN.GT.0.AND.IFSEAS.EQ.0) OFFSET=OFFSET+NSER
      NN=OFFSET+NPHI0+NPHI
      MXLAG=MAXLAG
      MAXLAG=1
C
C     SET AR ORDERS AND FIND NEW MAXIMUM LAG
C
      DO20I=1,NSER
         N(I)=NM
         ML=NM+LAG(I,INDEX)-1
         IF(ML.GT.MAXLAG) MAXLAG=ML
  20  CONTINUE
C
C     FIT EAR(NM) MODEL TO GET INVERSE FUNCTION COEFFICIENTS
C
CCCCC APRIL 1996.  ARGUMENT LIST FOR FOLLOWING ROUTINES
CCCCC CALL EAR(XTX,N,LAG(1,INDEX),PAR,ISIZ)
CCCCC CALL GJR(XTX,ISIZ,4,MXINV,MXINV1)
      CALL EAR(XTX,N,LAG(1,INDEX),PAR,ISIZ,XDDS,YDDS)
      CALL GJR(XTX,ISIZ,4,MXINV,MXINV1,IERROR)
      IF(IERROR.EQ.'YES')GOTO9999
C
C     UNSCALE INVERSE FUNCTION COEFFICIENTS
C
      K=1
      DO22I=1,NSER
        IF(I.NE.INDEX)THEN
          SCALE=SCA(INDEX)/SCA(I)
          KK=K
          DO21J=1,N(I)
            XTX(KK,NXTY)=XTX(KK,NXTY)*SCALE
            KK=KK+1
  21      CONTINUE
        ENDIF
        K=K+N(I)
  22  CONTINUE
C
C     UNSCALE ZERO LAG TERMS
C
         DO23I=INDEX+1,NSER
               IF(LAG(I,INDEX).EQ.0)THEN
                  XTX(K,NXTY)=XTX(K,NXTY)*SCA(INDEX)/SCA(I)
                  K=K+1
               ENDIF
23       CONTINUE
C
CCCCC WRITE(ICOUT,25) (XTX(I,NXTY),I=1,ISIZ)
CCC25 FORMAT (//,' THE INVERSE FUNCTION COEFFICIENTS ARE:',
CCCCC>/,9(/,8E14.6))
CCCCC CALL DPWRST('XXX','WRIT')
C
C     FIND INITIAL THETA'S
C
CCCCC APRIL 1996.  RENAME MP1 TO MP1Z TO AVOID COMMON BLOCK CONFLICT
CCCCC MP1=M+1
      MP1Z=M+1
      DO40J=1,M
CCCCC   DO40K=1,MP1
        DO40K=1,MP1Z
          SUM=0.0
          DO30I=0,(NSER-1)*NM,NM
            SUM=SUM+XTX(I+J+K,NXTY)
  30    CONTINUE
        XTX(J,K)=SUM
  40  CONTINUE
CCCCC APRIL 1996.  ADD IERROR TO ARGUMENT LIST
CCCCC CALL GJR(XTX,M,4,MXINV,MXINV1)
      CALL GJR(XTX,M,4,MXINV,MXINV1,IERROR)
      IF(IERROR.EQ.'YES')GOTO9999
C
C     CHECK FOR INVERTIBILITY AND FIX IF UNSTABLE
C
         DO50I=1,M
CCCCC       II=MP1-I
            II=MP1Z-I
CCCCC       THETA(I)=XTX(II,MP1)
            THETA(I)=XTX(II,MP1Z)
  50     CONTINUE
CCCCC    PRINT*,'THETAS BEFORE STBLIZ',(THETA(I),I=1,NMA(INDEX))  @@DIA
         CALL STBLIZ(THETA,M,IERROR)
CCCCC    PRINT*,'THETAS AFTER STBLIZ',(THETA(I),I=1,NMA(INDEX))  @@DIA
C
C     LOAD THETAS INTO PAR
C
         DO60I=1,M
            PAR(NN+I)=THETA(I)
  60     CONTINUE
C
C     FIND INITIAL PHI'S
C
         JPAR=OFFSET+NPHI0+1
         L=OFFSET+1
         DO80I=1,NSER
            IPAR=(I-1)*NM+1
C
C     FIND PHI0'S FOR SERIES I
C
            PHI0=0.0
            IF(I.EQ.INDEX) PHI0=1.0
            IF(LAG(I,INDEX).EQ.0)THEN
               IZHU = NSER*NM+L-OFFSET
               PAR(L)=XTX(IZHU,NXTY)
               PHI0=PAR(L)
              L=L+1
            ENDIF
C
C     FIND REST OF PHI'S FOR SERIES I
C
            DO80J=1,NAR(I,INDEX)
               SUM=0.0
               IF(J.LE.M) SUM=THETA(J)*PHI0
               DO70K=1,MIN(J-1,M)
                   SUM=SUM-THETA(K)*XTX(IPAR-K,NXTY)
  70           CONTINUE
               PAR(JPAR)=XTX(IPAR,NXTY)+SUM
               JPAR=JPAR+1
               IPAR=IPAR+1
  80     CONTINUE
C
C     PRINT INITIAL PARAMETER VALUES
C
CCCCC WRITE(ICOUT,90) (PAR(I),I=1,NPAR)
CCC90 FORMAT (//,' INITIAL PARAMETERS =',9E12.5)
CCCCC CALL DPWRST('XXX','WRIT')
      MAXLAG=MXLAG
CCCCC APRIL 1996.  ADD FOLLOWING LINE
 9999 CONTINUE
      RETURN
      END
 
C     ..........LS..........
 
      SUBROUTINE LS (NPAR,PAR,F,FORMT,IFDIF,SCA,IFPRT,SEXT,NDF,
CCCCC APRIL 1996.  ADD XDDS, YDDS, IERROR TO ARGUMENT LIST
CCCCC1               IORDAR,IORDMA)
     1               IORDAR,IORDMA,XDDS,YDDS,IERROR)
C
C     PURPOSE--DOES A MARQUART-LEVENBERG TYPE NON-LINEAR SEARCH
C
CCCCC APRIL 1996.  USE DDS INCLUDE FILE FOR SOME OF THE FOLLOWING
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODD.INC'
      INCLUDE 'DPCOZ2.INC'
      DIMENSION XDDS(MAXOBV,MXSER) 
      DIMENSION YDDS(MAXOBV,MXSER) 
      CHARACTER*4 IERROR
CCCCC PARAMETER (MXSER=3,MXNOB=1024,MXNOB1=MXNOB+1,MXPAR=45,
CCCCC>MXPAR1=MXPAR+1)
      LOGICAL LG,FLAG
      CHARACTER FORMT*50
      DOUBLE PRECISION PIVOT,CMULT,DENOM,SSRED,TRED
CCCCC DOUBLE PRECISION A(MXPAR1,MXPAR1),PARB(MXPAR),XX,FLR
      DOUBLE PRECISION A(MXPAR1,MXPAR1),PARB(MXPAR),FLR
CCCCC COMMON /INDEX/ INDEX,MAXLAG,NOB,NSER,NPHI0,NPHI,NSEAS
CCCCC COMMON /LAG/ LAG(MXSER,MXSER),NAR(MXSER,MXSER),NMA(MXSER)
CCCCC COMMON /FLAG/ IFMEAN,IFAT,IFSEAS,IFGREEN,IFSPECTR
CCCCC COMMON /SEASON/ NPOLY,NEXP,NSIN,ISEAS(30),SEAS(30),DELTA
CCCCC COMMON /BLOK1/ STEP,CONTOL,ITMAX,IFREF
CCCCC COMMON /JOSHI/ XX
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
      DIMENSION PAR(MXPAR),CHMAX(MXPAR),Z(MXNOB,MXPAR1),F(MXNOB1)
      DIMENSION SS(3),FL(3),FD(4),SD(4),LSTP(MXPAR1),SPDA(MXPAR1),
     >          SCA(MXSER)
CCCCC APRIL 1996.  ADD FOLLOWING EQUIVALENCE
      EQUIVALENCE (G2RBAG(IGAR11),Z(1,1))
      FINF=1.E19
C
CCCCC IF(IFPRT.GT.0) WRITE(ICOUT,10) NPAR
CCCCC IF(IFPRT.GT.0) WRITE(3,10) NPAR
CCC10 FORMAT (//,'   VERSION 4R1 OF LS, 09-03-82, WITH',I4,
CCCCC>' PARAMETERS',/)
CCCCC IF(IFPRT.GT.0) CALL DPWRST('XXX','WRIT')
C
      DEL=-.001
      BNDLW=-FINF
      BNDUP=FINF
      DO20I=1,NPAR
         CHMAX(I)=.2*ABS(PAR(I))
20    CONTINUE
CCCCC APRIL 1996.  SET ERROR FLAG INSTEAD OF STOP.
      DO60I=1,NPAR
CCCCC    IF(PAR(I).EQ.0.0) STOP 'A PARAMETER IS EQUAL TO ZERO'
         IF(PAR(I).EQ.0.0) THEN
           WRITE(ICOUT,12)
  12       FORMAT('******ERROR IN DDS ROUTINE LS--A PARAMETER IS ',
     >            'EQUAL TO ZERO.')
           CALL DPWRST('XXX','WRIT')
           IERROR='YES'
           GOTO9999
         ENDIF
CCCCC    IF(PAR(I).LT.BNDLW.OR.PAR(I).GT.BNDUP)
         IF(PAR(I).LT.BNDLW.OR.PAR(I).GT.BNDUP)THEN
           WRITE(ICOUT,22)
  22       FORMAT('******ERROR IN DDS ROUTINE LS--A PARAMETER IS ',
     >            'OUTSIDE ITS BOUNDS.')
           CALL DPWRST('XXX','WRIT')
           IERROR='YES'
           GOTO9999
         ENDIF
60    CONTINUE
C
C     MAIN ITERATION LOOP
C
      ITNO=1
      NPAR1=NPAR+1
      NFUNC=0
   70 CONTINUE
C
CCC70 IF(IFPRT.GT.0) WRITE(ICOUT,80) ITNO,NFUNC
CCCCC IF(IFPRT.GT.0) WRITE(3,80) ITNO,NFUNC
CCC80 FORMAT (/,' START ITERATION NO.',I3,' NO. OF CALLS TO MODEL',I4)
CCCCC IF(IFPRT.GT.0) CALL DPWRST('XXX','WRIT')
C
      FLAG=.TRUE.
C
C     FIND AT'S OF ORIGINAL PARAMETERS
C
   85 CONTINUE
CCCCC APRIL 1996.  ADD XDDS, YDDS ARGUMENTS
CCCCC CALL MODEL (PAR,F,1)
      CALL MODEL (PAR,F,1,XDDS,YDDS)
      NFUNC=NFUNC+1
C
C     STORE AT'S IN LAST COLUMN OF Z
C
      DO90 IOB=1,NOB
            Z(IOB,NPAR1)=-F(IOB)
   90 CONTINUE
      ITNO=ITNO+1
C
C     FIND DERIVATIVES WITH RESPECT TO EACH PARAMETER
C
      IF(IFDIF.NE.0)THEN
C
C     ANALYTICAL
C
CCCCC APRIL 1996.  ADD XDDS, YDDS ARRAYS, PASS IFMEAN THROUGH COMMON
CCCCC    CALL DIF(PAR,Z,F,IS,IFMEAN)
         CALL DIF(PAR,Z,F,IS,XDDS,YDDS)
      ELSE
C
C     NUMERICAL
C
         DO130 IPAR=1,NPAR
           IF(ABS(PAR(IPAR)).LE.1.E-25)THEN
             WRITE(ICOUT,101) 
 101         FORMAT (' ')
             CALL DPWRST('XXX','WRIT')
             WRITE(ICOUT,100) IPAR
CCCCC        WRITE(3,100) IPAR
100          FORMAT (' THE VALUE OF PAR(',I3,') IS TOO SMALL FOR'
     1       , 'DETERMINING THE DERIVATIVE')
             CALL DPWRST('XXX','WRIT')
CCCCC APRIL 1996.  SET ERROR FLAG RATHER THAN STOP
CCCCC        STOP'line 915'
             IERROR='YES'
             GOTO9999
           ENDIF
           PARD=PAR(IPAR)
           DPAR=ABS(PAR(IPAR)*DEL)
           S1=BNDUP-PARD-DPAR
           S2=PARD-DPAR-BNDLW
           IF(S1.LT.0..AND.S2.GT.S1)THEN
              PAR(IPAR)=AMAX1(PARD-DPAR,BNDLW)
              DENOM=PARD-PAR(I)
CCCCC APRIL 1996.  ADD XDDS, YDDS
CCCCC         CALL MODEL (PAR,F,1)
              CALL MODEL (PAR,F,1,XDDS,YDDS)
              NFUNC=NFUNC+1
              DO110 IOB=1,NOB
                 Z(IOB,IPAR)=-(Z(IOB,NPAR1)+F(IOB))/DENOM
  110         CONTINUE
            ELSE
              PAR(IPAR)=AMIN1(PARD+DPAR,BNDUP)
              DENOM=PAR(IPAR)-PARD
CCCCC APRIL 1996.  ADD XDDS, YDDS
CCCCC         CALL MODEL (PAR,F,1)
              CALL MODEL (PAR,F,1,XDDS,YDDS)
              NFUNC=NFUNC+1
              DO120 IOB=1,NOB
                        Z(IOB,IPAR)=(F(IOB)+Z(IOB,NPAR1))/DENOM
  120         CONTINUE
            ENDIF
            PAR(IPAR)=PARD
130      CONTINUE
         IS=1
      ENDIF
C
C     FORM XTX AND XTY MATRIX
C
      DO150 IPAR=1,NPAR1
         DO150 JPAR=1,IPAR
             XX=0.0
             DO140 IOB=IS,NOB
                 XX=XX+1.0D0*Z(IOB,IPAR)*Z(IOB,JPAR)
  140        CONTINUE
             A(IPAR,JPAR)=XX
             A(JPAR,IPAR)=XX
  150 CONTINUE
CCCCC IF(ITNO.EQ.2.AND.IFPRT.GT.0) WRITE(ICOUT,160) A(NPAR1,NPAR1)
CCCCC IF(ITNO.EQ.2.AND.IFPRT.GT.0) WRITE(3,160) A(NPAR1,NPAR1)
CC160 FORMAT (//,' INITIAL SUM OF SQUARES =',D12.4,/)
CCCCC CALL DPWRST('XXX','WRIT')
C     FIND GAUSS-NEWTON CORRECTIONS (XX'XX)**-1(XX'Y)
      NES=0
      NTRANS=0
      SSB=A(NPAR1,NPAR1)
      DO170I=1,NPAR
        LSTP(I)=0
        PARB(I)=PAR(I)
        SPDA(I)=STEP*A(I,I)
  170 CONTINUE
C
  180 CONTINUE
      SSRED=0
      NPIV=0
      DO190I=1,NPAR
         IF(LSTP(I).NE.0.OR.A(I,I).LE.SPDA(I).OR.ABS(CHMAX(I))
     >       .LT.1./FINF) GO TO 190
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
C
C     DO GAUSS JORDAN MODIFICATION FOR THIS ROW AND COLUMN
C
      PIVOT=A(NPIV,NPIV)
      A(NPIV,NPIV)=1.D0
      DO200J=1,NPAR1
         A(NPIV,J)=A(NPIV,J)/PIVOT
  200 CONTINUE
      DO220I=1,NPAR1
         IF(I.NE.NPIV)THEN
           CMULT=A(I,NPIV)
           DO210J=1,NPAR1
              IF(J.NE.NPIV)A(I,J)=A(I,J)-CMULT*A(NPIV,J)
  210      CONTINUE
           A(I,NPIV)=-A(I,NPIV)/PIVOT
         ENDIF
220   CONTINUE
      GO TO 180
C
  225 CONTINUE
CCCCC APRIL 1996.  SET ERROR FLAG INSTEAD OF STOP
CCCCC IF(NTRANS.LE.0) STOP'NO PARAMETER CHANGES POSSIBLE. INSPECT BOUND
      IF(NTRANS.LE.0) THEN
        WRITE(ICOUT,229)
        CALL DPWRST('XXX','WRIT')
  229   FORMAT('******ERROR IN DDS ROUTINE LS--NO PARAMETER CHANGES ',
     >         'POSSIBLE. INSPECT BOUNDS & CHMAX ARRAY')
        IERROR='YES'
        GOTO9999
      ENDIF
      SSE1=A(NPAR1,NPAR1)
      DO230I=1,NPAR
         IF(LSTP(I).EQ.0)A(I,NPAR1)=0.D0
230   CONTINUE
C
C     FIND MAXIMUM ALLOWABLE LAMBDA
C
      ILAM=0
      FLAM=1
      ILMAX=0
      FLMAX=FINF
      QMAX=FINF
      DO240I=1,NPAR
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
C
CCCCC IF(ILAM.NE.0.AND.IFPRT.GT.0) THEN
CCCCC    WRITE(ICOUT,250)ILAM,FLAM
CCCCC    WRITE(3,250)ILAM,FLAM
CC250 FORMAT (' PARAMETER',I4,' LIMITS THE CORRECTIONS TO ',E12.4,
CCCCC>/,' TIMES THE GAUSS-NEWTON VALUES.')
CCCCC     CALL DPWRST('XXX','WRIT')
CCCCC ENDIF
C
CCCCC IF(FLMAX.LT.1..AND.ILMAX.NE.ILAM.AND.IFPRT.GT.0)THEN
CCCCC    WRITE(ICOUT,250) ILMAX,FLMAX
CCCCC    WRITE(3,250) ILMAX,FLMAX
CCCCC    CALL DPWRST('XXX','WRIT')
CCCCC ENDIF
C
CCCCC APRIL 1996.  SET ERROR FLAG INSTEAD OF STOP
CCCCC IF(FLAM.LT.1./FINF) STOP'NO PARAMETER CHANGES POSSIBLE. INSPECT B
CCCCC>OUNDS & CHMAX ARRAY'
      IF(FLAM.LT.1./FINF) THEN
        WRITE(ICOUT,259)
        CALL DPWRST('XXX','WRIT')
  259   FORMAT('******ERROR IN DDS ROUTINE LS--NO PARAMETER CHANGES ',
     >         'POSSIBLE. INSPECT BOUNDS & CHMAX ARRAY')
        IERROR='YES'
        GOTO9999
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
C
C     ADJUST LAMBDA UNTIL BEST SSQ FOUND FOR THIS ITERATION
C
      DO390 IGRID=1,ITMAX
  260    CONTINUE
         DO270I=1,NPAR
            PAR(I)=PARB(I)+FLT*A(I,NPAR1)
            IF(PAR(I).GT.BNDUP)PAR(I)=BNDUP
            IF(PAR(I).LT.BNDLW)PAR(I)=BNDLW
  270    CONTINUE
C
C     FIND SSQ OF AT'S FOR NEW PARAMETERS
C
CCCCC APRIL 1996.  ADD XDDS, YDDS
CCCCC    CALL MODEL (PAR,F,1)
         CALL MODEL (PAR,F,1,XDDS,YDDS)
         NFUNC=NFUNC+1
         SST=0.
         DO290 IOB=1,NOB
            DF=ABS(F(IOB))
C
C     CHECK FOR UNSTABLE MODEL
C
            IF(DF.GT.1.E15)THEN
              WRITE(ICOUT,281) 
 281          FORMAT (' ')
              CALL DPWRST('XXX','WRIT')
              WRITE(ICOUT,280) IOB,F(IOB)
CCCCC         WRITE(3,280) IOB,F(IOB)
C280          FORMAT (1X,/,' AT(',I3,') =',E10.3,' IS TOO LARGE')
 280          FORMAT (' AT(',I3,') =',E10.3,' IS TOO LARGE')
              CALL DPWRST('XXX','WRIT')
              IF(FLAG)THEN
CCCCC APRIL 1996.  USE DATAPLOT I/O
CCCCC           PRINT*,'UNSTABLE MODEL- FIX AND RESTART ITERATION'
                WRITE(ICOUT,283)
  283           FORMAT('*****FROM DDS ROUTINE LS--UNSTABLE MODEL, FIX',
     >                 ' AND RESTART ITERATION')
                CALL DPWRST('XXX','WRIT')
                FLAG=.FALSE.
                N=NSEAS+NPHI0+NPHI+1
                IF(IFMEAN.GT.0.AND.IFSEAS.EQ.0) N=N+NSER
CCCCC           CALL STBLIZ(PAR(N),NMA(INDEX))
                CALL STBLIZ(PAR(N),NMA(INDEX),IERROR)
                IF(IERROR.EQ.'YES')GOTO9999
                GO TO 85
              ELSE
CCCCC           STOP'UNSTABLE MODEL - SECOND TRY'
                WRITE(ICOUT,287)
  287           FORMAT('*****FROM DDS ROUTINE LS--UNSTABLE MODEL, ',
     >                 'SECOND TRY')
                CALL DPWRST('XXX','WRIT')
              ENDIF
            ENDIF
            SST=SST+DF**2
  290    CONTINUE
         SSR=SST
         LG=LG.AND.SST.GT.SSB
         IF(KEY .EQ. 1)THEN
CCCCC      IF(IFPRT.GT.0) WRITE(ICOUT,310) IGRID,FLR,SSR
CCCCC      IF(IFPRT.GT.0)CALL DPWRST('XXX','WRIT')
           GO TO 410
         ENDIF
C
C     CHECK FOR CONVERGENCE
C
         IF((ABS(FLT-1.).LE.CONTOL.OR.ABS(FLT-FLMAX).LE.CONTOL).AND.
     >      ABS(SST-SSE1).LE.ABS(SSE1)*CONTOL.AND..NOT.LG)THEN
            FLR=FLT
  300       CONTINUE
CC300       IF(IFPRT.GT.0) WRITE(ICOUT,310) IGRID,FLR,SSR
CCCCC       IF(IFPRT.GT.0) WRITE(3,310) IGRID,FLR,SSR
CC310       FORMAT(' SEARCH CONVERGED AFTER',I3,' CYCLES, WITH LAMBDA ='
CCCCC>      ,D13.5,/,' AND SSQ =',E13.5,/)
CCCCC       CALL DPWRST('XXX','WRIT')
            GO TO 410
         ENDIF
         INS=0
         K=0
         DO320I=1,3
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
         DO330I=1,3
            IK=I+K
            FL(I)=FD(IK)
            SS(I)=SD(IK)
  330    CONTINUE
C
C     STORE BEST VALUES OF SSQ AND LAMBDA
C
         IF(SST.LT.SBEST)THEN
            SBEST=SST
            FBEST=FLT
         ENDIF
C
C     FIND NEW LAMBDA
C
         IF(FL(3).LE.FLMAX) GO TO 340
         IF(SS(1).LE.SS(2)) GO TO 350
         FLT=0.1*FL(1)+0.9*FL(2)
         GO TO 390
C
  340    CONTINUE
         DENOM=(FL(3)-FL(1))*(SS(2)-SS(1))+(FL(1)-FL(2))*(SS(3)-SS(1))
         IF(DENOM.LE.-1./FINF.AND.FL(3).LT.FINF) GO TO 370
         SSP=FINF
         IF(SS(1).GT.SS(2)) GO TO 360
C
  350    CONTINUE
         FLT=0.9*FL(1)+0.1*FL(2)
         GO TO 390
C
  360    CONTINUE
         FLT=FLMAX
         IF(FL(3).GE.0.98*FLMAX)FLT=0.1*FL(2)+0.9*FL(3)
         IF(FL(3).LT.0.49*FLMAX)FLT=2.*FL(3)
         GO TO 390
C
  370 CONTINUE
         FOLD=FLR
         FLR=((FL(3)**2-FL(1)**2)*(SS(2)-SS(1))+(FL(1)**2-FL(2)**2)*
     1   (SS(3)-SS(1)))/2./DENOM
         IF(FLR.GE.FLMAX)FLR=FLMAX
         IF(FLR.LE.FL(1))FLR=FL(1)
         SSR=SS(1)+(SS(2)-SS(1))*(FLR-FL(1))*(FLR-FL(3))/(FL(2)-FL(1))
     1   /(FL(2)-FL(3))+(SS(3)-SS(1))*(FLR-FL(1))*(FLR-FL(2))/(FL(3)-
     2   FL(1))/(FL(3)-FL(2))
         IF(ABS(SSR-SSP).GT.ABS(CONTOL*SSP).AND.DABS(FOLD-FLR).GT.
     >   DABS(CONTOL*FLR)) GO TO 380
         IF(SSR.LT.0..OR.FLR.LE.FL(1).OR.FLR.GT.FL(3).OR.LG) GO TO 380
         FLT=FLR
         KEY=1
         GO TO 260
C
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
C
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
C
C     MODIFY PARAMETERS FOR BEST CASE
C
  410 CONTINUE
      DO420I=1,NPAR
          PAR(I)=PARB(I)+A(I,NPAR1)*FLR
          IF(PAR(I).GT.BNDUP)PAR(I)=BNDUP
          IF(PAR(I).LT.BNDLW)PAR(I)=BNDLW
420   CONTINUE
      IF(SSB.LT.SSR.AND.IFPRT.GT.0) WRITE(ICOUT,430) SSR,SSB
CCCCC IF(SSB.LT.SSR.AND.IFPRT.GT.0) WRITE(3,430) SSR,SSB
430   FORMAT (1H ,' CURRENT SSQ',E15.8,' EXCEEDS SSQ',E15.8,
     1' OF PREVIOUS ITERATION')
      IF(SSB.LT.SSR.AND.IFPRT.GT.0) CALL DPWRST('XXX','WRIT')
      IF(ITNO.LE.ITMAX.AND.ABS((SSR-SSB)/CONTOL).GT.SSB.AND.IGRID.GT.1)
     1 GO TO 70
C
      IF(ABS((SSR-SSB)/CONTOL).GT.SSB.AND.IGRID.GT.1)THEN
         WRITE(ICOUT,441) 
 441     FORMAT (' ')
         CALL DPWRST('XXX','WRIT')
         WRITE(ICOUT,441) 
         CALL DPWRST('XXX','WRIT')
         WRITE(ICOUT,440)
 440     FORMAT (' ********CONVERGENCE CRITERION IS NOT SATISFIED',
     1   '********')
         WRITE(ICOUT,442)
 442     FORMAT(
     1    '******** MAXIMUM NUMBER OF ITERATIONS WAS REACHED ********')
         CALL DPWRST('XXX','WRIT')
CCCCC    WRITE(3,440)
C440      FORMAT (//' ********CONVERGENCE CRITERION IS NOT SATISFIED',
CCCCC1   '********'
CCCCC2,/,'******** MAXIMUM NUMBER OF ITERATIONS WAS REACHED ********')
      ENDIF
C
      IF(IFREF.GT.0) RETURN
C
C     FIND CONFIDENCE BOUNDS
C
      NDF=NOB-NES
      SEXT=0
      IF(NDF.GT.0)SEXT=SQRT(SSR/FLOAT(NDF))
C
C     REFORM XTX MATRIX AND INVERT IT
C
      DO460I=1,NPAR
         DO460J=1,I
            XX=0.0
            DO450K=IS,NOB
               XX=XX+Z(K,I)*Z(K,J)
  450       CONTINUE
            A(I,J)=XX
            A(J,I)=XX
  460 CONTINUE
CCCCC APRIL 1996.  ADD IERROR TO ARGUMENT LIST.  USE DATAPLOT I/O
CCCCC CALL GJR (A,NPAR,1,MXPAR1,MXPAR1)
      CALL GJR (A,NPAR,1,MXPAR1,MXPAR1,IERROR)
      IF(IERROR.EQ.'YES')GOTO9999
      DO470I=1,NPAR
            IF(A(I,I).LT.0.0)THEN
               A(I,I)=-A(I,I)
CCCCC          IF(IFPRT.GT.0) PRINT*,'***WARNING***',I,
CCCCC>         ' TH DIAGONAL OF COVARIANCE MATRIX IS NEGATIVE'
               IF(IFPRT.GT.0) THEN
                 WRITE(ICOUT,469)I
 469             FORMAT('*****WARNING FROM DDS ROUTINE LS--',I5,
     >                 'TH DIAGONAL OF COVARIANCE MATRIX IS NEGATIVE.')
               ENDIF
            ENDIF
            A(I,NPAR1)=DSQRT(A(I,I))
            PARB(I)=A(I,NPAR1)*SEXT*1.96
470   CONTINUE
C
C     FIND FINAL AT'S AND PRINT FINAL SSQ
C
CCCCC APRIL 1996.  ADD XDDS AND YDDS
CCCCC CALL MODEL (PAR,F,1)
      CALL MODEL (PAR,F,1,XDDS,YDDS)
      NFUNC=NFUNC+1
      XX=0.0
      DO630I=1,NOB
         XX=XX+F(I)*F(I)
  630 CONTINUE
      F(NOB+1)=SNGL(XX)
C
C     UNSCALE RESIDUAL SSQ FOR EARMA MODELS
C
      IF(NSER.GT.1) XX=XX*SCA(INDEX)**2
C
CCCCC WRITE(ICOUT,640) XX
CCCCC WRITE(3,640) XX
CC640 FORMAT(//,72('*'),///,1X,
CCCCC>  'FINAL RESIDUAL SUM OF SQUARES = ',D12.6,/)
CCCCC CALL DPWRST('XXX','WRIT')
C
CCCCC WRITE(ICOUT,'(4H SSQ,D15.8)') XX
CCCCC CALL DPWRST('XXX','WRIT')
C
C     UNSCALE DETERMINISTIC PARAMETERS
C
      IF(IFSEAS.EQ.1.OR.IFSEAS.EQ.3)THEN
         N=1
         DO471I=1,NEXP
            N=N+2
            PAR(N)=PAR(N)/DELTA
            PARB(N)=PARB(N)/DELTA
  471    CONTINUE
         DO472I=1,NSIN
            N=N+2
            PAR(N)=PAR(N)/DELTA
            PARB(N)=PARB(N)/DELTA
            N=N+2
            PAR(N)=PAR(N)/DELTA/6.2831853
            PARB(N)=PARB(N)/DELTA/6.2831853
  472    CONTINUE
      ENDIF
C
C     UNSCALE EARMA PARAMETERS AND CONFIDENCE BOUNDS
C
      IF(NSER.GT.1)THEN
         N=NSEAS+1
            IF(IFMEAN.GE.1)THEN
               DO476I=1,NSER
                  PAR(N)=PAR(N)*SCA(I)
                  PARB(N)=PARB(N)*SCA(I)
                  N=N+1
  476          CONTINUE
            ENDIF
C
C     UNSCALE ZERO LAG TERMS
C
            DO473I=INDEX+1,NSER
               IF(LAG(I,INDEX).EQ.0)THEN
                  PAR(N)=PAR(N)*SCA(INDEX)/SCA(I)
                  PARB(N)=PARB(N)*SCA(INDEX)/SCA(I)
                  N=N+1
               ENDIF
473         CONTINUE
C
C     SCALE REST OF PHI TERMS
C
              DO475I=1,NSER
                 IF(I.NE.INDEX)THEN
                    NN=N
                    SCALE=SCA(INDEX)/SCA(I)
                    DO474J=1,NAR(I,INDEX)
                       PAR(NN)=PAR(NN)*SCALE
                       PARB(NN)=PARB(NN)*SCALE
                       NN=NN+1
  474               CONTINUE
                 ENDIF
                 N=N+NAR(I,INDEX)
  475         CONTINUE
           ENDIF
C
C     PRINT PARAMETERS AND CONFIDENCE BOUNDS
C
CCCCC WRITE(ICOUT,*) INDEX,(NAR(J,INDEX),J=1,NSER),NMA(INDEX)
CCCCC CALL DPWRST('XXX','WRIT')
C
CCCCC WRITE(ICOUT,480)
CCCCC WRITE(3,480)
CC480 FORMAT (////,' BEST PARAMETER VALUES AND 95% CONFIDENCE LIMITS ',
CCCCC>'ESTIMATED',/,
CCCCC>' BY LINEARIZATION FOR THE INDIVIDUAL PARAMETERS ARE',
CCCCC>' AS FOLLOWS:')
CCCCC CALL DPWRST('XXX','WRIT')
C
      NP=1
C
C     PRINT SEASONALITY PARAMETERS
C
      IF(IFSEAS.EQ.1.OR.IFSEAS.EQ.3)THEN
         J1=(NSEAS+3)/4
         DO490 J2=1,J1
            I1=(J2-1)*4+1
            I2=MIN0(NSEAS,J2*4)
C
CCCCC       WRITE(ICOUT,485) (ISEAS(J),J=I1,I2)
CCCCC       WRITE(3,485) (ISEAS(J),J=I1,I2)
485         FORMAT (/,1X,'I  = ',3X,I3,3(5X,I3))
CCCCC       CALL DPWRST('XXX','WRIT')
C
            WRITE(ICOUT,495) (PAR(J),PARB(J),J=I1,I2)
CCCCC       WRITE(3,495) (PAR(J),PARB(J),J=I1,I2)
495         FORMAT (11X,'BETA(I) = ',4(E12.5,' +/-',E12.5,3X,/))
CCCCC       CALL DPWRST('XXX','WRIT')
  490    CONTINUE
C
         NP=NSEAS+1
      ENDIF
C
C     PRINT MEANS
C
      IF(IFMEAN.GT.0.AND.IFSEAS.EQ.0)THEN
CCCCC    WRITE(ICOUT,500) (I,PAR(I),PARB(I),I=NP,NP+NSER-1)
CCCCC    WRITE(3,500) (I,PAR(I),PARB(I),I=NP,NP+NSER-1)
CC500    FORMAT (/,4X,'SERIES',I3,' MEAN = ',E11.5,' +/-',E10.5)
CCCCC    CALL DPWRST('XXX','WRIT')
C
CCCCC    WRITE(ICOUT,'(4H MNS,2E15.8)') (PAR(I),PARB(I),I=NP,NP+NSER-1)
CCCCC    CALL DPWRST('XXX','WRIT')
C
         NP=NP+NSER
      ENDIF
C
C     PRINT PHI'S
C
      WRITE(ICOUT,999)
  999 FORMAT(1H )
      CALL DPWRST('XXX','WRIT')
C
CCCCC AUGUST, 1995.  MODIFIY FOLLOWING LINE FOR CRAY.
CCCCC CRAY DOESN'T ALLOW FREE FORMAT FOR INTERNAL WRITE.
CCCCC WRITE(ICOUT,*)'             ARMA Modeling'
      WRITE(ICOUT,9081)
 9081 FORMAT('             ARMA Modeling')
      CALL DPWRST('XXX','WRIT')
C
CCCCC THE FOLLOWING LINE MUST BE FIXED--REPLACE IORDAR AND IORDMA
C
      WRITE(ICOUT,505)IORDAR,IORDMA
  505 FORMAT(1X,5X,'AR Order = ',I4,'     MA Order = ',I4)
      CALL DPWRST('XXX','WRIT')
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
C
      WRITE(ICOUT,506)
  506 FORMAT(1X,'                Estimate       95% Conf. Halfwidth')
      CALL DPWRST('XXX','WRIT')
C
      IF(IFSEAS.NE.1)THEN
         L=NP
         NP=NP+NPHI0-1
         DO550I=1,NSER
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
            DO530 J2=1,J1
               I1=(J2-1)*4+1
               I2=MIN0(NAR(I,INDEX),J2*4)
               L1 = I1 + NP
               L2 = I2 + NP
               DO546J=L1,L2
                  WRITE(ICOUT,547) J,PAR(J),PARB(J)
  547             FORMAT(1X,'AR(',I3,')',2X,F15.6,' +-',F15.6)
                  CALL DPWRST('XXX','WRIT')
  546          CONTINUE
  530       CONTINUE
            NP=NP+NAR(I,INDEX)
  550    CONTINUE
C
C     PRINT THETAS
C
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','WRIT')
         J1=(NMA(INDEX)+3)/4
         DO560 J2=1,J1
            I1=(J2-1)*4+1
            I2=MIN0(NMA(INDEX),J2*4)
            L1 = I1 + NP
            L2 = I2 + NP
            DO576J=L1,L2
               JMNP=J-NP
               WRITE(ICOUT,577) JMNP,PAR(J),PARB(J)
  577          FORMAT(1X,'MA(',I3,')',2X,F15.6,' +-',F15.6)
               CALL DPWRST('XXX','WRIT')
  576       CONTINUE
  560    CONTINUE
         NP=NP+NMA(INDEX)
      ENDIF
C
      IF(NSER.GT.1) SEXT=SEXT*SCA(INDEX)
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
C
      WRITE(ICOUT,580)SEXT
  580 FORMAT(1X,5X,'Residual Standard Deviation = ',G15.7)
      CALL DPWRST('XXX','WRIT')
C
C     FIND AND PRINT CORRELATION MATRIX
C
      DO590I=1,NPAR
         DO590J=1,I
            A(I,J)=A(I,J)/A(I,NPAR1)/A(J,NPAR1)
  590 CONTINUE
C
CCCCC IF(IFPRT.GT.0) WRITE(ICOUT,600)
CCCCC IF(IFPRT.GT.0) WRITE(3,600)
CC600 FORMAT (/,' NORMALIZED CORRELATION MATRIX OF PARAMETER',
CCCCC> ' ESTIMATES',/)
CCCCC CALL DPWRST('XXX','WRIT')
C
      J1=(NPAR+9)/10
      DO610 J2=1,J1
         I1=(J2-1)*10+1
         I2=MIN0(NPAR,J2*10)
         DO610I=I1,NPAR
            II=MIN0(I,I2)
  610 CONTINUE
CCCCC IF(IFPRT.GT.0) WRITE(ICOUT,620) (A(I,J),J=I1,II)
CC620 FORMAT (6X,10(F10.7,2X))
CCCCC CALL DPWRST('XXX','WRIT')
C
CCCC APRIL 1996.  ADD FOLLOWING LINE
 9999 CONTINUE
      RETURN
      END
 
C     ..........ATSCOR..........
 
      SUBROUTINE ATSCOR(AT,NOB,INDEX,IFSER,MXNOB1,MXSER)
CCCCC APRIL 1996.  PASS DIMENSION SPECIFIERS AS PART OF ARGUMENT LIST
CCCCC SUBROUTINE ATSCOR(AT,NOB,INDEX,IFSER)
C
C     PURPOSE--CALCULATES CORRELATIONS OF THE RESIDUALS FOR 30 LAGS
C
CCCCC APRIL 1996.  COMMENT OUT FOLLOWING LINE.
CCCCC PARAMETER (MXSER=3,MXSR2=MXSER**2,MXNOB=1024,MXNOB1=MXNOB+1)
      DOUBLE PRECISION SUM,SUM1,SUM2
      DIMENSION AT(MXNOB1,MXSER)
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
      A=2./NOB
C
C     FIND AND PRINT THE AVERAGE OF THE RESIDUALS
C
      AVG=0.0
      DO10I=1,NOB
         AVG=AVG+AT(I,INDEX)
  10  CONTINUE
      AVG=AVG/NOB
C
CCCCC WRITE(ICOUT,20) INDEX,AVG
CCCCC WRITE(3,20) INDEX,AVG
CCC20 FORMAT (////,6X,
CCCCC> 'THE AVERAGE OF THE RESIDUALS FOR SERIES',I5,
CCCCC>'  =',E15.6)
CCCCC CALL DPWRST('XXX','WRIT')
C
C     MEAN SUBTRACT THE AT'S FOR THIS SERIES
C
      DO25I=1,NOB
         AT(I,INDEX)=AT(I,INDEX)-AVG
   25 CONTINUE
C
C     CALCULATE THE CORRELATIONS
C
      DO110I=1,INDEX-1
         IF(IFSER.EQ.0)THEN
C
C     FIND CROSS CORRELATIONS BETWEEN THE SERIES
C
            SSQ=0.0
            Q=SQRT(AT(NOB+1,I)*AT(NOB+1,INDEX))
C
CCCCC       WRITE(ICOUT,30) I,INDEX
CCCCC       WRITE(3,30) I,INDEX
CCC30       FORMAT (//,7X,'CORRELATIONS OF SERIES'
CCCCC>      ,I3,' RESIDUALS ',
CCCCC>      'WITH SERIES',I3,' RESIDUALS',//,1X,'LAG'
CCCCC>      ,1X,'CORRELATION',3X,'UNIFIED',2X,'LAG'
CCCCC>      ,1X,'CORRELATION',3X,'UNIFIED',/,
CCCCC>      2(1X,3('-'),1X,11('-'),3X,7('-'),1X))
CCCCC       CALL DPWRST('XXX','WRIT')
C
             DO55K=0,30
                SUM1=0.0
                SUM2=0.0
                DO40J=K+1,NOB
                   SUM1=SUM1+AT(J,I)*AT(J-K,INDEX)
                   SUM2=SUM2+AT(J-K,I)*AT(J,INDEX)
  40            CONTINUE
                R1=SUM1/Q
                R2=SUM2/Q
                SE=SQRT(FLOAT(NOB-K))
                S1=R1*SE
                S2=R2*SE
                SSQ=SSQ+R1**2+R2**2
C
CCCCC            WRITE(ICOUT,60) K,R1,S1,-K,R2,S2
CCCCC            WRITE(3,60) K,R1,S1,-K,R2,S2
CCC60            FORMAT(1X,I3,1X,F11.6,1X,F11.6,2X,I3,1X,F11.6,1X
CCCCC>                  ,F11.6)
CCCCC            CALL DPWRST('XXX','WRIT')
C
   55        CONTINUE
CCCCC        WRITE(ICOUT,65) SSQ
CCCCC        WRITE(3,65) SSQ
CCC65        FORMAT(//,3X,'THE SUM OF SQUARES OF THE CROSS',
CCCCC>       ' CORRELATIONS =',E12.4)
CCCCC        CALL DPWRST('XXX','WRIT')
C
         ENDIF
C
110   CONTINUE
C
C     FIND THE AUTOCORRELATIONS OF THE INDEX SERIES
C
      SSQ=0.0
      VAR=1./NOB
      Q=AT(NOB+1,INDEX)
C
CCCCC WRITE(ICOUT,70) INDEX
CCC70 FORMAT(//,10X,'AUTO CORRELATIONS OF SERIES',I3,' RESIDUALS'
CCCCC>, //,10X,'LAG',7X,'CORRELATION',13X,'UNIFIED',/)
CCCCC CALL DPWRST('XXX','WRIT')
C
      UAM = 0.
      LAGUAM=0
      LFUAG2=0
      NUAG2 =0
      DO90K=1,30
         SUM=0.0
         DO80J=1,NOB-K
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
C
CCCCC    WRITE(ICOUT,100) K,R,S
CCCCC    WRITE(3,100) K,R,S
CC100    FORMAT (10X,I3,5X,F12.7,10X,F12.7)
CCCCC    CALL DPWRST('XXX','WRIT')
C
   90 CONTINUE
C
CCCCC WRITE(ICOUT,105) SSQ
CCCCC WRITE(3,105) SSQ
CC105 FORMAT(//,9X,'THE SUM OF SQUARES OF THE AUTO',
CCCCC>' CORRELATIONS =',E12.4)
CCCCC CALL DPWRST('XXX','WRIT')
C
CCCCC WRITE(ICOUT,106) UAM,LAGUAM,NUAG2,LFUAG2
CCCCC WRITE(3,106) UAM,LAGUAM,NUAG2,LFUAG2
CC106 FORMAT(/,9X,
CCCCC>' THE MAXIMUM UNIFIED AUTOCORRELATION IS',E15.8,/,9X,
CCCCC>'   OCCURING AT LAG . . . . . . . . . . . ',I3,/,9X,
CCCCC>' THE NO. OF UNI. AUTCOR. WITH :UA: > 2 IS',I3,/,9X,
CCCCC>'   THE FIRST OCCURS AT LAG . . . . . . . ',I3)
CCCCC CALL DPWRST('XXX','WRIT')
C
CCCCC WRITE(ICOUT,'(E15.4,3I5)') UAM,LAGUAM,LFUAG2,NUAG2
CCCCC CALL DPWRST('XXX','WRIT')
C
      RETURN
      END
 
C     ..........ANALYS..........
 
      SUBROUTINE ANALYS(PAR,VAR,IOUNI2,IOUNI3)
CCCCC APRIL 1996.  ADD I/O UNITS TO ARGUMENT LIST, PASS DELTA THROUGH
CCCCC COMMON
CCCCC SUBROUTINE ANALYS(PAR,DELTA,VAR)
C
C     PURPOSE--FINDS THE ROOTS,FREQUENCIES,AND DAMPING, AND THE
C
C     GREEN'S FUNCTION AND IMPULSE RESPONSE COEFFICIENTS OF THE MODEL
CCCCC APRIL 1996.  USE DPCODD.INC FOR FOLLOWING
CCCCC PARAMETER (MXSER=3,MXNOB=1024,MXPAR=45)
CCCCC COMMON /INDEX/ INDEX,MAXLAG,NOB,NSER,NPHI0,NPHI,NSEAS
CCCCC COMMON /LAG/ LAG(MXSER,MXSER),NAR(MXSER,MXSER),NMA(MXSER)
CCCCC COMMON /FLAG/ IFMEAN,IFAT,IFSEAS,IFGREEN,IFSPECTR
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODD.INC'
      COMPLEX ROOT(MXPAR)
      DIMENSION PAR(MXPAR),VAR(MXSER)
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
C     FIND ROOTS OF AUTOREGRESSIVE SIDE
C
      IF(NAR(INDEX,INDEX).EQ.0) RETURN
      N=NPHI0+NSEAS+1
      IF(IFMEAN.GE.1.AND.IFSEAS.EQ.0) N=N+NSER
      DO10I=1,INDEX-1
         N=N+NAR(I,INDEX)
  10  CONTINUE
      NR=NAR(INDEX,INDEX)
CCCCC THE FOLLOWING LINE WAS QUESTIONABLY CHANGED--JJF NOVEMBER 1994
CCCCC CALL EIGEN(PAR(N),NR,ROOT)
      CALL EIGEN(PAR,NR,ROOT)
C
CCCCC WRITE(ICOUT,30)
CCC30 FORMAT (////,37X,20('*'),'MODEL CHARACTERISTICS',20('*'),///)
CCCCC CALL DPWRST('XXX','WRIT')
C
C     FIND EXPLICIT GREEN'S FUNCTION AND IMPULSE RESPONSE
C
      N=NSEAS+1
      IF(IFMEAN.GE.1.AND.IFSEAS.EQ.0) N=N+NSER
      N1=N
      N=N+NPHI0
      M=N+NPHI
      DO63I=1,NSER
C
C     SET ZERO LAG TERMS
C
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
C
C     FIND FUNCTION FOR SERIES I
C
         IF(I.NE.INDEX.AND.NR.GT.NN)THEN
            WRITE(ICOUT,61) INDEX,I
61          FORMAT(44X,'IMPULSE RESPONSE FUNCTION OF SERIES',
     >      I3,' TO SERIES',I3)
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,62)
62          FORMAT(' ')
            CALL DPWRST('XXX','WRIT')
C
            CALL GREENZ(PHI0,PAR(N2),ROOT,NR,NN,LAG(I,INDEX),'IMP(j) =',
CCCCC APRIL 1996.  ADD I/O UNITS TO ARGUMENT LIST, PASS DELTA THROUGH
CCCCC COMMON
CCCCC>                  DELTA,VAR(I))
     >                  VAR(I),IOUNI2,IOUNI3)
         ELSEIF(I.EQ.INDEX.AND.NR.GT.NMA(INDEX))THEN
C
CCCCC       WRITE(ICOUT,62) INDEX
CCC62       FORMAT (44X,'GREEN''S FUNCTION OF SERIES',I3,
CCCCC>      ' WITH ITS RESIDUALS',/)
CCCCC       CALL DPWRST('XXX','WRIT')
C
            CALL GREENZ(1.0,PAR(M),ROOT,NR,NMA(INDEX),0,'  G(j) =',
CCCCC APRIL 1996.  ADD I/O UNITS TO ARGUMENT LIST, PASS DELTA THROUGH
CCCCC COMMON
CCCCC>                 DELTA,VAR(I))
     >                 VAR(I),IOUNI2,IOUNI3)
C
         ELSEIF(I.EQ.INDEX.AND.NR.LE.NMA(INDEX))THEN
CCCCC       WRITE(ICOUT,409) INDEX
CC409       FORMAT(44X,'EXPLICIT FORM OF SERIES',I3,' GREENS',
CCCCC>      ' FUNCTION AND',/,50X,'COMPONENTS OF SPECTRUM',
CCCCC>      ' DONOT EXIST',/)
CCCCC       CALL DPWRST('XXX','WRIT')
C
CCCCC APRIL 1996.  ADD TO ARGUMENT LIST, MAKE NAME 6 CHARACTERS
CCCCC PASS DELTA THROUGH COMMON
CCCCC       CALL SPECTRUM(1.0,PAR(M),PAR(N),NR,NMA(INDEX),0,
CCCCC>                    DELTA)
            CALL SPCTRM(1.0,PAR(M),PAR(N),NR,NMA(INDEX),0,IOUNI3)
C
         ENDIF
         N=N+NAR(I,INDEX)
  63  CONTINUE
C
C     FIND FREQENCIES AND DAMPING OF AUTOREGRESSIVE OPERATOR
C
CCCCC WRITE(ICOUT,64)
CCC64 FORMAT (///,45X,'CHARACTERISTICS OF THE AUTOREGRESSIVE OPERATOR')
CCCCC CALL DPWRST('XXX','WRIT')
C
      CALL FREQ(NR,ROOT,DELTA)
C
C     FIND ROOTS OF MOVING AVERAGE SIDE
C
      NR=NMA(INDEX)
      IF(NR.GT.0)THEN
         N=NSEAS+NPHI0+NPHI+1
         IF(IFMEAN.GE.1.AND.IFSEAS.EQ.0) N=N+NSER
CCCCC THE FOLLOWING LINE WAS QUESTIONABLY CHANGED--JJF NOVEMBER 1994
CCCCC    CALL EIGEN(PAR(N),NR,ROOT)
         CALL EIGEN(PAR,NR,ROOT)
C
C     FIND FREQENCIES AND DAMPING OF MOVING AVERAGE OPERATOR
C
CCCCC    WRITE(ICOUT,80)
CCC80    FORMAT(/////,45X,'CHARACTERISTICS OF THE MOVING',
CCCCC>   ' AVERAGE OPERATOR')
CCCCC    CALL DPWRST('XXX','WRIT')
C
         CALL FREQ(NR,ROOT,DELTA)
      ENDIF
      RETURN
      END
 
C     ..........STBLIZ..........
 
      SUBROUTINE STBLIZ(THETA,N,IERROR)
C
C     PURPOSE--CHECKS FOR STABILITY OF THETA AND FIXES IF NECESSARY
C
      PARAMETER (MXPAR=45,MXPAR1=MXPAR+1)
      COMPLEX ROOT(MXPAR),UNSRT,RPRIME
CCCCC COMPLEX*16 CTHETA(MXPAR),F,FPRIME
      COMPLEX    CTHETA(MXPAR),F,FPRIME
      DIMENSION THETA(MXPAR),NSTABL(MXPAR)
      CHARACTER*4 IERROR
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
C     COMPUTE DISCRETE ROOTS
C
      CALL EIGEN(THETA,N,ROOT(1))
C     PRINT*,'INITIAL NUS',(ROOT(I),I=1,N)  @@DIA
C
C     CHECK FOR UNSTABLE MODEL
C
      ISTABL=0
      EPS=0.0001
      DO20I=1,N
         IF(CABS(ROOT(I)).GT.1.0+EPS)THEN
           ISTABL=ISTABL+1
            NSTABL(ISTABL)=I
         ENDIF
20    CONTINUE
C     PRINT*,'ISTABL=',ISTABL,'NSTABL=',(NSTABL(I),I=1,ISTABL) @@DIA
C
C     STABILIZE ANY UNSTABLE ROOTS - IF NECESSARY
C
      IF(ISTABL.GT.0)THEN
         DO30I=1,N
            CTHETA(I)=CMPLX(THETA(I),0.)
  30     CONTINUE
         DO50I=1,ISTABL
            UNSRT=ROOT(NSTABL(I))
            RPRIME=1./UNSRT
C           PRINT*,'UNSRT=',UNSRT,' RPRIME=',RPRIME  @@DIA
C
C     PARTIAL REVERSE ROUTINE - UPDATE PARAMETERS FOR ONE ALTERED ROOT
C
            F=CTHETA(1)-UNSRT
            CTHETA(1)=F+RPRIME
            DO40J=2,N
               FPRIME=F*RPRIME
               F=CTHETA(J)+F*UNSRT
               CTHETA(J)=F-FPRIME
  40        CONTINUE
  50     CONTINUE
         DO60I=1,N
            THETA(I)=REAL(CTHETA(I))
  60     CONTINUE
      ENDIF
C     PRINT*,'NEW THETAS',(THETA(I),I=1,N)  @@DIA
C     PRINT*,'NEW CTHETAS',(CTHETA(I),I=1,N)  @@DIA
      CALL EIGEN(THETA,N,ROOT(1))
C     PRINT*,'FINAL NUS',(ROOT(I),I=1,N)  @@DIA
      RETURN
      END
 
C     ..........DIF..........
 
      SUBROUTINE DIF(PAR,Z,AT,IS,XDDS,YDDS)
CCCCC APRIL 1996.  ADD XDDS AND YDDS, PASS IFMEAN THROUGH COMMON
CCCCC SUBROUTINE DIF(PAR,Z,AT,IS,IFMEAN)
C
C     PURPOSE--FINDS THE ANALYTICAL DERIVATIVES OF THE
C              RESIDUALS WITH RESPECT TO THE STOCHASTIC PARAMETERS
C
CCCCC APRIL 1996.  USE DPCODD.INC
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODD.INC'
      DIMENSION XDDS(MAXOBV,MXSER)
      DIMENSION YDDS(MAXOBV,MXSER)
CCCCC PARAMETER (MXSER=3,MXNOB=1024,MXNOB1=MXNOB+1,MXPAR=45,
CCCCC>MXPAR1=MXPAR+1)
      DOUBLE PRECISION S
CCCCC COMMON /INDEX/ INDEX,MAXLAG,NOB,NSER,NPHI0,NPHI,NSEAS
CCCCC COMMON /LAG/ LAG(MXSER,MXSER),NAR(MXSER,MXSER),NMA(MXSER)
CCCCC COMMON/DDSDAT/ YDDS(MXNOB,MXSER),XDDS(MXNOB,MXSER)
      DIMENSION PAR(MXPAR),AT(MXNOB1),Z(MXNOB,MXPAR1)
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
      IZ=1
      KPAR=NPHI0+NPHI+1
C
C     FIND DERIVATIVES WITH RESPECT TO MEANS IF ESTIMATED
C
         IF(IFMEAN.GE.1)THEN
C
C     RESTORE ORIGINAL DATA VALUES
C
            DO5I=1,NSER
               DO5J=1,NOB
                  XDDS(J,I)=YDDS(J,I)
   5        CONTINUE 
            KPAR=KPAR+NSER
            IPAR=NSER+1
            JPAR=IPAR+NPHI0
            DO30 IZ=1,NSER
               SUM=0.0
               IF(IZ.EQ.INDEX)SUM=-1.0
               IF(IZ.GT.INDEX.AND.LAG(IZ,INDEX).EQ.0)THEN
                  SUM=-PAR(IPAR)
                  IPAR=IPAR+1
               ENDIF
               DO10J=1,NAR(IZ,INDEX)
                  SUM=SUM+PAR(JPAR)
                  JPAR=JPAR+1
  10           CONTINUE
               DO30 IT=1,NOB
                  S=SUM
                  K=KPAR
                  DO20J=1,MIN(IT-1,NMA(INDEX))
                      S=S+PAR(K)*Z(IT-J,IZ)
                      K=K+1
  20              CONTINUE
                  Z(IT,IZ)=S
  30        CONTINUE
         ENDIF
C
C     FIND DERIVATIVES WITH RESPECT TO PHI0'S
C
         DO60I=1+INDEX,NSER
            IF(LAG(I,INDEX).EQ.0)THEN
               DO50 IT=1,NOB
                  K=KPAR
                  S=XDDS(IT,I)
                  DO40J=1,MIN(IT-1,NMA(INDEX))
                      S=S+PAR(K)*Z(IT-J,IZ)
                      K=K+1
  40              CONTINUE
                  Z(IT,IZ)=S
  50           CONTINUE
               IZ=IZ+1
            ENDIF
60      CONTINUE
C
C     FIND DERIVATIVES WITH RESPECT TO PHI'S - PHI1 FIRST
C
         DO140I=1,NSER
            LAGT=LAG(I,INDEX)
            IF(LAGT.EQ.0)LAGT=1
            N=1
  65        CONTINUE
            DO80 IT=1,LAGT
                S=0.0
                IF(IFMEAN.GE.1)S=PAR(I)
                K=KPAR
                DO70J=1,MIN(IT-1,NMA(INDEX))
                    S=S+PAR(K)*Z(IT-J,IZ)
                      K=K+1
  70            CONTINUE
                 Z(IT,IZ)=S
  80        CONTINUE
            DO100 IT=LAGT+1,NOB
               S=0.0
               IF(IFMEAN.GE.1)S=PAR(I)
               K=KPAR
               DO90J=1,MIN(IT-1,NMA(INDEX))
                   S=S+PAR(K)*Z(IT-J,IZ)
                   K=K+1
  90            CONTINUE
                Z(IT,IZ)=S-XDDS(IT-LAGT,I)
 100        CONTINUE
C
C     DO THE REST OF THE PHI'S FOR THIS SERIES
C
            IZ=IZ+1
            IF(IFMEAN.GE.1)THEN
               N=N+1
               LAGT=LAGT+1
               IF(N.LE.NAR(I,INDEX))GO TO 65
            ELSE
               DO130J=2,NAR(I,INDEX)
                  DO110 IT=I,J
                     Z(IT,IZ)=0.0
  110             CONTINUE
                  DO120 IT=J+1,NOB
                     Z(IT,IZ)=Z(IT-1,IZ-1)
  120             CONTINUE
                  IZ=IZ+1
  130          CONTINUE
            ENDIF
140     CONTINUE
C
C     FIND DERIVATIVES WITH RESPECT TO THETA - THETA1 FIRST
C
         Z(1,IZ)=0.0
         DO160 IT=2,NOB
            S=0.0
            K=KPAR
            DO150J=1,MIN(IT-1,NMA(INDEX))
               S=S+PAR(K)*Z(IT-J,IZ)
               K=K+1
  150       CONTINUE
            Z(IT,IZ)=S+AT(IT-1)
  160   CONTINUE
C
C     DO THE REST OF THE THETA TERMS
C
         DO180I=2,NMA(INDEX)
            IZ=IZ+1
            DO170 IT=1,I
               Z(IT,IZ)=0.0
  170       CONTINUE
            DO180 IT=I+1,NOB
                 Z(IT,IZ)=Z(IT-1,IZ-1)
  180    CONTINUE
      IS=MAXLAG
      RETURN
      END
 
C     ..........XA..........
 
      SUBROUTINE XA(X1,X2,XTX,N,M,IR,IC,MAXLAG,NOB,MXINV,MXINV1)
CCCCC APRIL 1996.  ADD ARRAY DIMENSION TO ARGUMENT LIST
CCCCC SUBROUTINE XA(X1,X2,XTX,N,M,IR,IC,MAXLAG,NOB)
C     PURPOSE--FORMS THE OFF DIAGONAL BLOCKS
C
C     NOTE--IR AND IC ARE THE POSITION OF THE UPPER LEFT ELEMEN
C
CCCCC APRIL 1996.  USE IMPLICIT DIMENSION
CCCCC PARAMETER (MXSER=3,MXNOB=1024,MXPAR=45,MXINV=50,MXINV1=MXINV+1)
      DOUBLE PRECISION XTX,SUM
CCCCC DIMENSION X1(MXNOB),X2(MXNOB),XTX(MXINV,MXINV1)
      DIMENSION X1(*),X2(*),XTX(MXINV,MXINV1)
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
      NOBM1=NOB-1
      NP1=N+1
C
C     FORM UPPER RIGHT HALF
C
C     GET INITIAL WINDOW
C
      DO20I=N-1,1,-1
         SUM=0.0
         DO10J=MAXLAG,NOBM1
            SUM=SUM+X1(J-I)*X2(J)
  10     CONTINUE
         XTX(IR,IC+I)=SUM
C
C     WINDOW REST OF DIAGONAL
C
         DO20J=1,MIN(M-1,N-I-1)
             SUM=SUM+X1(N-J-I)*X2(N-J)-X1(NOB-J-I)*X2(NOB-J)
             XTX(IR+J,IC+I+J)=SUM
  20  CONTINUE
C
C     FORM LOWER LEFT HALF
C
C     GET INITIAL WINDOW
C
      DO40I=0,M-1
         SUM=0.0
         DO30J=MAXLAG,NOBM1
            SUM=SUM+X1(J)*X2(J-I)
  30     CONTINUE
         XTX(IR+I,IC)=SUM
C
C     WINDOW REST OF DIAGONAL
C
         DO40J=1,M-I-1
            SUM=SUM+X1(M-J)*X2(M-J-I)-X1(NOB-J)*X2(NOB-J-I)
            XTX(IR+I+J,IC+J)=SUM
  40     CONTINUE
C
      RETURN
      END
 
C     ..........MODEL..........
 
      SUBROUTINE MODEL(PAR,AT,IFIRST,XDDS,YDDS)
CCCCC APRIL 1996.  ADD XDDS, YDDS TO ARGUMENT LIST
CCCCC SUBROUTINE MODEL(PAR,AT,IFIRST)
C
C     PURPOSE--FINDS THE RESIDUALS (AT'S) OF THE MODEL
C
      DOUBLE PRECISION SUM
CCCCC APRIL 1996.  REPLACE FOLLOWING WITH DPCODD.INC
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODD.INC'
      DIMENSION XDDS(MAXOBV,MXSER)
      DIMENSION YDDS(MAXOBV,MXSER)
CCCCC PARAMETER (MXSER=3,MXNOB=1024,MXNOB1=MXNOB+1,MXPAR=45)
CCCCC COMMON /INDEX/ INDEX,MAXLAG,NOB,NSER,NPHI0,NPHI,NSEAS
CCCCC COMMON /LAG/ LAG(MXSER,MXSER),NAR(MXSER,MXSER),NMA(MXSER)
CCCCC COMMON /FLAG/ IFMEAN,IFAT,IFSEAS,IFGREEN,IFSPECTR
CCCCC COMMON /SEASON/ NPOLY,NEXP,NSIN,ISEAS(30),SEAS(30),DELTA
CCCCC COMMON/DDSDAT/ YDDS(MXNOB,MXSER),XDDS(MXNOB,MXSER)
CCCCC DIMENSION PAR(MXPAR),AT(MXNOB1)
      DIMENSION PAR(*),AT(*)
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
      IPAR=1
C
C     SEASONALITY PARAMETERS
C
      IF(IFSEAS.EQ.1.OR.IFSEAS.EQ.3)THEN
C
C     LOAD SEASONALITY ARRAY
C
         N=1
         DO10I=1,NSEAS
            SEAS(ISEAS(I))=PAR(IPAR)
            IPAR=IPAR+1
10       CONTINUE
C
C     SUBTRACT SEASONALITY FROM DATA
C
CCCCC APRIL 1996.  ADD XDDS, YDDS ARGUMENTS, NOB PASSED THROUGH COMMON
CCCCC    CALL DETPAR(NOB)
         CALL DETPAR(XDDS,YDDS)
         IF(IFIRST.EQ.0) RETURN
C
C     SUBTRACT MEAN IF ESTIMATED
C
      ELSEIF(IFMEAN.GE.1.AND.IFSEAS.EQ.0)THEN
         DO40I=1,NSER
            DO30J=1,NOB
               XDDS(J,I)=YDDS(J,I)-PAR(IPAR)
  30        CONTINUE
            IPAR=IPAR+1
  40     CONTINUE
      ENDIF
      IF(IFSEAS.EQ.1)THEN
         DO20I=1,NOB
            AT(I)=XDDS(I,1)
  20     CONTINUE
         RETURN
      ENDIF
C
C     FIND FIRST MAXLAG AT'S
C
      JPAR=IPAR
      IF(IFAT.EQ.0)THEN
         DO50I=1,MAXLAG
            AT(I)=0.0
  50     CONTINUE
      ELSEIF(IFAT.GT.0)THEN
         DO100 IT=1,MAXLAG
           IPAR=JPAR
           SUM=0.0
C
C     FIND PHI(0) TERMS
C
           DO60I=INDEX+1,NSER
              IF(LAG(I,INDEX).EQ.0)THEN
                 SUM=SUM+PAR(IPAR)*XDDS(IT,I)
                 IPAR=IPAR+1
              ENDIF
60         CONTINUE
C
C     FIND REST OF PHI TERMS
C
           KPAR=IPAR
           DO80I=1,NSER
              LAGT=LAG(I,INDEX)
              IF(LAGT.EQ.0) LAGT=1
              IPAR=KPAR
              DO70J=LAGT,MIN(IT,LAGT+NAR(I,INDEX))-1
                 SUM=SUM-PAR(IPAR)*XDDS(IT-J,I)
                 IPAR=IPAR+1
  70          CONTINUE
              KPAR=KPAR+NAR(I,INDEX)
  80       CONTINUE
C
C     DOTHETA TERMS
C
           DO90I=1,MIN(IT-1,NMA(INDEX))
              SUM=SUM+PAR(KPAR)*AT(IT-I)
              KPAR=KPAR+1
  90       CONTINUE
           AT(IT)=XDDS(IT,INDEX)+SUM
 100     CONTINUE
      ELSE
         NP=IPAR+NPHI0+NPHI+NMA(INDEX)-1
         DO1I=1,MAXLAG
            AT(I)=PAR(NP+I)
 1       CONTINUE
      ENDIF
C
C     FIND REST OF AT'S
C
      DO140 IT=MAXLAG+1,NOB
        IPAR=JPAR
        SUM=0.0
        DO110I=INDEX+1,NSER
           IF(LAG(I,INDEX).EQ.0)THEN
              SUM=SUM+PAR(IPAR)*XDDS(IT,I)
              IPAR=IPAR+1
           ENDIF
110     CONTINUE
        DO120I=1,NSER
           LAGT=LAG(I,INDEX)
           IF(LAGT.EQ.0) LAGT=1
           DO120J=LAGT,LAGT+NAR(I,INDEX)-1
               SUM=SUM-PAR(IPAR)*XDDS(IT-J,I)
               IPAR=IPAR+1
  120   CONTINUE
        DO130I=1,NMA(INDEX)
           SUM=SUM+PAR(IPAR)*AT(IT-I)
           IPAR=IPAR+1
  130   CONTINUE
        AT(IT)=XDDS(IT,INDEX)+SUM
  140 CONTINUE
      MXLAG1 = MAXLAG+1
      RETURN
      END
 
C     ..........XX..........
 
      SUBROUTINE XXZ(XDDS,XTX,N,IR,IC,MAXLAG,NOB,MXINV,MXINV1)
CCCCC APRIL 1996.  ADD MXINV, MXINV1 TO ARGUMENT LIST, RENAME TO
CCCCC AVOID NAME CONFLICT.
CCCCC SUBROUTINE XX(XDDS,XTX,N,IR,IC,MAXLAG,NOB)
C
C     PURPOSE--FORMS THE LOWER TRIANGLE OF THE DIAGONAL BLOCKS
C
C     IR AND IC ARE THE POSITION OF THE UPPER LEFT CORNER
CCCCC APRIL 1996.  USE ARGUMENT LIST TO DIMENSION ARRAYS
CCCCC PARAMETER (MXSER=3,MXNOB=1024,MXPAR=45,MXINV=50,MXINV1=MXINV+1)
      DOUBLE PRECISION XTX,SUM
CCCCC DIMENSION XDDS(MXNOB),XTX(MXINV,MXINV1)
      DIMENSION XDDS(*),XTX(MXINV,MXINV1)
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
      K=NOB+1
      L=N+1
      DO20I=0,N-1
         SUM=0.0
C
C     FORM INITIAL WINDOW
C
         DO10J=MAXLAG,NOB-1
            SUM=SUM+XDDS(J-I)*XDDS(J)
  10     CONTINUE
         XTX(IR+I,IC)=SUM
C
C     WINDOW REST OF DIAGONAL
C
         DO20J=1,N-I-1
            IF(J.EQ.0) GO TO 20
            SUM=SUM+XDDS(N-J)*XDDS(N-J-I)-XDDS(NOB-J)*XDDS(NOB-J-I)
            XTX(IR+I+J,IC+J)=SUM
  20  CONTINUE
      RETURN
      END
 
C     ..........FREQ..........
 
      SUBROUTINE FREQ(NR,ROOT,DELTA)
C
C     PURPOSE--CALCULATES FREQUENCY AND DAMPING FROM THE ROOTS
C
CCCCC THE FOLLOWING LINE WAS FIXED--JJF    NOVEMBER 1994
CCCCC COMPLEX ROOT(1),CROOT
      COMPLEX ROOT(*),CROOT
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
CCCCC WRITE(ICOUT,10)
CCC10 FORMAT (/,46X,'DISCRETE',14X,'NATURAL',/,43X,
CCCCC>'COMPLEX ROOTS',11X,
CCCCC>'FREQUENCY',10X,'DAMPING',/,43X,'REAL',6X,'IMAG',12X,'(HZ)',
CCCCC>14X,'RATIO',/,41X,52('-'))
CCCCC CALL DPWRST('XXX','WRIT')
C
         DO40I=1,NR
C
C     FIND FREQUENCY AND DAMPING FOR 2ND ORDER ROOT
C
            AIROOT=AIMAG(ROOT(I))
               IF(AIROOT.GT.0)THEN
                  CROOT=CLOG(ROOT(I))/DELTA
                  ACROOT=CABS(CROOT)
                  W=ACROOT/6.2831853
                  Z=-REAL(CROOT)/ACROOT
C
CCCCC             WRITE(ICOUT,20) ROOT(I),W,Z
CCC20             FORMAT (40X,F7.4,' +/-',F7.4,2(7X,E11.4))
CCCCC             CALL DPWRST('XXX','WRIT')
C
C     FIND BREAK FREQUENCY FOR REAL ROOTS
C
               ELSEIF(AIROOT.EQ.0)THEN
                  RROOT=REAL(ROOT(I))
                     IF(RROOT.LE.0)THEN
C
C     NEGATIVE ROOT - SET BREAK FREQUENCY AT NYQUIST FREQUENCYDDS(
C
                        W=0.5/DELTA
                     ELSE
C     POSITIVE ROOT - CALCULATE BREAK FREQUENCY
C
                        W=-LOG(RROOT)/(DELTA*6.2831853)
                     ENDIF
C
CCCCC             WRITE(ICOUT,30) REAL(ROOT(I)),W
CCC30             FORMAT (40X,F7.4,18X,E11.4)
CCCCC             CALL DPWRST('XXX','WRIT')
C
               ENDIF
40       CONTINUE
C
      RETURN
      END
 
C     ..........EIGEN..........
 
      SUBROUTINE EIGEN(PAR,N,ROOT)
C
C     PURPOSE--USES A MODIFIED BAIRSTOW METHOD FOR ROOT EXTRACTION
C
CCCCC APRIL 1996.  USE DPCODD.INC
CCCCC PARAMETER (MXPAR=45,MP=6*MXPAR/5,MP1=MP+1,MXPAR1=MXPAR+1)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODD.INC'
CCCCC THE FOLLOWING LINE WAS FIXED--JJF    NOVEMBER 1994
CCCCC REAL PAR(1),A(MXPAR1)
CCCCC REAL PAR(MXPAR),A(MXPAR1)
      REAL PAR(*),A(MXPAR1)
      COMPLEX ROOT(MXPAR)
      DIMENSION H(MP),B(MP1),C(MP1),U(MXPAR),V(MXPAR)
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      REAL CPUMIN,CPUMAX
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      TEMP=0.
      DENOM=1.
      N1=N+1
      A(1)=-1.
      A(N1)=1./PAR(N)
      DO5I=2,N
         II=N1-I
         A(I)=-PAR(II)*A(N1)
    5 CONTINUE
      IREV=1
      NC=N+1
C
C     THIS LOOP STORES THE COEFFICIENTS OF THE ORIGINAL POLYNOMIAL INTO H
C
      DO10I=1,NC
         B(I)=0.
         H(I)=A(I)
   10 CONTINUE
      P=0.
      Q=0.
      R=0.
C
C     THE FOLLOWING SECTION CHECKS TO SEE IF 0 IS A ROOT
C
   30 CONTINUE
      IF(H(1).NE.0)GO TO 40
      NC=NC-1
      V(NC)=0.
      U(NC)=0.
C
C     THIS LOOP STORES THE COEFFICIENTS OF THE DEPRESSED POLYNOMIAL INTO H
C
      DO20I=1,NC
         H(I)=H(I+1)
   20 CONTINUE
      GO TO 30
C
C     GENERATE FIRST OR SECOND ORDER ROOTS
C
   40 CONTINUE
      IF(NC.EQ.1)GO TO 320
      IF(NC.NE.2)GO TO 50
      R=-H(1)/H(2)
      GO TO 200
C
   50 CONTINUE
      IF(NC.NE.3)GO TO 60
      P=H(2)/H(3)
      Q=H(1)/H(3)
      GO TO 240
C
C     REVERSE COEFFICIENT ORDER IF WILL SPEED CONVERGENCE OF P,Q,& R
C
   60 CONTINUE
      IF(DABS(H(NC-1)/H(NC)).GE.DABS(H(2)/H(1)))GO TO 100
C
C     REVERSE COEFFICIENTS AND SET IREV FOR INTERCHANGE
C
      IREV=-IREV
      M=NC/2
      DO70I=1,M
         NL=NC+1-I
         TEMP=H(NL)
         H(NL)=H(I)
         H(I)=TEMP
   70 CONTINUE
C
C     INITIAL GUESS OF P,Q,& R AFTER FIRST FACTOR AND ITS ROOTS FOUND
C
      IF(Q.EQ.0)GO TO 80
      P=P/Q
      Q=1./Q
      GO TO 90
C
   80 CONTINUE
      P=0.
   90 CONTINUE
      IF(R.EQ.0)GO TO 100
      R=1./R
C
C     SET ERROR CRITERION AND FIRST 2 B'S AND C'S
C
  100 CONTINUE
      E=5.E-10
      B(NC)=H(NC)
      C(NC)=H(NC)
      B(NC+1)=0.
      C(NC+1)=0.
      NP=NC-1
C
C     LOOP TO FIND THE LINEAR AND QUADRATIC FACTORS OF THE GIVEN POLYNOMIA
C
  190 CONTINUE
      DO110J=1,1000
C
C     THIS SECTION FINDS THE LINEAR FACTORS
C
         DO120K=1,NP
            I=NC-K
            B(I)=H(I)+R*B(I+1)
            C(I)=B(I)+R*C(I+1)
  120    CONTINUE
C
C     CHECK IF R IS CONSTANT TERM OF FACTOR - ELSE NEW R
C
         IF(DABS(B(1)/H(1)).LE.E)GO TO 200
         IF(C(2).EQ.0.)GO TO 130
         R=R-B(1)/C(2)
         GO TO 140
  130    CONTINUE
         R=R+1
C
C     THIS SECTION FINDS THE QUADRATIC FACTORS
C
  140    CONTINUE
         DO150K=1,NP
            I=NC-K
            B(I)=H(I)-P*B(I+1)-Q*B(I+2)
            C(I)=B(I)-P*C(I+1)-Q*C(I+2)
  150    CONTINUE
C
C     CHECK IF P&Q ARE COEFFICIENTS OF QUADRATIC FACTOR - ELSE NEW P&Q
C
CCCCC    IF(H(2).EQ.0.) IF(DABS(B(2)/H(1))-E)160,160,170
CCCCC    IF(DABS(B(2)/H(2))-E)160,160,170
         IF(H(2).EQ.0.) THEN
           IF(DABS(B(2)/H(1))-E.LE.0.)GOTO160
           GOTO170
         ENDIF
         IF(DABS(B(2)/H(2))-E.LE.0.)GOTO160
         GOTO170
C
  160    CONTINUE
CCCCC    IF(DABS(B(1)/H(1))-E)240,240,170
         IF(DABS(B(1)/H(1))-E.LE.0.0)GOTO240
  170    CONTINUE
         DENOM=C(3)**2-(C(2)-B(2))*C(4)
         IF(DENOM.EQ.0)GO TO 180
         P=P+(B(2)*C(3)-B(1)*C(4))/DENOM
         Q=Q+(-B(2)*(C(2)-B(2))+B(1)*C(3))/DENOM
         GO TO 110
C
  180    CONTINUE
         P=P-2.
         Q=Q*(Q+1.)
  110 CONTINUE
C
C     CHANGE E IF NOT CONVERGED AFTER 1000 ITERATIONS AND TRY AGAIN
C
      E=E*10.
      GO TO 190
C
C     THIS SECTION FINDS THE ROOTS OF THE LINEAR FACTORS
C
  200 CONTINUE
      NC=NC-1
      V(NC)=0.
      IF(IREV.EQ.-1)GO TO 210
      U(NC)=R
      GO TO 220
  210 CONTINUE
      U(NC)=1./R
C
C     STORE DEPRESSED POLYNOMIAL IN H TO FIND NEXT FACTOR
C
  220 CONTINUE
      DO230I=1,NC
         H(I)=B(I+1)
  230 CONTINUE
      GO TO 40
C
C     THIS SECTION FINDS THE ROOTS OF THE QUADRATIC FACTORS
C
  240 CONTINUE
      NC=NC-2
      IF(IREV.EQ.-1)GO TO 250
      QP=Q
      PP=P/2.
      GO TO 260
C
  250 CONTINUE
      QP=1./Q
      PP=P/(Q*2.)
  260 CONTINUE
      DISCRM=PP**2-QP
C
C     DETERMINE IF QUADRATIC ROOT IS REAL OR IMAGINARY
C
      IF(DISCRM.GE.0.)GO TO 270
      U(NC+1)=-PP
      U(NC)=-PP
      V(NC+1)=DSQRT(-DISCRM)
      V(NC)=-V(NC+1)
      GO TO 280
C
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
C
C     STORE DEPRESSED POLYNOMIAL IN H TO FIND NEXT FACTOR
C
  280 CONTINUE
      DO310I=1,NC
         H(I)=B(I+2)
  310 CONTINUE
      GO TO 40
  320 CONTINUE
      DO330I=1,N
         ROOT(I)=CMPLX(SNGL(U(I)),SNGL(V(I)))
  330 CONTINUE
      RETURN
      END
 
C     ..........GREEN..........
 
      SUBROUTINE GREENZ(THT0,THETA,ROOT,N,M,LAG2,TITLE,VAR,
     >                 IOUNI2,IOUNI3)
CCCCC APRIL 1996.  ADD IOUNIT, IOUNI2, RENAME LAG TO LAG2, PASS
CCCCC DELTA THROUGH COMMON
CCCCC SUBROUTINE GREEN(THT0,THETA,ROOT,N,M,LAG,TITLE,DELTA,VAR)
C
C     PURPOSE--FINDS THE SMALL G'S OF THE GREEN'S FUNCTION
C
      CHARACTER*8 TITLE,TTL
CCCCC COMPLEX CROOT
CCCCC COMPLEX ROOT,CNUM,DEN,D(45),G(45)
CCCCC DIMENSION ROOT(1),THETA(1)
CCCCC COMPLEX ROOT(*),THETA(*),CNUM,DEN,D(45),G(45),CROOT
CCCCC COMPLEX ROOT(*),CNUM,DEN,D(45),G(45),CROOT
      COMPLEX ROOT(*),G(45),CROOT
CCCCC COMPLEX*16 CNUM,DEN,D(45)
      COMPLEX    CNUM,DEN,D(45)
      DIMENSION THETA(*)
CCCCC APRIL 1996.  USE DPCODD.INC
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODD.INC'
CCCCC COMMON /INDEX/ INDEX,MAXLAG,NOB,NSER,NPHI0,NPHI,NSEAS
CCCCC COMMON /JOSHI/ XX
CCCCC COMMON /FLAG/ IFMEAN,IFAT,IFSEAS,IFGREEN,IFSPECTR
      COMPLEX COMPSPECTR(45)
      DOUBLE PRECISION TOTGREEN, COMPGREEN(45), TOTSPECTR, RCOMPSPEC(45)
      DOUBLE PRECISION OMEGA, BETA
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
      TTL=TITLE
         DO50I=1,N
               CNUM=THT0*ROOT(I)**(N-1)
                  DO10J=1,M
10                   CNUM=CNUM-THETA(J)*ROOT(I)**(N-1-J)
               DEN=CMPLX(1.0,0.0)
                  DO20J=1,N
20                   IF(I.NE.J) DEN=DEN*(ROOT(I)-ROOT(J))
               G(I)=CNUM/DEN
               IF(AIMAG(ROOT(I)) .GE. 0.0)THEN
C
C     PRINT TERM CORRESPONDING TO THIS G
C
                  IF(AIMAG(ROOT(I)).EQ.0)THEN
C
CCCCC                WRITE(ICOUT,30) TTL,REAL(G(I)),REAL(ROOT(I))
CCC30                FORMAT(5X,A,3X,E11.5,'*(',E11.5,')**J',/)
CCCCC                CALL DPWRST('XXX','WRIT')
C
                  ELSE
                     CAR=CABS(ROOT(I))
C
CCCCC                WRITE(ICOUT,40)
CCCCC>                TTL,2.*CABS(G(I)),CAR,ACOS(REAL(ROOT(I))/CAR)
CCCCC>               /(DELTA*6.2831853),ATAN2(AIMAG(G(I)),REAL(G(I)))
CCCCC                WRITE(3,40)
CCCCC>                TTL,2.*CABS(G),CAR,ACOS(REAL(ROOT(I))/CAR)
CCCCC>               /(DELTA*6.2831853),ATAN2(AIMAG(G),REAL(G))
CCC40                FORMAT (5X,A,3X,E11.5,'*(',E11.5,')**J*',
CCCCC>               'COS{2*PI*',E11.5,'*J+(',E11.5,')}'/)
CCCCC                CALL DPWRST('XXX','WRIT')
C
                  ENDIF
               TTL='       +'
            ENDIF
50       CONTINUE
C
CCCCC WRITE(ICOUT,60) LAG2
CCC60 FORMAT(45X,'FOR j GREATER THAN OR EQUAL TO',I3,///)
CCC     ***************** ICOUNT IS FOR COUNTING THE NUMBER OF MODES ***
CCCCC CALL DPWRST('XXX','WRIT')
C
      J=0
      ICOUNT=0
      TOTGREEN=1.
      DO701I=1,N
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
CCCCC APRIL 1996.  USE IOUNI3
CCCCC WRITE(3,703) DELTA*J,TOTGREEN,(COMPGREEN(K),K=1,ICOUNT)
      IF(ICOUNT.GE.1.AND.ICOUNT.LE.45)THEN
      WRITE(IOUNI2,703) DELTA*J,TOTGREEN,(COMPGREEN(K),K=1,ICOUNT)
703   FORMAT(F8.4,2X,F11.4,45(2X,F11.4))
      ELSE
      WRITE(IOUNI2,1703) DELTA*J,TOTGREEN
1703  FORMAT(F8.4,2X,F11.4)
      ENDIF
C
      DO802J=1,100
      ICOUNT=0
      TOTGREEN=0.
      DO801I=1,N
               IF(AIMAG(ROOT(I)) .GE. 0.0)THEN
                  IF(AIMAG(ROOT(I)).EQ.0)THEN
C
CCC     *********EXPONENTIAL MODE ************************
C
                     ICOUNT=ICOUNT+1
                     COMPGREEN(ICOUNT) = G(I)*ROOT(I)**J
                   ELSE
C
CCC     ********** SINUSOIDAL MODE ****************************
C
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
CCCCC APRIL 1996.  USE IOUNIT
        IF(IFGREEN .EQ. 1)THEN
CCCCC      WRITE(3,803) DELTA*J,TOTGREEN,(COMPGREEN(K),K=1,ICOUNT)
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
      DO70I=1,N
        IF(CABS(ROOT(I)) .GT. 1.0+EPS)THEN
            IFLAG = IFLAG+1
        ENDIF
C
CCCCC CHANGED HERE
CCCCC WRITE(ICOUT,*)I,ROOT(I),IFLAG
CCCCC CALL DPWRST('XXX','WRIT')
C
70    CONTINUE
C
      WRITE(ICOUT,999)
  999 FORMAT(1H )
      CALL DPWRST('XXX','WRIT')
C
      WRITE(ICOUT,81)
   81 FORMAT(1H ,
     1'Index  Roots    Roots   Natural  Damping   Green    Green',
     1'   Var.')
      CALL DPWRST('XXX','WRIT')
C
      WRITE(ICOUT,82)
   82 FORMAT(1H ,
     1'       Real     Imag     Freq.    Ratio    Real     Imag ',
     1'   D(I)')
      CALL DPWRST('XXX','WRIT')
C
      WRITE(ICOUT,84)
   84 FORMAT(1H ,
     1'---------------------------------------------------------',
     1'---------')
      CALL DPWRST('XXX','WRIT')
C
CCCCC CHANGED HERE
CCCCC WRITE(ICOUT,*)IFLAG
CCCCC CALL DPWRST('XXX','WRIT')
CCCCC PAUSE
C
      IF(IFLAG.LE.0)THEN
         Z=0.0
         DO80I=1,N
            D(I)=CMPLX(0.0,0.0)
            EPS=0.0001
C
            DO90J=1,N
               DENOM=1.0-(ROOT(I)*ROOT(J))
CCCCC          PATCH--THE FOLLOWING 2 LINES MUST BE FIXED
               IF(ABS(DENOM).LE.EPS)DENOM=EPS
               D(I) = D(I)+G(I)*G(J)/DENOM
   90       CONTINUE
C
CCCCC       D(I)=D(I)*XX/(NOB-N-M-1)    RES. VAR INSERTED LATER
            RESVAR=XX/(NOB-N-M-1)
C
   80    CONTINUE
      ENDIF
C
C     FIND FREQUENCY AND DAMPING FOR 2ND ORDER ROOT
C
      DO85I=1,N
         AIROOT=AIMAG(ROOT(I))
         ZD=(-999.0)
         IF(AIROOT.NE.0)THEN
            CROOT=CLOG(ROOT(I))/DELTA
            ACROOT=CABS(CROOT)
            W=ACROOT/6.2831853
            ZD=-REAL(CROOT)/ACROOT
C
C        FIND BREAK FREQUENCY FOR REAL ROOTS.
C        IF NEGATIVE ROOT - SET BREAK FREQUENCY AT NYQUIST FREQ
C        IF POSITIVE ROOT -CALCULATE BREAK FREQUENCY
C
         ELSEIF(AIROOT.EQ.0)THEN
            RROOT=REAL(ROOT(I))
            IF(RROOT.LE.0)THEN
               W=0.5/DELTA
            ELSE
               W=-LOG(RROOT)/(DELTA*6.2831853)
            ENDIF
         ENDIF
C
         IF(IFLAG.LE.0.AND.AIMAG(ROOT(I)).NE.0)THEN
            WRITE(ICOUT,101)I,REAL(ROOT(I)),AIMAG(ROOT(I)),W,ZD,
     1      REAL(G(I)),AIMAG(G(I)),REAL(D(I))
  101       FORMAT(1H ,I2,2X,6F9.3,E10.3)
            CALL DPWRST('XXX','WRIT')
C
         ELSEIF(IFLAG.GT.0.AND.AIMAG(ROOT(I)).NE.0)THEN
            WRITE(ICOUT,102)I,REAL(ROOT(I)),AIMAG(ROOT(I)),W,ZD,
     1      REAL(G(I)),AIMAG(G(I))
  102       FORMAT(1H ,I2,6F9.3,'  Infin.')
            CALL DPWRST('XXX','WRIT')
C
         ELSEIF(IFLAG.LE.0.AND.AIMAG(ROOT(I)).EQ.0)THEN
            WRITE(ICOUT,103)I,REAL(ROOT(I)),AIMAG(ROOT(I)),W,
     1      REAL(G(I)),AIMAG(G(I)),REAL(D(I))
  103       FORMAT(1H ,I2,2X,3F9.3,'  Undef. ',2F9.3,E10.3)
            CALL DPWRST('XXX','WRIT')
C
         ELSEIF(IFLAG.GT.0.AND.AIMAG(ROOT(I)).EQ.0)THEN
            WRITE(ICOUT,104)I,REAL(ROOT(I)),AIMAG(ROOT(I)),W,
     1      REAL(G(I)),AIMAG(G(I))
  104       FORMAT(1H ,I2,2X,3F9.3,'  Undef. ',2F9.3,'  Infin.')
            CALL DPWRST('XXX','WRIT')
C
         ENDIF
C
         Z = Z+REAL(D(I))
   85 CONTINUE
C
      IF(IFLAG.GT.0)THEN
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','WRIT')
CCCCC AUGUST, 1995.  MODIFIY FOLLOWING LINE FOR CRAY.
CCCCC CRAY DOESN'T ALLOW FREE FORMAT FOR INTERNAL WRITE.
CCCCC    WRITE(ICOUT,*)'Caution--Root > 1 ==> Non-Stationary'
         WRITE(ICOUT,9085)
 9085    FORMAT('Caution--Root > 1 ==> Non-Stationary')
         CALL DPWRST('XXX','WRIT')
      ENDIF
C
CCCCC STEP XX--CALCULATE SPECTRA
C
      DO805J=1,500
C
      ICOUNT=0
 
      TOTSPECTR=0.
      DEN = CEXP(CMPLX(0.0,-2.0*3.141593*0.001*J))
      DO806I=1,N
               IF(AIMAG(ROOT(I)) .GE. 0.0)THEN
                  IF(AIMAG(ROOT(I)).EQ.0)THEN
C
CCC     *********EXPONENTIAL MODE ************************
C
                     ICOUNT=ICOUNT+1
                     COMPSPECTR(ICOUNT) = 
     >               2.*DELTA*D(I)*(1.-ROOT(I)*ROOT(I))
     >               /((1.-ROOT(I)*DEN)*(1.-(ROOT(I)/DEN)))
                   ELSE
C
CCC     ********** SINUSOIDAL MODE ****************************
C
                     ICOUNT=ICOUNT+1
                     COMPSPECTR(ICOUNT) = 
     >               4.*DELTA*D(I)*(1.-ROOT(I)*ROOT(I))
     >               /((1.-ROOT(I)*DEN)*(1.-(ROOT(I)/DEN)))
C
CCCCC CHANGE HERE TWICE
CCCCC WRITE(ICOUT,*) J,I,ICOUNT,D(I),ROOT(I)
CCCCC CALL DPWRST('XXX','WRIT')
C
CCCCC WRITE(ICOUT,*)DEN,DELTA,G(I),THETA(I)
CCCCC CALL DPWRST('XXX','WRIT')
C
CCCCC WRITE(ICOUT,*)COMPSPECTR(ICOUNT)
CCCCC CALL DPWRST('XXX','WRIT')
C
                  ENDIF
                  COMPSPECTR(ICOUNT)=COMPSPECTR(ICOUNT)*RESVAR
                  RCOMPSPEC(ICOUNT)=REAL(COMPSPECTR(ICOUNT))
C
CCCCC PATCH--THE FOLLOWING LINE MUST BE FIXED
C
                  RCOMPSPEC(ICOUNT)=ABS(RCOMPSPEC(ICOUNT))
                  TOTSPECTR=TOTSPECTR+RCOMPSPEC(ICOUNT)
C
CCCCC CHANGED HERE
CCCCC WRITE(ICOUT,*)RCOMPSPEC(ICOUNT),TOTSPECTR
CCCCC CALL DPWRST('XXX','WRIT')
C
              ENDIF
806   CONTINUE
      IF(IFSPECTR .EQ. 1)THEN
C
CCCCC APRIL 1996.  USE IOUNI2
CCCCC WRITE(4,808) J*0.001/DELTA,TOTSPECTR,(RCOMPSPEC(K),K=1,ICOUNT)
      WRITE(IOUNI3,808) 
     >  J*0.001/DELTA,TOTSPECTR,(RCOMPSPEC(K),K=1,ICOUNT)
  808 FORMAT(E11.4,2X,E11.4,45(2X,E11.4))
C
      ENDIF
805   CONTINUE
 
      RETURN
      END
 
C     ..........SPECTRUM..........
 
      SUBROUTINE SPCTRM(THT0,THETA,PHI,N,M,LAG2,IOUNIT)
CCCCC APRIL 1996.  ADD TO ARGUMENT LIST, MAKE NAME 6 CHARACTERS
CCCCC RENAME LAG TO LAG2, PASS DELTA THROUGH COMMON
CCCCC SUBROUTINE SPECTRUM(THT0,THETA,PHI,N,M,LAG,DELTA)
C
C      PURPOSE--CALCULATES THE SPECTRUM WHEN THE NUMBER OF
C               MOVING AVERAGE PARAMETERS IS MORE THAN OR EQUAL TO AR
C
CCCCC APRIL 1996.  USE DPCODD.INC
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCODD.INC'
CCCCC DOUBLE PRECISION XX
CCCCC COMMON /INDEX/ INDEX,MAXLAG,NOB,NSER,NPHI0,NPHI,NSEAS
CCCCC COMMON /JOSHI/ XX
CCCCC DIMENSION PHI(1),THETA(1)
      DIMENSION PHI(*),THETA(*)
      DOUBLE PRECISION TOTSPECTR
CCCCC COMMON /FLAG/ IFMEAN,IFAT,IFSEAS,IFGREEN,IFSPECTR
      COMPLEX CNUM,DEN
 
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
      WRITE(*,*) 'N=',N
      DO405I=1,N
      WRITE(*,*) 'SPEC-PHI',I,'=',PHI(I)
405   CONTINUE
      WRITE(*,*) 'M=',M
      DO408I=1,M
      WRITE(*,*) 'SPEC-THETA',I,'=',THETA(I)
408   CONTINUE
      WRITE(*,*) '****RSS***=', XX
      DO400J=1,500
      TOTSPECTR=2.*DELTA*XX/(NOB-N-M-1)
      DEN=CEXP(CMPLX(0.,0.001*2.*3.14*J*N))
      CNUM=CEXP(CMPLX(0.,0.001*2.*3.14*J*M))
      DO401I=1,N
        DEN=DEN-(PHI(I)*CEXP(CMPLX(0.,0.001*2.*3.14*J*(N-I))))
401   CONTINUE
      DO402I=1,M
        CNUM=CNUM-(THETA(I)*CEXP(CMPLX(0.,0.001*2.*3.14*J*(M-I))))
402   CONTINUE
      TOTSPECTR=TOTSPECTR*(CABS(CNUM))**2
      TOTSPECTR=TOTSPECTR/((CABS(DEN))**2)
CCCCC APRIL 1996.  USE UNIT NUMBER INSTEAD OF HARD-CODING
      IF(IFSPECTR .EQ. 1)THEN
CCCCC    WRITE(4,403) J*0.001/DELTA,TOTSPECTR
         WRITE(IOUNIT,403) J*0.001/DELTA,TOTSPECTR
403      FORMAT(E11.4,2X,E11.4)
      ENDIF
400   CONTINUE
C
      RETURN
      END

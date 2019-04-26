      SUBROUTINE DQAG(F,A,B,EPSABS,EPSREL,KEY,RESULT,ABSERR,NEVAL,IER,
     1   LIMIT,LENW,LAST,IWORK,WORK)
C
C   ADDED TO DATAPLOT 12/2003.  THIS ROUTINE ADDED FOR INTERNAL
C   DATAPLOT USAGE.
C
C***BEGIN PROLOGUE  DQAG
C***DATE WRITTEN   800101   (YYMMDD)
C***REVISION DATE  830518   (YYMMDD)
C***CATEGORY NO.  H2A1A1
C***KEYWORDS  AUTOMATIC INTEGRATOR,GAUSS-KRONROD,GENERAL-PURPOSE,
C             GLOBALLY ADAPTIVE,INTEGRAND EXAMINATOR
C***AUTHOR  PIESSENS, ROBERT, APPLIED MATH. AND PROGR. DIV. -
C             K. U. LEUVEN
C           DE DONCKER, ELISE, APPLIED MATH. AND PROGR. DIV. -
C             K. U. LEUVEN
C***PURPOSE  The routine calculates an approximation result to a given
C            definite integral I = integral of F over (A,B),
C            hopefully satisfying following claim for accuracy
C            ABS(I-RESULT)LE.MAX(EPSABS,EPSREL*ABS(I)).
C***DESCRIPTION
C
C        Computation of a definite integral
C        Standard fortran subroutine
C        Double precision version
C
C            F      - Double precision
C                     Function subprogam defining the integrand
C                     Function F(X). The actual name for F needs to be
C                     Declared E X T E R N A L in the driver program.
C
C            A      - Double precision
C                     Lower limit of integration
C
C            B      - Double precision
C                     Upper limit of integration
C
C            EPSABS - Double precision
C                     Absolute accoracy requested
C            EPSREL - Double precision
C                     Relative accuracy requested
C                     If  EPSABS.LE.0
C                     And EPSREL.LT.MAX(50*REL.MACH.ACC.,0.5D-28),
C                     The routine will end with IER = 6.
C
C            KEY    - Integer
C                     Key for choice of local integration rule
C                     A GAUSS-KRONROD PAIR is used with
C                       7 - 15 POINTS If KEY.LT.2,
C                      10 - 21 POINTS If KEY = 2,
C                      15 - 31 POINTS If KEY = 3,
C                      20 - 41 POINTS If KEY = 4,
C                      25 - 51 POINTS If KEY = 5,
C                      30 - 61 POINTS If KEY.GT.5.
C
C         ON RETURN
C            RESULT - Double precision
C                     Approximation to the integral
C
C            ABSERR - Double precision
C                     Estimate of the modulus of the absolute error,
C                     Which should EQUAL or EXCEED ABS(I-RESULT)
C
C            NEVAL  - Integer
C                     Number of integrand evaluations
C
C            IER    - Integer
C                     IER = 0 Normal and reliable termination of the
C                             routine. It is assumed that the requested
C                             accuracy has been achieved.
C                     IER.GT.0 Abnormal termination of the routine
C                             The estimates for RESULT and ERROR are
C                             Less reliable. It is assumed that the
C                             requested accuracy has not been achieved.
C                      ERROR MESSAGES
C                     IER = 1 Maximum number of subdivisions allowed
C                             has been achieved. One can allow more
C                             subdivisions by increasing the value of
C                             LIMIT (and taking the according dimension
C                             adjustments into account). HOWEVER, If
C                             this yield no improvement it is advised
C                             to analyze the integrand in order to
C                             determine the integration difficulaties.
C                             If the position of a local difficulty can
C                             be determined (I.E.SINGULARITY,
C                             DISCONTINUITY WITHIN THE INTERVAL) One
C                             will probably gain from splitting up the
C                             interval at this point and calling the
C                             INTEGRATOR on the SUBRANGES. If possible,
C                             AN APPROPRIATE SPECIAL-PURPOSE INTEGRATOR
C                             should be used which is designed for
C                             handling the type of difficulty involved.
C                         = 2 The occurrence of roundoff error is
C                             detected, which prevents the requested
C                             tolerance from being achieved.
C                         = 3 Extremely bad integrand behaviour occurs
C                             at some points of the integration
C                             interval.
C                         = 6 The input is invalid, because
C                             (EPSABS.LE.0 AND
C                              EPSREL.LT.MAX(50*REL.MACH.ACC.,0.5D-28))
C                             OR LIMIT.LT.1 OR LENW.LT.LIMIT*4.
C                             RESULT, ABSERR, NEVAL, LAST are set
C                             to zero.
C                             EXCEPT when LENW is invalid, IWORK(1),
C                             WORK(LIMIT*2+1) and WORK(LIMIT*3+1) are
C                             set to zero, WORK(1) is set to A and
C                             WORK(LIMIT+1) to B.
C
C         DIMENSIONING PARAMETERS
C            LIMIT - Integer
C                    Dimensioning parameter for IWORK
C                    Limit determines the maximum number of subintervals
C                    in the partition of the given integration interval
C                    (A,B), LIMIT.GE.1.
C                    If LIMIT.LT.1, the routine will end with IER = 6.
C
C            LENW  - Integer
C                    Dimensioning parameter for work
C                    LENW must be at least LIMIT*4.
C                    IF LENW.LT.LIMIT*4, the routine will end with
C                    IER = 6.
C
C            LAST  - Integer
C                    On return, LAST equals the number of subintervals
C                    produced in the subdiviosion process, which
C                    determines the number of significant elements
C                    actually in the WORK ARRAYS.
C
C         WORK ARRAYS
C            IWORK - Integer
C                    Vector of dimension at least limit, the first K
C                    elements of which contain pointers to the error
C                    estimates over the subintervals, such that
C                    WORK(LIMIT*3+IWORK(1)),... , WORK(LIMIT*3+IWORK(K))
C                    form a decreasing sequence with K = LAST If
C                    LAST.LE.(LIMIT/2+2), and K = LIMIT+1-LAST otherwise
C
C            WORK  - Double precision
C                    Vector of dimension at least LENW
C                    on return
C                    WORK(1), ..., WORK(LAST) contain the left end
C                    points of the subintervals in the partition of
C                     (A,B),
C                    WORK(LIMIT+1), ..., WORK(LIMIT+LAST) contain the
C                     right end points,
C                    WORK(LIMIT*2+1), ..., WORK(LIMIT*2+LAST) contain
C                     the integral approximations over the subintervals,
C                    WORK(LIMIT*3+1), ..., WORK(LIMIT*3+LAST) contain
C                     the error estimates.
C***REFERENCES  (NONE)
C***ROUTINES CALLED  DQAGE,XERROR
C***END PROLOGUE  DQAG
C
      INCLUDE 'DPCOMC.INC'
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      DOUBLE PRECISION A,ABSERR,B,EPSABS,EPSREL,F,RESULT,WORK
      INTEGER IER,IWORK,KEY,LAST,LENW,LIMIT,LVL,L1,L2,L3,NEVAL
C
      DIMENSION IWORK(LIMIT),WORK(LENW)
C
      EXTERNAL F
C
C         CHECK VALIDITY OF LENW.
C
C***FIRST EXECUTABLE STATEMENT  DQAG
      IER = 6
      NEVAL = 0
      LAST = 0
      RESULT = 0.0D+00
      ABSERR = 0.0D+00
      IF(LIMIT.LT.1.OR.LENW.LT.LIMIT*4) GO TO 10
C
C         PREPARE CALL FOR DQAGE.
C
      L1 = LIMIT+1
      L2 = LIMIT+L1
      L3 = LIMIT+L2
C
      CALL DQAGE(F,A,B,EPSABS,EPSREL,KEY,LIMIT,RESULT,ABSERR,NEVAL,
     1  IER,WORK(1),WORK(L1),WORK(L2),WORK(L3),IWORK,LAST)
C
C         CALL ERROR HANDLER IF NECESSARY.
C
      LVL = 0
10    IF(IER.EQ.6) LVL = 1
      IF(IER.NE.0) THEN
CCCCC   CALL XERROR( 'ABNORMAL RETURN FROM DQAG ',26,IER,LVL)
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,901)
  901   FORMAT('***** ERROR--ABNORMAL RETURN FROM DQAG INTEGRATION ',
     1         'ROUTINE.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      RETURN
      END
      SUBROUTINE DQAGE(F,A,B,EPSABS,EPSREL,KEY,LIMIT,RESULT,ABSERR,
     1   NEVAL,IER,ALIST,BLIST,RLIST,ELIST,IORD,LAST)
C
C   ADDED TO DATAPLOT 12/2003.  THIS ROUTINE ADDED FOR INTERNAL
C   DATAPLOT USAGE.
C
C***BEGIN PROLOGUE  DQAGE
C***DATE WRITTEN   800101   (YYMMDD)
C***REVISION DATE  830518   (YYMMDD)
C***REVISION HISTORY (YYMMDD)
C   000601   Changed DMAX1/DABS to generic MAX/ABS
C***CATEGORY NO.  H2A1A1
C***KEYWORDS  AUTOMATIC INTEGRATOR,GAUSS-KRONROD,GENERAL-PURPOSE,
C             GLOBALLY ADAPTIVE,INTEGRAND EXAMINATOR
C***AUTHOR  PIESSENS, ROBERT, APPLIED MATH. AND PROGR. DIV. -
C             K. U. LEUVEN
C           DE DONCKER, ELISE, APPLIED MATH. AND PROGR. DIV. -
C             K. U. LEUVEN
C***PURPOSE  The routine calculates an approximation result to a given
C            definite integral   I = Integral of F over (A,B),
C            hopefully satisfying following claim for accuracy
C            ABS(I-RESLT).LE.MAX(EPSABS,EPSREL*ABS(I)).
C***DESCRIPTION
C
C        Computation of a definite integral
C        Standard fortran subroutine
C        Double precision version
C
C        PARAMETERS
C         ON ENTRY
C            F      - Double precision
C                     Function subprogram defining the integrand
C                     function F(X). The actual name for F needs to be
C                     declared E X T E R N A L in the driver program.
C
C            A      - Double precision
C                     Lower limit of integration
C
C            B      - Double precision
C                     Upper limit of integration
C
C            EPSABS - Double precision
C                     Absolute accuracy requested
C            EPSREL - Double precision
C                     Relative accuracy requested
C                     If  EPSABS.LE.0
C                     and EPSREL.LT.MAX(50*REL.MACH.ACC.,0.5D-28),
C                     the routine will end with IER = 6.
C
C            KEY    - Integer
C                     Key for choice of local integration rule
C                     A Gauss-Kronrod pair is used with
C                          7 - 15 points if KEY.LT.2,
C                         10 - 21 points if KEY = 2,
C                         15 - 31 points if KEY = 3,
C                         20 - 41 points if KEY = 4,
C                         25 - 51 points if KEY = 5,
C                         30 - 61 points if KEY.GT.5.
C
C            LIMIT  - Integer
C                     Gives an upperbound on the number of subintervals
C                     in the partition of (A,B), LIMIT.GE.1.
C
C         ON RETURN
C            RESULT - Double precision
C                     Approximation to the integral
C
C            ABSERR - Double precision
C                     Estimate of the modulus of the absolute error,
C                     which should equal or exceed ABS(I-RESULT)
C
C            NEVAL  - Integer
C                     Number of integrand evaluations
C
C            IER    - Integer
C                     IER = 0 Normal and reliable termination of the
C                             routine. It is assumed that the requested
C                             accuracy has been achieved.
C                     IER.GT.0 Abnormal termination of the routine
C                             The estimates for result and error are
C                             less reliable. It is assumed that the
C                             requested accuracy has not been achieved.
C            ERROR MESSAGES
C                     IER = 1 Maximum number of subdivisions allowed
C                             has been achieved. One can allow more
C                             subdivisions by increasing the value
C                             of LIMIT.
C                             However, if this yields no improvement it
C                             is rather advised to analyze the integrand
C                             in order to determine the integration
C                             difficulties. If the position of a local
C                             difficulty can be determined(e.g.
C                             SINGULARITY, DISCONTINUITY within the
C                             interval) one will probably gain from
C                             splitting up the interval at this point
C                             and calling the integrator on the
C                             subranges. If possible, an appropriate
C                             special-purpose integrator should be used
C                             which is designed for handling the type of
C                             difficulty involved.
C                         = 2 The occurrence of roundoff error is
C                             detected, which prevents the requested
C                             tolerance from being achieved.
C                         = 3 Extremely bad integrand behaviour occurs
C                             at some points of the integration
C                             interval.
C                         = 6 The input is invalid, because
C                             (EPSABS.LE.0 and
C                              EPSREL.LT.MAX(50*REL.MACH.ACC.,0.5D-28),
C                             RESULT, ABSERR, NEVAL, LAST, RLIST(1) ,
C                             ELIST(1) and IORD(1) are set to zero.
C                             ALIST(1) and BLIST(1) are set to A and B
C                             respectively.
C
C            ALIST   - Double precision
C                      Vector of dimension at least LIMIT, the first
C                       LAST  elements of which are the left
C                      end points of the subintervals in the partition
C                      of the given integration range (A,B)
C
C            BLIST   - Double precision
C                      Vector of dimension at least LIMIT, the first
C                       LAST  elements of which are the right
C                      end points of the subintervals in the partition
C                      of the given integration range (A,B)
C
C            RLIST   - Double precision
C                      Vector of dimension at least LIMIT, the first
C                       LAST  elements of which are the
C                      integral approximations on the subintervals
C
C            ELIST   - Double precision
C                      Vector of dimension at least LIMIT, the first
C                       LAST  elements of which are the moduli of the
C                      absolute error estimates on the subintervals
C
C            IORD    - Integer
C                      Vector of dimension at least LIMIT, the first K
C                      elements of which are pointers to the
C                      error estimates over the subintervals,
C                      such that ELIST(IORD(1)), ...,
C                      ELIST(IORD(K)) form a decreasing sequence,
C                      with K = LAST if LAST.LE.(LIMIT/2+2), and
C                      K = LIMIT+1-LAST otherwise
C
C            LAST    - Integer
C                      Number of subintervals actually produced in the
C                      subdivision process
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH,DQK15,DQK21,DQK31,DQK41,DQK51,DQK61,DQPSRT
C***END PROLOGUE  DQAGE
C
      INCLUDE 'DPCOMC.INC'
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      DOUBLE PRECISION A,ABSERR,ALIST,AREA,AREA1,AREA12,AREA2,A1,A2,B,
     1  BLIST,B1,B2,DEFABS,DEFAB1,DEFAB2,ELIST,EPMACH,
     2  EPSABS,EPSREL,ERRBND,ERRMAX,ERROR1,ERROR2,ERRO12,ERRSUM,F,
     3  RESABS,RESULT,RLIST,UFLOW
      INTEGER IER,IORD,IROFF1,IROFF2,K,KEY,KEYF,LAST,LIMIT,MAXERR,NEVAL,
     1  NRMAX
C
      DIMENSION ALIST(LIMIT),BLIST(LIMIT),ELIST(LIMIT),IORD(LIMIT),
     1  RLIST(LIMIT)
C
      EXTERNAL F
C
C            LIST OF MAJOR VARIABLES
C            -----------------------
C
C           ALIST     - LIST OF LEFT END POINTS OF ALL SUBINTERVALS
C                       CONSIDERED UP TO NOW
C           BLIST     - LIST OF RIGHT END POINTS OF ALL SUBINTERVALS
C                       CONSIDERED UP TO NOW
C           RLIST(I)  - APPROXIMATION TO THE INTEGRAL OVER
C                      (ALIST(I),BLIST(I))
C           ELIST(I)  - ERROR ESTIMATE APPLYING TO RLIST(I)
C           MAXERR    - POINTER TO THE INTERVAL WITH LARGEST
C                       ERROR ESTIMATE
C           ERRMAX    - ELIST(MAXERR)
C           AREA      - SUM OF THE INTEGRALS OVER THE SUBINTERVALS
C           ERRSUM    - SUM OF THE ERRORS OVER THE SUBINTERVALS
C           ERRBND    - REQUESTED ACCURACY MAX(EPSABS,EPSREL*
C                       ABS(RESULT))
C           *****1    - VARIABLE FOR THE LEFT SUBINTERVAL
C           *****2    - VARIABLE FOR THE RIGHT SUBINTERVAL
C           LAST      - INDEX FOR SUBDIVISION
C
C
C           MACHINE DEPENDENT CONSTANTS
C           ---------------------------
C
C           EPMACH  IS THE LARGEST RELATIVE SPACING.
C           UFLOW  IS THE SMALLEST POSITIVE MAGNITUDE.
C
C***FIRST EXECUTABLE STATEMENT  DQAGE
      EPMACH = D1MACH(4)
      UFLOW = D1MACH(1)
C
C           TEST ON VALIDITY OF PARAMETERS
C           ------------------------------
C
      IER = 0
      NEVAL = 0
      LAST = 0
      RESULT = 0.0D+00
      ABSERR = 0.0D+00
      ALIST(1) = A
      BLIST(1) = B
      RLIST(1) = 0.0D+00
      ELIST(1) = 0.0D+00
      IORD(1) = 0
      IF(EPSABS.LE.0.0D+00.AND.
     1  EPSREL.LT.MAX(0.5D+02*EPMACH,0.5D-28)) IER = 6
      IF(IER.EQ.6) GO TO 999
C
C           FIRST APPROXIMATION TO THE INTEGRAL
C           -----------------------------------
C
      KEYF = KEY
      IF(KEY.LE.0) KEYF = 1
      IF(KEY.GE.7) KEYF = 6
      NEVAL = 0
      IF(KEYF.EQ.1) CALL DQK15(F,A,B,RESULT,ABSERR,DEFABS,RESABS)
      IF(KEYF.EQ.2) CALL DQK21(F,A,B,RESULT,ABSERR,DEFABS,RESABS)
      IF(KEYF.EQ.3) CALL DQK31(F,A,B,RESULT,ABSERR,DEFABS,RESABS)
      IF(KEYF.EQ.4) CALL DQK41(F,A,B,RESULT,ABSERR,DEFABS,RESABS)
      IF(KEYF.EQ.5) CALL DQK51(F,A,B,RESULT,ABSERR,DEFABS,RESABS)
      IF(KEYF.EQ.6) CALL DQK61(F,A,B,RESULT,ABSERR,DEFABS,RESABS)
      LAST = 1
      RLIST(1) = RESULT
      ELIST(1) = ABSERR
      IORD(1) = 1
C
C           TEST ON ACCURACY.
C
      ERRBND = MAX(EPSABS,EPSREL*ABS(RESULT))
      IF(ABSERR.LE.0.5D+02*EPMACH*DEFABS.AND.ABSERR.GT.ERRBND) IER = 2
      IF(LIMIT.EQ.1) IER = 1
      IF(IER.NE.0.OR.(ABSERR.LE.ERRBND.AND.ABSERR.NE.RESABS)
     1  .OR.ABSERR.EQ.0.0D+00) GO TO 60
C
C           INITIALIZATION
C           --------------
C
C
      ERRMAX = ABSERR
      MAXERR = 1
      AREA = RESULT
      ERRSUM = ABSERR
      NRMAX = 1
      IROFF1 = 0
      IROFF2 = 0
C
C           MAIN DO-LOOP
C           ------------
C
      DO 30 LAST = 2,LIMIT
C
C           BISECT THE SUBINTERVAL WITH THE LARGEST ERROR ESTIMATE.
C
        A1 = ALIST(MAXERR)
        B1 = 0.5D+00*(ALIST(MAXERR)+BLIST(MAXERR))
        A2 = B1
        B2 = BLIST(MAXERR)
        IF(KEYF.EQ.1) CALL DQK15(F,A1,B1,AREA1,ERROR1,RESABS,DEFAB1)
        IF(KEYF.EQ.2) CALL DQK21(F,A1,B1,AREA1,ERROR1,RESABS,DEFAB1)
        IF(KEYF.EQ.3) CALL DQK31(F,A1,B1,AREA1,ERROR1,RESABS,DEFAB1)
        IF(KEYF.EQ.4) CALL DQK41(F,A1,B1,AREA1,ERROR1,RESABS,DEFAB1)
        IF(KEYF.EQ.5) CALL DQK51(F,A1,B1,AREA1,ERROR1,RESABS,DEFAB1)
        IF(KEYF.EQ.6) CALL DQK61(F,A1,B1,AREA1,ERROR1,RESABS,DEFAB1)
        IF(KEYF.EQ.1) CALL DQK15(F,A2,B2,AREA2,ERROR2,RESABS,DEFAB2)
        IF(KEYF.EQ.2) CALL DQK21(F,A2,B2,AREA2,ERROR2,RESABS,DEFAB2)
        IF(KEYF.EQ.3) CALL DQK31(F,A2,B2,AREA2,ERROR2,RESABS,DEFAB2)
        IF(KEYF.EQ.4) CALL DQK41(F,A2,B2,AREA2,ERROR2,RESABS,DEFAB2)
        IF(KEYF.EQ.5) CALL DQK51(F,A2,B2,AREA2,ERROR2,RESABS,DEFAB2)
        IF(KEYF.EQ.6) CALL DQK61(F,A2,B2,AREA2,ERROR2,RESABS,DEFAB2)
C
C           IMPROVE PREVIOUS APPROXIMATIONS TO INTEGRAL
C           AND ERROR AND TEST FOR ACCURACY.
C
        NEVAL = NEVAL+1
        AREA12 = AREA1+AREA2
        ERRO12 = ERROR1+ERROR2
        ERRSUM = ERRSUM+ERRO12-ERRMAX
        AREA = AREA+AREA12-RLIST(MAXERR)
        IF(DEFAB1.EQ.ERROR1.OR.DEFAB2.EQ.ERROR2) GO TO 5
        IF(ABS(RLIST(MAXERR)-AREA12).LE.0.1D-04*ABS(AREA12)
     1  .AND.ERRO12.GE.0.99D+00*ERRMAX) IROFF1 = IROFF1+1
        IF(LAST.GT.10.AND.ERRO12.GT.ERRMAX) IROFF2 = IROFF2+1
    5   RLIST(MAXERR) = AREA1
        RLIST(LAST) = AREA2
        ERRBND = MAX(EPSABS,EPSREL*ABS(AREA))
        IF(ERRSUM.LE.ERRBND) GO TO 8
C
C           TEST FOR ROUNDOFF ERROR AND EVENTUALLY SET ERROR FLAG.
C
        IF(IROFF1.GE.6.OR.IROFF2.GE.20) IER = 2
C
C           SET ERROR FLAG IN THE CASE THAT THE NUMBER OF SUBINTERVALS
C           EQUALS LIMIT.
C
        IF(LAST.EQ.LIMIT) IER = 1
C
C           SET ERROR FLAG IN THE CASE OF BAD INTEGRAND BEHAVIOUR
C           AT A POINT OF THE INTEGRATION RANGE.
C
        IF(MAX(ABS(A1),ABS(B2)).LE.(0.1D+01+0.1D+03*
     1  EPMACH)*(ABS(A2)+0.1D+04*UFLOW)) IER = 3
C
C           APPEND THE NEWLY-CREATED INTERVALS TO THE LIST.
C
    8   IF(ERROR2.GT.ERROR1) GO TO 10
        ALIST(LAST) = A2
        BLIST(MAXERR) = B1
        BLIST(LAST) = B2
        ELIST(MAXERR) = ERROR1
        ELIST(LAST) = ERROR2
        GO TO 20
   10   ALIST(MAXERR) = A2
        ALIST(LAST) = A1
        BLIST(LAST) = B1
        RLIST(MAXERR) = AREA2
        RLIST(LAST) = AREA1
        ELIST(MAXERR) = ERROR2
        ELIST(LAST) = ERROR1
C
C           CALL SUBROUTINE DQPSRT TO MAINTAIN THE DESCENDING ORDERING
C           IN THE LIST OF ERROR ESTIMATES AND SELECT THE SUBINTERVAL
C           WITH THE LARGEST ERROR ESTIMATE (TO BE BISECTED NEXT).
C
   20   CALL DQPSRT(LIMIT,LAST,MAXERR,ERRMAX,ELIST,IORD,NRMAX)
C ***JUMP OUT OF DO-LOOP
        IF(IER.NE.0.OR.ERRSUM.LE.ERRBND) GO TO 40
   30 CONTINUE
C
C           COMPUTE FINAL RESULT.
C           ---------------------
C
   40 RESULT = 0.0D+00
      DO 50 K=1,LAST
        RESULT = RESULT+RLIST(K)
   50 CONTINUE
      ABSERR = ERRSUM
   60 IF(KEYF.NE.1) NEVAL = (10*KEYF+1)*(2*NEVAL+1)
      IF(KEYF.EQ.1) NEVAL = 30*NEVAL+15
  999 RETURN
      END
      SUBROUTINE DQAGI(F,BOUND,INF,EPSABS,EPSREL,RESULT,ABSERR,NEVAL,
     1   IER,LIMIT,LENW,LAST,IWORK,WORK)
C
C   ADDED TO DATAPLOT 12/2003.  THIS ROUTINE ADDED FOR INTERNAL
C   DATAPLOT USAGE.
C
C***BEGIN PROLOGUE  DQAGI
C***DATE WRITTEN   800101   (YYMMDD)
C***REVISION DATE  830518   (YYMMDD)
C***CATEGORY NO.  H2A3A1,H2A4A1
C***KEYWORDS  AUTOMATIC INTEGRATOR,EXTRAPOLATION,GENERAL-PURPOSE,
C             GLOBALLY ADAPTIVE,INFINITE INTERVALS,TRANSFORMATION
C***AUTHOR  PIESSENS, ROBERT, APPLIED MATH. AND PROGR. DIV. -
C             K. U. LEUVEN
C           DE DONCKER, ELISE, APPLIED MATH. AND PROGR. DIV. -
C             K. U. LEUVEN
C***PURPOSE  The routine calculates an approximation result to a given
C            INTEGRAL   I = Integral of F over (BOUND,+INFINITY)
C            OR I = Integral of F over (-INFINITY,BOUND)
C            OR I = Integral of F over (-INFINITY,+INFINITY)
C            Hopefully satisfying following claim for accuracy
C            ABS(I-RESULT).LE.MAX(EPSABS,EPSREL*ABS(I)).
C***DESCRIPTION
C
C        Integration over infinite intervals
C        Standard fortran subroutine
C
C        PARAMETERS
C         ON ENTRY
C            F      - Double precision
C                     Function subprogram defining the integrand
C                     function F(X). The actual name for F needs to be
C                     declared E X T E R N A L in the driver program.
C
C            BOUND  - Double precision
C                     Finite bound of integration range
C                     (has no meaning if interval is doubly-infinite)
C
C            INF    - Integer
C                     indicating the kind of integration range involved
C                     INF = 1 corresponds to  (BOUND,+INFINITY),
C                     INF = -1            to  (-INFINITY,BOUND),
C                     INF = 2             to (-INFINITY,+INFINITY).
C
C            EPSABS - Double precision
C                     Absolute accuracy requested
C            EPSREL - Double precision
C                     Relative accuracy requested
C                     If  EPSABS.LE.0
C                     and EPSREL.LT.MAX(50*REL.MACH.ACC.,0.5D-28),
C                     the routine will end with IER = 6.
C
C
C         ON RETURN
C            RESULT - Double precision
C                     Approximation to the integral
C
C            ABSERR - Double precision
C                     Estimate of the modulus of the absolute error,
C                     which should equal or exceed ABS(I-RESULT)
C
C            NEVAL  - Integer
C                     Number of integrand evaluations
C
C            IER    - Integer
C                     IER = 0 normal and reliable termination of the
C                             routine. It is assumed that the requested
C                             accuracy has been achieved.
C                   - IER.GT.0 abnormal termination of the routine. The
C                             estimates for result and error are less
C                             reliable. It is assumed that the requested
C                             accuracy has not been achieved.
C            ERROR MESSAGES
C                     IER = 1 Maximum number of subdivisions allowed
C                             has been achieved. One can allow more
C                             subdivisions by increasing the value of
C                             LIMIT (and taking the according dimension
C                             adjustments into account). However, if
C                             this yields no improvement it is advised
C                             to analyze the integrand in order to
C                             determine the integration difficulties. If
C                             the position of a local difficulty can be
C                             determined (e.g. SINGULARITY,
C                             DISCONTINUITY within the interval) one
C                             will probably gain from splitting up the
C                             interval at this point and calling the
C                             integrator on the subranges. If possible,
C                             an appropriate special-purpose integrator
C                             should be used, which is designed for
C                             handling the type of difficulty involved.
C                         = 2 The occurrence of roundoff error is
C                             detected, which prevents the requested
C                             tolerance from being achieved.
C                             The error may be under-estimated.
C                         = 3 Extremely bad integrand behaviour occurs
C                             at some points of the integration
C                             interval.
C                         = 4 The algorithm does not converge.
C                             Roundoff error is detected in the
C                             extrapolation table.
C                             It is assumed that the requested tolerance
C                             cannot be achieved, and that the returned
C                             RESULT is the best which can be obtained.
C                         = 5 The integral is probably divergent, or
C                             slowly convergent. It must be noted that
C                             divergence can occur with any other value
C                             of IER.
C                         = 6 The input is invalid, because
C                             (EPSABS.LE.0 and
C                              EPSREL.LT.MAX(50*REL.MACH.ACC.,0.5D-28))
C                              or LIMIT.LT.1 or LENIW.LT.LIMIT*4.
C                             RESULT, ABSERR, NEVAL, LAST are set to
C                             zero. Exept when LIMIT or LENIW is
C                             invalid, IWORK(1), WORK(LIMIT*2+1) and
C                             WORK(LIMIT*3+1) are set to ZERO, WORK(1)
C                             is set to A and WORK(LIMIT+1) to B.
C
C         DIMENSIONING PARAMETERS
C            LIMIT - Integer
C                    Dimensioning parameter for IWORK
C                    LIMIT determines the maximum number of subintervals
C                    in the partition of the given integration interval
C                    (A,B), LIMIT.GE.1.
C                    If LIMIT.LT.1, the routine will end with IER = 6.
C
C            LENW  - Integer
C                    Dimensioning parameter for WORK
C                    LENW must be at least LIMIT*4.
C                    If LENW.LT.LIMIT*4, the routine will end
C                    with IER = 6.
C
C            LAST  - Integer
C                    On return, LAST equals the number of subintervals
C                    produced in the subdivision process, which
C                    determines the number of significant elements
C                    actually in the WORK ARRAYS.
C
C         WORK ARRAYS
C            IWORK - Integer
C                    Vector of dimension at least LIMIT, the first
C                    K elements of which contain pointers
C                    to the error estimates over the subintervals,
C                    such that WORK(LIMIT*3+IWORK(1)),... ,
C                    WORK(LIMIT*3+IWORK(K)) form a decreasing
C                    sequence, with K = LAST if LAST.LE.(LIMIT/2+2), and
C                    K = LIMIT+1-LAST otherwise
C
C            WORK  - Double precision
C                    Vector of dimension at least LENW
C                    on return
C                    WORK(1), ..., WORK(LAST) contain the left
C                     end points of the subintervals in the
C                     partition of (A,B),
C                    WORK(LIMIT+1), ..., WORK(LIMIT+LAST) Contain
C                     the right end points,
C                    WORK(LIMIT*2+1), ...,WORK(LIMIT*2+LAST) contain the
C                     integral approximations over the subintervals,
C                    WORK(LIMIT*3+1), ..., WORK(LIMIT*3)
C                     contain the error estimates.
C***REFERENCES  (NONE)
C***ROUTINES CALLED  DQAGIE,XERROR
C***END PROLOGUE  DQAGI
C
      DOUBLE PRECISION ABSERR,BOUND,EPSABS,EPSREL,F,RESULT,WORK
      INTEGER IER,INF,IWORK,LAST,LENW,LIMIT,LVL,L1,L2,L3,NEVAL
C
      DIMENSION IWORK(LIMIT),WORK(LENW)
C
      INCLUDE 'DPCOMC.INC'
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      EXTERNAL F
C
C         CHECK VALIDITY OF LIMIT AND LENW.
C
C***FIRST EXECUTABLE STATEMENT  DQAGI
      IER = 6
      NEVAL = 0
      LAST = 0
      RESULT = 0.0D+00
      ABSERR = 0.0D+00
      IF(LIMIT.LT.1.OR.LENW.LT.LIMIT*4) GO TO 10
C
C         PREPARE CALL FOR DQAGIE.
C
      L1 = LIMIT+1
      L2 = LIMIT+L1
      L3 = LIMIT+L2
C
      CALL DQAGIE(F,BOUND,INF,EPSABS,EPSREL,LIMIT,RESULT,ABSERR,
     1  NEVAL,IER,WORK(1),WORK(L1),WORK(L2),WORK(L3),IWORK,LAST)
C
C         CALL ERROR HANDLER IF NECESSARY.
C
       LVL = 0
10    IF(IER.EQ.6) LVL = 1
      IF(IER.NE.0) THEN
CCCCC   CALL XERROR( 'ABNORMAL RETURN FROM  QAGI',
CCCCC1  26,IER,LVL)
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,901)
  901   FORMAT('***** ERROR--ABNORMAL RETURN FROM QAGI INTEGRATION ',
     1         'ROUTINE.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      RETURN
      END
      SUBROUTINE DQAGIE(F,BOUND,INF,EPSABS,EPSREL,LIMIT,RESULT,ABSERR,
     1   NEVAL,IER,ALIST,BLIST,RLIST,ELIST,IORD,LAST)
C
C   ADDED TO DATAPLOT 12/2003.  THIS ROUTINE ADDED FOR INTERNAL
C   DATAPLOT USAGE.
C
C***BEGIN PROLOGUE  DQAGIE
C***DATE WRITTEN   800101   (YYMMDD)
C***REVISION DATE  830518   (YYMMDD)
C***REVISION DATE  980526   (YYMMDD) Fixed documentation of parameter INF
C***REVISION HISTORY (YYMMDD)
C   000601   Changed DMAX1/DABS to generic MAX/ABS
C***CATEGORY NO.  H2A3A1,H2A4A1
C***KEYWORDS  AUTOMATIC INTEGRATOR,EXTRAPOLATION,GENERAL-PURPOSE,
C             GLOBALLY ADAPTIVE,INFINITE INTERVALS,TRANSFORMATION
C***AUTHOR  PIESSENS, ROBERT, APPLIED MATH. AND PROGR. DIV. -
C             K. U. LEUVEN
C           DE DONCKER, ELISE, APPLIED MATH. AND PROGR. DIV. -
C             K. U. LEUVEN
C***PURPOSE  The routine calculates an approximation result to a given
C            integral   I = Integral of F over (BOUND,+INFINITY)
C            or I = Integral of F over (-INFINITY,BOUND)
C            or I = Integral of F over (-INFINITY,+INFINITY),
C            hopefully satisfying following claim for accuracy
C            ABS(I-RESULT).LE.MAX(EPSABS,EPSREL*ABS(I))
C***DESCRIPTION
C
C Integration over infinite intervals
C Standard fortran subroutine
C
C            F      - Double precision
C                     Function subprogram defining the integrand
C                     function F(X). The actual name for F needs to be
C                     declared E X T E R N A L in the driver program.
C
C            BOUND  - Double precision
C                     Finite bound of integration range
C                     (has no meaning if interval is doubly-infinite)
C
C            INF    - Integer
C                     Indicating the kind of integration range involved
C                     INF = 1 corresponds to  (BOUND,+INFINITY),
C                     INF = -1            to  (-INFINITY,BOUND),
C                     INF = 2             to (-INFINITY,+INFINITY).
C
C            EPSABS - Double precision
C                     Absolute accuracy requested
C            EPSREL - Double precision
C                     Relative accuracy requested
C                     If  EPSABS.LE.0
C                     and EPSREL.LT.MAX(50*REL.MACH.ACC.,0.5D-28),
C                     the routine will end with IER = 6.
C
C            LIMIT  - Integer
C                     Gives an upper bound on the number of subintervals
C                     in the partition of (A,B), LIMIT.GE.1
C
C         ON RETURN
C            RESULT - Double precision
C                     Approximation to the integral
C
C            ABSERR - Double precision
C                     Estimate of the modulus of the absolute error,
C                     which should equal or exceed ABS(I-RESULT)
C
C            NEVAL  - Integer
C                     Number of integrand evaluations
C
C            IER    - Integer
C                     IER = 0 Normal and reliable termination of the
C                             routine. It is assumed that the requested
C                             accuracy has been achieved.
C                   - IER.GT.0 Abnormal termination of the routine. The
C                             estimates for result and error are less
C                             reliable. It is assumed that the requested
C                             accuracy has not been achieved.
C            ERROR MESSAGES
C                     IER = 1 Maximum number of subdivisions allowed
C                             has been achieved. One can allow more
C                             subdivisions by increasing the value of
C                             LIMIT (and taking the according dimension
C                             adjustments into account). However,if
C                             this yields no improvement it is advised
C                             to analyze the integrand in order to
C                             determine the integration difficulties.
C                             If the position of a local difficulty can
C                             be determined (e.g. SINGULARITY,
C                             DISCONTINUITY within the interval) one
C                             will probably gain from splitting up the
C                             interval at this point and calling the
C                             integrator on the subranges. If possible,
C                             an appropriate special-purpose integrator
C                             should be used, which is designed for
C                             handling the type of difficulty involved.
C                         = 2 The occurrence of roundoff error is
C                             detected, which prevents the requested
C                             tolerance from being achieved.
C                             The error may be under-estimated.
C                         = 3 Extremely bad integrand behaviour occurs
C                             at some points of the integration
C                             interval.
C                         = 4 The algorithm does not converge.
C                             Roundoff error is detected in the
C                             extrapolation table.
C                             It is assumed that the requested tolerance
C                             cannot be achieved, and that the returned
C                             result is the best which can be obtained.
C                         = 5 The integral is probably divergent, or
C                             slowly convergent. It must be noted that
C                             divergence can occur with any other value
C                             of IER.
C                         = 6 The input is invalid, because
C                             (EPSABS.LE.0 and
C                              EPSREL.LT.MAX(50*REL.MACH.ACC.,0.5D-28),
C                             RESULT, ABSERR, NEVAL, LAST, RLIST(1),
C                             ELIST(1) and IORD(1) are set to zero.
C                             ALIST(1) and BLIST(1) are set to 0
C                             and 1 respectively.
C
C            ALIST  - Double precision
C                     Vector of dimension at least LIMIT, the first
C                      LAST  elements of which are the left
C                     end points of the subintervals in the partition
C                     of the transformed integration range (0,1).
C
C            BLIST  - Double precision
C                     Vector of dimension at least LIMIT, the first
C                      LAST  elements of which are the right
C                     end points of the subintervals in the partition
C                     of the transformed integration range (0,1).
C
C            RLIST  - Double precision
C                     Vector of dimension at least LIMIT, the first
C                      LAST  elements of which are the integral
C                     approximations on the subintervals
C
C            ELIST  - Double precision
C                     Vector of dimension at least LIMIT,  the first
C                     LAST elements of which are the moduli of the
C                     absolute error estimates on the subintervals
C
C            IORD   - Integer
C                     Vector of dimension LIMIT, the first K
C                     elements of which are pointers to the
C                     error estimates over the subintervals,
C                     such that ELIST(IORD(1)), ..., ELIST(IORD(K))
C                     form a decreasing sequence, with K = LAST
C                     If LAST.LE.(LIMIT/2+2), and K = LIMIT+1-LAST
C                     otherwise
C
C            LAST   - Integer
C                     Number of subintervals actually produced
C                     in the subdivision process
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH,DQELG,DQK15I,DQPSRT
C***END PROLOGUE  DQAGIE
      DOUBLE PRECISION ABSEPS,ABSERR,ALIST,AREA,AREA1,AREA12,AREA2,A1,
     1  A2,BLIST,BOUN,BOUND,B1,B2,CORREC,DEFABS,DEFAB1,DEFAB2,
     2  DRES,ELIST,EPMACH,EPSABS,EPSREL,ERLARG,ERLAST,
     3  ERRBND,ERRMAX,ERROR1,ERROR2,ERRO12,ERRSUM,ERTEST,F,OFLOW,RESABS,
     4  RESEPS,RESULT,RES3LA,RLIST,RLIST2,SMALL,UFLOW
      INTEGER ID,IER,IERRO,INF,IORD,IROFF1,IROFF2,IROFF3,JUPBND,K,KSGN,
     1  KTMIN,LAST,LIMIT,MAXERR,NEVAL,NRES,NRMAX,NUMRL2
      LOGICAL EXTRAP,NOEXT
C
      DIMENSION ALIST(LIMIT),BLIST(LIMIT),ELIST(LIMIT),IORD(LIMIT),
     1  RES3LA(3),RLIST(LIMIT),RLIST2(52)
C
      INCLUDE 'DPCOMC.INC'
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      EXTERNAL F
C
C            THE DIMENSION OF RLIST2 IS DETERMINED BY THE VALUE OF
C            LIMEXP IN SUBROUTINE DQELG.
C
C
C            LIST OF MAJOR VARIABLES
C            -----------------------
C
C           ALIST     - LIST OF LEFT END POINTS OF ALL SUBINTERVALS
C                       CONSIDERED UP TO NOW
C           BLIST     - LIST OF RIGHT END POINTS OF ALL SUBINTERVALS
C                       CONSIDERED UP TO NOW
C           RLIST(I)  - APPROXIMATION TO THE INTEGRAL OVER
C                       (ALIST(I),BLIST(I))
C           RLIST2    - ARRAY OF DIMENSION AT LEAST (LIMEXP+2),
C                       CONTAINING THE PART OF THE EPSILON TABLE
C                       WICH IS STILL NEEDED FOR FURTHER COMPUTATIONS
C           ELIST(I)  - ERROR ESTIMATE APPLYING TO RLIST(I)
C           MAXERR    - POINTER TO THE INTERVAL WITH LARGEST ERROR
C                       ESTIMATE
C           ERRMAX    - ELIST(MAXERR)
C           ERLAST    - ERROR ON THE INTERVAL CURRENTLY SUBDIVIDED
C                       (BEFORE THAT SUBDIVISION HAS TAKEN PLACE)
C           AREA      - SUM OF THE INTEGRALS OVER THE SUBINTERVALS
C           ERRSUM    - SUM OF THE ERRORS OVER THE SUBINTERVALS
C           ERRBND    - REQUESTED ACCURACY MAX(EPSABS,EPSREL*
C                       ABS(RESULT))
C           *****1    - VARIABLE FOR THE LEFT SUBINTERVAL
C           *****2    - VARIABLE FOR THE RIGHT SUBINTERVAL
C           LAST      - INDEX FOR SUBDIVISION
C           NRES      - NUMBER OF CALLS TO THE EXTRAPOLATION ROUTINE
C           NUMRL2    - NUMBER OF ELEMENTS CURRENTLY IN RLIST2. IF AN
C                       APPROPRIATE APPROXIMATION TO THE COMPOUNDED
C                       INTEGRAL HAS BEEN OBTAINED, IT IS PUT IN
C                       RLIST2(NUMRL2) AFTER NUMRL2 HAS BEEN INCREASED
C                       BY ONE.
C           SMALL     - LENGTH OF THE SMALLEST INTERVAL CONSIDERED UP
C                       TO NOW, MULTIPLIED BY 1.5
C           ERLARG    - SUM OF THE ERRORS OVER THE INTERVALS LARGER
C                       THAN THE SMALLEST INTERVAL CONSIDERED UP TO NOW
C           EXTRAP    - LOGICAL VARIABLE DENOTING THAT THE ROUTINE
C                       IS ATTEMPTING TO PERFORM EXTRAPOLATION. I.E.
C                       BEFORE SUBDIVIDING THE SMALLEST INTERVAL WE
C                       TRY TO DECREASE THE VALUE OF ERLARG.
C           NOEXT     - LOGICAL VARIABLE DENOTING THAT EXTRAPOLATION
C                       IS NO LONGER ALLOWED (TRUE-VALUE)
C
C            MACHINE DEPENDENT CONSTANTS
C            ---------------------------
C
C           EPMACH IS THE LARGEST RELATIVE SPACING.
C           UFLOW IS THE SMALLEST POSITIVE MAGNITUDE.
C           OFLOW IS THE LARGEST POSITIVE MAGNITUDE.
C
C***FIRST EXECUTABLE STATEMENT  DQAGIE
       EPMACH = D1MACH(4)
C
C           TEST ON VALIDITY OF PARAMETERS
C           -----------------------------
C
      IER = 0
      NEVAL = 0
      LAST = 0
      RESULT = 0.0D+00
      ABSERR = 0.0D+00
      ALIST(1) = 0.0D+00
      BLIST(1) = 0.1D+01
      RLIST(1) = 0.0D+00
      ELIST(1) = 0.0D+00
      IORD(1) = 0
      IF(EPSABS.LE.0.0D+00.AND.EPSREL.LT.MAX(0.5D+02*EPMACH,0.5D-28))
     1  IER = 6
       IF(IER.EQ.6) GO TO 999
C
C
C           FIRST APPROXIMATION TO THE INTEGRAL
C           -----------------------------------
C
C           DETERMINE THE INTERVAL TO BE MAPPED ONTO (0,1).
C           IF INF = 2 THE INTEGRAL IS COMPUTED AS I = I1+I2, WHERE
C           I1 = INTEGRAL OF F OVER (-INFINITY,0),
C           I2 = INTEGRAL OF F OVER (0,+INFINITY).
C
      BOUN = BOUND
      IF(INF.EQ.2) BOUN = 0.0D+00
      CALL DQK15I(F,BOUN,INF,0.0D+00,0.1D+01,RESULT,ABSERR,
     1  DEFABS,RESABS)
C
C           TEST ON ACCURACY
C
      LAST = 1
      RLIST(1) = RESULT
      ELIST(1) = ABSERR
      IORD(1) = 1
      DRES = ABS(RESULT)
      ERRBND = MAX(EPSABS,EPSREL*DRES)
      IF(ABSERR.LE.1.0D+02*EPMACH*DEFABS.AND.ABSERR.GT.ERRBND) IER = 2
      IF(LIMIT.EQ.1) IER = 1
      IF(IER.NE.0.OR.(ABSERR.LE.ERRBND.AND.ABSERR.NE.RESABS).OR.
     1  ABSERR.EQ.0.0D+00) GO TO 130
C
C           INITIALIZATION
C           --------------
C
      UFLOW = D1MACH(1)
      OFLOW = D1MACH(2)
      RLIST2(1) = RESULT
      ERRMAX = ABSERR
      MAXERR = 1
      AREA = RESULT
      ERRSUM = ABSERR
      ABSERR = OFLOW
      NRMAX = 1
      NRES = 0
      KTMIN = 0
      NUMRL2 = 2
      EXTRAP = .FALSE.
      NOEXT = .FALSE.
      IERRO = 0
      IROFF1 = 0
      IROFF2 = 0
      IROFF3 = 0
      KSGN = -1
      IF(DRES.GE.(0.1D+01-0.5D+02*EPMACH)*DEFABS) KSGN = 1
C
C           MAIN DO-LOOP
C           ------------
C
      DO 90 LAST = 2,LIMIT
C
C           BISECT THE SUBINTERVAL WITH NRMAX-TH LARGEST ERROR ESTIMATE.
C
        A1 = ALIST(MAXERR)
        B1 = 0.5D+00*(ALIST(MAXERR)+BLIST(MAXERR))
        A2 = B1
        B2 = BLIST(MAXERR)
        ERLAST = ERRMAX
        CALL DQK15I(F,BOUN,INF,A1,B1,AREA1,ERROR1,RESABS,DEFAB1)
        CALL DQK15I(F,BOUN,INF,A2,B2,AREA2,ERROR2,RESABS,DEFAB2)
C
C           IMPROVE PREVIOUS APPROXIMATIONS TO INTEGRAL
C           AND ERROR AND TEST FOR ACCURACY.
C
        AREA12 = AREA1+AREA2
        ERRO12 = ERROR1+ERROR2
        ERRSUM = ERRSUM+ERRO12-ERRMAX
        AREA = AREA+AREA12-RLIST(MAXERR)
        IF(DEFAB1.EQ.ERROR1.OR.DEFAB2.EQ.ERROR2)GO TO 15
        IF(ABS(RLIST(MAXERR)-AREA12).GT.0.1D-04*ABS(AREA12)
     1  .OR.ERRO12.LT.0.99D+00*ERRMAX) GO TO 10
        IF(EXTRAP) IROFF2 = IROFF2+1
        IF(.NOT.EXTRAP) IROFF1 = IROFF1+1
   10   IF(LAST.GT.10.AND.ERRO12.GT.ERRMAX) IROFF3 = IROFF3+1
   15   RLIST(MAXERR) = AREA1
        RLIST(LAST) = AREA2
        ERRBND = MAX(EPSABS,EPSREL*ABS(AREA))
C
C           TEST FOR ROUNDOFF ERROR AND EVENTUALLY SET ERROR FLAG.
C
        IF(IROFF1+IROFF2.GE.10.OR.IROFF3.GE.20) IER = 2
        IF(IROFF2.GE.5) IERRO = 3
C
C           SET ERROR FLAG IN THE CASE THAT THE NUMBER OF
C           SUBINTERVALS EQUALS LIMIT.
C
        IF(LAST.EQ.LIMIT) IER = 1
C
C           SET ERROR FLAG IN THE CASE OF BAD INTEGRAND BEHAVIOUR
C           AT SOME POINTS OF THE INTEGRATION RANGE.
C
        IF(MAX(ABS(A1),ABS(B2)).LE.(0.1D+01+0.1D+03*EPMACH)*
     1  (ABS(A2)+0.1D+04*UFLOW)) IER = 4
C
C           APPEND THE NEWLY-CREATED INTERVALS TO THE LIST.
C
        IF(ERROR2.GT.ERROR1) GO TO 20
        ALIST(LAST) = A2
        BLIST(MAXERR) = B1
        BLIST(LAST) = B2
        ELIST(MAXERR) = ERROR1
        ELIST(LAST) = ERROR2
        GO TO 30
   20   ALIST(MAXERR) = A2
        ALIST(LAST) = A1
        BLIST(LAST) = B1
        RLIST(MAXERR) = AREA2
        RLIST(LAST) = AREA1
        ELIST(MAXERR) = ERROR2
        ELIST(LAST) = ERROR1
C
C           CALL SUBROUTINE DQPSRT TO MAINTAIN THE DESCENDING ORDERING
C           IN THE LIST OF ERROR ESTIMATES AND SELECT THE SUBINTERVAL
C           WITH NRMAX-TH LARGEST ERROR ESTIMATE (TO BE BISECTED NEXT).
C
   30   CALL DQPSRT(LIMIT,LAST,MAXERR,ERRMAX,ELIST,IORD,NRMAX)
        IF(ERRSUM.LE.ERRBND) GO TO 115
        IF(IER.NE.0) GO TO 100
        IF(LAST.EQ.2) GO TO 80
        IF(NOEXT) GO TO 90
        ERLARG = ERLARG-ERLAST
        IF(ABS(B1-A1).GT.SMALL) ERLARG = ERLARG+ERRO12
        IF(EXTRAP) GO TO 40
C
C           TEST WHETHER THE INTERVAL TO BE BISECTED NEXT IS THE
C           SMALLEST INTERVAL.
C
        IF(ABS(BLIST(MAXERR)-ALIST(MAXERR)).GT.SMALL) GO TO 90
        EXTRAP = .TRUE.
        NRMAX = 2
   40   IF(IERRO.EQ.3.OR.ERLARG.LE.ERTEST) GO TO 60
C
C           THE SMALLEST INTERVAL HAS THE LARGEST ERROR.
C           BEFORE BISECTING DECREASE THE SUM OF THE ERRORS OVER THE
C           LARGER INTERVALS (ERLARG) AND PERFORM EXTRAPOLATION.
C
        ID = NRMAX
        JUPBND = LAST
        IF(LAST.GT.(2+LIMIT/2)) JUPBND = LIMIT+3-LAST
        DO 50 K = ID,JUPBND
          MAXERR = IORD(NRMAX)
          ERRMAX = ELIST(MAXERR)
          IF(ABS(BLIST(MAXERR)-ALIST(MAXERR)).GT.SMALL) GO TO 90
          NRMAX = NRMAX+1
   50   CONTINUE
C
C           PERFORM EXTRAPOLATION.
C
   60   NUMRL2 = NUMRL2+1
        RLIST2(NUMRL2) = AREA
        CALL DQELG(NUMRL2,RLIST2,RESEPS,ABSEPS,RES3LA,NRES)
        KTMIN = KTMIN+1
        IF(KTMIN.GT.5.AND.ABSERR.LT.0.1D-02*ERRSUM) IER = 5
        IF(ABSEPS.GE.ABSERR) GO TO 70
        KTMIN = 0
        ABSERR = ABSEPS
        RESULT = RESEPS
        CORREC = ERLARG
        ERTEST = MAX(EPSABS,EPSREL*ABS(RESEPS))
        IF(ABSERR.LE.ERTEST) GO TO 100
C
C            PREPARE BISECTION OF THE SMALLEST INTERVAL.
C
   70   IF(NUMRL2.EQ.1) NOEXT = .TRUE.
        IF(IER.EQ.5) GO TO 100
        MAXERR = IORD(1)
        ERRMAX = ELIST(MAXERR)
        NRMAX = 1
        EXTRAP = .FALSE.
        SMALL = SMALL*0.5D+00
        ERLARG = ERRSUM
        GO TO 90
   80   SMALL = 0.375D+00
        ERLARG = ERRSUM
        ERTEST = ERRBND
        RLIST2(2) = AREA
   90 CONTINUE
C
C           SET FINAL RESULT AND ERROR ESTIMATE.
C           ------------------------------------
C
  100 IF(ABSERR.EQ.OFLOW) GO TO 115
      IF((IER+IERRO).EQ.0) GO TO 110
      IF(IERRO.EQ.3) ABSERR = ABSERR+CORREC
      IF(IER.EQ.0) IER = 3
      IF(RESULT.NE.0.0D+00.AND.AREA.NE.0.0D+00)GO TO 105
      IF(ABSERR.GT.ERRSUM)GO TO 115
      IF(AREA.EQ.0.0D+00) GO TO 130
      GO TO 110
  105 IF(ABSERR/ABS(RESULT).GT.ERRSUM/ABS(AREA))GO TO 115
C
C           TEST ON DIVERGENCE
C
  110 IF(KSGN.EQ.(-1).AND.MAX(ABS(RESULT),ABS(AREA)).LE.
     1 DEFABS*0.1D-01) GO TO 130
      IF(0.1D-01.GT.(RESULT/AREA).OR.(RESULT/AREA).GT.0.1D+03.
     1OR.ERRSUM.GT.ABS(AREA)) IER = 6
      GO TO 130
C
C           COMPUTE GLOBAL INTEGRAL SUM.
C
  115 RESULT = 0.0D+00
      DO 120 K = 1,LAST
        RESULT = RESULT+RLIST(K)
  120 CONTINUE
      ABSERR = ERRSUM
  130 NEVAL = 30*LAST-15
      IF(INF.EQ.2) NEVAL = 2*NEVAL
      IF(IER.GT.2) IER=IER-1
  999 RETURN
      END
      SUBROUTINE DQELG(N,EPSTAB,RESULT,ABSERR,RES3LA,NRES)
C
C   ADDED TO DATAPLOT 12/2003.  THIS ROUTINE ADDED FOR INTERNAL
C   DATAPLOT USAGE.
C
C***BEGIN PROLOGUE  DQELG
C***REFER TO  DQAGIE,DQAGOE,DQAGPE,DQAGSE
C***ROUTINES CALLED  D1MACH
C***REVISION DATE  830518   (YYMMDD)
C***REVISION HISTORY (YYMMDD)
C   000601   Changed DMAX1/DABS to generic MAX/ABS
C***KEYWORDS  CONVERGENCE ACCELERATION,EPSILON ALGORITHM,EXTRAPOLATION
C***AUTHOR  PIESSENS, ROBERT, APPLIED MATH. AND PROGR. DIV. -
C             K. U. LEUVEN
C           DE DONCKER, ELISE, APPLIED MATH. AND PROGR. DIV. -
C             K. U. LEUVEN
C***PURPOSE  The routine determines the limit of a given sequence of
C            approximations, by means of the Epsilon algorithm of
C            P.Wynn. An estimate of the absolute error is also given.
C            The condensed Epsilon table is computed. Only those
C            elements needed for the computation of the next diagonal
C            are preserved.
C***DESCRIPTION
C
C           Epsilon algorithm
C           Standard fortran subroutine
C           Double precision version
C
C           PARAMETERS
C              N      - Integer
C                       EPSTAB(N) contains the new element in the
C                       first column of the epsilon table.
C
C              EPSTAB - Double precision
C                       Vector of dimension 52 containing the elements
C                       of the two lower diagonals of the triangular
C                       epsilon table. The elements are numbered
C                       starting at the right-hand corner of the
C                       triangle.
C
C              RESULT - Double precision
C                       Resulting approximation to the integral
C
C              ABSERR - Double precision
C                       Estimate of the absolute error computed from
C                       RESULT and the 3 previous results
C
C              RES3LA - Double precision
C                       Vector of dimension 3 containing the last 3
C                       results
C
C              NRES   - Integer
C                       Number of calls to the routine
C                       (should be zero at first call)
C***END PROLOGUE  DQELG
C
      DOUBLE PRECISION ABSERR,DELTA1,DELTA2,DELTA3,
     1  EPMACH,EPSINF,EPSTAB,ERROR,ERR1,ERR2,ERR3,E0,E1,E1ABS,E2,E3,
     2  OFLOW,RES,RESULT,RES3LA,SS,TOL1,TOL2,TOL3
      INTEGER I,IB,IB2,IE,INDX,K1,K2,K3,LIMEXP,N,NEWELM,NRES,NUM
      DIMENSION EPSTAB(52),RES3LA(3)
C
      INCLUDE 'DPCOMC.INC'
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C           LIST OF MAJOR VARIABLES
C           -----------------------
C
C           E0     - THE 4 ELEMENTS ON WHICH THE COMPUTATION OF A NEW
C           E1       ELEMENT IN THE EPSILON TABLE IS BASED
C           E2
C           E3                 E0
C                        E3    E1    NEW
C                              E2
C           NEWELM - NUMBER OF ELEMENTS TO BE COMPUTED IN THE NEW
C                    DIAGONAL
C           ERROR  - ERROR = ABS(E1-E0)+ABS(E2-E1)+ABS(NEW-E2)
C           RESULT - THE ELEMENT IN THE NEW DIAGONAL WITH LEAST VALUE
C                    OF ERROR
C
C           MACHINE DEPENDENT CONSTANTS
C           ---------------------------
C
C           EPMACH IS THE LARGEST RELATIVE SPACING.
C           OFLOW IS THE LARGEST POSITIVE MAGNITUDE.
C           LIMEXP IS THE MAXIMUM NUMBER OF ELEMENTS THE EPSILON
C           TABLE CAN CONTAIN. IF THIS NUMBER IS REACHED, THE UPPER
C           DIAGONAL OF THE EPSILON TABLE IS DELETED.
C
C***FIRST EXECUTABLE STATEMENT  DQELG
      EPMACH = D1MACH(4)
      OFLOW = D1MACH(2)
      NRES = NRES+1
      ABSERR = OFLOW
      RESULT = EPSTAB(N)
      IF(N.LT.3) GO TO 100
      LIMEXP = 50
      EPSTAB(N+2) = EPSTAB(N)
      NEWELM = (N-1)/2
      EPSTAB(N) = OFLOW
      NUM = N
      K1 = N
      DO 40 I = 1,NEWELM
        K2 = K1-1
        K3 = K1-2
        RES = EPSTAB(K1+2)
        E0 = EPSTAB(K3)
        E1 = EPSTAB(K2)
        E2 = RES
        E1ABS = ABS(E1)
        DELTA2 = E2-E1
        ERR2 = ABS(DELTA2)
        TOL2 = MAX(ABS(E2),E1ABS)*EPMACH
        DELTA3 = E1-E0
        ERR3 = ABS(DELTA3)
        TOL3 = MAX(E1ABS,ABS(E0))*EPMACH
        IF(ERR2.GT.TOL2.OR.ERR3.GT.TOL3) GO TO 10
C
C           IF E0, E1 AND E2 ARE EQUAL TO WITHIN MACHINE
C           ACCURACY, CONVERGENCE IS ASSUMED.
C           RESULT = E2
C           ABSERR = ABS(E1-E0)+ABS(E2-E1)
C
        RESULT = RES
        ABSERR = ERR2+ERR3
C ***JUMP OUT OF DO-LOOP
        GO TO 100
   10   E3 = EPSTAB(K1)
        EPSTAB(K1) = E1
        DELTA1 = E1-E3
        ERR1 = ABS(DELTA1)
        TOL1 = MAX(E1ABS,ABS(E3))*EPMACH
C
C           IF TWO ELEMENTS ARE VERY CLOSE TO EACH OTHER, OMIT
C           A PART OF THE TABLE BY ADJUSTING THE VALUE OF N
C
        IF(ERR1.LE.TOL1.OR.ERR2.LE.TOL2.OR.ERR3.LE.TOL3) GO TO 20
        SS = 0.1D+01/DELTA1+0.1D+01/DELTA2-0.1D+01/DELTA3
        EPSINF = ABS(SS*E1)
C
C           TEST TO DETECT IRREGULAR BEHAVIOUR IN THE TABLE, AND
C           EVENTUALLY OMIT A PART OF THE TABLE ADJUSTING THE VALUE
C           OF N.
C
        IF(EPSINF.GT.0.1D-03) GO TO 30
   20   N = I+I-1
C ***JUMP OUT OF DO-LOOP
        GO TO 50
C
C           COMPUTE A NEW ELEMENT AND EVENTUALLY ADJUST
C           THE VALUE OF RESULT.
C
   30   RES = E1+0.1D+01/SS
        EPSTAB(K1) = RES
        K1 = K1-2
        ERROR = ERR2+ABS(RES-E2)+ERR3
        IF(ERROR.GT.ABSERR) GO TO 40
        ABSERR = ERROR
        RESULT = RES
   40 CONTINUE
C
C           SHIFT THE TABLE.
C
   50 IF(N.EQ.LIMEXP) N = 2*(LIMEXP/2)-1
      IB = 1
      IF((NUM/2)*2.EQ.NUM) IB = 2
      IE = NEWELM+1
      DO 60 I=1,IE
        IB2 = IB+2
        EPSTAB(IB) = EPSTAB(IB2)
        IB = IB2
   60 CONTINUE
      IF(NUM.EQ.N) GO TO 80
      INDX = NUM-N+1
      DO 70 I = 1,N
        EPSTAB(I)= EPSTAB(INDX)
        INDX = INDX+1
   70 CONTINUE
   80 IF(NRES.GE.4) GO TO 90
      RES3LA(NRES) = RESULT
      ABSERR = OFLOW
      GO TO 100
C
C           COMPUTE ERROR ESTIMATE
C
   90 ABSERR = ABS(RESULT-RES3LA(3))+ABS(RESULT-RES3LA(2))
     1  +ABS(RESULT-RES3LA(1))
      RES3LA(1) = RES3LA(2)
      RES3LA(2) = RES3LA(3)
      RES3LA(3) = RESULT
  100 ABSERR = MAX(ABSERR,0.5D+01*EPMACH*ABS(RESULT))
      RETURN
      END
      SUBROUTINE DQK15(F,A,B,RESULT,ABSERR,RESABS,RESASC)
C
C   ADDED TO DATAPLOT 12/2003.  THIS ROUTINE ADDED FOR INTERNAL
C   DATAPLOT USAGE.
C
C***BEGIN PROLOGUE  DQK15
C***DATE WRITTEN   800101   (YYMMDD)
C***REVISION DATE  830518   (YYMMDD)
C***REVISION HISTORY (YYMMDD)
C   000601   Changed DMAX1/DMIN1/DABS to generic MAX/MIN/ABS
C***CATEGORY NO.  H2A1A2
C***KEYWORDS  15-POINT GAUSS-KRONROD RULES
C***AUTHOR  PIESSENS, ROBERT, APPLIED MATH. AND PROGR. DIV. -
C             K. U. LEUVEN
C           DE DONCKER, ELISE, APPLIED MATH. AND PROGR. DIV. -
C             K. U. LEUVEN
C***PURPOSE  To compute I = Integral of F over (A,B), with error
C                           estimate
C                       J = integral of ABS(F) over (A,B)
C***DESCRIPTION
C
C           Integration rules
C           Standard fortran subroutine
C           Double precision version
C
C           PARAMETERS
C            ON ENTRY
C              F      - Double precision
C                       Function subprogram defining the integrand
C                       FUNCTION F(X). The actual name for F needs to be
C                       Declared E X T E R N A L in the calling program.
C
C              A      - Double precision
C                       Lower limit of integration
C
C              B      - Double precision
C                       Upper limit of integration
C
C            ON RETURN
C              RESULT - Double precision
C                       Approximation to the integral I
C                       Result is computed by applying the 15-POINT
C                       KRONROD RULE (RESK) obtained by optimal addition
C                       of abscissae to the7-POINT GAUSS RULE(RESG).
C
C              ABSERR - Double precision
C                       Estimate of the modulus of the absolute error,
C                       which should not exceed ABS(I-RESULT)
C
C              RESABS - Double precision
C                       Approximation to the integral J
C
C              RESASC - Double precision
C                       Approximation to the integral of ABS(F-I/(B-A))
C                       over (A,B)
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH
C***END PROLOGUE  DQK15
C
C
      INCLUDE 'DPCOMC.INC'
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      DOUBLE PRECISION A,ABSC,ABSERR,B,CENTR,DHLGTH,
     1  EPMACH,F,FC,FSUM,FVAL1,FVAL2,FV1,FV2,HLGTH,RESABS,RESASC,
     2  RESG,RESK,RESKH,RESULT,UFLOW,WG,WGK,XGK
      INTEGER J,JTW,JTWM1
      EXTERNAL F
C
      DIMENSION FV1(7),FV2(7),WG(4),WGK(8),XGK(8)
C
C           THE ABSCISSAE AND WEIGHTS ARE GIVEN FOR THE INTERVAL (-1,1).
C           BECAUSE OF SYMMETRY ONLY THE POSITIVE ABSCISSAE AND THEIR
C           CORRESPONDING WEIGHTS ARE GIVEN.
C
C           XGK    - ABSCISSAE OF THE 15-POINT KRONROD RULE
C                    XGK(2), XGK(4), ...  ABSCISSAE OF THE 7-POINT
C                    GAUSS RULE
C                    XGK(1), XGK(3), ...  ABSCISSAE WHICH ARE OPTIMALLY
C                    ADDED TO THE 7-POINT GAUSS RULE
C
C           WGK    - WEIGHTS OF THE 15-POINT KRONROD RULE
C
C           WG     - WEIGHTS OF THE 7-POINT GAUSS RULE
C
C
C GAUSS QUADRATURE WEIGHTS AND KRONRON QUADRATURE ABSCISSAE AND WEIGHTS
C AS EVALUATED WITH 80 DECIMAL DIGIT ARITHMETIC BY L. W. FULLERTON,
C BELL LABS, NOV. 1981.
C
      DATA WG  (  1) / 0.1294849661 6886969327 0611432679 082 D0 /
      DATA WG  (  2) / 0.2797053914 8927666790 1467771423 780 D0 /
      DATA WG  (  3) / 0.3818300505 0511894495 0369775488 975 D0 /
      DATA WG  (  4) / 0.4179591836 7346938775 5102040816 327 D0 /
C
      DATA XGK (  1) / 0.9914553711 2081263920 6854697526 329 D0 /
      DATA XGK (  2) / 0.9491079123 4275852452 6189684047 851 D0 /
      DATA XGK (  3) / 0.8648644233 5976907278 9712788640 926 D0 /
      DATA XGK (  4) / 0.7415311855 9939443986 3864773280 788 D0 /
      DATA XGK (  5) / 0.5860872354 6769113029 4144838258 730 D0 /
      DATA XGK (  6) / 0.4058451513 7739716690 6606412076 961 D0 /
      DATA XGK (  7) / 0.2077849550 0789846760 0689403773 245 D0 /
      DATA XGK (  8) / 0.0000000000 0000000000 0000000000 000 D0 /
C
      DATA WGK (  1) / 0.0229353220 1052922496 3732008058 970 D0 /
      DATA WGK (  2) / 0.0630920926 2997855329 0700663189 204 D0 /
      DATA WGK (  3) / 0.1047900103 2225018383 9876322541 518 D0 /
      DATA WGK (  4) / 0.1406532597 1552591874 5189590510 238 D0 /
      DATA WGK (  5) / 0.1690047266 3926790282 6583426598 550 D0 /
      DATA WGK (  6) / 0.1903505780 6478540991 3256402421 014 D0 /
      DATA WGK (  7) / 0.2044329400 7529889241 4161999234 649 D0 /
      DATA WGK (  8) / 0.2094821410 8472782801 2999174891 714 D0 /
C
C
C           LIST OF MAJOR VARIABLES
C           -----------------------
C
C           CENTR  - MID POINT OF THE INTERVAL
C           HLGTH  - HALF-LENGTH OF THE INTERVAL
C           ABSC   - ABSCISSA
C           FVAL*  - FUNCTION VALUE
C           RESG   - RESULT OF THE 7-POINT GAUSS FORMULA
C           RESK   - RESULT OF THE 15-POINT KRONROD FORMULA
C           RESKH  - APPROXIMATION TO THE MEAN VALUE OF F OVER (A,B),
C                    I.E. TO I/(B-A)
C
C           MACHINE DEPENDENT CONSTANTS
C           ---------------------------
C
C           EPMACH IS THE LARGEST RELATIVE SPACING.
C           UFLOW IS THE SMALLEST POSITIVE MAGNITUDE.
C
C***FIRST EXECUTABLE STATEMENT  DQK15
      EPMACH = D1MACH(4)
      UFLOW = D1MACH(1)
C
      CENTR = 0.5D+00*(A+B)
      HLGTH = 0.5D+00*(B-A)
      DHLGTH = ABS(HLGTH)
C
C           COMPUTE THE 15-POINT KRONROD APPROXIMATION TO
C           THE INTEGRAL, AND ESTIMATE THE ABSOLUTE ERROR.
C
      FC = F(CENTR)
      RESG = FC*WG(4)
      RESK = FC*WGK(8)
      RESABS = ABS(RESK)
      DO 10 J=1,3
        JTW = J*2
        ABSC = HLGTH*XGK(JTW)
        FVAL1 = F(CENTR-ABSC)
        FVAL2 = F(CENTR+ABSC)
        FV1(JTW) = FVAL1
        FV2(JTW) = FVAL2
        FSUM = FVAL1+FVAL2
        RESG = RESG+WG(J)*FSUM
        RESK = RESK+WGK(JTW)*FSUM
        RESABS = RESABS+WGK(JTW)*(ABS(FVAL1)+ABS(FVAL2))
   10 CONTINUE
      DO 15 J = 1,4
        JTWM1 = J*2-1
        ABSC = HLGTH*XGK(JTWM1)
        FVAL1 = F(CENTR-ABSC)
        FVAL2 = F(CENTR+ABSC)
        FV1(JTWM1) = FVAL1
        FV2(JTWM1) = FVAL2
        FSUM = FVAL1+FVAL2
        RESK = RESK+WGK(JTWM1)*FSUM
        RESABS = RESABS+WGK(JTWM1)*(ABS(FVAL1)+ABS(FVAL2))
   15 CONTINUE
      RESKH = RESK*0.5D+00
      RESASC = WGK(8)*ABS(FC-RESKH)
      DO 20 J=1,7
        RESASC = RESASC+WGK(J)*(ABS(FV1(J)-RESKH)+ABS(FV2(J)-RESKH))
   20 CONTINUE
      RESULT = RESK*HLGTH
      RESABS = RESABS*DHLGTH
      RESASC = RESASC*DHLGTH
      ABSERR = ABS((RESK-RESG)*HLGTH)
      IF(RESASC.NE.0.0D+00.AND.ABSERR.NE.0.0D+00)
     1  ABSERR = RESASC*MIN(0.1D+01,(0.2D+03*ABSERR/RESASC)**1.5D+00)
      IF(RESABS.GT.UFLOW/(0.5D+02*EPMACH)) ABSERR = MAX
     1  ((EPMACH*0.5D+02)*RESABS,ABSERR)
      RETURN
      END
      SUBROUTINE DQK15I(F,BOUN,INF,A,B,RESULT,ABSERR,RESABS,RESASC)
C
C   ADDED TO DATAPLOT 12/2003.  THIS ROUTINE ADDED FOR INTERNAL
C   DATAPLOT USAGE.
C
C***BEGIN PROLOGUE  DQK15I
C***DATE WRITTEN   800101   (YYMMDD)
C***REVISION DATE  830518   (YYMMDD)
C***REVISION HISTORY (YYMMDD)
C   000601   Changed DMAX1/DMIN1/DABS to generic MAX/MIN/ABS
C***CATEGORY NO.  H2A3A2,H2A4A2
C***KEYWORDS  15-POINT TRANSFORMED GAUSS-KRONROD RULES
C***AUTHOR  PIESSENS, ROBERT, APPLIED MATH. AND PROGR. DIV. -
C             K. U. LEUVEN
C           DE DONCKER, ELISE, APPLIED MATH. AND PROGR. DIV. -
C             K. U. LEUVEN
C***PURPOSE  The original (infinite integration range is mapped
C            onto the interval (0,1) and (A,B) is a part of (0,1).
C            it is the purpose to compute
C            I = Integral of transformed integrand over (A,B),
C            J = Integral of ABS(Transformed Integrand) over (A,B).
C***DESCRIPTION
C
C           Integration Rule
C           Standard Fortran subroutine
C           Double precision version
C
C           PARAMETERS
C            ON ENTRY
C              F      - Double precision
C                       Fuction subprogram defining the integrand
C                       FUNCTION F(X). The actual name for F needs to be
C                       Declared E X T E R N A L in the calling program.
C
C              BOUN   - Double precision
C                       Finite bound of original integration
C                       Range (SET TO ZERO IF INF = +2)
C
C              INF    - Integer
C                       If INF = -1, the original interval is
C                                   (-INFINITY,BOUND),
C                       If INF = +1, the original interval is
C                                   (BOUND,+INFINITY),
C                       If INF = +2, the original interval is
C                                   (-INFINITY,+INFINITY) AND
C                       The integral is computed as the sum of two
C                       integrals, one over (-INFINITY,0) and one over
C                       (0,+INFINITY).
C
C              A      - Double precision
C                       Lower limit for integration over subrange
C                       of (0,1)
C
C              B      - Double precision
C                       Upper limit for integration over subrange
C                       of (0,1)
C
C            ON RETURN
C              RESULT - Double precision
C                       Approximation to the integral I
C                       Result is computed by applying the 15-POINT
C                       KRONROD RULE(RESK) obtained by optimal addition
C                       of abscissae to the 7-POINT GAUSS RULE(RESG).
C
C              ABSERR - Double precision
C                       Estimate of the modulus of the absolute error,
C                       WHICH SHOULD EQUAL or EXCEED ABS(I-RESULT)
C
C              RESABS - Double precision
C                       Approximation to the integral J
C
C              RESASC - Double precision
C                       Approximation to the integral of
C                       ABS((TRANSFORMED INTEGRAND)-I/(B-A)) over (A,B)
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH
C***END PROLOGUE  DQK15I
C
      DOUBLE PRECISION A,ABSC,ABSC1,ABSC2,ABSERR,B,BOUN,CENTR,DINF,
     1  EPMACH,F,FC,FSUM,FVAL1,FVAL2,FV1,FV2,HLGTH,
     2  RESABS,RESASC,RESG,RESK,RESKH,RESULT,TABSC1,TABSC2,UFLOW,WG,WGK,
     3  XGK
      INTEGER INF,J
C
      INCLUDE 'DPCOMC.INC'
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      EXTERNAL F
C
      DIMENSION FV1(7),FV2(7),XGK(8),WGK(8),WG(8)
C
C           THE ABSCISSAE AND WEIGHTS ARE SUPPLIED FOR THE INTERVAL
C           (-1,1).  BECAUSE OF SYMMETRY ONLY THE POSITIVE ABSCISSAE AND
C           THEIR CORRESPONDING WEIGHTS ARE GIVEN.
C
C           XGK    - ABSCISSAE OF THE 15-POINT KRONROD RULE
C                    XGK(2), XGK(4), ... ABSCISSAE OF THE 7-POINT
C                    GAUSS RULE
C                    XGK(1), XGK(3), ...  ABSCISSAE WHICH ARE OPTIMALLY
C                    ADDED TO THE 7-POINT GAUSS RULE
C
C           WGK    - WEIGHTS OF THE 15-POINT KRONROD RULE
C
C           WG     - WEIGHTS OF THE 7-POINT GAUSS RULE, CORRESPONDING
C                    TO THE ABSCISSAE XGK(2), XGK(4), ...
C                    WG(1), WG(3), ... ARE SET TO ZERO.
C
      DATA XGK(1),XGK(2),XGK(3),XGK(4),XGK(5),XGK(6),XGK(7),XGK(8)/
     1     0.9914553711208126D+00,     0.9491079123427585D+00,
     2     0.8648644233597691D+00,     0.7415311855993944D+00,
     3     0.5860872354676911D+00,     0.4058451513773972D+00,
     4     0.2077849550078985D+00,     0.0000000000000000D+00/
C
      DATA WGK(1),WGK(2),WGK(3),WGK(4),WGK(5),WGK(6),WGK(7),WGK(8)/
     1     0.2293532201052922D-01,     0.6309209262997855D-01,
     2     0.1047900103222502D+00,     0.1406532597155259D+00,
     3     0.1690047266392679D+00,     0.1903505780647854D+00,
     4     0.2044329400752989D+00,     0.2094821410847278D+00/
C
      DATA WG(1),WG(2),WG(3),WG(4),WG(5),WG(6),WG(7),WG(8)/
     1     0.0000000000000000D+00,     0.1294849661688697D+00,
     2     0.0000000000000000D+00,     0.2797053914892767D+00,
     3     0.0000000000000000D+00,     0.3818300505051189D+00,
     4     0.0000000000000000D+00,     0.4179591836734694D+00/
C
C
C           LIST OF MAJOR VARIABLES
C           -----------------------
C
C           CENTR  - MID POINT OF THE INTERVAL
C           HLGTH  - HALF-LENGTH OF THE INTERVAL
C           ABSC*  - ABSCISSA
C           TABSC* - TRANSFORMED ABSCISSA
C           FVAL*  - FUNCTION VALUE
C           RESG   - RESULT OF THE 7-POINT GAUSS FORMULA
C           RESK   - RESULT OF THE 15-POINT KRONROD FORMULA
C           RESKH  - APPROXIMATION TO THE MEAN VALUE OF THE TRANSFORMED
C                    INTEGRAND OVER (A,B), I.E. TO I/(B-A)
C
C           MACHINE DEPENDENT CONSTANTS
C           ---------------------------
C
C           EPMACH IS THE LARGEST RELATIVE SPACING.
C           UFLOW IS THE SMALLEST POSITIVE MAGNITUDE.
C
C***FIRST EXECUTABLE STATEMENT  DQK15I
      EPMACH = D1MACH(4)
      UFLOW = D1MACH(1)
      DINF = MIN0(1,INF)
C
      CENTR = 0.5D+00*(A+B)
      HLGTH = 0.5D+00*(B-A)
      TABSC1 = BOUN+DINF*(0.1D+01-CENTR)/CENTR
      FVAL1 = F(TABSC1)
      IF(INF.EQ.2) FVAL1 = FVAL1+F(-TABSC1)
      FC = (FVAL1/CENTR)/CENTR
C
C           COMPUTE THE 15-POINT KRONROD APPROXIMATION TO
C           THE INTEGRAL, AND ESTIMATE THE ERROR.
C
      RESG = WG(8)*FC
      RESK = WGK(8)*FC
      RESABS = ABS(RESK)
      DO 10 J=1,7
        ABSC = HLGTH*XGK(J)
        ABSC1 = CENTR-ABSC
        ABSC2 = CENTR+ABSC
        TABSC1 = BOUN+DINF*(0.1D+01-ABSC1)/ABSC1
        TABSC2 = BOUN+DINF*(0.1D+01-ABSC2)/ABSC2
        FVAL1 = F(TABSC1)
        FVAL2 = F(TABSC2)
        IF(INF.EQ.2) FVAL1 = FVAL1+F(-TABSC1)
        IF(INF.EQ.2) FVAL2 = FVAL2+F(-TABSC2)
        FVAL1 = (FVAL1/ABSC1)/ABSC1
        FVAL2 = (FVAL2/ABSC2)/ABSC2
        FV1(J) = FVAL1
        FV2(J) = FVAL2
        FSUM = FVAL1+FVAL2
        RESG = RESG+WG(J)*FSUM
        RESK = RESK+WGK(J)*FSUM
        RESABS = RESABS+WGK(J)*(ABS(FVAL1)+ABS(FVAL2))
   10 CONTINUE
      RESKH = RESK*0.5D+00
      RESASC = WGK(8)*ABS(FC-RESKH)
      DO 20 J=1,7
        RESASC = RESASC+WGK(J)*(ABS(FV1(J)-RESKH)+ABS(FV2(J)-RESKH))
   20 CONTINUE
      RESULT = RESK*HLGTH
      RESASC = RESASC*HLGTH
      RESABS = RESABS*HLGTH
      ABSERR = ABS((RESK-RESG)*HLGTH)
      IF(RESASC.NE.0.0D+00.AND.ABSERR.NE.0.D0) ABSERR = RESASC*
     1 MIN(0.1D+01,(0.2D+03*ABSERR/RESASC)**1.5D+00)
      IF(RESABS.GT.UFLOW/(0.5D+02*EPMACH)) ABSERR = MAX
     1 ((EPMACH*0.5D+02)*RESABS,ABSERR)
      RETURN
      END
      SUBROUTINE DQK21(F,A,B,RESULT,ABSERR,RESABS,RESASC)
C
C   ADDED TO DATAPLOT 12/2003.  THIS ROUTINE ADDED FOR INTERNAL
C   DATAPLOT USAGE.
C
C***BEGIN PROLOGUE  DQK21
C***DATE WRITTEN   800101   (YYMMDD)
C***REVISION DATE  830518   (YYMMDD)
C***REVISION HISTORY (YYMMDD)
C   000601   Changed DMAX1/DMIN1/DABS to generic MAX/MIN/ABS
C***CATEGORY NO.  H2A1A2
C***KEYWORDS  21-POINT GAUSS-KRONROD RULES
C***AUTHOR  PIESSENS, ROBERT, APPLIED MATH. AND PROGR. DIV. -
C             K. U. LEUVEN
C           DE DONCKER, ELISE, APPLIED MATH. AND PROGR. DIV. -
C             K. U. LEUVEN
C***PURPOSE  To compute I = Integral of F over (A,B), with error
C                           estimate
C                       J = Integral of ABS(F) over (A,B)
C***DESCRIPTION
C
C           Integration rules
C           Standard fortran subroutine
C           Double precision version
C
C           PARAMETERS
C            ON ENTRY
C              F      - Double precision
C                       Function subprogram defining the integrand
C                       FUNCTION F(X). The actual name for F needs to be
C                       Declared E X T E R N A L in the driver program.
C
C              A      - Double precision
C                       Lower limit of integration
C
C              B      - Double precision
C                       Upper limit of integration
C
C            ON RETURN
C              RESULT - Double precision
C                       Approximation to the integral I
C                       RESULT is computed by applying the 21-POINT
C                       KRONROD RULE (RESK) obtained by optimal addition
C                       of abscissae to the 10-POINT GAUSS RULE (RESG).
C
C              ABSERR - Double precision
C                       Estimate of the modulus of the absolute error,
C                       which should not exceed ABS(I-RESULT)
C
C              RESABS - Double precision
C                       Approximation to the integral J
C
C              RESASC - Double precision
C                       Approximation to the integral of ABS(F-I/(B-A))
C                       over (A,B)
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH
C***END PROLOGUE  DQK21
C
      INCLUDE 'DPCOMC.INC'
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      DOUBLE PRECISION A,ABSC,ABSERR,B,CENTR,DHLGTH,
     1  EPMACH,F,FC,FSUM,FVAL1,FVAL2,FV1,FV2,HLGTH,RESABS,RESASC,
     2  RESG,RESK,RESKH,RESULT,UFLOW,WG,WGK,XGK
      INTEGER J,JTW,JTWM1
      EXTERNAL F
C
      DIMENSION FV1(10),FV2(10),WG(5),WGK(11),XGK(11)
C
C           THE ABSCISSAE AND WEIGHTS ARE GIVEN FOR THE INTERVAL (-1,1).
C           BECAUSE OF SYMMETRY ONLY THE POSITIVE ABSCISSAE AND THEIR
C           CORRESPONDING WEIGHTS ARE GIVEN.
C
C           XGK    - ABSCISSAE OF THE 21-POINT KRONROD RULE
C                    XGK(2), XGK(4), ...  ABSCISSAE OF THE 10-POINT
C                    GAUSS RULE
C                    XGK(1), XGK(3), ...  ABSCISSAE WHICH ARE OPTIMALLY
C                    ADDED TO THE 10-POINT GAUSS RULE
C
C           WGK    - WEIGHTS OF THE 21-POINT KRONROD RULE
C
C           WG     - WEIGHTS OF THE 10-POINT GAUSS RULE
C
C
C GAUSS QUADRATURE WEIGHTS AND KRONRON QUADRATURE ABSCISSAE AND WEIGHTS
C AS EVALUATED WITH 80 DECIMAL DIGIT ARITHMETIC BY L. W. FULLERTON,
C BELL LABS, NOV. 1981.
C
      DATA WG  (  1) / 0.0666713443 0868813759 3568809893 332 D0 /
      DATA WG  (  2) / 0.1494513491 5058059314 5776339657 697 D0 /
      DATA WG  (  3) / 0.2190863625 1598204399 5534934228 163 D0 /
      DATA WG  (  4) / 0.2692667193 0999635509 1226921569 469 D0 /
      DATA WG  (  5) / 0.2955242247 1475287017 3892994651 338 D0 /
C
      DATA XGK (  1) / 0.9956571630 2580808073 5527280689 003 D0 /
      DATA XGK (  2) / 0.9739065285 1717172007 7964012084 452 D0 /
      DATA XGK (  3) / 0.9301574913 5570822600 1207180059 508 D0 /
      DATA XGK (  4) / 0.8650633666 8898451073 2096688423 493 D0 /
      DATA XGK (  5) / 0.7808177265 8641689706 3717578345 042 D0 /
      DATA XGK (  6) / 0.6794095682 9902440623 4327365114 874 D0 /
      DATA XGK (  7) / 0.5627571346 6860468333 9000099272 694 D0 /
      DATA XGK (  8) / 0.4333953941 2924719079 9265943165 784 D0 /
      DATA XGK (  9) / 0.2943928627 0146019813 1126603103 866 D0 /
      DATA XGK ( 10) / 0.1488743389 8163121088 4826001129 720 D0 /
      DATA XGK ( 11) / 0.0000000000 0000000000 0000000000 000 D0 /
C
      DATA WGK (  1) / 0.0116946388 6737187427 8064396062 192 D0 /
      DATA WGK (  2) / 0.0325581623 0796472747 8818972459 390 D0 /
      DATA WGK (  3) / 0.0547558965 7435199603 1381300244 580 D0 /
      DATA WGK (  4) / 0.0750396748 1091995276 7043140916 190 D0 /
      DATA WGK (  5) / 0.0931254545 8369760553 5065465083 366 D0 /
      DATA WGK (  6) / 0.1093871588 0229764189 9210590325 805 D0 /
      DATA WGK (  7) / 0.1234919762 6206585107 7958109831 074 D0 /
      DATA WGK (  8) / 0.1347092173 1147332592 8054001771 707 D0 /
      DATA WGK (  9) / 0.1427759385 7706008079 7094273138 717 D0 /
      DATA WGK ( 10) / 0.1477391049 0133849137 4841515972 068 D0 /
      DATA WGK ( 11) / 0.1494455540 0291690566 4936468389 821 D0 /
C
C
C           LIST OF MAJOR VARIABLES
C           -----------------------
C
C           CENTR  - MID POINT OF THE INTERVAL
C           HLGTH  - HALF-LENGTH OF THE INTERVAL
C           ABSC   - ABSCISSA
C           FVAL*  - FUNCTION VALUE
C           RESG   - RESULT OF THE 10-POINT GAUSS FORMULA
C           RESK   - RESULT OF THE 21-POINT KRONROD FORMULA
C           RESKH  - APPROXIMATION TO THE MEAN VALUE OF F OVER (A,B),
C                    I.E. TO I/(B-A)
C
C
C           MACHINE DEPENDENT CONSTANTS
C           ---------------------------
C
C           EPMACH IS THE LARGEST RELATIVE SPACING.
C           UFLOW IS THE SMALLEST POSITIVE MAGNITUDE.
C
C***FIRST EXECUTABLE STATEMENT  DQK21
      EPMACH = D1MACH(4)
      UFLOW = D1MACH(1)
C
      CENTR = 0.5D+00*(A+B)
      HLGTH = 0.5D+00*(B-A)
      DHLGTH = ABS(HLGTH)
C
C           COMPUTE THE 21-POINT KRONROD APPROXIMATION TO
C           THE INTEGRAL, AND ESTIMATE THE ABSOLUTE ERROR.
C
      RESG = 0.0D+00
      FC = F(CENTR)
      RESK = WGK(11)*FC
      RESABS = ABS(RESK)
      DO 10 J=1,5
        JTW = 2*J
        ABSC = HLGTH*XGK(JTW)
        FVAL1 = F(CENTR-ABSC)
        FVAL2 = F(CENTR+ABSC)
        FV1(JTW) = FVAL1
        FV2(JTW) = FVAL2
        FSUM = FVAL1+FVAL2
        RESG = RESG+WG(J)*FSUM
        RESK = RESK+WGK(JTW)*FSUM
        RESABS = RESABS+WGK(JTW)*(ABS(FVAL1)+ABS(FVAL2))
   10 CONTINUE
      DO 15 J = 1,5
        JTWM1 = 2*J-1
        ABSC = HLGTH*XGK(JTWM1)
        FVAL1 = F(CENTR-ABSC)
        FVAL2 = F(CENTR+ABSC)
        FV1(JTWM1) = FVAL1
        FV2(JTWM1) = FVAL2
        FSUM = FVAL1+FVAL2
        RESK = RESK+WGK(JTWM1)*FSUM
        RESABS = RESABS+WGK(JTWM1)*(ABS(FVAL1)+ABS(FVAL2))
   15 CONTINUE
      RESKH = RESK*0.5D+00
      RESASC = WGK(11)*ABS(FC-RESKH)
      DO 20 J=1,10
        RESASC = RESASC+WGK(J)*(ABS(FV1(J)-RESKH)+ABS(FV2(J)-RESKH))
   20 CONTINUE
      RESULT = RESK*HLGTH
      RESABS = RESABS*DHLGTH
      RESASC = RESASC*DHLGTH
      ABSERR = ABS((RESK-RESG)*HLGTH)
      IF(RESASC.NE.0.0D+00.AND.ABSERR.NE.0.0D+00)
     1  ABSERR = RESASC*MIN(0.1D+01,(0.2D+03*ABSERR/RESASC)**1.5D+00)
      IF(RESABS.GT.UFLOW/(0.5D+02*EPMACH)) ABSERR = MAX
     1  ((EPMACH*0.5D+02)*RESABS,ABSERR)
      RETURN
      END
      SUBROUTINE DQK31(F,A,B,RESULT,ABSERR,RESABS,RESASC)
C
C   ADDED TO DATAPLOT 12/2003.  THIS ROUTINE ADDED FOR INTERNAL
C   DATAPLOT USAGE.
C
C***BEGIN PROLOGUE  DQK31
C***DATE WRITTEN   800101   (YYMMDD)
C***REVISION DATE  830518   (YYMMDD)
C***REVISION HISTORY (YYMMDD)
C   000601   Changed DMAX1/DMIN1/DABS to generic MAX/MIN/ABS
C***CATEGORY NO.  H2A1A2
C***KEYWORDS  31-POINT GAUSS-KRONROD RULES
C***AUTHOR  PIESSENS, ROBERT, APPLIED MATH. AND PROGR. DIV. -
C             K. U. LEUVEN
C           DE DONCKER, ELISE, APPLIED MATH. AND PROGR. DIV. -
C             K. U. LEUVEN
C***PURPOSE  To compute I = Integral of F over (A,B) with error
C                           estimate
C                       J = Integral of ABS(F) over (A,B)
C***DESCRIPTION
C
C           Integration rules
C           Standard fortran subroutine
C           Double precision version
C
C           PARAMETERS
C            ON ENTRY
C              F      - Double precision
C                       Function subprogram defining the integrand
C                       FUNCTION F(X). The actual name for F needs to be
C                       Declared E X T E R N A L in the calling program.
C
C              A      - Double precision
C                       Lower limit of integration
C
C              B      - Double precision
C                       Upper limit of integration
C
C            ON RETURN
C              RESULT - Double precision
C                       Approximation to the integral I
C                       RESULT is computed by applying the 31-POINT
C                       GAUSS-KRONROD RULE (RESK), obtained by optimal
C                       addition of abscissae to the 15-POINT GAUSS
C                       RULE (RESG).
C
C              ABSERR - Double precison
C                       Estimate of the modulus of the modulus,
C                       which should not exceed ABS(I-RESULT)
C
C              RESABS - Double precision
C                       Approximation to the integral J
C
C              RESASC - Double precision
C                       Approximation to the integral of ABS(F-I/(B-A))
C                       over (A,B)
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH
C***END PROLOGUE  DQK31
C
      INCLUDE 'DPCOMC.INC'
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      DOUBLE PRECISION A,ABSC,ABSERR,B,CENTR,DHLGTH,
     1  EPMACH,F,FC,FSUM,FVAL1,FVAL2,FV1,FV2,HLGTH,RESABS,RESASC,
     2  RESG,RESK,RESKH,RESULT,UFLOW,WG,WGK,XGK
      INTEGER J,JTW,JTWM1
      EXTERNAL F
C
      DIMENSION FV1(15),FV2(15),XGK(16),WGK(16),WG(8)
C
C           THE ABSCISSAE AND WEIGHTS ARE GIVEN FOR THE INTERVAL (-1,1).
C           BECAUSE OF SYMMETRY ONLY THE POSITIVE ABSCISSAE AND THEIR
C           CORRESPONDING WEIGHTS ARE GIVEN.
C
C           XGK    - ABSCISSAE OF THE 31-POINT KRONROD RULE
C                    XGK(2), XGK(4), ...  ABSCISSAE OF THE 15-POINT
C                    GAUSS RULE
C                    XGK(1), XGK(3), ...  ABSCISSAE WHICH ARE OPTIMALLY
C                    ADDED TO THE 15-POINT GAUSS RULE
C
C           WGK    - WEIGHTS OF THE 31-POINT KRONROD RULE
C
C           WG     - WEIGHTS OF THE 15-POINT GAUSS RULE
C
C
C GAUSS QUADRATURE WEIGHTS AND KRONRON QUADRATURE ABSCISSAE AND WEIGHTS
C AS EVALUATED WITH 80 DECIMAL DIGIT ARITHMETIC BY L. W. FULLERTON,
C BELL LABS, NOV. 1981.
C
      DATA WG  (  1) / 0.0307532419 9611726835 4628393577 204 D0 /
      DATA WG  (  2) / 0.0703660474 8810812470 9267416450 667 D0 /
      DATA WG  (  3) / 0.1071592204 6717193501 1869546685 869 D0 /
      DATA WG  (  4) / 0.1395706779 2615431444 7804794511 028 D0 /
      DATA WG  (  5) / 0.1662692058 1699393355 3200860481 209 D0 /
      DATA WG  (  6) / 0.1861610000 1556221102 6800561866 423 D0 /
      DATA WG  (  7) / 0.1984314853 2711157645 6118326443 839 D0 /
      DATA WG  (  8) / 0.2025782419 2556127288 0620199967 519 D0 /
C
      DATA XGK (  1) / 0.9980022986 9339706028 5172840152 271 D0 /
      DATA XGK (  2) / 0.9879925180 2048542848 9565718586 613 D0 /
      DATA XGK (  3) / 0.9677390756 7913913425 7347978784 337 D0 /
      DATA XGK (  4) / 0.9372733924 0070590430 7758947710 209 D0 /
      DATA XGK (  5) / 0.8972645323 4408190088 2509656454 496 D0 /
      DATA XGK (  6) / 0.8482065834 1042721620 0648320774 217 D0 /
      DATA XGK (  7) / 0.7904185014 4246593296 7649294817 947 D0 /
      DATA XGK (  8) / 0.7244177313 6017004741 6186054613 938 D0 /
      DATA XGK (  9) / 0.6509967412 9741697053 3735895313 275 D0 /
      DATA XGK ( 10) / 0.5709721726 0853884753 7226737253 911 D0 /
      DATA XGK ( 11) / 0.4850818636 4023968069 3655740232 351 D0 /
      DATA XGK ( 12) / 0.3941513470 7756336989 7207370981 045 D0 /
      DATA XGK ( 13) / 0.2991800071 5316881216 6780024266 389 D0 /
      DATA XGK ( 14) / 0.2011940939 9743452230 0628303394 596 D0 /
      DATA XGK ( 15) / 0.1011420669 1871749902 7074231447 392 D0 /
      DATA XGK ( 16) / 0.0000000000 0000000000 0000000000 000 D0 /
C
      DATA WGK (  1) / 0.0053774798 7292334898 7792051430 128 D0 /
      DATA WGK (  2) / 0.0150079473 2931612253 8374763075 807 D0 /
      DATA WGK (  3) / 0.0254608473 2671532018 6874001019 653 D0 /
      DATA WGK (  4) / 0.0353463607 9137584622 2037948478 360 D0 /
      DATA WGK (  5) / 0.0445897513 2476487660 8227299373 280 D0 /
      DATA WGK (  6) / 0.0534815246 9092808726 5343147239 430 D0 /
      DATA WGK (  7) / 0.0620095678 0067064028 5139230960 803 D0 /
      DATA WGK (  8) / 0.0698541213 1872825870 9520077099 147 D0 /
      DATA WGK (  9) / 0.0768496807 5772037889 4432777482 659 D0 /
      DATA WGK ( 10) / 0.0830805028 2313302103 8289247286 104 D0 /
      DATA WGK ( 11) / 0.0885644430 5621177064 7275443693 774 D0 /
      DATA WGK ( 12) / 0.0931265981 7082532122 5486872747 346 D0 /
      DATA WGK ( 13) / 0.0966427269 8362367850 5179907627 589 D0 /
      DATA WGK ( 14) / 0.0991735987 2179195933 2393173484 603 D0 /
      DATA WGK ( 15) / 0.1007698455 2387559504 4946662617 570 D0 /
      DATA WGK ( 16) / 0.1013300070 1479154901 7374792767 493 D0 /
C
C
C           LIST OF MAJOR VARIABLES
C           -----------------------
C           CENTR  - MID POINT OF THE INTERVAL
C           HLGTH  - HALF-LENGTH OF THE INTERVAL
C           ABSC   - ABSCISSA
C           FVAL*  - FUNCTION VALUE
C           RESG   - RESULT OF THE 15-POINT GAUSS FORMULA
C           RESK   - RESULT OF THE 31-POINT KRONROD FORMULA
C           RESKH  - APPROXIMATION TO THE MEAN VALUE OF F OVER (A,B),
C                    I.E. TO I/(B-A)
C
C           MACHINE DEPENDENT CONSTANTS
C           ---------------------------
C           EPMACH IS THE LARGEST RELATIVE SPACING.
C           UFLOW IS THE SMALLEST POSITIVE MAGNITUDE.
C***FIRST EXECUTABLE STATEMENT  DQK31
      EPMACH = D1MACH(4)
      UFLOW = D1MACH(1)
C
      CENTR = 0.5D+00*(A+B)
      HLGTH = 0.5D+00*(B-A)
      DHLGTH = ABS(HLGTH)
C
C           COMPUTE THE 31-POINT KRONROD APPROXIMATION TO
C           THE INTEGRAL, AND ESTIMATE THE ABSOLUTE ERROR.
C
      FC = F(CENTR)
      RESG = WG(8)*FC
      RESK = WGK(16)*FC
      RESABS = ABS(RESK)
      DO 10 J=1,7
        JTW = J*2
        ABSC = HLGTH*XGK(JTW)
        FVAL1 = F(CENTR-ABSC)
        FVAL2 = F(CENTR+ABSC)
        FV1(JTW) = FVAL1
        FV2(JTW) = FVAL2
        FSUM = FVAL1+FVAL2
        RESG = RESG+WG(J)*FSUM
        RESK = RESK+WGK(JTW)*FSUM
        RESABS = RESABS+WGK(JTW)*(ABS(FVAL1)+ABS(FVAL2))
   10 CONTINUE
      DO 15 J = 1,8
        JTWM1 = J*2-1
        ABSC = HLGTH*XGK(JTWM1)
        FVAL1 = F(CENTR-ABSC)
        FVAL2 = F(CENTR+ABSC)
        FV1(JTWM1) = FVAL1
        FV2(JTWM1) = FVAL2
        FSUM = FVAL1+FVAL2
        RESK = RESK+WGK(JTWM1)*FSUM
        RESABS = RESABS+WGK(JTWM1)*(ABS(FVAL1)+ABS(FVAL2))
   15 CONTINUE
      RESKH = RESK*0.5D+00
      RESASC = WGK(16)*ABS(FC-RESKH)
      DO 20 J=1,15
        RESASC = RESASC+WGK(J)*(ABS(FV1(J)-RESKH)+ABS(FV2(J)-RESKH))
   20 CONTINUE
      RESULT = RESK*HLGTH
      RESABS = RESABS*DHLGTH
      RESASC = RESASC*DHLGTH
      ABSERR = ABS((RESK-RESG)*HLGTH)
      IF(RESASC.NE.0.0D+00.AND.ABSERR.NE.0.0D+00)
     1  ABSERR = RESASC*MIN(0.1D+01,(0.2D+03*ABSERR/RESASC)**1.5D+00)
      IF(RESABS.GT.UFLOW/(0.5D+02*EPMACH)) ABSERR = MAX
     1  ((EPMACH*0.5D+02)*RESABS,ABSERR)
      RETURN
      END
      SUBROUTINE DQK41(F,A,B,RESULT,ABSERR,RESABS,RESASC)
C
C   ADDED TO DATAPLOT 12/2003.  THIS ROUTINE ADDED FOR INTERNAL
C   DATAPLOT USAGE.
C
C***BEGIN PROLOGUE  DQK41
C***DATE WRITTEN   800101   (YYMMDD)
C***REVISION DATE  830518   (YYMMDD)
C***REVISION HISTORY (YYMMDD)
C   000601   Changed DMAX1/DMIN1/DABS to generic MAX/MIN/ABS
C***CATEGORY NO.  H2A1A2
C***KEYWORDS  41-POINT GAUSS-KRONROD RULES
C***AUTHOR  PIESSENS, ROBERT, APPLIED MATH. AND PROGR. DIV. -
C             K. U. LEUVEN
C           DE DONCKER, ELISE, APPLIED MATH. AND PROGR. DIV. -
C             K. U. LEUVEN
C***PURPOSE  To compute I = Integral of F over (A,B), with error
C                           estimate
C                       J = Integral of ABS(F) over (A,B)
C***DESCRIPTION
C
C           Integration rules
C           Standard fortran subroutine
C           Double precision version
C
C           PARAMETERS
C            ON ENTRY
C              F      - Double precision
C                       Function subprogram defining the integrand
C                       FUNCTION F(X). The actual name for F needs to be
C                       declared E X T E R N A L in the calling program.
C
C              A      - Double precision
C                       Lower limit of integration
C
C              B      - Double precision
C                       Upper limit of integration
C
C            ON RETURN
C              RESULT - Double precision
C                       Approximation to the integral I
C                       RESULT is computed by applying the 41-POINT
C                       GAUSS-KRONROD RULE (RESK) obtained by optimal
C                       addition of abscissae to the 20-POINT GAUSS
C                       RULE (RESG).
C
C              ABSERR - Double precision
C                       Estimate of the modulus of the absolute error,
C                       which should not exceed ABS(I-RESULT)
C
C              RESABS - Double precision
C                       Approximation to the integral J
C
C              RESASC - Double precision
C                       Approximation to the integal of ABS(F-I/(B-A))
C                       over (A,B)
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH
C***END PROLOGUE  DQK41
C
      INCLUDE 'DPCOMC.INC'
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      DOUBLE PRECISION A,ABSC,ABSERR,B,CENTR,DHLGTH,
     1  EPMACH,F,FC,FSUM,FVAL1,FVAL2,FV1,FV2,HLGTH,RESABS,RESASC,
     2  RESG,RESK,RESKH,RESULT,UFLOW,WG,WGK,XGK
      INTEGER J,JTW,JTWM1
      EXTERNAL F
C
      DIMENSION FV1(20),FV2(20),XGK(21),WGK(21),WG(10)
C
C           THE ABSCISSAE AND WEIGHTS ARE GIVEN FOR THE INTERVAL (-1,1).
C           BECAUSE OF SYMMETRY ONLY THE POSITIVE ABSCISSAE AND THEIR
C           CORRESPONDING WEIGHTS ARE GIVEN.
C
C           XGK    - ABSCISSAE OF THE 41-POINT GAUSS-KRONROD RULE
C                    XGK(2), XGK(4), ...  ABSCISSAE OF THE 20-POINT
C                    GAUSS RULE
C                    XGK(1), XGK(3), ...  ABSCISSAE WHICH ARE OPTIMALLY
C                    ADDED TO THE 20-POINT GAUSS RULE
C
C           WGK    - WEIGHTS OF THE 41-POINT GAUSS-KRONROD RULE
C
C           WG     - WEIGHTS OF THE 20-POINT GAUSS RULE
C
C
C GAUSS QUADRATURE WEIGHTS AND KRONRON QUADRATURE ABSCISSAE AND WEIGHTS
C AS EVALUATED WITH 80 DECIMAL DIGIT ARITHMETIC BY L. W. FULLERTON,
C BELL LABS, NOV. 1981.
C
      DATA WG  (  1) / 0.0176140071 3915211831 1861962351 853 D0 /
      DATA WG  (  2) / 0.0406014298 0038694133 1039952274 932 D0 /
      DATA WG  (  3) / 0.0626720483 3410906356 9506535187 042 D0 /
      DATA WG  (  4) / 0.0832767415 7670474872 4758143222 046 D0 /
      DATA WG  (  5) / 0.1019301198 1724043503 6750135480 350 D0 /
      DATA WG  (  6) / 0.1181945319 6151841731 2377377711 382 D0 /
      DATA WG  (  7) / 0.1316886384 4917662689 8494499748 163 D0 /
      DATA WG  (  8) / 0.1420961093 1838205132 9298325067 165 D0 /
      DATA WG  (  9) / 0.1491729864 7260374678 7828737001 969 D0 /
      DATA WG  ( 10) / 0.1527533871 3072585069 8084331955 098 D0 /
C
      DATA XGK (  1) / 0.9988590315 8827766383 8315576545 863 D0 /
      DATA XGK (  2) / 0.9931285991 8509492478 6122388471 320 D0 /
      DATA XGK (  3) / 0.9815078774 5025025919 3342994720 217 D0 /
      DATA XGK (  4) / 0.9639719272 7791379126 7666131197 277 D0 /
      DATA XGK (  5) / 0.9408226338 3175475351 9982722212 443 D0 /
      DATA XGK (  6) / 0.9122344282 5132590586 7752441203 298 D0 /
      DATA XGK (  7) / 0.8782768112 5228197607 7442995113 078 D0 /
      DATA XGK (  8) / 0.8391169718 2221882339 4529061701 521 D0 /
      DATA XGK (  9) / 0.7950414288 3755119835 0638833272 788 D0 /
      DATA XGK ( 10) / 0.7463319064 6015079261 4305070355 642 D0 /
      DATA XGK ( 11) / 0.6932376563 3475138480 5490711845 932 D0 /
      DATA XGK ( 12) / 0.6360536807 2651502545 2836696226 286 D0 /
      DATA XGK ( 13) / 0.5751404468 1971031534 2946036586 425 D0 /
      DATA XGK ( 14) / 0.5108670019 5082709800 4364050955 251 D0 /
      DATA XGK ( 15) / 0.4435931752 3872510319 9992213492 640 D0 /
      DATA XGK ( 16) / 0.3737060887 1541956067 2548177024 927 D0 /
      DATA XGK ( 17) / 0.3016278681 1491300432 0555356858 592 D0 /
      DATA XGK ( 18) / 0.2277858511 4164507808 0496195368 575 D0 /
      DATA XGK ( 19) / 0.1526054652 4092267550 5220241022 678 D0 /
      DATA XGK ( 20) / 0.0765265211 3349733375 4640409398 838 D0 /
      DATA XGK ( 21) / 0.0000000000 0000000000 0000000000 000 D0 /
C
      DATA WGK (  1) / 0.0030735837 1852053150 1218293246 031 D0 /
      DATA WGK (  2) / 0.0086002698 5564294219 8661787950 102 D0 /
      DATA WGK (  3) / 0.0146261692 5697125298 3787960308 868 D0 /
      DATA WGK (  4) / 0.0203883734 6126652359 8010231432 755 D0 /
      DATA WGK (  5) / 0.0258821336 0495115883 4505067096 153 D0 /
      DATA WGK (  6) / 0.0312873067 7703279895 8543119323 801 D0 /
      DATA WGK (  7) / 0.0366001697 5820079803 0557240707 211 D0 /
      DATA WGK (  8) / 0.0416688733 2797368626 3788305936 895 D0 /
      DATA WGK (  9) / 0.0464348218 6749767472 0231880926 108 D0 /
      DATA WGK ( 10) / 0.0509445739 2372869193 2707670050 345 D0 /
      DATA WGK ( 11) / 0.0551951053 4828599474 4832372419 777 D0 /
      DATA WGK ( 12) / 0.0591114008 8063957237 4967220648 594 D0 /
      DATA WGK ( 13) / 0.0626532375 5478116802 5870122174 255 D0 /
      DATA WGK ( 14) / 0.0658345971 3361842211 1563556969 398 D0 /
      DATA WGK ( 15) / 0.0686486729 2852161934 5623411885 368 D0 /
      DATA WGK ( 16) / 0.0710544235 5344406830 5790361723 210 D0 /
      DATA WGK ( 17) / 0.0730306903 3278666749 5189417658 913 D0 /
      DATA WGK ( 18) / 0.0745828754 0049918898 6581418362 488 D0 /
      DATA WGK ( 19) / 0.0757044976 8455667465 9542775376 617 D0 /
      DATA WGK ( 20) / 0.0763778676 7208073670 5502835038 061 D0 /
      DATA WGK ( 21) / 0.0766007119 1799965644 5049901530 102 D0 /
C
C
C           LIST OF MAJOR VARIABLES
C           -----------------------
C
C           CENTR  - MID POINT OF THE INTERVAL
C           HLGTH  - HALF-LENGTH OF THE INTERVAL
C           ABSC   - ABSCISSA
C           FVAL*  - FUNCTION VALUE
C           RESG   - RESULT OF THE 20-POINT GAUSS FORMULA
C           RESK   - RESULT OF THE 41-POINT KRONROD FORMULA
C           RESKH  - APPROXIMATION TO MEAN VALUE OF F OVER (A,B), I.E.
C                    TO I/(B-A)
C
C           MACHINE DEPENDENT CONSTANTS
C           ---------------------------
C
C           EPMACH IS THE LARGEST RELATIVE SPACING.
C           UFLOW IS THE SMALLEST POSITIVE MAGNITUDE.
C
C***FIRST EXECUTABLE STATEMENT  DQK41
      EPMACH = D1MACH(4)
      UFLOW = D1MACH(1)
C
      CENTR = 0.5D+00*(A+B)
      HLGTH = 0.5D+00*(B-A)
      DHLGTH = ABS(HLGTH)
C
C           COMPUTE THE 41-POINT GAUSS-KRONROD APPROXIMATION TO
C           THE INTEGRAL, AND ESTIMATE THE ABSOLUTE ERROR.
C
      RESG = 0.0D+00
      FC = F(CENTR)
      RESK = WGK(21)*FC
      RESABS = ABS(RESK)
      DO 10 J=1,10
        JTW = J*2
        ABSC = HLGTH*XGK(JTW)
        FVAL1 = F(CENTR-ABSC)
        FVAL2 = F(CENTR+ABSC)
        FV1(JTW) = FVAL1
        FV2(JTW) = FVAL2
        FSUM = FVAL1+FVAL2
        RESG = RESG+WG(J)*FSUM
        RESK = RESK+WGK(JTW)*FSUM
        RESABS = RESABS+WGK(JTW)*(ABS(FVAL1)+ABS(FVAL2))
   10 CONTINUE
      DO 15 J = 1,10
        JTWM1 = J*2-1
        ABSC = HLGTH*XGK(JTWM1)
        FVAL1 = F(CENTR-ABSC)
        FVAL2 = F(CENTR+ABSC)
        FV1(JTWM1) = FVAL1
        FV2(JTWM1) = FVAL2
        FSUM = FVAL1+FVAL2
        RESK = RESK+WGK(JTWM1)*FSUM
        RESABS = RESABS+WGK(JTWM1)*(ABS(FVAL1)+ABS(FVAL2))
   15 CONTINUE
      RESKH = RESK*0.5D+00
      RESASC = WGK(21)*ABS(FC-RESKH)
      DO 20 J=1,20
        RESASC = RESASC+WGK(J)*(ABS(FV1(J)-RESKH)+ABS(FV2(J)-RESKH))
   20 CONTINUE
      RESULT = RESK*HLGTH
      RESABS = RESABS*DHLGTH
      RESASC = RESASC*DHLGTH
      ABSERR = ABS((RESK-RESG)*HLGTH)
      IF(RESASC.NE.0.0D+00.AND.ABSERR.NE.0.D+00)
     1  ABSERR = RESASC*MIN(0.1D+01,(0.2D+03*ABSERR/RESASC)**1.5D+00)
      IF(RESABS.GT.UFLOW/(0.5D+02*EPMACH)) ABSERR = MAX
     1  ((EPMACH*0.5D+02)*RESABS,ABSERR)
      RETURN
      END
      SUBROUTINE DQK51(F,A,B,RESULT,ABSERR,RESABS,RESASC)
C
C   ADDED TO DATAPLOT 12/2003.  THIS ROUTINE ADDED FOR INTERNAL
C   DATAPLOT USAGE.
C
C***BEGIN PROLOGUE  DQK51
C***DATE WRITTEN   800101   (YYMMDD)
C***REVISION DATE  830518   (YYMMDD)
C                  960627   Missing WGK(26) restored (RFB).
C                  000601   Changed DMAX1/DMIN1/DABS to generic MAX/MIN/ABS
C***CATEGORY NO.  H2A1A2
C***KEYWORDS  51-POINT GAUSS-KRONROD RULES
C***AUTHOR  PIESSENS, ROBERT, APPLIED MATH. AND PROGR. DIV. -
C             K. U. LEUVEN
C           DE DONCKER, ELISE, APPLIED MATH. AND PROGR. DIV. -
C             K. U. LEUVEN
C***PURPOSE  To compute I = Integral of F over (A,B) with error
C                           estimate
C                       J = Integral of ABS(F) over (A,B)
C***DESCRIPTION
C
C           Integration rules
C           Standard fortran subroutine
C           Double precision version
C
C           PARAMETERS
C            ON ENTRY
C              F      - Double precision
C                       Function subroutine defining the integrand
C                       function F(X). The actual name for F needs to be
C                       declared E X T E R N A L in the calling program.
C
C              A      - Double precision
C                       Lower limit of integration
C
C              B      - Double precision
C                       Upper limit of integration
C
C            ON RETURN
C              RESULT - Double precision
C                       Approximation to the integral I
C                       RESULT is computed by applying the 51-point
C                       Kronrod rule (RESK) obtained by optimal addition
C                       of abscissae to the 25-point Gauss rule (RESG).
C
C              ABSERR - Double precision
C                       Estimate of the modulus of the absolute error,
C                       which should not exceed ABS(I-RESULT)
C
C              RESABS - Double precision
C                       Approximation to the integral J
C
C              RESASC - Double precision
C                       Approximation to the integral of ABS(F-I/(B-A))
C                       over (A,B)
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH
C***END PROLOGUE  DQK51
C
      INCLUDE 'DPCOMC.INC'
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      DOUBLE PRECISION A,ABSC,ABSERR,B,CENTR,DHLGTH,
     1  EPMACH,F,FC,FSUM,FVAL1,FVAL2,FV1,FV2,HLGTH,RESABS,RESASC,
     2  RESG,RESK,RESKH,RESULT,UFLOW,WG,WGK,XGK
      INTEGER J,JTW,JTWM1
      EXTERNAL F
C
      DIMENSION FV1(25),FV2(25),XGK(26),WGK(26),WG(13)
C
C           THE ABSCISSAE AND WEIGHTS ARE GIVEN FOR THE INTERVAL (-1,1).
C           BECAUSE OF SYMMETRY ONLY THE POSITIVE ABSCISSAE AND THEIR
C           CORRESPONDING WEIGHTS ARE GIVEN.
C
C           XGK    - ABSCISSAE OF THE 51-POINT KRONROD RULE
C                    XGK(2), XGK(4), ...  ABSCISSAE OF THE 25-POINT
C                    GAUSS RULE
C                    XGK(1), XGK(3), ...  ABSCISSAE WHICH ARE OPTIMALLY
C                    ADDED TO THE 25-POINT GAUSS RULE
C
C           WGK    - WEIGHTS OF THE 51-POINT KRONROD RULE
C
C           WG     - WEIGHTS OF THE 25-POINT GAUSS RULE
C
C
C GAUSS QUADRATURE WEIGHTS AND KRONRON QUADRATURE ABSCISSAE AND WEIGHTS
C AS EVALUATED WITH 80 DECIMAL DIGIT ARITHMETIC BY L. W. FULLERTON,
C BELL LABS, NOV. 1981.
C
      DATA WG  (  1) / 0.0113937985 0102628794 7902964113 235 D0 /
      DATA WG  (  2) / 0.0263549866 1503213726 1901815295 299 D0 /
      DATA WG  (  3) / 0.0409391567 0130631265 5623487711 646 D0 /
      DATA WG  (  4) / 0.0549046959 7583519192 5936891540 473 D0 /
      DATA WG  (  5) / 0.0680383338 1235691720 7187185656 708 D0 /
      DATA WG  (  6) / 0.0801407003 3500101801 3234959669 111 D0 /
      DATA WG  (  7) / 0.0910282619 8296364981 1497220702 892 D0 /
      DATA WG  (  8) / 0.1005359490 6705064420 2206890392 686 D0 /
      DATA WG  (  9) / 0.1085196244 7426365311 6093957050 117 D0 /
      DATA WG  ( 10) / 0.1148582591 4571164833 9325545869 556 D0 /
      DATA WG  ( 11) / 0.1194557635 3578477222 8178126512 901 D0 /
      DATA WG  ( 12) / 0.1222424429 9031004168 8959518945 852 D0 /
      DATA WG  ( 13) / 0.1231760537 2671545120 3902873079 050 D0 /
C
      DATA XGK (  1) / 0.9992621049 9260983419 3457486540 341 D0 /
      DATA XGK (  2) / 0.9955569697 9049809790 8784946893 902 D0 /
      DATA XGK (  3) / 0.9880357945 3407724763 7331014577 406 D0 /
      DATA XGK (  4) / 0.9766639214 5951751149 8315386479 594 D0 /
      DATA XGK (  5) / 0.9616149864 2584251241 8130033660 167 D0 /
      DATA XGK (  6) / 0.9429745712 2897433941 4011169658 471 D0 /
      DATA XGK (  7) / 0.9207471152 8170156174 6346084546 331 D0 /
      DATA XGK (  8) / 0.8949919978 7827536885 1042006782 805 D0 /
      DATA XGK (  9) / 0.8658470652 9327559544 8996969588 340 D0 /
      DATA XGK ( 10) / 0.8334426287 6083400142 1021108693 570 D0 /
      DATA XGK ( 11) / 0.7978737979 9850005941 0410904994 307 D0 /
      DATA XGK ( 12) / 0.7592592630 3735763057 7282865204 361 D0 /
      DATA XGK ( 13) / 0.7177664068 1308438818 6654079773 298 D0 /
      DATA XGK ( 14) / 0.6735663684 7346836448 5120633247 622 D0 /
      DATA XGK ( 15) / 0.6268100990 1031741278 8122681624 518 D0 /
      DATA XGK ( 16) / 0.5776629302 4122296772 3689841612 654 D0 /
      DATA XGK ( 17) / 0.5263252843 3471918259 9623778158 010 D0 /
      DATA XGK ( 18) / 0.4730027314 4571496052 2182115009 192 D0 /
      DATA XGK ( 19) / 0.4178853821 9303774885 1814394594 572 D0 /
      DATA XGK ( 20) / 0.3611723058 0938783773 5821730127 641 D0 /
      DATA XGK ( 21) / 0.3030895389 3110783016 7478909980 339 D0 /
      DATA XGK ( 22) / 0.2438668837 2098843204 5190362797 452 D0 /
      DATA XGK ( 23) / 0.1837189394 2104889201 5969888759 528 D0 /
      DATA XGK ( 24) / 0.1228646926 1071039638 7359818808 037 D0 /
      DATA XGK ( 25) / 0.0615444830 0568507888 6546392366 797 D0 /
      DATA XGK ( 26) / 0.0000000000 0000000000 0000000000 000 D0 /
C
      DATA WGK (  1) / 0.0019873838 9233031592 6507851882 843 D0 /
      DATA WGK (  2) / 0.0055619321 3535671375 8040236901 066 D0 /
      DATA WGK (  3) / 0.0094739733 8617415160 7207710523 655 D0 /
      DATA WGK (  4) / 0.0132362291 9557167481 3656405846 976 D0 /
      DATA WGK (  5) / 0.0168478177 0912829823 1516667536 336 D0 /
      DATA WGK (  6) / 0.0204353711 4588283545 6568292235 939 D0 /
      DATA WGK (  7) / 0.0240099456 0695321622 0092489164 881 D0 /
      DATA WGK (  8) / 0.0274753175 8785173780 2948455517 811 D0 /
      DATA WGK (  9) / 0.0307923001 6738748889 1109020215 229 D0 /
      DATA WGK ( 10) / 0.0340021302 7432933783 6748795229 551 D0 /
      DATA WGK ( 11) / 0.0371162714 8341554356 0330625367 620 D0 /
      DATA WGK ( 12) / 0.0400838255 0403238207 4839284467 076 D0 /
      DATA WGK ( 13) / 0.0428728450 2017004947 6895792439 495 D0 /
      DATA WGK ( 14) / 0.0455029130 4992178890 9870584752 660 D0 /
      DATA WGK ( 15) / 0.0479825371 3883671390 6392255756 915 D0 /
      DATA WGK ( 16) / 0.0502776790 8071567196 3325259433 440 D0 /
      DATA WGK ( 17) / 0.0523628858 0640747586 4366712137 873 D0 /
      DATA WGK ( 18) / 0.0542511298 8854549014 4543370459 876 D0 /
      DATA WGK ( 19) / 0.0559508112 2041231730 8240686382 747 D0 /
      DATA WGK ( 20) / 0.0574371163 6156783285 3582693939 506 D0 /
      DATA WGK ( 21) / 0.0586896800 2239420796 1974175856 788 D0 /
      DATA WGK ( 22) / 0.0597203403 2417405997 9099291932 562 D0 /
      DATA WGK ( 23) / 0.0605394553 7604586294 5360267517 565 D0 /
      DATA WGK ( 24) / 0.0611285097 1705304830 5859030416 293 D0 /
      DATA WGK ( 25) / 0.0614711898 7142531666 1544131965 264 D0 /
      DATA WGK ( 26) / 0.0615808180 6783293507 8759824240 055 D0 /
C
C
C           LIST OF MAJOR VARIABLES
C           -----------------------
C
C           CENTR  - MID POINT OF THE INTERVAL
C           HLGTH  - HALF-LENGTH OF THE INTERVAL
C           ABSC   - ABSCISSA
C           FVAL*  - FUNCTION VALUE
C           RESG   - RESULT OF THE 25-POINT GAUSS FORMULA
C           RESK   - RESULT OF THE 51-POINT KRONROD FORMULA
C           RESKH  - APPROXIMATION TO THE MEAN VALUE OF F OVER (A,B),
C                    I.E. TO I/(B-A)
C
C           MACHINE DEPENDENT CONSTANTS
C           ---------------------------
C
C           EPMACH IS THE LARGEST RELATIVE SPACING.
C           UFLOW IS THE SMALLEST POSITIVE MAGNITUDE.
C
C***FIRST EXECUTABLE STATEMENT  DQK51
      EPMACH = D1MACH(4)
      UFLOW = D1MACH(1)
C
      CENTR = 0.5D+00*(A+B)
      HLGTH = 0.5D+00*(B-A)
      DHLGTH = ABS(HLGTH)
C
C           COMPUTE THE 51-POINT KRONROD APPROXIMATION TO
C           THE INTEGRAL, AND ESTIMATE THE ABSOLUTE ERROR.
C
      FC = F(CENTR)
      RESG = WG(13)*FC
      RESK = WGK(26)*FC
      RESABS = ABS(RESK)
      DO 10 J=1,12
        JTW = J*2
        ABSC = HLGTH*XGK(JTW)
        FVAL1 = F(CENTR-ABSC)
        FVAL2 = F(CENTR+ABSC)
        FV1(JTW) = FVAL1
        FV2(JTW) = FVAL2
        FSUM = FVAL1+FVAL2
        RESG = RESG+WG(J)*FSUM
        RESK = RESK+WGK(JTW)*FSUM
        RESABS = RESABS+WGK(JTW)*(ABS(FVAL1)+ABS(FVAL2))
   10 CONTINUE
      DO 15 J = 1,13
        JTWM1 = J*2-1
        ABSC = HLGTH*XGK(JTWM1)
        FVAL1 = F(CENTR-ABSC)
        FVAL2 = F(CENTR+ABSC)
        FV1(JTWM1) = FVAL1
        FV2(JTWM1) = FVAL2
        FSUM = FVAL1+FVAL2
        RESK = RESK+WGK(JTWM1)*FSUM
        RESABS = RESABS+WGK(JTWM1)*(ABS(FVAL1)+ABS(FVAL2))
   15 CONTINUE
      RESKH = RESK*0.5D+00
      RESASC = WGK(26)*ABS(FC-RESKH)
      DO 20 J=1,25
        RESASC = RESASC+WGK(J)*(ABS(FV1(J)-RESKH)+ABS(FV2(J)-RESKH))
   20 CONTINUE
      RESULT = RESK*HLGTH
      RESABS = RESABS*DHLGTH
      RESASC = RESASC*DHLGTH
      ABSERR = ABS((RESK-RESG)*HLGTH)
      IF(RESASC.NE.0.0D+00.AND.ABSERR.NE.0.0D+00)
     1  ABSERR = RESASC*MIN(0.1D+01,(0.2D+03*ABSERR/RESASC)**1.5D+00)
      IF(RESABS.GT.UFLOW/(0.5D+02*EPMACH)) ABSERR = MAX
     1  ((EPMACH*0.5D+02)*RESABS,ABSERR)
      RETURN
      END
      SUBROUTINE DQK61(F,A,B,RESULT,ABSERR,RESABS,RESASC)
C
C   ADDED TO DATAPLOT 12/2003.  THIS ROUTINE ADDED FOR INTERNAL
C   DATAPLOT USAGE.
C
C***BEGIN PROLOGUE  DQK61
C***DATE WRITTEN   800101   (YYMMDD)
C***REVISION DATE  830518   (YYMMDD)
C***REVISION HISTORY (YYMMDD)
C   000601   Changed DMAX1/DMIN1/DABS to generic MAX/MIN/ABS
C***CATEGORY NO.  H2A1A2
C***KEYWORDS  61-POINT GAUSS-KRONROD RULES
C***AUTHOR  PIESSENS, ROBERT, APPLIED MATH. AND PROGR. DIV. -
C             K. U. LEUVEN
C           DE DONCKER, ELISE, APPLIED MATH. AND PROGR. DIV. -
C             K. U. LEUVEN
C***PURPOSE  To compute I = Integral of F over (A,B) with error
C                           estimate
C                       J = Integral of DABS(F) over (A,B)
C***DESCRIPTION
C
C        Integration rule
C        Standard fortran subroutine
C        Double precision version
C
C
C        PARAMETERS
C         ON ENTRY
C           F      - Double precision
C                    Function subprogram defining the integrand
C                    function F(X). The actual name for F needs to be
C                    declared E X T E R N A L in the calling program.
C
C           A      - Double precision
C                    Lower limit of integration
C
C           B      - Double precision
C                    Upper limit of integration
C
C         ON RETURN
C           RESULT - Double precision
C                    Approximation to the integral I
C                    RESULT is computed by applying the 61-point
C                    Kronrod rule (RESK) obtained by optimal addition of
C                    abscissae to the 30-point Gauss rule (RESG).
C
C           ABSERR - Double precision
C                    Estimate of the modulus of the absolute error,
C                    which should equal or exceed DABS(I-RESULT)
C
C           RESABS - Double precision
C                    Approximation to the integral J
C
C           RESASC - Double precision
C                    Approximation to the integral of DABS(F-I/(B-A))
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH
C***END PROLOGUE  DQK61
C
      INCLUDE 'DPCOMC.INC'
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
CCCCC DOUBLE PRECISION A,DABSC,ABSERR,B,CENTR,ABS,DHLGTH,
      DOUBLE PRECISION A,DABSC,ABSERR,B,CENTR,DHLGTH,
     1  EPMACH,F,FC,FSUM,FVAL1,FVAL2,FV1,FV2,HLGTH,RESABS,RESASC,
     2  RESG,RESK,RESKH,RESULT,UFLOW,WG,WGK,XGK
      INTEGER J,JTW,JTWM1
      EXTERNAL F
C
      DIMENSION FV1(30),FV2(30),XGK(31),WGK(31),WG(15)
C
C           THE ABSCISSAE AND WEIGHTS ARE GIVEN FOR THE
C           INTERVAL (-1,1). BECAUSE OF SYMMETRY ONLY THE POSITIVE
C           ABSCISSAE AND THEIR CORRESPONDING WEIGHTS ARE GIVEN.
C
C           XGK   - ABSCISSAE OF THE 61-POINT KRONROD RULE
C                   XGK(2), XGK(4)  ... ABSCISSAE OF THE 30-POINT
C                   GAUSS RULE
C                   XGK(1), XGK(3)  ... OPTIMALLY ADDED ABSCISSAE
C                   TO THE 30-POINT GAUSS RULE
C
C           WGK   - WEIGHTS OF THE 61-POINT KRONROD RULE
C
C           WG    - WEIGTHS OF THE 30-POINT GAUSS RULE
C
C
C GAUSS QUADRATURE WEIGHTS AND KRONRON QUADRATURE ABSCISSAE AND WEIGHTS
C AS EVALUATED WITH 80 DECIMAL DIGIT ARITHMETIC BY L. W. FULLERTON,
C BELL LABS, NOV. 1981.
C
      DATA WG  (  1) / 0.0079681924 9616660561 5465883474 674 D0 /
      DATA WG  (  2) / 0.0184664683 1109095914 2302131912 047 D0 /
      DATA WG  (  3) / 0.0287847078 8332336934 9719179611 292 D0 /
      DATA WG  (  4) / 0.0387991925 6962704959 6801936446 348 D0 /
      DATA WG  (  5) / 0.0484026728 3059405290 2938140422 808 D0 /
      DATA WG  (  6) / 0.0574931562 1761906648 1721689402 056 D0 /
      DATA WG  (  7) / 0.0659742298 8218049512 8128515115 962 D0 /
      DATA WG  (  8) / 0.0737559747 3770520626 8243850022 191 D0 /
      DATA WG  (  9) / 0.0807558952 2942021535 4694938460 530 D0 /
      DATA WG  ( 10) / 0.0868997872 0108297980 2387530715 126 D0 /
      DATA WG  ( 11) / 0.0921225222 3778612871 7632707087 619 D0 /
      DATA WG  ( 12) / 0.0963687371 7464425963 9468626351 810 D0 /
      DATA WG  ( 13) / 0.0995934205 8679526706 2780282103 569 D0 /
      DATA WG  ( 14) / 0.1017623897 4840550459 6428952168 554 D0 /
      DATA WG  ( 15) / 0.1028526528 9355884034 1285636705 415 D0 /
C
      DATA XGK (  1) / 0.9994844100 5049063757 1325895705 811 D0 /
      DATA XGK (  2) / 0.9968934840 7464954027 1630050918 695 D0 /
      DATA XGK (  3) / 0.9916309968 7040459485 8628366109 486 D0 /
      DATA XGK (  4) / 0.9836681232 7974720997 0032581605 663 D0 /
      DATA XGK (  5) / 0.9731163225 0112626837 4693868423 707 D0 /
      DATA XGK (  6) / 0.9600218649 6830751221 6871025581 798 D0 /
      DATA XGK (  7) / 0.9443744447 4855997941 5831324037 439 D0 /
      DATA XGK (  8) / 0.9262000474 2927432587 9324277080 474 D0 /
      DATA XGK (  9) / 0.9055733076 9990779854 6522558925 958 D0 /
      DATA XGK ( 10) / 0.8825605357 9205268154 3116462530 226 D0 /
      DATA XGK ( 11) / 0.8572052335 4606109895 8658510658 944 D0 /
      DATA XGK ( 12) / 0.8295657623 8276839744 2898119732 502 D0 /
      DATA XGK ( 13) / 0.7997278358 2183908301 3668942322 683 D0 /
      DATA XGK ( 14) / 0.7677774321 0482619491 7977340974 503 D0 /
      DATA XGK ( 15) / 0.7337900624 5322680472 6171131369 528 D0 /
      DATA XGK ( 16) / 0.6978504947 9331579693 2292388026 640 D0 /
      DATA XGK ( 17) / 0.6600610641 2662696137 0053668149 271 D0 /
      DATA XGK ( 18) / 0.6205261829 8924286114 0477556431 189 D0 /
      DATA XGK ( 19) / 0.5793452358 2636169175 6024932172 540 D0 /
      DATA XGK ( 20) / 0.5366241481 4201989926 4169793311 073 D0 /
      DATA XGK ( 21) / 0.4924804678 6177857499 3693061207 709 D0 /
      DATA XGK ( 22) / 0.4470337695 3808917678 0609900322 854 D0 /
      DATA XGK ( 23) / 0.4004012548 3039439253 5476211542 661 D0 /
      DATA XGK ( 24) / 0.3527047255 3087811347 1037207089 374 D0 /
      DATA XGK ( 25) / 0.3040732022 7362507737 2677107199 257 D0 /
      DATA XGK ( 26) / 0.2546369261 6788984643 9805129817 805 D0 /
      DATA XGK ( 27) / 0.2045251166 8230989143 8957671002 025 D0 /
      DATA XGK ( 28) / 0.1538699136 0858354696 3794672743 256 D0 /
      DATA XGK ( 29) / 0.1028069379 6673703014 7096751318 001 D0 /
      DATA XGK ( 30) / 0.0514718425 5531769583 3025213166 723 D0 /
      DATA XGK ( 31) / 0.0000000000 0000000000 0000000000 000 D0 /
C
      DATA WGK (  1) / 0.0013890136 9867700762 4551591226 760 D0 /
      DATA WGK (  2) / 0.0038904611 2709988405 1267201844 516 D0 /
      DATA WGK (  3) / 0.0066307039 1593129217 3319826369 750 D0 /
      DATA WGK (  4) / 0.0092732796 5951776342 8441146892 024 D0 /
      DATA WGK (  5) / 0.0118230152 5349634174 2232898853 251 D0 /
      DATA WGK (  6) / 0.0143697295 0704580481 2451432443 580 D0 /
      DATA WGK (  7) / 0.0169208891 8905327262 7572289420 322 D0 /
      DATA WGK (  8) / 0.0194141411 9394238117 3408951050 128 D0 /
      DATA WGK (  9) / 0.0218280358 2160919229 7167485738 339 D0 /
      DATA WGK ( 10) / 0.0241911620 7808060136 5686370725 232 D0 /
      DATA WGK ( 11) / 0.0265099548 8233310161 0601709335 075 D0 /
      DATA WGK ( 12) / 0.0287540487 6504129284 3978785354 334 D0 /
      DATA WGK ( 13) / 0.0309072575 6238776247 2884252943 092 D0 /
      DATA WGK ( 14) / 0.0329814470 5748372603 1814191016 854 D0 /
      DATA WGK ( 15) / 0.0349793380 2806002413 7499670731 468 D0 /
      DATA WGK ( 16) / 0.0368823646 5182122922 3911065617 136 D0 /
      DATA WGK ( 17) / 0.0386789456 2472759295 0348651532 281 D0 /
      DATA WGK ( 18) / 0.0403745389 5153595911 1995279752 468 D0 /
      DATA WGK ( 19) / 0.0419698102 1516424614 7147541285 970 D0 /
      DATA WGK ( 20) / 0.0434525397 0135606931 6831728117 073 D0 /
      DATA WGK ( 21) / 0.0448148001 3316266319 2355551616 723 D0 /
      DATA WGK ( 22) / 0.0460592382 7100698811 6271735559 374 D0 /
      DATA WGK ( 23) / 0.0471855465 6929915394 5261478181 099 D0 /
      DATA WGK ( 24) / 0.0481858617 5708712914 0779492298 305 D0 /
      DATA WGK ( 25) / 0.0490554345 5502977888 7528165367 238 D0 /
      DATA WGK ( 26) / 0.0497956834 2707420635 7811569379 942 D0 /
      DATA WGK ( 27) / 0.0504059214 0278234684 0893085653 585 D0 /
      DATA WGK ( 28) / 0.0508817958 9874960649 2297473049 805 D0 /
      DATA WGK ( 29) / 0.0512215478 4925877217 0656282604 944 D0 /
      DATA WGK ( 30) / 0.0514261285 3745902593 3862879215 781 D0 /
      DATA WGK ( 31) / 0.0514947294 2945156755 8340433647 099 D0 /
C
C           LIST OF MAJOR VARIABLES
C           -----------------------
C
C           CENTR  - MID POINT OF THE INTERVAL
C           HLGTH  - HALF-LENGTH OF THE INTERVAL
C           DABSC  - ABSCISSA
C           FVAL*  - FUNCTION VALUE
C           RESG   - RESULT OF THE 30-POINT GAUSS RULE
C           RESK   - RESULT OF THE 61-POINT KRONROD RULE
C           RESKH  - APPROXIMATION TO THE MEAN VALUE OF F
C                    OVER (A,B), I.E. TO I/(B-A)
C
C           MACHINE DEPENDENT CONSTANTS
C           ---------------------------
C
C           EPMACH IS THE LARGEST RELATIVE SPACING.
C           UFLOW IS THE SMALLEST POSITIVE MAGNITUDE.
C
C***FIRST EXECUTABLE STATEMENT  DQK61
      EPMACH = D1MACH(4)
      UFLOW = D1MACH(1)
C
      CENTR = 0.5D+00*(B+A)
      HLGTH = 0.5D+00*(B-A)
      DHLGTH = DABS(HLGTH)
C
C           COMPUTE THE 61-POINT KRONROD APPROXIMATION TO THE
C           INTEGRAL, AND ESTIMATE THE ABSOLUTE ERROR.
C
      RESG = 0.0D+00
      FC = F(CENTR)
      RESK = WGK(31)*FC
      RESABS = DABS(RESK)
      DO 10 J=1,15
        JTW = J*2
        DABSC = HLGTH*XGK(JTW)
        FVAL1 = F(CENTR-DABSC)
        FVAL2 = F(CENTR+DABSC)
        FV1(JTW) = FVAL1
        FV2(JTW) = FVAL2
        FSUM = FVAL1+FVAL2
        RESG = RESG+WG(J)*FSUM
        RESK = RESK+WGK(JTW)*FSUM
        RESABS = RESABS+WGK(JTW)*(DABS(FVAL1)+DABS(FVAL2))
   10 CONTINUE
      DO 15 J=1,15
        JTWM1 = J*2-1
        DABSC = HLGTH*XGK(JTWM1)
        FVAL1 = F(CENTR-DABSC)
        FVAL2 = F(CENTR+DABSC)
        FV1(JTWM1) = FVAL1
        FV2(JTWM1) = FVAL2
        FSUM = FVAL1+FVAL2
        RESK = RESK+WGK(JTWM1)*FSUM
        RESABS = RESABS+WGK(JTWM1)*(ABS(FVAL1)+DABS(FVAL2))
  15    CONTINUE
      RESKH = RESK*0.5D+00
      RESASC = WGK(31)*DABS(FC-RESKH)
      DO 20 J=1,30
        RESASC = RESASC+WGK(J)*(DABS(FV1(J)-RESKH)+DABS(FV2(J)-RESKH))
   20 CONTINUE
      RESULT = RESK*HLGTH
      RESABS = RESABS*DHLGTH
      RESASC = RESASC*DHLGTH
      ABSERR = DABS((RESK-RESG)*HLGTH)
      IF(RESASC.NE.0.0D+00.AND.ABSERR.NE.0.0D+00)
     1  ABSERR = RESASC*MIN(0.1D+01,(0.2D+03*ABSERR/RESASC)**1.5D+00)
      IF(RESABS.GT.UFLOW/(0.5D+02*EPMACH)) ABSERR = MAX
     1  ((EPMACH*0.5D+02)*RESABS,ABSERR)
      RETURN
      END
      SUBROUTINE DQPSRT(LIMIT,LAST,MAXERR,ERMAX,ELIST,IORD,NRMAX)
C
C   ADDED TO DATAPLOT 12/2003.  THIS ROUTINE ADDED FOR INTERNAL
C   DATAPLOT USAGE.
C
C***BEGIN PROLOGUE  DQPSRT
C***REFER TO  DQAGE,DQAGIE,DQAGPE,DQAWSE
C***ROUTINES CALLED  (NONE)
C***REVISION DATE  810101   (YYMMDD)
C***KEYWORDS  SEQUENTIAL SORTING
C***AUTHOR  PIESSENS, ROBERT, APPLIED MATH. AND PROGR. DIV. -
C             K. U. LEUVEN
C           DE DONCKER, ELISE, APPLIED MATH. AND PROGR. DIV. -
C             K. U. LEUVEN
C***PURPOSE  This routine maintains the descending ordering in the
C            list of the local error estimated resulting from the
C            interval subdivision process. At each call two error
C            estimates are inserted using the sequential search
C            method, top-down for the largest error estimate and
C            bottom-up for the smallest error estimate.
C***DESCRIPTION
C
C           Ordering routine
C           Standard fortran subroutine
C           Double precision version
C
C           PARAMETERS (MEANING AT OUTPUT)
C              LIMIT  - Integer
C                       Maximum number of error estimates the list
C                       can contain
C
C              LAST   - Integer
C                       Number of error estimates currently in the list
C
C              MAXERR - Integer
C                       Maxerr points to the NRMAX-th largest error
C                       estimate currently in the list
C
C              ERMAX  - Double precision
C                       NRMAX-th largest error estimate
C                       ERMAX = ELIST(MAXERR)
C
C              ELIST  - Double precision
C                       Vector of dimension LAST containing
C                       the error estimates
C
C              IORD   - Integer
C                       Vector of dimension LAST, the first K elements
C                       of which contain pointers to the error
C                       estimates, such that
C                       ELIST(IORD(1)),...,  ELIST(IORD(K))
C                       form a decreasing sequence, with
C                       K = LAST if LAST.LE.(LIMIT/2+2), and
C                       K = LIMIT+1-LAST otherwise
C
C              NRMAX  - Integer
C                       MAXERR = IORD(NRMAX)
C***END PROLOGUE  DQPSRT
C
      DOUBLE PRECISION ELIST,ERMAX,ERRMAX,ERRMIN
      INTEGER I,IBEG,IDO,IORD,ISUCC,J,JBND,JUPBN,K,LAST,LIMIT,MAXERR,
     1  NRMAX
      DIMENSION ELIST(LAST),IORD(LAST)
C
      INCLUDE 'DPCOMC.INC'
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C           CHECK WHETHER THE LIST CONTAINS MORE THAN
C           TWO ERROR ESTIMATES.
C
C***FIRST EXECUTABLE STATEMENT  DQPSRT
      IF(LAST.GT.2) GO TO 10
      IORD(1) = 1
      IORD(2) = 2
      GO TO 90
C
C           THIS PART OF THE ROUTINE IS ONLY EXECUTED IF, DUE TO A
C           DIFFICULT INTEGRAND, SUBDIVISION INCREASED THE ERROR
C           ESTIMATE. IN THE NORMAL CASE THE INSERT PROCEDURE SHOULD
C           START AFTER THE NRMAX-TH LARGEST ERROR ESTIMATE.
C
   10 ERRMAX = ELIST(MAXERR)
      IF(NRMAX.EQ.1) GO TO 30
      IDO = NRMAX-1
      DO 20 I = 1,IDO
        ISUCC = IORD(NRMAX-1)
C ***JUMP OUT OF DO-LOOP
        IF(ERRMAX.LE.ELIST(ISUCC)) GO TO 30
        IORD(NRMAX) = ISUCC
        NRMAX = NRMAX-1
   20    CONTINUE
C
C           COMPUTE THE NUMBER OF ELEMENTS IN THE LIST TO BE MAINTAINED
C           IN DESCENDING ORDER. THIS NUMBER DEPENDS ON THE NUMBER OF
C           SUBDIVISIONS STILL ALLOWED.
C
   30 JUPBN = LAST
      IF(LAST.GT.(LIMIT/2+2)) JUPBN = LIMIT+3-LAST
      ERRMIN = ELIST(LAST)
C
C           INSERT ERRMAX BY TRAVERSING THE LIST TOP-DOWN,
C           STARTING COMPARISON FROM THE ELEMENT ELIST(IORD(NRMAX+1)).
C
      JBND = JUPBN-1
      IBEG = NRMAX+1
      IF(IBEG.GT.JBND) GO TO 50
      DO 40 I=IBEG,JBND
        ISUCC = IORD(I)
C ***JUMP OUT OF DO-LOOP
        IF(ERRMAX.GE.ELIST(ISUCC)) GO TO 60
        IORD(I-1) = ISUCC
   40 CONTINUE
   50 IORD(JBND) = MAXERR
      IORD(JUPBN) = LAST
      GO TO 90
C
C           INSERT ERRMIN BY TRAVERSING THE LIST BOTTOM-UP.
C
   60 IORD(I-1) = MAXERR
      K = JBND
      DO 70 J=I,JBND
        ISUCC = IORD(K)
C ***JUMP OUT OF DO-LOOP
        IF(ERRMIN.LT.ELIST(ISUCC)) GO TO 80
        IORD(K+1) = ISUCC
        K = K-1
   70 CONTINUE
      IORD(I) = LAST
      GO TO 90
   80 IORD(K+1) = LAST
C
C           SET MAXERR AND ERMAX.
C
   90 MAXERR = IORD(NRMAX)
      ERMAX = ELIST(MAXERR)
      RETURN
      END
      SUBROUTINE DQRDC(X,LDX,N,P,QRAUX,JPVT,WORK,JOB)
C***BEGIN PROLOGUE  DQRDC
C***DATE WRITTEN   780814   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***REVISION HISTORY  (YYMMDD)
C   000330  Modified array declarations.  (JEC)
C***CATEGORY NO.  D5
C***KEYWORDS  DECOMPOSITION,DOUBLE PRECISION,LINEAR ALGEBRA,LINPACK,
C             MATRIX,ORTHOGONAL TRIANGULAR
C***AUTHOR  STEWART, G. W., (U. OF MARYLAND)
C***PURPOSE  Uses Householder transformations to compute the Qr factori-
C            zation of N by P matrix X.  Column pivoting is optional.
C***DESCRIPTION
C
C     DQRDC uses Householder transformations to compute the QR
C     factorization of an N by P matrix X.  Column pivoting
C     based on the 2-norms of the reduced columns may be
C     performed at the user's option.
C
C     On Entry
C
C        X       DOUBLE PRECISION(LDX,P), where LDX .GE. N.
C                X contains the matrix whose decomposition is to be
C                computed.
C
C        LDX     INTEGER.
C                LDX is the leading dimension of the array X.
C
C        N       INTEGER.
C                N is the number of rows of the matrix X.
C
C        P       INTEGER.
C                P is the number of columns of the matrix X.
C
C        JPVT    INTEGER(P).
C                JPVT contains integers that control the selection
C                of the pivot columns.  The K-th column X(K) of X
C                is placed in one of three classes according to the
C                value of JPVT(K).
C
C                   If JPVT(K) .GT. 0, then X(K) is an initial
C                                      column.
C
C                   If JPVT(K) .EQ. 0, then X(K) is a free column.
C
C                   If JPVT(K) .LT. 0, then X(K) is a final column.
C
C                Before the decomposition is computed, initial columns
C                are moved to the beginning of the array X and final
C                columns to the end.  Both initial and final columns
C                are frozen in place during the computation and only
C                free columns are moved.  At the K-th stage of the
C                reduction, if X(K) is occupied by a free column
C                it is interchanged with the free column of largest
C                reduced norm.  JPVT is not referenced if
C                JOB .EQ. 0.
C
C        WORK    DOUBLE PRECISION(P).
C                WORK is a work array.  WORK is not referenced if
C                JOB .EQ. 0.
C
C        JOB     INTEGER.
C                JOB is an integer that initiates column pivoting.
C                If JOB .EQ. 0, no pivoting is done.
C                If JOB .NE. 0, pivoting is done.
C
C     On Return
C
C        X       X contains in its upper triangle the upper
C                triangular matrix R of the QR factorization.
C                Below its diagonal X contains information from
C                which the orthogonal part of the decomposition
C                can be recovered.  Note that if pivoting has
C                been requested, the decomposition is not that
C                of the original matrix X but that of X
C                with its columns permuted as described by JPVT.
C
C        QRAUX   DOUBLE PRECISION(P).
C                QRAUX contains further information required to recover
C                the orthogonal part of the decomposition.
C
C        JPVT    JPVT(K) contains the index of the column of the
C                original matrix that has been interchanged into
C                the K-th column, if pivoting was requested.
C
C     LINPACK.  This version dated 08/14/78 .
C     G. W. Stewart, University of Maryland, Argonne National Lab.
C
C     DQRDC uses the following functions and subprograms.
C
C     BLAS DAXPY,DDOT,DSCAL,DSWAP,DNRM2
C     Fortran DABS,DMAX1,MIN0,DSQRT
C***REFERENCES  DONGARRA J.J., BUNCH J.R., MOLER C.B., STEWART G.W.,
C                 *LINPACK USERS  GUIDE*, SIAM, 1979.
C***ROUTINES CALLED  DAXPY,DDOT,DNRM2,DSCAL,DSWAP
C***END PROLOGUE  DQRDC
      INTEGER LDX,N,P,JOB
      INTEGER JPVT(*)
      DOUBLE PRECISION X(LDX,*),QRAUX(*),WORK(*)
C
      INTEGER J,JP,L,LP1,LUP,MAXJ,PL,PU
      DOUBLE PRECISION MAXNRM,DNRM2,TT
      DOUBLE PRECISION DDOT,NRMXL,T
      LOGICAL NEGJ,SWAPJ
C
C***FIRST EXECUTABLE STATEMENT  DQRDC
      PL = 1
      PU = 0
      IF (JOB .EQ. 0) GO TO 60
C
C        PIVOTING HAS BEEN REQUESTED.  REARRANGE THE COLUMNS
C        ACCORDING TO JPVT.
C
         DO 20 J = 1, P
            SWAPJ = JPVT(J) .GT. 0
            NEGJ = JPVT(J) .LT. 0
            JPVT(J) = J
            IF (NEGJ) JPVT(J) = -J
            IF (.NOT.SWAPJ) GO TO 10
               IF (J .NE. PL) CALL DSWAP(N,X(1,PL),1,X(1,J),1)
               JPVT(J) = JPVT(PL)
               JPVT(PL) = J
               PL = PL + 1
   10       CONTINUE
   20    CONTINUE
         PU = P
         DO 50 JJ = 1, P
            J = P - JJ + 1
            IF (JPVT(J) .GE. 0) GO TO 40
               JPVT(J) = -JPVT(J)
               IF (J .EQ. PU) GO TO 30
                  CALL DSWAP(N,X(1,PU),1,X(1,J),1)
                  JP = JPVT(PU)
                  JPVT(PU) = JPVT(J)
                  JPVT(J) = JP
   30          CONTINUE
               PU = PU - 1
   40       CONTINUE
   50    CONTINUE
   60 CONTINUE
C
C     COMPUTE THE NORMS OF THE FREE COLUMNS.
C
      IF (PU .LT. PL) GO TO 80
      DO 70 J = PL, PU
         QRAUX(J) = DNRM2(N,X(1,J),1)
         WORK(J) = QRAUX(J)
   70 CONTINUE
   80 CONTINUE
C
C     PERFORM THE HOUSEHOLDER REDUCTION OF X.
C
      LUP = MIN0(N,P)
      DO 200 L = 1, LUP
         IF (L .LT. PL .OR. L .GE. PU) GO TO 120
C
C           LOCATE THE COLUMN OF LARGEST NORM AND BRING IT
C           INTO THE PIVOT POSITION.
C
            MAXNRM = 0.0D0
            MAXJ = L
            DO 100 J = L, PU
               IF (QRAUX(J) .LE. MAXNRM) GO TO 90
                  MAXNRM = QRAUX(J)
                  MAXJ = J
   90          CONTINUE
  100       CONTINUE
            IF (MAXJ .EQ. L) GO TO 110
               CALL DSWAP(N,X(1,L),1,X(1,MAXJ),1)
               QRAUX(MAXJ) = QRAUX(L)
               WORK(MAXJ) = WORK(L)
               JP = JPVT(MAXJ)
               JPVT(MAXJ) = JPVT(L)
               JPVT(L) = JP
  110       CONTINUE
  120    CONTINUE
         QRAUX(L) = 0.0D0
         IF (L .EQ. N) GO TO 190
C
C           COMPUTE THE HOUSEHOLDER TRANSFORMATION FOR COLUMN L.
C
            NRMXL = DNRM2(N-L+1,X(L,L),1)
            IF (NRMXL .EQ. 0.0D0) GO TO 180
               IF (X(L,L) .NE. 0.0D0) NRMXL = DSIGN(NRMXL,X(L,L))
               CALL DSCAL(N-L+1,1.0D0/NRMXL,X(L,L),1)
               X(L,L) = 1.0D0 + X(L,L)
C
C              APPLY THE TRANSFORMATION TO THE REMAINING COLUMNS,
C              UPDATING THE NORMS.
C
               LP1 = L + 1
               IF (P .LT. LP1) GO TO 170
               DO 160 J = LP1, P
                  T = -DDOT(N-L+1,X(L,L),1,X(L,J),1)/X(L,L)
                  CALL DAXPY(N-L+1,T,X(L,L),1,X(L,J),1)
                  IF (J .LT. PL .OR. J .GT. PU) GO TO 150
                  IF (QRAUX(J) .EQ. 0.0D0) GO TO 150
                     TT = 1.0D0 - (DABS(X(L,J))/QRAUX(J))**2
                     TT = DMAX1(TT,0.0D0)
                     T = TT
                     TT = 1.0D0 + 0.05D0*TT*(QRAUX(J)/WORK(J))**2
                     IF (TT .EQ. 1.0D0) GO TO 130
                        QRAUX(J) = QRAUX(J)*DSQRT(T)
                     GO TO 140
  130                CONTINUE
                        QRAUX(J) = DNRM2(N-L,X(L+1,J),1)
                        WORK(J) = QRAUX(J)
  140                CONTINUE
  150             CONTINUE
  160          CONTINUE
  170          CONTINUE
C
C              SAVE THE TRANSFORMATION.
C
               QRAUX(L) = X(L,L)
               X(L,L) = -NRMXL
  180       CONTINUE
  190    CONTINUE
  200 CONTINUE
      RETURN
      END
      SUBROUTINE DQRSL(X,LDX,N,K,QRAUX,Y,QY,QTY,B,RSD,XB,JOB,INFO)
C***BEGIN PROLOGUE  DQRSL
C***DATE WRITTEN   780814   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***REVISION HISTORY  (YYMMDD)
C   000330  Modified array declarations.  (JEC)
C***CATEGORY NO.  D9,D2A1
C***KEYWORDS  DOUBLE PRECISION,LINEAR ALGEBRA,LINPACK,MATRIX,
C             ORTHOGONAL TRIANGULAR,SOLVE
C***AUTHOR  STEWART, G. W., (U. OF MARYLAND)
C***PURPOSE  Applies the output of DQRDC to compute coordinate
C            transformations, projections, and least squares solutions.
C***DESCRIPTION
C
C     DQRSL applies the output of DQRDC to compute coordinate
C     transformations, projections, and least squares solutions.
C     For K .LE. MIN(N,P), let XK be the matrix
C
C            XK = (X(JPVT(1)),X(JPVT(2)), ... ,X(JPVT(K)))
C
C     formed from columnns JPVT(1), ... ,JPVT(K) of the original
C     N X P matrix X that was input to DQRDC (if no pivoting was
C     done, XK consists of the first K columns of X in their
C     original order).  DQRDC produces a factored orthogonal matrix Q
C     and an upper triangular matrix R such that
C
C              XK = Q * (R)
C                       (0)
C
C     This information is contained in coded form in the arrays
C     X and QRAUX.
C
C     On Entry
C
C        X      DOUBLE PRECISION(LDX,P).
C               X contains the output of DQRDC.
C
C        LDX    INTEGER.
C               LDX is the leading dimension of the array X.
C
C        N      INTEGER.
C               N is the number of rows of the matrix XK.  It must
C               have the same value as N in DQRDC.
C
C        K      INTEGER.
C               K is the number of columns of the matrix XK.  K
C               must not be greater than MIN(N,P), where P is the
C               same as in the calling sequence to DQRDC.
C
C        QRAUX  DOUBLE PRECISION(P).
C               QRAUX contains the auxiliary output from DQRDC.
C
C        Y      DOUBLE PRECISION(N)
C               Y contains an N-vector that is to be manipulated
C               by DQRSL.
C
C        JOB    INTEGER.
C               JOB specifies what is to be computed.  JOB has
C               the decimal expansion ABCDE, with the following
C               meaning.
C
C                    If A .NE. 0, compute QY.
C                    If B,C,D, or E .NE. 0, compute QTY.
C                    If C .NE. 0, compute B.
C                    If D .NE. 0, compute RSD.
C                    If E .NE. 0, compute XB.
C
C               Note that a request to compute B, RSD, or XB
C               automatically triggers the computation of QTY, for
C               which an array must be provided in the calling
C               sequence.
C
C     On Return
C
C        QY     DOUBLE PRECISION(N).
C               QY contains Q*Y, if its computation has been
C               requested.
C
C        QTY    DOUBLE PRECISION(N).
C               QTY contains TRANS(Q)*Y, if its computation has
C               been requested.  Here TRANS(Q) is the
C               transpose of the matrix Q.
C
C        B      DOUBLE PRECISION(K)
C               B contains the solution of the least squares problem
C
C                    minimize norm2(Y - XK*B),
C
C               if its computation has been requested.  (Note that
C               if pivoting was requested in DQRDC, the J-th
C               component of B will be associated with column JPVT(J)
C               of the original matrix X that was input into DQRDC.)
C
C        RSD    DOUBLE PRECISION(N).
C               RSD contains the least squares residual Y - XK*B,
C               if its computation has been requested.  RSD is
C               also the orthogonal projection of Y onto the
C               orthogonal complement of the column space of XK.
C
C        XB     DOUBLE PRECISION(N).
C               XB contains the least squares approximation XK*B,
C               if its computation has been requested.  XB is also
C               the orthogonal projection of Y onto the column space
C               of X.
C
C        INFO   INTEGER.
C               INFO is zero unless the computation of B has
C               been requested and R is exactly singular.  In
C               this case, INFO is the index of the first zero
C               diagonal element of R and B is left unaltered.
C
C     The parameters QY, QTY, B, RSD, and XB are not referenced
C     if their computation is not requested and in this case
C     can be replaced by dummy variables in the calling program.
C     To save storage, the user may in some cases use the same
C     array for different parameters in the calling sequence.  A
C     frequently occuring example is when one wishes to compute
C     any of B, RSD, or XB and does not need Y or QTY.  In this
C     case one may identify Y, QTY, and one of B, RSD, or XB, while
C     providing separate arrays for anything else that is to be
C     computed.  Thus the calling sequence
C
C          CALL DQRSL(X,LDX,N,K,QRAUX,Y,DUM,Y,B,Y,DUM,110,INFO)
C
C     will result in the computation of B and RSD, with RSD
C     overwriting Y.  More generally, each item in the following
C     list contains groups of permissible identifications for
C     a single calling sequence.
C
C          1. (Y,QTY,B) (RSD) (XB) (QY)
C
C          2. (Y,QTY,RSD) (B) (XB) (QY)
C
C          3. (Y,QTY,XB) (B) (RSD) (QY)
C
C          4. (Y,QY) (QTY,B) (RSD) (XB)
C
C          5. (Y,QY) (QTY,RSD) (B) (XB)
C
C          6. (Y,QY) (QTY,XB) (B) (RSD)
C
C     In any group the value returned in the array allocated to
C     the group corresponds to the last member of the group.
C
C     LINPACK.  This version dated 08/14/78 .
C     G. W. Stewart, University of Maryland, Argonne National Lab.
C
C     DQRSL uses the following functions and subprograms.
C
C     BLAS DAXPY,DCOPY,DDOT
C     Fortran DABS,MIN0,MOD
C***REFERENCES  DONGARRA J.J., BUNCH J.R., MOLER C.B., STEWART G.W.,
C                 *LINPACK USERS  GUIDE*, SIAM, 1979.
C***ROUTINES CALLED  DAXPY,DCOPY,DDOT
C***END PROLOGUE  DQRSL
      INTEGER LDX,N,K,JOB,INFO
      DOUBLE PRECISION X(LDX,*),QRAUX(*),Y(*),QY(*),QTY(*),B(*),RSD(*),
     1                 XB(*)
C
      INTEGER I,J,JJ,JU,KP1
      DOUBLE PRECISION DDOT,T,TEMP
      LOGICAL CB,CQY,CQTY,CR,CXB
C
C     SET INFO FLAG.
C
C***FIRST EXECUTABLE STATEMENT  DQRSL
      INFO = 0
C
C     DETERMINE WHAT IS TO BE COMPUTED.
C
      CQY = JOB/10000 .NE. 0
      CQTY = MOD(JOB,10000) .NE. 0
      CB = MOD(JOB,1000)/100 .NE. 0
      CR = MOD(JOB,100)/10 .NE. 0
      CXB = MOD(JOB,10) .NE. 0
      JU = MIN0(K,N-1)
C
C     SPECIAL ACTION WHEN N=1.
C
      IF (JU .NE. 0) GO TO 40
         IF (CQY) QY(1) = Y(1)
         IF (CQTY) QTY(1) = Y(1)
         IF (CXB) XB(1) = Y(1)
         IF (.NOT.CB) GO TO 30
            IF (X(1,1) .NE. 0.0D0) GO TO 10
               INFO = 1
            GO TO 20
   10       CONTINUE
               B(1) = Y(1)/X(1,1)
   20       CONTINUE
   30    CONTINUE
         IF (CR) RSD(1) = 0.0D0
      GO TO 250
   40 CONTINUE
C
C        SET UP TO COMPUTE QY OR QTY.
C
         IF (CQY) CALL DCOPY(N,Y,1,QY,1)
         IF (CQTY) CALL DCOPY(N,Y,1,QTY,1)
         IF (.NOT.CQY) GO TO 70
C
C           COMPUTE QY.
C
            DO 60 JJ = 1, JU
               J = JU - JJ + 1
               IF (QRAUX(J) .EQ. 0.0D0) GO TO 50
                  TEMP = X(J,J)
                  X(J,J) = QRAUX(J)
                  T = -DDOT(N-J+1,X(J,J),1,QY(J),1)/X(J,J)
                  CALL DAXPY(N-J+1,T,X(J,J),1,QY(J),1)
                  X(J,J) = TEMP
   50          CONTINUE
   60       CONTINUE
   70    CONTINUE
         IF (.NOT.CQTY) GO TO 100
C
C           COMPUTE TRANS(Q)*Y.
C
            DO 90 J = 1, JU
               IF (QRAUX(J) .EQ. 0.0D0) GO TO 80
                  TEMP = X(J,J)
                  X(J,J) = QRAUX(J)
                  T = -DDOT(N-J+1,X(J,J),1,QTY(J),1)/X(J,J)
                  CALL DAXPY(N-J+1,T,X(J,J),1,QTY(J),1)
                  X(J,J) = TEMP
   80          CONTINUE
   90       CONTINUE
  100    CONTINUE
C
C        SET UP TO COMPUTE B, RSD, OR XB.
C
         IF (CB) CALL DCOPY(K,QTY,1,B,1)
         KP1 = K + 1
         IF (CXB) CALL DCOPY(K,QTY,1,XB,1)
         IF (CR .AND. K .LT. N) CALL DCOPY(N-K,QTY(KP1),1,RSD(KP1),1)
         IF (.NOT.CXB .OR. KP1 .GT. N) GO TO 120
            DO 110 I = KP1, N
               XB(I) = 0.0D0
  110       CONTINUE
  120    CONTINUE
         IF (.NOT.CR) GO TO 140
            DO 130 I = 1, K
               RSD(I) = 0.0D0
  130       CONTINUE
  140    CONTINUE
         IF (.NOT.CB) GO TO 190
C
C           COMPUTE B.
C
            DO 170 JJ = 1, K
               J = K - JJ + 1
               IF (X(J,J) .NE. 0.0D0) GO TO 150
                  INFO = J
C           ......EXIT
                  GO TO 180
  150          CONTINUE
               B(J) = B(J)/X(J,J)
               IF (J .EQ. 1) GO TO 160
                  T = -B(J)
                  CALL DAXPY(J-1,T,X(1,J),1,B,1)
  160          CONTINUE
  170       CONTINUE
  180       CONTINUE
  190    CONTINUE
         IF (.NOT.CR .AND. .NOT.CXB) GO TO 240
C
C           COMPUTE RSD OR XB AS REQUIRED.
C
            DO 230 JJ = 1, JU
               J = JU - JJ + 1
               IF (QRAUX(J) .EQ. 0.0D0) GO TO 220
                  TEMP = X(J,J)
                  X(J,J) = QRAUX(J)
                  IF (.NOT.CR) GO TO 200
                     T = -DDOT(N-J+1,X(J,J),1,RSD(J),1)/X(J,J)
                     CALL DAXPY(N-J+1,T,X(J,J),1,RSD(J),1)
  200             CONTINUE
                  IF (.NOT.CXB) GO TO 210
                     T = -DDOT(N-J+1,X(J,J),1,XB(J),1)/X(J,J)
                     CALL DAXPY(N-J+1,T,X(J,J),1,XB(J),1)
  210             CONTINUE
                  X(J,J) = TEMP
  220          CONTINUE
  230       CONTINUE
  240    CONTINUE
  250 CONTINUE
      RETURN
      END
      SUBROUTINE DRAW0(X,Y,N,ID,
     1XTEMP,YTEMP,TATEMP,NTEMP,NTRACE,
     1IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--XX
C
C     WRITTEN BY--DAVID W. BEHRINGER NOAA/AOML (MIAMI).
C                 AS PART OF NOAA'S CONCX V.3   MARCH 1988.
C     ORIGINAL VERSION (IN DATAPLOT)--AUGUST    1988.
C
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      DIMENSION X(*)
      DIMENSION Y(*)
C
      DIMENSION XTEMP(*)
      DIMENSION YTEMP(*)
      DIMENSION TATEMP(*)
C
C-----START POINT-----------------------------------------------------
C
CCCCC IF (ID.NE.0) CALL GDASH(ID)    AUGUST 1988
      IF (N.GT.1) CALL GVECT(X,Y,N,
     1XTEMP,YTEMP,TATEMP,NTEMP,NTRACE,
     1IBUGG3,ISUBRO,IERROR)
      N=1
C
      RETURN
      END
      SUBROUTINE DRAWL(X,Y,DST,N,IDSH,CHR,NCHR,SZ,DL0,
     1XTEMP,YTEMP,TATEMP,NTEMP,NTRACE,
     1IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--XX
C
C     WRITTEN BY--DAVID W. BEHRINGER NOAA/AOML (MIAMI).
C                 AS PART OF NOAA'S CONCX V.3   MARCH 1988.
C     ORIGINAL VERSION (IN DATAPLOT)--AUGUST    1988.
C     UPDATED                         MAY       1989.  TRIG DEGREE FUNCTIONS
C
C---------------------------------------------------------------------
C
CCCCC CHARACTER CHR(NCHR)*1    AUGUST 1988
      CHARACTER CHR(15)*1
C
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
CCCCC DIMENSION X(N),Y(N),DST(N),XL(3),YL(3)
C
      DIMENSION X(*)
      DIMENSION Y(*)
      DIMENSION DST(*)
C
      DIMENSION XTEMP(*)
      DIMENSION YTEMP(*)
      DIMENSION TATEMP(*)
C
      DIMENSION XL(3)
      DIMENSION YL(3)
C
      DATA EPS,RSSX/0.01,1.15/
C     DATA EPS,RSSX/0.01,1.05/
C
C-----START POINT-----------------------------------------------------
C
CCCCC THE FOLLOWING LINE WAS ADDED TO FIX         MAY 1989
CCCCC THE TRIG DEGREES FUNCTIONS PROBLEM          MAY 1989
      CONDR=3.1415926/180.0
C
      SZ2=0.5*SZ
      SSZ=SZ*FLOAT(NCHR)+SZ2
      SSZ2=0.5*SSZ
      CHR(NCHR+1)='$'
      DST(1)=0.
      DO100I=2,N
        DST(I)=DST(I-1)+SQRT((X(I)-X(I-1))**2+(Y(I)-Y(I-1))**2)
 100  CONTINUE
      I=2
 200  CONTINUE
      IF(I.GT.N)GOTO299
CCCCC DO WHILE (I.LE.N)                  JANUARY 1989
        IF (DST(I).LE.DST(I-1)) THEN
          N=N-1
          DO210J=I,N
            DST(J)=DST(J+1)
            X(J)=X(J+1)
            Y(J)=Y(J+1)
 210      CONTINUE
        ELSE
          I=I+1
        END IF
      GOTO200
 299  CONTINUE
      IF (DST(N).LT.EPS.OR.N.EQ.1) THEN
        N=1
        GOTO9000
      END IF
      NLBL=MAX0(INT(DST(N)/DL0),1)
      SEP=SQRT((X(N)-X(1))**2+(Y(N)-Y(1))**2)
      DL=DST(N)/FLOAT(NLBL)
      IF (NLBL.EQ.1.AND.SEP.LT.EPS.AND.DST(N).LT.2.*SSZ) THEN
        IR=1
        DO300I=2,N
          IF(X(I).GT.X(IR)) IR=I
 300    CONTINUE
CCCCC   CALL GCHARJ(4)   AUGUST 1988
        CALL GCHAR(CHR,X(IR)+SSZ2,Y(IR),SZ,
     1IBUGG3,ISUBRO,IERROR)
        CALL DRAW0(X,Y,N,IDSH,
     1XTEMP,YTEMP,TATEMP,NTEMP,NTRACE,
     1IBUGG3,ISUBRO,IERROR)
        GOTO9000
      END IF
      IE=2
      XE=X(1)
      YE=Y(1)
      DO400NL=1,NLBL
        L=0
        DLBL0=FLOAT(NL-1)*DL
        DLBL=(FLOAT(NL)-0.5)*DL-0.5*SSZ
        I0=IE
        IEO=IE
        XEO=XE
        YEO=YE
 500    CONTINUE
        IF(L.NE.0.OR.DLBL.LE.DLBL0)GOTO599
CCCCC   DO WHILE (L.EQ.0.AND.DLBL.GT.DLBL0)   JANAURY 1989
          I=I0-1
 600      CONTINUE
          IF(DST(I).GE.DLBL.OR.I.GE.N)GOTO699
CCCCC     DO WHILE (DST(I).LT.DLBL.AND.I.LT.N)   JANUARY 1989
            I=I+1
            GOTO600
 699      CONTINUE
          IF (DST(I).GE.DLBL) THEN
            IS=I-1
            R=(DLBL-DST(IS))/(DST(IS+1)-DST(IS))
            XS=X(IS)+R*(X(IS+1)-X(IS))
            YS=Y(IS)+R*(Y(IS+1)-Y(IS))
            C=SQRT((X(I)-XS)**2+(Y(I)-YS)**2)
            A0=C
 700        CONTINUE
            IF(C.GE.SSZ.OR.I.GE.N)GOTO799
CCCCC       DO WHILE (C.LT.SSZ.AND.I.LT.N)   JANUARY 1989
              A=C
              I=I+1
              C=SQRT((X(I)-XS)**2+(Y(I)-YS)**2)
              GOTO700
 799        CONTINUE
            IF (C.GE.SSZ) THEN
              IE=I
              IF (IE.GT.IS+1) THEN
                B=DST(IE)-DST(IE-1)
                ACS=(A**2+B**2-C**2)/(2.0*B)
                D=ACS+SQRT(SSZ**2-A**2+ACS**2)
                ANG=ATAN2(Y(IE)-Y(IE-1),X(IE)-X(IE-1))
                XE=D*COS(ANG)+X(IE-1)
                YE=D*SIN(ANG)+Y(IE-1)
                RSS=(A0+DST(IE-1)-DST(IS+1)+D)/SSZ
C               RSS=(A+D)/SSZ
              ELSE
                R=SSZ/(DST(IE)-DLBL)
                XE=XS+R*(X(IE)-XS)
                YE=YS+R*(Y(IE)-YS)
                RSS=1.0
              END IF
              IF (RSS.LE.RSSX) THEN
                L=1
                N0=IS-I0+1
                XL(1)=XEO
                YL(1)=YEO
                IF (N0.GT.0) THEN
                  XL(2)=X(IEO)
                  YL(2)=Y(IEO)
                  N2=2
                  CALL DRAW0(XL,YL,N2,IDSH,
     1XTEMP,YTEMP,TATEMP,NTEMP,NTRACE,
     1IBUGG3,ISUBRO,IERROR)
                  XL(1)=X(IS)
                  YL(1)=Y(IS)
                  CALL DRAW0(X(I0),Y(I0),N0,IDSH,
     1XTEMP,YTEMP,TATEMP,NTEMP,NTRACE,
     1IBUGG3,ISUBRO,IERROR)
                END IF
                XL(2)=XS
                YL(2)=YS
                N2=2
                CALL DRAW0(XL,YL,N2,IDSH,
     1XTEMP,YTEMP,TATEMP,NTEMP,NTRACE,
     1IBUGG3,ISUBRO,IERROR)
CCCCC THE FOLLOWING LINE WAS FIXED BY THE       MAY 1989
CCCCC SUCCEEDING 2 LINES                        MAY 1989
CCCCC           ANG=ATAN2D(YE-YS,XE-XS)
                ANGR=ATAN2(YE-YS,XE-XS)
                ANG=ANGR/CONDR
             IF (ANG.GT.90.) THEN
                  IANG=INT(ANG-180.499)
               JST=5
             ELSE IF (ANG.LT.-90.) THEN
                  IANG=INT(ANG+180.499)
               JST=5
             ELSE
                  IANG=INT(ANG+SIGN(0.499,ANG))
               JST=3
             END IF
CCCCC           CALL GCHARA(IANG)  AUGUST 1988
CCCCC           CALL GCHARJ(JST)  AUGUST 1988
CCCCC THE FOLLOWING LINE WAS FIXED BY THE       MAY 1989
CCCCC SUCCEEDING 2 LINES                        MAY 1989
CCCCC           CALL GCHAR(CHR,XS+SZ2*COSD(ANG),YS+SZ2*SIND(ANG),SZ,
                ANGR=ANG*CONDR
                CALL GCHAR(CHR,XS+SZ2*COS(ANGR),YS+SZ2*SIN(ANGR),SZ,
     1IBUGG3,ISUBRO,IERROR)
              ELSE
                DLBL=DLBL-0.5*SSZ
              END IF
            ELSE
              DLBL=DLBL-0.5*SSZ
            END IF
          ELSE
            DLBL=DLBL-0.5*SSZ
          END IF
          GOTO500
 599    CONTINUE
 400  CONTINUE
      XL(1)=XE
      YL(1)=YE
      XL(2)=X(IE)
      YL(2)=Y(IE)
      N2=2
      CALL DRAW0(XL,YL,N2,IDSH,
     1XTEMP,YTEMP,TATEMP,NTEMP,NTRACE,
     1IBUGG3,ISUBRO,IERROR)
      N0=N-IE+1
      CALL DRAW0(X(IE),Y(IE),N0,IDSH,
     1XTEMP,YTEMP,TATEMP,NTEMP,NTRACE,
     1IBUGG3,ISUBRO,IERROR)
      N=1
      GOTO9000
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE DRCIL1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN COMPLEX ITALIC LOWER CASE (PART 1).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
C     DEFINE CHARACTER   2151--LOWER CASE A
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',   6,   5/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',   4,  -2/
      DATA IOPERA(   3),IX(   3),IY(   3)/'DRAW',   3,  -6/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',   3,  -8/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',   4,  -9/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',   7,  -9/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',   9,  -7/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',  10,  -5/
      DATA IOPERA(   9),IX(   9),IY(   9)/'MOVE',   7,   5/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',   5,  -2/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',   4,  -6/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',   4,  -8/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',   5,  -9/
      DATA IOPERA(  14),IX(  14),IY(  14)/'MOVE',   4,  -2/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',   4,   1/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',   3,   4/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',   1,   5/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',  -1,   5/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',  -4,   4/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',  -6,   1/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',  -7,  -2/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',  -7,  -5/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',  -6,  -7/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',  -5,  -8/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',  -3,  -9/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',  -1,  -9/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',   1,  -8/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',   3,  -5/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',   4,  -2/
      DATA IOPERA(  30),IX(  30),IY(  30)/'MOVE',  -1,   5/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',  -3,   4/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',  -5,   1/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',  -6,  -2/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',  -6,  -6/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',  -5,  -8/
C
      DATA IXMIND(   1)/ -10/
      DATA IXMAXD(   1)/  11/
      DATA IXDELD(   1)/  21/
      DATA ISTARD(   1)/   1/
      DATA NUMCOO(   1)/  35/
C
C     DEFINE CHARACTER   2152--LOWER CASE B
C
      DATA IOPERA(  36),IX(  36),IY(  36)/'MOVE',  -2,  12/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',  -6,  -1/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',  -6,  -4/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',  -5,  -7/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',  -4,  -8/
      DATA IOPERA(  41),IX(  41),IY(  41)/'MOVE',  -1,  12/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',  -5,  -1/
      DATA IOPERA(  43),IX(  43),IY(  43)/'MOVE',  -5,  -1/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',  -4,   2/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',  -2,   4/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW',   0,   5/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',   2,   5/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',   4,   4/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',   5,   3/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW',   6,   1/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW',   6,  -2/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',   5,  -5/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',   3,  -8/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',   0,  -9/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',  -2,  -9/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',  -4,  -8/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',  -5,  -5/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',  -5,  -1/
      DATA IOPERA(  59),IX(  59),IY(  59)/'MOVE',   4,   4/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW',   5,   2/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',   5,  -2/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',   4,  -5/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',   2,  -8/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',   0,  -9/
      DATA IOPERA(  65),IX(  65),IY(  65)/'MOVE',  -5,  12/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW',  -1,  12/
C
      DATA IXMIND(   2)/ -10/
      DATA IXMAXD(   2)/   9/
      DATA IXDELD(   2)/  19/
      DATA ISTARD(   2)/  36/
      DATA NUMCOO(   2)/  31/
C
C     DEFINE CHARACTER   2153--LOWER CASE C
C
      DATA IOPERA(  67),IX(  67),IY(  67)/'MOVE',   5,   2/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',   5,   1/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',   6,   1/
      DATA IOPERA(  70),IX(  70),IY(  70)/'DRAW',   6,   2/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',   5,   4/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',   3,   5/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',   0,   5/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',  -3,   4/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',  -5,   1/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',  -6,  -2/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',  -6,  -5/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',  -5,  -7/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',  -4,  -8/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',  -2,  -9/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',   0,  -9/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',   3,  -8/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',   5,  -5/
      DATA IOPERA(  84),IX(  84),IY(  84)/'MOVE',   0,   5/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',  -2,   4/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',  -4,   1/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',  -5,  -2/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',  -5,  -6/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',  -4,  -8/
C
      DATA IXMIND(   3)/  -9/
      DATA IXMAXD(   3)/   9/
      DATA IXDELD(   3)/  18/
      DATA ISTARD(   3)/  67/
      DATA NUMCOO(   3)/  23/
C
C     DEFINE CHARACTER   2154--LOWER CASE D
C
      DATA IOPERA(  90),IX(  90),IY(  90)/'MOVE',   8,  12/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',   4,  -2/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW',   3,  -6/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',   3,  -8/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',   4,  -9/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',   7,  -9/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',   9,  -7/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',  10,  -5/
      DATA IOPERA(  98),IX(  98),IY(  98)/'MOVE',   9,  12/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',   5,  -2/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',   4,  -6/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',   4,  -8/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',   5,  -9/
      DATA IOPERA( 103),IX( 103),IY( 103)/'MOVE',   4,  -2/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',   4,   1/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',   3,   4/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',   1,   5/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',  -1,   5/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',  -4,   4/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',  -6,   1/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',  -7,  -2/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',  -7,  -5/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',  -6,  -7/
      DATA IOPERA( 113),IX( 113),IY( 113)/'DRAW',  -5,  -8/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',  -3,  -9/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',  -1,  -9/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',   1,  -8/
      DATA IOPERA( 117),IX( 117),IY( 117)/'DRAW',   3,  -5/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',   4,  -2/
      DATA IOPERA( 119),IX( 119),IY( 119)/'MOVE',  -1,   5/
      DATA IOPERA( 120),IX( 120),IY( 120)/'DRAW',  -3,   4/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW',  -5,   1/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW',  -6,  -2/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',  -6,  -6/
      DATA IOPERA( 124),IX( 124),IY( 124)/'DRAW',  -5,  -8/
      DATA IOPERA( 125),IX( 125),IY( 125)/'MOVE',   5,  12/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW',   9,  12/
C
      DATA IXMIND(   4)/ -10/
      DATA IXMAXD(   4)/  11/
      DATA IXDELD(   4)/  21/
      DATA ISTARD(   4)/  90/
      DATA NUMCOO(   4)/  37/
C
C     DEFINE CHARACTER   2155--LOWER CASE E
C
      DATA IOPERA( 127),IX( 127),IY( 127)/'MOVE',  -5,  -4/
      DATA IOPERA( 128),IX( 128),IY( 128)/'DRAW',  -1,  -3/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',   2,  -2/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',   5,   0/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',   6,   2/
      DATA IOPERA( 132),IX( 132),IY( 132)/'DRAW',   5,   4/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',   3,   5/
      DATA IOPERA( 134),IX( 134),IY( 134)/'DRAW',   0,   5/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',  -3,   4/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',  -5,   1/
      DATA IOPERA( 137),IX( 137),IY( 137)/'DRAW',  -6,  -2/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW',  -6,  -5/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW',  -5,  -7/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW',  -4,  -8/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',  -2,  -9/
      DATA IOPERA( 142),IX( 142),IY( 142)/'DRAW',   0,  -9/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',   3,  -8/
      DATA IOPERA( 144),IX( 144),IY( 144)/'DRAW',   5,  -6/
      DATA IOPERA( 145),IX( 145),IY( 145)/'MOVE',   0,   5/
      DATA IOPERA( 146),IX( 146),IY( 146)/'DRAW',  -2,   4/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',  -4,   1/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',  -5,  -2/
      DATA IOPERA( 149),IX( 149),IY( 149)/'DRAW',  -5,  -6/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',  -4,  -8/
C
      DATA IXMIND(   5)/  -9/
      DATA IXMAXD(   5)/   9/
      DATA IXDELD(   5)/  18/
      DATA ISTARD(   5)/ 127/
      DATA NUMCOO(   5)/  24/
C
C     DEFINE CHARACTER   2156--LOWER CASE F
C
      DATA IOPERA( 151),IX( 151),IY( 151)/'MOVE',   8,  11/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',   7,  10/
      DATA IOPERA( 153),IX( 153),IY( 153)/'DRAW',   8,   9/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',   9,  10/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW',   9,  11/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',   8,  12/
      DATA IOPERA( 157),IX( 157),IY( 157)/'DRAW',   6,  12/
      DATA IOPERA( 158),IX( 158),IY( 158)/'DRAW',   4,  11/
      DATA IOPERA( 159),IX( 159),IY( 159)/'DRAW',   3,  10/
      DATA IOPERA( 160),IX( 160),IY( 160)/'DRAW',   2,   8/
      DATA IOPERA( 161),IX( 161),IY( 161)/'DRAW',   1,   5/
      DATA IOPERA( 162),IX( 162),IY( 162)/'DRAW',  -2,  -9/
      DATA IOPERA( 163),IX( 163),IY( 163)/'DRAW',  -3, -13/
      DATA IOPERA( 164),IX( 164),IY( 164)/'DRAW',  -4, -15/
      DATA IOPERA( 165),IX( 165),IY( 165)/'MOVE',   6,  12/
      DATA IOPERA( 166),IX( 166),IY( 166)/'DRAW',   4,  10/
      DATA IOPERA( 167),IX( 167),IY( 167)/'DRAW',   3,   8/
      DATA IOPERA( 168),IX( 168),IY( 168)/'DRAW',   2,   4/
      DATA IOPERA( 169),IX( 169),IY( 169)/'DRAW',   0,  -5/
      DATA IOPERA( 170),IX( 170),IY( 170)/'DRAW',  -1,  -9/
      DATA IOPERA( 171),IX( 171),IY( 171)/'DRAW',  -2, -12/
      DATA IOPERA( 172),IX( 172),IY( 172)/'DRAW',  -3, -14/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW',  -4, -15/
      DATA IOPERA( 174),IX( 174),IY( 174)/'DRAW',  -6, -16/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW',  -8, -16/
      DATA IOPERA( 176),IX( 176),IY( 176)/'DRAW',  -9, -15/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW',  -9, -14/
      DATA IOPERA( 178),IX( 178),IY( 178)/'DRAW',  -8, -13/
      DATA IOPERA( 179),IX( 179),IY( 179)/'DRAW',  -7, -14/
      DATA IOPERA( 180),IX( 180),IY( 180)/'DRAW',  -8, -15/
      DATA IOPERA( 181),IX( 181),IY( 181)/'MOVE',  -3,   5/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',   7,   5/
C
      DATA IXMIND(   6)/  -7/
      DATA IXMAXD(   6)/   8/
      DATA IXDELD(   6)/  15/
      DATA ISTARD(   6)/ 151/
      DATA NUMCOO(   6)/  32/
C
C     DEFINE CHARACTER   2157--LOWER CASE G
C
      DATA IOPERA( 183),IX( 183),IY( 183)/'MOVE',   7,   5/
      DATA IOPERA( 184),IX( 184),IY( 184)/'DRAW',   3,  -9/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',   2, -12/
      DATA IOPERA( 186),IX( 186),IY( 186)/'DRAW',   0, -15/
      DATA IOPERA( 187),IX( 187),IY( 187)/'DRAW',  -3, -16/
      DATA IOPERA( 188),IX( 188),IY( 188)/'DRAW',  -6, -16/
      DATA IOPERA( 189),IX( 189),IY( 189)/'DRAW',  -8, -15/
      DATA IOPERA( 190),IX( 190),IY( 190)/'DRAW',  -9, -14/
      DATA IOPERA( 191),IX( 191),IY( 191)/'DRAW',  -9, -13/
      DATA IOPERA( 192),IX( 192),IY( 192)/'DRAW',  -8, -12/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW',  -7, -13/
      DATA IOPERA( 194),IX( 194),IY( 194)/'DRAW',  -8, -14/
      DATA IOPERA( 195),IX( 195),IY( 195)/'MOVE',   6,   5/
      DATA IOPERA( 196),IX( 196),IY( 196)/'DRAW',   2,  -9/
      DATA IOPERA( 197),IX( 197),IY( 197)/'DRAW',   1, -12/
      DATA IOPERA( 198),IX( 198),IY( 198)/'DRAW',  -1, -15/
      DATA IOPERA( 199),IX( 199),IY( 199)/'DRAW',  -3, -16/
      DATA IOPERA( 200),IX( 200),IY( 200)/'MOVE',   4,  -2/
      DATA IOPERA( 201),IX( 201),IY( 201)/'DRAW',   4,   1/
      DATA IOPERA( 202),IX( 202),IY( 202)/'DRAW',   3,   4/
      DATA IOPERA( 203),IX( 203),IY( 203)/'DRAW',   1,   5/
      DATA IOPERA( 204),IX( 204),IY( 204)/'DRAW',  -1,   5/
      DATA IOPERA( 205),IX( 205),IY( 205)/'DRAW',  -4,   4/
      DATA IOPERA( 206),IX( 206),IY( 206)/'DRAW',  -6,   1/
      DATA IOPERA( 207),IX( 207),IY( 207)/'DRAW',  -7,  -2/
      DATA IOPERA( 208),IX( 208),IY( 208)/'DRAW',  -7,  -5/
      DATA IOPERA( 209),IX( 209),IY( 209)/'DRAW',  -6,  -7/
      DATA IOPERA( 210),IX( 210),IY( 210)/'DRAW',  -5,  -8/
      DATA IOPERA( 211),IX( 211),IY( 211)/'DRAW',  -3,  -9/
      DATA IOPERA( 212),IX( 212),IY( 212)/'DRAW',  -1,  -9/
      DATA IOPERA( 213),IX( 213),IY( 213)/'DRAW',   1,  -8/
      DATA IOPERA( 214),IX( 214),IY( 214)/'DRAW',   3,  -5/
      DATA IOPERA( 215),IX( 215),IY( 215)/'DRAW',   4,  -2/
      DATA IOPERA( 216),IX( 216),IY( 216)/'MOVE',  -1,   5/
      DATA IOPERA( 217),IX( 217),IY( 217)/'DRAW',  -3,   4/
      DATA IOPERA( 218),IX( 218),IY( 218)/'DRAW',  -5,   1/
      DATA IOPERA( 219),IX( 219),IY( 219)/'DRAW',  -6,  -2/
      DATA IOPERA( 220),IX( 220),IY( 220)/'DRAW',  -6,  -6/
      DATA IOPERA( 221),IX( 221),IY( 221)/'DRAW',  -5,  -8/
C
      DATA IXMIND(   7)/ -10/
      DATA IXMAXD(   7)/  10/
      DATA IXDELD(   7)/  20/
      DATA ISTARD(   7)/ 183/
      DATA NUMCOO(   7)/  39/
C
C     DEFINE CHARACTER   2158--LOWER CASE H
C
      DATA IOPERA( 222),IX( 222),IY( 222)/'MOVE',  -2,  12/
      DATA IOPERA( 223),IX( 223),IY( 223)/'DRAW',  -8,  -9/
      DATA IOPERA( 224),IX( 224),IY( 224)/'MOVE',  -1,  12/
      DATA IOPERA( 225),IX( 225),IY( 225)/'DRAW',  -7,  -9/
      DATA IOPERA( 226),IX( 226),IY( 226)/'MOVE',  -5,  -2/
      DATA IOPERA( 227),IX( 227),IY( 227)/'DRAW',  -3,   2/
      DATA IOPERA( 228),IX( 228),IY( 228)/'DRAW',  -1,   4/
      DATA IOPERA( 229),IX( 229),IY( 229)/'DRAW',   1,   5/
      DATA IOPERA( 230),IX( 230),IY( 230)/'DRAW',   3,   5/
      DATA IOPERA( 231),IX( 231),IY( 231)/'DRAW',   5,   4/
      DATA IOPERA( 232),IX( 232),IY( 232)/'DRAW',   6,   3/
      DATA IOPERA( 233),IX( 233),IY( 233)/'DRAW',   6,   1/
      DATA IOPERA( 234),IX( 234),IY( 234)/'DRAW',   4,  -5/
      DATA IOPERA( 235),IX( 235),IY( 235)/'DRAW',   4,  -8/
      DATA IOPERA( 236),IX( 236),IY( 236)/'DRAW',   5,  -9/
      DATA IOPERA( 237),IX( 237),IY( 237)/'MOVE',   3,   5/
      DATA IOPERA( 238),IX( 238),IY( 238)/'DRAW',   5,   3/
      DATA IOPERA( 239),IX( 239),IY( 239)/'DRAW',   5,   1/
      DATA IOPERA( 240),IX( 240),IY( 240)/'DRAW',   3,  -5/
      DATA IOPERA( 241),IX( 241),IY( 241)/'DRAW',   3,  -8/
      DATA IOPERA( 242),IX( 242),IY( 242)/'DRAW',   4,  -9/
      DATA IOPERA( 243),IX( 243),IY( 243)/'DRAW',   7,  -9/
      DATA IOPERA( 244),IX( 244),IY( 244)/'DRAW',   9,  -7/
      DATA IOPERA( 245),IX( 245),IY( 245)/'DRAW',  10,  -5/
      DATA IOPERA( 246),IX( 246),IY( 246)/'MOVE',  -5,  12/
      DATA IOPERA( 247),IX( 247),IY( 247)/'DRAW',  -1,  12/
C
      DATA IXMIND(   8)/ -10/
      DATA IXMAXD(   8)/  11/
      DATA IXDELD(   8)/  21/
      DATA ISTARD(   8)/ 222/
      DATA NUMCOO(   8)/  26/
C
C     DEFINE CHARACTER   2159--LOWER CASE I
C
      DATA IOPERA( 248),IX( 248),IY( 248)/'MOVE',   3,  12/
      DATA IOPERA( 249),IX( 249),IY( 249)/'DRAW',   2,  11/
      DATA IOPERA( 250),IX( 250),IY( 250)/'DRAW',   3,  10/
      DATA IOPERA( 251),IX( 251),IY( 251)/'DRAW',   4,  11/
      DATA IOPERA( 252),IX( 252),IY( 252)/'DRAW',   3,  12/
      DATA IOPERA( 253),IX( 253),IY( 253)/'MOVE',  -5,   1/
      DATA IOPERA( 254),IX( 254),IY( 254)/'DRAW',  -4,   3/
      DATA IOPERA( 255),IX( 255),IY( 255)/'DRAW',  -2,   5/
      DATA IOPERA( 256),IX( 256),IY( 256)/'DRAW',   1,   5/
      DATA IOPERA( 257),IX( 257),IY( 257)/'DRAW',   2,   4/
      DATA IOPERA( 258),IX( 258),IY( 258)/'DRAW',   2,   1/
      DATA IOPERA( 259),IX( 259),IY( 259)/'DRAW',   0,  -5/
      DATA IOPERA( 260),IX( 260),IY( 260)/'DRAW',   0,  -8/
      DATA IOPERA( 261),IX( 261),IY( 261)/'DRAW',   1,  -9/
      DATA IOPERA( 262),IX( 262),IY( 262)/'MOVE',   0,   5/
      DATA IOPERA( 263),IX( 263),IY( 263)/'DRAW',   1,   4/
      DATA IOPERA( 264),IX( 264),IY( 264)/'DRAW',   1,   1/
      DATA IOPERA( 265),IX( 265),IY( 265)/'DRAW',  -1,  -5/
      DATA IOPERA( 266),IX( 266),IY( 266)/'DRAW',  -1,  -8/
      DATA IOPERA( 267),IX( 267),IY( 267)/'DRAW',   0,  -9/
      DATA IOPERA( 268),IX( 268),IY( 268)/'DRAW',   3,  -9/
      DATA IOPERA( 269),IX( 269),IY( 269)/'DRAW',   5,  -7/
      DATA IOPERA( 270),IX( 270),IY( 270)/'DRAW',   6,  -5/
C
      DATA IXMIND(   9)/  -6/
      DATA IXMAXD(   9)/   7/
      DATA IXDELD(   9)/  13/
      DATA ISTARD(   9)/ 248/
      DATA NUMCOO(   9)/  23/
C
C     DEFINE CHARACTER   2160--LOWER CASE J
C
      DATA IOPERA( 271),IX( 271),IY( 271)/'MOVE',   4,  12/
      DATA IOPERA( 272),IX( 272),IY( 272)/'DRAW',   3,  11/
      DATA IOPERA( 273),IX( 273),IY( 273)/'DRAW',   4,  10/
      DATA IOPERA( 274),IX( 274),IY( 274)/'DRAW',   5,  11/
      DATA IOPERA( 275),IX( 275),IY( 275)/'DRAW',   4,  12/
      DATA IOPERA( 276),IX( 276),IY( 276)/'MOVE',  -4,   1/
      DATA IOPERA( 277),IX( 277),IY( 277)/'DRAW',  -3,   3/
      DATA IOPERA( 278),IX( 278),IY( 278)/'DRAW',  -1,   5/
      DATA IOPERA( 279),IX( 279),IY( 279)/'DRAW',   2,   5/
      DATA IOPERA( 280),IX( 280),IY( 280)/'DRAW',   3,   4/
      DATA IOPERA( 281),IX( 281),IY( 281)/'DRAW',   3,   1/
      DATA IOPERA( 282),IX( 282),IY( 282)/'DRAW',   0,  -9/
      DATA IOPERA( 283),IX( 283),IY( 283)/'DRAW',  -1, -12/
      DATA IOPERA( 284),IX( 284),IY( 284)/'DRAW',  -2, -14/
      DATA IOPERA( 285),IX( 285),IY( 285)/'DRAW',  -3, -15/
      DATA IOPERA( 286),IX( 286),IY( 286)/'DRAW',  -5, -16/
      DATA IOPERA( 287),IX( 287),IY( 287)/'DRAW',  -7, -16/
      DATA IOPERA( 288),IX( 288),IY( 288)/'DRAW',  -8, -15/
      DATA IOPERA( 289),IX( 289),IY( 289)/'DRAW',  -8, -14/
      DATA IOPERA( 290),IX( 290),IY( 290)/'DRAW',  -7, -13/
      DATA IOPERA( 291),IX( 291),IY( 291)/'DRAW',  -6, -14/
      DATA IOPERA( 292),IX( 292),IY( 292)/'DRAW',  -7, -15/
      DATA IOPERA( 293),IX( 293),IY( 293)/'MOVE',   1,   5/
      DATA IOPERA( 294),IX( 294),IY( 294)/'DRAW',   2,   4/
      DATA IOPERA( 295),IX( 295),IY( 295)/'DRAW',   2,   1/
      DATA IOPERA( 296),IX( 296),IY( 296)/'DRAW',  -1,  -9/
      DATA IOPERA( 297),IX( 297),IY( 297)/'DRAW',  -2, -12/
      DATA IOPERA( 298),IX( 298),IY( 298)/'DRAW',  -3, -14/
      DATA IOPERA( 299),IX( 299),IY( 299)/'DRAW',  -5, -16/
C
      DATA IXMIND(  10)/  -6/
      DATA IXMAXD(  10)/   7/
      DATA IXDELD(  10)/  13/
      DATA ISTARD(  10)/ 271/
      DATA NUMCOO(  10)/  29/
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCIL1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **************************************
C               **  STEP 2--                        **
C               **  EXTRACT THE COORDINATES         **
C               **  FOR THIS PARTICULAR CHARACTER.  **
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
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DRCIL1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ICHARN
 9013 FORMAT('ICHARN = ',I8)
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
      SUBROUTINE DRCIL2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN COMPLEX ITALIC LOWER CASE (PART 2).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
C     DEFINE CHARACTER   2161--LOWER CASE K
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',  -2,  12/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',  -8,  -9/
      DATA IOPERA(   3),IX(   3),IY(   3)/'MOVE',  -1,  12/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',  -7,  -9/
      DATA IOPERA(   5),IX(   5),IY(   5)/'MOVE',   6,   4/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',   5,   3/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',   6,   2/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',   7,   3/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',   7,   4/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',   6,   5/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',   5,   5/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',   3,   4/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',  -1,   0/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',  -3,  -1/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',  -5,  -1/
      DATA IOPERA(  16),IX(  16),IY(  16)/'MOVE',  -3,  -1/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',  -1,  -2/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',   1,  -8/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',   2,  -9/
      DATA IOPERA(  20),IX(  20),IY(  20)/'MOVE',  -3,  -1/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',  -2,  -2/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',   0,  -8/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',   1,  -9/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',   3,  -9/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',   5,  -8/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',   7,  -5/
      DATA IOPERA(  27),IX(  27),IY(  27)/'MOVE',  -5,  12/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',  -1,  12/
C
      DATA IXMIND(  11)/ -10/
      DATA IXMAXD(  11)/  10/
      DATA IXDELD(  11)/  20/
      DATA ISTARD(  11)/   1/
      DATA NUMCOO(  11)/  28/
C
C     DEFINE CHARACTER   2162--LOWER CASE L
C
      DATA IOPERA(  29),IX(  29),IY(  29)/'MOVE',   3,  12/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',  -1,  -2/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',  -2,  -6/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',  -2,  -8/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',  -1,  -9/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',   2,  -9/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',   4,  -7/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',   5,  -5/
      DATA IOPERA(  37),IX(  37),IY(  37)/'MOVE',   4,  12/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',   0,  -2/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',  -1,  -6/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',  -1,  -8/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',   0,  -9/
      DATA IOPERA(  42),IX(  42),IY(  42)/'MOVE',   0,  12/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',   4,  12/
C
      DATA IXMIND(  12)/  -5/
      DATA IXMAXD(  12)/   7/
      DATA IXDELD(  12)/  12/
      DATA ISTARD(  12)/  29/
      DATA NUMCOO(  12)/  15/
C
C     DEFINE CHARACTER   2163--LOWER CASE M
C
      DATA IOPERA(  44),IX(  44),IY(  44)/'MOVE', -16,   1/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW', -15,   3/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW', -13,   5/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW', -10,   5/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',  -9,   4/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',  -9,   2/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW', -10,  -2/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW', -12,  -9/
      DATA IOPERA(  52),IX(  52),IY(  52)/'MOVE', -11,   5/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW', -10,   4/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW', -10,   2/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW', -11,  -2/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW', -13,  -9/
      DATA IOPERA(  57),IX(  57),IY(  57)/'MOVE', -10,  -2/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',  -8,   2/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',  -6,   4/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW',  -4,   5/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',  -2,   5/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',   0,   4/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',   1,   3/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',   1,   1/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',  -2,  -9/
      DATA IOPERA(  66),IX(  66),IY(  66)/'MOVE',  -2,   5/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',   0,   3/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',   0,   1/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',  -3,  -9/
      DATA IOPERA(  70),IX(  70),IY(  70)/'MOVE',   0,  -2/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',   2,   2/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',   4,   4/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',   6,   5/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',   8,   5/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',  10,   4/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',  11,   3/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',  11,   1/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',   9,  -5/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',   9,  -8/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',  10,  -9/
      DATA IOPERA(  81),IX(  81),IY(  81)/'MOVE',   8,   5/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',  10,   3/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',  10,   1/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW',   8,  -5/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',   8,  -8/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',   9,  -9/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',  12,  -9/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',  14,  -7/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',  15,  -5/
C
      DATA IXMIND(  13)/ -17/
      DATA IXMAXD(  13)/  16/
      DATA IXDELD(  13)/  33/
      DATA ISTARD(  13)/  44/
      DATA NUMCOO(  13)/  46/
C
C     DEFINE CHARACTER   2164--LOWER CASE N
C
      DATA IOPERA(  90),IX(  90),IY(  90)/'MOVE', -11,   1/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW', -10,   3/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW',  -8,   5/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',  -5,   5/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',  -4,   4/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',  -4,   2/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',  -5,  -2/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',  -7,  -9/
      DATA IOPERA(  98),IX(  98),IY(  98)/'MOVE',  -6,   5/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',  -5,   4/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',  -5,   2/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',  -6,  -2/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',  -8,  -9/
      DATA IOPERA( 103),IX( 103),IY( 103)/'MOVE',  -5,  -2/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',  -3,   2/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',  -1,   4/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',   1,   5/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',   3,   5/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',   5,   4/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',   6,   3/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',   6,   1/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',   4,  -5/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',   4,  -8/
      DATA IOPERA( 113),IX( 113),IY( 113)/'DRAW',   5,  -9/
      DATA IOPERA( 114),IX( 114),IY( 114)/'MOVE',   3,   5/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',   5,   3/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',   5,   1/
      DATA IOPERA( 117),IX( 117),IY( 117)/'DRAW',   3,  -5/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',   3,  -8/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',   4,  -9/
      DATA IOPERA( 120),IX( 120),IY( 120)/'DRAW',   7,  -9/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW',   9,  -7/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW',  10,  -5/
C
      DATA IXMIND(  14)/ -12/
      DATA IXMAXD(  14)/  11/
      DATA IXDELD(  14)/  23/
      DATA ISTARD(  14)/  90/
      DATA NUMCOO(  14)/  33/
C
C     DEFINE CHARACTER   2165--LOWER CASE O
C
      DATA IOPERA( 123),IX( 123),IY( 123)/'MOVE',   0,   5/
      DATA IOPERA( 124),IX( 124),IY( 124)/'DRAW',  -3,   4/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',  -5,   1/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW',  -6,  -2/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW',  -6,  -5/
      DATA IOPERA( 128),IX( 128),IY( 128)/'DRAW',  -5,  -7/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',  -4,  -8/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',  -2,  -9/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',   0,  -9/
      DATA IOPERA( 132),IX( 132),IY( 132)/'DRAW',   3,  -8/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',   5,  -5/
      DATA IOPERA( 134),IX( 134),IY( 134)/'DRAW',   6,  -2/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',   6,   1/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',   5,   3/
      DATA IOPERA( 137),IX( 137),IY( 137)/'DRAW',   4,   4/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW',   2,   5/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW',   0,   5/
      DATA IOPERA( 140),IX( 140),IY( 140)/'MOVE',   0,   5/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',  -2,   4/
      DATA IOPERA( 142),IX( 142),IY( 142)/'DRAW',  -4,   1/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',  -5,  -2/
      DATA IOPERA( 144),IX( 144),IY( 144)/'DRAW',  -5,  -6/
      DATA IOPERA( 145),IX( 145),IY( 145)/'DRAW',  -4,  -8/
      DATA IOPERA( 146),IX( 146),IY( 146)/'MOVE',   0,  -9/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',   2,  -8/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',   4,  -5/
      DATA IOPERA( 149),IX( 149),IY( 149)/'DRAW',   5,  -2/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',   5,   2/
      DATA IOPERA( 151),IX( 151),IY( 151)/'DRAW',   4,   4/
C
      DATA IXMIND(  15)/  -9/
      DATA IXMAXD(  15)/   9/
      DATA IXDELD(  15)/  18/
      DATA ISTARD(  15)/ 123/
      DATA NUMCOO(  15)/  29/
C
C     DEFINE CHARACTER   2166--LOWER CASE P
C
      DATA IOPERA( 152),IX( 152),IY( 152)/'MOVE', -10,   1/
      DATA IOPERA( 153),IX( 153),IY( 153)/'DRAW',  -9,   3/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',  -7,   5/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW',  -4,   5/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',  -3,   4/
      DATA IOPERA( 157),IX( 157),IY( 157)/'DRAW',  -3,   2/
      DATA IOPERA( 158),IX( 158),IY( 158)/'DRAW',  -4,  -2/
      DATA IOPERA( 159),IX( 159),IY( 159)/'DRAW',  -8, -16/
      DATA IOPERA( 160),IX( 160),IY( 160)/'MOVE',  -5,   5/
      DATA IOPERA( 161),IX( 161),IY( 161)/'DRAW',  -4,   4/
      DATA IOPERA( 162),IX( 162),IY( 162)/'DRAW',  -4,   2/
      DATA IOPERA( 163),IX( 163),IY( 163)/'DRAW',  -5,  -2/
      DATA IOPERA( 164),IX( 164),IY( 164)/'DRAW',  -9, -16/
      DATA IOPERA( 165),IX( 165),IY( 165)/'MOVE',  -4,  -2/
      DATA IOPERA( 166),IX( 166),IY( 166)/'DRAW',  -3,   1/
      DATA IOPERA( 167),IX( 167),IY( 167)/'DRAW',  -1,   4/
      DATA IOPERA( 168),IX( 168),IY( 168)/'DRAW',   1,   5/
      DATA IOPERA( 169),IX( 169),IY( 169)/'DRAW',   3,   5/
      DATA IOPERA( 170),IX( 170),IY( 170)/'DRAW',   5,   4/
      DATA IOPERA( 171),IX( 171),IY( 171)/'DRAW',   6,   3/
      DATA IOPERA( 172),IX( 172),IY( 172)/'DRAW',   7,   1/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW',   7,  -2/
      DATA IOPERA( 174),IX( 174),IY( 174)/'DRAW',   6,  -5/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW',   4,  -8/
      DATA IOPERA( 176),IX( 176),IY( 176)/'DRAW',   1,  -9/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW',  -1,  -9/
      DATA IOPERA( 178),IX( 178),IY( 178)/'DRAW',  -3,  -8/
      DATA IOPERA( 179),IX( 179),IY( 179)/'DRAW',  -4,  -5/
      DATA IOPERA( 180),IX( 180),IY( 180)/'DRAW',  -4,  -2/
      DATA IOPERA( 181),IX( 181),IY( 181)/'MOVE',   5,   4/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',   6,   2/
      DATA IOPERA( 183),IX( 183),IY( 183)/'DRAW',   6,  -2/
      DATA IOPERA( 184),IX( 184),IY( 184)/'DRAW',   5,  -5/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',   3,  -8/
      DATA IOPERA( 186),IX( 186),IY( 186)/'DRAW',   1,  -9/
      DATA IOPERA( 187),IX( 187),IY( 187)/'MOVE', -12, -16/
      DATA IOPERA( 188),IX( 188),IY( 188)/'DRAW',  -5, -16/
C
      DATA IXMIND(  16)/ -11/
      DATA IXMAXD(  16)/  10/
      DATA IXDELD(  16)/  21/
      DATA ISTARD(  16)/ 152/
      DATA NUMCOO(  16)/  37/
C
C     DEFINE CHARACTER   2167--LOWER CASE Q
C
      DATA IOPERA( 189),IX( 189),IY( 189)/'MOVE',   6,   5/
      DATA IOPERA( 190),IX( 190),IY( 190)/'DRAW',   0, -16/
      DATA IOPERA( 191),IX( 191),IY( 191)/'MOVE',   7,   5/
      DATA IOPERA( 192),IX( 192),IY( 192)/'DRAW',   1, -16/
      DATA IOPERA( 193),IX( 193),IY( 193)/'MOVE',   4,  -2/
      DATA IOPERA( 194),IX( 194),IY( 194)/'DRAW',   4,   1/
      DATA IOPERA( 195),IX( 195),IY( 195)/'DRAW',   3,   4/
      DATA IOPERA( 196),IX( 196),IY( 196)/'DRAW',   1,   5/
      DATA IOPERA( 197),IX( 197),IY( 197)/'DRAW',  -1,   5/
      DATA IOPERA( 198),IX( 198),IY( 198)/'DRAW',  -4,   4/
      DATA IOPERA( 199),IX( 199),IY( 199)/'DRAW',  -6,   1/
      DATA IOPERA( 200),IX( 200),IY( 200)/'DRAW',  -7,  -2/
      DATA IOPERA( 201),IX( 201),IY( 201)/'DRAW',  -7,  -5/
      DATA IOPERA( 202),IX( 202),IY( 202)/'DRAW',  -6,  -7/
      DATA IOPERA( 203),IX( 203),IY( 203)/'DRAW',  -5,  -8/
      DATA IOPERA( 204),IX( 204),IY( 204)/'DRAW',  -3,  -9/
      DATA IOPERA( 205),IX( 205),IY( 205)/'DRAW',  -1,  -9/
      DATA IOPERA( 206),IX( 206),IY( 206)/'DRAW',   1,  -8/
      DATA IOPERA( 207),IX( 207),IY( 207)/'DRAW',   3,  -5/
      DATA IOPERA( 208),IX( 208),IY( 208)/'DRAW',   4,  -2/
      DATA IOPERA( 209),IX( 209),IY( 209)/'MOVE',  -1,   5/
      DATA IOPERA( 210),IX( 210),IY( 210)/'DRAW',  -3,   4/
      DATA IOPERA( 211),IX( 211),IY( 211)/'DRAW',  -5,   1/
      DATA IOPERA( 212),IX( 212),IY( 212)/'DRAW',  -6,  -2/
      DATA IOPERA( 213),IX( 213),IY( 213)/'DRAW',  -6,  -6/
      DATA IOPERA( 214),IX( 214),IY( 214)/'DRAW',  -5,  -8/
      DATA IOPERA( 215),IX( 215),IY( 215)/'MOVE',  -3, -16/
      DATA IOPERA( 216),IX( 216),IY( 216)/'DRAW',   4, -16/
C
      DATA IXMIND(  17)/ -10/
      DATA IXMAXD(  17)/  10/
      DATA IXDELD(  17)/  20/
      DATA ISTARD(  17)/ 189/
      DATA NUMCOO(  17)/  28/
C
C     DEFINE CHARACTER   2168--LOWER CASE R
C
      DATA IOPERA( 217),IX( 217),IY( 217)/'MOVE',  -8,   1/
      DATA IOPERA( 218),IX( 218),IY( 218)/'DRAW',  -7,   3/
      DATA IOPERA( 219),IX( 219),IY( 219)/'DRAW',  -5,   5/
      DATA IOPERA( 220),IX( 220),IY( 220)/'DRAW',  -2,   5/
      DATA IOPERA( 221),IX( 221),IY( 221)/'DRAW',  -1,   4/
      DATA IOPERA( 222),IX( 222),IY( 222)/'DRAW',  -1,   2/
      DATA IOPERA( 223),IX( 223),IY( 223)/'DRAW',  -2,  -2/
      DATA IOPERA( 224),IX( 224),IY( 224)/'DRAW',  -4,  -9/
      DATA IOPERA( 225),IX( 225),IY( 225)/'MOVE',  -3,   5/
      DATA IOPERA( 226),IX( 226),IY( 226)/'DRAW',  -2,   4/
      DATA IOPERA( 227),IX( 227),IY( 227)/'DRAW',  -2,   2/
      DATA IOPERA( 228),IX( 228),IY( 228)/'DRAW',  -3,  -2/
      DATA IOPERA( 229),IX( 229),IY( 229)/'DRAW',  -5,  -9/
      DATA IOPERA( 230),IX( 230),IY( 230)/'MOVE',  -2,  -2/
      DATA IOPERA( 231),IX( 231),IY( 231)/'DRAW',   0,   2/
      DATA IOPERA( 232),IX( 232),IY( 232)/'DRAW',   2,   4/
      DATA IOPERA( 233),IX( 233),IY( 233)/'DRAW',   4,   5/
      DATA IOPERA( 234),IX( 234),IY( 234)/'DRAW',   6,   5/
      DATA IOPERA( 235),IX( 235),IY( 235)/'DRAW',   7,   4/
      DATA IOPERA( 236),IX( 236),IY( 236)/'DRAW',   7,   3/
      DATA IOPERA( 237),IX( 237),IY( 237)/'DRAW',   6,   2/
      DATA IOPERA( 238),IX( 238),IY( 238)/'DRAW',   5,   3/
      DATA IOPERA( 239),IX( 239),IY( 239)/'DRAW',   6,   4/
C
      DATA IXMIND(  18)/  -9/
      DATA IXMAXD(  18)/   8/
      DATA IXDELD(  18)/  17/
      DATA ISTARD(  18)/ 217/
      DATA NUMCOO(  18)/  23/
C
C     DEFINE CHARACTER   2169--LOWER CASE S
C
      DATA IOPERA( 240),IX( 240),IY( 240)/'MOVE',   6,   3/
      DATA IOPERA( 241),IX( 241),IY( 241)/'DRAW',   6,   2/
      DATA IOPERA( 242),IX( 242),IY( 242)/'DRAW',   7,   2/
      DATA IOPERA( 243),IX( 243),IY( 243)/'DRAW',   7,   3/
      DATA IOPERA( 244),IX( 244),IY( 244)/'DRAW',   6,   4/
      DATA IOPERA( 245),IX( 245),IY( 245)/'DRAW',   3,   5/
      DATA IOPERA( 246),IX( 246),IY( 246)/'DRAW',   0,   5/
      DATA IOPERA( 247),IX( 247),IY( 247)/'DRAW',  -3,   4/
      DATA IOPERA( 248),IX( 248),IY( 248)/'DRAW',  -4,   3/
      DATA IOPERA( 249),IX( 249),IY( 249)/'DRAW',  -4,   1/
      DATA IOPERA( 250),IX( 250),IY( 250)/'DRAW',  -3,   0/
      DATA IOPERA( 251),IX( 251),IY( 251)/'DRAW',   4,  -4/
      DATA IOPERA( 252),IX( 252),IY( 252)/'DRAW',   5,  -5/
      DATA IOPERA( 253),IX( 253),IY( 253)/'MOVE',  -4,   2/
      DATA IOPERA( 254),IX( 254),IY( 254)/'DRAW',  -3,   1/
      DATA IOPERA( 255),IX( 255),IY( 255)/'DRAW',   4,  -3/
      DATA IOPERA( 256),IX( 256),IY( 256)/'DRAW',   5,  -4/
      DATA IOPERA( 257),IX( 257),IY( 257)/'DRAW',   5,  -7/
      DATA IOPERA( 258),IX( 258),IY( 258)/'DRAW',   4,  -8/
      DATA IOPERA( 259),IX( 259),IY( 259)/'DRAW',   1,  -9/
      DATA IOPERA( 260),IX( 260),IY( 260)/'DRAW',  -2,  -9/
      DATA IOPERA( 261),IX( 261),IY( 261)/'DRAW',  -5,  -8/
      DATA IOPERA( 262),IX( 262),IY( 262)/'DRAW',  -6,  -7/
      DATA IOPERA( 263),IX( 263),IY( 263)/'DRAW',  -6,  -6/
      DATA IOPERA( 264),IX( 264),IY( 264)/'DRAW',  -5,  -6/
      DATA IOPERA( 265),IX( 265),IY( 265)/'DRAW',  -5,  -7/
C
      DATA IXMIND(  19)/  -8/
      DATA IXMAXD(  19)/   9/
      DATA IXDELD(  19)/  17/
      DATA ISTARD(  19)/ 240/
      DATA NUMCOO(  19)/  26/
C
C     DEFINE CHARACTER   2170--LOWER CASE T
C
      DATA IOPERA( 266),IX( 266),IY( 266)/'MOVE',   2,  12/
      DATA IOPERA( 267),IX( 267),IY( 267)/'DRAW',  -2,  -2/
      DATA IOPERA( 268),IX( 268),IY( 268)/'DRAW',  -3,  -6/
      DATA IOPERA( 269),IX( 269),IY( 269)/'DRAW',  -3,  -8/
      DATA IOPERA( 270),IX( 270),IY( 270)/'DRAW',  -2,  -9/
      DATA IOPERA( 271),IX( 271),IY( 271)/'DRAW',   1,  -9/
      DATA IOPERA( 272),IX( 272),IY( 272)/'DRAW',   3,  -7/
      DATA IOPERA( 273),IX( 273),IY( 273)/'DRAW',   4,  -5/
      DATA IOPERA( 274),IX( 274),IY( 274)/'MOVE',   3,  12/
      DATA IOPERA( 275),IX( 275),IY( 275)/'DRAW',  -1,  -2/
      DATA IOPERA( 276),IX( 276),IY( 276)/'DRAW',  -2,  -6/
      DATA IOPERA( 277),IX( 277),IY( 277)/'DRAW',  -2,  -8/
      DATA IOPERA( 278),IX( 278),IY( 278)/'DRAW',  -1,  -9/
      DATA IOPERA( 279),IX( 279),IY( 279)/'MOVE',  -4,   5/
      DATA IOPERA( 280),IX( 280),IY( 280)/'DRAW',   5,   5/
C
      DATA IXMIND(  20)/  -7/
      DATA IXMAXD(  20)/   7/
      DATA IXDELD(  20)/  14/
      DATA ISTARD(  20)/ 266/
      DATA NUMCOO(  20)/  15/
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCIL2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **************************************
C               **  STEP 2--                        **
C               **  EXTRACT THE COORDINATES         **
C               **  FOR THIS PARTICULAR CHARACTER.  **
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
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DRCIL2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ICHARN
 9013 FORMAT('ICHARN = ',I8)
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
      SUBROUTINE DRCIL3(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN COMPLEX ITALIC LOWER CASE (PART 3).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
C     DEFINE CHARACTER   2171--LOWER CASE U
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE', -11,   1/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW', -10,   3/
      DATA IOPERA(   3),IX(   3),IY(   3)/'DRAW',  -8,   5/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',  -5,   5/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',  -4,   4/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',  -4,   1/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',  -6,  -5/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',  -6,  -7/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',  -4,  -9/
      DATA IOPERA(  10),IX(  10),IY(  10)/'MOVE',  -6,   5/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',  -5,   4/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',  -5,   1/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',  -7,  -5/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',  -7,  -7/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',  -6,  -8/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',  -4,  -9/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',  -2,  -9/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',   0,  -8/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',   2,  -6/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',   4,  -2/
      DATA IOPERA(  21),IX(  21),IY(  21)/'MOVE',   6,   5/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',   4,  -2/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',   3,  -6/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',   3,  -8/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',   4,  -9/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',   7,  -9/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',   9,  -7/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',  10,  -5/
      DATA IOPERA(  29),IX(  29),IY(  29)/'MOVE',   7,   5/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',   5,  -2/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',   4,  -6/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',   4,  -8/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',   5,  -9/
C
      DATA IXMIND(  21)/ -12/
      DATA IXMAXD(  21)/  11/
      DATA IXDELD(  21)/  23/
      DATA ISTARD(  21)/   1/
      DATA NUMCOO(  21)/  33/
C
C     DEFINE CHARACTER   2172--LOWER CASE V
C
      DATA IOPERA(  34),IX(  34),IY(  34)/'MOVE',  -9,   1/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',  -8,   3/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',  -6,   5/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',  -3,   5/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',  -2,   4/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',  -2,   1/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',  -4,  -5/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',  -4,  -7/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',  -2,  -9/
      DATA IOPERA(  43),IX(  43),IY(  43)/'MOVE',  -4,   5/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',  -3,   4/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',  -3,   1/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW',  -5,  -5/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',  -5,  -7/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',  -4,  -8/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',  -2,  -9/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW',  -1,  -9/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW',   2,  -8/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',   4,  -6/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',   6,  -3/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',   7,   1/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',   7,   5/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',   6,   5/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',   7,   3/
C
      DATA IXMIND(  22)/ -10/
      DATA IXMAXD(  22)/  10/
      DATA IXDELD(  22)/  20/
      DATA ISTARD(  22)/  34/
      DATA NUMCOO(  22)/  24/
C
C     DEFINE CHARACTER   2173--LOWER CASE W
C
      DATA IOPERA(  58),IX(  58),IY(  58)/'MOVE', -14,   1/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW', -13,   3/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW', -11,   5/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',  -8,   5/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',  -7,   4/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',  -7,   1/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',  -9,  -5/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',  -9,  -7/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW',  -7,  -9/
      DATA IOPERA(  67),IX(  67),IY(  67)/'MOVE',  -9,   5/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',  -8,   4/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',  -8,   1/
      DATA IOPERA(  70),IX(  70),IY(  70)/'DRAW', -10,  -5/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW', -10,  -7/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',  -9,  -8/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',  -7,  -9/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',  -5,  -9/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',  -3,  -8/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',  -1,  -6/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',   0,  -4/
      DATA IOPERA(  78),IX(  78),IY(  78)/'MOVE',   2,   5/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',   0,  -4/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',   0,  -7/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',   1,  -8/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',   3,  -9/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',   5,  -9/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW',   7,  -8/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',   9,  -6/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',  10,  -4/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',  11,   0/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',  11,   5/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',  10,   5/
      DATA IOPERA(  90),IX(  90),IY(  90)/'DRAW',  11,   3/
      DATA IOPERA(  91),IX(  91),IY(  91)/'MOVE',   3,   5/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW',   1,  -4/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',   1,  -7/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',   3,  -9/
C
      DATA IXMIND(  23)/ -15/
      DATA IXMAXD(  23)/  14/
      DATA IXDELD(  23)/  29/
      DATA ISTARD(  23)/  58/
      DATA NUMCOO(  23)/  37/
C
C     DEFINE CHARACTER   2174--LOWER CASE X
C
      DATA IOPERA(  95),IX(  95),IY(  95)/'MOVE',  -7,   1/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',  -5,   4/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',  -3,   5/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',   0,   5/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',   1,   3/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',   1,   0/
      DATA IOPERA( 101),IX( 101),IY( 101)/'MOVE',  -1,   5/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',   0,   3/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',   0,   0/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',  -1,  -4/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',  -2,  -6/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',  -4,  -8/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',  -6,  -9/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',  -7,  -9/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',  -8,  -8/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',  -8,  -7/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',  -7,  -6/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',  -6,  -7/
      DATA IOPERA( 113),IX( 113),IY( 113)/'DRAW',  -7,  -8/
      DATA IOPERA( 114),IX( 114),IY( 114)/'MOVE',  -1,  -4/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',  -1,  -7/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',   0,  -9/
      DATA IOPERA( 117),IX( 117),IY( 117)/'DRAW',   3,  -9/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',   5,  -8/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',   7,  -5/
      DATA IOPERA( 120),IX( 120),IY( 120)/'MOVE',   7,   4/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW',   6,   3/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW',   7,   2/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',   8,   3/
      DATA IOPERA( 124),IX( 124),IY( 124)/'DRAW',   8,   4/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',   7,   5/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW',   6,   5/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW',   4,   4/
      DATA IOPERA( 128),IX( 128),IY( 128)/'DRAW',   2,   2/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',   1,   0/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',   0,  -4/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',   0,  -7/
      DATA IOPERA( 132),IX( 132),IY( 132)/'DRAW',   1,  -9/
C
      DATA IXMIND(  24)/ -10/
      DATA IXMAXD(  24)/  10/
      DATA IXDELD(  24)/  20/
      DATA ISTARD(  24)/  95/
      DATA NUMCOO(  24)/  38/
C
C     DEFINE CHARACTER   2175--LOWER CASE Y
C
      DATA IOPERA( 133),IX( 133),IY( 133)/'MOVE', -10,   1/
      DATA IOPERA( 134),IX( 134),IY( 134)/'DRAW',  -9,   3/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',  -7,   5/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',  -4,   5/
      DATA IOPERA( 137),IX( 137),IY( 137)/'DRAW',  -3,   4/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW',  -3,   1/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW',  -5,  -5/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW',  -5,  -7/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',  -3,  -9/
      DATA IOPERA( 142),IX( 142),IY( 142)/'MOVE',  -5,   5/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',  -4,   4/
      DATA IOPERA( 144),IX( 144),IY( 144)/'DRAW',  -4,   1/
      DATA IOPERA( 145),IX( 145),IY( 145)/'DRAW',  -6,  -5/
      DATA IOPERA( 146),IX( 146),IY( 146)/'DRAW',  -6,  -7/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',  -5,  -8/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',  -3,  -9/
      DATA IOPERA( 149),IX( 149),IY( 149)/'DRAW',  -1,  -9/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',   1,  -8/
      DATA IOPERA( 151),IX( 151),IY( 151)/'DRAW',   3,  -6/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',   5,  -2/
      DATA IOPERA( 153),IX( 153),IY( 153)/'MOVE',   8,   5/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',   4,  -9/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW',   3, -12/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',   1, -15/
      DATA IOPERA( 157),IX( 157),IY( 157)/'DRAW',  -2, -16/
      DATA IOPERA( 158),IX( 158),IY( 158)/'DRAW',  -5, -16/
      DATA IOPERA( 159),IX( 159),IY( 159)/'DRAW',  -7, -15/
      DATA IOPERA( 160),IX( 160),IY( 160)/'DRAW',  -8, -14/
      DATA IOPERA( 161),IX( 161),IY( 161)/'DRAW',  -8, -13/
      DATA IOPERA( 162),IX( 162),IY( 162)/'DRAW',  -7, -12/
      DATA IOPERA( 163),IX( 163),IY( 163)/'DRAW',  -6, -13/
      DATA IOPERA( 164),IX( 164),IY( 164)/'DRAW',  -7, -14/
      DATA IOPERA( 165),IX( 165),IY( 165)/'MOVE',   7,   5/
      DATA IOPERA( 166),IX( 166),IY( 166)/'DRAW',   3,  -9/
      DATA IOPERA( 167),IX( 167),IY( 167)/'DRAW',   2, -12/
      DATA IOPERA( 168),IX( 168),IY( 168)/'DRAW',   0, -15/
      DATA IOPERA( 169),IX( 169),IY( 169)/'DRAW',  -2, -16/
C
      DATA IXMIND(  25)/ -11/
      DATA IXMAXD(  25)/  10/
      DATA IXDELD(  25)/  21/
      DATA ISTARD(  25)/ 133/
      DATA NUMCOO(  25)/  37/
C
C     DEFINE CHARACTER   2176--LOWER CASE Z
C
      DATA IOPERA( 170),IX( 170),IY( 170)/'MOVE',   7,   5/
      DATA IOPERA( 171),IX( 171),IY( 171)/'DRAW',   6,   3/
      DATA IOPERA( 172),IX( 172),IY( 172)/'DRAW',   4,   1/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW',  -4,  -5/
      DATA IOPERA( 174),IX( 174),IY( 174)/'DRAW',  -6,  -7/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW',  -7,  -9/
      DATA IOPERA( 176),IX( 176),IY( 176)/'MOVE',  -6,   1/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW',  -5,   3/
      DATA IOPERA( 178),IX( 178),IY( 178)/'DRAW',  -3,   5/
      DATA IOPERA( 179),IX( 179),IY( 179)/'DRAW',   0,   5/
      DATA IOPERA( 180),IX( 180),IY( 180)/'DRAW',   4,   3/
      DATA IOPERA( 181),IX( 181),IY( 181)/'MOVE',  -5,   3/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',  -3,   4/
      DATA IOPERA( 183),IX( 183),IY( 183)/'DRAW',   0,   4/
      DATA IOPERA( 184),IX( 184),IY( 184)/'DRAW',   4,   3/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',   6,   3/
      DATA IOPERA( 186),IX( 186),IY( 186)/'MOVE',  -6,  -7/
      DATA IOPERA( 187),IX( 187),IY( 187)/'DRAW',  -4,  -7/
      DATA IOPERA( 188),IX( 188),IY( 188)/'DRAW',   0,  -8/
      DATA IOPERA( 189),IX( 189),IY( 189)/'DRAW',   3,  -8/
      DATA IOPERA( 190),IX( 190),IY( 190)/'DRAW',   5,  -7/
      DATA IOPERA( 191),IX( 191),IY( 191)/'MOVE',  -4,  -7/
      DATA IOPERA( 192),IX( 192),IY( 192)/'DRAW',   0,  -9/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW',   3,  -9/
      DATA IOPERA( 194),IX( 194),IY( 194)/'DRAW',   5,  -7/
      DATA IOPERA( 195),IX( 195),IY( 195)/'DRAW',   6,  -5/
C
      DATA IXMIND(  26)/ -10/
      DATA IXMAXD(  26)/  10/
      DATA IXDELD(  26)/  20/
      DATA ISTARD(  26)/ 170/
      DATA NUMCOO(  26)/  26/
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCIL3--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **************************************
C               **  STEP 2--                        **
C               **  EXTRACT THE COORDINATES         **
C               **  FOR THIS PARTICULAR CHARACTER.  **
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
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DRCIL3--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ICHARN
 9013 FORMAT('ICHARN = ',I8)
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
      SUBROUTINE DRCIN1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN COMPLEX ITALIC NUMERIC (PART 1).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
C     DEFINE CHARACTER      0--0
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE', -10,   0/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW', -13,  -1/
      DATA IOPERA(   3),IX(   3),IY(   3)/'DRAW', -15,  -3/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW', -17,  -6/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW', -18,  -9/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW', -19, -13/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW', -19, -16/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW', -18, -19/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW', -17, -20/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW', -15, -21/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW', -13, -21/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW', -10, -20/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',  -8, -18/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',  -6, -15/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',  -5, -12/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',  -4,  -8/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',  -4,  -5/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',  -5,  -2/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',  -6,  -1/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',  -8,   0/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW', -10,   0/
      DATA IOPERA(  22),IX(  22),IY(  22)/'MOVE', -10,   0/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW', -12,  -1/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW', -14,  -3/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW', -16,  -6/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW', -17,  -9/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW', -18, -13/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW', -18, -16/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW', -17, -19/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW', -15, -21/
      DATA IOPERA(  31),IX(  31),IY(  31)/'MOVE', -13, -21/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW', -11, -20/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',  -9, -18/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',  -7, -15/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',  -6, -12/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',  -5,  -8/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',  -5,  -5/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',  -6,  -2/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',  -8,   0/
C
      DATA IXMIND(   1)/ -10/
      DATA IXMAXD(   1)/  11/
      DATA IXDELD(   1)/  21/
      DATA ISTARD(   1)/   1/
      DATA NUMCOO(   1)/  39/
C
C     DEFINE CHARACTER      1--1
C
      DATA IOPERA(  40),IX(  40),IY(  40)/'MOVE', -10,  -4/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW', -15, -21/
      DATA IOPERA(  42),IX(  42),IY(  42)/'MOVE',  -8,   0/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW', -14, -21/
      DATA IOPERA(  44),IX(  44),IY(  44)/'MOVE',  -8,   0/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW', -11,  -3/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW', -14,  -5/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW', -16,  -6/
      DATA IOPERA(  48),IX(  48),IY(  48)/'MOVE',  -9,  -3/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW', -13,  -5/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW', -16,  -6/
C
      DATA IXMIND(   2)/ -10/
      DATA IXMAXD(   2)/  11/
      DATA IXDELD(   2)/  21/
      DATA ISTARD(   2)/  40/
      DATA NUMCOO(   2)/  11/
C
C     DEFINE CHARACTER      2--2
C
      DATA IOPERA(  51),IX(  51),IY(  51)/'MOVE', -15,  -4/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW', -14,  -5/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW', -15,  -6/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW', -16,  -5/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW', -16,  -4/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW', -15,  -2/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW', -14,  -1/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW', -11,   0/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',  -8,   0/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW',  -5,  -1/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',  -4,  -3/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',  -4,  -5/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',  -5,  -7/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',  -7,  -9/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW', -10, -11/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW', -14, -13/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW', -17, -15/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW', -19, -17/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW', -21, -21/
      DATA IOPERA(  70),IX(  70),IY(  70)/'MOVE',  -8,   0/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',  -6,  -1/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',  -5,  -3/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',  -5,  -5/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',  -6,  -7/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',  -8,  -9/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW', -14, -13/
      DATA IOPERA(  77),IX(  77),IY(  77)/'MOVE', -20, -19/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW', -19, -18/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW', -17, -18/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW', -12, -20/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',  -9, -20/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',  -7, -19/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',  -6, -17/
      DATA IOPERA(  84),IX(  84),IY(  84)/'MOVE', -17, -18/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW', -12, -21/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',  -9, -21/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',  -7, -20/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',  -6, -17/
      DATA IOPERA(  89),IX(  89),IY(  89)/'MOVE', -32, -32/
C
      DATA IXMIND(   3)/ -10/
      DATA IXMAXD(   3)/  11/
      DATA IXDELD(   3)/  21/
      DATA ISTARD(   3)/  51/
      DATA NUMCOO(   3)/  39/
C
C     DEFINE CHARACTER      3--3
C
      DATA IOPERA(  90),IX(  90),IY(  90)/'MOVE', -15,  -4/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW', -14,  -5/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW', -15,  -6/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW', -16,  -5/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW', -16,  -4/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW', -15,  -2/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW', -14,  -1/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW', -11,   0/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',  -8,   0/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',  -5,  -1/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',  -4,  -3/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',  -4,  -5/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',  -5,  -7/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',  -8,  -9/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW', -11, -10/
      DATA IOPERA( 105),IX( 105),IY( 105)/'MOVE',  -8,   0/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',  -6,  -1/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',  -5,  -3/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',  -5,  -5/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',  -6,  -7/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',  -8,  -9/
      DATA IOPERA( 111),IX( 111),IY( 111)/'MOVE', -13, -10/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW', -11, -10/
      DATA IOPERA( 113),IX( 113),IY( 113)/'DRAW',  -8, -11/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',  -7, -12/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',  -6, -14/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',  -6, -17/
      DATA IOPERA( 117),IX( 117),IY( 117)/'DRAW',  -7, -19/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',  -8, -20/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW', -11, -21/
      DATA IOPERA( 120),IX( 120),IY( 120)/'DRAW', -15, -21/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW', -18, -20/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW', -19, -19/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW', -20, -17/
      DATA IOPERA( 124),IX( 124),IY( 124)/'DRAW', -20, -16/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW', -19, -15/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW', -18, -16/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW', -19, -17/
      DATA IOPERA( 128),IX( 128),IY( 128)/'MOVE', -11, -10/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',  -9, -11/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',  -8, -12/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',  -7, -14/
      DATA IOPERA( 132),IX( 132),IY( 132)/'DRAW',  -7, -17/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',  -8, -19/
      DATA IOPERA( 134),IX( 134),IY( 134)/'DRAW',  -9, -20/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW', -11, -21/
      DATA IOPERA( 136),IX( 136),IY( 136)/'MOVE', -32, -32/
C
      DATA IXMIND(   4)/ -10/
      DATA IXMAXD(   4)/  11/
      DATA IXDELD(   4)/  21/
      DATA ISTARD(   4)/  90/
      DATA NUMCOO(   4)/  47/
C
C     DEFINE CHARACTER      4--4
C
      DATA IOPERA( 137),IX( 137),IY( 137)/'MOVE',  -6,  -1/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW', -12, -21/
      DATA IOPERA( 139),IX( 139),IY( 139)/'MOVE',  -5,   0/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW', -11, -21/
      DATA IOPERA( 141),IX( 141),IY( 141)/'MOVE',  -5,   0/
      DATA IOPERA( 142),IX( 142),IY( 142)/'DRAW', -20, -15/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',  -4, -15/
C
      DATA IXMIND(   5)/ -10/
      DATA IXMAXD(   5)/  11/
      DATA IXDELD(   5)/  21/
      DATA ISTARD(   5)/ 137/
      DATA NUMCOO(   5)/   7/
C
C     DEFINE CHARACTER      5--5
C
      DATA IOPERA( 144),IX( 144),IY( 144)/'MOVE', -13,   0/
      DATA IOPERA( 145),IX( 145),IY( 145)/'DRAW', -18, -10/
      DATA IOPERA( 146),IX( 146),IY( 146)/'MOVE', -13,   0/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',  -3,   0/
      DATA IOPERA( 148),IX( 148),IY( 148)/'MOVE', -13,  -1/
      DATA IOPERA( 149),IX( 149),IY( 149)/'DRAW',  -8,  -1/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',  -3,   0/
      DATA IOPERA( 151),IX( 151),IY( 151)/'MOVE', -18, -10/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW', -17,  -9/
      DATA IOPERA( 153),IX( 153),IY( 153)/'DRAW', -14,  -8/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW', -11,  -8/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW',  -8,  -9/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',  -7, -10/
      DATA IOPERA( 157),IX( 157),IY( 157)/'DRAW',  -6, -12/
      DATA IOPERA( 158),IX( 158),IY( 158)/'DRAW',  -6, -15/
      DATA IOPERA( 159),IX( 159),IY( 159)/'DRAW',  -7, -18/
      DATA IOPERA( 160),IX( 160),IY( 160)/'DRAW',  -9, -20/
      DATA IOPERA( 161),IX( 161),IY( 161)/'DRAW', -12, -21/
      DATA IOPERA( 162),IX( 162),IY( 162)/'DRAW', -15, -21/
      DATA IOPERA( 163),IX( 163),IY( 163)/'DRAW', -18, -20/
      DATA IOPERA( 164),IX( 164),IY( 164)/'DRAW', -19, -19/
      DATA IOPERA( 165),IX( 165),IY( 165)/'DRAW', -20, -17/
      DATA IOPERA( 166),IX( 166),IY( 166)/'DRAW', -20, -16/
      DATA IOPERA( 167),IX( 167),IY( 167)/'DRAW', -19, -15/
      DATA IOPERA( 168),IX( 168),IY( 168)/'DRAW', -18, -16/
      DATA IOPERA( 169),IX( 169),IY( 169)/'DRAW', -19, -17/
      DATA IOPERA( 170),IX( 170),IY( 170)/'MOVE', -11,  -8/
      DATA IOPERA( 171),IX( 171),IY( 171)/'DRAW',  -9,  -9/
      DATA IOPERA( 172),IX( 172),IY( 172)/'DRAW',  -8, -10/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW',  -7, -12/
      DATA IOPERA( 174),IX( 174),IY( 174)/'DRAW',  -7, -15/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW',  -8, -18/
      DATA IOPERA( 176),IX( 176),IY( 176)/'DRAW', -10, -20/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW', -12, -21/
      DATA IOPERA( 178),IX( 178),IY( 178)/'MOVE', -32, -32/
C
      DATA IXMIND(   6)/ -10/
      DATA IXMAXD(   6)/  11/
      DATA IXDELD(   6)/  21/
      DATA ISTARD(   6)/ 144/
      DATA NUMCOO(   6)/  35/
C
C     DEFINE CHARACTER      6--6
C
      DATA IOPERA( 179),IX( 179),IY( 179)/'MOVE',  -5,  -3/
      DATA IOPERA( 180),IX( 180),IY( 180)/'DRAW',  -6,  -4/
      DATA IOPERA( 181),IX( 181),IY( 181)/'DRAW',  -5,  -5/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',  -4,  -4/
      DATA IOPERA( 183),IX( 183),IY( 183)/'DRAW',  -4,  -3/
      DATA IOPERA( 184),IX( 184),IY( 184)/'DRAW',  -5,  -1/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',  -7,   0/
      DATA IOPERA( 186),IX( 186),IY( 186)/'DRAW', -10,   0/
      DATA IOPERA( 187),IX( 187),IY( 187)/'DRAW', -13,  -1/
      DATA IOPERA( 188),IX( 188),IY( 188)/'DRAW', -15,  -3/
      DATA IOPERA( 189),IX( 189),IY( 189)/'DRAW', -17,  -6/
      DATA IOPERA( 190),IX( 190),IY( 190)/'DRAW', -18,  -9/
      DATA IOPERA( 191),IX( 191),IY( 191)/'DRAW', -19, -13/
      DATA IOPERA( 192),IX( 192),IY( 192)/'DRAW', -19, -17/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW', -18, -19/
      DATA IOPERA( 194),IX( 194),IY( 194)/'DRAW', -17, -20/
      DATA IOPERA( 195),IX( 195),IY( 195)/'DRAW', -15, -21/
      DATA IOPERA( 196),IX( 196),IY( 196)/'DRAW', -12, -21/
      DATA IOPERA( 197),IX( 197),IY( 197)/'DRAW',  -9, -20/
      DATA IOPERA( 198),IX( 198),IY( 198)/'DRAW',  -7, -18/
      DATA IOPERA( 199),IX( 199),IY( 199)/'DRAW',  -6, -16/
      DATA IOPERA( 200),IX( 200),IY( 200)/'DRAW',  -6, -13/
      DATA IOPERA( 201),IX( 201),IY( 201)/'DRAW',  -7, -11/
      DATA IOPERA( 202),IX( 202),IY( 202)/'DRAW',  -8, -10/
      DATA IOPERA( 203),IX( 203),IY( 203)/'DRAW', -10,  -9/
      DATA IOPERA( 204),IX( 204),IY( 204)/'DRAW', -13,  -9/
      DATA IOPERA( 205),IX( 205),IY( 205)/'DRAW', -15, -10/
      DATA IOPERA( 206),IX( 206),IY( 206)/'DRAW', -17, -12/
      DATA IOPERA( 207),IX( 207),IY( 207)/'DRAW', -18, -14/
      DATA IOPERA( 208),IX( 208),IY( 208)/'MOVE', -10,   0/
      DATA IOPERA( 209),IX( 209),IY( 209)/'DRAW', -12,  -1/
      DATA IOPERA( 210),IX( 210),IY( 210)/'DRAW', -14,  -3/
      DATA IOPERA( 211),IX( 211),IY( 211)/'DRAW', -16,  -6/
      DATA IOPERA( 212),IX( 212),IY( 212)/'DRAW', -17,  -9/
      DATA IOPERA( 213),IX( 213),IY( 213)/'DRAW', -18, -13/
      DATA IOPERA( 214),IX( 214),IY( 214)/'DRAW', -18, -18/
      DATA IOPERA( 215),IX( 215),IY( 215)/'DRAW', -17, -20/
      DATA IOPERA( 216),IX( 216),IY( 216)/'MOVE', -12, -21/
      DATA IOPERA( 217),IX( 217),IY( 217)/'DRAW', -10, -20/
      DATA IOPERA( 218),IX( 218),IY( 218)/'DRAW',  -8, -18/
      DATA IOPERA( 219),IX( 219),IY( 219)/'DRAW',  -7, -16/
      DATA IOPERA( 220),IX( 220),IY( 220)/'DRAW',  -7, -12/
      DATA IOPERA( 221),IX( 221),IY( 221)/'DRAW',  -8, -10/
C
      DATA IXMIND(   7)/ -10/
      DATA IXMAXD(   7)/  11/
      DATA IXDELD(   7)/  21/
      DATA ISTARD(   7)/ 179/
      DATA NUMCOO(   7)/  43/
C
C     DEFINE CHARACTER      7--7
C
      DATA IOPERA( 222),IX( 222),IY( 222)/'MOVE', -16,   0/
      DATA IOPERA( 223),IX( 223),IY( 223)/'DRAW', -18,  -6/
      DATA IOPERA( 224),IX( 224),IY( 224)/'MOVE',  -3,   0/
      DATA IOPERA( 225),IX( 225),IY( 225)/'DRAW',  -4,  -3/
      DATA IOPERA( 226),IX( 226),IY( 226)/'DRAW',  -6,  -6/
      DATA IOPERA( 227),IX( 227),IY( 227)/'DRAW', -11, -12/
      DATA IOPERA( 228),IX( 228),IY( 228)/'DRAW', -13, -15/
      DATA IOPERA( 229),IX( 229),IY( 229)/'DRAW', -14, -17/
      DATA IOPERA( 230),IX( 230),IY( 230)/'DRAW', -15, -21/
      DATA IOPERA( 231),IX( 231),IY( 231)/'MOVE',  -6,  -6/
      DATA IOPERA( 232),IX( 232),IY( 232)/'DRAW', -12, -12/
      DATA IOPERA( 233),IX( 233),IY( 233)/'DRAW', -14, -15/
      DATA IOPERA( 234),IX( 234),IY( 234)/'DRAW', -15, -17/
      DATA IOPERA( 235),IX( 235),IY( 235)/'DRAW', -16, -21/
      DATA IOPERA( 236),IX( 236),IY( 236)/'MOVE', -17,  -3/
      DATA IOPERA( 237),IX( 237),IY( 237)/'DRAW', -14,   0/
      DATA IOPERA( 238),IX( 238),IY( 238)/'DRAW', -12,   0/
      DATA IOPERA( 239),IX( 239),IY( 239)/'DRAW',  -7,  -3/
      DATA IOPERA( 240),IX( 240),IY( 240)/'MOVE', -16,  -2/
      DATA IOPERA( 241),IX( 241),IY( 241)/'DRAW', -14,  -1/
      DATA IOPERA( 242),IX( 242),IY( 242)/'DRAW', -12,  -1/
      DATA IOPERA( 243),IX( 243),IY( 243)/'DRAW',  -7,  -3/
      DATA IOPERA( 244),IX( 244),IY( 244)/'DRAW',  -5,  -3/
      DATA IOPERA( 245),IX( 245),IY( 245)/'DRAW',  -4,  -2/
      DATA IOPERA( 246),IX( 246),IY( 246)/'DRAW',  -3,   0/
C
      DATA IXMIND(   8)/ -10/
      DATA IXMAXD(   8)/  11/
      DATA IXDELD(   8)/  21/
      DATA ISTARD(   8)/ 222/
      DATA NUMCOO(   8)/  25/
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCIN1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **************************************
C               **  STEP 2--                        **
C               **  EXTRACT THE COORDINATES         **
C               **  FOR THIS PARTICULAR CHARACTER.  **
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
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DRCIN1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ICHARN
 9013 FORMAT('ICHARN = ',I8)
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
      SUBROUTINE DRCIN2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN COMPLEX ITALIC NUMERIC (PART 2).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
C     DEFINE CHARACTER      8--8
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE', -11,   0/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW', -14,  -1/
      DATA IOPERA(   3),IX(   3),IY(   3)/'DRAW', -15,  -2/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW', -16,  -4/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW', -16,  -7/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW', -15,  -9/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW', -13, -10/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW', -10, -10/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',  -6,  -9/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',  -5,  -8/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',  -4,  -6/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',  -4,  -3/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',  -5,  -1/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',  -8,   0/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW', -11,   0/
      DATA IOPERA(  16),IX(  16),IY(  16)/'MOVE', -11,   0/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW', -13,  -1/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW', -14,  -2/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW', -15,  -4/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW', -15,  -7/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW', -14,  -9/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW', -13, -10/
      DATA IOPERA(  23),IX(  23),IY(  23)/'MOVE', -10, -10/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',  -7,  -9/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',  -6,  -8/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',  -5,  -6/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',  -5,  -3/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',  -6,  -1/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',  -8,   0/
      DATA IOPERA(  30),IX(  30),IY(  30)/'MOVE', -13, -10/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW', -17, -11/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW', -19, -13/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW', -20, -15/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW', -20, -18/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW', -19, -20/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW', -16, -21/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW', -12, -21/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',  -8, -20/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',  -7, -19/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',  -6, -17/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',  -6, -14/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',  -7, -12/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',  -8, -11/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW', -10, -10/
      DATA IOPERA(  45),IX(  45),IY(  45)/'MOVE', -13, -10/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW', -16, -11/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW', -18, -13/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW', -19, -15/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW', -19, -18/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW', -18, -20/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW', -16, -21/
      DATA IOPERA(  52),IX(  52),IY(  52)/'MOVE', -12, -21/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',  -9, -20/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',  -8, -19/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',  -7, -17/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',  -7, -13/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',  -8, -11/
C
      DATA IXMIND(   9)/ -10/
      DATA IXMAXD(   9)/  11/
      DATA IXDELD(   9)/  21/
      DATA ISTARD(   9)/   1/
      DATA NUMCOO(   9)/  57/
C
C     DEFINE CHARACTER      9--9
C
      DATA IOPERA(  58),IX(  58),IY(  58)/'MOVE',  -5,  -7/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',  -6,  -9/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW',  -8, -11/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW', -10, -12/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW', -13, -12/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW', -15, -11/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW', -16, -10/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW', -17,  -8/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW', -17,  -5/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW', -16,  -3/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW', -14,  -1/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW', -11,   0/
      DATA IOPERA(  70),IX(  70),IY(  70)/'DRAW',  -8,   0/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',  -6,  -1/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',  -5,  -2/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',  -4,  -4/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',  -4,  -8/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',  -5, -12/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',  -6, -15/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',  -8, -18/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW', -10, -20/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW', -13, -21/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW', -16, -21/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW', -18, -20/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW', -19, -18/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW', -19, -17/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW', -18, -16/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW', -17, -17/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW', -18, -18/
      DATA IOPERA(  87),IX(  87),IY(  87)/'MOVE', -15, -11/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW', -16,  -9/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW', -16,  -5/
      DATA IOPERA(  90),IX(  90),IY(  90)/'DRAW', -15,  -3/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW', -13,  -1/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW', -11,   0/
      DATA IOPERA(  93),IX(  93),IY(  93)/'MOVE',  -6,  -1/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',  -5,  -3/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',  -5,  -8/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',  -6, -12/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',  -7, -15/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',  -9, -18/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW', -11, -20/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW', -13, -21/
C
      DATA IXMIND(  10)/ -10/
      DATA IXMAXD(  10)/  11/
      DATA IXDELD(  10)/  21/
      DATA ISTARD(  10)/  58/
      DATA NUMCOO(  10)/  43/
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCIN2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **************************************
C               **  STEP 2--                        **
C               **  EXTRACT THE COORDINATES         **
C               **  FOR THIS PARTICULAR CHARACTER.  **
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
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DRCIN2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ICHARN
 9013 FORMAT('ICHARN = ',I8)
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
      SUBROUTINE DRCIU1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN COMPLEX ITALIC UPPER CASE (PART 1).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
C     DEFINE CHARACTER   2051--UPPER CASE A
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',   3,  12/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW', -10,  -9/
      DATA IOPERA(   3),IX(   3),IY(   3)/'MOVE',   3,  12/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',   4,  -9/
      DATA IOPERA(   5),IX(   5),IY(   5)/'MOVE',   2,  10/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',   3,  -9/
      DATA IOPERA(   7),IX(   7),IY(   7)/'MOVE',  -6,  -3/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',   3,  -3/
      DATA IOPERA(   9),IX(   9),IY(   9)/'MOVE', -12,  -9/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',  -6,  -9/
      DATA IOPERA(  11),IX(  11),IY(  11)/'MOVE',   0,  -9/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',   6,  -9/
C
      DATA IXMIND(   1)/ -10/
      DATA IXMAXD(   1)/  10/
      DATA IXDELD(   1)/  20/
      DATA ISTARD(   1)/   1/
      DATA NUMCOO(   1)/  12/
C
C     DEFINE CHARACTER   2052--UPPER CASE B
C
      DATA IOPERA(  13),IX(  13),IY(  13)/'MOVE',  -3,  12/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',  -9,  -9/
      DATA IOPERA(  15),IX(  15),IY(  15)/'MOVE',  -2,  12/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',  -8,  -9/
      DATA IOPERA(  17),IX(  17),IY(  17)/'MOVE',  -6,  12/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',   5,  12/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',   8,  11/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',   9,   9/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',   9,   7/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',   8,   4/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',   7,   3/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',   4,   2/
      DATA IOPERA(  25),IX(  25),IY(  25)/'MOVE',   5,  12/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',   7,  11/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',   8,   9/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',   8,   7/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',   7,   4/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',   6,   3/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',   4,   2/
      DATA IOPERA(  32),IX(  32),IY(  32)/'MOVE',  -5,   2/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',   4,   2/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',   6,   1/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',   7,  -1/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',   7,  -3/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',   6,  -6/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',   4,  -8/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',   0,  -9/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW', -12,  -9/
      DATA IOPERA(  41),IX(  41),IY(  41)/'MOVE',   4,   2/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',   5,   1/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',   6,  -1/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',   6,  -3/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',   5,  -6/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW',   3,  -8/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',   0,  -9/
C
      DATA IXMIND(   2)/ -12/
      DATA IXMAXD(   2)/  12/
      DATA IXDELD(   2)/  24/
      DATA ISTARD(   2)/  13/
      DATA NUMCOO(   2)/  35/
C
C     DEFINE CHARACTER   2053--UPPER CASE C
C
      DATA IOPERA(  48),IX(  48),IY(  48)/'MOVE',   8,  10/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',   9,  10/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW',  10,  12/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW',   9,   6/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',   9,   8/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',   8,  10/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',   7,  11/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',   5,  12/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',   2,  12/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',  -1,  11/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',  -3,   9/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',  -5,   6/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW',  -6,   3/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',  -7,  -1/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',  -7,  -4/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',  -6,  -7/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',  -5,  -8/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',  -2,  -9/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW',   1,  -9/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',   3,  -8/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',   5,  -6/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',   6,  -4/
      DATA IOPERA(  70),IX(  70),IY(  70)/'MOVE',   2,  12/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',   0,  11/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',  -2,   9/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',  -4,   6/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',  -5,   3/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',  -6,  -1/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',  -6,  -4/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',  -5,  -7/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',  -4,  -8/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',  -2,  -9/
C
      DATA IXMIND(   3)/ -10/
      DATA IXMAXD(   3)/  11/
      DATA IXDELD(   3)/  21/
      DATA ISTARD(   3)/  48/
      DATA NUMCOO(   3)/  32/
C
C     DEFINE CHARACTER   2054--UPPER CASE D
C
      DATA IOPERA(  80),IX(  80),IY(  80)/'MOVE',  -3,  12/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',  -9,  -9/
      DATA IOPERA(  82),IX(  82),IY(  82)/'MOVE',  -2,  12/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',  -8,  -9/
      DATA IOPERA(  84),IX(  84),IY(  84)/'MOVE',  -6,  12/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',   3,  12/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',   6,  11/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',   7,  10/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',   8,   7/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',   8,   3/
      DATA IOPERA(  90),IX(  90),IY(  90)/'DRAW',   7,  -1/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',   5,  -5/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW',   3,  -7/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',   1,  -8/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',  -3,  -9/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW', -12,  -9/
      DATA IOPERA(  96),IX(  96),IY(  96)/'MOVE',   3,  12/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',   5,  11/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',   6,  10/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',   7,   7/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',   7,   3/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',   6,  -1/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',   4,  -5/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',   2,  -7/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',   0,  -8/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',  -3,  -9/
C
      DATA IXMIND(   4)/ -12/
      DATA IXMAXD(   4)/  11/
      DATA IXDELD(   4)/  23/
      DATA ISTARD(   4)/  80/
      DATA NUMCOO(   4)/  26/
C
C     DEFINE CHARACTER   2055--UPPER CASE E
C
      DATA IOPERA( 106),IX( 106),IY( 106)/'MOVE',  -3,  12/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',  -9,  -9/
      DATA IOPERA( 108),IX( 108),IY( 108)/'MOVE',  -2,  12/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',  -8,  -9/
      DATA IOPERA( 110),IX( 110),IY( 110)/'MOVE',   2,   6/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',   0,  -2/
      DATA IOPERA( 112),IX( 112),IY( 112)/'MOVE',  -6,  12/
      DATA IOPERA( 113),IX( 113),IY( 113)/'DRAW',   9,  12/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',   8,   6/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',   8,  12/
      DATA IOPERA( 116),IX( 116),IY( 116)/'MOVE',  -5,   2/
      DATA IOPERA( 117),IX( 117),IY( 117)/'DRAW',   1,   2/
      DATA IOPERA( 118),IX( 118),IY( 118)/'MOVE', -12,  -9/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',   3,  -9/
      DATA IOPERA( 120),IX( 120),IY( 120)/'DRAW',   5,  -4/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW',   2,  -9/
C
      DATA IXMIND(   5)/ -12/
      DATA IXMAXD(   5)/  11/
      DATA IXDELD(   5)/  23/
      DATA ISTARD(   5)/ 106/
      DATA NUMCOO(   5)/  16/
C
C     DEFINE CHARACTER   2056--UPPER CASE F
C
      DATA IOPERA( 122),IX( 122),IY( 122)/'MOVE',  -3,  12/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',  -9,  -9/
      DATA IOPERA( 124),IX( 124),IY( 124)/'MOVE',  -2,  12/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',  -8,  -9/
      DATA IOPERA( 126),IX( 126),IY( 126)/'MOVE',   2,   6/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW',   0,  -2/
      DATA IOPERA( 128),IX( 128),IY( 128)/'MOVE',  -6,  12/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',   9,  12/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',   8,   6/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',   8,  12/
      DATA IOPERA( 132),IX( 132),IY( 132)/'MOVE',  -5,   2/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',   1,   2/
      DATA IOPERA( 134),IX( 134),IY( 134)/'MOVE', -12,  -9/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',  -5,  -9/
C
      DATA IXMIND(   6)/ -12/
      DATA IXMAXD(   6)/  10/
      DATA IXDELD(   6)/  22/
      DATA ISTARD(   6)/ 122/
      DATA NUMCOO(   6)/  14/
C
C     DEFINE CHARACTER   2057--UPPER CASE G
C
      DATA IOPERA( 136),IX( 136),IY( 136)/'MOVE',   8,  10/
      DATA IOPERA( 137),IX( 137),IY( 137)/'DRAW',   9,  10/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW',  10,  12/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW',   9,   6/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW',   9,   8/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',   8,  10/
      DATA IOPERA( 142),IX( 142),IY( 142)/'DRAW',   7,  11/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',   5,  12/
      DATA IOPERA( 144),IX( 144),IY( 144)/'DRAW',   2,  12/
      DATA IOPERA( 145),IX( 145),IY( 145)/'DRAW',  -1,  11/
      DATA IOPERA( 146),IX( 146),IY( 146)/'DRAW',  -3,   9/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',  -5,   6/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',  -6,   3/
      DATA IOPERA( 149),IX( 149),IY( 149)/'DRAW',  -7,  -1/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',  -7,  -4/
      DATA IOPERA( 151),IX( 151),IY( 151)/'DRAW',  -6,  -7/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',  -5,  -8/
      DATA IOPERA( 153),IX( 153),IY( 153)/'DRAW',  -2,  -9/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',   0,  -9/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW',   3,  -8/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',   5,  -6/
      DATA IOPERA( 157),IX( 157),IY( 157)/'DRAW',   7,  -2/
      DATA IOPERA( 158),IX( 158),IY( 158)/'MOVE',   2,  12/
      DATA IOPERA( 159),IX( 159),IY( 159)/'DRAW',   0,  11/
      DATA IOPERA( 160),IX( 160),IY( 160)/'DRAW',  -2,   9/
      DATA IOPERA( 161),IX( 161),IY( 161)/'DRAW',  -4,   6/
      DATA IOPERA( 162),IX( 162),IY( 162)/'DRAW',  -5,   3/
      DATA IOPERA( 163),IX( 163),IY( 163)/'DRAW',  -6,  -1/
      DATA IOPERA( 164),IX( 164),IY( 164)/'DRAW',  -6,  -4/
      DATA IOPERA( 165),IX( 165),IY( 165)/'DRAW',  -5,  -7/
      DATA IOPERA( 166),IX( 166),IY( 166)/'DRAW',  -4,  -8/
      DATA IOPERA( 167),IX( 167),IY( 167)/'DRAW',  -2,  -9/
      DATA IOPERA( 168),IX( 168),IY( 168)/'MOVE',   0,  -9/
      DATA IOPERA( 169),IX( 169),IY( 169)/'DRAW',   2,  -8/
      DATA IOPERA( 170),IX( 170),IY( 170)/'DRAW',   4,  -6/
      DATA IOPERA( 171),IX( 171),IY( 171)/'DRAW',   6,  -2/
      DATA IOPERA( 172),IX( 172),IY( 172)/'MOVE',   3,  -2/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW',  10,  -2/
C
      DATA IXMIND(   7)/ -10/
      DATA IXMAXD(   7)/  12/
      DATA IXDELD(   7)/  22/
      DATA ISTARD(   7)/ 136/
      DATA NUMCOO(   7)/  38/
C
C     DEFINE CHARACTER   2058--UPPER CASE H
C
      DATA IOPERA( 174),IX( 174),IY( 174)/'MOVE',  -4,  12/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW', -10,  -9/
      DATA IOPERA( 176),IX( 176),IY( 176)/'MOVE',  -3,  12/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW',  -9,  -9/
      DATA IOPERA( 178),IX( 178),IY( 178)/'MOVE',   9,  12/
      DATA IOPERA( 179),IX( 179),IY( 179)/'DRAW',   3,  -9/
      DATA IOPERA( 180),IX( 180),IY( 180)/'MOVE',  10,  12/
      DATA IOPERA( 181),IX( 181),IY( 181)/'DRAW',   4,  -9/
      DATA IOPERA( 182),IX( 182),IY( 182)/'MOVE',  -7,  12/
      DATA IOPERA( 183),IX( 183),IY( 183)/'DRAW',   0,  12/
      DATA IOPERA( 184),IX( 184),IY( 184)/'MOVE',   6,  12/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',  13,  12/
      DATA IOPERA( 186),IX( 186),IY( 186)/'MOVE',  -6,   2/
      DATA IOPERA( 187),IX( 187),IY( 187)/'DRAW',   6,   2/
      DATA IOPERA( 188),IX( 188),IY( 188)/'MOVE', -13,  -9/
      DATA IOPERA( 189),IX( 189),IY( 189)/'DRAW',  -6,  -9/
      DATA IOPERA( 190),IX( 190),IY( 190)/'MOVE',   0,  -9/
      DATA IOPERA( 191),IX( 191),IY( 191)/'DRAW',   7,  -9/
C
      DATA IXMIND(   8)/ -13/
      DATA IXMAXD(   8)/  13/
      DATA IXDELD(   8)/  26/
      DATA ISTARD(   8)/ 174/
      DATA NUMCOO(   8)/  18/
C
C     DEFINE CHARACTER   2059--UPPER CASE I
C
      DATA IOPERA( 192),IX( 192),IY( 192)/'MOVE',   3,  12/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW',  -3,  -9/
      DATA IOPERA( 194),IX( 194),IY( 194)/'MOVE',   4,  12/
      DATA IOPERA( 195),IX( 195),IY( 195)/'DRAW',  -2,  -9/
      DATA IOPERA( 196),IX( 196),IY( 196)/'MOVE',   0,  12/
      DATA IOPERA( 197),IX( 197),IY( 197)/'DRAW',   7,  12/
      DATA IOPERA( 198),IX( 198),IY( 198)/'MOVE',  -6,  -9/
      DATA IOPERA( 199),IX( 199),IY( 199)/'DRAW',   1,  -9/
C
      DATA IXMIND(   9)/  -6/
      DATA IXMAXD(   9)/   7/
      DATA IXDELD(   9)/  13/
      DATA ISTARD(   9)/ 192/
      DATA NUMCOO(   9)/   8/
C
C     DEFINE CHARACTER   2060--UPPER CASE J
C
      DATA IOPERA( 200),IX( 200),IY( 200)/'MOVE',   6,  12/
      DATA IOPERA( 201),IX( 201),IY( 201)/'DRAW',   1,  -5/
      DATA IOPERA( 202),IX( 202),IY( 202)/'DRAW',   0,  -7/
      DATA IOPERA( 203),IX( 203),IY( 203)/'DRAW',  -1,  -8/
      DATA IOPERA( 204),IX( 204),IY( 204)/'DRAW',  -3,  -9/
      DATA IOPERA( 205),IX( 205),IY( 205)/'DRAW',  -5,  -9/
      DATA IOPERA( 206),IX( 206),IY( 206)/'DRAW',  -7,  -8/
      DATA IOPERA( 207),IX( 207),IY( 207)/'DRAW',  -8,  -6/
      DATA IOPERA( 208),IX( 208),IY( 208)/'DRAW',  -8,  -4/
      DATA IOPERA( 209),IX( 209),IY( 209)/'DRAW',  -7,  -3/
      DATA IOPERA( 210),IX( 210),IY( 210)/'DRAW',  -6,  -4/
      DATA IOPERA( 211),IX( 211),IY( 211)/'DRAW',  -7,  -5/
      DATA IOPERA( 212),IX( 212),IY( 212)/'MOVE',   5,  12/
      DATA IOPERA( 213),IX( 213),IY( 213)/'DRAW',   0,  -5/
      DATA IOPERA( 214),IX( 214),IY( 214)/'DRAW',  -1,  -7/
      DATA IOPERA( 215),IX( 215),IY( 215)/'DRAW',  -3,  -9/
      DATA IOPERA( 216),IX( 216),IY( 216)/'MOVE',   2,  12/
      DATA IOPERA( 217),IX( 217),IY( 217)/'DRAW',   9,  12/
C
      DATA IXMIND(  10)/  -9/
      DATA IXMAXD(  10)/   9/
      DATA IXDELD(  10)/  18/
      DATA ISTARD(  10)/ 200/
      DATA NUMCOO(  10)/  18/
C
C     DEFINE CHARACTER   2061--UPPER CASE K
C
      DATA IOPERA( 218),IX( 218),IY( 218)/'MOVE',  -3,  12/
      DATA IOPERA( 219),IX( 219),IY( 219)/'DRAW',  -9,  -9/
      DATA IOPERA( 220),IX( 220),IY( 220)/'MOVE',  -2,  12/
      DATA IOPERA( 221),IX( 221),IY( 221)/'DRAW',  -8,  -9/
      DATA IOPERA( 222),IX( 222),IY( 222)/'MOVE',  11,  12/
      DATA IOPERA( 223),IX( 223),IY( 223)/'DRAW',  -6,  -1/
      DATA IOPERA( 224),IX( 224),IY( 224)/'MOVE',   1,   3/
      DATA IOPERA( 225),IX( 225),IY( 225)/'DRAW',   5,  -9/
      DATA IOPERA( 226),IX( 226),IY( 226)/'MOVE',   0,   3/
      DATA IOPERA( 227),IX( 227),IY( 227)/'DRAW',   4,  -9/
      DATA IOPERA( 228),IX( 228),IY( 228)/'MOVE',  -6,  12/
      DATA IOPERA( 229),IX( 229),IY( 229)/'DRAW',   1,  12/
      DATA IOPERA( 230),IX( 230),IY( 230)/'MOVE',   7,  12/
      DATA IOPERA( 231),IX( 231),IY( 231)/'DRAW',  13,  12/
      DATA IOPERA( 232),IX( 232),IY( 232)/'MOVE', -12,  -9/
      DATA IOPERA( 233),IX( 233),IY( 233)/'DRAW',  -5,  -9/
      DATA IOPERA( 234),IX( 234),IY( 234)/'MOVE',   1,  -9/
      DATA IOPERA( 235),IX( 235),IY( 235)/'DRAW',   7,  -9/
C
      DATA IXMIND(  11)/ -12/
      DATA IXMAXD(  11)/  11/
      DATA IXDELD(  11)/  23/
      DATA ISTARD(  11)/ 218/
      DATA NUMCOO(  11)/  18/
C
C     DEFINE CHARACTER   2062--UPPER CASE L
C
      DATA IOPERA( 236),IX( 236),IY( 236)/'MOVE',  -1,  12/
      DATA IOPERA( 237),IX( 237),IY( 237)/'DRAW',  -7,  -9/
      DATA IOPERA( 238),IX( 238),IY( 238)/'MOVE',   0,  12/
      DATA IOPERA( 239),IX( 239),IY( 239)/'DRAW',  -6,  -9/
      DATA IOPERA( 240),IX( 240),IY( 240)/'MOVE',  -4,  12/
      DATA IOPERA( 241),IX( 241),IY( 241)/'DRAW',   3,  12/
      DATA IOPERA( 242),IX( 242),IY( 242)/'MOVE', -10,  -9/
      DATA IOPERA( 243),IX( 243),IY( 243)/'DRAW',   5,  -9/
      DATA IOPERA( 244),IX( 244),IY( 244)/'DRAW',   7,  -3/
      DATA IOPERA( 245),IX( 245),IY( 245)/'DRAW',   4,  -9/
C
      DATA IXMIND(  12)/ -10/
      DATA IXMAXD(  12)/  10/
      DATA IXDELD(  12)/  20/
      DATA ISTARD(  12)/ 236/
      DATA NUMCOO(  12)/  10/
C
C     DEFINE CHARACTER   2063--UPPER CASE M
C
      DATA IOPERA( 246),IX( 246),IY( 246)/'MOVE',  -4,  12/
      DATA IOPERA( 247),IX( 247),IY( 247)/'DRAW', -10,  -9/
      DATA IOPERA( 248),IX( 248),IY( 248)/'MOVE',  -4,  12/
      DATA IOPERA( 249),IX( 249),IY( 249)/'DRAW',  -3,  -9/
      DATA IOPERA( 250),IX( 250),IY( 250)/'MOVE',  -3,  12/
      DATA IOPERA( 251),IX( 251),IY( 251)/'DRAW',  -2,  -7/
      DATA IOPERA( 252),IX( 252),IY( 252)/'MOVE',  10,  12/
      DATA IOPERA( 253),IX( 253),IY( 253)/'DRAW',  -3,  -9/
      DATA IOPERA( 254),IX( 254),IY( 254)/'MOVE',  10,  12/
      DATA IOPERA( 255),IX( 255),IY( 255)/'DRAW',   4,  -9/
      DATA IOPERA( 256),IX( 256),IY( 256)/'MOVE',  11,  12/
      DATA IOPERA( 257),IX( 257),IY( 257)/'DRAW',   5,  -9/
      DATA IOPERA( 258),IX( 258),IY( 258)/'MOVE',  -7,  12/
      DATA IOPERA( 259),IX( 259),IY( 259)/'DRAW',  -3,  12/
      DATA IOPERA( 260),IX( 260),IY( 260)/'MOVE',  10,  12/
      DATA IOPERA( 261),IX( 261),IY( 261)/'DRAW',  14,  12/
      DATA IOPERA( 262),IX( 262),IY( 262)/'MOVE', -13,  -9/
      DATA IOPERA( 263),IX( 263),IY( 263)/'DRAW',  -7,  -9/
      DATA IOPERA( 264),IX( 264),IY( 264)/'MOVE',   1,  -9/
      DATA IOPERA( 265),IX( 265),IY( 265)/'DRAW',   8,  -9/
C
      DATA IXMIND(  13)/ -13/
      DATA IXMAXD(  13)/  14/
      DATA IXDELD(  13)/  27/
      DATA ISTARD(  13)/ 246/
      DATA NUMCOO(  13)/  20/
C
C     DEFINE CHARACTER   2064--UPPER CASE N
C
      DATA IOPERA( 266),IX( 266),IY( 266)/'MOVE',  -3,  12/
      DATA IOPERA( 267),IX( 267),IY( 267)/'DRAW',  -9,  -9/
      DATA IOPERA( 268),IX( 268),IY( 268)/'MOVE',  -3,  12/
      DATA IOPERA( 269),IX( 269),IY( 269)/'DRAW',   4,  -6/
      DATA IOPERA( 270),IX( 270),IY( 270)/'MOVE',  -3,   9/
      DATA IOPERA( 271),IX( 271),IY( 271)/'DRAW',   4,  -9/
      DATA IOPERA( 272),IX( 272),IY( 272)/'MOVE',  10,  12/
      DATA IOPERA( 273),IX( 273),IY( 273)/'DRAW',   4,  -9/
      DATA IOPERA( 274),IX( 274),IY( 274)/'MOVE',  -6,  12/
      DATA IOPERA( 275),IX( 275),IY( 275)/'DRAW',  -3,  12/
      DATA IOPERA( 276),IX( 276),IY( 276)/'MOVE',   7,  12/
      DATA IOPERA( 277),IX( 277),IY( 277)/'DRAW',  13,  12/
      DATA IOPERA( 278),IX( 278),IY( 278)/'MOVE', -12,  -9/
      DATA IOPERA( 279),IX( 279),IY( 279)/'DRAW',  -6,  -9/
C
      DATA IXMIND(  14)/ -12/
      DATA IXMAXD(  14)/  13/
      DATA IXDELD(  14)/  25/
      DATA ISTARD(  14)/ 266/
      DATA NUMCOO(  14)/  14/
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCIU1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **************************************
C               **  STEP 2--                        **
C               **  EXTRACT THE COORDINATES         **
C               **  FOR THIS PARTICULAR CHARACTER.  **
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
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DRCIU1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ICHARN
 9013 FORMAT('ICHARN = ',I8)
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
      SUBROUTINE DRCIU2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN COMPLEX ITALIC UPPER CASE (PART 2).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
C     DEFINE CHARACTER   2065--UPPER CASE O
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',   1,  12/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',  -2,  11/
      DATA IOPERA(   3),IX(   3),IY(   3)/'DRAW',  -4,   9/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',  -6,   6/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',  -7,   3/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',  -8,  -1/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',  -8,  -4/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',  -7,  -7/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',  -6,  -8/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',  -4,  -9/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',  -1,  -9/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',   2,  -8/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',   4,  -6/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',   6,  -3/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',   7,   0/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',   8,   4/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',   8,   7/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',   7,  10/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',   6,  11/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',   4,  12/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',   1,  12/
      DATA IOPERA(  22),IX(  22),IY(  22)/'MOVE',   1,  12/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',  -1,  11/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',  -3,   9/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',  -5,   6/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',  -6,   3/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',  -7,  -1/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',  -7,  -4/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',  -6,  -7/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',  -4,  -9/
      DATA IOPERA(  31),IX(  31),IY(  31)/'MOVE',  -1,  -9/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',   1,  -8/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',   3,  -6/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',   5,  -3/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',   6,   0/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',   7,   4/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',   7,   7/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',   6,  10/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',   4,  12/
C
      DATA IXMIND(  15)/ -11/
      DATA IXMAXD(  15)/  11/
      DATA IXDELD(  15)/  22/
      DATA ISTARD(  15)/   1/
      DATA NUMCOO(  15)/  39/
C
C     DEFINE CHARACTER   2066--UPPER CASE P
C
      DATA IOPERA(  40),IX(  40),IY(  40)/'MOVE',  -3,  12/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',  -9,  -9/
      DATA IOPERA(  42),IX(  42),IY(  42)/'MOVE',  -2,  12/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',  -8,  -9/
      DATA IOPERA(  44),IX(  44),IY(  44)/'MOVE',  -6,  12/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',   6,  12/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW',   9,  11/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',  10,   9/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',  10,   7/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',   9,   4/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW',   7,   2/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW',   3,   1/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',  -5,   1/
      DATA IOPERA(  53),IX(  53),IY(  53)/'MOVE',   6,  12/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',   8,  11/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',   9,   9/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',   9,   7/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',   8,   4/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',   6,   2/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',   3,   1/
      DATA IOPERA(  60),IX(  60),IY(  60)/'MOVE', -12,  -9/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',  -5,  -9/
C
      DATA IXMIND(  16)/ -12/
      DATA IXMAXD(  16)/  11/
      DATA IXDELD(  16)/  23/
      DATA ISTARD(  16)/  40/
      DATA NUMCOO(  16)/  22/
C
C     DEFINE CHARACTER   2067--UPPER CASE Q
C
      DATA IOPERA(  62),IX(  62),IY(  62)/'MOVE',   1,  12/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',  -2,  11/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',  -4,   9/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',  -6,   6/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW',  -7,   3/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',  -8,  -1/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',  -8,  -4/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',  -7,  -7/
      DATA IOPERA(  70),IX(  70),IY(  70)/'DRAW',  -6,  -8/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',  -4,  -9/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',  -1,  -9/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',   2,  -8/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',   4,  -6/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',   6,  -3/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',   7,   0/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',   8,   4/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',   8,   7/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',   7,  10/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',   6,  11/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',   4,  12/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',   1,  12/
      DATA IOPERA(  83),IX(  83),IY(  83)/'MOVE',   1,  12/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW',  -1,  11/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',  -3,   9/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',  -5,   6/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',  -6,   3/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',  -7,  -1/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',  -7,  -4/
      DATA IOPERA(  90),IX(  90),IY(  90)/'DRAW',  -6,  -7/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',  -4,  -9/
      DATA IOPERA(  92),IX(  92),IY(  92)/'MOVE',  -1,  -9/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',   1,  -8/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',   3,  -6/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',   5,  -3/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',   6,   0/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',   7,   4/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',   7,   7/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',   6,  10/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',   4,  12/
      DATA IOPERA( 101),IX( 101),IY( 101)/'MOVE',  -6,  -7/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',  -6,  -6/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',  -5,  -4/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',  -3,  -3/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',  -2,  -3/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',   0,  -4/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',   1,  -6/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',   1, -13/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',   2, -14/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',   4, -14/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',   5, -12/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',   5, -11/
      DATA IOPERA( 113),IX( 113),IY( 113)/'MOVE',   1,  -6/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',   2, -12/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',   3, -13/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',   4, -13/
      DATA IOPERA( 117),IX( 117),IY( 117)/'DRAW',   5, -12/
C
      DATA IXMIND(  17)/ -11/
      DATA IXMAXD(  17)/  11/
      DATA IXDELD(  17)/  22/
      DATA ISTARD(  17)/  62/
      DATA NUMCOO(  17)/  56/
C
C     DEFINE CHARACTER   2068--UPPER CASE R
C
      DATA IOPERA( 118),IX( 118),IY( 118)/'MOVE',  -3,  12/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',  -9,  -9/
      DATA IOPERA( 120),IX( 120),IY( 120)/'MOVE',  -2,  12/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW',  -8,  -9/
      DATA IOPERA( 122),IX( 122),IY( 122)/'MOVE',  -6,  12/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',   5,  12/
      DATA IOPERA( 124),IX( 124),IY( 124)/'DRAW',   8,  11/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',   9,   9/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW',   9,   7/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW',   8,   4/
      DATA IOPERA( 128),IX( 128),IY( 128)/'DRAW',   7,   3/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',   4,   2/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',  -5,   2/
      DATA IOPERA( 131),IX( 131),IY( 131)/'MOVE',   5,  12/
      DATA IOPERA( 132),IX( 132),IY( 132)/'DRAW',   7,  11/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',   8,   9/
      DATA IOPERA( 134),IX( 134),IY( 134)/'DRAW',   8,   7/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',   7,   4/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',   6,   3/
      DATA IOPERA( 137),IX( 137),IY( 137)/'DRAW',   4,   2/
      DATA IOPERA( 138),IX( 138),IY( 138)/'MOVE',   0,   2/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW',   2,   1/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW',   3,   0/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',   4,  -8/
      DATA IOPERA( 142),IX( 142),IY( 142)/'DRAW',   5,  -9/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',   7,  -9/
      DATA IOPERA( 144),IX( 144),IY( 144)/'DRAW',   8,  -7/
      DATA IOPERA( 145),IX( 145),IY( 145)/'DRAW',   8,  -6/
      DATA IOPERA( 146),IX( 146),IY( 146)/'MOVE',   3,   0/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',   5,  -7/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',   6,  -8/
      DATA IOPERA( 149),IX( 149),IY( 149)/'DRAW',   7,  -8/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',   8,  -7/
      DATA IOPERA( 151),IX( 151),IY( 151)/'MOVE', -12,  -9/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',  -5,  -9/
C
      DATA IXMIND(  18)/ -12/
      DATA IXMAXD(  18)/  12/
      DATA IXDELD(  18)/  24/
      DATA ISTARD(  18)/ 118/
      DATA NUMCOO(  18)/  35/
C
C     DEFINE CHARACTER   2069--UPPER CASE S
C
      DATA IOPERA( 153),IX( 153),IY( 153)/'MOVE',   8,  10/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',   9,  10/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW',  10,  12/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',   9,   6/
      DATA IOPERA( 157),IX( 157),IY( 157)/'DRAW',   9,   8/
      DATA IOPERA( 158),IX( 158),IY( 158)/'DRAW',   8,  10/
      DATA IOPERA( 159),IX( 159),IY( 159)/'DRAW',   7,  11/
      DATA IOPERA( 160),IX( 160),IY( 160)/'DRAW',   4,  12/
      DATA IOPERA( 161),IX( 161),IY( 161)/'DRAW',   0,  12/
      DATA IOPERA( 162),IX( 162),IY( 162)/'DRAW',  -3,  11/
      DATA IOPERA( 163),IX( 163),IY( 163)/'DRAW',  -5,   9/
      DATA IOPERA( 164),IX( 164),IY( 164)/'DRAW',  -5,   7/
      DATA IOPERA( 165),IX( 165),IY( 165)/'DRAW',  -4,   5/
      DATA IOPERA( 166),IX( 166),IY( 166)/'DRAW',  -3,   4/
      DATA IOPERA( 167),IX( 167),IY( 167)/'DRAW',   4,   0/
      DATA IOPERA( 168),IX( 168),IY( 168)/'DRAW',   6,  -2/
      DATA IOPERA( 169),IX( 169),IY( 169)/'MOVE',  -5,   7/
      DATA IOPERA( 170),IX( 170),IY( 170)/'DRAW',  -3,   5/
      DATA IOPERA( 171),IX( 171),IY( 171)/'DRAW',   4,   1/
      DATA IOPERA( 172),IX( 172),IY( 172)/'DRAW',   5,   0/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW',   6,  -2/
      DATA IOPERA( 174),IX( 174),IY( 174)/'DRAW',   6,  -5/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW',   5,  -7/
      DATA IOPERA( 176),IX( 176),IY( 176)/'DRAW',   4,  -8/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW',   1,  -9/
      DATA IOPERA( 178),IX( 178),IY( 178)/'DRAW',  -3,  -9/
      DATA IOPERA( 179),IX( 179),IY( 179)/'DRAW',  -6,  -8/
      DATA IOPERA( 180),IX( 180),IY( 180)/'DRAW',  -7,  -7/
      DATA IOPERA( 181),IX( 181),IY( 181)/'DRAW',  -8,  -5/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',  -8,  -3/
      DATA IOPERA( 183),IX( 183),IY( 183)/'DRAW',  -9,  -9/
      DATA IOPERA( 184),IX( 184),IY( 184)/'DRAW',  -8,  -7/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',  -7,  -7/
C
      DATA IXMIND(  19)/ -11/
      DATA IXMAXD(  19)/  12/
      DATA IXDELD(  19)/  23/
      DATA ISTARD(  19)/ 153/
      DATA NUMCOO(  19)/  33/
C
C     DEFINE CHARACTER   2070--UPPER CASE T
C
      DATA IOPERA( 186),IX( 186),IY( 186)/'MOVE',   3,  12/
      DATA IOPERA( 187),IX( 187),IY( 187)/'DRAW',  -3,  -9/
      DATA IOPERA( 188),IX( 188),IY( 188)/'MOVE',   4,  12/
      DATA IOPERA( 189),IX( 189),IY( 189)/'DRAW',  -2,  -9/
      DATA IOPERA( 190),IX( 190),IY( 190)/'MOVE',  -3,  12/
      DATA IOPERA( 191),IX( 191),IY( 191)/'DRAW',  -6,   6/
      DATA IOPERA( 192),IX( 192),IY( 192)/'DRAW',  -4,  12/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW',  11,  12/
      DATA IOPERA( 194),IX( 194),IY( 194)/'DRAW',  10,   6/
      DATA IOPERA( 195),IX( 195),IY( 195)/'DRAW',  10,  12/
      DATA IOPERA( 196),IX( 196),IY( 196)/'MOVE',  -6,  -9/
      DATA IOPERA( 197),IX( 197),IY( 197)/'DRAW',   1,  -9/
C
      DATA IXMIND(  20)/ -10/
      DATA IXMAXD(  20)/  11/
      DATA IXDELD(  20)/  21/
      DATA ISTARD(  20)/ 186/
      DATA NUMCOO(  20)/  12/
C
C     DEFINE CHARACTER   2071--UPPER CASE U
C
      DATA IOPERA( 198),IX( 198),IY( 198)/'MOVE',  -4,  12/
      DATA IOPERA( 199),IX( 199),IY( 199)/'DRAW',  -7,   1/
      DATA IOPERA( 200),IX( 200),IY( 200)/'DRAW',  -8,  -3/
      DATA IOPERA( 201),IX( 201),IY( 201)/'DRAW',  -8,  -6/
      DATA IOPERA( 202),IX( 202),IY( 202)/'DRAW',  -7,  -8/
      DATA IOPERA( 203),IX( 203),IY( 203)/'DRAW',  -4,  -9/
      DATA IOPERA( 204),IX( 204),IY( 204)/'DRAW',   0,  -9/
      DATA IOPERA( 205),IX( 205),IY( 205)/'DRAW',   3,  -8/
      DATA IOPERA( 206),IX( 206),IY( 206)/'DRAW',   5,  -6/
      DATA IOPERA( 207),IX( 207),IY( 207)/'DRAW',   6,  -3/
      DATA IOPERA( 208),IX( 208),IY( 208)/'DRAW',  10,  12/
      DATA IOPERA( 209),IX( 209),IY( 209)/'MOVE',  -3,  12/
      DATA IOPERA( 210),IX( 210),IY( 210)/'DRAW',  -6,   1/
      DATA IOPERA( 211),IX( 211),IY( 211)/'DRAW',  -7,  -3/
      DATA IOPERA( 212),IX( 212),IY( 212)/'DRAW',  -7,  -6/
      DATA IOPERA( 213),IX( 213),IY( 213)/'DRAW',  -6,  -8/
      DATA IOPERA( 214),IX( 214),IY( 214)/'DRAW',  -4,  -9/
      DATA IOPERA( 215),IX( 215),IY( 215)/'MOVE',  -7,  12/
      DATA IOPERA( 216),IX( 216),IY( 216)/'DRAW',   0,  12/
      DATA IOPERA( 217),IX( 217),IY( 217)/'MOVE',   7,  12/
      DATA IOPERA( 218),IX( 218),IY( 218)/'DRAW',  13,  12/
C
      DATA IXMIND(  21)/ -12/
      DATA IXMAXD(  21)/  13/
      DATA IXDELD(  21)/  25/
      DATA ISTARD(  21)/ 198/
      DATA NUMCOO(  21)/  21/
C
C     DEFINE CHARACTER   2072--UPPER CASE V
C
      DATA IOPERA( 219),IX( 219),IY( 219)/'MOVE',  -4,  12/
      DATA IOPERA( 220),IX( 220),IY( 220)/'DRAW',  -3,  -9/
      DATA IOPERA( 221),IX( 221),IY( 221)/'MOVE',  -3,  12/
      DATA IOPERA( 222),IX( 222),IY( 222)/'DRAW',  -2,  -7/
      DATA IOPERA( 223),IX( 223),IY( 223)/'MOVE',  10,  12/
      DATA IOPERA( 224),IX( 224),IY( 224)/'DRAW',  -3,  -9/
      DATA IOPERA( 225),IX( 225),IY( 225)/'MOVE',  -6,  12/
      DATA IOPERA( 226),IX( 226),IY( 226)/'DRAW',   0,  12/
      DATA IOPERA( 227),IX( 227),IY( 227)/'MOVE',   6,  12/
      DATA IOPERA( 228),IX( 228),IY( 228)/'DRAW',  12,  12/
C
      DATA IXMIND(  22)/ -10/
      DATA IXMAXD(  22)/  10/
      DATA IXDELD(  22)/  20/
      DATA ISTARD(  22)/ 219/
      DATA NUMCOO(  22)/  10/
C
C     DEFINE CHARACTER   2073--UPPER CASE W
C
      DATA IOPERA( 229),IX( 229),IY( 229)/'MOVE',  -5,  12/
      DATA IOPERA( 230),IX( 230),IY( 230)/'DRAW',  -7,  -9/
      DATA IOPERA( 231),IX( 231),IY( 231)/'MOVE',  -4,  12/
      DATA IOPERA( 232),IX( 232),IY( 232)/'DRAW',  -6,  -7/
      DATA IOPERA( 233),IX( 233),IY( 233)/'MOVE',   3,  12/
      DATA IOPERA( 234),IX( 234),IY( 234)/'DRAW',  -7,  -9/
      DATA IOPERA( 235),IX( 235),IY( 235)/'MOVE',   3,  12/
      DATA IOPERA( 236),IX( 236),IY( 236)/'DRAW',   1,  -9/
      DATA IOPERA( 237),IX( 237),IY( 237)/'MOVE',   4,  12/
      DATA IOPERA( 238),IX( 238),IY( 238)/'DRAW',   2,  -7/
      DATA IOPERA( 239),IX( 239),IY( 239)/'MOVE',  11,  12/
      DATA IOPERA( 240),IX( 240),IY( 240)/'DRAW',   1,  -9/
      DATA IOPERA( 241),IX( 241),IY( 241)/'MOVE',  -8,  12/
      DATA IOPERA( 242),IX( 242),IY( 242)/'DRAW',  -1,  12/
      DATA IOPERA( 243),IX( 243),IY( 243)/'MOVE',   8,  12/
      DATA IOPERA( 244),IX( 244),IY( 244)/'DRAW',  14,  12/
C
      DATA IXMIND(  23)/ -13/
      DATA IXMAXD(  23)/  13/
      DATA IXDELD(  23)/  26/
      DATA ISTARD(  23)/ 229/
      DATA NUMCOO(  23)/  16/
C
C     DEFINE CHARACTER   2074--UPPER CASE X
C
      DATA IOPERA( 245),IX( 245),IY( 245)/'MOVE',  -4,  12/
      DATA IOPERA( 246),IX( 246),IY( 246)/'DRAW',   3,  -9/
      DATA IOPERA( 247),IX( 247),IY( 247)/'MOVE',  -3,  12/
      DATA IOPERA( 248),IX( 248),IY( 248)/'DRAW',   4,  -9/
      DATA IOPERA( 249),IX( 249),IY( 249)/'MOVE',  10,  12/
      DATA IOPERA( 250),IX( 250),IY( 250)/'DRAW', -10,  -9/
      DATA IOPERA( 251),IX( 251),IY( 251)/'MOVE',  -6,  12/
      DATA IOPERA( 252),IX( 252),IY( 252)/'DRAW',   0,  12/
      DATA IOPERA( 253),IX( 253),IY( 253)/'MOVE',   6,  12/
      DATA IOPERA( 254),IX( 254),IY( 254)/'DRAW',  12,  12/
      DATA IOPERA( 255),IX( 255),IY( 255)/'MOVE', -12,  -9/
      DATA IOPERA( 256),IX( 256),IY( 256)/'DRAW',  -6,  -9/
      DATA IOPERA( 257),IX( 257),IY( 257)/'MOVE',   0,  -9/
      DATA IOPERA( 258),IX( 258),IY( 258)/'DRAW',   6,  -9/
C
      DATA IXMIND(  24)/ -11/
      DATA IXMAXD(  24)/  11/
      DATA IXDELD(  24)/  22/
      DATA ISTARD(  24)/ 245/
      DATA NUMCOO(  24)/  14/
C
C     DEFINE CHARACTER   2075--UPPER CASE Y
C
      DATA IOPERA( 259),IX( 259),IY( 259)/'MOVE',  -4,  12/
      DATA IOPERA( 260),IX( 260),IY( 260)/'DRAW',   0,   2/
      DATA IOPERA( 261),IX( 261),IY( 261)/'DRAW',  -3,  -9/
      DATA IOPERA( 262),IX( 262),IY( 262)/'MOVE',  -3,  12/
      DATA IOPERA( 263),IX( 263),IY( 263)/'DRAW',   1,   2/
      DATA IOPERA( 264),IX( 264),IY( 264)/'DRAW',  -2,  -9/
      DATA IOPERA( 265),IX( 265),IY( 265)/'MOVE',  11,  12/
      DATA IOPERA( 266),IX( 266),IY( 266)/'DRAW',   1,   2/
      DATA IOPERA( 267),IX( 267),IY( 267)/'MOVE',  -6,  12/
      DATA IOPERA( 268),IX( 268),IY( 268)/'DRAW',   0,  12/
      DATA IOPERA( 269),IX( 269),IY( 269)/'MOVE',   7,  12/
      DATA IOPERA( 270),IX( 270),IY( 270)/'DRAW',  13,  12/
      DATA IOPERA( 271),IX( 271),IY( 271)/'MOVE',  -6,  -9/
      DATA IOPERA( 272),IX( 272),IY( 272)/'DRAW',   1,  -9/
C
      DATA IXMIND(  25)/ -10/
      DATA IXMAXD(  25)/  11/
      DATA IXDELD(  25)/  21/
      DATA ISTARD(  25)/ 259/
      DATA NUMCOO(  25)/  14/
C
C     DEFINE CHARACTER   2076--UPPER CASE Z
C
      DATA IOPERA( 273),IX( 273),IY( 273)/'MOVE',   9,  12/
      DATA IOPERA( 274),IX( 274),IY( 274)/'DRAW', -10,  -9/
      DATA IOPERA( 275),IX( 275),IY( 275)/'MOVE',  10,  12/
      DATA IOPERA( 276),IX( 276),IY( 276)/'DRAW',  -9,  -9/
      DATA IOPERA( 277),IX( 277),IY( 277)/'MOVE',  -3,  12/
      DATA IOPERA( 278),IX( 278),IY( 278)/'DRAW',  -6,   6/
      DATA IOPERA( 279),IX( 279),IY( 279)/'DRAW',  -4,  12/
      DATA IOPERA( 280),IX( 280),IY( 280)/'DRAW',  10,  12/
      DATA IOPERA( 281),IX( 281),IY( 281)/'MOVE', -10,  -9/
      DATA IOPERA( 282),IX( 282),IY( 282)/'DRAW',   4,  -9/
      DATA IOPERA( 283),IX( 283),IY( 283)/'DRAW',   6,  -3/
      DATA IOPERA( 284),IX( 284),IY( 284)/'DRAW',   3,  -9/
C
      DATA IXMIND(  26)/ -11/
      DATA IXMAXD(  26)/  11/
      DATA IXDELD(  26)/  22/
      DATA ISTARD(  26)/ 273/
      DATA NUMCOO(  26)/  12/
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCIU2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **************************************
C               **  STEP 2--                        **
C               **  EXTRACT THE COORDINATES         **
C               **  FOR THIS PARTICULAR CHARACTER.  **
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
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DRCIU2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ICHARN
 9013 FORMAT('ICHARN = ',I8)
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
      SUBROUTINE DRCL1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN COMPLEX LOWER CASE (PART 1).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
C     DEFINE CHARACTER   2101--LOWER CASE A
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',  -4,   3/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',  -4,   2/
      DATA IOPERA(   3),IX(   3),IY(   3)/'DRAW',  -5,   2/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',  -5,   3/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',  -4,   4/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',  -2,   5/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',   2,   5/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',   4,   4/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',   5,   3/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',   6,   1/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',   6,  -6/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',   7,  -8/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',   8,  -9/
      DATA IOPERA(  14),IX(  14),IY(  14)/'MOVE',   5,   3/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',   5,  -6/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',   6,  -8/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',   8,  -9/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',   9,  -9/
      DATA IOPERA(  19),IX(  19),IY(  19)/'MOVE',   5,   1/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',   4,   0/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',  -2,  -1/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',  -5,  -2/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',  -6,  -4/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',  -6,  -6/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',  -5,  -8/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',  -2,  -9/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',   1,  -9/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',   3,  -8/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',   5,  -6/
      DATA IOPERA(  30),IX(  30),IY(  30)/'MOVE',  -2,  -1/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',  -4,  -2/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',  -5,  -4/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',  -5,  -6/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',  -4,  -8/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',  -2,  -9/
C
      DATA IXMIND(   1)/  -9/
      DATA IXMAXD(   1)/  11/
      DATA IXDELD(   1)/  20/
      DATA ISTARD(   1)/   1/
      DATA NUMCOO(   1)/  35/
C
C     DEFINE CHARACTER   2102--LOWER CASE B
C
      DATA IOPERA(  36),IX(  36),IY(  36)/'MOVE',  -6,  12/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',  -6,  -9/
      DATA IOPERA(  38),IX(  38),IY(  38)/'MOVE',  -5,  12/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',  -5,  -9/
      DATA IOPERA(  40),IX(  40),IY(  40)/'MOVE',  -5,   2/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',  -3,   4/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',  -1,   5/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',   1,   5/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',   4,   4/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',   6,   2/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW',   7,  -1/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',   7,  -3/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',   6,  -6/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',   4,  -8/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW',   1,  -9/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW',  -1,  -9/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',  -3,  -8/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',  -5,  -6/
      DATA IOPERA(  54),IX(  54),IY(  54)/'MOVE',   1,   5/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',   3,   4/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',   5,   2/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',   6,  -1/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',   6,  -3/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',   5,  -6/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW',   3,  -8/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',   1,  -9/
      DATA IOPERA(  62),IX(  62),IY(  62)/'MOVE',  -9,  12/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',  -5,  12/
C
      DATA IXMIND(   2)/ -11/
      DATA IXMAXD(   2)/  10/
      DATA IXDELD(   2)/  21/
      DATA ISTARD(   2)/  36/
      DATA NUMCOO(   2)/  28/
C
C     DEFINE CHARACTER   2103--LOWER CASE C
C
      DATA IOPERA(  64),IX(  64),IY(  64)/'MOVE',   5,   2/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',   4,   1/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW',   5,   0/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',   6,   1/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',   6,   2/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',   4,   4/
      DATA IOPERA(  70),IX(  70),IY(  70)/'DRAW',   2,   5/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',  -1,   5/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',  -4,   4/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',  -6,   2/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',  -7,  -1/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',  -7,  -3/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',  -6,  -6/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',  -4,  -8/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',  -1,  -9/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',   1,  -9/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',   4,  -8/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',   6,  -6/
      DATA IOPERA(  82),IX(  82),IY(  82)/'MOVE',  -1,   5/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',  -3,   4/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW',  -5,   2/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',  -6,  -1/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',  -6,  -3/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',  -5,  -6/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',  -3,  -8/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',  -1,  -9/
C
      DATA IXMIND(   3)/ -10/
      DATA IXMAXD(   3)/   9/
      DATA IXDELD(   3)/  19/
      DATA ISTARD(   3)/  64/
      DATA NUMCOO(   3)/  26/
C
C     DEFINE CHARACTER   2104--LOWER CASE D
C
      DATA IOPERA(  90),IX(  90),IY(  90)/'MOVE',   5,  12/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',   5,  -9/
      DATA IOPERA(  92),IX(  92),IY(  92)/'MOVE',   6,  12/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',   6,  -9/
      DATA IOPERA(  94),IX(  94),IY(  94)/'MOVE',   5,   2/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',   3,   4/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',   1,   5/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',  -1,   5/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',  -4,   4/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',  -6,   2/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',  -7,  -1/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',  -7,  -3/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',  -6,  -6/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',  -4,  -8/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',  -1,  -9/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',   1,  -9/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',   3,  -8/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',   5,  -6/
      DATA IOPERA( 108),IX( 108),IY( 108)/'MOVE',  -1,   5/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',  -3,   4/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',  -5,   2/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',  -6,  -1/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',  -6,  -3/
      DATA IOPERA( 113),IX( 113),IY( 113)/'DRAW',  -5,  -6/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',  -3,  -8/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',  -1,  -9/
      DATA IOPERA( 116),IX( 116),IY( 116)/'MOVE',   2,  12/
      DATA IOPERA( 117),IX( 117),IY( 117)/'DRAW',   6,  12/
      DATA IOPERA( 118),IX( 118),IY( 118)/'MOVE',   5,  -9/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',   9,  -9/
C
      DATA IXMIND(   4)/ -10/
      DATA IXMAXD(   4)/  11/
      DATA IXDELD(   4)/  21/
      DATA ISTARD(   4)/  90/
      DATA NUMCOO(   4)/  30/
C
C     DEFINE CHARACTER   2105--LOWER CASE E
C
      DATA IOPERA( 120),IX( 120),IY( 120)/'MOVE',  -6,  -1/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW',   6,  -1/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW',   6,   1/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',   5,   3/
      DATA IOPERA( 124),IX( 124),IY( 124)/'DRAW',   4,   4/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',   2,   5/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW',  -1,   5/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW',  -4,   4/
      DATA IOPERA( 128),IX( 128),IY( 128)/'DRAW',  -6,   2/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',  -7,  -1/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',  -7,  -3/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',  -6,  -6/
      DATA IOPERA( 132),IX( 132),IY( 132)/'DRAW',  -4,  -8/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',  -1,  -9/
      DATA IOPERA( 134),IX( 134),IY( 134)/'DRAW',   1,  -9/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',   4,  -8/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',   6,  -6/
      DATA IOPERA( 137),IX( 137),IY( 137)/'MOVE',   5,  -1/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW',   5,   2/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW',   4,   4/
      DATA IOPERA( 140),IX( 140),IY( 140)/'MOVE',  -1,   5/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',  -3,   4/
      DATA IOPERA( 142),IX( 142),IY( 142)/'DRAW',  -5,   2/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',  -6,  -1/
      DATA IOPERA( 144),IX( 144),IY( 144)/'DRAW',  -6,  -3/
      DATA IOPERA( 145),IX( 145),IY( 145)/'DRAW',  -5,  -6/
      DATA IOPERA( 146),IX( 146),IY( 146)/'DRAW',  -3,  -8/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',  -1,  -9/
C
      DATA IXMIND(   5)/ -10/
      DATA IXMAXD(   5)/   9/
      DATA IXDELD(   5)/  19/
      DATA ISTARD(   5)/ 120/
      DATA NUMCOO(   5)/  28/
C
C     DEFINE CHARACTER   2106--LOWER CASE F
C
      DATA IOPERA( 148),IX( 148),IY( 148)/'MOVE',   3,  11/
      DATA IOPERA( 149),IX( 149),IY( 149)/'DRAW',   2,  10/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',   3,   9/
      DATA IOPERA( 151),IX( 151),IY( 151)/'DRAW',   4,  10/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',   4,  11/
      DATA IOPERA( 153),IX( 153),IY( 153)/'DRAW',   3,  12/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',   1,  12/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW',  -1,  11/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',  -2,   9/
      DATA IOPERA( 157),IX( 157),IY( 157)/'DRAW',  -2,  -9/
      DATA IOPERA( 158),IX( 158),IY( 158)/'MOVE',   1,  12/
      DATA IOPERA( 159),IX( 159),IY( 159)/'DRAW',   0,  11/
      DATA IOPERA( 160),IX( 160),IY( 160)/'DRAW',  -1,   9/
      DATA IOPERA( 161),IX( 161),IY( 161)/'DRAW',  -1,  -9/
      DATA IOPERA( 162),IX( 162),IY( 162)/'MOVE',  -5,   5/
      DATA IOPERA( 163),IX( 163),IY( 163)/'DRAW',   3,   5/
      DATA IOPERA( 164),IX( 164),IY( 164)/'MOVE',  -5,  -9/
      DATA IOPERA( 165),IX( 165),IY( 165)/'DRAW',   2,  -9/
C
      DATA IXMIND(   6)/  -7/
      DATA IXMAXD(   6)/   6/
      DATA IXDELD(   6)/  13/
      DATA ISTARD(   6)/ 148/
      DATA NUMCOO(   6)/  18/
C
C     DEFINE CHARACTER   2107--LOWER CASE G
C
      DATA IOPERA( 166),IX( 166),IY( 166)/'MOVE',  -1,   5/
      DATA IOPERA( 167),IX( 167),IY( 167)/'DRAW',  -3,   4/
      DATA IOPERA( 168),IX( 168),IY( 168)/'DRAW',  -4,   3/
      DATA IOPERA( 169),IX( 169),IY( 169)/'DRAW',  -5,   1/
      DATA IOPERA( 170),IX( 170),IY( 170)/'DRAW',  -5,  -1/
      DATA IOPERA( 171),IX( 171),IY( 171)/'DRAW',  -4,  -3/
      DATA IOPERA( 172),IX( 172),IY( 172)/'DRAW',  -3,  -4/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW',  -1,  -5/
      DATA IOPERA( 174),IX( 174),IY( 174)/'DRAW',   1,  -5/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW',   3,  -4/
      DATA IOPERA( 176),IX( 176),IY( 176)/'DRAW',   4,  -3/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW',   5,  -1/
      DATA IOPERA( 178),IX( 178),IY( 178)/'DRAW',   5,   1/
      DATA IOPERA( 179),IX( 179),IY( 179)/'DRAW',   4,   3/
      DATA IOPERA( 180),IX( 180),IY( 180)/'DRAW',   3,   4/
      DATA IOPERA( 181),IX( 181),IY( 181)/'DRAW',   1,   5/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',  -1,   5/
      DATA IOPERA( 183),IX( 183),IY( 183)/'MOVE',  -3,   4/
      DATA IOPERA( 184),IX( 184),IY( 184)/'DRAW',  -4,   2/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',  -4,  -2/
      DATA IOPERA( 186),IX( 186),IY( 186)/'DRAW',  -3,  -4/
      DATA IOPERA( 187),IX( 187),IY( 187)/'MOVE',   3,  -4/
      DATA IOPERA( 188),IX( 188),IY( 188)/'DRAW',   4,  -2/
      DATA IOPERA( 189),IX( 189),IY( 189)/'DRAW',   4,   2/
      DATA IOPERA( 190),IX( 190),IY( 190)/'DRAW',   3,   4/
      DATA IOPERA( 191),IX( 191),IY( 191)/'MOVE',   4,   3/
      DATA IOPERA( 192),IX( 192),IY( 192)/'DRAW',   5,   4/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW',   7,   5/
      DATA IOPERA( 194),IX( 194),IY( 194)/'DRAW',   7,   4/
      DATA IOPERA( 195),IX( 195),IY( 195)/'DRAW',   5,   4/
      DATA IOPERA( 196),IX( 196),IY( 196)/'MOVE',  -4,  -3/
      DATA IOPERA( 197),IX( 197),IY( 197)/'DRAW',  -5,  -4/
      DATA IOPERA( 198),IX( 198),IY( 198)/'DRAW',  -6,  -6/
      DATA IOPERA( 199),IX( 199),IY( 199)/'DRAW',  -6,  -7/
      DATA IOPERA( 200),IX( 200),IY( 200)/'DRAW',  -5,  -9/
      DATA IOPERA( 201),IX( 201),IY( 201)/'DRAW',  -2, -10/
      DATA IOPERA( 202),IX( 202),IY( 202)/'DRAW',   3, -10/
      DATA IOPERA( 203),IX( 203),IY( 203)/'DRAW',   6, -11/
      DATA IOPERA( 204),IX( 204),IY( 204)/'DRAW',   7, -12/
      DATA IOPERA( 205),IX( 205),IY( 205)/'MOVE',  -6,  -7/
      DATA IOPERA( 206),IX( 206),IY( 206)/'DRAW',  -5,  -8/
      DATA IOPERA( 207),IX( 207),IY( 207)/'DRAW',  -2,  -9/
      DATA IOPERA( 208),IX( 208),IY( 208)/'DRAW',   3,  -9/
      DATA IOPERA( 209),IX( 209),IY( 209)/'DRAW',   6, -10/
      DATA IOPERA( 210),IX( 210),IY( 210)/'DRAW',   7, -12/
      DATA IOPERA( 211),IX( 211),IY( 211)/'DRAW',   7, -13/
      DATA IOPERA( 212),IX( 212),IY( 212)/'DRAW',   6, -15/
      DATA IOPERA( 213),IX( 213),IY( 213)/'DRAW',   3, -16/
      DATA IOPERA( 214),IX( 214),IY( 214)/'DRAW',  -3, -16/
      DATA IOPERA( 215),IX( 215),IY( 215)/'DRAW',  -6, -15/
      DATA IOPERA( 216),IX( 216),IY( 216)/'DRAW',  -7, -13/
      DATA IOPERA( 217),IX( 217),IY( 217)/'DRAW',  -7, -12/
      DATA IOPERA( 218),IX( 218),IY( 218)/'DRAW',  -6, -10/
      DATA IOPERA( 219),IX( 219),IY( 219)/'DRAW',  -3,  -9/
C
      DATA IXMIND(   7)/  -9/
      DATA IXMAXD(   7)/  10/
      DATA IXDELD(   7)/  19/
      DATA ISTARD(   7)/ 166/
      DATA NUMCOO(   7)/  54/
C
C     DEFINE CHARACTER   2108--LOWER CASE H
C
      DATA IOPERA( 220),IX( 220),IY( 220)/'MOVE',  -6,  12/
      DATA IOPERA( 221),IX( 221),IY( 221)/'DRAW',  -6,  -9/
      DATA IOPERA( 222),IX( 222),IY( 222)/'MOVE',  -5,  12/
      DATA IOPERA( 223),IX( 223),IY( 223)/'DRAW',  -5,  -9/
      DATA IOPERA( 224),IX( 224),IY( 224)/'MOVE',  -5,   2/
      DATA IOPERA( 225),IX( 225),IY( 225)/'DRAW',  -3,   4/
      DATA IOPERA( 226),IX( 226),IY( 226)/'DRAW',   0,   5/
      DATA IOPERA( 227),IX( 227),IY( 227)/'DRAW',   2,   5/
      DATA IOPERA( 228),IX( 228),IY( 228)/'DRAW',   5,   4/
      DATA IOPERA( 229),IX( 229),IY( 229)/'DRAW',   6,   2/
      DATA IOPERA( 230),IX( 230),IY( 230)/'DRAW',   6,  -9/
      DATA IOPERA( 231),IX( 231),IY( 231)/'MOVE',   2,   5/
      DATA IOPERA( 232),IX( 232),IY( 232)/'DRAW',   4,   4/
      DATA IOPERA( 233),IX( 233),IY( 233)/'DRAW',   5,   2/
      DATA IOPERA( 234),IX( 234),IY( 234)/'DRAW',   5,  -9/
      DATA IOPERA( 235),IX( 235),IY( 235)/'MOVE',  -9,  12/
      DATA IOPERA( 236),IX( 236),IY( 236)/'DRAW',  -5,  12/
      DATA IOPERA( 237),IX( 237),IY( 237)/'MOVE',  -9,  -9/
      DATA IOPERA( 238),IX( 238),IY( 238)/'DRAW',  -2,  -9/
      DATA IOPERA( 239),IX( 239),IY( 239)/'MOVE',   2,  -9/
      DATA IOPERA( 240),IX( 240),IY( 240)/'DRAW',   9,  -9/
C
      DATA IXMIND(   8)/ -11/
      DATA IXMAXD(   8)/  11/
      DATA IXDELD(   8)/  22/
      DATA ISTARD(   8)/ 220/
      DATA NUMCOO(   8)/  21/
C
C     DEFINE CHARACTER   2109--LOWER CASE I
C
      DATA IOPERA( 241),IX( 241),IY( 241)/'MOVE',   0,  12/
      DATA IOPERA( 242),IX( 242),IY( 242)/'DRAW',  -1,  11/
      DATA IOPERA( 243),IX( 243),IY( 243)/'DRAW',   0,  10/
      DATA IOPERA( 244),IX( 244),IY( 244)/'DRAW',   1,  11/
      DATA IOPERA( 245),IX( 245),IY( 245)/'DRAW',   0,  12/
      DATA IOPERA( 246),IX( 246),IY( 246)/'MOVE',   0,   5/
      DATA IOPERA( 247),IX( 247),IY( 247)/'DRAW',   0,  -9/
      DATA IOPERA( 248),IX( 248),IY( 248)/'MOVE',   1,   5/
      DATA IOPERA( 249),IX( 249),IY( 249)/'DRAW',   1,  -9/
      DATA IOPERA( 250),IX( 250),IY( 250)/'MOVE',  -3,   5/
      DATA IOPERA( 251),IX( 251),IY( 251)/'DRAW',   1,   5/
      DATA IOPERA( 252),IX( 252),IY( 252)/'MOVE',  -3,  -9/
      DATA IOPERA( 253),IX( 253),IY( 253)/'DRAW',   4,  -9/
C
      DATA IXMIND(   9)/  -5/
      DATA IXMAXD(   9)/   6/
      DATA IXDELD(   9)/  11/
      DATA ISTARD(   9)/ 241/
      DATA NUMCOO(   9)/  13/
C
C     DEFINE CHARACTER   2110--LOWER CASE J
C
      DATA IOPERA( 254),IX( 254),IY( 254)/'MOVE',   1,  12/
      DATA IOPERA( 255),IX( 255),IY( 255)/'DRAW',   0,  11/
      DATA IOPERA( 256),IX( 256),IY( 256)/'DRAW',   1,  10/
      DATA IOPERA( 257),IX( 257),IY( 257)/'DRAW',   2,  11/
      DATA IOPERA( 258),IX( 258),IY( 258)/'DRAW',   1,  12/
      DATA IOPERA( 259),IX( 259),IY( 259)/'MOVE',   2,   5/
      DATA IOPERA( 260),IX( 260),IY( 260)/'DRAW',   2, -13/
      DATA IOPERA( 261),IX( 261),IY( 261)/'DRAW',   1, -15/
      DATA IOPERA( 262),IX( 262),IY( 262)/'DRAW',  -1, -16/
      DATA IOPERA( 263),IX( 263),IY( 263)/'DRAW',  -3, -16/
      DATA IOPERA( 264),IX( 264),IY( 264)/'DRAW',  -4, -15/
      DATA IOPERA( 265),IX( 265),IY( 265)/'DRAW',  -4, -14/
      DATA IOPERA( 266),IX( 266),IY( 266)/'DRAW',  -3, -13/
      DATA IOPERA( 267),IX( 267),IY( 267)/'DRAW',  -2, -14/
      DATA IOPERA( 268),IX( 268),IY( 268)/'DRAW',  -3, -15/
      DATA IOPERA( 269),IX( 269),IY( 269)/'MOVE',   1,   5/
      DATA IOPERA( 270),IX( 270),IY( 270)/'DRAW',   1, -13/
      DATA IOPERA( 271),IX( 271),IY( 271)/'DRAW',   0, -15/
      DATA IOPERA( 272),IX( 272),IY( 272)/'DRAW',  -1, -16/
      DATA IOPERA( 273),IX( 273),IY( 273)/'MOVE',  -2,   5/
      DATA IOPERA( 274),IX( 274),IY( 274)/'DRAW',   2,   5/
C
      DATA IXMIND(  10)/  -5/
      DATA IXMAXD(  10)/   6/
      DATA IXDELD(  10)/  11/
      DATA ISTARD(  10)/ 254/
      DATA NUMCOO(  10)/  21/
C
C     DEFINE CHARACTER   2111--LOWER CASE K
C
      DATA IOPERA( 275),IX( 275),IY( 275)/'MOVE',  -6,  12/
      DATA IOPERA( 276),IX( 276),IY( 276)/'DRAW',  -6,  -9/
      DATA IOPERA( 277),IX( 277),IY( 277)/'MOVE',  -5,  12/
      DATA IOPERA( 278),IX( 278),IY( 278)/'DRAW',  -5,  -9/
      DATA IOPERA( 279),IX( 279),IY( 279)/'MOVE',   5,   5/
      DATA IOPERA( 280),IX( 280),IY( 280)/'DRAW',  -5,  -5/
      DATA IOPERA( 281),IX( 281),IY( 281)/'MOVE',   0,  -1/
      DATA IOPERA( 282),IX( 282),IY( 282)/'DRAW',   6,  -9/
      DATA IOPERA( 283),IX( 283),IY( 283)/'MOVE',  -1,  -1/
      DATA IOPERA( 284),IX( 284),IY( 284)/'DRAW',   5,  -9/
      DATA IOPERA( 285),IX( 285),IY( 285)/'MOVE',  -9,  12/
      DATA IOPERA( 286),IX( 286),IY( 286)/'DRAW',  -5,  12/
      DATA IOPERA( 287),IX( 287),IY( 287)/'MOVE',   2,   5/
      DATA IOPERA( 288),IX( 288),IY( 288)/'DRAW',   8,   5/
      DATA IOPERA( 289),IX( 289),IY( 289)/'MOVE',  -9,  -9/
      DATA IOPERA( 290),IX( 290),IY( 290)/'DRAW',  -2,  -9/
      DATA IOPERA( 291),IX( 291),IY( 291)/'MOVE',   2,  -9/
      DATA IOPERA( 292),IX( 292),IY( 292)/'DRAW',   8,  -9/
C
      DATA IXMIND(  11)/ -11/
      DATA IXMAXD(  11)/  10/
      DATA IXDELD(  11)/  21/
      DATA ISTARD(  11)/ 275/
      DATA NUMCOO(  11)/  18/
C
C     DEFINE CHARACTER   2112--LOWER CASE L
C
      DATA IOPERA( 293),IX( 293),IY( 293)/'MOVE',   0,  12/
      DATA IOPERA( 294),IX( 294),IY( 294)/'DRAW',   0,  -9/
      DATA IOPERA( 295),IX( 295),IY( 295)/'MOVE',   1,  12/
      DATA IOPERA( 296),IX( 296),IY( 296)/'DRAW',   1,  -9/
      DATA IOPERA( 297),IX( 297),IY( 297)/'MOVE',  -3,  12/
      DATA IOPERA( 298),IX( 298),IY( 298)/'DRAW',   1,  12/
      DATA IOPERA( 299),IX( 299),IY( 299)/'MOVE',  -3,  -9/
      DATA IOPERA( 300),IX( 300),IY( 300)/'DRAW',   4,  -9/
C
      DATA IXMIND(  12)/  -5/
      DATA IXMAXD(  12)/   6/
      DATA IXDELD(  12)/  11/
      DATA ISTARD(  12)/ 293/
      DATA NUMCOO(  12)/   8/
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCL1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **************************************
C               **  STEP 2--                        **
C               **  EXTRACT THE COORDINATES         **
C               **  FOR THIS PARTICULAR CHARACTER.  **
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
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DRCL1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ICHARN
 9013 FORMAT('ICHARN = ',I8)
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
      SUBROUTINE DRCL2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN COMPLEX LOWER CASE (PART 2).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
C     DEFINE CHARACTER   2113--LOWER CASE M
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE', -11,   5/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW', -11,  -9/
      DATA IOPERA(   3),IX(   3),IY(   3)/'MOVE', -10,   5/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW', -10,  -9/
      DATA IOPERA(   5),IX(   5),IY(   5)/'MOVE', -10,   2/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',  -8,   4/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',  -5,   5/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',  -3,   5/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',   0,   4/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',   1,   2/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',   1,  -9/
      DATA IOPERA(  12),IX(  12),IY(  12)/'MOVE',  -3,   5/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',  -1,   4/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',   0,   2/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',   0,  -9/
      DATA IOPERA(  16),IX(  16),IY(  16)/'MOVE',   1,   2/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',   3,   4/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',   6,   5/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',   8,   5/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',  11,   4/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',  12,   2/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',  12,  -9/
      DATA IOPERA(  23),IX(  23),IY(  23)/'MOVE',   8,   5/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',  10,   4/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',  11,   2/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',  11,  -9/
      DATA IOPERA(  27),IX(  27),IY(  27)/'MOVE', -14,   5/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW', -10,   5/
      DATA IOPERA(  29),IX(  29),IY(  29)/'MOVE', -14,  -9/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',  -7,  -9/
      DATA IOPERA(  31),IX(  31),IY(  31)/'MOVE',  -3,  -9/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',   4,  -9/
      DATA IOPERA(  33),IX(  33),IY(  33)/'MOVE',   8,  -9/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',  15,  -9/
C
      DATA IXMIND(  13)/ -16/
      DATA IXMAXD(  13)/  17/
      DATA IXDELD(  13)/  33/
      DATA ISTARD(  13)/   1/
      DATA NUMCOO(  13)/  34/
C
C     DEFINE CHARACTER   2114--LOWER CASE N
C
      DATA IOPERA(  35),IX(  35),IY(  35)/'MOVE',  -6,   5/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',  -6,  -9/
      DATA IOPERA(  37),IX(  37),IY(  37)/'MOVE',  -5,   5/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',  -5,  -9/
      DATA IOPERA(  39),IX(  39),IY(  39)/'MOVE',  -5,   2/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',  -3,   4/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',   0,   5/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',   2,   5/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',   5,   4/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',   6,   2/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',   6,  -9/
      DATA IOPERA(  46),IX(  46),IY(  46)/'MOVE',   2,   5/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',   4,   4/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',   5,   2/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',   5,  -9/
      DATA IOPERA(  50),IX(  50),IY(  50)/'MOVE',  -9,   5/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW',  -5,   5/
      DATA IOPERA(  52),IX(  52),IY(  52)/'MOVE',  -9,  -9/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',  -2,  -9/
      DATA IOPERA(  54),IX(  54),IY(  54)/'MOVE',   2,  -9/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',   9,  -9/
C
      DATA IXMIND(  14)/ -11/
      DATA IXMAXD(  14)/  11/
      DATA IXDELD(  14)/  22/
      DATA ISTARD(  14)/  35/
      DATA NUMCOO(  14)/  21/
C
C     DEFINE CHARACTER   2115--LOWER CASE O
C
      DATA IOPERA(  56),IX(  56),IY(  56)/'MOVE',  -1,   5/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',  -4,   4/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',  -6,   2/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',  -7,  -1/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW',  -7,  -3/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',  -6,  -6/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',  -4,  -8/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',  -1,  -9/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',   1,  -9/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',   4,  -8/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW',   6,  -6/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',   7,  -3/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',   7,  -1/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',   6,   2/
      DATA IOPERA(  70),IX(  70),IY(  70)/'DRAW',   4,   4/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',   1,   5/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',  -1,   5/
      DATA IOPERA(  73),IX(  73),IY(  73)/'MOVE',  -1,   5/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',  -3,   4/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',  -5,   2/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',  -6,  -1/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',  -6,  -3/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',  -5,  -6/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',  -3,  -8/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',  -1,  -9/
      DATA IOPERA(  81),IX(  81),IY(  81)/'MOVE',   1,  -9/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',   3,  -8/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',   5,  -6/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW',   6,  -3/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',   6,  -1/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',   5,   2/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',   3,   4/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',   1,   5/
C
      DATA IXMIND(  15)/ -10/
      DATA IXMAXD(  15)/  10/
      DATA IXDELD(  15)/  20/
      DATA ISTARD(  15)/  56/
      DATA NUMCOO(  15)/  33/
C
C     DEFINE CHARACTER   2116--LOWER CASE P
C
      DATA IOPERA(  89),IX(  89),IY(  89)/'MOVE',  -6,   5/
      DATA IOPERA(  90),IX(  90),IY(  90)/'DRAW',  -6, -16/
      DATA IOPERA(  91),IX(  91),IY(  91)/'MOVE',  -5,   5/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW',  -5, -16/
      DATA IOPERA(  93),IX(  93),IY(  93)/'MOVE',  -5,   2/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',  -3,   4/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',  -1,   5/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',   1,   5/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',   4,   4/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',   6,   2/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',   7,  -1/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',   7,  -3/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',   6,  -6/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',   4,  -8/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',   1,  -9/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',  -1,  -9/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',  -3,  -8/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',  -5,  -6/
      DATA IOPERA( 107),IX( 107),IY( 107)/'MOVE',   1,   5/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',   3,   4/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',   5,   2/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',   6,  -1/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',   6,  -3/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',   5,  -6/
      DATA IOPERA( 113),IX( 113),IY( 113)/'DRAW',   3,  -8/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',   1,  -9/
      DATA IOPERA( 115),IX( 115),IY( 115)/'MOVE',  -9,   5/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',  -5,   5/
      DATA IOPERA( 117),IX( 117),IY( 117)/'MOVE',  -9, -16/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',  -2, -16/
C
      DATA IXMIND(  16)/ -11/
      DATA IXMAXD(  16)/  10/
      DATA IXDELD(  16)/  21/
      DATA ISTARD(  16)/  89/
      DATA NUMCOO(  16)/  30/
C
C     DEFINE CHARACTER   2117--LOWER CASE Q
C
      DATA IOPERA( 119),IX( 119),IY( 119)/'MOVE',   5,   5/
      DATA IOPERA( 120),IX( 120),IY( 120)/'DRAW',   5, -16/
      DATA IOPERA( 121),IX( 121),IY( 121)/'MOVE',   6,   5/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW',   6, -16/
      DATA IOPERA( 123),IX( 123),IY( 123)/'MOVE',   5,   2/
      DATA IOPERA( 124),IX( 124),IY( 124)/'DRAW',   3,   4/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',   1,   5/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW',  -1,   5/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW',  -4,   4/
      DATA IOPERA( 128),IX( 128),IY( 128)/'DRAW',  -6,   2/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',  -7,  -1/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',  -7,  -3/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',  -6,  -6/
      DATA IOPERA( 132),IX( 132),IY( 132)/'DRAW',  -4,  -8/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',  -1,  -9/
      DATA IOPERA( 134),IX( 134),IY( 134)/'DRAW',   1,  -9/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',   3,  -8/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',   5,  -6/
      DATA IOPERA( 137),IX( 137),IY( 137)/'MOVE',  -1,   5/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW',  -3,   4/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW',  -5,   2/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW',  -6,  -1/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',  -6,  -3/
      DATA IOPERA( 142),IX( 142),IY( 142)/'DRAW',  -5,  -6/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',  -3,  -8/
      DATA IOPERA( 144),IX( 144),IY( 144)/'DRAW',  -1,  -9/
      DATA IOPERA( 145),IX( 145),IY( 145)/'MOVE',   2, -16/
      DATA IOPERA( 146),IX( 146),IY( 146)/'DRAW',   9, -16/
C
      DATA IXMIND(  17)/ -10/
      DATA IXMAXD(  17)/  10/
      DATA IXDELD(  17)/  20/
      DATA ISTARD(  17)/ 119/
      DATA NUMCOO(  17)/  28/
C
C     DEFINE CHARACTER   2118--LOWER CASE R
C
      DATA IOPERA( 147),IX( 147),IY( 147)/'MOVE',  -4,   5/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',  -4,  -9/
      DATA IOPERA( 149),IX( 149),IY( 149)/'MOVE',  -3,   5/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',  -3,  -9/
      DATA IOPERA( 151),IX( 151),IY( 151)/'MOVE',  -3,  -1/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',  -2,   2/
      DATA IOPERA( 153),IX( 153),IY( 153)/'DRAW',   0,   4/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',   2,   5/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW',   5,   5/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',   6,   4/
      DATA IOPERA( 157),IX( 157),IY( 157)/'DRAW',   6,   3/
      DATA IOPERA( 158),IX( 158),IY( 158)/'DRAW',   5,   2/
      DATA IOPERA( 159),IX( 159),IY( 159)/'DRAW',   4,   3/
      DATA IOPERA( 160),IX( 160),IY( 160)/'DRAW',   5,   4/
      DATA IOPERA( 161),IX( 161),IY( 161)/'MOVE',  -7,   5/
      DATA IOPERA( 162),IX( 162),IY( 162)/'DRAW',  -3,   5/
      DATA IOPERA( 163),IX( 163),IY( 163)/'MOVE',  -7,  -9/
      DATA IOPERA( 164),IX( 164),IY( 164)/'DRAW',   0,  -9/
C
      DATA IXMIND(  18)/  -9/
      DATA IXMAXD(  18)/   8/
      DATA IXDELD(  18)/  17/
      DATA ISTARD(  18)/ 147/
      DATA NUMCOO(  18)/  18/
C
C     DEFINE CHARACTER   2119--LOWER CASE S
C
      DATA IOPERA( 165),IX( 165),IY( 165)/'MOVE',   5,   3/
      DATA IOPERA( 166),IX( 166),IY( 166)/'DRAW',   6,   5/
      DATA IOPERA( 167),IX( 167),IY( 167)/'DRAW',   6,   1/
      DATA IOPERA( 168),IX( 168),IY( 168)/'DRAW',   5,   3/
      DATA IOPERA( 169),IX( 169),IY( 169)/'DRAW',   4,   4/
      DATA IOPERA( 170),IX( 170),IY( 170)/'DRAW',   2,   5/
      DATA IOPERA( 171),IX( 171),IY( 171)/'DRAW',  -2,   5/
      DATA IOPERA( 172),IX( 172),IY( 172)/'DRAW',  -4,   4/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW',  -5,   3/
      DATA IOPERA( 174),IX( 174),IY( 174)/'DRAW',  -5,   1/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW',  -4,   0/
      DATA IOPERA( 176),IX( 176),IY( 176)/'DRAW',  -2,  -1/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW',   3,  -3/
      DATA IOPERA( 178),IX( 178),IY( 178)/'DRAW',   5,  -4/
      DATA IOPERA( 179),IX( 179),IY( 179)/'DRAW',   6,  -5/
      DATA IOPERA( 180),IX( 180),IY( 180)/'MOVE',  -5,   2/
      DATA IOPERA( 181),IX( 181),IY( 181)/'DRAW',  -4,   1/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',  -2,   0/
      DATA IOPERA( 183),IX( 183),IY( 183)/'DRAW',   3,  -2/
      DATA IOPERA( 184),IX( 184),IY( 184)/'DRAW',   5,  -3/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',   6,  -4/
      DATA IOPERA( 186),IX( 186),IY( 186)/'DRAW',   6,  -7/
      DATA IOPERA( 187),IX( 187),IY( 187)/'DRAW',   5,  -8/
      DATA IOPERA( 188),IX( 188),IY( 188)/'DRAW',   3,  -9/
      DATA IOPERA( 189),IX( 189),IY( 189)/'DRAW',  -1,  -9/
      DATA IOPERA( 190),IX( 190),IY( 190)/'DRAW',  -3,  -8/
      DATA IOPERA( 191),IX( 191),IY( 191)/'DRAW',  -4,  -7/
      DATA IOPERA( 192),IX( 192),IY( 192)/'DRAW',  -5,  -5/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW',  -5,  -9/
      DATA IOPERA( 194),IX( 194),IY( 194)/'DRAW',  -4,  -7/
C
      DATA IXMIND(  19)/  -8/
      DATA IXMAXD(  19)/   9/
      DATA IXDELD(  19)/  17/
      DATA ISTARD(  19)/ 165/
      DATA NUMCOO(  19)/  30/
C
C     DEFINE CHARACTER   2120--LOWER CASE T
C
      DATA IOPERA( 195),IX( 195),IY( 195)/'MOVE',  -2,  12/
      DATA IOPERA( 196),IX( 196),IY( 196)/'DRAW',  -2,  -5/
      DATA IOPERA( 197),IX( 197),IY( 197)/'DRAW',  -1,  -8/
      DATA IOPERA( 198),IX( 198),IY( 198)/'DRAW',   1,  -9/
      DATA IOPERA( 199),IX( 199),IY( 199)/'DRAW',   3,  -9/
      DATA IOPERA( 200),IX( 200),IY( 200)/'DRAW',   5,  -8/
      DATA IOPERA( 201),IX( 201),IY( 201)/'DRAW',   6,  -6/
      DATA IOPERA( 202),IX( 202),IY( 202)/'MOVE',  -1,  12/
      DATA IOPERA( 203),IX( 203),IY( 203)/'DRAW',  -1,  -5/
      DATA IOPERA( 204),IX( 204),IY( 204)/'DRAW',   0,  -8/
      DATA IOPERA( 205),IX( 205),IY( 205)/'DRAW',   1,  -9/
      DATA IOPERA( 206),IX( 206),IY( 206)/'MOVE',  -5,   5/
      DATA IOPERA( 207),IX( 207),IY( 207)/'DRAW',   3,   5/
C
      DATA IXMIND(  20)/  -7/
      DATA IXMAXD(  20)/   8/
      DATA IXDELD(  20)/  15/
      DATA ISTARD(  20)/ 195/
      DATA NUMCOO(  20)/  13/
C
C     DEFINE CHARACTER   2121--LOWER CASE U
C
      DATA IOPERA( 208),IX( 208),IY( 208)/'MOVE',  -6,   5/
      DATA IOPERA( 209),IX( 209),IY( 209)/'DRAW',  -6,  -6/
      DATA IOPERA( 210),IX( 210),IY( 210)/'DRAW',  -5,  -8/
      DATA IOPERA( 211),IX( 211),IY( 211)/'DRAW',  -2,  -9/
      DATA IOPERA( 212),IX( 212),IY( 212)/'DRAW',   0,  -9/
      DATA IOPERA( 213),IX( 213),IY( 213)/'DRAW',   3,  -8/
      DATA IOPERA( 214),IX( 214),IY( 214)/'DRAW',   5,  -6/
      DATA IOPERA( 215),IX( 215),IY( 215)/'MOVE',  -5,   5/
      DATA IOPERA( 216),IX( 216),IY( 216)/'DRAW',  -5,  -6/
      DATA IOPERA( 217),IX( 217),IY( 217)/'DRAW',  -4,  -8/
      DATA IOPERA( 218),IX( 218),IY( 218)/'DRAW',  -2,  -9/
      DATA IOPERA( 219),IX( 219),IY( 219)/'MOVE',   5,   5/
      DATA IOPERA( 220),IX( 220),IY( 220)/'DRAW',   5,  -9/
      DATA IOPERA( 221),IX( 221),IY( 221)/'MOVE',   6,   5/
      DATA IOPERA( 222),IX( 222),IY( 222)/'DRAW',   6,  -9/
      DATA IOPERA( 223),IX( 223),IY( 223)/'MOVE',  -9,   5/
      DATA IOPERA( 224),IX( 224),IY( 224)/'DRAW',  -5,   5/
      DATA IOPERA( 225),IX( 225),IY( 225)/'MOVE',   2,   5/
      DATA IOPERA( 226),IX( 226),IY( 226)/'DRAW',   6,   5/
      DATA IOPERA( 227),IX( 227),IY( 227)/'MOVE',   5,  -9/
      DATA IOPERA( 228),IX( 228),IY( 228)/'DRAW',   9,  -9/
C
      DATA IXMIND(  21)/ -11/
      DATA IXMAXD(  21)/  11/
      DATA IXDELD(  21)/  22/
      DATA ISTARD(  21)/ 208/
      DATA NUMCOO(  21)/  21/
C
C     DEFINE CHARACTER   2122--LOWER CASE V
C
      DATA IOPERA( 229),IX( 229),IY( 229)/'MOVE',  -6,   5/
      DATA IOPERA( 230),IX( 230),IY( 230)/'DRAW',   0,  -9/
      DATA IOPERA( 231),IX( 231),IY( 231)/'MOVE',  -5,   5/
      DATA IOPERA( 232),IX( 232),IY( 232)/'DRAW',   0,  -7/
      DATA IOPERA( 233),IX( 233),IY( 233)/'MOVE',   6,   5/
      DATA IOPERA( 234),IX( 234),IY( 234)/'DRAW',   0,  -9/
      DATA IOPERA( 235),IX( 235),IY( 235)/'MOVE',  -8,   5/
      DATA IOPERA( 236),IX( 236),IY( 236)/'DRAW',  -2,   5/
      DATA IOPERA( 237),IX( 237),IY( 237)/'MOVE',   2,   5/
      DATA IOPERA( 238),IX( 238),IY( 238)/'DRAW',   8,   5/
C
      DATA IXMIND(  22)/  -9/
      DATA IXMAXD(  22)/   9/
      DATA IXDELD(  22)/  18/
      DATA ISTARD(  22)/ 229/
      DATA NUMCOO(  22)/  10/
C
C     DEFINE CHARACTER   2123--LOWER CASE W
C
      DATA IOPERA( 239),IX( 239),IY( 239)/'MOVE',  -8,   5/
      DATA IOPERA( 240),IX( 240),IY( 240)/'DRAW',  -4,  -9/
      DATA IOPERA( 241),IX( 241),IY( 241)/'MOVE',  -7,   5/
      DATA IOPERA( 242),IX( 242),IY( 242)/'DRAW',  -4,  -6/
      DATA IOPERA( 243),IX( 243),IY( 243)/'MOVE',   0,   5/
      DATA IOPERA( 244),IX( 244),IY( 244)/'DRAW',  -4,  -9/
      DATA IOPERA( 245),IX( 245),IY( 245)/'MOVE',   0,   5/
      DATA IOPERA( 246),IX( 246),IY( 246)/'DRAW',   4,  -9/
      DATA IOPERA( 247),IX( 247),IY( 247)/'MOVE',   1,   5/
      DATA IOPERA( 248),IX( 248),IY( 248)/'DRAW',   4,  -6/
      DATA IOPERA( 249),IX( 249),IY( 249)/'MOVE',   8,   5/
      DATA IOPERA( 250),IX( 250),IY( 250)/'DRAW',   4,  -9/
      DATA IOPERA( 251),IX( 251),IY( 251)/'MOVE', -11,   5/
      DATA IOPERA( 252),IX( 252),IY( 252)/'DRAW',  -4,   5/
      DATA IOPERA( 253),IX( 253),IY( 253)/'MOVE',   5,   5/
      DATA IOPERA( 254),IX( 254),IY( 254)/'DRAW',  11,   5/
C
      DATA IXMIND(  23)/ -12/
      DATA IXMAXD(  23)/  12/
      DATA IXDELD(  23)/  24/
      DATA ISTARD(  23)/ 239/
      DATA NUMCOO(  23)/  16/
C
C     DEFINE CHARACTER   2124--LOWER CASE X
C
      DATA IOPERA( 255),IX( 255),IY( 255)/'MOVE',  -6,   5/
      DATA IOPERA( 256),IX( 256),IY( 256)/'DRAW',   5,  -9/
      DATA IOPERA( 257),IX( 257),IY( 257)/'MOVE',  -5,   5/
      DATA IOPERA( 258),IX( 258),IY( 258)/'DRAW',   6,  -9/
      DATA IOPERA( 259),IX( 259),IY( 259)/'MOVE',   6,   5/
      DATA IOPERA( 260),IX( 260),IY( 260)/'DRAW',  -6,  -9/
      DATA IOPERA( 261),IX( 261),IY( 261)/'MOVE',  -8,   5/
      DATA IOPERA( 262),IX( 262),IY( 262)/'DRAW',  -2,   5/
      DATA IOPERA( 263),IX( 263),IY( 263)/'MOVE',   2,   5/
      DATA IOPERA( 264),IX( 264),IY( 264)/'DRAW',   8,   5/
      DATA IOPERA( 265),IX( 265),IY( 265)/'MOVE',  -8,  -9/
      DATA IOPERA( 266),IX( 266),IY( 266)/'DRAW',  -2,  -9/
      DATA IOPERA( 267),IX( 267),IY( 267)/'MOVE',   2,  -9/
      DATA IOPERA( 268),IX( 268),IY( 268)/'DRAW',   8,  -9/
C
      DATA IXMIND(  24)/ -10/
      DATA IXMAXD(  24)/  10/
      DATA IXDELD(  24)/  20/
      DATA ISTARD(  24)/ 255/
      DATA NUMCOO(  24)/  14/
C
C     DEFINE CHARACTER   2125--LOWER CASE Y
C
      DATA IOPERA( 269),IX( 269),IY( 269)/'MOVE',  -6,   5/
      DATA IOPERA( 270),IX( 270),IY( 270)/'DRAW',   0,  -9/
      DATA IOPERA( 271),IX( 271),IY( 271)/'MOVE',  -5,   5/
      DATA IOPERA( 272),IX( 272),IY( 272)/'DRAW',   0,  -7/
      DATA IOPERA( 273),IX( 273),IY( 273)/'MOVE',   6,   5/
      DATA IOPERA( 274),IX( 274),IY( 274)/'DRAW',   0,  -9/
      DATA IOPERA( 275),IX( 275),IY( 275)/'DRAW',  -2, -13/
      DATA IOPERA( 276),IX( 276),IY( 276)/'DRAW',  -4, -15/
      DATA IOPERA( 277),IX( 277),IY( 277)/'DRAW',  -6, -16/
      DATA IOPERA( 278),IX( 278),IY( 278)/'DRAW',  -7, -16/
      DATA IOPERA( 279),IX( 279),IY( 279)/'DRAW',  -8, -15/
      DATA IOPERA( 280),IX( 280),IY( 280)/'DRAW',  -7, -14/
      DATA IOPERA( 281),IX( 281),IY( 281)/'DRAW',  -6, -15/
      DATA IOPERA( 282),IX( 282),IY( 282)/'MOVE',  -8,   5/
      DATA IOPERA( 283),IX( 283),IY( 283)/'DRAW',  -2,   5/
      DATA IOPERA( 284),IX( 284),IY( 284)/'MOVE',   2,   5/
      DATA IOPERA( 285),IX( 285),IY( 285)/'DRAW',   8,   5/
C
      DATA IXMIND(  25)/ -10/
      DATA IXMAXD(  25)/   9/
      DATA IXDELD(  25)/  19/
      DATA ISTARD(  25)/ 269/
      DATA NUMCOO(  25)/  17/
C
C     DEFINE CHARACTER   2126--LOWER CASE Z
C
      DATA IOPERA( 286),IX( 286),IY( 286)/'MOVE',   5,   5/
      DATA IOPERA( 287),IX( 287),IY( 287)/'DRAW',  -6,  -9/
      DATA IOPERA( 288),IX( 288),IY( 288)/'MOVE',   6,   5/
      DATA IOPERA( 289),IX( 289),IY( 289)/'DRAW',  -5,  -9/
      DATA IOPERA( 290),IX( 290),IY( 290)/'MOVE',  -5,   5/
      DATA IOPERA( 291),IX( 291),IY( 291)/'DRAW',  -6,   1/
      DATA IOPERA( 292),IX( 292),IY( 292)/'DRAW',  -6,   5/
      DATA IOPERA( 293),IX( 293),IY( 293)/'DRAW',   6,   5/
      DATA IOPERA( 294),IX( 294),IY( 294)/'MOVE',  -6,  -9/
      DATA IOPERA( 295),IX( 295),IY( 295)/'DRAW',   6,  -9/
      DATA IOPERA( 296),IX( 296),IY( 296)/'DRAW',   6,  -5/
      DATA IOPERA( 297),IX( 297),IY( 297)/'DRAW',   5,  -9/
C
      DATA IXMIND(  26)/  -9/
      DATA IXMAXD(  26)/   9/
      DATA IXDELD(  26)/  18/
      DATA ISTARD(  26)/ 286/
      DATA NUMCOO(  26)/  12/
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCL2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **************************************
C               **  STEP 2--                        **
C               **  EXTRACT THE COORDINATES         **
C               **  FOR THIS PARTICULAR CHARACTER.  **
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
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DRCL2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ICHARN
 9013 FORMAT('ICHARN = ',I8)
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
      SUBROUTINE DRCN1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN COMPLEX NUMERIC (PART 1).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
C     DEFINE CHARACTER   2200--0
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
      DATA IOPERA(  18),IX(  18),IY(  18)/'MOVE',  -1,  12/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',  -3,  11/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',  -4,  10/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',  -5,   8/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',  -6,   3/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',  -6,   0/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',  -5,  -5/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',  -4,  -7/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',  -3,  -8/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',  -1,  -9/
      DATA IOPERA(  28),IX(  28),IY(  28)/'MOVE',   1,  -9/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',   3,  -8/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',   4,  -7/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',   5,  -5/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',   6,   0/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',   6,   3/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',   5,   8/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',   4,  10/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',   3,  11/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',   1,  12/
C
      DATA IXMIND(   1)/ -10/
      DATA IXMAXD(   1)/  10/
      DATA IXDELD(   1)/  20/
      DATA ISTARD(   1)/   1/
      DATA NUMCOO(   1)/  37/
C
C     DEFINE CHARACTER   2201--1
C
      DATA IOPERA(  38),IX(  38),IY(  38)/'MOVE',  -4,   8/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',  -2,   9/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',   1,  12/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',   1,  -9/
      DATA IOPERA(  42),IX(  42),IY(  42)/'MOVE',   0,  11/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',   0,  -9/
      DATA IOPERA(  44),IX(  44),IY(  44)/'MOVE',  -4,  -9/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',   5,  -9/
C
      DATA IXMIND(   2)/ -10/
      DATA IXMAXD(   2)/  10/
      DATA IXDELD(   2)/  20/
      DATA ISTARD(   2)/  38/
      DATA NUMCOO(   2)/   8/
C
C     DEFINE CHARACTER   2202--2
C
      DATA IOPERA(  46),IX(  46),IY(  46)/'MOVE',  -6,   8/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',  -5,   7/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',  -6,   6/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',  -7,   7/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW',  -7,   8/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW',  -6,  10/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',  -5,  11/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',  -2,  12/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',   2,  12/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',   5,  11/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',   6,  10/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',   7,   8/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',   7,   6/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',   6,   4/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW',   3,   2/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',  -2,   0/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',  -4,  -1/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',  -6,  -3/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',  -7,  -6/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',  -7,  -9/
      DATA IOPERA(  66),IX(  66),IY(  66)/'MOVE',   2,  12/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',   4,  11/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',   5,  10/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',   6,   8/
      DATA IOPERA(  70),IX(  70),IY(  70)/'DRAW',   6,   6/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',   5,   4/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',   2,   2/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',  -2,   0/
      DATA IOPERA(  74),IX(  74),IY(  74)/'MOVE',  -7,  -7/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',  -6,  -6/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',  -4,  -6/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',   1,  -8/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',   4,  -8/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',   6,  -7/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',   7,  -6/
      DATA IOPERA(  81),IX(  81),IY(  81)/'MOVE',  -4,  -6/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',   1,  -9/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',   5,  -9/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW',   6,  -8/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',   7,  -6/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',   7,  -4/
C
      DATA IXMIND(   3)/ -10/
      DATA IXMAXD(   3)/  10/
      DATA IXDELD(   3)/  20/
      DATA ISTARD(   3)/  46/
      DATA NUMCOO(   3)/  41/
C
C     DEFINE CHARACTER   2203--3
C
      DATA IOPERA(  87),IX(  87),IY(  87)/'MOVE',  -6,   8/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',  -5,   7/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',  -6,   6/
      DATA IOPERA(  90),IX(  90),IY(  90)/'DRAW',  -7,   7/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',  -7,   8/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW',  -6,  10/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',  -5,  11/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',  -2,  12/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',   2,  12/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',   5,  11/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',   6,   9/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',   6,   6/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',   5,   4/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',   2,   3/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',  -1,   3/
      DATA IOPERA( 102),IX( 102),IY( 102)/'MOVE',   2,  12/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',   4,  11/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',   5,   9/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',   5,   6/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',   4,   4/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',   2,   3/
      DATA IOPERA( 108),IX( 108),IY( 108)/'MOVE',   2,   3/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',   4,   2/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',   6,   0/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',   7,  -2/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',   7,  -5/
      DATA IOPERA( 113),IX( 113),IY( 113)/'DRAW',   6,  -7/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',   5,  -8/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',   2,  -9/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',  -2,  -9/
      DATA IOPERA( 117),IX( 117),IY( 117)/'DRAW',  -5,  -8/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',  -6,  -7/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',  -7,  -5/
      DATA IOPERA( 120),IX( 120),IY( 120)/'DRAW',  -7,  -4/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW',  -6,  -3/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW',  -5,  -4/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',  -6,  -5/
      DATA IOPERA( 124),IX( 124),IY( 124)/'MOVE',   5,   1/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',   6,  -2/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW',   6,  -5/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW',   5,  -7/
      DATA IOPERA( 128),IX( 128),IY( 128)/'DRAW',   4,  -8/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',   2,  -9/
C
      DATA IXMIND(   4)/ -10/
      DATA IXMAXD(   4)/  10/
      DATA IXDELD(   4)/  20/
      DATA ISTARD(   4)/  87/
      DATA NUMCOO(   4)/  43/
C
C     DEFINE CHARACTER   2204--4
C
      DATA IOPERA( 130),IX( 130),IY( 130)/'MOVE',   2,  10/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',   2,  -9/
      DATA IOPERA( 132),IX( 132),IY( 132)/'MOVE',   3,  12/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',   3,  -9/
      DATA IOPERA( 134),IX( 134),IY( 134)/'MOVE',   3,  12/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',  -8,  -3/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',   8,  -3/
      DATA IOPERA( 137),IX( 137),IY( 137)/'MOVE',  -1,  -9/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW',   6,  -9/
C
      DATA IXMIND(   5)/ -10/
      DATA IXMAXD(   5)/  10/
      DATA IXDELD(   5)/  20/
      DATA ISTARD(   5)/ 130/
      DATA NUMCOO(   5)/   9/
C
C     DEFINE CHARACTER   2205--5
C
      DATA IOPERA( 139),IX( 139),IY( 139)/'MOVE',  -5,  12/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW',  -7,   2/
      DATA IOPERA( 141),IX( 141),IY( 141)/'MOVE',  -7,   2/
      DATA IOPERA( 142),IX( 142),IY( 142)/'DRAW',  -5,   4/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',  -2,   5/
      DATA IOPERA( 144),IX( 144),IY( 144)/'DRAW',   1,   5/
      DATA IOPERA( 145),IX( 145),IY( 145)/'DRAW',   4,   4/
      DATA IOPERA( 146),IX( 146),IY( 146)/'DRAW',   6,   2/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',   7,  -1/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',   7,  -3/
      DATA IOPERA( 149),IX( 149),IY( 149)/'DRAW',   6,  -6/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',   4,  -8/
      DATA IOPERA( 151),IX( 151),IY( 151)/'DRAW',   1,  -9/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',  -2,  -9/
      DATA IOPERA( 153),IX( 153),IY( 153)/'DRAW',  -5,  -8/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',  -6,  -7/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW',  -7,  -5/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',  -7,  -4/
      DATA IOPERA( 157),IX( 157),IY( 157)/'DRAW',  -6,  -3/
      DATA IOPERA( 158),IX( 158),IY( 158)/'DRAW',  -5,  -4/
      DATA IOPERA( 159),IX( 159),IY( 159)/'DRAW',  -6,  -5/
      DATA IOPERA( 160),IX( 160),IY( 160)/'MOVE',   1,   5/
      DATA IOPERA( 161),IX( 161),IY( 161)/'DRAW',   3,   4/
      DATA IOPERA( 162),IX( 162),IY( 162)/'DRAW',   5,   2/
      DATA IOPERA( 163),IX( 163),IY( 163)/'DRAW',   6,  -1/
      DATA IOPERA( 164),IX( 164),IY( 164)/'DRAW',   6,  -3/
      DATA IOPERA( 165),IX( 165),IY( 165)/'DRAW',   5,  -6/
      DATA IOPERA( 166),IX( 166),IY( 166)/'DRAW',   3,  -8/
      DATA IOPERA( 167),IX( 167),IY( 167)/'DRAW',   1,  -9/
      DATA IOPERA( 168),IX( 168),IY( 168)/'MOVE',  -5,  12/
      DATA IOPERA( 169),IX( 169),IY( 169)/'DRAW',   5,  12/
      DATA IOPERA( 170),IX( 170),IY( 170)/'MOVE',  -5,  11/
      DATA IOPERA( 171),IX( 171),IY( 171)/'DRAW',   0,  11/
      DATA IOPERA( 172),IX( 172),IY( 172)/'DRAW',   5,  12/
C
      DATA IXMIND(   6)/ -10/
      DATA IXMAXD(   6)/  10/
      DATA IXDELD(   6)/  20/
      DATA ISTARD(   6)/ 139/
      DATA NUMCOO(   6)/  34/
C
C     DEFINE CHARACTER   2206--6
C
      DATA IOPERA( 173),IX( 173),IY( 173)/'MOVE',   5,   9/
      DATA IOPERA( 174),IX( 174),IY( 174)/'DRAW',   4,   8/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW',   5,   7/
      DATA IOPERA( 176),IX( 176),IY( 176)/'DRAW',   6,   8/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW',   6,   9/
      DATA IOPERA( 178),IX( 178),IY( 178)/'DRAW',   5,  11/
      DATA IOPERA( 179),IX( 179),IY( 179)/'DRAW',   3,  12/
      DATA IOPERA( 180),IX( 180),IY( 180)/'DRAW',   0,  12/
      DATA IOPERA( 181),IX( 181),IY( 181)/'DRAW',  -3,  11/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',  -5,   9/
      DATA IOPERA( 183),IX( 183),IY( 183)/'DRAW',  -6,   7/
      DATA IOPERA( 184),IX( 184),IY( 184)/'DRAW',  -7,   3/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',  -7,  -3/
      DATA IOPERA( 186),IX( 186),IY( 186)/'DRAW',  -6,  -6/
      DATA IOPERA( 187),IX( 187),IY( 187)/'DRAW',  -4,  -8/
      DATA IOPERA( 188),IX( 188),IY( 188)/'DRAW',  -1,  -9/
      DATA IOPERA( 189),IX( 189),IY( 189)/'DRAW',   1,  -9/
      DATA IOPERA( 190),IX( 190),IY( 190)/'DRAW',   4,  -8/
      DATA IOPERA( 191),IX( 191),IY( 191)/'DRAW',   6,  -6/
      DATA IOPERA( 192),IX( 192),IY( 192)/'DRAW',   7,  -3/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW',   7,  -2/
      DATA IOPERA( 194),IX( 194),IY( 194)/'DRAW',   6,   1/
      DATA IOPERA( 195),IX( 195),IY( 195)/'DRAW',   4,   3/
      DATA IOPERA( 196),IX( 196),IY( 196)/'DRAW',   1,   4/
      DATA IOPERA( 197),IX( 197),IY( 197)/'DRAW',   0,   4/
      DATA IOPERA( 198),IX( 198),IY( 198)/'DRAW',  -3,   3/
      DATA IOPERA( 199),IX( 199),IY( 199)/'DRAW',  -5,   1/
      DATA IOPERA( 200),IX( 200),IY( 200)/'DRAW',  -6,  -2/
      DATA IOPERA( 201),IX( 201),IY( 201)/'MOVE',   0,  12/
      DATA IOPERA( 202),IX( 202),IY( 202)/'DRAW',  -2,  11/
      DATA IOPERA( 203),IX( 203),IY( 203)/'DRAW',  -4,   9/
      DATA IOPERA( 204),IX( 204),IY( 204)/'DRAW',  -5,   7/
      DATA IOPERA( 205),IX( 205),IY( 205)/'DRAW',  -6,   3/
      DATA IOPERA( 206),IX( 206),IY( 206)/'DRAW',  -6,  -3/
      DATA IOPERA( 207),IX( 207),IY( 207)/'DRAW',  -5,  -6/
      DATA IOPERA( 208),IX( 208),IY( 208)/'DRAW',  -3,  -8/
      DATA IOPERA( 209),IX( 209),IY( 209)/'DRAW',  -1,  -9/
      DATA IOPERA( 210),IX( 210),IY( 210)/'MOVE',   1,  -9/
      DATA IOPERA( 211),IX( 211),IY( 211)/'DRAW',   3,  -8/
      DATA IOPERA( 212),IX( 212),IY( 212)/'DRAW',   5,  -6/
      DATA IOPERA( 213),IX( 213),IY( 213)/'DRAW',   6,  -3/
      DATA IOPERA( 214),IX( 214),IY( 214)/'DRAW',   6,  -2/
      DATA IOPERA( 215),IX( 215),IY( 215)/'DRAW',   5,   1/
      DATA IOPERA( 216),IX( 216),IY( 216)/'DRAW',   3,   3/
      DATA IOPERA( 217),IX( 217),IY( 217)/'DRAW',   1,   4/
C
      DATA IXMIND(   7)/ -10/
      DATA IXMAXD(   7)/  10/
      DATA IXDELD(   7)/  20/
      DATA ISTARD(   7)/ 173/
      DATA NUMCOO(   7)/  45/
C
C     DEFINE CHARACTER   2207--7
C
      DATA IOPERA( 218),IX( 218),IY( 218)/'MOVE',  -7,  12/
      DATA IOPERA( 219),IX( 219),IY( 219)/'DRAW',  -7,   6/
      DATA IOPERA( 220),IX( 220),IY( 220)/'MOVE',  -7,   8/
      DATA IOPERA( 221),IX( 221),IY( 221)/'DRAW',  -6,  10/
      DATA IOPERA( 222),IX( 222),IY( 222)/'DRAW',  -4,  12/
      DATA IOPERA( 223),IX( 223),IY( 223)/'DRAW',  -2,  12/
      DATA IOPERA( 224),IX( 224),IY( 224)/'DRAW',   3,   9/
      DATA IOPERA( 225),IX( 225),IY( 225)/'DRAW',   5,   9/
      DATA IOPERA( 226),IX( 226),IY( 226)/'DRAW',   6,  10/
      DATA IOPERA( 227),IX( 227),IY( 227)/'DRAW',   7,  12/
      DATA IOPERA( 228),IX( 228),IY( 228)/'MOVE',  -6,  10/
      DATA IOPERA( 229),IX( 229),IY( 229)/'DRAW',  -4,  11/
      DATA IOPERA( 230),IX( 230),IY( 230)/'DRAW',  -2,  11/
      DATA IOPERA( 231),IX( 231),IY( 231)/'DRAW',   3,   9/
      DATA IOPERA( 232),IX( 232),IY( 232)/'MOVE',   7,  12/
      DATA IOPERA( 233),IX( 233),IY( 233)/'DRAW',   7,   9/
      DATA IOPERA( 234),IX( 234),IY( 234)/'DRAW',   6,   6/
      DATA IOPERA( 235),IX( 235),IY( 235)/'DRAW',   2,   1/
      DATA IOPERA( 236),IX( 236),IY( 236)/'DRAW',   1,  -1/
      DATA IOPERA( 237),IX( 237),IY( 237)/'DRAW',   0,  -4/
      DATA IOPERA( 238),IX( 238),IY( 238)/'DRAW',   0,  -9/
      DATA IOPERA( 239),IX( 239),IY( 239)/'MOVE',   6,   6/
      DATA IOPERA( 240),IX( 240),IY( 240)/'DRAW',   1,   1/
      DATA IOPERA( 241),IX( 241),IY( 241)/'DRAW',   0,  -1/
      DATA IOPERA( 242),IX( 242),IY( 242)/'DRAW',  -1,  -4/
      DATA IOPERA( 243),IX( 243),IY( 243)/'DRAW',  -1,  -9/
C
      DATA IXMIND(   8)/ -10/
      DATA IXMAXD(   8)/  10/
      DATA IXDELD(   8)/  20/
      DATA ISTARD(   8)/ 218/
      DATA NUMCOO(   8)/  26/
C
C     DEFINE CHARACTER   2208--8
C
      DATA IOPERA( 244),IX( 244),IY( 244)/'MOVE',  -2,  12/
      DATA IOPERA( 245),IX( 245),IY( 245)/'DRAW',  -5,  11/
      DATA IOPERA( 246),IX( 246),IY( 246)/'DRAW',  -6,   9/
      DATA IOPERA( 247),IX( 247),IY( 247)/'DRAW',  -6,   6/
      DATA IOPERA( 248),IX( 248),IY( 248)/'DRAW',  -5,   4/
      DATA IOPERA( 249),IX( 249),IY( 249)/'DRAW',  -2,   3/
      DATA IOPERA( 250),IX( 250),IY( 250)/'DRAW',   2,   3/
      DATA IOPERA( 251),IX( 251),IY( 251)/'DRAW',   5,   4/
      DATA IOPERA( 252),IX( 252),IY( 252)/'DRAW',   6,   6/
      DATA IOPERA( 253),IX( 253),IY( 253)/'DRAW',   6,   9/
      DATA IOPERA( 254),IX( 254),IY( 254)/'DRAW',   5,  11/
      DATA IOPERA( 255),IX( 255),IY( 255)/'DRAW',   2,  12/
      DATA IOPERA( 256),IX( 256),IY( 256)/'DRAW',  -2,  12/
      DATA IOPERA( 257),IX( 257),IY( 257)/'MOVE',  -2,  12/
      DATA IOPERA( 258),IX( 258),IY( 258)/'DRAW',  -4,  11/
      DATA IOPERA( 259),IX( 259),IY( 259)/'DRAW',  -5,   9/
      DATA IOPERA( 260),IX( 260),IY( 260)/'DRAW',  -5,   6/
      DATA IOPERA( 261),IX( 261),IY( 261)/'DRAW',  -4,   4/
      DATA IOPERA( 262),IX( 262),IY( 262)/'DRAW',  -2,   3/
      DATA IOPERA( 263),IX( 263),IY( 263)/'MOVE',   2,   3/
      DATA IOPERA( 264),IX( 264),IY( 264)/'DRAW',   4,   4/
      DATA IOPERA( 265),IX( 265),IY( 265)/'DRAW',   5,   6/
      DATA IOPERA( 266),IX( 266),IY( 266)/'DRAW',   5,   9/
      DATA IOPERA( 267),IX( 267),IY( 267)/'DRAW',   4,  11/
      DATA IOPERA( 268),IX( 268),IY( 268)/'DRAW',   2,  12/
      DATA IOPERA( 269),IX( 269),IY( 269)/'MOVE',  -2,   3/
      DATA IOPERA( 270),IX( 270),IY( 270)/'DRAW',  -5,   2/
      DATA IOPERA( 271),IX( 271),IY( 271)/'DRAW',  -6,   1/
      DATA IOPERA( 272),IX( 272),IY( 272)/'DRAW',  -7,  -1/
      DATA IOPERA( 273),IX( 273),IY( 273)/'DRAW',  -7,  -5/
      DATA IOPERA( 274),IX( 274),IY( 274)/'DRAW',  -6,  -7/
      DATA IOPERA( 275),IX( 275),IY( 275)/'DRAW',  -5,  -8/
      DATA IOPERA( 276),IX( 276),IY( 276)/'DRAW',  -2,  -9/
      DATA IOPERA( 277),IX( 277),IY( 277)/'DRAW',   2,  -9/
      DATA IOPERA( 278),IX( 278),IY( 278)/'DRAW',   5,  -8/
      DATA IOPERA( 279),IX( 279),IY( 279)/'DRAW',   6,  -7/
      DATA IOPERA( 280),IX( 280),IY( 280)/'DRAW',   7,  -5/
      DATA IOPERA( 281),IX( 281),IY( 281)/'DRAW',   7,  -1/
      DATA IOPERA( 282),IX( 282),IY( 282)/'DRAW',   6,   1/
      DATA IOPERA( 283),IX( 283),IY( 283)/'DRAW',   5,   2/
      DATA IOPERA( 284),IX( 284),IY( 284)/'DRAW',   2,   3/
      DATA IOPERA( 285),IX( 285),IY( 285)/'MOVE',  -2,   3/
      DATA IOPERA( 286),IX( 286),IY( 286)/'DRAW',  -4,   2/
      DATA IOPERA( 287),IX( 287),IY( 287)/'DRAW',  -5,   1/
      DATA IOPERA( 288),IX( 288),IY( 288)/'DRAW',  -6,  -1/
      DATA IOPERA( 289),IX( 289),IY( 289)/'DRAW',  -6,  -5/
      DATA IOPERA( 290),IX( 290),IY( 290)/'DRAW',  -5,  -7/
      DATA IOPERA( 291),IX( 291),IY( 291)/'DRAW',  -4,  -8/
      DATA IOPERA( 292),IX( 292),IY( 292)/'DRAW',  -2,  -9/
      DATA IOPERA( 293),IX( 293),IY( 293)/'MOVE',   2,  -9/
      DATA IOPERA( 294),IX( 294),IY( 294)/'DRAW',   4,  -8/
      DATA IOPERA( 295),IX( 295),IY( 295)/'DRAW',   5,  -7/
      DATA IOPERA( 296),IX( 296),IY( 296)/'DRAW',   6,  -5/
      DATA IOPERA( 297),IX( 297),IY( 297)/'DRAW',   6,  -1/
      DATA IOPERA( 298),IX( 298),IY( 298)/'DRAW',   5,   1/
      DATA IOPERA( 299),IX( 299),IY( 299)/'DRAW',   4,   2/
      DATA IOPERA( 300),IX( 300),IY( 300)/'DRAW',   2,   3/
C
      DATA IXMIND(   9)/ -10/
      DATA IXMAXD(   9)/  10/
      DATA IXDELD(   9)/  20/
      DATA ISTARD(   9)/ 244/
      DATA NUMCOO(   9)/  57/
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCN1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **************************************
C               **  STEP 2--                        **
C               **  EXTRACT THE COORDINATES         **
C               **  FOR THIS PARTICULAR CHARACTER.  **
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
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DRCN1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ICHARN
 9013 FORMAT('ICHARN = ',I8)
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
      SUBROUTINE DRCN2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN COMPLEX NUMERIC (PART 2).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
C     DEFINE CHARACTER   2209--9
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',   6,   5/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',   5,   2/
      DATA IOPERA(   3),IX(   3),IY(   3)/'DRAW',   3,   0/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',   0,  -1/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',  -1,  -1/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',  -4,   0/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',  -6,   2/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',  -7,   5/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',  -7,   6/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',  -6,   9/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',  -4,  11/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',  -1,  12/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',   1,  12/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',   4,  11/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',   6,   9/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',   7,   6/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',   7,   0/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',   6,  -4/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',   5,  -6/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',   3,  -8/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',   0,  -9/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',  -3,  -9/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',  -5,  -8/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',  -6,  -6/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',  -6,  -5/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',  -5,  -4/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',  -4,  -5/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',  -5,  -6/
      DATA IOPERA(  29),IX(  29),IY(  29)/'MOVE',  -1,  -1/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',  -3,   0/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',  -5,   2/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',  -6,   5/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',  -6,   6/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',  -5,   9/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',  -3,  11/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',  -1,  12/
      DATA IOPERA(  37),IX(  37),IY(  37)/'MOVE',   1,  12/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',   3,  11/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',   5,   9/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',   6,   6/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',   6,   0/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',   5,  -4/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',   4,  -6/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',   2,  -8/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',   0,  -9/
C
      DATA IXMIND(  10)/ -10/
      DATA IXMAXD(  10)/  10/
      DATA IXDELD(  10)/  20/
      DATA ISTARD(  10)/   1/
      DATA NUMCOO(  10)/  45/
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCN2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **************************************
C               **  STEP 2--                        **
C               **  EXTRACT THE COORDINATES         **
C               **  FOR THIS PARTICULAR CHARACTER.  **
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
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DRCN2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ICHARN
 9013 FORMAT('ICHARN = ',I8)
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
      SUBROUTINE DRCSL1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN COMPLEX SCRIPT LOWER CASE (PART 1).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
C     DEFINE CHARACTER   2651--LOWER CASE A
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',   3,  -3/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',   2,  -1/
      DATA IOPERA(   3),IX(   3),IY(   3)/'DRAW',   0,   0/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',  -2,   0/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',  -4,  -1/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',  -5,  -2/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',  -6,  -4/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',  -6,  -6/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',  -5,  -8/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',  -3,  -9/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',  -1,  -9/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',   1,  -8/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',   2,  -6/
      DATA IOPERA(  14),IX(  14),IY(  14)/'MOVE',  -2,   0/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',  -4,  -2/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',  -5,  -4/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',  -5,  -7/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',  -3,  -9/
      DATA IOPERA(  19),IX(  19),IY(  19)/'MOVE',   4,   0/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',   2,  -6/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',   2,  -8/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',   4,  -9/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',   6,  -8/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',   7,  -7/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',   9,  -4/
      DATA IOPERA(  26),IX(  26),IY(  26)/'MOVE',   5,   0/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',   3,  -6/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',   3,  -8/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',   4,  -9/
C
      DATA IXMIND(   1)/  -7/
      DATA IXMAXD(   1)/   9/
      DATA IXDELD(   1)/  16/
      DATA ISTARD(   1)/   1/
      DATA NUMCOO(   1)/  29/
C
C     DEFINE CHARACTER   2652--LOWER CASE B
C
      DATA IOPERA(  30),IX(  30),IY(  30)/'MOVE',  -6,  -4/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',  -4,  -1/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',  -2,   3/
      DATA IOPERA(  33),IX(  33),IY(  33)/'MOVE',   1,  12/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',  -5,  -6/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',  -5,  -8/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',  -3,  -9/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',  -2,  -9/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',   0,  -8/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',   2,  -6/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',   3,  -3/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',   3,   0/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',   4,  -4/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',   5,  -5/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',   6,  -5/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',   8,  -4/
      DATA IOPERA(  46),IX(  46),IY(  46)/'MOVE',   2,  12/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',  -4,  -6/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',  -4,  -8/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',  -3,  -9/
C
      DATA IXMIND(   2)/  -6/
      DATA IXMAXD(   2)/   8/
      DATA IXDELD(   2)/  14/
      DATA ISTARD(   2)/  30/
      DATA NUMCOO(   2)/  20/
C
C     DEFINE CHARACTER   2653--LOWER CASE C
C
      DATA IOPERA(  50),IX(  50),IY(  50)/'MOVE',   2,  -1/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW',   1,  -2/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',   2,  -2/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',   2,  -1/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',   1,   0/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',  -1,   0/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',  -3,  -1/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',  -4,  -2/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',  -5,  -4/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',  -5,  -6/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW',  -4,  -8/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',  -2,  -9/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',   1,  -9/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',   4,  -7/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',   6,  -4/
      DATA IOPERA(  65),IX(  65),IY(  65)/'MOVE',  -1,   0/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW',  -3,  -2/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',  -4,  -4/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',  -4,  -7/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',  -2,  -9/
C
      DATA IXMIND(   3)/  -6/
      DATA IXMAXD(   3)/   6/
      DATA IXDELD(   3)/  12/
      DATA ISTARD(   3)/  50/
      DATA NUMCOO(   3)/  20/
C
C     DEFINE CHARACTER   2654--LOWER CASE D
C
      DATA IOPERA(  70),IX(  70),IY(  70)/'MOVE',   3,  -3/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',   2,  -1/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',   0,   0/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',  -2,   0/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',  -4,  -1/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',  -5,  -2/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',  -6,  -4/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',  -6,  -6/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',  -5,  -8/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',  -3,  -9/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',  -1,  -9/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',   1,  -8/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',   2,  -6/
      DATA IOPERA(  83),IX(  83),IY(  83)/'MOVE',  -2,   0/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW',  -4,  -2/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',  -5,  -4/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',  -5,  -7/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',  -3,  -9/
      DATA IOPERA(  88),IX(  88),IY(  88)/'MOVE',   8,  12/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',   2,  -6/
      DATA IOPERA(  90),IX(  90),IY(  90)/'DRAW',   2,  -8/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',   4,  -9/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW',   6,  -8/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',   7,  -7/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',   9,  -4/
      DATA IOPERA(  95),IX(  95),IY(  95)/'MOVE',   9,  12/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',   3,  -6/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',   3,  -8/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',   4,  -9/
C
      DATA IXMIND(   4)/  -7/
      DATA IXMAXD(   4)/   9/
      DATA IXDELD(   4)/  16/
      DATA ISTARD(   4)/  70/
      DATA NUMCOO(   4)/  29/
C
C     DEFINE CHARACTER   2655--LOWER CASE E
C
      DATA IOPERA(  99),IX(  99),IY(  99)/'MOVE',  -3,  -7/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',  -1,  -6/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',   0,  -5/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',   1,  -3/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',   1,  -1/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',   0,   0/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',  -1,   0/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',  -3,  -1/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',  -4,  -2/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',  -5,  -4/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',  -5,  -6/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',  -4,  -8/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',  -2,  -9/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',   1,  -9/
      DATA IOPERA( 113),IX( 113),IY( 113)/'DRAW',   4,  -7/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',   6,  -4/
      DATA IOPERA( 115),IX( 115),IY( 115)/'MOVE',  -1,   0/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',  -3,  -2/
      DATA IOPERA( 117),IX( 117),IY( 117)/'DRAW',  -4,  -4/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',  -4,  -7/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',  -2,  -9/
C
      DATA IXMIND(   5)/  -6/
      DATA IXMAXD(   5)/   6/
      DATA IXDELD(   5)/  12/
      DATA ISTARD(   5)/  99/
      DATA NUMCOO(   5)/  21/
C
C     DEFINE CHARACTER   2656--LOWER CASE F
C
      DATA IOPERA( 120),IX( 120),IY( 120)/'MOVE',   0,   0/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW',   3,   3/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW',   5,   6/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',   6,   9/
      DATA IOPERA( 124),IX( 124),IY( 124)/'DRAW',   6,  11/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',   5,  12/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW',   3,  11/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW',   2,   9/
      DATA IOPERA( 128),IX( 128),IY( 128)/'DRAW',  -7, -18/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',  -7, -20/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',  -6, -21/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',  -4, -20/
      DATA IOPERA( 132),IX( 132),IY( 132)/'DRAW',  -3, -17/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',  -2,  -8/
      DATA IOPERA( 134),IX( 134),IY( 134)/'DRAW',  -1,  -9/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',   1,  -9/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',   3,  -8/
      DATA IOPERA( 137),IX( 137),IY( 137)/'DRAW',   4,  -7/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW',   6,  -4/
      DATA IOPERA( 139),IX( 139),IY( 139)/'MOVE',   2,   9/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW',   1,   4/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',   0,   0/
      DATA IOPERA( 142),IX( 142),IY( 142)/'DRAW',  -3,  -9/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',  -5, -14/
      DATA IOPERA( 144),IX( 144),IY( 144)/'DRAW',  -7, -18/
C
      DATA IXMIND(   6)/  -3/
      DATA IXMAXD(   6)/   6/
      DATA IXDELD(   6)/   9/
      DATA ISTARD(   6)/ 120/
      DATA NUMCOO(   6)/  25/
C
C     DEFINE CHARACTER   2657--LOWER CASE G
C
      DATA IOPERA( 145),IX( 145),IY( 145)/'MOVE',   3,  -3/
      DATA IOPERA( 146),IX( 146),IY( 146)/'DRAW',   2,  -1/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',   0,   0/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',  -2,   0/
      DATA IOPERA( 149),IX( 149),IY( 149)/'DRAW',  -4,  -1/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',  -5,  -2/
      DATA IOPERA( 151),IX( 151),IY( 151)/'DRAW',  -6,  -4/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',  -6,  -6/
      DATA IOPERA( 153),IX( 153),IY( 153)/'DRAW',  -5,  -8/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',  -3,  -9/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW',  -1,  -9/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',   1,  -8/
      DATA IOPERA( 157),IX( 157),IY( 157)/'DRAW',   2,  -6/
      DATA IOPERA( 158),IX( 158),IY( 158)/'MOVE',  -2,   0/
      DATA IOPERA( 159),IX( 159),IY( 159)/'DRAW',  -4,  -2/
      DATA IOPERA( 160),IX( 160),IY( 160)/'DRAW',  -5,  -4/
      DATA IOPERA( 161),IX( 161),IY( 161)/'DRAW',  -5,  -7/
      DATA IOPERA( 162),IX( 162),IY( 162)/'DRAW',  -3,  -9/
      DATA IOPERA( 163),IX( 163),IY( 163)/'MOVE',   4,   0/
      DATA IOPERA( 164),IX( 164),IY( 164)/'DRAW',  -2, -18/
      DATA IOPERA( 165),IX( 165),IY( 165)/'MOVE',   5,   0/
      DATA IOPERA( 166),IX( 166),IY( 166)/'DRAW',   2,  -9/
      DATA IOPERA( 167),IX( 167),IY( 167)/'DRAW',   0, -14/
      DATA IOPERA( 168),IX( 168),IY( 168)/'DRAW',  -2, -18/
      DATA IOPERA( 169),IX( 169),IY( 169)/'DRAW',  -3, -20/
      DATA IOPERA( 170),IX( 170),IY( 170)/'DRAW',  -5, -21/
      DATA IOPERA( 171),IX( 171),IY( 171)/'DRAW',  -6, -20/
      DATA IOPERA( 172),IX( 172),IY( 172)/'DRAW',  -6, -18/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW',  -5, -15/
      DATA IOPERA( 174),IX( 174),IY( 174)/'DRAW',  -3, -13/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW',   0, -11/
      DATA IOPERA( 176),IX( 176),IY( 176)/'DRAW',   4,  -9/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW',   7,  -7/
      DATA IOPERA( 178),IX( 178),IY( 178)/'DRAW',   9,  -4/
C
      DATA IXMIND(   7)/  -7/
      DATA IXMAXD(   7)/   9/
      DATA IXDELD(   7)/  16/
      DATA ISTARD(   7)/ 145/
      DATA NUMCOO(   7)/  34/
C
C     DEFINE CHARACTER   2658--LOWER CASE H
C
      DATA IOPERA( 179),IX( 179),IY( 179)/'MOVE',  -6,  -4/
      DATA IOPERA( 180),IX( 180),IY( 180)/'DRAW',  -4,  -1/
      DATA IOPERA( 181),IX( 181),IY( 181)/'DRAW',  -2,   3/
      DATA IOPERA( 182),IX( 182),IY( 182)/'MOVE',   1,  12/
      DATA IOPERA( 183),IX( 183),IY( 183)/'DRAW',  -6,  -9/
      DATA IOPERA( 184),IX( 184),IY( 184)/'MOVE',   2,  12/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',  -5,  -9/
      DATA IOPERA( 186),IX( 186),IY( 186)/'MOVE',  -3,  -3/
      DATA IOPERA( 187),IX( 187),IY( 187)/'DRAW',  -1,  -1/
      DATA IOPERA( 188),IX( 188),IY( 188)/'DRAW',   1,   0/
      DATA IOPERA( 189),IX( 189),IY( 189)/'DRAW',   2,   0/
      DATA IOPERA( 190),IX( 190),IY( 190)/'DRAW',   4,  -1/
      DATA IOPERA( 191),IX( 191),IY( 191)/'DRAW',   4,  -3/
      DATA IOPERA( 192),IX( 192),IY( 192)/'DRAW',   3,  -6/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW',   3,  -8/
      DATA IOPERA( 194),IX( 194),IY( 194)/'DRAW',   4,  -9/
      DATA IOPERA( 195),IX( 195),IY( 195)/'MOVE',   2,   0/
      DATA IOPERA( 196),IX( 196),IY( 196)/'DRAW',   3,  -1/
      DATA IOPERA( 197),IX( 197),IY( 197)/'DRAW',   3,  -3/
      DATA IOPERA( 198),IX( 198),IY( 198)/'DRAW',   2,  -6/
      DATA IOPERA( 199),IX( 199),IY( 199)/'DRAW',   2,  -8/
      DATA IOPERA( 200),IX( 200),IY( 200)/'DRAW',   4,  -9/
      DATA IOPERA( 201),IX( 201),IY( 201)/'DRAW',   6,  -8/
      DATA IOPERA( 202),IX( 202),IY( 202)/'DRAW',   7,  -7/
      DATA IOPERA( 203),IX( 203),IY( 203)/'DRAW',   9,  -4/
C
      DATA IXMIND(   8)/  -6/
      DATA IXMAXD(   8)/   9/
      DATA IXDELD(   8)/  15/
      DATA ISTARD(   8)/ 179/
      DATA NUMCOO(   8)/  25/
C
C     DEFINE CHARACTER   2659--LOWER CASE I
C
      DATA IOPERA( 204),IX( 204),IY( 204)/'MOVE',   1,   6/
      DATA IOPERA( 205),IX( 205),IY( 205)/'DRAW',   0,   5/
      DATA IOPERA( 206),IX( 206),IY( 206)/'DRAW',   1,   4/
      DATA IOPERA( 207),IX( 207),IY( 207)/'DRAW',   2,   5/
      DATA IOPERA( 208),IX( 208),IY( 208)/'DRAW',   1,   6/
      DATA IOPERA( 209),IX( 209),IY( 209)/'MOVE',  -1,   0/
      DATA IOPERA( 210),IX( 210),IY( 210)/'DRAW',  -3,  -6/
      DATA IOPERA( 211),IX( 211),IY( 211)/'DRAW',  -3,  -8/
      DATA IOPERA( 212),IX( 212),IY( 212)/'DRAW',  -1,  -9/
      DATA IOPERA( 213),IX( 213),IY( 213)/'DRAW',   1,  -8/
      DATA IOPERA( 214),IX( 214),IY( 214)/'DRAW',   2,  -7/
      DATA IOPERA( 215),IX( 215),IY( 215)/'DRAW',   4,  -4/
      DATA IOPERA( 216),IX( 216),IY( 216)/'MOVE',   0,   0/
      DATA IOPERA( 217),IX( 217),IY( 217)/'DRAW',  -2,  -6/
      DATA IOPERA( 218),IX( 218),IY( 218)/'DRAW',  -2,  -8/
      DATA IOPERA( 219),IX( 219),IY( 219)/'DRAW',  -1,  -9/
C
      DATA IXMIND(   9)/  -4/
      DATA IXMAXD(   9)/   4/
      DATA IXDELD(   9)/   8/
      DATA ISTARD(   9)/ 204/
      DATA NUMCOO(   9)/  16/
C
C     DEFINE CHARACTER   2660--LOWER CASE J
C
      DATA IOPERA( 220),IX( 220),IY( 220)/'MOVE',   1,   6/
      DATA IOPERA( 221),IX( 221),IY( 221)/'DRAW',   0,   5/
      DATA IOPERA( 222),IX( 222),IY( 222)/'DRAW',   1,   4/
      DATA IOPERA( 223),IX( 223),IY( 223)/'DRAW',   2,   5/
      DATA IOPERA( 224),IX( 224),IY( 224)/'DRAW',   1,   6/
      DATA IOPERA( 225),IX( 225),IY( 225)/'MOVE',  -1,   0/
      DATA IOPERA( 226),IX( 226),IY( 226)/'DRAW',  -7, -18/
      DATA IOPERA( 227),IX( 227),IY( 227)/'MOVE',   0,   0/
      DATA IOPERA( 228),IX( 228),IY( 228)/'DRAW',  -3,  -9/
      DATA IOPERA( 229),IX( 229),IY( 229)/'DRAW',  -5, -14/
      DATA IOPERA( 230),IX( 230),IY( 230)/'DRAW',  -7, -18/
      DATA IOPERA( 231),IX( 231),IY( 231)/'DRAW',  -8, -20/
      DATA IOPERA( 232),IX( 232),IY( 232)/'DRAW', -10, -21/
      DATA IOPERA( 233),IX( 233),IY( 233)/'DRAW', -11, -20/
      DATA IOPERA( 234),IX( 234),IY( 234)/'DRAW', -11, -18/
      DATA IOPERA( 235),IX( 235),IY( 235)/'DRAW', -10, -15/
      DATA IOPERA( 236),IX( 236),IY( 236)/'DRAW',  -8, -13/
      DATA IOPERA( 237),IX( 237),IY( 237)/'DRAW',  -5, -11/
      DATA IOPERA( 238),IX( 238),IY( 238)/'DRAW',  -1,  -9/
      DATA IOPERA( 239),IX( 239),IY( 239)/'DRAW',   2,  -7/
      DATA IOPERA( 240),IX( 240),IY( 240)/'DRAW',   4,  -4/
C
      DATA IXMIND(  10)/  -4/
      DATA IXMAXD(  10)/   4/
      DATA IXDELD(  10)/   8/
      DATA ISTARD(  10)/ 220/
      DATA NUMCOO(  10)/  21/
C
C     DEFINE CHARACTER   2661--LOWER CASE K
C
      DATA IOPERA( 241),IX( 241),IY( 241)/'MOVE',  -6,  -4/
      DATA IOPERA( 242),IX( 242),IY( 242)/'DRAW',  -4,  -1/
      DATA IOPERA( 243),IX( 243),IY( 243)/'DRAW',  -2,   3/
      DATA IOPERA( 244),IX( 244),IY( 244)/'MOVE',   1,  12/
      DATA IOPERA( 245),IX( 245),IY( 245)/'DRAW',  -6,  -9/
      DATA IOPERA( 246),IX( 246),IY( 246)/'MOVE',   2,  12/
      DATA IOPERA( 247),IX( 247),IY( 247)/'DRAW',  -5,  -9/
      DATA IOPERA( 248),IX( 248),IY( 248)/'MOVE',   3,   0/
      DATA IOPERA( 249),IX( 249),IY( 249)/'DRAW',   3,  -1/
      DATA IOPERA( 250),IX( 250),IY( 250)/'DRAW',   4,  -1/
      DATA IOPERA( 251),IX( 251),IY( 251)/'DRAW',   3,   0/
      DATA IOPERA( 252),IX( 252),IY( 252)/'DRAW',   2,   0/
      DATA IOPERA( 253),IX( 253),IY( 253)/'DRAW',   0,  -2/
      DATA IOPERA( 254),IX( 254),IY( 254)/'DRAW',  -3,  -3/
      DATA IOPERA( 255),IX( 255),IY( 255)/'MOVE',  -3,  -3/
      DATA IOPERA( 256),IX( 256),IY( 256)/'DRAW',   0,  -4/
      DATA IOPERA( 257),IX( 257),IY( 257)/'DRAW',   1,  -8/
      DATA IOPERA( 258),IX( 258),IY( 258)/'DRAW',   2,  -9/
      DATA IOPERA( 259),IX( 259),IY( 259)/'MOVE',  -3,  -3/
      DATA IOPERA( 260),IX( 260),IY( 260)/'DRAW',  -1,  -4/
      DATA IOPERA( 261),IX( 261),IY( 261)/'DRAW',   0,  -8/
      DATA IOPERA( 262),IX( 262),IY( 262)/'DRAW',   2,  -9/
      DATA IOPERA( 263),IX( 263),IY( 263)/'DRAW',   3,  -9/
      DATA IOPERA( 264),IX( 264),IY( 264)/'DRAW',   6,  -7/
      DATA IOPERA( 265),IX( 265),IY( 265)/'DRAW',   8,  -4/
C
      DATA IXMIND(  11)/  -6/
      DATA IXMAXD(  11)/   8/
      DATA IXDELD(  11)/  14/
      DATA ISTARD(  11)/ 241/
      DATA NUMCOO(  11)/  25/
C
C     DEFINE CHARACTER   2662--LOWER CASE L
C
      DATA IOPERA( 266),IX( 266),IY( 266)/'MOVE',  -4,  -4/
      DATA IOPERA( 267),IX( 267),IY( 267)/'DRAW',  -2,  -1/
      DATA IOPERA( 268),IX( 268),IY( 268)/'DRAW',   0,   3/
      DATA IOPERA( 269),IX( 269),IY( 269)/'MOVE',   3,  12/
      DATA IOPERA( 270),IX( 270),IY( 270)/'DRAW',  -3,  -6/
      DATA IOPERA( 271),IX( 271),IY( 271)/'DRAW',  -3,  -8/
      DATA IOPERA( 272),IX( 272),IY( 272)/'DRAW',  -1,  -9/
      DATA IOPERA( 273),IX( 273),IY( 273)/'DRAW',   1,  -8/
      DATA IOPERA( 274),IX( 274),IY( 274)/'DRAW',   2,  -7/
      DATA IOPERA( 275),IX( 275),IY( 275)/'DRAW',   4,  -4/
      DATA IOPERA( 276),IX( 276),IY( 276)/'MOVE',   4,  12/
      DATA IOPERA( 277),IX( 277),IY( 277)/'DRAW',  -2,  -6/
      DATA IOPERA( 278),IX( 278),IY( 278)/'DRAW',  -2,  -8/
      DATA IOPERA( 279),IX( 279),IY( 279)/'DRAW',  -1,  -9/
C
      DATA IXMIND(  12)/  -4/
      DATA IXMAXD(  12)/   4/
      DATA IXDELD(  12)/   8/
      DATA ISTARD(  12)/ 266/
      DATA NUMCOO(  12)/  14/
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCSL1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **************************************
C               **  STEP 2--                        **
C               **  EXTRACT THE COORDINATES         **
C               **  FOR THIS PARTICULAR CHARACTER.  **
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
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DRCSL1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ICHARN
 9013 FORMAT('ICHARN = ',I8)
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
      SUBROUTINE DRCSL2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN COMPLEX SCRIPT LOWER CASE (PART 2).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
C     DEFINE CHARACTER   2663--LOWER CASE M
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE', -13,  -4/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW', -11,  -1/
      DATA IOPERA(   3),IX(   3),IY(   3)/'DRAW',  -9,   0/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',  -7,  -1/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',  -7,  -3/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',  -9,  -9/
      DATA IOPERA(   7),IX(   7),IY(   7)/'MOVE',  -9,   0/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',  -8,  -1/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',  -8,  -3/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW', -10,  -9/
      DATA IOPERA(  11),IX(  11),IY(  11)/'MOVE',  -7,  -3/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',  -5,  -1/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',  -3,   0/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',  -2,   0/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',   0,  -1/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',   0,  -3/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',  -2,  -9/
      DATA IOPERA(  18),IX(  18),IY(  18)/'MOVE',  -2,   0/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',  -1,  -1/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',  -1,  -3/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',  -3,  -9/
      DATA IOPERA(  22),IX(  22),IY(  22)/'MOVE',   0,  -3/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',   2,  -1/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',   4,   0/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',   5,   0/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',   7,  -1/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',   7,  -3/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',   6,  -6/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',   6,  -8/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',   7,  -9/
      DATA IOPERA(  31),IX(  31),IY(  31)/'MOVE',   5,   0/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',   6,  -1/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',   6,  -3/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',   5,  -6/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',   5,  -8/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',   7,  -9/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',   9,  -8/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',  10,  -7/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',  12,  -4/
C
      DATA IXMIND(  13)/ -13/
      DATA IXMAXD(  13)/  12/
      DATA IXDELD(  13)/  25/
      DATA ISTARD(  13)/   1/
      DATA NUMCOO(  13)/  39/
C
C     DEFINE CHARACTER   2664--LOWER CASE N
C
      DATA IOPERA(  40),IX(  40),IY(  40)/'MOVE',  -9,  -4/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',  -7,  -1/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',  -5,   0/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',  -3,  -1/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',  -3,  -3/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',  -5,  -9/
      DATA IOPERA(  46),IX(  46),IY(  46)/'MOVE',  -5,   0/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',  -4,  -1/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',  -4,  -3/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',  -6,  -9/
      DATA IOPERA(  50),IX(  50),IY(  50)/'MOVE',  -3,  -3/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW',  -1,  -1/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',   1,   0/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',   2,   0/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',   4,  -1/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',   4,  -3/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',   3,  -6/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',   3,  -8/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',   4,  -9/
      DATA IOPERA(  59),IX(  59),IY(  59)/'MOVE',   2,   0/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW',   3,  -1/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',   3,  -3/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',   2,  -6/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',   2,  -8/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',   4,  -9/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',   6,  -8/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW',   7,  -7/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',   9,  -4/
C
      DATA IXMIND(  14)/  -9/
      DATA IXMAXD(  14)/   9/
      DATA IXDELD(  14)/  18/
      DATA ISTARD(  14)/  40/
      DATA NUMCOO(  14)/  28/
C
C     DEFINE CHARACTER   2665--LOWER CASE O
C
      DATA IOPERA(  68),IX(  68),IY(  68)/'MOVE',   0,   0/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',  -2,   0/
      DATA IOPERA(  70),IX(  70),IY(  70)/'DRAW',  -4,  -1/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',  -5,  -2/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',  -6,  -4/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',  -6,  -6/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',  -5,  -8/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',  -3,  -9/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',  -1,  -9/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',   1,  -8/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',   2,  -7/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',   3,  -5/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',   3,  -3/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',   2,  -1/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',   0,   0/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',  -1,  -1/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW',  -1,  -3/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',   0,  -5/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',   2,  -6/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',   4,  -6/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',   6,  -5/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',   7,  -4/
      DATA IOPERA(  90),IX(  90),IY(  90)/'MOVE',  -2,   0/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',  -4,  -2/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW',  -5,  -4/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',  -5,  -7/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',  -3,  -9/
C
      DATA IXMIND(  15)/  -7/
      DATA IXMAXD(  15)/   7/
      DATA IXDELD(  15)/  14/
      DATA ISTARD(  15)/  68/
      DATA NUMCOO(  15)/  27/
C
C     DEFINE CHARACTER   2666--LOWER CASE P
C
      DATA IOPERA(  95),IX(  95),IY(  95)/'MOVE',  -6,  -4/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',  -4,  -1/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',  -2,   3/
      DATA IOPERA(  98),IX(  98),IY(  98)/'MOVE',  -1,   6/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW', -10, -21/
      DATA IOPERA( 100),IX( 100),IY( 100)/'MOVE',   0,   6/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',  -9, -21/
      DATA IOPERA( 102),IX( 102),IY( 102)/'MOVE',  -3,  -3/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',  -1,  -1/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',   1,   0/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',   2,   0/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',   4,  -1/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',   4,  -3/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',   3,  -6/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',   3,  -8/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',   4,  -9/
      DATA IOPERA( 111),IX( 111),IY( 111)/'MOVE',   2,   0/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',   3,  -1/
      DATA IOPERA( 113),IX( 113),IY( 113)/'DRAW',   3,  -3/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',   2,  -6/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',   2,  -8/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',   4,  -9/
      DATA IOPERA( 117),IX( 117),IY( 117)/'DRAW',   6,  -8/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',   7,  -7/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',   9,  -4/
C
      DATA IXMIND(  16)/  -6/
      DATA IXMAXD(  16)/   9/
      DATA IXDELD(  16)/  15/
      DATA ISTARD(  16)/  95/
      DATA NUMCOO(  16)/  25/
C
C     DEFINE CHARACTER   2667--LOWER CASE Q
C
      DATA IOPERA( 120),IX( 120),IY( 120)/'MOVE',   3,  -3/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW',   2,  -1/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW',   0,   0/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',  -2,   0/
      DATA IOPERA( 124),IX( 124),IY( 124)/'DRAW',  -4,  -1/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',  -5,  -2/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW',  -6,  -4/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW',  -6,  -6/
      DATA IOPERA( 128),IX( 128),IY( 128)/'DRAW',  -5,  -8/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',  -3,  -9/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',  -1,  -9/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',   1,  -8/
      DATA IOPERA( 132),IX( 132),IY( 132)/'MOVE',  -2,   0/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',  -4,  -2/
      DATA IOPERA( 134),IX( 134),IY( 134)/'DRAW',  -5,  -4/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',  -5,  -7/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',  -3,  -9/
      DATA IOPERA( 137),IX( 137),IY( 137)/'MOVE',   4,   0/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW',  -2, -18/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW',  -2, -20/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW',  -1, -21/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',   1, -20/
      DATA IOPERA( 142),IX( 142),IY( 142)/'DRAW',   2, -17/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',   2,  -9/
      DATA IOPERA( 144),IX( 144),IY( 144)/'DRAW',   4,  -9/
      DATA IOPERA( 145),IX( 145),IY( 145)/'DRAW',   7,  -7/
      DATA IOPERA( 146),IX( 146),IY( 146)/'DRAW',   9,  -4/
      DATA IOPERA( 147),IX( 147),IY( 147)/'MOVE',   5,   0/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',   2,  -9/
      DATA IOPERA( 149),IX( 149),IY( 149)/'DRAW',   0, -14/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',  -2, -18/
C
      DATA IXMIND(  17)/  -7/
      DATA IXMAXD(  17)/   9/
      DATA IXDELD(  17)/  16/
      DATA ISTARD(  17)/ 120/
      DATA NUMCOO(  17)/  31/
C
C     DEFINE CHARACTER   2668--LOWER CASE R
C
      DATA IOPERA( 151),IX( 151),IY( 151)/'MOVE',  -6,  -4/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',  -4,  -1/
      DATA IOPERA( 153),IX( 153),IY( 153)/'DRAW',  -2,   0/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',   0,  -1/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW',   0,  -3/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',  -2,  -9/
      DATA IOPERA( 157),IX( 157),IY( 157)/'MOVE',  -2,   0/
      DATA IOPERA( 158),IX( 158),IY( 158)/'DRAW',  -1,  -1/
      DATA IOPERA( 159),IX( 159),IY( 159)/'DRAW',  -1,  -3/
      DATA IOPERA( 160),IX( 160),IY( 160)/'DRAW',  -3,  -9/
      DATA IOPERA( 161),IX( 161),IY( 161)/'MOVE',   0,  -3/
      DATA IOPERA( 162),IX( 162),IY( 162)/'DRAW',   2,  -1/
      DATA IOPERA( 163),IX( 163),IY( 163)/'DRAW',   4,   0/
      DATA IOPERA( 164),IX( 164),IY( 164)/'DRAW',   5,   0/
      DATA IOPERA( 165),IX( 165),IY( 165)/'DRAW',   4,  -3/
      DATA IOPERA( 166),IX( 166),IY( 166)/'MOVE',   4,   0/
      DATA IOPERA( 167),IX( 167),IY( 167)/'DRAW',   4,  -3/
      DATA IOPERA( 168),IX( 168),IY( 168)/'DRAW',   5,  -5/
      DATA IOPERA( 169),IX( 169),IY( 169)/'DRAW',   6,  -5/
      DATA IOPERA( 170),IX( 170),IY( 170)/'DRAW',   8,  -4/
C
      DATA IXMIND(  18)/  -6/
      DATA IXMAXD(  18)/   8/
      DATA IXDELD(  18)/  14/
      DATA ISTARD(  18)/ 151/
      DATA NUMCOO(  18)/  20/
C
C     DEFINE CHARACTER   2669--LOWER CASE S
C
      DATA IOPERA( 171),IX( 171),IY( 171)/'MOVE',  -4,  -4/
      DATA IOPERA( 172),IX( 172),IY( 172)/'DRAW',  -2,  -1/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW',  -1,   1/
      DATA IOPERA( 174),IX( 174),IY( 174)/'DRAW',  -1,  -1/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW',   2,  -3/
      DATA IOPERA( 176),IX( 176),IY( 176)/'DRAW',   3,  -5/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW',   3,  -7/
      DATA IOPERA( 178),IX( 178),IY( 178)/'DRAW',   2,  -8/
      DATA IOPERA( 179),IX( 179),IY( 179)/'DRAW',   0,  -9/
      DATA IOPERA( 180),IX( 180),IY( 180)/'MOVE',  -1,  -1/
      DATA IOPERA( 181),IX( 181),IY( 181)/'DRAW',   1,  -3/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',   2,  -5/
      DATA IOPERA( 183),IX( 183),IY( 183)/'DRAW',   2,  -7/
      DATA IOPERA( 184),IX( 184),IY( 184)/'DRAW',   0,  -9/
      DATA IOPERA( 185),IX( 185),IY( 185)/'MOVE',  -4,  -8/
      DATA IOPERA( 186),IX( 186),IY( 186)/'DRAW',  -2,  -9/
      DATA IOPERA( 187),IX( 187),IY( 187)/'DRAW',   3,  -9/
      DATA IOPERA( 188),IX( 188),IY( 188)/'DRAW',   6,  -7/
      DATA IOPERA( 189),IX( 189),IY( 189)/'DRAW',   8,  -4/
C
      DATA IXMIND(  19)/  -4/
      DATA IXMAXD(  19)/   8/
      DATA IXDELD(  19)/  12/
      DATA ISTARD(  19)/ 171/
      DATA NUMCOO(  19)/  19/
C
C     DEFINE CHARACTER   2670--LOWER CASE T
C
      DATA IOPERA( 190),IX( 190),IY( 190)/'MOVE',  -4,  -4/
      DATA IOPERA( 191),IX( 191),IY( 191)/'DRAW',  -2,  -1/
      DATA IOPERA( 192),IX( 192),IY( 192)/'DRAW',   0,   3/
      DATA IOPERA( 193),IX( 193),IY( 193)/'MOVE',   3,  12/
      DATA IOPERA( 194),IX( 194),IY( 194)/'DRAW',  -3,  -6/
      DATA IOPERA( 195),IX( 195),IY( 195)/'DRAW',  -3,  -8/
      DATA IOPERA( 196),IX( 196),IY( 196)/'DRAW',  -1,  -9/
      DATA IOPERA( 197),IX( 197),IY( 197)/'DRAW',   1,  -8/
      DATA IOPERA( 198),IX( 198),IY( 198)/'DRAW',   2,  -7/
      DATA IOPERA( 199),IX( 199),IY( 199)/'DRAW',   4,  -4/
      DATA IOPERA( 200),IX( 200),IY( 200)/'MOVE',   4,  12/
      DATA IOPERA( 201),IX( 201),IY( 201)/'DRAW',  -2,  -6/
      DATA IOPERA( 202),IX( 202),IY( 202)/'DRAW',  -2,  -8/
      DATA IOPERA( 203),IX( 203),IY( 203)/'DRAW',  -1,  -9/
      DATA IOPERA( 204),IX( 204),IY( 204)/'MOVE',  -2,   4/
      DATA IOPERA( 205),IX( 205),IY( 205)/'DRAW',   4,   4/
C
      DATA IXMIND(  20)/  -4/
      DATA IXMAXD(  20)/   4/
      DATA IXDELD(  20)/   8/
      DATA ISTARD(  20)/ 190/
      DATA NUMCOO(  20)/  16/
C
C     DEFINE CHARACTER   2671--LOWER CASE U
C
      DATA IOPERA( 206),IX( 206),IY( 206)/'MOVE',  -4,   0/
      DATA IOPERA( 207),IX( 207),IY( 207)/'DRAW',  -6,  -6/
      DATA IOPERA( 208),IX( 208),IY( 208)/'DRAW',  -6,  -8/
      DATA IOPERA( 209),IX( 209),IY( 209)/'DRAW',  -4,  -9/
      DATA IOPERA( 210),IX( 210),IY( 210)/'DRAW',  -3,  -9/
      DATA IOPERA( 211),IX( 211),IY( 211)/'DRAW',  -1,  -8/
      DATA IOPERA( 212),IX( 212),IY( 212)/'DRAW',   1,  -6/
      DATA IOPERA( 213),IX( 213),IY( 213)/'DRAW',   3,  -3/
      DATA IOPERA( 214),IX( 214),IY( 214)/'MOVE',  -3,   0/
      DATA IOPERA( 215),IX( 215),IY( 215)/'DRAW',  -5,  -6/
      DATA IOPERA( 216),IX( 216),IY( 216)/'DRAW',  -5,  -8/
      DATA IOPERA( 217),IX( 217),IY( 217)/'DRAW',  -4,  -9/
      DATA IOPERA( 218),IX( 218),IY( 218)/'MOVE',   4,   0/
      DATA IOPERA( 219),IX( 219),IY( 219)/'DRAW',   2,  -6/
      DATA IOPERA( 220),IX( 220),IY( 220)/'DRAW',   2,  -8/
      DATA IOPERA( 221),IX( 221),IY( 221)/'DRAW',   4,  -9/
      DATA IOPERA( 222),IX( 222),IY( 222)/'DRAW',   6,  -8/
      DATA IOPERA( 223),IX( 223),IY( 223)/'DRAW',   7,  -7/
      DATA IOPERA( 224),IX( 224),IY( 224)/'DRAW',   9,  -4/
      DATA IOPERA( 225),IX( 225),IY( 225)/'MOVE',   5,   0/
      DATA IOPERA( 226),IX( 226),IY( 226)/'DRAW',   3,  -6/
      DATA IOPERA( 227),IX( 227),IY( 227)/'DRAW',   3,  -8/
      DATA IOPERA( 228),IX( 228),IY( 228)/'DRAW',   4,  -9/
C
      DATA IXMIND(  21)/  -7/
      DATA IXMAXD(  21)/   9/
      DATA IXDELD(  21)/  16/
      DATA ISTARD(  21)/ 206/
      DATA NUMCOO(  21)/  23/
C
C     DEFINE CHARACTER   2672--LOWER CASE V
C
      DATA IOPERA( 229),IX( 229),IY( 229)/'MOVE',  -4,   0/
      DATA IOPERA( 230),IX( 230),IY( 230)/'DRAW',  -5,  -2/
      DATA IOPERA( 231),IX( 231),IY( 231)/'DRAW',  -6,  -5/
      DATA IOPERA( 232),IX( 232),IY( 232)/'DRAW',  -6,  -8/
      DATA IOPERA( 233),IX( 233),IY( 233)/'DRAW',  -4,  -9/
      DATA IOPERA( 234),IX( 234),IY( 234)/'DRAW',  -3,  -9/
      DATA IOPERA( 235),IX( 235),IY( 235)/'DRAW',   0,  -8/
      DATA IOPERA( 236),IX( 236),IY( 236)/'DRAW',   2,  -6/
      DATA IOPERA( 237),IX( 237),IY( 237)/'DRAW',   3,  -3/
      DATA IOPERA( 238),IX( 238),IY( 238)/'DRAW',   3,   0/
      DATA IOPERA( 239),IX( 239),IY( 239)/'MOVE',  -3,   0/
      DATA IOPERA( 240),IX( 240),IY( 240)/'DRAW',  -4,  -2/
      DATA IOPERA( 241),IX( 241),IY( 241)/'DRAW',  -5,  -5/
      DATA IOPERA( 242),IX( 242),IY( 242)/'DRAW',  -5,  -8/
      DATA IOPERA( 243),IX( 243),IY( 243)/'DRAW',  -4,  -9/
      DATA IOPERA( 244),IX( 244),IY( 244)/'MOVE',   3,   0/
      DATA IOPERA( 245),IX( 245),IY( 245)/'DRAW',   4,  -4/
      DATA IOPERA( 246),IX( 246),IY( 246)/'DRAW',   5,  -5/
      DATA IOPERA( 247),IX( 247),IY( 247)/'DRAW',   6,  -5/
      DATA IOPERA( 248),IX( 248),IY( 248)/'DRAW',   8,  -4/
C
      DATA IXMIND(  22)/  -7/
      DATA IXMAXD(  22)/   8/
      DATA IXDELD(  22)/  15/
      DATA ISTARD(  22)/ 229/
      DATA NUMCOO(  22)/  20/
C
C     DEFINE CHARACTER   2673--LOWER CASE W
C
      DATA IOPERA( 249),IX( 249),IY( 249)/'MOVE',  -6,   0/
      DATA IOPERA( 250),IX( 250),IY( 250)/'DRAW',  -8,  -2/
      DATA IOPERA( 251),IX( 251),IY( 251)/'DRAW',  -9,  -5/
      DATA IOPERA( 252),IX( 252),IY( 252)/'DRAW',  -9,  -8/
      DATA IOPERA( 253),IX( 253),IY( 253)/'DRAW',  -7,  -9/
      DATA IOPERA( 254),IX( 254),IY( 254)/'DRAW',  -6,  -9/
      DATA IOPERA( 255),IX( 255),IY( 255)/'DRAW',  -4,  -8/
      DATA IOPERA( 256),IX( 256),IY( 256)/'DRAW',  -2,  -6/
      DATA IOPERA( 257),IX( 257),IY( 257)/'MOVE',  -5,   0/
      DATA IOPERA( 258),IX( 258),IY( 258)/'DRAW',  -7,  -2/
      DATA IOPERA( 259),IX( 259),IY( 259)/'DRAW',  -8,  -5/
      DATA IOPERA( 260),IX( 260),IY( 260)/'DRAW',  -8,  -8/
      DATA IOPERA( 261),IX( 261),IY( 261)/'DRAW',  -7,  -9/
      DATA IOPERA( 262),IX( 262),IY( 262)/'MOVE',   0,   0/
      DATA IOPERA( 263),IX( 263),IY( 263)/'DRAW',  -2,  -6/
      DATA IOPERA( 264),IX( 264),IY( 264)/'DRAW',  -2,  -8/
      DATA IOPERA( 265),IX( 265),IY( 265)/'DRAW',   0,  -9/
      DATA IOPERA( 266),IX( 266),IY( 266)/'DRAW',   1,  -9/
      DATA IOPERA( 267),IX( 267),IY( 267)/'DRAW',   3,  -8/
      DATA IOPERA( 268),IX( 268),IY( 268)/'DRAW',   5,  -6/
      DATA IOPERA( 269),IX( 269),IY( 269)/'DRAW',   6,  -3/
      DATA IOPERA( 270),IX( 270),IY( 270)/'DRAW',   6,   0/
      DATA IOPERA( 271),IX( 271),IY( 271)/'MOVE',   1,   0/
      DATA IOPERA( 272),IX( 272),IY( 272)/'DRAW',  -1,  -6/
      DATA IOPERA( 273),IX( 273),IY( 273)/'DRAW',  -1,  -8/
      DATA IOPERA( 274),IX( 274),IY( 274)/'DRAW',   0,  -9/
      DATA IOPERA( 275),IX( 275),IY( 275)/'MOVE',   6,   0/
      DATA IOPERA( 276),IX( 276),IY( 276)/'DRAW',   7,  -4/
      DATA IOPERA( 277),IX( 277),IY( 277)/'DRAW',   8,  -5/
      DATA IOPERA( 278),IX( 278),IY( 278)/'DRAW',   9,  -5/
      DATA IOPERA( 279),IX( 279),IY( 279)/'DRAW',  11,  -4/
C
      DATA IXMIND(  23)/ -10/
      DATA IXMAXD(  23)/  11/
      DATA IXDELD(  23)/  21/
      DATA ISTARD(  23)/ 249/
      DATA NUMCOO(  23)/  31/
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCSL2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **************************************
C               **  STEP 2--                        **
C               **  EXTRACT THE COORDINATES         **
C               **  FOR THIS PARTICULAR CHARACTER.  **
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
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DRCSL2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ICHARN
 9013 FORMAT('ICHARN = ',I8)
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
      SUBROUTINE DRCSL3(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN COMPLEX SCRIPT LOWER CASE (PART 3).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
C     DEFINE CHARACTER   2674--LOWER CASE X
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',  -8,  -4/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',  -6,  -1/
      DATA IOPERA(   3),IX(   3),IY(   3)/'DRAW',  -4,   0/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',  -2,   0/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',  -1,  -1/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',  -1,  -3/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',  -2,  -6/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',  -3,  -8/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',  -5,  -9/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',  -6,  -9/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',  -7,  -8/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',  -7,  -7/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',  -6,  -7/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',  -7,  -8/
      DATA IOPERA(  15),IX(  15),IY(  15)/'MOVE',   5,  -1/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',   4,  -2/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',   5,  -2/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',   5,  -1/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',   4,   0/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',   3,   0/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',   1,  -1/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',   0,  -3/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',  -1,  -6/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',  -1,  -8/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',   0,  -9/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',   3,  -9/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',   6,  -7/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',   8,  -4/
      DATA IOPERA(  29),IX(  29),IY(  29)/'MOVE',  -1,  -1/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',   0,  -3/
      DATA IOPERA(  31),IX(  31),IY(  31)/'MOVE',   1,  -1/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',  -1,  -3/
      DATA IOPERA(  33),IX(  33),IY(  33)/'MOVE',  -2,  -6/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',  -1,  -8/
      DATA IOPERA(  35),IX(  35),IY(  35)/'MOVE',  -1,  -6/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',  -3,  -8/
C
      DATA IXMIND(  24)/  -8/
      DATA IXMAXD(  24)/   8/
      DATA IXDELD(  24)/  16/
      DATA ISTARD(  24)/   1/
      DATA NUMCOO(  24)/  36/
C
C     DEFINE CHARACTER   2675--LOWER CASE Y
C
      DATA IOPERA(  37),IX(  37),IY(  37)/'MOVE',  -4,   0/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',  -6,  -6/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',  -6,  -8/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',  -4,  -9/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',  -3,  -9/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',  -1,  -8/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',   1,  -6/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',   3,  -3/
      DATA IOPERA(  45),IX(  45),IY(  45)/'MOVE',  -3,   0/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW',  -5,  -6/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',  -5,  -8/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',  -4,  -9/
      DATA IOPERA(  49),IX(  49),IY(  49)/'MOVE',   4,   0/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW',  -2, -18/
      DATA IOPERA(  51),IX(  51),IY(  51)/'MOVE',   5,   0/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',   2,  -9/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',   0, -14/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',  -2, -18/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',  -3, -20/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',  -5, -21/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',  -6, -20/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',  -6, -18/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',  -5, -15/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW',  -3, -13/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',   0, -11/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',   4,  -9/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',   7,  -7/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',   9,  -4/
C
      DATA IXMIND(  25)/  -7/
      DATA IXMAXD(  25)/   9/
      DATA IXDELD(  25)/  16/
      DATA ISTARD(  25)/  37/
      DATA NUMCOO(  25)/  28/
C
C     DEFINE CHARACTER   2676--LOWER CASE Z
C
      DATA IOPERA(  65),IX(  65),IY(  65)/'MOVE',  -6,  -4/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW',  -4,  -1/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',  -2,   0/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',   0,   0/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',   2,  -1/
      DATA IOPERA(  70),IX(  70),IY(  70)/'DRAW',   2,  -4/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',   1,  -6/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',  -2,  -8/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',  -4,  -9/
      DATA IOPERA(  74),IX(  74),IY(  74)/'MOVE',   0,   0/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',   1,  -1/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',   1,  -4/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',   0,  -6/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',  -2,  -8/
      DATA IOPERA(  79),IX(  79),IY(  79)/'MOVE',  -4,  -9/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',  -2, -10/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',  -1, -12/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',  -1, -15/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',  -2, -18/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW',  -4, -20/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',  -6, -21/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',  -7, -20/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',  -7, -18/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',  -6, -15/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',  -3, -12/
      DATA IOPERA(  90),IX(  90),IY(  90)/'DRAW',   0, -10/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',   4,  -7/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW',   7,  -4/
      DATA IOPERA(  93),IX(  93),IY(  93)/'MOVE',  -4,  -9/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',  -3, -10/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',  -2, -12/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',  -2, -15/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',  -3, -18/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',  -4, -20/
C
      DATA IXMIND(  26)/  -6/
      DATA IXMAXD(  26)/   7/
      DATA IXDELD(  26)/  13/
      DATA ISTARD(  26)/  65/
      DATA NUMCOO(  26)/  34/
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCSL3--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **************************************
C               **  STEP 2--                        **
C               **  EXTRACT THE COORDINATES         **
C               **  FOR THIS PARTICULAR CHARACTER.  **
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
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DRCSL3--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ICHARN
 9013 FORMAT('ICHARN = ',I8)
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
      SUBROUTINE DRCSN1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN COMPLEX SCRIPT NUMERIC (PART 1).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
C     DEFINE CHARACTER   2750--0
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',   2,  12/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',  -1,  11/
      DATA IOPERA(   3),IX(   3),IY(   3)/'DRAW',  -3,   9/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',  -5,   6/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',  -6,   3/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',  -7,  -1/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',  -7,  -4/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',  -6,  -7/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',  -5,  -8/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',  -3,  -9/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',  -1,  -9/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',   2,  -8/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',   4,  -6/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',   6,  -3/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',   7,   0/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',   8,   4/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',   8,   7/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',   7,  10/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',   6,  11/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',   4,  12/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',   2,  12/
      DATA IOPERA(  22),IX(  22),IY(  22)/'MOVE',   2,  12/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',   0,  11/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',  -2,   9/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',  -4,   6/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',  -5,   3/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',  -6,  -1/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',  -6,  -4/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',  -5,  -7/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',  -3,  -9/
      DATA IOPERA(  31),IX(  31),IY(  31)/'MOVE',  -1,  -9/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',   1,  -8/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',   3,  -6/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',   5,  -3/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',   6,   0/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',   7,   4/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',   7,   7/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',   6,  10/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',   4,  12/
C
      DATA IXMIND(   1)/ -10/
      DATA IXMAXD(   1)/  11/
      DATA IXDELD(   1)/  21/
      DATA ISTARD(   1)/   1/
      DATA NUMCOO(   1)/  39/
C
C     DEFINE CHARACTER   2751--1
C
      DATA IOPERA(  40),IX(  40),IY(  40)/'MOVE',   2,   8/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',  -3,  -9/
      DATA IOPERA(  42),IX(  42),IY(  42)/'MOVE',   4,  12/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',  -2,  -9/
      DATA IOPERA(  44),IX(  44),IY(  44)/'MOVE',   4,  12/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',   1,   9/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW',  -2,   7/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',  -4,   6/
      DATA IOPERA(  48),IX(  48),IY(  48)/'MOVE',   3,   9/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',  -1,   7/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW',  -4,   6/
C
      DATA IXMIND(   2)/ -10/
      DATA IXMAXD(   2)/  11/
      DATA IXDELD(   2)/  21/
      DATA ISTARD(   2)/  40/
      DATA NUMCOO(   2)/  11/
C
C     DEFINE CHARACTER   2752--2
C
      DATA IOPERA(  51),IX(  51),IY(  51)/'MOVE',  -3,   8/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',  -2,   7/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',  -3,   6/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',  -4,   7/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',  -4,   8/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',  -3,  10/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',  -2,  11/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',   1,  12/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',   4,  12/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW',   7,  11/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',   8,   9/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',   8,   7/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',   7,   5/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',   5,   3/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',   2,   1/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW',  -2,  -1/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',  -5,  -3/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',  -7,  -5/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',  -9,  -9/
      DATA IOPERA(  70),IX(  70),IY(  70)/'MOVE',   4,  12/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',   6,  11/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',   7,   9/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',   7,   7/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',   6,   5/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',   4,   3/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',  -2,  -1/
      DATA IOPERA(  77),IX(  77),IY(  77)/'MOVE',  -8,  -7/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',  -7,  -6/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',  -5,  -6/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',   0,  -8/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',   3,  -8/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',   5,  -7/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',   6,  -5/
      DATA IOPERA(  84),IX(  84),IY(  84)/'MOVE',  -5,  -6/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',   0,  -9/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',   3,  -9/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',   5,  -8/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',   6,  -5/
C
      DATA IXMIND(   3)/ -10/
      DATA IXMAXD(   3)/  11/
      DATA IXDELD(   3)/  21/
      DATA ISTARD(   3)/  51/
      DATA NUMCOO(   3)/  38/
C
C     DEFINE CHARACTER   2753--3
C
      DATA IOPERA(  89),IX(  89),IY(  89)/'MOVE',  -3,   8/
      DATA IOPERA(  90),IX(  90),IY(  90)/'DRAW',  -2,   7/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',  -3,   6/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW',  -4,   7/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',  -4,   8/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',  -3,  10/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',  -2,  11/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',   1,  12/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',   4,  12/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',   7,  11/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',   8,   9/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',   8,   7/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',   7,   5/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',   4,   3/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',   1,   2/
      DATA IOPERA( 104),IX( 104),IY( 104)/'MOVE',   4,  12/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',   6,  11/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',   7,   9/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',   7,   7/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',   6,   5/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',   4,   3/
      DATA IOPERA( 110),IX( 110),IY( 110)/'MOVE',  -1,   2/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',   1,   2/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',   4,   1/
      DATA IOPERA( 113),IX( 113),IY( 113)/'DRAW',   5,   0/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',   6,  -2/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',   6,  -5/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',   5,  -7/
      DATA IOPERA( 117),IX( 117),IY( 117)/'DRAW',   4,  -8/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',   1,  -9/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',  -3,  -9/
      DATA IOPERA( 120),IX( 120),IY( 120)/'DRAW',  -6,  -8/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW',  -7,  -7/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW',  -8,  -5/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',  -8,  -4/
      DATA IOPERA( 124),IX( 124),IY( 124)/'DRAW',  -7,  -3/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',  -6,  -4/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW',  -7,  -5/
      DATA IOPERA( 127),IX( 127),IY( 127)/'MOVE',   1,   2/
      DATA IOPERA( 128),IX( 128),IY( 128)/'DRAW',   3,   1/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',   4,   0/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',   5,  -2/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',   5,  -5/
      DATA IOPERA( 132),IX( 132),IY( 132)/'DRAW',   4,  -7/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',   3,  -8/
      DATA IOPERA( 134),IX( 134),IY( 134)/'DRAW',   1,  -9/
C
      DATA IXMIND(   4)/ -10/
      DATA IXMAXD(   4)/  11/
      DATA IXDELD(   4)/  21/
      DATA ISTARD(   4)/  89/
      DATA NUMCOO(   4)/  46/
C
C     DEFINE CHARACTER   2754--4
C
      DATA IOPERA( 135),IX( 135),IY( 135)/'MOVE',   6,  11/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',   0,  -9/
      DATA IOPERA( 137),IX( 137),IY( 137)/'MOVE',   7,  12/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW',   1,  -9/
      DATA IOPERA( 139),IX( 139),IY( 139)/'MOVE',   7,  12/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW',  -8,  -3/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',   8,  -3/
C
      DATA IXMIND(   5)/ -10/
      DATA IXMAXD(   5)/  11/
      DATA IXDELD(   5)/  21/
      DATA ISTARD(   5)/ 135/
      DATA NUMCOO(   5)/   7/
C
C     DEFINE CHARACTER   2755--5
C
      DATA IOPERA( 142),IX( 142),IY( 142)/'MOVE',  -1,  12/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',  -6,   2/
      DATA IOPERA( 144),IX( 144),IY( 144)/'MOVE',  -1,  12/
      DATA IOPERA( 145),IX( 145),IY( 145)/'DRAW',   9,  12/
      DATA IOPERA( 146),IX( 146),IY( 146)/'MOVE',  -1,  11/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',   4,  11/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',   9,  12/
      DATA IOPERA( 149),IX( 149),IY( 149)/'MOVE',  -6,   2/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',  -5,   3/
      DATA IOPERA( 151),IX( 151),IY( 151)/'DRAW',  -2,   4/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',   1,   4/
      DATA IOPERA( 153),IX( 153),IY( 153)/'DRAW',   4,   3/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',   5,   2/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW',   6,   0/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',   6,  -3/
      DATA IOPERA( 157),IX( 157),IY( 157)/'DRAW',   5,  -6/
      DATA IOPERA( 158),IX( 158),IY( 158)/'DRAW',   3,  -8/
      DATA IOPERA( 159),IX( 159),IY( 159)/'DRAW',   0,  -9/
      DATA IOPERA( 160),IX( 160),IY( 160)/'DRAW',  -3,  -9/
      DATA IOPERA( 161),IX( 161),IY( 161)/'DRAW',  -6,  -8/
      DATA IOPERA( 162),IX( 162),IY( 162)/'DRAW',  -7,  -7/
      DATA IOPERA( 163),IX( 163),IY( 163)/'DRAW',  -8,  -5/
      DATA IOPERA( 164),IX( 164),IY( 164)/'DRAW',  -8,  -4/
      DATA IOPERA( 165),IX( 165),IY( 165)/'DRAW',  -7,  -3/
      DATA IOPERA( 166),IX( 166),IY( 166)/'DRAW',  -6,  -4/
      DATA IOPERA( 167),IX( 167),IY( 167)/'DRAW',  -7,  -5/
      DATA IOPERA( 168),IX( 168),IY( 168)/'MOVE',   1,   4/
      DATA IOPERA( 169),IX( 169),IY( 169)/'DRAW',   3,   3/
      DATA IOPERA( 170),IX( 170),IY( 170)/'DRAW',   4,   2/
      DATA IOPERA( 171),IX( 171),IY( 171)/'DRAW',   5,   0/
      DATA IOPERA( 172),IX( 172),IY( 172)/'DRAW',   5,  -3/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW',   4,  -6/
      DATA IOPERA( 174),IX( 174),IY( 174)/'DRAW',   2,  -8/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW',   0,  -9/
C
      DATA IXMIND(   6)/ -10/
      DATA IXMAXD(   6)/  11/
      DATA IXDELD(   6)/  21/
      DATA ISTARD(   6)/ 142/
      DATA NUMCOO(   6)/  34/
C
C     DEFINE CHARACTER   2756--6
C
      DATA IOPERA( 176),IX( 176),IY( 176)/'MOVE',   7,   9/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW',   6,   8/
      DATA IOPERA( 178),IX( 178),IY( 178)/'DRAW',   7,   7/
      DATA IOPERA( 179),IX( 179),IY( 179)/'DRAW',   8,   8/
      DATA IOPERA( 180),IX( 180),IY( 180)/'DRAW',   8,   9/
      DATA IOPERA( 181),IX( 181),IY( 181)/'DRAW',   7,  11/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',   5,  12/
      DATA IOPERA( 183),IX( 183),IY( 183)/'DRAW',   2,  12/
      DATA IOPERA( 184),IX( 184),IY( 184)/'DRAW',  -1,  11/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',  -3,   9/
      DATA IOPERA( 186),IX( 186),IY( 186)/'DRAW',  -5,   6/
      DATA IOPERA( 187),IX( 187),IY( 187)/'DRAW',  -6,   3/
      DATA IOPERA( 188),IX( 188),IY( 188)/'DRAW',  -7,  -1/
      DATA IOPERA( 189),IX( 189),IY( 189)/'DRAW',  -7,  -5/
      DATA IOPERA( 190),IX( 190),IY( 190)/'DRAW',  -6,  -7/
      DATA IOPERA( 191),IX( 191),IY( 191)/'DRAW',  -5,  -8/
      DATA IOPERA( 192),IX( 192),IY( 192)/'DRAW',  -3,  -9/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW',   0,  -9/
      DATA IOPERA( 194),IX( 194),IY( 194)/'DRAW',   3,  -8/
      DATA IOPERA( 195),IX( 195),IY( 195)/'DRAW',   5,  -6/
      DATA IOPERA( 196),IX( 196),IY( 196)/'DRAW',   6,  -4/
      DATA IOPERA( 197),IX( 197),IY( 197)/'DRAW',   6,  -1/
      DATA IOPERA( 198),IX( 198),IY( 198)/'DRAW',   5,   1/
      DATA IOPERA( 199),IX( 199),IY( 199)/'DRAW',   4,   2/
      DATA IOPERA( 200),IX( 200),IY( 200)/'DRAW',   2,   3/
      DATA IOPERA( 201),IX( 201),IY( 201)/'DRAW',  -1,   3/
      DATA IOPERA( 202),IX( 202),IY( 202)/'DRAW',  -3,   2/
      DATA IOPERA( 203),IX( 203),IY( 203)/'DRAW',  -5,   0/
      DATA IOPERA( 204),IX( 204),IY( 204)/'DRAW',  -6,  -2/
      DATA IOPERA( 205),IX( 205),IY( 205)/'MOVE',   2,  12/
      DATA IOPERA( 206),IX( 206),IY( 206)/'DRAW',   0,  11/
      DATA IOPERA( 207),IX( 207),IY( 207)/'DRAW',  -2,   9/
      DATA IOPERA( 208),IX( 208),IY( 208)/'DRAW',  -4,   6/
      DATA IOPERA( 209),IX( 209),IY( 209)/'DRAW',  -5,   3/
      DATA IOPERA( 210),IX( 210),IY( 210)/'DRAW',  -6,  -1/
      DATA IOPERA( 211),IX( 211),IY( 211)/'DRAW',  -6,  -6/
      DATA IOPERA( 212),IX( 212),IY( 212)/'DRAW',  -5,  -8/
      DATA IOPERA( 213),IX( 213),IY( 213)/'MOVE',   0,  -9/
      DATA IOPERA( 214),IX( 214),IY( 214)/'DRAW',   2,  -8/
      DATA IOPERA( 215),IX( 215),IY( 215)/'DRAW',   4,  -6/
      DATA IOPERA( 216),IX( 216),IY( 216)/'DRAW',   5,  -4/
      DATA IOPERA( 217),IX( 217),IY( 217)/'DRAW',   5,   0/
      DATA IOPERA( 218),IX( 218),IY( 218)/'DRAW',   4,   2/
C
      DATA IXMIND(   7)/ -10/
      DATA IXMAXD(   7)/  11/
      DATA IXDELD(   7)/  21/
      DATA ISTARD(   7)/ 176/
      DATA NUMCOO(   7)/  43/
C
C     DEFINE CHARACTER   2757--7
C
      DATA IOPERA( 219),IX( 219),IY( 219)/'MOVE',  -4,  12/
      DATA IOPERA( 220),IX( 220),IY( 220)/'DRAW',  -6,   6/
      DATA IOPERA( 221),IX( 221),IY( 221)/'MOVE',   9,  12/
      DATA IOPERA( 222),IX( 222),IY( 222)/'DRAW',   8,   9/
      DATA IOPERA( 223),IX( 223),IY( 223)/'DRAW',   6,   6/
      DATA IOPERA( 224),IX( 224),IY( 224)/'DRAW',   1,   0/
      DATA IOPERA( 225),IX( 225),IY( 225)/'DRAW',  -1,  -3/
      DATA IOPERA( 226),IX( 226),IY( 226)/'DRAW',  -2,  -5/
      DATA IOPERA( 227),IX( 227),IY( 227)/'DRAW',  -3,  -9/
      DATA IOPERA( 228),IX( 228),IY( 228)/'MOVE',   6,   6/
      DATA IOPERA( 229),IX( 229),IY( 229)/'DRAW',   0,   0/
      DATA IOPERA( 230),IX( 230),IY( 230)/'DRAW',  -2,  -3/
      DATA IOPERA( 231),IX( 231),IY( 231)/'DRAW',  -3,  -5/
      DATA IOPERA( 232),IX( 232),IY( 232)/'DRAW',  -4,  -9/
      DATA IOPERA( 233),IX( 233),IY( 233)/'MOVE',  -5,   9/
      DATA IOPERA( 234),IX( 234),IY( 234)/'DRAW',  -2,  12/
      DATA IOPERA( 235),IX( 235),IY( 235)/'DRAW',   0,  12/
      DATA IOPERA( 236),IX( 236),IY( 236)/'DRAW',   5,   9/
      DATA IOPERA( 237),IX( 237),IY( 237)/'MOVE',  -4,  10/
      DATA IOPERA( 238),IX( 238),IY( 238)/'DRAW',  -2,  11/
      DATA IOPERA( 239),IX( 239),IY( 239)/'DRAW',   0,  11/
      DATA IOPERA( 240),IX( 240),IY( 240)/'DRAW',   5,   9/
      DATA IOPERA( 241),IX( 241),IY( 241)/'DRAW',   7,   9/
      DATA IOPERA( 242),IX( 242),IY( 242)/'DRAW',   8,  10/
      DATA IOPERA( 243),IX( 243),IY( 243)/'DRAW',   9,  12/
C
      DATA IXMIND(   8)/ -10/
      DATA IXMAXD(   8)/  11/
      DATA IXDELD(   8)/  21/
      DATA ISTARD(   8)/ 219/
      DATA NUMCOO(   8)/  25/
C
C     DEFINE CHARACTER   2758--8
C
      DATA IOPERA( 244),IX( 244),IY( 244)/'MOVE',   1,  12/
      DATA IOPERA( 245),IX( 245),IY( 245)/'DRAW',  -2,  11/
      DATA IOPERA( 246),IX( 246),IY( 246)/'DRAW',  -3,  10/
      DATA IOPERA( 247),IX( 247),IY( 247)/'DRAW',  -4,   8/
      DATA IOPERA( 248),IX( 248),IY( 248)/'DRAW',  -4,   5/
      DATA IOPERA( 249),IX( 249),IY( 249)/'DRAW',  -3,   3/
      DATA IOPERA( 250),IX( 250),IY( 250)/'DRAW',  -1,   2/
      DATA IOPERA( 251),IX( 251),IY( 251)/'DRAW',   2,   2/
      DATA IOPERA( 252),IX( 252),IY( 252)/'DRAW',   6,   3/
      DATA IOPERA( 253),IX( 253),IY( 253)/'DRAW',   7,   4/
      DATA IOPERA( 254),IX( 254),IY( 254)/'DRAW',   8,   6/
      DATA IOPERA( 255),IX( 255),IY( 255)/'DRAW',   8,   9/
      DATA IOPERA( 256),IX( 256),IY( 256)/'DRAW',   7,  11/
      DATA IOPERA( 257),IX( 257),IY( 257)/'DRAW',   4,  12/
      DATA IOPERA( 258),IX( 258),IY( 258)/'DRAW',   1,  12/
      DATA IOPERA( 259),IX( 259),IY( 259)/'MOVE',   1,  12/
      DATA IOPERA( 260),IX( 260),IY( 260)/'DRAW',  -1,  11/
      DATA IOPERA( 261),IX( 261),IY( 261)/'DRAW',  -2,  10/
      DATA IOPERA( 262),IX( 262),IY( 262)/'DRAW',  -3,   8/
      DATA IOPERA( 263),IX( 263),IY( 263)/'DRAW',  -3,   5/
      DATA IOPERA( 264),IX( 264),IY( 264)/'DRAW',  -2,   3/
      DATA IOPERA( 265),IX( 265),IY( 265)/'DRAW',  -1,   2/
      DATA IOPERA( 266),IX( 266),IY( 266)/'MOVE',   2,   2/
      DATA IOPERA( 267),IX( 267),IY( 267)/'DRAW',   5,   3/
      DATA IOPERA( 268),IX( 268),IY( 268)/'DRAW',   6,   4/
      DATA IOPERA( 269),IX( 269),IY( 269)/'DRAW',   7,   6/
      DATA IOPERA( 270),IX( 270),IY( 270)/'DRAW',   7,   9/
      DATA IOPERA( 271),IX( 271),IY( 271)/'DRAW',   6,  11/
      DATA IOPERA( 272),IX( 272),IY( 272)/'DRAW',   4,  12/
      DATA IOPERA( 273),IX( 273),IY( 273)/'MOVE',  -1,   2/
      DATA IOPERA( 274),IX( 274),IY( 274)/'DRAW',  -5,   1/
      DATA IOPERA( 275),IX( 275),IY( 275)/'DRAW',  -7,  -1/
      DATA IOPERA( 276),IX( 276),IY( 276)/'DRAW',  -8,  -3/
      DATA IOPERA( 277),IX( 277),IY( 277)/'DRAW',  -8,  -6/
      DATA IOPERA( 278),IX( 278),IY( 278)/'DRAW',  -7,  -8/
      DATA IOPERA( 279),IX( 279),IY( 279)/'DRAW',  -4,  -9/
      DATA IOPERA( 280),IX( 280),IY( 280)/'DRAW',   0,  -9/
      DATA IOPERA( 281),IX( 281),IY( 281)/'DRAW',   4,  -8/
      DATA IOPERA( 282),IX( 282),IY( 282)/'DRAW',   5,  -7/
      DATA IOPERA( 283),IX( 283),IY( 283)/'DRAW',   6,  -5/
      DATA IOPERA( 284),IX( 284),IY( 284)/'DRAW',   6,  -2/
      DATA IOPERA( 285),IX( 285),IY( 285)/'DRAW',   5,   0/
      DATA IOPERA( 286),IX( 286),IY( 286)/'DRAW',   4,   1/
      DATA IOPERA( 287),IX( 287),IY( 287)/'DRAW',   2,   2/
      DATA IOPERA( 288),IX( 288),IY( 288)/'MOVE',  -1,   2/
      DATA IOPERA( 289),IX( 289),IY( 289)/'DRAW',  -4,   1/
      DATA IOPERA( 290),IX( 290),IY( 290)/'DRAW',  -6,  -1/
      DATA IOPERA( 291),IX( 291),IY( 291)/'DRAW',  -7,  -3/
      DATA IOPERA( 292),IX( 292),IY( 292)/'DRAW',  -7,  -6/
      DATA IOPERA( 293),IX( 293),IY( 293)/'DRAW',  -6,  -8/
      DATA IOPERA( 294),IX( 294),IY( 294)/'DRAW',  -4,  -9/
      DATA IOPERA( 295),IX( 295),IY( 295)/'MOVE',   0,  -9/
      DATA IOPERA( 296),IX( 296),IY( 296)/'DRAW',   3,  -8/
      DATA IOPERA( 297),IX( 297),IY( 297)/'DRAW',   4,  -7/
      DATA IOPERA( 298),IX( 298),IY( 298)/'DRAW',   5,  -5/
      DATA IOPERA( 299),IX( 299),IY( 299)/'DRAW',   5,  -1/
      DATA IOPERA( 300),IX( 300),IY( 300)/'DRAW',   4,   1/
C
      DATA IXMIND(   9)/ -10/
      DATA IXMAXD(   9)/  11/
      DATA IXDELD(   9)/  21/
      DATA ISTARD(   9)/ 244/
      DATA NUMCOO(   9)/  57/
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCSN1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **************************************
C               **  STEP 2--                        **
C               **  EXTRACT THE COORDINATES         **
C               **  FOR THIS PARTICULAR CHARACTER.  **
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
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DRCSN1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ICHARN
 9013 FORMAT('ICHARN = ',I8)
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
      SUBROUTINE DRCSN2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN COMPLEX SCRIPT NUMERIC (PART 2).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
C     DEFINE CHARACTER   2759--9
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',   7,   5/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',   6,   3/
      DATA IOPERA(   3),IX(   3),IY(   3)/'DRAW',   4,   1/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',   2,   0/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',  -1,   0/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',  -3,   1/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',  -4,   2/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',  -5,   4/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',  -5,   7/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',  -4,   9/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',  -2,  11/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',   1,  12/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',   4,  12/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',   6,  11/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',   7,  10/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',   8,   8/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',   8,   4/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',   7,   0/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',   6,  -3/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',   4,  -6/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',   2,  -8/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',  -1,  -9/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',  -4,  -9/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',  -6,  -8/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',  -7,  -6/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',  -7,  -5/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',  -6,  -4/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',  -5,  -5/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',  -6,  -6/
      DATA IOPERA(  30),IX(  30),IY(  30)/'MOVE',  -3,   1/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',  -4,   3/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',  -4,   7/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',  -3,   9/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',  -1,  11/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',   1,  12/
      DATA IOPERA(  36),IX(  36),IY(  36)/'MOVE',   6,  11/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',   7,   9/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',   7,   4/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',   6,   0/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',   5,  -3/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',   3,  -6/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',   1,  -8/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',  -1,  -9/
C
      DATA IXMIND(  10)/ -10/
      DATA IXMAXD(  10)/  11/
      DATA IXDELD(  10)/  21/
      DATA ISTARD(  10)/   1/
      DATA NUMCOO(  10)/  43/
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCSN2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **************************************
C               **  STEP 2--                        **
C               **  EXTRACT THE COORDINATES         **
C               **  FOR THIS PARTICULAR CHARACTER.  **
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
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DRCSN2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ICHARN
 9013 FORMAT('ICHARN = ',I8)
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
      SUBROUTINE DRCSU1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN COMPLEX SCRIPT UPPER CASE (PART 1).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
C     DEFINE CHARACTER   2551--UPPER CASE A
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',   6,  12/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',   4,  10/
      DATA IOPERA(   3),IX(   3),IY(   3)/'DRAW',   2,   7/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',  -1,   2/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',  -3,  -1/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',  -6,  -5/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',  -9,  -8/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW', -11,  -9/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW', -13,  -9/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW', -14,  -8/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW', -14,  -6/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW', -13,  -5/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW', -12,  -6/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW', -13,  -7/
      DATA IOPERA(  15),IX(  15),IY(  15)/'MOVE',   6,  12/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',   5,   8/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',   3,  -2/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',   2,  -9/
      DATA IOPERA(  19),IX(  19),IY(  19)/'MOVE',   6,  12/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',   3,  -9/
      DATA IOPERA(  21),IX(  21),IY(  21)/'MOVE',   2,  -9/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',   2,  -7/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',   1,  -4/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',   0,  -2/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',  -2,   0/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',  -4,   1/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',  -6,   1/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',  -7,   0/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',  -7,  -2/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',  -6,  -5/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',  -3,  -8/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',   0,  -9/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',   4,  -9/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',   6,  -8/
C
      DATA IXMIND(   1)/ -13/
      DATA IXMAXD(   1)/  10/
      DATA IXDELD(   1)/  23/
      DATA ISTARD(   1)/   1/
      DATA NUMCOO(   1)/  34/
C
C     DEFINE CHARACTER   2552--UPPER CASE B
C
      DATA IOPERA(  35),IX(  35),IY(  35)/'MOVE',   3,  11/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',   2,  10/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',   1,   8/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',  -1,   3/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',  -3,  -3/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',  -4,  -5/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',  -6,  -8/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',  -8,  -9/
      DATA IOPERA(  43),IX(  43),IY(  43)/'MOVE',   2,  10/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',   1,   7/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',  -1,  -1/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW',  -2,  -4/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',  -3,  -6/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',  -5,  -8/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',  -8,  -9/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW', -10,  -9/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW', -11,  -8/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW', -11,  -6/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW', -10,  -5/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',  -9,  -6/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW', -10,  -7/
      DATA IOPERA(  56),IX(  56),IY(  56)/'MOVE',  -3,   6/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',  -4,   4/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',  -5,   3/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',  -7,   3/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW',  -8,   4/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',  -8,   6/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',  -7,   8/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',  -5,  10/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',  -3,  11/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',   0,  12/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW',   6,  12/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',   8,  11/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',   9,   9/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',   9,   7/
      DATA IOPERA(  70),IX(  70),IY(  70)/'DRAW',   8,   5/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',   6,   4/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',   2,   3/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',   0,   3/
      DATA IOPERA(  74),IX(  74),IY(  74)/'MOVE',   6,  12/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',   7,  11/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',   8,   9/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',   8,   7/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',   7,   5/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',   6,   4/
      DATA IOPERA(  80),IX(  80),IY(  80)/'MOVE',   2,   3/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',   5,   2/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',   6,   1/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',   7,  -1/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW',   7,  -4/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',   6,  -7/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',   5,  -8/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',   3,  -9/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',   1,  -9/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',   0,  -8/
      DATA IOPERA(  90),IX(  90),IY(  90)/'DRAW',   0,  -6/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',   1,  -3/
      DATA IOPERA(  92),IX(  92),IY(  92)/'MOVE',   2,   3/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',   4,   2/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',   5,   1/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',   6,  -1/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',   6,  -4/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',   5,  -7/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',   3,  -9/
C
      DATA IXMIND(   2)/ -12/
      DATA IXMAXD(   2)/  12/
      DATA IXDELD(   2)/  24/
      DATA ISTARD(   2)/  35/
      DATA NUMCOO(   2)/  64/
C
C     DEFINE CHARACTER   2553--UPPER CASE C
C
      DATA IOPERA(  99),IX(  99),IY(  99)/'MOVE',  -7,  10/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',  -8,   8/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',  -8,   6/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',  -7,   4/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',  -4,   3/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',  -1,   3/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',   3,   4/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',   5,   5/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',   7,   7/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',   8,   9/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',   8,  11/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',   7,  12/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',   5,  12/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',   2,  11/
      DATA IOPERA( 113),IX( 113),IY( 113)/'DRAW',  -1,   8/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',  -3,   5/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',  -5,   1/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',  -6,  -3/
      DATA IOPERA( 117),IX( 117),IY( 117)/'DRAW',  -6,  -6/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',  -5,  -8/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',  -2,  -9/
      DATA IOPERA( 120),IX( 120),IY( 120)/'DRAW',   0,  -9/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW',   3,  -8/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW',   5,  -6/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',   6,  -4/
      DATA IOPERA( 124),IX( 124),IY( 124)/'DRAW',   6,  -2/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',   5,   0/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW',   3,   0/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW',   1,  -1/
      DATA IOPERA( 128),IX( 128),IY( 128)/'DRAW',   0,  -3/
      DATA IOPERA( 129),IX( 129),IY( 129)/'MOVE',   5,  12/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',   3,  11/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',   0,   8/
      DATA IOPERA( 132),IX( 132),IY( 132)/'DRAW',  -2,   5/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',  -4,   1/
      DATA IOPERA( 134),IX( 134),IY( 134)/'DRAW',  -5,  -3/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',  -5,  -6/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',  -4,  -8/
      DATA IOPERA( 137),IX( 137),IY( 137)/'DRAW',  -2,  -9/
C
      DATA IXMIND(   3)/ -10/
      DATA IXMAXD(   3)/  11/
      DATA IXDELD(   3)/  21/
      DATA ISTARD(   3)/  99/
      DATA NUMCOO(   3)/  39/
C
C     DEFINE CHARACTER   2554--UPPER CASE D
C
      DATA IOPERA( 138),IX( 138),IY( 138)/'MOVE',   3,  11/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW',   2,  10/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW',   1,   8/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',  -1,   3/
      DATA IOPERA( 142),IX( 142),IY( 142)/'DRAW',  -3,  -3/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',  -4,  -5/
      DATA IOPERA( 144),IX( 144),IY( 144)/'DRAW',  -6,  -8/
      DATA IOPERA( 145),IX( 145),IY( 145)/'DRAW',  -8,  -9/
      DATA IOPERA( 146),IX( 146),IY( 146)/'MOVE',   2,  10/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',   1,   7/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',  -1,  -1/
      DATA IOPERA( 149),IX( 149),IY( 149)/'DRAW',  -2,  -4/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',  -3,  -6/
      DATA IOPERA( 151),IX( 151),IY( 151)/'DRAW',  -5,  -8/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',  -8,  -9/
      DATA IOPERA( 153),IX( 153),IY( 153)/'DRAW', -10,  -9/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW', -11,  -8/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW', -11,  -6/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW', -10,  -5/
      DATA IOPERA( 157),IX( 157),IY( 157)/'DRAW',  -8,  -5/
      DATA IOPERA( 158),IX( 158),IY( 158)/'DRAW',  -6,  -6/
      DATA IOPERA( 159),IX( 159),IY( 159)/'DRAW',  -4,  -8/
      DATA IOPERA( 160),IX( 160),IY( 160)/'DRAW',  -2,  -9/
      DATA IOPERA( 161),IX( 161),IY( 161)/'DRAW',   1,  -9/
      DATA IOPERA( 162),IX( 162),IY( 162)/'DRAW',   3,  -8/
      DATA IOPERA( 163),IX( 163),IY( 163)/'DRAW',   5,  -6/
      DATA IOPERA( 164),IX( 164),IY( 164)/'DRAW',   7,  -2/
      DATA IOPERA( 165),IX( 165),IY( 165)/'DRAW',   8,   3/
      DATA IOPERA( 166),IX( 166),IY( 166)/'DRAW',   8,   6/
      DATA IOPERA( 167),IX( 167),IY( 167)/'DRAW',   7,   9/
      DATA IOPERA( 168),IX( 168),IY( 168)/'DRAW',   5,  11/
      DATA IOPERA( 169),IX( 169),IY( 169)/'DRAW',   3,  12/
      DATA IOPERA( 170),IX( 170),IY( 170)/'DRAW',  -2,  12/
      DATA IOPERA( 171),IX( 171),IY( 171)/'DRAW',  -5,  11/
      DATA IOPERA( 172),IX( 172),IY( 172)/'DRAW',  -7,   9/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW',  -8,   7/
      DATA IOPERA( 174),IX( 174),IY( 174)/'DRAW',  -8,   5/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW',  -7,   4/
      DATA IOPERA( 176),IX( 176),IY( 176)/'DRAW',  -5,   4/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW',  -4,   5/
      DATA IOPERA( 178),IX( 178),IY( 178)/'DRAW',  -3,   7/
C
      DATA IXMIND(   4)/ -12/
      DATA IXMAXD(   4)/  11/
      DATA IXDELD(   4)/  23/
      DATA ISTARD(   4)/ 138/
      DATA NUMCOO(   4)/  41/
C
C     DEFINE CHARACTER   2555--UPPER CASE E
C
      DATA IOPERA( 179),IX( 179),IY( 179)/'MOVE',   5,   9/
      DATA IOPERA( 180),IX( 180),IY( 180)/'DRAW',   4,   8/
      DATA IOPERA( 181),IX( 181),IY( 181)/'DRAW',   4,   6/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',   5,   5/
      DATA IOPERA( 183),IX( 183),IY( 183)/'DRAW',   7,   5/
      DATA IOPERA( 184),IX( 184),IY( 184)/'DRAW',   8,   7/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',   8,   9/
      DATA IOPERA( 186),IX( 186),IY( 186)/'DRAW',   7,  11/
      DATA IOPERA( 187),IX( 187),IY( 187)/'DRAW',   5,  12/
      DATA IOPERA( 188),IX( 188),IY( 188)/'DRAW',   2,  12/
      DATA IOPERA( 189),IX( 189),IY( 189)/'DRAW',   0,  11/
      DATA IOPERA( 190),IX( 190),IY( 190)/'DRAW',  -1,  10/
      DATA IOPERA( 191),IX( 191),IY( 191)/'DRAW',  -2,   8/
      DATA IOPERA( 192),IX( 192),IY( 192)/'DRAW',  -2,   6/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW',  -1,   4/
      DATA IOPERA( 194),IX( 194),IY( 194)/'DRAW',   1,   3/
      DATA IOPERA( 195),IX( 195),IY( 195)/'MOVE',   2,  12/
      DATA IOPERA( 196),IX( 196),IY( 196)/'DRAW',   0,  10/
      DATA IOPERA( 197),IX( 197),IY( 197)/'DRAW',  -1,   8/
      DATA IOPERA( 198),IX( 198),IY( 198)/'DRAW',  -1,   5/
      DATA IOPERA( 199),IX( 199),IY( 199)/'DRAW',   1,   3/
      DATA IOPERA( 200),IX( 200),IY( 200)/'MOVE',   1,   3/
      DATA IOPERA( 201),IX( 201),IY( 201)/'DRAW',  -1,   3/
      DATA IOPERA( 202),IX( 202),IY( 202)/'DRAW',  -4,   2/
      DATA IOPERA( 203),IX( 203),IY( 203)/'DRAW',  -6,   0/
      DATA IOPERA( 204),IX( 204),IY( 204)/'DRAW',  -7,  -2/
      DATA IOPERA( 205),IX( 205),IY( 205)/'DRAW',  -7,  -5/
      DATA IOPERA( 206),IX( 206),IY( 206)/'DRAW',  -6,  -7/
      DATA IOPERA( 207),IX( 207),IY( 207)/'DRAW',  -5,  -8/
      DATA IOPERA( 208),IX( 208),IY( 208)/'DRAW',  -3,  -9/
      DATA IOPERA( 209),IX( 209),IY( 209)/'DRAW',   0,  -9/
      DATA IOPERA( 210),IX( 210),IY( 210)/'DRAW',   3,  -8/
      DATA IOPERA( 211),IX( 211),IY( 211)/'DRAW',   5,  -6/
      DATA IOPERA( 212),IX( 212),IY( 212)/'DRAW',   6,  -4/
      DATA IOPERA( 213),IX( 213),IY( 213)/'DRAW',   6,  -2/
      DATA IOPERA( 214),IX( 214),IY( 214)/'DRAW',   5,   0/
      DATA IOPERA( 215),IX( 215),IY( 215)/'DRAW',   3,   0/
      DATA IOPERA( 216),IX( 216),IY( 216)/'DRAW',   1,  -1/
      DATA IOPERA( 217),IX( 217),IY( 217)/'DRAW',   0,  -3/
      DATA IOPERA( 218),IX( 218),IY( 218)/'MOVE',  -1,   3/
      DATA IOPERA( 219),IX( 219),IY( 219)/'DRAW',  -3,   2/
      DATA IOPERA( 220),IX( 220),IY( 220)/'DRAW',  -5,   0/
      DATA IOPERA( 221),IX( 221),IY( 221)/'DRAW',  -6,  -2/
      DATA IOPERA( 222),IX( 222),IY( 222)/'DRAW',  -6,  -6/
      DATA IOPERA( 223),IX( 223),IY( 223)/'DRAW',  -5,  -8/
C
      DATA IXMIND(   5)/  -9/
      DATA IXMAXD(   5)/  10/
      DATA IXDELD(   5)/  19/
      DATA ISTARD(   5)/ 179/
      DATA NUMCOO(   5)/  45/
C
C     DEFINE CHARACTER   2556--UPPER CASE F
C
      DATA IOPERA( 224),IX( 224),IY( 224)/'MOVE',   5,  10/
      DATA IOPERA( 225),IX( 225),IY( 225)/'DRAW',   4,   8/
      DATA IOPERA( 226),IX( 226),IY( 226)/'DRAW',   2,   3/
      DATA IOPERA( 227),IX( 227),IY( 227)/'DRAW',   0,  -3/
      DATA IOPERA( 228),IX( 228),IY( 228)/'DRAW',  -1,  -5/
      DATA IOPERA( 229),IX( 229),IY( 229)/'DRAW',  -3,  -8/
      DATA IOPERA( 230),IX( 230),IY( 230)/'DRAW',  -5,  -9/
      DATA IOPERA( 231),IX( 231),IY( 231)/'MOVE',  -1,   6/
      DATA IOPERA( 232),IX( 232),IY( 232)/'DRAW',  -2,   4/
      DATA IOPERA( 233),IX( 233),IY( 233)/'DRAW',  -4,   3/
      DATA IOPERA( 234),IX( 234),IY( 234)/'DRAW',  -6,   3/
      DATA IOPERA( 235),IX( 235),IY( 235)/'DRAW',  -7,   5/
      DATA IOPERA( 236),IX( 236),IY( 236)/'DRAW',  -7,   7/
      DATA IOPERA( 237),IX( 237),IY( 237)/'DRAW',  -6,   9/
      DATA IOPERA( 238),IX( 238),IY( 238)/'DRAW',  -4,  11/
      DATA IOPERA( 239),IX( 239),IY( 239)/'DRAW',  -1,  12/
      DATA IOPERA( 240),IX( 240),IY( 240)/'DRAW',   9,  12/
      DATA IOPERA( 241),IX( 241),IY( 241)/'DRAW',   6,  11/
      DATA IOPERA( 242),IX( 242),IY( 242)/'DRAW',   5,  10/
      DATA IOPERA( 243),IX( 243),IY( 243)/'DRAW',   4,   7/
      DATA IOPERA( 244),IX( 244),IY( 244)/'DRAW',   2,  -1/
      DATA IOPERA( 245),IX( 245),IY( 245)/'DRAW',   1,  -4/
      DATA IOPERA( 246),IX( 246),IY( 246)/'DRAW',   0,  -6/
      DATA IOPERA( 247),IX( 247),IY( 247)/'DRAW',  -2,  -8/
      DATA IOPERA( 248),IX( 248),IY( 248)/'DRAW',  -5,  -9/
      DATA IOPERA( 249),IX( 249),IY( 249)/'DRAW',  -7,  -9/
      DATA IOPERA( 250),IX( 250),IY( 250)/'DRAW',  -9,  -8/
      DATA IOPERA( 251),IX( 251),IY( 251)/'DRAW', -10,  -7/
      DATA IOPERA( 252),IX( 252),IY( 252)/'DRAW', -10,  -6/
      DATA IOPERA( 253),IX( 253),IY( 253)/'DRAW',  -9,  -5/
      DATA IOPERA( 254),IX( 254),IY( 254)/'DRAW',  -8,  -6/
      DATA IOPERA( 255),IX( 255),IY( 255)/'DRAW',  -9,  -7/
      DATA IOPERA( 256),IX( 256),IY( 256)/'MOVE',   1,  12/
      DATA IOPERA( 257),IX( 257),IY( 257)/'DRAW',   5,  11/
      DATA IOPERA( 258),IX( 258),IY( 258)/'DRAW',   6,  11/
      DATA IOPERA( 259),IX( 259),IY( 259)/'MOVE',  -3,  -1/
      DATA IOPERA( 260),IX( 260),IY( 260)/'DRAW',  -2,   0/
      DATA IOPERA( 261),IX( 261),IY( 261)/'DRAW',   0,   1/
      DATA IOPERA( 262),IX( 262),IY( 262)/'DRAW',   4,   1/
      DATA IOPERA( 263),IX( 263),IY( 263)/'DRAW',   6,   2/
      DATA IOPERA( 264),IX( 264),IY( 264)/'DRAW',   8,   5/
      DATA IOPERA( 265),IX( 265),IY( 265)/'DRAW',   6,  -2/
C
      DATA IXMIND(   6)/ -11/
      DATA IXMAXD(   6)/  10/
      DATA IXDELD(   6)/  21/
      DATA ISTARD(   6)/ 224/
      DATA NUMCOO(   6)/  42/
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCSU1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **************************************
C               **  STEP 2--                        **
C               **  EXTRACT THE COORDINATES         **
C               **  FOR THIS PARTICULAR CHARACTER.  **
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
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DRCSU1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ICHARN
 9013 FORMAT('ICHARN = ',I8)
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
      SUBROUTINE DRCSU2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN COMPLEX SCRIPT UPPER CASE (PART 2).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
C     DEFINE CHARACTER   2557--UPPER CASE G
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',  -8,   9/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',  -9,   7/
      DATA IOPERA(   3),IX(   3),IY(   3)/'DRAW',  -9,   5/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',  -8,   3/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',  -6,   2/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',  -3,   2/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',   0,   3/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',   2,   4/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',   5,   7/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',   6,  10/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',   6,  11/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',   5,  12/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',   4,  12/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',   2,  11/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',   0,   9/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',  -1,   7/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',  -2,   4/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',  -2,   1/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',  -1,  -1/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',   1,  -2/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',   3,  -2/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',   5,  -1/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',   7,   1/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',   8,   3/
      DATA IOPERA(  25),IX(  25),IY(  25)/'MOVE',   5,  12/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',   3,  11/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',   1,   9/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',   0,   7/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',  -1,   4/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',  -1,   0/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',   1,  -2/
      DATA IOPERA(  32),IX(  32),IY(  32)/'MOVE',   8,   3/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',   7,  -1/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',   5,  -5/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',   3,  -7/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',   1,  -8/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',  -3,  -9/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',  -6,  -9/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',  -8,  -8/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',  -9,  -6/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',  -9,  -5/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',  -8,  -4/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',  -7,  -5/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',  -8,  -6/
      DATA IOPERA(  45),IX(  45),IY(  45)/'MOVE',   7,  -1/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW',   5,  -4/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',   3,  -6/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',   0,  -8/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',  -3,  -9/
C
      DATA IXMIND(   7)/ -11/
      DATA IXMAXD(   7)/  11/
      DATA IXDELD(   7)/  22/
      DATA ISTARD(   7)/   1/
      DATA NUMCOO(   7)/  49/
C
C     DEFINE CHARACTER   2558--UPPER CASE H
C
      DATA IOPERA(  50),IX(  50),IY(  50)/'MOVE',  -6,   6/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW',  -7,   7/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',  -7,   9/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',  -6,  11/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',  -3,  12/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',   0,  12/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',  -3,   1/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',  -5,  -5/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',  -6,  -7/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',  -7,  -8/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW',  -9,  -9/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW', -11,  -9/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW', -12,  -8/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW', -12,  -6/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW', -11,  -5/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW', -10,  -6/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW', -11,  -7/
      DATA IOPERA(  67),IX(  67),IY(  67)/'MOVE',   0,  12/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',  -3,   3/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',  -4,   0/
      DATA IOPERA(  70),IX(  70),IY(  70)/'DRAW',  -6,  -5/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',  -7,  -7/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',  -9,  -9/
      DATA IOPERA(  73),IX(  73),IY(  73)/'MOVE',  -8,  -2/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',  -7,  -1/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',  -5,   0/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',   4,   3/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',   6,   4/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',   9,   6/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',  11,   8/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',  12,  10/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',  12,  11/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',  11,  12/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',  10,  12/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW',   8,  11/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',   6,   8/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',   5,   6/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',   3,   0/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',   2,  -4/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',   2,  -7/
      DATA IOPERA(  90),IX(  90),IY(  90)/'DRAW',   4,  -9/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',   5,  -9/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW',   7,  -8/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',   9,  -6/
      DATA IOPERA(  94),IX(  94),IY(  94)/'MOVE',  10,  12/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',   8,  10/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',   6,   6/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',   4,   0/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',   3,  -4/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',   3,  -7/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',   4,  -9/
C
      DATA IXMIND(   8)/ -12/
      DATA IXMAXD(   8)/  12/
      DATA IXDELD(   8)/  24/
      DATA ISTARD(   8)/  50/
      DATA NUMCOO(   8)/  51/
C
C     DEFINE CHARACTER   2559--UPPER CASE I
C
      DATA IOPERA( 101),IX( 101),IY( 101)/'MOVE',   5,  10/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',   3,   7/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',   1,   2/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',  -1,  -3/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',  -2,  -5/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',  -4,  -8/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',  -6,  -9/
      DATA IOPERA( 108),IX( 108),IY( 108)/'MOVE',   7,   6/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',   5,   4/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',   2,   3/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',  -1,   3/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',  -3,   4/
      DATA IOPERA( 113),IX( 113),IY( 113)/'DRAW',  -4,   6/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',  -4,   8/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',  -3,  10/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',  -1,  11/
      DATA IOPERA( 117),IX( 117),IY( 117)/'DRAW',   3,  12/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',   7,  12/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',   5,  10/
      DATA IOPERA( 120),IX( 120),IY( 120)/'DRAW',   4,   8/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW',   2,   2/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW',   0,  -4/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',  -1,  -6/
      DATA IOPERA( 124),IX( 124),IY( 124)/'DRAW',  -3,  -8/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',  -6,  -9/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW',  -8,  -9/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW',  -9,  -8/
      DATA IOPERA( 128),IX( 128),IY( 128)/'DRAW',  -9,  -6/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',  -8,  -5/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',  -7,  -6/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',  -8,  -7/
C
      DATA IXMIND(   9)/  -9/
      DATA IXMAXD(   9)/   7/
      DATA IXDELD(   9)/  16/
      DATA ISTARD(   9)/ 101/
      DATA NUMCOO(   9)/  31/
C
C     DEFINE CHARACTER   2560--UPPER CASE J
C
      DATA IOPERA( 132),IX( 132),IY( 132)/'MOVE',   7,  12/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',   5,  10/
      DATA IOPERA( 134),IX( 134),IY( 134)/'DRAW',   3,   7/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',   1,   2/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',  -2,  -7/
      DATA IOPERA( 137),IX( 137),IY( 137)/'DRAW',  -4, -11/
      DATA IOPERA( 138),IX( 138),IY( 138)/'MOVE',   7,   5/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW',   5,   3/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW',   2,   2/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',  -1,   2/
      DATA IOPERA( 142),IX( 142),IY( 142)/'DRAW',  -3,   3/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',  -4,   5/
      DATA IOPERA( 144),IX( 144),IY( 144)/'DRAW',  -4,   7/
      DATA IOPERA( 145),IX( 145),IY( 145)/'DRAW',  -3,   9/
      DATA IOPERA( 146),IX( 146),IY( 146)/'DRAW',  -1,  11/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',   3,  12/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',   7,  12/
      DATA IOPERA( 149),IX( 149),IY( 149)/'DRAW',   5,   9/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',   4,   7/
      DATA IOPERA( 151),IX( 151),IY( 151)/'DRAW',   1,  -2/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',  -1,  -6/
      DATA IOPERA( 153),IX( 153),IY( 153)/'DRAW',  -2,  -8/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',  -4, -11/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW',  -5, -12/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',  -7, -13/
      DATA IOPERA( 157),IX( 157),IY( 157)/'DRAW',  -8, -12/
      DATA IOPERA( 158),IX( 158),IY( 158)/'DRAW',  -8, -10/
      DATA IOPERA( 159),IX( 159),IY( 159)/'DRAW',  -7,  -8/
      DATA IOPERA( 160),IX( 160),IY( 160)/'DRAW',  -5,  -6/
      DATA IOPERA( 161),IX( 161),IY( 161)/'DRAW',  -3,  -5/
      DATA IOPERA( 162),IX( 162),IY( 162)/'DRAW',   0,  -4/
      DATA IOPERA( 163),IX( 163),IY( 163)/'DRAW',   4,  -3/
C
      DATA IXMIND(  10)/  -9/
      DATA IXMAXD(  10)/   8/
      DATA IXDELD(  10)/  17/
      DATA ISTARD(  10)/ 132/
      DATA NUMCOO(  10)/  32/
C
C     DEFINE CHARACTER   2561--UPPER CASE K
C
      DATA IOPERA( 164),IX( 164),IY( 164)/'MOVE',  -6,   6/
      DATA IOPERA( 165),IX( 165),IY( 165)/'DRAW',  -7,   7/
      DATA IOPERA( 166),IX( 166),IY( 166)/'DRAW',  -7,   9/
      DATA IOPERA( 167),IX( 167),IY( 167)/'DRAW',  -5,  11/
      DATA IOPERA( 168),IX( 168),IY( 168)/'DRAW',  -2,  12/
      DATA IOPERA( 169),IX( 169),IY( 169)/'DRAW',   0,  12/
      DATA IOPERA( 170),IX( 170),IY( 170)/'DRAW',  -3,   1/
      DATA IOPERA( 171),IX( 171),IY( 171)/'DRAW',  -5,  -5/
      DATA IOPERA( 172),IX( 172),IY( 172)/'DRAW',  -6,  -7/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW',  -7,  -8/
      DATA IOPERA( 174),IX( 174),IY( 174)/'DRAW',  -9,  -9/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW', -11,  -9/
      DATA IOPERA( 176),IX( 176),IY( 176)/'DRAW', -12,  -8/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW', -12,  -6/
      DATA IOPERA( 178),IX( 178),IY( 178)/'DRAW', -11,  -5/
      DATA IOPERA( 179),IX( 179),IY( 179)/'DRAW', -10,  -6/
      DATA IOPERA( 180),IX( 180),IY( 180)/'DRAW', -11,  -7/
      DATA IOPERA( 181),IX( 181),IY( 181)/'MOVE',   0,  12/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',  -3,   3/
      DATA IOPERA( 183),IX( 183),IY( 183)/'DRAW',  -4,   0/
      DATA IOPERA( 184),IX( 184),IY( 184)/'DRAW',  -6,  -5/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',  -7,  -7/
      DATA IOPERA( 186),IX( 186),IY( 186)/'DRAW',  -9,  -9/
      DATA IOPERA( 187),IX( 187),IY( 187)/'MOVE',   8,  11/
      DATA IOPERA( 188),IX( 188),IY( 188)/'DRAW',   5,   7/
      DATA IOPERA( 189),IX( 189),IY( 189)/'DRAW',   3,   5/
      DATA IOPERA( 190),IX( 190),IY( 190)/'DRAW',   1,   4/
      DATA IOPERA( 191),IX( 191),IY( 191)/'DRAW',  -2,   3/
      DATA IOPERA( 192),IX( 192),IY( 192)/'MOVE',  11,  11/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW',  10,  10/
      DATA IOPERA( 194),IX( 194),IY( 194)/'DRAW',  11,   9/
      DATA IOPERA( 195),IX( 195),IY( 195)/'DRAW',  12,  10/
      DATA IOPERA( 196),IX( 196),IY( 196)/'DRAW',  12,  11/
      DATA IOPERA( 197),IX( 197),IY( 197)/'DRAW',  11,  12/
      DATA IOPERA( 198),IX( 198),IY( 198)/'DRAW',  10,  12/
      DATA IOPERA( 199),IX( 199),IY( 199)/'DRAW',   8,  11/
      DATA IOPERA( 200),IX( 200),IY( 200)/'DRAW',   5,   6/
      DATA IOPERA( 201),IX( 201),IY( 201)/'DRAW',   4,   5/
      DATA IOPERA( 202),IX( 202),IY( 202)/'DRAW',   2,   4/
      DATA IOPERA( 203),IX( 203),IY( 203)/'DRAW',  -2,   3/
      DATA IOPERA( 204),IX( 204),IY( 204)/'MOVE',  -2,   3/
      DATA IOPERA( 205),IX( 205),IY( 205)/'DRAW',   1,   2/
      DATA IOPERA( 206),IX( 206),IY( 206)/'DRAW',   2,   0/
      DATA IOPERA( 207),IX( 207),IY( 207)/'DRAW',   3,  -7/
      DATA IOPERA( 208),IX( 208),IY( 208)/'DRAW',   4,  -9/
      DATA IOPERA( 209),IX( 209),IY( 209)/'MOVE',  -2,   3/
      DATA IOPERA( 210),IX( 210),IY( 210)/'DRAW',   0,   2/
      DATA IOPERA( 211),IX( 211),IY( 211)/'DRAW',   1,   0/
      DATA IOPERA( 212),IX( 212),IY( 212)/'DRAW',   2,  -7/
      DATA IOPERA( 213),IX( 213),IY( 213)/'DRAW',   4,  -9/
      DATA IOPERA( 214),IX( 214),IY( 214)/'DRAW',   5,  -9/
      DATA IOPERA( 215),IX( 215),IY( 215)/'DRAW',   7,  -8/
      DATA IOPERA( 216),IX( 216),IY( 216)/'DRAW',   9,  -6/
C
      DATA IXMIND(  11)/ -12/
      DATA IXMAXD(  11)/  12/
      DATA IXDELD(  11)/  24/
      DATA ISTARD(  11)/ 164/
      DATA NUMCOO(  11)/  53/
C
C     DEFINE CHARACTER   2562--UPPER CASE L
C
      DATA IOPERA( 217),IX( 217),IY( 217)/'MOVE',  -5,   9/
      DATA IOPERA( 218),IX( 218),IY( 218)/'DRAW',  -6,   7/
      DATA IOPERA( 219),IX( 219),IY( 219)/'DRAW',  -6,   5/
      DATA IOPERA( 220),IX( 220),IY( 220)/'DRAW',  -5,   3/
      DATA IOPERA( 221),IX( 221),IY( 221)/'DRAW',  -3,   2/
      DATA IOPERA( 222),IX( 222),IY( 222)/'DRAW',   0,   2/
      DATA IOPERA( 223),IX( 223),IY( 223)/'DRAW',   3,   3/
      DATA IOPERA( 224),IX( 224),IY( 224)/'DRAW',   5,   4/
      DATA IOPERA( 225),IX( 225),IY( 225)/'DRAW',   8,   7/
      DATA IOPERA( 226),IX( 226),IY( 226)/'DRAW',   9,  10/
      DATA IOPERA( 227),IX( 227),IY( 227)/'DRAW',   9,  11/
      DATA IOPERA( 228),IX( 228),IY( 228)/'DRAW',   8,  12/
      DATA IOPERA( 229),IX( 229),IY( 229)/'DRAW',   7,  12/
      DATA IOPERA( 230),IX( 230),IY( 230)/'DRAW',   5,  11/
      DATA IOPERA( 231),IX( 231),IY( 231)/'DRAW',   4,  10/
      DATA IOPERA( 232),IX( 232),IY( 232)/'DRAW',   2,   7/
      DATA IOPERA( 233),IX( 233),IY( 233)/'DRAW',  -2,  -3/
      DATA IOPERA( 234),IX( 234),IY( 234)/'DRAW',  -3,  -5/
      DATA IOPERA( 235),IX( 235),IY( 235)/'DRAW',  -5,  -8/
      DATA IOPERA( 236),IX( 236),IY( 236)/'DRAW',  -7,  -9/
      DATA IOPERA( 237),IX( 237),IY( 237)/'MOVE',   4,  10/
      DATA IOPERA( 238),IX( 238),IY( 238)/'DRAW',   2,   6/
      DATA IOPERA( 239),IX( 239),IY( 239)/'DRAW',   0,  -1/
      DATA IOPERA( 240),IX( 240),IY( 240)/'DRAW',  -1,  -4/
      DATA IOPERA( 241),IX( 241),IY( 241)/'DRAW',  -2,  -6/
      DATA IOPERA( 242),IX( 242),IY( 242)/'DRAW',  -4,  -8/
      DATA IOPERA( 243),IX( 243),IY( 243)/'DRAW',  -7,  -9/
      DATA IOPERA( 244),IX( 244),IY( 244)/'DRAW',  -9,  -9/
      DATA IOPERA( 245),IX( 245),IY( 245)/'DRAW', -10,  -8/
      DATA IOPERA( 246),IX( 246),IY( 246)/'DRAW', -10,  -6/
      DATA IOPERA( 247),IX( 247),IY( 247)/'DRAW',  -9,  -5/
      DATA IOPERA( 248),IX( 248),IY( 248)/'DRAW',  -7,  -5/
      DATA IOPERA( 249),IX( 249),IY( 249)/'DRAW',  -5,  -6/
      DATA IOPERA( 250),IX( 250),IY( 250)/'DRAW',  -2,  -8/
      DATA IOPERA( 251),IX( 251),IY( 251)/'DRAW',   0,  -9/
      DATA IOPERA( 252),IX( 252),IY( 252)/'DRAW',   3,  -9/
      DATA IOPERA( 253),IX( 253),IY( 253)/'DRAW',   5,  -8/
      DATA IOPERA( 254),IX( 254),IY( 254)/'DRAW',   7,  -6/
C
      DATA IXMIND(  12)/  -9/
      DATA IXMAXD(  12)/   9/
      DATA IXDELD(  12)/  18/
      DATA ISTARD(  12)/ 217/
      DATA NUMCOO(  12)/  38/
C
C     DEFINE CHARACTER   2563--UPPER CASE M
C
      DATA IOPERA( 255),IX( 255),IY( 255)/'MOVE',   0,  12/
      DATA IOPERA( 256),IX( 256),IY( 256)/'DRAW',  -4,   3/
      DATA IOPERA( 257),IX( 257),IY( 257)/'DRAW',  -7,  -3/
      DATA IOPERA( 258),IX( 258),IY( 258)/'DRAW',  -9,  -6/
      DATA IOPERA( 259),IX( 259),IY( 259)/'DRAW', -11,  -8/
      DATA IOPERA( 260),IX( 260),IY( 260)/'DRAW', -13,  -9/
      DATA IOPERA( 261),IX( 261),IY( 261)/'DRAW', -15,  -9/
      DATA IOPERA( 262),IX( 262),IY( 262)/'DRAW', -16,  -8/
      DATA IOPERA( 263),IX( 263),IY( 263)/'DRAW', -16,  -6/
      DATA IOPERA( 264),IX( 264),IY( 264)/'DRAW', -15,  -5/
      DATA IOPERA( 265),IX( 265),IY( 265)/'DRAW', -14,  -6/
      DATA IOPERA( 266),IX( 266),IY( 266)/'DRAW', -15,  -7/
      DATA IOPERA( 267),IX( 267),IY( 267)/'MOVE',   0,  12/
      DATA IOPERA( 268),IX( 268),IY( 268)/'DRAW',  -2,   5/
      DATA IOPERA( 269),IX( 269),IY( 269)/'DRAW',  -3,   1/
      DATA IOPERA( 270),IX( 270),IY( 270)/'DRAW',  -4,  -4/
      DATA IOPERA( 271),IX( 271),IY( 271)/'DRAW',  -4,  -8/
      DATA IOPERA( 272),IX( 272),IY( 272)/'DRAW',  -2,  -9/
      DATA IOPERA( 273),IX( 273),IY( 273)/'MOVE',   0,  12/
      DATA IOPERA( 274),IX( 274),IY( 274)/'DRAW',  -1,   8/
      DATA IOPERA( 275),IX( 275),IY( 275)/'DRAW',  -2,   3/
      DATA IOPERA( 276),IX( 276),IY( 276)/'DRAW',  -3,  -4/
      DATA IOPERA( 277),IX( 277),IY( 277)/'DRAW',  -3,  -8/
      DATA IOPERA( 278),IX( 278),IY( 278)/'DRAW',  -2,  -9/
      DATA IOPERA( 279),IX( 279),IY( 279)/'MOVE',   9,  12/
      DATA IOPERA( 280),IX( 280),IY( 280)/'DRAW',   5,   3/
      DATA IOPERA( 281),IX( 281),IY( 281)/'DRAW',   0,  -6/
      DATA IOPERA( 282),IX( 282),IY( 282)/'DRAW',  -2,  -9/
      DATA IOPERA( 283),IX( 283),IY( 283)/'MOVE',   9,  12/
      DATA IOPERA( 284),IX( 284),IY( 284)/'DRAW',   7,   5/
      DATA IOPERA( 285),IX( 285),IY( 285)/'DRAW',   6,   1/
      DATA IOPERA( 286),IX( 286),IY( 286)/'DRAW',   5,  -4/
      DATA IOPERA( 287),IX( 287),IY( 287)/'DRAW',   5,  -8/
      DATA IOPERA( 288),IX( 288),IY( 288)/'DRAW',   7,  -9/
      DATA IOPERA( 289),IX( 289),IY( 289)/'DRAW',   8,  -9/
      DATA IOPERA( 290),IX( 290),IY( 290)/'DRAW',  10,  -8/
      DATA IOPERA( 291),IX( 291),IY( 291)/'DRAW',  12,  -6/
      DATA IOPERA( 292),IX( 292),IY( 292)/'MOVE',   9,  12/
      DATA IOPERA( 293),IX( 293),IY( 293)/'DRAW',   8,   8/
      DATA IOPERA( 294),IX( 294),IY( 294)/'DRAW',   7,   3/
      DATA IOPERA( 295),IX( 295),IY( 295)/'DRAW',   6,  -4/
      DATA IOPERA( 296),IX( 296),IY( 296)/'DRAW',   6,  -8/
      DATA IOPERA( 297),IX( 297),IY( 297)/'DRAW',   7,  -9/
C
      DATA IXMIND(  13)/ -14/
      DATA IXMAXD(  13)/  14/
      DATA IXDELD(  13)/  28/
      DATA ISTARD(  13)/ 255/
      DATA NUMCOO(  13)/  43/
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCSU2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **************************************
C               **  STEP 2--                        **
C               **  EXTRACT THE COORDINATES         **
C               **  FOR THIS PARTICULAR CHARACTER.  **
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
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DRCSU2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ICHARN
 9013 FORMAT('ICHARN = ',I8)
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
      SUBROUTINE DRCSU3(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN COMPLEX SCRIPT UPPER CASE (PART 3).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
C     DEFINE CHARACTER   2564--UPPER CASE N
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',   0,  12/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',  -1,   8/
      DATA IOPERA(   3),IX(   3),IY(   3)/'DRAW',  -3,   2/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',  -5,  -3/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',  -6,  -5/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',  -8,  -8/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW', -10,  -9/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW', -12,  -9/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW', -13,  -8/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW', -13,  -6/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW', -12,  -5/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW', -11,  -6/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW', -12,  -7/
      DATA IOPERA(  14),IX(  14),IY(  14)/'MOVE',   0,  12/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',   0,   7/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',   1,  -4/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',   2,  -9/
      DATA IOPERA(  18),IX(  18),IY(  18)/'MOVE',   0,  12/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',   1,   7/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',   2,  -4/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',   2,  -9/
      DATA IOPERA(  22),IX(  22),IY(  22)/'MOVE',  14,  11/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',  13,  10/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',  14,   9/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',  15,  10/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',  15,  11/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',  14,  12/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',  12,  12/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',  10,  11/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',   8,   8/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',   7,   6/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',   5,   1/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',   3,  -5/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',   2,  -9/
C
      DATA IXMIND(  14)/ -11/
      DATA IXMAXD(  14)/  12/
      DATA IXDELD(  14)/  23/
      DATA ISTARD(  14)/   1/
      DATA NUMCOO(  14)/  34/
C
C     DEFINE CHARACTER   2565--UPPER CASE O
C
      DATA IOPERA(  35),IX(  35),IY(  35)/'MOVE',   1,  12/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',  -1,  11/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',  -3,   9/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',  -5,   6/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',  -6,   4/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',  -7,   0/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',  -7,  -4/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',  -6,  -7/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',  -5,  -8/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',  -3,  -9/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',  -1,  -9/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW',   2,  -8/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',   4,  -6/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',   6,  -3/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',   7,  -1/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW',   8,   3/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW',   8,   7/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',   7,  10/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',   6,  11/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',   5,  11/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',   3,  10/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',   1,   8/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',  -1,   4/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',  -2,  -1/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',  -2,  -4/
      DATA IOPERA(  60),IX(  60),IY(  60)/'MOVE',  -1,  11/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',  -3,   8/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',  -5,   4/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',  -6,   0/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',  -6,  -4/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',  -5,  -7/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW',  -3,  -9/
C
      DATA IXMIND(  15)/ -10/
      DATA IXMAXD(  15)/  11/
      DATA IXDELD(  15)/  21/
      DATA ISTARD(  15)/  35/
      DATA NUMCOO(  15)/  32/
C
C     DEFINE CHARACTER   2566--UPPER CASE P
C
      DATA IOPERA(  67),IX(  67),IY(  67)/'MOVE',   3,  11/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',   2,  10/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',   1,   8/
      DATA IOPERA(  70),IX(  70),IY(  70)/'DRAW',  -1,   3/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',  -3,  -3/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',  -4,  -5/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',  -6,  -8/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',  -8,  -9/
      DATA IOPERA(  75),IX(  75),IY(  75)/'MOVE',   2,  10/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',   1,   7/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',  -1,  -1/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',  -2,  -4/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',  -3,  -6/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',  -5,  -8/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',  -8,  -9/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW', -10,  -9/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW', -11,  -8/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW', -11,  -6/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW', -10,  -5/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',  -9,  -6/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW', -10,  -7/
      DATA IOPERA(  88),IX(  88),IY(  88)/'MOVE',  -3,   6/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',  -4,   4/
      DATA IOPERA(  90),IX(  90),IY(  90)/'DRAW',  -5,   3/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',  -7,   3/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW',  -8,   4/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',  -8,   6/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',  -7,   8/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',  -5,  10/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',  -3,  11/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',   0,  12/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',   4,  12/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',   7,  11/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',   8,  10/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',   9,   8/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',   9,   5/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',   8,   3/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',   7,   2/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',   4,   1/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',   2,   1/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',   0,   2/
      DATA IOPERA( 108),IX( 108),IY( 108)/'MOVE',   4,  12/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',   6,  11/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',   7,  10/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',   8,   8/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',   8,   5/
      DATA IOPERA( 113),IX( 113),IY( 113)/'DRAW',   7,   3/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',   6,   2/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',   4,   1/
C
      DATA IXMIND(  16)/ -12/
      DATA IXMAXD(  16)/  11/
      DATA IXDELD(  16)/  23/
      DATA ISTARD(  16)/  67/
      DATA NUMCOO(  16)/  49/
C
C     DEFINE CHARACTER   2567--UPPER CASE Q
C
      DATA IOPERA( 116),IX( 116),IY( 116)/'MOVE',   3,   8/
      DATA IOPERA( 117),IX( 117),IY( 117)/'DRAW',   3,   6/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',   2,   4/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',   1,   3/
      DATA IOPERA( 120),IX( 120),IY( 120)/'DRAW',  -1,   2/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW',  -3,   2/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW',  -4,   4/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',  -4,   6/
      DATA IOPERA( 124),IX( 124),IY( 124)/'DRAW',  -3,   9/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',  -1,  11/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW',   2,  12/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW',   5,  12/
      DATA IOPERA( 128),IX( 128),IY( 128)/'DRAW',   7,  11/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',   8,   9/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',   8,   5/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',   7,   2/
      DATA IOPERA( 132),IX( 132),IY( 132)/'DRAW',   5,  -1/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',   1,  -5/
      DATA IOPERA( 134),IX( 134),IY( 134)/'DRAW',  -2,  -7/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',  -4,  -8/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',  -7,  -9/
      DATA IOPERA( 137),IX( 137),IY( 137)/'DRAW',  -9,  -9/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW', -10,  -8/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW', -10,  -6/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW',  -9,  -5/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',  -7,  -5/
      DATA IOPERA( 142),IX( 142),IY( 142)/'DRAW',  -5,  -6/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',  -2,  -8/
      DATA IOPERA( 144),IX( 144),IY( 144)/'DRAW',   1,  -9/
      DATA IOPERA( 145),IX( 145),IY( 145)/'DRAW',   4,  -9/
      DATA IOPERA( 146),IX( 146),IY( 146)/'DRAW',   6,  -8/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',   8,  -6/
      DATA IOPERA( 148),IX( 148),IY( 148)/'MOVE',   5,  12/
      DATA IOPERA( 149),IX( 149),IY( 149)/'DRAW',   6,  11/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',   7,   9/
      DATA IOPERA( 151),IX( 151),IY( 151)/'DRAW',   7,   5/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',   6,   2/
      DATA IOPERA( 153),IX( 153),IY( 153)/'DRAW',   4,  -1/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',   1,  -4/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW',  -3,  -7/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',  -7,  -9/
C
      DATA IXMIND(  17)/ -10/
      DATA IXMAXD(  17)/  11/
      DATA IXDELD(  17)/  21/
      DATA ISTARD(  17)/ 116/
      DATA NUMCOO(  17)/  41/
C
C     DEFINE CHARACTER   2568--UPPER CASE R
C
      DATA IOPERA( 157),IX( 157),IY( 157)/'MOVE',   3,  11/
      DATA IOPERA( 158),IX( 158),IY( 158)/'DRAW',   2,  10/
      DATA IOPERA( 159),IX( 159),IY( 159)/'DRAW',   1,   8/
      DATA IOPERA( 160),IX( 160),IY( 160)/'DRAW',  -1,   3/
      DATA IOPERA( 161),IX( 161),IY( 161)/'DRAW',  -3,  -3/
      DATA IOPERA( 162),IX( 162),IY( 162)/'DRAW',  -4,  -5/
      DATA IOPERA( 163),IX( 163),IY( 163)/'DRAW',  -6,  -8/
      DATA IOPERA( 164),IX( 164),IY( 164)/'DRAW',  -8,  -9/
      DATA IOPERA( 165),IX( 165),IY( 165)/'MOVE',   2,  10/
      DATA IOPERA( 166),IX( 166),IY( 166)/'DRAW',   1,   7/
      DATA IOPERA( 167),IX( 167),IY( 167)/'DRAW',  -1,  -1/
      DATA IOPERA( 168),IX( 168),IY( 168)/'DRAW',  -2,  -4/
      DATA IOPERA( 169),IX( 169),IY( 169)/'DRAW',  -3,  -6/
      DATA IOPERA( 170),IX( 170),IY( 170)/'DRAW',  -5,  -8/
      DATA IOPERA( 171),IX( 171),IY( 171)/'DRAW',  -8,  -9/
      DATA IOPERA( 172),IX( 172),IY( 172)/'DRAW', -10,  -9/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW', -11,  -8/
      DATA IOPERA( 174),IX( 174),IY( 174)/'DRAW', -11,  -6/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW', -10,  -5/
      DATA IOPERA( 176),IX( 176),IY( 176)/'DRAW',  -9,  -6/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW', -10,  -7/
      DATA IOPERA( 178),IX( 178),IY( 178)/'MOVE',  -3,   6/
      DATA IOPERA( 179),IX( 179),IY( 179)/'DRAW',  -4,   4/
      DATA IOPERA( 180),IX( 180),IY( 180)/'DRAW',  -5,   3/
      DATA IOPERA( 181),IX( 181),IY( 181)/'DRAW',  -7,   3/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',  -8,   4/
      DATA IOPERA( 183),IX( 183),IY( 183)/'DRAW',  -8,   6/
      DATA IOPERA( 184),IX( 184),IY( 184)/'DRAW',  -7,   8/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',  -5,  10/
      DATA IOPERA( 186),IX( 186),IY( 186)/'DRAW',  -3,  11/
      DATA IOPERA( 187),IX( 187),IY( 187)/'DRAW',   0,  12/
      DATA IOPERA( 188),IX( 188),IY( 188)/'DRAW',   5,  12/
      DATA IOPERA( 189),IX( 189),IY( 189)/'DRAW',   8,  11/
      DATA IOPERA( 190),IX( 190),IY( 190)/'DRAW',   9,   9/
      DATA IOPERA( 191),IX( 191),IY( 191)/'DRAW',   9,   7/
      DATA IOPERA( 192),IX( 192),IY( 192)/'DRAW',   8,   5/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW',   7,   4/
      DATA IOPERA( 194),IX( 194),IY( 194)/'DRAW',   4,   3/
      DATA IOPERA( 195),IX( 195),IY( 195)/'DRAW',   0,   3/
      DATA IOPERA( 196),IX( 196),IY( 196)/'MOVE',   5,  12/
      DATA IOPERA( 197),IX( 197),IY( 197)/'DRAW',   7,  11/
      DATA IOPERA( 198),IX( 198),IY( 198)/'DRAW',   8,   9/
      DATA IOPERA( 199),IX( 199),IY( 199)/'DRAW',   8,   7/
      DATA IOPERA( 200),IX( 200),IY( 200)/'DRAW',   7,   5/
      DATA IOPERA( 201),IX( 201),IY( 201)/'DRAW',   6,   4/
      DATA IOPERA( 202),IX( 202),IY( 202)/'DRAW',   4,   3/
      DATA IOPERA( 203),IX( 203),IY( 203)/'MOVE',   0,   3/
      DATA IOPERA( 204),IX( 204),IY( 204)/'DRAW',   3,   2/
      DATA IOPERA( 205),IX( 205),IY( 205)/'DRAW',   4,   0/
      DATA IOPERA( 206),IX( 206),IY( 206)/'DRAW',   5,  -7/
      DATA IOPERA( 207),IX( 207),IY( 207)/'DRAW',   6,  -9/
      DATA IOPERA( 208),IX( 208),IY( 208)/'MOVE',   0,   3/
      DATA IOPERA( 209),IX( 209),IY( 209)/'DRAW',   2,   2/
      DATA IOPERA( 210),IX( 210),IY( 210)/'DRAW',   3,   0/
      DATA IOPERA( 211),IX( 211),IY( 211)/'DRAW',   4,  -7/
      DATA IOPERA( 212),IX( 212),IY( 212)/'DRAW',   6,  -9/
      DATA IOPERA( 213),IX( 213),IY( 213)/'DRAW',   7,  -9/
      DATA IOPERA( 214),IX( 214),IY( 214)/'DRAW',   9,  -8/
      DATA IOPERA( 215),IX( 215),IY( 215)/'DRAW',  11,  -6/
C
      DATA IXMIND(  18)/ -12/
      DATA IXMAXD(  18)/  12/
      DATA IXDELD(  18)/  24/
      DATA ISTARD(  18)/ 157/
      DATA NUMCOO(  18)/  59/
C
C     DEFINE CHARACTER   2569--UPPER CASE S
C
      DATA IOPERA( 216),IX( 216),IY( 216)/'MOVE',  -4,   9/
      DATA IOPERA( 217),IX( 217),IY( 217)/'DRAW',  -5,   7/
      DATA IOPERA( 218),IX( 218),IY( 218)/'DRAW',  -5,   5/
      DATA IOPERA( 219),IX( 219),IY( 219)/'DRAW',  -4,   3/
      DATA IOPERA( 220),IX( 220),IY( 220)/'DRAW',  -2,   2/
      DATA IOPERA( 221),IX( 221),IY( 221)/'DRAW',   1,   2/
      DATA IOPERA( 222),IX( 222),IY( 222)/'DRAW',   4,   3/
      DATA IOPERA( 223),IX( 223),IY( 223)/'DRAW',   6,   4/
      DATA IOPERA( 224),IX( 224),IY( 224)/'DRAW',   9,   7/
      DATA IOPERA( 225),IX( 225),IY( 225)/'DRAW',  10,  10/
      DATA IOPERA( 226),IX( 226),IY( 226)/'DRAW',  10,  11/
      DATA IOPERA( 227),IX( 227),IY( 227)/'DRAW',   9,  12/
      DATA IOPERA( 228),IX( 228),IY( 228)/'DRAW',   8,  12/
      DATA IOPERA( 229),IX( 229),IY( 229)/'DRAW',   6,  11/
      DATA IOPERA( 230),IX( 230),IY( 230)/'DRAW',   5,  10/
      DATA IOPERA( 231),IX( 231),IY( 231)/'DRAW',   4,   8/
      DATA IOPERA( 232),IX( 232),IY( 232)/'DRAW',   3,   5/
      DATA IOPERA( 233),IX( 233),IY( 233)/'DRAW',   1,  -2/
      DATA IOPERA( 234),IX( 234),IY( 234)/'DRAW',   0,  -5/
      DATA IOPERA( 235),IX( 235),IY( 235)/'DRAW',  -2,  -8/
      DATA IOPERA( 236),IX( 236),IY( 236)/'DRAW',  -4,  -9/
      DATA IOPERA( 237),IX( 237),IY( 237)/'MOVE',   4,   8/
      DATA IOPERA( 238),IX( 238),IY( 238)/'DRAW',   3,   4/
      DATA IOPERA( 239),IX( 239),IY( 239)/'DRAW',   2,  -3/
      DATA IOPERA( 240),IX( 240),IY( 240)/'DRAW',   1,  -6/
      DATA IOPERA( 241),IX( 241),IY( 241)/'DRAW',  -1,  -8/
      DATA IOPERA( 242),IX( 242),IY( 242)/'DRAW',  -4,  -9/
      DATA IOPERA( 243),IX( 243),IY( 243)/'DRAW',  -7,  -9/
      DATA IOPERA( 244),IX( 244),IY( 244)/'DRAW',  -9,  -8/
      DATA IOPERA( 245),IX( 245),IY( 245)/'DRAW', -10,  -6/
      DATA IOPERA( 246),IX( 246),IY( 246)/'DRAW', -10,  -5/
      DATA IOPERA( 247),IX( 247),IY( 247)/'DRAW',  -9,  -4/
      DATA IOPERA( 248),IX( 248),IY( 248)/'DRAW',  -8,  -5/
      DATA IOPERA( 249),IX( 249),IY( 249)/'DRAW',  -9,  -6/
C
      DATA IXMIND(  19)/ -10/
      DATA IXMAXD(  19)/  10/
      DATA IXDELD(  19)/  20/
      DATA ISTARD(  19)/ 216/
      DATA NUMCOO(  19)/  34/
C
C     DEFINE CHARACTER   2570--UPPER CASE T
C
      DATA IOPERA( 250),IX( 250),IY( 250)/'MOVE',   7,  10/
      DATA IOPERA( 251),IX( 251),IY( 251)/'DRAW',   6,   8/
      DATA IOPERA( 252),IX( 252),IY( 252)/'DRAW',   4,   3/
      DATA IOPERA( 253),IX( 253),IY( 253)/'DRAW',   2,  -3/
      DATA IOPERA( 254),IX( 254),IY( 254)/'DRAW',   1,  -5/
      DATA IOPERA( 255),IX( 255),IY( 255)/'DRAW',  -1,  -8/
      DATA IOPERA( 256),IX( 256),IY( 256)/'DRAW',  -3,  -9/
      DATA IOPERA( 257),IX( 257),IY( 257)/'MOVE',   1,   6/
      DATA IOPERA( 258),IX( 258),IY( 258)/'DRAW',   0,   4/
      DATA IOPERA( 259),IX( 259),IY( 259)/'DRAW',  -2,   3/
      DATA IOPERA( 260),IX( 260),IY( 260)/'DRAW',  -4,   3/
      DATA IOPERA( 261),IX( 261),IY( 261)/'DRAW',  -5,   5/
      DATA IOPERA( 262),IX( 262),IY( 262)/'DRAW',  -5,   7/
      DATA IOPERA( 263),IX( 263),IY( 263)/'DRAW',  -4,   9/
      DATA IOPERA( 264),IX( 264),IY( 264)/'DRAW',  -2,  11/
      DATA IOPERA( 265),IX( 265),IY( 265)/'DRAW',   1,  12/
      DATA IOPERA( 266),IX( 266),IY( 266)/'DRAW',  10,  12/
      DATA IOPERA( 267),IX( 267),IY( 267)/'DRAW',   8,  11/
      DATA IOPERA( 268),IX( 268),IY( 268)/'DRAW',   7,  10/
      DATA IOPERA( 269),IX( 269),IY( 269)/'DRAW',   6,   7/
      DATA IOPERA( 270),IX( 270),IY( 270)/'DRAW',   4,  -1/
      DATA IOPERA( 271),IX( 271),IY( 271)/'DRAW',   3,  -4/
      DATA IOPERA( 272),IX( 272),IY( 272)/'DRAW',   2,  -6/
      DATA IOPERA( 273),IX( 273),IY( 273)/'DRAW',   0,  -8/
      DATA IOPERA( 274),IX( 274),IY( 274)/'DRAW',  -3,  -9/
      DATA IOPERA( 275),IX( 275),IY( 275)/'DRAW',  -5,  -9/
      DATA IOPERA( 276),IX( 276),IY( 276)/'DRAW',  -7,  -8/
      DATA IOPERA( 277),IX( 277),IY( 277)/'DRAW',  -8,  -7/
      DATA IOPERA( 278),IX( 278),IY( 278)/'DRAW',  -8,  -6/
      DATA IOPERA( 279),IX( 279),IY( 279)/'DRAW',  -7,  -5/
      DATA IOPERA( 280),IX( 280),IY( 280)/'DRAW',  -6,  -6/
      DATA IOPERA( 281),IX( 281),IY( 281)/'DRAW',  -7,  -7/
      DATA IOPERA( 282),IX( 282),IY( 282)/'MOVE',   3,  12/
      DATA IOPERA( 283),IX( 283),IY( 283)/'DRAW',   7,  11/
      DATA IOPERA( 284),IX( 284),IY( 284)/'DRAW',   8,  11/
C
      DATA IXMIND(  20)/  -9/
      DATA IXMAXD(  20)/   9/
      DATA IXDELD(  20)/  18/
      DATA ISTARD(  20)/ 250/
      DATA NUMCOO(  20)/  35/
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCSU3--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **************************************
C               **  STEP 2--                        **
C               **  EXTRACT THE COORDINATES         **
C               **  FOR THIS PARTICULAR CHARACTER.  **
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
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DRCSU3--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ICHARN
 9013 FORMAT('ICHARN = ',I8)
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
      SUBROUTINE DRCSU4(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN COMPLEX SCRIPT UPPER CASE (PART 4).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
C     DEFINE CHARACTER   2571--UPPER CASE U
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE', -10,   8/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',  -8,  11/
      DATA IOPERA(   3),IX(   3),IY(   3)/'DRAW',  -6,  12/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',  -5,  12/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',  -3,  10/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',  -3,   7/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',  -4,   4/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',  -7,  -4/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',  -7,  -7/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',  -6,  -9/
      DATA IOPERA(  11),IX(  11),IY(  11)/'MOVE',  -5,  12/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',  -4,  10/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',  -4,   7/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',  -7,  -1/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',  -8,  -4/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',  -8,  -7/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',  -6,  -9/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',  -4,  -9/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',  -2,  -8/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',   1,  -5/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',   3,  -2/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',   4,   0/
      DATA IOPERA(  23),IX(  23),IY(  23)/'MOVE',   8,  12/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',   4,   0/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',   3,  -4/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',   3,  -7/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',   5,  -9/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',   6,  -9/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',   8,  -8/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',  10,  -6/
      DATA IOPERA(  31),IX(  31),IY(  31)/'MOVE',   9,  12/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',   5,   0/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',   4,  -4/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',   4,  -7/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',   5,  -9/
C
      DATA IXMIND(  21)/ -11/
      DATA IXMAXD(  21)/  11/
      DATA IXDELD(  21)/  22/
      DATA ISTARD(  21)/   1/
      DATA NUMCOO(  21)/  35/
C
C     DEFINE CHARACTER   2572--UPPER CASE V
C
      DATA IOPERA(  36),IX(  36),IY(  36)/'MOVE', -10,   8/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',  -8,  11/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',  -6,  12/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',  -5,  12/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',  -3,  10/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',  -3,   7/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',  -4,   3/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',  -6,  -4/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',  -6,  -7/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',  -5,  -9/
      DATA IOPERA(  46),IX(  46),IY(  46)/'MOVE',  -5,  12/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',  -4,  10/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',  -4,   7/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',  -6,   0/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW',  -7,  -4/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW',  -7,  -7/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',  -5,  -9/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',  -4,  -9/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',  -1,  -8/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',   2,  -5/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',   4,  -2/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',   6,   2/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',   7,   5/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',   8,   9/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW',   8,  11/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',   7,  12/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',   6,  12/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',   5,  11/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',   4,   9/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',   4,   6/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW',   5,   4/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',   7,   2/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',   9,   1/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',  11,   1/
C
      DATA IXMIND(  22)/ -11/
      DATA IXMAXD(  22)/  10/
      DATA IXDELD(  22)/  21/
      DATA ISTARD(  22)/  36/
      DATA NUMCOO(  22)/  34/
C
C     DEFINE CHARACTER   2573--UPPER CASE W
C
      DATA IOPERA(  70),IX(  70),IY(  70)/'MOVE',  -9,   6/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW', -10,   6/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW', -11,   7/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW', -11,   9/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW', -10,  11/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',  -8,  12/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',  -4,  12/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',  -5,  10/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',  -6,   6/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',  -7,  -3/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',  -8,  -9/
      DATA IOPERA(  81),IX(  81),IY(  81)/'MOVE',  -6,   6/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',  -6,  -3/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',  -7,  -9/
      DATA IOPERA(  84),IX(  84),IY(  84)/'MOVE',   4,  12/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',   2,  10/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',   0,   6/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',  -3,  -3/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',  -5,  -7/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',  -7,  -9/
      DATA IOPERA(  90),IX(  90),IY(  90)/'MOVE',   4,  12/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',   3,  10/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW',   2,   6/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',   1,  -3/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',   0,  -9/
      DATA IOPERA(  95),IX(  95),IY(  95)/'MOVE',   2,   6/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',   2,  -3/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',   1,  -9/
      DATA IOPERA(  98),IX(  98),IY(  98)/'MOVE',  14,  12/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',  12,  11/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',  10,   9/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',   8,   6/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',   5,  -3/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',   3,  -7/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',   1,  -9/
C
      DATA IXMIND(  23)/ -12/
      DATA IXMAXD(  23)/  11/
      DATA IXDELD(  23)/  23/
      DATA ISTARD(  23)/  70/
      DATA NUMCOO(  23)/  35/
C
C     DEFINE CHARACTER   2574--UPPER CASE X
C
      DATA IOPERA( 105),IX( 105),IY( 105)/'MOVE',  -2,   7/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',  -3,   6/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',  -5,   6/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',  -6,   7/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',  -6,   9/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',  -5,  11/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',  -3,  12/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',  -1,  12/
      DATA IOPERA( 113),IX( 113),IY( 113)/'DRAW',   1,  11/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',   2,   9/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',   2,   6/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',   1,   2/
      DATA IOPERA( 117),IX( 117),IY( 117)/'DRAW',  -1,  -3/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',  -3,  -6/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',  -5,  -8/
      DATA IOPERA( 120),IX( 120),IY( 120)/'DRAW',  -8,  -9/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW', -10,  -9/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW', -11,  -8/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW', -11,  -6/
      DATA IOPERA( 124),IX( 124),IY( 124)/'DRAW', -10,  -5/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',  -9,  -6/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW', -10,  -7/
      DATA IOPERA( 127),IX( 127),IY( 127)/'MOVE',  -1,  12/
      DATA IOPERA( 128),IX( 128),IY( 128)/'DRAW',   0,  11/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',   1,   9/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',   1,   6/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',   0,   2/
      DATA IOPERA( 132),IX( 132),IY( 132)/'DRAW',  -2,  -3/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',  -4,  -6/
      DATA IOPERA( 134),IX( 134),IY( 134)/'DRAW',  -6,  -8/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',  -8,  -9/
      DATA IOPERA( 136),IX( 136),IY( 136)/'MOVE',  11,  11/
      DATA IOPERA( 137),IX( 137),IY( 137)/'DRAW',  10,  10/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW',  11,   9/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW',  12,  10/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW',  12,  11/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',  11,  12/
      DATA IOPERA( 142),IX( 142),IY( 142)/'DRAW',   9,  12/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',   7,  11/
      DATA IOPERA( 144),IX( 144),IY( 144)/'DRAW',   5,   9/
      DATA IOPERA( 145),IX( 145),IY( 145)/'DRAW',   3,   6/
      DATA IOPERA( 146),IX( 146),IY( 146)/'DRAW',   1,   2/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',   0,  -3/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',   0,  -6/
      DATA IOPERA( 149),IX( 149),IY( 149)/'DRAW',   1,  -8/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',   2,  -9/
      DATA IOPERA( 151),IX( 151),IY( 151)/'DRAW',   3,  -9/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',   5,  -8/
      DATA IOPERA( 153),IX( 153),IY( 153)/'DRAW',   7,  -6/
C
      DATA IXMIND(  24)/ -10/
      DATA IXMAXD(  24)/  10/
      DATA IXDELD(  24)/  20/
      DATA ISTARD(  24)/ 105/
      DATA NUMCOO(  24)/  49/
C
C     DEFINE CHARACTER   2575--UPPER CASE Y
C
      DATA IOPERA( 154),IX( 154),IY( 154)/'MOVE',  -8,   8/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW',  -6,  11/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',  -4,  12/
      DATA IOPERA( 157),IX( 157),IY( 157)/'DRAW',  -3,  12/
      DATA IOPERA( 158),IX( 158),IY( 158)/'DRAW',  -1,  11/
      DATA IOPERA( 159),IX( 159),IY( 159)/'DRAW',  -1,   9/
      DATA IOPERA( 160),IX( 160),IY( 160)/'DRAW',  -3,   3/
      DATA IOPERA( 161),IX( 161),IY( 161)/'DRAW',  -3,   0/
      DATA IOPERA( 162),IX( 162),IY( 162)/'DRAW',  -2,  -2/
      DATA IOPERA( 163),IX( 163),IY( 163)/'MOVE',  -3,  12/
      DATA IOPERA( 164),IX( 164),IY( 164)/'DRAW',  -2,  11/
      DATA IOPERA( 165),IX( 165),IY( 165)/'DRAW',  -2,   9/
      DATA IOPERA( 166),IX( 166),IY( 166)/'DRAW',  -4,   3/
      DATA IOPERA( 167),IX( 167),IY( 167)/'DRAW',  -4,   0/
      DATA IOPERA( 168),IX( 168),IY( 168)/'DRAW',  -2,  -2/
      DATA IOPERA( 169),IX( 169),IY( 169)/'DRAW',   0,  -2/
      DATA IOPERA( 170),IX( 170),IY( 170)/'DRAW',   3,  -1/
      DATA IOPERA( 171),IX( 171),IY( 171)/'DRAW',   5,   1/
      DATA IOPERA( 172),IX( 172),IY( 172)/'DRAW',   7,   4/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW',   8,   6/
      DATA IOPERA( 174),IX( 174),IY( 174)/'MOVE',  10,  12/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW',   8,   6/
      DATA IOPERA( 176),IX( 176),IY( 176)/'DRAW',   5,  -2/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW',   3,  -6/
      DATA IOPERA( 178),IX( 178),IY( 178)/'MOVE',  11,  12/
      DATA IOPERA( 179),IX( 179),IY( 179)/'DRAW',   9,   6/
      DATA IOPERA( 180),IX( 180),IY( 180)/'DRAW',   7,   1/
      DATA IOPERA( 181),IX( 181),IY( 181)/'DRAW',   5,  -3/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',   3,  -6/
      DATA IOPERA( 183),IX( 183),IY( 183)/'DRAW',   1,  -8/
      DATA IOPERA( 184),IX( 184),IY( 184)/'DRAW',  -2,  -9/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',  -6,  -9/
      DATA IOPERA( 186),IX( 186),IY( 186)/'DRAW',  -8,  -8/
      DATA IOPERA( 187),IX( 187),IY( 187)/'DRAW',  -9,  -6/
      DATA IOPERA( 188),IX( 188),IY( 188)/'DRAW',  -9,  -5/
      DATA IOPERA( 189),IX( 189),IY( 189)/'DRAW',  -8,  -4/
      DATA IOPERA( 190),IX( 190),IY( 190)/'DRAW',  -7,  -5/
      DATA IOPERA( 191),IX( 191),IY( 191)/'DRAW',  -8,  -6/
C
      DATA IXMIND(  25)/ -11/
      DATA IXMAXD(  25)/  11/
      DATA IXDELD(  25)/  22/
      DATA ISTARD(  25)/ 154/
      DATA NUMCOO(  25)/  38/
C
C     DEFINE CHARACTER   2576--UPPER CASE Z
C
      DATA IOPERA( 192),IX( 192),IY( 192)/'MOVE',   8,  10/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW',   7,   8/
      DATA IOPERA( 194),IX( 194),IY( 194)/'DRAW',   5,   3/
      DATA IOPERA( 195),IX( 195),IY( 195)/'DRAW',   4,   0/
      DATA IOPERA( 196),IX( 196),IY( 196)/'DRAW',   3,  -2/
      DATA IOPERA( 197),IX( 197),IY( 197)/'DRAW',   1,  -5/
      DATA IOPERA( 198),IX( 198),IY( 198)/'DRAW',  -1,  -7/
      DATA IOPERA( 199),IX( 199),IY( 199)/'DRAW',  -3,  -8/
      DATA IOPERA( 200),IX( 200),IY( 200)/'DRAW',  -6,  -9/
      DATA IOPERA( 201),IX( 201),IY( 201)/'MOVE',   1,   6/
      DATA IOPERA( 202),IX( 202),IY( 202)/'DRAW',   0,   4/
      DATA IOPERA( 203),IX( 203),IY( 203)/'DRAW',  -2,   3/
      DATA IOPERA( 204),IX( 204),IY( 204)/'DRAW',  -4,   3/
      DATA IOPERA( 205),IX( 205),IY( 205)/'DRAW',  -5,   5/
      DATA IOPERA( 206),IX( 206),IY( 206)/'DRAW',  -5,   7/
      DATA IOPERA( 207),IX( 207),IY( 207)/'DRAW',  -4,   9/
      DATA IOPERA( 208),IX( 208),IY( 208)/'DRAW',  -2,  11/
      DATA IOPERA( 209),IX( 209),IY( 209)/'DRAW',   1,  12/
      DATA IOPERA( 210),IX( 210),IY( 210)/'DRAW',  11,  12/
      DATA IOPERA( 211),IX( 211),IY( 211)/'DRAW',   9,  11/
      DATA IOPERA( 212),IX( 212),IY( 212)/'DRAW',   8,  10/
      DATA IOPERA( 213),IX( 213),IY( 213)/'DRAW',   7,   7/
      DATA IOPERA( 214),IX( 214),IY( 214)/'DRAW',   6,   3/
      DATA IOPERA( 215),IX( 215),IY( 215)/'DRAW',   4,  -3/
      DATA IOPERA( 216),IX( 216),IY( 216)/'DRAW',   2,  -6/
      DATA IOPERA( 217),IX( 217),IY( 217)/'DRAW',  -1,  -8/
      DATA IOPERA( 218),IX( 218),IY( 218)/'DRAW',  -6,  -9/
      DATA IOPERA( 219),IX( 219),IY( 219)/'DRAW', -10,  -9/
      DATA IOPERA( 220),IX( 220),IY( 220)/'DRAW', -11,  -8/
      DATA IOPERA( 221),IX( 221),IY( 221)/'DRAW', -11,  -6/
      DATA IOPERA( 222),IX( 222),IY( 222)/'DRAW', -10,  -5/
      DATA IOPERA( 223),IX( 223),IY( 223)/'DRAW',  -8,  -5/
      DATA IOPERA( 224),IX( 224),IY( 224)/'DRAW',  -6,  -6/
      DATA IOPERA( 225),IX( 225),IY( 225)/'DRAW',  -3,  -8/
      DATA IOPERA( 226),IX( 226),IY( 226)/'DRAW',  -1,  -9/
      DATA IOPERA( 227),IX( 227),IY( 227)/'DRAW',   2,  -9/
      DATA IOPERA( 228),IX( 228),IY( 228)/'DRAW',   5,  -8/
      DATA IOPERA( 229),IX( 229),IY( 229)/'DRAW',   7,  -6/
      DATA IOPERA( 230),IX( 230),IY( 230)/'MOVE',   4,  12/
      DATA IOPERA( 231),IX( 231),IY( 231)/'DRAW',   8,  11/
      DATA IOPERA( 232),IX( 232),IY( 232)/'DRAW',   9,  11/
C
      DATA IXMIND(  26)/ -11/
      DATA IXMAXD(  26)/  10/
      DATA IXDELD(  26)/  21/
      DATA ISTARD(  26)/ 192/
      DATA NUMCOO(  26)/  41/
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCSU4--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **************************************
C               **  STEP 2--                        **
C               **  EXTRACT THE COORDINATES         **
C               **  FOR THIS PARTICULAR CHARACTER.  **
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
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DRCSU4--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ICHARN
 9013 FORMAT('ICHARN = ',I8)
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

      SUBROUTINE DQAG(F,A,B,EPSABS,EPSREL,KEY,RESULT,ABSERR,NEVAL,IER,   &
                      LIMIT,LENW,LAST,IWORK,WORK)
!
!   ADDED TO DATAPLOT 12/2003.  THIS ROUTINE ADDED FOR INTERNAL
!   DATAPLOT USAGE.
!
!***BEGIN PROLOGUE  DQAG
!***DATE WRITTEN   800101   (YYMMDD)
!***REVISION DATE  830518   (YYMMDD)
!***CATEGORY NO.  H2A1A1
!***KEYWORDS  AUTOMATIC INTEGRATOR,GAUSS-KRONROD,GENERAL-PURPOSE,
!             GLOBALLY ADAPTIVE,INTEGRAND EXAMINATOR
!***AUTHOR  PIESSENS, ROBERT, APPLIED MATH. AND PROGR. DIV. -
!             K. U. LEUVEN
!           DE DONCKER, ELISE, APPLIED MATH. AND PROGR. DIV. -
!             K. U. LEUVEN
!***PURPOSE  The routine calculates an approximation result to a given
!            definite integral I = integral of F over (A,B),
!            hopefully satisfying following claim for accuracy
!            ABS(I-RESULT)LE.MAX(EPSABS,EPSREL*ABS(I)).
!***DESCRIPTION
!
!        Computation of a definite integral
!        Standard fortran subroutine
!        Double precision version
!
!            F      - Double precision
!                     Function subprogam defining the integrand
!                     Function F(X). The actual name for F needs to be
!                     Declared E X T E R N A L in the driver program.
!
!            A      - Double precision
!                     Lower limit of integration
!
!            B      - Double precision
!                     Upper limit of integration
!
!            EPSABS - Double precision
!                     Absolute accoracy requested
!            EPSREL - Double precision
!                     Relative accuracy requested
!                     If  EPSABS.LE.0
!                     And EPSREL.LT.MAX(50*REL.MACH.ACC.,0.5D-28),
!                     The routine will end with IER = 6.
!
!            KEY    - Integer
!                     Key for choice of local integration rule
!                     A GAUSS-KRONROD PAIR is used with
!                       7 - 15 POINTS If KEY.LT.2,
!                      10 - 21 POINTS If KEY = 2,
!                      15 - 31 POINTS If KEY = 3,
!                      20 - 41 POINTS If KEY = 4,
!                      25 - 51 POINTS If KEY = 5,
!                      30 - 61 POINTS If KEY.GT.5.
!
!         ON RETURN
!            RESULT - Double precision
!                     Approximation to the integral
!
!            ABSERR - Double precision
!                     Estimate of the modulus of the absolute error,
!                     Which should EQUAL or EXCEED ABS(I-RESULT)
!
!            NEVAL  - Integer
!                     Number of integrand evaluations
!
!            IER    - Integer
!                     IER = 0 Normal and reliable termination of the
!                             routine. It is assumed that the requested
!                             accuracy has been achieved.
!                     IER.GT.0 Abnormal termination of the routine
!                             The estimates for RESULT and ERROR are
!                             Less reliable. It is assumed that the
!                             requested accuracy has not been achieved.
!                      ERROR MESSAGES
!                     IER = 1 Maximum number of subdivisions allowed
!                             has been achieved. One can allow more
!                             subdivisions by increasing the value of
!                             LIMIT (and taking the according dimension
!                             adjustments into account). HOWEVER, If
!                             this yield no improvement it is advised
!                             to analyze the integrand in order to
!                             determine the integration difficulaties.
!                             If the position of a local difficulty can
!                             be determined (I.E.SINGULARITY,
!                             DISCONTINUITY WITHIN THE INTERVAL) One
!                             will probably gain from splitting up the
!                             interval at this point and calling the
!                             INTEGRATOR on the SUBRANGES. If possible,
!                             AN APPROPRIATE SPECIAL-PURPOSE INTEGRATOR
!                             should be used which is designed for
!                             handling the type of difficulty involved.
!                         = 2 The occurrence of roundoff error is
!                             detected, which prevents the requested
!                             tolerance from being achieved.
!                         = 3 Extremely bad integrand behaviour occurs
!                             at some points of the integration
!                             interval.
!                         = 6 The input is invalid, because
!                             (EPSABS.LE.0 AND
!                              EPSREL.LT.MAX(50*REL.MACH.ACC.,0.5D-28))
!                             OR LIMIT.LT.1 OR LENW.LT.LIMIT*4.
!                             RESULT, ABSERR, NEVAL, LAST are set
!                             to zero.
!                             EXCEPT when LENW is invalid, IWORK(1),
!                             WORK(LIMIT*2+1) and WORK(LIMIT*3+1) are
!                             set to zero, WORK(1) is set to A and
!                             WORK(LIMIT+1) to B.
!
!         DIMENSIONING PARAMETERS
!            LIMIT - Integer
!                    Dimensioning parameter for IWORK
!                    Limit determines the maximum number of subintervals
!                    in the partition of the given integration interval
!                    (A,B), LIMIT.GE.1.
!                    If LIMIT.LT.1, the routine will end with IER = 6.
!
!            LENW  - Integer
!                    Dimensioning parameter for work
!                    LENW must be at least LIMIT*4.
!                    IF LENW.LT.LIMIT*4, the routine will end with
!                    IER = 6.
!
!            LAST  - Integer
!                    On return, LAST equals the number of subintervals
!                    produced in the subdiviosion process, which
!                    determines the number of significant elements
!                    actually in the WORK ARRAYS.
!
!         WORK ARRAYS
!            IWORK - Integer
!                    Vector of dimension at least limit, the first K
!                    elements of which contain pointers to the error
!                    estimates over the subintervals, such that
!                    WORK(LIMIT*3+IWORK(1)),... , WORK(LIMIT*3+IWORK(K))
!                    form a decreasing sequence with K = LAST If
!                    LAST.LE.(LIMIT/2+2), and K = LIMIT+1-LAST otherwise
!
!            WORK  - Double precision
!                    Vector of dimension at least LENW
!                    on return
!                    WORK(1), ..., WORK(LAST) contain the left end
!                    points of the subintervals in the partition of
!                     (A,B),
!                    WORK(LIMIT+1), ..., WORK(LIMIT+LAST) contain the
!                     right end points,
!                    WORK(LIMIT*2+1), ..., WORK(LIMIT*2+LAST) contain
!                     the integral approximations over the subintervals,
!                    WORK(LIMIT*3+1), ..., WORK(LIMIT*3+LAST) contain
!                     the error estimates.
!***REFERENCES  (NONE)
!***ROUTINES CALLED  DQAGE,XERROR
!***END PROLOGUE  DQAG
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DOUBLE PRECISION A,ABSERR,B,EPSABS,EPSREL,F,RESULT,WORK
      INTEGER IER,IWORK,KEY,LAST,LENW,LIMIT,LVL,L1,L2,L3,NEVAL
!
      DIMENSION IWORK(LIMIT),WORK(LENW)
!
      EXTERNAL F
!
!         CHECK VALIDITY OF LENW.
!
!***FIRST EXECUTABLE STATEMENT  DQAG
      IER = 6
      NEVAL = 0
      LAST = 0
      RESULT = 0.0D+00
      ABSERR = 0.0D+00
      IF(LIMIT.LT.1.OR.LENW.LT.LIMIT*4) GO TO 10
!
!         PREPARE CALL FOR DQAGE.
!
      L1 = LIMIT+1
      L2 = LIMIT+L1
      L3 = LIMIT+L2
!
      CALL DQAGE(F,A,B,EPSABS,EPSREL,KEY,LIMIT,RESULT,ABSERR,NEVAL,   &
        IER,WORK(1),WORK(L1),WORK(L2),WORK(L3),IWORK,LAST)
!
!         CALL ERROR HANDLER IF NECESSARY.
!
      LVL = 0
10    IF(IER.EQ.6) LVL = 1
      IF(IER.NE.0) THEN
!CCCC   CALL XERROR( 'ABNORMAL RETURN FROM DQAG ',26,IER,LVL)
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,901)
  901   FORMAT('***** ERROR--ABNORMAL RETURN FROM DQAG INTEGRATION ',   &
               'ROUTINE.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      RETURN
      END SUBROUTINE DQAG
      SUBROUTINE DQAGE(F,A,B,EPSABS,EPSREL,KEY,LIMIT,RESULT,ABSERR,   &
         NEVAL,IER,ALIST,BLIST,RLIST,ELIST,IORD,LAST)
!
!   ADDED TO DATAPLOT 12/2003.  THIS ROUTINE ADDED FOR INTERNAL
!   DATAPLOT USAGE.
!
!***BEGIN PROLOGUE  DQAGE
!***DATE WRITTEN   800101   (YYMMDD)
!***REVISION DATE  830518   (YYMMDD)
!***REVISION HISTORY (YYMMDD)
!   000601   Changed DMAX1/DABS to generic MAX/ABS
!***CATEGORY NO.  H2A1A1
!***KEYWORDS  AUTOMATIC INTEGRATOR,GAUSS-KRONROD,GENERAL-PURPOSE,
!             GLOBALLY ADAPTIVE,INTEGRAND EXAMINATOR
!***AUTHOR  PIESSENS, ROBERT, APPLIED MATH. AND PROGR. DIV. -
!             K. U. LEUVEN
!           DE DONCKER, ELISE, APPLIED MATH. AND PROGR. DIV. -
!             K. U. LEUVEN
!***PURPOSE  The routine calculates an approximation result to a given
!            definite integral   I = Integral of F over (A,B),
!            hopefully satisfying following claim for accuracy
!            ABS(I-RESLT).LE.MAX(EPSABS,EPSREL*ABS(I)).
!***DESCRIPTION
!
!        Computation of a definite integral
!        Standard fortran subroutine
!        Double precision version
!
!        PARAMETERS
!         ON ENTRY
!            F      - Double precision
!                     Function subprogram defining the integrand
!                     function F(X). The actual name for F needs to be
!                     declared E X T E R N A L in the driver program.
!
!            A      - Double precision
!                     Lower limit of integration
!
!            B      - Double precision
!                     Upper limit of integration
!
!            EPSABS - Double precision
!                     Absolute accuracy requested
!            EPSREL - Double precision
!                     Relative accuracy requested
!                     If  EPSABS.LE.0
!                     and EPSREL.LT.MAX(50*REL.MACH.ACC.,0.5D-28),
!                     the routine will end with IER = 6.
!
!            KEY    - Integer
!                     Key for choice of local integration rule
!                     A Gauss-Kronrod pair is used with
!                          7 - 15 points if KEY.LT.2,
!                         10 - 21 points if KEY = 2,
!                         15 - 31 points if KEY = 3,
!                         20 - 41 points if KEY = 4,
!                         25 - 51 points if KEY = 5,
!                         30 - 61 points if KEY.GT.5.
!
!            LIMIT  - Integer
!                     Gives an upperbound on the number of subintervals
!                     in the partition of (A,B), LIMIT.GE.1.
!
!         ON RETURN
!            RESULT - Double precision
!                     Approximation to the integral
!
!            ABSERR - Double precision
!                     Estimate of the modulus of the absolute error,
!                     which should equal or exceed ABS(I-RESULT)
!
!            NEVAL  - Integer
!                     Number of integrand evaluations
!
!            IER    - Integer
!                     IER = 0 Normal and reliable termination of the
!                             routine. It is assumed that the requested
!                             accuracy has been achieved.
!                     IER.GT.0 Abnormal termination of the routine
!                             The estimates for result and error are
!                             less reliable. It is assumed that the
!                             requested accuracy has not been achieved.
!            ERROR MESSAGES
!                     IER = 1 Maximum number of subdivisions allowed
!                             has been achieved. One can allow more
!                             subdivisions by increasing the value
!                             of LIMIT.
!                             However, if this yields no improvement it
!                             is rather advised to analyze the integrand
!                             in order to determine the integration
!                             difficulties. If the position of a local
!                             difficulty can be determined(e.g.
!                             SINGULARITY, DISCONTINUITY within the
!                             interval) one will probably gain from
!                             splitting up the interval at this point
!                             and calling the integrator on the
!                             subranges. If possible, an appropriate
!                             special-purpose integrator should be used
!                             which is designed for handling the type of
!                             difficulty involved.
!                         = 2 The occurrence of roundoff error is
!                             detected, which prevents the requested
!                             tolerance from being achieved.
!                         = 3 Extremely bad integrand behaviour occurs
!                             at some points of the integration
!                             interval.
!                         = 6 The input is invalid, because
!                             (EPSABS.LE.0 and
!                              EPSREL.LT.MAX(50*REL.MACH.ACC.,0.5D-28),
!                             RESULT, ABSERR, NEVAL, LAST, RLIST(1) ,
!                             ELIST(1) and IORD(1) are set to zero.
!                             ALIST(1) and BLIST(1) are set to A and B
!                             respectively.
!
!            ALIST   - Double precision
!                      Vector of dimension at least LIMIT, the first
!                       LAST  elements of which are the left
!                      end points of the subintervals in the partition
!                      of the given integration range (A,B)
!
!            BLIST   - Double precision
!                      Vector of dimension at least LIMIT, the first
!                       LAST  elements of which are the right
!                      end points of the subintervals in the partition
!                      of the given integration range (A,B)
!
!            RLIST   - Double precision
!                      Vector of dimension at least LIMIT, the first
!                       LAST  elements of which are the
!                      integral approximations on the subintervals
!
!            ELIST   - Double precision
!                      Vector of dimension at least LIMIT, the first
!                       LAST  elements of which are the moduli of the
!                      absolute error estimates on the subintervals
!
!            IORD    - Integer
!                      Vector of dimension at least LIMIT, the first K
!                      elements of which are pointers to the
!                      error estimates over the subintervals,
!                      such that ELIST(IORD(1)), ...,
!                      ELIST(IORD(K)) form a decreasing sequence,
!                      with K = LAST if LAST.LE.(LIMIT/2+2), and
!                      K = LIMIT+1-LAST otherwise
!
!            LAST    - Integer
!                      Number of subintervals actually produced in the
!                      subdivision process
!***REFERENCES  (NONE)
!***ROUTINES CALLED  D1MACH,DQK15,DQK21,DQK31,DQK41,DQK51,DQK61,DQPSRT
!***END PROLOGUE  DQAGE
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DOUBLE PRECISION A,ABSERR,ALIST,AREA,AREA1,AREA12,AREA2,A1,A2,B,   &
        BLIST,B1,B2,DEFABS,DEFAB1,DEFAB2,ELIST,EPMACH,   &
        EPSABS,EPSREL,ERRBND,ERRMAX,ERROR1,ERROR2,ERRO12,ERRSUM,F,   &
        RESABS,RESULT,RLIST,UFLOW
      INTEGER IER,IORD,IROFF1,IROFF2,K,KEY,KEYF,LAST,LIMIT,MAXERR,NEVAL,   &
        NRMAX
!
      DIMENSION ALIST(LIMIT),BLIST(LIMIT),ELIST(LIMIT),IORD(LIMIT),   &
        RLIST(LIMIT)
!
      EXTERNAL F
!
!            LIST OF MAJOR VARIABLES
!            -----------------------
!
!           ALIST     - LIST OF LEFT END POINTS OF ALL SUBINTERVALS
!                       CONSIDERED UP TO NOW
!           BLIST     - LIST OF RIGHT END POINTS OF ALL SUBINTERVALS
!                       CONSIDERED UP TO NOW
!           RLIST(I)  - APPROXIMATION TO THE INTEGRAL OVER
!                      (ALIST(I),BLIST(I))
!           ELIST(I)  - ERROR ESTIMATE APPLYING TO RLIST(I)
!           MAXERR    - POINTER TO THE INTERVAL WITH LARGEST
!                       ERROR ESTIMATE
!           ERRMAX    - ELIST(MAXERR)
!           AREA      - SUM OF THE INTEGRALS OVER THE SUBINTERVALS
!           ERRSUM    - SUM OF THE ERRORS OVER THE SUBINTERVALS
!           ERRBND    - REQUESTED ACCURACY MAX(EPSABS,EPSREL*
!                       ABS(RESULT))
!           *****1    - VARIABLE FOR THE LEFT SUBINTERVAL
!           *****2    - VARIABLE FOR THE RIGHT SUBINTERVAL
!           LAST      - INDEX FOR SUBDIVISION
!
!
!           MACHINE DEPENDENT CONSTANTS
!           ---------------------------
!
!           EPMACH  IS THE LARGEST RELATIVE SPACING.
!           UFLOW  IS THE SMALLEST POSITIVE MAGNITUDE.
!
!***FIRST EXECUTABLE STATEMENT  DQAGE
      EPMACH = D1MACH(4)
      UFLOW = D1MACH(1)
!
!           TEST ON VALIDITY OF PARAMETERS
!           ------------------------------
!
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
      IF(EPSABS.LE.0.0D+00.AND.   &
        EPSREL.LT.MAX(0.5D+02*EPMACH,0.5D-28)) IER = 6
      IF(IER.EQ.6) GO TO 999
!
!           FIRST APPROXIMATION TO THE INTEGRAL
!           -----------------------------------
!
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
!
!           TEST ON ACCURACY.
!
      ERRBND = MAX(EPSABS,EPSREL*ABS(RESULT))
      IF(ABSERR.LE.0.5D+02*EPMACH*DEFABS.AND.ABSERR.GT.ERRBND) IER = 2
      IF(LIMIT.EQ.1) IER = 1
      IF(IER.NE.0.OR.(ABSERR.LE.ERRBND.AND.ABSERR.NE.RESABS)   &
        .OR.ABSERR.EQ.0.0D+00) GO TO 60
!
!           INITIALIZATION
!           --------------
!
!
      ERRMAX = ABSERR
      MAXERR = 1
      AREA = RESULT
      ERRSUM = ABSERR
      NRMAX = 1
      IROFF1 = 0
      IROFF2 = 0
!
!           MAIN DO-LOOP
!           ------------
!
      DO 30 LAST = 2,LIMIT
!
!           BISECT THE SUBINTERVAL WITH THE LARGEST ERROR ESTIMATE.
!
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
!
!           IMPROVE PREVIOUS APPROXIMATIONS TO INTEGRAL
!           AND ERROR AND TEST FOR ACCURACY.
!
        NEVAL = NEVAL+1
        AREA12 = AREA1+AREA2
        ERRO12 = ERROR1+ERROR2
        ERRSUM = ERRSUM+ERRO12-ERRMAX
        AREA = AREA+AREA12-RLIST(MAXERR)
        IF(DEFAB1.EQ.ERROR1.OR.DEFAB2.EQ.ERROR2) GO TO 5
        IF(ABS(RLIST(MAXERR)-AREA12).LE.0.1D-04*ABS(AREA12)   &
        .AND.ERRO12.GE.0.99D+00*ERRMAX) IROFF1 = IROFF1+1
        IF(LAST.GT.10.AND.ERRO12.GT.ERRMAX) IROFF2 = IROFF2+1
    5   RLIST(MAXERR) = AREA1
        RLIST(LAST) = AREA2
        ERRBND = MAX(EPSABS,EPSREL*ABS(AREA))
        IF(ERRSUM.LE.ERRBND) GO TO 8
!
!           TEST FOR ROUNDOFF ERROR AND EVENTUALLY SET ERROR FLAG.
!
        IF(IROFF1.GE.6.OR.IROFF2.GE.20) IER = 2
!
!           SET ERROR FLAG IN THE CASE THAT THE NUMBER OF SUBINTERVALS
!           EQUALS LIMIT.
!
        IF(LAST.EQ.LIMIT) IER = 1
!
!           SET ERROR FLAG IN THE CASE OF BAD INTEGRAND BEHAVIOUR
!           AT A POINT OF THE INTEGRATION RANGE.
!
        IF(MAX(ABS(A1),ABS(B2)).LE.(0.1D+01+0.1D+03*   &
        EPMACH)*(ABS(A2)+0.1D+04*UFLOW)) IER = 3
!
!           APPEND THE NEWLY-CREATED INTERVALS TO THE LIST.
!
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
!
!           CALL SUBROUTINE DQPSRT TO MAINTAIN THE DESCENDING ORDERING
!           IN THE LIST OF ERROR ESTIMATES AND SELECT THE SUBINTERVAL
!           WITH THE LARGEST ERROR ESTIMATE (TO BE BISECTED NEXT).
!
   20   CALL DQPSRT(LIMIT,LAST,MAXERR,ERRMAX,ELIST,IORD,NRMAX)
! ***JUMP OUT OF DO-LOOP
        IF(IER.NE.0.OR.ERRSUM.LE.ERRBND) GO TO 40
   30 CONTINUE
!
!           COMPUTE FINAL RESULT.
!           ---------------------
!
   40 RESULT = 0.0D+00
      DO 50 K=1,LAST
        RESULT = RESULT+RLIST(K)
   50 CONTINUE
      ABSERR = ERRSUM
   60 IF(KEYF.NE.1) NEVAL = (10*KEYF+1)*(2*NEVAL+1)
      IF(KEYF.EQ.1) NEVAL = 30*NEVAL+15
  999 RETURN
      END SUBROUTINE DQAGE
      SUBROUTINE DQAGI(F,BOUND,INF,EPSABS,EPSREL,RESULT,ABSERR,NEVAL,   &
         IER,LIMIT,LENW,LAST,IWORK,WORK)
!
!   ADDED TO DATAPLOT 12/2003.  THIS ROUTINE ADDED FOR INTERNAL
!   DATAPLOT USAGE.
!
!***BEGIN PROLOGUE  DQAGI
!***DATE WRITTEN   800101   (YYMMDD)
!***REVISION DATE  830518   (YYMMDD)
!***CATEGORY NO.  H2A3A1,H2A4A1
!***KEYWORDS  AUTOMATIC INTEGRATOR,EXTRAPOLATION,GENERAL-PURPOSE,
!             GLOBALLY ADAPTIVE,INFINITE INTERVALS,TRANSFORMATION
!***AUTHOR  PIESSENS, ROBERT, APPLIED MATH. AND PROGR. DIV. -
!             K. U. LEUVEN
!           DE DONCKER, ELISE, APPLIED MATH. AND PROGR. DIV. -
!             K. U. LEUVEN
!***PURPOSE  The routine calculates an approximation result to a given
!            INTEGRAL   I = Integral of F over (BOUND,+INFINITY)
!            OR I = Integral of F over (-INFINITY,BOUND)
!            OR I = Integral of F over (-INFINITY,+INFINITY)
!            Hopefully satisfying following claim for accuracy
!            ABS(I-RESULT).LE.MAX(EPSABS,EPSREL*ABS(I)).
!***DESCRIPTION
!
!        Integration over infinite intervals
!        Standard fortran subroutine
!
!        PARAMETERS
!         ON ENTRY
!            F      - Double precision
!                     Function subprogram defining the integrand
!                     function F(X). The actual name for F needs to be
!                     declared E X T E R N A L in the driver program.
!
!            BOUND  - Double precision
!                     Finite bound of integration range
!                     (has no meaning if interval is doubly-infinite)
!
!            INF    - Integer
!                     indicating the kind of integration range involved
!                     INF = 1 corresponds to  (BOUND,+INFINITY),
!                     INF = -1            to  (-INFINITY,BOUND),
!                     INF = 2             to (-INFINITY,+INFINITY).
!
!            EPSABS - Double precision
!                     Absolute accuracy requested
!            EPSREL - Double precision
!                     Relative accuracy requested
!                     If  EPSABS.LE.0
!                     and EPSREL.LT.MAX(50*REL.MACH.ACC.,0.5D-28),
!                     the routine will end with IER = 6.
!
!
!         ON RETURN
!            RESULT - Double precision
!                     Approximation to the integral
!
!            ABSERR - Double precision
!                     Estimate of the modulus of the absolute error,
!                     which should equal or exceed ABS(I-RESULT)
!
!            NEVAL  - Integer
!                     Number of integrand evaluations
!
!            IER    - Integer
!                     IER = 0 normal and reliable termination of the
!                             routine. It is assumed that the requested
!                             accuracy has been achieved.
!                   - IER.GT.0 abnormal termination of the routine. The
!                             estimates for result and error are less
!                             reliable. It is assumed that the requested
!                             accuracy has not been achieved.
!            ERROR MESSAGES
!                     IER = 1 Maximum number of subdivisions allowed
!                             has been achieved. One can allow more
!                             subdivisions by increasing the value of
!                             LIMIT (and taking the according dimension
!                             adjustments into account). However, if
!                             this yields no improvement it is advised
!                             to analyze the integrand in order to
!                             determine the integration difficulties. If
!                             the position of a local difficulty can be
!                             determined (e.g. SINGULARITY,
!                             DISCONTINUITY within the interval) one
!                             will probably gain from splitting up the
!                             interval at this point and calling the
!                             integrator on the subranges. If possible,
!                             an appropriate special-purpose integrator
!                             should be used, which is designed for
!                             handling the type of difficulty involved.
!                         = 2 The occurrence of roundoff error is
!                             detected, which prevents the requested
!                             tolerance from being achieved.
!                             The error may be under-estimated.
!                         = 3 Extremely bad integrand behaviour occurs
!                             at some points of the integration
!                             interval.
!                         = 4 The algorithm does not converge.
!                             Roundoff error is detected in the
!                             extrapolation table.
!                             It is assumed that the requested tolerance
!                             cannot be achieved, and that the returned
!                             RESULT is the best which can be obtained.
!                         = 5 The integral is probably divergent, or
!                             slowly convergent. It must be noted that
!                             divergence can occur with any other value
!                             of IER.
!                         = 6 The input is invalid, because
!                             (EPSABS.LE.0 and
!                              EPSREL.LT.MAX(50*REL.MACH.ACC.,0.5D-28))
!                              or LIMIT.LT.1 or LENIW.LT.LIMIT*4.
!                             RESULT, ABSERR, NEVAL, LAST are set to
!                             zero. Exept when LIMIT or LENIW is
!                             invalid, IWORK(1), WORK(LIMIT*2+1) and
!                             WORK(LIMIT*3+1) are set to ZERO, WORK(1)
!                             is set to A and WORK(LIMIT+1) to B.
!
!         DIMENSIONING PARAMETERS
!            LIMIT - Integer
!                    Dimensioning parameter for IWORK
!                    LIMIT determines the maximum number of subintervals
!                    in the partition of the given integration interval
!                    (A,B), LIMIT.GE.1.
!                    If LIMIT.LT.1, the routine will end with IER = 6.
!
!            LENW  - Integer
!                    Dimensioning parameter for WORK
!                    LENW must be at least LIMIT*4.
!                    If LENW.LT.LIMIT*4, the routine will end
!                    with IER = 6.
!
!            LAST  - Integer
!                    On return, LAST equals the number of subintervals
!                    produced in the subdivision process, which
!                    determines the number of significant elements
!                    actually in the WORK ARRAYS.
!
!         WORK ARRAYS
!            IWORK - Integer
!                    Vector of dimension at least LIMIT, the first
!                    K elements of which contain pointers
!                    to the error estimates over the subintervals,
!                    such that WORK(LIMIT*3+IWORK(1)),... ,
!                    WORK(LIMIT*3+IWORK(K)) form a decreasing
!                    sequence, with K = LAST if LAST.LE.(LIMIT/2+2), and
!                    K = LIMIT+1-LAST otherwise
!
!            WORK  - Double precision
!                    Vector of dimension at least LENW
!                    on return
!                    WORK(1), ..., WORK(LAST) contain the left
!                     end points of the subintervals in the
!                     partition of (A,B),
!                    WORK(LIMIT+1), ..., WORK(LIMIT+LAST) Contain
!                     the right end points,
!                    WORK(LIMIT*2+1), ...,WORK(LIMIT*2+LAST) contain the
!                     integral approximations over the subintervals,
!                    WORK(LIMIT*3+1), ..., WORK(LIMIT*3)
!                     contain the error estimates.
!***REFERENCES  (NONE)
!***ROUTINES CALLED  DQAGIE,XERROR
!***END PROLOGUE  DQAGI
!
      DOUBLE PRECISION ABSERR,BOUND,EPSABS,EPSREL,F,RESULT,WORK
      INTEGER IER,INF,IWORK,LAST,LENW,LIMIT,LVL,L1,L2,L3,NEVAL
!
      DIMENSION IWORK(LIMIT),WORK(LENW)
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      EXTERNAL F
!
!         CHECK VALIDITY OF LIMIT AND LENW.
!
!***FIRST EXECUTABLE STATEMENT  DQAGI
      IER = 6
      NEVAL = 0
      LAST = 0
      RESULT = 0.0D+00
      ABSERR = 0.0D+00
      IF(LIMIT.LT.1.OR.LENW.LT.LIMIT*4) GO TO 10
!
!         PREPARE CALL FOR DQAGIE.
!
      L1 = LIMIT+1
      L2 = LIMIT+L1
      L3 = LIMIT+L2
!
      CALL DQAGIE(F,BOUND,INF,EPSABS,EPSREL,LIMIT,RESULT,ABSERR,   &
        NEVAL,IER,WORK(1),WORK(L1),WORK(L2),WORK(L3),IWORK,LAST)
!
!         CALL ERROR HANDLER IF NECESSARY.
!
       LVL = 0
10    IF(IER.EQ.6) LVL = 1
      IF(IER.NE.0) THEN
!CCCC   CALL XERROR( 'ABNORMAL RETURN FROM  QAGI',
!CCCC1  26,IER,LVL)
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,901)
  901   FORMAT('***** ERROR--ABNORMAL RETURN FROM QAGI INTEGRATION ',   &
               'ROUTINE.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      RETURN
      END SUBROUTINE DQAGI
      SUBROUTINE DQAGIE(F,BOUND,INF,EPSABS,EPSREL,LIMIT,RESULT,ABSERR,   &
         NEVAL,IER,ALIST,BLIST,RLIST,ELIST,IORD,LAST)
!
!   ADDED TO DATAPLOT 12/2003.  THIS ROUTINE ADDED FOR INTERNAL
!   DATAPLOT USAGE.
!
!***BEGIN PROLOGUE  DQAGIE
!***DATE WRITTEN   800101   (YYMMDD)
!***REVISION DATE  830518   (YYMMDD)
!***REVISION DATE  980526   (YYMMDD) Fixed documentation of parameter INF
!***REVISION HISTORY (YYMMDD)
!   000601   Changed DMAX1/DABS to generic MAX/ABS
!***CATEGORY NO.  H2A3A1,H2A4A1
!***KEYWORDS  AUTOMATIC INTEGRATOR,EXTRAPOLATION,GENERAL-PURPOSE,
!             GLOBALLY ADAPTIVE,INFINITE INTERVALS,TRANSFORMATION
!***AUTHOR  PIESSENS, ROBERT, APPLIED MATH. AND PROGR. DIV. -
!             K. U. LEUVEN
!           DE DONCKER, ELISE, APPLIED MATH. AND PROGR. DIV. -
!             K. U. LEUVEN
!***PURPOSE  The routine calculates an approximation result to a given
!            integral   I = Integral of F over (BOUND,+INFINITY)
!            or I = Integral of F over (-INFINITY,BOUND)
!            or I = Integral of F over (-INFINITY,+INFINITY),
!            hopefully satisfying following claim for accuracy
!            ABS(I-RESULT).LE.MAX(EPSABS,EPSREL*ABS(I))
!***DESCRIPTION
!
! Integration over infinite intervals
! Standard fortran subroutine
!
!            F      - Double precision
!                     Function subprogram defining the integrand
!                     function F(X). The actual name for F needs to be
!                     declared E X T E R N A L in the driver program.
!
!            BOUND  - Double precision
!                     Finite bound of integration range
!                     (has no meaning if interval is doubly-infinite)
!
!            INF    - Integer
!                     Indicating the kind of integration range involved
!                     INF = 1 corresponds to  (BOUND,+INFINITY),
!                     INF = -1            to  (-INFINITY,BOUND),
!                     INF = 2             to (-INFINITY,+INFINITY).
!
!            EPSABS - Double precision
!                     Absolute accuracy requested
!            EPSREL - Double precision
!                     Relative accuracy requested
!                     If  EPSABS.LE.0
!                     and EPSREL.LT.MAX(50*REL.MACH.ACC.,0.5D-28),
!                     the routine will end with IER = 6.
!
!            LIMIT  - Integer
!                     Gives an upper bound on the number of subintervals
!                     in the partition of (A,B), LIMIT.GE.1
!
!         ON RETURN
!            RESULT - Double precision
!                     Approximation to the integral
!
!            ABSERR - Double precision
!                     Estimate of the modulus of the absolute error,
!                     which should equal or exceed ABS(I-RESULT)
!
!            NEVAL  - Integer
!                     Number of integrand evaluations
!
!            IER    - Integer
!                     IER = 0 Normal and reliable termination of the
!                             routine. It is assumed that the requested
!                             accuracy has been achieved.
!                   - IER.GT.0 Abnormal termination of the routine. The
!                             estimates for result and error are less
!                             reliable. It is assumed that the requested
!                             accuracy has not been achieved.
!            ERROR MESSAGES
!                     IER = 1 Maximum number of subdivisions allowed
!                             has been achieved. One can allow more
!                             subdivisions by increasing the value of
!                             LIMIT (and taking the according dimension
!                             adjustments into account). However,if
!                             this yields no improvement it is advised
!                             to analyze the integrand in order to
!                             determine the integration difficulties.
!                             If the position of a local difficulty can
!                             be determined (e.g. SINGULARITY,
!                             DISCONTINUITY within the interval) one
!                             will probably gain from splitting up the
!                             interval at this point and calling the
!                             integrator on the subranges. If possible,
!                             an appropriate special-purpose integrator
!                             should be used, which is designed for
!                             handling the type of difficulty involved.
!                         = 2 The occurrence of roundoff error is
!                             detected, which prevents the requested
!                             tolerance from being achieved.
!                             The error may be under-estimated.
!                         = 3 Extremely bad integrand behaviour occurs
!                             at some points of the integration
!                             interval.
!                         = 4 The algorithm does not converge.
!                             Roundoff error is detected in the
!                             extrapolation table.
!                             It is assumed that the requested tolerance
!                             cannot be achieved, and that the returned
!                             result is the best which can be obtained.
!                         = 5 The integral is probably divergent, or
!                             slowly convergent. It must be noted that
!                             divergence can occur with any other value
!                             of IER.
!                         = 6 The input is invalid, because
!                             (EPSABS.LE.0 and
!                              EPSREL.LT.MAX(50*REL.MACH.ACC.,0.5D-28),
!                             RESULT, ABSERR, NEVAL, LAST, RLIST(1),
!                             ELIST(1) and IORD(1) are set to zero.
!                             ALIST(1) and BLIST(1) are set to 0
!                             and 1 respectively.
!
!            ALIST  - Double precision
!                     Vector of dimension at least LIMIT, the first
!                      LAST  elements of which are the left
!                     end points of the subintervals in the partition
!                     of the transformed integration range (0,1).
!
!            BLIST  - Double precision
!                     Vector of dimension at least LIMIT, the first
!                      LAST  elements of which are the right
!                     end points of the subintervals in the partition
!                     of the transformed integration range (0,1).
!
!            RLIST  - Double precision
!                     Vector of dimension at least LIMIT, the first
!                      LAST  elements of which are the integral
!                     approximations on the subintervals
!
!            ELIST  - Double precision
!                     Vector of dimension at least LIMIT,  the first
!                     LAST elements of which are the moduli of the
!                     absolute error estimates on the subintervals
!
!            IORD   - Integer
!                     Vector of dimension LIMIT, the first K
!                     elements of which are pointers to the
!                     error estimates over the subintervals,
!                     such that ELIST(IORD(1)), ..., ELIST(IORD(K))
!                     form a decreasing sequence, with K = LAST
!                     If LAST.LE.(LIMIT/2+2), and K = LIMIT+1-LAST
!                     otherwise
!
!            LAST   - Integer
!                     Number of subintervals actually produced
!                     in the subdivision process
!***REFERENCES  (NONE)
!***ROUTINES CALLED  D1MACH,DQELG,DQK15I,DQPSRT
!***END PROLOGUE  DQAGIE
      DOUBLE PRECISION ABSEPS,ABSERR,ALIST,AREA,AREA1,AREA12,AREA2,A1,   &
        A2,BLIST,BOUN,BOUND,B1,B2,CORREC,DEFABS,DEFAB1,DEFAB2,   &
        DRES,ELIST,EPMACH,EPSABS,EPSREL,ERLARG,ERLAST,   &
        ERRBND,ERRMAX,ERROR1,ERROR2,ERRO12,ERRSUM,ERTEST,F,OFLOW,RESABS,   &
        RESEPS,RESULT,RES3LA,RLIST,RLIST2,SMALL,UFLOW
      INTEGER ID,IER,IERRO,INF,IORD,IROFF1,IROFF2,IROFF3,JUPBND,K,KSGN,   &
        KTMIN,LAST,LIMIT,MAXERR,NEVAL,NRES,NRMAX,NUMRL2
      LOGICAL EXTRAP,NOEXT
!
      DIMENSION ALIST(LIMIT),BLIST(LIMIT),ELIST(LIMIT),IORD(LIMIT),   &
        RES3LA(3),RLIST(LIMIT),RLIST2(52)
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      EXTERNAL F
!
!            THE DIMENSION OF RLIST2 IS DETERMINED BY THE VALUE OF
!            LIMEXP IN SUBROUTINE DQELG.
!
!
!            LIST OF MAJOR VARIABLES
!            -----------------------
!
!           ALIST     - LIST OF LEFT END POINTS OF ALL SUBINTERVALS
!                       CONSIDERED UP TO NOW
!           BLIST     - LIST OF RIGHT END POINTS OF ALL SUBINTERVALS
!                       CONSIDERED UP TO NOW
!           RLIST(I)  - APPROXIMATION TO THE INTEGRAL OVER
!                       (ALIST(I),BLIST(I))
!           RLIST2    - ARRAY OF DIMENSION AT LEAST (LIMEXP+2),
!                       CONTAINING THE PART OF THE EPSILON TABLE
!                       WICH IS STILL NEEDED FOR FURTHER COMPUTATIONS
!           ELIST(I)  - ERROR ESTIMATE APPLYING TO RLIST(I)
!           MAXERR    - POINTER TO THE INTERVAL WITH LARGEST ERROR
!                       ESTIMATE
!           ERRMAX    - ELIST(MAXERR)
!           ERLAST    - ERROR ON THE INTERVAL CURRENTLY SUBDIVIDED
!                       (BEFORE THAT SUBDIVISION HAS TAKEN PLACE)
!           AREA      - SUM OF THE INTEGRALS OVER THE SUBINTERVALS
!           ERRSUM    - SUM OF THE ERRORS OVER THE SUBINTERVALS
!           ERRBND    - REQUESTED ACCURACY MAX(EPSABS,EPSREL*
!                       ABS(RESULT))
!           *****1    - VARIABLE FOR THE LEFT SUBINTERVAL
!           *****2    - VARIABLE FOR THE RIGHT SUBINTERVAL
!           LAST      - INDEX FOR SUBDIVISION
!           NRES      - NUMBER OF CALLS TO THE EXTRAPOLATION ROUTINE
!           NUMRL2    - NUMBER OF ELEMENTS CURRENTLY IN RLIST2. IF AN
!                       APPROPRIATE APPROXIMATION TO THE COMPOUNDED
!                       INTEGRAL HAS BEEN OBTAINED, IT IS PUT IN
!                       RLIST2(NUMRL2) AFTER NUMRL2 HAS BEEN INCREASED
!                       BY ONE.
!           SMALL     - LENGTH OF THE SMALLEST INTERVAL CONSIDERED UP
!                       TO NOW, MULTIPLIED BY 1.5
!           ERLARG    - SUM OF THE ERRORS OVER THE INTERVALS LARGER
!                       THAN THE SMALLEST INTERVAL CONSIDERED UP TO NOW
!           EXTRAP    - LOGICAL VARIABLE DENOTING THAT THE ROUTINE
!                       IS ATTEMPTING TO PERFORM EXTRAPOLATION. I.E.
!                       BEFORE SUBDIVIDING THE SMALLEST INTERVAL WE
!                       TRY TO DECREASE THE VALUE OF ERLARG.
!           NOEXT     - LOGICAL VARIABLE DENOTING THAT EXTRAPOLATION
!                       IS NO LONGER ALLOWED (TRUE-VALUE)
!
!            MACHINE DEPENDENT CONSTANTS
!            ---------------------------
!
!           EPMACH IS THE LARGEST RELATIVE SPACING.
!           UFLOW IS THE SMALLEST POSITIVE MAGNITUDE.
!           OFLOW IS THE LARGEST POSITIVE MAGNITUDE.
!
!***FIRST EXECUTABLE STATEMENT  DQAGIE
       EPMACH = D1MACH(4)
!
!           TEST ON VALIDITY OF PARAMETERS
!           -----------------------------
!
      IER = 0
      NEVAL = 0
      LAST = 0
      RESULT = 0.0D+00
      ABSERR = 0.0D+00
      ALIST(1) = 0.0D+00
      BLIST(1) = 0.1D+01
      RLIST(1) = 0.0D+00
      ELIST(1) = 0.0D+00
      SMALL = 0.0D0
      CORREC = 0.0D0
      ERTEST = 0.0D0
      ERLARG = 0.0D0
      IORD(1) = 0
      IF(EPSABS.LE.0.0D+00.AND.EPSREL.LT.MAX(0.5D+02*EPMACH,0.5D-28))   &
        IER = 6
       IF(IER.EQ.6) GO TO 999
!
!
!           FIRST APPROXIMATION TO THE INTEGRAL
!           -----------------------------------
!
!           DETERMINE THE INTERVAL TO BE MAPPED ONTO (0,1).
!           IF INF = 2 THE INTEGRAL IS COMPUTED AS I = I1+I2, WHERE
!           I1 = INTEGRAL OF F OVER (-INFINITY,0),
!           I2 = INTEGRAL OF F OVER (0,+INFINITY).
!
      BOUN = BOUND
      IF(INF.EQ.2) BOUN = 0.0D+00
      CALL DQK15I(F,BOUN,INF,0.0D+00,0.1D+01,RESULT,ABSERR,   &
        DEFABS,RESABS)
!
!           TEST ON ACCURACY
!
      LAST = 1
      RLIST(1) = RESULT
      ELIST(1) = ABSERR
      IORD(1) = 1
      DRES = ABS(RESULT)
      ERRBND = MAX(EPSABS,EPSREL*DRES)
      IF(ABSERR.LE.1.0D+02*EPMACH*DEFABS.AND.ABSERR.GT.ERRBND) IER = 2
      IF(LIMIT.EQ.1) IER = 1
      IF(IER.NE.0.OR.(ABSERR.LE.ERRBND.AND.ABSERR.NE.RESABS).OR.   &
        ABSERR.EQ.0.0D+00) GO TO 130
!
!           INITIALIZATION
!           --------------
!
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
!
!           MAIN DO-LOOP
!           ------------
!
      DO 90 LAST = 2,LIMIT
!
!           BISECT THE SUBINTERVAL WITH NRMAX-TH LARGEST ERROR ESTIMATE.
!
        A1 = ALIST(MAXERR)
        B1 = 0.5D+00*(ALIST(MAXERR)+BLIST(MAXERR))
        A2 = B1
        B2 = BLIST(MAXERR)
        ERLAST = ERRMAX
        CALL DQK15I(F,BOUN,INF,A1,B1,AREA1,ERROR1,RESABS,DEFAB1)
        CALL DQK15I(F,BOUN,INF,A2,B2,AREA2,ERROR2,RESABS,DEFAB2)
!
!           IMPROVE PREVIOUS APPROXIMATIONS TO INTEGRAL
!           AND ERROR AND TEST FOR ACCURACY.
!
        AREA12 = AREA1+AREA2
        ERRO12 = ERROR1+ERROR2
        ERRSUM = ERRSUM+ERRO12-ERRMAX
        AREA = AREA+AREA12-RLIST(MAXERR)
        IF(DEFAB1.EQ.ERROR1.OR.DEFAB2.EQ.ERROR2)GO TO 15
        IF(ABS(RLIST(MAXERR)-AREA12).GT.0.1D-04*ABS(AREA12)   &
        .OR.ERRO12.LT.0.99D+00*ERRMAX) GO TO 10
        IF(EXTRAP) IROFF2 = IROFF2+1
        IF(.NOT.EXTRAP) IROFF1 = IROFF1+1
   10   IF(LAST.GT.10.AND.ERRO12.GT.ERRMAX) IROFF3 = IROFF3+1
   15   RLIST(MAXERR) = AREA1
        RLIST(LAST) = AREA2
        ERRBND = MAX(EPSABS,EPSREL*ABS(AREA))
!
!           TEST FOR ROUNDOFF ERROR AND EVENTUALLY SET ERROR FLAG.
!
        IF(IROFF1+IROFF2.GE.10.OR.IROFF3.GE.20) IER = 2
        IF(IROFF2.GE.5) IERRO = 3
!
!           SET ERROR FLAG IN THE CASE THAT THE NUMBER OF
!           SUBINTERVALS EQUALS LIMIT.
!
        IF(LAST.EQ.LIMIT) IER = 1
!
!           SET ERROR FLAG IN THE CASE OF BAD INTEGRAND BEHAVIOUR
!           AT SOME POINTS OF THE INTEGRATION RANGE.
!
        IF(MAX(ABS(A1),ABS(B2)).LE.(0.1D+01+0.1D+03*EPMACH)*   &
        (ABS(A2)+0.1D+04*UFLOW)) IER = 4
!
!           APPEND THE NEWLY-CREATED INTERVALS TO THE LIST.
!
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
!
!           CALL SUBROUTINE DQPSRT TO MAINTAIN THE DESCENDING ORDERING
!           IN THE LIST OF ERROR ESTIMATES AND SELECT THE SUBINTERVAL
!           WITH NRMAX-TH LARGEST ERROR ESTIMATE (TO BE BISECTED NEXT).
!
   30   CALL DQPSRT(LIMIT,LAST,MAXERR,ERRMAX,ELIST,IORD,NRMAX)
        IF(ERRSUM.LE.ERRBND) GO TO 115
        IF(IER.NE.0) GO TO 100
        IF(LAST.EQ.2) GO TO 80
        IF(NOEXT) GO TO 90
        ERLARG = ERLARG-ERLAST
        IF(ABS(B1-A1).GT.SMALL) ERLARG = ERLARG+ERRO12
        IF(EXTRAP) GO TO 40
!
!           TEST WHETHER THE INTERVAL TO BE BISECTED NEXT IS THE
!           SMALLEST INTERVAL.
!
        IF(ABS(BLIST(MAXERR)-ALIST(MAXERR)).GT.SMALL) GO TO 90
        EXTRAP = .TRUE.
        NRMAX = 2
   40   IF(IERRO.EQ.3.OR.ERLARG.LE.ERTEST) GO TO 60
!
!           THE SMALLEST INTERVAL HAS THE LARGEST ERROR.
!           BEFORE BISECTING DECREASE THE SUM OF THE ERRORS OVER THE
!           LARGER INTERVALS (ERLARG) AND PERFORM EXTRAPOLATION.
!
        ID = NRMAX
        JUPBND = LAST
        IF(LAST.GT.(2+LIMIT/2)) JUPBND = LIMIT+3-LAST
        DO 50 K = ID,JUPBND
          MAXERR = IORD(NRMAX)
          ERRMAX = ELIST(MAXERR)
          IF(ABS(BLIST(MAXERR)-ALIST(MAXERR)).GT.SMALL) GO TO 90
          NRMAX = NRMAX+1
   50   CONTINUE
!
!           PERFORM EXTRAPOLATION.
!
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
!
!            PREPARE BISECTION OF THE SMALLEST INTERVAL.
!
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
!
!           SET FINAL RESULT AND ERROR ESTIMATE.
!           ------------------------------------
!
  100 IF(ABSERR.EQ.OFLOW) GO TO 115
      IF((IER+IERRO).EQ.0) GO TO 110
      IF(IERRO.EQ.3) ABSERR = ABSERR+CORREC
      IF(IER.EQ.0) IER = 3
      IF(RESULT.NE.0.0D+00.AND.AREA.NE.0.0D+00)GO TO 105
      IF(ABSERR.GT.ERRSUM)GO TO 115
      IF(AREA.EQ.0.0D+00) GO TO 130
      GO TO 110
  105 IF(ABSERR/ABS(RESULT).GT.ERRSUM/ABS(AREA))GO TO 115
!
!           TEST ON DIVERGENCE
!
  110 IF(KSGN.EQ.(-1).AND.MAX(ABS(RESULT),ABS(AREA)).LE.   &
       DEFABS*0.1D-01) GO TO 130
      IF(0.1D-01.GT.(RESULT/AREA).OR.(RESULT/AREA).GT.0.1D+03.OR.   &
        ERRSUM.GT.ABS(AREA)) IER = 6
      GO TO 130
!
!           COMPUTE GLOBAL INTEGRAL SUM.
!
  115 RESULT = 0.0D+00
      DO 120 K = 1,LAST
        RESULT = RESULT+RLIST(K)
  120 CONTINUE
      ABSERR = ERRSUM
  130 NEVAL = 30*LAST-15
      IF(INF.EQ.2) NEVAL = 2*NEVAL
      IF(IER.GT.2) IER=IER-1
  999 RETURN
      END SUBROUTINE DQAGIE
      SUBROUTINE DQELG(N,EPSTAB,RESULT,ABSERR,RES3LA,NRES)
!
!   ADDED TO DATAPLOT 12/2003.  THIS ROUTINE ADDED FOR INTERNAL
!   DATAPLOT USAGE.
!
!***BEGIN PROLOGUE  DQELG
!***REFER TO  DQAGIE,DQAGOE,DQAGPE,DQAGSE
!***ROUTINES CALLED  D1MACH
!***REVISION DATE  830518   (YYMMDD)
!***REVISION HISTORY (YYMMDD)
!   000601   Changed DMAX1/DABS to generic MAX/ABS
!***KEYWORDS  CONVERGENCE ACCELERATION,EPSILON ALGORITHM,EXTRAPOLATION
!***AUTHOR  PIESSENS, ROBERT, APPLIED MATH. AND PROGR. DIV. -
!             K. U. LEUVEN
!           DE DONCKER, ELISE, APPLIED MATH. AND PROGR. DIV. -
!             K. U. LEUVEN
!***PURPOSE  The routine determines the limit of a given sequence of
!            approximations, by means of the Epsilon algorithm of
!            P.Wynn. An estimate of the absolute error is also given.
!            The condensed Epsilon table is computed. Only those
!            elements needed for the computation of the next diagonal
!            are preserved.
!***DESCRIPTION
!
!           Epsilon algorithm
!           Standard fortran subroutine
!           Double precision version
!
!           PARAMETERS
!              N      - Integer
!                       EPSTAB(N) contains the new element in the
!                       first column of the epsilon table.
!
!              EPSTAB - Double precision
!                       Vector of dimension 52 containing the elements
!                       of the two lower diagonals of the triangular
!                       epsilon table. The elements are numbered
!                       starting at the right-hand corner of the
!                       triangle.
!
!              RESULT - Double precision
!                       Resulting approximation to the integral
!
!              ABSERR - Double precision
!                       Estimate of the absolute error computed from
!                       RESULT and the 3 previous results
!
!              RES3LA - Double precision
!                       Vector of dimension 3 containing the last 3
!                       results
!
!              NRES   - Integer
!                       Number of calls to the routine
!                       (should be zero at first call)
!***END PROLOGUE  DQELG
!
      DOUBLE PRECISION ABSERR,DELTA1,DELTA2,DELTA3,   &
        EPMACH,EPSINF,EPSTAB,ERROR,ERR1,ERR2,ERR3,E0,E1,E1ABS,E2,E3,   &
        OFLOW,RES,RESULT,RES3LA,SS,TOL1,TOL2,TOL3
      INTEGER I,IB,IB2,IE,INDX,K1,K2,K3,LIMEXP,N,NEWELM,NRES,NUM
      DIMENSION EPSTAB(52),RES3LA(3)
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!           LIST OF MAJOR VARIABLES
!           -----------------------
!
!           E0     - THE 4 ELEMENTS ON WHICH THE COMPUTATION OF A NEW
!           E1       ELEMENT IN THE EPSILON TABLE IS BASED
!           E2
!           E3                 E0
!                        E3    E1    NEW
!                              E2
!           NEWELM - NUMBER OF ELEMENTS TO BE COMPUTED IN THE NEW
!                    DIAGONAL
!           ERROR  - ERROR = ABS(E1-E0)+ABS(E2-E1)+ABS(NEW-E2)
!           RESULT - THE ELEMENT IN THE NEW DIAGONAL WITH LEAST VALUE
!                    OF ERROR
!
!           MACHINE DEPENDENT CONSTANTS
!           ---------------------------
!
!           EPMACH IS THE LARGEST RELATIVE SPACING.
!           OFLOW IS THE LARGEST POSITIVE MAGNITUDE.
!           LIMEXP IS THE MAXIMUM NUMBER OF ELEMENTS THE EPSILON
!           TABLE CAN CONTAIN. IF THIS NUMBER IS REACHED, THE UPPER
!           DIAGONAL OF THE EPSILON TABLE IS DELETED.
!
!***FIRST EXECUTABLE STATEMENT  DQELG
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
!
!           IF E0, E1 AND E2 ARE EQUAL TO WITHIN MACHINE
!           ACCURACY, CONVERGENCE IS ASSUMED.
!           RESULT = E2
!           ABSERR = ABS(E1-E0)+ABS(E2-E1)
!
        RESULT = RES
        ABSERR = ERR2+ERR3
! ***JUMP OUT OF DO-LOOP
        GO TO 100
   10   E3 = EPSTAB(K1)
        EPSTAB(K1) = E1
        DELTA1 = E1-E3
        ERR1 = ABS(DELTA1)
        TOL1 = MAX(E1ABS,ABS(E3))*EPMACH
!
!           IF TWO ELEMENTS ARE VERY CLOSE TO EACH OTHER, OMIT
!           A PART OF THE TABLE BY ADJUSTING THE VALUE OF N
!
        IF(ERR1.LE.TOL1.OR.ERR2.LE.TOL2.OR.ERR3.LE.TOL3) GO TO 20
        SS = 0.1D+01/DELTA1+0.1D+01/DELTA2-0.1D+01/DELTA3
        EPSINF = ABS(SS*E1)
!
!           TEST TO DETECT IRREGULAR BEHAVIOUR IN THE TABLE, AND
!           EVENTUALLY OMIT A PART OF THE TABLE ADJUSTING THE VALUE
!           OF N.
!
        IF(EPSINF.GT.0.1D-03) GO TO 30
   20   N = I+I-1
! ***JUMP OUT OF DO-LOOP
        GO TO 50
!
!           COMPUTE A NEW ELEMENT AND EVENTUALLY ADJUST
!           THE VALUE OF RESULT.
!
   30   RES = E1+0.1D+01/SS
        EPSTAB(K1) = RES
        K1 = K1-2
        ERROR = ERR2+ABS(RES-E2)+ERR3
        IF(ERROR.GT.ABSERR) GO TO 40
        ABSERR = ERROR
        RESULT = RES
   40 CONTINUE
!
!           SHIFT THE TABLE.
!
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
!
!           COMPUTE ERROR ESTIMATE
!
   90 ABSERR = ABS(RESULT-RES3LA(3))+ABS(RESULT-RES3LA(2))   &
        +ABS(RESULT-RES3LA(1))
      RES3LA(1) = RES3LA(2)
      RES3LA(2) = RES3LA(3)
      RES3LA(3) = RESULT
  100 ABSERR = MAX(ABSERR,0.5D+01*EPMACH*ABS(RESULT))
      RETURN
      END SUBROUTINE DQELG
      SUBROUTINE DQK15(F,A,B,RESULT,ABSERR,RESABS,RESASC)
!
!   ADDED TO DATAPLOT 12/2003.  THIS ROUTINE ADDED FOR INTERNAL
!   DATAPLOT USAGE.
!
!***BEGIN PROLOGUE  DQK15
!***DATE WRITTEN   800101   (YYMMDD)
!***REVISION DATE  830518   (YYMMDD)
!***REVISION HISTORY (YYMMDD)
!   000601   Changed DMAX1/DMIN1/DABS to generic MAX/MIN/ABS
!***CATEGORY NO.  H2A1A2
!***KEYWORDS  15-POINT GAUSS-KRONROD RULES
!***AUTHOR  PIESSENS, ROBERT, APPLIED MATH. AND PROGR. DIV. -
!             K. U. LEUVEN
!           DE DONCKER, ELISE, APPLIED MATH. AND PROGR. DIV. -
!             K. U. LEUVEN
!***PURPOSE  To compute I = Integral of F over (A,B), with error
!                           estimate
!                       J = integral of ABS(F) over (A,B)
!***DESCRIPTION
!
!           Integration rules
!           Standard fortran subroutine
!           Double precision version
!
!           PARAMETERS
!            ON ENTRY
!              F      - Double precision
!                       Function subprogram defining the integrand
!                       FUNCTION F(X). The actual name for F needs to be
!                       Declared E X T E R N A L in the calling program.
!
!              A      - Double precision
!                       Lower limit of integration
!
!              B      - Double precision
!                       Upper limit of integration
!
!            ON RETURN
!              RESULT - Double precision
!                       Approximation to the integral I
!                       Result is computed by applying the 15-POINT
!                       KRONROD RULE (RESK) obtained by optimal addition
!                       of abscissae to the7-POINT GAUSS RULE(RESG).
!
!              ABSERR - Double precision
!                       Estimate of the modulus of the absolute error,
!                       which should not exceed ABS(I-RESULT)
!
!              RESABS - Double precision
!                       Approximation to the integral J
!
!              RESASC - Double precision
!                       Approximation to the integral of ABS(F-I/(B-A))
!                       over (A,B)
!***REFERENCES  (NONE)
!***ROUTINES CALLED  D1MACH
!***END PROLOGUE  DQK15
!
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DOUBLE PRECISION A,ABSC,ABSERR,B,CENTR,DHLGTH,   &
        EPMACH,F,FC,FSUM,FVAL1,FVAL2,FV1,FV2,HLGTH,RESABS,RESASC,   &
        RESG,RESK,RESKH,RESULT,UFLOW,WG,WGK,XGK
      INTEGER J,JTW,JTWM1
      EXTERNAL F
!
      DIMENSION FV1(7),FV2(7),WG(4),WGK(8),XGK(8)
!
!           THE ABSCISSAE AND WEIGHTS ARE GIVEN FOR THE INTERVAL (-1,1).
!           BECAUSE OF SYMMETRY ONLY THE POSITIVE ABSCISSAE AND THEIR
!           CORRESPONDING WEIGHTS ARE GIVEN.
!
!           XGK    - ABSCISSAE OF THE 15-POINT KRONROD RULE
!                    XGK(2), XGK(4), ...  ABSCISSAE OF THE 7-POINT
!                    GAUSS RULE
!                    XGK(1), XGK(3), ...  ABSCISSAE WHICH ARE OPTIMALLY
!                    ADDED TO THE 7-POINT GAUSS RULE
!
!           WGK    - WEIGHTS OF THE 15-POINT KRONROD RULE
!
!           WG     - WEIGHTS OF THE 7-POINT GAUSS RULE
!
!
! GAUSS QUADRATURE WEIGHTS AND KRONRON QUADRATURE ABSCISSAE AND WEIGHTS
! AS EVALUATED WITH 80 DECIMAL DIGIT ARITHMETIC BY L. W. FULLERTON,
! BELL LABS, NOV. 1981.
!
      DATA WG  (  1) / 0.129484966168869693270611432679082D0 /
      DATA WG  (  2) / 0.279705391489276667901467771423780D0 /
      DATA WG  (  3) / 0.381830050505118944950369775488975D0 /
      DATA WG  (  4) / 0.417959183673469387755102040816327D0 /
!
      DATA XGK (  1) / 0.991455371120812639206854697526329D0 /
      DATA XGK (  2) / 0.949107912342758524526189684047851D0 /
      DATA XGK (  3) / 0.864864423359769072789712788640926D0 /
      DATA XGK (  4) / 0.741531185599394439863864773280788D0 /
      DATA XGK (  5) / 0.586087235467691130294144838258730D0 /
      DATA XGK (  6) / 0.405845151377397166906606412076961D0 /
      DATA XGK (  7) / 0.207784955007898467600689403773245D0 /
      DATA XGK (  8) / 0.000000000000000000000000000000000D0 /
!
      DATA WGK (  1) / 0.022935322010529224963732008058970D0 /
      DATA WGK (  2) / 0.063092092629978553290700663189204D0 /
      DATA WGK (  3) / 0.104790010322250183839876322541518D0 /
      DATA WGK (  4) / 0.140653259715525918745189590510238D0 /
      DATA WGK (  5) / 0.169004726639267902826583426598550D0 /
      DATA WGK (  6) / 0.190350578064785409913256402421014D0 /
      DATA WGK (  7) / 0.204432940075298892414161999234649D0 /
      DATA WGK (  8) / 0.209482141084727828012999174891714D0 /
!
!
!           LIST OF MAJOR VARIABLES
!           -----------------------
!
!           CENTR  - MID POINT OF THE INTERVAL
!           HLGTH  - HALF-LENGTH OF THE INTERVAL
!           ABSC   - ABSCISSA
!           FVAL*  - FUNCTION VALUE
!           RESG   - RESULT OF THE 7-POINT GAUSS FORMULA
!           RESK   - RESULT OF THE 15-POINT KRONROD FORMULA
!           RESKH  - APPROXIMATION TO THE MEAN VALUE OF F OVER (A,B),
!                    I.E. TO I/(B-A)
!
!           MACHINE DEPENDENT CONSTANTS
!           ---------------------------
!
!           EPMACH IS THE LARGEST RELATIVE SPACING.
!           UFLOW IS THE SMALLEST POSITIVE MAGNITUDE.
!
!***FIRST EXECUTABLE STATEMENT  DQK15
      EPMACH = D1MACH(4)
      UFLOW = D1MACH(1)
!
      CENTR = 0.5D+00*(A+B)
      HLGTH = 0.5D+00*(B-A)
      DHLGTH = ABS(HLGTH)
!
!           COMPUTE THE 15-POINT KRONROD APPROXIMATION TO
!           THE INTEGRAL, AND ESTIMATE THE ABSOLUTE ERROR.
!
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
      IF(RESASC.NE.0.0D+00.AND.ABSERR.NE.0.0D+00)   &
        ABSERR = RESASC*MIN(0.1D+01,(0.2D+03*ABSERR/RESASC)**1.5D+00)
      IF(RESABS.GT.UFLOW/(0.5D+02*EPMACH)) ABSERR = MAX   &
        ((EPMACH*0.5D+02)*RESABS,ABSERR)
      RETURN
      END SUBROUTINE DQK15
      SUBROUTINE DQK15I(F,BOUN,INF,A,B,RESULT,ABSERR,RESABS,RESASC)
!
!   ADDED TO DATAPLOT 12/2003.  THIS ROUTINE ADDED FOR INTERNAL
!   DATAPLOT USAGE.
!
!***BEGIN PROLOGUE  DQK15I
!***DATE WRITTEN   800101   (YYMMDD)
!***REVISION DATE  830518   (YYMMDD)
!***REVISION HISTORY (YYMMDD)
!   000601   Changed DMAX1/DMIN1/DABS to generic MAX/MIN/ABS
!***CATEGORY NO.  H2A3A2,H2A4A2
!***KEYWORDS  15-POINT TRANSFORMED GAUSS-KRONROD RULES
!***AUTHOR  PIESSENS, ROBERT, APPLIED MATH. AND PROGR. DIV. -
!             K. U. LEUVEN
!           DE DONCKER, ELISE, APPLIED MATH. AND PROGR. DIV. -
!             K. U. LEUVEN
!***PURPOSE  The original (infinite integration range is mapped
!            onto the interval (0,1) and (A,B) is a part of (0,1).
!            it is the purpose to compute
!            I = Integral of transformed integrand over (A,B),
!            J = Integral of ABS(Transformed Integrand) over (A,B).
!***DESCRIPTION
!
!           Integration Rule
!           Standard Fortran subroutine
!           Double precision version
!
!           PARAMETERS
!            ON ENTRY
!              F      - Double precision
!                       Fuction subprogram defining the integrand
!                       FUNCTION F(X). The actual name for F needs to be
!                       Declared E X T E R N A L in the calling program.
!
!              BOUN   - Double precision
!                       Finite bound of original integration
!                       Range (SET TO ZERO IF INF = +2)
!
!              INF    - Integer
!                       If INF = -1, the original interval is
!                                   (-INFINITY,BOUND),
!                       If INF = +1, the original interval is
!                                   (BOUND,+INFINITY),
!                       If INF = +2, the original interval is
!                                   (-INFINITY,+INFINITY) AND
!                       The integral is computed as the sum of two
!                       integrals, one over (-INFINITY,0) and one over
!                       (0,+INFINITY).
!
!              A      - Double precision
!                       Lower limit for integration over subrange
!                       of (0,1)
!
!              B      - Double precision
!                       Upper limit for integration over subrange
!                       of (0,1)
!
!            ON RETURN
!              RESULT - Double precision
!                       Approximation to the integral I
!                       Result is computed by applying the 15-POINT
!                       KRONROD RULE(RESK) obtained by optimal addition
!                       of abscissae to the 7-POINT GAUSS RULE(RESG).
!
!              ABSERR - Double precision
!                       Estimate of the modulus of the absolute error,
!                       WHICH SHOULD EQUAL or EXCEED ABS(I-RESULT)
!
!              RESABS - Double precision
!                       Approximation to the integral J
!
!              RESASC - Double precision
!                       Approximation to the integral of
!                       ABS((TRANSFORMED INTEGRAND)-I/(B-A)) over (A,B)
!***REFERENCES  (NONE)
!***ROUTINES CALLED  D1MACH
!***END PROLOGUE  DQK15I
!
      DOUBLE PRECISION A,ABSC,ABSC1,ABSC2,ABSERR,B,BOUN,CENTR,DINF,   &
        EPMACH,F,FC,FSUM,FVAL1,FVAL2,FV1,FV2,HLGTH,   &
        RESABS,RESASC,RESG,RESK,RESKH,RESULT,TABSC1,TABSC2,UFLOW,WG,WGK,   &
        XGK
      INTEGER INF,J
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      EXTERNAL F
!
      DIMENSION FV1(7),FV2(7),XGK(8),WGK(8),WG(8)
!
!           THE ABSCISSAE AND WEIGHTS ARE SUPPLIED FOR THE INTERVAL
!           (-1,1).  BECAUSE OF SYMMETRY ONLY THE POSITIVE ABSCISSAE AND
!           THEIR CORRESPONDING WEIGHTS ARE GIVEN.
!
!           XGK    - ABSCISSAE OF THE 15-POINT KRONROD RULE
!                    XGK(2), XGK(4), ... ABSCISSAE OF THE 7-POINT
!                    GAUSS RULE
!                    XGK(1), XGK(3), ...  ABSCISSAE WHICH ARE OPTIMALLY
!                    ADDED TO THE 7-POINT GAUSS RULE
!
!           WGK    - WEIGHTS OF THE 15-POINT KRONROD RULE
!
!           WG     - WEIGHTS OF THE 7-POINT GAUSS RULE, CORRESPONDING
!                    TO THE ABSCISSAE XGK(2), XGK(4), ...
!                    WG(1), WG(3), ... ARE SET TO ZERO.
!
      DATA XGK(1),XGK(2),XGK(3),XGK(4),XGK(5),XGK(6),XGK(7),XGK(8)/   &
           0.9914553711208126D+00,     0.9491079123427585D+00,   &
           0.8648644233597691D+00,     0.7415311855993944D+00,   &
           0.5860872354676911D+00,     0.4058451513773972D+00,   &
           0.2077849550078985D+00,     0.0000000000000000D+00/
!
      DATA WGK(1),WGK(2),WGK(3),WGK(4),WGK(5),WGK(6),WGK(7),WGK(8)/   &
           0.2293532201052922D-01,     0.6309209262997855D-01,   &
           0.1047900103222502D+00,     0.1406532597155259D+00,   &
           0.1690047266392679D+00,     0.1903505780647854D+00,   &
           0.2044329400752989D+00,     0.2094821410847278D+00/
!
      DATA WG(1),WG(2),WG(3),WG(4),WG(5),WG(6),WG(7),WG(8)/   &
           0.0000000000000000D+00,     0.1294849661688697D+00,   &
           0.0000000000000000D+00,     0.2797053914892767D+00,   &
           0.0000000000000000D+00,     0.3818300505051189D+00,   &
           0.0000000000000000D+00,     0.4179591836734694D+00/
!
!
!           LIST OF MAJOR VARIABLES
!           -----------------------
!
!           CENTR  - MID POINT OF THE INTERVAL
!           HLGTH  - HALF-LENGTH OF THE INTERVAL
!           ABSC*  - ABSCISSA
!           TABSC* - TRANSFORMED ABSCISSA
!           FVAL*  - FUNCTION VALUE
!           RESG   - RESULT OF THE 7-POINT GAUSS FORMULA
!           RESK   - RESULT OF THE 15-POINT KRONROD FORMULA
!           RESKH  - APPROXIMATION TO THE MEAN VALUE OF THE TRANSFORMED
!                    INTEGRAND OVER (A,B), I.E. TO I/(B-A)
!
!           MACHINE DEPENDENT CONSTANTS
!           ---------------------------
!
!           EPMACH IS THE LARGEST RELATIVE SPACING.
!           UFLOW IS THE SMALLEST POSITIVE MAGNITUDE.
!
!***FIRST EXECUTABLE STATEMENT  DQK15I
      EPMACH = D1MACH(4)
      UFLOW = D1MACH(1)
      DINF = MIN0(1,INF)
!
      CENTR = 0.5D+00*(A+B)
      HLGTH = 0.5D+00*(B-A)
      TABSC1 = BOUN+DINF*(0.1D+01-CENTR)/CENTR
      FVAL1 = F(TABSC1)
      IF(INF.EQ.2) FVAL1 = FVAL1+F(-TABSC1)
      FC = (FVAL1/CENTR)/CENTR
!
!           COMPUTE THE 15-POINT KRONROD APPROXIMATION TO
!           THE INTEGRAL, AND ESTIMATE THE ERROR.
!
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
      IF(RESASC.NE.0.0D+00.AND.ABSERR.NE.0.D0) ABSERR = RESASC*   &
       MIN(0.1D+01,(0.2D+03*ABSERR/RESASC)**1.5D+00)
      IF(RESABS.GT.UFLOW/(0.5D+02*EPMACH)) ABSERR = MAX   &
       ((EPMACH*0.5D+02)*RESABS,ABSERR)
      RETURN
      END SUBROUTINE DQK15I
      SUBROUTINE DQK21(F,A,B,RESULT,ABSERR,RESABS,RESASC)
!
!   ADDED TO DATAPLOT 12/2003.  THIS ROUTINE ADDED FOR INTERNAL
!   DATAPLOT USAGE.
!
!***BEGIN PROLOGUE  DQK21
!***DATE WRITTEN   800101   (YYMMDD)
!***REVISION DATE  830518   (YYMMDD)
!***REVISION HISTORY (YYMMDD)
!   000601   Changed DMAX1/DMIN1/DABS to generic MAX/MIN/ABS
!***CATEGORY NO.  H2A1A2
!***KEYWORDS  21-POINT GAUSS-KRONROD RULES
!***AUTHOR  PIESSENS, ROBERT, APPLIED MATH. AND PROGR. DIV. -
!             K. U. LEUVEN
!           DE DONCKER, ELISE, APPLIED MATH. AND PROGR. DIV. -
!             K. U. LEUVEN
!***PURPOSE  To compute I = Integral of F over (A,B), with error
!                           estimate
!                       J = Integral of ABS(F) over (A,B)
!***DESCRIPTION
!
!           Integration rules
!           Standard fortran subroutine
!           Double precision version
!
!           PARAMETERS
!            ON ENTRY
!              F      - Double precision
!                       Function subprogram defining the integrand
!                       FUNCTION F(X). The actual name for F needs to be
!                       Declared E X T E R N A L in the driver program.
!
!              A      - Double precision
!                       Lower limit of integration
!
!              B      - Double precision
!                       Upper limit of integration
!
!            ON RETURN
!              RESULT - Double precision
!                       Approximation to the integral I
!                       RESULT is computed by applying the 21-POINT
!                       KRONROD RULE (RESK) obtained by optimal addition
!                       of abscissae to the 10-POINT GAUSS RULE (RESG).
!
!              ABSERR - Double precision
!                       Estimate of the modulus of the absolute error,
!                       which should not exceed ABS(I-RESULT)
!
!              RESABS - Double precision
!                       Approximation to the integral J
!
!              RESASC - Double precision
!                       Approximation to the integral of ABS(F-I/(B-A))
!                       over (A,B)
!***REFERENCES  (NONE)
!***ROUTINES CALLED  D1MACH
!***END PROLOGUE  DQK21
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DOUBLE PRECISION A,ABSC,ABSERR,B,CENTR,DHLGTH,   &
        EPMACH,F,FC,FSUM,FVAL1,FVAL2,FV1,FV2,HLGTH,RESABS,RESASC,   &
        RESG,RESK,RESKH,RESULT,UFLOW,WG,WGK,XGK
      INTEGER J,JTW,JTWM1
      EXTERNAL F
!
      DIMENSION FV1(10),FV2(10),WG(5),WGK(11),XGK(11)
!
!           THE ABSCISSAE AND WEIGHTS ARE GIVEN FOR THE INTERVAL (-1,1).
!           BECAUSE OF SYMMETRY ONLY THE POSITIVE ABSCISSAE AND THEIR
!           CORRESPONDING WEIGHTS ARE GIVEN.
!
!           XGK    - ABSCISSAE OF THE 21-POINT KRONROD RULE
!                    XGK(2), XGK(4), ...  ABSCISSAE OF THE 10-POINT
!                    GAUSS RULE
!                    XGK(1), XGK(3), ...  ABSCISSAE WHICH ARE OPTIMALLY
!                    ADDED TO THE 10-POINT GAUSS RULE
!
!           WGK    - WEIGHTS OF THE 21-POINT KRONROD RULE
!
!           WG     - WEIGHTS OF THE 10-POINT GAUSS RULE
!
!
! GAUSS QUADRATURE WEIGHTS AND KRONRON QUADRATURE ABSCISSAE AND WEIGHTS
! AS EVALUATED WITH 80 DECIMAL DIGIT ARITHMETIC BY L. W. FULLERTON,
! BELL LABS, NOV. 1981.
!
      DATA WG  (  1) / 0.066671344308688137593568809893332D0 /
      DATA WG  (  2) / 0.149451349150580593145776339657697D0 /
      DATA WG  (  3) / 0.219086362515982043995534934228163D0 /
      DATA WG  (  4) / 0.269266719309996355091226921569469D0 /
      DATA WG  (  5) / 0.295524224714752870173892994651338D0 /
!
      DATA XGK (  1) / 0.995657163025808080735527280689003D0 /
      DATA XGK (  2) / 0.973906528517171720077964012084452D0 /
      DATA XGK (  3) / 0.930157491355708226001207180059508D0 /
      DATA XGK (  4) / 0.865063366688984510732096688423493D0 /
      DATA XGK (  5) / 0.780817726586416897063717578345042D0 /
      DATA XGK (  6) / 0.679409568299024406234327365114874D0 /
      DATA XGK (  7) / 0.562757134668604683339000099272694D0 /
      DATA XGK (  8) / 0.433395394129247190799265943165784D0 /
      DATA XGK (  9) / 0.294392862701460198131126603103866D0 /
      DATA XGK ( 10) / 0.148874338981631210884826001129720D0 /
      DATA XGK ( 11) / 0.000000000000000000000000000000000D0 /
!
      DATA WGK (  1) / 0.011694638867371874278064396062192D0 /
      DATA WGK (  2) / 0.032558162307964727478818972459390D0 /
      DATA WGK (  3) / 0.054755896574351996031381300244580D0 /
      DATA WGK (  4) / 0.075039674810919952767043140916190D0 /
      DATA WGK (  5) / 0.093125454583697605535065465083366D0 /
      DATA WGK (  6) / 0.109387158802297641899210590325805D0 /
      DATA WGK (  7) / 0.123491976262065851077958109831074D0 /
      DATA WGK (  8) / 0.134709217311473325928054001771707D0 /
      DATA WGK (  9) / 0.142775938577060080797094273138717D0 /
      DATA WGK ( 10) / 0.147739104901338491374841515972068D0 /
      DATA WGK ( 11) / 0.149445554002916905664936468389821D0 /
!
!
!           LIST OF MAJOR VARIABLES
!           -----------------------
!
!           CENTR  - MID POINT OF THE INTERVAL
!           HLGTH  - HALF-LENGTH OF THE INTERVAL
!           ABSC   - ABSCISSA
!           FVAL*  - FUNCTION VALUE
!           RESG   - RESULT OF THE 10-POINT GAUSS FORMULA
!           RESK   - RESULT OF THE 21-POINT KRONROD FORMULA
!           RESKH  - APPROXIMATION TO THE MEAN VALUE OF F OVER (A,B),
!                    I.E. TO I/(B-A)
!
!
!           MACHINE DEPENDENT CONSTANTS
!           ---------------------------
!
!           EPMACH IS THE LARGEST RELATIVE SPACING.
!           UFLOW IS THE SMALLEST POSITIVE MAGNITUDE.
!
!***FIRST EXECUTABLE STATEMENT  DQK21
      EPMACH = D1MACH(4)
      UFLOW = D1MACH(1)
!
      CENTR = 0.5D+00*(A+B)
      HLGTH = 0.5D+00*(B-A)
      DHLGTH = ABS(HLGTH)
!
!           COMPUTE THE 21-POINT KRONROD APPROXIMATION TO
!           THE INTEGRAL, AND ESTIMATE THE ABSOLUTE ERROR.
!
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
      IF(RESASC.NE.0.0D+00.AND.ABSERR.NE.0.0D+00)   &
        ABSERR = RESASC*MIN(0.1D+01,(0.2D+03*ABSERR/RESASC)**1.5D+00)
      IF(RESABS.GT.UFLOW/(0.5D+02*EPMACH)) ABSERR = MAX   &
        ((EPMACH*0.5D+02)*RESABS,ABSERR)
      RETURN
      END SUBROUTINE DQK21
      SUBROUTINE DQK31(F,A,B,RESULT,ABSERR,RESABS,RESASC)
!
!   ADDED TO DATAPLOT 12/2003.  THIS ROUTINE ADDED FOR INTERNAL
!   DATAPLOT USAGE.
!
!***BEGIN PROLOGUE  DQK31
!***DATE WRITTEN   800101   (YYMMDD)
!***REVISION DATE  830518   (YYMMDD)
!***REVISION HISTORY (YYMMDD)
!   000601   Changed DMAX1/DMIN1/DABS to generic MAX/MIN/ABS
!***CATEGORY NO.  H2A1A2
!***KEYWORDS  31-POINT GAUSS-KRONROD RULES
!***AUTHOR  PIESSENS, ROBERT, APPLIED MATH. AND PROGR. DIV. -
!             K. U. LEUVEN
!           DE DONCKER, ELISE, APPLIED MATH. AND PROGR. DIV. -
!             K. U. LEUVEN
!***PURPOSE  To compute I = Integral of F over (A,B) with error
!                           estimate
!                       J = Integral of ABS(F) over (A,B)
!***DESCRIPTION
!
!           Integration rules
!           Standard fortran subroutine
!           Double precision version
!
!           PARAMETERS
!            ON ENTRY
!              F      - Double precision
!                       Function subprogram defining the integrand
!                       FUNCTION F(X). The actual name for F needs to be
!                       Declared E X T E R N A L in the calling program.
!
!              A      - Double precision
!                       Lower limit of integration
!
!              B      - Double precision
!                       Upper limit of integration
!
!            ON RETURN
!              RESULT - Double precision
!                       Approximation to the integral I
!                       RESULT is computed by applying the 31-POINT
!                       GAUSS-KRONROD RULE (RESK), obtained by optimal
!                       addition of abscissae to the 15-POINT GAUSS
!                       RULE (RESG).
!
!              ABSERR - Double precison
!                       Estimate of the modulus of the modulus,
!                       which should not exceed ABS(I-RESULT)
!
!              RESABS - Double precision
!                       Approximation to the integral J
!
!              RESASC - Double precision
!                       Approximation to the integral of ABS(F-I/(B-A))
!                       over (A,B)
!***REFERENCES  (NONE)
!***ROUTINES CALLED  D1MACH
!***END PROLOGUE  DQK31
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DOUBLE PRECISION A,ABSC,ABSERR,B,CENTR,DHLGTH,   &
        EPMACH,F,FC,FSUM,FVAL1,FVAL2,FV1,FV2,HLGTH,RESABS,RESASC,   &
        RESG,RESK,RESKH,RESULT,UFLOW,WG,WGK,XGK
      INTEGER J,JTW,JTWM1
      EXTERNAL F
!
      DIMENSION FV1(15),FV2(15),XGK(16),WGK(16),WG(8)
!
!           THE ABSCISSAE AND WEIGHTS ARE GIVEN FOR THE INTERVAL (-1,1).
!           BECAUSE OF SYMMETRY ONLY THE POSITIVE ABSCISSAE AND THEIR
!           CORRESPONDING WEIGHTS ARE GIVEN.
!
!           XGK    - ABSCISSAE OF THE 31-POINT KRONROD RULE
!                    XGK(2), XGK(4), ...  ABSCISSAE OF THE 15-POINT
!                    GAUSS RULE
!                    XGK(1), XGK(3), ...  ABSCISSAE WHICH ARE OPTIMALLY
!                    ADDED TO THE 15-POINT GAUSS RULE
!
!           WGK    - WEIGHTS OF THE 31-POINT KRONROD RULE
!
!           WG     - WEIGHTS OF THE 15-POINT GAUSS RULE
!
!
! GAUSS QUADRATURE WEIGHTS AND KRONRON QUADRATURE ABSCISSAE AND WEIGHTS
! AS EVALUATED WITH 80 DECIMAL DIGIT ARITHMETIC BY L. W. FULLERTON,
! BELL LABS, NOV. 1981.
!
      DATA WG  (  1) / 0.030753241996117268354628393577204D0 /
      DATA WG  (  2) / 0.070366047488108124709267416450667D0 /
      DATA WG  (  3) / 0.107159220467171935011869546685869D0 /
      DATA WG  (  4) / 0.139570677926154314447804794511028D0 /
      DATA WG  (  5) / 0.166269205816993933553200860481209D0 /
      DATA WG  (  6) / 0.186161000015562211026800561866423D0 /
      DATA WG  (  7) / 0.198431485327111576456118326443839D0 /
      DATA WG  (  8) / 0.202578241925561272880620199967519D0 /
!
      DATA XGK (  1) / 0.998002298693397060285172840152271D0 /
      DATA XGK (  2) / 0.987992518020485428489565718586613D0 /
      DATA XGK (  3) / 0.967739075679139134257347978784337D0 /
      DATA XGK (  4) / 0.937273392400705904307758947710209D0 /
      DATA XGK (  5) / 0.897264532344081900882509656454496D0 /
      DATA XGK (  6) / 0.848206583410427216200648320774217D0 /
      DATA XGK (  7) / 0.790418501442465932967649294817947D0 /
      DATA XGK (  8) / 0.724417731360170047416186054613938D0 /
      DATA XGK (  9) / 0.650996741297416970533735895313275D0 /
      DATA XGK ( 10) / 0.570972172608538847537226737253911D0 /
      DATA XGK ( 11) / 0.485081863640239680693655740232351D0 /
      DATA XGK ( 12) / 0.394151347077563369897207370981045D0 /
      DATA XGK ( 13) / 0.299180007153168812166780024266389D0 /
      DATA XGK ( 14) / 0.201194093997434522300628303394596D0 /
      DATA XGK ( 15) / 0.101142066918717499027074231447392D0 /
      DATA XGK ( 16) / 0.000000000000000000000000000000000D0 /
!
      DATA WGK (  1) / 0.005377479872923348987792051430128D0 /
      DATA WGK (  2) / 0.015007947329316122538374763075807D0 /
      DATA WGK (  3) / 0.025460847326715320186874001019653D0 /
      DATA WGK (  4) / 0.035346360791375846222037948478360D0 /
      DATA WGK (  5) / 0.044589751324764876608227299373280D0 /
      DATA WGK (  6) / 0.053481524690928087265343147239430D0 /
      DATA WGK (  7) / 0.062009567800670640285139230960803D0 /
      DATA WGK (  8) / 0.069854121318728258709520077099147D0 /
      DATA WGK (  9) / 0.076849680757720378894432777482659D0 /
      DATA WGK ( 10) / 0.083080502823133021038289247286104D0 /
      DATA WGK ( 11) / 0.088564443056211770647275443693774D0 /
      DATA WGK ( 12) / 0.093126598170825321225486872747346D0 /
      DATA WGK ( 13) / 0.096642726983623678505179907627589D0 /
      DATA WGK ( 14) / 0.099173598721791959332393173484603D0 /
      DATA WGK ( 15) / 0.100769845523875595044946662617570D0 /
      DATA WGK ( 16) / 0.101330007014791549017374792767493D0 /
!
!
!           LIST OF MAJOR VARIABLES
!           -----------------------
!           CENTR  - MID POINT OF THE INTERVAL
!           HLGTH  - HALF-LENGTH OF THE INTERVAL
!           ABSC   - ABSCISSA
!           FVAL*  - FUNCTION VALUE
!           RESG   - RESULT OF THE 15-POINT GAUSS FORMULA
!           RESK   - RESULT OF THE 31-POINT KRONROD FORMULA
!           RESKH  - APPROXIMATION TO THE MEAN VALUE OF F OVER (A,B),
!                    I.E. TO I/(B-A)
!
!           MACHINE DEPENDENT CONSTANTS
!           ---------------------------
!           EPMACH IS THE LARGEST RELATIVE SPACING.
!           UFLOW IS THE SMALLEST POSITIVE MAGNITUDE.
!***FIRST EXECUTABLE STATEMENT  DQK31
      EPMACH = D1MACH(4)
      UFLOW = D1MACH(1)
!
      CENTR = 0.5D+00*(A+B)
      HLGTH = 0.5D+00*(B-A)
      DHLGTH = ABS(HLGTH)
!
!           COMPUTE THE 31-POINT KRONROD APPROXIMATION TO
!           THE INTEGRAL, AND ESTIMATE THE ABSOLUTE ERROR.
!
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
      IF(RESASC.NE.0.0D+00.AND.ABSERR.NE.0.0D+00)   &
        ABSERR = RESASC*MIN(0.1D+01,(0.2D+03*ABSERR/RESASC)**1.5D+00)
      IF(RESABS.GT.UFLOW/(0.5D+02*EPMACH)) ABSERR = MAX   &
        ((EPMACH*0.5D+02)*RESABS,ABSERR)
      RETURN
      END SUBROUTINE DQK31
      SUBROUTINE DQK41(F,A,B,RESULT,ABSERR,RESABS,RESASC)
!
!   ADDED TO DATAPLOT 12/2003.  THIS ROUTINE ADDED FOR INTERNAL
!   DATAPLOT USAGE.
!
!***BEGIN PROLOGUE  DQK41
!***DATE WRITTEN   800101   (YYMMDD)
!***REVISION DATE  830518   (YYMMDD)
!***REVISION HISTORY (YYMMDD)
!   000601   Changed DMAX1/DMIN1/DABS to generic MAX/MIN/ABS
!***CATEGORY NO.  H2A1A2
!***KEYWORDS  41-POINT GAUSS-KRONROD RULES
!***AUTHOR  PIESSENS, ROBERT, APPLIED MATH. AND PROGR. DIV. -
!             K. U. LEUVEN
!           DE DONCKER, ELISE, APPLIED MATH. AND PROGR. DIV. -
!             K. U. LEUVEN
!***PURPOSE  To compute I = Integral of F over (A,B), with error
!                           estimate
!                       J = Integral of ABS(F) over (A,B)
!***DESCRIPTION
!
!           Integration rules
!           Standard fortran subroutine
!           Double precision version
!
!           PARAMETERS
!            ON ENTRY
!              F      - Double precision
!                       Function subprogram defining the integrand
!                       FUNCTION F(X). The actual name for F needs to be
!                       declared E X T E R N A L in the calling program.
!
!              A      - Double precision
!                       Lower limit of integration
!
!              B      - Double precision
!                       Upper limit of integration
!
!            ON RETURN
!              RESULT - Double precision
!                       Approximation to the integral I
!                       RESULT is computed by applying the 41-POINT
!                       GAUSS-KRONROD RULE (RESK) obtained by optimal
!                       addition of abscissae to the 20-POINT GAUSS
!                       RULE (RESG).
!
!              ABSERR - Double precision
!                       Estimate of the modulus of the absolute error,
!                       which should not exceed ABS(I-RESULT)
!
!              RESABS - Double precision
!                       Approximation to the integral J
!
!              RESASC - Double precision
!                       Approximation to the integal of ABS(F-I/(B-A))
!                       over (A,B)
!***REFERENCES  (NONE)
!***ROUTINES CALLED  D1MACH
!***END PROLOGUE  DQK41
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DOUBLE PRECISION A,ABSC,ABSERR,B,CENTR,DHLGTH,   &
        EPMACH,F,FC,FSUM,FVAL1,FVAL2,FV1,FV2,HLGTH,RESABS,RESASC,   &
        RESG,RESK,RESKH,RESULT,UFLOW,WG,WGK,XGK
      INTEGER J,JTW,JTWM1
      EXTERNAL F
!
      DIMENSION FV1(20),FV2(20),XGK(21),WGK(21),WG(10)
!
!           THE ABSCISSAE AND WEIGHTS ARE GIVEN FOR THE INTERVAL (-1,1).
!           BECAUSE OF SYMMETRY ONLY THE POSITIVE ABSCISSAE AND THEIR
!           CORRESPONDING WEIGHTS ARE GIVEN.
!
!           XGK    - ABSCISSAE OF THE 41-POINT GAUSS-KRONROD RULE
!                    XGK(2), XGK(4), ...  ABSCISSAE OF THE 20-POINT
!                    GAUSS RULE
!                    XGK(1), XGK(3), ...  ABSCISSAE WHICH ARE OPTIMALLY
!                    ADDED TO THE 20-POINT GAUSS RULE
!
!           WGK    - WEIGHTS OF THE 41-POINT GAUSS-KRONROD RULE
!
!           WG     - WEIGHTS OF THE 20-POINT GAUSS RULE
!
!
! GAUSS QUADRATURE WEIGHTS AND KRONRON QUADRATURE ABSCISSAE AND WEIGHTS
! AS EVALUATED WITH 80 DECIMAL DIGIT ARITHMETIC BY L. W. FULLERTON,
! BELL LABS, NOV. 1981.
!
      DATA WG  (  1) / 0.017614007139152118311861962351853D0 /
      DATA WG  (  2) / 0.040601429800386941331039952274932D0 /
      DATA WG  (  3) / 0.062672048334109063569506535187042D0 /
      DATA WG  (  4) / 0.083276741576704748724758143222046D0 /
      DATA WG  (  5) / 0.101930119817240435036750135480350D0 /
      DATA WG  (  6) / 0.118194531961518417312377377711382D0 /
      DATA WG  (  7) / 0.131688638449176626898494499748163D0 /
      DATA WG  (  8) / 0.142096109318382051329298325067165D0 /
      DATA WG  (  9) / 0.149172986472603746787828737001969D0 /
      DATA WG  ( 10) / 0.152753387130725850698084331955098D0 /
!
      DATA XGK (  1) / 0.998859031588277663838315576545863D0 /
      DATA XGK (  2) / 0.993128599185094924786122388471320D0 /
      DATA XGK (  3) / 0.981507877450250259193342994720217D0 /
      DATA XGK (  4) / 0.963971927277913791267666131197277D0 /
      DATA XGK (  5) / 0.940822633831754753519982722212443D0 /
      DATA XGK (  6) / 0.912234428251325905867752441203298D0 /
      DATA XGK (  7) / 0.878276811252281976077442995113078D0 /
      DATA XGK (  8) / 0.839116971822218823394529061701521D0 /
      DATA XGK (  9) / 0.795041428837551198350638833272788D0 /
      DATA XGK ( 10) / 0.746331906460150792614305070355642D0 /
      DATA XGK ( 11) / 0.693237656334751384805490711845932D0 /
      DATA XGK ( 12) / 0.636053680726515025452836696226286D0 /
      DATA XGK ( 13) / 0.575140446819710315342946036586425D0 /
      DATA XGK ( 14) / 0.510867001950827098004364050955251D0 /
      DATA XGK ( 15) / 0.443593175238725103199992213492640D0 /
      DATA XGK ( 16) / 0.373706088715419560672548177024927D0 /
      DATA XGK ( 17) / 0.301627868114913004320555356858592D0 /
      DATA XGK ( 18) / 0.227785851141645078080496195368575D0 /
      DATA XGK ( 19) / 0.152605465240922675505220241022678D0 /
      DATA XGK ( 20) / 0.076526521133497333754640409398838D0 /
      DATA XGK ( 21) / 0.000000000000000000000000000000000D0 /
!
      DATA WGK (  1) / 0.003073583718520531501218293246031D0 /
      DATA WGK (  2) / 0.008600269855642942198661787950102D0 /
      DATA WGK (  3) / 0.014626169256971252983787960308868D0 /
      DATA WGK (  4) / 0.020388373461266523598010231432755D0 /
      DATA WGK (  5) / 0.025882133604951158834505067096153D0 /
      DATA WGK (  6) / 0.031287306777032798958543119323801D0 /
      DATA WGK (  7) / 0.036600169758200798030557240707211D0 /
      DATA WGK (  8) / 0.041668873327973686263788305936895D0 /
      DATA WGK (  9) / 0.046434821867497674720231880926108D0 /
      DATA WGK ( 10) / 0.050944573923728691932707670050345D0 /
      DATA WGK ( 11) / 0.055195105348285994744832372419777D0 /
      DATA WGK ( 12) / 0.059111400880639572374967220648594D0 /
      DATA WGK ( 13) / 0.062653237554781168025870122174255D0 /
      DATA WGK ( 14) / 0.065834597133618422111563556969398D0 /
      DATA WGK ( 15) / 0.068648672928521619345623411885368D0 /
      DATA WGK ( 16) / 0.071054423553444068305790361723210D0 /
      DATA WGK ( 17) / 0.073030690332786667495189417658913D0 /
      DATA WGK ( 18) / 0.074582875400499188986581418362488D0 /
      DATA WGK ( 19) / 0.075704497684556674659542775376617D0 /
      DATA WGK ( 20) / 0.076377867672080736705502835038061D0 /
      DATA WGK ( 21) / 0.076600711917999656445049901530102D0 /
!
!
!           LIST OF MAJOR VARIABLES
!           -----------------------
!
!           CENTR  - MID POINT OF THE INTERVAL
!           HLGTH  - HALF-LENGTH OF THE INTERVAL
!           ABSC   - ABSCISSA
!           FVAL*  - FUNCTION VALUE
!           RESG   - RESULT OF THE 20-POINT GAUSS FORMULA
!           RESK   - RESULT OF THE 41-POINT KRONROD FORMULA
!           RESKH  - APPROXIMATION TO MEAN VALUE OF F OVER (A,B), I.E.
!                    TO I/(B-A)
!
!           MACHINE DEPENDENT CONSTANTS
!           ---------------------------
!
!           EPMACH IS THE LARGEST RELATIVE SPACING.
!           UFLOW IS THE SMALLEST POSITIVE MAGNITUDE.
!
!***FIRST EXECUTABLE STATEMENT  DQK41
      EPMACH = D1MACH(4)
      UFLOW = D1MACH(1)
!
      CENTR = 0.5D+00*(A+B)
      HLGTH = 0.5D+00*(B-A)
      DHLGTH = ABS(HLGTH)
!
!           COMPUTE THE 41-POINT GAUSS-KRONROD APPROXIMATION TO
!           THE INTEGRAL, AND ESTIMATE THE ABSOLUTE ERROR.
!
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
      IF(RESASC.NE.0.0D+00.AND.ABSERR.NE.0.D+00)   &
        ABSERR = RESASC*MIN(0.1D+01,(0.2D+03*ABSERR/RESASC)**1.5D+00)
      IF(RESABS.GT.UFLOW/(0.5D+02*EPMACH)) ABSERR = MAX   &
        ((EPMACH*0.5D+02)*RESABS,ABSERR)
      RETURN
      END SUBROUTINE DQK41
      SUBROUTINE DQK51(F,A,B,RESULT,ABSERR,RESABS,RESASC)
!
!   ADDED TO DATAPLOT 12/2003.  THIS ROUTINE ADDED FOR INTERNAL
!   DATAPLOT USAGE.
!
!***BEGIN PROLOGUE  DQK51
!***DATE WRITTEN   800101   (YYMMDD)
!***REVISION DATE  830518   (YYMMDD)
!                  960627   Missing WGK(26) restored (RFB).
!                  000601   Changed DMAX1/DMIN1/DABS to generic MAX/MIN/ABS
!***CATEGORY NO.  H2A1A2
!***KEYWORDS  51-POINT GAUSS-KRONROD RULES
!***AUTHOR  PIESSENS, ROBERT, APPLIED MATH. AND PROGR. DIV. -
!             K. U. LEUVEN
!           DE DONCKER, ELISE, APPLIED MATH. AND PROGR. DIV. -
!             K. U. LEUVEN
!***PURPOSE  To compute I = Integral of F over (A,B) with error
!                           estimate
!                       J = Integral of ABS(F) over (A,B)
!***DESCRIPTION
!
!           Integration rules
!           Standard fortran subroutine
!           Double precision version
!
!           PARAMETERS
!            ON ENTRY
!              F      - Double precision
!                       Function subroutine defining the integrand
!                       function F(X). The actual name for F needs to be
!                       declared E X T E R N A L in the calling program.
!
!              A      - Double precision
!                       Lower limit of integration
!
!              B      - Double precision
!                       Upper limit of integration
!
!            ON RETURN
!              RESULT - Double precision
!                       Approximation to the integral I
!                       RESULT is computed by applying the 51-point
!                       Kronrod rule (RESK) obtained by optimal addition
!                       of abscissae to the 25-point Gauss rule (RESG).
!
!              ABSERR - Double precision
!                       Estimate of the modulus of the absolute error,
!                       which should not exceed ABS(I-RESULT)
!
!              RESABS - Double precision
!                       Approximation to the integral J
!
!              RESASC - Double precision
!                       Approximation to the integral of ABS(F-I/(B-A))
!                       over (A,B)
!***REFERENCES  (NONE)
!***ROUTINES CALLED  D1MACH
!***END PROLOGUE  DQK51
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DOUBLE PRECISION A,ABSC,ABSERR,B,CENTR,DHLGTH,   &
        EPMACH,F,FC,FSUM,FVAL1,FVAL2,FV1,FV2,HLGTH,RESABS,RESASC,   &
        RESG,RESK,RESKH,RESULT,UFLOW,WG,WGK,XGK
      INTEGER J,JTW,JTWM1
      EXTERNAL F
!
      DIMENSION FV1(25),FV2(25),XGK(26),WGK(26),WG(13)
!
!           THE ABSCISSAE AND WEIGHTS ARE GIVEN FOR THE INTERVAL (-1,1).
!           BECAUSE OF SYMMETRY ONLY THE POSITIVE ABSCISSAE AND THEIR
!           CORRESPONDING WEIGHTS ARE GIVEN.
!
!           XGK    - ABSCISSAE OF THE 51-POINT KRONROD RULE
!                    XGK(2), XGK(4), ...  ABSCISSAE OF THE 25-POINT
!                    GAUSS RULE
!                    XGK(1), XGK(3), ...  ABSCISSAE WHICH ARE OPTIMALLY
!                    ADDED TO THE 25-POINT GAUSS RULE
!
!           WGK    - WEIGHTS OF THE 51-POINT KRONROD RULE
!
!           WG     - WEIGHTS OF THE 25-POINT GAUSS RULE
!
!
! GAUSS QUADRATURE WEIGHTS AND KRONRON QUADRATURE ABSCISSAE AND WEIGHTS
! AS EVALUATED WITH 80 DECIMAL DIGIT ARITHMETIC BY L. W. FULLERTON,
! BELL LABS, NOV. 1981.
!
      DATA WG  (  1) / 0.011393798501026287947902964113235D0 /
      DATA WG  (  2) / 0.026354986615032137261901815295299D0 /
      DATA WG  (  3) / 0.040939156701306312655623487711646D0 /
      DATA WG  (  4) / 0.054904695975835191925936891540473D0 /
      DATA WG  (  5) / 0.068038333812356917207187185656708D0 /
      DATA WG  (  6) / 0.080140700335001018013234959669111D0 /
      DATA WG  (  7) / 0.091028261982963649811497220702892D0 /
      DATA WG  (  8) / 0.100535949067050644202206890392686D0 /
      DATA WG  (  9) / 0.108519624474263653116093957050117D0 /
      DATA WG  ( 10) / 0.114858259145711648339325545869556D0 /
      DATA WG  ( 11) / 0.119455763535784772228178126512901D0 /
      DATA WG  ( 12) / 0.122242442990310041688959518945852D0 /
      DATA WG  ( 13) / 0.123176053726715451203902873079050D0 /
!
      DATA XGK (  1) / 0.999262104992609834193457486540341D0 /
      DATA XGK (  2) / 0.995556969790498097908784946893902D0 /
      DATA XGK (  3) / 0.988035794534077247637331014577406D0 /
      DATA XGK (  4) / 0.976663921459517511498315386479594D0 /
      DATA XGK (  5) / 0.961614986425842512418130033660167D0 /
      DATA XGK (  6) / 0.942974571228974339414011169658471D0 /
      DATA XGK (  7) / 0.920747115281701561746346084546331D0 /
      DATA XGK (  8) / 0.894991997878275368851042006782805D0 /
      DATA XGK (  9) / 0.865847065293275595448996969588340D0 /
      DATA XGK ( 10) / 0.833442628760834001421021108693570D0 /
      DATA XGK ( 11) / 0.797873797998500059410410904994307D0 /
      DATA XGK ( 12) / 0.759259263037357630577282865204361D0 /
      DATA XGK ( 13) / 0.717766406813084388186654079773298D0 /
      DATA XGK ( 14) / 0.673566368473468364485120633247622D0 /
      DATA XGK ( 15) / 0.626810099010317412788122681624518D0 /
      DATA XGK ( 16) / 0.577662930241222967723689841612654D0 /
      DATA XGK ( 17) / 0.526325284334719182599623778158010D0 /
      DATA XGK ( 18) / 0.473002731445714960522182115009192D0 /
      DATA XGK ( 19) / 0.417885382193037748851814394594572D0 /
      DATA XGK ( 20) / 0.361172305809387837735821730127641D0 /
      DATA XGK ( 21) / 0.303089538931107830167478909980339D0 /
      DATA XGK ( 22) / 0.243866883720988432045190362797452D0 /
      DATA XGK ( 23) / 0.183718939421048892015969888759528D0 /
      DATA XGK ( 24) / 0.122864692610710396387359818808037D0 /
      DATA XGK ( 25) / 0.061544483005685078886546392366797D0 /
      DATA XGK ( 26) / 0.000000000000000000000000000000000D0 /
!
      DATA WGK (  1) / 0.001987383892330315926507851882843D0 /
      DATA WGK (  2) / 0.005561932135356713758040236901066D0 /
      DATA WGK (  3) / 0.009473973386174151607207710523655D0 /
      DATA WGK (  4) / 0.013236229195571674813656405846976D0 /
      DATA WGK (  5) / 0.016847817709128298231516667536336D0 /
      DATA WGK (  6) / 0.020435371145882835456568292235939D0 /
      DATA WGK (  7) / 0.024009945606953216220092489164881D0 /
      DATA WGK (  8) / 0.027475317587851737802948455517811D0 /
      DATA WGK (  9) / 0.030792300167387488891109020215229D0 /
      DATA WGK ( 10) / 0.034002130274329337836748795229551D0 /
      DATA WGK ( 11) / 0.037116271483415543560330625367620D0 /
      DATA WGK ( 12) / 0.040083825504032382074839284467076D0 /
      DATA WGK ( 13) / 0.042872845020170049476895792439495D0 /
      DATA WGK ( 14) / 0.045502913049921788909870584752660D0 /
      DATA WGK ( 15) / 0.047982537138836713906392255756915D0 /
      DATA WGK ( 16) / 0.050277679080715671963325259433440D0 /
      DATA WGK ( 17) / 0.052362885806407475864366712137873D0 /
      DATA WGK ( 18) / 0.054251129888545490144543370459876D0 /
      DATA WGK ( 19) / 0.055950811220412317308240686382747D0 /
      DATA WGK ( 20) / 0.057437116361567832853582693939506D0 /
      DATA WGK ( 21) / 0.058689680022394207961974175856788D0 /
      DATA WGK ( 22) / 0.059720340324174059979099291932562D0 /
      DATA WGK ( 23) / 0.060539455376045862945360267517565D0 /
      DATA WGK ( 24) / 0.061128509717053048305859030416293D0 /
      DATA WGK ( 25) / 0.061471189871425316661544131965264D0 /
      DATA WGK ( 26) / 0.061580818067832935078759824240055D0 /
!
!
!           LIST OF MAJOR VARIABLES
!           -----------------------
!
!           CENTR  - MID POINT OF THE INTERVAL
!           HLGTH  - HALF-LENGTH OF THE INTERVAL
!           ABSC   - ABSCISSA
!           FVAL*  - FUNCTION VALUE
!           RESG   - RESULT OF THE 25-POINT GAUSS FORMULA
!           RESK   - RESULT OF THE 51-POINT KRONROD FORMULA
!           RESKH  - APPROXIMATION TO THE MEAN VALUE OF F OVER (A,B),
!                    I.E. TO I/(B-A)
!
!           MACHINE DEPENDENT CONSTANTS
!           ---------------------------
!
!           EPMACH IS THE LARGEST RELATIVE SPACING.
!           UFLOW IS THE SMALLEST POSITIVE MAGNITUDE.
!
!***FIRST EXECUTABLE STATEMENT  DQK51
      EPMACH = D1MACH(4)
      UFLOW = D1MACH(1)
!
      CENTR = 0.5D+00*(A+B)
      HLGTH = 0.5D+00*(B-A)
      DHLGTH = ABS(HLGTH)
!
!           COMPUTE THE 51-POINT KRONROD APPROXIMATION TO
!           THE INTEGRAL, AND ESTIMATE THE ABSOLUTE ERROR.
!
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
      IF(RESASC.NE.0.0D+00.AND.ABSERR.NE.0.0D+00)   &
        ABSERR = RESASC*MIN(0.1D+01,(0.2D+03*ABSERR/RESASC)**1.5D+00)
      IF(RESABS.GT.UFLOW/(0.5D+02*EPMACH)) ABSERR = MAX   &
        ((EPMACH*0.5D+02)*RESABS,ABSERR)
      RETURN
      END SUBROUTINE DQK51
      SUBROUTINE DQK61(F,A,B,RESULT,ABSERR,RESABS,RESASC)
!
!   ADDED TO DATAPLOT 12/2003.  THIS ROUTINE ADDED FOR INTERNAL
!   DATAPLOT USAGE.
!
!***BEGIN PROLOGUE  DQK61
!***DATE WRITTEN   800101   (YYMMDD)
!***REVISION DATE  830518   (YYMMDD)
!***REVISION HISTORY (YYMMDD)
!   000601   Changed DMAX1/DMIN1/DABS to generic MAX/MIN/ABS
!***CATEGORY NO.  H2A1A2
!***KEYWORDS  61-POINT GAUSS-KRONROD RULES
!***AUTHOR  PIESSENS, ROBERT, APPLIED MATH. AND PROGR. DIV. -
!             K. U. LEUVEN
!           DE DONCKER, ELISE, APPLIED MATH. AND PROGR. DIV. -
!             K. U. LEUVEN
!***PURPOSE  To compute I = Integral of F over (A,B) with error
!                           estimate
!                       J = Integral of DABS(F) over (A,B)
!***DESCRIPTION
!
!        Integration rule
!        Standard fortran subroutine
!        Double precision version
!
!
!        PARAMETERS
!         ON ENTRY
!           F      - Double precision
!                    Function subprogram defining the integrand
!                    function F(X). The actual name for F needs to be
!                    declared E X T E R N A L in the calling program.
!
!           A      - Double precision
!                    Lower limit of integration
!
!           B      - Double precision
!                    Upper limit of integration
!
!         ON RETURN
!           RESULT - Double precision
!                    Approximation to the integral I
!                    RESULT is computed by applying the 61-point
!                    Kronrod rule (RESK) obtained by optimal addition of
!                    abscissae to the 30-point Gauss rule (RESG).
!
!           ABSERR - Double precision
!                    Estimate of the modulus of the absolute error,
!                    which should equal or exceed DABS(I-RESULT)
!
!           RESABS - Double precision
!                    Approximation to the integral J
!
!           RESASC - Double precision
!                    Approximation to the integral of DABS(F-I/(B-A))
!***REFERENCES  (NONE)
!***ROUTINES CALLED  D1MACH
!***END PROLOGUE  DQK61
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!CCCC DOUBLE PRECISION A,DABSC,ABSERR,B,CENTR,ABS,DHLGTH,
      DOUBLE PRECISION A,DABSC,ABSERR,B,CENTR,DHLGTH,   &
        EPMACH,F,FC,FSUM,FVAL1,FVAL2,FV1,FV2,HLGTH,RESABS,RESASC,   &
        RESG,RESK,RESKH,RESULT,UFLOW,WG,WGK,XGK
      INTEGER J,JTW,JTWM1
      EXTERNAL F
!
      DIMENSION FV1(30),FV2(30),XGK(31),WGK(31),WG(15)
!
!           THE ABSCISSAE AND WEIGHTS ARE GIVEN FOR THE
!           INTERVAL (-1,1). BECAUSE OF SYMMETRY ONLY THE POSITIVE
!           ABSCISSAE AND THEIR CORRESPONDING WEIGHTS ARE GIVEN.
!
!           XGK   - ABSCISSAE OF THE 61-POINT KRONROD RULE
!                   XGK(2), XGK(4)  ... ABSCISSAE OF THE 30-POINT
!                   GAUSS RULE
!                   XGK(1), XGK(3)  ... OPTIMALLY ADDED ABSCISSAE
!                   TO THE 30-POINT GAUSS RULE
!
!           WGK   - WEIGHTS OF THE 61-POINT KRONROD RULE
!
!           WG    - WEIGTHS OF THE 30-POINT GAUSS RULE
!
!
! GAUSS QUADRATURE WEIGHTS AND KRONRON QUADRATURE ABSCISSAE AND WEIGHTS
! AS EVALUATED WITH 80 DECIMAL DIGIT ARITHMETIC BY L. W. FULLERTON,
! BELL LABS, NOV. 1981.
!
      DATA WG  (  1) / 0.007968192496166605615465883474674D0 /
      DATA WG  (  2) / 0.018466468311090959142302131912047D0 /
      DATA WG  (  3) / 0.028784707883323369349719179611292D0 /
      DATA WG  (  4) / 0.038799192569627049596801936446348D0 /
      DATA WG  (  5) / 0.048402672830594052902938140422808D0 /
      DATA WG  (  6) / 0.057493156217619066481721689402056D0 /
      DATA WG  (  7) / 0.065974229882180495128128515115962D0 /
      DATA WG  (  8) / 0.073755974737705206268243850022191D0 /
      DATA WG  (  9) / 0.080755895229420215354694938460530D0 /
      DATA WG  ( 10) / 0.086899787201082979802387530715126D0 /
      DATA WG  ( 11) / 0.092122522237786128717632707087619D0 /
      DATA WG  ( 12) / 0.096368737174644259639468626351810D0 /
      DATA WG  ( 13) / 0.099593420586795267062780282103569D0 /
      DATA WG  ( 14) / 0.101762389748405504596428952168554D0 /
      DATA WG  ( 15) / 0.102852652893558840341285636705415D0 /
!
      DATA XGK (  1) / 0.999484410050490637571325895705811D0 /
      DATA XGK (  2) / 0.996893484074649540271630050918695D0 /
      DATA XGK (  3) / 0.991630996870404594858628366109486D0 /
      DATA XGK (  4) / 0.983668123279747209970032581605663D0 /
      DATA XGK (  5) / 0.973116322501126268374693868423707D0 /
      DATA XGK (  6) / 0.960021864968307512216871025581798D0 /
      DATA XGK (  7) / 0.944374444748559979415831324037439D0 /
      DATA XGK (  8) / 0.926200047429274325879324277080474D0 /
      DATA XGK (  9) / 0.905573307699907798546522558925958D0 /
      DATA XGK ( 10) / 0.882560535792052681543116462530226D0 /
      DATA XGK ( 11) / 0.857205233546061098958658510658944D0 /
      DATA XGK ( 12) / 0.829565762382768397442898119732502D0 /
      DATA XGK ( 13) / 0.799727835821839083013668942322683D0 /
      DATA XGK ( 14) / 0.767777432104826194917977340974503D0 /
      DATA XGK ( 15) / 0.733790062453226804726171131369528D0 /
      DATA XGK ( 16) / 0.697850494793315796932292388026640D0 /
      DATA XGK ( 17) / 0.660061064126626961370053668149271D0 /
      DATA XGK ( 18) / 0.620526182989242861140477556431189D0 /
      DATA XGK ( 19) / 0.579345235826361691756024932172540D0 /
      DATA XGK ( 20) / 0.536624148142019899264169793311073D0 /
      DATA XGK ( 21) / 0.492480467861778574993693061207709D0 /
      DATA XGK ( 22) / 0.447033769538089176780609900322854D0 /
      DATA XGK ( 23) / 0.400401254830394392535476211542661D0 /
      DATA XGK ( 24) / 0.352704725530878113471037207089374D0 /
      DATA XGK ( 25) / 0.304073202273625077372677107199257D0 /
      DATA XGK ( 26) / 0.254636926167889846439805129817805D0 /
      DATA XGK ( 27) / 0.204525116682309891438957671002025D0 /
      DATA XGK ( 28) / 0.153869913608583546963794672743256D0 /
      DATA XGK ( 29) / 0.102806937966737030147096751318001D0 /
      DATA XGK ( 30) / 0.051471842555317695833025213166723D0 /
      DATA XGK ( 31) / 0.000000000000000000000000000000000D0 /
!
      DATA WGK (  1) / 0.001389013698677007624551591226760D0 /
      DATA WGK (  2) / 0.003890461127099884051267201844516D0 /
      DATA WGK (  3) / 0.006630703915931292173319826369750D0 /
      DATA WGK (  4) / 0.009273279659517763428441146892024D0 /
      DATA WGK (  5) / 0.011823015253496341742232898853251D0 /
      DATA WGK (  6) / 0.014369729507045804812451432443580D0 /
      DATA WGK (  7) / 0.016920889189053272627572289420322D0 /
      DATA WGK (  8) / 0.019414141193942381173408951050128D0 /
      DATA WGK (  9) / 0.021828035821609192297167485738339D0 /
      DATA WGK ( 10) / 0.024191162078080601365686370725232D0 /
      DATA WGK ( 11) / 0.026509954882333101610601709335075D0 /
      DATA WGK ( 12) / 0.028754048765041292843978785354334D0 /
      DATA WGK ( 13) / 0.03090725756238776247884252943092D0 /
      DATA WGK ( 14) / 0.032981447057483726031814191016854D0 /
      DATA WGK ( 15) / 0.034979338028060024137499670731468D0 /
      DATA WGK ( 16) / 0.036882364651821229223911065617136D0 /
      DATA WGK ( 17) / 0.038678945624727592950348651532281D0 /
      DATA WGK ( 18) / 0.040374538951535959111995279752468D0 /
      DATA WGK ( 19) / 0.041969810215164246147147541285970D0 /
      DATA WGK ( 20) / 0.043452539701356069316831728117073D0 /
      DATA WGK ( 21) / 0.044814800133162663192355551616723D0 /
      DATA WGK ( 22) / 0.046059238271006988116271735559374D0 /
      DATA WGK ( 23) / 0.047185546569299153945261478181099D0 /
      DATA WGK ( 24) / 0.048185861757087129140779492298305D0 /
      DATA WGK ( 25) / 0.049055434555029778887528165367238D0 /
      DATA WGK ( 26) / 0.049795683427074206357811569379942D0 /
      DATA WGK ( 27) / 0.050405921402782346840893085653585D0 /
      DATA WGK ( 28) / 0.050881795898749606492297473049805D0 /
      DATA WGK ( 29) / 0.051221547849258772170656282604944D0 /
      DATA WGK ( 30) / 0.051426128537459025933862879215781D0 /
      DATA WGK ( 31) / 0.051494729429451567558340433647099D0 /
!
!           LIST OF MAJOR VARIABLES
!           -----------------------
!
!           CENTR  - MID POINT OF THE INTERVAL
!           HLGTH  - HALF-LENGTH OF THE INTERVAL
!           DABSC  - ABSCISSA
!           FVAL*  - FUNCTION VALUE
!           RESG   - RESULT OF THE 30-POINT GAUSS RULE
!           RESK   - RESULT OF THE 61-POINT KRONROD RULE
!           RESKH  - APPROXIMATION TO THE MEAN VALUE OF F
!                    OVER (A,B), I.E. TO I/(B-A)
!
!           MACHINE DEPENDENT CONSTANTS
!           ---------------------------
!
!           EPMACH IS THE LARGEST RELATIVE SPACING.
!           UFLOW IS THE SMALLEST POSITIVE MAGNITUDE.
!
!***FIRST EXECUTABLE STATEMENT  DQK61
      EPMACH = D1MACH(4)
      UFLOW = D1MACH(1)
!
      CENTR = 0.5D+00*(B+A)
      HLGTH = 0.5D+00*(B-A)
      DHLGTH = DABS(HLGTH)
!
!           COMPUTE THE 61-POINT KRONROD APPROXIMATION TO THE
!           INTEGRAL, AND ESTIMATE THE ABSOLUTE ERROR.
!
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
      IF(RESASC.NE.0.0D+00.AND.ABSERR.NE.0.0D+00)   &
        ABSERR = RESASC*MIN(0.1D+01,(0.2D+03*ABSERR/RESASC)**1.5D+00)
      IF(RESABS.GT.UFLOW/(0.5D+02*EPMACH)) ABSERR = MAX   &
        ((EPMACH*0.5D+02)*RESABS,ABSERR)
      RETURN
      END SUBROUTINE DQK61
      SUBROUTINE DQPSRT(LIMIT,LAST,MAXERR,ERMAX,ELIST,IORD,NRMAX)
!
!   ADDED TO DATAPLOT 12/2003.  THIS ROUTINE ADDED FOR INTERNAL
!   DATAPLOT USAGE.
!
!***BEGIN PROLOGUE  DQPSRT
!***REFER TO  DQAGE,DQAGIE,DQAGPE,DQAWSE
!***ROUTINES CALLED  (NONE)
!***REVISION DATE  810101   (YYMMDD)
!***KEYWORDS  SEQUENTIAL SORTING
!***AUTHOR  PIESSENS, ROBERT, APPLIED MATH. AND PROGR. DIV. -
!             K. U. LEUVEN
!           DE DONCKER, ELISE, APPLIED MATH. AND PROGR. DIV. -
!             K. U. LEUVEN
!***PURPOSE  This routine maintains the descending ordering in the
!            list of the local error estimated resulting from the
!            interval subdivision process. At each call two error
!            estimates are inserted using the sequential search
!            method, top-down for the largest error estimate and
!            bottom-up for the smallest error estimate.
!***DESCRIPTION
!
!           Ordering routine
!           Standard fortran subroutine
!           Double precision version
!
!           PARAMETERS (MEANING AT OUTPUT)
!              LIMIT  - Integer
!                       Maximum number of error estimates the list
!                       can contain
!
!              LAST   - Integer
!                       Number of error estimates currently in the list
!
!              MAXERR - Integer
!                       Maxerr points to the NRMAX-th largest error
!                       estimate currently in the list
!
!              ERMAX  - Double precision
!                       NRMAX-th largest error estimate
!                       ERMAX = ELIST(MAXERR)
!
!              ELIST  - Double precision
!                       Vector of dimension LAST containing
!                       the error estimates
!
!              IORD   - Integer
!                       Vector of dimension LAST, the first K elements
!                       of which contain pointers to the error
!                       estimates, such that
!                       ELIST(IORD(1)),...,  ELIST(IORD(K))
!                       form a decreasing sequence, with
!                       K = LAST if LAST.LE.(LIMIT/2+2), and
!                       K = LIMIT+1-LAST otherwise
!
!              NRMAX  - Integer
!                       MAXERR = IORD(NRMAX)
!***END PROLOGUE  DQPSRT
!
      DOUBLE PRECISION ELIST,ERMAX,ERRMAX,ERRMIN
      INTEGER I,IBEG,IDO,IORD,ISUCC,J,JBND,JUPBN,K,LAST,LIMIT,MAXERR,   &
        NRMAX
      DIMENSION ELIST(LAST),IORD(LAST)
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!           CHECK WHETHER THE LIST CONTAINS MORE THAN
!           TWO ERROR ESTIMATES.
!
!***FIRST EXECUTABLE STATEMENT  DQPSRT
      IF(LAST.GT.2) GO TO 10
      IORD(1) = 1
      IORD(2) = 2
      GO TO 90
!
!           THIS PART OF THE ROUTINE IS ONLY EXECUTED IF, DUE TO A
!           DIFFICULT INTEGRAND, SUBDIVISION INCREASED THE ERROR
!           ESTIMATE. IN THE NORMAL CASE THE INSERT PROCEDURE SHOULD
!           START AFTER THE NRMAX-TH LARGEST ERROR ESTIMATE.
!
   10 ERRMAX = ELIST(MAXERR)
      IF(NRMAX.EQ.1) GO TO 30
      IDO = NRMAX-1
      DO 20 I = 1,IDO
        ISUCC = IORD(NRMAX-1)
! ***JUMP OUT OF DO-LOOP
        IF(ERRMAX.LE.ELIST(ISUCC)) GO TO 30
        IORD(NRMAX) = ISUCC
        NRMAX = NRMAX-1
   20    CONTINUE
!
!           COMPUTE THE NUMBER OF ELEMENTS IN THE LIST TO BE MAINTAINED
!           IN DESCENDING ORDER. THIS NUMBER DEPENDS ON THE NUMBER OF
!           SUBDIVISIONS STILL ALLOWED.
!
   30 JUPBN = LAST
      IF(LAST.GT.(LIMIT/2+2)) JUPBN = LIMIT+3-LAST
      ERRMIN = ELIST(LAST)
!
!           INSERT ERRMAX BY TRAVERSING THE LIST TOP-DOWN,
!           STARTING COMPARISON FROM THE ELEMENT ELIST(IORD(NRMAX+1)).
!
      JBND = JUPBN-1
      IBEG = NRMAX+1
      IF(IBEG.GT.JBND) GO TO 50
      DO 40 I=IBEG,JBND
        ISUCC = IORD(I)
! ***JUMP OUT OF DO-LOOP
        IF(ERRMAX.GE.ELIST(ISUCC)) GO TO 60
        IORD(I-1) = ISUCC
   40 CONTINUE
   50 IORD(JBND) = MAXERR
      IORD(JUPBN) = LAST
      GO TO 90
!
!           INSERT ERRMIN BY TRAVERSING THE LIST BOTTOM-UP.
!
   60 IORD(I-1) = MAXERR
      K = JBND
      DO 70 J=I,JBND
        ISUCC = IORD(K)
! ***JUMP OUT OF DO-LOOP
        IF(ERRMIN.LT.ELIST(ISUCC)) GO TO 80
        IORD(K+1) = ISUCC
        K = K-1
   70 CONTINUE
      IORD(I) = LAST
      GO TO 90
   80 IORD(K+1) = LAST
!
!           SET MAXERR AND ERMAX.
!
   90 MAXERR = IORD(NRMAX)
      ERMAX = ELIST(MAXERR)
      RETURN
      END SUBROUTINE DQPSRT
      SUBROUTINE DQRDC(X,LDX,N,P,QRAUX,JPVT,WORK,JOB)
!***BEGIN PROLOGUE  DQRDC
!***DATE WRITTEN   780814   (YYMMDD)
!***REVISION DATE  820801   (YYMMDD)
!***REVISION HISTORY  (YYMMDD)
!   000330  Modified array declarations.  (JEC)
!***CATEGORY NO.  D5
!***KEYWORDS  DECOMPOSITION,DOUBLE PRECISION,LINEAR ALGEBRA,LINPACK,
!             MATRIX,ORTHOGONAL TRIANGULAR
!***AUTHOR  STEWART, G. W., (U. OF MARYLAND)
!***PURPOSE  Uses Householder transformations to compute the Qr factori-
!            zation of N by P matrix X.  Column pivoting is optional.
!***DESCRIPTION
!
!     DQRDC uses Householder transformations to compute the QR
!     factorization of an N by P matrix X.  Column pivoting
!     based on the 2-norms of the reduced columns may be
!     performed at the user's option.
!
!     On Entry
!
!        X       DOUBLE PRECISION(LDX,P), where LDX .GE. N.
!                X contains the matrix whose decomposition is to be
!                computed.
!
!        LDX     INTEGER.
!                LDX is the leading dimension of the array X.
!
!        N       INTEGER.
!                N is the number of rows of the matrix X.
!
!        P       INTEGER.
!                P is the number of columns of the matrix X.
!
!        JPVT    INTEGER(P).
!                JPVT contains integers that control the selection
!                of the pivot columns.  The K-th column X(K) of X
!                is placed in one of three classes according to the
!                value of JPVT(K).
!
!                   If JPVT(K) .GT. 0, then X(K) is an initial
!                                      column.
!
!                   If JPVT(K) .EQ. 0, then X(K) is a free column.
!
!                   If JPVT(K) .LT. 0, then X(K) is a final column.
!
!                Before the decomposition is computed, initial columns
!                are moved to the beginning of the array X and final
!                columns to the end.  Both initial and final columns
!                are frozen in place during the computation and only
!                free columns are moved.  At the K-th stage of the
!                reduction, if X(K) is occupied by a free column
!                it is interchanged with the free column of largest
!                reduced norm.  JPVT is not referenced if
!                JOB .EQ. 0.
!
!        WORK    DOUBLE PRECISION(P).
!                WORK is a work array.  WORK is not referenced if
!                JOB .EQ. 0.
!
!        JOB     INTEGER.
!                JOB is an integer that initiates column pivoting.
!                If JOB .EQ. 0, no pivoting is done.
!                If JOB .NE. 0, pivoting is done.
!
!     On Return
!
!        X       X contains in its upper triangle the upper
!                triangular matrix R of the QR factorization.
!                Below its diagonal X contains information from
!                which the orthogonal part of the decomposition
!                can be recovered.  Note that if pivoting has
!                been requested, the decomposition is not that
!                of the original matrix X but that of X
!                with its columns permuted as described by JPVT.
!
!        QRAUX   DOUBLE PRECISION(P).
!                QRAUX contains further information required to recover
!                the orthogonal part of the decomposition.
!
!        JPVT    JPVT(K) contains the index of the column of the
!                original matrix that has been interchanged into
!                the K-th column, if pivoting was requested.
!
!     LINPACK.  This version dated 08/14/78 .
!     G. W. Stewart, University of Maryland, Argonne National Lab.
!
!     DQRDC uses the following functions and subprograms.
!
!     BLAS DAXPY,DDOT,DSCAL,DSWAP,DNRM2
!     Fortran DABS,DMAX1,MIN0,DSQRT
!***REFERENCES  DONGARRA J.J., BUNCH J.R., MOLER C.B., STEWART G.W.,
!                 *LINPACK USERS  GUIDE*, SIAM, 1979.
!***ROUTINES CALLED  DAXPY,DDOT,DNRM2,DSCAL,DSWAP
!***END PROLOGUE  DQRDC
      INTEGER LDX,N,P,JOB
      INTEGER JPVT(*)
      DOUBLE PRECISION X(LDX,*),QRAUX(*),WORK(*)
!
      INTEGER J,JP,L,LP1,LUP,MAXJ,PL,PU
      DOUBLE PRECISION MAXNRM,DNRM2,TT
      DOUBLE PRECISION DDOT,NRMXL,T
      LOGICAL NEGJ,SWAPJ
!
!***FIRST EXECUTABLE STATEMENT  DQRDC
      PL = 1
      PU = 0
      IF (JOB .EQ. 0) GO TO 60
!
!        PIVOTING HAS BEEN REQUESTED.  REARRANGE THE COLUMNS
!        ACCORDING TO JPVT.
!
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
!
!     COMPUTE THE NORMS OF THE FREE COLUMNS.
!
      IF (PU .LT. PL) GO TO 80
      DO 70 J = PL, PU
         QRAUX(J) = DNRM2(N,X(1,J),1)
         WORK(J) = QRAUX(J)
   70 CONTINUE
   80 CONTINUE
!
!     PERFORM THE HOUSEHOLDER REDUCTION OF X.
!
      LUP = MIN0(N,P)
      DO 200 L = 1, LUP
         IF (L .LT. PL .OR. L .GE. PU) GO TO 120
!
!           LOCATE THE COLUMN OF LARGEST NORM AND BRING IT
!           INTO THE PIVOT POSITION.
!
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
!
!           COMPUTE THE HOUSEHOLDER TRANSFORMATION FOR COLUMN L.
!
            NRMXL = DNRM2(N-L+1,X(L,L),1)
            IF (NRMXL .EQ. 0.0D0) GO TO 180
               IF (X(L,L) .NE. 0.0D0) NRMXL = DSIGN(NRMXL,X(L,L))
               CALL DSCAL(N-L+1,1.0D0/NRMXL,X(L,L),1)
               X(L,L) = 1.0D0 + X(L,L)
!
!              APPLY THE TRANSFORMATION TO THE REMAINING COLUMNS,
!              UPDATING THE NORMS.
!
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
!
!              SAVE THE TRANSFORMATION.
!
               QRAUX(L) = X(L,L)
               X(L,L) = -NRMXL
  180       CONTINUE
  190    CONTINUE
  200 CONTINUE
      RETURN
      END SUBROUTINE DQRDC
      SUBROUTINE DQRSL(X,LDX,N,K,QRAUX,Y,QY,QTY,B,RSD,XB,JOB,INFO)
!***BEGIN PROLOGUE  DQRSL
!***DATE WRITTEN   780814   (YYMMDD)
!***REVISION DATE  820801   (YYMMDD)
!***REVISION HISTORY  (YYMMDD)
!   000330  Modified array declarations.  (JEC)
!***CATEGORY NO.  D9,D2A1
!***KEYWORDS  DOUBLE PRECISION,LINEAR ALGEBRA,LINPACK,MATRIX,
!             ORTHOGONAL TRIANGULAR,SOLVE
!***AUTHOR  STEWART, G. W., (U. OF MARYLAND)
!***PURPOSE  Applies the output of DQRDC to compute coordinate
!            transformations, projections, and least squares solutions.
!***DESCRIPTION
!
!     DQRSL applies the output of DQRDC to compute coordinate
!     transformations, projections, and least squares solutions.
!     For K .LE. MIN(N,P), let XK be the matrix
!
!            XK = (X(JPVT(1)),X(JPVT(2)), ... ,X(JPVT(K)))
!
!     formed from columnns JPVT(1), ... ,JPVT(K) of the original
!     N X P matrix X that was input to DQRDC (if no pivoting was
!     done, XK consists of the first K columns of X in their
!     original order).  DQRDC produces a factored orthogonal matrix Q
!     and an upper triangular matrix R such that
!
!              XK = Q * (R)
!                       (0)
!
!     This information is contained in coded form in the arrays
!     X and QRAUX.
!
!     On Entry
!
!        X      DOUBLE PRECISION(LDX,P).
!               X contains the output of DQRDC.
!
!        LDX    INTEGER.
!               LDX is the leading dimension of the array X.
!
!        N      INTEGER.
!               N is the number of rows of the matrix XK.  It must
!               have the same value as N in DQRDC.
!
!        K      INTEGER.
!               K is the number of columns of the matrix XK.  K
!               must not be greater than MIN(N,P), where P is the
!               same as in the calling sequence to DQRDC.
!
!        QRAUX  DOUBLE PRECISION(P).
!               QRAUX contains the auxiliary output from DQRDC.
!
!        Y      DOUBLE PRECISION(N)
!               Y contains an N-vector that is to be manipulated
!               by DQRSL.
!
!        JOB    INTEGER.
!               JOB specifies what is to be computed.  JOB has
!               the decimal expansion ABCDE, with the following
!               meaning.
!
!                    If A .NE. 0, compute QY.
!                    If B,C,D, or E .NE. 0, compute QTY.
!                    If C .NE. 0, compute B.
!                    If D .NE. 0, compute RSD.
!                    If E .NE. 0, compute XB.
!
!               Note that a request to compute B, RSD, or XB
!               automatically triggers the computation of QTY, for
!               which an array must be provided in the calling
!               sequence.
!
!     On Return
!
!        QY     DOUBLE PRECISION(N).
!               QY contains Q*Y, if its computation has been
!               requested.
!
!        QTY    DOUBLE PRECISION(N).
!               QTY contains TRANS(Q)*Y, if its computation has
!               been requested.  Here TRANS(Q) is the
!               transpose of the matrix Q.
!
!        B      DOUBLE PRECISION(K)
!               B contains the solution of the least squares problem
!
!                    minimize norm2(Y - XK*B),
!
!               if its computation has been requested.  (Note that
!               if pivoting was requested in DQRDC, the J-th
!               component of B will be associated with column JPVT(J)
!               of the original matrix X that was input into DQRDC.)
!
!        RSD    DOUBLE PRECISION(N).
!               RSD contains the least squares residual Y - XK*B,
!               if its computation has been requested.  RSD is
!               also the orthogonal projection of Y onto the
!               orthogonal complement of the column space of XK.
!
!        XB     DOUBLE PRECISION(N).
!               XB contains the least squares approximation XK*B,
!               if its computation has been requested.  XB is also
!               the orthogonal projection of Y onto the column space
!               of X.
!
!        INFO   INTEGER.
!               INFO is zero unless the computation of B has
!               been requested and R is exactly singular.  In
!               this case, INFO is the index of the first zero
!               diagonal element of R and B is left unaltered.
!
!     The parameters QY, QTY, B, RSD, and XB are not referenced
!     if their computation is not requested and in this case
!     can be replaced by dummy variables in the calling program.
!     To save storage, the user may in some cases use the same
!     array for different parameters in the calling sequence.  A
!     frequently occuring example is when one wishes to compute
!     any of B, RSD, or XB and does not need Y or QTY.  In this
!     case one may identify Y, QTY, and one of B, RSD, or XB, while
!     providing separate arrays for anything else that is to be
!     computed.  Thus the calling sequence
!
!          CALL DQRSL(X,LDX,N,K,QRAUX,Y,DUM,Y,B,Y,DUM,110,INFO)
!
!     will result in the computation of B and RSD, with RSD
!     overwriting Y.  More generally, each item in the following
!     list contains groups of permissible identifications for
!     a single calling sequence.
!
!          1. (Y,QTY,B) (RSD) (XB) (QY)
!
!          2. (Y,QTY,RSD) (B) (XB) (QY)
!
!          3. (Y,QTY,XB) (B) (RSD) (QY)
!
!          4. (Y,QY) (QTY,B) (RSD) (XB)
!
!          5. (Y,QY) (QTY,RSD) (B) (XB)
!
!          6. (Y,QY) (QTY,XB) (B) (RSD)
!
!     In any group the value returned in the array allocated to
!     the group corresponds to the last member of the group.
!
!     LINPACK.  This version dated 08/14/78 .
!     G. W. Stewart, University of Maryland, Argonne National Lab.
!
!     DQRSL uses the following functions and subprograms.
!
!     BLAS DAXPY,DCOPY,DDOT
!     Fortran DABS,MIN0,MOD
!***REFERENCES  DONGARRA J.J., BUNCH J.R., MOLER C.B., STEWART G.W.,
!                 *LINPACK USERS  GUIDE*, SIAM, 1979.
!***ROUTINES CALLED  DAXPY,DCOPY,DDOT
!***END PROLOGUE  DQRSL
      INTEGER LDX,N,K,JOB,INFO
      DOUBLE PRECISION X(LDX,*),QRAUX(*),Y(*),QY(*),QTY(*),B(*),RSD(*),   &
                       XB(*)
!
      INTEGER I,J,JJ,JU,KP1
      DOUBLE PRECISION DDOT,T,TEMP
      LOGICAL CB,CQY,CQTY,CR,CXB
!
!     SET INFO FLAG.
!
!***FIRST EXECUTABLE STATEMENT  DQRSL
      INFO = 0
!
!     DETERMINE WHAT IS TO BE COMPUTED.
!
      CQY = JOB/10000 .NE. 0
      CQTY = MOD(JOB,10000) .NE. 0
      CB = MOD(JOB,1000)/100 .NE. 0
      CR = MOD(JOB,100)/10 .NE. 0
      CXB = MOD(JOB,10) .NE. 0
      JU = MIN0(K,N-1)
!
!     SPECIAL ACTION WHEN N=1.
!
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
!
!        SET UP TO COMPUTE QY OR QTY.
!
         IF (CQY) CALL DCOPY(N,Y,1,QY,1)
         IF (CQTY) CALL DCOPY(N,Y,1,QTY,1)
         IF (.NOT.CQY) GO TO 70
!
!           COMPUTE QY.
!
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
!
!           COMPUTE TRANS(Q)*Y.
!
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
!
!        SET UP TO COMPUTE B, RSD, OR XB.
!
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
!
!           COMPUTE B.
!
            DO 170 JJ = 1, K
               J = K - JJ + 1
               IF (X(J,J) .NE. 0.0D0) GO TO 150
                  INFO = J
!           ......EXIT
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
!
!           COMPUTE RSD OR XB AS REQUIRED.
!
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
      END SUBROUTINE DQRSL
      SUBROUTINE DRAW0(X,Y,N,ID,   &
                       XTEMP,YTEMP,TATEMP,NTEMP,NTRACE,   &
                       IBUGG3,ISUBRO,IERROR)
!
!     PURPOSE--XX
!
!     WRITTEN BY--DAVID W. BEHRINGER NOAA/AOML (MIAMI).
!                 AS PART OF NOAA'S CONCX V.3   MARCH 1988.
!     ORIGINAL VERSION (IN DATAPLOT)--AUGUST    1988.
!
!
!---------------------------------------------------------------------
!
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      DIMENSION X(*)
      DIMENSION Y(*)
!
      DIMENSION XTEMP(*)
      DIMENSION YTEMP(*)
      DIMENSION TATEMP(*)
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'RAW0')THEN
        WRITE(ICOUT,51)ISUBRO,IBUGG3,IERROR,N,ID
   51   FORMAT('FROM DRAW0: ISUBRO,IBUGG3,IERROR,N,ID = ',3(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!CCCC IF (ID.NE.0) CALL GDASH(ID)    AUGUST 1988
      IF (N.GT.1) CALL GVECT(X,Y,N,   &
                             XTEMP,YTEMP,TATEMP,NTEMP,NTRACE,   &
                             IBUGG3,ISUBRO,IERROR)
      N=1
!
      RETURN
      END SUBROUTINE DRAW0
      SUBROUTINE DRAWL(X,Y,DST,N,IDSH,CHR,NCHR,SZ,DL0,   &
                       XTEMP,YTEMP,TATEMP,NTEMP,NTRACE,   &
                       IBUGG3,ISUBRO,IERROR)
!
!     PURPOSE--XX
!
!     WRITTEN BY--DAVID W. BEHRINGER NOAA/AOML (MIAMI).
!                 AS PART OF NOAA'S CONCX V.3   MARCH 1988.
!     ORIGINAL VERSION (IN DATAPLOT)--AUGUST    1988.
!     UPDATED                         MAY       1989.  TRIG DEGREE FUNCTIONS
!
!---------------------------------------------------------------------
!
!CCCC CHARACTER CHR(NCHR)*1    AUGUST 1988
      CHARACTER CHR(15)*1
!
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
!CCCC DIMENSION X(N),Y(N),DST(N),XL(3),YL(3)
!
      DIMENSION X(*)
      DIMENSION Y(*)
      DIMENSION DST(*)
!
      DIMENSION XTEMP(*)
      DIMENSION YTEMP(*)
      DIMENSION TATEMP(*)
!
      DIMENSION XL(3)
      DIMENSION YL(3)
!
      INCLUDE 'DPCOP2.INC'
!
      DATA EPS,RSSX/0.01,1.15/
!     DATA EPS,RSSX/0.01,1.05/
!
!-----START POINT-----------------------------------------------------
!
!CCCC THE FOLLOWING LINE WAS ADDED TO FIX         MAY 1989
!CCCC THE TRIG DEGREES FUNCTIONS PROBLEM          MAY 1989
      CONDR=3.1415926/180.0
!
      A=CPUMIN
      SZ2=0.5*SZ
      SSZ=SZ*FLOAT(NCHR)+SZ2
      SSZ2=0.5*SSZ
      CHR(NCHR+1)='$'
      DST(1)=0.
      DO 100 I=2,N
        DST(I)=DST(I-1)+SQRT((X(I)-X(I-1))**2+(Y(I)-Y(I-1))**2)
 100  CONTINUE
      I=2
 200  CONTINUE
      IF(I.GT.N)GO TO 299
!CCCC DO WHILE (I.LE.N)                  JANUARY 1989
        IF (DST(I).LE.DST(I-1)) THEN
          N=N-1
          DO 210 J=I,N
            DST(J)=DST(J+1)
            X(J)=X(J+1)
            Y(J)=Y(J+1)
 210      CONTINUE
        ELSE
          I=I+1
        END IF
      GO TO 200
 299  CONTINUE
      IF (DST(N).LT.EPS.OR.N.EQ.1) THEN
        N=1
        GO TO 9000
      END IF
      NLBL=MAX0(INT(DST(N)/DL0),1)
      SEP=SQRT((X(N)-X(1))**2+(Y(N)-Y(1))**2)
      DL=DST(N)/FLOAT(NLBL)
      IF (NLBL.EQ.1.AND.SEP.LT.EPS.AND.DST(N).LT.2.*SSZ) THEN
        IR=1
        DO 300 I=2,N
          IF(X(I).GT.X(IR)) IR=I
 300    CONTINUE
!CCCC   CALL GCHARJ(4)   AUGUST 1988
        CALL GCHAR(CHR,X(IR)+SSZ2,Y(IR),SZ,   &
                   IBUGG3,ISUBRO,IERROR)
        CALL DRAW0(X,Y,N,IDSH,   &
                   XTEMP,YTEMP,TATEMP,NTEMP,NTRACE,   &
                   IBUGG3,ISUBRO,IERROR)
        GO TO 9000
      END IF
      IE=2
      XE=X(1)
      YE=Y(1)
      DO 400 NL=1,NLBL
        L=0
        DLBL0=FLOAT(NL-1)*DL
        DLBL=(FLOAT(NL)-0.5)*DL-0.5*SSZ
        I0=IE
        IEO=IE
        XEO=XE
        YEO=YE
 500    CONTINUE
        IF(L.NE.0.OR.DLBL.LE.DLBL0)GO TO 599
!CCCC   DO WHILE (L.EQ.0.AND.DLBL.GT.DLBL0)   JANAURY 1989
          I=I0-1
 600      CONTINUE
          IF(DST(I).GE.DLBL.OR.I.GE.N)GO TO 699
!CCCC     DO WHILE (DST(I).LT.DLBL.AND.I.LT.N)   JANUARY 1989
            I=I+1
            GO TO 600
 699      CONTINUE
          IF (DST(I).GE.DLBL) THEN
            IS=I-1
            R=(DLBL-DST(IS))/(DST(IS+1)-DST(IS))
            XS=X(IS)+R*(X(IS+1)-X(IS))
            YS=Y(IS)+R*(Y(IS+1)-Y(IS))
            C=SQRT((X(I)-XS)**2+(Y(I)-YS)**2)
            A0=C
 700        CONTINUE
            IF(C.GE.SSZ.OR.I.GE.N)GO TO 799
!CCCC       DO WHILE (C.LT.SSZ.AND.I.LT.N)   JANUARY 1989
              A=C
              I=I+1
              C=SQRT((X(I)-XS)**2+(Y(I)-YS)**2)
              GO TO 700
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
!               RSS=(A+D)/SSZ
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
                  CALL DRAW0(XL,YL,N2,IDSH,   &
                             XTEMP,YTEMP,TATEMP,NTEMP,NTRACE,   &
                             IBUGG3,ISUBRO,IERROR)
                  XL(1)=X(IS)
                  YL(1)=Y(IS)
                  CALL DRAW0(X(I0),Y(I0),N0,IDSH,   &
                             XTEMP,YTEMP,TATEMP,NTEMP,NTRACE,   &
                             IBUGG3,ISUBRO,IERROR)
                END IF
                XL(2)=XS
                YL(2)=YS
                N2=2
                CALL DRAW0(XL,YL,N2,IDSH,   &
                           XTEMP,YTEMP,TATEMP,NTEMP,NTRACE,   &
                           IBUGG3,ISUBRO,IERROR)
!CCCC THE FOLLOWING LINE WAS FIXED BY THE       MAY 1989
!CCCC SUCCEEDING 2 LINES                        MAY 1989
!CCCC           ANG=ATAN2D(YE-YS,XE-XS)
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
!CCCC           CALL GCHARA(IANG)  AUGUST 1988
!CCCC           CALL GCHARJ(JST)  AUGUST 1988
!CCCC THE FOLLOWING LINE WAS FIXED BY THE       MAY 1989
!CCCC SUCCEEDING 2 LINES                        MAY 1989
!CCCC           CALL GCHAR(CHR,XS+SZ2*COSD(ANG),YS+SZ2*SIND(ANG),SZ,
                ANGR=ANG*CONDR
                CALL GCHAR(CHR,XS+SZ2*COS(ANGR),YS+SZ2*SIN(ANGR),SZ,   &
                           IBUGG3,ISUBRO,IERROR)
              ELSE
                DLBL=DLBL-0.5*SSZ
              END IF
            ELSE
              DLBL=DLBL-0.5*SSZ
            END IF
          ELSE
            DLBL=DLBL-0.5*SSZ
          END IF
          GO TO 500
 599    CONTINUE
 400  CONTINUE
      XL(1)=XE
      YL(1)=YE
      XL(2)=X(IE)
      YL(2)=Y(IE)
      N2=2
      CALL DRAW0(XL,YL,N2,IDSH,   &
                 XTEMP,YTEMP,TATEMP,NTEMP,NTRACE,   &
                 IBUGG3,ISUBRO,IERROR)
      N0=N-IE+1
      CALL DRAW0(X(IE),Y(IE),N0,IDSH,   &
                 XTEMP,YTEMP,TATEMP,NTEMP,NTRACE,   &
                 IBUGG3,ISUBRO,IERROR)
      N=1
      GO TO 9000
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE DRAWL
      SUBROUTINE DRCIL1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR ROMAN COMPLEX ITALIC LOWER CASE (PART 1).
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
!     DEFINE CHARACTER   2151--LOWER CASE A
!
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
!
      DATA IXMIND(   1)/ -10/
      DATA IXMAXD(   1)/  11/
      DATA IXDELD(   1)/  21/
      DATA ISTARD(   1)/   1/
      DATA NUMCOO(   1)/  35/
!
!     DEFINE CHARACTER   2152--LOWER CASE B
!
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
!
      DATA IXMIND(   2)/ -10/
      DATA IXMAXD(   2)/   9/
      DATA IXDELD(   2)/  19/
      DATA ISTARD(   2)/  36/
      DATA NUMCOO(   2)/  31/
!
!     DEFINE CHARACTER   2153--LOWER CASE C
!
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
!
      DATA IXMIND(   3)/  -9/
      DATA IXMAXD(   3)/   9/
      DATA IXDELD(   3)/  18/
      DATA ISTARD(   3)/  67/
      DATA NUMCOO(   3)/  23/
!
!     DEFINE CHARACTER   2154--LOWER CASE D
!
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
!
      DATA IXMIND(   4)/ -10/
      DATA IXMAXD(   4)/  11/
      DATA IXDELD(   4)/  21/
      DATA ISTARD(   4)/  90/
      DATA NUMCOO(   4)/  37/
!
!     DEFINE CHARACTER   2155--LOWER CASE E
!
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
!
      DATA IXMIND(   5)/  -9/
      DATA IXMAXD(   5)/   9/
      DATA IXDELD(   5)/  18/
      DATA ISTARD(   5)/ 127/
      DATA NUMCOO(   5)/  24/
!
!     DEFINE CHARACTER   2156--LOWER CASE F
!
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
!
      DATA IXMIND(   6)/  -7/
      DATA IXMAXD(   6)/   8/
      DATA IXDELD(   6)/  15/
      DATA ISTARD(   6)/ 151/
      DATA NUMCOO(   6)/  32/
!
!     DEFINE CHARACTER   2157--LOWER CASE G
!
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
!
      DATA IXMIND(   7)/ -10/
      DATA IXMAXD(   7)/  10/
      DATA IXDELD(   7)/  20/
      DATA ISTARD(   7)/ 183/
      DATA NUMCOO(   7)/  39/
!
!     DEFINE CHARACTER   2158--LOWER CASE H
!
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
!
      DATA IXMIND(   8)/ -10/
      DATA IXMAXD(   8)/  11/
      DATA IXDELD(   8)/  21/
      DATA ISTARD(   8)/ 222/
      DATA NUMCOO(   8)/  26/
!
!     DEFINE CHARACTER   2159--LOWER CASE I
!
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
!
      DATA IXMIND(   9)/  -6/
      DATA IXMAXD(   9)/   7/
      DATA IXDELD(   9)/  13/
      DATA ISTARD(   9)/ 248/
      DATA NUMCOO(   9)/  23/
!
!     DEFINE CHARACTER   2160--LOWER CASE J
!
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
!
      DATA IXMIND(  10)/  -6/
      DATA IXMAXD(  10)/   7/
      DATA IXDELD(  10)/  13/
      DATA ISTARD(  10)/ 271/
      DATA NUMCOO(  10)/  29/
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCIL1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
!
!               **************************************
!               **  STEP 2--                        **
!               **  EXTRACT THE COORDINATES         **
!               **  FOR THIS PARTICULAR CHARACTER.  **
!               **************************************
!
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
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GO TO 9090
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
      END SUBROUTINE DRCIL1
      SUBROUTINE DRCIL2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR ROMAN COMPLEX ITALIC LOWER CASE (PART 2).
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
!     DEFINE CHARACTER   2161--LOWER CASE K
!
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
!
      DATA IXMIND(  11)/ -10/
      DATA IXMAXD(  11)/  10/
      DATA IXDELD(  11)/  20/
      DATA ISTARD(  11)/   1/
      DATA NUMCOO(  11)/  28/
!
!     DEFINE CHARACTER   2162--LOWER CASE L
!
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
!
      DATA IXMIND(  12)/  -5/
      DATA IXMAXD(  12)/   7/
      DATA IXDELD(  12)/  12/
      DATA ISTARD(  12)/  29/
      DATA NUMCOO(  12)/  15/
!
!     DEFINE CHARACTER   2163--LOWER CASE M
!
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
!
      DATA IXMIND(  13)/ -17/
      DATA IXMAXD(  13)/  16/
      DATA IXDELD(  13)/  33/
      DATA ISTARD(  13)/  44/
      DATA NUMCOO(  13)/  46/
!
!     DEFINE CHARACTER   2164--LOWER CASE N
!
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
!
      DATA IXMIND(  14)/ -12/
      DATA IXMAXD(  14)/  11/
      DATA IXDELD(  14)/  23/
      DATA ISTARD(  14)/  90/
      DATA NUMCOO(  14)/  33/
!
!     DEFINE CHARACTER   2165--LOWER CASE O
!
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
!
      DATA IXMIND(  15)/  -9/
      DATA IXMAXD(  15)/   9/
      DATA IXDELD(  15)/  18/
      DATA ISTARD(  15)/ 123/
      DATA NUMCOO(  15)/  29/
!
!     DEFINE CHARACTER   2166--LOWER CASE P
!
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
!
      DATA IXMIND(  16)/ -11/
      DATA IXMAXD(  16)/  10/
      DATA IXDELD(  16)/  21/
      DATA ISTARD(  16)/ 152/
      DATA NUMCOO(  16)/  37/
!
!     DEFINE CHARACTER   2167--LOWER CASE Q
!
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
!
      DATA IXMIND(  17)/ -10/
      DATA IXMAXD(  17)/  10/
      DATA IXDELD(  17)/  20/
      DATA ISTARD(  17)/ 189/
      DATA NUMCOO(  17)/  28/
!
!     DEFINE CHARACTER   2168--LOWER CASE R
!
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
!
      DATA IXMIND(  18)/  -9/
      DATA IXMAXD(  18)/   8/
      DATA IXDELD(  18)/  17/
      DATA ISTARD(  18)/ 217/
      DATA NUMCOO(  18)/  23/
!
!     DEFINE CHARACTER   2169--LOWER CASE S
!
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
!
      DATA IXMIND(  19)/  -8/
      DATA IXMAXD(  19)/   9/
      DATA IXDELD(  19)/  17/
      DATA ISTARD(  19)/ 240/
      DATA NUMCOO(  19)/  26/
!
!     DEFINE CHARACTER   2170--LOWER CASE T
!
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
!
      DATA IXMIND(  20)/  -7/
      DATA IXMAXD(  20)/   7/
      DATA IXDELD(  20)/  14/
      DATA ISTARD(  20)/ 266/
      DATA NUMCOO(  20)/  15/
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCIL2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
!
!               **************************************
!               **  STEP 2--                        **
!               **  EXTRACT THE COORDINATES         **
!               **  FOR THIS PARTICULAR CHARACTER.  **
!               **************************************
!
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
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GO TO 9090
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
      END SUBROUTINE DRCIL2
      SUBROUTINE DRCIL3(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR ROMAN COMPLEX ITALIC LOWER CASE (PART 3).
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
!     DEFINE CHARACTER   2171--LOWER CASE U
!
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
!
      DATA IXMIND(  21)/ -12/
      DATA IXMAXD(  21)/  11/
      DATA IXDELD(  21)/  23/
      DATA ISTARD(  21)/   1/
      DATA NUMCOO(  21)/  33/
!
!     DEFINE CHARACTER   2172--LOWER CASE V
!
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
!
      DATA IXMIND(  22)/ -10/
      DATA IXMAXD(  22)/  10/
      DATA IXDELD(  22)/  20/
      DATA ISTARD(  22)/  34/
      DATA NUMCOO(  22)/  24/
!
!     DEFINE CHARACTER   2173--LOWER CASE W
!
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
!
      DATA IXMIND(  23)/ -15/
      DATA IXMAXD(  23)/  14/
      DATA IXDELD(  23)/  29/
      DATA ISTARD(  23)/  58/
      DATA NUMCOO(  23)/  37/
!
!     DEFINE CHARACTER   2174--LOWER CASE X
!
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
!
      DATA IXMIND(  24)/ -10/
      DATA IXMAXD(  24)/  10/
      DATA IXDELD(  24)/  20/
      DATA ISTARD(  24)/  95/
      DATA NUMCOO(  24)/  38/
!
!     DEFINE CHARACTER   2175--LOWER CASE Y
!
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
!
      DATA IXMIND(  25)/ -11/
      DATA IXMAXD(  25)/  10/
      DATA IXDELD(  25)/  21/
      DATA ISTARD(  25)/ 133/
      DATA NUMCOO(  25)/  37/
!
!     DEFINE CHARACTER   2176--LOWER CASE Z
!
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
!
      DATA IXMIND(  26)/ -10/
      DATA IXMAXD(  26)/  10/
      DATA IXDELD(  26)/  20/
      DATA ISTARD(  26)/ 170/
      DATA NUMCOO(  26)/  26/
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCIL3--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
!
!               **************************************
!               **  STEP 2--                        **
!               **  EXTRACT THE COORDINATES         **
!               **  FOR THIS PARTICULAR CHARACTER.  **
!               **************************************
!
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
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GO TO 9090
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
      END SUBROUTINE DRCIL3
      SUBROUTINE DRCIN1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR ROMAN COMPLEX ITALIC NUMERIC (PART 1).
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
!     DEFINE CHARACTER      0--0
!
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
!
      DATA IXMIND(   1)/ -10/
      DATA IXMAXD(   1)/  11/
      DATA IXDELD(   1)/  21/
      DATA ISTARD(   1)/   1/
      DATA NUMCOO(   1)/  39/
!
!     DEFINE CHARACTER      1--1
!
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
!
      DATA IXMIND(   2)/ -10/
      DATA IXMAXD(   2)/  11/
      DATA IXDELD(   2)/  21/
      DATA ISTARD(   2)/  40/
      DATA NUMCOO(   2)/  11/
!
!     DEFINE CHARACTER      2--2
!
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
!
      DATA IXMIND(   3)/ -10/
      DATA IXMAXD(   3)/  11/
      DATA IXDELD(   3)/  21/
      DATA ISTARD(   3)/  51/
      DATA NUMCOO(   3)/  39/
!
!     DEFINE CHARACTER      3--3
!
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
!
      DATA IXMIND(   4)/ -10/
      DATA IXMAXD(   4)/  11/
      DATA IXDELD(   4)/  21/
      DATA ISTARD(   4)/  90/
      DATA NUMCOO(   4)/  47/
!
!     DEFINE CHARACTER      4--4
!
      DATA IOPERA( 137),IX( 137),IY( 137)/'MOVE',  -6,  -1/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW', -12, -21/
      DATA IOPERA( 139),IX( 139),IY( 139)/'MOVE',  -5,   0/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW', -11, -21/
      DATA IOPERA( 141),IX( 141),IY( 141)/'MOVE',  -5,   0/
      DATA IOPERA( 142),IX( 142),IY( 142)/'DRAW', -20, -15/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',  -4, -15/
!
      DATA IXMIND(   5)/ -10/
      DATA IXMAXD(   5)/  11/
      DATA IXDELD(   5)/  21/
      DATA ISTARD(   5)/ 137/
      DATA NUMCOO(   5)/   7/
!
!     DEFINE CHARACTER      5--5
!
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
!
      DATA IXMIND(   6)/ -10/
      DATA IXMAXD(   6)/  11/
      DATA IXDELD(   6)/  21/
      DATA ISTARD(   6)/ 144/
      DATA NUMCOO(   6)/  35/
!
!     DEFINE CHARACTER      6--6
!
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
!
      DATA IXMIND(   7)/ -10/
      DATA IXMAXD(   7)/  11/
      DATA IXDELD(   7)/  21/
      DATA ISTARD(   7)/ 179/
      DATA NUMCOO(   7)/  43/
!
!     DEFINE CHARACTER      7--7
!
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
!
      DATA IXMIND(   8)/ -10/
      DATA IXMAXD(   8)/  11/
      DATA IXDELD(   8)/  21/
      DATA ISTARD(   8)/ 222/
      DATA NUMCOO(   8)/  25/
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCIN1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
!
!               **************************************
!               **  STEP 2--                        **
!               **  EXTRACT THE COORDINATES         **
!               **  FOR THIS PARTICULAR CHARACTER.  **
!               **************************************
!
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
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GO TO 9090
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
      END SUBROUTINE DRCIN1
      SUBROUTINE DRCIN2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR ROMAN COMPLEX ITALIC NUMERIC (PART 2).
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
!     DEFINE CHARACTER      8--8
!
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
!
      DATA IXMIND(   9)/ -10/
      DATA IXMAXD(   9)/  11/
      DATA IXDELD(   9)/  21/
      DATA ISTARD(   9)/   1/
      DATA NUMCOO(   9)/  57/
!
!     DEFINE CHARACTER      9--9
!
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
!
      DATA IXMIND(  10)/ -10/
      DATA IXMAXD(  10)/  11/
      DATA IXDELD(  10)/  21/
      DATA ISTARD(  10)/  58/
      DATA NUMCOO(  10)/  43/
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCIN2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
!
!               **************************************
!               **  STEP 2--                        **
!               **  EXTRACT THE COORDINATES         **
!               **  FOR THIS PARTICULAR CHARACTER.  **
!               **************************************
!
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
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GO TO 9090
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
      END SUBROUTINE DRCIN2
      SUBROUTINE DRCIU1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR ROMAN COMPLEX ITALIC UPPER CASE (PART 1).
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
!     DEFINE CHARACTER   2051--UPPER CASE A
!
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
!
      DATA IXMIND(   1)/ -10/
      DATA IXMAXD(   1)/  10/
      DATA IXDELD(   1)/  20/
      DATA ISTARD(   1)/   1/
      DATA NUMCOO(   1)/  12/
!
!     DEFINE CHARACTER   2052--UPPER CASE B
!
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
!
      DATA IXMIND(   2)/ -12/
      DATA IXMAXD(   2)/  12/
      DATA IXDELD(   2)/  24/
      DATA ISTARD(   2)/  13/
      DATA NUMCOO(   2)/  35/
!
!     DEFINE CHARACTER   2053--UPPER CASE C
!
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
!
      DATA IXMIND(   3)/ -10/
      DATA IXMAXD(   3)/  11/
      DATA IXDELD(   3)/  21/
      DATA ISTARD(   3)/  48/
      DATA NUMCOO(   3)/  32/
!
!     DEFINE CHARACTER   2054--UPPER CASE D
!
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
!
      DATA IXMIND(   4)/ -12/
      DATA IXMAXD(   4)/  11/
      DATA IXDELD(   4)/  23/
      DATA ISTARD(   4)/  80/
      DATA NUMCOO(   4)/  26/
!
!     DEFINE CHARACTER   2055--UPPER CASE E
!
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
!
      DATA IXMIND(   5)/ -12/
      DATA IXMAXD(   5)/  11/
      DATA IXDELD(   5)/  23/
      DATA ISTARD(   5)/ 106/
      DATA NUMCOO(   5)/  16/
!
!     DEFINE CHARACTER   2056--UPPER CASE F
!
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
!
      DATA IXMIND(   6)/ -12/
      DATA IXMAXD(   6)/  10/
      DATA IXDELD(   6)/  22/
      DATA ISTARD(   6)/ 122/
      DATA NUMCOO(   6)/  14/
!
!     DEFINE CHARACTER   2057--UPPER CASE G
!
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
!
      DATA IXMIND(   7)/ -10/
      DATA IXMAXD(   7)/  12/
      DATA IXDELD(   7)/  22/
      DATA ISTARD(   7)/ 136/
      DATA NUMCOO(   7)/  38/
!
!     DEFINE CHARACTER   2058--UPPER CASE H
!
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
!
      DATA IXMIND(   8)/ -13/
      DATA IXMAXD(   8)/  13/
      DATA IXDELD(   8)/  26/
      DATA ISTARD(   8)/ 174/
      DATA NUMCOO(   8)/  18/
!
!     DEFINE CHARACTER   2059--UPPER CASE I
!
      DATA IOPERA( 192),IX( 192),IY( 192)/'MOVE',   3,  12/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW',  -3,  -9/
      DATA IOPERA( 194),IX( 194),IY( 194)/'MOVE',   4,  12/
      DATA IOPERA( 195),IX( 195),IY( 195)/'DRAW',  -2,  -9/
      DATA IOPERA( 196),IX( 196),IY( 196)/'MOVE',   0,  12/
      DATA IOPERA( 197),IX( 197),IY( 197)/'DRAW',   7,  12/
      DATA IOPERA( 198),IX( 198),IY( 198)/'MOVE',  -6,  -9/
      DATA IOPERA( 199),IX( 199),IY( 199)/'DRAW',   1,  -9/
!
      DATA IXMIND(   9)/  -6/
      DATA IXMAXD(   9)/   7/
      DATA IXDELD(   9)/  13/
      DATA ISTARD(   9)/ 192/
      DATA NUMCOO(   9)/   8/
!
!     DEFINE CHARACTER   2060--UPPER CASE J
!
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
!
      DATA IXMIND(  10)/  -9/
      DATA IXMAXD(  10)/   9/
      DATA IXDELD(  10)/  18/
      DATA ISTARD(  10)/ 200/
      DATA NUMCOO(  10)/  18/
!
!     DEFINE CHARACTER   2061--UPPER CASE K
!
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
!
      DATA IXMIND(  11)/ -12/
      DATA IXMAXD(  11)/  11/
      DATA IXDELD(  11)/  23/
      DATA ISTARD(  11)/ 218/
      DATA NUMCOO(  11)/  18/
!
!     DEFINE CHARACTER   2062--UPPER CASE L
!
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
!
      DATA IXMIND(  12)/ -10/
      DATA IXMAXD(  12)/  10/
      DATA IXDELD(  12)/  20/
      DATA ISTARD(  12)/ 236/
      DATA NUMCOO(  12)/  10/
!
!     DEFINE CHARACTER   2063--UPPER CASE M
!
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
!
      DATA IXMIND(  13)/ -13/
      DATA IXMAXD(  13)/  14/
      DATA IXDELD(  13)/  27/
      DATA ISTARD(  13)/ 246/
      DATA NUMCOO(  13)/  20/
!
!     DEFINE CHARACTER   2064--UPPER CASE N
!
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
!
      DATA IXMIND(  14)/ -12/
      DATA IXMAXD(  14)/  13/
      DATA IXDELD(  14)/  25/
      DATA ISTARD(  14)/ 266/
      DATA NUMCOO(  14)/  14/
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCIU1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
!
!               **************************************
!               **  STEP 2--                        **
!               **  EXTRACT THE COORDINATES         **
!               **  FOR THIS PARTICULAR CHARACTER.  **
!               **************************************
!
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
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GO TO 9090
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
      END SUBROUTINE DRCIU1
      SUBROUTINE DRCIU2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR ROMAN COMPLEX ITALIC UPPER CASE (PART 2).
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
!     DEFINE CHARACTER   2065--UPPER CASE O
!
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
!
      DATA IXMIND(  15)/ -11/
      DATA IXMAXD(  15)/  11/
      DATA IXDELD(  15)/  22/
      DATA ISTARD(  15)/   1/
      DATA NUMCOO(  15)/  39/
!
!     DEFINE CHARACTER   2066--UPPER CASE P
!
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
!
      DATA IXMIND(  16)/ -12/
      DATA IXMAXD(  16)/  11/
      DATA IXDELD(  16)/  23/
      DATA ISTARD(  16)/  40/
      DATA NUMCOO(  16)/  22/
!
!     DEFINE CHARACTER   2067--UPPER CASE Q
!
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
!
      DATA IXMIND(  17)/ -11/
      DATA IXMAXD(  17)/  11/
      DATA IXDELD(  17)/  22/
      DATA ISTARD(  17)/  62/
      DATA NUMCOO(  17)/  56/
!
!     DEFINE CHARACTER   2068--UPPER CASE R
!
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
!
      DATA IXMIND(  18)/ -12/
      DATA IXMAXD(  18)/  12/
      DATA IXDELD(  18)/  24/
      DATA ISTARD(  18)/ 118/
      DATA NUMCOO(  18)/  35/
!
!     DEFINE CHARACTER   2069--UPPER CASE S
!
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
!
      DATA IXMIND(  19)/ -11/
      DATA IXMAXD(  19)/  12/
      DATA IXDELD(  19)/  23/
      DATA ISTARD(  19)/ 153/
      DATA NUMCOO(  19)/  33/
!
!     DEFINE CHARACTER   2070--UPPER CASE T
!
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
!
      DATA IXMIND(  20)/ -10/
      DATA IXMAXD(  20)/  11/
      DATA IXDELD(  20)/  21/
      DATA ISTARD(  20)/ 186/
      DATA NUMCOO(  20)/  12/
!
!     DEFINE CHARACTER   2071--UPPER CASE U
!
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
!
      DATA IXMIND(  21)/ -12/
      DATA IXMAXD(  21)/  13/
      DATA IXDELD(  21)/  25/
      DATA ISTARD(  21)/ 198/
      DATA NUMCOO(  21)/  21/
!
!     DEFINE CHARACTER   2072--UPPER CASE V
!
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
!
      DATA IXMIND(  22)/ -10/
      DATA IXMAXD(  22)/  10/
      DATA IXDELD(  22)/  20/
      DATA ISTARD(  22)/ 219/
      DATA NUMCOO(  22)/  10/
!
!     DEFINE CHARACTER   2073--UPPER CASE W
!
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
!
      DATA IXMIND(  23)/ -13/
      DATA IXMAXD(  23)/  13/
      DATA IXDELD(  23)/  26/
      DATA ISTARD(  23)/ 229/
      DATA NUMCOO(  23)/  16/
!
!     DEFINE CHARACTER   2074--UPPER CASE X
!
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
!
      DATA IXMIND(  24)/ -11/
      DATA IXMAXD(  24)/  11/
      DATA IXDELD(  24)/  22/
      DATA ISTARD(  24)/ 245/
      DATA NUMCOO(  24)/  14/
!
!     DEFINE CHARACTER   2075--UPPER CASE Y
!
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
!
      DATA IXMIND(  25)/ -10/
      DATA IXMAXD(  25)/  11/
      DATA IXDELD(  25)/  21/
      DATA ISTARD(  25)/ 259/
      DATA NUMCOO(  25)/  14/
!
!     DEFINE CHARACTER   2076--UPPER CASE Z
!
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
!
      DATA IXMIND(  26)/ -11/
      DATA IXMAXD(  26)/  11/
      DATA IXDELD(  26)/  22/
      DATA ISTARD(  26)/ 273/
      DATA NUMCOO(  26)/  12/
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCIU2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
!
!               **************************************
!               **  STEP 2--                        **
!               **  EXTRACT THE COORDINATES         **
!               **  FOR THIS PARTICULAR CHARACTER.  **
!               **************************************
!
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
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GO TO 9090
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
      END SUBROUTINE DRCIU2
      SUBROUTINE DRCL1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR ROMAN COMPLEX LOWER CASE (PART 1).
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
!     DEFINE CHARACTER   2101--LOWER CASE A
!
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
!
      DATA IXMIND(   1)/  -9/
      DATA IXMAXD(   1)/  11/
      DATA IXDELD(   1)/  20/
      DATA ISTARD(   1)/   1/
      DATA NUMCOO(   1)/  35/
!
!     DEFINE CHARACTER   2102--LOWER CASE B
!
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
!
      DATA IXMIND(   2)/ -11/
      DATA IXMAXD(   2)/  10/
      DATA IXDELD(   2)/  21/
      DATA ISTARD(   2)/  36/
      DATA NUMCOO(   2)/  28/
!
!     DEFINE CHARACTER   2103--LOWER CASE C
!
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
!
      DATA IXMIND(   3)/ -10/
      DATA IXMAXD(   3)/   9/
      DATA IXDELD(   3)/  19/
      DATA ISTARD(   3)/  64/
      DATA NUMCOO(   3)/  26/
!
!     DEFINE CHARACTER   2104--LOWER CASE D
!
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
!
      DATA IXMIND(   4)/ -10/
      DATA IXMAXD(   4)/  11/
      DATA IXDELD(   4)/  21/
      DATA ISTARD(   4)/  90/
      DATA NUMCOO(   4)/  30/
!
!     DEFINE CHARACTER   2105--LOWER CASE E
!
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
!
      DATA IXMIND(   5)/ -10/
      DATA IXMAXD(   5)/   9/
      DATA IXDELD(   5)/  19/
      DATA ISTARD(   5)/ 120/
      DATA NUMCOO(   5)/  28/
!
!     DEFINE CHARACTER   2106--LOWER CASE F
!
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
!
      DATA IXMIND(   6)/  -7/
      DATA IXMAXD(   6)/   6/
      DATA IXDELD(   6)/  13/
      DATA ISTARD(   6)/ 148/
      DATA NUMCOO(   6)/  18/
!
!     DEFINE CHARACTER   2107--LOWER CASE G
!
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
!
      DATA IXMIND(   7)/  -9/
      DATA IXMAXD(   7)/  10/
      DATA IXDELD(   7)/  19/
      DATA ISTARD(   7)/ 166/
      DATA NUMCOO(   7)/  54/
!
!     DEFINE CHARACTER   2108--LOWER CASE H
!
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
!
      DATA IXMIND(   8)/ -11/
      DATA IXMAXD(   8)/  11/
      DATA IXDELD(   8)/  22/
      DATA ISTARD(   8)/ 220/
      DATA NUMCOO(   8)/  21/
!
!     DEFINE CHARACTER   2109--LOWER CASE I
!
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
!
      DATA IXMIND(   9)/  -5/
      DATA IXMAXD(   9)/   6/
      DATA IXDELD(   9)/  11/
      DATA ISTARD(   9)/ 241/
      DATA NUMCOO(   9)/  13/
!
!     DEFINE CHARACTER   2110--LOWER CASE J
!
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
!
      DATA IXMIND(  10)/  -5/
      DATA IXMAXD(  10)/   6/
      DATA IXDELD(  10)/  11/
      DATA ISTARD(  10)/ 254/
      DATA NUMCOO(  10)/  21/
!
!     DEFINE CHARACTER   2111--LOWER CASE K
!
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
!
      DATA IXMIND(  11)/ -11/
      DATA IXMAXD(  11)/  10/
      DATA IXDELD(  11)/  21/
      DATA ISTARD(  11)/ 275/
      DATA NUMCOO(  11)/  18/
!
!     DEFINE CHARACTER   2112--LOWER CASE L
!
      DATA IOPERA( 293),IX( 293),IY( 293)/'MOVE',   0,  12/
      DATA IOPERA( 294),IX( 294),IY( 294)/'DRAW',   0,  -9/
      DATA IOPERA( 295),IX( 295),IY( 295)/'MOVE',   1,  12/
      DATA IOPERA( 296),IX( 296),IY( 296)/'DRAW',   1,  -9/
      DATA IOPERA( 297),IX( 297),IY( 297)/'MOVE',  -3,  12/
      DATA IOPERA( 298),IX( 298),IY( 298)/'DRAW',   1,  12/
      DATA IOPERA( 299),IX( 299),IY( 299)/'MOVE',  -3,  -9/
      DATA IOPERA( 300),IX( 300),IY( 300)/'DRAW',   4,  -9/
!
      DATA IXMIND(  12)/  -5/
      DATA IXMAXD(  12)/   6/
      DATA IXDELD(  12)/  11/
      DATA ISTARD(  12)/ 293/
      DATA NUMCOO(  12)/   8/
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCL1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
!
!               **************************************
!               **  STEP 2--                        **
!               **  EXTRACT THE COORDINATES         **
!               **  FOR THIS PARTICULAR CHARACTER.  **
!               **************************************
!
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
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GO TO 9090
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
      END SUBROUTINE DRCL1
      SUBROUTINE DRCL2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR ROMAN COMPLEX LOWER CASE (PART 2).
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
!     DEFINE CHARACTER   2113--LOWER CASE M
!
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
!
      DATA IXMIND(  13)/ -16/
      DATA IXMAXD(  13)/  17/
      DATA IXDELD(  13)/  33/
      DATA ISTARD(  13)/   1/
      DATA NUMCOO(  13)/  34/
!
!     DEFINE CHARACTER   2114--LOWER CASE N
!
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
!
      DATA IXMIND(  14)/ -11/
      DATA IXMAXD(  14)/  11/
      DATA IXDELD(  14)/  22/
      DATA ISTARD(  14)/  35/
      DATA NUMCOO(  14)/  21/
!
!     DEFINE CHARACTER   2115--LOWER CASE O
!
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
!
      DATA IXMIND(  15)/ -10/
      DATA IXMAXD(  15)/  10/
      DATA IXDELD(  15)/  20/
      DATA ISTARD(  15)/  56/
      DATA NUMCOO(  15)/  33/
!
!     DEFINE CHARACTER   2116--LOWER CASE P
!
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
!
      DATA IXMIND(  16)/ -11/
      DATA IXMAXD(  16)/  10/
      DATA IXDELD(  16)/  21/
      DATA ISTARD(  16)/  89/
      DATA NUMCOO(  16)/  30/
!
!     DEFINE CHARACTER   2117--LOWER CASE Q
!
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
!
      DATA IXMIND(  17)/ -10/
      DATA IXMAXD(  17)/  10/
      DATA IXDELD(  17)/  20/
      DATA ISTARD(  17)/ 119/
      DATA NUMCOO(  17)/  28/
!
!     DEFINE CHARACTER   2118--LOWER CASE R
!
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
!
      DATA IXMIND(  18)/  -9/
      DATA IXMAXD(  18)/   8/
      DATA IXDELD(  18)/  17/
      DATA ISTARD(  18)/ 147/
      DATA NUMCOO(  18)/  18/
!
!     DEFINE CHARACTER   2119--LOWER CASE S
!
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
!
      DATA IXMIND(  19)/  -8/
      DATA IXMAXD(  19)/   9/
      DATA IXDELD(  19)/  17/
      DATA ISTARD(  19)/ 165/
      DATA NUMCOO(  19)/  30/
!
!     DEFINE CHARACTER   2120--LOWER CASE T
!
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
!
      DATA IXMIND(  20)/  -7/
      DATA IXMAXD(  20)/   8/
      DATA IXDELD(  20)/  15/
      DATA ISTARD(  20)/ 195/
      DATA NUMCOO(  20)/  13/
!
!     DEFINE CHARACTER   2121--LOWER CASE U
!
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
!
      DATA IXMIND(  21)/ -11/
      DATA IXMAXD(  21)/  11/
      DATA IXDELD(  21)/  22/
      DATA ISTARD(  21)/ 208/
      DATA NUMCOO(  21)/  21/
!
!     DEFINE CHARACTER   2122--LOWER CASE V
!
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
!
      DATA IXMIND(  22)/  -9/
      DATA IXMAXD(  22)/   9/
      DATA IXDELD(  22)/  18/
      DATA ISTARD(  22)/ 229/
      DATA NUMCOO(  22)/  10/
!
!     DEFINE CHARACTER   2123--LOWER CASE W
!
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
!
      DATA IXMIND(  23)/ -12/
      DATA IXMAXD(  23)/  12/
      DATA IXDELD(  23)/  24/
      DATA ISTARD(  23)/ 239/
      DATA NUMCOO(  23)/  16/
!
!     DEFINE CHARACTER   2124--LOWER CASE X
!
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
!
      DATA IXMIND(  24)/ -10/
      DATA IXMAXD(  24)/  10/
      DATA IXDELD(  24)/  20/
      DATA ISTARD(  24)/ 255/
      DATA NUMCOO(  24)/  14/
!
!     DEFINE CHARACTER   2125--LOWER CASE Y
!
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
!
      DATA IXMIND(  25)/ -10/
      DATA IXMAXD(  25)/   9/
      DATA IXDELD(  25)/  19/
      DATA ISTARD(  25)/ 269/
      DATA NUMCOO(  25)/  17/
!
!     DEFINE CHARACTER   2126--LOWER CASE Z
!
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
!
      DATA IXMIND(  26)/  -9/
      DATA IXMAXD(  26)/   9/
      DATA IXDELD(  26)/  18/
      DATA ISTARD(  26)/ 286/
      DATA NUMCOO(  26)/  12/
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCL2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
!
!               **************************************
!               **  STEP 2--                        **
!               **  EXTRACT THE COORDINATES         **
!               **  FOR THIS PARTICULAR CHARACTER.  **
!               **************************************
!
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
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GO TO 9090
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
      END SUBROUTINE DRCL2
      SUBROUTINE DRCN1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR ROMAN COMPLEX NUMERIC (PART 1).
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
!     DEFINE CHARACTER   2200--0
!
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
!
      DATA IXMIND(   1)/ -10/
      DATA IXMAXD(   1)/  10/
      DATA IXDELD(   1)/  20/
      DATA ISTARD(   1)/   1/
      DATA NUMCOO(   1)/  37/
!
!     DEFINE CHARACTER   2201--1
!
      DATA IOPERA(  38),IX(  38),IY(  38)/'MOVE',  -4,   8/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',  -2,   9/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',   1,  12/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',   1,  -9/
      DATA IOPERA(  42),IX(  42),IY(  42)/'MOVE',   0,  11/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',   0,  -9/
      DATA IOPERA(  44),IX(  44),IY(  44)/'MOVE',  -4,  -9/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',   5,  -9/
!
      DATA IXMIND(   2)/ -10/
      DATA IXMAXD(   2)/  10/
      DATA IXDELD(   2)/  20/
      DATA ISTARD(   2)/  38/
      DATA NUMCOO(   2)/   8/
!
!     DEFINE CHARACTER   2202--2
!
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
!
      DATA IXMIND(   3)/ -10/
      DATA IXMAXD(   3)/  10/
      DATA IXDELD(   3)/  20/
      DATA ISTARD(   3)/  46/
      DATA NUMCOO(   3)/  41/
!
!     DEFINE CHARACTER   2203--3
!
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
!
      DATA IXMIND(   4)/ -10/
      DATA IXMAXD(   4)/  10/
      DATA IXDELD(   4)/  20/
      DATA ISTARD(   4)/  87/
      DATA NUMCOO(   4)/  43/
!
!     DEFINE CHARACTER   2204--4
!
      DATA IOPERA( 130),IX( 130),IY( 130)/'MOVE',   2,  10/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',   2,  -9/
      DATA IOPERA( 132),IX( 132),IY( 132)/'MOVE',   3,  12/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',   3,  -9/
      DATA IOPERA( 134),IX( 134),IY( 134)/'MOVE',   3,  12/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',  -8,  -3/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',   8,  -3/
      DATA IOPERA( 137),IX( 137),IY( 137)/'MOVE',  -1,  -9/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW',   6,  -9/
!
      DATA IXMIND(   5)/ -10/
      DATA IXMAXD(   5)/  10/
      DATA IXDELD(   5)/  20/
      DATA ISTARD(   5)/ 130/
      DATA NUMCOO(   5)/   9/
!
!     DEFINE CHARACTER   2205--5
!
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
!
      DATA IXMIND(   6)/ -10/
      DATA IXMAXD(   6)/  10/
      DATA IXDELD(   6)/  20/
      DATA ISTARD(   6)/ 139/
      DATA NUMCOO(   6)/  34/
!
!     DEFINE CHARACTER   2206--6
!
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
!
      DATA IXMIND(   7)/ -10/
      DATA IXMAXD(   7)/  10/
      DATA IXDELD(   7)/  20/
      DATA ISTARD(   7)/ 173/
      DATA NUMCOO(   7)/  45/
!
!     DEFINE CHARACTER   2207--7
!
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
!
      DATA IXMIND(   8)/ -10/
      DATA IXMAXD(   8)/  10/
      DATA IXDELD(   8)/  20/
      DATA ISTARD(   8)/ 218/
      DATA NUMCOO(   8)/  26/
!
!     DEFINE CHARACTER   2208--8
!
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
!
      DATA IXMIND(   9)/ -10/
      DATA IXMAXD(   9)/  10/
      DATA IXDELD(   9)/  20/
      DATA ISTARD(   9)/ 244/
      DATA NUMCOO(   9)/  57/
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCN1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
!
!               **************************************
!               **  STEP 2--                        **
!               **  EXTRACT THE COORDINATES         **
!               **  FOR THIS PARTICULAR CHARACTER.  **
!               **************************************
!
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
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GO TO 9090
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
      END SUBROUTINE DRCN1
      SUBROUTINE DRCN2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR ROMAN COMPLEX NUMERIC (PART 2).
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
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
!     DEFINE CHARACTER   2209--9
!
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
!
      DATA IXMIND(  10)/ -10/
      DATA IXMAXD(  10)/  10/
      DATA IXDELD(  10)/  20/
      DATA ISTARD(  10)/   1/
      DATA NUMCOO(  10)/  45/
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCN2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
!
!               **************************************
!               **  STEP 2--                        **
!               **  EXTRACT THE COORDINATES         **
!               **  FOR THIS PARTICULAR CHARACTER.  **
!               **************************************
!
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
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GO TO 9090
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
      END SUBROUTINE DRCN2
      SUBROUTINE DRCSL1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR ROMAN COMPLEX SCRIPT LOWER CASE (PART 1).
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
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
!     DEFINE CHARACTER   2651--LOWER CASE A
!
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
!
      DATA IXMIND(   1)/  -7/
      DATA IXMAXD(   1)/   9/
      DATA IXDELD(   1)/  16/
      DATA ISTARD(   1)/   1/
      DATA NUMCOO(   1)/  29/
!
!     DEFINE CHARACTER   2652--LOWER CASE B
!
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
!
      DATA IXMIND(   2)/  -6/
      DATA IXMAXD(   2)/   8/
      DATA IXDELD(   2)/  14/
      DATA ISTARD(   2)/  30/
      DATA NUMCOO(   2)/  20/
!
!     DEFINE CHARACTER   2653--LOWER CASE C
!
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
!
      DATA IXMIND(   3)/  -6/
      DATA IXMAXD(   3)/   6/
      DATA IXDELD(   3)/  12/
      DATA ISTARD(   3)/  50/
      DATA NUMCOO(   3)/  20/
!
!     DEFINE CHARACTER   2654--LOWER CASE D
!
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
!
      DATA IXMIND(   4)/  -7/
      DATA IXMAXD(   4)/   9/
      DATA IXDELD(   4)/  16/
      DATA ISTARD(   4)/  70/
      DATA NUMCOO(   4)/  29/
!
!     DEFINE CHARACTER   2655--LOWER CASE E
!
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
!
      DATA IXMIND(   5)/  -6/
      DATA IXMAXD(   5)/   6/
      DATA IXDELD(   5)/  12/
      DATA ISTARD(   5)/  99/
      DATA NUMCOO(   5)/  21/
!
!     DEFINE CHARACTER   2656--LOWER CASE F
!
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
!
      DATA IXMIND(   6)/  -3/
      DATA IXMAXD(   6)/   6/
      DATA IXDELD(   6)/   9/
      DATA ISTARD(   6)/ 120/
      DATA NUMCOO(   6)/  25/
!
!     DEFINE CHARACTER   2657--LOWER CASE G
!
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
!
      DATA IXMIND(   7)/  -7/
      DATA IXMAXD(   7)/   9/
      DATA IXDELD(   7)/  16/
      DATA ISTARD(   7)/ 145/
      DATA NUMCOO(   7)/  34/
!
!     DEFINE CHARACTER   2658--LOWER CASE H
!
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
!
      DATA IXMIND(   8)/  -6/
      DATA IXMAXD(   8)/   9/
      DATA IXDELD(   8)/  15/
      DATA ISTARD(   8)/ 179/
      DATA NUMCOO(   8)/  25/
!
!     DEFINE CHARACTER   2659--LOWER CASE I
!
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
!
      DATA IXMIND(   9)/  -4/
      DATA IXMAXD(   9)/   4/
      DATA IXDELD(   9)/   8/
      DATA ISTARD(   9)/ 204/
      DATA NUMCOO(   9)/  16/
!
!     DEFINE CHARACTER   2660--LOWER CASE J
!
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
!
      DATA IXMIND(  10)/  -4/
      DATA IXMAXD(  10)/   4/
      DATA IXDELD(  10)/   8/
      DATA ISTARD(  10)/ 220/
      DATA NUMCOO(  10)/  21/
!
!     DEFINE CHARACTER   2661--LOWER CASE K
!
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
!
      DATA IXMIND(  11)/  -6/
      DATA IXMAXD(  11)/   8/
      DATA IXDELD(  11)/  14/
      DATA ISTARD(  11)/ 241/
      DATA NUMCOO(  11)/  25/
!
!     DEFINE CHARACTER   2662--LOWER CASE L
!
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
!
      DATA IXMIND(  12)/  -4/
      DATA IXMAXD(  12)/   4/
      DATA IXDELD(  12)/   8/
      DATA ISTARD(  12)/ 266/
      DATA NUMCOO(  12)/  14/
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCSL1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
!
!               **************************************
!               **  STEP 2--                        **
!               **  EXTRACT THE COORDINATES         **
!               **  FOR THIS PARTICULAR CHARACTER.  **
!               **************************************
!
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
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GO TO 9090
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
      END SUBROUTINE DRCSL1
      SUBROUTINE DRCSL2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR ROMAN COMPLEX SCRIPT LOWER CASE (PART 2).
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
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
!     DEFINE CHARACTER   2663--LOWER CASE M
!
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
!
      DATA IXMIND(  13)/ -13/
      DATA IXMAXD(  13)/  12/
      DATA IXDELD(  13)/  25/
      DATA ISTARD(  13)/   1/
      DATA NUMCOO(  13)/  39/
!
!     DEFINE CHARACTER   2664--LOWER CASE N
!
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
!
      DATA IXMIND(  14)/  -9/
      DATA IXMAXD(  14)/   9/
      DATA IXDELD(  14)/  18/
      DATA ISTARD(  14)/  40/
      DATA NUMCOO(  14)/  28/
!
!     DEFINE CHARACTER   2665--LOWER CASE O
!
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
!
      DATA IXMIND(  15)/  -7/
      DATA IXMAXD(  15)/   7/
      DATA IXDELD(  15)/  14/
      DATA ISTARD(  15)/  68/
      DATA NUMCOO(  15)/  27/
!
!     DEFINE CHARACTER   2666--LOWER CASE P
!
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
!
      DATA IXMIND(  16)/  -6/
      DATA IXMAXD(  16)/   9/
      DATA IXDELD(  16)/  15/
      DATA ISTARD(  16)/  95/
      DATA NUMCOO(  16)/  25/
!
!     DEFINE CHARACTER   2667--LOWER CASE Q
!
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
!
      DATA IXMIND(  17)/  -7/
      DATA IXMAXD(  17)/   9/
      DATA IXDELD(  17)/  16/
      DATA ISTARD(  17)/ 120/
      DATA NUMCOO(  17)/  31/
!
!     DEFINE CHARACTER   2668--LOWER CASE R
!
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
!
      DATA IXMIND(  18)/  -6/
      DATA IXMAXD(  18)/   8/
      DATA IXDELD(  18)/  14/
      DATA ISTARD(  18)/ 151/
      DATA NUMCOO(  18)/  20/
!
!     DEFINE CHARACTER   2669--LOWER CASE S
!
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
!
      DATA IXMIND(  19)/  -4/
      DATA IXMAXD(  19)/   8/
      DATA IXDELD(  19)/  12/
      DATA ISTARD(  19)/ 171/
      DATA NUMCOO(  19)/  19/
!
!     DEFINE CHARACTER   2670--LOWER CASE T
!
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
!
      DATA IXMIND(  20)/  -4/
      DATA IXMAXD(  20)/   4/
      DATA IXDELD(  20)/   8/
      DATA ISTARD(  20)/ 190/
      DATA NUMCOO(  20)/  16/
!
!     DEFINE CHARACTER   2671--LOWER CASE U
!
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
!
      DATA IXMIND(  21)/  -7/
      DATA IXMAXD(  21)/   9/
      DATA IXDELD(  21)/  16/
      DATA ISTARD(  21)/ 206/
      DATA NUMCOO(  21)/  23/
!
!     DEFINE CHARACTER   2672--LOWER CASE V
!
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
!
      DATA IXMIND(  22)/  -7/
      DATA IXMAXD(  22)/   8/
      DATA IXDELD(  22)/  15/
      DATA ISTARD(  22)/ 229/
      DATA NUMCOO(  22)/  20/
!
!     DEFINE CHARACTER   2673--LOWER CASE W
!
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
!
      DATA IXMIND(  23)/ -10/
      DATA IXMAXD(  23)/  11/
      DATA IXDELD(  23)/  21/
      DATA ISTARD(  23)/ 249/
      DATA NUMCOO(  23)/  31/
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCSL2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
!
!               **************************************
!               **  STEP 2--                        **
!               **  EXTRACT THE COORDINATES         **
!               **  FOR THIS PARTICULAR CHARACTER.  **
!               **************************************
!
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
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GO TO 9090
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
      END SUBROUTINE DRCSL2
      SUBROUTINE DRCSL3(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR ROMAN COMPLEX SCRIPT LOWER CASE (PART 3).
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
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
!     DEFINE CHARACTER   2674--LOWER CASE X
!
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
!
      DATA IXMIND(  24)/  -8/
      DATA IXMAXD(  24)/   8/
      DATA IXDELD(  24)/  16/
      DATA ISTARD(  24)/   1/
      DATA NUMCOO(  24)/  36/
!
!     DEFINE CHARACTER   2675--LOWER CASE Y
!
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
!
      DATA IXMIND(  25)/  -7/
      DATA IXMAXD(  25)/   9/
      DATA IXDELD(  25)/  16/
      DATA ISTARD(  25)/  37/
      DATA NUMCOO(  25)/  28/
!
!     DEFINE CHARACTER   2676--LOWER CASE Z
!
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
!
      DATA IXMIND(  26)/  -6/
      DATA IXMAXD(  26)/   7/
      DATA IXDELD(  26)/  13/
      DATA ISTARD(  26)/  65/
      DATA NUMCOO(  26)/  34/
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCSL3--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
!
!               **************************************
!               **  STEP 2--                        **
!               **  EXTRACT THE COORDINATES         **
!               **  FOR THIS PARTICULAR CHARACTER.  **
!               **************************************
!
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
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GO TO 9090
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
      END SUBROUTINE DRCSL3
      SUBROUTINE DRCSN1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR ROMAN COMPLEX SCRIPT NUMERIC (PART 1).
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
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
!     DEFINE CHARACTER   2750--0
!
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
!
      DATA IXMIND(   1)/ -10/
      DATA IXMAXD(   1)/  11/
      DATA IXDELD(   1)/  21/
      DATA ISTARD(   1)/   1/
      DATA NUMCOO(   1)/  39/
!
!     DEFINE CHARACTER   2751--1
!
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
!
      DATA IXMIND(   2)/ -10/
      DATA IXMAXD(   2)/  11/
      DATA IXDELD(   2)/  21/
      DATA ISTARD(   2)/  40/
      DATA NUMCOO(   2)/  11/
!
!     DEFINE CHARACTER   2752--2
!
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
!
      DATA IXMIND(   3)/ -10/
      DATA IXMAXD(   3)/  11/
      DATA IXDELD(   3)/  21/
      DATA ISTARD(   3)/  51/
      DATA NUMCOO(   3)/  38/
!
!     DEFINE CHARACTER   2753--3
!
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
!
      DATA IXMIND(   4)/ -10/
      DATA IXMAXD(   4)/  11/
      DATA IXDELD(   4)/  21/
      DATA ISTARD(   4)/  89/
      DATA NUMCOO(   4)/  46/
!
!     DEFINE CHARACTER   2754--4
!
      DATA IOPERA( 135),IX( 135),IY( 135)/'MOVE',   6,  11/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',   0,  -9/
      DATA IOPERA( 137),IX( 137),IY( 137)/'MOVE',   7,  12/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW',   1,  -9/
      DATA IOPERA( 139),IX( 139),IY( 139)/'MOVE',   7,  12/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW',  -8,  -3/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',   8,  -3/
!
      DATA IXMIND(   5)/ -10/
      DATA IXMAXD(   5)/  11/
      DATA IXDELD(   5)/  21/
      DATA ISTARD(   5)/ 135/
      DATA NUMCOO(   5)/   7/
!
!     DEFINE CHARACTER   2755--5
!
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
!
      DATA IXMIND(   6)/ -10/
      DATA IXMAXD(   6)/  11/
      DATA IXDELD(   6)/  21/
      DATA ISTARD(   6)/ 142/
      DATA NUMCOO(   6)/  34/
!
!     DEFINE CHARACTER   2756--6
!
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
!
      DATA IXMIND(   7)/ -10/
      DATA IXMAXD(   7)/  11/
      DATA IXDELD(   7)/  21/
      DATA ISTARD(   7)/ 176/
      DATA NUMCOO(   7)/  43/
!
!     DEFINE CHARACTER   2757--7
!
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
!
      DATA IXMIND(   8)/ -10/
      DATA IXMAXD(   8)/  11/
      DATA IXDELD(   8)/  21/
      DATA ISTARD(   8)/ 219/
      DATA NUMCOO(   8)/  25/
!
!     DEFINE CHARACTER   2758--8
!
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
!
      DATA IXMIND(   9)/ -10/
      DATA IXMAXD(   9)/  11/
      DATA IXDELD(   9)/  21/
      DATA ISTARD(   9)/ 244/
      DATA NUMCOO(   9)/  57/
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCSN1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
!
!               **************************************
!               **  STEP 2--                        **
!               **  EXTRACT THE COORDINATES         **
!               **  FOR THIS PARTICULAR CHARACTER.  **
!               **************************************
!
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
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GO TO 9090
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
      END SUBROUTINE DRCSN1
      SUBROUTINE DRCSN2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR ROMAN COMPLEX SCRIPT NUMERIC (PART 2).
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
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
!     DEFINE CHARACTER   2759--9
!
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
!
      DATA IXMIND(  10)/ -10/
      DATA IXMAXD(  10)/  11/
      DATA IXDELD(  10)/  21/
      DATA ISTARD(  10)/   1/
      DATA NUMCOO(  10)/  43/
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCSN2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
!
!               **************************************
!               **  STEP 2--                        **
!               **  EXTRACT THE COORDINATES         **
!               **  FOR THIS PARTICULAR CHARACTER.  **
!               **************************************
!
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
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GO TO 9090
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
      END SUBROUTINE DRCSN2
      SUBROUTINE DRCSU1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR ROMAN COMPLEX SCRIPT UPPER CASE (PART 1).
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
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
!     DEFINE CHARACTER   2551--UPPER CASE A
!
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
!
      DATA IXMIND(   1)/ -13/
      DATA IXMAXD(   1)/  10/
      DATA IXDELD(   1)/  23/
      DATA ISTARD(   1)/   1/
      DATA NUMCOO(   1)/  34/
!
!     DEFINE CHARACTER   2552--UPPER CASE B
!
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
!
      DATA IXMIND(   2)/ -12/
      DATA IXMAXD(   2)/  12/
      DATA IXDELD(   2)/  24/
      DATA ISTARD(   2)/  35/
      DATA NUMCOO(   2)/  64/
!
!     DEFINE CHARACTER   2553--UPPER CASE C
!
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
!
      DATA IXMIND(   3)/ -10/
      DATA IXMAXD(   3)/  11/
      DATA IXDELD(   3)/  21/
      DATA ISTARD(   3)/  99/
      DATA NUMCOO(   3)/  39/
!
!     DEFINE CHARACTER   2554--UPPER CASE D
!
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
!
      DATA IXMIND(   4)/ -12/
      DATA IXMAXD(   4)/  11/
      DATA IXDELD(   4)/  23/
      DATA ISTARD(   4)/ 138/
      DATA NUMCOO(   4)/  41/
!
!     DEFINE CHARACTER   2555--UPPER CASE E
!
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
!
      DATA IXMIND(   5)/  -9/
      DATA IXMAXD(   5)/  10/
      DATA IXDELD(   5)/  19/
      DATA ISTARD(   5)/ 179/
      DATA NUMCOO(   5)/  45/
!
!     DEFINE CHARACTER   2556--UPPER CASE F
!
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
!
      DATA IXMIND(   6)/ -11/
      DATA IXMAXD(   6)/  10/
      DATA IXDELD(   6)/  21/
      DATA ISTARD(   6)/ 224/
      DATA NUMCOO(   6)/  42/
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCSU1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
!
!               **************************************
!               **  STEP 2--                        **
!               **  EXTRACT THE COORDINATES         **
!               **  FOR THIS PARTICULAR CHARACTER.  **
!               **************************************
!
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
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GO TO 9090
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
      END SUBROUTINE DRCSU1
      SUBROUTINE DRCSU2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR ROMAN COMPLEX SCRIPT UPPER CASE (PART 2).
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
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
!     DEFINE CHARACTER   2557--UPPER CASE G
!
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
!
      DATA IXMIND(   7)/ -11/
      DATA IXMAXD(   7)/  11/
      DATA IXDELD(   7)/  22/
      DATA ISTARD(   7)/   1/
      DATA NUMCOO(   7)/  49/
!
!     DEFINE CHARACTER   2558--UPPER CASE H
!
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
!
      DATA IXMIND(   8)/ -12/
      DATA IXMAXD(   8)/  12/
      DATA IXDELD(   8)/  24/
      DATA ISTARD(   8)/  50/
      DATA NUMCOO(   8)/  51/
!
!     DEFINE CHARACTER   2559--UPPER CASE I
!
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
!
      DATA IXMIND(   9)/  -9/
      DATA IXMAXD(   9)/   7/
      DATA IXDELD(   9)/  16/
      DATA ISTARD(   9)/ 101/
      DATA NUMCOO(   9)/  31/
!
!     DEFINE CHARACTER   2560--UPPER CASE J
!
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
!
      DATA IXMIND(  10)/  -9/
      DATA IXMAXD(  10)/   8/
      DATA IXDELD(  10)/  17/
      DATA ISTARD(  10)/ 132/
      DATA NUMCOO(  10)/  32/
!
!     DEFINE CHARACTER   2561--UPPER CASE K
!
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
!
      DATA IXMIND(  11)/ -12/
      DATA IXMAXD(  11)/  12/
      DATA IXDELD(  11)/  24/
      DATA ISTARD(  11)/ 164/
      DATA NUMCOO(  11)/  53/
!
!     DEFINE CHARACTER   2562--UPPER CASE L
!
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
!
      DATA IXMIND(  12)/  -9/
      DATA IXMAXD(  12)/   9/
      DATA IXDELD(  12)/  18/
      DATA ISTARD(  12)/ 217/
      DATA NUMCOO(  12)/  38/
!
!     DEFINE CHARACTER   2563--UPPER CASE M
!
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
!
      DATA IXMIND(  13)/ -14/
      DATA IXMAXD(  13)/  14/
      DATA IXDELD(  13)/  28/
      DATA ISTARD(  13)/ 255/
      DATA NUMCOO(  13)/  43/
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCSU2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
!
!               **************************************
!               **  STEP 2--                        **
!               **  EXTRACT THE COORDINATES         **
!               **  FOR THIS PARTICULAR CHARACTER.  **
!               **************************************
!
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
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GO TO 9090
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
      END SUBROUTINE DRCSU2
      SUBROUTINE DRCSU3(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR ROMAN COMPLEX SCRIPT UPPER CASE (PART 3).
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
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
!     DEFINE CHARACTER   2564--UPPER CASE N
!
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
!
      DATA IXMIND(  14)/ -11/
      DATA IXMAXD(  14)/  12/
      DATA IXDELD(  14)/  23/
      DATA ISTARD(  14)/   1/
      DATA NUMCOO(  14)/  34/
!
!     DEFINE CHARACTER   2565--UPPER CASE O
!
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
!
      DATA IXMIND(  15)/ -10/
      DATA IXMAXD(  15)/  11/
      DATA IXDELD(  15)/  21/
      DATA ISTARD(  15)/  35/
      DATA NUMCOO(  15)/  32/
!
!     DEFINE CHARACTER   2566--UPPER CASE P
!
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
!
      DATA IXMIND(  16)/ -12/
      DATA IXMAXD(  16)/  11/
      DATA IXDELD(  16)/  23/
      DATA ISTARD(  16)/  67/
      DATA NUMCOO(  16)/  49/
!
!     DEFINE CHARACTER   2567--UPPER CASE Q
!
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
!
      DATA IXMIND(  17)/ -10/
      DATA IXMAXD(  17)/  11/
      DATA IXDELD(  17)/  21/
      DATA ISTARD(  17)/ 116/
      DATA NUMCOO(  17)/  41/
!
!     DEFINE CHARACTER   2568--UPPER CASE R
!
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
!
      DATA IXMIND(  18)/ -12/
      DATA IXMAXD(  18)/  12/
      DATA IXDELD(  18)/  24/
      DATA ISTARD(  18)/ 157/
      DATA NUMCOO(  18)/  59/
!
!     DEFINE CHARACTER   2569--UPPER CASE S
!
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
!
      DATA IXMIND(  19)/ -10/
      DATA IXMAXD(  19)/  10/
      DATA IXDELD(  19)/  20/
      DATA ISTARD(  19)/ 216/
      DATA NUMCOO(  19)/  34/
!
!     DEFINE CHARACTER   2570--UPPER CASE T
!
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
!
      DATA IXMIND(  20)/  -9/
      DATA IXMAXD(  20)/   9/
      DATA IXDELD(  20)/  18/
      DATA ISTARD(  20)/ 250/
      DATA NUMCOO(  20)/  35/
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCSU3--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
!
!               **************************************
!               **  STEP 2--                        **
!               **  EXTRACT THE COORDINATES         **
!               **  FOR THIS PARTICULAR CHARACTER.  **
!               **************************************
!
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
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GO TO 9090
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
      END SUBROUTINE DRCSU3
      SUBROUTINE DRCSU4(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR ROMAN COMPLEX SCRIPT UPPER CASE (PART 4).
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
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
!     DEFINE CHARACTER   2571--UPPER CASE U
!
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
!
      DATA IXMIND(  21)/ -11/
      DATA IXMAXD(  21)/  11/
      DATA IXDELD(  21)/  22/
      DATA ISTARD(  21)/   1/
      DATA NUMCOO(  21)/  35/
!
!     DEFINE CHARACTER   2572--UPPER CASE V
!
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
!
      DATA IXMIND(  22)/ -11/
      DATA IXMAXD(  22)/  10/
      DATA IXDELD(  22)/  21/
      DATA ISTARD(  22)/  36/
      DATA NUMCOO(  22)/  34/
!
!     DEFINE CHARACTER   2573--UPPER CASE W
!
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
!
      DATA IXMIND(  23)/ -12/
      DATA IXMAXD(  23)/  11/
      DATA IXDELD(  23)/  23/
      DATA ISTARD(  23)/  70/
      DATA NUMCOO(  23)/  35/
!
!     DEFINE CHARACTER   2574--UPPER CASE X
!
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
!
      DATA IXMIND(  24)/ -10/
      DATA IXMAXD(  24)/  10/
      DATA IXDELD(  24)/  20/
      DATA ISTARD(  24)/ 105/
      DATA NUMCOO(  24)/  49/
!
!     DEFINE CHARACTER   2575--UPPER CASE Y
!
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
!
      DATA IXMIND(  25)/ -11/
      DATA IXMAXD(  25)/  11/
      DATA IXDELD(  25)/  22/
      DATA ISTARD(  25)/ 154/
      DATA NUMCOO(  25)/  38/
!
!     DEFINE CHARACTER   2576--UPPER CASE Z
!
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
!
      DATA IXMIND(  26)/ -11/
      DATA IXMAXD(  26)/  10/
      DATA IXDELD(  26)/  21/
      DATA ISTARD(  26)/ 192/
      DATA NUMCOO(  26)/  41/
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='YES'
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
   51 FORMAT('***** AT THE BEGINNING OF DRCSU4--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
!
!               **************************************
!               **  STEP 2--                        **
!               **  EXTRACT THE COORDINATES         **
!               **  FOR THIS PARTICULAR CHARACTER.  **
!               **************************************
!
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
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GO TO 9090
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
      END SUBROUTINE DRCSU4

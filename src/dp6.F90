      DOUBLE PRECISION FUNCTION DASUM(N,DX,INCX)
!***BEGIN PROLOGUE  DASUM
!***DATE WRITTEN   791001   (YYMMDD)
!***REVISION DATE  820801   (YYMMDD)
!***CATEGORY NO.  D1A3A
!***KEYWORDS  ADD,BLAS,DOUBLE PRECISION,LINEAR ALGEBRA,MAGNITUDE,SUM,
!             VECTOR
!***AUTHOR  LAWSON, C. L., (JPL)
!           HANSON, R. J., (SNLA)
!           KINCAID, D. R., (U. OF TEXAS)
!           KROGH, F. T., (JPL)
!***PURPOSE  Sum of magnitudes of d.p. vector components
!***DESCRIPTION
!
!                B L A S  Subprogram
!    Description of Parameters
!
!     --Input--
!        N  number of elements in input vector(s)
!       DX  double precision vector with N elements
!     INCX  storage spacing between elements of DX
!
!     --Output--
!    DASUM  double precision result (zero if N .LE. 0)
!
!     Returns sum of magnitudes of double precision DX.
!     DASUM = sum from 0 to N-1 of DABS(DX(1+I*INCX))
!***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,
!                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*,
!                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL
!                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323
!***ROUTINES CALLED  (NONE)
!***END PROLOGUE  DASUM
!
      DOUBLE PRECISION DX(1)
!***FIRST EXECUTABLE STATEMENT  DASUM
      DASUM = 0.D0
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1)GO TO  20
!
!        CODE FOR INCREMENTS NOT EQUAL TO 1.
!
      NS = N*INCX
          DO 10 I=1,NS,INCX
          DASUM = DASUM + DABS(DX(I))
   10     CONTINUE
      RETURN
!
!        CODE FOR INCREMENTS EQUAL TO 1.
!
!
!        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 6.
!
   20 M = MOD(N,6)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
         DASUM = DASUM + DABS(DX(I))
   30 CONTINUE
      IF( N .LT. 6 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,6
         DASUM = DASUM + DABS(DX(I)) + DABS(DX(I+1)) + DABS(DX(I+2))   &
         + DABS(DX(I+3)) + DABS(DX(I+4)) + DABS(DX(I+5))
   50 CONTINUE
      RETURN
      END FUNCTION DASUM
      SUBROUTINE DASYIK (X, FNU, KODE, FLGIK, RA, ARG, IN, Y)
!***BEGIN PROLOGUE  DASYIK
!***SUBSIDIARY
!***PURPOSE  Subsidiary to DBESI and DBESK
!***LIBRARY   SLATEC
!***TYPE      DOUBLE PRECISION (ASYIK-S, DASYIK-D)
!***AUTHOR  Amos, D. E., (SNLA)
!***DESCRIPTION
!
!                    DASYIK computes Bessel functions I and K
!                  for arguments X.GT.0.0 and orders FNU.GE.35
!                  on FLGIK = 1 and FLGIK = -1 respectively.
!
!                                    INPUT
!
!      X    - Argument, X.GT.0.0D0
!      FNU  - Order of first Bessel function
!      KODE - A parameter to indicate the scaling option
!             KODE=1 returns Y(I)=        I/SUB(FNU+I-1)/(X), I=1,IN
!                    or      Y(I)=        K/SUB(FNU+I-1)/(X), I=1,IN
!                    on FLGIK = 1.0D0 or FLGIK = -1.0D0
!             KODE=2 returns Y(I)=EXP(-X)*I/SUB(FNU+I-1)/(X), I=1,IN
!                    or      Y(I)=EXP( X)*K/SUB(FNU+I-1)/(X), I=1,IN
!                    on FLGIK = 1.0D0 or FLGIK = -1.0D0
!     FLGIK - Selection parameter for I or K FUNCTION
!             FLGIK =  1.0D0 gives the I function
!             FLGIK = -1.0D0 gives the K function
!        RA - SQRT(1.+Z*Z), Z=X/FNU
!       ARG - Argument of the leading exponential
!        IN - Number of functions desired, IN=1 or 2
!
!                                    OUTPUT
!
!         Y - A vector whose first IN components contain the sequence
!
!     Abstract  **** A double precision routine ****
!         DASYIK implements the uniform asymptotic expansion of
!         the I and K Bessel functions for FNU.GE.35 and real
!         X.GT.0.0D0. The forms are identical except for a change
!         in sign of some of the terms. This change in sign is
!         accomplished by means of the FLAG FLGIK = 1 or -1.
!
!***SEE ALSO  DBESI, DBESK
!***ROUTINES CALLED  D1MACH
!***REVISION HISTORY  (YYMMDD)
!   750101  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890911  Removed unnecessary intrinsics.  (WRB)
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900328  Added TYPE section.  (WRB)
!   910408  Updated the AUTHOR section.  (WRB)
!***END PROLOGUE  DASYIK
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      INTEGER IN, J, JN, K, KK, KODE, L
      DOUBLE PRECISION AK,AP,ARG,C,COEF,CON,ETX,FLGIK,FN,FNU,GLN,RA,   &
       S1, S2, T, TOL, T2, X, Y, Z
      DIMENSION Y(*), C(65), CON(2)
      SAVE CON, C
      DATA CON(1), CON(2)  /   &
              3.98942280401432678D-01,    1.25331413731550025D+00/
      DATA C(1), C(2), C(3), C(4), C(5), C(6), C(7), C(8), C(9), C(10),   &
           C(11), C(12), C(13), C(14), C(15), C(16), C(17), C(18),   &
           C(19), C(20), C(21), C(22), C(23), C(24)/   &
             -2.08333333333333D-01,        1.25000000000000D-01,   &
              3.34201388888889D-01,       -4.01041666666667D-01,   &
              7.03125000000000D-02,       -1.02581259645062D+00,   &
              1.84646267361111D+00,       -8.91210937500000D-01,   &
              7.32421875000000D-02,        4.66958442342625D+00,   &
             -1.12070026162230D+01,        8.78912353515625D+00,   &
             -2.36408691406250D+00,        1.12152099609375D-01,   &
             -2.82120725582002D+01,        8.46362176746007D+01,   &
             -9.18182415432400D+01,        4.25349987453885D+01,   &
             -7.36879435947963D+00,        2.27108001708984D-01,   &
              2.12570130039217D+02,       -7.65252468141182D+02,   &
              1.05999045252800D+03,       -6.99579627376133D+02/
      DATA C(25), C(26), C(27), C(28), C(29), C(30), C(31), C(32),   &
           C(33), C(34), C(35), C(36), C(37), C(38), C(39), C(40),   &
           C(41), C(42), C(43), C(44), C(45), C(46), C(47), C(48)/   &
              2.18190511744212D+02,       -2.64914304869516D+01,   &
              5.72501420974731D-01,       -1.91945766231841D+03,   &
              8.06172218173731D+03,       -1.35865500064341D+04,   &
              1.16553933368645D+04,       -5.30564697861340D+03,   &
              1.20090291321635D+03,       -1.08090919788395D+02,   &
              1.72772750258446D+00,        2.02042913309661D+04,   &
             -9.69805983886375D+04,        1.92547001232532D+05,   &
             -2.03400177280416D+05,        1.22200464983017D+05,   &
             -4.11926549688976D+04,        7.10951430248936D+03,   &
             -4.93915304773088D+02,        6.07404200127348D+00,   &
             -2.42919187900551D+05,        1.31176361466298D+06,   &
             -2.99801591853811D+06,        3.76327129765640D+06/
      DATA C(49), C(50), C(51), C(52), C(53), C(54), C(55), C(56),   &
           C(57), C(58), C(59), C(60), C(61), C(62), C(63), C(64),   &
           C(65)/   &
             -2.81356322658653D+06,        1.26836527332162D+06,   &
             -3.31645172484564D+05,        4.52187689813627D+04,   &
             -2.49983048181121D+03,        2.43805296995561D+01,   &
              3.28446985307204D+06,       -1.97068191184322D+07,   &
              5.09526024926646D+07,       -7.41051482115327D+07,   &
              6.63445122747290D+07,       -3.75671766607634D+07,   &
              1.32887671664218D+07,       -2.78561812808645D+06,   &
              3.08186404612662D+05,       -1.38860897537170D+04,   &
              1.10017140269247D+02/
!***FIRST EXECUTABLE STATEMENT  DASYIK
      TOL = D1MACH(3)
      TOL = MAX(TOL,1.0D-15)
      FN = FNU
      Z  = (3.0D0-FLGIK)/2.0D0
      KK = INT(Z)
      DO 50 JN=1,IN
        IF (JN.EQ.1) GO TO 10
        FN = FN - FLGIK
        Z = X/FN
        RA = SQRT(1.0D0+Z*Z)
        GLN = LOG((1.0D0+RA)/Z)
        ETX = KODE - 1
        T = RA*(1.0D0-ETX) + ETX/(Z+RA)
        ARG = FN*(T-GLN)*FLGIK
   10   COEF = EXP(ARG)
        T = 1.0D0/RA
        T2 = T*T
        T = T/FN
        T = SIGN(T,FLGIK)
        S2 = 1.0D0
        AP = 1.0D0
        L = 0
        DO 30 K=2,11
          L = L + 1
          S1 = C(L)
          DO 20 J=2,K
            L = L + 1
            S1 = S1*T2 + C(L)
   20     CONTINUE
          AP = AP*T
          AK = AP*S1
          S2 = S2 + AK
          IF (MAX(ABS(AK),ABS(AP)) .LT.TOL) GO TO 40
   30   CONTINUE
   40   CONTINUE
      T = ABS(T)
      Y(JN) = S2*COEF*SQRT(T)*CON(KK)
   50 CONTINUE
      RETURN
      END SUBROUTINE DASYIK 
      FUNCTION DAWS (X)
!***BEGIN PROLOGUE  DAWS
!***PURPOSE  Compute Dawson's function.
!***LIBRARY   SLATEC (FNLIB)
!***CATEGORY  C8C
!***TYPE      SINGLE PRECISION (DAWS-S, DDAWS-D)
!***KEYWORDS  DAWSON'S FUNCTION, FNLIB, SPECIAL FUNCTIONS
!***AUTHOR  Fullerton, W., (LANL)
!***DESCRIPTION
!
! DAWS(X) calculates Dawson's integral for real argument X.
!
! Series for DAW        on the interval  0.          to  1.00000D+00
!                                        with weighted error   3.83E-17
!                                         log weighted error  16.42
!                               significant figures required  15.78
!                                    decimal places required  16.97
!
! Series for DAW2       on the interval  0.          to  1.60000D+01
!                                        with weighted error   5.17E-17
!                                         log weighted error  16.29
!                               significant figures required  15.90
!                                    decimal places required  17.02
!
! Series for DAWA       on the interval  0.          to  6.25000D-02
!                                        with weighted error   2.24E-17
!                                         log weighted error  16.65
!                               significant figures required  14.73
!                                    decimal places required  17.36
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  CSEVL, INITS, R1MACH, XERMSG
!***REVISION HISTORY  (YYMMDD)
!   780401  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890531  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!   920618  Removed space from variable names.  (RWC, WRB)
!***END PROLOGUE  DAWS
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DIMENSION DAWCS(13), DAW2CS(29), DAWACS(26)
      LOGICAL FIRST
      SAVE DAWCS, DAW2CS, DAWACS, NTDAW, NTDAW2, NTDAWA,   &
       XSML, XBIG, XMAX, FIRST
      DATA DAWCS( 1) /   -.006351734375145949E0 /
      DATA DAWCS( 2) /   -.22940714796773869E0 /
      DATA DAWCS( 3) /    .022130500939084764E0 /
      DATA DAWCS( 4) /   -.001549265453892985E0 /
      DATA DAWCS( 5) /    .000084973277156849E0 /
      DATA DAWCS( 6) /   -.000003828266270972E0 /
      DATA DAWCS( 7) /    .000000146285480625E0 /
      DATA DAWCS( 8) /   -.000000004851982381E0 /
      DATA DAWCS( 9) /    .000000000142146357E0 /
      DATA DAWCS(10) /   -.000000000003728836E0 /
      DATA DAWCS(11) /    .000000000000088549E0 /
      DATA DAWCS(12) /   -.000000000000001920E0 /
      DATA DAWCS(13) /    .000000000000000038E0 /
      DATA DAW2CS( 1) /   -.056886544105215527E0 /
      DATA DAW2CS( 2) /   -.31811346996168131E0 /
      DATA DAW2CS( 3) /    .20873845413642237E0 /
      DATA DAW2CS( 4) /   -.12475409913779131E0 /
      DATA DAW2CS( 5) /    .067869305186676777E0 /
      DATA DAW2CS( 6) /   -.033659144895270940E0 /
      DATA DAW2CS( 7) /    .015260781271987972E0 /
      DATA DAW2CS( 8) /   -.006348370962596214E0 /
      DATA DAW2CS( 9) /    .002432674092074852E0 /
      DATA DAW2CS(10) /   -.000862195414910650E0 /
      DATA DAW2CS(11) /    .000283765733363216E0 /
      DATA DAW2CS(12) /   -.000087057549874170E0 /
      DATA DAW2CS(13) /    .000024986849985481E0 /
      DATA DAW2CS(14) /   -.000006731928676416E0 /
      DATA DAW2CS(15) /    .000001707857878557E0 /
      DATA DAW2CS(16) /   -.000000409175512264E0 /
      DATA DAW2CS(17) /    .000000092828292216E0 /
      DATA DAW2CS(18) /   -.000000019991403610E0 /
      DATA DAW2CS(19) /    .000000004096349064E0 /
      DATA DAW2CS(20) /   -.000000000800324095E0 /
      DATA DAW2CS(21) /    .000000000149385031E0 /
      DATA DAW2CS(22) /   -.000000000026687999E0 /
      DATA DAW2CS(23) /    .000000000004571221E0 /
      DATA DAW2CS(24) /   -.000000000000751873E0 /
      DATA DAW2CS(25) /    .000000000000118931E0 /
      DATA DAW2CS(26) /   -.000000000000018116E0 /
      DATA DAW2CS(27) /    .000000000000002661E0 /
      DATA DAW2CS(28) /   -.000000000000000377E0 /
      DATA DAW2CS(29) /    .000000000000000051E0 /
      DATA DAWACS( 1) /    .01690485637765704E0 /
      DATA DAWACS( 2) /    .00868325227840695E0 /
      DATA DAWACS( 3) /    .00024248640424177E0 /
      DATA DAWACS( 4) /    .00001261182399572E0 /
      DATA DAWACS( 5) /    .00000106645331463E0 /
      DATA DAWACS( 6) /    .00000013581597947E0 /
      DATA DAWACS( 7) /    .00000002171042356E0 /
      DATA DAWACS( 8) /    .00000000286701050E0 /
      DATA DAWACS( 9) /   -.00000000019013363E0 /
      DATA DAWACS(10) /   -.00000000030977804E0 /
      DATA DAWACS(11) /   -.00000000010294148E0 /
      DATA DAWACS(12) /   -.00000000000626035E0 /
      DATA DAWACS(13) /    .00000000000856313E0 /
      DATA DAWACS(14) /    .00000000000303304E0 /
      DATA DAWACS(15) /   -.00000000000025236E0 /
      DATA DAWACS(16) /   -.00000000000042106E0 /
      DATA DAWACS(17) /   -.00000000000004431E0 /
      DATA DAWACS(18) /    .00000000000004911E0 /
      DATA DAWACS(19) /    .00000000000001235E0 /
      DATA DAWACS(20) /   -.00000000000000578E0 /
      DATA DAWACS(21) /   -.00000000000000228E0 /
      DATA DAWACS(22) /    .00000000000000076E0 /
      DATA DAWACS(23) /    .00000000000000038E0 /
      DATA DAWACS(24) /   -.00000000000000011E0 /
      DATA DAWACS(25) /   -.00000000000000006E0 /
      DATA DAWACS(26) /    .00000000000000002E0 /
      DATA FIRST /.TRUE./
!***FIRST EXECUTABLE STATEMENT  DAWS
      IF (FIRST) THEN
         EPS = R1MACH(3)
         NTDAW  = INITS (DAWCS,  13, 0.1*EPS)
         NTDAW2 = INITS (DAW2CS, 29, 0.1*EPS)
         NTDAWA = INITS (DAWACS, 26, 0.1*EPS)
!
         XSML = SQRT (1.5*EPS)
         XBIG = SQRT (0.5/EPS)
         XMAX = EXP (MIN (-LOG(2.*R1MACH(1)), LOG(R1MACH(2))) - 1.0)
      ENDIF
      FIRST = .FALSE.
!
      Y = ABS(X)
      IF (Y.GT.1.0) GO TO 20
!
      DAWS = X
      IF (Y.LE.XSML) RETURN
!
      DAWS = X * (0.75 + CSEVL (2.0*Y*Y-1.0, DAWCS, NTDAW))
      RETURN
!
 20   IF (Y.GT.4.0) GO TO 30
      DAWS = X * (0.25 + CSEVL (0.125*Y*Y-1.0, DAW2CS, NTDAW2))
      RETURN
!
 30   IF (Y.GT.XMAX) GO TO 40
      DAWS = 0.5/X
      IF (Y.GT.XBIG) RETURN
!
      DAWS = (0.5 + CSEVL (32.0/Y**2-1.0, DAWACS, NTDAWA)) / X
      RETURN
!
 40   CONTINUE
      WRITE(ICOUT,41)
      CALL DPWRST('XXX','BUG ')
   41 FORMAT('***** WARNING FROM DAWS, UNDERFLOW BECAUSE THE ',   &
             'ABSOLUTE VALUE OF X IS SO LARGE.  ****')
      DAWS = 0.0
      RETURN
!
      END FUNCTION DAWS 
      SUBROUTINE DAXPY(N,DA,DX,INCX,DY,INCY)
!
!     CONSTANT TIMES A VECTOR PLUS A VECTOR.
!     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
!     JACK DONGARRA, LINPACK, 3/11/78.
!
      DOUBLE PRECISION DX(*),DY(*),DA
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N
!
      IF(N.LE.0)RETURN
      IF (DA .EQ. 0.0D0) RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
!
!        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
!          NOT EQUAL TO 1
!
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        DY(IY) = DY(IY) + DA*DX(IX)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
!
!        CODE FOR BOTH INCREMENTS EQUAL TO 1
!
!
!        CLEAN-UP LOOP
!
   20 M = MOD(N,4)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        DY(I) = DY(I) + DA*DX(I)
   30 CONTINUE
      IF( N .LT. 4 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,4
        DY(I) = DY(I) + DA*DX(I)
        DY(I + 1) = DY(I + 1) + DA*DX(I + 1)
        DY(I + 2) = DY(I + 2) + DA*DX(I + 2)
        DY(I + 3) = DY(I + 3) + DA*DX(I + 3)
   50 CONTINUE
      RETURN
      END SUBROUTINE DAXPY
      SUBROUTINE DBESI (X, ALPHA, KODE, N, Y, NZ)
!***BEGIN PROLOGUE  DBESI
!***PURPOSE  Compute an N member sequence of I Bessel functions
!            I/SUB(ALPHA+K-1)/(X), K=1,...,N or scaled Bessel functions
!            EXP(-X)*I/SUB(ALPHA+K-1)/(X), K=1,...,N for nonnegative
!            ALPHA and X.
!***LIBRARY   SLATEC
!***CATEGORY  C10B3
!***TYPE      DOUBLE PRECISION (BESI-S, DBESI-D)
!***KEYWORDS  I BESSEL FUNCTION, SPECIAL FUNCTIONS
!***AUTHOR  Amos, D. E., (SNLA)
!           Daniel, S. L., (SNLA)
!***DESCRIPTION
!
!     Abstract  **** a double precision routine ****
!         DBESI computes an N member sequence of I Bessel functions
!         I/sub(ALPHA+K-1)/(X), K=1,...,N or scaled Bessel functions
!         EXP(-X)*I/sub(ALPHA+K-1)/(X), K=1,...,N for nonnegative ALPHA
!         and X.  A combination of the power series, the asymptotic
!         expansion for X to infinity, and the uniform asymptotic
!         expansion for NU to infinity are applied over subdivisions of
!         the (NU,X) plane.  For values not covered by one of these
!         formulae, the order is incremented by an integer so that one
!         of these formulae apply.  Backward recursion is used to reduce
!         orders by integer values.  The asymptotic expansion for X to
!         infinity is used only when the entire sequence (specifically
!         the last member) lies within the region covered by the
!         expansion.  Leading terms of these expansions are used to test
!         for over or underflow where appropriate.  If a sequence is
!         requested and the last member would underflow, the result is
!         set to zero and the next lower order tried, etc., until a
!         member comes on scale or all are set to zero.  An overflow
!         cannot occur with scaling.
!
!         The maximum number of significant digits obtainable
!         is the smaller of 14 and the number of digits carried in
!         double precision arithmetic.
!
!     Description of Arguments
!
!         Input      X,ALPHA are double precision
!           X      - X .GE. 0.0D0
!           ALPHA  - order of first member of the sequence,
!                    ALPHA .GE. 0.0D0
!           KODE   - a parameter to indicate the scaling option
!                    KODE=1 returns
!                           Y(K)=        I/sub(ALPHA+K-1)/(X),
!                                K=1,...,N
!                    KODE=2 returns
!                           Y(K)=EXP(-X)*I/sub(ALPHA+K-1)/(X),
!                                K=1,...,N
!           N      - number of members in the sequence, N .GE. 1
!
!         Output     Y is double precision
!           Y      - a vector whose first N components contain
!                    values for I/sub(ALPHA+K-1)/(X) or scaled
!                    values for EXP(-X)*I/sub(ALPHA+K-1)/(X),
!                    K=1,...,N depending on KODE
!           NZ     - number of components of Y set to zero due to
!                    underflow,
!                    NZ=0   , normal return, computation completed
!                    NZ .NE. 0, last NZ components of Y set to zero,
!                             Y(K)=0.0D0, K=N-NZ+1,...,N.
!
!     Error Conditions
!         Improper input arguments - a fatal error
!         Overflow with KODE=1 - a fatal error
!         Underflow - a non-fatal error(NZ .NE. 0)
!
!***REFERENCES  D. E. Amos, S. L. Daniel and M. K. Weston, CDC 6600
!                 subroutines IBESS and JBESS for Bessel functions
!                 I(NU,X) and J(NU,X), X .GE. 0, NU .GE. 0, ACM
!                 Transactions on Mathematical Software 3, (1977),
!                 pp. 76-92.
!               F. W. J. Olver, Tables of Bessel Functions of Moderate
!                 or Large Orders, NPL Mathematical Tables 6, Her
!                 Majesty's Stationery Office, London, 1962.
!***ROUTINES CALLED  D1MACH, DASYIK, DLNGAM, I1MACH, XERMSG
!***REVISION HISTORY  (YYMMDD)
!   750101  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890911  Removed unnecessary intrinsics.  (WRB)
!   890911  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!   900326  Removed duplicate information from DESCRIPTION section.
!           (WRB)
!   920501  Reformatted the REFERENCES section.  (WRB)
!***END PROLOGUE  DBESI
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      INTEGER I, IALP, IN, INLIM, IS, I1, K, KK, KM, KODE, KT,   &
              N, NN, NS, NZ
      INTEGER I1MACH
      DOUBLE PRECISION AIN,AK,AKM,ALPHA,ANS,AP,ARG,ATOL,TOLLN,DFN,   &
       DTM, DX, EARG, ELIM, ETX, FLGIK,FN, FNF, FNI,FNP1,FNU,GLN,RA,   &
       RTTPI, S, SX, SXO2, S1, S2, T, TA, TB, TEMP, TFN, TM, TOL,   &
       TRX, T2, X, XO2, XO2L, Y, Z
      DOUBLE PRECISION DLNGAM
      DIMENSION Y(*), TEMP(3)
      SAVE RTTPI, INLIM
      DATA RTTPI           / 3.98942280401433D-01/
      DATA INLIM           /          80         /
!***FIRST EXECUTABLE STATEMENT  DBESI
!
      NZ = 0
      KT = 1
      NS = 0
      KM = 0
      XO2L = 0.0D0
!
!     I1MACH(15) REPLACES I1MACH(12) IN A DOUBLE PRECISION CODE
!     I1MACH(14) REPLACES I1MACH(11) IN A DOUBLE PRECISION CODE
      RA = D1MACH(3)
      TOL = MAX(RA,1.0D-15)
      I1 = -I1MACH(15)
      GLN = D1MACH(5)
      ELIM = 2.303D0*(I1*GLN-3.0D0)
!     TOLLN = -LN(TOL)
      I1 = I1MACH(14)+1
      TOLLN = 2.303D0*GLN*I1
      TOLLN = MIN(TOLLN,34.5388D0)
!CCCC IF (N-1) 590, 10, 20
      IF (N-1.LT.0) THEN
         GO TO  590
      ELSEIF (N-1.EQ.0) THEN
         GO TO  10
      ELSEIF (N-1.GT.0) THEN
         GO TO  20
      ENDIF
   10 KT = 2
   20 NN = N
      IF (KODE.LT.1 .OR. KODE.GT.2) GO TO 570
!CCCC IF (X) 600, 30, 80
      IF (X.LT.0.0D0) THEN
         GO TO  600
      ELSEIF (X.EQ.0.0D0) THEN
         GO TO  30
      ELSEIF (X.GT.0.0D0) THEN
         GO TO  80
      ENDIF
   30 CONTINUE
!CCCC IF (ALPHA) 580, 40, 50
      IF (ALPHA.LT.0.0D0)THEN
         GO TO  580
      ELSEIF (ALPHA.EQ.0.0D0)THEN
         GO TO  40
      ELSEIF (ALPHA.GT.0.0D0)THEN
         GO TO  50
      ENDIF
   40 Y(1) = 1.0D0
      IF (N.EQ.1) RETURN
      I1 = 2
      GO TO 60
   50 I1 = 1
   60 DO 70 I=I1,N
        Y(I) = 0.0D0
   70 CONTINUE
      RETURN
   80 CONTINUE
      IF (ALPHA.LT.0.0D0) GO TO 580
!
      IALP = INT(ALPHA)
      FNI = IALP + N - 1
      FNF = ALPHA - IALP
      DFN = FNI + FNF
      FNU = DFN
      IN = 0
      XO2 = X*0.5D0
      SXO2 = XO2*XO2
      ETX = KODE - 1
      SX = ETX*X
!
!     DECISION TREE FOR REGION WHERE SERIES, ASYMPTOTIC EXPANSION FOR X
!     TO INFINITY AND ASYMPTOTIC EXPANSION FOR NU TO INFINITY ARE
!     APPLIED.
!
      IF (SXO2.LE.(FNU+1.0D0)) GO TO 90
      IF (X.LE.12.0D0) GO TO 110
      FN = 0.55D0*FNU*FNU
      FN = MAX(17.0D0,FN)
      IF (X.GE.FN) GO TO 430
      ANS = MAX(36.0D0-FNU,0.0D0)
      NS = INT(ANS)
      FNI = FNI + NS
      DFN = FNI + FNF
      FN = DFN
      IS = KT
      KM = N - 1 + NS
      IF (KM.GT.0) IS = 3
      GO TO 120
   90 FN = FNU
      FNP1 = FN + 1.0D0
      XO2L = LOG(XO2)
      IS = KT
      IF (X.LE.0.5D0) GO TO 230
      NS = 0
  100 FNI = FNI + NS
      DFN = FNI + FNF
      FN = DFN
      FNP1 = FN + 1.0D0
      IS = KT
      IF (N-1+NS.GT.0) IS = 3
      GO TO 230
  110 XO2L = LOG(XO2)
      NS = INT(SXO2-FNU)
      GO TO 100
  120 CONTINUE
!
!     OVERFLOW TEST ON UNIFORM ASYMPTOTIC EXPANSION
!
      IF (KODE.EQ.2) GO TO 130
      IF (ALPHA.LT.1.0D0) GO TO 150
      Z = X/ALPHA
      RA = SQRT(1.0D0+Z*Z)
      GLN = LOG((1.0D0+RA)/Z)
      T = RA*(1.0D0-ETX) + ETX/(Z+RA)
      ARG = ALPHA*(T-GLN)
      IF (ARG.GT.ELIM) GO TO 610
      IF (KM.EQ.0) GO TO 140
  130 CONTINUE
!
!     UNDERFLOW TEST ON UNIFORM ASYMPTOTIC EXPANSION
!
      Z = X/FN
      RA = SQRT(1.0D0+Z*Z)
      GLN = LOG((1.0D0+RA)/Z)
      T = RA*(1.0D0-ETX) + ETX/(Z+RA)
      ARG = FN*(T-GLN)
  140 IF (ARG.LT.(-ELIM)) GO TO 280
      GO TO 190
  150 IF (X.GT.ELIM) GO TO 610
      GO TO 130
!
!     UNIFORM ASYMPTOTIC EXPANSION FOR NU TO INFINITY
!
  160 IF (KM.NE.0) GO TO 170
      Y(1) = TEMP(3)
      RETURN
  170 TEMP(1) = TEMP(3)
      IN = NS
      KT = 1
      I1 = 0
  180 CONTINUE
      IS = 2
      FNI = FNI - 1.0D0
      DFN = FNI + FNF
      FN = DFN
      IF(I1.EQ.2) GO TO 350
      Z = X/FN
      RA = SQRT(1.0D0+Z*Z)
      GLN = LOG((1.0D0+RA)/Z)
      T = RA*(1.0D0-ETX) + ETX/(Z+RA)
      ARG = FN*(T-GLN)
  190 CONTINUE
      I1 = ABS(3-IS)
      I1 = MAX(I1,1)
      FLGIK = 1.0D0
      CALL DASYIK(X,FN,KODE,FLGIK,RA,ARG,I1,TEMP(IS))
      GO TO (180, 350, 510), IS
!
!     SERIES FOR (X/2)**2.LE.NU+1
!
  230 CONTINUE
      GLN = DLNGAM(FNP1)
      ARG = FN*XO2L - GLN - SX
      IF (ARG.LT.(-ELIM)) GO TO 300
      EARG = EXP(ARG)
  240 CONTINUE
      S = 1.0D0
      IF (X.LT.TOL) GO TO 260
      AK = 3.0D0
      T2 = 1.0D0
      T = 1.0D0
      S1 = FN
      DO 250 K=1,17
        S2 = T2 + S1
        T = T*SXO2/S2
        S = S + T
        IF (ABS(T).LT.TOL) GO TO 260
        T2 = T2 + AK
        AK = AK + 2.0D0
        S1 = S1 + FN
  250 CONTINUE
  260 CONTINUE
      TEMP(IS) = S*EARG
      GO TO (270, 350, 500), IS
  270 EARG = EARG*FN/XO2
      FNI = FNI - 1.0D0
      DFN = FNI + FNF
      FN = DFN
      IS = 2
      GO TO 240
!
!     SET UNDERFLOW VALUE AND UPDATE PARAMETERS
!
  280 Y(NN) = 0.0D0
      NN = NN - 1
      FNI = FNI - 1.0D0
      DFN = FNI + FNF
      FN = DFN
!CCCC IF (NN-1) 340, 290, 130
      IF (NN-1.LT.0) THEN
         GO TO  340
      ELSEIF (NN-1.EQ.0) THEN
         GO TO  290
      ELSEIF (NN-1.GT.0) THEN
         GO TO  130
      ENDIF
  290 KT = 2
      IS = 2
      GO TO 130
  300 Y(NN) = 0.0D0
      NN = NN - 1
      FNP1 = FN
      FNI = FNI - 1.0D0
      DFN = FNI + FNF
      FN = DFN
!CCCC IF (NN-1) 340, 310, 320
      IF (NN-1.LT.0)THEN
         GO TO 340
      ELSEIF(NN-1.EQ.0)THEN
         GO TO 310
      ELSE
         GO TO 320
      ENDIF
  310 KT = 2
      IS = 2
  320 IF (SXO2.LE.FNP1) GO TO 330
      GO TO 130
  330 ARG = ARG - XO2L + LOG(FNP1)
      IF (ARG.LT.(-ELIM)) GO TO 300
      GO TO 230
  340 NZ = N - NN
      RETURN
!
!     BACKWARD RECURSION SECTION
!
  350 CONTINUE
      NZ = N - NN
  360 CONTINUE
      IF(KT.EQ.2) GO TO 420
      S1 = TEMP(1)
      S2 = TEMP(2)
      TRX = 2.0D0/X
      DTM = FNI
      TM = (DTM+FNF)*TRX
      IF (IN.EQ.0) GO TO 390
!     BACKWARD RECUR TO INDEX ALPHA+NN-1
      DO 380 I=1,IN
        S = S2
        S2 = TM*S2 + S1
        S1 = S
        DTM = DTM - 1.0D0
        TM = (DTM+FNF)*TRX
  380 CONTINUE
      Y(NN) = S1
      IF (NN.EQ.1) RETURN
      Y(NN-1) = S2
      IF (NN.EQ.2) RETURN
      GO TO 400
  390 CONTINUE
!     BACKWARD RECUR FROM INDEX ALPHA+NN-1 TO ALPHA
      Y(NN) = S1
      Y(NN-1) = S2
      IF (NN.EQ.2) RETURN
  400 K = NN + 1
      DO 410 I=3,NN
        K = K - 1
        Y(K-2) = TM*Y(K-1) + Y(K)
        DTM = DTM - 1.0D0
        TM = (DTM+FNF)*TRX
  410 CONTINUE
      RETURN
  420 Y(1) = TEMP(2)
      RETURN
!
!     ASYMPTOTIC EXPANSION FOR X TO INFINITY
!
  430 CONTINUE
      EARG = RTTPI/SQRT(X)
      IF (KODE.EQ.2) GO TO 440
      IF (X.GT.ELIM) GO TO 610
      EARG = EARG*EXP(X)
  440 ETX = 8.0D0*X
      IS = KT
      IN = 0
      FN = FNU
  450 DX = FNI + FNI
      TM = 0.0D0
      IF (FNI.EQ.0.0D0 .AND. ABS(FNF).LT.TOL) GO TO 460
      TM = 4.0D0*FNF*(FNI+FNI+FNF)
  460 CONTINUE
      DTM = DX*DX
      S1 = ETX
      TRX = DTM - 1.0D0
      DX = -(TRX+TM)/ETX
      T = DX
      S = 1.0D0 + DX
      ATOL = TOL*ABS(S)
      S2 = 1.0D0
      AK = 8.0D0
      DO 470 K=1,25
        S1 = S1 + ETX
        S2 = S2 + AK
        DX = DTM - S2
        AP = DX + TM
        T = -T*AP/S1
        S = S + T
        IF (ABS(T).LE.ATOL) GO TO 480
        AK = AK + 8.0D0
  470 CONTINUE
  480 TEMP(IS) = S*EARG
      IF(IS.EQ.2) GO TO 360
      IS = 2
      FNI = FNI - 1.0D0
      DFN = FNI + FNF
      FN = DFN
      GO TO 450
!
!     BACKWARD RECURSION WITH NORMALIZATION BY
!     ASYMPTOTIC EXPANSION FOR NU TO INFINITY OR POWER SERIES.
!
  500 CONTINUE
!     COMPUTATION OF LAST ORDER FOR SERIES NORMALIZATION
      AKM = MAX(3.0D0-FN,0.0D0)
      KM = INT(AKM)
      TFN = FN + KM
      TA = (GLN+TFN-0.9189385332D0-0.0833333333D0/TFN)/(TFN+0.5D0)
      TA = XO2L - TA
      TB = -(1.0D0-1.0D0/TFN)/TFN
      AIN = TOLLN/(-TA+SQRT(TA*TA-TOLLN*TB)) + 1.5D0
      IN = INT(AIN)
      IN = IN + KM
      GO TO 520
  510 CONTINUE
!     COMPUTATION OF LAST ORDER FOR ASYMPTOTIC EXPANSION NORMALIZATION
      T = 1.0D0/(FN*RA)
      AIN = TOLLN/(GLN+SQRT(GLN*GLN+T*TOLLN)) + 1.5D0
      IN = INT(AIN)
      IF (IN.GT.INLIM) GO TO 160
  520 CONTINUE
      TRX = 2.0D0/X
      DTM = FNI + IN
      TM = (DTM+FNF)*TRX
      TA = 0.0D0
      TB = TOL
      KK = 1
  530 CONTINUE
!
!     BACKWARD RECUR UNINDEXED
!
      DO 540 I=1,IN
        S = TB
        TB = TM*TB + TA
        TA = S
        DTM = DTM - 1.0D0
        TM = (DTM+FNF)*TRX
  540 CONTINUE
!     NORMALIZATION
      IF (KK.NE.1) GO TO 550
      TA = (TA/TB)*TEMP(3)
      TB = TEMP(3)
      KK = 2
      IN = NS
      IF (NS.NE.0) GO TO 530
  550 Y(NN) = TB
      NZ = N - NN
      IF (NN.EQ.1) RETURN
      TB = TM*TB + TA
      K = NN - 1
      Y(K) = TB
      IF (NN.EQ.2) RETURN
      DTM = DTM - 1.0D0
      TM = (DTM+FNF)*TRX
      KM = K - 1
!
!     BACKWARD RECUR INDEXED
!
      DO 560 I=1,KM
        Y(K-1) = TM*Y(K) + Y(K+1)
        DTM = DTM - 1.0D0
        TM = (DTM+FNF)*TRX
        K = K - 1
  560 CONTINUE
      RETURN
!
!
!
  570 CONTINUE
      WRITE(ICOUT,571)
  571 FORMAT('***** ERORR FROM DBESI, KODE IS NOT 1 OR 2. ***')
      CALL DPWRST('XXX','BUG ')
      RETURN
  580 CONTINUE
      WRITE(ICOUT,581)
  581 FORMAT('***** ERORR FROM DBESI, THE ORDER ALPHA IS NEGATIVE. **')
      CALL DPWRST('XXX','BUG ')
      RETURN
  590 CONTINUE
      WRITE(ICOUT,591)
  591 FORMAT('***** ERORR FROM DBESI, N IS LESS THAN ONE.. ***')
      CALL DPWRST('XXX','BUG ')
      RETURN
  600 CONTINUE
      WRITE(ICOUT,601)
  601 FORMAT('***** ERORR FROM DBESI, X IS LESS THAN ZERO.. ***')
      CALL DPWRST('XXX','BUG ')
      RETURN
  610 CONTINUE
      WRITE(ICOUT,611)
  611 FORMAT('**** ERORR FROM DBESI, OVERFLOW BECAUSE X IS TOO BIG. *')
      CALL DPWRST('XXX','BUG ')
      RETURN
      END SUBROUTINE DBESI 
      DOUBLE PRECISION FUNCTION DBESI0 (X)
!***BEGIN PROLOGUE  DBESI0
!***PURPOSE  Compute the hyperbolic Bessel function of the first kind
!            of order zero.
!***LIBRARY   SLATEC (FNLIB)
!***CATEGORY  C10B1
!***TYPE      DOUBLE PRECISION (BESI0-S, DBESI0-D)
!***KEYWORDS  FIRST KIND, FNLIB, HYPERBOLIC BESSEL FUNCTION,
!             MODIFIED BESSEL FUNCTION, ORDER ZERO, SPECIAL FUNCTIONS
!***AUTHOR  Fullerton, W., (LANL)
!***DESCRIPTION
!
! DBESI0(X) calculates the double precision modified (hyperbolic)
! Bessel function of the first kind of order zero and double
! precision argument X.
!
! Series for BI0        on the interval  0.          to  9.00000E+00
!                                        with weighted error   9.51E-34
!                                         log weighted error  33.02
!                               significant figures required  33.31
!                                    decimal places required  33.65
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  D1MACH, DBSI0E, DCSEVL, INITDS, XERMSG
!***REVISION HISTORY  (YYMMDD)
!   770701  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890531  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!***END PROLOGUE  DBESI0
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DOUBLE PRECISION X, BI0CS(18), XMAX, XSML, Y,   &
        DCSEVL, DBSI0E
      LOGICAL FIRST
      SAVE BI0CS, NTI0, XSML, XMAX, FIRST
      DATA BI0CS(  1) / -.7660547252839144951081894976243285D-1   /
      DATA BI0CS(  2) / +.1927337953993808269952408750881196D+1   /
      DATA BI0CS(  3) / +.2282644586920301338937029292330415D+0   /
      DATA BI0CS(  4) / +.1304891466707290428079334210691888D-1   /
      DATA BI0CS(  5) / +.4344270900816487451378682681026107D-3   /
      DATA BI0CS(  6) / +.9422657686001934663923171744118766D-5   /
      DATA BI0CS(  7) / +.1434006289510691079962091878179957D-6   /
      DATA BI0CS(  8) / +.1613849069661749069915419719994611D-8   /
      DATA BI0CS(  9) / +.1396650044535669699495092708142522D-10  /
      DATA BI0CS( 10) / +.9579451725505445344627523171893333D-13  /
      DATA BI0CS( 11) / +.5333981859862502131015107744000000D-15  /
      DATA BI0CS( 12) / +.2458716088437470774696785919999999D-17  /
      DATA BI0CS( 13) / +.9535680890248770026944341333333333D-20  /
      DATA BI0CS( 14) / +.3154382039721427336789333333333333D-22  /
      DATA BI0CS( 15) / +.9004564101094637431466666666666666D-25  /
      DATA BI0CS( 16) / +.2240647369123670016000000000000000D-27  /
      DATA BI0CS( 17) / +.4903034603242837333333333333333333D-30  /
      DATA BI0CS( 18) / +.9508172606122666666666666666666666D-33  /
      DATA FIRST /.TRUE./
!***FIRST EXECUTABLE STATEMENT  DBESI0
      IF (FIRST) THEN
         NTI0 = INITDS (BI0CS, 18, 0.1*REAL(D1MACH(3)))
         XSML = SQRT(4.5D0*D1MACH(3))
         XMAX = LOG (D1MACH(2))
      ENDIF
      FIRST = .FALSE.
!
      Y = ABS(X)
      IF (Y.GT.3.0D0) GO TO 20
!
      DBESI0 = 1.0D0
      IF (Y.GT.XSML) DBESI0 = 2.75D0 + DCSEVL (Y*Y/4.5D0-1.D0, BI0CS,   &
        NTI0)
      RETURN
!
 20   CONTINUE
      IF (Y.GT.XMAX) THEN
        WRITE(ICOUT,1)
        CALL DPWRST('XXX','BUG ')
        DBESI0 = 0.0D0
        RETURN
      ENDIF
    1 FORMAT('***** ERORR FROM DBESI0, OVERFLOW BECAUSE THE ',   &
             'ABSOLUTE VALUE OF X IS TOO BIG.  ****')
!
      DBESI0 = EXP(Y) * DBSI0E(X)
!
      RETURN
      END FUNCTION DBESI0 
      DOUBLE PRECISION FUNCTION DBESI1 (X)
!***BEGIN PROLOGUE  DBESI1
!***PURPOSE  Compute the modified (hyperbolic) Bessel function of the
!            first kind of order one.
!***LIBRARY   SLATEC (FNLIB)
!***CATEGORY  C10B1
!***TYPE      DOUBLE PRECISION (BESI1-S, DBESI1-D)
!***KEYWORDS  FIRST KIND, FNLIB, HYPERBOLIC BESSEL FUNCTION,
!             MODIFIED BESSEL FUNCTION, ORDER ONE, SPECIAL FUNCTIONS
!***AUTHOR  Fullerton, W., (LANL)
!***DESCRIPTION
!
! DBESI1(X) calculates the double precision modified (hyperbolic)
! Bessel function of the first kind of order one and double precision
! argument X.
!
! Series for BI1        on the interval  0.          to  9.00000E+00
!                                        with weighted error   1.44E-32
!                                         log weighted error  31.84
!                               significant figures required  31.45
!                                    decimal places required  32.46
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  D1MACH, DBSI1E, DCSEVL, INITDS, XERMSG
!***REVISION HISTORY  (YYMMDD)
!   770701  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890531  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!***END PROLOGUE  DBESI1
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DOUBLE PRECISION X, BI1CS(17), XMAX, XMIN, XSML, Y,   &
        DCSEVL, DBSI1E
      LOGICAL FIRST
      SAVE BI1CS, NTI1, XMIN, XSML, XMAX, FIRST
      DATA BI1CS(  1) / -.19717132610998597316138503218149D-2     /
      DATA BI1CS(  2) / +.40734887667546480608155393652014D+0     /
      DATA BI1CS(  3) / +.34838994299959455866245037783787D-1     /
      DATA BI1CS(  4) / +.15453945563001236038598401058489D-2     /
      DATA BI1CS(  5) / +.41888521098377784129458832004120D-4     /
      DATA BI1CS(  6) / +.76490267648362114741959703966069D-6     /
      DATA BI1CS(  7) / +.10042493924741178689179808037238D-7     /
      DATA BI1CS(  8) / +.99322077919238106481371298054863D-10    /
      DATA BI1CS(  9) / +.76638017918447637275200171681349D-12    /
      DATA BI1CS( 10) / +.47414189238167394980388091948160D-14    /
      DATA BI1CS( 11) / +.24041144040745181799863172032000D-16    /
      DATA BI1CS( 12) / +.10171505007093713649121100799999D-18    /
      DATA BI1CS( 13) / +.36450935657866949458491733333333D-21    /
      DATA BI1CS( 14) / +.11205749502562039344810666666666D-23    /
      DATA BI1CS( 15) / +.29875441934468088832000000000000D-26    /
      DATA BI1CS( 16) / +.69732310939194709333333333333333D-29    /
      DATA BI1CS( 17) / +.14367948220620800000000000000000D-31    /
      DATA FIRST /.TRUE./
!***FIRST EXECUTABLE STATEMENT  DBESI1
      IF (FIRST) THEN
         NTI1 = INITDS (BI1CS, 17, 0.1*REAL(D1MACH(3)))
         XMIN = 2.0D0*D1MACH(1)
         XSML = SQRT(4.5D0*D1MACH(3))
         XMAX = LOG (D1MACH(2))
      ENDIF
      FIRST = .FALSE.
!
      Y = ABS(X)
      IF (Y.GT.3.0D0) GO TO 20
!
      DBESI1 = 0.D0
      IF (Y.EQ.0.D0)  RETURN
!
      IF (Y .LE. XMIN) THEN
        WRITE(ICOUT,2)
        CALL DPWRST('XXX','BUG ')
      ENDIF
    2 FORMAT('***** WARNING FROM DBESI1, UNDERFLOW BECAUSE THE ',   &
             'ABSOLUTE VALUE OF X IS SO SMALL.  ****')
      IF (Y.GT.XMIN) DBESI1 = 0.5D0*X
      IF (Y.GT.XSML) DBESI1 = X*(0.875D0 + DCSEVL (Y*Y/4.5D0-1.D0,   &
        BI1CS, NTI1))
      RETURN
!
 20   CONTINUE
      IF (Y.GT.XMAX) THEN
        WRITE(ICOUT,1)
        CALL DPWRST('XXX','BUG ')
        DBESI1 = 0.0
        RETURN
      ENDIF
    1 FORMAT('***** ERORR FROM DBESI1, OVERFLOW BECAUSE THE ',   &
             'ABSOLUTE VALUE OF X IS TOO BIG.  ****')
!
      DBESI1 = EXP(Y) * DBSI1E(X)
!
      RETURN
      END FUNCTION DBESI1 
      SUBROUTINE DBESK (X, FNU, KODE, N, Y, NZ)
!***BEGIN PROLOGUE  DBESK
!***PURPOSE  Implement forward recursion on the three term recursion
!            relation for a sequence of non-negative order Bessel
!            functions K/SUB(FNU+I-1)/(X), or scaled Bessel functions
!            EXP(X)*K/SUB(FNU+I-1)/(X), I=1,...,N for real, positive
!            X and non-negative orders FNU.
!***LIBRARY   SLATEC
!***CATEGORY  C10B3
!***TYPE      DOUBLE PRECISION (BESK-S, DBESK-D)
!***KEYWORDS  K BESSEL FUNCTION, SPECIAL FUNCTIONS
!***AUTHOR  Amos, D. E., (SNLA)
!***DESCRIPTION
!
!     Abstract  **** a double precision routine ****
!         DBESK implements forward recursion on the three term
!         recursion relation for a sequence of non-negative order Bessel
!         functions K/sub(FNU+I-1)/(X), or scaled Bessel functions
!         EXP(X)*K/sub(FNU+I-1)/(X), I=1,..,N for real X .GT. 0.0D0 and
!         non-negative orders FNU.  If FNU .LT. NULIM, orders FNU and
!         FNU+1 are obtained from DBSKNU to start the recursion.  If
!         FNU .GE. NULIM, the uniform asymptotic expansion is used for
!         orders FNU and FNU+1 to start the recursion.  NULIM is 35 or
!         70 depending on whether N=1 or N .GE. 2.  Under and overflow
!         tests are made on the leading term of the asymptotic expansion
!         before any extensive computation is done.
!
!         The maximum number of significant digits obtainable
!         is the smaller of 14 and the number of digits carried in
!         double precision arithmetic.
!
!     Description of Arguments
!
!         Input      X,FNU are double precision
!           X      - X .GT. 0.0D0
!           FNU    - order of the initial K function, FNU .GE. 0.0D0
!           KODE   - a parameter to indicate the scaling option
!                    KODE=1 returns Y(I)=       K/sub(FNU+I-1)/(X),
!                                        I=1,...,N
!                    KODE=2 returns Y(I)=EXP(X)*K/sub(FNU+I-1)/(X),
!                                        I=1,...,N
!           N      - number of members in the sequence, N .GE. 1
!
!         Output     Y is double precision
!           Y      - a vector whose first N components contain values
!                    for the sequence
!                    Y(I)=       k/sub(FNU+I-1)/(X), I=1,...,N  or
!                    Y(I)=EXP(X)*K/sub(FNU+I-1)/(X), I=1,...,N
!                    depending on KODE
!           NZ     - number of components of Y set to zero due to
!                    underflow with KODE=1,
!                    NZ=0   , normal return, computation completed
!                    NZ .NE. 0, first NZ components of Y set to zero
!                             due to underflow, Y(I)=0.0D0, I=1,...,NZ
!
!     Error Conditions
!         Improper input arguments - a fatal error
!         Overflow - a fatal error
!         Underflow with KODE=1 -  a non-fatal error (NZ .NE. 0)
!
!***REFERENCES  F. W. J. Olver, Tables of Bessel Functions of Moderate
!                 or Large Orders, NPL Mathematical Tables 6, Her
!                 Majesty's Stationery Office, London, 1962.
!               N. M. Temme, On the numerical evaluation of the modified
!                 Bessel function of the third kind, Journal of
!                 Computational Physics 19, (1975), pp. 324-337.
!***ROUTINES CALLED  D1MACH, DASYIK, DBESK0, DBESK1, DBSK0E, DBSK1E,
!                    DBSKNU, I1MACH, XERMSG
!***REVISION HISTORY  (YYMMDD)
!   790201  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890911  Removed unnecessary intrinsics.  (WRB)
!   890911  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!   920501  Reformatted the REFERENCES section.  (WRB)
!***END PROLOGUE  DBESK
!
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      INTEGER I, J, K, KODE, MZ, N, NB, ND, NN, NUD, NULIM, NZ
      DOUBLE PRECISION CN,DNU,ELIM,ETX,FLGIK,FN,FNN,FNU,GLN,GNU,RTZ,   &
       S, S1, S2, T, TM, TRX, W, X, XLIM, Y, ZN
      DOUBLE PRECISION DBESK0, DBESK1, DBSK1E, DBSK0E
      DIMENSION W(2), NULIM(2), Y(*)
      SAVE NULIM
      DATA NULIM(1),NULIM(2) / 35 , 70 /
!***FIRST EXECUTABLE STATEMENT  DBESK
!
      TRX=0.0D0
      TM=0.0D0
      S2=0.0D0
!
      NN = -I1MACH(15)
      ELIM = 2.303D0*(NN*D1MACH(5)-3.0D0)
      XLIM = D1MACH(1)*1.0D+3
      IF (KODE.LT.1 .OR. KODE.GT.2) GO TO 280
      IF (FNU.LT.0.0D0) GO TO 290
      IF (X.LE.0.0D0) GO TO 300
      IF (X.LT.XLIM) GO TO 320
      IF (N.LT.1) GO TO 310
      ETX = KODE - 1
!
!     ND IS A DUMMY VARIABLE FOR N
!     GNU IS A DUMMY VARIABLE FOR FNU
!     NZ = NUMBER OF UNDERFLOWS ON KODE=1
!
      ND = N
      NZ = 0
      NUD = INT(FNU)
      DNU = FNU - NUD
      GNU = FNU
      NN = MIN(2,ND)
      FN = FNU + N - 1
      FNN = FN
      IF (FN.LT.2.0D0) GO TO 150
!
!     OVERFLOW TEST  (LEADING EXPONENTIAL OF ASYMPTOTIC EXPANSION)
!     FOR THE LAST ORDER, FNU+N-1.GE.NULIM
!
      ZN = X/FN
      IF (ZN.EQ.0.0D0) GO TO 320
      RTZ = SQRT(1.0D0+ZN*ZN)
      GLN = LOG((1.0D0+RTZ)/ZN)
      T = RTZ*(1.0D0-ETX) + ETX/(ZN+RTZ)
      CN = -FN*(T-GLN)
      IF (CN.GT.ELIM) GO TO 320
      IF (NUD.LT.NULIM(NN)) GO TO 30
      IF (NN.EQ.1) GO TO 20
   10 CONTINUE
!
!     UNDERFLOW TEST (LEADING EXPONENTIAL OF ASYMPTOTIC EXPANSION)
!     FOR THE FIRST ORDER, FNU.GE.NULIM
!
      FN = GNU
      ZN = X/FN
      RTZ = SQRT(1.0D0+ZN*ZN)
      GLN = LOG((1.0D0+RTZ)/ZN)
      T = RTZ*(1.0D0-ETX) + ETX/(ZN+RTZ)
      CN = -FN*(T-GLN)
   20 CONTINUE
      IF (CN.LT.-ELIM) GO TO 230
!
!     ASYMPTOTIC EXPANSION FOR ORDERS FNU AND FNU+1.GE.NULIM
!
      FLGIK = -1.0D0
      CALL DASYIK(X,GNU,KODE,FLGIK,RTZ,CN,NN,Y)
      IF (NN.EQ.1) GO TO 240
      TRX = 2.0D0/X
      TM = (GNU+GNU+2.0D0)/X
      GO TO 130
!
   30 CONTINUE
      IF (KODE.EQ.2) GO TO 40
!
!     UNDERFLOW TEST (LEADING EXPONENTIAL OF ASYMPTOTIC EXPANSION IN X)
!     FOR ORDER DNU
!
      IF (X.GT.ELIM) GO TO 230
   40 CONTINUE
      IF (DNU.NE.0.0D0) GO TO 80
      IF (KODE.EQ.2) GO TO 50
      S1 = DBESK0(X)
      GO TO 60
   50 S1 = DBSK0E(X)
   60 CONTINUE
      IF (NUD.EQ.0 .AND. ND.EQ.1) GO TO 120
      IF (KODE.EQ.2) GO TO 70
      S2 = DBESK1(X)
      GO TO 90
   70 S2 = DBSK1E(X)
      GO TO 90
   80 CONTINUE
      NB = 2
      IF (NUD.EQ.0 .AND. ND.EQ.1) NB = 1
      CALL DBSKNU(X, DNU, KODE, NB, W, NZ)
      S1 = W(1)
      IF (NB.EQ.1) GO TO 120
      S2 = W(2)
   90 CONTINUE
      TRX = 2.0D0/X
      TM = (DNU+DNU+2.0D0)/X
!     FORWARD RECUR FROM DNU TO FNU+1 TO GET Y(1) AND Y(2)
      IF (ND.EQ.1) NUD = NUD - 1
      IF (NUD.GT.0) GO TO 100
      IF (ND.GT.1) GO TO 120
      S1 = S2
      GO TO 120
  100 CONTINUE
      DO 110 I=1,NUD
        S = S2
        S2 = TM*S2 + S1
        S1 = S
        TM = TM + TRX
  110 CONTINUE
      IF (ND.EQ.1) S1 = S2
  120 CONTINUE
      Y(1) = S1
      IF (ND.EQ.1) GO TO 240
      Y(2) = S2
  130 CONTINUE
      IF (ND.EQ.2) GO TO 240
!     FORWARD RECUR FROM FNU+2 TO FNU+N-1
      DO 140 I=3,ND
        Y(I) = TM*Y(I-1) + Y(I-2)
        TM = TM + TRX
  140 CONTINUE
      GO TO 240
!
  150 CONTINUE
!     UNDERFLOW TEST FOR KODE=1
      IF (KODE.EQ.2) GO TO 160
      IF (X.GT.ELIM) GO TO 230
  160 CONTINUE
!     OVERFLOW TEST
      IF (FN.LE.1.0D0) GO TO 170
      IF (-FN*(LOG(X)-0.693D0).GT.ELIM) GO TO 320
  170 CONTINUE
      IF (DNU.EQ.0.0D0) GO TO 180
      CALL DBSKNU(X, FNU, KODE, ND, Y, MZ)
      GO TO 240
  180 CONTINUE
      J = NUD
      IF (J.EQ.1) GO TO 210
      J = J + 1
      IF (KODE.EQ.2) GO TO 190
      Y(J) = DBESK0(X)
      GO TO 200
  190 Y(J) = DBSK0E(X)
  200 IF (ND.EQ.1) GO TO 240
      J = J + 1
  210 IF (KODE.EQ.2) GO TO 220
      Y(J) = DBESK1(X)
      GO TO 240
  220 Y(J) = DBSK1E(X)
      GO TO 240
!
!     UPDATE PARAMETERS ON UNDERFLOW
!
  230 CONTINUE
      NUD = NUD + 1
      ND = ND - 1
      IF (ND.EQ.0) GO TO 240
      NN = MIN(2,ND)
      GNU = GNU + 1.0D0
      IF (FNN.LT.2.0D0) GO TO 230
      IF (NUD.LT.NULIM(NN)) GO TO 230
      GO TO 10
  240 CONTINUE
      NZ = N - ND
      IF (NZ.EQ.0) RETURN
      IF (ND.EQ.0) GO TO 260
      DO 250 I=1,ND
        J = N - I + 1
        K = ND - I + 1
        Y(J) = Y(K)
  250 CONTINUE
  260 CONTINUE
      DO 270 I=1,NZ
        Y(I) = 0.0D0
  270 CONTINUE
      RETURN
!
!
!
  280 CONTINUE
!CCCC CALL XERMSG ('SLATEC', 'DBESK',
!CCCC+   'SCALING OPTION, KODE, NOT 1 OR 2', 2, 1)
!CCCC RETURN
!C290 CONTINUE
!CCCC CALL XERMSG ('SLATEC', 'DBESK', 'ORDER, FNU, LESS THAN ZERO', 2,
!CCCC+   1)
!CCCC RETURN
!C300 CONTINUE
!CCCC CALL XERMSG ('SLATEC', 'DBESK', 'X LESS THAN OR EQUAL TO ZERO',
!CCCC+   2, 1)
!CCCC RETURN
!C310 CONTINUE
!CCCC CALL XERMSG ('SLATEC', 'DBESK', 'N LESS THAN ONE', 2, 1)
!CCCC RETURN
!C320 CONTINUE
!CCCC CALL XERMSG ('SLATEC', 'DBESK',
!CCCC+   'OVERFLOW, FNU OR N TOO LARGE OR X TOO SMALL', 6, 1)
      WRITE(ICOUT,281)
  281 FORMAT('***** ERORR FROM DBESK, KODE IS NOT 1 OR 2. ***')
      CALL DPWRST('XXX','BUG ')
      RETURN
  290 CONTINUE
      WRITE(ICOUT,291)
  291 FORMAT('***** ERORR FROM DBESK, THE ORDER FNU IS NEGATIVE.')
      CALL DPWRST('XXX','BUG ')
      RETURN
  300 CONTINUE
      WRITE(ICOUT,301)
  301 FORMAT('**** ERORR FROM DBESK, X IS LESS THAN OR EQUAL TO ZERO.')
      CALL DPWRST('XXX','BUG ')
      RETURN
  310 CONTINUE
      WRITE(ICOUT,311)
  311 FORMAT('***** ERORR FROM DBESK, N IS LESS THAN ONE.')
      CALL DPWRST('XXX','BUG ')
      RETURN
  320 CONTINUE
      WRITE(ICOUT,321)
  321 FORMAT('***** ERORR FROM DBESK, OVERFLOW, FNU OR N TOO LARGE OR',   &
             ' X TOO SMALL.')
      CALL DPWRST('XXX','BUG ')
      RETURN
      END SUBROUTINE DBESK 
      DOUBLE PRECISION FUNCTION DBESK0 (X)
!***BEGIN PROLOGUE  DBESK0
!***PURPOSE  Compute the modified (hyperbolic) Bessel function of the
!            third kind of order zero.
!***LIBRARY   SLATEC (FNLIB)
!***CATEGORY  C10B1
!***TYPE      DOUBLE PRECISION (BESK0-S, DBESK0-D)
!***KEYWORDS  FNLIB, HYPERBOLIC BESSEL FUNCTION,
!             MODIFIED BESSEL FUNCTION, ORDER ZERO, SPECIAL FUNCTIONS,
!             THIRD KIND
!***AUTHOR  Fullerton, W., (LANL)
!***DESCRIPTION
!
! DBESK0(X) calculates the double precision modified (hyperbolic)
! Bessel function of the third kind of order zero for double
! precision argument X.  The argument must be greater than zero
! but not so large that the result underflows.
!
! Series for BK0        on the interval  0.          to  4.00000E+00
!                                        with weighted error   3.08E-33
!                                         log weighted error  32.51
!                               significant figures required  32.05
!                                    decimal places required  33.11
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  D1MACH, DBESI0, DBSK0E, DCSEVL, INITDS, XERMSG
!***REVISION HISTORY  (YYMMDD)
!   770701  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890531  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!***END PROLOGUE  DBESK0
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DOUBLE PRECISION X, BK0CS(16), XMAX, XMAXT, XSML, Y,   &
                       DCSEVL, DBESI0, DBSK0E
      LOGICAL FIRST
      SAVE BK0CS, NTK0, XSML, XMAX, FIRST
      DATA BK0CS(  1) / -.353273932339027687201140060063153D-1    /
      DATA BK0CS(  2) / +.344289899924628486886344927529213D+0    /
      DATA BK0CS(  3) / +.359799365153615016265721303687231D-1    /
      DATA BK0CS(  4) / +.126461541144692592338479508673447D-2    /
      DATA BK0CS(  5) / +.228621210311945178608269830297585D-4    /
      DATA BK0CS(  6) / +.253479107902614945730790013428354D-6    /
      DATA BK0CS(  7) / +.190451637722020885897214059381366D-8    /
      DATA BK0CS(  8) / +.103496952576336245851008317853089D-10   /
      DATA BK0CS(  9) / +.425981614279108257652445327170133D-13   /
      DATA BK0CS( 10) / +.137446543588075089694238325440000D-15   /
      DATA BK0CS( 11) / +.357089652850837359099688597333333D-18   /
      DATA BK0CS( 12) / +.763164366011643737667498666666666D-21   /
      DATA BK0CS( 13) / +.136542498844078185908053333333333D-23   /
      DATA BK0CS( 14) / +.207527526690666808319999999999999D-26   /
      DATA BK0CS( 15) / +.271281421807298560000000000000000D-29   /
      DATA BK0CS( 16) / +.308259388791466666666666666666666D-32   /
      DATA FIRST /.TRUE./
!***FIRST EXECUTABLE STATEMENT  DBESK0
      IF (FIRST) THEN
         NTK0 = INITDS (BK0CS, 16, 0.1*REAL(D1MACH(3)))
         XSML = SQRT(4.0D0*D1MACH(3))
         XMAXT = -LOG(D1MACH(1))
         XMAX = XMAXT - 0.5D0*XMAXT*LOG(XMAXT)/(XMAXT+0.5D0)
      ENDIF
      FIRST = .FALSE.
!
!CCCC IF (X .LE. 0.D0) CALL XERMSG ('SLATEC', 'DBESK0',
!CCCC+   'X IS ZERO OR NEGATIVE', 2, 2)
      IF (X .LE. 0.D0) THEN
        WRITE(ICOUT,1)
    1   FORMAT('***** ERORR FROM DBESK0, X IS ZERO OR NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        DBESK0 = 0.0
        RETURN
      ENDIF
      IF (X.GT.2.0D0) GO TO 20
!
      Y = 0.D0
      IF (X.GT.XSML) Y = X*X
      DBESK0 = -LOG(0.5D0*X)*DBESI0(X) - 0.25D0 + DCSEVL (.5D0*Y-1.D0,   &
        BK0CS, NTK0)
      RETURN
!
 20   DBESK0 = 0.D0
!CCCC IF (X .GT. XMAX) CALL XERMSG ('SLATEC', 'DBESK0',
!CCCC+   'X SO BIG K0 UNDERFLOWS', 1, 1)
      IF (X.GT.XMAX) THEN
        WRITE(ICOUT,2)
        CALL DPWRST('XXX','BUG ')
        DBESK0 = 0.0
        RETURN
      ENDIF
    2 FORMAT('***** ERORR FROM DBESK0, UNDERFLOWS BECAUSE THE ',   &
             'VALUE OF X IS TOO BIG.')
      IF (X.GT.XMAX) RETURN
!
      DBESK0 = EXP(-X) * DBSK0E(X)
!
      RETURN
      END FUNCTION DBESK0 
      DOUBLE PRECISION FUNCTION DBESK1 (X)
!***BEGIN PROLOGUE  DBESK1
!***PURPOSE  Compute the modified (hyperbolic) Bessel function of the
!            third kind of order one.
!***LIBRARY   SLATEC (FNLIB)
!***CATEGORY  C10B1
!***TYPE      DOUBLE PRECISION (BESK1-S, DBESK1-D)
!***KEYWORDS  FNLIB, HYPERBOLIC BESSEL FUNCTION,
!             MODIFIED BESSEL FUNCTION, ORDER ONE, SPECIAL FUNCTIONS,
!             THIRD KIND
!***AUTHOR  Fullerton, W., (LANL)
!***DESCRIPTION
!
! DBESK1(X) calculates the double precision modified (hyperbolic)
! Bessel function of the third kind of order one for double precision
! argument X.  The argument must be large enough that the result does
! not overflow and small enough that the result does not underflow.
!
! Series for BK1        on the interval  0.          to  4.00000E+00
!                                        with weighted error   9.16E-32
!                                         log weighted error  31.04
!                               significant figures required  30.61
!                                    decimal places required  31.64
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  D1MACH, DBESI1, DBSK1E, DCSEVL, INITDS, XERMSG
!***REVISION HISTORY  (YYMMDD)
!   770701  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890531  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!***END PROLOGUE  DBESK1
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DOUBLE PRECISION X, BK1CS(16), XMAX, XMAXT, XMIN, XSML, Y,   &
        DCSEVL, DBESI1, DBSK1E
      LOGICAL FIRST
      SAVE BK1CS, NTK1, XMIN, XSML, XMAX, FIRST
      DATA BK1CS(  1) / +.25300227338947770532531120868533D-1     /
      DATA BK1CS(  2) / -.35315596077654487566723831691801D+0     /
      DATA BK1CS(  3) / -.12261118082265714823479067930042D+0     /
      DATA BK1CS(  4) / -.69757238596398643501812920296083D-2     /
      DATA BK1CS(  5) / -.17302889575130520630176507368979D-3     /
      DATA BK1CS(  6) / -.24334061415659682349600735030164D-5     /
      DATA BK1CS(  7) / -.22133876307347258558315252545126D-7     /
      DATA BK1CS(  8) / -.14114883926335277610958330212608D-9     /
      DATA BK1CS(  9) / -.66669016941993290060853751264373D-12    /
      DATA BK1CS( 10) / -.24274498505193659339263196864853D-14    /
      DATA BK1CS( 11) / -.70238634793862875971783797120000D-17    /
      DATA BK1CS( 12) / -.16543275155100994675491029333333D-19    /
      DATA BK1CS( 13) / -.32338347459944491991893333333333D-22    /
      DATA BK1CS( 14) / -.53312750529265274999466666666666D-25    /
      DATA BK1CS( 15) / -.75130407162157226666666666666666D-28    /
      DATA BK1CS( 16) / -.91550857176541866666666666666666D-31    /
      DATA FIRST /.TRUE./
!***FIRST EXECUTABLE STATEMENT  DBESK1
      IF (FIRST) THEN
         NTK1 = INITDS (BK1CS, 16, 0.1*REAL(D1MACH(3)))
         XMIN = EXP(MAX(LOG(D1MACH(1)), -LOG(D1MACH(2))) + 0.01D0)
         XSML = SQRT(4.0D0*D1MACH(3))
         XMAXT = -LOG(D1MACH(1))
         XMAX = XMAXT - 0.5D0*XMAXT*LOG(XMAXT)/(XMAXT+0.5D0)
      ENDIF
      FIRST = .FALSE.
!
!CCCC IF (X .LE. 0.D0) CALL XERMSG ('SLATEC', 'DBESK1',
!CCCC+   'X IS ZERO OR NEGATIVE', 2, 2)
      IF (X .LE. 0.D0) THEN
        WRITE(ICOUT,1)
    1   FORMAT('***** ERORR FROM DBESK1, X ZERO OR NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        DBESK1=0.0D0
        RETURN
      ENDIF
      IF (X.GT.2.0D0) GO TO 20
!
!CCCC IF (X .LT. XMIN) CALL XERMSG ('SLATEC', 'DBESK1',
!CCCC+   'X SO SMALL K1 OVERFLOWS', 3, 2)
      IF (X .LE. XMIN) THEN
        WRITE(ICOUT,2)
        CALL DPWRST('XXX','BUG ')
      ENDIF
    2 FORMAT('***** WARNING FROM DBESK1, UNDERFLOW BECAUSE THE ',   &
             'VALUE OF X IS SO SMALL.')
      Y = 0.D0
      IF (X.GT.XSML) Y = X*X
      DBESK1 = LOG(0.5D0*X)*DBESI1(X) + (0.75D0 + DCSEVL (.5D0*Y-1.D0,   &
        BK1CS, NTK1))/X
      RETURN
!
 20   DBESK1 = 0.D0
!CCCC IF (X .GT. XMAX) CALL XERMSG ('SLATEC', 'DBESK1',
!CCCC+   'X SO BIG K1 UNDERFLOWS', 1, 1)
      IF (X.GT.XMAX) THEN
        WRITE(ICOUT,3)
        CALL DPWRST('XXX','BUG ')
        DBESK1 = 0.0D0
        RETURN
      ENDIF
    3 FORMAT('***** ERORR FROM DBESK1, UNDERFLOW BECAUSE THE ',   &
             'VALUE OF X IS TOO BIG.')
      IF (X.GT.XMAX) RETURN
!
      DBESK1 = EXP(-X) * DBSK1E(X)
!
      RETURN
      END FUNCTION DBESK1 
      DOUBLE PRECISION FUNCTION DBINOM(N,M)
!***BEGIN PROLOGUE  DBINOM
!***DATE WRITTEN   770601   (YYMMDD)
!***REVISION DATE  820801   (YYMMDD)
!***CATEGORY NO.  C1
!***KEYWORDS  BINOMIAL COEFFICIENTS,DOUBLE PRECISION,SPECIAL FUNCTION
!***AUTHOR  FULLERTON, W., (LANL)
!***PURPOSE  Computes the d.p. binomial coefficients.
!***DESCRIPTION
!
! DBINOM(N,M) calculates the double precision binomial coefficient
! for integer arguments N and M.  The result is (N!)/((M!)(N-M)!).
!***REFERENCES  (NONE)
!***ROUTINES CALLED  D1MACH,D9LGMC,DINT,DLNREL,XERROR
!***END PROLOGUE  DBINOM
      DOUBLE PRECISION CORR, FINTMX, SQ2PIL, XK, XN, XNK, DINT, D9LGMC,   &
        DLNREL
      REAL BILNMX
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DATA SQ2PIL / 0.91893853320467274178032973640562D0 /
      DATA BILNMX, FINTMX / 0.0, 0.0D0 /
!***FIRST EXECUTABLE STATEMENT  DBINOM
!
      DBINOM = 0.0D0
!
      IF (BILNMX.NE.0.0) GO TO 10
      BILNMX = DLOG(D1MACH(2)) - 0.0001D0
      FINTMX = 0.9D0/D1MACH(3)
!
 10   CONTINUE
      IF(N.LT.0)THEN
        WRITE(ICOUT,1)
 1      FORMAT('***** ERROR: FIRST ARGUMENT TO BINOM IS NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
      IF(M.LT.0)THEN
        WRITE(ICOUT,2)
 2      FORMAT('***** ERROR: SECOND ARGUMENT TO BINOM IS NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
      K = MIN0 (M, N-M)
      IF (K.GT.20) GO TO 30
!CCCC IF (FLOAT(K)*LOG(AMAX0(N,1)).GT.BILNMX) GO TO 30
      IF (FLOAT(K)*LOG(AMAX0(N,1)).GT.BILNMX) GO TO 30
!
      DBINOM = 1.0D0
      IF (K.EQ.0) GO TO 9000
      DO 20 I=1,K
        XN = N - I + 1
        XK = I
        DBINOM = DBINOM * (XN/XK)
 20   CONTINUE
!
      IF (DBINOM.LT.FINTMX) DBINOM = DINT (DBINOM+0.5D0)
      GO TO 9000
!
! IF K.LT.9, APPROX IS NOT VALID AND ANSWER IS CLOSE TO THE OVERFLOW LIM
 30   CONTINUE
      IF (K.LT.9) THEN
        WRITE(ICOUT,31)
 31     FORMAT('***** ERROR: BINOM OVERFLOWS BECAUSE ONE (OR BOTH) OF ',   &
               'THE ARGUMENTS IS TOO LARGE.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
      XN = N + 1
      XK = K + 1
      XNK = N - K + 1
!
      CORR = D9LGMC(XN) - D9LGMC(XK) - D9LGMC(XNK)
      DBINOM = XK*DLOG(XNK/XK) - XN*DLNREL(-(XK-1.0D0)/XN)   &
        -0.5D0*DLOG(XN*XNK/XK) + 1.0D0 - SQ2PIL + CORR
!
      IF (DBINOM.GT.DBLE(BILNMX)) THEN
!
        WRITE(ICOUT,41)
 41     FORMAT('***** ERROR: BINOM OVERFLOWS BECAUSE ONE (OR BOTH) OF ',   &
               'THE ARGUMENTS IS TOO LARGE.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
      DBINOM = DEXP (DBINOM)
      IF (DBINOM.LT.FINTMX) DBINOM = DINT (DBINOM+0.5D0)
!
 9000 CONTINUE
      RETURN
      END FUNCTION DBINOM
      DOUBLE PRECISION FUNCTION DBINLN(N,M)
!***BEGIN PROLOGUE  DBINOM
!***DATE WRITTEN   770601   (YYMMDD)
!***REVISION DATE  820801   (YYMMDD)
!***REVISION HISTORY (YYMMDD)
!   000601 Changed DINT to generic AINT        (RFB)
!***CATEGORY NO.  C1
!***KEYWORDS  BINOMIAL COEFFICIENTS,DOUBLE PRECISION,SPECIAL FUNCTION
!***AUTHOR  FULLERTON, W., (LANL)
!***PURPOSE  Computes the d.p. binomial coefficients.
!***DESCRIPTION
!
! DBINOM(N,M) calculates the double precision binomial coefficient
! for integer arguments N and M.  The result is (N!)/((M!)(N-M)!).
!***REFERENCES  (NONE)
!***ROUTINES CALLED  D1MACH,D9LGMC,AINT,DLNREL,XERROR
!***END PROLOGUE  DBINOM
!
!   NOTE: THIS IS THE BBINOM ROUTINE MODIFIED TO RETURN THE
!         LOG OF THE BINOMIAL COEFFICIENT.
!
!         THIS IS USED INTERNALLY FOR SOME DISCRETE PROBABILITY
!         DISTRIBUTIONS.
!
      DOUBLE PRECISION CORR, FINTMX, SQ2PIL, XK, XN, XNK, D9LGMC,   &
        DLNREL
      REAL BILNMX
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DATA SQ2PIL / 0.91893853320467274178032973640562D0 /
      DATA BILNMX, FINTMX / 0.0, 0.0D0 /
!***FIRST EXECUTABLE STATEMENT  DBINOM
!
      DBINLN = 0.0D0
!
      IF (BILNMX.NE.0.0) GO TO 10
      BILNMX = DLOG(D1MACH(2)) - 0.0001D0
      FINTMX = 0.9D0/D1MACH(3)
!
 10   CONTINUE
      IF(N.LT.0)THEN
        WRITE(ICOUT,1)
 1      FORMAT('***** ERROR: FIRST ARGUMENT TO DBINOM IS NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
      IF(M.LT.0)THEN
        WRITE(ICOUT,2)
 2      FORMAT('***** ERROR: SECOND ARGUMENT TO DBINOM IS NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
      IF (N.LT.M) THEN
        WRITE(ICOUT,3)
 3      FORMAT('***** ERROR: FIRST ARGUMENT TO DBINOM IS LESS THAN ',   &
               'SECOND ARGUMENT.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
!10   IF (N.LT.0 .OR. M.LT.0) CALL XERROR ( 'DBINOM  N OR M LT ZERO', 22
!CCCC1, 1, 2)
!CCCC IF (N.LT.M) CALL XERROR ( 'DBINOM  N LT M', 14, 2, 2)
!
      K = MIN0 (M, N-M)
      IF (K.GT.20) GO TO 30
      IF (FLOAT(K)*LOG(AMAX0(N,1)).GT.BILNMX) GO TO 30
!
      DBINLN = DLOG(1.0D0)
      IF (K.EQ.0) RETURN
      DO 20 I=1,K
        XN = N - I + 1
        XK = I
        DBINLN = DBINLN + DLOG((XN/XK))
 20   CONTINUE
!
!CCCC IF (DBINLN.LT.FINTMX) DBINLN = AINT (DBINLN+0.5D0)
      RETURN
!
! IF K.LT.9, APPROX IS NOT VALID AND ANSWER IS CLOSE TO THE OVERFLOW LIM
 30   CONTINUE
      IF (K.LT.9) THEN
        WRITE(ICOUT,31)
 31     FORMAT('***** ERROR: BINOM OVERFLOWS BECAUSE ONE (OR BOTH) OF ',   &
               'THE ARGUMENTS IS TOO LARGE.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
!30   IF (K.LT.9) CALL XERROR( 'DBINOM  RESULT OVERFLOWS BECAUSE N AND/O
!CCCC1R M TOO BIG', 51, 3, 2)
!
      XN = N + 1
      XK = K + 1
      XNK = N - K + 1
!
      CORR = D9LGMC(XN) - D9LGMC(XK) - D9LGMC(XNK)
      DBINLN = XK*DLOG(XNK/XK) - XN*DLNREL(-(XK-1.0D0)/XN)   &
        -0.5D0*DLOG(XN*XNK/XK) + 1.0D0 - SQ2PIL + CORR
!
!CCCC IF (DBINOM.GT.DBLE(BILNMX)) CALL XERROR ( 'DBINOM  RESULT OVERFLOW
!CCCC1S BECAUSE N AND/OR M TOO BIG', 51, 3,2)
!CCCC IF (DBINOM.GT.BILNMX) THEN
!
!CCCC   WRITE(ICOUT,41)
!41     FORMAT('***** ERROR: DBINOM OVERFLOWS BECAUSE ONE (OR BOTH) ',
!CCCC1         'OF THE ARGUMENTS IS TOO LARGE.')
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   GO TO 9000
!CCCC ENDIF
!
!CCCC DBINOM = DEXP (DBINLN)
!CCCC IF (DBINOM.LT.FINTMX) DBINOM = AINT (DBINOM+0.5D0)
!
 9000 CONTINUE
      RETURN
      END FUNCTION DBINLN
      DOUBLE PRECISION FUNCTION DBSI0E (X)
!***BEGIN PROLOGUE  DBSI0E
!***PURPOSE  Compute the exponentially scaled modified (hyperbolic)
!            Bessel function of the first kind of order zero.
!***LIBRARY   SLATEC (FNLIB)
!***CATEGORY  C10B1
!***TYPE      DOUBLE PRECISION (BESI0E-S, DBSI0E-D)
!***KEYWORDS  EXPONENTIALLY SCALED, FIRST KIND, FNLIB,
!             HYPERBOLIC BESSEL FUNCTION, MODIFIED BESSEL FUNCTION,
!             ORDER ZERO, SPECIAL FUNCTIONS
!***AUTHOR  Fullerton, W., (LANL)
!***DESCRIPTION
!
! DBSI0E(X) calculates the double precision exponentially scaled
! modified (hyperbolic) Bessel function of the first kind of order
! zero for double precision argument X.  The result is the Bessel
! function I0(X) multiplied by EXP(-ABS(X)).
!
! Series for BI0        on the interval  0.          to  9.00000E+00
!                                        with weighted error   9.51E-34
!                                         log weighted error  33.02
!                               significant figures required  33.31
!                                    decimal places required  33.65
!
! Series for AI0        on the interval  1.25000E-01 to  3.33333E-01
!                                        with weighted error   2.74E-32
!                                         log weighted error  31.56
!                               significant figures required  30.15
!                                    decimal places required  32.39
!
! Series for AI02       on the interval  0.          to  1.25000E-01
!                                        with weighted error   1.97E-32
!                                         log weighted error  31.71
!                               significant figures required  30.15
!                                    decimal places required  32.63
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  D1MACH, DCSEVL, INITDS
!***REVISION HISTORY  (YYMMDD)
!   770701  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890531  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!***END PROLOGUE  DBSI0E
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DOUBLE PRECISION X, BI0CS(18), AI0CS(46), AI02CS(69),   &
        XSML, Y, DCSEVL
      LOGICAL FIRST
      SAVE BI0CS, AI0CS, AI02CS, NTI0, NTAI0, NTAI02, XSML, FIRST
      DATA BI0CS(  1) / -.7660547252839144951081894976243285D-1   /
      DATA BI0CS(  2) / +.1927337953993808269952408750881196D+1   /
      DATA BI0CS(  3) / +.2282644586920301338937029292330415D+0   /
      DATA BI0CS(  4) / +.1304891466707290428079334210691888D-1   /
      DATA BI0CS(  5) / +.4344270900816487451378682681026107D-3   /
      DATA BI0CS(  6) / +.9422657686001934663923171744118766D-5   /
      DATA BI0CS(  7) / +.1434006289510691079962091878179957D-6   /
      DATA BI0CS(  8) / +.1613849069661749069915419719994611D-8   /
      DATA BI0CS(  9) / +.1396650044535669699495092708142522D-10  /
      DATA BI0CS( 10) / +.9579451725505445344627523171893333D-13  /
      DATA BI0CS( 11) / +.5333981859862502131015107744000000D-15  /
      DATA BI0CS( 12) / +.2458716088437470774696785919999999D-17  /
      DATA BI0CS( 13) / +.9535680890248770026944341333333333D-20  /
      DATA BI0CS( 14) / +.3154382039721427336789333333333333D-22  /
      DATA BI0CS( 15) / +.9004564101094637431466666666666666D-25  /
      DATA BI0CS( 16) / +.2240647369123670016000000000000000D-27  /
      DATA BI0CS( 17) / +.4903034603242837333333333333333333D-30  /
      DATA BI0CS( 18) / +.9508172606122666666666666666666666D-33  /
      DATA AI0CS(  1) / +.7575994494023795942729872037438D-1      /
      DATA AI0CS(  2) / +.7591380810823345507292978733204D-2      /
      DATA AI0CS(  3) / +.4153131338923750501863197491382D-3      /
      DATA AI0CS(  4) / +.1070076463439073073582429702170D-4      /
      DATA AI0CS(  5) / -.7901179979212894660750319485730D-5      /
      DATA AI0CS(  6) / -.7826143501438752269788989806909D-6      /
      DATA AI0CS(  7) / +.2783849942948870806381185389857D-6      /
      DATA AI0CS(  8) / +.8252472600612027191966829133198D-8      /
      DATA AI0CS(  9) / -.1204463945520199179054960891103D-7      /
      DATA AI0CS( 10) / +.1559648598506076443612287527928D-8      /
      DATA AI0CS( 11) / +.2292556367103316543477254802857D-9      /
      DATA AI0CS( 12) / -.1191622884279064603677774234478D-9      /
      DATA AI0CS( 13) / +.1757854916032409830218331247743D-10     /
      DATA AI0CS( 14) / +.1128224463218900517144411356824D-11     /
      DATA AI0CS( 15) / -.1146848625927298877729633876982D-11     /
      DATA AI0CS( 16) / +.2715592054803662872643651921606D-12     /
      DATA AI0CS( 17) / -.2415874666562687838442475720281D-13     /
      DATA AI0CS( 18) / -.6084469888255125064606099639224D-14     /
      DATA AI0CS( 19) / +.3145705077175477293708360267303D-14     /
      DATA AI0CS( 20) / -.7172212924871187717962175059176D-15     /
      DATA AI0CS( 21) / +.7874493403454103396083909603327D-16     /
      DATA AI0CS( 22) / +.1004802753009462402345244571839D-16     /
      DATA AI0CS( 23) / -.7566895365350534853428435888810D-17     /
      DATA AI0CS( 24) / +.2150380106876119887812051287845D-17     /
      DATA AI0CS( 25) / -.3754858341830874429151584452608D-18     /
      DATA AI0CS( 26) / +.2354065842226992576900757105322D-19     /
      DATA AI0CS( 27) / +.1114667612047928530226373355110D-19     /
      DATA AI0CS( 28) / -.5398891884396990378696779322709D-20     /
      DATA AI0CS( 29) / +.1439598792240752677042858404522D-20     /
      DATA AI0CS( 30) / -.2591916360111093406460818401962D-21     /
      DATA AI0CS( 31) / +.2238133183998583907434092298240D-22     /
      DATA AI0CS( 32) / +.5250672575364771172772216831999D-23     /
      DATA AI0CS( 33) / -.3249904138533230784173432285866D-23     /
      DATA AI0CS( 34) / +.9924214103205037927857284710400D-24     /
      DATA AI0CS( 35) / -.2164992254244669523146554299733D-24     /
      DATA AI0CS( 36) / +.3233609471943594083973332991999D-25     /
      DATA AI0CS( 37) / -.1184620207396742489824733866666D-26     /
      DATA AI0CS( 38) / -.1281671853950498650548338687999D-26     /
      DATA AI0CS( 39) / +.5827015182279390511605568853333D-27     /
      DATA AI0CS( 40) / -.1668222326026109719364501503999D-27     /
      DATA AI0CS( 41) / +.3625309510541569975700684800000D-28     /
      DATA AI0CS( 42) / -.5733627999055713589945958399999D-29     /
      DATA AI0CS( 43) / +.3736796722063098229642581333333D-30     /
      DATA AI0CS( 44) / +.1602073983156851963365512533333D-30     /
      DATA AI0CS( 45) / -.8700424864057229884522495999999D-31     /
      DATA AI0CS( 46) / +.2741320937937481145603413333333D-31     /
      DATA AI02CS(  1) / +.5449041101410883160789609622680D-1      /
      DATA AI02CS(  2) / +.3369116478255694089897856629799D-2      /
      DATA AI02CS(  3) / +.6889758346916823984262639143011D-4      /
      DATA AI02CS(  4) / +.2891370520834756482966924023232D-5      /
      DATA AI02CS(  5) / +.2048918589469063741827605340931D-6      /
      DATA AI02CS(  6) / +.2266668990498178064593277431361D-7      /
      DATA AI02CS(  7) / +.3396232025708386345150843969523D-8      /
      DATA AI02CS(  8) / +.4940602388224969589104824497835D-9      /
      DATA AI02CS(  9) / +.1188914710784643834240845251963D-10     /
      DATA AI02CS( 10) / -.3149916527963241364538648629619D-10     /
      DATA AI02CS( 11) / -.1321581184044771311875407399267D-10     /
      DATA AI02CS( 12) / -.1794178531506806117779435740269D-11     /
      DATA AI02CS( 13) / +.7180124451383666233671064293469D-12     /
      DATA AI02CS( 14) / +.3852778382742142701140898017776D-12     /
      DATA AI02CS( 15) / +.1540086217521409826913258233397D-13     /
      DATA AI02CS( 16) / -.4150569347287222086626899720156D-13     /
      DATA AI02CS( 17) / -.9554846698828307648702144943125D-14     /
      DATA AI02CS( 18) / +.3811680669352622420746055355118D-14     /
      DATA AI02CS( 19) / +.1772560133056526383604932666758D-14     /
      DATA AI02CS( 20) / -.3425485619677219134619247903282D-15     /
      DATA AI02CS( 21) / -.2827623980516583484942055937594D-15     /
      DATA AI02CS( 22) / +.3461222867697461093097062508134D-16     /
      DATA AI02CS( 23) / +.4465621420296759999010420542843D-16     /
      DATA AI02CS( 24) / -.4830504485944182071255254037954D-17     /
      DATA AI02CS( 25) / -.7233180487874753954562272409245D-17     /
      DATA AI02CS( 26) / +.9921475412173698598880460939810D-18     /
      DATA AI02CS( 27) / +.1193650890845982085504399499242D-17     /
      DATA AI02CS( 28) / -.2488709837150807235720544916602D-18     /
      DATA AI02CS( 29) / -.1938426454160905928984697811326D-18     /
      DATA AI02CS( 30) / +.6444656697373443868783019493949D-19     /
      DATA AI02CS( 31) / +.2886051596289224326481713830734D-19     /
      DATA AI02CS( 32) / -.1601954907174971807061671562007D-19     /
      DATA AI02CS( 33) / -.3270815010592314720891935674859D-20     /
      DATA AI02CS( 34) / +.3686932283826409181146007239393D-20     /
      DATA AI02CS( 35) / +.1268297648030950153013595297109D-22     /
      DATA AI02CS( 36) / -.7549825019377273907696366644101D-21     /
      DATA AI02CS( 37) / +.1502133571377835349637127890534D-21     /
      DATA AI02CS( 38) / +.1265195883509648534932087992483D-21     /
      DATA AI02CS( 39) / -.6100998370083680708629408916002D-22     /
      DATA AI02CS( 40) / -.1268809629260128264368720959242D-22     /
      DATA AI02CS( 41) / +.1661016099890741457840384874905D-22     /
      DATA AI02CS( 42) / -.1585194335765885579379705048814D-23     /
      DATA AI02CS( 43) / -.3302645405968217800953817667556D-23     /
      DATA AI02CS( 44) / +.1313580902839239781740396231174D-23     /
      DATA AI02CS( 45) / +.3689040246671156793314256372804D-24     /
      DATA AI02CS( 46) / -.4210141910461689149219782472499D-24     /
      DATA AI02CS( 47) / +.4791954591082865780631714013730D-25     /
      DATA AI02CS( 48) / +.8459470390221821795299717074124D-25     /
      DATA AI02CS( 49) / -.4039800940872832493146079371810D-25     /
      DATA AI02CS( 50) / -.6434714653650431347301008504695D-26     /
      DATA AI02CS( 51) / +.1225743398875665990344647369905D-25     /
      DATA AI02CS( 52) / -.2934391316025708923198798211754D-26     /
      DATA AI02CS( 53) / -.1961311309194982926203712057289D-26     /
      DATA AI02CS( 54) / +.1503520374822193424162299003098D-26     /
      DATA AI02CS( 55) / -.9588720515744826552033863882069D-28     /
      DATA AI02CS( 56) / -.3483339380817045486394411085114D-27     /
      DATA AI02CS( 57) / +.1690903610263043673062449607256D-27     /
      DATA AI02CS( 58) / +.1982866538735603043894001157188D-28     /
      DATA AI02CS( 59) / -.5317498081491816214575830025284D-28     /
      DATA AI02CS( 60) / +.1803306629888392946235014503901D-28     /
      DATA AI02CS( 61) / +.6213093341454893175884053112422D-29     /
      DATA AI02CS( 62) / -.7692189292772161863200728066730D-29     /
      DATA AI02CS( 63) / +.1858252826111702542625560165963D-29     /
      DATA AI02CS( 64) / +.1237585142281395724899271545541D-29     /
      DATA AI02CS( 65) / -.1102259120409223803217794787792D-29     /
      DATA AI02CS( 66) / +.1886287118039704490077874479431D-30     /
      DATA AI02CS( 67) / +.2160196872243658913149031414060D-30     /
      DATA AI02CS( 68) / -.1605454124919743200584465949655D-30     /
      DATA AI02CS( 69) / +.1965352984594290603938848073318D-31     /
      DATA FIRST /.TRUE./
!
      DBSI0E = 0.0D0
!
!***FIRST EXECUTABLE STATEMENT  DBSI0E
      IF (FIRST) THEN
         ETA = 0.1*REAL(D1MACH(3))
         NTI0 = INITDS (BI0CS, 18, ETA)
         NTAI0 = INITDS (AI0CS, 46, ETA)
         NTAI02 = INITDS (AI02CS, 69, ETA)
         XSML = SQRT(4.5D0*D1MACH(3))
      ENDIF
      FIRST = .FALSE.
!
      Y = ABS(X)
      IF (Y.GT.3.0D0) GO TO 20
!
      DBSI0E = 1.0D0 - X
      IF (Y.GT.XSML) DBSI0E = EXP(-Y) * (2.75D0 +   &
        DCSEVL (Y*Y/4.5D0-1.D0, BI0CS, NTI0) )
      RETURN
!
 20   IF (Y.LE.8.D0) DBSI0E = (0.375D0 + DCSEVL ((48.D0/Y-11.D0)/5.D0,   &
        AI0CS, NTAI0))/SQRT(Y)
      IF (Y.GT.8.D0) DBSI0E = (0.375D0 + DCSEVL (16.D0/Y-1.D0, AI02CS,   &
        NTAI02))/SQRT(Y)
!
      RETURN
      END FUNCTION DBSI0E 
      DOUBLE PRECISION FUNCTION DBSI1E (X)
!***BEGIN PROLOGUE  DBSI1E
!***PURPOSE  Compute the exponentially scaled modified (hyperbolic)
!            Bessel function of the first kind of order one.
!***LIBRARY   SLATEC (FNLIB)
!***CATEGORY  C10B1
!***TYPE      DOUBLE PRECISION (BESI1E-S, DBSI1E-D)
!***KEYWORDS  EXPONENTIALLY SCALED, FIRST KIND, FNLIB,
!             HYPERBOLIC BESSEL FUNCTION, MODIFIED BESSEL FUNCTION,
!             ORDER ONE, SPECIAL FUNCTIONS
!***AUTHOR  Fullerton, W., (LANL)
!***DESCRIPTION
!
! DBSI1E(X) calculates the double precision exponentially scaled
! modified (hyperbolic) Bessel function of the first kind of order
! one for double precision argument X.  The result is I1(X)
! multiplied by EXP(-ABS(X)).
!
! Series for BI1        on the interval  0.          to  9.00000E+00
!                                        with weighted error   1.44E-32
!                                         log weighted error  31.84
!                               significant figures required  31.45
!                                    decimal places required  32.46
!
! Series for AI1        on the interval  1.25000E-01 to  3.33333E-01
!                                        with weighted error   2.81E-32
!                                         log weighted error  31.55
!                               significant figures required  29.93
!                                    decimal places required  32.38
!
! Series for AI12       on the interval  0.          to  1.25000E-01
!                                        with weighted error   1.83E-32
!                                         log weighted error  31.74
!                               significant figures required  29.97
!                                    decimal places required  32.66
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  D1MACH, DCSEVL, INITDS, XERMSG
!***REVISION HISTORY  (YYMMDD)
!   770701  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890531  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!***END PROLOGUE  DBSI1E
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DOUBLE PRECISION X, BI1CS(17), AI1CS(46), AI12CS(69), XMIN,   &
        XSML, Y, DCSEVL
      LOGICAL FIRST
      SAVE BI1CS, AI1CS, AI12CS, NTI1, NTAI1, NTAI12, XMIN, XSML,   &
        FIRST
      DATA BI1CS(  1) / -.19717132610998597316138503218149D-2     /
      DATA BI1CS(  2) / +.40734887667546480608155393652014D+0     /
      DATA BI1CS(  3) / +.34838994299959455866245037783787D-1     /
      DATA BI1CS(  4) / +.15453945563001236038598401058489D-2     /
      DATA BI1CS(  5) / +.41888521098377784129458832004120D-4     /
      DATA BI1CS(  6) / +.76490267648362114741959703966069D-6     /
      DATA BI1CS(  7) / +.10042493924741178689179808037238D-7     /
      DATA BI1CS(  8) / +.99322077919238106481371298054863D-10    /
      DATA BI1CS(  9) / +.76638017918447637275200171681349D-12    /
      DATA BI1CS( 10) / +.47414189238167394980388091948160D-14    /
      DATA BI1CS( 11) / +.24041144040745181799863172032000D-16    /
      DATA BI1CS( 12) / +.10171505007093713649121100799999D-18    /
      DATA BI1CS( 13) / +.36450935657866949458491733333333D-21    /
      DATA BI1CS( 14) / +.11205749502562039344810666666666D-23    /
      DATA BI1CS( 15) / +.29875441934468088832000000000000D-26    /
      DATA BI1CS( 16) / +.69732310939194709333333333333333D-29    /
      DATA BI1CS( 17) / +.14367948220620800000000000000000D-31    /
      DATA AI1CS(  1) / -.2846744181881478674100372468307D-1      /
      DATA AI1CS(  2) / -.1922953231443220651044448774979D-1      /
      DATA AI1CS(  3) / -.6115185857943788982256249917785D-3      /
      DATA AI1CS(  4) / -.2069971253350227708882823777979D-4      /
      DATA AI1CS(  5) / +.8585619145810725565536944673138D-5      /
      DATA AI1CS(  6) / +.1049498246711590862517453997860D-5      /
      DATA AI1CS(  7) / -.2918338918447902202093432326697D-6      /
      DATA AI1CS(  8) / -.1559378146631739000160680969077D-7      /
      DATA AI1CS(  9) / +.1318012367144944705525302873909D-7      /
      DATA AI1CS( 10) / -.1448423418183078317639134467815D-8      /
      DATA AI1CS( 11) / -.2908512243993142094825040993010D-9      /
      DATA AI1CS( 12) / +.1266388917875382387311159690403D-9      /
      DATA AI1CS( 13) / -.1664947772919220670624178398580D-10     /
      DATA AI1CS( 14) / -.1666653644609432976095937154999D-11     /
      DATA AI1CS( 15) / +.1242602414290768265232168472017D-11     /
      DATA AI1CS( 16) / -.2731549379672432397251461428633D-12     /
      DATA AI1CS( 17) / +.2023947881645803780700262688981D-13     /
      DATA AI1CS( 18) / +.7307950018116883636198698126123D-14     /
      DATA AI1CS( 19) / -.3332905634404674943813778617133D-14     /
      DATA AI1CS( 20) / +.7175346558512953743542254665670D-15     /
      DATA AI1CS( 21) / -.6982530324796256355850629223656D-16     /
      DATA AI1CS( 22) / -.1299944201562760760060446080587D-16     /
      DATA AI1CS( 23) / +.8120942864242798892054678342860D-17     /
      DATA AI1CS( 24) / -.2194016207410736898156266643783D-17     /
      DATA AI1CS( 25) / +.3630516170029654848279860932334D-18     /
      DATA AI1CS( 26) / -.1695139772439104166306866790399D-19     /
      DATA AI1CS( 27) / -.1288184829897907807116882538222D-19     /
      DATA AI1CS( 28) / +.5694428604967052780109991073109D-20     /
      DATA AI1CS( 29) / -.1459597009090480056545509900287D-20     /
      DATA AI1CS( 30) / +.2514546010675717314084691334485D-21     /
      DATA AI1CS( 31) / -.1844758883139124818160400029013D-22     /
      DATA AI1CS( 32) / -.6339760596227948641928609791999D-23     /
      DATA AI1CS( 33) / +.3461441102031011111108146626560D-23     /
      DATA AI1CS( 34) / -.1017062335371393547596541023573D-23     /
      DATA AI1CS( 35) / +.2149877147090431445962500778666D-24     /
      DATA AI1CS( 36) / -.3045252425238676401746206173866D-25     /
      DATA AI1CS( 37) / +.5238082144721285982177634986666D-27     /
      DATA AI1CS( 38) / +.1443583107089382446416789503999D-26     /
      DATA AI1CS( 39) / -.6121302074890042733200670719999D-27     /
      DATA AI1CS( 40) / +.1700011117467818418349189802666D-27     /
      DATA AI1CS( 41) / -.3596589107984244158535215786666D-28     /
      DATA AI1CS( 42) / +.5448178578948418576650513066666D-29     /
      DATA AI1CS( 43) / -.2731831789689084989162564266666D-30     /
      DATA AI1CS( 44) / -.1858905021708600715771903999999D-30     /
      DATA AI1CS( 45) / +.9212682974513933441127765333333D-31     /
      DATA AI1CS( 46) / -.2813835155653561106370833066666D-31     /
      DATA AI12CS(  1) / +.2857623501828012047449845948469D-1      /
      DATA AI12CS(  2) / -.9761097491361468407765164457302D-2      /
      DATA AI12CS(  3) / -.1105889387626237162912569212775D-3      /
      DATA AI12CS(  4) / -.3882564808877690393456544776274D-5      /
      DATA AI12CS(  5) / -.2512236237870208925294520022121D-6      /
      DATA AI12CS(  6) / -.2631468846889519506837052365232D-7      /
      DATA AI12CS(  7) / -.3835380385964237022045006787968D-8      /
      DATA AI12CS(  8) / -.5589743462196583806868112522229D-9      /
      DATA AI12CS(  9) / -.1897495812350541234498925033238D-10     /
      DATA AI12CS( 10) / +.3252603583015488238555080679949D-10     /
      DATA AI12CS( 11) / +.1412580743661378133163366332846D-10     /
      DATA AI12CS( 12) / +.2035628544147089507224526136840D-11     /
      DATA AI12CS( 13) / -.7198551776245908512092589890446D-12     /
      DATA AI12CS( 14) / -.4083551111092197318228499639691D-12     /
      DATA AI12CS( 15) / -.2101541842772664313019845727462D-13     /
      DATA AI12CS( 16) / +.4272440016711951354297788336997D-13     /
      DATA AI12CS( 17) / +.1042027698412880276417414499948D-13     /
      DATA AI12CS( 18) / -.3814403072437007804767072535396D-14     /
      DATA AI12CS( 19) / -.1880354775510782448512734533963D-14     /
      DATA AI12CS( 20) / +.3308202310920928282731903352405D-15     /
      DATA AI12CS( 21) / +.2962628997645950139068546542052D-15     /
      DATA AI12CS( 22) / -.3209525921993423958778373532887D-16     /
      DATA AI12CS( 23) / -.4650305368489358325571282818979D-16     /
      DATA AI12CS( 24) / +.4414348323071707949946113759641D-17     /
      DATA AI12CS( 25) / +.7517296310842104805425458080295D-17     /
      DATA AI12CS( 26) / -.9314178867326883375684847845157D-18     /
      DATA AI12CS( 27) / -.1242193275194890956116784488697D-17     /
      DATA AI12CS( 28) / +.2414276719454848469005153902176D-18     /
      DATA AI12CS( 29) / +.2026944384053285178971922860692D-18     /
      DATA AI12CS( 30) / -.6394267188269097787043919886811D-19     /
      DATA AI12CS( 31) / -.3049812452373095896084884503571D-19     /
      DATA AI12CS( 32) / +.1612841851651480225134622307691D-19     /
      DATA AI12CS( 33) / +.3560913964309925054510270904620D-20     /
      DATA AI12CS( 34) / -.3752017947936439079666828003246D-20     /
      DATA AI12CS( 35) / -.5787037427074799345951982310741D-22     /
      DATA AI12CS( 36) / +.7759997511648161961982369632092D-21     /
      DATA AI12CS( 37) / -.1452790897202233394064459874085D-21     /
      DATA AI12CS( 38) / -.1318225286739036702121922753374D-21     /
      DATA AI12CS( 39) / +.6116654862903070701879991331717D-22     /
      DATA AI12CS( 40) / +.1376279762427126427730243383634D-22     /
      DATA AI12CS( 41) / -.1690837689959347884919839382306D-22     /
      DATA AI12CS( 42) / +.1430596088595433153987201085385D-23     /
      DATA AI12CS( 43) / +.3409557828090594020405367729902D-23     /
      DATA AI12CS( 44) / -.1309457666270760227845738726424D-23     /
      DATA AI12CS( 45) / -.3940706411240257436093521417557D-24     /
      DATA AI12CS( 46) / +.4277137426980876580806166797352D-24     /
      DATA AI12CS( 47) / -.4424634830982606881900283123029D-25     /
      DATA AI12CS( 48) / -.8734113196230714972115309788747D-25     /
      DATA AI12CS( 49) / +.4045401335683533392143404142428D-25     /
      DATA AI12CS( 50) / +.7067100658094689465651607717806D-26     /
      DATA AI12CS( 51) / -.1249463344565105223002864518605D-25     /
      DATA AI12CS( 52) / +.2867392244403437032979483391426D-26     /
      DATA AI12CS( 53) / +.2044292892504292670281779574210D-26     /
      DATA AI12CS( 54) / -.1518636633820462568371346802911D-26     /
      DATA AI12CS( 55) / +.8110181098187575886132279107037D-28     /
      DATA AI12CS( 56) / +.3580379354773586091127173703270D-27     /
      DATA AI12CS( 57) / -.1692929018927902509593057175448D-27     /
      DATA AI12CS( 58) / -.2222902499702427639067758527774D-28     /
      DATA AI12CS( 59) / +.5424535127145969655048600401128D-28     /
      DATA AI12CS( 60) / -.1787068401578018688764912993304D-28     /
      DATA AI12CS( 61) / -.6565479068722814938823929437880D-29     /
      DATA AI12CS( 62) / +.7807013165061145280922067706839D-29     /
      DATA AI12CS( 63) / -.1816595260668979717379333152221D-29     /
      DATA AI12CS( 64) / -.1287704952660084820376875598959D-29     /
      DATA AI12CS( 65) / +.1114548172988164547413709273694D-29     /
      DATA AI12CS( 66) / -.1808343145039336939159368876687D-30     /
      DATA AI12CS( 67) / -.2231677718203771952232448228939D-30     /
      DATA AI12CS( 68) / +.1619029596080341510617909803614D-30     /
      DATA AI12CS( 69) / -.1834079908804941413901308439210D-31     /
      DATA FIRST /.TRUE./
!
      DBSI1E = 0.0D0
!
!***FIRST EXECUTABLE STATEMENT  DBSI1E
      IF (FIRST) THEN
         ETA = 0.1*REAL(D1MACH(3))
         NTI1 = INITDS (BI1CS, 17, ETA)
         NTAI1 = INITDS (AI1CS, 46, ETA)
         NTAI12 = INITDS (AI12CS, 69, ETA)
!
         XMIN = 2.0D0*D1MACH(1)
         XSML = SQRT(4.5D0*D1MACH(3))
      ENDIF
      FIRST = .FALSE.
!
      Y = ABS(X)
      IF (Y.GT.3.0D0) GO TO 20
!
      DBSI1E = 0.0D0
      IF (Y.EQ.0.D0)  RETURN
!
      IF (Y .LE. XMIN) THEN
        WRITE(ICOUT,1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
    1 FORMAT('***** WARNING FROM DBSI1E, UNDERFLOW BECAUSE THE ',   &
             'ABSOLUTE VALUE OF X IS SO SMALL.  ****')
      IF (Y.GT.XMIN) DBSI1E = 0.5D0*X
      IF (Y.GT.XSML) DBSI1E = X*(0.875D0 + DCSEVL (Y*Y/4.5D0-1.D0,   &
        BI1CS, NTI1) )
      DBSI1E = EXP(-Y) * DBSI1E
      RETURN
!
 20   IF (Y.LE.8.D0) DBSI1E = (0.375D0 + DCSEVL ((48.D0/Y-11.D0)/5.D0,   &
        AI1CS, NTAI1))/SQRT(Y)
      IF (Y.GT.8.D0) DBSI1E = (0.375D0 + DCSEVL (16.D0/Y-1.D0, AI12CS,   &
        NTAI12))/SQRT(Y)
      DBSI1E = SIGN (DBSI1E, X)
!
      RETURN
      END FUNCTION DBSI1E 
      DOUBLE PRECISION FUNCTION DBSK0E (X)
!***BEGIN PROLOGUE  DBSK0E
!***PURPOSE  Compute the exponentially scaled modified (hyperbolic)
!            Bessel function of the third kind of order zero.
!***LIBRARY   SLATEC (FNLIB)
!***CATEGORY  C10B1
!***TYPE      DOUBLE PRECISION (BESK0E-S, DBSK0E-D)
!***KEYWORDS  EXPONENTIALLY SCALED, FNLIB, HYPERBOLIC BESSEL FUNCTION,
!             MODIFIED BESSEL FUNCTION, ORDER ZERO, SPECIAL FUNCTIONS,
!             THIRD KIND
!***AUTHOR  Fullerton, W., (LANL)
!***DESCRIPTION
!
! DBSK0E(X) computes the double precision exponentially scaled
! modified (hyperbolic) Bessel function of the third kind of
! order zero for positive double precision argument X.
!
! Series for BK0        on the interval  0.          to  4.00000E+00
!                                        with weighted error   3.08E-33
!                                         log weighted error  32.51
!                               significant figures required  32.05
!                                    decimal places required  33.11
!
! Series for AK0        on the interval  1.25000E-01 to  5.00000E-01
!                                        with weighted error   2.85E-32
!                                         log weighted error  31.54
!                               significant figures required  30.19
!                                    decimal places required  32.33
!
! Series for AK02       on the interval  0.          to  1.25000E-01
!                                        with weighted error   2.30E-32
!                                         log weighted error  31.64
!                               significant figures required  29.68
!                                    decimal places required  32.40
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  D1MACH, DBESI0, DCSEVL, INITDS, XERMSG
!***REVISION HISTORY  (YYMMDD)
!   770701  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890531  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!***END PROLOGUE  DBSK0E
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DOUBLE PRECISION X, BK0CS(16), AK0CS(38), AK02CS(33),   &
        XSML, Y, DCSEVL, DBESI0
      LOGICAL FIRST
      SAVE BK0CS, AK0CS, AK02CS, NTK0, NTAK0, NTAK02, XSML, FIRST
      DATA BK0CS(  1) / -.353273932339027687201140060063153D-1    /
      DATA BK0CS(  2) / +.344289899924628486886344927529213D+0    /
      DATA BK0CS(  3) / +.359799365153615016265721303687231D-1    /
      DATA BK0CS(  4) / +.126461541144692592338479508673447D-2    /
      DATA BK0CS(  5) / +.228621210311945178608269830297585D-4    /
      DATA BK0CS(  6) / +.253479107902614945730790013428354D-6    /
      DATA BK0CS(  7) / +.190451637722020885897214059381366D-8    /
      DATA BK0CS(  8) / +.103496952576336245851008317853089D-10   /
      DATA BK0CS(  9) / +.425981614279108257652445327170133D-13   /
      DATA BK0CS( 10) / +.137446543588075089694238325440000D-15   /
      DATA BK0CS( 11) / +.357089652850837359099688597333333D-18   /
      DATA BK0CS( 12) / +.763164366011643737667498666666666D-21   /
      DATA BK0CS( 13) / +.136542498844078185908053333333333D-23   /
      DATA BK0CS( 14) / +.207527526690666808319999999999999D-26   /
      DATA BK0CS( 15) / +.271281421807298560000000000000000D-29   /
      DATA BK0CS( 16) / +.308259388791466666666666666666666D-32   /
      DATA AK0CS(  1) / -.7643947903327941424082978270088D-1      /
      DATA AK0CS(  2) / -.2235652605699819052023095550791D-1      /
      DATA AK0CS(  3) / +.7734181154693858235300618174047D-3      /
      DATA AK0CS(  4) / -.4281006688886099464452146435416D-4      /
      DATA AK0CS(  5) / +.3081700173862974743650014826660D-5      /
      DATA AK0CS(  6) / -.2639367222009664974067448892723D-6      /
      DATA AK0CS(  7) / +.2563713036403469206294088265742D-7      /
      DATA AK0CS(  8) / -.2742705549900201263857211915244D-8      /
      DATA AK0CS(  9) / +.3169429658097499592080832873403D-9      /
      DATA AK0CS( 10) / -.3902353286962184141601065717962D-10     /
      DATA AK0CS( 11) / +.5068040698188575402050092127286D-11     /
      DATA AK0CS( 12) / -.6889574741007870679541713557984D-12     /
      DATA AK0CS( 13) / +.9744978497825917691388201336831D-13     /
      DATA AK0CS( 14) / -.1427332841884548505389855340122D-13     /
      DATA AK0CS( 15) / +.2156412571021463039558062976527D-14     /
      DATA AK0CS( 16) / -.3349654255149562772188782058530D-15     /
      DATA AK0CS( 17) / +.5335260216952911692145280392601D-16     /
      DATA AK0CS( 18) / -.8693669980890753807639622378837D-17     /
      DATA AK0CS( 19) / +.1446404347862212227887763442346D-17     /
      DATA AK0CS( 20) / -.2452889825500129682404678751573D-18     /
      DATA AK0CS( 21) / +.4233754526232171572821706342400D-19     /
      DATA AK0CS( 22) / -.7427946526454464195695341294933D-20     /
      DATA AK0CS( 23) / +.1323150529392666866277967462400D-20     /
      DATA AK0CS( 24) / -.2390587164739649451335981465599D-21     /
      DATA AK0CS( 25) / +.4376827585923226140165712554666D-22     /
      DATA AK0CS( 26) / -.8113700607345118059339011413333D-23     /
      DATA AK0CS( 27) / +.1521819913832172958310378154666D-23     /
      DATA AK0CS( 28) / -.2886041941483397770235958613333D-24     /
      DATA AK0CS( 29) / +.5530620667054717979992610133333D-25     /
      DATA AK0CS( 30) / -.1070377329249898728591633066666D-25     /
      DATA AK0CS( 31) / +.2091086893142384300296328533333D-26     /
      DATA AK0CS( 32) / -.4121713723646203827410261333333D-27     /
      DATA AK0CS( 33) / +.8193483971121307640135680000000D-28     /
      DATA AK0CS( 34) / -.1642000275459297726780757333333D-28     /
      DATA AK0CS( 35) / +.3316143281480227195890346666666D-29     /
      DATA AK0CS( 36) / -.6746863644145295941085866666666D-30     /
      DATA AK0CS( 37) / +.1382429146318424677635413333333D-30     /
      DATA AK0CS( 38) / -.2851874167359832570811733333333D-31     /
      DATA AK02CS(  1) / -.1201869826307592239839346212452D-1      /
      DATA AK02CS(  2) / -.9174852691025695310652561075713D-2      /
      DATA AK02CS(  3) / +.1444550931775005821048843878057D-3      /
      DATA AK02CS(  4) / -.4013614175435709728671021077879D-5      /
      DATA AK02CS(  5) / +.1567831810852310672590348990333D-6      /
      DATA AK02CS(  6) / -.7770110438521737710315799754460D-8      /
      DATA AK02CS(  7) / +.4611182576179717882533130529586D-9      /
      DATA AK02CS(  8) / -.3158592997860565770526665803309D-10     /
      DATA AK02CS(  9) / +.2435018039365041127835887814329D-11     /
      DATA AK02CS( 10) / -.2074331387398347897709853373506D-12     /
      DATA AK02CS( 11) / +.1925787280589917084742736504693D-13     /
      DATA AK02CS( 12) / -.1927554805838956103600347182218D-14     /
      DATA AK02CS( 13) / +.2062198029197818278285237869644D-15     /
      DATA AK02CS( 14) / -.2341685117579242402603640195071D-16     /
      DATA AK02CS( 15) / +.2805902810643042246815178828458D-17     /
      DATA AK02CS( 16) / -.3530507631161807945815482463573D-18     /
      DATA AK02CS( 17) / +.4645295422935108267424216337066D-19     /
      DATA AK02CS( 18) / -.6368625941344266473922053461333D-20     /
      DATA AK02CS( 19) / +.9069521310986515567622348800000D-21     /
      DATA AK02CS( 20) / -.1337974785423690739845005311999D-21     /
      DATA AK02CS( 21) / +.2039836021859952315522088960000D-22     /
      DATA AK02CS( 22) / -.3207027481367840500060869973333D-23     /
      DATA AK02CS( 23) / +.5189744413662309963626359466666D-24     /
      DATA AK02CS( 24) / -.8629501497540572192964607999999D-25     /
      DATA AK02CS( 25) / +.1472161183102559855208038400000D-25     /
      DATA AK02CS( 26) / -.2573069023867011283812351999999D-26     /
      DATA AK02CS( 27) / +.4601774086643516587376640000000D-27     /
      DATA AK02CS( 28) / -.8411555324201093737130666666666D-28     /
      DATA AK02CS( 29) / +.1569806306635368939301546666666D-28     /
      DATA AK02CS( 30) / -.2988226453005757788979199999999D-29     /
      DATA AK02CS( 31) / +.5796831375216836520618666666666D-30     /
      DATA AK02CS( 32) / -.1145035994347681332155733333333D-30     /
      DATA AK02CS( 33) / +.2301266594249682802005333333333D-31     /
      DATA FIRST /.TRUE./
!***FIRST EXECUTABLE STATEMENT  DBSK0E
!
      DBSK0E=0.0D0
!
      IF (FIRST) THEN
         ETA = 0.1*REAL(D1MACH(3))
         NTK0 = INITDS (BK0CS, 16, ETA)
         NTAK0 = INITDS (AK0CS, 38, ETA)
         NTAK02 = INITDS (AK02CS, 33, ETA)
         XSML = SQRT(4.0D0*D1MACH(3))
      ENDIF
      FIRST = .FALSE.
!
!CCCC IF (X .LE. 0.D0) CALL XERMSG ('SLATEC', 'DBSK0E',
!CCCC+   'X IS ZERO OR NEGATIVE', 2, 2)
      IF (X .LE. 0.D0) THEN
        WRITE(ICOUT,1)
    1   FORMAT('***** ERORR FROM DBSK0E, X ZERO OR NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        DBSK0E=0.0D0
        RETURN
      ENDIF
      IF (X.GT.2.0D0) GO TO 20
!
      Y = 0.D0
      IF (X.GT.XSML) Y = X*X
      DBSK0E = EXP(X)*(-LOG(0.5D0*X)*DBESI0(X) - 0.25D0 +   &
        DCSEVL (.5D0*Y-1.D0, BK0CS, NTK0))
      RETURN
!
 20   IF (X.LE.8.D0) DBSK0E = (1.25D0 + DCSEVL ((16.D0/X-5.D0)/3.D0,   &
        AK0CS, NTAK0))/SQRT(X)
      IF (X.GT.8.D0) DBSK0E = (1.25D0 +   &
        DCSEVL (16.D0/X-1.D0, AK02CS, NTAK02))/SQRT(X)
!
      RETURN
      END FUNCTION DBSK0E 
      DOUBLE PRECISION FUNCTION DBSK1E (X)
!***BEGIN PROLOGUE  DBSK1E
!***PURPOSE  Compute the exponentially scaled modified (hyperbolic)
!            Bessel function of the third kind of order one.
!***LIBRARY   SLATEC (FNLIB)
!***CATEGORY  C10B1
!***TYPE      DOUBLE PRECISION (BESK1E-S, DBSK1E-D)
!***KEYWORDS  EXPONENTIALLY SCALED, FNLIB, HYPERBOLIC BESSEL FUNCTION,
!             MODIFIED BESSEL FUNCTION, ORDER ONE, SPECIAL FUNCTIONS,
!             THIRD KIND
!***AUTHOR  Fullerton, W., (LANL)
!***DESCRIPTION
!
! DBSK1E(S) computes the double precision exponentially scaled
! modified (hyperbolic) Bessel function of the third kind of order
! one for positive double precision argument X.
!
! Series for BK1        on the interval  0.          to  4.00000E+00
!                                        with weighted error   9.16E-32
!                                         log weighted error  31.04
!                               significant figures required  30.61
!                                    decimal places required  31.64
!
! Series for AK1        on the interval  1.25000E-01 to  5.00000E-01
!                                        with weighted error   3.07E-32
!                                         log weighted error  31.51
!                               significant figures required  30.71
!                                    decimal places required  32.30
!
! Series for AK12       on the interval  0.          to  1.25000E-01
!                                        with weighted error   2.41E-32
!                                         log weighted error  31.62
!                               significant figures required  30.25
!                                    decimal places required  32.38
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  D1MACH, DBESI1, DCSEVL, INITDS, XERMSG
!***REVISION HISTORY  (YYMMDD)
!   770701  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890531  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!***END PROLOGUE  DBSK1E
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DOUBLE PRECISION X, BK1CS(16), AK1CS(38), AK12CS(33), XMIN,   &
        XSML, Y, DCSEVL, DBESI1
      LOGICAL FIRST
      SAVE BK1CS, AK1CS, AK12CS, NTK1, NTAK1, NTAK12, XMIN, XSML,   &
        FIRST
      DATA BK1CS(  1) / +.25300227338947770532531120868533D-1     /
      DATA BK1CS(  2) / -.35315596077654487566723831691801D+0     /
      DATA BK1CS(  3) / -.12261118082265714823479067930042D+0     /
      DATA BK1CS(  4) / -.69757238596398643501812920296083D-2     /
      DATA BK1CS(  5) / -.17302889575130520630176507368979D-3     /
      DATA BK1CS(  6) / -.24334061415659682349600735030164D-5     /
      DATA BK1CS(  7) / -.22133876307347258558315252545126D-7     /
      DATA BK1CS(  8) / -.14114883926335277610958330212608D-9     /
      DATA BK1CS(  9) / -.66669016941993290060853751264373D-12    /
      DATA BK1CS( 10) / -.24274498505193659339263196864853D-14    /
      DATA BK1CS( 11) / -.70238634793862875971783797120000D-17    /
      DATA BK1CS( 12) / -.16543275155100994675491029333333D-19    /
      DATA BK1CS( 13) / -.32338347459944491991893333333333D-22    /
      DATA BK1CS( 14) / -.53312750529265274999466666666666D-25    /
      DATA BK1CS( 15) / -.75130407162157226666666666666666D-28    /
      DATA BK1CS( 16) / -.91550857176541866666666666666666D-31    /
      DATA AK1CS(  1) / +.27443134069738829695257666227266D+0     /
      DATA AK1CS(  2) / +.75719899531993678170892378149290D-1     /
      DATA AK1CS(  3) / -.14410515564754061229853116175625D-2     /
      DATA AK1CS(  4) / +.66501169551257479394251385477036D-4     /
      DATA AK1CS(  5) / -.43699847095201407660580845089167D-5     /
      DATA AK1CS(  6) / +.35402774997630526799417139008534D-6     /
      DATA AK1CS(  7) / -.33111637792932920208982688245704D-7     /
      DATA AK1CS(  8) / +.34459775819010534532311499770992D-8     /
      DATA AK1CS(  9) / -.38989323474754271048981937492758D-9     /
      DATA AK1CS( 10) / +.47208197504658356400947449339005D-10    /
      DATA AK1CS( 11) / -.60478356628753562345373591562890D-11    /
      DATA AK1CS( 12) / +.81284948748658747888193837985663D-12    /
      DATA AK1CS( 13) / -.11386945747147891428923915951042D-12    /
      DATA AK1CS( 14) / +.16540358408462282325972948205090D-13    /
      DATA AK1CS( 15) / -.24809025677068848221516010440533D-14    /
      DATA AK1CS( 16) / +.38292378907024096948429227299157D-15    /
      DATA AK1CS( 17) / -.60647341040012418187768210377386D-16    /
      DATA AK1CS( 18) / +.98324256232648616038194004650666D-17    /
      DATA AK1CS( 19) / -.16284168738284380035666620115626D-17    /
      DATA AK1CS( 20) / +.27501536496752623718284120337066D-18    /
      DATA AK1CS( 21) / -.47289666463953250924281069568000D-19    /
      DATA AK1CS( 22) / +.82681500028109932722392050346666D-20    /
      DATA AK1CS( 23) / -.14681405136624956337193964885333D-20    /
      DATA AK1CS( 24) / +.26447639269208245978085894826666D-21    /
      DATA AK1CS( 25) / -.48290157564856387897969868800000D-22    /
      DATA AK1CS( 26) / +.89293020743610130180656332799999D-23    /
      DATA AK1CS( 27) / -.16708397168972517176997751466666D-23    /
      DATA AK1CS( 28) / +.31616456034040694931368618666666D-24    /
      DATA AK1CS( 29) / -.60462055312274989106506410666666D-25    /
      DATA AK1CS( 30) / +.11678798942042732700718421333333D-25    /
      DATA AK1CS( 31) / -.22773741582653996232867840000000D-26    /
      DATA AK1CS( 32) / +.44811097300773675795305813333333D-27    /
      DATA AK1CS( 33) / -.88932884769020194062336000000000D-28    /
      DATA AK1CS( 34) / +.17794680018850275131392000000000D-28    /
      DATA AK1CS( 35) / -.35884555967329095821994666666666D-29    /
      DATA AK1CS( 36) / +.72906290492694257991679999999999D-30    /
      DATA AK1CS( 37) / -.14918449845546227073024000000000D-30    /
      DATA AK1CS( 38) / +.30736573872934276300799999999999D-31    /
      DATA AK12CS(  1) / +.6379308343739001036600488534102D-1      /
      DATA AK12CS(  2) / +.2832887813049720935835030284708D-1      /
      DATA AK12CS(  3) / -.2475370673905250345414545566732D-3      /
      DATA AK12CS(  4) / +.5771972451607248820470976625763D-5      /
      DATA AK12CS(  5) / -.2068939219536548302745533196552D-6      /
      DATA AK12CS(  6) / +.9739983441381804180309213097887D-8      /
      DATA AK12CS(  7) / -.5585336140380624984688895511129D-9      /
      DATA AK12CS(  8) / +.3732996634046185240221212854731D-10     /
      DATA AK12CS(  9) / -.2825051961023225445135065754928D-11     /
      DATA AK12CS( 10) / +.2372019002484144173643496955486D-12     /
      DATA AK12CS( 11) / -.2176677387991753979268301667938D-13     /
      DATA AK12CS( 12) / +.2157914161616032453939562689706D-14     /
      DATA AK12CS( 13) / -.2290196930718269275991551338154D-15     /
      DATA AK12CS( 14) / +.2582885729823274961919939565226D-16     /
      DATA AK12CS( 15) / -.3076752641268463187621098173440D-17     /
      DATA AK12CS( 16) / +.3851487721280491597094896844799D-18     /
      DATA AK12CS( 17) / -.5044794897641528977117282508800D-19     /
      DATA AK12CS( 18) / +.6888673850418544237018292223999D-20     /
      DATA AK12CS( 19) / -.9775041541950118303002132480000D-21     /
      DATA AK12CS( 20) / +.1437416218523836461001659733333D-21     /
      DATA AK12CS( 21) / -.2185059497344347373499733333333D-22     /
      DATA AK12CS( 22) / +.3426245621809220631645388800000D-23     /
      DATA AK12CS( 23) / -.5531064394246408232501248000000D-24     /
      DATA AK12CS( 24) / +.9176601505685995403782826666666D-25     /
      DATA AK12CS( 25) / -.1562287203618024911448746666666D-25     /
      DATA AK12CS( 26) / +.2725419375484333132349439999999D-26     /
      DATA AK12CS( 27) / -.4865674910074827992378026666666D-27     /
      DATA AK12CS( 28) / +.8879388552723502587357866666666D-28     /
      DATA AK12CS( 29) / -.1654585918039257548936533333333D-28     /
      DATA AK12CS( 30) / +.3145111321357848674303999999999D-29     /
      DATA AK12CS( 31) / -.6092998312193127612416000000000D-30     /
      DATA AK12CS( 32) / +.1202021939369815834623999999999D-30     /
      DATA AK12CS( 33) / -.2412930801459408841386666666666D-31     /
      DATA FIRST /.TRUE./
!***FIRST EXECUTABLE STATEMENT  DBSK1E
!
      DBSK1E=0.0D0
!
      IF (FIRST) THEN
         ETA = 0.1*REAL(D1MACH(3))
         NTK1 = INITDS (BK1CS, 16, ETA)
         NTAK1 = INITDS (AK1CS, 38, ETA)
         NTAK12 = INITDS (AK12CS, 33, ETA)
!
         XMIN = EXP (MAX(LOG(D1MACH(1)), -LOG(D1MACH(2))) + 0.01D0)
         XSML = SQRT(4.0D0*D1MACH(3))
      ENDIF
      FIRST = .FALSE.
!
!CCCC IF (X .LE. 0.D0) CALL XERMSG ('SLATEC', 'DBSK1E',
!CCCC+   'X IS ZERO OR NEGATIVE', 2, 2)
      IF (X .LE. 0.D0) THEN
        WRITE(ICOUT,1)
    1   FORMAT('***** ERORR FROM DBSK1E, X ZERO OR NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        DBSK1E=0.0D0
        RETURN
      ENDIF
      IF (X.GT.2.0D0) GO TO 20
!
!CCCC IF (X .LT. XMIN) CALL XERMSG ('SLATEC', 'DBSK1E',
!CCCC+   'X SO SMALL K1 OVERFLOWS', 3, 2)
      IF (X .LT. XMIN) THEN
        WRITE(ICOUT,2)
        CALL DPWRST('XXX','BUG ')
        DBSK1E = 0.0D0
        RETURN
      ENDIF
    2 FORMAT('***** ERROR FROM DBSK1E, OVERRFLOW BECAUSE THE ',   &
             'VALUE OF X IS SO SMALL.')
      Y = 0.D0
      IF (X.GT.XSML) Y = X*X
      DBSK1E = EXP(X)*(LOG(0.5D0*X)*DBESI1(X) + (0.75D0 +   &
        DCSEVL (0.5D0*Y-1.D0, BK1CS, NTK1))/X )
      RETURN
!
 20   IF (X.LE.8.D0) DBSK1E = (1.25D0 + DCSEVL ((16.D0/X-5.D0)/3.D0,   &
        AK1CS, NTAK1))/SQRT(X)
      IF (X.GT.8.D0) DBSK1E = (1.25D0 +   &
        DCSEVL (16.D0/X-1.D0, AK12CS, NTAK12))/SQRT(X)
!
      RETURN
      END FUNCTION DBSK1E 
      SUBROUTINE DBSKNU (X, FNU, KODE, N, Y, NZ)
!***BEGIN PROLOGUE  DBSKNU
!***SUBSIDIARY
!***PURPOSE  Subsidiary to DBESK
!***LIBRARY   SLATEC
!***TYPE      DOUBLE PRECISION (BESKNU-S, DBSKNU-D)
!***AUTHOR  Amos, D. E., (SNLA)
!***DESCRIPTION
!
!     Abstract  **** A DOUBLE PRECISION routine ****
!         DBSKNU computes N member sequences of K Bessel functions
!         K/SUB(FNU+I-1)/(X), I=1,N for non-negative orders FNU and
!         positive X. Equations of the references are implemented on
!         small orders DNU for K/SUB(DNU)/(X) and K/SUB(DNU+1)/(X).
!         Forward recursion with the three term recursion relation
!         generates higher orders FNU+I-1, I=1,...,N. The parameter
!         KODE permits K/SUB(FNU+I-1)/(X) values or scaled values
!         EXP(X)*K/SUB(FNU+I-1)/(X), I=1,N to be returned.
!
!         To start the recursion FNU is normalized to the interval
!         -0.5.LE.DNU.LT.0.5. A special form of the power series is
!         implemented on 0.LT.X.LE.X1 while the Miller algorithm for the
!         K Bessel function in terms of the confluent hypergeometric
!         function U(FNU+0.5,2*FNU+1,X) is implemented on X1.LT.X.LE.X2.
!         For X.GT.X2, the asymptotic expansion for large X is used.
!         When FNU is a half odd integer, a special formula for
!         DNU=-0.5 and DNU+1.0=0.5 is used to start the recursion.
!
!         The maximum number of significant digits obtainable
!         is the smaller of 14 and the number of digits carried in
!         DOUBLE PRECISION arithmetic.
!
!         DBSKNU assumes that a significant digit SINH function is
!         available.
!
!     Description of Arguments
!
!         INPUT      X,FNU are DOUBLE PRECISION
!           X      - X.GT.0.0D0
!           FNU    - Order of initial K function, FNU.GE.0.0D0
!           N      - Number of members of the sequence, N.GE.1
!           KODE   - A parameter to indicate the scaling option
!                    KODE= 1  returns
!                             Y(I)=       K/SUB(FNU+I-1)/(X)
!                                  I=1,...,N
!                        = 2  returns
!                             Y(I)=EXP(X)*K/SUB(FNU+I-1)/(X)
!                                  I=1,...,N
!
!         OUTPUT     Y is DOUBLE PRECISION
!           Y      - A vector whose first N components contain values
!                    for the sequence
!                    Y(I)=       K/SUB(FNU+I-1)/(X), I=1,...,N or
!                    Y(I)=EXP(X)*K/SUB(FNU+I-1)/(X), I=1,...,N
!                    depending on KODE
!           NZ     - Number of components set to zero due to
!                    underflow,
!                    NZ= 0   , normal return
!                    NZ.NE.0 , first NZ components of Y set to zero
!                              due to underflow, Y(I)=0.0D0,I=1,...,NZ
!
!     Error Conditions
!         Improper input arguments - a fatal error
!         Overflow - a fatal error
!         Underflow with KODE=1 - a non-fatal error (NZ.NE.0)
!
!***SEE ALSO  DBESK
!***REFERENCES  N. M. Temme, On the numerical evaluation of the modified
!                 Bessel function of the third kind, Journal of
!                 Computational Physics 19, (1975), pp. 324-337.
!***ROUTINES CALLED  D1MACH, DGAMMA, I1MACH, XERMSG
!***REVISION HISTORY  (YYMMDD)
!   790201  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890911  Removed unnecessary intrinsics.  (WRB)
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!   900326  Removed duplicate information from DESCRIPTION section.
!           (WRB)
!   900328  Added TYPE section.  (WRB)
!   900727  Added EXTERNAL statement.  (WRB)
!   910408  Updated the AUTHOR and REFERENCES sections.  (WRB)
!   920501  Reformatted the REFERENCES section.  (WRB)
!***END PROLOGUE  DBSKNU
!
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      INTEGER I, IFLAG, INU, J, K, KK, KODE, KODED, N, NN, NZ
      DOUBLE PRECISION A,AK,A1,A2,B,BK,CC,CK,COEF,CX,DK,DNU,DNU2,ELIM,   &
       ETEST, EX, F, FC, FHS, FK, FKS, FLRX, FMU, FNU, G1, G2, P, PI,   &
       PT, P1, P2, Q, RTHPI, RX, S, SMU, SQK, ST, S1, S2, TM, TOL, T1,   &
       T2, X, X1, X2, Y
      DIMENSION A(160), B(160), Y(*), CC(8)
      DOUBLE PRECISION DGAMMA
      EXTERNAL DGAMMA
      SAVE X1, X2, PI, RTHPI, CC
      DATA X1, X2 / 2.0D0, 17.0D0 /
      DATA PI,RTHPI        / 3.14159265358979D+00, 1.25331413731550D+00/
      DATA CC(1), CC(2), CC(3), CC(4), CC(5), CC(6), CC(7), CC(8)   &
                           / 5.77215664901533D-01,-4.20026350340952D-02,   &
      -4.21977345555443D-02, 7.21894324666300D-03,-2.15241674114900D-04,   &
      -2.01348547807000D-05, 1.13302723200000D-06, 6.11609500000000D-09/
!***FIRST EXECUTABLE STATEMENT  DBSKNU
!
      S2  = 0.0D0
      DNU2 = 0.0D0
      KK = -I1MACH(15)
      ELIM = 2.303D0*(KK*D1MACH(5)-3.0D0)
      AK = D1MACH(3)
      TOL = MAX(AK,1.0D-15)
      IF (X.LE.0.0D0) GO TO 350
      IF (FNU.LT.0.0D0) GO TO 360
      IF (KODE.LT.1 .OR. KODE.GT.2) GO TO 370
      IF (N.LT.1) GO TO 380
      NZ = 0
      IFLAG = 0
      KODED = KODE
      RX = 2.0D0/X
      INU = INT(FNU+0.5D0)
      DNU = FNU - INU
      IF (ABS(DNU).EQ.0.5D0) GO TO 120
      DNU2 = 0.0D0
      IF (ABS(DNU).LT.TOL) GO TO 10
      DNU2 = DNU*DNU
   10 CONTINUE
      IF (X.GT.X1) GO TO 120
!
!     SERIES FOR X.LE.X1
!
      A1 = 1.0D0 - DNU
      A2 = 1.0D0 + DNU
      T1 = 1.0D0/DGAMMA(A1)
      T2 = 1.0D0/DGAMMA(A2)
      IF (ABS(DNU).GT.0.1D0) GO TO 40
!     SERIES FOR F0 TO RESOLVE INDETERMINACY FOR SMALL ABS(DNU)
      S = CC(1)
      AK = 1.0D0
      DO 20 K=2,8
        AK = AK*DNU2
        TM = CC(K)*AK
        S = S + TM
        IF (ABS(TM).LT.TOL) GO TO 30
   20 CONTINUE
   30 G1 = -S
      GO TO 50
   40 CONTINUE
      G1 = (T1-T2)/(DNU+DNU)
   50 CONTINUE
      G2 = (T1+T2)*0.5D0
      SMU = 1.0D0
      FC = 1.0D0
      FLRX = LOG(RX)
      FMU = DNU*FLRX
      IF (DNU.EQ.0.0D0) GO TO 60
      FC = DNU*PI
      FC = FC/SIN(FC)
      IF (FMU.NE.0.0D0) SMU = SINH(FMU)/FMU
   60 CONTINUE
      F = FC*(G1*COSH(FMU)+G2*FLRX*SMU)
      FC = EXP(FMU)
      P = 0.5D0*FC/T2
      Q = 0.5D0/(FC*T1)
      AK = 1.0D0
      CK = 1.0D0
      BK = 1.0D0
      S1 = F
      S2 = P
      IF (INU.GT.0 .OR. N.GT.1) GO TO 90
      IF (X.LT.TOL) GO TO 80
      CX = X*X*0.25D0
   70 CONTINUE
      F = (AK*F+P+Q)/(BK-DNU2)
      P = P/(AK-DNU)
      Q = Q/(AK+DNU)
      CK = CK*CX/AK
      T1 = CK*F
      S1 = S1 + T1
      BK = BK + AK + AK + 1.0D0
      AK = AK + 1.0D0
      S = ABS(T1)/(1.0D0+ABS(S1))
      IF (S.GT.TOL) GO TO 70
   80 CONTINUE
      Y(1) = S1
      IF (KODED.EQ.1) RETURN
      Y(1) = S1*EXP(X)
      RETURN
   90 CONTINUE
      IF (X.LT.TOL) GO TO 110
      CX = X*X*0.25D0
  100 CONTINUE
      F = (AK*F+P+Q)/(BK-DNU2)
      P = P/(AK-DNU)
      Q = Q/(AK+DNU)
      CK = CK*CX/AK
      T1 = CK*F
      S1 = S1 + T1
      T2 = CK*(P-AK*F)
      S2 = S2 + T2
      BK = BK + AK + AK + 1.0D0
      AK = AK + 1.0D0
      S = ABS(T1)/(1.0D0+ABS(S1)) + ABS(T2)/(1.0D0+ABS(S2))
      IF (S.GT.TOL) GO TO 100
  110 CONTINUE
      S2 = S2*RX
      IF (KODED.EQ.1) GO TO 170
      F = EXP(X)
      S1 = S1*F
      S2 = S2*F
      GO TO 170
  120 CONTINUE
      COEF = RTHPI/SQRT(X)
      IF (KODED.EQ.2) GO TO 130
      IF (X.GT.ELIM) GO TO 330
      COEF = COEF*EXP(-X)
  130 CONTINUE
      IF (ABS(DNU).EQ.0.5D0) GO TO 340
      IF (X.GT.X2) GO TO 280
!
!     MILLER ALGORITHM FOR X1.LT.X.LE.X2
!
      ETEST = COS(PI*DNU)/(PI*X*TOL)
      FKS = 1.0D0
      FHS = 0.25D0
      FK = 0.0D0
      CK = X + X + 2.0D0
      P1 = 0.0D0
      P2 = 1.0D0
      K = 0
  140 CONTINUE
      K = K + 1
      FK = FK + 1.0D0
      AK = (FHS-DNU2)/(FKS+FK)
      BK = CK/(FK+1.0D0)
      PT = P2
      P2 = BK*P2 - AK*P1
      P1 = PT
      A(K) = AK
      B(K) = BK
      CK = CK + 2.0D0
      FKS = FKS + FK + FK + 1.0D0
      FHS = FHS + FK + FK
      IF (ETEST.GT.FK*P1) GO TO 140
      KK = K
      S = 1.0D0
      P1 = 0.0D0
      P2 = 1.0D0
      DO 150 I=1,K
        PT = P2
        P2 = (B(KK)*P2-P1)/A(KK)
        P1 = PT
        S = S + P2
        KK = KK - 1
  150 CONTINUE
      S1 = COEF*(P2/S)
      IF (INU.GT.0 .OR. N.GT.1) GO TO 160
      GO TO 200
  160 CONTINUE
      S2 = S1*(X+DNU+0.5D0-P1/P2)/X
!
!     FORWARD RECURSION ON THE THREE TERM RECURSION RELATION
!
  170 CONTINUE
      CK = (DNU+DNU+2.0D0)/X
      IF (N.EQ.1) INU = INU - 1
      IF (INU.GT.0) GO TO 180
      IF (N.GT.1) GO TO 200
      S1 = S2
      GO TO 200
  180 CONTINUE
      DO 190 I=1,INU
        ST = S2
        S2 = CK*S2 + S1
        S1 = ST
        CK = CK + RX
  190 CONTINUE
      IF (N.EQ.1) S1 = S2
  200 CONTINUE
      IF (IFLAG.EQ.1) GO TO 220
      Y(1) = S1
      IF (N.EQ.1) RETURN
      Y(2) = S2
      IF (N.EQ.2) RETURN
      DO 210 I=3,N
        Y(I) = CK*Y(I-1) + Y(I-2)
        CK = CK + RX
  210 CONTINUE
      RETURN
!     IFLAG=1 CASES
  220 CONTINUE
      S = -X + LOG(S1)
      Y(1) = 0.0D0
      NZ = 1
      IF (S.LT.-ELIM) GO TO 230
      Y(1) = EXP(S)
      NZ = 0
  230 CONTINUE
      IF (N.EQ.1) RETURN
      S = -X + LOG(S2)
      Y(2) = 0.0D0
      NZ = NZ + 1
      IF (S.LT.-ELIM) GO TO 240
      NZ = NZ - 1
      Y(2) = EXP(S)
  240 CONTINUE
      IF (N.EQ.2) RETURN
      KK = 2
      IF (NZ.LT.2) GO TO 260
      DO 250 I=3,N
        KK = I
        ST = S2
        S2 = CK*S2 + S1
        S1 = ST
        CK = CK + RX
        S = -X + LOG(S2)
        NZ = NZ + 1
        Y(I) = 0.0D0
        IF (S.LT.-ELIM) GO TO 250
        Y(I) = EXP(S)
        NZ = NZ - 1
        GO TO 260
  250 CONTINUE
      RETURN
  260 CONTINUE
      IF (KK.EQ.N) RETURN
      S2 = S2*CK + S1
      CK = CK + RX
      KK = KK + 1
      Y(KK) = EXP(-X+LOG(S2))
      IF (KK.EQ.N) RETURN
      KK = KK + 1
      DO 270 I=KK,N
        Y(I) = CK*Y(I-1) + Y(I-2)
        CK = CK + RX
  270 CONTINUE
      RETURN
!
!     ASYMPTOTIC EXPANSION FOR LARGE X, X.GT.X2
!
!     IFLAG=0 MEANS NO UNDERFLOW OCCURRED
!     IFLAG=1 MEANS AN UNDERFLOW OCCURRED- COMPUTATION PROCEEDS WITH
!     KODED=2 AND A TEST FOR ON SCALE VALUES IS MADE DURING FORWARD
!     RECURSION
  280 CONTINUE
      NN = 2
      IF (INU.EQ.0 .AND. N.EQ.1) NN = 1
      DNU2 = DNU + DNU
      FMU = 0.0D0
      IF (ABS(DNU2).LT.TOL) GO TO 290
      FMU = DNU2*DNU2
  290 CONTINUE
      EX = X*8.0D0
      S2 = 0.0D0
      DO 320 K=1,NN
        S1 = S2
        S = 1.0D0
        AK = 0.0D0
        CK = 1.0D0
        SQK = 1.0D0
        DK = EX
        DO 300 J=1,30
          CK = CK*(FMU-SQK)/DK
          S = S + CK
          DK = DK + EX
          AK = AK + 8.0D0
          SQK = SQK + AK
          IF (ABS(CK).LT.TOL) GO TO 310
  300   CONTINUE
  310   S2 = S*COEF
        FMU = FMU + 8.0D0*DNU + 4.0D0
  320 CONTINUE
      IF (NN.GT.1) GO TO 170
      S1 = S2
      GO TO 200
  330 CONTINUE
      KODED = 2
      IFLAG = 1
      GO TO 120
!
!     FNU=HALF ODD INTEGER CASE
!
  340 CONTINUE
      S1 = COEF
      S2 = COEF
      GO TO 170
!
!
!C350 CALL XERMSG ('SLATEC', 'DBSKNU', 'X NOT GREATER THAN ZERO', 2, 1)
!CCCC RETURN
!C360 CALL XERMSG ('SLATEC', 'DBSKNU', 'FNU NOT ZERO OR POSITIVE', 2,
!CCCC+   1)
!CCCC RETURN
!C370 CALL XERMSG ('SLATEC', 'DBSKNU', 'KODE NOT 1 OR 2', 2, 1)
!CCCC RETURN
!C380 CALL XERMSG ('SLATEC', 'DBSKNU', 'N NOT GREATER THAN 0', 2, 1)
!CCCC RETURN
  350 CONTINUE
      WRITE(ICOUT,351)
  351 FORMAT('** ERROR FROM DBSKNU, X IS LESS THAN OR EQUAL TO ZERO. ')
      CALL DPWRST('XXX','BUG ')
      RETURN
  360 CONTINUE
      WRITE(ICOUT,361)
  361 FORMAT('***** ERROR FROM DBSKNU, THE ORDER FNU IS NEGATIVE.')
      CALL DPWRST('XXX','BUG ')
      RETURN
  370 CONTINUE
      WRITE(ICOUT,371)
  371 FORMAT('***** ERROR FROM DBSKNU, KODE IS NOT 1 OR 2.')
      CALL DPWRST('XXX','BUG ')
      RETURN
  380 CONTINUE
      WRITE(ICOUT,381)
  381 FORMAT('***** ERROR FROM DBSKNU, N IS LESS THAN ONE.. ***')
      CALL DPWRST('XXX','BUG ')
      RETURN
      END SUBROUTINE DBSKNU 
      DOUBLE PRECISION FUNCTION D9CHU (A, B, Z)
!***BEGIN PROLOGUE  D9CHU
!***SUBSIDIARY
!***PURPOSE  Evaluate for large Z  Z**A * U(A,B,Z) where U is the
!            logarithmic confluent hypergeometric function.
!***LIBRARY   SLATEC (FNLIB)
!***CATEGORY  C11
!***TYPE      DOUBLE PRECISION (R9CHU-S, D9CHU-D)
!***KEYWORDS  FNLIB, LOGARITHMIC CONFLUENT HYPERGEOMETRIC FUNCTION,
!             SPECIAL FUNCTIONS
!***AUTHOR  Fullerton, W., (LANL)
!***DESCRIPTION
!
! Evaluate for large Z  Z**A * U(A,B,Z)  where U is the logarithmic
! confluent hypergeometric function.  A rational approximation due to Y.
! L. Luke is used.  When U is not in the asymptotic region, i.e., when A
! or B is large compared with Z, considerable significance loss occurs.
! A warning is provided when the computed result is less than half
! precision.
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  D1MACH, XERMSG
!***REVISION HISTORY  (YYMMDD)
!   770801  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890531  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!   900720  Routine changed from user-callable to subsidiary.  (WRB)
!***END PROLOGUE  D9CHU
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DOUBLE PRECISION A, B, Z, AA(4), BB(4), AB, ANBN, BP, CT1, CT2,   &
        CT3, C2, D1Z, EPS, G1, G2, G3, SAB, SQEPS, X2I1
      LOGICAL FIRST
      SAVE EPS, SQEPS, FIRST
      DATA FIRST /.TRUE./
!***FIRST EXECUTABLE STATEMENT  D9CHU
!
      D9CHU = 0.0D0
!
      IF (FIRST) THEN
         EPS = 4.0D0*D1MACH(4)
         SQEPS = SQRT(D1MACH(4))
      ENDIF
      FIRST = .FALSE.
!
      BP = 1.0D0 + A - B
      AB = A*BP
      CT2 = 2.0D0 * (Z - AB)
      SAB = A + BP
!
      BB(1) = 1.0D0
      AA(1) = 1.0D0
!
      CT3 = SAB + 1.0D0 + AB
      BB(2) = 1.0D0 + 2.0D0*Z/CT3
      AA(2) = 1.0D0 + CT2/CT3
!
      ANBN = CT3 + SAB + 3.0D0
      CT1 = 1.0D0 + 2.0D0*Z/ANBN
      BB(3) = 1.0D0 + 6.0D0*CT1*Z/CT3
      AA(3) = 1.0D0 + 6.0D0*AB/ANBN + 3.0D0*CT1*CT2/CT3
!
      DO 30 I=4,300
        X2I1 = 2*I - 3
        CT1 = X2I1/(X2I1-2.0D0)
        ANBN = ANBN + X2I1 + SAB
        CT2 = (X2I1 - 1.0D0)/ANBN
        C2 = X2I1*CT2 - 1.0D0
        D1Z = X2I1*2.0D0*Z/ANBN
!
        CT3 = SAB*CT2
        G1 = D1Z + CT1*(C2+CT3)
        G2 = D1Z - C2
        G3 = CT1*(1.0D0 - CT3 - 2.0D0*CT2)
!
        BB(4) = G1*BB(3) + G2*BB(2) + G3*BB(1)
        AA(4) = G1*AA(3) + G2*AA(2) + G3*AA(1)
        IF (ABS(AA(4)*BB(1)-AA(1)*BB(4)).LT.EPS*ABS(BB(4)*BB(1)))   &
          GO TO 40
!
! IF OVERFLOWS OR UNDERFLOWS PROVE TO BE A PROBLEM, THE STATEMENTS
! BELOW COULD BE ALTERED TO INCORPORATE A DYNAMICALLY ADJUSTED SCALE
! FACTOR.
!
        DO 20 J=1,3
          AA(J) = AA(J+1)
          BB(J) = BB(J+1)
 20     CONTINUE
 30   CONTINUE
      WRITE(ICOUT,101)
      CALL DPWRST('XXX','BUG ')
  101 FORMAT('***** ERROR FROM D9CHU, NO CONVERGENCE IN 300 TERMS. ***')
      RETURN
!
 40   D9CHU = AA(4)/BB(4)
!
      IF (D9CHU .LT. SQEPS .OR. D9CHU .GT. 1.0D0/SQEPS) THEN
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
      ENDIF
  111 FORMAT('***** WARNING FROM D9CHU, THE ANSWER IS LESS THAN HALF ',   &
             'PRECISION FOR CHU FUNCTION.  *****.')
!
      RETURN
      END FUNCTION D9CHU 
      DOUBLE PRECISION FUNCTION D9GMIT (A, X, ALGAP1, SGNGAM, ALX)
!***BEGIN PROLOGUE  D9GMIT
!***SUBSIDIARY
!***PURPOSE  Compute Tricomi's incomplete Gamma function for small
!            arguments.
!***LIBRARY   SLATEC (FNLIB)
!***CATEGORY  C7E
!***TYPE      DOUBLE PRECISION (R9GMIT-S, D9GMIT-D)
!***KEYWORDS  COMPLEMENTARY INCOMPLETE GAMMA FUNCTION, FNLIB, SMALL X,
!             SPECIAL FUNCTIONS, TRICOMI
!***AUTHOR  Fullerton, W., (LANL)
!***DESCRIPTION
!
! Compute Tricomi's incomplete gamma function for small X.
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  D1MACH, DLNGAM, XERMSG
!***REVISION HISTORY  (YYMMDD)
!   770701  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890911  Removed unnecessary intrinsics.  (WRB)
!   890911  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!   900720  Routine changed from user-callable to subsidiary.  (WRB)
!***END PROLOGUE  D9GMIT
      DOUBLE PRECISION A, X, ALGAP1, SGNGAM, ALX, AE, AEPS, ALGS, ALG2,   &
        BOT, EPS, FK, S, SGNG2, T, TE, DLNGAM
      LOGICAL FIRST
      SAVE EPS, BOT, FIRST
!
!---------------------------------------------------------------------
!
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DATA FIRST /.TRUE./
!
      ALGS=0.0D0
!
!***FIRST EXECUTABLE STATEMENT  D9GMIT
      IF(ISUBG4.EQ.'GMIT')THEN
        WRITE(ICOUT,91)A,X,ALGAP1,SGNGAM,ALX
   91   FORMAT('FROM D9GMIT: A,X,ALGAP1,SGNGAM,ALX = ',5G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF (FIRST) THEN
         EPS = 0.5D0*D1MACH(3)
         BOT = LOG (D1MACH(1))
      ENDIF
      FIRST = .FALSE.
!
      IF (X .LE. 0.D0) THEN
        WRITE(ICOUT,1)
    1   FORMAT('***** ERORR FROM D9GMIT, X MUST BE POSITIVE.  *******')
        CALL DPWRST('XXX','BUG ')
        D9GMIT=0.D0
        RETURN
      ENDIF
!
      MA = INT(A + 0.5D0)
      IF (A.LT.0.D0) MA = INT(A - 0.5D0)
      AEPS = A - REAL(MA)
!
      AE = A
      IF (A.LT.(-0.5D0)) AE = AEPS
!
      T = 1.D0
      TE = AE
      S = T
      DO 20 K=1,200
        FK = K
        TE = -X*TE/FK
        T = TE/(AE+FK)
        S = S + T
        IF (ABS(T).LT.EPS*ABS(S)) GO TO 30
 20   CONTINUE
!
      WRITE(ICOUT,21)
   21 FORMAT('***** ERROR FROM D9GMIT.  NO CONVERGENCE IN 200')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,22)
   22 FORMAT('      TERMS OF TAYLOR-S SERIES.                ******')
      CALL DPWRST('XXX','BUG ')
      D9GMIT=0.D0
      RETURN
!
 30   IF (A.GE.(-0.5D0)) ALGS = -ALGAP1 + LOG(S)
      IF (A.GE.(-0.5D0)) GO TO 60
!
      ALGS = -DLNGAM(1.D0+AEPS) + LOG(S)
      S = 1.0D0
      M = -MA - 1
      IF (M.EQ.0) GO TO 50
      T = 1.0D0
      DO 40 K=1,M
        T = X*T/(AEPS-(M+1-K))
        S = S + T
        IF (ABS(T).LT.EPS*ABS(S)) GO TO 50
 40   CONTINUE
!
 50   D9GMIT = 0.0D0
      ALGS = -MA*LOG(X) + ALGS
      IF (S.EQ.0.D0 .OR. AEPS.EQ.0.D0) GO TO 60
!
      SGNG2 = SGNGAM * SIGN (1.0D0, S)
      ALG2 = -X - ALGAP1 + LOG(ABS(S))
!
      IF (ALG2.GT.BOT) D9GMIT = SGNG2 * EXP(ALG2)
      IF (ALGS.GT.BOT) D9GMIT = D9GMIT + EXP(ALGS)
      RETURN
!
 60   D9GMIT = EXP (ALGS)
      RETURN
!
      END FUNCTION D9GMIT 
      DOUBLE PRECISION FUNCTION D9GMIC (A, X, ALX)
!***BEGIN PROLOGUE  D9GMIC
!***SUBSIDIARY
!***PURPOSE  Compute the complementary incomplete Gamma function for A
!            near a negative integer and X small.
!***LIBRARY   SLATEC (FNLIB)
!***CATEGORY  C7E
!***TYPE      DOUBLE PRECISION (R9GMIC-S, D9GMIC-D)
!***KEYWORDS  COMPLEMENTARY INCOMPLETE GAMMA FUNCTION, FNLIB, SMALL X,
!             SPECIAL FUNCTIONS
!***AUTHOR  Fullerton, W., (LANL)
!***DESCRIPTION
!
! Compute the complementary incomplete gamma function for A near
! a negative integer and for small X.
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  D1MACH, DLNGAM, XERMSG
!***REVISION HISTORY  (YYMMDD)
!   770701  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890911  Removed unnecessary intrinsics.  (WRB)
!   890911  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!   900720  Routine changed from user-callable to subsidiary.  (WRB)
!***END PROLOGUE  D9GMIC
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DOUBLE PRECISION A, X, ALX, ALNG, BOT, EPS, EULER, FK, FKP1, FM,   &
        S, SGNG, T, TE, DLNGAM
      LOGICAL FIRST
      SAVE EULER, EPS, BOT, FIRST
      DATA EULER / 0.57721566490153286060651209008240D0 /
      DATA FIRST /.TRUE./
!***FIRST EXECUTABLE STATEMENT  D9GMIC
      IF (FIRST) THEN
         EPS = 0.5D0*D1MACH(3)
         BOT = LOG (D1MACH(1))
      ENDIF
      FIRST = .FALSE.
!
      IF (A .GT. 0.D0) THEN
        WRITE(ICOUT,2)
    2   FORMAT('***** ERORR FROM D9GMIC, SECOND ARGUMENT MUST BE ',   &
         'NEAR A NEGATIVE INTEGER.  *******')
        CALL DPWRST('XXX','BUG ')
        D9GMIC=0.D0
        RETURN
      ENDIF
      IF (X .LE. 0.D0) THEN
        WRITE(ICOUT,1)
    1   FORMAT('***** ERORR FROM D9GMIC, X MUST BE POSITIVE.  *******')
        CALL DPWRST('XXX','BUG ')
        D9GMIC=0.D0
        RETURN
      ENDIF
!
      M = INT(-(A - 0.5D0))
      FM = REAL(M)
!
      TE = 1.0D0
      T = 1.0D0
      S = T
      DO 20 K=1,200
        FKP1 = K + 1
        TE = -X*TE/(FM+FKP1)
        T = TE/FKP1
        S = S + T
        IF (ABS(T).LT.EPS*S) GO TO 30
 20   CONTINUE
      WRITE(ICOUT,21)
   21 FORMAT('***** ERROR FROM D9GMIC.  NO CONVERGENCE IN 200')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,22)
   22 FORMAT('      TERMS OF TAYLOR-S SERIES.                ******')
      CALL DPWRST('XXX','BUG ')
      D9GMIC=0.D0
      RETURN
!
 30   D9GMIC = -ALX - EULER + X*S/(FM+1.0D0)
      IF (M.EQ.0) RETURN
!
      IF (M.EQ.1) D9GMIC = -D9GMIC - 1.D0 + 1.D0/X
      IF (M.EQ.1) RETURN
!
      TE = FM
      T = 1.D0
      S = T
      MM1 = M - 1
      DO 40 K=1,MM1
        FK = K
        TE = -X*TE/FK
        T = TE/(FM-FK)
        S = S + T
        IF (ABS(T).LT.EPS*ABS(S)) GO TO 50
 40   CONTINUE
!
 50   DO 60 K=1,M
        D9GMIC = D9GMIC + 1.0D0/K
 60   CONTINUE
!
      SGNG = 1.0D0
      IF (MOD(M,2).EQ.1) SGNG = -1.0D0
      ALNG = LOG(D9GMIC) - DLNGAM(FM+1.D0)
!
      D9GMIC = 0.D0
      IF (ALNG.GT.BOT) D9GMIC = SGNG * EXP(ALNG)
      IF (S.NE.0.D0) D9GMIC = D9GMIC +   &
        SIGN (EXP(-FM*ALX+LOG(ABS(S)/FM)), S)
!
      IF (D9GMIC .EQ. 0.D0 .AND. S .EQ. 0.D0) THEN
        WRITE(ICOUT,31)
   31   FORMAT('***** ERROR FROM D9GMIC.  RESULT UNDERFLOWS.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      RETURN
!
      END FUNCTION D9GMIC 
      DOUBLE PRECISION FUNCTION D9LGIC (A, X, ALX)
!***BEGIN PROLOGUE  D9LGIC
!***SUBSIDIARY
!***PURPOSE  Compute the log complementary incomplete Gamma function
!            for large X and for A .LE. X.
!***LIBRARY   SLATEC (FNLIB)
!***CATEGORY  C7E
!***TYPE      DOUBLE PRECISION (R9LGIC-S, D9LGIC-D)
!***KEYWORDS  COMPLEMENTARY INCOMPLETE GAMMA FUNCTION, FNLIB, LARGE X,
!             LOGARITHM, SPECIAL FUNCTIONS
!***AUTHOR  Fullerton, W., (LANL)
!***DESCRIPTION
!
! Compute the log complementary incomplete gamma function for large X
! and for A .LE. X.
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  D1MACH, XERMSG
!***REVISION HISTORY  (YYMMDD)
!   770701  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890531  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!   900720  Routine changed from user-callable to subsidiary.  (WRB)
!***END PROLOGUE  D9LGIC
      DOUBLE PRECISION A, X, ALX, EPS, FK, P, R, S, T, XMA, XPA
      SAVE EPS
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DATA EPS / 0.D0 /
!***FIRST EXECUTABLE STATEMENT  D9LGIC
      IF (EPS.EQ.0.D0) EPS = 0.5D0*D1MACH(3)
!
      XPA = X + 1.0D0 - A
      XMA = X - 1.D0 - A
!
      R = 0.D0
      P = 1.D0
      S = P
      DO 10 K=1,300
        FK = K
        T = FK*(A-FK)*(1.D0+R)
        R = -T/((XMA+2.D0*FK)*(XPA+2.D0*FK)+T)
        P = R*P
        S = S + P
        IF (ABS(P).LT.EPS*S) GO TO 20
 10   CONTINUE
      WRITE(ICOUT,98)
   98 FORMAT('***** ERROR FROM D9LGIC.  NO CONVERGENCE IN 300 ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,99)
   99 FORMAT('      TERMS OF CONTINUED FRACTION.             ******')
      CALL DPWRST('XXX','BUG ')
      D9LGIC = 0.D0
      RETURN
!
 20   D9LGIC = A*ALX - X + LOG(S/XPA)
!
      RETURN
      END FUNCTION D9LGIC 
      DOUBLE PRECISION FUNCTION D9LGIT (A, X, ALGAP1)
!***BEGIN PROLOGUE  D9LGIT
!***SUBSIDIARY
!***PURPOSE  Compute the logarithm of Tricomi's incomplete Gamma
!            function with Perron's continued fraction for large X and
!            A .GE. X.
!***LIBRARY   SLATEC (FNLIB)
!***CATEGORY  C7E
!***TYPE      DOUBLE PRECISION (R9LGIT-S, D9LGIT-D)
!***KEYWORDS  FNLIB, INCOMPLETE GAMMA FUNCTION, LOGARITHM,
!             PERRON'S CONTINUED FRACTION, SPECIAL FUNCTIONS, TRICOMI
!***AUTHOR  Fullerton, W., (LANL)
!***DESCRIPTION
!
! Compute the log of Tricomi's incomplete gamma function with Perron's
! continued fraction for large X and for A .GE. X.
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  D1MACH, XERMSG
!***REVISION HISTORY  (YYMMDD)
!   770701  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890531  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!   900720  Routine changed from user-callable to subsidiary.  (WRB)
!***END PROLOGUE  D9LGIT
      DOUBLE PRECISION A, X, ALGAP1, AX, A1X, EPS, FK, HSTAR, P, R, S,   &
        SQEPS, T
      LOGICAL FIRST
      SAVE EPS, SQEPS, FIRST
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DATA FIRST /.TRUE./
!***FIRST EXECUTABLE STATEMENT  D9LGIT
      IF (FIRST) THEN
         EPS = 0.5D0*D1MACH(3)
         SQEPS = SQRT(D1MACH(4))
      ENDIF
      FIRST = .FALSE.
!
      IF (X .LE. 0.D0 .OR. A .LT. X) THEN
        WRITE(ICOUT,11)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        D9LGIT = 0.D0
        RETURN
      ENDIF
   11 FORMAT('***** ERROR FROM D9LGIT.  X SHOULD BE POSITIVE ')
   12 FORMAT('      AND LESS THAN OR EQUAL TO A.             ******')
!
      AX = A + X
      A1X = AX + 1.0D0
      R = 0.D0
      P = 1.D0
      S = P
      DO 20 K=1,200
        FK = K
        T = (A+FK)*X*(1.D0+R)
        R = T/((AX+FK)*(A1X+FK)-T)
        P = R*P
        S = S + P
        IF (ABS(P).LT.EPS*S) GO TO 30
 20   CONTINUE
      WRITE(ICOUT,21)
 21   FORMAT('***** ERROR FROM D9LGIT.  NO CONVERGENCE IN 200 ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,22)
 22   FORMAT('      TERMS OF CONTINUED FRACTION.              *****')
      CALL DPWRST('XXX','BUG ')
      D9LGIT = 0.D0
      RETURN
!
 30   HSTAR = 1.0D0 - X*S/A1X
      IF (HSTAR .LT. SQEPS)THEN
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,32)
        CALL DPWRST('XXX','BUG ')
      ENDIF
 31   FORMAT('***** WARNING FROM D9LGIT.  RESULT LESS THAN HALF ')
 32   FORMAT('      PRECISION.                                  *****')
!
      D9LGIT = -X - ALGAP1 - LOG(HSTAR)
      RETURN
!
      END FUNCTION D9LGIT 
      DOUBLE PRECISION FUNCTION D9LGMC (X)
!***BEGIN PROLOGUE  D9LGMC
!***SUBSIDIARY
!***PURPOSE  Compute the log Gamma correction factor so that
!            LOG(DGAMMA(X)) = LOG(SQRT(2*PI)) + (X-5.)*LOG(X) - X
!            + D9LGMC(X).
!***LIBRARY   SLATEC (FNLIB)
!***CATEGORY  C7E
!***TYPE      DOUBLE PRECISION (R9LGMC-S, D9LGMC-D, C9LGMC-C)
!***KEYWORDS  COMPLETE GAMMA FUNCTION, CORRECTION TERM, FNLIB,
!             LOG GAMMA, LOGARITHM, SPECIAL FUNCTIONS
!***AUTHOR  Fullerton, W., (LANL)
!***DESCRIPTION
!
! Compute the log gamma correction factor for X .GE. 10. so that
! LOG (DGAMMA(X)) = LOG(SQRT(2*PI)) + (X-.5)*LOG(X) - X + D9lGMC(X)
!
! Series for ALGM       on the interval  0.          to  1.00000E-02
!                                        with weighted error   1.28E-31
!                                         log weighted error  30.89
!                               significant figures required  29.81
!                                    decimal places required  31.48
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  D1MACH, DCSEVL, INITDS, XERMSG
!***REVISION HISTORY  (YYMMDD)
!   770601  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890531  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!   900720  Routine changed from user-callable to subsidiary.  (WRB)
!***END PROLOGUE  D9LGMC
      DOUBLE PRECISION X, ALGMCS(15), XBIG, XMAX, DCSEVL
      LOGICAL FIRST
      SAVE ALGMCS, NALGM, XBIG, XMAX, FIRST
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DATA ALGMCS(  1) / +.1666389480451863247205729650822D+0      /
      DATA ALGMCS(  2) / -.1384948176067563840732986059135D-4      /
      DATA ALGMCS(  3) / +.9810825646924729426157171547487D-8      /
      DATA ALGMCS(  4) / -.1809129475572494194263306266719D-10     /
      DATA ALGMCS(  5) / +.6221098041892605227126015543416D-13     /
      DATA ALGMCS(  6) / -.3399615005417721944303330599666D-15     /
      DATA ALGMCS(  7) / +.2683181998482698748957538846666D-17     /
      DATA ALGMCS(  8) / -.2868042435334643284144622399999D-19     /
      DATA ALGMCS(  9) / +.3962837061046434803679306666666D-21     /
      DATA ALGMCS( 10) / -.6831888753985766870111999999999D-23     /
      DATA ALGMCS( 11) / +.1429227355942498147573333333333D-24     /
      DATA ALGMCS( 12) / -.3547598158101070547199999999999D-26     /
      DATA ALGMCS( 13) / +.1025680058010470912000000000000D-27     /
      DATA ALGMCS( 14) / -.3401102254316748799999999999999D-29     /
      DATA ALGMCS( 15) / +.1276642195630062933333333333333D-30     /
      DATA FIRST /.TRUE./
!***FIRST EXECUTABLE STATEMENT  D9LGMC
      IF (FIRST) THEN
         NALGM = INITDS (ALGMCS, 15, REAL(D1MACH(3)) )
         XBIG = 1.0D0/SQRT(D1MACH(3))
         XMAX = EXP (MIN(LOG(D1MACH(2)/12.D0), -LOG(12.D0*D1MACH(1))))
      ENDIF
      FIRST = .FALSE.
!
      IF (X .LT. 10.D0) THEN
        WRITE(ICOUT,11)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        D9LGMC = 0.D0
        RETURN
      ENDIF
   11 FORMAT('***** ERROR FROM D9LGMC.  X MUST BE GREATER THAN ')
   12 FORMAT('      OR EQUAL TO 10.                          ******')
      IF (X.GE.XMAX) GO TO 20
!
      D9LGMC = 1.D0/(12.D0*X)
      IF (X.LT.XBIG) D9LGMC = DCSEVL (2.0D0*(10.D0/X)**2-1.D0, ALGMCS,   &
        NALGM) / X
      RETURN
!
 20   D9LGMC = 0.D0
      WRITE(ICOUT,21)
 21   FORMAT('***** WARNING FROM D9LGMC.  X SO BIG D9LCMC UNDERFLOWS.')
      CALL DPWRST('XXX','BUG ')
      RETURN
!
      END FUNCTION D9LGMC 
      DOUBLE PRECISION FUNCTION DBETA (A, B)
!***BEGIN PROLOGUE  DBETA
!***PURPOSE  Compute the complete Beta function.
!***LIBRARY   SLATEC (FNLIB)
!***CATEGORY  C7B
!***TYPE      DOUBLE PRECISION (BETA-S, DBETA-D, CBETA-C)
!***KEYWORDS  COMPLETE BETA FUNCTION, FNLIB, SPECIAL FUNCTIONS
!***AUTHOR  Fullerton, W., (LANL)
!***DESCRIPTION
!
! DBETA(A,B) calculates the double precision complete beta function
! for double precision arguments A and B.
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  D1MACH, DGAMLM, DGAMMA, DLBETA, XERMSG
!***REVISION HISTORY  (YYMMDD)
!   770601  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890531  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!   900727  Added EXTERNAL statement.  (WRB)
!***END PROLOGUE  DBETA
      DOUBLE PRECISION A, B, ALNSML, XMAX, XMIN, DLBETA, DGAMMA
      LOGICAL FIRST
      EXTERNAL DGAMMA
      SAVE XMAX, ALNSML, FIRST
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DATA FIRST /.TRUE./
!***FIRST EXECUTABLE STATEMENT  DBETA
!
      DBETA = 0.0D0
!
      IF (FIRST) THEN
         CALL DGAMLM (XMIN, XMAX)
         ALNSML = LOG (D1MACH(1))
      ENDIF
      FIRST = .FALSE.
!
      IF (A .LE. 0.D0 .OR. B .LE. 0.D0) THEN
        WRITE(ICOUT,11)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        DBETA = 0.D0
        RETURN
      ENDIF
   11 FORMAT('***** ERROR FROM DBETA.  BOTH THE ARGUMENTS MUST ')
   12 FORMAT('      BE POSITIVE.                               ****')
!
      IF (A+B.LT.XMAX) DBETA = DGAMMA(A)*DGAMMA(B)/DGAMMA(A+B)
      IF (A+B.LT.XMAX) RETURN
!
      DBETA = DLBETA (A, B)
      IF (DBETA.LT.ALNSML) GO TO 20
      DBETA = EXP (DBETA)
      RETURN
!
 20   DBETA = 0.D0
      WRITE(ICOUT,21)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,22)
      CALL DPWRST('XXX','BUG ')
   21 FORMAT('***** ERROR FROM DBETA.  ALPHA AND BETA ARE SO ')
   22 FORMAT('      LARGE THAT THE BETA FUNCTION OVERFLOWS.  *****')
      RETURN
!
      END FUNCTION DBETA 
      DOUBLE PRECISION FUNCTION DBETAI (X, PIN, QIN)
!***BEGIN PROLOGUE  DBETAI
!***PURPOSE  Calculate the incomplete Beta function.
!***LIBRARY   SLATEC (FNLIB)
!***CATEGORY  C7F
!***TYPE      DOUBLE PRECISION (BETAI-S, DBETAI-D)
!***KEYWORDS  FNLIB, INCOMPLETE BETA FUNCTION, SPECIAL FUNCTIONS
!***AUTHOR  Fullerton, W., (LANL)
!***DESCRIPTION
!
!   DBETAI calculates the DOUBLE PRECISION incomplete beta function.
!
!   The incomplete beta function ratio is the probability that a
!   random variable from a beta distribution having parameters PIN and
!   QIN will be less than or equal to X.
!
!     -- Input Arguments -- All arguments are DOUBLE PRECISION.
!   X      upper limit of integration.  X must be in (0,1) inclusive.
!   PIN    first beta distribution parameter.  PIN must be .GT. 0.0.
!   QIN    second beta distribution parameter.  QIN must be .GT. 0.0.
!
!***REFERENCES  Nancy E. Bosten and E. L. Battiste, Remark on Algorithm
!                 179, Communications of the ACM 17, 3 (March 1974),
!                 pp. 156.
!***ROUTINES CALLED  D1MACH, DLBETA, XERMSG
!***REVISION HISTORY  (YYMMDD)
!   770701  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890911  Removed unnecessary intrinsics.  (WRB)
!   890911  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!   920528  DESCRIPTION and REFERENCES sections revised.  (WRB)
!***END PROLOGUE  DBETAI
      DOUBLE PRECISION X, PIN, QIN, ALNEPS, ALNSML, C, EPS, FINSUM, P,   &
        PS, Q, SML, TERM, XB, XI, Y, DLBETA, P1
      LOGICAL FIRST
      SAVE EPS, ALNEPS, SML, ALNSML, FIRST
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DATA FIRST /.TRUE./
!***FIRST EXECUTABLE STATEMENT  DBETAI
      IF (FIRST) THEN
         EPS = D1MACH(3)
         ALNEPS = LOG (EPS)
         SML = D1MACH(1)
         ALNSML = LOG (SML)
      ENDIF
      FIRST = .FALSE.
!
      IF (X .LT. 0.D0 .OR. X .GT. 1.D0) THEN
        WRITE(ICOUT,11)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        DBETAI = 0.D0
        RETURN
      ENDIF
   11 FORMAT('***** ERROR FROM DBETAI.  X IS NOT IN THE RANGE ')
   12 FORMAT('      (0,1).                                    *****')
      IF (PIN .LE. 0.D0 .OR. QIN .LE. 0.D0) THEN
        WRITE(ICOUT,16)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,17)
        CALL DPWRST('XXX','BUG ')
        DBETAI = 0.D0
        RETURN
      ENDIF
   16 FORMAT('***** ERROR FROM DBETAI.  P AND/OR Q IS LESS THAN ')
   17 FORMAT('      OR EQUAL TO ZERO.                           *****')
!
      Y = X
      P = PIN
      Q = QIN
      IF (Q.LE.P .AND. X.LT.0.8D0) GO TO 20
      IF (X.LT.0.2D0) GO TO 20
      Y = 1.0D0 - Y
      P = QIN
      Q = PIN
!
 20   IF ((P+Q)*Y/(P+1.D0).LT.EPS) GO TO 80
!
! EVALUATE THE INFINITE SUM FIRST.  TERM WILL EQUAL
! Y**P/BETA(PS,P) * (1.-PS)-SUB-I * Y**I / FAC(I) .
!
      PS = Q - AINT(Q)
      IF (PS.EQ.0.D0) PS = 1.0D0
      XB = P*LOG(Y) - DLBETA(PS,P) - LOG(P)
      DBETAI = 0.0D0
      IF (XB.LT.ALNSML) GO TO 40
!
      DBETAI = EXP (XB)
      TERM = DBETAI*P
      IF (PS.EQ.1.0D0) GO TO 40
      N = INT(MAX (ALNEPS/LOG(Y), 4.0D0))
      DO 30 I=1,N
        XI = REAL(I)
        TERM = TERM * (XI-PS)*Y/XI
        DBETAI = DBETAI + TERM/(P+XI)
 30   CONTINUE
!
! NOW EVALUATE THE FINITE SUM, MAYBE.
!
 40   IF (Q.LE.1.0D0) GO TO 70
!
      XB = P*LOG(Y) + Q*LOG(1.0D0-Y) - DLBETA(P,Q) - LOG(Q)
      IB = INT(MAX (XB/ALNSML, 0.0D0))
      TERM = EXP(XB - IB*ALNSML)
      C = 1.0D0/(1.D0-Y)
      P1 = Q*C/(P+Q-1.D0)
!
      FINSUM = 0.0D0
      N = INT(Q)
      IF (Q.EQ.DBLE(N)) N = N - 1
      DO 50 I=1,N
        IF (P1.LE.1.0D0 .AND. TERM/EPS.LE.FINSUM) GO TO 60
        XI = I
        TERM = (Q-XI+1.0D0)*C*TERM/(P+Q-XI)
!
        IF (TERM.GT.1.0D0) IB = IB - 1
        IF (TERM.GT.1.0D0) TERM = TERM*SML
!
        IF (IB.EQ.0) FINSUM = FINSUM + TERM
 50   CONTINUE
!
 60   DBETAI = DBETAI + FINSUM
 70   IF (Y.NE.X .OR. P.NE.PIN) DBETAI = 1.0D0 - DBETAI
      DBETAI = MAX (MIN (DBETAI, 1.0D0), 0.0D0)
      RETURN
!
 80   DBETAI = 0.0D0
      XB = P*LOG(MAX(Y,SML)) - LOG(P) - DLBETA(P,Q)
      IF (XB.GT.ALNSML .AND. Y.NE.0.0D0) DBETAI = EXP(XB)
      IF (Y.NE.X .OR. P.NE.PIN) DBETAI = 1.0D0 - DBETAI
!
      RETURN
      END FUNCTION DBETAI 
      DOUBLE PRECISION FUNCTION DEBYE1(XVALUE)
!
!
!   DEFINITION:
!
!      This program calculates the Debye function of order 1, defined as
!
!            DEBYE1(x) = [Integral {0 to x} t/(exp(t)-1) dt] / x
!
!      The code uses Chebyshev series whose coefficients
!      are given to 20 decimal places.
!
!
!   ERROR RETURNS:
!
!      If XVALUE < 0.0 an error message is printed and the
!      function returns the value 0.0
!
!
!   MACHINE-DEPENDENT PARAMETERS:
!
!      NTERMS - INTEGER - The no. of elements of the array ADEB1.
!                         The recommended value is such that
!                             ABS(ADEB1(NTERMS)) < EPS/100 , with
!                                   1 <= NTERMS <= 18
!
!      XLOW - DOUBLE PRECISION - The value below which
!                    DEBYE1 = 1 - x/4 + x*x/36 to machine precision.
!                    The recommended value is
!                        SQRT(8*EPSNEG)
!
!      XUPPER - DOUBLE PRECISION - The value above which
!                      DEBYE1 = (pi*pi/(6*x)) - exp(-x)(x+1)/x.
!                      The recommended value is
!                          -LOG(2*EPS)
!
!      XLIM - DOUBLE PRECISION - The value above which DEBYE1 = pi*pi/(6*x)
!                    The recommended value is
!                          -LOG(XMIN)
!
!      For values of EPS, EPSNEG, and XMIN see the file MACHCON.TXT
!
!      The machine-dependent constants are computed internally by
!      using the D1MACH subroutine.
!
!
!   INTRINSIC FUNCTIONS USED:
!
!      AINT , EXP , INT , LOG , SQRT
!
!
!   OTHER MISCFUN SUBROUTINES USED:
!
!          CHEVAL , ERRPRN, D1MACH
!
!
!   AUTHOR:
!          Dr. Allan J. MacLeod,
!          Dept. of Mathematics and Statistics,
!          University of Paisley
!          High St.
!          PAISLEY
!          SCOTLAND
!          PA1 2BE
!
!          (e-mail:  macl_ms0@paisley.ac.uk )
!
!
!   LATEST UPDATE:  23 january, 1996
!
      INTEGER I,NEXP,NTERMS
      DOUBLE PRECISION ADEB1(0:18),CHEVAL,DEBINF,EIGHT,EXPMX,FOUR,HALF,   &
           NINE,ONE,ONEHUN,QUART,RK,SUM,T,THIRT6,X,XK,XLIM,XLOW,   &
           XUPPER,XVALUE,ZERO
!CCCC CHARACTER FNNAME*6,ERRMSG*17
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!CCCC DATA FNNAME/'DEBYE1'/
!CCCC DATA ERRMSG/'ARGUMENT NEGATIVE'/
      DATA ZERO,QUART/0.0D0 , 0.25D0/
      DATA HALF,ONE/0.5D0 , 1.0D0/
      DATA FOUR,EIGHT/4.0D0 , 8.0D0/
      DATA NINE,THIRT6,ONEHUN/9.0D0 , 36.0D0 , 100.0D0/
      DATA DEBINF/0.60792710185402662866D0/
      DATA ADEB1/2.40065971903814101941D0,   &
                 0.19372130421893600885D0,   &
                -0.623291245548957703D-2,   &
                 0.35111747702064800D-3,   &
                -0.2282224667012310D-4,   &
                 0.158054678750300D-5,   &
                -0.11353781970719D-6,   &
                 0.835833611875D-8,   &
                -0.62644247872D-9,   &
                 0.4760334890D-10,   &
                -0.365741540D-11,   &
                 0.28354310D-12,   &
                -0.2214729D-13,   &
                 0.174092D-14,   &
                -0.13759D-15,   &
                 0.1093D-16,   &
                -0.87D-18,   &
                 0.7D-19,   &
                -0.1D-19/
!
!   Start computation
!
      X = XVALUE
!
!   Check XVALUE >= 0.0
!
      IF ( X .LT. ZERO ) THEN
!CCCC    CALL ERRPRN(FNNAME,ERRMSG)
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,101)X
         CALL DPWRST('XXX','BUG ')
         DEBYE1 = ZERO
         RETURN
      ENDIF
  999 FORMAT(1X)
  101 FORMAT('***** ERROR FROM DEBYE1--ARGUMENT MUST BE ',   &
             'NON-NEGATIVE, ARGUMENT = ',G15.7)
!
!   Compute the machine-dependent constants.
!
      T = D1MACH(3)
      XLOW = SQRT ( T * EIGHT )
      XUPPER = - LOG( T + T )
      XLIM = - LOG( D1MACH(1) )
      T = T / ONEHUN
      DO 10 NTERMS = 18 , 0 , -1
         IF ( ABS(ADEB1(NTERMS)) .GT. T ) GO TO  19
 10   CONTINUE
!
!   Code for x <= 4.0
!
 19   IF ( X .LE. FOUR ) THEN
         IF ( X .LT. XLOW ) THEN
            DEBYE1 = ( ( X - NINE ) * X + THIRT6 ) / THIRT6
         ELSE
            T = ( ( X * X / EIGHT ) - HALF ) - HALF
            DEBYE1 = CHEVAL( NTERMS , ADEB1 , T ) - QUART * X
         ENDIF
      ELSE
!
!   Code for x > 4.0
!
         DEBYE1 = ONE / ( X * DEBINF )
         IF ( X .LT. XLIM ) THEN
            EXPMX = EXP( -X )
            IF ( X .GT. XUPPER ) THEN
               DEBYE1 = DEBYE1 - EXPMX * ( ONE + ONE / X )
            ELSE
               SUM = ZERO
               RK = AINT( XLIM / X )
               NEXP = INT( RK )
               XK = RK * X
               DO 100 I = NEXP,1,-1
                  T =  ( ONE + ONE / XK ) / RK
                  SUM = SUM * EXPMX + T
                  RK = RK - ONE
                  XK = XK - X
 100           CONTINUE
               DEBYE1 = DEBYE1 - SUM * EXPMX
            ENDIF
         ENDIF
      ENDIF
      RETURN
      END FUNCTION DEBYE1
      DOUBLE PRECISION FUNCTION DEBYE2(XVALUE)
!
!
!   DEFINITION:
!
!      This program calculates the Debye function of order 1, defined as
!
!            DEBYE2(x) = 2*[Integral {0 to x} t*t/(exp(t)-1) dt] / (x*x)
!
!      The code uses Chebyshev series whose coefficients
!      are given to 20 decimal places.
!
!
!   ERROR RETURNS:
!
!      If XVALUE < 0.0 an error message is printed and the
!      function returns the value 0.0
!
!
!   MACHINE-DEPENDENT PARAMETERS:
!
!      NTERMS - INTEGER - The no. of elements of the array ADEB2.
!                         The recommended value is such that
!                             ABS(ADEB2(NTERMS)) < EPS/100,
!                         subject to 1 <= NTERMS <= 18.
!
!      XLOW - DOUBLE PRECISION - The value below which
!                    DEBYE2 = 1 - x/3 + x*x/24 to machine precision.
!                    The recommended value is
!                        SQRT(8*EPSNEG)
!
!      XUPPER - DOUBLE PRECISION - The value above which
!                      DEBYE2 = (4*zeta(3)/x^2) - 2*exp(-x)(x^2+2x+1)/x^2.
!                      The recommended value is
!                          -LOG(2*EPS)
!
!      XLIM1 - DOUBLE PRECISION - The value above which DEBYE2 = 4*zeta(3)/x^2
!                     The recommended value is
!                          -LOG(XMIN)
!
!      XLIM2 - DOUBLE PRECISION - The value above which DEBYE2 = 0.0 to machine
!                     precision. The recommended value is
!                           SQRT(4.8/XMIN)
!
!      For values of EPS, EPSNEG, and XMIN see the file MACHCON.TXT
!
!
!      The machine-dependent constants are computed internally by
!      using the D1MACH subroutine.
!
!
!   INTRINSIC FUNCTIONS USED:
!
!      AINT , EXP , INT , LOG , SQRT
!
!
!   OTHER MISCFUN SUBROUTINES USED:
!
!          CHEVAL , ERRPRN, D1MACH
!
!
!   AUTHOR:
!          Dr. Allan J. MacLeod,
!          Dept. of Mathematics and Statistics,
!          University of Paisley
!          High St.
!          PAISLEY
!          SCOTLAND
!          PA1 2BE
!
!          (e-mail:  macl_ms0@paisley.ac.uk )
!
!
!   LATEST UPDATE:  23 January, 1996
!
      INTEGER I,NEXP,NTERMS
      DOUBLE PRECISION ADEB2(0:18),CHEVAL,DEBINF,EIGHT,EXPMX,FOUR,   &
           HALF,ONE,ONEHUN,RK,SUM,T,THREE,TWENT4,TWO,X,XK,XLIM1,   &
           XLIM2,XLOW,XUPPER,XVALUE,ZERO
!CCCC CHARACTER FNNAME*6,ERRMSG*17
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!CCCC DATA FNNAME/'DEBYE2'/
!CCCC DATA ERRMSG/'ARGUMENT NEGATIVE'/
      DATA ZERO,HALF/0.0D0 , 0.5D0/
      DATA ONE,TWO,THREE/1.0D0 , 2.0D0 , 3.0D0/
      DATA FOUR,EIGHT,TWENT4/4.0D0 , 8.0D0 , 24.0D0/
      DATA ONEHUN/100.0D0/
      DATA DEBINF/4.80822761263837714160D0/
      DATA ADEB2/2.59438102325707702826D0,   &
                 0.28633572045307198337D0,   &
                -0.1020626561580467129D-1,   &
                 0.60491097753468435D-3,   &
                -0.4052576589502104D-4,   &
                 0.286338263288107D-5,   &
                -0.20863943030651D-6,   &
                 0.1552378758264D-7,   &
                -0.117312800866D-8,   &
                 0.8973585888D-10,   &
                -0.693176137D-11,   &
                 0.53980568D-12,   &
                -0.4232405D-13,   &
                 0.333778D-14,   &
                -0.26455D-15,   &
                 0.2106D-16,   &
                -0.168D-17,   &
                 0.13D-18,   &
                -0.1D-19/
!
!   Start computation
!
      X = XVALUE
!
!   Check XVALUE >= 0.0
!
      IF ( X .LT. ZERO ) THEN
!CCCC    CALL ERRPRN(FNNAME,ERRMSG)
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,101)X
         CALL DPWRST('XXX','BUG ')
         DEBYE2 = ZERO
         RETURN
      ENDIF
  999 FORMAT(1X)
  101 FORMAT('***** ERROR FROM DEBYE2--ARGUMENT MUST BE ',   &
             'NON-NEGATIVE, ARGUMENT = ',G15.7)
!
!   Compute the machine-dependent constants.
!
      T = D1MACH(1)
      XLIM1 = - LOG( T )
      XLIM2 = SQRT( DEBINF ) / SQRT( T )
      T = D1MACH(3)
      XLOW = SQRT ( T * EIGHT )
      XUPPER = - LOG( T + T )
      T = T / ONEHUN
      DO 10 NTERMS = 18 , 0 , -1
         IF ( ABS(ADEB2(NTERMS)) .GT. T ) GO TO  19
 10   CONTINUE
!
!   Code for x <= 4.0
!
 19   IF ( X .LE. FOUR ) THEN
         IF ( X .LT. XLOW ) THEN
            DEBYE2 = ( ( X - EIGHT ) * X + TWENT4 ) / TWENT4
         ELSE
            T = ( ( X * X / EIGHT ) - HALF ) - HALF
            DEBYE2 = CHEVAL ( NTERMS , ADEB2 , T ) - X / THREE
         ENDIF
      ELSE
!
!   Code for x > 4.0
!
         IF ( X .GT. XLIM2 ) THEN
            DEBYE2 = ZERO
         ELSE
            DEBYE2 = DEBINF / ( X * X )
            IF ( X .LT. XLIM1 ) THEN
               EXPMX = EXP ( -X )
               IF ( X .GT. XUPPER ) THEN
                  SUM = ( ( X + TWO ) * X + TWO ) / ( X * X )
               ELSE
                  SUM = ZERO
                  RK = AINT ( XLIM1 / X )
                  NEXP = INT ( RK )
                  XK = RK * X
                  DO 100 I = NEXP,1,-1
                     T =  ( ONE + TWO / XK + TWO / ( XK*XK ) ) / RK
                     SUM = SUM * EXPMX + T
                     RK = RK - ONE
                     XK = XK - X
 100              CONTINUE
               ENDIF
               DEBYE2 = DEBYE2 - TWO * SUM * EXPMX
            ENDIF
         ENDIF
      ENDIF
      RETURN
      END FUNCTION DEBYE2
      DOUBLE PRECISION FUNCTION DEBYE3(XVALUE)
!
!
!   DEFINITION:
!
!      This program calculates the Debye function of order 3, defined as
!
!            DEBYE3(x) = 3*[Integral {0 to x} t^3/(exp(t)-1) dt] / (x^3)
!
!      The code uses Chebyshev series whose coefficients
!      are given to 20 decimal places.
!
!
!   ERROR RETURNS:
!
!      If XVALUE < 0.0 an error message is printed and the
!      function returns the value 0.0
!
!
!   MACHINE-DEPENDENT PARAMETERS:
!
!      NTERMS - INTEGER - The no. of elements of the array ADEB3.
!                         The recommended value is such that
!                             ABS(ADEB3(NTERMS)) < EPS/100,
!                         subject to 1 <= NTERMS <= 18
!
!      XLOW - DOUBLE PRECISION - The value below which
!                    DEBYE3 = 1 - 3x/8 + x*x/20 to machine precision.
!                    The recommended value is
!                        SQRT(8*EPSNEG)
!
!      XUPPER - DOUBLE PRECISION - The value above which
!               DEBYE3 = (18*zeta(4)/x^3) - 3*exp(-x)(x^3+3x^2+6x+6)/x^3.
!                      The recommended value is
!                          -LOG(2*EPS)
!
!      XLIM1 - DOUBLE PRECISION - The value above which DEBYE3 = 18*zeta(4)/x^3
!                     The recommended value is
!                          -LOG(XMIN)
!
!      XLIM2 - DOUBLE PRECISION - The value above which DEBYE3 = 0.0 to machine
!                     precision. The recommended value is
!                          CUBE ROOT(19/XMIN)
!
!      For values of EPS, EPSNEG, and XMIN see the file MACHCON.TXT
!
!      The machine-dependent constants are computed internally by
!      using the D1MACH subroutine.
!
!
!   OTHER MISCFUN SUBROUTINES USED:
!
!          CHEVAL , ERRPRN, D1MACH
!
!
!   INTRINSIC FUNCTIONS USED:
!
!      AINT , EXP , INT , LOG , SQRT
!
!
!   AUTHOR:
!          Dr. Allan J. MacLeod,
!          Dept. of Mathematics and Statistics,
!          University of Paisley
!          High St.
!          PAISLEY
!          SCOTLAND
!          PA1 2BE
!
!          (e-mail:  macl_ms0@paisley.ac.uk )
!
!
!   LATEST UPDATE:  23 January, 1996
!
      INTEGER I,NEXP,NTERMS
      DOUBLE PRECISION ADEB3(0:18),CHEVAL,DEBINF,EIGHT,EXPMX,FOUR,   &
           HALF,ONE,ONEHUN,PT375,RK,SEVP5,SIX,SUM,T,THREE,TWENTY,X,   &
           XK,XKI,XLIM1,XLIM2,XLOW,XUPPER,XVALUE,ZERO
!CCCC CHARACTER FNNAME*6,ERRMSG*17
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!CCCC DATA FNNAME/'DEBYE3'/
!CCCC DATA ERRMSG/'ARGUMENT NEGATIVE'/
      DATA ZERO,PT375/0.0D0 , 0.375D0/
      DATA HALF,ONE/0.5D0 , 1.0D0/
      DATA THREE,FOUR,SIX/3.0D0 , 4.0D0 , 6.0D0/
      DATA SEVP5,EIGHT,TWENTY/7.5D0 , 8.0D0 , 20.0D0/
      DATA ONEHUN/100.0D0/
      DATA DEBINF/0.51329911273421675946D-1/
      DATA ADEB3/2.70773706832744094526D0,   &
                 0.34006813521109175100D0,   &
                -0.1294515018444086863D-1,   &
                 0.79637553801738164D-3,   &
                -0.5463600095908238D-4,   &
                 0.392430195988049D-5,   &
                -0.28940328235386D-6,   &
                 0.2173176139625D-7,   &
                -0.165420999498D-8,   &
                 0.12727961892D-9,   &
                -0.987963459D-11,   &
                 0.77250740D-12,   &
                -0.6077972D-13,   &
                 0.480759D-14,   &
                -0.38204D-15,   &
                 0.3048D-16,   &
                -0.244D-17,   &
                 0.20D-18,   &
                -0.2D-19/
!
!   Start computation
!
      X = XVALUE
!
!   Error test
!
      IF ( X .LT. ZERO ) THEN
!CCCC    CALL ERRPRN(FNNAME,ERRMSG)
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,101)X
         CALL DPWRST('XXX','BUG ')
         DEBYE3 = ZERO
         RETURN
      ENDIF
  999 FORMAT(1X)
  101 FORMAT('***** ERROR FROM DEBYE3--ARGUMENT MUST BE ',   &
             'NON-NEGATIVE, ARGUMENT = ',G15.7)
!
!   Compute the machine-dependent constants.
!
      T = D1MACH(1)
      XLIM1 = - LOG( T )
      XK = ONE / THREE
      XKI = (ONE/DEBINF) ** XK
      RK = T ** XK
      XLIM2 = XKI / RK
      T = D1MACH(3)
      XLOW = SQRT ( T * EIGHT )
      XUPPER = - LOG( T + T )
      T = T / ONEHUN
      DO 10 NTERMS = 18 , 0 , -1
         IF ( ABS(ADEB3(NTERMS)) .GT. T ) GO TO  19
 10   CONTINUE
!
!   Code for x <= 4.0
!
 19   IF ( X .LE. FOUR ) THEN
         IF ( X .LT. XLOW ) THEN
            DEBYE3 = ( ( X - SEVP5 ) * X + TWENTY ) / TWENTY
         ELSE
            T = ( ( X * X / EIGHT ) - HALF ) - HALF
            DEBYE3 = CHEVAL ( NTERMS , ADEB3 , T ) - PT375 * X
         ENDIF
      ELSE
!
!   Code for x > 4.0
!
         IF ( X .GT. XLIM2 ) THEN
            DEBYE3 = ZERO
         ELSE
            DEBYE3 = ONE / ( DEBINF * X * X * X )
            IF ( X .LT. XLIM1 ) THEN
               EXPMX = EXP ( -X )
               IF ( X .GT. XUPPER ) THEN
                  SUM = (((X+THREE)*X+SIX)*X+SIX) / (X*X*X)
               ELSE
                  SUM = ZERO
                  RK = AINT ( XLIM1 / X )
                  NEXP = INT ( RK )
                  XK = RK * X
                  DO 100 I = NEXP,1,-1
                     XKI = ONE / XK
                     T =  (((SIX*XKI+SIX)*XKI+THREE)*XKI+ONE) / RK
                     SUM = SUM * EXPMX + T
                     RK = RK - ONE
                     XK = XK - X
 100              CONTINUE
               ENDIF
               DEBYE3 = DEBYE3 - THREE * SUM * EXPMX
            ENDIF
         ENDIF
      ENDIF
      RETURN
      END FUNCTION DEBYE3
      DOUBLE PRECISION FUNCTION DEBYE4(XVALUE)
!
!
!   DEFINITION:
!
!      This program calculates the Debye function of order 4, defined as
!
!            DEBYE4(x) = 4*[Integral {0 to x} t^4/(exp(t)-1) dt] / (x^4)
!
!      The code uses Chebyshev series whose coefficients
!      are given to 20 decimal places.
!
!
!   ERROR RETURNS:
!
!      If XVALUE < 0.0 an error message is printed and the
!      function returns the value 0.0
!
!
!   MACHINE-DEPENDENT PARAMETERS:
!
!      NTERMS - INTEGER - The no. of elements of the array ADEB4.
!                         The recommended value is such that
!                             ABS(ADEB4(NTERMS)) < EPS/100,
!                         subject to 1 <= NTERMS <= 18
!
!      XLOW - DOUBLE PRECISION - The value below which
!                    DEBYE4 = 1 - 4x/10 + x*x/18 to machine precision.
!                    The recommended value is
!                        SQRT(8*EPSNEG)
!
!      XUPPER - DOUBLE PRECISION - The value above which
!               DEBYE4=(96*zeta(5)/x^4)-4*exp(-x)(x^4+4x^2+12x^2+24x+24)/x^4.
!                      The recommended value is
!                          -LOG(2*EPS)
!
!      XLIM1 - DOUBLE PRECISION - The value above which DEBYE4 = 96*zeta(5)/x^4
!                     The recommended value is
!                          -LOG(XMIN)
!
!      XLIM2 - DOUBLE PRECISION - The value above which DEBYE4 = 0.0 to machine
!                     precision. The recommended value is
!                          FOURTH ROOT(99/XMIN)
!
!      For values of EPS, EPSNEG, and XMIN see the file MACHCON.TXT
!
!
!      The machine-dependent constants are computed internally by
!      using the D1MACH subroutine.
!
!
!   INTRINSIC FUNCTIONS USED:
!
!      AINT , EXP , INT , LOG , SQRT
!
!
!   OTHER MISCFUN SUBROUTINES USED:
!
!          CHEVAL , ERRPRN, D1MACH
!
!
!   AUTHOR:
!          Dr. Allan J. MacLeod,
!          Dept. of Mathematics and Statistics,
!          University of Paisley
!          High St.
!          PAISLEY
!          SCOTLAND
!          PA1 2BE
!
!          (e-mail:  macl_ms0@paisley.ac.uk )
!
!
!   LATEST UPDATE:  23 January, 1996
!
      INTEGER I,NEXP,NTERMS
      DOUBLE PRECISION ADEB4(0:18),CHEVAL,DEBINF,EIGHT,EIGHTN,EXPMX,   &
           FIVE,FOUR,FORTY5,HALF,ONE,ONEHUN,RK,SUM,T,TWELVE,TWENT4,   &
           TWOPT5,X,XK,XKI,XLIM1,XLIM2,XLOW,XUPPER,XVALUE,ZERO
!CCCC CHARACTER FNNAME*6,ERRMSG*17
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!CCCC DATA FNNAME/'DEBYE4'/
!CCCC DATA ERRMSG/'ARGUMENT NEGATIVE'/
      DATA ZERO,HALF,ONE/0.0D0 , 0.5D0 , 1.0D0/
      DATA TWOPT5,FOUR,FIVE/2.5D0 , 4.0D0 , 5.0D0/
      DATA EIGHT,TWELVE,EIGHTN/8.0D0 , 12.0D0 , 18.0D0/
      DATA TWENT4,FORTY5,ONEHUN/24.0D0 , 45.0D0 , 100.0D0/
      DATA DEBINF/99.54506449376351292781D0/
      DATA ADEB4/2.78186941502052346008D0,   &
                 0.37497678352689286364D0,   &
                -0.1494090739903158326D-1,   &
                 0.94567981143704274D-3,   &
                -0.6613291613893255D-4,   &
                 0.481563298214449D-5,   &
                -0.35880839587593D-6,   &
                 0.2716011874160D-7,   &
                -0.208070991223D-8,   &
                 0.16093838692D-9,   &
                -0.1254709791D-10,   &
                 0.98472647D-12,   &
                -0.7772369D-13,   &
                 0.616483D-14,   &
                -0.49107D-15,   &
                 0.3927D-16,   &
                -0.315D-17,   &
                 0.25D-18,   &
                -0.2D-19/
!
!   Start computation
!
      X = XVALUE
!
!   Check XVALUE >= 0.0
!
      IF ( X .LT. ZERO ) THEN
!CCCC    CALL ERRPRN(FNNAME,ERRMSG)
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,101)X
         CALL DPWRST('XXX','BUG ')
         DEBYE4 = ZERO
         RETURN
      ENDIF
  999 FORMAT(1X)
  101 FORMAT('***** ERROR FROM DEBYE4--ARGUMENT MUST BE ',   &
             'NON-NEGATIVE, ARGUMENT = ',G15.7)
!
!   Compute the machine-dependent constants.
!
      T = D1MACH(1)
      XLIM1 = - LOG( T )
      RK = ONE / FOUR
      XK = DEBINF ** RK
      XKI = T ** RK
      XLIM2 = XK / XKI
      T = D1MACH(3)
      XLOW = SQRT ( T * EIGHT )
      XUPPER = - LOG( T + T )
      T = T / ONEHUN
      DO 10 NTERMS = 18 , 0 , -1
         IF ( ABS(ADEB4(NTERMS)) .GT. T ) GO TO  19
 10   CONTINUE
!
!   Code for x <= 4.0
!
 19   IF ( X .LE. FOUR ) THEN
         IF ( X .LT. XLOW ) THEN
            DEBYE4 = ( ( TWOPT5 * X - EIGHTN ) * X + FORTY5 ) / FORTY5
         ELSE
            T = ( ( X * X / EIGHT ) - HALF ) - HALF
            DEBYE4 = CHEVAL ( NTERMS , ADEB4 , T ) - ( X + X ) / FIVE
         ENDIF
      ELSE
!
!   Code for x > 4.0
!
         IF ( X .GT. XLIM2 ) THEN
            DEBYE4 = ZERO
         ELSE
            T = X * X
            DEBYE4 = ( DEBINF / T ) / T
            IF ( X .LT. XLIM1 ) THEN
               EXPMX = EXP ( -X )
               IF ( X .GT. XUPPER ) THEN
                  SUM = ( ( ( ( X + FOUR ) * X + TWELVE ) * X +   &
                        TWENT4 ) * X + TWENT4 ) / ( X * X * X * X )
               ELSE
                  SUM = ZERO
                  RK = AINT ( XLIM1 / X )
                  NEXP = INT ( RK )
                  XK = RK * X
                  DO 100 I = NEXP,1,-1
                     XKI = ONE / XK
                     T =  ( ( ( ( TWENT4 * XKI + TWENT4 ) * XKI +   &
                          TWELVE ) * XKI + FOUR ) * XKI + ONE ) / RK
                     SUM = SUM * EXPMX + T
                     RK = RK - ONE
                     XK = XK - X
 100              CONTINUE
               ENDIF
               DEBYE4 = DEBYE4 - FOUR * SUM * EXPMX
            ENDIF
         ENDIF
      ENDIF
      RETURN
      END FUNCTION DEBYE4
      SUBROUTINE DCHEX(R,LDR,P,K,L,Z,LDZ,NZ,C,S,JOB)
!***BEGIN PROLOGUE  DCHEX
!***DATE WRITTEN   780814   (YYMMDD)
!***REVISION DATE  820801   (YYMMDD)
!***REVISION HISTORY  (YYMMDD)
!   000330  Modified array declarations.  (JEC)
!***CATEGORY NO.  D7B
!***KEYWORDS  CHOLESKY DECOMPOSITION,DOUBLE PRECISION,EXCHANGE,
!             LINEAR ALGEBRA,LINPACK,MATRIX,POSITIVE DEFINITE
!***AUTHOR  STEWART, G. W., (U. OF MARYLAND)
!***PURPOSE  Updates the Cholesky factorization  A=TRANS(R)*R  of a
!            POSITIVE DEFINITE matrix A of order P under diagonal
!            permutations of the form  TRANS(E)*A*E  where E is a
!            permutation matrix.
!***DESCRIPTION
!
!     DCHEX updates the Cholesky factorization
!
!                   A = TRANS(R)*R
!
!     of a positive definite matrix A of order P under diagonal
!     permutations of the form
!
!                   TRANS(E)*A*E
!
!     where E is a permutation matrix.  Specifically, given
!     an upper triangular matrix R and a permutation matrix
!     E (which is specified by K, L, and JOB), DCHEX determines
!     an orthogonal matrix U such that
!
!                           U*R*E = RR,
!
!     where RR is upper triangular.  At the users option, the
!     transformation U will be multiplied into the array Z.
!     If A = TRANS(X)*X, so that R is the triangular part of the
!     QR factorization of X, then RR is the triangular part of the
!     QR factorization of X*E, i.e. X with its columns permuted.
!     For a less terse description of what DCHEX does and how
!     it may be applied, see the LINPACK guide.
!
!     The matrix Q is determined as the product U(L-K)*...*U(1)
!     of plane rotations of the form
!
!                           (    C(I)       S(I) )
!                           (                    ) ,
!                           (    -S(I)      C(I) )
!
!     where C(I) is double precision.  The rows these rotations operate
!     on are described below.
!
!     There are two types of permutations, which are determined
!     by the value of JOB.
!
!     1. Right circular shift (JOB = 1).
!
!         The columns are rearranged in the following order.
!
!                1,...,K-1,L,K,K+1,...,L-1,L+1,...,P.
!
!         U is the product of L-K rotations U(I), where U(I)
!         acts in the (L-I,L-I+1)-plane.
!
!     2. Left circular shift (JOB = 2).
!         The columns are rearranged in the following order
!
!                1,...,K-1,K+1,K+2,...,L,K,L+1,...,P.
!
!         U is the product of L-K rotations U(I), where U(I)
!         acts in the (K+I-1,K+I)-plane.
!
!     On Entry
!
!         R      DOUBLE PRECISION(LDR,P), where LDR .GE. P.
!                R contains the upper triangular factor
!                that is to be updated.  Elements of R
!                below the diagonal are not referenced.
!
!         LDR    INTEGER.
!                LDR is the leading dimension of the array R.
!
!         P      INTEGER.
!                P is the order of the matrix R.
!
!         K      INTEGER.
!                K is the first column to be permuted.
!
!         L      INTEGER.
!                L is the last column to be permuted.
!                L must be strictly greater than K.
!
!         Z      DOUBLE PRECISION(LDZ,N)Z), where LDZ .GE. P.
!                Z is an array of NZ P-vectors into which the
!                transformation U is multiplied.  Z is
!                not referenced if NZ = 0.
!
!         LDZ    INTEGER.
!                LDZ is the leading dimension of the array Z.
!
!         NZ     INTEGER.
!                NZ is the number of columns of the matrix Z.
!
!         JOB    INTEGER.
!                JOB determines the type of permutation.
!                       JOB = 1  right circular shift.
!                       JOB = 2  left circular shift.
!
!     On Return
!
!         R      contains the updated factor.
!
!         Z      contains the updated matrix Z.
!
!         C      DOUBLE PRECISION(P).
!                C contains the cosines of the transforming rotations.
!
!         S      DOUBLE PRECISION(P).
!                S contains the sines of the transforming rotations.
!
!     LINPACK.  This version dated 08/14/78 .
!     G. W. Stewart, University of Maryland, Argonne National Lab.
!
!     DCHEX uses the following functions and subroutines.
!
!     BLAS DROTG
!     Fortran MIN0
!***REFERENCES  DONGARRA J.J., BUNCH J.R., MOLER C.B., STEWART G.W.,
!                 *LINPACK USERS  GUIDE*, SIAM, 1979.
!***ROUTINES CALLED  DROTG
!***END PROLOGUE  DCHEX
      INTEGER LDR,P,K,L,LDZ,NZ,JOB
      DOUBLE PRECISION R(LDR,*),Z(LDZ,*),S(*)
      DOUBLE PRECISION C(*)
!
      INTEGER I,II,IL,IU,J,JJ,KM1,KP1,LMK,LM1
!CCCC DOUBLE PRECISION RJP1J,T
      DOUBLE PRECISION T
!
!     INITIALIZE
!
!***FIRST EXECUTABLE STATEMENT  DCHEX
      KM1 = K - 1
      KP1 = K + 1
      LMK = L - K
      LM1 = L - 1
!
!     PERFORM THE APPROPRIATE TASK.
!
      GO TO (10,130), JOB
!
!     RIGHT CIRCULAR SHIFT.
!
   10 CONTINUE
!
!        REORDER THE COLUMNS.
!
         DO 20 I = 1, L
            II = L - I + 1
            S(I) = R(II,L)
   20    CONTINUE
         DO 40 JJ = K, LM1
            J = LM1 - JJ + K
            DO 30 I = 1, J
               R(I,J+1) = R(I,J)
   30       CONTINUE
            R(J+1,J+1) = 0.0D0
   40    CONTINUE
         IF (K .EQ. 1) GO TO 60
            DO 50 I = 1, KM1
               II = L - I + 1
               R(I,K) = S(II)
   50       CONTINUE
   60    CONTINUE
!
!        CALCULATE THE ROTATIONS.
!
         T = S(1)
         DO 70 I = 1, LMK
            CALL DROTG(S(I+1),T,C(I),S(I))
            T = S(I+1)
   70    CONTINUE
         R(K,K) = T
         DO 90 J = KP1, P
            IL = MAX0(1,L-J+1)
            DO 80 II = IL, LMK
               I = L - II
               T = C(II)*R(I,J) + S(II)*R(I+1,J)
               R(I+1,J) = C(II)*R(I+1,J) - S(II)*R(I,J)
               R(I,J) = T
   80       CONTINUE
   90    CONTINUE
!
!        IF REQUIRED, APPLY THE TRANSFORMATIONS TO Z.
!
         IF (NZ .LT. 1) GO TO 120
         DO 110 J = 1, NZ
            DO 100 II = 1, LMK
               I = L - II
               T = C(II)*Z(I,J) + S(II)*Z(I+1,J)
               Z(I+1,J) = C(II)*Z(I+1,J) - S(II)*Z(I,J)
               Z(I,J) = T
  100       CONTINUE
  110    CONTINUE
  120    CONTINUE
      GO TO 260
!
!     LEFT CIRCULAR SHIFT
!
  130 CONTINUE
!
!        REORDER THE COLUMNS
!
         DO 140 I = 1, K
            II = LMK + I
            S(II) = R(I,K)
  140    CONTINUE
         DO 160 J = K, LM1
            DO 150 I = 1, J
               R(I,J) = R(I,J+1)
  150       CONTINUE
            JJ = J - KM1
            S(JJ) = R(J+1,J+1)
  160    CONTINUE
         DO 170 I = 1, K
            II = LMK + I
            R(I,L) = S(II)
  170    CONTINUE
         DO 180 I = KP1, L
            R(I,L) = 0.0D0
  180    CONTINUE
!
!        REDUCTION LOOP.
!
         DO 220 J = K, P
            IF (J .EQ. K) GO TO 200
!
!              APPLY THE ROTATIONS.
!
               IU = MIN0(J-1,L-1)
               DO 190 I = K, IU
                  II = I - K + 1
                  T = C(II)*R(I,J) + S(II)*R(I+1,J)
                  R(I+1,J) = C(II)*R(I+1,J) - S(II)*R(I,J)
                  R(I,J) = T
  190          CONTINUE
  200       CONTINUE
            IF (J .GE. L) GO TO 210
               JJ = J - K + 1
               T = S(JJ)
               CALL DROTG(R(J,J),T,C(JJ),S(JJ))
  210       CONTINUE
  220    CONTINUE
!
!        APPLY THE ROTATIONS TO Z.
!
         IF (NZ .LT. 1) GO TO 250
         DO 240 J = 1, NZ
            DO 230 I = K, LM1
               II = I - KM1
               T = C(II)*Z(I,J) + S(II)*Z(I+1,J)
               Z(I+1,J) = C(II)*Z(I+1,J) - S(II)*Z(I,J)
               Z(I,J) = T
  230       CONTINUE
  240    CONTINUE
  250    CONTINUE
  260 CONTINUE
      RETURN
      END SUBROUTINE DCHEX
      DOUBLE PRECISION FUNCTION DCHU (A, B, X)
!***BEGIN PROLOGUE  DCHU
!***PURPOSE  Compute the logarithmic confluent hypergeometric function.
!***LIBRARY   SLATEC (FNLIB)
!***CATEGORY  C11
!***TYPE      DOUBLE PRECISION (CHU-S, DCHU-D)
!***KEYWORDS  FNLIB, LOGARITHMIC CONFLUENT HYPERGEOMETRIC FUNCTION,
!             SPECIAL FUNCTIONS
!***AUTHOR  Fullerton, W., (LANL)
!***DESCRIPTION
!
! DCHU(A,B,X) calculates the double precision logarithmic confluent
! hypergeometric function U(A,B,X) for double precision arguments
! A, B, and X.
!
! This routine is not valid when 1+A-B is close to zero if X is small.
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  D1MACH, D9CHU, DEXPRL, DGAMMA, DGAMR, DPOCH,
!                    DPOCH1, XERMSG
!***REVISION HISTORY  (YYMMDD)
!   770801  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890531  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!   900727  Added EXTERNAL statement.  (WRB)
!***END PROLOGUE  DCHU
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DOUBLE PRECISION A, B, X, AINTB, ALNX, A0, BEPS, B0, C0, EPS,   &
        FACTOR, GAMRI1, GAMRNI, PCH1AI, PCH1I, PI, POCHAI, SUM, T,   &
        XEPS1, XI, XI1, XN, XTOEPS,  DPOCH, DGAMMA, DGAMR,   &
        DPOCH1, DEXPRL, D9CHU
      EXTERNAL DGAMMA
      SAVE PI, EPS
      DATA PI / 3.141592653589793238462643383279503D0 /
      DATA EPS / 0.0D0 /
!***FIRST EXECUTABLE STATEMENT  DCHU
!
      DCHU = 0.0D0
!
      IF (EPS.EQ.0.0D0) EPS = D1MACH(3)
!
      IF (X .EQ. 0.0D0) THEN
        WRITE(ICOUT,2)
    2   FORMAT('***** ERORR FROM DCHU, X IS ZERO, SO CHU IS ',   &
               'INFINITE.  *******')
        CALL DPWRST('XXX','BUG ')
        RETURN
      ENDIF
      IF (X .LT. 0.0D0) THEN
        WRITE(ICOUT,1)
    1   FORMAT('***** ERORR FROM DCHU, X IS NEGATIVE.  *******')
        CALL DPWRST('XXX','BUG ')
        RETURN
      ENDIF
!
      IF (MAX(ABS(A),1.0D0)*MAX(ABS(1.0D0+A-B),1.0D0).LT.   &
        0.99D0*ABS(X)) GO TO 120
!
! THE ASCENDING SERIES WILL BE USED, BECAUSE THE DESCENDING RATIONAL
! APPROXIMATION (WHICH IS BASED ON THE ASYMPTOTIC SERIES) IS UNSTABLE.
!
      IF (ABS(1.0D0+A-B) .LT. SQRT(EPS)) THEN
        WRITE(ICOUT,3)
    3   FORMAT('***** ERORR FROM DCHU, ALGORITHM IS BAD WHEN 1+A-B ',   &
               'IS NEAR ZERO FOR SMALL X. *****')
        CALL DPWRST('XXX','BUG ')
        RETURN
      ENDIF
!
      AINTB=0.0
      IF (B.GE.0.0D0) AINTB = AINT(B+0.5D0)
      IF (B.LT.0.0D0) AINTB = AINT(B-0.5D0)
      BEPS = B - AINTB
      N = INT(AINTB)
!
      ALNX = LOG(X)
      XTOEPS = EXP (-BEPS*ALNX)
!
! EVALUATE THE FINITE SUM.     -----------------------------------------
!
      IF (N.GE.1) GO TO 40
!
! CONSIDER THE CASE B .LT. 1.0 FIRST.
!
      SUM = 1.0D0
      IF (N.EQ.0) GO TO 30
!
      T = 1.0D0
      M = -N
      DO 20 I=1,M
        XI1 = I - 1
        T = T*(A+XI1)*X/((B+XI1)*(XI1+1.0D0))
        SUM = SUM + T
 20   CONTINUE
!
 30   SUM = DPOCH(1.0D0+A-B, -A)*SUM
      GO TO 70
!
! NOW CONSIDER THE CASE B .GE. 1.0.
!
 40   SUM = 0.0D0
      M = N - 2
      IF (M.LT.0) GO TO 70
      T = 1.0D0
      SUM = 1.0D0
      IF (M.EQ.0) GO TO 60
!
      DO 50 I=1,M
        XI = I
        T = T * (A-B+XI)*X/((1.0D0-B+XI)*XI)
        SUM = SUM + T
 50   CONTINUE
!
 60   SUM = DGAMMA(B-1.0D0) * DGAMR(A) * X**(1-N) * XTOEPS * SUM
!
! NEXT EVALUATE THE INFINITE SUM.     ----------------------------------
!
 70   ISTRT = 0
      IF (N.LT.1) ISTRT = 1 - N
      XI = ISTRT
!
      FACTOR = (-1.0D0)**N * DGAMR(1.0D0+A-B) * X**ISTRT
      IF (BEPS.NE.0.0D0) FACTOR = FACTOR * BEPS*PI/SIN(BEPS*PI)
!
      POCHAI = DPOCH (A, XI)
      GAMRI1 = DGAMR (XI+1.0D0)
      GAMRNI = DGAMR (AINTB+XI)
      B0 = FACTOR * DPOCH(A,XI-BEPS) * GAMRNI * DGAMR(XI+1.0D0-BEPS)
!
      IF (ABS(XTOEPS-1.0D0).GT.0.5D0) GO TO 90
!
! X**(-BEPS) IS CLOSE TO 1.0D0, SO WE MUST BE CAREFUL IN EVALUATING THE
! DIFFERENCES.
!
      PCH1AI = DPOCH1 (A+XI, -BEPS)
      PCH1I = DPOCH1 (XI+1.0D0-BEPS, BEPS)
      C0 = FACTOR * POCHAI * GAMRNI * GAMRI1 * (   &
        -DPOCH1(B+XI,-BEPS) + PCH1AI - PCH1I + BEPS*PCH1AI*PCH1I)
!
! XEPS1 = (1.0 - X**(-BEPS))/BEPS = (X**(-BEPS) - 1.0)/(-BEPS)
      XEPS1 = ALNX*DEXPRL(-BEPS*ALNX)
!
      DCHU = SUM + C0 + XEPS1*B0
      XN = N
      DO 80 I=1,1000
        XI = ISTRT + I
        XI1 = ISTRT + I - 1
        B0 = (A+XI1-BEPS)*B0*X/((XN+XI1)*(XI-BEPS))
        C0 = (A+XI1)*C0*X/((B+XI1)*XI)   &
          - ((A-1.0D0)*(XN+2.D0*XI-1.0D0) + XI*(XI-BEPS)) * B0   &
          / (XI*(B+XI1)*(A+XI1-BEPS))
        T = C0 + XEPS1*B0
        DCHU = DCHU + T
        IF (ABS(T).LT.EPS*ABS(DCHU)) GO TO 130
 80   CONTINUE
      WRITE(ICOUT,4)
    4 FORMAT('***** ERORR FROM DCHU, NO CONVERGENCE IN 1000 TERMS OF ',   &
               'THE ASCENDING SERIES. *****')
      CALL DPWRST('XXX','BUG ')
      RETURN
!
! X**(-BEPS) IS VERY DIFFERENT FROM 1.0, SO THE STRAIGHTFORWARD
! FORMULATION IS STABLE.
!
 90   A0 = FACTOR * POCHAI * DGAMR(B+XI) * GAMRI1 / BEPS
      B0 = XTOEPS * B0 / BEPS
!
      DCHU = SUM + A0 - B0
      DO 100 I=1,1000
        XI = ISTRT + I
        XI1 = ISTRT + I - 1
        A0 = (A+XI1)*A0*X/((B+XI1)*XI)
        B0 = (A+XI1-BEPS)*B0*X/((AINTB+XI1)*(XI-BEPS))
        T = A0 - B0
        DCHU = DCHU + T
        IF (ABS(T).LT.EPS*ABS(DCHU)) GO TO 130
 100  CONTINUE
      WRITE(ICOUT,4)
      CALL DPWRST('XXX','BUG ')
      RETURN
!
! USE LUKE-S RATIONAL APPROXIMATION IN THE ASYMPTOTIC REGION.
!
 120  DCHU = X**(-A) * D9CHU(A,B,X)
!
 130  RETURN
      END FUNCTION DCHU 
      SUBROUTINE DCOPY(N,DX,INCX,DY,INCY)
!
!     COPIES A VECTOR, X, TO A VECTOR, Y.
!     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
!     JACK DONGARRA, LINPACK, 3/11/78.
!
      DOUBLE PRECISION DX(1),DY(1)
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N
!
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
!
!        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
!          NOT EQUAL TO 1
!
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        DY(IY) = DX(IX)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
!
!        CODE FOR BOTH INCREMENTS EQUAL TO 1
!
!
!        CLEAN-UP LOOP
!
   20 M = MOD(N,7)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        DY(I) = DX(I)
   30 CONTINUE
      IF( N .LT. 7 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,7
        DY(I) = DX(I)
        DY(I + 1) = DX(I + 1)
        DY(I + 2) = DX(I + 2)
        DY(I + 3) = DX(I + 3)
        DY(I + 4) = DX(I + 4)
        DY(I + 5) = DX(I + 5)
        DY(I + 6) = DX(I + 6)
   50 CONTINUE
      RETURN
      END SUBROUTINE DCOPY
      DOUBLE PRECISION FUNCTION DCOT (X)
!***BEGIN PROLOGUE  DCOT
!***PURPOSE  Compute the cotangent.
!***LIBRARY   SLATEC (FNLIB)
!***CATEGORY  C4A
!***TYPE      DOUBLE PRECISION (COT-S, DCOT-D, CCOT-C)
!***KEYWORDS  COTANGENT, ELEMENTARY FUNCTIONS, FNLIB, TRIGONOMETRIC
!***AUTHOR  Fullerton, W., (LANL)
!***DESCRIPTION
!
! DCOT(X) calculates the double precision trigonometric cotangent
! for double precision argument X.  X is in units of radians.
!
! Series for COT        on the interval  0.          to  6.25000E-02
!                                        with weighted error   5.52E-34
!                                         log weighted error  33.26
!                               significant figures required  32.34
!                                    decimal places required  33.85
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  D1MACH, DCSEVL, INITDS, XERMSG
!***REVISION HISTORY  (YYMMDD)
!   770601  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890531  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!   920618  Removed space from variable names.  (RWC, WRB)
!***END PROLOGUE  DCOT
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DOUBLE PRECISION X, COTCS(15), AINTY, AINTY2, PI2REC, SQEPS,   &
        XMAX, XMIN, XSML, Y, YREM, PRODBG, DCSEVL
      LOGICAL FIRST
      SAVE COTCS, PI2REC, NTERMS, XMAX, XSML, XMIN, SQEPS, FIRST
      DATA COTCS(  1) / +.240259160982956302509553617744970D+0    /
      DATA COTCS(  2) / -.165330316015002278454746025255758D-1    /
      DATA COTCS(  3) / -.429983919317240189356476228239895D-4    /
      DATA COTCS(  4) / -.159283223327541046023490851122445D-6    /
      DATA COTCS(  5) / -.619109313512934872588620579343187D-9    /
      DATA COTCS(  6) / -.243019741507264604331702590579575D-11   /
      DATA COTCS(  7) / -.956093675880008098427062083100000D-14   /
      DATA COTCS(  8) / -.376353798194580580416291539706666D-16   /
      DATA COTCS(  9) / -.148166574646746578852176794666666D-18   /
      DATA COTCS( 10) / -.583335658903666579477984000000000D-21   /
      DATA COTCS( 11) / -.229662646964645773928533333333333D-23   /
      DATA COTCS( 12) / -.904197057307483326719999999999999D-26   /
      DATA COTCS( 13) / -.355988551920600064000000000000000D-28   /
      DATA COTCS( 14) / -.140155139824298666666666666666666D-30   /
      DATA COTCS( 15) / -.551800436872533333333333333333333D-33   /
      DATA PI2REC / .011619772367581343075535053490057D0 /
      DATA FIRST /.TRUE./
!***FIRST EXECUTABLE STATEMENT  DCOT
!
      DCOT=DBLE(CPUMIN)
!
      IF (FIRST) THEN
         NTERMS = INITDS (COTCS, 15, 0.1*REAL(D1MACH(3)) )
         XMAX = 1.0D0/D1MACH(4)
         XSML = SQRT(3.0D0*D1MACH(3))
         XMIN = EXP (MAX(LOG(D1MACH(1)), -LOG(D1MACH(2))) + 0.01D0)
         SQEPS = SQRT(D1MACH(4))
      ENDIF
      FIRST = .FALSE.
!
      Y = ABS(X)
      IF (Y .LT. XMIN) THEN
        WRITE(ICOUT,1)
    1   FORMAT('***** ERORR FROM DCOT, ABS(X) IS ZERO OR SO SMALL ',   &
               'THAT DCOT OVERFLOWS.  ****')
        CALL DPWRST('XXX','BUG ')
        RETURN
      ENDIF
      IF (Y .GT. XMAX) THEN
        WRITE(ICOUT,2)
    2   FORMAT('***** ERORR FROM DCOT, NO PRECISION BECAUSE ABS(X) ',   &
               'IS SO BIG.  ****')
        CALL DPWRST('XXX','BUG ')
        RETURN
      ENDIF
!
! CAREFULLY COMPUTE Y * (2/PI) = (AINT(Y) + REM(Y)) * (.625 + PI2REC)
! = AINT(.625*Y) + REM(.625*Y) + Y*PI2REC  =  AINT(.625*Y) + Z
! = AINT(.625*Y) + AINT(Z) + REM(Z)
!
      AINTY = AINT (Y)
      YREM = Y - AINTY
      PRODBG = 0.625D0*AINTY
      AINTY = AINT (PRODBG)
      Y = (PRODBG-AINTY) + 0.625D0*YREM + PI2REC*Y
      AINTY2 = AINT (Y)
      AINTY = AINTY + AINTY2
      Y = Y - AINTY2
!
      IFN = INT(MOD (AINTY, 2.0D0))
      IF (IFN.EQ.1) Y = 1.0D0 - Y
!
      IF (ABS(X) .GT. 0.5D0 .AND. Y .LT. ABS(X)*SQEPS) THEN
        WRITE(ICOUT,3)
    3   FORMAT('***** WARNING FROM DCOT, ANSWER IS LESS THAN HALF ',   &
         'PRECISION BECAUSE ABS(X) IS TOO BIG OR X IS NEAR PI.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF (Y.GT.0.25D0) GO TO 20
      DCOT = 1.0D0/X
      IF (Y.GT.XSML) DCOT = (0.5D0 + DCSEVL (32.0D0*Y*Y-1.D0, COTCS,   &
        NTERMS)) / Y
      GO TO 40
!
 20   IF (Y.GT.0.5D0) GO TO 30
      DCOT = (0.5D0 + DCSEVL (8.D0*Y*Y-1.D0, COTCS, NTERMS))/(0.5D0*Y)
      DCOT = (DCOT*DCOT-1.D0)*0.5D0/DCOT
      GO TO 40
!
 30   DCOT = (0.5D0 + DCSEVL (2.D0*Y*Y-1.D0, COTCS, NTERMS))/(.25D0*Y)
      DCOT = (DCOT*DCOT-1.D0)*0.5D0/DCOT
      DCOT = (DCOT*DCOT-1.D0)*0.5D0/DCOT
!
 40   IF (X.NE.0.D0) DCOT = SIGN (DCOT, X)
      IF (IFN.EQ.1) DCOT = -DCOT
!
      RETURN
      END FUNCTION DCOT 
      DOUBLE PRECISION FUNCTION DCSEVL (X, CS, N)
!***BEGIN PROLOGUE  DCSEVL
!***PURPOSE  Evaluate a Chebyshev series.
!***LIBRARY   SLATEC (FNLIB)
!***CATEGORY  C3A2
!***TYPE      DOUBLE PRECISION (CSEVL-S, DCSEVL-D)
!***KEYWORDS  CHEBYSHEV SERIES, FNLIB, SPECIAL FUNCTIONS
!***AUTHOR  Fullerton, W., (LANL)
!***DESCRIPTION
!
!  Evaluate the N-term Chebyshev series CS at X.  Adapted from
!  a method presented in the paper by Broucke referenced below.
!
!       Input Arguments --
!  X    value at which the series is to be evaluated.
!  CS   array of N terms of a Chebyshev series.  In evaluating
!       CS, only half the first coefficient is summed.
!  N    number of terms in array CS.
!
!***REFERENCES  R. Broucke, Ten subroutines for the manipulation of
!                 Chebyshev series, Algorithm 446, Communications of
!                 the A.C.M. 16, (1973) pp. 254-256.
!               L. Fox and I. B. Parker, Chebyshev Polynomials in
!                 Numerical Analysis, Oxford University Press, 1968,
!                 page 56.
!***ROUTINES CALLED  D1MACH, XERMSG
!***REVISION HISTORY  (YYMMDD)
!   770401  DATE WRITTEN
!   890831  Modified array declarations.  (WRB)
!   890831  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!   900329  Prologued revised extensively and code rewritten to allow
!           X to be slightly outside interval (-1,+1).  (WRB)
!   920501  Reformatted the REFERENCES section.  (WRB)
!***END PROLOGUE  DCSEVL
      DOUBLE PRECISION B0, B1, B2, CS(*), ONEPL, TWOX, X
      LOGICAL FIRST
      SAVE FIRST, ONEPL
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DATA FIRST /.TRUE./
!***FIRST EXECUTABLE STATEMENT  DCSEVL
!CCCC IF (FIRST) ONEPL = 1.0D0 + D1MACH(4)
      ONEPL = 1.0D0 + D1MACH(4)
      FIRST = .FALSE.
      IF (N .LT. 1) THEN
        WRITE(ICOUT,11)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        DCSEVL = 0.D0
        RETURN
      ENDIF
   11 FORMAT('***** ERROR FROM DCSEVL.  THE NUMBER OF TERMS IS ')
   12 FORMAT('      LESS THAN OR EQUAL TO ZERO.                *****')
      IF (N .GT. 1000) THEN
        WRITE(ICOUT,21)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,22)
        CALL DPWRST('XXX','BUG ')
        DCSEVL = 0.D0
        RETURN
      ENDIF
   21 FORMAT('***** ERROR FROM DCSEVL.  THE NUMBER OF TERMS IS ')
   22 FORMAT('      GREATER THAN 1000.                         *****')
!CCCC IF (ABS(X) .GT. ONEPL) THEN
!CCCC   WRITE(ICOUT,31)
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC   WRITE(ICOUT,32)X
!CCCC   CALL DPWRST('XXX','BUG ')
!CCCC ENDIF
!CC31 FORMAT('***** WARNING FROM DCSEVL.  X IS OUTSIDE THE ')
!CC32 FORMAT('      INTERVAL (-1,+1).  IT HAS THE VALUE ',E15.7,'.')
!
      B1 = 0.0D0
      B2 = 0.0D0
      B0 = 0.0D0
      TWOX = 2.0D0*X
      DO 10 I = 1,N
         B2 = B1
         B1 = B0
         NI = N + 1 - I
         B0 = TWOX*B1 - B2 + CS(NI)
   10 CONTINUE
!
      DCSEVL = 0.5D0*(B0-B2)
!
      RETURN
      END FUNCTION DCSEVL 
      DOUBLE PRECISION FUNCTION DDOT(N,DX,INCX,DY,INCY)
!***BEGIN PROLOGUE  DDOT
!***DATE WRITTEN   791001   (YYMMDD)
!***REVISION DATE  820801   (YYMMDD)
!***CATEGORY NO.  D1A4
!***KEYWORDS  BLAS,DOUBLE PRECISION,INNER PRODUCT,LINEAR ALGEBRA,VECTOR
!***AUTHOR  LAWSON, C. L., (JPL)
!           HANSON, R. J., (SNLA)
!           KINCAID, D. R., (U. OF TEXAS)
!           KROGH, F. T., (JPL)
!***PURPOSE  D.P. inner product of d.p. vectors
!***DESCRIPTION
!
!                B L A S  Subprogram
!    Description of Parameters
!
!     --Input--
!        N  number of elements in input vector(s)
!       DX  double precision vector with N elements
!     INCX  storage spacing between elements of DX
!       DY  double precision vector with N elements
!     INCY  storage spacing between elements of DY
!
!     --Output--
!     DDOT  double precision dot product (zero if N .LE. 0)
!
!     Returns the dot product of double precision DX and DY.
!     DDOT = sum for I = 0 to N-1 of  DX(LX+I*INCX) * DY(LY+I*INCY)
!     where LX = 1 if INCX .GE. 0, else LX = (-INCX)*N, and LY is
!     defined in a similar way using INCY.
!***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,
!                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*,
!                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL
!                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323
!***ROUTINES CALLED  (NONE)
!***END PROLOGUE  DDOT
!
      DOUBLE PRECISION DX(*),DY(*)
!***FIRST EXECUTABLE STATEMENT  DDOT
      DDOT = 0.D0
      IF(N.LE.0)RETURN
!CCCC IF(INCX.EQ.INCY) IF(INCX-1) 5,20,60
      IF(INCX.EQ.INCY) THEN
        IF(INCX-1.LT.0)THEN
          GO TO 5
        ELSEIF(INCX-1.EQ.0)THEN
          GO TO 20
        ELSE
          GO TO 60
        ENDIF
      ENDIF
    5 CONTINUE
!
!         CODE FOR UNEQUAL OR NONPOSITIVE INCREMENTS.
!
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
         DDOT = DDOT + DX(IX)*DY(IY)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
!
!        CODE FOR BOTH INCREMENTS EQUAL TO 1.
!
!
!        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 5.
!
   20 M = MOD(N,5)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
         DDOT = DDOT + DX(I)*DY(I)
   30 CONTINUE
      IF( N .LT. 5 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
         DDOT = DDOT + DX(I)*DY(I) + DX(I+1)*DY(I+1) +   &
         DX(I + 2)*DY(I + 2) + DX(I + 3)*DY(I + 3) + DX(I + 4)*DY(I + 4)
   50 CONTINUE
      RETURN
!
!         CODE FOR POSITIVE EQUAL INCREMENTS .NE.1.
!
   60 CONTINUE
      NS = N*INCX
          DO 70 I=1,NS,INCX
          DDOT = DDOT + DX(I)*DY(I)
   70     CONTINUE
      RETURN
      END FUNCTION DDOT
      SUBROUTINE DECHE2(IX,IA,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE CONVERTS AN INTEGER IN THE
!              RANGE 0 - 65535 (2**16 - 1) TO A TWO CHARACTER
!              HEXADECIMAL NUMBER.
!
!              THIS IS A UTILITY ROUTINE USED BY SOME DEVICES
!              (E.G., POSTSCRIPT) TO CONVERT RGB COMPONENTS TO
!              HEXADECIMAL NUMBERS.
!     INPUT  ARGUMENTS--IX     = THE INTEGER TO BE CONVERTED.
!     OUTPUT ARGUMENTS--IA     = THE CHARACTER*2 STRING THAT WILL
!                                CONTAIN THE HEX NUMBER.
!     OUTPUT--THE STRING CONTAINING THE NUMBER IN HEXADECIMAL FORMAT.
!     RESTRICTIONS--THE MAXIMUM VALUE OF IX IS 2**16-1.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--INTEGER.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2008.3
!     ORIGINAL VERSION--MARCH     2008.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      CHARACTER*2 IA
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DECH'
      ISUBN2='E2  '
      IERROR='NO'
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DECHE2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IX
   53   FORMAT('IX = ',I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      IMAX=(2**16) - 1
      IA=' '
!
      IF(IX.GT.IMAX)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN DECHE2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE INPUT DECIMAL NUMBER, ',I10,' IS GREATER')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)IMAX
  113   FORMAT('      THAN THE ALLOWED MAXIMUM ',I8)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
!               ******************************
!               **  STEP 2--                **
!               **  PERFORM THE CONVERSION. **
!               ******************************
!
      IVAL=IX/16
      IREM=IX - (16*IVAL)
!
      IF(IREM.LE.9)THEN
        WRITE(IA(2:2),'(I1)')IREM
      ELSEIF(IREM.EQ.10)THEN
        IA(2:2)='A'
      ELSEIF(IREM.EQ.11)THEN
        IA(2:2)='B'
      ELSEIF(IREM.EQ.12)THEN
        IA(2:2)='C'
      ELSEIF(IREM.EQ.13)THEN
        IA(2:2)='D'
      ELSEIF(IREM.EQ.14)THEN
        IA(2:2)='E'
      ELSEIF(IREM.EQ.15)THEN
        IA(2:2)='F'
      ENDIF
!
      IF(IVAL.LE.9)THEN
        WRITE(IA(1:1),'(I1)')IVAL
      ELSEIF(IVAL.EQ.10)THEN
        IA(1:1)='A'
      ELSEIF(IVAL.EQ.11)THEN
        IA(1:1)='B'
      ELSEIF(IVAL.EQ.12)THEN
        IA(1:1)='C'
      ELSEIF(IVAL.EQ.13)THEN
        IA(1:1)='D'
      ELSEIF(IVAL.EQ.14)THEN
        IA(1:1)='E'
      ELSEIF(IVAL.EQ.15)THEN
        IA(1:1)='F'
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF BINHE2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)IA
 9015   FORMAT('IA = ',A2)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DECHE2
      SUBROUTINE DECRAT(X,N,IWRITE,XQNUM,XQDEN,   &
                        RATIO,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--IF XQNUM = 0.9 AND XQDEN = 0.4, THIS STATISTIC
!              COMPUTES THE RATIO OF THE TOP 10% OF THE DATA
!              TO THE BOTTOM 40% OF THE DATA.
!
!              THIS HAS BEEN PROPOSED AS AN ALTERNATIVE MEASURE OF
!              "INCOME EQUALITY".  SPECIFICALLY, THE PALMA SPECIFICATION
!              USES QNUM = 0.9 AND QDEN = 0.4).  THAT IS, THIS IS THE
!              RATIO OF THE INCOME OF THE BOTTOM 40% RELATIVE TO THE
!              TOP 10%.
!
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                OBSERVATIONS FOR WHICH THE PERCENTAGE
!                                RANKS WILL BE COMPUTED.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!                     --XQNUM  = SCALAR THAT SPECIFIES QUANTILE FOR THE
!                                NUMERATOR
!                     --XQDEN  = SCALAR THAT SPECIFIES QUANTILE FOR THE
!                                DENOMINATOR
!     OUTPUT ARGUMENTS--RATIO  = THE SINGLE PRECISION SCALAR WHERE THE
!                                INTERDECILE RATIO IS SAVED
!     OUTPUT--THE SINGLE PRECISION SCALAR RATIO CONTAINING THE
!             INTERDECILE RATIO.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     OTHER DATAPAC   SUBROUTINES NEEDED--QUANT, SORT.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--COBHAM AND SUMNER (2014), "IS INEQUALITY ALL ABOUT THE
!                 TAILS", SIGNIFICANCE, PP. 10-13.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATION INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2015.2
!     ORIGINAL VERSION--FEBRUARY  2015.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
!
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
      DOUBLE PRECISION DRATIO
      DOUBLE PRECISION DDEN
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DECR'
      ISUBN2='AT  '
!
      IERROR='NO'
      RATIO=CPUMIN
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'CRAT')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DECRAT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N,XQNUM,XQDEN
   52   FORMAT('IBUGA3,ISUBRO,N,XQNUM,XQDEN = ',2(A4,2X),I8,2G15.7)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               *******************************************
!               **  COMPUTE THE INTERDECILE RATIO        **
!               *******************************************
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      AN=N
!
      IF(N.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN DECILE RATIO--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      THE NUMBER OF OBSERVATIONS IS LESS THAN ONE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,118)N
  118   FORMAT('      THE NUMBER OF OBSERVATIONS IS ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(XQNUM.LT.0.0 .OR. XQNUM.GT.1.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,123)
  123   FORMAT('      THE SPECIFIED QUANTILE FOR THE NUMERATOR IS ',   &
               'OUTSIDE THE (0,1) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,125)XQNUM
  125   FORMAT('      THE VALUE OF THE NUMERATOR QUANTILE = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(XQDEN.LT.0.0 .OR. XQDEN.GT.1.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,133)
  133   FORMAT('      THE SPECIFIED QUANTILE FOR THE DENOMINATOR IS ',   &
               'OUTSIDE THE (0,1) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,135)XQDEN
  135   FORMAT('      THE VALUE OF THE DENOMINATOR QUANTILE = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(N.EQ.1)THEN
        RATIO=1.0
        GO TO 8000
      ENDIF
!
      IF(XQDEN.GT.XQNUM)THEN
        AVAL=XQNUM
        XQNUM=XQDEN
        XQDEN=AVAL
      ENDIF
!
!               ***************************************************
!               **  STEP 2--                                     **
!               **  SORT THE DATA.                               **
!               ***************************************************
!
      CALL SORT(X,N,X)
!
      IF(X(1).LT.0.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,203)
  203   FORMAT('      THE RESPONSE VARIABLE CONTAINS NEGATIVE ',   &
               'NUMBERS AND THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,205)
  205   FORMAT('      DECILE RATIO IS NOT CURRENTLY SUPPORTED FOR ',   &
               'NEGATIVE NUMBERS.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               ***************************************************
!               **  STEP 3--                                     **
!               **  COMPUTE THE QUANTILES FOR THE NUMERATOR AND  **
!               **  DENOMINATOR.                                 **
!               ***************************************************
!
      NI=0
      NIP1=0
      ANI=0.0
      A2NI=0.0
      REM=0.0
      AN=REAL(N)
      P=XQDEN
      ANI=P*(AN+1.0)
      NI=INT(ANI+0.1)
      A2NI=REAL(NI)
      REM=ANI-A2NI
      NIP1=NI+1
      IF(NI.LE.1)NI=1
      IF(NI.GE.N)NI=N
      IF(NIP1.LE.1)NIP1=1
      IF(NIP1.GE.N)NIP1=N
      DSUM1=0.0D0
      DO 310 I=1,NI
        DSUM1=DSUM1 + DBLE(X(I))
  310 CONTINUE
      DSUM1=DSUM1 + DBLE(REM*X(NIP1))
      NIDEN=NI
      NIP1DN=NIP1
      REMDEN=REM
!
      NI=0
      NIP1=0
      ANI=0.0
      A2NI=0.0
      REM=0.0
      P=XQNUM
      ANI=P*(AN+1.0)
      NI=INT(ANI+0.1)
      A2NI=REAL(NI)
      REM=ANI-A2NI
      NIP1=NI+1
      IF(NI.LE.1)NI=1
      IF(NI.GE.N)NI=N
      IF(NIP1.LE.1)NIP1=1
      IF(NIP1.GE.N)NIP1=N
      DSUM2=0.0D0
      DO 320 I=NI,N
        DSUM2=DSUM2 + DBLE(X(I))
  320 CONTINUE
      DSUM2=DSUM2 - DBLE(REM*X(NIP1))
      NINUM=NI
      NIP1NU=NIP1
      REMNUM=REM
!
      DRATIO=DSUM2/DSUM1
      RATIO=REAL(DRATIO)
!
!               ******************************
!               **  STEP 4--                **
!               **  WRITE OUT A FEW LINES   **
!               **  OF SUMMARY INFORMATION  **
!               **  ABOUT THE CODING.       **
!               ******************************
!
 8000 CONTINUE
!
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,912)RATIO
  912   FORMAT('THE INTERDECILE RATIO IS ',G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'CRAT')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DECRAT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)NINUM,NIP1NU,REMNUM
 9012   FORMAT('NINUM,NIP1NU,REMNUM = ',2I8,G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NIDEN,NIP1DN,REMDEN
 9013   FORMAT('NIDEN,NIP1DN,REMDEN = ',2I8,G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)DSUM1,DSUM2,DDEN,DRATIO
 9015   FORMAT('DSUM1,DSUM2,DDEN,DRATIO = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DECRAT
      SUBROUTINE DEMFIT(Y,X,WGT,N,YSTAR,XSTAR,DELTA,MAXNXT,          &
                        IWFLAG,IDFIWI,ICASAN,PDFITH,                 &
                        YTEMP,XTEMP,WTEMP,ALPHAT,BETAT,DIFFT,        &
                        YTEMPR,XTEMPR,                               &
                        PRED,PREDSE,OPTRES,PREDSAVE,                 &
                        ALPHA,BETA,SDALPHA,SDBETA,ALPHAJN,BETAJN,    &
                        SXX,SYY,SXY,RESSD,                           &
                        SDDIFF,DIFFJN,XBAR,YBAR,                     &
                        IDFISE,ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--CARRY OUT A DEMINGS REGRESSION.  NOTE THAT THIS IS A
!              SPECIAL CASE OF ERRORS-IN-VARIABLES REGRESSION FOR
!              THE (Y,X) CASE.
!     REFERENCE--Adcock (1878), "A problem in least square," The Analyst,
!                Vol. 5, No. 2, pp. 53-54.
!              --Kummel (1879), "Reduction of observation equations which
!                contain more than one observed quantity," The Analyst,
!                Vol. 6, No. 4, pp. 97-105.
!              --Deming (1943), "Statistical Adjustment of Data," John
!                Wiley and Sons.
!              --Linnet (1990), "Estimation of the linear relationship
!                between the measurements of two methods with proportional
!                errors," Statistics in Medicine, Vol. 9, No. 12,
!                pp. 1463-1473.
!              --Linnet (1993), "Evaluation of Regression Procedures for
!                Method Comparison Studies," Clinical Chemistry, 39 (3),
!                pp. 424-432.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2025/02
!     ORIGINAL VERSION--FEBRUARY   2025
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IWFLAG
      CHARACTER*4 ICASAN
      CHARACTER*4 IDFIWI
      CHARACTER*4 IDFISE
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 IOP
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION WGT(*)
      DIMENSION YSTAR(*)
      DIMENSION XSTAR(*)
      DIMENSION YTEMP(*)
      DIMENSION XTEMP(*)
      DIMENSION YTEMPR(*)
      DIMENSION XTEMPR(*)
      DIMENSION WTEMP(*)
      DIMENSION ALPHAT(*)
      DIMENSION BETAT(*)
      DIMENSION DIFFT(*)
      DIMENSION PRED(*)
      DIMENSION PREDSE(*)
      DIMENSION OPTRES(*)
      DIMENSION PREDSAVE(N,N)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DEMF'
      ISUBN2='IT  '
      IERROR='NO'
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'MFIT')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DEMFIT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ISUBRO,IBUGA3,IERROR,IDFISE,IWFLAG,N,DELTA
   52   FORMAT('ISUBRO,IBUGA3,IERROR,IDFISE,IWFLAG,N,DELTA = ',     &
               5(A4,2X),I8,G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)IDFIWI,ICASAN,PDFITH
   54   FORMAT('IDFIWI,ICASAN,PDFITH = ',2(A4,2X),G15.7)
        CALL DPWRST('XXX','BUG ')
        IF(N.GE.1)THEN
          DO 61 I=1,N
            WRITE(ICOUT,62)I,Y(I),X(I)
   62       FORMAT('I,Y(I),X(I) = ',I8,2G15.7)
            CALL DPWRST('XXX','BUG ')
   61     CONTINUE
        ENDIF
      ENDIF
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      IF(N.LT.3)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN DEMING REGRESSION (DEMFIT)--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE INPUT SAMPLE SIZE MUST BE AT LEAST 3.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,116)N
  116   FORMAT('      THE SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               ***********************************************
!               **  STEP 11--                                **
!               **  CARRY OUT A DEMINGS REGRESSION           **
!               ***********************************************
!
      YTEMP(1:N)=0.0
      XTEMP(1:N)=0.0
      WTEMP(1:N)=0.0
      ALPHAT(1:N)=0.0
      BETAT(1:N)=0.0
      DIFFT(1:N)=0.0
      PRED(1:N)=0.0
      PREDSE(1:N)=0.0
      PREDSE(1:N)=0.0
      OPTRES(1:N)=0.0
      PREDSAVE=0.0
!
      IFLAGD=0
      IF(DELTA.LE.0.0)IFLAGD=1
!
      AN=REAL(N)
      ANM1=REAL(N-1)
      ITER=0
      MAXITER=20
      ALPHASV=CPUMIN
      BETASV=CPUMIN
!
 100  CONTINUE
!
!     NOTE: FOR ITERATIVE REWEIGHTING, USE NEW PREDICTED VALUES
!           TO COMPUTE THE WEIGHTS, BUT USE ORIGINAL DATA FOR THE
!           PARAMETER ESTIMATES.
!
      IF(IWFLAG.EQ.'ON')THEN
        IF(ITER.EQ.0)THEN
          XTEMP(1:N)=X(1:N)
          YTEMP(1:N)=Y(1:N)
        ENDIF
        SUMWT=0.0
        IF(IDFIWI.EQ.'ON' .AND. ITER.EQ.0)THEN
          WGT(1:N)=1.0
          SUMWT=REAL(N)
        ELSEIF(IDFIWI.EQ.'ON' .AND. ITER.GT.0)THEN
          DO II=1,N
!            TERM1=(XTEMP(II) + YTEMP(II))/2.0
!            WGT(II)=1.0/TERM1**2
             TERM1=XTEMP(II) + DELTA*YTEMP(II)
             TERM2=1.0 + DELTA
             TERM3=(TERM1/TERM2)**2
             WGT(II)=1.0/TERM3
             SUMWT=SUMWT + WGT(II)
          ENDDO
          XTEMP(1:N)=X(1:N)
          YTEMP(1:N)=Y(1:N)
        ELSE
          DO II=1,N
             TERM1=XTEMP(II) + DELTA*YTEMP(II)
             TERM2=1.0 + DELTA
             TERM3=(TERM1/TERM2)**2
             WGT(II)=1.0/TERM3
             SUMWT=SUMWT + WGT(II)
          ENDDO
        ENDIF
!
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'MFIT')THEN
          WRITE(ICOUT,211)ITER
  211     FORMAT('WEIGHTED CASE: ITERATION = ',I5)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,212)ALPHASV,BETASV
  212     FORMAT('ALPHASV,BETASV = ',2G15.7)
          CALL DPWRST('XXX','BUG ')
          DO II=1,N
             WRITE(ICOUT,213)II,XTEMP(II),YTEMP(II),WGT(II)
  213        FORMAT('II,XTEMP(II),YTEMP(II),WGT(II) = ',I8,3G15.7)
             CALL DPWRST('XXX','BUG ')
          ENDDO
        ENDIF
!
      ELSE
        WGT(1:N)=1.0
        SUMWT=REAL(N)
        XTEMP(1:N)=X(1:N)
        YTEMP(1:N)=Y(1:N)
!
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'MFIT')THEN
          WRITE(ICOUT,221)ITER
  221     FORMAT('UNWEIGHTED CASE: ITERATION = ',I5)
          CALL DPWRST('XXX','BUG ')
          DO II=1,N
             WRITE(ICOUT,213)II,XTEMP(II),YTEMP(II),WGT(II)
             CALL DPWRST('XXX','BUG ')
          ENDDO
        ENDIF
!
      ENDIF
!
      SUMX=SUM(WGT(1:N)*XTEMP(1:N))
      SUMY=SUM(WGT(1:N)*YTEMP(1:N))
      XBAR=SUMX/SUMWT
      YBAR=SUMY/SUMWT
      XBARSV=XBAR
      YBARSV=YBAR
!
      SXX=SUM(WGT(1:N)*(XTEMP(1:N)-XBAR)**2)
      SYY=SUM(WGT(1:N)*(YTEMP(1:N)-YBAR)**2)
      SXY=SUM(WGT(1:N)*(YTEMP(1:N)-YBAR)*(XTEMP(1:N)-XBAR))
!
!     IF DELTA IS NOT SPECIFIED, DEFAULT TO SXX/SYY
!
      IF(IFLAGD.EQ.1)THEN
        DELTA=SXX/SYY
      ENDIF
!
      BETA=0.0
      IF(SXY.NE.0.0)THEN
        TERM1=DELTA*SYY - SXX
        TERM2=(SXX - DELTA*SYY)**2 + 4.0*DELTA*SXY**2
        BETA=(TERM1 + SQRT(TERM2))/(2.0*DELTA*SXY)
      ELSE
        IERROR='YES'
        GO TO 9000
      ENDIF
      ALPHA=YBAR-BETA*XBAR
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'MFIT')THEN
        WRITE(ICOUT,1141)XBAR,YBAR,SXX,SYY,SXY
 1141   FORMAT('FROM DEMFIT: XBAR,YBAR,SXX,SYY,SXY = ',5G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1143)ALPHA,BETA
 1143   FORMAT('ALPHA,BETA = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
        DO II=1,N
           WRITE(ICOUT,1147)II,XTEMP(II),YTEMP(II),WGT(II)
 1147      FORMAT('II,XTEMP(II),YTEMP(II),WGT(II) = ',I8,3G15.7)
           CALL DPWRST('XXX','BUG ')
        ENDDO
      ENDIF
!
      IF(IDFIWI.EQ.'ON')THEN
        AVAL1=ABS(ALPHA-ALPHASV)
        AVAL2=ABS(BETA-BETASV)
        IF(AVAL1.LE.PDFITH .AND. AVAL2.LE.PDFITH)THEN
          GO TO 199
        ELSE
          ITER=ITER+1
          IF(ITER.GT.MAXITER)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,161)
  161       FORMAT('***** WARNING IN DEMING REGRESSION (DEMFIT)--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,162)MAXITER
  162       FORMAT('      MAXIMUM OF ',I3,' ITERATIONS REACHED ', &
                   'WITHOUT CONVERGENCE.')
            CALL DPWRST('XXX','BUG ')
            GO TO 199
          ENDIF
          ALPHASV=ALPHA
          BETASV=BETA
          DO I=1,N
             EI=YTEMP(I) - (ALPHA + BETA*XTEMP(I))
             ANUM=DELTA*BETA*EI
             DEN=DELTA*BETA**2 + 1.0
             XTEMP(I)=XTEMP(I) + (ANUM/DEN)
             YTEMP(I)=YTEMP(I) - EI/(DELTA*BETA**2 + 1.0)
          ENDDO
          IF(ICASAN.EQ.'WRDP')THEN
            CALL DEMFITR(YTEMP,YTEMPR,NS1,                    &
                         XTEMP,XTEMPR,NS2,                    &
                         ALPHAT,BETAT,YSTAR,XSTAR,DIFFT,      &
                         DELTA,NDIST,                         &
                         ISUBRO,IBUGA3,IERROR)
          ENDIF
          GO TO 100
        ENDIF
      ENDIF
!
  199 CONTINUE
!
      IOP='OPEN'
      IFLG11=1
      IFLG21=1
      IFLG31=1
      IFLAG4=0
      IF(IDFISE.EQ.'ANAL')IFLAG4=1
      IFLAG5=5
      CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,       &
                  IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      WRITE(IOUNI1,1701)
 1701 FORMAT(81X,'OPTIMIZED')
      WRITE(IOUNI1,1703)
 1703 FORMAT(10X,'X-HAT',10X,'Y-HAT',3X,'RAW RESIDUAL',5X,'X RESIDUAL',  &
             5X,'Y RESIDUAL',7X,'RESIDUAL',2X,'PREDICTED Y|X')
!
      PRED(1:N)=ALPHA + BETA*X(1:N)
      RESVAR=0.0
      DO I=1,N
        EI=Y(I) - (ALPHA + BETA*X(I))
        RESVAR=RESVAR + EI**2
        ANUM=DELTA*BETA*EI
        DEN=DELTA*BETA**2 + 1.0
        XSTAR(I)=X(I) + (ANUM/DEN)
        YSTAR(I)=Y(I) - EI/(DELTA*BETA**2 + 1.0)
        XEI=X(I) - XSTAR(I)
        YEI=Y(I) - YSTAR(I)
        OPTRES(I)=SQRT(XEI**2 + DELTA*YEI**2)
        IF(EI.LT.0.0)OPTRES(I)=-OPTRES(I)
        WRITE(IOUNI1,'(7E15.7)')XSTAR(I),YSTAR(I),EI,XEI,YEI,   &
                                OPTRES(I),PRED(I)
      ENDDO
      RESVAR=RESVAR/REAL(N-2)
      RESSD=SQRT(RESVAR)
!
      WRITE(IOUNI5,1706)
 1706 FORMAT('WEIGHTS')
      DO II=1,N
         WRITE(IOUNI5,'(E15.7)')WGT(II)
      ENDDO
!
!               ****************************************
!               **  STEP 13--                         **
!               **  COMPUTE SD OF PARAMETER ESTIMATES **
!               ****************************************
!
      SDALPHA=0.0
      SDBETA=0.0
      ALPHAJN=0.0
      BETAJN=0.0
!
!     NOTE THAT ANALYTIC STANDARD ERRORS ARE ONLY SUPPORTED FOR THE
!     UNWEIGHTED CASE
!
      IF(IDFISE.EQ.'ANAL' .AND. IWFLAG.EQ.'OFF')THEN
        ADF=REAL(N-2)
        AVAL=0.95
        CALL TPPF(AVAL,ADF,TPPF90)
        AVAL=0.975
        CALL TPPF(AVAL,ADF,TPPF95)
        AVAL=0.995
        CALL TPPF(AVAL,ADF,TPPF99)
!
        VARX=SXX/ANM1
        VARY=SYY/ANM1
!       DENOM=VARX*(1.0 + DELTA*(VARX/VARY))
        DENOM=SXX*(1.0 + DELTA*(SXX/SYY))
        SDBETA=SQRT(RESVAR/DENOM)
        SDALPHA=SQRT(RESVAR/(AN*VARX))
        SUMT=SUM((X(1:N)-XBAR)**2)
!
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'MFIT')THEN
          WRITE(ICOUT,1151)SXX,SYY,VARX,VARY,DENOM
 1151     FORMAT('FROM DEMFIT: SXX,SYY,VARX,VARY,DENOM = ',5G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1153)SDBETA,SDALPHA
 1153     FORMAT('SDBETA,SDALPHA = ',2G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        WRITE(IOUNI4,1311)
        WRITE(IOUNI4,1313)
!
        DO II=1,N
           TERM1=(X(II) - XBAR)**2
           TERM2=RESVAR*((1.0/REAL(N)) + (TERM1/SXX))
           PREDSE(II)=SQRT(TERM2)
           AVAL1=PRED(II) - TPPF90*PREDSE(II)
           AVAL2=PRED(II) + TPPF90*PREDSE(II)
           AVAL3=PRED(II) - TPPF95*PREDSE(II)
           AVAL4=PRED(II) + TPPF95*PREDSE(II)
           AVAL5=PRED(II) - TPPF99*PREDSE(II)
           AVAL6=PRED(II) + TPPF99*PREDSE(II)
           WRITE(IOUNI4,'(9E15.7)')X(II),PRED(II),PREDSE(II),AVAL1,AVAL2,    &
                                   AVAL3,AVAL4,AVAL5,AVAL6
        ENDDO
!
        IOP='CLOS'
        CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,   &
                    IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,       &
                    IBUGA3,ISUBRO,IERROR)
        GO TO 9000
      ENDIF
!
!     COMPUTE USING JACKNIFE PROCEDURE
!
!     WRITE ALPHA, BETA ESTIMATES TO "dpst2f.dat"
!
      WRITE(IOUNI2,1301)
 1301 FORMAT(7X,'ADJUSTED',7X,'ADJUSTED',5X,'UNADJUSTED',5X,'UNADJUSTED')
      WRITE(IOUNI2,1302)
 1302 FORMAT(10X,'ALPHA',11X,'BETA',10X,'ALPHA',11X,'BETA')
      WRITE(IOUNI3,1306)
 1306 FORMAT(20X,'      PREDICTED')
      WRITE(IOUNI3,1307)
 1307 FORMAT(7X,'ROW',' REPLICATE',10X,'VALUE')
!
      DO IROW=1,N
         ICNT=0
         SUMWT=0.0
         DO II=1,N
            IF(II.NE.IROW)THEN
              ICNT=ICNT+1
              XTEMP(ICNT)=X(II)
              YTEMP(ICNT)=Y(II)
              WTEMP(ICNT)=WGT(II)
              SUMWT=SUMWT + WTEMP(ICNT)
            ENDIF
         ENDDO
         NM1=N-1
         ANM1=REAL(NM1)
!
         IF(IWFLAG.EQ.'ON')THEN
!          SUMWTNEW=0.0
!          DO II=1,NM1
!             TERM1=XTEMP(II) + DELTA*YTEMP(II)
!             TERM2=1.0 + DELTA
!             TERM3=(TERM1/TERM2)**2
!             WTEMP(II)=1.0/TERM3
!             WTEMP(II)=1.0/TERM3
!             SUMWT=SUMWT + WTEMP(II)
!          ENDDO
         ELSE
           WTEMP(1:NM1)=1.0
           SUMWT=REAL(NM1)
         ENDIF
!
         SUMX=SUM(WTEMP(1:NM1)*XTEMP(1:NM1))
         SUMY=SUM(WTEMP(1:NM1)*YTEMP(1:NM1))
         XBARNEW=SUMX/SUMWT
         YBARNEW=SUMY/SUMWT
         DIFFT(IROW)=XBARNEW - YBARNEW
!
         SXXNEW=SUM(WTEMP(1:NM1)*(XTEMP(1:NM1)-XBARNEW)**2)
         SYYNEW=SUM(WTEMP(1:NM1)*(YTEMP(1:NM1)-YBARNEW)**2)
         SXYNEW=SUM(WTEMP(1:NM1)*(YTEMP(1:NM1)-YBARNEW)*(XTEMP(1:NM1)-XBARNEW))
!
         BETAT(IROW)=0.0
         IF(SXYNEW.NE.0.0)THEN
           TERM1=DELTA*SYYNEW - SXXNEW
           TERM2=(SXXNEW - DELTA*SYYNEW)**2 + 4.0*DELTA*SXYNEW**2
           BETAT(IROW)=(TERM1 + SQRT(TERM2))/(2.0*DELTA*SXYNEW)
         ELSE
           IERROR='YES'
           GO TO 9000
         ENDIF
         ALPHAT(IROW)=YBARNEW-BETAT(IROW)*XBARNEW
         B0T=ALPHAT(IROW)
         B1T=BETAT(IROW)
!
         IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'MFIT')THEN
           WRITE(ICOUT,1161)IROW,ICNT,XBARNEW,YBARNEW,SXXNEW,SYYNEW,SXYNEW
 1161      FORMAT('1161: IROW,ICNT,XBARNEW,YBARNEW,SXXNEW,SYYNEW,SXYNEW = ',   &
                  2I8,5G15.7)
           CALL DPWRST('XXX','BUG ')
           WRITE(ICOUT,1163)ALPHAT(IROW),BETAT(IROW)
 1163      FORMAT('ALPHAT(IROW),BETAT(IROW) = ',2G15.7)
           CALL DPWRST('XXX','BUG ')
         ENDIF
!
         BETAT(IROW)=AN*BETA - ANM1*BETAT(IROW)
         ALPHAT(IROW)=REAL(N)*ALPHA - REAL(N-1)*ALPHAT(IROW)
         DIFFT(IROW)=REAL(N)*(YBARSV-XBARSV) - REAL(N-1)*DIFFT(IROW)
!
         DO JJ=1,N
            PREDSAVE(IROW,JJ)=ALPHAT(IROW) + BETAT(IROW)*X(JJ)
            WRITE(IOUNI3,'(2F10.0,E15.7)')REAL(IROW),REAL(JJ),PREDSAVE(IROW,JJ)
         ENDDO
!
         IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'MFIT')THEN
           WRITE(ICOUT,1167)ALPHAT(IROW),BETAT(IROW)
 1167      FORMAT('AFTER ADJUSTMENT: ALPHAT(IROW),BETAT(IROW) = ',2G15.7)
           CALL DPWRST('XXX','BUG ')
         ENDIF
!
         WRITE(IOUNI2,'(4E15.7)')ALPHAT(IROW),BETAT(IROW),B0T,B1T
!
      ENDDO
!
      ALPHAJN=SUM(ALPHAT(1:N))/AN
      BETAJN=SUM(BETAT(1:N))/AN
      DIFFJN=SUM(DIFFT(1:N))/AN
!
      SUMX=SUM((ALPHAT(1:N) - ALPHAJN)**2)
      SUMY=SUM((BETAT(1:N) - BETAJN)**2)
      SUMD=SUM((DIFFT(1:N) - DIFFJN)**2)
!
      VARALPHA=SUMX/ANM1
      VARBETA=SUMY/ANM1
      VARDIFF=SUMD/ANM1
      SDALPHA=SQRT(VARALPHA/AN)
      SDBETA=SQRT(VARBETA/AN)
      SDDIFF=SQRT(VARDIFF/AN)
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'MFIT')THEN
        WRITE(ICOUT,1181)ALPHAJN,BETAJN,SDALPHA,SDBETA
 1181   FORMAT('ALPHAJN,BETAJN,SDALPHA,SDBETA = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IOP='CLOS'
      CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,       &
                  IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
!     COMPUTE STANDARD ERRORS OF PREDICTED VALUES
!
      IF(N**2.GT.5*MAXNXT)GO TO 8000
      IOP='OPEN'
      IFLG11=0
      IFLG21=0
      IFLG31=0
      IFLAG4=1
      IFLAG5=0
      CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,       &
                  IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
      WRITE(IOUNI4,1311)
 1311 FORMAT(15X,6X,'PREDICTED',6X,'PREDICTED',3(10X,'LOWER',10X,'UPPER'))
      WRITE(IOUNI4,1313)
 1313 FORMAT(11X,'X(I)',10X,'VALUE',' STANDARD ERROR',2(9X,'90% CI'),      &
             2(9X,'95% CI'),2(9X,'99% CI'))
!
      ANM1=REAL(N-1)
      NMAX=N*N
      ADF=REAL(N-2)
      AVAL=0.95
      CALL TPPF(AVAL,ADF,TPPF90)
      AVAL=0.975
      CALL TPPF(AVAL,ADF,TPPF95)
      AVAL=0.995
      CALL TPPF(AVAL,ADF,TPPF99)
!
      DO ICOL=1,N
         XTEMP(1:N)=PREDSAVE(1:N,ICOL)
!
         IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'MFIT')THEN
           WRITE(ICOUT,1750)ICOL,AN,ANM1
  1750     FORMAT('UNADJUSTED PREDICTED VALUES: ICOL,AN,ANM1 = ',I10,2F10.1)
           CALL DPWRST('XXX','BUG ')
           DO II=1,N
             WRITE(ICOUT,1751)II,XTEMP(II)
  1751       FORMAT('II,XTEMP(II) = ',I10,G15.7)
             CALL DPWRST('XXX','BUG ')
           ENDDO
         ENDIF
!
!        THE FOLLOWING GENERATES BAD RESULTS, SKIP FOR NOW
!        DO II=1,N
!           XTEMP(II)=AN*PRED(ICOL) - ANM1*XTEMP(II)
!        ENDDO
!
!        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'MFIT')THEN
!          WRITE(ICOUT,1756)ICOL
! 1756     FORMAT('ADJUSTED PREDICTED VALUES FOR ORDER STATISITC ',I10)
!          CALL DPWRST('XXX','BUG ')
!          DO II=1,N
!            WRITE(ICOUT,1758)II,XTEMP(II)
! 1758       FORMAT('II,XTEMP(II) = ',I10,G15.7)
!            CALL DPWRST('XXX','BUG ')
!          ENDDO
!        ENDIF
!
         XJACK=SUM(XTEMP(1:N))/AN
         SUMX=SUM((XTEMP(1:N) - XJACK)**2)
         PREDVAR=SUMX/ANM1
         PREDSE(ICOL)=SQRT(PREDVAR/AN)
!
         AVAL1=PRED(ICOL) - TPPF90*PREDSE(ICOL)
         AVAL2=PRED(ICOL) + TPPF90*PREDSE(ICOL)
         AVAL3=PRED(ICOL) - TPPF95*PREDSE(ICOL)
         AVAL4=PRED(ICOL) + TPPF95*PREDSE(ICOL)
         AVAL5=PRED(ICOL) - TPPF99*PREDSE(ICOL)
         AVAL6=PRED(ICOL) + TPPF99*PREDSE(ICOL)
         WRITE(IOUNI4,'(9E15.7)')X(ICOL),PRED(ICOL),PREDSE(ICOL),AVAL1,AVAL2,    &
                                 AVAL3,AVAL4,AVAL5,AVAL6
!
         IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'MFIT')THEN
           WRITE(ICOUT,1766)XJACK,SUMX,PREDVAR,PREDSE(ICOL)
  1766     FORMAT('XJACK,SUMX,PREDVAR,PREDSE(ICOL) = ',4G15.7)
           CALL DPWRST('XXX','BUG ')
         ENDIF
!
      ENDDO
!
      IOP='CLOS'
      CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,   &
                  IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,       &
                  IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
!
 8000 CONTINUE
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8011)
 8011   FORMAT('DEMING REGRESSION:')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8012)DELTA
 8012   FORMAT('RATIO OF ERROR VARIANCES:         ',G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8013)ALPHA
 8013   FORMAT('ESTIMATE OF INTERCEPT (ALPHA):    ',G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8015)BETA
 8015   FORMAT('ESTIMATE OF SLOPE (BETA):         ',G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8017)SDALPHA
 8017   FORMAT('SD OF INTERCEPT:                  ',G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8019)SDBETA
 8019   FORMAT('SD OF SLOPE:                      ',G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'MFIT')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DEMFIT--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DEMFIT
      SUBROUTINE DEMFITR(Y,TAGY,N1,X,TAGX,N2,                          &
                         TAGYDIST,TAGXDIST,YMEAN,XMEAN,TEMP1,          &
                         DELTA,NDIST,                                  &
                         ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS IS A UTILITY ROUTINE FOR DEMINGS REGRESSION.  IF
!              THERE IS REPLICATION IN THE DATA, THIS ROUTINE RETURNS
!              AN ESTIMATE OF THE VARIANCE RATIO (DELTA) AND RETURNS THE
!              MEAN VALUES FOR EACH GROUP IN YMEAN AND XMEAN.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2025/03
!     ORIGINAL VERSION--MARCH      2025
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IWRITE
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION TAGY(*)
      DIMENSION TAGX(*)
      DIMENSION TAGYDIST(*)
      DIMENSION TAGXDIST(*)
      DIMENSION YMEAN(*)
      DIMENSION XMEAN(*)
      DIMENSION TEMP1(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DEMF'
      ISUBN2='ITR '
      IWRITE='OFF'
      IERROR='NO'
      XVAR=0.0
      YVAR=0.0
      DELTA=CPUMIN
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FITR')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DEMFITR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ISUBRO,IBUGA3,IERROR,N1,N2
   52   FORMAT('ISUBRO,IBUGA3,IERROR,N1,N2 = ',3(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        IF(N1.GE.1)THEN
          DO I=1,N
            WRITE(ICOUT,62)I,Y(I),TAGY(I)
   62       FORMAT('I,Y(I),TAGY(I) = ',I8,2G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDDO
        ENDIF
        IF(N2.GE.1)THEN
          DO I=1,N2
            WRITE(ICOUT,67)I,X(I),TAGX(I)
   67       FORMAT('I,X(I),TAGX(I) = ',I8,2G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDDO
        ENDIF
      ENDIF
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FITR')     &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(N1.LT.3)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN DEMING REGRESSION (DEMFITR)--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE SAMPLE SIZE MUST BE AT LEAST 3.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,116)N1
  116   FORMAT('      THE SAMPLE SIZE FOR THE FIRST MEASUREMENT ',     &
               'PROCESS (Y) = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ELSEIF(N2.LT.3)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,118)N2
  118   FORMAT('      THE SAMPLE SIZE FOR THE SECOND MEASUREMENT ',    &
               'PROCESS (X) = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               ***********************************************
!               **  STEP 2--                                 **
!               **  EXTRACT THE UNIQUES GROUP-ID VALUES AND  **
!               **  CHECK THAT Y AND X HAVE THE SAME GROUPS  **
!               ***********************************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FITR')     &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DISTIN(TAGY,N1,IWRITE,TAGYDIST,NYDIST,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      CALL SORT(TAGYDIST,NYDIST,TAGYDIST)
      CALL DISTIN(TAGX,N2,IWRITE,TAGXDIST,NXDIST,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GO TO 9000
      CALL SORT(TAGXDIST,NXDIST,TAGXDIST)
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FITR')THEN
        WRITE(ICOUT,141)NYDIST,NXDIST
  141   FORMAT('NYDIST,NXDIST = ',2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NYDIST.EQ.NXDIST)THEN
          DO II=1,NYDIST
            WRITE(ICOUT,143)II,TAGYDIST(II),TAGXDIST(II)
  143       FORMAT('II,TAGYDIST(II),TAGXDIST(II) = ',I8,2G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDDO
        ELSE
          DO II=1,NYDIST
            WRITE(ICOUT,145)II,TAGYDIST(II)
  145       FORMAT('II,TAGYDIST(II) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDDO
          DO II=1,NXDIST
            WRITE(ICOUT,147)II,TAGXDIST(II)
  147       FORMAT('II,TAGXDIST(II) = ',I8,G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDDO
        ENDIF
      ENDIF
!
      ISTEPN='2B'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FITR')     &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NYDIST.NE.NXDIST)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,212)
  212   FORMAT('THE TWO MEASUREMENT PROCESSES DO NOT HAVE THE SAME')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,214)
  214   FORMAT('NUMBER OF GROUPS.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,216)NYDIST
  216   FORMAT('      NUMBER OF GROUPS FOR FIRST  MEASUREMENT PROCESS = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,218)NXDIST
  218   FORMAT('      NUMBER OF GROUPS FOR SECOND MEASUREMENT PROCESS = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      NDIST=NYDIST
      DO II=1,NDIST
        IF(TAGYDIST(II).NE.TAGXDIST(II))THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,111)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,252)
  252     FORMAT('THE TWO MEASUREMENT PROCESSES DO NOT HAVE THE SAME GROUPS.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
      ENDDO
!
!               ****************************************
!               **  STEP 3A--COMPUTE VARIANCES FOR Y  **
!               ****************************************
!
      ISTEPN='3A'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FITR')     &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      SUM1=0.0
      SUM2=0.0
      DO II=1,NDIST
        HOLD=TAGYDIST(II)
        ICNT=0
        DO JJ=1,N1
          IF(TAGY(JJ).EQ.HOLD)THEN
            ICNT=ICNT+1
            TEMP1(ICNT)=Y(JJ)
          ENDIF
        ENDDO
        IF(ICNT.EQ.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,111)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,312)HOLD
  312     FORMAT('FOR MEASUREMENT PROCESS ONE (Y), GROUP ',F10.1,' HAS ',  &
                 'NO DATA.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ELSEIF(ICNT.EQ.1)THEN
          YMEAN(II)=TEMP1(1)
        ELSEIF(ICNT.GT.1)THEN
          SUM2=SUM2+REAL(ICNT-1)
          CALL MEAN(TEMP1,ICNT,IWRITE,YMEAN(II),IBUGA3,IERROR)
          DO JJ=1,ICNT
            SUM1=SUM1 + (TEMP1(JJ) - YMEAN(II))**2
          ENDDO
        ENDIF
!
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FITR')THEN
          WRITE(ICOUT,161)II,ICNT,SUM1,SUM2,YMEAN(II)
  161     FORMAT('II,ICNT,SUM1,SUM2,YMEAN(II) = ',2I6,3G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
      ENDDO
      YVAR=SUM1/SUM2
!
      ISTEPN='3B'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FITR')     &
         CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      SUM1=0.0
      SUM2=0.0
      DO II=1,NDIST
        HOLD=TAGXDIST(II)
        ICNT=0
        DO JJ=1,N2
          IF(TAGX(JJ).EQ.HOLD)THEN
            ICNT=ICNT+1
            TEMP1(ICNT)=X(JJ)
          ENDIF
        ENDDO
        IF(ICNT.EQ.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,111)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,362)HOLD
  362     FORMAT('FOR MEASUREMENT PROCESS TWO (X), GROUP ',F10.1,' HAS ',  &
                 'NO DATA.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ELSEIF(ICNT.EQ.1)THEN
          XMEAN(II)=TEMP1(1)
        ELSEIF(ICNT.GT.1)THEN
          SUM2=SUM2+REAL(ICNT-1)
          CALL MEAN(TEMP1,ICNT,IWRITE,XMEAN(II),IBUGA3,IERROR)
          DO JJ=1,ICNT
            SUM1=SUM1 + (TEMP1(JJ) - XMEAN(II))**2
          ENDDO
        ENDIF
!
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'FITR')THEN
          WRITE(ICOUT,371)II,ICNT,SUM1,SUM2,XMEAN(II)
  371     FORMAT('II,ICNT,SUM1,SUM2,XMEAN(II) = ',2I6,3G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        XVAR=SUM1/SUM2
      ENDDO
!
      DELTA=CPUMIN
      IF(XVAR.GT.0.0 .AND. YVAR.GT.0.0)DELTA=XVAR/YVAR
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'FITR')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DEMFITR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)XVAR,YVAR,DELTA,IERROR
 9013   FORMAT('XVAR,YVAR,DELTA,IERROR = ',3G15.7,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DEMFITR
      SUBROUTINE DENEST(DT, NDT, DLO, DHI, WINDOW, FT, SMOOTH,   &
                        NFT, ICAL, IERROR)
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DOUBLE PRECISION DT(NDT), FT(NFT), SMOOTH(NFT)
!
      CHARACTER*4 IERROR
!
      INCLUDE 'DPCOP2.INC'
!
!     ALGORITHM AS 176  APPL. STATIST. (1982) VOL.31, NO.1
!     Modified using AS R50 (Appl. Statist. (1984))
!
!     Find density estimate by kernel method using Gaussian kernel.
!     The interval on which the estimate is evaluated has end points
!     DLO and DHI.   If ICAL is not zero then it is assumed that the
!     routine has been called before with the same data and end points
!     and that the array FT has not been altered.
!
!     Auxiliary routines called: FORRT & REVRT from AS 97
!
!     NOTE: MODIFIED JULY 2001 FOR INCLUSION INTO DATAPLOT:
!           1) MAKE DOUBLE PRECISION
!           2) ADD SOME DATAPLOT I/O, ERROR FLAG
!           3) MAKE A FEW STYLISTIC CHANGES
!
      DATA ZERO/0.0D0/, HALF/0.5D0/, ONE/1.0D0/, SIX/6.0D0/
      DATA THIR2/32.0D0/
      DATA BIG/30.0/, KFTLO/5/, KFTHI/11/
!
!     The constant BIG is set so that exp(-BIG) can be calculated
!     without causing underflow problems and can be considered = 0.
!
!     Initialize and check for valid parameter values.
!
  999 FORMAT(1X)
!
      IERROR='NO'
      IF (WINDOW .LE. ZERO) THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** ERROR IN KERNEL DENSITY--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)
 9012   FORMAT('      THE WINDOW MUST BE POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)WINDOW
 9013   FORMAT('      VALUE OF WINDOW = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9999
      ENDIF
!
      IF (DLO .GE. DHI) THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)
 9021   FORMAT('***** ERROR IN KERNEL DENSITY--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9023)
 9023   FORMAT('      THE LOWER BOUNDARY IS GREATER THAN THE UPPER ',   &
               'BOUNDARY.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9999
      ENDIF
!
!  CHECK FOR VALID NUMBER OF POINTS FOR DENSITY TRACE
!  (MUST BE A POWER OF 2 IN RANGE 2**KFTLO TO 2**KFTHI),
!  CURRENTLY VALUES BETWEEN 2**5 = 32 AND 2**11 = 2,048.
!
      II = 2**KFTLO
      DO 1 K = KFTLO, KFTHI
         IF (II .EQ. NFT) GO TO 2
         II = II + II
    1 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9031)
 9031 FORMAT('***** ERROR IN KERNEL DENSITY.  INVALID VALUE FOR')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9033)
 9033 FORMAT('      NUMBER OF POINTS IN THE DENSITY TRACE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9035)NFT
 9035 FORMAT('      NUMBER OF POINTS = ',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9999
!
    2 CONTINUE
      STEP = (DHI - DLO) / DBLE(NFT)
      AINC = ONE / (NDT * STEP)
      NFT2 = NFT / 2
      HW = WINDOW / STEP
      FAC1 = THIR2 * (ATAN(ONE) * HW / NFT) ** 2
      IF (ICAL .NE. 0) GO TO 10
!
!     Discretize the data
!
      DLO1 = DLO - STEP * HALF
      DO 3 J = 1, NFT
        FT(J) = ZERO
    3 CONTINUE
!
      DO 4 I = 1, NDT
         WT = (DT(I) - DLO1) / STEP
         JJ = INT(WT)
         IF (JJ .LT. 1 .OR. JJ .GT. NFT) GO TO 4
         WT = WT - JJ
         WINC = WT * AINC
         KK = JJ + 1
         IF (JJ .EQ. NFT) KK = 1
         FT(JJ) = FT(JJ) + AINC - WINC
         FT(KK) = FT(KK) + WINC
    4 CONTINUE
!
!     Transform to find FT.
!
      CALL FORRT(FT, NFT)
!
!     Find transform of density estimate.
!
   10 CONTINUE
      JHI = INT(SQRT(BIG / FAC1) + 0.1)
      JMAX = MIN(NFT2 - 1, JHI)
      SMOOTH(1) = FT(1)
      RJ = ZERO
      DO 11 J = 1, JMAX
         RJ = RJ + ONE
         RJFAC = RJ * RJ * FAC1
         BC = ONE - RJFAC / (HW * HW * SIX)
         FAC = EXP(-RJFAC) / BC
         J1 = J + 1
         J2 = J1 + NFT2
         SMOOTH(J1) = FAC * FT(J1)
         SMOOTH(J2) = FAC * FT(J2)
   11 CONTINUE
!
!     Cope with underflow by setting tail of transform to zero.
!
      IF (JHI + 1 - NFT2 .GT. 0) THEN
        SMOOTH(NFT2 + 1) = EXP(-FAC1 * FLOAT(NFT2)**2) * FT(NFT2 + 1)
      ELSEIF (JHI + 1 - NFT2 .LT. 0) THEN
        J2LO = JHI + 2
        DO 22 J1 = J2LO, NFT2
           J2 = J1 + NFT2
           SMOOTH(J1) = ZERO
           SMOOTH(J2) = ZERO
   22   CONTINUE
        SMOOTH(NFT2 + 1) = ZERO
      ELSE
        SMOOTH(NFT2 + 1) = ZERO
      ENDIF
!
!     Invert Fourier transform of SMOOTH to get estimate and eliminate
!     negative density values.
!
      CALL REVRT(SMOOTH, NFT)
      DO 25 J = 1, NFT
        IF (SMOOTH(J) .LT. ZERO) SMOOTH(J) = ZERO
   25 CONTINUE
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE DENEST
      DOUBLE PRECISION FUNCTION DNRM2(N,DX,INCX)
!***BEGIN PROLOGUE  DNRM2
!***DATE WRITTEN   791001   (YYMMDD)
!***REVISION DATE  820801   (YYMMDD)
!***CATEGORY NO.  D1A3B
!***KEYWORDS  BLAS,DOUBLE PRECISION,EUCLIDEAN,L2,LENGTH,LINEAR ALGEBRA,
!             NORM,VECTOR
!***AUTHOR  LAWSON, C. L., (JPL)
!           HANSON, R. J., (SNLA)
!           KINCAID, D. R., (U. OF TEXAS)
!           KROGH, F. T., (JPL)
!***PURPOSE  Euclidean length (L2 norm) of d.p. vector
!***DESCRIPTION
!
!                B L A S  Subprogram
!    Description of parameters
!
!     --Input--
!        N  number of elements in input vector(s)
!       DX  double precision vector with N elements
!     INCX  storage spacing between elements of DX
!
!     --Output--
!    DNRM2  double precision result (zero if N .LE. 0)
!
!     Euclidean norm of the N-vector stored in DX() with storage
!     increment INCX .
!     If    N .LE. 0 return with result = 0.
!     If N .GE. 1 then INCX must be .GE. 1
!
!           C.L. Lawson, 1978 Jan 08
!
!     Four phase method     using two built-in constants that are
!     hopefully applicable to all machines.
!         CUTLO = maximum of  DSQRT(U/EPS)  over all known machines.
!         CUTHI = minimum of  DSQRT(V)      over all known machines.
!     where
!         EPS = smallest no. such that EPS + 1. .GT. 1.
!         U   = smallest positive no.   (underflow limit)
!         V   = largest  no.            (overflow  limit)
!
!     Brief outline of algorithm..
!
!     Phase 1    scans zero components.
!     move to phase 2 when a component is nonzero and .LE. CUTLO
!     move to phase 3 when a component is .GT. CUTLO
!     move to phase 4 when a component is .GE. CUTHI/M
!     where M = N for X() real and M = 2*N for complex.
!
!     Values for CUTLO and CUTHI..
!     From the environmental parameters listed in the IMSL converter
!     document the limiting values are as followS..
!     CUTLO, S.P.   U/EPS = 2**(-102) for  Honeywell.  Close seconds are
!                   Univac and DEC at 2**(-103)
!                   Thus CUTLO = 2**(-51) = 4.44089E-16
!     CUTHI, S.P.   V = 2**127 for Univac, Honeywell, and DEC.
!                   Thus CUTHI = 2**(63.5) = 1.30438E19
!     CUTLO, D.P.   U/EPS = 2**(-67) for Honeywell and DEC.
!                   Thus CUTLO = 2**(-33.5) = 8.23181D-11
!     CUTHI, D.P.   same as S.P.  CUTHI = 1.30438D19
!     DATA CUTLO, CUTHI / 8.232D-11,  1.304D19 /
!     DATA CUTLO, CUTHI / 4.441E-16,  1.304E19 /
!***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,
!                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*,
!                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL
!                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323
!***ROUTINES CALLED  (NONE)
!***END PROLOGUE  DNRM2
      INTEGER          NEXT
      DOUBLE PRECISION   DX(1), CUTLO, CUTHI, HITEST, SUM, XMAX,ZERO,ONE
      DATA   ZERO, ONE /0.0D0, 1.0D0/
!
      DATA CUTLO, CUTHI / 8.232D-11,  1.304D19 /
!***FIRST EXECUTABLE STATEMENT  DNRM2
      IF(N .GT. 0) GO TO 10
         DNRM2  = ZERO
         GO TO 300
!
!CC10 ASSIGN 30 TO NEXT
   10 CONTINUE
      NEXT=30
      SUM = ZERO
      NN = N * INCX
!                                                 BEGIN MAIN LOOP
      I = 1
!CC20 GO TO NEXT,(30, 50, 70, 110)
   20 CONTINUE
      IF(NEXT.EQ.30)THEN
        GO TO 30
      ELSEIF(NEXT.EQ.50)THEN
        GO TO 50
      ELSEIF(NEXT.EQ.70)THEN
        GO TO 70
      ELSEIF(NEXT.EQ.110)THEN
        GO TO 110
      ENDIF
!
   30 IF( DABS(DX(I)) .GT. CUTLO) GO TO 85
!CCCC ASSIGN 50 TO NEXT
      NEXT=50
      XMAX = ZERO
!
!                        PHASE 1.  SUM IS ZERO
!
   50 IF( DX(I) .EQ. ZERO) GO TO 200
      IF( DABS(DX(I)) .GT. CUTLO) GO TO 85
!
!                                PREPARE FOR PHASE 2.
!CCCC ASSIGN 70 TO NEXT
      NEXT=70
      GO TO 105
!
!                                PREPARE FOR PHASE 4.
!
  100 I = J
!CCCC ASSIGN 110 TO NEXT
      NEXT=110
      SUM = (SUM / DX(I)) / DX(I)
  105 XMAX = DABS(DX(I))
      GO TO 115
!
!                   PHASE 2.  SUM IS SMALL.
!                             SCALE TO AVOID DESTRUCTIVE UNDERFLOW.
!
   70 IF( DABS(DX(I)) .GT. CUTLO ) GO TO 75
!
!                     COMMON CODE FOR PHASES 2 AND 4.
!                     IN PHASE 4 SUM IS LARGE.  SCALE TO AVOID OVERFLOW.
!
  110 IF( DABS(DX(I)) .LE. XMAX ) GO TO 115
         SUM = ONE + SUM * (XMAX / DX(I))**2
         XMAX = DABS(DX(I))
         GO TO 200
!
  115 SUM = SUM + (DX(I)/XMAX)**2
      GO TO 200
!
!
!                  PREPARE FOR PHASE 3.
!
   75 SUM = (SUM * XMAX) * XMAX
!
!
!     FOR REAL OR D.P. SET HITEST = CUTHI/N
!     FOR COMPLEX      SET HITEST = CUTHI/(2*N)
!
   85 CONTINUE
      HITEST = CUTHI/FLOAT( N )
!
!                   PHASE 3.  SUM IS MID-RANGE.  NO SCALING.
!
      DO 95 J =I,NN,INCX
         IF(DABS(DX(J)) .GE. HITEST) GO TO 100
         SUM = SUM + DX(J)**2
   95 CONTINUE
      DNRM2 = DSQRT( SUM )
      GO TO 300
!
  200 CONTINUE
      I = I + INCX
      IF ( I .LE. NN ) GO TO 20
!
!              END OF MAIN LOOP.
!
!              COMPUTE SQUARE ROOT AND ADJUST FOR SCALING.
!
      DNRM2 = XMAX * DSQRT(SUM)
  300 CONTINUE
      RETURN
      END FUNCTION DNRM2
      SUBROUTINE DECONV(Y1,N1,Y2,N2,NUMVAR,IWRITE,   &
                        Y3,N3,IBUGA3,IERROR)
!
!     PURPOSE--COMPUTE DECONVOLUTION OF 2 VARIABLES.
!     NOTE--IF  THE FIRST  VARIABLE IS Y1(.)
!           AND THE SECOND VARIABLE IS Y2(.),
!           THEN THE OUTPUT VARIABLE CONTAINING THE
!           DECONVOLUTION
!           WILL BE COMPUTED AS FOLLOWS (IF N1 EQUALS OR EXCEEDS N2)--
!              Y3(1)=Y2(1)/Y1(1)
!              Y3(2)=(Y2(2)-Y1(2)*Y3(1)) / Y1(1)
!              Y3(3)=(Y2(3) - Y1(3)*Y3(1) - Y1(2)*Y3(2)) / Y1(1)
!              ETC.
!           AND CONVERSELY IF N1 IS LESS THAN N2.
!     NOTE--IT IS NOT PERMISSIBLE TO HAVE THE OUTPUT VECTOR Y3(.)
!           BEING IDENTICAL WITH (OVERLAYED ONTO) THE INPUT VECTORS Y1(.)
!           OR Y2(.).
!     NOTE--Y1 AND Y2 NEED NOT BE THE SAME LENGTH.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-921-3651
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--NOVEMBER  1981.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!---------------------------------------------------------------------
!
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION Y3(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DECO'
      ISUBN2='NV  '
      IERROR='NO'
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DECONV--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,IWRITE
   52   FORMAT('IBUGA3,IWRITE = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)N1,N2,NUMVAR
   53   FORMAT('N1,N2,NUMVAR = ',3I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N1
          WRITE(ICOUT,56)I,Y1(I)
   56     FORMAT('I,Y1(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
        DO 57 I=1,N2
          WRITE(ICOUT,58)I,Y2(I)
   58     FORMAT('I,Y2(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   57   CONTINUE
      ENDIF
!
!               *********************************
!               **  COMPUTE THE DECONVOLUTION  **
!               *********************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(N1.LE.0)GO TO 150
      IF(N2.LE.0)GO TO 150
!
      IF(N1.LE.N2)N3=N2-N1+1
      IF(N1.GT.N2)N3=N1-N2+1
      IF(N3.LE.0)GO TO 170
!
      DO 100 I3=1,N3
      Y3(I3)=0.0
  100 CONTINUE
!
      DO 500 I3=1,N3
      SUM=0.0
      J3MAX=I3-1
      IF(J3MAX.LE.0)GO TO 550
      DO 600 J3=1,J3MAX
      J1ARG=I3-J3+1
      IF(N1.LE.N2)SUM=SUM+Y1(J1ARG)*Y3(J3)
      IF(N1.GT.N2)SUM=SUM+Y2(J1ARG)*Y3(J3)
  600 CONTINUE
  550 CONTINUE
      IF(N1.LE.N2)Y3(I3)=(Y2(I3)-SUM)/Y1(1)
      IF(N1.GT.N2)Y3(I3)=(Y1(I3)-SUM)/Y2(1)
  500 CONTINUE
      GO TO 190
!
  150 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,151)
  151 FORMAT('***** ERROR IN DECONV--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,152)
  152 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,153)
  153 FORMAT('      IN THE VARIABLES FOR WHICH')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,154)
  154 FORMAT('      THE DECONVOLUTION IS TO BE COMPUTED')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,155)
  155 FORMAT('      MUST BE 1 OR LARGER.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,156)
  156 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,157)N1,N2
  157 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS HERE = ',2I8,   &
      '.')
      CALL DPWRST('XXX','BUG ')
      GO TO 190
!
  170 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,171)
  171 FORMAT('***** ERROR IN DECONV--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,172)
  172 FORMAT('      THE NUMBER OF OBSERVATIONS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,173)
  173 FORMAT('      IN THE RESULTING DECONVOLUTION VARIABLE ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,175)
  175 FORMAT('      MUST BE 1 OR LARGER.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,176)
  176 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,177)N3
  177 FORMAT('      THE OUTPUT NUMBER OF OBSERVATIONS HERE = ',I8,   &
      '.')
      CALL DPWRST('XXX','BUG ')
      GO TO 190
!
  190 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DECONV--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR
 9012   FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)N1,N2,NUMVAR,N3
 9013   FORMAT('N1,N2,NUMVAR,N3 = ',4I8)
        CALL DPWRST('XXX','BUG ')
        N12=N1
        IF(N2.GT.N1)N12=N2
        DO 9015 I=1,N12
          WRITE(ICOUT,9016)I,Y1(I),Y2(I),Y3(I)
 9016     FORMAT('I,Y1(I),Y2(I),Y3(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DECONV
      SUBROUTINE DEHAAN(X,N,THRESH,GAMMA,SD,KK,ANM1)
!C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!   SUBROUTINE IMPLEMENTING THE DEHAAN-                 C
!   DEKKER MOMENT-BASED EXTREME VALUE                   C
!   INDEX ESTIMATOR AS DOCUMENTED IN                    C
!   "EXTREME VALUE THEORY AND APPLICATIONS",            C
!   EDITED BY GALAMBOS, LECHNER, AND SIMIU, PP. 93-122, C
!   KLUWER ACADEMIC PUBLISHERS, BOSTON, 1994.           C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C
!C NOTE: DEHAAN NORMALLY DONE AS A PLOT.  WE ARE PICKING A SINGLE
!C       "SAMPLE" VALUE, ALGORITHM WAS MODIFIED ACCORDINGLY.
!C
!C UPDATED 10/2010: SLIGHT TWEAK TO ALGORITHM.  PASS IN VALUE
!C OF THRESHOLD AND USE THIS AS VALUE FOR DX2.  THE X ARRAY SHOULD
!C CONTAIN POINTS ABOVE THE THRESHOLD ONLY.
!C
      DOUBLE PRECISION GAMNUM,GAMDEN, DGAMMA
      DOUBLE PRECISION DTERM1, DX1, DX2
      REAL GAMMA
      REAL X(*)
!C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!           THE MAIN LOOP         C
!   COMPUTE THE DEHAAN-DEKKER     C
!   INDEX "GAMMA" FOR THE K       C
!   HIGHEST ORDER STATISTICS,     C
!   ITERATING ON K.               C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C
      NI=N
!
      AN=REAL(NI)
      ATEMP=SQRT(AN)
      KK = NI
!C
!  GAMNUM AND GAMDEN ARE MN(1) AND MN(2) ON PAGE 100
!  OF THE REFERENCE CITED ABOVE.
!
      GAMNUM=0.D0
      GAMDEN=0.D0
!C
      DO 50 J=1,KK
!CCCC DO 50 J=1,NI
!C
          JM1=J-1
          DX1=DBLE(X(NI-JM1))
!CCCC     DX2=DBLE(X(NI-KK))
          DX2=THRESH
          DTERM1=DLOG(DX1)-DLOG(DX2)
          GAMNUM=GAMNUM+DTERM1
          GAMDEN=GAMDEN+DTERM1*DTERM1
!C
50      CONTINUE
!C
        GAMNUM=GAMNUM/DBLE(KK)
        GAMDEN=GAMDEN/DBLE(KK)
        ANM1=REAL(GAMNUM)
        ANM2=REAL(GAMDEN)
!C
        DTERM1=GAMNUM**2/GAMDEN
        DGAMMA=GAMNUM + 1.0D0 - 0.5D0*(1.0D0/(1.0D0 - DTERM1))
        GAMMA=REAL(DGAMMA)
!
!  COMPUTE THE STANDARD DEVIATION OF C
!
      IF(GAMMA.GE.0.0)THEN
        SD=SQRT((1.0+GAMMA*GAMMA)/REAL(KK))
      ELSE
        DTERM1=(1.0D0-DGAMMA)*(1.0D0-DGAMMA)*(1.0D0-2.0D0*DGAMMA)
        DTERM2=4.0D0-8.0D0*(1.0D0-2.0D0*DGAMMA)/(1.0D0-3.0D0*DGAMMA)
        DTERM3=(5.0D0-11.0D0*DGAMMA)*(1.0D0-2.0D0*DGAMMA)/   &
               ((1.0D0-3.0D0*DGAMMA)*(1.0D0-4.0D0*DGAMMA))
        SD=REAL(DSQRT(DTERM1*(DTERM2+DTERM3)/DBLE(KK)))
      ENDIF
!C
      RETURN
      END SUBROUTINE DEHAAN
      SUBROUTINE DEQUOT(IA,NCIN,IB,NCOUT2,IBUGSU,ISUBRO)
!
!     PURPOSE--CHECK A STRING FOR LEADING/TRAILING QUOTES AND
!              REMOVE IF FOUND.  USED FOR FILE NAME ARGUMENTS THAT
!              MAY BE QUOTED IF THEY CONTAIN SPACES OR HYPHENS.
!     INPUT  ARGUMENTS--IA     = INPUT CHARACTER STRING
!                       NCIN   = INTEGER NUMBER OF CHARACTERS TO CHECK
!                       IBUGSU = HOLLERITH BUG (= TRACE) VARIABLE
!     OUTPUT ARGUMENTS--IB     = OUTPUT CHARACTER STRING
!                       NCOUT2  = INTEGER NUMBER OF CHARACTERS ON OUTPUT
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
!     VERSION NUMBER--2004/8
!     ORIGINAL VERSION--OCTOBER   2004
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*(*) IA
      CHARACTER*(*) IB
!
      CHARACTER*1 IQUOTE
      CHARACTER*1 IQUOT2
!
      CHARACTER*4 IBUGSU
      CHARACTER*4 ISUBRO
!
!---------------------------------------------------------------------
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IF(IBUGSU.EQ.'ON' .OR. ISUBRO.EQ.'QUOT')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DEQUOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)NCIN,IBUGSU
   52   FORMAT('NCIN,IBUGSU = ',I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IA(1:MIN(80,NCIN))
   53   FORMAT('(IA(1:NCIN) = ',80A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ******************************************************
!               **  CHECK FOR LEADING/TRAILING QUOTES.              **
!               ******************************************************
!
!
      CALL DPCONA(39,IQUOTE)
      IQUOT2='"'
      NCOUT2=0
!
      IF(NCIN.GT.0)THEN
        IF(IA(1:1).EQ.IQUOT2)THEN
          DO 100 I=2,NCIN
            IF(IA(I:I).EQ.IQUOT2)GO TO 109
              NCOUT2=NCOUT2+1
              IB(NCOUT2:NCOUT2)=IA(I:I)
  100     CONTINUE
  109     CONTINUE
        ELSEIF(IA(1:1).EQ.'"')THEN
          DO 200 I=2,NCIN
            IF(IA(I:I).EQ.IQUOTE)GO TO 209
              NCOUT2=NCOUT2+1
              IB(NCOUT2:NCOUT2)=IA(I:I)
  200     CONTINUE
  209     CONTINUE
        ELSE
          IB(1:NCIN)=IA(1:NCIN)
          NCOUT2=NCIN
        ENDIF
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
      IF(IBUGSU.EQ.'ON' .OR. ISUBRO.EQ.'QUOT')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DEQUOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)NCOUT2
 9012   FORMAT('NCOUT2 = ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(NCOUT2.GT.0)THEN
          WRITE(ICOUT,9013)IB(1:MIN(80,NCOUT2))
 9013     FORMAT('(IB(1:NCOUT2) = ',80A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DEQUOT
!===================================================== DERF.FOR
      DOUBLE PRECISION FUNCTION DERFDP(X)
!CCCC 2020/03: RENAME TO AVOID CONFLICT WITH INTRNISIC FUNCTION
!CCCC DOUBLE PRECISION FUNCTION DERF(X)
!***********************************************************************
!*                                                                     *
!*  FORTRAN CODE WRITTEN FOR INCLUSION IN IBM RESEARCH REPORT RC20525, *
!*  'FORTRAN ROUTINES FOR USE WITH THE METHOD OF L-MOMENTS, VERSION 3' *
!*                                                                     *
!*  J. R. M. HOSKING                                                   *
!*  IBM RESEARCH DIVISION                                              *
!*  T. J. WATSON RESEARCH CENTER                                       *
!*  YORKTOWN HEIGHTS                                                   *
!*  NEW YORK 10598, U.S.A.                                             *
!*                                                                     *
!*  VERSION 3     AUGUST 1996                                          *
!*                                                                     *
!***********************************************************************
!
!  ERROR FUNCTION
!
!  BASED ON ALGORITHM 5666, J.F.HART ET AL. (1968) 'COMPUTER
!  APPROXIMATIONS'
!
!  ACCURATE TO 15 DECIMAL PLACES
!
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DATA ZERO/0D0/,ONE/1D0/,TWO/2D0/,THREE/3D0/,FOUR/4D0/,P65/0.65D0/
!
!         COEFFICIENTS OF RATIONAL-FUNCTION APPROXIMATION
!
      DATA P0,P1,P2,P3,P4,P5,P6/   &
        0.2202068679123761D3,    0.2212135961699311D3,   &
        0.1120792914978709D3,    0.3391286607838300D2,   &
        0.6373962203531650D1,    0.7003830644436881D0,   &
        0.3526249659989109D-1/
      DATA Q0,Q1,Q2,Q3,Q4,Q5,Q6,Q7/   &
        0.4404137358247522D3,   0.7938265125199484D3,   &
        0.6373336333788311D3,   0.2965642487796737D3,   &
        0.8678073220294608D2,   0.1606417757920695D2,   &
        0.1755667163182642D1,   0.8838834764831844D-1/
!
!         C1 IS SQRT(2), C2 IS SQRT(2/PI)
!         BIG IS THE POINT AT WHICH DERF=1 TO MACHINE PRECISION
!
      DATA C1/1.414213562373095D0/
      DATA C2/7.978845608028654D-1/
      DATA BIG/6.25D0/,CRIT/5D0/
!
      DERFDP=ZERO
      IF(X.EQ.ZERO)RETURN
      XX=DABS(X)
      IF(XX.GT.BIG)GO TO  20
      EXPNTL=DEXP(-X*X)
      ZZ=DABS(X*C1)
      IF(XX.GT.CRIT)GO TO  10
      DERFDP=EXPNTL*((((((P6*ZZ+P5)*ZZ+P4)*ZZ+P3)*ZZ+P2)*ZZ+P1)*ZZ+P0)/   &
        (((((((Q7*ZZ+Q6)*ZZ+Q5)*ZZ+Q4)*ZZ+Q3)*ZZ+Q2)*ZZ+Q1)*ZZ+Q0)
      IF(X.GT.ZERO)DERFDP=ONE-TWO*DERFDP
      IF(X.LT.ZERO)DERFDP=TWO*DERFDP-ONE
      RETURN
!
   10 DERFDP=EXPNTL*C2/(ZZ+ONE/(ZZ+TWO/(ZZ+THREE/(ZZ+FOUR/(ZZ+P65)))))
      IF(X.GT.ZERO)DERFDP=ONE-DERFDP
      IF(X.LT.ZERO)DERFDP=DERFDP-ONE
      RETURN
!
   20 DERFDP=ONE
      IF(X.LT.ZERO)DERFDP=-ONE
      RETURN
      END FUNCTION DERFDP
      SUBROUTINE DERIV0(IW21,IW22,ITYPE,NW,   &
      IPARN1,IPARN2,NUMPAR,IVARN1,IVARN2,NUMVAR,   &
      ICON,ICON1,ICON2,NCON,ID1,ID2,NWD,   &
      IBUGA3,ISUBRO,IFOUND,IERROR)
!
! NOTE--THE ARRAY ICONN (DEFINED BELOW AND USED
!       IN SUBSEQUENT SUBROUTINES) IS PROBABLY
!       SUPERFLUOUS AND PROBABLY NO LONGER SERVES ANY PURPOSE
!       (CHECK THIS).
!       THE NECESSITY OF IEXPN IS ALSO IN QUESTION.
!
!---------------------------------------------------------------------
!
      CHARACTER*4 IW21
      CHARACTER*4 IW22
      CHARACTER*4 ITYPE
      CHARACTER*4 IPARN1
      CHARACTER*4 IPARN2
      CHARACTER*4 IVARN1
      CHARACTER*4 IVARN2
      CHARACTER*4 ICON
      CHARACTER*4 ID1
      CHARACTER*4 ID2
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 ILF
      CHARACTER*4 IHOLD1
      CHARACTER*4 IHOLD2
      CHARACTER*4 IFUN01
      CHARACTER*4 IFUN02
      CHARACTER*4 IDER01
      CHARACTER*4 IDER02
      CHARACTER*4 ICONN
      CHARACTER*4 IEXPN
!
      CHARACTER*4 IHOLW1
      CHARACTER*4 IHOLW2
      CHARACTER*4 IHOLDT
      CHARACTER*4 ITER01
      CHARACTER*4 ITER02
!
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!CCCC CHARACTER*4 IBUG1
      CHARACTER*4 IBUG2
!CCCC CHARACTER*4 IBUG3
!CCCC CHARACTER*4 IBUG41
!CCCC CHARACTER*4 IBUG5
!CCCC CHARACTER*4 IBUG51
!
      DIMENSION IW21(*)
      DIMENSION IW22(*)
      DIMENSION ITYPE(*)
      DIMENSION IPARN1(*)
      DIMENSION IPARN2(*)
      DIMENSION IVARN1(*)
      DIMENSION IVARN2(*)
      DIMENSION ICON(*)
      DIMENSION ICON1(*)
      DIMENSION ICON2(*)
      DIMENSION ID1(*)
      DIMENSION ID2(*)
!
      DIMENSION IHOLD1(200)
      DIMENSION IHOLD2(200)
      DIMENSION IFUN01(200)
      DIMENSION IFUN02(200)
      DIMENSION IDER01(200)
      DIMENSION IDER02(200)
      DIMENSION ICONN(200)
      DIMENSION IEXPN(200)
!
      DIMENSION IHOLW1(200)
      DIMENSION IHOLW2(200)
      DIMENSION IHOLDT(200)
      DIMENSION ITER01(1000)
      DIMENSION ITER02(1000)
      DIMENSION ITERM1(100)
      DIMENSION ITERM2(100)
!
!-----COMMON VARIABLES (GENERAL)-----------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-----------------------------------------------------
!CCCC DATA IBUG1/'OFF '/
      DATA IBUG2/'OFF '/
!CCCC DATA IBUG3/'OFF '/
!CCCC DATA IBUG41/'OFF '/
!CCCC DATA IBUG5/'OFF '/
!CCCC DATA IBUG51/'OFF '/
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DERI'
      ISUBN2='V0  '
!
      IMIN=1
      IMAX=1
!
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'RIV0')GO TO 90
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('AT THE BEGINNING OF DERIV0--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)NW
   52 FORMAT('NW = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 55 I=1,NW
      WRITE(ICOUT,56)I,ITYPE(I),IW21(I),IW22(I)
   56 FORMAT('I,ITYPE(I),IW21(I),IW22(I) = ',I8,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
      WRITE(ICOUT,61)NCON
   61 FORMAT('NCON = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 65 I=1,NCON
      WRITE(ICOUT,66)I,ICON1(I),ICON2(I),ICON(I)
   66 FORMAT('I,ICON1(I),ICON2(I),ICON(I) = ',3I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
   90 CONTINUE
!
!               ***********************************
!               **  STEP 0--                     **
!               **  REDUCE THE FULL EXPRESSION   **
!               **  INTO NAMED SUB-EXPRESSIONS.  **
!               ***********************************
!
      IT2=0
!
!               *****************************************
!               **  STEP 1--                           **
!               **  REPLACE THE CONSTANTS              **
!               **  BY THE CONSTANT DESIGNATIONS.      **
!               *****************************************
!
      ILOOP=1
 2350 CONTINUE
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 2400 I=1,NW
      I2=I
      IF(ITYPE(I).EQ.'N   ')GO TO 2450
 2400 CONTINUE
      ISTOP=NW+1
      ISTART=0
      GO TO 2790
 2450 CONTINUE
!
      ISTART=I2
      ISTOP=ISTART
      CALL DPC4HI(IW21(ISTOP),IC,IBUGA3,IERROR)
!
!               ***************************************************
!               **  STEP 1.4--                                   **
!               **  TEMPORARILY COPY THE STRING WHICH IS BEYOND  **
!               **  THE CONSTANT NUMBER                          **
!               **  INTO IHOLD1(.).                               **
!               ***************************************************
!
      ISTEPN='1.4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      J=0
      ISTOP1=ISTOP+1
      IF(ISTOP1.GT.NW)GO TO 2249
      DO 2240 I=ISTOP1,NW
      J=J+1
      IHOLW1(J)=IW21(I)
      IHOLW2(J)=IW22(I)
      IHOLDT(J)=ITYPE(I)
 2240 CONTINUE
 2249 CONTINUE
      NREST=J
!
!               ****************************
!               **  STEP 1.5--            **
!               **  REPLACE THE CONSTANT  **
!               **  BY A & AND A NUMBER.  **
!               ****************************
!
      ISTEPN='1.5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      J=ISTART
      IW21(J)='&   '
      IW22(J)='    '
      ITYPE(J)='C   '
      J=J+1
      CALL DPC4IH(IC,IW21(J),IBUGA3,IERROR)
      IW22(J)='    '
      ITYPE(J)='C   '
!
      IF(NREST.LE.0)GO TO 2290
      DO 2280 I=1,NREST
      J=J+1
      IW21(J)=IHOLW1(I)
      IW22(J)=IHOLW2(I)
      ITYPE(J)=IHOLDT(I)
 2280 CONTINUE
 2290 CONTINUE
      NW=J
!
      IF(ISTART.LE.0)GO TO 2790
      ILOOP=ILOOP+1
      IF(ILOOP.LE.10000)GO TO 2350
 2790 CONTINUE
!
      ILOOP=1
 5310 CONTINUE
      DO 5400 I=1,NW
      I2=I
      IF(ITYPE(I).EQ.'RP  ')GO TO 5450
 5400 CONTINUE
      ISTOP=NW+1
      ISTART=0
      GO TO 5690
 5450 CONTINUE
!
      ISTOP=I2
      DO 5600 I=1,ISTOP
      IREV=ISTOP-I+1
      IF(ITYPE(IREV).EQ.'LP  ')GO TO 5650
 5600 CONTINUE
      WRITE(ICOUT,5605)
 5605 FORMAT('***** ERROR IN COMPID--ITYPE(IREV) NOT LP')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      RETURN
 5650 CONTINUE
      ISTART=IREV
 5690 CONTINUE
!
      ISTAP1=ISTART+1
      ISTOM1=ISTOP-1
!
!               *******************************************************
!               **  STEP 1.6--                                       **
!               **  CHECK THE INTERNAL STRING TO SEE                 **
!               **  IF IT IS EXACTLY 2 POSITIONS WIDE, AND           **
!               **  ALSO THAT IT IS OF THE FORM                      **
!               **  $ FOLLOWED BY A NUMBER.                          **
!               **  IF SO, THEN THIS IMPLIES                         **
!               **  THAT THE INTERNAL ORIGINAL STRING                **
!               **  HAS ALREADY BEEN FULLY REDUCED.                  **
!               **  IF NOT SO, THEN THIS IMPLIES                     **
!               **  THAT THE INTERNAL ORIGINAL                       **
!               **  STRING HAS NOT YET BEEN FULLY REDUCED,           **
!               **  AND THAT THE OPERATION PRELIMINARY               **
!               **  TO THE ( MUST BE CHECKED TO                      **
!               **  DETERMINE IF THE PARENTHESES                     **
!               **  ARE TO BE KEPT OR DELETED                        **
!               **  (KEEP IF A PRELIMINARY LIBRARY FUNCTION;         **
!               **  DELETE IF A PRELIMINARY OPERATION--+,-,*,/,**).  **
!               **  DELETE IF ANYTHING ELSE).                        **
!               *******************************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTOM2=ISTOP-2
      IWIDIS=ISTOM1-ISTAP1+1
      IF(IWIDIS.EQ.2.AND.IW21(ISTOM2).EQ.'$   ')GO TO 6300
      GO TO 6200
!
!               ******************************
!               ******************************
!               **  STEP 2--                **
!               **  TREAT THE NO-$ CASE.    ************************************
!               **  THIS WILL BE THE        **
!               **  NOT-FULLY-REDUCED CASE. **
!               ******************************
!
!               *************************************************
!               **  STEP 2.1--                                 **
!               **  CHECK FOR A PRELIMINARY LIBRARY FUNCTION.  **
!               *************************************************
!
 6200 CONTINUE
      ISTEPN='2.1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ILF='NO  '
      ISTAM1=ISTART-1
      IF(ISTAM1.LE.0)GO TO 6219
      IF(ITYPE(ISTAM1).EQ.'LF  ')ILF='YES'
 6219 CONTINUE
!
!               *******************************
!               **  STEP 2.2--               **
!               **  COPY THE STRING BETWEEN  **
!               **  (BUT NOT INCLUDING) THE  **
!               **  PARENTHESES.             **
!               *******************************
!
      ISTEPN='2.2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      J=0
      ITERM1(ILOOP)=IT2+1
      DO 6220 I=ISTAP1,ISTOM1
      J=J+1
      IT2=IT2+1
      ITER01(IT2)=IW21(I)
      ITER02(IT2)=IW22(I)
 6220 CONTINUE
      ITERM2(ILOOP)=IT2
!
!               ***************************************************
!               **  STEP 2.3--                                   **
!               **  TEMPORARILY COPY THE STRING WHICH IS BEYOND  **
!               **  THE RIGHT PARENTHESIS                        **
!               **  INTO IHOLD1(.).                               **
!               ***************************************************
!
      ISTEPN='2.3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      J=0
      ISTOP1=ISTOP+1
      IF(ISTOP1.GT.NW)GO TO 6249
      DO 6240 I=ISTOP1,NW
        J=J+1
        IHOLD1(J)=IW21(I)
        IHOLD2(J)=IW22(I)
        IHOLDT(J)=ITYPE(I)
 6240 CONTINUE
 6249 CONTINUE
      NREST=J
!
!               ********************************************
!               **  STEP 2.4--                            **
!               **  REPLACE THE EXTRACTED STRING BY       **
!               **  A $ AND THE LOOP NUMBER.              **
!               **  RETAIN OR DELETE PARENTHESES          **
!               **  DEPENDING ON WHETHER THE PRELIMINARY  **
!               **  OPERATION IS A LIBRARY FUNCTION       **
!               **  OR AN ARITHMETIC OPERATION.           **
!               ********************************************
!
      ISTEPN='2.4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ILF.EQ.'YES')J=ISTART
      IF(ILF.EQ.'NO  ')J=ISTART-1
      J=J+1
      IW21(J)='$   '
      IW22(J)='    '
      ITYPE(J)='E   '
      J=J+1
      CALL DPC4IH(ILOOP,IW21(J),IBUGA3,IERROR)
      IW22(J)='    '
      ITYPE(J)='E   '
      IF(ILF.EQ.'YES')J=J+1
      IF(ILF.EQ.'YES')IW21(J)=')   '
      IF(ILF.EQ.'YES')IW22(J)='    '
      IF(ILF.EQ.'YES')ITYPE(J)='RP  '
      IF(NREST.LE.0)GO TO 6290
      DO 6260 I=1,NREST
        J=J+1
        IW21(J)=IHOLD1(I)
        IW22(J)=IHOLD2(I)
        ITYPE(J)=IHOLDT(I)
 6260 CONTINUE
 6290 CONTINUE
      NW=J
      GO TO 6900
!
!               ****************************
!               **  STEP 3--              **
!               **  TREAT THE $ CASE.     **
!               **  THIS WILL BE THE      **
!               **  FULLY-REDUCED CASE.   **
!               ****************************
!
!               *************************************************
!               **  STEP 3.1--                                 **
!               **  CHECK FOR A PRELIMINARY LIBRARY FUNCTION.  **
!               *************************************************
!
 6300 CONTINUE
      ISTEPN='3.1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ILF='NO  '
      ISTAM1=ISTART-1
      IF(ISTAM1.LE.0)GO TO 6319
      IF(ITYPE(ISTAM1).EQ.'LF  ')ILF='YES'
 6319 CONTINUE
!
!               *******************************************
!               **  STEP 3.2--                           **
!               **  IF NO PRELIMINARY LIBRARY FUNCTION,  **
!               **  THEN COPY THE STRING BETWEEN         **
!               **  (BUT NOT INCLUDING) THE              **
!               **  PARENTHESES.                         **
!               **  IF A PRELIMINARY LIBRARY FUNCTION,   **
!               **  THEN COPY THE STRING                 **
!               **  STARTING WITH (AND INCLUDING)        **
!               **  THE PRELIMINARY  LIBRARY FUNCTION    **
!               **  AND STOPPING WITH (AND INCLUDING)    **
!               **  THE RIGHT PARENTHESIS.               **
!               *******************************************
!
      ISTEPN='3.2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(ILF.EQ.'YES')IMIN=ISTART-1
      IF(ILF.EQ.'YES')IMAX=ISTOP
      IF(ILF.EQ.'NO  ')IMIN=ISTART+1
      IF(ILF.EQ.'NO  ')IMAX=ISTOP-1
      J=0
      ITERM1(ILOOP)=IT2+1
      DO 6320 I=IMIN,IMAX
      J=J+1
      IT2=IT2+1
      ITER01(IT2)=IW21(I)
      ITER02(IT2)=IW22(I)
 6320 CONTINUE
      ITERM2(ILOOP)=IT2
!
!               ***************************************************
!               **  STEP 3.3--                                   **
!               **  TEMPORARILY COPY THE STRING WHICH IS BEYOND  **
!               **  THE RIGHT PARENTHESIS                        **
!               **  INTO IHOLD1(.).                               **
!               ***************************************************
!
      ISTEPN='3.3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      J=0
      ISTOP1=ISTOP+1
      IF(ISTOP1.GT.NW)GO TO 6349
      DO 6340 I=ISTOP1,NW
      J=J+1
      IHOLD1(J)=IW21(I)
      IHOLD2(J)=IW22(I)
      IHOLDT(J)=ITYPE(I)
 6340 CONTINUE
 6349 CONTINUE
      NREST=J
!
!               ********************************************
!               **  STEP 3.4--                            **
!               **  REPLACE THE EXTRACTED STRING BY       **
!               **  A $ AND THE LOOP NUMBER.              **
!               **  RETAIN OR DELETE PARENTHESES          **
!               **  DEPENDING ON WHETHER THE PRELIMINARY  **
!               **  OPERATION IS A LIBRARY FUNCTION       **
!               **  OR AN ARITHMETIC OPERATION.           **
!               ********************************************
!
      ISTEPN='3.4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
!CCCC J=IMIN-1
!CCCC J=J+1
      IF(ILF.EQ.'YES')J=ISTART-1
      IF(ILF.EQ.'NO  ')J=ISTART
      IW21(J)='$   '
      IW22(J)='    '
      ITYPE(J)='E   '
      J=J+1
      CALL DPC4IH(ILOOP,IW21(J),IBUGA3,IERROR)
      IW22(J)='    '
      ITYPE(J)='E   '
      IF(NREST.LE.0)GO TO 6390
      DO 6360 I=1,NREST
      J=J+1
      IW21(J)=IHOLD1(I)
      IW22(J)=IHOLD2(I)
      ITYPE(J)=IHOLDT(I)
 6360 CONTINUE
 6390 CONTINUE
      NW=J
      GO TO 6900
!
 6900 CONTINUE
!
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'RIV0')GO TO 6719
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6701)ILOOP
 6701 FORMAT('AFTER LOOP ',I8,'--  ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6709)NW
 6709 FORMAT('NW = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 6700 I=1,NW
      WRITE(ICOUT,6710)I,IW21(I),IW22(I),ITYPE(I)
 6710 FORMAT('I,IW21(I),IW22(I),ITYPE(I) = ',I8,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 6700 CONTINUE
 6719 CONTINUE
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'RIV0')GO TO 6799
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6791)ILOOP
 6791 FORMAT('AFTER LOOP ',I8,'--  ')
      CALL DPWRST('XXX','BUG ')
      IMIN=ITERM1(ILOOP)
      IMAX=ITERM2(ILOOP)
      NT=IMAX-IMIN+1
      WRITE(ICOUT,6792)ITERM1(ILOOP),ITERM2(ILOOP),NT
 6792 FORMAT('ITERM1(ILOOP),ITERM2(ILOOP),NT = ',3I8)
      CALL DPWRST('XXX','BUG ')
      DO 6795 I=IMIN,IMAX
      WRITE(ICOUT,6796)I,ITER01(I),ITER02(I)
 6796 FORMAT('I,ITER01(I),ITER02(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 6795 CONTINUE
 6799 CONTINUE
      IF(ISTART.LE.0)GO TO 5900
      ILOOP=ILOOP+1
      IF(ILOOP.LE.10000)GO TO 5310
!
 5900 CONTINUE
      NLOOP=ILOOP
!
!               ************************
!               **  STEP 4--          **
!               **  TAKE DERIVATIVES  **
!               ************************
!
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NWD=2
      ID1(1)='%   '
      ID2(1)='    '
!CCCC ID1(2)=NLOOP
      CALL DPC4IH(NLOOP,ID1(2),IBUGA3,IERROR)
      ID2(2)='    '
      IF(IBUG2.EQ.'ON')WRITE(ICOUT,710)NLOOP
  710 FORMAT('NLOOP = ',I8)
      IF(IBUG2.EQ.'ON')CALL DPWRST('XXX','BUG ')
!
      ILOOP=1
 7350 CONTINUE
      ISTEPN='7350'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')   &
      WRITE(ICOUT,881)ILOOP,NWD
  881 FORMAT('ILOOP,NWD = ',2I8)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')   &
      CALL DPWRST('XXX','BUG ')
      DO 7400 I=1,NWD
      I2=I
      IF(ID1(I).EQ.'%   '.AND.ID2(I).EQ.'    ')GO TO 7450
 7400 CONTINUE
      ISTOP=NWD+1
      ISTART=0
      GO TO 7790
 7450 CONTINUE
      ISTEPN='7450'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      ISTART=I2
      ISTOP=ISTART+1
!CCCC IF=ID1(ISTOP)
      CALL DPC4HI(ID1(ISTOP),IF,IBUGA3,IERROR)
      IF(IBUG2.EQ.'ON')WRITE(ICOUT,720)IF
  720 FORMAT('IF = ',I8)
      IF(IBUG2.EQ.'ON')CALL DPWRST('XXX','BUG ')
!
!               ******************************************
!               **  STEP 4.2--                          **
!               **  COPY OUT THE FUNCTION IN QUESTION   **
!               **  INTO A VECTOR FROM WHICH            **
!               **  THE DERIVATIVE WILL BE DETERMINED.  **
!               ******************************************
!
      ISTEPN='4.2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      J=0
      IMIN=ITERM1(IF)
      IMAX=ITERM2(IF)
      DO 740 I=IMIN,IMAX
      J=J+1
      IFUN01(J)=ITER01(I)
      IFUN02(J)=ITER02(I)
  740 CONTINUE
      NCF0=J
!
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'RIV0')GO TO 779
      WRITE(ICOUT,771)
  771 FORMAT('***** IN THE MIDDLE OF DERIV0 (IN STEP 4.2)--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,772)ILOOP
  772 FORMAT('      AT THE BEGINNING OF LOOP ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,773)
  773 FORMAT('      IMMEDIATELY PRIOR TO CALLING DERIV1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,774)NCF0
  774 FORMAT('NCF0 = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 775 I=1,NCF0
      WRITE(ICOUT,776)IFUN01(I),IFUN02(I)
  776 FORMAT('IFUN01(I),IFUN02(I) = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
  775 CONTINUE
  779 CONTINUE
!
!               ************************************
!               **  STEP 4.3--                    **
!               **  DETERMINE THE DERIVATIVE      **
!               **  OF THE FUNCTION UNDER STUDY.  **
!               ************************************
!
      ISTEPN='4.3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DERIV1(IFUN01,IFUN02,NCF0,   &
      IPARN1,IPARN2,NUMPAR,IVARN1,IVARN2,NUMVAR,   &
      ICONN,NUMCON,IEXPN,NUMEXP,   &
      IDER01,IDER02,NCD0,   &
      IBUGA3,ISUBRO,IFOUND,IERROR)
!
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'RIV0')GO TO 789
      WRITE(ICOUT,783)
  783 FORMAT('      IMMEDIATELY AFTER CALLING DERIV1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,784)NCD0
  784 FORMAT('NCD0 = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 785 I=1,NCD0
      WRITE(ICOUT,786)I,IDER01(I),IDER02(I)
  786 FORMAT('I,IDER01(I),IDER02(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
  785 CONTINUE
  789 CONTINUE
!
!               ***************************************************
!               **  STEP 4.4--                                   **
!               **  TEMPORARILY COPY THE STRING WHICH IS BEYOND  **
!               **  THE FUNCTION NUMBER                          **
!               **  INTO IHOLD1(.).                               **
!               ***************************************************
!
      ISTEPN='4.4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      J=0
      ISTOP1=ISTOP+1
      IF(ISTOP1.GT.NWD)GO TO 7249
      DO 7240 I=ISTOP1,NWD
      J=J+1
      IHOLD1(J)=ID1(I)
      IHOLD2(J)=ID2(I)
 7240 CONTINUE
 7249 CONTINUE
      NREST=J
!
!               *****************************************************
!               **  STEP 4.5--                                     **
!               **  REPLACE THE % AND THE FUNCTION NUMBER          **
!               **  (A SHORT-HAND DESIGNATION FOR THE DERIVATIVE)  **
!               **  BY THE FUNCTION'S DERIVATIVE.                  **
!               *****************************************************
!
      ISTEPN='4.5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      J=ISTART-1
      J=J+1
      ID1(J)='(   '
      ID2(J)='    '
      DO 7270 I=1,NCD0
      J=J+1
      ID1(J)=IDER01(I)
      ID2(J)=IDER02(I)
 7270 CONTINUE
      J=J+1
      ID1(J)=')   '
      ID2(J)='    '
      IF(NREST.LE.0)GO TO 7290
      DO 7280 I=1,NREST
      J=J+1
      ID1(J)=IHOLD1(I)
      ID2(J)=IHOLD2(I)
 7280 CONTINUE
 7290 CONTINUE
      NWD=J
!
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'RIV0')GO TO 799
      WRITE(ICOUT,792)ILOOP
  792 FORMAT('      AT THE END OF LOOP ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,794)NWD,ISTART,ILOOP
  794 FORMAT('NWD,ISTART,ILOOP = ',3I8)
      CALL DPWRST('XXX','BUG ')
      DO 795 I=1,NWD
      WRITE(ICOUT,796)I,ID1(I),ID2(I)
  796 FORMAT('I,ID1(I),ID2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
  795 CONTINUE
  799 CONTINUE
!
      IF(ISTART.LE.0)GO TO 7790
      ILOOP=ILOOP+1
      IF(ILOOP.LE.10000)GO TO 7350
 7790 CONTINUE
      ISTEPN='7790'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'RIV0')GO TO 7799
      WRITE(ICOUT,7792)
 7792 FORMAT('      AT THE END OF STEP 4 (AND 4.5)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7794)ILOOP,NWD
 7794 FORMAT('ILOOP,NWD = ',2I8)
      CALL DPWRST('XXX','BUG ')
      DO 7795 I=1,NWD
      WRITE(ICOUT,7796)I,ID1(I),ID2(I)
 7796 FORMAT('I,ID1(I),ID2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 7795 CONTINUE
 7799 CONTINUE
!
!               *****************************************
!               **  STEP 5--                           **
!               **  REPLACE THE FUNCTION DESIGNATIONS  **
!               **  BY THE FUNCTIONS                   **
!               *****************************************
!
      ILOOP=1
 8350 CONTINUE
!
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 8400 I=1,NWD
      I2=I
      IF(ID1(I).EQ.'$   '.AND.ID2(I).EQ.'    ')GO TO 8450
 8400 CONTINUE
      ISTOP=NWD+1
      ISTART=0
      GO TO 8790
 8450 CONTINUE
!
      ISTART=I2
      ISTOP=ISTART+1
!CCCC IF=ID1(ISTOP)
      CALL DPC4HI(ID1(ISTOP),IF,IBUGA3,IERROR)
!
!               ***************************************************
!               **  STEP 5.4--                                   **
!               **  TEMPORARILY COPY THE STRING WHICH IS BEYOND  **
!               **  THE FUNCTION NUMBER                          **
!               **  INTO IHOLD1(.).                               **
!               ***************************************************
!
      ISTEPN='5.4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      J=0
      ISTOP1=ISTOP+1
      IF(ISTOP1.GT.NWD)GO TO 8249
      DO 8240 I=ISTOP1,NWD
      J=J+1
      IHOLD1(J)=ID1(I)
      IHOLD2(J)=ID2(I)
 8240 CONTINUE
 8249 CONTINUE
      NREST=J
!
!               *************************************************
!               **  STEP 5.5--                                 **
!               **  REPLACE THE $ AND FUNCTION NUMBER          **
!               **  (A SHORT-HAND DESIGNATION FOR A FUNCTION)  **
!               **  BY THE FUNCTION.                           **
!               *************************************************
!
      ISTEPN='5.5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      J=ISTART-1
      J=J+1
      ID1(J)='(   '
      ID2(J)='    '
      IMIN=ITERM1(IF)
      IMAX=ITERM2(IF)
      DO 8270 I=IMIN,IMAX
      J=J+1
      ID1(J)=ITER01(I)
      ID2(J)=ITER02(I)
 8270 CONTINUE
      J=J+1
      ID1(J)=')   '
      ID2(J)='    '
      IF(NREST.LE.0)GO TO 8290
      DO 8280 I=1,NREST
      J=J+1
      ID1(J)=IHOLD1(I)
      ID2(J)=IHOLD2(I)
 8280 CONTINUE
 8290 CONTINUE
      NWD=J
!
      IF(ISTART.LE.0)GO TO 8790
      ILOOP=ILOOP+1
      IF(ILOOP.LE.10000)GO TO 8350
!
 8790 CONTINUE
!
!CCCC IF(IBUG51.EQ.'OFF')GO TO 8799
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'RIV0')GO TO 8799
      WRITE(ICOUT,8792)
 8792 FORMAT('      AT THE END OF STEP 5 (AND 5.5)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8794)NWD
 8794 FORMAT('NWD = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 8795 I=1,NWD
      WRITE(ICOUT,8796)I,ID1(I),ID2(I)
 8796 FORMAT('I,ID1(I),ID2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 8795 CONTINUE
 8799 CONTINUE
!
!               *****************************************
!               **  STEP 6--                           **
!               **  REPLACE THE CONSTANT DESIGNATIONS  **
!               **  BY THE CONSTANTS                   **
!               *****************************************
!
      ILOOP=1
 9350 CONTINUE
!
      ISTEPN='6'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 9400 I=1,NWD
      I2=I
      IF(ID1(I).EQ.'&   '.AND.ID2(I).EQ.'    ')GO TO 9450
 9400 CONTINUE
      ISTOP=NWD+1
      ISTART=0
      GO TO 9790
 9450 CONTINUE
!
      ISTART=I2
      ISTOP=ISTART+1
      CALL DPC4HI(ID1(ISTOP),IC,IBUGA3,IERROR)
!
!               ***************************************************
!               **  STEP 6.4--                                   **
!               **  TEMPORARILY COPY THE STRING WHICH IS BEYOND  **
!               **  THE CONSTANT NUMBER                          **
!               **  INTO IHOLD1(.).                               **
!               ***************************************************
!
      ISTEPN='6.4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      J=0
      ISTOP1=ISTOP+1
      IF(ISTOP1.GT.NWD)GO TO 9249
      DO 9240 I=ISTOP1,NWD
      J=J+1
      IHOLD1(J)=ID1(I)
      IHOLD2(J)=ID2(I)
 9240 CONTINUE
 9249 CONTINUE
      NREST=J
!
!               *************************************************
!               **  STEP 6.5--                                 **
!               **  REPLACE THE & AND CONSTANT NUMBER          **
!               **  (A SHORT-HAND DESIGNATION FOR A CONSTANT)  **
!               **  BY THE CONSTANT.                           **
!               *************************************************
!
      ISTEPN='6.5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')   &
      WRITE(ICOUT,9261)IC,ICON1(IC),ICON2(IC)
 9261 FORMAT('IC,ICON1(IC),ICON2(IC) = ',3I8)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')   &
      CALL DPWRST('XXX','BUG ')
!
      J=ISTART-1
      IMIN=ICON1(IC)
      IMAX=ICON2(IC)
      DO 9270 I=IMIN,IMAX
      J=J+1
      ID1(J)=ICON(I)
      ID2(J)='    '
 9270 CONTINUE
      IF(NREST.LE.0)GO TO 9290
      DO 9280 I=1,NREST
      J=J+1
      ID1(J)=IHOLD1(I)
      ID2(J)=IHOLD2(I)
 9280 CONTINUE
 9290 CONTINUE
      NWD=J
!
      IF(ISTART.LE.0)GO TO 9790
      ILOOP=ILOOP+1
      IF(ILOOP.LE.10000)GO TO 9350
 9790 CONTINUE
!
!               ****************
!               **  STEP 90-- **
!               **  EXIT.     **
!               ****************
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RIV0')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DERIV0--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)NWD
 9012   FORMAT('NWD = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,NWD
          WRITE(ICOUT,9016)I,ID1(I),ID2(I)
 9016     FORMAT('I,ID1(I),ID2(I) = ',I8,2X,A4,2X,A4)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DERIV0
      SUBROUTINE DERIV1(IFUN01,IFUN02,NCF0,   &
                        IPARN1,IPARN2,NUMPAR,IVARN1,IVARN2,NUMVAR,   &
                        ICONN,NUMCON,IEXPN,NEXP,   &
                        IDER01,IDER02,NCD0,   &
                        IBUGA3,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--DETERMINE THE DERIVATIVE OF AN
!              EXPRESSION WHICH HAS NO PARENTHESES
!              UNLESS THEY ARE AFTER A
!              LIBRARY FUNCTION, AND WHICH
!              MAY HAVE +, -, *, /, **).
!
!              THE INPUT EXPRESSION IS IN THE
!              VECTOR IFUN01(.) (FOR FIRST 4 CHARACTERS) AND
!              VECTOR IFUN02(.) (FOR NEXT  4 CHARACTERS)--IT HAS
!              LENGTH (= NUMBER OF CHARACTERS) NCF.
!
!              THE OUTPUT EXPRESSION WILL BE IN
!              VECTOR IDER01(.) (FOR FIRST 4 CHARACTERS) AND
!              VECTOR IDER02(.) (FOR NEXT  4 CHARACTERS)--IT HAS
!              HAVE LENGTH (= NUMBER OF CHARACTERS) NCD.
!
!     INPUT  ARGUMENTS--IFUN01 = THE VECTOR
!                                WHICH CONTAINS THE EXPRESSION
!                                OF INTEREST
!                                (FIRST 4 CHARACTERS).
!                     --IFUN02 = THE VECTOR
!                                WHICH CONTAINS THE EXPRESSION
!                                OF INTEREST
!                                (NEXT 4 CHARACTERS).
!                     --NCF0   = AN INTEGER NUMBER
!                                OF CHARACTERS IN IFUN01.
!     OUTPUT ARGUMENTS--IDER01 = THE VECTOR
!                                WHICH CONTAINS THE DERIVATIVE
!                                OF THE EXPRESSION OF INTEREST
!                                (FIRST 4 CHARACTERS).
!                     --IDER02 = THE VECTOR
!                                WHICH CONTAINS THE DERIVATIVE
!                                OF THE EXPRESSION OF INTEREST
!                                (NEXT  4 CHARACTERS).
!                     --NCD0   = AN INTEGER NUMBER
!                                OF CHARACTERS IN IDER01.
!
!     ORIGINAL VERSION--DECEMBER 8, 1978
!     UPDATED         --DECEMBER  1981.
!
!---------------------------------------------------------------------
!
      CHARACTER*4 IFUN01
      CHARACTER*4 IFUN02
      CHARACTER*4 IPARN1
      CHARACTER*4 IPARN2
      CHARACTER*4 IVARN1
      CHARACTER*4 IVARN2
      CHARACTER*4 ICONN
      CHARACTER*4 IEXPN
      CHARACTER*4 IDER01
      CHARACTER*4 IDER02
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!CCCC CHARACTER*4 IBUG1
!CCCC CHARACTER*4 IBUG2
!CCCC CHARACTER*4 IBUG3
!
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
      CHARACTER*4 IFUN11
      CHARACTER*4 IFUN12
      CHARACTER*4 IDER11
      CHARACTER*4 IDER12
!
      DIMENSION IFUN01(*)
      DIMENSION IFUN02(*)
      DIMENSION IDER01(*)
      DIMENSION IDER02(*)
!
      DIMENSION IPARN1(*)
      DIMENSION IPARN2(*)
      DIMENSION IVARN1(*)
      DIMENSION IVARN2(*)
      DIMENSION ICONN(*)
      DIMENSION IEXPN(*)
      DIMENSION IFUN11(20,80)
      DIMENSION IFUN12(20,80)
      DIMENSION NCF1(20)
      DIMENSION IDER11(20,80)
      DIMENSION IDER12(20,80)
      DIMENSION NCD1(20)
!
!-----COMMON VARIABLES (GENERAL)-----------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-----------------------------------------------------
!
!CCCC DATA IBUG1/'OFF'/
!CCCC DATA IBUG2/'OFF'/
!CCCC DATA IBUG3/'OFF'/
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DERI'
      ISUBN2='V1  '
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RIV1')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('AT THE BEGINNING OF DERIV1--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)NCF0,NEXP
   52   FORMAT('NCF0,NEXP = ',2I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,NCF0
          WRITE(ICOUT,56)I,IFUN01(I),IFUN02(I)
   56     FORMAT('I,IFUN01(I),IFUN02(I) = ',I8,2(2X,A4))
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ********************************************************
!               **  STEP 2--                                          **
!               **  EXTRACT EACH ADDITIVE SUBSTRING FROM IFUN01(.).     **
!               **  A SUBSTRING IS ADDITIVE IF SEPARATED              **
!               **  FROM OTHER SUBSTRINGS BY A    +   OR    -   .     **
!               **  PLACE THE I-TH SUBSTRING IN ROW I OF IFUN11(.,.).  **
!               **  DETERMINE THE NUMBER OF CHARACTERS IN             **
!               **  EACH SUBSTRING.  THE NUMBER OF CHARACTERS         **
!               **  IN THE I-TH SUBSTRING WILL BE PLACED              **
!               **  IN NCF1(I).                                       **
!               **  DETERMINE THE TOTAL NUMBER OF SUBSTRINGS.         **
!               **  THIS NUMBER WILL BE PLACED IN NFUN1.              **
!               ********************************************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV1')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NFUN1=0
      JMIN=1
      DO 400 I=1,NCF0
      I2=I
      IF(IFUN01(I).EQ.'+   '.AND.IFUN02(I).EQ.'    ')GO TO 420
      IF(IFUN01(I).EQ.'-   '.AND.IFUN02(I).EQ.'    ')GO TO 420
      GO TO 400
  420 CONTINUE
!
      JMAX=I2-1
      IF(JMAX.LT.JMIN)GO TO 400
!
      NFUN1=NFUN1+1
      K=0
      IF(IFUN01(JMIN).EQ.'+   '.AND.IFUN02(JMIN).EQ.'    ')GO TO 440
      IF(IFUN01(JMIN).EQ.'-   '.AND.IFUN02(JMIN).EQ.'    ')GO TO 440
      K=K+1
      IFUN11(NFUN1,K)='+   '
      IFUN12(NFUN1,K)='    '
  440 CONTINUE
!
      DO 450 J=JMIN,JMAX
      K=K+1
      IFUN11(NFUN1,K)=IFUN01(J)
      IFUN12(NFUN1,K)=IFUN02(J)
  450 CONTINUE
      NCF1(NFUN1)=K
      JMIN=I
  400 CONTINUE
!
      JMAX=NCF0
      NFUN1=NFUN1+1
      K=0
      IF(IFUN01(JMIN).EQ.'+   '.AND.IFUN02(JMIN).EQ.'    ')GO TO 540
      IF(IFUN01(JMIN).EQ.'-   '.AND.IFUN02(JMIN).EQ.'    ')GO TO 540
      K=K+1
      IFUN11(NFUN1,K)='+   '
      IFUN12(NFUN1,K)='    '
  540 CONTINUE
!
      DO 550 J=JMIN,JMAX
      K=K+1
      IFUN11(NFUN1,K)=IFUN01(J)
      IFUN12(NFUN1,K)=IFUN02(J)
  550 CONTINUE
      NCF1(NFUN1)=K
!
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'RIV1')GO TO 790
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,701)
  701 FORMAT('IN THE MIDDLE OF DERIV1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,702)NCD0
  702 FORMAT('NCD0 = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 705 I=1,NCD0
      WRITE(ICOUT,706)I,IDER01(I),IDER02(I)
  706 FORMAT('I,IDER01(I),IDER02(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
  705 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,709)NFUN1
  709 FORMAT('NFUN1 = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 710 IF1=1,NFUN1
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,712)IF1
  712 FORMAT('IF1 = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,713)NCD1(IF1)
  713 FORMAT('NCD1(IF1) = ',I8)
      CALL DPWRST('XXX','BUG ')
      JMAX=NCD1(IF1)
      DO 715 J=1,JMAX
      WRITE(ICOUT,716)J,IDER11(IF1,J),IDER12(IF1,J)
  716 FORMAT('J,IDER11(IF1,J),IDER12(IF1,J) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
  715 CONTINUE
  710 CONTINUE
  790 CONTINUE
!
!               *************************************************
!               **  STEP 3--                                   **
!               **  OPERATE ON EACH ADDITIVE COMPONENT         **
!               **  DETERMINE THE DERIVATIVE OF EACH ADDITIVE  **
!               **  COMPONENT.                                 **
!               *************************************************
!
      DO 1000 IROW1=1,NFUN1
!
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV1')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DERIV2(IFUN11,IFUN12,NCF1,IROW1,   &
      IPARN1,IPARN2,NUMPAR,IVARN1,IVARN2,NUMVAR,   &
      ICONN,NUMCON,IEXPN,NUMEXP,IDER11,IDER12,NCD1,   &
      IBUGA3,ISUBRO,IFOUND,IERROR)
 1000 CONTINUE
!
!               ***************************************
!               **  STEP 4--                         **
!               **  COMBINE EACH ADDITIVE COMPONENT  **
!               **  INTO ONE LONG STRING             **
!               **  SO AS TO FORM THE DERIVATIVE     **
!               **  FOR THE ENTIRE EXPRESSION.       **
!               ***************************************
!
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV1')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      K=0
      DO 2000 IROW1=1,NFUN1
      JMAX=NCD1(IROW1)
      IF(JMAX.LE.0)GO TO 2000
      IF(JMAX.EQ.1.AND.   &
      IDER11(IROW1,1).EQ.'0    '.AND.IDER12(IROW1,1).EQ.'    ')GO TO 2000
      DO 2100 J=1,JMAX
      K=K+1
      IDER01(K)=IDER11(IROW1,J)
      IDER02(K)=IDER12(IROW1,J)
 2100 CONTINUE
      IF(IROW1.EQ.NFUN1)GO TO 2000
      K=K+1
      IDER01(K)='+   '
      IDER02(K)='    '
 2000 CONTINUE
      IF(K.GE.1.AND.   &
      IDER01(K).EQ.'+   '.AND.IDER02(K).EQ.'    ')K=K-1
      IF(K.LE.0)GO TO 2150
      GO TO 2190
 2150 CONTINUE
      K=1
      IDER01(K)='0   '
      IDER02(K)='    '
 2190 CONTINUE
      NCD0=K
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RIV1')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('AT THE END       OF DERIV1--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)NFUN1,IF1,NCD1(IF1),NCD0
 9012   FORMAT('NFUN1,IF1,NCD1(IF1),NCD0 = ',4I8)
        CALL DPWRST('XXX','BUG ')
        DO 9015 IF1=1,NFUN1
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          JMAX=NCD1(IF1)
          DO 9020 J=1,JMAX
            WRITE(ICOUT,9021)J,IDER11(IF1,J),IDER12(IF1,J)
 9021       FORMAT('J,IDER11(IF1,J),IDER12(IF1,J) = ',I8,2(2X,A4))
            CALL DPWRST('XXX','BUG ')
 9020     CONTINUE
 9015   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        DO 9035 I=1,NCD0
          WRITE(ICOUT,9036)I,IDER01(I),IDER02(I)
 9036     FORMAT('I,IDER01(I),IDER02(I) = ',I8,2(2X,A4))
          CALL DPWRST('XXX','BUG ')
 9035   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DERIV1
      SUBROUTINE DERIV2(IFUN11,IFUN12,NCF1,IROW1,   &
      IPARN1,IPARN2,NUMPAR,IVARN1,IVARN2,NUMVAR,   &
      ICONN,NUMCON,IEXPN,NUMEXP,IDER11,IDER12,NCD1,   &
      IBUGA3,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--DETERMINE THE DERIVATIVE OF
!              A MULTIPLICATIVE EXPRESSION
!              (= 1 FULL ADDITIVE COMPONENT)
!              (EXAMPLE, A*X/C*D**E*X)
!
!              THE ENTIRE INPUT EXPRESSION IS LOCATED
!              IN ROW IROW1 OF IFUN11--
!              IT HAS LENGTH NF1
!
!              THE OUTPUT DERIVATIVE IS LOCATED
!              IN ROW IROW1 OF IFUN11--
!              IT HAS LENGTH NCD1.
!
!     INPUT  ARGUMENTS--IFUN11 = THE ARRAY WHOSE IROW1-TH ROW
!                                IS THE IROW1-TH ADDITIVE COMPONENT
!                                OF INTEREST
!                                (FIRST 4 CHARACTERS).
!                     --IFUN12 = THE ARRAY WHOSE IROW1-TH ROW
!                                IS THE IROW1-TH ADDITIVE COMPONENT
!                                OF INTEREST
!                                (NEXT  4 CHARACTERS).
!                     --NCF1   = AN INTEGER VECTOR
!                                WHOSE IROW1-TH ELEMENT
!                                IS THE LENGTH OF THE IROW1-TH
!                                STRING IN IFUN11(.,.);
!                                THAT IS, NCF1(IROW1) = THE LENGTH OF THE
!                                ADDITIVE COMPONENT OF INTEREST.
!                     --IROW1  = THE ROW NUMBER (IN IFUN11(.,.)) OF
!                                THE PARTICULAR
!                                ADDITIVE COMPONENT OF INTEREST.
!                     --IPARN1 = THE HOLLARITH VECTOR
!                                OF PARAMETER NAMES
!                                (FIRST 4 CHARACTERS).
!                     --IPARN2 = THE HOLLARITH VECTOR
!                                OF PARAMETER NAMES
!                                (NEXT  4 CHARACTERS).
!                     --NUMPAR = THE INTEGER NUMBER
!                                OF PARAMETERS.
!                     --IVARN1 = THE HOLLARITH VECTOR
!                                OF VARIABLE NAMES
!                                (FIRST 4 CHARACTERS).
!                     --IVARN2 = THE HOLLARITH VECTOR
!                                OF VARIABLE NAMES
!                                (NEXT  4 CHARACTERS).
!                     --NUMVAR = THE INTEGER NUMBER
!                                OF VARIABLE NAMES.
!                     --ICONN  = THE HOLLARITH VECTOR
!                                OF CONSTANT NAMES.
!                     --NUMCON = THE INTEGER NUMBER
!                                OF CONSTANTS.
!                     --IEXPN  = THE HOLLARITH VECTOR
!                                OF EXPRESSION NAMES.
!                     --NUMEXP = THE INTEGER NUMBER
!                                OF EXPRESSION NAMES.
!     OUTPUT ARGUMENTS--IDER11 = THE ARRAY WHOSE IROW1-TH R
!                                WILL BE THE DERIVATIVE OF THE
!                                IROW1-TH ADDITIVE STRING
!                                (FIRST 4 CHARACTERS).
!                     --IDER12 = THE ARRAY WHOSE IROW1-TH R
!                                WILL BE THE DERIVATIVE OF THE
!                                IROW1-TH ADDITIVE STRING
!                                (NEXT  4 CHARACTERS).
!                       NCD1   = AN INTEGER VECTOR
!                                WHOSE IROW1-TH ELEMENT
!                                WILL BE THE LENGTH OF THE IROW1-TH
!                                DERIVATIVE IN IDER1(.,.);
!                                THAT IS, NCD1(IROW1) = THE LENGTH OF THE
!                                DERIVATIVE OF INTEREST.
!     INTERNAL ARRAYS--
!                     --IFUN21 = THE ARRAY WHOSE I-TH
!                                ROW WILL BE THE I-TH MULTIPLICATIVE
!                                SUBSTRING OF THE IROW1-TH
!                                ADDITIVE COMPONENT
!                                (FIRST 4 CHARACTERS).
!                     --IFUN22 = THE ARRAY WHOSE I-TH
!                                ROW WILL BE THE I-TH MULTIPLICATIVE
!                                SUBSTRING OF THE IROW1-TH
!                                ADDITIVE COMPONENT
!                                (NEXT  4 CHARACTERS).
!                       NCF2   = AN INTEGER VECTOR
!                                WHOSE I-TH ELEMENT
!                                WILL BE THE LENGTH OF THE I-TH
!                                MULTIPLICATIVE SUBSTRING
!                                OF THE IROW1-TH ADDITIVE COMPONENT.
!                       NFUN2  = THE NUMBER OF ROWS
!                                (= THE NUMBER OF MULTIPLICATIVE
!                                SUBSTRINGS OF THE IROW1-TH
!                                ADDITIVE COMPONENT)
!                                THAT WILL BE
!                                IN THE ARRAY IFUN21(.,.)
!                       IOP2   = A VECTOR
!                                WHOSE I-TH ELEMENT
!                                WILL BE THE (TRAILING) OPERATION (* OR /)
!                                OF THE I-TH MULTIPLICATIVE SUBSTRING
!                                OF THE IROW1-TH ADDITIVE COMPONENT.
!
!     ORIGINAL VERSION--DECEMBER 2, 1978
!     UPDATED         --DECEMBER  1981.
!
!---------------------------------------------------------------------
!
      CHARACTER*4 IFUN11
      CHARACTER*4 IFUN12
      CHARACTER*4 IPARN1
      CHARACTER*4 IPARN2
      CHARACTER*4 IVARN1
      CHARACTER*4 IVARN2
      CHARACTER*4 ICONN
      CHARACTER*4 IEXPN
      CHARACTER*4 IDER11
      CHARACTER*4 IDER12
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
      CHARACTER*4 IFUN21
      CHARACTER*4 IFUN22
      CHARACTER*4 IDER21
      CHARACTER*4 IDER22
      CHARACTER*4 IOP2
!
!CCCC CHARACTER*4 IBUG1
!CCCC CHARACTER*4 IBUG2
!CCCC CHARACTER*4 IBUG3
!
      DIMENSION IFUN11(20,80)
      DIMENSION IFUN12(20,80)
      DIMENSION NCF1(*)
      DIMENSION IPARN1(*)
      DIMENSION IPARN2(*)
      DIMENSION IVARN1(*)
      DIMENSION IVARN2(*)
      DIMENSION ICONN(*)
      DIMENSION IEXPN(*)
      DIMENSION IDER11(20,80)
      DIMENSION IDER12(20,80)
      DIMENSION NCD1(*)
!
      DIMENSION IFUN21(20,80)
      DIMENSION IFUN22(20,80)
      DIMENSION NCF2(20)
      DIMENSION IDER21(20,80)
      DIMENSION IDER22(20,80)
      DIMENSION NCD2(20)
      DIMENSION IOP2(20)
!
!-----COMMON VARIABLES (GENERAL)-----------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-----------------------------------------------------
!
!CCCC DATA IBUG1/'OFF'/
!CCCC DATA IBUG2/'OFF'/
!CCCC DATA IBUG3/'OFF'/
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DERI'
      ISUBN2='V2  '
      IERROR='NO'
!
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'RIV2')GO TO 90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('AT THE BEGINNING OF DERIV2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGA3,IFOUND,IERROR
   52 FORMAT('IBUGA3,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IROW1
   53 FORMAT('IROW1 = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)NCF1(IROW1)
   54 FORMAT('NCF1(IROW1) = ',I8)
      CALL DPWRST('XXX','BUG ')
      ITEMP=NCF1(IROW1)
      DO 61 J=1,ITEMP
      WRITE(ICOUT,62)J,IFUN11(IROW1,J),IFUN12(IROW1,J)
   62 FORMAT('J,IFUN11(IROW1,J),IFUN12(IROW1,J) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   61 CONTINUE
   90 CONTINUE
!
!               ********************************************************
!               **  STEP 1--                                          **
!               **  EXTRACT EACH MULTIPLICATIVE SUBSTRING.            **
!               **  A SUBSTRING IS MULTIPLICATIVE IF SEPARATED        **
!               **  FROM OTHER SUBSTRINGS BY A    *   OR    /   .     **
!               **  PLACE THE I-TH SUBSTRING IN ROW I OF IFUN21(.,.).  **
!               **  DETERMINE THE NUMBER OF CHARACTERS IN             **
!               **  EACH SUBSTRING.  THE NUMBER OF CHARACTERS         **
!               **  IN THE I-TH SUBSTRING WILL BE PLACED              **
!               **  IN NCF2(I).                                       **
!               **  DETERMINE THE TOTAL NUMBER OF SUBSTRINGS.         **
!               **  THIS NUMBER WILL BE PLACED IN NFUN2.              **
!               ********************************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NFUN2=0
      JMIN=1
      IMIN=1
      IMAX=NCF1(IROW1)
      DO 400 I=IMIN,IMAX
      IF(IFUN11(IROW1,I).EQ.'*   '.AND.IFUN12(IROW1,I).EQ.'    ')GO TO 420
      IF(IFUN11(IROW1,I).EQ.'/   '.AND.IFUN12(IROW1,I).EQ.'    ')GO TO 420
      GO TO 400
  420 CONTINUE
!
      JMAX=I-1
      IF(JMAX.LT.JMIN)GO TO 430
      GO TO 440
  430 CONTINUE
!
      WRITE(ICOUT,431)
  431 FORMAT('*****ERROR IN DERIV2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,432)
  432 FORMAT('JMAX GREATER THAN JMIN')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,433)JMIN,JMAX
  433 FORMAT('JMIN,JMAX = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
  440 CONTINUE
!
      NFUN2=NFUN2+1
      K=0
      DO 450 J=JMIN,JMAX
      K=K+1
      IFUN21(NFUN2,K)=IFUN11(IROW1,J)
      IFUN22(NFUN2,K)=IFUN12(IROW1,J)
  450 CONTINUE
      NCF2(NFUN2)=K
      IOP2(NFUN2)=IFUN11(IROW1,I)
      JMIN=I+1
  400 CONTINUE
!
      JMAX=IMAX
      IF(JMAX.LT.JMIN)GO TO 530
      GO TO 540
  530 CONTINUE
!
      WRITE(ICOUT,531)
  531 FORMAT('*****ERROR IN DERIV2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,532)
  532 FORMAT('JMAX GREATER THAN JMIN')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,533)JMIN,JMAX
  533 FORMAT('JMIN,JMAX = ',2I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
  540 CONTINUE
!
      NFUN2=NFUN2+1
      K=0
      DO 550 J=JMIN,JMAX
      K=K+1
      IFUN21(NFUN2,K)=IFUN11(IROW1,J)
      IFUN22(NFUN2,K)=IFUN12(IROW1,J)
  550 CONTINUE
      NCF2(NFUN2)=K
!
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'RIV2')GO TO 690
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,601)
  601 FORMAT('AFTER STEP 1 OF DERIV2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,610)NFUN2
  610 FORMAT('NFUN2 = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 615 I=1,NFUN2
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,616)I
  616 FORMAT('I = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,617)NCF2(I)
  617 FORMAT('NCF2(I) = ',I8)
      CALL DPWRST('XXX','BUG ')
      ITEMP=NCF2(I)
      DO 620 J=1,ITEMP
      WRITE(ICOUT,621)I,J,IFUN21(I,J),IFUN22(I,J)
  621 FORMAT('I,J,IFUN21(I,J),IFUN22(I,J) = ',I8,I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
  620 CONTINUE
  615 CONTINUE
  690 CONTINUE
!
!               *******************************************************
!               **  STEP 2--                                         **
!               **  OPERATE ON EACH MULTIPLICATIVE COMPONENT.        **
!               **  DETERMINE THE DERIVATIVE OF EACH MULTIPLICATIVE  **
!               **  COMPONENT.                                       **
!               *******************************************************
!
      DO 700 IROW2=1,NFUN2
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DERIV3(IFUN21,IFUN22,NCF2,IROW2,   &
      IPARN1,IPARN2,NUMPAR,IVARN1,IVARN2,NUMVAR,   &
      ICONN,NUMCON,IEXPN,NUMEXP,IDER21,IDER22,NCD2,   &
      IBUGA3,ISUBRO,IFOUND,IERROR)
  700 CONTINUE
!
!               ****************************************
!               **  STEP 3--                          **
!               **  COMBINE MULTIPLICATIVE COMPONENT  **
!               **  DERIVATIVES TO DETERMINE THE      **
!               **  DERIVATIVE OF THE IROW1-TH        **
!               **  (IROW1 FIXED) ADDITIVE COMPONENT. **
!               ****************************************
!
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      CALL DERIV4(IFUN21,IFUN22,NCF2,NFUN2,   &
      IDER21,IDER22,NCD2,IOP2,IROW1,   &
      IDER11,IDER12,NCD1,   &
      IBUGA3,ISUBRO,IFOUND,IERROR)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'RIV2')GO TO 9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('AT THE END       OF DERIV2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGA3,IFOUND,IERROR
 9012 FORMAT('IBUGA3,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IROW1
 9013 FORMAT('IROW1 = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)NCD1(IROW1)
 9014 FORMAT('NCD1(IROW1) = ',I8)
      CALL DPWRST('XXX','BUG ')
      ITEMP=NCD1(IROW1)
      DO 9021 J=1,ITEMP
      WRITE(ICOUT,9022)J,IDER11(IROW1,J),IDER12(IROW1,J)
 9022 FORMAT('J,IDER11(IROW1,J),IDER12(IROW1,J) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9021 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DERIV2
      SUBROUTINE DERIV3(IFUN21,IFUN22,NCF2,IROW2,   &
                         IPARN1,IPARN2,NUMPAR,IVARN1,IVARN2,NUMVAR,   &
                         ICONN,NUMCON,IEXPN,NUMEXP,IDER21,IDER22,NCD2,   &
                         IBUGA3,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--DETERMINE THE DERIVATIVE OF
!              AN ELEMENTAL COMPONENT
!              (EXAMPLE, X, OR X**B, OR -X, OR -X**X)
!              WHICH IS A COMPONENT THAT HAS
!              NO +, -, *, OR /.
!              IT MAY HAVE ** (AS IN A**B).
!              IT MAY HAVE A SIGN (OR NO SIGN).
!              IT MAY BE ONLY A SINGLE ELEMENT.
!
!              THE INPUT ELEMENT IS LOCATED
!              IN ROW IROW2 OF IFUN21--
!              IT HAS LENGTH NF2.
!
!              THE OUTPUT DERIVATIVE IS LOCATED
!              IN ROW IROW2 OF IFUN21--
!              IT HAS LENGTH NCD2.
!
!     INPUT  ARGUMENTS--IFUN21 = THE ARRAY WHOSE IROW2-TH ROW
!                                IS THE IROW2-TH ELEMENTAL COMPONENT
!                                OF INTEREST
!                                (FIRST 4 CHARACTERS).
!                     --IFUN22 = THE ARRAY WHOSE IROW2-TH ROW
!                                IS THE IROW2-TH ELEMENTAL COMPONENT
!                                OF INTEREST
!                                (NEXT  4 CHARACTERS).
!                     --NCF2   = AN INTEGER VECTOR
!                                WHOSE IROW2-TH ELEMENT
!                                IS THE LENGTH OF THE IROW2-TH
!                                STRING IN IFUN21(.,.);
!                                THAT IS, NCF2(IROW2) = THE LENGTH OF THE
!                                ELEMENTAL COMPONENT OF INTEREST.
!                     --IROW2  = THE ROW NUMBER (IN IFUN21(.,.)) OF
!                                THE PARTICULAR
!                                ELEMENTAL COMPONENT OF INTEREST.
!                     --IPARN1 = THE HOLLARITH VECTOR
!                                OF PARAMETER NAMES
!                                (FIRST 4 CHARACTERS).
!                     --IPARN2 = THE HOLLARITH VECTOR
!                                OF PARAMETER NAMES
!                                (NEXT  4 CHARACTERS).
!                     --NUMPAR = THE INTEGER NUMBER
!                                OF PARAMETERS.
!                     --IVARN1 = THE HOLLARITH VECTOR
!                                OF VARIABLE NAMES
!                                (FIRST 4 CHARACTERS).
!                     --IVARN2 = THE HOLLARITH VECTOR
!                                OF VARIABLE NAMES
!                                (NEXT  4 CHARACTERS).
!                     --NUMVAR = THE INTEGER NUMBER
!                                OF VARIABLE NAMES.
!                     --ICONN  = THE HOLLARITH VECTOR
!                                OF CONSTANT NAMES.
!                     --NUMCON = THE INTEGER NUMBER
!                                OF CONSTANTS.
!                     --IEXPN  = THE HOLLARITH VECTOR
!                                OF EXPRESSION NAMES.
!                     --NUMEXP = THE INTEGER NUMBER
!                                OF EXPRESSION NAMES.
!     OUTPUT ARGUMENTS--IDER21 = THE ARRAY WHOSE IROW2-TH ROW
!                                WILL BE THE DERIVATIVE OF THE
!                                IROW2-TH ELEMENTAL STRING
!                                (FIRST 4 CHARACTERS).
!                     --IDER22 = THE ARRAY WHOSE IROW2-TH ROW
!                                WILL BE THE DERIVATIVE OF THE
!                                IROW2-TH ELEMENTAL STRING
!                                (NEXT  4 CHARACTERS).
!                     --NCD2   = AN INTEGER VECTOR
!                                WHOSE IROW2-TH ELEMENT
!                                WILL BE THE LENGTH OF THE IROW2-TH
!                                DERIVATIVE IN IDER21(.,.);
!                                THAT IS, NCD2(IROW2) = THE LENGTH OF THE
!                                DERIVATIVE OF INTEREST.
!
!     DATE--DECEMBER 9, 1978
!
!---------------------------------------------------------------------
!
      CHARACTER*4 IFUN21
      CHARACTER*4 IFUN22
      CHARACTER*4 IPARN1
      CHARACTER*4 IPARN2
      CHARACTER*4 IVARN1
      CHARACTER*4 IVARN2
      CHARACTER*4 ICONN
      CHARACTER*4 IEXPN
      CHARACTER*4 IDER21
      CHARACTER*4 IDER22
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IFUNZ1
      CHARACTER*4 IFUNZ2
      CHARACTER*4 IDERZ1
      CHARACTER*4 IDERZ2
!
!CCCC CHARACTER*4 IBUG1
!CCCC CHARACTER*4 IBUG2
!CCCC CHARACTER*4 IBUG3
!
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ITYPE
      CHARACTER*4 IMANTT
      CHARACTER*4 IEXPT
      CHARACTER*4 ISIGN1
      CHARACTER*4 ISIGN2
      CHARACTER*4 IH1
      CHARACTER*4 IH2
      CHARACTER*4 IHLF1
      CHARACTER*4 IHLF2
      CHARACTER*4 IMAN11
      CHARACTER*4 IMAN12
      CHARACTER*4 IMAN21
      CHARACTER*4 IMAN22
      CHARACTER*4 IEXP11
      CHARACTER*4 IEXP12
      CHARACTER*4 IEXP21
      CHARACTER*4 IEXP22
!
      CHARACTER*4 IHOL11
      CHARACTER*4 IHOL12
      CHARACTER*4 IHOL21
      CHARACTER*4 IHOL22
!
      DIMENSION IFUN21(20,80)
      DIMENSION IFUN22(20,80)
      DIMENSION NCF2(*)
      DIMENSION IPARN1(*)
      DIMENSION IPARN2(*)
      DIMENSION IVARN1(*)
      DIMENSION IVARN2(*)
      DIMENSION ICONN(*)
      DIMENSION IEXPN(*)
      DIMENSION IDER21(20,80)
      DIMENSION IDER22(20,80)
      DIMENSION NCD2(*)
!
      DIMENSION IFUNZ1(300)
      DIMENSION IFUNZ2(300)
      DIMENSION IDERZ1(300)
      DIMENSION IDERZ2(300)
!
!
!-----COMMON VARIABLES (GENERAL)-----------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DERI'
      ISUBN2='V3  '
!
      IERROR='NO'
      ITYPE='NULL'
      IMANTT='NULL'
      IEXPT='NULL'
      ISIGN1='NULL'
      ISIGN2='    '
      IFOUND='YES'
      IEXP11='    '
      IEXP12='    '
      IEXP21='    '
      IEXP22='    '
      IMAN21='    '
      IMAN22='    '
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RIV3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('AT THE BEGINNING OF DERIV3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICONN(1),IEXPN(1),NUMCON,IROW2,NCF2(IROW2)
   52   FORMAT('ICONN(1),IEXPN(1),NUMCON,IROW2,NCF2(IROW2) = ',   &
               2(A4,2X),3I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 J=1,NCF2(IROW2)
          WRITE(ICOUT,56)J,IFUN21(IROW2,J),IFUN22(IROW2,J)
   56     FORMAT('J,IFUN21(IROW2,J),IFUN22(IROW2,J) = ',I8,2(2X,A4))
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
        WRITE(ICOUT,71)NUMPAR,NUMVAR,NUMEXP
   71   FORMAT('NUMPAR,NUMVAR,NUMEXP = ',3I8)
        CALL DPWRST('XXX','BUG ')
        DO 62 I=1,NUMPAR
          WRITE(ICOUT,63)I,IPARN1(I),IPARN2(I)
   63     FORMAT('I,IPARN1(I),IPARN2(I) = ',I8,2(2X,A4))
         CALL DPWRST('XXX','BUG ')
   62   CONTINUE
        DO 72 I=1,NUMVAR
          WRITE(ICOUT,73)I,IVARN1(I),IVARN2(I)
   73     FORMAT('I,IVARN1(I),IVARN2(I) = ',I8,2(2X,A4))
          CALL DPWRST('XXX','BUG ')
   72   CONTINUE
      ENDIF
!
!               **********************************
!               **  STEP 1--                    **
!               **  COPY THE EXPRESSION         **
!               **  IN ROW IROW2 OF IFUN21(.,.) **
!               **  INTO THE VECTOR IFUNZ1(.).  **
!               **********************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NCFZ=NCF2(IROW2)
      DO 300 I=1,NCFZ
      IFUNZ1(I)=IFUN21(IROW2,I)
      IFUNZ2(I)=IFUN22(IROW2,I)
      IDERZ1(I)='OOOO'
      IDERZ2(I)='OOOO'
      IDER21(IROW2,I)='OOOO'
      IDER22(IROW2,I)='OOOO'
  300 CONTINUE
!
!               ***************************************
!               **  STEP 2--                         **
!               **  SEARCH FOR A LEFT PARENTHESIS--  **
!               **  THIS WILL INDICATE A PRECEDING   **
!               **  LIBRARY FUNCTION.                **
!               ***************************************
!
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 310 I=1,NCFZ
      I1=I
      IF(IFUNZ1(I).EQ.'(   '.AND.IFUNZ2(I).EQ.'    ')GO TO 320
  310 CONTINUE
      GO TO 3000
  320 CONTINUE
      I1M1=I1-1
      I1P1=I1+1
      I1P2=I1+2
      I1P3=I1+3
      IHLF1=IFUNZ1(I1M1)
      IHLF2=IFUNZ2(I1M1)
      IH1=IFUNZ1(I1P1)
      IH2=IFUNZ2(I1P1)
!
      IF(IH1.EQ.'$   '.AND.IH2.EQ.'    ')GO TO 330
      GO TO 339
  330 CONTINUE
      ITYPE='EXP '
      GO TO 380
  339 CONTINUE
!
      IF(IH1.EQ.'&   '.AND.IH2.EQ.'    ')GO TO 340
      GO TO 349
  340 CONTINUE
      I2=1
      IDERZ1(1)='0   '
      IDERZ2(1)='    '
      GO TO 985
  349 CONTINUE
!
      IF(NUMPAR.LE.0)GO TO 359
      DO 350 I=1,NUMPAR
      IF(IH1.EQ.IPARN1(I).AND.IH2.EQ.IPARN2(I))GO TO 355
  350 CONTINUE
      GO TO 359
  355 CONTINUE
      I2=1
      IDERZ1(1)='0   '
      IDERZ2(1)='    '
      GO TO 985
  359 CONTINUE
!
      IF(NUMVAR.LE.0)GO TO 369
      DO 360 I=1,NUMVAR
      IF(IH1.EQ.IVARN1(I).AND.IH2.EQ.IVARN2(I))GO TO 380
  360 CONTINUE
  369 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,371)
  371 FORMAT('******ERROR IN DERIV3--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,372)
  372 FORMAT('      CHARACTER AFTER ( NOT A ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,373)
  373 FORMAT('      $ (FOR EXPRESSION), & (FOR NUMBER),')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,374)
  374 FORMAT('      A PARAMETER, OR A VARIABLE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,375)NCFZ
  375 FORMAT('NCFZ = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 376 I=1,NCFZ
      WRITE(ICOUT,377)I,IFUNZ1(I),IFUNZ2(I)
  377 FORMAT('I,IFUNZ1(I),IFUNZ2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
  376 CONTINUE
      IERROR='YES'
      GO TO 9000
!
  380 CONTINUE
      I2=0
      IF(IFUNZ1(1).EQ.'-   '.AND.IFUNZ2(I).EQ.'    ')GO TO 385
      GO TO 390
  385 CONTINUE
      I2=I2+1
      IDERZ1(I2)='-   '
      IDERZ2(I2)='    '
  390 CONTINUE
!
!               *****************************************
!               **  STEP 3--                           **
!               **  TREAT THE LIBRARY FUNCTIONS CASE.  **
!               *****************************************
!
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IHLF1.EQ.'SQRT'.AND.IHLF2.EQ.'    ')GO TO 510
      IF(IHLF1.EQ.'EXP '.AND.IHLF2.EQ.'    ')GO TO 510
      IF(IHLF1.EQ.'ALOG'.AND.IHLF2.EQ.'    ')GO TO 510
      IF(IHLF1.EQ.'ALOG'.AND.IHLF2.EQ.'E   ')GO TO 510
      IF(IHLF1.EQ.'ALOG'.AND.IHLF2.EQ.'10  ')GO TO 510
      IF(IHLF1.EQ.'LOG '.AND.IHLF2.EQ.'    ')GO TO 510
      IF(IHLF1.EQ.'LOGE'.AND.IHLF2.EQ.'    ')GO TO 510
      IF(IHLF1.EQ.'LOG1'.AND.IHLF2.EQ.'0   ')GO TO 510
!
      IF(IHLF1.EQ.'SIN '.AND.IHLF2.EQ.'    ')GO TO 610
      IF(IHLF1.EQ.'COS '.AND.IHLF2.EQ.'    ')GO TO 610
      IF(IHLF1.EQ.'TAN '.AND.IHLF2.EQ.'    ')GO TO 610
      IF(IHLF1.EQ.'COT '.AND.IHLF2.EQ.'    ')GO TO 610
      IF(IHLF1.EQ.'SEC '.AND.IHLF2.EQ.'    ')GO TO 610
      IF(IHLF1.EQ.'CSC '.AND.IHLF2.EQ.'    ')GO TO 610
!
      IF(IHLF1.EQ.'ARCS'.AND.IHLF2.EQ.'IN  ')GO TO 620
      IF(IHLF1.EQ.'ARCC'.AND.IHLF2.EQ.'OS  ')GO TO 620
      IF(IHLF1.EQ.'ARCT'.AND.IHLF2.EQ.'AN  ')GO TO 620
      IF(IHLF1.EQ.'ARCC'.AND.IHLF2.EQ.'OT  ')GO TO 620
      IF(IHLF1.EQ.'ARCS'.AND.IHLF2.EQ.'EC  ')GO TO 620
      IF(IHLF1.EQ.'ARCC'.AND.IHLF2.EQ.'SC  ')GO TO 620
!
      IF(IHLF1.EQ.'SINH'.AND.IHLF2.EQ.'    ')GO TO 630
      IF(IHLF1.EQ.'COSH'.AND.IHLF2.EQ.'    ')GO TO 630
      IF(IHLF1.EQ.'TANH'.AND.IHLF2.EQ.'    ')GO TO 630
      IF(IHLF1.EQ.'COTH'.AND.IHLF2.EQ.'    ')GO TO 630
      IF(IHLF1.EQ.'SECH'.AND.IHLF2.EQ.'    ')GO TO 630
      IF(IHLF1.EQ.'CSCH'.AND.IHLF2.EQ.'    ')GO TO 630
!
      IF(IHLF1.EQ.'ARCS'.AND.IHLF2.EQ.'INH ')GO TO 640
      IF(IHLF1.EQ.'ARCC'.AND.IHLF2.EQ.'OSH ')GO TO 640
      IF(IHLF1.EQ.'ARCT'.AND.IHLF2.EQ.'ANH ')GO TO 640
      IF(IHLF1.EQ.'ARCC'.AND.IHLF2.EQ.'OTH ')GO TO 640
      IF(IHLF1.EQ.'ARCS'.AND.IHLF2.EQ.'ECH ')GO TO 640
      IF(IHLF1.EQ.'ARCC'.AND.IHLF2.EQ.'SCH ')GO TO 640
!
      IFOUND='NO'
      GO TO 8000
!
  510 CONTINUE
      CALL LIBFD1(IHLF1,IHLF2,I1,I2,ITYPE,   &
      IFUNZ1,IFUNZ2,IDERZ1,IDERZ2)
      GO TO 970
!
  610 CONTINUE
      CALL TRIGD1(IHLF1,IHLF2,I1,I2,ITYPE,   &
      IFUNZ1,IFUNZ2,IDERZ1,IDERZ2)
      GO TO 970
!
  620 CONTINUE
      CALL TRIGD2(IHLF1,IHLF2,I1,I2,ITYPE,   &
      IFUNZ1,IFUNZ2,IDERZ1,IDERZ2)
      GO TO 970
!
  630 CONTINUE
      CALL TRIGD3(IHLF1,IHLF2,I1,I2,ITYPE,   &
      IFUNZ1,IFUNZ2,IDERZ1,IDERZ2)
      GO TO 970
!
  640 CONTINUE
      CALL TRIGD4(IHLF1,IHLF2,I1,I2,ITYPE,   &
      IFUNZ1,IFUNZ2,IDERZ1,IDERZ2)
      GO TO 970
!
  970 CONTINUE
      IF(ITYPE.EQ.'EXP ')GO TO 980
      GO TO 985
!
  980 CONTINUE
      I2=I2+1
      IDERZ1(I2)='*   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='%   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IFUNZ1(I1P2)
      IDERZ2(I2)=IFUNZ2(I1P2)
!
  985 CONTINUE
      NCDZ=I2
      IF(NCDZ.LE.2)GO TO 990
      IF(IDERZ1(1).EQ.'-   '.AND.IDERZ2(1).EQ.'    '.AND.   &
         IDERZ1(2).EQ.'-   '.AND.IDERZ2(2).EQ.'    ')GO TO 986
      IF(IDERZ1(1).EQ.'+   '.AND.IDERZ2(1).EQ.'    '.AND.   &
         IDERZ1(2).EQ.'+   '.AND.IDERZ2(2).EQ.'    ')GO TO 986
      IF(IDERZ1(1).EQ.'-   '.AND.IDERZ2(1).EQ.'    '.AND.   &
         IDERZ1(2).EQ.'+   '.AND.IDERZ2(2).EQ.'    ')GO TO 988
      IF(IDERZ1(1).EQ.'+   '.AND.IDERZ2(1).EQ.'    '.AND.   &
         IDERZ1(2).EQ.'-   '.AND.IDERZ2(2).EQ.'    ')GO TO 988
      GO TO 990
  986 CONTINUE
      I2=0
      DO 987 I=3,NCDZ
      I2=I2+1
      IDERZ1(I2)=IDERZ1(I)
      IDERZ2(I2)=IDERZ2(I)
  987 CONTINUE
      GO TO 990
  988 CONTINUE
      I2=1
      IDERZ1(I2)='-   '
      IDERZ2(I2)='    '
      DO 989 I=3,NCDZ
      I2=I2+1
      IDERZ1(I2)=IDERZ1(I)
      IDERZ2(I2)=IDERZ2(I)
  989 CONTINUE
  990 CONTINUE
      NCDZ=I2
!
      GO TO 8000
!
!               *********************************
!               **  STEP 4--                   **
!               **  SEARCH FOR **  --          **
!               **  THIS WILL INDICATE AN      **
!               **  EXPONENTIATION OPERATION.  **
!               *********************************
!
 3000 CONTINUE
!
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      DO 3300 I=1,NCFZ
      I2=I
      IF(IFUNZ1(I).EQ.'**  '.AND.IFUNZ2(I).EQ.'    ')GO TO 5000
 3300 CONTINUE
!
!               ********************************************
!               **  STEP 5--                              **
!               **  TREAT THE LONE VARIABLE (ETC.) CASE.  **
!               ********************************************
!
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      I1=0
      I2=0
      I1=I1+1
      IF(IFUNZ1(I1).EQ.'-   '.AND.IFUNZ2(I1).EQ.'    ')GO TO 4100
      IF(IFUNZ1(I1).EQ.'+   '.AND.IFUNZ2(I1).EQ.'    ')GO TO 4150
      GO TO 4200
!
 4100 CONTINUE
      I2=I2+1
      IDERZ1(I2)=IFUNZ1(I1)
      IDERZ2(I2)=IFUNZ2(I1)
 4150 CONTINUE
      I1=I1+1
      GO TO 4200
!
 4200 CONTINUE
      IF(IFUNZ1(I1).EQ.'$   '.AND.IFUNZ2(I1).EQ.'    ')GO TO 4300
      GO TO 4400
!
 4300 CONTINUE
      I2=I2+1
      IDERZ1(I2)='%   '
      IDERZ2(I2)='    '
      I1=I1+1
      I2=I2+1
      IDERZ1(I2)=IFUNZ1(I1)
      IDERZ2(I2)=IFUNZ2(I1)
      GO TO 4900
!
 4400 CONTINUE
      IF(IFUNZ1(I1).EQ.'&   '.AND.IFUNZ2(I1).EQ.'    ')GO TO 4500
      GO TO 4600
!
 4500 CONTINUE
      I2=1
      IDERZ1(I2)='0   '
      IDERZ2(I2)='    '
      GO TO 4900
!
 4600 CONTINUE
!CCCC IH1=IFUNZ1(I1)
!CCCC IH2=IFUNZ2(I1)
!CCCC IF(NUMPAR.LE.0)GO TO 4690
!CCCC DO4610I=1,NUMPAR
!CCCC IF(IH1.EQ.IPARN1(I).AND.IH2.EQ.IPARN2(I))GO TO 4620
!4610 CONTINUE
!CCCC GO TO 4690
!4620 CONTINUE
!CCCC I2=1
!CCCC IDERZ1(I2)='0   '
!CCCC IDERZ2(I2)='    '
!CCCC GO TO 4900
!4690 CONTINUE
!
      IH1=IFUNZ1(I1)
      IH2=IFUNZ2(I1)
      IF(NUMVAR.LE.0)GO TO 4790
      DO 4710 I=1,NUMVAR
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV3')   &
      WRITE(ICOUT,4711)IH1,IH2,IVARN1(I),IVARN2(I)
 4711 FORMAT('IH1,IH2,IVARN1(I),IVARN2(I) = ',A4,2X,A4,2X,A4,2X,A4)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV3')   &
      CALL DPWRST('XXX','BUG ')
      IF(IH1.EQ.IVARN1(I).AND.IH2.EQ.IVARN2(I))GO TO 4720
 4710 CONTINUE
      GO TO 4780
 4720 CONTINUE
      I2=I2+1
      IDERZ1(I2)='1   '
      IDERZ2(I2)='    '
      GO TO 4900
 4780 CONTINUE
      I2=I2+1
      IDERZ1(I2)='0   '
      IDERZ2(I2)='    '
      GO TO 4900
 4790 CONTINUE
!
      WRITE(6,4801)
 4801 FORMAT('*****ERROR IN DERIV3--')
      WRITE(6,4802)
 4802 FORMAT('     ILLEGAL ELEMENT TYPE')
      WRITE(ICOUT,4803)NCFZ
 4803 FORMAT('NCFZ = ',I6)
      CALL DPWRST('XXX','BUG ')
      DO 4806 I=1,NCFZ
      WRITE(ICOUT,4807)I,IFUNZ1(I),IFUNZ2(I)
 4807 FORMAT('I,IFUNZ1(I),IFUNZ2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 4806 CONTINUE
      WRITE(ICOUT,4815)NCDZ
 4815 FORMAT('NCDZ = ',I6)
      CALL DPWRST('XXX','BUG ')
      DO 4816 I=1,NCDZ
      WRITE(ICOUT,4817)I,IDERZ1(I),IDERZ2(I)
 4817 FORMAT('I,IDERZ1(I),IDERZ2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 4816 CONTINUE
      WRITE(ICOUT,4821)NUMPAR
 4821 FORMAT('NUMPAR = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 4822 I=1,NUMPAR
      WRITE(ICOUT,4823)I,IPARN1(I),IPARN2(I)
 4823 FORMAT('I,IPARN1(I),IPARN2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 4822 CONTINUE
      WRITE(ICOUT,4831)NUMVAR
 4831 FORMAT('NUMVAR = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 4832 I=1,NUMVAR
      WRITE(ICOUT,4833)I,IVARN1(I),IVARN2(I)
 4833 FORMAT('I,IVARN1(I),IVARN2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 4832 CONTINUE
      IERROR='YES'
      GO TO 9000
!
 4900 CONTINUE
      NCDZ=I2
      GO TO 8000
!
!               ***********************************
!               **  STEP 6--                     **
!               **  TREAT THE EXPONENTIAL CASE.  **
!               ***********************************
!
 5000 CONTINUE
!
      ISTEPN='6'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      I1=0
      I1=I1+1
      IF(IFUNZ1(I1).EQ.'+   '.AND.IFUNZ2(I1).EQ.'    ')GO TO 5100
      IF(IFUNZ1(I1).EQ.'-   '.AND.IFUNZ2(I1).EQ.'    ')GO TO 5100
      GO TO 5150
!
 5100 CONTINUE
      ISIGN1=IFUNZ1(I1)
      ISIGN2=IFUNZ2(I1)
      I1=I1+1
      GO TO 5200
!
 5150 CONTINUE
      ISIGN1='+   '
      ISIGN2='    '
      GO TO 5200
!
 5200 CONTINUE
      IF(IFUNZ1(I1).EQ.'$   '.AND.IFUNZ2(I1).EQ.'    ')GO TO 5300
      GO TO 5400
!
 5300 CONTINUE
      IMAN11=IFUNZ1(I1)
      IMAN12=IFUNZ2(I1)
      I1=I1+1
      IMAN21=IFUNZ1(I1)
      IMAN22=IFUNZ2(I1)
      IMANTT='EXP '
      GO TO 5900
!
 5400 CONTINUE
      IF(IFUNZ1(I1).EQ.'&   '.AND.IFUNZ2(I1).EQ.'    ')GO TO 5500
      GO TO 5600
!
 5500 CONTINUE
      IMAN11=IFUNZ1(I1)
      IMAN12=IFUNZ2(I1)
      I1=I1+1
      IMAN21=IFUNZ1(I1)
      IMAN22=IFUNZ2(I1)
      IMANTT='CON '
      GO TO 5900
!
 5600 CONTINUE
      IH1=IFUNZ1(I1)
      IH2=IFUNZ2(I1)
      IF(NUMPAR.LE.0)GO TO 5690
      DO 5610 I=1,NUMPAR
      IF(IH1.EQ.IPARN1(I).AND.IH2.EQ.IPARN2(I))GO TO 5620
 5610 CONTINUE
      GO TO 5690
 5620 CONTINUE
      IMAN11=IFUNZ1(I1)
      IMAN12=IFUNZ2(I1)
      IMANTT='PAR '
      GO TO 5900
 5690 CONTINUE
!
      IH1=IFUNZ1(I1)
      IH2=IFUNZ2(I1)
      IF(NUMVAR.LE.0)GO TO 5790
      DO 5710 I=1,NUMVAR
      IF(IH1.EQ.IVARN1(I).AND.IH2.EQ.IVARN2(I))GO TO 5720
 5710 CONTINUE
      GO TO 5790
 5720 CONTINUE
      IDERZ1(I2)='1   '
      IDERZ2(I2)='    '
      IMAN11=IFUNZ1(I1)
      IMAN12=IFUNZ2(I1)
      IMANTT='VAR '
      GO TO 5900
 5790 CONTINUE
!
      WRITE(6,5801)
 5801 FORMAT('*****ERROR IN DERIV3--')
      WRITE(6,5802)
 5802 FORMAT('     ILLEGAL MANTISSA TYPE')
      DO 5806 I=1,NCFZ
      WRITE(ICOUT,5807)I,IFUNZ1(I),IFUNZ2(I)
 5807 FORMAT('I,IFUNZ1(I),IFUNZ2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 5806 CONTINUE
      WRITE(ICOUT,5815)NCDZ
 5815 FORMAT('NCDZ = ',I6)
      CALL DPWRST('XXX','BUG ')
      DO 5816 I=1,NCDZ
      WRITE(ICOUT,5817)I,IDERZ1(I),IDERZ2(I)
 5817 FORMAT('I,IDERZ1(I),IDERZ2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 5816 CONTINUE
      IERROR='YES'
      GO TO 9000
!
 5900 CONTINUE
!
      I1=I1+1
      IF(IFUNZ1(I1).EQ.'**  '.AND.IFUNZ2(I1).EQ.'    ')GO TO 6100
!
      WRITE(6,6001)
 6001 FORMAT('*****ERROR IN DERIV3--')
      WRITE(6,6002)
 6002 FORMAT('     ** NOT ENCOUNTERED,')
      WRITE(ICOUT,6003)
 6003 FORMAT('     WHERE IT SHOULD HAVE BEEN.')
      CALL DPWRST('XXX','BUG ')
      DO 6006 I=1,NCFZ
      WRITE(ICOUT,6007)I,IFUNZ1(I),IFUNZ2(I)
 6007 FORMAT('I,IFUNZ1(I),IFUNZ2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 6006 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,6015)NCDZ
 6015 FORMAT('NCDZ = ',I6)
      CALL DPWRST('XXX','BUG ')
      DO 6016 I=1,NCDZ
      WRITE(ICOUT,6017)I,IDERZ1(I),IDERZ2(I)
 6017 FORMAT('I,IDERZ1(I),IDERZ2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 6016 CONTINUE
      GO TO 9000
!
 6100 CONTINUE
      I1=I1+1
      GO TO 6200
!
 6200 CONTINUE
      IF(IFUNZ1(I1).EQ.'$   '.AND.IFUNZ2(I1).EQ.'    ')GO TO 6300
      GO TO 6400
!
 6300 CONTINUE
      IEXP11=IFUNZ1(I1)
      IEXP12=IFUNZ2(I1)
      I1=I1+1
      IEXP21=IFUNZ1(I1)
      IEXP22=IFUNZ2(I1)
      IEXPT='EXP '
      GO TO 6900
!
 6400 CONTINUE
      IF(IFUNZ1(I1).EQ.'&   '.AND.IFUNZ2(I1).EQ.'    ')GO TO 6500
      GO TO 6600
!
 6500 CONTINUE
      IEXP11=IFUNZ1(I1)
      IEXP12=IFUNZ2(I1)
      I1=I1+1
      IEXP21=IFUNZ1(I1)
      IEXP22=IFUNZ2(I1)
      IEXPT='CON '
      GO TO 6900
!
 6600 CONTINUE
      IH1=IFUNZ1(I1)
      IH2=IFUNZ2(I1)
      IF(NUMPAR.LE.0)GO TO 6690
      DO 6610 I=1,NUMPAR
      IF(IH1.EQ.IPARN1(I).AND.IH2.EQ.IPARN2(I))GO TO 6620
 6610 CONTINUE
      GO TO 6690
 6620 CONTINUE
      IEXP11=IFUNZ1(I1)
      IEXP12=IFUNZ2(I1)
      IEXPT='PAR '
      GO TO 6900
 6690 CONTINUE
!
      IH1=IFUNZ1(I1)
      IH2=IFUNZ2(I1)
      IF(NUMVAR.LE.0)GO TO 6790
      DO 6710 I=1,NUMVAR
      IF(IH1.EQ.IVARN1(I).AND.IH2.EQ.IVARN2(I))GO TO 6720
 6710 CONTINUE
      GO TO 6790
 6720 CONTINUE
      IDERZ1(I2)='1   '
      IDERZ2(I2)='    '
      IEXP11=IFUNZ1(I1)
      IEXP12=IFUNZ2(I1)
      IEXPT='VAR '
      GO TO 6900
 6790 CONTINUE
!
      WRITE(6,6801)
 6801 FORMAT('*****ERROR IN DERIV3--')
      WRITE(6,6802)
 6802 FORMAT('     ILLEGAL EXPONENT TYPE')
      DO 6805 I=1,NCDZ
      WRITE(ICOUT,6806)I,IFUNZ1(I),IFUNZ2(I)
 6806 FORMAT('I,IFUNZ1(I),IFUNZ2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 6805 CONTINUE
      IERROR='YES'
      GO TO 9000
!
 6900 CONTINUE
!
      IF((IMANTT.EQ.'CON '.OR.IMANTT.EQ.'PAR ').AND.   &
         (IEXPT.EQ.'CON '.OR.IEXPT.EQ.'PAR '))GO TO 7010
      IF((IMANTT.EQ.'VAR '.OR.IMANTT.EQ.'EXP ').AND.   &
         (IEXPT.EQ.'CON '.OR.IEXPT.EQ.'PAR '))GO TO 7020
      IF((IMANTT.EQ.'CON '.OR.IMANTT.EQ.'PAR ').AND.   &
         (IEXPT.EQ.'VAR '.OR.IEXPT.EQ.'EXP '))GO TO 7030
      IF((IMANTT.EQ.'VAR '.OR.IMANTT.EQ.'EXP ').AND.   &
         (IEXPT.EQ.'VAR '.OR.IEXPT.EQ.'EXP '))GO TO 7040
!
      WRITE(ICOUT,7071)
 7071 FORMAT('***** ERROR IN DERIV3--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7072)
 7072 FORMAT('     A MANTISSA OR EXPONENT TYPE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7073)
 7073 FORMAT('      IS NOT CON PAR VAR EXP')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7074)IMANTT,IEXPT
 7074 FORMAT('IMANTT, IEXPT = ',A6,2X,A6)
      CALL DPWRST('XXX','BUG ')
      DO 7075 I=1,NCDZ
      WRITE(ICOUT,7076)I,IFUNZ1(I),IFUNZ2(I)
 7076 FORMAT('I,IFUNZ1(I),IFUNZ2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 7075 CONTINUE
      IERROR='YES'
      GO TO 9000
!
!               ****************************
!               **  STEP 7.1--            **
!               **  TREAT THE A**B CASE.  **
!               ****************************
 7010 CONTINUE
!
      ISTEPN='7.1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      I2=1
      IDERZ1(I2)='0   '
      IDERZ2(I2)='    '
      GO TO 7900
!
!               ****************************
!               **  STEP 7.2--            **
!               **  TREAT THE X**A CASE.  **
!               ****************************
!
 7020 CONTINUE
!
      ISTEPN='7.2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      I2=0
      IF(ISIGN1.EQ.'-   '.AND.ISIGN2.EQ.'    ')I2=I2+1
      IF(ISIGN1.EQ.'-   '.AND.ISIGN2.EQ.'    ')IDERZ1(I2)='-   '
      IF(ISIGN1.EQ.'-   '.AND.ISIGN2.EQ.'    ')IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IEXP11
      IDERZ2(I2)=IEXP12
      IF(IEXPT.EQ.'CON ')I2=I2+1
      IF(IEXPT.EQ.'CON ')IDERZ1(I2)=IEXP21
      IF(IEXPT.EQ.'CON ')IDERZ2(I2)=IEXP22
      I2=I2+1
      IDERZ1(I2)='*   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IMAN11
      IDERZ2(I2)=IMAN12
      IF(IMANTT.EQ.'EXP ')I2=I2+1
      IF(IMANTT.EQ.'EXP ')IDERZ1(I2)=IMAN21
      IF(IMANTT.EQ.'EXP ')IDERZ2(I2)=IMAN22
      I2=I2+1
      IDERZ1(I2)='**  '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IEXP11
      IDERZ2(I2)=IEXP12
      IF(IEXPT.EQ.'CON ')I2=I2+1
      IF(IEXPT.EQ.'CON ')IDERZ1(I2)=IEXP21
      IF(IEXPT.EQ.'CON ')IDERZ2(I2)=IEXP22
      I2=I2+1
      IDERZ1(I2)='-   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='1   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      IF(IMANTT.EQ.'EXP ')GO TO 7025
      GO TO 7029
 7025 CONTINUE
      I2=I2+1
      IDERZ1(I2)='*   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='%   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IMAN21
      IDERZ2(I2)=IMAN22
 7029 CONTINUE
      GO TO 7900
!
!               ****************************
!               **  STEP 7.3--            **
!               **  TREAT THE A**X CASE.  **
!               ****************************
!
 7030 CONTINUE
!
      ISTEPN='7.3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      I2=0
      IF(ISIGN1.EQ.'-   '.AND.ISIGN2.EQ.'    ')I2=I2+1
      IF(ISIGN1.EQ.'-   '.AND.ISIGN2.EQ.'    ')IDERZ1(I2)='-   '
      IF(ISIGN1.EQ.'-   '.AND.ISIGN2.EQ.'    ')IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IMAN11
      IDERZ2(I2)=IMAN12
      IF(IMANTT.EQ.'CON ')I2=I2+1
      IF(IMANTT.EQ.'CON ')IDERZ1(I2)=IMAN21
      IF(IMANTT.EQ.'CON ')IDERZ2(I2)=IMAN22
      I2=I2+1
      IDERZ1(I2)='**  '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IEXP11
      IDERZ2(I2)=IEXP12
      IF(IEXPT.EQ.'EXP ')I2=I2+1
      IF(IEXPT.EQ.'EXP ')IDERZ1(I2)=IEXP21
      IF(IEXPT.EQ.'EXP ')IDERZ2(I2)=IEXP22
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='*   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='ALOG'
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IMAN11
      IDERZ2(I2)=IMAN12
      IF(IMANTT.EQ.'CON ')I2=I2+1
      IF(IMANTT.EQ.'CON ')IDERZ1(I2)=IMAN21
      IF(IMANTT.EQ.'CON ')IDERZ2(I2)=IMAN22
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      IF(IEXPT.EQ.'EXP ')GO TO 7035
      GO TO 7039
 7035 CONTINUE
      I2=I2+1
      IDERZ1(I2)='*   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='%   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IEXP21
      IDERZ2(I2)=IEXP22
 7039 CONTINUE
      GO TO 7900
!
!               ****************************
!               **  STEP 7.4--            **
!               **  TREAT THE U**V CASE.  **
!               ****************************
!
 7040 CONTINUE
!
      ISTEPN='7.4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      I2=0
      IF(ISIGN1.EQ.'-   '.AND.ISIGN2.EQ.'    ')I2=I2+1
      IF(ISIGN1.EQ.'-   '.AND.ISIGN2.EQ.'    ')IDERZ1(I2)='-   '
      IF(ISIGN1.EQ.'-   '.AND.ISIGN2.EQ.'    ')IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IEXP11
      IDERZ2(I2)=IEXP12
      IF(IEXPT.EQ.'EXP ')I2=I2+1
      IF(IEXPT.EQ.'EXP ')IDERZ1(I2)=IEXP21
      IF(IEXPT.EQ.'EXP ')IDERZ2(I2)=IEXP22
      I2=I2+1
      IDERZ1(I2)='*   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IMAN11
      IDERZ2(I2)=IMAN12
      IF(IMANTT.EQ.'EXP ')I2=I2+1
      IF(IMANTT.EQ.'EXP ')IDERZ1(I2)=IMAN21
      IF(IMANTT.EQ.'EXP ')IDERZ2(I2)=IMAN22
      I2=I2+1
      IDERZ1(I2)='**  '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IEXP11
      IDERZ2(I2)=IEXP12
      IF(IEXPT.EQ.'EXP ')I2=I2+1
      IF(IEXPT.EQ.'EXP ')IDERZ1(I2)=IEXP21
      IF(IEXPT.EQ.'EXP ')IDERZ2(I2)=IEXP22
      I2=I2+1
      IDERZ1(I2)='-   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='1   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      IF(IMANTT.EQ.'EXP ')GO TO 7041
      GO TO 7042
 7041 CONTINUE
      I2=I2+1
      IDERZ1(I2)='*   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='%   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IMAN21
      IDERZ2(I2)=IMAN22
 7042 CONTINUE
!
      I2=I2+1
      IDERZ1(I2)='+   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='ALOG'
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='(   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IMAN11
      IDERZ2(I2)=IMAN12
      IF(IMANTT.EQ.'EXP ')I2=I2+1
      IF(IMANTT.EQ.'EXP ')IDERZ1(I2)=IMAN21
      IF(IMANTT.EQ.'EXP ')IDERZ2(I2)=IMAN22
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='*   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IMAN11
      IDERZ2(I2)=IMAN12
      IF(IMANTT.EQ.'EXP ')I2=I2+1
      IF(IMANTT.EQ.'EXP ')IDERZ1(I2)=IMAN21
      IF(IMANTT.EQ.'EXP ')IDERZ2(I2)=IMAN22
      I2=I2+1
      IDERZ1(I2)='**  '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IEXP11
      IDERZ2(I2)=IEXP12
      IF(IEXPT.EQ.'EXP ')I2=I2+1
      IF(IEXPT.EQ.'EXP ')IDERZ1(I2)=IEXP21
      IF(IEXPT.EQ.'EXP ')IDERZ2(I2)=IEXP22
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      IF(IEXPT.EQ.'EXP ')GO TO 7043
      GO TO 7044
 7043 CONTINUE
      I2=I2+1
      IDERZ1(I2)='*   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)='%   '
      IDERZ2(I2)='    '
      I2=I2+1
      IDERZ1(I2)=IEXP21
      IDERZ2(I2)=IEXP22
 7044 CONTINUE
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      GO TO 7900
!
 7900 CONTINUE
      NCDZ=I2
      GO TO 8000
!
!               ************************************
!               **  STEP 8--                      **
!               **  COPY THE EXPRESSION           **
!               **  IN THE VECTOR IDERZ1(.)        **
!               **  INTO ROW IROW2 OF IDER21(.,.)  **
!               ************************************
!
 8000 CONTINUE
!
      ISTEPN='8'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV3')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IDERZ1(1).EQ.'+   '.AND.IDERZ2(1).EQ.'    ')GO TO 8010
      IF(IDERZ1(1).EQ.'-   '.AND.IDERZ2(1).EQ.'    ')GO TO 8010
      GO TO 8090
 8010 CONTINUE
      IHOL11='(   '
      IHOL12='    '
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV3')   &
      WRITE(ICOUT,8011)NCDZ
 8011 FORMAT('NCDZ = ',I8)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV3')   &
      CALL DPWRST('XXX','BUG ')
      DO 8020 I=1,NCDZ
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV3')   &
      WRITE(ICOUT,8021)I,IDERZ1(I),IDERZ2(I)
 8021 FORMAT('I,IDERZ1(I),IDERZ2(I) = ',I8,2X,A4,2X,A4)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV3')   &
      CALL DPWRST('XXX','BUG ')
      IHOL21=IDERZ1(I)
      IHOL22=IDERZ2(I)
      IDERZ1(I)=IHOL11
      IDERZ2(I)=IHOL12
      IHOL11=IHOL21
      IHOL12=IHOL22
 8020 CONTINUE
      I2=NCDZ
      I2=I2+1
      IDERZ1(I2)=IHOL11
      IDERZ2(I2)=IHOL12
      I2=I2+1
      IDERZ1(I2)=')   '
      IDERZ2(I2)='    '
      NCDZ=I2
 8090 CONTINUE
!
      NCD2(IROW2)=NCDZ
      DO 8100 I=1,NCDZ
      IDER21(IROW2,I)=IDERZ1(I)
      IDER22(IROW2,I)=IDERZ2(I)
 8100 CONTINUE
!
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'RIV3')GO TO 9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('AT THE END       OF DERIV3--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)NCD2(IROW2)
 9013 FORMAT('NCD2(IROW2) = ',I8)
      CALL DPWRST('XXX','BUG ')
      IMAX=NCD2(IROW2)
      DO 9015 I=1,IMAX
      WRITE(ICOUT,9016)I,IDER21(IROW2,I),IDER22(IROW2,I)
 9016 FORMAT('I,IDER21(IROW2,I),IDER22(IROW2,I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
      WRITE(ICOUT,9021)NUMPAR
 9021 FORMAT('NUMPAR = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 9022 I=1,NUMPAR
      WRITE(ICOUT,9023)I,IPARN1(I),IPARN2(I)
 9023 FORMAT('I,IPARN1(I),IPARN2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9022 CONTINUE
      WRITE(ICOUT,9031)NUMVAR
 9031 FORMAT('NUMVAR = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO 9032 I=1,NUMVAR
      WRITE(ICOUT,9033)I,IVARN1(I),IVARN2(I)
 9033 FORMAT('I,IVARN1(I),IVARN2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9032 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DERIV3
      SUBROUTINE DERIV4(IFUN21,IFUN22,NCF2,NFUN2,   &
                        IDER21,IDER22,NCD2,IOP2,IROW1,   &
                        IDER11,IDER12,NCD1,IBUGA3,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--DETERMINE THE DERIVATIVE OF
!              A MULTIPLICATIVE EXPRESSION
!              (= 1 FULL ADDITIVE COMPONENT)
!              (EXAMPLE, A*X/C*D**E*X)
!              BY COMBINING DERIVATIVES OF EACH
!              ELEMENTAL COMPONENT.
!
!              THE ENTIRE INPUT EXPRESSION IS LOCATED
!              IN ROW IROW1 OF IFUN11--
!              IT HAS LENGTH NF1
!              (THIS SUBROUTINE NEED NEVER SEE
!              THIS ENTIRE EXPRESSION.)
!
!              THE INPUT ELEMENTS OF THE
!              INPUT EXPRESSION ARE LOCATED
!              IN VARIOUS ROWS OF IFUN21.
!
!              THE INPUT DERIVATIVES OF THE
!              INPUT ELEMENTS ARE LOCATED
!              IN VARIOUS ROWS OF IDER21.
!
!              THE OUTPUT DERIVATIVE IS LOCATED
!              IN ROW IROW1 OF IFUN1--
!              IT HAS LENGTH NCD1.
!
!     INPUT  ARGUMENTS--IFUN21 = THE ARRAY WHOSE I-TH ROW
!                                IS THE I-TH
!                                MULTIPLICATIVE COMPONENT
!                                OF THE IROW1-TH (IROW1 FIXED)
!                                ADDITIVE COMPONENT
!                                (FIRST 4 CHARACTERS).
!                     --IFUN22 = THE ARRAY WHOSE I-TH ROW
!                                IS THE I-TH
!                                MULTIPLICATIVE COMPONENT
!                                OF THE IROW1-TH (IROW1 FIXED)
!                                ADDITIVE COMPONENT
!                                (NEXT  4 CHARACTERS).
!                     --NCF2   = AN INTEGER VECTOR
!                                WHOSE IROW1-TH ELEMENT
!                                IS THE LENGTH
!                                OF THE I-TH
!                                MULTIPLICATIVE COMPONENT
!                                OF THE IROW1-TH (IROW1 FIXED)
!                                ADDITIVE COMPONENT.
!                     --NFUN2  = THE NUMBER OF ROWS
!                                (= THE NUMBER OF MULTIPLICATIVE
!                                SUBSTRINGS OF THE IROW1-TH
!                                ADDITIVE COMPONENT)
!                                THAT IS
!                                IN THE ARRAY IFUN21(.,.)
!                     --IOP2   = A VECTOR OF OPERATIONS
!                                (BETWEEN ELEMENTS--* OR /.
!                     --IDER21  = THE ARRAY WHOSE I-TH ROW
!                                IS THE DERIVATIVE OF THE I-TH
!                                MULTIPLICATIVE COMPONENT
!                                OF THE IROW1-TH (IROW1 FIXED)
!                                (FIRST 4 CHARACTERS).
!                     --IDER22 = THE ARRAY WHOSE I-TH ROW
!                                IS THE DERIVATIVE OF THE I-TH
!                                MULTIPLICATIVE COMPONENT
!                                OF THE IROW1-TH (IROW1 FIXED)
!                                (NEXT  4 CHARACTERS).
!                     --NCD2   = AN INTEGER VECTOR
!                                WHOSE IROW1-TH ELEMENT
!                                IS THE LENGTH
!                                OF THE DERIVATIVE OF THE I-TH
!                                MULTIPLICATIVE COMPONENT
!                                OF THE IROW1-TH (IROW1 FIXED)
!                                ADDITIVE COMPONENT.
!                                WHOSE I-TH ELEMENT
!                                IS THE (TRAILING) OPERATION (* OR /)
!                                OF THE I-TH MULTIPLICATIVE SUBSTRING
!                                OF THE IROW1-TH ADDITIVE COMPONENT.
!                     --IROW1  = THE ROW NUMBER (IN IFUN1(.,.)) OF
!                                THE PARTICULAR
!                                ADDITIVE COMPONENT OF INTEREST.
!     OUTPUT ARGUMENTS--IDER11 = THE ARRAY WHOSE IROW1-TH ROW
!                                WILL BE THE DERIVATIVE OF THE
!                                IROW1-TH ADDITIVE STRING
!                                (FIRST 4 CHARACTERS).
!                     --IDER12 = THE ARRAY WHOSE IROW1-TH ROW
!                                WILL BE THE DERIVATIVE OF THE
!                                IROW1-TH ADDITIVE STRING
!                                (NEXT  4 CHARACTERS).
!                       NCD1   = AN INTEGER VECTOR
!                                WHOSE IROW1-TH ELEMENT
!                                WILL BE THE LENGTH OF THE IROW1-TH
!                                DERIVATIVE IN IDER11(.,.);
!                                THAT IS, NCD1(IROW1) = THE LENGTH OF THE
!                                DERIVATIVE OF INTEREST.
!     INTERNAL ARRAYS--
!                       IFUN21  = THE ARRAY WHOSE I-TH
!                                ROW WILL BE THE I-TH MULTIPLICATIVE
!                                SUBSTRING OF THE IROW1-TH
!                                ADDITIVE COMPONENT.
!                       NCF2   = AN INTEGER VECTOR
!                                WHOSE I-TH ELEMENT
!                                WILL BE THE LENGTH OF THE I-TH
!                                MULTIPLICATIVE SUBSTRING
!                                OF THE IROW1-TH ADDITIVE COMPONENT.
!
!     ORIGINAL VERSION--DECEMBER 2, 1978
!     UPDATED         --DECEMBER  1981.
!
!---------------------------------------------------------------------
!
      CHARACTER*4 IFUN21
      CHARACTER*4 IFUN22
      CHARACTER*4 IDER21
      CHARACTER*4 IDER22
      CHARACTER*4 IDER11
      CHARACTER*4 IDER12
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!CCCC CHARACTER*4 IBUG1
!CCCC CHARACTER*4 IBUG2
!CCCC CHARACTER*4 IBUG3
!
      CHARACTER*4 IDER31
      CHARACTER*4 IDER32
!
      CHARACTER*4 IFUN31
      CHARACTER*4 IFUN32
!
      CHARACTER*4 IOP2
!
      DIMENSION IFUN21(20,80)
      DIMENSION IFUN22(20,80)
      DIMENSION NCF2(1)
      DIMENSION IDER21(20,80)
      DIMENSION IDER22(20,80)
      DIMENSION NCD2(1)
      DIMENSION IOP2(1)
!
      DIMENSION IDER11(20,80)
      DIMENSION IDER12(20,80)
      DIMENSION NCD1(1)
!
      DIMENSION IFUN31(2,80)
      DIMENSION IFUN32(2,80)
      DIMENSION NCF3(2)
      DIMENSION IDER31(2,80)
      DIMENSION IDER32(2,80)
      DIMENSION NCD3(2)
!
!
!-----COMMON VARIABLES (GENERAL)-----------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-----------------------------------------------------
!
!CCCC DATA IBUG1/'OFF'/
!CCCC DATA IBUG2/'OFF'/
!CCCC DATA IBUG3/'OFF'/
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DERI'
      ISUBN2='V4  '
      IERROR='NO'
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RIV4')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('AT THE BEGINNING OF DERIV4--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IFOUND,IROW1,NFUN2
   52   FORMAT('IFOUND,IROW1,NFUN2 = ',A4,2X,2I8)
        CALL DPWRST('XXX','BUG ')
        ITEMP=80
        DO 60 I=1,NFUN2
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,61)I,NCF2(I),IOP2(I)
   61     FORMAT('I,NCF2(I) = ',2I8,2X,A4)
          CALL DPWRST('XXX','BUG ')
          DO 65 J=1,ITEMP
            WRITE(ICOUT,66)J,IFUN21(I,J),IFUN22(I,J)
   66       FORMAT('J,IFUN21(I,J),IFUN22(I,J) = ',I8,2(2X,A4))
            CALL DPWRST('XXX','BUG ')
   65     CONTINUE
   60   CONTINUE
!
        DO 70 I=1,NFUN2
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,71)I,NCD2(I)
   71     FORMAT('I,NCD2(I) = ',2I8)
          CALL DPWRST('XXX','BUG ')
          DO 75 J=1,ITEMP
            WRITE(ICOUT,76)J,IDER21(I,J),IDER22(I,J)
   76       FORMAT('J,IDER21(I,J),IDER22(I,J) = ',I8,2(2X,A4))
            CALL DPWRST('XXX','BUG ')
   75     CONTINUE
   70   CONTINUE
      ENDIF
!
!               ***********************************
!               **  STEP 1.1--                   **
!               **  FORM THE FIRST 2 FUNCTIONS.  **
!               ***********************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV4')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      NFUN3=NFUN2
      IF(NFUN2.GE.1)GO TO 1020
!
      WRITE(ICOUT,1011)
 1011 FORMAT('***** ERROR IN DERIV4--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1012)NFUN2
 1012 FORMAT('NFUN2 NON-POSITIVE. NFUN2 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
!
 1020 CONTINUE
      IROW3=1
      JMAX=NCF2(IROW3)
      K=0
      DO 1050 J=1,JMAX
      K=K+1
      IFUN31(1,K)=IFUN21(IROW3,J)
      IFUN32(1,K)=IFUN22(IROW3,J)
      IFUN31(2,K)=IFUN21(IROW3,J)
      IFUN32(2,K)=IFUN22(IROW3,J)
 1050 CONTINUE
      NCF3(1)=K
      NCF3(2)=K
!
!               *************************************
!               **  STEP 1.2--                     **
!               **  FORM THE FIRST 2 DERIVATIVES.  **
!               *************************************
!
      ISTEPN='1.2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV4')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NFUN2.GE.1)GO TO 2020
!
      WRITE(ICOUT,2001)
 2001 FORMAT('***** ERROR IN DERIV4--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2002)NFUN2
 2002 FORMAT('NFUN2 NON-POSITIVE. NFUN2 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
!
 2020 CONTINUE
      IROW3=1
      JMAX=NCD2(IROW3)
      K=0
      DO 2030 J=1,JMAX
      K=K+1
      IDER31(1,K)=IDER21(IROW3,J)
      IDER32(1,K)=IDER22(IROW3,J)
      IDER31(2,K)=IDER21(IROW3,J)
      IDER32(2,K)=IDER22(IROW3,J)
 2030 CONTINUE
      NCD3(1)=K
      NCD3(2)=K
!
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'RIV4')GO TO 2090
      WRITE(ICOUT,2006)
 2006 FORMAT('***** IN THE MIDDLE OF DERIV4--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2007)IROW3,NCF2(IROW3),NCD2(IROW3)
 2007 FORMAT('IROW3, NCF2(IROW3), NCD2(IROW3) = ',3I6)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2008)IROW3,NCF3(2),NCD3(2)
 2008 FORMAT('IROW3, NCF3(2), NCD3(2) = ',3I6)
      CALL DPWRST('XXX','BUG ')
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IMAX=NCF2(IROW3)
      DO 2040 I=1,IMAX
      WRITE(ICOUT,2045)I,IFUN21(IROW3,I),IFUN22(IROW3,I)
 2045 FORMAT('I,IFUN21(IROW3,I),IFUN22(IROW3,I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 2040 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IMAX=NCD2(IROW3)
      DO 2050 I=1,IMAX
      WRITE(ICOUT,2055)I,IDER21(IROW3,I),IDER22(IROW3,I)
 2055 FORMAT('I,IDER21(IROW3,I),IDER22(IROW3,I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 2050 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IMAX=NCF3(2)
      DO 2060 I=1,IMAX
      WRITE(ICOUT,2065)I,IFUN31(IROW3,I),IFUN32(IROW3,I)
 2065 FORMAT('I,IFUN31(IROW3,I),IFUN32(IROW3,I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 2060 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IMAX=NCD3(2)
      DO 2070 I=1,IMAX
      WRITE(ICOUT,2075)I,IDER31(IROW3,I),IDER32(IROW3,I)
 2075 FORMAT('I,IDER31(IROW3,I),IDER32(IROW3,I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 2070 CONTINUE
!
 2090 CONTINUE
      IF(NFUN2.EQ.1)GO TO 5000
!
      IF(NFUN3.LT.2)GO TO 2900
      DO 2100 IROW3=2,NFUN3
!
!               ***********************************************
!               **  STEP 2.1--                               **
!               **  MOVE THE CUMULATIVE FUNCTION             **
!               **  IN THE SECOND ROW OF IFUN31(.)            **
!               **  TO THE FIRST ROW OF IFUN31(.).            **
!               **  MOVE THE CUMULATIVE FUNCTION DERIVATIVE  **
!               **  IN THE SECOND ROW OF OF IDER31(.)         **
!               **  TO THE FIRST ROW OF IDER31(.).            **
!               ***********************************************
!
      ISTEPN='2.1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV4')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      JMAX=NCF3(2)
      DO 1110 J=1,JMAX
      IFUN31(1,J)=IFUN31(2,J)
      IFUN32(1,J)=IFUN32(2,J)
 1110 CONTINUE
      NCF3(1)=NCF3(2)
!
      JMAX=NCD3(2)
      DO 1120 J=1,JMAX
      IDER31(1,J)=IDER31(2,J)
      IDER32(1,J)=IDER32(2,J)
 1120 CONTINUE
      NCD3(1)=NCD3(2)
!
!               ******************************************************
!               **  STEP 2.2--                                      **
!               **  DEFINE THE FUNCTIONS (IN IFUN31(.,.))            **
!               **  WHICH COMBINE ITERATIVELY AND SEQUENTIALLY      **
!               **  EACH OF THE INDIVIDUAL MULTIPLICATIVE           **
!               **  COMPONENTS.                                     **
!               ******************************************************
!
      ISTEPN='2.2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV4')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IROW3M=IROW3-1
      IF(IOP2(IROW3M).EQ.'*')GO TO 1200
      IF(IOP2(IROW3M).EQ.'/')GO TO 1200
!
      WRITE(ICOUT,1061)
 1061 FORMAT('***** ERROR IN DERIV4--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1062)
 1062 FORMAT('OPERATION NOT * OR /')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1063)IROW3M
 1063 FORMAT('IROW3M = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1064)IOP2(IROW3M)
 1064 FORMAT('IOP2(IROW3M) = ',A6)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
!
!     TREAT EITHER THE * CASE OR THE / CASE.
!
 1200 CONTINUE
!
      K=0
      JMAX=NCF3(1)
      DO 1210 J=1,JMAX
      K=K+1
      IFUN31(2,K)=IFUN31(1,J)
      IFUN32(2,K)=IFUN32(1,J)
 1210 CONTINUE
!
      K=K+1
      IFUN31(2,K)=IOP2(IROW3M)
      IFUN32(2,K)='    '
!
      JMAX=NCF2(IROW3)
      DO 1215 J=1,JMAX
      K=K+1
      IFUN31(2,K)=IFUN21(IROW3,J)
      IFUN32(2,K)=IFUN22(IROW3,J)
 1215 CONTINUE
!
      NCF3(2)=K
      NFUN3=NFUN2
!
!               ********************************************************
!               **  STEP 2.3--                                        **
!               **  ITERATIVELY COMBINE IN SEQUENCE DERIVATIVES       **
!               **  FOR THE MULTIPLICATIVE SUBSTRINGS.                **
!               ********************************************************
!
      ISTEPN='2.3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV4')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IROW3M=IROW3-1
      IF(IOP2(IROW3M).EQ.'*')GO TO 2200
      IF(IOP2(IROW3M).EQ.'/')GO TO 2300
!
      WRITE(ICOUT,2061)
 2061 FORMAT('***** ERROR IN DERIV4--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2062)
 2062 FORMAT('OPERATION NOT * OR /')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2063)IROW3M
 2063 FORMAT('IROW3M = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2064)IOP2(IROW3M)
 2064 FORMAT('IOP2(IROW3M) = ',A6)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
!
!               *******************************
!               **  STEP 2.4--               **
!               **  TREAT THE PRODUCT CASE.  **
!               *******************************
!
 2200 CONTINUE
!
      ISTEPN='2.4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV4')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NCD3(1).EQ.1.AND.   &
      IDER31(1,1).EQ.'0'.AND.IDER32(1,1).EQ.' '.AND.   &
      NCD2(IROW3).EQ.1.AND.   &
      IDER21(IROW3,1).EQ.'0'.AND.IDER22(IROW3,1).EQ.' ')GO TO 2202
      GO TO 2209
 2202 CONTINUE
      K=1
      IDER31(2,K)='0'
      IDER32(2,K)=' '
      GO TO 2249
 2209 CONTINUE
!
      K=0
      K=K+1
      IDER31(2,K)='('
      IDER32(2,K)=' '
!
      IF(NCD2(IROW3).EQ.1.AND.   &
      IDER21(IROW3,1).EQ.'0'.AND.IDER22(IROW3,1).EQ.' ')GO TO 2222
!
      JMAX=NCF3(1)
      DO 2210 J=1,JMAX
      K=K+1
      IDER31(2,K)=IFUN31(1,J)
      IDER32(2,K)=IFUN32(1,J)
 2210 CONTINUE
!
      IF(NCD2(IROW3).EQ.1.AND.   &
      IDER21(IROW3,1).EQ.'0'.AND.IDER22(IROW3,1).EQ.' ')GO TO 2222
!
      K=K+1
      IDER31(2,K)='*'
      IDER32(2,K)=' '
!
      JMAX=NCD2(IROW3)
      DO 2220 J=1,JMAX
      K=K+1
      IDER31(2,K)=IDER21(IROW3,J)
      IDER32(2,K)=IDER22(IROW3,J)
 2220 CONTINUE
 2222 CONTINUE
!
      IF(NCD3(1).EQ.1.AND.   &
      IDER31(1,1).EQ.'0'.AND.IDER32(1,1).EQ.' ')GO TO 2242
!
      K=K+1
      IDER31(2,K)='+'
      IDER32(2,K)=' '
!
      JMAX=NCF2(IROW3)
      DO 2230 J=1,JMAX
      K=K+1
      IDER31(2,K)=IFUN21(IROW3,J)
      IDER32(2,K)=IFUN22(IROW3,J)
 2230 CONTINUE
!
      IF(NCD3(1).EQ.1.AND.   &
      IDER31(1,1).EQ.'1'.AND.IDER32(1,1).EQ.' ')GO TO 2242
!
      K=K+1
      IDER31(2,K)='*'
      IDER32(2,K)=' '
!
      JMAX=NCD3(1)
      DO 2240 J=1,JMAX
      K=K+1
      IDER31(2,K)=IDER31(1,J)
      IDER32(2,K)=IDER32(1,J)
 2240 CONTINUE
 2242 CONTINUE
!
      K=K+1
      IDER31(2,K)=')'
      IDER32(2,K)=' '
!
 2249 CONTINUE
      NCD3(2)=K
      GO TO 2400
!
!               ********************************
!               **  STEP 2.5--                **
!               **  TREAT THE DIVISION CASE.  **
!               ********************************
!
 2300 CONTINUE
!
      ISTEPN='2.5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV4')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(NCD3(1).EQ.1.AND.   &
      IDER31(1,1).EQ.'0'.AND.IDER32(1,1).EQ.' '.AND.   &
      NCD2(IROW3).EQ.1.AND.   &
      IDER21(IROW3,1).EQ.'0'.AND.IDER22(IROW3,1).EQ.' ')GO TO 2302
      GO TO 2309
 2302 CONTINUE
      K=1
      IDER31(2,K)='0'
      IDER32(2,K)=' '
      GO TO 2349
 2309 CONTINUE
!
      K=0
      K=K+1
      IDER31(2,K)='('
      IDER32(2,K)=' '
!
      K=K+1
      IDER31(2,K)='('
      IDER32(2,K)=' '
!
      IF(NCD3(1).EQ.1.AND.   &
      IDER31(1,1).EQ.'0'.AND.IDER32(1,1).EQ.' ')GO TO 2322
!
      JMAX=NCF2(IROW3)
      DO 2310 J=1,JMAX
      K=K+1
      IDER31(2,K)=IFUN21(IROW3,J)
      IDER32(2,K)=IFUN22(IROW3,J)
 2310 CONTINUE
!
      IF(NCD3(1).EQ.1.AND.   &
      IDER31(1,1).EQ.'1'.AND.IDER32(1,1).EQ.' ')GO TO 2322
!
      K=K+1
      IDER31(2,K)='*'
      IDER32(2,K)=' '
!
      JMAX=NCD3(1)
      DO 2320 J=1,JMAX
      K=K+1
      IDER31(2,K)=IDER31(1,J)
      IDER32(2,K)=IDER32(1,J)
 2320 CONTINUE
 2322 CONTINUE
!
      IF(NCD2(IROW3).EQ.1.AND.   &
      IDER21(IROW3,1).EQ.'0'.AND.IDER22 (IROW3,1).EQ.' ')GO TO 2342
!
      K=K+1
      IDER31(2,K)='-'
      IDER32(2,K)=' '
!
      JMAX=NCF3(1)
      DO 2330 J=1,JMAX
      K=K+1
      IDER31(2,K)=IFUN31(1,J)
      IDER32(2,K)=IFUN32(1,J)
 2330 CONTINUE
!
      IF(NCD2(IROW3).EQ.1.AND.   &
      IDER21(IROW3,1).EQ.'1'.AND.IDER22 (IROW3,1).EQ.' ')GO TO 2342
!
      K=K+1
      IDER31(2,K)='*'
      IDER32(2,K)=' '
!
      JMAX=NCD2(IROW3)
      DO 2340 J=1,JMAX
      K=K+1
      IDER31(2,K)=IDER21(IROW3,J)
      IDER32(2,K)=IDER22(IROW3,J)
 2340 CONTINUE
 2342 CONTINUE
!
      K=K+1
      IDER31(2,K)=')'
      IDER32(2,K)=' '
!
      K=K+1
      IDER31(2,K)='/'
      IDER32(2,K)=' '
!
      K=K+1
      IDER31(2,K)='('
      IDER32(2,K)=' '
!
      JMAX=NCF2(IROW3)
      DO 2350 J=1,JMAX
      K=K+1
      IDER31(2,K)=IFUN21(IROW3,J)
      IDER32(2,K)=IFUN22(IROW3,J)
 2350 CONTINUE
!
      K=K+1
      IDER31(2,K)='**'
      IDER32(2,K)='  '
      K=K+1
      IDER31(2,K)='2'
      IDER32(2,K)=' '
      K=K+1
      IDER31(2,K)=')'
      IDER32(2,K)=' '
!
      K=K+1
      IDER31(2,K)=')'
      IDER32(2,K)=' '
!
 2349 CONTINUE
      NCD3(2)=K
      GO TO 2400
!
 2400 CONTINUE
!
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'RIV4')GO TO 2100
      WRITE(ICOUT,2401)
 2401 FORMAT('***** IN THE MIDDLE OF DERIV4--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2407)IROW3,NCF2(IROW3),NCD2(IROW3)
 2407 FORMAT('IROW3, NCF2(IROW3), NCD2(IROW3) = ',3I6)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2408)IROW3,NCF3(2),NCD3(2)
 2408 FORMAT('IROW3, NCF3(2), NCD3(2) = ',3I6)
      CALL DPWRST('XXX','BUG ')
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IMAX=NCF2(IROW3)
      DO 2440 I=1,IMAX
      WRITE(ICOUT,2445)I,IFUN21(IROW3,I),IFUN22(IROW3,I)
 2445 FORMAT('I,IFUN21(IROW3,I),IFUN22(IROW3,I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 2440 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IMAX=NCD2(IROW3)
      DO 2450 I=1,IMAX
      WRITE(ICOUT,2455)I,IDER21(IROW3,I),IDER22(IROW3,I)
 2455 FORMAT('I,IDER21(IROW3,I),IDER22(IROW3,I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 2450 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IMAX=NCF3(2)
      DO 2460 I=1,IMAX
      WRITE(ICOUT,2465)I,IFUN31(IROW3,I),IFUN32(IROW3,I)
 2465 FORMAT('I,IFUN31(IROW3,I),IFUN32(IROW3,I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 2460 CONTINUE
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IMAX=NCD3(2)
      DO 2470 I=1,IMAX
      WRITE(ICOUT,2475)I,IDER31(IROW3,I),IDER32(IROW3,I)
 2475 FORMAT('I,IDER31(IROW3,I),IDER32(IROW3,I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 2470 CONTINUE
!
 2100 CONTINUE
 2900 CONTINUE
!
!               ****************************************
!               **  STEP 3--                          **
!               **  EXAMINE ROW 2     OF IDER31(.,.).  **
!               **  CHANGE ALL (+ TO (                **
!               ****************************************
!
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV4')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      JMAX=NCD3(2)
      IF(JMAX.LE.0)GO TO 3190
      K=0
      DO 3100 J=1,JMAX
      IF(J.EQ.1)GO TO 3110
      JM1=J-1
      IF(IDER31(2,JM1).EQ.'('.AND.IDER32(2,JM1).EQ.' '.AND.   &
      IDER31(2,J).EQ.'+'.AND.IDER32(2,J).EQ.' ')GO TO 3100
 3110 CONTINUE
      K=K+1
      IDER31(2,K)=IDER31(2,J)
      IDER32(2,K)=IDER32(2,J)
 3100 CONTINUE
      NCD3(2)=K
 3190 CONTINUE
!
!               *******************************************
!               **  STEP 4--                             **
!               **  COPY OVER THE DERIVATIVE             **
!               **  FROM ROW 2     OF IFUN31(.,.)         **
!               **  TO ROW IROW1 (FIXED) OF IFUN1(.,.).  **
!               *******************************************
 5000 CONTINUE
!
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV4')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      JMAX=NCD3(2)
      DO 5100 J=1,JMAX
      IDER11(IROW1,J)=IDER31(2,J)
      IDER12(IROW1,J)=IDER32(2,J)
 5100 CONTINUE
      NCD1(IROW1)=NCD3(2)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'RIV4')GO TO 9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('AT THE END       OF DERIV4--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IROW1
 9012 FORMAT('IROW1 = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)NCD1(IROW1)
 9013 FORMAT('NCD1(IROW1) = ',I8)
      CALL DPWRST('XXX','BUG ')
      ITEMP=NCD1(IROW1)
      DO 9020 J=1,ITEMP
      WRITE(ICOUT,9021)J,IDER11(IROW1,J),IDER12(IROW1,J)
 9021 FORMAT('J,IDER11(IROW1,J),IDER12(IROW1,J) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9020 CONTINUE
 9090 CONTINUE
!
      RETURN
      END SUBROUTINE DERIV4
      SUBROUTINE DERIVC(MODEL,NUMCHA,PARAM,IPARN,IPARN2,NUMPV,   &
      IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,   &
      IVARN,IVARN2,NUMVAR,X0,XDER,IBUGA3,IBUGCO,IBUGEV,IERROR)
!
!     PURPOSE--COMPUTE THE DERIVATIVE OF A FUNCTION
!              AT THE POINT X0.
!     ORIGINAL VERSION--NOVEMBER  1978.
!     UPDATED         --FEBRUARY  1979.
!     UPDATED         --JANUARY   1982.
!
!---------------------------------------------------------------------
!
      CHARACTER*4 MODEL
      CHARACTER*4 IPARN
      CHARACTER*4 IPARN2
      CHARACTER*4 IVARN
      CHARACTER*4 IVARN2
      CHARACTER*4 IANGLU
      CHARACTER*4 ITYPEH
      CHARACTER*4 IW21HO
      CHARACTER*4 IW22HO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGCO
      CHARACTER*4 IBUGEV
      CHARACTER*4 IERROR
!
      CHARACTER*4 IH
      CHARACTER*4 IH2
!
      DIMENSION MODEL(*)
      DIMENSION PARAM(*)
      DIMENSION IPARN(*)
      DIMENSION IPARN2(*)
      DIMENSION IVARN(*)
      DIMENSION IVARN2(*)
      DIMENSION ILOCV(10)
!
      DIMENSION ITYPEH(*)
      DIMENSION IW21HO(*)
      DIMENSION IW22HO(*)
      DIMENSION W2HOLD(*)
!
!-----COMMON VARIABLES (GENERAL)-----------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      CUTOFF=0.001
      ACCUR=0.0000001
      MAXIT=10
      IPASS=2
!
      J2=0
      H=0.0
      X0MH=0.0
      X0PH=0.0
      WIDTH=0.0
      XDER2=0.0
      RATIO2=0.0
!
      IF(IBUGA3.EQ.'OFF')GO TO 90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('AT THE BEGINNING OF DERIVC--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)NUMCHA,NUMPV,NUMVAR,IBUGA3
   52 FORMAT('NUMCHA,NUMPV,NUMVAR,IBUGA3 = ',4I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)(MODEL(J),J=1,NUMCHA)
   54 FORMAT('MODEL(I) = ',100A1)
      CALL DPWRST('XXX','BUG ')
      DO 55 I=1,NUMPV
      WRITE(ICOUT,56)I,IPARN(I),IPARN2(I),PARAM(I)
   56 FORMAT('I,IPARN(I),IPARN2(I),PARAM(I) = ',   &
      I8,2X,A4,2X,A4,E15.7)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
      WRITE(ICOUT,57)IANGLU
   57 FORMAT('IANGLU = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO 65 I=1,NUMVAR
      WRITE(ICOUT,66)I,IVARN(I),IVARN2(I)
   66 FORMAT('I,IVARN(I),IVARN2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,68)X0
   68 FORMAT('X0 = ',E15.8)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
!
!               ***************************************************
!               **  STEP 1--                                     **
!               **  DETERMINE THE LOCATIONS (IN THE LIST IPARN)  **
!               **  OF THE VARIABLES OF DIFFERENTIATION.         **
!               ***************************************************
!
      DO 100 I=1,NUMVAR
      IH=IVARN(I)
      IH2=IVARN2(I)
      DO 200 J=1,NUMPV
      J2=J
      IF(IH.EQ.IPARN(J).AND.IH2.EQ.IPARN2(J))GO TO 210
  200 CONTINUE
  210 CONTINUE
      ILOCV(I)=J2
  100 CONTINUE
!
!               ************************************************
!               **  STEP 3--                                  **
!               **  STEP THROUGH DIFFERENT WIDTHS             **
!               **  (HALVING THE WIDTHS FOR EACH ITERATION).  **
!               ************************************************
!
      IF(X0.LE.CUTOFF)H=CUTOFF
      IF(X0.GT.CUTOFF)H=X0*1.01
      DO 3100 NUMIT=1,MAXIT
!
!               ********************************************************
!               **  STEP 4--                                          **
!               **  FOR A GIVEN WIDTH (= 2*H), COMPUTE THE DIFFERENCE **
!               **  FORMULA D = (Y(X0+H) - Y(X0-H))/(2*H)             **
!               ********************************************************
!
      IF(NUMIT.GE.2)H=H/2.0
      X0MH=X0-H
      X0PH=X0+H
!
      X=X0MH
      DO 3410 K=1,NUMVAR
      JLOC=ILOCV(K)
      PARAM(JLOC)=X
 3410 CONTINUE
      CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM,IPARN,IPARN2,NUMPV,   &
      IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,Y0MH,   &
      IBUGCO,IBUGEV,IERROR)
!
      X=X0PH
      DO 3420 K=1,NUMVAR
      JLOC=ILOCV(K)
      PARAM(JLOC)=X
 3420 CONTINUE
      CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM,IPARN,IPARN2,NUMPV,   &
      IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,Y0PH,   &
      IBUGCO,IBUGEV,IERROR)
!
      IF(IBUGA3.EQ.'ON')WRITE(ICOUT,3402)X,Y0MH,Y0PH
 3402 FORMAT('X,Y0MH,Y0PH = ',3E15.8)
      IF(IBUGA3.EQ.'ON')CALL DPWRST('XXX','BUG ')
!
      WIDTH=2.0*H
      XDER=(Y0PH-Y0MH)/WIDTH
!
!               **************************************
!               **  STEP 5--                        **
!               **  WRITE OUT THE DERIVATIVE VALUE  **
!               **************************************
!
      WRITE(ICOUT,3103)WIDTH,XDER
 3103 FORMAT(E15.8,'* ',E15.8)
      CALL DPWRST('XXX','BUG ')
!
      IF(NUMIT.EQ.1)GO TO 3195
      ABSXDE=ABS(XDER)
!
      DIFF2=ABS(XDER-XDER2)
      IF(ABSXDE.LE.CUTOFF.AND.DIFF2.LE.ACCUR)GO TO 3170
      IF(ABSXDE.LE.CUTOFF.AND.DIFF2.GT.ACCUR)GO TO 3190
      RATIO2=ABS(DIFF2/XDER)
      IF(ABSXDE.GT.CUTOFF.AND.RATIO2.LE.ACCUR)GO TO 3170
      IF(ABSXDE.GT.CUTOFF.AND.RATIO2.GT.ACCUR)GO TO 3190
!
 3170 CONTINUE
      GO TO 3500
 3190 CONTINUE
      IF(IBUGA3.EQ.'ON')WRITE(ICOUT,3191)DIFF2,RATIO2,ABSXDE
 3191 FORMAT('DIFF2,RATIO2,ABSXDE = ',3E15.8)
      IF(IBUGA3.EQ.'ON')CALL DPWRST('XXX','BUG ')
!CCCC XDER3=XDER2
 3195 CONTINUE
      XDER2=XDER
!
 3100 CONTINUE
!
 3500 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3511)XDER
 3511 FORMAT('DERIVATIVE VALUE        = ',E15.8)
      CALL DPWRST('XXX','BUG ')
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('AT THE END       OF DERIVC--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)NUMCHA,NUMPV,NUMVAR,IBUGA3
 9012   FORMAT('NUMCHA,NUMPV,NUMVAR,IBUGA3 = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)(MODEL(J),J=1,MIN(100,NUMCHA))
 9014   FORMAT('MODEL(I) = ',100A1)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,NUMPV
          WRITE(ICOUT,9016)I,IPARN(I),IPARN2(I),PARAM(I)
 9016     FORMAT('I,IPARN(I),IPARN2(I),PARAM(I) = ',   &
                 I8,2(2X,A4),G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
        WRITE(ICOUT,9028)X0
 9028   FORMAT('X0 = ',E15.8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9031)H,WIDTH,X0MH,X0PH
 9031   FORMAT('H,WIDTH,X0MH,X0PH = ',4E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9032)Y0MH,Y0PH,XDER,XDER2
 9032   FORMAT('Y0MH,Y0PH,XDER,XDER2 = ',4E15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DERIVC
      SUBROUTINE DEXCDF(X,CDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
!              FUNCTION VALUE FOR THE DOUBLE EXPONENTIAL
!              (LAPLACE) DISTRIBUTION WITH MEAN = 0 AND
!              STANDARD DEVIATION = SQRT(2).
!              THIS DISTRIBUTION IS DEFINED FOR ALL X AND HAS
!              THE PROBABILITY DENSITY FUNCTION
!              F(X) = 0.5*EXP(-ABS(X)).
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE CUMULATIVE DISTRIBUTION
!                                FUNCTION IS TO BE EVALUATED.
!     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE
!                                DISTRIBUTION FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION
!             FUNCTION VALUE CDF.
!     PRINTING--NONE.
!     RESTRICTIONS--NONE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--EXP.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--2, 1970, PAGES 22-36.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING LABORATORY (205.03)
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE:  301-921-2315
!     ORIGINAL VERSION--APRIL     1994.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!---------------------------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS.
!     NO INPUT ARGUMENT ERRORS POSSIBLE
!     FOR THIS DISTRIBUTION.
!
!-----START POINT-----------------------------------------------------
!
      IF(X.LE.0.0)CDF=0.5*EXP(X)
      IF(X.GT.0.0)CDF=1.0-(0.5*EXP(-X))
!
      RETURN
      END SUBROUTINE DEXCDF
      SUBROUTINE DEXLI1(Y,N,ALOC,SCALE,   &
                        ALIK,AIC,AICC,BIC,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS ROUTINE COMPUTES THE LIKELIHOOD FUNCTION FOR
!              THE DOUBLE EXPONENTIAL (LAPLACE) DISTRIBUTION.  THIS
!              IS FOR THE RAW DATA CASE (I.E., NO GROUPING AND NO
!              CENSORING).
!
!              IT IS ASSUMED THAT BASIC ERROR CHECKING HAS ALREADY BEEN
!              PERFORMED.
!
!     REEFERENCE--NORTON, "THE DOUBLE EXPONENTIAL DISTRIBUTION: USING
!                 CALCULUS TO FIND A MAXIMUM LIKELIHOOD ESTIMATOR",
!                 THE AMERICAN STATISTICIAN, VOL. 28, NO. 2, 1984,
!                 PP. 135-136.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2010/6
!     ORIGINAL VERSION--JUNE      2010.
!     UPDATED  VERSION--MARCH     2021. CORRECT ERROR IN LOG-LIKELIHOOD
!                                       FUNCTION
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      DOUBLE PRECISION DX
      DOUBLE PRECISION DS
      DOUBLE PRECISION DU
      DOUBLE PRECISION DN
      DOUBLE PRECISION DNP
      DOUBLE PRECISION DLIK
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
      DOUBLE PRECISION DTERM3
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DEXL'
      ISUBN2='I1  '
!
      IERROR='NO'
!
      ALIK=-99.0
      AIC=-99.0
      AICC=-99.0
      BIC=-99.0
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'XLI1')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DEXLI1--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO
   52   FORMAT('IBUGA3,ISUBRO = ',A4,2X,A4)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,55)N,ALOC,SCALE
   55   FORMAT('N,ALOC,SCALE = ',I8,2G15.7)
        CALL DPWRST('XXX','WRIT')
        DO 56 I=1,MIN(N,100)
          WRITE(ICOUT,57)I,Y(I)
   57     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
!
!               ******************************************
!               **  STEP 1--                            **
!               **  COMPUTE LIKELIHOOD FUNCTION         **
!               ******************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'XLI1')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IERFLG=0
      IERROR='NO'
      IWRITE='OFF'
!
!     DOUBLE EXPONENTIAL LOG-LIKELIHOOD FUNCTION IS:
!
!     -N*LOG(2) - SUM[i=1 TO N][ABS(X(i) - LOC)/SCALE]
!
!
!     2021/03: LEFT OUT TERM IN LOG LIKELIHOOD FUNCTION
!
!     -N*LOG(2*SCALE) - SUM[i=1 TO N][ABS(X(i) - LOC)/SCALE]
!
      DN=DBLE(N)
      DS=DBLE(SCALE)
      DU=DBLE(ALOC)
      DTERM1=-DN*DLOG(2.0D0*DBLE(SCALE))
      DSUM1=0.0D0
      DO 1000 I=1,N
        DX=DBLE(Y(I))
        DTERM2=DABS(DX - DU)/DS
        DSUM1=DSUM1 + DTERM2
 1000 CONTINUE
!
      DLIK=DTERM1 - DSUM1
      ALIK=REAL(DLIK)
      DNP=2.0D0
      AIC=REAL(-2.0D0*DLIK + 2.0D0*DNP)
      DTERM3=(2.0D0*DNP*(DNP+1.0D0))/(DN-DNP-1.0D0)
      AICC=REAL(-2.0D0*DLIK + 2.0D0*DNP + DTERM3)
      BIC=REAL(-2.0D0*DLIK + DNP*LOG(DN))
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'XLI1')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('**** AT THE END OF DEXLI1--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9013)DSUM1,DTERM1,DTERM3
 9013   FORMAT('DSUM1,DTERM1,DTERM3 = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9014)ALIK,AIC,AICC,BIC
 9014   FORMAT('ALIK,AIC,AICC,BIC = ',4G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      RETURN
      END SUBROUTINE DEXLI1
      SUBROUTINE DEXML1(Y,N,XTEMP,ICASE,MAXNXT,   &
                        ALOWLO,AUPPLO,ALOWSC,AUPPSC,   &
                        ALPHA,NUMALP,NUMOUT,   &
                        XMEAN,XMED,XSD,XMIN,XMAX,   &
                        ALOC,ASCALE,   &
                        ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS ROUTINE COMPUTES THE MAXIMUM LIKELIHOOD ESTIMATES
!              FOR THE DOUBLE EXPONENTIAL (LAPLACE) DISTRIBUTION FOR
!              THE RAW DATA CASE (I.E., NO CENSORING AND NO GROUPING).
!              IT WILL OPTIONALLY RETURN THE CONFIDENCE INTERVALS FOR
!              THE LOCATION AND SCALE PARAMETERS.
!
!              IT IS ASSUMED THAT BASIC ERROR CHECKING HAS ALREADY BEEN
!              PERFORMED.
!
!              PUT THIS IN A SEPARATE ROUTINE AS IT MAY BE CALLED
!              FROM MULTIPLE PLACES (DPMLDE WILL GENERATE THE OUTPUT
!              FOR THE DOUBLE EXPONENTIAL MLE COMMAND).
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
!     VERSION NUMBER--2009/10
!     ORIGINAL VERSION--OCTOBER   2009. EXTRACTED AS A SEPARATE
!                                       SUBROUTINE (FROM DPMLDE)
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      DIMENSION ALOWLO(*)
      DIMENSION AUPPLO(*)
      DIMENSION ALOWSC(*)
      DIMENSION AUPPSC(*)
      DIMENSION ALPHA(*)
!
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      CHARACTER*4 IWRITE
      CHARACTER*40 IDIST
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      INTEGER IFLAG
      INTEGER ICASE
!
      DOUBLE PRECISION DN
      DOUBLE PRECISION DSUM
!
!---------------------------------------------------------------------
!
      DIMENSION Y(*)
      DIMENSION XTEMP(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DEXM'
      ISUBN2='L1  '
      IWRITE='OFF'
      IERROR='NO'
!
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'XML1')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DEXML1--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N,MAXNXT,ICASE
   52   FORMAT('IBUGA3,ISUBRO,N,MAXNXT,ICASE = ',2(A4,2X),3I8)
        CALL DPWRST('XXX','WRIT')
        DO 56 I=1,MIN(N,100)
          WRITE(ICOUT,57)I,Y(I)
   57     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
!
!               ******************************************
!               **  STEP 1--                            **
!               **  CARRY OUT CALCULATIONS              **
!               **  FOR DOUBLE EXPONENTIAL MLE ESTIMATE **
!               ******************************************
!
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'XML1')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IDIST='DOUBLE EXPONENTIAL'
      IFLAG=0
      CALL SUMRAW(Y,N,IDIST,IFLAG,   &
                  XMEAN,XVAR,XSD,XMIN,XMAX,   &
                  ISUBRO,IBUGA3,IERROR)
      CALL MEDIAN(Y,N,IWRITE,XTEMP,MAXNXT,XMED,IBUGA3,IERROR)
      ALOC=XMED
!
      DN=DBLE(N)
      DSUM=0.0D0
      DO 4110 I=1,N
        DSUM=DSUM + DBLE(ABS(Y(I) - XMED))
 4110 CONTINUE
      ASCALE=REAL(DSUM/DN)
!
      IF(ICASE.EQ.0)GO TO 9000
!
      AN=REAL(N)
      IDF=2*N-1
      DO 4120 I=1,NUMALP
!
        ALP=ALPHA(I)
        P1=ALP/2.0
        P2=1.0-(ALP/2.0)
!
        CALL CHSPPF(P1,IDF,AUPP)
        CALL CHSPPF(P2,IDF,ALOW)
        ALOWSC(I)=XMEAN + 2.0*REAL(DSUM)/ALOW
        AUPPSC(I)=XMEAN + 2.0*REAL(DSUM)/AUPP
!
        CALL NORPPF(P2,APPF2)
        ALOWLO(I)=ALOC - APPF2*REAL(DSUM)/(AN*SQRT(AN-APPF2**2))
        AUPPLO(I)=ALOC + APPF2*REAL(DSUM)/(AN*SQRT(AN-APPF2**2))
!
 4120 CONTINUE
      NUMOUT=NUMALP
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'XML1')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('**** AT THE END OF DEXML1--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9055)N,XMEAN,XMED,XSD,XMIN,XMAX
 9055   FORMAT('N,XMEAN,XMED,XSD,XMIN,XMAX = ',I8,5G15.7)
        CALL DPWRST('XXX','WRIT')
        DO 9060 I=1,NUMALP
          WRITE(ICOUT,9065)I,ALPHA(I),ALOWLO(I),AUPPLO(I),ALOWSC(I),   &
                           AUPPSC(I)
 9065     FORMAT('I,ALPHA(I),ALOWLO(I),AUPPLO(I),ALOWSC(I),AUPPSC(I)=',   &
                 I8,5G15.7)
          CALL DPWRST('XXX','WRIT')
 9060   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DEXML1
      SUBROUTINE DEXPDF(X,PDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
!              FUNCTION VALUE FOR THE DOUBLE EXPONENTIAL
!              (LAPLACE) DISTRIBUTION WITH MEAN = 0 AND
!              STANDARD DEVIAITON = SQRT(2).
!              THIS DISTRIBUTION IS DEFINED FOR ALL X AND HAS
!              THE PROBABILITY DENSITY FUNCTION
!              F(X) = 0.5*EXP(-ABS(X)).
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE PROBABILITY DENSITY
!                                FUNCTION IS TO BE EVALUATED.
!     OUTPUT ARGUMENTS--PDF    = THE SINGLE PRECISION PROBABILITY
!                                DENSITY FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
!             FUNCTION VALUE PDF.
!     PRINTING--NONE.
!     RESTRICTIONS--NONE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--EXP.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--2, 1970, PAGES 22-36.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING LABORATORY (205.03)
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE:  301-921-2315
!     ORIGINAL VERSION--APRIL     1994.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!---------------------------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS.
!     NO INPUT ARGUMENT ERRORS POSSIBLE
!     FOR THIS DISTRIBUTION.
!
!-----START POINT-----------------------------------------------------
!
      ARG=X
      IF(X.LT.0.0)ARG=-X
      PDF=0.5*EXP(-ARG)
!
      RETURN
      END SUBROUTINE DEXPDF
      SUBROUTINE DEXPPF(P,PPF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
!              FUNCTION VALUE FOR THE DOUBLE EXPONENTIAL
!              (LAPLACE) DISTRIBUTION WITH MEAN = 0 AND
!              STANDARD DEVIATION = SQRT(2).
!              THIS DISTRIBUTION IS DEFINED FOR ALL X AND HAS
!              THE PROBABILITY DENSITY FUNCTION
!              F(X) = 0.5*EXP(-ABS(X)).
!              NOTE THAT THE PERCENT POINT FUNCTION OF A DISTRIBUTION
!              IS IDENTICALLY THE SAME AS THE INVERSE CUMULATIVE
!              DISTRIBUTION FUNCTION OF THE DISTRIBUTION.
!     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE
!                                (BETWEEN 0.0 AND 1.0)
!                                AT WHICH THE PERCENT POINT
!                                FUNCTION IS TO BE EVALUATED.
!     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT
!                                POINT FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PERCENT POINT
!             FUNCTION VALUE PPF.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--P SHOULD BE BETWEEN 0.0 AND 1.0, EXCLUSIVELY.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--LOG.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--FILLIBEN, SIMPLE AND ROBUST LINEAR ESTIMATION
!                 OF THE LOCATION PARAMETER OF A SYMMETRIC
!                 DISTRIBUTION (UNPUBLISHED PH.D. DISSERTATION,
!                 PRINCETON UNIVERSITY), 1969, PAGES 21-44, 229-231.
!               --FILLIBEN, 'THE PERCENT POINT FUNCTION',
!                 (UNPUBLISHED MANUSCRIPT), 1970, PAGES 28-31.
!               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--2, 1970, PAGES 22-36.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-921-3651
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1966)
!               EXCEPTION--HOLLERITH STRINGS IN FORMAT STATEMENTS
!                          DENOTED BY QUOTES RATHER THAN NH.
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--JUNE      1972.
!     UPDATED         --SEPTEMBER 1975.
!     UPDATED         --NOVEMBER  1975.
!     UPDATED         --DECEMBER  1981.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(P.LE.0.0.OR.P.GE.1.0)GO TO 50
      GO TO 90
   50 WRITE(ICOUT,1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,46)P
      CALL DPWRST('XXX','BUG ')
      RETURN
   90 CONTINUE
    1 FORMAT('***** FATAL ERROR--THE 1ST INPUT ARGUMENT TO THE ',   &
      'DEXPPF SUBROUTINE IS OUTSIDE THE ALLOWABLE (0,1) INTERVAL')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',E15.8,' *****')
!
      PHOLD=P
!CCCC IF(PHOLD.LE.0.5)PPF=LOG(2.0*PHOLD)
!CCCC IF(PHOLD.GT.0.5)PPF=-LOG(2.0*(1.0-PHOLD))
      IF(PHOLD.LE.0.5)PPF=LOG(2.0*PHOLD)
      IF(PHOLD.GT.0.5)PPF=-LOG(2.0*(1.0-PHOLD))
!
      RETURN
      END SUBROUTINE DEXPPF
      DOUBLE PRECISION FUNCTION DEXPRL (X)
!***BEGIN PROLOGUE  DEXPRL
!***PURPOSE  Calculate the relative error exponential (EXP(X)-1)/X.
!***LIBRARY   SLATEC (FNLIB)
!***CATEGORY  C4B
!***TYPE      DOUBLE PRECISION (EXPREL-S, DEXPRL-D, CEXPRL-C)
!***KEYWORDS  ELEMENTARY FUNCTIONS, EXPONENTIAL, FIRST ORDER, FNLIB
!***AUTHOR  Fullerton, W., (LANL)
!***DESCRIPTION
!
! Evaluate  EXPREL(X) = (EXP(X) - 1.0) / X.   For small ABS(X) the
! Taylor series is used.  If X is negative the reflection formula
!         EXPREL(X) = EXP(X) * EXPREL(ABS(X))
! may be used.  This reflection formula will be of use when the
! evaluation for small ABS(X) is done by Chebyshev series rather than
! Taylor series.
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  D1MACH
!***REVISION HISTORY  (YYMMDD)
!   770801  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890911  Removed unnecessary intrinsics.  (WRB)
!   890911  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!***END PROLOGUE  DEXPRL
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DOUBLE PRECISION X, ABSX, ALNEPS, XBND, XLN, XN
      LOGICAL FIRST
      SAVE NTERMS, XBND, FIRST
      DATA FIRST /.TRUE./
!
      DEXPRL = 0.0D0
!
!***FIRST EXECUTABLE STATEMENT  DEXPRL
      IF (FIRST) THEN
         ALNEPS = LOG(D1MACH(3))
         XN = 3.72D0 - 0.3D0*ALNEPS
         XLN = LOG((XN+1.0D0)/1.36D0)
         NTERMS = INT(XN - (XN*XLN+ALNEPS)/(XLN+1.36D0) + 1.5D0)
         XBND = D1MACH(3)
      ENDIF
      FIRST = .FALSE.
!
      ABSX = ABS(X)
      IF (ABSX.GT.0.5D0) DEXPRL = (EXP(X)-1.0D0)/X
      IF (ABSX.GT.0.5D0) RETURN
!
      DEXPRL = 1.0D0
      IF (ABSX.LT.XBND) RETURN
!
      DEXPRL = 0.0D0
      DO 20 I=1,NTERMS
        DEXPRL = 1.0D0 + DEXPRL*X/(NTERMS+2-I)
 20   CONTINUE
!
      RETURN
      END FUNCTION DEXPRL 
      SUBROUTINE DEXRAN(N,ISEED,X)
!
!     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
!              FROM THE DOUBLE EXPONENTIAL
!              (LAPLACE) DISTRIBUTION WITH MEAN = 0 AND
!              STANDARD DEVIATION = SQRT(2).
!              THIS DISTRIBUTION IS DEFINED FOR ALL X AND HAS
!              THE PROBABILITY DENSITY FUNCTION
!              F(X) = 0.5*EXP(-ABS(X)).
!     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
!                                OF RANDOM NUMBERS TO BE
!                                GENERATED.
!     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
!                                (OF DIMENSION AT LEAST N)
!                                INTO WHICH THE GENERATED
!                                RANDOM SAMPLE WILL BE PLACED.
!     OUTPUT--A RANDOM SAMPLE OF SIZE N
!             FROM THE DOUBLE EXPONENTIAL
!             (LAPLACE) DISTRIBUTION WITH MEAN = 0 AND
!             STANDARD DEVIATION = SQRT(2).
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--LOG.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--TOCHER, THE ART OF SIMULATION,
!                 1963, PAGES 14-15.
!               --HAMMERSLEY AND HANDSCOMB, MONTE CARLO METHODS,
!                 1964, PAGE 36.
!               --FILLIBEN, SIMPLE AND ROBUST LINEAR ESTIMATION
!                 OF THE LOCATION PARAMETER OF A SYMMETRIC
!                 DISTRIBUTION (UNPUBLISHED PH.D. DISSERTATION,
!                 PRINCETON UNIVERSITY), 1969, PAGE 231.
!               --FILLIBEN, 'THE PERCENT POINT FUNCTION',
!                 (UNPUBLISHED MANUSCRIPT), 1970, PAGES 28-31.
!               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--2, 1970, PAGES 22-36.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-921-3651
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1966)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--JUNE      1972.
!     UPDATED         --SEPTEMBER 1975.
!     UPDATED         --NOVEMBER  1975.
!     UPDATED         --DECEMBER  1981.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(N.LT.1)GO TO 50
      GO TO 90
   50 WRITE(ICOUT, 5)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,47)N
      CALL DPWRST('XXX','BUG ')
      RETURN
   90 CONTINUE
    5 FORMAT('***** FATAL ERROR--THE 1ST INPUT ARGUMENT TO THE ',   &
      'DEXRAN SUBROUTINE IS NON-POSITIVE *****')
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8,' *****')
!
!     GENERATE N UNIFORM (0,1) RANDOM NUMBERS;
!
      CALL UNIRAN(N,ISEED,X)
!
!     GENERATE N DOUBLE EXPONENTIAL RANDOM NUMBERS
!     USING THE PERCENT POINT FUNCTION TRANSFORMATION METHOD.
!
      DO 100 I=1,N
      Q=X(I)
!CCCC IF(Q.LE.0.5)X(I)=LOG(2.0*Q)
!CCCC IF(Q.GT.0.5)X(I)=-LOG(2.0*(1.0-Q))
      IF(Q.LE.0.5)X(I)=LOG(2.0*Q)
      IF(Q.GT.0.5)X(I)=-LOG(2.0*(1.0-Q))
  100 CONTINUE
!
      RETURN
      END SUBROUTINE DEXRAN
      SUBROUTINE DEXSF(P,SF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE SPARSITY
!              FUNCTION VALUE FOR THE DOUBLE EXPONENTIAL
!              (LAPLACE) DISTRIBUTION WITH MEAN = 0 AND
!              STANDARD DEVIATION = SQRT(2).
!              THIS DISTRIBUTION IS DEFINED FOR ALL X AND HAS
!              THE PROBABILITY DENSITY FUNCTION
!              F(X) = 0.5*EXP(-ABS(X)).
!              NOTE THAT THE SPARSITY FUNCTION OF A DISTRIBUTION
!              IS THE DERIVATIVE OF THE PERCENT POINT FUNCTION,
!              AND ALSO IS THE RECIPROCAL OF THE PROBABILITY
!              DENSITY FUNCTION (BUT IN UNITS OF P RATHER THAN X).
!     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE
!                                (BETWEEN 0.0 AND 1.0)
!                                AT WHICH THE SPARSITY
!                                FUNCTION IS TO BE EVALUATED.
!     OUTPUT ARGUMENTS--SF     = THE SINGLE PRECISION
!                                SPARSITY FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION SPARSITY
!             FUNCTION VALUE SF.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--P SHOULD BE BETWEEN 0.0 AND 1.0, EXCLUSIVELY.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--FILLIBEN, SIMPLE AND ROBUST LINEAR ESTIMATION
!                 OF THE LOCATION PARAMETER OF A SYMMETRIC
!                 DISTRIBUTION (UNPUBLISHED PH.D. DISSERTATION,
!                 PRINCETON UNIVERSITY), 1969, PAGES 21-44, 229-231.
!               --FILLIBEN, 'THE PERCENT POINT FUNCTION',
!                 (UNPUBLISHED MANUSCRIPT), 1970, PAGES 28-31.
!               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--2, 1970, PAGES 22-36.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING LABORATORY (205.03)
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE:  301-921-2315
!     ORIGINAL VERSION--APRIL     1994.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!---------------------------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(P.LE.0.0.OR.P.GE.1.0)GO TO 50
      GO TO 90
   50 WRITE(ICOUT,1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,46)P
      CALL DPWRST('XXX','BUG ')
      RETURN
   90 CONTINUE
    1 FORMAT('***** FATAL ERROR--THE 1ST INPUT ARGUMENT TO THE ',   &
      ' DEXSF SUBROUTINE IS OUTSIDE THE ALLOWABLE (0,1) INTERVAL')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',E15.8,' *****')
!
      IF(P.LE.0.5)SF=1.0/P
      IF(P.GT.0.5)SF=1.0/(1.0-P)
!
      RETURN
      END SUBROUTINE DEXSF
      DOUBLE PRECISION FUNCTION DFAC (N)
!***BEGIN PROLOGUE  DFAC
!***PURPOSE  Compute the factorial function.
!***LIBRARY   SLATEC (FNLIB)
!***CATEGORY  C1
!***TYPE      DOUBLE PRECISION (FAC-S, DFAC-D)
!***KEYWORDS  FACTORIAL, FNLIB, SPECIAL FUNCTIONS
!***AUTHOR  Fullerton, W., (LANL)
!***DESCRIPTION
!
! DFAC(N) calculates the double precision factorial for integer
! argument N.
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  D9LGMC, DGAMLM, XERMSG
!***REVISION HISTORY  (YYMMDD)
!   770601  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890531  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!***END PROLOGUE  DFAC
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DOUBLE PRECISION FACN(31), SQ2PIL, X, XMAX, XMIN,  D9LGMC
      SAVE FACN, SQ2PIL, NMAX
      DATA FACN  (  1) / +.100000000000000000000000000000000D+1    /
      DATA FACN  (  2) / +.100000000000000000000000000000000D+1    /
      DATA FACN  (  3) / +.200000000000000000000000000000000D+1    /
      DATA FACN  (  4) / +.600000000000000000000000000000000D+1    /
      DATA FACN  (  5) / +.240000000000000000000000000000000D+2    /
      DATA FACN  (  6) / +.120000000000000000000000000000000D+3    /
      DATA FACN  (  7) / +.720000000000000000000000000000000D+3    /
      DATA FACN  (  8) / +.504000000000000000000000000000000D+4    /
      DATA FACN  (  9) / +.403200000000000000000000000000000D+5    /
      DATA FACN  ( 10) / +.362880000000000000000000000000000D+6    /
      DATA FACN  ( 11) / +.362880000000000000000000000000000D+7    /
      DATA FACN  ( 12) / +.399168000000000000000000000000000D+8    /
      DATA FACN  ( 13) / +.479001600000000000000000000000000D+9    /
      DATA FACN  ( 14) / +.622702080000000000000000000000000D+10   /
      DATA FACN  ( 15) / +.871782912000000000000000000000000D+11   /
      DATA FACN  ( 16) / +.130767436800000000000000000000000D+13   /
      DATA FACN  ( 17) / +.209227898880000000000000000000000D+14   /
      DATA FACN  ( 18) / +.355687428096000000000000000000000D+15   /
      DATA FACN  ( 19) / +.640237370572800000000000000000000D+16   /
      DATA FACN  ( 20) / +.121645100408832000000000000000000D+18   /
      DATA FACN  ( 21) / +.243290200817664000000000000000000D+19   /
      DATA FACN  ( 22) / +.510909421717094400000000000000000D+20   /
      DATA FACN  ( 23) / +.112400072777760768000000000000000D+22   /
      DATA FACN  ( 24) / +.258520167388849766400000000000000D+23   /
      DATA FACN  ( 25) / +.620448401733239439360000000000000D+24   /
      DATA FACN  ( 26) / +.155112100433309859840000000000000D+26   /
      DATA FACN  ( 27) / +.403291461126605635584000000000000D+27   /
      DATA FACN  ( 28) / +.108888694504183521607680000000000D+29   /
      DATA FACN  ( 29) / +.304888344611713860501504000000000D+30   /
      DATA FACN  ( 30) / +.884176199373970195454361600000000D+31   /
      DATA FACN  ( 31) / +.265252859812191058636308480000000D+33   /
      DATA SQ2PIL / 0.91893853320467274178032973640562D0 /
      DATA NMAX / 0 /
!***FIRST EXECUTABLE STATEMENT  DFAC
!
      DFAC=0.0D0
!
      IF (NMAX.NE.0) GO TO 10
      CALL DGAMLM (XMIN, XMAX)
      NMAX = INT(XMAX - 1.D0)
!
 10   IF (N .LT. 0) THEN
        WRITE(ICOUT,1)
    1   FORMAT('***** ERORR FROM DFAC, THE FACTORIAL OF A NEGATIVE',   &
               ' NUMBER IS UNDEFINED. *****')
        CALL DPWRST('XXX','BUG ')
        RETURN
      ENDIF
!
      IF (N.LE.30) DFAC = FACN(N+1)
      IF (N.LE.30) RETURN
!
      IF (N .GT. NMAX) THEN
        WRITE(ICOUT,2)
    2   FORMAT('***** ERORR FROM DFAC, THE ARGUMENT IS SO BIG THAT ',   &
               ' THE FACTORIAL OVERFLOWS. *****')
        CALL DPWRST('XXX','BUG ')
        RETURN
      ENDIF
!
      X = REAL(N + 1)
      DFAC = EXP ((X-0.5D0)*LOG(X) - X + SQ2PIL + D9LGMC(X) )
!
      RETURN
      END FUNCTION DFAC 
      DOUBLE PRECISION FUNCTION DFRENC (X,MODE)
!
!     .  COPYRIGHT (C) 1992, CALIFORNIA INSTITUTE OF TECHNOLOGY.
!     .  U. S. GOVERNMENT SPONSORSHIP UNDER
!     .  NASA CONTRACT NAS7-918 IS ACKNOWLEDGED.
!>> ALAN HECKERT MODIFIED FOR INCLUSION INTO DATAPLOT (BASICALLY,
!   PASS MODE AS ARGUMENT AND ELIMINATE MULTIPLE ENTRY POINTS.
!   ALSO, DELETED COMMENT LINES FOR COEFFICIENTS USING DIFFERENT
!   ORDERS OF APPROXIMATION.
!>> 1992-09-15 DFRENL WV SNYDER SPECIALIZING INSTRUCTIONS
!>> 1992-04-13 DFRENL WV SNYDER DECLARE DFRENF, DFRENG, DFRENS
!>> 1992-03-18 DFRENL WV SNYDER MOVE DECLARATIONS FOR COEFFICIENT ARRAYS
!>> 1992-01-24 DFRENL WV SNYDER ORIGINAL CODE
! ENTRIES IN THIS SUBPROGRAM COMPUTE THE FRESNEL COSINE AND SINE
! INTEGRALS C(X) AND S(X), AND THE AUXILIARY FUNCTIONS F(X) AND G(X),
! FOR ANY X:
!     DFRENC(X) FOR FRESNEL INTEGRAL C(X)
!     DFRENS(X) FOR FRESNEL INTEGRAL S(X)
!     DFRENF(X) FOR FRESNEL INTEGRAL AUXILIARY FUNCTION F(X)
!     DFRENG(X) FOR FRESNEL INTEGRAL AUXILIARY FUNCTION G(X).
!
! DEVELOPED BY W. V. SNYDER, JET PROPULSION LABORATORY, 24 JANUARY 1992.
!
! REF: W. J. CODY, "CHEBYSHEV APPROXIMATIONS FOR THE FRESNEL INTEGRALS",
! MATHEMATICS OF COMPUTATION, 1968, PP 450-453 PLUS MICROFICHE SUPPL.
! ACCURACIES OF HIGHEST ORDER FORMULAE, WHERE E IS RELATIVE ERROR:
!
! RANGE           FUNCTION   -LOG10(E)   FUNCTION   -LOG10(E)
! |X|<=1.2          C(X)       16.24       S(X)       17.26
! 1.2<|X|<=1.6      C(X)       17.47       S(X)       18.66
! 1.6<|X|<=1.9      F(X)       17.13       G(X)       16.25
! 1.9<|X|<=2.4      F(X)       16.64       G(X)       15.65
! 2.4<|X|           F(X)       16.89       G(X)       15.58
!
! REFER TO CODY FOR ACCURACY OF OTHER APPROXIMATIONS.
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION X
!
!--   S VERSION USES SFRENC,SFRENC,SFRENF,SFRENG,SFRENS,R1MACH,R1MACH
!--   D VERSION USES DFRENC,DFRENC,DFRENF,DFRENG,DFRENS,D1MACH,D1MACH
!
! DFRENF, DFRENG, DFRENS ARE ALTERNATE ENTRIES.
!CCCC DOUBLE PRECISION DFRENF, DFRENG, DFRENS
!
! PID2 IS PI / 2.
      DOUBLE PRECISION PID2
      PARAMETER (PID2 = 1.570796326794896619231321691639751442099D0)
! RPI IS THE RECIPROCAL OF PI:
      DOUBLE PRECISION RPI
      PARAMETER (RPI = 0.3183098861837906715377675267450287240689D0)
! RPISQ IS THE RECIPROCAL OF PI SQUARED:
      DOUBLE PRECISION RPISQ
      PARAMETER (RPISQ = RPI * RPI)
! AX IS ABS(X).
! BIGX IS 1/SQRT(ROUND-OFF).  IF X > BIGX THEN TO THE WORKING
!         PRECISION X**2 IS AN INTEGER (WHICH WE ASSUME TO BE A MULTIPLE
!         OF FOUR), SO COS(PI/2 * X**2) = 1, AND SIN(PI/2 * X**2) = 0.
! C AND S ARE VALUES OF C(X) AND S(X), RESPECTIVELY.
! CX AND SX ARE COS(PI/2 * AX**2) AND SIN(PI/2 * AX**2), RESPECTIVELY.
! F AND G ARE USED TO COMPUTE F(X) AND G(X) WHEN X > 1.6.
! HAVEC, HAVEF, HAVEG, HAVES ARE LOGICAL VARIABLES THAT INDICATE
!         WHETHER THE VALUES STORED IN C, F, G AND S CORRESPOND TO THE
!         VALUE STORED IN X.  HAVEF INDICATES WE HAVE BOTH F AND G WHEN
!         XSAVE .LE. 1.6, AND HAVEC INDICATES WE HAVE BOTH C AND S WHEN
!         XSAVE .GT. 1.6.
! LARGEF IS 1/(PI * UNDERFLOW).  IF X > LARGEF THEN F ~ 0.
! LARGEG IS CBRT(1/(PI**2 * UNDERFLOW)).  IF X > LARGEG THEN G ~ 0.
! LARGEX IS 1/SQRT(SQRT(UNDERFLOW)).  IF X > LARGEX THEN F ~ 1/(PI * X)
!         AND G ~ 1/(PI**2 * X**3).
! MODE INDICATES THE FUNCTION TO BE COMPUTED: 1 = C(X), 2 = S(X),
!         3 = F(X), 4 = G(X).
! NEEDC, NEEDF, NEEDG, NEEDS ARE ARRAYS INDEXED BY MODE (MODE+4 WHEN
!         X .GT. 1.6) THAT INDICATE WHAT FUNCTIONS ARE NEEDED.
! RESULT IS EQUIVALENCED TO C, F, G, AND S.
! WANTC INDICATES WHETHER C AND S MUST BE COMPUTED FROM F AND G.
! WANTF AND WANTG INDICATE WE COMPUTED F AND G ON THE PRESENT CALL.
! XSAVE IS THE MOST RECENTLY PROVIDED VALUE OF X.
! X4 IS EITHER X ** 4 OR (1.0/X) ** 4.
      DOUBLE PRECISION AX, BIGX, C, CX, F, G, LARGEF, LARGEG, LARGEX
      DOUBLE PRECISION RESULT(4), S, SX, XSAVE, X4
      SAVE BIGX, C, F, G, LARGEF, LARGEG, LARGEX, S, RESULT, XSAVE
      EQUIVALENCE (RESULT(1), C), (RESULT(2), S)
      EQUIVALENCE (RESULT(3), F), (RESULT(4), G)
      LOGICAL HAVEC, HAVEF, HAVEG, HAVES, WANTC, WANTF, WANTG
      SAVE HAVEC, HAVEF, HAVEG, HAVES
      INTEGER MODE
      LOGICAL NEEDC(8), NEEDF(8), NEEDG(8), NEEDS(8)
!
      INCLUDE 'DPCOMC.INC'
!
!     DECLARATIONS FOR COEFFICIENT ARRAYS.  IF YOU CHANGE THE ORDER OF
!     APPROXIMATION, YOU MUST CHANGE THE DECLARATION HERE, THE DATA
!     STATEMENTS BELOW, AND THE EXECUTABLE STATEMENTS THAT EVALUATE
!     THE APPROXIMATIONS.
      DOUBLE PRECISION PC1(0:4), QC1(1:4)
      DOUBLE PRECISION PC2(0:5), QC2(1:5)
      DOUBLE PRECISION PS1(0:4), QS1(1:4)
      DOUBLE PRECISION PS2(0:5), QS2(1:5)
      DOUBLE PRECISION PF1(0:5), QF1(1:5)
      DOUBLE PRECISION PF2(0:5), QF2(1:5)
      DOUBLE PRECISION PF3(0:6), QF3(1:6)
      DOUBLE PRECISION PG1(0:5), QG1(1:5)
      DOUBLE PRECISION PG2(0:5), QG2(1:5)
      DOUBLE PRECISION PG3(0:6), QG3(1:6)
!
      DATA BIGX /-1.0D0/
      DATA C /0.0D0/, F /0.5D0/, G /0.5D0/, S /0.0D0/, XSAVE /0.0D0/
      DATA HAVEC/.TRUE./, HAVEF/.TRUE./, HAVEG/.TRUE./, HAVES/.TRUE./
!        C(X)    S(X)    F(X)    G(X)    C(X)    S(X)    F(X)    G(X)
      DATA NEEDC   &
       /.TRUE., .FALSE.,.TRUE., .TRUE., .TRUE., .FALSE.,.FALSE.,.FALSE./
      DATA NEEDS   &
       /.FALSE.,.TRUE., .TRUE., .TRUE., .FALSE.,.TRUE., .FALSE.,.FALSE./
      DATA NEEDF   &
       /.FALSE.,.FALSE.,.TRUE., .FALSE.,.TRUE., .TRUE., .TRUE., .FALSE./
      DATA NEEDG   &
       /.FALSE.,.FALSE.,.FALSE.,.TRUE. ,.TRUE., .TRUE., .FALSE.,.TRUE. /
!
!     COEFFICIENTS FOR C(X), |X| <= 1.2
!
      DATA PC1(0) / 9.999999999999999421D-1/
      DATA PC1(1) /-1.994608988261842706D-1/
      DATA QC1(1) / 4.727921120104532689D-2/
      DATA PC1(2) / 1.761939525434914045D-2/
      DATA QC1(2) / 1.099572150256418851D-3/
      DATA PC1(3) /-5.280796513726226960D-4/
      DATA QC1(3) / 1.552378852769941331D-5/
      DATA PC1(4) / 5.477113856826871660D-6/
      DATA QC1(4) / 1.189389014228757184D-7/
!
!     COEFFICIENTS FOR C(X), 1.2 < |X| <= 1.6
      DATA PC2(0) / 1.00000000000111043640D0 /
      DATA PC2(1) /-2.07073360335323894245D-1/
      DATA QC2(1) / 3.96667496952323433510D-2/
      DATA PC2(2) / 1.91870279431746926505D-2/
      DATA QC2(2) / 7.88905245052359907842D-4/
      DATA PC2(3) /-6.71376034694922109230D-4/
      DATA QC2(3) / 1.01344630866749406081D-5/
      DATA PC2(4) / 1.02365435056105864908D-5/
      DATA QC2(4) / 8.77945377892369265356D-8/
      DATA PC2(5) /-5.68293310121870728343D-8/
      DATA QC2(5) / 4.41701374065009620393D-10/
!
!     COEFFICIENTS FOR S(X), |X| <= 1.2
      DATA PS1(0) / 5.2359877559829887021D-1/
      DATA PS1(1) /-7.0748991514452302596D-2/
      DATA QS1(1) / 4.1122315114238422205D-2/
      DATA PS1(2) / 3.8778212346368287939D-3/
      DATA QS1(2) / 8.1709194215213447204D-4/
      DATA PS1(3) /-8.4555728435277680591D-5/
      DATA QS1(3) / 9.6269087593903403370D-6/
      DATA PS1(4) / 6.7174846662514086196D-7/
      DATA QS1(4) / 5.9528122767840998345D-8/
!
!     COEFFICIENTS FOR S(X), 1.2 < |X| <= 1.6
      DATA PS2(0) / 5.23598775598344165913D-1/
      DATA PS2(1) /-7.37766914010191323867D-2/
      DATA QS2(1) / 3.53398342767472162540D-2/
      DATA PS2(2) / 4.30730526504366510217D-3/
      DATA QS2(2) / 6.18224620195473216538D-4/
      DATA PS2(3) /-1.09540023911434994566D-4/
      DATA QS2(3) / 6.87086265718620117905D-6/
      DATA PS2(4) / 1.28531043742724820610D-6/
      DATA QS2(4) / 5.03090581246612375866D-8/
      DATA PS2(5) /-5.76765815593088804567D-9/
      DATA QS2(5) / 2.05539124458579596075D-10/
!
!     COEFFICIENTS FOR F(X), 1.6 < |X| <= 1.9
      DATA PF1(0) / 3.1830975293580985290D-1/
      DATA PF1(1) / 1.2226000551672961219D1 /
      DATA QF1(1) / 3.8713003365583442831D1 /
      DATA PF1(2) / 1.2924886131901657025D2 /
      DATA QF1(2) / 4.1674359830705629745D2 /
      DATA PF1(3) / 4.3886367156695547655D2 /
      DATA QF1(3) / 1.4740030733966610568D3 /
      DATA PF1(4) / 4.1466722177958961672D2 /
      DATA QF1(4) / 1.5371675584895759916D3 /
      DATA PF1(5) / 5.6771463664185116454D1 /
      DATA QF1(5) / 2.9113088788847831515D2 /
!
!     COEFFICIENTS FOR F(X), 1.9 < |X| <= 2.4
      DATA PF2(0) / 3.183098818220169217D-1/
      DATA PF2(1) / 1.958839410219691002D1 /
      DATA QF2(1) / 6.184271381728873709D1 /
      DATA PF2(2) / 3.398371349269842400D2 /
      DATA QF2(2) / 1.085350675006501251D3 /
      DATA PF2(3) / 1.930076407867157531D3 /
      DATA QF2(3) / 6.337471558511437898D3 /
      DATA PF2(4) / 3.091451615744296552D3 /
      DATA QF2(4) / 1.093342489888087888D4 /
      DATA PF2(5) / 7.177032493651399590D2 /
      DATA QF2(5) / 3.361216991805511494D3 /
!
!     COEFFICIENTS FOR F(X), 2.4 < |X|
      DATA PF3(0) /-9.675460329952532343D-2/
      DATA PF3(1) /-2.431275407194161683D1 /
      DATA QF3(1) / 2.548289012949732752D2 /
      DATA PF3(2) /-1.947621998306889176D3 /
      DATA QF3(2) / 2.099761536857815105D4 /
      DATA PF3(3) /-6.059852197160773639D4 /
      DATA QF3(3) / 6.924122509827708985D5 /
      DATA PF3(4) /-7.076806952837779823D5 /
      DATA QF3(4) / 9.178823229918143780D6 /
      DATA PF3(5) /-2.417656749061154155D6 /
      DATA QF3(5) / 4.292733255630186679D7 /
      DATA PF3(6) /-7.834914590078317336D5 /
      DATA QF3(6) / 4.803294784260528342D7 /
!
!     COEFFICIENTS FOR G(X), 1.6 < |X| <= 1.9
      DATA PG1(0) / 1.013206188102747985D-1/
      DATA PG1(1) / 4.445338275505123778D0 /
      DATA QG1(1) / 4.539250196736893605D1 /
      DATA PG1(2) / 5.311228134809894481D1 /
      DATA QG1(2) / 5.835905757164290666D2 /
      DATA PG1(3) / 1.991828186789025318D2 /
      DATA QG1(3) / 2.544731331818221034D3 /
      DATA PG1(4) / 1.962320379716626191D2 /
      DATA QG1(4) / 3.481121478565452837D3 /
      DATA PG1(5) / 2.054214324985006303D1 /
      DATA QG1(5) / 1.013794833960028555D3 /
!
!     COEFFICIENTS FOR G(X), 1.9 < |X| <= 2.4
      DATA PG2(0) / 1.01321161761804586D-1/
      DATA PG2(1) / 7.11205001789782823D0 /
      DATA QG2(1) / 7.17128596939302198D1 /
      DATA PG2(2) / 1.40959617911315524D2 /
      DATA QG2(2) / 1.49051922797329229D3 /
      DATA PG2(3) / 9.08311749529593938D2 /
      DATA QG2(3) / 1.06729678030580897D4 /
      DATA PG2(4) / 1.59268006085353864D3 /
      DATA QG2(4) / 2.41315567213369742D4 /
      DATA PG2(5) / 3.13330163068755950D2 /
      DATA QG2(5) / 1.15149832376260604D4 /
!
!     COEFFICIENTS FOR G(X), 2.4 < |X|
      DATA PG3(0) /-1.53989733819769316D-1/
      DATA PG3(1) /-4.31710157823357568D1 /
      DATA QG3(1) / 2.86733194975899483D2 /
      DATA PG3(2) /-3.87754141746378493D3 /
      DATA QG3(2) / 2.69183180396242536D4 /
      DATA PG3(3) /-1.35678867813756347D5 /
      DATA QG3(3) / 1.02878693056687506D6 /
      DATA PG3(4) /-1.77758950838029676D6 /
      DATA QG3(4) / 1.62095600500231646D7 /
      DATA PG3(5) /-6.66907061668636416D6 /
      DATA QG3(5) / 9.38695862531635179D7 /
      DATA PG3(6) /-1.72590224654836845D6 /
      DATA QG3(6) / 1.40622441123580005D8 /
!
!  MODE = 1 = FRESNEL COSINE INTEGRAL
!  MODE = 2 = FRESNEL SINE INTEGRAL
!  MODE = 3 = F AUXILLARY FUNCTION
!  MODE = 4 = G AUXILLARY FUNCTION
!
!     *****     EXECUTABLE STATEMENTS     ****************************
!
      IF (BIGX .LT. 0.0D0) THEN
         BIGX = 1.0D0 / SQRT(D1MACH(4))
         LARGEF = RPI / D1MACH(1)
         LARGEG = (RPI * LARGEF) ** (1.0D0 / 3.0D0)
         LARGEX = 1.0D0/SQRT(SQRT(D1MACH(1)))
      END IF
      IF (X .NE. XSAVE) THEN
         HAVEC = .FALSE.
         HAVEF = .FALSE.
         HAVEG = .FALSE.
         HAVES = .FALSE.
      END IF
      AX = ABS(X)
      IF (AX .LE. 1.6D0) THEN
         X4 = AX**4
         IF (NEEDC(MODE) .AND. .NOT. HAVEC) THEN
            IF (AX .LE. 1.2D0) THEN
               C = X * ((((PC1(4)*X4+PC1(3))*X4+PC1(2))*X4+PC1(1))*X4+   &
                           PC1(0))   &
                 / ((((QC1(4)*X4+QC1(3))*X4+QC1(2))*X4+QC1(1))*X4+1.0D0)
            ELSE
               C = X * (((((PC2(5)*X4+PC2(4))*X4+PC2(3))*X4+PC2(2))*X4+   &
                           PC2(1))*X4+PC2(0))   &
                 /   (((((QC2(5)*X4+QC2(4))*X4+QC2(3))*X4+QC2(2))*X4+   &
                         QC2(1))*X4+1.0D0)
            END IF
            HAVEC = .TRUE.
         END IF
         IF (NEEDS(MODE) .AND. .NOT. HAVES) THEN
            IF (AX .LE. 1.2D0) THEN
               S = X**3*((((PS1(4)*X4+PS1(3))*X4+PS1(2))*X4+PS1(1))*X4+   &
                            PS1(0))   &
                 / ((((QS1(4)*X4+QS1(3))*X4+QS1(2))*X4+QS1(1))*X4+1.0D0)
            ELSE
               S = X**3*(((((PS2(5)*X4+PS2(4))*X4+PS2(3))*X4+PS2(2))*X4+   &
                            PS2(1))*X4+PS2(0))   &
                 /   (((((QS2(5)*X4+QS2(4))*X4+QS2(3))*X4+QS2(2))*X4+   &
                          QS2(1))*X4+1.0D0)
            END IF
            HAVES = .TRUE.
         END IF
         IF ((NEEDF(MODE) .OR. NEEDG(MODE)) .AND. .NOT. HAVEF) THEN
            CX = COS(PID2 * AX*AX)
            SX = SIN(PID2 * AX*AX)
            F = (0.5D0 - S) * CX - (0.5D0 - C) * SX
            G = (0.5D0 - C) * CX + (0.5D0 - S) * SX
            HAVEF = .TRUE.
         END IF
      ELSE
         IF (AX .LE. LARGEX) THEN
            X4 = (1.0D0 / AX) ** 4
            WANTF = NEEDF(MODE+4) .AND. .NOT. HAVEF
            IF (WANTF) THEN
               IF (AX .LE. 1.9D0) THEN
                  F = (((((PF1(5)*X4+PF1(4))*X4+PF1(3))*X4+PF1(2))*X4+   &
                          PF1(1))*X4+PF1(0))   &
                   / ((((((QF1(5)*X4+QF1(4))*X4+QF1(3))*X4+QF1(2))*X4+   &
                          QF1(1))*X4+1.0D0) * AX)
               ELSE IF (AX .LE. 2.4) THEN
                  F = (((((PF2(5)*X4+PF2(4))*X4+PF2(3))*X4+PF2(2))*X4+   &
                          PF2(1))*X4+PF2(0))   &
                   / ((((((QF2(5)*X4+QF2(4))*X4+QF2(3))*X4+QF2(2))*X4+   &
                          QF2(1))*X4+1.0D0) * AX)
               ELSE
                  F = (RPI +   &
                    X4*((((((PF3(6)*X4+PF3(5))*X4+PF3(4))*X4+PF3(3))*X4+   &
                         PF3(2))*X4+PF3(1))*X4+PF3(0))   &
                  /    ((((((QF3(6)*X4+QF3(5))*X4+QF3(4))*X4+QF3(3))*X4+   &
                         QF3(2))*X4+QF3(1))*X4+1.0D0)) / AX
               END IF
               HAVEF = .TRUE.
            END IF
            WANTG = NEEDG(MODE+4) .AND. .NOT. HAVEG
            IF (WANTG) THEN
               IF (X .LE. 1.9D0) THEN
                  G = (((((PG1(5)*X4+PG1(4))*X4+PG1(3))*X4+PG1(2))*X4+   &
                          PG1(1))*X4+PG1(0))   &
                   / ((((((QG1(5)*X4+QG1(4))*X4+QG1(3))*X4+QG1(2))*X4+   &
                          QG1(1))*X4+1.0D0) * AX**3)
               ELSE IF (AX .LE. 2.4D0) THEN
                  G = (((((PG2(5)*X4+PG2(4))*X4+PG2(3))*X4+PG2(2))*X4+   &
                           PG2(1))*X4+PG2(0))   &
                   / ((((((QG2(5)*X4+QG2(4))*X4+QG2(3))*X4+QG2(2))*X4+   &
                          QG2(1))*X4+1.0D0) * AX**3)
               ELSE
                  G = (RPISQ +   &
                    X4*((((((PG3(6)*X4+PG3(5))*X4+PG3(4))*X4+PG3(3))*X4+   &
                         PG3(2))*X4+PG3(1))*X4+PG3(0))   &
                  /    ((((((QG3(6)*X4+QG3(5))*X4+QG3(4))*X4+QG3(3))*X4+   &
                         QG3(2))*X4+QG3(1))*X4+1.0D0)) / AX**3
               END IF
               HAVEG = .TRUE.
            END IF
         ELSE
            WANTF = NEEDF(MODE)
            IF (WANTF) THEN
               IF (X .LE. LARGEF) THEN
                  F = RPI / AX
               ELSE
                  F = 0.0D0
               END IF
            END IF
            WANTG = NEEDG(MODE)
            IF (WANTG) THEN
               IF (X .LE. LARGEG) THEN
                  G = RPISQ / AX**3
               ELSE
                  G = 0.0D0
               END IF
            END IF
         END IF
         WANTC = (NEEDC(MODE+4) .OR. NEEDS(MODE+4)) .AND. .NOT. HAVEC
         IF (WANTC .OR. X.LT.0.0D0) THEN
            IF (AX .LE. BIGX) THEN
               CX = COS(PID2 * AX*AX)
               SX = SIN(PID2 * AX*AX)
            ELSE
               CX = 1.0D0
               SX = 0.0D0
            END IF
            IF (WANTC) THEN
               C = 0.5D0 + F*SX - G*CX
               S = 0.5D0 - F*CX - G*SX
               IF (X .LT. 0.0) THEN
                  C = -C
                  S = -S
               END IF
               HAVEC = .TRUE.
            END IF
            IF (X .LT. 0.0) THEN
!              WE COULD DO THE FOLLOWING BEFORE THE PRECEEDING, AND THEN
!              NOT PUT IN A TEST IN THE PRECEEDING FOR X .LT. 0, BUT
!              EVEN THOUGH THE RESULTS ARE MATHEMATICALLY IDENTICAL, WE
!              WOULD HAVE SOME CANCELLATION ABOVE IF WE DID SO.
               IF (WANTG) G = CX + SX - G
               IF (WANTF) F = CX - SX - F
            END IF
          END IF
      END IF
      XSAVE = X
!
      DFRENC = RESULT(MODE)
      RETURN
      END FUNCTION DFRENC 
      SUBROUTINE DFZERO (F, B, C, R, RE, AE, IFLAG)
!***BEGIN PROLOGUE  DFZERO
!***PURPOSE  Search for a zero of a function F(X) in a given interval
!            (B,C).  It is designed primarily for problems where F(B)
!            and F(C) have opposite signs.
!***LIBRARY   SLATEC
!***CATEGORY  F1B
!***TYPE      DOUBLE PRECISION (FZERO-S, DFZERO-D)
!***KEYWORDS  BISECTION, NONLINEAR, ROOTS, ZEROS
!***AUTHOR  Shampine, L. F., (SNLA)
!           Watts, H. A., (SNLA)
!***DESCRIPTION
!
!     DFZERO searches for a zero of a DOUBLE PRECISION function F(X)
!     between the given DOUBLE PRECISION values B and C until the width
!     of the interval (B,C) has collapsed to within a tolerance
!     specified by the stopping criterion,
!        ABS(B-C) .LE. 2.*(RW*ABS(B)+AE).
!     The method used is an efficient combination of bisection and the
!     secant rule and is due to T. J. Dekker.
!
!     Description Of Arguments
!
!   F     :EXT   - Name of the DOUBLE PRECISION external function.  This
!                  name must be in an EXTERNAL statement in the calling
!                  program.  F must be a function of one DOUBLE
!                  PRECISION argument.
!
!   B     :INOUT - One end of the DOUBLE PRECISION interval (B,C).  The
!                  value returned for B usually is the better
!                  approximation to a zero of F.
!
!   C     :INOUT - The other end of the DOUBLE PRECISION interval (B,C)
!
!   R     :IN    - A (better) DOUBLE PRECISION guess of a zero of F
!                  which could help in speeding up convergence.  If F(B)
!                  and F(R) have opposite signs, a root will be found in
!                  the interval (B,R);  if not, but F(R) and F(C) have
!                  opposite signs, a root will be found in the interval
!                  (R,C);  otherwise, the interval (B,C) will be
!                  searched for a possible root.  When no better guess
!                  is known, it is recommended that R be set to B or C,
!                  since if R is not interior to the interval (B,C), it
!                  will be ignored.
!
!   RE    :IN    - Relative error used for RW in the stopping criterion.
!                  If the requested RE is less than machine precision,
!                  then RW is set to approximately machine precision.
!
!   AE    :IN    - Absolute error used in the stopping criterion.  If
!                  the given interval (B,C) contains the origin, then a
!                  nonzero value should be chosen for AE.
!
!   IFLAG :OUT   - A status code.  User must check IFLAG after each
!                  call.  Control returns to the user from DFZERO in all
!                  cases.
!
!                1  B is within the requested tolerance of a zero.
!                   The interval (B,C) collapsed to the requested
!                   tolerance, the function changes sign in (B,C), and
!                   F(X) decreased in magnitude as (B,C) collapsed.
!
!                2  F(B) = 0.  However, the interval (B,C) may not have
!                   collapsed to the requested tolerance.
!
!                3  B may be near a singular point of F(X).
!                   The interval (B,C) collapsed to the requested tol-
!                   erance and the function changes sign in (B,C), but
!                   F(X) increased in magnitude as (B,C) collapsed, i.e.
!                     ABS(F(B out)) .GT. MAX(ABS(F(B in)),ABS(F(C in)))
!
!                4  No change in sign of F(X) was found although the
!                   interval (B,C) collapsed to the requested tolerance.
!                   The user must examine this case and decide whether
!                   B is near a local minimum of F(X), or B is near a
!                   zero of even multiplicity, or neither of these.
!
!                5  Too many (.GT. 500) function evaluations used.
!
!***REFERENCES  L. F. Shampine and H. A. Watts, FZERO, a root-solving
!                 code, Report SC-TM-70-631, Sandia Laboratories,
!                 September 1970.
!               T. J. Dekker, Finding a zero by means of successive
!                 linear interpolation, Constructive Aspects of the
!                 Fundamental Theorem of Algebra, edited by B. Dejon
!                 and P. Henrici, Wiley-Interscience, 1969.
!***ROUTINES CALLED  D1MACH
!***REVISION HISTORY  (YYMMDD)
!   700901  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890531  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   920501  Reformatted the REFERENCES section.  (WRB)
!***END PROLOGUE  DFZERO
!CCCC DOUBLE PRECISION A,ACBS,ACMB,AE,AW,B,C,CMB,D1MACH,ER,
      DOUBLE PRECISION A,ACBS,ACMB,AE,AW,B,C,CMB,ER,   &
                       F,FA,FB,FC,FX,FZ,P,Q,R,RE,RW,T,TOL,Z
      INTEGER IC,IFLAG,KOUNT
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!***FIRST EXECUTABLE STATEMENT  DFZERO
!
!   ER is two times the computer unit roundoff value which is defined
!   here by the function D1MACH.
!
      ER = 2.0D0 * D1MACH(4)
!
!   Initialize.
!
      Z = R
      IF (R .LE. MIN(B,C)  .OR.  R .GE. MAX(B,C)) Z = C
      RW = MAX(RE,ER)
      AW = MAX(AE,0.D0)
      IC = 0
      T = Z
      FZ = F(T)
      FC = FZ
      T = B
      FB = F(T)
      KOUNT = 2
      IF (SIGN(1.0D0,FZ) .EQ. SIGN(1.0D0,FB)) GO TO 1
      C = Z
      GO TO 2
    1 IF (Z .EQ. C) GO TO 2
      T = C
      FC = F(T)
      KOUNT = 3
      IF (SIGN(1.0D0,FZ) .EQ. SIGN(1.0D0,FC)) GO TO 2
      B = Z
      FB = FZ
    2 A = C
      FA = FC
      ACBS = ABS(B-C)
      FX = MAX(ABS(FB),ABS(FC))
!
    3 IF (ABS(FC) .GE. ABS(FB)) GO TO 4
!
!   Perform interchange.
!
      A = B
      FA = FB
      B = C
      FB = FC
      C = A
      FC = FA
!
    4 CMB = 0.5D0*(C-B)
      ACMB = ABS(CMB)
      TOL = RW*ABS(B) + AW
!
!   Test stopping criterion and function count.
!
      IF (ACMB .LE. TOL) GO TO 10
      IF (FB .EQ. 0.D0) GO TO 11
      IF (KOUNT .GE. 500) GO TO 14
!
!   Calculate new iterate implicitly as B+P/Q, where we arrange
!   P .GE. 0.  The implicit form is used to prevent overflow.
!
      P = (B-A)*FB
      Q = FA - FB
      IF (P .GE. 0.D0) GO TO 5
      P = -P
      Q = -Q
!
!   Update A and check for satisfactory reduction in the size of the
!   bracketing interval.  If not, perform bisection.
!
    5 A = B
      FA = FB
      IC = IC + 1
      IF (IC .LT. 4) GO TO 6
      IF (8.0D0*ACMB .GE. ACBS) GO TO 8
      IC = 0
      ACBS = ACMB
!
!   Test for too small a change.
!
    6 IF (P .GT. ABS(Q)*TOL) GO TO 7
!
!   Increment by TOLerance.
!
      B = B + SIGN(TOL,CMB)
      GO TO 9
!
!   Root ought to be between B and (C+B)/2.
!
    7 IF (P .GE. CMB*Q) GO TO 8
!
!   Use secant rule.
!
      B = B + P/Q
      GO TO 9
!
!   Use bisection (C+B)/2.
!
    8 B = B + CMB
!
!   Have completed computation for new iterate B.
!
    9 T = B
      FB = F(T)
      KOUNT = KOUNT + 1
!
!   Decide whether next step is interpolation or extrapolation.
!
      IF (SIGN(1.0D0,FB) .NE. SIGN(1.0D0,FC)) GO TO 3
      C = A
      FC = FA
      GO TO 3
!
!   Finished.  Process results for proper setting of IFLAG.
!
   10 IF (SIGN(1.0D0,FB) .EQ. SIGN(1.0D0,FC)) GO TO 13
      IF (ABS(FB) .GT. FX) GO TO 12
      IFLAG = 1
      RETURN
   11 IFLAG = 2
      RETURN
   12 IFLAG = 3
      RETURN
   13 IFLAG = 4
      RETURN
   14 IFLAG = 5
      RETURN
      END SUBROUTINE DFZERO 
      SUBROUTINE DFZER2 (F, B, C, R, RE, AE, IFLAG,X)
!***MODIFIED VERSION OF DFZERO.  PASS ALONG DATA ARRAY X
!***BEGIN PROLOGUE  DFZERO
!***PURPOSE  Search for a zero of a function F(X) in a given interval
!            (B,C).  It is designed primarily for problems where F(B)
!            and F(C) have opposite signs.
!***LIBRARY   SLATEC
!***CATEGORY  F1B
!***TYPE      DOUBLE PRECISION (FZERO-S, DFZERO-D)
!***KEYWORDS  BISECTION, NONLINEAR, ROOTS, ZEROS
!***AUTHOR  Shampine, L. F., (SNLA)
!           Watts, H. A., (SNLA)
!***DESCRIPTION
!
!     DFZERO searches for a zero of a DOUBLE PRECISION function F(X)
!     between the given DOUBLE PRECISION values B and C until the width
!     of the interval (B,C) has collapsed to within a tolerance
!     specified by the stopping criterion,
!        ABS(B-C) .LE. 2.*(RW*ABS(B)+AE).
!     The method used is an efficient combination of bisection and the
!     secant rule and is due to T. J. Dekker.
!
!     Description Of Arguments
!
!   F     :EXT   - Name of the DOUBLE PRECISION external function.  This
!                  name must be in an EXTERNAL statement in the calling
!                  program.  F must be a function of one DOUBLE
!                  PRECISION argument.
!
!   B     :INOUT - One end of the DOUBLE PRECISION interval (B,C).  The
!                  value returned for B usually is the better
!                  approximation to a zero of F.
!
!   C     :INOUT - The other end of the DOUBLE PRECISION interval (B,C)
!
!   R     :IN    - A (better) DOUBLE PRECISION guess of a zero of F
!                  which could help in speeding up convergence.  If F(B)
!                  and F(R) have opposite signs, a root will be found in
!                  the interval (B,R);  if not, but F(R) and F(C) have
!                  opposite signs, a root will be found in the interval
!                  (R,C);  otherwise, the interval (B,C) will be
!                  searched for a possible root.  When no better guess
!                  is known, it is recommended that R be set to B or C,
!                  since if R is not interior to the interval (B,C), it
!                  will be ignored.
!
!   RE    :IN    - Relative error used for RW in the stopping criterion.
!                  If the requested RE is less than machine precision,
!                  then RW is set to approximately machine precision.
!
!   AE    :IN    - Absolute error used in the stopping criterion.  If
!                  the given interval (B,C) contains the origin, then a
!                  nonzero value should be chosen for AE.
!
!   IFLAG :OUT   - A status code.  User must check IFLAG after each
!                  call.  Control returns to the user from DFZERO in all
!                  cases.
!
!                1  B is within the requested tolerance of a zero.
!                   The interval (B,C) collapsed to the requested
!                   tolerance, the function changes sign in (B,C), and
!                   F(X) decreased in magnitude as (B,C) collapsed.
!
!                2  F(B) = 0.  However, the interval (B,C) may not have
!                   collapsed to the requested tolerance.
!
!                3  B may be near a singular point of F(X).
!                   The interval (B,C) collapsed to the requested tol-
!                   erance and the function changes sign in (B,C), but
!                   F(X) increased in magnitude as (B,C) collapsed, i.e.
!                     ABS(F(B out)) .GT. MAX(ABS(F(B in)),ABS(F(C in)))
!
!                4  No change in sign of F(X) was found although the
!                   interval (B,C) collapsed to the requested tolerance.
!                   The user must examine this case and decide whether
!                   B is near a local minimum of F(X), or B is near a
!                   zero of even multiplicity, or neither of these.
!
!                5  Too many (.GT. 500) function evaluations used.
!
!***REFERENCES  L. F. Shampine and H. A. Watts, FZERO, a root-solving
!                 code, Report SC-TM-70-631, Sandia Laboratories,
!                 September 1970.
!               T. J. Dekker, Finding a zero by means of successive
!                 linear interpolation, Constructive Aspects of the
!                 Fundamental Theorem of Algebra, edited by B. Dejon
!                 and P. Henrici, Wiley-Interscience, 1969.
!***ROUTINES CALLED  D1MACH
!***REVISION HISTORY  (YYMMDD)
!   700901  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890531  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   920501  Reformatted the REFERENCES section.  (WRB)
!***END PROLOGUE  DFZERO
!CCCC DOUBLE PRECISION A,ACBS,ACMB,AE,AW,B,C,CMB,D1MACH,ER,
      DOUBLE PRECISION A,ACBS,ACMB,AE,AW,B,C,CMB,ER,   &
                       F,FA,FB,FC,FX,FZ,P,Q,R,RE,RW,T,TOL,Z
      DOUBLE PRECISION X(*)
      INTEGER IC,IFLAG,KOUNT
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!***FIRST EXECUTABLE STATEMENT  DFZERO
!
!   ER is two times the computer unit roundoff value which is defined
!   here by the function D1MACH.
!
      ER = 2.0D0 * D1MACH(4)
!
!   Initialize.
!
      Z = R
      IF (R .LE. MIN(B,C)  .OR.  R .GE. MAX(B,C)) Z = C
      RW = MAX(RE,ER)
      AW = MAX(AE,0.D0)
      IC = 0
      T = Z
      FZ = F(T,X)
      FC = FZ
      T = B
      FB = F(T,X)
      KOUNT = 2
      IF (SIGN(1.0D0,FZ) .EQ. SIGN(1.0D0,FB)) GO TO 1
      C = Z
      GO TO 2
    1 IF (Z .EQ. C) GO TO 2
      T = C
      FC = F(T,X)
      KOUNT = 3
      IF (SIGN(1.0D0,FZ) .EQ. SIGN(1.0D0,FC)) GO TO 2
      B = Z
      FB = FZ
    2 A = C
      FA = FC
      ACBS = ABS(B-C)
      FX = MAX(ABS(FB),ABS(FC))
!
    3 IF (ABS(FC) .GE. ABS(FB)) GO TO 4
!
!   Perform interchange.
!
      A = B
      FA = FB
      B = C
      FB = FC
      C = A
      FC = FA
!
    4 CMB = 0.5D0*(C-B)
      ACMB = ABS(CMB)
      TOL = RW*ABS(B) + AW
!
!   Test stopping criterion and function count.
!
      IF (ACMB .LE. TOL) GO TO 10
      IF (FB .EQ. 0.D0) GO TO 11
      IF (KOUNT .GE. 500) GO TO 14
!
!   Calculate new iterate implicitly as B+P/Q, where we arrange
!   P .GE. 0.  The implicit form is used to prevent overflow.
!
      P = (B-A)*FB
      Q = FA - FB
      IF (P .GE. 0.D0) GO TO 5
      P = -P
      Q = -Q
!
!   Update A and check for satisfactory reduction in the size of the
!   bracketing interval.  If not, perform bisection.
!
    5 A = B
      FA = FB
      IC = IC + 1
      IF (IC .LT. 4) GO TO 6
      IF (8.0D0*ACMB .GE. ACBS) GO TO 8
      IC = 0
      ACBS = ACMB
!
!   Test for too small a change.
!
    6 IF (P .GT. ABS(Q)*TOL) GO TO 7
!
!   Increment by TOLerance.
!
      B = B + SIGN(TOL,CMB)
      GO TO 9
!
!   Root ought to be between B and (C+B)/2.
!
    7 IF (P .GE. CMB*Q) GO TO 8
!
!   Use secant rule.
!
      B = B + P/Q
      GO TO 9
!
!   Use bisection (C+B)/2.
!
    8 B = B + CMB
!
!   Have completed computation for new iterate B.
!
    9 T = B
      FB = F(T,X)
      KOUNT = KOUNT + 1
!
!   Decide whether next step is interpolation or extrapolation.
!
      IF (SIGN(1.0D0,FB) .NE. SIGN(1.0D0,FC)) GO TO 3
      C = A
      FC = FA
      GO TO 3
!
!   Finished.  Process results for proper setting of IFLAG.
!
   10 IF (SIGN(1.0D0,FB) .EQ. SIGN(1.0D0,FC)) GO TO 13
      IF (ABS(FB) .GT. FX) GO TO 12
      IFLAG = 1
      RETURN
   11 IFLAG = 2
      RETURN
   12 IFLAG = 3
      RETURN
   13 IFLAG = 4
      RETURN
   14 IFLAG = 5
      RETURN
      END SUBROUTINE DFZER2 
      SUBROUTINE DFZER3 (F, B, C, R, RE, AE, IFLAG,X)
!***COPY OF DFZER2.  A WEIBULL MLE PROBLEM REQUIRES THE ROOT
!***FUNCTION TO COMPUTE A NEEDED PARAMETER BY FINDING ANOTHER
!***ROOT.  SINCE FORTRAN 77 DOES NOT ALLOW RECURSION, IMPLEMENT
!***VIA A SEPARATE ROUTINE.
!***MODIFIED VERSION OF DFZERO.  PASS ALONG DATA ARRAY X
!***BEGIN PROLOGUE  DFZERO
!***PURPOSE  Search for a zero of a function F(X) in a given interval
!            (B,C).  It is designed primarily for problems where F(B)
!            and F(C) have opposite signs.
!***LIBRARY   SLATEC
!***CATEGORY  F1B
!***TYPE      DOUBLE PRECISION (FZERO-S, DFZERO-D)
!***KEYWORDS  BISECTION, NONLINEAR, ROOTS, ZEROS
!***AUTHOR  Shampine, L. F., (SNLA)
!           Watts, H. A., (SNLA)
!***DESCRIPTION
!
!     DFZERO searches for a zero of a DOUBLE PRECISION function F(X)
!     between the given DOUBLE PRECISION values B and C until the width
!     of the interval (B,C) has collapsed to within a tolerance
!     specified by the stopping criterion,
!        ABS(B-C) .LE. 2.*(RW*ABS(B)+AE).
!     The method used is an efficient combination of bisection and the
!     secant rule and is due to T. J. Dekker.
!
!     Description Of Arguments
!
!   F     :EXT   - Name of the DOUBLE PRECISION external function.  This
!                  name must be in an EXTERNAL statement in the calling
!                  program.  F must be a function of one DOUBLE
!                  PRECISION argument.
!
!   B     :INOUT - One end of the DOUBLE PRECISION interval (B,C).  The
!                  value returned for B usually is the better
!                  approximation to a zero of F.
!
!   C     :INOUT - The other end of the DOUBLE PRECISION interval (B,C)
!
!   R     :IN    - A (better) DOUBLE PRECISION guess of a zero of F
!                  which could help in speeding up convergence.  If F(B)
!                  and F(R) have opposite signs, a root will be found in
!                  the interval (B,R);  if not, but F(R) and F(C) have
!                  opposite signs, a root will be found in the interval
!                  (R,C);  otherwise, the interval (B,C) will be
!                  searched for a possible root.  When no better guess
!                  is known, it is recommended that R be set to B or C,
!                  since if R is not interior to the interval (B,C), it
!                  will be ignored.
!
!   RE    :IN    - Relative error used for RW in the stopping criterion.
!                  If the requested RE is less than machine precision,
!                  then RW is set to approximately machine precision.
!
!   AE    :IN    - Absolute error used in the stopping criterion.  If
!                  the given interval (B,C) contains the origin, then a
!                  nonzero value should be chosen for AE.
!
!   IFLAG :OUT   - A status code.  User must check IFLAG after each
!                  call.  Control returns to the user from DFZERO in all
!                  cases.
!
!                1  B is within the requested tolerance of a zero.
!                   The interval (B,C) collapsed to the requested
!                   tolerance, the function changes sign in (B,C), and
!                   F(X) decreased in magnitude as (B,C) collapsed.
!
!                2  F(B) = 0.  However, the interval (B,C) may not have
!                   collapsed to the requested tolerance.
!
!                3  B may be near a singular point of F(X).
!                   The interval (B,C) collapsed to the requested tol-
!                   erance and the function changes sign in (B,C), but
!                   F(X) increased in magnitude as (B,C) collapsed, i.e.
!                     ABS(F(B out)) .GT. MAX(ABS(F(B in)),ABS(F(C in)))
!
!                4  No change in sign of F(X) was found although the
!                   interval (B,C) collapsed to the requested tolerance.
!                   The user must examine this case and decide whether
!                   B is near a local minimum of F(X), or B is near a
!                   zero of even multiplicity, or neither of these.
!
!                5  Too many (.GT. 500) function evaluations used.
!
!***REFERENCES  L. F. Shampine and H. A. Watts, FZERO, a root-solving
!                 code, Report SC-TM-70-631, Sandia Laboratories,
!                 September 1970.
!               T. J. Dekker, Finding a zero by means of successive
!                 linear interpolation, Constructive Aspects of the
!                 Fundamental Theorem of Algebra, edited by B. Dejon
!                 and P. Henrici, Wiley-Interscience, 1969.
!***ROUTINES CALLED  D1MACH
!***REVISION HISTORY  (YYMMDD)
!   700901  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890531  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   920501  Reformatted the REFERENCES section.  (WRB)
!***END PROLOGUE  DFZERO
!CCCC DOUBLE PRECISION A,ACBS,ACMB,AE,AW,B,C,CMB,D1MACH,ER,
      DOUBLE PRECISION A,ACBS,ACMB,AE,AW,B,C,CMB,ER,   &
                       F,FA,FB,FC,FX,FZ,P,Q,R,RE,RW,T,TOL,Z
      DOUBLE PRECISION X(*)
      INTEGER IC,IFLAG,KOUNT
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!***FIRST EXECUTABLE STATEMENT  DFZERO
!
!   ER is two times the computer unit roundoff value which is defined
!   here by the function D1MACH.
!
      ER = 2.0D0 * D1MACH(4)
!
!   Initialize.
!
      Z = R
      IF (R .LE. MIN(B,C)  .OR.  R .GE. MAX(B,C)) Z = C
      RW = MAX(RE,ER)
      AW = MAX(AE,0.D0)
      IC = 0
      T = Z
      FZ = F(T,X)
      FC = FZ
      T = B
      FB = F(T,X)
      KOUNT = 2
      IF (SIGN(1.0D0,FZ) .EQ. SIGN(1.0D0,FB)) GO TO 1
      C = Z
      GO TO 2
    1 IF (Z .EQ. C) GO TO 2
      T = C
      FC = F(T,X)
      KOUNT = 3
      IF (SIGN(1.0D0,FZ) .EQ. SIGN(1.0D0,FC)) GO TO 2
      B = Z
      FB = FZ
    2 A = C
      FA = FC
      ACBS = ABS(B-C)
      FX = MAX(ABS(FB),ABS(FC))
!
    3 IF (ABS(FC) .GE. ABS(FB)) GO TO 4
!
!   Perform interchange.
!
      A = B
      FA = FB
      B = C
      FB = FC
      C = A
      FC = FA
!
    4 CMB = 0.5D0*(C-B)
      ACMB = ABS(CMB)
      TOL = RW*ABS(B) + AW
!
!   Test stopping criterion and function count.
!
      IF (ACMB .LE. TOL) GO TO 10
      IF (FB .EQ. 0.D0) GO TO 11
      IF (KOUNT .GE. 500) GO TO 14
!
!   Calculate new iterate implicitly as B+P/Q, where we arrange
!   P .GE. 0.  The implicit form is used to prevent overflow.
!
      P = (B-A)*FB
      Q = FA - FB
      IF (P .GE. 0.D0) GO TO 5
      P = -P
      Q = -Q
!
!   Update A and check for satisfactory reduction in the size of the
!   bracketing interval.  If not, perform bisection.
!
    5 A = B
      FA = FB
      IC = IC + 1
      IF (IC .LT. 4) GO TO 6
      IF (8.0D0*ACMB .GE. ACBS) GO TO 8
      IC = 0
      ACBS = ACMB
!
!   Test for too small a change.
!
    6 IF (P .GT. ABS(Q)*TOL) GO TO 7
!
!   Increment by TOLerance.
!
      B = B + SIGN(TOL,CMB)
      GO TO 9
!
!   Root ought to be between B and (C+B)/2.
!
    7 IF (P .GE. CMB*Q) GO TO 8
!
!   Use secant rule.
!
      B = B + P/Q
      GO TO 9
!
!   Use bisection (C+B)/2.
!
    8 B = B + CMB
!
!   Have completed computation for new iterate B.
!
    9 T = B
      FB = F(T,X)
      KOUNT = KOUNT + 1
!
!   Decide whether next step is interpolation or extrapolation.
!
      IF (SIGN(1.0D0,FB) .NE. SIGN(1.0D0,FC)) GO TO 3
      C = A
      FC = FA
      GO TO 3
!
!   Finished.  Process results for proper setting of IFLAG.
!
   10 IF (SIGN(1.0D0,FB) .EQ. SIGN(1.0D0,FC)) GO TO 13
      IF (ABS(FB) .GT. FX) GO TO 12
      IFLAG = 1
      RETURN
   11 IFLAG = 2
      RETURN
   12 IFLAG = 3
      RETURN
   13 IFLAG = 4
      RETURN
   14 IFLAG = 5
      RETURN
      END SUBROUTINE DFZER3 
      DOUBLE PRECISION FUNCTION DGAMI (A, X)
!***BEGIN PROLOGUE  DGAMI
!***PURPOSE  Evaluate the incomplete Gamma function.
!***LIBRARY   SLATEC (FNLIB)
!***CATEGORY  C7E
!***TYPE      DOUBLE PRECISION (GAMI-S, DGAMI-D)
!***KEYWORDS  FNLIB, INCOMPLETE GAMMA FUNCTION, SPECIAL FUNCTIONS
!***AUTHOR  Fullerton, W., (LANL)
!***DESCRIPTION
!
! Evaluate the incomplete gamma function defined by
!
! DGAMI = integral from T = 0 to X of EXP(-T) * T**(A-1.0) .
!
! DGAMI is evaluated for positive values of A and non-negative values
! of X.  A slight deterioration of 2 or 3 digits accuracy will occur
! when DGAMI is very large or very small, because logarithmic variables
! are used.  The function and both arguments are double precision.
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  DGAMIT, DLNGAM, XERMSG
!***REVISION HISTORY  (YYMMDD)
!   770701  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890531  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!***END PROLOGUE  DGAMI
      DOUBLE PRECISION A, X, FACTOR, DLNGAM, DGAMIT
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!***FIRST EXECUTABLE STATEMENT  DGAMI
      IF (A .LE. 0.D0) THEN
        WRITE(ICOUT,11)
        CALL DPWRST('XXX','BUG ')
        DGAMI = 0.D0
        RETURN
      ENDIF
   11 FORMAT('***** ERROR FROM DGAMI.  ALPHA SHOULD BE POSITIVE.')
      IF (X .LT. 0.D0) THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,13)
        CALL DPWRST('XXX','BUG ')
        DGAMI = 0.D0
        RETURN
      ENDIF
   12 FORMAT('***** ERROR FROM DGAMI.  X MUST BE GREATER THAN OR ')
   13 FORMAT('      EQUAL TO ZERO.                               ****')
!
      DGAMI = 0.D0
      IF (X.EQ.0.0D0) RETURN
!
! THE ONLY ERROR POSSIBLE IN THE EXPRESSION BELOW IS A FATAL OVERFLOW.
!
      FACTOR = EXP (DLNGAM(A) + A*LOG(X))
!
      DGAMI = FACTOR * DGAMIT (A, X)
!
      RETURN
      END FUNCTION DGAMI 
      DOUBLE PRECISION FUNCTION DGAMIP (A, X)
!***BEGIN PROLOGUE  DGAMIP
!***PURPOSE  Evaluate the incomplete Gamma function.
!***LIBRARY   SLATEC (FNLIB)
!***CATEGORY  C7E
!***TYPE      DOUBLE PRECISION (GAMI-S, DGAMIP-D)
!***KEYWORDS  FNLIB, INCOMPLETE GAMMA FUNCTION, SPECIAL FUNCTIONS
!***AUTHOR  Fullerton, W., (LANL)
!***DESCRIPTION
!
! Evaluate the incomplete gamma function defined by
!
! DGAMIP = integral from T = 0 to X of EXP(-T) * T**(A-1.0) .
!
! DGAMIP is evaluated for positive values of A and non-negative values
! of X.  A slight deterioration of 2 or 3 digits accuracy will occur
! when DGAMIP is very large or very small, because logarithmic variables
! are used.  The function and both arguments are double precision.
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  DGAMIPT, DLNGAM, XERMSG
!***REVISION HISTORY  (YYMMDD)
!   770701  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890531  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!***END PROLOGUE  DGAMIP
!CCCC DOUBLE PRECISION A, X, FACTOR, DLNGAM, DGAMIT
      DOUBLE PRECISION A, X, FACTOR, DGAMIT
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!***FIRST EXECUTABLE STATEMENT  DGAMIP
      IF (A .LE. 0.D0) THEN
        WRITE(ICOUT,11)
        CALL DPWRST('XXX','BUG ')
        DGAMIP = 0.D0
        RETURN
      ENDIF
   11 FORMAT('***** ERROR FROM DGAMIP.  ALPHA SHOULD BE POSITIVE.')
      IF (X .LT. 0.D0) THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,13)
        CALL DPWRST('XXX','BUG ')
        DGAMIP = 0.D0
        RETURN
      ENDIF
   12 FORMAT('***** ERROR FROM DGAMIP.  X MUST BE GREATER THAN OR ')
   13 FORMAT('      EQUAL TO ZERO.                               ****')
!
      DGAMIP = 0.D0
      IF (X.EQ.0.0D0) RETURN
!
! THE ONLY ERROR POSSIBLE IN THE EXPRESSION BELOW IS A FATAL OVERFLOW.
!CCCC NOTE:  FOR DATAPLOT, WANT FORM OF INCOMPLETE GAMMA THAT HAS
!CCCC        DIVISION BY COMPLETE GAMMA FUNCTION INCLUDED!
!
!CCCC FACTOR = EXP (DLNGAM(A) + A*LOG(X))
      FACTOR = EXP(A*LOG(X))
!
      DGAMIP = FACTOR * DGAMIT (A, X)
!
      RETURN
      END FUNCTION DGAMIP 
      DOUBLE PRECISION FUNCTION DGAMIC (A, X)
!***BEGIN PROLOGUE  DGAMIC
!***PURPOSE  Calculate the complementary incomplete Gamma function.
!***LIBRARY   SLATEC (FNLIB)
!***CATEGORY  C7E
!***TYPE      DOUBLE PRECISION (GAMIC-S, DGAMIC-D)
!***KEYWORDS  COMPLEMENTARY INCOMPLETE GAMMA FUNCTION, FNLIB,
!             SPECIAL FUNCTIONS
!***AUTHOR  Fullerton, W., (LANL)
!***DESCRIPTION
!
!   Evaluate the complementary incomplete Gamma function
!
!   DGAMIC = integral from X to infinity of EXP(-T) * T**(A-1.)  .
!
!   DGAMIC is evaluated for arbitrary real values of A and for non-
!   negative values of X (even though DGAMIC is defined for X .LT.
!   0.0), except that for X = 0 and A .LE. 0.0, DGAMIC is undefined.
!
!   DGAMIC, A, and X are DOUBLE PRECISION.
!
!   A slight deterioration of 2 or 3 digits accuracy will occur when
!   DGAMIC is very large or very small in absolute value, because log-
!   arithmic variables are used.  Also, if the parameter A is very close
!   to a negative INTEGER (but not a negative integer), there is a loss
!   of accuracy, which is reported if the result is less than half
!   machine precision.
!
!***REFERENCES  W. Gautschi, A computational procedure for incomplete
!                 gamma functions, ACM Transactions on Mathematical
!                 Software 5, 4 (December 1979), pp. 466-481.
!               W. Gautschi, Incomplete gamma functions, Algorithm 542,
!                 ACM Transactions on Mathematical Software 5, 4
!                 (December 1979), pp. 482-489.
!***ROUTINES CALLED  D1MACH, D9GMIC, D9GMIT, D9LGIC, D9LGIT, DLGAMS,
!                    DLNGAM, XERCLR, XERMSG
!***REVISION HISTORY  (YYMMDD)
!   770701  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890531  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!   920528  DESCRIPTION and REFERENCES sections revised.  (WRB)
!***END PROLOGUE  DGAMIC
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DOUBLE PRECISION A, X, AEPS, AINTA, ALGAP1, ALNEPS, ALNGS, ALX,   &
        BOT, E, EPS, GSTAR, H, SGA, SGNG, SGNGAM, SGNGS, SQEPS, T,   &
        DLNGAM, D9GMIC, D9GMIT, D9LGIC, D9LGIT
      LOGICAL FIRST
      SAVE EPS, SQEPS, ALNEPS, BOT, FIRST
      DATA FIRST /.TRUE./
!***FIRST EXECUTABLE STATEMENT  DGAMIC
!
      DGAMIC = 0.D0
!
      IF (FIRST) THEN
         EPS = 0.5D0*D1MACH(3)
         SQEPS = SQRT(D1MACH(4))
         ALNEPS = -LOG (D1MACH(3))
         BOT = LOG (D1MACH(1))
      ENDIF
      FIRST = .FALSE.
!
      IF (X .LT. 0.D0) THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        DGAMIC = 0.D0
        RETURN
      ENDIF
   12 FORMAT('***** ERROR FROM DGAMIC.  X MUST BE GREATER THAN OR ',   &
             'EQUAL TO ZERO. ****')
!
      IF (X.GT.0.D0) GO TO 20
      IF (A .LE. 0.D0) THEN
        WRITE(ICOUT,11)
        CALL DPWRST('XXX','BUG ')
        DGAMIC = 0.D0
        RETURN
      ENDIF
   11 FORMAT('***** ERROR FROM DGAMI.  GAMMAIC IS UNDEFINED SINCE X ',   &
             'ZERO AND A IS NON-POSITIVE. *****')
!
      DGAMIC = EXP (DLNGAM(A+1.D0) - LOG(A))
      RETURN
!
 20   ALX = LOG (X)
      SGA = 1.0D0
      IF (A.NE.0.D0) SGA = SIGN (1.0D0, A)
      AINTA = AINT (A + 0.5D0*SGA)
      AEPS = A - AINTA
!
      IZERO = 0
      IF (X.GE.1.0D0) GO TO 40
!
      IF (A.GT.0.5D0 .OR. ABS(AEPS).GT.0.001D0) GO TO 30
      E = 2.0D0
      IF (-AINTA.GT.1.D0) E = 2.D0*(-AINTA+2.D0)/(AINTA*AINTA-1.0D0)
      E = E - ALX * X**(-0.001D0)
      IF (E*ABS(AEPS).GT.EPS) GO TO 30
!
      DGAMIC = D9GMIC (A, X, ALX)
      RETURN
!
 30   CALL DLGAMS (A+1.0D0, ALGAP1, SGNGAM)
      GSTAR = D9GMIT (A, X, ALGAP1, SGNGAM, ALX)
      IF (GSTAR.EQ.0.D0) IZERO = 1
      IF (GSTAR.NE.0.D0) ALNGS = LOG (ABS(GSTAR))
      IF (GSTAR.NE.0.D0) SGNGS = SIGN (1.0D0, GSTAR)
      GO TO 50
!
 40   IF (A.LT.X) DGAMIC = EXP (D9LGIC(A, X, ALX))
      IF (A.LT.X) RETURN
!
      SGNGAM = 1.0D0
      ALGAP1 = DLNGAM (A+1.0D0)
      SGNGS = 1.0D0
      ALNGS = D9LGIT (A, X, ALGAP1)
!
! EVALUATION OF DGAMIC(A,X) IN TERMS OF TRICOMI-S INCOMPLETE GAMMA FN.
!
 50   H = 1.D0
      IF (IZERO.EQ.1) GO TO 60
!
      T = A*ALX + ALNGS
      IF (T.GT.ALNEPS) GO TO 70
      IF (T.GT.(-ALNEPS)) H = 1.0D0 - SGNGS*EXP(T)
!
!CCCC IF (ABS(H).LT.SQEPS) CALL XERCLR
      IF (ABS(H) .LT. SQEPS) THEN
        WRITE(ICOUT,51)
        CALL DPWRST('XXX','BUG ')
      ENDIF
   51 FORMAT('***** WARNING FROM DGAMIC, RESULT IS LESS THAN HALF ',   &
             'PRECISION.  ****')
!
 60   SGNG = SIGN (1.0D0, H) * SGA * SGNGAM
      T = LOG(ABS(H)) + ALGAP1 - LOG(ABS(A))
!CCCC IF (T.LT.BOT) CALL XERCLR
      DGAMIC = SGNG * EXP(T)
      RETURN
!
 70   SGNG = -SGNGS * SGA * SGNGAM
      T = T + ALGAP1 - LOG(ABS(A))
!CCCC IF (T.LT.BOT) CALL XERCLR
      DGAMIC = SGNG * EXP(T)
      RETURN
!
      END FUNCTION DGAMIC 
      DOUBLE PRECISION FUNCTION DGAMIT (A, X)
!***BEGIN PROLOGUE  DGAMIT
!***PURPOSE  Calculate Tricomi's form of the incomplete Gamma function.
!***LIBRARY   SLATEC (FNLIB)
!***CATEGORY  C7E
!***TYPE      DOUBLE PRECISION (GAMIT-S, DGAMIT-D)
!***KEYWORDS  COMPLEMENTARY INCOMPLETE GAMMA FUNCTION, FNLIB,
!             SPECIAL FUNCTIONS, TRICOMI
!***AUTHOR  Fullerton, W., (LANL)
!***DESCRIPTION
!
!   Evaluate Tricomi's incomplete Gamma function defined by
!
!   DGAMIT = X**(-A)/GAMMA(A) * integral from 0 to X of EXP(-T) *
!              T**(A-1.)
!
!   for A .GT. 0.0 and by analytic continuation for A .LE. 0.0.
!   GAMMA(X) is the complete gamma function of X.
!
!   DGAMIT is evaluated for arbitrary real values of A and for non-
!   negative values of X (even though DGAMIT is defined for X .LT.
!   0.0), except that for X = 0 and A .LE. 0.0, DGAMIT is infinite,
!   which is a fatal error.
!
!   The function and both arguments are DOUBLE PRECISION.
!
!   A slight deterioration of 2 or 3 digits accuracy will occur when
!   DGAMIT is very large or very small in absolute value, because log-
!   arithmic variables are used.  Also, if the parameter  A  is very
!   close to a negative integer (but not a negative integer), there is
!   a loss of accuracy, which is reported if the result is less than
!   half machine precision.
!
!***REFERENCES  W. Gautschi, A computational procedure for incomplete
!                 gamma functions, ACM Transactions on Mathematical
!                 Software 5, 4 (December 1979), pp. 466-481.
!               W. Gautschi, Incomplete gamma functions, Algorithm 542,
!                 ACM Transactions on Mathematical Software 5, 4
!                 (December 1979), pp. 482-489.
!***ROUTINES CALLED  D1MACH, D9GMIT, D9LGIC, D9LGIT, DGAMR, DLGAMS,
!                    DLNGAM, XERCLR, XERMSG
!***REVISION HISTORY  (YYMMDD)
!   770701  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890531  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!   920528  DESCRIPTION and REFERENCES sections revised.  (WRB)
!***END PROLOGUE  DGAMIT
      DOUBLE PRECISION A, X, AEPS, AINTA, ALGAP1, ALNEPS, ALNG, ALX,   &
        BOT, H, SGA, SGNGAM, SQEPS, T, DGAMR, D9GMIT, D9LGIT,   &
        DLNGAM, D9LGIC
      LOGICAL FIRST
      SAVE ALNEPS, SQEPS, BOT, FIRST
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DATA FIRST /.TRUE./
!***FIRST EXECUTABLE STATEMENT  DGAMIT
      IF (FIRST) THEN
         ALNEPS = -LOG (D1MACH(3))
         SQEPS = SQRT(D1MACH(4))
         BOT = LOG (D1MACH(1))
      ENDIF
      FIRST = .FALSE.
!
      IF (X .LT. 0.D0) THEN
        WRITE(ICOUT,11)
        CALL DPWRST('XXX','BUG ')
        DGAMIT = 0.D0
        RETURN
      ENDIF
   11 FORMAT('***** ERROR FROM DGAMIT.  X IS NEGATIVE.  *****')
!
      IF (X.NE.0.D0) ALX = LOG (X)
      SGA = 1.0D0
      IF (A.NE.0.D0) SGA = SIGN (1.0D0, A)
      AINTA = AINT (A + 0.5D0*SGA)
      AEPS = A - AINTA
!
      IF (X.GT.0.D0) GO TO 20
      DGAMIT = 0.0D0
      IF (AINTA.GT.0.D0 .OR. AEPS.NE.0.D0) DGAMIT = DGAMR(A+1.0D0)
      RETURN
!
 20   IF (X.GT.1.D0) GO TO 30
      IF (A.GE.(-0.5D0) .OR. AEPS.NE.0.D0) CALL DLGAMS (A+1.0D0, ALGAP1,   &
        SGNGAM)
      DGAMIT = D9GMIT (A, X, ALGAP1, SGNGAM, ALX)
      RETURN
!
 30   IF (A.LT.X) GO TO 40
      T = D9LGIT (A, X, DLNGAM(A+1.0D0))
!CCCC IF (T.LT.BOT) CALL XERCLR
      DGAMIT = EXP (T)
      RETURN
!
 40   ALNG = D9LGIC (A, X, ALX)
!
! EVALUATE DGAMIT IN TERMS OF LOG (DGAMIC (A, X))
!
      H = 1.0D0
      IF (AEPS.EQ.0.D0 .AND. AINTA.LE.0.D0) GO TO 50
!
      CALL DLGAMS (A+1.0D0, ALGAP1, SGNGAM)
      T = LOG (ABS(A)) + ALNG - ALGAP1
      IF (T.GT.ALNEPS) GO TO 60
!
      IF (T.GT.(-ALNEPS)) H = 1.0D0 - SGA * SGNGAM * EXP(T)
      IF (ABS(H).GT.SQEPS) GO TO 50
!
      WRITE(ICOUT,41)
 41   FORMAT('***** WARNING FROM DGAMIT.  RESULT IS LESS THAN ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,42)
 42   FORMAT('      HALF PRECISION.                           *****')
      CALL DPWRST('XXX','BUG ')
!
 50   T = -A*ALX + LOG(ABS(H))
!CCCC IF (T.LT.BOT) CALL XERCLR
      DGAMIT = SIGN (EXP(T), H)
      RETURN
!
 60   T = T - A*ALX
!CCCC IF (T.LT.BOT) CALL XERCLR
      DGAMIT = -SGA * SGNGAM * EXP(T)
      RETURN
!
      END FUNCTION DGAMIT 
      SUBROUTINE DGAMLM (XMIN, XMAX)
!***BEGIN PROLOGUE  DGAMLM
!***PURPOSE  Compute the minimum and maximum bounds for the argument in
!            the Gamma function.
!***LIBRARY   SLATEC (FNLIB)
!***CATEGORY  C7A, R2
!***TYPE      DOUBLE PRECISION (GAMLIM-S, DGAMLM-D)
!***KEYWORDS  COMPLETE GAMMA FUNCTION, FNLIB, LIMITS, SPECIAL FUNCTIONS
!***AUTHOR  Fullerton, W., (LANL)
!***DESCRIPTION
!
! Calculate the minimum and maximum legal bounds for X in gamma(X).
! XMIN and XMAX are not the only bounds, but they are the only non-
! trivial ones to calculate.
!
!             Output Arguments --
! XMIN   double precision minimum legal value of X in gamma(X).  Any
!        smaller value of X might result in underflow.
! XMAX   double precision maximum legal value of X in gamma(X).  Any
!        larger value of X might cause overflow.
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  D1MACH, XERMSG
!***REVISION HISTORY  (YYMMDD)
!   770601  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890531  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!***END PROLOGUE  DGAMLM
      DOUBLE PRECISION XMIN, XMAX, ALNBIG, ALNSML, XLN, XOLD
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!***FIRST EXECUTABLE STATEMENT  DGAMLM
      ALNSML = LOG(D1MACH(1))
      XMIN = -ALNSML
      DO 10 I=1,10
        XOLD = XMIN
        XLN = LOG(XMIN)
        XMIN = XMIN - XMIN*((XMIN+0.5D0)*XLN - XMIN - 0.2258D0 + ALNSML)   &
          / (XMIN*XLN+0.5D0)
        IF (ABS(XMIN-XOLD).LT.0.005D0) GO TO 20
 10   CONTINUE
      WRITE(ICOUT,11)
 11   FORMAT('***** ERROR FROM DGAMLM.  UNABLE TO FIND XMIN.  ******')
      CALL DPWRST('XXX','BUG ')
      RETURN
!
 20   XMIN = -XMIN + 0.01D0
!
      ALNBIG = LOG (D1MACH(2))
      XMAX = ALNBIG
      DO 30 I=1,10
        XOLD = XMAX
        XLN = LOG(XMAX)
        XMAX = XMAX - XMAX*((XMAX-0.5D0)*XLN - XMAX + 0.9189D0 - ALNBIG)   &
          / (XMAX*XLN-0.5D0)
        IF (ABS(XMAX-XOLD).LT.0.005D0) GO TO 40
 30   CONTINUE
      WRITE(ICOUT,21)
 21   FORMAT('***** ERROR FROM DGAMLM.  UNABLE TO FIND XMAX.  ******')
      CALL DPWRST('XXX','BUG ')
      RETURN
!
 40   XMAX = XMAX - 0.01D0
      XMIN = MAX (XMIN, -XMAX+1.D0)
!
      RETURN
      END SUBROUTINE DGAMLM 
      DOUBLE PRECISION FUNCTION DGAMMA (X)
!***BEGIN PROLOGUE  DGAMMA
!***PURPOSE  Compute the complete Gamma function.
!***LIBRARY   SLATEC (FNLIB)
!***CATEGORY  C7A
!***TYPE      DOUBLE PRECISION (GAMMA-S, DGAMMA-D, CGAMMA-C)
!***KEYWORDS  COMPLETE GAMMA FUNCTION, FNLIB, SPECIAL FUNCTIONS
!***AUTHOR  Fullerton, W., (LANL)
!***DESCRIPTION
!
! DGAMMA(X) calculates the double precision complete Gamma function
! for double precision argument X.
!
! Series for GAM        on the interval  0.          to  1.00000E+00
!                                        with weighted error   5.79E-32
!                                         log weighted error  31.24
!                               significant figures required  30.00
!                                    decimal places required  32.05
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  D1MACH, D9LGMC, DCSEVL, DGAMLM, INITDS, XERMSG
!***REVISION HISTORY  (YYMMDD)
!   770601  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890911  Removed unnecessary intrinsics.  (WRB)
!   890911  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!   920618  Removed space from variable name.  (RWC, WRB)
!***END PROLOGUE  DGAMMA
      DOUBLE PRECISION X, GAMCS(42), DXREL, PI, SINPIY, SQ2PIL, XMAX,   &
        XMIN, Y, D9LGMC, DCSEVL
      LOGICAL FIRST
!
      SAVE GAMCS, PI, SQ2PIL, NGAM, XMIN, XMAX, DXREL, FIRST
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DATA GAMCS(  1) / +.8571195590989331421920062399942D-2      /
      DATA GAMCS(  2) / +.4415381324841006757191315771652D-2      /
      DATA GAMCS(  3) / +.5685043681599363378632664588789D-1      /
      DATA GAMCS(  4) / -.4219835396418560501012500186624D-2      /
      DATA GAMCS(  5) / +.1326808181212460220584006796352D-2      /
      DATA GAMCS(  6) / -.1893024529798880432523947023886D-3      /
      DATA GAMCS(  7) / +.3606925327441245256578082217225D-4      /
      DATA GAMCS(  8) / -.6056761904460864218485548290365D-5      /
      DATA GAMCS(  9) / +.1055829546302283344731823509093D-5      /
      DATA GAMCS( 10) / -.1811967365542384048291855891166D-6      /
      DATA GAMCS( 11) / +.3117724964715322277790254593169D-7      /
      DATA GAMCS( 12) / -.5354219639019687140874081024347D-8      /
      DATA GAMCS( 13) / +.9193275519859588946887786825940D-9      /
      DATA GAMCS( 14) / -.1577941280288339761767423273953D-9      /
      DATA GAMCS( 15) / +.2707980622934954543266540433089D-10     /
      DATA GAMCS( 16) / -.4646818653825730144081661058933D-11     /
      DATA GAMCS( 17) / +.7973350192007419656460767175359D-12     /
      DATA GAMCS( 18) / -.1368078209830916025799499172309D-12     /
      DATA GAMCS( 19) / +.2347319486563800657233471771688D-13     /
      DATA GAMCS( 20) / -.4027432614949066932766570534699D-14     /
      DATA GAMCS( 21) / +.6910051747372100912138336975257D-15     /
      DATA GAMCS( 22) / -.1185584500221992907052387126192D-15     /
      DATA GAMCS( 23) / +.2034148542496373955201026051932D-16     /
      DATA GAMCS( 24) / -.3490054341717405849274012949108D-17     /
      DATA GAMCS( 25) / +.5987993856485305567135051066026D-18     /
      DATA GAMCS( 26) / -.1027378057872228074490069778431D-18     /
      DATA GAMCS( 27) / +.1762702816060529824942759660748D-19     /
      DATA GAMCS( 28) / -.3024320653735306260958772112042D-20     /
      DATA GAMCS( 29) / +.5188914660218397839717833550506D-21     /
      DATA GAMCS( 30) / -.8902770842456576692449251601066D-22     /
      DATA GAMCS( 31) / +.1527474068493342602274596891306D-22     /
      DATA GAMCS( 32) / -.2620731256187362900257328332799D-23     /
      DATA GAMCS( 33) / +.4496464047830538670331046570666D-24     /
      DATA GAMCS( 34) / -.7714712731336877911703901525333D-25     /
      DATA GAMCS( 35) / +.1323635453126044036486572714666D-25     /
      DATA GAMCS( 36) / -.2270999412942928816702313813333D-26     /
      DATA GAMCS( 37) / +.3896418998003991449320816639999D-27     /
      DATA GAMCS( 38) / -.6685198115125953327792127999999D-28     /
      DATA GAMCS( 39) / +.1146998663140024384347613866666D-28     /
      DATA GAMCS( 40) / -.1967938586345134677295103999999D-29     /
      DATA GAMCS( 41) / +.3376448816585338090334890666666D-30     /
      DATA GAMCS( 42) / -.5793070335782135784625493333333D-31     /
      DATA PI / 3.14159265358979323846264338327950D0 /
      DATA SQ2PIL / 0.91893853320467274178032973640562D0 /
      DATA FIRST /.TRUE./
!***FIRST EXECUTABLE STATEMENT  DGAMMA
      IF (FIRST) THEN
         NGAM = INITDS (GAMCS, 42, 0.1*REAL(D1MACH(3)) )
!
         CALL DGAMLM (XMIN, XMAX)
         DXREL = SQRT(D1MACH(4))
      ENDIF
      FIRST = .FALSE.
!
      Y = ABS(X)
      IF (Y.GT.10.D0) GO TO 50
!
! COMPUTE GAMMA(X) FOR -XBND .LE. X .LE. XBND.  REDUCE INTERVAL AND FIND
! GAMMA(1+Y) FOR 0.0 .LE. Y .LT. 1.0 FIRST OF ALL.
!
      N = INT(X+0.1)
      IF (X.LT.0.D0) N = N - 1
      Y = X - N
      N = N - 1
      DGAMMA = 0.9375D0 + DCSEVL (2.D0*Y-1.D0, GAMCS, NGAM)
      IF (N.EQ.0) RETURN
!
      IF (N.GT.0) GO TO 30
!
! COMPUTE GAMMA(X) FOR X .LT. 1.0
!
      N = -N
      IF (X .EQ. 0.D0) THEN
        WRITE(ICOUT,11)
        CALL DPWRST('XXX','BUG ')
        DGAMMA = 0.D0
        RETURN
      ENDIF
   11 FORMAT('***** ERROR FROM DGAMMA.  X IS 0.  ******')
      IF (X .LT. 0.0 .AND. X+N-2 .EQ. 0.D0)THEN
        WRITE(ICOUT,16)
        CALL DPWRST('XXX','BUG ')
        DGAMMA = 0.D0
        RETURN
      ENDIF
   16 FORMAT('***** ERROR FROM DGAMMA.  X IS A NEGATIVE INTEGER. ****')
      IF(X .LT. (-0.5D0) .AND. ABS((X-AINT(X-0.5D0))/X) .LT. DXREL)THEN
        WRITE(ICOUT,21)
        CALL DPWRST('XXX','BUG ')
      ENDIF
   21 FORMAT('***** WARNING FROM DGAMMA.  ANSWER IS LESS THAN ')
!CC22 FORMAT('      HALF PRECISION BECAUSE X IS TOO NEAR A ')
!CC23 FORMAT('      NEGATIVE INTEGER.                          *****')
!
      DO 20 I=1,N
        DGAMMA = DGAMMA/(X+I-1 )
 20   CONTINUE
      RETURN
!
! GAMMA(X) FOR X .GE. 2.0 AND X .LE. 10.0
!
 30   DO 40 I=1,N
        DGAMMA = (Y+I) * DGAMMA
 40   CONTINUE
      RETURN
!
! GAMMA(X) FOR ABS(X) .GT. 10.0.  RECALL Y = ABS(X).
!
 50   IF (X .GT. XMAX) THEN
        WRITE(ICOUT,51)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)
        CALL DPWRST('XXX','BUG ')
        DGAMMA = 0.D0
        RETURN
      ENDIF
   51 FORMAT('***** ERROR FROM DGAMMA.  X IS SO BIG THAT THE ')
   52 FORMAT('      DGAMMA FUNCTION OVERFLOWS.               *****')
!
      DGAMMA = 0.D0
      IF (X .LT. XMIN) THEN
        WRITE(ICOUT,56)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,57)
        CALL DPWRST('XXX','BUG ')
      ENDIF
   56 FORMAT('***** WARNING FROM DGAMMA.  X IS SO SMALL THAT THE ')
   57 FORMAT('      DGAMMA FUNCTION UNDERFLOWS.                 *****')
      IF (X.LT.XMIN) RETURN
!
      DGAMMA = EXP ((Y-0.5D0)*LOG(Y) - Y + SQ2PIL + D9LGMC(Y) )
      IF (X.GT.0.D0) RETURN
!
      IF (ABS((X-AINT(X-0.5D0))/X) .LT. DXREL) THEN
        WRITE(ICOUT,61)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,62)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,63)
        CALL DPWRST('XXX','BUG ')
      ENDIF
   61 FORMAT('***** WARNING FROM DGAMMA.  ANSWER IS LESS THAN ')
   62 FORMAT('      PRECISION BECAUSE X IS TOO NEAR A NEGATIVE ')
   63 FORMAT('      NUMBER.                                    *****')
!
      SINPIY = SIN (PI*Y)
      IF (SINPIY .EQ. 0.D0) THEN
        WRITE(ICOUT,71)
        CALL DPWRST('XXX','BUG ')
        DGAMMA = 0.D0
        RETURN
      ENDIF
   71 FORMAT('***** ERROR FROM DGAMMA.  X IS A NEGATIVE INTEGER. ****')
!
      DGAMMA = -PI/(Y*SINPIY*DGAMMA)
!
      RETURN
      END FUNCTION DGAMMA 
      DOUBLE PRECISION FUNCTION DGAMM2 (X)
!***BEGIN PROLOGUE  DGAMMA
!***PURPOSE  Compute the complete Gamma function.
!***LIBRARY   SLATEC (FNLIB)
!***CATEGORY  C7A
!***TYPE      DOUBLE PRECISION (GAMMA-S, DGAMMA-D, CGAMMA-C)
!***KEYWORDS  COMPLETE GAMMA FUNCTION, FNLIB, SPECIAL FUNCTIONS
!***AUTHOR  Fullerton, W., (LANL)
!***DESCRIPTION
!
! DGAMMA(X) calculates the double precision complete Gamma function
! for double precision argument X.
!
! This same as DGAMMA, except error messages are suppressed.
!
! Series for GAM        on the interval  0.          to  1.00000E+00
!                                        with weighted error   5.79E-32
!                                         log weighted error  31.24
!                               significant figures required  30.00
!                                    decimal places required  32.05
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  D1MACH, D9LGMC, DCSEVL, DGAMLM, INITDS, XERMSG
!***REVISION HISTORY  (YYMMDD)
!   770601  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890911  Removed unnecessary intrinsics.  (WRB)
!   890911  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!   920618  Removed space from variable name.  (RWC, WRB)
!***END PROLOGUE  DGAMMA
      DOUBLE PRECISION X, GAMCS(42), DXREL, PI, SINPIY, SQ2PIL, XMAX,   &
        XMIN, Y, D9LGMC, DCSEVL
      LOGICAL FIRST
!
      SAVE GAMCS, PI, SQ2PIL, NGAM, XMIN, XMAX, DXREL, FIRST
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DATA GAMCS(  1) / +.8571195590989331421920062399942D-2      /
      DATA GAMCS(  2) / +.4415381324841006757191315771652D-2      /
      DATA GAMCS(  3) / +.5685043681599363378632664588789D-1      /
      DATA GAMCS(  4) / -.4219835396418560501012500186624D-2      /
      DATA GAMCS(  5) / +.1326808181212460220584006796352D-2      /
      DATA GAMCS(  6) / -.1893024529798880432523947023886D-3      /
      DATA GAMCS(  7) / +.3606925327441245256578082217225D-4      /
      DATA GAMCS(  8) / -.6056761904460864218485548290365D-5      /
      DATA GAMCS(  9) / +.1055829546302283344731823509093D-5      /
      DATA GAMCS( 10) / -.1811967365542384048291855891166D-6      /
      DATA GAMCS( 11) / +.3117724964715322277790254593169D-7      /
      DATA GAMCS( 12) / -.5354219639019687140874081024347D-8      /
      DATA GAMCS( 13) / +.9193275519859588946887786825940D-9      /
      DATA GAMCS( 14) / -.1577941280288339761767423273953D-9      /
      DATA GAMCS( 15) / +.2707980622934954543266540433089D-10     /
      DATA GAMCS( 16) / -.4646818653825730144081661058933D-11     /
      DATA GAMCS( 17) / +.7973350192007419656460767175359D-12     /
      DATA GAMCS( 18) / -.1368078209830916025799499172309D-12     /
      DATA GAMCS( 19) / +.2347319486563800657233471771688D-13     /
      DATA GAMCS( 20) / -.4027432614949066932766570534699D-14     /
      DATA GAMCS( 21) / +.6910051747372100912138336975257D-15     /
      DATA GAMCS( 22) / -.1185584500221992907052387126192D-15     /
      DATA GAMCS( 23) / +.2034148542496373955201026051932D-16     /
      DATA GAMCS( 24) / -.3490054341717405849274012949108D-17     /
      DATA GAMCS( 25) / +.5987993856485305567135051066026D-18     /
      DATA GAMCS( 26) / -.1027378057872228074490069778431D-18     /
      DATA GAMCS( 27) / +.1762702816060529824942759660748D-19     /
      DATA GAMCS( 28) / -.3024320653735306260958772112042D-20     /
      DATA GAMCS( 29) / +.5188914660218397839717833550506D-21     /
      DATA GAMCS( 30) / -.8902770842456576692449251601066D-22     /
      DATA GAMCS( 31) / +.1527474068493342602274596891306D-22     /
      DATA GAMCS( 32) / -.2620731256187362900257328332799D-23     /
      DATA GAMCS( 33) / +.4496464047830538670331046570666D-24     /
      DATA GAMCS( 34) / -.7714712731336877911703901525333D-25     /
      DATA GAMCS( 35) / +.1323635453126044036486572714666D-25     /
      DATA GAMCS( 36) / -.2270999412942928816702313813333D-26     /
      DATA GAMCS( 37) / +.3896418998003991449320816639999D-27     /
      DATA GAMCS( 38) / -.6685198115125953327792127999999D-28     /
      DATA GAMCS( 39) / +.1146998663140024384347613866666D-28     /
      DATA GAMCS( 40) / -.1967938586345134677295103999999D-29     /
      DATA GAMCS( 41) / +.3376448816585338090334890666666D-30     /
      DATA GAMCS( 42) / -.5793070335782135784625493333333D-31     /
      DATA PI / 3.14159265358979323846264338327950D0 /
      DATA SQ2PIL / 0.91893853320467274178032973640562D0 /
      DATA FIRST /.TRUE./
!***FIRST EXECUTABLE STATEMENT  DGAMMA
      IF (FIRST) THEN
         NGAM = INITDS (GAMCS, 42, 0.1*REAL(D1MACH(3)) )
!
         CALL DGAMLM (XMIN, XMAX)
         DXREL = SQRT(D1MACH(4))
      ENDIF
      FIRST = .FALSE.
!
      Y = ABS(X)
      IF (Y.GT.10.D0) GO TO 50
!
! COMPUTE GAMMA(X) FOR -XBND .LE. X .LE. XBND.  REDUCE INTERVAL AND FIND
! GAMMA(1+Y) FOR 0.0 .LE. Y .LT. 1.0 FIRST OF ALL.
!
      N = INT(X)
      IF (X.LT.0.D0) N = N - 1
      Y = X - REAL(N)
      N = N - 1
      DGAMM2 = 0.9375D0 + DCSEVL (2.D0*Y-1.D0, GAMCS, NGAM)
      IF (N.EQ.0) RETURN
!
      IF (N.GT.0) GO TO 30
!
! COMPUTE GAMMA(X) FOR X .LT. 1.0
!
      N = -N
      IF (X .EQ. 0.D0) THEN
        DGAMM2 = 0.D0
        RETURN
      ENDIF
      IF (X .LT. 0.0 .AND. X+N-2 .EQ. 0.D0)THEN
        DGAMM2 = 0.D0
        RETURN
      ENDIF
      IF(X .LT. (-0.5D0) .AND. ABS((X-AINT(X-0.5D0))/X) .LT. DXREL)THEN
        CONTINUE
      ENDIF
!
      DO 20 I=1,N
        DGAMM2 = DGAMM2/(X+I-1 )
 20   CONTINUE
      RETURN
!
! GAMMA(X) FOR X .GE. 2.0 AND X .LE. 10.0
!
 30   DO 40 I=1,N
        DGAMM2 = (Y+I) * DGAMM2
 40   CONTINUE
      RETURN
!
! GAMMA(X) FOR ABS(X) .GT. 10.0.  RECALL Y = ABS(X).
!
 50   IF (X .GT. XMAX) THEN
        DGAMM2 = 0.D0
        RETURN
      ENDIF
!
      DGAMM2 = 0.D0
      IF (X .LT. XMIN) THEN
        CONTINUE
      ENDIF
      IF (X.LT.XMIN) RETURN
!
      DGAMM2 = EXP ((Y-0.5D0)*LOG(Y) - Y + SQ2PIL + D9LGMC(Y) )
      IF (X.GT.0.D0) RETURN
!
      IF (ABS((X-AINT(X-0.5D0))/X) .LT. DXREL) THEN
        CONTINUE
      ENDIF
!
      SINPIY = SIN (PI*Y)
      IF (SINPIY .EQ. 0.D0) THEN
        DGAMM2 = 0.D0
        RETURN
      ENDIF
!
      DGAMM2 = -PI/(Y*SINPIY*DGAMM2)
!
      RETURN
      END FUNCTION DGAMM2 
      SUBROUTINE DGAMMF(DX,DGF)
!
!     THIS PROGRAM CALCULATES THE GAMMA FUNCTION
!     THE INPUT IS DOUBLE PRECISION DX
!     THE OUTPUT IS DOUBLE PRECISION DGF
!     ALL INTERNAL OPERATIONS ARE DONE IN DOUBLE PRECISION
!     THE ALGORITHM IS TO USE THE RECURSION FORMULA G(X)=G(X+1)/X
!     UNTIL X IS LARGE ENOUGH TO USE AN ASYMPTOTIC FORMULA FOR G(X)--THE CUT-OFF
!     POINT USED WAS X = 10
!     THE ASYMPTOTIC FORMULA USED IS IN AMS 55, PAGE 257, 6.1.41 (THE FIRST 9
!     TERMS OF THE SERIES WERE USED--I.E., OUT TO X**-17)
!     ALTHOUGH THE DATA STATEMENT DEFINES 10 COEFFICIENTS, THE PROGRAM MAKES USE
!     OF ONLY 9 COEFFICIENTS (THE ERROR BEING BOUNDED BY THE TENTH COEFFICIENT
!     DIVIDED BY X**19
!     SUBROUTINES NEEDED--NONE
!     PRINTING--NONE UNLESS AN ERROR CONDITION EXISTS
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-921-3651
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--JUNE      1972.
!     UPDATED         --FEBRUARY  1981.
!     UPDATED         --FEBRUARY  1982.
!     UPDATED         --MAY       1982.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DX
      DOUBLE PRECISION DGF
      DOUBLE PRECISION Y,Y2,Y3,Y4,Y5,DEN,A,B,C,D
!
      DIMENSION D(10)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
      DATA C/ .918938533204672741D0/
      DATA D(1),D(2),D(3),D(4),D(5)   &
           /+.833333333333333333D-1,-.277777777777777778D-2,   &
            +.793650793650793651D-3,-.595238095238095238D-3,   &
            +.841750841750841751D-3/
      DATA D(6),D(7),D(8),D(9),D(10)   &
           /-.191752691752691753D-2,+.641025641025641025D-2,   &
            -.295506535947712418D-1,+.179644372368830573D0,   &
            -.139243221690590111D1/
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(DX.LE.0.0D0)GO TO 50
      GO TO 90
   50 WRITE(ICOUT,5)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,45)DX
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
   90 CONTINUE
    5 FORMAT('***** FATAL ERROR--THE FIRST  INPUT ARGUMENT ',   &
      'TO THE DGAMMF SUBROUTINE IS NON-POSITIVE *****')
   45 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',D22.15,' *****')
!
      Y=DX
      DEN=1.0D0
  100 IF(Y.GE.10.0D0)GO TO 200
      DEN=DEN*Y
      Y=Y+1
      GO TO 100
  200 Y2=Y*Y
      Y3=Y*Y2
      Y4=Y2*Y2
      Y5=Y2*Y3
      A=(Y-0.5D0)*DLOG(Y)-Y+C
      B=D(1)/Y+D(2)/Y3+D(3)/Y5+D(4)/(Y2*Y5)+D(5)/(Y4*Y5)+   &
      D(6)/(Y*Y5*Y5)+D(7)/(Y3*Y5*Y5)+D(8)/(Y5*Y5*Y5)+D(9)/(Y2*Y5*Y5*Y5)
      DGF=DEXP(A+B)/DEN
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE DGAMMF
      DOUBLE PRECISION FUNCTION DGAMR (X)
!***BEGIN PROLOGUE  DGAMR
!***PURPOSE  Compute the reciprocal of the Gamma function.
!***LIBRARY   SLATEC (FNLIB)
!***CATEGORY  C7A
!***TYPE      DOUBLE PRECISION (GAMR-S, DGAMR-D, CGAMR-C)
!***KEYWORDS  FNLIB, RECIPROCAL GAMMA FUNCTION, SPECIAL FUNCTIONS
!***AUTHOR  Fullerton, W., (LANL)
!***DESCRIPTION
!
! DGAMR(X) calculates the double precision reciprocal of the
! complete Gamma function for double precision argument X.
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  DGAMMA, DLGAMS, XERCLR, XGETF, XSETF
!***REVISION HISTORY  (YYMMDD)
!   770701  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890531  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900727  Added EXTERNAL statement.  (WRB)
!***END PROLOGUE  DGAMR
      DOUBLE PRECISION X, ALNGX, SGNGX, DGAMMA
      EXTERNAL DGAMMA
!***FIRST EXECUTABLE STATEMENT  DGAMR
      DGAMR = 0.0D0
      IF (X.LE.0.0D0 .AND. AINT(X).EQ.X) RETURN
!
      IF (ABS(X).GT.10.0D0) GO TO 10
      DGAMR = 1.0D0/DGAMMA(X)
      RETURN
!
 10   CALL DLGAMS (X, ALNGX, SGNGX)
      DGAMR = SGNGX * EXP(-ALNGX)
      RETURN
!
      END FUNCTION DGAMR 
      SUBROUTINE DGACDF(X,GAMMA,CDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
!              FUNCTION VALUE FOR THE DOUBLE GAMMA
!              DISTRIBUTION WITH SINGLE PRECISION
!              TAIL LENGTH PARAMETER = GAMMA.
!              THE DOUBLE GAMMA DISTRIBUTION USED
!              HEREIN IS DEFINED FOR ALL REAL X,
!              AND HAS THE PROBABILITY DENSITY FUNCTION
!                 F(X) = (1/2)*ABS(X)**(GAMMA-1)*EXP(-ABS(X))/GAMMA(X)
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE PROBABILITY DENSITY
!                                FUNCTION IS TO BE EVALUATED.
!                     --GAMMA  = THE SHAPE PARAMETER
!                                GAMMA SHOULD BE POSITIVE.
!     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION PROBABILITY
!                                DENSITY FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
!             FUNCTION VALUE CDF FOR THE DOUBLE GAMMA DISTRIBUTION
!             WITH TAIL LENGHT PARAMETER = GAMMA.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--GAMMA SHOULD BE POSITIVE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--GAMCDF.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCE --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--1, 2ND. ED., 1994, PAGE 387
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1966)
!     VERSION NUMBER--96/1
!     ORIGINAL VERSION--JANUARY   1996.
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(GAMMA.LE.0)THEN
        WRITE(ICOUT,15)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)GAMMA
        CALL DPWRST('XXX','BUG ')
        CDF=0.0
        GO TO 9999
      ENDIF
   15 FORMAT('***** FATAL ERROR--THE 2ND INPUT ARGUMENT TO THE ',   &
      'DGACDF SUBROUTINE IS NON-POSITIVE *****')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',E15.8,' *****')
!
      IF(X.EQ.0.0)THEN
        CDF=0.5
      ELSEIF(X.GT.0.0)THEN
        CALL GAMCDF(X,GAMMA,CDF2)
        CDF=0.5+CDF2/2.0
      ELSE
        ARG1=-X
        CALL GAMCDF(ARG1,GAMMA,CDF2)
        CDF=0.5-CDF2/2.0
      ENDIF
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE DGACDF
      SUBROUTINE DGAPDF(X,GAMMA,PDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
!              FUNCTION VALUE FOR THE DOUBLE GAMMA
!              DISTRIBUTION WITH SINGLE PRECISION
!              TAIL LENGTH PARAMETER = GAMMA.
!              THE DOUBLE GAMMA DISTRIBUTION USED
!              HEREIN IS DEFINED FOR ALL REAL X,
!              AND HAS THE PROBABILITY DENSITY FUNCTION
!                 F(X) = (1/2)*ABS(X)**(GAMMA-1)*EXP(-ABS(X))/GAMMA(X)
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE PROBABILITY DENSITY
!                                FUNCTION IS TO BE EVALUATED.
!                     --GAMMA  = THE SHAPE PARAMETER
!                                GAMMA SHOULD BE POSITIVE.
!     OUTPUT ARGUMENTS--PDF    = THE SINGLE PRECISION PROBABILITY
!                                DENSITY FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
!             FUNCTION VALUE PDF FOR THE DOUBLE GAMMA DISTRIBUTION
!             WITH TAIL LENGHT PARAMETER = GAMMA.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--GAMMA SHOULD BE POSITIVE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--EXP.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCE --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--1, 2ND. ED., 1994, PAGE 387
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1966)
!     VERSION NUMBER--96/1
!     ORIGINAL VERSION--JANUARY   1996.
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(GAMMA.LE.0)THEN
        WRITE(ICOUT,15)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)GAMMA
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GO TO 9999
      ENDIF
   15 FORMAT('***** FATAL ERROR--THE 2ND INPUT ARGUMENT TO THE ',   &
      'DGAPDF SUBROUTINE IS NON-POSITIVE *****')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',E15.8,' *****')
!
      ARG1=ABS(X)
      CALL GAMPDF(ARG1,GAMMA,PDF2)
      PDF=PDF2/2.0
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE DGAPDF
      SUBROUTINE DGAPPF(P,GAMMA,PPF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
!              FUNCTION VALUE FOR THE DOUBLE GAMMA
!              DISTRIBUTION WITH SINGLE PRECISION
!              TAIL LENGTH PARAMETER = GAMMA.
!              THE DOUBLE GAMMA DISTRIBUTION USED
!              HEREIN IS DEFINED FOR ALL REAL X,
!              AND HAS THE PROBABILITY DENSITY FUNCTION
!                 F(X) = (1/2)*ABS(X)**(GAMMA-1)*EXP(-ABS(X))/GAMMA(X)
!     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE
!                                (BETWEEN 0.0 (INCLUSIVELY)
!                                AND 1.0 (EXCLUSIVELY))
!                                AT WHICH THE PERCENT POINT
!                                FUNCTION IS TO BE EVALUATED.
!                     --GAMMA  = THE SINGLE PRECISION VALUE
!                                OF THE TAIL LENGTH PARAMETER.
!                                GAMMA SHOULD BE POSITIVE.
!     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT
!                                POINT FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PERCENT POINT FUNCTION .
!             VALUE PPF FOR THE GAMMA DISTRIBUTION
!             WITH TAIL LENGTH PARAMETER VALUE = GAMMA.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--GAMMA SHOULD BE POSITIVE.
!                 --P SHOULD BE BETWEEN 0.0 (EXCLUSIVELY)
!                   AND 1.0 (EXCLUSIVELY).
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--LOG.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCE --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--1, 2ND. ED., 1994, PAGE 387
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1966)
!     VERSION NUMBER--96/1
!     ORIGINAL VERSION--JANUARY   1996.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(P.LE.0.0.OR.P.GE.1.0)THEN
        WRITE(ICOUT,1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
        CALL DPWRST('XXX','BUG ')
        PPF=0.0
        GO TO 9999
      ENDIF
      IF(GAMMA.LE.0.0)THEN
        WRITE(ICOUT,15)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)GAMMA
        CALL DPWRST('XXX','BUG ')
        PPF=0.0
        GO TO 9999
      ENDIF
    1 FORMAT('***** FATAL ERROR--THE 1ST INPUT ARGUMENT TO THE ',   &
      'DGAPPF SUBROUTINE IS OUTSIDE THE ALLOWABLE (0,1) INTERVAL')
   15 FORMAT('***** FATAL ERROR--THE 2ND INPUT ARGUMENT TO THE ',   &
      'DGAPPF SUBROUTINE IS NON-POSITIVE *****')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',E15.8,' *****')
!
      IF(P.EQ.0.5)THEN
        PPF=0.0
      ELSEIF(P.LT.0.5)THEN
        ARG1=2.0*(0.5-P)
        CALL GAMPPF(ARG1,GAMMA,PPF)
        PPF=-PPF
      ELSE
        ARG1=2.0*(P-0.5)
        CALL GAMPPF(ARG1,GAMMA,PPF)
      ENDIF
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE DGAPPF
      SUBROUTINE DGARAN(N,GAMMA,ISEED,X)
!
!     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
!              FROM THE DOUBLE GAMMA DISTRIBUTION
!              WITH TAIL LENGTH PARAMETER VALUE = GAMMA.
!     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
!                                OF RANDOM NUMBERS TO BE
!                                GENERATED.
!                     --GAMMA  = THE SINGLE PRECISION VALUE OF THE
!                                TAIL LENGTH PARAMETER.
!                                GAMMA SHOULD BE POSITIVE.
!     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
!                                (OF DIMENSION AT LEAST N)
!                                INTO WHICH THE GENERATED
!                                RANDOM SAMPLE WILL BE PLACED.
!     OUTPUT--A RANDOM SAMPLE OF SIZE N
!             FROM THE DOUBLE GAMMA DISTRIBUTION
!             WITH TAIL LENGTH PARAMETER VALUE = GAMMA.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!                 --GAMMA SHOULD BE POSITIVE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--XX
!               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--1, 2ND. ED., 1994.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1966)
!     VERSION NUMBER--2001.10
!     ORIGINAL VERSION--OCTOBER   2001.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(N.LT.1)THEN
        WRITE(ICOUT, 5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
      IF(GAMMA.LE.0.0)THEN
        WRITE(ICOUT,15)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)GAMMA
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
    5 FORMAT('***** FATAL ERROR--THE 1ST INPUT ARGUMENT TO THE ',   &
      'DGARAN SUBROUTINE IS NON-POSITIVE *****')
   15 FORMAT('***** FATAL ERROR--THE 2ND INPUT ARGUMENT TO THE ',   &
      'DGARAN SUBROUTINE IS NON-POSITIVE *****')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',E15.8,' *****')
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8,' *****')
!
!     GENERATE N UNIFORM (0,1) RANDOM NUMBERS;
!
      CALL UNIRAN(N,ISEED,X)
!
!     GENERATE N INVERTED WEIBULL DISTRIBUTION RANDOM NUMBERS
!     USING THE PERCENT POINT FUNCTION TRANSFORMATION METHOD.
!
      DO 100 I=1,N
        CALL DGAPPF(X(I),GAMMA,XTEMP)
        X(I)=XTEMP
  100 CONTINUE
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE DGARAN
      SUBROUTINE DGCL1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR GREEK COMPLEX LOWER CASE (PART 1).
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-921-3651
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
!     DEFINE CHARACTER   2127--LOWER CASE ALPH
!
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',  -1,   5/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',  -4,   4/
      DATA IOPERA(   3),IX(   3),IY(   3)/'DRAW',  -6,   2/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',  -7,   0/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',  -8,  -3/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',  -8,  -6/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',  -7,  -8/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',  -4,  -9/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',  -2,  -9/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',   0,  -8/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',   3,  -5/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',   5,  -2/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',   7,   2/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',   8,   5/
      DATA IOPERA(  15),IX(  15),IY(  15)/'MOVE',  -1,   5/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',  -3,   4/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',  -5,   2/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',  -6,   0/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',  -7,  -3/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',  -7,  -6/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',  -6,  -8/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',  -4,  -9/
      DATA IOPERA(  23),IX(  23),IY(  23)/'MOVE',  -1,   5/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',   1,   5/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',   3,   4/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',   4,   2/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',   6,  -6/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',   7,  -8/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',   8,  -9/
      DATA IOPERA(  30),IX(  30),IY(  30)/'MOVE',   1,   5/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',   2,   4/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',   3,   2/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',   5,  -6/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',   6,  -8/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',   8,  -9/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',   9,  -9/
!
      DATA IXMIND(   1)/ -11/
      DATA IXMAXD(   1)/  12/
      DATA IXDELD(   1)/  23/
      DATA ISTARD(   1)/   1/
      DATA NUMCOO(   1)/  36/
!
!     DEFINE CHARACTER   2128--LOWER CASE BETA
!
      DATA IOPERA(  37),IX(  37),IY(  37)/'MOVE',   2,  12/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',  -1,  11/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',  -3,   9/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',  -5,   5/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',  -6,   2/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',  -7,  -2/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',  -8,  -8/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',  -9, -16/
      DATA IOPERA(  45),IX(  45),IY(  45)/'MOVE',   2,  12/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW',   0,  11/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',  -2,   9/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',  -4,   5/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',  -5,   2/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW',  -6,  -2/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW',  -7,  -8/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',  -8, -16/
      DATA IOPERA(  53),IX(  53),IY(  53)/'MOVE',   2,  12/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',   4,  12/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',   6,  11/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',   7,  10/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',   7,   7/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',   6,   5/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',   5,   4/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW',   2,   3/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',  -2,   3/
      DATA IOPERA(  62),IX(  62),IY(  62)/'MOVE',   4,  12/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',   6,  10/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',   6,   7/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',   5,   5/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW',   4,   4/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',   2,   3/
      DATA IOPERA(  68),IX(  68),IY(  68)/'MOVE',  -2,   3/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',   2,   2/
      DATA IOPERA(  70),IX(  70),IY(  70)/'DRAW',   4,   0/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',   5,  -2/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',   5,  -5/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',   4,  -7/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',   3,  -8/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',   0,  -9/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',  -2,  -9/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',  -4,  -8/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',  -5,  -7/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',  -6,  -4/
      DATA IOPERA(  80),IX(  80),IY(  80)/'MOVE',  -2,   3/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',   1,   2/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',   3,   0/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',   4,  -2/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW',   4,  -5/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',   3,  -7/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',   2,  -8/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',   0,  -9/
!
      DATA IXMIND(   2)/ -11/
      DATA IXMAXD(   2)/  10/
      DATA IXDELD(   2)/  21/
      DATA ISTARD(   2)/  37/
      DATA NUMCOO(   2)/  51/
!
!     DEFINE CHARACTER   2129--LOWER CASE GAMM
!
      DATA IOPERA(  88),IX(  88),IY(  88)/'MOVE',  -9,   2/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',  -7,   4/
      DATA IOPERA(  90),IX(  90),IY(  90)/'DRAW',  -5,   5/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',  -3,   5/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW',  -1,   4/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',   0,   3/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',   1,   0/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',   1,  -4/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',   0,  -8/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',  -3, -16/
      DATA IOPERA(  98),IX(  98),IY(  98)/'MOVE',  -8,   3/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',  -6,   4/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',  -2,   4/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',   0,   3/
      DATA IOPERA( 102),IX( 102),IY( 102)/'MOVE',   8,   5/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',   7,   2/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',   6,   0/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',   1,  -7/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',  -2, -12/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',  -4, -16/
      DATA IOPERA( 108),IX( 108),IY( 108)/'MOVE',   7,   5/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',   6,   2/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',   5,   0/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',   1,  -7/
!
      DATA IXMIND(   3)/ -10/
      DATA IXMAXD(   3)/  10/
      DATA IXDELD(   3)/  20/
      DATA ISTARD(   3)/  88/
      DATA NUMCOO(   3)/  24/
!
!     DEFINE CHARACTER   2130--LOWER CASE DELT
!
      DATA IOPERA( 112),IX( 112),IY( 112)/'MOVE',   4,   4/
      DATA IOPERA( 113),IX( 113),IY( 113)/'DRAW',   2,   5/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',   0,   5/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',  -3,   4/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',  -5,   1/
      DATA IOPERA( 117),IX( 117),IY( 117)/'DRAW',  -6,  -2/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',  -6,  -5/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',  -5,  -7/
      DATA IOPERA( 120),IX( 120),IY( 120)/'DRAW',  -4,  -8/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW',  -2,  -9/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW',   0,  -9/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',   3,  -8/
      DATA IOPERA( 124),IX( 124),IY( 124)/'DRAW',   5,  -5/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',   6,  -2/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW',   6,   1/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW',   5,   3/
      DATA IOPERA( 128),IX( 128),IY( 128)/'DRAW',   1,   8/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',   0,  10/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',   0,  12/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',   1,  13/
      DATA IOPERA( 132),IX( 132),IY( 132)/'DRAW',   3,  13/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',   5,  12/
      DATA IOPERA( 134),IX( 134),IY( 134)/'DRAW',   7,  10/
      DATA IOPERA( 135),IX( 135),IY( 135)/'MOVE',   0,   5/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',  -2,   4/
      DATA IOPERA( 137),IX( 137),IY( 137)/'DRAW',  -4,   1/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW',  -5,  -2/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW',  -5,  -6/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW',  -4,  -8/
      DATA IOPERA( 141),IX( 141),IY( 141)/'MOVE',   0,  -9/
      DATA IOPERA( 142),IX( 142),IY( 142)/'DRAW',   2,  -8/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',   4,  -5/
      DATA IOPERA( 144),IX( 144),IY( 144)/'DRAW',   5,  -2/
      DATA IOPERA( 145),IX( 145),IY( 145)/'DRAW',   5,   2/
      DATA IOPERA( 146),IX( 146),IY( 146)/'DRAW',   4,   4/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',   2,   7/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',   1,   9/
      DATA IOPERA( 149),IX( 149),IY( 149)/'DRAW',   1,  11/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',   2,  12/
      DATA IOPERA( 151),IX( 151),IY( 151)/'DRAW',   4,  12/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',   7,  10/
!
      DATA IXMIND(   4)/  -9/
      DATA IXMAXD(   4)/  10/
      DATA IXDELD(   4)/  19/
      DATA ISTARD(   4)/ 112/
      DATA NUMCOO(   4)/  41/
!
!     DEFINE CHARACTER   2131--LOWER CASE EPSI
!
      DATA IOPERA( 153),IX( 153),IY( 153)/'MOVE',   6,   2/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',   4,   4/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW',   2,   5/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',  -2,   5/
      DATA IOPERA( 157),IX( 157),IY( 157)/'DRAW',  -4,   4/
      DATA IOPERA( 158),IX( 158),IY( 158)/'DRAW',  -4,   2/
      DATA IOPERA( 159),IX( 159),IY( 159)/'DRAW',  -2,   0/
      DATA IOPERA( 160),IX( 160),IY( 160)/'DRAW',   1,  -1/
      DATA IOPERA( 161),IX( 161),IY( 161)/'MOVE',  -2,   5/
      DATA IOPERA( 162),IX( 162),IY( 162)/'DRAW',  -3,   4/
      DATA IOPERA( 163),IX( 163),IY( 163)/'DRAW',  -3,   2/
      DATA IOPERA( 164),IX( 164),IY( 164)/'DRAW',  -1,   0/
      DATA IOPERA( 165),IX( 165),IY( 165)/'DRAW',   1,  -1/
      DATA IOPERA( 166),IX( 166),IY( 166)/'MOVE',   1,  -1/
      DATA IOPERA( 167),IX( 167),IY( 167)/'DRAW',  -4,  -2/
      DATA IOPERA( 168),IX( 168),IY( 168)/'DRAW',  -6,  -4/
      DATA IOPERA( 169),IX( 169),IY( 169)/'DRAW',  -6,  -6/
      DATA IOPERA( 170),IX( 170),IY( 170)/'DRAW',  -5,  -8/
      DATA IOPERA( 171),IX( 171),IY( 171)/'DRAW',  -2,  -9/
      DATA IOPERA( 172),IX( 172),IY( 172)/'DRAW',   1,  -9/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW',   3,  -8/
      DATA IOPERA( 174),IX( 174),IY( 174)/'DRAW',   5,  -6/
      DATA IOPERA( 175),IX( 175),IY( 175)/'MOVE',   1,  -1/
      DATA IOPERA( 176),IX( 176),IY( 176)/'DRAW',  -3,  -2/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW',  -5,  -4/
      DATA IOPERA( 178),IX( 178),IY( 178)/'DRAW',  -5,  -6/
      DATA IOPERA( 179),IX( 179),IY( 179)/'DRAW',  -4,  -8/
      DATA IOPERA( 180),IX( 180),IY( 180)/'DRAW',  -2,  -9/
!
      DATA IXMIND(   5)/  -9/
      DATA IXMAXD(   5)/   9/
      DATA IXDELD(   5)/  18/
      DATA ISTARD(   5)/ 153/
      DATA NUMCOO(   5)/  28/
!
!     DEFINE CHARACTER   2132--LOWER CASE ZETA
!
      DATA IOPERA( 181),IX( 181),IY( 181)/'MOVE',   2,  12/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',   0,  11/
      DATA IOPERA( 183),IX( 183),IY( 183)/'DRAW',  -1,  10/
      DATA IOPERA( 184),IX( 184),IY( 184)/'DRAW',  -1,   9/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',   0,   8/
      DATA IOPERA( 186),IX( 186),IY( 186)/'DRAW',   3,   7/
      DATA IOPERA( 187),IX( 187),IY( 187)/'DRAW',   8,   7/
      DATA IOPERA( 188),IX( 188),IY( 188)/'DRAW',   8,   8/
      DATA IOPERA( 189),IX( 189),IY( 189)/'DRAW',   5,   7/
      DATA IOPERA( 190),IX( 190),IY( 190)/'DRAW',   1,   5/
      DATA IOPERA( 191),IX( 191),IY( 191)/'DRAW',  -2,   3/
      DATA IOPERA( 192),IX( 192),IY( 192)/'DRAW',  -5,   0/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW',  -6,  -3/
      DATA IOPERA( 194),IX( 194),IY( 194)/'DRAW',  -6,  -5/
      DATA IOPERA( 195),IX( 195),IY( 195)/'DRAW',  -5,  -7/
      DATA IOPERA( 196),IX( 196),IY( 196)/'DRAW',  -2,  -9/
      DATA IOPERA( 197),IX( 197),IY( 197)/'DRAW',   1, -11/
      DATA IOPERA( 198),IX( 198),IY( 198)/'DRAW',   2, -13/
      DATA IOPERA( 199),IX( 199),IY( 199)/'DRAW',   2, -15/
      DATA IOPERA( 200),IX( 200),IY( 200)/'DRAW',   1, -16/
      DATA IOPERA( 201),IX( 201),IY( 201)/'DRAW',  -1, -16/
      DATA IOPERA( 202),IX( 202),IY( 202)/'DRAW',  -2, -15/
      DATA IOPERA( 203),IX( 203),IY( 203)/'MOVE',   3,   6/
      DATA IOPERA( 204),IX( 204),IY( 204)/'DRAW',  -1,   3/
      DATA IOPERA( 205),IX( 205),IY( 205)/'DRAW',  -4,   0/
      DATA IOPERA( 206),IX( 206),IY( 206)/'DRAW',  -5,  -3/
      DATA IOPERA( 207),IX( 207),IY( 207)/'DRAW',  -5,  -5/
      DATA IOPERA( 208),IX( 208),IY( 208)/'DRAW',  -4,  -7/
      DATA IOPERA( 209),IX( 209),IY( 209)/'DRAW',  -2,  -9/
!
      DATA IXMIND(   6)/  -9/
      DATA IXMAXD(   6)/   9/
      DATA IXDELD(   6)/  18/
      DATA ISTARD(   6)/ 181/
      DATA NUMCOO(   6)/  29/
!
!     DEFINE CHARACTER   2133--LOWER CASE ETA
!
      DATA IOPERA( 210),IX( 210),IY( 210)/'MOVE', -10,   1/
      DATA IOPERA( 211),IX( 211),IY( 211)/'DRAW',  -9,   3/
      DATA IOPERA( 212),IX( 212),IY( 212)/'DRAW',  -7,   5/
      DATA IOPERA( 213),IX( 213),IY( 213)/'DRAW',  -4,   5/
      DATA IOPERA( 214),IX( 214),IY( 214)/'DRAW',  -3,   4/
      DATA IOPERA( 215),IX( 215),IY( 215)/'DRAW',  -3,   2/
      DATA IOPERA( 216),IX( 216),IY( 216)/'DRAW',  -4,  -2/
      DATA IOPERA( 217),IX( 217),IY( 217)/'DRAW',  -6,  -9/
      DATA IOPERA( 218),IX( 218),IY( 218)/'MOVE',  -5,   5/
      DATA IOPERA( 219),IX( 219),IY( 219)/'DRAW',  -4,   4/
      DATA IOPERA( 220),IX( 220),IY( 220)/'DRAW',  -4,   2/
      DATA IOPERA( 221),IX( 221),IY( 221)/'DRAW',  -5,  -2/
      DATA IOPERA( 222),IX( 222),IY( 222)/'DRAW',  -7,  -9/
      DATA IOPERA( 223),IX( 223),IY( 223)/'MOVE',  -4,  -2/
      DATA IOPERA( 224),IX( 224),IY( 224)/'DRAW',  -2,   2/
      DATA IOPERA( 225),IX( 225),IY( 225)/'DRAW',   0,   4/
      DATA IOPERA( 226),IX( 226),IY( 226)/'DRAW',   2,   5/
      DATA IOPERA( 227),IX( 227),IY( 227)/'DRAW',   4,   5/
      DATA IOPERA( 228),IX( 228),IY( 228)/'DRAW',   6,   4/
      DATA IOPERA( 229),IX( 229),IY( 229)/'DRAW',   7,   3/
      DATA IOPERA( 230),IX( 230),IY( 230)/'DRAW',   7,   0/
      DATA IOPERA( 231),IX( 231),IY( 231)/'DRAW',   6,  -5/
      DATA IOPERA( 232),IX( 232),IY( 232)/'DRAW',   3, -16/
      DATA IOPERA( 233),IX( 233),IY( 233)/'MOVE',   4,   5/
      DATA IOPERA( 234),IX( 234),IY( 234)/'DRAW',   6,   3/
      DATA IOPERA( 235),IX( 235),IY( 235)/'DRAW',   6,   0/
      DATA IOPERA( 236),IX( 236),IY( 236)/'DRAW',   5,  -5/
      DATA IOPERA( 237),IX( 237),IY( 237)/'DRAW',   2, -16/
!
      DATA IXMIND(   7)/ -11/
      DATA IXMAXD(   7)/  11/
      DATA IXDELD(   7)/  22/
      DATA ISTARD(   7)/ 210/
      DATA NUMCOO(   7)/  28/
!
!     DEFINE CHARACTER   2134--LOWER CASE THET
!
      DATA IOPERA( 238),IX( 238),IY( 238)/'MOVE', -11,   1/
      DATA IOPERA( 239),IX( 239),IY( 239)/'DRAW', -10,   3/
      DATA IOPERA( 240),IX( 240),IY( 240)/'DRAW',  -8,   5/
      DATA IOPERA( 241),IX( 241),IY( 241)/'DRAW',  -5,   5/
      DATA IOPERA( 242),IX( 242),IY( 242)/'DRAW',  -4,   4/
      DATA IOPERA( 243),IX( 243),IY( 243)/'DRAW',  -4,   2/
      DATA IOPERA( 244),IX( 244),IY( 244)/'DRAW',  -5,  -3/
      DATA IOPERA( 245),IX( 245),IY( 245)/'DRAW',  -5,  -6/
      DATA IOPERA( 246),IX( 246),IY( 246)/'DRAW',  -4,  -8/
      DATA IOPERA( 247),IX( 247),IY( 247)/'DRAW',  -3,  -9/
      DATA IOPERA( 248),IX( 248),IY( 248)/'MOVE',  -6,   5/
      DATA IOPERA( 249),IX( 249),IY( 249)/'DRAW',  -5,   4/
      DATA IOPERA( 250),IX( 250),IY( 250)/'DRAW',  -5,   2/
      DATA IOPERA( 251),IX( 251),IY( 251)/'DRAW',  -6,  -3/
      DATA IOPERA( 252),IX( 252),IY( 252)/'DRAW',  -6,  -6/
      DATA IOPERA( 253),IX( 253),IY( 253)/'DRAW',  -5,  -8/
      DATA IOPERA( 254),IX( 254),IY( 254)/'DRAW',  -3,  -9/
      DATA IOPERA( 255),IX( 255),IY( 255)/'DRAW',  -1,  -9/
      DATA IOPERA( 256),IX( 256),IY( 256)/'DRAW',   1,  -8/
      DATA IOPERA( 257),IX( 257),IY( 257)/'DRAW',   3,  -6/
      DATA IOPERA( 258),IX( 258),IY( 258)/'DRAW',   5,  -3/
      DATA IOPERA( 259),IX( 259),IY( 259)/'DRAW',   6,   0/
      DATA IOPERA( 260),IX( 260),IY( 260)/'DRAW',   7,   5/
      DATA IOPERA( 261),IX( 261),IY( 261)/'DRAW',   7,   9/
      DATA IOPERA( 262),IX( 262),IY( 262)/'DRAW',   6,  11/
      DATA IOPERA( 263),IX( 263),IY( 263)/'DRAW',   4,  12/
      DATA IOPERA( 264),IX( 264),IY( 264)/'DRAW',   2,  12/
      DATA IOPERA( 265),IX( 265),IY( 265)/'DRAW',   0,  10/
      DATA IOPERA( 266),IX( 266),IY( 266)/'DRAW',   0,   8/
      DATA IOPERA( 267),IX( 267),IY( 267)/'DRAW',   1,   5/
      DATA IOPERA( 268),IX( 268),IY( 268)/'DRAW',   3,   2/
      DATA IOPERA( 269),IX( 269),IY( 269)/'DRAW',   5,   0/
      DATA IOPERA( 270),IX( 270),IY( 270)/'DRAW',   8,  -2/
      DATA IOPERA( 271),IX( 271),IY( 271)/'MOVE',   1,  -8/
      DATA IOPERA( 272),IX( 272),IY( 272)/'DRAW',   3,  -5/
      DATA IOPERA( 273),IX( 273),IY( 273)/'DRAW',   4,  -3/
      DATA IOPERA( 274),IX( 274),IY( 274)/'DRAW',   5,   0/
      DATA IOPERA( 275),IX( 275),IY( 275)/'DRAW',   6,   5/
      DATA IOPERA( 276),IX( 276),IY( 276)/'DRAW',   6,   9/
      DATA IOPERA( 277),IX( 277),IY( 277)/'DRAW',   5,  11/
      DATA IOPERA( 278),IX( 278),IY( 278)/'DRAW',   4,  12/
!
      DATA IXMIND(   8)/ -12/
      DATA IXMAXD(   8)/  11/
      DATA IXDELD(   8)/  23/
      DATA ISTARD(   8)/ 238/
      DATA NUMCOO(   8)/  41/
!
!     DEFINE CHARACTER   2135--LOWER CASE IOTA
!
      DATA IOPERA( 279),IX( 279),IY( 279)/'MOVE',   0,   5/
      DATA IOPERA( 280),IX( 280),IY( 280)/'DRAW',  -2,  -2/
      DATA IOPERA( 281),IX( 281),IY( 281)/'DRAW',  -3,  -6/
      DATA IOPERA( 282),IX( 282),IY( 282)/'DRAW',  -3,  -8/
      DATA IOPERA( 283),IX( 283),IY( 283)/'DRAW',  -2,  -9/
      DATA IOPERA( 284),IX( 284),IY( 284)/'DRAW',   1,  -9/
      DATA IOPERA( 285),IX( 285),IY( 285)/'DRAW',   3,  -7/
      DATA IOPERA( 286),IX( 286),IY( 286)/'DRAW',   4,  -5/
      DATA IOPERA( 287),IX( 287),IY( 287)/'MOVE',   1,   5/
      DATA IOPERA( 288),IX( 288),IY( 288)/'DRAW',  -1,  -2/
      DATA IOPERA( 289),IX( 289),IY( 289)/'DRAW',  -2,  -6/
      DATA IOPERA( 290),IX( 290),IY( 290)/'DRAW',  -2,  -8/
      DATA IOPERA( 291),IX( 291),IY( 291)/'DRAW',  -1,  -9/
!
      DATA IXMIND(   9)/  -6/
      DATA IXMAXD(   9)/   6/
      DATA IXDELD(   9)/  12/
      DATA ISTARD(   9)/ 279/
      DATA NUMCOO(   9)/  13/
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
   51 FORMAT('***** AT THE BEGINNING OF DGCL1--')
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
 9011 FORMAT('***** AT THE END       OF DGCL1--')
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
      END SUBROUTINE DGCL1
      SUBROUTINE DGCL2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR GREEK COMPLEX LOWER CASE (PART 2).
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-921-3651
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
!     DEFINE CHARACTER   2136--LOWER CASE KAPP
!
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',  -4,   5/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',  -8,  -9/
      DATA IOPERA(   3),IX(   3),IY(   3)/'MOVE',  -3,   5/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',  -7,  -9/
      DATA IOPERA(   5),IX(   5),IY(   5)/'MOVE',   6,   5/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',   7,   4/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',   8,   4/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',   7,   5/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',   5,   5/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',   3,   4/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',  -1,   0/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',  -3,  -1/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',  -5,  -1/
      DATA IOPERA(  14),IX(  14),IY(  14)/'MOVE',  -3,  -1/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',  -1,  -2/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',   1,  -8/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',   2,  -9/
      DATA IOPERA(  18),IX(  18),IY(  18)/'MOVE',  -3,  -1/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',  -2,  -2/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',   0,  -8/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',   1,  -9/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',   3,  -9/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',   5,  -8/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',   7,  -5/
!
      DATA IXMIND(  10)/ -10/
      DATA IXMAXD(  10)/  10/
      DATA IXDELD(  10)/  20/
      DATA ISTARD(  10)/   1/
      DATA NUMCOO(  10)/  24/
!
!     DEFINE CHARACTER   2137--LOWER CASE LAMB
!
      DATA IOPERA(  25),IX(  25),IY(  25)/'MOVE',  -7,  12/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',  -5,  12/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',  -3,  11/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',  -2,  10/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',  -1,   8/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',   5,  -6/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',   6,  -8/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',   7,  -9/
      DATA IOPERA(  33),IX(  33),IY(  33)/'MOVE',  -5,  12/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',  -3,  10/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',  -2,   8/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',   4,  -6/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',   5,  -8/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',   7,  -9/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',   8,  -9/
      DATA IOPERA(  40),IX(  40),IY(  40)/'MOVE',   0,   5/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',  -8,  -9/
      DATA IOPERA(  42),IX(  42),IY(  42)/'MOVE',   0,   5/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',  -7,  -9/
!
      DATA IXMIND(  11)/ -10/
      DATA IXMAXD(  11)/  10/
      DATA IXDELD(  11)/  20/
      DATA ISTARD(  11)/  25/
      DATA NUMCOO(  11)/  19/
!
!     DEFINE CHARACTER   2138--LOWER CASE MU
!
      DATA IOPERA(  44),IX(  44),IY(  44)/'MOVE',  -5,   5/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW', -11, -16/
      DATA IOPERA(  46),IX(  46),IY(  46)/'MOVE',  -4,   5/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW', -10, -16/
      DATA IOPERA(  48),IX(  48),IY(  48)/'MOVE',  -5,   2/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',  -6,  -4/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW',  -6,  -7/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW',  -4,  -9/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',  -2,  -9/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',   0,  -8/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',   2,  -6/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',   4,  -3/
      DATA IOPERA(  56),IX(  56),IY(  56)/'MOVE',   6,   5/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',   3,  -6/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',   3,  -8/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',   4,  -9/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW',   7,  -9/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',   9,  -7/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',  10,  -5/
      DATA IOPERA(  63),IX(  63),IY(  63)/'MOVE',   7,   5/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',   4,  -6/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',   4,  -8/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW',   5,  -9/
!
      DATA IXMIND(  12)/ -12/
      DATA IXMAXD(  12)/  11/
      DATA IXDELD(  12)/  23/
      DATA ISTARD(  12)/  44/
      DATA NUMCOO(  12)/  23/
!
!     DEFINE CHARACTER   2139--LOWER CASE NU
!
      DATA IOPERA(  67),IX(  67),IY(  67)/'MOVE',  -4,   5/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',  -6,  -9/
      DATA IOPERA(  69),IX(  69),IY(  69)/'MOVE',  -3,   5/
      DATA IOPERA(  70),IX(  70),IY(  70)/'DRAW',  -4,  -1/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',  -5,  -6/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',  -6,  -9/
      DATA IOPERA(  73),IX(  73),IY(  73)/'MOVE',   7,   5/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',   6,   1/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',   4,  -3/
      DATA IOPERA(  76),IX(  76),IY(  76)/'MOVE',   8,   5/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',   7,   2/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',   6,   0/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',   4,  -3/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',   2,  -5/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',  -1,  -7/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',  -3,  -8/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',  -6,  -9/
      DATA IOPERA(  84),IX(  84),IY(  84)/'MOVE',  -7,   5/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',  -3,   5/
!
      DATA IXMIND(  13)/ -10/
      DATA IXMAXD(  13)/  10/
      DATA IXDELD(  13)/  20/
      DATA ISTARD(  13)/  67/
      DATA NUMCOO(  13)/  19/
!
!     DEFINE CHARACTER   2140--LOWER CASE XI
!
      DATA IOPERA(  86),IX(  86),IY(  86)/'MOVE',   2,  12/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',   0,  11/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',  -1,  10/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',  -1,   9/
      DATA IOPERA(  90),IX(  90),IY(  90)/'DRAW',   0,   8/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',   3,   7/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW',   6,   7/
      DATA IOPERA(  93),IX(  93),IY(  93)/'MOVE',   3,   7/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',  -1,   6/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',  -3,   5/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',  -4,   3/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',  -4,   1/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',  -2,  -1/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',   1,  -2/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',   4,  -2/
      DATA IOPERA( 101),IX( 101),IY( 101)/'MOVE',   3,   7/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',   0,   6/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',  -2,   5/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',  -3,   3/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',  -3,   1/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',  -1,  -1/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',   1,  -2/
      DATA IOPERA( 108),IX( 108),IY( 108)/'MOVE',   1,  -2/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',  -3,  -3/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',  -5,  -4/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',  -6,  -6/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',  -6,  -8/
      DATA IOPERA( 113),IX( 113),IY( 113)/'DRAW',  -4, -10/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',   1, -12/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',   2, -13/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',   2, -15/
      DATA IOPERA( 117),IX( 117),IY( 117)/'DRAW',   0, -16/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',  -2, -16/
      DATA IOPERA( 119),IX( 119),IY( 119)/'MOVE',   1,  -2/
      DATA IOPERA( 120),IX( 120),IY( 120)/'DRAW',  -2,  -3/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW',  -4,  -4/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW',  -5,  -6/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',  -5,  -8/
      DATA IOPERA( 124),IX( 124),IY( 124)/'DRAW',  -3, -10/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',   1, -12/
!
      DATA IXMIND(  14)/  -9/
      DATA IXMAXD(  14)/   8/
      DATA IXDELD(  14)/  17/
      DATA ISTARD(  14)/  86/
      DATA NUMCOO(  14)/  40/
!
!     DEFINE CHARACTER   2141--LOWER CASE OMIC
!
      DATA IOPERA( 126),IX( 126),IY( 126)/'MOVE',   0,   5/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW',  -3,   4/
      DATA IOPERA( 128),IX( 128),IY( 128)/'DRAW',  -5,   1/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',  -6,  -2/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',  -6,  -5/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',  -5,  -7/
      DATA IOPERA( 132),IX( 132),IY( 132)/'DRAW',  -4,  -8/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',  -2,  -9/
      DATA IOPERA( 134),IX( 134),IY( 134)/'DRAW',   0,  -9/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',   3,  -8/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',   5,  -5/
      DATA IOPERA( 137),IX( 137),IY( 137)/'DRAW',   6,  -2/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW',   6,   1/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW',   5,   3/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW',   4,   4/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',   2,   5/
      DATA IOPERA( 142),IX( 142),IY( 142)/'DRAW',   0,   5/
      DATA IOPERA( 143),IX( 143),IY( 143)/'MOVE',   0,   5/
      DATA IOPERA( 144),IX( 144),IY( 144)/'DRAW',  -2,   4/
      DATA IOPERA( 145),IX( 145),IY( 145)/'DRAW',  -4,   1/
      DATA IOPERA( 146),IX( 146),IY( 146)/'DRAW',  -5,  -2/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',  -5,  -6/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',  -4,  -8/
      DATA IOPERA( 149),IX( 149),IY( 149)/'MOVE',   0,  -9/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',   2,  -8/
      DATA IOPERA( 151),IX( 151),IY( 151)/'DRAW',   4,  -5/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',   5,  -2/
      DATA IOPERA( 153),IX( 153),IY( 153)/'DRAW',   5,   2/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',   4,   4/
!
      DATA IXMIND(  15)/  -9/
      DATA IXMAXD(  15)/   9/
      DATA IXDELD(  15)/  18/
      DATA ISTARD(  15)/ 126/
      DATA NUMCOO(  15)/  29/
!
!     DEFINE CHARACTER   2142--LOWER CASE PI
!
      DATA IOPERA( 155),IX( 155),IY( 155)/'MOVE',  -2,   4/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',  -6,  -9/
      DATA IOPERA( 157),IX( 157),IY( 157)/'MOVE',  -2,   4/
      DATA IOPERA( 158),IX( 158),IY( 158)/'DRAW',  -5,  -9/
      DATA IOPERA( 159),IX( 159),IY( 159)/'MOVE',   4,   4/
      DATA IOPERA( 160),IX( 160),IY( 160)/'DRAW',   4,  -9/
      DATA IOPERA( 161),IX( 161),IY( 161)/'MOVE',   4,   4/
      DATA IOPERA( 162),IX( 162),IY( 162)/'DRAW',   5,  -9/
      DATA IOPERA( 163),IX( 163),IY( 163)/'MOVE',  -9,   2/
      DATA IOPERA( 164),IX( 164),IY( 164)/'DRAW',  -7,   4/
      DATA IOPERA( 165),IX( 165),IY( 165)/'DRAW',  -4,   5/
      DATA IOPERA( 166),IX( 166),IY( 166)/'DRAW',   9,   5/
      DATA IOPERA( 167),IX( 167),IY( 167)/'MOVE',  -9,   2/
      DATA IOPERA( 168),IX( 168),IY( 168)/'DRAW',  -7,   3/
      DATA IOPERA( 169),IX( 169),IY( 169)/'DRAW',  -4,   4/
      DATA IOPERA( 170),IX( 170),IY( 170)/'DRAW',   9,   4/
!
      DATA IXMIND(  16)/ -11/
      DATA IXMAXD(  16)/  11/
      DATA IXDELD(  16)/  22/
      DATA ISTARD(  16)/ 155/
      DATA NUMCOO(  16)/  16/
!
!     DEFINE CHARACTER   2143--LOWER CASE RHO
!
      DATA IOPERA( 171),IX( 171),IY( 171)/'MOVE',  -6,  -4/
      DATA IOPERA( 172),IX( 172),IY( 172)/'DRAW',  -5,  -7/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW',  -4,  -8/
      DATA IOPERA( 174),IX( 174),IY( 174)/'DRAW',  -2,  -9/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW',   0,  -9/
      DATA IOPERA( 176),IX( 176),IY( 176)/'DRAW',   3,  -8/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW',   5,  -5/
      DATA IOPERA( 178),IX( 178),IY( 178)/'DRAW',   6,  -2/
      DATA IOPERA( 179),IX( 179),IY( 179)/'DRAW',   6,   1/
      DATA IOPERA( 180),IX( 180),IY( 180)/'DRAW',   5,   3/
      DATA IOPERA( 181),IX( 181),IY( 181)/'DRAW',   4,   4/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',   2,   5/
      DATA IOPERA( 183),IX( 183),IY( 183)/'DRAW',   0,   5/
      DATA IOPERA( 184),IX( 184),IY( 184)/'DRAW',  -3,   4/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',  -5,   1/
      DATA IOPERA( 186),IX( 186),IY( 186)/'DRAW',  -6,  -2/
      DATA IOPERA( 187),IX( 187),IY( 187)/'DRAW', -10, -16/
      DATA IOPERA( 188),IX( 188),IY( 188)/'MOVE',   0,  -9/
      DATA IOPERA( 189),IX( 189),IY( 189)/'DRAW',   2,  -8/
      DATA IOPERA( 190),IX( 190),IY( 190)/'DRAW',   4,  -5/
      DATA IOPERA( 191),IX( 191),IY( 191)/'DRAW',   5,  -2/
      DATA IOPERA( 192),IX( 192),IY( 192)/'DRAW',   5,   2/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW',   4,   4/
      DATA IOPERA( 194),IX( 194),IY( 194)/'MOVE',   0,   5/
      DATA IOPERA( 195),IX( 195),IY( 195)/'DRAW',  -2,   4/
      DATA IOPERA( 196),IX( 196),IY( 196)/'DRAW',  -4,   1/
      DATA IOPERA( 197),IX( 197),IY( 197)/'DRAW',  -5,  -2/
      DATA IOPERA( 198),IX( 198),IY( 198)/'DRAW',  -9, -16/
!
      DATA IXMIND(  17)/ -10/
      DATA IXMAXD(  17)/   9/
      DATA IXDELD(  17)/  19/
      DATA ISTARD(  17)/ 171/
      DATA NUMCOO(  17)/  28/
!
!     DEFINE CHARACTER   2144--LOWER CASE SIGM
!
      DATA IOPERA( 199),IX( 199),IY( 199)/'MOVE',   9,   5/
      DATA IOPERA( 200),IX( 200),IY( 200)/'DRAW',  -1,   5/
      DATA IOPERA( 201),IX( 201),IY( 201)/'DRAW',  -4,   4/
      DATA IOPERA( 202),IX( 202),IY( 202)/'DRAW',  -6,   1/
      DATA IOPERA( 203),IX( 203),IY( 203)/'DRAW',  -7,  -2/
      DATA IOPERA( 204),IX( 204),IY( 204)/'DRAW',  -7,  -5/
      DATA IOPERA( 205),IX( 205),IY( 205)/'DRAW',  -6,  -7/
      DATA IOPERA( 206),IX( 206),IY( 206)/'DRAW',  -5,  -8/
      DATA IOPERA( 207),IX( 207),IY( 207)/'DRAW',  -3,  -9/
      DATA IOPERA( 208),IX( 208),IY( 208)/'DRAW',  -1,  -9/
      DATA IOPERA( 209),IX( 209),IY( 209)/'DRAW',   2,  -8/
      DATA IOPERA( 210),IX( 210),IY( 210)/'DRAW',   4,  -5/
      DATA IOPERA( 211),IX( 211),IY( 211)/'DRAW',   5,  -2/
      DATA IOPERA( 212),IX( 212),IY( 212)/'DRAW',   5,   1/
      DATA IOPERA( 213),IX( 213),IY( 213)/'DRAW',   4,   3/
      DATA IOPERA( 214),IX( 214),IY( 214)/'DRAW',   3,   4/
      DATA IOPERA( 215),IX( 215),IY( 215)/'DRAW',   1,   5/
      DATA IOPERA( 216),IX( 216),IY( 216)/'MOVE',  -1,   5/
      DATA IOPERA( 217),IX( 217),IY( 217)/'DRAW',  -3,   4/
      DATA IOPERA( 218),IX( 218),IY( 218)/'DRAW',  -5,   1/
      DATA IOPERA( 219),IX( 219),IY( 219)/'DRAW',  -6,  -2/
      DATA IOPERA( 220),IX( 220),IY( 220)/'DRAW',  -6,  -6/
      DATA IOPERA( 221),IX( 221),IY( 221)/'DRAW',  -5,  -8/
      DATA IOPERA( 222),IX( 222),IY( 222)/'MOVE',  -1,  -9/
      DATA IOPERA( 223),IX( 223),IY( 223)/'DRAW',   1,  -8/
      DATA IOPERA( 224),IX( 224),IY( 224)/'DRAW',   3,  -5/
      DATA IOPERA( 225),IX( 225),IY( 225)/'DRAW',   4,  -2/
      DATA IOPERA( 226),IX( 226),IY( 226)/'DRAW',   4,   2/
      DATA IOPERA( 227),IX( 227),IY( 227)/'DRAW',   3,   4/
      DATA IOPERA( 228),IX( 228),IY( 228)/'MOVE',   3,   4/
      DATA IOPERA( 229),IX( 229),IY( 229)/'DRAW',   9,   4/
!
      DATA IXMIND(  18)/ -10/
      DATA IXMAXD(  18)/  11/
      DATA IXDELD(  18)/  21/
      DATA ISTARD(  18)/ 199/
      DATA NUMCOO(  18)/  31/
!
!     DEFINE CHARACTER   2145--LOWER CASE TAU
!
      DATA IOPERA( 230),IX( 230),IY( 230)/'MOVE',   1,   4/
      DATA IOPERA( 231),IX( 231),IY( 231)/'DRAW',  -2,  -9/
      DATA IOPERA( 232),IX( 232),IY( 232)/'MOVE',   1,   4/
      DATA IOPERA( 233),IX( 233),IY( 233)/'DRAW',  -1,  -9/
      DATA IOPERA( 234),IX( 234),IY( 234)/'MOVE',  -8,   2/
      DATA IOPERA( 235),IX( 235),IY( 235)/'DRAW',  -6,   4/
      DATA IOPERA( 236),IX( 236),IY( 236)/'DRAW',  -3,   5/
      DATA IOPERA( 237),IX( 237),IY( 237)/'DRAW',   8,   5/
      DATA IOPERA( 238),IX( 238),IY( 238)/'MOVE',  -8,   2/
      DATA IOPERA( 239),IX( 239),IY( 239)/'DRAW',  -6,   3/
      DATA IOPERA( 240),IX( 240),IY( 240)/'DRAW',  -3,   4/
      DATA IOPERA( 241),IX( 241),IY( 241)/'DRAW',   8,   4/
!
      DATA IXMIND(  19)/ -10/
      DATA IXMAXD(  19)/  10/
      DATA IXDELD(  19)/  20/
      DATA ISTARD(  19)/ 230/
      DATA NUMCOO(  19)/  12/
!
!     DEFINE CHARACTER   2146--LOWER CASE UPSI
!
      DATA IOPERA( 242),IX( 242),IY( 242)/'MOVE',  -9,   1/
      DATA IOPERA( 243),IX( 243),IY( 243)/'DRAW',  -8,   3/
      DATA IOPERA( 244),IX( 244),IY( 244)/'DRAW',  -6,   5/
      DATA IOPERA( 245),IX( 245),IY( 245)/'DRAW',  -3,   5/
      DATA IOPERA( 246),IX( 246),IY( 246)/'DRAW',  -2,   4/
      DATA IOPERA( 247),IX( 247),IY( 247)/'DRAW',  -2,   2/
      DATA IOPERA( 248),IX( 248),IY( 248)/'DRAW',  -4,  -4/
      DATA IOPERA( 249),IX( 249),IY( 249)/'DRAW',  -4,  -7/
      DATA IOPERA( 250),IX( 250),IY( 250)/'DRAW',  -2,  -9/
      DATA IOPERA( 251),IX( 251),IY( 251)/'MOVE',  -4,   5/
      DATA IOPERA( 252),IX( 252),IY( 252)/'DRAW',  -3,   4/
      DATA IOPERA( 253),IX( 253),IY( 253)/'DRAW',  -3,   2/
      DATA IOPERA( 254),IX( 254),IY( 254)/'DRAW',  -5,  -4/
      DATA IOPERA( 255),IX( 255),IY( 255)/'DRAW',  -5,  -7/
      DATA IOPERA( 256),IX( 256),IY( 256)/'DRAW',  -4,  -8/
      DATA IOPERA( 257),IX( 257),IY( 257)/'DRAW',  -2,  -9/
      DATA IOPERA( 258),IX( 258),IY( 258)/'DRAW',  -1,  -9/
      DATA IOPERA( 259),IX( 259),IY( 259)/'DRAW',   2,  -8/
      DATA IOPERA( 260),IX( 260),IY( 260)/'DRAW',   4,  -6/
      DATA IOPERA( 261),IX( 261),IY( 261)/'DRAW',   6,  -3/
      DATA IOPERA( 262),IX( 262),IY( 262)/'DRAW',   7,   0/
      DATA IOPERA( 263),IX( 263),IY( 263)/'DRAW',   7,   3/
      DATA IOPERA( 264),IX( 264),IY( 264)/'DRAW',   6,   5/
      DATA IOPERA( 265),IX( 265),IY( 265)/'DRAW',   5,   4/
      DATA IOPERA( 266),IX( 266),IY( 266)/'DRAW',   6,   3/
      DATA IOPERA( 267),IX( 267),IY( 267)/'DRAW',   7,   0/
      DATA IOPERA( 268),IX( 268),IY( 268)/'MOVE',   6,  -3/
      DATA IOPERA( 269),IX( 269),IY( 269)/'DRAW',   7,   3/
!
      DATA IXMIND(  20)/ -10/
      DATA IXMAXD(  20)/  10/
      DATA IXDELD(  20)/  20/
      DATA ISTARD(  20)/ 242/
      DATA NUMCOO(  20)/  28/
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
   51 FORMAT('***** AT THE BEGINNING OF DGCL2--')
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
 9011 FORMAT('***** AT THE END       OF DGCL2--')
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
      END SUBROUTINE DGCL2
      SUBROUTINE DGCL3(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR GREEK COMPLEX LOWER CASE (PART 3).
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-921-3651
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
!     DEFINE CHARACTER   2147--LOWER CASE PHI
!
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',  -3,   4/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',  -5,   3/
      DATA IOPERA(   3),IX(   3),IY(   3)/'DRAW',  -7,   1/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',  -8,  -2/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',  -8,  -5/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',  -7,  -7/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',  -6,  -8/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',  -4,  -9/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',  -1,  -9/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',   2,  -8/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',   5,  -6/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',   7,  -3/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',   8,   0/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',   8,   3/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',   6,   5/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',   4,   5/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',   2,   3/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',   0,  -1/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',  -2,  -6/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',  -5, -16/
      DATA IOPERA(  21),IX(  21),IY(  21)/'MOVE',  -8,  -5/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',  -6,  -7/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',  -4,  -8/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',  -1,  -8/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',   2,  -7/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',   5,  -5/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',   7,  -3/
      DATA IOPERA(  28),IX(  28),IY(  28)/'MOVE',   8,   3/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',   6,   4/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',   4,   4/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',   2,   2/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',   0,  -1/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',  -2,  -7/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',  -4, -16/
!
      DATA IXMIND(  21)/ -11/
      DATA IXMAXD(  21)/  11/
      DATA IXDELD(  21)/  22/
      DATA ISTARD(  21)/   1/
      DATA NUMCOO(  21)/  34/
!
!     DEFINE CHARACTER   2148--LOWER CASE CHI
!
      DATA IOPERA(  35),IX(  35),IY(  35)/'MOVE',  -7,   5/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',  -5,   5/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',  -3,   4/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',  -2,   2/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',   3, -13/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',   4, -15/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',   5, -16/
      DATA IOPERA(  42),IX(  42),IY(  42)/'MOVE',  -5,   5/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',  -4,   4/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',  -3,   2/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',   2, -13/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW',   3, -15/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',   5, -16/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',   7, -16/
      DATA IOPERA(  49),IX(  49),IY(  49)/'MOVE',   8,   5/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW',   7,   3/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW',   5,   0/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',  -5, -11/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',  -7, -14/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',  -8, -16/
!
      DATA IXMIND(  22)/  -9/
      DATA IXMAXD(  22)/   9/
      DATA IXDELD(  22)/  18/
      DATA ISTARD(  22)/  35/
      DATA NUMCOO(  22)/  20/
!
!     DEFINE CHARACTER   2149--LOWER CASE PSI
!
      DATA IOPERA(  55),IX(  55),IY(  55)/'MOVE',   3,  12/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',  -3, -16/
      DATA IOPERA(  57),IX(  57),IY(  57)/'MOVE',   4,  12/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',  -4, -16/
      DATA IOPERA(  59),IX(  59),IY(  59)/'MOVE', -11,   1/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW', -10,   3/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',  -8,   5/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',  -5,   5/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',  -4,   4/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',  -4,   2/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',  -5,  -3/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW',  -5,  -6/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',  -3,  -8/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',   0,  -8/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',   2,  -7/
      DATA IOPERA(  70),IX(  70),IY(  70)/'DRAW',   5,  -4/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',   7,  -1/
      DATA IOPERA(  72),IX(  72),IY(  72)/'MOVE',  -6,   5/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',  -5,   4/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',  -5,   2/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',  -6,  -3/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',  -6,  -6/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',  -5,  -8/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',  -3,  -9/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',   0,  -9/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',   2,  -8/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',   4,  -6/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',   6,  -3/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',   7,  -1/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW',   9,   5/
!
      DATA IXMIND(  23)/ -12/
      DATA IXMAXD(  23)/  11/
      DATA IXDELD(  23)/  23/
      DATA ISTARD(  23)/  55/
      DATA NUMCOO(  23)/  30/
!
!     DEFINE CHARACTER   2150--LOWER CASE OMEG
!
      DATA IOPERA(  85),IX(  85),IY(  85)/'MOVE',  -8,   1/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',  -6,   3/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',  -3,   4/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',  -4,   5/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',  -6,   4/
      DATA IOPERA(  90),IX(  90),IY(  90)/'DRAW',  -8,   1/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',  -9,  -2/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW',  -9,  -5/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',  -8,  -8/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',  -7,  -9/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',  -5,  -9/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',  -3,  -8/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',  -1,  -5/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',   0,  -2/
      DATA IOPERA(  99),IX(  99),IY(  99)/'MOVE',  -9,  -5/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',  -8,  -7/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',  -7,  -8/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',  -5,  -8/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',  -3,  -7/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',  -1,  -5/
      DATA IOPERA( 105),IX( 105),IY( 105)/'MOVE',  -1,  -2/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',  -1,  -5/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',   0,  -8/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',   1,  -9/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',   3,  -9/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',   5,  -8/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',   7,  -5/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',   8,  -2/
      DATA IOPERA( 113),IX( 113),IY( 113)/'DRAW',   8,   1/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',   7,   4/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',   6,   5/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',   5,   4/
      DATA IOPERA( 117),IX( 117),IY( 117)/'DRAW',   7,   3/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',   8,   1/
      DATA IOPERA( 119),IX( 119),IY( 119)/'MOVE',  -1,  -5/
      DATA IOPERA( 120),IX( 120),IY( 120)/'DRAW',   0,  -7/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW',   1,  -8/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW',   3,  -8/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',   5,  -7/
      DATA IOPERA( 124),IX( 124),IY( 124)/'DRAW',   7,  -5/
!
      DATA IXMIND(  24)/ -12/
      DATA IXMAXD(  24)/  11/
      DATA IXDELD(  24)/  23/
      DATA ISTARD(  24)/  85/
      DATA NUMCOO(  24)/  40/
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
   51 FORMAT('***** AT THE BEGINNING OF DGCL3--')
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
 9011 FORMAT('***** AT THE END       OF DGCL3--')
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
      END SUBROUTINE DGCL3
      SUBROUTINE DGCU1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR GREEK COMPLEX UPPER CASE (PART 1).
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-921-3651
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
!     DEFINE CHARACTER   2027--UPPER CASE ALPH
!
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',   0,  12/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',  -7,  -9/
      DATA IOPERA(   3),IX(   3),IY(   3)/'MOVE',   0,  12/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',   7,  -9/
      DATA IOPERA(   5),IX(   5),IY(   5)/'MOVE',   0,   9/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',   6,  -9/
      DATA IOPERA(   7),IX(   7),IY(   7)/'MOVE',  -5,  -3/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',   4,  -3/
      DATA IOPERA(   9),IX(   9),IY(   9)/'MOVE',  -9,  -9/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',  -3,  -9/
      DATA IOPERA(  11),IX(  11),IY(  11)/'MOVE',   3,  -9/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',   9,  -9/
!
      DATA IXMIND(   1)/ -10/
      DATA IXMAXD(   1)/  10/
      DATA IXDELD(   1)/  20/
      DATA ISTARD(   1)/   1/
      DATA NUMCOO(   1)/  12/
!
!     DEFINE CHARACTER   2028--UPPER CASE BETA
!
      DATA IOPERA(  13),IX(  13),IY(  13)/'MOVE',  -6,  12/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',  -6,  -9/
      DATA IOPERA(  15),IX(  15),IY(  15)/'MOVE',  -5,  12/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',  -5,  -9/
      DATA IOPERA(  17),IX(  17),IY(  17)/'MOVE',  -9,  12/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',   3,  12/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',   6,  11/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',   7,  10/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',   8,   8/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',   8,   6/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',   7,   4/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',   6,   3/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',   3,   2/
      DATA IOPERA(  26),IX(  26),IY(  26)/'MOVE',   3,  12/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',   5,  11/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',   6,  10/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',   7,   8/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',   7,   6/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',   6,   4/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',   5,   3/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',   3,   2/
      DATA IOPERA(  34),IX(  34),IY(  34)/'MOVE',  -5,   2/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',   3,   2/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',   6,   1/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',   7,   0/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',   8,  -2/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',   8,  -5/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',   7,  -7/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',   6,  -8/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',   3,  -9/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',  -9,  -9/
      DATA IOPERA(  44),IX(  44),IY(  44)/'MOVE',   3,   2/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',   5,   1/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW',   6,   0/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',   7,  -2/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',   7,  -5/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',   6,  -7/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW',   5,  -8/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW',   3,  -9/
!
      DATA IXMIND(   2)/ -11/
      DATA IXMAXD(   2)/  11/
      DATA IXDELD(   2)/  22/
      DATA ISTARD(   2)/  13/
      DATA NUMCOO(   2)/  39/
!
!     DEFINE CHARACTER   2029--UPPER CASE GAMM
!
      DATA IOPERA(  52),IX(  52),IY(  52)/'MOVE',  -4,  12/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',  -4,  -9/
      DATA IOPERA(  54),IX(  54),IY(  54)/'MOVE',  -3,  12/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',  -3,  -9/
      DATA IOPERA(  56),IX(  56),IY(  56)/'MOVE',  -7,  12/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',   8,  12/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',   8,   6/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',   7,  12/
      DATA IOPERA(  60),IX(  60),IY(  60)/'MOVE',  -7,  -9/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',   0,  -9/
!
      DATA IXMIND(   3)/  -9/
      DATA IXMAXD(   3)/   9/
      DATA IXDELD(   3)/  18/
      DATA ISTARD(   3)/  52/
      DATA NUMCOO(   3)/  10/
!
!     DEFINE CHARACTER   2030--UPPER CASE DELT
!
      DATA IOPERA(  62),IX(  62),IY(  62)/'MOVE',   0,  12/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',  -8,  -9/
      DATA IOPERA(  64),IX(  64),IY(  64)/'MOVE',   0,  12/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',   8,  -9/
      DATA IOPERA(  66),IX(  66),IY(  66)/'MOVE',   0,   9/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',   7,  -9/
      DATA IOPERA(  68),IX(  68),IY(  68)/'MOVE',  -7,  -8/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',   7,  -8/
      DATA IOPERA(  70),IX(  70),IY(  70)/'MOVE',  -8,  -9/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',   8,  -9/
!
      DATA IXMIND(   4)/ -10/
      DATA IXMAXD(   4)/  10/
      DATA IXDELD(   4)/  20/
      DATA ISTARD(   4)/  62/
      DATA NUMCOO(   4)/  10/
!
!     DEFINE CHARACTER   2031--UPPER CASE EPSI
!
      DATA IOPERA(  72),IX(  72),IY(  72)/'MOVE',  -6,  12/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',  -6,  -9/
      DATA IOPERA(  74),IX(  74),IY(  74)/'MOVE',  -5,  12/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',  -5,  -9/
      DATA IOPERA(  76),IX(  76),IY(  76)/'MOVE',   1,   6/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',   1,  -2/
      DATA IOPERA(  78),IX(  78),IY(  78)/'MOVE',  -9,  12/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',   7,  12/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',   7,   6/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',   6,  12/
      DATA IOPERA(  82),IX(  82),IY(  82)/'MOVE',  -5,   2/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',   1,   2/
      DATA IOPERA(  84),IX(  84),IY(  84)/'MOVE',  -9,  -9/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',   7,  -9/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',   7,  -3/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',   6,  -9/
!
      DATA IXMIND(   5)/ -11/
      DATA IXMAXD(   5)/  10/
      DATA IXDELD(   5)/  21/
      DATA ISTARD(   5)/  72/
      DATA NUMCOO(   5)/  16/
!
!     DEFINE CHARACTER   2032--UPPER CASE ZETA
!
      DATA IOPERA(  88),IX(  88),IY(  88)/'MOVE',   6,  12/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',  -7,  -9/
      DATA IOPERA(  90),IX(  90),IY(  90)/'MOVE',   7,  12/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',  -6,  -9/
      DATA IOPERA(  92),IX(  92),IY(  92)/'MOVE',  -6,  12/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',  -7,   6/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',  -7,  12/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',   7,  12/
      DATA IOPERA(  96),IX(  96),IY(  96)/'MOVE',  -7,  -9/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',   7,  -9/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',   7,  -3/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',   6,  -9/
!
      DATA IXMIND(   6)/ -10/
      DATA IXMAXD(   6)/  10/
      DATA IXDELD(   6)/  20/
      DATA ISTARD(   6)/  88/
      DATA NUMCOO(   6)/  12/
!
!     DEFINE CHARACTER   2033--UPPER CASE ETA
!
      DATA IOPERA( 100),IX( 100),IY( 100)/'MOVE',  -7,  12/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',  -7,  -9/
      DATA IOPERA( 102),IX( 102),IY( 102)/'MOVE',  -6,  12/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',  -6,  -9/
      DATA IOPERA( 104),IX( 104),IY( 104)/'MOVE',   6,  12/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',   6,  -9/
      DATA IOPERA( 106),IX( 106),IY( 106)/'MOVE',   7,  12/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',   7,  -9/
      DATA IOPERA( 108),IX( 108),IY( 108)/'MOVE', -10,  12/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',  -3,  12/
      DATA IOPERA( 110),IX( 110),IY( 110)/'MOVE',   3,  12/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',  10,  12/
      DATA IOPERA( 112),IX( 112),IY( 112)/'MOVE',  -6,   2/
      DATA IOPERA( 113),IX( 113),IY( 113)/'DRAW',   6,   2/
      DATA IOPERA( 114),IX( 114),IY( 114)/'MOVE', -10,  -9/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',  -3,  -9/
      DATA IOPERA( 116),IX( 116),IY( 116)/'MOVE',   3,  -9/
      DATA IOPERA( 117),IX( 117),IY( 117)/'DRAW',  10,  -9/
!
      DATA IXMIND(   7)/ -12/
      DATA IXMAXD(   7)/  12/
      DATA IXDELD(   7)/  24/
      DATA ISTARD(   7)/ 100/
      DATA NUMCOO(   7)/  18/
!
!     DEFINE CHARACTER   2034--UPPER CASE THET
!
      DATA IOPERA( 118),IX( 118),IY( 118)/'MOVE',  -1,  12/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',  -4,  11/
      DATA IOPERA( 120),IX( 120),IY( 120)/'DRAW',  -6,   9/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW',  -7,   7/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW',  -8,   3/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',  -8,   0/
      DATA IOPERA( 124),IX( 124),IY( 124)/'DRAW',  -7,  -4/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',  -6,  -6/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW',  -4,  -8/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW',  -1,  -9/
      DATA IOPERA( 128),IX( 128),IY( 128)/'DRAW',   1,  -9/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',   4,  -8/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',   6,  -6/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',   7,  -4/
      DATA IOPERA( 132),IX( 132),IY( 132)/'DRAW',   8,   0/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',   8,   3/
      DATA IOPERA( 134),IX( 134),IY( 134)/'DRAW',   7,   7/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',   6,   9/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',   4,  11/
      DATA IOPERA( 137),IX( 137),IY( 137)/'DRAW',   1,  12/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW',  -1,  12/
      DATA IOPERA( 139),IX( 139),IY( 139)/'MOVE',  -1,  12/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW',  -3,  11/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',  -5,   9/
      DATA IOPERA( 142),IX( 142),IY( 142)/'DRAW',  -6,   7/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',  -7,   3/
      DATA IOPERA( 144),IX( 144),IY( 144)/'DRAW',  -7,   0/
      DATA IOPERA( 145),IX( 145),IY( 145)/'DRAW',  -6,  -4/
      DATA IOPERA( 146),IX( 146),IY( 146)/'DRAW',  -5,  -6/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',  -3,  -8/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',  -1,  -9/
      DATA IOPERA( 149),IX( 149),IY( 149)/'MOVE',   1,  -9/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',   3,  -8/
      DATA IOPERA( 151),IX( 151),IY( 151)/'DRAW',   5,  -6/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',   6,  -4/
      DATA IOPERA( 153),IX( 153),IY( 153)/'DRAW',   7,   0/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',   7,   3/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW',   6,   7/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',   5,   9/
      DATA IOPERA( 157),IX( 157),IY( 157)/'DRAW',   3,  11/
      DATA IOPERA( 158),IX( 158),IY( 158)/'DRAW',   1,  12/
      DATA IOPERA( 159),IX( 159),IY( 159)/'MOVE',  -3,   5/
      DATA IOPERA( 160),IX( 160),IY( 160)/'DRAW',  -3,  -2/
      DATA IOPERA( 161),IX( 161),IY( 161)/'MOVE',   3,   5/
      DATA IOPERA( 162),IX( 162),IY( 162)/'DRAW',   3,  -2/
      DATA IOPERA( 163),IX( 163),IY( 163)/'MOVE',  -3,   2/
      DATA IOPERA( 164),IX( 164),IY( 164)/'DRAW',   3,   2/
      DATA IOPERA( 165),IX( 165),IY( 165)/'MOVE',  -3,   1/
      DATA IOPERA( 166),IX( 166),IY( 166)/'DRAW',   3,   1/
!
      DATA IXMIND(   8)/ -11/
      DATA IXMAXD(   8)/  11/
      DATA IXDELD(   8)/  22/
      DATA ISTARD(   8)/ 118/
      DATA NUMCOO(   8)/  49/
!
!     DEFINE CHARACTER   2035--UPPER CASE IOTA
!
      DATA IOPERA( 167),IX( 167),IY( 167)/'MOVE',   0,  12/
      DATA IOPERA( 168),IX( 168),IY( 168)/'DRAW',   0,  -9/
      DATA IOPERA( 169),IX( 169),IY( 169)/'MOVE',   1,  12/
      DATA IOPERA( 170),IX( 170),IY( 170)/'DRAW',   1,  -9/
      DATA IOPERA( 171),IX( 171),IY( 171)/'MOVE',  -3,  12/
      DATA IOPERA( 172),IX( 172),IY( 172)/'DRAW',   4,  12/
      DATA IOPERA( 173),IX( 173),IY( 173)/'MOVE',  -3,  -9/
      DATA IOPERA( 174),IX( 174),IY( 174)/'DRAW',   4,  -9/
!
      DATA IXMIND(   9)/  -5/
      DATA IXMAXD(   9)/   6/
      DATA IXDELD(   9)/  11/
      DATA ISTARD(   9)/ 167/
      DATA NUMCOO(   9)/   8/
!
!     DEFINE CHARACTER   2036--UPPER CASE KAPP
!
      DATA IOPERA( 175),IX( 175),IY( 175)/'MOVE',  -7,  12/
      DATA IOPERA( 176),IX( 176),IY( 176)/'DRAW',  -7,  -9/
      DATA IOPERA( 177),IX( 177),IY( 177)/'MOVE',  -6,  12/
      DATA IOPERA( 178),IX( 178),IY( 178)/'DRAW',  -6,  -9/
      DATA IOPERA( 179),IX( 179),IY( 179)/'MOVE',   7,  12/
      DATA IOPERA( 180),IX( 180),IY( 180)/'DRAW',  -6,  -1/
      DATA IOPERA( 181),IX( 181),IY( 181)/'MOVE',  -1,   3/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',   7,  -9/
      DATA IOPERA( 183),IX( 183),IY( 183)/'MOVE',  -2,   3/
      DATA IOPERA( 184),IX( 184),IY( 184)/'DRAW',   6,  -9/
      DATA IOPERA( 185),IX( 185),IY( 185)/'MOVE', -10,  12/
      DATA IOPERA( 186),IX( 186),IY( 186)/'DRAW',  -3,  12/
      DATA IOPERA( 187),IX( 187),IY( 187)/'MOVE',   3,  12/
      DATA IOPERA( 188),IX( 188),IY( 188)/'DRAW',   9,  12/
      DATA IOPERA( 189),IX( 189),IY( 189)/'MOVE', -10,  -9/
      DATA IOPERA( 190),IX( 190),IY( 190)/'DRAW',  -3,  -9/
      DATA IOPERA( 191),IX( 191),IY( 191)/'MOVE',   3,  -9/
      DATA IOPERA( 192),IX( 192),IY( 192)/'DRAW',   9,  -9/
!
      DATA IXMIND(  10)/ -12/
      DATA IXMAXD(  10)/  10/
      DATA IXDELD(  10)/  22/
      DATA ISTARD(  10)/ 175/
      DATA NUMCOO(  10)/  18/
!
!     DEFINE CHARACTER   2037--UPPER CASE LAMB
!
      DATA IOPERA( 193),IX( 193),IY( 193)/'MOVE',   0,  12/
      DATA IOPERA( 194),IX( 194),IY( 194)/'DRAW',  -7,  -9/
      DATA IOPERA( 195),IX( 195),IY( 195)/'MOVE',   0,  12/
      DATA IOPERA( 196),IX( 196),IY( 196)/'DRAW',   7,  -9/
      DATA IOPERA( 197),IX( 197),IY( 197)/'MOVE',   0,   9/
      DATA IOPERA( 198),IX( 198),IY( 198)/'DRAW',   6,  -9/
      DATA IOPERA( 199),IX( 199),IY( 199)/'MOVE',  -9,  -9/
      DATA IOPERA( 200),IX( 200),IY( 200)/'DRAW',  -3,  -9/
      DATA IOPERA( 201),IX( 201),IY( 201)/'MOVE',   3,  -9/
      DATA IOPERA( 202),IX( 202),IY( 202)/'DRAW',   9,  -9/
!
      DATA IXMIND(  11)/ -10/
      DATA IXMAXD(  11)/  10/
      DATA IXDELD(  11)/  20/
      DATA ISTARD(  11)/ 193/
      DATA NUMCOO(  11)/  10/
!
!     DEFINE CHARACTER   2038--UPPER CASE MU
!
      DATA IOPERA( 203),IX( 203),IY( 203)/'MOVE',  -7,  12/
      DATA IOPERA( 204),IX( 204),IY( 204)/'DRAW',  -7,  -9/
      DATA IOPERA( 205),IX( 205),IY( 205)/'MOVE',  -6,  12/
      DATA IOPERA( 206),IX( 206),IY( 206)/'DRAW',   0,  -6/
      DATA IOPERA( 207),IX( 207),IY( 207)/'MOVE',  -7,  12/
      DATA IOPERA( 208),IX( 208),IY( 208)/'DRAW',   0,  -9/
      DATA IOPERA( 209),IX( 209),IY( 209)/'MOVE',   7,  12/
      DATA IOPERA( 210),IX( 210),IY( 210)/'DRAW',   0,  -9/
      DATA IOPERA( 211),IX( 211),IY( 211)/'MOVE',   7,  12/
      DATA IOPERA( 212),IX( 212),IY( 212)/'DRAW',   7,  -9/
      DATA IOPERA( 213),IX( 213),IY( 213)/'MOVE',   8,  12/
      DATA IOPERA( 214),IX( 214),IY( 214)/'DRAW',   8,  -9/
      DATA IOPERA( 215),IX( 215),IY( 215)/'MOVE', -10,  12/
      DATA IOPERA( 216),IX( 216),IY( 216)/'DRAW',  -6,  12/
      DATA IOPERA( 217),IX( 217),IY( 217)/'MOVE',   7,  12/
      DATA IOPERA( 218),IX( 218),IY( 218)/'DRAW',  11,  12/
      DATA IOPERA( 219),IX( 219),IY( 219)/'MOVE', -10,  -9/
      DATA IOPERA( 220),IX( 220),IY( 220)/'DRAW',  -4,  -9/
      DATA IOPERA( 221),IX( 221),IY( 221)/'MOVE',   4,  -9/
      DATA IOPERA( 222),IX( 222),IY( 222)/'DRAW',  11,  -9/
!
      DATA IXMIND(  12)/ -12/
      DATA IXMAXD(  12)/  13/
      DATA IXDELD(  12)/  25/
      DATA ISTARD(  12)/ 203/
      DATA NUMCOO(  12)/  20/
!
!     DEFINE CHARACTER   2039--UPPER CASE NU
!
      DATA IOPERA( 223),IX( 223),IY( 223)/'MOVE',  -6,  12/
      DATA IOPERA( 224),IX( 224),IY( 224)/'DRAW',  -6,  -9/
      DATA IOPERA( 225),IX( 225),IY( 225)/'MOVE',  -5,  12/
      DATA IOPERA( 226),IX( 226),IY( 226)/'DRAW',   7,  -7/
      DATA IOPERA( 227),IX( 227),IY( 227)/'MOVE',  -5,  10/
      DATA IOPERA( 228),IX( 228),IY( 228)/'DRAW',   7,  -9/
      DATA IOPERA( 229),IX( 229),IY( 229)/'MOVE',   7,  12/
      DATA IOPERA( 230),IX( 230),IY( 230)/'DRAW',   7,  -9/
      DATA IOPERA( 231),IX( 231),IY( 231)/'MOVE',  -9,  12/
      DATA IOPERA( 232),IX( 232),IY( 232)/'DRAW',  -5,  12/
      DATA IOPERA( 233),IX( 233),IY( 233)/'MOVE',   4,  12/
      DATA IOPERA( 234),IX( 234),IY( 234)/'DRAW',  10,  12/
      DATA IOPERA( 235),IX( 235),IY( 235)/'MOVE',  -9,  -9/
      DATA IOPERA( 236),IX( 236),IY( 236)/'DRAW',  -3,  -9/
!
      DATA IXMIND(  13)/ -11/
      DATA IXMAXD(  13)/  12/
      DATA IXDELD(  13)/  23/
      DATA ISTARD(  13)/ 223/
      DATA NUMCOO(  13)/  14/
!
!     DEFINE CHARACTER   2040--UPPER CASE XI
!
      DATA IOPERA( 237),IX( 237),IY( 237)/'MOVE',  -7,  13/
      DATA IOPERA( 238),IX( 238),IY( 238)/'DRAW',  -8,   8/
      DATA IOPERA( 239),IX( 239),IY( 239)/'MOVE',   8,  13/
      DATA IOPERA( 240),IX( 240),IY( 240)/'DRAW',   7,   8/
      DATA IOPERA( 241),IX( 241),IY( 241)/'MOVE',  -3,   4/
      DATA IOPERA( 242),IX( 242),IY( 242)/'DRAW',  -4,  -1/
      DATA IOPERA( 243),IX( 243),IY( 243)/'MOVE',   4,   4/
      DATA IOPERA( 244),IX( 244),IY( 244)/'DRAW',   3,  -1/
      DATA IOPERA( 245),IX( 245),IY( 245)/'MOVE',  -7,  -5/
      DATA IOPERA( 246),IX( 246),IY( 246)/'DRAW',  -8, -10/
      DATA IOPERA( 247),IX( 247),IY( 247)/'MOVE',   8,  -5/
      DATA IOPERA( 248),IX( 248),IY( 248)/'DRAW',   7, -10/
      DATA IOPERA( 249),IX( 249),IY( 249)/'MOVE',  -7,  11/
      DATA IOPERA( 250),IX( 250),IY( 250)/'DRAW',   7,  11/
      DATA IOPERA( 251),IX( 251),IY( 251)/'MOVE',  -7,  10/
      DATA IOPERA( 252),IX( 252),IY( 252)/'DRAW',   7,  10/
      DATA IOPERA( 253),IX( 253),IY( 253)/'MOVE',  -3,   2/
      DATA IOPERA( 254),IX( 254),IY( 254)/'DRAW',   3,   2/
      DATA IOPERA( 255),IX( 255),IY( 255)/'MOVE',  -3,   1/
      DATA IOPERA( 256),IX( 256),IY( 256)/'DRAW',   3,   1/
      DATA IOPERA( 257),IX( 257),IY( 257)/'MOVE',  -7,  -7/
      DATA IOPERA( 258),IX( 258),IY( 258)/'DRAW',   7,  -7/
      DATA IOPERA( 259),IX( 259),IY( 259)/'MOVE',  -7,  -8/
      DATA IOPERA( 260),IX( 260),IY( 260)/'DRAW',   7,  -8/
!
      DATA IXMIND(  14)/ -11/
      DATA IXMAXD(  14)/  11/
      DATA IXDELD(  14)/  22/
      DATA ISTARD(  14)/ 237/
      DATA NUMCOO(  14)/  24/
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
   51 FORMAT('***** AT THE BEGINNING OF DGCU1--')
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
 9011 FORMAT('***** AT THE END       OF DGCU1--')
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
      END SUBROUTINE DGCU1
      SUBROUTINE DGCU2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR GREEK COMPLEX UPPER CASE (PART 2).
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-921-3651
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
!     DEFINE CHARACTER   2041--UPPER CASE OMIC
!
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',  -1,  12/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',  -4,  11/
      DATA IOPERA(   3),IX(   3),IY(   3)/'DRAW',  -6,   9/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',  -7,   7/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',  -8,   3/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',  -8,   0/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',  -7,  -4/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',  -6,  -6/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',  -4,  -8/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',  -1,  -9/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',   1,  -9/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',   4,  -8/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',   6,  -6/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',   7,  -4/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',   8,   0/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',   8,   3/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',   7,   7/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',   6,   9/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',   4,  11/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',   1,  12/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',  -1,  12/
      DATA IOPERA(  22),IX(  22),IY(  22)/'MOVE',  -1,  12/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',  -3,  11/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',  -5,   9/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',  -6,   7/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',  -7,   3/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',  -7,   0/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',  -6,  -4/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',  -5,  -6/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',  -3,  -8/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',  -1,  -9/
      DATA IOPERA(  32),IX(  32),IY(  32)/'MOVE',   1,  -9/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',   3,  -8/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',   5,  -6/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',   6,  -4/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',   7,   0/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',   7,   3/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',   6,   7/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',   5,   9/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',   3,  11/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',   1,  12/
!
      DATA IXMIND(  15)/ -11/
      DATA IXMAXD(  15)/  11/
      DATA IXDELD(  15)/  22/
      DATA ISTARD(  15)/   1/
      DATA NUMCOO(  15)/  41/
!
!     DEFINE CHARACTER   2042--UPPER CASE PI
!
      DATA IOPERA(  42),IX(  42),IY(  42)/'MOVE',  -7,  12/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',  -7,  -9/
      DATA IOPERA(  44),IX(  44),IY(  44)/'MOVE',  -6,  12/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',  -6,  -9/
      DATA IOPERA(  46),IX(  46),IY(  46)/'MOVE',   6,  12/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',   6,  -9/
      DATA IOPERA(  48),IX(  48),IY(  48)/'MOVE',   7,  12/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',   7,  -9/
      DATA IOPERA(  50),IX(  50),IY(  50)/'MOVE', -10,  12/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW',  10,  12/
      DATA IOPERA(  52),IX(  52),IY(  52)/'MOVE', -10,  -9/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',  -3,  -9/
      DATA IOPERA(  54),IX(  54),IY(  54)/'MOVE',   3,  -9/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',  10,  -9/
!
      DATA IXMIND(  16)/ -12/
      DATA IXMAXD(  16)/  12/
      DATA IXDELD(  16)/  24/
      DATA ISTARD(  16)/  42/
      DATA NUMCOO(  16)/  14/
!
!     DEFINE CHARACTER   2043--UPPER CASE RHO
!
      DATA IOPERA(  56),IX(  56),IY(  56)/'MOVE',  -6,  12/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',  -6,  -9/
      DATA IOPERA(  58),IX(  58),IY(  58)/'MOVE',  -5,  12/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',  -5,  -9/
      DATA IOPERA(  60),IX(  60),IY(  60)/'MOVE',  -9,  12/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',   3,  12/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',   6,  11/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',   7,  10/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',   8,   8/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',   8,   5/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW',   7,   3/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',   6,   2/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',   3,   1/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',  -5,   1/
      DATA IOPERA(  70),IX(  70),IY(  70)/'MOVE',   3,  12/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',   5,  11/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',   6,  10/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',   7,   8/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',   7,   5/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',   6,   3/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',   5,   2/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',   3,   1/
      DATA IOPERA(  78),IX(  78),IY(  78)/'MOVE',  -9,  -9/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',  -2,  -9/
!
      DATA IXMIND(  17)/ -11/
      DATA IXMAXD(  17)/  11/
      DATA IXDELD(  17)/  22/
      DATA ISTARD(  17)/  56/
      DATA NUMCOO(  17)/  24/
!
!     DEFINE CHARACTER   2044--UPPER CASE SIGM
!
      DATA IOPERA(  80),IX(  80),IY(  80)/'MOVE',  -7,  12/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',   0,   2/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',  -8,  -9/
      DATA IOPERA(  83),IX(  83),IY(  83)/'MOVE',  -8,  12/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW',  -1,   2/
      DATA IOPERA(  85),IX(  85),IY(  85)/'MOVE',  -8,  12/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',   7,  12/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',   8,   6/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',   6,  12/
      DATA IOPERA(  89),IX(  89),IY(  89)/'MOVE',  -7,  -8/
      DATA IOPERA(  90),IX(  90),IY(  90)/'DRAW',   6,  -8/
      DATA IOPERA(  91),IX(  91),IY(  91)/'MOVE',  -8,  -9/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW',   7,  -9/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',   8,  -3/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',   6,  -9/
!
      DATA IXMIND(  18)/ -10/
      DATA IXMAXD(  18)/  11/
      DATA IXDELD(  18)/  21/
      DATA ISTARD(  18)/  80/
      DATA NUMCOO(  18)/  15/
!
!     DEFINE CHARACTER   2045--UPPER CASE TAU
!
      DATA IOPERA(  95),IX(  95),IY(  95)/'MOVE',   0,  12/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',   0,  -9/
      DATA IOPERA(  97),IX(  97),IY(  97)/'MOVE',   1,  12/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',   1,  -9/
      DATA IOPERA(  99),IX(  99),IY(  99)/'MOVE',  -6,  12/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',  -7,   6/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',  -7,  12/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',   8,  12/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',   8,   6/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',   7,  12/
      DATA IOPERA( 105),IX( 105),IY( 105)/'MOVE',  -3,  -9/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',   4,  -9/
!
      DATA IXMIND(  19)/  -9/
      DATA IXMAXD(  19)/  10/
      DATA IXDELD(  19)/  19/
      DATA ISTARD(  19)/  95/
      DATA NUMCOO(  19)/  12/
!
!     DEFINE CHARACTER   2046--UPPER CASE UPSI
!
      DATA IOPERA( 107),IX( 107),IY( 107)/'MOVE',  -7,   7/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',  -7,   9/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',  -6,  11/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',  -5,  12/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',  -3,  12/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',  -2,  11/
      DATA IOPERA( 113),IX( 113),IY( 113)/'DRAW',  -1,   9/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',   0,   5/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',   0,  -9/
      DATA IOPERA( 116),IX( 116),IY( 116)/'MOVE',  -7,   9/
      DATA IOPERA( 117),IX( 117),IY( 117)/'DRAW',  -5,  11/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',  -3,  11/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',  -1,   9/
      DATA IOPERA( 120),IX( 120),IY( 120)/'MOVE',   8,   7/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW',   8,   9/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW',   7,  11/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',   6,  12/
      DATA IOPERA( 124),IX( 124),IY( 124)/'DRAW',   4,  12/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',   3,  11/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW',   2,   9/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW',   1,   5/
      DATA IOPERA( 128),IX( 128),IY( 128)/'DRAW',   1,  -9/
      DATA IOPERA( 129),IX( 129),IY( 129)/'MOVE',   8,   9/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',   6,  11/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',   4,  11/
      DATA IOPERA( 132),IX( 132),IY( 132)/'DRAW',   2,   9/
      DATA IOPERA( 133),IX( 133),IY( 133)/'MOVE',  -3,  -9/
      DATA IOPERA( 134),IX( 134),IY( 134)/'DRAW',   4,  -9/
!
      DATA IXMIND(  20)/  -9/
      DATA IXMAXD(  20)/  10/
      DATA IXDELD(  20)/  19/
      DATA ISTARD(  20)/ 107/
      DATA NUMCOO(  20)/  28/
!
!     DEFINE CHARACTER   2047--UPPER CASE PHI
!
      DATA IOPERA( 135),IX( 135),IY( 135)/'MOVE',   0,  12/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',   0,  -9/
      DATA IOPERA( 137),IX( 137),IY( 137)/'MOVE',   1,  12/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW',   1,  -9/
      DATA IOPERA( 139),IX( 139),IY( 139)/'MOVE',  -2,   7/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW',  -5,   6/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',  -6,   5/
      DATA IOPERA( 142),IX( 142),IY( 142)/'DRAW',  -7,   3/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',  -7,   0/
      DATA IOPERA( 144),IX( 144),IY( 144)/'DRAW',  -6,  -2/
      DATA IOPERA( 145),IX( 145),IY( 145)/'DRAW',  -5,  -3/
      DATA IOPERA( 146),IX( 146),IY( 146)/'DRAW',  -2,  -4/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',   3,  -4/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',   6,  -3/
      DATA IOPERA( 149),IX( 149),IY( 149)/'DRAW',   7,  -2/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',   8,   0/
      DATA IOPERA( 151),IX( 151),IY( 151)/'DRAW',   8,   3/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',   7,   5/
      DATA IOPERA( 153),IX( 153),IY( 153)/'DRAW',   6,   6/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',   3,   7/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW',  -2,   7/
      DATA IOPERA( 156),IX( 156),IY( 156)/'MOVE',  -2,   7/
      DATA IOPERA( 157),IX( 157),IY( 157)/'DRAW',  -4,   6/
      DATA IOPERA( 158),IX( 158),IY( 158)/'DRAW',  -5,   5/
      DATA IOPERA( 159),IX( 159),IY( 159)/'DRAW',  -6,   3/
      DATA IOPERA( 160),IX( 160),IY( 160)/'DRAW',  -6,   0/
      DATA IOPERA( 161),IX( 161),IY( 161)/'DRAW',  -5,  -2/
      DATA IOPERA( 162),IX( 162),IY( 162)/'DRAW',  -4,  -3/
      DATA IOPERA( 163),IX( 163),IY( 163)/'DRAW',  -2,  -4/
      DATA IOPERA( 164),IX( 164),IY( 164)/'MOVE',   3,  -4/
      DATA IOPERA( 165),IX( 165),IY( 165)/'DRAW',   5,  -3/
      DATA IOPERA( 166),IX( 166),IY( 166)/'DRAW',   6,  -2/
      DATA IOPERA( 167),IX( 167),IY( 167)/'DRAW',   7,   0/
      DATA IOPERA( 168),IX( 168),IY( 168)/'DRAW',   7,   3/
      DATA IOPERA( 169),IX( 169),IY( 169)/'DRAW',   6,   5/
      DATA IOPERA( 170),IX( 170),IY( 170)/'DRAW',   5,   6/
      DATA IOPERA( 171),IX( 171),IY( 171)/'DRAW',   3,   7/
      DATA IOPERA( 172),IX( 172),IY( 172)/'MOVE',  -3,  12/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW',   4,  12/
      DATA IOPERA( 174),IX( 174),IY( 174)/'MOVE',  -3,  -9/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW',   4,  -9/
!
      DATA IXMIND(  21)/ -10/
      DATA IXMAXD(  21)/  11/
      DATA IXDELD(  21)/  21/
      DATA ISTARD(  21)/ 135/
      DATA NUMCOO(  21)/  41/
!
!     DEFINE CHARACTER   2048--UPPER CASE CHI
!
      DATA IOPERA( 176),IX( 176),IY( 176)/'MOVE',  -7,  12/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW',   6,  -9/
      DATA IOPERA( 178),IX( 178),IY( 178)/'MOVE',  -6,  12/
      DATA IOPERA( 179),IX( 179),IY( 179)/'DRAW',   7,  -9/
      DATA IOPERA( 180),IX( 180),IY( 180)/'MOVE',   7,  12/
      DATA IOPERA( 181),IX( 181),IY( 181)/'DRAW',  -7,  -9/
      DATA IOPERA( 182),IX( 182),IY( 182)/'MOVE',  -9,  12/
      DATA IOPERA( 183),IX( 183),IY( 183)/'DRAW',  -3,  12/
      DATA IOPERA( 184),IX( 184),IY( 184)/'MOVE',   3,  12/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',   9,  12/
      DATA IOPERA( 186),IX( 186),IY( 186)/'MOVE',  -9,  -9/
      DATA IOPERA( 187),IX( 187),IY( 187)/'DRAW',  -3,  -9/
      DATA IOPERA( 188),IX( 188),IY( 188)/'MOVE',   3,  -9/
      DATA IOPERA( 189),IX( 189),IY( 189)/'DRAW',   9,  -9/
!
      DATA IXMIND(  22)/ -10/
      DATA IXMAXD(  22)/  10/
      DATA IXDELD(  22)/  20/
      DATA ISTARD(  22)/ 176/
      DATA NUMCOO(  22)/  14/
!
!     DEFINE CHARACTER   2049--UPPER CASE PSI
!
      DATA IOPERA( 190),IX( 190),IY( 190)/'MOVE',   0,  12/
      DATA IOPERA( 191),IX( 191),IY( 191)/'DRAW',   0,  -9/
      DATA IOPERA( 192),IX( 192),IY( 192)/'MOVE',   1,  12/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW',   1,  -9/
      DATA IOPERA( 194),IX( 194),IY( 194)/'MOVE',  -9,   5/
      DATA IOPERA( 195),IX( 195),IY( 195)/'DRAW',  -8,   6/
      DATA IOPERA( 196),IX( 196),IY( 196)/'DRAW',  -6,   5/
      DATA IOPERA( 197),IX( 197),IY( 197)/'DRAW',  -5,   1/
      DATA IOPERA( 198),IX( 198),IY( 198)/'DRAW',  -4,  -1/
      DATA IOPERA( 199),IX( 199),IY( 199)/'DRAW',  -3,  -2/
      DATA IOPERA( 200),IX( 200),IY( 200)/'DRAW',  -1,  -3/
      DATA IOPERA( 201),IX( 201),IY( 201)/'MOVE',  -8,   6/
      DATA IOPERA( 202),IX( 202),IY( 202)/'DRAW',  -7,   5/
      DATA IOPERA( 203),IX( 203),IY( 203)/'DRAW',  -6,   1/
      DATA IOPERA( 204),IX( 204),IY( 204)/'DRAW',  -5,  -1/
      DATA IOPERA( 205),IX( 205),IY( 205)/'DRAW',  -4,  -2/
      DATA IOPERA( 206),IX( 206),IY( 206)/'DRAW',  -1,  -3/
      DATA IOPERA( 207),IX( 207),IY( 207)/'DRAW',   2,  -3/
      DATA IOPERA( 208),IX( 208),IY( 208)/'DRAW',   5,  -2/
      DATA IOPERA( 209),IX( 209),IY( 209)/'DRAW',   6,  -1/
      DATA IOPERA( 210),IX( 210),IY( 210)/'DRAW',   7,   1/
      DATA IOPERA( 211),IX( 211),IY( 211)/'DRAW',   8,   5/
      DATA IOPERA( 212),IX( 212),IY( 212)/'DRAW',   9,   6/
      DATA IOPERA( 213),IX( 213),IY( 213)/'MOVE',   2,  -3/
      DATA IOPERA( 214),IX( 214),IY( 214)/'DRAW',   4,  -2/
      DATA IOPERA( 215),IX( 215),IY( 215)/'DRAW',   5,  -1/
      DATA IOPERA( 216),IX( 216),IY( 216)/'DRAW',   6,   1/
      DATA IOPERA( 217),IX( 217),IY( 217)/'DRAW',   7,   5/
      DATA IOPERA( 218),IX( 218),IY( 218)/'DRAW',   9,   6/
      DATA IOPERA( 219),IX( 219),IY( 219)/'DRAW',  10,   5/
      DATA IOPERA( 220),IX( 220),IY( 220)/'MOVE',  -3,  12/
      DATA IOPERA( 221),IX( 221),IY( 221)/'DRAW',   4,  12/
      DATA IOPERA( 222),IX( 222),IY( 222)/'MOVE',  -3,  -9/
      DATA IOPERA( 223),IX( 223),IY( 223)/'DRAW',   4,  -9/
!
      DATA IXMIND(  23)/ -11/
      DATA IXMAXD(  23)/  12/
      DATA IXDELD(  23)/  23/
      DATA ISTARD(  23)/ 190/
      DATA NUMCOO(  23)/  34/
!
!     DEFINE CHARACTER   2050--UPPER CASE OMEG
!
      DATA IOPERA( 224),IX( 224),IY( 224)/'MOVE',  -8,  -6/
      DATA IOPERA( 225),IX( 225),IY( 225)/'DRAW',  -7,  -9/
      DATA IOPERA( 226),IX( 226),IY( 226)/'DRAW',  -3,  -9/
      DATA IOPERA( 227),IX( 227),IY( 227)/'DRAW',  -5,  -5/
      DATA IOPERA( 228),IX( 228),IY( 228)/'DRAW',  -7,  -1/
      DATA IOPERA( 229),IX( 229),IY( 229)/'DRAW',  -8,   2/
      DATA IOPERA( 230),IX( 230),IY( 230)/'DRAW',  -8,   6/
      DATA IOPERA( 231),IX( 231),IY( 231)/'DRAW',  -7,   9/
      DATA IOPERA( 232),IX( 232),IY( 232)/'DRAW',  -5,  11/
      DATA IOPERA( 233),IX( 233),IY( 233)/'DRAW',  -2,  12/
      DATA IOPERA( 234),IX( 234),IY( 234)/'DRAW',   2,  12/
      DATA IOPERA( 235),IX( 235),IY( 235)/'DRAW',   5,  11/
      DATA IOPERA( 236),IX( 236),IY( 236)/'DRAW',   7,   9/
      DATA IOPERA( 237),IX( 237),IY( 237)/'DRAW',   8,   6/
      DATA IOPERA( 238),IX( 238),IY( 238)/'DRAW',   8,   2/
      DATA IOPERA( 239),IX( 239),IY( 239)/'DRAW',   7,  -1/
      DATA IOPERA( 240),IX( 240),IY( 240)/'DRAW',   5,  -5/
      DATA IOPERA( 241),IX( 241),IY( 241)/'DRAW',   3,  -9/
      DATA IOPERA( 242),IX( 242),IY( 242)/'DRAW',   7,  -9/
      DATA IOPERA( 243),IX( 243),IY( 243)/'DRAW',   8,  -6/
      DATA IOPERA( 244),IX( 244),IY( 244)/'MOVE',  -5,  -5/
      DATA IOPERA( 245),IX( 245),IY( 245)/'DRAW',  -6,  -2/
      DATA IOPERA( 246),IX( 246),IY( 246)/'DRAW',  -7,   2/
      DATA IOPERA( 247),IX( 247),IY( 247)/'DRAW',  -7,   6/
      DATA IOPERA( 248),IX( 248),IY( 248)/'DRAW',  -6,   9/
      DATA IOPERA( 249),IX( 249),IY( 249)/'DRAW',  -4,  11/
      DATA IOPERA( 250),IX( 250),IY( 250)/'DRAW',  -2,  12/
      DATA IOPERA( 251),IX( 251),IY( 251)/'MOVE',   2,  12/
      DATA IOPERA( 252),IX( 252),IY( 252)/'DRAW',   4,  11/
      DATA IOPERA( 253),IX( 253),IY( 253)/'DRAW',   6,   9/
      DATA IOPERA( 254),IX( 254),IY( 254)/'DRAW',   7,   6/
      DATA IOPERA( 255),IX( 255),IY( 255)/'DRAW',   7,   2/
      DATA IOPERA( 256),IX( 256),IY( 256)/'DRAW',   6,  -2/
      DATA IOPERA( 257),IX( 257),IY( 257)/'DRAW',   5,  -5/
      DATA IOPERA( 258),IX( 258),IY( 258)/'MOVE',  -7,  -8/
      DATA IOPERA( 259),IX( 259),IY( 259)/'DRAW',  -4,  -8/
      DATA IOPERA( 260),IX( 260),IY( 260)/'MOVE',   4,  -8/
      DATA IOPERA( 261),IX( 261),IY( 261)/'DRAW',   7,  -8/
!
      DATA IXMIND(  24)/ -11/
      DATA IXMAXD(  24)/  11/
      DATA IXDELD(  24)/  22/
      DATA ISTARD(  24)/ 224/
      DATA NUMCOO(  24)/  38/
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
   51 FORMAT('***** AT THE BEGINNING OF DGCU2--')
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
 9011 FORMAT('***** AT THE END       OF DGCU2--')
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
      END SUBROUTINE DGCU2
      SUBROUTINE DGECO(A,LDA,N,IPVT,RCOND,Z)
!***BEGIN PROLOGUE  DGECO
!***DATE WRITTEN   780814   (YYMMDD)
!***REVISION DATE  820801   (YYMMDD)
!***CATEGORY NO.  D2A1
!***KEYWORDS  CONDITION,DOUBLE PRECISION,FACTOR,LINEAR ALGEBRA,LINPACK,
!             MATRIX
!***AUTHOR  MOLER, C. B., (U. OF NEW MEXICO)
!***PURPOSE  Factors a double precision matrix by Gaussian elimination
!            and estimates the condition of the matrix.
!***DESCRIPTION
!
!     DGECO factors a double precision matrix by Gaussian elimination
!     and estimates the condition of the matrix.
!
!     If  RCOND  is not needed, DGEFA is slightly faster.
!     To solve  A*X = B , follow DGECO by DGESL.
!     To compute  INVERSE(A)*C , follow DGECO by DGESL.
!     To compute  DETERMINANT(A) , follow DGECO by DGEDI.
!     To compute  INVERSE(A) , follow DGECO by DGEDI.
!
!     On Entry
!
!        A       DOUBLE PRECISION(LDA, N)
!                the matrix to be factored.
!
!        LDA     INTEGER
!                the leading dimension of the array  A .
!
!        N       INTEGER
!                the order of the matrix  A .
!
!     On Return
!
!        A       an upper triangular matrix and the multipliers
!                which were used to obtain it.
!                The factorization can be written  A = L*U  where
!                L  is a product of permutation and unit lower
!                triangular matrices and  U  is upper triangular.
!
!        IPVT    INTEGER(N)
!                an INTEGER vector of pivot indices.
!
!        RCOND   DOUBLE PRECISION
!                an estimate of the reciprocal condition of  A .
!                For the system  A*X = B , relative perturbations
!                in  A  and  B  of size  EPSILON  may cause
!                relative perturbations in  X  of size  EPSILON/RCOND .
!                If  RCOND  is so small that the logical expression
!                           1.0 + RCOND .EQ. 1.0
!                is true, then  A  may be singular to working
!                precision.  In particular,  RCOND  is zero  if
!                exact singularity is detected or the estimate
!                underflows.
!
!        Z       DOUBLE PRECISION(N)
!                a work vector whose contents are usually unimportant.
!                If  A  is close to a singular matrix, then  Z  is
!                an approximate null vector in the sense that
!                NORM(A*Z) = RCOND*NORM(A)*NORM(Z) .
!
!     LINPACK.  This version dated 08/14/78 .
!     Cleve Moler, University of New Mexico, Argonne National Lab.
!
!     Subroutines and Functions
!
!     LINPACK DGEFA
!     BLAS DAXPY,DDOT,DSCAL,DASUM
!     Fortran DABS,DMAX1,DSIGN
!***REFERENCES  DONGARRA J.J., BUNCH J.R., MOLER C.B., STEWART G.W.,
!                 *LINPACK USERS  GUIDE*, SIAM, 1979.
!***ROUTINES CALLED  DASUM,DAXPY,DDOT,DGEFA,DSCAL
!***END PROLOGUE  DGECO
      INTEGER LDA,N,IPVT(1)
      DOUBLE PRECISION A(LDA,1),Z(1)
      DOUBLE PRECISION RCOND
!
      DOUBLE PRECISION DDOT,EK,T,WK,WKM
      DOUBLE PRECISION ANORM,S,DASUM,SM,YNORM
      INTEGER INFO,J,K,KB,KP1,L
!
!     COMPUTE 1-NORM OF A
!
!***FIRST EXECUTABLE STATEMENT  DGECO
      ANORM = 0.0D0
      DO 10 J = 1, N
         ANORM = DMAX1(ANORM,DASUM(N,A(1,J),1))
   10 CONTINUE
!
!     FACTOR
!
      CALL DGEFA(A,LDA,N,IPVT,INFO)
!
!     RCOND = 1/(NORM(A)*(ESTIMATE OF NORM(INVERSE(A)))) .
!     ESTIMATE = NORM(Z)/NORM(Y) WHERE  A*Z = Y  AND  TRANS(A)*Y = E .
!     TRANS(A)  IS THE TRANSPOSE OF A .  THE COMPONENTS OF  E  ARE
!     CHOSEN TO CAUSE MAXIMUM LOCAL GROWTH IN THE ELEMENTS OF W  WHERE
!     TRANS(U)*W = E .  THE VECTORS ARE FREQUENTLY RESCALED TO AVOID
!     OVERFLOW.
!
!     SOLVE TRANS(U)*W = E
!
      EK = 1.0D0
      DO 20 J = 1, N
         Z(J) = 0.0D0
   20 CONTINUE
      DO 100 K = 1, N
         IF (Z(K) .NE. 0.0D0) EK = DSIGN(EK,-Z(K))
         IF (DABS(EK-Z(K)) .LE. DABS(A(K,K))) GO TO 30
            S = DABS(A(K,K))/DABS(EK-Z(K))
            CALL DSCAL(N,S,Z,1)
            EK = S*EK
   30    CONTINUE
         WK = EK - Z(K)
         WKM = -EK - Z(K)
         S = DABS(WK)
         SM = DABS(WKM)
         IF (A(K,K) .EQ. 0.0D0) GO TO 40
            WK = WK/A(K,K)
            WKM = WKM/A(K,K)
         GO TO 50
   40    CONTINUE
            WK = 1.0D0
            WKM = 1.0D0
   50    CONTINUE
         KP1 = K + 1
         IF (KP1 .GT. N) GO TO 90
            DO 60 J = KP1, N
               SM = SM + DABS(Z(J)+WKM*A(K,J))
               Z(J) = Z(J) + WK*A(K,J)
               S = S + DABS(Z(J))
   60       CONTINUE
            IF (S .GE. SM) GO TO 80
               T = WKM - WK
               WK = WKM
               DO 70 J = KP1, N
                  Z(J) = Z(J) + T*A(K,J)
   70          CONTINUE
   80       CONTINUE
   90    CONTINUE
         Z(K) = WK
  100 CONTINUE
      S = 1.0D0/DASUM(N,Z,1)
      CALL DSCAL(N,S,Z,1)
!
!     SOLVE TRANS(L)*Y = W
!
      DO 120 KB = 1, N
         K = N + 1 - KB
         IF (K .LT. N) Z(K) = Z(K) + DDOT(N-K,A(K+1,K),1,Z(K+1),1)
         IF (DABS(Z(K)) .LE. 1.0D0) GO TO 110
            S = 1.0D0/DABS(Z(K))
            CALL DSCAL(N,S,Z,1)
  110    CONTINUE
         L = IPVT(K)
         T = Z(L)
         Z(L) = Z(K)
         Z(K) = T
  120 CONTINUE
      S = 1.0D0/DASUM(N,Z,1)
      CALL DSCAL(N,S,Z,1)
!
      YNORM = 1.0D0
!
!     SOLVE L*V = Y
!
      DO 140 K = 1, N
         L = IPVT(K)
         T = Z(L)
         Z(L) = Z(K)
         Z(K) = T
         IF (K .LT. N) CALL DAXPY(N-K,T,A(K+1,K),1,Z(K+1),1)
         IF (DABS(Z(K)) .LE. 1.0D0) GO TO 130
            S = 1.0D0/DABS(Z(K))
            CALL DSCAL(N,S,Z,1)
            YNORM = S*YNORM
  130    CONTINUE
  140 CONTINUE
      S = 1.0D0/DASUM(N,Z,1)
      CALL DSCAL(N,S,Z,1)
      YNORM = S*YNORM
!
!     SOLVE  U*Z = V
!
      DO 160 KB = 1, N
         K = N + 1 - KB
         IF (DABS(Z(K)) .LE. DABS(A(K,K))) GO TO 150
            S = DABS(A(K,K))/DABS(Z(K))
            CALL DSCAL(N,S,Z,1)
            YNORM = S*YNORM
  150    CONTINUE
         IF (A(K,K) .NE. 0.0D0) Z(K) = Z(K)/A(K,K)
         IF (A(K,K) .EQ. 0.0D0) Z(K) = 1.0D0
         T = -Z(K)
         CALL DAXPY(K-1,T,A(1,K),1,Z(1),1)
  160 CONTINUE
!     MAKE ZNORM = 1.0
      S = 1.0D0/DASUM(N,Z,1)
      CALL DSCAL(N,S,Z,1)
      YNORM = S*YNORM
!
      IF (ANORM .NE. 0.0D0) RCOND = YNORM/ANORM
      IF (ANORM .EQ. 0.0D0) RCOND = 0.0D0
      RETURN
      END SUBROUTINE DGECO
      SUBROUTINE DGEFA(A,LDA,N,IPVT,INFO)
!***BEGIN PROLOGUE  DGEFA
!***DATE WRITTEN   780814   (YYMMDD)
!***REVISION DATE  820801   (YYMMDD)
!***CATEGORY NO.  D2A1
!***KEYWORDS  DOUBLE PRECISION,FACTOR,LINEAR ALGEBRA,LINPACK,MATRIX
!***AUTHOR  MOLER, C. B., (U. OF NEW MEXICO)
!***PURPOSE  Factors a double precision matrix by Gaussian elimination.
!***DESCRIPTION
!
!     DGEFA factors a double precision matrix by Gaussian elimination.
!
!     DGEFA is usually called by DGECO, but it can be called
!     directly with a saving in time if  RCOND  is not needed.
!     (Time for DGECO) = (1 + 9/N)*(Time for DGEFA) .
!
!     On Entry
!
!        A       DOUBLE PRECISION(LDA, N)
!                the matrix to be factored.
!
!        LDA     INTEGER
!                the leading dimension of the array  A .
!
!        N       INTEGER
!                the order of the matrix  A .
!
!     On Return
!
!        A       an upper triangular matrix and the multipliers
!                which were used to obtain it.
!                The factorization can be written  A = L*U  where
!                L  is a product of permutation and unit lower
!                triangular matrices and  U  is upper triangular.
!
!        IPVT    INTEGER(N)
!                an integer vector of pivot indices.
!
!        INFO    INTEGER
!                = 0  normal value.
!                = K  if  U(K,K) .EQ. 0.0 .  This is not an error
!                     condition for this subroutine, but it does
!                     indicate that DGESL or DGEDI will divide by zero
!                     if called.  Use  RCOND  in DGECO for a reliable
!                     indication of singularity.
!
!     LINPACK.  This version dated 08/14/78 .
!     Cleve Moler, University of New Mexico, Argonne National Lab.
!
!     Subroutines and Functions
!
!     BLAS DAXPY,DSCAL,IDAMAX
!***REFERENCES  DONGARRA J.J., BUNCH J.R., MOLER C.B., STEWART G.W.,
!                 *LINPACK USERS  GUIDE*, SIAM, 1979.
!***ROUTINES CALLED  DAXPY,DSCAL,IDAMAX
!***END PROLOGUE  DGEFA
      INTEGER LDA,N,IPVT(1),INFO
      DOUBLE PRECISION A(LDA,1)
!
      DOUBLE PRECISION T
      INTEGER IDAMAX,J,K,KP1,L,NM1
!
!     GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING
!
!***FIRST EXECUTABLE STATEMENT  DGEFA
      INFO = 0
      NM1 = N - 1
      IF (NM1 .LT. 1) GO TO 70
      DO 60 K = 1, NM1
         KP1 = K + 1
!
!        FIND L = PIVOT INDEX
!
         L = IDAMAX(N-K+1,A(K,K),1) + K - 1
         IPVT(K) = L
!
!        ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED
!
         IF (A(L,K) .EQ. 0.0D0) GO TO 40
!
!           INTERCHANGE IF NECESSARY
!
            IF (L .EQ. K) GO TO 10
               T = A(L,K)
               A(L,K) = A(K,K)
               A(K,K) = T
   10       CONTINUE
!
!           COMPUTE MULTIPLIERS
!
            T = -1.0D0/A(K,K)
            CALL DSCAL(N-K,T,A(K+1,K),1)
!
!           ROW ELIMINATION WITH COLUMN INDEXING
!
            DO 30 J = KP1, N
               T = A(L,J)
               IF (L .EQ. K) GO TO 20
                  A(L,J) = A(K,J)
                  A(K,J) = T
   20          CONTINUE
               CALL DAXPY(N-K,T,A(K+1,K),1,A(K+1,J),1)
   30       CONTINUE
         GO TO 50
   40    CONTINUE
            INFO = K
   50    CONTINUE
   60 CONTINUE
   70 CONTINUE
      IPVT(N) = N
      IF (A(N,N) .EQ. 0.0D0) INFO = N
      RETURN
      END SUBROUTINE DGEFA
      SUBROUTINE DGESL(A,LDA,N,IPVT,B,JOB)
!***BEGIN PROLOGUE  DGESL
!***DATE WRITTEN   780814   (YYMMDD)
!***REVISION DATE  820801   (YYMMDD)
!***CATEGORY NO.  D2A1
!***KEYWORDS  DOUBLE PRECISION,LINEAR ALGEBRA,LINPACK,MATRIX,SOLVE
!***AUTHOR  MOLER, C. B., (U. OF NEW MEXICO)
!***PURPOSE  Solves the double precision system  A*X=B or  TRANS(A)*X=B
!            using the factors computed by DGECO or DGEFA.
!***DESCRIPTION
!
!     DGESL solves the double precision system
!     A * X = B  or  TRANS(A) * X = B
!     using the factors computed by DGECO or DGEFA.
!
!     On Entry
!
!        A       DOUBLE PRECISION(LDA, N)
!                the output from DGECO or DGEFA.
!
!        LDA     INTEGER
!                the leading dimension of the array  A .
!
!        N       INTEGER
!                the order of the matrix  A .
!
!        IPVT    INTEGER(N)
!                the pivot vector from DGECO or DGEFA.
!
!        B       DOUBLE PRECISION(N)
!                the right hand side vector.
!
!        JOB     INTEGER
!                = 0         to solve  A*X = B ,
!                = nonzero   to solve  TRANS(A)*X = B  where
!                            TRANS(A)  is the transpose.
!
!     On Return
!
!        B       the solution vector  X .
!
!     Error Condition
!
!        A division by zero will occur if the input factor contains a
!        zero on the diagonal.  Technically this indicates singularity
!        but it is often caused by improper arguments or improper
!        setting of LDA .  It will not occur if the subroutines are
!        called correctly and if DGECO has set RCOND .GT. 0.0
!        or DGEFA has set INFO .EQ. 0 .
!
!     To compute  INVERSE(A) * C  where  C  is a matrix
!     with  P  columns
!           CALL DGECO(A,LDA,N,IPVT,RCOND,Z)
!           IF (RCOND is too small) GO TO ...
!           DO 10 J = 1, P
!              CALL DGESL(A,LDA,N,IPVT,C(1,J),0)
!        10 CONTINUE
!
!     LINPACK.  This version dated 08/14/78 .
!     Cleve Moler, University of New Mexico, Argonne National Lab.
!
!     Subroutines and Functions
!
!     BLAS DAXPY,DDOT
!***REFERENCES  DONGARRA J.J., BUNCH J.R., MOLER C.B., STEWART G.W.,
!                 *LINPACK USERS  GUIDE*, SIAM, 1979.
!***ROUTINES CALLED  DAXPY,DDOT
!***END PROLOGUE  DGESL
      INTEGER LDA,N,IPVT(1),JOB
      DOUBLE PRECISION A(LDA,1),B(1)
!
      DOUBLE PRECISION DDOT,T
      INTEGER K,KB,L,NM1
!***FIRST EXECUTABLE STATEMENT  DGESL
      NM1 = N - 1
      IF (JOB .NE. 0) GO TO 50
!
!        JOB = 0 , SOLVE  A * X = B
!        FIRST SOLVE  L*Y = B
!
         IF (NM1 .LT. 1) GO TO 30
         DO 20 K = 1, NM1
            L = IPVT(K)
            T = B(L)
            IF (L .EQ. K) GO TO 10
               B(L) = B(K)
               B(K) = T
   10       CONTINUE
            CALL DAXPY(N-K,T,A(K+1,K),1,B(K+1),1)
   20    CONTINUE
   30    CONTINUE
!
!        NOW SOLVE  U*X = Y
!
         DO 40 KB = 1, N
            K = N + 1 - KB
            B(K) = B(K)/A(K,K)
            T = -B(K)
            CALL DAXPY(K-1,T,A(1,K),1,B(1),1)
   40    CONTINUE
      GO TO 100
   50 CONTINUE
!
!        JOB = NONZERO, SOLVE  TRANS(A) * X = B
!        FIRST SOLVE  TRANS(U)*Y = B
!
         DO 60 K = 1, N
            T = DDOT(K-1,A(1,K),1,B(1),1)
            B(K) = (B(K) - T)/A(K,K)
   60    CONTINUE
!
!        NOW SOLVE TRANS(L)*X = Y
!
         IF (NM1 .LT. 1) GO TO 90
         DO 80 KB = 1, NM1
            K = N - KB
            B(K) = B(K) + DDOT(N-K,A(K+1,K),1,B(K+1),1)
            L = IPVT(K)
            IF (L .EQ. K) GO TO 70
               T = B(L)
               B(L) = B(K)
               B(K) = T
   70       CONTINUE
   80    CONTINUE
   90    CONTINUE
  100 CONTINUE
      RETURN
      END SUBROUTINE DGESL
      SUBROUTINE DGEDI(A,LDA,N,IPVT,DET,WORK,JOB)
!***BEGIN PROLOGUE  DGEDI
!***DATE WRITTEN   780814   (YYMMDD)
!***REVISION DATE  820801   (YYMMDD)
!***REVISION HISTORY  (YYMMDD)
!   000330  Modified array declarations.  (JEC)
!***CATEGORY NO.  D3A1,D2A1
!***KEYWORDS  DETERMINANT,DOUBLE PRECISION,FACTOR,INVERSE,
!             LINEAR ALGEBRA,LINPACK,MATRIX
!***AUTHOR  MOLER, C. B., (U. OF NEW MEXICO)
!***PURPOSE  Computes the determinant and inverse of a matrix using
!            factors computed by DGECO or DGEFA.
!***DESCRIPTION
!
!     DGEDI computes the determinant and inverse of a matrix
!     using the factors computed by DGECO or DGEFA.
!
!     On Entry
!
!        A       DOUBLE PRECISION(LDA, N)
!                the output from DGECO or DGEFA.
!
!        LDA     INTEGER
!                the leading dimension of the array  A .
!
!        N       INTEGER
!                the order of the matrix  A .
!
!        IPVT    INTEGER(N)
!                the pivot vector from DGECO or DGEFA.
!
!        WORK    DOUBLE PRECISION(N)
!                work vector.  Contents destroyed.
!
!        JOB     INTEGER
!                = 11   both determinant and inverse.
!                = 01   inverse only.
!                = 10   determinant only.
!
!     On Return
!
!        A       inverse of original matrix if requested.
!                Otherwise unchanged.
!
!        DET     DOUBLE PRECISION(2)
!                determinant of original matrix if requested.
!                Otherwise not referenced.
!                Determinant = DET(1) * 10.0**DET(2)
!                with  1.0 .LE. DABS(DET(1)) .LT. 10.0
!                or  DET(1) .EQ. 0.0 .
!
!     Error Condition
!
!        A division by zero will occur if the input factor contains
!        a zero on the diagonal and the inverse is requested.
!        It will not occur if the subroutines are called correctly
!        and if DGECO has set RCOND .GT. 0.0 or DGEFA has set
!        INFO .EQ. 0 .
!
!     LINPACK.  This version dated 08/14/78 .
!     Cleve Moler, University of New Mexico, Argonne National Lab.
!
!     Subroutines and Functions
!
!     BLAS DAXPY,DSCAL,DSWAP
!     Fortran DABS,MOD
!***REFERENCES  DONGARRA J.J., BUNCH J.R., MOLER C.B., STEWART G.W.,
!                 *LINPACK USERS  GUIDE*, SIAM, 1979.
!***ROUTINES CALLED  DAXPY,DSCAL,DSWAP
!***END PROLOGUE  DGEDI
      INTEGER LDA,N,IPVT(*),JOB
      DOUBLE PRECISION A(LDA,*),DET(2),WORK(*)
!
      DOUBLE PRECISION T
      DOUBLE PRECISION TEN
      INTEGER I,J,K,KB,KP1,L,NM1
!
!     COMPUTE DETERMINANT
!
!***FIRST EXECUTABLE STATEMENT  DGEDI
      IF (JOB/10 .EQ. 0) GO TO 70
         DET(1) = 1.0D0
         DET(2) = 0.0D0
         TEN = 10.0D0
         DO 50 I = 1, N
            IF (IPVT(I) .NE. I) DET(1) = -DET(1)
            DET(1) = A(I,I)*DET(1)
!        ...EXIT
            IF (DET(1) .EQ. 0.0D0) GO TO 60
   10       IF (DABS(DET(1)) .GE. 1.0D0) GO TO 20
               DET(1) = TEN*DET(1)
               DET(2) = DET(2) - 1.0D0
            GO TO 10
   20       CONTINUE
   30       IF (DABS(DET(1)) .LT. TEN) GO TO 40
               DET(1) = DET(1)/TEN
               DET(2) = DET(2) + 1.0D0
            GO TO 30
   40       CONTINUE
   50    CONTINUE
   60    CONTINUE
   70 CONTINUE
!
!     COMPUTE INVERSE(U)
!
      IF (MOD(JOB,10) .EQ. 0) GO TO 150
         DO 100 K = 1, N
            A(K,K) = 1.0D0/A(K,K)
            T = -A(K,K)
            CALL DSCAL(K-1,T,A(1,K),1)
            KP1 = K + 1
            IF (N .LT. KP1) GO TO 90
            DO 80 J = KP1, N
               T = A(K,J)
               A(K,J) = 0.0D0
               CALL DAXPY(K,T,A(1,K),1,A(1,J),1)
   80       CONTINUE
   90       CONTINUE
  100    CONTINUE
!
!        FORM INVERSE(U)*INVERSE(L)
!
         NM1 = N - 1
         IF (NM1 .LT. 1) GO TO 140
         DO 130 KB = 1, NM1
            K = N - KB
            KP1 = K + 1
            DO 110 I = KP1, N
               WORK(I) = A(I,K)
               A(I,K) = 0.0D0
  110       CONTINUE
            DO 120 J = KP1, N
               T = WORK(J)
               CALL DAXPY(N,T,A(1,J),1,A(1,K),1)
  120       CONTINUE
            L = IPVT(K)
            IF (L .NE. K) CALL DSWAP(N,A(1,K),1,A(1,L),1)
  130    CONTINUE
  140    CONTINUE
  150 CONTINUE
      RETURN
      END SUBROUTINE DGEDI
      SUBROUTINE DGEMM ( TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, LDB,   &
                         BETA, C, LDC ,   &
                         IERROR)
!     .. Scalar Arguments ..
      CHARACTER*1        TRANSA, TRANSB
      CHARACTER*4        IERROR
      INTEGER            M, N, K, LDA, LDB, LDC
      DOUBLE PRECISION   ALPHA, BETA
!     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), C( LDC, * )
!     ..
!
      INCLUDE 'DPCOP2.INC'
!
!
!  Purpose
!  =======
!
!  DGEMM  performs one of the matrix-matrix operations
!
!     C := alpha*op( A )*op( B ) + beta*C,
!
!  where  op( X ) is one of
!
!     op( X ) = X   or   op( X ) = X',
!
!  alpha and beta are scalars, and A, B and C are matrices, with op( A )
!  an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
!
!  Parameters
!  ==========
!
!  TRANSA - CHARACTER*1.
!           On entry, TRANSA specifies the form of op( A ) to be used in
!           the matrix multiplication as follows:
!
!              TRANSA = 'N' or 'n',  op( A ) = A.
!
!              TRANSA = 'T' or 't',  op( A ) = A'.
!
!              TRANSA = 'C' or 'c',  op( A ) = A'.
!
!           Unchanged on exit.
!
!  TRANSB - CHARACTER*1.
!           On entry, TRANSB specifies the form of op( B ) to be used in
!           the matrix multiplication as follows:
!
!              TRANSB = 'N' or 'n',  op( B ) = B.
!
!              TRANSB = 'T' or 't',  op( B ) = B'.
!
!              TRANSB = 'C' or 'c',  op( B ) = B'.
!
!           Unchanged on exit.
!
!  M      - INTEGER.
!           On entry,  M  specifies  the number  of rows  of the  matrix
!           op( A )  and of the  matrix  C.  M  must  be at least  zero.
!           Unchanged on exit.
!
!  N      - INTEGER.
!           On entry,  N  specifies the number  of columns of the matrix
!           op( B ) and the number of columns of the matrix C. N must be
!           at least zero.
!           Unchanged on exit.
!
!  K      - INTEGER.
!           On entry,  K  specifies  the number of columns of the matrix
!           op( A ) and the number of rows of the matrix op( B ). K must
!           be at least  zero.
!           Unchanged on exit.
!
!  ALPHA  - DOUBLE PRECISION.
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.
!
!  A      - DOUBLE PRECISION array of DIMENSION ( LDA, ka ), where ka is
!           k  when  TRANSA = 'N' or 'n',  and is  m  otherwise.
!           Before entry with  TRANSA = 'N' or 'n',  the leading  m by k
!           part of the array  A  must contain the matrix  A,  otherwise
!           the leading  k by m  part of the array  A  must contain  the
!           matrix A.
!           Unchanged on exit.
!
!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. When  TRANSA = 'N' or 'n' then
!           LDA must be at least  max( 1, m ), otherwise  LDA must be at
!           least  max( 1, k ).
!           Unchanged on exit.
!
!  B      - DOUBLE PRECISION array of DIMENSION ( LDB, kb ), where kb is
!           n  when  TRANSB = 'N' or 'n',  and is  k  otherwise.
!           Before entry with  TRANSB = 'N' or 'n',  the leading  k by n
!           part of the array  B  must contain the matrix  B,  otherwise
!           the leading  n by k  part of the array  B  must contain  the
!           matrix B.
!           Unchanged on exit.
!
!  LDB    - INTEGER.
!           On entry, LDB specifies the first dimension of B as declared
!           in the calling (sub) program. When  TRANSB = 'N' or 'n' then
!           LDB must be at least  max( 1, k ), otherwise  LDB must be at
!           least  max( 1, n ).
!           Unchanged on exit.
!
!  BETA   - DOUBLE PRECISION.
!           On entry,  BETA  specifies the scalar  beta.  When  BETA  is
!           supplied as zero then C need not be set on input.
!           Unchanged on exit.
!
!  C      - DOUBLE PRECISION array of DIMENSION ( LDC, n ).
!           Before entry, the leading  m by n  part of the array  C must
!           contain the matrix  C,  except when  beta  is zero, in which
!           case C need not be set on entry.
!           On exit, the array  C  is overwritten by the  m by n  matrix
!           ( alpha*op( A )*op( B ) + beta*C ).
!
!  LDC    - INTEGER.
!           On entry, LDC specifies the first dimension of C as declared
!           in  the  calling  (sub)  program.   LDC  must  be  at  least
!           max( 1, m ).
!           Unchanged on exit.
!
!
!  Level 3 Blas routine.
!
!  -- Written on 8-February-1989.
!     Jack Dongarra, Argonne National Laboratory.
!     Iain Duff, AERE Harwell.
!     Jeremy Du Croz, Numerical Algorithms Group Ltd.
!     Sven Hammarling, Numerical Algorithms Group Ltd.
!
!     Slight modifications made by Alan Heckert 8/97 to
!     incorporate into Dataplot (no numerical modifications,
!     just error handling and printing)
!
!     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
!     .. External Subroutines ..
!CCCC EXTERNAL           XERBLA
!     .. Intrinsic Functions ..
      INTRINSIC          MAX
!     .. Local Scalars ..
      LOGICAL            NOTA, NOTB
      INTEGER            I, INFO, J, L, NCOLA, NROWA, NROWB
      DOUBLE PRECISION   TEMP
!     .. Parameters ..
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!     ..
!     .. Executable Statements ..
!
!     Set  NOTA  and  NOTB  as  true if  A  and  B  respectively are not
!     transposed and set  NROWA, NCOLA and  NROWB  as the number of rows
!     and  columns of  A  and the  number of  rows  of  B  respectively.
!
      IERROR='NO'
      NOTA  = LSAME( TRANSA, 'N' )
      NOTB  = LSAME( TRANSB, 'N' )
      IF( NOTA )THEN
         NROWA = M
         NCOLA = K
      ELSE
         NROWA = K
         NCOLA = M
      END IF
      IF( NOTB )THEN
         NROWB = K
      ELSE
         NROWB = N
      END IF
!
!     Test the input parameters.
!
      INFO = 0
      IF(      ( .NOT.NOTA                 ).AND.   &
               ( .NOT.LSAME( TRANSA, 'C' ) ).AND.   &
               ( .NOT.LSAME( TRANSA, 'T' ) )      )THEN
         INFO = 1
      ELSE IF( ( .NOT.NOTB                 ).AND.   &
               ( .NOT.LSAME( TRANSB, 'C' ) ).AND.   &
               ( .NOT.LSAME( TRANSB, 'T' ) )      )THEN
         INFO = 2
      ELSE IF( M  .LT.0               )THEN
         INFO = 3
      ELSE IF( N  .LT.0               )THEN
         INFO = 4
      ELSE IF( K  .LT.0               )THEN
         INFO = 5
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 8
      ELSE IF( LDB.LT.MAX( 1, NROWB ) )THEN
         INFO = 10
      ELSE IF( LDC.LT.MAX( 1, M     ) )THEN
         INFO = 13
      END IF
      IF( INFO.NE.0 )THEN
!CCCC    CALL XERBLA( 'DGEMM ', INFO )
         WRITE(ICOUT,1001)
         CALL DPWRST('XXX','BUG ')
         IERROR='YES'
         RETURN
      END IF
 1001 FORMAT('***** RECIPE ERROR: INTERNAL ERROR FROM DGEMM, INVALID',   &
      ' ARGUMENTS.')
!
!     Quick return if possible.
!
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.   &
          ( ( ( ALPHA.EQ.ZERO ).OR.( K.EQ.0 ) ).AND.( BETA.EQ.ONE ) ) )   &
         RETURN
!
!     And if  alpha.eq.zero.
!
      IF( ALPHA.EQ.ZERO )THEN
         IF( BETA.EQ.ZERO )THEN
            DO 20, J = 1, N
               DO 10, I = 1, M
                  C( I, J ) = ZERO
   10          CONTINUE
   20       CONTINUE
         ELSE
            DO 40, J = 1, N
               DO 30, I = 1, M
                  C( I, J ) = BETA*C( I, J )
   30          CONTINUE
   40       CONTINUE
         END IF
         RETURN
      END IF
!
!     Start the operations.
!
      IF( NOTB )THEN
         IF( NOTA )THEN
!
!           Form  C := alpha*A*B + beta*C.
!
            DO 90, J = 1, N
               IF( BETA.EQ.ZERO )THEN
                  DO 50, I = 1, M
                     C( I, J ) = ZERO
   50             CONTINUE
               ELSE IF( BETA.NE.ONE )THEN
                  DO 60, I = 1, M
                     C( I, J ) = BETA*C( I, J )
   60             CONTINUE
               END IF
               DO 80, L = 1, K
                  IF( B( L, J ).NE.ZERO )THEN
                     TEMP = ALPHA*B( L, J )
                     DO 70, I = 1, M
                        C( I, J ) = C( I, J ) + TEMP*A( I, L )
   70                CONTINUE
                  END IF
   80          CONTINUE
   90       CONTINUE
         ELSE
!
!           Form  C := alpha*A'*B + beta*C
!
            DO 120, J = 1, N
               DO 110, I = 1, M
                  TEMP = ZERO
                  DO 100, L = 1, K
                     TEMP = TEMP + A( L, I )*B( L, J )
  100             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  110          CONTINUE
  120       CONTINUE
         END IF
      ELSE
         IF( NOTA )THEN
!
!           Form  C := alpha*A*B' + beta*C
!
            DO 170, J = 1, N
               IF( BETA.EQ.ZERO )THEN
                  DO 130, I = 1, M
                     C( I, J ) = ZERO
  130             CONTINUE
               ELSE IF( BETA.NE.ONE )THEN
                  DO 140, I = 1, M
                     C( I, J ) = BETA*C( I, J )
  140             CONTINUE
               END IF
               DO 160, L = 1, K
                  IF( B( J, L ).NE.ZERO )THEN
                     TEMP = ALPHA*B( J, L )
                     DO 150, I = 1, M
                        C( I, J ) = C( I, J ) + TEMP*A( I, L )
  150                CONTINUE
                  END IF
  160          CONTINUE
  170       CONTINUE
         ELSE
!
!           Form  C := alpha*A'*B' + beta*C
!
            DO 200, J = 1, N
               DO 190, I = 1, M
                  TEMP = ZERO
                  DO 180, L = 1, K
                     TEMP = TEMP + A( L, I )*B( J, L )
  180             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  190          CONTINUE
  200       CONTINUE
         END IF
      END IF
!
      RETURN
!
!     End of DGEMM .
!
      END SUBROUTINE DGEMM 
      SUBROUTINE DGEMV ( TRANS, M, N, ALPHA, A, LDA, X, INCX,   &
                         BETA, Y, INCY,   &
                         IERROR )
!     .. Scalar Arguments ..
      DOUBLE PRECISION   ALPHA, BETA
      INTEGER            INCX, INCY, LDA, M, N
      CHARACTER*1        TRANS
      CHARACTER*4        IERROR
!     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), X( * ), Y( * )
!     ..
!
!  Purpose
!  =======
!
!  DGEMV  performs one of the matrix-vector operations
!
!     y := alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y,
!
!  where alpha and beta are scalars, x and y are vectors and A is an
!  m by n matrix.
!
!  Parameters
!  ==========
!
!  TRANS  - CHARACTER*1.
!           On entry, TRANS specifies the operation to be performed as
!           follows:
!
!              TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.
!
!              TRANS = 'T' or 't'   y := alpha*A'*x + beta*y.
!
!              TRANS = 'C' or 'c'   y := alpha*A'*x + beta*y.
!
!           Unchanged on exit.
!
!  M      - INTEGER.
!           On entry, M specifies the number of rows of the matrix A.
!           M must be at least zero.
!           Unchanged on exit.
!
!  N      - INTEGER.
!           On entry, N specifies the number of columns of the matrix A.
!           N must be at least zero.
!           Unchanged on exit.
!
!  ALPHA  - DOUBLE PRECISION.
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.
!
!  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
!           Before entry, the leading m by n part of the array A must
!           contain the matrix of coefficients.
!           Unchanged on exit.
!
!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least
!           max( 1, m ).
!           Unchanged on exit.
!
!  X      - DOUBLE PRECISION array of DIMENSION at least
!           ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'
!           and at least
!           ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.
!           Before entry, the incremented array X must contain the
!           vector x.
!           Unchanged on exit.
!
!  INCX   - INTEGER.
!           On entry, INCX specifies the increment for the elements of
!           X. INCX must not be zero.
!           Unchanged on exit.
!
!  BETA   - DOUBLE PRECISION.
!           On entry, BETA specifies the scalar beta. When BETA is
!           supplied as zero then Y need not be set on input.
!           Unchanged on exit.
!
!  Y      - DOUBLE PRECISION array of DIMENSION at least
!           ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'
!           and at least
!           ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
!           Before entry with BETA non-zero, the incremented array Y
!           must contain the vector y. On exit, Y is overwritten by the
!           updated vector y.
!
!  INCY   - INTEGER.
!           On entry, INCY specifies the increment for the elements of
!           Y. INCY must not be zero.
!           Unchanged on exit.
!
!
!  Level 2 Blas routine.
!
!  -- Written on 22-October-1986.
!     Jack Dongarra, Argonne National Lab.
!     Jeremy Du Croz, Nag Central Office.
!     Sven Hammarling, Nag Central Office.
!     Richard Hanson, Sandia National Labs.
!
!     Slight modifications 8/97 by Alan Heckert to incorporate
!     into Dataplot.  No numerical modifications, just for
!     error handling and printing.
!
!     .. Parameters ..
!
      INCLUDE 'DPCOP2.INC'
!
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!     .. Local Scalars ..
      DOUBLE PRECISION   TEMP
      INTEGER            I, INFO, IX, IY, J, JX, JY, KX, KY, LENX, LENY
!     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
!     .. External Subroutines ..
!CCCC EXTERNAL           XERBLA
!     .. Intrinsic Functions ..
      INTRINSIC          MAX
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      IERROR='NO'
      INFO = 0
      IF     ( .NOT.LSAME( TRANS, 'N' ).AND.   &
               .NOT.LSAME( TRANS, 'T' ).AND.   &
               .NOT.LSAME( TRANS, 'C' )      )THEN
         INFO = 1
      ELSE IF( M.LT.0 )THEN
         INFO = 2
      ELSE IF( N.LT.0 )THEN
         INFO = 3
      ELSE IF( LDA.LT.MAX( 1, M ) )THEN
         INFO = 6
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 8
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 11
      END IF
      IF( INFO.NE.0 )THEN
!CCCC    CALL XERBLA( 'DGEMV ', INFO )
         WRITE(ICOUT,1001)
         CALL DPWRST('XXX','BUG ')
         IERROR='YES'
         RETURN
      END IF
 1001 FORMAT('***** RECIPE ERROR: INTERNAL ERROR FROM DGEMV, INVALID',   &
      ' ARGUMENTS.')
!
!     Quick return if possible.
!
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.   &
          ( ( ALPHA.EQ.ZERO ).AND.( BETA.EQ.ONE ) ) )   &
         RETURN
!
!     Set  LENX  and  LENY, the lengths of the vectors x and y, and set
!     up the start points in  X  and  Y.
!
      IF( LSAME( TRANS, 'N' ) )THEN
         LENX = N
         LENY = M
      ELSE
         LENX = M
         LENY = N
      END IF
      IF( INCX.GT.0 )THEN
         KX = 1
      ELSE
         KX = 1 - ( LENX - 1 )*INCX
      END IF
      IF( INCY.GT.0 )THEN
         KY = 1
      ELSE
         KY = 1 - ( LENY - 1 )*INCY
      END IF
!
!     Start the operations. In this version the elements of A are
!     accessed sequentially with one pass through A.
!
!     First form  y := beta*y.
!
      IF( BETA.NE.ONE )THEN
         IF( INCY.EQ.1 )THEN
            IF( BETA.EQ.ZERO )THEN
               DO 10, I = 1, LENY
                  Y( I ) = ZERO
   10          CONTINUE
            ELSE
               DO 20, I = 1, LENY
                  Y( I ) = BETA*Y( I )
   20          CONTINUE
            END IF
         ELSE
            IY = KY
            IF( BETA.EQ.ZERO )THEN
               DO 30, I = 1, LENY
                  Y( IY ) = ZERO
                  IY      = IY   + INCY
   30          CONTINUE
            ELSE
               DO 40, I = 1, LENY
                  Y( IY ) = BETA*Y( IY )
                  IY      = IY           + INCY
   40          CONTINUE
            END IF
         END IF
      END IF
      IF( ALPHA.EQ.ZERO )   &
         RETURN
      IF( LSAME( TRANS, 'N' ) )THEN
!
!        Form  y := alpha*A*x + y.
!
         JX = KX
         IF( INCY.EQ.1 )THEN
            DO 60, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  DO 50, I = 1, M
                     Y( I ) = Y( I ) + TEMP*A( I, J )
   50             CONTINUE
               END IF
               JX = JX + INCX
   60       CONTINUE
         ELSE
            DO 80, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  IY   = KY
                  DO 70, I = 1, M
                     Y( IY ) = Y( IY ) + TEMP*A( I, J )
                     IY      = IY      + INCY
   70             CONTINUE
               END IF
               JX = JX + INCX
   80       CONTINUE
         END IF
      ELSE
!
!        Form  y := alpha*A'*x + y.
!
         JY = KY
         IF( INCX.EQ.1 )THEN
            DO 100, J = 1, N
               TEMP = ZERO
               DO 90, I = 1, M
                  TEMP = TEMP + A( I, J )*X( I )
   90          CONTINUE
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
  100       CONTINUE
         ELSE
            DO 120, J = 1, N
               TEMP = ZERO
               IX   = KX
               DO 110, I = 1, M
                  TEMP = TEMP + A( I, J )*X( IX )
                  IX   = IX   + INCX
  110          CONTINUE
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
  120       CONTINUE
         END IF
      END IF
!
      RETURN
!
!     End of DGEMV .
!
      END SUBROUTINE DGEMV 
      SUBROUTINE DGER  ( M, N, ALPHA, X, INCX, Y, INCY, A, LDA, IERROR )
!     .. Scalar Arguments ..
      DOUBLE PRECISION   ALPHA
      INTEGER            INCX, INCY, LDA, M, N
!     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), X( * ), Y( * )
!
      CHARACTER*4 IERROR
      INCLUDE 'DPCOP2.INC'
!
!     ..
!
!  Purpose
!  =======
!
!  DGER   performs the rank 1 operation
!
!     A := alpha*x*y' + A,
!
!  where alpha is a scalar, x is an m element vector, y is an n element
!  vector and A is an m by n matrix.
!
!  Parameters
!  ==========
!
!  M      - INTEGER.
!           On entry, M specifies the number of rows of the matrix A.
!           M must be at least zero.
!           Unchanged on exit.
!
!  N      - INTEGER.
!           On entry, N specifies the number of columns of the matrix A.
!           N must be at least zero.
!           Unchanged on exit.
!
!  ALPHA  - DOUBLE PRECISION.
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.
!
!  X      - DOUBLE PRECISION array of dimension at least
!           ( 1 + ( m - 1 )*abs( INCX ) ).
!           Before entry, the incremented array X must contain the m
!           element vector x.
!           Unchanged on exit.
!
!  INCX   - INTEGER.
!           On entry, INCX specifies the increment for the elements of
!           X. INCX must not be zero.
!           Unchanged on exit.
!
!  Y      - DOUBLE PRECISION array of dimension at least
!           ( 1 + ( n - 1 )*abs( INCY ) ).
!           Before entry, the incremented array Y must contain the n
!           element vector y.
!           Unchanged on exit.
!
!  INCY   - INTEGER.
!           On entry, INCY specifies the increment for the elements of
!           Y. INCY must not be zero.
!           Unchanged on exit.
!
!  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
!           Before entry, the leading m by n part of the array A must
!           contain the matrix of coefficients. On exit, A is
!           overwritten by the updated matrix.
!
!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least
!           max( 1, m ).
!           Unchanged on exit.
!
!
!  Level 2 Blas routine.
!
!  -- Written on 22-October-1986.
!     Jack Dongarra, Argonne National Lab.
!     Jeremy Du Croz, Nag Central Office.
!     Sven Hammarling, Nag Central Office.
!     Richard Hanson, Sandia National Labs.
!
!     Minor modifications 8/97 by Alan Heckert to incorporate
!     into Dataplot.  No numerical modifications.  Just
!     error handling and printing.
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER        ( ZERO = 0.0D+0 )
!     .. Local Scalars ..
      DOUBLE PRECISION   TEMP
      INTEGER            I, INFO, IX, J, JY, KX
!     .. External Subroutines ..
!CCCC EXTERNAL           XERBLA
!     .. Intrinsic Functions ..
      INTRINSIC          MAX
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      IERROR='NO'
      INFO = 0
      IF     ( M.LT.0 )THEN
         INFO = 1
      ELSE IF( N.LT.0 )THEN
         INFO = 2
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 5
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 7
      ELSE IF( LDA.LT.MAX( 1, M ) )THEN
         INFO = 9
      END IF
      IF( INFO.NE.0 )THEN
!CCCC    CALL XERBLA( 'DGER  ', INFO )
         WRITE(ICOUT,1001)
         CALL DPWRST('XXX','BUG ')
         IERROR='YES'
         RETURN
      END IF
 1001 FORMAT('***** RECIPE ERROR: INTERNAL ERROR FROM DGER, INVALID',   &
      ' ARGUMENTS.')
!
!     Quick return if possible.
!
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.( ALPHA.EQ.ZERO ) )   &
         RETURN
!
!     Start the operations. In this version the elements of A are
!     accessed sequentially with one pass through A.
!
      IF( INCY.GT.0 )THEN
         JY = 1
      ELSE
         JY = 1 - ( N - 1 )*INCY
      END IF
      IF( INCX.EQ.1 )THEN
         DO 20, J = 1, N
            IF( Y( JY ).NE.ZERO )THEN
               TEMP = ALPHA*Y( JY )
               DO 10, I = 1, M
                  A( I, J ) = A( I, J ) + X( I )*TEMP
   10          CONTINUE
            END IF
            JY = JY + INCY
   20    CONTINUE
      ELSE
         IF( INCX.GT.0 )THEN
            KX = 1
         ELSE
            KX = 1 - ( M - 1 )*INCX
         END IF
         DO 40, J = 1, N
            IF( Y( JY ).NE.ZERO )THEN
               TEMP = ALPHA*Y( JY )
               IX   = KX
               DO 30, I = 1, M
                  A( I, J ) = A( I, J ) + X( IX )*TEMP
                  IX        = IX        + INCX
   30          CONTINUE
            END IF
            JY = JY + INCY
   40    CONTINUE
      END IF
!
      RETURN
!
!     End of DGER  .
!
      END SUBROUTINE DGER  
      SUBROUTINE DGSL1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR GREEK SIMPLEX LOWER CASE (PART 1).
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-921-3651
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
!     DEFINE CHARACTER    627--LOWER CASE ALPH
!
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',  -1,   5/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',  -3,   4/
      DATA IOPERA(   3),IX(   3),IY(   3)/'DRAW',  -5,   2/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',  -6,   0/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',  -7,  -3/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',  -7,  -6/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',  -6,  -8/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',  -4,  -9/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',  -2,  -9/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',   0,  -8/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',   3,  -5/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',   5,  -2/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',   7,   2/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',   8,   5/
      DATA IOPERA(  15),IX(  15),IY(  15)/'MOVE',  -1,   5/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',   1,   5/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',   2,   4/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',   3,   2/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',   5,  -6/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',   6,  -8/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',   7,  -9/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',   8,  -9/
!
      DATA IXMIND(   1)/ -10/
      DATA IXMAXD(   1)/  11/
      DATA IXDELD(   1)/  21/
      DATA ISTARD(   1)/   1/
      DATA NUMCOO(   1)/  22/
!
!     DEFINE CHARACTER    628--LOWER CASE BETA
!
      DATA IOPERA(  23),IX(  23),IY(  23)/'MOVE',   3,  12/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',   1,  11/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',  -1,   9/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',  -3,   5/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',  -4,   2/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',  -5,  -2/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',  -6,  -8/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',  -7, -16/
      DATA IOPERA(  31),IX(  31),IY(  31)/'MOVE',   3,  12/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',   5,  12/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',   7,  10/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',   7,   7/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',   6,   5/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',   5,   4/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',   3,   3/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',   0,   3/
      DATA IOPERA(  39),IX(  39),IY(  39)/'MOVE',   0,   3/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',   2,   2/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',   4,   0/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',   5,  -2/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',   5,  -5/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',   4,  -7/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',   3,  -8/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW',   1,  -9/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',  -1,  -9/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',  -3,  -8/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',  -4,  -7/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW',  -5,  -4/
!
      DATA IXMIND(   2)/  -9/
      DATA IXMAXD(   2)/  10/
      DATA IXDELD(   2)/  19/
      DATA ISTARD(   2)/  23/
      DATA NUMCOO(   2)/  28/
!
!     DEFINE CHARACTER    629--LOWER CASE GAMM
!
      DATA IOPERA(  51),IX(  51),IY(  51)/'MOVE',  -8,   2/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',  -6,   4/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',  -4,   5/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',  -3,   5/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',  -1,   4/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',   0,   3/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',   1,   0/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',   1,  -4/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',   0,  -9/
      DATA IOPERA(  60),IX(  60),IY(  60)/'MOVE',   8,   5/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',   7,   2/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',   6,   0/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',   0,  -9/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',  -2, -13/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',  -3, -16/
!
      DATA IXMIND(   3)/  -9/
      DATA IXMAXD(   3)/  10/
      DATA IXDELD(   3)/  19/
      DATA ISTARD(   3)/  51/
      DATA NUMCOO(   3)/  15/
!
!     DEFINE CHARACTER    630--LOWER CASE DELT
!
      DATA IOPERA(  66),IX(  66),IY(  66)/'MOVE',   2,   5/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',  -1,   5/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',  -3,   4/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',  -5,   2/
      DATA IOPERA(  70),IX(  70),IY(  70)/'DRAW',  -6,  -1/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',  -6,  -4/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',  -5,  -7/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',  -4,  -8/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',  -2,  -9/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',   0,  -9/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',   2,  -8/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',   4,  -6/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',   5,  -3/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',   5,   0/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',   4,   3/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',   2,   5/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',   0,   7/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',  -1,   9/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW',  -1,  11/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',   0,  12/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',   2,  12/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',   4,  11/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',   6,   9/
!
      DATA IXMIND(   4)/  -9/
      DATA IXMAXD(   4)/   9/
      DATA IXDELD(   4)/  18/
      DATA ISTARD(   4)/  66/
      DATA NUMCOO(   4)/  23/
!
!     DEFINE CHARACTER    631--LOWER CASE EPSI
!
      DATA IOPERA(  89),IX(  89),IY(  89)/'MOVE',   5,   3/
      DATA IOPERA(  90),IX(  90),IY(  90)/'DRAW',   4,   4/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',   2,   5/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW',  -1,   5/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',  -3,   4/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',  -3,   2/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',  -2,   0/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',   1,  -1/
      DATA IOPERA(  97),IX(  97),IY(  97)/'MOVE',   1,  -1/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',  -3,  -2/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',  -5,  -4/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',  -5,  -6/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',  -4,  -8/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',  -2,  -9/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',   1,  -9/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',   3,  -8/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',   5,  -6/
!
      DATA IXMIND(   5)/  -8/
      DATA IXMAXD(   5)/   8/
      DATA IXDELD(   5)/  16/
      DATA ISTARD(   5)/  89/
      DATA NUMCOO(   5)/  17/
!
!     DEFINE CHARACTER    632--LOWER CASE ZETA
!
      DATA IOPERA( 106),IX( 106),IY( 106)/'MOVE',   2,  12/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',   0,  11/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',  -1,  10/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',  -1,   9/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',   0,   8/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',   3,   7/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',   6,   7/
      DATA IOPERA( 113),IX( 113),IY( 113)/'MOVE',   6,   7/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',   2,   5/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',  -1,   3/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',  -4,   0/
      DATA IOPERA( 117),IX( 117),IY( 117)/'DRAW',  -5,  -3/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',  -5,  -5/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',  -4,  -7/
      DATA IOPERA( 120),IX( 120),IY( 120)/'DRAW',  -2,  -9/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW',   1, -11/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW',   2, -13/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',   2, -15/
      DATA IOPERA( 124),IX( 124),IY( 124)/'DRAW',   1, -16/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',  -1, -16/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW',  -2, -14/
!
      DATA IXMIND(   6)/  -8/
      DATA IXMAXD(   6)/   7/
      DATA IXDELD(   6)/  15/
      DATA ISTARD(   6)/ 106/
      DATA NUMCOO(   6)/  21/
!
!     DEFINE CHARACTER    633--LOWER CASE ETA
!
      DATA IOPERA( 127),IX( 127),IY( 127)/'MOVE',  -9,   1/
      DATA IOPERA( 128),IX( 128),IY( 128)/'DRAW',  -8,   3/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',  -6,   5/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',  -4,   5/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',  -3,   4/
      DATA IOPERA( 132),IX( 132),IY( 132)/'DRAW',  -3,   2/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',  -4,  -2/
      DATA IOPERA( 134),IX( 134),IY( 134)/'DRAW',  -6,  -9/
      DATA IOPERA( 135),IX( 135),IY( 135)/'MOVE',  -4,  -2/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',  -2,   2/
      DATA IOPERA( 137),IX( 137),IY( 137)/'DRAW',   0,   4/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW',   2,   5/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW',   4,   5/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW',   6,   3/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',   6,   0/
      DATA IOPERA( 142),IX( 142),IY( 142)/'DRAW',   5,  -5/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',   2, -16/
!
      DATA IXMIND(   7)/ -10/
      DATA IXMAXD(   7)/  10/
      DATA IXDELD(   7)/  20/
      DATA ISTARD(   7)/ 127/
      DATA NUMCOO(   7)/  17/
!
!     DEFINE CHARACTER    634--LOWER CASE THET
!
      DATA IOPERA( 144),IX( 144),IY( 144)/'MOVE', -10,   1/
      DATA IOPERA( 145),IX( 145),IY( 145)/'DRAW',  -9,   3/
      DATA IOPERA( 146),IX( 146),IY( 146)/'DRAW',  -7,   5/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',  -5,   5/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',  -4,   4/
      DATA IOPERA( 149),IX( 149),IY( 149)/'DRAW',  -4,   2/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',  -5,  -3/
      DATA IOPERA( 151),IX( 151),IY( 151)/'DRAW',  -5,  -6/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',  -4,  -8/
      DATA IOPERA( 153),IX( 153),IY( 153)/'DRAW',  -3,  -9/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',  -1,  -9/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW',   1,  -8/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',   3,  -5/
      DATA IOPERA( 157),IX( 157),IY( 157)/'DRAW',   4,  -3/
      DATA IOPERA( 158),IX( 158),IY( 158)/'DRAW',   5,   0/
      DATA IOPERA( 159),IX( 159),IY( 159)/'DRAW',   6,   5/
      DATA IOPERA( 160),IX( 160),IY( 160)/'DRAW',   6,   8/
      DATA IOPERA( 161),IX( 161),IY( 161)/'DRAW',   5,  11/
      DATA IOPERA( 162),IX( 162),IY( 162)/'DRAW',   3,  12/
      DATA IOPERA( 163),IX( 163),IY( 163)/'DRAW',   1,  12/
      DATA IOPERA( 164),IX( 164),IY( 164)/'DRAW',   0,  10/
      DATA IOPERA( 165),IX( 165),IY( 165)/'DRAW',   0,   8/
      DATA IOPERA( 166),IX( 166),IY( 166)/'DRAW',   1,   5/
      DATA IOPERA( 167),IX( 167),IY( 167)/'DRAW',   3,   2/
      DATA IOPERA( 168),IX( 168),IY( 168)/'DRAW',   5,   0/
      DATA IOPERA( 169),IX( 169),IY( 169)/'DRAW',   8,  -2/
!
      DATA IXMIND(   8)/ -11/
      DATA IXMAXD(   8)/  10/
      DATA IXDELD(   8)/  21/
      DATA ISTARD(   8)/ 144/
      DATA NUMCOO(   8)/  26/
!
!     DEFINE CHARACTER    635--LOWER CASE IOTA
!
      DATA IOPERA( 170),IX( 170),IY( 170)/'MOVE',   0,   5/
      DATA IOPERA( 171),IX( 171),IY( 171)/'DRAW',  -2,  -2/
      DATA IOPERA( 172),IX( 172),IY( 172)/'DRAW',  -3,  -6/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW',  -3,  -8/
      DATA IOPERA( 174),IX( 174),IY( 174)/'DRAW',  -2,  -9/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW',   0,  -9/
      DATA IOPERA( 176),IX( 176),IY( 176)/'DRAW',   2,  -7/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW',   3,  -5/
!
      DATA IXMIND(   9)/  -6/
      DATA IXMAXD(   9)/   5/
      DATA IXDELD(   9)/  11/
      DATA ISTARD(   9)/ 170/
      DATA NUMCOO(   9)/   8/
!
!     DEFINE CHARACTER    636--LOWER CASE KAPP
!
      DATA IOPERA( 178),IX( 178),IY( 178)/'MOVE',  -3,   5/
      DATA IOPERA( 179),IX( 179),IY( 179)/'DRAW',  -7,  -9/
      DATA IOPERA( 180),IX( 180),IY( 180)/'MOVE',   7,   4/
      DATA IOPERA( 181),IX( 181),IY( 181)/'DRAW',   6,   5/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',   5,   5/
      DATA IOPERA( 183),IX( 183),IY( 183)/'DRAW',   3,   4/
      DATA IOPERA( 184),IX( 184),IY( 184)/'DRAW',  -1,   0/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',  -3,  -1/
      DATA IOPERA( 186),IX( 186),IY( 186)/'DRAW',  -4,  -1/
      DATA IOPERA( 187),IX( 187),IY( 187)/'MOVE',  -4,  -1/
      DATA IOPERA( 188),IX( 188),IY( 188)/'DRAW',  -2,  -2/
      DATA IOPERA( 189),IX( 189),IY( 189)/'DRAW',  -1,  -3/
      DATA IOPERA( 190),IX( 190),IY( 190)/'DRAW',   1,  -8/
      DATA IOPERA( 191),IX( 191),IY( 191)/'DRAW',   2,  -9/
      DATA IOPERA( 192),IX( 192),IY( 192)/'DRAW',   3,  -9/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW',   4,  -8/
!
      DATA IXMIND(  10)/  -9/
      DATA IXMAXD(  10)/   9/
      DATA IXDELD(  10)/  18/
      DATA ISTARD(  10)/ 178/
      DATA NUMCOO(  10)/  16/
!
!     DEFINE CHARACTER    637--LOWER CASE LAMB
!
      DATA IOPERA( 194),IX( 194),IY( 194)/'MOVE',  -7,  12/
      DATA IOPERA( 195),IX( 195),IY( 195)/'DRAW',  -5,  12/
      DATA IOPERA( 196),IX( 196),IY( 196)/'DRAW',  -3,  11/
      DATA IOPERA( 197),IX( 197),IY( 197)/'DRAW',  -2,  10/
      DATA IOPERA( 198),IX( 198),IY( 198)/'DRAW',   6,  -9/
      DATA IOPERA( 199),IX( 199),IY( 199)/'MOVE',   0,   5/
      DATA IOPERA( 200),IX( 200),IY( 200)/'DRAW',  -6,  -9/
!
      DATA IXMIND(  11)/  -8/
      DATA IXMAXD(  11)/   8/
      DATA IXDELD(  11)/  16/
      DATA ISTARD(  11)/ 194/
      DATA NUMCOO(  11)/   7/
!
!     DEFINE CHARACTER    638--LOWER CASE MU
!
      DATA IOPERA( 201),IX( 201),IY( 201)/'MOVE',  -3,   5/
      DATA IOPERA( 202),IX( 202),IY( 202)/'DRAW',  -9, -16/
      DATA IOPERA( 203),IX( 203),IY( 203)/'MOVE',  -4,   1/
      DATA IOPERA( 204),IX( 204),IY( 204)/'DRAW',  -5,  -4/
      DATA IOPERA( 205),IX( 205),IY( 205)/'DRAW',  -5,  -7/
      DATA IOPERA( 206),IX( 206),IY( 206)/'DRAW',  -3,  -9/
      DATA IOPERA( 207),IX( 207),IY( 207)/'DRAW',  -1,  -9/
      DATA IOPERA( 208),IX( 208),IY( 208)/'DRAW',   1,  -8/
      DATA IOPERA( 209),IX( 209),IY( 209)/'DRAW',   3,  -6/
      DATA IOPERA( 210),IX( 210),IY( 210)/'DRAW',   5,  -2/
      DATA IOPERA( 211),IX( 211),IY( 211)/'MOVE',   7,   5/
      DATA IOPERA( 212),IX( 212),IY( 212)/'DRAW',   5,  -2/
      DATA IOPERA( 213),IX( 213),IY( 213)/'DRAW',   4,  -6/
      DATA IOPERA( 214),IX( 214),IY( 214)/'DRAW',   4,  -8/
      DATA IOPERA( 215),IX( 215),IY( 215)/'DRAW',   5,  -9/
      DATA IOPERA( 216),IX( 216),IY( 216)/'DRAW',   7,  -9/
      DATA IOPERA( 217),IX( 217),IY( 217)/'DRAW',   9,  -7/
      DATA IOPERA( 218),IX( 218),IY( 218)/'DRAW',  10,  -5/
!
      DATA IXMIND(  12)/ -10/
      DATA IXMAXD(  12)/  11/
      DATA IXDELD(  12)/  21/
      DATA ISTARD(  12)/ 201/
      DATA NUMCOO(  12)/  18/
!
!     DEFINE CHARACTER    639--LOWER CASE NU
!
      DATA IOPERA( 219),IX( 219),IY( 219)/'MOVE',  -6,   5/
      DATA IOPERA( 220),IX( 220),IY( 220)/'DRAW',  -3,   5/
      DATA IOPERA( 221),IX( 221),IY( 221)/'DRAW',  -4,  -1/
      DATA IOPERA( 222),IX( 222),IY( 222)/'DRAW',  -5,  -6/
      DATA IOPERA( 223),IX( 223),IY( 223)/'DRAW',  -6,  -9/
      DATA IOPERA( 224),IX( 224),IY( 224)/'MOVE',   7,   5/
      DATA IOPERA( 225),IX( 225),IY( 225)/'DRAW',   6,   2/
      DATA IOPERA( 226),IX( 226),IY( 226)/'DRAW',   5,   0/
      DATA IOPERA( 227),IX( 227),IY( 227)/'DRAW',   3,  -3/
      DATA IOPERA( 228),IX( 228),IY( 228)/'DRAW',   0,  -6/
      DATA IOPERA( 229),IX( 229),IY( 229)/'DRAW',  -3,  -8/
      DATA IOPERA( 230),IX( 230),IY( 230)/'DRAW',  -6,  -9/
!
      DATA IXMIND(  13)/  -9/
      DATA IXMAXD(  13)/   9/
      DATA IXDELD(  13)/  18/
      DATA ISTARD(  13)/ 219/
      DATA NUMCOO(  13)/  12/
!
!     DEFINE CHARACTER    640--LOWER CASE XI
!
      DATA IOPERA( 231),IX( 231),IY( 231)/'MOVE',   2,  12/
      DATA IOPERA( 232),IX( 232),IY( 232)/'DRAW',   0,  11/
      DATA IOPERA( 233),IX( 233),IY( 233)/'DRAW',  -1,  10/
      DATA IOPERA( 234),IX( 234),IY( 234)/'DRAW',  -1,   9/
      DATA IOPERA( 235),IX( 235),IY( 235)/'DRAW',   0,   8/
      DATA IOPERA( 236),IX( 236),IY( 236)/'DRAW',   3,   7/
      DATA IOPERA( 237),IX( 237),IY( 237)/'DRAW',   6,   7/
      DATA IOPERA( 238),IX( 238),IY( 238)/'MOVE',   3,   7/
      DATA IOPERA( 239),IX( 239),IY( 239)/'DRAW',   0,   6/
      DATA IOPERA( 240),IX( 240),IY( 240)/'DRAW',  -2,   5/
      DATA IOPERA( 241),IX( 241),IY( 241)/'DRAW',  -3,   3/
      DATA IOPERA( 242),IX( 242),IY( 242)/'DRAW',  -3,   1/
      DATA IOPERA( 243),IX( 243),IY( 243)/'DRAW',  -1,  -1/
      DATA IOPERA( 244),IX( 244),IY( 244)/'DRAW',   2,  -2/
      DATA IOPERA( 245),IX( 245),IY( 245)/'DRAW',   4,  -2/
      DATA IOPERA( 246),IX( 246),IY( 246)/'MOVE',   2,  -2/
      DATA IOPERA( 247),IX( 247),IY( 247)/'DRAW',  -2,  -3/
      DATA IOPERA( 248),IX( 248),IY( 248)/'DRAW',  -4,  -4/
      DATA IOPERA( 249),IX( 249),IY( 249)/'DRAW',  -5,  -6/
      DATA IOPERA( 250),IX( 250),IY( 250)/'DRAW',  -5,  -8/
      DATA IOPERA( 251),IX( 251),IY( 251)/'DRAW',  -3, -10/
      DATA IOPERA( 252),IX( 252),IY( 252)/'DRAW',   1, -12/
      DATA IOPERA( 253),IX( 253),IY( 253)/'DRAW',   2, -13/
      DATA IOPERA( 254),IX( 254),IY( 254)/'DRAW',   2, -15/
      DATA IOPERA( 255),IX( 255),IY( 255)/'DRAW',   0, -16/
      DATA IOPERA( 256),IX( 256),IY( 256)/'DRAW',  -2, -16/
!
      DATA IXMIND(  14)/  -8/
      DATA IXMAXD(  14)/   8/
      DATA IXDELD(  14)/  16/
      DATA ISTARD(  14)/ 231/
      DATA NUMCOO(  14)/  26/
!
!     DEFINE CHARACTER    641--LOWER CASE OMIC
!
      DATA IOPERA( 257),IX( 257),IY( 257)/'MOVE',   0,   5/
      DATA IOPERA( 258),IX( 258),IY( 258)/'DRAW',  -2,   4/
      DATA IOPERA( 259),IX( 259),IY( 259)/'DRAW',  -4,   2/
      DATA IOPERA( 260),IX( 260),IY( 260)/'DRAW',  -5,  -1/
      DATA IOPERA( 261),IX( 261),IY( 261)/'DRAW',  -5,  -4/
      DATA IOPERA( 262),IX( 262),IY( 262)/'DRAW',  -4,  -7/
      DATA IOPERA( 263),IX( 263),IY( 263)/'DRAW',  -3,  -8/
      DATA IOPERA( 264),IX( 264),IY( 264)/'DRAW',  -1,  -9/
      DATA IOPERA( 265),IX( 265),IY( 265)/'DRAW',   1,  -9/
      DATA IOPERA( 266),IX( 266),IY( 266)/'DRAW',   3,  -8/
      DATA IOPERA( 267),IX( 267),IY( 267)/'DRAW',   5,  -6/
      DATA IOPERA( 268),IX( 268),IY( 268)/'DRAW',   6,  -3/
      DATA IOPERA( 269),IX( 269),IY( 269)/'DRAW',   6,   0/
      DATA IOPERA( 270),IX( 270),IY( 270)/'DRAW',   5,   3/
      DATA IOPERA( 271),IX( 271),IY( 271)/'DRAW',   4,   4/
      DATA IOPERA( 272),IX( 272),IY( 272)/'DRAW',   2,   5/
      DATA IOPERA( 273),IX( 273),IY( 273)/'DRAW',   0,   5/
!
      DATA IXMIND(  15)/  -8/
      DATA IXMAXD(  15)/   9/
      DATA IXDELD(  15)/  17/
      DATA ISTARD(  15)/ 257/
      DATA NUMCOO(  15)/  17/
!
!     DEFINE CHARACTER    642--LOWER CASE PI
!
      DATA IOPERA( 274),IX( 274),IY( 274)/'MOVE',  -2,   5/
      DATA IOPERA( 275),IX( 275),IY( 275)/'DRAW',  -6,  -9/
      DATA IOPERA( 276),IX( 276),IY( 276)/'MOVE',   3,   5/
      DATA IOPERA( 277),IX( 277),IY( 277)/'DRAW',   4,  -1/
      DATA IOPERA( 278),IX( 278),IY( 278)/'DRAW',   5,  -6/
      DATA IOPERA( 279),IX( 279),IY( 279)/'DRAW',   6,  -9/
      DATA IOPERA( 280),IX( 280),IY( 280)/'MOVE',  -9,   2/
      DATA IOPERA( 281),IX( 281),IY( 281)/'DRAW',  -7,   4/
      DATA IOPERA( 282),IX( 282),IY( 282)/'DRAW',  -4,   5/
      DATA IOPERA( 283),IX( 283),IY( 283)/'DRAW',   9,   5/
!
      DATA IXMIND(  16)/ -11/
      DATA IXMAXD(  16)/  11/
      DATA IXDELD(  16)/  22/
      DATA ISTARD(  16)/ 274/
      DATA NUMCOO(  16)/  10/
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
   51 FORMAT('***** AT THE BEGINNING OF DGSL1--')
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
 9011 FORMAT('***** AT THE END       OF DGSL1--')
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
      END SUBROUTINE DGSL1
      SUBROUTINE DGSL2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,   &
      IBUGD2,IFOUND,IERROR)
!
!     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
!              FOR GREEK SIMPLEX LOWER CASE (PART 2).
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-921-3651
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--87/4
!     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MARCH     1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --MARCH     1987.
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
!     DEFINE CHARACTER    643--LOWER CASE RHO
!
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',  -5,  -1/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',  -5,  -4/
      DATA IOPERA(   3),IX(   3),IY(   3)/'DRAW',  -4,  -7/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',  -3,  -8/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',  -1,  -9/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',   1,  -9/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',   3,  -8/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',   5,  -6/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',   6,  -3/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',   6,   0/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',   5,   3/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',   4,   4/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',   2,   5/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',   0,   5/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',  -2,   4/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',  -4,   2/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',  -5,  -1/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',  -9, -16/
!
      DATA IXMIND(  17)/  -9/
      DATA IXMAXD(  17)/   9/
      DATA IXDELD(  17)/  18/
      DATA ISTARD(  17)/   1/
      DATA NUMCOO(  17)/  18/
!
!     DEFINE CHARACTER    644--LOWER CASE SIGM
!
      DATA IOPERA(  19),IX(  19),IY(  19)/'MOVE',   9,   5/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',  -1,   5/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',  -3,   4/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',  -5,   2/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',  -6,  -1/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',  -6,  -4/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',  -5,  -7/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',  -4,  -8/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',  -2,  -9/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',   0,  -9/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',   2,  -8/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',   4,  -6/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',   5,  -3/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',   5,   0/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',   4,   3/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',   3,   4/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',   1,   5/
!
      DATA IXMIND(  18)/  -9/
      DATA IXMAXD(  18)/  11/
      DATA IXDELD(  18)/  20/
      DATA ISTARD(  18)/  19/
      DATA NUMCOO(  18)/  17/
!
!     DEFINE CHARACTER    645--LOWER CASE TAU
!
      DATA IOPERA(  36),IX(  36),IY(  36)/'MOVE',   1,   5/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',  -2,  -9/
      DATA IOPERA(  38),IX(  38),IY(  38)/'MOVE',  -8,   2/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',  -6,   4/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',  -3,   5/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',   8,   5/
!
      DATA IXMIND(  19)/ -10/
      DATA IXMAXD(  19)/  10/
      DATA IXDELD(  19)/  20/
      DATA ISTARD(  19)/  36/
      DATA NUMCOO(  19)/   6/
!
!     DEFINE CHARACTER    646--LOWER CASE UPSI
!
      DATA IOPERA(  42),IX(  42),IY(  42)/'MOVE',  -9,   1/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',  -8,   3/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',  -6,   5/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',  -4,   5/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW',  -3,   4/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',  -3,   2/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',  -5,  -4/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',  -5,  -7/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW',  -3,  -9/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW',  -1,  -9/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',   2,  -8/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',   4,  -6/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',   6,  -2/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',   7,   2/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',   7,   5/
!
      DATA IXMIND(  20)/ -10/
      DATA IXMAXD(  20)/  10/
      DATA IXDELD(  20)/  20/
      DATA ISTARD(  20)/  42/
      DATA NUMCOO(  20)/  15/
!
!     DEFINE CHARACTER    647--LOWER CASE PHI
!
      DATA IOPERA(  57),IX(  57),IY(  57)/'MOVE',  -3,   4/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',  -5,   3/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',  -7,   1/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW',  -8,  -2/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',  -8,  -5/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',  -7,  -7/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',  -6,  -8/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',  -4,  -9/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',  -1,  -9/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW',   2,  -8/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',   5,  -6/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',   7,  -3/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',   8,   0/
      DATA IOPERA(  70),IX(  70),IY(  70)/'DRAW',   8,   3/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',   6,   5/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',   4,   5/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',   2,   3/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',   0,  -1/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',  -2,  -6/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',  -5, -16/
!
      DATA IXMIND(  21)/ -11/
      DATA IXMAXD(  21)/  11/
      DATA IXDELD(  21)/  22/
      DATA ISTARD(  21)/  57/
      DATA NUMCOO(  21)/  20/
!
!     DEFINE CHARACTER    648--LOWER CASE CHI
!
      DATA IOPERA(  77),IX(  77),IY(  77)/'MOVE',  -7,   5/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',  -5,   5/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',  -3,   3/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',   3, -14/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',   5, -16/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',   7, -16/
      DATA IOPERA(  83),IX(  83),IY(  83)/'MOVE',   8,   5/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW',   7,   3/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',   5,   0/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',  -5, -11/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',  -7, -14/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',  -8, -16/
!
      DATA IXMIND(  22)/  -9/
      DATA IXMAXD(  22)/   9/
      DATA IXDELD(  22)/  18/
      DATA ISTARD(  22)/  77/
      DATA NUMCOO(  22)/  12/
!
!     DEFINE CHARACTER    649--LOWER CASE PSI
!
      DATA IOPERA(  89),IX(  89),IY(  89)/'MOVE',   4,  12/
      DATA IOPERA(  90),IX(  90),IY(  90)/'DRAW',  -4, -16/
      DATA IOPERA(  91),IX(  91),IY(  91)/'MOVE', -11,   1/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW', -10,   3/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',  -8,   5/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',  -6,   5/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',  -5,   4/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',  -5,   2/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',  -6,  -3/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',  -6,  -6/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',  -5,  -8/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',  -3,  -9/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',  -1,  -9/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',   2,  -8/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',   4,  -6/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',   6,  -3/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',   8,   2/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',   9,   5/
!
      DATA IXMIND(  23)/ -12/
      DATA IXMAXD(  23)/  11/
      DATA IXDELD(  23)/  23/
      DATA ISTARD(  23)/  89/
      DATA NUMCOO(  23)/  18/
!
!     DEFINE CHARACTER    650--LOWER CASE OMEG
!
      DATA IOPERA( 107),IX( 107),IY( 107)/'MOVE',  -4,   5/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',  -6,   4/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',  -8,   1/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',  -9,  -2/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',  -9,  -5/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',  -8,  -8/
      DATA IOPERA( 113),IX( 113),IY( 113)/'DRAW',  -7,  -9/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',  -5,  -9/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',  -3,  -8/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',  -1,  -5/
      DATA IOPERA( 117),IX( 117),IY( 117)/'MOVE',   0,  -1/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',  -1,  -5/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',   0,  -8/
      DATA IOPERA( 120),IX( 120),IY( 120)/'DRAW',   1,  -9/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW',   3,  -9/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW',   5,  -8/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',   7,  -5/
      DATA IOPERA( 124),IX( 124),IY( 124)/'DRAW',   8,  -2/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',   8,   1/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW',   7,   4/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW',   6,   5/
!
      DATA IXMIND(  24)/ -12/
      DATA IXMAXD(  24)/  11/
      DATA IXDELD(  24)/  23/
      DATA ISTARD(  24)/ 107/
      DATA NUMCOO(  24)/  21/
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
   51 FORMAT('***** AT THE BEGINNING OF DGSL2--')
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
 9011 FORMAT('***** AT THE END       OF DGSL2--')
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
      END SUBROUTINE DGSL2
      SUBROUTINE DIFF(IORD,X0,XMIN,XMAX,F,EPS,ACC,DERIV,ERROR,IFAIL)
!
!             NUMERICAL DIFFERENTIATION OF USER DEFINED FUNCTION
!
!                         DAVID KAHANER, NBS (GAITHERSBURG)
!
!  THE PROCEDURE DIFFERENTIATE CALCULATES THE FIRST, SECOND OR
!   THIRD ORDER DERIVATIVE OF A FUNCTION BY USING NEVILLE'S PROCESS TO
!   EXTRAPOLATE FROM A SEQUENCE OF SIMPLE POLYNOMIAL APPROXIMATIONS BASED ON
!   INTERPOLATING POINTS DISTRIBUTED SYMMETRICALLY ABOUT X0 (OR LYING ONLY ON
!   ONE SIDE OF X0 SHOULD THIS BE NECESSARY).  IF THE SPECIFIED TOLERANCE IS
!   NON-ZERO THEN THE PROCEDURE ATTEMPTS TO SATISFY THIS ABSOLUTE OR RELATIVE
!   ACCURACY REQUIREMENT, WHILE IF IT IS UNSUCCESSFUL OR IF THE TOLERANCE IS
!   SET TO ZERO THEN THE RESULT HAVING THE MINIMUM ACHIEVABLE ESTIMATED ERROR
!   IS RETURNED INSTEAD.
!
! INPUT PARAMETERS:
! IORD = 1, 2 OR 3 SPECIFIES THAT THE FIRST, SECOND OR THIRD ORDER
!   DERIVATIVE,RESPECTIVELY, IS REQUIRED.
! X0 IS THE POINT AT WHICH THE DERIVATIVE OF THE FUNCTION IS TO BE CALCULATED.
! XMIN, XMAX RESTRICT THE INTERPOLATING POINTS TO LIE IN [XMIN, XMAX], WHICH
!   SHOULD BE THE LARGEST INTERVAL INCLUDING X0 IN WHICH THE FUNCTION IS
!   CALCULABLE AND CONTINUOUS.
! F, A REAL PROCEDURE SUPPLIED BY THE USER, MUST YIELD THE VALUE OF THE
!   FUNCTION AT X FOR ANY X IN [XMIN, XMAX] WHEN CALLED BY F(X).
! EPS DENOTES THE TOLERANCE, EITHER ABSOLUTE OR RELATIVE.  EPS=0 SPECIFIES THAT
!   THE ERROR IS TO BE MINIMISED, WHILE EPS>0 OR EPS<0 SPECIFIES THAT THE
!   ABSOLUTE OR RELATIVE ERROR, RESPECTIVELY, MUST NOT EXCEED ABS(EPS) IF
!   POSSIBLE.  THE ACCURACY REQUIREMENT SHOULD NOT BE MADE STRICTER THAN
!   NECESSARY, SINCE THE AMOUNT OF COMPUTATION TENDS TO INCREASE AS
!   THE MAGNITUDE OF EPS DECREASES, AND IS PARTICULARLY HIGH WHEN EPS=0.
! ACC DENOTES THAT THE ABSOLUTE (ACC>0) OR RELATIVE (ACC<0) ERRORS IN THE
!   COMPUTED VALUES OF THE FUNCTION ARE MOST UNLIKELY TO EXCEED ABS(ACC), WHICH
!   SHOULD BE AS SMALL AS POSSIBLE.  IF THE USER CANNOT ESTIMATE ACC WITH
!   COMPLETE CONFIDENCE, THEN IT SHOULD BE SET TO ZERO.
!
! OUTPUT PARAMETERS:
! DERIV IS THE CALCULATED VALUE OF THE DERIVATIVE.
! ERROR IS AN ESTIMATED UPPER BOUND ON THE MAGNITUDE OF THE ABSOLUTE ERROR IN
!   THE CALCULATED RESULT.  IT SHOULD ALWAYS BE EXAMINED, SINCE IN EXTREME CASE
!   MAY INDICATE THAT THERE ARE NO CORRECT SIGNIFICANT DIGITS IN THE VALUE
!   RETURNED FOR DERIVATIVE.
! IFAIL WILL HAVE ONE OF THE FOLLOWING VALUES ON EXIT:
!   0   THE PROCEDURE WAS SUCCESSFUL.
!   1   THE ESTIMATED ERROR IN THE RESULT EXCEEDS THE (NON-ZERO) REQUESTED
!          ERROR, BUT THE MOST ACCURATE RESULT POSSIBLE HAS BEEN RETURNED.
!   2   INPUT DATA INCORRECT (DERIVATIVE AND ERROR WILL BE UNDEFINED).
!   3   THE INTERVAL [XMIN, XMAX] IS TOO SMALL (DERIVATIVE AND ERROR WILL BE
!          UNDEFINED);
!
      EXTERNAL F
      REAL X0,XMIN,XMAX,ACC,DERIV,ERROR,BETA,BETA4,H,H0,H1,H2,   &
      NEWH1,NEWH2,HEVAL,HPREV,BASEH,HACC1,HACC2,NHACC1,   &
      NHACC2,MINH,MAXH,MAXH1,MAXH2,TDERIV,F0,TWOF0,F1,F2,F3,F4,FMAX,   &
      MAXFUN,PMAXF,DF1,DELTAF,PDELTA,Z,ZPOWER,C0F0,C1,C2,C3,DNEW,DPREV,   &
      RE,TE,NEWERR,TEMERR,NEWACC,PACC1,PACC2,FACC1,FACC2,ACC0,   &
      ACC1,ACC2,RELACC,TWOINF,TWOSUP,S,   &
      D(10),DENOM(10),E(10),MINERR(10),MAXF(0:10),SAVE(0:13),   &
      STOREF(-45:45),FACTOR
!
      INTEGER IORD,IFAIL,ETA,INF,SUP,I,J,K,N,NMAX,METHOD,SIGNH,FCOUNT,   &
      INIT
      LOGICAL IGNORE(10),CONTIN,SAVED
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!
! ETA IS THE MINIMUM NUMBER OF SIGNIFICANT BINARY DIGITS (APART FROM THE
! SIGN DIGIT) USED TO REPRESENT THE MANTISSA OF REAL NUMBERS. IT SHOULD
! BE DEVREASED BY ONE IF THE COMPUTER TRUNCATES RATHER THAN ROUNDS.
! INF, SUP ARE THE LARGEST POSSIBLE POSITIVE INTEGERS SUBJECT TO
! 2**(-INF), -2**(-INF), 2**SUP, AND -2**SUP ALL BEING REPRESENTABLE REAL
! NUMBERS.
      DO 2 I=0,13
         SAVE(I)=0.0
    2 CONTINUE
      NEWACC=0.0
      PMAXF=0.0
      PDELTA=0.0
      DELTAF=0.0
      F2=0.0
      F3=0.0
      F4=0.0
      C1=0.0
      C2=0.0
      C3=0.0
      C0F0=0.0
      TEMERR=0.0
      TDERIV=0.0
      MAXH=0.0
      HEVAL=0.0
      BETA4=0.0
      BETA=0.0
      BASEH=0.0
      MAXFUN=0
      J=0
      SAVED=.FALSE.
!
      ETA=I1MACH(11) - 1
      INF=-I1MACH(12) - 2
      SUP=I1MACH(13)-1
      IF(IORD.LT.1 .OR. IORD.GT.3 .OR. XMAX.LE.XMIN .OR.   &
        X0.GT.XMAX .OR. X0.LT.XMIN) THEN
          IFAIL = 2
          RETURN
      ENDIF
!
      TWOINF = 2.**(-INF)
      TWOSUP = 2.**SUP
      FACTOR = 2**(FLOAT((INF+SUP))/30.)
      IF(FACTOR.LT.256.)FACTOR=256.
      MAXH1 = XMAX - X0
      SIGNH = 1
      IF(X0-XMIN .LE. MAXH1)THEN
          MAXH2 = X0 - XMIN
      ELSE
          MAXH2 = MAXH1
          MAXH1 = X0 - XMIN
          SIGNH = -1
      ENDIF
      RELACC = 2.**(1-ETA)
      MAXH1 = (1.-RELACC)*MAXH1
      MAXH2 = (1.-RELACC)*MAXH2
      S=128.*TWOINF
      IF(ABS(X0).GT.128.*TWOINF*2.**ETA) S = ABS(X0)*2.**(-ETA)
      IF(MAXH1.LT.S)THEN
!         INTERVAL TOO SMALL
          IFAIL =3
          RETURN
      ENDIF
      IF(ACC.LT.0.) THEN
          IF(-ACC.GT.RELACC)RELACC = -ACC
          ACC = 0.
      ENDIF
!
!     DETERMINE THE SMALLEST SPACING AT WHICH THE CALCULATED
!     FUNCTION VALUES ARE UNEQUAL NEAR X0.
!
      F0 = F(X0)
      TWOF0 = F0 + F0
      IF(ABS(X0) .GT. TWOINF*2.**ETA) THEN
          H = ABS(X0)*2.**(-ETA)
          Z = 2.
      ELSE
          H = TWOINF
          Z = 64.
      ENDIF
      DF1 = F(X0+SIGNH*H) - F0
   80 IF(DF1 .NE. 0. .OR. Z*H .GT. MAXH1) GO TO  100
      H = Z*H
      DF1 = F(X0+SIGNH*H) - F0
      IF(Z .NE.2.) THEN
          IF(DF1 .NE. 0.) THEN
              H = H/Z
              Z = 2.
              DF1 = 0.
          ELSE
              IF(Z*H .GT. MAXH1) Z = 2.
          ENDIF
      ENDIF
      GO TO  80
  100 CONTINUE
!
      IF(DF1 .EQ. 0.) THEN
!         CONSTANT FUNCTION
          DERIV = 0.
          ERROR = 0.
          IFAIL = 0
          RETURN
      ENDIF
      IF(H .GT. MAXH1/128.) THEN
!         MINIMUM H TOO LARGE
          IFAIL = 3
          RETURN
      ENDIF
!
      H = 8.*H
      H1 = SIGNH*H
      H0 = H1
      H2 = -H1
      MINH = 2.**(-MIN(INF,SUP)/IORD)
      IF(MINH.LT.H) MINH = H
      IF(IORD.EQ.1) S = 8.
      IF(IORD.EQ.2) S = 9.*SQRT(3.)
      IF(IORD.EQ.3) S = 27.
      IF(MINH.GT.MAXH1/S) THEN
          IFAIL = 3
          RETURN
      ENDIF
      IF(MINH.GT.MAXH2/S .OR. MAXH2.LT.128.*TWOINF) THEN
          METHOD = 1
      ELSE
          METHOD = 2
      ENDIF
!
!     METHOD 1 USES 1-SIDED FORMULAE, AND METHOD 2 SYMMETRIC.
!         NOW ESTIMATE ACCURACY OF CALCULATED FUNCTION VALUES.
!
      IF(METHOD.NE.2 .OR. IORD.EQ.2) THEN
          IF(X0.NE.0.) THEN
              CALL FACCUR(0.,-H1,ACC0,X0,F,TWOINF,F0,F1)
          ELSE
              ACC0 = 0.
          ENDIF
      ENDIF
!
      IF(ABS(H1) .GT. TWOSUP/128.) THEN
          HACC1 = TWOSUP
      ELSE
          HACC1 = 128.*H1
      ENDIF
!
      IF(ABS(HACC1)/4. .LT. MINH) THEN
          HACC1 = 4.*SIGNH*MINH
      ELSEIF(ABS(HACC1) .GT. MAXH1) THEN
          HACC1 = SIGNH*MAXH1
      ENDIF
      F1 = F(X0+HACC1)
      CALL FACCUR(HACC1,H1,ACC1,X0,F,TWOINF,F0,F1)
      IF(METHOD.EQ.2) THEN
          HACC2 = -HACC1
          IF(ABS(HACC2) .GT. MAXH2) HACC2 = -SIGNH * MAXH2
          F1 = F(X0 + HACC2)
          CALL FACCUR(HACC2,H2,ACC2,X0,F,TWOINF,F0,F1)
      ENDIF
      NMAX = 8
      IF(ETA.GT.36) NMAX = 10
      N = -1
      FCOUNT = 0
      DERIV = 0.
      ERROR = TWOSUP
      INIT = 3
      CONTIN = .TRUE.
!
  130 CONTINUE
      N = N+1
      IF(.NOT. CONTIN) GO TO  800
!
      IF(INIT.EQ.3) THEN
!         CALCULATE COEFFICIENTS FOR DIFFERENTIATION FORMULAE
!             AND NEVILLE EXTRAPOLATION ALGORITHM
          IF(IORD.EQ.1) THEN
              BETA=2.
          ELSEIF(METHOD.EQ.2)THEN
              BETA = SQRT(2.)
          ELSE
              BETA = SQRT(3.)
          ENDIF
          BETA4 = BETA**4.
          Z = BETA
          IF(METHOD.EQ.2) Z = Z**2
          ZPOWER = 1.
          DO 150 K = 1,NMAX
              ZPOWER = Z*ZPOWER
              DENOM(K) = ZPOWER-1
  150     CONTINUE
          IF(METHOD.EQ.2 .AND. IORD.EQ.1) THEN
              E(1) = 5.
              E(2) = 6.3
              DO 160 I = 3,NMAX
                  E(I) = 6.81
  160         CONTINUE
        ELSEIF((METHOD.NE.2.AND.IORD.EQ.1) .OR. (METHOD.EQ.2.AND.   &
                  IORD.EQ.2)) THEN
              E(1) = 10.
              E(2) = 16.
              E(3) = 20.36
              E(4) = 23.
              E(5) = 24.46
              DO 165 I = 6,NMAX
                  E(I) = 26.
  165         CONTINUE
              IF(METHOD.EQ.2.AND.IORD.EQ.2) THEN
                  DO 170 I = 1,NMAX
                       E(I)=2*E(I)
  170             CONTINUE
              ENDIF
          ELSEIF(METHOD.NE.2.AND.IORD.EQ.2) THEN
              E(1) = 17.78
              E(2) = 30.06
              E(3) = 39.66
              E(4) = 46.16
              E(5) = 50.26
              DO 175 I = 6,NMAX
                  E(I) = 55.
  175         CONTINUE
          ELSEIF(METHOD.EQ.2.AND.IORD.EQ.3) THEN
              E(1) = 25.97
              E(2) = 41.22
              E(3) = 50.95
              E(4) = 56.4
              E(5) = 59.3
              DO 180 I = 6,NMAX
                  E(I) = 62.
  180         CONTINUE
          ELSE
              E(1) = 24.5
              E(2) = 40.4
              E(3) = 52.78
              E(4) = 61.2
              E(5) = 66.55
              DO 185 I = 6,NMAX
                  E(I) = 73.
  185         CONTINUE
              C0F0 = -TWOF0/(3.*BETA)
              C1 = 3./(3.*BETA-1.)
              C2 = -1./(3.*(BETA-1.))
              C3 = 1./(3.*BETA*(5.-2.*BETA))
          ENDIF
      ENDIF
!
!
      IF(INIT.GE.2) THEN
!         INITIALIZATION OF STEPLENGTHS, ACCURACY AND OTHER
!             PARAMETERS
!
          HEVAL = SIGNH*MINH
          H = HEVAL
          BASEH = HEVAL
          MAXH = MAXH2
          IF(METHOD.EQ.1)MAXH = MAXH1
          DO 300 K = 1,NMAX
              MINERR(K) = TWOSUP
              IGNORE(K) = .FALSE.
  300     CONTINUE
          IF(METHOD.EQ.1) NEWACC = ACC1
          IF(METHOD.EQ.-1) NEWACC = ACC2
          IF(METHOD.EQ.2) NEWACC = (ACC1+ACC2)/2.
          IF(NEWACC.LT.ACC) NEWACC = ACC
          IF((METHOD.NE.2 .OR. IORD.EQ.2) .AND. NEWACC.LT.ACC0)   &
                  NEWACC = ACC0
          IF(METHOD.NE.-1) THEN
              FACC1 = ACC1
              NHACC1 = HACC1
              NEWH1 = H1
          ENDIF
          IF(METHOD.NE.1) THEN
              FACC2 = ACC2
              NHACC2 = HACC2
              NEWH2 = H2
          ELSE
              FACC2 = 0.
              NHACC2 = 0.
          ENDIF
          INIT = 1
          J = 0
          SAVED = .FALSE.
      ENDIF
!
!     CALCULATE NEW OR INITIAL FUNCTION VALUES
!
      IF(INIT.EQ.1 .AND. (N.EQ.0 .OR. IORD.EQ.1) .AND.   &
              .NOT.(METHOD.EQ.2 .AND. FCOUNT.GE.45)) THEN
          IF(METHOD.EQ.2) THEN
              FCOUNT = FCOUNT + 1
              F1 = F(X0+HEVAL)
              STOREF(FCOUNT) = F1
              F2 = F(X0-HEVAL)
              STOREF(-FCOUNT) = F2
          ELSE
              J = J+1
              IF(J.LE.FCOUNT) THEN
                  F1 = STOREF(J*METHOD)
              ELSE
                  F1 = F(X0+HEVAL)
              ENDIF
          ENDIF
      ELSE
          F1 = F(X0+HEVAL)
          IF(METHOD.EQ.2) F2 = F(X0-HEVAL)
      ENDIF
      IF(N.EQ.0) THEN
          IF(METHOD.EQ.2 .AND. IORD.EQ.3) THEN
              PDELTA = F1-F2
              PMAXF = (ABS(F1)+ABS(F2))/2.
              HEVAL = BETA*HEVAL
              F1 = F(X0+HEVAL)
              F2 = F(X0-HEVAL)
              DELTAF = F1-F2
              MAXFUN = (ABS(F1)+ABS(F2))/2.
              HEVAL = BETA*HEVAL
              F1 = F(X0+HEVAL)
              F2 = F(X0-HEVAL)
          ELSEIF(METHOD.NE.2 .AND. IORD.GE.2) THEN
              IF(IORD.EQ.2) THEN
                  F3 = F1
              ELSE
                  F4 = F1
                  HEVAL = BETA*HEVAL
                  F3 = F(X0+HEVAL)
              ENDIF
              HEVAL = BETA*HEVAL
              F2 = F(X0+HEVAL)
              HEVAL = BETA*HEVAL
              F1 = F(X0+HEVAL)
          ENDIF
      ENDIF
!
!     EVALUATE A NEW APPROXIMATION DNEW TO THE DERIVATIVE
!
      IF(N.GT.NMAX) THEN
          N = NMAX
          DO 400 I = 1,N
              MAXF(I-1) = MAXF(I)
  400     CONTINUE
      ENDIF
      IF(METHOD.EQ.2) THEN
          MAXF(N) = (ABS(F1)+ABS(F2))/2.
          IF(IORD.EQ.1) THEN
              DNEW = (F1-F2)/2.
          ELSEIF(IORD.EQ.2) THEN
              DNEW = F1+F2-TWOF0
          ELSE
              DNEW = -PDELTA
              PDELTA = DELTAF
              DELTAF = F1-F2
              DNEW = DNEW + .5*DELTAF
              IF(MAXF(N).LT.PMAXF) MAXF(N) = PMAXF
              PMAXF = MAXFUN
              MAXFUN = (ABS(F1)+ABS(F2))/2.
          ENDIF
      ELSE
          MAXF(N) = ABS(F1)
          IF(IORD.EQ.1) THEN
              DNEW = F1-F0
          ELSEIF(IORD.EQ.2) THEN
              DNEW = (TWOF0-3*F3+F1)/3.
              IF(MAXF(N).LT.ABS(F3)) MAXF(N) = ABS(F3)
              F3 = F2
              F2 = F1
          ELSE
              DNEW = C3*F1+C2*F2+C1*F4+C0F0
              IF(MAXF(N).LT.ABS(F2)) MAXF(N) = ABS(F2)
              IF(MAXF(N).LT.ABS(F4)) MAXF(N) = ABS(F4)
              F4 = F3
              F3 = F2
              F2 = F1
          ENDIF
      ENDIF
      IF(ABS(H).GT.1) THEN
          DNEW = DNEW/H**IORD
      ELSE
          IF(128.*ABS(DNEW).GT.TWOSUP*ABS(H)**IORD) THEN
              DNEW = TWOSUP/128.
          ELSE
              DNEW = DNEW/H**IORD
          ENDIF
      ENDIF
!
      IF(INIT.EQ.0) THEN
!         UPDATE ESTIMATED ACCURACY OF FUNCTION VALUES
          NEWACC = ACC
          IF((METHOD.NE.2 .OR. IORD.EQ.2) .AND. NEWACC.LT.ACC0)   &
              NEWACC = ACC0
          IF(METHOD.NE.-1 .AND. ABS(NHACC1).LE.1.125*ABS(HEVAL)/BETA4)   &
                     THEN
              NHACC1 = HEVAL
              PACC1 = FACC1
              CALL FACCUR(NHACC1,NEWH1,FACC1,X0,F,TWOINF,F0,F1)
              IF(FACC1.LT.PACC1) FACC1=(3*FACC1+PACC1)/4.
          ENDIF
          IF(METHOD.NE.1 .AND. ABS(NHACC2).LE.1.125*ABS(HEVAL)/BETA4)   &
                  THEN
              IF(METHOD.EQ.2) THEN
                  F1 = F2
                  NHACC2 = -HEVAL
              ELSE
                  NHACC2 = HEVAL
              ENDIF
              PACC2 = FACC2
              CALL FACCUR(NHACC2,NEWH2,FACC2,X0,F,TWOINF,F0,F1)
              IF(FACC2.LT.PACC2) FACC2 = (3*FACC2+PACC2)/4.
          ENDIF
          IF(METHOD.EQ.1 .AND. NEWACC.LT.FACC1) NEWACC = FACC1
          IF(METHOD.EQ.-1 .AND. NEWACC.LT.FACC2) NEWACC = FACC2
          IF(METHOD.EQ.2 .AND. NEWACC.LT.(FACC1+FACC2)/2.)   &
                  NEWACC = (FACC1+FACC2)/2.
      ENDIF
!
!     EVALUATE SUCCESSIVE ELEMENTS OF THE CURRENT ROW IN THE NEVILLE
!     ARRAY, ESTIMATING AND EXAMINING THE TRUNCATION AND ROUNDING
!     ERRORS IN EACH
!
      CONTIN = N.LT.NMAX
      HPREV = ABS(H)
      FMAX = MAXF(N)
      IF((METHOD.NE.2 .OR. IORD.EQ.2) .AND. FMAX.LT.ABS(F0))   &
              FMAX = ABS(F0)
!
      DO 500 K = 1,N
          DPREV = D(K)
          D(K) = DNEW
          DNEW = DPREV+(DPREV-DNEW)/DENOM(K)
          TE = ABS(DNEW-D(K))
          IF(FMAX.LT.MAXF(N-K)) FMAX = MAXF(N-K)
          HPREV = HPREV/BETA
          IF(NEWACC.GE.RELACC*FMAX) THEN
              RE = NEWACC*E(K)
          ELSE
              RE = RELACC*FMAX*E(K)
          ENDIF
          IF(RE.NE.0.) THEN
              IF(HPREV.GT.1) THEN
                  RE = RE/HPREV**IORD
              ELSEIF(2*RE.GT.TWOSUP*HPREV**IORD) THEN
                  RE = TWOSUP/2.
              ELSE
                  RE = RE/HPREV**IORD
              ENDIF
          ENDIF
          NEWERR = TE+RE
          IF(TE.GT.RE) NEWERR = 1.25*NEWERR
          IF(.NOT. IGNORE(K)) THEN
              IF((INIT.EQ.0 .OR. (K.EQ.2 .AND. .NOT.IGNORE(1)))   &
                      .AND. NEWERR.LT.ERROR) THEN
                  DERIV = D(K)
                  ERROR = NEWERR
              ENDIF
              IF(INIT.EQ.1 .AND. N.EQ.1) THEN
              TDERIV = D(1)
                  TEMERR = NEWERR
              ENDIF
              IF(MINERR(K).LT.TWOSUP/4) THEN
                  S = 4*MINERR(K)
              ELSE
                  S = TWOSUP
              ENDIF
              IF(TE.GT.RE .OR. NEWERR.GT.S) THEN
                  IGNORE(K) = .TRUE.
              ELSE
                  CONTIN = .TRUE.
              ENDIF
              IF(NEWERR.LT.MINERR(K)) MINERR(K) = NEWERR
              IF(INIT.EQ.1 .AND. N.EQ.2 .AND. K.EQ.1 .AND.   &
                      .NOT.IGNORE(1)) THEN
                  IF(NEWERR.LT.TEMERR) THEN
                      TDERIV = D(1)
                      TEMERR = NEWERR
                  ENDIF
                  IF(TEMERR.LT.ERROR) THEN
                      DERIV = TDERIV
                      ERROR = TEMERR
                  ENDIF
              ENDIF
          ENDIF
  500 CONTINUE
!
      IF(N.LT.NMAX) D(N+1) = DNEW
                 IF(EPS.LT.0.) THEN
          S = ABS(EPS*DERIV)
      ELSE
          S = EPS
      ENDIF
      IF(ERROR.LE.S) THEN
          CONTIN = .FALSE.
      ELSEIF(INIT.EQ.1 .AND. (N.EQ.2 .OR. IGNORE(1))) THEN
          IF((IGNORE(1) .OR. IGNORE(2)) .AND. SAVED) THEN
              SAVED = .FALSE.
              N = 2
              H = BETA * SAVE(0)
              HEVAL = BETA*SAVE(1)
              MAXF(0) = SAVE(2)
              MAXF(1) = SAVE(3)
              MAXF(2) = SAVE(4)
              D(1) = SAVE(5)
              D(2) = SAVE(6)
              D(3) = SAVE(7)
              MINERR(1) = SAVE(8)
              MINERR(2) = SAVE(9)
              IF(METHOD.EQ.2 .AND. IORD.EQ.3) THEN
                  PDELTA = SAVE(10)
                  DELTAF = SAVE(11)
                  PMAXF = SAVE(12)
                  MAXFUN = SAVE(13)
              ELSEIF(METHOD.NE.2 .AND. IORD.GE.2) THEN
                  F2 = SAVE(10)
                  F3 = SAVE(11)
                  IF(IORD.EQ.3) F4 = SAVE(12)
              ENDIF
              INIT = 0
              IGNORE(1) = .FALSE.
              IGNORE(2) = .FALSE.
          ELSEIF(.NOT. (IGNORE(1) .OR. IGNORE(2)) .AND. N.EQ.2   &
                  .AND. BETA4*FACTOR*ABS(HEVAL).LE.MAXH) THEN
!             SAVE ALL CURRENT VALUES IN CASE OF RETURN TO
!                 CURRENT POINT
              SAVED = .TRUE.
              SAVE(0) = H
              SAVE(1) = HEVAL
              SAVE(2) = MAXF(0)
              SAVE(3) = MAXF(1)
              SAVE(4) = MAXF(2)
              SAVE(5) = D(1)
              SAVE(6) = D(2)
              SAVE(7) = D(3)
              SAVE(8) = MINERR(1)
              SAVE(9) = MINERR (2)
              IF(METHOD.EQ.2 .AND. IORD.EQ.3) THEN
                  SAVE(10) = PDELTA
                  SAVE(11) = DELTAF
                  SAVE(12) = PMAXF
                  SAVE(13) = MAXFUN
              ELSEIF(METHOD.NE.2 .AND. IORD.GE.2) THEN
                  SAVE(10) = F2
                  SAVE(11) = F3
                  IF(IORD.EQ.3) SAVE(12) = F4
              ENDIF
              H = FACTOR*BASEH
              HEVAL = H
              BASEH = H
              N = -1
          ELSE
              INIT = 0
              H = BETA*H
              HEVAL = BETA*HEVAL
          ENDIF
      ELSEIF(CONTIN .AND. BETA*ABS(HEVAL).LE.MAXH) THEN
          H = BETA*H
          HEVAL = BETA*HEVAL
      ELSEIF(METHOD.NE.1) THEN
          CONTIN = .TRUE.
          IF(METHOD.EQ.2) THEN
              INIT = 3
              METHOD = -1
              IF(IORD.NE.2) THEN
                  IF(X0.NE.0.) THEN
                      CALL FACCUR(0.,-H0,ACC0,X0,F,TWOINF,F0,F1)
                  ELSE
                      ACC0 = 0.
                  ENDIF
              ENDIF
          ELSE
              INIT = 2
              METHOD = 1
          ENDIF
          N = -1
          SIGNH = -SIGNH
      ELSE
          CONTIN = .FALSE.
      ENDIF
      GO TO  130
  800 IF(EPS.LT.0.) THEN
          S = ABS(EPS*DERIV)
      ELSE
          S = EPS
      ENDIF
      IFAIL = 0
      IF(EPS.NE.0. .AND. ERROR.GT.S) IFAIL = 1
      RETURN
      END SUBROUTINE DIFF
      SUBROUTINE DIFFER(NDIM, A, B, WIDTH, Z, DIF, FUNCTN,   &
           DIVAXN, DIFCLS)
!
!     Compute fourth differences and subdivision axes
!
      EXTERNAL FUNCTN
      INTEGER I, NDIM, DIVAXN, DIFCLS
      DOUBLE PRECISION   &
           A(NDIM), B(NDIM), WIDTH(NDIM), Z(NDIM), DIF(NDIM), FUNCTN
      DOUBLE PRECISION FRTHDF, FUNCEN, WIDTHI
      DIFCLS = 0
      DIVAXN = MOD( DIVAXN, NDIM ) + 1
      IF ( NDIM .GT. 1 ) THEN
         DO 100 I = 1,NDIM
            DIF(I) = 0
            Z(I) = A(I) + WIDTH(I)
 100     CONTINUE
 10      FUNCEN = FUNCTN(NDIM, Z)
         DO 200 I = 1,NDIM
            WIDTHI = WIDTH(I)/5
            FRTHDF = 6*FUNCEN
            Z(I) = Z(I) - 4*WIDTHI
            FRTHDF = FRTHDF + FUNCTN(NDIM,Z)
            Z(I) = Z(I) + 2*WIDTHI
            FRTHDF = FRTHDF - 4*FUNCTN(NDIM,Z)
            Z(I) = Z(I) + 4*WIDTHI
            FRTHDF = FRTHDF - 4*FUNCTN(NDIM,Z)
            Z(I) = Z(I) + 2*WIDTHI
            FRTHDF = FRTHDF + FUNCTN(NDIM,Z)
!     Do not include differences below roundoff
            IF ( FUNCEN + FRTHDF/8 .NE. FUNCEN )   &
                 DIF(I) = DIF(I) + ABS(FRTHDF)*WIDTH(I)
            Z(I) = Z(I) - 4*WIDTHI
  200    CONTINUE
         DIFCLS = DIFCLS + 4*NDIM + 1
         DO 300 I = 1,NDIM
            Z(I) = Z(I) + 2*WIDTH(I)
            IF ( Z(I) .LT. B(I) ) GO TO 10
            Z(I) = A(I) + WIDTH(I)
  300    CONTINUE
         DO 400 I = 1,NDIM
            IF ( DIF(DIVAXN) .LT. DIF(I) ) DIVAXN = I
  400    CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DIFFER
      SUBROUTINE DIGITS(XVAL,IWRITE,XDIGI,NDIGI,ISUBRO,IBUGA3,IERROR)
!
!     PURPOSE--THIS SUBROUTINE RETURNS A VECTOR CONTAINING THE
!              DIGITS FROM THE POSITIVE INTEGER PART OF A NUMBER
!              (I.E., FOR NEGATIVE NUMBERS TAKE THE ABSOLUTE VALUE).
!     INPUT  ARGUMENTS--XVAL   = THE SINGLE PRECISION VALUE FOR WHICH
!                                THE DIGITS WILL BE EXTRACTED
!     OUTPUT ARGUMENTS--XDIGI  = THE SINGLE PRECISION VECTOR OF THE
!                                COMPUTED DIGITS
!                     --NDIGI  = THE INTEGER VALUE OF THE NUMBER OF
!                                DIGITS
!     OUTPUT--THE COMPUTED SINGLE PRECISION VECTOR OF THE
!             DIGITS FROM THE INTEGER PART OF THE NUMBER
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2015.1
!     ORIGINAL VERSION--JANUARY   2015.
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
      CHARACTER*1 IATEMP
      CHARACTER*20 IA
!
!---------------------------------------------------------------------
!
      DIMENSION XDIGI(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DIGI'
      ISUBN2='TS  '
      IERROR='NO'
      NDIGI=0
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'GITS')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DIGITS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,IWRITE,XVAL
   52   FORMAT('IBUGA3,ISUBRO,IWRITE,XVAL = ',3(A4,2X),G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ********************************************
!               **  STEP 1--                              **
!               **  EXTRACT INTEGER PART OF NUMBER        **
!               ********************************************
!
      EPS=0.0001
      XVALT=ABS(XVAL+EPS)
      IVAL=INT(XVALT)
!
!               ********************************************
!               **  STEP 2--                              **
!               **  EXTRACT THE DIGITS                    **
!               ********************************************
!
!
      IA=' '
      WRITE(IA(1:20),'(I20)')IVAL
!
      DO 100 I=1,20
        IATEMP=IA(I:I)
        IF(IATEMP.EQ.'1')THEN
          NDIGI=NDIGI+1
          XDIGI(NDIGI)=1.0
        ELSEIF(IATEMP.EQ.'2')THEN
          NDIGI=NDIGI+1
          XDIGI(NDIGI)=2.0
        ELSEIF(IATEMP.EQ.'3')THEN
          NDIGI=NDIGI+1
          XDIGI(NDIGI)=3.0
        ELSEIF(IATEMP.EQ.'4')THEN
          NDIGI=NDIGI+1
          XDIGI(NDIGI)=4.0
        ELSEIF(IATEMP.EQ.'5')THEN
          NDIGI=NDIGI+1
          XDIGI(NDIGI)=5.0
        ELSEIF(IATEMP.EQ.'6')THEN
          NDIGI=NDIGI+1
          XDIGI(NDIGI)=6.0
        ELSEIF(IATEMP.EQ.'7')THEN
          NDIGI=NDIGI+1
          XDIGI(NDIGI)=7.0
        ELSEIF(IATEMP.EQ.'8')THEN
          NDIGI=NDIGI+1
          XDIGI(NDIGI)=8.0
        ELSEIF(IATEMP.EQ.'9')THEN
          NDIGI=NDIGI+1
          XDIGI(NDIGI)=9.0
        ELSEIF(IATEMP.EQ.'0')THEN
          IF(NDIGI.GT.0)THEN
            NDIGI=NDIGI+1
            XDIGI(NDIGI)=0.0
          ENDIF
        ENDIF
  100 CONTINUE
!
      IF(NDIGI.EQ.0)THEN
        NDIGI=NDIGI+1
        XDIGI(NDIGI)=0.0
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'GITS')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DIGITS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NDIGI,IVAL,XVALT,IA
 9013   FORMAT('NDIGI,IVAL,XVALT,IA = ',2I8,G15.7,2X,A20)
        CALL DPWRST('XXX','BUG ')
        DO 9014 I=1,NDIGI
          WRITE(ICOUT,9015)I,XDIGI(I)
 9015     FORMAT('I,XDIGI(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
 9014   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DIGITS
      SUBROUTINE DIPERC(X,N,XPT,IWRITE,DIOUT,   &
                        IBUGA3,ISUBRO,IERROR)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE "PERCENTAGE DIFFERENCE"
!              STATISTIC Di% GIVEN IN ISO 13528 (P. 25):
!
!                 D(i)% = (X(i) - Xpt)/Xpt)*100
!
!              WHERE Xpt IS A CONSENSUS OR ASSIGNED VALUE.
!
!              THE D(i) = X(i) - Xpt OR D(i)% IS COMPARED TO
!              AN "ALLOWANCE FOR MEASUREMENT ERROR" VALUE
!              DeltaE.  THAT IS
!
!                -DeltaE < D(i) < DeltaE
!
!              THE PERCENTAGE VERSION IS TYPICALLY COMPARED TO
!              SOME TRANSFORMATION OF DeltaE.
!
!              NOTE THAT XPT AND DELTAE ARE NOT COMPUTED FROM THE
!              CURRENT DATA.  THE XPT IS CONSIDERED THE "TRUE" VALUE
!              (OR THE BEST GUESS OF THE TRUE VALUE).  THE ISO 13528
!              STANDARD DISCUSSES NUMEROUS WAYS OF DETERMINING THIS
!              VALUE.  THE DELTAE IS AN "ACCEPTABLE" ERROR.  THERE IS
!              NO STANDARD WAY FOR DETERMINING THIS.
!
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
!                                (UNSORTED OR SORTED) OBSERVATIONS.
!                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
!                                IN THE VECTOR X.
!                     --XPT    = THE SINGLE PRECISION VALUE CONTAINING
!                                THE ASSIGNED VALUE
!     OUTPUT ARGUMENTS--DIOUT  = THE SINGLE PRECISION VECTOR OF THE
!                                COMPUTED Di% VALUES.
!     OUTPUT--THE COMPUTED SINGLE PRECISION VECTOR OF THE SAMPLE Di%
!             VALUES.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCE--ISO 13528, SECOND EDITION, STATISTICAL METHODS FOR USE
!                IN PROFICIENCY TESTING BY INTERLABORATORY COMPARISONS,
!                2015, PP. 25.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2016.2
!     ORIGINAL VERSION--FEBRUARY  2016.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!---------------------------------------------------------------------
!
      DIMENSION X(*)
      DIMENSION DIOUT(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DIPE'
      ISUBN2='RC  '
!
      IERROR='NO'
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'PERC')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DIPERC--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,N,XPT
   52   FORMAT('IBUGA3,N,XPT = ',A4,2X,I8,G15.7)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ************************
!               **  COMPUTE DiPERC    **
!               ************************
!
!               ********************************************
!               **  STEP 1--                              **
!               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
!               ********************************************
!
      AN=N
!
      IF(N.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN DIPERC--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS IN THE RESPONSE ',   &
               'VARIABLE IS LESS THAN 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE NUMBER OF OBSERVATIONS = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               *****************************
!               **  STEP 2--               **
!               **  COMPUTE THE DIPERC     **
!               *****************************
!
      DO 200 I=1,N
        DIOUT(I)=((X(I) - XPT)/XPT)*100.
  200 CONTINUE
!
!               *******************************
!               **  STEP 3--                 **
!               **  WRITE OUT A LINE         **
!               **  OF SUMMARY INFORMATION.  **
!               *******************************
!
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,811)N
  811   FORMAT('THE NUMBER OF DI PERCENT VALUES GENERATED = ',I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'PERC')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DIPERC--')
        CALL DPWRST('XXX','BUG ')
        DO 9012 I=1,N
          WRITE(ICOUT,9015)I,X(I),DIOUT(I)
 9015     FORMAT('I,X(I),PAOUT(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9012   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DIPERC
      SUBROUTINE DISCDF(IX,N,CDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
!              FUNCTION VALUE FOR THE DISCRETE UNIFORM (RECTANGULAR)
!              DISTRIBUTION ON THE INTERVAL (0,N).
!              THIS DISTRIBUTION HAS MEAN = N/2
!              AND STANDARD DEVIATION = SQRT(N(N+2)/12)
!              THIS DISTRIBUTION HAS THE PROBABILITY
!              DENSITY FUNCTION F(X) = 1/(N+1).
!              IT HAS THE CUMULATIVE PROBABILITY DISTRIBUTION
!              CDF(X) = (X+1)/(N+1)
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE CUMULATIVE DISTRIBUTION
!                                FUNCTION IS TO BE EVALUATED.
!                     --N        UPPER LIMIT OF DISTRIBUTION
!     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE
!                                DISTRIBUTION FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION
!             FUNCTION VALUE CDF.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE AN INTEGER BETWEEN 0 AND N, INCLUSIVELY.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--EVANS, HASTINGS, AND PEACOCK, STATISTICAL
!                 DISTRIBUTIONS, 2ND ED.--1993, CHAPTER 36
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING LABORATORY (205.03)
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE:  301-975-2855
!     ORIGINAL VERSION--SEPTEMBER 1994.
!     UPDATED         --DECEMBER  1994. FIX BUG
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!---------------------------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(IX.LT.0.OR.IX.GT.N)GO TO 50
      IF(N.LT.1)GO TO 60
      GO TO 90
   50 CONTINUE
      WRITE(ICOUT,2)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,46)IX
      CALL DPWRST('XXX','BUG ')
      IF(IX.LT.0)CDF=0.0
      IF(IX.GT.N)CDF=1.0
      RETURN
   60 CONTINUE
      WRITE(ICOUT,12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,13)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,46)N
      CALL DPWRST('XXX','BUG ')
      CDF=0.0
      RETURN
    2 FORMAT(   &
      '***** NON-FATAL DIAGNOSTIC--THE FIRST  INPUT ARGUMENT TO THE')
    3 FORMAT(   &
      '      DISCDF SUBROUTINE IS OUTSIDE THE USUAL (0,N) INTERVAL ***')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8,' *****')
   12 FORMAT(   &
      '***** FATAL DIAGNOSTIC--THE SECOND INPUT ARGUMENT TO THE')
   13 FORMAT(   &
      '      DISCDF SUBROUTINE IS LESS THAN 1.                     ***')
!
!-----START POINT-----------------------------------------------------
!
   90 CONTINUE
      AX=REAL(IX)
!CCCC FIX FOLLOWING LINE.  DECEMBER 1994.
!CCCC AN=REAL(AN)
      AN=REAL(N)
      CDF=(AX+1.0)/(AN+1.0)
!
      RETURN
      END SUBROUTINE DISCDF
      SUBROUTINE DISPDF(IX,N,PDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
!              FUNCTION VALUE FOR THE DISCRETE UNIFORM (RECTANGULAR)
!              DISTRIBUTION ON THE INTERVAL (0,N).
!              THIS DISTRIBUTION HAS MEAN = N/2
!              AND STANDARD DEVIATION = SQRT(N(N+2)/12)
!              THIS DISTRIBUTION HAS THE PROBABILITY
!              DENSITY FUNCTION F(X) = 1/(N+1)
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE PROBABILITY DENSITY
!                                FUNCTION IS TO BE EVALUATED.
!     OUTPUT ARGUMENTS--PDF    = THE SINGLE PRECISION PROBABILITY
!                                DENSITY FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
!             FUNCTION VALUE PDF.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE BETWEEN 0 AND 1, INCLUSIVELY.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN.
!     REFERENCES--JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
!                 DISTRIBUTIONS--2, 1970, PAGES 57-74.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING LABORATORY (205.03)
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE:  301-975-2855
!     ORIGINAL VERSION--SEPTEMBER 1994.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!---------------------------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      PDF=0.0
      IF(IX.LT.0.OR.IX.GT.N)GO TO 50
      IF(N.LT.1)GO TO 60
      GO TO 90
   50 CONTINUE
      WRITE(ICOUT,2)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,46)IX
      CALL DPWRST('XXX','BUG ')
      RETURN
   60 CONTINUE
      WRITE(ICOUT,12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,13)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,46)N
      CALL DPWRST('XXX','BUG ')
      RETURN
    2 FORMAT(   &
      '***** NON-FATAL DIAGNOSTIC--THE FIRST  INPUT ARGUMENT TO THE')
    3 FORMAT(   &
      '      DISPDF SUBROUTINE IS OUTSIDE THE USUAL (0,N) INTERVAL **')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8,' *****')
   12 FORMAT(   &
      '***** FATAL DIAGNOSTIC--THE SECOND INPUT ARGUMENT TO THE')
   13 FORMAT(   &
      '      DISPDF SUBROUTINE IS LESS THAN 1.                     **')
!
!-----START POINT-----------------------------------------------------
!
   90 CONTINUE
      PDF=1.0/REAL(N+1)
!
      RETURN
      END SUBROUTINE DISPDF
      SUBROUTINE DISPPF(P,N,PPF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
!              FUNCTION VALUE FOR THE DISCRETE UNIFORM (RECTANGUALAR)
!              DISTRIBUTION FROM 0 TO N
!              THIS DISTRIBUTION HAS THE PROBABILITY DENSITY FUNCTION
!              F(X)=1/(N+1)
!              IT HAS THE PPF FUNCTION G(P)=P*(N+1)-1.
!              NOTE THAT THE PERCENT POINT FUNCTION OF A DISTRIBUTION
!              IS IDENTICALLY THE SAME AS THE INVERSE CUMULATIVE
!              DISTRIBUTION FUNCTION OF THE DISTRIBUTION.
!     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE
!                                (BETWEEN 0.0 AND 1.0)
!                                AT WHICH THE PERCENT POINT
!                                FUNCTION IS TO BE EVALUATED.
!                      --N     = UPPER LIMIT OF THE DISTRIBUTION
!     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT
!                                POINT FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PERCENT POINT
!             FUNCTION VALUE PPF.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--P SHOULD BE BETWEEN 0.0 AND 1.0, INCLUSIVELY.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1966)
!     VERSION NUMBER--94.9
!     ORIGINAL VERSION--SEPTEMBER 1994.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(P.LT.0.0.OR.P.GT.1.0)GO TO 50
      IF(N.LT.1)GO TO 60
      GO TO 90
   50 WRITE(ICOUT,1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,46)P
      CALL DPWRST('XXX','BUG ')
      RETURN
   60 CONTINUE
      WRITE(ICOUT,12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,13)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,47)N
      CALL DPWRST('XXX','BUG ')
      PPF=0.0
      RETURN
   90 CONTINUE
    1 FORMAT('***** FATAL ERROR--THE 1ST INPUT ARGUMENT TO THE ',   &
      'DISPPF SUBROUTINE IS OUTSIDE THE ALLOWABLE (0,1) INTERVAL')
   12 FORMAT(   &
      '***** FATAL DIAGNOSTIC--THE SECOND INPUT ARGUMENT TO THE')
   13 FORMAT(   &
      '      DISPDF SUBROUTINE IS LESS THAN 1.                     **')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',E15.8,' *****')
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8,' *****')
!
!-----START POINT-----------------------------------------------------
!
      PPF=P*(REAL(N)+1.0)-1.0
      IPPF=INT(PPF)
      IF(IPPF.LT.0)IPPF=0
      IF(IPPF.GT.N)IPPF=N
      PPF=REAL(IPPF)
      RETURN
      END SUBROUTINE DISPPF
      SUBROUTINE DISTIN(X,NX,IWRITE,Y,NY,IBUGA3,IERROR)
!
!     PURPOSE--COMPUTE DISTINCT VALUES OF A VARIABLE--
!              Y(1) = X(1)
!              Y(2) = X(2) OR X(3) OR X(4) ETC., THE FIRST ONE
!                     OF WHICH IS DIFFERENT FROM Y(1);
!              Y(3) = X(3) OR X(4) OR X(5) ETC., THE FIRST ONE
!                     OF WHICH IS DIFFERENT FROM Y(1) AND Y(2);
!              ETC.
!     NOTE--IT IS PERMISSIBLE TO HAVE THE OUTPUT VECTOR Y(.)
!           BEING IDENTICAL TO THE INPUT VECTOR X(.).
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-921-3651
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82/7
!     ORIGINAL VERSION--FEBRUARY  1979.
!     UPDATED         --APRIL     1979.
!     UPDATED         --AUGUST    1981.
!     UPDATED         --MAY       1982.
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
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DIST'
      ISUBN2='IN  '
      IERROR='NO'
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DISTIN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,IWRITE,NX
   52   FORMAT('IBUGA3,IWRITE,NX = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,NX
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ********************************
!               **  COMPUTE DISTINCT VALUES.  **
!               ********************************
!
      NY=0
      IF(NX.LT.1)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,151)
  151   FORMAT('***** ERROR IN DISTIN (DISTINCT)--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,152)
  152   FORMAT('      THE NUMBER OF OBSERVATIONS FOR THE RESPONSE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,153)
  153   FORMAT('      VARIABLE IS LESS THAN ONE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,157)NX
  157   FORMAT('      THE NUMBER OF OBSERVATIONS HERE = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
      ELSE
        NY=1
        Y(NY)=X(1)
        IF(NX.LT.2)GO TO 9000
        DO 100 I=2,NX
          DO 120 J=1,NY
            IF(X(I).EQ.Y(J))GO TO 100
  120     CONTINUE
          NY=NY+1
          Y(NY)=X(I)
  100   CONTINUE
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
 9011   FORMAT('***** AT THE END       OF DISTIN--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGA3,IERROR,NX,NY
 9012   FORMAT('IBUGA3,IERROR,NX,NY = ',2(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,NY
          WRITE(ICOUT,9016)I,Y(I)
 9016     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DISTIN
      SUBROUTINE DISTI2(X,NX,IWRITE,Y,NY,IBUGA3,IERROR)
!
!     PURPOSE--COMPUTE DISTI2CT VALUES OF A VARIABLE--
!              Y(1) = X(1)
!              Y(2) = X(2) OR X(3) OR X(4) ETC., THE FIRST ONE
!                     OF WHICH IS DIFFERENT FROM Y(1);
!              Y(3) = X(3) OR X(4) OR X(5) ETC., THE FIRST ONE
!                     OF WHICH IS DIFFERENT FROM Y(1) AND Y(2);
!              ETC.
!     NOTE--IT IS PERMISSIBLE TO HAVE THE OUTPUT VECTOR Y(.)
!           BEING IDENTICAL TO THE INPUT VECTOR X(.).
!     NOTE--THIS IS IDENTICAL TO DISTIN WITH THE EXCEPTION THAT
!           THIS VERSION WORKS ON DOUBLE PREICISION ARRAYS.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-921-3651
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--97/8
!     ORIGINAL VERSION--AUGUST    1997.
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
      DOUBLE PRECISION X(*)
      DOUBLE PRECISION Y(*)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DIST'
      ISUBN2='IN  '
      IERROR='NO'
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DISTI2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,IWRITE,NX
   52   FORMAT('IBUGA3,IWRITE,NX = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO 55 I=1,NX
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
!               ********************************
!               **  COMPUTE DISTI2CT VALUES.  **
!               ********************************
!
      NY=0
      IF(NX.LT.1)GO TO 150
      DO 100 I=1,NX
      IF(I.EQ.1)GO TO 130
      DO 120 J=1,NY
      IF(X(I).EQ.Y(J))GO TO 100
  120 CONTINUE
  130 CONTINUE
      NY=NY+1
      Y(NY)=X(I)
  100 CONTINUE
      GO TO 190
!
  150 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,151)
  151 FORMAT('***** ERROR IN DISTI2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,152)
  152 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,153)
  153 FORMAT('      IN THE VARIABLE FOR WHICH')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,154)
  154 FORMAT('      THE DISTI2CT VALUES ARE TO BE FOUND')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,155)
  155 FORMAT('      MUST BE 1 OR LARGER.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,156)
  156 FORMAT('      SUCH WAS NOT THE CASE HERE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,157)NX
  157 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS HERE = ',I8,   &
      '.')
      CALL DPWRST('XXX','BUG ')
!
  190 CONTINUE
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DISTI2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR,NX,NY
 9012   FORMAT('IERROR,NX,NY = ',A4,2X,2I8)
        CALL DPWRST('XXX','BUG ')
        DO 9015 I=1,MAX(NX,NY)
          WRITE(ICOUT,9016)I,X(I),Y(I)
 9016     FORMAT('I,X(I),Y(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DISTI2
      SUBROUTINE DIWCDF(X,Q,BETA,CDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
!              FUNCTION VALUE FOR THE DISCRETE WEIBULL
!              DISTRIBUTION WITH SHAPE PARAMETERS Q AND BETA.
!              THIS DISTRIBUTION IS DEFINED FOR ALL INTEGER X >= 0.
!              THE CUMULATIVE DISTRIBUTION FUNCTION IS:
!                  F(X;Q,BETA) = 1 - (Q)**((X+1)**BETA)
!                  X = 0, 1, 2, ...;  0 < Q < 1;  BETA > 0
!     INPUT  ARGUMENTS--X      = THE DOUBLE PRECISION VALUE AT
!                                WHICH THE CUMULATIVE DISTRIBUTION
!                                FUNCTION IS TO BE EVALUATED.
!                                X SHOULD BE A NON-NEGATIVE INTEGER.
!                     --Q      = THE DOUBLE PRECISION VALUE OF THE
!                                FIRST SHAPE PARAMETER
!                     --BETA   = THE DOUBLE PRECISION VALUE OF THE
!                                SECOND SHAPE PARAMETER
!     OUTPUT ARGUMENTS--CDF    = THE DOUBLE PRECISION CUMULATIVE
!                                DISTRIBUTION FUNCTION VALUE.
!     OUTPUT--THE DOUBLE PRECISION CUMULATIVE DISTRIBUTION FUNCTION
!             VALUE CDF FOR THE DISCRETE WEIBULL DISTRIBUTION WITH
!             SHAPE PARAMETERS Q AND BETA
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE A NON-NEGATIVE INTEGER
!                 --0 < Q < 1; BETA > 0
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--JOHNSON, KEMP, AND KOTZ (2005), "UNIVARIATE DISCRETE
!                 DISTRIBUTIONS", THIRD EDITION, WILEY, PP. 510-511.
!               --NAKAGAWA AND OSAKI (1975), "THE DISCRETE WEIBULL
!                 DISTRIBUTION", IEEE TRANSACTIONS ON RELIABILITY,
!                 R-24, PP. 300-301.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2006/11
!     ORIGINAL VERSION--NOVEMBER  2006.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION X
      DOUBLE PRECISION Q
      DOUBLE PRECISION BETA
      DOUBLE PRECISION CDF
      DOUBLE PRECISION DTERM1
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IX=INT(X+0.5D0)
      IF(IX.LT.0)THEN
        WRITE(ICOUT,4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        CDF=0.0D0
        GO TO 9000
      ENDIF
    4 FORMAT('***** ERROR--THE FIRST ARGUMENT TO DIWCDF IS LESS ',   &
      'THAN 0')
!
      IF(Q.LE.0.0D0 .OR. Q.GE.1.0D0)THEN
        WRITE(ICOUT,15)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)Q
        CALL DPWRST('XXX','BUG ')
        CDF=0.0D0
        GO TO 9000
      ENDIF
   15 FORMAT('***** ERROR--THE SECOND ARGUMENT TO DIWCDF IS NOT IN ',   &
      'THE INTERVAL (0,1)')
!
      IF(BETA.LE.0.0D0)THEN
        WRITE(ICOUT,25)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        CDF=0.0
        GO TO 9000
      ENDIF
   25 FORMAT('***** ERROR--THE THIRD ARGUMENT TO DIWCDF IS NEGATIVE')
!
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
!
      DTERM1=((X+1.0D0)**BETA)*DLOG(Q)
      CDF=1.0D0 - DEXP(DTERM1)
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE DIWCDF
      SUBROUTINE DIWHAZ(X,Q,BETA,HAZ)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE HAZARD
!              FUNCTION VALUE FOR THE DISCRETE WEIBULL
!              DISTRIBUTION WITH SHAPE PARAMETERS Q AND BETA.
!              THIS DISTRIBUTION IS DEFINED FOR ALL INTEGER X >= 0.
!              THE HAZARD FUNCTION IS:
!                  h(X;Q,BETA) = 1 - (Q)**(X+1)**BETA/(Q)**(X**BETA)
!                  X = 0, 1, 2, ...;  0 < Q < 1;  BETA > 0
!     INPUT  ARGUMENTS--X      = THE DOUBLE PRECISION VALUE AT
!                                WHICH THE PROBABILITYU MASS
!                                FUNCTION IS TO BE EVALUATED.
!                                X SHOULD BE A NON-NEGATIVE INTEGER.
!                     --Q      = THE DOUBLE PRECISION VALUE OF THE
!                                FIRST SHAPE PARAMETER
!                     --BETA   = THE DOUBLE PRECISION VALUE OF THE
!                                SECOND SHAPE PARAMETER
!     OUTPUT ARGUMENTS--HAZ    = THE DOUBLE PRECISION HAZARD
!                                FUNCTION VALUE.
!     OUTPUT--THE DOUBLE PRECISION HAZARD FUNCTION
!             VALUE HAZ FOR THE DISCRETE WEIBULL DISTRIBUTION WITH
!             SHAPE PARAMETERS Q AND BETA
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE A NON-NEGATIVE INTEGER
!                 --0 < Q < 1; BETA > 0
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--JOHNSON, KEMP, AND KOTZ (2005), "UNIVARIATE DISCRETE
!                 DISTRIBUTIONS", THIRD EDITION, WILEY, PP. 515-516.
!               --NAKAGAWA AND OSAKI (1975), "THE DISCRETE WEIBULL
!                 DISTRIBUTION", IEEE TRANSACTIONS ON RELIABILITY,
!                 R-24, PP. 300-301.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2006/11
!     ORIGINAL VERSION--NOVEMBER  2006.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION X
      DOUBLE PRECISION Q
      DOUBLE PRECISION BETA
      DOUBLE PRECISION HAZ
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IX=INT(X+0.5D0)
      IF(IX.LT.0)THEN
        WRITE(ICOUT,4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        HAZ=0.0D0
        GO TO 9000
      ENDIF
    4 FORMAT('***** ERROR--THE FIRST ARGUMENT TO DIWHAZ IS LESS ',   &
      'THAN 0')
!
      IF(Q.LE.0.0D0 .OR. Q.GE.1.0D0)THEN
        WRITE(ICOUT,15)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)Q
        CALL DPWRST('XXX','BUG ')
        HAZ=0.0D0
        GO TO 9000
      ENDIF
   15 FORMAT('***** ERROR--THE SECOND ARGUMENT TO DIWHAZ IS NOT IN ',   &
      'THE INTERVAL (0,1)')
!
      IF(BETA.LE.0.0D0)THEN
        WRITE(ICOUT,25)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        HAZ=0.0
        GO TO 9000
      ENDIF
   25 FORMAT('***** ERROR--THE THIRD ARGUMENT TO DIWHAZ IS NEGATIVE')
!
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
!
      DTERM1=((X+1.0D0)**BETA)*DLOG(Q)
      DTERM2=(X**BETA)*DLOG(Q)
      HAZ=1.0D0 - DEXP(DTERM1 - DTERM2)
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE DIWHAZ
      SUBROUTINE DIWPDF(X,Q,BETA,PDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY MASS
!              FUNCTION VALUE FOR THE DISCRETE WEIBULL
!              DISTRIBUTION WITH SHAPE PARAMETERS Q AND BETA.
!              THIS DISTRIBUTION IS DEFINED FOR ALL INTEGER X >= 0.
!              THE PROBABILITY MASS FUNCTION IS:
!                  p(X;Q,BETA) = (Q)**(X**BETA) - (Q)**((X+1)**BETA)
!                  X = 0, 1, 2, ...;  0 < Q < 1;  BETA > 0
!     INPUT  ARGUMENTS--X      = THE DOUBLE PRECISION VALUE AT
!                                WHICH THE PROBABILITYU MASS
!                                FUNCTION IS TO BE EVALUATED.
!                                X SHOULD BE A NON-NEGATIVE INTEGER.
!                     --Q      = THE DOUBLE PRECISION VALUE OF THE
!                                FIRST SHAPE PARAMETER
!                     --BETA   = THE DOUBLE PRECISION VALUE OF THE
!                                SECOND SHAPE PARAMETER
!     OUTPUT ARGUMENTS--PDF    = THE DOUBLE PRECISION PROBABILITY MASS
!                                FUNCTION VALUE.
!     OUTPUT--THE DOUBLE PRECISION PROBABILITY MASS FUNCTION
!             VALUE PDF FOR THE DISCRETE WEIBULL DISTRIBUTION WITH
!             SHAPE PARAMETERS Q AND BETA
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE A NON-NEGATIVE INTEGER
!                 --0 < Q < 1; BETA > 0
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--JOHNSON, KEMP, AND KOTZ (2005), "UNIVARIATE DISCRETE
!                 DISTRIBUTIONS", THIRD EDITION, WILEY, PP. 510-511.
!               --NAKAGAWA AND OSAKI (1975), "THE DISCRETE WEIBULL
!                 DISTRIBUTION", IEEE TRANSACTIONS ON RELIABILITY,
!                 R-24, PP. 300-301.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2006/11
!     ORIGINAL VERSION--NOVEMBER  2006.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION X
      DOUBLE PRECISION Q
      DOUBLE PRECISION BETA
      DOUBLE PRECISION PDF
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IX=INT(X+0.5D0)
      IF(IX.LT.0)THEN
        WRITE(ICOUT,4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        PDF=0.0D0
        GO TO 9000
      ENDIF
    4 FORMAT('***** ERROR--THE FIRST ARGUMENT TO DIWPDF IS LESS ',   &
      'THAN 0')
!
      IF(Q.LE.0.0D0 .OR. Q.GE.1.0D0)THEN
        WRITE(ICOUT,15)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)Q
        CALL DPWRST('XXX','BUG ')
        PDF=0.0D0
        GO TO 9000
      ENDIF
   15 FORMAT('***** ERROR--THE SECOND ARGUMENT TO DIWPDF IS NOT IN ',   &
      'THE INTERVAL (0,1)')
!
      IF(BETA.LE.0.0D0)THEN
        WRITE(ICOUT,25)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GO TO 9000
      ENDIF
   25 FORMAT('***** ERROR--THE THIRD ARGUMENT TO DIWPDF IS NEGATIVE')
!
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
!
      DTERM1=(X**BETA)*DLOG(Q)
      DTERM2=((X+1)**BETA)*DLOG(Q)
      PDF=DEXP(DTERM1) - DEXP(DTERM2)
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE DIWPDF
      SUBROUTINE DIWPPF(P,Q,BETA,PPF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
!              FUNCTION VALUE FOR THE DISCRETE WEIBULL
!              DISTRIBUTION WITH SHAPE PARAMETERS Q AND BETA.
!              THIS DISTRIBUTION IS DEFINED FOR ALL INTEGER X >= 0.
!              THE PERCENT POINT FUNCTION IS:
!                  G(P;Q,BETA) = {LOG(1-P)/LOG(Q)]**(1/BETA)  0 <= P < 1
!     INPUT  ARGUMENTS--P      = THE DOUBLE PRECISION VALUE AT
!                                WHICH THE PERCENT POINT
!                                FUNCTION IS TO BE EVALUATED.
!                                P SHOULD BE IN THE INTERVAL (0,1]
!                     --Q      = THE DOUBLE PRECISION VALUE OF THE
!                                FIRST SHAPE PARAMETER
!                     --BETA   = THE DOUBLE PRECISION VALUE OF THE
!                                SECOND SHAPE PARAMETER
!     OUTPUT ARGUMENTS--PPF    = THE DOUBLE PRECISION PERCENT POINT
!                                FUNCTION VALUE.
!     OUTPUT--THE DOUBLE PRECISION PERCENT POINT FUNCTION
!             VALUE PPF FOR THE DISCRETE WEIBULL DISTRIBUTION WITH
!             SHAPE PARAMETERS Q AND BETA
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--0 <= P < 1; 0 < Q < 1; BETA > 0
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--JOHNSON, KEMP, AND KOTZ (2005), "UNIVARIATE DISCRETE
!                 DISTRIBUTIONS", THIRD EDITION, WILEY, PP. 510-511.
!               --NAKAGAWA AND OSAKI (1975), "THE DISCRETE WEIBULL
!                 DISTRIBUTION", IEEE TRANSACTIONS ON RELIABILITY,
!                 R-24, PP. 300-301.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2006/11
!     ORIGINAL VERSION--NOVEMBER  2006.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION P
      DOUBLE PRECISION Q
      DOUBLE PRECISION BETA
      DOUBLE PRECISION PPF
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DEPS
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
      DATA DEPS/0.1D-15/
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(P.LT.0.0D0 .OR. P.GE.1.0)THEN
        WRITE(ICOUT,4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
        CALL DPWRST('XXX','BUG ')
        PPF=0.0D0
        GO TO 9000
      ENDIF
    4 FORMAT('***** ERROR--THE FIRST ARGUMENT TO DIWPPF IS OUTSIDE ',   &
      'THE ALLOWABLE (0,1] INTERVAL')
!
      IF(Q.LE.0.0D0 .OR. Q.GE.1.0D0)THEN
        WRITE(ICOUT,15)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)Q
        CALL DPWRST('XXX','BUG ')
        PPF=0.0D0
        GO TO 9000
      ENDIF
   15 FORMAT('***** ERROR--THE SECOND ARGUMENT TO DIWPPF IS NOT IN ',   &
      'THE INTERVAL (0,1)')
!
      IF(BETA.LE.0.0D0)THEN
        WRITE(ICOUT,25)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        PPF=0.0
        GO TO 9000
      ENDIF
   25 FORMAT('***** ERROR--THE THIRD ARGUMENT TO DIWPPF IS NEGATIVE')
!
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
!
      DTERM1=(DLOG(1.0D0 - P)/DLOG(Q))**(1.0D0/BETA)
      IPPF=INT(DTERM1+DEPS)
      PPF=DBLE(IPPF)
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE DIWPPF
      SUBROUTINE DIWRAN(N,Q,BETA,ISEED,X)
!
!     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
!              FROM THE DISCRETE WEIBULL DISTRIBUTION
!              WITH SHAPE PARAMETERS Q AND BETA.
!              THIS DISTRIBUTION IS DEFINED FOR ALL
!              NON-NEGATIVE INTEGER X >= 0 AND HAS
!              THE PROBABILITY MASS FUNCTION IS:
!                  p(X;Q,BETA) = (Q)**(X**BETA) - (Q)**((X+1)**BETA)
!                  X = 0, 1, 2, ...;  0 < Q < 1;  BETA > 0
!     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
!                                OF RANDOM NUMBERS TO BE
!                                GENERATED.
!                     --Q      = THE SINGLE PRECISION VALUE
!                                OF THE FIRST SHAPE PARAMETER.
!                     --BETA   = THE SINGLE PRECISION VALUE
!                                OF THE SECOND SHAPE PARAMETER.
!     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
!                                (OF DIMENSION AT LEAST N)
!                                INTO WHICH THE GENERATED
!                                RANDOM SAMPLE WILL BE PLACED.
!     OUTPUT--A RANDOM SAMPLE OF SIZE N
!             FROM THE DISCRETE WEIBULL DISTRIBUTION
!             WITH SHAPE PARAMETERS Q AND BETA.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!                 --0 < Q < 1, BETA > 0
!     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN, DIWPPF
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--JOHNSON, KEMP, AND KOTZ (2005), "UNIVARIATE DISCRETE
!                 DISTRIBUTIONS", THIRD EDITION, WILEY, PP. 510-511.
!               --NAKAGAWA AND OSAKI (1975), "THE DISCRETE WEIBULL
!                 DISTRIBUTION", IEEE TRANSACTIONS ON RELIABILITY,
!                 R-24, PP. 300-301.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2006/11
!     ORIGINAL VERSION--NOVEMBER  2006.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      REAL Q
      REAL BETA
      DIMENSION X(*)
!
      DOUBLE PRECISION DQ
      DOUBLE PRECISION DBETA
      DOUBLE PRECISION DPPF
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(N.LT.1)THEN
        WRITE(ICOUT,5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
    5 FORMAT('***** ERROR--THE REQUESTED NUMBER OF DISCRETE WEIBULL')
    6 FORMAT('      RANDOM NUMBERS IS NON-POSITIVE')
      IF(Q.LE.0.0 .OR. Q.GE.1.0)THEN
        WRITE(ICOUT,11)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)Q
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
   11 FORMAT('***** ERROR--THE Q PARAMETER FOR THE ',   &
      'DISCRETE WEIBULL')
   12 FORMAT('      RANDOM NUMBERS IS OUTSIDE THE (0,1) INTERVAL')
!
      IF(BETA.LE.0.0)THEN
        WRITE(ICOUT,21)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,22)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
   21 FORMAT('***** ERROR--THE BETA PARAMETER FOR THE ',   &
      'DISCRETE WEIBULL')
   22 FORMAT('      RANDOM NUMBERS IS NON-POSITIVE.')
!
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
!
!     GENERATE N DISCRETE WEIBULL DISTRIBUTION
!     RANDOM NUMBERS.
!
      DQ=DBLE(Q)
      DBETA=DBLE(BETA)
      CALL UNIRAN(N,ISEED,X)
!
      DO 100 I=1,N
        ZTEMP=X(I)
        CALL DIWPPF(DBLE(ZTEMP),DQ,DBETA,DPPF)
        X(I)=REAL(DPPF)
  100 CONTINUE
!
 9999 CONTINUE
!
      RETURN
      END SUBROUTINE DIWRAN
      DOUBLE PRECISION FUNCTION DLBETA (A, B)
!***BEGIN PROLOGUE  DLBETA
!***PURPOSE  Compute the natural logarithm of the complete Beta
!            function.
!***LIBRARY   SLATEC (FNLIB)
!***CATEGORY  C7B
!***TYPE      DOUBLE PRECISION (ALBETA-S, DLBETA-D, CLBETA-C)
!***KEYWORDS  FNLIB, LOGARITHM OF THE COMPLETE BETA FUNCTION,
!             SPECIAL FUNCTIONS
!***AUTHOR  Fullerton, W., (LANL)
!***DESCRIPTION
!
! DLBETA(A,B) calculates the double precision natural logarithm of
! the complete beta function for double precision arguments
! A and B.
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  D9LGMC, DGAMMA, DLNGAM, DLNREL, XERMSG
!***REVISION HISTORY  (YYMMDD)
!   770701  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890531  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!   900727  Added EXTERNAL statement.  (WRB)
!***END PROLOGUE  DLBETA
      DOUBLE PRECISION A, B, P, Q, CORR, SQ2PIL, D9LGMC, DGAMMA, DLNGAM,   &
        DLNREL
      EXTERNAL DGAMMA
      SAVE SQ2PIL
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
      DATA SQ2PIL / 0.91893853320467274178032973640562D0 /
!***FIRST EXECUTABLE STATEMENT  DLBETA
      P = MIN (A, B)
      Q = MAX (A, B)
!
      IF (P .LE. 0.D0) THEN
        WRITE(ICOUT,11)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        DLBETA = 0.D0
        RETURN
      ENDIF
   11 FORMAT('***** ERROR FROM DLBETA.  BOTH INPUT ARGUMENTS ')
   12 FORMAT('      MUST BE GREATER THAN ZERO.               ******')
!
      IF (P.GE.10.D0) GO TO 30
      IF (Q.GE.10.D0) GO TO 20
!
! P AND Q ARE SMALL.
!
      DLBETA = LOG (DGAMMA(P) * (DGAMMA(Q)/DGAMMA(P+Q)) )
      RETURN
!
! P IS SMALL, BUT Q IS BIG.
!
 20   CORR = D9LGMC(Q) - D9LGMC(P+Q)
      DLBETA = DLNGAM(P) + CORR + P - P*LOG(P+Q)   &
        + (Q-0.5D0)*DLNREL(-P/(P+Q))
      RETURN
!
! P AND Q ARE BIG.
!
 30   CORR = D9LGMC(P) + D9LGMC(Q) - D9LGMC(P+Q)
      DLBETA = -0.5D0*LOG(Q) + SQ2PIL + CORR + (P-0.5D0)*LOG(P/(P+Q))   &
        + Q*DLNREL(-P/(P+Q))
      RETURN
!
      END FUNCTION DLBETA 
!===================================================== DLGAMA.FOR
      DOUBLE PRECISION FUNCTION DLGADP(X)
!
!     2020/03: RENAME TO AVOID CONFLICT WITH INTRINSIC ROUTINE
!
!CCCC DOUBLE PRECISION FUNCTION DLGAMA(X)
!***********************************************************************
!*                                                                     *
!*  FORTRAN CODE WRITTEN FOR INCLUSION IN IBM RESEARCH REPORT RC20525, *
!*  'FORTRAN ROUTINES FOR USE WITH THE METHOD OF L-MOMENTS, VERSION 3' *
!*                                                                     *
!*  J. R. M. HOSKING                                                   *
!*  IBM RESEARCH DIVISION                                              *
!*  T. J. WATSON RESEARCH CENTER                                       *
!*  YORKTOWN HEIGHTS                                                   *
!*  NEW YORK 10598, U.S.A.                                             *
!*                                                                     *
!*  VERSION 3     AUGUST 1996                                          *
!*                                                                     *
!***********************************************************************
!
!  LOGARITHM OF GAMMA FUNCTION
!
!  BASED ON ALGORITHM ACM291, COMMUN. ASSOC. COMPUT. MACH. (1966)
!
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
      INCLUDE 'DPCOP2.INC'
!
      DATA SMALL,CRIT,BIG,TOOBIG/1D-7,13D0,1D9,2D36/
!
!         C0 IS 0.5*LOG(2*PI)
!         C1...C7 ARE THE COEFFTS OF THE ASYMPTOTIC EXPANSION OF DLGAMA
!
      DATA C0,C1,C2,C3,C4,C5,C6,C7/   &
         0.918938533204672742D0,  0.833333333333333333D-1,   &
        -0.277777777777777778D-2,  0.793650793650793651D-3,   &
        -0.595238095238095238D-3,  0.841750841750841751D-3,   &
        -0.191752691752691753D-2,  0.641025641025641026D-2/
!
!         S1 IS -(EULER'S CONSTANT), S2 IS PI**2/12
!
      DATA S1/-0.577215664901532861D0/
      DATA S2/ 0.822467033424113218D0/
!
      DATA ZERO/0D0/,HALF/0.5D0/,ONE/1D0/,TWO/2D0/
!
!CCCC DLGAMA=ZERO
      DLGADP=ZERO
!
      IF(X.LE.ZERO .OR. X.GT.TOOBIG)THEN
        WRITE(ICOUT,7000)
 7000   FORMAT('****** ERROR IN DLGAMA: ARGUMENT OUT OF RANGE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7002)X
 7002   FORMAT('       VALUE OF THE ARGUMENT IS ',D24.16)
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
!         USE SMALL-X APPROXIMATION IF X IS NEAR 0, 1 OR 2
!
      IF(DABS(X-TWO).GT.SMALL)GO TO  10
!CCCC DLGAMA=DLOG(X-ONE)
      DLGADP=DLOG(X-ONE)
      XX=X-TWO
      GO TO  20
   10 IF(DABS(X-ONE).GT.SMALL)GO TO  30
      XX=X-ONE
!CC20 DLGAMA=DLGAMA+XX*(S1+XX*S2)
   20 DLGADP=DLGADP+XX*(S1+XX*S2)
      GO TO 9000
   30 IF(X.GT.SMALL)GO TO  40
!CCCC DLGAMA=-DLOG(X)+S1*X
      DLGADP=-DLOG(X)+S1*X
      GO TO 9000
!
!         REDUCE TO DLGAMA(X+N) WHERE X+N.GE.CRIT
!
   40 SUM1=ZERO
      Y=X
      IF(Y.GE.CRIT)GO TO  60
      Z=ONE
   50 Z=Z*Y
      Y=Y+ONE
      IF(Y.LT.CRIT)GO TO  50
      SUM1=SUM1-DLOG(Z)
!
!         USE ASYMPTOTIC EXPANSION IF Y.GE.CRIT
!
   60 SUM1=SUM1+(Y-HALF)*DLOG(Y)-Y+C0
      SUM2=ZERO
      IF(Y.GE.BIG)GO TO  70
      Z=ONE/(Y*Y)
      SUM2=((((((C7*Z+C6)*Z+C5)*Z+C4)*Z+C3)*Z+C2)*Z+C1)/Y
!CC70 DLGAMA=SUM1+SUM2
   70 DLGADP=SUM1+SUM2
      GO TO 9000
!
 9000 CONTINUE
      RETURN
      END FUNCTION DLGADP
      SUBROUTINE DLGAMS (X, DLGAM, SGNGAM)
!***BEGIN PROLOGUE  DLGAMS
!***PURPOSE  Compute the logarithm of the absolute value of the Gamma
!            function.
!***LIBRARY   SLATEC (FNLIB)
!***CATEGORY  C7A
!***TYPE      DOUBLE PRECISION (ALGAMS-S, DLGAMS-D)
!***KEYWORDS  ABSOLUTE VALUE OF THE LOGARITHM OF THE GAMMA FUNCTION,
!             FNLIB, SPECIAL FUNCTIONS
!***AUTHOR  Fullerton, W., (LANL)
!***DESCRIPTION
!
! DLGAMS(X,DLGAM,SGNGAM) calculates the double precision natural
! logarithm of the absolute value of the Gamma function for
! double precision argument X and stores the result in double
! precision argument DLGAM.
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  DLNGAM
!***REVISION HISTORY  (YYMMDD)
!   770701  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890531  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!***END PROLOGUE  DLGAMS
      DOUBLE PRECISION X, DLGAM, SGNGAM, DLNGAM
!***FIRST EXECUTABLE STATEMENT  DLGAMS
      DLGAM = DLNGAM(X)
      SGNGAM = 1.0D0
      IF (X.GT.0.D0) RETURN
!
      INTZ = INT(MOD (-AINT(X), 2.0D0) + 0.1D0)
      IF (INTZ.EQ.0) SGNGAM = -1.0D0
!
      RETURN
      END SUBROUTINE DLGAMS 
      DOUBLE PRECISION FUNCTION DLNGAM (X)
!***BEGIN PROLOGUE  DLNGAM
!***PURPOSE  Compute the logarithm of the absolute value of the Gamma
!            function.
!***LIBRARY   SLATEC (FNLIB)
!***CATEGORY  C7A
!***TYPE      DOUBLE PRECISION (ALNGAM-S, DLNGAM-D, CLNGAM-C)
!***KEYWORDS  ABSOLUTE VALUE, COMPLETE GAMMA FUNCTION, FNLIB, LOGARITHM,
!             SPECIAL FUNCTIONS
!***AUTHOR  Fullerton, W., (LANL)
!***DESCRIPTION
!
! DLNGAM(X) calculates the double precision logarithm of the
! absolute value of the Gamma function for double precision
! argument X.
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  D1MACH, D9LGMC, DGAMMA, XERMSG
!***REVISION HISTORY  (YYMMDD)
!   770601  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890531  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!   900727  Added EXTERNAL statement.  (WRB)
!***END PROLOGUE  DLNGAM
      DOUBLE PRECISION X, DXREL, PI, SINPIY, SQPI2L, SQ2PIL, XMAX,   &
        Y, DGAMMA, D9LGMC, TEMP
      LOGICAL FIRST
      EXTERNAL DGAMMA
      SAVE SQ2PIL, SQPI2L, PI, XMAX, DXREL, FIRST
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DATA SQ2PIL / 0.91893853320467274178032973640562D0 /
      DATA SQPI2L / +.225791352644727432363097614947441D+0    /
      DATA PI / 3.14159265358979323846264338327950D0 /
      DATA FIRST /.TRUE./
!***FIRST EXECUTABLE STATEMENT  DLNGAM
!
      DLNGAM = 0.0D0
!
      IF (FIRST) THEN
         TEMP = 1.D0/LOG(D1MACH(2))
         XMAX = TEMP*D1MACH(2)
         DXREL = SQRT(D1MACH(4))
      ENDIF
      FIRST = .FALSE.
!
      Y = ABS (X)
      IF (Y.GT.10.D0) GO TO 20
!
! LOG (ABS (DGAMMA(X)) ) FOR ABS(X) .LE. 10.0
!
      DLNGAM = LOG (ABS (DGAMMA(X)) )
      RETURN
!
! LOG ( ABS (DGAMMA(X)) ) FOR ABS(X) .GT. 10.0
!
 20   IF (Y .GT. XMAX) THEN
        WRITE(ICOUT,21)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,22)
        CALL DPWRST('XXX','BUG ')
        DLNGAM = 0.D0
        RETURN
      ENDIF
   21 FORMAT('***** ERROR FROM DLNGAM.  ABSOLUTE VALUE OF X SO ')
   22 FORMAT('      LARGE THAT DLNGAM OVERFLOWS.             ******')
!
      IF (X.GT.0.D0) DLNGAM = SQ2PIL + (X-0.5D0)*LOG(X) - X + D9LGMC(Y)
      IF (X.GT.0.D0) RETURN
!
      SINPIY = ABS (SIN(PI*Y))
      IF (SINPIY .EQ. 0.D0) THEN
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        DLNGAM = 0.D0
        RETURN
      ENDIF
   31 FORMAT('***** ERROR FROM DLNGAM.  X IS A NEGATIVE INTEGER. ')
!
      IF (ABS((X-AINT(X-0.5D0))/X) .LT. DXREL)THEN
        WRITE(ICOUT,41)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,42)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,43)
        CALL DPWRST('XXX','BUG ')
      ENDIF
   41 FORMAT('***** WARNING FROM DLNGAM.  ANSWER LESS THAN HALF ')
   42 FORMAT('      PRECISION BECAUSE X IS TOO NEAR A NEGATIVE ')
   43 FORMAT('      INTEGER.                                    *****')
!
      DLNGAM = SQPI2L + (X-0.5D0)*LOG(Y) - X - LOG(SINPIY) - D9LGMC(Y)
      RETURN
!
      END FUNCTION DLNGAM 
      SUBROUTINE DLGCDF(X,THETA,CDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
!              FUNCTION VALUE FOR THE DISCRETE LOGARITHMIC SERIES
!              DISTRIBUTION WITH SHAPE PARAMETER = THETA.
!              THIS DISTRIBUTION IS DEFINED FOR ALL INTEGER X>1.
!              THE PROBABILITY DENSITY FUNCTION IS:
!              F(X,THETA)=A*THETA**X/X      X=1,2,3,...
!              WHERE A = 1/LN(1-THETA), 0<THETA<1
!              FOR CDF, USE RECURRENCE RELATION:
!                P(X=x+1) = THETA*P(X=x)/(X+1)     X=1,2,...
!              WHERE
!                P(X=1)=-THETA/LN(1-THETA)
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE CUMULATIVE DISTRIBUTION
!                                FUNCTION IS TO BE EVALUATED.
!                                X SHOULD BE NON-NEGATIVE.
!                     --THETA    = THE SHAPE PARAMETER
!     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE
!                                DISTRIBUTION FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
!             FUNCTION VALUE CDF FOR THE LOGARITHMIC SERIES
!             DISTRIBUTION WITH SHAPE PARAMETER = THETA
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE A POSITIVE INTEGER
!                 --0 < THETA < 1
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--JOHNSON AND KOTZ, DISCRETE UNIVARIATE
!                 DISTRIBUTIONS--1, 1994, CHAPTER 7
!               --"STATISTICAL DISTRIBUTIONS", EVANS, HASTINGS,
!                 PEACOCK.  WILEY, 1993.  CHAPTER 23.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1966)
!     VERSION NUMBER--95/4
!     ORIGINAL VERSION--APRIL     1995.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DTERM1, DTERM2, DTERM3
      DOUBLE PRECISION DX, DTHETA, DLTHET, DSUM
      DOUBLE PRECISION DCURR, DPREV
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----DATA STATEMENTS-------------------------------------------------
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IX=INT(X+0.5)
      CDF=0.0
      IF(THETA.LE.0.0.OR.THETA.GE.1.0)THEN
        WRITE(ICOUT,15)
   15   FORMAT('***** ERROR--THE SECOND ARGUMENT TO DLGCDF ',   &
              'DLGCDF IS NOT IN THE INTERVAL (0,1).')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)THETA
        CALL DPWRST('XXX','BUG ')
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7,'.')
        GO TO 9000
      ELSEIF(IX.LT.1)THEN
        WRITE(ICOUT,4)
    4   FORMAT('***** WARNING--THE FIRST ARGUMENT TO DLGCDF ',   &
               'IS LESS THAN 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
      DX=DBLE(IX)
      DTHETA=DBLE(THETA)
      DSUM=0.0D0
!
      DTERM1=-DTHETA/DLOG(1.0D0-DTHETA)
      IF(IX.EQ.1)THEN
        CDF=REAL(DTERM1)
        GO TO 9000
      ENDIF
!
      DSUM=DTERM1
      DPREV=DTERM1
      DLTHET=DLOG(DTHETA)
      DO 100 I=2,IX
!
        IF(DPREV.LE.D1MACH(1))THEN
          CDF=REAL(DSUM)
          GO TO 9000
        ENDIF
!
        DTERM3=DBLE(I)
        DTERM2=DLTHET + DLOG(DTERM3-1.0D0) + DLOG(DPREV) - DLOG(DTERM3)
        DCURR=DEXP(DTERM2)
        DSUM=DSUM+DCURR
        DPREV=DCURR
 100  CONTINUE
!
      CDF=REAL(DSUM)
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE DLGCDF
      REAL FUNCTION DLGFU2(X)
!
!     PURPOSE--DPMLDL CALLS FZERO TO FIND A ROOT FOR THE EQUATION
!                 XBAR = THETAHAT/[-(1-THETAHAT)LN(1-THETAHAT)
!              DLGFU2 IS THE FUNCTION FOR WHICH THE ZERO IS FOUND.
!              IT IS:
!                 XBAR - THETAHAT/[-(1-THETAHAT)LN(1-THETAHAT) = 0
!              WHERE THETAHAT IS THE DESIRED VALUE (I.E., X)
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE EQUATION IS EVALUATED.
!     OUTPUT--THE SINGLE PRECISION FUNCTION VALUE DLGFU2.
!     PRINTING--NONE.
!     RESTRICTIONS--NONE.
!     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--JOHNSON, KOTZ, AND KEMP, "DISCRETE
!                 UNIVARIATE DISTRIBUTIONS", SECOND EDITION,
!                 JOHN WILEY, 1992, CHAPTER 7.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATION INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATION INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2004.3
!     ORIGINAL VERSION--MARCH     2003.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      REAL XBAR
      COMMON/DLGCOM/XBAR
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      DLGFU2=XBAR - X/(-(1.0-X)*LOG(1.0-X))
!
      RETURN
      END FUNCTION DLGFU2
      SUBROUTINE DLGPDF(X,THETA,PDF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
!              FUNCTION VALUE FOR THE DISCRETE LOGARITHMIC SERIES
!              DISTRIBUTION WITH SHAPE PARAMETER = THETA.
!              THIS DISTRIBUTION IS DEFINED FOR ALL INTEGER X>1.
!              THE PROBABILITY DENSITY FUNCTION IS:
!              F(X,THETA)=A*THETA**X/X      X=1,2,3,...
!              WHERE A = 1/LN(1-THETA), 0<THETA<1
!     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
!                                WHICH THE CUMULATIVE DISTRIBUTION
!                                FUNCTION IS TO BE EVALUATED.
!                                X SHOULD BE NON-NEGATIVE.
!                     --THETA    = THE SHAPE PARAMETER
!     OUTPUT ARGUMENTS--PDF    = THE SINGLE PRECISION CUMULATIVE
!                                DISTRIBUTION FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
!             FUNCTION VALUE PDF FOR THE LOGARITHMIC SERIES
!             DISTRIBUTION WITH SHAPE PARAMETER = THETA
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--X SHOULD BE A POSITIVE INTEGER
!                 --0 < THETA < 1
!     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--JOHNSON AND KOTZ, DISCRETE UNIVARIATE
!                 DISTRIBUTIONS--1, 1994, CHAPTER 7
!               --"STATISTICAL DISTRIBUTIONS", EVANS, HASTINGS,
!                 PEACOCK.  WILEY, 1993.  CHAPTER 23.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1966)
!     VERSION NUMBER--95/4
!     ORIGINAL VERSION--APRIL     1995.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION DTERM1, DTERM2, DTERM3, DTERM4, DTERM5
      DOUBLE PRECISION DX, DTHETA, DCONST
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IX=INT(X+0.5)
      PDF=0.0
      IF(THETA.LE.0.0 .OR. THETA.GE.1.0)THEN
        WRITE(ICOUT,15)
   15   FORMAT('***** ERROR--THE SECOND ARGUMENT TO DLGPDF ',   &
               'IS NOT IN THE INTERVAL (0,1).')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)THETA
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(IX.LT.1)THEN
        WRITE(ICOUT,4)
    4   FORMAT('***** WARNING--THE FIRST ARGUMENT TO DLGPDF ',   &
               'IS LESS THAN 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
      DX=DBLE(IX)
      DTHETA=DBLE(THETA)
!
      DCONST=-1.0D0/DLOG(1.0D0-DTHETA)
      DTERM1=DLOG(DCONST)
!
      DTERM2=DX*DLOG(DTHETA)
      DTERM3=DLOG(DX)
      DTERM4=DTERM1+DTERM2-DTERM3
      DTERM5=DEXP(DTERM4)
      PDF=REAL(DTERM5)
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE DLGPDF
      SUBROUTINE DLGPPF(P,THETA,PPF)
!
!     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
!              FUNCTION VALUE AT THE SINGLE PRECISION VALUE P
!              FOR THE LOGARITMIC SERIES DISTRIBUTION
!     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE
!                                AT WHICH THE PERCENT POINT
!                                FUNCTION IS TO BE EVALUATED.
!                                IT SHOULD BE IN THE INTERVAL (0,1).
!                     --THETA  = THE SHAPE PARAMETER
!     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE
!                                DISTRIBUTION FUNCTION VALUE.
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--P SHOULD BE BETWEEN 0 AND 1 (EXCLUSIVELY FOR 1).
!                 --THETA SHOULD BE IN THE INTERVAL (0,1) (EXCLUSIVELY)
!                 --NN SHOULD BE A POSITIVE INTEGER BETWEEN 1 AND MM.
!     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT
!                                POINT FUNCTION VALUE.
!     OUTPUT--THE SINGLE PRECISION PERCENT POINT  .
!             FUNCTION VALUE PPF
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     OTHER DATAPAC   SUBROUTINES NEEDED--DLGCDF.
!     MODE OF INTERNAL OPERATIONS--SINGLE AND DOUBLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     COMMENT--NOTE THAT EVEN THOUGH THE OUTPUT
!              FROM THIS DISCRETE DISTRIBUTION
!              PERCENT POINT FUNCTION
!              SUBROUTINE MUST NECESSARILY BE A
!              DISCRETE INTEGER VALUE,
!              THE OUTPUT VARIABLE PPF IS SINGLE
!              PRECISION IN MODE.
!              PPF HAS BEEN SPECIFIED AS SINGLE
!              PRECISION SO AS TO CONFORM WITH THE DATAPAC
!              CONVENTION THAT ALL OUTPUT VARIABLES FROM ALL
!              DATAPAC SUBROUTINES ARE SINGLE PRECISION.
!              THIS CONVENTION IS BASED ON THE BELIEF THAT
!              1) A MIXTURE OF MODES (FLOATING POINT
!              VERSUS INTEGER) IS INCONSISTENT AND
!              AN UNNECESSARY COMPLICATION
!              IN A DATA ANALYSIS; AND
!              2) FLOATING POINT MACHINE ARITHMETIC
!              (AS OPPOSED TO INTEGER ARITHMETIC)
!              IS THE MORE NATURAL MODE FOR DOING
!              DATA ANALYSIS.
!     REFERENCES--JOHNSON AND KOTZ, DISCRETE
!                 DISTRIBUTIONS, 1994.  CHAPTER 7.
!               --EVANS, HASTINGS, PEACOCK, STATISTICAL
!                 DISTRIBUTIONS--1993, CHAPTER 23.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1966)
!     VERSION NUMBER--95/4
!     ORIGINAL VERSION--APRIL     1995.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      PPF=0.0
      IF(P.LT.0.0.OR.P.GE.1.0)THEN
        WRITE(ICOUT,1)
    1   FORMAT('***** ERROR--THE FIRST ARGUMENT TO DLGPPF ',   &
               'IS OUTSIDE THE ALLOWABLE (0,1) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(THETA.LE.0.0.OR.THETA.GE.1.0)THEN
        WRITE(ICOUT,11)
   11   FORMAT('***** ERROR--THE SECOND ARGUMENT TO DLGPPF (THE ',   &
               'SHAPE PARAMETER) IS OUTSIDE THE (0,1) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)THETA
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
      PPF=1.0
      IX0=1
      IX1=1
      IX2=1
      P0=0.0
      P1=0.0
      P2=0.0
!
!     TREAT CERTAIN SPECIAL CASES IMMEDIATELY--
!     1) P = 0.0
!
      IF(P.EQ.0.0)THEN
        PPF=1.0
        RETURN
      ENDIF
!
!     DETERMINE AN INITIAL APPROXIMATION TO THE LOGARITHMIC SERIES
!     PERCENT POINT.  USE MEAN VALUE = -THETA/[(1-THETA)LOG(1-THETA)]
!
      X2=-THETA/((1.0-THETA)*LOG(1.0-THETA))
      IX2=INT(X2+0.5)
      IF(IX2.LT.5)IX2=5
!
!     DETERMINE UPPER AND LOWER BOUNDS ON THE DESIRED
!     PERCENT POINT BY ITERATING OUT (BOTH BELOW AND ABOVE)
!     FROM THE ORIGINAL APPROXIMATION AT STEPS
!     OF 1 STANDARD DEVIATION.
!     THE RESULTING BOUNDS WILL BE AT MOST
!     1 STANDARD DEVIATION APART.
!
      IX0=1
      IX1=100000
      CONST=-1.0/LOG(1.0-THETA)
      SD=CONST*THETA*(1.0-CONST*THETA)/(1.0-THETA)**2
      IF(SD.GE.1)THEN
        SD=SQRT(SD)
      ELSE
        SD=1.0
      ENDIF
      ISD=INT(SD+1.0)
      CALL DLGCDF(REAL(IX2),THETA,P2)
!
      IF(P2.LT.P)GO TO 210
      GO TO 250
!
  210 CONTINUE
      IX0=IX2
      IF(IX0.LT.1)IX0=1
      I=1
  215 CONTINUE
      IX2=IX0+ISD
      IF(IX2.LT.1)IX2=1
      IF(IX2.GE.IX1)GO TO 275
      CALL DLGCDF(REAL(IX2),THETA,P2)
      IF(P2.GE.P)GO TO 230
      IX0=IX2
!C220 CONTINUE
      I=I+1
      IF(I.LE.1000000)GO TO 215
      WRITE(ICOUT,249)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,222)
      CALL DPWRST('XXX','BUG ')
      GO TO 950
  230 IX1=IX2
      GO TO 275
!
  250 CONTINUE
      IX1=IX2
      I=1
  255 CONTINUE
      IX2=IX1-ISD
      IF(IX2.LT.1)IX2=1
      IF(IX2.LE.IX0)GO TO 275
      CALL DLGCDF(REAL(IX2),THETA,P2)
      IF(P2.LT.P)GO TO 270
      IX1=IX2
!C260 CONTINUE
      I=I+1
      IF(I.LE.1000000)GO TO 255
      WRITE(ICOUT,249)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,262)
      CALL DPWRST('XXX','BUG ')
      GO TO 950
  270 IX0=IX2
!
  275 IF(IX0.EQ.IX1)GO TO 280
      GO TO 295
  280 IF(IX0.EQ.0)GO TO 285
!CCCC IF(IX0.EQ.N)GO TO 290
      WRITE(ICOUT,249)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,282)
      CALL DPWRST('XXX','BUG ')
      GO TO 950
  285 IX1=IX1+1
      GO TO 295
!C290 IX0=IX0-1
!CCCC IF(IX0.LT.1)IX0=1
  295 CONTINUE
!
!     COMPUTE HYPERGEOMETRIC PROBABILITIES FOR THE
!     DERIVED LOWER AND UPPER BOUNDS.
!
      CALL DLGCDF(REAL(IX0),THETA,P0)
      CALL DLGCDF(REAL(IX1),THETA,P1)
!
!     CHECK THE PROBABILITIES FOR PROPER ORDERING
!
      IF(P0.LT.P.AND.P.LE.P1)GO TO 490
      IF(P0.EQ.P)GO TO 410
      IF(P1.EQ.P)GO TO 420
      IF(P0.GT.P1)GO TO 430
      IF(P0.GT.P)GO TO 440
      IF(P1.LT.P)GO TO 450
      WRITE(ICOUT,249)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,401)
      CALL DPWRST('XXX','BUG ')
      GO TO 950
  410 PPF=IX0
      RETURN
  420 PPF=IX1
      RETURN
  430 WRITE(ICOUT,249)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,431)
      CALL DPWRST('XXX','BUG ')
      GO TO 950
  440 CONTINUE
!CCCC WRITE(ICOUT,249)
!CCCC CALL DPWRST('XXX','BUG ')
!CCCC WRITE(ICOUT,441)
!CCCC CALL DPWRST('XXX','BUG ')
      PPF=1.0
      RETURN
!CCCC GO TO 950
  450 WRITE(ICOUT,249)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,451)
      CALL DPWRST('XXX','BUG ')
      GO TO 950
  490 CONTINUE
!
!     THE STOPPING CRITERION IS THAT THE LOWER BOUND
!     AND UPPER BOUND ARE EXACTLY 1 UNIT APART.
!     CHECK TO SEE IF IX1 = IX0 + 1;
!     IF SO, THE ITERATIONS ARE COMPLETE;
!     IF NOT, THEN BISECT, COMPUTE PROBABILIIES,
!     CHECK PROBABILITIES, AND CONTINUE ITERATING
!     UNTIL IX1 = IX0 + 1.
!
  300 IX0P1=IX0+1
      IF(IX1.EQ.IX0P1)GO TO 690
      IX2=(IX0+IX1)/2
      IF(IX2.LT.1)IX2=1
      IF(IX2.EQ.IX0)GO TO 610
      IF(IX2.EQ.IX1)GO TO 620
      CALL DLGCDF(REAL(IX2),THETA,P2)
      IF(P0.LT.P2.AND.P2.LT.P1)GO TO 630
      IF(P2.LE.P0)GO TO 640
      IF(P2.GE.P1)GO TO 650
  610 WRITE(ICOUT,249)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,611)
      CALL DPWRST('XXX','BUG ')
      GO TO 950
  620 WRITE(ICOUT,249)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,611)
      CALL DPWRST('XXX','BUG ')
      GO TO 950
  630 IF(P2.LE.P)GO TO 635
      IX1=IX2
      P1=P2
      GO TO 300
  635 IX0=IX2
      P0=P2
      GO TO 300
  640 WRITE(ICOUT,249)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,641)
      CALL DPWRST('XXX','BUG ')
      GO TO 950
  650 WRITE(ICOUT,249)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,651)
      CALL DPWRST('XXX','BUG ')
      GO TO 950
  690 PPF=IX1
      IF(P0.EQ.P)PPF=IX0
      RETURN
!
  950 WRITE(ICOUT,240)IX0,P0
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,241)IX1,P1
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,242)IX2,P2
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,244)P
      CALL DPWRST('XXX','BUG ')
!
  222 FORMAT('NO UPPER BOUND FOUND AFTER 10**7 ITERATIONS')
  240 FORMAT('IX0  = ',I8,10X,'P0 = ',F14.7)
  241 FORMAT('IX1  = ',I8,10X,'P1 = ',F14.7)
  242 FORMAT('IX2  = ',I8,10X,'P2 = ',F14.7)
  244 FORMAT('P    = ',F14.7)
  249 FORMAT('***** INTERNAL ERROR IN DLGPPF SUBROUTINE.')
  262 FORMAT('NO LOWER BOUND FOUND AFTER 10**7 ITERATIONS')
  282 FORMAT('LOWER AND UPPER BOUND IDENTICAL')
  401 FORMAT('IMPOSSIBLE BRANCH CONDITION ENCOUNTERED')
  431 FORMAT('LOWER BOUND PROBABILITY (P0) GREATER THAN ',   &
             'UPPER BOUND PROBABILITY (P1)')
!C441 FORMAT('LOWER BOUND PROBABILITY (P0) GREATER THAN ',
!CCCC1       'INPUT PROBABILITY (P)')
  451 FORMAT('UPPER BOUND PROBABILITY (P1) LESS    THAN ',   &
             'INPUT PROBABILITY (P)')
  611 FORMAT('BISECTION VALUE (X2) = LOWER BOUND (X0)')
!C621 FORMAT('BISECTION VALUE (X2) = UPPER BOUND (X1)')
  641 FORMAT('BISECTION VALUE PROBABILITY (P2) ',   &
             'LESS THAN LOWER BOUND PROBABILITY (P0)')
  651 FORMAT('BISECTION VALUE PROBABILITY (P2) ',   &
             'GREATER THAN UPPER BOUND PROBABILITY (P1)')
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE DLGPPF
      SUBROUTINE DLGRAN(N,THETA,ISEED,X)
!
!     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
!              FROM THE LOGARITHMIC SERIES DISTRIBUTION
!              WITH SINGLE PRECISION 'BERNOULLI PROBABILITY'
!              PARAMETER = THETA.
!              THE LOGARITHMIC SERIES DISTRIBUTION HAS THE
!              PROBABILITY FUNCTION
!              F(X) = [-1/(LOG(1-THETA)]*THETA**X/X
!              THIS DISTRIBUTION IS DEFINED FOR
!              ALL POSITIVE INTEGERS X--X = 1, 2, ... .
!     ALGORITHM--METHOD OF KEMP AS DESCRIBED ON PAGE 548 OF
!                "NON-UNIFORM RANDOM VARIATE GENERATION",
!                LUC DEVROYE, SPRINGER-VERLAG, 1986.
!     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
!                                OF RANDOM NUMBERS TO BE
!                                GENERATED.
!                     --THETA  = THE SINGLE PRECISION VALUE
!                                OF THE SHAPE PARAMETER FOR THE
!                                LOGARITHMIC SERIES DISTRIBUTION.
!                                P SHOULD BE BETWEEN
!                                0.0 AND 1.0 (EXCLUSIVELY).
!     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
!                                (OF DIMENSION AT LEAST N)
!                                INTO WHICH THE GENERATED
!                                RANDOM SAMPLE WILL BE PLACED.
!     OUTPUT--A RANDOM SAMPLE OF SIZE N
!             FROM THE LOGARITHMIC SERIES DISTRIBUTION
!     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
!     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
!                   OF N FOR THIS SUBROUTINE.
!                 --THETA SHOULD BE BETWEEN 0.0 (EXCLUSIVELY)
!                   AND 1.0 (EXCLUSIVELY).
!     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN.
!     FORTRAN LIBRARY SUBROUTINES NEEDED--LOG.
!     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
!     LANGUAGE--ANSI FORTRAN (1977)
!     REFERENCES--LUC DEVROYE, "NIN-UNIFORM RANDOM VARIATE
!                 GENERATION", SPRINGER-VERLAG, 1986.
!     WRITTEN BY--ALAN HECKERT
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
!---------------------------------------------------------------------
!
      DIMENSION X(*)
      DIMENSION XTEMP(1)
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!     CHECK THE INPUT ARGUMENTS FOR ERRORS
!
      IF(N.LT.1)THEN
        WRITE(ICOUT,5)
    5   FORMAT('***** ERROR--THE FIRST ARGUMENT TO DLGRAN IS ',   &
               'NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
   47   FORMAT('      THE VALUE OF THE ARGUMENT IS ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ELSEIF(THETA.LE.0.0.OR.THETA.GE.1.0)THEN
        WRITE(ICOUT,11)
   11   FORMAT('***** ERROR--THE SECOND ARGUMENT TO DLGRAN IS ',   &
               'OUTSIDE THE ALLOWABLE (0,1) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)THETA
   46   FORMAT('      THE VALUE OF THE ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9999
      ENDIF
!
!     GENERATE N UNIFORM (0,1) RANDOM NUMBERS
!
      CALL UNIRAN(N,ISEED,X)
!
!     GENERATE N LOGARITHMIC SERIES RANDOM NUMBERS
!     USING THE KEMP ALGORITHM.
!
      NTEMP=1
      AR=LOG(1-THETA)
      DO 100 I=1,N
        AV=X(I)
        IF(AV.GE.THETA)THEN
          X(I)=1.0
        ELSE
          NTEMP=1
          CALL UNIRAN(NTEMP,ISEED,XTEMP)
          AU=XTEMP(1)
          AQ=1.0-EXP(AR*AU)
          IF(AV.LE.AQ*AQ)THEN
            X(I)=1.0 + LOG(AV)/LOG(AQ)
            X(I)=REAL(INT(X(I)))
          ELSEIF(AQ*AQ.LT.AV .AND. AV.LE.AQ)THEN
            X(I)=1.0
          ELSE
            X(I)=2.0
          ENDIF
        ENDIF
  100 CONTINUE
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE DLGRAN
      DOUBLE PRECISION FUNCTION DLNREL (X)
!***BEGIN PROLOGUE  DLNREL
!***PURPOSE  Evaluate ln(1+X) accurate in the sense of relative error.
!***LIBRARY   SLATEC (FNLIB)
!***CATEGORY  C4B
!***TYPE      DOUBLE PRECISION (ALNREL-S, DLNREL-D, CLNREL-C)
!***KEYWORDS  ELEMENTARY FUNCTIONS, FNLIB, LOGARITHM
!***AUTHOR  Fullerton, W., (LANL)
!***DESCRIPTION
!
! DLNREL(X) calculates the double precision natural logarithm of
! (1.0+X) for double precision argument X.  This routine should
! be used when X is small and accurate to calculate the logarithm
! accurately (in the relative error sense) in the neighborhood
! of 1.0.
!
! Series for ALNR       on the interval -3.75000E-01 to  3.75000E-01
!                                        with weighted error   6.35E-32
!                                         log weighted error  31.20
!                               significant figures required  30.93
!                                    decimal places required  32.01
!
!***REFERENCES  (NONE)
!***ROUTINES CALLED  D1MACH, DCSEVL, INITDS, XERMSG
!***REVISION HISTORY  (YYMMDD)
!   770601  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890531  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!***END PROLOGUE  DLNREL
      DOUBLE PRECISION ALNRCS(43), X, XMIN,  DCSEVL
      LOGICAL FIRST
      SAVE ALNRCS, NLNREL, XMIN, FIRST
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DATA ALNRCS(  1) / +.10378693562743769800686267719098D+1     /
      DATA ALNRCS(  2) / -.13364301504908918098766041553133D+0     /
      DATA ALNRCS(  3) / +.19408249135520563357926199374750D-1     /
      DATA ALNRCS(  4) / -.30107551127535777690376537776592D-2     /
      DATA ALNRCS(  5) / +.48694614797154850090456366509137D-3     /
      DATA ALNRCS(  6) / -.81054881893175356066809943008622D-4     /
      DATA ALNRCS(  7) / +.13778847799559524782938251496059D-4     /
      DATA ALNRCS(  8) / -.23802210894358970251369992914935D-5     /
      DATA ALNRCS(  9) / +.41640416213865183476391859901989D-6     /
      DATA ALNRCS( 10) / -.73595828378075994984266837031998D-7     /
      DATA ALNRCS( 11) / +.13117611876241674949152294345011D-7     /
      DATA ALNRCS( 12) / -.23546709317742425136696092330175D-8     /
      DATA ALNRCS( 13) / +.42522773276034997775638052962567D-9     /
      DATA ALNRCS( 14) / -.77190894134840796826108107493300D-10    /
      DATA ALNRCS( 15) / +.14075746481359069909215356472191D-10    /
      DATA ALNRCS( 16) / -.25769072058024680627537078627584D-11    /
      DATA ALNRCS( 17) / +.47342406666294421849154395005938D-12    /
      DATA ALNRCS( 18) / -.87249012674742641745301263292675D-13    /
      DATA ALNRCS( 19) / +.16124614902740551465739833119115D-13    /
      DATA ALNRCS( 20) / -.29875652015665773006710792416815D-14    /
      DATA ALNRCS( 21) / +.55480701209082887983041321697279D-15    /
      DATA ALNRCS( 22) / -.10324619158271569595141333961932D-15    /
      DATA ALNRCS( 23) / +.19250239203049851177878503244868D-16    /
      DATA ALNRCS( 24) / -.35955073465265150011189707844266D-17    /
      DATA ALNRCS( 25) / +.67264542537876857892194574226773D-18    /
      DATA ALNRCS( 26) / -.12602624168735219252082425637546D-18    /
      DATA ALNRCS( 27) / +.23644884408606210044916158955519D-19    /
      DATA ALNRCS( 28) / -.44419377050807936898878389179733D-20    /
      DATA ALNRCS( 29) / +.83546594464034259016241293994666D-21    /
      DATA ALNRCS( 30) / -.15731559416479562574899253521066D-21    /
      DATA ALNRCS( 31) / +.29653128740247422686154369706666D-22    /
      DATA ALNRCS( 32) / -.55949583481815947292156013226666D-23    /
      DATA ALNRCS( 33) / +.10566354268835681048187284138666D-23    /
      DATA ALNRCS( 34) / -.19972483680670204548314999466666D-24    /
      DATA ALNRCS( 35) / +.37782977818839361421049855999999D-25    /
      DATA ALNRCS( 36) / -.71531586889081740345038165333333D-26    /
      DATA ALNRCS( 37) / +.13552488463674213646502024533333D-26    /
      DATA ALNRCS( 38) / -.25694673048487567430079829333333D-27    /
      DATA ALNRCS( 39) / +.48747756066216949076459519999999D-28    /
      DATA ALNRCS( 40) / -.92542112530849715321132373333333D-29    /
      DATA ALNRCS( 41) / +.17578597841760239233269760000000D-29    /
      DATA ALNRCS( 42) / -.33410026677731010351377066666666D-30    /
      DATA ALNRCS( 43) / +.63533936180236187354180266666666D-31    /
      DATA FIRST /.TRUE./
!***FIRST EXECUTABLE STATEMENT  DLNREL
!
      DLNREL = 0.0
!
      IF (FIRST) THEN
         NLNREL = INITDS (ALNRCS, 43, 0.1*REAL(D1MACH(3)))
         XMIN = -1.0D0 + SQRT(D1MACH(4))
      ENDIF
      FIRST = .FALSE.
!
      IF (X .LE. (-1.D0)) THEN
        WRITE(ICOUT,11)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        DLNREL = 0.0
        RETURN
      ENDIF
   11 FORMAT('***** ERROR FROM DLNREL.  X IS LESS THAN OR ')
   12 FORMAT('      EQUAL TO -1.                             ******')
      IF (X .LT. XMIN) THEN
      WRITE(ICOUT,21)
 21   FORMAT('***** WARNING FROM DLNREL.  ANSWER LESS THAN HALF ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,22)
 22   FORMAT('      PRECISION BECAUSE X IS TOO NEAR -1.       *****')
      CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF (ABS(X).LE.0.375D0) DLNREL = X*(1.D0 -   &
        X*DCSEVL (X/.375D0, ALNRCS, NLNREL))
!
      IF (ABS(X).GT.0.375D0) DLNREL = LOG (1.0D0+X)
!
      RETURN
      END FUNCTION DLNREL 

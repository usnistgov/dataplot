      DOUBLE PRECISION FUNCTION DASUM(N,DX,INCX)
C***BEGIN PROLOGUE  DASUM
C***DATE WRITTEN   791001   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  D1A3A
C***KEYWORDS  ADD,BLAS,DOUBLE PRECISION,LINEAR ALGEBRA,MAGNITUDE,SUM,
C             VECTOR
C***AUTHOR  LAWSON, C. L., (JPL)
C           HANSON, R. J., (SNLA)
C           KINCAID, D. R., (U. OF TEXAS)
C           KROGH, F. T., (JPL)
C***PURPOSE  Sum of magnitudes of d.p. vector components
C***DESCRIPTION
C
C                B L A S  Subprogram
C    Description of Parameters
C
C     --Input--
C        N  number of elements in input vector(s)
C       DX  double precision vector with N elements
C     INCX  storage spacing between elements of DX
C
C     --Output--
C    DASUM  double precision result (zero if N .LE. 0)
C
C     Returns sum of magnitudes of double precision DX.
C     DASUM = sum from 0 to N-1 of DABS(DX(1+I*INCX))
C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,
C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*,
C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL
C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  DASUM
C
      DOUBLE PRECISION DX(1)
C***FIRST EXECUTABLE STATEMENT  DASUM
      DASUM = 0.D0
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1)GOTO 20
C
C        CODE FOR INCREMENTS NOT EQUAL TO 1.
C
      NS = N*INCX
          DO 10 I=1,NS,INCX
          DASUM = DASUM + DABS(DX(I))
   10     CONTINUE
      RETURN
C
C        CODE FOR INCREMENTS EQUAL TO 1.
C
C
C        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 6.
C
   20 M = MOD(N,6)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
         DASUM = DASUM + DABS(DX(I))
   30 CONTINUE
      IF( N .LT. 6 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,6
         DASUM = DASUM + DABS(DX(I)) + DABS(DX(I+1)) + DABS(DX(I+2))
     1   + DABS(DX(I+3)) + DABS(DX(I+4)) + DABS(DX(I+5))
   50 CONTINUE
      RETURN
      END
      SUBROUTINE DASYIK (X, FNU, KODE, FLGIK, RA, ARG, IN, Y)
C***BEGIN PROLOGUE  DASYIK
C***SUBSIDIARY
C***PURPOSE  Subsidiary to DBESI and DBESK
C***LIBRARY   SLATEC
C***TYPE      DOUBLE PRECISION (ASYIK-S, DASYIK-D)
C***AUTHOR  Amos, D. E., (SNLA)
C***DESCRIPTION
C
C                    DASYIK computes Bessel functions I and K
C                  for arguments X.GT.0.0 and orders FNU.GE.35
C                  on FLGIK = 1 and FLGIK = -1 respectively.
C
C                                    INPUT
C
C      X    - Argument, X.GT.0.0D0
C      FNU  - Order of first Bessel function
C      KODE - A parameter to indicate the scaling option
C             KODE=1 returns Y(I)=        I/SUB(FNU+I-1)/(X), I=1,IN
C                    or      Y(I)=        K/SUB(FNU+I-1)/(X), I=1,IN
C                    on FLGIK = 1.0D0 or FLGIK = -1.0D0
C             KODE=2 returns Y(I)=EXP(-X)*I/SUB(FNU+I-1)/(X), I=1,IN
C                    or      Y(I)=EXP( X)*K/SUB(FNU+I-1)/(X), I=1,IN
C                    on FLGIK = 1.0D0 or FLGIK = -1.0D0
C     FLGIK - Selection parameter for I or K FUNCTION
C             FLGIK =  1.0D0 gives the I function
C             FLGIK = -1.0D0 gives the K function
C        RA - SQRT(1.+Z*Z), Z=X/FNU
C       ARG - Argument of the leading exponential
C        IN - Number of functions desired, IN=1 or 2
C
C                                    OUTPUT
C
C         Y - A vector whose first IN components contain the sequence
C
C     Abstract  **** A double precision routine ****
C         DASYIK implements the uniform asymptotic expansion of
C         the I and K Bessel functions for FNU.GE.35 and real
C         X.GT.0.0D0. The forms are identical except for a change
C         in sign of some of the terms. This change in sign is
C         accomplished by means of the FLAG FLGIK = 1 or -1.
C
C***SEE ALSO  DBESI, DBESK
C***ROUTINES CALLED  D1MACH
C***REVISION HISTORY  (YYMMDD)
C   750101  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890911  Removed unnecessary intrinsics.  (WRB)
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900328  Added TYPE section.  (WRB)
C   910408  Updated the AUTHOR section.  (WRB)
C***END PROLOGUE  DASYIK
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
      INTEGER IN, J, JN, K, KK, KODE, L
      DOUBLE PRECISION AK,AP,ARG,C,COEF,CON,ETX,FLGIK,FN,FNU,GLN,RA,
     1 S1, S2, T, TOL, T2, X, Y, Z
      DIMENSION Y(*), C(65), CON(2)
      SAVE CON, C
      DATA CON(1), CON(2)  /
     1        3.98942280401432678D-01,    1.25331413731550025D+00/
      DATA C(1), C(2), C(3), C(4), C(5), C(6), C(7), C(8), C(9), C(10),
     1     C(11), C(12), C(13), C(14), C(15), C(16), C(17), C(18),
     2     C(19), C(20), C(21), C(22), C(23), C(24)/
     3       -2.08333333333333D-01,        1.25000000000000D-01,
     4        3.34201388888889D-01,       -4.01041666666667D-01,
     5        7.03125000000000D-02,       -1.02581259645062D+00,
     6        1.84646267361111D+00,       -8.91210937500000D-01,
     7        7.32421875000000D-02,        4.66958442342625D+00,
     8       -1.12070026162230D+01,        8.78912353515625D+00,
     9       -2.36408691406250D+00,        1.12152099609375D-01,
     1       -2.82120725582002D+01,        8.46362176746007D+01,
     2       -9.18182415432400D+01,        4.25349987453885D+01,
     3       -7.36879435947963D+00,        2.27108001708984D-01,
     4        2.12570130039217D+02,       -7.65252468141182D+02,
     5        1.05999045252800D+03,       -6.99579627376133D+02/
      DATA C(25), C(26), C(27), C(28), C(29), C(30), C(31), C(32),
     1     C(33), C(34), C(35), C(36), C(37), C(38), C(39), C(40),
     2     C(41), C(42), C(43), C(44), C(45), C(46), C(47), C(48)/
     3        2.18190511744212D+02,       -2.64914304869516D+01,
     4        5.72501420974731D-01,       -1.91945766231841D+03,
     5        8.06172218173731D+03,       -1.35865500064341D+04,
     6        1.16553933368645D+04,       -5.30564697861340D+03,
     7        1.20090291321635D+03,       -1.08090919788395D+02,
     8        1.72772750258446D+00,        2.02042913309661D+04,
     9       -9.69805983886375D+04,        1.92547001232532D+05,
     1       -2.03400177280416D+05,        1.22200464983017D+05,
     2       -4.11926549688976D+04,        7.10951430248936D+03,
     3       -4.93915304773088D+02,        6.07404200127348D+00,
     4       -2.42919187900551D+05,        1.31176361466298D+06,
     5       -2.99801591853811D+06,        3.76327129765640D+06/
      DATA C(49), C(50), C(51), C(52), C(53), C(54), C(55), C(56),
     1     C(57), C(58), C(59), C(60), C(61), C(62), C(63), C(64),
     2     C(65)/
     3       -2.81356322658653D+06,        1.26836527332162D+06,
     4       -3.31645172484564D+05,        4.52187689813627D+04,
     5       -2.49983048181121D+03,        2.43805296995561D+01,
     6        3.28446985307204D+06,       -1.97068191184322D+07,
     7        5.09526024926646D+07,       -7.41051482115327D+07,
     8        6.63445122747290D+07,       -3.75671766607634D+07,
     9        1.32887671664218D+07,       -2.78561812808645D+06,
     1        3.08186404612662D+05,       -1.38860897537170D+04,
     2        1.10017140269247D+02/
C***FIRST EXECUTABLE STATEMENT  DASYIK
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
      END
      FUNCTION DAWS (X)
C***BEGIN PROLOGUE  DAWS
C***PURPOSE  Compute Dawson's function.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C8C
C***TYPE      SINGLE PRECISION (DAWS-S, DDAWS-D)
C***KEYWORDS  DAWSON'S FUNCTION, FNLIB, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C DAWS(X) calculates Dawson's integral for real argument X.
C
C Series for DAW        on the interval  0.          to  1.00000D+00
C                                        with weighted error   3.83E-17
C                                         log weighted error  16.42
C                               significant figures required  15.78
C                                    decimal places required  16.97
C
C Series for DAW2       on the interval  0.          to  1.60000D+01
C                                        with weighted error   5.17E-17
C                                         log weighted error  16.29
C                               significant figures required  15.90
C                                    decimal places required  17.02
C
C Series for DAWA       on the interval  0.          to  6.25000D-02
C                                        with weighted error   2.24E-17
C                                         log weighted error  16.65
C                               significant figures required  14.73
C                                    decimal places required  17.36
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  CSEVL, INITS, R1MACH, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   780401  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   920618  Removed space from variable names.  (RWC, WRB)
C***END PROLOGUE  DAWS
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
      DIMENSION DAWCS(13), DAW2CS(29), DAWACS(26)
      LOGICAL FIRST
      SAVE DAWCS, DAW2CS, DAWACS, NTDAW, NTDAW2, NTDAWA,
     1 XSML, XBIG, XMAX, FIRST
      DATA DAWCS( 1) /   -.0063517343 75145949E0 /
      DATA DAWCS( 2) /   -.2294071479 6773869E0 /
      DATA DAWCS( 3) /    .0221305009 39084764E0 /
      DATA DAWCS( 4) /   -.0015492654 53892985E0 /
      DATA DAWCS( 5) /    .0000849732 77156849E0 /
      DATA DAWCS( 6) /   -.0000038282 66270972E0 /
      DATA DAWCS( 7) /    .0000001462 85480625E0 /
      DATA DAWCS( 8) /   -.0000000048 51982381E0 /
      DATA DAWCS( 9) /    .0000000001 42146357E0 /
      DATA DAWCS(10) /   -.0000000000 03728836E0 /
      DATA DAWCS(11) /    .0000000000 00088549E0 /
      DATA DAWCS(12) /   -.0000000000 00001920E0 /
      DATA DAWCS(13) /    .0000000000 00000038E0 /
      DATA DAW2CS( 1) /   -.0568865441 05215527E0 /
      DATA DAW2CS( 2) /   -.3181134699 6168131E0 /
      DATA DAW2CS( 3) /    .2087384541 3642237E0 /
      DATA DAW2CS( 4) /   -.1247540991 3779131E0 /
      DATA DAW2CS( 5) /    .0678693051 86676777E0 /
      DATA DAW2CS( 6) /   -.0336591448 95270940E0 /
      DATA DAW2CS( 7) /    .0152607812 71987972E0 /
      DATA DAW2CS( 8) /   -.0063483709 62596214E0 /
      DATA DAW2CS( 9) /    .0024326740 92074852E0 /
      DATA DAW2CS(10) /   -.0008621954 14910650E0 /
      DATA DAW2CS(11) /    .0002837657 33363216E0 /
      DATA DAW2CS(12) /   -.0000870575 49874170E0 /
      DATA DAW2CS(13) /    .0000249868 49985481E0 /
      DATA DAW2CS(14) /   -.0000067319 28676416E0 /
      DATA DAW2CS(15) /    .0000017078 57878557E0 /
      DATA DAW2CS(16) /   -.0000004091 75512264E0 /
      DATA DAW2CS(17) /    .0000000928 28292216E0 /
      DATA DAW2CS(18) /   -.0000000199 91403610E0 /
      DATA DAW2CS(19) /    .0000000040 96349064E0 /
      DATA DAW2CS(20) /   -.0000000008 00324095E0 /
      DATA DAW2CS(21) /    .0000000001 49385031E0 /
      DATA DAW2CS(22) /   -.0000000000 26687999E0 /
      DATA DAW2CS(23) /    .0000000000 04571221E0 /
      DATA DAW2CS(24) /   -.0000000000 00751873E0 /
      DATA DAW2CS(25) /    .0000000000 00118931E0 /
      DATA DAW2CS(26) /   -.0000000000 00018116E0 /
      DATA DAW2CS(27) /    .0000000000 00002661E0 /
      DATA DAW2CS(28) /   -.0000000000 00000377E0 /
      DATA DAW2CS(29) /    .0000000000 00000051E0 /
      DATA DAWACS( 1) /    .0169048563 7765704E0 /
      DATA DAWACS( 2) /    .0086832522 7840695E0 /
      DATA DAWACS( 3) /    .0002424864 0424177E0 /
      DATA DAWACS( 4) /    .0000126118 2399572E0 /
      DATA DAWACS( 5) /    .0000010664 5331463E0 /
      DATA DAWACS( 6) /    .0000001358 1597947E0 /
      DATA DAWACS( 7) /    .0000000217 1042356E0 /
      DATA DAWACS( 8) /    .0000000028 6701050E0 /
      DATA DAWACS( 9) /   -.0000000001 9013363E0 /
      DATA DAWACS(10) /   -.0000000003 0977804E0 /
      DATA DAWACS(11) /   -.0000000001 0294148E0 /
      DATA DAWACS(12) /   -.0000000000 0626035E0 /
      DATA DAWACS(13) /    .0000000000 0856313E0 /
      DATA DAWACS(14) /    .0000000000 0303304E0 /
      DATA DAWACS(15) /   -.0000000000 0025236E0 /
      DATA DAWACS(16) /   -.0000000000 0042106E0 /
      DATA DAWACS(17) /   -.0000000000 0004431E0 /
      DATA DAWACS(18) /    .0000000000 0004911E0 /
      DATA DAWACS(19) /    .0000000000 0001235E0 /
      DATA DAWACS(20) /   -.0000000000 0000578E0 /
      DATA DAWACS(21) /   -.0000000000 0000228E0 /
      DATA DAWACS(22) /    .0000000000 0000076E0 /
      DATA DAWACS(23) /    .0000000000 0000038E0 /
      DATA DAWACS(24) /   -.0000000000 0000011E0 /
      DATA DAWACS(25) /   -.0000000000 0000006E0 /
      DATA DAWACS(26) /    .0000000000 0000002E0 /
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  DAWS
      IF (FIRST) THEN
         EPS = R1MACH(3)
         NTDAW  = INITS (DAWCS,  13, 0.1*EPS)
         NTDAW2 = INITS (DAW2CS, 29, 0.1*EPS)
         NTDAWA = INITS (DAWACS, 26, 0.1*EPS)
C
         XSML = SQRT (1.5*EPS)
         XBIG = SQRT (0.5/EPS)
         XMAX = EXP (MIN (-LOG(2.*R1MACH(1)), LOG(R1MACH(2))) - 1.0)
      ENDIF
      FIRST = .FALSE.
C
      Y = ABS(X)
      IF (Y.GT.1.0) GO TO 20
C
      DAWS = X
      IF (Y.LE.XSML) RETURN
C
      DAWS = X * (0.75 + CSEVL (2.0*Y*Y-1.0, DAWCS, NTDAW))
      RETURN
C
 20   IF (Y.GT.4.0) GO TO 30
      DAWS = X * (0.25 + CSEVL (0.125*Y*Y-1.0, DAW2CS, NTDAW2))
      RETURN
C
 30   IF (Y.GT.XMAX) GO TO 40
      DAWS = 0.5/X
      IF (Y.GT.XBIG) RETURN
C
      DAWS = (0.5 + CSEVL (32.0/Y**2-1.0, DAWACS, NTDAWA)) / X
      RETURN
C
 40   CONTINUE
      WRITE(ICOUT,41)
      CALL DPWRST('XXX','BUG ')
   41 FORMAT('***** WARNING FROM DAWS, UNDERFLOW BECAUSE THE ',
     1       'ABSOLUTE VALUE OF X IS SO LARGE.  ****')
      DAWS = 0.0
      RETURN
C
      END
      SUBROUTINE DAXPY(N,DA,DX,INCX,DY,INCY)
C
C     CONSTANT TIMES A VECTOR PLUS A VECTOR.
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
      DOUBLE PRECISION DX(*),DY(*),DA
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N
C
      IF(N.LE.0)RETURN
      IF (DA .EQ. 0.0D0) RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
C          NOT EQUAL TO 1
C
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
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C
C        CLEAN-UP LOOP
C
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
      END
      SUBROUTINE DBESI (X, ALPHA, KODE, N, Y, NZ)
C***BEGIN PROLOGUE  DBESI
C***PURPOSE  Compute an N member sequence of I Bessel functions
C            I/SUB(ALPHA+K-1)/(X), K=1,...,N or scaled Bessel functions
C            EXP(-X)*I/SUB(ALPHA+K-1)/(X), K=1,...,N for nonnegative
C            ALPHA and X.
C***LIBRARY   SLATEC
C***CATEGORY  C10B3
C***TYPE      DOUBLE PRECISION (BESI-S, DBESI-D)
C***KEYWORDS  I BESSEL FUNCTION, SPECIAL FUNCTIONS
C***AUTHOR  Amos, D. E., (SNLA)
C           Daniel, S. L., (SNLA)
C***DESCRIPTION
C
C     Abstract  **** a double precision routine ****
C         DBESI computes an N member sequence of I Bessel functions
C         I/sub(ALPHA+K-1)/(X), K=1,...,N or scaled Bessel functions
C         EXP(-X)*I/sub(ALPHA+K-1)/(X), K=1,...,N for nonnegative ALPHA
C         and X.  A combination of the power series, the asymptotic
C         expansion for X to infinity, and the uniform asymptotic
C         expansion for NU to infinity are applied over subdivisions of
C         the (NU,X) plane.  For values not covered by one of these
C         formulae, the order is incremented by an integer so that one
C         of these formulae apply.  Backward recursion is used to reduce
C         orders by integer values.  The asymptotic expansion for X to
C         infinity is used only when the entire sequence (specifically
C         the last member) lies within the region covered by the
C         expansion.  Leading terms of these expansions are used to test
C         for over or underflow where appropriate.  If a sequence is
C         requested and the last member would underflow, the result is
C         set to zero and the next lower order tried, etc., until a
C         member comes on scale or all are set to zero.  An overflow
C         cannot occur with scaling.
C
C         The maximum number of significant digits obtainable
C         is the smaller of 14 and the number of digits carried in
C         double precision arithmetic.
C
C     Description of Arguments
C
C         Input      X,ALPHA are double precision
C           X      - X .GE. 0.0D0
C           ALPHA  - order of first member of the sequence,
C                    ALPHA .GE. 0.0D0
C           KODE   - a parameter to indicate the scaling option
C                    KODE=1 returns
C                           Y(K)=        I/sub(ALPHA+K-1)/(X),
C                                K=1,...,N
C                    KODE=2 returns
C                           Y(K)=EXP(-X)*I/sub(ALPHA+K-1)/(X),
C                                K=1,...,N
C           N      - number of members in the sequence, N .GE. 1
C
C         Output     Y is double precision
C           Y      - a vector whose first N components contain
C                    values for I/sub(ALPHA+K-1)/(X) or scaled
C                    values for EXP(-X)*I/sub(ALPHA+K-1)/(X),
C                    K=1,...,N depending on KODE
C           NZ     - number of components of Y set to zero due to
C                    underflow,
C                    NZ=0   , normal return, computation completed
C                    NZ .NE. 0, last NZ components of Y set to zero,
C                             Y(K)=0.0D0, K=N-NZ+1,...,N.
C
C     Error Conditions
C         Improper input arguments - a fatal error
C         Overflow with KODE=1 - a fatal error
C         Underflow - a non-fatal error(NZ .NE. 0)
C
C***REFERENCES  D. E. Amos, S. L. Daniel and M. K. Weston, CDC 6600
C                 subroutines IBESS and JBESS for Bessel functions
C                 I(NU,X) and J(NU,X), X .GE. 0, NU .GE. 0, ACM
C                 Transactions on Mathematical Software 3, (1977),
C                 pp. 76-92.
C               F. W. J. Olver, Tables of Bessel Functions of Moderate
C                 or Large Orders, NPL Mathematical Tables 6, Her
C                 Majesty's Stationery Office, London, 1962.
C***ROUTINES CALLED  D1MACH, DASYIK, DLNGAM, I1MACH, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   750101  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890911  Removed unnecessary intrinsics.  (WRB)
C   890911  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900326  Removed duplicate information from DESCRIPTION section.
C           (WRB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  DBESI
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
      INTEGER I, IALP, IN, INLIM, IS, I1, K, KK, KM, KODE, KT,
     1        N, NN, NS, NZ
      INTEGER I1MACH
      DOUBLE PRECISION AIN,AK,AKM,ALPHA,ANS,AP,ARG,ATOL,TOLLN,DFN,
     1 DTM, DX, EARG, ELIM, ETX, FLGIK,FN, FNF, FNI,FNP1,FNU,GLN,RA,
     2 RTTPI, S, SX, SXO2, S1, S2, T, TA, TB, TEMP, TFN, TM, TOL,
     3 TRX, T2, X, XO2, XO2L, Y, Z
      DOUBLE PRECISION DLNGAM
      DIMENSION Y(*), TEMP(3)
      SAVE RTTPI, INLIM
      DATA RTTPI           / 3.98942280401433D-01/
      DATA INLIM           /          80         /
C***FIRST EXECUTABLE STATEMENT  DBESI
C
      NZ = 0
      KT = 1
      NS = 0
      KM = 0
      XO2L = 0.0D0
C
C     I1MACH(15) REPLACES I1MACH(12) IN A DOUBLE PRECISION CODE
C     I1MACH(14) REPLACES I1MACH(11) IN A DOUBLE PRECISION CODE
      RA = D1MACH(3)
      TOL = MAX(RA,1.0D-15)
      I1 = -I1MACH(15)
      GLN = D1MACH(5)
      ELIM = 2.303D0*(I1*GLN-3.0D0)
C     TOLLN = -LN(TOL)
      I1 = I1MACH(14)+1
      TOLLN = 2.303D0*GLN*I1
      TOLLN = MIN(TOLLN,34.5388D0)
CCCCC IF (N-1) 590, 10, 20
      IF (N-1.LT.0) THEN
         GOTO 590
      ELSEIF (N-1.EQ.0) THEN
         GOTO 10
      ELSEIF (N-1.GT.0) THEN
         GOTO 20
      ENDIF
   10 KT = 2
   20 NN = N
      IF (KODE.LT.1 .OR. KODE.GT.2) GO TO 570
CCCCC IF (X) 600, 30, 80
      IF (X.LT.0.0D0) THEN
         GOTO 600
      ELSEIF (X.EQ.0.0D0) THEN
         GOTO 30
      ELSEIF (X.GT.0.0D0) THEN
         GOTO 80
      ENDIF
   30 CONTINUE
CCCCC IF (ALPHA) 580, 40, 50
      IF (ALPHA.LT.0.0D0)THEN
         GOTO 580
      ELSEIF (ALPHA.EQ.0.0D0)THEN
         GOTO 40
      ELSEIF (ALPHA.GT.0.0D0)THEN
         GOTO 50
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
C
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
C
C     DECISION TREE FOR REGION WHERE SERIES, ASYMPTOTIC EXPANSION FOR X
C     TO INFINITY AND ASYMPTOTIC EXPANSION FOR NU TO INFINITY ARE
C     APPLIED.
C
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
C
C     OVERFLOW TEST ON UNIFORM ASYMPTOTIC EXPANSION
C
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
C
C     UNDERFLOW TEST ON UNIFORM ASYMPTOTIC EXPANSION
C
      Z = X/FN
      RA = SQRT(1.0D0+Z*Z)
      GLN = LOG((1.0D0+RA)/Z)
      T = RA*(1.0D0-ETX) + ETX/(Z+RA)
      ARG = FN*(T-GLN)
  140 IF (ARG.LT.(-ELIM)) GO TO 280
      GO TO 190
  150 IF (X.GT.ELIM) GO TO 610
      GO TO 130
C
C     UNIFORM ASYMPTOTIC EXPANSION FOR NU TO INFINITY
C
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
C
C     SERIES FOR (X/2)**2.LE.NU+1
C
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
C
C     SET UNDERFLOW VALUE AND UPDATE PARAMETERS
C
  280 Y(NN) = 0.0D0
      NN = NN - 1
      FNI = FNI - 1.0D0
      DFN = FNI + FNF
      FN = DFN
CCCCC IF (NN-1) 340, 290, 130
      IF (NN-1.LT.0) THEN
         GOTO 340
      ELSEIF (NN-1.EQ.0) THEN
         GOTO 290
      ELSEIF (NN-1.GT.0) THEN
         GOTO 130
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
CCCCC IF (NN-1) 340, 310, 320
      IF (NN-1.LT.0)THEN
         GOTO340
      ELSEIF(NN-1.EQ.0)THEN
         GOTO310
      ELSE
         GOTO320
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
C
C     BACKWARD RECURSION SECTION
C
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
C     BACKWARD RECUR TO INDEX ALPHA+NN-1
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
C     BACKWARD RECUR FROM INDEX ALPHA+NN-1 TO ALPHA
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
C
C     ASYMPTOTIC EXPANSION FOR X TO INFINITY
C
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
C
C     BACKWARD RECURSION WITH NORMALIZATION BY
C     ASYMPTOTIC EXPANSION FOR NU TO INFINITY OR POWER SERIES.
C
  500 CONTINUE
C     COMPUTATION OF LAST ORDER FOR SERIES NORMALIZATION
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
C     COMPUTATION OF LAST ORDER FOR ASYMPTOTIC EXPANSION NORMALIZATION
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
C
C     BACKWARD RECUR UNINDEXED
C
      DO 540 I=1,IN
        S = TB
        TB = TM*TB + TA
        TA = S
        DTM = DTM - 1.0D0
        TM = (DTM+FNF)*TRX
  540 CONTINUE
C     NORMALIZATION
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
C
C     BACKWARD RECUR INDEXED
C
      DO 560 I=1,KM
        Y(K-1) = TM*Y(K) + Y(K+1)
        DTM = DTM - 1.0D0
        TM = (DTM+FNF)*TRX
        K = K - 1
  560 CONTINUE
      RETURN
C
C
C
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
      END
      DOUBLE PRECISION FUNCTION DBESI0 (X)
C***BEGIN PROLOGUE  DBESI0
C***PURPOSE  Compute the hyperbolic Bessel function of the first kind
C            of order zero.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C10B1
C***TYPE      DOUBLE PRECISION (BESI0-S, DBESI0-D)
C***KEYWORDS  FIRST KIND, FNLIB, HYPERBOLIC BESSEL FUNCTION,
C             MODIFIED BESSEL FUNCTION, ORDER ZERO, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C DBESI0(X) calculates the double precision modified (hyperbolic)
C Bessel function of the first kind of order zero and double
C precision argument X.
C
C Series for BI0        on the interval  0.          to  9.00000E+00
C                                        with weighted error   9.51E-34
C                                         log weighted error  33.02
C                               significant figures required  33.31
C                                    decimal places required  33.65
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, DBSI0E, DCSEVL, INITDS, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C***END PROLOGUE  DBESI0
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
      DOUBLE PRECISION X, BI0CS(18), XMAX, XSML, Y,
     1  DCSEVL, DBSI0E
      LOGICAL FIRST
      SAVE BI0CS, NTI0, XSML, XMAX, FIRST
      DATA BI0CS(  1) / -.7660547252 8391449510 8189497624 3285 D-1   /
      DATA BI0CS(  2) / +.1927337953 9938082699 5240875088 1196 D+1   /
      DATA BI0CS(  3) / +.2282644586 9203013389 3702929233 0415 D+0   /
      DATA BI0CS(  4) / +.1304891466 7072904280 7933421069 1888 D-1   /
      DATA BI0CS(  5) / +.4344270900 8164874513 7868268102 6107 D-3   /
      DATA BI0CS(  6) / +.9422657686 0019346639 2317174411 8766 D-5   /
      DATA BI0CS(  7) / +.1434006289 5106910799 6209187817 9957 D-6   /
      DATA BI0CS(  8) / +.1613849069 6617490699 1541971999 4611 D-8   /
      DATA BI0CS(  9) / +.1396650044 5356696994 9509270814 2522 D-10  /
      DATA BI0CS( 10) / +.9579451725 5054453446 2752317189 3333 D-13  /
      DATA BI0CS( 11) / +.5333981859 8625021310 1510774400 0000 D-15  /
      DATA BI0CS( 12) / +.2458716088 4374707746 9678591999 9999 D-17  /
      DATA BI0CS( 13) / +.9535680890 2487700269 4434133333 3333 D-20  /
      DATA BI0CS( 14) / +.3154382039 7214273367 8933333333 3333 D-22  /
      DATA BI0CS( 15) / +.9004564101 0946374314 6666666666 6666 D-25  /
      DATA BI0CS( 16) / +.2240647369 1236700160 0000000000 0000 D-27  /
      DATA BI0CS( 17) / +.4903034603 2428373333 3333333333 3333 D-30  /
      DATA BI0CS( 18) / +.9508172606 1226666666 6666666666 6666 D-33  /
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  DBESI0
      IF (FIRST) THEN
         NTI0 = INITDS (BI0CS, 18, 0.1*REAL(D1MACH(3)))
         XSML = SQRT(4.5D0*D1MACH(3))
         XMAX = LOG (D1MACH(2))
      ENDIF
      FIRST = .FALSE.
C
      Y = ABS(X)
      IF (Y.GT.3.0D0) GO TO 20
C
      DBESI0 = 1.0D0
      IF (Y.GT.XSML) DBESI0 = 2.75D0 + DCSEVL (Y*Y/4.5D0-1.D0, BI0CS,
     1  NTI0)
      RETURN
C
 20   CONTINUE
      IF (Y.GT.XMAX) THEN
        WRITE(ICOUT,1)
        CALL DPWRST('XXX','BUG ')
        DBESI0 = 0.0D0
        RETURN
      ENDIF
    1 FORMAT('***** ERORR FROM DBESI0, OVERFLOW BECAUSE THE ',
     1       'ABSOLUTE VALUE OF X IS TOO BIG.  ****')
C
      DBESI0 = EXP(Y) * DBSI0E(X)
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION DBESI1 (X)
C***BEGIN PROLOGUE  DBESI1
C***PURPOSE  Compute the modified (hyperbolic) Bessel function of the
C            first kind of order one.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C10B1
C***TYPE      DOUBLE PRECISION (BESI1-S, DBESI1-D)
C***KEYWORDS  FIRST KIND, FNLIB, HYPERBOLIC BESSEL FUNCTION,
C             MODIFIED BESSEL FUNCTION, ORDER ONE, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C DBESI1(X) calculates the double precision modified (hyperbolic)
C Bessel function of the first kind of order one and double precision
C argument X.
C
C Series for BI1        on the interval  0.          to  9.00000E+00
C                                        with weighted error   1.44E-32
C                                         log weighted error  31.84
C                               significant figures required  31.45
C                                    decimal places required  32.46
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, DBSI1E, DCSEVL, INITDS, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C***END PROLOGUE  DBESI1
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
      DOUBLE PRECISION X, BI1CS(17), XMAX, XMIN, XSML, Y,
     1  DCSEVL, DBSI1E
      LOGICAL FIRST
      SAVE BI1CS, NTI1, XMIN, XSML, XMAX, FIRST
      DATA BI1CS(  1) / -.1971713261 0998597316 1385032181 49 D-2     /
      DATA BI1CS(  2) / +.4073488766 7546480608 1553936520 14 D+0     /
      DATA BI1CS(  3) / +.3483899429 9959455866 2450377837 87 D-1     /
      DATA BI1CS(  4) / +.1545394556 3001236038 5984010584 89 D-2     /
      DATA BI1CS(  5) / +.4188852109 8377784129 4588320041 20 D-4     /
      DATA BI1CS(  6) / +.7649026764 8362114741 9597039660 69 D-6     /
      DATA BI1CS(  7) / +.1004249392 4741178689 1798080372 38 D-7     /
      DATA BI1CS(  8) / +.9932207791 9238106481 3712980548 63 D-10    /
      DATA BI1CS(  9) / +.7663801791 8447637275 2001716813 49 D-12    /
      DATA BI1CS( 10) / +.4741418923 8167394980 3880919481 60 D-14    /
      DATA BI1CS( 11) / +.2404114404 0745181799 8631720320 00 D-16    /
      DATA BI1CS( 12) / +.1017150500 7093713649 1211007999 99 D-18    /
      DATA BI1CS( 13) / +.3645093565 7866949458 4917333333 33 D-21    /
      DATA BI1CS( 14) / +.1120574950 2562039344 8106666666 66 D-23    /
      DATA BI1CS( 15) / +.2987544193 4468088832 0000000000 00 D-26    /
      DATA BI1CS( 16) / +.6973231093 9194709333 3333333333 33 D-29    /
      DATA BI1CS( 17) / +.1436794822 0620800000 0000000000 00 D-31    /
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  DBESI1
      IF (FIRST) THEN
         NTI1 = INITDS (BI1CS, 17, 0.1*REAL(D1MACH(3)))
         XMIN = 2.0D0*D1MACH(1)
         XSML = SQRT(4.5D0*D1MACH(3))
         XMAX = LOG (D1MACH(2))
      ENDIF
      FIRST = .FALSE.
C
      Y = ABS(X)
      IF (Y.GT.3.0D0) GO TO 20
C
      DBESI1 = 0.D0
      IF (Y.EQ.0.D0)  RETURN
C
      IF (Y .LE. XMIN) THEN
        WRITE(ICOUT,2)
        CALL DPWRST('XXX','BUG ')
      ENDIF
    2 FORMAT('***** WARNING FROM DBESI1, UNDERFLOW BECAUSE THE ',
     1       'ABSOLUTE VALUE OF X IS SO SMALL.  ****')
      IF (Y.GT.XMIN) DBESI1 = 0.5D0*X
      IF (Y.GT.XSML) DBESI1 = X*(0.875D0 + DCSEVL (Y*Y/4.5D0-1.D0,
     1  BI1CS, NTI1))
      RETURN
C
 20   CONTINUE
      IF (Y.GT.XMAX) THEN
        WRITE(ICOUT,1)
        CALL DPWRST('XXX','BUG ')
        DBESI1 = 0.0
        RETURN
      ENDIF
    1 FORMAT('***** ERORR FROM DBESI1, OVERFLOW BECAUSE THE ',
     1       'ABSOLUTE VALUE OF X IS TOO BIG.  ****')
C
      DBESI1 = EXP(Y) * DBSI1E(X)
C
      RETURN
      END
      SUBROUTINE DBESK (X, FNU, KODE, N, Y, NZ)
C***BEGIN PROLOGUE  DBESK
C***PURPOSE  Implement forward recursion on the three term recursion
C            relation for a sequence of non-negative order Bessel
C            functions K/SUB(FNU+I-1)/(X), or scaled Bessel functions
C            EXP(X)*K/SUB(FNU+I-1)/(X), I=1,...,N for real, positive
C            X and non-negative orders FNU.
C***LIBRARY   SLATEC
C***CATEGORY  C10B3
C***TYPE      DOUBLE PRECISION (BESK-S, DBESK-D)
C***KEYWORDS  K BESSEL FUNCTION, SPECIAL FUNCTIONS
C***AUTHOR  Amos, D. E., (SNLA)
C***DESCRIPTION
C
C     Abstract  **** a double precision routine ****
C         DBESK implements forward recursion on the three term
C         recursion relation for a sequence of non-negative order Bessel
C         functions K/sub(FNU+I-1)/(X), or scaled Bessel functions
C         EXP(X)*K/sub(FNU+I-1)/(X), I=1,..,N for real X .GT. 0.0D0 and
C         non-negative orders FNU.  If FNU .LT. NULIM, orders FNU and
C         FNU+1 are obtained from DBSKNU to start the recursion.  If
C         FNU .GE. NULIM, the uniform asymptotic expansion is used for
C         orders FNU and FNU+1 to start the recursion.  NULIM is 35 or
C         70 depending on whether N=1 or N .GE. 2.  Under and overflow
C         tests are made on the leading term of the asymptotic expansion
C         before any extensive computation is done.
C
C         The maximum number of significant digits obtainable
C         is the smaller of 14 and the number of digits carried in
C         double precision arithmetic.
C
C     Description of Arguments
C
C         Input      X,FNU are double precision
C           X      - X .GT. 0.0D0
C           FNU    - order of the initial K function, FNU .GE. 0.0D0
C           KODE   - a parameter to indicate the scaling option
C                    KODE=1 returns Y(I)=       K/sub(FNU+I-1)/(X),
C                                        I=1,...,N
C                    KODE=2 returns Y(I)=EXP(X)*K/sub(FNU+I-1)/(X),
C                                        I=1,...,N
C           N      - number of members in the sequence, N .GE. 1
C
C         Output     Y is double precision
C           Y      - a vector whose first N components contain values
C                    for the sequence
C                    Y(I)=       k/sub(FNU+I-1)/(X), I=1,...,N  or
C                    Y(I)=EXP(X)*K/sub(FNU+I-1)/(X), I=1,...,N
C                    depending on KODE
C           NZ     - number of components of Y set to zero due to
C                    underflow with KODE=1,
C                    NZ=0   , normal return, computation completed
C                    NZ .NE. 0, first NZ components of Y set to zero
C                             due to underflow, Y(I)=0.0D0, I=1,...,NZ
C
C     Error Conditions
C         Improper input arguments - a fatal error
C         Overflow - a fatal error
C         Underflow with KODE=1 -  a non-fatal error (NZ .NE. 0)
C
C***REFERENCES  F. W. J. Olver, Tables of Bessel Functions of Moderate
C                 or Large Orders, NPL Mathematical Tables 6, Her
C                 Majesty's Stationery Office, London, 1962.
C               N. M. Temme, On the numerical evaluation of the modified
C                 Bessel function of the third kind, Journal of
C                 Computational Physics 19, (1975), pp. 324-337.
C***ROUTINES CALLED  D1MACH, DASYIK, DBESK0, DBESK1, DBSK0E, DBSK1E,
C                    DBSKNU, I1MACH, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   790201  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890911  Removed unnecessary intrinsics.  (WRB)
C   890911  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  DBESK
C
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
      INTEGER I, J, K, KODE, MZ, N, NB, ND, NN, NUD, NULIM, NZ
      DOUBLE PRECISION CN,DNU,ELIM,ETX,FLGIK,FN,FNN,FNU,GLN,GNU,RTZ,
     1 S, S1, S2, T, TM, TRX, W, X, XLIM, Y, ZN
      DOUBLE PRECISION DBESK0, DBESK1, DBSK1E, DBSK0E
      DIMENSION W(2), NULIM(2), Y(*)
      SAVE NULIM
      DATA NULIM(1),NULIM(2) / 35 , 70 /
C***FIRST EXECUTABLE STATEMENT  DBESK
C
      TRX=0.0D0
      TM=0.0D0
      S2=0.0D0
C
      NN = -I1MACH(15)
      ELIM = 2.303D0*(NN*D1MACH(5)-3.0D0)
      XLIM = D1MACH(1)*1.0D+3
      IF (KODE.LT.1 .OR. KODE.GT.2) GO TO 280
      IF (FNU.LT.0.0D0) GO TO 290
      IF (X.LE.0.0D0) GO TO 300
      IF (X.LT.XLIM) GO TO 320
      IF (N.LT.1) GO TO 310
      ETX = KODE - 1
C
C     ND IS A DUMMY VARIABLE FOR N
C     GNU IS A DUMMY VARIABLE FOR FNU
C     NZ = NUMBER OF UNDERFLOWS ON KODE=1
C
      ND = N
      NZ = 0
      NUD = INT(FNU)
      DNU = FNU - NUD
      GNU = FNU
      NN = MIN(2,ND)
      FN = FNU + N - 1
      FNN = FN
      IF (FN.LT.2.0D0) GO TO 150
C
C     OVERFLOW TEST  (LEADING EXPONENTIAL OF ASYMPTOTIC EXPANSION)
C     FOR THE LAST ORDER, FNU+N-1.GE.NULIM
C
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
C
C     UNDERFLOW TEST (LEADING EXPONENTIAL OF ASYMPTOTIC EXPANSION)
C     FOR THE FIRST ORDER, FNU.GE.NULIM
C
      FN = GNU
      ZN = X/FN
      RTZ = SQRT(1.0D0+ZN*ZN)
      GLN = LOG((1.0D0+RTZ)/ZN)
      T = RTZ*(1.0D0-ETX) + ETX/(ZN+RTZ)
      CN = -FN*(T-GLN)
   20 CONTINUE
      IF (CN.LT.-ELIM) GO TO 230
C
C     ASYMPTOTIC EXPANSION FOR ORDERS FNU AND FNU+1.GE.NULIM
C
      FLGIK = -1.0D0
      CALL DASYIK(X,GNU,KODE,FLGIK,RTZ,CN,NN,Y)
      IF (NN.EQ.1) GO TO 240
      TRX = 2.0D0/X
      TM = (GNU+GNU+2.0D0)/X
      GO TO 130
C
   30 CONTINUE
      IF (KODE.EQ.2) GO TO 40
C
C     UNDERFLOW TEST (LEADING EXPONENTIAL OF ASYMPTOTIC EXPANSION IN X)
C     FOR ORDER DNU
C
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
C     FORWARD RECUR FROM DNU TO FNU+1 TO GET Y(1) AND Y(2)
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
C     FORWARD RECUR FROM FNU+2 TO FNU+N-1
      DO 140 I=3,ND
        Y(I) = TM*Y(I-1) + Y(I-2)
        TM = TM + TRX
  140 CONTINUE
      GO TO 240
C
  150 CONTINUE
C     UNDERFLOW TEST FOR KODE=1
      IF (KODE.EQ.2) GO TO 160
      IF (X.GT.ELIM) GO TO 230
  160 CONTINUE
C     OVERFLOW TEST
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
C
C     UPDATE PARAMETERS ON UNDERFLOW
C
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
C
C
C
  280 CONTINUE
CCCCC CALL XERMSG ('SLATEC', 'DBESK',
CCCCC+   'SCALING OPTION, KODE, NOT 1 OR 2', 2, 1)
CCCCC RETURN
CC290 CONTINUE
CCCCC CALL XERMSG ('SLATEC', 'DBESK', 'ORDER, FNU, LESS THAN ZERO', 2,
CCCCC+   1)
CCCCC RETURN
CC300 CONTINUE
CCCCC CALL XERMSG ('SLATEC', 'DBESK', 'X LESS THAN OR EQUAL TO ZERO',
CCCCC+   2, 1)
CCCCC RETURN
CC310 CONTINUE
CCCCC CALL XERMSG ('SLATEC', 'DBESK', 'N LESS THAN ONE', 2, 1)
CCCCC RETURN
CC320 CONTINUE
CCCCC CALL XERMSG ('SLATEC', 'DBESK',
CCCCC+   'OVERFLOW, FNU OR N TOO LARGE OR X TOO SMALL', 6, 1)
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
  321 FORMAT('***** ERORR FROM DBESK, OVERFLOW, FNU OR N TOO LARGE OR',
     1       ' X TOO SMALL.')
      CALL DPWRST('XXX','BUG ')
      RETURN
      END
      DOUBLE PRECISION FUNCTION DBESK0 (X)
C***BEGIN PROLOGUE  DBESK0
C***PURPOSE  Compute the modified (hyperbolic) Bessel function of the
C            third kind of order zero.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C10B1
C***TYPE      DOUBLE PRECISION (BESK0-S, DBESK0-D)
C***KEYWORDS  FNLIB, HYPERBOLIC BESSEL FUNCTION,
C             MODIFIED BESSEL FUNCTION, ORDER ZERO, SPECIAL FUNCTIONS,
C             THIRD KIND
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C DBESK0(X) calculates the double precision modified (hyperbolic)
C Bessel function of the third kind of order zero for double
C precision argument X.  The argument must be greater than zero
C but not so large that the result underflows.
C
C Series for BK0        on the interval  0.          to  4.00000E+00
C                                        with weighted error   3.08E-33
C                                         log weighted error  32.51
C                               significant figures required  32.05
C                                    decimal places required  33.11
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, DBESI0, DBSK0E, DCSEVL, INITDS, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C***END PROLOGUE  DBESK0
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
      DOUBLE PRECISION X, BK0CS(16), XMAX, XMAXT, XSML, Y,
     1                 DCSEVL, DBESI0, DBSK0E
      LOGICAL FIRST
      SAVE BK0CS, NTK0, XSML, XMAX, FIRST
      DATA BK0CS(  1) / -.3532739323 3902768720 1140060063 153 D-1    /
      DATA BK0CS(  2) / +.3442898999 2462848688 6344927529 213 D+0    /
      DATA BK0CS(  3) / +.3597993651 5361501626 5721303687 231 D-1    /
      DATA BK0CS(  4) / +.1264615411 4469259233 8479508673 447 D-2    /
      DATA BK0CS(  5) / +.2286212103 1194517860 8269830297 585 D-4    /
      DATA BK0CS(  6) / +.2534791079 0261494573 0790013428 354 D-6    /
      DATA BK0CS(  7) / +.1904516377 2202088589 7214059381 366 D-8    /
      DATA BK0CS(  8) / +.1034969525 7633624585 1008317853 089 D-10   /
      DATA BK0CS(  9) / +.4259816142 7910825765 2445327170 133 D-13   /
      DATA BK0CS( 10) / +.1374465435 8807508969 4238325440 000 D-15   /
      DATA BK0CS( 11) / +.3570896528 5083735909 9688597333 333 D-18   /
      DATA BK0CS( 12) / +.7631643660 1164373766 7498666666 666 D-21   /
      DATA BK0CS( 13) / +.1365424988 4407818590 8053333333 333 D-23   /
      DATA BK0CS( 14) / +.2075275266 9066680831 9999999999 999 D-26   /
      DATA BK0CS( 15) / +.2712814218 0729856000 0000000000 000 D-29   /
      DATA BK0CS( 16) / +.3082593887 9146666666 6666666666 666 D-32   /
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  DBESK0
      IF (FIRST) THEN
         NTK0 = INITDS (BK0CS, 16, 0.1*REAL(D1MACH(3)))
         XSML = SQRT(4.0D0*D1MACH(3))
         XMAXT = -LOG(D1MACH(1))
         XMAX = XMAXT - 0.5D0*XMAXT*LOG(XMAXT)/(XMAXT+0.5D0)
      ENDIF
      FIRST = .FALSE.
C
CCCCC IF (X .LE. 0.D0) CALL XERMSG ('SLATEC', 'DBESK0',
CCCCC+   'X IS ZERO OR NEGATIVE', 2, 2)
      IF (X .LE. 0.D0) THEN
        WRITE(ICOUT,1)
    1   FORMAT('***** ERORR FROM DBESK0, X IS ZERO OR NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        DBESK0 = 0.0
        RETURN
      ENDIF
      IF (X.GT.2.0D0) GO TO 20
C
      Y = 0.D0
      IF (X.GT.XSML) Y = X*X
      DBESK0 = -LOG(0.5D0*X)*DBESI0(X) - 0.25D0 + DCSEVL (.5D0*Y-1.D0,
     1  BK0CS, NTK0)
      RETURN
C
 20   DBESK0 = 0.D0
CCCCC IF (X .GT. XMAX) CALL XERMSG ('SLATEC', 'DBESK0',
CCCCC+   'X SO BIG K0 UNDERFLOWS', 1, 1)
      IF (X.GT.XMAX) THEN
        WRITE(ICOUT,2)
        CALL DPWRST('XXX','BUG ')
        DBESK0 = 0.0
        RETURN
      ENDIF
    2 FORMAT('***** ERORR FROM DBESK0, UNDERFLOWS BECAUSE THE ',
     1       'VALUE OF X IS TOO BIG.')
      IF (X.GT.XMAX) RETURN
C
      DBESK0 = EXP(-X) * DBSK0E(X)
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION DBESK1 (X)
C***BEGIN PROLOGUE  DBESK1
C***PURPOSE  Compute the modified (hyperbolic) Bessel function of the
C            third kind of order one.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C10B1
C***TYPE      DOUBLE PRECISION (BESK1-S, DBESK1-D)
C***KEYWORDS  FNLIB, HYPERBOLIC BESSEL FUNCTION,
C             MODIFIED BESSEL FUNCTION, ORDER ONE, SPECIAL FUNCTIONS,
C             THIRD KIND
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C DBESK1(X) calculates the double precision modified (hyperbolic)
C Bessel function of the third kind of order one for double precision
C argument X.  The argument must be large enough that the result does
C not overflow and small enough that the result does not underflow.
C
C Series for BK1        on the interval  0.          to  4.00000E+00
C                                        with weighted error   9.16E-32
C                                         log weighted error  31.04
C                               significant figures required  30.61
C                                    decimal places required  31.64
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, DBESI1, DBSK1E, DCSEVL, INITDS, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C***END PROLOGUE  DBESK1
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
      DOUBLE PRECISION X, BK1CS(16), XMAX, XMAXT, XMIN, XSML, Y,
     1  DCSEVL, DBESI1, DBSK1E
      LOGICAL FIRST
      SAVE BK1CS, NTK1, XMIN, XSML, XMAX, FIRST
      DATA BK1CS(  1) / +.2530022733 8947770532 5311208685 33 D-1     /
      DATA BK1CS(  2) / -.3531559607 7654487566 7238316918 01 D+0     /
      DATA BK1CS(  3) / -.1226111808 2265714823 4790679300 42 D+0     /
      DATA BK1CS(  4) / -.6975723859 6398643501 8129202960 83 D-2     /
      DATA BK1CS(  5) / -.1730288957 5130520630 1765073689 79 D-3     /
      DATA BK1CS(  6) / -.2433406141 5659682349 6007350301 64 D-5     /
      DATA BK1CS(  7) / -.2213387630 7347258558 3152525451 26 D-7     /
      DATA BK1CS(  8) / -.1411488392 6335277610 9583302126 08 D-9     /
      DATA BK1CS(  9) / -.6666901694 1993290060 8537512643 73 D-12    /
      DATA BK1CS( 10) / -.2427449850 5193659339 2631968648 53 D-14    /
      DATA BK1CS( 11) / -.7023863479 3862875971 7837971200 00 D-17    /
      DATA BK1CS( 12) / -.1654327515 5100994675 4910293333 33 D-19    /
      DATA BK1CS( 13) / -.3233834745 9944491991 8933333333 33 D-22    /
      DATA BK1CS( 14) / -.5331275052 9265274999 4666666666 66 D-25    /
      DATA BK1CS( 15) / -.7513040716 2157226666 6666666666 66 D-28    /
      DATA BK1CS( 16) / -.9155085717 6541866666 6666666666 66 D-31    /
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  DBESK1
      IF (FIRST) THEN
         NTK1 = INITDS (BK1CS, 16, 0.1*REAL(D1MACH(3)))
         XMIN = EXP(MAX(LOG(D1MACH(1)), -LOG(D1MACH(2))) + 0.01D0)
         XSML = SQRT(4.0D0*D1MACH(3))
         XMAXT = -LOG(D1MACH(1))
         XMAX = XMAXT - 0.5D0*XMAXT*LOG(XMAXT)/(XMAXT+0.5D0)
      ENDIF
      FIRST = .FALSE.
C
CCCCC IF (X .LE. 0.D0) CALL XERMSG ('SLATEC', 'DBESK1',
CCCCC+   'X IS ZERO OR NEGATIVE', 2, 2)
      IF (X .LE. 0.D0) THEN
        WRITE(ICOUT,1)
    1   FORMAT('***** ERORR FROM DBESK1, X ZERO OR NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        DBESK1=0.0D0
        RETURN
      ENDIF
      IF (X.GT.2.0D0) GO TO 20
C
CCCCC IF (X .LT. XMIN) CALL XERMSG ('SLATEC', 'DBESK1',
CCCCC+   'X SO SMALL K1 OVERFLOWS', 3, 2)
      IF (X .LE. XMIN) THEN
        WRITE(ICOUT,2)
        CALL DPWRST('XXX','BUG ')
      ENDIF
    2 FORMAT('***** WARNING FROM DBESK1, UNDERFLOW BECAUSE THE ',
     1       'VALUE OF X IS SO SMALL.')
      Y = 0.D0
      IF (X.GT.XSML) Y = X*X
      DBESK1 = LOG(0.5D0*X)*DBESI1(X) + (0.75D0 + DCSEVL (.5D0*Y-1.D0,
     1  BK1CS, NTK1))/X
      RETURN
C
 20   DBESK1 = 0.D0
CCCCC IF (X .GT. XMAX) CALL XERMSG ('SLATEC', 'DBESK1',
CCCCC+   'X SO BIG K1 UNDERFLOWS', 1, 1)
      IF (X.GT.XMAX) THEN
        WRITE(ICOUT,3)
        CALL DPWRST('XXX','BUG ')
        DBESK1 = 0.0D0
        RETURN
      ENDIF
    3 FORMAT('***** ERORR FROM DBESK1, UNDERFLOW BECAUSE THE ',
     1       'VALUE OF X IS TOO BIG.')
      IF (X.GT.XMAX) RETURN
C
      DBESK1 = EXP(-X) * DBSK1E(X)
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION DBINOM(N,M)
C***BEGIN PROLOGUE  DBINOM
C***DATE WRITTEN   770601   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  C1
C***KEYWORDS  BINOMIAL COEFFICIENTS,DOUBLE PRECISION,SPECIAL FUNCTION
C***AUTHOR  FULLERTON, W., (LANL)
C***PURPOSE  Computes the d.p. binomial coefficients.
C***DESCRIPTION
C
C DBINOM(N,M) calculates the double precision binomial coefficient
C for integer arguments N and M.  The result is (N!)/((M!)(N-M)!).
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH,D9LGMC,DINT,DLNREL,XERROR
C***END PROLOGUE  DBINOM
      DOUBLE PRECISION CORR, FINTMX, SQ2PIL, XK, XN, XNK, DINT, D9LGMC,
     1  DLNREL
      REAL BILNMX
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
      DATA SQ2PIL / 0.9189385332 0467274178 0329736405 62 D0 /
      DATA BILNMX, FINTMX / 0.0, 0.0D0 /
C***FIRST EXECUTABLE STATEMENT  DBINOM
C
      DBINOM = 0.0D0
C
      IF (BILNMX.NE.0.0) GO TO 10
      BILNMX = DLOG(D1MACH(2)) - 0.0001D0
      FINTMX = 0.9D0/D1MACH(3)
C
 10   CONTINUE
      IF(N.LT.0)THEN
        WRITE(ICOUT,1)
 1      FORMAT('***** ERROR: FIRST ARGUMENT TO BINOM IS NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
      IF(M.LT.0)THEN
        WRITE(ICOUT,2)
 2      FORMAT('***** ERROR: SECOND ARGUMENT TO BINOM IS NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
      K = MIN0 (M, N-M)
      IF (K.GT.20) GO TO 30
CCCCC IF (FLOAT(K)*LOG(AMAX0(N,1)).GT.BILNMX) GO TO 30
      IF (FLOAT(K)*LOG(AMAX0(N,1)).GT.BILNMX) GO TO 30
C
      DBINOM = 1.0D0
      IF (K.EQ.0) GOTO9000
      DO 20 I=1,K
        XN = N - I + 1
        XK = I
        DBINOM = DBINOM * (XN/XK)
 20   CONTINUE
C
      IF (DBINOM.LT.FINTMX) DBINOM = DINT (DBINOM+0.5D0)
      GOTO9000
C
C IF K.LT.9, APPROX IS NOT VALID AND ANSWER IS CLOSE TO THE OVERFLOW LIM
 30   CONTINUE
      IF (K.LT.9) THEN
        WRITE(ICOUT,31)
 31     FORMAT('***** ERROR: BINOM OVERFLOWS BECAUSE ONE (OR BOTH) OF ',
     1         'THE ARGUMENTS IS TOO LARGE.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
      XN = N + 1
      XK = K + 1
      XNK = N - K + 1
C
      CORR = D9LGMC(XN) - D9LGMC(XK) - D9LGMC(XNK)
      DBINOM = XK*DLOG(XNK/XK) - XN*DLNREL(-(XK-1.0D0)/XN)
     1  -0.5D0*DLOG(XN*XNK/XK) + 1.0D0 - SQ2PIL + CORR
C
      IF (DBINOM.GT.DBLE(BILNMX)) THEN
C
        WRITE(ICOUT,41)
 41     FORMAT('***** ERROR: BINOM OVERFLOWS BECAUSE ONE (OR BOTH) OF ',
     1         'THE ARGUMENTS IS TOO LARGE.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
      DBINOM = DEXP (DBINOM)
      IF (DBINOM.LT.FINTMX) DBINOM = DINT (DBINOM+0.5D0)
C
 9000 CONTINUE
      RETURN
      END
      DOUBLE PRECISION FUNCTION DBINLN(N,M)
C***BEGIN PROLOGUE  DBINOM
C***DATE WRITTEN   770601   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***REVISION HISTORY (YYMMDD)
C   000601 Changed DINT to generic AINT        (RFB)
C***CATEGORY NO.  C1
C***KEYWORDS  BINOMIAL COEFFICIENTS,DOUBLE PRECISION,SPECIAL FUNCTION
C***AUTHOR  FULLERTON, W., (LANL)
C***PURPOSE  Computes the d.p. binomial coefficients.
C***DESCRIPTION
C
C DBINOM(N,M) calculates the double precision binomial coefficient
C for integer arguments N and M.  The result is (N!)/((M!)(N-M)!).
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH,D9LGMC,AINT,DLNREL,XERROR
C***END PROLOGUE  DBINOM
C
C   NOTE: THIS IS THE BBINOM ROUTINE MODIFIED TO RETURN THE
C         LOG OF THE BINOMIAL COEFFICIENT.
C
C         THIS IS USED INTERNALLY FOR SOME DISCRETE PROBABILITY
C         DISTRIBUTIONS.
C
      DOUBLE PRECISION CORR, FINTMX, SQ2PIL, XK, XN, XNK, D9LGMC,
     1  DLNREL
      REAL BILNMX
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
      DATA SQ2PIL / 0.9189385332 0467274178 0329736405 62 D0 /
      DATA BILNMX, FINTMX / 0.0, 0.0D0 /
C***FIRST EXECUTABLE STATEMENT  DBINOM
C
      DBINLN = 0.0D0
C
      IF (BILNMX.NE.0.0) GO TO 10
      BILNMX = DLOG(D1MACH(2)) - 0.0001D0
      FINTMX = 0.9D0/D1MACH(3)
C
 10   CONTINUE
      IF(N.LT.0)THEN
        WRITE(ICOUT,1)
 1      FORMAT('***** ERROR: FIRST ARGUMENT TO DBINOM IS NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
      IF(M.LT.0)THEN
        WRITE(ICOUT,2)
 2      FORMAT('***** ERROR: SECOND ARGUMENT TO DBINOM IS NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
      IF (N.LT.M) THEN
        WRITE(ICOUT,3)
 3      FORMAT('***** ERROR: FIRST ARGUMENT TO DBINOM IS LESS THAN ',
     1         'SECOND ARGUMENT.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
C10   IF (N.LT.0 .OR. M.LT.0) CALL XERROR ( 'DBINOM  N OR M LT ZERO', 22
CCCCC1, 1, 2)
CCCCC IF (N.LT.M) CALL XERROR ( 'DBINOM  N LT M', 14, 2, 2)
C
      K = MIN0 (M, N-M)
      IF (K.GT.20) GO TO 30
      IF (FLOAT(K)*LOG(AMAX0(N,1)).GT.BILNMX) GO TO 30
C
      DBINLN = DLOG(1.0D0)
      IF (K.EQ.0) RETURN
      DO 20 I=1,K
        XN = N - I + 1
        XK = I
        DBINLN = DBINLN + DLOG((XN/XK))
 20   CONTINUE
C
CCCCC IF (DBINLN.LT.FINTMX) DBINLN = AINT (DBINLN+0.5D0)
      RETURN
C
C IF K.LT.9, APPROX IS NOT VALID AND ANSWER IS CLOSE TO THE OVERFLOW LIM
 30   CONTINUE
      IF (K.LT.9) THEN
        WRITE(ICOUT,31)
 31     FORMAT('***** ERROR: BINOM OVERFLOWS BECAUSE ONE (OR BOTH) OF ',
     1         'THE ARGUMENTS IS TOO LARGE.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
C30   IF (K.LT.9) CALL XERROR( 'DBINOM  RESULT OVERFLOWS BECAUSE N AND/O
CCCCC1R M TOO BIG', 51, 3, 2)
C
      XN = N + 1
      XK = K + 1
      XNK = N - K + 1
C
      CORR = D9LGMC(XN) - D9LGMC(XK) - D9LGMC(XNK)
      DBINLN = XK*DLOG(XNK/XK) - XN*DLNREL(-(XK-1.0D0)/XN)
     1  -0.5D0*DLOG(XN*XNK/XK) + 1.0D0 - SQ2PIL + CORR
C
CCCCC IF (DBINOM.GT.DBLE(BILNMX)) CALL XERROR ( 'DBINOM  RESULT OVERFLOW
CCCCC1S BECAUSE N AND/OR M TOO BIG', 51, 3,2)
CCCCC IF (DBINOM.GT.BILNMX) THEN
C
CCCCC   WRITE(ICOUT,41)
C41     FORMAT('***** ERROR: DBINOM OVERFLOWS BECAUSE ONE (OR BOTH) ',
CCCCC1         'OF THE ARGUMENTS IS TOO LARGE.')
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   GOTO9000
CCCCC ENDIF
C
CCCCC DBINOM = DEXP (DBINLN)
CCCCC IF (DBINOM.LT.FINTMX) DBINOM = AINT (DBINOM+0.5D0)
C
 9000 CONTINUE
      RETURN
      END
      DOUBLE PRECISION FUNCTION DBSI0E (X)
C***BEGIN PROLOGUE  DBSI0E
C***PURPOSE  Compute the exponentially scaled modified (hyperbolic)
C            Bessel function of the first kind of order zero.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C10B1
C***TYPE      DOUBLE PRECISION (BESI0E-S, DBSI0E-D)
C***KEYWORDS  EXPONENTIALLY SCALED, FIRST KIND, FNLIB,
C             HYPERBOLIC BESSEL FUNCTION, MODIFIED BESSEL FUNCTION,
C             ORDER ZERO, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C DBSI0E(X) calculates the double precision exponentially scaled
C modified (hyperbolic) Bessel function of the first kind of order
C zero for double precision argument X.  The result is the Bessel
C function I0(X) multiplied by EXP(-ABS(X)).
C
C Series for BI0        on the interval  0.          to  9.00000E+00
C                                        with weighted error   9.51E-34
C                                         log weighted error  33.02
C                               significant figures required  33.31
C                                    decimal places required  33.65
C
C Series for AI0        on the interval  1.25000E-01 to  3.33333E-01
C                                        with weighted error   2.74E-32
C                                         log weighted error  31.56
C                               significant figures required  30.15
C                                    decimal places required  32.39
C
C Series for AI02       on the interval  0.          to  1.25000E-01
C                                        with weighted error   1.97E-32
C                                         log weighted error  31.71
C                               significant figures required  30.15
C                                    decimal places required  32.63
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, DCSEVL, INITDS
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C***END PROLOGUE  DBSI0E
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
      DOUBLE PRECISION X, BI0CS(18), AI0CS(46), AI02CS(69),
     1  XSML, Y, DCSEVL
      LOGICAL FIRST
      SAVE BI0CS, AI0CS, AI02CS, NTI0, NTAI0, NTAI02, XSML, FIRST
      DATA BI0CS(  1) / -.7660547252 8391449510 8189497624 3285 D-1   /
      DATA BI0CS(  2) / +.1927337953 9938082699 5240875088 1196 D+1   /
      DATA BI0CS(  3) / +.2282644586 9203013389 3702929233 0415 D+0   /
      DATA BI0CS(  4) / +.1304891466 7072904280 7933421069 1888 D-1   /
      DATA BI0CS(  5) / +.4344270900 8164874513 7868268102 6107 D-3   /
      DATA BI0CS(  6) / +.9422657686 0019346639 2317174411 8766 D-5   /
      DATA BI0CS(  7) / +.1434006289 5106910799 6209187817 9957 D-6   /
      DATA BI0CS(  8) / +.1613849069 6617490699 1541971999 4611 D-8   /
      DATA BI0CS(  9) / +.1396650044 5356696994 9509270814 2522 D-10  /
      DATA BI0CS( 10) / +.9579451725 5054453446 2752317189 3333 D-13  /
      DATA BI0CS( 11) / +.5333981859 8625021310 1510774400 0000 D-15  /
      DATA BI0CS( 12) / +.2458716088 4374707746 9678591999 9999 D-17  /
      DATA BI0CS( 13) / +.9535680890 2487700269 4434133333 3333 D-20  /
      DATA BI0CS( 14) / +.3154382039 7214273367 8933333333 3333 D-22  /
      DATA BI0CS( 15) / +.9004564101 0946374314 6666666666 6666 D-25  /
      DATA BI0CS( 16) / +.2240647369 1236700160 0000000000 0000 D-27  /
      DATA BI0CS( 17) / +.4903034603 2428373333 3333333333 3333 D-30  /
      DATA BI0CS( 18) / +.9508172606 1226666666 6666666666 6666 D-33  /
      DATA AI0CS(  1) / +.7575994494 0237959427 2987203743 8 D-1      /
      DATA AI0CS(  2) / +.7591380810 8233455072 9297873320 4 D-2      /
      DATA AI0CS(  3) / +.4153131338 9237505018 6319749138 2 D-3      /
      DATA AI0CS(  4) / +.1070076463 4390730735 8242970217 0 D-4      /
      DATA AI0CS(  5) / -.7901179979 2128946607 5031948573 0 D-5      /
      DATA AI0CS(  6) / -.7826143501 4387522697 8898980690 9 D-6      /
      DATA AI0CS(  7) / +.2783849942 9488708063 8118538985 7 D-6      /
      DATA AI0CS(  8) / +.8252472600 6120271919 6682913319 8 D-8      /
      DATA AI0CS(  9) / -.1204463945 5201991790 5496089110 3 D-7      /
      DATA AI0CS( 10) / +.1559648598 5060764436 1228752792 8 D-8      /
      DATA AI0CS( 11) / +.2292556367 1033165434 7725480285 7 D-9      /
      DATA AI0CS( 12) / -.1191622884 2790646036 7777423447 8 D-9      /
      DATA AI0CS( 13) / +.1757854916 0324098302 1833124774 3 D-10     /
      DATA AI0CS( 14) / +.1128224463 2189005171 4441135682 4 D-11     /
      DATA AI0CS( 15) / -.1146848625 9272988777 2963387698 2 D-11     /
      DATA AI0CS( 16) / +.2715592054 8036628726 4365192160 6 D-12     /
      DATA AI0CS( 17) / -.2415874666 5626878384 4247572028 1 D-13     /
      DATA AI0CS( 18) / -.6084469888 2551250646 0609963922 4 D-14     /
      DATA AI0CS( 19) / +.3145705077 1754772937 0836026730 3 D-14     /
      DATA AI0CS( 20) / -.7172212924 8711877179 6217505917 6 D-15     /
      DATA AI0CS( 21) / +.7874493403 4541033960 8390960332 7 D-16     /
      DATA AI0CS( 22) / +.1004802753 0094624023 4524457183 9 D-16     /
      DATA AI0CS( 23) / -.7566895365 3505348534 2843588881 0 D-17     /
      DATA AI0CS( 24) / +.2150380106 8761198878 1205128784 5 D-17     /
      DATA AI0CS( 25) / -.3754858341 8308744291 5158445260 8 D-18     /
      DATA AI0CS( 26) / +.2354065842 2269925769 0075710532 2 D-19     /
      DATA AI0CS( 27) / +.1114667612 0479285302 2637335511 0 D-19     /
      DATA AI0CS( 28) / -.5398891884 3969903786 9677932270 9 D-20     /
      DATA AI0CS( 29) / +.1439598792 2407526770 4285840452 2 D-20     /
      DATA AI0CS( 30) / -.2591916360 1110934064 6081840196 2 D-21     /
      DATA AI0CS( 31) / +.2238133183 9985839074 3409229824 0 D-22     /
      DATA AI0CS( 32) / +.5250672575 3647711727 7221683199 9 D-23     /
      DATA AI0CS( 33) / -.3249904138 5332307841 7343228586 6 D-23     /
      DATA AI0CS( 34) / +.9924214103 2050379278 5728471040 0 D-24     /
      DATA AI0CS( 35) / -.2164992254 2446695231 4655429973 3 D-24     /
      DATA AI0CS( 36) / +.3233609471 9435940839 7333299199 9 D-25     /
      DATA AI0CS( 37) / -.1184620207 3967424898 2473386666 6 D-26     /
      DATA AI0CS( 38) / -.1281671853 9504986505 4833868799 9 D-26     /
      DATA AI0CS( 39) / +.5827015182 2793905116 0556885333 3 D-27     /
      DATA AI0CS( 40) / -.1668222326 0261097193 6450150399 9 D-27     /
      DATA AI0CS( 41) / +.3625309510 5415699757 0068480000 0 D-28     /
      DATA AI0CS( 42) / -.5733627999 0557135899 4595839999 9 D-29     /
      DATA AI0CS( 43) / +.3736796722 0630982296 4258133333 3 D-30     /
      DATA AI0CS( 44) / +.1602073983 1568519633 6551253333 3 D-30     /
      DATA AI0CS( 45) / -.8700424864 0572298845 2249599999 9 D-31     /
      DATA AI0CS( 46) / +.2741320937 9374811456 0341333333 3 D-31     /
      DATA AI02CS(  1) / +.5449041101 4108831607 8960962268 0 D-1      /
      DATA AI02CS(  2) / +.3369116478 2556940898 9785662979 9 D-2      /
      DATA AI02CS(  3) / +.6889758346 9168239842 6263914301 1 D-4      /
      DATA AI02CS(  4) / +.2891370520 8347564829 6692402323 2 D-5      /
      DATA AI02CS(  5) / +.2048918589 4690637418 2760534093 1 D-6      /
      DATA AI02CS(  6) / +.2266668990 4981780645 9327743136 1 D-7      /
      DATA AI02CS(  7) / +.3396232025 7083863451 5084396952 3 D-8      /
      DATA AI02CS(  8) / +.4940602388 2249695891 0482449783 5 D-9      /
      DATA AI02CS(  9) / +.1188914710 7846438342 4084525196 3 D-10     /
      DATA AI02CS( 10) / -.3149916527 9632413645 3864862961 9 D-10     /
      DATA AI02CS( 11) / -.1321581184 0447713118 7540739926 7 D-10     /
      DATA AI02CS( 12) / -.1794178531 5068061177 7943574026 9 D-11     /
      DATA AI02CS( 13) / +.7180124451 3836662336 7106429346 9 D-12     /
      DATA AI02CS( 14) / +.3852778382 7421427011 4089801777 6 D-12     /
      DATA AI02CS( 15) / +.1540086217 5214098269 1325823339 7 D-13     /
      DATA AI02CS( 16) / -.4150569347 2872220866 2689972015 6 D-13     /
      DATA AI02CS( 17) / -.9554846698 8283076487 0214494312 5 D-14     /
      DATA AI02CS( 18) / +.3811680669 3526224207 4605535511 8 D-14     /
      DATA AI02CS( 19) / +.1772560133 0565263836 0493266675 8 D-14     /
      DATA AI02CS( 20) / -.3425485619 6772191346 1924790328 2 D-15     /
      DATA AI02CS( 21) / -.2827623980 5165834849 4205593759 4 D-15     /
      DATA AI02CS( 22) / +.3461222867 6974610930 9706250813 4 D-16     /
      DATA AI02CS( 23) / +.4465621420 2967599990 1042054284 3 D-16     /
      DATA AI02CS( 24) / -.4830504485 9441820712 5525403795 4 D-17     /
      DATA AI02CS( 25) / -.7233180487 8747539545 6227240924 5 D-17     /
      DATA AI02CS( 26) / +.9921475412 1736985988 8046093981 0 D-18     /
      DATA AI02CS( 27) / +.1193650890 8459820855 0439949924 2 D-17     /
      DATA AI02CS( 28) / -.2488709837 1508072357 2054491660 2 D-18     /
      DATA AI02CS( 29) / -.1938426454 1609059289 8469781132 6 D-18     /
      DATA AI02CS( 30) / +.6444656697 3734438687 8301949394 9 D-19     /
      DATA AI02CS( 31) / +.2886051596 2892243264 8171383073 4 D-19     /
      DATA AI02CS( 32) / -.1601954907 1749718070 6167156200 7 D-19     /
      DATA AI02CS( 33) / -.3270815010 5923147208 9193567485 9 D-20     /
      DATA AI02CS( 34) / +.3686932283 8264091811 4600723939 3 D-20     /
      DATA AI02CS( 35) / +.1268297648 0309501530 1359529710 9 D-22     /
      DATA AI02CS( 36) / -.7549825019 3772739076 9636664410 1 D-21     /
      DATA AI02CS( 37) / +.1502133571 3778353496 3712789053 4 D-21     /
      DATA AI02CS( 38) / +.1265195883 5096485349 3208799248 3 D-21     /
      DATA AI02CS( 39) / -.6100998370 0836807086 2940891600 2 D-22     /
      DATA AI02CS( 40) / -.1268809629 2601282643 6872095924 2 D-22     /
      DATA AI02CS( 41) / +.1661016099 8907414578 4038487490 5 D-22     /
      DATA AI02CS( 42) / -.1585194335 7658855793 7970504881 4 D-23     /
      DATA AI02CS( 43) / -.3302645405 9682178009 5381766755 6 D-23     /
      DATA AI02CS( 44) / +.1313580902 8392397817 4039623117 4 D-23     /
      DATA AI02CS( 45) / +.3689040246 6711567933 1425637280 4 D-24     /
      DATA AI02CS( 46) / -.4210141910 4616891492 1978247249 9 D-24     /
      DATA AI02CS( 47) / +.4791954591 0828657806 3171401373 0 D-25     /
      DATA AI02CS( 48) / +.8459470390 2218217952 9971707412 4 D-25     /
      DATA AI02CS( 49) / -.4039800940 8728324931 4607937181 0 D-25     /
      DATA AI02CS( 50) / -.6434714653 6504313473 0100850469 5 D-26     /
      DATA AI02CS( 51) / +.1225743398 8756659903 4464736990 5 D-25     /
      DATA AI02CS( 52) / -.2934391316 0257089231 9879821175 4 D-26     /
      DATA AI02CS( 53) / -.1961311309 1949829262 0371205728 9 D-26     /
      DATA AI02CS( 54) / +.1503520374 8221934241 6229900309 8 D-26     /
      DATA AI02CS( 55) / -.9588720515 7448265520 3386388206 9 D-28     /
      DATA AI02CS( 56) / -.3483339380 8170454863 9441108511 4 D-27     /
      DATA AI02CS( 57) / +.1690903610 2630436730 6244960725 6 D-27     /
      DATA AI02CS( 58) / +.1982866538 7356030438 9400115718 8 D-28     /
      DATA AI02CS( 59) / -.5317498081 4918162145 7583002528 4 D-28     /
      DATA AI02CS( 60) / +.1803306629 8883929462 3501450390 1 D-28     /
      DATA AI02CS( 61) / +.6213093341 4548931758 8405311242 2 D-29     /
      DATA AI02CS( 62) / -.7692189292 7721618632 0072806673 0 D-29     /
      DATA AI02CS( 63) / +.1858252826 1117025426 2556016596 3 D-29     /
      DATA AI02CS( 64) / +.1237585142 2813957248 9927154554 1 D-29     /
      DATA AI02CS( 65) / -.1102259120 4092238032 1779478779 2 D-29     /
      DATA AI02CS( 66) / +.1886287118 0397044900 7787447943 1 D-30     /
      DATA AI02CS( 67) / +.2160196872 2436589131 4903141406 0 D-30     /
      DATA AI02CS( 68) / -.1605454124 9197432005 8446594965 5 D-30     /
      DATA AI02CS( 69) / +.1965352984 5942906039 3884807331 8 D-31     /
      DATA FIRST /.TRUE./
C
      DBSI0E = 0.0D0
C
C***FIRST EXECUTABLE STATEMENT  DBSI0E
      IF (FIRST) THEN
         ETA = 0.1*REAL(D1MACH(3))
         NTI0 = INITDS (BI0CS, 18, ETA)
         NTAI0 = INITDS (AI0CS, 46, ETA)
         NTAI02 = INITDS (AI02CS, 69, ETA)
         XSML = SQRT(4.5D0*D1MACH(3))
      ENDIF
      FIRST = .FALSE.
C
      Y = ABS(X)
      IF (Y.GT.3.0D0) GO TO 20
C
      DBSI0E = 1.0D0 - X
      IF (Y.GT.XSML) DBSI0E = EXP(-Y) * (2.75D0 +
     1  DCSEVL (Y*Y/4.5D0-1.D0, BI0CS, NTI0) )
      RETURN
C
 20   IF (Y.LE.8.D0) DBSI0E = (0.375D0 + DCSEVL ((48.D0/Y-11.D0)/5.D0,
     1  AI0CS, NTAI0))/SQRT(Y)
      IF (Y.GT.8.D0) DBSI0E = (0.375D0 + DCSEVL (16.D0/Y-1.D0, AI02CS,
     1  NTAI02))/SQRT(Y)
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION DBSI1E (X)
C***BEGIN PROLOGUE  DBSI1E
C***PURPOSE  Compute the exponentially scaled modified (hyperbolic)
C            Bessel function of the first kind of order one.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C10B1
C***TYPE      DOUBLE PRECISION (BESI1E-S, DBSI1E-D)
C***KEYWORDS  EXPONENTIALLY SCALED, FIRST KIND, FNLIB,
C             HYPERBOLIC BESSEL FUNCTION, MODIFIED BESSEL FUNCTION,
C             ORDER ONE, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C DBSI1E(X) calculates the double precision exponentially scaled
C modified (hyperbolic) Bessel function of the first kind of order
C one for double precision argument X.  The result is I1(X)
C multiplied by EXP(-ABS(X)).
C
C Series for BI1        on the interval  0.          to  9.00000E+00
C                                        with weighted error   1.44E-32
C                                         log weighted error  31.84
C                               significant figures required  31.45
C                                    decimal places required  32.46
C
C Series for AI1        on the interval  1.25000E-01 to  3.33333E-01
C                                        with weighted error   2.81E-32
C                                         log weighted error  31.55
C                               significant figures required  29.93
C                                    decimal places required  32.38
C
C Series for AI12       on the interval  0.          to  1.25000E-01
C                                        with weighted error   1.83E-32
C                                         log weighted error  31.74
C                               significant figures required  29.97
C                                    decimal places required  32.66
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, DCSEVL, INITDS, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C***END PROLOGUE  DBSI1E
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
      DOUBLE PRECISION X, BI1CS(17), AI1CS(46), AI12CS(69), XMIN,
     1  XSML, Y, DCSEVL
      LOGICAL FIRST
      SAVE BI1CS, AI1CS, AI12CS, NTI1, NTAI1, NTAI12, XMIN, XSML,
     1  FIRST
      DATA BI1CS(  1) / -.1971713261 0998597316 1385032181 49 D-2     /
      DATA BI1CS(  2) / +.4073488766 7546480608 1553936520 14 D+0     /
      DATA BI1CS(  3) / +.3483899429 9959455866 2450377837 87 D-1     /
      DATA BI1CS(  4) / +.1545394556 3001236038 5984010584 89 D-2     /
      DATA BI1CS(  5) / +.4188852109 8377784129 4588320041 20 D-4     /
      DATA BI1CS(  6) / +.7649026764 8362114741 9597039660 69 D-6     /
      DATA BI1CS(  7) / +.1004249392 4741178689 1798080372 38 D-7     /
      DATA BI1CS(  8) / +.9932207791 9238106481 3712980548 63 D-10    /
      DATA BI1CS(  9) / +.7663801791 8447637275 2001716813 49 D-12    /
      DATA BI1CS( 10) / +.4741418923 8167394980 3880919481 60 D-14    /
      DATA BI1CS( 11) / +.2404114404 0745181799 8631720320 00 D-16    /
      DATA BI1CS( 12) / +.1017150500 7093713649 1211007999 99 D-18    /
      DATA BI1CS( 13) / +.3645093565 7866949458 4917333333 33 D-21    /
      DATA BI1CS( 14) / +.1120574950 2562039344 8106666666 66 D-23    /
      DATA BI1CS( 15) / +.2987544193 4468088832 0000000000 00 D-26    /
      DATA BI1CS( 16) / +.6973231093 9194709333 3333333333 33 D-29    /
      DATA BI1CS( 17) / +.1436794822 0620800000 0000000000 00 D-31    /
      DATA AI1CS(  1) / -.2846744181 8814786741 0037246830 7 D-1      /
      DATA AI1CS(  2) / -.1922953231 4432206510 4444877497 9 D-1      /
      DATA AI1CS(  3) / -.6115185857 9437889822 5624991778 5 D-3      /
      DATA AI1CS(  4) / -.2069971253 3502277088 8282377797 9 D-4      /
      DATA AI1CS(  5) / +.8585619145 8107255655 3694467313 8 D-5      /
      DATA AI1CS(  6) / +.1049498246 7115908625 1745399786 0 D-5      /
      DATA AI1CS(  7) / -.2918338918 4479022020 9343232669 7 D-6      /
      DATA AI1CS(  8) / -.1559378146 6317390001 6068096907 7 D-7      /
      DATA AI1CS(  9) / +.1318012367 1449447055 2530287390 9 D-7      /
      DATA AI1CS( 10) / -.1448423418 1830783176 3913446781 5 D-8      /
      DATA AI1CS( 11) / -.2908512243 9931420948 2504099301 0 D-9      /
      DATA AI1CS( 12) / +.1266388917 8753823873 1115969040 3 D-9      /
      DATA AI1CS( 13) / -.1664947772 9192206706 2417839858 0 D-10     /
      DATA AI1CS( 14) / -.1666653644 6094329760 9593715499 9 D-11     /
      DATA AI1CS( 15) / +.1242602414 2907682652 3216847201 7 D-11     /
      DATA AI1CS( 16) / -.2731549379 6724323972 5146142863 3 D-12     /
      DATA AI1CS( 17) / +.2023947881 6458037807 0026268898 1 D-13     /
      DATA AI1CS( 18) / +.7307950018 1168836361 9869812612 3 D-14     /
      DATA AI1CS( 19) / -.3332905634 4046749438 1377861713 3 D-14     /
      DATA AI1CS( 20) / +.7175346558 5129537435 4225466567 0 D-15     /
      DATA AI1CS( 21) / -.6982530324 7962563558 5062922365 6 D-16     /
      DATA AI1CS( 22) / -.1299944201 5627607600 6044608058 7 D-16     /
      DATA AI1CS( 23) / +.8120942864 2427988920 5467834286 0 D-17     /
      DATA AI1CS( 24) / -.2194016207 4107368981 5626664378 3 D-17     /
      DATA AI1CS( 25) / +.3630516170 0296548482 7986093233 4 D-18     /
      DATA AI1CS( 26) / -.1695139772 4391041663 0686679039 9 D-19     /
      DATA AI1CS( 27) / -.1288184829 8979078071 1688253822 2 D-19     /
      DATA AI1CS( 28) / +.5694428604 9670527801 0999107310 9 D-20     /
      DATA AI1CS( 29) / -.1459597009 0904800565 4550990028 7 D-20     /
      DATA AI1CS( 30) / +.2514546010 6757173140 8469133448 5 D-21     /
      DATA AI1CS( 31) / -.1844758883 1391248181 6040002901 3 D-22     /
      DATA AI1CS( 32) / -.6339760596 2279486419 2860979199 9 D-23     /
      DATA AI1CS( 33) / +.3461441102 0310111111 0814662656 0 D-23     /
      DATA AI1CS( 34) / -.1017062335 3713935475 9654102357 3 D-23     /
      DATA AI1CS( 35) / +.2149877147 0904314459 6250077866 6 D-24     /
      DATA AI1CS( 36) / -.3045252425 2386764017 4620617386 6 D-25     /
      DATA AI1CS( 37) / +.5238082144 7212859821 7763498666 6 D-27     /
      DATA AI1CS( 38) / +.1443583107 0893824464 1678950399 9 D-26     /
      DATA AI1CS( 39) / -.6121302074 8900427332 0067071999 9 D-27     /
      DATA AI1CS( 40) / +.1700011117 4678184183 4918980266 6 D-27     /
      DATA AI1CS( 41) / -.3596589107 9842441585 3521578666 6 D-28     /
      DATA AI1CS( 42) / +.5448178578 9484185766 5051306666 6 D-29     /
      DATA AI1CS( 43) / -.2731831789 6890849891 6256426666 6 D-30     /
      DATA AI1CS( 44) / -.1858905021 7086007157 7190399999 9 D-30     /
      DATA AI1CS( 45) / +.9212682974 5139334411 2776533333 3 D-31     /
      DATA AI1CS( 46) / -.2813835155 6535611063 7083306666 6 D-31     /
      DATA AI12CS(  1) / +.2857623501 8280120474 4984594846 9 D-1      /
      DATA AI12CS(  2) / -.9761097491 3614684077 6516445730 2 D-2      /
      DATA AI12CS(  3) / -.1105889387 6262371629 1256921277 5 D-3      /
      DATA AI12CS(  4) / -.3882564808 8776903934 5654477627 4 D-5      /
      DATA AI12CS(  5) / -.2512236237 8702089252 9452002212 1 D-6      /
      DATA AI12CS(  6) / -.2631468846 8895195068 3705236523 2 D-7      /
      DATA AI12CS(  7) / -.3835380385 9642370220 4500678796 8 D-8      /
      DATA AI12CS(  8) / -.5589743462 1965838068 6811252222 9 D-9      /
      DATA AI12CS(  9) / -.1897495812 3505412344 9892503323 8 D-10     /
      DATA AI12CS( 10) / +.3252603583 0154882385 5508067994 9 D-10     /
      DATA AI12CS( 11) / +.1412580743 6613781331 6336633284 6 D-10     /
      DATA AI12CS( 12) / +.2035628544 1470895072 2452613684 0 D-11     /
      DATA AI12CS( 13) / -.7198551776 2459085120 9258989044 6 D-12     /
      DATA AI12CS( 14) / -.4083551111 0921973182 2849963969 1 D-12     /
      DATA AI12CS( 15) / -.2101541842 7726643130 1984572746 2 D-13     /
      DATA AI12CS( 16) / +.4272440016 7119513542 9778833699 7 D-13     /
      DATA AI12CS( 17) / +.1042027698 4128802764 1741449994 8 D-13     /
      DATA AI12CS( 18) / -.3814403072 4370078047 6707253539 6 D-14     /
      DATA AI12CS( 19) / -.1880354775 5107824485 1273453396 3 D-14     /
      DATA AI12CS( 20) / +.3308202310 9209282827 3190335240 5 D-15     /
      DATA AI12CS( 21) / +.2962628997 6459501390 6854654205 2 D-15     /
      DATA AI12CS( 22) / -.3209525921 9934239587 7837353288 7 D-16     /
      DATA AI12CS( 23) / -.4650305368 4893583255 7128281897 9 D-16     /
      DATA AI12CS( 24) / +.4414348323 0717079499 4611375964 1 D-17     /
      DATA AI12CS( 25) / +.7517296310 8421048054 2545808029 5 D-17     /
      DATA AI12CS( 26) / -.9314178867 3268833756 8484784515 7 D-18     /
      DATA AI12CS( 27) / -.1242193275 1948909561 1678448869 7 D-17     /
      DATA AI12CS( 28) / +.2414276719 4548484690 0515390217 6 D-18     /
      DATA AI12CS( 29) / +.2026944384 0532851789 7192286069 2 D-18     /
      DATA AI12CS( 30) / -.6394267188 2690977870 4391988681 1 D-19     /
      DATA AI12CS( 31) / -.3049812452 3730958960 8488450357 1 D-19     /
      DATA AI12CS( 32) / +.1612841851 6514802251 3462230769 1 D-19     /
      DATA AI12CS( 33) / +.3560913964 3099250545 1027090462 0 D-20     /
      DATA AI12CS( 34) / -.3752017947 9364390796 6682800324 6 D-20     /
      DATA AI12CS( 35) / -.5787037427 0747993459 5198231074 1 D-22     /
      DATA AI12CS( 36) / +.7759997511 6481619619 8236963209 2 D-21     /
      DATA AI12CS( 37) / -.1452790897 2022333940 6445987408 5 D-21     /
      DATA AI12CS( 38) / -.1318225286 7390367021 2192275337 4 D-21     /
      DATA AI12CS( 39) / +.6116654862 9030707018 7999133171 7 D-22     /
      DATA AI12CS( 40) / +.1376279762 4271264277 3024338363 4 D-22     /
      DATA AI12CS( 41) / -.1690837689 9593478849 1983938230 6 D-22     /
      DATA AI12CS( 42) / +.1430596088 5954331539 8720108538 5 D-23     /
      DATA AI12CS( 43) / +.3409557828 0905940204 0536772990 2 D-23     /
      DATA AI12CS( 44) / -.1309457666 2707602278 4573872642 4 D-23     /
      DATA AI12CS( 45) / -.3940706411 2402574360 9352141755 7 D-24     /
      DATA AI12CS( 46) / +.4277137426 9808765808 0616679735 2 D-24     /
      DATA AI12CS( 47) / -.4424634830 9826068819 0028312302 9 D-25     /
      DATA AI12CS( 48) / -.8734113196 2307149721 1530978874 7 D-25     /
      DATA AI12CS( 49) / +.4045401335 6835333921 4340414242 8 D-25     /
      DATA AI12CS( 50) / +.7067100658 0946894656 5160771780 6 D-26     /
      DATA AI12CS( 51) / -.1249463344 5651052230 0286451860 5 D-25     /
      DATA AI12CS( 52) / +.2867392244 4034370329 7948339142 6 D-26     /
      DATA AI12CS( 53) / +.2044292892 5042926702 8177957421 0 D-26     /
      DATA AI12CS( 54) / -.1518636633 8204625683 7134680291 1 D-26     /
      DATA AI12CS( 55) / +.8110181098 1875758861 3227910703 7 D-28     /
      DATA AI12CS( 56) / +.3580379354 7735860911 2717370327 0 D-27     /
      DATA AI12CS( 57) / -.1692929018 9279025095 9305717544 8 D-27     /
      DATA AI12CS( 58) / -.2222902499 7024276390 6775852777 4 D-28     /
      DATA AI12CS( 59) / +.5424535127 1459696550 4860040112 8 D-28     /
      DATA AI12CS( 60) / -.1787068401 5780186887 6491299330 4 D-28     /
      DATA AI12CS( 61) / -.6565479068 7228149388 2392943788 0 D-29     /
      DATA AI12CS( 62) / +.7807013165 0611452809 2206770683 9 D-29     /
      DATA AI12CS( 63) / -.1816595260 6689797173 7933315222 1 D-29     /
      DATA AI12CS( 64) / -.1287704952 6600848203 7687559895 9 D-29     /
      DATA AI12CS( 65) / +.1114548172 9881645474 1370927369 4 D-29     /
      DATA AI12CS( 66) / -.1808343145 0393369391 5936887668 7 D-30     /
      DATA AI12CS( 67) / -.2231677718 2037719522 3244822893 9 D-30     /
      DATA AI12CS( 68) / +.1619029596 0803415106 1790980361 4 D-30     /
      DATA AI12CS( 69) / -.1834079908 8049414139 0130843921 0 D-31     /
      DATA FIRST /.TRUE./
C
      DBSI1E = 0.0D0
C
C***FIRST EXECUTABLE STATEMENT  DBSI1E
      IF (FIRST) THEN
         ETA = 0.1*REAL(D1MACH(3))
         NTI1 = INITDS (BI1CS, 17, ETA)
         NTAI1 = INITDS (AI1CS, 46, ETA)
         NTAI12 = INITDS (AI12CS, 69, ETA)
C
         XMIN = 2.0D0*D1MACH(1)
         XSML = SQRT(4.5D0*D1MACH(3))
      ENDIF
      FIRST = .FALSE.
C
      Y = ABS(X)
      IF (Y.GT.3.0D0) GO TO 20
C
      DBSI1E = 0.0D0
      IF (Y.EQ.0.D0)  RETURN
C
      IF (Y .LE. XMIN) THEN
        WRITE(ICOUT,1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
    1 FORMAT('***** WARNING FROM DBSI1E, UNDERFLOW BECAUSE THE ',
     1       'ABSOLUTE VALUE OF X IS SO SMALL.  ****')
      IF (Y.GT.XMIN) DBSI1E = 0.5D0*X
      IF (Y.GT.XSML) DBSI1E = X*(0.875D0 + DCSEVL (Y*Y/4.5D0-1.D0,
     1  BI1CS, NTI1) )
      DBSI1E = EXP(-Y) * DBSI1E
      RETURN
C
 20   IF (Y.LE.8.D0) DBSI1E = (0.375D0 + DCSEVL ((48.D0/Y-11.D0)/5.D0,
     1  AI1CS, NTAI1))/SQRT(Y)
      IF (Y.GT.8.D0) DBSI1E = (0.375D0 + DCSEVL (16.D0/Y-1.D0, AI12CS,
     1  NTAI12))/SQRT(Y)
      DBSI1E = SIGN (DBSI1E, X)
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION DBSK0E (X)
C***BEGIN PROLOGUE  DBSK0E
C***PURPOSE  Compute the exponentially scaled modified (hyperbolic)
C            Bessel function of the third kind of order zero.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C10B1
C***TYPE      DOUBLE PRECISION (BESK0E-S, DBSK0E-D)
C***KEYWORDS  EXPONENTIALLY SCALED, FNLIB, HYPERBOLIC BESSEL FUNCTION,
C             MODIFIED BESSEL FUNCTION, ORDER ZERO, SPECIAL FUNCTIONS,
C             THIRD KIND
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C DBSK0E(X) computes the double precision exponentially scaled
C modified (hyperbolic) Bessel function of the third kind of
C order zero for positive double precision argument X.
C
C Series for BK0        on the interval  0.          to  4.00000E+00
C                                        with weighted error   3.08E-33
C                                         log weighted error  32.51
C                               significant figures required  32.05
C                                    decimal places required  33.11
C
C Series for AK0        on the interval  1.25000E-01 to  5.00000E-01
C                                        with weighted error   2.85E-32
C                                         log weighted error  31.54
C                               significant figures required  30.19
C                                    decimal places required  32.33
C
C Series for AK02       on the interval  0.          to  1.25000E-01
C                                        with weighted error   2.30E-32
C                                         log weighted error  31.64
C                               significant figures required  29.68
C                                    decimal places required  32.40
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, DBESI0, DCSEVL, INITDS, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C***END PROLOGUE  DBSK0E
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
      DOUBLE PRECISION X, BK0CS(16), AK0CS(38), AK02CS(33),
     1  XSML, Y, DCSEVL, DBESI0
      LOGICAL FIRST
      SAVE BK0CS, AK0CS, AK02CS, NTK0, NTAK0, NTAK02, XSML, FIRST
      DATA BK0CS(  1) / -.3532739323 3902768720 1140060063 153 D-1    /
      DATA BK0CS(  2) / +.3442898999 2462848688 6344927529 213 D+0    /
      DATA BK0CS(  3) / +.3597993651 5361501626 5721303687 231 D-1    /
      DATA BK0CS(  4) / +.1264615411 4469259233 8479508673 447 D-2    /
      DATA BK0CS(  5) / +.2286212103 1194517860 8269830297 585 D-4    /
      DATA BK0CS(  6) / +.2534791079 0261494573 0790013428 354 D-6    /
      DATA BK0CS(  7) / +.1904516377 2202088589 7214059381 366 D-8    /
      DATA BK0CS(  8) / +.1034969525 7633624585 1008317853 089 D-10   /
      DATA BK0CS(  9) / +.4259816142 7910825765 2445327170 133 D-13   /
      DATA BK0CS( 10) / +.1374465435 8807508969 4238325440 000 D-15   /
      DATA BK0CS( 11) / +.3570896528 5083735909 9688597333 333 D-18   /
      DATA BK0CS( 12) / +.7631643660 1164373766 7498666666 666 D-21   /
      DATA BK0CS( 13) / +.1365424988 4407818590 8053333333 333 D-23   /
      DATA BK0CS( 14) / +.2075275266 9066680831 9999999999 999 D-26   /
      DATA BK0CS( 15) / +.2712814218 0729856000 0000000000 000 D-29   /
      DATA BK0CS( 16) / +.3082593887 9146666666 6666666666 666 D-32   /
      DATA AK0CS(  1) / -.7643947903 3279414240 8297827008 8 D-1      /
      DATA AK0CS(  2) / -.2235652605 6998190520 2309555079 1 D-1      /
      DATA AK0CS(  3) / +.7734181154 6938582353 0061817404 7 D-3      /
      DATA AK0CS(  4) / -.4281006688 8860994644 5214643541 6 D-4      /
      DATA AK0CS(  5) / +.3081700173 8629747436 5001482666 0 D-5      /
      DATA AK0CS(  6) / -.2639367222 0096649740 6744889272 3 D-6      /
      DATA AK0CS(  7) / +.2563713036 4034692062 9408826574 2 D-7      /
      DATA AK0CS(  8) / -.2742705549 9002012638 5721191524 4 D-8      /
      DATA AK0CS(  9) / +.3169429658 0974995920 8083287340 3 D-9      /
      DATA AK0CS( 10) / -.3902353286 9621841416 0106571796 2 D-10     /
      DATA AK0CS( 11) / +.5068040698 1885754020 5009212728 6 D-11     /
      DATA AK0CS( 12) / -.6889574741 0078706795 4171355798 4 D-12     /
      DATA AK0CS( 13) / +.9744978497 8259176913 8820133683 1 D-13     /
      DATA AK0CS( 14) / -.1427332841 8845485053 8985534012 2 D-13     /
      DATA AK0CS( 15) / +.2156412571 0214630395 5806297652 7 D-14     /
      DATA AK0CS( 16) / -.3349654255 1495627721 8878205853 0 D-15     /
      DATA AK0CS( 17) / +.5335260216 9529116921 4528039260 1 D-16     /
      DATA AK0CS( 18) / -.8693669980 8907538076 3962237883 7 D-17     /
      DATA AK0CS( 19) / +.1446404347 8622122278 8776344234 6 D-17     /
      DATA AK0CS( 20) / -.2452889825 5001296824 0467875157 3 D-18     /
      DATA AK0CS( 21) / +.4233754526 2321715728 2170634240 0 D-19     /
      DATA AK0CS( 22) / -.7427946526 4544641956 9534129493 3 D-20     /
      DATA AK0CS( 23) / +.1323150529 3926668662 7796746240 0 D-20     /
      DATA AK0CS( 24) / -.2390587164 7396494513 3598146559 9 D-21     /
      DATA AK0CS( 25) / +.4376827585 9232261401 6571255466 6 D-22     /
      DATA AK0CS( 26) / -.8113700607 3451180593 3901141333 3 D-23     /
      DATA AK0CS( 27) / +.1521819913 8321729583 1037815466 6 D-23     /
      DATA AK0CS( 28) / -.2886041941 4833977702 3595861333 3 D-24     /
      DATA AK0CS( 29) / +.5530620667 0547179799 9261013333 3 D-25     /
      DATA AK0CS( 30) / -.1070377329 2498987285 9163306666 6 D-25     /
      DATA AK0CS( 31) / +.2091086893 1423843002 9632853333 3 D-26     /
      DATA AK0CS( 32) / -.4121713723 6462038274 1026133333 3 D-27     /
      DATA AK0CS( 33) / +.8193483971 1213076401 3568000000 0 D-28     /
      DATA AK0CS( 34) / -.1642000275 4592977267 8075733333 3 D-28     /
      DATA AK0CS( 35) / +.3316143281 4802271958 9034666666 6 D-29     /
      DATA AK0CS( 36) / -.6746863644 1452959410 8586666666 6 D-30     /
      DATA AK0CS( 37) / +.1382429146 3184246776 3541333333 3 D-30     /
      DATA AK0CS( 38) / -.2851874167 3598325708 1173333333 3 D-31     /
      DATA AK02CS(  1) / -.1201869826 3075922398 3934621245 2 D-1      /
      DATA AK02CS(  2) / -.9174852691 0256953106 5256107571 3 D-2      /
      DATA AK02CS(  3) / +.1444550931 7750058210 4884387805 7 D-3      /
      DATA AK02CS(  4) / -.4013614175 4357097286 7102107787 9 D-5      /
      DATA AK02CS(  5) / +.1567831810 8523106725 9034899033 3 D-6      /
      DATA AK02CS(  6) / -.7770110438 5217377103 1579975446 0 D-8      /
      DATA AK02CS(  7) / +.4611182576 1797178825 3313052958 6 D-9      /
      DATA AK02CS(  8) / -.3158592997 8605657705 2666580330 9 D-10     /
      DATA AK02CS(  9) / +.2435018039 3650411278 3588781432 9 D-11     /
      DATA AK02CS( 10) / -.2074331387 3983478977 0985337350 6 D-12     /
      DATA AK02CS( 11) / +.1925787280 5899170847 4273650469 3 D-13     /
      DATA AK02CS( 12) / -.1927554805 8389561036 0034718221 8 D-14     /
      DATA AK02CS( 13) / +.2062198029 1978182782 8523786964 4 D-15     /
      DATA AK02CS( 14) / -.2341685117 5792424026 0364019507 1 D-16     /
      DATA AK02CS( 15) / +.2805902810 6430422468 1517882845 8 D-17     /
      DATA AK02CS( 16) / -.3530507631 1618079458 1548246357 3 D-18     /
      DATA AK02CS( 17) / +.4645295422 9351082674 2421633706 6 D-19     /
      DATA AK02CS( 18) / -.6368625941 3442664739 2205346133 3 D-20     /
      DATA AK02CS( 19) / +.9069521310 9865155676 2234880000 0 D-21     /
      DATA AK02CS( 20) / -.1337974785 4236907398 4500531199 9 D-21     /
      DATA AK02CS( 21) / +.2039836021 8599523155 2208896000 0 D-22     /
      DATA AK02CS( 22) / -.3207027481 3678405000 6086997333 3 D-23     /
      DATA AK02CS( 23) / +.5189744413 6623099636 2635946666 6 D-24     /
      DATA AK02CS( 24) / -.8629501497 5405721929 6460799999 9 D-25     /
      DATA AK02CS( 25) / +.1472161183 1025598552 0803840000 0 D-25     /
      DATA AK02CS( 26) / -.2573069023 8670112838 1235199999 9 D-26     /
      DATA AK02CS( 27) / +.4601774086 6435165873 7664000000 0 D-27     /
      DATA AK02CS( 28) / -.8411555324 2010937371 3066666666 6 D-28     /
      DATA AK02CS( 29) / +.1569806306 6353689393 0154666666 6 D-28     /
      DATA AK02CS( 30) / -.2988226453 0057577889 7919999999 9 D-29     /
      DATA AK02CS( 31) / +.5796831375 2168365206 1866666666 6 D-30     /
      DATA AK02CS( 32) / -.1145035994 3476813321 5573333333 3 D-30     /
      DATA AK02CS( 33) / +.2301266594 2496828020 0533333333 3 D-31     /
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  DBSK0E
C
      DBSK0E=0.0D0
C
      IF (FIRST) THEN
         ETA = 0.1*REAL(D1MACH(3))
         NTK0 = INITDS (BK0CS, 16, ETA)
         NTAK0 = INITDS (AK0CS, 38, ETA)
         NTAK02 = INITDS (AK02CS, 33, ETA)
         XSML = SQRT(4.0D0*D1MACH(3))
      ENDIF
      FIRST = .FALSE.
C
CCCCC IF (X .LE. 0.D0) CALL XERMSG ('SLATEC', 'DBSK0E',
CCCCC+   'X IS ZERO OR NEGATIVE', 2, 2)
      IF (X .LE. 0.D0) THEN
        WRITE(ICOUT,1)
    1   FORMAT('***** ERORR FROM DBSK0E, X ZERO OR NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        DBSK0E=0.0D0
        RETURN
      ENDIF
      IF (X.GT.2.0D0) GO TO 20
C
      Y = 0.D0
      IF (X.GT.XSML) Y = X*X
      DBSK0E = EXP(X)*(-LOG(0.5D0*X)*DBESI0(X) - 0.25D0 +
     1  DCSEVL (.5D0*Y-1.D0, BK0CS, NTK0))
      RETURN
C
 20   IF (X.LE.8.D0) DBSK0E = (1.25D0 + DCSEVL ((16.D0/X-5.D0)/3.D0,
     1  AK0CS, NTAK0))/SQRT(X)
      IF (X.GT.8.D0) DBSK0E = (1.25D0 +
     1  DCSEVL (16.D0/X-1.D0, AK02CS, NTAK02))/SQRT(X)
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION DBSK1E (X)
C***BEGIN PROLOGUE  DBSK1E
C***PURPOSE  Compute the exponentially scaled modified (hyperbolic)
C            Bessel function of the third kind of order one.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C10B1
C***TYPE      DOUBLE PRECISION (BESK1E-S, DBSK1E-D)
C***KEYWORDS  EXPONENTIALLY SCALED, FNLIB, HYPERBOLIC BESSEL FUNCTION,
C             MODIFIED BESSEL FUNCTION, ORDER ONE, SPECIAL FUNCTIONS,
C             THIRD KIND
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C DBSK1E(S) computes the double precision exponentially scaled
C modified (hyperbolic) Bessel function of the third kind of order
C one for positive double precision argument X.
C
C Series for BK1        on the interval  0.          to  4.00000E+00
C                                        with weighted error   9.16E-32
C                                         log weighted error  31.04
C                               significant figures required  30.61
C                                    decimal places required  31.64
C
C Series for AK1        on the interval  1.25000E-01 to  5.00000E-01
C                                        with weighted error   3.07E-32
C                                         log weighted error  31.51
C                               significant figures required  30.71
C                                    decimal places required  32.30
C
C Series for AK12       on the interval  0.          to  1.25000E-01
C                                        with weighted error   2.41E-32
C                                         log weighted error  31.62
C                               significant figures required  30.25
C                                    decimal places required  32.38
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, DBESI1, DCSEVL, INITDS, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C***END PROLOGUE  DBSK1E
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
      DOUBLE PRECISION X, BK1CS(16), AK1CS(38), AK12CS(33), XMIN,
     1  XSML, Y, DCSEVL, DBESI1
      LOGICAL FIRST
      SAVE BK1CS, AK1CS, AK12CS, NTK1, NTAK1, NTAK12, XMIN, XSML,
     1  FIRST
      DATA BK1CS(  1) / +.2530022733 8947770532 5311208685 33 D-1     /
      DATA BK1CS(  2) / -.3531559607 7654487566 7238316918 01 D+0     /
      DATA BK1CS(  3) / -.1226111808 2265714823 4790679300 42 D+0     /
      DATA BK1CS(  4) / -.6975723859 6398643501 8129202960 83 D-2     /
      DATA BK1CS(  5) / -.1730288957 5130520630 1765073689 79 D-3     /
      DATA BK1CS(  6) / -.2433406141 5659682349 6007350301 64 D-5     /
      DATA BK1CS(  7) / -.2213387630 7347258558 3152525451 26 D-7     /
      DATA BK1CS(  8) / -.1411488392 6335277610 9583302126 08 D-9     /
      DATA BK1CS(  9) / -.6666901694 1993290060 8537512643 73 D-12    /
      DATA BK1CS( 10) / -.2427449850 5193659339 2631968648 53 D-14    /
      DATA BK1CS( 11) / -.7023863479 3862875971 7837971200 00 D-17    /
      DATA BK1CS( 12) / -.1654327515 5100994675 4910293333 33 D-19    /
      DATA BK1CS( 13) / -.3233834745 9944491991 8933333333 33 D-22    /
      DATA BK1CS( 14) / -.5331275052 9265274999 4666666666 66 D-25    /
      DATA BK1CS( 15) / -.7513040716 2157226666 6666666666 66 D-28    /
      DATA BK1CS( 16) / -.9155085717 6541866666 6666666666 66 D-31    /
      DATA AK1CS(  1) / +.2744313406 9738829695 2576662272 66 D+0     /
      DATA AK1CS(  2) / +.7571989953 1993678170 8923781492 90 D-1     /
      DATA AK1CS(  3) / -.1441051556 4754061229 8531161756 25 D-2     /
      DATA AK1CS(  4) / +.6650116955 1257479394 2513854770 36 D-4     /
      DATA AK1CS(  5) / -.4369984709 5201407660 5808450891 67 D-5     /
      DATA AK1CS(  6) / +.3540277499 7630526799 4171390085 34 D-6     /
      DATA AK1CS(  7) / -.3311163779 2932920208 9826882457 04 D-7     /
      DATA AK1CS(  8) / +.3445977581 9010534532 3114997709 92 D-8     /
      DATA AK1CS(  9) / -.3898932347 4754271048 9819374927 58 D-9     /
      DATA AK1CS( 10) / +.4720819750 4658356400 9474493390 05 D-10    /
      DATA AK1CS( 11) / -.6047835662 8753562345 3735915628 90 D-11    /
      DATA AK1CS( 12) / +.8128494874 8658747888 1938379856 63 D-12    /
      DATA AK1CS( 13) / -.1138694574 7147891428 9239159510 42 D-12    /
      DATA AK1CS( 14) / +.1654035840 8462282325 9729482050 90 D-13    /
      DATA AK1CS( 15) / -.2480902567 7068848221 5160104405 33 D-14    /
      DATA AK1CS( 16) / +.3829237890 7024096948 4292272991 57 D-15    /
      DATA AK1CS( 17) / -.6064734104 0012418187 7682103773 86 D-16    /
      DATA AK1CS( 18) / +.9832425623 2648616038 1940046506 66 D-17    /
      DATA AK1CS( 19) / -.1628416873 8284380035 6666201156 26 D-17    /
      DATA AK1CS( 20) / +.2750153649 6752623718 2841203370 66 D-18    /
      DATA AK1CS( 21) / -.4728966646 3953250924 2810695680 00 D-19    /
      DATA AK1CS( 22) / +.8268150002 8109932722 3920503466 66 D-20    /
      DATA AK1CS( 23) / -.1468140513 6624956337 1939648853 33 D-20    /
      DATA AK1CS( 24) / +.2644763926 9208245978 0858948266 66 D-21    /
      DATA AK1CS( 25) / -.4829015756 4856387897 9698688000 00 D-22    /
      DATA AK1CS( 26) / +.8929302074 3610130180 6563327999 99 D-23    /
      DATA AK1CS( 27) / -.1670839716 8972517176 9977514666 66 D-23    /
      DATA AK1CS( 28) / +.3161645603 4040694931 3686186666 66 D-24    /
      DATA AK1CS( 29) / -.6046205531 2274989106 5064106666 66 D-25    /
      DATA AK1CS( 30) / +.1167879894 2042732700 7184213333 33 D-25    /
      DATA AK1CS( 31) / -.2277374158 2653996232 8678400000 00 D-26    /
      DATA AK1CS( 32) / +.4481109730 0773675795 3058133333 33 D-27    /
      DATA AK1CS( 33) / -.8893288476 9020194062 3360000000 00 D-28    /
      DATA AK1CS( 34) / +.1779468001 8850275131 3920000000 00 D-28    /
      DATA AK1CS( 35) / -.3588455596 7329095821 9946666666 66 D-29    /
      DATA AK1CS( 36) / +.7290629049 2694257991 6799999999 99 D-30    /
      DATA AK1CS( 37) / -.1491844984 5546227073 0240000000 00 D-30    /
      DATA AK1CS( 38) / +.3073657387 2934276300 7999999999 99 D-31    /
      DATA AK12CS(  1) / +.6379308343 7390010366 0048853410 2 D-1      /
      DATA AK12CS(  2) / +.2832887813 0497209358 3503028470 8 D-1      /
      DATA AK12CS(  3) / -.2475370673 9052503454 1454556673 2 D-3      /
      DATA AK12CS(  4) / +.5771972451 6072488204 7097662576 3 D-5      /
      DATA AK12CS(  5) / -.2068939219 5365483027 4553319655 2 D-6      /
      DATA AK12CS(  6) / +.9739983441 3818041803 0921309788 7 D-8      /
      DATA AK12CS(  7) / -.5585336140 3806249846 8889551112 9 D-9      /
      DATA AK12CS(  8) / +.3732996634 0461852402 2121285473 1 D-10     /
      DATA AK12CS(  9) / -.2825051961 0232254451 3506575492 8 D-11     /
      DATA AK12CS( 10) / +.2372019002 4841441736 4349695548 6 D-12     /
      DATA AK12CS( 11) / -.2176677387 9917539792 6830166793 8 D-13     /
      DATA AK12CS( 12) / +.2157914161 6160324539 3956268970 6 D-14     /
      DATA AK12CS( 13) / -.2290196930 7182692759 9155133815 4 D-15     /
      DATA AK12CS( 14) / +.2582885729 8232749619 1993956522 6 D-16     /
      DATA AK12CS( 15) / -.3076752641 2684631876 2109817344 0 D-17     /
      DATA AK12CS( 16) / +.3851487721 2804915970 9489684479 9 D-18     /
      DATA AK12CS( 17) / -.5044794897 6415289771 1728250880 0 D-19     /
      DATA AK12CS( 18) / +.6888673850 4185442370 1829222399 9 D-20     /
      DATA AK12CS( 19) / -.9775041541 9501183030 0213248000 0 D-21     /
      DATA AK12CS( 20) / +.1437416218 5238364610 0165973333 3 D-21     /
      DATA AK12CS( 21) / -.2185059497 3443473734 9973333333 3 D-22     /
      DATA AK12CS( 22) / +.3426245621 8092206316 4538880000 0 D-23     /
      DATA AK12CS( 23) / -.5531064394 2464082325 0124800000 0 D-24     /
      DATA AK12CS( 24) / +.9176601505 6859954037 8282666666 6 D-25     /
      DATA AK12CS( 25) / -.1562287203 6180249114 4874666666 6 D-25     /
      DATA AK12CS( 26) / +.2725419375 4843331323 4943999999 9 D-26     /
      DATA AK12CS( 27) / -.4865674910 0748279923 7802666666 6 D-27     /
      DATA AK12CS( 28) / +.8879388552 7235025873 5786666666 6 D-28     /
      DATA AK12CS( 29) / -.1654585918 0392575489 3653333333 3 D-28     /
      DATA AK12CS( 30) / +.3145111321 3578486743 0399999999 9 D-29     /
      DATA AK12CS( 31) / -.6092998312 1931276124 1600000000 0 D-30     /
      DATA AK12CS( 32) / +.1202021939 3698158346 2399999999 9 D-30     /
      DATA AK12CS( 33) / -.2412930801 4594088413 8666666666 6 D-31     /
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  DBSK1E
C
      DBSK1E=0.0D0
C
      IF (FIRST) THEN
         ETA = 0.1*REAL(D1MACH(3))
         NTK1 = INITDS (BK1CS, 16, ETA)
         NTAK1 = INITDS (AK1CS, 38, ETA)
         NTAK12 = INITDS (AK12CS, 33, ETA)
C
         XMIN = EXP (MAX(LOG(D1MACH(1)), -LOG(D1MACH(2))) + 0.01D0)
         XSML = SQRT(4.0D0*D1MACH(3))
      ENDIF
      FIRST = .FALSE.
C
CCCCC IF (X .LE. 0.D0) CALL XERMSG ('SLATEC', 'DBSK1E',
CCCCC+   'X IS ZERO OR NEGATIVE', 2, 2)
      IF (X .LE. 0.D0) THEN
        WRITE(ICOUT,1)
    1   FORMAT('***** ERORR FROM DBSK1E, X ZERO OR NEGATIVE.')
        CALL DPWRST('XXX','BUG ')
        DBSK1E=0.0D0
        RETURN
      ENDIF
      IF (X.GT.2.0D0) GO TO 20
C
CCCCC IF (X .LT. XMIN) CALL XERMSG ('SLATEC', 'DBSK1E',
CCCCC+   'X SO SMALL K1 OVERFLOWS', 3, 2)
      IF (X .LT. XMIN) THEN
        WRITE(ICOUT,2)
        CALL DPWRST('XXX','BUG ')
        DBSK1E = 0.0D0
        RETURN
      ENDIF
    2 FORMAT('***** ERROR FROM DBSK1E, OVERRFLOW BECAUSE THE ',
     1       'VALUE OF X IS SO SMALL.')
      Y = 0.D0
      IF (X.GT.XSML) Y = X*X
      DBSK1E = EXP(X)*(LOG(0.5D0*X)*DBESI1(X) + (0.75D0 +
     1  DCSEVL (0.5D0*Y-1.D0, BK1CS, NTK1))/X )
      RETURN
C
 20   IF (X.LE.8.D0) DBSK1E = (1.25D0 + DCSEVL ((16.D0/X-5.D0)/3.D0,
     1  AK1CS, NTAK1))/SQRT(X)
      IF (X.GT.8.D0) DBSK1E = (1.25D0 +
     1  DCSEVL (16.D0/X-1.D0, AK12CS, NTAK12))/SQRT(X)
C
      RETURN
      END
      SUBROUTINE DBSKNU (X, FNU, KODE, N, Y, NZ)
C***BEGIN PROLOGUE  DBSKNU
C***SUBSIDIARY
C***PURPOSE  Subsidiary to DBESK
C***LIBRARY   SLATEC
C***TYPE      DOUBLE PRECISION (BESKNU-S, DBSKNU-D)
C***AUTHOR  Amos, D. E., (SNLA)
C***DESCRIPTION
C
C     Abstract  **** A DOUBLE PRECISION routine ****
C         DBSKNU computes N member sequences of K Bessel functions
C         K/SUB(FNU+I-1)/(X), I=1,N for non-negative orders FNU and
C         positive X. Equations of the references are implemented on
C         small orders DNU for K/SUB(DNU)/(X) and K/SUB(DNU+1)/(X).
C         Forward recursion with the three term recursion relation
C         generates higher orders FNU+I-1, I=1,...,N. The parameter
C         KODE permits K/SUB(FNU+I-1)/(X) values or scaled values
C         EXP(X)*K/SUB(FNU+I-1)/(X), I=1,N to be returned.
C
C         To start the recursion FNU is normalized to the interval
C         -0.5.LE.DNU.LT.0.5. A special form of the power series is
C         implemented on 0.LT.X.LE.X1 while the Miller algorithm for the
C         K Bessel function in terms of the confluent hypergeometric
C         function U(FNU+0.5,2*FNU+1,X) is implemented on X1.LT.X.LE.X2.
C         For X.GT.X2, the asymptotic expansion for large X is used.
C         When FNU is a half odd integer, a special formula for
C         DNU=-0.5 and DNU+1.0=0.5 is used to start the recursion.
C
C         The maximum number of significant digits obtainable
C         is the smaller of 14 and the number of digits carried in
C         DOUBLE PRECISION arithmetic.
C
C         DBSKNU assumes that a significant digit SINH function is
C         available.
C
C     Description of Arguments
C
C         INPUT      X,FNU are DOUBLE PRECISION
C           X      - X.GT.0.0D0
C           FNU    - Order of initial K function, FNU.GE.0.0D0
C           N      - Number of members of the sequence, N.GE.1
C           KODE   - A parameter to indicate the scaling option
C                    KODE= 1  returns
C                             Y(I)=       K/SUB(FNU+I-1)/(X)
C                                  I=1,...,N
C                        = 2  returns
C                             Y(I)=EXP(X)*K/SUB(FNU+I-1)/(X)
C                                  I=1,...,N
C
C         OUTPUT     Y is DOUBLE PRECISION
C           Y      - A vector whose first N components contain values
C                    for the sequence
C                    Y(I)=       K/SUB(FNU+I-1)/(X), I=1,...,N or
C                    Y(I)=EXP(X)*K/SUB(FNU+I-1)/(X), I=1,...,N
C                    depending on KODE
C           NZ     - Number of components set to zero due to
C                    underflow,
C                    NZ= 0   , normal return
C                    NZ.NE.0 , first NZ components of Y set to zero
C                              due to underflow, Y(I)=0.0D0,I=1,...,NZ
C
C     Error Conditions
C         Improper input arguments - a fatal error
C         Overflow - a fatal error
C         Underflow with KODE=1 - a non-fatal error (NZ.NE.0)
C
C***SEE ALSO  DBESK
C***REFERENCES  N. M. Temme, On the numerical evaluation of the modified
C                 Bessel function of the third kind, Journal of
C                 Computational Physics 19, (1975), pp. 324-337.
C***ROUTINES CALLED  D1MACH, DGAMMA, I1MACH, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   790201  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890911  Removed unnecessary intrinsics.  (WRB)
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900326  Removed duplicate information from DESCRIPTION section.
C           (WRB)
C   900328  Added TYPE section.  (WRB)
C   900727  Added EXTERNAL statement.  (WRB)
C   910408  Updated the AUTHOR and REFERENCES sections.  (WRB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  DBSKNU
C
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
      INTEGER I, IFLAG, INU, J, K, KK, KODE, KODED, N, NN, NZ
      DOUBLE PRECISION A,AK,A1,A2,B,BK,CC,CK,COEF,CX,DK,DNU,DNU2,ELIM,
     1 ETEST, EX, F, FC, FHS, FK, FKS, FLRX, FMU, FNU, G1, G2, P, PI,
     2 PT, P1, P2, Q, RTHPI, RX, S, SMU, SQK, ST, S1, S2, TM, TOL, T1,
     3 T2, X, X1, X2, Y
      DIMENSION A(160), B(160), Y(*), CC(8)
      DOUBLE PRECISION DGAMMA
      EXTERNAL DGAMMA
      SAVE X1, X2, PI, RTHPI, CC
      DATA X1, X2 / 2.0D0, 17.0D0 /
      DATA PI,RTHPI        / 3.14159265358979D+00, 1.25331413731550D+00/
      DATA CC(1), CC(2), CC(3), CC(4), CC(5), CC(6), CC(7), CC(8)
     1                     / 5.77215664901533D-01,-4.20026350340952D-02,
     2-4.21977345555443D-02, 7.21894324666300D-03,-2.15241674114900D-04,
     3-2.01348547807000D-05, 1.13302723200000D-06, 6.11609500000000D-09/
C***FIRST EXECUTABLE STATEMENT  DBSKNU
C
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
C
C     SERIES FOR X.LE.X1
C
      A1 = 1.0D0 - DNU
      A2 = 1.0D0 + DNU
      T1 = 1.0D0/DGAMMA(A1)
      T2 = 1.0D0/DGAMMA(A2)
      IF (ABS(DNU).GT.0.1D0) GO TO 40
C     SERIES FOR F0 TO RESOLVE INDETERMINACY FOR SMALL ABS(DNU)
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
C
C     MILLER ALGORITHM FOR X1.LT.X.LE.X2
C
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
C
C     FORWARD RECURSION ON THE THREE TERM RECURSION RELATION
C
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
C     IFLAG=1 CASES
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
C
C     ASYMPTOTIC EXPANSION FOR LARGE X, X.GT.X2
C
C     IFLAG=0 MEANS NO UNDERFLOW OCCURRED
C     IFLAG=1 MEANS AN UNDERFLOW OCCURRED- COMPUTATION PROCEEDS WITH
C     KODED=2 AND A TEST FOR ON SCALE VALUES IS MADE DURING FORWARD
C     RECURSION
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
C
C     FNU=HALF ODD INTEGER CASE
C
  340 CONTINUE
      S1 = COEF
      S2 = COEF
      GO TO 170
C
C
CC350 CALL XERMSG ('SLATEC', 'DBSKNU', 'X NOT GREATER THAN ZERO', 2, 1)
CCCCC RETURN
CC360 CALL XERMSG ('SLATEC', 'DBSKNU', 'FNU NOT ZERO OR POSITIVE', 2,
CCCCC+   1)
CCCCC RETURN
CC370 CALL XERMSG ('SLATEC', 'DBSKNU', 'KODE NOT 1 OR 2', 2, 1)
CCCCC RETURN
CC380 CALL XERMSG ('SLATEC', 'DBSKNU', 'N NOT GREATER THAN 0', 2, 1)
CCCCC RETURN
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
      END
      DOUBLE PRECISION FUNCTION D9CHU (A, B, Z)
C***BEGIN PROLOGUE  D9CHU
C***SUBSIDIARY
C***PURPOSE  Evaluate for large Z  Z**A * U(A,B,Z) where U is the
C            logarithmic confluent hypergeometric function.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C11
C***TYPE      DOUBLE PRECISION (R9CHU-S, D9CHU-D)
C***KEYWORDS  FNLIB, LOGARITHMIC CONFLUENT HYPERGEOMETRIC FUNCTION,
C             SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C Evaluate for large Z  Z**A * U(A,B,Z)  where U is the logarithmic
C confluent hypergeometric function.  A rational approximation due to Y.
C L. Luke is used.  When U is not in the asymptotic region, i.e., when A
C or B is large compared with Z, considerable significance loss occurs.
C A warning is provided when the computed result is less than half
C precision.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770801  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900720  Routine changed from user-callable to subsidiary.  (WRB)
C***END PROLOGUE  D9CHU
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
      DOUBLE PRECISION A, B, Z, AA(4), BB(4), AB, ANBN, BP, CT1, CT2,
     1  CT3, C2, D1Z, EPS, G1, G2, G3, SAB, SQEPS, X2I1
      LOGICAL FIRST
      SAVE EPS, SQEPS, FIRST
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  D9CHU
C
      D9CHU = 0.0D0
C
      IF (FIRST) THEN
         EPS = 4.0D0*D1MACH(4)
         SQEPS = SQRT(D1MACH(4))
      ENDIF
      FIRST = .FALSE.
C
      BP = 1.0D0 + A - B
      AB = A*BP
      CT2 = 2.0D0 * (Z - AB)
      SAB = A + BP
C
      BB(1) = 1.0D0
      AA(1) = 1.0D0
C
      CT3 = SAB + 1.0D0 + AB
      BB(2) = 1.0D0 + 2.0D0*Z/CT3
      AA(2) = 1.0D0 + CT2/CT3
C
      ANBN = CT3 + SAB + 3.0D0
      CT1 = 1.0D0 + 2.0D0*Z/ANBN
      BB(3) = 1.0D0 + 6.0D0*CT1*Z/CT3
      AA(3) = 1.0D0 + 6.0D0*AB/ANBN + 3.0D0*CT1*CT2/CT3
C
      DO 30 I=4,300
        X2I1 = 2*I - 3
        CT1 = X2I1/(X2I1-2.0D0)
        ANBN = ANBN + X2I1 + SAB
        CT2 = (X2I1 - 1.0D0)/ANBN
        C2 = X2I1*CT2 - 1.0D0
        D1Z = X2I1*2.0D0*Z/ANBN
C
        CT3 = SAB*CT2
        G1 = D1Z + CT1*(C2+CT3)
        G2 = D1Z - C2
        G3 = CT1*(1.0D0 - CT3 - 2.0D0*CT2)
C
        BB(4) = G1*BB(3) + G2*BB(2) + G3*BB(1)
        AA(4) = G1*AA(3) + G2*AA(2) + G3*AA(1)
        IF (ABS(AA(4)*BB(1)-AA(1)*BB(4)).LT.EPS*ABS(BB(4)*BB(1)))
     1    GO TO 40
C
C IF OVERFLOWS OR UNDERFLOWS PROVE TO BE A PROBLEM, THE STATEMENTS
C BELOW COULD BE ALTERED TO INCORPORATE A DYNAMICALLY ADJUSTED SCALE
C FACTOR.
C
        DO 20 J=1,3
          AA(J) = AA(J+1)
          BB(J) = BB(J+1)
 20     CONTINUE
 30   CONTINUE
      WRITE(ICOUT,101)
      CALL DPWRST('XXX','BUG ')
  101 FORMAT('***** ERROR FROM D9CHU, NO CONVERGENCE IN 300 TERMS. ***')
      RETURN
C
 40   D9CHU = AA(4)/BB(4)
C
      IF (D9CHU .LT. SQEPS .OR. D9CHU .GT. 1.0D0/SQEPS) THEN
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
      ENDIF
  111 FORMAT('***** WARNING FROM D9CHU, THE ANSWER IS LESS THAN HALF ',
     1       'PRECISION FOR CHU FUNCTION.  *****.')
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION D9GMIT (A, X, ALGAP1, SGNGAM, ALX)
C***BEGIN PROLOGUE  D9GMIT
C***SUBSIDIARY
C***PURPOSE  Compute Tricomi's incomplete Gamma function for small
C            arguments.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7E
C***TYPE      DOUBLE PRECISION (R9GMIT-S, D9GMIT-D)
C***KEYWORDS  COMPLEMENTARY INCOMPLETE GAMMA FUNCTION, FNLIB, SMALL X,
C             SPECIAL FUNCTIONS, TRICOMI
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C Compute Tricomi's incomplete gamma function for small X.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, DLNGAM, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890911  Removed unnecessary intrinsics.  (WRB)
C   890911  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900720  Routine changed from user-callable to subsidiary.  (WRB)
C***END PROLOGUE  D9GMIT
      DOUBLE PRECISION A, X, ALGAP1, SGNGAM, ALX, AE, AEPS, ALGS, ALG2,
     1  BOT, EPS, FK, S, SGNG2, T, TE, DLNGAM
      LOGICAL FIRST
      SAVE EPS, BOT, FIRST
C
C---------------------------------------------------------------------
C
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
      DATA FIRST /.TRUE./
C
      ALGS=0.0D0
C
C***FIRST EXECUTABLE STATEMENT  D9GMIT
      IF(ISUBG4.EQ.'GMIT')THEN
        WRITE(ICOUT,91)A,X,ALGAP1,SGNGAM,ALX
   91   FORMAT('FROM D9GMIT: A,X,ALGAP1,SGNGAM,ALX = ',5G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF (FIRST) THEN
         EPS = 0.5D0*D1MACH(3)
         BOT = LOG (D1MACH(1))
      ENDIF
      FIRST = .FALSE.
C
      IF (X .LE. 0.D0) THEN
        WRITE(ICOUT,1)
    1   FORMAT('***** ERORR FROM D9GMIT, X MUST BE POSITIVE.  *******')
        CALL DPWRST('XXX','BUG ')
        D9GMIT=0.D0
        RETURN
      ENDIF
C
      MA = INT(A + 0.5D0)
      IF (A.LT.0.D0) MA = INT(A - 0.5D0)
      AEPS = A - REAL(MA)
C
      AE = A
      IF (A.LT.(-0.5D0)) AE = AEPS
C
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
C
      WRITE(ICOUT,21)
   21 FORMAT('***** ERROR FROM D9GMIT.  NO CONVERGENCE IN 200')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,22)
   22 FORMAT('      TERMS OF TAYLOR-S SERIES.                ******')
      CALL DPWRST('XXX','BUG ')
      D9GMIT=0.D0
      RETURN
C
 30   IF (A.GE.(-0.5D0)) ALGS = -ALGAP1 + LOG(S)
      IF (A.GE.(-0.5D0)) GO TO 60
C
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
C
 50   D9GMIT = 0.0D0
      ALGS = -MA*LOG(X) + ALGS
      IF (S.EQ.0.D0 .OR. AEPS.EQ.0.D0) GO TO 60
C
      SGNG2 = SGNGAM * SIGN (1.0D0, S)
      ALG2 = -X - ALGAP1 + LOG(ABS(S))
C
      IF (ALG2.GT.BOT) D9GMIT = SGNG2 * EXP(ALG2)
      IF (ALGS.GT.BOT) D9GMIT = D9GMIT + EXP(ALGS)
      RETURN
C
 60   D9GMIT = EXP (ALGS)
      RETURN
C
      END
      DOUBLE PRECISION FUNCTION D9GMIC (A, X, ALX)
C***BEGIN PROLOGUE  D9GMIC
C***SUBSIDIARY
C***PURPOSE  Compute the complementary incomplete Gamma function for A
C            near a negative integer and X small.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7E
C***TYPE      DOUBLE PRECISION (R9GMIC-S, D9GMIC-D)
C***KEYWORDS  COMPLEMENTARY INCOMPLETE GAMMA FUNCTION, FNLIB, SMALL X,
C             SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C Compute the complementary incomplete gamma function for A near
C a negative integer and for small X.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, DLNGAM, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890911  Removed unnecessary intrinsics.  (WRB)
C   890911  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900720  Routine changed from user-callable to subsidiary.  (WRB)
C***END PROLOGUE  D9GMIC
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
      DOUBLE PRECISION A, X, ALX, ALNG, BOT, EPS, EULER, FK, FKP1, FM,
     1  S, SGNG, T, TE, DLNGAM
      LOGICAL FIRST
      SAVE EULER, EPS, BOT, FIRST
      DATA EULER / 0.5772156649 0153286060 6512090082 40 D0 /
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  D9GMIC
      IF (FIRST) THEN
         EPS = 0.5D0*D1MACH(3)
         BOT = LOG (D1MACH(1))
      ENDIF
      FIRST = .FALSE.
C
      IF (A .GT. 0.D0) THEN
        WRITE(ICOUT,2)
    2   FORMAT('***** ERORR FROM D9GMIC, SECOND ARGUMENT MUST BE ',
     1   'NEAR A NEGATIVE INTEGER.  *******')
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
C
      M = INT(-(A - 0.5D0))
      FM = REAL(M)
C
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
C
 30   D9GMIC = -ALX - EULER + X*S/(FM+1.0D0)
      IF (M.EQ.0) RETURN
C
      IF (M.EQ.1) D9GMIC = -D9GMIC - 1.D0 + 1.D0/X
      IF (M.EQ.1) RETURN
C
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
C
 50   DO 60 K=1,M
        D9GMIC = D9GMIC + 1.0D0/K
 60   CONTINUE
C
      SGNG = 1.0D0
      IF (MOD(M,2).EQ.1) SGNG = -1.0D0
      ALNG = LOG(D9GMIC) - DLNGAM(FM+1.D0)
C
      D9GMIC = 0.D0
      IF (ALNG.GT.BOT) D9GMIC = SGNG * EXP(ALNG)
      IF (S.NE.0.D0) D9GMIC = D9GMIC +
     1  SIGN (EXP(-FM*ALX+LOG(ABS(S)/FM)), S)
C
      IF (D9GMIC .EQ. 0.D0 .AND. S .EQ. 0.D0) THEN
        WRITE(ICOUT,31)
   31   FORMAT('***** ERROR FROM D9GMIC.  RESULT UNDERFLOWS.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      RETURN
C
      END
      DOUBLE PRECISION FUNCTION D9LGIC (A, X, ALX)
C***BEGIN PROLOGUE  D9LGIC
C***SUBSIDIARY
C***PURPOSE  Compute the log complementary incomplete Gamma function
C            for large X and for A .LE. X.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7E
C***TYPE      DOUBLE PRECISION (R9LGIC-S, D9LGIC-D)
C***KEYWORDS  COMPLEMENTARY INCOMPLETE GAMMA FUNCTION, FNLIB, LARGE X,
C             LOGARITHM, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C Compute the log complementary incomplete gamma function for large X
C and for A .LE. X.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900720  Routine changed from user-callable to subsidiary.  (WRB)
C***END PROLOGUE  D9LGIC
      DOUBLE PRECISION A, X, ALX, EPS, FK, P, R, S, T, XMA, XPA
      SAVE EPS
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
      DATA EPS / 0.D0 /
C***FIRST EXECUTABLE STATEMENT  D9LGIC
      IF (EPS.EQ.0.D0) EPS = 0.5D0*D1MACH(3)
C
      XPA = X + 1.0D0 - A
      XMA = X - 1.D0 - A
C
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
C
 20   D9LGIC = A*ALX - X + LOG(S/XPA)
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION D9LGIT (A, X, ALGAP1)
C***BEGIN PROLOGUE  D9LGIT
C***SUBSIDIARY
C***PURPOSE  Compute the logarithm of Tricomi's incomplete Gamma
C            function with Perron's continued fraction for large X and
C            A .GE. X.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7E
C***TYPE      DOUBLE PRECISION (R9LGIT-S, D9LGIT-D)
C***KEYWORDS  FNLIB, INCOMPLETE GAMMA FUNCTION, LOGARITHM,
C             PERRON'S CONTINUED FRACTION, SPECIAL FUNCTIONS, TRICOMI
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C Compute the log of Tricomi's incomplete gamma function with Perron's
C continued fraction for large X and for A .GE. X.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900720  Routine changed from user-callable to subsidiary.  (WRB)
C***END PROLOGUE  D9LGIT
      DOUBLE PRECISION A, X, ALGAP1, AX, A1X, EPS, FK, HSTAR, P, R, S,
     1  SQEPS, T
      LOGICAL FIRST
      SAVE EPS, SQEPS, FIRST
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  D9LGIT
      IF (FIRST) THEN
         EPS = 0.5D0*D1MACH(3)
         SQEPS = SQRT(D1MACH(4))
      ENDIF
      FIRST = .FALSE.
C
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
C
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
C
 30   HSTAR = 1.0D0 - X*S/A1X
      IF (HSTAR .LT. SQEPS)THEN
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,32)
        CALL DPWRST('XXX','BUG ')
      ENDIF
 31   FORMAT('***** WARNING FROM D9LGIT.  RESULT LESS THAN HALF ')
 32   FORMAT('      PRECISION.                                  *****')
C
      D9LGIT = -X - ALGAP1 - LOG(HSTAR)
      RETURN
C
      END
      DOUBLE PRECISION FUNCTION D9LGMC (X)
C***BEGIN PROLOGUE  D9LGMC
C***SUBSIDIARY
C***PURPOSE  Compute the log Gamma correction factor so that
C            LOG(DGAMMA(X)) = LOG(SQRT(2*PI)) + (X-5.)*LOG(X) - X
C            + D9LGMC(X).
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7E
C***TYPE      DOUBLE PRECISION (R9LGMC-S, D9LGMC-D, C9LGMC-C)
C***KEYWORDS  COMPLETE GAMMA FUNCTION, CORRECTION TERM, FNLIB,
C             LOG GAMMA, LOGARITHM, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C Compute the log gamma correction factor for X .GE. 10. so that
C LOG (DGAMMA(X)) = LOG(SQRT(2*PI)) + (X-.5)*LOG(X) - X + D9lGMC(X)
C
C Series for ALGM       on the interval  0.          to  1.00000E-02
C                                        with weighted error   1.28E-31
C                                         log weighted error  30.89
C                               significant figures required  29.81
C                                    decimal places required  31.48
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, DCSEVL, INITDS, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900720  Routine changed from user-callable to subsidiary.  (WRB)
C***END PROLOGUE  D9LGMC
      DOUBLE PRECISION X, ALGMCS(15), XBIG, XMAX, DCSEVL
      LOGICAL FIRST
      SAVE ALGMCS, NALGM, XBIG, XMAX, FIRST
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
      DATA ALGMCS(  1) / +.1666389480 4518632472 0572965082 2 D+0      /
      DATA ALGMCS(  2) / -.1384948176 0675638407 3298605913 5 D-4      /
      DATA ALGMCS(  3) / +.9810825646 9247294261 5717154748 7 D-8      /
      DATA ALGMCS(  4) / -.1809129475 5724941942 6330626671 9 D-10     /
      DATA ALGMCS(  5) / +.6221098041 8926052271 2601554341 6 D-13     /
      DATA ALGMCS(  6) / -.3399615005 4177219443 0333059966 6 D-15     /
      DATA ALGMCS(  7) / +.2683181998 4826987489 5753884666 6 D-17     /
      DATA ALGMCS(  8) / -.2868042435 3346432841 4462239999 9 D-19     /
      DATA ALGMCS(  9) / +.3962837061 0464348036 7930666666 6 D-21     /
      DATA ALGMCS( 10) / -.6831888753 9857668701 1199999999 9 D-23     /
      DATA ALGMCS( 11) / +.1429227355 9424981475 7333333333 3 D-24     /
      DATA ALGMCS( 12) / -.3547598158 1010705471 9999999999 9 D-26     /
      DATA ALGMCS( 13) / +.1025680058 0104709120 0000000000 0 D-27     /
      DATA ALGMCS( 14) / -.3401102254 3167487999 9999999999 9 D-29     /
      DATA ALGMCS( 15) / +.1276642195 6300629333 3333333333 3 D-30     /
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  D9LGMC
      IF (FIRST) THEN
         NALGM = INITDS (ALGMCS, 15, REAL(D1MACH(3)) )
         XBIG = 1.0D0/SQRT(D1MACH(3))
         XMAX = EXP (MIN(LOG(D1MACH(2)/12.D0), -LOG(12.D0*D1MACH(1))))
      ENDIF
      FIRST = .FALSE.
C
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
C
      D9LGMC = 1.D0/(12.D0*X)
      IF (X.LT.XBIG) D9LGMC = DCSEVL (2.0D0*(10.D0/X)**2-1.D0, ALGMCS,
     1  NALGM) / X
      RETURN
C
 20   D9LGMC = 0.D0
      WRITE(ICOUT,21)
 21   FORMAT('***** WARNING FROM D9LGMC.  X SO BIG D9LCMC UNDERFLOWS.')
      CALL DPWRST('XXX','BUG ')
      RETURN
C
      END
      DOUBLE PRECISION FUNCTION DBETA (A, B)
C***BEGIN PROLOGUE  DBETA
C***PURPOSE  Compute the complete Beta function.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7B
C***TYPE      DOUBLE PRECISION (BETA-S, DBETA-D, CBETA-C)
C***KEYWORDS  COMPLETE BETA FUNCTION, FNLIB, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C DBETA(A,B) calculates the double precision complete beta function
C for double precision arguments A and B.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, DGAMLM, DGAMMA, DLBETA, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900727  Added EXTERNAL statement.  (WRB)
C***END PROLOGUE  DBETA
      DOUBLE PRECISION A, B, ALNSML, XMAX, XMIN, DLBETA, DGAMMA
      LOGICAL FIRST
      EXTERNAL DGAMMA
      SAVE XMAX, ALNSML, FIRST
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  DBETA
C
      DBETA = 0.0D0
C
      IF (FIRST) THEN
         CALL DGAMLM (XMIN, XMAX)
         ALNSML = LOG (D1MACH(1))
      ENDIF
      FIRST = .FALSE.
C
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
C
      IF (A+B.LT.XMAX) DBETA = DGAMMA(A)*DGAMMA(B)/DGAMMA(A+B)
      IF (A+B.LT.XMAX) RETURN
C
      DBETA = DLBETA (A, B)
      IF (DBETA.LT.ALNSML) GO TO 20
      DBETA = EXP (DBETA)
      RETURN
C
 20   DBETA = 0.D0
      WRITE(ICOUT,21)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,22)
      CALL DPWRST('XXX','BUG ')
   21 FORMAT('***** ERROR FROM DBETA.  ALPHA AND BETA ARE SO ')
   22 FORMAT('      LARGE THAT THE BETA FUNCTION OVERFLOWS.  *****')
      RETURN
C
      END
      DOUBLE PRECISION FUNCTION DBETAI (X, PIN, QIN)
C***BEGIN PROLOGUE  DBETAI
C***PURPOSE  Calculate the incomplete Beta function.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7F
C***TYPE      DOUBLE PRECISION (BETAI-S, DBETAI-D)
C***KEYWORDS  FNLIB, INCOMPLETE BETA FUNCTION, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C   DBETAI calculates the DOUBLE PRECISION incomplete beta function.
C
C   The incomplete beta function ratio is the probability that a
C   random variable from a beta distribution having parameters PIN and
C   QIN will be less than or equal to X.
C
C     -- Input Arguments -- All arguments are DOUBLE PRECISION.
C   X      upper limit of integration.  X must be in (0,1) inclusive.
C   PIN    first beta distribution parameter.  PIN must be .GT. 0.0.
C   QIN    second beta distribution parameter.  QIN must be .GT. 0.0.
C
C***REFERENCES  Nancy E. Bosten and E. L. Battiste, Remark on Algorithm
C                 179, Communications of the ACM 17, 3 (March 1974),
C                 pp. 156.
C***ROUTINES CALLED  D1MACH, DLBETA, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890911  Removed unnecessary intrinsics.  (WRB)
C   890911  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   920528  DESCRIPTION and REFERENCES sections revised.  (WRB)
C***END PROLOGUE  DBETAI
      DOUBLE PRECISION X, PIN, QIN, ALNEPS, ALNSML, C, EPS, FINSUM, P,
     1  PS, Q, SML, TERM, XB, XI, Y, DLBETA, P1
      LOGICAL FIRST
      SAVE EPS, ALNEPS, SML, ALNSML, FIRST
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  DBETAI
      IF (FIRST) THEN
         EPS = D1MACH(3)
         ALNEPS = LOG (EPS)
         SML = D1MACH(1)
         ALNSML = LOG (SML)
      ENDIF
      FIRST = .FALSE.
C
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
C
      Y = X
      P = PIN
      Q = QIN
      IF (Q.LE.P .AND. X.LT.0.8D0) GO TO 20
      IF (X.LT.0.2D0) GO TO 20
      Y = 1.0D0 - Y
      P = QIN
      Q = PIN
C
 20   IF ((P+Q)*Y/(P+1.D0).LT.EPS) GO TO 80
C
C EVALUATE THE INFINITE SUM FIRST.  TERM WILL EQUAL
C Y**P/BETA(PS,P) * (1.-PS)-SUB-I * Y**I / FAC(I) .
C
      PS = Q - AINT(Q)
      IF (PS.EQ.0.D0) PS = 1.0D0
      XB = P*LOG(Y) - DLBETA(PS,P) - LOG(P)
      DBETAI = 0.0D0
      IF (XB.LT.ALNSML) GO TO 40
C
      DBETAI = EXP (XB)
      TERM = DBETAI*P
      IF (PS.EQ.1.0D0) GO TO 40
      N = INT(MAX (ALNEPS/LOG(Y), 4.0D0))
      DO 30 I=1,N
        XI = REAL(I)
        TERM = TERM * (XI-PS)*Y/XI
        DBETAI = DBETAI + TERM/(P+XI)
 30   CONTINUE
C
C NOW EVALUATE THE FINITE SUM, MAYBE.
C
 40   IF (Q.LE.1.0D0) GO TO 70
C
      XB = P*LOG(Y) + Q*LOG(1.0D0-Y) - DLBETA(P,Q) - LOG(Q)
      IB = INT(MAX (XB/ALNSML, 0.0D0))
      TERM = EXP(XB - IB*ALNSML)
      C = 1.0D0/(1.D0-Y)
      P1 = Q*C/(P+Q-1.D0)
C
      FINSUM = 0.0D0
      N = INT(Q)
      IF (Q.EQ.DBLE(N)) N = N - 1
      DO 50 I=1,N
        IF (P1.LE.1.0D0 .AND. TERM/EPS.LE.FINSUM) GO TO 60
        XI = I
        TERM = (Q-XI+1.0D0)*C*TERM/(P+Q-XI)
C
        IF (TERM.GT.1.0D0) IB = IB - 1
        IF (TERM.GT.1.0D0) TERM = TERM*SML
C
        IF (IB.EQ.0) FINSUM = FINSUM + TERM
 50   CONTINUE
C
 60   DBETAI = DBETAI + FINSUM
 70   IF (Y.NE.X .OR. P.NE.PIN) DBETAI = 1.0D0 - DBETAI
      DBETAI = MAX (MIN (DBETAI, 1.0D0), 0.0D0)
      RETURN
C
 80   DBETAI = 0.0D0
      XB = P*LOG(MAX(Y,SML)) - LOG(P) - DLBETA(P,Q)
      IF (XB.GT.ALNSML .AND. Y.NE.0.0D0) DBETAI = EXP(XB)
      IF (Y.NE.X .OR. P.NE.PIN) DBETAI = 1.0D0 - DBETAI
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION DEBYE1(XVALUE)
C
C
C   DEFINITION:
C
C      This program calculates the Debye function of order 1, defined as
C
C            DEBYE1(x) = [Integral {0 to x} t/(exp(t)-1) dt] / x
C
C      The code uses Chebyshev series whose coefficients
C      are given to 20 decimal places.
C
C
C   ERROR RETURNS:
C
C      If XVALUE < 0.0 an error message is printed and the
C      function returns the value 0.0
C
C
C   MACHINE-DEPENDENT PARAMETERS:
C
C      NTERMS - INTEGER - The no. of elements of the array ADEB1.
C                         The recommended value is such that
C                             ABS(ADEB1(NTERMS)) < EPS/100 , with
C                                   1 <= NTERMS <= 18
C
C      XLOW - DOUBLE PRECISION - The value below which
C                    DEBYE1 = 1 - x/4 + x*x/36 to machine precision.
C                    The recommended value is 
C                        SQRT(8*EPSNEG)
C
C      XUPPER - DOUBLE PRECISION - The value above which 
C                      DEBYE1 = (pi*pi/(6*x)) - exp(-x)(x+1)/x.
C                      The recommended value is
C                          -LOG(2*EPS)
C
C      XLIM - DOUBLE PRECISION - The value above which DEBYE1 = pi*pi/(6*x)
C                    The recommended value is 
C                          -LOG(XMIN)
C
C      For values of EPS, EPSNEG, and XMIN see the file MACHCON.TXT
C
C      The machine-dependent constants are computed internally by
C      using the D1MACH subroutine.
C
C
C   INTRINSIC FUNCTIONS USED:
C
C      AINT , EXP , INT , LOG , SQRT
C      
C
C   OTHER MISCFUN SUBROUTINES USED:
C
C          CHEVAL , ERRPRN, D1MACH
C
C
C   AUTHOR:
C          Dr. Allan J. MacLeod,
C          Dept. of Mathematics and Statistics,
C          University of Paisley
C          High St.
C          PAISLEY
C          SCOTLAND
C          PA1 2BE
C
C          (e-mail:  macl_ms0@paisley.ac.uk )
C
C
C   LATEST UPDATE:  23 january, 1996
C
      INTEGER I,NEXP,NTERMS
      DOUBLE PRECISION ADEB1(0:18),CHEVAL,DEBINF,EIGHT,EXPMX,FOUR,HALF,
     &     NINE,ONE,ONEHUN,QUART,RK,SUM,T,THIRT6,X,XK,XLIM,XLOW,
     &     XUPPER,XVALUE,ZERO
CCCCC CHARACTER FNNAME*6,ERRMSG*17
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
CCCCC DATA FNNAME/'DEBYE1'/
CCCCC DATA ERRMSG/'ARGUMENT NEGATIVE'/
      DATA ZERO,QUART/0.0 D 0 , 0.25 D 0/
      DATA HALF,ONE/0.5 D 0 , 1.0 D 0/
      DATA FOUR,EIGHT/4.0 D 0 , 8.0 D 0/
      DATA NINE,THIRT6,ONEHUN/9.0 D 0 , 36.0 D 0 , 100.0 D 0/
      DATA DEBINF/0.60792 71018 54026 62866 D 0/
      DATA ADEB1/2.40065 97190 38141 01941  D    0,
     1           0.19372 13042 18936 00885  D    0,
     2          -0.62329 12455 48957 703    D   -2,
     3           0.35111 74770 20648 00     D   -3,
     4          -0.22822 24667 01231 0      D   -4,
     5           0.15805 46787 50300        D   -5,
     6          -0.11353 78197 0719         D   -6,
     7           0.83583 36118 75           D   -8,
     8          -0.62644 24787 2            D   -9,
     9           0.47603 34890              D  -10,
     X          -0.36574 1540               D  -11,
     1           0.28354 310                D  -12,
     2          -0.22147 29                 D  -13,
     3           0.17409 2                  D  -14,
     4          -0.13759                    D  -15,
     5           0.1093                     D  -16,
     6          -0.87                       D  -18,
     7           0.7                        D  -19,
     8          -0.1                        D  -19/
C
C   Start computation
C
      X = XVALUE
C
C   Check XVALUE >= 0.0
C 
      IF ( X .LT. ZERO ) THEN
CCCCC    CALL ERRPRN(FNNAME,ERRMSG)
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,101)X
         CALL DPWRST('XXX','BUG ')
         DEBYE1 = ZERO
         RETURN
      ENDIF
  999 FORMAT(1X)
  101 FORMAT('***** ERROR FROM DEBYE1--ARGUMENT MUST BE ',
     1       'NON-NEGATIVE, ARGUMENT = ',G15.7)
C
C   Compute the machine-dependent constants.
C
      T = D1MACH(3)
      XLOW = SQRT ( T * EIGHT )
      XUPPER = - LOG( T + T )
      XLIM = - LOG( D1MACH(1) )
      T = T / ONEHUN
      DO 10 NTERMS = 18 , 0 , -1
         IF ( ABS(ADEB1(NTERMS)) .GT. T ) GOTO 19
 10   CONTINUE
C
C   Code for x <= 4.0
C 
 19   IF ( X .LE. FOUR ) THEN
         IF ( X .LT. XLOW ) THEN
            DEBYE1 = ( ( X - NINE ) * X + THIRT6 ) / THIRT6
         ELSE
            T = ( ( X * X / EIGHT ) - HALF ) - HALF
            DEBYE1 = CHEVAL( NTERMS , ADEB1 , T ) - QUART * X
         ENDIF
      ELSE
C
C   Code for x > 4.0
C 
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
      END
      DOUBLE PRECISION FUNCTION DEBYE2(XVALUE)
C
C
C   DEFINITION:
C
C      This program calculates the Debye function of order 1, defined as
C
C            DEBYE2(x) = 2*[Integral {0 to x} t*t/(exp(t)-1) dt] / (x*x)
C
C      The code uses Chebyshev series whose coefficients
C      are given to 20 decimal places.
C
C
C   ERROR RETURNS:
C
C      If XVALUE < 0.0 an error message is printed and the
C      function returns the value 0.0
C
C
C   MACHINE-DEPENDENT PARAMETERS:
C
C      NTERMS - INTEGER - The no. of elements of the array ADEB2.
C                         The recommended value is such that
C                             ABS(ADEB2(NTERMS)) < EPS/100,
C                         subject to 1 <= NTERMS <= 18.
C
C      XLOW - DOUBLE PRECISION - The value below which
C                    DEBYE2 = 1 - x/3 + x*x/24 to machine precision.
C                    The recommended value is 
C                        SQRT(8*EPSNEG)
C
C      XUPPER - DOUBLE PRECISION - The value above which 
C                      DEBYE2 = (4*zeta(3)/x^2) - 2*exp(-x)(x^2+2x+1)/x^2.
C                      The recommended value is
C                          -LOG(2*EPS)
C
C      XLIM1 - DOUBLE PRECISION - The value above which DEBYE2 = 4*zeta(3)/x^2
C                     The recommended value is 
C                          -LOG(XMIN)
C
C      XLIM2 - DOUBLE PRECISION - The value above which DEBYE2 = 0.0 to machine
C                     precision. The recommended value is
C                           SQRT(4.8/XMIN)
C
C      For values of EPS, EPSNEG, and XMIN see the file MACHCON.TXT
C
C
C      The machine-dependent constants are computed internally by
C      using the D1MACH subroutine.
C
C
C   INTRINSIC FUNCTIONS USED:
C
C      AINT , EXP , INT , LOG , SQRT
C      
C
C   OTHER MISCFUN SUBROUTINES USED:
C
C          CHEVAL , ERRPRN, D1MACH
C
C
C   AUTHOR:
C          Dr. Allan J. MacLeod,
C          Dept. of Mathematics and Statistics,
C          University of Paisley
C          High St.
C          PAISLEY
C          SCOTLAND
C          PA1 2BE
C
C          (e-mail:  macl_ms0@paisley.ac.uk )
C
C
C   LATEST UPDATE:  23 January, 1996
C
      INTEGER I,NEXP,NTERMS
      DOUBLE PRECISION ADEB2(0:18),CHEVAL,DEBINF,EIGHT,EXPMX,FOUR,
     &     HALF,ONE,ONEHUN,RK,SUM,T,THREE,TWENT4,TWO,X,XK,XLIM1,
     &     XLIM2,XLOW,XUPPER,XVALUE,ZERO
CCCCC CHARACTER FNNAME*6,ERRMSG*17
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
CCCCC DATA FNNAME/'DEBYE2'/
CCCCC DATA ERRMSG/'ARGUMENT NEGATIVE'/
      DATA ZERO,HALF/0.0 D 0 , 0.5 D 0/
      DATA ONE,TWO,THREE/1.0 D 0 , 2.0 D 0 , 3.0 D 0/
      DATA FOUR,EIGHT,TWENT4/4.0 D 0 , 8.0 D 0 , 24.0 D 0/
      DATA ONEHUN/100.0 D 0/
      DATA DEBINF/4.80822 76126 38377 14160 D 0/
      DATA ADEB2/2.59438 10232 57077 02826  D    0,
     1           0.28633 57204 53071 98337  D    0,
     2          -0.10206 26561 58046 7129   D   -1,
     3           0.60491 09775 34684 35     D   -3,
     4          -0.40525 76589 50210 4      D   -4,
     5           0.28633 82632 88107        D   -5,
     6          -0.20863 94303 0651         D   -6,
     7           0.15523 78758 264          D   -7,
     8          -0.11731 28008 66           D   -8,
     9           0.89735 85888              D  -10,
     X          -0.69317 6137               D  -11,
     1           0.53980 568                D  -12,
     2          -0.42324 05                 D  -13,
     3           0.33377 8                  D  -14,
     4          -0.26455                    D  -15,
     5           0.2106                     D  -16,
     6          -0.168                      D  -17,
     7           0.13                       D  -18,
     8          -0.1                        D  -19/
C
C   Start computation
C
      X = XVALUE
C
C   Check XVALUE >= 0.0
C 
      IF ( X .LT. ZERO ) THEN
CCCCC    CALL ERRPRN(FNNAME,ERRMSG)
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,101)X
         CALL DPWRST('XXX','BUG ')
         DEBYE2 = ZERO
         RETURN
      ENDIF
  999 FORMAT(1X)
  101 FORMAT('***** ERROR FROM DEBYE2--ARGUMENT MUST BE ',
     1       'NON-NEGATIVE, ARGUMENT = ',G15.7)
C
C   Compute the machine-dependent constants.
C
      T = D1MACH(1)
      XLIM1 = - LOG( T )
      XLIM2 = SQRT( DEBINF ) / SQRT( T )
      T = D1MACH(3)
      XLOW = SQRT ( T * EIGHT )
      XUPPER = - LOG( T + T )
      T = T / ONEHUN
      DO 10 NTERMS = 18 , 0 , -1
         IF ( ABS(ADEB2(NTERMS)) .GT. T ) GOTO 19
 10   CONTINUE
C
C   Code for x <= 4.0
C 
 19   IF ( X .LE. FOUR ) THEN
         IF ( X .LT. XLOW ) THEN
            DEBYE2 = ( ( X - EIGHT ) * X + TWENT4 ) / TWENT4
         ELSE
            T = ( ( X * X / EIGHT ) - HALF ) - HALF
            DEBYE2 = CHEVAL ( NTERMS , ADEB2 , T ) - X / THREE
         ENDIF
      ELSE
C
C   Code for x > 4.0
C 
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
      END
      DOUBLE PRECISION FUNCTION DEBYE3(XVALUE)
C
C
C   DEFINITION:
C
C      This program calculates the Debye function of order 3, defined as
C
C            DEBYE3(x) = 3*[Integral {0 to x} t^3/(exp(t)-1) dt] / (x^3)
C
C      The code uses Chebyshev series whose coefficients
C      are given to 20 decimal places.
C
C
C   ERROR RETURNS:
C
C      If XVALUE < 0.0 an error message is printed and the
C      function returns the value 0.0
C
C
C   MACHINE-DEPENDENT PARAMETERS:
C
C      NTERMS - INTEGER - The no. of elements of the array ADEB3.
C                         The recommended value is such that
C                             ABS(ADEB3(NTERMS)) < EPS/100,
C                         subject to 1 <= NTERMS <= 18
C
C      XLOW - DOUBLE PRECISION - The value below which
C                    DEBYE3 = 1 - 3x/8 + x*x/20 to machine precision.
C                    The recommended value is 
C                        SQRT(8*EPSNEG)
C
C      XUPPER - DOUBLE PRECISION - The value above which 
C               DEBYE3 = (18*zeta(4)/x^3) - 3*exp(-x)(x^3+3x^2+6x+6)/x^3.
C                      The recommended value is
C                          -LOG(2*EPS)
C
C      XLIM1 - DOUBLE PRECISION - The value above which DEBYE3 = 18*zeta(4)/x^3
C                     The recommended value is 
C                          -LOG(XMIN)
C
C      XLIM2 - DOUBLE PRECISION - The value above which DEBYE3 = 0.0 to machine
C                     precision. The recommended value is
C                          CUBE ROOT(19/XMIN)
C
C      For values of EPS, EPSNEG, and XMIN see the file MACHCON.TXT
C
C      The machine-dependent constants are computed internally by
C      using the D1MACH subroutine.
C
C
C   OTHER MISCFUN SUBROUTINES USED:
C
C          CHEVAL , ERRPRN, D1MACH
C
C
C   INTRINSIC FUNCTIONS USED:
C
C      AINT , EXP , INT , LOG , SQRT
C      
C
C   AUTHOR:
C          Dr. Allan J. MacLeod,
C          Dept. of Mathematics and Statistics,
C          University of Paisley
C          High St.
C          PAISLEY
C          SCOTLAND
C          PA1 2BE
C
C          (e-mail:  macl_ms0@paisley.ac.uk )
C
C
C   LATEST UPDATE:  23 January, 1996
C
      INTEGER I,NEXP,NTERMS
      DOUBLE PRECISION ADEB3(0:18),CHEVAL,DEBINF,EIGHT,EXPMX,FOUR,
     &     HALF,ONE,ONEHUN,PT375,RK,SEVP5,SIX,SUM,T,THREE,TWENTY,X,
     &     XK,XKI,XLIM1,XLIM2,XLOW,XUPPER,XVALUE,ZERO
CCCCC CHARACTER FNNAME*6,ERRMSG*17
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
CCCCC DATA FNNAME/'DEBYE3'/
CCCCC DATA ERRMSG/'ARGUMENT NEGATIVE'/
      DATA ZERO,PT375/0.0 D 0 , 0.375 D 0/
      DATA HALF,ONE/0.5 D 0 , 1.0 D 0/
      DATA THREE,FOUR,SIX/3.0 D 0 , 4.0 D 0 , 6.0 D 0/
      DATA SEVP5,EIGHT,TWENTY/7.5 D 0 , 8.0 D 0 , 20.0 D 0/
      DATA ONEHUN/100.0 D 0/
      DATA DEBINF/0.51329 91127 34216 75946 D -1/
      DATA ADEB3/2.70773 70683 27440 94526  D    0,
     1           0.34006 81352 11091 75100  D    0,
     2          -0.12945 15018 44408 6863   D   -1,
     3           0.79637 55380 17381 64     D   -3,
     4          -0.54636 00095 90823 8      D   -4,
     5           0.39243 01959 88049        D   -5,
     6          -0.28940 32823 5386         D   -6,
     7           0.21731 76139 625          D   -7,
     8          -0.16542 09994 98           D   -8,
     9           0.12727 96189 2            D   -9,
     X          -0.98796 3459               D  -11,
     1           0.77250 740                D  -12,
     2          -0.60779 72                 D  -13,
     3           0.48075 9                  D  -14,
     4          -0.38204                    D  -15,
     5           0.3048                     D  -16,
     6          -0.244                      D  -17,
     7           0.20                       D  -18,
     8          -0.2                        D  -19/
C
C   Start computation
C
      X = XVALUE
C
C   Error test
C 
      IF ( X .LT. ZERO ) THEN
CCCCC    CALL ERRPRN(FNNAME,ERRMSG)
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,101)X
         CALL DPWRST('XXX','BUG ')
         DEBYE3 = ZERO
         RETURN
      ENDIF
  999 FORMAT(1X)
  101 FORMAT('***** ERROR FROM DEBYE3--ARGUMENT MUST BE ',
     1       'NON-NEGATIVE, ARGUMENT = ',G15.7)
C
C   Compute the machine-dependent constants.
C
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
         IF ( ABS(ADEB3(NTERMS)) .GT. T ) GOTO 19
 10   CONTINUE
C
C   Code for x <= 4.0
C 
 19   IF ( X .LE. FOUR ) THEN
         IF ( X .LT. XLOW ) THEN
            DEBYE3 = ( ( X - SEVP5 ) * X + TWENTY ) / TWENTY
         ELSE
            T = ( ( X * X / EIGHT ) - HALF ) - HALF
            DEBYE3 = CHEVAL ( NTERMS , ADEB3 , T ) - PT375 * X
         ENDIF
      ELSE
C
C   Code for x > 4.0
C 
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
      END
      DOUBLE PRECISION FUNCTION DEBYE4(XVALUE)
C
C
C   DEFINITION:
C
C      This program calculates the Debye function of order 4, defined as
C
C            DEBYE4(x) = 4*[Integral {0 to x} t^4/(exp(t)-1) dt] / (x^4)
C
C      The code uses Chebyshev series whose coefficients
C      are given to 20 decimal places.
C
C
C   ERROR RETURNS:
C
C      If XVALUE < 0.0 an error message is printed and the
C      function returns the value 0.0
C
C
C   MACHINE-DEPENDENT PARAMETERS:
C
C      NTERMS - INTEGER - The no. of elements of the array ADEB4.
C                         The recommended value is such that
C                             ABS(ADEB4(NTERMS)) < EPS/100,
C                         subject to 1 <= NTERMS <= 18
C
C      XLOW - DOUBLE PRECISION - The value below which
C                    DEBYE4 = 1 - 4x/10 + x*x/18 to machine precision.
C                    The recommended value is 
C                        SQRT(8*EPSNEG)
C
C      XUPPER - DOUBLE PRECISION - The value above which 
C               DEBYE4=(96*zeta(5)/x^4)-4*exp(-x)(x^4+4x^2+12x^2+24x+24)/x^4.
C                      The recommended value is
C                          -LOG(2*EPS)
C
C      XLIM1 - DOUBLE PRECISION - The value above which DEBYE4 = 96*zeta(5)/x^4
C                     The recommended value is 
C                          -LOG(XMIN)
C
C      XLIM2 - DOUBLE PRECISION - The value above which DEBYE4 = 0.0 to machine
C                     precision. The recommended value is
C                          FOURTH ROOT(99/XMIN)
C
C      For values of EPS, EPSNEG, and XMIN see the file MACHCON.TXT
C
C
C      The machine-dependent constants are computed internally by
C      using the D1MACH subroutine.
C
C
C   INTRINSIC FUNCTIONS USED:
C
C      AINT , EXP , INT , LOG , SQRT
C      
C
C   OTHER MISCFUN SUBROUTINES USED:
C
C          CHEVAL , ERRPRN, D1MACH
C
C
C   AUTHOR:
C          Dr. Allan J. MacLeod,
C          Dept. of Mathematics and Statistics,
C          University of Paisley
C          High St.
C          PAISLEY
C          SCOTLAND
C          PA1 2BE
C
C          (e-mail:  macl_ms0@paisley.ac.uk )
C
C
C   LATEST UPDATE:  23 January, 1996
C
      INTEGER I,NEXP,NTERMS
      DOUBLE PRECISION ADEB4(0:18),CHEVAL,DEBINF,EIGHT,EIGHTN,EXPMX,
     1     FIVE,FOUR,FORTY5,HALF,ONE,ONEHUN,RK,SUM,T,TWELVE,TWENT4,
     2     TWOPT5,X,XK,XKI,XLIM1,XLIM2,XLOW,XUPPER,XVALUE,ZERO
CCCCC CHARACTER FNNAME*6,ERRMSG*17
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
CCCCC DATA FNNAME/'DEBYE4'/
CCCCC DATA ERRMSG/'ARGUMENT NEGATIVE'/
      DATA ZERO,HALF,ONE/0.0 D 0 , 0.5 D 0 , 1.0 D 0/
      DATA TWOPT5,FOUR,FIVE/2.5 D 0 , 4.0 D 0 , 5.0 D 0/
      DATA EIGHT,TWELVE,EIGHTN/8.0 D 0 , 12.0 D 0 , 18.0 D 0/
      DATA TWENT4,FORTY5,ONEHUN/24.0 D 0 , 45.0 D 0 , 100.0 D 0/
      DATA DEBINF/99.54506 44937 63512 92781 D 0/
      DATA ADEB4/2.78186 94150 20523 46008  D    0,
     1           0.37497 67835 26892 86364  D    0,
     2          -0.14940 90739 90315 8326   D   -1,
     3           0.94567 98114 37042 74     D   -3,
     4          -0.66132 91613 89325 5      D   -4,
     5           0.48156 32982 14449        D   -5,
     6          -0.35880 83958 7593         D   -6,
     7           0.27160 11874 160          D   -7,
     8          -0.20807 09912 23           D   -8,
     9           0.16093 83869 2            D   -9,
     X          -0.12547 09791              D  -10,
     1           0.98472 647                D  -12,
     2          -0.77723 69                 D  -13,
     3           0.61648 3                  D  -14,
     4          -0.49107                    D  -15,
     5           0.3927                     D  -16,
     6          -0.315                      D  -17,
     7           0.25                       D  -18,
     8          -0.2                        D  -19/
C
C   Start computation
C
      X = XVALUE
C
C   Check XVALUE >= 0.0
C 
      IF ( X .LT. ZERO ) THEN
CCCCC    CALL ERRPRN(FNNAME,ERRMSG)
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,101)X
         CALL DPWRST('XXX','BUG ')
         DEBYE4 = ZERO
         RETURN
      ENDIF
  999 FORMAT(1X)
  101 FORMAT('***** ERROR FROM DEBYE4--ARGUMENT MUST BE ',
     1       'NON-NEGATIVE, ARGUMENT = ',G15.7)
C
C   Compute the machine-dependent constants.
C
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
         IF ( ABS(ADEB4(NTERMS)) .GT. T ) GOTO 19
 10   CONTINUE
C
C   Code for x <= 4.0
C 
 19   IF ( X .LE. FOUR ) THEN
         IF ( X .LT. XLOW ) THEN
            DEBYE4 = ( ( TWOPT5 * X - EIGHTN ) * X + FORTY5 ) / FORTY5
         ELSE
            T = ( ( X * X / EIGHT ) - HALF ) - HALF
            DEBYE4 = CHEVAL ( NTERMS , ADEB4 , T ) - ( X + X ) / FIVE
         ENDIF
      ELSE
C
C   Code for x > 4.0
C 
         IF ( X .GT. XLIM2 ) THEN
            DEBYE4 = ZERO
         ELSE
            T = X * X
            DEBYE4 = ( DEBINF / T ) / T
            IF ( X .LT. XLIM1 ) THEN
               EXPMX = EXP ( -X )
               IF ( X .GT. XUPPER ) THEN
                  SUM = ( ( ( ( X + FOUR ) * X + TWELVE ) * X +
     &                  TWENT4 ) * X + TWENT4 ) / ( X * X * X * X )
               ELSE
                  SUM = ZERO
                  RK = AINT ( XLIM1 / X )
                  NEXP = INT ( RK )
                  XK = RK * X
                  DO 100 I = NEXP,1,-1
                     XKI = ONE / XK
                     T =  ( ( ( ( TWENT4 * XKI + TWENT4 ) * XKI +
     &                    TWELVE ) * XKI + FOUR ) * XKI + ONE ) / RK
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
      END
      SUBROUTINE DCHEX(R,LDR,P,K,L,Z,LDZ,NZ,C,S,JOB)
C***BEGIN PROLOGUE  DCHEX
C***DATE WRITTEN   780814   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***REVISION HISTORY  (YYMMDD)
C   000330  Modified array declarations.  (JEC)
C***CATEGORY NO.  D7B
C***KEYWORDS  CHOLESKY DECOMPOSITION,DOUBLE PRECISION,EXCHANGE,
C             LINEAR ALGEBRA,LINPACK,MATRIX,POSITIVE DEFINITE
C***AUTHOR  STEWART, G. W., (U. OF MARYLAND)
C***PURPOSE  Updates the Cholesky factorization  A=TRANS(R)*R  of a
C            POSITIVE DEFINITE matrix A of order P under diagonal
C            permutations of the form  TRANS(E)*A*E  where E is a
C            permutation matrix.
C***DESCRIPTION
C
C     DCHEX updates the Cholesky factorization
C
C                   A = TRANS(R)*R
C
C     of a positive definite matrix A of order P under diagonal
C     permutations of the form
C
C                   TRANS(E)*A*E
C
C     where E is a permutation matrix.  Specifically, given
C     an upper triangular matrix R and a permutation matrix
C     E (which is specified by K, L, and JOB), DCHEX determines
C     an orthogonal matrix U such that
C
C                           U*R*E = RR,
C
C     where RR is upper triangular.  At the users option, the
C     transformation U will be multiplied into the array Z.
C     If A = TRANS(X)*X, so that R is the triangular part of the
C     QR factorization of X, then RR is the triangular part of the
C     QR factorization of X*E, i.e. X with its columns permuted.
C     For a less terse description of what DCHEX does and how
C     it may be applied, see the LINPACK guide.
C
C     The matrix Q is determined as the product U(L-K)*...*U(1)
C     of plane rotations of the form
C
C                           (    C(I)       S(I) )
C                           (                    ) ,
C                           (    -S(I)      C(I) )
C
C     where C(I) is double precision.  The rows these rotations operate
C     on are described below.
C
C     There are two types of permutations, which are determined
C     by the value of JOB.
C
C     1. Right circular shift (JOB = 1).
C
C         The columns are rearranged in the following order.
C
C                1,...,K-1,L,K,K+1,...,L-1,L+1,...,P.
C
C         U is the product of L-K rotations U(I), where U(I)
C         acts in the (L-I,L-I+1)-plane.
C
C     2. Left circular shift (JOB = 2).
C         The columns are rearranged in the following order
C
C                1,...,K-1,K+1,K+2,...,L,K,L+1,...,P.
C
C         U is the product of L-K rotations U(I), where U(I)
C         acts in the (K+I-1,K+I)-plane.
C
C     On Entry
C
C         R      DOUBLE PRECISION(LDR,P), where LDR .GE. P.
C                R contains the upper triangular factor
C                that is to be updated.  Elements of R
C                below the diagonal are not referenced.
C
C         LDR    INTEGER.
C                LDR is the leading dimension of the array R.
C
C         P      INTEGER.
C                P is the order of the matrix R.
C
C         K      INTEGER.
C                K is the first column to be permuted.
C
C         L      INTEGER.
C                L is the last column to be permuted.
C                L must be strictly greater than K.
C
C         Z      DOUBLE PRECISION(LDZ,N)Z), where LDZ .GE. P.
C                Z is an array of NZ P-vectors into which the
C                transformation U is multiplied.  Z is
C                not referenced if NZ = 0.
C
C         LDZ    INTEGER.
C                LDZ is the leading dimension of the array Z.
C
C         NZ     INTEGER.
C                NZ is the number of columns of the matrix Z.
C
C         JOB    INTEGER.
C                JOB determines the type of permutation.
C                       JOB = 1  right circular shift.
C                       JOB = 2  left circular shift.
C
C     On Return
C
C         R      contains the updated factor.
C
C         Z      contains the updated matrix Z.
C
C         C      DOUBLE PRECISION(P).
C                C contains the cosines of the transforming rotations.
C
C         S      DOUBLE PRECISION(P).
C                S contains the sines of the transforming rotations.
C
C     LINPACK.  This version dated 08/14/78 .
C     G. W. Stewart, University of Maryland, Argonne National Lab.
C
C     DCHEX uses the following functions and subroutines.
C
C     BLAS DROTG
C     Fortran MIN0
C***REFERENCES  DONGARRA J.J., BUNCH J.R., MOLER C.B., STEWART G.W.,
C                 *LINPACK USERS  GUIDE*, SIAM, 1979.
C***ROUTINES CALLED  DROTG
C***END PROLOGUE  DCHEX
      INTEGER LDR,P,K,L,LDZ,NZ,JOB
      DOUBLE PRECISION R(LDR,*),Z(LDZ,*),S(*)
      DOUBLE PRECISION C(*)
C
      INTEGER I,II,IL,IU,J,JJ,KM1,KP1,LMK,LM1
CCCCC DOUBLE PRECISION RJP1J,T
      DOUBLE PRECISION T
C
C     INITIALIZE
C
C***FIRST EXECUTABLE STATEMENT  DCHEX
      KM1 = K - 1
      KP1 = K + 1
      LMK = L - K
      LM1 = L - 1
C
C     PERFORM THE APPROPRIATE TASK.
C
      GO TO (10,130), JOB
C
C     RIGHT CIRCULAR SHIFT.
C
   10 CONTINUE
C
C        REORDER THE COLUMNS.
C
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
C
C        CALCULATE THE ROTATIONS.
C
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
C
C        IF REQUIRED, APPLY THE TRANSFORMATIONS TO Z.
C
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
C
C     LEFT CIRCULAR SHIFT
C
  130 CONTINUE
C
C        REORDER THE COLUMNS
C
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
C
C        REDUCTION LOOP.
C
         DO 220 J = K, P
            IF (J .EQ. K) GO TO 200
C
C              APPLY THE ROTATIONS.
C
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
C
C        APPLY THE ROTATIONS TO Z.
C
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
      END
      DOUBLE PRECISION FUNCTION DCHU (A, B, X)
C***BEGIN PROLOGUE  DCHU
C***PURPOSE  Compute the logarithmic confluent hypergeometric function.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C11
C***TYPE      DOUBLE PRECISION (CHU-S, DCHU-D)
C***KEYWORDS  FNLIB, LOGARITHMIC CONFLUENT HYPERGEOMETRIC FUNCTION,
C             SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C DCHU(A,B,X) calculates the double precision logarithmic confluent
C hypergeometric function U(A,B,X) for double precision arguments
C A, B, and X.
C
C This routine is not valid when 1+A-B is close to zero if X is small.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, D9CHU, DEXPRL, DGAMMA, DGAMR, DPOCH,
C                    DPOCH1, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770801  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900727  Added EXTERNAL statement.  (WRB)
C***END PROLOGUE  DCHU
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
      DOUBLE PRECISION A, B, X, AINTB, ALNX, A0, BEPS, B0, C0, EPS,
     1  FACTOR, GAMRI1, GAMRNI, PCH1AI, PCH1I, PI, POCHAI, SUM, T,
     2  XEPS1, XI, XI1, XN, XTOEPS,  DPOCH, DGAMMA, DGAMR,
     3  DPOCH1, DEXPRL, D9CHU
      EXTERNAL DGAMMA
      SAVE PI, EPS
      DATA PI / 3.1415926535 8979323846 2643383279 503 D0 /
      DATA EPS / 0.0D0 /
C***FIRST EXECUTABLE STATEMENT  DCHU
C
      DCHU = 0.0D0
C
      IF (EPS.EQ.0.0D0) EPS = D1MACH(3)
C
      IF (X .EQ. 0.0D0) THEN
        WRITE(ICOUT,2)
    2   FORMAT('***** ERORR FROM DCHU, X IS ZERO, SO CHU IS ',
     1         'INFINITE.  *******')
        CALL DPWRST('XXX','BUG ')
        RETURN
      ENDIF
      IF (X .LT. 0.0D0) THEN
        WRITE(ICOUT,1)
    1   FORMAT('***** ERORR FROM DCHU, X IS NEGATIVE.  *******')
        CALL DPWRST('XXX','BUG ')
        RETURN
      ENDIF
C
      IF (MAX(ABS(A),1.0D0)*MAX(ABS(1.0D0+A-B),1.0D0).LT.
     1  0.99D0*ABS(X)) GO TO 120
C
C THE ASCENDING SERIES WILL BE USED, BECAUSE THE DESCENDING RATIONAL
C APPROXIMATION (WHICH IS BASED ON THE ASYMPTOTIC SERIES) IS UNSTABLE.
C
      IF (ABS(1.0D0+A-B) .LT. SQRT(EPS)) THEN
        WRITE(ICOUT,3)
    3   FORMAT('***** ERORR FROM DCHU, ALGORITHM IS BAD WHEN 1+A-B ',
     1         'IS NEAR ZERO FOR SMALL X. *****')
        CALL DPWRST('XXX','BUG ')
        RETURN
      ENDIF
C
      AINTB=0.0
      IF (B.GE.0.0D0) AINTB = AINT(B+0.5D0)
      IF (B.LT.0.0D0) AINTB = AINT(B-0.5D0)
      BEPS = B - AINTB
      N = INT(AINTB)
C
      ALNX = LOG(X)
      XTOEPS = EXP (-BEPS*ALNX)
C
C EVALUATE THE FINITE SUM.     -----------------------------------------
C
      IF (N.GE.1) GO TO 40
C
C CONSIDER THE CASE B .LT. 1.0 FIRST.
C
      SUM = 1.0D0
      IF (N.EQ.0) GO TO 30
C
      T = 1.0D0
      M = -N
      DO 20 I=1,M
        XI1 = I - 1
        T = T*(A+XI1)*X/((B+XI1)*(XI1+1.0D0))
        SUM = SUM + T
 20   CONTINUE
C
 30   SUM = DPOCH(1.0D0+A-B, -A)*SUM
      GO TO 70
C
C NOW CONSIDER THE CASE B .GE. 1.0.
C
 40   SUM = 0.0D0
      M = N - 2
      IF (M.LT.0) GO TO 70
      T = 1.0D0
      SUM = 1.0D0
      IF (M.EQ.0) GO TO 60
C
      DO 50 I=1,M
        XI = I
        T = T * (A-B+XI)*X/((1.0D0-B+XI)*XI)
        SUM = SUM + T
 50   CONTINUE
C
 60   SUM = DGAMMA(B-1.0D0) * DGAMR(A) * X**(1-N) * XTOEPS * SUM
C
C NEXT EVALUATE THE INFINITE SUM.     ----------------------------------
C
 70   ISTRT = 0
      IF (N.LT.1) ISTRT = 1 - N
      XI = ISTRT
C
      FACTOR = (-1.0D0)**N * DGAMR(1.0D0+A-B) * X**ISTRT
      IF (BEPS.NE.0.0D0) FACTOR = FACTOR * BEPS*PI/SIN(BEPS*PI)
C
      POCHAI = DPOCH (A, XI)
      GAMRI1 = DGAMR (XI+1.0D0)
      GAMRNI = DGAMR (AINTB+XI)
      B0 = FACTOR * DPOCH(A,XI-BEPS) * GAMRNI * DGAMR(XI+1.0D0-BEPS)
C
      IF (ABS(XTOEPS-1.0D0).GT.0.5D0) GO TO 90
C
C X**(-BEPS) IS CLOSE TO 1.0D0, SO WE MUST BE CAREFUL IN EVALUATING THE
C DIFFERENCES.
C
      PCH1AI = DPOCH1 (A+XI, -BEPS)
      PCH1I = DPOCH1 (XI+1.0D0-BEPS, BEPS)
      C0 = FACTOR * POCHAI * GAMRNI * GAMRI1 * (
     1  -DPOCH1(B+XI,-BEPS) + PCH1AI - PCH1I + BEPS*PCH1AI*PCH1I)
C
C XEPS1 = (1.0 - X**(-BEPS))/BEPS = (X**(-BEPS) - 1.0)/(-BEPS)
      XEPS1 = ALNX*DEXPRL(-BEPS*ALNX)
C
      DCHU = SUM + C0 + XEPS1*B0
      XN = N
      DO 80 I=1,1000
        XI = ISTRT + I
        XI1 = ISTRT + I - 1
        B0 = (A+XI1-BEPS)*B0*X/((XN+XI1)*(XI-BEPS))
        C0 = (A+XI1)*C0*X/((B+XI1)*XI)
     1    - ((A-1.0D0)*(XN+2.D0*XI-1.0D0) + XI*(XI-BEPS)) * B0
     2    / (XI*(B+XI1)*(A+XI1-BEPS))
        T = C0 + XEPS1*B0
        DCHU = DCHU + T
        IF (ABS(T).LT.EPS*ABS(DCHU)) GO TO 130
 80   CONTINUE
      WRITE(ICOUT,4)
    4 FORMAT('***** ERORR FROM DCHU, NO CONVERGENCE IN 1000 TERMS OF ',
     1         'THE ASCENDING SERIES. *****')
      CALL DPWRST('XXX','BUG ')
      RETURN
C
C X**(-BEPS) IS VERY DIFFERENT FROM 1.0, SO THE STRAIGHTFORWARD
C FORMULATION IS STABLE.
C
 90   A0 = FACTOR * POCHAI * DGAMR(B+XI) * GAMRI1 / BEPS
      B0 = XTOEPS * B0 / BEPS
C
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
C
C USE LUKE-S RATIONAL APPROXIMATION IN THE ASYMPTOTIC REGION.
C
 120  DCHU = X**(-A) * D9CHU(A,B,X)
C
 130  RETURN
      END
      SUBROUTINE DCOPY(N,DX,INCX,DY,INCY)
C
C     COPIES A VECTOR, X, TO A VECTOR, Y.
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
      DOUBLE PRECISION DX(1),DY(1)
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N
C
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
C          NOT EQUAL TO 1
C
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
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C
C        CLEAN-UP LOOP
C
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
      END
      DOUBLE PRECISION FUNCTION DCOT (X)
C***BEGIN PROLOGUE  DCOT
C***PURPOSE  Compute the cotangent.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C4A
C***TYPE      DOUBLE PRECISION (COT-S, DCOT-D, CCOT-C)
C***KEYWORDS  COTANGENT, ELEMENTARY FUNCTIONS, FNLIB, TRIGONOMETRIC
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C DCOT(X) calculates the double precision trigonometric cotangent
C for double precision argument X.  X is in units of radians.
C
C Series for COT        on the interval  0.          to  6.25000E-02
C                                        with weighted error   5.52E-34
C                                         log weighted error  33.26
C                               significant figures required  32.34
C                                    decimal places required  33.85
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, DCSEVL, INITDS, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   920618  Removed space from variable names.  (RWC, WRB)
C***END PROLOGUE  DCOT
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
      DOUBLE PRECISION X, COTCS(15), AINTY, AINTY2, PI2REC, SQEPS,
     1  XMAX, XMIN, XSML, Y, YREM, PRODBG, DCSEVL
      LOGICAL FIRST
      SAVE COTCS, PI2REC, NTERMS, XMAX, XSML, XMIN, SQEPS, FIRST
      DATA COTCS(  1) / +.2402591609 8295630250 9553617744 970 D+0    /
      DATA COTCS(  2) / -.1653303160 1500227845 4746025255 758 D-1    /
      DATA COTCS(  3) / -.4299839193 1724018935 6476228239 895 D-4    /
      DATA COTCS(  4) / -.1592832233 2754104602 3490851122 445 D-6    /
      DATA COTCS(  5) / -.6191093135 1293487258 8620579343 187 D-9    /
      DATA COTCS(  6) / -.2430197415 0726460433 1702590579 575 D-11   /
      DATA COTCS(  7) / -.9560936758 8000809842 7062083100 000 D-14   /
      DATA COTCS(  8) / -.3763537981 9458058041 6291539706 666 D-16   /
      DATA COTCS(  9) / -.1481665746 4674657885 2176794666 666 D-18   /
      DATA COTCS( 10) / -.5833356589 0366657947 7984000000 000 D-21   /
      DATA COTCS( 11) / -.2296626469 6464577392 8533333333 333 D-23   /
      DATA COTCS( 12) / -.9041970573 0748332671 9999999999 999 D-26   /
      DATA COTCS( 13) / -.3559885519 2060006400 0000000000 000 D-28   /
      DATA COTCS( 14) / -.1401551398 2429866666 6666666666 666 D-30   /
      DATA COTCS( 15) / -.5518004368 7253333333 3333333333 333 D-33   /
      DATA PI2REC / .01161977236 7581343075 5350534900 57 D0 /
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  DCOT
C
      DCOT=DBLE(CPUMIN)
C
      IF (FIRST) THEN
         NTERMS = INITDS (COTCS, 15, 0.1*REAL(D1MACH(3)) )
         XMAX = 1.0D0/D1MACH(4)
         XSML = SQRT(3.0D0*D1MACH(3))
         XMIN = EXP (MAX(LOG(D1MACH(1)), -LOG(D1MACH(2))) + 0.01D0)
         SQEPS = SQRT(D1MACH(4))
      ENDIF
      FIRST = .FALSE.
C
      Y = ABS(X)
      IF (Y .LT. XMIN) THEN
        WRITE(ICOUT,1)
    1   FORMAT('***** ERORR FROM DCOT, ABS(X) IS ZERO OR SO SMALL ',
     1         'THAT DCOT OVERFLOWS.  ****')
        CALL DPWRST('XXX','BUG ')
        RETURN
      ENDIF
      IF (Y .GT. XMAX) THEN
        WRITE(ICOUT,2)
    2   FORMAT('***** ERORR FROM DCOT, NO PRECISION BECAUSE ABS(X) ',
     1         'IS SO BIG.  ****')
        CALL DPWRST('XXX','BUG ')
        RETURN
      ENDIF
C
C CAREFULLY COMPUTE Y * (2/PI) = (AINT(Y) + REM(Y)) * (.625 + PI2REC)
C = AINT(.625*Y) + REM(.625*Y) + Y*PI2REC  =  AINT(.625*Y) + Z
C = AINT(.625*Y) + AINT(Z) + REM(Z)
C
      AINTY = AINT (Y)
      YREM = Y - AINTY
      PRODBG = 0.625D0*AINTY
      AINTY = AINT (PRODBG)
      Y = (PRODBG-AINTY) + 0.625D0*YREM + PI2REC*Y
      AINTY2 = AINT (Y)
      AINTY = AINTY + AINTY2
      Y = Y - AINTY2
C
      IFN = INT(MOD (AINTY, 2.0D0))
      IF (IFN.EQ.1) Y = 1.0D0 - Y
C
      IF (ABS(X) .GT. 0.5D0 .AND. Y .LT. ABS(X)*SQEPS) THEN
        WRITE(ICOUT,3)
    3   FORMAT('***** WARNING FROM DCOT, ANSWER IS LESS THAN HALF ',
     1   'PRECISION BECAUSE ABS(X) IS TOO BIG OR X IS NEAR PI.')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF (Y.GT.0.25D0) GO TO 20
      DCOT = 1.0D0/X
      IF (Y.GT.XSML) DCOT = (0.5D0 + DCSEVL (32.0D0*Y*Y-1.D0, COTCS,
     1  NTERMS)) / Y
      GO TO 40
C
 20   IF (Y.GT.0.5D0) GO TO 30
      DCOT = (0.5D0 + DCSEVL (8.D0*Y*Y-1.D0, COTCS, NTERMS))/(0.5D0*Y)
      DCOT = (DCOT*DCOT-1.D0)*0.5D0/DCOT
      GO TO 40
C
 30   DCOT = (0.5D0 + DCSEVL (2.D0*Y*Y-1.D0, COTCS, NTERMS))/(.25D0*Y)
      DCOT = (DCOT*DCOT-1.D0)*0.5D0/DCOT
      DCOT = (DCOT*DCOT-1.D0)*0.5D0/DCOT
C
 40   IF (X.NE.0.D0) DCOT = SIGN (DCOT, X)
      IF (IFN.EQ.1) DCOT = -DCOT
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION DCSEVL (X, CS, N)
C***BEGIN PROLOGUE  DCSEVL
C***PURPOSE  Evaluate a Chebyshev series.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C3A2
C***TYPE      DOUBLE PRECISION (CSEVL-S, DCSEVL-D)
C***KEYWORDS  CHEBYSHEV SERIES, FNLIB, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C  Evaluate the N-term Chebyshev series CS at X.  Adapted from
C  a method presented in the paper by Broucke referenced below.
C
C       Input Arguments --
C  X    value at which the series is to be evaluated.
C  CS   array of N terms of a Chebyshev series.  In evaluating
C       CS, only half the first coefficient is summed.
C  N    number of terms in array CS.
C
C***REFERENCES  R. Broucke, Ten subroutines for the manipulation of
C                 Chebyshev series, Algorithm 446, Communications of
C                 the A.C.M. 16, (1973) pp. 254-256.
C               L. Fox and I. B. Parker, Chebyshev Polynomials in
C                 Numerical Analysis, Oxford University Press, 1968,
C                 page 56.
C***ROUTINES CALLED  D1MACH, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770401  DATE WRITTEN
C   890831  Modified array declarations.  (WRB)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900329  Prologued revised extensively and code rewritten to allow
C           X to be slightly outside interval (-1,+1).  (WRB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  DCSEVL
      DOUBLE PRECISION B0, B1, B2, CS(*), ONEPL, TWOX, X
      LOGICAL FIRST
      SAVE FIRST, ONEPL
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  DCSEVL
      IF (FIRST) ONEPL = 1.0D0 + D1MACH(4)
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
      IF (ABS(X) .GT. ONEPL) THEN
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,32)
        CALL DPWRST('XXX','BUG ')
      ENDIF
   31 FORMAT('***** WARNING FROM DCSEVL.  X IS OUTSIDE THE ')
   32 FORMAT('      INTERVAL (-1,+1).                          *****')
C
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
C
      DCSEVL = 0.5D0*(B0-B2)
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION DDOT(N,DX,INCX,DY,INCY)
C***BEGIN PROLOGUE  DDOT
C***DATE WRITTEN   791001   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  D1A4
C***KEYWORDS  BLAS,DOUBLE PRECISION,INNER PRODUCT,LINEAR ALGEBRA,VECTOR
C***AUTHOR  LAWSON, C. L., (JPL)
C           HANSON, R. J., (SNLA)
C           KINCAID, D. R., (U. OF TEXAS)
C           KROGH, F. T., (JPL)
C***PURPOSE  D.P. inner product of d.p. vectors
C***DESCRIPTION
C
C                B L A S  Subprogram
C    Description of Parameters
C
C     --Input--
C        N  number of elements in input vector(s)
C       DX  double precision vector with N elements
C     INCX  storage spacing between elements of DX
C       DY  double precision vector with N elements
C     INCY  storage spacing between elements of DY
C
C     --Output--
C     DDOT  double precision dot product (zero if N .LE. 0)
C
C     Returns the dot product of double precision DX and DY.
C     DDOT = sum for I = 0 to N-1 of  DX(LX+I*INCX) * DY(LY+I*INCY)
C     where LX = 1 if INCX .GE. 0, else LX = (-INCX)*N, and LY is
C     defined in a similar way using INCY.
C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,
C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*,
C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL
C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  DDOT
C
      DOUBLE PRECISION DX(*),DY(*)
C***FIRST EXECUTABLE STATEMENT  DDOT
      DDOT = 0.D0
      IF(N.LE.0)RETURN
CCCCC IF(INCX.EQ.INCY) IF(INCX-1) 5,20,60
      IF(INCX.EQ.INCY) THEN
        IF(INCX-1.LT.0)THEN
          GOTO5
        ELSEIF(INCX-1.EQ.0)THEN
          GOTO20
        ELSE
          GOTO60
        ENDIF
      ENDIF
    5 CONTINUE
C
C         CODE FOR UNEQUAL OR NONPOSITIVE INCREMENTS.
C
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
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1.
C
C
C        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 5.
C
   20 M = MOD(N,5)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
         DDOT = DDOT + DX(I)*DY(I)
   30 CONTINUE
      IF( N .LT. 5 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
         DDOT = DDOT + DX(I)*DY(I) + DX(I+1)*DY(I+1) +
     1   DX(I + 2)*DY(I + 2) + DX(I + 3)*DY(I + 3) + DX(I + 4)*DY(I + 4)
   50 CONTINUE
      RETURN
C
C         CODE FOR POSITIVE EQUAL INCREMENTS .NE.1.
C
   60 CONTINUE
      NS = N*INCX
          DO 70 I=1,NS,INCX
          DDOT = DDOT + DX(I)*DY(I)
   70     CONTINUE
      RETURN
      END
      SUBROUTINE DECHE2(IX,IA,IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE CONVERTS AN INTEGER IN THE
C              RANGE 0 - 65535 (2**16 - 1) TO A TWO CHARACTER
C              HEXADECIMAL NUMBER.
C
C              THIS IS A UTILITY ROUTINE USED BY SOME DEVICES
C              (E.G., POSTSCRIPT) TO CONVERT RGB COMPONENTS TO
C              HEXADECIMAL NUMBERS.
C     INPUT  ARGUMENTS--IX     = THE INTEGER TO BE CONVERTED.
C     OUTPUT ARGUMENTS--IA     = THE CHARACTER*2 STRING THAT WILL
C                                CONTAIN THE HEX NUMBER.
C     OUTPUT--THE STRING CONTAINING THE NUMBER IN HEXADECIMAL FORMAT.
C     RESTRICTIONS--THE MAXIMUM VALUE OF IX IS 2**16-1.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--INTEGER.
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
C     VERSION NUMBER--2008.3
C     ORIGINAL VERSION--MARCH     2008.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      CHARACTER*2 IA
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DECH'
      ISUBN2='E2  '
      IERROR='NO'
C
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
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      IMAX=(2**16) - 1
      IA=' '
C
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
        GOTO9000
      ENDIF
C
C               ******************************
C               **  STEP 2--                **
C               **  PERFORM THE CONVERSION. **
C               ******************************
C
      IVAL=IX/16
      IREM=IX - (16*IVAL)
C
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
C
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
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
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
C
      RETURN
      END
      SUBROUTINE DECRAT(X,N,IWRITE,XQNUM,XQDEN,
     1                  RATIO,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--IF XQNUM = 0.9 AND XQDEN = 0.4, THIS STATISTIC
C              COMPUTES THE RATIO OF THE TOP 10% OF THE DATA
C              TO THE BOTTOM 40% OF THE DATA.
C
C              THIS HAS BEEN PROPOSED AS AN ALTERNATIVE MEASURE OF
C              "INCOME EQUALITY".  SPECIFICALLY, THE PALMA SPECIFICATION
C              USES QNUM = 0.9 AND QDEN = 0.4).  THAT IS, THIS IS THE
C              RATIO OF THE INCOME OF THE BOTTOM 40% RELATIVE TO THE
C              TOP 10%.
C
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                OBSERVATIONS FOR WHICH THE PERCENTAGE
C                                RANKS WILL BE COMPUTED.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C                     --XQNUM  = SCALAR THAT SPECIFIES QUANTILE FOR THE
C                                NUMERATOR
C                     --XQDEN  = SCALAR THAT SPECIFIES QUANTILE FOR THE
C                                DENOMINATOR
C     OUTPUT ARGUMENTS--RATIO  = THE SINGLE PRECISION SCALAR WHERE THE
C                                INTERDECILE RATIO IS SAVED
C     OUTPUT--THE SINGLE PRECISION SCALAR RATIO CONTAINING THE
C             INTERDECILE RATIO.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     OTHER DATAPAC   SUBROUTINES NEEDED--QUANT, SORT.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--COBHAM AND SUMNER (2014), "IS INEQUALITY ALL ABOUT THE
C                 TAILS", SIGNIFICANCE, PP. 10-13.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATION INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2015.2
C     ORIGINAL VERSION--FEBRUARY  2015.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
C
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
      DOUBLE PRECISION DRATIO
      DOUBLE PRECISION DDEN
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DECR'
      ISUBN2='AT  '
C
      IERROR='NO'
      RATIO=CPUMIN
C
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
        DO55I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               *******************************************
C               **  COMPUTE THE INTERDECILE RATIO        **
C               *******************************************
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      AN=N
C
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
        GOTO9000
      ELSEIF(XQNUM.LT.0.0 .OR. XQNUM.GT.1.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,123)
  123   FORMAT('      THE SPECIFIED QUANTILE FOR THE NUMERATOR IS ',
     1         'OUTSIDE THE (0,1) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,125)XQNUM
  125   FORMAT('      THE VALUE OF THE NUMERATOR QUANTILE = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ELSEIF(XQDEN.LT.0.0 .OR. XQDEN.GT.1.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,133)
  133   FORMAT('      THE SPECIFIED QUANTILE FOR THE DENOMINATOR IS ',
     1         'OUTSIDE THE (0,1) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,135)XQDEN
  135   FORMAT('      THE VALUE OF THE DENOMINATOR QUANTILE = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ELSEIF(N.EQ.1)THEN
        RATIO=1.0
        GOTO8000
      ENDIF
C
      IF(XQDEN.GT.XQNUM)THEN
        AVAL=XQNUM
        XQNUM=XQDEN
        XQDEN=AVAL
      ENDIF
C
C               ***************************************************
C               **  STEP 2--                                     **
C               **  SORT THE DATA.                               **
C               ***************************************************
C
      CALL SORT(X,N,X)
C
      IF(X(1).LT.0.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,203)
  203   FORMAT('      THE RESPONSE VARIABLE CONTAINS NEGATIVE ',
     1         'NUMBERS AND THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,205)
  205   FORMAT('      DECILE RATIO IS NOT CURRENTLY SUPPORTED FOR ',
     1         'NEGATIVE NUMBERS.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               ***************************************************
C               **  STEP 3--                                     **
C               **  COMPUTE THE QUANTILES FOR THE NUMERATOR AND  **
C               **  DENOMINATOR.                                 **
C               ***************************************************
C
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
      DO310I=1,NI
        DSUM1=DSUM1 + DBLE(X(I))
  310 CONTINUE
      DSUM1=DSUM1 + DBLE(REM*X(NIP1))
      NIDEN=NI
      NIP1DN=NIP1
      REMDEN=REM
C
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
      DO320I=NI,N
        DSUM2=DSUM2 + DBLE(X(I))
  320 CONTINUE
      DSUM2=DSUM2 - DBLE(REM*X(NIP1))
      NINUM=NI
      NIP1NU=NIP1
      REMNUM=REM
C
      DRATIO=DSUM2/DSUM1
      RATIO=REAL(DRATIO)
C
C               ******************************
C               **  STEP 4--                **
C               **  WRITE OUT A FEW LINES   **
C               **  OF SUMMARY INFORMATION  **
C               **  ABOUT THE CODING.       **
C               ******************************
C
 8000 CONTINUE
C
      IF(IFEEDB.EQ.'ON' .AND. IWRITE.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,912)RATIO
  912   FORMAT('THE INTERDECILE RATIO IS ',G15.7)
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
C
      RETURN
      END
      SUBROUTINE DENEST(DT, NDT, DLO, DHI, WINDOW, FT, SMOOTH,
     *                  NFT, ICAL, IERROR)
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DOUBLE PRECISION DT(NDT), FT(NFT), SMOOTH(NFT)
C
      CHARACTER*4 IERROR
C
      INCLUDE 'DPCOP2.INC'
C
C     ALGORITHM AS 176  APPL. STATIST. (1982) VOL.31, NO.1
C     Modified using AS R50 (Appl. Statist. (1984))
C
C     Find density estimate by kernel method using Gaussian kernel.
C     The interval on which the estimate is evaluated has end points
C     DLO and DHI.   If ICAL is not zero then it is assumed that the
C     routine has been called before with the same data and end points
C     and that the array FT has not been altered.
C
C     Auxiliary routines called: FORRT & REVRT from AS 97
C
C     NOTE: MODIFIED JULY 2001 FOR INCLUSION INTO DATAPLOT:
C           1) MAKE DOUBLE PRECISION
C           2) ADD SOME DATAPLOT I/O, ERROR FLAG
C           3) MAKE A FEW STYLISTIC CHANGES
C
      DATA ZERO/0.0D0/, HALF/0.5D0/, ONE/1.0D0/, SIX/6.0D0/
      DATA THIR2/32.0D0/
      DATA BIG/30.0/, KFTLO/5/, KFTHI/11/
C
C     The constant BIG is set so that exp(-BIG) can be calculated
C     without causing underflow problems and can be considered = 0.
C
C     Initialize and check for valid parameter values.
C
  999 FORMAT(1X)
C
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
        GOTO9999
      ENDIF
C
      IF (DLO .GE. DHI) THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)
 9021   FORMAT('***** ERROR IN KERNEL DENSITY--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9023)
 9023   FORMAT('      THE LOWER BOUNDARY IS GREATER THAN THE UPPER ',
     1         'BOUNDARY.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9999
      ENDIF
C
C  CHECK FOR VALID NUMBER OF POINTS FOR DENSITY TRACE
C  (MUST BE A POWER OF 2 IN RANGE 2**KFTLO TO 2**KFTHI),
C  CURRENTLY VALUES BETWEEN 2**5 = 32 AND 2**11 = 2,048.
C
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
      GOTO9999
C
    2 CONTINUE
      STEP = (DHI - DLO) / DBLE(NFT)
      AINC = ONE / (NDT * STEP)
      NFT2 = NFT / 2
      HW = WINDOW / STEP
      FAC1 = THIR2 * (ATAN(ONE) * HW / NFT) ** 2
      IF (ICAL .NE. 0) GO TO 10
C
C     Discretize the data
C
      DLO1 = DLO - STEP * HALF
      DO 3 J = 1, NFT
        FT(J) = ZERO
    3 CONTINUE
C
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
C
C     Transform to find FT.
C
      CALL FORRT(FT, NFT)
C
C     Find transform of density estimate.
C
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
C
C     Cope with underflow by setting tail of transform to zero.
C
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
C
C     Invert Fourier transform of SMOOTH to get estimate and eliminate
C     negative density values.
C
      CALL REVRT(SMOOTH, NFT)
      DO 25 J = 1, NFT
        IF (SMOOTH(J) .LT. ZERO) SMOOTH(J) = ZERO
   25 CONTINUE
C
 9999 CONTINUE
      RETURN
      END
      DOUBLE PRECISION FUNCTION DNRM2(N,DX,INCX)
C***BEGIN PROLOGUE  DNRM2
C***DATE WRITTEN   791001   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  D1A3B
C***KEYWORDS  BLAS,DOUBLE PRECISION,EUCLIDEAN,L2,LENGTH,LINEAR ALGEBRA,
C             NORM,VECTOR
C***AUTHOR  LAWSON, C. L., (JPL)
C           HANSON, R. J., (SNLA)
C           KINCAID, D. R., (U. OF TEXAS)
C           KROGH, F. T., (JPL)
C***PURPOSE  Euclidean length (L2 norm) of d.p. vector
C***DESCRIPTION
C
C                B L A S  Subprogram
C    Description of parameters
C
C     --Input--
C        N  number of elements in input vector(s)
C       DX  double precision vector with N elements
C     INCX  storage spacing between elements of DX
C
C     --Output--
C    DNRM2  double precision result (zero if N .LE. 0)
C
C     Euclidean norm of the N-vector stored in DX() with storage
C     increment INCX .
C     If    N .LE. 0 return with result = 0.
C     If N .GE. 1 then INCX must be .GE. 1
C
C           C.L. Lawson, 1978 Jan 08
C
C     Four phase method     using two built-in constants that are
C     hopefully applicable to all machines.
C         CUTLO = maximum of  DSQRT(U/EPS)  over all known machines.
C         CUTHI = minimum of  DSQRT(V)      over all known machines.
C     where
C         EPS = smallest no. such that EPS + 1. .GT. 1.
C         U   = smallest positive no.   (underflow limit)
C         V   = largest  no.            (overflow  limit)
C
C     Brief outline of algorithm..
C
C     Phase 1    scans zero components.
C     move to phase 2 when a component is nonzero and .LE. CUTLO
C     move to phase 3 when a component is .GT. CUTLO
C     move to phase 4 when a component is .GE. CUTHI/M
C     where M = N for X() real and M = 2*N for complex.
C
C     Values for CUTLO and CUTHI..
C     From the environmental parameters listed in the IMSL converter
C     document the limiting values are as followS..
C     CUTLO, S.P.   U/EPS = 2**(-102) for  Honeywell.  Close seconds are
C                   Univac and DEC at 2**(-103)
C                   Thus CUTLO = 2**(-51) = 4.44089E-16
C     CUTHI, S.P.   V = 2**127 for Univac, Honeywell, and DEC.
C                   Thus CUTHI = 2**(63.5) = 1.30438E19
C     CUTLO, D.P.   U/EPS = 2**(-67) for Honeywell and DEC.
C                   Thus CUTLO = 2**(-33.5) = 8.23181D-11
C     CUTHI, D.P.   same as S.P.  CUTHI = 1.30438D19
C     DATA CUTLO, CUTHI / 8.232D-11,  1.304D19 /
C     DATA CUTLO, CUTHI / 4.441E-16,  1.304E19 /
C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,
C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*,
C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL
C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  DNRM2
      INTEGER          NEXT
      DOUBLE PRECISION   DX(1), CUTLO, CUTHI, HITEST, SUM, XMAX,ZERO,ONE
      DATA   ZERO, ONE /0.0D0, 1.0D0/
C
      DATA CUTLO, CUTHI / 8.232D-11,  1.304D19 /
C***FIRST EXECUTABLE STATEMENT  DNRM2
      IF(N .GT. 0) GO TO 10
         DNRM2  = ZERO
         GO TO 300
C
CCC10 ASSIGN 30 TO NEXT
   10 CONTINUE
      NEXT=30
      SUM = ZERO
      NN = N * INCX
C                                                 BEGIN MAIN LOOP
      I = 1
CCC20 GO TO NEXT,(30, 50, 70, 110)
   20 CONTINUE
      IF(NEXT.EQ.30)THEN
        GOTO30
      ELSEIF(NEXT.EQ.50)THEN
        GOTO50
      ELSEIF(NEXT.EQ.70)THEN
        GOTO70
      ELSEIF(NEXT.EQ.110)THEN
        GOTO110
      ENDIF
C
   30 IF( DABS(DX(I)) .GT. CUTLO) GO TO 85
CCCCC ASSIGN 50 TO NEXT
      NEXT=50
      XMAX = ZERO
C
C                        PHASE 1.  SUM IS ZERO
C
   50 IF( DX(I) .EQ. ZERO) GO TO 200
      IF( DABS(DX(I)) .GT. CUTLO) GO TO 85
C
C                                PREPARE FOR PHASE 2.
CCCCC ASSIGN 70 TO NEXT
      NEXT=70
      GO TO 105
C
C                                PREPARE FOR PHASE 4.
C
  100 I = J
CCCCC ASSIGN 110 TO NEXT
      NEXT=110
      SUM = (SUM / DX(I)) / DX(I)
  105 XMAX = DABS(DX(I))
      GO TO 115
C
C                   PHASE 2.  SUM IS SMALL.
C                             SCALE TO AVOID DESTRUCTIVE UNDERFLOW.
C
   70 IF( DABS(DX(I)) .GT. CUTLO ) GO TO 75
C
C                     COMMON CODE FOR PHASES 2 AND 4.
C                     IN PHASE 4 SUM IS LARGE.  SCALE TO AVOID OVERFLOW.
C
  110 IF( DABS(DX(I)) .LE. XMAX ) GO TO 115
         SUM = ONE + SUM * (XMAX / DX(I))**2
         XMAX = DABS(DX(I))
         GO TO 200
C
  115 SUM = SUM + (DX(I)/XMAX)**2
      GO TO 200
C
C
C                  PREPARE FOR PHASE 3.
C
   75 SUM = (SUM * XMAX) * XMAX
C
C
C     FOR REAL OR D.P. SET HITEST = CUTHI/N
C     FOR COMPLEX      SET HITEST = CUTHI/(2*N)
C
   85 CONTINUE
      HITEST = CUTHI/FLOAT( N )
C
C                   PHASE 3.  SUM IS MID-RANGE.  NO SCALING.
C
      DO 95 J =I,NN,INCX
         IF(DABS(DX(J)) .GE. HITEST) GO TO 100
         SUM = SUM + DX(J)**2
   95 CONTINUE
      DNRM2 = DSQRT( SUM )
      GO TO 300
C
  200 CONTINUE
      I = I + INCX
      IF ( I .LE. NN ) GO TO 20
C
C              END OF MAIN LOOP.
C
C              COMPUTE SQUARE ROOT AND ADJUST FOR SCALING.
C
      DNRM2 = XMAX * DSQRT(SUM)
  300 CONTINUE
      RETURN
      END
      SUBROUTINE DECONV(Y1,N1,Y2,N2,NUMVAR,IWRITE,
     1                  Y3,N3,IBUGA3,IERROR)
C
C     PURPOSE--COMPUTE DECONVOLUTION OF 2 VARIABLES.
C     NOTE--IF  THE FIRST  VARIABLE IS Y1(.)
C           AND THE SECOND VARIABLE IS Y2(.),
C           THEN THE OUTPUT VARIABLE CONTAINING THE
C           DECONVOLUTION
C           WILL BE COMPUTED AS FOLLOWS (IF N1 EQUALS OR EXCEEDS N2)--
C              Y3(1)=Y2(1)/Y1(1)
C              Y3(2)=(Y2(2)-Y1(2)*Y3(1)) / Y1(1)
C              Y3(3)=(Y2(3) - Y1(3)*Y3(1) - Y1(2)*Y3(2)) / Y1(1)
C              ETC.
C           AND CONVERSELY IF N1 IS LESS THAN N2.
C     NOTE--IT IS NOT PERMISSIBLE TO HAVE THE OUTPUT VECTOR Y3(.)
C           BEING IDENTICAL WITH (OVERLAYED ONTO) THE INPUT VECTORS Y1(.)
C           OR Y2(.).
C     NOTE--Y1 AND Y2 NEED NOT BE THE SAME LENGTH.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--NOVEMBER  1981.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
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
      DIMENSION Y3(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DECO'
      ISUBN2='NV  '
      IERROR='NO'
C
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
        DO55I=1,N1
          WRITE(ICOUT,56)I,Y1(I)
   56     FORMAT('I,Y1(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
        DO57I=1,N2
          WRITE(ICOUT,58)I,Y2(I)
   58     FORMAT('I,Y2(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   57   CONTINUE
      ENDIF
C
C               *********************************
C               **  COMPUTE THE DECONVOLUTION  **
C               *********************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N1.LE.0)GOTO150
      IF(N2.LE.0)GOTO150
C
      IF(N1.LE.N2)N3=N2-N1+1
      IF(N1.GT.N2)N3=N1-N2+1
      IF(N3.LE.0)GOTO170
C
      DO100I3=1,N3
      Y3(I3)=0.0
  100 CONTINUE
C
      DO500I3=1,N3
      SUM=0.0
      J3MAX=I3-1
      IF(J3MAX.LE.0)GOTO550
      DO600J3=1,J3MAX
      J1ARG=I3-J3+1
      IF(N1.LE.N2)SUM=SUM+Y1(J1ARG)*Y3(J3)
      IF(N1.GT.N2)SUM=SUM+Y2(J1ARG)*Y3(J3)
  600 CONTINUE
  550 CONTINUE
      IF(N1.LE.N2)Y3(I3)=(Y2(I3)-SUM)/Y1(1)
      IF(N1.GT.N2)Y3(I3)=(Y1(I3)-SUM)/Y2(1)
  500 CONTINUE
      GOTO190
C
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
  157 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS HERE = ',2I8,
     1'.')
      CALL DPWRST('XXX','BUG ')
      GOTO190
C
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
  177 FORMAT('      THE OUTPUT NUMBER OF OBSERVATIONS HERE = ',I8,
     1'.')
      CALL DPWRST('XXX','BUG ')
      GOTO190
C
  190 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
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
        DO9015I=1,N12
          WRITE(ICOUT,9016)I,Y1(I),Y2(I),Y3(I)
 9016     FORMAT('I,Y1(I),Y2(I),Y3(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DEHAAN(X,N,THRESH,GAMMA,SD,KK,ANM1)
CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   SUBROUTINE IMPLEMENTING THE DEHAAN-                 C
C   DEKKER MOMENT-BASED EXTREME VALUE                   C
C   INDEX ESTIMATOR AS DOCUMENTED IN                    C
C   "EXTREME VALUE THEORY AND APPLICATIONS",            C
C   EDITED BY GALAMBOS, LECHNER, AND SIMIU, PP. 93-122, C
C   KLUWER ACADEMIC PUBLISHERS, BOSTON, 1994.           C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC NOTE: DEHAAN NORMALLY DONE AS A PLOT.  WE ARE PICKING A SINGLE
CC       "SAMPLE" VALUE, ALGORITHM WAS MODIFIED ACCORDINGLY.  
CC
CC UPDATED 10/2010: SLIGHT TWEAK TO ALGORITHM.  PASS IN VALUE
CC OF THRESHOLD AND USE THIS AS VALUE FOR DX2.  THE X ARRAY SHOULD
CC CONTAIN POINTS ABOVE THE THRESHOLD ONLY.
CC
      DOUBLE PRECISION GAMNUM,GAMDEN, DGAMMA
      DOUBLE PRECISION DTERM1, DX1, DX2
      REAL GAMMA
      REAL X(*)
CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C           THE MAIN LOOP         C
C   COMPUTE THE DEHAAN-DEKKER     C
C   INDEX "GAMMA" FOR THE K       C
C   HIGHEST ORDER STATISTICS,     C
C   ITERATING ON K.               C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
      NI=N
C
      AN=REAL(NI)
      ATEMP=SQRT(AN)
      KK = NI
CC
C  GAMNUM AND GAMDEN ARE MN(1) AND MN(2) ON PAGE 100
C  OF THE REFERENCE CITED ABOVE.
C
      GAMNUM=0.D0
      GAMDEN=0.D0
CC
      DO 50 J=1,KK
CCCCC DO 50 J=1,NI
CC
          JM1=J-1
          DX1=DBLE(X(NI-JM1))
CCCCC     DX2=DBLE(X(NI-KK))
          DX2=THRESH
          DTERM1=DLOG(DX1)-DLOG(DX2)
          GAMNUM=GAMNUM+DTERM1
          GAMDEN=GAMDEN+DTERM1*DTERM1
CC
50      CONTINUE
CC
        GAMNUM=GAMNUM/DBLE(KK) 
        GAMDEN=GAMDEN/DBLE(KK) 
        ANM1=REAL(GAMNUM)
        ANM2=REAL(GAMDEN)
CC
        DTERM1=GAMNUM**2/GAMDEN
        DGAMMA=GAMNUM + 1.0D0 - 0.5D0*(1.0D0/(1.0D0 - DTERM1))
        GAMMA=REAL(DGAMMA)
C
C  COMPUTE THE STANDARD DEVIATION OF C
C
      IF(GAMMA.GE.0.0)THEN
        SD=SQRT((1.0+GAMMA*GAMMA)/REAL(KK))
      ELSE
        DTERM1=(1.0D0-DGAMMA)*(1.0D0-DGAMMA)*(1.0D0-2.0D0*DGAMMA)
        DTERM2=4.0D0-8.0D0*(1.0D0-2.0D0*DGAMMA)/(1.0D0-3.0D0*DGAMMA)
        DTERM3=(5.0D0-11.0D0*DGAMMA)*(1.0D0-2.0D0*DGAMMA)/
     1         ((1.0D0-3.0D0*DGAMMA)*(1.0D0-4.0D0*DGAMMA))
        SD=REAL(DSQRT(DTERM1*(DTERM2+DTERM3)/DBLE(KK)))
      ENDIF 
CC
      RETURN
      END 
      SUBROUTINE DEQUOT(IA,NCIN,IB,NCOUT2,IBUGSU,ISUBRO)
C
C     PURPOSE--CHECK A STRING FOR LEADING/TRAILING QUOTES AND
C              REMOVE IF FOUND.  USED FOR FILE NAME ARGUMENTS THAT
C              MAY BE QUOTED IF THEY CONTAIN SPACES OR HYPHENS.
C     INPUT  ARGUMENTS--IA     = INPUT CHARACTER STRING
C                       NCIN   = INTEGER NUMBER OF CHARACTERS TO CHECK
C                       IBUGSU = HOLLERITH BUG (= TRACE) VARIABLE
C     OUTPUT ARGUMENTS--IB     = OUTPUT CHARACTER STRING
C                       NCOUT2  = INTEGER NUMBER OF CHARACTERS ON OUTPUT
C
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2004/8
C     ORIGINAL VERSION--OCTOBER   2004
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*(*) IA
      CHARACTER*(*) IB
C
      CHARACTER*1 IQUOTE
      CHARACTER*1 IQUOT2
C
      CHARACTER*4 IBUGSU
      CHARACTER*4 ISUBRO
C
C---------------------------------------------------------------------
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
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
C
C               ******************************************************
C               **  CHECK FOR LEADING/TRAILING QUOTES.              **
C               ******************************************************
C
C
      CALL DPCONA(39,IQUOTE)
      IQUOT2='"'
      NCOUT2=0
C
      IF(NCIN.GT.0)THEN
        IF(IA(1:1).EQ.IQUOT2)THEN
          DO100I=2,NCIN
            IF(IA(I:I).EQ.IQUOT2)GOTO109
              NCOUT2=NCOUT2+1
              IB(NCOUT2:NCOUT2)=IA(I:I)
  100     CONTINUE
  109     CONTINUE
        ELSEIF(IA(1:1).EQ.'"')THEN
          DO200I=2,NCIN
            IF(IA(I:I).EQ.IQUOTE)GOTO209
              NCOUT2=NCOUT2+1
              IB(NCOUT2:NCOUT2)=IA(I:I)
  200     CONTINUE
  209     CONTINUE
        ELSE
          IB(1:NCIN)=IA(1:NCIN)
          NCOUT2=NCIN
        ENDIF
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
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
C
      RETURN
      END
C===================================================== DERF.FOR
      DOUBLE PRECISION FUNCTION DERFDP(X)
CCCCC 2020/03: RENAME TO AVOID CONFLICT WITH INTRNISIC FUNCTION
CCCCC DOUBLE PRECISION FUNCTION DERF(X)
C***********************************************************************
C*                                                                     *
C*  FORTRAN CODE WRITTEN FOR INCLUSION IN IBM RESEARCH REPORT RC20525, *
C*  'FORTRAN ROUTINES FOR USE WITH THE METHOD OF L-MOMENTS, VERSION 3' *
C*                                                                     *
C*  J. R. M. HOSKING                                                   *
C*  IBM RESEARCH DIVISION                                              *
C*  T. J. WATSON RESEARCH CENTER                                       *
C*  YORKTOWN HEIGHTS                                                   *
C*  NEW YORK 10598, U.S.A.                                             *
C*                                                                     *
C*  VERSION 3     AUGUST 1996                                          *
C*                                                                     *
C***********************************************************************
C
C  ERROR FUNCTION
C
C  BASED ON ALGORITHM 5666, J.F.HART ET AL. (1968) 'COMPUTER
C  APPROXIMATIONS'
C
C  ACCURATE TO 15 DECIMAL PLACES
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DATA ZERO/0D0/,ONE/1D0/,TWO/2D0/,THREE/3D0/,FOUR/4D0/,P65/0.65D0/
C
C         COEFFICIENTS OF RATIONAL-FUNCTION APPROXIMATION
C
      DATA P0,P1,P2,P3,P4,P5,P6/
     *  0.22020 68679 12376 1D3,    0.22121 35961 69931 1D3,
     *  0.11207 92914 97870 9D3,    0.33912 86607 83830 0D2,
     *  0.63739 62203 53165 0D1,    0.70038 30644 43688 1D0,
     *  0.35262 49659 98910 9D-1/
      DATA Q0,Q1,Q2,Q3,Q4,Q5,Q6,Q7/
     *  0.44041 37358 24752 2D3,   0.79382 65125 19948 4D3,
     *  0.63733 36333 78831 1D3,   0.29656 42487 79673 7D3,
     *  0.86780 73220 29460 8D2,   0.16064 17757 92069 5D2,
     *  0.17556 67163 18264 2D1,   0.88388 34764 83184 4D-1/
C
C         C1 IS SQRT(2), C2 IS SQRT(2/PI)
C         BIG IS THE POINT AT WHICH DERF=1 TO MACHINE PRECISION
C
      DATA C1/1.4142 13562 37309 5D0/
      DATA C2/7.9788 45608 02865 4D-1/
      DATA BIG/6.25D0/,CRIT/5D0/
C
      DERFDP=ZERO
      IF(X.EQ.ZERO)RETURN
      XX=DABS(X)
      IF(XX.GT.BIG)GOTO 20
      EXPNTL=DEXP(-X*X)
      ZZ=DABS(X*C1)
      IF(XX.GT.CRIT)GOTO 10
      DERFDP=EXPNTL*((((((P6*ZZ+P5)*ZZ+P4)*ZZ+P3)*ZZ+P2)*ZZ+P1)*ZZ+P0)/
     *  (((((((Q7*ZZ+Q6)*ZZ+Q5)*ZZ+Q4)*ZZ+Q3)*ZZ+Q2)*ZZ+Q1)*ZZ+Q0)
      IF(X.GT.ZERO)DERFDP=ONE-TWO*DERFDP
      IF(X.LT.ZERO)DERFDP=TWO*DERFDP-ONE
      RETURN
C
   10 DERFDP=EXPNTL*C2/(ZZ+ONE/(ZZ+TWO/(ZZ+THREE/(ZZ+FOUR/(ZZ+P65)))))
      IF(X.GT.ZERO)DERFDP=ONE-DERFDP
      IF(X.LT.ZERO)DERFDP=DERFDP-ONE
      RETURN
C
   20 DERFDP=ONE
      IF(X.LT.ZERO)DERFDP=-ONE
      RETURN
      END
      SUBROUTINE DERIV0(IW21,IW22,ITYPE,NW,
     1IPARN1,IPARN2,NUMPAR,IVARN1,IVARN2,NUMVAR,
     1ICON,ICON1,ICON2,NCON,ID1,ID2,NWD,
     1IBUGA3,ISUBRO,IFOUND,IERROR)
C
C NOTE--THE ARRAY ICONN (DEFINED BELOW AND USED
C       IN SUBSEQUENT SUBROUTINES) IS PROBABLY
C       SUPERFLUOUS AND PROBABLY NO LONGER SERVES ANY PURPOSE
C       (CHECK THIS).
C       THE NECESSITY OF IEXPN IS ALSO IN QUESTION.
C
C---------------------------------------------------------------------
C
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
C
      CHARACTER*4 ILF
      CHARACTER*4 IHOLD1
      CHARACTER*4 IHOLD2
      CHARACTER*4 IFUN01
      CHARACTER*4 IFUN02
      CHARACTER*4 IDER01
      CHARACTER*4 IDER02
      CHARACTER*4 ICONN
      CHARACTER*4 IEXPN
C
      CHARACTER*4 IHOLW1
      CHARACTER*4 IHOLW2
      CHARACTER*4 IHOLDT
      CHARACTER*4 ITER01
      CHARACTER*4 ITER02
C
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
CCCCC CHARACTER*4 IBUG1
      CHARACTER*4 IBUG2
CCCCC CHARACTER*4 IBUG3
CCCCC CHARACTER*4 IBUG41
CCCCC CHARACTER*4 IBUG5
CCCCC CHARACTER*4 IBUG51
C
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
C
      DIMENSION IHOLD1(200)
      DIMENSION IHOLD2(200)
      DIMENSION IFUN01(200)
      DIMENSION IFUN02(200)
      DIMENSION IDER01(200)
      DIMENSION IDER02(200)
      DIMENSION ICONN(200)
      DIMENSION IEXPN(200)
C
      DIMENSION IHOLW1(200)
      DIMENSION IHOLW2(200)
      DIMENSION IHOLDT(200)
      DIMENSION ITER01(1000)
      DIMENSION ITER02(1000)
      DIMENSION ITERM1(100)
      DIMENSION ITERM2(100)
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----DATA STATEMENTS-----------------------------------------------------
CCCCC DATA IBUG1/'OFF '/
      DATA IBUG2/'OFF '/
CCCCC DATA IBUG3/'OFF '/
CCCCC DATA IBUG41/'OFF '/
CCCCC DATA IBUG5/'OFF '/
CCCCC DATA IBUG51/'OFF '/
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DERI'
      ISUBN2='V0  '
C
      IMIN=1
      IMAX=1
C
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'RIV0')GOTO90
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('AT THE BEGINNING OF DERIV0--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)NW
   52 FORMAT('NW = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO55I=1,NW
      WRITE(ICOUT,56)I,ITYPE(I),IW21(I),IW22(I)
   56 FORMAT('I,ITYPE(I),IW21(I),IW22(I) = ',I8,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
      WRITE(ICOUT,61)NCON
   61 FORMAT('NCON = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO65I=1,NCON
      WRITE(ICOUT,66)I,ICON1(I),ICON2(I),ICON(I)
   66 FORMAT('I,ICON1(I),ICON2(I),ICON(I) = ',3I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
   90 CONTINUE
C
C               ***********************************
C               **  STEP 0--                     **
C               **  REDUCE THE FULL EXPRESSION   **
C               **  INTO NAMED SUB-EXPRESSIONS.  **
C               ***********************************
C
      IT2=0
C
C               *****************************************
C               **  STEP 1--                           **
C               **  REPLACE THE CONSTANTS              **
C               **  BY THE CONSTANT DESIGNATIONS.      **
C               *****************************************
C
      ILOOP=1
 2350 CONTINUE
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO2400I=1,NW
      I2=I
      IF(ITYPE(I).EQ.'N   ')GOTO2450
 2400 CONTINUE
      ISTOP=NW+1
      ISTART=0
      GOTO2790
 2450 CONTINUE
C
      ISTART=I2
      ISTOP=ISTART
      CALL DPC4HI(IW21(ISTOP),IC,IBUGA3,IERROR)
C
C               ***************************************************
C               **  STEP 1.4--                                   **
C               **  TEMPORARILY COPY THE STRING WHICH IS BEYOND  **
C               **  THE CONSTANT NUMBER                          **
C               **  INTO IHOLD1(.).                               **
C               ***************************************************
C
      ISTEPN='1.4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      J=0
      ISTOP1=ISTOP+1
      IF(ISTOP1.GT.NW)GOTO2249
      DO2240I=ISTOP1,NW
      J=J+1
      IHOLW1(J)=IW21(I)
      IHOLW2(J)=IW22(I)
      IHOLDT(J)=ITYPE(I)
 2240 CONTINUE
 2249 CONTINUE
      NREST=J
C
C               ****************************
C               **  STEP 1.5--            **
C               **  REPLACE THE CONSTANT  **
C               **  BY A & AND A NUMBER.  **
C               ****************************
C
      ISTEPN='1.5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      J=ISTART
      IW21(J)='&   '
      IW22(J)='    '
      ITYPE(J)='C   '
      J=J+1
      CALL DPC4IH(IC,IW21(J),IBUGA3,IERROR)
      IW22(J)='    '
      ITYPE(J)='C   '
C
      IF(NREST.LE.0)GOTO2290
      DO2280I=1,NREST
      J=J+1
      IW21(J)=IHOLW1(I)
      IW22(J)=IHOLW2(I)
      ITYPE(J)=IHOLDT(I)
 2280 CONTINUE
 2290 CONTINUE
      NW=J
C
      IF(ISTART.LE.0)GOTO2790
      ILOOP=ILOOP+1
      IF(ILOOP.LE.10000)GOTO2350
 2790 CONTINUE
C
      ILOOP=1
 5310 CONTINUE
      DO5400I=1,NW
      I2=I
      IF(ITYPE(I).EQ.'RP  ')GOTO5450
 5400 CONTINUE
      ISTOP=NW+1
      ISTART=0
      GOTO5690
 5450 CONTINUE
C
      ISTOP=I2
      DO5600I=1,ISTOP
      IREV=ISTOP-I+1
      IF(ITYPE(IREV).EQ.'LP  ')GOTO5650
 5600 CONTINUE
      WRITE(ICOUT,5605)
 5605 FORMAT('***** ERROR IN COMPID--ITYPE(IREV) NOT LP')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      RETURN
 5650 CONTINUE
      ISTART=IREV
 5690 CONTINUE
C
      ISTAP1=ISTART+1
      ISTOM1=ISTOP-1
C
C               *******************************************************
C               **  STEP 1.6--                                       **
C               **  CHECK THE INTERNAL STRING TO SEE                 **
C               **  IF IT IS EXACTLY 2 POSITIONS WIDE, AND           **
C               **  ALSO THAT IT IS OF THE FORM                      **
C               **  $ FOLLOWED BY A NUMBER.                          **
C               **  IF SO, THEN THIS IMPLIES                         **
C               **  THAT THE INTERNAL ORIGINAL STRING                **
C               **  HAS ALREADY BEEN FULLY REDUCED.                  **
C               **  IF NOT SO, THEN THIS IMPLIES                     **
C               **  THAT THE INTERNAL ORIGINAL                       **
C               **  STRING HAS NOT YET BEEN FULLY REDUCED,           **
C               **  AND THAT THE OPERATION PRELIMINARY               **
C               **  TO THE ( MUST BE CHECKED TO                      **
C               **  DETERMINE IF THE PARENTHESES                     **
C               **  ARE TO BE KEPT OR DELETED                        **
C               **  (KEEP IF A PRELIMINARY LIBRARY FUNCTION;         **
C               **  DELETE IF A PRELIMINARY OPERATION--+,-,*,/,**).  **
C               **  DELETE IF ANYTHING ELSE).                        **
C               *******************************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ISTOM2=ISTOP-2
      IWIDIS=ISTOM1-ISTAP1+1
      IF(IWIDIS.EQ.2.AND.IW21(ISTOM2).EQ.'$   ')GOTO6300
      GOTO6200
C
C               ******************************
C               ******************************
C               **  STEP 2--                **
C               **  TREAT THE NO-$ CASE.    ************************************
C               **  THIS WILL BE THE        **
C               **  NOT-FULLY-REDUCED CASE. **
C               ******************************
C
C               *************************************************
C               **  STEP 2.1--                                 **
C               **  CHECK FOR A PRELIMINARY LIBRARY FUNCTION.  **
C               *************************************************
C
 6200 CONTINUE
      ISTEPN='2.1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ILF='NO  '
      ISTAM1=ISTART-1
      IF(ISTAM1.LE.0)GOTO6219
      IF(ITYPE(ISTAM1).EQ.'LF  ')ILF='YES'
 6219 CONTINUE
C
C               *******************************
C               **  STEP 2.2--               **
C               **  COPY THE STRING BETWEEN  **
C               **  (BUT NOT INCLUDING) THE  **
C               **  PARENTHESES.             **
C               *******************************
C
      ISTEPN='2.2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      J=0
      ITERM1(ILOOP)=IT2+1
      DO6220I=ISTAP1,ISTOM1
      J=J+1
      IT2=IT2+1
      ITER01(IT2)=IW21(I)
      ITER02(IT2)=IW22(I)
 6220 CONTINUE
      ITERM2(ILOOP)=IT2
C
C               ***************************************************
C               **  STEP 2.3--                                   **
C               **  TEMPORARILY COPY THE STRING WHICH IS BEYOND  **
C               **  THE RIGHT PARENTHESIS                        **
C               **  INTO IHOLD1(.).                               **
C               ***************************************************
C
      ISTEPN='2.3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      J=0
      ISTOP1=ISTOP+1
      IF(ISTOP1.GT.NW)GOTO6249
      DO6240I=ISTOP1,NW
        J=J+1
        IHOLD1(J)=IW21(I)
        IHOLD2(J)=IW22(I)
        IHOLDT(J)=ITYPE(I)
 6240 CONTINUE
 6249 CONTINUE
      NREST=J
C
C               ********************************************
C               **  STEP 2.4--                            **
C               **  REPLACE THE EXTRACTED STRING BY       **
C               **  A $ AND THE LOOP NUMBER.              **
C               **  RETAIN OR DELETE PARENTHESES          **
C               **  DEPENDING ON WHETHER THE PRELIMINARY  **
C               **  OPERATION IS A LIBRARY FUNCTION       **
C               **  OR AN ARITHMETIC OPERATION.           **
C               ********************************************
C
      ISTEPN='2.4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
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
      IF(NREST.LE.0)GOTO6290
      DO6260I=1,NREST
        J=J+1
        IW21(J)=IHOLD1(I)
        IW22(J)=IHOLD2(I)
        ITYPE(J)=IHOLDT(I)
 6260 CONTINUE
 6290 CONTINUE
      NW=J
      GOTO6900
C
C               ****************************
C               **  STEP 3--              **
C               **  TREAT THE $ CASE.     **
C               **  THIS WILL BE THE      **
C               **  FULLY-REDUCED CASE.   **
C               ****************************
C
C               *************************************************
C               **  STEP 3.1--                                 **
C               **  CHECK FOR A PRELIMINARY LIBRARY FUNCTION.  **
C               *************************************************
C
 6300 CONTINUE
      ISTEPN='3.1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ILF='NO  '
      ISTAM1=ISTART-1
      IF(ISTAM1.LE.0)GOTO6319
      IF(ITYPE(ISTAM1).EQ.'LF  ')ILF='YES'
 6319 CONTINUE
C
C               *******************************************
C               **  STEP 3.2--                           **
C               **  IF NO PRELIMINARY LIBRARY FUNCTION,  **
C               **  THEN COPY THE STRING BETWEEN         **
C               **  (BUT NOT INCLUDING) THE              **
C               **  PARENTHESES.                         **
C               **  IF A PRELIMINARY LIBRARY FUNCTION,   **
C               **  THEN COPY THE STRING                 **
C               **  STARTING WITH (AND INCLUDING)        **
C               **  THE PRELIMINARY  LIBRARY FUNCTION    **
C               **  AND STOPPING WITH (AND INCLUDING)    **
C               **  THE RIGHT PARENTHESIS.               **
C               *******************************************
C
      ISTEPN='3.2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ILF.EQ.'YES')IMIN=ISTART-1
      IF(ILF.EQ.'YES')IMAX=ISTOP
      IF(ILF.EQ.'NO  ')IMIN=ISTART+1
      IF(ILF.EQ.'NO  ')IMAX=ISTOP-1
      J=0
      ITERM1(ILOOP)=IT2+1
      DO6320I=IMIN,IMAX
      J=J+1
      IT2=IT2+1
      ITER01(IT2)=IW21(I)
      ITER02(IT2)=IW22(I)
 6320 CONTINUE
      ITERM2(ILOOP)=IT2
C
C               ***************************************************
C               **  STEP 3.3--                                   **
C               **  TEMPORARILY COPY THE STRING WHICH IS BEYOND  **
C               **  THE RIGHT PARENTHESIS                        **
C               **  INTO IHOLD1(.).                               **
C               ***************************************************
C
      ISTEPN='3.3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      J=0
      ISTOP1=ISTOP+1
      IF(ISTOP1.GT.NW)GOTO6349
      DO6340I=ISTOP1,NW
      J=J+1
      IHOLD1(J)=IW21(I)
      IHOLD2(J)=IW22(I)
      IHOLDT(J)=ITYPE(I)
 6340 CONTINUE
 6349 CONTINUE
      NREST=J
C
C               ********************************************
C               **  STEP 3.4--                            **
C               **  REPLACE THE EXTRACTED STRING BY       **
C               **  A $ AND THE LOOP NUMBER.              **
C               **  RETAIN OR DELETE PARENTHESES          **
C               **  DEPENDING ON WHETHER THE PRELIMINARY  **
C               **  OPERATION IS A LIBRARY FUNCTION       **
C               **  OR AN ARITHMETIC OPERATION.           **
C               ********************************************
C
      ISTEPN='3.4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC J=IMIN-1
CCCCC J=J+1
      IF(ILF.EQ.'YES')J=ISTART-1
      IF(ILF.EQ.'NO  ')J=ISTART
      IW21(J)='$   '
      IW22(J)='    '
      ITYPE(J)='E   '
      J=J+1
      CALL DPC4IH(ILOOP,IW21(J),IBUGA3,IERROR)
      IW22(J)='    '
      ITYPE(J)='E   '
      IF(NREST.LE.0)GOTO6390
      DO6360I=1,NREST
      J=J+1
      IW21(J)=IHOLD1(I)
      IW22(J)=IHOLD2(I)
      ITYPE(J)=IHOLDT(I)
 6360 CONTINUE
 6390 CONTINUE
      NW=J
      GOTO6900
C
 6900 CONTINUE
C
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'RIV0')GOTO6719
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6701)ILOOP
 6701 FORMAT('AFTER LOOP ',I8,'--  ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,6709)NW
 6709 FORMAT('NW = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO6700I=1,NW
      WRITE(ICOUT,6710)I,IW21(I),IW22(I),ITYPE(I)
 6710 FORMAT('I,IW21(I),IW22(I),ITYPE(I) = ',I8,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 6700 CONTINUE
 6719 CONTINUE
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'RIV0')GOTO6799
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
      DO6795I=IMIN,IMAX
      WRITE(ICOUT,6796)I,ITER01(I),ITER02(I)
 6796 FORMAT('I,ITER01(I),ITER02(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 6795 CONTINUE
 6799 CONTINUE
      IF(ISTART.LE.0)GOTO5900
      ILOOP=ILOOP+1
      IF(ILOOP.LE.10000)GOTO5310
C
 5900 CONTINUE
      NLOOP=ILOOP
C
C               ************************
C               **  STEP 4--          **
C               **  TAKE DERIVATIVES  **
C               ************************
C
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NWD=2
      ID1(1)='%   '
      ID2(1)='    '
CCCCC ID1(2)=NLOOP
      CALL DPC4IH(NLOOP,ID1(2),IBUGA3,IERROR)
      ID2(2)='    '
      IF(IBUG2.EQ.'ON')WRITE(ICOUT,710)NLOOP
  710 FORMAT('NLOOP = ',I8)
      IF(IBUG2.EQ.'ON')CALL DPWRST('XXX','BUG ')
C
      ILOOP=1
 7350 CONTINUE
      ISTEPN='7350'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')
     1WRITE(ICOUT,881)ILOOP,NWD
  881 FORMAT('ILOOP,NWD = ',2I8)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')
     1CALL DPWRST('XXX','BUG ')
      DO7400I=1,NWD
      I2=I
      IF(ID1(I).EQ.'%   '.AND.ID2(I).EQ.'    ')GOTO7450
 7400 CONTINUE
      ISTOP=NWD+1
      ISTART=0
      GOTO7790
 7450 CONTINUE
      ISTEPN='7450'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ISTART=I2
      ISTOP=ISTART+1
CCCCC IF=ID1(ISTOP)
      CALL DPC4HI(ID1(ISTOP),IF,IBUGA3,IERROR)
      IF(IBUG2.EQ.'ON')WRITE(ICOUT,720)IF
  720 FORMAT('IF = ',I8)
      IF(IBUG2.EQ.'ON')CALL DPWRST('XXX','BUG ')
C
C               ******************************************
C               **  STEP 4.2--                          **
C               **  COPY OUT THE FUNCTION IN QUESTION   **
C               **  INTO A VECTOR FROM WHICH            **
C               **  THE DERIVATIVE WILL BE DETERMINED.  **
C               ******************************************
C
      ISTEPN='4.2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      J=0
      IMIN=ITERM1(IF)
      IMAX=ITERM2(IF)
      DO740I=IMIN,IMAX
      J=J+1
      IFUN01(J)=ITER01(I)
      IFUN02(J)=ITER02(I)
  740 CONTINUE
      NCF0=J
C
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'RIV0')GOTO779
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
      DO775I=1,NCF0
      WRITE(ICOUT,776)IFUN01(I),IFUN02(I)
  776 FORMAT('IFUN01(I),IFUN02(I) = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
  775 CONTINUE
  779 CONTINUE
C
C               ************************************
C               **  STEP 4.3--                    **
C               **  DETERMINE THE DERIVATIVE      **
C               **  OF THE FUNCTION UNDER STUDY.  **
C               ************************************
C
      ISTEPN='4.3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DERIV1(IFUN01,IFUN02,NCF0,
     1IPARN1,IPARN2,NUMPAR,IVARN1,IVARN2,NUMVAR,
     1ICONN,NUMCON,IEXPN,NUMEXP,
     1IDER01,IDER02,NCD0,
     1IBUGA3,ISUBRO,IFOUND,IERROR)
C
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'RIV0')GOTO789
      WRITE(ICOUT,783)
  783 FORMAT('      IMMEDIATELY AFTER CALLING DERIV1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,784)NCD0
  784 FORMAT('NCD0 = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO785I=1,NCD0
      WRITE(ICOUT,786)I,IDER01(I),IDER02(I)
  786 FORMAT('I,IDER01(I),IDER02(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
  785 CONTINUE
  789 CONTINUE
C
C               ***************************************************
C               **  STEP 4.4--                                   **
C               **  TEMPORARILY COPY THE STRING WHICH IS BEYOND  **
C               **  THE FUNCTION NUMBER                          **
C               **  INTO IHOLD1(.).                               **
C               ***************************************************
C
      ISTEPN='4.4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      J=0
      ISTOP1=ISTOP+1
      IF(ISTOP1.GT.NWD)GOTO7249
      DO7240I=ISTOP1,NWD
      J=J+1
      IHOLD1(J)=ID1(I)
      IHOLD2(J)=ID2(I)
 7240 CONTINUE
 7249 CONTINUE
      NREST=J
C
C               *****************************************************
C               **  STEP 4.5--                                     **
C               **  REPLACE THE % AND THE FUNCTION NUMBER          **
C               **  (A SHORT-HAND DESIGNATION FOR THE DERIVATIVE)  **
C               **  BY THE FUNCTION'S DERIVATIVE.                  **
C               *****************************************************
C
      ISTEPN='4.5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      J=ISTART-1
      J=J+1
      ID1(J)='(   '
      ID2(J)='    '
      DO7270I=1,NCD0
      J=J+1
      ID1(J)=IDER01(I)
      ID2(J)=IDER02(I)
 7270 CONTINUE
      J=J+1
      ID1(J)=')   '
      ID2(J)='    '
      IF(NREST.LE.0)GOTO7290
      DO7280I=1,NREST
      J=J+1
      ID1(J)=IHOLD1(I)
      ID2(J)=IHOLD2(I)
 7280 CONTINUE
 7290 CONTINUE
      NWD=J
C
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'RIV0')GOTO799
      WRITE(ICOUT,792)ILOOP
  792 FORMAT('      AT THE END OF LOOP ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,794)NWD,ISTART,ILOOP
  794 FORMAT('NWD,ISTART,ILOOP = ',3I8)
      CALL DPWRST('XXX','BUG ')
      DO795I=1,NWD
      WRITE(ICOUT,796)I,ID1(I),ID2(I)
  796 FORMAT('I,ID1(I),ID2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
  795 CONTINUE
  799 CONTINUE
C
      IF(ISTART.LE.0)GOTO7790
      ILOOP=ILOOP+1
      IF(ILOOP.LE.10000)GOTO7350
 7790 CONTINUE
      ISTEPN='7790'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'RIV0')GOTO7799
      WRITE(ICOUT,7792)
 7792 FORMAT('      AT THE END OF STEP 4 (AND 4.5)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7794)ILOOP,NWD
 7794 FORMAT('ILOOP,NWD = ',2I8)
      CALL DPWRST('XXX','BUG ')
      DO7795I=1,NWD
      WRITE(ICOUT,7796)I,ID1(I),ID2(I)
 7796 FORMAT('I,ID1(I),ID2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 7795 CONTINUE
 7799 CONTINUE
C
C               *****************************************
C               **  STEP 5--                           **
C               **  REPLACE THE FUNCTION DESIGNATIONS  **
C               **  BY THE FUNCTIONS                   **
C               *****************************************
C
      ILOOP=1
 8350 CONTINUE
C
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO8400I=1,NWD
      I2=I
      IF(ID1(I).EQ.'$   '.AND.ID2(I).EQ.'    ')GOTO8450
 8400 CONTINUE
      ISTOP=NWD+1
      ISTART=0
      GOTO8790
 8450 CONTINUE
C
      ISTART=I2
      ISTOP=ISTART+1
CCCCC IF=ID1(ISTOP)
      CALL DPC4HI(ID1(ISTOP),IF,IBUGA3,IERROR)
C
C               ***************************************************
C               **  STEP 5.4--                                   **
C               **  TEMPORARILY COPY THE STRING WHICH IS BEYOND  **
C               **  THE FUNCTION NUMBER                          **
C               **  INTO IHOLD1(.).                               **
C               ***************************************************
C
      ISTEPN='5.4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      J=0
      ISTOP1=ISTOP+1
      IF(ISTOP1.GT.NWD)GOTO8249
      DO8240I=ISTOP1,NWD
      J=J+1
      IHOLD1(J)=ID1(I)
      IHOLD2(J)=ID2(I)
 8240 CONTINUE
 8249 CONTINUE
      NREST=J
C
C               *************************************************
C               **  STEP 5.5--                                 **
C               **  REPLACE THE $ AND FUNCTION NUMBER          **
C               **  (A SHORT-HAND DESIGNATION FOR A FUNCTION)  **
C               **  BY THE FUNCTION.                           **
C               *************************************************
C
      ISTEPN='5.5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      J=ISTART-1
      J=J+1
      ID1(J)='(   '
      ID2(J)='    '
      IMIN=ITERM1(IF)
      IMAX=ITERM2(IF)
      DO8270I=IMIN,IMAX
      J=J+1
      ID1(J)=ITER01(I)
      ID2(J)=ITER02(I)
 8270 CONTINUE
      J=J+1
      ID1(J)=')   '
      ID2(J)='    '
      IF(NREST.LE.0)GOTO8290
      DO8280I=1,NREST
      J=J+1
      ID1(J)=IHOLD1(I)
      ID2(J)=IHOLD2(I)
 8280 CONTINUE
 8290 CONTINUE
      NWD=J
C
      IF(ISTART.LE.0)GOTO8790
      ILOOP=ILOOP+1
      IF(ILOOP.LE.10000)GOTO8350
C
 8790 CONTINUE
C
CCCCC IF(IBUG51.EQ.'OFF')GOTO8799
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'RIV0')GOTO8799
      WRITE(ICOUT,8792)
 8792 FORMAT('      AT THE END OF STEP 5 (AND 5.5)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8794)NWD
 8794 FORMAT('NWD = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO8795I=1,NWD
      WRITE(ICOUT,8796)I,ID1(I),ID2(I)
 8796 FORMAT('I,ID1(I),ID2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 8795 CONTINUE
 8799 CONTINUE
C
C               *****************************************
C               **  STEP 6--                           **
C               **  REPLACE THE CONSTANT DESIGNATIONS  **
C               **  BY THE CONSTANTS                   **
C               *****************************************
C
      ILOOP=1
 9350 CONTINUE
C
      ISTEPN='6'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO9400I=1,NWD
      I2=I
      IF(ID1(I).EQ.'&   '.AND.ID2(I).EQ.'    ')GOTO9450
 9400 CONTINUE
      ISTOP=NWD+1
      ISTART=0
      GOTO9790
 9450 CONTINUE
C
      ISTART=I2
      ISTOP=ISTART+1
      CALL DPC4HI(ID1(ISTOP),IC,IBUGA3,IERROR)
C
C               ***************************************************
C               **  STEP 6.4--                                   **
C               **  TEMPORARILY COPY THE STRING WHICH IS BEYOND  **
C               **  THE CONSTANT NUMBER                          **
C               **  INTO IHOLD1(.).                               **
C               ***************************************************
C
      ISTEPN='6.4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      J=0
      ISTOP1=ISTOP+1
      IF(ISTOP1.GT.NWD)GOTO9249
      DO9240I=ISTOP1,NWD
      J=J+1
      IHOLD1(J)=ID1(I)
      IHOLD2(J)=ID2(I)
 9240 CONTINUE
 9249 CONTINUE
      NREST=J
C
C               *************************************************
C               **  STEP 6.5--                                 **
C               **  REPLACE THE & AND CONSTANT NUMBER          **
C               **  (A SHORT-HAND DESIGNATION FOR A CONSTANT)  **
C               **  BY THE CONSTANT.                           **
C               *************************************************
C
      ISTEPN='6.5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')
     1WRITE(ICOUT,9261)IC,ICON1(IC),ICON2(IC)
 9261 FORMAT('IC,ICON1(IC),ICON2(IC) = ',3I8)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV0')
     1CALL DPWRST('XXX','BUG ')
C
      J=ISTART-1
      IMIN=ICON1(IC)
      IMAX=ICON2(IC)
      DO9270I=IMIN,IMAX
      J=J+1
      ID1(J)=ICON(I)
      ID2(J)='    '
 9270 CONTINUE
      IF(NREST.LE.0)GOTO9290
      DO9280I=1,NREST
      J=J+1
      ID1(J)=IHOLD1(I)
      ID2(J)=IHOLD2(I)
 9280 CONTINUE
 9290 CONTINUE
      NWD=J
C
      IF(ISTART.LE.0)GOTO9790
      ILOOP=ILOOP+1
      IF(ILOOP.LE.10000)GOTO9350
 9790 CONTINUE
C
C               ****************
C               **  STEP 90-- **
C               **  EXIT.     **
C               ****************
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RIV0')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DERIV0--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)NWD
 9012   FORMAT('NWD = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,NWD
          WRITE(ICOUT,9016)I,ID1(I),ID2(I)
 9016     FORMAT('I,ID1(I),ID2(I) = ',I8,2X,A4,2X,A4)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DERIV1(IFUN01,IFUN02,NCF0,
     1                  IPARN1,IPARN2,NUMPAR,IVARN1,IVARN2,NUMVAR,
     1                  ICONN,NUMCON,IEXPN,NEXP,
     1                  IDER01,IDER02,NCD0,
     1                  IBUGA3,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--DETERMINE THE DERIVATIVE OF AN
C              EXPRESSION WHICH HAS NO PARENTHESES
C              UNLESS THEY ARE AFTER A
C              LIBRARY FUNCTION, AND WHICH
C              MAY HAVE +, -, *, /, **).
C
C              THE INPUT EXPRESSION IS IN THE
C              VECTOR IFUN01(.) (FOR FIRST 4 CHARACTERS) AND
C              VECTOR IFUN02(.) (FOR NEXT  4 CHARACTERS)--IT HAS
C              LENGTH (= NUMBER OF CHARACTERS) NCF.
C
C              THE OUTPUT EXPRESSION WILL BE IN
C              VECTOR IDER01(.) (FOR FIRST 4 CHARACTERS) AND
C              VECTOR IDER02(.) (FOR NEXT  4 CHARACTERS)--IT HAS
C              HAVE LENGTH (= NUMBER OF CHARACTERS) NCD.
C
C     INPUT  ARGUMENTS--IFUN01 = THE VECTOR
C                                WHICH CONTAINS THE EXPRESSION
C                                OF INTEREST
C                                (FIRST 4 CHARACTERS).
C                     --IFUN02 = THE VECTOR
C                                WHICH CONTAINS THE EXPRESSION
C                                OF INTEREST
C                                (NEXT 4 CHARACTERS).
C                     --NCF0   = AN INTEGER NUMBER
C                                OF CHARACTERS IN IFUN01.
C     OUTPUT ARGUMENTS--IDER01 = THE VECTOR
C                                WHICH CONTAINS THE DERIVATIVE
C                                OF THE EXPRESSION OF INTEREST
C                                (FIRST 4 CHARACTERS).
C                     --IDER02 = THE VECTOR
C                                WHICH CONTAINS THE DERIVATIVE
C                                OF THE EXPRESSION OF INTEREST
C                                (NEXT  4 CHARACTERS).
C                     --NCD0   = AN INTEGER NUMBER
C                                OF CHARACTERS IN IDER01.
C
C     ORIGINAL VERSION--DECEMBER 8, 1978
C     UPDATED         --DECEMBER  1981.
C
C---------------------------------------------------------------------
C
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
C
CCCCC CHARACTER*4 IBUG1
CCCCC CHARACTER*4 IBUG2
CCCCC CHARACTER*4 IBUG3
C
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
      CHARACTER*4 IFUN11
      CHARACTER*4 IFUN12
      CHARACTER*4 IDER11
      CHARACTER*4 IDER12
C
      DIMENSION IFUN01(*)
      DIMENSION IFUN02(*)
      DIMENSION IDER01(*)
      DIMENSION IDER02(*)
C
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
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----DATA STATEMENTS-----------------------------------------------------
C
CCCCC DATA IBUG1/'OFF'/
CCCCC DATA IBUG2/'OFF'/
CCCCC DATA IBUG3/'OFF'/
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DERI'
      ISUBN2='V1  '
C
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
        DO55I=1,NCF0
          WRITE(ICOUT,56)I,IFUN01(I),IFUN02(I)
   56     FORMAT('I,IFUN01(I),IFUN02(I) = ',I8,2(2X,A4))
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ********************************************************
C               **  STEP 2--                                          **
C               **  EXTRACT EACH ADDITIVE SUBSTRING FROM IFUN01(.).     **
C               **  A SUBSTRING IS ADDITIVE IF SEPARATED              **
C               **  FROM OTHER SUBSTRINGS BY A    +   OR    -   .     **
C               **  PLACE THE I-TH SUBSTRING IN ROW I OF IFUN11(.,.).  **
C               **  DETERMINE THE NUMBER OF CHARACTERS IN             **
C               **  EACH SUBSTRING.  THE NUMBER OF CHARACTERS         **
C               **  IN THE I-TH SUBSTRING WILL BE PLACED              **
C               **  IN NCF1(I).                                       **
C               **  DETERMINE THE TOTAL NUMBER OF SUBSTRINGS.         **
C               **  THIS NUMBER WILL BE PLACED IN NFUN1.              **
C               ********************************************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV1')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NFUN1=0
      JMIN=1
      DO400I=1,NCF0
      I2=I
      IF(IFUN01(I).EQ.'+   '.AND.IFUN02(I).EQ.'    ')GOTO420
      IF(IFUN01(I).EQ.'-   '.AND.IFUN02(I).EQ.'    ')GOTO420
      GOTO400
  420 CONTINUE
C
      JMAX=I2-1
      IF(JMAX.LT.JMIN)GOTO400
C
      NFUN1=NFUN1+1
      K=0
      IF(IFUN01(JMIN).EQ.'+   '.AND.IFUN02(JMIN).EQ.'    ')GOTO440
      IF(IFUN01(JMIN).EQ.'-   '.AND.IFUN02(JMIN).EQ.'    ')GOTO440
      K=K+1
      IFUN11(NFUN1,K)='+   '
      IFUN12(NFUN1,K)='    '
  440 CONTINUE
C
      DO450J=JMIN,JMAX
      K=K+1
      IFUN11(NFUN1,K)=IFUN01(J)
      IFUN12(NFUN1,K)=IFUN02(J)
  450 CONTINUE
      NCF1(NFUN1)=K
      JMIN=I
  400 CONTINUE
C
      JMAX=NCF0
      NFUN1=NFUN1+1
      K=0
      IF(IFUN01(JMIN).EQ.'+   '.AND.IFUN02(JMIN).EQ.'    ')GOTO540
      IF(IFUN01(JMIN).EQ.'-   '.AND.IFUN02(JMIN).EQ.'    ')GOTO540
      K=K+1
      IFUN11(NFUN1,K)='+   '
      IFUN12(NFUN1,K)='    '
  540 CONTINUE
C
      DO550J=JMIN,JMAX
      K=K+1
      IFUN11(NFUN1,K)=IFUN01(J)
      IFUN12(NFUN1,K)=IFUN02(J)
  550 CONTINUE
      NCF1(NFUN1)=K
C
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'RIV1')GOTO790
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,701)
  701 FORMAT('IN THE MIDDLE OF DERIV1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,702)NCD0
  702 FORMAT('NCD0 = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO705I=1,NCD0
      WRITE(ICOUT,706)I,IDER01(I),IDER02(I)
  706 FORMAT('I,IDER01(I),IDER02(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
  705 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,709)NFUN1
  709 FORMAT('NFUN1 = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO710IF1=1,NFUN1
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,712)IF1
  712 FORMAT('IF1 = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,713)NCD1(IF1)
  713 FORMAT('NCD1(IF1) = ',I8)
      CALL DPWRST('XXX','BUG ')
      JMAX=NCD1(IF1)
      DO715J=1,JMAX
      WRITE(ICOUT,716)J,IDER11(IF1,J),IDER12(IF1,J)
  716 FORMAT('J,IDER11(IF1,J),IDER12(IF1,J) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
  715 CONTINUE
  710 CONTINUE
  790 CONTINUE
C
C               *************************************************
C               **  STEP 3--                                   **
C               **  OPERATE ON EACH ADDITIVE COMPONENT         **
C               **  DETERMINE THE DERIVATIVE OF EACH ADDITIVE  **
C               **  COMPONENT.                                 **
C               *************************************************
C
      DO1000IROW1=1,NFUN1
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV1')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DERIV2(IFUN11,IFUN12,NCF1,IROW1,
     1IPARN1,IPARN2,NUMPAR,IVARN1,IVARN2,NUMVAR,
     1ICONN,NUMCON,IEXPN,NUMEXP,IDER11,IDER12,NCD1,
     1IBUGA3,ISUBRO,IFOUND,IERROR)
 1000 CONTINUE
C
C               ***************************************
C               **  STEP 4--                         **
C               **  COMBINE EACH ADDITIVE COMPONENT  **
C               **  INTO ONE LONG STRING             **
C               **  SO AS TO FORM THE DERIVATIVE     **
C               **  FOR THE ENTIRE EXPRESSION.       **
C               ***************************************
C
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV1')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      K=0
      DO2000IROW1=1,NFUN1
      JMAX=NCD1(IROW1)
      IF(JMAX.LE.0)GOTO2000
      IF(JMAX.EQ.1.AND.
     1IDER11(IROW1,1).EQ.'0    '.AND.IDER12(IROW1,1).EQ.'    ')GOTO2000
      DO2100J=1,JMAX
      K=K+1
      IDER01(K)=IDER11(IROW1,J)
      IDER02(K)=IDER12(IROW1,J)
 2100 CONTINUE
      IF(IROW1.EQ.NFUN1)GOTO2000
      K=K+1
      IDER01(K)='+   '
      IDER02(K)='    '
 2000 CONTINUE
      IF(K.GE.1.AND.
     1IDER01(K).EQ.'+   '.AND.IDER02(K).EQ.'    ')K=K-1
      IF(K.LE.0)GOTO2150
      GOTO2190
 2150 CONTINUE
      K=1
      IDER01(K)='0   '
      IDER02(K)='    '
 2190 CONTINUE
      NCD0=K
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RIV1')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('AT THE END       OF DERIV1--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)NFUN1,IF1,NCD1(IF1),NCD0
 9012   FORMAT('NFUN1,IF1,NCD1(IF1),NCD0 = ',4I8)
        CALL DPWRST('XXX','BUG ')
        DO9015IF1=1,NFUN1
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          JMAX=NCD1(IF1)
          DO9020J=1,JMAX
            WRITE(ICOUT,9021)J,IDER11(IF1,J),IDER12(IF1,J)
 9021       FORMAT('J,IDER11(IF1,J),IDER12(IF1,J) = ',I8,2(2X,A4))
            CALL DPWRST('XXX','BUG ')
 9020     CONTINUE
 9015   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        DO9035I=1,NCD0
          WRITE(ICOUT,9036)I,IDER01(I),IDER02(I)
 9036     FORMAT('I,IDER01(I),IDER02(I) = ',I8,2(2X,A4))
          CALL DPWRST('XXX','BUG ')
 9035   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DERIV2(IFUN11,IFUN12,NCF1,IROW1,
     1IPARN1,IPARN2,NUMPAR,IVARN1,IVARN2,NUMVAR,
     1ICONN,NUMCON,IEXPN,NUMEXP,IDER11,IDER12,NCD1,
     1IBUGA3,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--DETERMINE THE DERIVATIVE OF
C              A MULTIPLICATIVE EXPRESSION
C              (= 1 FULL ADDITIVE COMPONENT)
C              (EXAMPLE, A*X/C*D**E*X)
C
C              THE ENTIRE INPUT EXPRESSION IS LOCATED
C              IN ROW IROW1 OF IFUN11--
C              IT HAS LENGTH NF1
C
C              THE OUTPUT DERIVATIVE IS LOCATED
C              IN ROW IROW1 OF IFUN11--
C              IT HAS LENGTH NCD1.
C
C     INPUT  ARGUMENTS--IFUN11 = THE ARRAY WHOSE IROW1-TH ROW
C                                IS THE IROW1-TH ADDITIVE COMPONENT
C                                OF INTEREST
C                                (FIRST 4 CHARACTERS).
C                     --IFUN12 = THE ARRAY WHOSE IROW1-TH ROW
C                                IS THE IROW1-TH ADDITIVE COMPONENT
C                                OF INTEREST
C                                (NEXT  4 CHARACTERS).
C                     --NCF1   = AN INTEGER VECTOR
C                                WHOSE IROW1-TH ELEMENT
C                                IS THE LENGTH OF THE IROW1-TH
C                                STRING IN IFUN11(.,.);
C                                THAT IS, NCF1(IROW1) = THE LENGTH OF THE
C                                ADDITIVE COMPONENT OF INTEREST.
C                     --IROW1  = THE ROW NUMBER (IN IFUN11(.,.)) OF
C                                THE PARTICULAR
C                                ADDITIVE COMPONENT OF INTEREST.
C                     --IPARN1 = THE HOLLARITH VECTOR
C                                OF PARAMETER NAMES
C                                (FIRST 4 CHARACTERS).
C                     --IPARN2 = THE HOLLARITH VECTOR
C                                OF PARAMETER NAMES
C                                (NEXT  4 CHARACTERS).
C                     --NUMPAR = THE INTEGER NUMBER
C                                OF PARAMETERS.
C                     --IVARN1 = THE HOLLARITH VECTOR
C                                OF VARIABLE NAMES
C                                (FIRST 4 CHARACTERS).
C                     --IVARN2 = THE HOLLARITH VECTOR
C                                OF VARIABLE NAMES
C                                (NEXT  4 CHARACTERS).
C                     --NUMVAR = THE INTEGER NUMBER
C                                OF VARIABLE NAMES.
C                     --ICONN  = THE HOLLARITH VECTOR
C                                OF CONSTANT NAMES.
C                     --NUMCON = THE INTEGER NUMBER
C                                OF CONSTANTS.
C                     --IEXPN  = THE HOLLARITH VECTOR
C                                OF EXPRESSION NAMES.
C                     --NUMEXP = THE INTEGER NUMBER
C                                OF EXPRESSION NAMES.
C     OUTPUT ARGUMENTS--IDER11 = THE ARRAY WHOSE IROW1-TH R
C                                WILL BE THE DERIVATIVE OF THE
C                                IROW1-TH ADDITIVE STRING
C                                (FIRST 4 CHARACTERS).
C                     --IDER12 = THE ARRAY WHOSE IROW1-TH R
C                                WILL BE THE DERIVATIVE OF THE
C                                IROW1-TH ADDITIVE STRING
C                                (NEXT  4 CHARACTERS).
C                       NCD1   = AN INTEGER VECTOR
C                                WHOSE IROW1-TH ELEMENT
C                                WILL BE THE LENGTH OF THE IROW1-TH
C                                DERIVATIVE IN IDER1(.,.);
C                                THAT IS, NCD1(IROW1) = THE LENGTH OF THE
C                                DERIVATIVE OF INTEREST.
C     INTERNAL ARRAYS--
C                     --IFUN21 = THE ARRAY WHOSE I-TH
C                                ROW WILL BE THE I-TH MULTIPLICATIVE
C                                SUBSTRING OF THE IROW1-TH
C                                ADDITIVE COMPONENT
C                                (FIRST 4 CHARACTERS).
C                     --IFUN22 = THE ARRAY WHOSE I-TH
C                                ROW WILL BE THE I-TH MULTIPLICATIVE
C                                SUBSTRING OF THE IROW1-TH
C                                ADDITIVE COMPONENT
C                                (NEXT  4 CHARACTERS).
C                       NCF2   = AN INTEGER VECTOR
C                                WHOSE I-TH ELEMENT
C                                WILL BE THE LENGTH OF THE I-TH
C                                MULTIPLICATIVE SUBSTRING
C                                OF THE IROW1-TH ADDITIVE COMPONENT.
C                       NFUN2  = THE NUMBER OF ROWS
C                                (= THE NUMBER OF MULTIPLICATIVE
C                                SUBSTRINGS OF THE IROW1-TH
C                                ADDITIVE COMPONENT)
C                                THAT WILL BE
C                                IN THE ARRAY IFUN21(.,.)
C                       IOP2   = A VECTOR
C                                WHOSE I-TH ELEMENT
C                                WILL BE THE (TRAILING) OPERATION (* OR /)
C                                OF THE I-TH MULTIPLICATIVE SUBSTRING
C                                OF THE IROW1-TH ADDITIVE COMPONENT.
C
C     ORIGINAL VERSION--DECEMBER 2, 1978
C     UPDATED         --DECEMBER  1981.
C
C---------------------------------------------------------------------
C
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
C
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
      CHARACTER*4 IFUN21
      CHARACTER*4 IFUN22
      CHARACTER*4 IDER21
      CHARACTER*4 IDER22
      CHARACTER*4 IOP2
C
CCCCC CHARACTER*4 IBUG1
CCCCC CHARACTER*4 IBUG2
CCCCC CHARACTER*4 IBUG3
C
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
C
      DIMENSION IFUN21(20,80)
      DIMENSION IFUN22(20,80)
      DIMENSION NCF2(20)
      DIMENSION IDER21(20,80)
      DIMENSION IDER22(20,80)
      DIMENSION NCD2(20)
      DIMENSION IOP2(20)
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----DATA STATEMENTS-----------------------------------------------------
C
CCCCC DATA IBUG1/'OFF'/
CCCCC DATA IBUG2/'OFF'/
CCCCC DATA IBUG3/'OFF'/
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DERI'
      ISUBN2='V2  '
      IERROR='NO'
C
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'RIV2')GOTO90
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
      DO61J=1,ITEMP
      WRITE(ICOUT,62)J,IFUN11(IROW1,J),IFUN12(IROW1,J)
   62 FORMAT('J,IFUN11(IROW1,J),IFUN12(IROW1,J) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   61 CONTINUE
   90 CONTINUE
C
C               ********************************************************
C               **  STEP 1--                                          **
C               **  EXTRACT EACH MULTIPLICATIVE SUBSTRING.            **
C               **  A SUBSTRING IS MULTIPLICATIVE IF SEPARATED        **
C               **  FROM OTHER SUBSTRINGS BY A    *   OR    /   .     **
C               **  PLACE THE I-TH SUBSTRING IN ROW I OF IFUN21(.,.).  **
C               **  DETERMINE THE NUMBER OF CHARACTERS IN             **
C               **  EACH SUBSTRING.  THE NUMBER OF CHARACTERS         **
C               **  IN THE I-TH SUBSTRING WILL BE PLACED              **
C               **  IN NCF2(I).                                       **
C               **  DETERMINE THE TOTAL NUMBER OF SUBSTRINGS.         **
C               **  THIS NUMBER WILL BE PLACED IN NFUN2.              **
C               ********************************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NFUN2=0
      JMIN=1
      IMIN=1
      IMAX=NCF1(IROW1)
      DO400I=IMIN,IMAX
      IF(IFUN11(IROW1,I).EQ.'*   '.AND.IFUN12(IROW1,I).EQ.'    ')GOTO420
      IF(IFUN11(IROW1,I).EQ.'/   '.AND.IFUN12(IROW1,I).EQ.'    ')GOTO420
      GOTO400
  420 CONTINUE
C
      JMAX=I-1
      IF(JMAX.LT.JMIN)GOTO430
      GOTO440
  430 CONTINUE
C
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
      GOTO9000
  440 CONTINUE
C
      NFUN2=NFUN2+1
      K=0
      DO450J=JMIN,JMAX
      K=K+1
      IFUN21(NFUN2,K)=IFUN11(IROW1,J)
      IFUN22(NFUN2,K)=IFUN12(IROW1,J)
  450 CONTINUE
      NCF2(NFUN2)=K
      IOP2(NFUN2)=IFUN11(IROW1,I)
      JMIN=I+1
  400 CONTINUE
C
      JMAX=IMAX
      IF(JMAX.LT.JMIN)GOTO530
      GOTO540
  530 CONTINUE
C
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
      GOTO9000
  540 CONTINUE
C
      NFUN2=NFUN2+1
      K=0
      DO550J=JMIN,JMAX
      K=K+1
      IFUN21(NFUN2,K)=IFUN11(IROW1,J)
      IFUN22(NFUN2,K)=IFUN12(IROW1,J)
  550 CONTINUE
      NCF2(NFUN2)=K
C
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'RIV2')GOTO690
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,601)
  601 FORMAT('AFTER STEP 1 OF DERIV2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,610)NFUN2
  610 FORMAT('NFUN2 = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO615I=1,NFUN2
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,616)I
  616 FORMAT('I = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,617)NCF2(I)
  617 FORMAT('NCF2(I) = ',I8)
      CALL DPWRST('XXX','BUG ')
      ITEMP=NCF2(I)
      DO620J=1,ITEMP
      WRITE(ICOUT,621)I,J,IFUN21(I,J),IFUN22(I,J)
  621 FORMAT('I,J,IFUN21(I,J),IFUN22(I,J) = ',I8,I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
  620 CONTINUE
  615 CONTINUE
  690 CONTINUE
C
C               *******************************************************
C               **  STEP 2--                                         **
C               **  OPERATE ON EACH MULTIPLICATIVE COMPONENT.        **
C               **  DETERMINE THE DERIVATIVE OF EACH MULTIPLICATIVE  **
C               **  COMPONENT.                                       **
C               *******************************************************
C
      DO700IROW2=1,NFUN2
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DERIV3(IFUN21,IFUN22,NCF2,IROW2,
     1IPARN1,IPARN2,NUMPAR,IVARN1,IVARN2,NUMVAR,
     1ICONN,NUMCON,IEXPN,NUMEXP,IDER21,IDER22,NCD2,
     1IBUGA3,ISUBRO,IFOUND,IERROR)
  700 CONTINUE
C
C               ****************************************
C               **  STEP 3--                          **
C               **  COMBINE MULTIPLICATIVE COMPONENT  **
C               **  DERIVATIVES TO DETERMINE THE      **
C               **  DERIVATIVE OF THE IROW1-TH        **
C               **  (IROW1 FIXED) ADDITIVE COMPONENT. **
C               ****************************************
C
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DERIV4(IFUN21,IFUN22,NCF2,NFUN2,
     1IDER21,IDER22,NCD2,IOP2,IROW1,
     1IDER11,IDER12,NCD1,
     1IBUGA3,ISUBRO,IFOUND,IERROR)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'RIV2')GOTO9090
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
      DO9021J=1,ITEMP
      WRITE(ICOUT,9022)J,IDER11(IROW1,J),IDER12(IROW1,J)
 9022 FORMAT('J,IDER11(IROW1,J),IDER12(IROW1,J) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9021 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DERIV3(IFUN21,IFUN22,NCF2,IROW2,
     1                   IPARN1,IPARN2,NUMPAR,IVARN1,IVARN2,NUMVAR,
     1                   ICONN,NUMCON,IEXPN,NUMEXP,IDER21,IDER22,NCD2,
     1                   IBUGA3,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--DETERMINE THE DERIVATIVE OF
C              AN ELEMENTAL COMPONENT
C              (EXAMPLE, X, OR X**B, OR -X, OR -X**X)
C              WHICH IS A COMPONENT THAT HAS
C              NO +, -, *, OR /.
C              IT MAY HAVE ** (AS IN A**B).
C              IT MAY HAVE A SIGN (OR NO SIGN).
C              IT MAY BE ONLY A SINGLE ELEMENT.
C
C              THE INPUT ELEMENT IS LOCATED
C              IN ROW IROW2 OF IFUN21--
C              IT HAS LENGTH NF2.
C
C              THE OUTPUT DERIVATIVE IS LOCATED
C              IN ROW IROW2 OF IFUN21--
C              IT HAS LENGTH NCD2.
C
C     INPUT  ARGUMENTS--IFUN21 = THE ARRAY WHOSE IROW2-TH ROW
C                                IS THE IROW2-TH ELEMENTAL COMPONENT
C                                OF INTEREST
C                                (FIRST 4 CHARACTERS).
C                     --IFUN22 = THE ARRAY WHOSE IROW2-TH ROW
C                                IS THE IROW2-TH ELEMENTAL COMPONENT
C                                OF INTEREST
C                                (NEXT  4 CHARACTERS).
C                     --NCF2   = AN INTEGER VECTOR
C                                WHOSE IROW2-TH ELEMENT
C                                IS THE LENGTH OF THE IROW2-TH
C                                STRING IN IFUN21(.,.);
C                                THAT IS, NCF2(IROW2) = THE LENGTH OF THE
C                                ELEMENTAL COMPONENT OF INTEREST.
C                     --IROW2  = THE ROW NUMBER (IN IFUN21(.,.)) OF
C                                THE PARTICULAR
C                                ELEMENTAL COMPONENT OF INTEREST.
C                     --IPARN1 = THE HOLLARITH VECTOR
C                                OF PARAMETER NAMES
C                                (FIRST 4 CHARACTERS).
C                     --IPARN2 = THE HOLLARITH VECTOR
C                                OF PARAMETER NAMES
C                                (NEXT  4 CHARACTERS).
C                     --NUMPAR = THE INTEGER NUMBER
C                                OF PARAMETERS.
C                     --IVARN1 = THE HOLLARITH VECTOR
C                                OF VARIABLE NAMES
C                                (FIRST 4 CHARACTERS).
C                     --IVARN2 = THE HOLLARITH VECTOR
C                                OF VARIABLE NAMES
C                                (NEXT  4 CHARACTERS).
C                     --NUMVAR = THE INTEGER NUMBER
C                                OF VARIABLE NAMES.
C                     --ICONN  = THE HOLLARITH VECTOR
C                                OF CONSTANT NAMES.
C                     --NUMCON = THE INTEGER NUMBER
C                                OF CONSTANTS.
C                     --IEXPN  = THE HOLLARITH VECTOR
C                                OF EXPRESSION NAMES.
C                     --NUMEXP = THE INTEGER NUMBER
C                                OF EXPRESSION NAMES.
C     OUTPUT ARGUMENTS--IDER21 = THE ARRAY WHOSE IROW2-TH ROW
C                                WILL BE THE DERIVATIVE OF THE
C                                IROW2-TH ELEMENTAL STRING
C                                (FIRST 4 CHARACTERS).
C                     --IDER22 = THE ARRAY WHOSE IROW2-TH ROW
C                                WILL BE THE DERIVATIVE OF THE
C                                IROW2-TH ELEMENTAL STRING
C                                (NEXT  4 CHARACTERS).
C                     --NCD2   = AN INTEGER VECTOR
C                                WHOSE IROW2-TH ELEMENT
C                                WILL BE THE LENGTH OF THE IROW2-TH
C                                DERIVATIVE IN IDER21(.,.);
C                                THAT IS, NCD2(IROW2) = THE LENGTH OF THE
C                                DERIVATIVE OF INTEREST.
C
C     DATE--DECEMBER 9, 1978
C
C---------------------------------------------------------------------
C
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
C
      CHARACTER*4 IFUNZ1
      CHARACTER*4 IFUNZ2
      CHARACTER*4 IDERZ1
      CHARACTER*4 IDERZ2
C
CCCCC CHARACTER*4 IBUG1
CCCCC CHARACTER*4 IBUG2
CCCCC CHARACTER*4 IBUG3
C
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
C
      CHARACTER*4 IHOL11
      CHARACTER*4 IHOL12
      CHARACTER*4 IHOL21
      CHARACTER*4 IHOL22
C
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
C
      DIMENSION IFUNZ1(300)
      DIMENSION IFUNZ2(300)
      DIMENSION IDERZ1(300)
      DIMENSION IDERZ2(300)
C
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DERI'
      ISUBN2='V3  '
C
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
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'RIV3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('AT THE BEGINNING OF DERIV3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICONN(1),IEXPN(1),NUMCON,IROW2,NCF2(IROW2)
   52   FORMAT('ICONN(1),IEXPN(1),NUMCON,IROW2,NCF2(IROW2) = ',
     1         2(A4,2X),3I8)
        CALL DPWRST('XXX','BUG ')
        DO55J=1,NCF2(IROW2)
          WRITE(ICOUT,56)J,IFUN21(IROW2,J),IFUN22(IROW2,J)
   56     FORMAT('J,IFUN21(IROW2,J),IFUN22(IROW2,J) = ',I8,2(2X,A4))
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
        WRITE(ICOUT,71)NUMPAR,NUMVAR,NUMEXP
   71   FORMAT('NUMPAR,NUMVAR,NUMEXP = ',3I8)
        CALL DPWRST('XXX','BUG ')
        DO62I=1,NUMPAR
          WRITE(ICOUT,63)I,IPARN1(I),IPARN2(I)
   63     FORMAT('I,IPARN1(I),IPARN2(I) = ',I8,2(2X,A4))
         CALL DPWRST('XXX','BUG ')
   62   CONTINUE
        DO72I=1,NUMVAR
          WRITE(ICOUT,73)I,IVARN1(I),IVARN2(I)
   73     FORMAT('I,IVARN1(I),IVARN2(I) = ',I8,2(2X,A4))
          CALL DPWRST('XXX','BUG ')
   72   CONTINUE
      ENDIF
C
C               **********************************
C               **  STEP 1--                    **
C               **  COPY THE EXPRESSION         **
C               **  IN ROW IROW2 OF IFUN21(.,.) **
C               **  INTO THE VECTOR IFUNZ1(.).  **
C               **********************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NCFZ=NCF2(IROW2)
      DO300I=1,NCFZ
      IFUNZ1(I)=IFUN21(IROW2,I)
      IFUNZ2(I)=IFUN22(IROW2,I)
      IDERZ1(I)='OOOO'
      IDERZ2(I)='OOOO'
      IDER21(IROW2,I)='OOOO'
      IDER22(IROW2,I)='OOOO'
  300 CONTINUE
C
C               ***************************************
C               **  STEP 2--                         **
C               **  SEARCH FOR A LEFT PARENTHESIS--  **
C               **  THIS WILL INDICATE A PRECEDING   **
C               **  LIBRARY FUNCTION.                **
C               ***************************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO310I=1,NCFZ
      I1=I
      IF(IFUNZ1(I).EQ.'(   '.AND.IFUNZ2(I).EQ.'    ')GOTO320
  310 CONTINUE
      GOTO3000
  320 CONTINUE
      I1M1=I1-1
      I1P1=I1+1
      I1P2=I1+2
      I1P3=I1+3
      IHLF1=IFUNZ1(I1M1)
      IHLF2=IFUNZ2(I1M1)
      IH1=IFUNZ1(I1P1)
      IH2=IFUNZ2(I1P1)
C
      IF(IH1.EQ.'$   '.AND.IH2.EQ.'    ')GOTO330
      GOTO339
  330 CONTINUE
      ITYPE='EXP '
      GOTO380
  339 CONTINUE
C
      IF(IH1.EQ.'&   '.AND.IH2.EQ.'    ')GOTO340
      GOTO349
  340 CONTINUE
      I2=1
      IDERZ1(1)='0   '
      IDERZ2(1)='    '
      GOTO985
  349 CONTINUE
C
      IF(NUMPAR.LE.0)GOTO359
      DO350I=1,NUMPAR
      IF(IH1.EQ.IPARN1(I).AND.IH2.EQ.IPARN2(I))GOTO355
  350 CONTINUE
      GOTO359
  355 CONTINUE
      I2=1
      IDERZ1(1)='0   '
      IDERZ2(1)='    '
      GOTO985
  359 CONTINUE
C
      IF(NUMVAR.LE.0)GOTO369
      DO360I=1,NUMVAR
      IF(IH1.EQ.IVARN1(I).AND.IH2.EQ.IVARN2(I))GOTO380
  360 CONTINUE
  369 CONTINUE
C
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
      DO376I=1,NCFZ
      WRITE(ICOUT,377)I,IFUNZ1(I),IFUNZ2(I)
  377 FORMAT('I,IFUNZ1(I),IFUNZ2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
  376 CONTINUE
      IERROR='YES'
      GOTO9000
C
  380 CONTINUE
      I2=0
      IF(IFUNZ1(1).EQ.'-   '.AND.IFUNZ2(I).EQ.'    ')GOTO385
      GOTO390
  385 CONTINUE
      I2=I2+1
      IDERZ1(I2)='-   '
      IDERZ2(I2)='    '
  390 CONTINUE
C
C               *****************************************
C               **  STEP 3--                           **
C               **  TREAT THE LIBRARY FUNCTIONS CASE.  **
C               *****************************************
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IHLF1.EQ.'SQRT'.AND.IHLF2.EQ.'    ')GOTO510
      IF(IHLF1.EQ.'EXP '.AND.IHLF2.EQ.'    ')GOTO510
      IF(IHLF1.EQ.'ALOG'.AND.IHLF2.EQ.'    ')GOTO510
      IF(IHLF1.EQ.'ALOG'.AND.IHLF2.EQ.'E   ')GOTO510
      IF(IHLF1.EQ.'ALOG'.AND.IHLF2.EQ.'10  ')GOTO510
      IF(IHLF1.EQ.'LOG '.AND.IHLF2.EQ.'    ')GOTO510
      IF(IHLF1.EQ.'LOGE'.AND.IHLF2.EQ.'    ')GOTO510
      IF(IHLF1.EQ.'LOG1'.AND.IHLF2.EQ.'0   ')GOTO510
C
      IF(IHLF1.EQ.'SIN '.AND.IHLF2.EQ.'    ')GOTO610
      IF(IHLF1.EQ.'COS '.AND.IHLF2.EQ.'    ')GOTO610
      IF(IHLF1.EQ.'TAN '.AND.IHLF2.EQ.'    ')GOTO610
      IF(IHLF1.EQ.'COT '.AND.IHLF2.EQ.'    ')GOTO610
      IF(IHLF1.EQ.'SEC '.AND.IHLF2.EQ.'    ')GOTO610
      IF(IHLF1.EQ.'CSC '.AND.IHLF2.EQ.'    ')GOTO610
C
      IF(IHLF1.EQ.'ARCS'.AND.IHLF2.EQ.'IN  ')GOTO620
      IF(IHLF1.EQ.'ARCC'.AND.IHLF2.EQ.'OS  ')GOTO620
      IF(IHLF1.EQ.'ARCT'.AND.IHLF2.EQ.'AN  ')GOTO620
      IF(IHLF1.EQ.'ARCC'.AND.IHLF2.EQ.'OT  ')GOTO620
      IF(IHLF1.EQ.'ARCS'.AND.IHLF2.EQ.'EC  ')GOTO620
      IF(IHLF1.EQ.'ARCC'.AND.IHLF2.EQ.'SC  ')GOTO620
C
      IF(IHLF1.EQ.'SINH'.AND.IHLF2.EQ.'    ')GOTO630
      IF(IHLF1.EQ.'COSH'.AND.IHLF2.EQ.'    ')GOTO630
      IF(IHLF1.EQ.'TANH'.AND.IHLF2.EQ.'    ')GOTO630
      IF(IHLF1.EQ.'COTH'.AND.IHLF2.EQ.'    ')GOTO630
      IF(IHLF1.EQ.'SECH'.AND.IHLF2.EQ.'    ')GOTO630
      IF(IHLF1.EQ.'CSCH'.AND.IHLF2.EQ.'    ')GOTO630
C
      IF(IHLF1.EQ.'ARCS'.AND.IHLF2.EQ.'INH ')GOTO640
      IF(IHLF1.EQ.'ARCC'.AND.IHLF2.EQ.'OSH ')GOTO640
      IF(IHLF1.EQ.'ARCT'.AND.IHLF2.EQ.'ANH ')GOTO640
      IF(IHLF1.EQ.'ARCC'.AND.IHLF2.EQ.'OTH ')GOTO640
      IF(IHLF1.EQ.'ARCS'.AND.IHLF2.EQ.'ECH ')GOTO640
      IF(IHLF1.EQ.'ARCC'.AND.IHLF2.EQ.'SCH ')GOTO640
C
      IFOUND='NO'
      GOTO8000
C
  510 CONTINUE
      CALL LIBFD1(IHLF1,IHLF2,I1,I2,ITYPE,
     1IFUNZ1,IFUNZ2,IDERZ1,IDERZ2)
      GOTO970
C
  610 CONTINUE
      CALL TRIGD1(IHLF1,IHLF2,I1,I2,ITYPE,
     1IFUNZ1,IFUNZ2,IDERZ1,IDERZ2)
      GOTO970
C
  620 CONTINUE
      CALL TRIGD2(IHLF1,IHLF2,I1,I2,ITYPE,
     1IFUNZ1,IFUNZ2,IDERZ1,IDERZ2)
      GOTO970
C
  630 CONTINUE
      CALL TRIGD3(IHLF1,IHLF2,I1,I2,ITYPE,
     1IFUNZ1,IFUNZ2,IDERZ1,IDERZ2)
      GOTO970
C
  640 CONTINUE
      CALL TRIGD4(IHLF1,IHLF2,I1,I2,ITYPE,
     1IFUNZ1,IFUNZ2,IDERZ1,IDERZ2)
      GOTO970
C
  970 CONTINUE
      IF(ITYPE.EQ.'EXP ')GOTO980
      GOTO985
C
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
C
  985 CONTINUE
      NCDZ=I2
      IF(NCDZ.LE.2)GOTO990
      IF(IDERZ1(1).EQ.'-   '.AND.IDERZ2(1).EQ.'    '.AND.
     1   IDERZ1(2).EQ.'-   '.AND.IDERZ2(2).EQ.'    ')GOTO986
      IF(IDERZ1(1).EQ.'+   '.AND.IDERZ2(1).EQ.'    '.AND.
     1   IDERZ1(2).EQ.'+   '.AND.IDERZ2(2).EQ.'    ')GOTO986
      IF(IDERZ1(1).EQ.'-   '.AND.IDERZ2(1).EQ.'    '.AND.
     1   IDERZ1(2).EQ.'+   '.AND.IDERZ2(2).EQ.'    ')GOTO988
      IF(IDERZ1(1).EQ.'+   '.AND.IDERZ2(1).EQ.'    '.AND.
     1   IDERZ1(2).EQ.'-   '.AND.IDERZ2(2).EQ.'    ')GOTO988
      GOTO990
  986 CONTINUE
      I2=0
      DO987I=3,NCDZ
      I2=I2+1
      IDERZ1(I2)=IDERZ1(I)
      IDERZ2(I2)=IDERZ2(I)
  987 CONTINUE
      GOTO990
  988 CONTINUE
      I2=1
      IDERZ1(I2)='-   '
      IDERZ2(I2)='    '
      DO989I=3,NCDZ
      I2=I2+1
      IDERZ1(I2)=IDERZ1(I)
      IDERZ2(I2)=IDERZ2(I)
  989 CONTINUE
  990 CONTINUE
      NCDZ=I2
C
      GOTO8000
C
C               *********************************
C               **  STEP 4--                   **
C               **  SEARCH FOR **  --          **
C               **  THIS WILL INDICATE AN      **
C               **  EXPONENTIATION OPERATION.  **
C               *********************************
C
 3000 CONTINUE
C
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO3300I=1,NCFZ
      I2=I
      IF(IFUNZ1(I).EQ.'**  '.AND.IFUNZ2(I).EQ.'    ')GOTO5000
 3300 CONTINUE
C
C               ********************************************
C               **  STEP 5--                              **
C               **  TREAT THE LONE VARIABLE (ETC.) CASE.  **
C               ********************************************
C
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      I1=0
      I2=0
      I1=I1+1
      IF(IFUNZ1(I1).EQ.'-   '.AND.IFUNZ2(I1).EQ.'    ')GOTO4100
      IF(IFUNZ1(I1).EQ.'+   '.AND.IFUNZ2(I1).EQ.'    ')GOTO4150
      GOTO4200
C
 4100 CONTINUE
      I2=I2+1
      IDERZ1(I2)=IFUNZ1(I1)
      IDERZ2(I2)=IFUNZ2(I1)
 4150 CONTINUE
      I1=I1+1
      GOTO4200
C
 4200 CONTINUE
      IF(IFUNZ1(I1).EQ.'$   '.AND.IFUNZ2(I1).EQ.'    ')GOTO4300
      GOTO4400
C
 4300 CONTINUE
      I2=I2+1
      IDERZ1(I2)='%   '
      IDERZ2(I2)='    '
      I1=I1+1
      I2=I2+1
      IDERZ1(I2)=IFUNZ1(I1)
      IDERZ2(I2)=IFUNZ2(I1)
      GOTO4900
C
 4400 CONTINUE
      IF(IFUNZ1(I1).EQ.'&   '.AND.IFUNZ2(I1).EQ.'    ')GOTO4500
      GOTO4600
C
 4500 CONTINUE
      I2=1
      IDERZ1(I2)='0   '
      IDERZ2(I2)='    '
      GOTO4900
C
 4600 CONTINUE
CCCCC IH1=IFUNZ1(I1)
CCCCC IH2=IFUNZ2(I1)
CCCCC IF(NUMPAR.LE.0)GOTO4690
CCCCC DO4610I=1,NUMPAR
CCCCC IF(IH1.EQ.IPARN1(I).AND.IH2.EQ.IPARN2(I))GOTO4620
C4610 CONTINUE
CCCCC GOTO4690
C4620 CONTINUE
CCCCC I2=1
CCCCC IDERZ1(I2)='0   '
CCCCC IDERZ2(I2)='    '
CCCCC GOTO4900
C4690 CONTINUE
C
      IH1=IFUNZ1(I1)
      IH2=IFUNZ2(I1)
      IF(NUMVAR.LE.0)GOTO4790
      DO4710I=1,NUMVAR
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV3')
     1WRITE(ICOUT,4711)IH1,IH2,IVARN1(I),IVARN2(I)
 4711 FORMAT('IH1,IH2,IVARN1(I),IVARN2(I) = ',A4,2X,A4,2X,A4,2X,A4)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV3')
     1CALL DPWRST('XXX','BUG ')
      IF(IH1.EQ.IVARN1(I).AND.IH2.EQ.IVARN2(I))GOTO4720
 4710 CONTINUE
      GOTO4780
 4720 CONTINUE
      I2=I2+1
      IDERZ1(I2)='1   '
      IDERZ2(I2)='    '
      GOTO4900
 4780 CONTINUE
      I2=I2+1
      IDERZ1(I2)='0   '
      IDERZ2(I2)='    '
      GOTO4900
 4790 CONTINUE
C
      WRITE(6,4801)
 4801 FORMAT('*****ERROR IN DERIV3--')
      WRITE(6,4802)
 4802 FORMAT('     ILLEGAL ELEMENT TYPE')
      WRITE(ICOUT,4803)NCFZ
 4803 FORMAT('NCFZ = ',I6)
      CALL DPWRST('XXX','BUG ')
      DO4806I=1,NCFZ
      WRITE(ICOUT,4807)I,IFUNZ1(I),IFUNZ2(I)
 4807 FORMAT('I,IFUNZ1(I),IFUNZ2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 4806 CONTINUE
      WRITE(ICOUT,4815)NCDZ
 4815 FORMAT('NCDZ = ',I6)
      CALL DPWRST('XXX','BUG ')
      DO4816I=1,NCDZ
      WRITE(ICOUT,4817)I,IDERZ1(I),IDERZ2(I)
 4817 FORMAT('I,IDERZ1(I),IDERZ2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 4816 CONTINUE
      WRITE(ICOUT,4821)NUMPAR
 4821 FORMAT('NUMPAR = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO4822I=1,NUMPAR
      WRITE(ICOUT,4823)I,IPARN1(I),IPARN2(I)
 4823 FORMAT('I,IPARN1(I),IPARN2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 4822 CONTINUE
      WRITE(ICOUT,4831)NUMVAR
 4831 FORMAT('NUMVAR = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO4832I=1,NUMVAR
      WRITE(ICOUT,4833)I,IVARN1(I),IVARN2(I)
 4833 FORMAT('I,IVARN1(I),IVARN2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 4832 CONTINUE
      IERROR='YES'
      GOTO9000
C
 4900 CONTINUE
      NCDZ=I2
      GOTO8000
C
C               ***********************************
C               **  STEP 6--                     **
C               **  TREAT THE EXPONENTIAL CASE.  **
C               ***********************************
C
 5000 CONTINUE
C
      ISTEPN='6'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      I1=0
      I1=I1+1
      IF(IFUNZ1(I1).EQ.'+   '.AND.IFUNZ2(I1).EQ.'    ')GOTO5100
      IF(IFUNZ1(I1).EQ.'-   '.AND.IFUNZ2(I1).EQ.'    ')GOTO5100
      GOTO5150
C
 5100 CONTINUE
      ISIGN1=IFUNZ1(I1)
      ISIGN2=IFUNZ2(I1)
      I1=I1+1
      GOTO5200
C
 5150 CONTINUE
      ISIGN1='+   '
      ISIGN2='    '
      GOTO5200
C
 5200 CONTINUE
      IF(IFUNZ1(I1).EQ.'$   '.AND.IFUNZ2(I1).EQ.'    ')GOTO5300
      GOTO5400
C
 5300 CONTINUE
      IMAN11=IFUNZ1(I1)
      IMAN12=IFUNZ2(I1)
      I1=I1+1
      IMAN21=IFUNZ1(I1)
      IMAN22=IFUNZ2(I1)
      IMANTT='EXP '
      GOTO5900
C
 5400 CONTINUE
      IF(IFUNZ1(I1).EQ.'&   '.AND.IFUNZ2(I1).EQ.'    ')GOTO5500
      GOTO5600
C
 5500 CONTINUE
      IMAN11=IFUNZ1(I1)
      IMAN12=IFUNZ2(I1)
      I1=I1+1
      IMAN21=IFUNZ1(I1)
      IMAN22=IFUNZ2(I1)
      IMANTT='CON '
      GOTO5900
C
 5600 CONTINUE
      IH1=IFUNZ1(I1)
      IH2=IFUNZ2(I1)
      IF(NUMPAR.LE.0)GOTO5690
      DO5610I=1,NUMPAR
      IF(IH1.EQ.IPARN1(I).AND.IH2.EQ.IPARN2(I))GOTO5620
 5610 CONTINUE
      GOTO5690
 5620 CONTINUE
      IMAN11=IFUNZ1(I1)
      IMAN12=IFUNZ2(I1)
      IMANTT='PAR '
      GOTO5900
 5690 CONTINUE
C
      IH1=IFUNZ1(I1)
      IH2=IFUNZ2(I1)
      IF(NUMVAR.LE.0)GOTO5790
      DO5710I=1,NUMVAR
      IF(IH1.EQ.IVARN1(I).AND.IH2.EQ.IVARN2(I))GOTO5720
 5710 CONTINUE
      GOTO5790
 5720 CONTINUE
      IDERZ1(I2)='1   '
      IDERZ2(I2)='    '
      IMAN11=IFUNZ1(I1)
      IMAN12=IFUNZ2(I1)
      IMANTT='VAR '
      GOTO5900
 5790 CONTINUE
C
      WRITE(6,5801)
 5801 FORMAT('*****ERROR IN DERIV3--')
      WRITE(6,5802)
 5802 FORMAT('     ILLEGAL MANTISSA TYPE')
      DO5806I=1,NCFZ
      WRITE(ICOUT,5807)I,IFUNZ1(I),IFUNZ2(I)
 5807 FORMAT('I,IFUNZ1(I),IFUNZ2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 5806 CONTINUE
      WRITE(ICOUT,5815)NCDZ
 5815 FORMAT('NCDZ = ',I6)
      CALL DPWRST('XXX','BUG ')
      DO5816I=1,NCDZ
      WRITE(ICOUT,5817)I,IDERZ1(I),IDERZ2(I)
 5817 FORMAT('I,IDERZ1(I),IDERZ2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 5816 CONTINUE
      IERROR='YES'
      GOTO9000
C
 5900 CONTINUE
C
      I1=I1+1
      IF(IFUNZ1(I1).EQ.'**  '.AND.IFUNZ2(I1).EQ.'    ')GOTO6100
C
      WRITE(6,6001)
 6001 FORMAT('*****ERROR IN DERIV3--')
      WRITE(6,6002)
 6002 FORMAT('     ** NOT ENCOUNTERED,')
      WRITE(ICOUT,6003)
 6003 FORMAT('     WHERE IT SHOULD HAVE BEEN.')
      CALL DPWRST('XXX','BUG ')
      DO6006I=1,NCFZ
      WRITE(ICOUT,6007)I,IFUNZ1(I),IFUNZ2(I)
 6007 FORMAT('I,IFUNZ1(I),IFUNZ2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 6006 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,6015)NCDZ
 6015 FORMAT('NCDZ = ',I6)
      CALL DPWRST('XXX','BUG ')
      DO6016I=1,NCDZ
      WRITE(ICOUT,6017)I,IDERZ1(I),IDERZ2(I)
 6017 FORMAT('I,IDERZ1(I),IDERZ2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 6016 CONTINUE
      GOTO9000
C
 6100 CONTINUE
      I1=I1+1
      GOTO6200
C
 6200 CONTINUE
      IF(IFUNZ1(I1).EQ.'$   '.AND.IFUNZ2(I1).EQ.'    ')GOTO6300
      GOTO6400
C
 6300 CONTINUE
      IEXP11=IFUNZ1(I1)
      IEXP12=IFUNZ2(I1)
      I1=I1+1
      IEXP21=IFUNZ1(I1)
      IEXP22=IFUNZ2(I1)
      IEXPT='EXP '
      GOTO6900
C
 6400 CONTINUE
      IF(IFUNZ1(I1).EQ.'&   '.AND.IFUNZ2(I1).EQ.'    ')GOTO6500
      GOTO6600
C
 6500 CONTINUE
      IEXP11=IFUNZ1(I1)
      IEXP12=IFUNZ2(I1)
      I1=I1+1
      IEXP21=IFUNZ1(I1)
      IEXP22=IFUNZ2(I1)
      IEXPT='CON '
      GOTO6900
C
 6600 CONTINUE
      IH1=IFUNZ1(I1)
      IH2=IFUNZ2(I1)
      IF(NUMPAR.LE.0)GOTO6690
      DO6610I=1,NUMPAR
      IF(IH1.EQ.IPARN1(I).AND.IH2.EQ.IPARN2(I))GOTO6620
 6610 CONTINUE
      GOTO6690
 6620 CONTINUE
      IEXP11=IFUNZ1(I1)
      IEXP12=IFUNZ2(I1)
      IEXPT='PAR '
      GOTO6900
 6690 CONTINUE
C
      IH1=IFUNZ1(I1)
      IH2=IFUNZ2(I1)
      IF(NUMVAR.LE.0)GOTO6790
      DO6710I=1,NUMVAR
      IF(IH1.EQ.IVARN1(I).AND.IH2.EQ.IVARN2(I))GOTO6720
 6710 CONTINUE
      GOTO6790
 6720 CONTINUE
      IDERZ1(I2)='1   '
      IDERZ2(I2)='    '
      IEXP11=IFUNZ1(I1)
      IEXP12=IFUNZ2(I1)
      IEXPT='VAR '
      GOTO6900
 6790 CONTINUE
C
      WRITE(6,6801)
 6801 FORMAT('*****ERROR IN DERIV3--')
      WRITE(6,6802)
 6802 FORMAT('     ILLEGAL EXPONENT TYPE')
      DO6805I=1,NCDZ
      WRITE(ICOUT,6806)I,IFUNZ1(I),IFUNZ2(I)
 6806 FORMAT('I,IFUNZ1(I),IFUNZ2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 6805 CONTINUE
      IERROR='YES'
      GOTO9000
C
 6900 CONTINUE
C
      IF((IMANTT.EQ.'CON '.OR.IMANTT.EQ.'PAR ').AND.
     1   (IEXPT.EQ.'CON '.OR.IEXPT.EQ.'PAR '))GOTO7010
      IF((IMANTT.EQ.'VAR '.OR.IMANTT.EQ.'EXP ').AND.
     1   (IEXPT.EQ.'CON '.OR.IEXPT.EQ.'PAR '))GOTO7020
      IF((IMANTT.EQ.'CON '.OR.IMANTT.EQ.'PAR ').AND.
     1   (IEXPT.EQ.'VAR '.OR.IEXPT.EQ.'EXP '))GOTO7030
      IF((IMANTT.EQ.'VAR '.OR.IMANTT.EQ.'EXP ').AND.
     1   (IEXPT.EQ.'VAR '.OR.IEXPT.EQ.'EXP '))GOTO7040
C
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
      DO7075I=1,NCDZ
      WRITE(ICOUT,7076)I,IFUNZ1(I),IFUNZ2(I)
 7076 FORMAT('I,IFUNZ1(I),IFUNZ2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 7075 CONTINUE
      IERROR='YES'
      GOTO9000
C
C               ****************************
C               **  STEP 7.1--            **
C               **  TREAT THE A**B CASE.  **
C               ****************************
 7010 CONTINUE
C
      ISTEPN='7.1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      I2=1
      IDERZ1(I2)='0   '
      IDERZ2(I2)='    '
      GOTO7900
C
C               ****************************
C               **  STEP 7.2--            **
C               **  TREAT THE X**A CASE.  **
C               ****************************
C
 7020 CONTINUE
C
      ISTEPN='7.2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
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
      IF(IMANTT.EQ.'EXP ')GOTO7025
      GOTO7029
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
      GOTO7900
C
C               ****************************
C               **  STEP 7.3--            **
C               **  TREAT THE A**X CASE.  **
C               ****************************
C
 7030 CONTINUE
C
      ISTEPN='7.3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
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
      IF(IEXPT.EQ.'EXP ')GOTO7035
      GOTO7039
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
      GOTO7900
C
C               ****************************
C               **  STEP 7.4--            **
C               **  TREAT THE U**V CASE.  **
C               ****************************
C
 7040 CONTINUE
C
      ISTEPN='7.4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
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
      IF(IMANTT.EQ.'EXP ')GOTO7041
      GOTO7042
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
C
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
      IF(IEXPT.EQ.'EXP ')GOTO7043
      GOTO7044
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
      GOTO7900
C
 7900 CONTINUE
      NCDZ=I2
      GOTO8000
C
C               ************************************
C               **  STEP 8--                      **
C               **  COPY THE EXPRESSION           **
C               **  IN THE VECTOR IDERZ1(.)        **
C               **  INTO ROW IROW2 OF IDER21(.,.)  **
C               ************************************
C
 8000 CONTINUE
C
      ISTEPN='8'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IDERZ1(1).EQ.'+   '.AND.IDERZ2(1).EQ.'    ')GOTO8010
      IF(IDERZ1(1).EQ.'-   '.AND.IDERZ2(1).EQ.'    ')GOTO8010
      GOTO8090
 8010 CONTINUE
      IHOL11='(   '
      IHOL12='    '
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV3')
     1WRITE(ICOUT,8011)NCDZ
 8011 FORMAT('NCDZ = ',I8)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV3')
     1CALL DPWRST('XXX','BUG ')
      DO8020I=1,NCDZ
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV3')
     1WRITE(ICOUT,8021)I,IDERZ1(I),IDERZ2(I)
 8021 FORMAT('I,IDERZ1(I),IDERZ2(I) = ',I8,2X,A4,2X,A4)
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV3')
     1CALL DPWRST('XXX','BUG ')
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
C
      NCD2(IROW2)=NCDZ
      DO8100I=1,NCDZ
      IDER21(IROW2,I)=IDERZ1(I)
      IDER22(IROW2,I)=IDERZ2(I)
 8100 CONTINUE
C
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'RIV3')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('AT THE END       OF DERIV3--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)NCD2(IROW2)
 9013 FORMAT('NCD2(IROW2) = ',I8)
      CALL DPWRST('XXX','BUG ')
      IMAX=NCD2(IROW2)
      DO9015I=1,IMAX
      WRITE(ICOUT,9016)I,IDER21(IROW2,I),IDER22(IROW2,I)
 9016 FORMAT('I,IDER21(IROW2,I),IDER22(IROW2,I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
      WRITE(ICOUT,9021)NUMPAR
 9021 FORMAT('NUMPAR = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO9022I=1,NUMPAR
      WRITE(ICOUT,9023)I,IPARN1(I),IPARN2(I)
 9023 FORMAT('I,IPARN1(I),IPARN2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9022 CONTINUE
      WRITE(ICOUT,9031)NUMVAR
 9031 FORMAT('NUMVAR = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO9032I=1,NUMVAR
      WRITE(ICOUT,9033)I,IVARN1(I),IVARN2(I)
 9033 FORMAT('I,IVARN1(I),IVARN2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9032 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DERIV4(IFUN21,IFUN22,NCF2,NFUN2,
     1                  IDER21,IDER22,NCD2,IOP2,IROW1,
     1                  IDER11,IDER12,NCD1,IBUGA3,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--DETERMINE THE DERIVATIVE OF
C              A MULTIPLICATIVE EXPRESSION
C              (= 1 FULL ADDITIVE COMPONENT)
C              (EXAMPLE, A*X/C*D**E*X)
C              BY COMBINING DERIVATIVES OF EACH
C              ELEMENTAL COMPONENT.
C
C              THE ENTIRE INPUT EXPRESSION IS LOCATED
C              IN ROW IROW1 OF IFUN11--
C              IT HAS LENGTH NF1
C              (THIS SUBROUTINE NEED NEVER SEE
C              THIS ENTIRE EXPRESSION.)
C
C              THE INPUT ELEMENTS OF THE
C              INPUT EXPRESSION ARE LOCATED
C              IN VARIOUS ROWS OF IFUN21.
C
C              THE INPUT DERIVATIVES OF THE
C              INPUT ELEMENTS ARE LOCATED
C              IN VARIOUS ROWS OF IDER21.
C
C              THE OUTPUT DERIVATIVE IS LOCATED
C              IN ROW IROW1 OF IFUN1--
C              IT HAS LENGTH NCD1.
C
C     INPUT  ARGUMENTS--IFUN21 = THE ARRAY WHOSE I-TH ROW
C                                IS THE I-TH
C                                MULTIPLICATIVE COMPONENT
C                                OF THE IROW1-TH (IROW1 FIXED)
C                                ADDITIVE COMPONENT
C                                (FIRST 4 CHARACTERS).
C                     --IFUN22 = THE ARRAY WHOSE I-TH ROW
C                                IS THE I-TH
C                                MULTIPLICATIVE COMPONENT
C                                OF THE IROW1-TH (IROW1 FIXED)
C                                ADDITIVE COMPONENT
C                                (NEXT  4 CHARACTERS).
C                     --NCF2   = AN INTEGER VECTOR
C                                WHOSE IROW1-TH ELEMENT
C                                IS THE LENGTH
C                                OF THE I-TH
C                                MULTIPLICATIVE COMPONENT
C                                OF THE IROW1-TH (IROW1 FIXED)
C                                ADDITIVE COMPONENT.
C                     --NFUN2  = THE NUMBER OF ROWS
C                                (= THE NUMBER OF MULTIPLICATIVE
C                                SUBSTRINGS OF THE IROW1-TH
C                                ADDITIVE COMPONENT)
C                                THAT IS
C                                IN THE ARRAY IFUN21(.,.)
C                     --IOP2   = A VECTOR OF OPERATIONS
C                                (BETWEEN ELEMENTS--* OR /.
C                     --IDER21  = THE ARRAY WHOSE I-TH ROW
C                                IS THE DERIVATIVE OF THE I-TH
C                                MULTIPLICATIVE COMPONENT
C                                OF THE IROW1-TH (IROW1 FIXED)
C                                (FIRST 4 CHARACTERS).
C                     --IDER22 = THE ARRAY WHOSE I-TH ROW
C                                IS THE DERIVATIVE OF THE I-TH
C                                MULTIPLICATIVE COMPONENT
C                                OF THE IROW1-TH (IROW1 FIXED)
C                                (NEXT  4 CHARACTERS).
C                     --NCD2   = AN INTEGER VECTOR
C                                WHOSE IROW1-TH ELEMENT
C                                IS THE LENGTH
C                                OF THE DERIVATIVE OF THE I-TH
C                                MULTIPLICATIVE COMPONENT
C                                OF THE IROW1-TH (IROW1 FIXED)
C                                ADDITIVE COMPONENT.
C                                WHOSE I-TH ELEMENT
C                                IS THE (TRAILING) OPERATION (* OR /)
C                                OF THE I-TH MULTIPLICATIVE SUBSTRING
C                                OF THE IROW1-TH ADDITIVE COMPONENT.
C                     --IROW1  = THE ROW NUMBER (IN IFUN1(.,.)) OF
C                                THE PARTICULAR
C                                ADDITIVE COMPONENT OF INTEREST.
C     OUTPUT ARGUMENTS--IDER11 = THE ARRAY WHOSE IROW1-TH ROW
C                                WILL BE THE DERIVATIVE OF THE
C                                IROW1-TH ADDITIVE STRING
C                                (FIRST 4 CHARACTERS).
C                     --IDER12 = THE ARRAY WHOSE IROW1-TH ROW
C                                WILL BE THE DERIVATIVE OF THE
C                                IROW1-TH ADDITIVE STRING
C                                (NEXT  4 CHARACTERS).
C                       NCD1   = AN INTEGER VECTOR
C                                WHOSE IROW1-TH ELEMENT
C                                WILL BE THE LENGTH OF THE IROW1-TH
C                                DERIVATIVE IN IDER11(.,.);
C                                THAT IS, NCD1(IROW1) = THE LENGTH OF THE
C                                DERIVATIVE OF INTEREST.
C     INTERNAL ARRAYS--
C                       IFUN21  = THE ARRAY WHOSE I-TH
C                                ROW WILL BE THE I-TH MULTIPLICATIVE
C                                SUBSTRING OF THE IROW1-TH
C                                ADDITIVE COMPONENT.
C                       NCF2   = AN INTEGER VECTOR
C                                WHOSE I-TH ELEMENT
C                                WILL BE THE LENGTH OF THE I-TH
C                                MULTIPLICATIVE SUBSTRING
C                                OF THE IROW1-TH ADDITIVE COMPONENT.
C
C     ORIGINAL VERSION--DECEMBER 2, 1978
C     UPDATED         --DECEMBER  1981.
C
C---------------------------------------------------------------------
C
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
C
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
CCCCC CHARACTER*4 IBUG1
CCCCC CHARACTER*4 IBUG2
CCCCC CHARACTER*4 IBUG3
C
      CHARACTER*4 IDER31
      CHARACTER*4 IDER32
C
      CHARACTER*4 IFUN31
      CHARACTER*4 IFUN32
C
      CHARACTER*4 IOP2
C
      DIMENSION IFUN21(20,80)
      DIMENSION IFUN22(20,80)
      DIMENSION NCF2(1)
      DIMENSION IDER21(20,80)
      DIMENSION IDER22(20,80)
      DIMENSION NCD2(1)
      DIMENSION IOP2(1)
C
      DIMENSION IDER11(20,80)
      DIMENSION IDER12(20,80)
      DIMENSION NCD1(1)
C
      DIMENSION IFUN31(2,80)
      DIMENSION IFUN32(2,80)
      DIMENSION NCF3(2)
      DIMENSION IDER31(2,80)
      DIMENSION IDER32(2,80)
      DIMENSION NCD3(2)
C
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----DATA STATEMENTS-----------------------------------------------------
C
CCCCC DATA IBUG1/'OFF'/
CCCCC DATA IBUG2/'OFF'/
CCCCC DATA IBUG3/'OFF'/
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DERI'
      ISUBN2='V4  '
      IERROR='NO'
C
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
        DO60I=1,NFUN2
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,61)I,NCF2(I),IOP2(I)
   61     FORMAT('I,NCF2(I) = ',2I8,2X,A4)
          CALL DPWRST('XXX','BUG ')
          DO65J=1,ITEMP
            WRITE(ICOUT,66)J,IFUN21(I,J),IFUN22(I,J)
   66       FORMAT('J,IFUN21(I,J),IFUN22(I,J) = ',I8,2(2X,A4))
            CALL DPWRST('XXX','BUG ')
   65     CONTINUE
   60   CONTINUE
C
        DO70I=1,NFUN2
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,71)I,NCD2(I)
   71     FORMAT('I,NCD2(I) = ',2I8)
          CALL DPWRST('XXX','BUG ')
          DO75J=1,ITEMP
            WRITE(ICOUT,76)J,IDER21(I,J),IDER22(I,J)
   76       FORMAT('J,IDER21(I,J),IDER22(I,J) = ',I8,2(2X,A4))
            CALL DPWRST('XXX','BUG ')
   75     CONTINUE
   70   CONTINUE
      ENDIF
C
C               ***********************************
C               **  STEP 1.1--                   **
C               **  FORM THE FIRST 2 FUNCTIONS.  **
C               ***********************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV4')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NFUN3=NFUN2
      IF(NFUN2.GE.1)GOTO1020
C
      WRITE(ICOUT,1011)
 1011 FORMAT('***** ERROR IN DERIV4--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1012)NFUN2
 1012 FORMAT('NFUN2 NON-POSITIVE. NFUN2 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
 1020 CONTINUE
      IROW3=1
      JMAX=NCF2(IROW3)
      K=0
      DO1050J=1,JMAX
      K=K+1
      IFUN31(1,K)=IFUN21(IROW3,J)
      IFUN32(1,K)=IFUN22(IROW3,J)
      IFUN31(2,K)=IFUN21(IROW3,J)
      IFUN32(2,K)=IFUN22(IROW3,J)
 1050 CONTINUE
      NCF3(1)=K
      NCF3(2)=K
C
C               *************************************
C               **  STEP 1.2--                     **
C               **  FORM THE FIRST 2 DERIVATIVES.  **
C               *************************************
C
      ISTEPN='1.2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV4')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NFUN2.GE.1)GOTO2020
C
      WRITE(ICOUT,2001)
 2001 FORMAT('***** ERROR IN DERIV4--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2002)NFUN2
 2002 FORMAT('NFUN2 NON-POSITIVE. NFUN2 = ',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
 2020 CONTINUE
      IROW3=1
      JMAX=NCD2(IROW3)
      K=0
      DO2030J=1,JMAX
      K=K+1
      IDER31(1,K)=IDER21(IROW3,J)
      IDER32(1,K)=IDER22(IROW3,J)
      IDER31(2,K)=IDER21(IROW3,J)
      IDER32(2,K)=IDER22(IROW3,J)
 2030 CONTINUE
      NCD3(1)=K
      NCD3(2)=K
C
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'RIV4')GOTO2090
      WRITE(ICOUT,2006)
 2006 FORMAT('***** IN THE MIDDLE OF DERIV4--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2007)IROW3,NCF2(IROW3),NCD2(IROW3)
 2007 FORMAT('IROW3, NCF2(IROW3), NCD2(IROW3) = ',3I6)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2008)IROW3,NCF3(2),NCD3(2)
 2008 FORMAT('IROW3, NCF3(2), NCD3(2) = ',3I6)
      CALL DPWRST('XXX','BUG ')
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IMAX=NCF2(IROW3)
      DO2040I=1,IMAX
      WRITE(ICOUT,2045)I,IFUN21(IROW3,I),IFUN22(IROW3,I)
 2045 FORMAT('I,IFUN21(IROW3,I),IFUN22(IROW3,I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 2040 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IMAX=NCD2(IROW3)
      DO2050I=1,IMAX
      WRITE(ICOUT,2055)I,IDER21(IROW3,I),IDER22(IROW3,I)
 2055 FORMAT('I,IDER21(IROW3,I),IDER22(IROW3,I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 2050 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IMAX=NCF3(2)
      DO2060I=1,IMAX
      WRITE(ICOUT,2065)I,IFUN31(IROW3,I),IFUN32(IROW3,I)
 2065 FORMAT('I,IFUN31(IROW3,I),IFUN32(IROW3,I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 2060 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IMAX=NCD3(2)
      DO2070I=1,IMAX
      WRITE(ICOUT,2075)I,IDER31(IROW3,I),IDER32(IROW3,I)
 2075 FORMAT('I,IDER31(IROW3,I),IDER32(IROW3,I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 2070 CONTINUE
C
 2090 CONTINUE
      IF(NFUN2.EQ.1)GOTO5000
C
      IF(NFUN3.LT.2)GOTO2900
      DO2100IROW3=2,NFUN3
C
C               ***********************************************
C               **  STEP 2.1--                               **
C               **  MOVE THE CUMULATIVE FUNCTION             **
C               **  IN THE SECOND ROW OF IFUN31(.)            **
C               **  TO THE FIRST ROW OF IFUN31(.).            **
C               **  MOVE THE CUMULATIVE FUNCTION DERIVATIVE  **
C               **  IN THE SECOND ROW OF OF IDER31(.)         **
C               **  TO THE FIRST ROW OF IDER31(.).            **
C               ***********************************************
C
      ISTEPN='2.1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV4')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      JMAX=NCF3(2)
      DO1110J=1,JMAX
      IFUN31(1,J)=IFUN31(2,J)
      IFUN32(1,J)=IFUN32(2,J)
 1110 CONTINUE
      NCF3(1)=NCF3(2)
C
      JMAX=NCD3(2)
      DO1120J=1,JMAX
      IDER31(1,J)=IDER31(2,J)
      IDER32(1,J)=IDER32(2,J)
 1120 CONTINUE
      NCD3(1)=NCD3(2)
C
C               ******************************************************
C               **  STEP 2.2--                                      **
C               **  DEFINE THE FUNCTIONS (IN IFUN31(.,.))            **
C               **  WHICH COMBINE ITERATIVELY AND SEQUENTIALLY      **
C               **  EACH OF THE INDIVIDUAL MULTIPLICATIVE           **
C               **  COMPONENTS.                                     **
C               ******************************************************
C
      ISTEPN='2.2'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV4')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IROW3M=IROW3-1
      IF(IOP2(IROW3M).EQ.'*')GOTO1200
      IF(IOP2(IROW3M).EQ.'/')GOTO1200
C
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
      GOTO9000
C
C     TREAT EITHER THE * CASE OR THE / CASE.
C
 1200 CONTINUE
C
      K=0
      JMAX=NCF3(1)
      DO1210J=1,JMAX
      K=K+1
      IFUN31(2,K)=IFUN31(1,J)
      IFUN32(2,K)=IFUN32(1,J)
 1210 CONTINUE
C
      K=K+1
      IFUN31(2,K)=IOP2(IROW3M)
      IFUN32(2,K)='    '
C
      JMAX=NCF2(IROW3)
      DO1215J=1,JMAX
      K=K+1
      IFUN31(2,K)=IFUN21(IROW3,J)
      IFUN32(2,K)=IFUN22(IROW3,J)
 1215 CONTINUE
C
      NCF3(2)=K
      NFUN3=NFUN2
C
C               ********************************************************
C               **  STEP 2.3--                                        **
C               **  ITERATIVELY COMBINE IN SEQUENCE DERIVATIVES       **
C               **  FOR THE MULTIPLICATIVE SUBSTRINGS.                **
C               ********************************************************
C
      ISTEPN='2.3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV4')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IROW3M=IROW3-1
      IF(IOP2(IROW3M).EQ.'*')GOTO2200
      IF(IOP2(IROW3M).EQ.'/')GOTO2300
C
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
      GOTO9000
C
C               *******************************
C               **  STEP 2.4--               **
C               **  TREAT THE PRODUCT CASE.  **
C               *******************************
C
 2200 CONTINUE
C
      ISTEPN='2.4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV4')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NCD3(1).EQ.1.AND.
     1IDER31(1,1).EQ.'0'.AND.IDER32(1,1).EQ.' '.AND.
     1NCD2(IROW3).EQ.1.AND.
     1IDER21(IROW3,1).EQ.'0'.AND.IDER22(IROW3,1).EQ.' ')GOTO2202
      GOTO2209
 2202 CONTINUE
      K=1
      IDER31(2,K)='0'
      IDER32(2,K)=' '
      GOTO2249
 2209 CONTINUE
C
      K=0
      K=K+1
      IDER31(2,K)='('
      IDER32(2,K)=' '
C
      IF(NCD2(IROW3).EQ.1.AND.
     1IDER21(IROW3,1).EQ.'0'.AND.IDER22(IROW3,1).EQ.' ')GOTO2222
C
      JMAX=NCF3(1)
      DO2210J=1,JMAX
      K=K+1
      IDER31(2,K)=IFUN31(1,J)
      IDER32(2,K)=IFUN32(1,J)
 2210 CONTINUE
C
      IF(NCD2(IROW3).EQ.1.AND.
     1IDER21(IROW3,1).EQ.'0'.AND.IDER22(IROW3,1).EQ.' ')GOTO2222
C
      K=K+1
      IDER31(2,K)='*'
      IDER32(2,K)=' '
C
      JMAX=NCD2(IROW3)
      DO2220J=1,JMAX
      K=K+1
      IDER31(2,K)=IDER21(IROW3,J)
      IDER32(2,K)=IDER22(IROW3,J)
 2220 CONTINUE
 2222 CONTINUE
C
      IF(NCD3(1).EQ.1.AND.
     1IDER31(1,1).EQ.'0'.AND.IDER32(1,1).EQ.' ')GOTO2242
C
      K=K+1
      IDER31(2,K)='+'
      IDER32(2,K)=' '
C
      JMAX=NCF2(IROW3)
      DO2230J=1,JMAX
      K=K+1
      IDER31(2,K)=IFUN21(IROW3,J)
      IDER32(2,K)=IFUN22(IROW3,J)
 2230 CONTINUE
C
      IF(NCD3(1).EQ.1.AND.
     1IDER31(1,1).EQ.'1'.AND.IDER32(1,1).EQ.' ')GOTO2242
C
      K=K+1
      IDER31(2,K)='*'
      IDER32(2,K)=' '
C
      JMAX=NCD3(1)
      DO2240J=1,JMAX
      K=K+1
      IDER31(2,K)=IDER31(1,J)
      IDER32(2,K)=IDER32(1,J)
 2240 CONTINUE
 2242 CONTINUE
C
      K=K+1
      IDER31(2,K)=')'
      IDER32(2,K)=' '
C
 2249 CONTINUE
      NCD3(2)=K
      GOTO2400
C
C               ********************************
C               **  STEP 2.5--                **
C               **  TREAT THE DIVISION CASE.  **
C               ********************************
C
 2300 CONTINUE
C
      ISTEPN='2.5'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV4')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NCD3(1).EQ.1.AND.
     1IDER31(1,1).EQ.'0'.AND.IDER32(1,1).EQ.' '.AND.
     1NCD2(IROW3).EQ.1.AND.
     1IDER21(IROW3,1).EQ.'0'.AND.IDER22(IROW3,1).EQ.' ')GOTO2302
      GOTO2309
 2302 CONTINUE
      K=1
      IDER31(2,K)='0'
      IDER32(2,K)=' '
      GOTO2349
 2309 CONTINUE
C
      K=0
      K=K+1
      IDER31(2,K)='('
      IDER32(2,K)=' '
C
      K=K+1
      IDER31(2,K)='('
      IDER32(2,K)=' '
C
      IF(NCD3(1).EQ.1.AND.
     1IDER31(1,1).EQ.'0'.AND.IDER32(1,1).EQ.' ')GOTO2322
C
      JMAX=NCF2(IROW3)
      DO2310J=1,JMAX
      K=K+1
      IDER31(2,K)=IFUN21(IROW3,J)
      IDER32(2,K)=IFUN22(IROW3,J)
 2310 CONTINUE
C
      IF(NCD3(1).EQ.1.AND.
     1IDER31(1,1).EQ.'1'.AND.IDER32(1,1).EQ.' ')GOTO2322
C
      K=K+1
      IDER31(2,K)='*'
      IDER32(2,K)=' '
C
      JMAX=NCD3(1)
      DO2320J=1,JMAX
      K=K+1
      IDER31(2,K)=IDER31(1,J)
      IDER32(2,K)=IDER32(1,J)
 2320 CONTINUE
 2322 CONTINUE
C
      IF(NCD2(IROW3).EQ.1.AND.
     1IDER21(IROW3,1).EQ.'0'.AND.IDER22 (IROW3,1).EQ.' ')GOTO2342
C
      K=K+1
      IDER31(2,K)='-'
      IDER32(2,K)=' '
C
      JMAX=NCF3(1)
      DO2330J=1,JMAX
      K=K+1
      IDER31(2,K)=IFUN31(1,J)
      IDER32(2,K)=IFUN32(1,J)
 2330 CONTINUE
C
      IF(NCD2(IROW3).EQ.1.AND.
     1IDER21(IROW3,1).EQ.'1'.AND.IDER22 (IROW3,1).EQ.' ')GOTO2342
C
      K=K+1
      IDER31(2,K)='*'
      IDER32(2,K)=' '
C
      JMAX=NCD2(IROW3)
      DO2340J=1,JMAX
      K=K+1
      IDER31(2,K)=IDER21(IROW3,J)
      IDER32(2,K)=IDER22(IROW3,J)
 2340 CONTINUE
 2342 CONTINUE
C
      K=K+1
      IDER31(2,K)=')'
      IDER32(2,K)=' '
C
      K=K+1
      IDER31(2,K)='/'
      IDER32(2,K)=' '
C
      K=K+1
      IDER31(2,K)='('
      IDER32(2,K)=' '
C
      JMAX=NCF2(IROW3)
      DO2350J=1,JMAX
      K=K+1
      IDER31(2,K)=IFUN21(IROW3,J)
      IDER32(2,K)=IFUN22(IROW3,J)
 2350 CONTINUE
C
      K=K+1
      IDER31(2,K)='**'
      IDER32(2,K)='  '
      K=K+1
      IDER31(2,K)='2'
      IDER32(2,K)=' '
      K=K+1
      IDER31(2,K)=')'
      IDER32(2,K)=' '
C
      K=K+1
      IDER31(2,K)=')'
      IDER32(2,K)=' '
C
 2349 CONTINUE
      NCD3(2)=K
      GOTO2400
C
 2400 CONTINUE
C
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'RIV4')GOTO2100
      WRITE(ICOUT,2401)
 2401 FORMAT('***** IN THE MIDDLE OF DERIV4--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2407)IROW3,NCF2(IROW3),NCD2(IROW3)
 2407 FORMAT('IROW3, NCF2(IROW3), NCD2(IROW3) = ',3I6)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2408)IROW3,NCF3(2),NCD3(2)
 2408 FORMAT('IROW3, NCF3(2), NCD3(2) = ',3I6)
      CALL DPWRST('XXX','BUG ')
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IMAX=NCF2(IROW3)
      DO2440I=1,IMAX
      WRITE(ICOUT,2445)I,IFUN21(IROW3,I),IFUN22(IROW3,I)
 2445 FORMAT('I,IFUN21(IROW3,I),IFUN22(IROW3,I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 2440 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IMAX=NCD2(IROW3)
      DO2450I=1,IMAX
      WRITE(ICOUT,2455)I,IDER21(IROW3,I),IDER22(IROW3,I)
 2455 FORMAT('I,IDER21(IROW3,I),IDER22(IROW3,I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 2450 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IMAX=NCF3(2)
      DO2460I=1,IMAX
      WRITE(ICOUT,2465)I,IFUN31(IROW3,I),IFUN32(IROW3,I)
 2465 FORMAT('I,IFUN31(IROW3,I),IFUN32(IROW3,I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 2460 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IMAX=NCD3(2)
      DO2470I=1,IMAX
      WRITE(ICOUT,2475)I,IDER31(IROW3,I),IDER32(IROW3,I)
 2475 FORMAT('I,IDER31(IROW3,I),IDER32(IROW3,I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 2470 CONTINUE
C
 2100 CONTINUE
 2900 CONTINUE
C
C               ****************************************
C               **  STEP 3--                          **
C               **  EXAMINE ROW 2     OF IDER31(.,.).  **
C               **  CHANGE ALL (+ TO (                **
C               ****************************************
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV4')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      JMAX=NCD3(2)
      IF(JMAX.LE.0)GOTO3190
      K=0
      DO3100J=1,JMAX
      IF(J.EQ.1)GOTO3110
      JM1=J-1
      IF(IDER31(2,JM1).EQ.'('.AND.IDER32(2,JM1).EQ.' '.AND.
     1IDER31(2,J).EQ.'+'.AND.IDER32(2,J).EQ.' ')GOTO3100
 3110 CONTINUE
      K=K+1
      IDER31(2,K)=IDER31(2,J)
      IDER32(2,K)=IDER32(2,J)
 3100 CONTINUE
      NCD3(2)=K
 3190 CONTINUE
C
C               *******************************************
C               **  STEP 4--                             **
C               **  COPY OVER THE DERIVATIVE             **
C               **  FROM ROW 2     OF IFUN31(.,.)         **
C               **  TO ROW IROW1 (FIXED) OF IFUN1(.,.).  **
C               *******************************************
 5000 CONTINUE
C
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'RIV4')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      JMAX=NCD3(2)
      DO5100J=1,JMAX
      IDER11(IROW1,J)=IDER31(2,J)
      IDER12(IROW1,J)=IDER32(2,J)
 5100 CONTINUE
      NCD1(IROW1)=NCD3(2)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'OFF'.AND.ISUBRO.NE.'RIV4')GOTO9090
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
      DO9020J=1,ITEMP
      WRITE(ICOUT,9021)J,IDER11(IROW1,J),IDER12(IROW1,J)
 9021 FORMAT('J,IDER11(IROW1,J),IDER12(IROW1,J) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9020 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DERIVC(MODEL,NUMCHA,PARAM,IPARN,IPARN2,NUMPV,
     1IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,
     1IVARN,IVARN2,NUMVAR,X0,XDER,IBUGA3,IBUGCO,IBUGEV,IERROR)
C
C     PURPOSE--COMPUTE THE DERIVATIVE OF A FUNCTION
C              AT THE POINT X0.
C     ORIGINAL VERSION--NOVEMBER  1978.
C     UPDATED         --FEBRUARY  1979.
C     UPDATED         --JANUARY   1982.
C
C---------------------------------------------------------------------
C
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
C
      CHARACTER*4 IH
      CHARACTER*4 IH2
C
      DIMENSION MODEL(*)
      DIMENSION PARAM(*)
      DIMENSION IPARN(*)
      DIMENSION IPARN2(*)
      DIMENSION IVARN(*)
      DIMENSION IVARN2(*)
      DIMENSION ILOCV(10)
C
      DIMENSION ITYPEH(*)
      DIMENSION IW21HO(*)
      DIMENSION IW22HO(*)
      DIMENSION W2HOLD(*)
C
C-----COMMON VARIABLES (GENERAL)-----------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      CUTOFF=0.001
      ACCUR=0.0000001
      MAXIT=10
      IPASS=2
C
      J2=0
      H=0.0
      X0MH=0.0
      X0PH=0.0
      WIDTH=0.0
      XDER2=0.0
      RATIO2=0.0
C
      IF(IBUGA3.EQ.'OFF')GOTO90
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
      DO55I=1,NUMPV
      WRITE(ICOUT,56)I,IPARN(I),IPARN2(I),PARAM(I)
   56 FORMAT('I,IPARN(I),IPARN2(I),PARAM(I) = ',
     1I8,2X,A4,2X,A4,E15.7)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
      WRITE(ICOUT,57)IANGLU
   57 FORMAT('IANGLU = ',A4)
      CALL DPWRST('XXX','BUG ')
      DO65I=1,NUMVAR
      WRITE(ICOUT,66)I,IVARN(I),IVARN2(I)
   66 FORMAT('I,IVARN(I),IVARN2(I) = ',I8,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
      WRITE(ICOUT,68)X0
   68 FORMAT('X0 = ',E15.8)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               ***************************************************
C               **  STEP 1--                                     **
C               **  DETERMINE THE LOCATIONS (IN THE LIST IPARN)  **
C               **  OF THE VARIABLES OF DIFFERENTIATION.         **
C               ***************************************************
C
      DO100I=1,NUMVAR
      IH=IVARN(I)
      IH2=IVARN2(I)
      DO200J=1,NUMPV
      J2=J
      IF(IH.EQ.IPARN(J).AND.IH2.EQ.IPARN2(J))GOTO210
  200 CONTINUE
  210 CONTINUE
      ILOCV(I)=J2
  100 CONTINUE
C
C               ************************************************
C               **  STEP 3--                                  **
C               **  STEP THROUGH DIFFERENT WIDTHS             **
C               **  (HALVING THE WIDTHS FOR EACH ITERATION).  **
C               ************************************************
C
      IF(X0.LE.CUTOFF)H=CUTOFF
      IF(X0.GT.CUTOFF)H=X0*1.01
      DO3100NUMIT=1,MAXIT
C
C               ********************************************************
C               **  STEP 4--                                          **
C               **  FOR A GIVEN WIDTH (= 2*H), COMPUTE THE DIFFERENCE **
C               **  FORMULA D = (Y(X0+H) - Y(X0-H))/(2*H)             **
C               ********************************************************
C
      IF(NUMIT.GE.2)H=H/2.0
      X0MH=X0-H
      X0PH=X0+H
C
      X=X0MH
      DO3410K=1,NUMVAR
      JLOC=ILOCV(K)
      PARAM(JLOC)=X
 3410 CONTINUE
      CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM,IPARN,IPARN2,NUMPV,
     1IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,Y0MH,
     1IBUGCO,IBUGEV,IERROR)
C
      X=X0PH
      DO3420K=1,NUMVAR
      JLOC=ILOCV(K)
      PARAM(JLOC)=X
 3420 CONTINUE
      CALL COMPIM(MODEL,NUMCHA,IPASS,PARAM,IPARN,IPARN2,NUMPV,
     1IANGLU,ITYPEH,IW21HO,IW22HO,W2HOLD,NWHOLD,Y0PH,
     1IBUGCO,IBUGEV,IERROR)
C
      IF(IBUGA3.EQ.'ON')WRITE(ICOUT,3402)X,Y0MH,Y0PH
 3402 FORMAT('X,Y0MH,Y0PH = ',3E15.8)
      IF(IBUGA3.EQ.'ON')CALL DPWRST('XXX','BUG ')
C
      WIDTH=2.0*H
      XDER=(Y0PH-Y0MH)/WIDTH
C
C               **************************************
C               **  STEP 5--                        **
C               **  WRITE OUT THE DERIVATIVE VALUE  **
C               **************************************
C
      WRITE(ICOUT,3103)WIDTH,XDER
 3103 FORMAT(E15.8,'* ',E15.8)
      CALL DPWRST('XXX','BUG ')
C
      IF(NUMIT.EQ.1)GOTO3195
      ABSXDE=ABS(XDER)
C
      DIFF2=ABS(XDER-XDER2)
      IF(ABSXDE.LE.CUTOFF.AND.DIFF2.LE.ACCUR)GOTO3170
      IF(ABSXDE.LE.CUTOFF.AND.DIFF2.GT.ACCUR)GOTO3190
      RATIO2=ABS(DIFF2/XDER)
      IF(ABSXDE.GT.CUTOFF.AND.RATIO2.LE.ACCUR)GOTO3170
      IF(ABSXDE.GT.CUTOFF.AND.RATIO2.GT.ACCUR)GOTO3190
C
 3170 CONTINUE
      GOTO3500
 3190 CONTINUE
      IF(IBUGA3.EQ.'ON')WRITE(ICOUT,3191)DIFF2,RATIO2,ABSXDE
 3191 FORMAT('DIFF2,RATIO2,ABSXDE = ',3E15.8)
      IF(IBUGA3.EQ.'ON')CALL DPWRST('XXX','BUG ')
CCCCC XDER3=XDER2
 3195 CONTINUE
      XDER2=XDER
C
 3100 CONTINUE
C
 3500 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3511)XDER
 3511 FORMAT('DERIVATIVE VALUE        = ',E15.8)
      CALL DPWRST('XXX','BUG ')
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
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
        DO9015I=1,NUMPV
          WRITE(ICOUT,9016)I,IPARN(I),IPARN2(I),PARAM(I)
 9016     FORMAT('I,IPARN(I),IPARN2(I),PARAM(I) = ',
     1           I8,2(2X,A4),G15.7)
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
C
      RETURN
      END
      SUBROUTINE DEXCDF(X,CDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
C              FUNCTION VALUE FOR THE DOUBLE EXPONENTIAL
C              (LAPLACE) DISTRIBUTION WITH MEAN = 0 AND
C              STANDARD DEVIATION = SQRT(2).
C              THIS DISTRIBUTION IS DEFINED FOR ALL X AND HAS
C              THE PROBABILITY DENSITY FUNCTION
C              F(X) = 0.5*EXP(-ABS(X)). 
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE CUMULATIVE DISTRIBUTION
C                                FUNCTION IS TO BE EVALUATED.
C     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE
C                                DISTRIBUTION FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION
C             FUNCTION VALUE CDF.
C     PRINTING--NONE.
C     RESTRICTIONS--NONE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--EXP.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--2, 1970, PAGES 22-36.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE:  301-921-2315
C     ORIGINAL VERSION--APRIL     1994. 
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C---------------------------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS.
C     NO INPUT ARGUMENT ERRORS POSSIBLE 
C     FOR THIS DISTRIBUTION.
C
C-----START POINT-----------------------------------------------------
C
      IF(X.LE.0.0)CDF=0.5*EXP(X)
      IF(X.GT.0.0)CDF=1.0-(0.5*EXP(-X)) 
C
      RETURN
      END 
      SUBROUTINE DEXLI1(Y,N,ALOC,SCALE,
     1                  ALIK,AIC,AICC,BIC,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS ROUTINE COMPUTES THE LIKELIHOOD FUNCTION FOR
C              THE DOUBLE EXPONENTIAL (LAPLACE) DISTRIBUTION.  THIS
C              IS FOR THE RAW DATA CASE (I.E., NO GROUPING AND NO
C              CENSORING).
C
C              IT IS ASSUMED THAT BASIC ERROR CHECKING HAS ALREADY BEEN
C              PERFORMED.
C
C     REEFERENCE--NORTON, "THE DOUBLE EXPONENTIAL DISTRIBUTION: USING
C                 CALCULUS TO FIND A MAXIMUM LIKELIHOOD ESTIMATOR",
C                 THE AMERICAN STATISTICIAN, VOL. 28, NO. 2, 1984,
C                 PP. 135-136.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2010/6
C     ORIGINAL VERSION--JUNE      2010.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
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
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DEXL'
      ISUBN2='I1  '
C
      IERROR='NO'
C
      ALIK=-99.0
      AIC=-99.0
      AICC=-99.0
      BIC=-99.0
C
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
        DO56I=1,MIN(N,100)
          WRITE(ICOUT,57)I,Y(I)
   57     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
C
C               ******************************************
C               **  STEP 1--                            **
C               **  COMPUTE LIKELIHOOD FUNCTION         **
C               ******************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'XLI1')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IERFLG=0
      IERROR='NO'
      IWRITE='OFF'
C
C     DOUBLE EXPONENTIAL LOG-LIKELIHOOD FUNCTION IS:
C
C     -N*LOG(2) - SUM[i=1 TO N][ABS(X(i) - LOC)/SCALE]
C
      DN=DBLE(N)
      DS=DBLE(SCALE)
      DU=DBLE(ALOC)
      DTERM1=-DN*DLOG(2.0D0)
      DSUM1=0.0D0
      DO1000I=1,N
        DX=DBLE(Y(I))
        DTERM2=DABS(DX - DU)/DS
        DSUM1=DSUM1 + DTERM2
 1000 CONTINUE
C
      DLIK=DTERM1 - DSUM1
      ALIK=REAL(DLIK)
      DNP=2.0D0
      AIC=REAL(-2.0D0*DLIK + 2.0D0*DNP)
      DTERM3=(2.0D0*DNP*(DNP+1.0D0))/(DN-DNP-1.0D0)
      AICC=REAL(-2.0D0*DLIK + 2.0D0*DNP + DTERM3)
      BIC=REAL(-2.0D0*DLIK + DNP*LOG(DN))
C
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
C
      RETURN
      END
      SUBROUTINE DEXML1(Y,N,XTEMP,ICASE,MAXNXT,
     1                  ALOWLO,AUPPLO,ALOWSC,AUPPSC,
     1                  ALPHA,NUMALP,NUMOUT,
     1                  XMEAN,XMED,XSD,XMIN,XMAX,
     1                  ALOC,ASCALE,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS ROUTINE COMPUTES THE MAXIMUM LIKELIHOOD ESTIMATES
C              FOR THE DOUBLE EXPONENTIAL (LAPLACE) DISTRIBUTION FOR
C              THE RAW DATA CASE (I.E., NO CENSORING AND NO GROUPING).
C              IT WILL OPTIONALLY RETURN THE CONFIDENCE INTERVALS FOR
C              THE LOCATION AND SCALE PARAMETERS.
C
C              IT IS ASSUMED THAT BASIC ERROR CHECKING HAS ALREADY BEEN
C              PERFORMED.
C
C              PUT THIS IN A SEPARATE ROUTINE AS IT MAY BE CALLED
C              FROM MULTIPLE PLACES (DPMLDE WILL GENERATE THE OUTPUT
C              FOR THE DOUBLE EXPONENTIAL MLE COMMAND).
C
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2009/10
C     ORIGINAL VERSION--OCTOBER   2009. EXTRACTED AS A SEPARATE
C                                       SUBROUTINE (FROM DPMLDE)
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      DIMENSION ALOWLO(*)
      DIMENSION AUPPLO(*)
      DIMENSION ALOWSC(*)
      DIMENSION AUPPSC(*)
      DIMENSION ALPHA(*)
C
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
      CHARACTER*40 IDIST
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      INTEGER IFLAG
      INTEGER ICASE
C
      DOUBLE PRECISION DN
      DOUBLE PRECISION DSUM
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION XTEMP(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DEXM'
      ISUBN2='L1  '
      IWRITE='OFF'
      IERROR='NO'
C
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
        DO56I=1,MIN(N,100)
          WRITE(ICOUT,57)I,Y(I)
   57     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
C
C               ******************************************
C               **  STEP 1--                            **
C               **  CARRY OUT CALCULATIONS              **
C               **  FOR DOUBLE EXPONENTIAL MLE ESTIMATE **
C               ******************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'XML1')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IDIST='DOUBLE EXPONENTIAL'
      IFLAG=0
      CALL SUMRAW(Y,N,IDIST,IFLAG,
     1            XMEAN,XVAR,XSD,XMIN,XMAX,
     1            ISUBRO,IBUGA3,IERROR)
      CALL MEDIAN(Y,N,IWRITE,XTEMP,MAXNXT,XMED,IBUGA3,IERROR)
      ALOC=XMED
C
      DN=DBLE(N)
      DSUM=0.0D0
      DO4110I=1,N
        DSUM=DSUM + DBLE(ABS(Y(I) - XMED))
 4110 CONTINUE
      ASCALE=REAL(DSUM/DN)
C
      IF(ICASE.EQ.0)GOTO9000
C
      AN=REAL(N)
      IDF=2*N-1
      DO4120I=1,NUMALP
C
        ALP=ALPHA(I)
        P1=ALP/2.0
        P2=1.0-(ALP/2.0)
C
        CALL CHSPPF(P1,IDF,AUPP)
        CALL CHSPPF(P2,IDF,ALOW)
        ALOWSC(I)=XMEAN + 2.0*REAL(DSUM)/ALOW
        AUPPSC(I)=XMEAN + 2.0*REAL(DSUM)/AUPP
C
        CALL NORPPF(P2,APPF2)
        ALOWLO(I)=ALOC - APPF2*REAL(DSUM)/(AN*SQRT(AN-APPF2**2))
        AUPPLO(I)=ALOC + APPF2*REAL(DSUM)/(AN*SQRT(AN-APPF2**2))
C
 4120 CONTINUE
      NUMOUT=NUMALP
C
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
        DO9060I=1,NUMALP
          WRITE(ICOUT,9065)I,ALPHA(I),ALOWLO(I),AUPPLO(I),ALOWSC(I),
     1                     AUPPSC(I)
 9065     FORMAT('I,ALPHA(I),ALOWLO(I),AUPPLO(I),ALOWSC(I),AUPPSC(I)=',
     1           I8,5G15.7)
          CALL DPWRST('XXX','WRIT')
 9060   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DEXPDF(X,PDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
C              FUNCTION VALUE FOR THE DOUBLE EXPONENTIAL
C              (LAPLACE) DISTRIBUTION WITH MEAN = 0 AND
C              STANDARD DEVIAITON = SQRT(2).
C              THIS DISTRIBUTION IS DEFINED FOR ALL X AND HAS
C              THE PROBABILITY DENSITY FUNCTION
C              F(X) = 0.5*EXP(-ABS(X)). 
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE PROBABILITY DENSITY
C                                FUNCTION IS TO BE EVALUATED.
C     OUTPUT ARGUMENTS--PDF    = THE SINGLE PRECISION PROBABILITY
C                                DENSITY FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
C             FUNCTION VALUE PDF.
C     PRINTING--NONE.
C     RESTRICTIONS--NONE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--EXP.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--2, 1970, PAGES 22-36.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE:  301-921-2315
C     ORIGINAL VERSION--APRIL     1994. 
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C---------------------------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS.
C     NO INPUT ARGUMENT ERRORS POSSIBLE 
C     FOR THIS DISTRIBUTION.
C
C-----START POINT-----------------------------------------------------
C
      ARG=X
      IF(X.LT.0.0)ARG=-X
      PDF=0.5*EXP(-ARG)
C
      RETURN
      END 
      SUBROUTINE DEXPPF(P,PPF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
C              FUNCTION VALUE FOR THE DOUBLE EXPONENTIAL
C              (LAPLACE) DISTRIBUTION WITH MEAN = 0 AND
C              STANDARD DEVIATION = SQRT(2).
C              THIS DISTRIBUTION IS DEFINED FOR ALL X AND HAS
C              THE PROBABILITY DENSITY FUNCTION
C              F(X) = 0.5*EXP(-ABS(X)).
C              NOTE THAT THE PERCENT POINT FUNCTION OF A DISTRIBUTION
C              IS IDENTICALLY THE SAME AS THE INVERSE CUMULATIVE
C              DISTRIBUTION FUNCTION OF THE DISTRIBUTION.
C     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE
C                                (BETWEEN 0.0 AND 1.0)
C                                AT WHICH THE PERCENT POINT
C                                FUNCTION IS TO BE EVALUATED.
C     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT
C                                POINT FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PERCENT POINT
C             FUNCTION VALUE PPF.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--P SHOULD BE BETWEEN 0.0 AND 1.0, EXCLUSIVELY.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--LOG.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--FILLIBEN, SIMPLE AND ROBUST LINEAR ESTIMATION
C                 OF THE LOCATION PARAMETER OF A SYMMETRIC
C                 DISTRIBUTION (UNPUBLISHED PH.D. DISSERTATION,
C                 PRINCETON UNIVERSITY), 1969, PAGES 21-44, 229-231.
C               --FILLIBEN, 'THE PERCENT POINT FUNCTION',
C                 (UNPUBLISHED MANUSCRIPT), 1970, PAGES 28-31.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--2, 1970, PAGES 22-36.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1966)
C               EXCEPTION--HOLLERITH STRINGS IN FORMAT STATEMENTS
C                          DENOTED BY QUOTES RATHER THAN NH.
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--JUNE      1972.
C     UPDATED         --SEPTEMBER 1975.
C     UPDATED         --NOVEMBER  1975.
C     UPDATED         --DECEMBER  1981.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(P.LE.0.0.OR.P.GE.1.0)GOTO50
      GOTO90
   50 WRITE(ICOUT,1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,46)P
      CALL DPWRST('XXX','BUG ')
      RETURN
   90 CONTINUE
    1 FORMAT('***** FATAL ERROR--THE 1ST INPUT ARGUMENT TO THE ',
     1'DEXPPF SUBROUTINE IS OUTSIDE THE ALLOWABLE (0,1) INTERVAL')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',E15.8,' *****')
C
      PHOLD=P
CCCCC IF(PHOLD.LE.0.5)PPF=LOG(2.0*PHOLD)
CCCCC IF(PHOLD.GT.0.5)PPF=-LOG(2.0*(1.0-PHOLD))
      IF(PHOLD.LE.0.5)PPF=LOG(2.0*PHOLD)
      IF(PHOLD.GT.0.5)PPF=-LOG(2.0*(1.0-PHOLD))
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION DEXPRL (X)
C***BEGIN PROLOGUE  DEXPRL
C***PURPOSE  Calculate the relative error exponential (EXP(X)-1)/X.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C4B
C***TYPE      DOUBLE PRECISION (EXPREL-S, DEXPRL-D, CEXPRL-C)
C***KEYWORDS  ELEMENTARY FUNCTIONS, EXPONENTIAL, FIRST ORDER, FNLIB
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C Evaluate  EXPREL(X) = (EXP(X) - 1.0) / X.   For small ABS(X) the
C Taylor series is used.  If X is negative the reflection formula
C         EXPREL(X) = EXP(X) * EXPREL(ABS(X))
C may be used.  This reflection formula will be of use when the
C evaluation for small ABS(X) is done by Chebyshev series rather than
C Taylor series.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH
C***REVISION HISTORY  (YYMMDD)
C   770801  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890911  Removed unnecessary intrinsics.  (WRB)
C   890911  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C***END PROLOGUE  DEXPRL
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
      DOUBLE PRECISION X, ABSX, ALNEPS, XBND, XLN, XN
      LOGICAL FIRST
      SAVE NTERMS, XBND, FIRST
      DATA FIRST /.TRUE./
C
      DEXPRL = 0.0D0
C
C***FIRST EXECUTABLE STATEMENT  DEXPRL
      IF (FIRST) THEN
         ALNEPS = LOG(D1MACH(3))
         XN = 3.72D0 - 0.3D0*ALNEPS
         XLN = LOG((XN+1.0D0)/1.36D0)
         NTERMS = INT(XN - (XN*XLN+ALNEPS)/(XLN+1.36D0) + 1.5D0)
         XBND = D1MACH(3)
      ENDIF
      FIRST = .FALSE.
C
      ABSX = ABS(X)
      IF (ABSX.GT.0.5D0) DEXPRL = (EXP(X)-1.0D0)/X
      IF (ABSX.GT.0.5D0) RETURN
C
      DEXPRL = 1.0D0
      IF (ABSX.LT.XBND) RETURN
C
      DEXPRL = 0.0D0
      DO 20 I=1,NTERMS
        DEXPRL = 1.0D0 + DEXPRL*X/(NTERMS+2-I)
 20   CONTINUE
C
      RETURN
      END
      SUBROUTINE DEXRAN(N,ISEED,X)
C
C     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
C              FROM THE DOUBLE EXPONENTIAL
C              (LAPLACE) DISTRIBUTION WITH MEAN = 0 AND
C              STANDARD DEVIATION = SQRT(2).
C              THIS DISTRIBUTION IS DEFINED FOR ALL X AND HAS
C              THE PROBABILITY DENSITY FUNCTION
C              F(X) = 0.5*EXP(-ABS(X)).
C     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
C                                OF RANDOM NUMBERS TO BE
C                                GENERATED.
C     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
C                                (OF DIMENSION AT LEAST N)
C                                INTO WHICH THE GENERATED
C                                RANDOM SAMPLE WILL BE PLACED.
C     OUTPUT--A RANDOM SAMPLE OF SIZE N
C             FROM THE DOUBLE EXPONENTIAL
C             (LAPLACE) DISTRIBUTION WITH MEAN = 0 AND
C             STANDARD DEVIATION = SQRT(2).
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--LOG.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--TOCHER, THE ART OF SIMULATION,
C                 1963, PAGES 14-15.
C               --HAMMERSLEY AND HANDSCOMB, MONTE CARLO METHODS,
C                 1964, PAGE 36.
C               --FILLIBEN, SIMPLE AND ROBUST LINEAR ESTIMATION
C                 OF THE LOCATION PARAMETER OF A SYMMETRIC
C                 DISTRIBUTION (UNPUBLISHED PH.D. DISSERTATION,
C                 PRINCETON UNIVERSITY), 1969, PAGE 231.
C               --FILLIBEN, 'THE PERCENT POINT FUNCTION',
C                 (UNPUBLISHED MANUSCRIPT), 1970, PAGES 28-31.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--2, 1970, PAGES 22-36.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1966)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--JUNE      1972.
C     UPDATED         --SEPTEMBER 1975.
C     UPDATED         --NOVEMBER  1975.
C     UPDATED         --DECEMBER  1981.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(N.LT.1)GOTO50
      GOTO90
   50 WRITE(ICOUT, 5)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,47)N
      CALL DPWRST('XXX','BUG ')
      RETURN
   90 CONTINUE
    5 FORMAT('***** FATAL ERROR--THE 1ST INPUT ARGUMENT TO THE ',
     1'DEXRAN SUBROUTINE IS NON-POSITIVE *****')
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8,' *****')
C
C     GENERATE N UNIFORM (0,1) RANDOM NUMBERS;
C
      CALL UNIRAN(N,ISEED,X)
C
C     GENERATE N DOUBLE EXPONENTIAL RANDOM NUMBERS
C     USING THE PERCENT POINT FUNCTION TRANSFORMATION METHOD.
C
      DO100I=1,N
      Q=X(I)
CCCCC IF(Q.LE.0.5)X(I)=LOG(2.0*Q)
CCCCC IF(Q.GT.0.5)X(I)=-LOG(2.0*(1.0-Q))
      IF(Q.LE.0.5)X(I)=LOG(2.0*Q)
      IF(Q.GT.0.5)X(I)=-LOG(2.0*(1.0-Q))
  100 CONTINUE
C
      RETURN
      END
      SUBROUTINE DEXSF(P,SF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE SPARSITY
C              FUNCTION VALUE FOR THE DOUBLE EXPONENTIAL
C              (LAPLACE) DISTRIBUTION WITH MEAN = 0 AND
C              STANDARD DEVIATION = SQRT(2).
C              THIS DISTRIBUTION IS DEFINED FOR ALL X AND HAS
C              THE PROBABILITY DENSITY FUNCTION
C              F(X) = 0.5*EXP(-ABS(X)). 
C              NOTE THAT THE SPARSITY FUNCTION OF A DISTRIBUTION
C              IS THE DERIVATIVE OF THE PERCENT POINT FUNCTION,
C              AND ALSO IS THE RECIPROCAL OF THE PROBABILITY
C              DENSITY FUNCTION (BUT IN UNITS OF P RATHER THAN X).
C     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE 
C                                (BETWEEN 0.0 AND 1.0)
C                                AT WHICH THE SPARSITY
C                                FUNCTION IS TO BE EVALUATED.
C     OUTPUT ARGUMENTS--SF     = THE SINGLE PRECISION
C                                SPARSITY FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION SPARSITY
C             FUNCTION VALUE SF.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS. 
C     RESTRICTIONS--P SHOULD BE BETWEEN 0.0 AND 1.0, EXCLUSIVELY.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--FILLIBEN, SIMPLE AND ROBUST LINEAR ESTIMATION
C                 OF THE LOCATION PARAMETER OF A SYMMETRIC
C                 DISTRIBUTION (UNPUBLISHED PH.D. DISSERTATION,
C                 PRINCETON UNIVERSITY), 1969, PAGES 21-44, 229-231.
C               --FILLIBEN, 'THE PERCENT POINT FUNCTION',
C                 (UNPUBLISHED MANUSCRIPT), 1970, PAGES 28-31.
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--2, 1970, PAGES 22-36.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE:  301-921-2315
C     ORIGINAL VERSION--APRIL     1994. 
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C---------------------------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(P.LE.0.0.OR.P.GE.1.0)GOTO50
      GOTO90
   50 WRITE(ICOUT,1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,46)P
      CALL DPWRST('XXX','BUG ')
      RETURN
   90 CONTINUE
    1 FORMAT('***** FATAL ERROR--THE 1ST INPUT ARGUMENT TO THE ',
     1' DEXSF SUBROUTINE IS OUTSIDE THE ALLOWABLE (0,1) INTERVAL')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',E15.8,' *****')
C
      IF(P.LE.0.5)SF=1.0/P
      IF(P.GT.0.5)SF=1.0/(1.0-P)
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION DFAC (N)
C***BEGIN PROLOGUE  DFAC
C***PURPOSE  Compute the factorial function.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C1
C***TYPE      DOUBLE PRECISION (FAC-S, DFAC-D)
C***KEYWORDS  FACTORIAL, FNLIB, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C DFAC(N) calculates the double precision factorial for integer
C argument N.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D9LGMC, DGAMLM, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C***END PROLOGUE  DFAC
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
      DOUBLE PRECISION FACN(31), SQ2PIL, X, XMAX, XMIN,  D9LGMC
      SAVE FACN, SQ2PIL, NMAX
      DATA FACN  (  1) / +.1000000000 0000000000 0000000000 000 D+1    /
      DATA FACN  (  2) / +.1000000000 0000000000 0000000000 000 D+1    /
      DATA FACN  (  3) / +.2000000000 0000000000 0000000000 000 D+1    /
      DATA FACN  (  4) / +.6000000000 0000000000 0000000000 000 D+1    /
      DATA FACN  (  5) / +.2400000000 0000000000 0000000000 000 D+2    /
      DATA FACN  (  6) / +.1200000000 0000000000 0000000000 000 D+3    /
      DATA FACN  (  7) / +.7200000000 0000000000 0000000000 000 D+3    /
      DATA FACN  (  8) / +.5040000000 0000000000 0000000000 000 D+4    /
      DATA FACN  (  9) / +.4032000000 0000000000 0000000000 000 D+5    /
      DATA FACN  ( 10) / +.3628800000 0000000000 0000000000 000 D+6    /
      DATA FACN  ( 11) / +.3628800000 0000000000 0000000000 000 D+7    /
      DATA FACN  ( 12) / +.3991680000 0000000000 0000000000 000 D+8    /
      DATA FACN  ( 13) / +.4790016000 0000000000 0000000000 000 D+9    /
      DATA FACN  ( 14) / +.6227020800 0000000000 0000000000 000 D+10   /
      DATA FACN  ( 15) / +.8717829120 0000000000 0000000000 000 D+11   /
      DATA FACN  ( 16) / +.1307674368 0000000000 0000000000 000 D+13   /
      DATA FACN  ( 17) / +.2092278988 8000000000 0000000000 000 D+14   /
      DATA FACN  ( 18) / +.3556874280 9600000000 0000000000 000 D+15   /
      DATA FACN  ( 19) / +.6402373705 7280000000 0000000000 000 D+16   /
      DATA FACN  ( 20) / +.1216451004 0883200000 0000000000 000 D+18   /
      DATA FACN  ( 21) / +.2432902008 1766400000 0000000000 000 D+19   /
      DATA FACN  ( 22) / +.5109094217 1709440000 0000000000 000 D+20   /
      DATA FACN  ( 23) / +.1124000727 7776076800 0000000000 000 D+22   /
      DATA FACN  ( 24) / +.2585201673 8884976640 0000000000 000 D+23   /
      DATA FACN  ( 25) / +.6204484017 3323943936 0000000000 000 D+24   /
      DATA FACN  ( 26) / +.1551121004 3330985984 0000000000 000 D+26   /
      DATA FACN  ( 27) / +.4032914611 2660563558 4000000000 000 D+27   /
      DATA FACN  ( 28) / +.1088886945 0418352160 7680000000 000 D+29   /
      DATA FACN  ( 29) / +.3048883446 1171386050 1504000000 000 D+30   /
      DATA FACN  ( 30) / +.8841761993 7397019545 4361600000 000 D+31   /
      DATA FACN  ( 31) / +.2652528598 1219105863 6308480000 000 D+33   /
      DATA SQ2PIL / 0.9189385332 0467274178 0329736405 62 D0 /
      DATA NMAX / 0 /
C***FIRST EXECUTABLE STATEMENT  DFAC
C
      DFAC=0.0D0
C
      IF (NMAX.NE.0) GO TO 10
      CALL DGAMLM (XMIN, XMAX)
      NMAX = INT(XMAX - 1.D0)
C
 10   IF (N .LT. 0) THEN
        WRITE(ICOUT,1)
    1   FORMAT('***** ERORR FROM DFAC, THE FACTORIAL OF A NEGATIVE',
     1         ' NUMBER IS UNDEFINED. *****')
        CALL DPWRST('XXX','BUG ')
        RETURN
      ENDIF
C
      IF (N.LE.30) DFAC = FACN(N+1)
      IF (N.LE.30) RETURN
C
      IF (N .GT. NMAX) THEN
        WRITE(ICOUT,2)
    2   FORMAT('***** ERORR FROM DFAC, THE ARGUMENT IS SO BIG THAT ',
     1         ' THE FACTORIAL OVERFLOWS. *****')
        CALL DPWRST('XXX','BUG ')
        RETURN
      ENDIF
C
      X = REAL(N + 1)
      DFAC = EXP ((X-0.5D0)*LOG(X) - X + SQ2PIL + D9LGMC(X) )
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION DFRENC (X,MODE)
C
C     .  COPYRIGHT (C) 1992, CALIFORNIA INSTITUTE OF TECHNOLOGY.
C     .  U. S. GOVERNMENT SPONSORSHIP UNDER
C     .  NASA CONTRACT NAS7-918 IS ACKNOWLEDGED.
C>> ALAN HECKERT MODIFIED FOR INCLUSION INTO DATAPLOT (BASICALLY,
C   PASS MODE AS ARGUMENT AND ELIMINATE MULTIPLE ENTRY POINTS.
C   ALSO, DELETED COMMENT LINES FOR COEFFICIENTS USING DIFFERENT
C   ORDERS OF APPROXIMATION.
C>> 1992-09-15 DFRENL WV SNYDER SPECIALIZING INSTRUCTIONS
C>> 1992-04-13 DFRENL WV SNYDER DECLARE DFRENF, DFRENG, DFRENS
C>> 1992-03-18 DFRENL WV SNYDER MOVE DECLARATIONS FOR COEFFICIENT ARRAYS
C>> 1992-01-24 DFRENL WV SNYDER ORIGINAL CODE
C ENTRIES IN THIS SUBPROGRAM COMPUTE THE FRESNEL COSINE AND SINE
C INTEGRALS C(X) AND S(X), AND THE AUXILIARY FUNCTIONS F(X) AND G(X),
C FOR ANY X:
C     DFRENC(X) FOR FRESNEL INTEGRAL C(X)
C     DFRENS(X) FOR FRESNEL INTEGRAL S(X)
C     DFRENF(X) FOR FRESNEL INTEGRAL AUXILIARY FUNCTION F(X)
C     DFRENG(X) FOR FRESNEL INTEGRAL AUXILIARY FUNCTION G(X).
C
C DEVELOPED BY W. V. SNYDER, JET PROPULSION LABORATORY, 24 JANUARY 1992.
C
C REF: W. J. CODY, "CHEBYSHEV APPROXIMATIONS FOR THE FRESNEL INTEGRALS",
C MATHEMATICS OF COMPUTATION, 1968, PP 450-453 PLUS MICROFICHE SUPPL.
C ACCURACIES OF HIGHEST ORDER FORMULAE, WHERE E IS RELATIVE ERROR:
C
C RANGE           FUNCTION   -LOG10(E)   FUNCTION   -LOG10(E)
C |X|<=1.2          C(X)       16.24       S(X)       17.26
C 1.2<|X|<=1.6      C(X)       17.47       S(X)       18.66
C 1.6<|X|<=1.9      F(X)       17.13       G(X)       16.25
C 1.9<|X|<=2.4      F(X)       16.64       G(X)       15.65
C 2.4<|X|           F(X)       16.89       G(X)       15.58
C
C REFER TO CODY FOR ACCURACY OF OTHER APPROXIMATIONS.
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION X
C
C--   S VERSION USES SFRENC,SFRENC,SFRENF,SFRENG,SFRENS,R1MACH,R1MACH
C--   D VERSION USES DFRENC,DFRENC,DFRENF,DFRENG,DFRENS,D1MACH,D1MACH
C
C DFRENF, DFRENG, DFRENS ARE ALTERNATE ENTRIES.
CCCCC DOUBLE PRECISION DFRENF, DFRENG, DFRENS
C
C PID2 IS PI / 2.
      DOUBLE PRECISION PID2
      PARAMETER (PID2 = 1.570796326794896619231321691639751442099D0)
C RPI IS THE RECIPROCAL OF PI:
      DOUBLE PRECISION RPI
      PARAMETER (RPI = 0.3183098861837906715377675267450287240689D0)
C RPISQ IS THE RECIPROCAL OF PI SQUARED:
      DOUBLE PRECISION RPISQ
      PARAMETER (RPISQ = RPI * RPI)
C AX IS ABS(X).
C BIGX IS 1/SQRT(ROUND-OFF).  IF X > BIGX THEN TO THE WORKING
C         PRECISION X**2 IS AN INTEGER (WHICH WE ASSUME TO BE A MULTIPLE
C         OF FOUR), SO COS(PI/2 * X**2) = 1, AND SIN(PI/2 * X**2) = 0.
C C AND S ARE VALUES OF C(X) AND S(X), RESPECTIVELY.
C CX AND SX ARE COS(PI/2 * AX**2) AND SIN(PI/2 * AX**2), RESPECTIVELY.
C F AND G ARE USED TO COMPUTE F(X) AND G(X) WHEN X > 1.6.
C HAVEC, HAVEF, HAVEG, HAVES ARE LOGICAL VARIABLES THAT INDICATE
C         WHETHER THE VALUES STORED IN C, F, G AND S CORRESPOND TO THE
C         VALUE STORED IN X.  HAVEF INDICATES WE HAVE BOTH F AND G WHEN
C         XSAVE .LE. 1.6, AND HAVEC INDICATES WE HAVE BOTH C AND S WHEN
C         XSAVE .GT. 1.6.
C LARGEF IS 1/(PI * UNDERFLOW).  IF X > LARGEF THEN F ~ 0.
C LARGEG IS CBRT(1/(PI**2 * UNDERFLOW)).  IF X > LARGEG THEN G ~ 0.
C LARGEX IS 1/SQRT(SQRT(UNDERFLOW)).  IF X > LARGEX THEN F ~ 1/(PI * X)
C         AND G ~ 1/(PI**2 * X**3).
C MODE INDICATES THE FUNCTION TO BE COMPUTED: 1 = C(X), 2 = S(X),
C         3 = F(X), 4 = G(X).
C NEEDC, NEEDF, NEEDG, NEEDS ARE ARRAYS INDEXED BY MODE (MODE+4 WHEN
C         X .GT. 1.6) THAT INDICATE WHAT FUNCTIONS ARE NEEDED.
C RESULT IS EQUIVALENCED TO C, F, G, AND S.
C WANTC INDICATES WHETHER C AND S MUST BE COMPUTED FROM F AND G.
C WANTF AND WANTG INDICATE WE COMPUTED F AND G ON THE PRESENT CALL.
C XSAVE IS THE MOST RECENTLY PROVIDED VALUE OF X.
C X4 IS EITHER X ** 4 OR (1.0/X) ** 4.
      DOUBLE PRECISION AX, BIGX, C, CX, F, G, LARGEF, LARGEG, LARGEX
      DOUBLE PRECISION RESULT(4), S, SX, XSAVE, X4
      SAVE BIGX, C, F, G, LARGEF, LARGEG, LARGEX, S, RESULT, XSAVE
      EQUIVALENCE (RESULT(1), C), (RESULT(2), S)
      EQUIVALENCE (RESULT(3), F), (RESULT(4), G)
      LOGICAL HAVEC, HAVEF, HAVEG, HAVES, WANTC, WANTF, WANTG
      SAVE HAVEC, HAVEF, HAVEG, HAVES
      INTEGER MODE
      LOGICAL NEEDC(8), NEEDF(8), NEEDG(8), NEEDS(8)
C
      INCLUDE 'DPCOMC.INC'
C
C     DECLARATIONS FOR COEFFICIENT ARRAYS.  IF YOU CHANGE THE ORDER OF
C     APPROXIMATION, YOU MUST CHANGE THE DECLARATION HERE, THE DATA
C     STATEMENTS BELOW, AND THE EXECUTABLE STATEMENTS THAT EVALUATE
C     THE APPROXIMATIONS.
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
C
      DATA BIGX /-1.0D0/
      DATA C /0.0D0/, F /0.5D0/, G /0.5D0/, S /0.0D0/, XSAVE /0.0D0/
      DATA HAVEC/.TRUE./, HAVEF/.TRUE./, HAVEG/.TRUE./, HAVES/.TRUE./
C        C(X)    S(X)    F(X)    G(X)    C(X)    S(X)    F(X)    G(X)
      DATA NEEDC
     1 /.TRUE., .FALSE.,.TRUE., .TRUE., .TRUE., .FALSE.,.FALSE.,.FALSE./
      DATA NEEDS
     1 /.FALSE.,.TRUE., .TRUE., .TRUE., .FALSE.,.TRUE., .FALSE.,.FALSE./
      DATA NEEDF
     1 /.FALSE.,.FALSE.,.TRUE., .FALSE.,.TRUE., .TRUE., .TRUE., .FALSE./
      DATA NEEDG
     1 /.FALSE.,.FALSE.,.FALSE.,.TRUE. ,.TRUE., .TRUE., .FALSE.,.TRUE. /
C
C     COEFFICIENTS FOR C(X), |X| <= 1.2
C
      DATA PC1(0) / 9.99999 99999 99999 421 D-1/
      DATA PC1(1) /-1.99460 89882 61842 706 D-1/
      DATA QC1(1) / 4.72792 11201 04532 689 D-2/
      DATA PC1(2) / 1.76193 95254 34914 045 D-2/
      DATA QC1(2) / 1.09957 21502 56418 851 D-3/
      DATA PC1(3) /-5.28079 65137 26226 960 D-4/
      DATA QC1(3) / 1.55237 88527 69941 331 D-5/
      DATA PC1(4) / 5.47711 38568 26871 660 D-6/
      DATA QC1(4) / 1.18938 90142 28757 184 D-7/
C
C     COEFFICIENTS FOR C(X), 1.2 < |X| <= 1.6
      DATA PC2(0) / 1.00000 00000 01110 43640 D0 /
      DATA PC2(1) /-2.07073 36033 53238 94245 D-1/
      DATA QC2(1) / 3.96667 49695 23234 33510 D-2/
      DATA PC2(2) / 1.91870 27943 17469 26505 D-2/
      DATA QC2(2) / 7.88905 24505 23599 07842 D-4/
      DATA PC2(3) /-6.71376 03469 49221 09230 D-4/
      DATA QC2(3) / 1.01344 63086 67494 06081 D-5/
      DATA PC2(4) / 1.02365 43505 61058 64908 D-5/
      DATA QC2(4) / 8.77945 37789 23692 65356 D-8/
      DATA PC2(5) /-5.68293 31012 18707 28343 D-8/
      DATA QC2(5) / 4.41701 37406 50096 20393 D-10/
C
C     COEFFICIENTS FOR S(X), |X| <= 1.2
      DATA PS1(0) / 5.23598 77559 82988 7021 D-1/
      DATA PS1(1) /-7.07489 91514 45230 2596 D-2/
      DATA QS1(1) / 4.11223 15114 23842 2205 D-2/
      DATA PS1(2) / 3.87782 12346 36828 7939 D-3/
      DATA QS1(2) / 8.17091 94215 21344 7204 D-4/
      DATA PS1(3) /-8.45557 28435 27768 0591 D-5/
      DATA QS1(3) / 9.62690 87593 90340 3370 D-6/
      DATA PS1(4) / 6.71748 46662 51408 6196 D-7/
      DATA QS1(4) / 5.95281 22767 84099 8345 D-8/
C
C     COEFFICIENTS FOR S(X), 1.2 < |X| <= 1.6
      DATA PS2(0) / 5.23598 77559 83441 65913 D-1/
      DATA PS2(1) /-7.37766 91401 01913 23867 D-2/
      DATA QS2(1) / 3.53398 34276 74721 62540 D-2/
      DATA PS2(2) / 4.30730 52650 43665 10217 D-3/
      DATA QS2(2) / 6.18224 62019 54732 16538 D-4/
      DATA PS2(3) /-1.09540 02391 14349 94566 D-4/
      DATA QS2(3) / 6.87086 26571 86201 17905 D-6/
      DATA PS2(4) / 1.28531 04374 27248 20610 D-6/
      DATA QS2(4) / 5.03090 58124 66123 75866 D-8/
      DATA PS2(5) /-5.76765 81559 30888 04567 D-9/
      DATA QS2(5) / 2.05539 12445 85795 96075 D-10/
C
C     COEFFICIENTS FOR F(X), 1.6 < |X| <= 1.9
      DATA PF1(0) / 3.18309 75293 58098 5290 D-1/
      DATA PF1(1) / 1.22260 00551 67296 1219 D1 /
      DATA QF1(1) / 3.87130 03365 58344 2831 D1 /
      DATA PF1(2) / 1.29248 86131 90165 7025 D2 /
      DATA QF1(2) / 4.16743 59830 70562 9745 D2 /
      DATA PF1(3) / 4.38863 67156 69554 7655 D2 /
      DATA QF1(3) / 1.47400 30733 96661 0568 D3 /
      DATA PF1(4) / 4.14667 22177 95896 1672 D2 /
      DATA QF1(4) / 1.53716 75584 89575 9916 D3 /
      DATA PF1(5) / 5.67714 63664 18511 6454 D1 /
      DATA QF1(5) / 2.91130 88788 84783 1515 D2 /
C
C     COEFFICIENTS FOR F(X), 1.9 < |X| <= 2.4
      DATA PF2(0) / 3.18309 88182 20169 217 D-1/
      DATA PF2(1) / 1.95883 94102 19691 002 D1 /
      DATA QF2(1) / 6.18427 13817 28873 709 D1 /
      DATA PF2(2) / 3.39837 13492 69842 400 D2 /
      DATA QF2(2) / 1.08535 06750 06501 251 D3 /
      DATA PF2(3) / 1.93007 64078 67157 531 D3 /
      DATA QF2(3) / 6.33747 15585 11437 898 D3 /
      DATA PF2(4) / 3.09145 16157 44296 552 D3 /
      DATA QF2(4) / 1.09334 24898 88087 888 D4 /
      DATA PF2(5) / 7.17703 24936 51399 590 D2 /
      DATA QF2(5) / 3.36121 69918 05511 494 D3 /
C
C     COEFFICIENTS FOR F(X), 2.4 < |X|
      DATA PF3(0) /-9.67546 03299 52532 343 D-2/
      DATA PF3(1) /-2.43127 54071 94161 683 D1 /
      DATA QF3(1) / 2.54828 90129 49732 752 D2 /
      DATA PF3(2) /-1.94762 19983 06889 176 D3 /
      DATA QF3(2) / 2.09976 15368 57815 105 D4 /
      DATA PF3(3) /-6.05985 21971 60773 639 D4 /
      DATA QF3(3) / 6.92412 25098 27708 985 D5 /
      DATA PF3(4) /-7.07680 69528 37779 823 D5 /
      DATA QF3(4) / 9.17882 32299 18143 780 D6 /
      DATA PF3(5) /-2.41765 67490 61154 155 D6 /
      DATA QF3(5) / 4.29273 32556 30186 679 D7 /
      DATA PF3(6) /-7.83491 45900 78317 336 D5 /
      DATA QF3(6) / 4.80329 47842 60528 342 D7 /
C
C     COEFFICIENTS FOR G(X), 1.6 < |X| <= 1.9
      DATA PG1(0) / 1.01320 61881 02747 985 D-1/
      DATA PG1(1) / 4.44533 82755 05123 778 D0 /
      DATA QG1(1) / 4.53925 01967 36893 605 D1 /
      DATA PG1(2) / 5.31122 81348 09894 481 D1 /
      DATA QG1(2) / 5.83590 57571 64290 666 D2 /
      DATA PG1(3) / 1.99182 81867 89025 318 D2 /
      DATA QG1(3) / 2.54473 13318 18221 034 D3 /
      DATA PG1(4) / 1.96232 03797 16626 191 D2 /
      DATA QG1(4) / 3.48112 14785 65452 837 D3 /
      DATA PG1(5) / 2.05421 43249 85006 303 D1 /
      DATA QG1(5) / 1.01379 48339 60028 555 D3 /
C
C     COEFFICIENTS FOR G(X), 1.9 < |X| <= 2.4
      DATA PG2(0) / 1.01321 16176 18045 86 D-1/
      DATA PG2(1) / 7.11205 00178 97828 23 D0 /
      DATA QG2(1) / 7.17128 59693 93021 98 D1 /
      DATA PG2(2) / 1.40959 61791 13155 24 D2 /
      DATA QG2(2) / 1.49051 92279 73292 29 D3 /
      DATA PG2(3) / 9.08311 74952 95939 38 D2 /
      DATA QG2(3) / 1.06729 67803 05808 97 D4 /
      DATA PG2(4) / 1.59268 00608 53538 64 D3 /
      DATA QG2(4) / 2.41315 56721 33697 42 D4 /
      DATA PG2(5) / 3.13330 16306 87559 50 D2 /
      DATA QG2(5) / 1.15149 83237 62606 04 D4 /
C
C     COEFFICIENTS FOR G(X), 2.4 < |X|
      DATA PG3(0) /-1.53989 73381 97693 16 D-1/
      DATA PG3(1) /-4.31710 15782 33575 68 D1 /
      DATA QG3(1) / 2.86733 19497 58994 83 D2 /
      DATA PG3(2) /-3.87754 14174 63784 93 D3 /
      DATA QG3(2) / 2.69183 18039 62425 36 D4 /
      DATA PG3(3) /-1.35678 86781 37563 47 D5 /
      DATA QG3(3) / 1.02878 69305 66875 06 D6 /
      DATA PG3(4) /-1.77758 95083 80296 76 D6 /
      DATA QG3(4) / 1.62095 60050 02316 46 D7 /
      DATA PG3(5) /-6.66907 06166 86364 16 D6 /
      DATA QG3(5) / 9.38695 86253 16351 79 D7 /
      DATA PG3(6) /-1.72590 22465 48368 45 D6 /
      DATA QG3(6) / 1.40622 44112 35800 05 D8 /
C
C  MODE = 1 = FRESNEL COSINE INTEGRAL
C  MODE = 2 = FRESNEL SINE INTEGRAL
C  MODE = 3 = F AUXILLARY FUNCTION
C  MODE = 4 = G AUXILLARY FUNCTION
C
C     *****     EXECUTABLE STATEMENTS     ****************************
C
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
               C = X * ((((PC1(4)*X4+PC1(3))*X4+PC1(2))*X4+PC1(1))*X4+
     1                     PC1(0))
     2           / ((((QC1(4)*X4+QC1(3))*X4+QC1(2))*X4+QC1(1))*X4+1.0D0)
            ELSE
               C = X * (((((PC2(5)*X4+PC2(4))*X4+PC2(3))*X4+PC2(2))*X4+
     1                     PC2(1))*X4+PC2(0))
     2           /   (((((QC2(5)*X4+QC2(4))*X4+QC2(3))*X4+QC2(2))*X4+
     3                   QC2(1))*X4+1.0D0)
            END IF
            HAVEC = .TRUE.
         END IF
         IF (NEEDS(MODE) .AND. .NOT. HAVES) THEN
            IF (AX .LE. 1.2D0) THEN
               S = X**3*((((PS1(4)*X4+PS1(3))*X4+PS1(2))*X4+PS1(1))*X4+
     1                      PS1(0))
     2           / ((((QS1(4)*X4+QS1(3))*X4+QS1(2))*X4+QS1(1))*X4+1.0D0)
            ELSE
               S = X**3*(((((PS2(5)*X4+PS2(4))*X4+PS2(3))*X4+PS2(2))*X4+
     1                      PS2(1))*X4+PS2(0))
     2           /   (((((QS2(5)*X4+QS2(4))*X4+QS2(3))*X4+QS2(2))*X4+
     3                    QS2(1))*X4+1.0D0)
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
                  F = (((((PF1(5)*X4+PF1(4))*X4+PF1(3))*X4+PF1(2))*X4+
     1                    PF1(1))*X4+PF1(0))
     2             / ((((((QF1(5)*X4+QF1(4))*X4+QF1(3))*X4+QF1(2))*X4+
     3                    QF1(1))*X4+1.0D0) * AX)
               ELSE IF (AX .LE. 2.4) THEN
                  F = (((((PF2(5)*X4+PF2(4))*X4+PF2(3))*X4+PF2(2))*X4+
     1                    PF2(1))*X4+PF2(0))
     2             / ((((((QF2(5)*X4+QF2(4))*X4+QF2(3))*X4+QF2(2))*X4+
     3                    QF2(1))*X4+1.0D0) * AX)
               ELSE
                  F = (RPI +
     1              X4*((((((PF3(6)*X4+PF3(5))*X4+PF3(4))*X4+PF3(3))*X4+
     2                   PF3(2))*X4+PF3(1))*X4+PF3(0))
     3            /    ((((((QF3(6)*X4+QF3(5))*X4+QF3(4))*X4+QF3(3))*X4+
     4                   QF3(2))*X4+QF3(1))*X4+1.0D0)) / AX
               END IF
               HAVEF = .TRUE.
            END IF
            WANTG = NEEDG(MODE+4) .AND. .NOT. HAVEG
            IF (WANTG) THEN
               IF (X .LE. 1.9D0) THEN
                  G = (((((PG1(5)*X4+PG1(4))*X4+PG1(3))*X4+PG1(2))*X4+
     1                    PG1(1))*X4+PG1(0))
     2             / ((((((QG1(5)*X4+QG1(4))*X4+QG1(3))*X4+QG1(2))*X4+
     3                    QG1(1))*X4+1.0D0) * AX**3)
               ELSE IF (AX .LE. 2.4D0) THEN
                  G = (((((PG2(5)*X4+PG2(4))*X4+PG2(3))*X4+PG2(2))*X4+
     1                     PG2(1))*X4+PG2(0))
     2             / ((((((QG2(5)*X4+QG2(4))*X4+QG2(3))*X4+QG2(2))*X4+
     3                    QG2(1))*X4+1.0D0) * AX**3)
               ELSE
                  G = (RPISQ +
     1              X4*((((((PG3(6)*X4+PG3(5))*X4+PG3(4))*X4+PG3(3))*X4+
     2                   PG3(2))*X4+PG3(1))*X4+PG3(0))
     3            /    ((((((QG3(6)*X4+QG3(5))*X4+QG3(4))*X4+QG3(3))*X4+
     4                   QG3(2))*X4+QG3(1))*X4+1.0D0)) / AX**3
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
C              WE COULD DO THE FOLLOWING BEFORE THE PRECEEDING, AND THEN
C              NOT PUT IN A TEST IN THE PRECEEDING FOR X .LT. 0, BUT
C              EVEN THOUGH THE RESULTS ARE MATHEMATICALLY IDENTICAL, WE
C              WOULD HAVE SOME CANCELLATION ABOVE IF WE DID SO.
               IF (WANTG) G = CX + SX - G
               IF (WANTF) F = CX - SX - F
            END IF
          END IF
      END IF
      XSAVE = X
C
      DFRENC = RESULT(MODE)
      RETURN
      END
      SUBROUTINE DFZERO (F, B, C, R, RE, AE, IFLAG)
C***BEGIN PROLOGUE  DFZERO
C***PURPOSE  Search for a zero of a function F(X) in a given interval
C            (B,C).  It is designed primarily for problems where F(B)
C            and F(C) have opposite signs.
C***LIBRARY   SLATEC
C***CATEGORY  F1B
C***TYPE      DOUBLE PRECISION (FZERO-S, DFZERO-D)
C***KEYWORDS  BISECTION, NONLINEAR, ROOTS, ZEROS
C***AUTHOR  Shampine, L. F., (SNLA)
C           Watts, H. A., (SNLA)
C***DESCRIPTION
C
C     DFZERO searches for a zero of a DOUBLE PRECISION function F(X)
C     between the given DOUBLE PRECISION values B and C until the width
C     of the interval (B,C) has collapsed to within a tolerance
C     specified by the stopping criterion,
C        ABS(B-C) .LE. 2.*(RW*ABS(B)+AE).
C     The method used is an efficient combination of bisection and the
C     secant rule and is due to T. J. Dekker.
C
C     Description Of Arguments
C
C   F     :EXT   - Name of the DOUBLE PRECISION external function.  This
C                  name must be in an EXTERNAL statement in the calling
C                  program.  F must be a function of one DOUBLE
C                  PRECISION argument.
C
C   B     :INOUT - One end of the DOUBLE PRECISION interval (B,C).  The
C                  value returned for B usually is the better
C                  approximation to a zero of F.
C
C   C     :INOUT - The other end of the DOUBLE PRECISION interval (B,C)
C
C   R     :IN    - A (better) DOUBLE PRECISION guess of a zero of F
C                  which could help in speeding up convergence.  If F(B)
C                  and F(R) have opposite signs, a root will be found in
C                  the interval (B,R);  if not, but F(R) and F(C) have
C                  opposite signs, a root will be found in the interval
C                  (R,C);  otherwise, the interval (B,C) will be
C                  searched for a possible root.  When no better guess
C                  is known, it is recommended that R be set to B or C,
C                  since if R is not interior to the interval (B,C), it
C                  will be ignored.
C
C   RE    :IN    - Relative error used for RW in the stopping criterion.
C                  If the requested RE is less than machine precision,
C                  then RW is set to approximately machine precision.
C
C   AE    :IN    - Absolute error used in the stopping criterion.  If
C                  the given interval (B,C) contains the origin, then a
C                  nonzero value should be chosen for AE.
C
C   IFLAG :OUT   - A status code.  User must check IFLAG after each
C                  call.  Control returns to the user from DFZERO in all
C                  cases.
C
C                1  B is within the requested tolerance of a zero.
C                   The interval (B,C) collapsed to the requested
C                   tolerance, the function changes sign in (B,C), and
C                   F(X) decreased in magnitude as (B,C) collapsed.
C
C                2  F(B) = 0.  However, the interval (B,C) may not have
C                   collapsed to the requested tolerance.
C
C                3  B may be near a singular point of F(X).
C                   The interval (B,C) collapsed to the requested tol-
C                   erance and the function changes sign in (B,C), but
C                   F(X) increased in magnitude as (B,C) collapsed, i.e.
C                     ABS(F(B out)) .GT. MAX(ABS(F(B in)),ABS(F(C in)))
C
C                4  No change in sign of F(X) was found although the
C                   interval (B,C) collapsed to the requested tolerance.
C                   The user must examine this case and decide whether
C                   B is near a local minimum of F(X), or B is near a
C                   zero of even multiplicity, or neither of these.
C
C                5  Too many (.GT. 500) function evaluations used.
C
C***REFERENCES  L. F. Shampine and H. A. Watts, FZERO, a root-solving
C                 code, Report SC-TM-70-631, Sandia Laboratories,
C                 September 1970.
C               T. J. Dekker, Finding a zero by means of successive
C                 linear interpolation, Constructive Aspects of the
C                 Fundamental Theorem of Algebra, edited by B. Dejon
C                 and P. Henrici, Wiley-Interscience, 1969.
C***ROUTINES CALLED  D1MACH
C***REVISION HISTORY  (YYMMDD)
C   700901  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  DFZERO
CCCCC DOUBLE PRECISION A,ACBS,ACMB,AE,AW,B,C,CMB,D1MACH,ER,
      DOUBLE PRECISION A,ACBS,ACMB,AE,AW,B,C,CMB,ER,
     +                 F,FA,FB,FC,FX,FZ,P,Q,R,RE,RW,T,TOL,Z
      INTEGER IC,IFLAG,KOUNT
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
C***FIRST EXECUTABLE STATEMENT  DFZERO
C
C   ER is two times the computer unit roundoff value which is defined
C   here by the function D1MACH.
C
      ER = 2.0D0 * D1MACH(4)
C
C   Initialize.
C
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
C
    3 IF (ABS(FC) .GE. ABS(FB)) GO TO 4
C
C   Perform interchange.
C
      A = B
      FA = FB
      B = C
      FB = FC
      C = A
      FC = FA
C
    4 CMB = 0.5D0*(C-B)
      ACMB = ABS(CMB)
      TOL = RW*ABS(B) + AW
C
C   Test stopping criterion and function count.
C
      IF (ACMB .LE. TOL) GO TO 10
      IF (FB .EQ. 0.D0) GO TO 11
      IF (KOUNT .GE. 500) GO TO 14
C
C   Calculate new iterate implicitly as B+P/Q, where we arrange
C   P .GE. 0.  The implicit form is used to prevent overflow.
C
      P = (B-A)*FB
      Q = FA - FB
      IF (P .GE. 0.D0) GO TO 5
      P = -P
      Q = -Q
C
C   Update A and check for satisfactory reduction in the size of the
C   bracketing interval.  If not, perform bisection.
C
    5 A = B
      FA = FB
      IC = IC + 1
      IF (IC .LT. 4) GO TO 6
      IF (8.0D0*ACMB .GE. ACBS) GO TO 8
      IC = 0
      ACBS = ACMB
C
C   Test for too small a change.
C
    6 IF (P .GT. ABS(Q)*TOL) GO TO 7
C
C   Increment by TOLerance.
C
      B = B + SIGN(TOL,CMB)
      GO TO 9
C
C   Root ought to be between B and (C+B)/2.
C
    7 IF (P .GE. CMB*Q) GO TO 8
C
C   Use secant rule.
C
      B = B + P/Q
      GO TO 9
C
C   Use bisection (C+B)/2.
C
    8 B = B + CMB
C
C   Have completed computation for new iterate B.
C
    9 T = B
      FB = F(T)
      KOUNT = KOUNT + 1
C
C   Decide whether next step is interpolation or extrapolation.
C
      IF (SIGN(1.0D0,FB) .NE. SIGN(1.0D0,FC)) GO TO 3
      C = A
      FC = FA
      GO TO 3
C
C   Finished.  Process results for proper setting of IFLAG.
C
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
      END
      SUBROUTINE DFZER2 (F, B, C, R, RE, AE, IFLAG,X)
C***MODIFIED VERSION OF DFZERO.  PASS ALONG DATA ARRAY X
C***BEGIN PROLOGUE  DFZERO
C***PURPOSE  Search for a zero of a function F(X) in a given interval
C            (B,C).  It is designed primarily for problems where F(B)
C            and F(C) have opposite signs.
C***LIBRARY   SLATEC
C***CATEGORY  F1B
C***TYPE      DOUBLE PRECISION (FZERO-S, DFZERO-D)
C***KEYWORDS  BISECTION, NONLINEAR, ROOTS, ZEROS
C***AUTHOR  Shampine, L. F., (SNLA)
C           Watts, H. A., (SNLA)
C***DESCRIPTION
C
C     DFZERO searches for a zero of a DOUBLE PRECISION function F(X)
C     between the given DOUBLE PRECISION values B and C until the width
C     of the interval (B,C) has collapsed to within a tolerance
C     specified by the stopping criterion,
C        ABS(B-C) .LE. 2.*(RW*ABS(B)+AE).
C     The method used is an efficient combination of bisection and the
C     secant rule and is due to T. J. Dekker.
C
C     Description Of Arguments
C
C   F     :EXT   - Name of the DOUBLE PRECISION external function.  This
C                  name must be in an EXTERNAL statement in the calling
C                  program.  F must be a function of one DOUBLE
C                  PRECISION argument.
C
C   B     :INOUT - One end of the DOUBLE PRECISION interval (B,C).  The
C                  value returned for B usually is the better
C                  approximation to a zero of F.
C
C   C     :INOUT - The other end of the DOUBLE PRECISION interval (B,C)
C
C   R     :IN    - A (better) DOUBLE PRECISION guess of a zero of F
C                  which could help in speeding up convergence.  If F(B)
C                  and F(R) have opposite signs, a root will be found in
C                  the interval (B,R);  if not, but F(R) and F(C) have
C                  opposite signs, a root will be found in the interval
C                  (R,C);  otherwise, the interval (B,C) will be
C                  searched for a possible root.  When no better guess
C                  is known, it is recommended that R be set to B or C,
C                  since if R is not interior to the interval (B,C), it
C                  will be ignored.
C
C   RE    :IN    - Relative error used for RW in the stopping criterion.
C                  If the requested RE is less than machine precision,
C                  then RW is set to approximately machine precision.
C
C   AE    :IN    - Absolute error used in the stopping criterion.  If
C                  the given interval (B,C) contains the origin, then a
C                  nonzero value should be chosen for AE.
C
C   IFLAG :OUT   - A status code.  User must check IFLAG after each
C                  call.  Control returns to the user from DFZERO in all
C                  cases.
C
C                1  B is within the requested tolerance of a zero.
C                   The interval (B,C) collapsed to the requested
C                   tolerance, the function changes sign in (B,C), and
C                   F(X) decreased in magnitude as (B,C) collapsed.
C
C                2  F(B) = 0.  However, the interval (B,C) may not have
C                   collapsed to the requested tolerance.
C
C                3  B may be near a singular point of F(X).
C                   The interval (B,C) collapsed to the requested tol-
C                   erance and the function changes sign in (B,C), but
C                   F(X) increased in magnitude as (B,C) collapsed, i.e.
C                     ABS(F(B out)) .GT. MAX(ABS(F(B in)),ABS(F(C in)))
C
C                4  No change in sign of F(X) was found although the
C                   interval (B,C) collapsed to the requested tolerance.
C                   The user must examine this case and decide whether
C                   B is near a local minimum of F(X), or B is near a
C                   zero of even multiplicity, or neither of these.
C
C                5  Too many (.GT. 500) function evaluations used.
C
C***REFERENCES  L. F. Shampine and H. A. Watts, FZERO, a root-solving
C                 code, Report SC-TM-70-631, Sandia Laboratories,
C                 September 1970.
C               T. J. Dekker, Finding a zero by means of successive
C                 linear interpolation, Constructive Aspects of the
C                 Fundamental Theorem of Algebra, edited by B. Dejon
C                 and P. Henrici, Wiley-Interscience, 1969.
C***ROUTINES CALLED  D1MACH
C***REVISION HISTORY  (YYMMDD)
C   700901  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  DFZERO
CCCCC DOUBLE PRECISION A,ACBS,ACMB,AE,AW,B,C,CMB,D1MACH,ER,
      DOUBLE PRECISION A,ACBS,ACMB,AE,AW,B,C,CMB,ER,
     +                 F,FA,FB,FC,FX,FZ,P,Q,R,RE,RW,T,TOL,Z
      DOUBLE PRECISION X(*)
      INTEGER IC,IFLAG,KOUNT
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
C***FIRST EXECUTABLE STATEMENT  DFZERO
C
C   ER is two times the computer unit roundoff value which is defined
C   here by the function D1MACH.
C
      ER = 2.0D0 * D1MACH(4)
C
C   Initialize.
C
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
C
    3 IF (ABS(FC) .GE. ABS(FB)) GO TO 4
C
C   Perform interchange.
C
      A = B
      FA = FB
      B = C
      FB = FC
      C = A
      FC = FA
C
    4 CMB = 0.5D0*(C-B)
      ACMB = ABS(CMB)
      TOL = RW*ABS(B) + AW
C
C   Test stopping criterion and function count.
C
      IF (ACMB .LE. TOL) GO TO 10
      IF (FB .EQ. 0.D0) GO TO 11
      IF (KOUNT .GE. 500) GO TO 14
C
C   Calculate new iterate implicitly as B+P/Q, where we arrange
C   P .GE. 0.  The implicit form is used to prevent overflow.
C
      P = (B-A)*FB
      Q = FA - FB
      IF (P .GE. 0.D0) GO TO 5
      P = -P
      Q = -Q
C
C   Update A and check for satisfactory reduction in the size of the
C   bracketing interval.  If not, perform bisection.
C
    5 A = B
      FA = FB
      IC = IC + 1
      IF (IC .LT. 4) GO TO 6
      IF (8.0D0*ACMB .GE. ACBS) GO TO 8
      IC = 0
      ACBS = ACMB
C
C   Test for too small a change.
C
    6 IF (P .GT. ABS(Q)*TOL) GO TO 7
C
C   Increment by TOLerance.
C
      B = B + SIGN(TOL,CMB)
      GO TO 9
C
C   Root ought to be between B and (C+B)/2.
C
    7 IF (P .GE. CMB*Q) GO TO 8
C
C   Use secant rule.
C
      B = B + P/Q
      GO TO 9
C
C   Use bisection (C+B)/2.
C
    8 B = B + CMB
C
C   Have completed computation for new iterate B.
C
    9 T = B
      FB = F(T,X)
      KOUNT = KOUNT + 1
C
C   Decide whether next step is interpolation or extrapolation.
C
      IF (SIGN(1.0D0,FB) .NE. SIGN(1.0D0,FC)) GO TO 3
      C = A
      FC = FA
      GO TO 3
C
C   Finished.  Process results for proper setting of IFLAG.
C
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
      END
      SUBROUTINE DFZER3 (F, B, C, R, RE, AE, IFLAG,X)
C***COPY OF DFZER2.  A WEIBULL MLE PROBLEM REQUIRES THE ROOT
C***FUNCTION TO COMPUTE A NEEDED PARAMETER BY FINDING ANOTHER
C***ROOT.  SINCE FORTRAN 77 DOES NOT ALLOW RECURSION, IMPLEMENT
C***VIA A SEPARATE ROUTINE.
C***MODIFIED VERSION OF DFZERO.  PASS ALONG DATA ARRAY X
C***BEGIN PROLOGUE  DFZERO
C***PURPOSE  Search for a zero of a function F(X) in a given interval
C            (B,C).  It is designed primarily for problems where F(B)
C            and F(C) have opposite signs.
C***LIBRARY   SLATEC
C***CATEGORY  F1B
C***TYPE      DOUBLE PRECISION (FZERO-S, DFZERO-D)
C***KEYWORDS  BISECTION, NONLINEAR, ROOTS, ZEROS
C***AUTHOR  Shampine, L. F., (SNLA)
C           Watts, H. A., (SNLA)
C***DESCRIPTION
C
C     DFZERO searches for a zero of a DOUBLE PRECISION function F(X)
C     between the given DOUBLE PRECISION values B and C until the width
C     of the interval (B,C) has collapsed to within a tolerance
C     specified by the stopping criterion,
C        ABS(B-C) .LE. 2.*(RW*ABS(B)+AE).
C     The method used is an efficient combination of bisection and the
C     secant rule and is due to T. J. Dekker.
C
C     Description Of Arguments
C
C   F     :EXT   - Name of the DOUBLE PRECISION external function.  This
C                  name must be in an EXTERNAL statement in the calling
C                  program.  F must be a function of one DOUBLE
C                  PRECISION argument.
C
C   B     :INOUT - One end of the DOUBLE PRECISION interval (B,C).  The
C                  value returned for B usually is the better
C                  approximation to a zero of F.
C
C   C     :INOUT - The other end of the DOUBLE PRECISION interval (B,C)
C
C   R     :IN    - A (better) DOUBLE PRECISION guess of a zero of F
C                  which could help in speeding up convergence.  If F(B)
C                  and F(R) have opposite signs, a root will be found in
C                  the interval (B,R);  if not, but F(R) and F(C) have
C                  opposite signs, a root will be found in the interval
C                  (R,C);  otherwise, the interval (B,C) will be
C                  searched for a possible root.  When no better guess
C                  is known, it is recommended that R be set to B or C,
C                  since if R is not interior to the interval (B,C), it
C                  will be ignored.
C
C   RE    :IN    - Relative error used for RW in the stopping criterion.
C                  If the requested RE is less than machine precision,
C                  then RW is set to approximately machine precision.
C
C   AE    :IN    - Absolute error used in the stopping criterion.  If
C                  the given interval (B,C) contains the origin, then a
C                  nonzero value should be chosen for AE.
C
C   IFLAG :OUT   - A status code.  User must check IFLAG after each
C                  call.  Control returns to the user from DFZERO in all
C                  cases.
C
C                1  B is within the requested tolerance of a zero.
C                   The interval (B,C) collapsed to the requested
C                   tolerance, the function changes sign in (B,C), and
C                   F(X) decreased in magnitude as (B,C) collapsed.
C
C                2  F(B) = 0.  However, the interval (B,C) may not have
C                   collapsed to the requested tolerance.
C
C                3  B may be near a singular point of F(X).
C                   The interval (B,C) collapsed to the requested tol-
C                   erance and the function changes sign in (B,C), but
C                   F(X) increased in magnitude as (B,C) collapsed, i.e.
C                     ABS(F(B out)) .GT. MAX(ABS(F(B in)),ABS(F(C in)))
C
C                4  No change in sign of F(X) was found although the
C                   interval (B,C) collapsed to the requested tolerance.
C                   The user must examine this case and decide whether
C                   B is near a local minimum of F(X), or B is near a
C                   zero of even multiplicity, or neither of these.
C
C                5  Too many (.GT. 500) function evaluations used.
C
C***REFERENCES  L. F. Shampine and H. A. Watts, FZERO, a root-solving
C                 code, Report SC-TM-70-631, Sandia Laboratories,
C                 September 1970.
C               T. J. Dekker, Finding a zero by means of successive
C                 linear interpolation, Constructive Aspects of the
C                 Fundamental Theorem of Algebra, edited by B. Dejon
C                 and P. Henrici, Wiley-Interscience, 1969.
C***ROUTINES CALLED  D1MACH
C***REVISION HISTORY  (YYMMDD)
C   700901  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  DFZERO
CCCCC DOUBLE PRECISION A,ACBS,ACMB,AE,AW,B,C,CMB,D1MACH,ER,
      DOUBLE PRECISION A,ACBS,ACMB,AE,AW,B,C,CMB,ER,
     +                 F,FA,FB,FC,FX,FZ,P,Q,R,RE,RW,T,TOL,Z
      DOUBLE PRECISION X(*)
      INTEGER IC,IFLAG,KOUNT
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
C***FIRST EXECUTABLE STATEMENT  DFZERO
C
C   ER is two times the computer unit roundoff value which is defined
C   here by the function D1MACH.
C
      ER = 2.0D0 * D1MACH(4)
C
C   Initialize.
C
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
C
    3 IF (ABS(FC) .GE. ABS(FB)) GO TO 4
C
C   Perform interchange.
C
      A = B
      FA = FB
      B = C
      FB = FC
      C = A
      FC = FA
C
    4 CMB = 0.5D0*(C-B)
      ACMB = ABS(CMB)
      TOL = RW*ABS(B) + AW
C
C   Test stopping criterion and function count.
C
      IF (ACMB .LE. TOL) GO TO 10
      IF (FB .EQ. 0.D0) GO TO 11
      IF (KOUNT .GE. 500) GO TO 14
C
C   Calculate new iterate implicitly as B+P/Q, where we arrange
C   P .GE. 0.  The implicit form is used to prevent overflow.
C
      P = (B-A)*FB
      Q = FA - FB
      IF (P .GE. 0.D0) GO TO 5
      P = -P
      Q = -Q
C
C   Update A and check for satisfactory reduction in the size of the
C   bracketing interval.  If not, perform bisection.
C
    5 A = B
      FA = FB
      IC = IC + 1
      IF (IC .LT. 4) GO TO 6
      IF (8.0D0*ACMB .GE. ACBS) GO TO 8
      IC = 0
      ACBS = ACMB
C
C   Test for too small a change.
C
    6 IF (P .GT. ABS(Q)*TOL) GO TO 7
C
C   Increment by TOLerance.
C
      B = B + SIGN(TOL,CMB)
      GO TO 9
C
C   Root ought to be between B and (C+B)/2.
C
    7 IF (P .GE. CMB*Q) GO TO 8
C
C   Use secant rule.
C
      B = B + P/Q
      GO TO 9
C
C   Use bisection (C+B)/2.
C
    8 B = B + CMB
C
C   Have completed computation for new iterate B.
C
    9 T = B
      FB = F(T,X)
      KOUNT = KOUNT + 1
C
C   Decide whether next step is interpolation or extrapolation.
C
      IF (SIGN(1.0D0,FB) .NE. SIGN(1.0D0,FC)) GO TO 3
      C = A
      FC = FA
      GO TO 3
C
C   Finished.  Process results for proper setting of IFLAG.
C
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
      END
      DOUBLE PRECISION FUNCTION DGAMI (A, X)
C***BEGIN PROLOGUE  DGAMI
C***PURPOSE  Evaluate the incomplete Gamma function.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7E
C***TYPE      DOUBLE PRECISION (GAMI-S, DGAMI-D)
C***KEYWORDS  FNLIB, INCOMPLETE GAMMA FUNCTION, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C Evaluate the incomplete gamma function defined by
C
C DGAMI = integral from T = 0 to X of EXP(-T) * T**(A-1.0) .
C
C DGAMI is evaluated for positive values of A and non-negative values
C of X.  A slight deterioration of 2 or 3 digits accuracy will occur
C when DGAMI is very large or very small, because logarithmic variables
C are used.  The function and both arguments are double precision.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  DGAMIT, DLNGAM, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C***END PROLOGUE  DGAMI
      DOUBLE PRECISION A, X, FACTOR, DLNGAM, DGAMIT
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C***FIRST EXECUTABLE STATEMENT  DGAMI
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
C
      DGAMI = 0.D0
      IF (X.EQ.0.0D0) RETURN
C
C THE ONLY ERROR POSSIBLE IN THE EXPRESSION BELOW IS A FATAL OVERFLOW.
C
      FACTOR = EXP (DLNGAM(A) + A*LOG(X))
C
      DGAMI = FACTOR * DGAMIT (A, X)
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION DGAMIP (A, X)
C***BEGIN PROLOGUE  DGAMIP
C***PURPOSE  Evaluate the incomplete Gamma function.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7E
C***TYPE      DOUBLE PRECISION (GAMI-S, DGAMIP-D)
C***KEYWORDS  FNLIB, INCOMPLETE GAMMA FUNCTION, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C Evaluate the incomplete gamma function defined by
C
C DGAMIP = integral from T = 0 to X of EXP(-T) * T**(A-1.0) .
C
C DGAMIP is evaluated for positive values of A and non-negative values
C of X.  A slight deterioration of 2 or 3 digits accuracy will occur
C when DGAMIP is very large or very small, because logarithmic variables
C are used.  The function and both arguments are double precision.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  DGAMIPT, DLNGAM, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C***END PROLOGUE  DGAMIP
CCCCC DOUBLE PRECISION A, X, FACTOR, DLNGAM, DGAMIT
      DOUBLE PRECISION A, X, FACTOR, DGAMIT
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C***FIRST EXECUTABLE STATEMENT  DGAMIP
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
C
      DGAMIP = 0.D0
      IF (X.EQ.0.0D0) RETURN
C
C THE ONLY ERROR POSSIBLE IN THE EXPRESSION BELOW IS A FATAL OVERFLOW.
CCCCC NOTE:  FOR DATAPLOT, WANT FORM OF INCOMPLETE GAMMA THAT HAS
CCCCC        DIVISION BY COMPLETE GAMMA FUNCTION INCLUDED!
C
CCCCC FACTOR = EXP (DLNGAM(A) + A*LOG(X))
      FACTOR = EXP(A*LOG(X))
C
      DGAMIP = FACTOR * DGAMIT (A, X)
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION DGAMIC (A, X)
C***BEGIN PROLOGUE  DGAMIC
C***PURPOSE  Calculate the complementary incomplete Gamma function.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7E
C***TYPE      DOUBLE PRECISION (GAMIC-S, DGAMIC-D)
C***KEYWORDS  COMPLEMENTARY INCOMPLETE GAMMA FUNCTION, FNLIB,
C             SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C   Evaluate the complementary incomplete Gamma function
C
C   DGAMIC = integral from X to infinity of EXP(-T) * T**(A-1.)  .
C
C   DGAMIC is evaluated for arbitrary real values of A and for non-
C   negative values of X (even though DGAMIC is defined for X .LT.
C   0.0), except that for X = 0 and A .LE. 0.0, DGAMIC is undefined.
C
C   DGAMIC, A, and X are DOUBLE PRECISION.
C
C   A slight deterioration of 2 or 3 digits accuracy will occur when
C   DGAMIC is very large or very small in absolute value, because log-
C   arithmic variables are used.  Also, if the parameter A is very close
C   to a negative INTEGER (but not a negative integer), there is a loss
C   of accuracy, which is reported if the result is less than half
C   machine precision.
C
C***REFERENCES  W. Gautschi, A computational procedure for incomplete
C                 gamma functions, ACM Transactions on Mathematical
C                 Software 5, 4 (December 1979), pp. 466-481.
C               W. Gautschi, Incomplete gamma functions, Algorithm 542,
C                 ACM Transactions on Mathematical Software 5, 4
C                 (December 1979), pp. 482-489.
C***ROUTINES CALLED  D1MACH, D9GMIC, D9GMIT, D9LGIC, D9LGIT, DLGAMS,
C                    DLNGAM, XERCLR, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   920528  DESCRIPTION and REFERENCES sections revised.  (WRB)
C***END PROLOGUE  DGAMIC
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
      DOUBLE PRECISION A, X, AEPS, AINTA, ALGAP1, ALNEPS, ALNGS, ALX,
     1  BOT, E, EPS, GSTAR, H, SGA, SGNG, SGNGAM, SGNGS, SQEPS, T,
     2  DLNGAM, D9GMIC, D9GMIT, D9LGIC, D9LGIT
      LOGICAL FIRST
      SAVE EPS, SQEPS, ALNEPS, BOT, FIRST
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  DGAMIC
C
      DGAMIC = 0.D0
C
      IF (FIRST) THEN
         EPS = 0.5D0*D1MACH(3)
         SQEPS = SQRT(D1MACH(4))
         ALNEPS = -LOG (D1MACH(3))
         BOT = LOG (D1MACH(1))
      ENDIF
      FIRST = .FALSE.
C
      IF (X .LT. 0.D0) THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        DGAMIC = 0.D0
        RETURN
      ENDIF
   12 FORMAT('***** ERROR FROM DGAMIC.  X MUST BE GREATER THAN OR ',
     1       'EQUAL TO ZERO. ****')
C
      IF (X.GT.0.D0) GO TO 20
      IF (A .LE. 0.D0) THEN
        WRITE(ICOUT,11)
        CALL DPWRST('XXX','BUG ')
        DGAMIC = 0.D0
        RETURN
      ENDIF
   11 FORMAT('***** ERROR FROM DGAMI.  GAMMAIC IS UNDEFINED SINCE X ',
     1       'ZERO AND A IS NON-POSITIVE. *****')
C
      DGAMIC = EXP (DLNGAM(A+1.D0) - LOG(A))
      RETURN
C
 20   ALX = LOG (X)
      SGA = 1.0D0
      IF (A.NE.0.D0) SGA = SIGN (1.0D0, A)
      AINTA = AINT (A + 0.5D0*SGA)
      AEPS = A - AINTA
C
      IZERO = 0
      IF (X.GE.1.0D0) GO TO 40
C
      IF (A.GT.0.5D0 .OR. ABS(AEPS).GT.0.001D0) GO TO 30
      E = 2.0D0
      IF (-AINTA.GT.1.D0) E = 2.D0*(-AINTA+2.D0)/(AINTA*AINTA-1.0D0)
      E = E - ALX * X**(-0.001D0)
      IF (E*ABS(AEPS).GT.EPS) GO TO 30
C
      DGAMIC = D9GMIC (A, X, ALX)
      RETURN
C
 30   CALL DLGAMS (A+1.0D0, ALGAP1, SGNGAM)
      GSTAR = D9GMIT (A, X, ALGAP1, SGNGAM, ALX)
      IF (GSTAR.EQ.0.D0) IZERO = 1
      IF (GSTAR.NE.0.D0) ALNGS = LOG (ABS(GSTAR))
      IF (GSTAR.NE.0.D0) SGNGS = SIGN (1.0D0, GSTAR)
      GO TO 50
C
 40   IF (A.LT.X) DGAMIC = EXP (D9LGIC(A, X, ALX))
      IF (A.LT.X) RETURN
C
      SGNGAM = 1.0D0
      ALGAP1 = DLNGAM (A+1.0D0)
      SGNGS = 1.0D0
      ALNGS = D9LGIT (A, X, ALGAP1)
C
C EVALUATION OF DGAMIC(A,X) IN TERMS OF TRICOMI-S INCOMPLETE GAMMA FN.
C
 50   H = 1.D0
      IF (IZERO.EQ.1) GO TO 60
C
      T = A*ALX + ALNGS
      IF (T.GT.ALNEPS) GO TO 70
      IF (T.GT.(-ALNEPS)) H = 1.0D0 - SGNGS*EXP(T)
C
CCCCC IF (ABS(H).LT.SQEPS) CALL XERCLR
      IF (ABS(H) .LT. SQEPS) THEN
        WRITE(ICOUT,51)
        CALL DPWRST('XXX','BUG ')
      ENDIF
   51 FORMAT('***** WARNING FROM DGAMIC, RESULT IS LESS THAN HALF ',
     1       'PRECISION.  ****')
C
 60   SGNG = SIGN (1.0D0, H) * SGA * SGNGAM
      T = LOG(ABS(H)) + ALGAP1 - LOG(ABS(A))
CCCCC IF (T.LT.BOT) CALL XERCLR
      DGAMIC = SGNG * EXP(T)
      RETURN
C
 70   SGNG = -SGNGS * SGA * SGNGAM
      T = T + ALGAP1 - LOG(ABS(A))
CCCCC IF (T.LT.BOT) CALL XERCLR
      DGAMIC = SGNG * EXP(T)
      RETURN
C
      END
      DOUBLE PRECISION FUNCTION DGAMIT (A, X)
C***BEGIN PROLOGUE  DGAMIT
C***PURPOSE  Calculate Tricomi's form of the incomplete Gamma function.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7E
C***TYPE      DOUBLE PRECISION (GAMIT-S, DGAMIT-D)
C***KEYWORDS  COMPLEMENTARY INCOMPLETE GAMMA FUNCTION, FNLIB,
C             SPECIAL FUNCTIONS, TRICOMI
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C   Evaluate Tricomi's incomplete Gamma function defined by
C
C   DGAMIT = X**(-A)/GAMMA(A) * integral from 0 to X of EXP(-T) *
C              T**(A-1.)
C
C   for A .GT. 0.0 and by analytic continuation for A .LE. 0.0.
C   GAMMA(X) is the complete gamma function of X.
C
C   DGAMIT is evaluated for arbitrary real values of A and for non-
C   negative values of X (even though DGAMIT is defined for X .LT.
C   0.0), except that for X = 0 and A .LE. 0.0, DGAMIT is infinite,
C   which is a fatal error.
C
C   The function and both arguments are DOUBLE PRECISION.
C
C   A slight deterioration of 2 or 3 digits accuracy will occur when
C   DGAMIT is very large or very small in absolute value, because log-
C   arithmic variables are used.  Also, if the parameter  A  is very
C   close to a negative integer (but not a negative integer), there is
C   a loss of accuracy, which is reported if the result is less than
C   half machine precision.
C
C***REFERENCES  W. Gautschi, A computational procedure for incomplete
C                 gamma functions, ACM Transactions on Mathematical
C                 Software 5, 4 (December 1979), pp. 466-481.
C               W. Gautschi, Incomplete gamma functions, Algorithm 542,
C                 ACM Transactions on Mathematical Software 5, 4
C                 (December 1979), pp. 482-489.
C***ROUTINES CALLED  D1MACH, D9GMIT, D9LGIC, D9LGIT, DGAMR, DLGAMS,
C                    DLNGAM, XERCLR, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   920528  DESCRIPTION and REFERENCES sections revised.  (WRB)
C***END PROLOGUE  DGAMIT
      DOUBLE PRECISION A, X, AEPS, AINTA, ALGAP1, ALNEPS, ALNG, ALX,
     1  BOT, H, SGA, SGNGAM, SQEPS, T, DGAMR, D9GMIT, D9LGIT,
     2  DLNGAM, D9LGIC
      LOGICAL FIRST
      SAVE ALNEPS, SQEPS, BOT, FIRST
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  DGAMIT
      IF (FIRST) THEN
         ALNEPS = -LOG (D1MACH(3))
         SQEPS = SQRT(D1MACH(4))
         BOT = LOG (D1MACH(1))
      ENDIF
      FIRST = .FALSE.
C
      IF (X .LT. 0.D0) THEN
        WRITE(ICOUT,11)
        CALL DPWRST('XXX','BUG ')
        DGAMIT = 0.D0
        RETURN
      ENDIF
   11 FORMAT('***** ERROR FROM DGAMIT.  X IS NEGATIVE.  *****')
C
      IF (X.NE.0.D0) ALX = LOG (X)
      SGA = 1.0D0
      IF (A.NE.0.D0) SGA = SIGN (1.0D0, A)
      AINTA = AINT (A + 0.5D0*SGA)
      AEPS = A - AINTA
C
      IF (X.GT.0.D0) GO TO 20
      DGAMIT = 0.0D0
      IF (AINTA.GT.0.D0 .OR. AEPS.NE.0.D0) DGAMIT = DGAMR(A+1.0D0)
      RETURN
C
 20   IF (X.GT.1.D0) GO TO 30
      IF (A.GE.(-0.5D0) .OR. AEPS.NE.0.D0) CALL DLGAMS (A+1.0D0, ALGAP1,
     1  SGNGAM)
      DGAMIT = D9GMIT (A, X, ALGAP1, SGNGAM, ALX)
      RETURN
C
 30   IF (A.LT.X) GO TO 40
      T = D9LGIT (A, X, DLNGAM(A+1.0D0))
CCCCC IF (T.LT.BOT) CALL XERCLR
      DGAMIT = EXP (T)
      RETURN
C
 40   ALNG = D9LGIC (A, X, ALX)
C
C EVALUATE DGAMIT IN TERMS OF LOG (DGAMIC (A, X))
C
      H = 1.0D0
      IF (AEPS.EQ.0.D0 .AND. AINTA.LE.0.D0) GO TO 50
C
      CALL DLGAMS (A+1.0D0, ALGAP1, SGNGAM)
      T = LOG (ABS(A)) + ALNG - ALGAP1
      IF (T.GT.ALNEPS) GO TO 60
C
      IF (T.GT.(-ALNEPS)) H = 1.0D0 - SGA * SGNGAM * EXP(T)
      IF (ABS(H).GT.SQEPS) GO TO 50
C
      WRITE(ICOUT,41)
 41   FORMAT('***** WARNING FROM DGAMIT.  RESULT IS LESS THAN ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,42)
 42   FORMAT('      HALF PRECISION.                           *****')
      CALL DPWRST('XXX','BUG ')
C
 50   T = -A*ALX + LOG(ABS(H))
CCCCC IF (T.LT.BOT) CALL XERCLR
      DGAMIT = SIGN (EXP(T), H)
      RETURN
C
 60   T = T - A*ALX
CCCCC IF (T.LT.BOT) CALL XERCLR
      DGAMIT = -SGA * SGNGAM * EXP(T)
      RETURN
C
      END
      SUBROUTINE DGAMLM (XMIN, XMAX)
C***BEGIN PROLOGUE  DGAMLM
C***PURPOSE  Compute the minimum and maximum bounds for the argument in
C            the Gamma function.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7A, R2
C***TYPE      DOUBLE PRECISION (GAMLIM-S, DGAMLM-D)
C***KEYWORDS  COMPLETE GAMMA FUNCTION, FNLIB, LIMITS, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C Calculate the minimum and maximum legal bounds for X in gamma(X).
C XMIN and XMAX are not the only bounds, but they are the only non-
C trivial ones to calculate.
C
C             Output Arguments --
C XMIN   double precision minimum legal value of X in gamma(X).  Any
C        smaller value of X might result in underflow.
C XMAX   double precision maximum legal value of X in gamma(X).  Any
C        larger value of X might cause overflow.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C***END PROLOGUE  DGAMLM
      DOUBLE PRECISION XMIN, XMAX, ALNBIG, ALNSML, XLN, XOLD
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
C***FIRST EXECUTABLE STATEMENT  DGAMLM
      ALNSML = LOG(D1MACH(1))
      XMIN = -ALNSML
      DO 10 I=1,10
        XOLD = XMIN
        XLN = LOG(XMIN)
        XMIN = XMIN - XMIN*((XMIN+0.5D0)*XLN - XMIN - 0.2258D0 + ALNSML)
     1    / (XMIN*XLN+0.5D0)
        IF (ABS(XMIN-XOLD).LT.0.005D0) GO TO 20
 10   CONTINUE
      WRITE(ICOUT,11)
 11   FORMAT('***** ERROR FROM DGAMLM.  UNABLE TO FIND XMIN.  ******')
      CALL DPWRST('XXX','BUG ')
      RETURN
C
 20   XMIN = -XMIN + 0.01D0
C
      ALNBIG = LOG (D1MACH(2))
      XMAX = ALNBIG
      DO 30 I=1,10
        XOLD = XMAX
        XLN = LOG(XMAX)
        XMAX = XMAX - XMAX*((XMAX-0.5D0)*XLN - XMAX + 0.9189D0 - ALNBIG)
     1    / (XMAX*XLN-0.5D0)
        IF (ABS(XMAX-XOLD).LT.0.005D0) GO TO 40
 30   CONTINUE
      WRITE(ICOUT,21)
 21   FORMAT('***** ERROR FROM DGAMLM.  UNABLE TO FIND XMAX.  ******')
      CALL DPWRST('XXX','BUG ')
      RETURN
C
 40   XMAX = XMAX - 0.01D0
      XMIN = MAX (XMIN, -XMAX+1.D0)
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION DGAMMA (X)
C***BEGIN PROLOGUE  DGAMMA
C***PURPOSE  Compute the complete Gamma function.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7A
C***TYPE      DOUBLE PRECISION (GAMMA-S, DGAMMA-D, CGAMMA-C)
C***KEYWORDS  COMPLETE GAMMA FUNCTION, FNLIB, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C DGAMMA(X) calculates the double precision complete Gamma function
C for double precision argument X.
C
C Series for GAM        on the interval  0.          to  1.00000E+00
C                                        with weighted error   5.79E-32
C                                         log weighted error  31.24
C                               significant figures required  30.00
C                                    decimal places required  32.05
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, D9LGMC, DCSEVL, DGAMLM, INITDS, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890911  Removed unnecessary intrinsics.  (WRB)
C   890911  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   920618  Removed space from variable name.  (RWC, WRB)
C***END PROLOGUE  DGAMMA
      DOUBLE PRECISION X, GAMCS(42), DXREL, PI, SINPIY, SQ2PIL, XMAX,
     1  XMIN, Y, D9LGMC, DCSEVL
      LOGICAL FIRST
C
      SAVE GAMCS, PI, SQ2PIL, NGAM, XMIN, XMAX, DXREL, FIRST
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
      DATA GAMCS(  1) / +.8571195590 9893314219 2006239994 2 D-2      /
      DATA GAMCS(  2) / +.4415381324 8410067571 9131577165 2 D-2      /
      DATA GAMCS(  3) / +.5685043681 5993633786 3266458878 9 D-1      /
      DATA GAMCS(  4) / -.4219835396 4185605010 1250018662 4 D-2      /
      DATA GAMCS(  5) / +.1326808181 2124602205 8400679635 2 D-2      /
      DATA GAMCS(  6) / -.1893024529 7988804325 2394702388 6 D-3      /
      DATA GAMCS(  7) / +.3606925327 4412452565 7808221722 5 D-4      /
      DATA GAMCS(  8) / -.6056761904 4608642184 8554829036 5 D-5      /
      DATA GAMCS(  9) / +.1055829546 3022833447 3182350909 3 D-5      /
      DATA GAMCS( 10) / -.1811967365 5423840482 9185589116 6 D-6      /
      DATA GAMCS( 11) / +.3117724964 7153222777 9025459316 9 D-7      /
      DATA GAMCS( 12) / -.5354219639 0196871408 7408102434 7 D-8      /
      DATA GAMCS( 13) / +.9193275519 8595889468 8778682594 0 D-9      /
      DATA GAMCS( 14) / -.1577941280 2883397617 6742327395 3 D-9      /
      DATA GAMCS( 15) / +.2707980622 9349545432 6654043308 9 D-10     /
      DATA GAMCS( 16) / -.4646818653 8257301440 8166105893 3 D-11     /
      DATA GAMCS( 17) / +.7973350192 0074196564 6076717535 9 D-12     /
      DATA GAMCS( 18) / -.1368078209 8309160257 9949917230 9 D-12     /
      DATA GAMCS( 19) / +.2347319486 5638006572 3347177168 8 D-13     /
      DATA GAMCS( 20) / -.4027432614 9490669327 6657053469 9 D-14     /
      DATA GAMCS( 21) / +.6910051747 3721009121 3833697525 7 D-15     /
      DATA GAMCS( 22) / -.1185584500 2219929070 5238712619 2 D-15     /
      DATA GAMCS( 23) / +.2034148542 4963739552 0102605193 2 D-16     /
      DATA GAMCS( 24) / -.3490054341 7174058492 7401294910 8 D-17     /
      DATA GAMCS( 25) / +.5987993856 4853055671 3505106602 6 D-18     /
      DATA GAMCS( 26) / -.1027378057 8722280744 9006977843 1 D-18     /
      DATA GAMCS( 27) / +.1762702816 0605298249 4275966074 8 D-19     /
      DATA GAMCS( 28) / -.3024320653 7353062609 5877211204 2 D-20     /
      DATA GAMCS( 29) / +.5188914660 2183978397 1783355050 6 D-21     /
      DATA GAMCS( 30) / -.8902770842 4565766924 4925160106 6 D-22     /
      DATA GAMCS( 31) / +.1527474068 4933426022 7459689130 6 D-22     /
      DATA GAMCS( 32) / -.2620731256 1873629002 5732833279 9 D-23     /
      DATA GAMCS( 33) / +.4496464047 8305386703 3104657066 6 D-24     /
      DATA GAMCS( 34) / -.7714712731 3368779117 0390152533 3 D-25     /
      DATA GAMCS( 35) / +.1323635453 1260440364 8657271466 6 D-25     /
      DATA GAMCS( 36) / -.2270999412 9429288167 0231381333 3 D-26     /
      DATA GAMCS( 37) / +.3896418998 0039914493 2081663999 9 D-27     /
      DATA GAMCS( 38) / -.6685198115 1259533277 9212799999 9 D-28     /
      DATA GAMCS( 39) / +.1146998663 1400243843 4761386666 6 D-28     /
      DATA GAMCS( 40) / -.1967938586 3451346772 9510399999 9 D-29     /
      DATA GAMCS( 41) / +.3376448816 5853380903 3489066666 6 D-30     /
      DATA GAMCS( 42) / -.5793070335 7821357846 2549333333 3 D-31     /
      DATA PI / 3.1415926535 8979323846 2643383279 50 D0 /
      DATA SQ2PIL / 0.9189385332 0467274178 0329736405 62 D0 /
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  DGAMMA
      IF (FIRST) THEN
         NGAM = INITDS (GAMCS, 42, 0.1*REAL(D1MACH(3)) )
C
         CALL DGAMLM (XMIN, XMAX)
         DXREL = SQRT(D1MACH(4))
      ENDIF
      FIRST = .FALSE.
C
      Y = ABS(X)
      IF (Y.GT.10.D0) GO TO 50
C
C COMPUTE GAMMA(X) FOR -XBND .LE. X .LE. XBND.  REDUCE INTERVAL AND FIND
C GAMMA(1+Y) FOR 0.0 .LE. Y .LT. 1.0 FIRST OF ALL.
C
      N = INT(X+0.1)
      IF (X.LT.0.D0) N = N - 1
      Y = X - N
      N = N - 1
      DGAMMA = 0.9375D0 + DCSEVL (2.D0*Y-1.D0, GAMCS, NGAM)
      IF (N.EQ.0) RETURN
C
      IF (N.GT.0) GO TO 30
C
C COMPUTE GAMMA(X) FOR X .LT. 1.0
C
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
CCC22 FORMAT('      HALF PRECISION BECAUSE X IS TOO NEAR A ')
CCC23 FORMAT('      NEGATIVE INTEGER.                          *****')
C
      DO 20 I=1,N
        DGAMMA = DGAMMA/(X+I-1 )
 20   CONTINUE
      RETURN
C
C GAMMA(X) FOR X .GE. 2.0 AND X .LE. 10.0
C
 30   DO 40 I=1,N
        DGAMMA = (Y+I) * DGAMMA
 40   CONTINUE
      RETURN
C
C GAMMA(X) FOR ABS(X) .GT. 10.0.  RECALL Y = ABS(X).
C
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
C
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
C
      DGAMMA = EXP ((Y-0.5D0)*LOG(Y) - Y + SQ2PIL + D9LGMC(Y) )
      IF (X.GT.0.D0) RETURN
C
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
C
      SINPIY = SIN (PI*Y)
      IF (SINPIY .EQ. 0.D0) THEN
        WRITE(ICOUT,71)
        CALL DPWRST('XXX','BUG ')
        DGAMMA = 0.D0
        RETURN
      ENDIF
   71 FORMAT('***** ERROR FROM DGAMMA.  X IS A NEGATIVE INTEGER. ****')
C
      DGAMMA = -PI/(Y*SINPIY*DGAMMA)
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION DGAMM2 (X)
C***BEGIN PROLOGUE  DGAMMA
C***PURPOSE  Compute the complete Gamma function.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7A
C***TYPE      DOUBLE PRECISION (GAMMA-S, DGAMMA-D, CGAMMA-C)
C***KEYWORDS  COMPLETE GAMMA FUNCTION, FNLIB, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C DGAMMA(X) calculates the double precision complete Gamma function
C for double precision argument X.
C
C This same as DGAMMA, except error messages are suppressed.
C
C Series for GAM        on the interval  0.          to  1.00000E+00
C                                        with weighted error   5.79E-32
C                                         log weighted error  31.24
C                               significant figures required  30.00
C                                    decimal places required  32.05
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, D9LGMC, DCSEVL, DGAMLM, INITDS, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890911  Removed unnecessary intrinsics.  (WRB)
C   890911  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   920618  Removed space from variable name.  (RWC, WRB)
C***END PROLOGUE  DGAMMA
      DOUBLE PRECISION X, GAMCS(42), DXREL, PI, SINPIY, SQ2PIL, XMAX,
     1  XMIN, Y, D9LGMC, DCSEVL
      LOGICAL FIRST
C
      SAVE GAMCS, PI, SQ2PIL, NGAM, XMIN, XMAX, DXREL, FIRST
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
      DATA GAMCS(  1) / +.8571195590 9893314219 2006239994 2 D-2      /
      DATA GAMCS(  2) / +.4415381324 8410067571 9131577165 2 D-2      /
      DATA GAMCS(  3) / +.5685043681 5993633786 3266458878 9 D-1      /
      DATA GAMCS(  4) / -.4219835396 4185605010 1250018662 4 D-2      /
      DATA GAMCS(  5) / +.1326808181 2124602205 8400679635 2 D-2      /
      DATA GAMCS(  6) / -.1893024529 7988804325 2394702388 6 D-3      /
      DATA GAMCS(  7) / +.3606925327 4412452565 7808221722 5 D-4      /
      DATA GAMCS(  8) / -.6056761904 4608642184 8554829036 5 D-5      /
      DATA GAMCS(  9) / +.1055829546 3022833447 3182350909 3 D-5      /
      DATA GAMCS( 10) / -.1811967365 5423840482 9185589116 6 D-6      /
      DATA GAMCS( 11) / +.3117724964 7153222777 9025459316 9 D-7      /
      DATA GAMCS( 12) / -.5354219639 0196871408 7408102434 7 D-8      /
      DATA GAMCS( 13) / +.9193275519 8595889468 8778682594 0 D-9      /
      DATA GAMCS( 14) / -.1577941280 2883397617 6742327395 3 D-9      /
      DATA GAMCS( 15) / +.2707980622 9349545432 6654043308 9 D-10     /
      DATA GAMCS( 16) / -.4646818653 8257301440 8166105893 3 D-11     /
      DATA GAMCS( 17) / +.7973350192 0074196564 6076717535 9 D-12     /
      DATA GAMCS( 18) / -.1368078209 8309160257 9949917230 9 D-12     /
      DATA GAMCS( 19) / +.2347319486 5638006572 3347177168 8 D-13     /
      DATA GAMCS( 20) / -.4027432614 9490669327 6657053469 9 D-14     /
      DATA GAMCS( 21) / +.6910051747 3721009121 3833697525 7 D-15     /
      DATA GAMCS( 22) / -.1185584500 2219929070 5238712619 2 D-15     /
      DATA GAMCS( 23) / +.2034148542 4963739552 0102605193 2 D-16     /
      DATA GAMCS( 24) / -.3490054341 7174058492 7401294910 8 D-17     /
      DATA GAMCS( 25) / +.5987993856 4853055671 3505106602 6 D-18     /
      DATA GAMCS( 26) / -.1027378057 8722280744 9006977843 1 D-18     /
      DATA GAMCS( 27) / +.1762702816 0605298249 4275966074 8 D-19     /
      DATA GAMCS( 28) / -.3024320653 7353062609 5877211204 2 D-20     /
      DATA GAMCS( 29) / +.5188914660 2183978397 1783355050 6 D-21     /
      DATA GAMCS( 30) / -.8902770842 4565766924 4925160106 6 D-22     /
      DATA GAMCS( 31) / +.1527474068 4933426022 7459689130 6 D-22     /
      DATA GAMCS( 32) / -.2620731256 1873629002 5732833279 9 D-23     /
      DATA GAMCS( 33) / +.4496464047 8305386703 3104657066 6 D-24     /
      DATA GAMCS( 34) / -.7714712731 3368779117 0390152533 3 D-25     /
      DATA GAMCS( 35) / +.1323635453 1260440364 8657271466 6 D-25     /
      DATA GAMCS( 36) / -.2270999412 9429288167 0231381333 3 D-26     /
      DATA GAMCS( 37) / +.3896418998 0039914493 2081663999 9 D-27     /
      DATA GAMCS( 38) / -.6685198115 1259533277 9212799999 9 D-28     /
      DATA GAMCS( 39) / +.1146998663 1400243843 4761386666 6 D-28     /
      DATA GAMCS( 40) / -.1967938586 3451346772 9510399999 9 D-29     /
      DATA GAMCS( 41) / +.3376448816 5853380903 3489066666 6 D-30     /
      DATA GAMCS( 42) / -.5793070335 7821357846 2549333333 3 D-31     /
      DATA PI / 3.1415926535 8979323846 2643383279 50 D0 /
      DATA SQ2PIL / 0.9189385332 0467274178 0329736405 62 D0 /
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  DGAMMA
      IF (FIRST) THEN
         NGAM = INITDS (GAMCS, 42, 0.1*REAL(D1MACH(3)) )
C
         CALL DGAMLM (XMIN, XMAX)
         DXREL = SQRT(D1MACH(4))
      ENDIF
      FIRST = .FALSE.
C
      Y = ABS(X)
      IF (Y.GT.10.D0) GO TO 50
C
C COMPUTE GAMMA(X) FOR -XBND .LE. X .LE. XBND.  REDUCE INTERVAL AND FIND
C GAMMA(1+Y) FOR 0.0 .LE. Y .LT. 1.0 FIRST OF ALL.
C
      N = INT(X)
      IF (X.LT.0.D0) N = N - 1
      Y = X - REAL(N)
      N = N - 1
      DGAMM2 = 0.9375D0 + DCSEVL (2.D0*Y-1.D0, GAMCS, NGAM)
      IF (N.EQ.0) RETURN
C
      IF (N.GT.0) GO TO 30
C
C COMPUTE GAMMA(X) FOR X .LT. 1.0
C
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
C
      DO 20 I=1,N
        DGAMM2 = DGAMM2/(X+I-1 )
 20   CONTINUE
      RETURN
C
C GAMMA(X) FOR X .GE. 2.0 AND X .LE. 10.0
C
 30   DO 40 I=1,N
        DGAMM2 = (Y+I) * DGAMM2
 40   CONTINUE
      RETURN
C
C GAMMA(X) FOR ABS(X) .GT. 10.0.  RECALL Y = ABS(X).
C
 50   IF (X .GT. XMAX) THEN
        DGAMM2 = 0.D0
        RETURN
      ENDIF
C
      DGAMM2 = 0.D0
      IF (X .LT. XMIN) THEN
        CONTINUE
      ENDIF
      IF (X.LT.XMIN) RETURN
C
      DGAMM2 = EXP ((Y-0.5D0)*LOG(Y) - Y + SQ2PIL + D9LGMC(Y) )
      IF (X.GT.0.D0) RETURN
C
      IF (ABS((X-AINT(X-0.5D0))/X) .LT. DXREL) THEN
        CONTINUE
      ENDIF
C
      SINPIY = SIN (PI*Y)
      IF (SINPIY .EQ. 0.D0) THEN
        DGAMM2 = 0.D0
        RETURN
      ENDIF
C
      DGAMM2 = -PI/(Y*SINPIY*DGAMM2)
C
      RETURN
      END
      SUBROUTINE DGAMMF(DX,DGF)
C
C     THIS PROGRAM CALCULATES THE GAMMA FUNCTION
C     THE INPUT IS DOUBLE PRECISION DX
C     THE OUTPUT IS DOUBLE PRECISION DGF
C     ALL INTERNAL OPERATIONS ARE DONE IN DOUBLE PRECISION
C     THE ALGORITHM IS TO USE THE RECURSION FORMULA G(X)=G(X+1)/X
C     UNTIL X IS LARGE ENOUGH TO USE AN ASYMPTOTIC FORMULA FOR G(X)--THE CUT-OFF
C     POINT USED WAS X = 10
C     THE ASYMPTOTIC FORMULA USED IS IN AMS 55, PAGE 257, 6.1.41 (THE FIRST 9
C     TERMS OF THE SERIES WERE USED--I.E., OUT TO X**-17)
C     ALTHOUGH THE DATA STATEMENT DEFINES 10 COEFFICIENTS, THE PROGRAM MAKES USE
C     OF ONLY 9 COEFFICIENTS (THE ERROR BEING BOUNDED BY THE TENTH COEFFICIENT
C     DIVIDED BY X**19
C     SUBROUTINES NEEDED--NONE
C     PRINTING--NONE UNLESS AN ERROR CONDITION EXISTS
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--JUNE      1972.
C     UPDATED         --FEBRUARY  1981.
C     UPDATED         --FEBRUARY  1982.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DX
      DOUBLE PRECISION DGF
      DOUBLE PRECISION Y,Y2,Y3,Y4,Y5,DEN,A,B,C,D
C
      DIMENSION D(10)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----DATA STATEMENTS-------------------------------------------------
C
      DATA C/ .918938533204672741D0/
      DATA D(1),D(2),D(3),D(4),D(5)
     1                 /+.833333333333333333D-1,-.277777777777777778D-2,
     1+.793650793650793651D-3,-.595238095238095238D-3,+.8417508417508417
     151D-3/
      DATA D(6),D(7),D(8),D(9),D(10)
     1     /-.191752691752691753D-2,+.641025641025641025D-2,-.2955065359
     147712418D-1,+.179644372368830573D0,-.139243221690590111D1/
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(DX.LE.0.0D0)GOTO50
      GOTO90
   50 WRITE(ICOUT,5)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,45)DX
      CALL DPWRST('XXX','BUG ')
      GOTO9000
   90 CONTINUE
    5 FORMAT('***** FATAL ERROR--THE FIRST  INPUT ARGUMENT ',
     1'TO THE DGAMMF SUBROUTINE IS NON-POSITIVE *****')
   45 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',D22.15,' *****')
C
      Y=DX
      DEN=1.0D0
  100 IF(Y.GE.10.0D0)GOTO200
      DEN=DEN*Y
      Y=Y+1
      GOTO100
  200 Y2=Y*Y
      Y3=Y*Y2
      Y4=Y2*Y2
      Y5=Y2*Y3
      A=(Y-0.5D0)*DLOG(Y)-Y+C
      B=D(1)/Y+D(2)/Y3+D(3)/Y5+D(4)/(Y2*Y5)+D(5)/(Y4*Y5)+
     1D(6)/(Y*Y5*Y5)+D(7)/(Y3*Y5*Y5)+D(8)/(Y5*Y5*Y5)+D(9)/(Y2*Y5*Y5*Y5)
      DGF=DEXP(A+B)/DEN
C
 9000 CONTINUE
      RETURN
      END
      DOUBLE PRECISION FUNCTION DGAMR (X)
C***BEGIN PROLOGUE  DGAMR
C***PURPOSE  Compute the reciprocal of the Gamma function.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7A
C***TYPE      DOUBLE PRECISION (GAMR-S, DGAMR-D, CGAMR-C)
C***KEYWORDS  FNLIB, RECIPROCAL GAMMA FUNCTION, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C DGAMR(X) calculates the double precision reciprocal of the
C complete Gamma function for double precision argument X.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  DGAMMA, DLGAMS, XERCLR, XGETF, XSETF
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900727  Added EXTERNAL statement.  (WRB)
C***END PROLOGUE  DGAMR
      DOUBLE PRECISION X, ALNGX, SGNGX, DGAMMA
      EXTERNAL DGAMMA
C***FIRST EXECUTABLE STATEMENT  DGAMR
      DGAMR = 0.0D0
      IF (X.LE.0.0D0 .AND. AINT(X).EQ.X) RETURN
C
      IF (ABS(X).GT.10.0D0) GO TO 10
      DGAMR = 1.0D0/DGAMMA(X)
      RETURN
C
 10   CALL DLGAMS (X, ALNGX, SGNGX)
      DGAMR = SGNGX * EXP(-ALNGX)
      RETURN
C
      END
      SUBROUTINE DGACDF(X,GAMMA,CDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
C              FUNCTION VALUE FOR THE DOUBLE GAMMA
C              DISTRIBUTION WITH SINGLE PRECISION
C              TAIL LENGTH PARAMETER = GAMMA.
C              THE DOUBLE GAMMA DISTRIBUTION USED
C              HEREIN IS DEFINED FOR ALL REAL X,
C              AND HAS THE PROBABILITY DENSITY FUNCTION
C                 F(X) = (1/2)*ABS(X)**(GAMMA-1)*EXP(-ABS(X))/GAMMA(X)
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE PROBABILITY DENSITY
C                                FUNCTION IS TO BE EVALUATED.
C                     --GAMMA  = THE SHAPE PARAMETER
C                                GAMMA SHOULD BE POSITIVE.
C     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION PROBABILITY
C                                DENSITY FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
C             FUNCTION VALUE CDF FOR THE DOUBLE GAMMA DISTRIBUTION
C             WITH TAIL LENGHT PARAMETER = GAMMA.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--GAMMA SHOULD BE POSITIVE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--GAMCDF.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCE --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--1, 2ND. ED., 1994, PAGE 387
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1966)
C     VERSION NUMBER--96/1
C     ORIGINAL VERSION--JANUARY   1996.
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(GAMMA.LE.0)THEN
        WRITE(ICOUT,15)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)GAMMA
        CALL DPWRST('XXX','BUG ')
        CDF=0.0
        GOTO9999
      ENDIF
   15 FORMAT('***** FATAL ERROR--THE 2ND INPUT ARGUMENT TO THE ',
     1'DGACDF SUBROUTINE IS NON-POSITIVE *****')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',E15.8,' *****')
C
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
C
 9999 CONTINUE
      RETURN
      END
      SUBROUTINE DGAPDF(X,GAMMA,PDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
C              FUNCTION VALUE FOR THE DOUBLE GAMMA
C              DISTRIBUTION WITH SINGLE PRECISION
C              TAIL LENGTH PARAMETER = GAMMA.
C              THE DOUBLE GAMMA DISTRIBUTION USED
C              HEREIN IS DEFINED FOR ALL REAL X,
C              AND HAS THE PROBABILITY DENSITY FUNCTION
C                 F(X) = (1/2)*ABS(X)**(GAMMA-1)*EXP(-ABS(X))/GAMMA(X)
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE PROBABILITY DENSITY
C                                FUNCTION IS TO BE EVALUATED.
C                     --GAMMA  = THE SHAPE PARAMETER
C                                GAMMA SHOULD BE POSITIVE.
C     OUTPUT ARGUMENTS--PDF    = THE SINGLE PRECISION PROBABILITY
C                                DENSITY FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
C             FUNCTION VALUE PDF FOR THE DOUBLE GAMMA DISTRIBUTION
C             WITH TAIL LENGHT PARAMETER = GAMMA.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--GAMMA SHOULD BE POSITIVE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--EXP.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCE --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--1, 2ND. ED., 1994, PAGE 387
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1966)
C     VERSION NUMBER--96/1
C     ORIGINAL VERSION--JANUARY   1996.
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(GAMMA.LE.0)THEN
        WRITE(ICOUT,15)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)GAMMA
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GOTO9999
      ENDIF
   15 FORMAT('***** FATAL ERROR--THE 2ND INPUT ARGUMENT TO THE ',
     1'DGAPDF SUBROUTINE IS NON-POSITIVE *****')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',E15.8,' *****')
C
      ARG1=ABS(X)
      CALL GAMPDF(ARG1,GAMMA,PDF2)
      PDF=PDF2/2.0
C
 9999 CONTINUE
      RETURN
      END
      SUBROUTINE DGAPPF(P,GAMMA,PPF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
C              FUNCTION VALUE FOR THE DOUBLE GAMMA
C              DISTRIBUTION WITH SINGLE PRECISION
C              TAIL LENGTH PARAMETER = GAMMA.
C              THE DOUBLE GAMMA DISTRIBUTION USED
C              HEREIN IS DEFINED FOR ALL REAL X,
C              AND HAS THE PROBABILITY DENSITY FUNCTION
C                 F(X) = (1/2)*ABS(X)**(GAMMA-1)*EXP(-ABS(X))/GAMMA(X)
C     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE
C                                (BETWEEN 0.0 (INCLUSIVELY)
C                                AND 1.0 (EXCLUSIVELY))
C                                AT WHICH THE PERCENT POINT
C                                FUNCTION IS TO BE EVALUATED.
C                     --GAMMA  = THE SINGLE PRECISION VALUE
C                                OF THE TAIL LENGTH PARAMETER.
C                                GAMMA SHOULD BE POSITIVE.
C     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT
C                                POINT FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PERCENT POINT FUNCTION .
C             VALUE PPF FOR THE GAMMA DISTRIBUTION
C             WITH TAIL LENGTH PARAMETER VALUE = GAMMA.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--GAMMA SHOULD BE POSITIVE.
C                 --P SHOULD BE BETWEEN 0.0 (EXCLUSIVELY)
C                   AND 1.0 (EXCLUSIVELY).
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--LOG.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCE --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--1, 2ND. ED., 1994, PAGE 387
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1966)
C     VERSION NUMBER--96/1
C     ORIGINAL VERSION--JANUARY   1996.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(P.LE.0.0.OR.P.GE.1.0)THEN
        WRITE(ICOUT,1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
        CALL DPWRST('XXX','BUG ')
        PPF=0.0
        GOTO9999
      ENDIF
      IF(GAMMA.LE.0.0)THEN
        WRITE(ICOUT,15)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)GAMMA
        CALL DPWRST('XXX','BUG ')
        PPF=0.0
        GOTO9999
      ENDIF
    1 FORMAT('***** FATAL ERROR--THE 1ST INPUT ARGUMENT TO THE ',
     1'DGAPPF SUBROUTINE IS OUTSIDE THE ALLOWABLE (0,1) INTERVAL')
   15 FORMAT('***** FATAL ERROR--THE 2ND INPUT ARGUMENT TO THE ',
     1'DGAPPF SUBROUTINE IS NON-POSITIVE *****')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',E15.8,' *****')
C
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
C
 9999 CONTINUE
      RETURN
      END
      SUBROUTINE DGARAN(N,GAMMA,ISEED,X)
C
C     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
C              FROM THE DOUBLE GAMMA DISTRIBUTION
C              WITH TAIL LENGTH PARAMETER VALUE = GAMMA.
C     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
C                                OF RANDOM NUMBERS TO BE
C                                GENERATED.
C                     --GAMMA  = THE SINGLE PRECISION VALUE OF THE
C                                TAIL LENGTH PARAMETER.
C                                GAMMA SHOULD BE POSITIVE.
C     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
C                                (OF DIMENSION AT LEAST N)
C                                INTO WHICH THE GENERATED
C                                RANDOM SAMPLE WILL BE PLACED.
C     OUTPUT--A RANDOM SAMPLE OF SIZE N
C             FROM THE DOUBLE GAMMA DISTRIBUTION
C             WITH TAIL LENGTH PARAMETER VALUE = GAMMA.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C                 --GAMMA SHOULD BE POSITIVE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--XX
C               --JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--1, 2ND. ED., 1994.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1966)
C     VERSION NUMBER--2001.10
C     ORIGINAL VERSION--OCTOBER   2001.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(N.LT.1)THEN
        WRITE(ICOUT, 5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
      IF(GAMMA.LE.0.0)THEN
        WRITE(ICOUT,15)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)GAMMA
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
    5 FORMAT('***** FATAL ERROR--THE 1ST INPUT ARGUMENT TO THE ',
     1'DGARAN SUBROUTINE IS NON-POSITIVE *****')
   15 FORMAT('***** FATAL ERROR--THE 2ND INPUT ARGUMENT TO THE ',
     1'DGARAN SUBROUTINE IS NON-POSITIVE *****')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',E15.8,' *****')
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8,' *****')
C
C     GENERATE N UNIFORM (0,1) RANDOM NUMBERS;
C
      CALL UNIRAN(N,ISEED,X)
C
C     GENERATE N INVERTED WEIBULL DISTRIBUTION RANDOM NUMBERS
C     USING THE PERCENT POINT FUNCTION TRANSFORMATION METHOD.
C
      DO100I=1,N
        CALL DGAPPF(X(I),GAMMA,XTEMP)
        X(I)=XTEMP
  100 CONTINUE
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE DGCL1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR GREEK COMPLEX LOWER CASE (PART 1).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
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
C     DEFINE CHARACTER   2127--LOWER CASE ALPH
C
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
C
      DATA IXMIND(   1)/ -11/
      DATA IXMAXD(   1)/  12/
      DATA IXDELD(   1)/  23/
      DATA ISTARD(   1)/   1/
      DATA NUMCOO(   1)/  36/
C
C     DEFINE CHARACTER   2128--LOWER CASE BETA
C
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
C
      DATA IXMIND(   2)/ -11/
      DATA IXMAXD(   2)/  10/
      DATA IXDELD(   2)/  21/
      DATA ISTARD(   2)/  37/
      DATA NUMCOO(   2)/  51/
C
C     DEFINE CHARACTER   2129--LOWER CASE GAMM
C
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
C
      DATA IXMIND(   3)/ -10/
      DATA IXMAXD(   3)/  10/
      DATA IXDELD(   3)/  20/
      DATA ISTARD(   3)/  88/
      DATA NUMCOO(   3)/  24/
C
C     DEFINE CHARACTER   2130--LOWER CASE DELT
C
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
C
      DATA IXMIND(   4)/  -9/
      DATA IXMAXD(   4)/  10/
      DATA IXDELD(   4)/  19/
      DATA ISTARD(   4)/ 112/
      DATA NUMCOO(   4)/  41/
C
C     DEFINE CHARACTER   2131--LOWER CASE EPSI
C
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
C
      DATA IXMIND(   5)/  -9/
      DATA IXMAXD(   5)/   9/
      DATA IXDELD(   5)/  18/
      DATA ISTARD(   5)/ 153/
      DATA NUMCOO(   5)/  28/
C
C     DEFINE CHARACTER   2132--LOWER CASE ZETA
C
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
C
      DATA IXMIND(   6)/  -9/
      DATA IXMAXD(   6)/   9/
      DATA IXDELD(   6)/  18/
      DATA ISTARD(   6)/ 181/
      DATA NUMCOO(   6)/  29/
C
C     DEFINE CHARACTER   2133--LOWER CASE ETA
C
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
C
      DATA IXMIND(   7)/ -11/
      DATA IXMAXD(   7)/  11/
      DATA IXDELD(   7)/  22/
      DATA ISTARD(   7)/ 210/
      DATA NUMCOO(   7)/  28/
C
C     DEFINE CHARACTER   2134--LOWER CASE THET
C
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
C
      DATA IXMIND(   8)/ -12/
      DATA IXMAXD(   8)/  11/
      DATA IXDELD(   8)/  23/
      DATA ISTARD(   8)/ 238/
      DATA NUMCOO(   8)/  41/
C
C     DEFINE CHARACTER   2135--LOWER CASE IOTA
C
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
C
      DATA IXMIND(   9)/  -6/
      DATA IXMAXD(   9)/   6/
      DATA IXDELD(   9)/  12/
      DATA ISTARD(   9)/ 279/
      DATA NUMCOO(   9)/  13/
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
   51 FORMAT('***** AT THE BEGINNING OF DGCL1--')
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
      SUBROUTINE DGCL2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR GREEK COMPLEX LOWER CASE (PART 2).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
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
C     DEFINE CHARACTER   2136--LOWER CASE KAPP
C
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
C
      DATA IXMIND(  10)/ -10/
      DATA IXMAXD(  10)/  10/
      DATA IXDELD(  10)/  20/
      DATA ISTARD(  10)/   1/
      DATA NUMCOO(  10)/  24/
C
C     DEFINE CHARACTER   2137--LOWER CASE LAMB
C
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
C
      DATA IXMIND(  11)/ -10/
      DATA IXMAXD(  11)/  10/
      DATA IXDELD(  11)/  20/
      DATA ISTARD(  11)/  25/
      DATA NUMCOO(  11)/  19/
C
C     DEFINE CHARACTER   2138--LOWER CASE MU
C
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
C
      DATA IXMIND(  12)/ -12/
      DATA IXMAXD(  12)/  11/
      DATA IXDELD(  12)/  23/
      DATA ISTARD(  12)/  44/
      DATA NUMCOO(  12)/  23/
C
C     DEFINE CHARACTER   2139--LOWER CASE NU
C
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
C
      DATA IXMIND(  13)/ -10/
      DATA IXMAXD(  13)/  10/
      DATA IXDELD(  13)/  20/
      DATA ISTARD(  13)/  67/
      DATA NUMCOO(  13)/  19/
C
C     DEFINE CHARACTER   2140--LOWER CASE XI
C
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
C
      DATA IXMIND(  14)/  -9/
      DATA IXMAXD(  14)/   8/
      DATA IXDELD(  14)/  17/
      DATA ISTARD(  14)/  86/
      DATA NUMCOO(  14)/  40/
C
C     DEFINE CHARACTER   2141--LOWER CASE OMIC
C
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
C
      DATA IXMIND(  15)/  -9/
      DATA IXMAXD(  15)/   9/
      DATA IXDELD(  15)/  18/
      DATA ISTARD(  15)/ 126/
      DATA NUMCOO(  15)/  29/
C
C     DEFINE CHARACTER   2142--LOWER CASE PI
C
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
C
      DATA IXMIND(  16)/ -11/
      DATA IXMAXD(  16)/  11/
      DATA IXDELD(  16)/  22/
      DATA ISTARD(  16)/ 155/
      DATA NUMCOO(  16)/  16/
C
C     DEFINE CHARACTER   2143--LOWER CASE RHO
C
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
C
      DATA IXMIND(  17)/ -10/
      DATA IXMAXD(  17)/   9/
      DATA IXDELD(  17)/  19/
      DATA ISTARD(  17)/ 171/
      DATA NUMCOO(  17)/  28/
C
C     DEFINE CHARACTER   2144--LOWER CASE SIGM
C
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
C
      DATA IXMIND(  18)/ -10/
      DATA IXMAXD(  18)/  11/
      DATA IXDELD(  18)/  21/
      DATA ISTARD(  18)/ 199/
      DATA NUMCOO(  18)/  31/
C
C     DEFINE CHARACTER   2145--LOWER CASE TAU
C
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
C
      DATA IXMIND(  19)/ -10/
      DATA IXMAXD(  19)/  10/
      DATA IXDELD(  19)/  20/
      DATA ISTARD(  19)/ 230/
      DATA NUMCOO(  19)/  12/
C
C     DEFINE CHARACTER   2146--LOWER CASE UPSI
C
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
C
      DATA IXMIND(  20)/ -10/
      DATA IXMAXD(  20)/  10/
      DATA IXDELD(  20)/  20/
      DATA ISTARD(  20)/ 242/
      DATA NUMCOO(  20)/  28/
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
   51 FORMAT('***** AT THE BEGINNING OF DGCL2--')
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
      SUBROUTINE DGCL3(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR GREEK COMPLEX LOWER CASE (PART 3).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
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
C     DEFINE CHARACTER   2147--LOWER CASE PHI
C
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
C
      DATA IXMIND(  21)/ -11/
      DATA IXMAXD(  21)/  11/
      DATA IXDELD(  21)/  22/
      DATA ISTARD(  21)/   1/
      DATA NUMCOO(  21)/  34/
C
C     DEFINE CHARACTER   2148--LOWER CASE CHI
C
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
C
      DATA IXMIND(  22)/  -9/
      DATA IXMAXD(  22)/   9/
      DATA IXDELD(  22)/  18/
      DATA ISTARD(  22)/  35/
      DATA NUMCOO(  22)/  20/
C
C     DEFINE CHARACTER   2149--LOWER CASE PSI
C
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
C
      DATA IXMIND(  23)/ -12/
      DATA IXMAXD(  23)/  11/
      DATA IXDELD(  23)/  23/
      DATA ISTARD(  23)/  55/
      DATA NUMCOO(  23)/  30/
C
C     DEFINE CHARACTER   2150--LOWER CASE OMEG
C
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
C
      DATA IXMIND(  24)/ -12/
      DATA IXMAXD(  24)/  11/
      DATA IXDELD(  24)/  23/
      DATA ISTARD(  24)/  85/
      DATA NUMCOO(  24)/  40/
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
   51 FORMAT('***** AT THE BEGINNING OF DGCL3--')
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
      SUBROUTINE DGCU1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR GREEK COMPLEX UPPER CASE (PART 1).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
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
C     DEFINE CHARACTER   2027--UPPER CASE ALPH
C
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
C
      DATA IXMIND(   1)/ -10/
      DATA IXMAXD(   1)/  10/
      DATA IXDELD(   1)/  20/
      DATA ISTARD(   1)/   1/
      DATA NUMCOO(   1)/  12/
C
C     DEFINE CHARACTER   2028--UPPER CASE BETA
C
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
C
      DATA IXMIND(   2)/ -11/
      DATA IXMAXD(   2)/  11/
      DATA IXDELD(   2)/  22/
      DATA ISTARD(   2)/  13/
      DATA NUMCOO(   2)/  39/
C
C     DEFINE CHARACTER   2029--UPPER CASE GAMM
C
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
C
      DATA IXMIND(   3)/  -9/
      DATA IXMAXD(   3)/   9/
      DATA IXDELD(   3)/  18/
      DATA ISTARD(   3)/  52/
      DATA NUMCOO(   3)/  10/
C
C     DEFINE CHARACTER   2030--UPPER CASE DELT
C
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
C
      DATA IXMIND(   4)/ -10/
      DATA IXMAXD(   4)/  10/
      DATA IXDELD(   4)/  20/
      DATA ISTARD(   4)/  62/
      DATA NUMCOO(   4)/  10/
C
C     DEFINE CHARACTER   2031--UPPER CASE EPSI
C
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
C
      DATA IXMIND(   5)/ -11/
      DATA IXMAXD(   5)/  10/
      DATA IXDELD(   5)/  21/
      DATA ISTARD(   5)/  72/
      DATA NUMCOO(   5)/  16/
C
C     DEFINE CHARACTER   2032--UPPER CASE ZETA
C
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
C
      DATA IXMIND(   6)/ -10/
      DATA IXMAXD(   6)/  10/
      DATA IXDELD(   6)/  20/
      DATA ISTARD(   6)/  88/
      DATA NUMCOO(   6)/  12/
C
C     DEFINE CHARACTER   2033--UPPER CASE ETA
C
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
C
      DATA IXMIND(   7)/ -12/
      DATA IXMAXD(   7)/  12/
      DATA IXDELD(   7)/  24/
      DATA ISTARD(   7)/ 100/
      DATA NUMCOO(   7)/  18/
C
C     DEFINE CHARACTER   2034--UPPER CASE THET
C
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
C
      DATA IXMIND(   8)/ -11/
      DATA IXMAXD(   8)/  11/
      DATA IXDELD(   8)/  22/
      DATA ISTARD(   8)/ 118/
      DATA NUMCOO(   8)/  49/
C
C     DEFINE CHARACTER   2035--UPPER CASE IOTA
C
      DATA IOPERA( 167),IX( 167),IY( 167)/'MOVE',   0,  12/
      DATA IOPERA( 168),IX( 168),IY( 168)/'DRAW',   0,  -9/
      DATA IOPERA( 169),IX( 169),IY( 169)/'MOVE',   1,  12/
      DATA IOPERA( 170),IX( 170),IY( 170)/'DRAW',   1,  -9/
      DATA IOPERA( 171),IX( 171),IY( 171)/'MOVE',  -3,  12/
      DATA IOPERA( 172),IX( 172),IY( 172)/'DRAW',   4,  12/
      DATA IOPERA( 173),IX( 173),IY( 173)/'MOVE',  -3,  -9/
      DATA IOPERA( 174),IX( 174),IY( 174)/'DRAW',   4,  -9/
C
      DATA IXMIND(   9)/  -5/
      DATA IXMAXD(   9)/   6/
      DATA IXDELD(   9)/  11/
      DATA ISTARD(   9)/ 167/
      DATA NUMCOO(   9)/   8/
C
C     DEFINE CHARACTER   2036--UPPER CASE KAPP
C
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
C
      DATA IXMIND(  10)/ -12/
      DATA IXMAXD(  10)/  10/
      DATA IXDELD(  10)/  22/
      DATA ISTARD(  10)/ 175/
      DATA NUMCOO(  10)/  18/
C
C     DEFINE CHARACTER   2037--UPPER CASE LAMB
C
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
C
      DATA IXMIND(  11)/ -10/
      DATA IXMAXD(  11)/  10/
      DATA IXDELD(  11)/  20/
      DATA ISTARD(  11)/ 193/
      DATA NUMCOO(  11)/  10/
C
C     DEFINE CHARACTER   2038--UPPER CASE MU
C
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
C
      DATA IXMIND(  12)/ -12/
      DATA IXMAXD(  12)/  13/
      DATA IXDELD(  12)/  25/
      DATA ISTARD(  12)/ 203/
      DATA NUMCOO(  12)/  20/
C
C     DEFINE CHARACTER   2039--UPPER CASE NU
C
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
C
      DATA IXMIND(  13)/ -11/
      DATA IXMAXD(  13)/  12/
      DATA IXDELD(  13)/  23/
      DATA ISTARD(  13)/ 223/
      DATA NUMCOO(  13)/  14/
C
C     DEFINE CHARACTER   2040--UPPER CASE XI
C
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
C
      DATA IXMIND(  14)/ -11/
      DATA IXMAXD(  14)/  11/
      DATA IXDELD(  14)/  22/
      DATA ISTARD(  14)/ 237/
      DATA NUMCOO(  14)/  24/
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
   51 FORMAT('***** AT THE BEGINNING OF DGCU1--')
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
      SUBROUTINE DGCU2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR GREEK COMPLEX UPPER CASE (PART 2).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
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
C     DEFINE CHARACTER   2041--UPPER CASE OMIC
C
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
C
      DATA IXMIND(  15)/ -11/
      DATA IXMAXD(  15)/  11/
      DATA IXDELD(  15)/  22/
      DATA ISTARD(  15)/   1/
      DATA NUMCOO(  15)/  41/
C
C     DEFINE CHARACTER   2042--UPPER CASE PI
C
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
C
      DATA IXMIND(  16)/ -12/
      DATA IXMAXD(  16)/  12/
      DATA IXDELD(  16)/  24/
      DATA ISTARD(  16)/  42/
      DATA NUMCOO(  16)/  14/
C
C     DEFINE CHARACTER   2043--UPPER CASE RHO
C
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
C
      DATA IXMIND(  17)/ -11/
      DATA IXMAXD(  17)/  11/
      DATA IXDELD(  17)/  22/
      DATA ISTARD(  17)/  56/
      DATA NUMCOO(  17)/  24/
C
C     DEFINE CHARACTER   2044--UPPER CASE SIGM
C
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
C
      DATA IXMIND(  18)/ -10/
      DATA IXMAXD(  18)/  11/
      DATA IXDELD(  18)/  21/
      DATA ISTARD(  18)/  80/
      DATA NUMCOO(  18)/  15/
C
C     DEFINE CHARACTER   2045--UPPER CASE TAU
C
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
C
      DATA IXMIND(  19)/  -9/
      DATA IXMAXD(  19)/  10/
      DATA IXDELD(  19)/  19/
      DATA ISTARD(  19)/  95/
      DATA NUMCOO(  19)/  12/
C
C     DEFINE CHARACTER   2046--UPPER CASE UPSI
C
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
C
      DATA IXMIND(  20)/  -9/
      DATA IXMAXD(  20)/  10/
      DATA IXDELD(  20)/  19/
      DATA ISTARD(  20)/ 107/
      DATA NUMCOO(  20)/  28/
C
C     DEFINE CHARACTER   2047--UPPER CASE PHI
C
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
C
      DATA IXMIND(  21)/ -10/
      DATA IXMAXD(  21)/  11/
      DATA IXDELD(  21)/  21/
      DATA ISTARD(  21)/ 135/
      DATA NUMCOO(  21)/  41/
C
C     DEFINE CHARACTER   2048--UPPER CASE CHI
C
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
C
      DATA IXMIND(  22)/ -10/
      DATA IXMAXD(  22)/  10/
      DATA IXDELD(  22)/  20/
      DATA ISTARD(  22)/ 176/
      DATA NUMCOO(  22)/  14/
C
C     DEFINE CHARACTER   2049--UPPER CASE PSI
C
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
C
      DATA IXMIND(  23)/ -11/
      DATA IXMAXD(  23)/  12/
      DATA IXDELD(  23)/  23/
      DATA ISTARD(  23)/ 190/
      DATA NUMCOO(  23)/  34/
C
C     DEFINE CHARACTER   2050--UPPER CASE OMEG
C
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
C
      DATA IXMIND(  24)/ -11/
      DATA IXMAXD(  24)/  11/
      DATA IXDELD(  24)/  22/
      DATA ISTARD(  24)/ 224/
      DATA NUMCOO(  24)/  38/
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
   51 FORMAT('***** AT THE BEGINNING OF DGCU2--')
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
      SUBROUTINE DGECO(A,LDA,N,IPVT,RCOND,Z)
C***BEGIN PROLOGUE  DGECO
C***DATE WRITTEN   780814   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  D2A1
C***KEYWORDS  CONDITION,DOUBLE PRECISION,FACTOR,LINEAR ALGEBRA,LINPACK,
C             MATRIX
C***AUTHOR  MOLER, C. B., (U. OF NEW MEXICO)
C***PURPOSE  Factors a double precision matrix by Gaussian elimination
C            and estimates the condition of the matrix.
C***DESCRIPTION
C
C     DGECO factors a double precision matrix by Gaussian elimination
C     and estimates the condition of the matrix.
C
C     If  RCOND  is not needed, DGEFA is slightly faster.
C     To solve  A*X = B , follow DGECO by DGESL.
C     To compute  INVERSE(A)*C , follow DGECO by DGESL.
C     To compute  DETERMINANT(A) , follow DGECO by DGEDI.
C     To compute  INVERSE(A) , follow DGECO by DGEDI.
C
C     On Entry
C
C        A       DOUBLE PRECISION(LDA, N)
C                the matrix to be factored.
C
C        LDA     INTEGER
C                the leading dimension of the array  A .
C
C        N       INTEGER
C                the order of the matrix  A .
C
C     On Return
C
C        A       an upper triangular matrix and the multipliers
C                which were used to obtain it.
C                The factorization can be written  A = L*U  where
C                L  is a product of permutation and unit lower
C                triangular matrices and  U  is upper triangular.
C
C        IPVT    INTEGER(N)
C                an INTEGER vector of pivot indices.
C
C        RCOND   DOUBLE PRECISION
C                an estimate of the reciprocal condition of  A .
C                For the system  A*X = B , relative perturbations
C                in  A  and  B  of size  EPSILON  may cause
C                relative perturbations in  X  of size  EPSILON/RCOND .
C                If  RCOND  is so small that the logical expression
C                           1.0 + RCOND .EQ. 1.0
C                is true, then  A  may be singular to working
C                precision.  In particular,  RCOND  is zero  if
C                exact singularity is detected or the estimate
C                underflows.
C
C        Z       DOUBLE PRECISION(N)
C                a work vector whose contents are usually unimportant.
C                If  A  is close to a singular matrix, then  Z  is
C                an approximate null vector in the sense that
C                NORM(A*Z) = RCOND*NORM(A)*NORM(Z) .
C
C     LINPACK.  This version dated 08/14/78 .
C     Cleve Moler, University of New Mexico, Argonne National Lab.
C
C     Subroutines and Functions
C
C     LINPACK DGEFA
C     BLAS DAXPY,DDOT,DSCAL,DASUM
C     Fortran DABS,DMAX1,DSIGN
C***REFERENCES  DONGARRA J.J., BUNCH J.R., MOLER C.B., STEWART G.W.,
C                 *LINPACK USERS  GUIDE*, SIAM, 1979.
C***ROUTINES CALLED  DASUM,DAXPY,DDOT,DGEFA,DSCAL
C***END PROLOGUE  DGECO
      INTEGER LDA,N,IPVT(1)
      DOUBLE PRECISION A(LDA,1),Z(1)
      DOUBLE PRECISION RCOND
C
      DOUBLE PRECISION DDOT,EK,T,WK,WKM
      DOUBLE PRECISION ANORM,S,DASUM,SM,YNORM
      INTEGER INFO,J,K,KB,KP1,L
C
C     COMPUTE 1-NORM OF A
C
C***FIRST EXECUTABLE STATEMENT  DGECO
      ANORM = 0.0D0
      DO 10 J = 1, N
         ANORM = DMAX1(ANORM,DASUM(N,A(1,J),1))
   10 CONTINUE
C
C     FACTOR
C
      CALL DGEFA(A,LDA,N,IPVT,INFO)
C
C     RCOND = 1/(NORM(A)*(ESTIMATE OF NORM(INVERSE(A)))) .
C     ESTIMATE = NORM(Z)/NORM(Y) WHERE  A*Z = Y  AND  TRANS(A)*Y = E .
C     TRANS(A)  IS THE TRANSPOSE OF A .  THE COMPONENTS OF  E  ARE
C     CHOSEN TO CAUSE MAXIMUM LOCAL GROWTH IN THE ELEMENTS OF W  WHERE
C     TRANS(U)*W = E .  THE VECTORS ARE FREQUENTLY RESCALED TO AVOID
C     OVERFLOW.
C
C     SOLVE TRANS(U)*W = E
C
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
C
C     SOLVE TRANS(L)*Y = W
C
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
C
      YNORM = 1.0D0
C
C     SOLVE L*V = Y
C
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
C
C     SOLVE  U*Z = V
C
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
C     MAKE ZNORM = 1.0
      S = 1.0D0/DASUM(N,Z,1)
      CALL DSCAL(N,S,Z,1)
      YNORM = S*YNORM
C
      IF (ANORM .NE. 0.0D0) RCOND = YNORM/ANORM
      IF (ANORM .EQ. 0.0D0) RCOND = 0.0D0
      RETURN
      END
      SUBROUTINE DGEFA(A,LDA,N,IPVT,INFO)
C***BEGIN PROLOGUE  DGEFA
C***DATE WRITTEN   780814   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  D2A1
C***KEYWORDS  DOUBLE PRECISION,FACTOR,LINEAR ALGEBRA,LINPACK,MATRIX
C***AUTHOR  MOLER, C. B., (U. OF NEW MEXICO)
C***PURPOSE  Factors a double precision matrix by Gaussian elimination.
C***DESCRIPTION
C
C     DGEFA factors a double precision matrix by Gaussian elimination.
C
C     DGEFA is usually called by DGECO, but it can be called
C     directly with a saving in time if  RCOND  is not needed.
C     (Time for DGECO) = (1 + 9/N)*(Time for DGEFA) .
C
C     On Entry
C
C        A       DOUBLE PRECISION(LDA, N)
C                the matrix to be factored.
C
C        LDA     INTEGER
C                the leading dimension of the array  A .
C
C        N       INTEGER
C                the order of the matrix  A .
C
C     On Return
C
C        A       an upper triangular matrix and the multipliers
C                which were used to obtain it.
C                The factorization can be written  A = L*U  where
C                L  is a product of permutation and unit lower
C                triangular matrices and  U  is upper triangular.
C
C        IPVT    INTEGER(N)
C                an integer vector of pivot indices.
C
C        INFO    INTEGER
C                = 0  normal value.
C                = K  if  U(K,K) .EQ. 0.0 .  This is not an error
C                     condition for this subroutine, but it does
C                     indicate that DGESL or DGEDI will divide by zero
C                     if called.  Use  RCOND  in DGECO for a reliable
C                     indication of singularity.
C
C     LINPACK.  This version dated 08/14/78 .
C     Cleve Moler, University of New Mexico, Argonne National Lab.
C
C     Subroutines and Functions
C
C     BLAS DAXPY,DSCAL,IDAMAX
C***REFERENCES  DONGARRA J.J., BUNCH J.R., MOLER C.B., STEWART G.W.,
C                 *LINPACK USERS  GUIDE*, SIAM, 1979.
C***ROUTINES CALLED  DAXPY,DSCAL,IDAMAX
C***END PROLOGUE  DGEFA
      INTEGER LDA,N,IPVT(1),INFO
      DOUBLE PRECISION A(LDA,1)
C
      DOUBLE PRECISION T
      INTEGER IDAMAX,J,K,KP1,L,NM1
C
C     GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING
C
C***FIRST EXECUTABLE STATEMENT  DGEFA
      INFO = 0
      NM1 = N - 1
      IF (NM1 .LT. 1) GO TO 70
      DO 60 K = 1, NM1
         KP1 = K + 1
C
C        FIND L = PIVOT INDEX
C
         L = IDAMAX(N-K+1,A(K,K),1) + K - 1
         IPVT(K) = L
C
C        ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED
C
         IF (A(L,K) .EQ. 0.0D0) GO TO 40
C
C           INTERCHANGE IF NECESSARY
C
            IF (L .EQ. K) GO TO 10
               T = A(L,K)
               A(L,K) = A(K,K)
               A(K,K) = T
   10       CONTINUE
C
C           COMPUTE MULTIPLIERS
C
            T = -1.0D0/A(K,K)
            CALL DSCAL(N-K,T,A(K+1,K),1)
C
C           ROW ELIMINATION WITH COLUMN INDEXING
C
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
      END
      SUBROUTINE DGESL(A,LDA,N,IPVT,B,JOB)
C***BEGIN PROLOGUE  DGESL
C***DATE WRITTEN   780814   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***CATEGORY NO.  D2A1
C***KEYWORDS  DOUBLE PRECISION,LINEAR ALGEBRA,LINPACK,MATRIX,SOLVE
C***AUTHOR  MOLER, C. B., (U. OF NEW MEXICO)
C***PURPOSE  Solves the double precision system  A*X=B or  TRANS(A)*X=B
C            using the factors computed by DGECO or DGEFA.
C***DESCRIPTION
C
C     DGESL solves the double precision system
C     A * X = B  or  TRANS(A) * X = B
C     using the factors computed by DGECO or DGEFA.
C
C     On Entry
C
C        A       DOUBLE PRECISION(LDA, N)
C                the output from DGECO or DGEFA.
C
C        LDA     INTEGER
C                the leading dimension of the array  A .
C
C        N       INTEGER
C                the order of the matrix  A .
C
C        IPVT    INTEGER(N)
C                the pivot vector from DGECO or DGEFA.
C
C        B       DOUBLE PRECISION(N)
C                the right hand side vector.
C
C        JOB     INTEGER
C                = 0         to solve  A*X = B ,
C                = nonzero   to solve  TRANS(A)*X = B  where
C                            TRANS(A)  is the transpose.
C
C     On Return
C
C        B       the solution vector  X .
C
C     Error Condition
C
C        A division by zero will occur if the input factor contains a
C        zero on the diagonal.  Technically this indicates singularity
C        but it is often caused by improper arguments or improper
C        setting of LDA .  It will not occur if the subroutines are
C        called correctly and if DGECO has set RCOND .GT. 0.0
C        or DGEFA has set INFO .EQ. 0 .
C
C     To compute  INVERSE(A) * C  where  C  is a matrix
C     with  P  columns
C           CALL DGECO(A,LDA,N,IPVT,RCOND,Z)
C           IF (RCOND is too small) GO TO ...
C           DO 10 J = 1, P
C              CALL DGESL(A,LDA,N,IPVT,C(1,J),0)
C        10 CONTINUE
C
C     LINPACK.  This version dated 08/14/78 .
C     Cleve Moler, University of New Mexico, Argonne National Lab.
C
C     Subroutines and Functions
C
C     BLAS DAXPY,DDOT
C***REFERENCES  DONGARRA J.J., BUNCH J.R., MOLER C.B., STEWART G.W.,
C                 *LINPACK USERS  GUIDE*, SIAM, 1979.
C***ROUTINES CALLED  DAXPY,DDOT
C***END PROLOGUE  DGESL
      INTEGER LDA,N,IPVT(1),JOB
      DOUBLE PRECISION A(LDA,1),B(1)
C
      DOUBLE PRECISION DDOT,T
      INTEGER K,KB,L,NM1
C***FIRST EXECUTABLE STATEMENT  DGESL
      NM1 = N - 1
      IF (JOB .NE. 0) GO TO 50
C
C        JOB = 0 , SOLVE  A * X = B
C        FIRST SOLVE  L*Y = B
C
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
C
C        NOW SOLVE  U*X = Y
C
         DO 40 KB = 1, N
            K = N + 1 - KB
            B(K) = B(K)/A(K,K)
            T = -B(K)
            CALL DAXPY(K-1,T,A(1,K),1,B(1),1)
   40    CONTINUE
      GO TO 100
   50 CONTINUE
C
C        JOB = NONZERO, SOLVE  TRANS(A) * X = B
C        FIRST SOLVE  TRANS(U)*Y = B
C
         DO 60 K = 1, N
            T = DDOT(K-1,A(1,K),1,B(1),1)
            B(K) = (B(K) - T)/A(K,K)
   60    CONTINUE
C
C        NOW SOLVE TRANS(L)*X = Y
C
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
      END
      SUBROUTINE DGEDI(A,LDA,N,IPVT,DET,WORK,JOB)
C***BEGIN PROLOGUE  DGEDI
C***DATE WRITTEN   780814   (YYMMDD)
C***REVISION DATE  820801   (YYMMDD)
C***REVISION HISTORY  (YYMMDD)
C   000330  Modified array declarations.  (JEC)
C***CATEGORY NO.  D3A1,D2A1
C***KEYWORDS  DETERMINANT,DOUBLE PRECISION,FACTOR,INVERSE,
C             LINEAR ALGEBRA,LINPACK,MATRIX
C***AUTHOR  MOLER, C. B., (U. OF NEW MEXICO)
C***PURPOSE  Computes the determinant and inverse of a matrix using
C            factors computed by DGECO or DGEFA.
C***DESCRIPTION
C
C     DGEDI computes the determinant and inverse of a matrix
C     using the factors computed by DGECO or DGEFA.
C
C     On Entry
C
C        A       DOUBLE PRECISION(LDA, N)
C                the output from DGECO or DGEFA.
C
C        LDA     INTEGER
C                the leading dimension of the array  A .
C
C        N       INTEGER
C                the order of the matrix  A .
C
C        IPVT    INTEGER(N)
C                the pivot vector from DGECO or DGEFA.
C
C        WORK    DOUBLE PRECISION(N)
C                work vector.  Contents destroyed.
C
C        JOB     INTEGER
C                = 11   both determinant and inverse.
C                = 01   inverse only.
C                = 10   determinant only.
C
C     On Return
C
C        A       inverse of original matrix if requested.
C                Otherwise unchanged.
C
C        DET     DOUBLE PRECISION(2)
C                determinant of original matrix if requested.
C                Otherwise not referenced.
C                Determinant = DET(1) * 10.0**DET(2)
C                with  1.0 .LE. DABS(DET(1)) .LT. 10.0
C                or  DET(1) .EQ. 0.0 .
C
C     Error Condition
C
C        A division by zero will occur if the input factor contains
C        a zero on the diagonal and the inverse is requested.
C        It will not occur if the subroutines are called correctly
C        and if DGECO has set RCOND .GT. 0.0 or DGEFA has set
C        INFO .EQ. 0 .
C
C     LINPACK.  This version dated 08/14/78 .
C     Cleve Moler, University of New Mexico, Argonne National Lab.
C
C     Subroutines and Functions
C
C     BLAS DAXPY,DSCAL,DSWAP
C     Fortran DABS,MOD
C***REFERENCES  DONGARRA J.J., BUNCH J.R., MOLER C.B., STEWART G.W.,
C                 *LINPACK USERS  GUIDE*, SIAM, 1979.
C***ROUTINES CALLED  DAXPY,DSCAL,DSWAP
C***END PROLOGUE  DGEDI
      INTEGER LDA,N,IPVT(*),JOB
      DOUBLE PRECISION A(LDA,*),DET(2),WORK(*)
C
      DOUBLE PRECISION T
      DOUBLE PRECISION TEN
      INTEGER I,J,K,KB,KP1,L,NM1
C
C     COMPUTE DETERMINANT
C
C***FIRST EXECUTABLE STATEMENT  DGEDI
      IF (JOB/10 .EQ. 0) GO TO 70
         DET(1) = 1.0D0
         DET(2) = 0.0D0
         TEN = 10.0D0
         DO 50 I = 1, N
            IF (IPVT(I) .NE. I) DET(1) = -DET(1)
            DET(1) = A(I,I)*DET(1)
C        ...EXIT
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
C
C     COMPUTE INVERSE(U)
C
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
C
C        FORM INVERSE(U)*INVERSE(L)
C
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
      END
      SUBROUTINE DGEMM ( TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, LDB,
     $                   BETA, C, LDC ,
     $                   IERROR)
*     .. Scalar Arguments ..
      CHARACTER*1        TRANSA, TRANSB
      CHARACTER*4        IERROR
      INTEGER            M, N, K, LDA, LDB, LDC
      DOUBLE PRECISION   ALPHA, BETA
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), C( LDC, * )
*     ..
C
      INCLUDE 'DPCOP2.INC'
C
*
*  Purpose
*  =======
*
*  DGEMM  performs one of the matrix-matrix operations
*
*     C := alpha*op( A )*op( B ) + beta*C,
*
*  where  op( X ) is one of
*
*     op( X ) = X   or   op( X ) = X',
*
*  alpha and beta are scalars, and A, B and C are matrices, with op( A )
*  an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
*
*  Parameters
*  ==========
*
*  TRANSA - CHARACTER*1.
*           On entry, TRANSA specifies the form of op( A ) to be used in
*           the matrix multiplication as follows:
*
*              TRANSA = 'N' or 'n',  op( A ) = A.
*
*              TRANSA = 'T' or 't',  op( A ) = A'.
*
*              TRANSA = 'C' or 'c',  op( A ) = A'.
*
*           Unchanged on exit.
*
*  TRANSB - CHARACTER*1.
*           On entry, TRANSB specifies the form of op( B ) to be used in
*           the matrix multiplication as follows:
*
*              TRANSB = 'N' or 'n',  op( B ) = B.
*
*              TRANSB = 'T' or 't',  op( B ) = B'.
*
*              TRANSB = 'C' or 'c',  op( B ) = B'.
*
*           Unchanged on exit.
*
*  M      - INTEGER.
*           On entry,  M  specifies  the number  of rows  of the  matrix
*           op( A )  and of the  matrix  C.  M  must  be at least  zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry,  N  specifies the number  of columns of the matrix
*           op( B ) and the number of columns of the matrix C. N must be
*           at least zero.
*           Unchanged on exit.
*
*  K      - INTEGER.
*           On entry,  K  specifies  the number of columns of the matrix
*           op( A ) and the number of rows of the matrix op( B ). K must
*           be at least  zero.
*           Unchanged on exit.
*
*  ALPHA  - DOUBLE PRECISION.
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  A      - DOUBLE PRECISION array of DIMENSION ( LDA, ka ), where ka is
*           k  when  TRANSA = 'N' or 'n',  and is  m  otherwise.
*           Before entry with  TRANSA = 'N' or 'n',  the leading  m by k
*           part of the array  A  must contain the matrix  A,  otherwise
*           the leading  k by m  part of the array  A  must contain  the
*           matrix A.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. When  TRANSA = 'N' or 'n' then
*           LDA must be at least  max( 1, m ), otherwise  LDA must be at
*           least  max( 1, k ).
*           Unchanged on exit.
*
*  B      - DOUBLE PRECISION array of DIMENSION ( LDB, kb ), where kb is
*           n  when  TRANSB = 'N' or 'n',  and is  k  otherwise.
*           Before entry with  TRANSB = 'N' or 'n',  the leading  k by n
*           part of the array  B  must contain the matrix  B,  otherwise
*           the leading  n by k  part of the array  B  must contain  the
*           matrix B.
*           Unchanged on exit.
*
*  LDB    - INTEGER.
*           On entry, LDB specifies the first dimension of B as declared
*           in the calling (sub) program. When  TRANSB = 'N' or 'n' then
*           LDB must be at least  max( 1, k ), otherwise  LDB must be at
*           least  max( 1, n ).
*           Unchanged on exit.
*
*  BETA   - DOUBLE PRECISION.
*           On entry,  BETA  specifies the scalar  beta.  When  BETA  is
*           supplied as zero then C need not be set on input.
*           Unchanged on exit.
*
*  C      - DOUBLE PRECISION array of DIMENSION ( LDC, n ).
*           Before entry, the leading  m by n  part of the array  C must
*           contain the matrix  C,  except when  beta  is zero, in which
*           case C need not be set on entry.
*           On exit, the array  C  is overwritten by the  m by n  matrix
*           ( alpha*op( A )*op( B ) + beta*C ).
*
*  LDC    - INTEGER.
*           On entry, LDC specifies the first dimension of C as declared
*           in  the  calling  (sub)  program.   LDC  must  be  at  least
*           max( 1, m ).
*           Unchanged on exit.
*
*
*  Level 3 Blas routine.
*
*  -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*     Slight modifications made by Alan Heckert 8/97 to 
*     incorporate into Dataplot (no numerical modifications,
*     just error handling and printing)
*
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
CCCCC EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     .. Local Scalars ..
      LOGICAL            NOTA, NOTB
      INTEGER            I, INFO, J, L, NCOLA, NROWA, NROWB
      DOUBLE PRECISION   TEMP
*     .. Parameters ..
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Executable Statements ..
*
*     Set  NOTA  and  NOTB  as  true if  A  and  B  respectively are not
*     transposed and set  NROWA, NCOLA and  NROWB  as the number of rows
*     and  columns of  A  and the  number of  rows  of  B  respectively.
*
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
*
*     Test the input parameters.
*
      INFO = 0
      IF(      ( .NOT.NOTA                 ).AND.
     $         ( .NOT.LSAME( TRANSA, 'C' ) ).AND.
     $         ( .NOT.LSAME( TRANSA, 'T' ) )      )THEN
         INFO = 1
      ELSE IF( ( .NOT.NOTB                 ).AND.
     $         ( .NOT.LSAME( TRANSB, 'C' ) ).AND.
     $         ( .NOT.LSAME( TRANSB, 'T' ) )      )THEN
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
CCCCC    CALL XERBLA( 'DGEMM ', INFO )
         WRITE(ICOUT,1001)
         CALL DPWRST('XXX','BUG ')
         IERROR='YES'
         RETURN
      END IF
 1001 FORMAT('***** RECIPE ERROR: INTERNAL ERROR FROM DGEMM, INVALID',
     1' ARGUMENTS.')
*
*     Quick return if possible.
*
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.
     $    ( ( ( ALPHA.EQ.ZERO ).OR.( K.EQ.0 ) ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
*
*     And if  alpha.eq.zero.
*
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
*
*     Start the operations.
*
      IF( NOTB )THEN
         IF( NOTA )THEN
*
*           Form  C := alpha*A*B + beta*C.
*
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
*
*           Form  C := alpha*A'*B + beta*C
*
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
*
*           Form  C := alpha*A*B' + beta*C
*
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
*
*           Form  C := alpha*A'*B' + beta*C
*
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
*
      RETURN
*
*     End of DGEMM .
*
      END
      SUBROUTINE DGEMV ( TRANS, M, N, ALPHA, A, LDA, X, INCX,
     $                   BETA, Y, INCY,
     $                   IERROR )
*     .. Scalar Arguments ..
      DOUBLE PRECISION   ALPHA, BETA
      INTEGER            INCX, INCY, LDA, M, N
      CHARACTER*1        TRANS
      CHARACTER*4        IERROR
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), X( * ), Y( * )
*     ..
*
*  Purpose
*  =======
*
*  DGEMV  performs one of the matrix-vector operations
*
*     y := alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y,
*
*  where alpha and beta are scalars, x and y are vectors and A is an
*  m by n matrix.
*
*  Parameters
*  ==========
*
*  TRANS  - CHARACTER*1.
*           On entry, TRANS specifies the operation to be performed as
*           follows:
*
*              TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.
*
*              TRANS = 'T' or 't'   y := alpha*A'*x + beta*y.
*
*              TRANS = 'C' or 'c'   y := alpha*A'*x + beta*y.
*
*           Unchanged on exit.
*
*  M      - INTEGER.
*           On entry, M specifies the number of rows of the matrix A.
*           M must be at least zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry, N specifies the number of columns of the matrix A.
*           N must be at least zero.
*           Unchanged on exit.
*
*  ALPHA  - DOUBLE PRECISION.
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
*           Before entry, the leading m by n part of the array A must
*           contain the matrix of coefficients.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. LDA must be at least
*           max( 1, m ).
*           Unchanged on exit.
*
*  X      - DOUBLE PRECISION array of DIMENSION at least
*           ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'
*           and at least
*           ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.
*           Before entry, the incremented array X must contain the
*           vector x.
*           Unchanged on exit.
*
*  INCX   - INTEGER.
*           On entry, INCX specifies the increment for the elements of
*           X. INCX must not be zero.
*           Unchanged on exit.
*
*  BETA   - DOUBLE PRECISION.
*           On entry, BETA specifies the scalar beta. When BETA is
*           supplied as zero then Y need not be set on input.
*           Unchanged on exit.
*
*  Y      - DOUBLE PRECISION array of DIMENSION at least
*           ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'
*           and at least
*           ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
*           Before entry with BETA non-zero, the incremented array Y
*           must contain the vector y. On exit, Y is overwritten by the
*           updated vector y.
*
*  INCY   - INTEGER.
*           On entry, INCY specifies the increment for the elements of
*           Y. INCY must not be zero.
*           Unchanged on exit.
*
*
*  Level 2 Blas routine.
*
*  -- Written on 22-October-1986.
*     Jack Dongarra, Argonne National Lab.
*     Jeremy Du Croz, Nag Central Office.
*     Sven Hammarling, Nag Central Office.
*     Richard Hanson, Sandia National Labs.
*
*     Slight modifications 8/97 by Alan Heckert to incorporate
*     into Dataplot.  No numerical modifications, just for
*     error handling and printing.
*
*     .. Parameters ..
C
      INCLUDE 'DPCOP2.INC'
C
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     .. Local Scalars ..
      DOUBLE PRECISION   TEMP
      INTEGER            I, INFO, IX, IY, J, JX, JY, KX, KY, LENX, LENY
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
CCCCC EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      IERROR='NO'
      INFO = 0
      IF     ( .NOT.LSAME( TRANS, 'N' ).AND.
     $         .NOT.LSAME( TRANS, 'T' ).AND.
     $         .NOT.LSAME( TRANS, 'C' )      )THEN
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
CCCCC    CALL XERBLA( 'DGEMV ', INFO )
         WRITE(ICOUT,1001)
         CALL DPWRST('XXX','BUG ')
         IERROR='YES'
         RETURN
      END IF
 1001 FORMAT('***** RECIPE ERROR: INTERNAL ERROR FROM DGEMV, INVALID',
     1' ARGUMENTS.')
*
*     Quick return if possible.
*
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.
     $    ( ( ALPHA.EQ.ZERO ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
*
*     Set  LENX  and  LENY, the lengths of the vectors x and y, and set
*     up the start points in  X  and  Y.
*
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
*
*     Start the operations. In this version the elements of A are
*     accessed sequentially with one pass through A.
*
*     First form  y := beta*y.
*
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
      IF( ALPHA.EQ.ZERO )
     $   RETURN
      IF( LSAME( TRANS, 'N' ) )THEN
*
*        Form  y := alpha*A*x + y.
*
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
*
*        Form  y := alpha*A'*x + y.
*
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
*
      RETURN
*
*     End of DGEMV .
*
      END
      SUBROUTINE DGER  ( M, N, ALPHA, X, INCX, Y, INCY, A, LDA, IERROR )
*     .. Scalar Arguments ..
      DOUBLE PRECISION   ALPHA
      INTEGER            INCX, INCY, LDA, M, N
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), X( * ), Y( * )
C
      CHARACTER*4 IERROR
      INCLUDE 'DPCOP2.INC'
C
*     ..
*
*  Purpose
*  =======
*
*  DGER   performs the rank 1 operation
*
*     A := alpha*x*y' + A,
*
*  where alpha is a scalar, x is an m element vector, y is an n element
*  vector and A is an m by n matrix.
*
*  Parameters
*  ==========
*
*  M      - INTEGER.
*           On entry, M specifies the number of rows of the matrix A.
*           M must be at least zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry, N specifies the number of columns of the matrix A.
*           N must be at least zero.
*           Unchanged on exit.
*
*  ALPHA  - DOUBLE PRECISION.
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  X      - DOUBLE PRECISION array of dimension at least
*           ( 1 + ( m - 1 )*abs( INCX ) ).
*           Before entry, the incremented array X must contain the m
*           element vector x.
*           Unchanged on exit.
*
*  INCX   - INTEGER.
*           On entry, INCX specifies the increment for the elements of
*           X. INCX must not be zero.
*           Unchanged on exit.
*
*  Y      - DOUBLE PRECISION array of dimension at least
*           ( 1 + ( n - 1 )*abs( INCY ) ).
*           Before entry, the incremented array Y must contain the n
*           element vector y.
*           Unchanged on exit.
*
*  INCY   - INTEGER.
*           On entry, INCY specifies the increment for the elements of
*           Y. INCY must not be zero.
*           Unchanged on exit.
*
*  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
*           Before entry, the leading m by n part of the array A must
*           contain the matrix of coefficients. On exit, A is
*           overwritten by the updated matrix.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. LDA must be at least
*           max( 1, m ).
*           Unchanged on exit.
*
*
*  Level 2 Blas routine.
*
*  -- Written on 22-October-1986.
*     Jack Dongarra, Argonne National Lab.
*     Jeremy Du Croz, Nag Central Office.
*     Sven Hammarling, Nag Central Office.
*     Richard Hanson, Sandia National Labs.
*
*     Minor modifications 8/97 by Alan Heckert to incorporate
*     into Dataplot.  No numerical modifications.  Just
*     error handling and printing.
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER        ( ZERO = 0.0D+0 )
*     .. Local Scalars ..
      DOUBLE PRECISION   TEMP
      INTEGER            I, INFO, IX, J, JY, KX
*     .. External Subroutines ..
CCCCC EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
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
CCCCC    CALL XERBLA( 'DGER  ', INFO )
         WRITE(ICOUT,1001)
         CALL DPWRST('XXX','BUG ')
         IERROR='YES'
         RETURN
      END IF
 1001 FORMAT('***** RECIPE ERROR: INTERNAL ERROR FROM DGER, INVALID',
     1' ARGUMENTS.')
*
*     Quick return if possible.
*
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.( ALPHA.EQ.ZERO ) )
     $   RETURN
*
*     Start the operations. In this version the elements of A are
*     accessed sequentially with one pass through A.
*
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
*
      RETURN
*
*     End of DGER  .
*
      END
      SUBROUTINE DGSL1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR GREEK SIMPLEX LOWER CASE (PART 1).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
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
C     DEFINE CHARACTER    627--LOWER CASE ALPH
C
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
C
      DATA IXMIND(   1)/ -10/
      DATA IXMAXD(   1)/  11/
      DATA IXDELD(   1)/  21/
      DATA ISTARD(   1)/   1/
      DATA NUMCOO(   1)/  22/
C
C     DEFINE CHARACTER    628--LOWER CASE BETA
C
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
C
      DATA IXMIND(   2)/  -9/
      DATA IXMAXD(   2)/  10/
      DATA IXDELD(   2)/  19/
      DATA ISTARD(   2)/  23/
      DATA NUMCOO(   2)/  28/
C
C     DEFINE CHARACTER    629--LOWER CASE GAMM
C
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
C
      DATA IXMIND(   3)/  -9/
      DATA IXMAXD(   3)/  10/
      DATA IXDELD(   3)/  19/
      DATA ISTARD(   3)/  51/
      DATA NUMCOO(   3)/  15/
C
C     DEFINE CHARACTER    630--LOWER CASE DELT
C
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
C
      DATA IXMIND(   4)/  -9/
      DATA IXMAXD(   4)/   9/
      DATA IXDELD(   4)/  18/
      DATA ISTARD(   4)/  66/
      DATA NUMCOO(   4)/  23/
C
C     DEFINE CHARACTER    631--LOWER CASE EPSI
C
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
C
      DATA IXMIND(   5)/  -8/
      DATA IXMAXD(   5)/   8/
      DATA IXDELD(   5)/  16/
      DATA ISTARD(   5)/  89/
      DATA NUMCOO(   5)/  17/
C
C     DEFINE CHARACTER    632--LOWER CASE ZETA
C
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
C
      DATA IXMIND(   6)/  -8/
      DATA IXMAXD(   6)/   7/
      DATA IXDELD(   6)/  15/
      DATA ISTARD(   6)/ 106/
      DATA NUMCOO(   6)/  21/
C
C     DEFINE CHARACTER    633--LOWER CASE ETA
C
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
C
      DATA IXMIND(   7)/ -10/
      DATA IXMAXD(   7)/  10/
      DATA IXDELD(   7)/  20/
      DATA ISTARD(   7)/ 127/
      DATA NUMCOO(   7)/  17/
C
C     DEFINE CHARACTER    634--LOWER CASE THET
C
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
C
      DATA IXMIND(   8)/ -11/
      DATA IXMAXD(   8)/  10/
      DATA IXDELD(   8)/  21/
      DATA ISTARD(   8)/ 144/
      DATA NUMCOO(   8)/  26/
C
C     DEFINE CHARACTER    635--LOWER CASE IOTA
C
      DATA IOPERA( 170),IX( 170),IY( 170)/'MOVE',   0,   5/
      DATA IOPERA( 171),IX( 171),IY( 171)/'DRAW',  -2,  -2/
      DATA IOPERA( 172),IX( 172),IY( 172)/'DRAW',  -3,  -6/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW',  -3,  -8/
      DATA IOPERA( 174),IX( 174),IY( 174)/'DRAW',  -2,  -9/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW',   0,  -9/
      DATA IOPERA( 176),IX( 176),IY( 176)/'DRAW',   2,  -7/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW',   3,  -5/
C
      DATA IXMIND(   9)/  -6/
      DATA IXMAXD(   9)/   5/
      DATA IXDELD(   9)/  11/
      DATA ISTARD(   9)/ 170/
      DATA NUMCOO(   9)/   8/
C
C     DEFINE CHARACTER    636--LOWER CASE KAPP
C
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
C
      DATA IXMIND(  10)/  -9/
      DATA IXMAXD(  10)/   9/
      DATA IXDELD(  10)/  18/
      DATA ISTARD(  10)/ 178/
      DATA NUMCOO(  10)/  16/
C
C     DEFINE CHARACTER    637--LOWER CASE LAMB
C
      DATA IOPERA( 194),IX( 194),IY( 194)/'MOVE',  -7,  12/
      DATA IOPERA( 195),IX( 195),IY( 195)/'DRAW',  -5,  12/
      DATA IOPERA( 196),IX( 196),IY( 196)/'DRAW',  -3,  11/
      DATA IOPERA( 197),IX( 197),IY( 197)/'DRAW',  -2,  10/
      DATA IOPERA( 198),IX( 198),IY( 198)/'DRAW',   6,  -9/
      DATA IOPERA( 199),IX( 199),IY( 199)/'MOVE',   0,   5/
      DATA IOPERA( 200),IX( 200),IY( 200)/'DRAW',  -6,  -9/
C
      DATA IXMIND(  11)/  -8/
      DATA IXMAXD(  11)/   8/
      DATA IXDELD(  11)/  16/
      DATA ISTARD(  11)/ 194/
      DATA NUMCOO(  11)/   7/
C
C     DEFINE CHARACTER    638--LOWER CASE MU
C
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
C
      DATA IXMIND(  12)/ -10/
      DATA IXMAXD(  12)/  11/
      DATA IXDELD(  12)/  21/
      DATA ISTARD(  12)/ 201/
      DATA NUMCOO(  12)/  18/
C
C     DEFINE CHARACTER    639--LOWER CASE NU
C
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
C
      DATA IXMIND(  13)/  -9/
      DATA IXMAXD(  13)/   9/
      DATA IXDELD(  13)/  18/
      DATA ISTARD(  13)/ 219/
      DATA NUMCOO(  13)/  12/
C
C     DEFINE CHARACTER    640--LOWER CASE XI
C
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
C
      DATA IXMIND(  14)/  -8/
      DATA IXMAXD(  14)/   8/
      DATA IXDELD(  14)/  16/
      DATA ISTARD(  14)/ 231/
      DATA NUMCOO(  14)/  26/
C
C     DEFINE CHARACTER    641--LOWER CASE OMIC
C
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
C
      DATA IXMIND(  15)/  -8/
      DATA IXMAXD(  15)/   9/
      DATA IXDELD(  15)/  17/
      DATA ISTARD(  15)/ 257/
      DATA NUMCOO(  15)/  17/
C
C     DEFINE CHARACTER    642--LOWER CASE PI
C
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
C
      DATA IXMIND(  16)/ -11/
      DATA IXMAXD(  16)/  11/
      DATA IXDELD(  16)/  22/
      DATA ISTARD(  16)/ 274/
      DATA NUMCOO(  16)/  10/
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
   51 FORMAT('***** AT THE BEGINNING OF DGSL1--')
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
      SUBROUTINE DGSL2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR GREEK SIMPLEX LOWER CASE (PART 2).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--87/4
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MARCH     1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --MARCH     1987.
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
      INCLUDE 'DPCOP2.INC'
C
C-----DATA STATEMENTS-------------------------------------------------
C
C     DEFINE CHARACTER    643--LOWER CASE RHO
C
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
C
      DATA IXMIND(  17)/  -9/
      DATA IXMAXD(  17)/   9/
      DATA IXDELD(  17)/  18/
      DATA ISTARD(  17)/   1/
      DATA NUMCOO(  17)/  18/
C
C     DEFINE CHARACTER    644--LOWER CASE SIGM
C
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
C
      DATA IXMIND(  18)/  -9/
      DATA IXMAXD(  18)/  11/
      DATA IXDELD(  18)/  20/
      DATA ISTARD(  18)/  19/
      DATA NUMCOO(  18)/  17/
C
C     DEFINE CHARACTER    645--LOWER CASE TAU
C
      DATA IOPERA(  36),IX(  36),IY(  36)/'MOVE',   1,   5/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',  -2,  -9/
      DATA IOPERA(  38),IX(  38),IY(  38)/'MOVE',  -8,   2/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',  -6,   4/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',  -3,   5/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',   8,   5/
C
      DATA IXMIND(  19)/ -10/
      DATA IXMAXD(  19)/  10/
      DATA IXDELD(  19)/  20/
      DATA ISTARD(  19)/  36/
      DATA NUMCOO(  19)/   6/
C
C     DEFINE CHARACTER    646--LOWER CASE UPSI
C
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
C
      DATA IXMIND(  20)/ -10/
      DATA IXMAXD(  20)/  10/
      DATA IXDELD(  20)/  20/
      DATA ISTARD(  20)/  42/
      DATA NUMCOO(  20)/  15/
C
C     DEFINE CHARACTER    647--LOWER CASE PHI
C
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
C
      DATA IXMIND(  21)/ -11/
      DATA IXMAXD(  21)/  11/
      DATA IXDELD(  21)/  22/
      DATA ISTARD(  21)/  57/
      DATA NUMCOO(  21)/  20/
C
C     DEFINE CHARACTER    648--LOWER CASE CHI
C
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
C
      DATA IXMIND(  22)/  -9/
      DATA IXMAXD(  22)/   9/
      DATA IXDELD(  22)/  18/
      DATA ISTARD(  22)/  77/
      DATA NUMCOO(  22)/  12/
C
C     DEFINE CHARACTER    649--LOWER CASE PSI
C
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
C
      DATA IXMIND(  23)/ -12/
      DATA IXMAXD(  23)/  11/
      DATA IXDELD(  23)/  23/
      DATA ISTARD(  23)/  89/
      DATA NUMCOO(  23)/  18/
C
C     DEFINE CHARACTER    650--LOWER CASE OMEG
C
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
C
      DATA IXMIND(  24)/ -12/
      DATA IXMAXD(  24)/  11/
      DATA IXDELD(  24)/  23/
      DATA ISTARD(  24)/ 107/
      DATA NUMCOO(  24)/  21/
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
   51 FORMAT('***** AT THE BEGINNING OF DGSL2--')
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
      SUBROUTINE DIFF(IORD,X0,XMIN,XMAX,F,EPS,ACC,DERIV,ERROR,IFAIL)
C
C             NUMERICAL DIFFERENTIATION OF USER DEFINED FUNCTION
C
C                         DAVID KAHANER, NBS (GAITHERSBURG) 
C
C  THE PROCEDURE DIFFERENTIATE CALCULATES THE FIRST, SECOND OR
C   THIRD ORDER DERIVATIVE OF A FUNCTION BY USING NEVILLE'S PROCESS TO
C   EXTRAPOLATE FROM A SEQUENCE OF SIMPLE POLYNOMIAL APPROXIMATIONS BASED ON
C   INTERPOLATING POINTS DISTRIBUTED SYMMETRICALLY ABOUT X0 (OR LYING ONLY ON
C   ONE SIDE OF X0 SHOULD THIS BE NECESSARY).  IF THE SPECIFIED TOLERANCE IS
C   NON-ZERO THEN THE PROCEDURE ATTEMPTS TO SATISFY THIS ABSOLUTE OR RELATIVE
C   ACCURACY REQUIREMENT, WHILE IF IT IS UNSUCCESSFUL OR IF THE TOLERANCE IS
C   SET TO ZERO THEN THE RESULT HAVING THE MINIMUM ACHIEVABLE ESTIMATED ERROR
C   IS RETURNED INSTEAD.
C
C INPUT PARAMETERS:
C IORD = 1, 2 OR 3 SPECIFIES THAT THE FIRST, SECOND OR THIRD ORDER
C   DERIVATIVE,RESPECTIVELY, IS REQUIRED.
C X0 IS THE POINT AT WHICH THE DERIVATIVE OF THE FUNCTION IS TO BE CALCULATED.
C XMIN, XMAX RESTRICT THE INTERPOLATING POINTS TO LIE IN [XMIN, XMAX], WHICH
C   SHOULD BE THE LARGEST INTERVAL INCLUDING X0 IN WHICH THE FUNCTION IS
C   CALCULABLE AND CONTINUOUS.
C F, A REAL PROCEDURE SUPPLIED BY THE USER, MUST YIELD THE VALUE OF THE
C   FUNCTION AT X FOR ANY X IN [XMIN, XMAX] WHEN CALLED BY F(X).
C EPS DENOTES THE TOLERANCE, EITHER ABSOLUTE OR RELATIVE.  EPS=0 SPECIFIES THAT 
C   THE ERROR IS TO BE MINIMISED, WHILE EPS>0 OR EPS<0 SPECIFIES THAT THE
C   ABSOLUTE OR RELATIVE ERROR, RESPECTIVELY, MUST NOT EXCEED ABS(EPS) IF
C   POSSIBLE.  THE ACCURACY REQUIREMENT SHOULD NOT BE MADE STRICTER THAN
C   NECESSARY, SINCE THE AMOUNT OF COMPUTATION TENDS TO INCREASE AS
C   THE MAGNITUDE OF EPS DECREASES, AND IS PARTICULARLY HIGH WHEN EPS=0.
C ACC DENOTES THAT THE ABSOLUTE (ACC>0) OR RELATIVE (ACC<0) ERRORS IN THE
C   COMPUTED VALUES OF THE FUNCTION ARE MOST UNLIKELY TO EXCEED ABS(ACC), WHICH 
C   SHOULD BE AS SMALL AS POSSIBLE.  IF THE USER CANNOT ESTIMATE ACC WITH
C   COMPLETE CONFIDENCE, THEN IT SHOULD BE SET TO ZERO.
C
C OUTPUT PARAMETERS:
C DERIV IS THE CALCULATED VALUE OF THE DERIVATIVE.
C ERROR IS AN ESTIMATED UPPER BOUND ON THE MAGNITUDE OF THE ABSOLUTE ERROR IN
C   THE CALCULATED RESULT.  IT SHOULD ALWAYS BE EXAMINED, SINCE IN EXTREME CASE 
C   MAY INDICATE THAT THERE ARE NO CORRECT SIGNIFICANT DIGITS IN THE VALUE
C   RETURNED FOR DERIVATIVE.
C IFAIL WILL HAVE ONE OF THE FOLLOWING VALUES ON EXIT:
C   0   THE PROCEDURE WAS SUCCESSFUL.
C   1   THE ESTIMATED ERROR IN THE RESULT EXCEEDS THE (NON-ZERO) REQUESTED
C          ERROR, BUT THE MOST ACCURATE RESULT POSSIBLE HAS BEEN RETURNED.
C   2   INPUT DATA INCORRECT (DERIVATIVE AND ERROR WILL BE UNDEFINED).
C   3   THE INTERVAL [XMIN, XMAX] IS TOO SMALL (DERIVATIVE AND ERROR WILL BE
C          UNDEFINED);
C
      EXTERNAL F
      REAL X0,XMIN,XMAX,ACC,DERIV,ERROR,BETA,BETA4,H,H0,H1,H2,
     +NEWH1,NEWH2,HEVAL,HPREV,BASEH,HACC1,HACC2,NHACC1,
     +NHACC2,MINH,MAXH,MAXH1,MAXH2,TDERIV,F0,TWOF0,F1,F2,F3,F4,FMAX,
     +MAXFUN,PMAXF,DF1,DELTAF,PDELTA,Z,ZPOWER,C0F0,C1,C2,C3,DNEW,DPREV,
     +RE,TE,NEWERR,TEMERR,NEWACC,PACC1,PACC2,FACC1,FACC2,ACC0,
     +ACC1,ACC2,RELACC,TWOINF,TWOSUP,S, 
     +D(10),DENOM(10),E(10),MINERR(10),MAXF(0:10),SAVE(0:13),
     +STOREF(-45:45),FACTOR
C
      INTEGER IORD,IFAIL,ETA,INF,SUP,I,J,K,N,NMAX,METHOD,SIGNH,FCOUNT,
     +INIT
      LOGICAL IGNORE(10),CONTIN,SAVED
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
C
C ETA IS THE MINIMUM NUMBER OF SIGNIFICANT BINARY DIGITS (APART FROM THE
C SIGN DIGIT) USED TO REPRESENT THE MANTISSA OF REAL NUMBERS. IT SHOULD
C BE DEVREASED BY ONE IF THE COMPUTER TRUNCATES RATHER THAN ROUNDS.
C INF, SUP ARE THE LARGEST POSSIBLE POSITIVE INTEGERS SUBJECT TO
C 2**(-INF), -2**(-INF), 2**SUP, AND -2**SUP ALL BEING REPRESENTABLE REAL
C NUMBERS.
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
C
      ETA=I1MACH(11) - 1
      INF=-I1MACH(12) - 2
      SUP=I1MACH(13)-1
      IF(IORD.LT.1 .OR. IORD.GT.3 .OR. XMAX.LE.XMIN .OR.
     +  X0.GT.XMAX .OR. X0.LT.XMIN) THEN
          IFAIL = 2 
          RETURN
      ENDIF
C
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
C         INTERVAL TOO SMALL
          IFAIL =3
          RETURN
      ENDIF
      IF(ACC.LT.0.) THEN
          IF(-ACC.GT.RELACC)RELACC = -ACC
          ACC = 0.
      ENDIF
C
C     DETERMINE THE SMALLEST SPACING AT WHICH THE CALCULATED
C     FUNCTION VALUES ARE UNEQUAL NEAR X0.
C
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
   80 IF(DF1 .NE. 0. .OR. Z*H .GT. MAXH1) GOTO 100
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
      GOTO 80
  100 CONTINUE
C
      IF(DF1 .EQ. 0.) THEN
C         CONSTANT FUNCTION
          DERIV = 0.
          ERROR = 0.
          IFAIL = 0 
          RETURN
      ENDIF
      IF(H .GT. MAXH1/128.) THEN
C         MINIMUM H TOO LARGE 
          IFAIL = 3 
          RETURN
      ENDIF
C
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
C
C     METHOD 1 USES 1-SIDED FORMULAE, AND METHOD 2 SYMMETRIC.
C         NOW ESTIMATE ACCURACY OF CALCULATED FUNCTION VALUES.
C
      IF(METHOD.NE.2 .OR. IORD.EQ.2) THEN
          IF(X0.NE.0.) THEN
              CALL FACCUR(0.,-H1,ACC0,X0,F,TWOINF,F0,F1)
          ELSE
              ACC0 = 0.
          ENDIF
      ENDIF
C
      IF(ABS(H1) .GT. TWOSUP/128.) THEN 
          HACC1 = TWOSUP
      ELSE
          HACC1 = 128.*H1
      ENDIF
C
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
C
  130 CONTINUE
      N = N+1
      IF(.NOT. CONTIN) GOTO 800
C
      IF(INIT.EQ.3) THEN
C         CALCULATE COEFFICIENTS FOR DIFFERENTIATION FORMULAE
C             AND NEVILLE EXTRAPOLATION ALGORITHM 
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
        ELSEIF((METHOD.NE.2.AND.IORD.EQ.1) .OR. (METHOD.EQ.2.AND.
     +            IORD.EQ.2)) THEN
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
C
C
      IF(INIT.GE.2) THEN
C         INITIALIZATION OF STEPLENGTHS, ACCURACY AND OTHER 
C             PARAMETERS
C
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
          IF((METHOD.NE.2 .OR. IORD.EQ.2) .AND. NEWACC.LT.ACC0)
     +            NEWACC = ACC0
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
C
C     CALCULATE NEW OR INITIAL FUNCTION VALUES
C
      IF(INIT.EQ.1 .AND. (N.EQ.0 .OR. IORD.EQ.1) .AND.
     +        .NOT.(METHOD.EQ.2 .AND. FCOUNT.GE.45)) THEN
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
C
C     EVALUATE A NEW APPROXIMATION DNEW TO THE DERIVATIVE
C
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
C
      IF(INIT.EQ.0) THEN
C         UPDATE ESTIMATED ACCURACY OF FUNCTION VALUES
          NEWACC = ACC
          IF((METHOD.NE.2 .OR. IORD.EQ.2) .AND. NEWACC.LT.ACC0)
     +        NEWACC = ACC0
          IF(METHOD.NE.-1 .AND. ABS(NHACC1).LE.1.125*ABS(HEVAL)/BETA4)
     +               THEN
              NHACC1 = HEVAL
              PACC1 = FACC1
              CALL FACCUR(NHACC1,NEWH1,FACC1,X0,F,TWOINF,F0,F1)
              IF(FACC1.LT.PACC1) FACC1=(3*FACC1+PACC1)/4.
          ENDIF
          IF(METHOD.NE.1 .AND. ABS(NHACC2).LE.1.125*ABS(HEVAL)/BETA4) 
     +            THEN
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
          IF(METHOD.EQ.2 .AND. NEWACC.LT.(FACC1+FACC2)/2.)
     +            NEWACC = (FACC1+FACC2)/2.
      ENDIF
C
C     EVALUATE SUCCESSIVE ELEMENTS OF THE CURRENT ROW IN THE NEVILLE
C     ARRAY, ESTIMATING AND EXAMINING THE TRUNCATION AND ROUNDING
C     ERRORS IN EACH
C
      CONTIN = N.LT.NMAX
      HPREV = ABS(H)
      FMAX = MAXF(N)
      IF((METHOD.NE.2 .OR. IORD.EQ.2) .AND. FMAX.LT.ABS(F0))
     +        FMAX = ABS(F0)
C
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
              IF((INIT.EQ.0 .OR. (K.EQ.2 .AND. .NOT.IGNORE(1)))
     +                .AND. NEWERR.LT.ERROR) THEN 
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
              IF(INIT.EQ.1 .AND. N.EQ.2 .AND. K.EQ.1 .AND.
     +                .NOT.IGNORE(1)) THEN
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
C
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
          ELSEIF(.NOT. (IGNORE(1) .OR. IGNORE(2)) .AND. N.EQ.2
     +            .AND. BETA4*FACTOR*ABS(HEVAL).LE.MAXH) THEN
C             SAVE ALL CURRENT VALUES IN CASE OF RETURN TO
C                 CURRENT POINT
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
      GOTO 130
  800 IF(EPS.LT.0.) THEN
          S = ABS(EPS*DERIV)
      ELSE
          S = EPS
      ENDIF
      IFAIL = 0
      IF(EPS.NE.0. .AND. ERROR.GT.S) IFAIL = 1
      RETURN
      END 
      SUBROUTINE DIFFER(NDIM, A, B, WIDTH, Z, DIF, FUNCTN, 
     &     DIVAXN, DIFCLS)
*
*     Compute fourth differences and subdivision axes
*
      EXTERNAL FUNCTN
      INTEGER I, NDIM, DIVAXN, DIFCLS
      DOUBLE PRECISION 
     &     A(NDIM), B(NDIM), WIDTH(NDIM), Z(NDIM), DIF(NDIM), FUNCTN
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
*     Do not include differences below roundoff
            IF ( FUNCEN + FRTHDF/8 .NE. FUNCEN ) 
     &           DIF(I) = DIF(I) + ABS(FRTHDF)*WIDTH(I)
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
C
      RETURN
      END
      SUBROUTINE DIGITS(XVAL,IWRITE,XDIGI,NDIGI,ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE RETURNS A VECTOR CONTAINING THE
C              DIGITS FROM THE POSITIVE INTEGER PART OF A NUMBER
C              (I.E., FOR NEGATIVE NUMBERS TAKE THE ABSOLUTE VALUE).
C     INPUT  ARGUMENTS--XVAL   = THE SINGLE PRECISION VALUE FOR WHICH
C                                THE DIGITS WILL BE EXTRACTED
C     OUTPUT ARGUMENTS--XDIGI  = THE SINGLE PRECISION VECTOR OF THE
C                                COMPUTED DIGITS
C                     --NDIGI  = THE INTEGER VALUE OF THE NUMBER OF
C                                DIGITS
C     OUTPUT--THE COMPUTED SINGLE PRECISION VECTOR OF THE
C             DIGITS FROM THE INTEGER PART OF THE NUMBER
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGYS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2015.1
C     ORIGINAL VERSION--JANUARY   2015.
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
      CHARACTER*1 IATEMP
      CHARACTER*20 IA
C
C---------------------------------------------------------------------
C
      DIMENSION XDIGI(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DIGI'
      ISUBN2='TS  '
      IERROR='NO'
      NDIGI=0
C
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
C
C               ********************************************
C               **  STEP 1--                              **
C               **  EXTRACT INTEGER PART OF NUMBER        **
C               ********************************************
C
      EPS=0.0001
      XVALT=ABS(XVAL+EPS)
      IVAL=INT(XVALT)
C
C               ********************************************
C               **  STEP 2--                              **
C               **  EXTRACT THE DIGITS                    **
C               ********************************************
C
C
      IA=' '
      WRITE(IA(1:20),'(I20)')IVAL
C
      DO100I=1,20
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
C
      IF(NDIGI.EQ.0)THEN
        NDIGI=NDIGI+1
        XDIGI(NDIGI)=0.0
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'GITS')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DIGITS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NDIGI,IVAL,XVALT,IA
 9013   FORMAT('NDIGI,IVAL,XVALT,IA = ',2I8,G15.7,2X,A20)
        CALL DPWRST('XXX','BUG ')
        DO9014I=1,NDIGI
          WRITE(ICOUT,9015)I,XDIGI(I)
 9015     FORMAT('I,XDIGI(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
 9014   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DIPERC(X,N,XPT,IWRITE,DIOUT,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE "PERCENTAGE DIFFERENCE"
C              STATISTIC Di% GIVEN IN ISO 13528 (P. 25):
C
C                 D(i)% = (X(i) - Xpt)/Xpt)*100
C
C              WHERE Xpt IS A CONSENSUS OR ASSIGNED VALUE.
C
C              THE D(i) = X(i) - Xpt OR D(i)% IS COMPARED TO
C              AN "ALLOWANCE FOR MEASUREMENT ERROR" VALUE
C              DeltaE.  THAT IS
C
C                -DeltaE < D(i) < DeltaE
C
C              THE PERCENTAGE VERSION IS TYPICALLY COMPARED TO
C              SOME TRANSFORMATION OF DeltaE.
C
C              NOTE THAT XPT AND DELTAE ARE NOT COMPUTED FROM THE
C              CURRENT DATA.  THE XPT IS CONSIDERED THE "TRUE" VALUE
C              (OR THE BEST GUESS OF THE TRUE VALUE).  THE ISO 13528
C              STANDARD DISCUSSES NUMEROUS WAYS OF DETERMINING THIS
C              VALUE.  THE DELTAE IS AN "ACCEPTABLE" ERROR.  THERE IS
C              NO STANDARD WAY FOR DETERMINING THIS.
C
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VECTOR OF
C                                (UNSORTED OR SORTED) OBSERVATIONS.
C                     --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                                IN THE VECTOR X.
C                     --XPT    = THE SINGLE PRECISION VALUE CONTAINING
C                                THE ASSIGNED VALUE
C     OUTPUT ARGUMENTS--DIOUT  = THE SINGLE PRECISION VECTOR OF THE
C                                COMPUTED Di% VALUES.
C     OUTPUT--THE COMPUTED SINGLE PRECISION VECTOR OF THE SAMPLE Di%
C             VALUES.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCE--ISO 13528, SECOND EDITION, STATISTICAL METHODS FOR USE
C                IN PROFICIENCY TESTING BY INTERLABORATORY COMPARISONS,
C                2015, PP. 25.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2016.2
C     ORIGINAL VERSION--FEBRUARY  2016.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWRITE
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION DIOUT(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DIPE'
      ISUBN2='RC  '
C
      IERROR='NO'
C
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
        DO55I=1,N
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ************************
C               **  COMPUTE DiPERC    **
C               ************************
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      AN=N
C
      IF(N.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN DIPERC--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS IN THE RESPONSE ',
     1         'VARIABLE IS LESS THAN 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N
  117   FORMAT('      THE NUMBER OF OBSERVATIONS = ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               *****************************
C               **  STEP 2--               **
C               **  COMPUTE THE DIPERC     **
C               *****************************
C
      DO200I=1,N
        DIOUT(I)=((X(I) - XPT)/XPT)*100.
  200 CONTINUE
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
        WRITE(ICOUT,811)N
  811   FORMAT('THE NUMBER OF DI PERCENT VALUES GENERATED = ',I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'PERC')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DIPERC--')
        CALL DPWRST('XXX','BUG ')
        DO9012I=1,N
          WRITE(ICOUT,9015)I,X(I),DIOUT(I)
 9015     FORMAT('I,X(I),PAOUT(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9012   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DISCDF(IX,N,CDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
C              FUNCTION VALUE FOR THE DISCRETE UNIFORM (RECTANGULAR) 
C              DISTRIBUTION ON THE INTERVAL (0,N).
C              THIS DISTRIBUTION HAS MEAN = N/2
C              AND STANDARD DEVIATION = SQRT(N(N+2)/12)
C              THIS DISTRIBUTION HAS THE PROBABILITY
C              DENSITY FUNCTION F(X) = 1/(N+1).
C              IT HAS THE CUMULATIVE PROBABILITY DISTRIBUTION 
C              CDF(X) = (X+1)/(N+1)
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE CUMULATIVE DISTRIBUTION
C                                FUNCTION IS TO BE EVALUATED.
C                     --N        UPPER LIMIT OF DISTRIBUTION
C     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE
C                                DISTRIBUTION FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION CUMULATIVE DISTRIBUTION
C             FUNCTION VALUE CDF.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS. 
C     RESTRICTIONS--X SHOULD BE AN INTEGER BETWEEN 0 AND N, INCLUSIVELY.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--EVANS, HASTINGS, AND PEACOCK, STATISTICAL
C                 DISTRIBUTIONS, 2ND ED.--1993, CHAPTER 36
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE:  301-975-2855
C     ORIGINAL VERSION--SEPTEMBER 1994. 
C     UPDATED         --DECEMBER  1994. FIX BUG
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C---------------------------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(IX.LT.0.OR.IX.GT.N)GOTO50
      IF(N.LT.1)GOTO60
      GOTO90
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
    2 FORMAT(
     1'***** NON-FATAL DIAGNOSTIC--THE FIRST  INPUT ARGUMENT TO THE')
    3 FORMAT(
     1'      DISCDF SUBROUTINE IS OUTSIDE THE USUAL (0,N) INTERVAL ***')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8,' *****')
   12 FORMAT(
     1'***** FATAL DIAGNOSTIC--THE SECOND INPUT ARGUMENT TO THE')
   13 FORMAT(
     1'      DISCDF SUBROUTINE IS LESS THAN 1.                     ***')
C
C-----START POINT-----------------------------------------------------
C
   90 CONTINUE
      AX=REAL(IX)
CCCCC FIX FOLLOWING LINE.  DECEMBER 1994.
CCCCC AN=REAL(AN)
      AN=REAL(N)
      CDF=(AX+1.0)/(AN+1.0)
C
      RETURN
      END 
      SUBROUTINE DISPDF(IX,N,PDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
C              FUNCTION VALUE FOR THE DISCRETE UNIFORM (RECTANGULAR) 
C              DISTRIBUTION ON THE INTERVAL (0,N).
C              THIS DISTRIBUTION HAS MEAN = N/2
C              AND STANDARD DEVIATION = SQRT(N(N+2)/12)
C              THIS DISTRIBUTION HAS THE PROBABILITY
C              DENSITY FUNCTION F(X) = 1/(N+1)
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE PROBABILITY DENSITY
C                                FUNCTION IS TO BE EVALUATED.
C     OUTPUT ARGUMENTS--PDF    = THE SINGLE PRECISION PROBABILITY
C                                DENSITY FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
C             FUNCTION VALUE PDF.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS. 
C     RESTRICTIONS--X SHOULD BE BETWEEN 0 AND 1, INCLUSIVELY.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--JOHNSON AND KOTZ, CONTINUOUS UNIVARIATE
C                 DISTRIBUTIONS--2, 1970, PAGES 57-74.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING LABORATORY (205.03)
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE:  301-975-2855
C     ORIGINAL VERSION--SEPTEMBER 1994. 
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C---------------------------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      PDF=0.0
      IF(IX.LT.0.OR.IX.GT.N)GOTO50
      IF(N.LT.1)GOTO60
      GOTO90
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
    2 FORMAT(
     1'***** NON-FATAL DIAGNOSTIC--THE FIRST  INPUT ARGUMENT TO THE')
    3 FORMAT(
     1'      DISPDF SUBROUTINE IS OUTSIDE THE USUAL (0,N) INTERVAL **')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8,' *****')
   12 FORMAT(
     1'***** FATAL DIAGNOSTIC--THE SECOND INPUT ARGUMENT TO THE')
   13 FORMAT(
     1'      DISPDF SUBROUTINE IS LESS THAN 1.                     **')
C
C-----START POINT-----------------------------------------------------
C
   90 CONTINUE
      PDF=1.0/REAL(N+1)
C
      RETURN
      END 
      SUBROUTINE DISPPF(P,N,PPF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
C              FUNCTION VALUE FOR THE DISCRETE UNIFORM (RECTANGUALAR)
C              DISTRIBUTION FROM 0 TO N
C              THIS DISTRIBUTION HAS THE PROBABILITY DENSITY FUNCTION
C              F(X)=1/(N+1)
C              IT HAS THE PPF FUNCTION G(P)=P*(N+1)-1.
C              NOTE THAT THE PERCENT POINT FUNCTION OF A DISTRIBUTION
C              IS IDENTICALLY THE SAME AS THE INVERSE CUMULATIVE
C              DISTRIBUTION FUNCTION OF THE DISTRIBUTION.
C     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE
C                                (BETWEEN 0.0 AND 1.0)
C                                AT WHICH THE PERCENT POINT
C                                FUNCTION IS TO BE EVALUATED.
C                      --N     = UPPER LIMIT OF THE DISTRIBUTION
C     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT
C                                POINT FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PERCENT POINT
C             FUNCTION VALUE PPF.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--P SHOULD BE BETWEEN 0.0 AND 1.0, INCLUSIVELY.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
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
C     LANGUAGE--ANSI FORTRAN (1966)
C     VERSION NUMBER--94.9
C     ORIGINAL VERSION--SEPTEMBER 1994.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(P.LT.0.0.OR.P.GT.1.0)GOTO50
      IF(N.LT.1)GOTO60
      GOTO90
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
    1 FORMAT('***** FATAL ERROR--THE 1ST INPUT ARGUMENT TO THE ',
     1'DISPPF SUBROUTINE IS OUTSIDE THE ALLOWABLE (0,1) INTERVAL')
   12 FORMAT(
     1'***** FATAL DIAGNOSTIC--THE SECOND INPUT ARGUMENT TO THE')
   13 FORMAT(
     1'      DISPDF SUBROUTINE IS LESS THAN 1.                     **')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',E15.8,' *****')
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8,' *****')
C
C-----START POINT-----------------------------------------------------
C
      PPF=P*(REAL(N)+1.0)-1.0
      IPPF=INT(PPF)
      IF(IPPF.LT.0)IPPF=0
      IF(IPPF.GT.N)IPPF=N
      PPF=REAL(IPPF)
      RETURN
      END
      SUBROUTINE DISTIN(X,NX,IWRITE,Y,NY,IBUGA3,IERROR)
C
C     PURPOSE--COMPUTE DISTINCT VALUES OF A VARIABLE--
C              Y(1) = X(1)
C              Y(2) = X(2) OR X(3) OR X(4) ETC., THE FIRST ONE
C                     OF WHICH IS DIFFERENT FROM Y(1);
C              Y(3) = X(3) OR X(4) OR X(5) ETC., THE FIRST ONE
C                     OF WHICH IS DIFFERENT FROM Y(1) AND Y(2);
C              ETC.
C     NOTE--IT IS PERMISSIBLE TO HAVE THE OUTPUT VECTOR Y(.)
C           BEING IDENTICAL TO THE INPUT VECTOR X(.).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--FEBRUARY  1979.
C     UPDATED         --APRIL     1979.
C     UPDATED         --AUGUST    1981.
C     UPDATED         --MAY       1982.
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
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DIST'
      ISUBN2='IN  '
      IERROR='NO'
C
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
        DO55I=1,NX
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ********************************
C               **  COMPUTE DISTINCT VALUES.  **
C               ********************************
C
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
        IF(NX.LT.2)GOTO9000
        DO100I=2,NX
          DO120J=1,NY
            IF(X(I).EQ.Y(J))GOTO100
  120     CONTINUE
          NY=NY+1
          Y(NY)=X(I)
  100   CONTINUE
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DISTIN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGA3,IERROR
 9012 FORMAT('IBUGA3,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)NX,NY
 9013 FORMAT('NX,NY = ',2I8)
      CALL DPWRST('XXX','BUG ')
      DO9015I=1,NX
      WRITE(ICOUT,9016)I,X(I),Y(I)
 9016 FORMAT('I,X(I),Y(I) = ',I8,2E15.7)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DISTI2(X,NX,IWRITE,Y,NY,IBUGA3,IERROR)
C
C     PURPOSE--COMPUTE DISTI2CT VALUES OF A VARIABLE--
C              Y(1) = X(1)
C              Y(2) = X(2) OR X(3) OR X(4) ETC., THE FIRST ONE
C                     OF WHICH IS DIFFERENT FROM Y(1);
C              Y(3) = X(3) OR X(4) OR X(5) ETC., THE FIRST ONE
C                     OF WHICH IS DIFFERENT FROM Y(1) AND Y(2);
C              ETC.
C     NOTE--IT IS PERMISSIBLE TO HAVE THE OUTPUT VECTOR Y(.)
C           BEING IDENTICAL TO THE INPUT VECTOR X(.).
C     NOTE--THIS IS IDENTICAL TO DISTIN WITH THE EXCEPTION THAT
C           THIS VERSION WORKS ON DOUBLE PREICISION ARRAYS.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--97/8
C     ORIGINAL VERSION--AUGUST    1997.
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
      DOUBLE PRECISION X(*)
      DOUBLE PRECISION Y(*)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DIST'
      ISUBN2='IN  '
      IERROR='NO'
C
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
        DO55I=1,NX
          WRITE(ICOUT,56)I,X(I)
   56     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
C               ********************************
C               **  COMPUTE DISTI2CT VALUES.  **
C               ********************************
C
      NY=0
      IF(NX.LT.1)GOTO150
      DO100I=1,NX
      IF(I.EQ.1)GOTO130
      DO120J=1,NY
      IF(X(I).EQ.Y(J))GOTO100
  120 CONTINUE
  130 CONTINUE
      NY=NY+1
      Y(NY)=X(I)
  100 CONTINUE
      GOTO190
C
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
  157 FORMAT('      THE INPUT NUMBER OF OBSERVATIONS HERE = ',I8,
     1'.')
      CALL DPWRST('XXX','BUG ')
C
  190 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DISTI2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR,NX,NY
 9012   FORMAT('IERROR,NX,NY = ',A4,2X,2I8)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,MAX(NX,NY)
          WRITE(ICOUT,9016)I,X(I),Y(I)
 9016     FORMAT('I,X(I),Y(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DIWCDF(X,Q,BETA,CDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
C              FUNCTION VALUE FOR THE DISCRETE WEIBULL
C              DISTRIBUTION WITH SHAPE PARAMETERS Q AND BETA.
C              THIS DISTRIBUTION IS DEFINED FOR ALL INTEGER X >= 0.
C              THE CUMULATIVE DISTRIBUTION FUNCTION IS:
C                  F(X;Q,BETA) = 1 - (Q)**((X+1)**BETA)
C                  X = 0, 1, 2, ...;  0 < Q < 1;  BETA > 0
C     INPUT  ARGUMENTS--X      = THE DOUBLE PRECISION VALUE AT
C                                WHICH THE CUMULATIVE DISTRIBUTION
C                                FUNCTION IS TO BE EVALUATED.
C                                X SHOULD BE A NON-NEGATIVE INTEGER.
C                     --Q      = THE DOUBLE PRECISION VALUE OF THE
C                                FIRST SHAPE PARAMETER
C                     --BETA   = THE DOUBLE PRECISION VALUE OF THE
C                                SECOND SHAPE PARAMETER
C     OUTPUT ARGUMENTS--CDF    = THE DOUBLE PRECISION CUMULATIVE
C                                DISTRIBUTION FUNCTION VALUE.
C     OUTPUT--THE DOUBLE PRECISION CUMULATIVE DISTRIBUTION FUNCTION
C             VALUE CDF FOR THE DISCRETE WEIBULL DISTRIBUTION WITH
C             SHAPE PARAMETERS Q AND BETA
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--X SHOULD BE A NON-NEGATIVE INTEGER
C                 --0 < Q < 1; BETA > 0
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--JOHNSON, KEMP, AND KOTZ (2005), "UNIVARIATE DISCRETE
C                 DISTRIBUTIONS", THIRD EDITION, WILEY, PP. 510-511.
C               --NAKAGAWA AND OSAKI (1975), "THE DISCRETE WEIBULL
C                 DISTRIBUTION", IEEE TRANSACTIONS ON RELIABILITY,
C                 R-24, PP. 300-301.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2006/11
C     ORIGINAL VERSION--NOVEMBER  2006.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION X
      DOUBLE PRECISION Q
      DOUBLE PRECISION BETA
      DOUBLE PRECISION CDF
      DOUBLE PRECISION DTERM1
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IX=INT(X+0.5D0)
      IF(IX.LT.0)THEN
        WRITE(ICOUT,4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        CDF=0.0D0
        GOTO9000
      ENDIF
    4 FORMAT('***** ERROR--THE FIRST ARGUMENT TO DIWCDF IS LESS ',
     1'THAN 0')
C
      IF(Q.LE.0.0D0 .OR. Q.GE.1.0D0)THEN
        WRITE(ICOUT,15)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)Q
        CALL DPWRST('XXX','BUG ')
        CDF=0.0D0
        GOTO9000
      ENDIF
   15 FORMAT('***** ERROR--THE SECOND ARGUMENT TO DIWCDF IS NOT IN ',
     1'THE INTERVAL (0,1)')
C
      IF(BETA.LE.0.0D0)THEN
        WRITE(ICOUT,25)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        CDF=0.0
        GOTO9000
      ENDIF
   25 FORMAT('***** ERROR--THE THIRD ARGUMENT TO DIWCDF IS NEGATIVE')
C
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
C
      DTERM1=((X+1.0D0)**BETA)*DLOG(Q)
      CDF=1.0D0 - DEXP(DTERM1)
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE DIWHAZ(X,Q,BETA,HAZ)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE HAZARD
C              FUNCTION VALUE FOR THE DISCRETE WEIBULL
C              DISTRIBUTION WITH SHAPE PARAMETERS Q AND BETA.
C              THIS DISTRIBUTION IS DEFINED FOR ALL INTEGER X >= 0.
C              THE HAZARD FUNCTION IS:
C                  h(X;Q,BETA) = 1 - (Q)**(X+1)**BETA/(Q)**(X**BETA)
C                  X = 0, 1, 2, ...;  0 < Q < 1;  BETA > 0
C     INPUT  ARGUMENTS--X      = THE DOUBLE PRECISION VALUE AT
C                                WHICH THE PROBABILITYU MASS
C                                FUNCTION IS TO BE EVALUATED.
C                                X SHOULD BE A NON-NEGATIVE INTEGER.
C                     --Q      = THE DOUBLE PRECISION VALUE OF THE
C                                FIRST SHAPE PARAMETER
C                     --BETA   = THE DOUBLE PRECISION VALUE OF THE
C                                SECOND SHAPE PARAMETER
C     OUTPUT ARGUMENTS--HAZ    = THE DOUBLE PRECISION HAZARD
C                                FUNCTION VALUE.
C     OUTPUT--THE DOUBLE PRECISION HAZARD FUNCTION
C             VALUE HAZ FOR THE DISCRETE WEIBULL DISTRIBUTION WITH
C             SHAPE PARAMETERS Q AND BETA
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--X SHOULD BE A NON-NEGATIVE INTEGER
C                 --0 < Q < 1; BETA > 0
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--JOHNSON, KEMP, AND KOTZ (2005), "UNIVARIATE DISCRETE
C                 DISTRIBUTIONS", THIRD EDITION, WILEY, PP. 515-516.
C               --NAKAGAWA AND OSAKI (1975), "THE DISCRETE WEIBULL
C                 DISTRIBUTION", IEEE TRANSACTIONS ON RELIABILITY,
C                 R-24, PP. 300-301.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2006/11
C     ORIGINAL VERSION--NOVEMBER  2006.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION X
      DOUBLE PRECISION Q
      DOUBLE PRECISION BETA
      DOUBLE PRECISION HAZ
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IX=INT(X+0.5D0)
      IF(IX.LT.0)THEN
        WRITE(ICOUT,4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        HAZ=0.0D0
        GOTO9000
      ENDIF
    4 FORMAT('***** ERROR--THE FIRST ARGUMENT TO DIWHAZ IS LESS ',
     1'THAN 0')
C
      IF(Q.LE.0.0D0 .OR. Q.GE.1.0D0)THEN
        WRITE(ICOUT,15)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)Q
        CALL DPWRST('XXX','BUG ')
        HAZ=0.0D0
        GOTO9000
      ENDIF
   15 FORMAT('***** ERROR--THE SECOND ARGUMENT TO DIWHAZ IS NOT IN ',
     1'THE INTERVAL (0,1)')
C
      IF(BETA.LE.0.0D0)THEN
        WRITE(ICOUT,25)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        HAZ=0.0
        GOTO9000
      ENDIF
   25 FORMAT('***** ERROR--THE THIRD ARGUMENT TO DIWHAZ IS NEGATIVE')
C
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
C
      DTERM1=((X+1.0D0)**BETA)*DLOG(Q)
      DTERM2=(X**BETA)*DLOG(Q)
      HAZ=1.0D0 - DEXP(DTERM1 - DTERM2)
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE DIWPDF(X,Q,BETA,PDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY MASS
C              FUNCTION VALUE FOR THE DISCRETE WEIBULL
C              DISTRIBUTION WITH SHAPE PARAMETERS Q AND BETA.
C              THIS DISTRIBUTION IS DEFINED FOR ALL INTEGER X >= 0.
C              THE PROBABILITY MASS FUNCTION IS:
C                  p(X;Q,BETA) = (Q)**(X**BETA) - (Q)**((X+1)**BETA)
C                  X = 0, 1, 2, ...;  0 < Q < 1;  BETA > 0
C     INPUT  ARGUMENTS--X      = THE DOUBLE PRECISION VALUE AT
C                                WHICH THE PROBABILITYU MASS
C                                FUNCTION IS TO BE EVALUATED.
C                                X SHOULD BE A NON-NEGATIVE INTEGER.
C                     --Q      = THE DOUBLE PRECISION VALUE OF THE
C                                FIRST SHAPE PARAMETER
C                     --BETA   = THE DOUBLE PRECISION VALUE OF THE
C                                SECOND SHAPE PARAMETER
C     OUTPUT ARGUMENTS--PDF    = THE DOUBLE PRECISION PROBABILITY MASS
C                                FUNCTION VALUE.
C     OUTPUT--THE DOUBLE PRECISION PROBABILITY MASS FUNCTION
C             VALUE PDF FOR THE DISCRETE WEIBULL DISTRIBUTION WITH
C             SHAPE PARAMETERS Q AND BETA
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--X SHOULD BE A NON-NEGATIVE INTEGER
C                 --0 < Q < 1; BETA > 0
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--JOHNSON, KEMP, AND KOTZ (2005), "UNIVARIATE DISCRETE
C                 DISTRIBUTIONS", THIRD EDITION, WILEY, PP. 510-511.
C               --NAKAGAWA AND OSAKI (1975), "THE DISCRETE WEIBULL
C                 DISTRIBUTION", IEEE TRANSACTIONS ON RELIABILITY,
C                 R-24, PP. 300-301.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2006/11
C     ORIGINAL VERSION--NOVEMBER  2006.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION X
      DOUBLE PRECISION Q
      DOUBLE PRECISION BETA
      DOUBLE PRECISION PDF
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DTERM2
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IX=INT(X+0.5D0)
      IF(IX.LT.0)THEN
        WRITE(ICOUT,4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        PDF=0.0D0
        GOTO9000
      ENDIF
    4 FORMAT('***** ERROR--THE FIRST ARGUMENT TO DIWPDF IS LESS ',
     1'THAN 0')
C
      IF(Q.LE.0.0D0 .OR. Q.GE.1.0D0)THEN
        WRITE(ICOUT,15)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)Q
        CALL DPWRST('XXX','BUG ')
        PDF=0.0D0
        GOTO9000
      ENDIF
   15 FORMAT('***** ERROR--THE SECOND ARGUMENT TO DIWPDF IS NOT IN ',
     1'THE INTERVAL (0,1)')
C
      IF(BETA.LE.0.0D0)THEN
        WRITE(ICOUT,25)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GOTO9000
      ENDIF
   25 FORMAT('***** ERROR--THE THIRD ARGUMENT TO DIWPDF IS NEGATIVE')
C
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
C
      DTERM1=(X**BETA)*DLOG(Q)
      DTERM2=((X+1)**BETA)*DLOG(Q)
      PDF=DEXP(DTERM1) - DEXP(DTERM2)
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE DIWPPF(P,Q,BETA,PPF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
C              FUNCTION VALUE FOR THE DISCRETE WEIBULL
C              DISTRIBUTION WITH SHAPE PARAMETERS Q AND BETA.
C              THIS DISTRIBUTION IS DEFINED FOR ALL INTEGER X >= 0.
C              THE PERCENT POINT FUNCTION IS:
C                  G(P;Q,BETA) = {LOG(1-P)/LOG(Q)]**(1/BETA)  0 <= P < 1
C     INPUT  ARGUMENTS--P      = THE DOUBLE PRECISION VALUE AT
C                                WHICH THE PERCENT POINT
C                                FUNCTION IS TO BE EVALUATED.
C                                P SHOULD BE IN THE INTERVAL (0,1]
C                     --Q      = THE DOUBLE PRECISION VALUE OF THE
C                                FIRST SHAPE PARAMETER
C                     --BETA   = THE DOUBLE PRECISION VALUE OF THE
C                                SECOND SHAPE PARAMETER
C     OUTPUT ARGUMENTS--PPF    = THE DOUBLE PRECISION PERCENT POINT
C                                FUNCTION VALUE.
C     OUTPUT--THE DOUBLE PRECISION PERCENT POINT FUNCTION
C             VALUE PPF FOR THE DISCRETE WEIBULL DISTRIBUTION WITH
C             SHAPE PARAMETERS Q AND BETA
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--0 <= P < 1; 0 < Q < 1; BETA > 0
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--JOHNSON, KEMP, AND KOTZ (2005), "UNIVARIATE DISCRETE
C                 DISTRIBUTIONS", THIRD EDITION, WILEY, PP. 510-511.
C               --NAKAGAWA AND OSAKI (1975), "THE DISCRETE WEIBULL
C                 DISTRIBUTION", IEEE TRANSACTIONS ON RELIABILITY,
C                 R-24, PP. 300-301.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2006/11
C     ORIGINAL VERSION--NOVEMBER  2006.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION P
      DOUBLE PRECISION Q
      DOUBLE PRECISION BETA
      DOUBLE PRECISION PPF
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DEPS
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
      DATA DEPS/0.1D-15/
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(P.LT.0.0D0 .OR. P.GE.1.0)THEN
        WRITE(ICOUT,4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
        CALL DPWRST('XXX','BUG ')
        PPF=0.0D0
        GOTO9000
      ENDIF
    4 FORMAT('***** ERROR--THE FIRST ARGUMENT TO DIWPPF IS OUTSIDE ',
     1'THE ALLOWABLE (0,1] INTERVAL')
C
      IF(Q.LE.0.0D0 .OR. Q.GE.1.0D0)THEN
        WRITE(ICOUT,15)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)Q
        CALL DPWRST('XXX','BUG ')
        PPF=0.0D0
        GOTO9000
      ENDIF
   15 FORMAT('***** ERROR--THE SECOND ARGUMENT TO DIWPPF IS NOT IN ',
     1'THE INTERVAL (0,1)')
C
      IF(BETA.LE.0.0D0)THEN
        WRITE(ICOUT,25)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        PPF=0.0
        GOTO9000
      ENDIF
   25 FORMAT('***** ERROR--THE THIRD ARGUMENT TO DIWPPF IS NEGATIVE')
C
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
C
      DTERM1=(DLOG(1.0D0 - P)/DLOG(Q))**(1.0D0/BETA)
      IPPF=INT(DTERM1+DEPS)
      PPF=DBLE(IPPF)
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE DIWRAN(N,Q,BETA,ISEED,X)
C
C     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
C              FROM THE DISCRETE WEIBULL DISTRIBUTION
C              WITH SHAPE PARAMETERS Q AND BETA.
C              THIS DISTRIBUTION IS DEFINED FOR ALL
C              NON-NEGATIVE INTEGER X >= 0 AND HAS
C              THE PROBABILITY MASS FUNCTION IS:
C                  p(X;Q,BETA) = (Q)**(X**BETA) - (Q)**((X+1)**BETA)
C                  X = 0, 1, 2, ...;  0 < Q < 1;  BETA > 0
C     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
C                                OF RANDOM NUMBERS TO BE
C                                GENERATED.
C                     --Q      = THE SINGLE PRECISION VALUE
C                                OF THE FIRST SHAPE PARAMETER.
C                     --BETA   = THE SINGLE PRECISION VALUE
C                                OF THE SECOND SHAPE PARAMETER.
C     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
C                                (OF DIMENSION AT LEAST N)
C                                INTO WHICH THE GENERATED
C                                RANDOM SAMPLE WILL BE PLACED.
C     OUTPUT--A RANDOM SAMPLE OF SIZE N
C             FROM THE DISCRETE WEIBULL DISTRIBUTION
C             WITH SHAPE PARAMETERS Q AND BETA.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C                 --0 < Q < 1, BETA > 0
C     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN, DIWPPF
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--JOHNSON, KEMP, AND KOTZ (2005), "UNIVARIATE DISCRETE
C                 DISTRIBUTIONS", THIRD EDITION, WILEY, PP. 510-511.
C               --NAKAGAWA AND OSAKI (1975), "THE DISCRETE WEIBULL
C                 DISTRIBUTION", IEEE TRANSACTIONS ON RELIABILITY,
C                 R-24, PP. 300-301.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
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
C---------------------------------------------------------------------
C
      REAL Q
      REAL BETA
      DIMENSION X(*)
C
      DOUBLE PRECISION DQ
      DOUBLE PRECISION DBETA
      DOUBLE PRECISION DPPF
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----DATA STATEMENTS-------------------------------------------------
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(N.LT.1)THEN
        WRITE(ICOUT,5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
        CALL DPWRST('XXX','BUG ')
        GOTO9999
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
        GOTO9999
      ENDIF
   11 FORMAT('***** ERROR--THE Q PARAMETER FOR THE ',
     1'DISCRETE WEIBULL')
   12 FORMAT('      RANDOM NUMBERS IS OUTSIDE THE (0,1) INTERVAL')
C
      IF(BETA.LE.0.0)THEN
        WRITE(ICOUT,21)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,22)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
   21 FORMAT('***** ERROR--THE BETA PARAMETER FOR THE ',
     1'DISCRETE WEIBULL')
   22 FORMAT('      RANDOM NUMBERS IS NON-POSITIVE.')
C
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7)
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
C
C     GENERATE N DISCRETE WEIBULL DISTRIBUTION
C     RANDOM NUMBERS.
C
      DQ=DBLE(Q)
      DBETA=DBLE(BETA)
      CALL UNIRAN(N,ISEED,X)
C
      DO100I=1,N
        ZTEMP=X(I)
        CALL DIWPPF(DBLE(ZTEMP),DQ,DBETA,DPPF)
        X(I)=REAL(DPPF)
  100 CONTINUE
C
 9999 CONTINUE
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION DLBETA (A, B)
C***BEGIN PROLOGUE  DLBETA
C***PURPOSE  Compute the natural logarithm of the complete Beta
C            function.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7B
C***TYPE      DOUBLE PRECISION (ALBETA-S, DLBETA-D, CLBETA-C)
C***KEYWORDS  FNLIB, LOGARITHM OF THE COMPLETE BETA FUNCTION,
C             SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C DLBETA(A,B) calculates the double precision natural logarithm of
C the complete beta function for double precision arguments
C A and B.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D9LGMC, DGAMMA, DLNGAM, DLNREL, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900727  Added EXTERNAL statement.  (WRB)
C***END PROLOGUE  DLBETA
      DOUBLE PRECISION A, B, P, Q, CORR, SQ2PIL, D9LGMC, DGAMMA, DLNGAM,
     1  DLNREL
      EXTERNAL DGAMMA
      SAVE SQ2PIL
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
      DATA SQ2PIL / 0.9189385332 0467274178 0329736405 62 D0 /
C***FIRST EXECUTABLE STATEMENT  DLBETA
      P = MIN (A, B)
      Q = MAX (A, B)
C
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
C
      IF (P.GE.10.D0) GO TO 30
      IF (Q.GE.10.D0) GO TO 20
C
C P AND Q ARE SMALL.
C
      DLBETA = LOG (DGAMMA(P) * (DGAMMA(Q)/DGAMMA(P+Q)) )
      RETURN
C
C P IS SMALL, BUT Q IS BIG.
C
 20   CORR = D9LGMC(Q) - D9LGMC(P+Q)
      DLBETA = DLNGAM(P) + CORR + P - P*LOG(P+Q)
     1  + (Q-0.5D0)*DLNREL(-P/(P+Q))
      RETURN
C
C P AND Q ARE BIG.
C
 30   CORR = D9LGMC(P) + D9LGMC(Q) - D9LGMC(P+Q)
      DLBETA = -0.5D0*LOG(Q) + SQ2PIL + CORR + (P-0.5D0)*LOG(P/(P+Q))
     1  + Q*DLNREL(-P/(P+Q))
      RETURN
C
      END
C===================================================== DLGAMA.FOR
      DOUBLE PRECISION FUNCTION DLGADP(X)
C
C     2020/03: RENAME TO AVOID CONFLICT WITH INTRINSIC ROUTINE
C
CCCCC DOUBLE PRECISION FUNCTION DLGAMA(X)
C***********************************************************************
C*                                                                     *
C*  FORTRAN CODE WRITTEN FOR INCLUSION IN IBM RESEARCH REPORT RC20525, *
C*  'FORTRAN ROUTINES FOR USE WITH THE METHOD OF L-MOMENTS, VERSION 3' *
C*                                                                     *
C*  J. R. M. HOSKING                                                   *
C*  IBM RESEARCH DIVISION                                              *
C*  T. J. WATSON RESEARCH CENTER                                       *
C*  YORKTOWN HEIGHTS                                                   *
C*  NEW YORK 10598, U.S.A.                                             *
C*                                                                     *
C*  VERSION 3     AUGUST 1996                                          *
C*                                                                     *
C***********************************************************************
C
C  LOGARITHM OF GAMMA FUNCTION
C
C  BASED ON ALGORITHM ACM291, COMMUN. ASSOC. COMPUT. MACH. (1966)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'DPCOP2.INC'
C
      DATA SMALL,CRIT,BIG,TOOBIG/1D-7,13D0,1D9,2D36/
C
C         C0 IS 0.5*LOG(2*PI)
C         C1...C7 ARE THE COEFFTS OF THE ASYMPTOTIC EXPANSION OF DLGAMA
C
      DATA C0,C1,C2,C3,C4,C5,C6,C7/
     *   0.91893 85332 04672 742D 0,  0.83333 33333 33333 333D-1,
     *  -0.27777 77777 77777 778D-2,  0.79365 07936 50793 651D-3,
     *  -0.59523 80952 38095 238D-3,  0.84175 08417 50841 751D-3,
     *  -0.19175 26917 52691 753D-2,  0.64102 56410 25641 026D-2/
C
C         S1 IS -(EULER'S CONSTANT), S2 IS PI**2/12
C
      DATA S1/-0.57721 56649 01532 861D 0/
      DATA S2/ 0.82246 70334 24113 218D 0/
C
      DATA ZERO/0D0/,HALF/0.5D0/,ONE/1D0/,TWO/2D0/
C
CCCCC DLGAMA=ZERO
      DLGADP=ZERO
C
      IF(X.LE.ZERO .OR. X.GT.TOOBIG)THEN
        WRITE(ICOUT,7000)
 7000   FORMAT('****** ERROR IN DLGAMA: ARGUMENT OUT OF RANGE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7002)X
 7002   FORMAT('       VALUE OF THE ARGUMENT IS ',D24.16)
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
C         USE SMALL-X APPROXIMATION IF X IS NEAR 0, 1 OR 2
C
      IF(DABS(X-TWO).GT.SMALL)GOTO 10
CCCCC DLGAMA=DLOG(X-ONE)
      DLGADP=DLOG(X-ONE)
      XX=X-TWO
      GOTO 20
   10 IF(DABS(X-ONE).GT.SMALL)GOTO 30
      XX=X-ONE
CCC20 DLGAMA=DLGAMA+XX*(S1+XX*S2)
   20 DLGADP=DLGADP+XX*(S1+XX*S2)
      GOTO9000
   30 IF(X.GT.SMALL)GOTO 40
CCCCC DLGAMA=-DLOG(X)+S1*X
      DLGADP=-DLOG(X)+S1*X
      GOTO9000
C
C         REDUCE TO DLGAMA(X+N) WHERE X+N.GE.CRIT
C
   40 SUM1=ZERO
      Y=X
      IF(Y.GE.CRIT)GOTO 60
      Z=ONE
   50 Z=Z*Y
      Y=Y+ONE
      IF(Y.LT.CRIT)GOTO 50
      SUM1=SUM1-DLOG(Z)
C
C         USE ASYMPTOTIC EXPANSION IF Y.GE.CRIT
C
   60 SUM1=SUM1+(Y-HALF)*DLOG(Y)-Y+C0
      SUM2=ZERO
      IF(Y.GE.BIG)GOTO 70
      Z=ONE/(Y*Y)
      SUM2=((((((C7*Z+C6)*Z+C5)*Z+C4)*Z+C3)*Z+C2)*Z+C1)/Y
CCC70 DLGAMA=SUM1+SUM2
   70 DLGADP=SUM1+SUM2
      GOTO9000
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE DLGAMS (X, DLGAM, SGNGAM)
C***BEGIN PROLOGUE  DLGAMS
C***PURPOSE  Compute the logarithm of the absolute value of the Gamma
C            function.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7A
C***TYPE      DOUBLE PRECISION (ALGAMS-S, DLGAMS-D)
C***KEYWORDS  ABSOLUTE VALUE OF THE LOGARITHM OF THE GAMMA FUNCTION,
C             FNLIB, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C DLGAMS(X,DLGAM,SGNGAM) calculates the double precision natural
C logarithm of the absolute value of the Gamma function for
C double precision argument X and stores the result in double
C precision argument DLGAM.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  DLNGAM
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C***END PROLOGUE  DLGAMS
      DOUBLE PRECISION X, DLGAM, SGNGAM, DLNGAM
C***FIRST EXECUTABLE STATEMENT  DLGAMS
      DLGAM = DLNGAM(X)
      SGNGAM = 1.0D0
      IF (X.GT.0.D0) RETURN
C
      INTZ = INT(MOD (-AINT(X), 2.0D0) + 0.1D0)
      IF (INTZ.EQ.0) SGNGAM = -1.0D0
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION DLNGAM (X)
C***BEGIN PROLOGUE  DLNGAM
C***PURPOSE  Compute the logarithm of the absolute value of the Gamma
C            function.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7A
C***TYPE      DOUBLE PRECISION (ALNGAM-S, DLNGAM-D, CLNGAM-C)
C***KEYWORDS  ABSOLUTE VALUE, COMPLETE GAMMA FUNCTION, FNLIB, LOGARITHM,
C             SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C DLNGAM(X) calculates the double precision logarithm of the
C absolute value of the Gamma function for double precision
C argument X.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, D9LGMC, DGAMMA, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900727  Added EXTERNAL statement.  (WRB)
C***END PROLOGUE  DLNGAM
      DOUBLE PRECISION X, DXREL, PI, SINPIY, SQPI2L, SQ2PIL, XMAX,
     1  Y, DGAMMA, D9LGMC, TEMP
      LOGICAL FIRST
      EXTERNAL DGAMMA
      SAVE SQ2PIL, SQPI2L, PI, XMAX, DXREL, FIRST
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
      DATA SQ2PIL / 0.9189385332 0467274178 0329736405 62 D0 /
      DATA SQPI2L / +.2257913526 4472743236 3097614947 441 D+0    /
      DATA PI / 3.1415926535 8979323846 2643383279 50 D0 /
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  DLNGAM
C
      DLNGAM = 0.0D0
C
      IF (FIRST) THEN
         TEMP = 1.D0/LOG(D1MACH(2))
         XMAX = TEMP*D1MACH(2)
         DXREL = SQRT(D1MACH(4))
      ENDIF
      FIRST = .FALSE.
C
      Y = ABS (X)
      IF (Y.GT.10.D0) GO TO 20
C
C LOG (ABS (DGAMMA(X)) ) FOR ABS(X) .LE. 10.0
C
      DLNGAM = LOG (ABS (DGAMMA(X)) )
      RETURN
C
C LOG ( ABS (DGAMMA(X)) ) FOR ABS(X) .GT. 10.0
C
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
C
      IF (X.GT.0.D0) DLNGAM = SQ2PIL + (X-0.5D0)*LOG(X) - X + D9LGMC(Y)
      IF (X.GT.0.D0) RETURN
C
      SINPIY = ABS (SIN(PI*Y))
      IF (SINPIY .EQ. 0.D0) THEN
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        DLNGAM = 0.D0
        RETURN
      ENDIF
   31 FORMAT('***** ERROR FROM DLNGAM.  X IS A NEGATIVE INTEGER. ')
C
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
C
      DLNGAM = SQPI2L + (X-0.5D0)*LOG(Y) - X - LOG(SINPIY) - D9LGMC(Y)
      RETURN
C
      END
      SUBROUTINE DLGCDF(X,THETA,CDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
C              FUNCTION VALUE FOR THE DISCRETE LOGARITHMIC SERIES
C              DISTRIBUTION WITH SHAPE PARAMETER = THETA.
C              THIS DISTRIBUTION IS DEFINED FOR ALL INTEGER X>1.
C              THE PROBABILITY DENSITY FUNCTION IS:
C              F(X,THETA)=A*THETA**X/X      X=1,2,3,...
C              WHERE A = 1/LN(1-THETA), 0<THETA<1
C              FOR CDF, USE RECURRENCE RELATION:
C                P(X=x+1) = THETA*P(X=x)/(X+1)     X=1,2,...
C              WHERE
C                P(X=1)=-THETA/LN(1-THETA)
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE CUMULATIVE DISTRIBUTION
C                                FUNCTION IS TO BE EVALUATED.
C                                X SHOULD BE NON-NEGATIVE.
C                     --THETA    = THE SHAPE PARAMETER
C     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE
C                                DISTRIBUTION FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
C             FUNCTION VALUE CDF FOR THE LOGARITHMIC SERIES
C             DISTRIBUTION WITH SHAPE PARAMETER = THETA
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--X SHOULD BE A POSITIVE INTEGER
C                 --0 < THETA < 1
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--JOHNSON AND KOTZ, DISCRETE UNIVARIATE
C                 DISTRIBUTIONS--1, 1994, CHAPTER 7
C               --"STATISTICAL DISTRIBUTIONS", EVANS, HASTINGS, 
C                 PEACOCK.  WILEY, 1993.  CHAPTER 23.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1966)
C     VERSION NUMBER--95/4
C     ORIGINAL VERSION--APRIL     1995.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DTERM1, DTERM2, DTERM3
      DOUBLE PRECISION DX, DTHETA, DLTHET, DSUM
      DOUBLE PRECISION DCURR, DPREV
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----DATA STATEMENTS-------------------------------------------------
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IX=INT(X+0.5)
      CDF=0.0
      IF(THETA.LE.0.0.OR.THETA.GE.1.0)THEN
        WRITE(ICOUT,15)
   15   FORMAT('***** ERROR--THE SECOND ARGUMENT TO DLGCDF ',
     1        'DLGCDF IS NOT IN THE INTERVAL (0,1).')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)THETA
        CALL DPWRST('XXX','BUG ')
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7,'.')
        GOTO9000
      ELSEIF(IX.LT.1)THEN
        WRITE(ICOUT,4)
    4   FORMAT('***** WARNING--THE FIRST ARGUMENT TO DLGCDF ',
     1         'IS LESS THAN 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
      DX=DBLE(IX)
      DTHETA=DBLE(THETA)
      DSUM=0.0D0
C
      DTERM1=-DTHETA/DLOG(1.0D0-DTHETA)
      IF(IX.EQ.1)THEN
        CDF=REAL(DTERM1)
        GOTO9000
      ENDIF
C
      DSUM=DTERM1
      DPREV=DTERM1
      DLTHET=DLOG(DTHETA)
      DO100I=2,IX
C
        IF(DPREV.LE.D1MACH(1))THEN
          CDF=REAL(DSUM)
          GOTO9000
        ENDIF
C
        DTERM3=DBLE(I)
        DTERM2=DLTHET + DLOG(DTERM3-1.0D0) + DLOG(DPREV) - DLOG(DTERM3)
        DCURR=DEXP(DTERM2)
        DSUM=DSUM+DCURR
        DPREV=DCURR
 100  CONTINUE
C
      CDF=REAL(DSUM)
C
 9000 CONTINUE
      RETURN
      END
      REAL FUNCTION DLGFU2(X)
C
C     PURPOSE--DPMLDL CALLS FZERO TO FIND A ROOT FOR THE EQUATION
C                 XBAR = THETAHAT/[-(1-THETAHAT)LN(1-THETAHAT)
C              DLGFU2 IS THE FUNCTION FOR WHICH THE ZERO IS FOUND.
C              IT IS:
C                 XBAR - THETAHAT/[-(1-THETAHAT)LN(1-THETAHAT) = 0
C              WHERE THETAHAT IS THE DESIRED VALUE (I.E., X)
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE EQUATION IS EVALUATED.
C     OUTPUT--THE SINGLE PRECISION FUNCTION VALUE DLGFU2.
C     PRINTING--NONE.
C     RESTRICTIONS--NONE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--JOHNSON, KOTZ, AND KEMP, "DISCRETE
C                 UNIVARIATE DISTRIBUTIONS", SECOND EDITION,
C                 JOHN WILEY, 1992, CHAPTER 7.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATION INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATION INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2004.3
C     ORIGINAL VERSION--MARCH     2003.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      REAL XBAR
      COMMON/DLGCOM/XBAR
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      DLGFU2=XBAR - X/(-(1.0-X)*LOG(1.0-X))
C
      RETURN
      END
      SUBROUTINE DLGPDF(X,THETA,PDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
C              FUNCTION VALUE FOR THE DISCRETE LOGARITHMIC SERIES
C              DISTRIBUTION WITH SHAPE PARAMETER = THETA.
C              THIS DISTRIBUTION IS DEFINED FOR ALL INTEGER X>1.
C              THE PROBABILITY DENSITY FUNCTION IS:
C              F(X,THETA)=A*THETA**X/X      X=1,2,3,...
C              WHERE A = 1/LN(1-THETA), 0<THETA<1
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE CUMULATIVE DISTRIBUTION
C                                FUNCTION IS TO BE EVALUATED.
C                                X SHOULD BE NON-NEGATIVE.
C                     --THETA    = THE SHAPE PARAMETER
C     OUTPUT ARGUMENTS--PDF    = THE SINGLE PRECISION CUMULATIVE
C                                DISTRIBUTION FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY
C             FUNCTION VALUE PDF FOR THE LOGARITHMIC SERIES
C             DISTRIBUTION WITH SHAPE PARAMETER = THETA
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--X SHOULD BE A POSITIVE INTEGER
C                 --0 < THETA < 1
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--JOHNSON AND KOTZ, DISCRETE UNIVARIATE
C                 DISTRIBUTIONS--1, 1994, CHAPTER 7
C               --"STATISTICAL DISTRIBUTIONS", EVANS, HASTINGS, 
C                 PEACOCK.  WILEY, 1993.  CHAPTER 23.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1966)
C     VERSION NUMBER--95/4
C     ORIGINAL VERSION--APRIL     1995.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DTERM1, DTERM2, DTERM3, DTERM4, DTERM5
      DOUBLE PRECISION DX, DTHETA, DCONST
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IX=INT(X+0.5)
      PDF=0.0
      IF(THETA.LE.0.0 .OR. THETA.GE.1.0)THEN
        WRITE(ICOUT,15)
   15   FORMAT('***** ERROR--THE SECOND ARGUMENT TO DLGPDF ',
     1         'IS NOT IN THE INTERVAL (0,1).')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)THETA
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(IX.LT.1)THEN
        WRITE(ICOUT,4)
    4   FORMAT('***** WARNING--THE FIRST ARGUMENT TO DLGPDF ',
     1         'IS LESS THAN 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)X
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
      DX=DBLE(IX)
      DTHETA=DBLE(THETA)
C
      DCONST=-1.0D0/DLOG(1.0D0-DTHETA)
      DTERM1=DLOG(DCONST)
C
      DTERM2=DX*DLOG(DTHETA)
      DTERM3=DLOG(DX)
      DTERM4=DTERM1+DTERM2-DTERM3
      DTERM5=DEXP(DTERM4)
      PDF=REAL(DTERM5)
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE DLGPPF(P,THETA,PPF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
C              FUNCTION VALUE AT THE SINGLE PRECISION VALUE P
C              FOR THE LOGARITMIC SERIES DISTRIBUTION
C     INPUT  ARGUMENTS--P      = THE SINGLE PRECISION VALUE
C                                AT WHICH THE PERCENT POINT
C                                FUNCTION IS TO BE EVALUATED.
C                                IT SHOULD BE IN THE INTERVAL (0,1).
C                     --THETA  = THE SHAPE PARAMETER
C     OUTPUT ARGUMENTS--CDF    = THE SINGLE PRECISION CUMULATIVE
C                                DISTRIBUTION FUNCTION VALUE.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--P SHOULD BE BETWEEN 0 AND 1 (EXCLUSIVELY FOR 1).
C                 --THETA SHOULD BE IN THE INTERVAL (0,1) (EXCLUSIVELY)
C                 --NN SHOULD BE A POSITIVE INTEGER BETWEEN 1 AND MM.
C     OUTPUT ARGUMENTS--PPF    = THE SINGLE PRECISION PERCENT
C                                POINT FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PERCENT POINT  .
C             FUNCTION VALUE PPF
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     OTHER DATAPAC   SUBROUTINES NEEDED--DLGCDF.
C     MODE OF INTERNAL OPERATIONS--SINGLE AND DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     COMMENT--NOTE THAT EVEN THOUGH THE OUTPUT
C              FROM THIS DISCRETE DISTRIBUTION
C              PERCENT POINT FUNCTION
C              SUBROUTINE MUST NECESSARILY BE A
C              DISCRETE INTEGER VALUE,
C              THE OUTPUT VARIABLE PPF IS SINGLE
C              PRECISION IN MODE.
C              PPF HAS BEEN SPECIFIED AS SINGLE
C              PRECISION SO AS TO CONFORM WITH THE DATAPAC
C              CONVENTION THAT ALL OUTPUT VARIABLES FROM ALL
C              DATAPAC SUBROUTINES ARE SINGLE PRECISION.
C              THIS CONVENTION IS BASED ON THE BELIEF THAT
C              1) A MIXTURE OF MODES (FLOATING POINT
C              VERSUS INTEGER) IS INCONSISTENT AND
C              AN UNNECESSARY COMPLICATION
C              IN A DATA ANALYSIS; AND
C              2) FLOATING POINT MACHINE ARITHMETIC
C              (AS OPPOSED TO INTEGER ARITHMETIC)
C              IS THE MORE NATURAL MODE FOR DOING
C              DATA ANALYSIS.
C     REFERENCES--JOHNSON AND KOTZ, DISCRETE
C                 DISTRIBUTIONS, 1994.  CHAPTER 7.
C               --EVANS, HASTINGS, PEACOCK, STATISTICAL
C                 DISTRIBUTIONS--1993, CHAPTER 23.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1966)
C     VERSION NUMBER--95/4
C     ORIGINAL VERSION--APRIL     1995.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      PPF=0.0
      IF(P.LT.0.0.OR.P.GE.1.0)THEN
        WRITE(ICOUT,1)
    1   FORMAT('***** ERROR--THE FIRST ARGUMENT TO DLGPPF ',
     1         'IS OUTSIDE THE ALLOWABLE (0,1) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)P
   46   FORMAT('***** THE VALUE OF THE ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(THETA.LE.0.0.OR.THETA.GE.1.0)THEN
        WRITE(ICOUT,11)
   11   FORMAT('***** ERROR--THE SECOND ARGUMENT TO DLGPPF (THE ',
     1         'SHAPE PARAMETER) IS OUTSIDE THE (0,1) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)THETA
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
      PPF=1.0
      IX0=1
      IX1=1
      IX2=1
      P0=0.0
      P1=0.0
      P2=0.0
C
C     TREAT CERTAIN SPECIAL CASES IMMEDIATELY--
C     1) P = 0.0
C
      IF(P.EQ.0.0)THEN
        PPF=1.0
        RETURN
      ENDIF
C
C     DETERMINE AN INITIAL APPROXIMATION TO THE LOGARITHMIC SERIES
C     PERCENT POINT.  USE MEAN VALUE = -THETA/[(1-THETA)LOG(1-THETA)]
C
      X2=-THETA/((1.0-THETA)*LOG(1.0-THETA))
      IX2=INT(X2+0.5)
      IF(IX2.LT.5)IX2=5
C
C     DETERMINE UPPER AND LOWER BOUNDS ON THE DESIRED
C     PERCENT POINT BY ITERATING OUT (BOTH BELOW AND ABOVE)
C     FROM THE ORIGINAL APPROXIMATION AT STEPS
C     OF 1 STANDARD DEVIATION.
C     THE RESULTING BOUNDS WILL BE AT MOST
C     1 STANDARD DEVIATION APART.
C
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
C
      IF(P2.LT.P)GOTO210
      GOTO250
C
  210 CONTINUE
      IX0=IX2
      IF(IX0.LT.1)IX0=1
      I=1
  215 CONTINUE
      IX2=IX0+ISD
      IF(IX2.LT.1)IX2=1
      IF(IX2.GE.IX1)GOTO275
      CALL DLGCDF(REAL(IX2),THETA,P2)
      IF(P2.GE.P)GOTO230
      IX0=IX2
CC220 CONTINUE
      I=I+1
      IF(I.LE.1000000)GOTO215
      WRITE(ICOUT,249)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,222)
      CALL DPWRST('XXX','BUG ')
      GOTO950
  230 IX1=IX2
      GOTO275
C
  250 CONTINUE
      IX1=IX2
      I=1
  255 CONTINUE
      IX2=IX1-ISD
      IF(IX2.LT.1)IX2=1
      IF(IX2.LE.IX0)GOTO275
      CALL DLGCDF(REAL(IX2),THETA,P2)
      IF(P2.LT.P)GOTO270
      IX1=IX2
CC260 CONTINUE
      I=I+1
      IF(I.LE.1000000)GOTO255
      WRITE(ICOUT,249)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,262)
      CALL DPWRST('XXX','BUG ')
      GOTO950
  270 IX0=IX2
C
  275 IF(IX0.EQ.IX1)GOTO280
      GOTO295
  280 IF(IX0.EQ.0)GOTO285
CCCCC IF(IX0.EQ.N)GOTO290
      WRITE(ICOUT,249)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,282)
      CALL DPWRST('XXX','BUG ')
      GOTO950
  285 IX1=IX1+1
      GOTO295
CC290 IX0=IX0-1
CCCCC IF(IX0.LT.1)IX0=1
  295 CONTINUE
C
C     COMPUTE HYPERGEOMETRIC PROBABILITIES FOR THE
C     DERIVED LOWER AND UPPER BOUNDS.
C
      CALL DLGCDF(REAL(IX0),THETA,P0)
      CALL DLGCDF(REAL(IX1),THETA,P1)
C
C     CHECK THE PROBABILITIES FOR PROPER ORDERING
C
      IF(P0.LT.P.AND.P.LE.P1)GOTO490
      IF(P0.EQ.P)GOTO410
      IF(P1.EQ.P)GOTO420
      IF(P0.GT.P1)GOTO430
      IF(P0.GT.P)GOTO440
      IF(P1.LT.P)GOTO450
      WRITE(ICOUT,249)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,401)
      CALL DPWRST('XXX','BUG ')
      GOTO950
  410 PPF=IX0
      RETURN
  420 PPF=IX1
      RETURN
  430 WRITE(ICOUT,249)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,431)
      CALL DPWRST('XXX','BUG ')
      GOTO950
  440 CONTINUE
CCCCC WRITE(ICOUT,249)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,441)
CCCCC CALL DPWRST('XXX','BUG ')
      PPF=1.0
      RETURN
CCCCC GOTO950
  450 WRITE(ICOUT,249)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,451)
      CALL DPWRST('XXX','BUG ')
      GOTO950
  490 CONTINUE
C
C     THE STOPPING CRITERION IS THAT THE LOWER BOUND
C     AND UPPER BOUND ARE EXACTLY 1 UNIT APART.
C     CHECK TO SEE IF IX1 = IX0 + 1;
C     IF SO, THE ITERATIONS ARE COMPLETE;
C     IF NOT, THEN BISECT, COMPUTE PROBABILIIES,
C     CHECK PROBABILITIES, AND CONTINUE ITERATING
C     UNTIL IX1 = IX0 + 1.
C
  300 IX0P1=IX0+1
      IF(IX1.EQ.IX0P1)GOTO690
      IX2=(IX0+IX1)/2
      IF(IX2.LT.1)IX2=1
      IF(IX2.EQ.IX0)GOTO610
      IF(IX2.EQ.IX1)GOTO620
      CALL DLGCDF(REAL(IX2),THETA,P2)
      IF(P0.LT.P2.AND.P2.LT.P1)GOTO630
      IF(P2.LE.P0)GOTO640
      IF(P2.GE.P1)GOTO650
  610 WRITE(ICOUT,249)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,611)
      CALL DPWRST('XXX','BUG ')
      GOTO950
  620 WRITE(ICOUT,249)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,611)
      CALL DPWRST('XXX','BUG ')
      GOTO950
  630 IF(P2.LE.P)GOTO635
      IX1=IX2
      P1=P2
      GOTO300
  635 IX0=IX2
      P0=P2
      GOTO300
  640 WRITE(ICOUT,249)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,641)
      CALL DPWRST('XXX','BUG ')
      GOTO950
  650 WRITE(ICOUT,249)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,651)
      CALL DPWRST('XXX','BUG ')
      GOTO950
  690 PPF=IX1
      IF(P0.EQ.P)PPF=IX0
      RETURN
C
  950 WRITE(ICOUT,240)IX0,P0
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,241)IX1,P1
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,242)IX2,P2
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,244)P
      CALL DPWRST('XXX','BUG ')
C
  222 FORMAT('NO UPPER BOUND FOUND AFTER 10**7 ITERATIONS')
  240 FORMAT('IX0  = ',I8,10X,'P0 = ',F14.7)
  241 FORMAT('IX1  = ',I8,10X,'P1 = ',F14.7)
  242 FORMAT('IX2  = ',I8,10X,'P2 = ',F14.7)
  244 FORMAT('P    = ',F14.7)
  249 FORMAT('***** INTERNAL ERROR IN DLGPPF SUBROUTINE.')
  262 FORMAT('NO LOWER BOUND FOUND AFTER 10**7 ITERATIONS')
  282 FORMAT('LOWER AND UPPER BOUND IDENTICAL')
  401 FORMAT('IMPOSSIBLE BRANCH CONDITION ENCOUNTERED')
  431 FORMAT('LOWER BOUND PROBABILITY (P0) GREATER THAN ',
     1       'UPPER BOUND PROBABILITY (P1)')
CC441 FORMAT('LOWER BOUND PROBABILITY (P0) GREATER THAN ',
CCCCC1       'INPUT PROBABILITY (P)')
  451 FORMAT('UPPER BOUND PROBABILITY (P1) LESS    THAN ',
     1       'INPUT PROBABILITY (P)')
  611 FORMAT('BISECTION VALUE (X2) = LOWER BOUND (X0)')
CC621 FORMAT('BISECTION VALUE (X2) = UPPER BOUND (X1)')
  641 FORMAT('BISECTION VALUE PROBABILITY (P2) ',
     1       'LESS THAN LOWER BOUND PROBABILITY (P0)')
  651 FORMAT('BISECTION VALUE PROBABILITY (P2) ',
     1       'GREATER THAN UPPER BOUND PROBABILITY (P1)')
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE DLGRAN(N,THETA,ISEED,X)
C
C     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
C              FROM THE LOGARITHMIC SERIES DISTRIBUTION
C              WITH SINGLE PRECISION 'BERNOULLI PROBABILITY'
C              PARAMETER = THETA.
C              THE LOGARITHMIC SERIES DISTRIBUTION HAS THE
C              PROBABILITY FUNCTION
C              F(X) = [-1/(LOG(1-THETA)]*THETA**X/X
C              THIS DISTRIBUTION IS DEFINED FOR
C              ALL POSITIVE INTEGERS X--X = 1, 2, ... .
C     ALGORITHM--METHOD OF KEMP AS DESCRIBED ON PAGE 548 OF
C                "NON-UNIFORM RANDOM VARIATE GENERATION",
C                LUC DEVROYE, SPRINGER-VERLAG, 1986.
C     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
C                                OF RANDOM NUMBERS TO BE
C                                GENERATED.
C                     --THETA  = THE SINGLE PRECISION VALUE
C                                OF THE SHAPE PARAMETER FOR THE
C                                LOGARITHMIC SERIES DISTRIBUTION.
C                                P SHOULD BE BETWEEN
C                                0.0 AND 1.0 (EXCLUSIVELY).
C     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
C                                (OF DIMENSION AT LEAST N)
C                                INTO WHICH THE GENERATED
C                                RANDOM SAMPLE WILL BE PLACED.
C     OUTPUT--A RANDOM SAMPLE OF SIZE N
C             FROM THE LOGARITHMIC SERIES DISTRIBUTION
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C                 --THETA SHOULD BE BETWEEN 0.0 (EXCLUSIVELY)
C                   AND 1.0 (EXCLUSIVELY).
C     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--LOG.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--LUC DEVROYE, "NIN-UNIFORM RANDOM VARIATE
C                 GENERATION", SPRINGER-VERLAG, 1986.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2002/8
C     ORIGINAL VERSION--AUGUST    2002.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION XTEMP(1)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(N.LT.1)THEN
        WRITE(ICOUT,5)
    5   FORMAT('***** ERROR--THE FIRST ARGUMENT TO DLGRAN IS ',
     1         'NON-POSITIVE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
   47   FORMAT('      THE VALUE OF THE ARGUMENT IS ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ELSEIF(THETA.LE.0.0.OR.THETA.GE.1.0)THEN
        WRITE(ICOUT,11)
   11   FORMAT('***** ERROR--THE SECOND ARGUMENT TO DLGRAN IS ',
     1         'OUTSIDE THE ALLOWABLE (0,1) INTERVAL.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)THETA
   46   FORMAT('      THE VALUE OF THE ARGUMENT IS ',G15.7,'.')
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
C
C     GENERATE N UNIFORM (0,1) RANDOM NUMBERS
C
      CALL UNIRAN(N,ISEED,X)
C
C     GENERATE N LOGARITHMIC SERIES RANDOM NUMBERS
C     USING THE KEMP ALGORITHM.
C
      NTEMP=1
      AR=LOG(1-THETA)
      DO100I=1,N
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
C
 9999 CONTINUE
      RETURN
      END
      DOUBLE PRECISION FUNCTION DLNREL (X)
C***BEGIN PROLOGUE  DLNREL
C***PURPOSE  Evaluate ln(1+X) accurate in the sense of relative error.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C4B
C***TYPE      DOUBLE PRECISION (ALNREL-S, DLNREL-D, CLNREL-C)
C***KEYWORDS  ELEMENTARY FUNCTIONS, FNLIB, LOGARITHM
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C DLNREL(X) calculates the double precision natural logarithm of
C (1.0+X) for double precision argument X.  This routine should
C be used when X is small and accurate to calculate the logarithm
C accurately (in the relative error sense) in the neighborhood
C of 1.0.
C
C Series for ALNR       on the interval -3.75000E-01 to  3.75000E-01
C                                        with weighted error   6.35E-32
C                                         log weighted error  31.20
C                               significant figures required  30.93
C                                    decimal places required  32.01
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, DCSEVL, INITDS, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C***END PROLOGUE  DLNREL
      DOUBLE PRECISION ALNRCS(43), X, XMIN,  DCSEVL
      LOGICAL FIRST
      SAVE ALNRCS, NLNREL, XMIN, FIRST
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
C
      DATA ALNRCS(  1) / +.1037869356 2743769800 6862677190 98 D+1     /
      DATA ALNRCS(  2) / -.1336430150 4908918098 7660415531 33 D+0     /
      DATA ALNRCS(  3) / +.1940824913 5520563357 9261993747 50 D-1     /
      DATA ALNRCS(  4) / -.3010755112 7535777690 3765377765 92 D-2     /
      DATA ALNRCS(  5) / +.4869461479 7154850090 4563665091 37 D-3     /
      DATA ALNRCS(  6) / -.8105488189 3175356066 8099430086 22 D-4     /
      DATA ALNRCS(  7) / +.1377884779 9559524782 9382514960 59 D-4     /
      DATA ALNRCS(  8) / -.2380221089 4358970251 3699929149 35 D-5     /
      DATA ALNRCS(  9) / +.4164041621 3865183476 3918599019 89 D-6     /
      DATA ALNRCS( 10) / -.7359582837 8075994984 2668370319 98 D-7     /
      DATA ALNRCS( 11) / +.1311761187 6241674949 1522943450 11 D-7     /
      DATA ALNRCS( 12) / -.2354670931 7742425136 6960923301 75 D-8     /
      DATA ALNRCS( 13) / +.4252277327 6034997775 6380529625 67 D-9     /
      DATA ALNRCS( 14) / -.7719089413 4840796826 1081074933 00 D-10    /
      DATA ALNRCS( 15) / +.1407574648 1359069909 2153564721 91 D-10    /
      DATA ALNRCS( 16) / -.2576907205 8024680627 5370786275 84 D-11    /
      DATA ALNRCS( 17) / +.4734240666 6294421849 1543950059 38 D-12    /
      DATA ALNRCS( 18) / -.8724901267 4742641745 3012632926 75 D-13    /
      DATA ALNRCS( 19) / +.1612461490 2740551465 7398331191 15 D-13    /
      DATA ALNRCS( 20) / -.2987565201 5665773006 7107924168 15 D-14    /
      DATA ALNRCS( 21) / +.5548070120 9082887983 0413216972 79 D-15    /
      DATA ALNRCS( 22) / -.1032461915 8271569595 1413339619 32 D-15    /
      DATA ALNRCS( 23) / +.1925023920 3049851177 8785032448 68 D-16    /
      DATA ALNRCS( 24) / -.3595507346 5265150011 1897078442 66 D-17    /
      DATA ALNRCS( 25) / +.6726454253 7876857892 1945742267 73 D-18    /
      DATA ALNRCS( 26) / -.1260262416 8735219252 0824256375 46 D-18    /
      DATA ALNRCS( 27) / +.2364488440 8606210044 9161589555 19 D-19    /
      DATA ALNRCS( 28) / -.4441937705 0807936898 8783891797 33 D-20    /
      DATA ALNRCS( 29) / +.8354659446 4034259016 2412939946 66 D-21    /
      DATA ALNRCS( 30) / -.1573155941 6479562574 8992535210 66 D-21    /
      DATA ALNRCS( 31) / +.2965312874 0247422686 1543697066 66 D-22    /
      DATA ALNRCS( 32) / -.5594958348 1815947292 1560132266 66 D-23    /
      DATA ALNRCS( 33) / +.1056635426 8835681048 1872841386 66 D-23    /
      DATA ALNRCS( 34) / -.1997248368 0670204548 3149994666 66 D-24    /
      DATA ALNRCS( 35) / +.3778297781 8839361421 0498559999 99 D-25    /
      DATA ALNRCS( 36) / -.7153158688 9081740345 0381653333 33 D-26    /
      DATA ALNRCS( 37) / +.1355248846 3674213646 5020245333 33 D-26    /
      DATA ALNRCS( 38) / -.2569467304 8487567430 0798293333 33 D-27    /
      DATA ALNRCS( 39) / +.4874775606 6216949076 4595199999 99 D-28    /
      DATA ALNRCS( 40) / -.9254211253 0849715321 1323733333 33 D-29    /
      DATA ALNRCS( 41) / +.1757859784 1760239233 2697600000 00 D-29    /
      DATA ALNRCS( 42) / -.3341002667 7731010351 3770666666 66 D-30    /
      DATA ALNRCS( 43) / +.6353393618 0236187354 1802666666 66 D-31    /
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  DLNREL
C
      DLNREL = 0.0
C
      IF (FIRST) THEN
         NLNREL = INITDS (ALNRCS, 43, 0.1*REAL(D1MACH(3)))
         XMIN = -1.0D0 + SQRT(D1MACH(4))
      ENDIF
      FIRST = .FALSE.
C
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
C
      IF (ABS(X).LE.0.375D0) DLNREL = X*(1.D0 -
     1  X*DCSEVL (X/.375D0, ALNRCS, NLNREL))
C
      IF (ABS(X).GT.0.375D0) DLNREL = LOG (1.0D0+X)
C
      RETURN
      END

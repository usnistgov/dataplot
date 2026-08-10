! ODRPACK ROUTINES:
!
!   1. ODR    - DRIVER ROUTINE FOR FINDING THE WEIGHTED EXPLICIT OR
!               IMPLICIT ORTHOGONAL DISTANCE REGRESSION (ODR) OR
!               ORDINARY LINEAR OR NONLINEAR LEAST SQUARES (OLS)
!               SOLUTION (SHORT CALL STATEMENT)
!   2. DACCES - ACCESS OR STORE VALUES IN THE WORK SPACE
!   3. DESUBI - COMPUTE E = WD + ALPHA*TT**2
!   4. DETAF  - COMPUTE NOISE AND NUMBER OF GOOD DIGITS IN FUNCTION
!               RESULTS (ADAPTED FROM STARPAC SUBROUTINE ETAFUN)
!   5. DEVJAC - COMPUTE THE WEIGHTED JACOBIANS WRT BETA AND DELTA
!   6. DWGHT  - SCALE MATRIX T USING WT, I.E., COMPUTE WTT = WT*T
!   7. DFCTR  - FACTOR THE POSITIVE (SEMI)DEFINITE MATRIX A USING A
!               MODIFIED CHOLESKY FACTORIZATION
!               (ADAPTED FROM LINPACK SUBROUTINE DPOFA)
!   8. DFCTRW - CHECK INPUT PARAMETERS, INDICATING ERRORS FOUND USING
!               NONZERO VALUES OF ARGUMENT INFO AS DESCRIBED IN THE
!               ODRPACK REFERENCE GUIDE
!   9. DFLAGS - SET FLAGS INDICATING CONDITIONS SPECIFIED BY JOB
!  10. DIFIX  - SET ELEMENTS OF T TO ZERO ACCORDING TO IFIX
!  11. DINIWK - INITIALIZE WORK VECTORS AS NECESSARY
!  12. DIWINF - SET STORAGE LOCATIONS WITHIN INTEGER WORK SPACE
!  13. DJACCD - COMPUTE CENTRAL DIFFERENCE APPROXIMATIONS TO THE
!               JACOBIAN WRT THE ESTIMATED BETAS AND WRT THE DELTAS
!  14. MBFB   - ENSURE RANGE OF BOUNDS IS LARGE ENOUGH FOR DERIVATIVE
!               CHECKING.  MOVE BETA AWAY FROM BOUNDS SO THAT
!               DERIVATIVES CAN BE CALCULATED.
!  15. DJACFD - COMPUTE FORWARD DIFFERENCE APPROXIMATIONS TO THE
!               JACOBIAN WRT THE ESTIMATED BETAS AND WRT THE DELTAS
!  16. DJCK   - DRIVER ROUTINE FOR THE DERIVATIVE CHECKING PROCESS
!               (ADAPTED FROM STARPAC SUBROUTINE DCKCNT)
!  17. DJCKC  - CHECK WHETHER HIGH CURVATURE COULD BE THE CAUSE OF THE
!               DISAGREEMENT BETWEEN THE NUMERICAL AND ANALYTIC DERVIATIVES
!               (ADAPTED FROM STARPAC SUBROUTINE DCKCRV)
!  18. DJCKF  - CHECK WHETHER FINITE PRECISION ARITHMETIC COULD BE THE
!               CAUSE OF THE DISAGREEMENT BETWEEN THE DERIVATIVES
!               (ADAPTED FROM STARPAC SUBROUTINE DCKFPA)
!  19. DJCKM  - CHECK USER SUPPLIED ANALYTIC DERIVATIVES AGAINST NUMERICAL
!               DERIVATIVES (ADAPTED FROM STARPAC SUBROUTINE DCKMN)
!  20. DJCKZ  - RECHECK THE DERIVATIVES IN THE CASE WHERE THE FINITE
!               DIFFERENCE DERIVATIVE DISAGREES WITH THE ANALYTIC
!               DERIVATIVE AND THE ANALYTIC DERIVATIVE IS ZERO
!               (ADAPTED FROM STARPAC SUBROUTINE DCKZRO)
!  21. DODCHK - CHECK INPUT PARAMETERS, INDICATING ERRORS FOUND USING
!               NONZERO VALUES OF ARGUMENT INFO
!  22. DODCNT - DOUBLE PRECISION DRIVER ROUTINE FOR FINDING
!               THE WEIGHTED EXPLICIT OR IMPLICIT ORTHOGONAL DISTANCE
!               REGRESSION (ODR) OR ORDINARY LINEAR OR NONLINEAR LEAST
!               SQUARES (OLS) SOLUTION
!  23. DODDRV - PERFORM ERROR CHECKING AND INITIALIZATION, AND BEGIN
!               PROCEDURE FOR PERFORMING ORTHOGONAL DISTANCE REGRESSION
!               (ODR) OR ORDINARY LINEAR OR NONLINEAR LEAST SQUARES (OLS)
!  24. DWGHT  - SCALE MATRIX T USING WT, I.E., COMPUTE WTT = WT*T
!  25. DODLM  - COMPUTE LEVENBERG-MARQUARDT PARAMETER AND STEPS S AND T
!               USING ANALOG OF THE TRUST-REGION LEVENBERG-MARQUARDT
!               ALGORITHM
!  26. DODMN  - ITERATIVELY COMPUTE LEAST SQUARES SOLUTION
!  27. DODPC1 - GENERATE INITIAL SUMMARY REPORT
!  28. DODPC2 - GENERATE ITERATION REPORTS
!  29. DODPC3 - GENERATE FINAL SUMMARY REPORT
!  30. DODPCR - GENERATE COMPUTATION REPORTS
!  31. DODPE1 - PRINT ERROR REPORTS
!  32. DODPE2 - GENERATE THE DERIVATIVE CHECKING REPORT
!  33. DODPE3 - PRINT ERROR REPORTS INDICATING THAT COMPUTATIONS WERE
!               STOPPED IN USER SUPPLIED SUBROUTINES FCN
!  34. DODPER - CONTROLLING ROUTINE FOR PRINTING ERROR REPORTS
!  35. DODPHD - PRINT ODRPACK HEADING
!  36. DODSTP - COMPUTE LOCALLY CONSTRAINED STEPS S AND T, AND PHI(ALPHA)
!  37. DODVCV - COMPUTE COVARIANCE MATRIX OF ESTIMATED PARAMETERS
!  38. DPACK  - SELECT THE UNFIXED ELEMENTS OF V2 AND RETURN THEM IN V1
!  39. DPVB   - COMPUTE THE NROW-TH FUNCTION VALUE USING BETA(J) + STP
!  40. DPVD   - COMPUTE NROW-TH FUNCTION VALUE USING
!               X(NROW,J) + DELTA(NROW,J) + STP
!  41. DSCALE - SCALE T BY THE INVERSE OF SCL, I.E., COMPUTE T/SCL
!  42. DSCLB  - SELECT SCALING VALUES FOR BETA ACCORDING TO THE
!               ALGORITHM GIVEN IN THE ODRPACK REFERENCE GUIDE
!  43. DSCLD  - SELECT SCALING VALUES FOR DELTA ACCORDING TO THE
!               ALGORITHM GIVEN IN THE ODRPACK REFERENCE GUIDE
!  44. DSETN  - SELECT THE ROW AT WHICH THE DERIVATIVE WILL BE CHECKED
!  45. DSOLVE - SOLVE SYSTEMS OF THE FORM
!                   T * X = B  OR  TRANS(T) * X = B
!               WHERE T IS AN UPPER OR LOWER TRIANGULAR MATRIX OF ORDER N,
!               AND THE SOLUTION X OVERWRITES THE RHS B.
!               (ADAPTED FROM LINPACK SUBROUTINE DTRSL)
!  46. DUNPAC - COPY THE ELEMENTS OF V1 INTO THE LOCATIONS OF V2 WHICH
!               ARE UNFIXED
!  47. DVEVTR - COMPUTE  V*E*TRANS(V) FOR THE (INDX)TH M BY NQ ARRAY IN V
!  48. DXMY   - COMPUTE XMY = X - Y
!  49. DXPY   - COMPUTE XPY = X + Y
!  50. DZERO  - SET A = ZERO
!  51. DPPNML - COMPUTE THE PERCENT POINT FUNCTION VALUE FOR THE
!               NORMAL (GAUSSIAN) DISTRIBUTION WITH MEAN 0 AND STANDARD
!               DEVIATION 1, AND WITH PROBABILITY DENSITY FUNCTION
!               F(X) = (1/SQRT(2*PI))*EXP(-X*X/2).
!               (ADAPTED FROM DATAPAC SUBROUTINE TPPF, WITH MODIFICATIONS
!               TO FACILITATE CONVERSION TO DOUBLE PRECISION AUTOMATICALLY)
!               REPLACE THIS WITH DATAPLOT NODPPF SUBROUTINE
!  52. DPPT   - COMPUTE THE PERCENT POINT FUNCTION VALUE FOR THE
!               STUDENT'S T DISTRIBUTION WITH IDF DEGREES OF FREEDOM.
!               (ADAPTED FROM DATAPAC SUBROUTINE TPPF, WITH MODIFICATIONS
!               TO FACILITATE CONVERSION TO DOUBLE PRECISION AUTOMATICALLY)
!               NOTE: REPLACE THIS WITH DATAPLOT TPPF SUBROUTINE.
!  53. DHSTEP - SET RELATIVE STEP SIZE FOR FINITE DIFFERENCE DERIVATIVES
!  54. DERSTEP- Compute step size for center and forward difference
!               calculations
!
!  THESE ROUTINES HAVE BEEN MODIFIED SLIGHTLY FOR INTEGRATION WITH
!  DATAPLOT:
!
!     1. I/O GOES THROUGH DATAPLOT "DPWRST" SUBROUTINE
!     2. USE DATAPLOT INTERNAL STORAGE RATHER THAN ALLOCATABLE
!        ARRAYS
!
! From HOMPACK90.
      MODULE REAL_PRECISION
!       This is for 64-bit arithmetic.
        INTEGER, PARAMETER:: R8=SELECTED_REAL_KIND(13)
      END MODULE REAL_PRECISION
!ODRPACK95
      MODULE ODRPACK95
!***Begin Prologue  ODRPACK95
!***Refer to  ODR
!***Date Written  20040524 (YYYYMMDD)
!***Revision Date N/A
!***Purpose: Define the interface to the ODR subroutine
!***End Prologue ODRPACK95

      USE REAL_PRECISION

!   A temporary work array for holding return values before copying to a lower
!   rank array.
      REAL (KIND=R8), ALLOCATABLE :: TEMPRET(:,:)

      CONTAINS
!ODR
      SUBROUTINE ODR(FCN,N,M,NP,NQ,BETA,Y,X,DELTA,WE,WD,IFIXB,IFIXX,   &
                     JOB,NDIGIT,TAUFAC,SSTOL,PARTOL,MAXIT,             &
                     IPRINT,LUNERR,LUNRPT,STPB,STPD,SCLB,SCLD,         &
                     WORK,IWORK,INFO,LOWER,UPPER)
!***Begin Prologue  ODR
!***Date Written   860529   (YYMMDD)
!***Revision Date  20040301 (YYYYMMDD)
!***Category No.  G2E,I1B1
!***Keywords  Orthogonal distance regression,
!             Nonlinear least squares,
!             Measurement error models,
!             Errors in variables
!***Author  Boggs, Paul T.
!             Applied and Computational Mathematics Division
!             National Institute of Standards and Technology
!             Gaithersburg, MD 20899
!           Byrd, Richard H.
!             Department of Computer Science
!             University of Colorado, Boulder, CO 80309
!           Rogers, Janet E.
!             Applied and Computational Mathematics Division
!             National Institute of Standards and Technology
!             Boulder, CO 80303-3328
!           Schnabel, Robert B.
!             Department of Computer Science
!             University of Colorado, Boulder, CO 80309
!             and
!             Applied and Computational Mathematics Division
!             National Institute of Standards and Technology
!             Boulder, CO 80303-3328
!***Purpose  REAL (KIND=R8) driver routine for finding 
!            the weighted explicit or implicit orthogonal distance  
!            regression (ODR) or ordinary linear or nonlinear least  
!            squares (OLS) solution (long call statement)
!***Description
!   For details, see ODRPACK95 User's Reference Guide.
!***References  Boggs, P. T., R. H. Byrd, J. R. Donaldson, and
!                 R. B. Schnabel (1989),
!                 "Algorithm 676 --- ODRPACK: Software for Weighted
!                 Orthogonal Distance Regression,"
!                 ACM Trans. Math. Software., 15(4):348-364.
!               Boggs, P. T., R. H. Byrd, J. E. Rogers, and
!                 R. B. Schnabel (1992),
!                 "User's Reference Guide for ODRPACK Version 2.01,
!                 Software for Weighted Orthogonal Distance Regression,"
!                 National Institute of Standards and Technology
!                 Internal Report Number 92-4834.
!               Boggs, P. T., R. H. Byrd, and R. B. Schnabel (1987),
!                 "A Stable and Efficient Algorithm for Nonlinear
!                 Orthogonal Distance Regression,"
!                 SIAM J. Sci. Stat. Comput., 8(6):1052-1078.
!***Routines Called  DODCNT
!***End Prologue  ODR

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      REAL (KIND=R8) PARTOL,SSTOL,TAUFAC
      INTEGER INFO,IPRINT,JOB,LUNERR,LUNRPT,M,MAXIT,N,NDIGIT,NP,NQ

!...Array arguments
      REAL (KIND=R8) BETA(:),DELTA(:,:),LOWER(:),SCLB(:),SCLD(:,:), &
           STPB(:),STPD(:,:),UPPER(:),WD(:,:,:),WE(:,:,:),          &
           WORK(:),X(:,:),Y(:,:)
      INTEGER IFIXB(:),IFIXX(:,:),IWORK(:)

!...Subroutine arguments
      EXTERNAL FCN

!...Optional arguments
      OPTIONAL                                                         &
         DELTA,IFIXB,IFIXX,INFO,IPRINT,IWORK,JOB,LOWER,LUNERR,         &
         LUNRPT,MAXIT,NDIGIT,PARTOL,SCLB,SCLD,SSTOL,STPB,              &
         STPD,TAUFAC,UPPER,WE,WD,WORK

!...Pointers
      POINTER DELTA,IWORK,WORK

!...Local scalars
      REAL (KIND=R8) NEGONE,ZERO,LTAUFAC,LSSTOL,LPARTOL
      INTEGER LDWE,LD2WE,LDWD,LD2WD,LDIFX,LDSCLD,LDSTPD,               &
              LJOB,LNDIGIT,LMAXIT,LIPRINT,LLUNERR,LLUNRPT,LINFO,       &
              LENWORK,LENIWORK,LINFO1,LINFO2,LINFO3,LINFO4,LINFO5
      LOGICAL HEAD

!...Local arrays
      REAL (KIND=R8)                                                   &
           LDELTA(:,:),LLOWER(NP),LWE(N,NQ,NQ),LWD(N,M,M),             &
           LSTPB(NP),LSTPD(N,M),LSCLB(NP),                             &
           LSCLD(N,M),LUPPER(NP),LWORK(:),WD1(1,1,1)
      INTEGER LIFIXB(NP),LIWORK(:),LIFIXX(N,M)

!...Pointer
      POINTER LDELTA,LIWORK,LWORK

!...Saved variables
      SAVE LDELTA,LIWORK,LWORK

!...External subroutines
      EXTERNAL DODCNT

!...Data statements
      DATA NEGONE,ZERO /-1.0E0_R8,0.0E0_R8/

!...Routine names used as subprogram arguments
!   FCN:     The user-supplied subroutine for evaluating the model.

!...Variable definitions (alphabetically)
!   BETA:    The function parameters.
!   DELTA:   The initial error in the X data
!   IFIXB:   The values designating whether the elements of BETA are 
!            fixed at their input values or not.
!   IFIXX:   The values designating whether the elements of X are 
!            fixed at their input values or not.
!   INFO:    The variable designating why the computations were stopped.
!   IPRINT:  The print control variable.
!   IWORK:   The integer work space.
!   JOB:     The variable controlling problem initialization and 
!            computational method.
!   LOWER:   The lower bound on BETA.
!   LUNERR:  The logical unit number for error messages.
!   LUNRPT:  The logical unit number for computation reports.
!   M:       The number of columns of data in the explanatory variable.
!   MAXIT:   The maximum number of iterations allowed.
!   N:       The number of observations.
!   NDIGIT:  The number of accurate digits in the function results, as
!            supplied by the user.
!   NP:      The number of function parameters.
!   NQ:      The number of responses per observation.
!   PARTOL:  The parameter convergence stopping tolerance.
!   SCLB:    The scaling values for BETA.
!   SCLD:    The scaling values for DELTA.
!   STPB:    The relative step for computing finite difference
!            derivatives with respect to BETA.
!   STPD:    The relative step for computing finite difference
!            derivatives with respect to DELTA.
!   SSTOL:   The sum-of-squares convergence stopping tolerance.
!   TAUFAC:  The factor used to compute the initial trust region 
!            diameter.
!   UPPER:   The upper bound on BETA.
!   WD:      The DELTA weights.
!   WD1:     A dummy array used when WD(1,1,1)=0.0E0_R8.
!   WE:      The EPSILON weights.
!   WORK:    The REAL (KIND=R8) work space.
!   X:       The explanatory variable.
!   Y:       The dependent variable.  Unused when the model is implicit.


!***First executable statement  ODR


!  Set LINFO to zero indicating no errors have been found thus far

      LINFO  = 0
      LINFO1 = 0
      LINFO2 = 0
      LINFO3 = 0
      LINFO4 = 0
      LINFO5 = 0

!  Set all scalar variable defaults except JOB

      LDWE         = 1
      LD2WE        = 1
      LDWD         = 1
      LD2WD        = 1
      LDIFX        = 1
      LDSCLD       = 1
      LDSTPD       = 1
      LIPRINT      = -1
      LLUNERR      = -1
      LLUNRPT      = -1
      LMAXIT       = -1
      LNDIGIT      = -1
      LPARTOL      = NEGONE
      LSSTOL       = NEGONE
      LTAUFAC      = NEGONE
      HEAD         = .TRUE.

!  Check for the option arguments for printing (so error messages can be 
!  printed appropriately from here on out

      IF (PRESENT(IPRINT)) THEN
         LIPRINT = IPRINT
      END IF

      IF (PRESENT(LUNRPT)) THEN
         LLUNRPT = LUNRPT
      END IF
      IF (LLUNRPT.LT.0) THEN
         LLUNRPT = 6
      END IF

      IF (PRESENT(LUNERR)) THEN
         LLUNERR = LUNERR
      END IF
      IF (LLUNERR.LT.0) THEN
         LLUNERR = 6
      END IF

!  Ensure the problem size is valid

      IF (N.LE.0) THEN
         LINFO5 = 1
         LINFO4 = 1
      END IF

      IF (M.LE.0) THEN
         LINFO5 = 1
         LINFO3 = 1
      END IF

      IF (NP.LE.0) THEN
         LINFO5 = 1
         LINFO2 = 1
      END IF

      IF (NQ.LE.0) THEN
         LINFO5 = 1
         LINFO1 = 1
      END IF

      IF (LINFO5.NE.0) THEN
         LINFO = 10000*LINFO5+1000*LINFO4+100*LINFO3+10*LINFO2+LINFO1
         IF (LLUNERR.GT.0.AND.LIPRINT.NE.0) THEN
            CALL DODPHD(HEAD,LLUNRPT)
            CALL DODPE1(                                               &
                 LLUNERR,LINFO,LINFO5,LINFO4,LINFO3,LINFO2,LINFO1,     &
                 N,M,NQ,                                               &
                 LDSCLD,LDSTPD,LDWE,LD2WE,LDWD,LD2WD,                  &
                 LENWORK,LENIWORK)
         END IF
         IF (PRESENT(INFO)) THEN
            INFO = LINFO
         END IF
         RETURN
      END IF

!  Define LJOB and check that necessary arguments are passed for JOB

      IF (PRESENT(JOB)) THEN
         LJOB = JOB
         IF (MOD(JOB,10000)/1000.GE.1) THEN
            IF (.NOT.PRESENT(DELTA)) THEN
               LINFO5 = 7
               LINFO4 = 1
            ELSE IF (.NOT.ASSOCIATED(DELTA)) THEN
               LINFO5 = 7
               LINFO4 = 1
            END IF
         END IF
         IF (JOB.GE.10000) THEN
            IF (.NOT.PRESENT(IWORK)) THEN
               LINFO5 = 7
               LINFO2 = 1
            ELSE IF (.NOT.ASSOCIATED(IWORK)) THEN
               LINFO5 = 7
               LINFO2 = 1
            END IF
         END IF
         IF (JOB.GE.10000) THEN
            IF (.NOT.PRESENT(WORK)) THEN
               LINFO5 = 7
               LINFO3 = 1
            ELSE IF (.NOT.ASSOCIATED(WORK)) THEN
               LINFO5 = 7
               LINFO3 = 1
            END IF
         END IF
      ELSE
         LJOB = -1
      END IF

      IF (LINFO5.NE.0) THEN
         LINFO = 10000*LINFO5+1000*LINFO4+100*LINFO3+10*LINFO2+LINFO1
         IF (LLUNERR.GT.0.AND.LIPRINT.NE.0) THEN
            CALL DODPHD(HEAD,LLUNRPT)
            CALL DODPE1(                                               &
                 LLUNERR,LINFO,LINFO5,LINFO4,LINFO3,LINFO2,LINFO1,     &
                 N,M,NQ,                                               &
                 LDSCLD,LDSTPD,LDWE,LD2WE,LDWD,LD2WD,                  &
                 LENWORK,LENIWORK)
         END IF
         IF (PRESENT(INFO)) THEN
            INFO = LINFO
         END IF
         RETURN
      END IF

!  Determine the size of WORK

      IF (LJOB.LT.0.OR.MOD(LJOB,10).LE.1) THEN
         LENWORK = 18+13*NP+NP**2+M+M**2+4*N*NQ+6*N*M+2*N*NQ*NP+       &
            2*N*NQ*M+NQ**2+5*NQ+NQ*(NP+M)+N*NQ*NQ
      ELSE
         LENWORK = 18+13*NP+NP**2+M+M**2+4*N*NQ+2*N*M+2*N*NQ*NP+       &
            5*NQ+NQ*(NP+M)+N*NQ*NQ
      END IF

!  Determine the size of IWORK

      LENIWORK = 20+2*NP+NQ*(NP+M)

!  Allocate the work arrays

      ALLOCATE(LWORK(LENWORK),TEMPRET(MAX(N,NP),MAX(NQ,M)),STAT=LINFO3)
      ALLOCATE(LIWORK(LENIWORK),STAT=LINFO2)
      LWORK(:) = 0.0_R8
      LIWORK(:) = 0
      IF (PRESENT(DELTA)) THEN
         IF (.NOT.ASSOCIATED(DELTA)) THEN
            ALLOCATE(LDELTA(N,M),STAT=LINFO4)
         END IF
      END IF
      IF (LINFO4.NE.0.OR.LINFO3.NE.0.OR.LINFO2.NE.0) THEN
          LINFO5 = 8
      END IF

      IF (LINFO5.NE.0) THEN
         LINFO = 10000*MOD(LINFO5,10)+1000*MOD(LINFO4,10)+             &
            100*MOD(LINFO3,10)+10*MOD(LINFO2,10)+MOD(LINFO1,10)
         IF (LLUNERR.GT.0.AND.LIPRINT.NE.0) THEN
            CALL DODPHD(HEAD,LLUNRPT)
            CALL DODPE1(                                               &
                 LLUNERR,LINFO,LINFO5,LINFO4,LINFO3,LINFO2,LINFO1,     &
                 N,M,NQ,                                               &
                 LDSCLD,LDSTPD,LDWE,LD2WE,LDWD,LD2WD,                  &
                 LENWORK,LENIWORK)
         END IF
         IF (PRESENT(INFO)) THEN
            INFO = LINFO
         END IF
         RETURN
      END IF

!  Set array variable defaults except IWORK

      LWORK(1:N*M) = ZERO
      LIFIXB(1)    = -1
      LIFIXX(1,1)  = -1
      LLOWER(1:NP) = -HUGE(ZERO)
      LSCLB(1)     = NEGONE
      LSCLD(1,1)   = NEGONE
      LSTPB(1)     = NEGONE
      LSTPD(1,1)   = NEGONE
      LUPPER(1:NP) = HUGE(ZERO)
      LWE(1,1,1)   = NEGONE
      LWD(1,1,1)   = NEGONE

!  Check the size of required arguments and return errors if they are too small

      IF (SIZE(BETA).LT.NP) THEN
         LINFO1 = LINFO1 + 1
      END IF

      IF (ANY(SIZE(Y).LT.(/N,NQ/))) THEN
         LINFO1 = LINFO1 + 2
      END IF

      IF (ANY(SIZE(X).LT.(/N,M/))) THEN
         LINFO1 = LINFO1 + 4
      END IF

!  Check the presence of optional arguments and copy their values internally or
!  report errors as necessary

      IF (PRESENT(IFIXB)) THEN
         IF (SIZE(IFIXB).LT.NP) THEN
            LINFO1 = LINFO1 + 64
         END IF
         IF (IFIXB(1).LT.0.0_R8) THEN
            LIFIXB(1) = IFIXB(1)
         ELSE
            LIFIXB(1:NP) = IFIXB(1:NP)
         END IF
      END IF

      IF (PRESENT(IFIXX)) THEN
         LDIFX = SIZE(IFIXX,1)
         IF (ANY(SIZE(IFIXX).LE.(/0,0/))) THEN
            LINFO1 = LINFO1 + 128
         END IF
         IF (.NOT.(IFIXX(1,1).LT.ZERO.OR.LDIFX.EQ.1.OR.LDIFX.GE.N).OR. &
            SIZE(IFIXX,2).LT.M) THEN
            LINFO1 = LINFO1 + 128
         END IF
         IF (LDIFX.GT.N) THEN
            LDIFX = N
         END IF
         IF (IFIXX(1,1).LT.0.0_R8) THEN
            LIFIXX(1,1) = IFIXX(1,1)
         ELSE
            LIFIXX(1:LDIFX,1:M) = IFIXX(1:LDIFX,1:M)
         END IF
      END IF

      IF (PRESENT(IWORK)) THEN
         IF (ASSOCIATED(IWORK)) THEN
            IF (SIZE(IWORK).LT.LENIWORK) THEN
               LINFO1 = LINFO1 + 8192
            END IF
            !  This is a restart, copy IWORK.
            IF (MOD(LJOB/10000,10).GE.1) THEN
               LIWORK(1:LENIWORK) = IWORK(1:LENIWORK)
            END IF
         END IF
      END IF

      IF (PRESENT(MAXIT)) THEN
         LMAXIT = MAXIT
      END IF

      IF (PRESENT(NDIGIT)) THEN
         LNDIGIT = NDIGIT
      END IF

      IF (PRESENT(PARTOL)) THEN
         LPARTOL = PARTOL
      END IF

      IF (PRESENT(SCLB)) THEN
         IF (SIZE(SCLB).LT.NP) THEN
            LINFO1 = LINFO1 + 1024
         END IF
         IF (SCLB(1).LE.0.0_R8) THEN
            LSCLB(1) = SCLB(1)
         ELSE
            LSCLB(1:NP) = SCLB(1:NP)
         END IF
      END IF

      IF (PRESENT(SCLD)) THEN
         LDSCLD = SIZE(SCLD,1)
         IF (ANY(SIZE(SCLD).LE.(/0,0/))) THEN
            LINFO1 = LINFO1 + 2048
         END IF
         IF (.NOT.(SCLD(1,1).LE.ZERO.OR.LDSCLD.EQ.1.OR.LDSCLD.GE.N).OR. &
            SIZE(SCLD,2).LT.M) THEN
            LINFO1 = LINFO1 + 2048
         END IF
         IF (LDSCLD.GT.N) THEN
            LDSCLD = N
         END IF
         IF (SCLD(1,1).LE.0.0_R8) THEN
            LSCLD(1,1) = SCLD(1,1)
         ELSE
            LSCLD(1:LDSCLD,1:M) = SCLD(1:LDSCLD,1:M)
         END IF
      END IF

      IF (PRESENT(SSTOL)) THEN
         LSSTOL = SSTOL
      END IF

      IF (PRESENT(STPB)) THEN
         IF (SIZE(STPB).LT.NP) THEN
            LINFO1 = LINFO1 + 256
         END IF
         IF (STPB(1).LE.0.0_R8) THEN
            LSTPB(1) = STPB(1)
         ELSE
            LSTPB(1:NP) = STPB(1:NP)
         END IF
      END IF

      IF (PRESENT(STPD)) THEN
         LDSTPD = SIZE(STPD,1)
         IF (ANY(SIZE(STPD).LE.(/0,0/))) THEN
            LINFO1 = LINFO1 + 512
         END IF
         IF (.NOT.(STPD(1,1).LE.ZERO.OR.LDSTPD.EQ.1.OR.LDSTPD.GE.N).OR. &
            SIZE(STPD,2).LT.M) THEN
            LINFO1 = LINFO1 + 512
         END IF
         IF (LDSTPD.GT.N) THEN
            LDSTPD = N
         END IF
         IF (STPD(1,1).LE.0.0_R8) THEN
            LSTPD(1,1) = STPD(1,1)
         ELSE
            LSTPD(1:LDSTPD,1:M) = STPD(1:LDSTPD,1:M)
         END IF
      END IF

      IF (PRESENT(TAUFAC)) THEN
         LTAUFAC = TAUFAC
      END IF

      IF (PRESENT(WE)) THEN
         LDWE  = SIZE(WE,1)
         LD2WE = SIZE(WE,2)
         IF (ANY(SIZE(WE).LE.(/0,0,0/))) THEN
            LINFO1 = LINFO1 + 16
         END IF
         IF (.NOT.(WE(1,1,1).LT.ZERO.OR.((LDWE.EQ.1.OR.LDWE.GE.N)      &
            .AND.(LD2WE.EQ.1.OR.LD2WE.GE.NQ))).OR.SIZE(WE,3).LT.NQ) THEN
            LINFO1 = LINFO1 + 16
         END IF
         IF (LDWE.GT.N) THEN
            LDWE = N
         END IF
         IF (LD2WE.GT.NQ) THEN
            LD2WE = NQ
         END IF
         IF (WE(1,1,1).LT.0.0_R8) THEN
            LWE(1,1,1) = WE(1,1,1)
         ELSE
            LWE(1:LDWE,1:LD2WE,1:NQ) = WE(1:LDWE,1:LD2WE,1:NQ)
         END IF
      END IF

      IF (PRESENT(WD)) THEN
         LDWD  = SIZE(WD,1)
         LD2WD = SIZE(WD,2)
         IF (ANY(SIZE(WD).LE.(/0,0,0/))) THEN
            LINFO1 = LINFO1 + 32
         END IF
         IF (.NOT.(WD(1,1,1).LT.ZERO.OR.((LDWD.EQ.1.OR.LDWD.GE.N)      &
            .AND.(LD2WD.EQ.1.OR.LD2WD.GE.M))).OR.SIZE(WD,3).LT.M) THEN
            LINFO1 = LINFO1 + 32
         END IF
         IF (LDWD.GT.N) THEN
            LDWD = N
         END IF
         IF (LD2WD.GT.M) THEN
            LD2WD = M
         END IF
         IF (WD(1,1,1).LE.0.0_R8) THEN
            LWD(1,1,1) = WD(1,1,1)
         ELSE
            LWD(1:LDWD,1:LD2WD,1:M) = WD(1:LDWD,1:LD2WD,1:M)
         END IF
      END IF

      IF (PRESENT(WORK)) THEN
         IF (ASSOCIATED(WORK)) THEN
            IF (SIZE(WORK).LT.LENWORK) THEN
               LINFO1 = LINFO1 + 4096
            END IF
            !  Deltas are in WORK, copy them.
            IF (MOD(LJOB/1000,10).GE.1.AND..NOT.PRESENT(DELTA)) THEN
               LWORK(1:N*M) = WORK(1:N*M)
            END IF
            !  This is a restart, copy WORK.
            IF (MOD(LJOB/10000,10).GE.1) THEN
               LWORK(1:LENWORK) = WORK(1:LENWORK)
            END IF
         END IF
      END IF

      IF (PRESENT(DELTA)) THEN
         IF (ASSOCIATED(DELTA)) THEN
            IF (ANY(SHAPE(DELTA).LT.(/N,M/))) THEN
               LINFO1 = LINFO1 + 8
            END IF
            LWORK(1:N*M) = RESHAPE(DELTA(1:N,1:M),(/N*M/))
         END IF
      END IF

      IF (PRESENT(LOWER)) THEN
         IF (SIZE(LOWER).LT.NP) THEN
            LINFO1 = LINFO1 + 32768
         END IF
         LLOWER(1:NP) = LOWER(1:NP)
      END IF

      IF (PRESENT(UPPER)) THEN
         IF (SIZE(UPPER).LT.NP) THEN
            LINFO1 = LINFO1 + 16384
         END IF
         LUPPER(1:NP) = UPPER(1:NP)
      END IF

!  Report an error if any of the array sizes didn't match.

      IF (LINFO1.NE.0) THEN
         LINFO = 100000 + LINFO1
         LINFO1 = 0
         IF (LLUNERR.GT.0.AND.LIPRINT.NE.0) THEN
            CALL DODPHD(HEAD,LLUNRPT)
            CALL DODPE1(                                               &
                 LLUNERR,LINFO,LINFO5,LINFO4,LINFO3,LINFO2,LINFO1,     &
                 N,M,NQ,                                               &
                 LDSCLD,LDSTPD,LDWE,LD2WE,LDWD,LD2WD,                  &
                 LENWORK,LENIWORK)
         END IF
         IF (PRESENT(INFO)) THEN
            INFO = LINFO
         END IF
         RETURN
      END IF


      IF (LWD(1,1,1).NE.ZERO) THEN
         CALL DODCNT                                                   &
              (FCN,                                                    &
              N,M,NP,NQ,                                               &
              BETA(1:NP),                                              &
              Y(1:N,1:NQ),N,X(1:N,1:M),N,                              &
              LWE(1:LDWE,1:LD2WE,1:NQ),LDWE,LD2WE,                     &
              LWD(1:LDWD,1:LD2WD,1:M),LDWD,LD2WD,                      &
              LIFIXB,LIFIXX(1:LDIFX,1:M),LDIFX,                        &
              LJOB,LNDIGIT,LTAUFAC,                                    &
              LSSTOL,LPARTOL,LMAXIT,                                   &
              LIPRINT,LLUNERR,LLUNRPT,                                 &
              LSTPB,LSTPD(1:LDSTPD,1:M),LDSTPD,                        &
              LSCLB,LSCLD(1:LDSCLD,1:M),LDSCLD,                        &
              LWORK,LENWORK,LIWORK,LENIWORK,                           &
              LINFO,                                                   &
              LLOWER,LUPPER)
      ELSE
         WD1(1,1,1) = NEGONE
         CALL DODCNT                                                   &
              (FCN,                                                    &
              N,M,NP,NQ,                                               &
              BETA(1:NP),                                              &
              Y(1:N,1:NQ),N,X(1:N,1:M),N,                              &
              LWE(1:LDWE,1:LD2WE,1:NQ),LDWE,LD2WE,                     &
              WD1,1,1,                                                 &
              LIFIXB,LIFIXX(1:LDIFX,1:M),LDIFX,                        &
              LJOB,LNDIGIT,LTAUFAC,                                    &
              LSSTOL,LPARTOL,LMAXIT,                                   &
              LIPRINT,LLUNERR,LLUNRPT,                                 &
              LSTPB,LSTPD(1:LDSTPD,1:M),LDSTPD,                        &
              LSCLB,LSCLD(1:LDSCLD,1:M),LDSCLD,                        &
              LWORK,LENWORK,LIWORK,LENIWORK,                           &
              LINFO,                                                   &
              LLOWER,LUPPER)
      END IF

      IF (PRESENT(DELTA)) THEN
         IF (ASSOCIATED(DELTA)) THEN
            DELTA(1:N,1:M) = RESHAPE(LWORK(1:N*M),(/N,M/))
         ELSE
            LDELTA(1:N,1:M) = RESHAPE(LWORK(1:N*M),(/N,M/))
            DELTA => LDELTA
         END IF
      END IF

      IF (PRESENT(INFO)) THEN
         INFO = LINFO
      END IF

      IF (PRESENT(IWORK)) THEN
         IF (.NOT.ASSOCIATED(IWORK)) THEN
            IWORK => LIWORK
         ELSE
            IWORK(1:LENIWORK) = LIWORK(1:LENIWORK)
            DEALLOCATE(LIWORK)
         END IF
      ELSE
         DEALLOCATE(LIWORK)
      END IF

      IF (PRESENT(WORK)) THEN
         IF (.NOT.ASSOCIATED(WORK)) THEN
            WORK => LWORK
         ELSE
            WORK(1:LENWORK) = LWORK(1:LENWORK)
            DEALLOCATE(LWORK)
         END IF
      ELSE
         DEALLOCATE(LWORK)
      END IF

      DEALLOCATE(TEMPRET)

      RETURN

      END SUBROUTINE ODR
      END MODULE ODRPACK95
!DACCES
      SUBROUTINE DACCES(N,M,NP,NQ,LDWE,LD2WE,                          &
                        WORK,LWORK,IWORK,LIWORK,ACCESS,ISODR,          &
                        JPVT,OMEGA,U,QRAUX,SD,VCV,WRK1,WRK2,WRK3,      &
                        WRK4,WRK5,WRK6,NNZW,NPP,                       &
                        JOB,PARTOL,SSTOL,MAXIT,TAUFAC,ETA,NETA,        &
                        LUNRPT,IPR1,IPR2,IPR2F,IPR3,                   &
                        WSS,RVAR,IDF,                                  &
                        TAU,ALPHA,NITER,NFEV,NJEV,INT2,OLMAVG,         &
                        RCOND,IRANK,ACTRS,PNORM,PRERS,RNORMS,ISTOP)
!***Begin Prologue  DACCES
!***Refer to  ODR
!***Routines Called  DIWINF,DWINF
!***Date Written   860529   (YYMMDD)
!***Revision Date  920619   (YYMMDD)
!***Purpose  Access or store values in the work arrays
!***End Prologue  DACESS

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      REAL (KIND=R8) ACTRS,ALPHA,ETA,OLMAVG,PARTOL,PNORM,PRERS,RCOND,  &
                     RNORMS,RVAR,SSTOL,TAU,TAUFAC
      INTEGER                                                          &
         IDF,INT2,IPR1,IPR2,IPR2F,IPR3,IRANK,ISTOP,ISTOPI,JOB,JPVT,    &
         LDWE,LD2WE,LIWORK,LUNRPT,LWORK,M,MAXIT,N,NETA,NFEV,NITER,     &
         NJEV,NNZW,NP,NPP,NQ,OMEGA,QRAUX,SD,U,VCV,                     &
         WRK1,WRK2,WRK3,WRK4,WRK5,WRK6
      LOGICAL ACCESS,ISODR

!...Array arguments
      REAL (KIND=R8) WORK(LWORK),WSS(3)
      INTEGER IWORK(LIWORK)

!...Local scalars
      INTEGER                                                          &
         ACTRSI,ALPHAI,BETACI,BETANI,BETASI,BETA0I,BOUNDI,             &
         DELTAI,DELTNI,DELTSI,DIFFI,EPSI,                              &
         EPSMAI,ETAI,FJACBI,FJACDI,FNI,FSI,IDFI,INT2I,IPRINI,IPRINT,   &
         IRANKI,JOBI,JPVTI,LDTTI,LIWKMN,LOWERI,LUNERI,LUNRPI,LWKMN,    &
         MAXITI,                                                       &
         MSGB,MSGD,NETAI,NFEVI,NITERI,NJEVI,NNZWI,NPPI,NROWI,          &
         NTOLI,OLMAVI,OMEGAI,PARTLI,PNORMI,PRERSI,QRAUXI,RCONDI,       &
         RNORSI,RVARI,SDI,SI,SSFI,SSI,SSTOLI,TAUFCI,TAUI,TI,TTI,UI,    &
         UPPERI,                                                       &
         VCVI,WE1I,WRK1I,WRK2I,WRK3I,WRK4I,WRK5I,WRK6I,WRK7I,          &
         WSSI,WSSDEI,WSSEPI,XPLUSI
!...External subroutines
      EXTERNAL DIWINF,DWINF

!...Variable Definitions (alphabetically)
!   ACCESS:  The variable designating whether information is to be 
!            accessed from the work arrays (ACCESS=TRUE) or stored in
!            them (ACCESS=FALSE).
!   ACTRS:   The saved actual relative reduction in the sum-of-squares.
!   ACTRSI:  The location in array WORK of variable ACTRS.
!   ALPHA:   The Levenberg-Marquardt parameter.
!   ALPHAI:  The location in array WORK of variable ALPHA.
!   BETACI:  The starting location in array WORK of array BETAC.
!   BETANI:  The starting location in array WORK of array BETAN.
!   BETASI:  The starting location in array WORK of array BETAS.
!   BETA0I:  The starting location in array WORK of array BETA0.
!   DELTAI:  The starting location in array WORK of array DELTA.
!   DELTNI:  The starting location in array WORK of array DELTAN.
!   DELTSI:  The starting location in array WORK of array DELTAS.
!   DIFFI:   The starting location in array WORK of array DIFF.
!   EPSI:    The starting location in array WORK of array EPS.
!   EPSMAI:  The location in array WORK of variable EPSMAC.
!   ETA:     The relative noise in the function results.
!   ETAI:    The location in array WORK of variable ETA.
!   FJACBI:  The starting location in array WORK of array FJACB.
!   FJACDI:  The starting location in array WORK of array FJACD.
!   FNI:     The starting location in array WORK of array FN.
!   FSI:     The starting location in array WORK of array FS.
!   IDF:     The degrees of freedom of the fit, equal to the number of
!            observations with nonzero weighted derivatives minus the
!            number of parameters being estimated.
!   IDFI:    The starting location in array IWORK of variable IDF.
!   INT2:    The number of internal doubling steps.
!   INT2I:   The location in array IWORK of variable INT2.
!   IPR1:    The value of the fourth digit (from the right) of IPRINT,
!            which controls the initial summary report.
!   IPR2:    The value of the third digit (from the right) of IPRINT,
!            which controls the iteration reports.
!   IPR2F:   The value of the second digit (from the right) of IPRINT,
!            which controls the frequency of the iteration reports.
!   IPR3:    The value of the first digit (from the right) of IPRINT,
!            which controls the final summary report.
!   IPRINI:  The location in array IWORK of variable IPRINT.
!   IPRINT:  The print control variable.
!   IRANK:   The rank deficiency of the Jacobian wrt BETA.
!   IRANKI:  The location in array IWORK of variable IRANK.
!   ISODR:   The variable designating whether the solution is to be 
!            found by ODR (ISODR=TRUE) or by OLS (ISODR=FALSE).
!   ISTOP:   The variable designating whether there are problems
!            computing the function at the current BETA and DELTA.
!   ISTOPI:  The location in array IWORK of variable ISTOP.
!   IWORK:   The integer work space.
!   JOB:     The variable controling problem initialization and 
!            computational method.
!   JOBI:    The location in array IWORK of variable JOB.
!   JPVT:    The pivot vector.
!   JPVTI:   The starting location in array IWORK of variable JPVT.
!   LDTTI:   The starting location in array IWORK of variable LDTT.
!   LDWE:    The leading dimension of array WE. 
!   LD2WE:   The second dimension of array WE. 
!   LIWORK:  The length of vector IWORK.
!   LUNERI:  The location in array IWORK of variable LUNERR.
!   LUNERR:  The logical unit number used for error messages.
!   LUNRPI:  The location in array IWORK of variable LUNRPT.
!   LUNRPT:  The logical unit number used for computation reports.
!   LWKMN:   The minimum acceptable length of array WORK.
!   LWORK:   The length of vector WORK.
!   M:       The number of columns of data in the explanatory variable.
!   MAXIT:   The maximum number of iterations allowed.
!   MAXITI:  The location in array IWORK of variable MAXIT.
!   MSGB:    The starting location in array IWORK of array MSGB.
!   MSGD:    The starting location in array IWORK of array MSGD.
!   N:       The number of observations.
!   NETA:    The number of accurate digits in the function results.
!   NETAI:   The location in array IWORK of variable NETA.
!   NFEV:    The number of function evaluations.
!   NFEVI:   The location in array IWORK of variable NFEV.
!   NITER:   The number of iterations taken.
!   NITERI:  The location in array IWORK of variable NITER.
!   NJEV:    The number of Jacobian evaluations.
!   NJEVI:   The location in array IWORK of variable NJEV.
!   NNZW:    The number of nonzero weighted observations.
!   NNZWI:   The location in array IWORK of variable NNZW.
!   NP:      The number of function parameters.
!   NPP:     The number of function parameters actually estimated.
!   NPPI:    The location in array IWORK of variable NPP.
!   NQ:      The number of responses per observation.
!   NROWI:   The location in array IWORK of variable NROW.
!   NTOLI:   The location in array IWORK of variable NTOL.
!   OLMAVG:  The average number of Levenberg-Marquardt steps per 
!            iteration.
!   OLMAVI:  The location in array WORK of variable OLMAVG.
!   OMEGA:   The starting location in array WORK of array OMEGA.
!   OMEGAI:  The starting location in array WORK of array OMEGA.
!   PARTLI:  The location in array work of variable PARTOL.
!   PARTOL:  The parameter convergence stopping tolerance.
!   PNORM:   The norm of the scaled estimated parameters.
!   PNORMI:  The location in array WORK of variable PNORM.
!   PRERS:   The saved predicted relative reduction in the 
!            sum-of-squares.
!   PRERSI:  The location in array WORK of variable PRERS.
!   QRAUX:   The starting location in array WORK of array QRAUX.
!   QRAUXI:  The starting location in array WORK of array QRAUX.
!   RCOND:   The approximate reciprocal condition of FJACB.
!   RCONDI:  The location in array WORK of variable RCOND.
!   RESTRT:  The variable designating whether the call is a restart 
!            (RESTRT=TRUE) or not (RESTRT=FALSE).
!   RNORMS:  The norm of the saved weighted EPSILONS and DELTAS.
!   RNORSI:  The location in array WORK of variable RNORMS.
!   RVAR:    The residual variance, i.e. standard deviation squared.
!   RVARI:   The location in array WORK of variable RVAR.
!   SCLB:    The scaling values used for BETA.
!   SCLD:    The scaling values used for DELTA.
!   SD:      The starting location in array WORK of array SD.
!   SDI:     The starting location in array WORK of array SD.
!   SI:      The starting location in array WORK of array S.
!   SSFI:    The starting location in array WORK of array SSF.
!   SSI:     The starting location in array WORK of array SS.
!   SSTOL:   The sum-of-squares convergence stopping tolerance.
!   SSTOLI:  The location in array WORK of variable SSTOL.
!   TAU:     The trust region diameter.
!   TAUFAC:  The factor used to compute the initial trust region 
!            diameter.
!   TAUFCI:  The location in array WORK of variable TAUFAC.
!   TAUI:    the location in array WORK of variable TAU.
!   TI:      The starting location in array WORK of array T.
!   TTI:     The starting location in array WORK of array TT.
!   U:       The starting location in array WORK of array U.
!   UI:      The starting location in array WORK of array U.
!   VCV:     The starting location in array WORK of array VCV.
!   VCVI:    The starting location in array WORK of array VCV.
!   WE1I:    The starting location in array WORK of array WE1.
!   WORK:    The REAL (KIND=R8) work space.
!   WRK1:    The starting location in array WORK of array WRK1.
!   WRK1I:   The starting location in array WORK of array WRK1.
!   WRK2:    The starting location in array WORK of array WRK2.
!   WRK2I:   The starting location in array WORK of array WRK2.
!   WRK3:    The starting location in array WORK of array wrk3.
!   WRK3I:   The starting location in array WORK of array wrk3.
!   WRK4:    The starting location in array WORK of array wrk4.
!   WRK4I:   The starting location in array WORK of array wrk4.
!   WRK5:    The starting location in array WORK of array wrk5.
!   WRK5I:   The starting location in array WORK of array wrk5.
!   WRK6:    The starting location in array WORK of array wrk6.
!   WRK6I:   The starting location in array WORK of array wrk6.
!   WRK7I:   The starting location in array WORK of array wrk7.
!   WSS:     The sum of the squares of the weighted EPSILONS and DELTAS,
!            the sum of the squares of the weighted DELTAS, and
!            the sum of the squares of the weighted EPSILONS.
!   WSSI:    The starting location in array WORK of variable WSS(1).
!   WSSDEI:  The starting location in array WORK of variable WSS(2).
!   WSSEPI:  The starting location in array WORK of variable WSS(3).
!   XPLUSI:  The starting location in array WORK of array XPLUSD.


!***First executable statement  DACCES


!  Find starting locations within integer workspace

      CALL DIWINF(M,NP,NQ,MSGB,MSGD,JPVTI,ISTOPI,                      &
                  NNZWI,NPPI,IDFI,JOBI,IPRINI,LUNERI,LUNRPI,           &
                  NROWI,NTOLI,NETAI,                                   &
                  MAXITI,NITERI,NFEVI,NJEVI,INT2I,IRANKI,LDTTI,        &
                  BOUNDI,LIWKMN)

!  Find starting locations within REAL (KIND=R8) work space

      CALL DWINF(N,M,NP,NQ,LDWE,LD2WE,ISODR,                           &
                 DELTAI,EPSI,XPLUSI,FNI,SDI,VCVI,                      &
                 RVARI,WSSI,WSSDEI,WSSEPI,RCONDI,ETAI,                 &
                 OLMAVI,TAUI,ALPHAI,ACTRSI,PNORMI,RNORSI,PRERSI,       &
                 PARTLI,SSTOLI,TAUFCI,EPSMAI,                          &
                 BETA0I,BETACI,BETASI,BETANI,SI,SSI,SSFI,QRAUXI,UI,    &
                 FSI,FJACBI,WE1I,DIFFI,                                &
                 DELTSI,DELTNI,TI,TTI,OMEGAI,FJACDI,                   &
                 WRK1I,WRK2I,WRK3I,WRK4I,WRK5I,WRK6I,WRK7I,            &
                 LOWERI,UPPERI,LWKMN)

      IF (ACCESS) THEN

!  Set starting locations for work vectors

         JPVT   = JPVTI
         OMEGA  = OMEGAI
         QRAUX  = QRAUXI
         SD     = SDI
         VCV    = VCVI
         U      = UI
         WRK1   = WRK1I
         WRK2   = WRK2I
         WRK3   = WRK3I
         WRK4   = WRK4I
         WRK5   = WRK5I
         WRK6   = WRK6I

!  Access values from the work vectors

         ACTRS  = WORK(ACTRSI)
         ALPHA  = WORK(ALPHAI)
         ETA    = WORK(ETAI)
         OLMAVG = WORK(OLMAVI)
         PARTOL = WORK(PARTLI)
         PNORM  = WORK(PNORMI)
         PRERS  = WORK(PRERSI)
         RCOND  = WORK(RCONDI)
         WSS(1) = WORK(WSSI)
         WSS(2) = WORK(WSSDEI)
         WSS(3) = WORK(WSSEPI)
         RVAR   = WORK(RVARI)
         RNORMS = WORK(RNORSI)
         SSTOL  = WORK(SSTOLI)
         TAU    = WORK(TAUI)
         TAUFAC = WORK(TAUFCI)
   
         NETA   = IWORK(NETAI)
         IRANK  = IWORK(IRANKI)
         JOB    = IWORK(JOBI)
         LUNRPT = IWORK(LUNRPI)
         MAXIT  = IWORK(MAXITI)
         NFEV   = IWORK(NFEVI)
         NITER  = IWORK(NITERI)
         NJEV   = IWORK(NJEVI)
         NNZW   = IWORK(NNZWI)
         NPP    = IWORK(NPPI)
         IDF    = IWORK(IDFI)
         INT2   = IWORK(INT2I)
       
!  Set up print control variables
 
         IPRINT = IWORK(IPRINI)
   
         IPR1   = MOD(IPRINT,10000)/1000
         IPR2   = MOD(IPRINT,1000)/100
         IPR2F  = MOD(IPRINT,100)/10
         IPR3   = MOD(IPRINT,10)
    
      ELSE

!  Store values into the work vectors

         WORK(ACTRSI)  = ACTRS   
         WORK(ALPHAI)  = ALPHA   
         WORK(OLMAVI)  = OLMAVG  
         WORK(PARTLI)  = PARTOL  
         WORK(PNORMI)  = PNORM   
         WORK(PRERSI)  = PRERS   
         WORK(RCONDI)  = RCOND   
         WORK(WSSI)    = WSS(1)
         WORK(WSSDEI)  = WSS(2)
         WORK(WSSEPI)  = WSS(3)
         WORK(RVARI)   = RVAR
         WORK(RNORSI)  = RNORMS  
         WORK(SSTOLI)  = SSTOL   
         WORK(TAUI)    = TAU     

         IWORK(IRANKI) = IRANK   
         IWORK(ISTOPI) = ISTOP   
         IWORK(NFEVI)  = NFEV    
         IWORK(NITERI) = NITER   
         IWORK(NJEVI)  = NJEV    
         IWORK(IDFI)   = IDF    
         IWORK(INT2I)  = INT2    
      END IF

      RETURN
      END SUBROUTINE
!DESUBI
      SUBROUTINE DESUBI(N,M,WD,LDWD,LD2WD,ALPHA,TT,LDTT,I,E)
!***Begin Prologue  DESUBI
!***Refer to  ODR
!***Routines Called  DZERO
!***Date Written   860529   (YYMMDD)
!***Revision Date  920304   (YYMMDD)
!***Purpose  Compute E = WD + ALPHA*TT**2
!***End Prologue  DESUBI

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      REAL (KIND=R8) ALPHA
      INTEGER LDTT,LDWD,LD2WD,M,N

!...Array arguments
      REAL (KIND=R8) E(M,M),TT(LDTT,M),WD(LDWD,LD2WD,M)

!...Local scalars
      REAL (KIND=R8) ZERO
      INTEGER I,J,J1,J2

!...External subroutines
      EXTERNAL DZERO

!...Data statements
      DATA ZERO /0.0E0_R8/

!...Variable Definitions (alphabetically)
!   ALPHA:  The Levenberg-Marquardt parameter.
!   E:      The value of the array E = WD + ALPHA*TT**2
!   I:      An indexing variable.
!   J:      An indexing variable.
!   J1:     An indexing variable.
!   J2:     An indexing variable.
!   LDWD:   The leading dimension of array WD.
!   LD2WD:  The second dimension of array WD.
!   M:      The number of columns of data in the independent variable.
!   N:      The number of observations.
!   NP:     The number of responses per observation.
!   TT:     The scaling values used for DELTA.
!   WD:     The squared DELTA weights, D**2.
!   ZERO:   The value 0.0E0_R8.


!***First executable statement  DESUBI


!   N.B. the locations of WD and TT accessed depend on the value
!        of the first element of each array and the leading dimensions
!        of the multiply subscripted arrays.

      IF (N.EQ.0 .OR. M.EQ.0) RETURN

      IF (WD(1,1,1).GE.ZERO) THEN
         IF (LDWD.GE.N) THEN
!  The elements of WD have been individually specified

            IF (LD2WD.EQ.1) THEN
!  The arrays stored in WD are diagonal
               CALL DZERO(M,M,E,M)
               DO 10 J=1,M
                  E(J,J) = WD(I,1,J)
   10          CONTINUE
            ELSE
!  The arrays stored in WD are full positive semidefinite matrices
               DO 30 J1=1,M
                  DO 20 J2=1,M
                     E(J1,J2) = WD(I,J1,J2)
   20             CONTINUE
   30          CONTINUE
            END IF

            IF (TT(1,1).GT.ZERO) THEN
               IF (LDTT.GE.N) THEN
                  DO 110 J=1,M
                     E(J,J) = E(J,J) + ALPHA*TT(I,J)**2
  110             CONTINUE
               ELSE
                  DO 120 J=1,M
                     E(J,J) = E(J,J) + ALPHA*TT(1,J)**2
  120             CONTINUE
               END IF
            ELSE
               DO 130 J=1,M
                  E(J,J) = E(J,J) + ALPHA*TT(1,1)**2
  130          CONTINUE
            END IF
         ELSE
!  WD is an M by M matrix

            IF (LD2WD.EQ.1) THEN
!  The array stored in WD is diagonal
               CALL DZERO(M,M,E,M)
               DO 140 J=1,M
                  E(J,J) = WD(1,1,J)
  140          CONTINUE
            ELSE
!  The array stored in WD is a full positive semidefinite matrices
               DO 160 J1=1,M
                  DO 150 J2=1,M
                     E(J1,J2) = WD(1,J1,J2)
  150             CONTINUE
  160          CONTINUE
            END IF

            IF (TT(1,1).GT.ZERO) THEN
               IF (LDTT.GE.N) THEN
                  DO 210 J=1,M
                     E(J,J) = E(J,J) + ALPHA*TT(I,J)**2
  210             CONTINUE
               ELSE
                  DO 220 J=1,M
                     E(J,J) = E(J,J) + ALPHA*TT(1,J)**2
  220             CONTINUE
               END IF
            ELSE
               DO 230 J=1,M
                  E(J,J) = E(J,J) + ALPHA*TT(1,1)**2
  230          CONTINUE
            END IF
         END IF
      ELSE
!  WD is a diagonal matrix with elements ABS(WD(1,1,1))
         CALL DZERO(M,M,E,M)
         IF (TT(1,1).GT.ZERO) THEN
            IF (LDTT.GE.N) THEN
               DO 310 J=1,M
                  E(J,J) = ABS(WD(1,1,1)) + ALPHA*TT(I,J)**2
  310          CONTINUE
            ELSE
               DO 320 J=1,M
                  E(J,J) = ABS(WD(1,1,1)) + ALPHA*TT(1,J)**2
  320          CONTINUE
            END IF
         ELSE
            DO 330 J=1,M
               E(J,J) = ABS(WD(1,1,1)) + ALPHA*TT(1,1)**2
  330       CONTINUE
         END IF
      END IF

      RETURN
      END SUBROUTINE
!DETAF
      SUBROUTINE DETAF(FCN,N,M,NP,NQ,XPLUSD,BETA,EPSMAC,NROW,          &
                       PARTMP,PV0,IFIXB,IFIXX,LDIFX,                   &
                       ISTOP,NFEV,ETA,NETA,WRK1,WRK2,WRK6,WRK7,        &
                       INFO,LOWER,UPPER)
!***Begin Prologue  DETAF
!***Refer to  ODR
!***Routines Called  FCN
!***Date Written   860529   (YYMMDD)
!***Revision Date  920619   (YYMMDD)
!***Purpose  Compute noise and number of good digits in function results
!            (Adapted from STARPAC subroutine ETAFUN)
!***End Prologue  DETAF

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      REAL (KIND=R8) EPSMAC,ETA
      INTEGER INFO,ISTOP,LDIFX,M,N,NETA,NFEV,NP,NQ,NROW

!...Array arguments
      REAL (KIND=R8) BETA(NP),LOWER(NP),PARTMP(NP),PV0(N,NQ),UPPER(NP), &
         WRK1(N,M,NQ),WRK2(N,NQ),WRK6(N,NP,NQ),WRK7(-2:2,NQ),XPLUSD(N,M)
      INTEGER IFIXB(NP),IFIXX(LDIFX,M)

!...Subroutine arguments
      EXTERNAL FCN

!...Local scalars
      REAL (KIND=R8) A,B,FAC,HUNDRD,ONE,P1,P2,P5,SHIFT,STP,TWO,ZERO
      INTEGER J,K,L,SBK

!...Local arrays
      REAL (KIND=R8) PARPTS(-2:2,NP)

!...Data statements
      DATA ZERO,P1,P2,P5,ONE,TWO,HUNDRD                                &
         /0.0E0_R8,0.1E0_R8,0.2E0_R8,0.5E0_R8,1.0E0_R8,2.0E0_R8,1.0E2_R8/

!...Routine names used as subprogram arguments
!   FCN:      The user supplied subroutine for evaluating the model.

!...Variable Definitions (ALPHABETICALLY)
!   A:       Parameters of the local fit.
!   B:       Parameters of the local fit.
!   BETA:    The function parameters.
!   EPSMAC:  The value of machine precision.
!   ETA:     The noise in the model results.
!   FAC:     A factor used in the computations.
!   HUNDRD:  The value 1.0E2_R8.
!   IFIXB:   The values designating whether the elements of BETA are
!            fixed at their input values or not.
!   IFIXX:   The values designating whether the elements of X are
!            fixed at their input values or not.
!   ISTOP:   The variable designating whether there are problems
!            Computing the function at the current BETA and DELTA.
!   J:       An index variable.
!   K:       An index variable.
!   L:       AN INDEX VARIABLE.
!   LDIFX:   The leading dimension of array IFIXX.
!   LOWER:   The lower bound of BETA.
!   M:       The number of columns of data in the explanatory variable.
!   N:       The number of observations.
!   NETA:    The number of accurate digits in the model results.
!   NFEV:    The number of function evaluations.
!   NP:      The number of function parameters.
!   NQ:      The number of responses per observation.
!   NROW:    The row number at which the derivative is to be checked.
!   ONE:     The value 1.0E0_R8.
!   P1:      The value 0.1E0_R8.
!   P2:      The value 0.2E0_R8.
!   P5:      The value 0.5E0_R8.
!   PARPTS:  The points that PARTMP will take on during FCN evaluations.
!   PARTMP:  The model parameters.
!   PV0:     The original predicted values.
!   SHIFT:   When PARPTS cross the parameter bounds they are shifted by SHIFT.
!   SBK:     The sign of BETA(K).
!   STP:     A small value used to perturb the parameters.
!   UPPER:   The upper bound of BETA.
!   WRK1:    A work array of (N BY M BY NQ) elements.
!   WRK2:    A work array of (N BY NQ) elements.
!   WRK6:    A work array of (N BY NP BY NQ) elements.
!   WRK7:    A work array of (5 BY NQ) elements.
!   XPLUSD:  The values of X + DELTA.
!   ZERO:    The value 0.0E0_R8.


!***First executable statement  DETAF


      STP = HUNDRD*EPSMAC
      ETA = EPSMAC

!   Create points to use in calculating FCN for ETA and NETA.
      DO J=-2,2
         IF (J.EQ.0) THEN
            PARPTS(0,:) = BETA(:)
         ELSE
            DO K=1,NP
               IF (IFIXB(1).LT.0) THEN
                  PARPTS(J,K) = BETA(K) + J*STP*BETA(K)
               ELSE IF (IFIXB(K).NE.0) THEN
                  PARPTS(J,K) = BETA(K) + J*STP*BETA(K)
               ELSE 
                  PARPTS(J,K) = BETA(K)
               END IF
            END DO
         END IF
      END DO

!   Adjust the points used in calculating FCN to uphold the boundary
!   constraints.
      DO K=1,NP
         SBK = SIGN(ONE,PARPTS(2,K)-PARPTS(-2,K))
         IF (PARPTS(SBK*2,K).GT.UPPER(K)) THEN 
            SHIFT = UPPER(K) - PARPTS(SBK*2,K)
            PARPTS(SBK*2,K) = UPPER(K)
            DO J=-SBK*2,SBK*1,SBK
               PARPTS(J,K) = PARPTS(J,K) + SHIFT
            END DO
            IF (PARPTS(-SBK*2,K).LT.LOWER(K)) THEN
               INFO = 90010
               RETURN
            END IF
         END IF
         IF (PARPTS(-SBK*2,K).LT.LOWER(K)) THEN
            SHIFT = LOWER(K) - PARPTS(-SBK*2,K)
            PARPTS(-SBK*2,K) = LOWER(K)
            DO J=-SBK*1,SBK*2,SBK
               PARPTS(J,K) = PARPTS(J,K) + SHIFT
            END DO
            IF (PARPTS(SBK*2,K).GT.UPPER(K)) THEN
               INFO = 90010
               RETURN
            END IF
         END IF
      END DO

!   Evaluate FCN for all points in PARPTS.
      DO J=-2,2
         IF (ALL(PARPTS(J,:).EQ.BETA(:))) THEN
            DO L=1,NQ
               WRK7(J,L) = PV0(NROW,L)
            END DO
         ELSE
            PARTMP(:) = PARPTS(J,:)
            ISTOP = 0
            CALL FCN(N,M,NP,NQ,N,M,NP,PARTMP(:),XPLUSD,                &
                     IFIXB,IFIXX,LDIFX,003,WRK2,WRK6,WRK1,ISTOP)
            IF (ISTOP.NE.0) THEN
               RETURN
            ELSE
               NFEV = NFEV + 1
            END IF
            DO L=1,NQ
               WRK7(J,L) = WRK2(NROW,L)
            END DO
         END IF
      END DO

!   Calculate ETA and NETA.
      DO 100 L=1,NQ
         A = ZERO
         B = ZERO
         DO 50 J=-2,2
            A = A + WRK7(J,L)
            B = B + J*WRK7(J,L)
   50    CONTINUE
         A = P2*A
         B = P1*B
         IF ((WRK7(0,L).NE.ZERO) .AND.                                 &
             (ABS(WRK7(1,L)+WRK7(-1,L)).GT.HUNDRD*EPSMAC)) THEN
            FAC = ONE/ABS(WRK7(0,L))
         ELSE
            FAC = ONE
         END IF
         DO 60 J=-2,2
            WRK7(J,L) = ABS((WRK7(J,L)-(A+J*B))*FAC)
            ETA = MAX(WRK7(J,L),ETA)
   60    CONTINUE
  100 CONTINUE
      NETA = MAX(TWO,P5-LOG10(ETA))

      RETURN
      END SUBROUTINE
!DEVJAC
      SUBROUTINE DEVJAC(FCN,ANAJAC,CDJAC,N,M,NP,NQ,BETAC,BETA,STPB,   &
                        IFIXB,IFIXX,LDIFX,                            &
                        X,LDX,DELTA,XPLUSD,STPD,LDSTPD,               &
                        SSF,TT,LDTT,NETA,FN,                          &
                        STP,WRK1,WRK2,WRK3,WRK6,                      &
                        FJACB,ISODR,FJACD,WE1,LDWE,LD2WE,             &
                        NJEV,NFEV,ISTOP,INFO,LOWER,UPPER)
!***Begin Prologue  DEVJAC
!***Refer to  ODR
!***Routines Called  FCN,DDOT,DIFIX,DJACCD,DJACFD,DWGHT,DUNPAC,DXPY
!***Date Written   860529   (YYMMDD)
!***Revision Date  920304   (YYMMDD)
!***Purpose  Compute the weighted Jacobians wrt BETA and DELTA
!***End Prologue  DEVJAC

!...Used modules
      USE REAL_PRECISION
      USE ODRPACK95, ONLY : TEMPRET

!...Scalar arguments
      INTEGER INFO,ISTOP,LDIFX,LDSTPD,LDTT,LDWE,LDX,LD2WE,             &
              M,N,NETA,NFEV,NJEV,NP,NQ
      LOGICAL ANAJAC,CDJAC,ISODR

!...Array arguments
      REAL (KIND=R8)                                                   &
           BETA(NP),BETAC(NP),DELTA(N,M),FJACB(N,NP,NQ),FJACD(N,M,NQ), &
           FN(N,NQ),LOWER(NP),SSF(NP),STP(N),STPB(NP),STPD(LDSTPD,M),  &
           TT(LDTT,M),UPPER(NP),                                       &
           WE1(LDWE,LD2WE,NQ),WRK1(N,M,NQ),WRK2(N,NQ),WRK3(NP),        &
           WRK6(N,NP,NQ),X(LDX,M),XPLUSD(N,M)
      INTEGER IFIXB(NP),IFIXX(LDIFX,M)

!...Subroutine arguments
      EXTERNAL FCN

!...Local scalars
      INTEGER IDEVAL,J,K,K1,L
      REAL (KIND=R8) ZERO
      LOGICAL ERROR

!...External subroutines
      EXTERNAL DIFIX,DJACCD,DJACFD,DUNPAC,DXPY

!...External functions
      REAL (KIND=R8) DDOT
      EXTERNAL DDOT

!...Data statements
      DATA ZERO /0.0E0_R8/

!...Interface blocks
      INTERFACE
      SUBROUTINE DWGHT (N,M,WT,LDWT,LD2WT,T,WTT)
      USE REAL_PRECISION
      INTEGER LDWT,LD2WT,M,N
      REAL (KIND=R8) T(:,:),WT(:,:,:),WTT(:,:)
      END SUBROUTINE
      END INTERFACE

!...Routine names used as subprogram arguments
!   FCN:     The user-supplied subroutine for evaluating the model.

!...Variable Definitions (alphabetically)
!   ANAJAC:  The variable designating whether the Jacobians are 
!            computed by finite differences (ANAJAC=FALSE) or not
!            (ANAJAC=TRUE).
!   BETA:    The function parameters.
!   BETAC:   The current estimated values of the unfixed BETA's.
!   CDJAC:   The variable designating whether the Jacobians are 
!            computed by central differences (CDJAC=TRUE) or by forward
!            differences (CDJAC=FALSE).
!   DELTA:   The estimated values of DELTA.
!   ERROR:   The variable designating whether ODRPACK95 detected nonzero 
!            values in array DELTA in the OLS case, and thus whether 
!            the user may have overwritten important information
!            by computing FJACD in the OLS case.
!   FJACB:   The Jacobian with respect to BETA.
!   FJACD:   The Jacobian with respect to DELTA.
!   FN:      The predicted values of the function at the current point.
!   IDEVAL:  The variable designating what computations are to be
!            performed by user-supplied subroutine FCN.
!   IFIXB:   The values designating whether the elements of BETA are 
!            fixed at their input values or not.
!   IFIXX:   The values designating whether the elements of DELTA are 
!            fixed at their input values or not.
!   INFO:    The variable designating why the computations were stopped.
!   ISTOP:   The variable designating that the user wishes the 
!            computations stopped.
!   ISODR:   The variable designating whether the solution is by ODR 
!            (ISODR=TRUE) or OLS (ISODR=FALSE).
!   J:       An indexing variable.
!   K:       An indexing variable.
!   K1:      An indexing variable.
!   L:       An indexing variable.
!   LDIFX:   The leading dimension of array IFIXX.
!   LDSTPD:  The leading dimension of array STPD.
!   LDTT:    The leading dimension of array TT.
!   LDWE:    The leading dimension of arrays WE and WE1.
!   LDX:     The leading dimension of array X.
!   LD2WE:   The second dimension of arrays WE and WE1.
!   M:       The number of columns of data in the independent variable.
!   N:       The number of observations.
!   NETA:    The number of accurate digits in the function results.
!   NFEV:    The number of function evaluations.
!   NJEV:    The number of Jacobian evaluations.
!   NP:      The number of function parameters.
!   NQ:      The number of responses per observation.
!   SSF:     The scale used for the BETA's.
!   STP:     The step used for computing finite difference
!            derivatives with respect to DELTA.
!   STPB:    The relative step used for computing finite difference
!            derivatives with respect to BETA.
!   STPD:    The relative step used for computing finite difference
!            derivatives with respect to DELTA.
!   TT:      The scaling values used for DELTA.
!   WE1:     The square roots of the EPSILON weights in array WE.
!   WRK1:    A work array of (N by M by NQ) elements.
!   WRK2:    A work array of (N by NQ) elements.
!   WRK3:    A work array of (NP) elements.
!   WRK6:    A work array of (N BY NP BY NQ) elements.
!   X:       The independent variable.
!   XPLUSD:  The values of X + DELTA.
!   ZERO:    The value 0.0E0_R8.


!***First executable statement  DEVJAC


!  Insert current unfixed BETA estimates into BETA 

      CALL DUNPAC(NP,BETAC,BETA,IFIXB)

!  Compute XPLUSD = X + DELTA

      CALL DXPY(N,M,X,LDX,DELTA,N,XPLUSD,N)

!  Compute the Jacobian wrt the estimated BETAS (FJACB) and
!          the Jacobian wrt DELTA (FJACD)

      ISTOP = 0
      IF (ISODR) THEN
         IDEVAL = 110
      ELSE
         IDEVAL = 010
      END IF
      IF (ANAJAC) THEN
         CALL FCN(N,M,NP,NQ,N,M,NP,BETA,XPLUSD,IFIXB,IFIXX,LDIFX,      &
                  IDEVAL,WRK2,FJACB,FJACD,ISTOP)
         IF (ISTOP.NE.0) THEN
            RETURN
         ELSE
            NJEV = NJEV+1
         END IF
!  Make sure fixed elements of FJACD are zero
         IF (ISODR) THEN
            DO 10 L=1,NQ
               CALL DIFIX(N,M,IFIXX,LDIFX,FJACD(1,1,L),N,FJACD(1,1,L),N)
   10       CONTINUE
         END IF
      ELSE IF (CDJAC) THEN
         CALL DJACCD(FCN,N,M,NP,NQ,                                    &
                     BETA,X,LDX,DELTA,XPLUSD,IFIXB,IFIXX,LDIFX,        &
                     STPB,STPD,LDSTPD,                                 &
                     SSF,TT,LDTT,NETA,FN,STP,WRK1,WRK2,WRK3,WRK6,      &
                     FJACB,ISODR,FJACD,NFEV,ISTOP,INFO,LOWER,UPPER)
      ELSE 
         CALL DJACFD(FCN,N,M,NP,NQ,                                    &
                     BETA,X,LDX,DELTA,XPLUSD,IFIXB,IFIXX,LDIFX,        &
                     STPB,STPD,LDSTPD,                                 &
                     SSF,TT,LDTT,NETA,FN,STP,WRK1,WRK2,WRK3,WRK6,      &
                     FJACB,ISODR,FJACD,NFEV,ISTOP,INFO,LOWER,UPPER)
      END IF
      IF (ISTOP.LT.0.OR.INFO.GE.10000) THEN
         RETURN
      ELSE IF (.NOT.ISODR) THEN
!  Try to detect whether the user has computed JFACD 
!  Within FCN in the OLS case
         ERROR = DDOT(N*M,DELTA,1,DELTA,1).NE.ZERO
         IF (ERROR) THEN
            INFO = 50300
            RETURN
         END IF
      END IF

!  Weight the Jacobian wrt the estimated BETAS

      IF (IFIXB(1).LT.0) THEN
         DO 20 K=1,NP
            CALL DWGHT(N,NQ,WE1,LDWE,LD2WE,                            &
                      FJACB(1:N,K,1:NQ),TEMPRET(1:N,1:NQ))
            FJACB(1:N,K,1:NQ) = TEMPRET(1:N,1:NQ)
   20    CONTINUE
      ELSE
         K1 = 0
         DO 30 K=1,NP
            IF (IFIXB(K).GE.1) THEN
               K1 = K1 + 1
               CALL DWGHT(N,NQ,WE1,LDWE,LD2WE,                         &
                          FJACB(1:N,K,1:NQ),TEMPRET(1:N,1:NQ))
               FJACB(1:N,K1,1:NQ) = TEMPRET(1:N,1:NQ)
            END IF
   30    CONTINUE
      END IF

!  Weight the Jacobian's wrt DELTA as appropriate

      IF (ISODR) THEN
         DO 40 J=1,M
            CALL DWGHT(N,NQ,WE1,LDWE,LD2WE,                            &
                      FJACD(1:N,J,1:NQ),TEMPRET(1:N,1:NQ))
            FJACD(1:N,J,1:NQ) = TEMPRET(1:N,1:NQ)
   40    CONTINUE
      END IF

      RETURN
      END SUBROUTINE
!DFCTR
      SUBROUTINE DFCTR(OKSEMI,A,LDA,N,INFO)
!***Begin Prologue  DFCTR
!***Refer to  ODR
!***Routines Called  DDOT
!***Date Written   910706   (YYMMDD)
!***Revision Date  920619   (YYMMDD)
!***Purpose  Factor the positive (semi)definite matrix A using a
!            modified Cholesky factorization
!            (adapted from LINPACK subroutine DPOFA)
!***References  Dongarra J.J., Bunch J.R., Moler C.B., Stewart G.W.,
!                 *LINPACK Users Guide*, SIAM, 1979.
!***End PROLOGUE  DFCTR

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      INTEGER INFO,LDA,N
      LOGICAL OKSEMI

!...Array arguments
      REAL (KIND=R8) A(LDA,N)

!...Local scalars
      REAL (KIND=R8) XI,S,T,TEN,ZERO
      INTEGER J,K

!...External functions
      EXTERNAL DDOT
      REAL (KIND=R8) DDOT
 
      DATA ZERO,TEN /0.0E0_R8,10.0E0_R8/

!...Variable Definitions (alphabetically)
!   A:       The array to be factored.  Upon return, A contains the
!            upper triangular matrix  R  so that  A = trans(R)*R
!            where the strict lower triangle is set to zero
!            if  INFO .NE. 0 , the factorization is not complete.
!   I:       An indexing variable.
!   INFO:    An idicator variable, where if
!            INFO = 0  then factorization was completed
!            INFO = K  signals an error condition.  The leading minor
!                      of order  K  is not positive (semi)definite.
!   J:       An indexing variable.
!   LDA:     The leading dimension of array A.
!   N:       The number of rows and columns of data in array A.
!   OKSEMI:  The indicating whether the factored array can be positive 
!            semidefinite (OKSEMI=TRUE) or whether it must be found to
!            be positive definite (OKSEMI=FALSE).
!   TEN:     The value 10.0E0_R8.
!   XI:      A value used to test for non positive semidefiniteness.
!   ZERO:    The value 0.0E0_R8.


!***First executable statement  DFCTR


!  Set relative tolerance for detecting non positive semidefiniteness.
      XI = -TEN*EPSILON(ZERO)

!  Compute factorization, storing in upper triangular portion of A
      DO 20 J=1,N
         INFO = J
         S = ZERO
         DO 10 K=1,J-1
            IF (A(K,K).EQ.ZERO) THEN
               T      = ZERO
            ELSE
               T      = A(K,J) - DDOT(K-1,A(1,K),1,A(1,J),1)
               T      = T/A(K,K)
            END IF
            A(K,J) = T
            S      = S + T*T
   10    CONTINUE
         S = A(J,J) - S
!     ......Exit
         IF (A(J,J).LT.ZERO .OR. S.LT.XI*ABS(A(J,J))) THEN
            RETURN
         ELSE IF (.NOT.OKSEMI .AND. S.LE.ZERO) THEN
            RETURN
         ELSE IF (S.LE.ZERO) THEN
            A(J,J) = ZERO
         ELSE
            A(J,J) = SQRT(S)
         END IF
   20 CONTINUE
      INFO = 0

!  Zero out lower portion of A
      DO 40 J=2,N
         DO 30 K=1,J-1
            A(J,K) = ZERO
   30    CONTINUE
   40 CONTINUE

      RETURN
      END SUBROUTINE
!DFCTRW
      SUBROUTINE DFCTRW(N,M,NQ,NPP,ISODR,WE,LDWE,LD2WE,WD,LDWD,LD2WD,  &
                        WRK0,WRK4,WE1,NNZW,INFO)
!***Begin Prologue  DFCTRW
!***Refer to  ODR
!***Routines Called  DFCTR
!***Date Written   860529   (YYMMDD)
!***Revision Date  920619   (YYMMDD)
!***Purpose  Check input parameters, indicating errors found using
!            nonzero values of argument INFO as described in the
!            ODRPACK95 reference guide 
!***End Prologue  DFCTRW

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      INTEGER INFO,LDWD,LDWE,LD2WD,LD2WE,M,N,NNZW,NPP,NQ
      LOGICAL ISODR

!...Array arguments
      REAL (KIND=R8) WE(LDWE,LD2WE,NQ),WE1(LDWE,LD2WE,NQ),             &
                     WD(LDWD,LD2WD,M),WRK0(NQ,NQ),WRK4(M,M)

!...Local scalars
      REAL (KIND=R8) ZERO
      INTEGER I,INF,J,J1,J2,L,L1,L2
      LOGICAL NOTZRO

!...External subroutines
      EXTERNAL DFCTR

!...Data statements
      DATA ZERO /0.0E0_R8/

!...Variable Definitions (alphabetically)
!   I:       An indexing variable.
!   INFO:    The variable designating why the computations were stopped.
!   ISODR:   The variable designating whether the solution is by ODR 
!            (ISODR=TRUE) or by OLS (ISODR=FALSE).
!   J:       An indexing variable.
!   J1:      An indexing variable.
!   J2:      An indexing variable.
!   L:       An indexing variable.
!   L1:      An indexing variable.
!   L2:      An indexing variable.
!   LAST:    The last row of the array to be accessed.
!   LDWD:    The leading dimension of array WD.
!   LDWE:    The leading dimension of array WE.
!   LD2WD:   The second dimension of array WD.
!   LD2WE:   The second dimension of array WE.
!   M:       The number of columns of data in the explanatory variable.
!   N:       The number of observations.
!   NNZW:    The number of nonzero weighted observations.
!   NOTZRO:  The variable designating whether a given component of the 
!            weight array WE contains a nonzero element (NOTZRO=FALSE) 
!            or not (NOTZRO=TRUE).
!   NPP:     The number of function parameters being estimated.
!   NQ:      The number of responses per observations.
!   WE:      The (squared) EPSILON weights.
!   WE1:     The factored EPSILON weights, S.T. trans(WE1)*WE1 = WE.
!   WD:      The (squared) DELTA weights.
!   WRK0:    A work array of (NQ BY NQ) elements.
!   WRK4:    A work array of (M BY M) elements.
!   ZERO:    The value 0.0E0_R8.


!***First executable statement  DFCTRW


!  Check EPSILON weights, and store factorization in WE1

      IF (WE(1,1,1).LT.ZERO) THEN
!  WE contains a scalar
         WE1(1,1,1) = -SQRT(ABS(WE(1,1,1)))
         NNZW = N

      ELSE
         NNZW = 0

         IF (LDWE.EQ.1) THEN

            IF (LD2WE.EQ.1) THEN
!  WE contains a diagonal matrix
               DO 110 L=1,NQ
                  IF (WE(1,1,L).GT.ZERO) THEN
                     NNZW = N
                     WE1(1,1,L) = SQRT(WE(1,1,L))
                  ELSE IF (WE(1,1,L).LT.ZERO) THEN
                     INFO = 30010
                     GO TO 300
                  END IF
  110          CONTINUE
            ELSE

!  WE contains a full NQ by NQ semidefinite matrix 
               DO 130 L1=1,NQ
                  DO 120 L2=L1,NQ
                     WRK0(L1,L2) = WE(1,L1,L2)
  120             CONTINUE
  130          CONTINUE
               CALL DFCTR(.TRUE.,WRK0,NQ,NQ,INF)
               IF (INF.NE.0) THEN
                  INFO = 30010
                  GO TO 300
               ELSE
                  DO 150 L1=1,NQ
                     DO 140 L2=1,NQ
                        WE1(1,L1,L2) = WRK0(L1,L2)
  140                CONTINUE
                     IF (WE1(1,L1,L1).NE.ZERO) THEN
                        NNZW = N
                     END IF
  150             CONTINUE
               END IF
            END IF

         ELSE

            IF (LD2WE.EQ.1) THEN
!  WE contains an array of  diagonal matrix
               DO 220 I=1,N
                  NOTZRO = .FALSE.
                  DO 210 L=1,NQ
                     IF (WE(I,1,L).GT.ZERO) THEN
                        NOTZRO = .TRUE.
                        WE1(I,1,L) = SQRT(WE(I,1,L))
                     ELSE IF (WE(I,1,L).LT.ZERO) THEN
                        INFO = 30010
                        GO TO 300
                     END IF
  210             CONTINUE
                  IF (NOTZRO) THEN
                     NNZW = NNZW + 1
                  END IF
  220          CONTINUE
            ELSE

!  WE contains an array of full NQ by NQ semidefinite matrices 
               DO 270 I=1,N
                  DO 240 L1=1,NQ
                     DO 230 L2=L1,NQ
                        WRK0(L1,L2) = WE(I,L1,L2)
  230                CONTINUE
  240             CONTINUE
                  CALL DFCTR(.TRUE.,WRK0,NQ,NQ,INF)
                  IF (INF.NE.0) THEN
                     INFO = 30010
                     GO TO 300
                  ELSE
                     NOTZRO = .FALSE.
                     DO 260 L1=1,NQ
                        DO 250 L2=1,NQ
                           WE1(I,L1,L2) = WRK0(L1,L2)
  250                   CONTINUE
                        IF (WE1(I,L1,L1).NE.ZERO) THEN
                           NOTZRO = .TRUE.
                        END IF
  260                CONTINUE
                  END IF
                  IF (NOTZRO) THEN
                     NNZW = NNZW + 1
                  END IF
  270          CONTINUE
            END IF
         END IF
      END IF

!  Check for a sufficient number of nonzero EPSILON weights

      IF (NNZW.LT.NPP) THEN
         INFO = 30020
      END IF


!  Check DELTA weights

  300 CONTINUE
      IF (.NOT.ISODR .OR. WD(1,1,1).LT.ZERO) THEN
!  Problem is not ODR, or WD contains a scalar
         RETURN

      ELSE

         IF (LDWD.EQ.1) THEN

            IF (LD2WD.EQ.1) THEN
!  WD contains a diagonal matrix
               DO 310 J=1,M
                  IF (WD(1,1,J).LE.ZERO) THEN
                     INFO = MAX(30001,INFO+1)
                     RETURN
                  END IF
  310          CONTINUE
            ELSE

!  WD contains a full M by M positive definite matrix 
               DO 330 J1=1,M
                  DO 320 J2=J1,M
                     WRK4(J1,J2) = WD(1,J1,J2)
  320             CONTINUE
  330          CONTINUE
               CALL DFCTR(.FALSE.,WRK4,M,M,INF)
               IF (INF.NE.0) THEN
                  INFO = MAX(30001,INFO+1)
                  RETURN
               END IF
            END IF

         ELSE

            IF (LD2WD.EQ.1) THEN
!  WD contains an array of diagonal matrices
               DO 420 I=1,N
                  DO 410 J=1,M
                     IF (WD(I,1,J).LE.ZERO) THEN
                        INFO = MAX(30001,INFO+1)
                        RETURN
                     END IF
  410             CONTINUE
  420          CONTINUE
            ELSE

!  WD contains an array of full M by M positive definite matrices 
               DO 470 I=1,N
                  DO 440 J1=1,M
                     DO 430 J2=J1,M
                        WRK4(J1,J2) = WD(I,J1,J2)
  430                CONTINUE
  440             CONTINUE
                  CALL DFCTR(.FALSE.,WRK4,M,M,INF)
                  IF (INF.NE.0) THEN
                     INFO = MAX(30001,INFO+1)
                     RETURN
                  END IF
  470          CONTINUE
            END IF
         END IF
      END IF

      RETURN
      END SUBROUTINE
!DFLAGS
      SUBROUTINE DFLAGS(JOB,RESTRT,INITD,DOVCV,REDOJ,ANAJAC,CDJAC,     &
                        CHKJAC,ISODR,IMPLCT)
!***Begin Prologue  DFLAGS
!***Refer to  ODR
!***Routines Called  (None)
!***Date Written   860529   (YYMMDD)
!***Revision Date  920304   (YYMMDD)
!***Purpose  Set flags indicating conditions specified by JOB
!***End Prologue  DFLAGS

!...Scalar arguments
      INTEGER JOB
      LOGICAL ANAJAC,CDJAC,CHKJAC,DOVCV,IMPLCT,INITD,ISODR,REDOJ,RESTRT

!...Local scalars
      INTEGER J

!...Variable Definitions (alphabetically)
!   ANAJAC:  The variable designating whether the Jacobians are computed
!            by finite differences (ANAJAC=FALSE) or not (ANAJAC=TRUE).
!   CDJAC:   The variable designating whether the Jacobians are computed
!            by central differences (CDJAC=TRUE) or by forward 
!            differences (CDJAC=FALSE).
!   CHKJAC:  The variable designating whether the user-supplied 
!            Jacobians are to be checked (CHKJAC=TRUE) or not 
!            (CHKJAC=FALSE).
!   DOVCV:   The variable designating whether the covariance matrix is 
!            to be computed (DOVCV=TRUE) or not (DOVCV=FALSE).
!   IMPLCT:  The variable designating whether the solution is by 
!            implicit ODR (IMPLCT=TRUE) or explicit ODR (IMPLCT=FALSE).
!   INITD:   The variable designating whether DELTA is to be initialized
!            to zero (INITD=TRUE) or to the first N by M elements of 
!            array WORK (INITD=FALSE).
!   ISODR:   The variable designating whether the solution is by ODR 
!            (ISODR=TRUE) or by OLS (ISODR=FALSE).
!   J:       The value of a specific digit of JOB.
!   JOB:     The variable controling problem initialization and 
!            computational method.
!   REDOJ:   The variable designating whether the Jacobian matrix is to
!            be recomputed for the computation of the covariance matrix 
!            (REDOJ=TRUE) or not (REDOJ=FALSE).
!   RESTRT:  The variable designating whether the call is a restart 
!            (RESTRT=TRUE) or not (RESTRT=FALSE).


!***First executable statement  DFLAGS


      IF (JOB.GE.0) THEN

         RESTRT= JOB.GE.10000

         INITD = MOD(JOB,10000)/1000.EQ.0

         J = MOD(JOB,1000)/100
         IF (J.EQ.0) THEN
            DOVCV = .TRUE.
            REDOJ = .TRUE.
         ELSE IF (J.EQ.1) THEN
            DOVCV = .TRUE.
            REDOJ = .FALSE.
         ELSE
            DOVCV = .FALSE.
            REDOJ = .FALSE.
         END IF

         J = MOD(JOB,100)/10
         IF (J.EQ.0) THEN
            ANAJAC = .FALSE.
            CDJAC  = .FALSE.
            CHKJAC = .FALSE.
         ELSE IF (J.EQ.1) THEN
            ANAJAC = .FALSE.
            CDJAC  = .TRUE.
            CHKJAC = .FALSE.
         ELSE IF (J.EQ.2) THEN
            ANAJAC = .TRUE.
            CDJAC  = .FALSE.
            CHKJAC = .TRUE.
         ELSE
            ANAJAC = .TRUE.
            CDJAC  = .FALSE.
            CHKJAC = .FALSE.
         END IF

         J = MOD(JOB,10)
         IF (J.EQ.0) THEN
            ISODR  = .TRUE.
            IMPLCT = .FALSE.
         ELSE IF (J.EQ.1) THEN
            ISODR  = .TRUE.
            IMPLCT = .TRUE.
         ELSE 
            ISODR  = .FALSE.
            IMPLCT = .FALSE.
         END IF

      ELSE

         RESTRT  = .FALSE.
         INITD   = .TRUE.
         DOVCV   = .TRUE.
         REDOJ   = .TRUE.
         ANAJAC  = .FALSE.
         CDJAC   = .FALSE.
         CHKJAC  = .FALSE.
         ISODR   = .TRUE.
         IMPLCT  = .FALSE.

      END IF

      RETURN
      END SUBROUTINE
!DHSTEP
      FUNCTION DHSTEP (ITYPE,NETA,I,J,STP,LDSTP) RESULT(DHSTEPR)
!***Begin Prologue  DHSTEP
!***Refer to  ODR
!***Routines Called  (NONE)
!***Date Written   860529   (YYMMDD)
!***Revision Date  920304   (YYMMDD)
!***Purpose  Set relative step size for finite difference derivatives
!***End Prologue  DHSTEP

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      INTEGER I,ITYPE,J,LDSTP,NETA

!...Array arguments
      REAL (KIND=R8) STP(LDSTP,J)

!...Result
      REAL (KIND=R8) DHSTEPR

!...Local scalars
      REAL (KIND=R8) TEN,THREE,TWO,ZERO
 
!...Data statements
      DATA ZERO,TWO,THREE,TEN /0.0E0_R8,2.0E0_R8,3.0E0_R8,10.0E0_R8/
 
!...Variable Definitions (alphabetically)
!   I:       An identifier for selecting user supplied step sizes.
!   ITYPE:   The finite difference method being used, where
!            ITYPE = 0 indicates forward finite differences, and
!            ITYPE = 1 indicates central finite differences.
!   J:       An identifier for selecting user supplied step sizes.
!   LDSTP:   The leading dimension of array STP.
!   NETA:    The number of good digits in the function results.
!   STP:     The step size for the finite difference derivative.
!   TEN:     The value 10.0E0_R8.
!   THREE:   The value 3.0E0_R8.
!   TWO:     The value 2.0E0_R8.
!   ZERO:    The value 0.0E0_R8.



!***First executable statement  DHSTEP


!  Set DHSTEP to relative finite difference step size

      IF (STP(1,1).LE.ZERO) THEN

         IF (ITYPE.EQ.0) THEN
!  Use default forward finite difference step size
            DHSTEPR = TEN**(-ABS(NETA)/TWO - TWO)

         ELSE
!  Use default central finite difference step size
            DHSTEPR = TEN**(-ABS(NETA)/THREE)
         END IF

      ELSE IF (LDSTP.EQ.1) THEN
         DHSTEPR = STP(1,J)

      ELSE
         DHSTEPR = STP(I,J)
      END IF

      RETURN
      END FUNCTION
!DIFIX
      SUBROUTINE DIFIX (N,M,IFIX,LDIFIX,T,LDT,TFIX,LDTFIX)
!***Begin Prologue  DIFIX
!***Refer to  ODR
!***Routines Called  (None)
!***Date Written   910612   (YYMMDD)
!***Revision Date  920304   (YYMMDD)
!***Purpose  Set elements of T to zero according to IFIX
!***End Prologue  DIFIX

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      INTEGER LDIFIX,LDT,LDTFIX,M,N

!...Array arguments
      REAL (KIND=R8) T(LDT,M),TFIX(LDTFIX,M)
      INTEGER IFIX(LDIFIX,M)

!...Local scalars
      REAL (KIND=R8) ZERO
      INTEGER I,J

!...Data statements
      DATA ZERO /0.0E0_R8/

!...Variable Definitions (alphabetically)
!   I:       An indexing variable.
!   IFIX:    The array designating whether an element of T is to be
!            set to zero.
!   J:       an indexing variable.
!   LDT:     The leading dimension of array T.
!   LDIFIX:  The leading dimension of array IFIX.
!   LDTFIX:  The leading dimension of array TFIX.
!   M:       The number of columns of data in the array.
!   N:       The number of rows of data in the array.
!   T:       The array being set to zero according to the elements 
!            of IFIX.
!   TFIX:    The resulting array.
!   ZERO:    The value 0.0E0_R8.


!***First executable statement  DIFIX


      IF (N.EQ.0 .OR. M.EQ.0) RETURN

      IF (IFIX(1,1).GE.ZERO) THEN
         IF (LDIFIX.GE.N) THEN
            DO 20 J=1,M
               DO 10 I=1,N
                  IF (IFIX(I,J).EQ.0) THEN
                     TFIX(I,J) = ZERO
                  ELSE
                     TFIX(I,J) = T(I,J)
                  END IF
   10          CONTINUE
   20       CONTINUE
         ELSE
            DO 100 J=1,M
               IF (IFIX(1,J).EQ.0) THEN
                  DO 30 I=1,N
                     TFIX(I,J) = ZERO
   30             CONTINUE
               ELSE
                  DO 90 I=1,N
                     TFIX(I,J) = T(I,J)
   90             CONTINUE
               END IF
  100       CONTINUE
         END IF
      END IF

      RETURN
      END SUBROUTINE
!DINIWK
      SUBROUTINE DINIWK(N,M,NP,WORK,LWORK,IWORK,LIWORK,                &
                        X,LDX,IFIXX,LDIFX,SCLD,LDSCLD,BETA,SCLB,       &
                        SSTOL,PARTOL,MAXIT,TAUFAC,                     &
                        JOB,IPRINT,LUNERR,LUNRPT,LOWER,UPPER,          &
                        EPSMAI,SSTOLI,PARTLI,MAXITI,TAUFCI,            &
                        JOBI,IPRINI,LUNERI,LUNRPI,                     &
                        SSFI,TTI,LDTTI,DELTAI,LOWERI,UPPERI,BOUNDI)
!***Begin Prologue  DINIWK
!***Refer to  ODR
!***Routines Called  DFLAGS,DSCLB,DSCLD,DZERO
!***Date Written   860529   (YYMMDD)
!***Revision Date  920304   (YYMMDD)
!***Purpose  Initialize work vectors as necessary
!***End Prologue  DINIWK

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      REAL (KIND=R8) PARTOL,SSTOL,TAUFAC
      INTEGER BOUNDI,DELTAI,EPSMAI,IPRINI,IPRINT,JOB,JOBI,LDIFX,       &
              LDSCLD,LDTTI,LDX,LIWORK,LOWERI,LUNERI,LUNERR,LUNRPI,     &
              LUNRPT,LWORK,M,MAXIT,MAXITI,N,NP,PARTLI,SSFI,SSTOLI,     &
              TAUFCI,TTI,UPPERI

!...Array arguments
      REAL (KIND=R8) BETA(NP),LOWER(NP),SCLB(NP),SCLD(LDSCLD,M),       &
                     UPPER(NP),WORK(LWORK),X(LDX,M)
      INTEGER IFIXX(LDIFX,M),IWORK(LIWORK)

!...Local scalars
      REAL (KIND=R8) ONE,THREE,TWO,ZERO
      INTEGER I,J 
      LOGICAL ANAJAC,CDJAC,CHKJAC,DOVCV,IMPLCT,INITD,ISODR,REDOJ,RESTRT

!...External functions

!...External subroutines
      EXTERNAL DCOPY,DFLAGS,DSCLB,DSCLD,DZERO

!...Data statements
      DATA ZERO,ONE,TWO,THREE /0.0E0_R8,1.0E0_R8,2.0E0_R8,3.0E0_R8/

!...Variable Definitions (alphabetically)
!   ANAJAC:  The variable designating whether the Jacobians are 
!            computed by finite differences (ANAJAC=FALSE) or not
!            (ANAJAC=TRUE).
!   BETA:    The function parameters.
!   CDJAC:   The variable designating whether the Jacobians are 
!            computed by central differences (CDJAC=TRUE) or by forward
!            differences (CDJAC=FALSE).
!   CHKJAC:  The variable designating whether the user-supplied 
!            Jacobians are to be checked (CHKJAC=TRUE) or not
!            (CHKJAC=FALSE).
!   DELTAI:  The starting location in array WORK of array DELTA.
!   DOVCV:   The variable designating whether the covariance matrix is 
!            to be computed (DOVCV=TRUE) or not (DOVCV=FALSE).
!   EPSMAI:  The location in array WORK of variable EPSMAC.
!   I:       An indexing variable.
!   IFIXX:   The values designating whether the elements of X are fixed 
!            at their input values or not.
!   IMPLCT:  The variable designating whether the solution is by 
!            implicit ODR (IMPLCT=TRUE) or explicit ODR (IMPLCT=FALSE).
!   INITD:   The variable designating whether DELTA is to be initialized
!            to zero (INITD=TRUE) or to the values in the first N by M
!            elements of array WORK (INITD=FALSE).
!   IPRINI:  The location in array IWORK of variable IPRINT.
!   IPRINT:  The print control variable.
!   ISODR:   The variable designating whether the solution is by ODR 
!            (ISODR=TRUE) or by OLS (ISODR=FALSE).
!   IWORK:   The integer work space.
!   J:       An indexing variable.
!   JOB:     The variable controling problem initialization and 
!            computational method.
!   JOBI:    The location in array IWORK of variable JOB.
!   LDIFX:   The leading dimension of array IFIXX.
!   LDSCLD:  The leading dimension of array SCLD.
!   LDTTI:   The leading dimension of array TT.
!   LDX:     The leading dimension of array X.
!   LIWORK:  The length of vector IWORK.
!   LUNERI:  The location in array IWORK of variable LUNERR.
!   LUNERR:  The logical unit number used for error messages.
!   LUNRPI:  The location in array iwork of variable LUNRPT.
!   LUNRPT:  The logical unit number used for computation reports.
!   LWORK:   The length of vector WORK.
!   M:       The number of columns of data in the independent variable.
!   MAXIT:   The maximum number of iterations allowed.
!   MAXITI:  The location in array IWORK of variable MAXIT.
!   N:       The number of observations.
!   NP:      The number of function parameters.
!   ONE:     The value 1.0E0_R8.
!   PARTLI:  The location in array work of variable partol.
!   PARTOL:  The parameter convergence stopping criteria.
!   REDOJ:   The variable designating whether the Jacobian matrix is to 
!            be recomputed for the computation of the covariance matrix 
!            (REDOJ=TRUE) or not (REDOJ=FALSE).
!   RESTRT:  The variable designating whether the call is a restart 
!            (RESTRT=TRUE) or not (RESTRT=FALSE).
!   SCLB:    The scaling values for BETA.
!   SCLD:    The scaling values for DELTA.
!   SSFI:    The starting location in array WORK of array SSF.
!   SSTOL:   The sum-of-squares convergence stopping criteria.
!   SSTOLI:  The location in array WORK of variable SSTOL.
!   TAUFAC:  The factor used to compute the initial trust region 
!            diameter.
!   TAUFCI:  The location in array WORK of variable TAUFAC.
!   THREE:   The value 3.0E0_R8.
!   TTI:     The starting location in array WORK of the ARRAY TT.
!   TWO:     The value 2.0E0_R8.
!   WORK:    The REAL (KIND=R8) work space.
!   X:       The independent variable.
!   ZERO:    The value 0.0E0_R8.


!***First executable statement  DINIWK


      CALL DFLAGS(JOB,RESTRT,INITD,DOVCV,REDOJ,                        &
                   ANAJAC,CDJAC,CHKJAC,ISODR,IMPLCT)

!  Store value of machine precision in work vector

      WORK(EPSMAI) = EPSILON(ZERO)

!  Set tolerance for stopping criteria based on the change in the
!  parameters  (see also subprogram DODCNT)

      IF (PARTOL.LT.ZERO) THEN
         WORK(PARTLI) = WORK(EPSMAI)**(TWO/THREE)
      ELSE
         WORK(PARTLI) = MIN(PARTOL, ONE)
      END IF

!  Set tolerance for stopping criteria based on the change in the
!  sum of squares of the weighted observational errors

      IF (SSTOL.LT.ZERO) THEN
         WORK(SSTOLI) = SQRT(WORK(EPSMAI))
      ELSE
         WORK(SSTOLI) = MIN(SSTOL, ONE)
      END IF

!  Set factor for computing trust region diameter at first iteration

      IF (TAUFAC.LE.ZERO) THEN
         WORK(TAUFCI) = ONE
      ELSE
         WORK(TAUFCI) = MIN(TAUFAC, ONE)
      END IF

!  Set maximum number of iterations

      IF (MAXIT.LT.0) THEN
         IWORK(MAXITI) = 50
      ELSE
         IWORK(MAXITI) = MAXIT
      END IF

!  Store problem initialization and computational method control
!  variable

      IF (JOB.LE.0) THEN
         IWORK(JOBI) = 0
      ELSE
         IWORK(JOBI) = JOB
      END IF

!  Set print control

      IF (IPRINT.LT.0) THEN
         IWORK(IPRINI) = 2001
      ELSE
         IWORK(IPRINI) = IPRINT
      END IF

!  Set logical unit number for error messages

      IF (LUNERR.LT.0) THEN
         IWORK(LUNERI) = 6
      ELSE
         IWORK(LUNERI) = LUNERR
      END IF

!  Set logical unit number for computation reports

      IF (LUNRPT.LT.0) THEN
         IWORK(LUNRPI) = 6
      ELSE
         IWORK(LUNRPI) = LUNRPT
      END IF

!  Compute scaling for BETA's and DELTA's

      IF (SCLB(1).LE.ZERO) THEN
         CALL DSCLB(NP,BETA,WORK(SSFI))
      ELSE
         CALL DCOPY(NP,SCLB,1,WORK(SSFI),1)
      END IF
      IF (ISODR) THEN
         IF (SCLD(1,1).LE.ZERO) THEN
            IWORK(LDTTI) = N
            CALL DSCLD(N,M,X,LDX,WORK(TTI),IWORK(LDTTI))
         ELSE
            IF (LDSCLD.EQ.1) THEN
               IWORK(LDTTI) = 1
               CALL DCOPY(M,SCLD(1,1),1,WORK(TTI),1)
            ELSE
               IWORK(LDTTI) = N
               DO 10 J=1,M
                  CALL DCOPY(N,SCLD(1,J),1,                            &
                             WORK(TTI+(J-1)*IWORK(LDTTI)),1)
   10          CONTINUE
            END IF
         END IF
      END IF

!  Initialize DELTA's as necessary

      IF (ISODR) THEN
         IF (INITD) THEN
            CALL DZERO(N,M,WORK(DELTAI),N)
         ELSE
            IF (IFIXX(1,1).GE.0) THEN
               IF (LDIFX.EQ.1) THEN
                  DO 20 J=1,M
                     IF (IFIXX(1,J).EQ.0) THEN
                        CALL DZERO(N,1,WORK(DELTAI+(J-1)*N),N)
                     END IF
   20             CONTINUE
               ELSE
                  DO 40 J=1,M
                     DO 30 I=1,N
                        IF (IFIXX(I,J).EQ.0) THEN
                           WORK(DELTAI-1+I+(J-1)*N) = ZERO
                        END IF
   30                CONTINUE
   40             CONTINUE
               END IF
            END IF
         END IF
      ELSE
         CALL DZERO(N,M,WORK(DELTAI),N)
      END IF

!  Copy bounds into WORK

      WORK(LOWERI:LOWERI+NP-1) = LOWER(1:NP)
      WORK(UPPERI:UPPERI+NP-1) = UPPER(1:NP)

!  Initialize parameters on bounds in IWORK

      IWORK(BOUNDI:BOUNDI+NP-1) = 0

      RETURN
      END SUBROUTINE
!DIWINF
      SUBROUTINE DIWINF(M,NP,NQ,MSGBI,MSGDI,IFIX2I,ISTOPI,             &
                        NNZWI,NPPI,IDFI,JOBI,IPRINI,LUNERI,LUNRPI,     &
                        NROWI,NTOLI,NETAI,                             &
                        MAXITI,NITERI,NFEVI,NJEVI,INT2I,IRANKI,LDTTI,  &
                        BOUNDI,LIWKMN)
!***Begin Prologue  DIWINF
!***Refer to  ODR
!***Routines Called  (NONE)
!***Date Written   860529   (YYMMDD)
!***Revision Date  920304   (YYMMDD)
!***Purpose  Set storage locations within integer work space
!***End Prologue  DIWINF

!...Scalar arguments
      INTEGER BOUNDI,IDFI,INT2I,IPRINI,IRANKI,ISTOPI,JOBI,IFIX2I,      &
              LDTTI,LIWKMN,LUNERI,LUNRPI,M,MAXITI,MSGBI,MSGDI,NETAI,   &
              NFEVI,NITERI,NJEVI,NNZWI,NP,NPPI,NQ,NROWI,NTOLI

!...Variable Definitions (alphabetically)
!   IDFI:    The location in array IWORK of variable IDF.
!   IFIX2I:  The starting location in array IWORK of array IFIX2.
!   INT2I:   The location in array IWORK of variable INT2.
!   IPRINI:  The location in array IWORK of variable IPRINT.
!   IRANKI:  The location in array IWORK of variable IRANK.
!   ISTOPI:  The location in array IWORK of variable ISTOP.
!   JOBI:    The location in array IWORK of variable JOB.
!   LDTTI:   The location in array IWORK of variable LDTT.
!   LIWKMN:  The minimum acceptable length of array IWORK.
!   LUNERI:  The location in array IWORK of variable LUNERR.
!   LUNRPI:  The location in array IWORK of variable LUNRPT.
!   M:       The number of columns of data in the independent variable.
!   MAXITI:  The location in array iwork of variable MAXIT.
!   MSGBI:   The starting location in array IWORK of array MSGB.
!   MSGDI:   The starting location in array IWORK of array MSGD.
!   NETAI:   The location in array IWORK of variable NETA.
!   NFEVI:   The location in array IWORK of variable NFEV.
!   NITERI:  The location in array IWORK of variabel NITER.
!   NJEVI:   The location in array IWORK of variable NJEV.
!   NNZWI:   The location in array IWORK of variable NNZW.
!   NP:      The number of function parameters.
!   NPPI:    The location in array IWORK of variable NPP.
!   NQ:      The number of responses per observation.
!   NROWI:   The location in array IWORK of variable NROW.
!   NTOLI:   The location in array IWORK of variable NTOL.


!***First executable statement  DIWINF


      IF (NP.GE.1 .AND. M.GE.1) THEN
         MSGBI  = 1
         MSGDI  = MSGBI  + NQ*NP+1
         IFIX2I = MSGDI  + NQ*M+1
         ISTOPI = IFIX2I + NP
         NNZWI  = ISTOPI + 1
         NPPI   = NNZWI  + 1
         IDFI   = NPPI   + 1
         JOBI   = IDFI   + 1
         IPRINI = JOBI   + 1
         LUNERI = IPRINI + 1
         LUNRPI = LUNERI + 1
         NROWI  = LUNRPI + 1
         NTOLI  = NROWI  + 1
         NETAI  = NTOLI  + 1
         MAXITI = NETAI  + 1
         NITERI = MAXITI + 1
         NFEVI  = NITERI + 1
         NJEVI  = NFEVI  + 1
         INT2I  = NJEVI  + 1
         IRANKI = INT2I  + 1
         LDTTI  = IRANKI + 1
         BOUNDI = LDTTI  + 1
         LIWKMN = BOUNDI + NP - 1
      ELSE
         MSGBI  = 1
         MSGDI  = 1
         IFIX2I = 1
         ISTOPI = 1
         NNZWI  = 1
         NPPI   = 1
         IDFI   = 1
         JOBI   = 1
         IPRINI = 1
         LUNERI = 1
         LUNRPI = 1
         NROWI  = 1
         NTOLI  = 1
         NETAI  = 1
         MAXITI = 1
         NITERI = 1
         NFEVI  = 1
         NJEVI  = 1
         INT2I  = 1
         IRANKI = 1
         LDTTI  = 1
         BOUNDI = 1
         LIWKMN = 1
      END IF

      RETURN
      END SUBROUTINE
!DJACCD
      SUBROUTINE DJACCD(FCN,N,M,NP,NQ,                                 &
                        BETA,X,LDX,DELTA,XPLUSD,IFIXB,IFIXX,LDIFX,     &
                        STPB,STPD,LDSTPD,                              &
                        SSF,TT,LDTT,NETA,FN,STP,WRK1,WRK2,WRK3,WRK6,   &
                        FJACB,ISODR,FJACD,NFEV,ISTOP,INFO,LOWER,UPPER)
!***Begin Prologue  DJACCD
!***Refer to  ODR
!***Routines Called  FCN,DHSTEP,DZERO
!***Date Written   860529   (YYMMDD)
!***Revision Date  920619   (YYMMDD)
!***Purpose  Compute central difference approximations to the
!            Jacobian wrt the estimated BETAS and wrt the DELTAS
!***End Prologue  DJACCD

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      INTEGER INFO,ISTOP,LDIFX,LDSTPD,LDTT,LDX,M,N,NETA,NFEV,NP,NQ
      LOGICAL ISODR

!...Array arguments
      REAL (KIND=R8) BETA(NP),DELTA(N,M),FJACB(N,NP,NQ),FJACD(N,M,NQ), &
                     FN(N,NQ),LOWER(NP),                               &
                     SSF(NP),STP(N),STPB(NP),STPD(LDSTPD,M),TT(LDTT,M),&
                     UPPER(NP),WRK1(N,M,NQ),WRK2(N,NQ),WRK3(NP),       &
                     WRK6(N,NP,NQ),X(LDX,M),XPLUSD(N,M)
      INTEGER IFIXB(NP),IFIXX(LDIFX,M)

!...Subroutine arguments
      EXTERNAL FCN

!...Local scalars
      REAL (KIND=R8) BETAK,ONE,TYPJ,ZERO
      INTEGER I,J,K,L
      LOGICAL DOIT,SETZRO

!...External subroutines
      EXTERNAL DZERO

!...External functions
      REAL (KIND=R8) DHSTEP,DERSTEP
      EXTERNAL DHSTEP,DERSTEP

!...Data statements
      DATA ZERO,ONE /0.0E0_R8,1.0E0_R8/

!...Routine names used as subprogram arguments
!   FCN:     The user supplied subroutine for evaluating the model.

!...Variable Definitions (alphabetically)
!   BETA:    The function parameters.
!   BETAK:   The K-th function parameter.
!   DELTA:   The estimated errors in the explanatory variables.
!   DOIT:    The variable designating whether the derivative wrt a given
!            BETA or DELTA needs to be computed (DOIT=TRUE) or not 
!            (DOIT=FALSE).
!   FJACB:   The Jacobian with respect to BETA.
!   FJACD:   The Jacobian with respect to DELTA.
!   FN:      The new predicted values from the function.  Used when parameter is
!            on a boundary.
!   I:       An indexing variable.
!   IFIXB:   The values designating whether the elements of BETA are 
!            fixed at their input values or not.
!   IFIXX:   The values designating whether the elements of X are fixed 
!            at their input values or not.
!   INFO:    The variable designating why the computations were stopped.
!   ISODR:   The variable designating whether the solution is by ODR
!            (ISODR=TRUE) or by OLS (ISODR=FALSE).
!   ISTOP:   The variable designating whether there are problems
!            computing the function at the current BETA and DELTA.
!   J:       An indexing variable.
!   K:       An indexing variable.
!   L:       An indexing variable.
!   LDIFX:   The leading dimension of array IFIXX.
!   LDSTPD:  The leading dimension of array STPD.
!   LDTT:    The leading dimension of array TT.
!   LDX:     The leading dimension of array X.
!   LOWER:   The lower bound on BETA.
!   M:       The number of columns of data in the explanatory variable.
!   N:       The number of observations.
!   NETA:    The number of good digits in the function results.
!   NFEV:    The number of function evaluations.
!   NP:      The number of function parameters.
!   ONE:     The value 1.0E0_R8.
!   SETZRO:  The variable designating whether the derivative wrt some 
!            DELTA needs to be set to zero (SETZRO=TRUE) or not
!            (SETZRO=FALSE).
!   SSF:     The scaling values used for BETA.
!   STP:     The step used for computing finite difference
!            derivatives with respect to each DELTA.
!   STPB:    the relative step used for computing finite difference
!            derivatives with respect to each BETA.
!   STPD:    The relative step used for computing finite difference
!            derivatives with respect to each DELTA.
!   TT:      The scaling values used for DELTA.
!   TYPJ:    The typical size of the J-th unknown BETA or DELTA.
!   UPPER:   The upper bound on BETA.
!   X:       The explanatory variable.
!   XPLUSD:  The values of X + DELTA.
!   WRK1:    A work array of (N BY M BY NQ) elements.
!   WRK2:    A work array of (N BY NQ) elements.
!   WRK3:    A work array of (NP) elements.
!   WRK6:    A WORK ARRAY OF (N BY NP BY NQ) elements.
!   ZERO:    The value 0.0E0_R8.


!***First executable statement  DJACCD


!  Compute the Jacobian wrt the estimated BETAS

      DO 60 K=1,NP
         IF (IFIXB(1).GE.0) THEN
            IF (IFIXB(K).EQ.0) THEN
               DOIT = .FALSE.
            ELSE
               DOIT = .TRUE.
            END IF
         ELSE
            DOIT = .TRUE.
         END IF
         IF (.NOT.DOIT) THEN
            DO 10 L=1,NQ
               CALL DZERO(N,1,FJACB(1,K,L),N)
   10       CONTINUE
         ELSE
            BETAK = BETA(K)
            WRK3(K) = BETAK + DERSTEP(1,K,BETAK,SSF,STPB,NETA)
            WRK3(K) = WRK3(K) - BETAK

            BETA(K) = BETAK + WRK3(K)
            IF (BETA(K).GT.UPPER(K)) THEN
               BETA(K) = UPPER(K)
            ELSE IF (BETA(K).LT.LOWER(K)) THEN
               BETA(K) = LOWER(K)
            END IF
            IF (BETA(K)-2*WRK3(K).LT.LOWER(K)) THEN
               BETA(K) = LOWER(K) + 2*WRK3(K)
            ELSE IF (BETA(K)-2*WRK3(K).GT.UPPER(K)) THEN
               BETA(K) = UPPER(K) + 2*WRK3(K)
            END IF
            IF (BETA(K).GT.UPPER(K).OR.BETA(K).LT.LOWER(K)) THEN
               INFO = 60001
               RETURN
            END IF
            ISTOP = 0
            IF (BETA(K).EQ.BETAK) THEN
               WRK2(1:N,1:NQ) = FN(1:N,1:NQ)
            ELSE
               CALL FCN(N,M,NP,NQ,N,M,NP,BETA,XPLUSD,                  &
                        IFIXB,IFIXX,LDIFX,001,WRK2,WRK6,WRK1,ISTOP)
               IF (ISTOP.NE.0) THEN
                  RETURN
               ELSE
                  NFEV = NFEV + 1
               END IF
            END IF
            DO 30 L=1,NQ
               DO 20 I=1,N
                  FJACB(I,K,L) = WRK2(I,L)
   20          CONTINUE
   30       CONTINUE

            BETA(K) = BETA(K) - 2*WRK3(K)
            IF (BETA(K).GT.UPPER(K)) THEN
               INFO = 60001
               RETURN
            END IF
            IF (BETA(K).LT.LOWER(K)) THEN
               INFO = 60001
               RETURN
            END IF
            ISTOP = 0
            IF (BETA(K).EQ.BETAK) THEN
               WRK2(1:N,1:NQ) = FN(1:N,1:NQ)
            ELSE
               CALL FCN(N,M,NP,NQ,N,M,NP,BETA,XPLUSD,                  &
                        IFIXB,IFIXX,LDIFX,001,WRK2,WRK6,WRK1,ISTOP)
               IF (ISTOP.NE.0) THEN
                  RETURN
               ELSE
                  NFEV = NFEV + 1
               END IF
            END IF

            DO 50 L=1,NQ
               DO 40 I=1,N
                  FJACB(I,K,L) = (FJACB(I,K,L)-WRK2(I,L))/(2*WRK3(K))
   40          CONTINUE
   50       CONTINUE
            BETA(K) = BETAK
         END IF
   60 CONTINUE

!  Compute the Jacobian wrt the X'S

      IF (ISODR) THEN
         DO 220 J=1,M
            IF (IFIXX(1,1).LT.0) THEN
               DOIT = .TRUE.
               SETZRO = .FALSE.
            ELSE IF (LDIFX.EQ.1) THEN
               IF (IFIXX(1,J).EQ.0) THEN
                  DOIT = .FALSE.
               ELSE
                  DOIT = .TRUE.
               END IF
               SETZRO = .FALSE.
            ELSE
               DOIT = .FALSE.
               SETZRO = .FALSE.
               DO 100 I=1,N
                  IF (IFIXX(I,J).NE.0) THEN
                     DOIT = .TRUE.
                  ELSE
                     SETZRO = .TRUE.
                  END IF
  100          CONTINUE
            END IF
            IF (.NOT.DOIT) THEN
               DO 110 L=1,NQ
                  CALL DZERO(N,1,FJACD(1,J,L),N)
  110          CONTINUE
            ELSE
               DO 120 I=1,N
                  IF (XPLUSD(I,J).EQ.ZERO) THEN
                     IF (TT(1,1).LT.ZERO) THEN
                        TYPJ = ONE/ABS(TT(1,1))
                     ELSE IF (LDTT.EQ.1) THEN
                        TYPJ = ONE/TT(1,J)
                     ELSE
                        TYPJ = ONE/TT(I,J)
                     END IF
                  ELSE
                     TYPJ = ABS(XPLUSD(I,J))
                  END IF
                  STP(I) = XPLUSD(I,J) + SIGN(ONE,XPLUSD(I,J))         &
                             *TYPJ*DHSTEP(1,NETA,I,J,STPD,LDSTPD)
                  STP(I) = STP(I) - XPLUSD(I,J)
                  XPLUSD(I,J) = XPLUSD(I,J) + STP(I)
  120          CONTINUE
               ISTOP = 0
               CALL FCN(N,M,NP,NQ,N,M,NP,BETA,XPLUSD,                  &
                        IFIXB,IFIXX,LDIFX,001,WRK2,WRK6,WRK1,ISTOP)
               IF (ISTOP.NE.0) THEN
                  RETURN
               ELSE
                  NFEV = NFEV + 1
                  DO 140 L=1,NQ
                     DO 130 I=1,N
                        FJACD(I,J,L) = WRK2(I,L)
  130                CONTINUE
  140             CONTINUE
               END IF

               DO 150 I=1,N
                  XPLUSD(I,J) = X(I,J) + DELTA(I,J) - STP(I)
  150          CONTINUE
               ISTOP = 0
               CALL FCN(N,M,NP,NQ,N,M,NP,BETA,XPLUSD,                  &
                        IFIXB,IFIXX,LDIFX,001,WRK2,WRK6,WRK1,ISTOP)
               IF (ISTOP.NE.0) THEN
                  RETURN
               ELSE
                  NFEV = NFEV + 1
               END IF

               IF (SETZRO) THEN
                  DO 180 I=1,N
                     IF (IFIXX(I,J).EQ.0) THEN
                        DO 160 L=1,NQ
                           FJACD(I,J,L) = ZERO
  160                   CONTINUE
                     ELSE
                        DO 170 L=1,NQ
                           FJACD(I,J,L) = (FJACD(I,J,L)-WRK2(I,L))/    &
                                          (2*STP(I))
  170                   CONTINUE
                     END IF
  180             CONTINUE
               ELSE
                  DO 200 L=1,NQ
                     DO 190 I=1,N
                        FJACD(I,J,L) = (FJACD(I,J,L)-WRK2(I,L))/       &
                                       (2*STP(I))
  190                CONTINUE
  200             CONTINUE
               END IF
               DO 210 I=1,N
                  XPLUSD(I,J) = X(I,J) + DELTA(I,J)
  210          CONTINUE
            END IF
  220    CONTINUE
      END IF

      RETURN
      END SUBROUTINE
!MBFB
      SUBROUTINE MBFB(NP,BETA,LOWER,UPPER,SSF,STPB,NETA,ETA,INTERVAL)
!***BEGIN PROLOGUE  MBFB
!***REFER TO  ODR
!***ROUTINES CALLED  DHSTEP
!***DATE WRITTEN   20040624   (YYYYMMDD)
!***REVISION DATE  20040624   (YYYYMMDD)
!***PURPOSE  ENSURE RANGE OF BOUNDS IS LARGE ENOUGH FOR DERIVATIVE CHECKING.
!***         MOVE BETA AWAY FROM BOUNDS SO THAT DERIVATIVES CAN BE CALCULATED.
!***END PROLOGUE  MBFB

!...USED MODULES
      USE REAL_PRECISION

!...SCALAR ARGUMENTS
      INTEGER NETA,NP
      REAL (KIND=R8) ETA

!...ARRAY ARGUMENTS
      INTEGER INTERVAL(NP)
      REAL (KIND=R8) BETA(NP),LOWER(NP),SSF(NP),STPB(NP),UPPER(NP)

!...LOCAL SCALARS
      INTEGER K
      REAL (KIND=R8) H,H0,H1,HC,HC0,HC1,HUNDRED,ONE,STPR,STPL,TEN,     &
                     THREE,TYPJ,ZERO

!...EXTERNAL FUNCTIONS
      REAL (KIND=R8) DHSTEP
      EXTERNAL DHSTEP

!...DATA STATEMENTS
      DATA ZERO,ONE,TEN,HUNDRED,THREE                                  &
           /0.0E0_R8,1.0E0_R8,10.0E0_R8,100.0E0_R8,3.0E0_R8/

!...VARIABLE DEFINITIONS (ALPHABETICALLY)
!   BETA:    BETA for the jacobian checker.  BETA will be moved far enough from
!            the bounds so that the derivative checker may proceed.
!   H:       Relative step size for forward differences.
!   H0:      Initial relative step size for forward differences.
!   H1:      Default relative step size for forward differences.
!   HC:      Relative step size for center differences.
!   HC0:     Initial relative step size for center differences.
!   HC1:     Default relative step size for center differences.
!   HUNDRED: 100.0E0_R8
!   INTERVAL: Specifies which difference methods and step sizes are supported by
!            the current intervale UPPER-LOWER.
!   K:       Index variable for BETA.
!   NETA:    Number of good digits in the function results.
!   ONE:     The value 1.0E0_R8.
!   SSF:     The scale used for the BETA'S.
!   STPB:    The relative step used for computing finite difference derivatives
!            with respect to BETA.
!   STPL:    Maximum step to the left of BETA (-) the derivative checker will
!            use.
!   STPR:    Maximum step to the right of BETA (+) the derivative checker will
!            use.
!   TEN:     10.0E0_R8
!   THREE:   3.0E0_R8
!   TYPJ:    The typical size of the J-th unkonwn BETA.
!   ZERO:    The value 0.0E0_R8.

      INTERVAL(:) = 111
      DO K=1,NP
         H0 = DHSTEP(0,NETA,1,K,STPB,1)
         HC0 = H0
         H1 = SQRT(ETA)
         HC1 = ETA**(ONE/THREE)
         H = MAX(TEN*H1,MIN(HUNDRED*H0,ONE))
         HC = MAX(TEN*HC1,MIN(HUNDRED*HC0,ONE))
         IF (BETA(K).EQ.ZERO) THEN
            IF (SSF(1).LT.ZERO) THEN
               TYPJ = ONE/ABS(SSF(1))
            ELSE   
               TYPJ = ONE/SSF(K)
            END IF 
         ELSE
            TYPJ = ABS(BETA(K))
         END IF
         STPR = (H*TYPJ*SIGN(ONE,BETA(K))+BETA(K))-BETA(K)
         STPL = (HC*TYPJ*SIGN(ONE,BETA(K))+BETA(K))-BETA(K)
!   Check outer interval.
         IF (LOWER(K)+2*ABS(STPL).GT.UPPER(K)) THEN
            IF (INTERVAL(K).GE.100) THEN
               INTERVAL(K) = INTERVAL(K) - 100
            END IF
         ELSE IF (BETA(K)+STPL.GT.UPPER(K).OR.BETA(K)-STPL.GT.UPPER(K)) &
         THEN
            BETA(K) = UPPER(K) - ABS(STPL)
         ELSE IF (BETA(K)+STPL.LT.LOWER(K).OR.BETA(K)-STPL.LT.LOWER(K)) &
         THEN
            BETA(K) = LOWER(K) + ABS(STPL)
         END IF
!   Check middle interval.
         IF (LOWER(K)+2*ABS(STPR).GT.UPPER(K)) THEN
            IF (MOD(INTERVAL(K),100).GE.10) THEN
               INTERVAL(K) = INTERVAL(K) - 10
            END IF
         ELSE IF (BETA(K)+STPR.GT.UPPER(K).OR.BETA(K)-STPR.GT.UPPER(K)) &
         THEN
            BETA(K) = UPPER(K) - ABS(STPR)
         ELSE IF (BETA(K)+STPR.LT.LOWER(K).OR.BETA(K)-STPR.LT.LOWER(K)) &
         THEN
            BETA(K) = LOWER(K) + ABS(STPR)
         END IF
!   Check inner interval
         IF (LOWER(K)+ABS(STPR).GT.UPPER(K)) THEN
            INTERVAL(K) = 0
         ELSE IF (BETA(K)+STPR.GT.UPPER(K)) THEN
            BETA(K) = UPPER(K) - STPR
         ELSE IF (BETA(K)+STPR.LT.LOWER(K)) THEN
            BETA(K) = LOWER(K) - STPR
         END IF
      END DO

      END SUBROUTINE
!DERSTEP
      FUNCTION DERSTEP(ITYPE,K,BETAK,SSF,STPB,NETA) RESULT(DERSTEPR)
!***Begin Prologue  DERSTEP
!***Refer to  ODR
!***Routines Called  DHSTEP
!***Date Written   20040616   (YYYYMMDD)
!***Revision Date  20040616   (YYYYMMDD)
!***Purpose  Compute step size for center and forward difference calculations
!***End Prologue  DERSTEP

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      INTEGER ITYPE,K,NETA
      REAL (KIND=R8) BETAK

!...Array arguments
      REAL (KIND=R8) SSF(K),STPB(K)

!...Result
      REAL (KIND=R8) DERSTEPR

!...Local scalars
      REAL (KIND=R8) ONE,TYPJ,ZERO

!...External functions
      REAL (KIND=R8) DHSTEP
      EXTERNAL DHSTEP

!...Data statements
      DATA ZERO,ONE /0.0E0_R8,1.0E0_R8/

!...Variable definitions (alphabetically)
!   BETAK:   The K-th function parameter.
!   ITYPE:   0 - calc foward difference step, 1 - calc center difference step.
!   K:       Index into beta where BETAK resides.
!   NETA:    Number of good digits in the function results.
!   ONE:     The value 1.0E0_R8.
!   SSF:     The scale used for the BETA'S.
!   STPB:    The relative step used for computing finite difference derivatives
!            with respect to BETA.
!   TYPJ:    The typical size of the J-th unkonwn BETA.
!   ZERO:    The value 0.0E0_R8.


!***First executable statement  DERSTEP


      IF (BETAK.EQ.ZERO) THEN
         IF (SSF(1).LT.ZERO) THEN
            TYPJ = ONE/ABS(SSF(1))
         ELSE   
            TYPJ = ONE/SSF(K)
         END IF 
      ELSE
         TYPJ = ABS(BETAK)
      END IF
      DERSTEPR = SIGN(ONE,BETAK)*TYPJ*DHSTEP(ITYPE,NETA,1,K,STPB,1)

      RETURN
      END FUNCTION
!DJACFD
      SUBROUTINE DJACFD(FCN,N,M,NP,NQ,                                 &
                        BETA,X,LDX,DELTA,XPLUSD,IFIXB,IFIXX,LDIFX,     &
                        STPB,STPD,LDSTPD,                              &
                        SSF,TT,LDTT,NETA,FN,STP,WRK1,WRK2,WRK3,WRK6,   &
                        FJACB,ISODR,FJACD,NFEV,ISTOP,INFO,LOWER,UPPER)
!***Begin Prologue  DJACFD
!***Refer to  ODR
!***Routines Called  FCN,DHSTEP,DZERO,DERSTEP
!***Date Written   860529   (YYMMDD)
!***Revision Date  920619   (YYMMDD)
!***Purpose  Compute forward difference approximations to the
!            Jacobian wrt the estimated BETAS and wrt the DELTAS
!***End Prologue  DJACFD

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      INTEGER INFO,ISTOP,LDIFX,LDSTPD,LDTT,LDX,M,N,NETA,NFEV,NP,NQ
      LOGICAL ISODR

!...Array arguments
      REAL (KIND=R8)                                                   &
         BETA(NP),DELTA(N,M),FJACB(N,NP,NQ),FJACD(N,M,NQ),FN(N,NQ),    &
         LOWER(NP),SSF(NP),STP(N),STPB(NP),STPD(LDSTPD,M),TT(LDTT,M),  &
         UPPER(NP),WRK1(N,M,NQ),WRK2(N,NQ),WRK3(NP),WRK6(N,NP,NQ),     &
         X(LDX,M),XPLUSD(N,M)
      INTEGER IFIXB(NP),IFIXX(LDIFX,M)

!...Subroutine arguments
      EXTERNAL FCN

!...Local scalars
      REAL (KIND=R8) BETAK,ONE,STEP,TYPJ,ZERO
      INTEGER I,J,K,L
      LOGICAL DOIT,SETZRO

!...External subroutines
      EXTERNAL DZERO

!...External functions
      REAL (KIND=R8) DHSTEP,DERSTEP
      EXTERNAL DHSTEP,DERSTEP

!...Data statements
      DATA ZERO,ONE /0.0E0_R8,1.0E0_R8/

!...Routine names used as subprogram arguments
!   FCN:     The user supplied subroutine for evaluating the model.

!...Variable Definitions (alphabetically)
!   BETA:    The function parameters.
!   BETAK:   The K-th function parameter.
!   DELTA:   The estimated errors in the explanatory variables.
!   DOIT:    The variable designating whether the derivative wrt a 
!            given BETA or DELTA needs to be computed (DOIT=TRUE)
!            or not (DOIT=FALSE).
!   FJACB:   The Jacobian with respect to BETA.
!   FJACD:   The Jacobian with respect to DELTA.
!   FN:      The new predicted values from the function.
!   I:       An indexing variable.
!   IFIXB:   The values designating whether the elements of BETA are 
!            fixed at their input values or not.
!   IFIXX:   The values designating whether the elements of X are 
!            fixed at their input values or not. 
!   ISODR:   The variable designating whether the solution is by ODR 
!            (ISODR=TRUE) or by OLS (ISODR=FALSE).
!   ISTOP:   The variable designating whether there are problems
!            computing the function at the current BETA and DELTA.
!   J:       An indexing variable.
!   K:       An indexing variable.
!   L:       An indexing variable.
!   LDIFX:   The leading dimension of array IFIXX.
!   LDSTPD:  The leading dimension of array STPD.
!   LDTT:    The leading dimension of array TT.
!   LDX:     The leading dimension of array X.
!   M:       The number of columns of data in the explanatory variable.
!   N:       The number of observations.
!   NETA:    The number of good digits in the function results.
!   NFEV:    The number of function evaluations.
!   NP:      The number of function parameters.
!   ONE:     The value 1.0E0_R8.
!   SETZRO:  The variable designating whether the derivative wrt some 
!            DELTA needs to be set to zero (SETZRO=TRUE) or not
!            (SETZRO=FALSE).
!   SSF:     The scale used for the BETA'S.
!   STP:     The step used for computing finite difference
!            derivatives with respect to DELTA.
!   STPB:    The relative step used for computing finite difference
!            derivatives with respect to BETA.
!   STPD:    The relative step used for computing finite difference
!            derivatives with respect to DELTA.
!   TT:      The scaling values used for DELTA.
!   TYPJ:    The typical size of the J-th unknown BETA or DELTA.
!   X:       The explanatory variable.
!   XPLUSD:  The values of X + DELTA.
!   WRK1:    A work array of (N by M by NQ) elements.
!   WRK2:    A work array of (N BY NQ) elements.
!   WRK3:    A work array of (NP) elements.
!   WRK6:    A work array of (N BY NP BY NQ) elements.
!   ZERO:    The value 0.0E0_R8.


!***First executable statement  DJACFD


!  Compute the Jacobian wrt the estimated BETAS

      DO 40 K=1,NP
         IF (IFIXB(1).GE.0) THEN
            IF (IFIXB(K).EQ.0) THEN
               DOIT = .FALSE.
            ELSE
               DOIT = .TRUE.
            END IF
         ELSE
            DOIT = .TRUE.
         END IF
         IF (.NOT.DOIT) THEN
            DO 10 L=1,NQ
               CALL DZERO(N,1,FJACB(1,K,L),N)
   10       CONTINUE
         ELSE
            BETAK = BETA(K)
            STEP = DERSTEP(0,K,BETAK,SSF,STPB,NETA)
            WRK3(K) = BETAK + STEP
            WRK3(K) = WRK3(K) - BETAK
            BETA(K) = BETAK + WRK3(K)
            IF (BETA(K).GT.UPPER(K)) THEN
               STEP = -STEP
               WRK3(K) = BETAK + STEP
               WRK3(K) = WRK3(K) - BETAK
               BETA(K) = BETAK + WRK3(K)
            END IF
            IF (BETA(K).LT.LOWER(K)) THEN
               STEP = -STEP
               WRK3(K) = BETAK + STEP
               WRK3(K) = WRK3(K) - BETAK
               BETA(K) = BETAK + WRK3(K)
               IF (BETA(K).GT.UPPER(K)) THEN
                  INFO = 60001
                  RETURN
               END IF
            END IF
            ISTOP = 0
            CALL FCN(N,M,NP,NQ,M,NPBETA,XPLUSD,IFIXB,IFIXX,LDIFX,     &
                     001,WRK2,WRK6,WRK1,ISTOP)
            IF (ISTOP.NE.0) THEN
               RETURN
            ELSE
               NFEV = NFEV + 1
            END IF
            DO 30 L=1,NQ
               DO 20 I=1,N
                  FJACB(I,K,L) = (WRK2(I,L)-FN(I,L))/WRK3(K)
   20          CONTINUE
   30       CONTINUE
            BETA(K) = BETAK
         END IF
   40 CONTINUE

!  Compute the Jacobian wrt the X'S

      IF (ISODR) THEN
         DO 220 J=1,M
            IF (IFIXX(1,1).LT.0) THEN
               DOIT = .TRUE.
               SETZRO = .FALSE.
            ELSE IF (LDIFX.EQ.1) THEN
               IF (IFIXX(1,J).EQ.0) THEN
                  DOIT = .FALSE.
               ELSE
                  DOIT = .TRUE.
               END IF
               SETZRO = .FALSE.
            ELSE
               DOIT = .FALSE.
               SETZRO = .FALSE.
               DO 100 I=1,N
                  IF (IFIXX(I,J).NE.0) THEN
                     DOIT = .TRUE.
                  ELSE
                     SETZRO = .TRUE.
                  END IF
  100          CONTINUE
            END IF
            IF (.NOT.DOIT) THEN
               DO 110 L=1,NQ
                  CALL DZERO(N,1,FJACD(1,J,L),N)
  110          CONTINUE
            ELSE
               DO 120 I=1,N
                  IF (XPLUSD(I,J).EQ.ZERO) THEN
                     IF (TT(1,1).LT.ZERO) THEN
                        TYPJ = ONE/ABS(TT(1,1))
                     ELSE IF (LDTT.EQ.1) THEN
                        TYPJ = ONE/TT(1,J)
                     ELSE
                        TYPJ = ONE/TT(I,J)
                     END IF
                  ELSE
                     TYPJ = ABS(XPLUSD(I,J))
                  END IF

                  STP(I) = XPLUSD(I,J) + SIGN(ONE,XPLUSD(I,J))         &
                             *TYPJ*DHSTEP(0,NETA,I,J,STPD,LDSTPD)
                  STP(I) = STP(I) - XPLUSD(I,J)
                  XPLUSD(I,J) = XPLUSD(I,J) + STP(I)
  120          CONTINUE

               ISTOP = 0
               CALL FCN(N,M,NP,NQ,N,M,NP,BETA,XPLUSD,IFIXB,IFIXX,LDIFX, &
                        001,WRK2,WRK6,WRK1,ISTOP)
               IF (ISTOP.NE.0) THEN
                  RETURN
               ELSE
                  NFEV = NFEV + 1
                  DO 140 L=1,NQ
                     DO 130 I=1,N
                        FJACD(I,J,L) = WRK2(I,L)
  130                CONTINUE
  140             CONTINUE

               END IF

               IF (SETZRO) THEN
                  DO 180 I=1,N
                     IF (IFIXX(I,J).EQ.0) THEN
                        DO 160 L=1,NQ
                           FJACD(I,J,L) = ZERO
  160                   CONTINUE
                     ELSE
                        DO 170 L=1,NQ
                           FJACD(I,J,L) = (FJACD(I,J,L)-FN(I,L))/STP(I)
  170                   CONTINUE
                     END IF
  180             CONTINUE
               ELSE
                  DO 200 L=1,NQ
                     DO 190 I=1,N
                        FJACD(I,J,L) = (FJACD(I,J,L)-FN(I,L))/STP(I)
  190                CONTINUE
  200             CONTINUE
               END IF
               DO 210 I=1,N
                  XPLUSD(I,J) = X(I,J) + DELTA(I,J)
  210          CONTINUE
            END IF
  220    CONTINUE
      END IF

      RETURN
      END SUBROUTINE
!DJCK
      SUBROUTINE DJCK(FCN,N,M,NP,NQ,BETA,BETAJ,XPLUSD,                 &
                      IFIXB,IFIXX,LDIFX,STPB,STPD,LDSTPD,              &
                      SSF,TT,LDTT,ETA,NETA,NTOL,NROW,ISODR,EPSMAC,     &
                      PV0I,FJACB,FJACD,                                &
                      MSGB,MSGD,DIFF,ISTOP,NFEV,NJEV,WRK1,WRK2,WRK6,   &
                      INTERVAL)
!***Begin Prologue  DJCK
!***Refer to  ODR
!***Routines Called  FCN,DHSTEP,DJCKM
!***Date Written   860529   (YYMMDD)
!***Revision Date  920619   (YYMMDD)
!***Purpose  Driver routine for the derivative checking process
!            (adapted from STARPAC subroutine DCKCNT)
!***End Prologue  DJCK

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      REAL (KIND=R8) EPSMAC,ETA
      INTEGER ISTOP,LDIFX,LDSTPD,LDTT, M,N,NETA,NFEV,NJEV,NP,NQ,NROW,NTOL
      LOGICAL ISODR

!...Array arguments
      REAL (KIND=R8)                                                   &
         BETA(NP),BETAJ(NP),DIFF(NQ,NP+M),FJACB(N,NP,NQ),FJACD(N,M,NQ),&
         PV0I(N,NQ),SSF(NP),STPB(NP),STPD(LDSTPD,M),TT(LDTT,M),        &
         WRK1(N,M,NQ),WRK2(N,NQ),WRK6(N,NP,NQ),XPLUSD(N,M)
      INTEGER IFIXB(NP),IFIXX(LDIFX,M),INTERVAL(NP),MSGB(1+NQ*NP),     &
              MSGD(1+NQ*M)

!...Subroutine arguments
      EXTERNAL FCN

!...Local scalars
      REAL (KIND=R8) DIFFJ,H0,HC0,ONE,P5,PV,TOL,TYPJ,ZERO
      INTEGER IDEVAL,J,LQ,MSGB1,MSGD1
      LOGICAL ISFIXD,ISWRTB

!...Local arrays
      REAL (KIND=R8) PV0(N,NQ)

!...External subroutines
      EXTERNAL DJCKM

!...External functions
      REAL (KIND=R8) DHSTEP
      EXTERNAL DHSTEP

!...Data statements
      DATA ZERO,P5,ONE /0.0E0_R8,0.5E0_R8,1.0E0_R8/

!...Routine names used as subprogram arguments
!   FCN:     The user supplied subroutine for evaluating the model.

!...Variable Definitions (alphabetically)
!   BETA:    The function parameters.
!   BETAJ:   The function parameters offset such that steps don't cross 
!            bounds.
!   DIFF:    The relative differences between the user supplied and
!            finite difference derivatives for each derivative checked.
!   DIFFJ:   The relative differences between the user supplied and
!            finite difference derivatives for the derivative being
!            checked.
!   EPSMAC:  The value of machine precision.
!   ETA:     The relative noise in the function results.
!   FJACB:   The Jacobian with respect to BETA.
!   FJACD:   The Jacobian with respect to DELTA.
!   H0:      The initial relative step size for forward differences.
!   HC0:     The initial relative step size for central differences.
!   IDEVAL:  The variable designating what computations are to be 
!            performed by user supplied subroutine FCN.
!   IFIXB:   The values designating whether the elements of BETA are
!            fixed at their input values or not.
!   IFIXX:   The values designating whether the elements of X are
!            fixed at their input values or not.
!   INTERVAL: Specifies which checks can be performed when checking derivatives
!            based on the interval of the bound constraints.
!   ISFIXD:  The variable designating whether the parameter is fixed
!            (ISFIXD=TRUE) or not (ISFIXD=FALSE).
!   ISTOP:   The variable designating whether there are problems
!            computing the function at the current BETA and DELTA.
!   ISODR:   The variable designating whether the solution is by ODR 
!            (ISODR=.TRUE.) or by OLS (ISODR=.FALSE.).
!   ISWRTB:  The variable designating whether the derivatives wrt BETA 
!            (ISWRTB=TRUE) or DELTA (ISWRTB=FALSE) are being checked.
!   J:       An index variable.
!   LDIFX:   The leading dimension of array IFIXX.
!   LDSTPD:  The leading dimension of array STPD.
!   LDTT:    The leading dimension of array TT.
!   LQ:      The response currently being examined.
!   M:       The number of columns of data in the explanatory variable.
!   MSGB:    The error checking results for the Jacobian wrt BETA.
!   MSGB1:   The error checking results for the Jacobian wrt BETA.
!   MSGD:    The error checking results for the Jacobian wrt DELTA.
!   MSGD1:   The error checking results for the Jacobian wrt DELTA.
!   N:       The number of observations.
!   NETA:    The number of reliable digits in the model results, either
!            set by the user or computed by DETAF.
!   NFEV:    The number of function evaluations.
!   NJEV:    The number of Jacobian evaluations.
!   NP:      The number of function parameters.
!   NQ:      The number of responses per observation.
!   NROW:    The row number of the explanatory variable array at which 
!            the derivative is checked.
!   NTOL:    The number of digits of agreement required between the
!            numerical derivatives and the user supplied derivatives.
!   ONE:     The value 1.0E0_R8.
!   P5:      The value 0.5E0_R8.
!   PV:      The scalar in which the predicted value from the model for
!            row   NROW   is stored.
!   PV0:     The predicted values using the current parameter estimates
!            (possibly offset from the user supplied estimates to create 
!            distance between parameters and the bounds on the parameters).
!   PV0I:    The predicted values using the user supplied parameter estimates.
!   SSF:     The scaling values used for BETA.
!   STPB:    The step size for finite difference derivatives wrt BETA.
!   STPD:    The step size for finite difference derivatives wrt DELTA.
!   TOL:     The agreement tolerance.
!   TT:      The scaling values used for DELTA.
!   TYPJ:    The typical size of the J-th unknown BETA or DELTA.
!   WRK1:    A work array of (N BY M BY NQ) elements.
!   WRK2:    A work array of (N BY NQ) elements.
!   WRK6:    A work array of (N BY NP BY NQ) elements.
!   XPLUSD:  The values of X + DELTA.
!   ZERO:    The value 0.0E0_R8.


!***First executable statement  DJCK


!  Set tolerance for checking derivatives

      TOL  = ETA**(0.25E0_R8)
      NTOL = MAX(ONE,P5-LOG10(TOL))


!  Compute, if necessary, PV0

      PV0 = PV0I
      IF ( ANY(BETA(:).NE.BETAJ(:)) ) THEN
         ISTOP = 0
         IDEVAL = 001
         CALL FCN(N,M,NP,NQ,N,M,NP,BETAJ,XPLUSD,IFIXB,IFIXX,LDIFX,     &
                  IDEVAL,PV0,FJACB,FJACD,ISTOP)
         IF (ISTOP.NE.0) THEN
            RETURN
         ELSE
            NJEV = NJEV + 1
         END IF
      END IF


!  Compute user supplied derivative values

      ISTOP = 0
      IF (ISODR) THEN
         IDEVAL = 110
      ELSE
         IDEVAL = 010
      END IF
      CALL FCN(N,M,NP,NQ,N,M,NP,BETAJ,XPLUSD,IFIXB,IFIXX,LDIFX,        &
               IDEVAL,WRK2,FJACB,FJACD,ISTOP)
      IF (ISTOP.NE.0) THEN
         RETURN
      ELSE
         NJEV = NJEV + 1
      END IF

!  Check derivatives wrt BETA for each response of observation NROW

      MSGB1 = 0
      MSGD1 = 0

      DO 30 LQ=1,NQ

!  Set predicted value of model at current parameter estimates
         PV = PV0(NROW,LQ)

         ISWRTB = .TRUE.
         DO 10 J=1,NP

            IF (IFIXB(1).LT.0) THEN
               ISFIXD = .FALSE.
            ELSE IF (IFIXB(J).EQ.0) THEN
               ISFIXD = .TRUE.
            ELSE
               ISFIXD = .FALSE.
            END IF

            IF (ISFIXD) THEN
               MSGB(1+LQ+(J-1)*NQ) = -1
            ELSE
               IF (BETA(J).EQ.ZERO) THEN
                  IF (SSF(1).LT.ZERO) THEN
                     TYPJ = ONE/ABS(SSF(1))
                  ELSE
                     TYPJ = ONE/SSF(J)
                  END IF
               ELSE
                  TYPJ = ABS(BETA(J))
               END IF
   
               H0  = DHSTEP(0,NETA,1,J,STPB,1)
               HC0 = H0

!  Check derivative wrt the J-th parameter at the NROW-th row

               IF (INTERVAL(J).GE.1) THEN
                  CALL DJCKM(FCN,N,M,NP,NQ,BETAJ,XPLUSD,               &
                             IFIXB,IFIXX,LDIFX,                        &
                             ETA,TOL,NROW,EPSMAC,J,LQ,TYPJ,H0,HC0,     &
                             ISWRTB,PV,FJACB(NROW,J,LQ),               &
                             DIFFJ,MSGB1,MSGB(2),ISTOP,NFEV,           &
                             WRK1,WRK2,WRK6,INTERVAL)
                  IF (ISTOP.NE.0) THEN
                     MSGB(1) = -1
                     RETURN
                  ELSE
                     DIFF(LQ,J) = DIFFJ
                  END IF
               ELSE
                  MSGB(1+J) = 9
               END IF
            END IF

   10    CONTINUE

!  Check derivatives wrt X for each response of observation NROW

         IF (ISODR) THEN
            ISWRTB = .FALSE.
            DO 20 J=1,M

               IF (IFIXX(1,1).LT.0) THEN
                  ISFIXD = .FALSE.
               ELSE IF (LDIFX.EQ.1) THEN
                  IF (IFIXX(1,J).EQ.0) THEN
                     ISFIXD = .TRUE.
                  ELSE
                     ISFIXD = .FALSE.
                  END IF
               ELSE
                  ISFIXD = .FALSE.
               END IF

               IF (ISFIXD) THEN
                  MSGD(1+LQ+(J-1)*NQ) = -1
               ELSE

                  IF (XPLUSD(NROW,J).EQ.ZERO) THEN
                     IF (TT(1,1).LT.ZERO) THEN
                        TYPJ = ONE/ABS(TT(1,1))
                     ELSE IF (LDTT.EQ.1) THEN
                        TYPJ = ONE/TT(1,J)
                     ELSE
                        TYPJ = ONE/TT(NROW,J)
                     END IF
                  ELSE  
                     TYPJ = ABS(XPLUSD(NROW,J))
                  END IF
 
                  H0  = DHSTEP(0,NETA,NROW,J,STPD,LDSTPD)
                  HC0 = DHSTEP(1,NETA,NROW,J,STPD,LDSTPD)

!  Check derivative wrt the J-th column of DELTA at row NROW

                  CALL DJCKM(FCN,N,M,NP,NQ,BETAJ,XPLUSD,                &
                             IFIXB,IFIXX,LDIFX,                         &
                             ETA,TOL,NROW,EPSMAC,J,LQ,TYPJ,H0,HC0,      &
                             ISWRTB,PV,FJACD(NROW,J,LQ),                &
                             DIFFJ,MSGD1,MSGD(2),ISTOP,NFEV,            &
                             WRK1,WRK2,WRK6,INTERVAL)
                  IF (ISTOP.NE.0) THEN
                     MSGD(1) = -1
                     RETURN
               ELSE
                  DIFF(LQ,NP+J) = DIFFJ
                  END IF
               END IF

   20       CONTINUE
         END IF
   30 CONTINUE
      MSGB(1) = MSGB1
      MSGD(1) = MSGD1

      RETURN
      END SUBROUTINE
!DJCKC
      SUBROUTINE DJCKC(FCN,N,M,NP,NQ,BETA,XPLUSD,IFIXB,IFIXX,LDIFX,    &
                       ETA,TOL,NROW,EPSMAC,J,LQ,HC,ISWRTB,             &
                       FD,TYPJ,PVPSTP,STP0,PV,D,                       &
                       DIFFJ,MSG,ISTOP,NFEV,WRK1,WRK2,WRK6)
!***Begin Prologue  DJCKC
!***Refer to  ODR
!***Routines Called  DJCKF,DPVB,DPVD
!***Date Written   860529   (YYMMDD)
!***Revision Date  920619   (YYMMDD)
!***Purpose  Check whether high curvature could be the cause of the
!            disagreement between the numerical and analytic derviatives
!            (adapted from STARPAC subroutine DCKCRV)
!***End prologue  DJCKC

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      REAL (KIND=R8) D,DIFFJ,EPSMAC,ETA,FD,HC,PV,PVPSTP,STP0,TOL,TYPJ
      INTEGER ISTOP,J,LDIFX,LQ,M,N,NFEV,NP,NQ,NROW
      LOGICAL ISWRTB

!...Array arguments
      REAL (KIND=R8)                                                   &
         BETA(NP),WRK1(N,M,NQ),WRK2(N,NQ),WRK6(N,NP,NQ),XPLUSD(N,M)
      INTEGER IFIXB(NP),IFIXX(LDIFX,M),MSG(NQ,J)

!...Subroutine arguments
      EXTERNAL FCN

!...Local scalars
      REAL (KIND=R8) CURVE,ONE,PVMCRV,PVPCRV,P01,STP,STPCRV,TEN,TWO

!...External subroutines
      EXTERNAL DJCKF,DPVB,DPVD

!...Data statements
      DATA P01,ONE,TWO,TEN /0.01E0_R8,1.0E0_R8,2.0E0_R8,10.0E0_R8/

!...Routine names used as subprogram arguments
!   FCN:     The user supplied subroutine for evaluating the model.

!...Variable Definitions (alphabetically)
!   BETA:    The function parameters.
!   CURVE:   A measure of the curvature in the model.
!   D:       The derivative with respect to the Jth unknown parameter.
!   DIFFJ:   The relative differences between the user supplied and
!            finite difference derivatives for the derivative being
!            checked.
!   EPSMAC:  The value of machine precision.
!   ETA:     The relative noise in the model
!   FD:      The forward difference derivative wrt the Jth parameter.
!   HC:      The relative step size for central finite differences.
!   IFIXB:   The values designating whether the elements of BETA are
!            fixed at their input values or not.
!   IFIXX:   The values designating whether the elements of X are
!            fixed at their input values or not.
!   ISTOP:   The variable designating whether there are problems
!            computing the function at the current BETA and DELTA.
!   ISWRTB:  The variable designating whether the derivatives wrt BETA 
!            (ISWRTB=TRUE) or DELTA(ISWRTB=FALSE) are being checked.
!   J:       The index of the partial derivative being examined.
!   LDIFX:   The leading dimension of array IFIXX.
!   LQ:      The response currently being examined.
!   M:       The number of columns of data in the explanatory variable.
!   MSG:     The error checking results.
!   N:       The number of observations.
!   NFEV:    The number of function evaluations. 
!   NP:      The number of function parameters.
!   NQ:      The number of responses per observation.
!   NROW:    The row number of the explanatory variable array at which 
!            the derivative is to be checked.
!   ONE:     The value 1.0E0_R8.
!   PV:      The predicted value of the model for row   NROW   .
!   PVMCRV:  The predicted value for row    NROW   of the model
!            based on the current parameter estimates for all but the 
!            Jth parameter value, which is BETA(J)-STPCRV.
!   PVPCRV:  The predicted value for row    NROW   of the model
!            based on the current parameter estimates for all but the 
!            Jth parameter value, which is BETA(J)+STPCRV.
!   PVPSTP:  The predicted value for row    NROW   of the model
!            based on the current parameter estimates for all but the 
!            Jth parameter value, which is BETA(J) + STP0.
!   P01:     The value 0.01E0_R8.
!   STP0:    The initial step size for the finite difference derivative.
!   STP:     A step size for the finite difference derivative.
!   STPCRV:  The step size selected to check for curvature in the model.
!   TEN:     The value 10.0E0_R8.
!   TOL:     The agreement tolerance.
!   TWO:     The value 2.0E0_R8.
!   TYPJ:    The typical size of the J-th unknown BETA or DELTA.
!   WRK1:    A work array of (N BY M BY NQ) elements.
!   WRK2:    A work array of (N BY NQ) elements.
!   WRK6:    A work array of (N BY NP BY NQ) elements.
!   XPLUSD:  The values of X + DELTA.


!***First executable statement  DJCKC


      IF (ISWRTB) THEN

!  Perform central difference computations for derivatives wrt BETA

         STPCRV = (HC*TYPJ*SIGN(ONE,BETA(J))+BETA(J)) - BETA(J)
         CALL DPVB(FCN,N,M,NP,NQ,                                      &
                   BETA,XPLUSD,IFIXB,IFIXX,LDIFX,NROW,J,LQ,STPCRV,     &
                   ISTOP,NFEV,PVPCRV,WRK1,WRK2,WRK6)
         IF (ISTOP.NE.0) THEN
            RETURN
         END IF
         CALL DPVB(FCN,N,M,NP,NQ,                                      &
                   BETA,XPLUSD,IFIXB,IFIXX,LDIFX,NROW,J,LQ,-STPCRV,    &
                   ISTOP,NFEV,PVMCRV,WRK1,WRK2,WRK6)
         IF (ISTOP.NE.0) THEN
            RETURN
         END IF
      ELSE

!  Perform central difference computations for derivatives wrt DELTA

         STPCRV = (HC*TYPJ*SIGN(ONE,XPLUSD(NROW,J))+XPLUSD(NROW,J)) -  &
                  XPLUSD(NROW,J)
         CALL DPVD(FCN,N,M,NP,NQ,                                      &
                   BETA,XPLUSD,IFIXB,IFIXX,LDIFX,NROW,J,LQ,STPCRV,     &
                   ISTOP,NFEV,PVPCRV,WRK1,WRK2,WRK6)
         IF (ISTOP.NE.0) THEN
            RETURN
         END IF
         CALL DPVD(FCN,N,M,NP,NQ,                                      &
                   BETA,XPLUSD,IFIXB,IFIXX,LDIFX,NROW,J,LQ,-STPCRV,    &
                   ISTOP,NFEV,PVMCRV,WRK1,WRK2,WRK6)
         IF (ISTOP.NE.0) THEN
            RETURN
         END IF
      END IF

!  Estimate curvature by second derivative of model

      CURVE = ABS((PVPCRV-PV)+(PVMCRV-PV)) / (STPCRV*STPCRV)
      CURVE = CURVE +                                                  &
              ETA*(ABS(PVPCRV)+ABS(PVMCRV)+TWO*ABS(PV)) / (STPCRV**2)


!  Check if finite precision arithmetic could be the culprit.
      CALL DJCKF(FCN,N,M,NP,NQ,                                        &
                 BETA,XPLUSD,IFIXB,IFIXX,LDIFX,ETA,TOL,NROW,J,LQ,      &
                 ISWRTB,FD,TYPJ,PVPSTP,STP0,CURVE,PV,D,                &
                 DIFFJ,MSG,ISTOP,NFEV,WRK1,WRK2,WRK6)
      IF (ISTOP.NE.0) THEN
         RETURN
      END IF
      IF (MSG(LQ,J).EQ.0) THEN
         RETURN
      END IF

!  Check if high curvature could be the problem.

      STP = TWO*MAX(TOL*ABS(D)/CURVE,EPSMAC)
      IF (STP.LT.ABS(TEN*STP0)) THEN
         STP = MIN(STP,P01*ABS(STP0))
      END IF


      IF (ISWRTB) THEN

!  Perform computations for derivatives wrt BETA
         STP = (STP*SIGN(ONE,BETA(J)) + BETA(J)) - BETA(J)
         CALL DPVB(FCN,N,M,NP,NQ,BETA,XPLUSD,IFIXB,IFIXX,LDIFX,        &
                   NROW,J,LQ,STP,ISTOP,NFEV,PVPSTP,WRK1,WRK2,WRK6)
         IF (ISTOP.NE.0) THEN
            RETURN
         END IF
      ELSE

!  Perform computations for derivatives wrt DELTA
         STP = (STP*SIGN(ONE,XPLUSD(NROW,J)) + XPLUSD(NROW,J)) -       &
     &         XPLUSD(NROW,J)
         CALL DPVD(FCN,N,M,NP,NQ,BETA,XPLUSD,IFIXB,IFIXX,LDIFX,        &
                   NROW,J,LQ,STP,ISTOP,NFEV,PVPSTP,WRK1,WRK2,WRK6)
         IF (ISTOP.NE.0) THEN
            RETURN
         END IF
      END IF

!  Compute the new numerical derivative

      FD = (PVPSTP-PV)/STP
      DIFFJ = MIN(DIFFJ,ABS(FD-D)/ABS(D))

!  Check whether the new numerical derivative is ok
      IF (ABS(FD-D).LE.TOL*ABS(D)) THEN
         MSG(LQ,J) = 0

!  Check if finite precision may be the culprit (fudge factor = 2)
      ELSE IF (ABS(STP*(FD-D)).LT.TWO*ETA*(ABS(PV)+ABS(PVPSTP))        &
                                      + CURVE*(EPSMAC*TYPJ)**2) THEN
         MSG(LQ,J) = 5
      END IF

      RETURN
      END SUBROUTINE
!DJCKF
      SUBROUTINE DJCKF(FCN,N,M,NP,NQ,BETA,XPLUSD,IFIXB,IFIXX,LDIFX,    &
                       ETA,TOL,NROW,J,LQ,ISWRTB,                       &
                       FD,TYPJ,PVPSTP,STP0,CURVE,PV,D,                 &
                       DIFFJ,MSG,ISTOP,NFEV,WRK1,WRK2,WRK6)
!***Begin Prologue  DJCKF
!***Refer to  ODR
!***Routines Called  DPVB,DPVD
!***Date Written   860529   (YYMMDD)
!***Revision Date  920619   (YYMMDD)
!***Purpose  Check whether finite precision arithmetic could be the
!            cause of the disagreement between the derivatives
!            (adapted from STARPAC subroutine DCKFPA)
!***End Prologue  DJCKF

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      REAL (KIND=R8) CURVE,D,DIFFJ,ETA,FD,PV,PVPSTP,STP0,TOL,TYPJ
      INTEGER ISTOP,J,LDIFX,LQ,M,N,NFEV,NP,NQ,NROW
      LOGICAL ISWRTB

!...Array arguments
      REAL (KIND=R8) BETA(NP),WRK1(N,M,NQ),WRK2(N,NQ),WRK6(N,NP,NQ),XPLUSD(N,M)
      INTEGER IFIXB(NP),IFIXX(LDIFX,M),MSG(NQ,J)

!...Subroutine arguments
      EXTERNAL FCN

!...Local scalars
      REAL (KIND=R8) HUNDRD,ONE,P1,STP,TWO
      LOGICAL LARGE

!...External subroutines
      EXTERNAL DPVB,DPVD

!...Data statements
      DATA P1,ONE,TWO,HUNDRD /0.1E0_R8,1.0E0_R8,2.0E0_R8,100.0E0_R8/

!...Routine names used as subprogram arguments
!   FCN:     The user supplied subroutine for evaluating the model.

!...Variable Definitions (alphabetically)
!   BETA:    The function parameters.
!   CURVE:   A measure of the curvature in the model.
!   D:       The derivative with respect to the Jth unknown parameter.
!   DIFFJ:   The relative differences between the user supplied and
!            finite difference derivatives for the derivative being
!            checked.
!   ETA:     The relative noise in the model
!   FD:      The forward difference derivative wrt the Jth parameter.
!   HUNDRD:  The value 100.0E0_R8.
!   IFIXB:   The values designating whether the elements of BETA are
!            fixed at their input values or not.
!   IFIXX:   The values designating whether the elements of X are
!            fixed at their input values or not.
!   ISTOP:   The variable designating whether there are problems
!            computing the function at the current BETA and DELTA.
!   ISWRTB:  The variable designating whether the derivatives wrt BETA 
!            (ISWRTB=TRUE) or DELTA(ISWRTB=FALSE) are being checked.
!   J:       The index of the partial derivative being examined.
!   LARGE:   The value designating whether the recommended increase in 
!            the step size would be greater than TYPJ.
!   LDIFX:   The leading dimension of array IFIXX.
!   LQ:      The response currently being examined.
!   M:       The number of columns of data in the explanatory variable.
!   MSG:     The error checking results.
!   N:       The number of observations.
!   NFEV:    The number of function evaluations. 
!   NP:      The number of function parameters.
!   NQ:      The number of responses per observation.
!   NROW:    The row number of the explanatory variable array at which 
!            the derivative is to be checked.
!   ONE:     The value 1.0E0_R8.
!   PV:      The predicted value for row   NROW   .
!   PVPSTP:  The predicted value for row    NROW   of the model
!            based on the current parameter estimates for all but the 
!            Jth parameter value, which is BETA(J) + STP0.
!   P1:      The value 0.1E0_R8.
!   STP0:    The step size for the finite difference derivative.
!   TOL:     The agreement tolerance.
!   TWO:     The value 2.0E0_R8.
!   TYPJ:    The typical size of the J-th unknown BETA or DELTA.
!   WRK1:    A work array of (N BY M BY NQ) elements.
!   WRK2:    A work array of (N BY NQ) elements.
!   WRK6:    A work array of (N BY NP BY NQ) elements.
!   XPLUSD:  The values of X + DELTA.


!***First executable statement  DJCKF


!  Finite precision arithmetic could be the problem.
!  Try a larger step size based on estimate of condition error

      STP = ETA*(ABS(PV)+ABS(PVPSTP))/(TOL*ABS(D))
      IF (STP.GT.ABS(P1*STP0)) THEN
         STP = MAX(STP,HUNDRD*ABS(STP0))
      END IF
      IF (STP.GT.TYPJ) THEN
         STP = TYPJ
         LARGE = .TRUE.
      ELSE
         LARGE = .FALSE.
      END IF
 
      IF (ISWRTB) THEN

!  Perform computations for derivatives wrt BETA
         STP = (STP*SIGN(ONE,BETA(J))+BETA(J)) - BETA(J)
         CALL DPVB(FCN,N,M,NP,NQ,BETA,XPLUSD,IFIXB,IFIXX,LDIFX,        &
                   NROW,J,LQ,STP,ISTOP,NFEV,PVPSTP,WRK1,WRK2,WRK6)
      ELSE

!  Perform computations for derivatives wrt DELTA
         STP = (STP*SIGN(ONE,XPLUSD(NROW,J)) + XPLUSD(NROW,J)) -       &
               XPLUSD(NROW,J)
         CALL DPVD(FCN,N,M,NP,NQ,BETA,XPLUSD,IFIXB,IFIXX,LDIFX,        &
                   NROW,J,LQ,STP,ISTOP,NFEV,PVPSTP,WRK1,WRK2,WRK6)
      END IF
      IF (ISTOP.NE.0) THEN
         RETURN
      END IF

      FD = (PVPSTP-PV)/STP
      DIFFJ = MIN(DIFFJ,ABS(FD-D)/ABS(D))

!  Check for agreement

      IF ((ABS(FD-D)).LE.TOL*ABS(D)) THEN
!  Forward difference quotient and analytic derivatives agree.
         MSG(LQ,J) = 0

      ELSE IF ((ABS(FD-D).LE.ABS(TWO*CURVE*STP)) .OR. LARGE) THEN
!  Curvature may be the culprit (fudge factor = 2)
         IF (LARGE) THEN
            MSG(LQ,J) = 4
         ELSE
            MSG(LQ,J) = 5
         END IF
      END IF

      RETURN
      END SUBROUTINE
!DJCKM
      SUBROUTINE DJCKM(FCN,N,M,NP,NQ,BETA,XPLUSD,IFIXB,IFIXX,LDIFX,    &
                       ETA,TOL,NROW,EPSMAC,J,LQ,TYPJ,H0,HC0,           &
                       ISWRTB,PV,D,DIFFJ,MSG1,MSG,ISTOP,NFEV,          &
                       WRK1,WRK2,WRK6,INTERVAL)
!***Begin Prologue  DJCKM
!***Refer to  ODR
!***Routines Called  DJCKC,DJCKZ,DPVB,DPVD
!***Date Written   860529   (YYMMDD)
!***Revision Date  920619   (YYMMDD)
!***Purpose  Check user supplied analytic derivatives against numerical
!            derivatives
!            (adapted from STARPAC subroutine DCKMN)
!***End prologue  DJCKM

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      REAL (KIND=R8) D,DIFFJ,EPSMAC,ETA,H0,HC0,PV,TOL,TYPJ
      INTEGER ISTOP,J,LDIFX,LQ,M,MSG1,N,NFEV,NP,NQ,NROW
      LOGICAL ISWRTB

!...Array arguments
      REAL (KIND=R8) BETA(NP),WRK1(N,M,NQ),WRK2(N,NQ),WRK6(N,NP,NQ),   &
                     XPLUSD(N,M)
      INTEGER IFIXB(NP),IFIXX(LDIFX,M),INTERVAL(NP),MSG(NQ,J)

!...Subroutine arguments
      EXTERNAL FCN

!...Local scalars
      REAL (KIND=R8) BIG,FD,H,HC,H1,HC1,HUNDRD,ONE,PVPSTP,P01,P1,STP0, &
         TEN,THREE,TOL2,TWO,ZERO
      INTEGER I

!...External subroutines
      EXTERNAL DJCKC,DJCKZ,DPVB,DPVD

!...Data statements
      DATA ZERO,P01,P1,ONE,TWO,THREE,TEN,HUNDRD                        &
         /0.0E0_R8,0.01E0_R8,0.1E0_R8,1.0E0_R8,2.0E0_R8,3.0E0_R8,      &
         1.0E1_R8,1.0E2_R8/
      DATA BIG,TOL2 /1.0E19_R8,5.0E-2_R8/

!...Routine names used as subprogram arguments
!   FCN:     The user supplied subroutine for evaluating the model.

!...Variable Definitions (alphabetically)
!   BETA:    The function parameters.
!   BIG:     A big value, used to initialize DIFFJ.
!   D:       The derivative with respect to the Jth unknown parameter.
!   DIFFJ:   The relative differences between the user supplied and
!            finite difference derivatives for the derivative being
!            checked.
!   EPSMAC:  The value of machine precision.
!   ETA:     The relative noise in the function results.
!   FD:      The forward difference derivative wrt the Jth parameter.
!   H:       The relative step size for forward differences.
!   H0:      The initial relative step size for forward differences.
!   H1:      The default relative step size for forward differences.
!   HC:      The relative step size for central differences.
!   HC0:     The initial relative step size for central differences.
!   HC1:     The default relative step size for central differences.
!   HUNDRD:  The value 100.0E0_R8.
!   IFIXB:   The values designating whether the elements of BETA are
!            fixed at their input values or not.
!   IFIXX:   The values designating whether the elements of X are
!            fixed at their input values or not.
!   INTERVAL: Specifies which checks can be performed when checking derivatives
!            based on the interval of the bound constraints.
!   ISTOP:   The variable designating whether there are problems
!            computing the function at the current BETA and DELTA.
!   ISWRTB:  The variable designating whether the derivatives wrt BETA 
!            (ISWRTB=TRUE) or DELTAS (ISWRTB=FALSE) are being checked.
!   J:       The index of the partial derivative being examined.
!   LDIFX:   The leading dimension of array IFIXX.
!   LQ:      The response currently being examined.
!   M:       The number of columns of data in the explanatory variable.
!   MSG:     The error checking results.
!   MSG1:    The error checking results summary.
!   N:       The number of observations.
!   NFEV:    The number of function evaluations.
!   NP:      The number of function parameters.
!   NQ:      The number of responses per observation.
!   NROW:    The row number of the explanatory variable array at which 
!            the derivative is to be checked.
!   ONE:     The value 1.0E0_R8.
!   PV:      The predicted value from the model for row   NROW   .
!   PVPSTP:  The predicted value for row    NROW   of the model
!            Using the current parameter estimates for all but the Jth 
!            parameter value, which is BETA(J) + STP0.
!   P01:     The value 0.01E0_R8.
!   P1:      The value 0.1E0_R8.
!   STP0:    The initial step size for the finite difference derivative.
!   TEN:     The value 10.0E0_R8.
!   THREE:   The value 3.0E0_R8.
!   TWO:     The value 2.0E0_R8.
!   TOL:     The agreement tolerance.
!   TOL2:    A minimum agreement tolerance.
!   TYPJ:    The typical size of the J-th unknown BETA or DELTA.
!   WRK1:    A work array of (N BY M BY NQ) elements.
!   WRK2:    A work array of (N BY NQ) elements.
!   WRK6:    A work array of (N BY NP BY NQ) elements.
!   XPLUSD:  The values of X + DELTA.
!   ZERO:    The value 0.0E0_R8.


!***First executable statement  DJCKM


!  Calculate the Jth partial derivative using forward difference
!  quotients and decide if it agrees with user supplied values

      H1  = SQRT(ETA)
      HC1 = ETA**(ONE/THREE)

      MSG(LQ,J) = 7
      DIFFJ = BIG

      DO 10 I=1,3

         IF (I.EQ.1) THEN
!  Try initial relative step size
            H  = H0
            HC = HC0

         ELSE IF (I.EQ.2) THEN
!  Try larger relative step size
            H  = MAX(TEN*H1, MIN(HUNDRD*H0, ONE))
            HC = MAX(TEN*HC1,MIN(HUNDRD*HC0,ONE))

         ELSE IF (I.EQ.3) THEN
!  Try smaller relative step size
            H  = MIN(P1*H1, MAX(P01*H,TWO*EPSMAC))
            HC = MIN(P1*HC1,MAX(P01*HC,TWO*EPSMAC))
         END IF

         IF (ISWRTB) THEN

!  Perform computations for derivatives wrt BETA

            STP0 = (H*TYPJ*SIGN(ONE,BETA(J))+BETA(J)) - BETA(J)
            CALL DPVB(FCN,N,M,NP,NQ,BETA,XPLUSD,IFIXB,IFIXX,LDIFX,     &
                      NROW,J,LQ,STP0,ISTOP,NFEV,PVPSTP,WRK1,WRK2,WRK6)
         ELSE

!  Perform computations for derivatives wrt DELTA

            STP0 = (H*TYPJ*SIGN(ONE,XPLUSD(NROW,J))+XPLUSD(NROW,J))    &
     &            - XPLUSD(NROW,J)
            CALL DPVD(FCN,N,M,NP,NQ,BETA,XPLUSD,IFIXB,IFIXX,LDIFX,     &
                      NROW,J,LQ,STP0,ISTOP,NFEV,PVPSTP,WRK1,WRK2,WRK6)
         END IF
         IF (ISTOP.NE.0) THEN
            RETURN
         END IF

         FD = (PVPSTP-PV)/STP0

!  Check for agreement

         IF (ABS(FD-D).LE.TOL*ABS(D)) THEN
!  Numerical and analytic derivatives agree

!  Set relative difference for derivative checking report
            IF ((D.EQ.ZERO) .OR. (FD.EQ.ZERO)) THEN
               DIFFJ = ABS(FD-D)
            ELSE
               DIFFJ = ABS(FD-D)/ABS(D)
            END IF

!  Set MSG flag.
            IF (D.EQ.ZERO) THEN

!  JTH analytic and numerical derivatives are both zero.
               MSG(LQ,J) = 1

            ELSE
!  JTH analytic and numerical derivatives are both nonzero.
               MSG(LQ,J) = 0
            END IF

         ELSE

!  Numerical and analytic derivatives disagree.  Check why
            IF ((D.EQ.ZERO) .OR. (FD.EQ.ZERO)) THEN
               IF (INTERVAL(J).GE.10.OR..NOT.ISWRTB) THEN
                  CALL DJCKZ(FCN,N,M,NP,NQ,                            &
                             BETA,XPLUSD,IFIXB,IFIXX,LDIFX,            &
                             NROW,EPSMAC,J,LQ,ISWRTB,                  &
                             TOL,D,FD,TYPJ,PVPSTP,STP0,PV,             &
                             DIFFJ,MSG,ISTOP,NFEV,WRK1,WRK2,WRK6)
               ELSE
                  MSG(LQ,J) = 8
               END IF
            ELSE
               IF (INTERVAL(J).GE.100.OR..NOT.ISWRTB) THEN
                  CALL DJCKC(FCN,N,M,NP,NQ,                            &
                             BETA,XPLUSD,IFIXB,IFIXX,LDIFX,            &
                             ETA,TOL,NROW,EPSMAC,J,LQ,HC,ISWRTB,       &
                             FD,TYPJ,PVPSTP,STP0,PV,D,                 &
                             DIFFJ,MSG,ISTOP,NFEV,WRK1,WRK2,WRK6)
               ELSE
                  MSG(LQ,J) = 8
               END IF
            END IF
            IF (MSG(LQ,J).LE.2) THEN
               GO TO 20
            END IF
         END IF
   10 CONTINUE

!  Set summary flag to indicate questionable results
   20 CONTINUE
      IF ((MSG(LQ,J).GE.7) .AND. (DIFFJ.LE.TOL2)) MSG(LQ,J) = 6
      IF ((MSG(LQ,J).GE.1) .AND. (MSG(LQ,J).LE.6)) THEN
         MSG1 = MAX(MSG1,1)
      ELSE IF (MSG(LQ,J).GE.7) THEN
         MSG1 = 2
      END IF

      RETURN
      END SUBROUTINE
!DJCKZ
      SUBROUTINE DJCKZ(FCN,N,M,NP,NQ,BETA,XPLUSD,IFIXB,IFIXX,LDIFX,    &
                       NROW,EPSMAC,J,LQ,ISWRTB,                        &
                       TOL,D,FD,TYPJ,PVPSTP,STP0,PV,                   &
                       DIFFJ,MSG,ISTOP,NFEV,WRK1,WRK2,WRK6)
!***Begin Prologue  DJCKZ
!***Refer to  ODR
!***Routines Called  DPVB,DPVD
!***Date Written   860529   (YYMMDD)
!***Revision Date  920619   (YYMMDD)
!***Purpose  Recheck the derivatives in the case where the finite
!            difference derivative disagrees with the analytic
!            derivative and the analytic derivative is zero
!            (adapted from STARPAC subroutine DCKZRO)
!***End Prologue  DJCKZ

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      REAL (KIND=R8) D,DIFFJ,EPSMAC,FD,PV,PVPSTP,STP0,TOL,TYPJ
      INTEGER ISTOP,J,LDIFX,LQ,M,N,NFEV,NP,NQ,NROW
      LOGICAL ISWRTB

!...Array arguments
      REAL (KIND=R8) BETA(NP),WRK1(N,M,NQ),WRK2(N,NQ),WRK6(N,NP,NQ),   &
                     XPLUSD(N,M)
      INTEGER IFIXB(NP),IFIXX(LDIFX,M),MSG(NQ,J)

!...Subroutine arguments
      EXTERNAL FCN

!...Local scalars
      REAL (KIND=R8) CD,ONE,PVMSTP,THREE,TWO,ZERO

!...External subroutines
      EXTERNAL DPVB,DPVD

!...Data statements
      DATA ZERO,ONE,TWO,THREE /0.0E0_R8,1.0E0_R8,2.0E0_R8,3.0E0_R8/

!...Routine names used as subprogram arguments
!   FCN:     THE USER SUPPLIED SUBROUTINE FOR EVALUATING THE MODEL.

!...Variable Definitions (alphabetically)
!   BETA:    The function parameters.
!   CD:      The central difference derivative wrt the Jth parameter.
!   D:       The derivative with respect to the Jth unknown parameter.
!   DIFFJ:   The relative differences between the user supplied and
!            finite difference derivatives for the derivative being
!            checked.
!   EPSMAC:  The value of machine precision.
!   FD:      The forward difference derivative wrt the Jth parameter.
!   IFIXB:   The values designating whether the elements of BETA are
!            Fixed at their input values or not.
!   IFIXX:   The values designating whether the elements of X are
!            fixed at their input values or not.
!   ISTOP:   The variable designating whether there are problems
!            computing the function at the current BETA and DELTA.
!   ISWRTB:  The variable designating whether the derivatives wrt BETA 
!            (ISWRTB=TRUE) or X (ISWRTB=FALSE) are being checked.
!   J:       The index of the partial derivative being examined.
!   LDIFX:   The leading dimension of array IFIXX.
!   LQ:      The response currently being examined.
!   M:       The number of columns of data in the explanatory variable.
!   MSG:     The error checking results.
!   N:       The number of observations.
!   NFEV:    The number of function evaluations. 
!   NP:      The number of function parameters.
!   NQ:      The number of responses per observation.
!   NROW:    The row number of the explanatory variable array at which 
!            The derivative is to be checked.
!   ONE:     The value 1.0E0_R8.
!   PV:      The predicted value from the model for row   NROW   .
!   PVMSTP:  The predicted value for row    NROW   of the model
!            using the current parameter estimates for all but the 
!            Jth parameter value, which is BETA(J) - STP0.
!   PVPSTP:  The predicted value for row    NROW   of the model
!            using the current parameter estimates for all but the 
!            JTH parameter value, which is BETA(J) + STP0.
!   STP0:    The initial step size for the finite difference derivative.
!   THREE:   The value 3.0E0_R8.
!   TWO:     The value 2.0E0_R8.
!   TOL:     The agreement tolerance.
!   TYPJ:    The typical size of the J-th unknown BETA or DELTA.
!   WRK1:    A work array of (N BY M BY NQ) elements.
!   WRK2:    A work array of (N BY NQ) elements.
!   WRK6:    A work array of (N BY NP BY NQ) elements.
!   XPLUSD:  The values of X + DELTA.
!   ZERO:    The value 0.0E0_R8.


!***First executable statement  DJCKZ


!  Recalculate numerical derivative using central difference and step
!  size of 2*STP0

      IF (ISWRTB) THEN

!  Perform computations for derivatives wrt BETA

         CALL DPVB(FCN,N,M,NP,NQ,BETA,XPLUSD,IFIXB,IFIXX,LDIFX,        &
                   NROW,J,LQ,-STP0,ISTOP,NFEV,PVMSTP,WRK1,WRK2,WRK6)
      ELSE

!  Perform computations for derivatives wrt DELTA

         CALL DPVD(FCN,N,M,NP,NQ,BETA,XPLUSD,IFIXB,IFIXX,LDIFX,        &
                   NROW,J,LQ,-STP0,ISTOP,NFEV,PVMSTP,WRK1,WRK2,WRK6)
      END IF
      IF (ISTOP.NE.0) THEN
         RETURN
      END IF

      CD = (PVPSTP-PVMSTP)/(TWO*STP0)
      DIFFJ = MIN(ABS(CD-D),ABS(FD-D))

!  Check for agreement

      IF (DIFFJ.LE.TOL*ABS(D)) THEN

!  Finite difference and analytic derivatives now agree.
         IF (D.EQ.ZERO) THEN
            MSG(LQ,J) = 1
         ELSE
            MSG(LQ,J) = 0
         END IF

      ELSE IF (DIFFJ*TYPJ.LE.ABS(PV*EPSMAC**(ONE/THREE))) THEN
!  Derivatives are both close to zero
         MSG(LQ,J) = 2

      ELSE
!  Derivatives are not both close to zero
         MSG(LQ,J) = 3
      END IF

      RETURN
      END SUBROUTINE
!DODCHK
      SUBROUTINE DODCHK(N,M,NP,NQ,ISODR,ANAJAC,IMPLCT,BETA,IFIXB,      &
         LDX,LDIFX,LDSCLD,LDSTPD,LDWE,LD2WE,LDWD,LD2WD,LDY,            &
         LWORK,LWKMN,LIWORK,LIWKMN,SCLB,SCLD,STPB,STPD,INFO,LOWER,UPPER)
!***Begin Prologue  DODCHK
!***Refer to  ODR
!***Routines Called  (None)
!***Date Written   860529   (YYMMDD)
!***Revision Date  920619   (YYMMDD)
!***Purpose  Check input parameters, indicating errors found using
!            nonzero values of argument INFO 
!***End Prologue  DODCHK

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      INTEGER INFO,LDIFX,LDSCLD,LDSTPD,LDWD,LDWE,LDX,LDY,LD2WD,LD2WE,  &
              LIWKMN,LIWORK,LWKMN,LWORK,M,N,NP,NQ
      LOGICAL ANAJAC,IMPLCT,ISODR

!...Array arguments
      REAL (KIND=R8) BETA(NP),LOWER(NP),SCLB(NP),SCLD(LDSCLD,M),       &
                     STPB(NP),STPD(LDSTPD,M),UPPER(NP)
      INTEGER IFIXB(NP)

!...Local scalars
      INTEGER I,J,K,LAST,NPP

!...Variable Definitions (alphabetically)
!   ANAJAC:  The variable designating whether the Jacobians are 
!            computed by finite differences (ANAJAC=FALSE) or not
!            (ANAJAC=TRUE).
!   I:       An indexing variable.
!   IFIXB:   The values designating whether the elements of BETA are 
!            fixed at their input values or not.
!   IMPLCT:  The variable designating whether the solution is by 
!            implicit ODR (IMPLCT=TRUE) or explicit ODR (IMPLCT=FALSE).
!   INFO:    The variable designating why the computations were stopped.
!   ISODR:   The variable designating whether the solution is by ODR 
!            (ISODR=TRUE) or by OLS (ISODR=FALSE).
!   J:       An indexing variable.
!   K:       An indexing variable.
!   LAST:    The last row of the array to be accessed.
!   LDIFX:   The leading dimension of array IFIXX.
!   LDSCLD:  The leading dimension of array SCLD.
!   LDSTPD:  The leading dimension of array STPD.
!   LDWD:    The leading dimension of array WD.
!   LDWE:    The leading dimension of array WE.
!   LDX:     The leading dimension of array X.
!   LDY:     The leading dimension of array X.
!   LD2WD:   The second dimension of array WD.
!   LD2WE:   The second dimension of array WE.
!   LIWKMN:  The minimum acceptable length of array IWORK.
!   LIWORK:  The length of vector IWORK.
!   LWKMN:   The minimum acceptable length of array WORK.
!   LWORK:   The length of vector WORK.
!   M:       The number of columns of data in the explanatory variable.
!   N:       The number of observations.
!   NP:      The number of function parameters.
!   NPP:     The number of function parameters being estimated.
!   NQ:      The number of responses per observations.
!   SCLB:    The scaling values for BETA.
!   SCLD:    The scaling value for DELTA.
!   STPB:    The step for the finite difference derivitive wrt BETA.
!   STPD:    The step for the finite difference derivitive wrt DELTA.


!***First executable statement  DODCHK


!  Find actual number of parameters being estimated

      IF (NP.LE.0 .OR. IFIXB(1).LT.0) THEN
         NPP = NP
      ELSE
         NPP = 0
         DO 10 K=1,NP
            IF (IFIXB(K).NE.0) THEN
               NPP = NPP + 1
            END IF
   10    CONTINUE
      END IF

!  Check problem specification parameters

      IF (N.LE.0 .OR. M.LE.0 .OR.  (NPP.LE.0 .OR. NPP.GT.N) .OR.       &
          (NQ.LE.0)) THEN

         INFO = 10000
         IF (N.LE.0) THEN
            INFO = INFO + 1000
         END IF
         IF (M.LE.0) THEN
            INFO = INFO + 100
         END IF
         IF (NPP.LE.0 .OR. NPP.GT.N) THEN
            INFO = INFO + 10
         END IF
         IF (NQ.LE.0) THEN
            INFO = INFO + 1
         END IF

         RETURN

      END IF

!  Check dimension specification parameters

      IF ((.NOT.IMPLCT .AND. LDY.LT.N) .OR. (LDX.LT.N) .OR.            &
          (LDWE.NE.1 .AND. LDWE.LT.N) .OR.                             &
          (LD2WE.NE.1 .AND. LD2WE.LT.NQ) .OR.                          &
          (ISODR .AND. (LDWD.NE.1 .AND. LDWD.LT.N)) .OR.               &
          (ISODR .AND. (LD2WD.NE.1 .AND. LD2WD.LT.M)) .OR.             &
          (ISODR .AND. (LDIFX.NE.1 .AND. LDIFX.LT.N)) .OR.             &
          (ISODR .AND. (LDSTPD.NE.1 .AND. LDSTPD.LT.N)) .OR.           &
          (ISODR .AND. (LDSCLD.NE.1 .AND. LDSCLD.LT.N)) .OR.           &
          (LWORK.LT.LWKMN) .OR.  (LIWORK.LT.LIWKMN)) THEN

         INFO = 20000
         IF (.NOT.IMPLCT .AND. LDY.LT.N) THEN
            INFO = INFO + 1000
         END IF
         IF (LDX.LT.N) THEN
            INFO = INFO + 2000
         END IF

         IF ((LDWE.NE.1 .AND. LDWE.LT.N) .OR.                          &
             (LD2WE.NE.1 .AND. LD2WE.LT.NQ)) THEN
            INFO = INFO + 100
         END IF
         IF (ISODR .AND. ((LDWD.NE.1 .AND. LDWD.LT.N) .OR.             &
                          (LD2WD.NE.1 .AND. LD2WD.LT.M))) THEN
            INFO = INFO + 200
         END IF

         IF (ISODR .AND. (LDIFX.NE.1 .AND. LDIFX.LT.N)) THEN
            INFO = INFO + 10
         END IF
         IF (ISODR .AND. (LDSTPD.NE.1 .AND. LDSTPD.LT.N)) THEN
            INFO = INFO + 20
         END IF
         IF (ISODR .AND. (LDSCLD.NE.1 .AND. LDSCLD.LT.N)) THEN
            INFO = INFO + 40
         END IF

         IF (LWORK.LT.LWKMN) THEN
            INFO = INFO + 1
         END IF
         IF (LIWORK.LT.LIWKMN) THEN
            INFO = INFO + 2
         END IF
         RETURN

      END IF

!  Check DELTA scaling

      IF (ISODR .AND. SCLD(1,1).GT.0) THEN
         IF (LDSCLD.GE.N) THEN
            LAST = N
         ELSE
            LAST = 1
         END IF
         DO 120 J=1,M
            DO 110 I=1,LAST
               IF (SCLD(I,J).LE.0) THEN
                  INFO = 30200
                  GO TO 130
               END IF
  110       CONTINUE
  120    CONTINUE
      END IF
  130 CONTINUE

!  Check BETA scaling

      IF (SCLB(1).GT.0) THEN
         DO 210 K=1,NP
            IF (SCLB(K).LE.0) THEN
               IF (INFO.EQ.0) THEN
                  INFO = 30100
               ELSE
                  INFO = INFO + 100
               END IF
               GO TO 220
            END IF
  210    CONTINUE
      END IF
  220 CONTINUE

!  Check DELTA finite difference step sizes

      IF (ANAJAC .AND. ISODR .AND. STPD(1,1).GT.0) THEN
         IF (LDSTPD.GE.N) THEN
            LAST = N
         ELSE
            LAST = 1
         END IF
         DO 320 J=1,M
            DO 310 I=1,LAST
               IF (STPD(I,J).LE.0) THEN
                  IF (INFO.EQ.0) THEN
                     INFO = 32000
                  ELSE
                     INFO = INFO + 2000
                  END IF
                  GO TO 330
               END IF
  310       CONTINUE
  320    CONTINUE
      END IF
  330 CONTINUE

!  Check BETA finite difference step sizes

      IF (ANAJAC .AND. STPB(1).GT.0) THEN
         DO 410 K=1,NP
            IF (STPB(K).LE.0) THEN
               IF (INFO.EQ.0) THEN
                  INFO = 31000
               ELSE
                  INFO = INFO + 1000
               END IF
               GO TO 420
            END IF
  410    CONTINUE
      END IF
  420 CONTINUE

!  Check bounds

      IF (ANY(UPPER(1:NP).LT.LOWER(1:NP))) THEN
         IF (INFO.EQ.0) THEN
            INFO = 91000
         END IF
      END IF

      IF (ANY((UPPER(1:NP).LT.BETA(1:NP).OR.LOWER(1:NP).GT.BETA(1:NP)) &
          .AND..NOT.UPPER(1:NP).LT.LOWER(1:NP))) THEN
         IF (INFO.GE.90000) THEN
            INFO = INFO + 100
         ELSE
            INFO = 90100
         END IF
      END IF

      RETURN
      END SUBROUTINE
!DODCNT
      SUBROUTINE DODCNT(FCN, N,M,NP,NQ, BETA, Y,LDY,X,LDX,             &
                        WE,LDWE,LD2WE,WD,LDWD,LD2WD, IFIXB,IFIXX,LDIFX,&
                        JOB,NDIGIT,TAUFAC, SSTOL,PARTOL,MAXIT,IPRINT,  &
                        LUNERR,LUNRPT,                                 &
                        STPB,STPD,LDSTPD, SCLB,SCLD,LDSCLD,            &
                        WORK,LWORK,IWORK,LIWORK,INFO,LOWER,UPPER)
!***Begin Prologue  DODCNT
!***Refer to  ODR
!***Routines Called  DODDRV
!***Date Written   860529   (YYMMDD)
!***Revision Date  920304   (YYMMDD)
!***Purpose  REAL (KIND=R8) driver routine for finding
!            the weighted explicit or implicit orthogonal distance 
!            regression (ODR) or ordinary linear or nonlinear least 
!            squares (OLS) solution
!***End Prologue  DODCNT

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      REAL (KIND=R8) PARTOL,SSTOL,TAUFAC
      INTEGER INFO,IPRINT,JOB,LDIFX,LDSCLD,LDSTPD,LDWD,LDWE,LDX,LDY,   &
              LD2WD,LD2WE,LIWORK,LUNERR,LUNRPT,LWORK,M,MAXIT,N,NDIGIT, &
              NP,NQ

!...Array arguments
      REAL (KIND=R8) BETA(NP),LOWER(NP),SCLB(NP),SCLD(LDSCLD,M),       &
                     STPB(NP),STPD(LDSTPD,M),UPPER(NP),                &
                     WD(LDWD,LD2WD,M),WE(LDWE,LD2WE,NQ),WORK(LWORK),   &
                     X(LDX,M),Y(LDY,NQ)
      INTEGER IFIXB(NP),IFIXX(LDIFX,M),IWORK(LIWORK)

!...Subroutine arguments
      EXTERNAL FCN

!...Local scalars
      REAL (KIND=R8) CNVTOL,ONE,PCHECK,PFAC,PSTART,THREE,TSTIMP,ZERO
      INTEGER IPRNTI,IPR1,IPR2,IPR2F,IPR3,JOBI,JOB1,JOB2,JOB3,JOB4,    &
              JOB5,MAXITI,MAXIT1
      LOGICAL DONE,FSTITR,HEAD,IMPLCT,PRTPEN

!...Local arrays
      REAL (KIND=R8) PNLTY(1,1,1)

!...External subroutines
      EXTERNAL DODDRV

!...External functions

!...Data statements
      DATA PCHECK,PSTART,PFAC,ZERO,ONE,THREE                           &
           /1.0E3_R8,1.0E1_R8,1.0E1_R8,0.0E0_R8,1.0E0_R8,3.0E0_R8/

!...Routine names used as subprogram arguments
!   FCN:     The user-supplied subroutine for evaluating the model.

!...Variable Definitions (alphabetically)
!   BETA:    The function parameters.
!   CNVTOL:  The convergence tolerance for implicit models.
!   DONE:    The variable designating whether the inplicit solution has 
!            been found (DONE=TRUE) or not (DONE=FALSE).
!   FSTITR:  The variable designating whether this is the first 
!            iteration (FSTITR=TRUE) or not (FSTITR=FALSE).
!   HEAD:    The variable designating whether the heading is to be 
!            printed (HEAD=TRUE) or not (HEAD=FALSE).
!   IFIXB:   The values designating whether the elements of BETA are 
!            fixed at their input values or not.
!   IFIXX:   The values designating whether the elements of X are 
!            fixed at their input values or not.
!   IMPLCT:  The variable designating whether the solution is by 
!            implicit ODR (IMPLCT=TRUE) or explicit ODR (IMPLCT=FALSE).
!   INFO:    The variable designating why the computations were stopped.
!   IPRINT:  The print control variables.
!   IPRNTI:  The print control variables.
!   IPR1:    The 1st digit of the print control variable.
!   IPR2:    The 2nd digit of the print control variable.
!   IPR3:    The 3rd digit of the print control variable.
!   IPR4:    The 4th digit of the print control variable.
!   IWORK:   The integer work space.
!   JOB:     The variable controling problem initialization and 
!            computational method.
!   JOBI:    The variable controling problem initialization and 
!            computational method.
!   JOB1:    The 1st digit of the variable controling problem 
!            initialization and computational method.
!   JOB2:    The 2nd digit of the variable controling problem 
!            initialization and computational method.
!   JOB3:    The 3rd digit of the variable controling problem 
!            initialization and computational method.
!   JOB4:    The 4th digit of the variable controling problem 
!            initialization and computational method.
!   JOB5:    The 5th digit of the variable controling problem 
!            initialization and computational method.
!   LDIFX:   The leading dimension of array IFIXX.
!   LDSCLD:  The leading dimension of array SCLD.
!   LDSTPD:  The leading dimension of array STPD.
!   LDWD:    The leading dimension of array WD.
!   LDWE:    The leading dimension of array WE.
!   LDX:     The leading dimension of array X.
!   LDY:     The leading dimension of array Y.
!   LD2WD:   The second dimension of array WD.
!   LD2WE:   The second dimension of array WE.
!   LIWORK:  The length of vector IWORK.
!   LOWER:   The lower bound for BETA.
!   LUNERR:  The logical unit number used for error messages.
!   LUNRPT:  The logical unit number used for computation reports.
!   LWORK:   The length of vector work.
!   M:       The number of columns of data in the independent variable.
!   MAXIT:   The maximum number of iterations allowed.
!   MAXITI:  For implicit models, the number of iterations allowed for
!            The current penalty parameter value.
!   MAXIT1:  For implicit models, the number of iterations allowed for
!            the next penalty parameter value.
!   N:       The number of observations.
!   NDIGIT:  The number of accurate digits in the function results, as
!            supplied by the user.
!   NP:      The number of function parameters.
!   NQ:      The number of responses per observation.
!   ONE:     The value 1.0E0_R8.
!   PARTOL:  The user supplied parameter convergence stopping tolerance.
!   PCHECK:  The value designating the minimum penalty parameter allowed
!            before the implicit problem can be considered solved.
!   PFAC:    The factor for increasing the penalty parameter.
!   PNLTY:   The penalty parameter for an implicit model.
!   PRTPEN:  The value designating whether the penalty parameter is to be
!            printed in the iteration report (PRTPEN=TRUE) or not
!            (PRTPEN=FALSE).
!   PSTART:  The factor for increasing the penalty parameter.
!   SCLB:    The scaling values for BETA.
!   SCLD:    The scaling values for DELTA.
!   STPB:    The relative step for computing finite difference 
!            Derivatives with respect to BETA.
!   STPD:    The relative step for computing finite difference 
!            Derivatives with respect to DELTA.
!   SSTOL:   The sum-of-squares convergence stopping tolerance.
!   TAUFAC:  The factor used to compute the initial trust region 
!            diameter.
!   THREE:   The value 3.0E0_R8.
!   TSTIMP:  The relative change in the parameters between the initial
!            values and the solution.
!   UPPER:   The upper bound for BETA.
!   WD:      The DELTA weights.
!   WE:      The EPSILON weights.
!   WORK:    The REAL (KIND=R8) work space.
!   X:       The independent variable.
!   Y:       The dependent variable.  Unused when the model is implicit.
!   ZERO:    The value 0.0E0_R8.


!***First executable statement  DODCNT


      IMPLCT = MOD(JOB,10).EQ.1
      FSTITR = .TRUE.
      HEAD   = .TRUE.
      PRTPEN = .FALSE.
 
      IF (IMPLCT) THEN 

!  Set up for implicit problem

         IF (IPRINT.GE.0) THEN
            IPR1   = MOD(IPRINT,10000)/1000
            IPR2   = MOD(IPRINT,1000)/100
            IPR2F  = MOD(IPRINT,100)/10
            IPR3   = MOD(IPRINT,10)
         ELSE
            IPR1   = 2
            IPR2   = 0
            IPR2F  = 0
            IPR3   = 1
         END IF
         IPRNTI = IPR1*1000 + IPR2*100 + IPR2F*10 

         JOB5   = MOD(JOB,100000)/10000
         JOB4   = MOD(JOB,10000)/1000
         JOB3   = MOD(JOB,1000)/100
         JOB2   = MOD(JOB,100)/10
         JOB1   = MOD(JOB,10)
         JOBI   = JOB5*10000 + JOB4*1000 + JOB3*100 + JOB2*10 + JOB1

         IF (WE(1,1,1).LE.ZERO) THEN
            PNLTY(1,1,1)  = -PSTART
         ELSE
            PNLTY(1,1,1)  = -WE(1,1,1)
         END IF

         IF (PARTOL.LT.ZERO) THEN
            CNVTOL = EPSILON(ZERO)**(ONE/THREE)
         ELSE
            CNVTOL = MIN(PARTOL,ONE)
         END IF

         IF (MAXIT.GE.1) THEN
            MAXITI = MAXIT
         ELSE
            MAXITI = 100
         END IF

         DONE   = MAXITI.EQ.0
         PRTPEN = .TRUE.

   10    CONTINUE
            CALL DODDRV(HEAD,FSTITR,PRTPEN,                            &
                 FCN, N,M,NP,NQ, BETA, Y,LDY,X,LDX,                    &
                 PNLTY,1,1,WD,LDWD,LD2WD, IFIXB,IFIXX,LDIFX,           &
                 JOBI,NDIGIT,TAUFAC, SSTOL,CNVTOL,MAXITI,              &
                 IPRNTI,LUNERR,LUNRPT,                                 &
                 STPB,STPD,LDSTPD, SCLB,SCLD,LDSCLD,                   &
                 WORK,LWORK,IWORK,LIWORK,                              &
     &           MAXIT1,TSTIMP, INFO, LOWER,UPPER) 

            IF (DONE) THEN
               RETURN
            ELSE
               DONE = MAXIT1.LE.0 .OR.                                 &
                    (ABS(PNLTY(1,1,1)).GE.PCHECK .AND. TSTIMP.LE.CNVTOL)
            END IF

            IF (DONE) THEN
               IF (TSTIMP.LE.CNVTOL) THEN
                  INFO = (INFO/10)*10 + 2
               ELSE
                  INFO = (INFO/10)*10 + 4
               END IF
               JOBI = 10000 + 1000 + JOB3*100 + JOB2*10 + JOB1
               MAXITI = 0
               IPRNTI = IPR3
            ELSE
               PRTPEN = .TRUE.
               PNLTY(1,1,1) = PFAC*PNLTY(1,1,1)
               JOBI = 10000 + 1000 + 000 + JOB2*10 + JOB1
               MAXITI = MAXIT1
               IPRNTI = 0000 + IPR2*100 + IPR2F*10 
            END IF
         GO TO 10
      ELSE        
         CALL DODDRV(HEAD,FSTITR,PRTPEN,                               &
              FCN, N,M,NP,NQ, BETA, Y,LDY,X,LDX,                       &
              WE,LDWE,LD2WE,WD,LDWD,LD2WD, IFIXB,IFIXX,LDIFX,          &
              JOB,NDIGIT,TAUFAC, SSTOL,PARTOL,MAXIT,                   &
              IPRINT,LUNERR,LUNRPT,                                    &
              STPB,STPD,LDSTPD, SCLB,SCLD,LDSCLD,                      &
              WORK,LWORK,IWORK,LIWORK,                                 &
              MAXIT1,TSTIMP, INFO, LOWER,UPPER)
      END IF

      RETURN

      END SUBROUTINE
!DODDRV
      SUBROUTINE DODDRV(HEAD,FSTITR,PRTPEN,                            &
                        FCN,  N,M,NP,NQ, BETA, Y,LDY,X,LDX,            &
                        WE,LDWE,LD2WE,WD,LDWD,LD2WD, IFIXB,IFIXX,LDIFX,&
                        JOB,NDIGIT,TAUFAC, SSTOL,PARTOL,MAXIT,         &
                        IPRINT,LUNERR,LUNRPT,                          &
                        STPB,STPD,LDSTPD, SCLB,SCLD,LDSCLD,            &
                        WORK,LWORK,IWORK,LIWORK,                       &
                        MAXIT1,TSTIMP, INFO, LOWER,UPPER)
!***Begin Prologue  DODDRV
!***Refer to  ODR
!***Routines Called  FCN,DCOPY,DDOT,DETAF,DFCTRW,DFLAGS,
!                    DINIWK,DIWINF,DJCK,DNRM2,DODCHK,DODMN,
!                    DODPER,DPACK,DSETN,DUNPAC,DWGHT,DWINF,DXMY,DXPY,
!                    DERSTEP
!***Date Written   860529   (YYMMDD)
!***Revision Date  920619   (YYMMDD)
!***Purpose  Perform error checking and initialization, and begin
!            procedure for performing orthogonal distance regression
!            (ODR) or ordinary linear or nonlinear least squares (OLS)
!***End Prologue  DODDRV

!...Used modules
      USE REAL_PRECISION
      USE ODRPACK95, ONLY : TEMPRET

!...Scalar arguments
      REAL (KIND=R8) PARTOL,SSTOL,TAUFAC,TSTIMP
      INTEGER INFO,IPRINT,JOB,LDIFX,LDSCLD,LDSTPD,LDWD,LDWE,LDX,LDY,   &
              LD2WD,LD2WE,LIWORK,LUNERR,LUNRPT,LWORK,M,MAXIT,MAXIT1,   &
              N,NDIGIT,NP,NQ
      LOGICAL FSTITR,HEAD,PRTPEN

!...Array arguments
      REAL (KIND=R8) BETA(NP),LOWER(NP),SCLB(NP),SCLD(LDSCLD,M),       &
                     STPB(NP),STPD(LDSTPD,M),UPPER(NP),                &
                     WE(LDWE,LD2WE,NQ),WD(LDWD,LD2WD,M),WORK(LWORK),   &
                     X(LDX,M),Y(LDY,NQ)
      INTEGER IFIXB(NP),IFIXX(LDIFX,M),IWORK(LIWORK)

!...Subroutine arguments
      EXTERNAL FCN

!...Local scalars
      REAL (KIND=R8) EPSMAC,ETA,P5,ONE,TEN,ZERO
      INTEGER ACTRSI,ALPHAI,BETACI,BETANI,BETASI,BETA0I,BOUNDI,DELTAI, &
              DELTNI,DELTSI,DIFFI,EPSMAI,ETAI,FI,FJACBI,FJACDI,FNI,    &
              FSI,I,IDFI,INT2I,IPRINI,IRANKI,ISTOP,ISTOPI,JOBI,JPVTI,  &
              K,LDTT,LDTTI,LIWKMN,LOWERI,LUNERI,LUNRPI,LWKMN,LWRK,     &
              MAXITI,MSGB,MSGD,NETA,NETAI,NFEV,NFEVI,NITERI,NJEV,      &
              NJEVI,NNZW,NNZWI,NPP,NPPI,NROW,NROWI,NTOL,NTOLI,OLMAVI,  &
              OMEGAI,PARTLI,PNORMI,PRERSI,QRAUXI,RCONDI,RNORSI,RVARI,  &
              SDI,SI,SSFI,SSI,SSTOLI,TAUFCI,TAUI,TI,TTI,UI,UPPERI,     &
              VCVI,WE1I,WRK1I,WRK2I,WRK3I,WRK4I,WRK5I,WRK6I,WRK7I,WRK, &
              WSSI,WSSDEI,WSSEPI,XPLUSI
      LOGICAL ANAJAC,CDJAC,CHKJAC,DOVCV,IMPLCT,INITD,ISODR,REDOJ,RESTRT

!...Local arrays
      REAL (KIND=R8) BETAJ(NP)
      INTEGER INTERVAL(NP)

!...External functions
      REAL (KIND=R8) DDOT,DNRM2,DERSTEP
      EXTERNAL DDOT,DNRM2,DERSTEP

!...External subroutines
      EXTERNAL DCOPY,DETAF,DFCTRW,DFLAGS,DINIWK,DIWINF,DJCK,DODCHK,    &
               DODMN,DODPER,DPACK,DSETN,DUNPAC,DWINF,DXMY,DXPY

!...Data statements
      DATA ZERO,P5,ONE,TEN /0.0E0_R8,0.5E0_R8,1.0E0_R8,10.0E0_R8/

!...Interface blocks
      INTERFACE
      SUBROUTINE DWGHT (N,M,WT,LDWT,LD2WT,T,WTT)
      USE REAL_PRECISION
      INTEGER LDWT,LD2WT,M,N
      REAL (KIND=R8) T(:,:),WT(:,:,:),WTT(:,:)
      END SUBROUTINE
      END INTERFACE

!...Routine names used as subprogram arguments
!   FCN:     THE USER SUPPLIED SUBROUTINE FOR EVALUATING THE MODEL.

!...Variable Definitions (alphabetically)
!   ACTRSI:  The location in array work of variable ACTRS.
!   ALPHAI:  The location in array work of variable ALPHA.
!   ANAJAC:  The variable designating whether the Jacobians are 
!            computed by finite differences (ANAJAC=FALSE) or not
!            (ANAJAC=TRUE).
!   BETA:    The function parameters.
!   BETACI:  The starting location in array WORK of array BETAC.
!   BETAJ:   The parameters to use in the derivative checking algorithm.
!   BETANI:  The starting location in array WORK of array BETAN.
!   BETASI:  The starting location in array WORK of array BETAS.
!   BETA0I:  The starting location in array WORK of array BETA0.
!   CDJAC:   The variable designating whether the Jacobians are 
!            Computed by central differences (CDJAC=TRUE) or forward
!            differences (CDJAC=FALSE).
!   CHKJAC:  The variable designating whether the user supplied 
!            Jacobians are to be checked (CHKJAC=TRUE) or not
!            (CHKJAC=FALSE).
!   DELTAI:  The starting location in array WORK of array DELTA.
!   DELTNI:  The starting location in array WORK of array DELTAN.
!   DELTSI:  The starting location in array WORK of array DELTAS.
!   DIFFI:   The starting location in array WORK of array DIFF.
!   DOVCV:   The variable designating whether the covariance matrix is 
!            to be computed (DOVCV=TRUE) or not (DOVCV=FALSE).
!   EPSMAI:  The location in array WORK of variable EPSMAC.
!   ETA:     The relative noise in the function results.
!   ETAI:    The location in array WORK of variable ETA.
!   FI:      The starting location in array WORK of array F.
!   FJACBI:  The starting location in array WORK of array FJACB.
!   FJACDI:  The starting location in array WORK of array FJACD.
!   FNI:     The starting location in array WORK of array FN.
!   FSI:     The starting location in array WORK of array FS.
!   FSTITR:  The variable designating whether this is the first 
!            iteration (FSTITR=TRUE) or not (FSTITR=FALSE).
!   HEAD:    The variable designating whether the heading is to be 
!            printed (HEAD=TRUE) or not (HEAD=FALSE).
!   I:       An index variable.
!   IDFI:    The location in array iwork of variable IDF.
!   IFIXB:   The values designating whether the elements of BETA are 
!            fixed at their input values or not.
!   IFIXX:   The values designating whether the elements of X are 
!            fixed at their input values or not.
!   IMPLCT:  The variable designating whether the solution is by 
!            implicit ODR (IMPLCT=TRUE) or explicit ODR (IMPLCT=FALSE). 
!   INFO:    The variable designating why the computations were stopped.
!   INITD:   The variable designating whether DELTA is to be initialized
!            to zero (INITD=TRUE) or to the values in the first N by M
!            elements of array WORK (INITD=FALSE).
!   INT2I:   The location in array IWORK of variable INT2.
!   INTERVAL: Specifies which checks can be performed when checking derivatives
!            based on the interval of the bound constraints.
!   IPRINI:  The location in array iwork of variable IPRINT.
!   IPRINT:  The print control variable.
!   IRANKI:  The location in array IWORK of variable IRANK.
!   ISODR:   The variable designating whether the solution is by ODR 
!            (ISODR=TRUE) or by OLS (ISODR=FALSE).
!   ISTOP:   The variable designating whether there are problems
!            computing the function at the current BETA and DELTA.
!   ISTOPI:  The location in array IWORK of variable ISTOP.
!   IWORK:   The integer work space.
!   JOB:     The variable controling problem initialization and 
!            computational method.
!   JOBI:    The location in array IWORK of variable JOB.
!   JPVTI:   The starting location in array IWORK of array JPVT.
!   K:       An index variable.
!   LDIFX:   The leading dimension of array IFIXX.
!   LDSCLD:  The leading dimension of array SCLD.
!   LDSTPD:  The leading dimension of array STPD.
!   LDTT:    The leading dimension of array TT.
!   LDTTI:   The location in array IWORK of variable LDTT.
!   LDWD:    The leading dimension of array WD.
!   LDWE:    The leading dimension of array WE.
!   LDX:     The leading dimension of array X.
!   LDY:     The leading dimension of array Y.
!   LD2WD:   The second dimension of array WD.
!   LD2WE:   The second dimension of array WE.
!   LIWKMN:  The minimum acceptable length of array IWORK.
!   LIWORK:  The length of vector IWORK.
!   LOWER:   The lower bound for BETA.
!   LUNERI:  The location in array IWORK of variable LUNERR.
!   LUNERR:  The logical unit number used for error messages.
!   LUNRPI:  The location in array IWORK of variable LUNRPT.
!   LUNRPT:  The logical unit number used for computation reports.
!   LWKMN:   The minimum acceptable length of array WORK.
!   LWORK:   The length of vector WORK.
!   LWRK:    The length of vector WRK.
!   M:       The number of columns of data in the explanatory variable.
!   MAXIT:   The maximum number of iterations allowed.
!   MAXIT1:  For implicit models, the iterations allowed for the next 
!            penalty parameter value.
!   MAXITI:  The location in array IWORK of variable MAXIT.
!   MSGB:    The starting location in array IWORK of array MSGB.
!   MSGD:    The starting location in ARRAY IWORK of array MSGD.
!   N:       The number of observations.
!   NDIGIT:  The number of accurate digits in the function results, as
!            supplied by the user.
!   NETA:    The number of accurate digits in the function results.
!   NETAI:   The location in array IWORK of variable NETA.
!   NFEV:    The number of function evaluations.
!   NFEVI:   The location in array IWORK of variable NFEV.
!   NITERI:  The location in array IWORK of variable NITER.
!   NJEV:    The number of Jacobian evaluations.
!   NJEVI:   The location in array IWORK of variable NJEV.
!   NNZW:    The number of nonzero observational error weights.
!   NNZWI:   The location in array IWORK of variable NNZW.
!   NP:      The number of function parameters.
!   NPP:     The number of function parameters being estimated.
!   NPPI:    The location in array IWORK of variable NPP.
!   NQ:      The number of responses per observation.
!   NROW:    The row number at which the derivative is to be checked.
!   NROWI:   The location in array IWORK of variable NROW.
!   NTOL:    The number of digits of agreement required between the
!            numerical derivatives and the user supplied derivatives,
!            set by DJCK.
!   NTOLI:   The location in array IWORK of variable NTOL.
!   OLMAVI:  The location in array WORK of variable OLMAVG.
!   OMEGAI:  The starting location in array WORK of array OMEGA.
!   ONE:     The value 1.0E0_R8.
!   PARTLI:  The location in array WORK of variable PARTOL.
!   PARTOL:  The parameter convergence stopping tolerance.
!   PNORM:   The norm of the scaled estimated parameters.
!   PNORMI:  The location in array WORK of variable PNORM.
!   PRERSI:  The location in array WORK of variable PRERS.
!   PRTPEN:  The variable designating whether the penalty parameter is 
!            to be printed in the iteration report (PRTPEN=TRUE) or not 
!            (PRTPEN=FALSE).
!   P5:      The value 0.5E0_R8.
!   QRAUXI:  The starting location in array WORK of array QRAUX.
!   RCONDI:  The location in array WORK of variable RCOND.
!   REDOJ:   The variable designating whether the Jacobian matrix is to 
!            be recomputed for the computation of the covariance matrix 
!            (REDOJ=TRUE) or not (REDOJ=FALSE).
!   RESTRT:  The variable designating whether the call is a restart 
!            (RESTRT=TRUE) or not (RESTRT=FALSE).
!   RNORSI:  The location in array WORK of variable RNORMS.
!   RVARI:   The location in array WORK of variable RVAR.
!   SCLB:    The scaling values for BETA.
!   SCLD:    The scaling values for DELTA.
!   SDI:     The starting location in array WORK of array SD.
!   SI:      The starting location in array WORK of array S.
!   SSFI:    The starting location in array WORK of array SSF.
!   SSI:     The starting location in array WORK of array SS.
!   SSTOL:   The sum-of-squares convergence stopping tolerance.
!   SSTOLI:  The location in array WORK of variable SSTOL.
!   STPB:    The step size for finite difference derivatives wrt BETA.
!   STPD:    The step size for finite difference derivatives wrt DELTA.
!   TAUFAC:  The factor used to compute the initial trust region 
!            diameter.
!   TAUFCI:  The location in array WORK of variable TAUFAC.
!   TAUI:    The location in array WORK of variable TAU.
!   TEN:     The value 10.0E0_R8.
!   TI:      The starting location in array WORK of array T.
!   TSTIMP:  The relative change in the parameters between the initial
!            values and the solution.
!   TTI:     The starting location in array WORK of array TT.
!   UI:      The starting location in array WORK of array U.
!   UPPER:   The upper bound for BETA.
!   VCVI:    The starting location in array WORK of array VCV.
!   WD:      The DELTA weights.
!   WE:      The EPSILON weights.
!   WE1I:    The starting location in array WORK of array WE1.
!   WORK:    The REAL (KIND=R8) work space.
!   WRK:     The starting location in array WORK of array WRK,
!            equivalenced to WRK1 and WRK2.
!   WRK1I:   The starting location in array WORK of array WRK1.
!   WRK2I:   The starting location in array WORK of array WRK2.
!   WRK3I:   The starting location in array WORK of array WRK3.
!   WRK4I:   The starting location in array WORK of array WRK4.
!   WRK5I:   The starting location in array WORK of array WRK5.
!   WRK6I:   The starting location in array WORK of array WRK6.
!   WRK7I:   The starting location in array WORK of array WRK7.
!   WSSI:    The location in array WORK of variable wss.
!   WSSDEI:  The location in array WORK of variable WSSDEL.
!   WSSEPI:  The location in array WORK of variable WSSEPS.
!   X:       The explanatory variable.
!   XPLUSI:  The starting location in array WORK of array XPLUSD.
!   Y:       The dependent variable.  Unused when the model is implicit.
!   ZERO:    The value 0.0E0_R8.


!***First executable statement  DODDRV


!  Initialize necessary variables

      CALL DFLAGS(JOB,RESTRT,INITD,DOVCV,REDOJ,                        &
                  ANAJAC,CDJAC,CHKJAC,ISODR,IMPLCT)

!  Set starting locations within integer workspace
!  (invalid values of M, NP and/or NQ are handled reasonably by DIWINF)

      CALL DIWINF(M,NP,NQ,MSGB,MSGD,JPVTI,ISTOPI,NNZWI,NPPI,IDFI,      &
                  JOBI,IPRINI,LUNERI,LUNRPI,NROWI,NTOLI,NETAI,         &
                  MAXITI,NITERI,NFEVI,NJEVI,INT2I,IRANKI,LDTTI,BOUNDI, &
                  LIWKMN)

!  Set starting locations within REAL (KIND=R8) work space
!  (invalid values of N, M, NP, NQ, LDWE and/or LD2WE 
!  are handled reasonably by DWINF)

      CALL DWINF(N,M,NP,NQ,LDWE,LD2WE,ISODR,DELTAI,FI,XPLUSI,FNI,SDI,  &
                 VCVI,RVARI,WSSI,WSSDEI,WSSEPI,RCONDI,ETAI,            &
                 OLMAVI,TAUI,ALPHAI,ACTRSI,PNORMI,RNORSI,PRERSI,       &
                 PARTLI,SSTOLI,TAUFCI,EPSMAI,                          &
                 BETA0I,BETACI,BETASI,BETANI,SI,SSI,SSFI,QRAUXI,UI,    &
                 FSI,FJACBI,WE1I,DIFFI,                                &
                 DELTSI,DELTNI,TI,TTI,OMEGAI,FJACDI,                   &
                 WRK1I,WRK2I,WRK3I,WRK4I,WRK5I,WRK6I,WRK7I,            &
                 LOWERI,UPPERI,LWKMN)
      IF (ISODR) THEN
         WRK = WRK1I
         LWRK = N*M*NQ + N*NQ
      ELSE
         WRK = WRK2I
         LWRK = N*NQ
      END IF

!  Update the penalty parameters 
!  (WE(1,1,1) is not a user supplied array in this case)
      IF (RESTRT .AND. IMPLCT) THEN
         WE(1,1,1)  = MAX(WORK(WE1I)**2,ABS(WE(1,1,1)))
         WORK(WE1I) = -SQRT(ABS(WE(1,1,1)))
      END IF

      IF (RESTRT) THEN

!  Reset maximum number of iterations

         IF (MAXIT.GE.0) THEN
            IWORK(MAXITI) = IWORK(NITERI) + MAXIT
         ELSE
            IWORK(MAXITI) = IWORK(NITERI) + 10
         END IF

         IF (IWORK(NITERI).LT.IWORK(MAXITI)) THEN
            INFO = 0
         END IF

         IF (JOB.GE.0) IWORK(JOBI) = JOB
         IF (IPRINT.GE.0) IWORK(IPRINI) = IPRINT
         IF (PARTOL.GE.ZERO .AND. PARTOL.LT.ONE) WORK(PARTLI) = PARTOL
         IF (SSTOL.GE.ZERO .AND. SSTOL.LT.ONE) WORK(SSTOLI) = SSTOL

         WORK(OLMAVI) = WORK(OLMAVI)*IWORK(NITERI)

         IF (IMPLCT) THEN
            CALL DCOPY(N*NQ,WORK(FNI),1,WORK(FI),1)
         ELSE
            CALL DXMY(N,NQ,WORK(FNI),N,Y,LDY,WORK(FI),N)
         END IF
         CALL DWGHT(N,NQ,                                              &
            RESHAPE(WORK(WE1I:WE1I+LDWE*LD2WE*NQ-1),(/LDWE,LD2WE,NQ/)),&
            LDWE,LD2WE,RESHAPE(WORK(FI:FI+N*NQ-1),(/N,NQ/)),           &
            TEMPRET(1:N,1:NQ))
         WORK(FI:FI+N*NQ-1) = RESHAPE(TEMPRET(1:N,1:NQ),(/N*NQ/))
         WORK(WSSEPI) = DDOT(N*NQ,WORK(FI),1,WORK(FI),1)
         WORK(WSSI) = WORK(WSSEPI) + WORK(WSSDEI)

      ELSE

!  Perform error checking

         INFO = 0

         CALL DODCHK(N,M,NP,NQ,ISODR,ANAJAC,IMPLCT,BETA,IFIXB,         &
                     LDX,LDIFX,LDSCLD,LDSTPD,LDWE,LD2WE,LDWD,LD2WD,    &
                     LDY,LWORK,LWKMN,LIWORK,LIWKMN,                    &
                     SCLB,SCLD,STPB,STPD,INFO,LOWER,UPPER)
         IF (INFO.GT.0) THEN
            GO TO 50
         END IF

!  Initialize work vectors as necessary

         DO 10 I=N*M+N*NQ+1,LWORK
            WORK(I) = ZERO
   10    CONTINUE
         DO 20 I=1,LIWORK
            IWORK(I) = 0
   20    CONTINUE

         CALL DINIWK(N,M,NP,WORK,LWORK,IWORK,LIWORK,                   &
                     X,LDX,IFIXX,LDIFX,SCLD,LDSCLD,BETA,SCLB,          &
                     SSTOL,PARTOL,MAXIT,TAUFAC,JOB,IPRINT,LUNERR,      &
                     LUNRPT,LOWER,UPPER,EPSMAI,SSTOLI,PARTLI,MAXITI,   &
                     TAUFCI,JOBI,IPRINI,LUNERI,LUNRPI,                 &
                     SSFI,TTI,LDTTI,DELTAI,LOWERI,UPPERI,BOUNDI)

         IWORK(MSGB) = -1
         IWORK(MSGD) = -1
         WORK(TAUI)   = -WORK(TAUFCI)

!  Set up for parameter estimation -
!  Pull BETA's to be estimated and corresponding scale values
!  and store in WORK(BETACI) and WORK(SSI), respectively

         CALL DPACK(NP,IWORK(NPPI),WORK(BETACI),BETA,IFIXB)
         CALL DPACK(NP,IWORK(NPPI),WORK(SSI),WORK(SSFI),IFIXB)
         NPP = IWORK(NPPI)

!  Check that WD is positive definite and WE is positive semidefinite, 
!  saving factorization of WE, and counting number of nonzero weights

         CALL DFCTRW(N,M,NQ,NPP,ISODR,WE,LDWE,LD2WE,WD,LDWD,LD2WD,     &
                     WORK(WRK2I),WORK(WRK4I),WORK(WE1I),NNZW,INFO)
         IWORK(NNZWI) = NNZW

         IF (INFO.NE.0) THEN
            GO TO 50
         END IF

!  Evaluate the predicted values and
!               weighted EPSILONS at the starting point
 
         CALL DUNPAC(NP,WORK(BETACI),BETA,IFIXB)
         CALL DXPY(N,M,X,LDX,WORK(DELTAI),N,WORK(XPLUSI),N)
         ISTOP = 0
         CALL FCN(N,M,NP,NQ,N,M,NP,BETA,WORK(XPLUSI),IFIXB,IFIXX,LDIFX, &
                  002,WORK(FNI),WORK(WRK6I),WORK(WRK1I),ISTOP)
         IWORK(ISTOPI) = ISTOP
         IF (ISTOP.EQ.0) THEN
            IWORK(NFEVI) = IWORK(NFEVI) + 1
            IF (IMPLCT) THEN
               CALL DCOPY(N*NQ,WORK(FNI),1,WORK(FI),1)
            ELSE
               CALL DXMY(N,NQ,WORK(FNI),N,Y,LDY,WORK(FI),N)
            END IF
            CALL DWGHT(N,NQ,RESHAPE(WORK(WE1I:WE1I+LDWE*LD2WE*NQ-1),   &
                       (/LDWE,LD2WE,NQ/)),LDWE,LD2WE,                  &
                       RESHAPE(WORK(FI:FI+N*NQ-1),(/N,NQ/)),           &
                       TEMPRET(1:N,1:NQ))
            WORK(FI:FI+N*NQ-1) = RESHAPE(TEMPRET(1:N,1:NQ),(/N*NQ/))
         ELSE 
            INFO = 52000
            GO TO 50
         END IF

!  Compute norm of the initial estimates

         CALL DWGHT(NPP,1,RESHAPE(WORK(SSI:SSI+NPP-1),(/NPP,1,1/)),    &
                    NPP,1,RESHAPE(WORK(BETACI:BETACI+NPP-1),(/NPP,1/)),&
                    TEMPRET(1:NPP,1:1))
         WORK(WRK:WRK+NPP-1) = TEMPRET(1:NPP,1)
         IF (ISODR) THEN
            CALL DWGHT(N,M,RESHAPE(WORK(TTI:TTI+IWORK(LDTTI)*1*M-1),   &
                       (/IWORK(LDTTI),1,M/)),IWORK(LDTTI),1,           &
                       RESHAPE(WORK(DELTAI:DELTAI+N*M-1),(/N,M/)),     &
                       TEMPRET(1:N,1:M))
            WORK(WRK+NPP:WRK+NPP+N*M-1) =                              &
                 RESHAPE(TEMPRET(1:N,1:M),(/N*M/))
            WORK(PNORMI) = DNRM2(NPP+N*M,WORK(WRK),1)
         ELSE
            WORK(PNORMI) = DNRM2(NPP,WORK(WRK),1)
         END IF
 
!  Compute sum of squares of the weighted EPSILONS and weighted DELTAS
 
         WORK(WSSEPI) = DDOT(N*NQ,WORK(FI),1,WORK(FI),1)
         IF (ISODR) THEN
            CALL DWGHT(N,M,WD,LDWD,LD2WD,                              &
                       RESHAPE(WORK(DELTAI:DELTAI+N*M),(/N,M/)),       &
                       TEMPRET(1:N,1:M))
            WORK(WRK:WRK+N*M-1) = RESHAPE(TEMPRET(1:N,1:M),(/N*M/))
            WORK(WSSDEI) = DDOT(N*M,WORK(DELTAI),1,WORK(WRK),1)
         ELSE
            WORK(WSSDEI) = ZERO
         END IF
         WORK(WSSI) = WORK(WSSEPI) + WORK(WSSDEI)

!  Select first row of X + DELTA that contains no zeros

         NROW = -1
         CALL DSETN(N,M,WORK(XPLUSI),N,NROW)
         IWORK(NROWI) = NROW

!  Set number of good digits in function results

         EPSMAC = WORK(EPSMAI)
         IF (NDIGIT.LT.2) THEN
            IWORK(NETAI) = -1
            NFEV = IWORK(NFEVI)
            CALL DETAF(FCN,N,M,NP,NQ,WORK(XPLUSI),BETA,EPSMAC,NROW,    &
                       WORK(BETANI),WORK(FNI),IFIXB,IFIXX,LDIFX,       &
                       ISTOP,NFEV,ETA,NETA,                            &
                       WORK(WRK1I),WORK(WRK2I),WORK(WRK6I),            &
                       WORK(WRK7I),INFO,LOWER,UPPER)
            IWORK(ISTOPI) = ISTOP
            IWORK(NFEVI) = NFEV
            IF (ISTOP.NE.0.OR.INFO.NE.0) THEN
               IF (INFO.EQ.0) THEN
                  INFO = 53000
               END IF
               IWORK(NETAI) = 0
               WORK(ETAI) = ZERO
               GO TO 50
            ELSE
               IWORK(NETAI) = -NETA
               WORK(ETAI) = ETA
            END IF
         ELSE
            IWORK(NETAI) = MIN(NDIGIT,INT(P5-LOG10(EPSMAC)))
            WORK(ETAI) = MAX(EPSMAC,TEN**(-NDIGIT))
         END IF

!  Check bounds are large enough for derivative calculations.

         IF (.NOT.ANAJAC .OR. CHKJAC) THEN
            IF (CDJAC) THEN
               DO K=1,NP
                  IF (UPPER(K)-                                        &
                     ABS(2*DERSTEP(1,K,UPPER(K),WORK(SSFI),STPB,NETA)) &
                     .LT.LOWER(K)) THEN
                     INFO = 90020
                     GO TO 50
                  END IF
               END DO
            ELSE
               DO K=1,NP
                  IF (UPPER(K)-                                        &
                     ABS(2*DERSTEP(0,K,UPPER(K),WORK(SSFI),STPB,NETA)) &
                     .LT.LOWER(K)) THEN
                     INFO = 90020
                     GO TO 50
                  END IF
               END DO
            END IF
         END IF

!  CHECK DERIVATIVES IF NECESSARY

         IF (CHKJAC .AND. ANAJAC) THEN
            NTOL = -1
            NFEV = IWORK(NFEVI)
            NJEV = IWORK(NJEVI)
            NETA = IWORK(NETAI)
            LDTT = IWORK(LDTTI)
            ETA = WORK(ETAI)
            EPSMAC = WORK(EPSMAI)
!  ENSURE BETA IS NOT TOO CLOSE TO BOUNDS FOR THE DERIVATIVE CHECK.
            BETAJ(:) = BETA(:)
            CALL MBFB(NP,BETAJ,LOWER,UPPER,WORK(SSFI),STPB,NETA,ETA,   &
                      INTERVAL)
!  CHECK THE DERIVATIVES.
            CALL DJCK(FCN,N,M,NP,NQ,BETA,BETAJ,WORK(XPLUSI),           &
                      IFIXB,IFIXX,LDIFX,STPB,STPD,LDSTPD,              &
                      WORK(SSFI),WORK(TTI),LDTT,                       &
                      ETA,NETA,NTOL,NROW,ISODR,EPSMAC,                 &
                      WORK(FNI),WORK(FJACBI),WORK(FJACDI),             &
                      IWORK(MSGB),IWORK(MSGD),WORK(DIFFI),             &
                      ISTOP,NFEV,NJEV,                                 &
                      WORK(WRK1I),WORK(WRK2I),WORK(WRK6I),INTERVAL)
            IWORK(ISTOPI) = ISTOP
            IWORK(NFEVI) = NFEV
            IWORK(NJEVI) = NJEV
            IWORK(NTOLI) = NTOL
            IF (ISTOP.NE.0) THEN
               INFO = 54000
            ELSE IF (IWORK(MSGB).NE.0 .OR. IWORK(MSGD).NE.0) THEN
               INFO = 40000
            END IF
         ELSE

!  Indicate user supplied derivatives were not checked
            IWORK(MSGB) = -1
            IWORK(MSGD) = -1
         END IF

!  Print appropriate error messages

   50    IF ((INFO.NE.0) .OR. (IWORK(MSGB).NE.-1)) THEN
            IF (LUNERR.NE.0 .AND. IPRINT.NE.0) THEN
               CALL DODPER(INFO,LUNERR,N,M,NP,NQ,                      &
                           LDSCLD,LDSTPD,LDWE,LD2WE,LDWD,LD2WD,        &
                           LWKMN,LIWKMN,WORK(FJACBI),WORK(FJACDI),     &
                           WORK(DIFFI),IWORK(MSGB),ISODR,IWORK(MSGD),  &
                           WORK(XPLUSI),IWORK(NROWI),IWORK(NETAI),     &
                           IWORK(NTOLI))
            END IF

!  Set INFO to reflect errors in the user supplied Jacobians

            IF (INFO.EQ.40000) THEN
               IF (IWORK(MSGB).EQ.2 .OR. IWORK(MSGD).EQ.2) THEN
                  IF (IWORK(MSGB).EQ.2) THEN
                     INFO = INFO + 1000
                  END IF
                  IF (IWORK(MSGD).EQ.2) THEN
                     INFO = INFO + 100
                  END IF
               ELSE 
                  INFO = 0
               END IF
            END IF
            IF (INFO.NE.0) THEN
               RETURN
            END IF
         END IF
      END IF

!  Save the initial values of BETA
      CALL DCOPY(NP,BETA,1,WORK(BETA0I),1)

!  Find least squares solution

      CALL DCOPY(N*NQ,WORK(FNI),1,WORK(FSI),1)
      LDTT = IWORK(LDTTI)
      CALL DODMN(HEAD,FSTITR,PRTPEN,FCN, N,M,NP,NQ, JOB, BETA,Y,LDY,X, &
                 LDX,WE,WORK(WE1I),LDWE,LD2WE,WD,LDWD,LD2WD,           &
                 IFIXB,IFIXX,LDIFX,                                    &
                 WORK(BETACI),WORK(BETANI),WORK(BETASI),WORK(SI),      &
                 WORK(DELTAI),WORK(DELTNI),WORK(DELTSI),               &
                 WORK(LOWERI),WORK(UPPERI),                            &
                 WORK(TI),WORK(FI),WORK(FNI),WORK(FSI),                &
                 WORK(FJACBI),IWORK(MSGB),WORK(FJACDI),IWORK(MSGD),    &
                 WORK(SSFI),WORK(SSI),WORK(TTI),LDTT,                  &
                 STPB,STPD,LDSTPD,WORK(XPLUSI),WORK(WRK),LWRK,         &
                 WORK,LWORK,IWORK,LIWORK,INFO,IWORK(BOUNDI))
      MAXIT1 = IWORK(MAXITI) - IWORK(NITERI)
      TSTIMP = ZERO
      DO 100 K=1,NP
         IF (BETA(K).EQ.ZERO) THEN
            TSTIMP = MAX(TSTIMP,                                       &
                         ABS(BETA(K)-WORK(BETA0I-1+K))/WORK(SSFI-1+K))
         ELSE
            TSTIMP = MAX(TSTIMP,                                       &
                         ABS(BETA(K)-WORK(BETA0I-1+K))/ABS(BETA(K)))
         END IF
  100 CONTINUE

      RETURN

      END SUBROUTINE
!DODLM
      SUBROUTINE DODLM(N,M,NP,NQ,NPP,F,FJACB,FJACD,                    &
                       WD,LDWD,LD2WD,SS,TT,LDTT,DELTA,                 &
                       ALPHA2,TAU,EPSFCN,ISODR,                        &
                       TFJACB,OMEGA,U,QRAUX,JPVT,                      &
                       S,T,NLMS,RCOND,IRANK,                           &
                       WRK1,WRK2,WRK3,WRK4,WRK5,WRK,LWRK,ISTOPC)
!***Begin Prologue  DODLM
!***Refer to  ODR
!***Routines Called  DDOT,DNRM2,DODSTP,DSCALE,DWGHT
!***Date Written   860529   (YYMMDD)
!***Revision Date  920619   (YYMMDD)
!***Purpose  Compute Levenberg-Marquardt parameter and steps S AND T
!            using analog of the trust-region Levenberg-Marquardt
!            algorithm
!***End Prologue  DODLM

!...Used modules
      USE REAL_PRECISION
      USE ODRPACK95, ONLY : TEMPRET

!...Scalar arguments
      REAL (KIND=R8)ALPHA2,EPSFCN,RCOND,TAU
      INTEGER IRANK,ISTOPC,LDTT,LDWD,LD2WD,LWRK,M,N,NLMS,NP,NPP,NQ
      LOGICAL ISODR

!...Array arguments
      REAL (KIND=R8) DELTA(N,M),F(N,NQ),FJACB(N,NP,NQ),FJACD(N,M,NQ),  &
                     OMEGA(NQ,NQ),QRAUX(NP),S(NP),SS(NP),              &
                     T(N,M),TFJACB(N,NQ,NP),TT(LDTT,M),U(NP),          &
                     WD(LDWD,LD2WD,M),WRK(LWRK),WRK1(N,NQ,M),          &
                     WRK2(N,NQ),WRK3(NP),WRK4(M,M),WRK5(M)
      INTEGER JPVT(NP)

!...Local scalars
      REAL (KIND=R8) ALPHA1,ALPHAN,BOT,P001,P1,PHI1,PHI2,SA,TOP,ZERO
      INTEGER I,IWRK,J,K,L
      LOGICAL FORVCV

!...External functions
      REAL (KIND=R8) DDOT,DNRM2
      EXTERNAL DDOT,DNRM2

!...External subroutines
      EXTERNAL DODSTP,DSCALE

!...Data statements
      DATA ZERO,P001,P1 /0.0E0_R8,0.001E0_R8,0.1E0_R8/

!...Interface blocks
      INTERFACE
      SUBROUTINE DWGHT (N,M,WT,LDWT,LD2WT,T,WTT)
      USE REAL_PRECISION
      INTEGER LDWT,LD2WT,M,N
      REAL (KIND=R8) T(:,:),WT(:,:,:),WTT(:,:)
      END SUBROUTINE
      END INTERFACE

!...Variable Definitions (alphabetically)
!   ALPHAN:  The new Levenberg-Marquardt parameter.
!   ALPHA1:  The previous Levenberg-Marquardt parameter.
!   ALPHA2:  The current Levenberg-Marquardt parameter.
!   BOT:     The lower limit for setting ALPHA.
!   DELTA:   The estimated errors in the explanatory variables.
!   EPSFCN:  The function's precision.
!   F:       The (weighted) estimated values of EPSILON.
!   FJACB:   The Jacobian with respect to BETA.
!   FJACD:   The Jacobian with respect to DELTA.
!   FORVCV:  The variable designating whether this subroutine was 
!            called to set up for the covariance matrix computations 
!            (FORVCV=TRUE) or not (FORVCV=FALSE).
!   I:       An indexing variable.
!   IRANK:   The rank deficiency of the Jacobian wrt BETA.
!   ISODR:   The variable designating whether the solution is by ODR 
!            (ISODR=TRUE) or by OLS (ISODR=FALSE).
!   ISTOPC:  The variable designating whether the computations were
!            stoped due to some numerical error detected within 
!            subroutine DODSTP.
!   IWRK:    An indexing variable.
!   J:       An indexing variable.
!   K:       An indexing variable.
!   L:       An indexing variable.
!   JPVT:    The pivot vector.
!   LDTT:    The leading dimension of array TT.
!   LDWD:    The leading dimension of array WD.
!   LD2WD:   The second dimension of array WD.
!   LWRK:    The length of vector WRK.
!   M:       The number of columns of data in the explanatory variable.
!   N:       The number of observations.
!   NLMS:    The number of Levenberg-Marquardt steps taken.
!   NP:      The number of function parameters.
!   NPP:     The number of function parameters being estimated.
!   NQ:      The number of responses per observation.
!   OMEGA:   The array (I-FJACD*INV(P)*trans(FJACD))**(-1/2)  where
!            P = trans(FJACD)*FJACD + D**2 + ALPHA*TT**2
!   P001:    The value 0.001E0_R8
!   P1:      The value 0.1E0_R8
!   PHI1:    The previous difference between the norm of the scaled step
!            and the trust region diameter.
!   PHI2:    The current difference between the norm of the scaled step
!            and the trust region diameter.
!   QRAUX:   The array required to recover the orthogonal part of the
!            Q-R decomposition.
!   RCOND:   The approximate reciprocal condition of TFJACB.
!   S:       The step for BETA.
!   SA:      The scalar PHI2*(ALPHA1-ALPHA2)/(PHI1-PHI2).
!   SS:      The scaling values used for the unfixed BETAS.
!   T:       The step for DELTA.
!   TAU:     The trust region diameter.
!   TFJACB:  The array OMEGA*FJACB.
!   TOP:     The upper limit for setting ALPHA.
!   TT:      The scale used for the DELTA'S.
!   U:       The approximate null vector for TFJACB.
!   WD:      The DELTA weights.
!   WRK:     A work array of (LWRK) elements, 
!            equivalenced to WRK1 and WRK2.
!   WRK1:    A work array of (N by NQ by M) elements.
!   WRK2:    A work array of (N by NQ) elements.
!   WRK3:    A work array of (NP) elements.
!   WRK4:    A work array of (M by M) elements.
!   WRK5:    A work array of (M) elements.
!   ZERO:    The value 0.0E0_R8.


!***First executable statement  DODLM

      FORVCV = .FALSE.
      ISTOPC = 0

!  Compute full Gauss-Newton step (ALPHA=0)

      ALPHA1 = ZERO
      CALL DODSTP(N,M,NP,NQ,NPP,F,FJACB,FJACD,                         &
                  WD,LDWD,LD2WD,SS,TT,LDTT,DELTA,ALPHA1,EPSFCN,ISODR,  &
                  TFJACB,OMEGA,U,QRAUX,JPVT,S,T,PHI1,IRANK,RCOND,      &
                  FORVCV,WRK1,WRK2,WRK3,WRK4,WRK5,WRK,LWRK,ISTOPC)
      IF (ISTOPC.NE.0) THEN
         RETURN
      END IF

!  Initialize TAU if necessary

      IF (TAU.LT.ZERO) THEN
         TAU = ABS(TAU)*PHI1
      END IF

!  Check if full Gauss-Newton step is optimal

      IF ((PHI1-TAU).LE.P1*TAU) THEN
         NLMS = 1
         ALPHA2 = ZERO
         RETURN
      END IF

!  Full Gauss-Newton step is outside trust region -
!  find locally constrained optimal step

      PHI1 = PHI1 - TAU

!  Initialize upper and lower bounds for ALPHA

      BOT = ZERO

      DO 30 K=1,NPP
         DO 20 L=1,NQ
            DO 10 I=1,N
               TFJACB(I,L,K) = FJACB(I,K,L)
   10       CONTINUE
   20    CONTINUE
         WRK(K) = DDOT(N*NQ,TFJACB(1,1,K),1,F(1,1),1)
   30 CONTINUE
      CALL DSCALE(NPP,1,SS,NPP,WRK,NPP,WRK,NPP)

      IF (ISODR) THEN
         CALL DWGHT(N,M,WD,LDWD,LD2WD,DELTA,TEMPRET(1:N,1:M))
         WRK(NPP+1:NPP+1+N*M-1) = RESHAPE(TEMPRET(1:N,1:M),(/N*M/))
         IWRK = NPP
         DO 50 J=1,M
            DO 40 I=1,N
               IWRK = IWRK + 1
               WRK(IWRK) = WRK(IWRK) +                                 &
                           DDOT(NQ,FJACD(I,J,1),N*M,F(I,1),N)
   40       CONTINUE
   50    CONTINUE
         CALL DSCALE(N,M,TT,LDTT,WRK(NPP+1),N,WRK(NPP+1),N)
         TOP = DNRM2(NPP+N*M,WRK,1)/TAU
      ELSE
         TOP = DNRM2(NPP,WRK,1)/TAU
      END IF

      IF (ALPHA2.GT.TOP .OR. ALPHA2.EQ.ZERO) THEN
         ALPHA2 = P001*TOP
      END IF

!  Main loop

      DO 60 I=1,10

!  Compute locally constrained steps S and T and PHI(ALPHA) for
!  current value of ALPHA

         CALL DODSTP(N,M,NP,NQ,NPP,F,FJACB,FJACD,WD,                   &
                     LDWD,LD2WD,SS,TT,LDTT,DELTA,ALPHA2,EPSFCN,ISODR,  &
                     TFJACB,OMEGA,U,QRAUX,JPVT,S,T,PHI2,IRANK,RCOND,   &
                     FORVCV,WRK1,WRK2,WRK3,WRK4,WRK5,WRK,LWRK,ISTOPC)
         IF (ISTOPC.NE.0) THEN
            RETURN
         END IF
         PHI2 = PHI2-TAU

!  Check whether current step is optimal

         IF (ABS(PHI2).LE.P1*TAU .OR.                                  &
            (ALPHA2.EQ.BOT .AND. PHI2.LT.ZERO)) THEN
            NLMS = I+1
            RETURN
         END IF

!  Current step is not optimaL

!  Update bounds for ALPHA and compute new ALPHA

         IF (PHI1-PHI2.EQ.ZERO) THEN
            NLMS = 12
            RETURN
         END IF
         SA = PHI2*(ALPHA1-ALPHA2)/(PHI1-PHI2)
         IF (PHI2.LT.ZERO) THEN
            TOP = MIN(TOP,ALPHA2)
         ELSE
            BOT = MAX(BOT,ALPHA2)
         END IF
         IF (PHI1*PHI2.GT.ZERO) THEN
            BOT = MAX(BOT,ALPHA2-SA)
         ELSE
            TOP = MIN(TOP,ALPHA2-SA)
         END IF

         ALPHAN = ALPHA2 - SA*(PHI1+TAU)/TAU
         IF (ALPHAN.GE.TOP .OR. ALPHAN.LE.BOT) THEN
            ALPHAN = MAX(P001*TOP,SQRT(TOP*BOT))
         END IF

!  Get ready for next iteration

         ALPHA1 = ALPHA2
         ALPHA2 = ALPHAN
         PHI1 = PHI2
   60 CONTINUE

!  Set NLMS to indicate an optimal step could not be found in 10 trys

      NLMS = 12

      RETURN
      END SUBROUTINE
!DODMN
      SUBROUTINE DODMN(HEAD,FSTITR,PRTPEN,                             &
                       FCN, N,M,NP,NQ, JOB, BETA,Y,LDY,X,LDX,          &
                       WE,WE1,LDWE,LD2WE,WD,LDWD,LD2WD,                &
                       IFIXB,IFIXX,LDIFX,                              &
                       BETAC,BETAN,BETAS,S,DELTA,DELTAN,DELTAS,        &
                       LOWER,UPPER,T,F,FN,FS,FJACB,MSGB,FJACD,MSGD,    &
                       SSF,SS,TT,LDTT,STPB,STPD,LDSTPD,                &
                       XPLUSD,WRK,LWRK,WORK,LWORK,IWORK,LIWORK,INFO,   &
                       BOUND)
!***Begin Prologue  DODMN
!***Refer to  ODR
!***Routines Called  FCN,DACCES,DCOPY,DDOT,DEVJAC,DFLAGS,DNRM2,DODLM,
!                    DODPCR,DODVCV,DUNPAC,DWGHT,DXMY,DXPY
!***Date Written   860529   (YYMMDD)
!***Revision Date  920619   (YYMMDD)
!***Purpose  Iteratively compute least squares solution
!***End Prologue  DODMN

!...Used modules
      USE REAL_PRECISION
      USE ODRPACK95, ONLY : TEMPRET

!...Scalar arguments
      INTEGER INFO,JOB,LDIFX,LDSTPD,LDTT,LDWD,LDWE,LDX,LDY,LD2WD,LD2WE, &
              LIWORK,LWORK,LWRK,M,N,NP,NQ

!...Array arguments
      REAL (KIND=R8) BETA(NP),BETAC(NP),BETAN(NP),BETAS(NP),           &
                     DELTA(N,M),DELTAN(N,M),DELTAS(N,M),               &
                     F(N,NQ),FJACB(N,NP,NQ),FJACD(N,M,NQ),FN(N,NQ),    &
                     FS(N,NQ),LOWER(NP),                               &
                     S(NP),SS(NP),SSF(NP),STPB(NP),STPD(LDSTPD,M),     &
                     T(N,M),TT(LDTT,M),UPPER(NP),                      &
                     WD(LDWD,LD2WD,M),WE(LDWE,LD2WE,NQ),               &
                     WE1(LDWE,LD2WE,NQ),                               &
                     WORK(LWORK),X(LDX,M),XPLUSD(N,M),WRK(LWRK),Y(LDY,NQ)
      INTEGER BOUND(NP),IFIXB(NP),IFIXX(LDIFX,M),IWORK(LIWORK),        &
              MSGB(NQ*NP+1),MSGD(NQ*M+1)
      LOGICAL FSTITR,HEAD,PRTPEN

!...Subroutine arguments
      EXTERNAL FCN

!...Local scalars
      REAL (KIND=R8) ACTRED,ACTRS,ALPHA,DIRDER,ETA,OLMAVG,ONE,         &
                     P0001,P1,P25,P5,P75,PARTOL,PNORM,PRERED,PRERS,    &
                     RATIO,RCOND,RNORM,RNORMN,RNORMS,RSS,RVAR,SSTOL,   &
                     TAU,TAUFAC,TEMP,TEMP1,TEMP2,TSNORM,ZERO
      INTEGER I,IDF,IFLAG,INT2,IPR,IPR1,IPR2,IPR2F,IPR3,IRANK,         &
              ISTOP,ISTOPC,IWRK,J,JPVT,L,LOOPED,LUDFLT,LUNR,LUNRPT,    &
              MAXIT,NETA,NFEV,NITER,NJEV,NLMS,NNZW,NPP,NPR,NPU,OMEGA,  &
              QRAUX,SD,U,VCV,WRK1,WRK2,WRK3,WRK4,WRK5,WRK6
      LOGICAL ACCESS,ANAJAC,CDJAC,CHKJAC,CNVPAR,CNVSS,DIDVCV,DOVCV,    &
              IMPLCT,INITD,INTDBL,ISODR,LSTEP,REDOJ,RESTRT

!...Local arrays
      REAL (KIND=R8) LOWERU(NP),UPPERU(NP),WSS(3)

!...External functions
      REAL (KIND=R8) DDOT,DNRM2
      EXTERNAL DDOT,DNRM2

!...External subroutines
      EXTERNAL DACCES,DCOPY,DEVJAC,DFLAGS,                             &
               DODLM,DODPCR,DODVCV,DUNPAC,DXMY,DXPY

!...Data statements
      DATA ZERO,P0001,P1,P25,P5,P75,ONE                                &
           /0.0E0_R8,0.00010E0_R8,0.10E0_R8,0.250E0_R8,                &
            0.50E0_R8,0.750E0_R8,1.0E0_R8/
      DATA LUDFLT /6/

!...Interface blocks
      INTERFACE
      SUBROUTINE DWGHT (N,M,WT,LDWT,LD2WT,T,WTT)
      USE REAL_PRECISION
      INTEGER LDWT,LD2WT,M,N
      REAL (KIND=R8) T(:,:),WT(:,:,:),WTT(:,:)
      END SUBROUTINE
      END INTERFACE

!...Routine names used as subprogram arguments
!   FCN:     The user supplied subroutine for evaluating the model.

!...Variable Definitions (alphabetically)
!   ACCESS:  The variable designating whether information is to be 
!            accessed from the work arrays (ACCESS=TRUE) or stored in 
!            them (ACCESS=FALSE).
!   ACTRED:  The actual relative reduction in the sum-of-squares.
!   ACTRS:   The saved actual relative reduction in the sum-of-squares.
!   ALPHA:   The Levenberg-Marquardt parameter.
!   ANAJAC:  The variable designating whether the Jacobians are computed
!            by finite differences (ANAJAC=FALSE) or not (ANAJAC=TRUE).
!   BETA:    The function parameters.
!   BETAC:   The current estimated values of the unfixed BETA'S.
!   BETAN:   The new estimated values of the unfixed BETA'S.
!   BETAS:   The saved estimated values of the unfixed BETA'S.
!   CDJAC:   The variable designating whether the Jacobians are computed
!            by central differences (cdjac=true) or by forward
!            differences (CDJAC=FALSE).
!   CHKJAC:  The variable designating whether the user supplied
!            Jacobians are to be checked (CHKJAC=TRUE) or not
!            (CHKJAC=FALSE).
!   CNVPAR:  The variable designating whether parameter convergence was 
!            attained (CNVPAR=TRUE) or not (CNVPAR=FALSE).
!   CNVSS:   The variable designating whether sum-of-squares convergence
!            was attained (CNVSS=TRUE) or not (CNVSS=FALSE).
!   DELTA:   The estimated errors in the explanatory variables.
!   DELTAN:  The new estimated errors in the explanatory variables.
!   DELTAS:  The saved estimated errors in the explanatory variables.
!   DIDVCV:  The variable designating whether the covariance matrix was
!            computed (DIDVCV=TRUE) or not (DIDVCV=FALSE).
!   DIRDER:  The directional derivative.
!   DOVCV:   The variable designating whether the covariance matrix
!            should to be computed (DOVCV=TRUE) or not (DOVCV=FALSE).
!   ETA:     The relative noise in the function results.
!   F:       The (weighted) estimated values of EPSILON.
!   FJACB:   The Jacobian with respect to BETA.
!   FJACD:   The Jacobian with respect to DELTA.
!   FN:      The new predicted values from the function.
!   FS:      The saved predicted values from the function.
!   FSTITR:  The variable designating whether this is the first
!            iteration (FSTITR=TRUE) or not (FSTITR=FALSE).
!   HEAD:    The variable designating whether the heading is to be 
!            printed (HEAD=TRUE) or not (HEAD=FALSE).
!   I:       An indexing variable.
!   IDF:     The degrees of freedom of the fit, equal to the number of
!            observations with nonzero weighted derivatives minus the
!            number of parameters being estimated.
!   IFIXB:   The values designating whether the elements of BETA are 
!            fixed at their input values or not.
!   IFIXX:   The values designating whether the elements of X are 
!            fixed at their input values or not.
!   IFLAG:   The variable designating which report is to be printed.
!   IMPLCT:  The variable designating whether the solution is by 
!            implicit ODR (IMPLCT=TRUE) or explicit ODR (IMPLCT=FALSE). 
!   INFO:    The variable designating why the computations were stopped.
!   INITD:   The variable designating whether delta is initialized to 
!            zero (INITD=TRUE) or to the values in the first N by M
!            elements of array work (INITD=FALSE).
!   INT2:    The number of internal doubling steps taken.
!   INTDBL:  The variable designating whether internal doubling is to be 
!            used (INTDBL=TRUE) or NOT (INTDBL=FALSE).
!   IPR:     The values designating the length of the printed report.
!   IPR1:    The value of the 4th digit (from the right) of iprint,
!            which controls the initial summary report.
!   IPR2:    The value of the 3rd digit (from the right) of iprint,
!            which controls the iteration report.
!   IPR2F:   The value of the 2nd digit (from the right) of iprint,
!            which controls the frequency of the iteration reports.
!   IPR3:    The value of the 1st digit (from the right) of iprint,
!            which controls the final summary report.
!   IRANK:   The rank deficiency of the Jacobian wrt BETA.
!   ISODR:   The variable designating whether the solution is by ODR 
!            (ISODR=TRUE) or OLS (ISODR=FALSE).
!   ISTOP:   The variable designating whether there are problems
!            computing the function at the current BETA and DELTA.
!   ISTOPC:  The variable designating whether the computations were
!            stoped due to some numerical error within routine  DODSTP. 
!   IWORK:   The integer work space.
!   IWRK:    An index variable.
!   J:       An index variable.
!   JOB:     The variable controling problem initialization and 
!            computational method.
!   JPVT:    The starting location in IWORK of array JPVT.
!   L:       An index variable.
!   LDIFX:   The leading dimension of array IFIXX.
!   LDTT:    The leading dimension of array TT.
!   LDWD:    The leading dimension of array WD.
!   LDWE:    The leading dimension of array WE and WE1.
!   LDX:     The leading dimension of array X.
!   LDY:     The leading dimension of array Y.
!   LD2WD:   The second dimension of array WD.
!   LD2WE:   The second dimension of array WE and WE1.
!   LIWORK:  The length of vector IWORK.
!   LOOPED:  A counter used to determine how many times the subloop
!            has been executed, where if the count becomes large
!            enough the computations will be stopped.
!   LOWERU:  The lower bound for unfixed BETAs.
!   LSTEP:   The variable designating whether a successful step has 
!            been found (LSTEP=TRUE) or not (LSTEP=FALSE).
!   LUDFLT:  The default logical unit number, used for computation
!            reports to the screen.
!   LUNR:    The logical unit number used for computation reports.
!   LUNRPT:  The logical unit number used for computation reports.
!   LWORK:   The length of vector WORK.
!   LWRK:    The length of vector WRK.
!   M:       The number of columns of data in the explanatory variable.
!   MAXIT:   The maximum number of iterations allowed. 
!   MSGB:    The error checking results for the Jacobian wrt BETA.
!   MSGD:    The error checking results for the Jacobian wrt DELTA.
!   N:       The number of observations.
!   NETA:    The number of accurate digits in the function results.
!   NFEV:    The number of function evaluations.
!   NITER:   The number of iterations taken.
!   NJEV:    The number of Jacobian evaluations.
!   NLMS:    The number of Levenberg-Marquardt steps taken.
!   NNZW:    The number of nonzero weighted observations.
!   NP:      The number of function parameters.
!   NPP:     The number of function parameters being estimated.
!   NPR:     The number of times the report is to be written.
!   NPU:     The number of unfixed parameters.
!   NQ:      The number of responses per observation.
!   OLMAVG:  The average number of Levenberg-Marquardt steps per 
!            iteration.
!   OMEGA:   The starting location in WORK of array OMEGA.
!   ONE:     The value 1.0E0_R8.
!   P0001:   The value 0.0001E0_R8.
!   P1:      The value 0.1E0_R8.
!   P25:     The value 0.25E0_R8.
!   P5:      The value 0.5E0_R8.
!   P75:     The value 0.75E0_R8.
!   PARTOL:  The parameter convergence stopping tolerance.
!   PNORM:   The norm of the scaled estimated parameters.
!   PRERED:  The predicted relative reduction in the sum-of-squares.
!   PRERS:   The old predicted relative reduction in the sum-of-squares.
!   PRTPEN:  The value designating whether the penalty parameter is to
!            be printed in the iteration report (PRTPEN=TRUE) or not 
!            (PRTPEN=FALSE).
!   QRAUX:   The starting location in array WORK of array QRAUX.
!   RATIO:   The ratio of the actual relative reduction to the predicted
!            relative reduction in the sum-of-squares.
!   RCOND:   The approximate reciprocal condition of FJACB.
!   REDOJ:   The variable designating whether the Jacobian matrix is to
!            be recomputed for the computation of the covariance matrix 
!            (REDOJ=TRUE) or not (REDOJ=FALSE).
!   RESTRT:  The variable designating whether the call is a restart 
!            (RESTRT=TRUE) or not (RESTRT=FALSE).
!   RNORM:   The norm of the weighted errors.
!   RNORMN:  The new norm of the weighted errors.
!   RNORMS:  The saved norm of the weighted errors.
!   RSS:     The residual sum of squares.
!   RVAR:    The residual variance.
!   S:       The step for BETA.
!   SD:      The starting location in array work of array SD.
!   SS:      The scaling values used for the unfixed BETAS.
!   SSF:     The scaling values used for BETA.
!   SSTOL:   The sum-of-squares convergence stopping tolerance.
!   STPB:    The relative step used for computing finite difference
!            derivatives with respect to each BETA.
!   STPD:    The relative step used for computing finite difference
!            derivatives with respect to DELTA.
!   T:       The step for DELTA.
!   TAU:     The trust region diameter.
!   TAUFAC:  The factor used to compute the initial trust region 
!            diameter.
!   TEMP:    A temporary storage location.
!   TEMP1:   A temporary storage location.
!   TEMP2:   A temporary storage location.
!   TSNORM:  The norm of the scaled step.
!   TT:      The scaling values used for DELTA.
!   U:       The starting location in array WORK of array U.
!   UPPERU:  The upper bound for unfixed BETAs.
!   VCV:     The starting location in array WORK of array VCV.
!   WE:      The EPSILON weights.
!   WE1:     The square root of the EPSILON weights.
!   WD:      The DELTA weights.
!   WORK:    The REAL (KIND=R8) work space.
!   WSS:     The sum-of-squares of the weighted EPSILONS and DELTAS,
!            the sum-of-squares of the weighted DELTAS, and
!            the sum-of-squares of the weighted EPSILONS.
!   WRK:     A work array, equivalenced to WRK1 and WRK2
!   WRK1:    The starting location in array WORK of array WRK1.
!   WRK2:    The starting location in array WORK of array WRK2.
!   WRK3:    The starting location in array WORK of array WRK3.
!   WRK4:    The starting location in array WORK of array WRK4.
!   WRK5:    The starting location in array WORK of array WRK5.
!   WRK6:    The starting location in array WORK of array WRK6.
!   X:       The explanatory variable.
!   XPLUSD:  The values of X + DELTA.
!   Y:       The dependent variable.  Unused when the model is implicit.
!   ZERO:    The value 0.0E0_R8.


!***First executable statement  DODMN


!  Initialize necessary variables

      CALL DPACK(NP,NPU,LOWERU,LOWER,IFIXB)
      CALL DPACK(NP,NPU,UPPERU,UPPER,IFIXB)
      CALL DFLAGS(JOB,RESTRT,INITD,DOVCV,REDOJ,                        &
                  ANAJAC,CDJAC,CHKJAC,ISODR,IMPLCT)
      ACCESS = .TRUE.
      CALL DACCES(N,M,NP,NQ,LDWE,LD2WE,WORK,LWORK,IWORK,LIWORK,        &
                  ACCESS,ISODR,JPVT,OMEGA,U,QRAUX,SD,VCV,              &
                  WRK1,WRK2,WRK3,WRK4,WRK5,WRK6,NNZW,NPP,              &
                  JOB,PARTOL,SSTOL,MAXIT,TAUFAC,ETA,NETA,              &
                  LUNRPT,IPR1,IPR2,IPR2F,IPR3,WSS,RVAR,IDF,            &
                  TAU,ALPHA,NITER,NFEV,NJEV,INT2,OLMAVG,               &
                  RCOND,IRANK,ACTRS,PNORM,PRERS,RNORMS,ISTOP)
      RNORM = SQRT(WSS(1))

      DIDVCV = .FALSE.
      INTDBL = .FALSE.
      LSTEP = .TRUE.

!  Print initial summary if desired

      IF (IPR1.NE.0 .AND. LUNRPT.NE.0) THEN
         IFLAG = 1
         IF (IPR1.GE.3 .AND. LUNRPT.NE.LUDFLT) THEN
            NPR = 2
         ELSE
            NPR = 1
         END IF
         IF (IPR1.GE.6) THEN
            IPR = 2 
         ELSE
            IPR = 2 - MOD(IPR1,2)
         END IF
         LUNR = LUNRPT
         DO 10 I=1,NPR
            CALL DODPCR(IPR,LUNR,HEAD,PRTPEN,FSTITR,DIDVCV,IFLAG,      &
                         N,M,NP,NQ,NPP,NNZW,                           &
                         MSGB,MSGD, BETA,Y,LDY,X,LDX,DELTA,            &
                         WE,LDWE,LD2WE,WD,LDWD,LD2WD,                  &
                         IFIXB,IFIXX,LDIFX,LOWER,UPPER,                &
                         SSF,TT,LDTT,STPB,STPD,LDSTPD,                 &
                         JOB,NETA,TAUFAC,SSTOL,PARTOL,MAXIT,           &
                         WSS,RVAR,IDF,WORK(SD),                        &
                         NITER,NFEV,NJEV,ACTRED,PRERED,                &
                         TAU,PNORM,ALPHA,F,RCOND,IRANK,INFO,ISTOP)
            IF (IPR1.GE.5) THEN
               IPR = 2
            ELSE
               IPR = 1
            END IF
            LUNR = LUDFLT
   10    CONTINUE

      END IF

!  Stop if initial estimates are exact solution

      IF (RNORM.EQ.ZERO) THEN
         INFO = 1
         OLMAVG = ZERO
         ISTOP = 0
         GO TO 150
      END IF

!  Stop if number of iterations already equals maximum permitted

      IF (RESTRT .AND. (NITER.GE.MAXIT)) THEN
         ISTOP = 0
         GO TO 150
      ELSE IF (NITER.GE.MAXIT) THEN
         INFO = 4
         ISTOP = 0
         GO TO 150
      END IF

!  Main loop

  100 CONTINUE
 
      NITER = NITER + 1
      RNORMS = RNORM
      LOOPED = 0

!  Evaluate jacobian using best estimate of function (FS)

      IF ((NITER.EQ.1) .AND. (ANAJAC.AND.CHKJAC)) THEN
         ISTOP = 0
      ELSE
         CALL DEVJAC(FCN,ANAJAC,CDJAC,N,M,NP,NQ,BETAC,BETA,STPB,       &
                     IFIXB,IFIXX,LDIFX,                                &
                     X,LDX,DELTA,XPLUSD,STPD,LDSTPD,                   &
                     SSF,TT,LDTT,NETA,FS,                              &
                     T,WORK(WRK1),WORK(WRK2),WORK(WRK3),WORK(WRK6),    &
                     FJACB,ISODR,FJACD,WE1,LDWE,LD2WE,                 &
                     NJEV,NFEV,ISTOP,INFO,LOWER,UPPER)
      END IF
      IF (ISTOP.NE.0) THEN
         INFO = 51000
         GO TO 200
      ELSE IF (INFO.EQ.50300) THEN
         GO TO 200
      END IF

!  Sub loop for
!     internal doubling or
!     computing new step when old failed

  110 CONTINUE

!  Compute steps S and T

      IF (LOOPED.GT.100) THEN
         INFO = 60000
         GO TO 200
      ELSE
         LOOPED = LOOPED + 1
         CALL DODLM(N,M,NP,NQ,NPP,F,FJACB,FJACD,                       &
                    WD,LDWD,LD2WD,SS,TT,LDTT,DELTA,                    &
                    ALPHA,TAU,ETA,ISODR,WORK(WRK6),WORK(OMEGA),        &
                    WORK(U),WORK(QRAUX),IWORK(JPVT),                   &
                    S,T,NLMS,RCOND,IRANK,                              &
                    WORK(WRK1),WORK(WRK2),WORK(WRK3),WORK(WRK4),       &
                    WORK(WRK5),WRK,LWRK,ISTOPC)
      END IF
      IF (ISTOPC.NE.0) THEN
         INFO = ISTOPC
         GO TO 200
      END IF
      OLMAVG = OLMAVG+NLMS

!  Compute BETAN = BETAC + S
!          DELTAN = DELTA + T

      CALL DXPY(NPP,1,BETAC,NPP,S,NPP,BETAN,NPP)
      IF (ISODR) CALL DXPY(N,M,DELTA,N,T,N,DELTAN,N)

!  Project the step wrt the bounds
      DO I = 1, NPU
         IF (LOWERU(I).EQ.UPPERU(I)) THEN
            BETAN(I) = UPPERU(I)
            S(I) = UPPERU(I)-BETAC(I)
            BOUND(I) = 3
         ELSE IF (BETAN(I).LE.LOWERU(I)) THEN
            BETAN(I) = LOWERU(I)
            S(I) = LOWERU(I)-BETAC(I)
            BOUND(I) = 2
         ELSE IF (BETAN(I).GE.UPPERU(I)) THEN
            BETAN(I) = UPPERU(I)
            S(I) = UPPERU(I)-BETAC(I)
            BOUND(I) = 1
         ELSE
            BOUND(I) = 0
         END IF
      END DO

!  Compute norm of scaled steps S and T (TSNORM)

      CALL DWGHT(NPP,1,RESHAPE(SS,(/NPP,1,1/)),NPP,1,                  &
                 RESHAPE(S,(/NPP,1/)),TEMPRET(1:NPP,1:1))
      WRK(1:NPP) = TEMPRET(1:NPP,1)
      IF (ISODR) THEN
         CALL DWGHT(N,M,RESHAPE(TT,(/LDTT,1,M/)),LDTT,1,               &
                    T,TEMPRET(1:N,1:M))
         WRK(NPP+1:NPP+1+N*M-1) = RESHAPE(TEMPRET(1:N,1:M),(/N*M/))
         TSNORM = DNRM2(NPP+N*M,WRK,1)
      ELSE 
         TSNORM = DNRM2(NPP,WRK,1)
      END IF

!  Compute scaled predicted reduction

      IWRK = 0
      DO 130 L=1,NQ
         DO 120 I=1,N
           IWRK = IWRK + 1
           WRK(IWRK) = DDOT(NPP,FJACB(I,1,L),N,S,1)
           IF (ISODR) WRK(IWRK) = WRK(IWRK) +                          &
                                  DDOT(M,FJACD(I,1,L),N,T(I,1),N)
  120    CONTINUE
  130 CONTINUE
      IF (ISODR) THEN
         CALL DWGHT(N,M,WD,LDWD,LD2WD,T,TEMPRET(1:N,1:M))
         WRK(N*NQ+1:N*NQ+1+N*M-1) = RESHAPE(TEMPRET(1:N,1:M),(/N*M/))
         TEMP1 = DDOT(N*NQ,WRK,1,WRK,1) + DDOT(N*M,T,1,WRK(N*NQ+1),1)
         TEMP1 = SQRT(TEMP1)/RNORM
      ELSE
         TEMP1 = DNRM2(N*NQ,WRK,1)/RNORM
      END IF
      TEMP2 = SQRT(ALPHA)*TSNORM/RNORM
      PRERED = TEMP1**2+TEMP2**2/P5

      DIRDER = -(TEMP1**2+TEMP2**2)

!  Evaluate predicted values at new point

      CALL DUNPAC(NP,BETAN,BETA,IFIXB)
      CALL DXPY(N,M,X,LDX,DELTAN,N,XPLUSD,N)
      ISTOP = 0
      CALL FCN(N,M,NP,NQ,N,M,NP,BETA,XPLUSD,IFIXB,IFIXX,LDIFX,         &
               002,FN,WORK(WRK6),WORK(WRK1),ISTOP)
      IF (ISTOP.EQ.0) THEN
         NFEV = NFEV + 1
      END IF

      IF (ISTOP.LT.0) THEN

!  Set INFO to indicate user has stopped the computations in FCN

         INFO = 51000
         GO TO 200
      ELSE IF (ISTOP.GT.0) THEN

!  Set norm to indicate step should be rejected

         RNORMN = RNORM/(P1*P75)
      ELSE

!  Compute norm of new weighted EPSILONS and weighted DELTAS (RNORMN)

         IF (IMPLCT) THEN
            CALL DCOPY(N*NQ,FN,1,WRK,1)
         ELSE
            CALL DXMY(N,NQ,FN,N,Y,LDY,WRK,N)
         END IF
         CALL DWGHT(N,NQ,WE1,LDWE,LD2WE,RESHAPE(WRK,(/N,NQ/)),         &
            TEMPRET(1:N,1:NQ))
         WRK(1:N*NQ) = RESHAPE(TEMPRET(1:N,1:NQ),(/N*NQ/))
         IF (ISODR) THEN
            CALL DWGHT(N,M,WD,LDWD,LD2WD,DELTAN,TEMPRET(1:N,1:M))
            WRK(N*NQ+1:N*NQ+1+N*M-1) = RESHAPE(TEMPRET(1:N,1:M),(/N*M/))
            RNORMN = SQRT(DDOT(N*NQ,WRK,1,WRK,1) +                     &
                          DDOT(N*M,DELTAN,1,WRK(N*NQ+1),1))
         ELSE
            RNORMN = DNRM2(N*NQ,WRK,1)
         END IF
      END IF

!  Compute scaled actual reduction

      IF (P1*RNORMN.LT.RNORM) THEN
         ACTRED = ONE - (RNORMN/RNORM)**2
      ELSE
         ACTRED = -ONE
      END IF

!  Compute ratio of actual reduction to predicted reduction

      IF(PRERED .EQ. ZERO) THEN
         RATIO = ZERO
      ELSE
         RATIO = ACTRED/PRERED
      END IF

!  Check on lack of reduction in internal doubling case

      IF (INTDBL .AND. (RATIO.LT.P0001 .OR. RNORMN.GT.RNORMS)) THEN
         ISTOP = 0
         TAU = TAU*P5
         ALPHA = ALPHA/P5
         CALL DCOPY(NPP,BETAS,1,BETAN,1)
         CALL DCOPY(N*M,DELTAS,1,DELTAN,1)
         CALL DCOPY(N*NQ,FS,1,FN,1)
         ACTRED = ACTRS
         PRERED = PRERS
         RNORMN = RNORMS
         RATIO = P5
      END IF

!  Update step bound

      INTDBL = .FALSE.
      IF (RATIO.LT.P25) THEN
         IF (ACTRED.GE.ZERO) THEN
            TEMP = P5
         ELSE
            TEMP = P5*DIRDER/(DIRDER+P5*ACTRED)
         END IF
         IF (P1*RNORMN.GE.RNORM .OR. TEMP.LT.P1) THEN
            TEMP = P1
         END IF
         TAU = TEMP*MIN(TAU,TSNORM/P1)
         ALPHA = ALPHA/TEMP

      ELSE IF (ALPHA.EQ.ZERO) THEN
         TAU = TSNORM/P5

      ELSE IF (RATIO.GE.P75 .AND. NLMS.LE.11) THEN

!  Step qualifies for internal doubling
!     - Update TAU and ALPHA
!     - Save information for current point

         INTDBL = .TRUE.

         TAU = TSNORM/P5
         ALPHA = ALPHA*P5

         CALL DCOPY(NPP,BETAN,1,BETAS,1)
         CALL DCOPY(N*M,DELTAN,1,DELTAS,1)
         CALL DCOPY(N*NQ,FN,1,FS,1)
         ACTRS = ACTRED
         PRERS = PRERED
         RNORMS = RNORMN
      END IF

!  If internal doubling, skip convergence checks

      IF (INTDBL .AND. TAU.GT.ZERO) THEN
         INT2 = INT2+1
         GO TO 110
      END IF

!  Check acceptance

      IF (RATIO.GE.P0001) THEN
         CALL DCOPY(N*NQ,FN,1,FS,1)
         IF (IMPLCT) THEN
            CALL DCOPY(N*NQ,FS,1,F,1)
         ELSE
            CALL DXMY(N,NQ,FS,N,Y,LDY,F,N)
         END IF
         CALL DWGHT(N,NQ,WE1,LDWE,LD2WE,F,TEMPRET(1:N,1:NQ))
         F(1:N,1:NQ) = TEMPRET(1:N,1:NQ)
         CALL DCOPY(NPP,BETAN,1,BETAC,1)
         CALL DCOPY(N*M,DELTAN,1,DELTA,1)
         RNORM = RNORMN
         CALL DWGHT(NPP,1,RESHAPE(SS,(/NPP,1,1/)),NPP,1,               &
                    RESHAPE(BETAC,(/NPP,1/)),TEMPRET(1:NPP,1:1))
         WRK(1:NPP) = TEMPRET(1:NPP,1)
         IF (ISODR) THEN
            CALL DWGHT(N,M,RESHAPE(TT,(/LDTT,1,M/)),LDTT,1,            &
                       DELTA,TEMPRET(1:N,1:M))
            WRK(NPP+1:NPP+1+N*M-1) = RESHAPE(TEMPRET(1:N,1:M),(/N*M/))
            PNORM = DNRM2(NPP+N*M,WRK,1)
         ELSE
            PNORM = DNRM2(NPP,WRK,1)
         END IF
         LSTEP = .TRUE.
      ELSE
         LSTEP = .FALSE.
      END IF

!  TEST CONVERGENCE

      INFO = 0
      CNVSS = RNORM.EQ.ZERO .OR. (ABS(ACTRED).LE.SSTOL .AND.           &
              PRERED.LE.SSTOL .AND. P5*RATIO.LE.ONE)
      CNVPAR = (TAU.LE.PARTOL*PNORM) .AND. (.NOT.IMPLCT)
      IF (CNVSS)                            INFO = 1
      IF (CNVPAR)                           INFO = 2
      IF (CNVSS .AND. CNVPAR)               INFO = 3

!  Print iteration report

      IF (INFO.NE.0 .OR. LSTEP) THEN
         IF (IPR2.NE.0 .AND. IPR2F.NE.0 .AND. LUNRPT.NE.0) THEN
            IF (IPR2F.EQ.1 .OR. MOD(NITER,IPR2F).EQ.1) THEN
               IFLAG = 2
               CALL DUNPAC(NP,BETAC,BETA,IFIXB)
               WSS(1) = RNORM*RNORM
               IF (IPR2.GE.3 .AND. LUNRPT.NE.LUDFLT) THEN
                  NPR = 2
               ELSE
                  NPR = 1
               END IF
               IF (IPR2.GE.6) THEN
                  IPR = 2 
               ELSE
                  IPR = 2 - MOD(IPR2,2)
               END IF
               LUNR = LUNRPT
               DO 140 I=1,NPR
                  CALL DODPCR(IPR,LUNR,                                &
                              HEAD,PRTPEN,FSTITR,DIDVCV,IFLAG,         &
                              N,M,NP,NQ,NPP,NNZW,                      &
                              MSGB,MSGD, BETA,Y,LDY,X,LDX,DELTA,       &
                              WE,LDWE,LD2WE,WD,LDWD,LD2WD,             &
                              IFIXB,IFIXX,LDIFX,LOWER,UPPER,           &
                              SSF,TT,LDTT,STPB,STPD,LDSTPD,            &
                              JOB,NETA,TAUFAC,SSTOL,PARTOL,MAXIT,      &
                              WSS,RVAR,IDF,WORK(SD),                   &
                              NITER,NFEV,NJEV,ACTRED,PRERED,           &
                              TAU,PNORM,ALPHA,F,RCOND,IRANK,INFO,ISTOP)
                  IF (IPR2.GE.5) THEN
                     IPR = 2
                  ELSE
                     IPR = 1
                  END IF
                  LUNR = LUDFLT
  140          CONTINUE
               FSTITR = .FALSE.
               PRTPEN = .FALSE.
            END IF
         END IF
      END IF

!  Check if finished

      IF (INFO.EQ.0) THEN
         IF (LSTEP) THEN

!  Begin next interation unless a stopping criteria has been met

            IF (NITER.GE.MAXIT) THEN
               INFO = 4
            ELSE
               GO TO 100
            END IF
         ELSE

!  Step failed - recompute unless a stopping criteria has been met

            GO TO 110
         END IF
      END IF

  150 CONTINUE

      IF (ISTOP.GT.0) INFO = INFO + 100

!  Store unweighted EPSILONS and X+DELTA to return to user

      IF (IMPLCT) THEN
         CALL DCOPY(N*NQ,FS,1,F,1)
      ELSE
         CALL DXMY(N,NQ,FS,N,Y,LDY,F,N)
      END IF
      CALL DUNPAC(NP,BETAC,BETA,IFIXB)
      CALL DXPY(N,M,X,LDX,DELTA,N,XPLUSD,N)

!  Compute covariance matrix of estimated parameters
!  in upper NP by NP portion of WORK(VCV) if requested

      IF (DOVCV .AND. ISTOP.EQ.0) THEN
            
!  Re-evaluate Jacobian at final solution, if requested
!  Otherwise, Jacobian from beginning of last iteration will be used
!  to compute covariance matrix

         IF (REDOJ) THEN
            CALL DEVJAC(FCN,ANAJAC,CDJAC,N,M,NP,NQ,BETAC,BETA,STPB,    &
                         IFIXB,IFIXX,LDIFX,                            &
                         X,LDX,DELTA,XPLUSD,STPD,LDSTPD,               &
                         SSF,TT,LDTT,NETA,FS,                          &
                         T,WORK(WRK1),WORK(WRK2),WORK(WRK3),WORK(WRK6),&
                         FJACB,ISODR,FJACD,WE1,LDWE,LD2WE,             &
                         NJEV,NFEV,ISTOP,INFO,LOWER,UPPER)


            IF (ISTOP.NE.0) THEN
               INFO = 51000
               GO TO 200
            ELSE IF (INFO.EQ.50300) THEN
               GO TO 200
            END IF
         END IF

         IF (IMPLCT) THEN
            CALL DWGHT(N,M,WD,LDWD,LD2WD,DELTA,TEMPRET(1:N,1:M))
            WRK(N*NQ+1:N*NQ+1+N*M-1) = RESHAPE(TEMPRET(1:N,1:M),(/N*M/))
            RSS = DDOT(N*M,DELTA,1,WRK(N*NQ+1),1)
         ELSE
            RSS = RNORM*RNORM
         END IF
         IF (REDOJ .OR. NITER.GE.1) THEN
            CALL DODVCV(N,M,NP,NQ,NPP,F,FJACB,FJACD,                   &
                        WD,LDWD,LD2WD,SSF,SS,TT,LDTT,DELTA,            &
                        ETA,ISODR,WORK(VCV),WORK(SD),                  &
                        WORK(WRK6),WORK(OMEGA),                        &
                        WORK(U),WORK(QRAUX),IWORK(JPVT),               &
                        S,T,IRANK,RCOND,RSS,IDF,RVAR,IFIXB,            &
                        WORK(WRK1),WORK(WRK2),WORK(WRK3),WORK(WRK4),   &
                        WORK(WRK5),WRK,LWRK,ISTOPC)
            IF (ISTOPC.NE.0) THEN
               INFO = ISTOPC
               GO TO 200
            END IF
            DIDVCV = .TRUE.
         END IF

      END IF

!  Set JPVT to indicate dropped, fixed and estimated parameters

  200 DO 210 I=0,NP-1
         WORK(WRK3+I) = IWORK(JPVT+I)
         IWORK(JPVT+I) = -2
  210 CONTINUE
      IF (REDOJ .OR. NITER.GE.1) THEN
         DO 220 I=0,NPP-1
            J = WORK(WRK3+I) - 1
            IF (I.LE.NPP-IRANK-1) THEN
               IWORK(JPVT+J) = 1
            ELSE 
               IWORK(JPVT+J) = -1
            END IF
  220    CONTINUE
         IF (NPP.LT.NP) THEN
            J = NPP-1
            DO 230 I=NP-1,0,-1
               IF (IFIXB(I+1).EQ.0) THEN
                  IWORK(JPVT+I) = 0
               ELSE
                  IWORK(JPVT+I) = IWORK(JPVT+J)
                  J = J - 1
               END IF
  230       CONTINUE
         END IF
      END IF

!  Store various scalars in work arrays for return to user

      IF (NITER.GE.1) THEN
         OLMAVG = OLMAVG/NITER
      ELSE
         OLMAVG = ZERO
      END IF

!  Compute weighted sums of squares for return to user

      CALL DWGHT(N,NQ,WE1,LDWE,LD2WE,F,TEMPRET(1:N,1:NQ))
      WRK(1:N*NQ) = RESHAPE(TEMPRET(1:N,1:NQ),(/N*NQ/))
      WSS(3) = DDOT(N*NQ,WRK,1,WRK,1)
      IF (ISODR) THEN
         CALL DWGHT(N,M,WD,LDWD,LD2WD,DELTA,TEMPRET(1:N,1:M))
         WRK(N*NQ+1:N*NQ+1+N*M-1) = RESHAPE(TEMPRET(1:N,1:M),(/N*M/))
         WSS(2) = DDOT(N*M,DELTA,1,WRK(N*NQ+1),1)
      ELSE
         WSS(2) = ZERO
      END IF
      WSS(1) = WSS(2) + WSS(3)

      ACCESS = .FALSE.
      CALL DACCES(N,M,NP,NQ,LDWE,LD2WE,WORK,LWORK,IWORK,LIWORK,        &
                  ACCESS,ISODR,JPVT,OMEGA,U,QRAUX,SD,VCV,              &
                  WRK1,WRK2,WRK3,WRK4,WRK5,WRK6,NNZW,NPP,              &
                  JOB,PARTOL,SSTOL,MAXIT,TAUFAC,ETA,NETA,              &
                  LUNRPT,IPR1,IPR2,IPR2F,IPR3,WSS,RVAR,IDF,            &
                  TAU,ALPHA,NITER,NFEV,NJEV,INT2,OLMAVG,               &
                  RCOND,IRANK,ACTRS,PNORM,PRERS,RNORMS,ISTOP)

!  Encode existance of questionable results into info

      IF (INFO.LE.9 .OR. INFO.GE.60000) THEN
         IF (MSGB(1).EQ.1 .OR. MSGD(1).EQ.1) THEN
            INFO = INFO + 1000
         END IF
         IF (ISTOP.NE.0) THEN
            INFO = INFO + 100
         END IF
         IF (IRANK.GE.1) THEN
            IF (NPP.GT.IRANK) THEN
               INFO = INFO + 10
            ELSE
               INFO = INFO + 20
            END IF
         END IF
      END IF

!  Print final summary

      IF (IPR3.NE.0 .AND. LUNRPT.NE.0) THEN
         IFLAG = 3

         IF (IPR3.GE.3 .AND. LUNRPT.NE.LUDFLT) THEN
            NPR = 2
         ELSE
            NPR = 1
         END IF
         IF (IPR3.GE.6) THEN
            IPR = 2 
         ELSE
            IPR = 2 - MOD(IPR3,2)
         END IF
         LUNR = LUNRPT
         DO 240 I=1,NPR
            CALL DODPCR(IPR,LUNR,HEAD,PRTPEN,FSTITR,DIDVCV,IFLAG,      &
                        N,M,NP,NQ,NPP,NNZW,                            &
                        MSGB,MSGD, BETA,Y,LDY,X,LDX,DELTA,             &
                        WE,LDWE,LD2WE,WD,LDWD,LD2WD,                   &
                        IWORK(JPVT),IFIXX,LDIFX,LOWER,UPPER,           &
                        SSF,TT,LDTT,STPB,STPD,LDSTPD,                  &
                        JOB,NETA,TAUFAC,SSTOL,PARTOL,MAXIT,            &
                        WSS,RVAR,IDF,WORK(SD),                         &
                        NITER,NFEV,NJEV,ACTRED,PRERED,                 &
                        TAU,PNORM,ALPHA,F,RCOND,IRANK,INFO,ISTOP)
            IF (IPR3.GE.5) THEN
               IPR = 2
            ELSE
               IPR = 1
            END IF
            LUNR = LUDFLT
  240    CONTINUE
      END IF

      RETURN

      END SUBROUTINE
!DODPC1
      SUBROUTINE DODPC1(IPRTMP,LUNRPT,ANAJAC,CDJAC,CHKJAC,INITD,RESTRT,&
                        ISODR,IMPLCT,DOVCV,REDOJ,MSGB1,MSGB,MSGD1,     &
                        MSGD,N,M,NP,NQ,NPP,NNZW,X,LDX,IFIXX,LDIFX,     &
                        DELTA,WD,LDWD,LD2WD,TT,LDTT,STPD,LDSTPD,       &
                        Y,LDY,WE,LDWE,LD2WE,PNLTY,                     &
                        BETA,IFIXB,SSF,STPB,LOWER,UPPER,               &
                        JOB,NETA,TAUFAC,SSTOL,PARTOL,MAXIT,            &
                        WSS,WSSDEL,WSSEPS)
!***Begin Prologue  DODPC1
!***Refer to  ODR
!***Routines Called  DHSTEP
!***Date Written   860529   (YYMMDD)
!***Revision Date  920619   (YYMMDD)
!***Purpose  Generate initial summary report
!***End Prologue  DODPC1

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      REAL (KIND=R8) PARTOL,PNLTY,SSTOL,TAUFAC,WSS,WSSDEL,WSSEPS
      INTEGER IPRTMP,JOB,LDIFX,LDSTPD,LDTT,LDWD,LDWE,LDX,LDY,LD2WD,    &
              LD2WE,LUNRPT,M,MAXIT,MSGB1,MSGD1,N,NETA,NNZW,NP,NPP,NQ
      LOGICAL ANAJAC,CDJAC,CHKJAC,DOVCV,IMPLCT,INITD,ISODR,REDOJ,RESTRT

!...Array arguments
      REAL (KIND=R8) BETA(NP),DELTA(N,M),LOWER(NP),SSF(NP),STPB(NP),   &
                     STPD(LDSTPD,M),TT(LDTT,M),UPPER(NP),              &
                     WD(LDWD,LD2WD,M),WE(LDWE,LD2WE,NQ),               &
                     X(LDX,M),Y(LDY,NQ)
      INTEGER IFIXB(NP),IFIXX(LDIFX,M),MSGB(NQ,NP),MSGD(NQ,M)

!...Local scalars
      REAL (KIND=R8) TEMP1,TEMP2,TEMP3,ZERO
      INTEGER I,ITEMP,J,JOB1,JOB2,JOB3,JOB4,JOB5,L

!...Local arrays
      CHARACTER TEMPC0*2,TEMPC1*5,TEMPC2*13

!...External functions
      REAL (KIND=R8) DHSTEP
      EXTERNAL DHSTEP
!
!     INCLUDE FILES FROM DATAPLOT
!
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
!...Data statements
      DATA ZERO /0.0E0_R8/

!...Variable Definitions (alphabetically)
!   ANAJAC:  The variable designating whether the Jacobians are computed
!            by finite differences (ANAJAC=FALSE) or not (ANAJAC=TRUE).
!   BETA:    The function parameters.
!   CDJAC:   The variable designating whether the Jacobians are computed
!            by central differences (CDJAC=TRUE) or forward differences 
!            (CDJAC=FALSE).
!   CHKJAC:  The variable designating whether the user supplied 
!            Jacobians are to be checked (CHKJAC=TRUE) or not 
!            (CHKJAC=FALSE).
!   DELTA:   The estimated errors in the explanatory variables.
!   DOVCV:   The variable designating whether the covariance matrix is 
!            to be computed (DOVCV=TRUE) or not (DOVCV=FALSE).
!   I:       An indexing variable.
!   IFIXB:   The values designating whether the elements of BETA are 
!            fixed at their input values or not.
!   IFIXX:   The values designating whether the elements of X are 
!            fixed at their input values or not.
!   IMPLCT:  The variable designating whether the solution is by
!            implicit ODR (IMPLCT=TRUE) or explicit ODR (IMPLCT=FALSE). 
!   INITD:   The variable designating whether DELTA is initialized to 
!            zero (INITD=TRUE) or to the values in the first N by M
!            elements of array WORK (INITD=FALSE).
!   IPRTMP:  The value indicating the report to be printed.
!   ISODR:   The variable designating whether the solution is by ODR 
!            (ISODR=TRUE) or by OLS (ISODR=FALSE).
!   ITEMP:   A temporary integer value.
!   J:       An indexing variable.
!   JOB:     The variable controling problem initialization and  
!            computational method.
!   JOB1:    The 1st digit (from the left) of variable JOB.
!   JOB2:    The 2nd digit (from the left) of variable JOB.
!   JOB3:    The 3rd digit (from the left) of variable JOB.
!   JOB4:    The 4th digit (from the left) of variable JOB.
!   JOB5:    The 5th digit (from the left) of variable JOB.
!   L:       An indexing variable.
!   LDIFX:   The leading dimension of array IFIXX.
!   LDTT:    The leading dimension of array TT.
!   LDWD:    The leading dimension of array WD.
!   LDWE:    The leading dimension of array WE.
!   LDX:     The leading dimension of array X.
!   LDY:     The leading dimension of array Y.
!   LD2WD:   The second dimension of array WD.
!   LD2WE:   The second dimension of array WE.
!   LUNRPT:  The logical unit number for the computation reports.
!   M:       The number of columns of data in the explanatory variable.
!   MAXIT:   The maximum number of iterations allowed. 
!   MSGB:    The error checking results for the Jacobian wrt beta.
!   MSGB1:   The error checking results for the Jacobian wrt BETA.
!   MSGD:    The error checking results for the Jacobian wrt DELTA.
!   MSGD1:   The error checking results for the Jacobian wrt DELTA.
!   N:       The number of observations.
!   NETA:    The number of accurate digits in the function results.
!            A negative value indicates that NETA was estimated by
!            ODRPACK95. A positive value indictes the value was supplied
!            by the user.
!   NNZW:    The number of nonzero observational error weights.
!   NP:      The number of function parameters.
!   NPP:     The number of function parameters being estimated.
!   NQ:      The number of responses per observation.
!   PARTOL:  The parameter convergence stopping tolerance.
!   PNLTY:   The penalty parameter for an implicit model.
!   REDOJ:   The variable designating whether the Jacobian matrix is to
!            be recomputed for the computation of the covariance matrix 
!            (REDOJ=TRUE) or not (REDOJ=FALSE).
!   RESTRT:  The variable designating whether the call is a restart 
!            (RESTRT=TRUE) or not (RESTRT=FALSE).
!   SSF:     The scaling values for BETA.
!   SSTOL:   The sum-of-squares convergence stopping tolerance.
!   STPB:    The relative step used for computing finite difference
!            derivatives with respect to BETA.
!   STPD:    The relative step used for computing finite difference
!            derivatives with respect to DELTA.
!   TAUFAC:  The factor used to compute the initial trust region 
!            diameter.
!   TEMPC0:  A temporary CHARACTER*2 value.
!   TEMPC1:  A temporary CHARACTER*5 value.
!   TEMPC2:  A temporary CHARACTER*13 value.
!   TEMP1:   A temporary REAL (KIND=R8) value.
!   TEMP2:   A temporary REAL (KIND=R8) value.
!   TEMP3:   A temporary REAL (KIND=R8) value.
!   TT:      The scaling values for DELTA.
!   WD:      The DELTA weights.
!   WE:      The EPSILON weights.
!   WSS:     The sum-of-squares of the weighted EPSILONS and DELTAS.
!   WSSDEL:  The sum-of-squares of the weighted DELTAS.
!   WSSEPS:  The sum-of-squares of the weighted EPSILONS.
!   X:       The explanatory variable.
!   Y:       The response variable.  Unused when the model is implicit.
!   ZERO:    The value 0.0E0_R8.


!***First executable statement  DODPC1

!
!     For Dataplot, send output through DPWRST
!

!
!     Dataplot debugging code
!
      IF(ISUBG4.EQ.'DPC1')THEN
        WRITE(ICOUT,52)LUNRPT
   52   FORMAT('LUNRPT = ',I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!  Print problem size specification

!     WRITE (LUNRPT,1000) N,NNZW,NQ,M,NP,NPP
!1000 FORMAT
!    &  (/' --- Problem Size:'/
!    &    '            N = ',I5,
!    &    '          (number with nonzero weight = ',I5,')'/
!    &    '           NQ = ',I5/
!    &    '            M = ',I5/
!    &    '           NP = ',I5,
!    &    '          (number unfixed = ',I5,')')
      WRITE (ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE (ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE (ICOUT,1000)
 1000 FORMAT(' --- PROBLEM SIZE:')
      CALL DPWRST('XXX','BUG ')
      WRITE (ICOUT,1002)
 1002 FORMAT(' -------------')
      CALL DPWRST('XXX','BUG ')
      WRITE (ICOUT,1003) N
 1003 FORMAT('      NUMBER OF OBSERVATIONS                   = ',I5)
      CALL DPWRST('XXX','BUG ')
      WRITE (ICOUT,1004) NNZW
 1004 FORMAT('      NUMBER WITH NONZERO WEIGHT               = ',I5)
      CALL DPWRST('XXX','BUG ')
      WRITE (ICOUT,1005) NQ
 1005 FORMAT('      NUMBER OF RESPONSES PER OBSERVATION (NQ) = ',I5)
      CALL DPWRST('XXX','BUG ')
      WRITE (ICOUT,1006) M
 1006 FORMAT('      NUMBER OF INDEPENDENT VARIABLES (M)      = ',I5)
      CALL DPWRST('XXX','BUG ')
      WRITE (ICOUT,1008) NP
 1008 FORMAT('      NUMBER OF FUNCTION PARAMETERS (NP)       = ',I5)
      CALL DPWRST('XXX','BUG ')
      WRITE (ICOUT,1010) NPP
 1010 FORMAT('      NUMBER OF UNFIXED FUNCTION PARAMETERS    = ',I5)
      CALL DPWRST('XXX','BUG ')

!  Print control values

      JOB1 = JOB/10000
      JOB2 = MOD(JOB,10000)/1000
      JOB3 = MOD(JOB,1000)/100
      JOB4 = MOD(JOB,100)/10
      JOB5 = MOD(JOB,10)
!     WRITE (LUNRPT,1100) JOB
!1100 FORMAT
!    &   (/' --- Control Values:'/
!    &     '          JOB = ',I5.5/
!    &     '              = ABCDE, where')
      WRITE (ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE (ICOUT,1100)
 1100 FORMAT(' --- CONTROL VALUES:')
      CALL DPWRST('XXX','BUG ')
      WRITE (ICOUT,1101) JOB
 1101 FORMAT('          JOB = ',I5.5)
      CALL DPWRST('XXX','BUG ')
      WRITE (ICOUT,1102)
 1102 FORMAT('              = ABCDE, WHERE')
      CALL DPWRST('XXX','BUG ')
      IF (RESTRT) THEN
!        WRITE (LUNRPT,1110) JOB1
!1110    FORMAT
!    &   ('                       A=',I1,' ==> fit is a restart.')
         WRITE (ICOUT,1110) JOB1
 1110    FORMAT('                       A=',I1,' ==> FIT IS A RESTART.')
         CALL DPWRST('XXX','BUG ')
      ELSE
!        WRITE (LUNRPT,1111) JOB1
!1111    FORMAT
!    &   ('                       A=',I1,' ==> fit is not a restart.')
         WRITE (ICOUT,1111) JOB1
 1111    FORMAT('                       A=',I1,' ==> FIT IS NOT A RESTART.')
         CALL DPWRST('XXX','BUG ')
      END IF
      IF (ISODR) THEN
         IF (INITD) THEN
!           WRITE (LUNRPT,1120) JOB2
!1120       FORMAT
!    &   ('                       B=',I1,' ==> deltas are initialized',
!    &                                     ' to zero.')
            WRITE (ICOUT,1120) JOB2
 1120       FORMAT('                       B=',I1,' ==> DELTAS ARE ', &
                   'INITIALIZED TO ZERO.')
            CALL DPWRST('XXX','BUG ')
         ELSE
!           WRITE (LUNRPT,1121) JOB2
!1121       FORMAT
!    &   ('                       B=',I1,' ==> deltas are initialized',
!    &                                     ' by user.')
            WRITE (ICOUT,1121) JOB2
 1121       FORMAT('                       B=',I1,' ==> DELTAS ARE ', &
                   'INITIALIZED  BY USER.')
            CALL DPWRST('XXX','BUG ')
         END IF
      ELSE
!        WRITE (LUNRPT,1122) JOB2,JOB5
!1122    FORMAT
!    &   ('                       B=',I1,' ==> deltas are fixed at',
!    &                                     ' zero since E=',I1,'.')
         WRITE (ICOUT,1122) JOB2,JOB5
 1122    FORMAT('                       B=',I1,' ==> DELTAS ARE ',  &
             'FIXED AT ZERO SINCE E=',I1,'.')
         CALL DPWRST('XXX','BUG ')
      END IF
      IF (DOVCV) THEN
!        WRITE (LUNRPT,1130) JOB3
         WRITE (ICOUT,1130) JOB3
 1130    FORMAT('                       C=',I1,' ==> COVARIANCE ',   &
                'MATRIX WILL BE COMPUTED USING')
         CALL DPWRST('XXX','BUG ')
         IF (REDOJ) THEN
!           WRITE (LUNRPT,1131) 
            WRITE (ICOUT,1131) JOB3
 1131       FORMAT('                               DERIVATIVES ',   &
                   'RE-EVALUATED AT THE SOLUTION.')
            CALL DPWRST('XXX','BUG ')
         ELSE
!           WRITE (LUNRPT,1132)
            WRITE (ICOUT,1132) JOB3
 1132       FORMAT('                               DERIVATIVES FROM ',  &
                   'THE LAST ITERATION.')
            CALL DPWRST('XXX','BUG ')
         END IF
      ELSE
!        WRITE (LUNRPT,1133) JOB3
         WRITE (ICOUT,1133) JOB3
 1133    FORMAT('                       C=',I1,' ==> COVARIANCE ',   &
                'MATRIX WILL NOT BE COMPUTED.')
         CALL DPWRST('XXX','BUG ')
      END IF
      IF (ANAJAC) THEN
!        WRITE (LUNRPT,1140) JOB4
         WRITE (ICOUT,1140) JOB4
 1140    FORMAT('                       D=',I1,' ==> DERIVATIVES ARE', &
                ' SUPPLIED BY USER.')
         CALL DPWRST('XXX','BUG ')
         IF (CHKJAC) THEN
            IF (MSGB1.GE.1 .OR. MSGD1.GE.1) THEN
!              WRITE (LUNRPT,1141)
               WRITE (ICOUT,1141)
 1141          FORMAT('                               DERIVATIVES ',   &
                      'WERE CHECKED.')
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,11141)
11141          FORMAT('                               RESULTS APPEAR ', &
                      'QUESTIONABLE.')
               CALL DPWRST('XXX','BUG ')
            ELSE
!              WRITE (LUNRPT,1142)
               WRITE (ICOUT,1142)
 1142          FORMAT('                               DERIVATIVES ', &
                      'WERE CHECKED.')
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,11142)
11142          FORMAT('                               RESULTS APPEAR ', &
                      'CORRECT.')
               CALL DPWRST('XXX','BUG ')
            END IF
         ELSE
!           WRITE (LUNRPT,1143)
            WRITE (ICOUT,1143)
 1143       FORMAT('                               DERIVATIVES WERE ', &
                   'NOT CHECKED.')
            CALL DPWRST('XXX','BUG ')
         END IF
      ELSE IF (CDJAC) THEN
!        WRITE (LUNRPT,1144) JOB4
         WRITE (ICOUT,1144) JOB4
 1144    FORMAT('                       D=',I1,' ==> DERIVATIVES ARE', &
                ' ESTIMATED BY CENTRAL DIFFERENCES.')
         CALL DPWRST('XXX','BUG ')
      ELSE 
!        WRITE (LUNRPT,1145) JOB4
         WRITE (ICOUT,1145) JOB4
 1145    FORMAT('                       D=',I1,' ==> DERIVATIVES ARE', &
                ' ESTIMATED BY FORWARD DIFFERENCES.')
         CALL DPWRST('XXX','BUG ')
      END IF
      IF (ISODR) THEN
         IF (IMPLCT) THEN
!           WRITE (LUNRPT,1150) JOB5
            WRITE (ICOUT,1150) JOB5
 1150       FORMAT('                       E=',I1,' ==> METHOD IS ',  &
                   'IMPLICIT ODR.')
            CALL DPWRST('XXX','BUG ')
         ELSE
!           WRITE (LUNRPT,1151) JOB5
 1151       FORMAT('                       E=',I1,' ==> METHOD IS ',  &
                   'EXPLICIT ODR.')
         END IF
      ELSE
!        WRITE (LUNRPT,1152) JOB5
         WRITE (ICOUT,1152) JOB5
 1152    FORMAT('                       E=',I1,' ==> METHOD IS ',  &
                'EXPLICIT OLS.')
         CALL DPWRST('XXX','BUG ')
      END IF
      IF (NETA.LT.0) THEN
!        WRITE (LUNRPT,1200) -NETA
         WRITE (ICOUT,1200) -NETA
 1200    FORMAT('       NDIGIT = ',I5,'          (ESTIMATED BY ODRPACK)')
         CALL DPWRST('XXX','BUG ')
      ELSE
!        WRITE (LUNRPT,1210) NETA
         WRITE (ICOUT,1210) NETA
 1210    FORMAT('       NDIGIT = ',I5,'          (SUPPLIED BY USER)')
         CALL DPWRST('XXX','BUG ')
      END IF
!     WRITE (LUNRPT,1300) TAUFAC
      WRITE (ICOUT,1300) TAUFAC
 1300 FORMAT('       TAUFAC = ',1P,D12.2)
      CALL DPWRST('XXX','BUG ')


!  Print stopping criteria

!     WRITE (LUNRPT,1400) SSTOL,PARTOL,MAXIT
      WRITE (ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE (ICOUT,1400)
 1400 FORMAT(' --- STOPPING CRITERIA:')
      CALL DPWRST('XXX','BUG ')
      WRITE (ICOUT,1401) SSTOL
 1401 FORMAT('        SSTOL = ',1P,D12.2,   &
             '   (SUM OF SQUARES STOPPING TOLERANCE)')
      CALL DPWRST('XXX','BUG ')
      WRITE (ICOUT,1402) PARTOL
 1402 FORMAT('       PARTOL = ',1P,D12.2,   &
             '   (PARAMETER STOPPING TOLERANCE)')
      CALL DPWRST('XXX','BUG ')
      WRITE (ICOUT,1403) MAXIT
 1403 FORMAT('        MAXIT = ',I5,   &
             '          (MAXIMUM NUMBER OF ITERATIONS)')
      CALL DPWRST('XXX','BUG ')


!  Print initial sum of squares

      IF (IMPLCT) THEN
!        WRITE (LUNRPT,1500) WSSDEL
         WRITE (ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,1500) WSSDEL
 1500    FORMAT(' --- INITIAL SUM OF SQUARED WEIGHTED DELTAS =',   &
                17X,1P,D17.8)
         CALL DPWRST('XXX','BUG ')
         IF (ISODR) THEN
!           WRITE (LUNRPT,1510) WSS,WSSEPS,PNLTY
            WRITE (ICOUT,1510) WSS
 1510       FORMAT('         INITIAL PENALTY FUNCTION VALUE     =', &
                   1P,D17.8)
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,1511) WSSEPS
 1511       FORMAT('                 PENALTY TERM               =', &
                   1P,D17.8)
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,1512) PNLTY
 1512       FORMAT('                 PENALTY PARAMETER          =', &
                   1P,D10.1)
            CALL DPWRST('XXX','BUG ')
         END IF
      ELSE
!        WRITE (LUNRPT,1600) WSS
         WRITE (ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,1600) WSS
 1600    FORMAT(' --- INITIAL WEIGHTED SUM OF SQUARES        =',   &
                17X,1P,D17.8)
         CALL DPWRST('XXX','BUG ')
         IF (ISODR) THEN
!           WRITE (LUNRPT,1610) WSSDEL,WSSEPS
            WRITE (ICOUT,1610) WSSDEL
 1610       FORMAT('         SUM OF SQUARED WEIGHTED DELTAS     =', &
                   1P,D17.8)
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,1611) WSSEPS
 1611       FORMAT('         SUM OF SQUARED WEIGHTED EPSILONS   =', &
                   1P,D17.8)
            CALL DPWRST('XXX','BUG ')
         END IF
      END IF

 
      IF (IPRTMP.GE.2) THEN


!  Print function parameter data

!        WRITE (LUNRPT,4000)
         WRITE (ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,4000)
 4000    FORMAT(' --- FUNCTION PARAMETER SUMMARY:')
         CALL DPWRST('XXX','BUG ')
         IF (CHKJAC .AND. ((MSGB1.GE.1) .OR. (MSGD1.GE.1))) THEN
!           WRITE (LUNRPT,4110)
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,4110)
 4110       FORMAT('       INDEX         BETA(K)    FIXED           ', &
                   'SCALE     DERIVATIVE')
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
         ELSE IF (ANAJAC) THEN
!           WRITE (LUNRPT,4120)
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,4120)
 4120       FORMAT('       INDEX         BETA(K)    FIXED           ', &
                   'SCALE              ')
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
         ELSE 
!           WRITE (LUNRPT,4200)
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,4200)
 4200       FORMAT('       INDEX         BETA(K)    FIXED           ', &
                   'SCALE    DERIVATIVE')
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
         END IF 
         DO 130 J=1,NP
            IF (IFIXB(1).LT.0) THEN
               TEMPC1 = '   NO'
            ELSE
               IF (IFIXB(J).NE.0) THEN
                  TEMPC1 = '   NO'
               ELSE
                  TEMPC1 = '  YES'
               END IF
            END IF
            IF (ANAJAC) THEN
               IF (CHKJAC .AND. ((MSGB1.GE.1) .OR. (MSGD1.GE.1))) THEN
                  ITEMP = -1
                  DO 110 L=1,NQ
                     ITEMP = MAX(ITEMP,MSGB(L,J))
  110             CONTINUE
                  IF (ITEMP.LE.-1) THEN
                     TEMPC2 = '    UNCHECKED'
                  ELSE IF (ITEMP.EQ.0) THEN
                     TEMPC2 = '     VERIFIED'
                  ELSE IF (ITEMP.GE.1) THEN
                     TEMPC2 = ' QUESTIONABLE'
                  END IF
               ELSE
                  TEMPC2 = '             '
               END IF
            ELSE
               TEMPC2 = '             '
            END IF
            IF (SSF(1).LT.ZERO) THEN
               TEMP1 = ABS(SSF(1))
            ELSE
               TEMP1 = SSF(J)
            END IF
            IF (ANAJAC) THEN
!              WRITE (LUNRPT,4310) J,BETA(J),TEMPC1,TEMP1,LOWER(J),
!    &                             UPPER(J),TEMPC2
               WRITE (ICOUT,4310) J,BETA(J),TEMPC1,TEMP1,TEMPC2
 4310          FORMAT(7X,I5,1P,D16.8,4X,A5,D16.8,1X,A13)
               CALL DPWRST('XXX','BUG ')
            ELSE
               IF (CDJAC) THEN 
                  TEMP2 = DHSTEP(1,NETA,1,J,STPB,1)
               ELSE
                  TEMP2 = DHSTEP(0,NETA,1,J,STPB,1)
               END IF
!              WRITE (LUNRPT,4320) J,BETA(J),TEMPC1,TEMP1,
!    &                             LOWER(J),UPPER(J),TEMP2
               WRITE (ICOUT,4320) J,BETA(J),TEMPC1,TEMP1,TEMP2
 4320          FORMAT(7X,I5,1P,D16.8,4X,A5,D16.8,1X,D13.5)
               CALL DPWRST('XXX','BUG ')
            END IF
  130    CONTINUE

!  Print explanatory variable data 

         IF (ISODR) THEN
!           WRITE (LUNRPT,2010)
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,2010)
 2010       FORMAT(' --- EXPLANATORY VARIABLE AND DELTA WEIGHT SUMMARY:')
            CALL DPWRST('XXX','BUG ')
            IF (CHKJAC .AND. ((MSGB1.GE.1) .OR. (MSGD1.GE.1))) THEN
!              WRITE (LUNRPT,2110)
               WRITE (ICOUT,999)
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,2110)
 2110          FORMAT('       INDEX      X(I,J)  DELTA(I,J)    FIXED', &
                      '     SCALE    WEIGHT    DERIVATIVE')
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,2111)
 2111          FORMAT('                                             ', &
                      '                        ASSESSMENT')
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,2112)
 2112          FORMAT('       (I,J)                          (IFIXX)', &
                      '    (SCLD)      (WD)              ')
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,999)
               CALL DPWRST('XXX','BUG ')
            ELSE IF (ANAJAC) THEN
!              WRITE (LUNRPT,2120)
               WRITE (ICOUT,999)
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,2120)
 2120          FORMAT('       INDEX      X(I,J)  DELTA(I,J)    FIXED', &
                      '     SCALE    WEIGHT              ')
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,2121)
 2121          FORMAT('                                             ', &
                      '                                  ')
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,2122)
 2122          FORMAT('       (I,J)                          (IFIXX)', &
                      '    (SCLD)      (WD)              ')
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,999)
               CALL DPWRST('XXX','BUG ')
            ELSE
!              WRITE (LUNRPT,2130)
               WRITE (ICOUT,999)
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,2130)
 2130          FORMAT('       INDEX      X(I,J)  DELTA(I,J)    FIXED', &
                      '     SCALE    WEIGHT    DERIVATIVE')
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,2131)
 2131          FORMAT('                                             ', &
                      '                         STEP SIZE')
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,2132)
 2132          FORMAT('       (I,J)                          (IFIXX)', &
                      '    (SCLD)      (WD)        (STPD)')
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,999)
               CALL DPWRST('XXX','BUG ')
            END IF
         ELSE
!           WRITE (LUNRPT,2020)
!           WRITE (LUNRPT,2140)
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,2020)
 2020       FORMAT(' --- EXPLANATORY VARIABLE SUMMARY:')
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,2140)
 2140       FORMAT('       INDEX      X(I,J)')
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,2141)
 2141       FORMAT('       (I,J)            ')
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
         END IF
         IF (ISODR) THEN
            DO 240 J = 1,M
               TEMPC0 = '1,'
               DO 230 I=1,N,N-1

                  IF (IFIXX(1,1).LT.0) THEN
                     TEMPC1 = '   NO'
                  ELSE
                     IF (LDIFX.EQ.1) THEN
                        IF (IFIXX(1,J).EQ.0) THEN
                           TEMPC1 = '  YES'
                        ELSE
                           TEMPC1 = '   NO'
                        END IF
                     ELSE
                        IF (IFIXX(I,J).EQ.0) THEN
                           TEMPC1 = '  YES'
                        ELSE
                           TEMPC1 = '   NO'
                        END IF
                     END IF
                  END IF

                  IF (TT(1,1).LT.ZERO) THEN
                     TEMP1 = ABS(TT(1,1))
                  ELSE
                     IF (LDTT.EQ.1) THEN
                        TEMP1 = TT(1,J)
                     ELSE
                        TEMP1 = TT(I,J)
                     END IF
                  END IF

                  IF (WD(1,1,1).LT.ZERO) THEN
                     TEMP2 = ABS(WD(1,1,1))
                  ELSE
                     IF (LDWD.EQ.1) THEN
                        IF (LD2WD.EQ.1) THEN
                           TEMP2 = WD(1,1,J)
                        ELSE
                           TEMP2 = WD(1,J,J)
                        END IF
                     ELSE
                        IF (LD2WD.EQ.1) THEN
                           TEMP2 = WD(I,1,J)
                        ELSE
                           TEMP2 = WD(I,J,J)
                        END IF
                     END IF
                  END IF

                  IF (ANAJAC) THEN
                     IF (CHKJAC .AND.                                  &
                         (((MSGB1.GE.1) .OR. (MSGD1.GE.1)) .AND.       &
                          (I.EQ.1))) THEN
                        ITEMP = -1
                        DO 210 L=1,NQ
                           ITEMP = MAX(ITEMP,MSGD(L,J))
  210                   CONTINUE
                        IF (ITEMP.LE.-1) THEN
                           TEMPC2 = '    UNCHECKED'
                        ELSE IF (ITEMP.EQ.0) THEN
                           TEMPC2 = '     VERIFIED'
                        ELSE IF (ITEMP.GE.1) THEN
                           TEMPC2 = ' QUESTIONABLE'
                        END IF
                     ELSE
                        TEMPC2 = '             '
                     END IF
                     IF (M.LE.9) THEN
  !                     WRITE (LUNRPT,5110) 
  !  &                     TEMPC0,J,X(I,J),
  !  &                     DELTA(I,J),TEMPC1,TEMP1,TEMP2,TEMPC2
                        WRITE (ICOUT,5110) TEMPC0,J,X(I,J),   &
                                           DELTA(I,J),TEMPC1, &
                                           TEMP1,TEMP2,TEMPC2
 5110                   FORMAT(9X,A2,I1,1P,2D12.3,4X,A5,2D10.2,1X,A13)
                        CALL DPWRST('XXX','BUG ')
                     ELSE
!                       WRITE (LUNRPT,5120) 
!    &                     TEMPC0,J,X(I,J),
!    &                     DELTA(I,J),TEMPC1,TEMP1,TEMP2,TEMPC2
                        WRITE (ICOUT,5120) TEMPC0,J,X(I,J),   &
                                           DELTA(I,J),TEMPC1, &
                                           TEMP1,TEMP2,TEMPC2
 5120                   FORMAT(8X,A2,I2,1P,2D12.3,4X,A5,2D10.2,1X,A13)
                        CALL DPWRST('XXX','BUG ')
                     END IF
                  ELSE
                     TEMPC2 = '             '  
                     IF (CDJAC) THEN 
                        TEMP3 = DHSTEP(1,NETA,I,J,STPD,LDSTPD)
                     ELSE
                        TEMP3 = DHSTEP(0,NETA,I,J,STPD,LDSTPD)
                     END IF
                     IF (M.LE.9) THEN
!                       WRITE (LUNRPT,5210) 
!    &                     TEMPC0,J,X(I,J),
!    &                     DELTA(I,J),TEMPC1,TEMP1,TEMP2,TEMP3
                        WRITE (ICOUT,5210) TEMPC0,J,X(I,J),   &
                                           DELTA(I,J),TEMPC1, &
                                           TEMP1,TEMP2,TEMP3
 5210                   FORMAT(9X,A2,I1,1P,2D12.3,4X,A5,2D10.2,1X,D13.5)
                        CALL DPWRST('XXX','BUG ')
                     ELSE
!                       WRITE (LUNRPT,5220) 
!    &                     TEMPC0,J,X(I,J),
!    &                     DELTA(I,J),TEMPC1,TEMP1,TEMP2,TEMP3
                        WRITE (ICOUT,5220) TEMPC0,J,X(I,J),   &
                                           DELTA(I,J),TEMPC1, &
                                           TEMP1,TEMP2,TEMP3
 5220                   FORMAT(8X,A2,I2,1P,2E12.3,4X,A5,2E10.2,1X,E13.5)
                        CALL DPWRST('XXX','BUG ')
                     END IF
                  END IF

                  TEMPC0 = 'N,'

  230          CONTINUE
!              IF (J.LT.M) WRITE (LUNRPT,6000)
               IF (J.LT.M) THEN
                 WRITE (ICOUT,6000)
 6000            FORMAT(' ')
                 CALL DPWRST('XXX','BUG ')
                ENDIF
  240       CONTINUE
         ELSE

            DO 260 J = 1,M
               TEMPC0 = '1,'
               DO 250 I=1,N,N-1
                  IF (M.LE.9) THEN
!                    WRITE (LUNRPT,5110) 
!    &                  TEMPC0,J,X(I,J)
                     WRITE (ICOUT,5110) TEMPC0,J,X(I,J)
                     CALL DPWRST('XXX','BUG ')
                  ELSE
!                    WRITE (LUNRPT,5120) 
!    &                  TEMPC0,J,X(I,J)
                     WRITE (ICOUT,5120) TEMPC0,J,X(I,J)
                     CALL DPWRST('XXX','BUG ')
                  END IF
                  TEMPC0 = 'N,'
  250          CONTINUE
!              IF (J.LT.M) WRITE (LUNRPT,6000)
               IF (J.LT.M) THEN
                  WRITE (ICOUT,6000)
                  CALL DPWRST('XXX','BUG ')
               ENDIF
  260       CONTINUE
         END IF

!  Print response variable data and observation error weights

         IF (.NOT.IMPLCT) THEN
!           WRITE (LUNRPT,3000)
!           WRITE (LUNRPT,3100)
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,3000)
 3000       FORMAT(' --- Response Variable and Epsilon Error Weight',  &
                   ' Summary:')
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,3100)
 3100       FORMAT('       Index      Y(I,L)      Weight')
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,3101)
 3101       FORMAT('       (I,L)                    (WE)')
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            DO 310 L=1,NQ
               TEMPC0 = '1,'
               DO 300 I=1,N,N-1
                  IF (WE(1,1,1).LT.ZERO) THEN
                     TEMP1 = ABS(WE(1,1,1))
                  ELSE IF (LDWE.EQ.1) THEN
                     IF (LD2WE.EQ.1) THEN
                        TEMP1 = WE(1,1,L)
                     ELSE 
                        TEMP1 = WE(1,L,L)
                     END IF
                  ELSE 
                     IF (LD2WE.EQ.1) THEN
                        TEMP1 = WE(I,1,L)
                     ELSE 
                        TEMP1 = WE(I,L,L)
                     END IF
                  END IF
                  IF (NQ.LE.9) THEN
!                    WRITE (LUNRPT,5110) 
!    &                  TEMPC0,L,Y(I,L),TEMP1
                     WRITE (ICOUT,5110) TEMPC0,L,Y(I,L),TEMP1
                     CALL DPWRST('XXX','BUG ')
                  ELSE
!                    WRITE (LUNRPT,5120) 
!    &                  TEMPC0,L,Y(I,L),TEMP1
                     WRITE (ICOUT,5120) TEMPC0,L,Y(I,L),TEMP1
                     CALL DPWRST('XXX','BUG ')
                  END IF
                  TEMPC0 = 'N,'
  300          CONTINUE
!              IF (L.LT.NQ) WRITE (LUNRPT,6000)
               IF (L.LT.NQ) THEN
                  WRITE (ICOUT,999)
                  CALL DPWRST('XXX','BUG ')
               ENDIF
  310       CONTINUE
         END IF
      END IF

      RETURN

!  Format statements

!1130 FORMAT
!    &   ('                       C=',I1,' ==> covariance matrix will',
!    &                                     ' be computed using')
!1131 FORMAT
!    &   ('                               derivatives re-',
!    &                                     'evaluated at the solution.')
!1132 FORMAT
!    &   ('                               derivatives from the',
!    &                                     ' last iteration.')
!1133 FORMAT
!    &   ('                       C=',I1,' ==> covariance matrix will',
!    &                                     ' not be computed.')
!1140 FORMAT
!    &   ('                       D=',I1,' ==> derivatives are',
!    &                                     ' supplied by user.')
!1141 FORMAT
!    &   ('                               derivatives were checked.'/
!    &    '                               results appear questionable.')
!1142 FORMAT
!    &   ('                               derivatives were checked.'/
!    &    '                               results appear correct.')
!1143 FORMAT
!    &   ('                               derivatives were not',
!    &                                     ' checked.')
!1144 FORMAT
!    &   ('                       D=',I1,' ==> derivatives are',
!    &                                     ' estimated by central',
!    &                                     ' differences.')
!1145 FORMAT
!    &   ('                       D=',I1,' ==> derivatives are',
!    &                                     ' estimated by forward',
!    &                                     ' differences.')
!1150 FORMAT
!    &   ('                       E=',I1,' ==> method is implicit ODR.')
!1151 FORMAT
!    &   ('                       E=',I1,' ==> method is explicit ODR.')
!1152 FORMAT
!    &   ('                       E=',I1,' ==> method is explicit OLS.')
!1200 FORMAT
!    &   ('       NDIGIT = ',I5,'          (estimated by ODRPACK95)')
!1210 FORMAT
!    &   ('       NDIGIT = ',I5,'          (supplied by user)')
!1300 FORMAT
!    &   ('       TAUFAC = ',1P,E12.2)
!1400 FORMAT
!    &   (/' --- Stopping Criteria:'/
!    &     '        SSTOL = ',1P,E12.2,
!    &                      '   (sum of squares stopping tolerance)'/
!    &     '       PARTOL = ',1P,E12.2,
!    &                      '   (parameter stopping tolerance)'/
!    &     '        MAXIT = ',I5,
!    &                      '          (maximum number of iterations)')
!1500 FORMAT
!    &   (/' --- Initial Sum of Squared Weighted Deltas =',
!    &     17X,1P,E17.8)
!1510 FORMAT
!    &   ( '         Initial Penalty Function Value     =',1P,E17.8/
!    &     '                 Penalty Term               =',1P,E17.8/
!    &     '                 Penalty Parameter          =',1P,E10.1)
!1600 FORMAT
!    &   (/' --- Initial Weighted Sum of Squares        =',
!    &     17X,1P,E17.8)
!1610 FORMAT
!    &   ( '         Sum of Squared Weighted Deltas     =',1P,E17.8/
!    &     '         Sum of Squared Weighted Epsilons   =',1P,E17.8)
!2010 FORMAT
!    &   (/' --- Explanatory Variable and Delta Weight Summary:')
!2020 FORMAT
!    &   (/' --- Explanatory Variable Summary:')
!2110 FORMAT
!    &   (/'       Index      X(I,J)  DELTA(I,J)    Fixed',
!    &           '     Scale    Weight    Derivative'/
!    &     '                                             ',
!    &           '                        Assessment'/,
!    &     '       (I,J)                          (IFIXX)',
!    &           '    (SCLD)      (WD)              '/)
!2120 FORMAT
!    &   (/'       Index      X(I,J)  DELTA(I,J)    Fixed',
!    &           '     Scale    Weight              '/
!    &     '                                             ',
!    &           '                                  '/,
!    &     '       (I,J)                          (IFIXX)',
!    &           '    (SCLD)      (WD)              '/)
!2130 FORMAT
!    &   (/'       Index      X(I,J)  DELTA(I,J)    Fixed',
!    &           '     Scale    Weight    Derivative'/
!    &     '                                             ',
!    &           '                         Step Size'/,
!    &     '       (I,J)                          (IFIXX)',
!    &           '    (SCLD)      (WD)        (STPD)'/)
!2140 FORMAT
!    &   (/'       Index      X(I,J)'/
!    &     '       (I,J)            '/)
!3000 FORMAT
!    &   (/' --- Response Variable and Epsilon Error Weight',
!    &   ' Summary:')
!3100 FORMAT
!    &   (/'       Index      Y(I,L)      Weight'/
!    &     '       (I,L)                    (WE)'/)
!4000 FORMAT
!    &   (/' --- Function Parameter Summary:')
!4110 FORMAT
!    &   (/'       Index   BETA(K)    Fixed     Scale   LOWER(K)',
!    &     '   UPPER(K)    Derivative'/
!    &     '                                                    ',
!    &     '               Assessment'/,
!    &     '         (K)            (IFIXB)    (SCLB)           ',
!    &     '                         '/)
!4120 FORMAT
!    &   (/'       Index   BETA(K)    Fixed     Scale   LOWER(K)',
!    &     '   UPPER(K)              '/
!    &     '                                                    ',
!    &     '                         '/,
!    &     '         (K)            (IFIXB)    (SCLB)           ',
!    &     '                         '/)
!4200 FORMAT
!    &   (/'       Index   BETA(K)    Fixed     Scale   LOWER(K)',
!    &     '   UPPER(K)    Derivative'/
!    &     '                                                    ',
!    &     '                Step Size'/,
!    &     '         (K)            (IFIXB)    (SCLB)           ',
!    &     '                   (STPB)'/)
!4310 FORMAT
!    &    (7X,I5,1P,E10.2,4X,A5,E10.2,E11.2E3,E11.2E3,1X,A13)
!4320 FORMAT
!    &    (7X,I5,1P,E10.2,4X,A5,E10.2,E11.2E3,E11.2E3,1X,E13.5)
!5110 FORMAT
!    &    (9X,A2,I1,1P,2E12.3,4X,A5,2E10.2,1X,A13)
!5120 FORMAT
!    &    (8X,A2,I2,1P,2E12.3,4X,A5,2E10.2,1X,A13)
!5210 FORMAT
!    &    (9X,A2,I1,1P,2E12.3,4X,A5,2E10.2,1X,E13.5)
!5220 FORMAT
!    &    (8X,A2,I2,1P,2E12.3,4X,A5,2E10.2,1X,E13.5)
!6000 FORMAT
!    &   (' ')
      END SUBROUTINE
!DODPC2
      SUBROUTINE DODPC2(IPRTMP,LUNRPT, FSTITR,IMPLCT,PRTPEN,PNLTY,     &
                        NITER,NFEV,WSS,ACTRED,PRERED,ALPHA,TAU,PNORM,  &
                        NP,BETA)
!***Begin Prologue  DODPC2
!***Refer to  ODR
!***Routines Called  (NONE)
!***Date Written   860529   (YYMMDD)
!***Revision Date  920304   (YYMMDD)
!***Purpose  Generate iteration reports
!***End Prologue  DODPC2

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      REAL (KIND=R8) ACTRED,ALPHA,PNLTY,PNORM,PRERED,TAU,WSS
      INTEGER IPRTMP,LUNRPT,NFEV,NITER,NP
      LOGICAL FSTITR,IMPLCT,PRTPEN

!...Array arguments
      REAL (KIND=R8) BETA(NP)

!...Local scalars
      REAL (KIND=R8) RATIO,ZERO
      INTEGER J,K,L
      CHARACTER GN*3
!
!     Following 2 lines added for Dataplot integration
!
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'

!...Data statements
      DATA ZERO /0.0E0_R8/

!...Variable Definitions (alphabetically)
!   ACTRED:  The actual relative reduction in the sum-of-squares.
!   ALPHA:   The Levenberg-Marquardt parameter.
!   BETA:    The function parameters.
!   FSTITR:  The variable designating whether this is the first 
!            iteration (FSTITR=.TRUE.) or not (FSTITR=.FALSE.).
!   GN:      The CHARACTER*3 variable indicating whether a Gauss-Newton
!            step was taken.
!   IMPLCT:  The variable designating whether the solution is by 
!            implicit ODR (IMPLCT=TRUE) or explicit ODR (IMPLCT=FALSE).
!   IPRTMP:  The value indicating the report to be printed.
!   J:       An indexing variable.
!   K:       An indexing variable.
!   L:       An indexing variable.
!   LUNRPT:  The logical unit number used for computation reports.
!   NFEV:    The number of function evaluations.
!   NITER:   The number of iterations.
!   NP:      The number of function parameters.
!   PNLTY:   The penalty parameter for an implicit model.
!   PNORM:   The norm of the scaled estimated parameters.
!   PRERED:  The predicted relative reduction in the sum-of-squares. 
!   PRTPEN:  The variable designating whether the penalty parameter is
!            to be printed in the iteration report (PRTPEN=TRUE) or not 
!            (PRTPEN=FALSE).
!   RATIO:   The ratio of TAU to PNORM.
!   TAU:     The trust region diameter.
!   WSS:     The sum-of-squares of the weighted EPSILONS and DELTAS.
!   ZERO:    The value 0.0E0_R8.


!***First executable statement  DODPC2

      IF(ISUBG4.EQ.'DPC2')THEN
        WRITE(ICOUT,52)LUNRPT
   52   FORMAT('LUNRPT = ',I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!

      IF (FSTITR) THEN
         IF (IPRTMP.EQ.1) THEN
            IF (IMPLCT) THEN
!              WRITE (LUNRPT,1121)
               WRITE (ICOUT,999)
  999          FORMAT(1X)
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,999)
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,1121)
 1121          FORMAT('         CUM.      PENALTY    ACT. REL.   ',    &
                      'PRED. REL.')
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,2121)
 2121          FORMAT('  IT.  NO. FN     FUNCTION   SUM-OF-SQS   ',    &
                      'SUM-OF-SQS               G-N')
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,3121)
 3121          FORMAT(' NUM.   EVALS        VALUE    REDUCTION    ',   &
                      'REDUCTION   TAU/PNORM  STEP')
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,4121)
 4121          FORMAT(' ----  ------  -----------  -----------  ',     &
                      '-----------   ---------  ----')
               CALL DPWRST('XXX','BUG ')
            ELSE
!              WRITE (LUNRPT,1122)
               WRITE (ICOUT,999)
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,999)
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,1122)
 1122          FORMAT('         CUM.                 ACT. REL.   ',    &
                      'PRED. REL.')
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,2122)
 2122          FORMAT('  IT.  NO. FN     WEIGHTED   SUM-OF-SQS   ',    &
                      'SUM-OF-SQS               G-N')
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,3122)
 3122          FORMAT(' NUM.   EVALS   SUM-OF-SQS    REDUCTION    ',   &
                      'REDUCTION   TAU/PNORM  STEP')
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,4122)
 4122          FORMAT(' ----  ------  -----------  -----------  ',     &
                      '-----------   ---------  ----')
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,999)
               CALL DPWRST('XXX','BUG ')
            END IF
         ELSE
            IF (IMPLCT) THEN
!              WRITE (LUNRPT,1131)
               WRITE (ICOUT,999)
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,999)
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,1131)
 1131          FORMAT('         CUM.      PENALTY    ACT. REL.   '     &
                      'PRED. REL.')
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,2131)
 2131          FORMAT('  IT.  NO. FN     FUNCTION   SUM-OF-SQS   ',    &
                      'SUM-OF-SQS               G-N      BETA    ',    &
                      '-------------->')
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,3131)
 3131          FORMAT(' NUM.   EVALS        VALUE    REDUCTION    ',   &
                      'REDUCTION   TAU/PNORM  STEP     INDEX      ',   &
                      '     VALUE')
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,4131)
 4131          FORMAT(' ----  ------  -----------  -----------  ',     &
                      '-----------   ---------  ----     -----  ',     &
                      '         -----')
               CALL DPWRST('XXX','BUG ')
            ELSE
!              WRITE (LUNRPT,1132)
               WRITE (ICOUT,999)
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,999)
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,1132)
 1132          FORMAT('         CUM.                 ACT. REL.   ',    &
                      'PRED. REL.')
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,999)
               CALL DPWRST('XXX','BUG ')
            END IF
         END IF
      END IF
      IF (PRTPEN) THEN
!        WRITE (LUNRPT,1133) PNLTY
 1133    FORMAT(' PENALTY PARAMETER VALUE = ', 1P,E10.1)
         CALL DPWRST('XXX','BUG ')
      END IF

      IF (ALPHA.EQ.ZERO) THEN
         GN = 'YES'
      ELSE
         GN = ' NO'
      END IF
      IF (PNORM.NE.ZERO) THEN
         RATIO = TAU/PNORM
      ELSE
         RATIO = ZERO
      END IF
      IF (IPRTMP.EQ.1) THEN
!        WRITE (LUNRPT,1141) NITER,NFEV,WSS,ACTRED,PRERED,
!    &                       RATIO,GN
         WRITE (ICOUT,1141) NITER,NFEV,WSS,ACTRED,PRERED,RATIO,GN
 1141    FORMAT(1X,I4,I8,1X,1P,D12.5,2D13.4,D11.3,3X,A3,7X,I3,3D16.8)
         CALL DPWRST('XXX','BUG ')
      ELSE
         J = 1
         K = MIN(3,NP)
         IF (J.EQ.K) THEN
!           WRITE (LUNRPT,1141) NITER,NFEV,WSS,ACTRED,PRERED,
!    &                          RATIO,GN,J,BETA(J)
            WRITE (ICOUT,1141) NITER,NFEV,WSS,ACTRED,PRERED,           &
                                RATIO,GN,J,BETA(J)
            CALL DPWRST('XXX','BUG ')
         ELSE
!           WRITE (LUNRPT,1142) NITER,NFEV,WSS,ACTRED,PRERED,
!    &                          RATIO,GN,J,K,(BETA(L),L=J,K)
            WRITE (ICOUT,1142) NITER,NFEV,WSS,ACTRED,PRERED,           &
                                RATIO,GN,J,K,(BETA(L),L=J,K)
 1142       FORMAT(1X,I4,I8,1X,1P,E12.5,2E13.4,E11.3,3X,A3,1X,I3,      &
             ' To',I3,3E16.8)
            CALL DPWRST('XXX','BUG ')
         END IF
         IF (NP.GT.3) THEN
            DO 10 J=4,NP,3
               K = MIN(J+2,NP)
               IF (J.EQ.K) THEN
!                 WRITE (LUNRPT,1151) J,BETA(J)
                  WRITE (ICOUT,1151) J,BETA(J)
 1151             FORMAT(76X,I3,1P,D16.8)
                  CALL DPWRST('XXX','BUG ')
               ELSE
!                 WRITE (LUNRPT,1152) J,K,(BETA(L),L=J,K)
                  WRITE (ICOUT,1152) J,K,(BETA(L),L=J,K)
 1152             FORMAT(70X,I3,' TO',I3,1P,3D16.8)
                  CALL DPWRST('XXX','BUG ')
               END IF
   10       CONTINUE
         END IF
      END IF

      RETURN

!  Format statements

!1121 FORMAT
!    &   (//
!    &    '         Cum.      Penalty    Act. Rel.   Pred. Rel.'/
!    &    '  It.  No. FN     Function   Sum-of-Sqs   Sum-of-Sqs',
!    &    '              G-N'/
!    &    ' Num.   Evals        Value    Reduction    Reduction',
!    &    '  TAU/PNORM  Step'/
!    &    ' ----  ------  -----------  -----------  -----------',
!    &    '  ---------  ----')
!1122 FORMAT
!    &   (//
!    &    '         Cum.                 Act. Rel.   Pred. Rel.'/
!    &    '  It.  No. FN     Weighted   Sum-of-Sqs   Sum-of-Sqs',
!    &    '              G-N'/
!    &    ' Num.   Evals   Sum-of-Sqs    Reduction    Reduction',
!    &    '  TAU/PNORM  Step'/
!    &    ' ----  ------  -----------  -----------  -----------',
!    &    '  ---------  ----'/)
!1131 FORMAT
!    &   (//
!    &    '         Cum.      Penalty    Act. Rel.   Pred. Rel.'/
!    &    '  It.  No. FN     Function   Sum-of-Sqs   Sum-of-Sqs',
!    &    '              G-N      BETA -------------->'/
!    &    ' Num.   Evals        Value    Reduction    Reduction',
!    &    '  TAU/PNORM  Step     Index           Value'/
!    &    ' ----  ------  -----------  -----------  -----------',
!    &    '  ---------  ----     -----           -----')
!1132 FORMAT
!    &   (//
!    &    '         Cum.                 Act. Rel.   Pred. Rel.'/
!    &    '  It.  No. FN     Weighted   Sum-of-Sqs   Sum-of-Sqs',
!    &    '              G-N      BETA -------------->'/
!    &    ' Num.   Evals   Sum-of-Sqs    Reduction    Reduction',
!    &    '  TAU/PNORM  Step     Index           Value'/
!    &    ' ----  ------  -----------  -----------  -----------',
!    &    '  ---------  ----     -----           -----'/)
!1133 FORMAT
!    &   (/' Penalty Parameter Value = ', 1P,E10.1)
!1141 FORMAT
!    &   (1X,I4,I8,1X,1P,E12.5,2E13.4,E11.3,3X,A3,7X,I3,3E16.8)
!1142 FORMAT
!    &   (1X,I4,I8,1X,1P,E12.5,2E13.4,E11.3,3X,A3,1X,I3,' To',I3,3E16.8)
!1151 FORMAT
!    &   (76X,I3,1P,E16.8)
!1152 FORMAT
!    &   (70X,I3,' To',I3,1P,3E16.8)
      END SUBROUTINE
!DODPC3
      SUBROUTINE DODPC3(IPRTMP,LUNRPT,ISODR,IMPLCT,DIDVCV,DOVCV,REDOJ, &
                        ANAJAC,N,M,NP,NQ,NPP,                          &
                        INFO,NITER,NFEV,NJEV,IRANK,RCOND,ISTOP,        &
                        WSS,WSSDEL,WSSEPS,PNLTY,RVAR,IDF,              &
                        BETA,SDBETA,IFIXB2,F,DELTA,LOWER,UPPER)
!***Begin Prologue  DODPC3
!***Refer to  ODR
!***Routines Called  DPPT
!***Date Written   860529   (YYMMDD)
!***REvision Date  920619   (YYMMDD)
!***Purpose  Generate final summary report
!***End Prologue  DODPC3

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      REAL (KIND=R8) PNLTY,RCOND,RVAR,WSS,WSSDEL,WSSEPS
      INTEGER IDF,INFO,IPRTMP,IRANK,ISTOP,LUNRPT,M,                    &
              N,NFEV,NITER,NJEV,NP,NPP,NQ
      LOGICAL ANAJAC,DIDVCV,DOVCV,IMPLCT,ISODR,REDOJ

!...Array arguments
      REAL (KIND=R8) BETA(NP),DELTA(N,M),F(N,NQ),LOWER(NP),UPPER(NP),  &
                     SDBETA(NP)
      INTEGER IFIXB2(NP)

!...Local scalars
      REAL (KIND=R8) TVAL
      INTEGER D1,D2,D3,D4,D5,I,J,K,L,NPLM1
      CHARACTER FMT1*90

!...External functions
!     REAL (KIND=R8) DPPT
!     EXTERNAL DPPT
!
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!

!...Variable Definitions (alphabetically)
!   ANAJAC:  The variable designating whether the JACOBIANS are computed
!            by finite differences (ANAJAC=FALSE) or not (ANAJAC=TRUE).
!   BETA:    The function parameters.
!   D1:      The first digit of INFO.
!   D2:      The second digit of INFO.
!   D3:      The third digit of INFO.
!   D4:      The fourth digit of INFO.
!   D5:      The fifth digit of INFO.
!   DELTA:   The estimated errors in the explanatory variables.
!   DIDVCV:  The variable designating whether the covariance matrix was
!            computed (DIDVCV=TRUE) or not (DIDVCV=FALSE).
!   DOVCV:   The variable designating whether the covariance matrix was
!            to be computed (DOVCV=TRUE) or not (DOVCV=FALSE).
!   F:       The estimated values of EPSILON.
!   FMT1:    A CHARACTER*90 variable used for formats.
!   I:       An indexing variable.
!   IDF:     The degrees of freedom of the fit, equal to the number of
!            observations with nonzero weighted derivatives minus the
!            number of parameters being estimated.
!   IFIXB2:  The values designating whether the elements of BETA were 
!            estimated, fixed, or dropped because they caused rank 
!            deficiency, corresponding to values of IFIXB2 equaling 1,
!            0, and -1, respectively.  If IFIXB2 is -2, then no attempt
!            was made to estimate the parameters because MAXIT = 0.
!   IMPLCT:  The variable designating whether the solution is by 
!            implicit ODR (IMPLCT=TRUE) or explicit ODR (IMPLCT=FALSE).
!   INFO:    The variable designating why the computations were stopped.
!   IPRTMP:  The variable indicating what is to be printed.
!   IRANK:   The rank deficiency of the Jacobian wrt BETA.
!   ISODR:   The variable designating whether the solution is by ODR 
!            (ISODR=TRUE) or by OLS (ISODR=FALSE).
!   ISTOP:   The variable designating whether there are problems
!            computing the function at the current BETA and DELTA.
!   J:       An indexing variable.
!   K:       An indexing variable.
!   L:       An indexing variable.
!   LOWER:   Lower bound on BETA.
!   LUNRPT:  The logical unit number used for computation reports.
!   M:       The number of columns of data in the explanatory variable.
!   N:       The number of observations.
!   NFEV:    The number of function evaluations.
!   NITER:   The number of iterations.
!   NJEV:    The number of Jacobian evaluations.
!   NP:      The number of function parameters.
!   NPLM1:   The number of items to be printed per line, minus one.
!   NPP:     The number of function parameters being estimated.
!   NQ:      The number of responses per observation.
!   PNLTY:   The penalty parameter for an implicit model.
!   RCOND:   The approximate reciprocal condition of TFJACB.
!   REDOJ:   The variable designating whether the Jacobian matrix is
!            to be recomputed for the computation of the covariance 
!            matrix (REDOJ=TRUE) or not (REDOJ=FALSE).
!   RVAR:    The residual variance.
!   SDBETA:  The standard errors of the estimated parameters.
!   TVAL:    The value of the 97.5 percent point function for the
!            T distribution.
!   UPPER:   Upper bound on BETA.
!   WSS:     The sum-of-squares of the weighted EPSILONS and DELTAS.
!   WSSDEL:  The sum-of-squares of the weighted DELTAS.
!   WSSEPS:  The sum-of-squares of the weighted EPSILONS.


!***First executable statement  DODPC3

!
      IF(ISUBG4.EQ.'DPC3')THEN
        WRITE(ICOUT,52)LUNRPT
   52   FORMAT('LUNRPT = ',I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!

      D1 = INFO/10000
      D2 = MOD(INFO,10000)/1000
      D3 = MOD(INFO,1000)/100
      D4 = MOD(INFO,100)/10
      D5 = MOD(INFO,10)

!  Print stopping conditions

!     WRITE (LUNRPT,1000)
      WRITE (ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE (ICOUT,1000)
 1000 FORMAT(' --- STOPPING CONDITIONS:')
      CALL DPWRST('XXX','BUG ')
      IF (INFO.LE.9) THEN
         IF (INFO.EQ.1) THEN
            WRITE (ICOUT,1011) INFO
 1011       FORMAT('         INFO = ',I5,' ==> SUM OF SQUARES CONVERGENCE.')
            CALL DPWRST('XXX','BUG ')
         ELSE IF (INFO.EQ.2) THEN
            WRITE (ICOUT,1012) INFO
 1012       FORMAT('         INFO = ',I5,' ==> PARAMETER CONVERGENCE.')
            CALL DPWRST('XXX','BUG ')
         ELSE IF (INFO.EQ.3) THEN
            WRITE (ICOUT,1013) INFO
 1013       FORMAT('         INFO = ',I5,' ==> SUM OF SQUARES ',       &
                   'CONVERGENCE AND PARAMETER CONVERGENCE.')
            CALL DPWRST('XXX','BUG ')
         ELSE IF (INFO.EQ.4) THEN
            WRITE (ICOUT,1014) INFO
 1014       FORMAT('         INFO = ',I5,' ==> ITERATION LIMIT REACHED.')
            CALL DPWRST('XXX','BUG ')
         ELSE IF (INFO.LE.9) THEN
            WRITE (ICOUT,1015) INFO
 1015       FORMAT('         INFO = ',I5,' ==> UNEXPECTED VALUE,',     &
                   ' PROBABLY INDICATING')
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,1016)
 1016       FORMAT('                           INCORRECTLY SPECIFIED', &
                   ' USER INPUT.')
            CALL DPWRST('XXX','BUG ')
         END IF
      ELSE IF (INFO.LE.9999) THEN

!  Print warning diagnostics

!        WRITE (LUNRPT,1020) INFO
!        IF (D2.EQ.1) WRITE (LUNRPT,1021)
!        IF (D3.EQ.1) WRITE (LUNRPT,1022)
!        IF (D4.EQ.1) WRITE (LUNRPT,1023)
!        IF (D4.EQ.2) WRITE (LUNRPT,1024)
!        IF (D5.EQ.1) THEN
!           WRITE (LUNRPT,1031)
!        ELSE IF (D5.EQ.2) THEN
!           WRITE (LUNRPT,1032)
!        ELSE IF (D5.EQ.3) THEN
!           WRITE (LUNRPT,1033)
!        ELSE IF (D5.EQ.4) THEN
!           WRITE (LUNRPT,1034)
!        ELSE IF (D5.LE.9) THEN
!           WRITE (LUNRPT,1035) D5
!        END IF
         WRITE (ICOUT,1017) INFO
 1017    FORMAT('         INFO = ',I5.4)
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,1018)
 1018    FORMAT('              =  ABCD, WHERE A NONZERO VALUE FOR ',   &
                'DIGIT A, B, OR C INDICATES WHY')
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,1019)
 1019    FORMAT('                       THE RESULTS MIGHT BE ',        &
                'QUESTIONABLE, AND DIGIT D INDICATES')
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,1020)
 1020    FORMAT('                       THE ACTUAL STOPPING CONDITION.')
         CALL DPWRST('XXX','BUG ')
         IF (D2.EQ.1) THEN
            WRITE (ICOUT,1021)
 1021       FORMAT('                       A=1 ==> DERIVATIVES ARE',   &
                   ' QUESTIONABLE.')
            CALL DPWRST('XXX','BUG ')
         ENDIF
         IF (D3.EQ.1) THEN
            WRITE (ICOUT,1022)
 1022       FORMAT('                       B=1 ==> USER SET ISTOP TO', &
                   ' NONZERO VALUE DURING LAST')
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,1027)
 1027       FORMAT('                               CALL TO SUBROUTINE FCN.')
            CALL DPWRST('XXX','BUG ')
         ENDIF
         IF (D4.EQ.1) THEN
            WRITE (ICOUT,1023)
 1023       FORMAT('                       C=1 ==> DERIVATIVES ARE NOT', &
                   ' FULL RANK AT THE SOLUTION.')
            CALL DPWRST('XXX','BUG ')
         ENDIF
         IF (D4.EQ.2) THEN
            WRITE (ICOUT,1024)
 1024       FORMAT('                       C=2 ==> DERIVATIVES ARE ',  &
                   'ZERO RANK AT THE SOLUTION.')
            CALL DPWRST('XXX','BUG ')
         ENDIF
         IF (D5.EQ.1) THEN
            WRITE (ICOUT,1031)
 1031       FORMAT('                       D=1 ==> SUM OF SQUARES ',   &
                   'CONVERGENCE.')
            CALL DPWRST('XXX','BUG ')
         ELSE IF (D5.EQ.2) THEN
            WRITE (ICOUT,1032)
 1032       FORMAT('                       D=2 ==> PARAMETER CONVERGENCE.')
            CALL DPWRST('XXX','BUG ')
         ELSE IF (D5.EQ.3) THEN
            WRITE (ICOUT,1033)
 1033       FORMAT('                       D=3 ==> SUM OF SQUARES ',   &
                   'CONVERGENCE AND PARAMETER CONVERGENCE.')
            CALL DPWRST('XXX','BUG ')
         ELSE IF (D5.EQ.4) THEN
            WRITE (ICOUT,1034)
 1034       FORMAT('                       D=4 ==> ITERATION LIMIT REACHED.')
            CALL DPWRST('XXX','BUG ')
         ELSE IF (D5.LE.9) THEN
            WRITE (ICOUT,1035) D5
 1035       FORMAT('                       D=',I1,' ==> UNEXPECTED ',  &
                   'VALUE, PROBABLY INDICATING')
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,1036)
 1036       FORMAT('                               INCORRECTLY ',      &
                   'SPECIFIED USER INPUT.')
            CALL DPWRST('XXX','BUG ')
         END IF
      ELSE

!  Print error messages

!        WRITE (LUNRPT,1040) INFO
         WRITE (ICOUT,1039) INFO
 1039    FORMAT('         INFO = ',I5.5)
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,1040)
 1040    FORMAT('              = ABCDE, WHERE A NONZERO VALUE FOR A ', &
                'GIVEN DIGIT INDICATES AN')
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,1041)
 1041    FORMAT('                       ABNORMAL STOPPING CONDITION.')
         CALL DPWRST('XXX','BUG ')
         IF (D1.EQ.5) THEN
!           WRITE (LUNRPT,1042)
            WRITE (ICOUT,1042)
 1042       FORMAT('                       A=5 ==> USER STOPPED ',     &
                   'COMPUTATIONS IN SUBROUTINE FCN.')
            CALL DPWRST('XXX','BUG ')
            IF (D2.NE.0) THEN
!              WRITE (LUNRPT,1043) D2
               WRITE (ICOUT,1043) D2
 1043          FORMAT('                       B=',I1,' ==> ',          &
                      'COMPUTATIONS WERE STOPPED DURING THE')
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,11043)
11043          FORMAT('                                    FUNCTION ', &
                      'EVALUATION.')
               CALL DPWRST('XXX','BUG ')
            ENDIF
            IF (D3.EQ.3) THEN
!              WRITE (LUNRPT,1044) D3
               WRITE (ICOUT,1044) D3
 1044          FORMAT('                       C=',I1,' ==> ',          &
                      'COMPUTATIONS WERE STOPPED BECAUSE')
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,1047)
 1047          FORMAT('                                    DERIVAT',   &
                      'IVES WITH RESPECT TO DELTA WERE')
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,1048)
 1048          FORMAT('                                    COMPUTED ', &
                      'BY SUBROUTINE FCN WHEN')
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,1049)
 1049          FORMAT('                                    FIT IS OLS.')
               CALL DPWRST('XXX','BUG ')
            ELSE IF (D3.NE.0) THEN
!              WRITE (LUNRPT,1045) D3
               WRITE (ICOUT,1045) D3
 1045          FORMAT('                       C=',I1,' ==> ',          &
                      'COMPUTATIONS WERE STOPPED DURING THE')
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,1046)
 1046          FORMAT('                                    JACOBIAN ', &
                      'EVALUATION.')
               CALL DPWRST('XXX','BUG ')
            END IF
         ELSE IF (D1.EQ.6) THEN
!           WRITE (LUNRPT,1050)
            WRITE (ICOUT,1050)
 1050       FORMAT('                       A=6 ==> NUMERICAL ',        &
                   'INSTABILITIES HAVE BEEN DETECTED,')
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,1051)
 1051       FORMAT('                               POSSIBLY ',         &
                   'INDICATING A DISCONTINUITY IN THE')
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,1052)
 1052       FORMAT('                               DERIVATIVES OR A ', &
                   'POOR CHOICE OF PROBLEM')
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,1053)
 1053       FORMAT('                               SCALE OR WEIGHTS.')
            CALL DPWRST('XXX','BUG ')
         ELSE
!           WRITE (LUNRPT,1060) D1
            WRITE (ICOUT,1060) D1
 1060       FORMAT('                       A=',I1,' ==> UNEXPECTED ',  &
                   'VALUE, PROBABLY INDICATING')
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,1061)
 1061       FORMAT('                               INCORRECTLY ',      &
                   'SPECIFIED USER INPUT.')
            CALL DPWRST('XXX','BUG ')
         END IF
      END IF

!  Print misc. stopping info

!     WRITE (LUNRPT,1300) NITER
!     WRITE (LUNRPT,1310) NFEV
!     IF (ANAJAC) WRITE (LUNRPT,1320) NJEV
!     WRITE (LUNRPT,1330) IRANK
!     WRITE (LUNRPT,1340) RCOND
!     WRITE (LUNRPT,1350) ISTOP
      WRITE (ICOUT,1300) NITER
 1300 FORMAT('        NITER = ',I5,'          (NUMBER OF ITERATIONS)')
      CALL DPWRST('XXX','BUG ')
      WRITE (ICOUT,1310) NFEV
 1310 FORMAT('         NFEV = ',I5,'          (NUMBER OF FUNCTION ',   &
             'EVALUATIONS)')
      CALL DPWRST('XXX','BUG ')
      IF (ANAJAC) THEN
         WRITE (ICOUT,1320) NJEV
 1320    FORMAT('         NJEV = ',I5,                                 &
                '          (NUMBER OF JACOBIAN EVALUATIONS)')
         CALL DPWRST('XXX','BUG ')
      ENDIF
      WRITE (ICOUT,1330) IRANK
 1330 FORMAT('        IRANK = ',I5,'          (RANK DEFICIENCY)')
      CALL DPWRST('XXX','BUG ')
      WRITE (ICOUT,1340) RCOND
 1340 FORMAT('        RCOND = ',1P,D12.2,'   (INVERSE CONDITION ',     &
             'NUMBER)')
      CALL DPWRST('XXX','BUG ')
      WRITE (ICOUT,1350) ISTOP
 1350 FORMAT('        ISTOP = ',I5,   &
             '          (RETURNED BY USER FROM SUBROUTINE FCN)')
      CALL DPWRST('XXX','BUG ')

!  Print final sum of squares

      IF (IMPLCT) THEN
!        WRITE (LUNRPT,2000) WSSDEL
         WRITE (ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,2000) WSSDEL
 2000    FORMAT(' --- FINAL SUM OF SQUARED WEIGHTED DELTAS = ',        &
                17X,1P,D17.8)
         CALL DPWRST('XXX','BUG ')
         IF (ISODR) THEN
!           WRITE (LUNRPT,2010) WSS,WSSEPS,PNLTY
            WRITE (ICOUT,2010) WSS
 2010       FORMAT('         FINAL PENALTY FUNCTION VALUE     = ',     &
                   1P,D17.8)
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,2011) WSSEPS
 2011       FORMAT('               PENALTY TERM               = ',     &
                   1P,D17.8)
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,2012) PNLTY
 2012       FORMAT('               PENALTY PARAMETER          = ',     &
                   1P,D10.1)
            CALL DPWRST('XXX','BUG ')
         END IF
      ELSE
!        WRITE (LUNRPT,2100) WSS
         WRITE (ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,2100) WSS
 2100    FORMAT(' --- FINAL WEIGHTED SUMS OF SQUARES       = ',        &
                17X,1P,D17.8)
         CALL DPWRST('XXX','BUG ')
         IF (ISODR) THEN
!           WRITE (LUNRPT,2110) WSSDEL,WSSEPS
            WRITE (ICOUT,2110) WSSDEL
 2110       FORMAT('         SUM OF SQUARED WEIGHTED DELTAS   = ',     &
                   1P,D17.8)
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,2111) WSSEPS
 2111       FORMAT('         SUM OF SQUARED WEIGHTED EPSILONS = ',     &
                   1P,D17.8)
            CALL DPWRST('XXX','BUG ')
         END IF
      END IF
      IF (DIDVCV) THEN
!        WRITE (LUNRPT,2200) SQRT(RVAR),IDF
         WRITE (ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,2200) SQRT(RVAR)
 2200    FORMAT(' --- RESIDUAL STANDARD DEVIATION          = ',        &
                17X,1P,D17.8)
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,2201) IDF
 2201    FORMAT('         DEGREES OF FREEDOM               =',I5)
         CALL DPWRST('XXX','BUG ')
      END IF

      NPLM1 = 3

!  Print estimated BETA's, and,
!  if, full rank, their standard errors

!     WRITE (LUNRPT,3000)
      WRITE (ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE (ICOUT,3000)
 3000 FORMAT(' --- ESTIMATED BETA(J), J = 1, ..., NP:')
      CALL DPWRST('XXX','BUG ')
      IF (DIDVCV) THEN
!        WRITE (LUNRPT,7300)
         WRITE (ICOUT,7300)
 7300    FORMAT('                     BETA      S.D. BETA',            &
                '    ---- 95%  CONFIDENCE INTERVAL ----')
         CALL DPWRST('XXX','BUG ')
!        USE DATAPLOT T PERCENT POINT FUNCTION
!        TVAL = DPPT(0.975E0_R8,IDF)
         AVAL=0.975
         ADF=REAL(IDF)
         CALL TPPF(AVAL,ADF,TVAL)
         DO 10 J=1,NP
            IF (IFIXB2(J).GE.1) THEN
 !             WRITE (LUNRPT,8400) J,BETA(J),
 !   &                             LOWER(J),UPPER(J),
 !   &                             SDBETA(J),
 !   &                             BETA(J)-TVAL*SDBETA(J),
 !   &                             BETA(J)+TVAL*SDBETA(J) 
               WRITE (ICOUT,8400) J,BETA(J),SDBETA(J),   &
                                   BETA(J)-TVAL*SDBETA(J),   &
                                   BETA(J)+TVAL*SDBETA(J)
 8400          FORMAT(3X,I5,1X,1P,D16.8,3X,D12.4,3X,D16.8,1X,'TO',D16.8)
               CALL DPWRST('XXX','BUG ')
            ELSE IF (IFIXB2(J).EQ.0) THEN
!              WRITE (LUNRPT,8600) J,BETA(J),LOWER(J),UPPER(J)
               WRITE (ICOUT,8600) J,BETA(J)
 8600          FORMAT(3X,I5,1X,1P,D16.8,6X,'    FIXED')
               CALL DPWRST('XXX','BUG ')
            ELSE
!              WRITE (LUNRPT,8700) J,BETA(J),LOWER(J),UPPER(J)
               WRITE (ICOUT,8700) J,BETA(J)
 8700          FORMAT(3X,I5,1X,1P,D16.8,6X,'  DROPPED')
               CALL DPWRST('XXX','BUG ')
            END IF
   10    CONTINUE
!        IF (.NOT.REDOJ) WRITE (LUNRPT,7310)
         IF (.NOT.REDOJ) THEN
            WRITE (ICOUT,7310)
 7310       FORMAT('     N.B. STANDARD ERRORS AND CONFIDENCE ',        &
                   'INTERVALS ARE COMPUTED USING')
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,7311)
 7311       FORMAT('          DERIVATIVES CALCULATED AT THE BEGINNING',&
                   ' OF THE LAST ITERATION,')
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,7312)
 7312       FORMAT('          AND NOT USING DERIVATIVES RE-EVALUATED ',&
                   'AT THE FINAL SOLUTION.')
            CALL DPWRST('XXX','BUG ')
         ENDIF
      ELSE
         IF (DOVCV) THEN
            IF (D1.LE.5) THEN
!              WRITE (LUNRPT,7410)
               WRITE (ICOUT,999)
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,7410)
 7410          FORMAT('     N.B. THE STANDARD ERRORS OF THE ',         &
                      'ESTIMATED BETAS WERE NOT COMPUTED BECAUSE')
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,7411)
 7411          FORMAT('          THE DERIVATIVES WERE NOT AVAILABLE.', &
                      '  EITHER MAXIT IS 0 AND THE THIRD')
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,7412)
 7412          FORMAT('          DIGIT OF JOB IS GREATER THAN 1, OR ', &
                      'THE MOST RECENTLY TRIED VALUES OF')
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,7413)
 7413          FORMAT('          BETA AND OR X+DELTA WERE IDENTIFIED', &
                      ' AS UNACCEPTABLE BY USER SUPPLIED')
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,7414)
 7414          FORMAT('          SUBROUTINE FCN.')
               CALL DPWRST('XXX','BUG ')
            ELSE
!              WRITE (LUNRPT,7420)
               WRITE (ICOUT,999)
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,7420)
 7420          FORMAT('     N.B. THE STANDARD ERRORS OF THE ',         &
                      'ESTIMATED BETAS WERE NOT COMPUTED.')
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,7421)
 7421          FORMAT('          (SEE INFO ABOVE.)')
               CALL DPWRST('XXX','BUG ')
            END IF
         END IF

         IF ((IRANK.EQ.0 .AND. NPP.EQ.NP) .OR.  NITER.EQ.0) THEN
            IF (NP.EQ.1) THEN
!              WRITE (LUNRPT,7100)
               WRITE (ICOUT,999)
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,7100)
 7100          FORMAT('           INDEX           VALUE')
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,999)
               CALL DPWRST('XXX','BUG ')
            ELSE
!              WRITE (LUNRPT,7200)
               WRITE (ICOUT,999)
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,7200)
 7200          FORMAT('           INDEX           VALUE -------------->')
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,999)
               CALL DPWRST('XXX','BUG ')
            END IF
            DO 20 J=1,NP,NPLM1+1
               K = MIN(J+NPLM1,NP)
               IF (K.EQ.J) THEN
!                 WRITE (LUNRPT,8100) J,BETA(J)
                  WRITE (ICOUT,8100) J,BETA(J)
 8100             FORMAT(11X,I5,1P,D16.8)
                  CALL DPWRST('XXX','BUG ')
               ELSE
!                 WRITE (LUNRPT,8200) J,K,(BETA(L),L=J,K)
                  WRITE (ICOUT,8200) J,K,(BETA(L),L=J,K)
 8200             FORMAT(3X,I5,' TO',I5,1P,7D16.8)
                  CALL DPWRST('XXX','BUG ')
               END IF
   20       CONTINUE
            IF (NITER.GE.1) THEN
!              WRITE (LUNRPT,8800)
               WRITE (ICOUT,999)
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,8800)
 8800          FORMAT('     N.B. NO PARAMETERS WERE FIXED BY THE ',    &
                      'USER OR DROPPED AT THE LAST')
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,8801)
 8801          FORMAT('          ITERATION BECAUSE THEY CAUSED THE ', &
                      'MODEL TO BE RANK DEFICIENT.')
               CALL DPWRST('XXX','BUG ')
            ELSE
!              WRITE (LUNRPT,8900)
               WRITE (ICOUT,999)
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,8900)
 8900          FORMAT('     N.B. NO CHANGE WAS MADE TO THE USER ',     &
                      'SUPPLIED PARAMETER VALUES BECAUSE')
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,8901)
 8901          FORMAT('          MAXIT=0.')
               CALL DPWRST('XXX','BUG ')
            END IF
         ELSE
!           WRITE (LUNRPT,7500)
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,7500)
 7500       FORMAT('                     BETA         STATUS')
            CALL DPWRST('XXX','BUG ')
            DO 30 J=1,NP
               IF (IFIXB2(J).GE.1) THEN
!                 WRITE (LUNRPT,8500) J,BETA(J),LOWER(J),UPPER(J)
                  WRITE (ICOUT,8500) J,BETA(J)
 8500             FORMAT(3X,I5,1X,1P,D16.8,6X,'ESTIMATED')
                  CALL DPWRST('XXX','BUG ')
               ELSE IF (IFIXB2(J).EQ.0) THEN
!                 WRITE (LUNRPT,8600) J,BETA(J),LOWER(J),UPPER(J)
                  WRITE (ICOUT,8600) J,BETA(J)
                  CALL DPWRST('XXX','BUG ')
               ELSE
!                 WRITE (LUNRPT,8700) J,BETA(J),LOWER(J),UPPER(J)
                  WRITE (ICOUT,8700) J,BETA(J)
                  CALL DPWRST('XXX','BUG ')
               END IF
   30       CONTINUE
         END IF
      END IF

      IF (IPRTMP.EQ.1) RETURN


!  Print EPSILON's and DELTA's together in a column if the number of
!  columns of data in EPSILON and DELTA is less than or equal to three.

      IF (IMPLCT .AND. (M.LE.4)) THEN
!        WRITE (LUNRPT,4100)
         WRITE (ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,4100)
 4100    FORMAT(' --- ESTIMATED DELTA(I,*), I = 1, ..., N:')
         CALL DPWRST('XXX','BUG ')
         WRITE (FMT1,9110) M
 9110    FORMAT('(''         I'',',I2,'(''      DELTA(I,'',I1,'')''))')
!        WRITE (LUNRPT,FMT1) (J,J=1,M)
         WRITE (ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,FMT1) (J,J=1,M)
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         DO 40 I=1,N
!           WRITE (LUNRPT,4130) I,(DELTA(I,J),J=1,M)
            WRITE (ICOUT,4130) I,(DELTA(I,J),J=1,M)
 4130       FORMAT(5X,I5,1P,5D16.8)
            CALL DPWRST('XXX','BUG ')
   40    CONTINUE

      ELSE IF (ISODR .AND. (NQ+M.LE.4)) THEN
!        WRITE (LUNRPT,4110)
         WRITE (ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,4110)
 4110    FORMAT(' --- ESTIMATED EPSILON(I) AND DELTA(I,*), ',          &
                'I = 1, ..., N:')
         CALL DPWRST('XXX','BUG ')
         WRITE (FMT1,9120) NQ,M
 9120    FORMAT('(''         I'',',                                    &
                I2,'(''    EPSILON(I,'',I1,'')''),',                   &
                I2,'(''      DELTA(I,'',I1,'')''))')
!        WRITE (LUNRPT,FMT1) (L,L=1,NQ),(J,J=1,M)
         WRITE (ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,FMT1) (L,L=1,NQ),(J,J=1,M)
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         DO 50 I=1,N
!           WRITE (LUNRPT,4130) I,(F(I,L),L=1,NQ),(DELTA(I,J),J=1,M)
            WRITE (ICOUT,4130) I,(F(I,L),L=1,NQ),(DELTA(I,J),J=1,M)
            CALL DPWRST('XXX','BUG ')
   50    CONTINUE

      ELSE IF (.NOT.ISODR .AND. ((NQ.GE.2) .AND. (NQ.LE.4))) THEN
!        WRITE (LUNRPT,4120)
         WRITE (ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,4120)
 4120    FORMAT(' --- ESTIMATED EPSILON(I), I = 1, ..., N:')
         CALL DPWRST('XXX','BUG ')
         WRITE (FMT1,9130) NQ
 9130    FORMAT('(''         I'',',I2,'(''    EPSILON(I,'',I1,'')''))')
!        WRITE (LUNRPT,FMT1) (L,L=1,NQ)
         WRITE (ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,FMT1) (L,L=1,NQ)
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         DO 60 I=1,N
!           WRITE (LUNRPT,4130) I,(F(I,L),L=1,NQ)
            WRITE (ICOUT,4130) I,(F(I,L),L=1,NQ)
            CALL DPWRST('XXX','BUG ')
   60    CONTINUE
      ELSE

!  Print EPSILON's and DELTA's separately

         IF (.NOT.IMPLCT) THEN

!  Print EPSILON'S

            DO 80 J=1,NQ
!              WRITE (LUNRPT,4200) J
               WRITE (ICOUT,4200) J
 4200          FORMAT(' --- ESTIMATED EPSILON(I,',I3,'), I = 1, ..., N:')
               CALL DPWRST('XXX','BUG ')
               IF (N.EQ.1) THEN
!                 WRITE (LUNRPT,7100)
                  WRITE (ICOUT,999)
                  CALL DPWRST('XXX','BUG ')
                  WRITE (ICOUT,7100)
                  CALL DPWRST('XXX','BUG ')
                  WRITE (ICOUT,999)
                  CALL DPWRST('XXX','BUG ')
               ELSE
!                 WRITE (LUNRPT,7200)
                  WRITE (ICOUT,999)
                  CALL DPWRST('XXX','BUG ')
                  WRITE (ICOUT,7200)
                  CALL DPWRST('XXX','BUG ')
                  WRITE (ICOUT,999)
                  CALL DPWRST('XXX','BUG ')
               END IF
               DO 70 I=1,N,NPLM1+1
                  K = MIN(I+NPLM1,N)
                  IF (I.EQ.K) THEN
!                    WRITE (LUNRPT,8100) I,F(I,J)
                     WRITE (ICOUT,8100) I,F(I,J)
                     CALL DPWRST('XXX','BUG ')
                  ELSE
!                    WRITE (LUNRPT,8200) I,K,(F(L,J),L=I,K)
                     WRITE (ICOUT,8200) I,K,(F(L,J),L=I,K)
                     CALL DPWRST('XXX','BUG ')
                  END IF
   70          CONTINUE
   80       CONTINUE
         END IF

!  Print DELTA'S

         IF (ISODR) THEN
            DO 100 J=1,M
!              WRITE (LUNRPT,4300) J
               WRITE (ICOUT,999)
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,4300) J
 4300          FORMAT(' --- ESTIMATED DELTA(I,',I3,'), I = 1, ..., N:')
               CALL DPWRST('XXX','BUG ')
               IF (N.EQ.1) THEN
!                 WRITE (LUNRPT,7100)
                  WRITE (ICOUT,999)
                  CALL DPWRST('XXX','BUG ')
                  WRITE (ICOUT,7100)
                  CALL DPWRST('XXX','BUG ')
                  WRITE (ICOUT,999)
                  CALL DPWRST('XXX','BUG ')
               ELSE
!                 WRITE (LUNRPT,7200)
                  WRITE (ICOUT,999)
                  CALL DPWRST('XXX','BUG ')
                  WRITE (ICOUT,7200)
                  CALL DPWRST('XXX','BUG ')
                  WRITE (ICOUT,999)
                  CALL DPWRST('XXX','BUG ')
               END IF
               DO 90 I=1,N,NPLM1+1
                  K = MIN(I+NPLM1,N)
                  IF (I.EQ.K) THEN
!                    WRITE (LUNRPT,8100) I,DELTA(I,J)
                     WRITE (ICOUT,8100) I,DELTA(I,J)
                     CALL DPWRST('XXX','BUG ')
                  ELSE
!                    WRITE (LUNRPT,8200) I,K,(DELTA(L,J),L=I,K)
                     WRITE (ICOUT,8200) I,K,(DELTA(L,J),L=I,K)
                     CALL DPWRST('XXX','BUG ')
                  END IF
   90          CONTINUE
  100       CONTINUE
         END IF
      END IF

      RETURN

!  Format statements

!1000 FORMAT
!    & (/' --- Stopping Conditions:')
!1011 FORMAT
!    &  ('         INFO = ',I5,' ==> sum of squares convergence.')
!1012 FORMAT
!    &  ('         INFO = ',I5,' ==> parameter convergence.')
!1013 FORMAT
!    &  ('         INFO = ',I5,' ==> sum of squares convergence and',
!    &                        ' parameter convergence.')
!1014 FORMAT
!    &  ('         INFO = ',I5,' ==> iteration limit reached.')
!1015 FORMAT
!    &  ('         INFO = ',I5,' ==> unexpected value,',
!    &                                 ' probably indicating'/
!    &   '                           incorrectly specified',
!    &                                 ' user input.')
!1020 FORMAT
!    &  ('         INFO = ',I5.4/
!    &   '              =  ABCD, where a nonzero value for digit A,',
!    &                         ' B, or C indicates why'/
!    &   '                       the results might be questionable,',
!    &                         ' and digit D indicates'/
!    &   '                       the actual stopping condition.')
!1021 FORMAT
!    &  ('                       A=1 ==> derivatives are',
!    &                                 ' questionable.')
!1022 FORMAT
!    &  ('                       B=1 ==> user set ISTOP to',
!    &                                 ' nonzero value during last'/
!    &   '                               call to subroutine FCN.')
!1023 FORMAT
!    &  ('                       C=1 ==> derivatives are not',
!    &                                 ' full rank at the solution.')
!1024 FORMAT
!    &  ('                       C=2 ==> derivatives are zero',
!    &                                 ' rank at the solution.')
!1031 FORMAT
!    &  ('                       D=1 ==> sum of squares convergence.')
!1032 FORMAT
!    &  ('                       D=2 ==> parameter convergence.')
!1033 FORMAT
!    &  ('                       D=3 ==> sum of squares convergence',
!    &                                 ' and parameter convergence.')
!1034 FORMAT
!    &  ('                       D=4 ==> iteration limit reached.')
!1035 FORMAT
!    &  ('                       D=',I1,' ==> unexpected value,',
!    &                                 ' probably indicating'/
!    &   '                               incorrectly specified',
!    &                                 ' user input.')
!1040 FORMAT
!    &  ('         INFO = ',I5.5/
!    &   '              = ABCDE, where a nonzero value for a given',
!    &                         ' digit indicates an'/
!    &   '                       abnormal stopping condition.')
!1042 FORMAT
!    &  ('                       A=5 ==> user stopped computations',
!    &                                 ' in subroutine FCN.')
!1043 FORMAT
!    &  ('                       B=',I1,' ==> computations were',
!    &                                 ' stopped during the'/
!    &   '                                    function evaluation.')
!1044 FORMAT
!    &  ('                       C=',I1,' ==> computations were',
!    &                                 ' stopped because'/
!    &   '                                    derivatives with',
!    &                                 ' respect to delta were'/
!    &   '                                    computed by',
!    &                                 ' subroutine FCN when'/
!    &   '                                    fit is OLS.')
!1045 FORMAT
!    &  ('                       C=',I1,' ==> computations were',
!    &                                 ' stopped during the'/
!    &   '                                    jacobian evaluation.')
!1050 FORMAT
!    &  ('                       A=6 ==> numerical instabilities',
!    &                                 ' have been detected,'/
!    &   '                               possibly indicating',
!    &                                 ' a discontinuity in the'/
!    &   '                               derivatives or a poor',
!    &                                 ' poor choice of problem'/
!    &   '                               scale or weights.')
!1060 FORMAT
!    &  ('                       A=',I1,' ==> unexpected value,',
!    &                                 ' probably indicating'/
!    &   '                               incorrectly specified',
!    &                                 ' user input.')
!1300 FORMAT
!    &  ('        NITER = ',I5,
!    &                    '          (number of iterations)')
!1310 FORMAT
!    &  ('         NFEV = ',I5,
!    &                    '          (number of function evaluations)')
!1320 FORMAT
!    &  ('         NJEV = ',I5,
!    &                    '          (number of jacobian evaluations)')
!1330 FORMAT
!    &  ('        IRANK = ',I5,
!    &                    '          (rank deficiency)')
!1340 FORMAT
!    &  ('        RCOND = ',1P,E12.2,
!    &                           '   (inverse condition number)')
!1341 FORMAT
!    +  ('                      ==> POSSIBLY FEWER THAN 2 SIGNIFICANT',
!    +                        ' DIGITS IN RESULTS;'/
!    +   '                          SEE ODRPACK95 REFERENCE',
!    +                        ' GUIDE, SECTION 4.C.')
!1350 FORMAT
!    &  ('        ISTOP = ',I5,
!    &                    '          (returned by user from',
!    &                        ' subroutine FCN)')
!2000 FORMAT
!    & (/' --- Final Sum of Squared Weighted Deltas = ',
!    &     17X,1P,E17.8)
!2010 FORMAT
!    & ( '         Final Penalty Function Value     = ',1P,E17.8/
!    &   '               Penalty Term               = ',1P,E17.8/
!    &   '               Penalty Parameter          = ',1P,E10.1)
!2100 FORMAT
!    & (/' --- Final Weighted Sums of Squares       = ',17X,1P,E17.8)
!2110 FORMAT
!    & ( '         Sum of Squared Weighted Deltas   = ',1P,E17.8/
!    &   '         Sum of Squared Weighted Epsilons = ',1P,E17.8)
!2200 FORMAT
!    & (/' --- Residual Standard Deviation          = ',
!    &     17X,1P,E17.8/
!    &   '         Degrees of Freedom               =',I5)
!3000 FORMAT
!    & (/' --- Estimated BETA(J), J = 1, ..., NP:')
!4100 FORMAT
!    & (/' --- Estimated DELTA(I,*), I = 1, ..., N:')
!4110 FORMAT
!    & (/' --- Estimated EPSILON(I) and DELTA(I,*), I = 1, ..., N:')
!4120 FORMAT
!    & (/' --- Estimated EPSILON(I), I = 1, ..., N:')
!4130 FORMAT(5X,I5,1P,5E16.8)
!4200 FORMAT
!    & (/' --- Estimated EPSILON(I,',I3,'), I = 1, ..., N:')
!4300 FORMAT
!    & (/' --- Estimated DELTA(I,',I3,'), I = 1, ..., N:')
!7100 FORMAT
!    & (/'           Index           Value'/)
!7200 FORMAT
!    & (/'           Index           Value -------------->'/)
!7300 FORMAT
!    & (/'                     BETA      LOWER     UPPER      S.D. ',
!    &   ' ___ 95% Confidence ___'/
!    &   '                                                    BETA ',
!    &   '        Interval'/)
!7310 FORMAT
!    & (/'     N.B. standard errors and confidence intervals are',
!    &                ' computed using'/
!    &   '          derivatives calculated at the beginning',
!    &                ' of the last iteration,'/
!    &   '          and not using derivatives re-evaluated at the',
!    &                ' final solution.')
!7410 FORMAT
!    & (/'     N.B. the standard errors of the estimated betas were',
!    &                ' not computed because'/
!    &   '          the derivatives were not available.  Either MAXIT',
!    &                ' is 0 and the third'/
!    &   '          digit of JOB is greater than 1, or the most',
!    &                ' recently tried values of'/
!    &   '          BETA and/or X+DELTA were identified as',
!    &                ' unacceptable by user supplied'/
!    &   '          subroutine FCN.')
!7420 FORMAT
!    & (/'     N.B. the standard errors of the estimated betas were',
!    &                ' not computed.'/
!    &   '          (see info above.)')
!7500 FORMAT
!    & (/'                     BETA         Status')
!8100 FORMAT
!    &  (11X,I5,1P,E16.8)
!8200 FORMAT
!    &  (3X,I5,' to',I5,1P,7E16.8)
!8400 FORMAT
!    &  (3X,I5,1X,1P,E16.8,1X,E10.2,E10.2,E10.2,1X,E10.2,1X,'to',E10.2)
!8500 FORMAT
!    &  (3X,I5,1X,1P,E16.8,1X,E10.2,E10.2,4X,'Estimated')
!8600 FORMAT
!    &  (3X,I5,1X,1P,E16.8,1X,E10.2,E10.2,4X,'    Fixed')
!8700 FORMAT
!    &  (3X,I5,1X,1P,E16.8,1X,E10.2,E10.2,4X,'  Dropped')
!8800 FORMAT
!    & (/'     N.B. no parameters were fixed by the user or',
!    &                ' dropped at the last'/
!    &   '          iteration because they caused the model to be',
!    &                ' rank deficient.')
!8900 FORMAT
!    & (/'     N.B. no change was made to the user supplied parameter',
!    &                ' values because'/
!    &   '          MAXIT=0.')
!9110 FORMAT
!    &  ('(/''         I'',',
!    &   I2,'(''      DELTA(I,'',I1,'')'')/)')
!9120 FORMAT
!    &  ('(/''         I'',',
!    &   I2,'(''    EPSILON(I,'',I1,'')''),',
!    &   I2,'(''      DELTA(I,'',I1,'')'')/)')
!9130 FORMAT
!    &  ('(/''         I'',',
!    &   I2,'(''    EPSILON(I,'',I1,'')'')/)')

      END SUBROUTINE
!DODPCR
      SUBROUTINE DODPCR(IPR,LUNRPT,HEAD,PRTPEN,FSTITR,DIDVCV,IFLAG,    &
                        N,M,NP,NQ,NPP,NNZW,                            &
                        MSGB,MSGD, BETA,Y,LDY,X,LDX,DELTA,             &
                        WE,LDWE,LD2WE,WD,LDWD,LD2WD,                   &
                        IFIXB,IFIXX,LDIFX,LOWER,UPPER,                 &
                        SSF,TT,LDTT,STPB,STPD,LDSTPD,                  &
                        JOB,NETA,TAUFAC,SSTOL,PARTOL,MAXIT,            &
                        WSS,RVAR,IDF,SDBETA,                           &
                        NITER,NFEV,NJEV,ACTRED,PRERED,                 &
                        TAU,PNORM,ALPHA,F,RCOND,IRANK,INFO,ISTOP)
!***Begin Prologue  DODPCR
!***Refer to  ODR
!***Routines Called  DFLAGS,DODPC1,DODPC2,DODPC3,DODPHD
!***Date Written   860529   (YYMMDD)
!***Revision Date  920619   (YYMMDD)
!***Purpose  Generate computation reports
!***End Prologue  DODPCR

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      REAL (KIND=R8) ACTRED,ALPHA,PARTOL,PNORM,PRERED,RCOND,RVAR,      &
                     SSTOL,TAU,TAUFAC
      INTEGER IDF,IFLAG,INFO,IPR,IRANK,ISTOP,JOB,LDIFX,LDSTPD,LDTT,    &
              LDWD,LDWE,LDX,LDY,LD2WD,LD2WE,LUNRPT,M,MAXIT,N,NETA,     &
              NFEV,NITER,NJEV,NNZW,NP,NPP,NQ
      LOGICAL DIDVCV,FSTITR,HEAD,PRTPEN

!...Array arguments
      REAL (KIND=R8) BETA(NP),DELTA(N,M),F(N,NQ),LOWER(NP),SDBETA(NP), &
                     SSF(NP),STPB(NP),STPD(LDSTPD,M),TT(LDTT,M),       &
                     UPPER(NP),WD(LDWD,LD2WD,M),WE(LDWE,LD2WE,NQ),     &
                     WSS(3),X(LDX,M),Y(LDY,NQ)
      INTEGER IFIXB(NP),IFIXX(LDIFX,M),MSGB(NQ*NP+1),MSGD(NQ*M+1)

!...Local scalars
      REAL (KIND=R8) PNLTY
      LOGICAL ANAJAC,CDJAC,CHKJAC,DOVCV,IMPLCT,INITD,ISODR,REDOJ,RESTRT
      CHARACTER TYP*3

!...External subroutines
      EXTERNAL DFLAGS,DODPC1,DODPC2,DODPC3,DODPHD

!...Variable Definitions (alphabetically)
!   ACTRED:  The actual relative reduction in the sum-of-squares.
!   ALPHA:   The Levenberg-Marquardt parameter.
!   ANAJAC:  The variable designating whether the Jacobians are computed
!            by finite differences (ANAJAC=FALSE) or not (ANAJAC=TRUE).
!   BETA:    The function parameters.
!   CDJAC:   The variable designating whether the jacobians are computed
!            by central differences (CDJAC=TRUE) or by forward
!            differences (CDJAC=FALSE).
!   CHKJAC:  The variable designating whether the user supplied 
!            Jacobians are to be checked (CHKJAC=TRUE) or not
!            (CHKJAC=FALSE).
!   DELTA:   The estimated errors in the explanatory variables.
!   DIDVCV:  The variable designating whether the covariance matrix was
!            computed (DIDVCV=TRUE) or not (DIDVCV=FALSE).
!   DOVCV:   The variable designating whether the covariance matrix is 
!            to be computed (DOVCV=TRUE) or not (DOVCV=FALSE).
!   F:       The (weighted) estimated values of EPSILON.
!   FSTITR:  The variable designating whether this is the first 
!            iteration (FSTITR=TRUE) or not (FSTITR=FALSE).
!   HEAD:    The variable designating whether the heading is to be 
!            printed (HEAD=TRUE) or not (HEAD=FALSE).
!   IDF:     The degrees of freedom of the fit, equal to the number of
!            observations with nonzero weighted derivatives minus the
!            number of parameters being estimated.
!   IFIXB:   The values designating whether the elements of BETA are 
!            fixed at their input values or not.
!   IFIXX:   The values designating whether the elements of X are 
!            fixed at their input values or not.
!   IFLAG:   The variable designating what is to be printed.
!   IMPLCT:  The variable designating whether the solution is by 
!            implicit ODR (IMPLCT=TRUE) or explicit ODR (IMPLCT=FALSE). 
!   INFO:    The variable designating why the computations were stopped.
!   INITD:   The variable designating whether DELTA is initialized to 
!            zero (INITD=TRUE) or to the values in the first N  by M
!            elements of array WORK (INITD=FALSE).
!   IPR:     The value indicating the report to be printed.
!   IRANK:   The rank deficiency of the Jacobian wrt BETA.
!   ISODR:   The variable designating whether the solution is by ODR 
!            (ISODR=TRUE) or by OLS (ISODR=FALSE).
!   ISTOP:   The variable designating whether there are problems
!            computing the function at the current BETA and DELTA.
!   JOB:     The variable controling problem initialization and 
!            computational method.
!   LDIFX:   The leading dimension of array IFIXX.
!   LDSTPD:  The leading dimension of array STPD.
!   LDTT:    The leading dimension of array TT.
!   LDWD:    The leading dimension of array WD.
!   LDWE:    The leading dimension of array WE.
!   LDX:     The leading dimension of array X.
!   LDY:     The leading dimension of array Y.
!   LD2WD:   The second dimension of array WD.
!   LD2WE:   The second dimension of array WE.
!   LOWER:   Lower bound on BETA.
!   LUNRPT:  The logical unit number for computation reports.
!   M:       The number of columns of data in the explanatory variable.
!   MAXIT:   The maximum number of iterations allowed. 
!   MSGB:    The error checking results for the Jacobian wrt BETA.
!   MSGD:    The error checking results for the Jacobian wrt DELTA.
!   N:       The number of observations.
!   NETA:    The number of accurate digits in the function results.
!   NFEV:    The number of function evaluations.
!   NITER:   The number of iterations.
!   NJEV:    The number of Jacobian evaluations.
!   NNZW:    The number of nonzero weighted observations.
!   NP:      The number of function parameters.
!   NQ:      The number of responses per observation.
!   NPP:     The number of function parameters being estimated.
!   PARTOL:  The parameter convergence stopping tolerance.
!   PNLTY:   The penalty parameter for an implicit model.
!   PNORM:   The norm of the scaled estimated parameters.
!   PRERED:  The predicted relative reduction in the sum-of-squares.
!   PRTPEN:  The variable designating whether the penalty parameter is
!            to be printed in the iteration report (PRTPEN=TRUE) or not
!            (PRTPEN=FALSE).
!   RCOND:   The approximate reciprocal condition number of TFJACB.
!   REDOJ:   The variable designating whether the Jacobian matrix is to
!            be recomputed for the computation of the covariance matrix
!            (REDOJ=TRUE) or not (REDOJ=FALSE).
!   RESTRT:  The variable designating whether the call is a restart  
!            (RESTRT=TRUE) OR NOT (RESTRT=FALSE).
!   RVAR:    The residual variance.
!   SDBETA:  The standard deviations of the estimated BETA'S.
!   SSF:     The scaling values for BETA.
!   SSTOL:   The sum-of-squares convergence stopping tolerance.
!   STPB:    The relative step for computing finite difference 
!            derivatives with respect to BETA.
!   STPD:    The relative step for computing finite difference
!            derivatives with respect to DELTA.
!   TAU:     The trust region diameter.
!   TAUFAC:  The factor used to compute the initial trust region 
!            diameter.
!   TT:      The scaling values for DELTA.
!   TYP:     The CHARACTER*3 string "ODR" or "OLS".
!   UPPER:   Upper bound on BETA.
!   WE:      The EPSILON weights.
!   WD:      The DELTA weights.
!   WSS:     The sum-of-squares of the weighted EPSILONS and DELTAS,
!            the sum-of-squares of the weighted DELTAS, and
!            the sum-of-squares of the weighted EPSILONS.
!   X:       The explanatory variable.
!   Y:       The dependent variable.  Unused when the model is implicit.


!***First executable statement  DODPCR


      CALL DFLAGS(JOB,RESTRT,INITD,DOVCV,REDOJ,                        &
                  ANAJAC,CDJAC,CHKJAC,ISODR,IMPLCT)
      PNLTY = ABS(WE(1,1,1))

      IF (HEAD) THEN
         CALL DODPHD(HEAD,LUNRPT)
      END IF
      IF (ISODR) THEN
         TYP = 'ODR'
      ELSE
         TYP = 'OLS'
      END IF

!  Print initial summary

      IF (IFLAG.EQ.1) THEN
!        WRITE (LUNRPT,1200) TYP
         WRITE (ICOUT,999)
  999    FORMAT(1X)
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,1200) TYP
 1200    FORMAT(' *** Initial summary for fit by method of ',A3, ' ***')
         CALL DPWRST('XXX','BUG ')
         CALL DODPC1(IPR,LUNRPT,ANAJAC,CDJAC,CHKJAC,INITD,             &
                     RESTRT,ISODR,IMPLCT,DOVCV,REDOJ,                  &
                     MSGB(1),MSGB(2),MSGD(1),MSGD(2),                  &
                     N,M,NP,NQ,NPP,NNZW,X,LDX,IFIXX,LDIFX,DELTA,       &
                     WD,LDWD,LD2WD,TT,LDTT,STPD,LDSTPD,                &
                     Y,LDY,WE,LDWE,LD2WE,PNLTY,                        &
                     BETA,IFIXB,SSF,STPB,LOWER,UPPER,                  &
                     JOB,NETA,TAUFAC,SSTOL,PARTOL,MAXIT,               &
                     WSS(1),WSS(2),WSS(3))

!  Print iteration reports

      ELSE IF (IFLAG.EQ.2) THEN

         IF (FSTITR) THEN
!           WRITE (LUNRPT,1300) TYP
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,1300) TYP
 1300       FORMAT(' *** Iteration reports for fit by method of ',A3,  &
                   ' ***')
            CALL DPWRST('XXX','BUG ')
         END IF
         CALL DODPC2(IPR,LUNRPT, FSTITR,IMPLCT,PRTPEN,PNLTY,NITER,     &
                     NFEV,WSS(1),ACTRED,PRERED,ALPHA,TAU,PNORM,NP,BETA)

!  Print final summary

      ELSE IF (IFLAG.EQ.3) THEN

!        WRITE (LUNRPT,1400) TYP
         WRITE (ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,1400) TYP
 1400    FORMAT(' *** Final summary for fit by method of ',A3, ' ***')
         CALL DPWRST('XXX','BUG ')
         CALL DODPC3(IPR,LUNRPT,ISODR,IMPLCT,DIDVCV,DOVCV,REDOJ,       &
                     ANAJAC,N,M,NP,NQ,NPP,                             &
                     INFO,NITER,NFEV,NJEV,IRANK,RCOND,ISTOP,           &
                     WSS(1),WSS(2),WSS(3),PNLTY,RVAR,IDF,              &
                     BETA,SDBETA,IFIXB,F,DELTA,LOWER,UPPER)
      END IF

      RETURN

      END SUBROUTINE
!DODPE1
      SUBROUTINE DODPE1(UNIT,INFO,D1,D2,D3,D4,D5,N,M,NQ,               &
                        LDSCLD,LDSTPD,LDWE,LD2WE,LDWD,LD2WD,LWKMN,LIWKMN)
!***Begin Prologue  DODPE1
!***Refer to  ODR
!***Routines Called  (NONE)
!***Date Written   860529   (YYMMDD)
!***Revision Date  920619   (YYMMDD)
!***Purpose  Print error reports
!***End Prologue  DODPE1

!...Scalar arguments
      INTEGER D1,D2,D3,D4,D5,INFO,LDSCLD,LDSTPD,LDWD,LDWE,LD2WD,LD2WE, &
              LIWKMN,LWKMN,M,N,NQ,UNIT

!...Variable Definitions (alphabetically)
!   D1:      The 1st digit (from the left) of INFO.
!   D2:      The 2nd digit (from the left) of INFO.
!   D3:      The 3rd digit (from the left) of INFO.
!   D4:      The 4th digit (from the left) of INFO.
!   D5:      The 5th digit (from the left) of INFO.
!   INFO:    The variable designating why the computations were stopped.
!   LDSCLD:  The leading dimension of array SCLD.
!   LDSTPD:  The leading dimension of array STPD.
!   LDWD:    The leading dimension of array WD.
!   LDWE:    The leading dimension of array WE.
!   LIWKMN:  The minimum acceptable length of array IWORK.
!   LWKMN:   The minimum acceptable length of array WORK.
!   LD2WD:   The second dimension of array WD.
!   LD2WE:   The second dimension of array WE.
!   M:       The number of columns of data in the explanatory variable.
!   N:       The number of observations.
!   NQ:      The number of responses per observation.
!   UNIT:    The logical unit number used for error messages.

!
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!

!***First executable statement  DODPE1

      IF(ISUBG4.EQ.'DPE1')THEN
        WRITE(ICOUT,52)UNIT
   52   FORMAT('UNIT = ',I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!

!  Print appropriate messages for errors in problem specification
!  parameters

      IF (D1.EQ.1) THEN
         IF (D2.NE.0) THEN
!           WRITE(UNIT,1100)
            WRITE (ICOUT,999)
  999       FORMAT(1X)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1100)
 1100       FORMAT(' ERROR :  N IS LESS THAN ONE.')
            CALL DPWRST('XXX','BUG ')
         END IF
         IF (D3.NE.0) THEN
!           WRITE(UNIT,1200)
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1200)
 1200       FORMAT(' ERROR :  M IS LESS THAN ONE.')
            CALL DPWRST('XXX','BUG ')
         END IF
         IF (D4.NE.0) THEN
!           WRITE(UNIT,1300)
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1300)
 1300       FORMAT(' ERROR :  NP IS LESS THAN ONE')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1301)
 1301       FORMAT('          OR NP IS GREATER THAN N.')
            CALL DPWRST('XXX','BUG ')
         END IF
         IF (D5.NE.0) THEN
!           WRITE(UNIT,1400)
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1400)
 1400       FORMAT(' ERROR :  NQ IS LESS THAN ONE.')
            CALL DPWRST('XXX','BUG ')
         END IF

!  Print appropriate messages for errors in dimension specification
!  parameters

      ELSE IF (D1.EQ.2) THEN

         IF (D2.NE.0) THEN
            IF (D2.EQ.1 .OR. D2.EQ.3) THEN
!              WRITE(UNIT,2110)
               WRITE (ICOUT,999)
               CALL DPWRST('XXX','BUG ')
               WRITE(ICOUT,2110)
 2110          FORMAT(' ERROR :  LDX IS LESS THAN N.')
               CALL DPWRST('XXX','BUG ')
            END IF
            IF (D2.EQ.2 .OR. D2.EQ.3) THEN
!              WRITE(UNIT,2120)
               WRITE (ICOUT,999)
               CALL DPWRST('XXX','BUG ')
               WRITE(ICOUT,2120)
 2120          FORMAT(' ERROR :  LDY IS LESS THAN N.')
               CALL DPWRST('XXX','BUG ')
            END IF
         END IF

         IF (D3.NE.0) THEN
            IF (D3.EQ.1 .OR. D3.EQ.3 .OR. D3.EQ.5 .OR. D3.EQ.7) THEN
!              WRITE(UNIT,2210)
               WRITE (ICOUT,999)
               CALL DPWRST('XXX','BUG ')
               WRITE(ICOUT,2210)
 2210          FORMAT(' ERROR :  LDIFX IS LESS THAN N')
               CALL DPWRST('XXX','BUG ')
               WRITE(ICOUT,2211)
 2211          FORMAT('          AND LDIFX IS NOT EQUAL TO ONE.')
               CALL DPWRST('XXX','BUG ')
            END IF
            IF (D3.EQ.2 .OR. D3.EQ.3 .OR. D3.EQ.6 .OR. D3.EQ.7) THEN
!              WRITE(UNIT,2220)
               WRITE (ICOUT,999)
               CALL DPWRST('XXX','BUG ')
               WRITE(ICOUT,2220)
 2220          FORMAT(' ERROR :  LDSCLD IS LESS THAN N')
               CALL DPWRST('XXX','BUG ')
               WRITE(ICOUT,2221)
 2221          FORMAT('          AND LDSCLD IS NOT EQUAL TO ONE.')
               CALL DPWRST('XXX','BUG ')
            END IF
            IF (D3.EQ.4 .OR. D3.EQ.5 .OR. D3.EQ.6 .OR. D3.EQ.7) THEN
!              WRITE(UNIT,2230)
               WRITE (ICOUT,999)
               CALL DPWRST('XXX','BUG ')
               WRITE(ICOUT,2230)
 2230          FORMAT(' ERROR :  LDSTPD IS LESS THAN N')
               CALL DPWRST('XXX','BUG ')
               WRITE(ICOUT,2231)
 2231          FORMAT('          AND LDSTPD IS NOT EQUAL TO ONE.')
               CALL DPWRST('XXX','BUG ')
            END IF
         END IF

         IF (D4.NE.0) THEN
            IF (D4.EQ.1 .OR. D4.EQ.3) THEN
!              WRITE(UNIT,2310)
               WRITE (ICOUT,999)
               CALL DPWRST('XXX','BUG ')
               WRITE(ICOUT,2310)
 2310          FORMAT(' ERROR :  LDWE IS LESS THAN N')
               CALL DPWRST('XXX','BUG ')
               WRITE(ICOUT,2311)
 2311          FORMAT('          AND LDWE IS NOT EQUAL TO ONE OR')
               CALL DPWRST('XXX','BUG ')
               WRITE(ICOUT,2312)
 2312          FORMAT('          OR')
               CALL DPWRST('XXX','BUG ')
               WRITE(ICOUT,2313)
 2313          FORMAT('          LD2WE IS LESS THAN NQ')
               CALL DPWRST('XXX','BUG ')
               WRITE(ICOUT,2314)
 2314          FORMAT('          AND LD2WE IS NOT EQUAL TO ONE.')
               CALL DPWRST('XXX','BUG ')
            END IF
            IF (D4.EQ.2 .OR. D4.EQ.3) THEN
 !             WRITE(UNIT,2320)
               WRITE (ICOUT,999)
               CALL DPWRST('XXX','BUG ')
               WRITE(ICOUT,2320)
 2320          FORMAT(' ERROR :  LDWD IS LESS THAN N')
               CALL DPWRST('XXX','BUG ')
               WRITE(ICOUT,2321)
 2321          FORMAT('          AND LDWD IS NOT EQUAL TO ONE.')
               CALL DPWRST('XXX','BUG ')
            END IF
         END IF

         IF (D5.NE.0) THEN
            IF (D5.EQ.1 .OR. D5.EQ.3) THEN
!              WRITE(UNIT,2410) LWKMN
               WRITE (ICOUT,999)
               CALL DPWRST('XXX','BUG ')
               WRITE(ICOUT,2410) LWKMN
 2410          FORMAT(' ERROR :  LWORK IS LESS THAN ',I7, ',')
               CALL DPWRST('XXX','BUG ')
               WRITE(ICOUT,2411)
 2411          FORMAT('          THE SMALLEST ACCEPTABLE DIMENSION ',  &
                      'OF ARRAY WORK.')
               CALL DPWRST('XXX','BUG ')
            END IF
            IF (D5.EQ.2 .OR. D5.EQ.3) THEN
!              WRITE(UNIT,2420) LIWKMN
               WRITE (ICOUT,999)
               CALL DPWRST('XXX','BUG ')
               WRITE(ICOUT,2420) LIWKMN
 2420          FORMAT(' ERROR :  LIWORK IS LESS THAN ',I7, ',')
               CALL DPWRST('XXX','BUG ')
               WRITE(ICOUT,2421)
 2421          FORMAT('          THE SMALLEST ACCEPTABLE DIMENSION ',  &
                      'OF ARRAY IWORK.')
               CALL DPWRST('XXX','BUG ')
            END IF
         END IF

      ELSE IF (D1.EQ.3) THEN

!  Print appropriate messages for errors in scale values

         IF (D3.NE.0) THEN
            IF (D3.EQ.2 .OR. D3.EQ.3) THEN
               IF (LDSCLD.GE.N) THEN
!                 WRITE(UNIT,3110)
                  WRITE (ICOUT,999)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,3110)
 3110             FORMAT(' ERROR :  SCLD(I,J) IS LESS THAN OR EQUAL ', &
                         'TO ZERO')
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,3111)
 3111             FORMAT('          FOR SOME I = 1, ..., N AND J = ',  &
                         '1, ..., M.')
                  CALL DPWRST('XXX','BUG ')
                  WRITE (ICOUT,999)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,3112)
 3112             FORMAT('          WHEN SCLD(1,1) IS GREATER THAN ZERO')
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,3113)
 3113             FORMAT('          AND LDSCLD IS GREATER THAN OR ',   &
                         'EQUAL TO N THEN')
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,3114)
 3114             FORMAT('          EACH OF THE N BY M ELEMENTS OF')
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,3115)
 3115             FORMAT('          SCLD MUST BE GREATER THAN ZERO.')
                  CALL DPWRST('XXX','BUG ')
               ELSE
!                 WRITE(UNIT,3120)
                  WRITE (ICOUT,999)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,3120)
 3120             FORMAT(' ERROR :  SCLD(1,J) IS LESS THAN OR EQUAL ', &
                         'TO ZERO')
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,3121)
 3121             FORMAT('          FOR SOME J = 1, ..., M.')
                  CALL DPWRST('XXX','BUG ')
                  WRITE (ICOUT,999)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,3122)
 3122             FORMAT('          WHEN SCLD(1,1) IS GREATER THAN ZERO')
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,3123)
 3123             FORMAT('          AND LDSCLD IS EQUAL TO ONE THEN')
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,3124)
 3124             FORMAT('          EACH OF THE 1 BY M ELEMENTS OF')
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,3125)
 3125             FORMAT('          SCLD MUST BE GREATER THAN ZERO.')
                  CALL DPWRST('XXX','BUG ')
               END IF
            END IF
            IF (D3.EQ.1 .OR. D3.EQ.3) THEN
!              WRITE(UNIT,3130)
               WRITE (ICOUT,999)
               CALL DPWRST('XXX','BUG ')
               WRITE(ICOUT,3130)
 3130          FORMAT(' ERROR :  SCLB(K) IS LESS THAN OR EQUAL ',   &
                      'TO ZERO')
               CALL DPWRST('XXX','BUG ')
               WRITE(ICOUT,3131)
 3131          FORMAT('          FOR SOME K = 1, ..., NP.')
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,999)
               CALL DPWRST('XXX','BUG ')
               WRITE(ICOUT,3132)
 3132          FORMAT('          ALL NP ELEMENTS OF',               &
                      ' SCLB MUST BE GREATER THAN ZERO.')
                  CALL DPWRST('XXX','BUG ')
            END IF
         END IF

!  Print appropriate messages for errors in derivative step values

         IF (D2.NE.0) THEN
            IF (D2.EQ.2 .OR. D2.EQ.3) THEN
               IF (LDSTPD.GE.N) THEN
!                 WRITE(UNIT,3210)
                  WRITE (ICOUT,999)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,3210)
 3210             FORMAT(' ERROR :  STPD(I,J) IS LESS THAN OR EQUAL ', &
                         'TO ZERO')
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,3211)
 3211             FORMAT('          FOR SOME I = 1, ..., N AND ',      &
                         'J = 1, ..., M.')
                  CALL DPWRST('XXX','BUG ')
                  WRITE (ICOUT,999)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,3212)
 3212             FORMAT('          WHEN STPD(1,1) IS GREATER THAN ZERO')
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,3213)
 3213             FORMAT('          AND LDSTPD IS GREATER THAN OR ',   &
                         'EQUAL TO N THEN')
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,3214)
 3214             FORMAT('          EACH OF THE N BY M ELEMENTS OF')
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,3215)
 3215             FORMAT('          STPD MUST BE GREATER THAN ZERO.')
                  CALL DPWRST('XXX','BUG ')
               ELSE
!                 WRITE(UNIT,3220)
                  WRITE (ICOUT,999)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,3220)
 3220             FORMAT(' ERROR :  STPD(1,J) IS LESS THAN OR EQUAL ', &
                         'TO ZERO')
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,3221)
 3221             FORMAT('          FOR SOME J = 1, ..., M.')
                  CALL DPWRST('XXX','BUG ')
                  WRITE (ICOUT,999)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,3222)
 3222             FORMAT('          WHEN STPD(1,1) IS GREATER THAN ZERO')
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,3223)
 3223             FORMAT('          AND LDSTPD IS EQUAL TO ONE THEN')
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,3224)
 3224             FORMAT('          EACH OF THE 1 BY M ELEMENTS OF')
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,3225)
 3225             FORMAT('          STPD MUST BE GREATER THAN ZERO.')
                  CALL DPWRST('XXX','BUG ')
               END IF
            END IF
            IF (D2.EQ.1 .OR. D2.EQ.3) THEN
!              WRITE(UNIT,3230)
               WRITE (ICOUT,999)
               CALL DPWRST('XXX','BUG ')
               WRITE(ICOUT,3230)
 3230          FORMAT(' ERROR :  STPB(K) IS LESS THAN OR EQUAL TO ZERO')
               CALL DPWRST('XXX','BUG ')
               WRITE(ICOUT,3231)
 3231          FORMAT('          FOR SOME K = 1, ..., NP.')
               CALL DPWRST('XXX','BUG ')
               WRITE (ICOUT,999)
               CALL DPWRST('XXX','BUG ')
               WRITE(ICOUT,3232)
 3232          FORMAT('          ALL NP ELEMENTS OF',                  &
                      ' STPB MUST BE GREATER THAN ZERO.')
               CALL DPWRST('XXX','BUG ')
            END IF
         END IF

!  Print appropriate messages for errors in observational error weights

         IF (D4.NE.0) THEN
            IF (D4.EQ.1) THEN
               IF (LDWE.GE.N) THEN
                  IF (LD2WE.GE.NQ) THEN
!                    WRITE(UNIT,3310)
                     WRITE (ICOUT,999)
                     CALL DPWRST('XXX','BUG ')
                     WRITE(ICOUT,3310)
 3310                FORMAT(' ERROR :  AT LEAST ONE OF THE (NQ BY ',   &
                            'NQ) ARRAYS STARTING')
                     CALL DPWRST('XXX','BUG ')
                     WRITE(ICOUT,3311)
 3311                FORMAT('          IN WE(I,1,1), I = 1, ..., N, ', &
                            'IS NOT POSITIVE')
                     CALL DPWRST('XXX','BUG ')
                     WRITE(ICOUT,3312)
 3312                FORMAT('          SEMIDEFINITE.  WHEN WE(1,1,1)', &
                            ' IS GREATER THAN')
                     CALL DPWRST('XXX','BUG ')
                     WRITE(ICOUT,3313)
 3313                FORMAT('          OR EQUAL TO ZERO, AND LDWE ',   &
                            'IS GREATER THAN OR')
                     CALL DPWRST('XXX','BUG ')
                     WRITE(ICOUT,3314)
 3314                FORMAT('          EQUAL TO N, AND LD2WE IS ',     &
                            'GREATER THAN OR EQUAL')
                     CALL DPWRST('XXX','BUG ')
                     WRITE(ICOUT,3315)
 3315                FORMAT('          TO NQ, THEN EACH OF THE (NQ ',  &
                            'BY NQ) ARRAYS IN WE')
                     CALL DPWRST('XXX','BUG ')
                     WRITE(ICOUT,3316)
 3316                FORMAT('          MUST BE POSITIVE SEMIDEFINITE.')
                     CALL DPWRST('XXX','BUG ')
                  ELSE
!                    WRITE(UNIT,3320)
                     WRITE (ICOUT,999)
                     CALL DPWRST('XXX','BUG ')
                     WRITE(ICOUT,3320)
 3320                FORMAT(' ERROR :  AT LEAST ONE OF THE (1 BY NQ)', &
                            ' ARRAYS STARTING')
                     CALL DPWRST('XXX','BUG ')
                     WRITE(ICOUT,3321)
 3321                FORMAT('          IN WE(I,1,1), I = 1, ..., N, ', &
                            'HAS A NEGATIVE')
                     CALL DPWRST('XXX','BUG ')
                     WRITE(ICOUT,3322)
 3322                FORMAT('          ELEMENT.  WHEN WE(1,1,1) IS ',  &
                            'GREATER THAN OR')
                     CALL DPWRST('XXX','BUG ')
                     WRITE(ICOUT,3323)
 3323                FORMAT('          EQUAL TO ZERO, AND LDWE IS ',   &
                            'GREATER THAN OR EQUAL')
                     CALL DPWRST('XXX','BUG ')
                     WRITE(ICOUT,3324)
 3324                FORMAT('          TO N, AND LD2WE IS EQUAL TO ',  &
                            '1, THEN EACH OF THE')
                     CALL DPWRST('XXX','BUG ')
                     WRITE(ICOUT,3325)
 3325                FORMAT('          (1 BY NQ) ARRAYS IN WE MUST ',  &
                            'HAVE ONLY NON-')
                     CALL DPWRST('XXX','BUG ')
                     WRITE(ICOUT,3326)
 3326                FORMAT('          NEGATIVE ELEMENTS.')
                     CALL DPWRST('XXX','BUG ')
                  END IF
               ELSE
                  IF (LD2WE.GE.NQ) THEN
!                    WRITE(UNIT,3410)
                     WRITE (ICOUT,999)
                     CALL DPWRST('XXX','BUG ')
                     WRITE(ICOUT,3410)
 3410                FORMAT(' ERROR :  THE (NQ BY NQ) ARRAY STARTING', &
                            ' IN WE(1,1,1) IS')
                     CALL DPWRST('XXX','BUG ')
                     WRITE(ICOUT,3411)
 3411                FORMAT('          NOT POSITIVE SEMIDEFINITE.  ',  &
                            'WHEN WE(1,1,1) IS')
                     CALL DPWRST('XXX','BUG ')
                     WRITE(ICOUT,3412)
 3412                FORMAT('          GREATER THAN OR EQUAL TO ',     &
                            'ZERO, AND LDWE IS EQUAL')
                     CALL DPWRST('XXX','BUG ')
                     WRITE(ICOUT,3413)
 3413                FORMAT('          TO 1, AND LD2WE IS GREATER ',   &
                            'THAN OR EQUAL TO NQ,')
                     CALL DPWRST('XXX','BUG ')
                     WRITE(ICOUT,3414)
 3414                FORMAT('          THEN THE (NQ BY NQ) ARRAY IN ', &
                            'WE MUST BE POSITIVE')
                     CALL DPWRST('XXX','BUG ')
                     WRITE(ICOUT,3415)
 3415                FORMAT('          SEMIDEFINITE.')
                     CALL DPWRST('XXX','BUG ')
                  ELSE
!                    WRITE(UNIT,3420)
                     WRITE (ICOUT,999)
                     CALL DPWRST('XXX','BUG ')
                     WRITE(ICOUT,3420)
 3420                FORMAT(' ERROR :  THE (1 BY NQ) ARRAY STARTING ', &
                            'IN WE(1,1,1) HAS')
                     CALL DPWRST('XXX','BUG ')
                     WRITE(ICOUT,3421)
 3421                FORMAT('          A NEGATIVE ELEMENT.  WHEN ',    &
                            'WE(1,1,1) IS GREATER')
                     CALL DPWRST('XXX','BUG ')
                     WRITE(ICOUT,3422)
 3422                FORMAT('          THAN OR EQUAL TO ZERO, AND ',   &
                            'LDWE IS EQUAL TO 1,')
                     CALL DPWRST('XXX','BUG ')
                     WRITE(ICOUT,3423)
 3423                FORMAT('          AND LD2WE IS EQUAL TO 1, ',     &
                            'THEN THE (1 BY NQ)')
                     CALL DPWRST('XXX','BUG ')
                     WRITE(ICOUT,3424)
 3424                FORMAT('          ARRAY IN WE MUST HAVE ONLY ',   &
                            'NONNEGATIVE ELEMENTS.')
                     CALL DPWRST('XXX','BUG ')
                  END IF
               END IF
            END IF
            IF (D4.EQ.2) THEN
!              WRITE(UNIT,3500)
               WRITE (ICOUT,999)
               CALL DPWRST('XXX','BUG ')
               WRITE(ICOUT,3500)
 3500          FORMAT(' ERROR :  THE NUMBER OF NONZERO ARRAYS IN ',    &
                      'ARRAY WE IS')
               CALL DPWRST('XXX','BUG ')
               WRITE(ICOUT,3501)
 3501          FORMAT('          LESS THAN NP.')
               CALL DPWRST('XXX','BUG ')
            END IF
         END IF

!  Print appropriate messages for errors in DELTA weights

         IF (D5.NE.0) THEN
            IF (LDWD.GE.N) THEN
               IF (LD2WD.GE.M) THEN
!                 WRITE(UNIT,4310)
                  WRITE (ICOUT,999)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,4310)
 4310             FORMAT(' ERROR :  AT LEAST ONE OF THE (M BY M) ',    &
                         'ARRAYS STARTING')
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,4311)
 4311             FORMAT('          IN WD(I,1,1), I = 1, ..., N, ',    &
                         'IS NOT POSITIVE')
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,4312)
 4312             FORMAT('          DEFINITE.  WHEN WD(1,1,1) IS ',    &
                         'GREATER THAN ZERO,')
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,4313)
 4313             FORMAT('          AND LDWD IS GREATER THAN OR ',     &
                         'EQUAL TO N, AND')
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,4314)
 4314             FORMAT('          LD2WD IS GREATER THAN OR EQUAL ',  &
                         'TO M, THEN EACH')
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,4315)
 4315             FORMAT('          OF THE (M BY M) ARRAYS IN WD ',    &
                         'MUST BE POSITIVE')
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,4316)
 4316             FORMAT('          DEFINITE.')
                  CALL DPWRST('XXX','BUG ')
               ELSE
 !                WRITE(UNIT,4320)
                  WRITE (ICOUT,999)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,4320)
 4320             FORMAT(' ERROR :  AT LEAST ONE OF THE (1 BY M) ',    &
                         'ARRAYS STARTING')
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,4321)
 4321             FORMAT('          IN WD(I,1,1), I = 1, ..., N, ',    &
                         'HAS A NONPOSITIVE')
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,4322)
 4322             FORMAT('          ELEMENT.  WHEN WD(1,1,1) IS ',     &
                         'GREATER THAN ZERO,')
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,4323)
 4323             FORMAT('          AND LDWD IS GREATER THAN OR ',     &
                         'EQUAL TO N, AND')
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,4324)
 4324             FORMAT('          LD2WD IS EQUAL TO 1, THEN EACH ',  &
                         'OF THE (1 BY M)')
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,4325)
 4325             FORMAT('          ARRAYS IN WD MUST HAVE ONLY ',     &
                         'POSITIVE ELEMENTS.')
                  CALL DPWRST('XXX','BUG ')
               END IF
            ELSE
               IF (LD2WD.GE.M) THEN
!                 WRITE(UNIT,4410)
                  WRITE (ICOUT,999)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,4410)
 4410             FORMAT(' ERROR :  THE (M BY M) ARRAY STARTING IN ',  &
                         'WD(1,1,1) IS')
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,4411)
 4411             FORMAT('          NOT POSITIVE DEFINITE.  WHEN ',    &
                         'WD(1,1,1) IS')
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,4412)
 4412             FORMAT('          GREATER THAN ZERO, AND LDWD IS ',  &
                         'EQUAL TO 1, AND')
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,4413)
 4413             FORMAT('          LD2WD IS GREATER THAN OR EQUAL ',  &
                         'TO M, THEN THE')
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,4414)
 4414             FORMAT('          (M BY M) ARRAY IN WD MUST BE ',    &
                         'POSITIVE DEFINITE.')
                  CALL DPWRST('XXX','BUG ')
               ELSE
!                 WRITE(UNIT,4420)
                  WRITE (ICOUT,999)
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,4420)
 4420             FORMAT(' ERROR :  THE (1 BY M) ARRAY STARTING IN ',  &
                         'WD(1,1,1) HAS A')
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,4421)
 4421             FORMAT('          NONPOSITIVE ELEMENT.  WHEN ',      &
                         'WD(1,1,1) IS GREATER')
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,4422)
 4422             FORMAT('          THAN ZERO, AND LDWD IS EQUAL TO ', &
                         '1, AND LD2WD IS')
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,4423)
 4423             FORMAT('          EQUAL TO 1, THEN THE (1 BY M) ',   &
                         'ARRAY IN WD MUST')
                  CALL DPWRST('XXX','BUG ')
                  WRITE(ICOUT,4424)
 4424             FORMAT('          HAVE ONLY POSITIVE ELEMENTS.')
                  CALL DPWRST('XXX','BUG ')
               END IF
            END IF
         END IF

      ELSE IF (D1.EQ.7) THEN

!  Print the appropriate messages for errors in JOB

         IF (D2.NE.0) THEN
!           WRITE(UNIT,5000)
!5000       FORMAT
!    &      (/' ERROR :  JOB requires the optional argument DELTA and'/
!    &       '          DELTA is not present or not associated.')
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5000)
 5000       FORMAT(' ERROR :  JOB requires the optional argument ',    &
                   'DELTA and')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5001)
 5001       FORMAT('          DELTA is not present or not associated.')
            CALL DPWRST('XXX','BUG ')
         END IF

         IF (D3.NE.0) THEN
!           WRITE(UNIT,5100)
!5100       FORMAT
!    &      (/' ERROR :  JOB requires the optional argument WORK and'/
!    &       '          WORK is not present or not associated.')
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5100)
 5100       FORMAT(' ERROR :  JOB requires the optional argument ',    &
                   'WORK and')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5101)
 5101       FORMAT('          WORK is not present or not associated.')
            CALL DPWRST('XXX','BUG ')
         END IF
  
         IF (D4.NE.0) THEN
!           WRITE(UNIT,5200)
!5200       FORMAT
!    &      (/' ERROR :  JOB requires the optional argument IWORK and'/
!    &       '          IWORK is not present or not associated.')
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5200)
 5200       FORMAT(' ERROR :  JOB requires the optional argument ',    &
                   'IWORK and')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5201)
 5201       FORMAT('          IWORK is not present or not associated.')
            CALL DPWRST('XXX','BUG ')
         END IF
  
      ELSE IF (D1.EQ.8) THEN

!  Print the appropriate messages for errors in array allocation

         IF (D2.NE.0) THEN
!           WRITE(UNIT,7200)
!7200 FORMAT
!    &   (/' ERROR :  DELTA could not be allocated. ')
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,7200)
 7200       FORMAT(' ERROR :  DELTA could not be allocated.')
            CALL DPWRST('XXX','BUG ')
         END IF

         IF (D3.NE.0) THEN
!           WRITE(UNIT,7300)
!7300 FORMAT
!    &   (/' ERROR :  WORK could not be allocated. ')
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,7300)
 7300       FORMAT(' ERROR :  WORK could not be allocated. ')
            CALL DPWRST('XXX','BUG ')
         END IF

         IF (D4.NE.0) THEN
!           WRITE(UNIT,7400)
!7400 FORMAT
!    &   (/' ERROR :  IWORK could not be allocated. ')
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,7400)
 7400       FORMAT(' ERROR :  IWORK could not be allocated. ')
            CALL DPWRST('XXX','BUG ')
         END IF

      ELSE IF (D1.EQ.9) THEN

!  Print the appropriate messages for errors in bounds

         IF (D2.NE.0) THEN
!           WRITE(UNIT,6000)
!6000 FORMAT
!    &   (/' ERROR :  LOWER(K).GT.UPPER(K) for some K.  Adjust the'/
!    &     '          the bounds so that LOWER(K).LE.UPPER(K) holds'/
!    &     '          for all K.')
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,6000)
 6000       FORMAT(' ERROR :  LOWER(K).GT.UPPER(K) for some K.  ',     &
                   'Adjust the')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,6001)
 6001       FORMAT('          the bounds so that ',                    &
                   'LOWER(K).LE.UPPER(K) holds')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,6002)
 6002       FORMAT('          for all K.')
            CALL DPWRST('XXX','BUG ')
         END IF

         IF (D3.NE.0) THEN
!           WRITE(UNIT,6100)
!6100 FORMAT
!    &   (/' ERROR :  BETA(K).GT.UPPER(K) or BETA(K).LT.LOWER(K) '/
!    &     '          for some K.  Adjust the bounds or BETA so '/
!    &     '          that LOWER(K).LE.BETA(K).LE.UPPER(K) holds'/
!    &     '          for all K.')
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,6100)
 6100       FORMAT(' ERROR :  BETA(K).GT.UPPER(K) or ',                &
                   'BETA(K).LT.LOWER(K) ')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,6101)
 6101       FORMAT('          for some K.  Adjust the bounds or ',     &
                   'BETA so ')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,6102)
 6102       FORMAT('          that LOWER(K).LE.BETA(K).LE.UPPER(K) ',  &
                   'holds')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,6103)
 6103       FORMAT('          for all K.')
            CALL DPWRST('XXX','BUG ')
         END IF

         IF (D4.EQ.1) THEN
!           WRITE(UNIT,6210)
!6210 FORMAT
!    &   (/' ERROR :  UPPER(K)-LOWER(K) .LT. 400*BETA(K)*EPSMAC  '/
!    &     '          for some K and EPSMAC having the largest '/
!    &     '          value such that 1+EPSMAC.NE.1.  This '/
!    &     '          constraint on UPPER and LOWER is necessary'/
!    &     '          for the calculation of NDIGIT.  Increase the'/
!    &     '          range of the bounds or specify NDIGIT '/
!    &     '          explicitly.')
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,6210)
 6210       FORMAT(' ERROR :  UPPER(K)-LOWER(K) .LT. 400*BETA(K)*EPSMAC')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,6211)
 6211       FORMAT('          for some K and EPSMAC having the largest ')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,6212)
 6212       FORMAT('          value such that 1+EPSMAC.NE.1.  This ')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,6213)
 6213       FORMAT('          constraint on UPPER and LOWER is ',      &
                   'necessary')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,6214)
 6214       FORMAT('          for the calculation of NDIGIT.  ',       &
                   'Increase the')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,6215)
 6215       FORMAT('          range of the bounds or specify NDIGIT ')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,6216)
 6216       FORMAT('          explicitly.')
            CALL DPWRST('XXX','BUG ')
         END IF

         IF (D4.EQ.2) THEN
!           WRITE(UNIT,6220)
!6220 FORMAT
!    &   (/' ERROR :  UPPER(K)-LOWER(K) .LT. ABS(STEP) for some'/
!    &     '          K where step is the step size for numeric'/
!    &     '          derivatives.  Increase the bounds or supply'/
!    &     '          an analytic jacobian.')
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,6220)
 6220       FORMAT(' ERROR :  UPPER(K)-LOWER(K) .LT. ABS(STEP) for ',  &
                   'some')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,6221)
 6221       FORMAT('          K where step is the step size for numeric')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,6222)
 6222       FORMAT('          derivatives.  Increase the bounds or ',  &
                   'supply')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,6223)
 6223       FORMAT('          an analytic jacobian.')
            CALL DPWRST('XXX','BUG ')
         END IF

      END IF

!  Print error messages for array sizes incorrect
 
      IF (INFO/100000.EQ.1) THEN
         INFO = INFO - 100000
         IF (INFO.GE.32768) THEN
            INFO = INFO - 32768
!           WRITE(UNIT,8015)
!8015 FORMAT
!    &   (/' ERROR :  LOWER has incorrect size. ')
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,8015)
 8015       FORMAT(' ERROR :  LOWER has incorrect size. ')
            CALL DPWRST('XXX','BUG ')
         END IF
         IF (INFO.GE.16384) THEN
            INFO = INFO - 16384 
!           WRITE(UNIT,8014)
!8014 FORMAT
!    &   (/' ERROR :  UPPER has incorrect size. ')
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,8014)
 8014       FORMAT(' ERROR :  UPPER has incorrect size. ')
            CALL DPWRST('XXX','BUG ')
         END IF
         IF (INFO.GE.8192) THEN
            INFO = INFO - 8192 
!           WRITE(UNIT,8013)
!8013 FORMAT
!    &   (/' ERROR :  IWORK has incorrect size. ')
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,8013)
 8013       FORMAT(' ERROR :  IWORK has incorrect size. ')
            CALL DPWRST('XXX','BUG ')
         END IF
         IF (INFO.GE.4096) THEN
            INFO = INFO - 4096 
!           WRITE(UNIT,8012)
!8012 FORMAT
!    &   (/' ERROR :  WORK has incorrect size. ')
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,8012)
 8012       FORMAT(' ERROR :  WORK has incorrect size. ')
            CALL DPWRST('XXX','BUG ')
         END IF
         IF (INFO.GE.2048) THEN
            INFO = INFO - 2048 
!           WRITE(UNIT,8011)
!8011 FORMAT
!    &   (/' ERROR :  SCLD has incorrect size. ')
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,8011)
 8011       FORMAT(' ERROR :  SCLD has incorrect size. ')
            CALL DPWRST('XXX','BUG ')
         END IF
         IF (INFO.GE.1024) THEN
            INFO = INFO - 1024 
!           WRITE(UNIT,8010)
!8010 FORMAT
!    &   (/' ERROR :  SCLB has incorrect size. ')
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,8010)
 8010       FORMAT(' ERROR :  SCLB has incorrect size. ')
            CALL DPWRST('XXX','BUG ')
         END IF
         IF (INFO.GE.512) THEN
            INFO = INFO - 512 
!           WRITE(UNIT,8009)
!8009 FORMAT
!    &   (/' ERROR :  STPD has incorrect size. ')
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,8009)
 8009       FORMAT(' ERROR :  STPD has incorrect size. ')
            CALL DPWRST('XXX','BUG ')
         END IF
         IF (INFO.GE.256) THEN
            INFO = INFO - 256 
!           WRITE(UNIT,8008)
!8008 FORMAT
!    &   (/' ERROR :  STPB has incorrect size. ')
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,8008)
 8008       FORMAT(' ERROR :  STPB has incorrect size. ')
            CALL DPWRST('XXX','BUG ')
         END IF
         IF (INFO.GE.128) THEN
            INFO = INFO - 128 
!           WRITE(UNIT,8007)
!8007 FORMAT
!    &   (/' ERROR :  IFIXX has incorrect size. ')
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,8007)
 8007       FORMAT(' ERROR :  IFIXX has incorrect size. ')
            CALL DPWRST('XXX','BUG ')
         END IF
         IF (INFO.GE.64) THEN
            INFO = INFO - 64 
!           WRITE(UNIT,8006)
!8006 FORMAT
!    &   (/' ERROR :  IFIXB has incorrect size. ')
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,8006)
 8006       FORMAT(' ERROR :  IFIXB has incorrect size. ')
            CALL DPWRST('XXX','BUG ')
         END IF
         IF (INFO.GE.32) THEN
            INFO = INFO - 32 
!           WRITE(UNIT,8005)
!8005 FORMAT
!    &   (/' ERROR :  WD has incorrect size. ')
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,8005)
 8005       FORMAT(' ERROR :  WD has incorrect size. ')
            CALL DPWRST('XXX','BUG ')
         END IF
         IF (INFO.GE.16) THEN
            INFO = INFO - 16 
!           WRITE(UNIT,8004)
!8004 FORMAT
!    &   (/' ERROR :  WE has incorrect size. ')
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,8004)
 8004       FORMAT(' ERROR :  WE has incorrect size. ')
            CALL DPWRST('XXX','BUG ')
         END IF
         IF (INFO.GE.8) THEN
            INFO = INFO - 8 
!           WRITE(UNIT,8003)
!8003 FORMAT
!    &   (/' ERROR :  DELTA has incorrect size. ')
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,8003)
 8003       FORMAT(' ERROR :  DELTA has incorrect size. ')
            CALL DPWRST('XXX','BUG ')
         END IF
         IF (INFO.GE.4) THEN
            INFO = INFO - 4 
!           WRITE(UNIT,8002)
!8002 FORMAT
!    &   (/' ERROR :  X has incorrect size. ')
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,8002)
 8002       FORMAT(' ERROR :  X has incorrect size. ')
            CALL DPWRST('XXX','BUG ')
         END IF
         IF (INFO.GE.2) THEN
            INFO = INFO - 2 
!           WRITE(UNIT,8001)
!8001 FORMAT
!    &   (/' ERROR :  Y has incorrect size. ')
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,8001)
 8001       FORMAT(' ERROR :  Y has incorrect size. ')
            CALL DPWRST('XXX','BUG ')
         END IF
         IF (INFO.GE.1) THEN
            INFO = INFO - 1 
!           WRITE(UNIT,8000)
!8000 FORMAT
!    &   (/' ERROR :  BETA has incorrect size. ')
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,8000)
 8000       FORMAT(' ERROR :  BETA has incorrect size. ')
            CALL DPWRST('XXX','BUG ')
         END IF
      END IF

!  Format statements

!1100 FORMAT
!    &   (/' ERROR :  N is less than one.')
!1200 FORMAT
!    &   (/' ERROR :  M is less than one.')
!1300 FORMAT
!    &   (/' ERROR :  NP is less than one'/
!    &     '          or NP is greater than N.')
!1400 FORMAT
!    &   (/' ERROR :  NQ is less than one.')
!2110 FORMAT
!    &   (/' ERROR :  LDX is less than N.')
!2120 FORMAT
!    &   (/' ERROR :  LDY is less than N.')
!2210 FORMAT
!    &   (/' ERROR :  LDIFX is less than N'/
!    &     '          and LDIFX is not equal to one.')
!2220 FORMAT
!    &   (/' ERROR :  LDSCLD is less than N'/
!    &     '          and LDSCLD is not equal to one.')
!2230 FORMAT
!    &   (/' ERROR :  LDSTPD is less than N'/
!    &     '          and LDSTPD is not equal to one.')
!2310 FORMAT
!    &   (/' ERROR :  LDWE is less than N'/
!    &     '          and LDWE is not equal to one or'/
!    &     '          or'/
!    &     '          LD2WE is less than NQ'/
!    &     '          and LD2WE is not equal to one.')
!2320 FORMAT
!    &   (/' ERROR :  LDWD is less than N'/
!    &     '          and LDWD is not equal to one.')
!2410 FORMAT
!    &   (/' ERROR :  LWORK is less than ',I7, ','/
!    &     '          the smallest acceptable dimension of array WORK.')
!2420 FORMAT
!    &   (/' ERROR :  LIWORK is less than ',I7, ','/
!    &     '          the smallest acceptable dimension of array',
!    &              ' IWORK.')
!3110 FORMAT
!    &   (/' ERROR :  SCLD(I,J) is less than or equal to zero'/
!    &     '          for some I = 1, ..., N and J = 1, ..., M.'//
!    &     '          when SCLD(1,1) is greater than zero'/
!    &     '          and LDSCLD is greater than or equal to N then'/
!    &     '          each of the N by M elements of'/
!    &     '          SCLD must be greater than zero.')
!3120 FORMAT
!    &   (/' ERROR :  SCLD(1,J) is less than or equal to zero'/
!    &     '          for some J = 1, ..., M.'//
!    &     '          when SCLD(1,1) is greater than zero'/
!    &     '          and LDSCLD is equal to one then'/
!    &     '          each of the 1 by M elements of'/
!    &     '          SCLD must be greater than zero.')
!3130 FORMAT
!    &   (/' ERROR :  SCLB(K) is less than or equal to zero'/
!    &     '          for some K = 1, ..., NP.'//
!    &     '          all NP elements of',
!    &     '          SCLB must be greater than zero.')
!3210 FORMAT
!    &   (/' ERROR :  STPD(I,J) is less than or equal to zero'/
!    &     '          for some I = 1, ..., N and J = 1, ..., M.'//
!    &     '          when STPD(1,1) is greater than zero'/
!    &     '          and LDSTPD is greater than or equal to N then'/
!    &     '          each of the N by M elements of'/
!    &     '          STPD must be greater than zero.')
!3220 FORMAT
!    &   (/' ERROR :  STPD(1,J) is less than or equal to zero'/
!    &     '          for some J = 1, ..., M.'//
!    &     '          when STPD(1,1) is greater than zero'/
!    &     '          and LDSTPD is equal to one then'/
!    &     '          each of the 1 by M elements of'/
!    &     '          STPD must be greater than zero.')
!3230 FORMAT
!    &   (/' ERROR :  STPB(K) is less than or equal to zero'/
!    &     '          for some K = 1, ..., NP.'//
!    &     '          all NP elements of',
!    &              ' STPB must be greater than zero.')
!3310 FORMAT
!    &   (/' ERROR :  At least one of the (NQ by NQ) arrays starting'/
!    &     '          in WE(I,1,1), I = 1, ..., N, is not positive'/
!    &     '          semidefinite.  When WE(1,1,1) is greater than'/
!    &     '          or equal to zero, and LDWE is greater than or'/
!    &     '          equal to N, and LD2WE is greater than or equal'/
!    &     '          to NQ, then each of the (NQ by NQ) arrays in WE'/
!    &     '          must be positive semidefinite.')
!3320 FORMAT
!    &   (/' ERROR :  At least one of the (1 by NQ) arrays starting'/
!    &     '          in WE(I,1,1), I = 1, ..., N, has a negative'/
!    &     '          element.  When WE(1,1,1) is greater than or'/
!    &     '          equal to zero, and LDWE is greater than or equal'/
!    &     '          to N, and LD2WE is equal to 1, then each of the'/
!    &     '          (1 by NQ) arrays in WE must have only non-'/
!    &     '          negative elements.')
!3410 FORMAT
!    &   (/' ERROR :  The (NQ by NQ) array starting in WE(1,1,1) is'/
!    &     '          not positive semidefinite.  When WE(1,1,1) is'/
!    &     '          greater than or equal to zero, and LDWE is equal'/
!    &     '          to 1, and LD2WE is greater than or equal to NQ,'/
!    &     '          then the (NQ by NQ) array in WE must be positive'/
!    &     '          semidefinite.')
!3420 FORMAT
!    &   (/' ERROR :  The (1 by NQ) array starting in WE(1,1,1) has'/
!    &     '          a negative element.  When WE(1,1,1) is greater'/
!    &     '          than or equal to zero, and LDWE is equal to 1,'/
!    &     '          and LD2WE is equal to 1, then the (1 by NQ)'/
!    &     '          array in WE must have only nonnegative elements.')
!3500 FORMAT
!    &   (/' ERROR :  The number of nonzero arrays in array WE is'/
!    &     '          less than NP.')
!4310 FORMAT
!    &   (/' ERROR :  At least one of the (M by M) arrays starting'/
!    &     '          in WD(I,1,1), I = 1, ..., N, is not positive'/
!    &     '          definite.  When WD(1,1,1) is greater than zero,'/
!    &     '          and LDWD is greater than or equal to N, and'/
!    &     '          LD2WD is greater than or equal to M, then each'/
!    &     '          of the (M by M) arrays in WD must be positive'/
!    &     '          definite.')
!4320 FORMAT
!    &   (/' ERROR :  At least one of the (1 by M) arrays starting'/
!    &     '          in WD(I,1,1), I = 1, ..., N, has a nonpositive'/
!    &     '          element.  When WD(1,1,1) is greater than zero,'/
!    &     '          and LDWD is greater than or equal to N, and'/
!    &     '          LD2WD is equal to 1, then each of the (1 by M)'/
!    &     '          arrays in WD must have only positive elements.')
!4410 FORMAT
!    &   (/' ERROR :  The (M by M) array starting in WD(1,1,1) is'/
!    &     '          not positive definite.  When WD(1,1,1) is'/
!    &     '          greater than zero, and LDWD is equal to 1, and'/
!    &     '          LD2WD is greater than or equal to M, then the'/
!    &     '          (M by M) array in WD must be positive definite.')
!4420 FORMAT
!    &   (/' ERROR :  The (1 by M) array starting in WD(1,1,1) has a'/
!    &     '          nonpositive element.  When WD(1,1,1) is greater'/
!    &     '          than zero, and LDWD is equal to 1, and LD2WD is'/
!    &     '          equal to 1, then the (1 by M) array in WD must'/
!    &     '          have only positive elements.')
      END SUBROUTINE
!DODPE2
      SUBROUTINE DODPE2(UNIT,N,M,NP,NQ,FJACB,FJACD,                    &
                        DIFF,MSGB1,MSGB,ISODR,MSGD1,MSGD,              &
                        XPLUSD,NROW,NETA,NTOL)
!***Begin Prologue  DODPE2
!***Refer to  ODR
!***Routines Called  (NONE)
!***Date Written   860529   (YYMMDD)
!***Revision Date  920619   (YYMMDD)
!***Purpose  Generate the derivative checking report
!***End Prologue  DODPE2

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      INTEGER M,MSGB1,MSGD1,N,NETA,NP,NQ,NROW,NTOL,UNIT
      LOGICAL ISODR

!...Array arguments
      REAL (KIND=R8) DIFF(NQ,NP+M),FJACB(N,NP,NQ),FJACD(N,M,NQ),       &
                     XPLUSD(N,M)
      INTEGER MSGB(NQ,NP),MSGD(NQ,M)

!...Local scalars
      INTEGER I,J,K,L
      CHARACTER FLAG*1,TYP*3

!...Local arrays
      LOGICAL FTNOTE(0:9)

!...Variable Definitions (alphabetically)
!   DIFF:    The relative differences between the user supplied and
!            finite difference derivatives for each derivative checked.
!   FJACB:   The Jacobian with respect to BETA.
!   FJACD:   The Jacobian with respect to DELTA.
!   FLAG:    The character string indicating highly questionable results.
!   FTNOTE:  The array controling footnotes.
!   I:       An index variable.
!   ISODR:   The variable designating whether the solution is by ODR 
!            (ISODR=.TRUE.) or by OLS (ISODR=.FALSE.).
!   J:       An index variable.
!   K:       An index variable.
!   L:       An index variable.
!   M:       The number of columns of data in the explanatory variable.
!   MSGB:    The error checking results for the Jacobian wrt BETA.
!   MSGB1:   The error checking results for the Jacobian wrt BETA.
!   MSGD:    The error checking results for the Jacobian wrt DELTA.
!   MSGD1:   The error checking results for the Jacobian wrt DELTA.
!   N:       The number of observations.
!   NETA:    The number of reliable digits in the model.
!   NP:      The number of function parameters.
!   NQ:      The number of responses per observation.
!   NROW:    The row number of the explanatory variable array at
!            which the derivative is to be checked.
!   NTOL:    The number of digits of agreement required between the
!            finite difference and the user supplied derivatives.
!   TYP:     The character string indicating solution type, ODR or OLS.
!   UNIT:    The logical unit number used for error messages.
!   XPLUSD:  The values of X + DELTA.


!***First executable statement  DODPE2


!  Set up for footnotes

      DO 10 I=0,9
         FTNOTE(I) = .FALSE.
   10 CONTINUE

      DO 40 L=1,NQ
         IF (MSGB1.GE.1) THEN
            DO 20 I=1,NP
               IF (MSGB(L,I).GE.1) THEN
                  FTNOTE(0) = .TRUE.
                  FTNOTE(MSGB(L,I)) = .TRUE.
               END IF
   20       CONTINUE
         END IF

         IF (MSGD1.GE.1) THEN
            DO 30 I=1,M
               IF (MSGD(L,I).GE.1) THEN
                  FTNOTE(0) = .TRUE.
                  FTNOTE(MSGD(L,I)) = .TRUE.
               END IF
   30       CONTINUE
         END IF
   40 CONTINUE

!     Print report 

      IF (ISODR) THEN
         TYP = 'ODR'
      ELSE
         TYP = 'OLS'
      END IF
!     WRITE (UNIT,1000) TYP
      WRITE (ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE (ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE (ICOUT,1000) TYP
 1000 FORMAT(' *** DERIVATIVE CHECKING REPORT FOR FIT BY METHOD OF ', &
             A3,' ***')
      CALL DPWRST('XXX','BUG ')
      WRITE (ICOUT,999)
      CALL DPWRST('XXX','BUG ')

      DO 70 L=1,NQ

!        WRITE (UNIT,2100) L,NROW
!        WRITE (UNIT,2200)
         WRITE (ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,2100) L,NROW
 2100    FORMAT ('     FOR RESPONSE ',I2,' OF OBSERVATION ', I5)
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,2200)
 2200    FORMAT('                      ','         USER',              &
                '               ','                ')
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,2201)
 2201    FORMAT('                      ','     SUPPLIED',              &
                '     RELATIVE','    DERIVATIVE ')
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,2202)
 2202    FORMAT('        DERIVATIVE WRT','        VALUE',              &
                '   DIFFERENCE','    ASSESSMENT '/)
         CALL DPWRST('XXX','BUG ')

         DO 50 I=1,NP
            K = MSGB(L,I)
            IF (K.EQ.7) THEN
               FLAG = '*'
            ELSE
               FLAG = ' '
            END IF
            IF (K.LE.-1) THEN
!              WRITE (UNIT,3100) I
               WRITE (ICOUT,3100) I
 3100          FORMAT ('             BETA(',I3,')', '       ---   ',   &
                       '       ---   ','    UNCHECKED')
               CALL DPWRST('XXX','BUG ')
            ELSE IF (K.EQ.0) THEN
!              WRITE (UNIT,3200) I,FJACB(NROW,I,L),DIFF(L,I),FLAG
               WRITE (ICOUT,3200) I,FJACB(NROW,I,L),DIFF(L,I),FLAG
 3200          FORMAT ('             BETA(',I3,')', 1P,2D13.2,3X,A1,   &
                       'VERIFIED')
               CALL DPWRST('XXX','BUG ')
            ELSE IF (K.EQ.8) THEN
!              WRITE (UNIT,3400) I,FJACB(NROW,I,L),FLAG,K
               WRITE (ICOUT,3400)I,FJACB(NROW,I,L),FLAG,K
 3400          FORMAT ('             BETA(',I3,')', 1P,1E13.2,13X,     &
                       3X,A1,'Questionable (see note ',I1,')')
               CALL DPWRST('XXX','BUG ')
            ELSE IF (K.EQ.9) THEN
!              WRITE (UNIT,3500) I,FLAG,K
               WRITE (ICOUT,3500)I,FLAG,K
 3500          FORMAT ('             BETA(',I3,')', 1P,13X,13X,3X,A1,  &
                       'Small bounds (see note ',I1,')')
               CALL DPWRST('XXX','BUG ')
            ELSE IF (K.GE.1) THEN
!              WRITE (UNIT,3300) I,FJACB(NROW,I,L),DIFF(L,I),FLAG,K
               WRITE (ICOUT,3300) I,FJACB(NROW,I,L),DIFF(L,I),FLAG,K
 3300          FORMAT ('             BETA(',I3,')', 1P,2D13.2,3X,A1,   &
                       'QUESTIONABLE (SEE NOTE ',I1,')')
               CALL DPWRST('XXX','BUG ')
            END IF
   50    CONTINUE
         IF (ISODR) THEN
            DO 60 I=1,M
               K = MSGD(L,I)
               IF (K.EQ.7) THEN
                  FLAG = '*'
               ELSE
                  FLAG = ' '
               END IF
               IF (K.LE.-1) THEN
!                 WRITE (UNIT,4100) NROW,I
                  WRITE (ICOUT,4100) NROW,I
 4100             FORMAT ('          DELTA(',I2,',',I2,')',            &
                          '       ---          ---   ','    UNCHECKED')
                  CALL DPWRST('XXX','BUG ')
               ELSE IF (K.EQ.0) THEN
!                 WRITE (UNIT,4200) NROW,I, 
!    &                              FJACD(NROW,I,L),DIFF(L,NP+I),FLAG
                  WRITE (ICOUT,4200) NROW,I,                           &
                                     FJACD(NROW,I,L),DIFF(L,NP+I),FLAG
 4200             FORMAT ('          DELTA(',I2,',',I2,')', 1P,2D13.2, &
                          3X,A1,'VERIFIED')
                  CALL DPWRST('XXX','BUG ')
               ELSE IF (K.GE.1) THEN
!                 WRITE (UNIT,4300) NROW,I, 
!    &                              FJACD(NROW,I,L),DIFF(L,NP+I),FLAG,K
                  WRITE (ICOUT,4300) NROW,I,                           &
                                     FJACD(NROW,I,L),DIFF(L,NP+I),FLAG,K
 4300             FORMAT ('          DELTA(',I2,',',I2,')', 1P,2D13.2, &
                          3X,A1,'QUESTIONABLE (SEE NOTE ',I1,')')
                  CALL DPWRST('XXX','BUG ')
               END IF
   60       CONTINUE
         END IF
   70 CONTINUE

!     Print footnotes

      IF (FTNOTE(0)) THEN

!        WRITE (UNIT,5000)
!        IF (FTNOTE(1)) WRITE (UNIT,5100)
!        IF (FTNOTE(2)) WRITE (UNIT,5200)
!        IF (FTNOTE(3)) WRITE (UNIT,5300)
!        IF (FTNOTE(4)) WRITE (UNIT,5400)
!        IF (FTNOTE(5)) WRITE (UNIT,5500)
!        IF (FTNOTE(6)) WRITE (UNIT,5600)
!        IF (FTNOTE(7)) WRITE (UNIT,5700)
!        IF (FTNOTE(8)) WRITE (UNIT,5800)
!        IF (FTNOTE(9)) WRITE (UNIT,5900)
         WRITE (ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,5000)
 5000    FORMAT('     NOTES:')
         CALL DPWRST('XXX','BUG ')
         IF (FTNOTE(1)) THEN
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,5100)
 5100       FORMAT('      (1) USER SUPPLIED AND FINITE DIFFERENCE ',   &
                   'DERIVATIVES AGREE, BUT')
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,5101)
 5101       FORMAT('          RESULTS ARE QUESTIONABLE BECAUSE BOTH ', &
                   'ARE ZERO.')
            CALL DPWRST('XXX','BUG ')
         END IF
         IF (FTNOTE(2)) THEN
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,5200)
 5200       FORMAT('      (2) USER SUPPLIED AND FINITE DIFFERENCE ',   &
                   'DERIVATIVES AGREE, BUT')
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,5201)
 5201       FORMAT('          RESULTS ARE QUESTIONABLE BECAUSE ONE ',&
                   'IS IDENTICALLY ZERO')
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,5202)
 5202       FORMAT('          AND THE OTHER IS ONLY APPROXIMATELY ZERO.')
            CALL DPWRST('XXX','BUG ')
         END IF
         IF (FTNOTE(3)) THEN
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,5300)
 5300       FORMAT('      (3) USER SUPPLIED AND FINITE DIFFERENCE ',   &
                   'DERIVATIVES DISAGREE, BUT')
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,5301)
 5301       FORMAT('          RESULTS ARE QUESTIONABLE BECAUSE ONE ',  &
                   'IS IDENTICALLY ZERO')
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,5302)
 5302       FORMAT('          AND THE OTHER IS NOT.')
            CALL DPWRST('XXX','BUG ')
         END IF
         IF (FTNOTE(4)) THEN
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,5400)
 5400       FORMAT('      (4) USER SUPPLIED AND FINITE DIFFERENCE ',   &
                   'DERIVATIVES DISAGREE, BUT')
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,5401)
 5401       FORMAT('          FINITE DIFFERENCE DERIVATIVE IS ',       &
                   'QUESTIONABLE BECAUSE EITHER')
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,5402)
 5402       FORMAT('          THE RATIO OF RELATIVE CURVATURE TO ',    &
                   'RELATIVE SLOPE IS TOO HIGH')
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,5403)
 5403       FORMAT('          OR THE SCALE IS WRONG.')
            CALL DPWRST('XXX','BUG ')
         END IF
         IF (FTNOTE(5)) THEN
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,5500)
 5500       FORMAT('      (5) USER SUPPLIED AND FINITE DIFFERENCE ',   &
                   'DERIVATIVES DISAGREE, BUT')
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,5501)
 5501       FORMAT('          FINITE DIFFERENCE DERIVATIVE IS ',       &
                   'QUESTIONABLE BECAUSE THE')
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,5502)
 5502       FORMAT('          RATIO OF RELATIVE CURVATURE TO ',        &
                   'RELATIVE SLOPE IS TOO HIGH.')
            CALL DPWRST('XXX','BUG ')
         END IF
         IF (FTNOTE(6)) THEN
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,5600)
 5600       FORMAT('      (6) USER SUPPLIED AND FINITE DIFFERENCE ',   &
                   'DERIVATIVES')
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,5601)
 5601       FORMAT('           DISAGREE, BUT',                         &
           '          HAVE AT LEAST 2 DIGITS IN COMMON.')
            CALL DPWRST('XXX','BUG ')
         END IF
         IF (FTNOTE(7)) THEN
            WRITE (ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,5700)
 5700       FORMAT('      (7) USER SUPPLIED AND FINITE DIFFERENCE ',   &
                   'DERIVATIVES DISAGREE, AND')
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,5701)
 5701       FORMAT('          HAVE FEWER THAN 2 DIGITS IN COMMON.  ',  &
                   'DERIVATIVE CHECKING MUST')
            CALL DPWRST('XXX','BUG ')
            WRITE (ICOUT,5702)
 5702       FORMAT('          BE TURNED OFF IN ORDER TO PROCEED.')
            CALL DPWRST('XXX','BUG ')
         END IF
      END IF

      IF (NETA.LT.0) THEN
!        WRITE (UNIT,6000) -NETA
         WRITE (ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,6000) -NETA
 6000    FORMAT('     NUMBER OF RELIABLE DIGITS IN FUNCTION ',         &
                'RESULTS       ',I5)
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,6001)
 6001    FORMAT('        (ESTIMATED BY ODRPACK)')
         CALL DPWRST('XXX','BUG ')
      ELSE
!        WRITE (UNIT,6100) NETA
         WRITE (ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,6100) NETA
 6100    FORMAT('     NUMBER OF RELIABLE DIGITS IN FUNCTION ',         &
                'RESULTS       ',I5)
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,6101)
 6101    FORMAT('        (SUPPLIED BY USER)')
         CALL DPWRST('XXX','BUG ')
      END IF
!     WRITE (UNIT,7000) NTOL
      WRITE (ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE (ICOUT,7000)
 7000 FORMAT('     NUMBER OF DIGITS OF AGREEMENT REQUIRED BETWEEN      ')
      CALL DPWRST('XXX','BUG ')
      WRITE (ICOUT,7001)
 7001 FORMAT('     USER SUPPLIED AND FINITE DIFFERENCE DERIVATIVE FOR  ')
      CALL DPWRST('XXX','BUG ')
      WRITE (ICOUT,7002) NTOL
 7002 FORMAT('     USER SUPPLIED DERIVATIVE TO BE CONSIDERED ',        &
             'VERIFIED  ',I5)
      CALL DPWRST('XXX','BUG ')

!  Print out row of explanatory variable which was checked.

 !    WRITE (UNIT,8100) NROW
      WRITE (ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE (ICOUT,8100) NROW
 8100 FORMAT('     ROW NUMBER AT WHICH DERIVATIVES WERE ',             &
             'CHECKED        ',I5)
      CALL DPWRST('XXX','BUG ')
      WRITE (ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE (ICOUT,8101)
 8101 FORMAT('       -VALUES OF THE EXPLANATORY VARIABLES AT THIS ROW')
      CALL DPWRST('XXX','BUG ')
      WRITE (ICOUT,999)
      CALL DPWRST('XXX','BUG ')

      DO 80 J=1,M
!        WRITE (UNIT,8110) NROW,J,XPLUSD(NROW,J)
         WRITE (ICOUT,8110) NROW,J,XPLUSD(NROW,J)
 8110    FORMAT(10X,'X(',I2,',',I2,')',1X,1P,3D16.8)
         CALL DPWRST('XXX','BUG ')
   80 CONTINUE

      RETURN

!     Format statements

!1000 FORMAT
!    &   (//' *** Derivative checking report for fit by method of ',A3,
!    &     ' ***'/)
!2100 FORMAT (/'     For response ',I2,' of observation ', I5/)
!2200 FORMAT ('                      ','         User',
!    &           '               ','                '/
!    &        '                      ','     Supplied',
!    &           '     Relative','    Derivative '/
!    &        '        Derivative WRT','        Value',
!    &           '   Difference','    Assessment '/)
!3100 FORMAT ('             BETA(',I3,')', '       ---   ',
!    &            '       ---   ','    Unchecked')
!3200 FORMAT ('             BETA(',I3,')', 1P,2E13.2,3X,A1,
!    &           'Verified')
!3300 FORMAT ('             BETA(',I3,')', 1P,2E13.2,3X,A1,
!    &           'Questionable (see note ',I1,')')
!4100 FORMAT ('          DELTA(',I2,',',I2,')', '       ---   ',
!    &            '       ---   ','    Unchecked')
!4200 FORMAT ('          DELTA(',I2,',',I2,')', 1P,2E13.2,3X,A1,
!    &           'Verified')
!4300 FORMAT ('          DELTA(',I2,',',I2,')', 1P,2E13.2,3X,A1,
!    &           'Questionable (see note ',I1,')')
!5000 FORMAT
!    &   (/'     NOTES:')
!5100 FORMAT
!    &   (/'      (1) User supplied and finite difference derivatives',
!    &                   ' agree, but'/
!    &     '          results are questionable because both are zero.')
!5200 FORMAT
!    &   (/'      (2) User supplied and finite difference derivatives',
!    &                   ' agree, but'/
!    &     '          results are questionable because one is',
!    &                   ' identically zero'/
!    &     '          and the other is only approximately zero.')
!5300 FORMAT
!    &   (/'      (3) User supplied and finite difference derivatives',
!    &                   ' disagree, but'/
!    &     '          results are questionable because one is',
!    &                   ' identically zero'/
!    &     '          and the other is not.')
!5400 FORMAT
!    &   (/'      (4) User supplied and finite difference derivatives',
!    &                   ' disagree, but'/
!    &     '          finite difference derivative is questionable',
!    &                   ' because either'/
!    &     '          the ratio of relative curvature to relative',
!    &                   ' slope is too high'/
!    &     '          or the scale is wrong.')
!5500 FORMAT
!    &   (/'      (5) User supplied and finite difference derivatives',
!    &                   ' disagree, but'/
!    &     '          finite difference derivative is questionable',
!    &                   ' because the'/
!    &     '          ratio of relative curvature to relative slope is',
!    &                   ' too high.')
!5600 FORMAT
!    &   (/'      (6) User supplied and finite difference derivatives',
!    &                   ' disagree, but'/
!    &     '          have at least 2 digits in common.')
!5700 FORMAT
!    &   (/'      (7) User supplied and finite difference derivatives',
!    &                   ' disagree, and'/
!    &     '          have fewer than 2 digits in common.  derivative',
!    &                   ' checking must'/
!    &     '          be turned off in order to proceed.')
!5800 FORMAT
!    &   (/'      (8) User supplied and finite difference derivatives',
!    &                   ' disagree, and'/
!    &     '          bound constraints are too small to calculate',
!    &                   ' further'/
!    &     '          information.')
!5900 FORMAT
!    &   (/'      (9) Bound constraints too small to check derivative.')
!6000 FORMAT
!    &   (/'     Number of reliable digits in function results       ',
!    &        I5/
!    &     '        (estimated by ODRPACK95)')
!6100 FORMAT
!    &   (/'     Number of reliable digits in function results       ',
!    &        I5/
!    &     '        (supplied by user)')
!7000 FORMAT
!    &   (/'     Number of digits of agreement required between      '/
!    &     '     user supplied and finite difference derivative for  '/
!    &     '     user supplied derivative to be considered verified  ',
!    &        I5)
!8100 FORMAT
!    &   (/'     Row number at which derivatives were checked        ',
!    &        I5//
!    &     '       -values of the explanatory variables at this row'/)
!8110 FORMAT
!    &   (10X,'X(',I2,',',I2,')',1X,1P,3E16.8)
      END SUBROUTINE
!DODPE3
      SUBROUTINE DODPE3(UNIT,D2,D3)
!***Begin Prologue  DODPE3
!***Refer to  ODR
!***Routines Called  (NONE)
!***Date Written   860529   (YYMMDD)
!***Revision Date  920619   (YYMMDD)
!***Purpose  Print error reports indicating that computations were
!            stopped in user supplied subroutines FCN
!***End Prologue  DODPE3

!...Scalar arguments
      INTEGER D2,D3,UNIT

!...Variable Definitions (alphabetically)
!   D2:      The 2nd digit (from the left) of INFO.
!   D3:      The 3rd digit (from the left) of INFO.
!   UNIT:    The logical unit number used for error messages.

!
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!

!***First executable statement  DODPE3


!  Print appropriate messages to indicate where computations were
!  stopped

      IF (D2.EQ.2) THEN
!        WRITE(UNIT,1100)
         WRITE (ICOUT,999)
  999    FORMAT(1X)
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1100)
 1100    FORMAT(' VARIABLE ISTOP HAS BEEN RETURNED WITH A NONZERO VALUE')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1101)
 1101    FORMAT(' FROM USER SUPPLIED SUBROUTINE FCN WHEN INVOKED ',    &
                'USING THE')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1102)
 1102    FORMAT(' INITIAL ESTIMATES OF BETA AND DELTA SUPPLIED BY THE')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1103)
 1103    FORMAT(' USER.  THE INITIAL ESTIMATES MUST BE ADJUSTED TO ',  &
                'ALLOW')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1104)
 1104    FORMAT(' PROPER EVALUATION OF SUBROUTINE FCN BEFORE THE')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1105)
 1105    FORMAT(' REGRESSION PROCEDURE CAN CONTINUE.')
         CALL DPWRST('XXX','BUG ')
      ELSE IF (D2.EQ.3) THEN
!        WRITE(UNIT,1200)
         WRITE (ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1200)
 1200    FORMAT(' VARIABLE ISTOP HAS BEEN RETURNED WITH A NONZERO VALUE')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1201)
 1201    FORMAT(' FROM USER SUPPLIED SUBROUTINE FCN.  THIS OCCURRED ', &
                'DURING')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1202)
 1202    FORMAT(' THE COMPUTATION OF THE NUMBER OF RELIABLE DIGITS ',  &
                'IN THE ')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1203)
 1203    FORMAT(' PREDICTED VALUES (F) RETURNED FROM SUBROUTINE FCN, ',&
                'INDI-')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1204)
 1204    FORMAT(' CATING THAT CHANGES IN THE INITIAL ESTIMATES OF ',   &
                'BETA(K),')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1205)
 1205    FORMAT(' K=1,NP, AS SMALL AS 2*BETA(K)*SQRT(MACHINE PRECISION),')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1206)
 1206    FORMAT(' WHERE MACHINE PRECISION IS DEFINED AS THE ',         &
                'SMALLEST VALUE')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1207)
 1207    FORMAT(' E SUCH THAT 1+E>1 ON THE COMPUTER BEING USED, PREVENT')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1208)
 1208    FORMAT(' SUBROUTINE FCN FROM BEING PROPERLY EVALUATED.  THE')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1209)
 1209    FORMAT(' INITIAL ESTIMATES MUST BE ADJUSTED TO ALLOW PROPER')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1210)
 1210    FORMAT(' EVALUATION OF SUBROUTINE FCN DURING THESE ',         &
                'COMPUTATIONS  ')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1211)
 1211    FORMAT(' BEFORE THE REGRESSION PROCEDURE CAN CONTINUE.')
         CALL DPWRST('XXX','BUG ')
      ELSE IF (D2.EQ.4) THEN
!        WRITE(UNIT,1300)
         WRITE (ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1300)
 1300    FORMAT(' VARIABLE ISTOP HAS BEEN RETURNED WITH A NONZERO ',   &
                'VALUE')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1301)
 1301    FORMAT(' FROM USER SUPPLIED SUBROUTINE FCN.  THIS OCCURRED ', &
                'DURING')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1302)
 1302    FORMAT(' THE DERIVATIVE CHECKING PROCEDURE, INDICATING THAT')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1303)
 1303    FORMAT(' CHANGES IN THE INITIAL ESTIMATES OF BETA(K), ',      &
                'K=1,NP, AS ')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1304)
 1304    FORMAT(' SMALL AS MAX[BETA(K),1/SCLB(K)]*10**(-NETA/2), ',    &
                'AND/OR')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1305)
 1305    FORMAT(' OF DELTA(I,J), I=1,N AND J=1,M, AS SMALL AS')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1306)
 1306    FORMAT(' MAX[DELTA(I,J),1/SCLD(I,J)]*10**(-NETA/2), WHERE ',  &
                'NETA')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1307)
 1307    FORMAT(' IS DEFINED TO BE THE NUMBER OF RELIABLE DIGITS IN')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1308)
 1308    FORMAT(' PREDICTED VALUES (F) RETURNED FROM SUBROUTINE FCN,')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1309)
 1309    FORMAT(' PREVENT SUBROUTINE FCN FROM BEING PROPERLY EVALUATED.')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1310)
 1310    FORMAT(' THE INITIAL ESTIMATES MUST BE ADJUSTED TO ALLOW ',   &
                'PROPER')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1311)
 1311    FORMAT(' EVALUATION OF SUBROUTINE FCN DURING THESE ',         &
                'COMPUTATIONS  ')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1312)
 1312    FORMAT(' BEFORE THE REGRESSION PROCEDURE CAN CONTINUE.')
         CALL DPWRST('XXX','BUG ')
      END IF
      IF (D3.EQ.2) THEN
!        WRITE(UNIT,1400)
         WRITE (ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1400)
 1400    FORMAT(' VARIABLE ISTOP HAS BEEN RETURNED WITH A NONZERO ', &
                'VALUE')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1401)
 1401    FORMAT(' FROM USER SUPPLIED SUBROUTINE FCN WHEN INVOKED FOR')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1402)
 1402    FORMAT(' DERIVATIVE EVALUATIONS USING THE INITIAL ESTIMATES OF')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1403)
 1403    FORMAT(' BETA AND DELTA SUPPLIED BY THE USER.  THE INITIAL')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1404)
 1404    FORMAT(' ESTIMATES MUST BE ADJUSTED TO ALLOW PROPER EVALUATION')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1405)
 1405    FORMAT(' OF SUBROUTINE FCN BEFORE THE REGRESSION PROCEDURE CAN')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1406)
 1406    FORMAT(' CONTINUE.')
         CALL DPWRST('XXX','BUG ')
      END IF

!  Format statements

!1100 FORMAT
!    &   (//' Variable ISTOP has been returned with a nonzero value  '/
!    &      ' from user supplied subroutine FCN when invoked using the'/
!    &      ' initial estimates of BETA and DELTA supplied by the     '/
!    &      ' user.  The initial estimates must be adjusted to allow  '/
!    &      ' proper evaluation of subroutine FCN before the          '/
!    &      ' regression procedure can continue.')
!1200 FORMAT
!    &   (//' Variable ISTOP has been returned with a nonzero value  '/
!    &      ' from user supplied subroutine FCN.  This occurred during'/
!    &      ' the computation of the number of reliable digits in the '/
!    &      ' predicted values (F) returned from subroutine FCN, indi-'/
!    &      ' cating that changes in the initial estimates of BETA(K),'/
!    &      ' K=1,NP, as small as 2*BETA(K)*SQRT(MACHINE PRECISION),  '/
!    &      ' where MACHINE PRECISION is defined as the smallest value'/
!    &      ' E such that 1+E>1 on the computer being used, prevent   '/
!    &      ' subroutine FCN from being properly evaluated.  The      '/
!    &      ' initial estimates must be adjusted to allow proper      '/
!    &      ' evaluation of subroutine FCN during these computations  '/
!    &      ' before the regression procedure can continue.')
!1300 FORMAT
!    &   (//' Variable ISTOP has been returned with a nonzero value  '/
!    &      ' from user supplied subroutine FCN.  This occurred during'/
!    &      ' the derivative checking procedure, indicating that      '/
!    &      ' changes in the initial estimates of BETA(K), K=1,NP, as '/
!    &      ' small as MAX[BETA(K),1/SCLB(K)]*10**(-NETA/2), and/or   '/
!    &      ' of DELTA(I,J), I=1,N and J=1,M, as small as             '/
!    &      ' MAX[DELTA(I,J),1/SCLD(I,J)]*10**(-NETA/2), where NETA   '/
!    &      ' is defined to be the number of reliable digits in       '/
!    &      ' predicted values (F) returned from subroutine FCN,      '/
!    &      ' prevent subroutine FCN from being properly evaluated.   '/
!    &      ' the initial estimates must be adjusted to allow proper  '/
!    &      ' evaluation of subroutine FCN during these computations  '/
!    &      ' before the regression procedure can continue.')
!1400 FORMAT
!    &   (//' Variable ISTOP has been returned with a nonzero value  '/
!    &      ' from user supplied subroutine FCN when invoked for '/
!    &      ' derivative evaluations using the initial estimates of '/
!    &      ' BETA and DELTA supplied by the user.  The initial '/
!    &      ' estimates must be adjusted to allow proper evaluation '/
!    &      ' of subroutine FCN before the regression procedure can '/
!    &      ' continue.')
      END SUBROUTINE
!DODPER
      SUBROUTINE DODPER(INFO,LUNERR,N,M,NP,NQ,                         &
                        LDSCLD,LDSTPD,LDWE,LD2WE,LDWD,LD2WD,           &
                        LWKMN,LIWKMN,FJACB,FJACD,                      &
                        DIFF,MSGB,ISODR,MSGD,XPLUSD,NROW,NETA,NTOL)
!***Begin Prologue  DODPER
!***Refer to  ODR
!***Routines Called  DODPE1,DODPE2,DODPE3,DODPHD
!***Date Written   860529   (YYMMDD)
!***Revision Date  920619   (YYMMDD)
!***Purpose  Controlling routine for printing error reports
!***End Prologue  DODPER

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      INTEGER INFO,LDSCLD,LDSTPD,LDWD,LDWE,LD2WD,LD2WE,LIWKMN,LUNERR,  &
              LWKMN,M,N,NETA,NP,NQ,NROW,NTOL
      LOGICAL ISODR

!...Array arguments
      REAL (KIND=R8) DIFF(NQ,NP+M),FJACB(N,NP,NQ),FJACD(N,M,NQ),       &
                     XPLUSD(N,M)
      INTEGER MSGB(NQ*NP+1),MSGD(NQ*M+1)

!...Local scalars
      INTEGER D1,D2,D3,D4,D5,UNIT
      LOGICAL HEAD

!...External subroutines
      EXTERNAL DODPE1,DODPE2,DODPE3,DODPHD

!...Variable Definitions (alphabetically)
!   D1:      The 1st digit (from the left) of INFO.
!   D2:      The 2nd digit (from the left) of INFO.
!   D3:      The 3rd digit (from the left) of INFO.
!   D4:      The 4th digit (from the left) of INFO.
!   D5:      The 5th digit (from the left) of INFO.
!   DIFF:    The relative differences between the user supplied and
!            finite difference derivatives for each derivative checked.
!   FJACB:   The Jacobian with respect to BETA.
!   FJACD:   The Jacobian with respect to DELTA.
!   HEAD:    The variable designating whether the heading is to be 
!            printed (HEAD=.TRUE.) or not (HEAD=.FALSE.).
!   INFO:    The variable designating why the computations were stopped.
!   ISODR:   The variable designating whether the solution is by ODR 
!            (ISODR=.TRUE.) or by OLS (ISODR=.FALSE.).
!   LDSCLD:  The leading dimension of array SCLD.
!   LDSTPD:  The leading dimension of array STPD.
!   LDWD:    The leading dimension of array WD.
!   LDWE:    The leading dimension of array WE.
!   LD2WD:   The second dimension of array WD.
!   LD2WE:   The second dimension of array WE.
!   LIWKMN:  The minimum acceptable length of array IWORK.
!   LUNERR:  The logical unit number used for error messages.
!   LWKMN:   The minimum acceptable length of array WORK.
!   M:       The number of columns of data in the explanatory variable.
!   MSGB:    The error checking results for the Jacobian wrt BETA.
!   MSGD:    The error checking results for the Jacobian wrt DELTA.
!   N:       The number of observations.
!   NETA:    The number of reliable digits in the model.
!   NP:      The number of function parameters.
!   NQ:      The number of responses per observation.
!   NROW:    The row number of the explanatory variable array at
!            which the derivative is to be checked.
!   NTOL:    The number of digits of agreement required between the
!            finite difference and the user supplied derivatives.
!   UNIT:    The logical unit number for error messages.
!   XPLUSD:  The values X + DELTA.


!***First executable statement  DODPER


!  Set logical unit number for error report

      IF (LUNERR.EQ.0) THEN
         RETURN
      ELSE IF (LUNERR.LT.0) THEN
         UNIT = 6
      ELSE
         UNIT = LUNERR
      END IF

!  Print heading

      HEAD = .TRUE.
      CALL DODPHD(HEAD,UNIT)

!  Extract individual digits from variable INFO

      D1 = MOD(INFO,100000)/10000
      D2 = MOD(INFO,10000)/1000
      D3 = MOD(INFO,1000)/100
      D4 = MOD(INFO,100)/10
      D5 = MOD(INFO,10)

!  Print appropriate error messages for ODRPACK95 invoked stop

      IF ((D1.GE.1 .AND. D1.LE.3) .OR. (D1.EQ.7 .OR. D1.EQ.9)) THEN

!  Print appropriate messages for errors in
!     problem specification parameters
!     dimension specification parameters
!     number of good digits in X
!     weights

         CALL DODPE1(UNIT,INFO,D1,D2,D3,D4,D5,N,M,NQ,                  &
                     LDSCLD,LDSTPD,LDWE,LD2WE,LDWD,LD2WD,LWKMN,LIWKMN)

      ELSE IF ((D1.EQ.4) .OR. (MSGB(1).GE.0)) THEN

!  Print appropriate messages for derivative checking

         CALL DODPE2(UNIT,N,M,NP,NQ,FJACB,FJACD,                       &
                      DIFF,MSGB(1),MSGB(2),ISODR,MSGD(1),MSGD(2),      &
                      XPLUSD,NROW,NETA,NTOL)

      ELSE IF (D1.EQ.5) THEN

!  Print appropriate error message for user invoked stop from FCN

         CALL DODPE3(UNIT,D2,D3)

      END IF

!  Print correct form of call statement

      IF ((D1.GE.1 .AND. D1.LE.3) .OR.                                 &
          (D1.EQ.4 .AND. (D2.EQ.2 .OR. D3.EQ.2)) .OR. (D1.EQ.5)) THEN
!        WRITE (UNIT,1100)
         WRITE (ICOUT,999)
  999    FORMAT(1X)
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,1100)
 1100    FORMAT(' The correct form of the call statement is ')
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,1101)
 1101    FORMAT('       CALL ODR')
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,1102)
 1102    FORMAT('      +     (FCN,')
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,1103)
 1103    FORMAT('      +     N,M,NP,NQ,')
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,1104)
 1104    FORMAT('      +     BETA,')
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,1105)
 1105    FORMAT('      +     Y,X,')
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,1106)
 1106    FORMAT('      +     DELTA*,')
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,1107)
 1107    FORMAT('      +     WE*,WD*,')
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,1108)
 1108    FORMAT('      +     IFIXB*,IFIXX*,')
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,1109)
 1109    FORMAT('      +     JOB*,NDIGIT*,TAUFAC*,')
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,1110)
 1110    FORMAT('      +     SSTOL*,PARTOL*,MAXIT*,')
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,1111)
 1111    FORMAT('      +     IPRINT*,LUNERR*,LUNRPT*,')
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,1112)
 1112    FORMAT('      +     STPB*,STPD*,')
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,1113)
 1113    FORMAT('      +     SCLB*,SCLD*,')
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,1114)
 1114    FORMAT('      +     WORK*,IWORK*,')
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,1115)
 1115    FORMAT('      +     INFO*,')
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,1116)
 1116    FORMAT('      +     LOWER*,UPPER*)')
         CALL DPWRST('XXX','BUG ')
      END IF

      RETURN

!  Format statements

!1100 FORMAT
!    &   (//' The correct form of the call statement is '//
!    &      '       CALL ODR'/
!    &      '      +     (FCN,'/
!    &      '      +     N,M,NP,NQ,'/
!    &      '      +     BETA,'/
!    &      '      +     Y,X,'/
!    &      '      +     DELTA*,'/
!    &      '      +     WE*,WD*,'/
!    &      '      +     IFIXB*,IFIXX*,'/
!    &      '      +     JOB*,NDIGIT*,TAUFAC*,'/
!    &      '      +     SSTOL*,PARTOL*,MAXIT*,'/
!    &      '      +     IPRINT*,LUNERR*,LUNRPT*,'/
!    &      '      +     STPB*,STPD*,'/
!    &      '      +     SCLB*,SCLD*,'/
!    &      '      +     WORK*,IWORK*,'/
!    &      '      +     INFO*,'/
!    &      '      +     LOWER*,UPPER*)'/
!    &      ' * optional argument')

      END SUBROUTINE
!DODPHD
      SUBROUTINE DODPHD(HEAD,UNIT)
!***Begin Prologue  DODPHD
!***Refer to  ODR
!***Routines Called  (NONE)
!***Date Written   860529   (YYMMDD)
!***Revision Date  920619   (YYMMDD)
!***Purpose  Print ODRPACK95 heading
!***End Prologue  DODPHD

!...Scalar arguments
      INTEGER UNIT
      LOGICAL HEAD

!...Variable Definitions (alphabetically)
!   HEAD:    The variable designating whether the heading is to be 
!            printed (HEAD=.TRUE.) or not (HEAD=.FALSE.).
!   UNIT:    The logical unit number to which the heading is written.


!***First executable statement  DODPHD

      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
                                                                                                                                  
      IF(ISUBG4.EQ.'DPHD')THEN
        WRITE(ICOUT,52)UNIT
   52   FORMAT('UNIT = ',I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!

      IF (HEAD) THEN
!        WRITE(UNIT,1000)
         WRITE (ICOUT,999)
  999    FORMAT(1X)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1000)
 1000    FORMAT(' *************************************************',  &
                '******** ')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1001)
 1001    FORMAT(' * ODRPACK95 version 1.00 of 12-27-2005 (REAL ',      &
                '(KIND=R8)) * ')
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,1002)
 1002    FORMAT(' *************************************************',  &
                '******** ')
         CALL DPWRST('XXX','BUG ')
         WRITE (ICOUT,999)
         CALL DPWRST('XXX','BUG ')
         HEAD = .FALSE.
      END IF

      RETURN

!   Format statements

!1000 FORMAT (
!    &   ' ********************************************************* '/
!    &   ' * ODRPACK95 version 1.00 of 12-27-2005 (REAL (KIND=R8)) * '/
!    &   ' ********************************************************* '/)
      END SUBROUTINE
!DODSTP
      SUBROUTINE DODSTP(N,M,NP,NQ,NPP,F,FJACB,FJACD,                   &
                        WD,LDWD,LD2WD,SS,TT,LDTT,DELTA,                &
                        ALPHA,EPSFCN,ISODR,                            &
                        TFJACB,OMEGA,U,QRAUX,KPVT,                     &
                        S,T,PHI,IRANK,RCOND,FORVCV,                    &
                        WRK1,WRK2,WRK3,WRK4,WRK5,WRK,LWRK,ISTOPC)
!***Begin Prologue  DODSTP
!***Refer to  ODR
!***Routines Called  IDAMAX,DCHEX,DESUBI,DFCTR,DNRM2,DQRDC,DQRSL,DROT,
!                    DROTG,DSOLVE,DTRCO,DTRSL,DVEVTR,DWGHT,DZERO
!***Date Written   860529   (YYMMDD)
!***Revision Date  920619   (YYMMDD)
!***Purpose  Compute locally constrained steps S and T, and PHI(ALPHA)
!***End Prologue  DODSTP

!...Used modules
      USE REAL_PRECISION
      USE ODRPACK95, ONLY : TEMPRET

!...Scalar arguments
      REAL (KIND=R8) ALPHA,EPSFCN,PHI,RCOND
      INTEGER IRANK,ISTOPC,LDTT,LDWD,LD2WD,LWRK,M,N,NP,NPP,NQ
      LOGICAL ISODR

!...Array arguments
      REAL (KIND=R8) DELTA(N,M),F(N,NQ),FJACB(N,NP,NQ),FJACD(N,M,NQ),  &
                     OMEGA(NQ,NQ),QRAUX(NP),S(NP),SS(NP),              &
                     T(N,M),TFJACB(N,NQ,NP),TT(LDTT,M),U(NP),          &
                     WD(LDWD,LD2WD,M),WRK1(N,NQ,M),WRK2(N,NQ),         &
                     WRK3(NP),WRK4(M,M),WRK5(M),WRK(LWRK)
      INTEGER KPVT(NP)

!...Local scalars
      REAL (KIND=R8) CO,ONE,SI,TEMP,ZERO
      INTEGER I,IMAX,INF,IPVT,J,K,K1,K2,KP,L
      LOGICAL ELIM,FORVCV

!...LOCAL ARRAYS
      REAL (KIND=R8) DUM(2)

!...External functions
      REAL (KIND=R8) DNRM2
      INTEGER IDAMAX
      EXTERNAL DNRM2,IDAMAX

!...External subroutines
      EXTERNAL DCHEX,DESUBI,DFCTR,DQRDC,DQRSL,DROT,DROTG,              &
               DSOLVE,DTRCO,DTRSL,DVEVTR,DZERO

!...Data statements
      DATA ZERO,ONE /0.0E0_R8,1.0E0_R8/

!...Interface blocks
      INTERFACE
      SUBROUTINE DWGHT (N,M,WT,LDWT,LD2WT,T,WTT)
      USE REAL_PRECISION
      INTEGER LDWT,LD2WT,M,N
      REAL (KIND=R8) T(:,:),WT(:,:,:),WTT(:,:)
      END SUBROUTINE
      END INTERFACE

!...Variable definitions (alphabetically)
!   ALPHA:   The Levenberg-Marquardt parameter.
!   CO:      The cosine from the plane rotation.
!   DELTA:   The estimated errors in the explanatory variables.
!   DUM:     A dummy array.
!   ELIM:    The variable designating whether columns of the Jacobian 
!            wrt BETA have been eliminated (ELIM=TRUE) or not
!            (ELIM=FALSE).
!   EPSFCN:  The function's precision.
!   F:       The (weighted) estimated values of EPSILON.
!   FJACB:   The Jacobian with respect to BETA.
!   FJACD:   The Jacobian with respect to DELTA.
!   FORVCV:  The variable designating whether this subroutine was 
!            called to set up for the covariance matrix computations 
!            (FORVCV=TRUE) or not (FORVCV=FALSE).
!   I:       An indexing variable.
!   IMAX:    The index of the element of U having the largest absolute
!            value.
!   INF:     The return code from LINPACK routines.
!   IPVT:    The variable designating whether pivoting is to be done.
!   IRANK:   The rank deficiency of the Jacobian wrt BETA.
!   ISODR:   The variable designating whether the solution is by ODR
!            (ISODR=TRUE) or by OLS (ISODR=FALSE).
!   ISTOPC:  The variable designating whether the computations were 
!            stoped due to a numerical error within subroutine DODSTP.
!   J:       An indexing variable.
!   K:       An indexing variable.
!   K1:      An indexing variable.
!   K2:      An indexing variable.
!   KP:      The rank of the Jacobian wrt BETA.
!   KPVT:    The pivot vector.
!   L:       An indexing variable.
!   LDTT:    The leading dimension of array TT.
!   LDWD:    The leading dimension of array WD.
!   LD2WD:   The second dimension of array WD.
!   LWRK:    The length of vector WRK.
!   M:       The number of columns of data in the explanatory variable.
!   N:       The number of observations.
!   NP:      The number of function parameters.
!   NPP:     The number of function parameters being estimated.
!   OMEGA:   The array defined S.T. 
!            OMEGA*trans(OMEGA) = inv(I+FJACD*inv(E)*trans(FJACD))
!                               = (I-FJACD*inv(P)*trans(FJACD)) 
!            where E = D**2 + ALPHA*TT**2
!                  P = trans(FJACD)*FJACD + D**2 + ALPHA*TT**2
!   ONE:     The value 1.0E0_R8.
!   PHI:     The difference between the norm of the scaled step
!            And the trust region diameter.
!   QRAUX:   The array required to recover the orthogonal part of the
!            Q-R decomposition.
!   RCOND:   The approximate reciprocal condition number of TFJACB.
!   S:       The step for BETA.
!   SI:      The sine from the plane rotation.
!   SS:      The scaling values for the unfixed BETAS.
!   T:       The step for DELTA.
!   TEMP:    A temporary storage LOCATION.
!   TFJACB:  The array OMEGA*FJACB.
!   TT:      The scaling values for DELTA.
!   U:       The approximate null vector for TFJACB.
!   WD:      The (squared) DELTA weights.
!   WRK:     A work array of (LWRK) elements, 
!            equivalenced to WRK1 and WRK2.
!   WRK1:    A work array of (N by NQ by M) elements.
!   WRK2:    A work array of (N by NQ) elements.
!   WRK3:    A work array of (NP) elements.
!   WRK4:    A work array of (M by M) elements.
!   WRK5:    A work array of (M) elements.
!   ZERO:    The value 0.0E0_R8.


!***First executable statement  DODSTP


!  Compute loop parameters which depend on weight structure

!  Set up KPVT if ALPHA = 0

      IF (ALPHA.EQ.ZERO) THEN
         KP = NPP
         DO 10 K=1,NP
            KPVT(K) = K
   10    CONTINUE
      ELSE
         IF (NPP.GE.1) THEN
            KP = NPP-IRANK
         ELSE
            KP = NPP
         END IF
      END IF

      IF (ISODR) THEN

!  T = WD * DELTA = D*G2
         CALL DWGHT(N,M,WD,LDWD,LD2WD,DELTA,T)

         DO 300 I=1,N

!  Compute WRK4, such that
!                TRANS(WRK4)*WRK4 = E = (D**2 + ALPHA*TT**2)
            CALL DESUBI(N,M,WD,LDWD,LD2WD,ALPHA,TT,LDTT,I,WRK4)
            CALL DFCTR(.FALSE.,WRK4,M,M,INF)
            IF (INF.NE.0) THEN
               ISTOPC = 60000
               RETURN
            END IF

!  Compute OMEGA, such that
!                 trans(OMEGA)*OMEGA = I+FJACD*inv(E)*trans(FJACD)
!                 inv(trans(OMEGA)*OMEGA) = I-FJACD*inv(P)*trans(FJACD)
            CALL DVEVTR(M,NQ,I,FJACD,N,M, WRK4,M, WRK1,N,NQ, OMEGA,NQ, &
                        WRK5)
            DO 110 L=1,NQ
               OMEGA(L,L) = ONE + OMEGA(L,L) 
  110       CONTINUE
            CALL DFCTR(.FALSE.,OMEGA,NQ,NQ,INF)
            IF (INF.NE.0) THEN
               ISTOPC = 60000
               RETURN
            END IF

!  Compute WRK1 = trans(FJACD)*(I-FJACD*inv(P)*trans(JFACD))
!               = trans(FJACD)*inv(trans(OMEGA)*OMEGA)
            DO 130 J=1,M
               DO 120 L=1,NQ
                  WRK1(I,L,J) = FJACD(I,J,L)
  120          CONTINUE
               CALL DSOLVE(NQ,OMEGA,NQ,WRK1(I,1:NQ,J),4)
               CALL DSOLVE(NQ,OMEGA,NQ,WRK1(I,1:NQ,J),2)
  130       CONTINUE

!  Compute WRK5 = inv(E)*D*G2
            DO 140 J=1,M
               WRK5(J) = T(I,J)
  140       CONTINUE
            CALL DSOLVE(M,WRK4,M,WRK5,4)
            CALL DSOLVE(M,WRK4,M,WRK5,2)

!  Compute TFJACB = inv(trans(OMEGA))*FJACB
            DO 170 K=1,KP
               DO 150 L=1,NQ
                  TFJACB(I,L,K) = FJACB(I,KPVT(K),L)
  150          CONTINUE
               CALL DSOLVE(NQ,OMEGA,NQ,TFJACB(I,1:NQ,K),4)
               DO 160 L=1,NQ
                  IF (SS(1).GT.ZERO) THEN
                     TFJACB(I,L,K) = TFJACB(I,L,K)/SS(KPVT(K))
                  ELSE
                     TFJACB(I,L,K) = TFJACB(I,L,K)/ABS(SS(1))
                  END IF
  160          CONTINUE
  170       CONTINUE

!  Compute WRK2 = (V*inv(E)*D**2*G2 - G1)
            DO 190 L=1,NQ
               WRK2(I,L) = ZERO
               DO 180 J=1,M
                  WRK2(I,L) = WRK2(I,L) + FJACD(I,J,L)*WRK5(J)
  180          CONTINUE
               WRK2(I,L) = WRK2(I,L) - F(I,L)
  190       CONTINUE

!  Compute WRK2 = inv(trans(OMEGA))*(V*inv(E)*D**2*G2 - G1)
            CALL DSOLVE(NQ,OMEGA,NQ,WRK2(I,1:NQ),4)
  300    CONTINUE

      ELSE
         DO 360 I=1,N
            DO 350 L=1,NQ
               DO 340 K=1,KP
                  TFJACB(I,L,K) = FJACB(I,KPVT(K),L)
                  IF (SS(1).GT.ZERO) THEN
                     TFJACB(I,L,K) = TFJACB(I,L,K)/SS(KPVT(K))
                  ELSE
                     TFJACB(I,L,K) = TFJACB(I,L,K)/ABS(SS(1))
                  END IF
  340          CONTINUE
               WRK2(I,L) = -F(I,L)
  350       CONTINUE
  360    CONTINUE
      END IF

!  Compute S

!  Do QR factorization (with column pivoting of TFJACB if ALPHA = 0)

      IF (ALPHA.EQ.ZERO) THEN
         IPVT = 1
         DO 410 K=1,NP
            KPVT(K) = 0
  410    CONTINUE
      ELSE
         IPVT = 0
      END IF

      CALL DQRDC(TFJACB,N*NQ,N*NQ,KP,QRAUX,KPVT,WRK3,IPVT)
      CALL DQRSL(TFJACB,N*NQ,N*NQ,KP,                                  &
                 QRAUX,WRK2,DUM,WRK2,DUM,DUM,DUM,1000,INF)
      IF (INF.NE.0) THEN
         ISTOPC = 60000
         RETURN
      END IF

!  Eliminate alpha part using givens rotations

      IF (ALPHA.NE.ZERO) THEN
         CALL DZERO(NPP,1,S,NPP)
         DO 430 K1=1,KP
            CALL DZERO(KP,1,WRK3,KP)
            WRK3(K1) = SQRT(ALPHA)
            DO 420 K2=K1,KP
               CALL DROTG(TFJACB(K2,1,K2),WRK3(K2),CO,SI)
               IF (KP-K2.GE.1) THEN
                  CALL DROT(KP-K2,TFJACB(K2,1,K2+1),N*NQ,              &
                            WRK3(K2+1),1,CO,SI)
               END IF
               TEMP       =  CO*WRK2(K2,1) + SI*S(KPVT(K1)) 
               S(KPVT(K1)) = -SI*WRK2(K2,1) + CO*S(KPVT(K1))
               WRK2(K2,1)      = TEMP
  420       CONTINUE
  430    CONTINUE
      END IF

!  Compute solution - eliminate variables if necessary

      IF (NPP.GE.1) THEN
         IF (ALPHA.EQ.ZERO) THEN
            KP = NPP

!  Estimate RCOND - U will contain approx null vector

  440       CALL DTRCO(TFJACB,N*NQ,KP,RCOND,U,1)
            IF (RCOND.LE.EPSFCN) THEN
               ELIM = .TRUE.
               IMAX = IDAMAX(KP,U,1)

! IMAX is the column to remove - use DCHEX and fix KPVT

               IF (IMAX.NE.KP) THEN
                  CALL DCHEX(TFJACB,N*NQ,KP,IMAX,KP,WRK2,N*NQ,1,       &
                             QRAUX,WRK3,2)
                  K = KPVT(IMAX)
                  DO 450 I=IMAX,KP-1
                     KPVT(I) = KPVT(I+1)
  450             CONTINUE
                  KPVT(KP) = K
               END IF
               KP = KP-1
            ELSE
               ELIM = .FALSE.
            END IF
            IF (ELIM .AND. KP.GE.1) THEN
               GO TO 440
            ELSE
               IRANK = NPP-KP
            END IF
         END IF
      END IF

      IF (FORVCV) RETURN

!  Backsolve and unscramble

      IF (NPP.GE.1) THEN
         DO 510 I=KP+1,NPP
            WRK2(I,1) = ZERO
  510    CONTINUE
         IF (KP.GE.1) THEN
            CALL DTRSL(TFJACB,N*NQ,KP,WRK2,01,INF)
            IF (INF.NE.0) THEN
               ISTOPC = 60000
               RETURN
            END IF
         END IF
         DO 520 I=1,NPP
            IF (SS(1).GT.ZERO) THEN
               S(KPVT(I)) = WRK2(I,1)/SS(KPVT(I))
            ELSE
               S(KPVT(I)) = WRK2(I,1)/ABS(SS(1))
            END IF
  520    CONTINUE
      END IF

      IF (ISODR) THEN

!  NOTE: T and WRK1 have been initialized above,
!        where T    = WD * DELTA = D*G2
!              WRK1 = trans(FJACD)*(I-FJACD*inv(P)*trans(JFACD))

         DO 670 I=1,N

!  Compute WRK4, such that
!                trans(WRK4)*WRK4 = E = (D**2 + ALPHA*TT**2)
            CALL DESUBI(N,M,WD,LDWD,LD2WD,ALPHA,TT,LDTT,I,WRK4)
            CALL DFCTR(.FALSE.,WRK4,M,M,INF)
            IF (INF.NE.0) THEN
               ISTOPC = 60000
               RETURN
            END IF

!  Compute WRK5 = inv(E)*D*G2
            DO 610 J=1,M
               WRK5(J) = T(I,J)
  610       CONTINUE
            CALL DSOLVE(M,WRK4,M,WRK5,4)
            CALL DSOLVE(M,WRK4,M,WRK5,2)

            DO 640 L=1,NQ
               WRK2(I,L) = F(I,L) 
               DO 620 K=1,NPP
                  WRK2(I,L) = WRK2(I,L) + FJACB(I,K,L)*S(K)
  620          CONTINUE
               DO 630 J=1,M
                  WRK2(I,L) = WRK2(I,L) - FJACD(I,J,L)*WRK5(J)
  630          CONTINUE
  640       CONTINUE

            DO 660 J=1,M
               WRK5(J) = ZERO
               DO 650 L=1,NQ
                  WRK5(J) = WRK5(J) + WRK1(I,L,J)*WRK2(I,L)
  650          CONTINUE
               T(I,J) = -(WRK5(J) + T(I,J))
  660       CONTINUE
            CALL DSOLVE(M,WRK4,M,T(I,1:M),4)
            CALL DSOLVE(M,WRK4,M,T(I,1:M),2)
  670    CONTINUE

      END IF

!  Compute PHI(ALPHA) from scaled S and T

      CALL DWGHT(NPP,1,RESHAPE(SS,(/NPP,1,1/)),NPP,1,                  &
                 RESHAPE(S,(/NPP,1/)),TEMPRET(1:NPP,1:1))
      WRK(1:NPP) = TEMPRET(1:NPP,1)
      IF (ISODR) THEN
         CALL DWGHT(N,M,RESHAPE(TT,(/LDTT,1,M/)),LDTT,1,               &
                    T,TEMPRET(1:N,1:M))
         WRK(NPP+1:NPP+1+N*M-1) = RESHAPE(TEMPRET(1:N,1:M),(/N*M/))
         PHI = DNRM2(NPP+N*M,WRK,1)
      ELSE
         PHI = DNRM2(NPP,WRK,1)
      END IF

      RETURN
      END SUBROUTINE
!DODVCV
      SUBROUTINE DODVCV(N,M,NP,NQ,NPP,F,FJACB,FJACD,                   &
                        WD,LDWD,LD2WD,SSF,SS,TT,LDTT,DELTA,            &
                        EPSFCN,ISODR,VCV,SD,WRK6,OMEGA,U,QRAUX,JPVT,   &
                        S,T,IRANK,RCOND,RSS,IDF,RVAR,IFIXB,            &
                        WRK1,WRK2,WRK3,WRK4,WRK5,WRK,LWRK,ISTOPC)
!***Begin Prologue  DODVCV
!***Refer to  ODR
!***Routines Called  DPODI,DODSTP
!***Date Written   901207   (YYMMDD)
!***Revision Date  920619   (YYMMDD)
!***Purpose  Compute covariance matrix of estimated parameters
!***End Prologue  DODVCV

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      REAL (KIND=R8) EPSFCN,RCOND,RSS,RVAR
      INTEGER IDF,IRANK,ISTOPC,LDTT,LDWD,LD2WD,LWRK,M,N,NP,NPP,NQ
      LOGICAL ISODR

!...Array arguments
      REAL (KIND=R8) DELTA(N,M),F(N,NQ),FJACB(N,NP,NQ),FJACD(N,M,NQ),  &
                     OMEGA(NQ,NQ),QRAUX(NP),S(NP),SD(NP),SS(NP),       &
                     SSF(NP),T(N,M),TT(LDTT,M),U(NP),VCV(NP,NP),       &
                     WD(LDWD,LD2WD,M),WRK1(N,NQ,M),WRK2(N,NQ),         &
                     WRK3(NP),WRK4(M,M),WRK5(M),WRK6(N*NQ,NP),WRK(LWRK)
      INTEGER IFIXB(NP),JPVT(NP)

!...Local scalars
      REAL (KIND=R8) TEMP,ZERO
      INTEGER I,IUNFIX,J,JUNFIX,KP,L
      LOGICAL FORVCV

!...External subroutines
      EXTERNAL DPODI,DODSTP

!...Data statements
      DATA ZERO /0.0E0_R8/

!...Variable definitions (alphabetically)
!   DELTA:   The estimated errors in the explanatory variables.
!   EPSFCN:  The function's precision.
!   F:       The (weighted) estimated values of EPSILON.
!   FJACB:   The Jacobian with respect to BETA.
!   FJACD:   The Jacobian with respect to DELTA.
!   FORVCV:  The variable designating whether subroutine DODSTP is 
!            called to set up for the covariance matrix computations 
!            (FORVCV=TRUE) or not (FORVCV=FALSE).
!   I:       An indexing variable.
!   IDF:     The degrees of freedom of the fit, equal to the number of
!            observations with nonzero weighted derivatives minus the
!            number of parameters being estimated.
!   IFIXB:   The values designating whether the elements of BETA are 
!            fixed at their input values or not.
!   IMAX:    The index of the element of U having the largest absolute
!            value.
!   IRANK:   The rank deficiency of the Jacobian wrt BETA.
!   ISODR:   The variable designating whether the solution is by ODR 
!            (ISODR=TRUE) or by OLS (ISODR=FALSE).
!   ISTOPC:  The variable designating whether the computations were
!            stoped due to a numerical error within subroutine DODSTP.
!   IUNFIX:  The index of the next unfixed parameter.
!   J:       An indexing variable.
!   JPVT:    The pivot vector.
!   JUNFIX:  The index of the next unfixed parameter.
!   KP:      The rank of the Jacobian wrt BETA.
!   L:       An indexing variable.
!   LDTT:    The leading dimension of array TT.
!   LDWD:    The leading dimension of array WD.
!   LD2WD:   The second dimension of array WD.
!   LWRK:    The length of vector WRK.
!   M:       The number of columns of data in the explanatory variable.
!   N:       The number of observations.
!   NP:      The number of function parameters.
!   NPP:     The number of function parameters being estimated.
!   NQ:      The number of responses per observation.
!   OMEGA:   The array defined S.T.
!            OMEGA*trans(OMEGA) = inv(I+FJACD*inv(E)*trans(FJACD))
!                               = (I-FJACD*inv(P)*trans(FJACD))
!            where E = D**2 + ALPHA*TT**2
!                  P = trans(FJACD)*FJACD + D**2 + ALPHA*TT**2
!   QRAUX:   The array required to recover the orthogonal part of the
!            Q-R decomposition.
!   RCOND:   The approximate reciprocal condition of FJACB.
!   RSS:     The residual sum of squares.
!   RVAR:    The residual variance.
!   S:       The step for BETA.
!   SD:      The standard deviations of the estimated BETAS.
!   SS:      The scaling values for the unfixed BETAS.
!   SSF:     The scaling values used for BETA.
!   T:       The step for DELTA.
!   TEMP:    A temporary storage location
!   TT:      The scaling values for DELTA.
!   U:       The approximate null vector for FJACB.
!   VCV:     The covariance matrix of the estimated BETAS.
!   WD:      The DELTA weights.
!   WRK:     A work array of (LWRK) elements,
!            equivalenced to WRK1 and WRK2.
!   WRK1:    A work array of (N by NQ by M) elements.
!   WRK2:    A work array of (N by NQ) elements.
!   WRK3:    A work array of (NP) elements.
!   WRK4:    A work array of (M by M) elements.
!   WRK5:    A work array of (M) elements.
!   WRK6:    A work array of (N*NQ by P) elements.
!   ZERO:    The value 0.0E0_R8.


!***First executable statement  DODVCV


      FORVCV = .TRUE.
      ISTOPC = 0

      CALL DODSTP(N,M,NP,NQ,NPP,F,FJACB,FJACD,                         &
                  WD,LDWD,LD2WD,SS,TT,LDTT,DELTA,ZERO,EPSFCN,ISODR,    &
                  WRK6,OMEGA,U,QRAUX,JPVT,S,T,TEMP,IRANK,RCOND,FORVCV, &
                  WRK1,WRK2,WRK3,WRK4,WRK5,WRK,LWRK,ISTOPC)
      IF (ISTOPC.NE.0) THEN
         RETURN
      END IF
      KP = NPP - IRANK
      CALL DPODI (WRK6,N*NQ,KP,WRK3,1)

      IDF = 0
      DO 150 I=1,N
         DO 120 J=1,NPP
            DO 110 L=1,NQ
               IF (FJACB(I,J,L).NE.ZERO) THEN
                  IDF = IDF + 1
                  GO TO 150
               END IF
  110       CONTINUE
  120    CONTINUE
         IF (ISODR) THEN
            DO 140 J=1,M
               DO 130 L=1,NQ
                  IF (FJACD(I,J,L).NE.ZERO) THEN
                     IDF = IDF + 1
                     GO TO 150
                  END IF
  130          CONTINUE
  140       CONTINUE
         END IF
  150 CONTINUE

      IF (IDF.GT.KP) THEN
         IDF = IDF - KP
         RVAR = RSS/IDF
      ELSE
         IDF = 0
         RVAR = RSS
      END IF

!  Store variances in SD, restoring original order

      DO 200 I=1,NP
         SD(I) = ZERO
  200 CONTINUE
      DO 210 I=1,KP
         SD(JPVT(I)) = WRK6(I,I)
  210 CONTINUE
      IF (NP.GT.NPP) THEN
         JUNFIX = NPP
         DO 220 J=NP,1,-1
            IF (IFIXB(J).EQ.0) THEN
               SD(J) = ZERO
            ELSE
               SD(J) = SD(JUNFIX)
               JUNFIX = JUNFIX - 1
            END IF
  220    CONTINUE
      END IF

!  Store covariance matrix in VCV, restoring original order

      DO 310 I=1,NP
         DO 300 J=1,I
            VCV(I,J) = ZERO
  300    CONTINUE
  310 CONTINUE
      DO 330 I=1,KP
         DO 320 J=I+1,KP
            IF (JPVT(I).GT.JPVT(J)) THEN
               VCV(JPVT(I),JPVT(J))=WRK6(I,J)
            ELSE
               VCV(JPVT(J),JPVT(I))=WRK6(I,J)
            END IF
  320    CONTINUE
  330 CONTINUE
      IF (NP.GT.NPP) THEN
         IUNFIX = NPP
         DO 360 I=NP,1,-1
            IF (IFIXB(I).EQ.0) THEN
               DO 340 J=I,1,-1
                  VCV(I,J) = ZERO
  340          CONTINUE
            ELSE
               JUNFIX = NPP
               DO 350 J=NP,1,-1
                  IF (IFIXB(J).EQ.0) THEN
                     VCV(I,J) = ZERO
                  ELSE
                     VCV(I,J) = VCV(IUNFIX,JUNFIX)
                     JUNFIX = JUNFIX - 1
                  END IF
  350          CONTINUE
               IUNFIX = IUNFIX - 1
            END IF
  360    CONTINUE
      END IF

      DO 380 I=1,NP
         VCV(I,I) = SD(I)
         SD(I) = SQRT(RVAR*SD(I))
         DO 370 J=1,I
            VCV(J,I) = VCV(I,J)
  370    CONTINUE
  380 CONTINUE

!  Unscale standard errors and covariance matrix
      DO 410 I=1,NP
         IF (SSF(1).GT.ZERO) THEN
            SD(I) = SD(I)/SSF(I)
         ELSE
            SD(I) = SD(I)/ABS(SSF(1))
         END IF
         DO 400 J=1,NP
            IF (SSF(1).GT.ZERO) THEN
               VCV(I,J) = VCV(I,J)/(SSF(I)*SSF(J))
            ELSE
               VCV(I,J) = VCV(I,J)/(SSF(1)*SSF(1))
            END IF
  400    CONTINUE
  410 CONTINUE

      RETURN
      END SUBROUTINE
!DPACK
      SUBROUTINE DPACK(N2,N1,V1,V2,IFIX)
!***Begin Prologue  DPACK
!***Refer to  ODR
!***Routines Called  DCOPY
!***Date Written   860529   (YYMMDD)
!***Revision Date  920304   (YYMMDD)
!***Purpose  Select the unfixed elements of V2 and return them in V1
!***End Prologue  DPACK

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      INTEGER N1,N2

!...Array arguments
      REAL (KIND=R8) V1(N2),V2(N2)
      INTEGER IFIX(N2)

!...Local scalars
      INTEGER I

!...External subroutines
      EXTERNAL DCOPY

!...Variable definitions (alphabetically)
!   I:       An indexing variable.
!   IFIX:    The values designating whether the elements of V2 are 
!            fixed at their input values or not.
!   N1:      The number of items in V1.
!   N2:      The number of items in V2.
!   V1:      The vector of the unfixed items from V2.
!   V2:      The vector of the fixed and unfixed items from which the
!            unfixed elements are to be extracted.


!***First executable statement  DPACK


      N1 = 0
      IF (IFIX(1).GE.0) THEN
         DO 10 I=1,N2
            IF (IFIX(I).NE.0) THEN
               N1 = N1+1
               V1(N1) = V2(I)
            END IF
   10    CONTINUE
      ELSE
         N1 = N2
         CALL DCOPY(N2,V2,1,V1,1)
      END IF

      RETURN
      END SUBROUTINE
!DPVB
      SUBROUTINE DPVB(FCN,N,M,NP,NQ,BETA,XPLUSD,IFIXB,IFIXX,LDIFX,     &
                      NROW,J,LQ,STP,ISTOP,NFEV,PVB,WRK1,WRK2,WRK6)
!***Begin Prologue  DPVB
!***Refer to  ODR
!***Routines Called  FCN
!***Date Written   860529   (YYMMDD)
!***Revision Date  920304   (YYMMDD)
!***Purpose  Compute the NROW-th function value using BETA(J) + STP
!***End Prologue  DPVB

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      REAL (KIND=R8) PVB,STP
      INTEGER ISTOP,J,LDIFX,LQ,M,N,NFEV,NP,NQ,NROW

!...Array arguments
      REAL (KIND=R8) BETA(NP),WRK1(N,M,NQ),WRK2(N,NQ),WRK6(N,NP,NQ),   &
                     XPLUSD(N,M)
      INTEGER IFIXB(NP),IFIXX(LDIFX,M)

!...Subroutine arguments
      EXTERNAL FCN

!...Local scalars
      REAL (KIND=R8) BETAJ

!...Routine names used as subprogram arguments
!   FCN:     The user-supplied subroutine for evaluating the model.

!...Variable Definitions (alphabetically)
!   BETA:    The function parameters.
!   BETAJ:   The current estimate of the jth parameter.
!   IFIXB:   The values designating whether the elements of BETA are
!            fixed at their input values or not.
!   IFIXX:   The values designating whether the elements of X are
!            fixed at their input values or not.
!   ISTOP:   The variable designating whether there are problems
!            computing the function at the current BETA and DELTA.
!   J:       The index of the partial derivative being examined.
!   LDIFX:   The leading dimension of array IFIXX.
!   LQ:      The response currently being examined.
!   M:       The number of columns of data in the independent variable.
!   N:       The number of observations.
!   NFEV:    The number of function evaluations. 
!   NP:      The number of function parameters.
!   NQ:      The number of responses per observation.
!   NROW:    The row number of the independent variable array at
!            which the derivative is to be checked.
!   PVB:     The function value for the selected observation & response.
!   STP:     The step size for the finite difference derivative.
!   XPLUSD:  The values of X + DELTA.


!***First executable statement  DPVB


!  Compute predicted values

      BETAJ = BETA(J)
      BETA(J) = BETA(J) + STP
      ISTOP = 0
      CALL FCN(N,M,NP,NQ,N,M,NP,BETA,XPLUSD,IFIXB,IFIXX,LDIFX,         &
               003,WRK2,WRK6,WRK1,ISTOP)
      IF (ISTOP.EQ.0) THEN
         NFEV = NFEV + 1
      ELSE
         RETURN
      END IF
      BETA(J) = BETAJ

      PVB = WRK2(NROW,LQ)

      RETURN
      END SUBROUTINE
!DPVD
      SUBROUTINE DPVD(FCN,N,M,NP,NQ,BETA,XPLUSD,IFIXB,IFIXX,LDIFX,     &
                      NROW,J,LQ,STP,ISTOP,NFEV,PVD,WRK1,WRK2,WRK6)
!***Begin Prologue  DPVD
!***Refer to  ODR
!***Routines Called  FCN
!***Date Written   860529   (YYMMDD)
!***Revision Date  920304   (YYMMDD)
!***Purpose  Compute NROW-th function value using
!            X(NROW,J) + DELTA(NROW,J) + STP
!***End Prologue  DPVD

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      REAL (KIND=R8) PVD,STP
      INTEGER ISTOP,J,LDIFX,LQ,M,N,NFEV,NP,NQ,NROW

!...Array arguments
      REAL (KIND=R8) BETA(NP),WRK1(N,M,NQ),WRK2(N,NQ),WRK6(N,NP,NQ),   &
                     XPLUSD(N,M)
      INTEGER IFIXB(NP),IFIXX(LDIFX,M)

!...Subroutine arguments
      EXTERNAL FCN

!...Local scalars
      REAL (KIND=R8) XPDJ

!...Routine names used as subprogram arguments
!   FCN:     The user-supplied subroutine for evaluating the model.

!...Variable Definitions (alphabetically)
!   BETA:    The function parameters.
!   IFIXB:   The values designating whether the elements of BETA are
!            fixed at their input values or not.
!   IFIXX:   The values designating whether the elements of X are
!            fixed at their input values or not.
!   ISTOP:   The variable designating whether there are problems 
!            computing the function at the current BETA and DELTA.
!   J:       The index of the partial derivative being examined.
!   LDIFX:   The leading dimension of array IFIXX.
!   LQ:      The response currently being examined.
!   M:       The number of columns of data in the independent variable.
!   N:       The number of observations.
!   NFEV:    The number of function evaluations. 
!   NP:      The number of function parameters.
!   NQ:      The number of responses per observation.
!   NROW:    The row number of the independent variable array at
!            which the derivative is to be checked.
!   PVD:     The function value for the selected observation & response.
!   STP:     The step size for the finite difference derivative.
!   XPDJ:    The (NROW,J)th element of XPLUSD.
!   XPLUSD:  The values of X + DELTA.


!***First executable statement  DPVD


!  Compute predicted values

      XPDJ = XPLUSD(NROW,J)
      XPLUSD(NROW,J) = XPLUSD(NROW,J) + STP
      ISTOP = 0
      CALL FCN(N,M,NP,NQ,N,M,NP,BETA,XPLUSD,IFIXB,IFIXX,LDIFX,         &
               003,WRK2,WRK6,WRK1,ISTOP)
      IF (ISTOP.EQ.0) THEN
         NFEV = NFEV + 1
      ELSE
         RETURN
      END IF
      XPLUSD(NROW,J) = XPDJ

      PVD = WRK2(NROW,LQ)

      RETURN
      END SUBROUTINE
!DSCALE
      SUBROUTINE DSCALE(N,M,SCL,LDSCL,T,LDT,SCLT,LDSCLT)
!***Begin Prologue  DSCALE
!***Refer to  ODR
!***Routines Called  (NONE)
!***Date Written   860529   (YYMMDD)
!***Revision Date  920304   (YYMMDD)
!***Purpose  Scale T by the inverse of SCL, I.E., compute T/SCL
!***End Prologue  DSCALE

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      INTEGER LDT,LDSCL,LDSCLT,M,N

!...Array arguments
      REAL (KIND=R8) T(LDT,M),SCL(LDSCL,M),SCLT(LDSCLT,M)

!...Local scalars
      REAL (KIND=R8) ONE,TEMP,ZERO
      INTEGER I,J

!...Data statements
      DATA ONE,ZERO /1.0E0_R8,0.0E0_R8/

!...Variable Definitions (alphabetically)
!   I:       An indexing variable.
!   J:       An indexing variable.
!   LDSCL:   The leading dimension of array SCL.
!   LDSCLT:  The leading dimension of array SCLT.
!   LDT:     The leading dimension of array T.
!   M:       The number of columns of data in T.
!   N:       The number of rows of data in T.
!   ONE:     The value 1.0E0_R8.
!   SCL:     The scale values.
!   SCLT:    The inversely scaled matrix.
!   T:       The array to be inversely scaled by SCL.
!   TEMP:    A temporary scalar.
!   ZERO:    The value 0.0E0_R8.


!***First executable statement  DSCALE


      IF (N.EQ.0 .OR. M.EQ.0) RETURN

      IF (SCL(1,1).GE.ZERO) THEN
         IF (LDSCL.GE.N) THEN
            DO 80 J=1,M
               DO 70 I=1,N
                  SCLT(I,J) = T(I,J)/SCL(I,J)
   70          CONTINUE
   80       CONTINUE
         ELSE
            DO 100 J=1,M
               TEMP = ONE/SCL(1,J)
               DO 90 I=1,N
                  SCLT(I,J) = T(I,J)*TEMP
   90          CONTINUE
  100       CONTINUE
         END IF
      ELSE
         TEMP = ONE/ABS(SCL(1,1))
         DO 120 J=1,M
            DO 110 I=1,N
               SCLT(I,J) = T(I,J)*TEMP
  110       CONTINUE
  120    CONTINUE
      END IF

      RETURN
      END SUBROUTINE
!DSCLB
      SUBROUTINE DSCLB(NP,BETA,SSF)
!***Begin Prologue  DSCLB
!***Refer to  ODR
!***Routines Called  (NONE)
!***Date Written   860529   (YYMMDD)
!***Revision Date  920304   (YYMMDD)
!***Purpose  Select scaling values for BETA according to the
!            algorithm given in the ODRPACK95 reference guide
!***End Prologue  DSCLB

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      INTEGER NP

!...Array arguments
      REAL (KIND=R8) BETA(NP),SSF(NP)

!...Local scalars
      REAL (KIND=R8) BMAX,BMIN,ONE,TEN,ZERO
      INTEGER K
      LOGICAL BIGDIF

!...Data statements
      DATA ZERO,ONE,TEN /0.0E0_R8,1.0E0_R8,10.0E0_R8/

!...Variable Definitions (alphabetically)
!   BETA:    The function parameters.
!   BIGDIF:  The variable designating whether there is a significant 
!            difference in the magnitudes of the nonzero elements of
!            BETA (BIGDIF=.TRUE.) or not (BIGDIF=.FALSE.).
!   BMAX:    The largest nonzero magnitude.
!   BMIN:    The smallest nonzero magnitude.
!   K:       An indexing variable.
!   NP:      The number of function parameters.
!   ONE:     The value 1.0E0_R8.
!   SSF:     The scaling values for BETA.
!   TEN:     The value 10.0E0_R8.
!   ZERO:    The value 0.0E0_R8.


!***First executable statement  DSCLB


      BMAX = ABS(BETA(1))
      DO 10 K=2,NP
         BMAX = MAX(BMAX,ABS(BETA(K)))
   10 CONTINUE

      IF (BMAX.EQ.ZERO) THEN

!  All input values of BETA are zero

         DO 20 K=1,NP
            SSF(K) = ONE
   20    CONTINUE

      ELSE

!  Some of the input values are nonzero

         BMIN = BMAX
         DO 30 K=1,NP
            IF (BETA(K).NE.ZERO) THEN
               BMIN = MIN(BMIN,ABS(BETA(K)))
            END IF
   30    CONTINUE
         BIGDIF = LOG10(BMAX)-LOG10(BMIN).GE.ONE
         DO 40 K=1,NP
            IF (BETA(K).EQ.ZERO) THEN
               SSF(K) =  TEN/BMIN
            ELSE
               IF (BIGDIF) THEN
                  SSF(K) = ONE/ABS(BETA(K))
               ELSE
                  SSF(K) = ONE/BMAX
               END IF
            END IF
   40    CONTINUE

      END IF

      RETURN
      END SUBROUTINE
!DSCLD
      SUBROUTINE DSCLD(N,M,X,LDX,TT,LDTT)
!***Begin Prologue  DSCLD
!***Refer to  ODR
!***Routines Called  (None)
!***Date Written   860529   (YYMMDD)
!***Revision Date  920304   (YYMMDD)
!***Purpose  Select scaling values for DELTA according to the 
!            algorithm given in the ODRPACK95 reference guide
!***End Prologue  DSCLD

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      INTEGER LDTT,LDX,M,N

!...Array arguments
      REAL (KIND=R8) TT(LDTT,M),X(LDX,M)

!...Local scalars
      REAL (KIND=R8) ONE,TEN,XMAX,XMIN,ZERO
      INTEGER I,J
      LOGICAL BIGDIF

!...Data statements
      DATA ZERO,ONE,TEN /0.0E0_R8,1.0E0_R8,10.0E0_R8/

!...Variable Definitions (alphabetically)
!   BIGDIF:  The variable designating whether there is a significant 
!            difference in the magnitudes of the nonzero elements of
!            X (BIGDIF=.TRUE.) or not (BIGDIF=.FALSE.).
!   I:       An indexing variable.
!   J:       An indexing variable.
!   LDTT:    The leading dimension of array TT.
!   LDX:     The leading dimension of array X.
!   M:       The number of columns of data in the independent variable.
!   N:       The number of observations.
!   ONE:     The value 1.0E0_R8.
!   TT:      THE SCALING VALUES FOR DELTA.
!   X:       The independent variable.
!   XMAX:    The largest nonzero magnitude.
!   XMIN:    THE SMALLEST NONZERO MAGNITUDE.
!   ZERO:    The value 0.0E0_R8.


!***First executable statement  DSCLD


      DO 50 J=1,M
         XMAX = ABS(X(1,J))
         DO 10 I=2,N
            XMAX = MAX(XMAX,ABS(X(I,J)))
   10    CONTINUE

         IF (XMAX.EQ.ZERO) THEN

!  All input values of X(I,J), I=1,...,N, are zero

            DO 20 I=1,N
               TT(I,J) = ONE
   20       CONTINUE

         ELSE

!  Some of the input values are nonzero

            XMIN = XMAX
            DO 30 I=1,N
               IF (X(I,J).NE.ZERO) THEN
                  XMIN = MIN(XMIN,ABS(X(I,J)))
               END IF
   30       CONTINUE
            BIGDIF = LOG10(XMAX)-LOG10(XMIN).GE.ONE
            DO 40 I=1,N
               IF (X(I,J).NE.ZERO) THEN
                  IF (BIGDIF) THEN
                     TT(I,J) = ONE/ABS(X(I,J))
                  ELSE
                     TT(I,J) = ONE/XMAX
                  END IF
               ELSE
                  TT(I,J) = TEN/XMIN
               END IF
   40       CONTINUE
         END IF
   50 CONTINUE

      RETURN
      END SUBROUTINE
!DSETN
      SUBROUTINE DSETN(N,M,X,LDX,NROW)
!***Begin Prologue  DSETN
!***Refer to  ODR
!***Routines Called  (None)
!***Date Written   860529   (YYMMDD)
!***Revision Date  920304   (YYMMDD)
!***Purpose  Select the row at which the derivative will be checked
!***End Prologue  DSETN

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      INTEGER LDX,M,N,NROW

!...Array arguments
      REAL (KIND=R8) X(LDX,M)

!...Local scalars
      INTEGER I,J

!...Variable Definitions (alphabetically)
!   I:       An index variable.
!   J:       An index variable.
!   LDX:     The leading dimension of array X.
!   M:       The number of columns of data in the independent variable.
!   N:       The number of observations.
!   NROW:    The selected row number of the independent variable.
!   X:       The independent variable.


!***First executable statement  DSETN


      IF ((NROW.GE.1) .AND. (NROW.LE.N)) RETURN

!     Select first row of independent variables which contains no zeros
!     if there is one, otherwise first row is used.

      DO 20 I = 1, N
         DO 10 J = 1, M
            IF (X(I,J).EQ.0.0) GO TO 20
   10    CONTINUE
         NROW = I
         RETURN
   20 CONTINUE

      NROW = 1

      RETURN
      END SUBROUTINE
!DSOLVE
      SUBROUTINE DSOLVE(N,T,LDT,B,JOB)
!***Begin Prologue  DSOLVE
!***Refer to  ODR
!***Routines Called  DAXPY,DDOT
!***Date Written   920220   (YYMMDD)
!***Revision Date  920619   (YYMMDD)
!***Purpose  Solve systems of the form
!                   T * X = B  or  trans(T) * X = B
!            where T is an upper or lower triangular matrix of order N,
!            and the solution X overwrites the RHS B.
!            (adapted from LINPACK subroutine DTRSL)
!***References  Dongarra J.J., Bunch J.R., Moler C.B., Stewart G.W.,
!                 *LINPACK Users Guide*, SIAM, 1979.
!***End Prologue  DSOLVE

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      INTEGER JOB,LDT,N

!...Array arguments
      REAL (KIND=R8) B(N),T(LDT,N)

!...Local scalars
      REAL (KIND=R8) TEMP,ZERO
      INTEGER J1,J,JN

!...External functions
      REAL (KIND=R8) DDOT
      EXTERNAL DDOT

!...External subroutines
      EXTERNAL DAXPY

!...Data statements
      DATA ZERO /0.0E0_R8/

!...Variable Definitions (alphabetically)
!   B:       On input:  the right hand side;  On exit:  the solution
!   J1:      The first nonzero entry in T.
!   J:       An indexing variable.
!   JN:      The last nonzero entry in T.
!   JOB:     What kind of system is to be solved, where if JOB is
!            1   Solve T*X=B, T lower triangular,
!            2   Solve T*X=B, T upper triangular,
!            3   Solve trans(T)*X=B, T lower triangular,
!            4   Solve trans(T)*X=B, T upper triangular.
!   LDT:     The leading dimension of array T.
!   N:       The number of rows and columns of data in array T.
!   T:       The upper or lower tridiagonal system.
!   ZERO:    The value 0.0E0_R8.


!***First executable statement  DSOLVE


!  Find first nonzero diagonal entry in T
         J1 = 0
         DO 10 J=1,N
            IF (J1.EQ.0 .AND. T(J,J).NE.ZERO) THEN
               J1 = J
            ELSE IF (T(J,J).EQ.ZERO) THEN
               B(J) = ZERO
            END IF
   10    CONTINUE
         IF (J1.EQ.0) RETURN

!  Find last nonzero diagonal entry in T
         JN = 0
         DO 20 J=N,J1,-1
            IF (JN.EQ.0 .AND. T(J,J).NE.ZERO) THEN
               JN = J
            ELSE IF (T(J,J).EQ.ZERO) THEN
               B(J) = ZERO
            END IF
   20    CONTINUE

         IF (JOB.EQ.1) THEN

!  Solve T*X=B for T lower triangular
            B(J1) = B(J1)/T(J1,J1)
            DO 30 J = J1+1, JN
               TEMP = -B(J-1)
               CALL DAXPY(JN-J+1,TEMP,T(J,J-1),1,B(J),1)
               IF (T(J,J).NE.ZERO) THEN
                  B(J) = B(J)/T(J,J)
               ELSE
                  B(J) = ZERO
               END IF
   30       CONTINUE

         ELSE IF (JOB.EQ.2) THEN

!  Solve T*X=B for T upper triangular.
            B(JN) = B(JN)/T(JN,JN)
            DO 40 J = JN-1,J1,-1
               TEMP = -B(J+1)
               CALL DAXPY(J,TEMP,T(1,J+1),1,B(1),1)
               IF (T(J,J).NE.ZERO) THEN
                  B(J) = B(J)/T(J,J)
               ELSE
                  B(J) = ZERO
               END IF
   40       CONTINUE

         ELSE IF (JOB.EQ.3) THEN

!  Solve trans(T)*X=B for T lower triangular.
            B(JN) = B(JN)/T(JN,JN)
            DO 50 J = JN-1,J1,-1
               B(J) = B(J) - DDOT(JN-J+1,T(J+1,J),1,B(J+1),1)
               IF (T(J,J).NE.ZERO) THEN
                  B(J) = B(J)/T(J,J)
               ELSE
                  B(J) = ZERO
               END IF
   50       CONTINUE

         ELSE IF (JOB.EQ.4) THEN

!  Solve trans(T)*X=B for T upper triangular.
            B(J1) = B(J1)/T(J1,J1)
            DO 60 J = J1+1,JN
               B(J) = B(J) - DDOT(J-1,T(1,J),1,B(1),1)
               IF (T(J,J).NE.ZERO) THEN
                  B(J) = B(J)/T(J,J)
               ELSE
                  B(J) = ZERO
               END IF
   60       CONTINUE
         END IF

      RETURN
      END SUBROUTINE
!DUNPAC
      SUBROUTINE DUNPAC(N2,V1,V2,IFIX)
!***Begin Prologue  DUNPAC
!***Refer to  ODR
!***Routines Called  DCOPY
!***Date Written   860529   (YYMMDD)
!***Revision Date  920304   (YYMMDD)
!***Purpose  Copy the elements of V1 into the locations of V2 which are
!            unfixed
!***End Prologue  DUNPAC

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      INTEGER N2

!...Array arguments
      REAL (KIND=R8) V1(N2),V2(N2)
      INTEGER IFIX(N2)

!...Local scalars
      INTEGER I,N1

!...External subroutines
      EXTERNAL DCOPY

!...Variable Definitions (alphabetically)
!   I:       An indexing variable.
!   IFIX:    The values designating whether the elements of V2 are 
!            fixed at their input values or not.
!            ODRPACK95 reference guide.)
!   N1:      The number of items in V1.
!   N2:      The number of items in V2.
!   V1:      The vector of the unfixed items.
!   V2:      The vector of the fixed and unfixed items into which the
!            elements of V1 are to be inserted.


!***First executable statement  DUNPAC


      N1 = 0
      IF (IFIX(1).GE.0) THEN
         DO 10 I = 1,N2
            IF (IFIX(I).NE.0) THEN
               N1 = N1 + 1
               V2(I) = V1(N1)
            END IF
   10    CONTINUE
      ELSE
         N1 = N2
         CALL DCOPY(N2,V1,1,V2,1)
      END IF

      RETURN
      END SUBROUTINE
!DVEVTR
      SUBROUTINE DVEVTR(M,NQ,INDX,                                     &
          V,LDV,LD2V, E,LDE, VE,LDVE,LD2VE, VEV,LDVEV,WRK5)
!***Begin Prologue  DVEVTR
!***Refer to  ODR
!***Routines Called  DSOLVE
!***Date Written   910613   (YYMMDD)
!***Revision Date  920304   (YYMMDD)
!***Purpose  Compute  V*E*trans(V) for the (INDX)TH M by NQ array in V
!***End Prologue  DVEVTR

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      INTEGER INDX,LDE,LDV,LDVE,LDVEV,LD2V,LD2VE,M,NQ

!...Array arguments
      REAL (KIND=R8) E(LDE,M),V(LDV,LD2V,NQ),VE(LDVE,LD2VE,M),         &
                     VEV(LDVEV,NQ),WRK5(M)

!...Local scalars
      REAL (KIND=R8) ZERO
      INTEGER J,L1,L2

!...External subroutines
      EXTERNAL DSOLVE

!...Data statements
      DATA ZERO /0.0E0_R8/

!...Variable Definitions (alphabetically)
!   INDX:    The row in V in which the M by NQ array is stored.
!   J:       An indexing variable.
!   LDE:     The leading dimension of array E.
!   LDV:     The leading dimension of array V.
!   LDVE:    The leading dimension of array VE.
!   LDVEV:   The leading dimension of array VEV.
!   LD2V:    The second dimension of array V.
!   L1:      An indexing variable.
!   L2:      An indexing variable.
!   M:       The number of columns of data in the independent variable.
!   NQ:      The number of responses per observation.
!   E:       The M by M matrix of the factors so ETE = (D**2 + ALPHA*T**2).
!   V:       An array of NQ by M matrices.
!   VE:      The NQ by M array VE = V * inv(E)
!   VEV:     The NQ by NQ array VEV = V * inv(ETE) * trans(V).
!   WRK5:    An M work vector.
!   ZERO:    The value 0.0E0_R8.


!***First executable statement  DVEVTR


      IF (NQ.EQ.0 .OR. M.EQ.0) RETURN

      DO 140 L1 = 1,NQ
         DO 110 J = 1,M
            WRK5(J) = V(INDX,J,L1)
  110    CONTINUE
         CALL DSOLVE(M,E,LDE,WRK5,4)
         DO 120 J = 1,M
            VE(INDX,L1,J) = WRK5(J)
  120    CONTINUE
  140 CONTINUE

      DO 230 L1 = 1,NQ
         DO 220 L2 = 1,L1
            VEV(L1,L2) = ZERO
            DO 210 J = 1,M
               VEV(L1,L2) = VEV(L1,L2) + VE(INDX,L1,J)*VE(INDX,L2,J)
  210       CONTINUE
            VEV(L2,L1) = VEV(L1,L2)
  220    CONTINUE
  230 CONTINUE

      RETURN
      END SUBROUTINE
!DWGHT
      SUBROUTINE DWGHT(N,M,WT,LDWT,LD2WT,T,WTT)
!***Begin Prologue  DWGHT
!***Refer to  ODR
!***Routines Called  (NONE)
!***Date Written   860529   (YYMMDD)
!***Revision Date  920304   (YYMMDD)
!***Purpose  Scale matrix T using WT, i.e., compute WTT = WT*T
!***End Prologue  DWGHT

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      INTEGER LDWT,LD2WT,M,N

!...Array arguments
      REAL (KIND=R8) T(:,:),WT(:,:,:),WTT(:,:)

!...Local scalars
      REAL (KIND=R8) TEMP,ZERO
      INTEGER I,J,K

!...Data statements
      DATA ZERO /0.0E0_R8/

!...Variable Definitions (alphabetically)
!   I:       An indexing variable.
!   J:       An indexing variable.
!   K:       An indexing variable.
!   LDWT:    The leading dimension of array WT.
!   LD2WT:   The second dimension of array WT.
!   M:       The number of columns of data in T.
!   N:       The number of rows of data in T.
!   T:       The array being scaled by WT.
!   TEMP:    A temporary scalar.
!   WT:      The weights.
!   WTT:     The results of weighting array T by WT.
!            Array WTT can be the same as T only if the arrays in WT 
!            are upper triangular with zeros below the diagonal.
!   ZERO:    The value 0.0E0_R8.


!***First executable statement  DWGHT


      IF (N.EQ.0 .OR. M.EQ.0) RETURN

      IF (WT(1,1,1).GE.ZERO) THEN
         IF (LDWT.GE.N) THEN
            IF (LD2WT.GE.M) THEN
!  WT is an N-array of M by M matrices
               DO 130 I=1,N
                  DO 120 J=1,M
                     TEMP = ZERO
                     DO 110 K=1,M
                        TEMP = TEMP + WT(I,J,K)*T(I,K)
  110                CONTINUE
                     WTT(I,J) = TEMP
  120             CONTINUE
  130          CONTINUE
            ELSE
!  WT is an N-array of diagonal matrices
               DO 230 I=1,N
                  DO 220 J=1,M
                     WTT(I,J) = WT(I,1,J)*T(I,J)
  220             CONTINUE
  230          CONTINUE
            END IF
         ELSE
            IF (LD2WT.GE.M) THEN
!  WT is an M by M matrix
               DO 330 I=1,N
                  DO 320 J=1,M
                     TEMP = ZERO
                     DO 310 K=1,M
                        TEMP = TEMP + WT(1,J,K)*T(I,K)
  310                CONTINUE
                     WTT(I,J) = TEMP
  320             CONTINUE
  330          CONTINUE
            ELSE
!  WT is a diagonal matrice
               DO 430 I=1,N
                  DO 420 J=1,M
                     WTT(I,J) = WT(1,1,J)*T(I,J)
  420             CONTINUE
  430          CONTINUE
            END IF
         END IF
      ELSE
!  WT is a scalar
         DO 520 J=1,M
            DO 510 I=1,N
               WTT(I,J) = ABS(WT(1,1,1))*T(I,J)
  510       CONTINUE
  520    CONTINUE
      END IF

      RETURN
      END SUBROUTINE
!DWINF
      SUBROUTINE DWINF(N,M,NP,NQ,LDWE,LD2WE,ISODR,                     &
                       DELTAI,EPSI,XPLUSI,FNI,SDI,VCVI,                &
                       RVARI,WSSI,WSSDEI,WSSEPI,RCONDI,ETAI,           &
                       OLMAVI,TAUI,ALPHAI,ACTRSI,PNORMI,RNORSI,PRERSI, &
                       PARTLI,SSTOLI,TAUFCI,EPSMAI,                    &
                       BETA0I,BETACI,BETASI,BETANI,SI,SSI,SSFI,QRAUXI, &
                       UI,FSI,FJACBI,WE1I,DIFFI,                       &
                       DELTSI,DELTNI,TI,TTI,OMEGAI,FJACDI,             &
                       WRK1I,WRK2I,WRK3I,WRK4I,WRK5I,WRK6I,WRK7I,      &
                       LOWERI,UPPERI,LWKMN)
!***Begin Prologue  DWINF
!***Refer to  ODR
!***Routines Called  (NONE)
!***Date Written   860529   (YYMMDD)
!***Revision Date  920619   (YYMMDD)
!***Purpose  Set storage locations within REAL (KIND=R8) work space
!***End Prologue  DWINF

!...Scalar arguments
      INTEGER ACTRSI,ALPHAI,BETACI,BETANI,BETASI,BETA0I,DELTAI,DELTNI, &
              DELTSI,DIFFI,EPSI,EPSMAI,ETAI,FJACBI,FJACDI,FNI,FSI,     &
              LDWE,LD2WE,LOWERI,LWKMN,M,N,NP,NQ,OLMAVI,OMEGAI,PARTLI,  &
              PNORMI,PRERSI,QRAUXI,RCONDI,RNORSI,RVARI,SDI,SI,SSFI,    &
              SSI,SSTOLI,TAUFCI,TAUI,TI,TTI,                           &
              UI,UPPERI,VCVI,WE1I,WRK1I,WRK2I,WRK3I,WRK4I,WRK5I,WRK6I, &
              WRK7I,WSSI,WSSDEI,WSSEPI,XPLUSI
      LOGICAL ISODR

!...Local scalars
      INTEGER NEXT

!...Variable Definitions (alphabetically)
!   ACTRSI:  The location in array WORK of variable ACTRS.
!   ALPHAI:  The location in array WORK of variable ALPHA.
!   BETACI:  The starting location in array WORK of array BETAC.
!   BETANI:  The starting location in array WORK of array BETAN.
!   BETASI:  The starting location in array WORK of array BETAS.
!   BETA0I:  The starting location in array WORK of array BETA0.
!   DELTAI:  The starting location in array WORK of array DELTA.
!   DELTNI:  The starting location in array WORK of array DELTAN.
!   DELTSI:  The starting location in array WORK of array DELTAS.
!   DIFFI:   The starting location in array WORK of array DIFF.
!   EPSI:    The starting location in array WORK of array EPS.
!   EPSMAI:  The location in array WORK of variable EPSMAC.
!   ETAI:    The location in array WORK of variable ETA.
!   FJACBI:  The starting location in array WORK of array FJACB.
!   FJACDI:  The starting location in array WORK of array FJACD.
!   FNI:     The starting location in array WORK of array FN.
!   FSI:     The starting location in array WORK of array FS.
!   ISODR:   The variable designating whether the solution is by ODR 
!            (ISODR=TRUE) or by OLS (ISODR=FALSE).
!   LDWE:    The leading dimension of array WE.
!   LD2WE:   The second dimension of array WE.
!   LWKMN:   The minimum acceptable length of vector work.
!   M:       The number of columns of data in the explanatory variable.
!   N:       The number of observations.
!   NEXT:    The next available location with WORK.
!   NP:      The number of function parameters.
!   NQ:      The number of responses per observation.
!   OLMAVI:  The location in array WORK of variable OLMAVG.
!   OMEGAI:  The starting location in array WORK of array OMEGA.
!   PARTLI:  The location in array WORK of variable PARTOL.
!   PNORMI:  The location in array WORK of variable PNORM.
!   PRERSI:  The location in array WORK of variable PRERS.
!   QRAUXI:  The starting location in array WORK of array QRAUX.
!   RCONDI:  The location in array WORK of variable RCONDI.
!   RNORSI:  The location in array WORK of variable RNORMS.
!   RVARI:   The location in array WORK of variable RVAR.
!   SDI:     The starting location in array WORK of array SD.
!   SI:      The starting location in array WORK of array S.
!   SSFI:    The starting location in array WORK of array SSF.
!   SSI:     The starting location in array WORK of array SS.
!   SSTOLI:  The location in array WORK of variable SSTOL.
!   TAUFCI:  The location in array WORK of variable TAUFAC.
!   TAUI:    The location in array WORK of variable TAU.
!   TI:      The starting location in array WORK of array T.
!   TTI:     The starting location in array WORK of array TT.
!   UI:      The starting location in array WORK of array U.
!   VCVI:    The starting location in array WORK of array VCV.
!   WE1I:    The starting location in array WORK of array WE1.
!   WRK1I:   The starting location in array WORK of array WRK1.
!   WRK2I:   The starting location in array WORK of array WRK2.
!   WRK3I:   The starting location in array WORK of array WRK3.
!   WRK4I:   The starting location in array WORK of array WRK4.
!   WRK5I:   The starting location in array WORK of array WRK5.
!   WRK6I:   The starting location in array WORK of array WRK6.
!   WRK7I:   The starting location in array WORK of array WRK7.
!   WSSI:    The location in array WORK of variable WSS.
!   WSSDEI:  The location in array WORK of variable WSSDEL.
!   WSSEPI:  The location in array work of variable WSSEPS.
!   XPLUSI:  The starting location in array WORK of array XPLUSD.


!***First executable statement  DWINF


      IF (N.GE.1 .AND. M.GE.1 .AND. NP.GE.1 .AND. NQ.GE.1 .AND.        &
          LDWE.GE.1 .AND. LD2WE.GE.1) THEN

         DELTAI =          1
         EPSI   = DELTAI + N*M
         XPLUSI = EPSI   + N*NQ
         FNI    = XPLUSI + N*M
         SDI    = FNI    + N*NQ
         VCVI   = SDI    + NP
         RVARI  = VCVI   + NP*NP

         WSSI   = RVARI  + 1
         WSSDEI = WSSI   + 1
         WSSEPI = WSSDEI + 1
         RCONDI = WSSEPI + 1
         ETAI   = RCONDI + 1
         OLMAVI = ETAI   + 1

         TAUI   = OLMAVI + 1
         ALPHAI = TAUI   + 1
         ACTRSI = ALPHAI + 1
         PNORMI = ACTRSI + 1
         RNORSI = PNORMI + 1
         PRERSI = RNORSI + 1
         PARTLI = PRERSI + 1
         SSTOLI = PARTLI + 1
         TAUFCI = SSTOLI + 1
         EPSMAI = TAUFCI + 1
         BETA0I = EPSMAI + 1

         BETACI = BETA0I + NP
         BETASI = BETACI + NP
         BETANI = BETASI + NP
         SI     = BETANI + NP
         SSI    = SI     + NP
         SSFI   = SSI    + NP
         QRAUXI = SSFI   + NP
         UI     = QRAUXI + NP
         FSI    = UI     + NP

         FJACBI = FSI    + N*NQ

         WE1I   = FJACBI + N*NP*NQ

         DIFFI  = WE1I + LDWE*LD2WE*NQ

         NEXT   = DIFFI + NQ*(NP+M)

         IF (ISODR) THEN
            DELTSI = NEXT
            DELTNI = DELTSI + N*M
            TI     = DELTNI + N*M
            TTI    = TI     + N*M
            OMEGAI = TTI    + N*M
            FJACDI = OMEGAI + NQ*NQ
            WRK1I  = FJACDI + N*M*NQ
            NEXT   = WRK1I  + N*M*NQ
         ELSE
            DELTSI = DELTAI
            DELTNI = DELTAI
            TI     = DELTAI
            TTI    = DELTAI
            OMEGAI = DELTAI
            FJACDI = DELTAI
            WRK1I  = DELTAI
         END IF

         WRK2I  = NEXT
         WRK3I  = WRK2I + N*NQ
         WRK4I  = WRK3I + NP
         WRK5I  = WRK4I + M*M
         WRK6I  = WRK5I + M
         WRK7I  = WRK6I + N*NQ*NP
         LOWERI = WRK7I + 5*NQ
         UPPERI = LOWERI + NP
         NEXT   = UPPERI + NP

         LWKMN  = NEXT
      ELSE
         DELTAI = 1
         EPSI   = 1
         XPLUSI = 1
         FNI    = 1
         SDI    = 1
         VCVI   = 1
         RVARI  = 1
         WSSI   = 1
         WSSDEI = 1
         WSSEPI = 1
         RCONDI = 1
         ETAI   = 1
         OLMAVI = 1
         TAUI   = 1
         ALPHAI = 1
         ACTRSI = 1
         PNORMI = 1
         RNORSI = 1
         PRERSI = 1
         PARTLI = 1
         SSTOLI = 1
         TAUFCI = 1
         EPSMAI = 1
         BETA0I = 1
         BETACI = 1
         BETASI = 1
         BETANI = 1
         SI     = 1
         SSI    = 1
         SSFI   = 1
         QRAUXI = 1
         FSI    = 1
         UI     = 1
         FJACBI = 1
         WE1I   = 1
         DIFFI  = 1
         DELTSI = 1
         DELTNI = 1
         TI     = 1
         TTI    = 1
         FJACDI = 1
         OMEGAI = 1
         WRK1I  = 1
         WRK2I  = 1
         WRK3I  = 1
         WRK4I  = 1
         WRK5I  = 1
         WRK6I  = 1
         WRK7I  = 1
         LOWERI = 1
         UPPERI = 1
         LWKMN  = 1
      END IF

      RETURN
      END SUBROUTINE
!DXMY
      SUBROUTINE DXMY(N,M,X,LDX,Y,LDY,XMY,LDXMY)
!***Begin Prologue  DXMY
!***Refer to  ODR
!***Routines Called  (NONE)
!***Date Written   860529   (YYMMDD)
!***Revision Date  920304   (YYMMDD)
!***Purpose  Compute XMY = X - Y
!***End Prologue  DXMY

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      INTEGER LDX,LDXMY,LDY,M,N

!...Array arguments
      REAL (KIND=R8) X(LDX,M),XMY(LDXMY,M),Y(LDY,M)

!...Local scalars
      INTEGER I,J

!...Variable Definitions (alphabetically)
!   I:       An indexing variable.
!   J:       An indexing variable.
!   LDX:     The leading dimension of array X.
!   LDXMY:   The leading dimension of array XMY.
!   LDY:     The leading dimension of array Y.
!   M:       The number of columns of data in arrays X and Y.
!   N:       The number of rows of data in arrays X and Y.
!   X:       The first of the two arrays.
!   XMY:     The values of X-Y.
!   Y:       The second of the two arrays.


!***First executable statement  DXMY


      DO 20 J=1,M
         DO 10 I=1,N
            XMY(I,J) = X(I,J) - Y(I,J)
   10    CONTINUE
   20 CONTINUE

      RETURN
      END SUBROUTINE
!DXPY
      SUBROUTINE DXPY(N,M,X,LDX,Y,LDY,XPY,LDXPY)
!***Begin Prologue  DXPY
!***Refer to  ODR
!***Routines Called  (None)
!***Date Written   860529   (YYMMDD)
!***Revision Date  920304   (YYMMDD)
!***Purpose  Compute XPY = X + Y
!***End Prologue  DXPY

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      INTEGER LDX,LDXPY,LDY,M,N

!...Array arguments
      REAL (KIND=R8) X(LDX,M),XPY(LDXPY,M),Y(LDY,M)

!...Local scalars
      INTEGER I,J

!...Variable Definitions (alphabetically)
!   I:       An indexing variable.
!   J:       An indexing variable.
!   LDX:     The leading dimension of array X.
!   LDXPY:   The leading dimension of array XPY.
!   LDY:     The leading dimension of array Y.
!   M:       The number of columns of data in arrays X and Y.
!   N:       The number of rows of data in arrays X and Y.
!   X:       The first of the two arrays to be added together.
!   XPY:     The values of X+Y.
!   Y:       The second of the two arrays to be added together.


!***First executable statement  DXPY


      DO 20 J=1,M
         DO 10 I=1,N
            XPY(I,J) = X(I,J) + Y(I,J)
   10    CONTINUE
   20 CONTINUE

      RETURN
      END SUBROUTINE
!DZERO
      SUBROUTINE DZERO(N,M,A,LDA)
!***Begin Prologue  DZERO
!***Refer to  ODR
!***Routines Called  (None)
!***Date Written   860529   (YYMMDD)
!***Revision Date  920304   (YYMMDD)
!***Purpose  Set A = ZERO
!***End Prologue  DZERO

!...Used modules
      USE REAL_PRECISION

!...Scalar arguments
      INTEGER LDA,M,N

!...Array arguments
      REAL (KIND=R8) A(LDA,M)

!...Local scalars
      REAL (KIND=R8) ZERO
      INTEGER I,J

!...Data statements
      DATA ZERO /0.0E0_R8/

!...Variable Definitions (alphabetically)
!   A:       The array to be set to zero.
!   I:       An indexing variable.
!   J:       An indexing variable.
!   LDA:     The leading dimension of array A.
!   M:       The number of columns to be set to zero.
!   N:       The number of rows to be set to zero.
!   ZERO:    The value 0.0E0_R8.


!***First executable statement  DZERO


      DO 20 J=1,M
         DO 10 I=1,N
            A(I,J) = ZERO
   10    CONTINUE
   20 CONTINUE

      RETURN
      END SUBROUTINE
